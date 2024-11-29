use itertools::peek_nth;
use itertools::structs::PeekNth;
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenStream as TokenStream2, TokenTree};
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::token::{Pound, Star};
use syn::{braced, parse_macro_input, parse_quote, Error, Ident, LitInt, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq_input = parse_macro_input!(input as SeqInput);

    seq_input
        .render()
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

struct SeqInput {
    variable: Ident,
    range: Vec<usize>,
    tt: TokenStream2,
}

impl Parse for SeqInput {
    fn parse(input: ParseStream) -> Result<Self> {
        // N
        let variable = input.parse()?;
        // in
        let _in: Token![in] = input.parse()?;

        // a..=b
        let from: LitInt = input.parse()?;
        // ref: https://github.com/jonhoo/proc-macro-workshop/blob/108929e4f3c90cf0a838507f6b557c742261fab9/seq/seq-proc/src/lib.rs#L24-L29
        let inclusive = input.peek(Token![..=]);
        if inclusive {
            <Token![..=]>::parse(input)?;
        } else {
            <Token![..]>::parse(input)?;
        }
        let to: LitInt = input.parse()?;
        let range = make_range(from, to, inclusive)?;

        // {..}
        let content;
        let _braces = braced!(content in input);
        let tt = TokenStream2::parse(&content)?;

        Ok(Self {
            variable,
            range,
            tt,
        })
    }
}

fn make_range(from: LitInt, to: LitInt, inclusive: bool) -> Result<Vec<usize>> {
    let from = from.base10_parse()?;
    let mut to = to.base10_parse()?;

    if inclusive {
        to += 1usize;
    }

    Ok((from..to).collect())
}

impl SeqInput {
    fn render(self) -> Result<TokenStream2> {
        let Self {
            variable,
            range,
            tt,
        } = self;

        let tokens_with_place_holders = TokensWithPlaceHolders::new(variable, tt);

        tokens_with_place_holders.render(&range)
    }
}

#[derive(Clone, Debug)]
enum SeqToken {
    Group(Delimiter, Vec<SeqToken>, Span),
    PlaceHolder(PlaceHolder),
    Other(TokenTree),
    RepeatTarget(Token![#], Vec<SeqToken>, Token![*], Span),
}

impl ToTokens for SeqToken {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Group(delimiter, vec, span) => {
                let mut stream = TokenStream2::new();
                vec.iter().for_each(|st| st.to_tokens(&mut stream));
                let mut group = Group::new(*delimiter, stream);
                group.set_span(*span);
                group.to_tokens(tokens);
            }
            Self::PlaceHolder(place_holder) => place_holder.to_tokens(tokens),
            Self::Other(token_tree) => token_tree.to_tokens(tokens),
            Self::RepeatTarget(pound, vec, star, span) => {
                pound.to_tokens(tokens);
                let mut stream = TokenStream2::new();
                vec.iter().for_each(|st| st.to_tokens(&mut stream));
                let mut group = Group::new(Delimiter::Parenthesis, stream);
                group.set_span(*span);
                group.to_tokens(tokens);
                star.to_tokens(tokens);
            }
        }
    }
}

#[derive(Clone, Debug)]
enum PlaceHolder {
    AsLit(Ident),
    AsIdent {
        // prefix~N~suffix
        prefix: Ident,
        tilde1: Token![~],
        target: Ident,
        suffix: Option<(Token![~], Ident)>,
    },
}

impl ToTokens for PlaceHolder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            PlaceHolder::AsLit(ident) => ident.to_tokens(tokens),
            PlaceHolder::AsIdent {
                prefix,
                tilde1,
                target,
                suffix,
            } => {
                prefix.to_tokens(tokens);
                tilde1.to_tokens(tokens);
                target.to_tokens(tokens);
                if let Some((t, i)) = suffix {
                    t.to_tokens(tokens);
                    i.to_tokens(tokens);
                }
            }
        }
    }
}

impl PlaceHolder {
    fn render(&self, count: usize) -> TokenStream2 {
        match self {
            Self::AsLit(ident) => {
                let mut count = proc_macro2::Literal::usize_unsuffixed(count);
                count.set_span(ident.span());

                count.into_token_stream()
            }
            as_ident @ Self::AsIdent { .. } => {
                let span = as_ident.span();

                let Self::AsIdent { prefix, suffix, .. } = as_ident else {
                    unreachable!()
                };

                let mut ident = if let Some((_, suffix)) = suffix {
                    format_ident!("{}{}{}", prefix, count, suffix)
                } else {
                    format_ident!("{}{}", prefix, count)
                };

                ident.set_span(span);

                ident.into_token_stream()
            }
        }
    }
}

impl SeqToken {
    fn render(&self, range: &[usize]) -> Result<TokenStream2> {
        match self.clone() {
            Self::Group(delim, tokens, span) => {
                let tokens = tokens
                    .iter()
                    .map(|t| t.render(range))
                    .collect::<Result<Vec<_>>>()?;

                let mut group = Group::new(delim, quote! { #(#tokens)* });
                group.set_span(span);

                Ok(group.into_token_stream())
            }
            Self::PlaceHolder(_) => Err(Error::new(
                self.span(),
                "invalid position of repeat variable place holder.",
            )),
            Self::RepeatTarget(_, tokens, _, _) => {
                let tokens = range.iter().flat_map(|&count| {
                    tokens
                        .iter()
                        .map(move |token| token.render_in_repeat_target(count))
                });

                Ok(quote! { #(#tokens)* })
            }
            Self::Other(t) => Ok(t.into_token_stream()),
        }
    }

    fn render_in_repeat_target(&self, count: usize) -> TokenStream2 {
        match self.clone() {
            Self::Group(delim, tokens, span) => {
                let tokens = tokens.iter().map(|t| t.render_in_repeat_target(count));

                let mut group = Group::new(delim, quote! { #(#tokens)* });
                group.set_span(span);

                group.into_token_stream()
            }
            Self::PlaceHolder(place_holder) => place_holder.render(count),
            rt @ Self::RepeatTarget(..) => rt.render_in_deep_nest(count),
            Self::Other(t) => quote! { #t },
        }
    }

    // 2段以上のRepeatTarget( `#(...)*` )については現在対象のプレースホルダを置き換える以上のことはしない。例えば別のマクロのためのものと考える
    fn render_in_deep_nest(&self, count: usize) -> TokenStream2 {
        match self.clone() {
            Self::Group(delim, tokens, span) => {
                let tokens = tokens.iter().map(|t| t.render_in_deep_nest(count));

                let mut group = Group::new(delim, quote! { #(#tokens)* });
                group.set_span(span);

                quote! { #group }
            }
            Self::PlaceHolder(place_holder) => place_holder.render(count),
            Self::RepeatTarget(pound, tokens, star, span) => {
                let tokens = tokens.iter().map(|t| t.render_in_deep_nest(count));

                let mut group = Group::new(Delimiter::Parenthesis, quote! { #(#tokens)* });
                group.set_span(span);

                quote! { #pound #group #star }
            }
            Self::Other(t) => quote! { #t },
        }
    }
}

struct TokensWithPlaceHolders(SeqToken);

impl TokensWithPlaceHolders {
    fn new(place_holder_ident: Ident, stream: TokenStream2) -> Self {
        let mut tokens = Vec::new();
        let mut has_repeat_block = false;

        parse_stream_rec(
            place_holder_ident,
            stream,
            &mut tokens,
            &mut has_repeat_block,
        );

        let tokens = if has_repeat_block {
            SeqToken::Group(Delimiter::None, tokens, Span::call_site())
        } else {
            SeqToken::RepeatTarget(Pound::default(), tokens, Star::default(), Span::call_site())
        };

        Self(tokens)
    }

    fn render(&self, range: &[usize]) -> Result<TokenStream2> {
        self.0.render(range)
    }
}

fn parse_stream_rec(
    place_holder_ident: Ident,
    stream: TokenStream2,
    tokens: &mut Vec<SeqToken>,
    has_repeat_block: &mut bool,
) {
    let mut trees = peek_nth(stream);
    while let Some(tree) = trees.next() {
        match tree {
            TokenTree::Group(group) => {
                let delim = group.delimiter();
                let mut inner = Vec::new();
                parse_stream_rec(
                    place_holder_ident.clone(),
                    group.stream(),
                    &mut inner,
                    has_repeat_block,
                );
                tokens.push(SeqToken::Group(delim, inner, group.span()));
            }
            TokenTree::Ident(ident) => {
                let t = if ident == place_holder_ident {
                    SeqToken::PlaceHolder(PlaceHolder::AsLit(ident))
                } else if let Some(as_ident) =
                    parse_place_holder_ident(place_holder_ident.clone(), ident.clone(), &mut trees)
                {
                    SeqToken::PlaceHolder(as_ident)
                } else {
                    SeqToken::Other(TokenTree::Ident(ident))
                };
                tokens.push(t);
            }
            TokenTree::Punct(pound) if pound.as_char() == '#' => {
                let group = trees.peek().cloned();
                let star = trees.peek_nth(1).cloned();

                let (Some(TokenTree::Group(group)), Some(TokenTree::Punct(star))) = (group, star)
                else {
                    tokens.push(SeqToken::Other(TokenTree::Punct(pound)));
                    continue;
                };

                if group.delimiter() != Delimiter::Parenthesis {
                    tokens.push(SeqToken::Other(TokenTree::Punct(pound)));
                    continue;
                }

                if star.as_char() != '*' {
                    tokens.push(SeqToken::Other(TokenTree::Punct(pound)));
                    continue;
                }

                *has_repeat_block = true;

                let (Some(TokenTree::Group(_group)), Some(TokenTree::Punct(_star))) =
                    (trees.next(), trees.next())
                else {
                    unreachable!()
                };

                let pound: Token![#] = parse_quote! { #pound };
                let mut inner = Vec::new();
                parse_stream_rec(
                    place_holder_ident.clone(),
                    group.stream(),
                    &mut inner,
                    has_repeat_block,
                );
                let star: Token![*] = parse_quote! { #star };

                tokens.push(SeqToken::RepeatTarget(pound, inner, star, group.span()));
            }
            t => tokens.push(SeqToken::Other(t)),
        }
    }
}

fn parse_place_holder_ident(
    place_holder_ident: Ident,
    prefix: Ident,
    trees: &mut PeekNth<impl Iterator<Item = TokenTree>>,
) -> Option<PlaceHolder> {
    // pre~N~suff

    // ~
    let Some(TokenTree::Punct(punct)) = trees.peek() else {
        return None;
    };

    // ~
    if punct.as_char() != '~' {
        return None;
    };

    // N
    let Some(TokenTree::Ident(ident)) = trees.peek_nth(1) else {
        return None;
    };

    // N
    if &place_holder_ident != ident {
        return None;
    }

    let suffix = (|| {
        // ~
        let Some(TokenTree::Punct(punct)) = trees.peek_nth(2) else {
            return None;
        };

        // ~
        if punct.as_char() != '~' {
            return None;
        }

        // suff
        if let Some(TokenTree::Ident(ident)) = trees.peek_nth(3) {
            Some(ident.clone())
        } else {
            None
        }
    })();

    // ~
    let tilde1 = trees.next().unwrap();
    let tilde1 = parse_quote!( #tilde1 );
    // N (target)
    let target = trees.next().unwrap();
    let target = parse_quote!( #target );
    let suffix = suffix.map(|suffix| {
        // ~
        let tilde2 = trees.next().unwrap();
        let tilde2 = parse_quote!( #tilde2 );
        // suff
        let _suffix = trees.next().unwrap();

        (tilde2, suffix)
    });

    Some(PlaceHolder::AsIdent {
        prefix,
        tilde1,
        target,
        suffix,
    })
}

/* // 良い方針と思ったがうまくいかなかった
#[derive(Debug)]
struct VariableReplacer {
    variable: Ident,
    count: usize,
}

impl VisitMut for VariableReplacer {
    fn visit_expr_mut(&mut self, i: &mut Expr) {
        eprintln!("beep 1: {:?}", i);

        if let Expr::Path(p) = i {
            eprintln!("beep 2: {:?}", p);

            if p.path.is_ident(&self.variable) {
                eprintln!("beep 3: {:?}", p.path);

                *i = Expr::Lit(ExprLit {
                    lit: Lit::Int(LitInt::new(&self.count.to_string(), p.span())),
                    attrs: p.attrs.clone(),
                });
            }
        }

        visit_expr_mut(self, i);
    }
}
*/

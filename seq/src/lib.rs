use itertools::peek_nth;
use itertools::structs::PeekNth;
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenStream as TokenStream2, TokenTree};
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{braced, parse_macro_input, Error, Expr, ExprLit, ExprRange, Ident, Lit, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let repeated_block = parse_macro_input!(input as RepeatedBlock);

    repeated_block.render().into()
}

struct RepeatedBlock {
    variable: Ident,
    range: Box<dyn Iterator<Item = usize>>,
    tt: TokenStream2,
}

impl Parse for RepeatedBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let variable = input.parse()?;
        let _in: Token![in] = input.parse()?;
        let range: ExprRange = input.parse()?;
        let range = range_to_iter(range)?;
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

fn range_to_iter(range: ExprRange) -> Result<Box<dyn Iterator<Item = usize>>> {
    use std::ops::{Range, RangeInclusive};
    use syn::RangeLimits::{Closed, HalfOpen};

    let span = range.span();

    let ExprRange {
        start, limits, end, ..
    } = range;

    let into_usize = |exp: &Expr| -> Result<usize> {
        let Expr::Lit(ExprLit {
            lit: Lit::Int(i), ..
        }) = exp
        else {
            return Err(Error::new(span, "expected usize literal"));
        };

        i.base10_parse()
    };

    let Some(end) = end.map(|e| into_usize(&e)).transpose()? else {
        return Err(Error::new(span, "expected end of range"));
    };

    let start = match start {
        Some(start) => into_usize(&start)?,
        None => 0,
    };

    match limits {
        // start..end
        HalfOpen(_) => {
            let range = Range { start, end };
            Ok(Box::new(range.into_iter()))
        }
        // start..=end
        Closed(_) => {
            let range = RangeInclusive::new(start, end);
            Ok(Box::new(range.into_iter()))
        }
    }
}

impl RepeatedBlock {
    fn render(self) -> TokenStream2 {
        let Self {
            variable,
            range,
            tt,
        } = self;

        let tokens_with_place_holders = TokensWithPlaceHolders::new(variable, tt);

        let tokens = range.map(|count| tokens_with_place_holders.render(count));

        quote! { #(#tokens)* }
    }
}

#[derive(Clone)]
enum NestToken {
    Block(Delimiter, Vec<NestToken>, Span),
    PlaceHolder(PlaceHolder),
    Other(TokenTree),
}

#[derive(Clone)]
enum PlaceHolder {
    AsLit(Span),
    AsIdent {
        prefix: Ident,
        suffix: Option<Ident>,
        span: Span,
    },
}

impl NestToken {
    fn render(&self, count: usize) -> TokenStream2 {
        match self.clone() {
            Self::Block(delim, tokens, span) => {
                let tokens = tokens.iter().map(|t| t.render(count));

                let mut group = Group::new(delim, quote! { #(#tokens)* });
                group.set_span(span);

                quote! { #group }
            }
            Self::PlaceHolder(PlaceHolder::AsLit(span)) => {
                let mut count = proc_macro2::Literal::usize_unsuffixed(count);
                count.set_span(span);

                quote! { #count }
            }
            Self::PlaceHolder(PlaceHolder::AsIdent {
                prefix,
                suffix,
                span,
            }) => {
                let mut ident = if let Some(suffix) = suffix {
                    format_ident!("{}{}{}", prefix, count, suffix)
                } else {
                    format_ident!("{}{}", prefix, count)
                };

                ident.set_span(span);

                quote! { #ident }
            }
            Self::Other(t) => quote! { #t },
        }
    }
}

struct TokensWithPlaceHolders(Vec<NestToken>);

impl TokensWithPlaceHolders {
    fn new(place_holder_ident: Ident, stream: TokenStream2) -> Self {
        let mut tokens = Vec::new();

        parse_stream_rec(place_holder_ident, stream, &mut tokens);

        Self(tokens)
    }

    fn render(&self, count: usize) -> TokenStream2 {
        let nt = self.0.iter().map(|nt| nt.render(count));

        quote! { #(#nt)* }
    }
}

fn parse_stream_rec(place_holder_ident: Ident, stream: TokenStream2, tokens: &mut Vec<NestToken>) {
    let mut trees = peek_nth(stream.into_iter());
    while let Some(tree) = trees.next() {
        match tree {
            TokenTree::Group(group) => {
                let delim = group.delimiter();
                let span = group.span();
                let mut inner = Vec::new();
                parse_stream_rec(place_holder_ident.clone(), group.stream(), &mut inner);
                tokens.push(NestToken::Block(delim, inner, span));
            }
            TokenTree::Ident(ident) => {
                let t = if ident == place_holder_ident {
                    NestToken::PlaceHolder(PlaceHolder::AsLit(ident.span()))
                } else {
                    if let Some(as_ident) = parse_place_holder_ident(
                        place_holder_ident.clone(),
                        ident.clone(),
                        &mut trees,
                    ) {
                        NestToken::PlaceHolder(as_ident)
                    } else {
                        NestToken::Other(TokenTree::Ident(ident))
                    }
                };
                tokens.push(t);
            }
            t => tokens.push(NestToken::Other(t)),
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
    let _ = trees.next();
    // N
    let _ = trees.next();
    if let Some(_) = suffix.as_ref() {
        // ~
        let _ = trees.next();
        // suff
        let _ = trees.next();
    }

    let span = prefix.span();

    Some(PlaceHolder::AsIdent {
        prefix,
        suffix,
        span,
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

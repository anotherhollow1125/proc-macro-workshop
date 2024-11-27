use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenStream as TokenStream2, TokenTree};
use quote::quote;
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

enum NestToken {
    Block(Delimiter, Vec<NestToken>, Span),
    PlaceHolder(Span),
    Other(TokenTree),
}

impl NestToken {
    fn render(&self, count: usize) -> TokenStream2 {
        match self {
            Self::Block(delim, tokens, span) => {
                let tokens = tokens.iter().map(|t| t.render(count));

                let mut group = Group::new(*delim, quote! { #(#tokens)* });
                group.set_span(*span);

                quote! { #group }
            }
            Self::PlaceHolder(s) => {
                let mut count = proc_macro2::Literal::usize_unsuffixed(count);
                count.set_span(*s);

                quote! { #count }
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
    let mut trees = stream.into_iter();
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
                    NestToken::PlaceHolder(ident.span())
                } else {
                    NestToken::Other(TokenTree::Ident(ident))
                };
                tokens.push(t);
            }
            t => tokens.push(NestToken::Other(t)),
        }
    }
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

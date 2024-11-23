use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Field, Fields, Ident,
    Lit, Meta, MetaNameValue, Result,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let DeriveInput {
        ident,
        data: Data::Struct(DataStruct { fields, .. }),
        ..
    } = derive_input
    else {
        return Error::new(
            derive_input.ident.span(),
            "Invalid Target: this macro can be used for only structs.",
        )
        .into_compile_error()
        .into();
    };

    derive_inner(ident, fields)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn derive_inner(ident: Ident, fields: Fields) -> Result<TokenStream2> {
    let fields = fields
        .into_iter()
        .map(field_method)
        .collect::<Result<Vec<_>>>()?;

    Ok(quote! {
        impl ::std::fmt::Debug for #ident {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                fmt.debug_struct(::std::stringify!(#ident))
                    #(
                        #fields
                    )*
                    .finish()
            }
        }
    })
}

fn field_method(field: Field) -> Result<TokenStream2> {
    let Field {
        ident: Some(ident),
        attrs,
        ..
    } = field
    else {
        return Err(Error::new(field.span(), "Expected named fields."));
    };

    let fmt = attrs
        .into_iter()
        .find_map(|attr| {
            if !attr.path().is_ident("debug") {
                return None;
            }

            let Meta::NameValue(MetaNameValue {
                path: _,
                eq_token: _,
                value,
            }) = attr.meta
            else {
                return None;
            };

            let Expr::Lit(ExprLit {
                lit: Lit::Str(lit), ..
            }) = value
            else {
                return Some(Err(Error::new(value.span(), "Expected a string literal.")));
            };

            Some(Ok(lit))
        })
        .transpose()?;

    let res = if let Some(fmt) = fmt {
        quote! { .field(::std::stringify!(#ident), &::std::format_args!(#fmt, &self.#ident)) }
    } else {
        quote! { .field(::std::stringify!(#ident), &self.#ident) }
    };

    Ok(res)
}

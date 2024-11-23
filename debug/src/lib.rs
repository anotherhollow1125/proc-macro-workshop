use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Error, Field, Fields, Ident, Result};

#[proc_macro_derive(CustomDebug)]
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
        .map(|field| {
            let Field {
                ident: Some(ident), ..
            } = field
            else {
                return Err(Error::new(ident.span(), "Expected named fields."));
            };

            Ok(quote! { .field(::std::stringify!(#ident), &self.#ident) })
        })
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

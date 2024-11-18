use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data: Data::Struct(DataStruct { fields, .. }),
        ..
    } = parse_macro_input!(input as DeriveInput)
    else {
        panic!("Invalid Input");
    };

    let builder_name = Ident::new(&format!("{}Builder", ident), Span::call_site());

    let mut builder_fields = vec![];
    let mut builder_fields_for_new = vec![];
    let mut methods = vec![];

    for field in fields {
        let vis = field.vis;
        let ident = field.ident.unwrap();
        let ty = field.ty;

        builder_fields.push(quote! {
            #vis #ident: Option<#ty>,
        });

        builder_fields_for_new.push(quote! {
            #ident: None,
        });

        methods.push(quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        });
    }

    quote! {
        pub struct #builder_name {
            #(
                #builder_fields
            )*
        }

        impl #ident {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(
                        #builder_fields_for_new
                    )*
                }
            }
        }

        impl #builder_name {
            #(
                #methods
            )*
        }
    }
    .into()
}

use proc_macro::TokenStream;
use quote::{format_ident, quote};
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

    let builder_name = format_ident!("{}Builder", ident);

    let mut builder_fields = vec![];
    let mut builder_fields_for_new = vec![];
    let mut methods = vec![];
    let mut builder_to_struct = vec![];

    for field in fields {
        let vis = field.vis;
        let ident = field.ident.unwrap();
        let ty = field.ty;

        builder_fields.push(quote! {
            #vis #ident: ::std::option::Option<#ty>,
        });

        builder_fields_for_new.push(quote! {
            #ident: ::std::option::Option::None,
        });

        methods.push(quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = ::std::option::Option::Some(#ident);
                self
            }
        });

        builder_to_struct.push(quote! {
            #ident: self.#ident
                .take()
                .ok_or_else(|| ::std::boxed::Box::<dyn ::std::error::Error>::from(
                    ::std::format!("'{}' is not set.", ::std::stringify!(#ident))
                ))?,
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
            pub fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                ::std::result::Result::Ok(#ident {
                    #(
                        #builder_to_struct
                    )*
                })
            }

            #(
                #methods
            )*
        }
    }
    .into()
}

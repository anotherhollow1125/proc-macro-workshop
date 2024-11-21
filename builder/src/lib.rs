use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, Attribute, Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Ident, Lit,
    Meta, MetaNameValue, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Nop, attributes(nop))]
pub fn nop(_input: TokenStream) -> TokenStream {
    quote! {}.into()
}

#[proc_macro_derive(Builder, attributes(builder))]
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
        let Field { attrs, .. } = field.clone();

        // dbg!(&attrs);

        let attr = attrs.iter().find(|attr| {
            matches!(
                attr,
                &Attribute {
                    meta: Meta::List(_),
                    ..
                }
            ) && attr.path().is_ident("builder")
        });

        if let Some(attr) = attr {
            vec_field(
                field.clone(),
                attr,
                &mut builder_fields,
                &mut builder_fields_for_new,
                &mut methods,
                &mut builder_to_struct,
            );
        } else {
            simple_field(
                field.clone(),
                &mut builder_fields,
                &mut builder_fields_for_new,
                &mut methods,
                &mut builder_to_struct,
            );
        }
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

fn simple_field(
    field: Field,
    builder_fields: &mut Vec<TokenStream2>,
    builder_fields_for_new: &mut Vec<TokenStream2>,
    methods: &mut Vec<TokenStream2>,
    builder_to_struct: &mut Vec<TokenStream2>,
) {
    let Field {
        vis,
        ident: Some(ident),
        ty,
        ..
    } = field.clone()
    else {
        panic!("Invalid Structure.");
    };

    let (typ, is_option) = match ty {
        Type::Path(TypePath { qself: None, path })
            if path.segments.first().map(|s| &s.ident) == Some(&format_ident!("Option")) =>
        {
            let PathArguments::AngleBracketed(aga) =
                path.segments.into_iter().next().unwrap().arguments
            else {
                panic!("Invalid Option Structure.");
            };

            (aga.args.to_token_stream(), true)
        }
        ty => (ty.to_token_stream(), false),
    };

    builder_fields.push(quote! {
        #vis #ident: ::std::option::Option<#typ>,
    });

    builder_fields_for_new.push(quote! {
        #ident: ::std::option::Option::None,
    });

    methods.push(quote! {
        fn #ident(&mut self, #ident: #typ) -> &mut Self {
            self.#ident = ::std::option::Option::Some(#ident);
            self
        }
    });

    let ts = if is_option {
        quote! {
            #ident: self.#ident
                .take(),
        }
    } else {
        quote! {
            #ident: self.#ident
                .take()
                .ok_or_else(|| ::std::boxed::Box::<dyn ::std::error::Error>::from(
                    ::std::format!("'{}' is not set.", ::std::stringify!(#ident))
                ))?,
        }
    };

    builder_to_struct.push(ts);
}

fn vec_field(
    field: Field,
    attr: &Attribute,
    builder_fields: &mut Vec<TokenStream2>,
    builder_fields_for_new: &mut Vec<TokenStream2>,
    methods: &mut Vec<TokenStream2>,
    builder_to_struct: &mut Vec<TokenStream2>,
) {
    let each_ident = (|| -> syn::parse::Result<Ident> {
        let meta_name_value: MetaNameValue = attr.parse_args()?;
        let Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) = meta_name_value.value
        else {
            panic!("[1] Invalid Attribute. expect: #[builder(each = \"xxx\")]");
        };

        Ok(format_ident!("{}", lit.value()))
    })()
    .expect("[2] Invalid Attribute. expect: #[builder(each = \"xxx\")]");

    let Field {
        vis,
        ident: Some(ident),
        ty,
        ..
    } = field.clone()
    else {
        panic!("Invalid Field.");
    };

    let Type::Path(TypePath { qself: None, path }) = ty else {
        panic!("Invalid Field. It must be Vec.");
    };

    if path.segments.first().map(|s| &s.ident) != Some(&format_ident!("Vec")) {
        panic!("Invalid Field. It must be Vec.");
    }

    let PathArguments::AngleBracketed(aga) = path.segments.into_iter().next().unwrap().arguments
    else {
        panic!("Invalid Vec Structure.");
    };

    let typ = aga.args.to_token_stream();

    builder_fields.push(quote! {
        #vis #ident: ::std::vec::Vec<#typ>,
    });

    builder_fields_for_new.push(quote! {
        #ident: ::std::vec::Vec::new(),
    });

    let method_block = if each_ident != ident {
        quote! {
            fn #ident(&mut self, mut #ident: ::std::vec::Vec<#typ>) -> &mut Self {
                self.#ident.append(&mut #ident);
                self
            }

            fn #each_ident(&mut self, #ident: #typ) -> &mut Self {
                self.#ident.push(#ident);
                self
            }
        }
    } else {
        quote! {
            fn #each_ident(&mut self, #ident: #typ) -> &mut Self {
                self.#ident.push(#ident);
                self
            }
        }
    };

    methods.push(method_block);

    builder_to_struct.push(quote! {
        #ident: self.#ident
            .drain(..)
            .collect(),
    });
}

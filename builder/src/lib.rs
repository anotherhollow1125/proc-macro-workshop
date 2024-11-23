use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, Attribute, Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Field,
    Fields, Ident, Lit, Meta, MetaNameValue, PathArguments, Result, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: TokenStream) -> TokenStream {
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

    derive_builder_inner(ident, fields)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn derive_builder_inner(ident: Ident, fields: Fields) -> Result<TokenStream2> {
    let span = ident.span();
    let builder_name = format_ident!("{}Builder", ident);

    let mut builder_fields = vec![];
    let mut builder_fields_for_new = vec![];
    let mut methods = vec![];
    let mut builder_to_struct = vec![];

    for field in fields {
        let Field {
            attrs,
            ident: Some(ident),
            ..
        } = field.clone()
        else {
            return Err(Error::new(
                span,
                "Invalid Field: this macro can be used for only named fields.",
            ));
        };

        let span = ident.span();

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
                span,
                field.clone(),
                attr,
                &mut builder_fields,
                &mut builder_fields_for_new,
                &mut methods,
                &mut builder_to_struct,
            )?;
        } else {
            simple_field(
                span,
                field.clone(),
                &mut builder_fields,
                &mut builder_fields_for_new,
                &mut methods,
                &mut builder_to_struct,
            )?;
        }
    }

    let res = quote! {
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
    };

    Ok(res)
}

fn simple_field(
    span: Span,
    field: Field,
    builder_fields: &mut Vec<TokenStream2>,
    builder_fields_for_new: &mut Vec<TokenStream2>,
    methods: &mut Vec<TokenStream2>,
    builder_to_struct: &mut Vec<TokenStream2>,
) -> Result<()> {
    let Field {
        vis,
        ident: Some(ident),
        ty,
        ..
    } = field.clone()
    else {
        return Err(Error::new(
            span,
            "Invalid Field: this macro can be used for only named fields.",
        ));
    };

    let span = ident.span();

    let (typ, is_option) = match ty {
        Type::Path(TypePath { qself: None, path })
            if path.segments.first().map(|s| &s.ident) == Some(&format_ident!("Option")) =>
        {
            let PathArguments::AngleBracketed(aga) =
                path.segments.into_iter().next().unwrap().arguments
            else {
                return Err(Error::new(
                    span,
                    "Invalid Option Structure: this macro can be used for only Option<T>.",
                ));
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

    Ok(())
}

fn vec_field(
    span: Span,
    field: Field,
    attr: &Attribute,
    builder_fields: &mut Vec<TokenStream2>,
    builder_fields_for_new: &mut Vec<TokenStream2>,
    methods: &mut Vec<TokenStream2>,
    builder_to_struct: &mut Vec<TokenStream2>,
) -> Result<()> {
    let MetaNameValue {
        path,
        eq_token: _,
        value,
    } = attr.parse_args()?;

    // attr全体のSpanを得る方法がわからず...

    let path = path.require_ident()?;
    if path != "each" {
        return Err(Error::new(
            path.span(),
            "expected `builder(each = \"...\")`",
        ));
    }

    let Expr::Lit(ExprLit {
        lit: Lit::Str(lit), ..
    }) = value
    else {
        return Err(Error::new(
            path.span(),
            "expected `builder(each = \"...\")`",
        ));
    };

    let each_ident = format_ident!("{}", lit.value());

    let Field {
        vis,
        ident: Some(ident),
        ty,
        ..
    } = field.clone()
    else {
        return Err(Error::new(
            span,
            "Invalid Field: this macro can be used for only named fields.",
        ));
    };

    let span = ident.span();

    let Type::Path(TypePath { qself: None, path }) = ty else {
        return Err(Error::new(span, "Invalid Field. It must be Vec."));
    };

    if path.segments.first().map(|s| &s.ident) != Some(&format_ident!("Vec")) {
        return Err(Error::new(span, "Invalid Field. It must be Vec."));
    }

    let PathArguments::AngleBracketed(aga) = path.segments.into_iter().next().unwrap().arguments
    else {
        return Err(Error::new(span, "Invalid Vec Structure."));
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

    Ok(())
}

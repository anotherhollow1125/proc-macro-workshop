use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Field,
    Fields, GenericArgument, GenericParam, Generics, Ident, Lit, Meta, MetaNameValue, Path,
    PathArguments, Result, Type, TypePath,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let DeriveInput {
        ident,
        data: Data::Struct(DataStruct { fields, .. }),
        generics,
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

    derive_inner(ident, fields, generics)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn derive_inner(ident: Ident, fields: Fields, generics: Generics) -> Result<TokenStream2> {
    let generics = add_trait_bounds(generics, fields.clone());
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields = fields
        .into_iter()
        .map(field_method)
        .collect::<Result<Vec<_>>>()?;

    Ok(quote! {
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
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

// Add a bound `T: Debug` to every type parameter T.
// ref: https://github.com/dtolnay/syn/blob/master/examples/heapsize/heapsize_derive/src/lib.rs
fn add_trait_bounds(mut generics: Generics, fields: Fields) -> Generics {
    for param in generics.params.clone() {
        let GenericParam::Type(ref type_param) = param else {
            continue;
        };

        // T
        let need_debug = fields.iter().any(|field| {
            let Type::Path(TypePath { path, .. }) = &field.ty else {
                return false;
            };

            if path.segments.last().unwrap().ident == "PhantomData" {
                return false;
            }

            has_ident(path, &type_param.ident)
        });

        if need_debug {
            let ident = type_param.ident.clone();

            generics.make_where_clause().predicates.push(parse_quote! {
                #ident: ::std::fmt::Debug
            });
        }

        // T::Assoc
        let need_debug_for_assocfields = fields.iter().find_map(|field| {
            let Type::Path(TypePath { path, .. }) = &field.ty else {
                return None;
            };

            has_assoc(path, &type_param.ident)
        });

        if let Some(assoc) = need_debug_for_assocfields {
            generics.make_where_clause().predicates.push(parse_quote! {
                #assoc: ::std::fmt::Debug
            });
        }
    }

    generics
}

fn has_ident<I>(path: &Path, ident: &I) -> bool
where
    I: ?Sized,
    Ident: PartialEq<I>,
{
    if path.segments.last().unwrap().ident == *ident {
        return true;
    }

    path.segments.iter().any(|seg| {
        let PathArguments::AngleBracketed(ab) = &seg.arguments else {
            return false;
        };

        ab.args.iter().any(|arg| {
            let GenericArgument::Type(Type::Path(TypePath { path, .. })) = arg else {
                return false;
            };

            has_ident(path, ident)
        })
    })
}

fn has_assoc<'a, I>(path: &'a Path, ident: &'a I) -> Option<&'a Path>
where
    I: ?Sized,
    Ident: PartialEq<I>,
{
    if path.segments.first().unwrap().ident == *ident && path.segments.len() > 1 {
        return Some(path);
    }

    path.segments.iter().find_map(|seg| {
        let PathArguments::AngleBracketed(ab) = &seg.arguments else {
            return None;
        };

        ab.args.iter().find_map(|arg| {
            let GenericArgument::Type(Type::Path(TypePath { path, .. })) = arg else {
                return None;
            };

            has_assoc(path, ident)
        })
    })
}

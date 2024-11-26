use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, Attribute, Data, DataStruct, DeriveInput, Error, Expr, ExprLit,
    Field, Fields, GenericArgument, GenericParam, Generics, Ident, Lit, LitStr, Meta,
    MetaNameValue, Path, PathArguments, Result, Type, TypeParam, TypePath, WherePredicate,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let DeriveInput {
        attrs,
        ident,
        generics,
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

    derive_inner(attrs, ident, generics, fields)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

struct AddDebugTarget {
    generic: TypeParam,
    where_predicates: Vec<WherePredicate>,
}

fn derive_inner(
    attrs: Vec<Attribute>,
    ident: Ident,
    mut generics: Generics,
    fields: Fields,
) -> Result<TokenStream2> {
    let mut add_debug_targets: Vec<AddDebugTarget> = generics
        .params
        .iter()
        .filter_map(|param| {
            if let GenericParam::Type(ref type_param) = param {
                Some(AddDebugTarget {
                    generic: type_param.clone(),
                    where_predicates: Vec::new(),
                })
            } else {
                None
            }
        })
        .collect();

    let mut extra_bounds: Vec<WherePredicate> = attrs
        .into_iter()
        .map(|attr| parse_extra_bound(attr))
        .collect::<Result<Vec<_>>>()?;

    let fields = fields
        .into_iter()
        .map(|f| field_method(f, &mut add_debug_targets, &mut extra_bounds))
        .collect::<Result<Vec<_>>>()?;

    let where_clause = generics.make_where_clause();
    where_clause.predicates.extend(extra_bounds);
    for AddDebugTarget {
        where_predicates, ..
    } in add_debug_targets
    {
        where_clause.predicates.extend(where_predicates);
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

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

fn field_method(
    field: Field,
    add_debug_targets: &mut Vec<AddDebugTarget>,
    extra_bounds: &mut Vec<WherePredicate>,
) -> Result<TokenStream2> {
    let Field {
        ident: Some(ident),
        attrs,
        ..
    } = field
    else {
        return Err(Error::new(field.span(), "expected named fields."));
    };

    let FieldAttrInfo {
        fmt,
        extra_bounds: append,
    } = parse_field_attrs(attrs)?;

    // if no debug(bound = "...") and type of field involve T, then add T: Debug
    if extra_bounds.is_empty() {
        if let Type::Path(TypePath { path, .. }) = &field.ty {
            check_need_debugs(path, add_debug_targets);
        } // else, give up and make user use #[debug(bound = "...")]
    }

    extra_bounds.extend(append);

    let res = if let Some(fmt) = fmt {
        quote! { .field(::std::stringify!(#ident), &::std::format_args!(#fmt, &self.#ident)) }
    } else {
        quote! { .field(::std::stringify!(#ident), &self.#ident) }
    };

    Ok(res)
}

struct FieldAttrInfo {
    fmt: Option<LitStr>,
    extra_bounds: Vec<WherePredicate>,
}

// #[debug(bound = "T: Debug")]
fn parse_extra_bound(attr: Attribute) -> Result<WherePredicate> {
    let MetaNameValue {
        path,
        eq_token: _,
        value,
    } = attr.parse_args()?;

    let path = path.require_ident()?;
    if path != "bound" {
        return Err(Error::new_spanned(
            attr.meta.clone(),
            "expected `debug(bound = \"...\")`",
        ));
    }

    let Expr::Lit(ExprLit {
        lit: Lit::Str(lit), ..
    }) = value
    else {
        return Err(Error::new(value.span(), "expected a string literal."));
    };

    Ok(lit.parse()?)
}

// #[debug = "fmt"]
fn parse_fmt_attr(value: Expr) -> Result<LitStr> {
    let Expr::Lit(ExprLit {
        lit: Lit::Str(lit), ..
    }) = value
    else {
        return Err(Error::new(value.span(), "expected a string literal."));
    };

    Ok(lit)
}

fn parse_field_attrs(attrs: Vec<Attribute>) -> Result<FieldAttrInfo> {
    let mut fmt = None;
    let mut extra_bounds = Vec::new();

    for attr in attrs {
        if !attr.path().is_ident("debug") {
            continue;
        }

        match attr.meta {
            Meta::List(_) => {
                let bound = parse_extra_bound(attr)?;
                extra_bounds.push(bound);
            }
            Meta::NameValue(MetaNameValue {
                path: _,
                eq_token: _,
                value,
            }) => {
                fmt = Some(parse_fmt_attr(value)?);
            }
            _ => (),
        }
    }

    Ok(FieldAttrInfo { fmt, extra_bounds })
}

fn check_need_debugs(path: &Path, add_debug_targets: &mut Vec<AddDebugTarget>) {
    if path.segments.last().unwrap().ident == "PhantomData" {
        return;
    }

    for AddDebugTarget {
        generic,
        where_predicates,
    } in add_debug_targets.iter_mut()
    {
        let ident = generic.ident.clone();

        if has_ident(path, &ident) {
            where_predicates.push(parse_quote! {
                #ident: ::std::fmt::Debug
            });
        }

        if let Some(assoc) = has_assoc(path, &ident) {
            where_predicates.push(parse_quote! {
                #assoc: ::std::fmt::Debug
            });
        }
    }
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

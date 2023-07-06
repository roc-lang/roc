extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DataEnum, DataStruct, DeriveInput, Generics, Ident};

#[proc_macro_derive(Syntax, attributes(malformed))]
pub fn syntax_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let malformed_impl = impl_malformed(&input);

    // Build the impl
    let expanded = quote! {
        #malformed_impl
    };

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}

/// Generate the `Malformed` impl for the given type.
/// ```
/// pub trait Malformed {
///     /// Returns whether this node is malformed, or contains a malformed node (recursively).
///     fn is_malformed(&self) -> bool;
/// }
/// ```
///
/// This impl defers to the Malformed impl of all fields, except in the case of an enum variant
/// that's marked as `#[malformed]`, in which case it returns `true` immediately.
fn impl_malformed(input: &DeriveInput) -> proc_macro2::TokenStream {
    match &input.data {
        syn::Data::Struct(s) => impl_malformed_struct(&input.ident, &input.generics, s),
        syn::Data::Enum(e) => impl_malformed_enum(&input.ident, &input.generics, e),
        syn::Data::Union(_) => panic!("Malformed cannot be derived for unions"),
    }
}

fn impl_malformed_struct(
    ident: &Ident,
    generics: &Generics,
    s: &DataStruct,
) -> proc_macro2::TokenStream {
    let fields = s.fields.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();

        quote! {
            if self.#field_name.is_malformed() {
                return true;
            }
        }
    });

    // Add `: Malformed` bound to each type parameter in the generics.
    let mut generics = generics.clone();
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(Malformed));
        }
    }

    // Prepare the (impl_generics, ty_generics, where_clause) for the impl.
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics Malformed for #ident #ty_generics #where_clause {
            fn is_malformed(&self) -> bool {
                #(#fields)*
                false
            }
        }
    }
}

fn impl_malformed_enum(
    ident: &Ident,
    generics: &Generics,
    e: &DataEnum,
) -> proc_macro2::TokenStream {
    let variants = e
        .variants
        .iter()
        .map(|variant| {
            let variant_ident = &variant.ident;
            let is_malformed = variant
                .attrs
                .iter()
                .any(|attr| attr.path.is_ident("malformed"));
            if is_malformed {
                return match &variant.fields {
                    syn::Fields::Named(_) => {
                        quote! {
                            #ident::#variant_ident { .. } => true
                        }
                    }
                    syn::Fields::Unnamed(_) => {
                        quote! {
                            #ident::#variant_ident(..) => true
                        }
                    }
                    syn::Fields::Unit => {
                        quote! {
                            #ident::#variant_ident => true
                        }
                    }
                };
            }

            match &variant.fields {
                syn::Fields::Named(named) => {
                    let field_names: Vec<_> = named
                        .named
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap())
                        .collect();
                    let field_checks: Vec<_> = field_names
                        .iter()
                        .map(|name| quote! { if #name.is_malformed() { return true; } })
                        .collect();
                    quote! {
                        #ident::#variant_ident { #(#field_names),* } => {
                            #(#field_checks)*
                            false
                        }
                    }
                }
                syn::Fields::Unnamed(unnamed) => {
                    let field_names: Vec<_> = unnamed
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, _)| Ident::new(&format!("field_{}", i), Span::call_site()))
                        .collect();
                    let field_checks: Vec<_> = field_names
                        .iter()
                        .map(|name| quote! { if #name.is_malformed() { return true; } })
                        .collect();
                    quote! {
                        #ident::#variant_ident(#(#field_names),*) => {
                            #(#field_checks)*
                            false
                        }
                    }
                }
                syn::Fields::Unit => {
                    quote! {
                        #ident::#variant_ident => false
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    // Add `: Malformed` bound to each type parameter in the generics.
    let mut generics = generics.clone();
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(Malformed));
        }
    }

    // Prepare the (impl_generics, ty_generics, where_clause) for the impl.
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics Malformed for #ident #ty_generics #where_clause {
            fn is_malformed(&self) -> bool {
                match self {
                    #(#variants),*
                }
            }
        }
    }
}

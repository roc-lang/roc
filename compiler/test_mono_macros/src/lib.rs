#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::clippy::float_cmp)]

extern crate proc_macro;

use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::spanned::Spanned;

// pub mod gen_compare;
// pub mod gen_dict;
// pub mod gen_hash;
// pub mod gen_list;
// pub mod gen_num;
// pub mod gen_primitives;
// pub mod gen_records;
// pub mod gen_result;
// pub mod gen_set;
// pub mod gen_str;
// pub mod gen_tags;
// mod helpers;

#[proc_macro_attribute]
pub fn mono_test(args: TokenStream, item: TokenStream) -> TokenStream {
    let macro_args = syn::parse_macro_input!(args as syn::AttributeArgs);
    let task_fn = syn::parse_macro_input!(item as syn::ItemFn);

    let mut arg_names: syn::punctuated::Punctuated<syn::Ident, syn::Token![,]> =
        syn::punctuated::Punctuated::new();
    let mut args = task_fn.sig.inputs.clone();

    let name = task_fn.sig.ident.clone();
    let body = task_fn.block.clone();

    let visibility = &task_fn.vis;

    let result = quote! {
        #visibility fn #name(#args) {
            println!( #body);
        }
    };
    result.into()
}

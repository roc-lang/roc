//! Macros for use in `test_mono`.
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_attribute]
pub fn mono_test(_args: TokenStream, item: TokenStream) -> TokenStream {
    let task_fn = syn::parse_macro_input!(item as syn::ItemFn);

    let args = task_fn.sig.inputs.clone();

    let name = task_fn.sig.ident.clone();
    let name_str = name.to_string();
    let body = task_fn.block.clone();

    let visibility = &task_fn.vis;
    let attributes = task_fn.attrs;

    let result = quote! {
        #[test]
        #(#attributes)*
        #visibility fn #name(#args) {
            compiles_to_ir(#name_str, #body);

        }
    };
    result.into()
}

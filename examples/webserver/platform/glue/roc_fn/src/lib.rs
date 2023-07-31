extern crate proc_macro;

use core::panic;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, Ident, ItemFn, Lit, Meta, NestedMeta};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct HostedFn {
    name: &'static str,
    arg_types: &'static [&'static str],
    ret_type: &'static str,
}

const ROC_HOSTED_FNS: &[HostedFn] = &[
    HostedFn {
        name: "sendRequest",
        arg_types: &["&roc_app::Request"],
        ret_type: "roc_app::Response",
    },
    HostedFn {
        name: "tcpConnect",
        arg_types: &["&roc_std::RocStr", "u16"],
        ret_type: "roc_app::ConnectResult",
    },
    HostedFn {
        name: "envVar",
        arg_types: &["&roc_std::RocStr"],
        ret_type: "roc_std::RocResult<RocStr, ()>",
    },
];

fn find_hosted_fn_by_name(name: &str) -> Option<HostedFn> {
    for hosted in ROC_HOSTED_FNS.iter().copied() {
        if hosted.name == name {
            return Some(hosted);
        }
    }

    None
}

#[proc_macro_attribute]
pub fn roc_fn(args: TokenStream, input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input_fn = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as AttributeArgs);

    // Get the name from the attribute's arguments
    let name = match args.first() {
        Some(NestedMeta::Meta(Meta::NameValue(nv))) if nv.path.is_ident("name") => {
            if let Lit::Str(lit) = &nv.lit {
                lit.value()
            } else {
                panic!("Expected a string after `name=`");
            }
        }
        _ => panic!("Expected `name=\"...\"`"),
    };

    let hosted_fn = find_hosted_fn_by_name(&name.as_str()).unwrap_or_else(|| {
        panic!("The Roc platform which `roc glue` was run on does not have a hosted function by the name of {:?}", name);
    });

    let input_fn_name = &input_fn.sig.ident;

    // Generate the appropriate arg names and types, and return type,
    // using the types that were in the .roc file as the source of truth.
    let arg_names = (0..hosted_fn.arg_types.len())
        .into_iter()
        .map(|index| Ident::new(&format!("arg{index}"), Span::call_site()))
        .collect::<Vec<_>>();

    let arg_types: Vec<_> = hosted_fn
        .arg_types
        .iter()
        .zip(arg_names.iter())
        .map(|(arg_type, arg_name)| {
            let arg_type = arg_type
                .parse::<proc_macro2::TokenStream>()
                .expect("Failed to parse argument type");

            quote! { #arg_name: #arg_type }
        })
        .collect();

    let ret_type = hosted_fn
        .ret_type
        .parse::<proc_macro2::TokenStream>()
        .expect("Failed to parse return type");

    // Create the extern "C" function
    let output_fn_name = Ident::new(&format!("roc_fx_{}", name), Span::call_site());

    let gen = quote! {
        #input_fn

        #[no_mangle]
        pub extern "C" fn #output_fn_name(#(#arg_types),*) -> #ret_type {
            #input_fn_name(#(#arg_names),*)
        }
    };

    gen.into()
}

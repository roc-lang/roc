#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::float_cmp)]

pub mod gen_abilities;
pub mod gen_compare;
pub mod gen_definitions;
pub mod gen_dict;
pub mod gen_erased;
pub mod gen_list;
pub mod gen_num;
pub mod gen_panic;
pub mod gen_primitives;
pub mod gen_records;
pub mod gen_refcount;
pub mod gen_result;
pub mod gen_return;
pub mod gen_set;
pub mod gen_str;
pub mod gen_tags;
pub mod gen_tuples;
mod helpers;

#[cfg(feature = "gen-wasm")]
pub mod wasm_linking;

pub use helpers::platform_functions::*;

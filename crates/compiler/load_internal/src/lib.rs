//! The internal implementation of roc_load, separate from roc_load to support caching.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

use roc_module::symbol::ModuleId;
pub mod docs;
pub mod file;
pub mod module;
mod module_cache;

#[cfg(target_family = "wasm")]
mod wasm_instant;

pub const BUILTIN_MODULES: &[(ModuleId, &str)] = &[
    (ModuleId::BOOL, "Bool"),
    (ModuleId::RESULT, "Result"),
    (ModuleId::NUM, "Num"),
    (ModuleId::LIST, "List"),
    (ModuleId::STR, "Str"),
    (ModuleId::DICT, "Dict"),
    (ModuleId::SET, "Set"),
    (ModuleId::BOX, "Box"),
    (ModuleId::ENCODE, "Encode"),
    (ModuleId::DECODE, "Decode"),
    (ModuleId::HASH, "Hash"),
    (ModuleId::INSPECT, "Inspect"),
];

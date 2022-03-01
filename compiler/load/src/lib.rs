#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod docs;
pub mod file;
mod work;

#[cfg(target_family = "wasm")]
mod wasm_system_time;

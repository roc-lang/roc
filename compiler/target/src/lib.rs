#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub struct TargetInfo {
    architecture: Architecture,
}

pub enum Architecture {
    X86_64,
    X86_32,
    Aarch64,
    Arm,
    Wasm32,
}

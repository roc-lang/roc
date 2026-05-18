//! WebAssembly backend surface for statement-only LIR.
//!
//! The active wasm code generator consumes strongest-form LIR directly.
//! Ownership boundary:
//! - wasm may lower explicit LIR RC statements
//! - builtin/runtime helpers may perform primitive-internal RC
//! - ordinary wasm lowering is forbidden from inventing ownership policy

pub const WasmModule = @import("WasmModule.zig");
pub const WasmCodeGen = @import("WasmCodeGen.zig");
pub const WasmLayout = @import("WasmLayout.zig");
pub const Storage = @import("Storage.zig");

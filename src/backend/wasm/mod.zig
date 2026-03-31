//! WebAssembly backend surface for statement-only LIR.
//!
//! The active wasm code generator consumes strongest-form LIR directly.

pub const WasmModule = @import("WasmModule.zig");
pub const WasmCodeGen = @import("WasmCodeGen.zig");
pub const WasmLayout = @import("WasmLayout.zig");
pub const Storage = @import("Storage.zig");

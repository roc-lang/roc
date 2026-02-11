//! WebAssembly code generation backend.
//!
//! Generates wasm bytecode from Mono IR. Unlike the dev backend (which uses
//! a register-based code generator), the wasm backend is a standalone
//! code generator since wasm is a stack machine.

pub const WasmModule = @import("WasmModule.zig");
pub const WasmCodeGen = @import("WasmCodeGen.zig");
pub const WasmLayout = @import("WasmLayout.zig");
pub const Storage = @import("Storage.zig");

//! Typed indices for WebAssembly module spaces used by the wasm backend.
//!
//! These wrappers keep function indices, defined-function local indices, and
//! linking symbol indices separate until the final wasm binary encoding boundary.

/// Global WebAssembly function index, including imported functions.
pub const FunctionIndex = enum(u32) {
    _,

    /// Return the raw wasm function index.
    pub fn raw(self: FunctionIndex) u32 {
        return @intFromEnum(self);
    }

    /// Wrap a raw wasm function index.
    pub fn fromRaw(value: u32) FunctionIndex {
        return @enumFromInt(value);
    }
};

/// Defined-function index local to the wasm function section.
pub const LocalFunctionIndex = enum(u32) {
    _,

    /// Return the raw function-section-local index.
    pub fn raw(self: LocalFunctionIndex) u32 {
        return @intFromEnum(self);
    }

    /// Wrap a raw function-section-local index.
    pub fn fromRaw(value: u32) LocalFunctionIndex {
        return @enumFromInt(value);
    }
};

/// Index into the wasm linking symbol table.
pub const SymbolIndex = enum(u32) {
    _,

    /// Return the raw linking symbol index.
    pub fn raw(self: SymbolIndex) u32 {
        return @intFromEnum(self);
    }

    /// Wrap a raw linking symbol index.
    pub fn fromRaw(value: u32) SymbolIndex {
        return @enumFromInt(value);
    }
};

/// Function indices returned when adding a defined wasm function.
pub const DefinedFunction = struct {
    local: LocalFunctionIndex,
    function: FunctionIndex,
};

/// Convert a defined-function local index to a global function index.
pub fn localToFunction(local: LocalFunctionIndex, import_count: u32) FunctionIndex {
    return FunctionIndex.fromRaw(import_count + local.raw());
}

/// Convert a global function index to a defined-function local index.
pub fn functionToLocal(function: FunctionIndex, import_count: u32) LocalFunctionIndex {
    return LocalFunctionIndex.fromRaw(function.raw() - import_count);
}

test "typed wasm index conversions" {
    const local = LocalFunctionIndex.fromRaw(7);
    const function = localToFunction(local, 3);
    try @import("std").testing.expectEqual(@as(u32, 10), function.raw());
    try @import("std").testing.expectEqual(local, functionToLocal(function, 3));
}

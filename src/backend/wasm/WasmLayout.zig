//! Maps layout.Idx to wasm value types.
//!
//! Determines how Roc types are represented in wasm:
//! - Primitives that fit in a wasm value type are returned directly
//! - Composites (i128, Dec, Str, List, records) use linear memory

const layout = @import("layout");
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

/// How a Roc value is represented in wasm
pub const WasmRepr = union(enum) {
    /// Value fits in a single wasm value type (returned directly)
    primitive: ValType,
    /// Value lives in linear memory (returned as i32 pointer)
    stack_memory: u32, // size in bytes
};

/// Map a layout.Idx to its wasm representation.
pub fn wasmRepr(layout_idx: layout.Idx) WasmRepr {
    return switch (layout_idx) {
        .bool, .u8, .i8 => .{ .primitive = .i32 },
        .u16, .i16 => .{ .primitive = .i32 },
        .u32, .i32 => .{ .primitive = .i32 },
        .u64, .i64 => .{ .primitive = .i64 },
        .f32 => .{ .primitive = .f32 },
        .f64 => .{ .primitive = .f64 },
        .i128, .u128 => .{ .stack_memory = 16 },
        .dec => .{ .stack_memory = 16 },
        .str => .{ .stack_memory = 12 }, // wasm32: ptr(4) + len(4) + cap(4)
        .opaque_ptr => .{ .primitive = .i32 }, // pointers are i32 on wasm32
        else => .{ .stack_memory = 0 }, // TODO: look up from layout store for records/tuples/tags
    };
}

/// Get the wasm ValType for a result that is returned directly from a function.
/// For primitives, this is the value type itself.
/// For composites, the function returns an i32 pointer to linear memory.
pub fn resultValType(layout_idx: layout.Idx) ValType {
    return switch (wasmRepr(layout_idx)) {
        .primitive => |vt| vt,
        .stack_memory => .i32,
    };
}

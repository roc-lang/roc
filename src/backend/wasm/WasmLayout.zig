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
/// For composite types (records, tuples, tags), returns stack_memory with size 0.
/// Use wasmReprWithStore for accurate composite sizes.
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
        else => .{ .stack_memory = 0 }, // composite — use wasmReprWithStore for size
    };
}

/// Map a layout.Idx to its wasm representation using the layout store for
/// composite types (records, tuples, tags).
pub fn wasmReprWithStore(layout_idx: layout.Idx, ls: *const layout.Store) WasmRepr {
    // Try the scalar path first
    const basic = wasmRepr(layout_idx);
    switch (basic) {
        .primitive => return basic,
        .stack_memory => |size| {
            if (size > 0) return basic; // known size (i128, dec, str)
            // Composite type — look up from layout store
            const l = ls.getLayout(layout_idx);
            return switch (l.tag) {
                .scalar => .{ .primitive = scalarValType(l) },
                .record => .{ .stack_memory = ls.layoutSize(l) },
                .tuple => .{ .stack_memory = ls.layoutSize(l) },
                .tag_union => blk: {
                    const size2 = ls.layoutSize(l);
                    const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
                    // Discriminant-only tag unions (enums, disc_offset == 0) with size ≤ 4
                    // are treated as i32 primitives. Tag unions with payloads
                    // (disc_offset > 0) always use stack memory so the payload
                    // can be stored and extracted correctly.
                    if (size2 <= 4 and tu_data.discriminant_offset == 0) break :blk .{ .primitive = .i32 };
                    break :blk .{ .stack_memory = size2 };
                },
                .zst => .{ .primitive = .i32 }, // zero-sized, dummy i32
                .box, .box_of_zst => .{ .primitive = .i32 }, // pointer
                .list, .list_of_zst => .{ .stack_memory = 12 }, // RocList
                .closure => blk: {
                    // For unwrapped_capture closures, the runtime value IS the capture
                    // value itself (not a pointer). Resolve the captures layout to check.
                    const captures_repr = wasmReprWithStore(l.data.closure.captures_layout_idx, ls);
                    break :blk switch (captures_repr) {
                        .primitive => captures_repr,
                        .stack_memory => .{ .stack_memory = ls.layoutSize(l) },
                    };
                },
            };
        },
    }
}

/// Extract ValType from a scalar Layout.
fn scalarValType(l: layout.Layout) ValType {
    return switch (l.data.scalar.tag) {
        .int => switch (l.data.scalar.data.int) {
            .u8, .i8, .u16, .i16, .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128 => .i32, // pointer to stack memory
        },
        .frac => switch (l.data.scalar.data.frac) {
            .f32 => .f32,
            .f64 => .f64,
            .dec => .i32, // pointer to stack memory
        },
        .str => .i32, // pointer
        .opaque_ptr => .i32,
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

/// Get the wasm ValType for a result, using the layout store for composites.
pub fn resultValTypeWithStore(layout_idx: layout.Idx, ls: *const layout.Store) ValType {
    return switch (wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| vt,
        .stack_memory => .i32,
    };
}

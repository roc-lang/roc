//! Maps layout.Idx to wasm value types.
//!
//! Determines how Roc types are represented in wasm:
//! - Primitives that fit in a wasm value type are returned directly
//! - Composites (i128, Dec, Str, List, records) use linear memory
//!
//! This module only performs the wasm-specific classification (which layouts map
//! to a wasm value type versus linear memory). Every concrete size, offset,
//! alignment, and discriminant width is read from `layout.Store` at the wasm32
//! pointer width (`.u32`), which is the single source of truth shared with the
//! dev and LLVM backends.

const base = @import("base");
const layout = @import("layout");
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

/// The pointer width wasm codegen targets. wasm32 uses 32-bit pointers, so every
/// store query in this module is made at this width.
const wasm_target: base.target.TargetUsize = .u32;

/// Error set for the fallible wasm layout query API, keeping these queries
/// uniform with the rest of wasm codegen's fallible layout accessors.
pub const Error = error{OutOfMemory};

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
        .u8, .i8 => .{ .primitive = .i32 },
        .u16, .i16 => .{ .primitive = .i32 },
        .u32, .i32 => .{ .primitive = .i32 },
        .u64, .i64 => .{ .primitive = .i64 },
        .f32 => .{ .primitive = .f32 },
        .f64 => .{ .primitive = .f64 },
        .i128, .u128 => .{ .stack_memory = 16 },
        .dec => .{ .stack_memory = 16 },
        .str => .{ .stack_memory = 12 }, // wasm32: ptr(4) + encoded cap(4) + len(4)
        else => .{ .stack_memory = 0 }, // composite — use wasmReprWithStore for size
    };
}

/// Map a layout.Idx to its wasm representation using the layout store for
/// composite types (records, tuples, tags).
pub fn wasmReprWithStore(layout_idx: layout.Idx, ls: *const layout.Store) Error!WasmRepr {
    // For unwrapped_capture closures, the runtime value IS the capture value
    // itself, so a closure's representation is its captures' representation —
    // except that a stack_memory captures makes the closure live in stack memory
    // sized to this outermost closure. We follow the closure-capture chain with a
    // loop (no recursion) and remember the outermost closure.
    var idx = layout_idx;
    var outer_closure: ?layout.Layout = null;
    while (true) {
        // Try the scalar fast-path first; named scalar idxs are not valid for getLayout.
        const basic = wasmRepr(idx);
        switch (basic) {
            .primitive => return basic, // a primitive captures makes the closure that primitive
            .stack_memory => |size| {
                if (size > 0) {
                    // Known-size scalar (i128, dec, str).
                    if (outer_closure) |outer| return .{ .stack_memory = ls.layoutSize(outer) };
                    return basic;
                }
                // Composite type — look up from layout store.
                const l = ls.getLayout(idx);
                if (l.tag == .closure) {
                    if (outer_closure == null) outer_closure = l;
                    idx = l.getClosure().captures_layout_idx;
                    continue;
                }
                const repr: WasmRepr = switch (l.tag) {
                    .scalar => .{ .primitive = scalarValType(l) },
                    .struct_ => .{ .stack_memory = ls.sizeAt(l, wasm_target) },
                    .tag_union => blk: {
                        const tu_layout = try tagUnionLayoutWithStore(l.getTagUnion().idx, ls);
                        // Discriminant-only tag unions (enums, disc_offset == 0) with size ≤ 4
                        // are treated as i32 primitives. Tag unions with payloads
                        // (disc_offset > 0) always use stack memory so the payload
                        // can be stored and extracted correctly.
                        if (tu_layout.size <= 4 and tu_layout.discriminant_offset == 0) break :blk .{ .primitive = .i32 };
                        break :blk .{ .stack_memory = tu_layout.size };
                    },
                    .zst => .{ .primitive = .i32 }, // zero-sized, dummy i32
                    .box, .box_of_zst, .erased_callable, .ptr => .{ .primitive = .i32 }, // pointer
                    .list, .list_of_zst => .{ .stack_memory = ls.sizeAt(l, wasm_target) }, // RocList
                    .closure => unreachable, // handled above
                };
                if (outer_closure) |outer| {
                    return switch (repr) {
                        .primitive => repr,
                        .stack_memory => .{ .stack_memory = ls.layoutSize(outer) },
                    };
                }
                return repr;
            },
        }
    }
}

/// wasm-level size, discriminant offset, and discriminant width of a tag union,
/// all read from the layout store at the wasm32 pointer width.
pub const TagUnionWasmLayout = struct {
    size: u32,
    discriminant_offset: u32,
    discriminant_size: u8,
};

/// wasm32 byte size of a struct, read from the layout store's committed size.
pub fn structSizeWithStore(struct_idx: layout.StructIdx, ls: *const layout.Store) Error!u32 {
    return ls.getStructSizeAt(struct_idx, wasm_target);
}

/// wasm32 byte alignment of a layout, read from its committed alignment class.
pub fn layoutAlignWasm(l: layout.Layout) u32 {
    return @intCast(l.alignment(wasm_target).toByteUnits());
}

/// wasm32 size/discriminant metrics of a tag union, all read from the store.
pub fn tagUnionLayoutWithStore(tu_idx: layout.TagUnionIdx, ls: *const layout.Store) Error!TagUnionWasmLayout {
    return .{
        .size = ls.getTagUnionSizeAt(tu_idx, wasm_target),
        .discriminant_offset = ls.getTagUnionDiscriminantOffsetAt(tu_idx, wasm_target),
        .discriminant_size = ls.getTagUnionDiscriminantSize(tu_idx),
    };
}

/// Extract ValType from a scalar Layout.
fn scalarValType(l: layout.Layout) ValType {
    return switch (l.getScalar().tag) {
        .int => switch (l.getScalar().getInt()) {
            .u8, .i8, .u16, .i16, .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128 => .i32, // pointer to stack memory
        },
        .frac => switch (l.getScalar().getFrac()) {
            .f32 => .f32,
            .f64 => .f64,
            .dec => .i32, // pointer to stack memory
        },
        .opaque_ptr => .i32,
        .str => .i32, // pointer
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
pub fn resultValTypeWithStore(layout_idx: layout.Idx, ls: *const layout.Store) Error!ValType {
    return switch (try wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| vt,
        .stack_memory => .i32,
    };
}

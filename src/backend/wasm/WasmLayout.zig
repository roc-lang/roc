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
        .u8, .i8 => .{ .primitive = .i32 },
        .u16, .i16 => .{ .primitive = .i32 },
        .u32, .i32 => .{ .primitive = .i32 },
        .u64, .i64 => .{ .primitive = .i64 },
        .f32 => .{ .primitive = .f32 },
        .f64 => .{ .primitive = .f64 },
        .i128, .u128 => .{ .stack_memory = 16 },
        .dec => .{ .stack_memory = 16 },
        .str => .{ .stack_memory = 12 }, // wasm32: ptr(4) + len(4) + cap(4)
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
                .struct_ => .{ .stack_memory = structSizeWasm(ls, l.data.struct_.idx) },
                .tag_union => blk: {
                    const tu_layout = tagUnionLayoutWithStore(l.data.tag_union.idx, ls);
                    // Discriminant-only tag unions (enums, disc_offset == 0) with size ≤ 4
                    // are treated as i32 primitives. Tag unions with payloads
                    // (disc_offset > 0) always use stack memory so the payload
                    // can be stored and extracted correctly.
                    if (tu_layout.size <= 4 and tu_layout.discriminant_offset == 0) break :blk .{ .primitive = .i32 };
                    break :blk .{ .stack_memory = tu_layout.size };
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

/// Public struct `TagUnionWasmLayout`.
pub const TagUnionWasmLayout = struct {
    size: u32,
    discriminant_offset: u32,
    discriminant_size: u8,
    alignment: u32,
};

/// Public function `structSizeWithStore`.
pub fn structSizeWithStore(struct_idx: layout.StructIdx, ls: *const layout.Store) u32 {
    return structSizeWasm(ls, struct_idx);
}

/// Public function `structAlignWithStore`.
pub fn structAlignWithStore(struct_idx: layout.StructIdx, ls: *const layout.Store) u32 {
    return structAlignWasm(ls, struct_idx);
}

/// Public function `tagUnionLayoutWithStore`.
pub fn tagUnionLayoutWithStore(tu_idx: layout.TagUnionIdx, ls: *const layout.Store) TagUnionWasmLayout {
    const tu_data = ls.getTagUnionData(tu_idx);
    const variants = ls.getTagUnionVariants(tu_data);

    var max_payload_size: u32 = 0;
    var max_payload_align: u32 = 1;
    for (0..variants.len) |i| {
        const payload_layout = variants.get(i).payload_layout;
        const payload_size = layoutStorageByteSizeWasm(payload_layout, ls);
        const payload_align = layoutByteAlignWasm(payload_layout, ls);
        if (payload_size > max_payload_size) max_payload_size = payload_size;
        if (payload_align > max_payload_align) max_payload_align = payload_align;
    }

    const discriminant_size: u8 = tagUnionDiscriminantSize(variants.len);
    const disc_align = layout.TagUnionData.alignmentForDiscriminantSize(discriminant_size);
    const disc_align_bytes: u32 = @intCast(disc_align.toByteUnits());
    const discriminant_offset: u32 = alignUp(max_payload_size, disc_align_bytes);
    const tag_union_alignment: u32 = if (max_payload_align > disc_align_bytes) max_payload_align else disc_align_bytes;
    const total_size: u32 = alignUp(discriminant_offset + discriminant_size, tag_union_alignment);

    return .{
        .size = total_size,
        .discriminant_offset = discriminant_offset,
        .discriminant_size = discriminant_size,
        .alignment = tag_union_alignment,
    };
}

fn layoutStorageByteSizeWasm(layout_idx: layout.Idx, ls: *const layout.Store) u32 {
    const l = ls.getLayout(layout_idx);
    return switch (l.tag) {
        .zst => 0,
        .scalar => switch (l.data.scalar.tag) {
            .str => 12,
            .opaque_ptr => 4,
            .int => switch (l.data.scalar.data.int) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                .u128, .i128 => 16,
            },
            .frac => switch (l.data.scalar.data.frac) {
                .f32 => 4,
                .f64 => 8,
                .dec => 16,
            },
        },
        .list, .list_of_zst => 12,
        .box, .box_of_zst => 4,
        .struct_ => structSizeWasm(ls, l.data.struct_.idx),
        .tag_union => tagUnionLayoutWithStore(l.data.tag_union.idx, ls).size,
        .closure => ls.layoutSize(l),
    };
}

fn layoutByteAlignWasm(layout_idx: layout.Idx, ls: *const layout.Store) u32 {
    const l = ls.getLayout(layout_idx);
    return switch (l.tag) {
        .zst => 1,
        .scalar => switch (l.data.scalar.tag) {
            .str => 4,
            .opaque_ptr => 4,
            .int => @intCast(l.data.scalar.data.int.alignment().toByteUnits()),
            .frac => @intCast(l.data.scalar.data.frac.alignment().toByteUnits()),
        },
        .list, .list_of_zst, .box, .box_of_zst => 4,
        .struct_ => structAlignWasm(ls, l.data.struct_.idx),
        .tag_union => tagUnionLayoutWithStore(l.data.tag_union.idx, ls).alignment,
        .closure => @intCast(ls.layoutSizeAlign(l).alignment.toByteUnits()),
    };
}

fn structAlignWasm(ls: *const layout.Store, struct_idx: layout.StructIdx) u32 {
    const sd = ls.getStructData(struct_idx);
    const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
    var max_align: u32 = 1;
    for (0..sorted_fields.len) |i| {
        const field = sorted_fields.get(i);
        const field_align = layoutByteAlignWasm(field.layout, ls);
        if (field_align > max_align) max_align = field_align;
    }
    return max_align;
}

fn structSizeWasm(ls: *const layout.Store, struct_idx: layout.StructIdx) u32 {
    const sd = ls.getStructData(struct_idx);
    const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
    var offset: u32 = 0;
    var max_align: u32 = 1;
    for (0..sorted_fields.len) |i| {
        const field = sorted_fields.get(i);
        const field_align = layoutByteAlignWasm(field.layout, ls);
        const field_size = layoutStorageByteSizeWasm(field.layout, ls);
        if (field_align > max_align) max_align = field_align;
        offset = alignUp(offset, field_align);
        offset += field_size;
    }
    return alignUp(offset, max_align);
}

fn tagUnionDiscriminantSize(variant_count: usize) u8 {
    return if (variant_count <= 1)
        0
    else if (variant_count <= 256)
        1
    else if (variant_count <= 65536)
        2
    else if (variant_count <= (1 << 32))
        4
    else
        8;
}

fn alignUp(value: u32, alignment: u32) u32 {
    if (alignment <= 1) return value;
    return (value + alignment - 1) & ~(alignment - 1);
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
pub fn resultValTypeWithStore(layout_idx: layout.Idx, ls: *const layout.Store) ValType {
    return switch (wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| vt,
        .stack_memory => .i32,
    };
}

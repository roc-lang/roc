//! Maps layout.Idx to wasm value types.
//!
//! Determines how Roc types are represented in wasm:
//! - Primitives that fit in a wasm value type are returned directly
//! - Composites (i128, Dec, Str, List, records) use linear memory
//!
//! Size and alignment of composite layouts are computed iteratively (via
//! `wasmSizeAlign`) using an explicit work stack, so deeply nested types never
//! grow the native call stack.

const std = @import("std");
const layout = @import("layout");
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

/// Errors returned while computing wasm layout sizes (only out-of-memory, when
/// an unusually deep type spills the inline work-stack buffer onto the heap).
pub const Error = error{OutOfMemory};

/// How a Roc value is represented in wasm
pub const WasmRepr = union(enum) {
    /// Value fits in a single wasm value type (returned directly)
    primitive: ValType,
    /// Value lives in linear memory (returned as i32 pointer)
    stack_memory: u32, // size in bytes
};

/// Size and alignment of a layout in wasm linear memory (bytes).
const SizeAlign = struct {
    size: u32,
    alignment: u32,
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
                    .struct_ => .{ .stack_memory = try structSizeWasm(ls, l.getStruct().idx) },
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
                    .list, .list_of_zst => .{ .stack_memory = 12 }, // RocList
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

/// Public struct `TagUnionWasmLayout`.
pub const TagUnionWasmLayout = struct {
    size: u32,
    discriminant_offset: u32,
    discriminant_size: u8,
    alignment: u32,
};

/// Public function `structSizeWithStore`.
pub fn structSizeWithStore(struct_idx: layout.StructIdx, ls: *const layout.Store) Error!u32 {
    return structSizeWasm(ls, struct_idx);
}

/// Public function `structAlignWithStore`.
pub fn structAlignWithStore(struct_idx: layout.StructIdx, ls: *const layout.Store) Error!u32 {
    return structAlignWasm(ls, struct_idx);
}

/// Public function `tagUnionLayoutWithStore`.
pub fn tagUnionLayoutWithStore(tu_idx: layout.TagUnionIdx, ls: *const layout.Store) Error!TagUnionWasmLayout {
    const tu_data = ls.getTagUnionData(tu_idx);
    const variants = ls.getTagUnionVariants(tu_data);

    var max_payload_size: u32 = 0;
    var max_payload_align: u32 = 1;
    for (0..variants.len) |i| {
        const payload_layout = variants.get(i).payload_layout;
        const payload = try wasmSizeAlign(payload_layout, ls);
        if (payload.size > max_payload_size) max_payload_size = payload.size;
        if (payload.alignment > max_payload_align) max_payload_align = payload.alignment;
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

fn structAlignWasm(ls: *const layout.Store, struct_idx: layout.StructIdx) Error!u32 {
    const sd = ls.getStructData(struct_idx);
    const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
    var max_align: u32 = 1;
    for (0..sorted_fields.len) |i| {
        const field = sorted_fields.get(i);
        // Padding spacers are alignment 1 and never inflate the struct's alignment.
        if (field.is_padding) continue;
        const field_align = (try wasmSizeAlign(field.layout, ls)).alignment;
        if (field_align > max_align) max_align = field_align;
    }
    return max_align;
}

fn structSizeWasm(ls: *const layout.Store, struct_idx: layout.StructIdx) Error!u32 {
    const sd = ls.getStructData(struct_idx);
    const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
    var offset: u32 = 0;
    var max_align: u32 = 1;
    for (0..sorted_fields.len) |i| {
        const field = sorted_fields.get(i);
        const field_sa = try wasmSizeAlign(field.layout, ls);
        // Padding spacers are alignment 1 (their value layout's alignment is ignored).
        const field_align: u32 = if (field.is_padding) 1 else field_sa.alignment;
        if (field_align > max_align) max_align = field_align;
        offset = alignUp(offset, field_align);
        offset += field_sa.size;
    }
    return alignUp(offset, max_align);
}

/// Compute the wasm linear-memory size and alignment of a layout. Walks nested
/// records/tag-union payloads/closures with an explicit work stack (post-order)
/// instead of recursion. Boxes are pointers, which is where recursive types
/// bottom out, so the traversal always terminates.
fn wasmSizeAlign(root_idx: layout.Idx, ls: *const layout.Store) Error!SizeAlign {
    const Item = union(enum) {
        enter: layout.Idx,
        combine_struct: layout.StructIdx,
        combine_tag: u32,
    };

    var work_sfa = std.heap.stackFallback(64 * @sizeOf(Item), ls.allocator);
    const wa = work_sfa.get();
    var work = std.ArrayList(Item).empty;
    defer work.deinit(wa);

    var res_sfa = std.heap.stackFallback(64 * @sizeOf(SizeAlign), ls.allocator);
    const ra = res_sfa.get();
    var results = std.ArrayList(SizeAlign).empty;
    defer results.deinit(ra);

    try work.append(wa, .{ .enter = root_idx });
    while (work.pop()) |item| switch (item) {
        .enter => |idx| {
            const l = ls.getLayout(idx);
            switch (l.tag) {
                .zst => try results.append(ra, .{ .size = 0, .alignment = 1 }),
                .scalar => try results.append(ra, scalarSizeAlign(l)),
                .list, .list_of_zst => try results.append(ra, .{ .size = 12, .alignment = 4 }),
                .box, .box_of_zst, .erased_callable, .ptr => try results.append(ra, .{ .size = 4, .alignment = 4 }),
                .closure => {
                    const sa = ls.layoutSizeAlign(l);
                    try results.append(ra, .{ .size = sa.size, .alignment = @intCast(sa.alignment.toByteUnits()) });
                },
                .struct_ => {
                    const sd = ls.getStructData(l.getStruct().idx);
                    const fields = ls.struct_fields.sliceRange(sd.getFields());
                    try work.append(wa, .{ .combine_struct = l.getStruct().idx });
                    var i: usize = fields.len;
                    while (i > 0) {
                        i -= 1;
                        try work.append(wa, .{ .enter = fields.get(i).layout });
                    }
                },
                .tag_union => {
                    const tu_data = ls.getTagUnionData(l.getTagUnion().idx);
                    const variants = ls.getTagUnionVariants(tu_data);
                    try work.append(wa, .{ .combine_tag = @intCast(variants.len) });
                    var i: usize = variants.len;
                    while (i > 0) {
                        i -= 1;
                        try work.append(wa, .{ .enter = variants.get(i).payload_layout });
                    }
                },
            }
        },
        .combine_struct => |struct_idx| {
            const sd = ls.getStructData(struct_idx);
            const fields = ls.struct_fields.sliceRange(sd.getFields());
            const field_count: u32 = @intCast(fields.len);
            const base = results.items.len - field_count;
            var offset: u32 = 0;
            var max_align: u32 = 1;
            for (results.items[base..], 0..) |field_sa, fi| {
                // Padding spacers are alignment 1 and never inflate the struct's alignment.
                const field_align: u32 = if (fields.get(fi).is_padding) 1 else field_sa.alignment;
                if (field_align > max_align) max_align = field_align;
                offset = alignUp(offset, field_align);
                offset += field_sa.size;
            }
            const size = alignUp(offset, max_align);
            results.shrinkRetainingCapacity(base);
            try results.append(ra, .{ .size = size, .alignment = max_align });
        },
        .combine_tag => |variant_count| {
            const base = results.items.len - variant_count;
            var max_payload_size: u32 = 0;
            var max_payload_align: u32 = 1;
            for (results.items[base..]) |payload| {
                if (payload.size > max_payload_size) max_payload_size = payload.size;
                if (payload.alignment > max_payload_align) max_payload_align = payload.alignment;
            }
            const discriminant_size: u8 = tagUnionDiscriminantSize(variant_count);
            const disc_align = layout.TagUnionData.alignmentForDiscriminantSize(discriminant_size);
            const disc_align_bytes: u32 = @intCast(disc_align.toByteUnits());
            const discriminant_offset: u32 = alignUp(max_payload_size, disc_align_bytes);
            const tag_union_alignment: u32 = if (max_payload_align > disc_align_bytes) max_payload_align else disc_align_bytes;
            const total_size: u32 = alignUp(discriminant_offset + discriminant_size, tag_union_alignment);
            results.shrinkRetainingCapacity(base);
            try results.append(ra, .{ .size = total_size, .alignment = tag_union_alignment });
        },
    };

    return results.items[0];
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

/// Extract the wasm linear-memory size and alignment of a scalar Layout.
fn scalarSizeAlign(l: layout.Layout) SizeAlign {
    return switch (l.getScalar().tag) {
        .str => .{ .size = 12, .alignment = 4 },
        .opaque_ptr => .{ .size = 4, .alignment = 4 },
        .int => .{
            .size = switch (l.getScalar().getInt()) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                .u128, .i128 => 16,
            },
            .alignment = @intCast(l.getScalar().getInt().alignment().toByteUnits()),
        },
        .frac => .{
            .size = switch (l.getScalar().getFrac()) {
                .f32 => 4,
                .f64 => 8,
                .dec => 16,
            },
            .alignment = @intCast(l.getScalar().getFrac().alignment().toByteUnits()),
        },
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

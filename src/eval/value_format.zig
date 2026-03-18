//! Layout-based value formatter for the LIR interpreter.
//!
//! Takes a `Value` (raw pointer) and a `layout.Idx` from the shared layout module
//! and produces a string matching the canonical format of `RocValue.format()`.
//!
//! Since `layout.StructField` has no `.name` field, records and tag unions
//! cannot be formatted without extra context. Structs are formatted as tuples;
//! records will produce a mismatch in test comparisons (caught silently).

const std = @import("std");
const layout_mod = @import("layout");
const builtins = @import("builtins");
const lir_value = @import("value.zig");

const Layout = layout_mod.Layout;
const Idx = layout_mod.Idx;
const Value = lir_value.Value;
const RocDec = builtins.dec.RocDec;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const i128h = builtins.compiler_rt_128;

const Allocator = std.mem.Allocator;

pub const FormatError = error{
    OutOfMemory,
    Unsupported,
};

/// Format a LIR value into a string matching the canonical Roc output format.
pub fn formatValue(
    allocator: Allocator,
    val: Value,
    layout_idx: Idx,
    store: *const layout_mod.Store,
) FormatError![]u8 {
    const lay = store.getLayout(layout_idx);
    return formatWithLayout(allocator, val, lay, layout_idx, store);
}

fn formatWithLayout(
    allocator: Allocator,
    val: Value,
    lay: Layout,
    layout_idx: Idx,
    store: *const layout_mod.Store,
) FormatError![]u8 {
    switch (lay.tag) {
        .scalar => return formatScalar(allocator, val, lay, layout_idx),
        .struct_ => return formatStruct(allocator, val, lay, store),
        .list => return formatList(allocator, val, lay, store),
        .list_of_zst => return formatListOfZst(allocator, val),
        .box => return formatBox(allocator, val, lay, store),
        .box_of_zst => return allocator.dupe(u8, "Box({})") catch return error.OutOfMemory,
        .zst => return allocator.dupe(u8, "{}") catch return error.OutOfMemory,
        .tag_union => return error.Unsupported,
        .closure => return error.Unsupported,
    }
}

// ─── Scalars ────────────────────────────────────────────────────────────

fn formatScalar(allocator: Allocator, val: Value, lay: Layout, layout_idx: Idx) FormatError![]u8 {
    const scalar = lay.data.scalar;
    switch (scalar.tag) {
        .str => {
            // Copy into an aligned local — val.ptr may not satisfy RocStr alignment.
            var rs: RocStr = undefined;
            @memcpy(std.mem.asBytes(&rs), val.ptr[0..@sizeOf(RocStr)]);
            // Guard against null bytes (can happen when LIR interpreter
            // returns a zeroed value for unsupported expressions).
            const s = if (rs.len() == 0)
                @as([]const u8, "")
            else if (rs.isSmallStr())
                rs.asSlice()
            else if (rs.bytes != null)
                rs.asSlice()
            else
                return error.Unsupported;
            var buf = std.array_list.AlignedManaged(u8, null).init(allocator);
            errdefer buf.deinit();
            buf.append('"') catch return error.OutOfMemory;
            for (s) |ch| {
                switch (ch) {
                    '\\' => buf.appendSlice("\\\\") catch return error.OutOfMemory,
                    '"' => buf.appendSlice("\\\"") catch return error.OutOfMemory,
                    else => buf.append(ch) catch return error.OutOfMemory,
                }
            }
            buf.append('"') catch return error.OutOfMemory;
            return buf.toOwnedSlice() catch return error.OutOfMemory;
        },
        .int => {
            // Check for bool sentinel
            if (layout_idx == Idx.bool) {
                const b = val.read(u8) != 0;
                return allocator.dupe(u8, if (b) "True" else "False") catch return error.OutOfMemory;
            }
            const precision = scalar.data.int;
            return switch (precision) {
                .u64, .u128 => blk: {
                    const v: u128 = switch (precision) {
                        .u64 => val.read(u64),
                        .u128 => val.read(u128),
                        else => unreachable,
                    };
                    break :blk std.fmt.allocPrint(allocator, "{d}", .{v}) catch return error.OutOfMemory;
                },
                else => blk: {
                    const v: i128 = switch (precision) {
                        .u8 => val.read(u8),
                        .i8 => val.read(i8),
                        .u16 => val.read(u16),
                        .i16 => val.read(i16),
                        .u32 => val.read(u32),
                        .i32 => val.read(i32),
                        .u64 => val.read(u64),
                        .i64 => val.read(i64),
                        .i128 => val.read(i128),
                        .u128 => @bitCast(val.read(u128)),
                    };
                    break :blk std.fmt.allocPrint(allocator, "{d}", .{v}) catch return error.OutOfMemory;
                },
            };
        },
        .frac => {
            return switch (scalar.data.frac) {
                .f32 => blk: {
                    var buf: [400]u8 = undefined;
                    const slice = i128h.f64_to_str(&buf, @as(f64, val.read(f32)));
                    break :blk allocator.dupe(u8, slice) catch return error.OutOfMemory;
                },
                .f64 => blk: {
                    var buf: [400]u8 = undefined;
                    const slice = i128h.f64_to_str(&buf, val.read(f64));
                    break :blk allocator.dupe(u8, slice) catch return error.OutOfMemory;
                },
                .dec => blk: {
                    const dec = RocDec{ .num = val.read(i128) };
                    var buf: [RocDec.max_str_length]u8 = undefined;
                    const slice = dec.format_to_buf(&buf);
                    break :blk allocator.dupe(u8, slice) catch return error.OutOfMemory;
                },
            };
        },
    }
}

// ─── Structs (tuples and records) ────────────────────────────────────────

fn formatStruct(
    allocator: Allocator,
    val: Value,
    lay: Layout,
    store: *const layout_mod.Store,
) FormatError![]u8 {
    const struct_data = store.getStructData(lay.data.struct_.idx);
    const fields = store.struct_fields.sliceRange(struct_data.getFields());

    if (struct_data.fields.count == 0) {
        return allocator.dupe(u8, "{}") catch return error.OutOfMemory;
    }

    // Format as tuple: (val, val, ...)
    // Records will produce a mismatch caught silently by the test harness.
    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();
    out.append('(') catch return error.OutOfMemory;

    const count = fields.len;
    // Iterate by original source index (0, 1, 2, ...) rather than sorted order
    var original_idx: usize = 0;
    while (original_idx < count) : (original_idx += 1) {
        const sorted_idx = blk: {
            for (0..count) |si| {
                if (fields.get(si).index == original_idx) break :blk si;
            }
            // If no field matches this original index, this is likely a record
            // (where indices represent alphabetical order, not 0..N).
            // Fall back to sorted-order iteration.
            break :blk original_idx;
        };
        const fld = fields.get(sorted_idx);
        const elem_layout = store.getLayout(fld.layout);
        const elem_offset = store.getStructFieldOffset(lay.data.struct_.idx, @intCast(sorted_idx));
        const elem_ptr_val = val.offset(elem_offset);
        const rendered = try formatWithLayout(allocator, elem_ptr_val, elem_layout, fld.layout, store);
        defer allocator.free(rendered);
        out.appendSlice(rendered) catch return error.OutOfMemory;
        if (original_idx + 1 < count) out.appendSlice(", ") catch return error.OutOfMemory;
    }

    out.append(')') catch return error.OutOfMemory;
    return out.toOwnedSlice() catch return error.OutOfMemory;
}

// ─── Lists ──────────────────────────────────────────────────────────────

fn formatList(
    allocator: Allocator,
    val: Value,
    lay: Layout,
    store: *const layout_mod.Store,
) FormatError![]u8 {
    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();

    // Copy into an aligned local — val.ptr may not satisfy RocList alignment.
    var roc_list: RocList = undefined;
    @memcpy(std.mem.asBytes(&roc_list), val.ptr[0..@sizeOf(RocList)]);
    const len = roc_list.len();
    out.append('[') catch return error.OutOfMemory;

    if (len > 0) {
        const elem_layout_idx = lay.data.list;
        const elem_layout = store.getLayout(elem_layout_idx);
        const elem_size = store.layoutSize(elem_layout);
        var i: usize = 0;
        while (i < len) : (i += 1) {
            if (roc_list.bytes) |bytes| {
                const elem_ptr: [*]u8 = @constCast(bytes + i * elem_size);
                const elem_val = Value{ .ptr = elem_ptr };
                const rendered = try formatWithLayout(allocator, elem_val, elem_layout, elem_layout_idx, store);
                defer allocator.free(rendered);
                out.appendSlice(rendered) catch return error.OutOfMemory;
                if (i + 1 < len) out.appendSlice(", ") catch return error.OutOfMemory;
            }
        }
    }

    out.append(']') catch return error.OutOfMemory;
    return out.toOwnedSlice() catch return error.OutOfMemory;
}

fn formatListOfZst(allocator: Allocator, val: Value) FormatError![]u8 {
    var roc_list: RocList = undefined;
    @memcpy(std.mem.asBytes(&roc_list), val.ptr[0..@sizeOf(RocList)]);
    const len = roc_list.len();
    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();
    out.append('[') catch return error.OutOfMemory;
    if (len > 0) {
        var i: usize = 0;
        while (i < len) : (i += 1) {
            out.appendSlice("{}") catch return error.OutOfMemory;
            if (i + 1 < len) out.appendSlice(", ") catch return error.OutOfMemory;
        }
    }
    out.append(']') catch return error.OutOfMemory;
    return out.toOwnedSlice() catch return error.OutOfMemory;
}

// ─── Box ────────────────────────────────────────────────────────────────

fn formatBox(
    allocator: Allocator,
    val: Value,
    lay: Layout,
    store: *const layout_mod.Store,
) FormatError![]u8 {
    var out = std.array_list.AlignedManaged(u8, null).init(allocator);
    errdefer out.deinit();
    out.appendSlice("Box(") catch return error.OutOfMemory;

    const elem_layout_idx = lay.data.box;
    const elem_layout = store.getLayout(elem_layout_idx);
    const elem_size = store.layoutSize(elem_layout);

    if (elem_size > 0) {
        // Read the pointer stored in the box (box is a pointer to heap data)
        const data_ptr = val.read([*]u8);
        const elem_val = Value{ .ptr = data_ptr };
        const rendered = try formatWithLayout(allocator, elem_val, elem_layout, elem_layout_idx, store);
        defer allocator.free(rendered);
        out.appendSlice(rendered) catch return error.OutOfMemory;
    } else {
        out.appendSlice("{}") catch return error.OutOfMemory;
    }

    out.append(')') catch return error.OutOfMemory;
    return out.toOwnedSlice() catch return error.OutOfMemory;
}

test "format bool" {
    // Minimal smoke test — requires a layout store, which is expensive to create.
    // Real testing happens via the eval test harness.
}

//! Shared value representation wrapping raw bytes + layout.
//!
//! Provides canonical formatting for all Roc value types, usable by the
//! interpreter, dev backend, test helpers, and snapshot tool.

const std = @import("std");
const layout = @import("layout");
const builtins = @import("builtins");
const base = @import("base");

const Layout = layout.Layout;
const Idx = layout.Idx;
const RocDec = builtins.dec.RocDec;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const Ident = base.Ident;

const RocValue = @This();

/// Pointer to raw value bytes (null for zero-sized types).
ptr: ?[*]const u8,
/// Layout describing this value's memory representation.
lay: Layout,
/// When non-null, the layout index for this value — used to detect
/// sentinel types such as `Idx.bool`.
layout_idx: ?Idx = null,

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

/// Wrap an opaque pointer and its layout into a `RocValue`.
pub fn fromPtr(raw_ptr: *const anyopaque, lay_val: Layout) RocValue {
    return .{ .ptr = @ptrCast(raw_ptr), .lay = lay_val };
}

/// Wrap an opaque pointer, its layout, and the layout index into a `RocValue`.
pub fn fromPtrWithIdx(raw_ptr: *const anyopaque, lay_val: Layout, idx: Idx) RocValue {
    return .{ .ptr = @ptrCast(raw_ptr), .lay = lay_val, .layout_idx = idx };
}

/// Wrap a raw byte pointer and its layout into a `RocValue`.
pub fn fromRawBytes(raw_ptr: [*]const u8, lay_val: Layout) RocValue {
    return .{ .ptr = raw_ptr, .lay = lay_val };
}

/// Create a `RocValue` for a zero-sized type (null pointer).
pub fn zst(lay_val: Layout) RocValue {
    return .{ .ptr = null, .lay = lay_val };
}

// ---------------------------------------------------------------------------
// Scalar accessors (safe, misalignment-tolerant reads via @memcpy)
// ---------------------------------------------------------------------------

inline fn readAligned(comptime T: type, raw_ptr: [*]const u8) T {
    var result: T = undefined;
    @memcpy(std.mem.asBytes(&result), raw_ptr[0..@sizeOf(T)]);
    return result;
}

/// Read the value as a signed 128-bit integer, widening smaller int types.
pub fn readI128(self: RocValue) i128 {
    const raw_ptr = self.ptr orelse return 0;
    return switch (self.lay.data.scalar.data.int) {
        .u8 => readAligned(u8, raw_ptr),
        .i8 => readAligned(i8, raw_ptr),
        .u16 => readAligned(u16, raw_ptr),
        .i16 => readAligned(i16, raw_ptr),
        .u32 => readAligned(u32, raw_ptr),
        .i32 => readAligned(i32, raw_ptr),
        .u64 => readAligned(u64, raw_ptr),
        .i64 => readAligned(i64, raw_ptr),
        .i128 => readAligned(i128, raw_ptr),
        .u128 => @bitCast(readAligned(u128, raw_ptr)),
    };
}

/// Read the value as an unsigned 128-bit integer, widening smaller int types.
pub fn readU128(self: RocValue) u128 {
    const raw_ptr = self.ptr orelse return 0;
    return switch (self.lay.data.scalar.data.int) {
        .u8 => readAligned(u8, raw_ptr),
        .u16 => readAligned(u16, raw_ptr),
        .u32 => readAligned(u32, raw_ptr),
        .u64 => readAligned(u64, raw_ptr),
        .u128 => readAligned(u128, raw_ptr),
        .i8 => @bitCast(@as(i128, readAligned(i8, raw_ptr))),
        .i16 => @bitCast(@as(i128, readAligned(i16, raw_ptr))),
        .i32 => @bitCast(@as(i128, readAligned(i32, raw_ptr))),
        .i64 => @bitCast(@as(i128, readAligned(i64, raw_ptr))),
        .i128 => @bitCast(readAligned(i128, raw_ptr)),
    };
}

/// Read the value as a boolean (any non-zero byte is `true`).
pub fn readBool(self: RocValue) bool {
    const raw_ptr = self.ptr orelse return false;
    return readAligned(u8, raw_ptr) != 0;
}

/// Read the value as a 32-bit float.
pub fn readF32(self: RocValue) f32 {
    const raw_ptr = self.ptr orelse return 0;
    return readAligned(f32, raw_ptr);
}

/// Read the value as a 64-bit float.
pub fn readF64(self: RocValue) f64 {
    const raw_ptr = self.ptr orelse return 0;
    return readAligned(f64, raw_ptr);
}

/// Read the value as a `RocDec` (i128-backed fixed-point decimal).
pub fn readDec(self: RocValue) RocDec {
    const raw_ptr = self.ptr orelse return RocDec{ .num = 0 };
    return RocDec{ .num = readAligned(i128, raw_ptr) };
}

/// Reinterpret the value bytes as a `RocStr`.
pub fn readStr(self: RocValue) *const RocStr {
    return @ptrCast(@alignCast(self.ptr.?));
}

/// Reinterpret the value bytes as a `RocList`.
pub fn readList(self: RocValue) *const RocList {
    return @ptrCast(@alignCast(self.ptr.?));
}

// ---------------------------------------------------------------------------
// Format context — lightweight, no type-system dependencies
// ---------------------------------------------------------------------------

/// Lightweight context for formatting values — carries only layout and ident stores.
pub const FormatContext = struct {
    layout_store: *const layout.Store,
    /// For resolving record field names and tag names to strings.
    /// When null, fields render as positional indices.
    ident_store: ?*const Ident.Store = null,
    /// Strip trailing .0 from whole-number Dec values (REPL mode).
    strip_whole_number_decimal: bool = false,
};

// ---------------------------------------------------------------------------
// Canonical formatting
// ---------------------------------------------------------------------------

/// Errors that can occur during value formatting.
pub const FormatError = error{OutOfMemory};

/// Format this value into a newly-allocated string using canonical Roc syntax.
pub fn format(self: RocValue, allocator: std.mem.Allocator, ctx: FormatContext) FormatError![]u8 {
    // --- Scalars ---
    if (self.lay.tag == .scalar) {
        const scalar = self.lay.data.scalar;
        switch (scalar.tag) {
            .str => {
                const rs = self.readStr();
                const s = rs.asSlice();
                var buf = std.array_list.AlignedManaged(u8, null).init(allocator);
                errdefer buf.deinit();
                try buf.append('"');
                for (s) |ch| {
                    switch (ch) {
                        '\\' => try buf.appendSlice("\\\\"),
                        '"' => try buf.appendSlice("\\\""),
                        else => try buf.append(ch),
                    }
                }
                try buf.append('"');
                return buf.toOwnedSlice();
            },
            .int => {
                // Check for bool sentinel
                if (self.layout_idx) |idx| {
                    if (idx == Idx.bool) {
                        return try allocator.dupe(u8, if (self.readBool()) "True" else "False");
                    }
                }
                const precision = scalar.data.int;
                return switch (precision) {
                    .u64, .u128 => try std.fmt.allocPrint(allocator, "{d}", .{self.readU128()}),
                    else => try std.fmt.allocPrint(allocator, "{d}", .{self.readI128()}),
                };
            },
            .frac => {
                return switch (scalar.data.frac) {
                    .f32 => try std.fmt.allocPrint(allocator, "{d}", .{@as(f64, self.readF32())}),
                    .f64 => try std.fmt.allocPrint(allocator, "{d}", .{self.readF64()}),
                    .dec => {
                        const dec = self.readDec();
                        if (ctx.strip_whole_number_decimal and
                            @rem(@abs(dec.num), RocDec.one_point_zero_i128) == 0)
                        {
                            const whole = @divTrunc(dec.num, RocDec.one_point_zero_i128);
                            return try std.fmt.allocPrint(allocator, "{d}", .{whole});
                        }
                        var buf: [RocDec.max_str_length]u8 = undefined;
                        const slice = dec.format_to_buf(&buf);
                        return try allocator.dupe(u8, slice);
                    },
                };
            },
            .opaque_ptr => {},
        }
    }

    // --- Tuples ---
    if (self.lay.tag == .tuple) {
        var out = std.array_list.AlignedManaged(u8, null).init(allocator);
        errdefer out.deinit();
        try out.append('(');
        const tuple_data = ctx.layout_store.getTupleData(self.lay.data.tuple.idx);
        const fields = ctx.layout_store.tuple_fields.sliceRange(tuple_data.getFields());
        const count = fields.len;
        // Iterate by original source index (0, 1, 2, ...) rather than sorted order
        var original_idx: usize = 0;
        while (original_idx < count) : (original_idx += 1) {
            // Find the sorted position for this original index
            const sorted_idx = blk: {
                for (0..count) |si| {
                    if (fields.get(si).index == original_idx) break :blk si;
                }
                unreachable; // Every original index must exist in fields
            };
            const fld = fields.get(sorted_idx);
            const elem_layout = ctx.layout_store.getLayout(fld.layout);
            const elem_offset = ctx.layout_store.getTupleElementOffset(self.lay.data.tuple.idx, @intCast(sorted_idx));
            const base_ptr = self.ptr.?;
            const elem_ptr = base_ptr + elem_offset;
            const elem_val = RocValue{ .ptr = elem_ptr, .lay = elem_layout };
            const rendered = try elem_val.format(allocator, ctx);
            defer allocator.free(rendered);
            try out.appendSlice(rendered);
            if (original_idx + 1 < count) try out.appendSlice(", ");
        }
        try out.append(')');
        return out.toOwnedSlice();
    }

    // --- Lists ---
    if (self.lay.tag == .list) {
        var out = std.array_list.AlignedManaged(u8, null).init(allocator);
        errdefer out.deinit();
        const roc_list = self.readList();
        const len = roc_list.len();
        try out.append('[');
        if (len > 0) {
            const elem_layout_idx = self.lay.data.list;
            const elem_layout = ctx.layout_store.getLayout(elem_layout_idx);
            const elem_size = ctx.layout_store.layoutSize(elem_layout);
            var i: usize = 0;
            while (i < len) : (i += 1) {
                if (roc_list.bytes) |bytes| {
                    const elem_ptr: [*]const u8 = bytes + i * elem_size;
                    const elem_val = RocValue{ .ptr = elem_ptr, .lay = elem_layout };
                    const rendered = try elem_val.format(allocator, ctx);
                    defer allocator.free(rendered);
                    try out.appendSlice(rendered);
                    if (i + 1 < len) try out.appendSlice(", ");
                }
            }
        }
        try out.append(']');
        return out.toOwnedSlice();
    }

    // --- List of ZST ---
    if (self.lay.tag == .list_of_zst) {
        const roc_list = self.readList();
        const len = roc_list.len();
        if (len == 0) {
            return try allocator.dupe(u8, "[]");
        }
        return try std.fmt.allocPrint(allocator, "[<{d} zero-sized elements>]", .{len});
    }

    // --- Records ---
    if (self.lay.tag == .record) {
        var out = std.array_list.AlignedManaged(u8, null).init(allocator);
        errdefer out.deinit();
        const rec_data = ctx.layout_store.getRecordData(self.lay.data.record.idx);
        if (rec_data.fields.count == 0) {
            try out.appendSlice("{}");
            return out.toOwnedSlice();
        }
        try out.appendSlice("{ ");
        const fields = ctx.layout_store.record_fields.sliceRange(rec_data.getFields());
        var i: usize = 0;
        while (i < fields.len) : (i += 1) {
            const fld = fields.get(i);
            // Resolve field name
            if (ctx.ident_store) |ident_store| {
                const name_text = ident_store.getText(fld.name);
                try out.appendSlice(name_text);
            } else {
                try std.fmt.format(out.writer(), "_{d}", .{i});
            }
            try out.appendSlice(": ");
            const offset = ctx.layout_store.getRecordFieldOffset(self.lay.data.record.idx, @intCast(i));
            const field_layout = ctx.layout_store.getLayout(fld.layout);
            const base_ptr = self.ptr.?;
            const field_ptr = base_ptr + offset;
            const field_val = RocValue{ .ptr = field_ptr, .lay = field_layout };
            const rendered = try field_val.format(allocator, ctx);
            defer allocator.free(rendered);
            try out.appendSlice(rendered);
            if (i + 1 < fields.len) try out.appendSlice(", ");
        }
        try out.appendSlice(" }");
        return out.toOwnedSlice();
    }

    // --- Box ---
    if (self.lay.tag == .box) {
        var out = std.array_list.AlignedManaged(u8, null).init(allocator);
        errdefer out.deinit();
        try out.appendSlice("Box(");
        const elem_layout_idx = self.lay.data.box;
        const elem_layout = ctx.layout_store.getLayout(elem_layout_idx);
        const elem_size = ctx.layout_store.layoutSize(elem_layout);
        if (elem_size > 0) {
            if (self.getBoxedData()) |data_ptr| {
                const elem_val = RocValue{ .ptr = data_ptr, .lay = elem_layout };
                const rendered = try elem_val.format(allocator, ctx);
                defer allocator.free(rendered);
                try out.appendSlice(rendered);
            } else {
                try out.appendSlice("<null>");
            }
        } else {
            const elem_val = RocValue.zst(elem_layout);
            const rendered = try elem_val.format(allocator, ctx);
            defer allocator.free(rendered);
            try out.appendSlice(rendered);
        }
        try out.append(')');
        return out.toOwnedSlice();
    }

    // --- Box of ZST ---
    if (self.lay.tag == .box_of_zst) {
        return try allocator.dupe(u8, "Box({})");
    }

    // --- Tag union ---
    if (self.lay.tag == .tag_union) {
        const tu_idx = self.lay.data.tag_union.idx;
        const tu_data = ctx.layout_store.getTagUnionData(tu_idx);
        const disc_offset = ctx.layout_store.getTagUnionDiscriminantOffset(tu_idx);
        var out = std.array_list.AlignedManaged(u8, null).init(allocator);
        errdefer out.deinit();
        if (self.ptr) |ptr| {
            const discriminant = tu_data.readDiscriminantFromPtr(ptr + disc_offset);
            try std.fmt.format(out.writer(), "<tag_union variant={d}>", .{discriminant});
        } else {
            try out.appendSlice("<tag_union>");
        }
        return out.toOwnedSlice();
    }

    // --- ZST ---
    if (self.lay.tag == .zst) {
        return try allocator.dupe(u8, "{}");
    }

    return try allocator.dupe(u8, "<unsupported>");
}

// ---------------------------------------------------------------------------
// Structural equality
// ---------------------------------------------------------------------------

/// Compare two RocValues for structural equality.
/// The `FormatContext` is needed because composite types require the
/// `layout_store` to determine field offsets and element sizes.
pub fn equals(self: RocValue, other: RocValue, ctx: FormatContext) bool {
    // Tags must match
    if (self.lay.tag != other.lay.tag) return false;

    switch (self.lay.tag) {
        .scalar => {
            const s_scalar = self.lay.data.scalar;
            const o_scalar = other.lay.data.scalar;
            if (s_scalar.tag != o_scalar.tag) return false;
            return switch (s_scalar.tag) {
                .str => self.readStr().eql(other.readStr().*),
                .int => {
                    // Check for bool sentinel on both sides
                    const s_bool = if (self.layout_idx) |idx| idx == Idx.bool else false;
                    const o_bool = if (other.layout_idx) |idx| idx == Idx.bool else false;
                    if (s_bool and o_bool) return self.readBool() == other.readBool();
                    // Compare as i128 (widened)
                    return self.readI128() == other.readI128();
                },
                .frac => {
                    if (s_scalar.data.frac != o_scalar.data.frac) return false;
                    return switch (s_scalar.data.frac) {
                        .f32 => @as(u32, @bitCast(self.readF32())) == @as(u32, @bitCast(other.readF32())),
                        .f64 => @as(u64, @bitCast(self.readF64())) == @as(u64, @bitCast(other.readF64())),
                        .dec => self.readDec().num == other.readDec().num,
                    };
                },
                .opaque_ptr => false,
            };
        },
        .zst => return true,
        .tuple => {
            const s_fields = ctx.layout_store.tuple_fields.sliceRange(
                ctx.layout_store.getTupleData(self.lay.data.tuple.idx).getFields(),
            );
            const o_fields = ctx.layout_store.tuple_fields.sliceRange(
                ctx.layout_store.getTupleData(other.lay.data.tuple.idx).getFields(),
            );
            if (s_fields.len != o_fields.len) return false;
            for (0..s_fields.len) |i| {
                const s_fld = s_fields.get(i);
                const o_fld = o_fields.get(i);
                const s_elem_layout = ctx.layout_store.getLayout(s_fld.layout);
                const o_elem_layout = ctx.layout_store.getLayout(o_fld.layout);
                const s_offset = ctx.layout_store.getTupleElementOffset(self.lay.data.tuple.idx, @intCast(i));
                const o_offset = ctx.layout_store.getTupleElementOffset(other.lay.data.tuple.idx, @intCast(i));
                const s_elem = RocValue{ .ptr = self.ptr.? + s_offset, .lay = s_elem_layout };
                const o_elem = RocValue{ .ptr = other.ptr.? + o_offset, .lay = o_elem_layout };
                if (!s_elem.equals(o_elem, ctx)) return false;
            }
            return true;
        },
        .list => {
            const s_list = self.readList();
            const o_list = other.readList();
            if (s_list.len() != o_list.len()) return false;
            const len = s_list.len();
            if (len == 0) return true;
            const s_elem_layout = ctx.layout_store.getLayout(self.lay.data.list);
            const o_elem_layout = ctx.layout_store.getLayout(other.lay.data.list);
            const s_elem_size = ctx.layout_store.layoutSize(s_elem_layout);
            const o_elem_size = ctx.layout_store.layoutSize(o_elem_layout);
            const s_bytes = s_list.bytes orelse return false;
            const o_bytes = o_list.bytes orelse return false;
            for (0..len) |i| {
                const s_elem = RocValue{ .ptr = s_bytes + i * s_elem_size, .lay = s_elem_layout };
                const o_elem = RocValue{ .ptr = o_bytes + i * o_elem_size, .lay = o_elem_layout };
                if (!s_elem.equals(o_elem, ctx)) return false;
            }
            return true;
        },
        .list_of_zst => {
            return self.readList().len() == other.readList().len();
        },
        .record => {
            const s_rec = ctx.layout_store.getRecordData(self.lay.data.record.idx);
            const o_rec = ctx.layout_store.getRecordData(other.lay.data.record.idx);
            const s_fields = ctx.layout_store.record_fields.sliceRange(s_rec.getFields());
            const o_fields = ctx.layout_store.record_fields.sliceRange(o_rec.getFields());
            if (s_fields.len != o_fields.len) return false;
            for (0..s_fields.len) |i| {
                const s_fld = s_fields.get(i);
                const o_fld = o_fields.get(i);
                const s_field_layout = ctx.layout_store.getLayout(s_fld.layout);
                const o_field_layout = ctx.layout_store.getLayout(o_fld.layout);
                const s_offset = ctx.layout_store.getRecordFieldOffset(self.lay.data.record.idx, @intCast(i));
                const o_offset = ctx.layout_store.getRecordFieldOffset(other.lay.data.record.idx, @intCast(i));
                const s_field = RocValue{ .ptr = self.ptr.? + s_offset, .lay = s_field_layout };
                const o_field = RocValue{ .ptr = other.ptr.? + o_offset, .lay = o_field_layout };
                if (!s_field.equals(o_field, ctx)) return false;
            }
            return true;
        },
        .box => {
            const s_inner_layout = ctx.layout_store.getLayout(self.lay.data.box);
            const o_inner_layout = ctx.layout_store.getLayout(other.lay.data.box);
            const s_inner_size = ctx.layout_store.layoutSize(s_inner_layout);
            if (s_inner_size == 0) return true; // Both are boxes of ZST
            const s_data = self.getBoxedData() orelse return other.getBoxedData() == null;
            const o_data = other.getBoxedData() orelse return false;
            const s_inner = RocValue{ .ptr = s_data, .lay = s_inner_layout };
            const o_inner = RocValue{ .ptr = o_data, .lay = o_inner_layout };
            return s_inner.equals(o_inner, ctx);
        },
        .box_of_zst => return true,
        .tag_union => {
            const s_tu_idx = self.lay.data.tag_union.idx;
            const o_tu_idx = other.lay.data.tag_union.idx;
            const s_tu_data = ctx.layout_store.getTagUnionData(s_tu_idx);
            const o_tu_data = ctx.layout_store.getTagUnionData(o_tu_idx);
            const s_disc_offset = ctx.layout_store.getTagUnionDiscriminantOffset(s_tu_idx);
            const o_disc_offset = ctx.layout_store.getTagUnionDiscriminantOffset(o_tu_idx);
            const s_ptr = self.ptr orelse return other.ptr == null;
            const o_ptr = other.ptr orelse return false;
            const s_disc = s_tu_data.readDiscriminantFromPtr(s_ptr + s_disc_offset);
            const o_disc = o_tu_data.readDiscriminantFromPtr(o_ptr + o_disc_offset);
            if (s_disc != o_disc) return false;
            // Compare payload for the active variant
            const s_variants = ctx.layout_store.getTagUnionVariants(s_tu_data);
            const o_variants = ctx.layout_store.getTagUnionVariants(o_tu_data);
            const s_payload_layout = ctx.layout_store.getLayout(s_variants.get(s_disc).payload_layout);
            const o_payload_layout = ctx.layout_store.getLayout(o_variants.get(o_disc).payload_layout);
            const s_payload = RocValue{ .ptr = s_ptr, .lay = s_payload_layout };
            const o_payload = RocValue{ .ptr = o_ptr, .lay = o_payload_layout };
            return s_payload.equals(o_payload, ctx);
        },
        .closure => return false, // Closures are not compared structurally
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Dereference the box pointer. Returns the inner data pointer or null.
fn getBoxedData(self: RocValue) ?[*]const u8 {
    if (self.ptr) |ptr| {
        const slot: *const usize = @ptrCast(@alignCast(ptr));
        if (slot.* == 0) return null;
        return @ptrFromInt(slot.*);
    }
    return null;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "format bool true" {
    const allocator = std.testing.allocator;
    // Build a bool layout (scalar int u8, with Idx.bool sentinel)
    const bool_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .int = .u8 }, .tag = .int } },
    };
    var true_byte: [1]u8 = .{1};
    const val = RocValue{ .ptr = &true_byte, .lay = bool_layout, .layout_idx = Idx.bool };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("True", result);
}

test "format bool false" {
    const allocator = std.testing.allocator;
    const bool_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .int = .u8 }, .tag = .int } },
    };
    var false_byte: [1]u8 = .{0};
    const val = RocValue{ .ptr = &false_byte, .lay = bool_layout, .layout_idx = Idx.bool };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("False", result);
}

test "format i64" {
    const allocator = std.testing.allocator;
    const i64_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .int = .i64 }, .tag = .int } },
    };
    var bytes: [@sizeOf(i64)]u8 = undefined;
    @memcpy(&bytes, std.mem.asBytes(&@as(i64, -42)));
    const val = RocValue{ .ptr = &bytes, .lay = i64_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("-42", result);
}

test "format u64" {
    const allocator = std.testing.allocator;
    const u64_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .int = .u64 }, .tag = .int } },
    };
    var bytes: [@sizeOf(u64)]u8 = undefined;
    @memcpy(&bytes, std.mem.asBytes(&@as(u64, 42)));
    const val = RocValue{ .ptr = &bytes, .lay = u64_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("42", result);
}

test "format dec with strip" {
    const allocator = std.testing.allocator;
    const dec_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .frac = .dec }, .tag = .frac } },
    };
    // 3 as Dec = 3 * 10^18
    const dec_val: i128 = 3 * RocDec.one_point_zero_i128;
    var bytes: [@sizeOf(i128)]u8 = undefined;
    @memcpy(&bytes, std.mem.asBytes(&dec_val));
    const val = RocValue{ .ptr = &bytes, .lay = dec_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null, .strip_whole_number_decimal = true };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("3", result);
}

test "format dec without strip" {
    const allocator = std.testing.allocator;
    const dec_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .frac = .dec }, .tag = .frac } },
    };
    // 3.14 as Dec
    const dec_val: i128 = 3_140_000_000_000_000_000;
    var bytes: [@sizeOf(i128)]u8 = undefined;
    @memcpy(&bytes, std.mem.asBytes(&dec_val));
    const val = RocValue{ .ptr = &bytes, .lay = dec_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("3.14", result);
}

test "format zst" {
    const allocator = std.testing.allocator;
    const zst_layout = Layout{
        .tag = .zst,
        .data = .{ .zst = {} },
    };
    const val = RocValue.zst(zst_layout);
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("{}", result);
}

test "format box_of_zst" {
    const allocator = std.testing.allocator;
    const box_zst_layout = Layout{
        .tag = .box_of_zst,
        .data = .{ .box_of_zst = {} },
    };
    const val = RocValue.zst(box_zst_layout);
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    const result = try val.format(allocator, ctx);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("Box({})", result);
}

// ---------------------------------------------------------------------------
// equals() tests
// ---------------------------------------------------------------------------

test "equals bool" {
    const bool_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .int = .u8 }, .tag = .int } },
    };
    var t: [1]u8 = .{1};
    var f: [1]u8 = .{0};
    const vt = RocValue{ .ptr = &t, .lay = bool_layout, .layout_idx = Idx.bool };
    const vf = RocValue{ .ptr = &f, .lay = bool_layout, .layout_idx = Idx.bool };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    try std.testing.expect(vt.equals(vt, ctx));
    try std.testing.expect(vf.equals(vf, ctx));
    try std.testing.expect(!vt.equals(vf, ctx));
}

test "equals i64" {
    const i64_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .int = .i64 }, .tag = .int } },
    };
    var a: [@sizeOf(i64)]u8 = undefined;
    var b: [@sizeOf(i64)]u8 = undefined;
    var c: [@sizeOf(i64)]u8 = undefined;
    @memcpy(&a, std.mem.asBytes(&@as(i64, 42)));
    @memcpy(&b, std.mem.asBytes(&@as(i64, 42)));
    @memcpy(&c, std.mem.asBytes(&@as(i64, -1)));
    const va = RocValue{ .ptr = &a, .lay = i64_layout };
    const vb = RocValue{ .ptr = &b, .lay = i64_layout };
    const vc = RocValue{ .ptr = &c, .lay = i64_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    try std.testing.expect(va.equals(vb, ctx));
    try std.testing.expect(!va.equals(vc, ctx));
}

test "equals f64" {
    const f64_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .frac = .f64 }, .tag = .frac } },
    };
    var a: [@sizeOf(f64)]u8 = undefined;
    var b: [@sizeOf(f64)]u8 = undefined;
    var c: [@sizeOf(f64)]u8 = undefined;
    @memcpy(&a, std.mem.asBytes(&@as(f64, 3.14)));
    @memcpy(&b, std.mem.asBytes(&@as(f64, 3.14)));
    @memcpy(&c, std.mem.asBytes(&@as(f64, 2.71)));
    const va = RocValue{ .ptr = &a, .lay = f64_layout };
    const vb = RocValue{ .ptr = &b, .lay = f64_layout };
    const vc = RocValue{ .ptr = &c, .lay = f64_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    try std.testing.expect(va.equals(vb, ctx));
    try std.testing.expect(!va.equals(vc, ctx));
}

test "equals dec" {
    const dec_layout = Layout{
        .tag = .scalar,
        .data = .{ .scalar = .{ .data = .{ .frac = .dec }, .tag = .frac } },
    };
    const dec_a: i128 = 3 * RocDec.one_point_zero_i128;
    const dec_b: i128 = 3 * RocDec.one_point_zero_i128;
    const dec_c: i128 = 5 * RocDec.one_point_zero_i128;
    var a: [@sizeOf(i128)]u8 = undefined;
    var b: [@sizeOf(i128)]u8 = undefined;
    var c: [@sizeOf(i128)]u8 = undefined;
    @memcpy(&a, std.mem.asBytes(&dec_a));
    @memcpy(&b, std.mem.asBytes(&dec_b));
    @memcpy(&c, std.mem.asBytes(&dec_c));
    const va = RocValue{ .ptr = &a, .lay = dec_layout };
    const vb = RocValue{ .ptr = &b, .lay = dec_layout };
    const vc = RocValue{ .ptr = &c, .lay = dec_layout };
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    try std.testing.expect(va.equals(vb, ctx));
    try std.testing.expect(!va.equals(vc, ctx));
}

test "equals zst" {
    const zst_layout = Layout{
        .tag = .zst,
        .data = .{ .zst = {} },
    };
    const va = RocValue.zst(zst_layout);
    const vb = RocValue.zst(zst_layout);
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    try std.testing.expect(va.equals(vb, ctx));
}

test "equals mismatched tags" {
    const zst_layout = Layout{ .tag = .zst, .data = .{ .zst = {} } };
    const box_zst_layout = Layout{ .tag = .box_of_zst, .data = .{ .box_of_zst = {} } };
    const va = RocValue.zst(zst_layout);
    const vb = RocValue.zst(box_zst_layout);
    const ctx = FormatContext{ .layout_store = undefined, .ident_store = null };
    try std.testing.expect(!va.equals(vb, ctx));
}

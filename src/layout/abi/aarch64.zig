//! AArch64 (AAPCS64) C-ABI parameter/return classification for Roc layouts.
//!
//! Adapted from the Zig compiler (MIT License, "Copyright (c) Zig contributors"):
//! `src/codegen/aarch64/abi.zig` @ 24fdd5b7a4 (Release 0.16.0). The classification
//! algorithm — homogeneous-float-aggregate (HFA) detection, then size-based
//! integer/double-integer/memory selection — is unchanged; it has been rewritten to
//! read Roc's layout store instead of Zig's `Type`/`Zcu`.
//!
//! AAPCS64 does not vary parameter classification by argument-vs-return position, and
//! Windows-on-ARM64 follows AAPCS64 for fixed (non-variadic) prototypes, so this single
//! classifier covers arm64 macOS, Linux, and Windows.

const std = @import("std");

const layout = @import("../layout.zig");
const store_mod = @import("../store.zig");

const Store = store_mod.Store;
const Idx = layout.Idx;
const Layout = layout.Layout;

/// How a value is passed/returned per AAPCS64.
pub const Class = union(enum) {
    /// Passed/returned in memory: the caller allocates and passes a pointer (args) or the
    /// callee writes through an indirect-result pointer in x8 (returns).
    memory,
    /// A true scalar (int, float, pointer) passed in its natural register.
    byval,
    /// An aggregate that fits in a single general-purpose register.
    integer,
    /// An aggregate (>8 and <=16 bytes) passed in two general-purpose registers.
    double_integer,
    /// A homogeneous float aggregate passed in 1..4 SIMD registers.
    float_array: FloatArray,
};

/// A homogeneous aggregate of `count` floats, each `elem_bits` wide (32 or 64).
pub const FloatArray = struct {
    count: u8,
    elem_bits: u16,
};

/// AAPCS64 passes a homogeneous float aggregate of at most four members in SIMD registers.
const max_hfa_floats = 4;

/// Classify how the value of layout `idx` is passed or returned under AAPCS64.
/// The layout must have runtime bits; zero-sized values are not passed at all and
/// must be filtered out by the caller before classification.
pub fn classifyType(store: *const Store, idx: Idx) Class {
    const lay = store.getLayout(idx);
    std.debug.assert(store.layoutSize(lay) > 0);

    switch (lay.tag) {
        .scalar => {
            const scalar = lay.getScalar();
            switch (scalar.tag) {
                // Integers (including bool, which is a u8 scalar) and pointers pass directly.
                .int, .opaque_ptr => return .byval,
                // f32/f64 pass directly in a SIMD register; Dec is an i128-backed value,
                // so it is an integer-class scalar, not a float.
                .frac => return .byval,
                // RocStr is a three-word aggregate; classify it by size like any aggregate.
                .str => return classifyBySize(store, lay),
            }
        },
        // A Box is a single pointer.
        .box, .box_of_zst, .ptr => return .byval,
        // RocList is a three-word aggregate.
        .list, .list_of_zst => return classifyBySize(store, lay),
        .struct_ => {
            var maybe_float_bits: ?u16 = null;
            const float_count = countFloats(store, idx, &maybe_float_bits);
            if (float_count >= 1 and float_count <= max_hfa_floats) {
                return .{ .float_array = .{
                    .count = float_count,
                    .elem_bits = maybe_float_bits.?,
                } };
            }
            return classifyBySize(store, lay);
        },
        // Tag unions carry a discriminant, closures and erased callables carry pointers, so
        // none of these are homogeneous float aggregates — classify them purely by size.
        .tag_union, .closure, .erased_callable => return classifyBySize(store, lay),
        .zst => unreachable, // filtered out by the assert above; ZSTs are never passed
    }
}

/// Size-based fallback for aggregates that are not homogeneous float aggregates.
fn classifyBySize(store: *const Store, lay: Layout) Class {
    const bit_size = @as(u64, store.layoutSize(lay)) * 8;
    if (bit_size > 128) return .memory;
    if (bit_size > 64) return .double_integer;
    return .integer;
}

const invalid_float_count = std.math.maxInt(u8);

/// Count the float members of an aggregate, returning `invalid_float_count` (sentinel)
/// the moment a non-float member, a mismatched float width, or more than the HFA limit is
/// seen. All members of an HFA must be the same IEEE float width; `maybe_float_bits` is
/// threaded through to enforce that and to report the element width to the caller.
fn countFloats(store: *const Store, idx: Idx, maybe_float_bits: *?u16) u8 {
    const lay = store.getLayout(idx);
    switch (lay.tag) {
        .struct_ => {
            const struct_idx = lay.getStruct().idx;
            const field_count = store.getStructData(struct_idx).fields.count;
            var count: u8 = 0;
            var i: u32 = 0;
            while (i < field_count) : (i += 1) {
                // Unnamed padding is opaque, alignment-1 bytes, never a float
                // member, so any padding makes the aggregate non-homogeneous (it
                // falls back to size-based integer classification).
                if (store.getStructFieldIsPadding(struct_idx, i)) return invalid_float_count;
                const field_layout = store.getStructFieldLayout(struct_idx, i);
                const field_count_floats = countFloats(store, field_layout, maybe_float_bits);
                if (field_count_floats == invalid_float_count) return invalid_float_count;
                count += field_count_floats;
                if (count > max_hfa_floats) return invalid_float_count;
            }
            return count;
        },
        .scalar => {
            const scalar = lay.getScalar();
            // Only IEEE floats (f32/f64) count toward an HFA. Dec is i128-backed, so it is
            // an integer member, not a float.
            if (scalar.tag != .frac) return invalid_float_count;
            const bits: u16 = switch (scalar.getFrac()) {
                .f32 => 32,
                .f64 => 64,
                .dec => return invalid_float_count,
            };
            if (maybe_float_bits.*) |existing| {
                if (existing != bits) return invalid_float_count;
                return 1;
            }
            maybe_float_bits.* = bits;
            return 1;
        },
        // Anything else (pointer, list, box, tag union, …) makes the aggregate non-homogeneous.
        else => return invalid_float_count,
    }
}

const testing = std.testing;

/// Build a struct layout from the given field layout indices (in semantic order).
fn testStruct(store: *Store, field_idxs: []const Idx) std.mem.Allocator.Error!Idx {
    var fields: [16]layout.StructField = undefined;
    for (field_idxs, 0..) |field_idx, i| {
        fields[i] = .{ .index = @intCast(i), .layout = field_idx };
    }
    return store.putStructFields(fields[0..field_idxs.len]);
}

test "aarch64 classify: scalars pass by value" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    try testing.expectEqual(Class.byval, classifyType(&store, .i32));
    try testing.expectEqual(Class.byval, classifyType(&store, .u8));
    try testing.expectEqual(Class.byval, classifyType(&store, .i128));
    try testing.expectEqual(Class.byval, classifyType(&store, .f32));
    try testing.expectEqual(Class.byval, classifyType(&store, .f64));
    try testing.expectEqual(Class.byval, classifyType(&store, .dec));
    try testing.expectEqual(Class.byval, classifyType(&store, .opaque_ptr));
}

test "aarch64 classify: three-word aggregates go to memory" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // RocStr and RocList are 24 bytes (> 16), so they pass in memory.
    try testing.expectEqual(Class.memory, classifyType(&store, .str));
    const list_idx = try store.insertLayout(Layout.list(.u8));
    try testing.expectEqual(Class.memory, classifyType(&store, list_idx));
}

test "aarch64 classify: box is a single pointer" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const box_idx = try store.insertLayout(Layout.box(.u8));
    try testing.expectEqual(Class.byval, classifyType(&store, box_idx));
}

test "aarch64 classify: small integer aggregates use registers" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // Plant { x: i32, type: u32 } is 8 bytes -> one general-purpose register.
    const plant = try testStruct(&store, &.{ .i32, .u32 });
    try testing.expectEqual(Class.integer, classifyType(&store, plant));

    // 16 bytes -> two general-purpose registers.
    const two_words = try testStruct(&store, &.{ .i64, .i64 });
    try testing.expectEqual(Class.double_integer, classifyType(&store, two_words));

    // 24 bytes -> memory.
    const three_words = try testStruct(&store, &.{ .i64, .i64, .i64 });
    try testing.expectEqual(Class.memory, classifyType(&store, three_words));
}

test "aarch64 classify: homogeneous float aggregates" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const one_f64 = try testStruct(&store, &.{.f64});
    try testing.expectEqual(Class{ .float_array = .{ .count = 1, .elem_bits = 64 } }, classifyType(&store, one_f64));

    const two_f32 = try testStruct(&store, &.{ .f32, .f32 });
    try testing.expectEqual(Class{ .float_array = .{ .count = 2, .elem_bits = 32 } }, classifyType(&store, two_f32));

    const four_f64 = try testStruct(&store, &.{ .f64, .f64, .f64, .f64 });
    try testing.expectEqual(Class{ .float_array = .{ .count = 4, .elem_bits = 64 } }, classifyType(&store, four_f64));

    // Five floats exceeds the HFA limit (and is 40 bytes) -> memory.
    const five_f64 = try testStruct(&store, &.{ .f64, .f64, .f64, .f64, .f64 });
    try testing.expectEqual(Class.memory, classifyType(&store, five_f64));
}

test "aarch64 classify: mixed and Dec aggregates are not HFAs" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // A float mixed with an int is not homogeneous: 8 bytes -> one register.
    const mixed = try testStruct(&store, &.{ .f32, .i32 });
    try testing.expectEqual(Class.integer, classifyType(&store, mixed));

    // Mismatched float widths are not homogeneous: 16 bytes -> two registers.
    const mismatched = try testStruct(&store, &.{ .f32, .f64 });
    try testing.expectEqual(Class.double_integer, classifyType(&store, mismatched));

    // Dec is i128-backed, so a struct of one Dec is an integer aggregate (16 bytes).
    const one_dec = try testStruct(&store, &.{.dec});
    try testing.expectEqual(Class.double_integer, classifyType(&store, one_dec));
}

test "aarch64 classify: unnamed padding makes an aggregate non-HFA" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // { a : F32, b : F32, _ : F32 } — three f32-sized fields, but the third is unnamed,
    // alignment-1 padding rather than a float member, so this is NOT a homogeneous float
    // aggregate. It falls back to size-based classification (12 bytes -> two registers).
    const padded = try store.putNominalStructFields(&.{
        .{ .index = 0, .layout = .f32 },
        .{ .index = 1, .layout = .f32 },
        .{ .index = 2, .layout = .f32, .is_padding = true },
    });
    try testing.expectEqual(Class.double_integer, classifyType(&store, padded));

    // Contrast: with all three as real float members it IS a 3×f32 HFA.
    const hfa = try testStruct(&store, &.{ .f32, .f32, .f32 });
    try testing.expectEqual(Class{ .float_array = .{ .count = 3, .elem_bits = 32 } }, classifyType(&store, hfa));
}

test "aarch64 classify: Bool (a one-byte enum) uses one register" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // Roc models Bool as a no-payload two-variant tag union, laid out as a single byte, so
    // it classifies as a one-register integer aggregate (ABI-equivalent to a byval u8).
    try testing.expectEqual(Class.integer, classifyType(&store, .bool));
}

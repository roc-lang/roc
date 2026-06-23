//! WebAssembly C-ABI parameter/return classification for Roc layouts.
//!
//! Adapted from the Zig compiler (MIT License, "Copyright (c) Zig contributors"):
//! `src/codegen/wasm/abi.zig` @ 24fdd5b7a4 (Release 0.16.0). The convention follows the
//! WebAssembly tool-conventions Basic C ABI
//! (https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md), rewritten to
//! read Roc's layout store instead of Zig's `Type`/`Zcu`.
//!
//! Scalars (and single-scalar aggregates) are passed directly; everything multi-field is
//! passed indirectly through a pointer.

const std = @import("std");

const layout = @import("../layout.zig");
const store_mod = @import("../store.zig");

const Store = store_mod.Store;
const Idx = layout.Idx;
const Layout = layout.Layout;

/// How a value is passed/returned under the wasm C ABI.
pub const Class = union(enum) {
    /// Passed by value as the scalar at this layout index. A value wider than 64 bits is
    /// passed as two i64s (see `lowerAsDoubleI64`).
    direct: Idx,
    /// Passed indirectly through a pointer to caller-allocated storage.
    indirect,
};

/// Classify how the value of layout `idx` is passed or returned under the wasm C ABI.
pub fn classifyType(store: *const Store, idx: Idx) Class {
    const lay = store.getLayout(idx);
    std.debug.assert(store.layoutSize(lay) > 0);

    switch (lay.tag) {
        .scalar => {
            const scalar = lay.getScalar();
            switch (scalar.tag) {
                .int, .frac, .opaque_ptr => return .{ .direct = idx },
                // RocStr is a multi-field aggregate.
                .str => return .indirect,
            }
        },
        .box, .box_of_zst, .ptr => return .{ .direct = idx }, // single pointer
        .list, .list_of_zst => return .indirect, // multi-field aggregate
        .struct_ => {
            const struct_idx = lay.getStruct().idx;
            const field_count = store.getStructData(struct_idx).fields.count;
            // A single-field struct is passed as its field; any other aggregate is
            // indirect. A lone unnamed-padding field is not a real newtype member,
            // so it is never unwrapped.
            if (field_count != 1 or store.getStructFieldIsPadding(struct_idx, 0)) return .indirect;
            return classifyType(store, store.getStructFieldLayout(struct_idx, 0));
        },
        .tag_union => {
            const info = store.getTagUnionInfo(lay);
            if (info.variants.len == 1) {
                // Single-variant tag unions have an implicit discriminant, so their C ABI
                // is exactly the payload's C ABI.
                return classifyType(store, info.variants.get(0).payload_layout);
            }

            // A no-payload tag union is an enum — a single integer, passed directly. Any
            // multi-variant tag union carrying a payload is a multi-field aggregate and is
            // passed indirectly.
            var v: usize = 0;
            while (v < info.variants.len) : (v += 1) {
                if (store.getLayout(info.variants.get(v).payload_layout).tag != .zst) {
                    return .indirect;
                }
            }
            return .{ .direct = idx };
        },
        .closure => return classifyType(store, lay.getClosure().captures_layout_idx),
        .erased_callable => return .indirect,
        .zst => unreachable,
    }
}

/// Whether a directly-passed value is wider than 64 bits and must be lowered as two i64s.
pub fn lowerAsDoubleI64(store: *const Store, idx: Idx) bool {
    return store.layoutSize(store.getLayout(idx)) > 8;
}

const testing = std.testing;

fn testStruct(store: *Store, field_idxs: []const Idx) std.mem.Allocator.Error!Idx {
    var fields: [16]layout.StructField = undefined;
    for (field_idxs, 0..) |field_idx, i| {
        fields[i] = .{ .index = @intCast(i), .layout = field_idx };
    }
    return store.putStructFields(fields[0..field_idxs.len]);
}

test "wasm classify: scalars are direct" {
    var store = try Store.init(testing.allocator, .u32);
    defer store.deinit();

    try testing.expectEqual(Class{ .direct = .i32 }, classifyType(&store, .i32));
    try testing.expectEqual(Class{ .direct = .f64 }, classifyType(&store, .f64));
    try testing.expectEqual(Class{ .direct = .i128 }, classifyType(&store, .i128));
    try testing.expect(lowerAsDoubleI64(&store, .i128));
    try testing.expect(!lowerAsDoubleI64(&store, .i32));
}

test "wasm classify: aggregates are indirect, single-field structs unwrap" {
    var store = try Store.init(testing.allocator, .u32);
    defer store.deinit();

    // RocStr / RocList are aggregates -> indirect.
    try testing.expectEqual(Class.indirect, classifyType(&store, .str));
    const list_idx = try store.insertLayout(Layout.list(.u8));
    try testing.expectEqual(Class.indirect, classifyType(&store, list_idx));

    // Plant { i32, u32 } has two fields -> indirect.
    const plant = try testStruct(&store, &.{ .i32, .u32 });
    try testing.expectEqual(Class.indirect, classifyType(&store, plant));

    // A single-field struct is passed as its field.
    const wrapped = try testStruct(&store, &.{.i64});
    try testing.expectEqual(Class{ .direct = .i64 }, classifyType(&store, wrapped));

    // A single-variant tag union has an implicit discriminant and follows the
    // payload's ABI.
    const single_tag_u64 = try store.putTagUnion(&.{.u64});
    try testing.expectEqual(Class{ .direct = .u64 }, classifyType(&store, single_tag_u64));

    // A struct whose only field is unnamed padding is not a real newtype, so it is
    // passed indirectly rather than unwrapped to the padding's borrowed type.
    const padding_only = try store.putNominalStructFields(&.{
        .{ .index = 0, .layout = .u64, .is_padding = true },
    });
    try testing.expectEqual(Class.indirect, classifyType(&store, padding_only));
}

test "wasm classify: enums are direct, Bool included" {
    var store = try Store.init(testing.allocator, .u32);
    defer store.deinit();

    // Bool is a no-payload tag union -> a direct integer.
    try testing.expectEqual(Class{ .direct = .bool }, classifyType(&store, .bool));
}

test "wasm classify: single-variant tag union with aggregate payload follows payload ABI" {
    var store = try Store.init(testing.allocator, .u32);
    defer store.deinit();

    const wrapped_str = try store.putTagUnion(&.{.str});
    try testing.expectEqual(Class.indirect, classifyType(&store, wrapped_str));

    const wrapped_struct = try store.putTagUnion(&.{try testStruct(&store, &.{ .i32, .u32 })});
    try testing.expectEqual(Class.indirect, classifyType(&store, wrapped_struct));
}

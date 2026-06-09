//! Per-target C-ABI parameter/return classification for Roc layouts.
//!
//! These classifiers are adapted from the Zig compiler (MIT License, "Copyright (c) Zig
//! contributors"), `src/codegen/<arch>/abi.zig` @ 24fdd5b7a4 (Release 0.16.0), rewritten to
//! read Roc's layout store instead of Zig's `Type`/`Zcu`. They decide, per the published
//! platform C ABI, whether a value is passed/returned in registers or in memory, which the
//! backends and glue then lower into actual calls.

const std = @import("std");

const layout = @import("../layout.zig");
const store_mod = @import("../store.zig");

const Store = store_mod.Store;
const Idx = layout.Idx;

pub const aarch64 = @import("aarch64.zig");
pub const x86_64 = @import("x86_64.zig");
pub const wasm = @import("wasm.zig");

pub const call = @import("call.zig");
pub const LoweredCall = call.LoweredCall;
pub const Placement = call.Placement;
pub const RegPiece = call.RegPiece;
pub const RegClass = call.RegClass;
pub const Target = call.Target;
pub const lower = call.lower;

/// Whether a hosted function with these argument and return layouts must be passed a leading
/// `*RocOps` — i.e. whether it could allocate or free Roc-managed memory. True exactly when
/// its return type or any argument type transitively contains a heap-allocated (refcounted)
/// Roc value (List, Str, Box, recursive union), which is when the host needs the allocator
/// vtable. A function over only flat scalars/structs is a bare C call with no `*RocOps`.
///
/// Both the backends (deciding whether to thread `roc_ops` at a hosted call site) and glue
/// (deciding whether to emit the `*RocOps` parameter) call this, so the two sides agree by
/// construction.
pub fn needsRocOps(store: *const Store, arg_idxs: []const Idx, ret_idx: Idx) bool {
    if (store.layoutContainsRefcounted(store.getLayout(ret_idx))) return true;
    for (arg_idxs) |arg_idx| {
        if (store.layoutContainsRefcounted(store.getLayout(arg_idx))) return true;
    }
    return false;
}

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(aarch64);
    std.testing.refAllDecls(x86_64);
    std.testing.refAllDecls(wasm);
    std.testing.refAllDecls(call);
}

test "needsRocOps: flat scalars and structs do not need ops" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    // random_plant : I32 => Plant {i32,u32}: flat in, flat out -> no ops.
    var plant_fields = [_]layout.StructField{
        .{ .index = 0, .layout = .i32 },
        .{ .index = 1, .layout = .u32 },
    };
    const plant = try store.putStructFields(&plant_fields);
    try std.testing.expect(!needsRocOps(&store, &.{.i32}, plant));
    try std.testing.expect(!needsRocOps(&store, &.{ .i64, .f64 }, .i32));
}

test "needsRocOps: heap return or heap arg needs ops" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    const list_u8 = try store.insertLayout(layout.Layout.list(.u8));
    // Returning a List needs ops (host allocates it).
    try std.testing.expect(needsRocOps(&store, &.{.i32}, list_u8));
    // Taking a Str arg needs ops (host may free it).
    try std.testing.expect(needsRocOps(&store, &.{.str}, .i32));
}

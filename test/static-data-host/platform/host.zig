//! Host integration test for provided readonly data exports.
//!
//! This host links directly against `roc__answer`, `roc__table`, `roc__names`,
//! and `roc__tree`. It verifies that provided constants are normal Roc runtime
//! values in readonly data, including nested heap-shaped values whose refcount
//! header is `REFCOUNT_STATIC_DATA`.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");

const RocAlloc = builtins.host_abi.RocAlloc;
const RocCrashed = builtins.host_abi.RocCrashed;
const RocDbg = builtins.host_abi.RocDbg;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocStr = builtins.str.RocStr;

const CheckError = error{StaticDataHostCheckFailed};

const HostEnv = struct {
    gpa: std.heap.GeneralPurposeAllocator(.{}),
    dealloc_count: usize,
};

const Counts = extern struct {
    @"0": i64,
    @"1": i64,
};

const Status = extern struct {
    payload: RocStr,
    discriminant: u8,
    padding: [7]u8,
};

const User = extern struct {
    name: RocStr,
    tags: RocList,
};

const Table = extern struct {
    counts: Counts,
    status: Status,
    user: User,
};

const Branch = extern struct {
    payload: [16]u8,
    discriminant: u8,
    padding: [7]u8,
};

const Tree = extern struct {
    payload: [16]u8,
    discriminant: u8,
    padding: [7]u8,
};

const BranchPairPayload = extern struct {
    first: *const i64,
    second: *const i64,
};

const TreeNodePayload = extern struct {
    left: *const Branch,
    right: *const Branch,
};

extern const roc__answer: i64;
extern const roc__table: Table;
extern const roc__names: RocList;
extern const roc__tree: Tree;
extern fn roc__main(ops: *RocOps, ret_ptr: ?*anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

comptime {
    @export(&main, .{ .name = "main" });

    if (builtin.os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;

    var host_env = HostEnv{
        .gpa = std.heap.GeneralPurposeAllocator(.{}){},
        .dealloc_count = 0,
    };
    defer _ = host_env.gpa.deinit();

    var roc_ops = RocOps{
        .env = @ptrCast(&host_env),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
    };

    runStaticDataChecks(&roc_ops, &host_env) catch |err| {
        std.debug.print("static data host check failed: {s}\n", .{@errorName(err)});
        return 1;
    };

    var dummy_ret: u8 = 0;
    var dummy_arg: u8 = 0;
    roc__main(&roc_ops, @ptrCast(&dummy_ret), @ptrCast(&dummy_arg));

    expectEqualUsize(host_env.dealloc_count, 0, "no runtime deallocs for static data") catch return 1;

    std.debug.print("static data host constants ok\n", .{});
    return 0;
}

fn runStaticDataChecks(roc_ops: *RocOps, host_env: *HostEnv) !void {
    try expectEqualI64(roc__answer, 42, "answer");

    try expectEqualI64(roc__table.counts.@"0", 3, "table.counts.0");
    try expectEqualI64(roc__table.counts.@"1", 5, "table.counts.1");
    try expectEqualU8(roc__table.status.discriminant, 1, "table.status discriminant for Ok");
    try expectStr(roc__table.status.payload, "ready readonly exported status", roc_ops, "table.status payload");
    try expectStr(roc__table.user.name, "Alice readonly exported name", roc_ops, "table.user.name");
    try expectListOfStr(
        roc__table.user.tags,
        &.{
            "admin readonly exported tag",
            "ops readonly exported tag",
        },
        roc_ops,
        "table.user.tags",
    );

    try expectListOfListOfStr(
        roc__names,
        &.{
            &.{ "Alice readonly nested list", "Bob readonly nested list" },
            &.{},
            &.{"Eve readonly nested list"},
        },
        roc_ops,
        "names",
    );

    try expectTree(roc__tree, roc_ops);

    try expectEqualUsize(host_env.dealloc_count, 0, "static checks did not dealloc");
}

fn expectTree(tree: Tree, roc_ops: *RocOps) !void {
    try expectEqualU8(tree.discriminant, 1, "tree is Node");

    const node: *const TreeNodePayload = payloadAs(TreeNodePayload, &tree.payload);
    try expectStaticAllocationPtr(node.left, @alignOf(Branch), true, roc_ops, "tree.left Branch box");
    try expectStaticAllocationPtr(node.right, @alignOf(Branch), true, roc_ops, "tree.right Branch box");

    try expectBranchLeaf(node.left.*, 5, roc_ops, "tree.left");
    try expectBranchPair(node.right.*, 7, 11, roc_ops, "tree.right");
}

fn expectBranchLeaf(branch: Branch, expected: i64, roc_ops: *RocOps, label: []const u8) !void {
    _ = roc_ops;
    try expectEqualU8(branch.discriminant, 0, label);
    const value: *const i64 = payloadAs(i64, &branch.payload);
    try expectEqualI64(value.*, expected, label);
}

fn expectBranchPair(branch: Branch, first_expected: i64, second_expected: i64, roc_ops: *RocOps, label: []const u8) !void {
    try expectEqualU8(branch.discriminant, 1, label);

    const pair: *const BranchPairPayload = payloadAs(BranchPairPayload, &branch.payload);
    try expectStaticAllocationPtr(pair.first, @alignOf(i64), false, roc_ops, "branch pair first Box(I64)");
    try expectStaticAllocationPtr(pair.second, @alignOf(i64), false, roc_ops, "branch pair second Box(I64)");
    try expectEqualI64(pair.first.*, first_expected, label);
    try expectEqualI64(pair.second.*, second_expected, label);
}

fn expectListOfListOfStr(list: RocList, expected: []const []const []const u8, roc_ops: *RocOps, label: []const u8) !void {
    try expectStaticList(list, @alignOf(RocList), @sizeOf(RocList), true, expected.len, roc_ops, label);

    if (expected.len == 0) return;
    const rows = list.elements(RocList) orelse return fail("expected non-empty outer list bytes");
    for (expected, 0..) |row_expected, i| {
        var row_label_buf: [128]u8 = undefined;
        const row_label = std.fmt.bufPrint(&row_label_buf, "{s}[{d}]", .{ label, i }) catch label;
        try expectListOfStr(rows[i], row_expected, roc_ops, row_label);
    }
}

fn expectListOfStr(list: RocList, expected: []const []const u8, roc_ops: *RocOps, label: []const u8) !void {
    try expectStaticList(list, @alignOf(RocStr), @sizeOf(RocStr), true, expected.len, roc_ops, label);

    if (expected.len == 0) return;
    const values = list.elements(RocStr) orelse return fail("expected non-empty string list bytes");
    for (expected, 0..) |expected_str, i| {
        var item_label_buf: [128]u8 = undefined;
        const item_label = std.fmt.bufPrint(&item_label_buf, "{s}[{d}]", .{ label, i }) catch label;
        try expectStr(values[i], expected_str, roc_ops, item_label);
    }
}

fn expectStr(str: RocStr, expected: []const u8, roc_ops: *RocOps, label: []const u8) !void {
    var local = str;
    if (!std.mem.eql(u8, local.asSlice(), expected)) {
        std.debug.print("expected {s} to equal \"{s}\", got \"{s}\"\n", .{ label, expected, local.asSlice() });
        return CheckError.StaticDataHostCheckFailed;
    }

    if (!local.isSmallStr()) {
        const ptr = local.getAllocationPtr();
        try expectStaticDataPtr(ptr, label);
        const before = try readRefcount(ptr);
        local.incref(1, roc_ops);
        try expectEqualIsize(try readRefcount(ptr), before, label);
        local.decref(roc_ops);
        try expectEqualIsize(try readRefcount(ptr), before, label);
    }
}

fn expectStaticList(
    list: RocList,
    element_alignment: u32,
    element_width: usize,
    elements_refcounted: bool,
    expected_len: usize,
    roc_ops: *RocOps,
    label: []const u8,
) !void {
    try expectEqualUsize(list.len(), expected_len, label);
    if (expected_len == 0) return;

    const data_ptr = list.getAllocationDataPtr(roc_ops);
    try expectStaticDataPtr(data_ptr, label);
    if (elements_refcounted) {
        try expectEqualUsize(try readAllocationElementCount(data_ptr), expected_len, label);
    }

    const before = try readRefcount(data_ptr);
    list.incref(1, elements_refcounted, roc_ops);
    try expectEqualIsize(try readRefcount(data_ptr), before, label);
    list.decref(element_alignment, element_width, elements_refcounted, null, builtins.utils.rcNone, roc_ops);
    try expectEqualIsize(try readRefcount(data_ptr), before, label);
}

fn expectStaticAllocationPtr(
    ptr: anytype,
    alignment: u32,
    contains_refcounted: bool,
    roc_ops: *RocOps,
    label: []const u8,
) !void {
    const data_ptr = ptrToDataPtr(ptr);
    try expectStaticDataPtr(data_ptr, label);
    const before = try readRefcount(data_ptr);
    builtins.utils.increfDataPtrC(data_ptr, 1, roc_ops);
    try expectEqualIsize(try readRefcount(data_ptr), before, label);
    builtins.utils.decrefDataPtrC(data_ptr, alignment, contains_refcounted, roc_ops);
    try expectEqualIsize(try readRefcount(data_ptr), before, label);
}

fn expectStaticDataPtr(data_ptr: ?[*]u8, label: []const u8) !void {
    const refcount = try readRefcount(data_ptr);
    try expectEqualIsize(refcount, builtins.utils.REFCOUNT_STATIC_DATA, label);
}

fn readRefcount(data_ptr: ?[*]u8) !isize {
    const ptr = data_ptr orelse return fail("expected static data pointer");
    const unmasked = unmaskedDataAddress(ptr);
    const refcount_ptr: *const isize = @ptrFromInt(unmasked - @sizeOf(usize));
    return refcount_ptr.*;
}

fn readAllocationElementCount(data_ptr: ?[*]u8) !usize {
    const ptr = data_ptr orelse return fail("expected allocation element count pointer");
    const unmasked = unmaskedDataAddress(ptr);
    const count_ptr: *const usize = @ptrFromInt(unmasked - 2 * @sizeOf(usize));
    return count_ptr.*;
}

fn unmaskedDataAddress(ptr: [*]u8) usize {
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    return @intFromPtr(ptr) & ~tag_mask;
}

fn ptrToDataPtr(ptr: anytype) ?[*]u8 {
    return @ptrFromInt(@intFromPtr(ptr));
}

fn payloadAs(comptime T: type, payload: *const [16]u8) *const T {
    return @ptrCast(@alignCast(payload));
}

fn expectEqualI64(actual: i64, expected: i64, label: []const u8) !void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn expectEqualIsize(actual: isize, expected: isize, label: []const u8) !void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn expectEqualU8(actual: u8, expected: u8, label: []const u8) !void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn expectEqualUsize(actual: usize, expected: usize, label: []const u8) !void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn fail(message: []const u8) CheckError {
    std.debug.print("{s}\n", .{message});
    return CheckError.StaticDataHostCheckFailed;
}

fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_alloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const total_size = roc_alloc.length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        std.debug.print("host allocation failed for {d} bytes\n", .{total_size});
        std.process.exit(1);
    };

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    host.dealloc_count += 1;

    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(roc_dealloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - size_storage_bytes);

    allocator.rawFree(base_ptr[0..total_size], align_enum, @returnAddress());
}

fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_realloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);

    const new_total_size = roc_realloc.new_length + size_storage_bytes;
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        std.debug.print("host reallocation failed for {d} bytes\n", .{new_total_size});
        std.process.exit(1);
    };
    @memcpy(new_ptr[0..@min(old_total_size, new_total_size)], old_base_ptr[0..@min(old_total_size, new_total_size)]);
    allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    roc_realloc.answer = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
}

fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    std.debug.print("ROC DBG: {s}\n", .{roc_dbg.utf8_bytes[0..roc_dbg.len]});
}

fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    std.debug.print("ROC EXPECT FAILED: {s}\n", .{roc_expect.utf8_bytes[0..roc_expect.len]});
}

fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    _ = env;
    std.debug.print("ROC CRASHED: {s}\n", .{roc_crashed.utf8_bytes[0..roc_crashed.len]});
    std.process.exit(1);
}

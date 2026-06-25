//! Host integration test for provided readonly data exports.
//!
//! This host links directly against `roc_answer`, `roc_table`, `roc_names`,
//! and `roc_tree`. It verifies that provided constants are normal Roc runtime
//! values in readonly data, including nested heap-shaped values whose refcount
//! header is `REFCOUNT_STATIC_DATA`.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const shim_io = @import("shim_io");

// Route std.debug's IO through shim_io instead of the default std.Io.Threaded.
// Zig 0.16's Threaded backend pulls in timestampToPosix, whose i128 division
// references compiler-rt (__divti3/__modti3) that this libc/compiler_rt-free
// host archive does not link. Setting debug_threaded_io = null avoids that pull-in.
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    .allow_stack_tracing = false,
};

const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;

const CheckError = error{StaticDataHostCheckFailed};

const HostEnv = struct {
    // thread_safe = false: this single-threaded test host must stay compiler_rt-free,
    // but DebugAllocator's thread-safe mutex pulls in std.Io.Threaded (timestampToPosix
    // -> i128 division -> __divti3/__modti3, which this archive does not link).
    gpa: std.heap.DebugAllocator(.{ .thread_safe = false }),
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
    payload: [16]u8 align(8),
    discriminant: u8,
    padding: [7]u8,
};

const Tree = extern struct {
    payload: [16]u8 align(8),
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

extern const roc_answer: i64;
extern const roc_flag: u8;
extern const roc_flags: RocList;
extern const roc_table: Table;
extern const roc_names: RocList;
extern const roc_tree: Tree;
extern const roc_boxed_add_one: ?[*]u8;
// The app's entrypoint, exported under its provides symbol with its natural
// C ABI: main_for_host! takes no arguments and returns {}.
extern fn roc_main() callconv(.c) void;

// The host's private RocOps. The exported runtime symbols below and the
// builtins helpers reach the host allocator through this global, set by main
// before any Roc code runs.
var g_roc_ops: ?*RocOps = null;

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocAllocFn(g_roc_ops.?, length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    rocDeallocFn(g_roc_ops.?, ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocReallocFn(g_roc_ops.?, ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocDbgFn(g_roc_ops.?, bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocExpectFailedFn(g_roc_ops.?, bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocCrashedFn(g_roc_ops.?, bytes, len);
}

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });
}

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
        .gpa = std.heap.DebugAllocator(.{ .thread_safe = false }){},
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
    g_roc_ops = &roc_ops;

    runStaticDataChecks(&roc_ops, &host_env) catch |err| {
        std.debug.print("static data host check failed: {s}\n", .{@errorName(err)});
        return 1;
    };

    roc_main();

    expectEqualUsize(host_env.dealloc_count, 0, "no runtime deallocs for static data") catch return 1;

    std.debug.print("static data host constants ok\n", .{});
    return 0;
}

fn runStaticDataChecks(roc_ops: *RocOps, host_env: *HostEnv) error{StaticDataHostCheckFailed}!void {
    try expectEqualI64(roc_answer, 42, "answer");
    try expectEqualU8(roc_flag, 1, "flag True discriminant");
    try expectListOfBool(roc_flags, &.{ 0, 1, 0 }, roc_ops, "flags");

    try expectEqualI64(roc_table.counts.@"0", 3, "table.counts.0");
    try expectEqualI64(roc_table.counts.@"1", 5, "table.counts.1");
    try expectEqualU8(roc_table.status.discriminant, 1, "table.status discriminant for Ok");
    try expectStr(roc_table.status.payload, "ready readonly exported status", roc_ops, "table.status payload");
    try expectStr(roc_table.user.name, "Alice readonly exported name", roc_ops, "table.user.name");
    try expectListOfStr(
        roc_table.user.tags,
        &.{
            "admin readonly exported tag",
            "ops readonly exported tag",
        },
        roc_ops,
        "table.user.tags",
    );

    try expectListOfListOfStr(
        roc_names,
        &.{
            &.{ "Alice readonly nested list", "Bob readonly nested list" },
            &.{},
            &.{"Eve readonly nested list"},
        },
        roc_ops,
        "names",
    );

    try expectTree(roc_tree, roc_ops);
    try expectBoxedAddOne(roc_boxed_add_one, roc_ops);

    try expectEqualUsize(host_env.dealloc_count, 0, "static checks did not dealloc");
}

fn expectTree(tree: Tree, roc_ops: *RocOps) error{StaticDataHostCheckFailed}!void {
    try expectEqualU8(tree.discriminant, 1, "tree is Node");

    const node: *const TreeNodePayload = payloadAs(TreeNodePayload, &tree.payload);
    try expectStaticAllocationPtr(node.left, @alignOf(Branch), true, roc_ops, "tree.left Branch box");
    try expectStaticAllocationPtr(node.right, @alignOf(Branch), true, roc_ops, "tree.right Branch box");

    try expectBranchLeaf(node.left.*, 5, roc_ops, "tree.left");
    try expectBranchPair(node.right.*, 7, 11, roc_ops, "tree.right");
}

fn expectBranchLeaf(branch: Branch, expected: i64, roc_ops: *RocOps, label: []const u8) error{StaticDataHostCheckFailed}!void {
    _ = roc_ops;
    try expectEqualU8(branch.discriminant, 0, label);
    const value: *const i64 = payloadAs(i64, &branch.payload);
    try expectEqualI64(value.*, expected, label);
}

fn expectBranchPair(branch: Branch, first_expected: i64, second_expected: i64, roc_ops: *RocOps, label: []const u8) error{StaticDataHostCheckFailed}!void {
    try expectEqualU8(branch.discriminant, 1, label);

    const pair: *const BranchPairPayload = payloadAs(BranchPairPayload, &branch.payload);
    try expectStaticAllocationPtr(pair.first, @alignOf(i64), false, roc_ops, "branch pair first Box(I64)");
    try expectStaticAllocationPtr(pair.second, @alignOf(i64), false, roc_ops, "branch pair second Box(I64)");
    try expectEqualI64(pair.first.*, first_expected, label);
    try expectEqualI64(pair.second.*, second_expected, label);
}

fn expectListOfListOfStr(list: RocList, expected: []const []const []const u8, roc_ops: *RocOps, label: []const u8) error{StaticDataHostCheckFailed}!void {
    try expectStaticList(list, @alignOf(RocList), @sizeOf(RocList), true, expected.len, roc_ops, label);

    if (expected.len == 0) return;
    const rows = list.elements(RocList) orelse return fail("expected non-empty outer list bytes");
    for (expected, 0..) |row_expected, i| {
        var row_label_buf: [128]u8 = undefined;
        const row_label = std.fmt.bufPrint(&row_label_buf, "{s}[{d}]", .{ label, i }) catch label;
        try expectListOfStr(rows[i], row_expected, roc_ops, row_label);
    }
}

fn expectListOfStr(list: RocList, expected: []const []const u8, roc_ops: *RocOps, label: []const u8) error{StaticDataHostCheckFailed}!void {
    try expectStaticList(list, @alignOf(RocStr), @sizeOf(RocStr), true, expected.len, roc_ops, label);

    if (expected.len == 0) return;
    const values = list.elements(RocStr) orelse return fail("expected non-empty string list bytes");
    for (expected, 0..) |expected_str, i| {
        var item_label_buf: [128]u8 = undefined;
        const item_label = std.fmt.bufPrint(&item_label_buf, "{s}[{d}]", .{ label, i }) catch label;
        try expectStr(values[i], expected_str, roc_ops, item_label);
    }
}

fn expectListOfBool(list: RocList, expected: []const u8, roc_ops: *RocOps, label: []const u8) error{StaticDataHostCheckFailed}!void {
    try expectStaticList(list, @alignOf(u8), @sizeOf(u8), false, expected.len, roc_ops, label);

    if (expected.len == 0) return;
    const values = list.elements(u8) orelse return fail("expected non-empty bool list bytes");
    for (expected, 0..) |expected_discriminant, i| {
        var item_label_buf: [128]u8 = undefined;
        const item_label = std.fmt.bufPrint(&item_label_buf, "{s}[{d}]", .{ label, i }) catch label;
        try expectEqualU8(values[i], expected_discriminant, item_label);
    }
}

fn expectStr(str: RocStr, expected: []const u8, roc_ops: *RocOps, label: []const u8) error{StaticDataHostCheckFailed}!void {
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
) error{StaticDataHostCheckFailed}!void {
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
) error{StaticDataHostCheckFailed}!void {
    const data_ptr = ptrToDataPtr(ptr);
    try expectStaticDataPtr(data_ptr, label);
    const before = try readRefcount(data_ptr);
    builtins.utils.increfDataPtrC(data_ptr, 1, roc_ops);
    try expectEqualIsize(try readRefcount(data_ptr), before, label);
    builtins.utils.decrefDataPtrC(data_ptr, alignment, contains_refcounted, roc_ops);
    try expectEqualIsize(try readRefcount(data_ptr), before, label);
}

const I64ToI64Args = extern struct {
    arg0: i64,
};

fn expectBoxedAddOne(boxed: ?[*]u8, roc_ops: *RocOps) error{StaticDataHostCheckFailed}!void {
    const ptr = boxed orelse return fail("expected boxed_add_one static data pointer");
    try expectStaticDataPtr(ptr, "boxed_add_one");

    const before = try readRefcount(ptr);
    builtins.erased_callable.incref(ptr, 1, roc_ops);
    try expectEqualIsize(try readRefcount(ptr), before, "boxed_add_one incref keeps static refcount");

    var args = I64ToI64Args{ .arg0 = 41 };
    var result: i64 = undefined;
    const payload = builtins.erased_callable.payloadPtr(ptr);
    payload.callable_fn_ptr(
        roc_ops,
        @ptrCast(&result),
        @ptrCast(&args),
        builtins.erased_callable.capturePtr(ptr),
    );
    try expectEqualI64(result, 42, "boxed_add_one call");

    builtins.erased_callable.decref(ptr, roc_ops);
    try expectEqualIsize(try readRefcount(ptr), before, "boxed_add_one decref keeps static refcount");
}

fn expectStaticDataPtr(data_ptr: ?[*]u8, label: []const u8) error{StaticDataHostCheckFailed}!void {
    const refcount = try readRefcount(data_ptr);
    try expectEqualIsize(refcount, builtins.utils.REFCOUNT_STATIC_DATA, label);
}

fn readRefcount(data_ptr: ?[*]u8) error{StaticDataHostCheckFailed}!isize {
    const ptr = data_ptr orelse return fail("expected static data pointer");
    const unmasked = unmaskedDataAddress(ptr);
    const refcount_ptr: *const isize = @ptrFromInt(unmasked - @sizeOf(usize));
    return refcount_ptr.*;
}

fn readAllocationElementCount(data_ptr: ?[*]u8) error{StaticDataHostCheckFailed}!usize {
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

fn expectEqualI64(actual: i64, expected: i64, label: []const u8) error{StaticDataHostCheckFailed}!void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn expectEqualIsize(actual: isize, expected: isize, label: []const u8) error{StaticDataHostCheckFailed}!void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn expectEqualU8(actual: u8, expected: u8, label: []const u8) error{StaticDataHostCheckFailed}!void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn expectEqualUsize(actual: usize, expected: usize, label: []const u8) error{StaticDataHostCheckFailed}!void {
    if (actual != expected) {
        std.debug.print("expected {s} = {d}, got {d}\n", .{ label, expected, actual });
        return CheckError.StaticDataHostCheckFailed;
    }
}

fn fail(message: []const u8) CheckError {
    std.debug.print("{s}\n", .{message});
    return CheckError.StaticDataHostCheckFailed;
}

fn rocAllocFn(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const total_size = length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        std.debug.print("host allocation failed for {d} bytes\n", .{total_size});
        std.process.exit(1);
    };

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    return @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn rocDeallocFn(ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    host.dealloc_count += 1;

    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    allocator.rawFree(base_ptr[0..total_size], align_enum, @returnAddress());
}

fn rocReallocFn(ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    const new_total_size = new_length + size_storage_bytes;
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        std.debug.print("host reallocation failed for {d} bytes\n", .{new_total_size});
        std.process.exit(1);
    };
    @memcpy(new_ptr[0..@min(old_total_size, new_total_size)], old_base_ptr[0..@min(old_total_size, new_total_size)]);
    allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    return @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
}

fn rocDbgFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    std.debug.print("ROC DBG: {s}\n", .{bytes[0..len]});
}

fn rocExpectFailedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    std.debug.print("ROC EXPECT FAILED: {s}\n", .{bytes[0..len]});
}

fn rocCrashedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    std.debug.print("ROC CRASHED: {s}\n", .{bytes[0..len]});
    std.process.exit(1);
}

//! End-to-end host ABI regression for provided roots containing boxed callables.
//!
//! The host obtains a capturing callable from one provided root and passes
//! ownership to another provided root which ignores it. The callee must emit
//! the erased-callable release helper, including its closure-environment drop.

const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const builtins = @import("builtins");
const host_alloc = @import("host_alloc");
const shim_io = @import("shim_io");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    .allow_stack_tracing = false,
};

const RocOps = builtins.host_abi.RocOps;

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{
        .thread_safe = false,
        .stack_trace_frames = build_options.debug_gpa_stack_trace_frames,
    }),
    alloc_count: usize,
    dealloc_count: usize,

    pub fn rocAllocator(self: *HostEnv) std.mem.Allocator {
        return self.gpa.allocator();
    }
};

extern fn roc_make_boxed_callable(offset: u64) callconv(.c) ?[*]u8;
extern fn roc_drop_boxed_callable(callable: ?[*]u8) callconv(.c) void;
extern fn roc_make_aliased_boxed_callables() callconv(.c) ?[*]u8;
extern fn roc_make_shared_boxed_callables() callconv(.c) ?[*]u8;
extern fn roc_drop_aliased_boxed_callables(callables: ?[*]u8) callconv(.c) void;

/// Host view of the app's `{ first : Box(U64 -> U64), second : Box(U64 -> U64) }`.
const AliasedCallables = extern struct {
    first: ?[*]u8,
    second: ?[*]u8,
};

var g_roc_ops: ?*RocOps = null;

fn getOps() *RocOps {
    return g_roc_ops.?;
}

comptime {
    host_alloc.exportRuntimeSymbols(getOps, .{});
    @export(&main, .{ .name = "main" });

    if (builtin.os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    const drop_mode = "--run-provided-boxed-callable-drop";
    const identity_mode = "--run-provided-boxed-callable-identity";
    const mode = if (argc == 2) std.mem.span(argv[1]) else "";
    const run_drop = std.mem.eql(u8, mode, drop_mode);
    const run_identity = std.mem.eql(u8, mode, identity_mode);
    if (!run_drop and !run_identity) {
        std.debug.print("usage: <app> {s}|{s}\n", .{ drop_mode, identity_mode });
        return 1;
    }

    var host_env = HostEnv{
        .gpa = .{},
        .alloc_count = 0,
        .dealloc_count = 0,
    };
    defer _ = build_options.debugGpaOk(host_env.gpa.deinit());

    var roc_ops = RocOps{
        .env = @ptrCast(&host_env),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = callbacks.rocReallocFn,
        .roc_dbg = callbacks.rocDbgFn,
        .roc_expect_failed = callbacks.rocExpectFailedFn,
        .roc_crashed = callbacks.rocCrashedFn,
        .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
    };
    g_roc_ops = &roc_ops;

    if (run_drop) {
        const callable = roc_make_boxed_callable(41) orelse {
            std.debug.print("provided callable maker returned null\n", .{});
            return 1;
        };
        if (host_env.alloc_count == 0) {
            std.debug.print("provided callable maker did not allocate\n", .{});
            return 1;
        }

        roc_drop_boxed_callable(callable);
        if (host_env.dealloc_count != host_env.alloc_count) {
            std.debug.print("provided callable drop released {d} of {d} allocations\n", .{
                host_env.dealloc_count,
                host_env.alloc_count,
            });
            return 1;
        }

        std.debug.print("provided boxed callable drop ok\n", .{});
        return 0;
    }

    // One boxed callable held by two record fields is one heap allocation, so
    // both fields must reach the host as the same erased-callable pointer.
    var failed = false;
    const makers = [_]struct { name: []const u8, make: *const fn () callconv(.c) ?[*]u8 }{
        .{ .name = "aliased", .make = &roc_make_aliased_boxed_callables },
        .{ .name = "shared", .make = &roc_make_shared_boxed_callables },
    };
    for (makers) |maker| {
        const before_allocs = host_env.alloc_count;
        const aliased_ptr = maker.make() orelse {
            std.debug.print("provided {s} callable maker returned null\n", .{maker.name});
            return 1;
        };
        if (host_env.alloc_count == before_allocs) {
            std.debug.print("provided {s} callable maker did not allocate\n", .{maker.name});
            return 1;
        }
        const aliased: *const AliasedCallables = @ptrCast(@alignCast(aliased_ptr));
        const first = aliased.first;
        const second = aliased.second;
        roc_drop_aliased_boxed_callables(aliased_ptr);

        if (first != second) {
            std.debug.print("provided {s} callables arrived as {?*} and {?*}\n", .{ maker.name, first, second });
            failed = true;
        }
    }
    if (host_env.dealloc_count != host_env.alloc_count) {
        std.debug.print("provided aliased callable drop released {d} of {d} allocations\n", .{
            host_env.dealloc_count,
            host_env.alloc_count,
        });
        failed = true;
    }
    if (failed) return 1;

    std.debug.print("provided boxed callable identity ok\n", .{});
    return 0;
}

const callbacks = host_alloc.Callbacks(HostEnv);

fn rocAllocFn(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    host.alloc_count += 1;
    return callbacks.rocAllocFn(ops, length, alignment);
}

fn rocDeallocFn(ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    host.dealloc_count += 1;
    callbacks.rocDeallocFn(ops, ptr, alignment);
}

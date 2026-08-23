const std = @import("std");
const shim_io = @import("shim_io");
const builtins = @import("builtins");
const host_alloc = @import("host_alloc");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
pub const std_options = shim_io.std_options_static_archive;

const RocOps = builtins.host_abi.RocOps;
const Box = usize;
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,
    bytes: u64 = 0,

    pub fn rocAllocator(self: *HostEnv) std.mem.Allocator {
        return self.arena.allocator();
    }
};

const BaseCallbacks = host_alloc.Callbacks(HostEnv);

fn rocAlloc(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const env: *HostEnv = @ptrCast(@alignCast(ops.env));
    env.bytes += length;
    return BaseCallbacks.rocAllocFn(ops, length, alignment);
}

fn rocRealloc(ops: *RocOps, ptr: *anyopaque, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const env: *HostEnv = @ptrCast(@alignCast(ops.env));
    env.bytes += length;
    return BaseCallbacks.rocReallocFn(ops, ptr, length, alignment);
}

var host_env = HostEnv{ .arena = .init(std.heap.page_allocator) };
var roc_ops = RocOps{
    .env = @ptrCast(&host_env),
    .roc_alloc = rocAlloc,
    .roc_dealloc = BaseCallbacks.rocDeallocFn,
    .roc_realloc = rocRealloc,
    .roc_dbg = BaseCallbacks.rocDbgFn,
    .roc_expect_failed = BaseCallbacks.rocExpectFailedFn,
    .roc_crashed = BaseCallbacks.rocCrashedFn,
    .hosted_fns = .{ .count = 0, .fns = undefined },
};

fn getOps() *RocOps {
    return &roc_ops;
}

fn hostedBranch() callconv(.c) bool {
    return true;
}

extern fn roc_init() callconv(.c) Box;
extern fn roc_init_append() callconv(.c) Box;
extern fn roc_update_straight(Box) callconv(.c) Box;
extern fn roc_update_adapter(Box) callconv(.c) Box;
extern fn roc_update_append(Box) callconv(.c) Box;
extern fn roc_update_pattern(Box) callconv(.c) Box;
extern fn roc_cursor(Box) callconv(.c) u64;

comptime {
    @export(&hostedBranch, .{ .name = "roc_host_branch", .visibility = .hidden });
    host_alloc.exportRuntimeSymbols(getOps, .{});
    @export(&main, .{ .name = "main" });
    if (@import("builtin").os.tag == .windows) @export(&__main, .{ .name = "__main" });
}

fn __main() callconv(.c) void {}

fn checkLoop(
    comptime init: *const fn () callconv(.c) Box,
    comptime update: *const fn (Box) callconv(.c) Box,
    comptime label: []const u8,
) bool {
    var boxed = init();
    for (0..8) |iteration| {
        host_env.bytes = 0;
        boxed = update(boxed);
        if (iteration >= 2 and host_env.bytes > 512) {
            std.debug.print("{s} iteration {d} allocated {d} bytes\n", .{ label, iteration, host_env.bytes });
            _ = roc_cursor(boxed);
            return false;
        }
    }
    const cursor = roc_cursor(boxed);
    if (cursor != 8) {
        std.debug.print("{s} cursor was {d}\n", .{ label, cursor });
        return false;
    }
    return true;
}

fn main(_: c_int, _: [*][*:0]u8) callconv(.c) c_int {
    const straight_ok = checkLoop(&roc_init, &roc_update_straight, "straight");
    const adapter_ok = checkLoop(&roc_init, &roc_update_adapter, "adapter");
    const append_ok = checkLoop(&roc_init_append, &roc_update_append, "append-after-oob");
    const pattern_ok = checkLoop(&roc_init, &roc_update_pattern, "pattern-adapter");
    if (!straight_ok or !adapter_ok or !append_ok or !pattern_ok) return 1;
    std.debug.print("box model updates stayed in place\n", .{});
    return 0;
}

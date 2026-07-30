//! Host that counts the allocations the running app performs and exposes the
//! count to the app as a hosted function (`Host.alloc_count!`), so Roc test
//! apps can assert how many allocations a region of code performed.

const std = @import("std");
const shim_io = @import("shim_io");
const builtins = @import("builtins");
const host_alloc = @import("host_alloc");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_no_stack_tracing` for why stack tracing is disabled.
pub const std_options = shim_io.std_options_no_stack_tracing;

const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;

/// Host environment - contains our arena allocator
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,

    pub fn rocAllocator(self: *HostEnv) std.mem.Allocator {
        return self.arena.allocator();
    }
};

const callbacks = host_alloc.Callbacks(HostEnv);

/// Roc crashed function
fn rocCrashedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    @panic(message);
}

// The app's entrypoint, exported under its provides symbol with its natural
// C ABI under the symbol ABI.
extern fn roc_run(input: RocStr) callconv(.c) RocStr;

// --- Symbol-ABI runtime exports
// The fixed runtime symbols every symbol-ABI host defines. They delegate to
// the same allocator the host's private RocOps uses for builtins helpers.
var g_host_env = HostEnv{
    .arena = .init(std.heap.page_allocator),
};

var g_roc_ops = RocOps{
    .env = @as(*anyopaque, @ptrCast(&g_host_env)),
    .roc_alloc = callbacks.rocAllocFn,
    .roc_dealloc = callbacks.rocDeallocFn,
    .roc_realloc = callbacks.rocReallocFn,
    .roc_dbg = callbacks.rocDbgFn,
    .roc_expect_failed = callbacks.rocExpectFailedFn,
    .roc_crashed = rocCrashedFn,
    .hosted_fns = .{ .count = 0, .fns = undefined }, // No hosted functions in this platform
};

/// Allocations served to the app through the symbol-ABI exports. The host's
/// own uses of builtins helpers (e.g. RocStr.fromSlice) call the vtable
/// callbacks directly and are deliberately not counted.
var g_alloc_count: u64 = 0;

fn countAlloc() void {
    g_alloc_count += 1;
}

/// Host.alloc_count! (hosted): () => U64 involves no refcounted values, so
/// under the hosted C ABI it takes no parameters.
fn hostedAllocCount() callconv(.c) u64 {
    return g_alloc_count;
}

fn getOps() *RocOps {
    return &g_roc_ops;
}

comptime {
    @export(&hostedAllocCount, .{ .name = "roc_host_alloc_count", .visibility = .hidden });
    host_alloc.exportRuntimeSymbols(getOps, .{ .on_alloc = countAlloc });
}

// OS-specific entry point handling
comptime {
    // Export main for all platforms
    @export(&main, .{ .name = "main" });

    // Windows MinGW/MSVCRT compatibility: export __main stub
    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

// Windows MinGW/MSVCRT compatibility stub
// The C runtime on Windows calls __main from main for constructor initialization
fn __main() callconv(.c) void {}

// C compatible main for runtime
fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;
    platform_main() catch |err| {
        std.debug.print("HOST ERROR: {s}", .{@errorName(err)});
        return 1;
    };
    return 0;
}

/// Platform host entrypoint -- calls into the Roc app and prints its result.
fn platform_main() error{TestFailed}!void {
    // The g_roc_ops here is the host's PRIVATE RocOps for using builtins
    // helpers like RocStr; it is not part of the ABI.
    const input_string = "string from host";
    const input_roc_str = RocStr.fromSlice(input_string, &g_roc_ops);
    // Ownership of the argument transfers to the entrypoint.

    // Call the Roc entrypoint with its natural C ABI.
    var roc_str: RocStr = roc_run(input_roc_str);
    defer roc_str.decref(&g_roc_ops);

    std.debug.print("{s}\n", .{roc_str.asSlice()});
}

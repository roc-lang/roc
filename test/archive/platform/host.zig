//! Host for the static-archive test platform. This code is packaged INTO the
//! archive that `roc build` produces, and it exposes the archive's outward-facing
//! C API (`roc_run_app`). A separate consumer program (test/archive/consumer.zig)
//! links the archive and calls that API, which drives the Roc app through the
//! platform ABI.

const std = @import("std");
const shim_io = @import("shim_io");
const host_alloc = @import("host_alloc");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_no_stack_tracing` for why stack tracing is disabled.
pub const std_options = shim_io.std_options_no_stack_tracing;

/// Allocation state for the host's exported runtime symbols. Under the symbol
/// ABI no context parameter reaches these functions; the host owns its
/// delivery, here via a process-global arena.
var host_arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return host_alloc.alloc(host_arena.allocator(), length, alignment) orelse {
        @panic("Host allocation failed");
    };
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    host_alloc.dealloc(host_arena.allocator(), ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return host_alloc.realloc(host_arena.allocator(), ptr, new_length, alignment) orelse {
        @panic("Host reallocation failed");
    };
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    std.debug.print("ROC DBG: {s}\n", .{bytes[0..len]});
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    const trimmed = std.mem.trim(u8, bytes[0..len], " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    @panic(bytes[0..len]);
}

// The fixed runtime symbols every symbol-ABI host defines, plus this
// platform's hosted functions under their header symbols.
comptime {
    host_alloc.exportRuntimeFns(.{
        .alloc = &hostAlloc,
        .dealloc = &hostDealloc,
        .realloc = &hostRealloc,
        .dbg = &hostDbg,
        .expect_failed = &hostExpectFailed,
        .crashed = &hostCrashed,
    });
    @export(&hostedHostDouble, .{ .name = "roc_host_double", .visibility = .hidden });
}

/// Host.double!: double a number in the host. I64 -> I64 involves no
/// refcounted values, so under the hosted C ABI it takes no parameters
/// beyond its arguments.
fn hostedHostDouble(n: i64) callconv(.c) i64 {
    return @call(.never_inline, sharedPrivateHelper, .{n}) * 2;
}

// --- Dead-code-elimination canaries
// The dead hosted function owns one private constant, calls one dead-only
// private helper with its own constant, and also calls a shared private helper
// used by live Host.double!. Final-link section GC must drop the dead-only
// data while keeping the shared helper/data alive.
fn canaryBlob(comptime marker: []const u8) [4096]u8 {
    @setEvalBranchQuota(20000);
    var blob: [4096]u8 = undefined;
    var i: usize = 0;
    while (i < blob.len) : (i += 1) {
        blob[i] = marker[i % marker.len];
    }
    return blob;
}

const dead_hosted_canary_blob = canaryBlob("ROC_DCE_CANARY_BLOB_7f3a9c");
const dead_helper_canary_blob = canaryBlob("ROC_DCE_DEAD_HELPER_BLOB_28d0aa");
const shared_canary_blob = canaryBlob("ROC_DCE_SHARED_BLOB_93e2c1");

fn sharedPrivateHelper(n: i64) i64 {
    std.mem.doNotOptimizeAway(&shared_canary_blob);
    return n;
}

fn deadOnlyPrivateHelper(n: i64) i64 {
    std.mem.doNotOptimizeAway(&dead_helper_canary_blob);
    return n + 1;
}

fn hostUnusedNicheFeature(n: i64) callconv(.c) i64 {
    std.mem.doNotOptimizeAway(&dead_hosted_canary_blob);
    const dead_value = @call(.never_inline, deadOnlyPrivateHelper, .{n});
    return @call(.never_inline, sharedPrivateHelper, .{dead_value});
}

comptime {
    @export(&hostUnusedNicheFeature, .{ .name = "roc_host_unused_niche_feature", .visibility = .hidden });
}

// The app's entrypoint, named by `provides { "roc_main": main_for_host! }`,
// exported with its natural C ABI under the symbol ABI.
extern fn roc_main(n: i64) callconv(.c) i64;

/// The shared library's outward-facing C API: run the Roc app on `n` and
/// return its answer.
export fn roc_run_app(n: i64) callconv(.c) i64 {
    return roc_main(n);
}

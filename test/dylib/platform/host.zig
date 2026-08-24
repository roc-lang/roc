//! Host for the shared-library test platform. Unlike executable platform hosts,
//! this code is linked INTO the shared library that `roc build` produces, and it
//! exposes the library's outward-facing C API (`roc_run_app`). A separate loader
//! process (test/dylib/loader.zig) dlopens the library and calls that API, which
//! drives the Roc app through the platform ABI.

const std = @import("std");
const builtin = @import("builtin");
const host_alloc = @import("host_alloc");
const shim_io = @import("shim_io");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_static_archive` for why these settings matter to a
// static archive that roc links into a program.
pub const std_options = shim_io.std_options_static_archive;

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

// The fixed runtime symbols every symbol-ABI host defines.
comptime {
    host_alloc.exportRuntimeFns(.{
        .alloc = &hostAlloc,
        .dealloc = &hostDealloc,
        .realloc = &hostRealloc,
        .dbg = &hostDbg,
        .expect_failed = &hostExpectFailed,
        .crashed = &hostCrashed,
    });
}

// The platform's hosted functions live in host_hosted.zig, a separate archive
// member: see that file for why.

// The app's entrypoint, named by `provides { "roc_main": main_for_host! }`,
// exported with its natural C ABI under the symbol ABI.
extern fn roc_main(n: i64) callconv(.c) i64;

/// The shared library's outward-facing C API: run the Roc app on `n` and
/// return its answer.
export fn roc_run_app(n: i64) callconv(.c) i64 {
    return roc_main(n);
}

// On ELF/Mach-O, `export fn`'s default visibility is enough for the symbol to
// land in the shared library's export table. COFF has no such notion: a static
// library built from `export fn` carries no `dllexport` intent, so the symbol
// would be dropped from the DLL. Emit the `.drectve /EXPORT:` directive that
// `__declspec(dllexport)` would—`roc build` reads it back out of the host and
// forwards `/export:roc_run_app` to the linker. Any host that exposes a C API
// from a Windows shared library must declare its exports this way.
comptime {
    if (builtin.os.tag == .windows) {
        asm (
            \\.section .drectve,"yn"
            \\.ascii " -export:roc_run_app"
        );
    }
}

//! Loader for the shared-library test platform. Opens the Roc-built shared
//! library at the path given on the command line, looks up its outward-facing
//! C API (`roc_run_app`, defined by test/dylib/platform/host.zig), calls it,
//! and verifies the answer.
//!
//! Run with: zig build run-test-dylib

const std = @import("std");
const builtin = @import("builtin");

const RunAppFn = *const fn (i64) callconv(.c) i64;

const DynLib = switch (builtin.target.os.tag) {
    .windows => struct {
        handle: std.os.windows.HMODULE,

        const kernel32 = struct {
            extern "kernel32" fn LoadLibraryW(lpLibFileName: [*:0]const u16) callconv(.winapi) ?std.os.windows.HMODULE;
            extern "kernel32" fn GetProcAddress(hModule: std.os.windows.HMODULE, lpProcName: [*:0]const u8) callconv(.winapi) ?std.os.windows.FARPROC;
            extern "kernel32" fn FreeLibrary(hLibModule: std.os.windows.HMODULE) callconv(.winapi) c_int;
        };

        fn open(allocator: std.mem.Allocator, path: []const u8) anyerror!@This() {
            const wide_path = try std.unicode.utf8ToUtf16LeAllocZ(allocator, path);
            defer allocator.free(wide_path);
            const handle = kernel32.LoadLibraryW(wide_path.ptr) orelse return error.LibraryNotLoadable;
            return .{ .handle = handle };
        }

        fn close(self: *@This()) void {
            _ = kernel32.FreeLibrary(self.handle);
        }

        fn lookup(self: *@This(), comptime T: type, name: [:0]const u8) ?T {
            const proc = kernel32.GetProcAddress(self.handle, name.ptr) orelse return null;
            return @ptrCast(@alignCast(proc));
        }
    },
    else => struct {
        inner: std.DynLib,

        fn open(_: std.mem.Allocator, path: []const u8) anyerror!@This() {
            return .{ .inner = try std.DynLib.open(path) };
        }

        fn close(self: *@This()) void {
            self.inner.close();
        }

        fn lookup(self: *@This(), comptime T: type, name: [:0]const u8) ?T {
            return self.inner.lookup(T, name);
        }
    },
};

pub fn main(init: std.process.Init) anyerror!void {
    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var arg_iter = try std.process.Args.Iterator.initAllocator(init.minimal.args, arena);
    defer arg_iter.deinit();
    _ = arg_iter.skip();
    const lib_path = arg_iter.next() orelse {
        std.debug.print("Usage: dylib_loader <path-to-shared-library>\n", .{});
        return error.MissingLibraryPath;
    };

    var lib = try DynLib.open(arena, lib_path);
    defer lib.close();

    const roc_run_app = lib.lookup(RunAppFn, "roc_run_app") orelse {
        std.debug.print("FAILED: symbol roc_run_app not found in {s}\n", .{lib_path});
        return error.SymbolNotFound;
    };

    // The platform's `provides` symbol must also be exported under its own name.
    if (lib.lookup(RunAppFn, "roc_main") == null) {
        std.debug.print("FAILED: provides symbol roc_main not exported by {s}\n", .{lib_path});
        return error.SymbolNotFound;
    }

    // Negative: a host symbol that is NOT a declared export (the hosted
    // `roc_host_double`, exported `.hidden`) must not leak into the export
    // table. On Linux this is what auto-export used to paper over; resolving it
    // here on every target catches over-export.
    if (lib.lookup(RunAppFn, "roc_host_double") != null) {
        std.debug.print("FAILED: internal symbol roc_host_double is exported by {s}\n", .{lib_path});
        return error.UnexpectedExport;
    }

    // The app sums an allocated 3-element list of n, doubles it via the hosted
    // Host.double! function, and adds one: 6n + 1.
    const answer = roc_run_app(20);
    if (answer != 121) {
        std.debug.print("FAILED: roc_run_app(20) returned {d}, expected 121\n", .{answer});
        return error.WrongAnswer;
    }

    std.debug.print("SUCCESS: roc_run_app(20) == 121, roc_main exported, roc_host_double hidden\n", .{});
}

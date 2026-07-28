//! Dynamic library loading shared by eval tests and compiler plugins.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const eval_loader = @import("vendor_eval_loader");

const Allocator = std.mem.Allocator;
const native_runtime_libcalls = builtins.native_runtime_libcalls;

/// Errors from opening or resolving symbols in a dynamic library.
pub const Error = Allocator.Error || std.DynLib.Error || error{ InvalidUtf8, LlvmBackendUnavailable };

/// Cross-platform dynamic library loader shared by optimized eval and
/// the glue plugin pipeline.
pub const DynLib = switch (builtin.target.os.tag) {
    .windows => struct {
        handle: std.os.windows.HMODULE,

        const kernel32 = struct {
            extern "kernel32" fn LoadLibraryW(lpLibFileName: [*:0]const u16) callconv(.winapi) ?std.os.windows.HMODULE;
            extern "kernel32" fn GetProcAddress(hModule: std.os.windows.HMODULE, lpProcName: [*:0]const u8) callconv(.winapi) ?std.os.windows.FARPROC;
            extern "kernel32" fn FreeLibrary(hLibModule: std.os.windows.HMODULE) callconv(.winapi) c_int;
        };

        pub fn open(allocator: Allocator, path: [:0]const u8) Error!@This() {
            const wide_path = try std.unicode.utf8ToUtf16LeAllocZ(allocator, path);
            defer allocator.free(wide_path);
            const handle = kernel32.LoadLibraryW(wide_path.ptr) orelse return error.LlvmBackendUnavailable;
            return .{ .handle = handle };
        }

        pub fn close(self: *@This()) void {
            _ = kernel32.FreeLibrary(self.handle);
        }

        pub fn lookup(self: *@This(), comptime T: type, name: [:0]const u8) ?T {
            const proc = kernel32.GetProcAddress(self.handle, name.ptr) orelse return null;
            return @ptrCast(@alignCast(proc));
        }
    },
    else => struct {
        // On a static, no-libc roc binary `std.DynLib` falls back to Zig's
        // `ElfDynLib`, which mishandles writable segments and applies no dynamic
        // relocations. Use a vendored loader that does both correctly. Every
        // other configuration keeps `std.DynLib`, whose `DlDynLib` defers to the
        // OS dynamic loader.
        const Inner = if (eval_loader.active) eval_loader.ElfDynLib else std.DynLib;

        inner: Inner,

        pub fn open(_: Allocator, path: [:0]const u8) Error!@This() {
            // The vendored loader has no dynamic linker behind it, so it needs a
            // resolver to bind the compiler-rt libcalls native codegen emits.
            // `std.DynLib` defers to the OS loader, which resolves them itself.
            if (comptime eval_loader.active) {
                return .{ .inner = try Inner.open(path, &native_runtime_libcalls.resolve) };
            } else {
                return .{ .inner = try Inner.open(path) };
            }
        }

        pub fn close(self: *@This()) void {
            self.inner.close();
        }

        pub fn lookup(self: *@This(), comptime T: type, name: [:0]const u8) ?T {
            return self.inner.lookup(T, name);
        }
    },
};

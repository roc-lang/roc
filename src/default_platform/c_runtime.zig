//! C-runtime default app support for non-Linux native targets.
//!
//! The dev backend emits symbol-ABI calls for compiled executables. This object
//! provides those symbols for the synthetic default platform on targets whose
//! process entrypoint is the C runtime's `main`.

const builtin = @import("builtin");
const default_platform_options = @import("default_platform_options");

const RocStr = @import("roc_str_view").RocStr;
const RocList = @import("roc_str_view").RocList;
const roc_args = @import("roc_args");
const shim_symbols = @import("shim_symbols");

const c = switch (builtin.os.tag) {
    .windows => struct {
        extern fn malloc(size: usize) ?*anyopaque;
        extern fn free(ptr: ?*anyopaque) void;
        extern fn exit(code: i32) noreturn;
        extern fn _write(fd: i32, buf: [*]const u8, len: u32) i32;

        fn write(fd: i32, buf: [*]const u8, len: usize) isize {
            const chunk_len: u32 = @intCast(@min(len, 0x7fff_ffff));
            return _write(fd, buf, chunk_len);
        }
    },
    else => struct {
        extern fn malloc(size: usize) ?*anyopaque;
        extern fn free(ptr: ?*anyopaque) void;
        extern fn exit(code: i32) noreturn;
        extern fn write(fd: i32, buf: [*]const u8, len: usize) isize;
    },
};

const AllocationHeader = extern struct {
    raw: [*]u8,
    len: usize,
};

const SourceFrame = extern struct {
    name_ptr: [*]const u8,
    name_len: usize,
    file_ptr: [*]const u8,
    file_len: usize,
    line: u32,
    column: u32,
};

const windows = if (builtin.os.tag == .windows) struct {
    extern "kernel32" fn GetCommandLineW() callconv(.winapi) [*:0]u16;
    extern "shell32" fn CommandLineToArgvW(
        command_line: [*:0]const u16,
        argc: *c_int,
    ) callconv(.winapi) ?[*][*:0]u16;
    extern "kernel32" fn LocalFree(memory: ?*anyopaque) callconv(.winapi) ?*anyopaque;
} else struct {};

comptime {
    @export(&runtimeInit, .{ .name = shim_symbols.roc_default_runtime_init });
    @export(&defaultExit, .{ .name = shim_symbols.roc_default_exit });
    @export(&defaultEchoLine, .{ .name = shim_symbols.roc_default_echo_line });
    @export(&rocDbg, .{ .name = shim_symbols.roc_dbg });
    @export(&rocExpectFailed, .{ .name = shim_symbols.roc_expect_failed });
    @export(&rocCrashed, .{ .name = shim_symbols.roc_crashed });
    @export(&rocDefaultCrashedWithFrames, .{ .name = shim_symbols.roc_default_crashed_with_frames });
    @export(&rocAlloc, .{ .name = shim_symbols.roc_alloc });
    @export(&rocRealloc, .{ .name = shim_symbols.roc_realloc });
    @export(&rocDealloc, .{ .name = shim_symbols.roc_dealloc });
    if (default_platform_options.include_process_entrypoint) {
        @export(&cMain, .{ .name = "main" });
    }
}

/// Set when an inline `expect` fails. A failed inline expect reports and lets
/// the program continue; the process exit turns an otherwise-successful status
/// into 1, matching the interpreter's default-app behavior.
var inline_expect_failed: bool = false;

/// The Roc entrypoint the synthetic default platform exports.
const roc_default_start_main: *const fn (RocList) callconv(.c) i32 =
    @extern(*const fn (RocList) callconv(.c) i32, .{ .name = shim_symbols.roc_default_start_main });

/// The C runtime owns the process entrypoint: it initializes the Roc runtime,
/// runs the Roc entrypoint, and folds failed inline expects into the status.
fn cMain(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    runtimeInit();
    const args = if (comptime builtin.os.tag == .windows)
        windowsArgs() orelse {
            writeAll(2, "Unable to read command-line arguments\n");
            return 1;
        }
    else
        roc_args.fromPosixArgv(@intCast(@max(argc, 0)), argv, &rocAlloc) orelse {
            writeAll(2, "Unable to allocate command-line arguments\n");
            return 1;
        };
    const status = roc_default_start_main(args);
    if (status == 0 and inline_expect_failed) return 1;
    return status;
}

fn windowsArgs() ?RocList {
    var argc: c_int = 0;
    const argv = windows.CommandLineToArgvW(windows.GetCommandLineW(), &argc) orelse return null;
    defer _ = windows.LocalFree(@ptrCast(argv));
    return roc_args.fromWindowsArgv(@intCast(@max(argc, 0)), argv, &rocAlloc);
}

fn runtimeInit() callconv(.c) void {}

fn defaultExit(code: u8) callconv(.c) noreturn {
    if (code == 0 and inline_expect_failed) c.exit(1);
    c.exit(code);
}

fn defaultEchoLine(str: RocStr) callconv(.c) void {
    var owned = str;
    const message = owned.asSlice();
    writeAll(1, message);
    owned.decref(rocDealloc);
}

fn rocDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeAll(2, "[dbg] ");
    writeAll(2, bytes[0..len]);
    writeAll(2, "\n");
}

fn rocExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    inline_expect_failed = true;
    writeAll(2, "Expect failed: ");
    writeAll(2, bytes[0..len]);
    writeAll(2, "\n");
}

fn rocCrashed(bytes: [*]const u8, len: usize) callconv(.c) noreturn {
    writeAll(2, "Roc application crashed with this message:\n\n\t");
    writeAll(2, bytes[0..len]);
    writeAll(2, "\n\n");
    c.exit(1);
}

fn rocDefaultCrashedWithFrames(
    bytes: [*]const u8,
    len: usize,
    _: [*]const SourceFrame,
    _: usize,
) callconv(.c) noreturn {
    rocCrashed(bytes, len);
}

fn rocAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const byte_alignment = normalizedAlignment(alignment);
    const total = length + byte_alignment + @sizeOf(AllocationHeader);
    const raw_any = c.malloc(total) orelse return null;
    const raw: [*]u8 = @ptrCast(raw_any);
    const user_addr = alignForward(@intFromPtr(raw) + @sizeOf(AllocationHeader), byte_alignment);
    const user: [*]u8 = @ptrFromInt(user_addr);
    allocationHeader(user).* = .{ .raw = raw, .len = length };
    return @ptrCast(user);
}

fn rocRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const old_user: [*]u8 = @ptrCast(ptr);
    const old_header = allocationHeader(old_user).*;
    const new_ptr = rocAlloc(new_length, alignment) orelse return null;
    const new_user: [*]u8 = @ptrCast(new_ptr);

    const copy_len = @min(old_header.len, new_length);
    var i: usize = 0;
    while (i < copy_len) : (i += 1) {
        new_user[i] = old_user[i];
    }
    rocDealloc(ptr, alignment);
    return new_ptr;
}

fn rocDealloc(ptr: *anyopaque, _: usize) callconv(.c) void {
    const user: [*]u8 = @ptrCast(ptr);
    c.free(@ptrCast(allocationHeader(user).raw));
}

fn allocationHeader(user: [*]u8) *AllocationHeader {
    return @ptrCast(@alignCast(user - @sizeOf(AllocationHeader)));
}

fn normalizedAlignment(alignment: usize) usize {
    return @max(alignment, @alignOf(usize));
}

fn alignForward(value: usize, alignment: usize) usize {
    return (value + alignment - 1) & ~(alignment - 1);
}

fn writeAll(fd: i32, bytes: []const u8) void {
    var remaining = bytes;
    while (remaining.len != 0) {
        const written = c.write(fd, remaining.ptr, remaining.len);
        if (written <= 0) return;
        remaining = remaining[@intCast(written)..];
    }
}

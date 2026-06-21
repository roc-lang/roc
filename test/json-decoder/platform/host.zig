const std = @import("std");
const builtin = @import("builtin");

pub const std_options: std.Options = .{
    .allow_stack_tracing = false,
};

pub const panic = std.debug.FullPanic(panicImpl);

const input_buffer_size = 16 * 1024;
const stdin_fd: c_int = 0;
const stdout_fd: c_int = 1;
const stderr_fd: c_int = 2;

const HostError = error{
    UnsupportedOperatingSystem,
    InputTooLarge,
    InvalidUtf8,
    ReadFailed,
    WriteFailed,
};

const RocStr = extern struct {
    bytes: ?[*]u8,
    capacity_or_alloc_ptr: usize,
    length: usize,

    const small_bit: usize = @as(usize, 1) << (@bitSizeOf(usize) - 1);
    const seamless_slice_tag: usize = 1;

    fn empty() RocStr {
        return .{
            .bytes = null,
            .capacity_or_alloc_ptr = 0,
            .length = small_bit,
        };
    }

    fn borrowed(slice: []const u8) RocStr {
        if (slice.len < @sizeOf(RocStr)) {
            var result = RocStr.empty();
            const out: [*]u8 = @ptrCast(&result);
            @memcpy(out[0..slice.len], slice);
            out[@sizeOf(RocStr) - 1] = @intCast(slice.len | 0x80);
            return result;
        }

        return .{
            .bytes = @constCast(slice.ptr),
            .capacity_or_alloc_ptr = @intFromPtr(staticBorrowedDataPtr()) | seamless_slice_tag,
            .length = slice.len,
        };
    }
};

const StaticBorrowedData = extern struct {
    refcount: isize,
    data: u8,
};

var static_borrowed_data: StaticBorrowedData align(@alignOf(usize)) = .{
    .refcount = 0,
    .data = 0,
};

fn staticBorrowedDataPtr() [*]u8 {
    return @as([*]u8, @ptrCast(&static_borrowed_data.data));
}

extern fn roc_main(json: RocStr) callconv(.c) u64;

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });

    switch (builtin.os.tag) {
        .linux => {
            if (builtin.cpu.arch == .x86_64) {
                @export(&linuxStartX86_64, .{ .name = "_start" });
                @export(&linuxStartMain, .{ .name = "roc_json_linux_start_main", .visibility = .hidden });
            } else {
                @export(&linuxStart, .{ .name = "_start" });
            }
            @export(&linuxMemcpy, .{ .name = "memcpy", .visibility = .hidden });
            @export(&linuxMemmove, .{ .name = "memmove", .visibility = .hidden });
            @export(&linuxMemset, .{ .name = "memset", .visibility = .hidden });
        },
        .macos, .windows => {
            @export(&cMain, .{ .name = "main" });
            if (builtin.os.tag == .windows) {
                @export(&windowsMainStub, .{ .name = "__main" });
            }
        },
        else => {},
    }
}

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    _ = addr;
    rawWriteStderr("zig panic: ");
    rawWriteStderr(msg);
    rawWriteStderr("\n");
    abortProcess();
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = length;
    _ = alignment;
    rawWriteStderr("roc_alloc called\n");
    abortProcess();
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    _ = ptr;
    _ = alignment;
    rawWriteStderr("roc_dealloc called\n");
    abortProcess();
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = ptr;
    _ = new_length;
    _ = alignment;
    rawWriteStderr("roc_realloc called\n");
    abortProcess();
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rawWriteStderr(bytes[0..len]);
    rawWriteStderr("\n");
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rawWriteStderr("roc_expect_failed: ");
    rawWriteStderr(bytes[0..len]);
    rawWriteStderr("\n");
    abortProcess();
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rawWriteStderr("roc_crashed: ");
    rawWriteStderr(bytes[0..len]);
    rawWriteStderr("\n");
    abortProcess();
}

fn windowsMainStub() callconv(.c) void {}

fn cMain(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;
    return hostMain();
}

fn linuxStart() callconv(.c) noreturn {
    if (comptime builtin.os.tag != .linux) unreachable;
    linux.exitGroup(hostMain());
}

fn linuxStartMain() callconv(.c) c_int {
    return hostMain();
}

fn linuxStartX86_64() callconv(.naked) noreturn {
    asm volatile (
        \\call roc_json_linux_start_main
        \\movl %%eax, %%edi
        \\movl $231, %%eax
        \\syscall
        \\ud2
    );
}

fn linuxMemcpy(dest: ?*anyopaque, src: ?*const anyopaque, len: usize) callconv(.c) ?*anyopaque {
    const dest_ptr: [*]u8 = @ptrCast(dest.?);
    const src_ptr: [*]const u8 = @ptrCast(src.?);
    var i: usize = 0;
    while (i < len) : (i += 1) {
        dest_ptr[i] = src_ptr[i];
    }
    return dest;
}

fn linuxMemmove(dest: ?*anyopaque, src: ?*const anyopaque, len: usize) callconv(.c) ?*anyopaque {
    const dest_ptr: [*]u8 = @ptrCast(dest.?);
    const src_ptr: [*]const u8 = @ptrCast(src.?);

    if (@intFromPtr(dest_ptr) <= @intFromPtr(src_ptr)) {
        var i: usize = 0;
        while (i < len) : (i += 1) {
            dest_ptr[i] = src_ptr[i];
        }
    } else {
        var i = len;
        while (i != 0) {
            i -= 1;
            dest_ptr[i] = src_ptr[i];
        }
    }

    return dest;
}

fn linuxMemset(dest: ?*anyopaque, byte: c_int, len: usize) callconv(.c) ?*anyopaque {
    const dest_ptr: [*]u8 = @ptrCast(dest.?);
    const value: u8 = @truncate(@as(c_uint, @bitCast(byte)));
    var i: usize = 0;
    while (i < len) : (i += 1) {
        dest_ptr[i] = value;
    }
    return dest;
}

fn hostMain() c_int {
    var input_buffer: [input_buffer_size]u8 = undefined;
    const input = readAllStdin(&input_buffer) catch |err| return fail(err);

    if (!std.unicode.utf8ValidateSlice(input)) return fail(error.InvalidUtf8);

    const result = roc_main(RocStr.borrowed(input));

    var result_buf: [32]u8 = undefined;
    rawWriteStdout(formatU64(result, &result_buf));
    rawWriteStdout("\n");
    return 0;
}

fn fail(err: anyerror) c_int {
    rawWriteStderr("json parser host failed: ");
    rawWriteStderr(@errorName(err));
    rawWriteStderr("\n");
    return 1;
}

fn readAllStdin(buffer: *[input_buffer_size]u8) HostError![]const u8 {
    var total: usize = 0;

    while (true) {
        if (total == buffer.len) return error.InputTooLarge;

        const n = try rawRead(stdin_fd, buffer[total..]);
        if (n == 0) return buffer[0..total];

        total += n;
    }
}

fn rawRead(fd: c_int, buffer: []u8) HostError!usize {
    return switch (builtin.os.tag) {
        .linux => linux.rawRead(fd, buffer),
        .macos => darwin.rawRead(fd, buffer),
        .windows => windows.rawRead(fd, buffer),
        else => error.UnsupportedOperatingSystem,
    };
}

fn rawWriteStdout(bytes: []const u8) void {
    rawWrite(stdout_fd, bytes);
}

fn rawWriteStderr(bytes: []const u8) void {
    rawWrite(stderr_fd, bytes);
}

fn rawWrite(fd: c_int, bytes: []const u8) void {
    switch (builtin.os.tag) {
        .linux => linux.rawWrite(fd, bytes),
        .macos => darwin.rawWrite(fd, bytes),
        .windows => windows.rawWrite(fd, bytes),
        else => {},
    }
}

fn abortProcess() noreturn {
    switch (builtin.os.tag) {
        .linux => linux.exitGroup(134),
        .macos => darwin.abort(),
        .windows => windows.exitProcess(134),
        else => @trap(),
    }
}

fn formatU64(value: u64, buffer: []u8) []const u8 {
    var n = value;
    var i = buffer.len;

    if (n == 0) {
        i -= 1;
        buffer[i] = '0';
        return buffer[i..];
    }

    while (n != 0) {
        i -= 1;
        buffer[i] = @intCast('0' + (n % 10));
        n /= 10;
    }

    return buffer[i..];
}

const linux = struct {
    const os = std.os.linux;
    const EINTR = @intFromEnum(os.E.INTR);

    fn rawRead(fd: c_int, buffer: []u8) HostError!usize {
        while (true) {
            const result = os.read(fd, buffer.ptr, buffer.len);
            if (!isErr(result)) return result;
            if (errno(result) != EINTR) return error.ReadFailed;
        }
    }

    fn rawWrite(fd: c_int, bytes: []const u8) void {
        var written: usize = 0;
        while (written < bytes.len) {
            const result = os.write(fd, bytes[written..].ptr, bytes.len - written);
            if (isErr(result)) return;
            if (result == 0) return;
            written += result;
        }
    }

    fn exitGroup(code: c_int) noreturn {
        os.exit_group(code);
    }

    fn isErr(result: usize) bool {
        return os.errno(result) != .SUCCESS;
    }

    fn errno(result: usize) usize {
        return @intFromEnum(os.errno(result));
    }
};

const darwin = struct {
    const EINTR = 4;

    extern "c" fn __error() *c_int;
    extern "c" fn abort() noreturn;
    extern "c" fn read(fd: c_int, buffer: *anyopaque, len: usize) isize;
    extern "c" fn write(fd: c_int, buffer: *const anyopaque, len: usize) isize;

    fn rawRead(fd: c_int, buffer: []u8) HostError!usize {
        while (true) {
            const result = read(fd, buffer.ptr, buffer.len);
            if (result >= 0) return @intCast(result);
            if (__error().* != EINTR) return error.ReadFailed;
        }
    }

    fn rawWrite(fd: c_int, bytes: []const u8) void {
        var written: usize = 0;
        while (written < bytes.len) {
            const result = write(fd, bytes[written..].ptr, bytes.len - written);
            if (result <= 0) return;
            written += @intCast(result);
        }
    }
};

const windows = struct {
    const ERROR_HANDLE_EOF: u32 = 38;
    const ERROR_BROKEN_PIPE: u32 = 109;
    const STD_INPUT_HANDLE: u32 = @bitCast(@as(i32, -10));
    const STD_OUTPUT_HANDLE: u32 = @bitCast(@as(i32, -11));
    const STD_ERROR_HANDLE: u32 = @bitCast(@as(i32, -12));

    extern "kernel32" fn GetStdHandle(nStdHandle: u32) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn GetLastError() callconv(.winapi) u32;
    extern "kernel32" fn ReadFile(hFile: ?*anyopaque, lpBuffer: [*]u8, nNumberOfBytesToRead: u32, lpNumberOfBytesRead: ?*u32, lpOverlapped: ?*anyopaque) callconv(.winapi) c_int;
    extern "kernel32" fn WriteFile(hFile: ?*anyopaque, lpBuffer: [*]const u8, nNumberOfBytesToWrite: u32, lpNumberOfBytesWritten: ?*u32, lpOverlapped: ?*anyopaque) callconv(.winapi) c_int;
    extern "kernel32" fn ExitProcess(exit_code: u32) callconv(.winapi) noreturn;

    fn rawRead(fd: c_int, buffer: []u8) HostError!usize {
        const handle = GetStdHandle(if (fd == stdin_fd) STD_INPUT_HANDLE else return error.ReadFailed);
        const chunk_len = @min(buffer.len, @as(usize, std.math.maxInt(u32)));
        var read: u32 = 0;
        if (ReadFile(handle, buffer.ptr, @intCast(chunk_len), &read, null) == 0) {
            return switch (GetLastError()) {
                ERROR_BROKEN_PIPE, ERROR_HANDLE_EOF => 0,
                else => error.ReadFailed,
            };
        }
        return read;
    }

    fn rawWrite(fd: c_int, bytes: []const u8) void {
        const handle = GetStdHandle(if (fd == stderr_fd) STD_ERROR_HANDLE else STD_OUTPUT_HANDLE);
        var written_total: usize = 0;
        while (written_total < bytes.len) {
            const chunk_len = @min(bytes.len - written_total, @as(usize, std.math.maxInt(u32)));
            var written: u32 = 0;
            if (WriteFile(handle, bytes[written_total..].ptr, @intCast(chunk_len), &written, null) == 0) return;
            if (written == 0) return;
            written_total += written;
        }
    }

    fn exitProcess(code: u32) noreturn {
        ExitProcess(code);
    }
};

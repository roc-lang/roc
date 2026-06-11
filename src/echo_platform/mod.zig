//! Echo platform module for headerless Roc app modules.
//!
//! Provides the echo! hosted function and utilities for building CLI arguments
//! as Roc types. This module exists as a separate build module to break the
//! dependency cycle: main.zig → builtins → host_abi → @import("root") → main.zig.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");

const is_wasm = builtin.target.cpu.arch == .wasm32;

pub const host_abi = builtins.host_abi;
pub const RocStr = builtins.str.RocStr;
pub const RocList = builtins.list.RocList;

/// Embedded source for the echo platform's main.roc (platform header + main_for_host!).
pub const platform_main_source = @embedFile("platform/main.roc");
/// Embedded source for the echo platform's Echo.roc module (hosted line! function).
pub const echo_module_source = @embedFile("platform/Echo.roc");

/// Echo platform environment, passed as RocOps.env.
/// On WASM the std_io field is unused (undefined); on native it holds the
/// std.Io obtained from the process init or the global single-threaded I/O.
pub const EchoEnv = struct {
    std_io: std.Io,
    /// Set to true the first time roc_expect_failed is invoked. Allows the
    /// host to exit with a non-zero status after running the program.
    inline_expect_failed: bool = false,
};

/// Echo host function: reads a RocStr arg and prints it + newline to stdout.
/// Ownership of `roc_str` transfers to this host function — the RC insertion
/// pass emits zero RC ops for hosted-call args (see test in `src/lir/arc.zig`
/// "RC hosted call transfers unused refcounted arg to host", and the test
/// platform host in `test/fx/platform/host.zig` which decrefs every RocStr
/// arg). Without this decref every `echo!` call leaks one heap RocStr.
pub fn echoHostedFn(ops: *host_abi.RocOps, str: RocStr) callconv(.c) void {
    var owned = str;
    defer owned.decref(ops);

    const message = owned.asSlice();

    if (comptime is_wasm) {
        const js = struct {
            extern "env" fn js_echo(ptr: [*]const u8, len: usize) void;
        };
        js.js_echo(message.ptr, message.len);
    } else {
        const env: *EchoEnv = @ptrCast(@alignCast(ops.env));
        const stdout_file: std.Io.File = .stdout();
        stdout_file.writeStreamingAll(env.std_io, message) catch |err| handleStdoutError(err);
        stdout_file.writeStreamingAll(env.std_io, "\n") catch |err| handleStdoutError(err);
    }
    // Returns {} (ZST) — no bytes to write to ret_bytes
}

/// Handle stdout write errors: exit cleanly on broken pipe (standard
/// Unix behavior), crash on other errors.
fn handleStdoutError(err: anyerror) noreturn {
    if (comptime is_wasm) {
        @trap();
    } else {
        switch (err) {
            error.BrokenPipe => std.process.exit(0),
            else => {
                std.debug.print("echo!: stdout write failed: {}\n", .{err});
                std.process.exit(1);
            },
        }
    }
}

/// Create a minimal RocOps struct for default_app execution.
pub fn makeDefaultRocOps(env: *EchoEnv, hosted_fns: []host_abi.HostedFn) host_abi.RocOps {
    const fns = struct {
        const size_prefix = @sizeOf(usize);

        /// Allocate with a size prefix so realloc/dealloc can recover the old length.
        fn rocAlloc(_: *host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
            const alloc = if (comptime is_wasm) std.heap.wasm_allocator else std.heap.smp_allocator;
            const total = length + size_prefix;
            const align_enum = std.mem.Alignment.fromByteUnits(@max(alignment, @alignOf(usize)));
            const raw = alloc.rawAlloc(total, align_enum, @returnAddress()) orelse {
                if (comptime is_wasm) @trap() else {
                    std.debug.print("roc_alloc failed: OOM\n", .{});
                    std.process.exit(1);
                }
            };
            // Store the allocation length in the prefix
            const size_slot: *usize = @ptrCast(@alignCast(raw));
            size_slot.* = length;
            return @ptrCast(raw + size_prefix);
        }

        fn rocDealloc(_: *host_abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
            const alloc = if (comptime is_wasm) std.heap.wasm_allocator else std.heap.smp_allocator;
            // Recover the length rocAlloc stored in the prefix, then free the whole block.
            const user_ptr: [*]u8 = @ptrCast(ptr);
            const raw = user_ptr - size_prefix;
            const length = @as(*const usize, @ptrCast(@alignCast(raw))).*;
            const align_enum = std.mem.Alignment.fromByteUnits(@max(alignment, @alignOf(usize)));
            alloc.rawFree(raw[0 .. length + size_prefix], align_enum, @returnAddress());
        }

        fn rocRealloc(_: *host_abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
            const alloc = if (comptime is_wasm) std.heap.wasm_allocator else std.heap.smp_allocator;
            const align_enum = std.mem.Alignment.fromByteUnits(@max(alignment, @alignOf(usize)));

            // Read old size from prefix
            const old_ptr: [*]u8 = @ptrCast(ptr);
            const old_raw = old_ptr - size_prefix;
            const old_size: usize = @as(*const usize, @ptrCast(@alignCast(old_raw))).*;

            // Allocate new block with size prefix
            const new_total = new_length + size_prefix;
            const new_raw = alloc.rawAlloc(new_total, align_enum, @returnAddress()) orelse {
                if (comptime is_wasm) @trap() else {
                    std.debug.print("roc_realloc failed: OOM\n", .{});
                    std.process.exit(1);
                }
            };

            // Write new size prefix
            const new_size_slot: *usize = @ptrCast(@alignCast(new_raw));
            new_size_slot.* = new_length;
            const new_ptr = new_raw + size_prefix;

            // Copy old data (only up to the smaller of old/new sizes)
            const copy_len = @min(old_size, new_length);
            if (copy_len > 0) {
                @memcpy(new_ptr[0..copy_len], old_ptr[0..copy_len]);
            }

            // Free the old block now that its contents have been copied.
            alloc.rawFree(old_raw[0 .. old_size + size_prefix], align_enum, @returnAddress());

            return @ptrCast(new_ptr);
        }

        fn rocDbg(ops: *host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
            if (comptime is_wasm) {
                // No-op on wasm — no stderr available
            } else {
                const echo_env: *EchoEnv = @ptrCast(@alignCast(ops.env));
                const msg = bytes[0..len];
                const stderr_file: std.Io.File = .stderr();
                stderr_file.writeStreamingAll(echo_env.std_io, "[dbg] ") catch {};
                stderr_file.writeStreamingAll(echo_env.std_io, msg) catch {};
                stderr_file.writeStreamingAll(echo_env.std_io, "\n") catch {};
            }
        }
        fn rocExpectFailed(ops: *host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
            const echo_env_for_flag: *EchoEnv = @ptrCast(@alignCast(ops.env));
            echo_env_for_flag.inline_expect_failed = true;
            if (comptime is_wasm) {
                // No-op on wasm — no stderr available
            } else {
                const echo_env: *EchoEnv = @ptrCast(@alignCast(ops.env));
                const msg = bytes[0..len];
                const stderr_file: std.Io.File = .stderr();
                stderr_file.writeStreamingAll(echo_env.std_io, "Expect failed: ") catch {};
                stderr_file.writeStreamingAll(echo_env.std_io, msg) catch {};
                stderr_file.writeStreamingAll(echo_env.std_io, "\n") catch {};
            }
        }
        fn rocCrashed(ops: *host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
            if (comptime is_wasm) {
                @trap();
            } else {
                const echo_env: *EchoEnv = @ptrCast(@alignCast(ops.env));
                const msg = bytes[0..len];
                const stderr_file: std.Io.File = .stderr();
                stderr_file.writeStreamingAll(echo_env.std_io, "Roc crashed: ") catch {};
                stderr_file.writeStreamingAll(echo_env.std_io, msg) catch {};
                stderr_file.writeStreamingAll(echo_env.std_io, "\n") catch {};
                std.process.exit(1);
            }
        }
    };

    return .{
        .env = @ptrCast(env),
        .roc_alloc = &fns.rocAlloc,
        .roc_dealloc = &fns.rocDealloc,
        .roc_realloc = &fns.rocRealloc,
        .roc_dbg = &fns.rocDbg,
        .roc_expect_failed = &fns.rocExpectFailed,
        .roc_crashed = &fns.rocCrashed,
        .hosted_fns = .{ .count = @intCast(hosted_fns.len), .fns = hosted_fns.ptr },
    };
}

/// Build a RocList of RocStr from CLI argument slices.
/// Each argument is sanitized to valid UTF-8.
pub fn buildCliArgs(app_args: []const []const u8, roc_ops: *host_abi.RocOps) std.mem.Allocator.Error!RocList {
    if (comptime is_wasm) return RocList.empty();
    if (app_args.len == 0) return RocList.empty();

    const allocator = std.heap.smp_allocator;
    const roc_strs = try allocator.alloc(RocStr, app_args.len);
    defer allocator.free(roc_strs);

    for (app_args, 0..) |arg, i| {
        const sanitized = try sanitizeUtf8(arg, allocator);
        roc_strs[i] = RocStr.fromSlice(sanitized, roc_ops);
        // fromSlice copied the bytes into Roc memory, so the host scratch is done.
        if (sanitized.ptr != arg.ptr) allocator.free(sanitized);
    }

    return RocList.fromSlice(RocStr, roc_strs, true, roc_ops);
}

/// Sanitize a byte slice to valid UTF-8, replacing invalid bytes with U+FFFD.
/// Returns the input slice unchanged if it's already valid UTF-8.
fn sanitizeUtf8(input: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
    if (std.unicode.utf8ValidateSlice(input)) return input;

    // Worst case: each invalid byte becomes 3-byte replacement char
    const buf = try allocator.alloc(u8, input.len * 3);
    var out_i: usize = 0;
    var in_i: usize = 0;
    while (in_i < input.len) {
        const seq_len = std.unicode.utf8ByteSequenceLength(input[in_i]) catch {
            // Invalid lead byte — replacement char
            buf[out_i] = 0xEF;
            buf[out_i + 1] = 0xBF;
            buf[out_i + 2] = 0xBD;
            out_i += 3;
            in_i += 1;
            continue;
        };
        if (in_i + seq_len > input.len) {
            // Truncated sequence
            buf[out_i] = 0xEF;
            buf[out_i + 1] = 0xBF;
            buf[out_i + 2] = 0xBD;
            out_i += 3;
            in_i += 1;
            continue;
        }
        if (std.unicode.utf8Decode(input[in_i..][0..seq_len])) |_| {
            @memcpy(buf[out_i..][0..seq_len], input[in_i..][0..seq_len]);
            out_i += seq_len;
            in_i += seq_len;
        } else |_| {
            buf[out_i] = 0xEF;
            buf[out_i + 1] = 0xBF;
            buf[out_i + 2] = 0xBD;
            out_i += 3;
            in_i += 1;
        }
    }
    // realloc to the exact length so the returned slice is the whole allocation
    // and callers can free it (a plain resize can decline a shrink, e.g. across
    // smp size classes, which would leave the returned sub-slice unfreeable).
    return allocator.realloc(buf, out_i) catch buf[0..out_i];
}

const testing = std.testing;
const test_allocator = std.testing.allocator;

test "sanitizeUtf8: valid ASCII passes through unchanged" {
    const input = "hello world";
    const result = try sanitizeUtf8(input, test_allocator);
    try testing.expectEqualStrings("hello world", result);
    // Should return the original slice (no allocation)
    try testing.expectEqual(input.ptr, result.ptr);
}

test "sanitizeUtf8: valid multibyte UTF-8 passes through unchanged" {
    const input = "na\xc3\xafve \xe2\x9c\x93"; // "naïve ✓"
    const result = try sanitizeUtf8(input, test_allocator);
    try testing.expectEqualStrings(input, result);
    try testing.expectEqual(input.ptr, result.ptr);
}

test "sanitizeUtf8: single invalid byte becomes replacement char" {
    const result = try sanitizeUtf8("\xff", test_allocator);
    defer test_allocator.free(result);
    try testing.expectEqualStrings("\xef\xbf\xbd", result); // U+FFFD
}

test "sanitizeUtf8: invalid byte surrounded by valid ASCII" {
    const result = try sanitizeUtf8("a\xffb", test_allocator);
    defer test_allocator.free(result);
    try testing.expectEqualStrings("a\xef\xbf\xbdb", result);
}

test "sanitizeUtf8: truncated 2-byte sequence" {
    // 0xC3 starts a 2-byte sequence but there's no continuation byte
    const result = try sanitizeUtf8("a\xc3", test_allocator);
    defer test_allocator.free(result);
    try testing.expectEqualStrings("a\xef\xbf\xbd", result);
}

test "sanitizeUtf8: truncated 3-byte sequence" {
    // 0xE2 starts a 3-byte sequence but only one continuation follows
    const result = try sanitizeUtf8("\xe2\x9c", test_allocator);
    defer test_allocator.free(result);
    // Each byte of the truncated sequence is replaced individually
    try testing.expectEqualStrings("\xef\xbf\xbd\xef\xbf\xbd", result);
}

test "sanitizeUtf8: multiple consecutive invalid bytes" {
    const result = try sanitizeUtf8("\xff\xfe\xfd", test_allocator);
    defer test_allocator.free(result);
    // Each invalid byte gets its own replacement char
    try testing.expectEqualStrings("\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd", result);
}

test "sanitizeUtf8: empty input" {
    const input: []const u8 = "";
    const result = try sanitizeUtf8(input, test_allocator);
    try testing.expectEqual(@as(usize, 0), result.len);
}

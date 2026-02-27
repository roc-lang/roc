//! Echo platform module for headerless Roc app modules.
//!
//! Provides the echo! hosted function and utilities for building CLI arguments
//! as Roc types. This module exists as a separate build module to break the
//! dependency cycle: main.zig → builtins → host_abi → @import("root") → main.zig.

const std = @import("std");
const builtins = @import("builtins");

pub const host_abi = builtins.host_abi;
pub const RocStr = builtins.str.RocStr;
pub const RocList = builtins.list.RocList;

/// Embedded source for the echo platform's main.roc (platform header + main_for_host!).
pub const platform_main_source = @embedFile("platform/main.roc");
/// Embedded source for the echo platform's Echo.roc module (hosted line! function).
pub const echo_module_source = @embedFile("platform/Echo.roc");

/// Static sentinel for RocOps.env — a valid non-null address that won't segfault
/// if accidentally inspected in a debugger, unlike @ptrFromInt(1).
const env_sentinel: u8 = 0;

/// Echo host function: reads a RocStr arg and prints it + newline to stdout.
/// Arguments are borrowed — refcounting is handled by the caller (RC insertion pass).
pub fn echoHostedFn(_: *anyopaque, _: [*]u8, roc_str: *RocStr) callconv(.c) void {
    const message = roc_str.asSlice();
    const stdout_file: std.fs.File = .stdout();
    stdout_file.writeAll(message) catch |err| handleStdoutError(err);
    stdout_file.writeAll("\n") catch |err| handleStdoutError(err);
    // Returns {} (ZST) — no bytes to write to ret_bytes
}

/// Handle stdout write errors: exit cleanly on broken pipe (standard
/// Unix behavior), crash on other errors.
fn handleStdoutError(err: anyerror) noreturn {
    switch (err) {
        error.BrokenPipe => std.process.exit(0),
        else => {
            std.debug.print("echo!: stdout write failed: {}\n", .{err});
            std.process.exit(1);
        },
    }
}

/// Create a minimal RocOps struct for default_app execution.
pub fn makeDefaultRocOps(hosted_fns: []host_abi.HostedFn) host_abi.RocOps {
    const fns = struct {
        const size_prefix = @sizeOf(usize);

        /// Allocate with a size prefix so realloc/dealloc can recover the old length.
        fn rocAlloc(alloc_args: *host_abi.RocAlloc, _: *anyopaque) callconv(.c) void {
            const allocator = std.heap.page_allocator;
            const total = alloc_args.length + size_prefix;
            const align_enum = std.mem.Alignment.fromByteUnits(@max(alloc_args.alignment, @alignOf(usize)));
            const raw = allocator.rawAlloc(total, align_enum, @returnAddress()) orelse {
                std.debug.print("roc_alloc failed: OOM\n", .{});
                std.process.exit(1);
            };
            // Store the allocation length in the prefix
            const size_slot: *usize = @ptrCast(@alignCast(raw));
            size_slot.* = alloc_args.length;
            alloc_args.answer = @ptrCast(raw + size_prefix);
        }

        fn rocDealloc(_: *host_abi.RocDealloc, _: *anyopaque) callconv(.c) void {
            // No-op for simplicity — short-lived process, pages freed on exit
        }

        fn rocRealloc(realloc_args: *host_abi.RocRealloc, _: *anyopaque) callconv(.c) void {
            const allocator = std.heap.page_allocator;
            const align_enum = std.mem.Alignment.fromByteUnits(@max(realloc_args.alignment, @alignOf(usize)));

            // Read old size from prefix
            const old_ptr: [*]u8 = @ptrCast(realloc_args.answer);
            const old_raw = old_ptr - size_prefix;
            const old_size: usize = @as(*const usize, @ptrCast(@alignCast(old_raw))).*;

            // Allocate new block with size prefix
            const new_total = realloc_args.new_length + size_prefix;
            const new_raw = allocator.rawAlloc(new_total, align_enum, @returnAddress()) orelse {
                std.debug.print("roc_realloc failed: OOM\n", .{});
                std.process.exit(1);
            };

            // Write new size prefix
            const new_size_slot: *usize = @ptrCast(@alignCast(new_raw));
            new_size_slot.* = realloc_args.new_length;
            const new_ptr = new_raw + size_prefix;

            // Copy old data (only up to the smaller of old/new sizes)
            const copy_len = @min(old_size, realloc_args.new_length);
            if (copy_len > 0) {
                @memcpy(new_ptr[0..copy_len], old_ptr[0..copy_len]);
            }

            realloc_args.answer = @ptrCast(new_ptr);
        }

        fn rocDbg(dbg_args: *const host_abi.RocDbg, _: *anyopaque) callconv(.c) void {
            const msg = dbg_args.utf8_bytes[0..dbg_args.len];
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll("[dbg] ") catch {};
            stderr_file.writeAll(msg) catch {};
            stderr_file.writeAll("\n") catch {};
        }
        fn rocExpectFailed(expect_args: *const host_abi.RocExpectFailed, _: *anyopaque) callconv(.c) void {
            const msg = expect_args.utf8_bytes[0..expect_args.len];
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll("Expect failed: ") catch {};
            stderr_file.writeAll(msg) catch {};
            stderr_file.writeAll("\n") catch {};
        }
        fn rocCrashed(crash_args: *const host_abi.RocCrashed, _: *anyopaque) callconv(.c) void {
            const msg = crash_args.utf8_bytes[0..crash_args.len];
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll("Roc crashed: ") catch {};
            stderr_file.writeAll(msg) catch {};
            stderr_file.writeAll("\n") catch {};
            std.process.exit(1);
        }
    };

    return .{
        .env = @constCast(&env_sentinel),
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
pub fn buildCliArgs(app_args: []const []const u8, roc_ops: *host_abi.RocOps) RocList {
    if (app_args.len == 0) return RocList.empty();

    const allocator = std.heap.page_allocator;
    const roc_strs = allocator.alloc(RocStr, app_args.len) catch return RocList.empty();

    for (app_args, 0..) |arg, i| {
        const sanitized = sanitizeUtf8(arg, allocator);
        roc_strs[i] = RocStr.fromSlice(sanitized, roc_ops);
    }

    return RocList.fromSlice(RocStr, roc_strs, true, roc_ops);
}

/// Sanitize a byte slice to valid UTF-8, replacing invalid bytes with U+FFFD.
/// Returns the input slice unchanged if it's already valid UTF-8.
fn sanitizeUtf8(input: []const u8, allocator: std.mem.Allocator) []const u8 {
    if (std.unicode.utf8ValidateSlice(input)) return input;

    // Worst case: each invalid byte becomes 3-byte replacement char
    const buf = allocator.alloc(u8, input.len * 3) catch return input;
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
    _ = allocator.resize(buf, out_i);
    return buf[0..out_i];
}

const testing = std.testing;
// sanitizeUtf8 uses allocator.resize which page_allocator supports but
// testing.allocator (GeneralPurposeAllocator) does not handle well with
// sub-slice frees. Use page_allocator to match production behavior.
const test_allocator = std.heap.page_allocator;

test "sanitizeUtf8: valid ASCII passes through unchanged" {
    const input = "hello world";
    const result = sanitizeUtf8(input, test_allocator);
    try testing.expectEqualStrings("hello world", result);
    // Should return the original slice (no allocation)
    try testing.expectEqual(input.ptr, result.ptr);
}

test "sanitizeUtf8: valid multibyte UTF-8 passes through unchanged" {
    const input = "na\xc3\xafve \xe2\x9c\x93"; // "naïve ✓"
    const result = sanitizeUtf8(input, test_allocator);
    try testing.expectEqualStrings(input, result);
    try testing.expectEqual(input.ptr, result.ptr);
}

test "sanitizeUtf8: single invalid byte becomes replacement char" {
    const result = sanitizeUtf8("\xff", test_allocator);
    try testing.expectEqualStrings("\xef\xbf\xbd", result); // U+FFFD
}

test "sanitizeUtf8: invalid byte surrounded by valid ASCII" {
    const result = sanitizeUtf8("a\xffb", test_allocator);
    try testing.expectEqualStrings("a\xef\xbf\xbdb", result);
}

test "sanitizeUtf8: truncated 2-byte sequence" {
    // 0xC3 starts a 2-byte sequence but there's no continuation byte
    const result = sanitizeUtf8("a\xc3", test_allocator);
    try testing.expectEqualStrings("a\xef\xbf\xbd", result);
}

test "sanitizeUtf8: truncated 3-byte sequence" {
    // 0xE2 starts a 3-byte sequence but only one continuation follows
    const result = sanitizeUtf8("\xe2\x9c", test_allocator);
    // Each byte of the truncated sequence is replaced individually
    try testing.expectEqualStrings("\xef\xbf\xbd\xef\xbf\xbd", result);
}

test "sanitizeUtf8: multiple consecutive invalid bytes" {
    const result = sanitizeUtf8("\xff\xfe\xfd", test_allocator);
    // Each invalid byte gets its own replacement char
    try testing.expectEqualStrings("\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd", result);
}

test "sanitizeUtf8: empty input" {
    const input: []const u8 = "";
    const result = sanitizeUtf8(input, test_allocator);
    try testing.expectEqual(@as(usize, 0), result.len);
}

//! Platform host for testing effectful Roc applications.
//!
//! This host provides stdin/stdout/stderr effects and includes a test mode for
//! verifying IO behavior without performing actual syscalls.
//!
//! ## Test Mode
//!
//! Run with `--test <spec>` to simulate IO and verify behavior:
//! ```
//! ./zig-out/bin/roc app.roc -- --test "1>Hello, world!"
//! ```
//!
//! Spec format uses pipe-separated operations:
//! - `0<input` - provide "input" as stdin
//! - `1>output` - expect "output" on stdout
//! - `2>output` - expect "output" on stderr
//!
//! Example with multiple operations:
//! ```
//! --test "0<user input|1>Before stdin|1>After stdin"
//! ```
//!
//! Use `--test-verbose <spec>` for detailed output during test execution.
//!
//! Exit codes:
//! - 0: All expectations matched in order
//! - 1: Test failed (mismatch, missing output, extra output, or invalid spec)
const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const build_options = @import("build_options");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .wasi) std.posix else undefined;

const trace_refcount = build_options.trace_refcount;

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
};

/// Override the default panic handler to avoid secondary crashes in stack trace generation
pub const panic = std.debug.FullPanic(panicImpl);

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    const stderr: std.fs.File = .stderr();
    stderr.writeAll("\n=== PANIC (no stack trace) ===\n") catch {};
    stderr.writeAll(msg) catch {};
    if (addr) |a| {
        var buf: [32]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, " at address 0x{x}\n", .{a}) catch "";
        stderr.writeAll(hex) catch {};
    } else {
        stderr.writeAll("\n") catch {};
    }
    std.process.abort();
}

/// Error message to display on stack overflow in a Roc program
const STACK_OVERFLOW_MESSAGE = "\nThis Roc application overflowed its stack memory and crashed.\n\n";

/// Callback for stack overflow in a Roc program
fn handleRocStackOverflow() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(134);
    } else if (comptime builtin.os.tag != .wasi) {
        _ = posix.write(posix.STDERR_FILENO, STACK_OVERFLOW_MESSAGE) catch {};
        posix.exit(134);
    } else {
        std.process.exit(134);
    }
}

/// Callback for access violation in a Roc program
fn handleRocAccessViolation(fault_addr: usize) noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        var addr_buf: [18]u8 = undefined;
        const addr_str = builtins.handlers.formatHex(fault_addr, &addr_buf);

        const msg1 = "\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ";
        const msg2 = "\n\n";
        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, msg1.ptr, msg1.len, &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, addr_str.ptr, @intCast(addr_str.len), &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, msg2.ptr, msg2.len, &bytes_written, null);
        kernel32.ExitProcess(139);
    } else {
        // POSIX (and WASI fallback)
        const msg = "\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ";
        _ = posix.write(posix.STDERR_FILENO, msg) catch {};

        var addr_buf: [18]u8 = undefined;
        const addr_str = builtins.handlers.formatHex(fault_addr, &addr_buf);
        _ = posix.write(posix.STDERR_FILENO, addr_str) catch {};
        _ = posix.write(posix.STDERR_FILENO, "\n\n") catch {};
        posix.exit(139);
    }
}

/// Error message to display on division by zero in a Roc program
const DIVISION_BY_ZERO_MESSAGE = "\nThis Roc application divided by zero and crashed.\n\n";

/// Callback for arithmetic errors (division by zero) in a Roc program
fn handleRocArithmeticError() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, DIVISION_BY_ZERO_MESSAGE.ptr, DIVISION_BY_ZERO_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(136);
    } else if (comptime builtin.os.tag != .wasi) {
        _ = posix.write(posix.STDERR_FILENO, DIVISION_BY_ZERO_MESSAGE) catch {};
        posix.exit(136); // 128 + 8 (SIGFPE)
    } else {
        std.process.exit(136);
    }
}

/// Type of IO operation in test spec
const EffectType = enum(u8) {
    stdin_input, // 0<
    stdout_expect, // 1>
    stderr_expect, // 2>
};

/// A single entry in the test spec
const SpecEntry = struct {
    effect_type: EffectType,
    value: []const u8,
    spec_line: usize, // For error reporting
};

/// Test state for simulated IO mode
const TestState = struct {
    enabled: bool,
    verbose: bool,
    entries: []const SpecEntry,
    current_index: usize,
    failed: bool,
    failure_info: ?FailureInfo,

    const FailureInfo = struct {
        expected_type: EffectType,
        expected_value: []const u8,
        actual_type: EffectType,
        actual_value: []const u8,
        spec_line: usize,
    };

    fn init() TestState {
        return .{
            .enabled = false,
            .verbose = false,
            .entries = &.{},
            .current_index = 0,
            .failed = false,
            .failure_info = null,
        };
    }
};

/// Parse error for invalid spec format
const ParseError = error{
    InvalidSpecFormat,
    OutOfMemory,
};

/// Unescape a spec value string, converting \n to actual newlines
/// Always returns an allocated copy (simplifies cleanup)
fn unescapeSpecValue(allocator: std.mem.Allocator, input: []const u8) ParseError![]u8 {
    // Quick check: if no backslash, just duplicate
    if (std.mem.indexOfScalar(u8, input, '\\') == null) {
        return allocator.dupe(u8, input) catch return ParseError.OutOfMemory;
    }

    // Allocate buffer for unescaped string (can only be same size or smaller)
    var result = std.ArrayListUnmanaged(u8).initCapacity(allocator, input.len) catch return ParseError.OutOfMemory;
    errdefer result.deinit(allocator);

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '\\' and i + 1 < input.len) {
            switch (input[i + 1]) {
                'n' => {
                    result.append(allocator, '\n') catch return ParseError.OutOfMemory;
                    i += 2;
                },
                't' => {
                    result.append(allocator, '\t') catch return ParseError.OutOfMemory;
                    i += 2;
                },
                'r' => {
                    result.append(allocator, '\r') catch return ParseError.OutOfMemory;
                    i += 2;
                },
                '\\' => {
                    result.append(allocator, '\\') catch return ParseError.OutOfMemory;
                    i += 2;
                },
                else => {
                    // Not a recognized escape, keep the backslash
                    result.append(allocator, input[i]) catch return ParseError.OutOfMemory;
                    i += 1;
                },
            }
        } else {
            result.append(allocator, input[i]) catch return ParseError.OutOfMemory;
            i += 1;
        }
    }

    return result.toOwnedSlice(allocator) catch ParseError.OutOfMemory;
}

/// Parse test spec string into array of SpecEntry
/// Format: "0<input|1>output|2>error" (pipe-separated)
/// Supports escape sequences in values: \n (newline), \t (tab), \r (carriage return), \\ (backslash)
/// Returns error if any segment doesn't start with a valid pattern (0<, 1>, 2>)
fn parseTestSpec(allocator: std.mem.Allocator, spec: []const u8) ParseError![]SpecEntry {
    var entries = std.ArrayList(SpecEntry).initCapacity(allocator, 8) catch return ParseError.OutOfMemory;
    errdefer entries.deinit(allocator);

    var line_num: usize = 1;
    // Split on pipe character
    var iter = std.mem.splitScalar(u8, spec, '|');

    while (iter.next()) |segment| {
        defer line_num += 1;

        // Skip empty segments (e.g., trailing pipe)
        if (segment.len == 0) continue;

        // Check for valid pattern prefix
        if (segment.len < 2) {
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll("Error: Invalid spec segment '") catch {};
            stderr_file.writeAll(segment) catch {};
            stderr_file.writeAll("' - must start with 0<, 1>, or 2>\n") catch {};
            return ParseError.InvalidSpecFormat;
        }

        const effect_type: EffectType = blk: {
            if (segment[0] == '0' and segment[1] == '<') break :blk .stdin_input;
            if (segment[0] == '1' and segment[1] == '>') break :blk .stdout_expect;
            if (segment[0] == '2' and segment[1] == '>') break :blk .stderr_expect;
            // Invalid pattern - report error
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll("Error: Invalid spec segment '") catch {};
            stderr_file.writeAll(segment) catch {};
            stderr_file.writeAll("' - must start with 0<, 1>, or 2>\n") catch {};
            return ParseError.InvalidSpecFormat;
        };

        // Unescape the value to handle \n, \t, etc.
        const unescaped_value = try unescapeSpecValue(allocator, segment[2..]);

        entries.append(allocator, .{
            .effect_type = effect_type,
            .value = unescaped_value,
            .spec_line = line_num,
        }) catch return ParseError.OutOfMemory;
    }

    return entries.toOwnedSlice(allocator) catch ParseError.OutOfMemory;
}

/// Tracking entry for a Roc allocation
const RocAllocation = struct {
    ptr: [*]u8, // Base pointer (before user data, includes size metadata)
    total_size: usize,
    alignment: std.mem.Alignment,
};

/// Host environment - contains GeneralPurposeAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.GeneralPurposeAllocator(.{ .safety = true }),
    test_state: TestState,
    /// Track Roc allocations for cleanup on test failure
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .{},
    /// Allocation counters for diagnostics
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
    // Debug check: verify roc_alloc pointer alignment
    const roc_alloc_addr = @intFromPtr(roc_alloc);
    if (roc_alloc_addr % @alignOf(builtins.host_abi.RocAlloc) != 0) {
        std.debug.panic("[rocAllocFn] roc_alloc ptr not aligned! addr=0x{x} required={}", .{ roc_alloc_addr, @alignOf(builtins.host_abi.RocAlloc) });
    }

    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("rocAllocFn: env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }

    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    // The allocation must be at least 8-byte aligned because:
    // 1. The refcount (isize/usize) is stored before the data and needs proper alignment
    // 2. The builtins code casts data pointers to [*]isize for refcount access
    const min_alignment: usize = @max(roc_alloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(roc_alloc.alignment, @alignOf(usize));
    const total_size = roc_alloc.length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        const stderr: std.fs.File = .stderr();
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "\x1b[31mHost error:\x1b[0m allocation failed for size={d} align={d}\n", .{
            total_size,
            roc_alloc.alignment,
        }) catch "\x1b[31mHost error:\x1b[0m allocation failed, out of memory\n";
        stderr.writeAll(msg) catch {};
        std.process.exit(1);
    };

    // Debug check: verify the allocator returned properly aligned memory
    const base_addr = @intFromPtr(base_ptr);
    if (base_addr % min_alignment != 0) {
        @panic("Host allocator returned misaligned memory in rocAllocFn");
    }

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    // Debug check: verify the returned pointer is also properly aligned
    const answer_addr = @intFromPtr(roc_alloc.answer);
    if (answer_addr % roc_alloc.alignment != 0) {
        @panic("Host allocator returned misaligned answer in rocAllocFn");
    }

    // Track this allocation for cleanup on test failure
    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch {};
    host.alloc_count += 1;

    if (trace_refcount) {
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ @intFromPtr(roc_alloc.answer), roc_alloc.length, roc_alloc.alignment });
    }
}

/// Roc deallocation function with size-tracking metadata
fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocDeallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    // Use same minimum alignment as alloc
    const min_alignment: usize = @max(roc_dealloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(roc_dealloc.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    if (trace_refcount) {
        std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{
            @intFromPtr(roc_dealloc.ptr),
            roc_dealloc.alignment,
            total_size,
            size_storage_bytes,
        });
    }

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - size_storage_bytes);

    // Remove from tracking list
    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.dealloc_count += 1;

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    allocator.rawFree(slice, align_enum, @returnAddress());
}

/// Roc reallocation function with size-tracking metadata
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocReallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    // Use same minimum alignment as alloc
    const min_alignment: usize = @max(roc_realloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = roc_realloc.new_length + size_storage_bytes;

    // Free old memory and allocate new with proper alignment
    // This is necessary because Zig's realloc infers alignment from slice type ([]u8 = alignment 1)
    // which could cause the new allocation to be misaligned
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];

    // Allocate new memory with proper alignment
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        const stderr: std.fs.File = .stderr();
        stderr.writeAll("\x1b[31mHost error:\x1b[0m reallocation failed, out of memory\n") catch {};
        std.process.exit(1);
    };

    // Copy old data to new location
    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_slice[0..copy_size]);

    // Update tracking: remove old allocation, add new one
    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == old_base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = new_ptr,
        .total_size = new_total_size,
        .alignment = align_enum,
    }) catch {};

    // Free old memory
    allocator.rawFree(old_slice, align_enum, @returnAddress());

    const new_slice = new_ptr[0..new_total_size];

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    roc_realloc.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(old_base_ptr) + size_storage_bytes, @intFromPtr(roc_realloc.answer), roc_realloc.new_length });
    }
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    const stderr: std.fs.File = .stderr();
    var buf: [256]u8 = undefined;
    var w = stderr.writer(&buf);
    w.interface.print("\n\x1b[31mRoc crashed:\x1b[0m {s}\n", .{message}) catch {};
    w.interface.flush() catch {};
    std.process.exit(1);
}

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__main(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

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
    // Parse --test or --test-verbose argument
    var test_spec: ?[]const u8 = null;
    var test_verbose: bool = false;
    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    const stderr_file: std.fs.File = .stderr();
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--test-verbose")) {
            if (i + 1 < arg_count) {
                i += 1;
                test_spec = std.mem.span(argv[i]);
                test_verbose = true;
            } else {
                stderr_file.writeAll("Error: --test-verbose requires a spec argument\n") catch {};
                return 1;
            }
        } else if (std.mem.eql(u8, arg, "--test")) {
            if (i + 1 < arg_count) {
                i += 1;
                test_spec = std.mem.span(argv[i]);
            } else {
                stderr_file.writeAll("Error: --test requires a spec argument\n") catch {};
                return 1;
            }
        } else if (arg.len >= 2 and arg[0] == '-' and arg[1] == '-') {
            stderr_file.writeAll("Error: unknown flag '") catch {};
            stderr_file.writeAll(arg) catch {};
            stderr_file.writeAll("'\n") catch {};
            stderr_file.writeAll("Usage: <app> [--test <spec>] [--test-verbose <spec>]\n") catch {};
            return 1;
        }
    }

    const exit_code = platform_main(test_spec, test_verbose) catch |err| {
        stderr_file.writeAll("HOST ERROR: ") catch {};
        stderr_file.writeAll(@errorName(err)) catch {};
        stderr_file.writeAll("\n") catch {};
        return 1;
    };
    return exit_code;
}

// Use the actual RocStr from builtins instead of defining our own
const RocStr = builtins.str.RocStr;

/// Hosted function: Stderr.line! (index 0 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStderrLine(ops: *builtins.host_abi.RocOps, _: *anyopaque, args: *const extern struct { str: RocStr }) callconv(.c) void {
    const message = args.str.asSlice();

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: verify output matches expected
    if (host.test_state.enabled) {
        const stderr_file: std.fs.File = .stderr();
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stderr_expect and std.mem.eql(u8, entry.value, message)) {
                host.test_state.current_index += 1;
                if (host.test_state.verbose) {
                    stderr_file.writeAll("[OK] stderr: \"") catch {};
                    stderr_file.writeAll(message) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                }
                return; // Match!
            }
            // Mismatch - must allocate a copy of the message since the RocStr may be freed
            const actual_copy = host.gpa.allocator().dupe(u8, message) catch "";
            // Free previous actual_value if any (to avoid leak on multiple failures)
            if (host.test_state.failure_info) |info| {
                if (info.actual_value.len > 0) {
                    host.gpa.allocator().free(info.actual_value);
                }
            }
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = entry.effect_type,
                .expected_value = entry.value,
                .actual_type = .stderr_expect,
                .actual_value = actual_copy,
                .spec_line = entry.spec_line,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stderr: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (expected ") catch {};
                stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                stderr_file.writeAll(": \"") catch {};
                stderr_file.writeAll(entry.value) catch {};
                stderr_file.writeAll("\")\n") catch {};
            }
        } else {
            // Extra output not in spec - must allocate a copy of the message
            const actual_copy = host.gpa.allocator().dupe(u8, message) catch "";
            // Free previous actual_value if any (to avoid leak on multiple failures)
            if (host.test_state.failure_info) |info| {
                if (info.actual_value.len > 0) {
                    host.gpa.allocator().free(info.actual_value);
                }
            }
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = .stderr_expect, // We expected nothing
                .expected_value = "",
                .actual_type = .stderr_expect,
                .actual_value = actual_copy,
                .spec_line = 0,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stderr: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (unexpected - no more expected operations)\n") catch {};
            }
        }
        return;
    }

    // Normal mode: write to stderr
    const stderr: std.fs.File = .stderr();
    stderr.writeAll(message) catch {};
    stderr.writeAll("\n") catch {};
}

/// Hosted function: Stdin.line! (index 1 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns Str and takes {} as argument
fn hostedStdinLine(ops: *builtins.host_abi.RocOps, result: *RocStr, _: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: consume next stdin_input entry from spec
    if (host.test_state.enabled) {
        const stderr_file: std.fs.File = .stderr();
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stdin_input) {
                host.test_state.current_index += 1;
                result.* = RocStr.fromSlice(entry.value, ops);
                if (host.test_state.verbose) {
                    stderr_file.writeAll("[OK] stdin: \"") catch {};
                    stderr_file.writeAll(entry.value) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                }
                return;
            }
            // Wrong type - expected stdin but spec has output
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = entry.effect_type,
                .expected_value = entry.value,
                .actual_type = .stdin_input,
                .actual_value = "(stdin read)",
                .spec_line = entry.spec_line,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdin read (expected ") catch {};
                stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                stderr_file.writeAll(": \"") catch {};
                stderr_file.writeAll(entry.value) catch {};
                stderr_file.writeAll("\")\n") catch {};
            }
        } else {
            // Ran out of entries - app tried to read more stdin than provided
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = .stdin_input,
                .expected_value = "",
                .actual_type = .stdin_input,
                .actual_value = "(stdin read)",
                .spec_line = 0,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdin read (unexpected - no more expected operations)\n") catch {};
            }
        }
        result.* = RocStr.empty();
        return;
    }

    // Normal mode: Read a line from stdin
    var buffer: [4096]u8 = undefined;
    const stdin_file: std.fs.File = .stdin();
    const bytes_read = stdin_file.read(&buffer) catch {
        // Return empty string on error
        result.* = RocStr.empty();
        return;
    };

    // Handle EOF (no bytes read)
    if (bytes_read == 0) {
        result.* = RocStr.empty();
        return;
    }

    // Find newline and trim it (handle both \n and \r\n)
    const line_with_newline = buffer[0..bytes_read];
    var line = if (std.mem.indexOfScalar(u8, line_with_newline, '\n')) |newline_idx|
        line_with_newline[0..newline_idx]
    else
        line_with_newline;

    // Also trim trailing \r for Windows line endings
    if (line.len > 0 and line[line.len - 1] == '\r') {
        line = line[0 .. line.len - 1];
    }

    // Create RocStr from the read line and return it
    // RocStr.fromSlice handles allocation internally (either inline for small strings
    // or via roc_alloc for big strings with proper refcount tracking)
    result.* = RocStr.fromSlice(line, ops);
}

/// Hosted function: Stdout.line! (index 2 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStdoutLine(ops: *builtins.host_abi.RocOps, _: *anyopaque, args: *const extern struct { str: RocStr }) callconv(.c) void {
    const message = args.str.asSlice();

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: verify output matches expected
    if (host.test_state.enabled) {
        const stderr_file: std.fs.File = .stderr();
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stdout_expect and std.mem.eql(u8, entry.value, message)) {
                host.test_state.current_index += 1;
                if (host.test_state.verbose) {
                    stderr_file.writeAll("[OK] stdout: \"") catch {};
                    stderr_file.writeAll(message) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                }
                return; // Match!
            }
            // Mismatch - must allocate a copy of the message since the RocStr may be freed
            const actual_copy = host.gpa.allocator().dupe(u8, message) catch "";
            // Free previous actual_value if any (to avoid leak on multiple failures)
            if (host.test_state.failure_info) |info| {
                if (info.actual_value.len > 0) {
                    host.gpa.allocator().free(info.actual_value);
                }
            }
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = entry.effect_type,
                .expected_value = entry.value,
                .actual_type = .stdout_expect,
                .actual_value = actual_copy,
                .spec_line = entry.spec_line,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdout: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (expected ") catch {};
                stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                stderr_file.writeAll(": \"") catch {};
                stderr_file.writeAll(entry.value) catch {};
                stderr_file.writeAll("\")\n") catch {};
            }
        } else {
            // Extra output not in spec - must allocate a copy of the message
            const actual_copy = host.gpa.allocator().dupe(u8, message) catch "";
            // Free previous actual_value if any (to avoid leak on multiple failures)
            if (host.test_state.failure_info) |info| {
                if (info.actual_value.len > 0) {
                    host.gpa.allocator().free(info.actual_value);
                }
            }
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = .stdout_expect, // We expected nothing
                .expected_value = "",
                .actual_type = .stdout_expect,
                .actual_value = actual_copy,
                .spec_line = 0,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdout: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (unexpected - no more expected operations)\n") catch {};
            }
        }
        return;
    }

    // Normal mode: write to stdout
    const stdout: std.fs.File = .stdout();
    stdout.writeAll(message) catch {};
    stdout.writeAll("\n") catch {};
}

/// Hosted function: Builder.print_value! (index 0 - sorted alphabetically: "Builder.print_value!" comes before "Stderr.line!")
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Builder as argument
const BuilderArgs = extern struct {
    count: u64,
    value: RocStr,
};

fn hostedBuilderPrintValue(ops: *builtins.host_abi.RocOps, _: *anyopaque, args: *const BuilderArgs) callconv(.c) void {
    const value_slice = args.value.asSlice();

    // Format the output messages
    var buf: [256]u8 = undefined;
    const count_str = std.fmt.bufPrint(&buf, "{d}", .{args.count}) catch "?";

    // Use hostedStdoutLine to respect test mode tracking
    // Create temporary RocStr instances for each line
    var empty_ret: u8 = 0;
    var line1 = RocStr.fromSlice("SUCCESS: Builder.print_value! called via static dispatch!", ops);
    defer line1.decref(ops);
    hostedStdoutLine(ops, @ptrCast(&empty_ret), @ptrCast(&line1));

    var line2_buf: [256]u8 = undefined;
    const line2_str = std.fmt.bufPrint(&line2_buf, "  value: {s}", .{value_slice}) catch "  value: ?";
    var line2 = RocStr.fromSlice(line2_str, ops);
    defer line2.decref(ops);
    hostedStdoutLine(ops, @ptrCast(&empty_ret), @ptrCast(&line2));

    var line3_buf: [256]u8 = undefined;
    const line3_str = std.fmt.bufPrint(&line3_buf, "  count: {s}", .{count_str}) catch "  count: ?";
    var line3 = RocStr.fromSlice(line3_str, ops);
    defer line3.decref(ops);
    hostedStdoutLine(ops, @ptrCast(&empty_ret), @ptrCast(&line3));
}

/// Hosted function: Host.get_greeting! (index 1 - sorted alphabetically)
/// This tests hosted effects on opaque types with data (not just []).
/// Takes Host { name: Str } as first argument, returns Str
fn hostedHostGetGreeting(ops: *builtins.host_abi.RocOps, ret: *RocStr, args: *const extern struct { name: RocStr }) callconv(.c) void {
    const name_slice = args.name.asSlice();

    // Create the result string: "Hello, <name>!"
    var buf: [256]u8 = undefined;
    const result_str = std.fmt.bufPrint(&buf, "Hello, {s}!", .{name_slice}) catch "Hello!";
    ret.* = RocStr.fromSlice(result_str, ops);
}

/// Array of hosted function pointers, sorted alphabetically by fully-qualified name
/// These correspond to the hosted functions defined in Stderr, Stdin, Stdout, Builder, and Host Type Modules
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{
    builtins.host_abi.hostedFn(&hostedBuilderPrintValue), // Builder.print_value! (index 0)
    builtins.host_abi.hostedFn(&hostedHostGetGreeting), // Host.get_greeting! (index 1)
    builtins.host_abi.hostedFn(&hostedStderrLine), // Stderr.line! (index 2)
    builtins.host_abi.hostedFn(&hostedStdinLine), // Stdin.line! (index 3)
    builtins.host_abi.hostedFn(&hostedStdoutLine), // Stdout.line! (index 4)
};

/// Platform host entrypoint
fn platform_main(test_spec: ?[]const u8, test_verbose: bool) !c_int {
    // Install signal handlers for stack overflow, access violations, and division by zero
    // This allows us to display helpful error messages instead of crashing
    _ = builtins.handlers.install(handleRocStackOverflow, handleRocAccessViolation, handleRocArithmeticError);

    var host_env = HostEnv{
        .gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){},
        .test_state = TestState.init(),
    };

    // Parse test spec if provided
    if (test_spec) |spec| {
        host_env.test_state.entries = try parseTestSpec(host_env.gpa.allocator(), spec);
        host_env.test_state.enabled = true;
        host_env.test_state.verbose = test_verbose;
    }

    defer {
        // Free duplicated actual_value if allocated (on test failure)
        if (host_env.test_state.failure_info) |info| {
            if (info.actual_value.len > 0) {
                host_env.gpa.allocator().free(info.actual_value);
            }
        }

        // Free test entries if allocated (including unescaped value strings)
        if (host_env.test_state.entries.len > 0) {
            for (host_env.test_state.entries) |entry| {
                if (entry.value.len > 0) {
                    host_env.gpa.allocator().free(entry.value);
                }
            }
            host_env.gpa.allocator().free(host_env.test_state.entries);
        }

        // Free any remaining Roc allocations to prevent false leak reports
        // This handles cases where Roc's normal cleanup doesn't complete
        // (e.g., test failure mid-execution, early return, etc.)
        const allocator = host_env.gpa.allocator();
        const remaining_count = host_env.roc_allocations.items.len;
        const test_passed = !host_env.test_state.failed and
            host_env.test_state.current_index == host_env.test_state.entries.len;

        // Only report remaining allocations if test passed (otherwise it's expected
        // that cleanup may be incomplete due to test failure)
        if (remaining_count > 0 and test_passed) {
            const stderr_file: std.fs.File = .stderr();
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf,
                \\[Roc Memory Info] {d} allocation(s) not freed by Roc runtime.
                \\  This is likely internal to Roc's builtins or refcounting implementation,
                \\  not a bug in the application code. Cleaning up {d} allocations...
                \\
            , .{ remaining_count, remaining_count }) catch "";
            stderr_file.writeAll(msg) catch {};
        }

        for (host_env.roc_allocations.items) |alloc| {
            const slice = alloc.ptr[0..alloc.total_size];
            allocator.rawFree(slice, alloc.alignment, @returnAddress());
        }
        // Free the tracking list itself
        host_env.roc_allocations.deinit(allocator);

        const leaked = host_env.gpa.deinit();
        if (leaked == .leak) {
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll(
                \\
                \\[Roc Memory Info] Additional memory leak detected by GPA.
                \\  This indicates memory allocated outside of rocAllocFn was not freed.
                \\  This is internal to Roc's compiler/runtime, not application code.
                \\
            ) catch {};
        }
    }

    // Create the RocOps struct
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{
            .count = hosted_function_ptrs.len,
            .fns = @constCast(&hosted_function_ptrs),
        },
    };

    // Call the app's main! entrypoint
    var ret: [0]u8 = undefined; // Result is {} which is zero-sized
    var args: [0]u8 = undefined;
    // Note: although this is a function with no args and a zero-sized return value,
    // we can't currently pass null pointers for either of these because Roc will
    // currently dereference both of these eagerly even though it won't use either,
    // causing a segfault if you pass null. This should be changed! Dereferencing
    // garbage memory is obviously pointless, and there's no reason we should do it.
    roc__main(&roc_ops, @as(*anyopaque, @ptrCast(&ret)), @as(*anyopaque, @ptrCast(&args)));

    // Check test results if in test mode
    if (host_env.test_state.enabled) {
        // Check if test failed or not all entries were consumed
        if (host_env.test_state.failed or host_env.test_state.current_index != host_env.test_state.entries.len) {
            const stderr_file: std.fs.File = .stderr();

            // Print failure info
            if (host_env.test_state.failure_info) |info| {
                if (info.spec_line == 0) {
                    // Extra/unexpected output
                    stderr_file.writeAll("TEST FAILED: Unexpected ") catch {};
                    stderr_file.writeAll(effectTypeName(info.actual_type)) catch {};
                    stderr_file.writeAll(" output: \"") catch {};
                    stderr_file.writeAll(info.actual_value) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                } else {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at spec line {d}:\n  Expected: {s} \"{s}\"\n  Got:      {s} \"{s}\"\n", .{
                        info.spec_line,
                        effectTypeName(info.expected_type),
                        info.expected_value,
                        effectTypeName(info.actual_type),
                        info.actual_value,
                    }) catch "TEST FAILED\n";
                    stderr_file.writeAll(msg) catch {};
                }
            } else if (host_env.test_state.current_index < host_env.test_state.entries.len) {
                // Not all entries were consumed - list what's remaining
                const remaining = host_env.test_state.entries.len - host_env.test_state.current_index;
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "TEST FAILED: {d} expected IO operation(s) not performed:\n", .{remaining}) catch "TEST FAILED: expected IO operations not performed\n";
                stderr_file.writeAll(msg) catch {};

                // List up to 5 unconsumed entries
                const max_to_show: usize = 5;
                var shown: usize = 0;
                for (host_env.test_state.entries[host_env.test_state.current_index..]) |entry| {
                    if (shown >= max_to_show) {
                        stderr_file.writeAll("  ...\n") catch {};
                        break;
                    }
                    stderr_file.writeAll("  - ") catch {};
                    stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                    stderr_file.writeAll(": \"") catch {};
                    stderr_file.writeAll(entry.value) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                    shown += 1;
                }
            } else {
                stderr_file.writeAll("TEST FAILED\n") catch {};
            }

            return 1;
        }
    }

    return 0;
}

fn effectTypeName(effect_type: EffectType) []const u8 {
    return switch (effect_type) {
        .stdin_input => "stdin",
        .stdout_expect => "stdout",
        .stderr_expect => "stderr",
    };
}

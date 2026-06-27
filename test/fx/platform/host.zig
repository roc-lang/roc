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
const Allocator = std.mem.Allocator;
const shim_io = @import("shim_io");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
const build_options = @import("build_options");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .wasi) std.posix else undefined;

const trace_refcount = build_options.trace_refcount;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    // Zig 0.16's std.debug.SelfInfo (Windows) references ntdll.LdrRegisterDllNotification,
    // which isn't available when the host static archive is linked into a roc-compiled
    // program. Disabling stack tracing avoids pulling that code in.
    .allow_stack_tracing = false,
};

/// Override the default panic handler to avoid secondary crashes in stack trace generation
pub const panic = std.debug.FullPanic(panicImpl);

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    std.debug.print("{s}", .{"\n=== PANIC (no stack trace) ===\n"});
    std.debug.print("{s}", .{msg});
    if (addr) |a| {
        var buf: [32]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, " at address 0x{x}\n", .{a}) catch "";
        std.debug.print("{s}", .{hex});
    } else {
        std.debug.print("{s}", .{"\n"});
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
            extern "kernel32" fn TerminateProcess(hProcess: HANDLE, uExitCode: c_uint) callconv(.winapi) i32;
            extern "kernel32" fn GetCurrentProcess() callconv(.winapi) HANDLE;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len, &bytes_written, null);
        // Use TerminateProcess instead of ExitProcess: after a stack overflow the
        // stack is blown and ExitProcess's DLL cleanup can trigger a secondary crash.
        _ = kernel32.TerminateProcess(kernel32.GetCurrentProcess(), 134);
        @trap();
    } else if (comptime builtin.os.tag != .wasi) {
        std.debug.print("{s}", .{STACK_OVERFLOW_MESSAGE});
        std.process.exit(134);
    } else {
        std.process.exit(134);
    }
}

/// Callback for access violation in a Roc program
fn handleRocAccessViolation(fault_addr: usize, _: base.signal_handler.AccessViolationContext) noreturn {
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
        const addr_str = base.signal_handler.formatHex(fault_addr, &addr_buf);

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
        std.debug.print("{s}", .{msg});

        var addr_buf: [18]u8 = undefined;
        const addr_str = base.signal_handler.formatHex(fault_addr, &addr_buf);
        std.debug.print("{s}", .{addr_str});
        std.debug.print("{s}", .{"\n\n"});
        std.process.exit(139);
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
        std.debug.print("{s}", .{DIVISION_BY_ZERO_MESSAGE});
        std.process.exit(136); // 128 + 8 (SIGFPE)
    } else {
        std.process.exit(136);
    }
}

const HostSelfTest = enum {
    none,
    stack_overflow,
    division_by_zero,
};

fn installRuntimeSignalHandlers() void {
    _ = base.signal_handler.installForCurrentThread(.{
        .stack_overflow = handleRocStackOverflow,
        .access_violation = handleRocAccessViolation,
        .arithmetic_error = handleRocArithmeticError,
    });
}

fn triggerSelfTest(mode: HostSelfTest) noreturn {
    installRuntimeSignalHandlers();

    switch (mode) {
        .stack_overflow => triggerSelfTestStackOverflow(),
        .division_by_zero => triggerSelfTestDivisionByZero(),
        .none => unreachable,
    }
}

fn triggerSelfTestStackOverflow() noreturn {
    _ = selfTestStackOverflow(0);
    unreachable;
}

fn selfTestStackOverflow(depth: usize) usize {
    var padding: [4096]u8 = undefined;
    @memset(&padding, @as(u8, @truncate(depth)));
    std.mem.doNotOptimizeAway(&padding);

    return selfTestStackOverflow(depth +% 1) + padding[0];
}

fn triggerSelfTestDivisionByZero() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const ULONG_PTR = usize;
        const EXCEPTION_INT_DIVIDE_BY_ZERO: DWORD = 0xC0000094;

        const kernel32 = struct {
            extern "kernel32" fn RaiseException(dwExceptionCode: DWORD, dwExceptionFlags: DWORD, nNumberOfArguments: DWORD, lpArguments: ?[*]const ULONG_PTR) callconv(.winapi) noreturn;
        };

        kernel32.RaiseException(EXCEPTION_INT_DIVIDE_BY_ZERO, 0, 0, null);
    } else if (comptime builtin.os.tag != .wasi) {
        posix.raise(posix.SIG.FPE) catch {};
        std.process.exit(136);
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
    if (std.mem.findScalar(u8, input, '\\') == null) {
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
            std.debug.print("{s}", .{"Error: Invalid spec segment '"});
            std.debug.print("{s}", .{segment});
            std.debug.print("{s}", .{"' - must start with 0<, 1>, or 2>\n"});
            return ParseError.InvalidSpecFormat;
        }

        const effect_type: EffectType = blk: {
            if (segment[0] == '0' and segment[1] == '<') break :blk .stdin_input;
            if (segment[0] == '1' and segment[1] == '>') break :blk .stdout_expect;
            if (segment[0] == '2' and segment[1] == '>') break :blk .stderr_expect;
            // Invalid pattern - report error
            std.debug.print("{s}", .{"Error: Invalid spec segment '"});
            std.debug.print("{s}", .{segment});
            std.debug.print("{s}", .{"' - must start with 0<, 1>, or 2>\n"});
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

/// Host environment - contains DebugAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true, .thread_safe = false }),
    test_state: TestState,
    std_io: std.Io,
    /// Track Roc allocations for cleanup on test failure
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty,
    /// Allocation counters for diagnostics
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    /// Set when an inline `expect` fails. Inline expect failures are
    /// non-fatal but must cause the process to exit with a non-zero status.
    inline_expect_failed: bool = false,
};

/// File-level pointer to the host's `RocOps`, used by hosted functions that
/// have no leading `*RocOps` parameter (and therefore cannot reach state via
/// `ops.env`). Set wherever `RocOps.env` is set.
var g_roc_ops: ?*builtins.host_abi.RocOps = null;

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(ops: *builtins.host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(ops.env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("rocAllocFn: env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    // The allocation must be at least 8-byte aligned because:
    // 1. The refcount (isize/usize) is stored before the data and needs proper alignment
    // 2. The builtins code casts data pointers to [*]isize for refcount access
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const total_size = length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "\x1b[31mHost error:\x1b[0m allocation failed for size={d} align={d}\n", .{
            total_size,
            alignment,
        }) catch "\x1b[31mHost error:\x1b[0m allocation failed, out of memory\n";
        std.debug.print("{s}", .{msg});
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

    // Pointer to the user data (after the size metadata)
    const answer: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    // Debug check: verify the returned pointer is also properly aligned
    const answer_addr = @intFromPtr(answer);
    if (answer_addr % alignment != 0) {
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
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ answer_addr, length, alignment });
    }

    return answer;
}

/// Roc deallocation function with size-tracking metadata
fn rocDeallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(ops.env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocDeallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    // Use same minimum alignment as alloc
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    if (trace_refcount) {
        std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{
            @intFromPtr(ptr),
            alignment,
            total_size,
            size_storage_bytes,
        });
    }

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

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
fn rocReallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(ops.env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocReallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    // Use same minimum alignment as alloc
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = new_length + size_storage_bytes;

    // Free old memory and allocate new with proper alignment
    // This is necessary because Zig's realloc infers alignment from slice type ([]u8 = alignment 1)
    // which could cause the new allocation to be misaligned
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];

    // Allocate new memory with proper alignment
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        std.debug.print("{s}", .{"\x1b[31mHost error:\x1b[0m reallocation failed, out of memory\n"});
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

    // Pointer to the user data (after the size metadata)
    const answer: *anyopaque = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(old_base_ptr) + size_storage_bytes, @intFromPtr(answer), new_length });
    }

    return answer;
}

/// Roc debug function
fn rocDbgFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    host.inline_expect_failed = true;
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
    std.process.exit(1);
}

/// Roc crashed function
fn rocCrashedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "\n\x1b[31mRoc crashed:\x1b[0m {s}\n", .{message}) catch "\n\x1b[31mRoc crashed\x1b[0m\n";
    std.debug.print("{s}", .{msg});
    std.process.exit(1);
}

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc_main() callconv(.c) void;

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
    var self_test: HostSelfTest = .none;
    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--test-verbose")) {
            if (i + 1 < arg_count) {
                i += 1;
                test_spec = std.mem.span(argv[i]);
                test_verbose = true;
            } else {
                std.debug.print("{s}", .{"Error: --test-verbose requires a spec argument\n"});
                return 1;
            }
        } else if (std.mem.eql(u8, arg, "--test")) {
            if (i + 1 < arg_count) {
                i += 1;
                test_spec = std.mem.span(argv[i]);
            } else {
                std.debug.print("{s}", .{"Error: --test requires a spec argument\n"});
                return 1;
            }
        } else if (std.mem.eql(u8, arg, "--host-test-stack-overflow")) {
            self_test = .stack_overflow;
        } else if (std.mem.eql(u8, arg, "--host-test-division-by-zero")) {
            self_test = .division_by_zero;
        } else if (arg.len >= 2 and arg[0] == '-' and arg[1] == '-') {
            std.debug.print("{s}", .{"Error: unknown flag '"});
            std.debug.print("{s}", .{arg});
            std.debug.print("{s}", .{"'\n"});
            std.debug.print("{s}", .{"Usage: <app> [--test <spec>] [--test-verbose <spec>] [--host-test-stack-overflow] [--host-test-division-by-zero]\n"});
            return 1;
        }
    }

    if (self_test != .none) {
        triggerSelfTest(self_test);
    }

    const exit_code = platform_main(test_spec, test_verbose) catch |err| {
        std.debug.print("{s}", .{"HOST ERROR: "});
        std.debug.print("{s}", .{@errorName(err)});
        std.debug.print("{s}", .{"\n"});
        return 1;
    };
    return exit_code;
}

// Use the actual RocStr from builtins instead of defining our own
const RocStr = builtins.str.RocStr;

/// Hosted function: Stderr.line! (index 0 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStderrLine(str: RocStr) callconv(.c) void {
    const ops = g_roc_ops.?;
    const message = str.asSlice();
    defer str.decref(ops);

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: verify output matches expected
    if (host.test_state.enabled) {
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stderr_expect and std.mem.eql(u8, entry.value, message)) {
                host.test_state.current_index += 1;
                if (host.test_state.verbose) {
                    std.debug.print("{s}", .{"[OK] stderr: \""});
                    std.debug.print("{s}", .{message});
                    std.debug.print("{s}", .{"\"\n"});
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
                std.debug.print("{s}", .{"[FAIL] stderr: \""});
                std.debug.print("{s}", .{message});
                std.debug.print("{s}", .{"\" (expected "});
                std.debug.print("{s}", .{effectTypeName(entry.effect_type)});
                std.debug.print("{s}", .{": \""});
                std.debug.print("{s}", .{entry.value});
                std.debug.print("{s}", .{"\")\n"});
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
                std.debug.print("{s}", .{"[FAIL] stderr: \""});
                std.debug.print("{s}", .{message});
                std.debug.print("{s}", .{"\" (unexpected - no more expected operations)\n"});
            }
        }
        return;
    }

    // Normal mode: write to stderr
    std.debug.print("{s}", .{message});
    std.debug.print("{s}", .{"\n"});
}

/// Hosted function: Stdin.line! (index 1 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns Str and takes {} as argument
fn hostedStdinLine() callconv(.c) RocStr {
    const ops = g_roc_ops.?;
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: consume next stdin_input entry from spec
    if (host.test_state.enabled) {
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stdin_input) {
                host.test_state.current_index += 1;
                const line = RocStr.fromSlice(entry.value, ops);
                if (host.test_state.verbose) {
                    std.debug.print("{s}", .{"[OK] stdin: \""});
                    std.debug.print("{s}", .{entry.value});
                    std.debug.print("{s}", .{"\"\n"});
                }
                return line;
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
                std.debug.print("{s}", .{"[FAIL] stdin read (expected "});
                std.debug.print("{s}", .{effectTypeName(entry.effect_type)});
                std.debug.print("{s}", .{": \""});
                std.debug.print("{s}", .{entry.value});
                std.debug.print("{s}", .{"\")\n"});
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
                std.debug.print("{s}", .{"[FAIL] stdin read (unexpected - no more expected operations)\n"});
            }
        }
        return RocStr.empty();
    }

    // Normal mode: Read a line from stdin
    var buffer: [4096]u8 = undefined;
    const bytes_read = std.Io.File.stdin().readStreaming(host.std_io, &.{&buffer}) catch {
        // Return empty string on error
        return RocStr.empty();
    };

    // Handle EOF (no bytes read)
    if (bytes_read == 0) {
        return RocStr.empty();
    }

    // Find newline and trim it (handle both \n and \r\n)
    const line_with_newline = buffer[0..bytes_read];
    var line = if (std.mem.findScalar(u8, line_with_newline, '\n')) |newline_idx|
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
    return RocStr.fromSlice(line, ops);
}

/// Hosted function: Stdout.line! (index 2 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStdoutLine(str: RocStr) callconv(.c) void {
    const ops = g_roc_ops.?;
    const message = str.asSlice();
    defer str.decref(ops);

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: verify output matches expected
    if (host.test_state.enabled) {
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stdout_expect and std.mem.eql(u8, entry.value, message)) {
                host.test_state.current_index += 1;
                if (host.test_state.verbose) {
                    std.debug.print("{s}", .{"[OK] stdout: \""});
                    std.debug.print("{s}", .{message});
                    std.debug.print("{s}", .{"\"\n"});
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
                std.debug.print("{s}", .{"[FAIL] stdout: \""});
                std.debug.print("{s}", .{message});
                std.debug.print("{s}", .{"\" (expected "});
                std.debug.print("{s}", .{effectTypeName(entry.effect_type)});
                std.debug.print("{s}", .{": \""});
                std.debug.print("{s}", .{entry.value});
                std.debug.print("{s}", .{"\")\n"});
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
                std.debug.print("{s}", .{"[FAIL] stdout: \""});
                std.debug.print("{s}", .{message});
                std.debug.print("{s}", .{"\" (unexpected - no more expected operations)\n"});
            }
        }
        return;
    }

    // Normal mode: write to stdout
    std.Io.File.stdout().writeStreamingAll(host.std_io, message) catch {};
    std.Io.File.stdout().writeStreamingAll(host.std_io, "\n") catch {};
}

/// Hosted function: Builder.print_value! (index 0 - sorted alphabetically: "Builder.print_value!" comes before "Stderr.line!")
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Builder as argument
// Mirrors `Builder := { value : Str, count : U64, _ : {} }`. A nominal record
// keeps its declared field order only when it includes an unnamed `_` field;
// Builder does, so the host struct lists `value` before `count` to match the Roc
// declaration. Without the `_` it would lay out structurally (by descending
// alignment, hoisting `count` ahead of `value`).
const BuilderArgs = extern struct {
    value: RocStr,
    count: u64,
};

fn hostedBuilderPrintValue(builder: BuilderArgs) callconv(.c) void {
    const ops = g_roc_ops.?;
    const value_slice = builder.value.asSlice();

    // Format the output messages
    var buf: [256]u8 = undefined;
    const count_str = std.fmt.bufPrint(&buf, "{d}", .{builder.count}) catch "?";

    // Use hostedStdoutLine to respect test mode tracking
    // Create temporary RocStr instances for each line
    const line1 = RocStr.fromSlice("SUCCESS: Builder.print_value! called via static dispatch!", ops);
    hostedStdoutLine(line1);

    var line2_buf: [256]u8 = undefined;
    const line2_str = std.fmt.bufPrint(&line2_buf, "  value: {s}", .{value_slice}) catch "  value: ?";
    const line2 = RocStr.fromSlice(line2_str, ops);
    hostedStdoutLine(line2);

    var line3_buf: [256]u8 = undefined;
    const line3_str = std.fmt.bufPrint(&line3_buf, "  count: {s}", .{count_str}) catch "  count: ?";
    const line3 = RocStr.fromSlice(line3_str, ops);
    hostedStdoutLine(line3);
}

/// Hosted function: Host.get_greeting! (index 5 - sorted alphabetically)
/// This tests hosted effects on opaque types with data (not just []).
/// Takes Host { name: Str } as first argument, returns Str
const HostRecord = extern struct { name: RocStr };

fn hostedHostGetGreeting(host: HostRecord) callconv(.c) RocStr {
    const ops = g_roc_ops.?;
    const name_slice = host.name.asSlice();
    defer host.name.decref(ops);

    // Create the result string: "Hello, <name>!"
    var buf: [256]u8 = undefined;
    const result_str = std.fmt.bufPrint(&buf, "Hello, {s}!", .{name_slice}) catch "Hello!";
    return RocStr.fromSlice(result_str, ops);
}

/// Hosted function: Padded.check! — end-to-end host-interop check for a nominal
/// record with an unnamed padding field. The Roc type is
/// `Padded := { z : U32, _ : U32, a : U32 }`, whose runtime layout must match
/// this extern struct (declared order, with four reserved padding bytes): z@0,
/// _pad@4, a@8. We read `z` and `a` and return "<z*100 + a>", so a layout
/// mismatch (wrong field order or dropped padding) yields the wrong number.
const PaddedArgs = extern struct { z: u32, _pad: u32, a: u32 };

comptime {
    // Lock in the byte offsets this test asserts against the Roc layout.
    std.debug.assert(@offsetOf(PaddedArgs, "z") == 0);
    std.debug.assert(@offsetOf(PaddedArgs, "a") == 8);
    std.debug.assert(@sizeOf(PaddedArgs) == 12);
}

fn hostedPaddedCheck(padded: PaddedArgs) callconv(.c) RocStr {
    const ops = g_roc_ops.?;
    const result: u64 = @as(u64, padded.z) * 100 + padded.a;
    var buf: [32]u8 = undefined;
    const result_str = std.fmt.bufPrint(&buf, "{d}", .{result}) catch "err";
    return RocStr.fromSlice(result_str, ops);
}

const BoxedHostDropCounts = struct {
    primitive: usize = 0,
    nested_record: usize = 0,
    nested_record_str_releases: usize = 0,
    recursive_tree: usize = 0,
    recursive_tree_child_box_releases: usize = 0,
    boxed_capture: usize = 0,
};

var boxed_host_drop_counts: BoxedHostDropCounts = .{};
var stored_boxed_callable: ?[*]u8 = null;

const AddCapture = extern struct {
    amount: i64,
};

const NestedRecordCapture = extern struct {
    inner: extern struct {
        label: RocStr,
        base: i64,
    },
    adjustment: i64,
};

const HostTreeNode = extern struct {
    left: ?[*]u8,
    right: ?[*]u8,
};

const HostTree = extern struct {
    payload: extern union {
        leaf: i64,
        node: HostTreeNode,
    },
    discriminant: u8,
    padding: [7]u8,
};

const TreeCapture = extern struct {
    tree: HostTree,
};

const BoxedCallableCapture = extern struct {
    inner: ?[*]u8,
    bonus: i64,
};

fn capturePtrAs(comptime T: type, capture_ptr: ?[*]u8) *T {
    return @ptrCast(@alignCast(capture_ptr orelse unreachable));
}

fn writeErasedCallable(
    comptime Capture: type,
    ret: *?[*]u8,
    callable_fn_ptr: builtins.erased_callable.CallableFnPtr,
    on_drop: ?builtins.erased_callable.OnDropFn,
    capture: Capture,
    ops: *builtins.host_abi.RocOps,
) void {
    comptime {
        if (@alignOf(Capture) > builtins.erased_callable.capture_alignment) {
            @compileError("boxed erased callable host capture alignment exceeds Roc erased callable ABI alignment");
        }
    }
    const payload = builtins.erased_callable.allocate(callable_fn_ptr, on_drop, @sizeOf(Capture), ops);
    capturePtrAs(Capture, builtins.erased_callable.capturePtr(payload)).* = capture;
    ret.* = payload;
}

const I64ToI64Args = extern struct {
    arg0: i64,
};

fn readI64ToI64Arg(args: ?[*]const u8) i64 {
    return @as(*align(1) const I64ToI64Args, @ptrCast(args orelse unreachable)).arg0;
}

fn writeI64Result(ret: ?[*]u8, value: i64) void {
    @as(*align(1) i64, @ptrCast(ret orelse unreachable)).* = value;
}

fn callBoxedI64ToI64(ops: *builtins.host_abi.RocOps, boxed: ?[*]u8, arg0: i64) i64 {
    const payload_ptr = boxed orelse {
        ops.crash("host attempted to call a null boxed erased callable");
        unreachable;
    };
    const payload = builtins.erased_callable.payloadPtr(payload_ptr);
    var call_args = I64ToI64Args{ .arg0 = arg0 };
    var result: i64 = undefined;
    payload.callable_fn_ptr(
        ops,
        @ptrCast(&result),
        @ptrCast(&call_args),
        builtins.erased_callable.capturePtr(payload_ptr),
    );
    return result;
}

fn hostAddCallable(_: *builtins.host_abi.RocOps, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = capturePtrAs(AddCapture, capture_ptr);
    writeI64Result(ret, readI64ToI64Arg(args) + capture.amount);
}

fn hostAddCaptureOnDrop(_: ?[*]u8, _: *builtins.host_abi.RocOps) callconv(.c) void {
    boxed_host_drop_counts.primitive += 1;
}

fn hostedHostBoxedAdd(amount: i64) callconv(.c) ?[*]u8 {
    const ops = g_roc_ops.?;
    var ret: ?[*]u8 = null;
    writeErasedCallable(
        AddCapture,
        &ret,
        @ptrCast(&hostAddCallable),
        &hostAddCaptureOnDrop,
        .{ .amount = amount },
        ops,
    );
    return ret;
}

fn hostNestedRecordCallable(_: *builtins.host_abi.RocOps, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = capturePtrAs(NestedRecordCapture, capture_ptr);
    const x = readI64ToI64Arg(args);
    writeI64Result(ret, x + capture.inner.base + capture.adjustment + @as(i64, @intCast(capture.inner.label.asSlice().len)));
}

fn hostNestedRecordCaptureOnDrop(capture_ptr: ?[*]u8, _: *builtins.host_abi.RocOps) callconv(.c) void {
    // The ops argument is whatever the final release passed; app code under
    // the symbol ABI passes null, so the host uses its own RocOps.
    const ops = g_roc_ops.?;
    const capture = capturePtrAs(NestedRecordCapture, capture_ptr);
    capture.inner.label.decref(ops);
    boxed_host_drop_counts.nested_record_str_releases += 1;
    boxed_host_drop_counts.nested_record += 1;
}

fn hostedHostBoxedNestedRecord(label: RocStr) callconv(.c) ?[*]u8 {
    const ops = g_roc_ops.?;
    const capture_label = label;
    capture_label.incref(1, ops);
    defer label.decref(ops);
    var ret: ?[*]u8 = null;
    writeErasedCallable(
        NestedRecordCapture,
        &ret,
        @ptrCast(&hostNestedRecordCallable),
        &hostNestedRecordCaptureOnDrop,
        .{
            .inner = .{
                .label = capture_label,
                .base = 20,
            },
            .adjustment = 3,
        },
        ops,
    );
    return ret;
}

fn hostTreeCloneBox(tree: *const HostTree, ops: *builtins.host_abi.RocOps) ?[*]u8 {
    const ptr = builtins.utils.allocateWithRefcount(@sizeOf(HostTree), @alignOf(HostTree), true, ops);
    capturePtrAs(HostTree, ptr).* = hostTreeClonePayload(tree, ops);
    return ptr;
}

fn hostTreeClonePayload(tree: *const HostTree, ops: *builtins.host_abi.RocOps) HostTree {
    return switch (tree.discriminant) {
        0 => .{
            .payload = .{ .leaf = tree.payload.leaf },
            .discriminant = 0,
            .padding = [_]u8{0} ** 7,
        },
        1 => .{
            .payload = .{ .node = .{
                .left = hostTreeCloneBox(capturePtrAs(HostTree, tree.payload.node.left), ops),
                .right = hostTreeCloneBox(capturePtrAs(HostTree, tree.payload.node.right), ops),
            } },
            .discriminant = 1,
            .padding = [_]u8{0} ** 7,
        },
        else => blk: {
            ops.crash("host boxed recursive tree capture had invalid discriminant");
            break :blk undefined;
        },
    };
}

fn hostTreeDropPayload(tree_ptr: ?[*]u8, ops: *builtins.host_abi.RocOps) callconv(.c) void {
    const tree = capturePtrAs(HostTree, tree_ptr);
    switch (tree.discriminant) {
        0 => {},
        1 => {
            boxed_host_drop_counts.recursive_tree_child_box_releases += 2;
            builtins.dev_wrappers.roc_builtins_box_decref_with(tree.payload.node.left, @alignOf(HostTree), &hostTreeDropPayload, ops);
            builtins.dev_wrappers.roc_builtins_box_decref_with(tree.payload.node.right, @alignOf(HostTree), &hostTreeDropPayload, ops);
        },
        else => ops.crash("host boxed recursive tree drop had invalid discriminant"),
    }
}

fn hostTreeDropPayloadWithoutReport(tree_ptr: ?[*]u8, ops: *builtins.host_abi.RocOps) callconv(.c) void {
    const tree = capturePtrAs(HostTree, tree_ptr);
    switch (tree.discriminant) {
        0 => {},
        1 => {
            builtins.dev_wrappers.roc_builtins_box_decref_with(tree.payload.node.left, @alignOf(HostTree), &hostTreeDropPayloadWithoutReport, ops);
            builtins.dev_wrappers.roc_builtins_box_decref_with(tree.payload.node.right, @alignOf(HostTree), &hostTreeDropPayloadWithoutReport, ops);
        },
        else => ops.crash("host boxed recursive tree drop had invalid discriminant"),
    }
}

fn hostTreeSum(tree: *const HostTree) i64 {
    return switch (tree.discriminant) {
        0 => tree.payload.leaf,
        1 => blk: {
            const left = capturePtrAs(HostTree, tree.payload.node.left);
            const right = capturePtrAs(HostTree, tree.payload.node.right);
            break :blk hostTreeSum(left) + hostTreeSum(right);
        },
        else => 0,
    };
}

fn hostTreeCallable(_: *builtins.host_abi.RocOps, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = capturePtrAs(TreeCapture, capture_ptr);
    writeI64Result(ret, readI64ToI64Arg(args) + hostTreeSum(&capture.tree));
}

fn hostTreeCaptureOnDrop(capture_ptr: ?[*]u8, _: *builtins.host_abi.RocOps) callconv(.c) void {
    const ops = g_roc_ops.?;
    const capture = capturePtrAs(TreeCapture, capture_ptr);
    hostTreeDropPayload(@ptrCast(&capture.tree), ops);
    boxed_host_drop_counts.recursive_tree += 1;
}

fn hostedHostBoxedRecursiveTree(tree: HostTree) callconv(.c) ?[*]u8 {
    const ops = g_roc_ops.?;
    var tree_local = tree;
    defer hostTreeDropPayloadWithoutReport(@ptrCast(&tree_local), ops);
    var ret: ?[*]u8 = null;
    writeErasedCallable(
        TreeCapture,
        &ret,
        @ptrCast(&hostTreeCallable),
        &hostTreeCaptureOnDrop,
        .{ .tree = hostTreeClonePayload(&tree_local, ops) },
        ops,
    );
    return ret;
}

fn hostBoxedCaptureCallable(ops: *builtins.host_abi.RocOps, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = capturePtrAs(BoxedCallableCapture, capture_ptr);
    const x = readI64ToI64Arg(args);
    writeI64Result(ret, callBoxedI64ToI64(ops, capture.inner, x) + capture.bonus);
}

fn hostBoxedCaptureOnDrop(capture_ptr: ?[*]u8, _: *builtins.host_abi.RocOps) callconv(.c) void {
    const ops = g_roc_ops.?;
    const capture = capturePtrAs(BoxedCallableCapture, capture_ptr);
    builtins.erased_callable.decref(capture.inner, ops);
    boxed_host_drop_counts.boxed_capture += 1;
}

fn hostedHostBoxedWithBoxedCapture(inner: ?[*]u8, bonus: i64) callconv(.c) ?[*]u8 {
    const ops = g_roc_ops.?;
    if (inner) |inner_ptr| {
        builtins.erased_callable.incref(inner_ptr, 1, ops);
    } else {
        ops.crash("host boxed callable capture received null inner callable");
        unreachable;
    }
    defer builtins.erased_callable.decref(inner, ops);

    var ret: ?[*]u8 = null;
    writeErasedCallable(
        BoxedCallableCapture,
        &ret,
        @ptrCast(&hostBoxedCaptureCallable),
        &hostBoxedCaptureOnDrop,
        .{
            .inner = inner,
            .bonus = bonus,
        },
        ops,
    );
    return ret;
}

fn hostedHostCallBoxed(boxed: ?[*]u8, value: i64) callconv(.c) i64 {
    const ops = g_roc_ops.?;
    defer builtins.erased_callable.decref(boxed, ops);
    return callBoxedI64ToI64(ops, boxed, value);
}

fn hostedHostReleaseStoredBoxed() callconv(.c) void {
    const ops = g_roc_ops.?;
    if (stored_boxed_callable) |boxed| {
        builtins.erased_callable.decref(boxed, ops);
        stored_boxed_callable = null;
    }
}

fn hostedHostRoundtripBoxed(boxed: ?[*]u8) callconv(.c) ?[*]u8 {
    const ops = g_roc_ops.?;
    if (boxed) |b| {
        builtins.erased_callable.incref(b, 1, ops);
        builtins.erased_callable.decref(b, ops);
    }
    return boxed;
}

fn hostedHostStoreBoxed(boxed: ?[*]u8) callconv(.c) void {
    const ops = g_roc_ops.?;
    if (stored_boxed_callable) |prev| {
        builtins.erased_callable.decref(prev, ops);
        stored_boxed_callable = null;
    }
    const new_boxed = boxed orelse {
        ops.crash("host attempted to store a null boxed erased callable");
        unreachable;
    };
    builtins.erased_callable.incref(new_boxed, 1, ops);
    stored_boxed_callable = new_boxed;
    builtins.erased_callable.decref(new_boxed, ops);
}

fn hostedHostStoredBoxedCall(value: i64) callconv(.c) i64 {
    const ops = g_roc_ops.?;
    return callBoxedI64ToI64(ops, stored_boxed_callable, value);
}

fn hostedHostBoxedDropReport() callconv(.c) RocStr {
    const ops = g_roc_ops.?;
    var buf: [256]u8 = undefined;
    const report = std.fmt.bufPrint(
        &buf,
        "drops primitive={d} nested_record={d} nested_str={d} recursive_tree={d} tree_child_boxes={d} boxed_capture={d}",
        .{
            boxed_host_drop_counts.primitive,
            boxed_host_drop_counts.nested_record,
            boxed_host_drop_counts.nested_record_str_releases,
            boxed_host_drop_counts.recursive_tree,
            boxed_host_drop_counts.recursive_tree_child_box_releases,
            boxed_host_drop_counts.boxed_capture,
        },
    ) catch "drops unavailable";
    return RocStr.fromSlice(report, ops);
}

fn hostedHostResetBoxedDropReport() callconv(.c) void {
    const ops = g_roc_ops.?;
    if (stored_boxed_callable) |boxed| {
        builtins.erased_callable.decref(boxed, ops);
        stored_boxed_callable = null;
    }
    boxed_host_drop_counts = .{};
}

// The platform's hosted functions, exported under their header symbols, plus
// the fixed runtime symbols every symbol-ABI host defines.
comptime {
    @export(&hostedBuilderPrintValue, .{ .name = "roc_builder_print_value", .visibility = .hidden });
    @export(&hostedHostBoxedAdd, .{ .name = "roc_host_boxed_add", .visibility = .hidden });
    @export(&hostedHostBoxedDropReport, .{ .name = "roc_host_boxed_drop_report", .visibility = .hidden });
    @export(&hostedHostBoxedNestedRecord, .{ .name = "roc_host_boxed_nested_record", .visibility = .hidden });
    @export(&hostedHostBoxedRecursiveTree, .{ .name = "roc_host_boxed_recursive_tree", .visibility = .hidden });
    @export(&hostedHostBoxedWithBoxedCapture, .{ .name = "roc_host_boxed_with_boxed_capture", .visibility = .hidden });
    @export(&hostedHostCallBoxed, .{ .name = "roc_host_call_boxed", .visibility = .hidden });
    @export(&hostedHostGetGreeting, .{ .name = "roc_host_get_greeting", .visibility = .hidden });
    @export(&hostedHostReleaseStoredBoxed, .{ .name = "roc_host_release_stored_boxed", .visibility = .hidden });
    @export(&hostedHostResetBoxedDropReport, .{ .name = "roc_host_reset_boxed_drop_report", .visibility = .hidden });
    @export(&hostedHostRoundtripBoxed, .{ .name = "roc_host_roundtrip_boxed", .visibility = .hidden });
    @export(&hostedHostStoreBoxed, .{ .name = "roc_host_store_boxed", .visibility = .hidden });
    @export(&hostedHostStoredBoxedCall, .{ .name = "roc_host_stored_boxed_call", .visibility = .hidden });
    @export(&hostedPaddedCheck, .{ .name = "roc_padded_check", .visibility = .hidden });
    @export(&hostedStderrLine, .{ .name = "roc_stderr_line", .visibility = .hidden });
    @export(&hostedStdinLine, .{ .name = "roc_stdin_line", .visibility = .hidden });
    @export(&hostedStdoutLine, .{ .name = "roc_stdout_line", .visibility = .hidden });

    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocAllocFn(g_roc_ops.?, length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    rocDeallocFn(g_roc_ops.?, ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocReallocFn(g_roc_ops.?, ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocDbgFn(g_roc_ops.?, bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocExpectFailedFn(g_roc_ops.?, bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocCrashedFn(g_roc_ops.?, bytes, len);
}

/// Platform host entrypoint
fn platform_main(test_spec: ?[]const u8, test_verbose: bool) (Allocator.Error || error{InvalidSpecFormat})!c_int {
    // Install signal handlers for stack overflow, access violations, and division by zero
    // This allows us to display helpful error messages instead of crashing
    installRuntimeSignalHandlers();

    if (trace_refcount) {
        builtins.utils.DebugRefcountTracker.enable();
        defer builtins.utils.DebugRefcountTracker.disable();
    }

    var host_env = HostEnv{
        .gpa = std.heap.DebugAllocator(.{ .safety = true, .thread_safe = false }){},
        .test_state = TestState.init(),
        .std_io = shim_io.io(),
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
            if (trace_refcount) {
                _ = builtins.utils.DebugRefcountTracker.reportLeaks();
            }
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf,
                \\[Roc Memory Info] {d} allocation(s) not freed by Roc runtime.
                \\  This is likely internal to Roc's builtins or refcounting implementation,
                \\  not a bug in the application code. Cleaning up {d} allocations...
                \\
            , .{ remaining_count, remaining_count }) catch "";
            std.debug.print("{s}", .{msg});
        }

        for (host_env.roc_allocations.items) |alloc| {
            const slice = alloc.ptr[0..alloc.total_size];
            allocator.rawFree(slice, alloc.alignment, @returnAddress());
        }
        // Free the tracking list itself
        host_env.roc_allocations.deinit(allocator);

        const leaked = host_env.gpa.deinit();
        if (leaked == .leak) {
            std.debug.print("{s}", .{
                \\
                \\[Roc Memory Info] Additional memory leak detected by GPA.
                \\  This indicates memory allocated outside of rocAllocFn was not freed.
                \\  This is internal to Roc's compiler/runtime, not application code.
                \\
            });
        }
    }

    // The host-private RocOps used by builtins helpers (RocStr etc.) and the
    // exported runtime symbols; not part of the ABI.
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{ .count = 0, .fns = undefined },
    };

    g_roc_ops = &roc_ops;

    // Call the app's main! entrypoint with its natural C ABI.
    roc_main();

    // Check test results if in test mode
    if (host_env.test_state.enabled) {
        // Check if test failed or not all entries were consumed
        if (host_env.test_state.failed or host_env.test_state.current_index != host_env.test_state.entries.len) {
            // Print failure info
            if (host_env.test_state.failure_info) |info| {
                if (info.spec_line == 0) {
                    // Extra/unexpected output
                    std.debug.print("{s}", .{"TEST FAILED: Unexpected "});
                    std.debug.print("{s}", .{effectTypeName(info.actual_type)});
                    std.debug.print("{s}", .{" output: \""});
                    std.debug.print("{s}", .{info.actual_value});
                    std.debug.print("{s}", .{"\"\n"});
                } else {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at spec line {d}:\n  Expected: {s} \"{s}\"\n  Got:      {s} \"{s}\"\n", .{
                        info.spec_line,
                        effectTypeName(info.expected_type),
                        info.expected_value,
                        effectTypeName(info.actual_type),
                        info.actual_value,
                    }) catch "TEST FAILED\n";
                    std.debug.print("{s}", .{msg});
                }
            } else if (host_env.test_state.current_index < host_env.test_state.entries.len) {
                // Not all entries were consumed - list what's remaining
                const remaining = host_env.test_state.entries.len - host_env.test_state.current_index;
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "TEST FAILED: {d} expected IO operation(s) not performed:\n", .{remaining}) catch "TEST FAILED: expected IO operations not performed\n";
                std.debug.print("{s}", .{msg});

                // List up to 5 unconsumed entries
                const max_to_show: usize = 5;
                var shown: usize = 0;
                for (host_env.test_state.entries[host_env.test_state.current_index..]) |entry| {
                    if (shown >= max_to_show) {
                        std.debug.print("{s}", .{"  ...\n"});
                        break;
                    }
                    std.debug.print("{s}", .{"  - "});
                    std.debug.print("{s}", .{effectTypeName(entry.effect_type)});
                    std.debug.print("{s}", .{": \""});
                    std.debug.print("{s}", .{entry.value});
                    std.debug.print("{s}", .{"\"\n"});
                    shown += 1;
                }
            } else {
                std.debug.print("{s}", .{"TEST FAILED\n"});
            }

            return 1;
        }
    }

    // An inline `expect` failure doesn't halt execution, but must be
    // reported via a non-zero exit status.
    if (host_env.inline_expect_failed) {
        return 1;
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

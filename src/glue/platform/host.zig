//! Platform host for the Roc glue generator.
//!
//! This host provides the runtime for executing glue specs. It compiles
//! platform source files, extracts type information, and passes it to
//! the Roc glue spec, which then handles generating glue code.
//!
//! Entry point: make_glue : List Types -> Result (List File) Str
const std = @import("std");
const Allocator = std.mem.Allocator;
const shim_io = @import("shim_io");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
const build_options = @import("build_options");

const trace_refcount = build_options.trace_refcount;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal std.Io override for debug output; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Zig logging configuration override
pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
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

/// Tracking entry for a Roc allocation
const RocAllocation = struct {
    ptr: [*]u8, // Base pointer (before user data, includes size metadata)
    total_size: usize,
    alignment: std.mem.Alignment,
};

/// Host environment - contains DebugAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    std_io: std.Io,
    /// Track Roc allocations for cleanup on test failure
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .{ .items = &.{}, .capacity = 0 },
    /// Allocation counters for diagnostics
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
};

/// Report an out-of-memory failure from a Roc host allocation callback and
/// abort. These callbacks use the C ABI and cannot return a Zig error, so a
/// reported `exit(1)` is the graceful failure path.
fn hostAllocFailed(host: *HostEnv) noreturn {
    const stderr: std.Io.File = .stderr();
    stderr.writeStreamingAll(host.std_io, "\x1b[31mHost error:\x1b[0m out of memory\n") catch {};
    std.process.exit(1);
}

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(ops: *builtins.host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const env = ops.env;
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("rocAllocFn: env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }

    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const total_size = length + size_storage_bytes;

    const result = allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse hostAllocFailed(host);

    const base_addr = @intFromPtr(base_ptr);
    if (base_addr % min_alignment != 0) {
        @panic("Host allocator returned misaligned memory in rocAllocFn");
    }

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    const answer: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    const answer_addr = @intFromPtr(answer);
    if (answer_addr % alignment != 0) {
        @panic("Host allocator returned misaligned answer in rocAllocFn");
    }

    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch hostAllocFailed(host);
    host.alloc_count += 1;

    if (trace_refcount or (builtin.mode == .Debug and builtin.os.tag != .freestanding)) {
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ answer_addr, length, alignment });
    }

    return answer;
}

/// Roc deallocation function with size-tracking metadata
fn rocDeallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const env = ops.env;
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocDeallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    if (trace_refcount or (builtin.mode == .Debug and builtin.os.tag != .freestanding)) {
        std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{
            @intFromPtr(ptr),
            alignment,
            total_size,
            size_storage_bytes,
        });
    }

    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.dealloc_count += 1;

    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    allocator.rawFree(slice, align_enum, @returnAddress());
}

/// Roc reallocation function with size-tracking metadata
fn rocReallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const env = ops.env;
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocReallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));

    const old_total_size = old_size_ptr.*;

    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    const new_total_size = new_length + size_storage_bytes;

    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];

    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse hostAllocFailed(host);

    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_slice[0..copy_size]);

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
    }) catch hostAllocFailed(host);

    allocator.rawFree(old_slice, align_enum, @returnAddress());

    const new_slice = new_ptr[0..new_total_size];

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    const answer: *anyopaque = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);

    if (trace_refcount or (builtin.mode == .Debug and builtin.os.tag != .freestanding)) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(old_base_ptr) + size_storage_bytes, @intFromPtr(answer), new_length });
    }

    return answer;
}

/// Roc debug function
fn rocDbgFn(_: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(_: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const message = bytes[0..len];
    const stderr: std.Io.File = .stderr();
    var buf: [256]u8 = undefined;
    var w = stderr.writer(host.std_io, &buf);
    w.interface.print("\n\x1b[31mRoc crashed:\x1b[0m {s}\n", .{message}) catch {};
    w.interface.flush() catch {};
    std.process.exit(1);
}

// Use the actual types from builtins
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;

// Roc Type Definitions for Glue Platform

/// TypeId is just a U64 wrapper in Roc
const TypeId = u64;

/// File record - matches Roc { name : Str, content : Str }
/// Roc orders record fields alphabetically: content, name
const File = extern struct {
    content: RocStr,
    name: RocStr,
};

/// EntryPoint record: { name : Str, type_id : U64 }
/// Roc records are ordered alphabetically by field name: name < type_id
const EntryPoint = extern struct {
    name: RocStr,
    type_id: TypeId,
};

/// FunctionInfo record: { name : Str, type_str : Str }
/// Roc records are ordered alphabetically by field name: name < type_str
const FunctionInfoRoc = extern struct {
    name: RocStr,
    type_str: RocStr,
};

/// HostedFunctionInfo record: { index : U64, name : Str, type_str : Str }
/// Roc records are ordered alphabetically: index < name < type_str
const HostedFunctionInfoRoc = extern struct {
    index: u64,
    name: RocStr,
    type_str: RocStr,
};

/// ModuleTypeInfo record: { functions : List(FunctionInfo), hosted_functions : List(HostedFunctionInfo), main_type : Str, name : Str }
/// Roc records are ordered alphabetically: functions < hosted_functions < main_type < name
const ModuleTypeInfoRoc = extern struct {
    functions: RocList, // List(FunctionInfo)
    hosted_functions: RocList, // List(HostedFunctionInfo)
    main_type: RocStr,
    name: RocStr,
};

/// ProvidesEntry := { ffi_symbol : Str, name : Str, type_id : U64 }
/// Fields ordered by alignment descending, then alphabetically
const ProvidesEntry = extern struct {
    ffi_symbol: RocStr,
    name: RocStr,
    type_id: u64,
};

/// Types opaque type internals (matches Types.roc)
/// Fields ordered by alignment descending, then alphabetically
/// Types record: { entrypoints : List(EntryPoint), modules : List(ModuleTypeInfo), provides_entries : List(ProvidesEntry), type_table : List(TypeRepr) }
const TypesInner = extern struct {
    entrypoints: RocList, // List({ name : Str, type_id : U64 })
    modules: RocList, // List(ModuleTypeInfo)
    provides_entries: RocList, // List(ProvidesEntry)
    type_table: RocList, // List(TypeRepr)
};

/// Result (List File) Str - Roc Result type
/// Ok variant has tag 1, Err variant has tag 0 (alphabetical: Err < Ok)
const ResultTag = enum(u8) {
    Err = 0,
    Ok = 1,
};

const ResultListFileStr = extern struct {
    payload: extern union {
        ok: RocList, // List File
        err: RocStr,
    },
    tag: ResultTag,
};

// The app's entrypoint, exported under its provides symbol with its natural
// C ABI: make_glue_for_host takes List(Types) and returns Try(List(File), Str).
extern fn roc_make_glue(types_list: RocList) callconv(.c) ResultListFileStr;

// The host's private RocOps. The exported runtime symbols below and the
// builtins helpers reach the host allocator through this global, set by
// platform_main before any Roc code runs.
var g_roc_ops: ?*builtins.host_abi.RocOps = null;

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

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });
}

// OS-specific entry point handling
comptime {
    @export(&main, .{ .name = "main" });

    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    const std_io = shim_io.io();
    const stderr_file: std.Io.File = .stderr();

    // Expect platform source path as first argument
    const arg_count: usize = @intCast(argc);
    if (arg_count < 2) {
        stderr_file.writeStreamingAll(std_io, "HOST ERROR: Expected platform source path as argument\n") catch {};
        return 1;
    }

    // Convert argv to slice, skipping program name (argv[0])
    const args = argv[1..arg_count];

    const exit_code = platform_main(args, std_io) catch |err| {
        stderr_file.writeStreamingAll(std_io, "HOST ERROR: ") catch {};
        stderr_file.writeStreamingAll(std_io, @errorName(err)) catch {};
        stderr_file.writeStreamingAll(std_io, "\n") catch {};
        return 1;
    };
    return exit_code;
}

const SMALL_STRING_SIZE = @sizeOf(RocStr);

/// Create a big RocStr from a slice (avoids small string encoding issues)
fn createBigRocStr(str: []const u8, roc_ops: *builtins.host_abi.RocOps) RocStr {
    if (str.len < SMALL_STRING_SIZE) {
        // Force big string allocation by allocating at least 24 bytes
        const first_element = builtins.utils.allocateWithRefcount(
            SMALL_STRING_SIZE,
            @sizeOf(usize),
            false,
            roc_ops,
        );
        @memcpy(first_element[0..str.len], str);
        @memset(first_element[str.len..SMALL_STRING_SIZE], 0);

        return RocStr{
            .bytes = first_element,
            .capacity_or_alloc_ptr = RocStr.encodeCapacity(SMALL_STRING_SIZE),
            .length = str.len,
        };
    } else {
        return RocStr.fromSlice(str, roc_ops);
    }
}

/// JSON structures for parsing types_json
const JsonFunctionInfo = struct {
    name: []const u8,
    type_str: []const u8,
};

const JsonHostedFunctionInfo = struct {
    index: u64,
    name: []const u8,
    type_str: []const u8,
};

const JsonModuleTypeInfo = struct {
    name: []const u8,
    main_type: []const u8,
    functions: []const JsonFunctionInfo,
    hosted_functions: []const JsonHostedFunctionInfo,
};

/// Clean up result payload from roc_make_glue
fn cleanupResult(result: *ResultListFileStr, roc_ops: *builtins.host_abi.RocOps) void {
    switch (result.tag) {
        .Ok => {
            const files = result.payload.ok;
            if (files.bytes) |file_bytes| {
                const file_slice: [*]File = @ptrCast(@alignCast(file_bytes));
                for (0..files.length) |i| {
                    file_slice[i].content.decref(roc_ops);
                    file_slice[i].name.decref(roc_ops);
                }
            }
            // Note: The files list container itself is Roc-allocated and will be
            // cleaned up by the defer block's rawFree of remaining roc_allocations
        },
        .Err => {
            result.payload.err.decref(roc_ops);
        },
    }
}

/// Parse types_json and build RocList of ModuleTypeInfoRoc
/// All list allocations use Roc's allocation scheme (allocateWithRefcount) so that
/// Roc's compiled code can properly check refcounts at bytes-8.
fn parseTypesJson(
    allocator: std.mem.Allocator,
    json_str: []const u8,
    roc_ops: *builtins.host_abi.RocOps,
) Allocator.Error!RocList {
    const host: *HostEnv = @ptrCast(@alignCast(roc_ops.env));
    // Parse the JSON
    const parsed = std.json.parseFromSlice([]const JsonModuleTypeInfo, allocator, json_str, .{}) catch |err| {
        const stderr: std.Io.File = .stderr();
        stderr.writeStreamingAll(host.std_io, "Error parsing types JSON: ") catch {};
        var buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "{}\n", .{err}) catch "unknown error\n";
        stderr.writeStreamingAll(host.std_io, msg) catch {};
        return RocList.empty();
    };
    defer parsed.deinit();

    const modules = parsed.value;
    if (modules.len == 0) {
        return RocList.empty();
    }

    // Allocate array for ModuleTypeInfoRoc entries using Roc's allocation scheme
    // ModuleTypeInfoRoc contains RocStr and RocList fields which are refcounted
    const modules_data_size = modules.len * @sizeOf(ModuleTypeInfoRoc);
    const modules_bytes = builtins.utils.allocateWithRefcount(
        modules_data_size,
        @alignOf(ModuleTypeInfoRoc),
        true, // elements ARE refcounted (ModuleTypeInfoRoc contains RocStr/RocList)
        roc_ops,
    );

    const modules_ptr: [*]ModuleTypeInfoRoc = @ptrCast(@alignCast(modules_bytes));

    for (modules, 0..) |mod, mod_idx| {
        // Build functions list using Roc's allocation scheme
        // FunctionInfoRoc contains RocStr fields which are refcounted
        const functions_list = if (mod.functions.len > 0) blk: {
            const funcs_data_size = mod.functions.len * @sizeOf(FunctionInfoRoc);
            const funcs_bytes = builtins.utils.allocateWithRefcount(
                funcs_data_size,
                @alignOf(FunctionInfoRoc),
                true, // elements ARE refcounted (FunctionInfoRoc contains RocStr)
                roc_ops,
            );

            const funcs_ptr: [*]FunctionInfoRoc = @ptrCast(@alignCast(funcs_bytes));

            for (mod.functions, 0..) |func, func_idx| {
                funcs_ptr[func_idx] = FunctionInfoRoc{
                    .name = createBigRocStr(func.name, roc_ops),
                    .type_str = createBigRocStr(func.type_str, roc_ops),
                };
            }

            break :blk RocList{
                .bytes = funcs_bytes,
                .length = mod.functions.len,
                .capacity_or_alloc_ptr = RocList.encodeCapacity(mod.functions.len),
            };
        } else RocList.empty();

        // Build hosted_functions list using Roc's allocation scheme
        // HostedFunctionInfoRoc contains RocStr fields which are refcounted
        const hosted_functions_list = if (mod.hosted_functions.len > 0) blk: {
            const hosted_data_size = mod.hosted_functions.len * @sizeOf(HostedFunctionInfoRoc);
            const hosted_bytes = builtins.utils.allocateWithRefcount(
                hosted_data_size,
                @alignOf(HostedFunctionInfoRoc),
                true, // elements ARE refcounted (HostedFunctionInfoRoc contains RocStr)
                roc_ops,
            );

            const hosted_ptr: [*]HostedFunctionInfoRoc = @ptrCast(@alignCast(hosted_bytes));

            for (mod.hosted_functions, 0..) |hosted, hosted_idx| {
                hosted_ptr[hosted_idx] = HostedFunctionInfoRoc{
                    .index = hosted.index,
                    .name = createBigRocStr(hosted.name, roc_ops),
                    .type_str = createBigRocStr(hosted.type_str, roc_ops),
                };
            }

            break :blk RocList{
                .bytes = hosted_bytes,
                .length = mod.hosted_functions.len,
                .capacity_or_alloc_ptr = RocList.encodeCapacity(mod.hosted_functions.len),
            };
        } else RocList.empty();

        modules_ptr[mod_idx] = ModuleTypeInfoRoc{
            .functions = functions_list,
            .hosted_functions = hosted_functions_list,
            .main_type = createBigRocStr(mod.main_type, roc_ops),
            .name = createBigRocStr(mod.name, roc_ops),
        };
    }

    return RocList{
        .bytes = modules_bytes,
        .length = modules.len,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(modules.len),
    };
}

/// Platform host entrypoint
/// Receives args: [platform_path, --types-json=<json>, entry_point_names...]
/// If no entry point names are provided, defaults to ["main"].
fn platform_main(args: [][*:0]u8, std_io: std.Io) (Allocator.Error || error{ MissingPlatformPath, MissingEntrypointNames })!c_int {
    if (args.len < 1) {
        return error.MissingPlatformPath;
    }

    // Parse --types-json and --output-dir arguments if present
    var types_json: ?[]const u8 = null;
    var output_dir: ?[]const u8 = null;
    var entry_point_start_idx: usize = 1;
    for (args[1..], 1..) |arg, idx| {
        const arg_str = std.mem.span(arg);
        if (std.mem.startsWith(u8, arg_str, "--types-json=")) {
            types_json = arg_str["--types-json=".len..];
            entry_point_start_idx = idx + 1;
        } else if (std.mem.startsWith(u8, arg_str, "--output-dir=")) {
            output_dir = arg_str["--output-dir=".len..];
            entry_point_start_idx = idx + 1;
        } else {
            // First non-flag argument is start of entry points
            break;
        }
    }

    // Install signal handlers
    _ = base.signal_handler.installForCurrentThread(.{
        .stack_overflow = handleRocStackOverflow,
        .access_violation = handleRocAccessViolation,
        .arithmetic_error = handleRocArithmeticError,
    });
    if (builtin.mode == .Debug and builtin.os.tag != .freestanding) {
        builtins.utils.DebugRefcountTracker.enable();
    }

    var host_env = HostEnv{
        .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
        .std_io = std_io,
    };

    defer {
        const allocator = host_env.gpa.allocator();
        const remaining_count = host_env.roc_allocations.items.len;

        if (remaining_count > 0) {
            if (builtin.mode == .Debug and builtin.os.tag != .freestanding) {
                _ = builtins.utils.DebugRefcountTracker.reportLeaks();
            }
            const stderr_file: std.Io.File = .stderr();
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf,
                \\[Roc Memory Info] {d} allocation(s) not freed by Roc runtime.
                \\  Cleaning up {d} allocations...
                \\
            , .{ remaining_count, remaining_count }) catch "";
            stderr_file.writeStreamingAll(host_env.std_io, msg) catch {};
        }

        for (host_env.roc_allocations.items) |alloc| {
            const slice = alloc.ptr[0..alloc.total_size];
            allocator.rawFree(slice, alloc.alignment, @returnAddress());
        }
        host_env.roc_allocations.deinit(allocator);

        const leaked = host_env.gpa.deinit();
        if (leaked == .leak) {
            const stderr_file: std.Io.File = .stderr();
            stderr_file.writeStreamingAll(host_env.std_io,
                \\
                \\[Roc Memory Info] Additional memory leak detected by GPA.
                \\
            ) catch {};
        }
    }

    // The host's private RocOps for using builtins helpers (RocStr/RocList
    // allocation, decref). Not part of the ABI.
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

    const allocator = host_env.gpa.allocator();

    const stdout: std.Io.File = .stdout();

    if (args.len <= entry_point_start_idx) return error.MissingEntrypointNames;
    const entry_point_names = allocator.alloc([]const u8, args.len - entry_point_start_idx) catch return error.OutOfMemory;
    for (args[entry_point_start_idx..], 0..) |arg, i| {
        entry_point_names[i] = std.mem.span(arg);
    }
    defer allocator.free(entry_point_names);

    // Allocate array for EntryPoint entries using Roc's allocation scheme
    // This ensures a valid refcount is present at bytes-8, which Roc's
    // list operations expect when checking isUnique() etc.
    // EntryPoint contains RocStr which is refcounted, so use elements_refcounted=true
    // to allocate 16 bytes of header space (refcount + element count).
    const tuple1_size = @sizeOf(EntryPoint);
    const entrypoints_data_size = entry_point_names.len * tuple1_size;
    const entrypoints_bytes = builtins.utils.allocateWithRefcount(
        entrypoints_data_size,
        @alignOf(EntryPoint),
        true, // elements ARE refcounted (EntryPoint contains RocStr)
        &roc_ops,
    );

    const entrypoints_ptr: [*]EntryPoint = @ptrCast(@alignCast(entrypoints_bytes));

    // Build EntryPoint for each entry point
    for (entry_point_names, 0..) |name, idx| {
        // Force big string creation to avoid small string encoding mismatch.
        // Roc's compiled code reads the length field directly, which is 0 for small strings
        // (small strings store length in byte[23] with high bit set).
        // By always allocating >= 24 bytes, we ensure the string is treated as a big string.
        const roc_name = if (name.len < SMALL_STRING_SIZE) blk: {
            // Force big string allocation by allocating at least 24 bytes
            const first_element = builtins.utils.allocateWithRefcount(
                SMALL_STRING_SIZE,
                @sizeOf(usize),
                false,
                &roc_ops,
            );
            @memcpy(first_element[0..name.len], name);
            @memset(first_element[name.len..SMALL_STRING_SIZE], 0);

            break :blk RocStr{
                .bytes = first_element,
                .capacity_or_alloc_ptr = RocStr.encodeCapacity(SMALL_STRING_SIZE),
                .length = name.len,
            };
        } else blk: {
            // For strings >= 24 bytes, fromSlice already creates a big string
            break :blk RocStr.fromSlice(name, &roc_ops);
        };

        entrypoints_ptr[idx] = EntryPoint{
            .name = roc_name,
            .type_id = 0, // TypeId 0 for now - will be populated by type extraction
        };
    }

    // Create RocList for entrypoints
    const entrypoints_list = RocList{
        .bytes = entrypoints_bytes,
        .length = entry_point_names.len,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(entry_point_names.len),
    };

    // Parse types_json to create modules list
    const modules_list = if (types_json) |json|
        parseTypesJson(allocator, json, &roc_ops) catch RocList.empty()
    else
        RocList.empty();

    // Build provides list from entry point names
    // In standalone mode, entry point names are FFI symbols, so use them as both name and ffi_symbol
    const provides_list = if (entry_point_names.len > 0) pblk: {
        const prov_data_size = entry_point_names.len * @sizeOf(ProvidesEntry);
        const prov_bytes = builtins.utils.allocateWithRefcount(
            prov_data_size,
            @alignOf(ProvidesEntry),
            true, // elements ARE refcounted (ProvidesEntry contains RocStr)
            &roc_ops,
        );
        const prov_ptr: [*]ProvidesEntry = @ptrCast(@alignCast(prov_bytes));

        for (entry_point_names, 0..) |name, idx| {
            prov_ptr[idx] = ProvidesEntry{
                .ffi_symbol = createBigRocStr(name, &roc_ops),
                .name = createBigRocStr(name, &roc_ops),
                .type_id = 0,
            };
        }

        break :pblk RocList{
            .bytes = prov_bytes,
            .length = entry_point_names.len,
            .capacity_or_alloc_ptr = RocList.encodeCapacity(entry_point_names.len),
        };
    } else RocList.empty();

    // Heap-allocate TypesInner using Roc's allocation scheme
    // This ensures a valid refcount is present at bytes-8, which Roc's
    // list operations expect when checking isUnique() etc.
    // TypesInner contains RocList fields which are refcounted
    const types_inner_bytes = builtins.utils.allocateWithRefcount(
        @sizeOf(TypesInner),
        @alignOf(TypesInner),
        true, // elements ARE refcounted (TypesInner contains RocList)
        &roc_ops,
    );

    const types_inner_ptr: *TypesInner = @ptrCast(@alignCast(types_inner_bytes));
    types_inner_ptr.* = TypesInner{
        .entrypoints = entrypoints_list,
        .modules = modules_list,
        .provides_entries = provides_list,
        .type_table = RocList.empty(),
    };

    // Create a List Types with one element (the Types structure)
    const types_list = RocList{
        .bytes = types_inner_bytes,
        .length = 1,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(1),
    };

    // Call the Roc glue spec
    // Note: Roc consumes types_list (takes ownership), so it handles cleanup
    // of all nested structures. We must NOT manually clean up after this call.
    var result: ResultListFileStr = roc_make_glue(types_list);
    defer cleanupResult(&result, &roc_ops);

    // Handle the result
    const stderr: std.Io.File = .stderr();

    switch (result.tag) {
        .Err => {
            const err_str = result.payload.err;
            stderr.writeStreamingAll(host_env.std_io, "Glue spec error: ") catch {};
            stderr.writeStreamingAll(host_env.std_io, err_str.asSlice()) catch {};
            stderr.writeStreamingAll(host_env.std_io, "\n") catch {};
            return 1;
        },

        .Ok => {
            const files = result.payload.ok;
            if (files.len() == 0) {
                stdout.writeStreamingAll(host_env.std_io, "Glue spec returned 0 files.\n") catch {};
                return 0;
            }

            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Glue spec returned {d} file(s):\n", .{files.len()}) catch "Glue spec returned files:\n";
            stdout.writeStreamingAll(host_env.std_io, msg) catch {};

            // Write files to output directory if provided
            const file_bytes = files.bytes orelse return 0;
            const file_slice: [*]const File = @ptrCast(@alignCast(file_bytes));

            const out_dir = output_dir orelse {
                stderr.writeStreamingAll(host_env.std_io, "Error: No --output-dir specified; cannot write glue files\n") catch {};
                return 1;
            };

            // For real filesystem writes (createDirPath / writeFile) we need the
            // full threaded std.Io vtable rather than the minimal shim_io one,
            // which only implements stdio + futex + a few read helpers. The glue
            // host is a build-time tool (invoked during `zig build` to generate
            // glue code), so pulling in std.Io.Threaded here is fine.
            const fs_io = std.Io.Threaded.global_single_threaded.io();

            // Create output directory if needed
            std.Io.Dir.cwd().createDirPath(fs_io, out_dir) catch |err| {
                stderr.writeStreamingAll(host_env.std_io, "Error: Could not create output directory: ") catch {};
                var err_buf: [256]u8 = undefined;
                const err_msg = std.fmt.bufPrint(&err_buf, "{}\n", .{err}) catch "unknown error\n";
                stderr.writeStreamingAll(host_env.std_io, err_msg) catch {};
                return 1;
            };

            // Write each file
            for (0..files.len()) |i| {
                const file = file_slice[i];
                const file_name = file.name.asSlice();
                const file_path = std.fs.path.join(allocator, &.{ out_dir, file_name }) catch {
                    stderr.writeStreamingAll(host_env.std_io, "Error: Out of memory allocating file path\n") catch {};
                    return 1;
                };
                defer allocator.free(file_path);

                std.Io.Dir.cwd().writeFile(fs_io, .{
                    .sub_path = file_path,
                    .data = file.content.asSlice(),
                }) catch |err| {
                    stderr.writeStreamingAll(host_env.std_io, "Error: Could not write file '") catch {};
                    stderr.writeStreamingAll(host_env.std_io, file_path) catch {};
                    stderr.writeStreamingAll(host_env.std_io, "': ") catch {};
                    var err_buf: [256]u8 = undefined;
                    const err_msg = std.fmt.bufPrint(&err_buf, "{}\n", .{err}) catch "unknown error\n";
                    stderr.writeStreamingAll(host_env.std_io, err_msg) catch {};
                    return 1;
                };

                stdout.writeStreamingAll(host_env.std_io, "  Wrote: ") catch {};
                stdout.writeStreamingAll(host_env.std_io, file_path) catch {};
                stdout.writeStreamingAll(host_env.std_io, "\n") catch {};
            }

            return 0;
        },
    }
}

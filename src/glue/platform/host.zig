//! Platform host for the Roc glue generator.
//!
//! This host provides the runtime for executing glue specs. It compiles
//! platform source files, extracts type information, and passes it to
//! the Roc glue spec, which then handles generating glue code.
//!
//! Entry point: make_glue : List Types -> Result (List File) Str
const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const build_options = @import("build_options");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .wasi) std.posix else undefined;

// Compiler modules for type extraction
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");
const layout_mod = @import("layout");

const trace_refcount = build_options.trace_refcount;

/// Zig logging configuration override
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

/// Tracking entry for a Roc allocation
const RocAllocation = struct {
    ptr: [*]u8, // Base pointer (before user data, includes size metadata)
    total_size: usize,
    alignment: std.mem.Alignment,
};

/// Host environment - contains GeneralPurposeAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.GeneralPurposeAllocator(.{ .safety = true }),
    /// Track Roc allocations for cleanup on test failure
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .{},
    /// Allocation counters for diagnostics
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
    const roc_alloc_addr = @intFromPtr(roc_alloc);
    if (roc_alloc_addr % @alignOf(builtins.host_abi.RocAlloc) != 0) {
        std.debug.panic("[rocAllocFn] roc_alloc ptr not aligned! addr=0x{x} required={}", .{ roc_alloc_addr, @alignOf(builtins.host_abi.RocAlloc) });
    }

    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("rocAllocFn: env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }

    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_alloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(roc_alloc.alignment, @alignOf(usize));
    const total_size = roc_alloc.length + size_storage_bytes;

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

    const base_addr = @intFromPtr(base_ptr);
    if (base_addr % min_alignment != 0) {
        @panic("Host allocator returned misaligned memory in rocAllocFn");
    }

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    const answer_addr = @intFromPtr(roc_alloc.answer);
    if (answer_addr % roc_alloc.alignment != 0) {
        @panic("Host allocator returned misaligned answer in rocAllocFn");
    }

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
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocDeallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_dealloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

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

    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - size_storage_bytes);

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
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocReallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_realloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));

    const old_total_size = old_size_ptr.*;

    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);

    const new_total_size = roc_realloc.new_length + size_storage_bytes;

    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];

    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        const stderr: std.fs.File = .stderr();
        stderr.writeAll("\x1b[31mHost error:\x1b[0m reallocation failed, out of memory\n") catch {};
        std.process.exit(1);
    };

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
    }) catch {};

    allocator.rawFree(old_slice, align_enum, @returnAddress());

    const new_slice = new_ptr[0..new_total_size];

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    roc_realloc.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(old_base_ptr) + size_storage_bytes, @intFromPtr(roc_realloc.answer), roc_realloc.new_length });
    }
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, _: *anyopaque) callconv(.c) void {
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, _: *anyopaque) callconv(.c) void {
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, _: *anyopaque) callconv(.c) noreturn {
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    const stderr: std.fs.File = .stderr();
    var buf: [256]u8 = undefined;
    var w = stderr.writer(&buf);
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

/// Tuple2 = [T TypeId (List TypeId)] - single-tag union
const Tuple2 = extern struct {
    type_id: TypeId,
    type_ids: RocList,
};

/// Shape tag values - alphabetically ordered
const ShapeTag = enum(u8) {
    Bool = 0,
    EmptyTagUnion = 1,
    Function = 2,
    Num = 3,
    RecursivePointer = 4,
    RocBox = 5,
    RocDict = 6,
    RocList = 7,
    RocResult = 8,
    RocSet = 9,
    RocStr = 10,
    Struct = 11,
    TagUnion = 12,
    TagUnionPayload = 13,
    Unit = 14,
    Unsized = 15,
};

/// Shape union - we represent this as the largest possible size
/// The actual payload depends on the tag
const Shape = extern struct {
    // The shape is a tagged union. We'll represent it with enough space for the largest variant.
    // For simplicity in the minimal implementation, we use a fixed-size payload area.
    payload: [64]u8 align(8),
    tag: ShapeTag,
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

/// Types opaque type internals (matches Types.roc)
/// Fields are ordered alphabetically in Roc records
/// Types record: { entrypoints : List({ name : Str, type_id : U64 }), modules : List(ModuleTypeInfo) }
const TypesInner = extern struct {
    entrypoints: RocList, // List({ name : Str, type_id : U64 })
    modules: RocList, // List(ModuleTypeInfo)
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

// External Roc entry point
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
// External Roc entry point - name comes from "provides { make_glue_for_host: "make_glue" }"
// The extern symbol is "roc__" + the quoted name ("make_glue")
extern fn roc__make_glue(
    ops: *builtins.host_abi.RocOps,
    ret_ptr: *ResultListFileStr,
    args_ptr: *RocList,
) callconv(.c) void;

// OS-specific entry point handling
comptime {
    @export(&main, .{ .name = "main" });

    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    const stderr_file: std.fs.File = .stderr();

    // Expect platform source path as first argument
    const arg_count: usize = @intCast(argc);
    if (arg_count < 2) {
        stderr_file.writeAll("HOST ERROR: Expected platform source path as argument\n") catch {};
        return 1;
    }

    // Convert argv to slice, skipping program name (argv[0])
    const args = argv[1..arg_count];

    const exit_code = platform_main(args) catch |err| {
        stderr_file.writeAll("HOST ERROR: ") catch {};
        stderr_file.writeAll(@errorName(err)) catch {};
        stderr_file.writeAll("\n") catch {};
        return 1;
    };
    return exit_code;
}

/// No hosted functions for glue platform
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{};

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
            .length = str.len,
            .capacity_or_alloc_ptr = SMALL_STRING_SIZE,
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

/// Clean up all RocStr values in a ModuleTypeInfoRoc and decref container lists
/// All allocations now use Roc's allocation scheme, so we use utils.decref instead of allocator.free
fn cleanupModuleTypeInfo(mod: *ModuleTypeInfoRoc, roc_ops: *builtins.host_abi.RocOps) void {
    // Decref name and main_type strings
    mod.name.decref(roc_ops);
    mod.main_type.decref(roc_ops);

    // Clean up functions list - decref strings first, then decref the list container
    if (mod.functions.bytes) |func_bytes| {
        const funcs: [*]FunctionInfoRoc = @ptrCast(@alignCast(func_bytes));
        for (0..mod.functions.length) |i| {
            funcs[i].name.decref(roc_ops);
            funcs[i].type_str.decref(roc_ops);
        }
    }
    // Decref the functions list container (allocated via allocateWithRefcount)
    builtins.utils.decref(
        mod.functions.bytes,
        mod.functions.length * @sizeOf(FunctionInfoRoc),
        @alignOf(FunctionInfoRoc),
        true, // elements ARE refcounted (FunctionInfoRoc contains RocStr)
        roc_ops,
    );

    // Clean up hosted_functions list - decref strings first, then decref the list container
    if (mod.hosted_functions.bytes) |hosted_bytes| {
        const hosted: [*]HostedFunctionInfoRoc = @ptrCast(@alignCast(hosted_bytes));
        for (0..mod.hosted_functions.length) |i| {
            hosted[i].name.decref(roc_ops);
            hosted[i].type_str.decref(roc_ops);
        }
    }
    // Decref the hosted_functions list container
    builtins.utils.decref(
        mod.hosted_functions.bytes,
        mod.hosted_functions.length * @sizeOf(HostedFunctionInfoRoc),
        @alignOf(HostedFunctionInfoRoc),
        true, // elements ARE refcounted (HostedFunctionInfoRoc contains RocStr)
        roc_ops,
    );
}

/// Clean up modules_list and all its contained allocations
/// All allocations now use Roc's allocation scheme, so we use utils.decref instead of allocator.free
fn cleanupModulesList(modules_list: RocList, roc_ops: *builtins.host_abi.RocOps) void {
    if (modules_list.bytes) |mod_bytes| {
        const mods: [*]ModuleTypeInfoRoc = @ptrCast(@alignCast(mod_bytes));
        for (0..modules_list.length) |i| {
            cleanupModuleTypeInfo(&mods[i], roc_ops);
        }
    }
    // Decref the modules list container
    builtins.utils.decref(
        modules_list.bytes,
        modules_list.length * @sizeOf(ModuleTypeInfoRoc),
        @alignOf(ModuleTypeInfoRoc),
        true, // elements ARE refcounted (ModuleTypeInfoRoc contains RocStr/RocList)
        roc_ops,
    );
}

/// Clean up result payload from roc__make_glue
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
) !RocList {
    // Parse the JSON
    const parsed = std.json.parseFromSlice([]const JsonModuleTypeInfo, allocator, json_str, .{}) catch |err| {
        const stderr: std.fs.File = .stderr();
        stderr.writeAll("Error parsing types JSON: ") catch {};
        var buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "{}\n", .{err}) catch "unknown error\n";
        stderr.writeAll(msg) catch {};
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
                .capacity_or_alloc_ptr = mod.functions.len,
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
                .capacity_or_alloc_ptr = mod.hosted_functions.len,
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
        .capacity_or_alloc_ptr = modules.len,
    };
}

/// Platform host entrypoint
/// Receives args: [platform_path, --types-json=<json>, entry_point_names...]
/// If no entry point names are provided, defaults to ["main"].
fn platform_main(args: [][*:0]u8) !c_int {
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
    _ = builtins.handlers.install(handleRocStackOverflow, handleRocAccessViolation, handleRocArithmeticError);

    var host_env = HostEnv{
        .gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){},
    };

    defer {
        const allocator = host_env.gpa.allocator();
        const remaining_count = host_env.roc_allocations.items.len;

        if (remaining_count > 0) {
            const stderr_file: std.fs.File = .stderr();
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf,
                \\[Roc Memory Info] {d} allocation(s) not freed by Roc runtime.
                \\  Cleaning up {d} allocations...
                \\
            , .{ remaining_count, remaining_count }) catch "";
            stderr_file.writeAll(msg) catch {};
        }

        for (host_env.roc_allocations.items) |alloc| {
            const slice = alloc.ptr[0..alloc.total_size];
            allocator.rawFree(slice, alloc.alignment, @returnAddress());
        }
        host_env.roc_allocations.deinit(allocator);

        const leaked = host_env.gpa.deinit();
        if (leaked == .leak) {
            const stderr_file: std.fs.File = .stderr();
            stderr_file.writeAll(
                \\
                \\[Roc Memory Info] Additional memory leak detected by GPA.
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

    // Build entrypoints list
    // For now, create a single entry point with the platform path as the name
    // TODO: Extract actual entry points from compiled platform module
    const allocator = host_env.gpa.allocator();

    const stdout: std.fs.File = .stdout();

    // Entry point names from args[entry_point_start_idx..], or default to ["main"] if none provided
    const default_entry_points = [_][]const u8{"main"};
    const entry_point_names: []const []const u8 = if (args.len > entry_point_start_idx) blk: {
        const names = allocator.alloc([]const u8, args.len - entry_point_start_idx) catch return error.OutOfMemory;
        for (args[entry_point_start_idx..], 0..) |arg, i| {
            names[i] = std.mem.span(arg);
        }
        break :blk names;
    } else &default_entry_points;
    defer if (args.len > entry_point_start_idx) allocator.free(entry_point_names);

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
                .length = name.len,
                .capacity_or_alloc_ptr = SMALL_STRING_SIZE,
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
        .capacity_or_alloc_ptr = entry_point_names.len,
    };

    // Parse types_json to create modules list
    const modules_list = if (types_json) |json|
        parseTypesJson(allocator, json, &roc_ops) catch RocList.empty()
    else
        RocList.empty();

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
    };

    // Create a List Types with one element (the Types structure)
    var types_list = RocList{
        .bytes = types_inner_bytes,
        .length = 1,
        .capacity_or_alloc_ptr = 1,
    };

    // Call the Roc glue spec
    // Note: Roc consumes types_list (takes ownership), so it handles cleanup
    // of all nested structures. We must NOT manually clean up after this call.
    var result: ResultListFileStr = undefined;
    roc__make_glue(&roc_ops, &result, &types_list);
    defer cleanupResult(&result, &roc_ops);

    // Handle the result
    const stderr: std.fs.File = .stderr();

    switch (result.tag) {
        .Err => {
            const err_str = result.payload.err;
            stderr.writeAll("Glue spec error: ") catch {};
            stderr.writeAll(err_str.asSlice()) catch {};
            stderr.writeAll("\n") catch {};
            return 1;
        },

        .Ok => {
            const files = result.payload.ok;
            if (files.len() == 0) {
                stdout.writeAll("Glue spec returned 0 files.\n") catch {};
                return 0;
            }

            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Glue spec returned {d} file(s):\n", .{files.len()}) catch "Glue spec returned files:\n";
            stdout.writeAll(msg) catch {};

            // Write files to output directory if provided
            const file_bytes = files.bytes orelse return 0;
            const file_slice: [*]const File = @ptrCast(@alignCast(file_bytes));

            const out_dir = output_dir orelse {
                stderr.writeAll("Error: No --output-dir specified; cannot write glue files\n") catch {};
                return 1;
            };

            // Create output directory if needed
            std.fs.cwd().makePath(out_dir) catch |err| {
                stderr.writeAll("Error: Could not create output directory: ") catch {};
                var err_buf: [256]u8 = undefined;
                const err_msg = std.fmt.bufPrint(&err_buf, "{}\n", .{err}) catch "unknown error\n";
                stderr.writeAll(err_msg) catch {};
                return 1;
            };

            // Write each file
            for (0..files.len()) |i| {
                const file = file_slice[i];
                const file_name = file.name.asSlice();
                const file_path = std.fs.path.join(allocator, &.{ out_dir, file_name }) catch {
                    stderr.writeAll("Error: Out of memory allocating file path\n") catch {};
                    return 1;
                };
                defer allocator.free(file_path);

                std.fs.cwd().writeFile(.{
                    .sub_path = file_path,
                    .data = file.content.asSlice(),
                }) catch |err| {
                    stderr.writeAll("Error: Could not write file '") catch {};
                    stderr.writeAll(file_path) catch {};
                    stderr.writeAll("': ") catch {};
                    var err_buf: [256]u8 = undefined;
                    const err_msg = std.fmt.bufPrint(&err_buf, "{}\n", .{err}) catch "unknown error\n";
                    stderr.writeAll(err_msg) catch {};
                    return 1;
                };

                stdout.writeAll("  Wrote: ") catch {};
                stdout.writeAll(file_path) catch {};
                stdout.writeAll("\n") catch {};
            }

            return 0;
        },
    }
}

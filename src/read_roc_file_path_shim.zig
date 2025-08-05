//! A shim to read the ModuleEnv from shared memory for the interpreter

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const base = @import("base");
const compile = @import("compile");
const types = @import("types");
const eval = @import("eval/interpreter.zig");
const stack = @import("eval/stack.zig");
const layout_store = @import("layout/store.zig");
const layout = @import("layout/layout.zig");

const RocStr = builtins.str.RocStr;
const ModuleEnv = compile.ModuleEnv;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset
const MODULE_ENV_OFFSET = 0x10; // 8 bytes for u64, 4 bytes for u32, 4 bytes padding
const MAX_CLOSURE_PARAMS = 8; // Maximum number of closure parameters supported
const RESULT_BUFFER_SIZE = 1024; // Buffer size for formatting results

// Error types for comprehensive error handling
const MemoryError = error{
    BufferOverflow,
    InvalidOffset,
    NullPointer,
};

const EvaluationError = error{
    InvalidExpressionType,
    UnexpectedClosureStructure,
    StackInitFailed,
    LayoutCacheInitFailed,
    InterpreterInitFailed,
    EvaluationFailed,
    ClosureCallFailed,
    ParameterCountExceeded,
};

const SharedMemoryError = error{
    FdInfoReadFailed,
    SharedMemoryMappingFailed,
    InvalidMemoryLayout,
    RelocationFailed,
};

/// Safely copy memory with bounds checking
fn safeCopy(dst: []u8, src: []const u8, total_size: usize) MemoryError!void {
    if (src.len > total_size) {
        return error.BufferOverflow;
    }
    if (dst.len < src.len) {
        return error.BufferOverflow;
    }
    @memcpy(dst[0..src.len], src);
}

/// Safely get a slice from a pointer with bounds checking
fn safeSlice(ptr: ?*anyopaque, offset: usize, length: usize, total_size: usize) MemoryError![]u8 {
    if (ptr == null) {
        return error.NullPointer;
    }
    if (offset >= total_size) {
        return error.InvalidOffset;
    }
    if (offset + length > total_size) {
        return error.BufferOverflow;
    }
    const base_ptr = @as([*]u8, @ptrCast(ptr.?));
    return base_ptr[offset .. offset + length];
}

/// Validate parameter count is within reasonable bounds
fn validateParameterCount(param_count: usize) EvaluationError!void {
    if (param_count > MAX_CLOSURE_PARAMS) {
        std.log.err("Parameter count {} exceeds maximum {}", .{ param_count, MAX_CLOSURE_PARAMS });
        return error.ParameterCountExceeded;
    }
}

/// Safely copy argument data with bounds checking
fn safeCopyArgument(arg_ptr: ?*anyopaque, dest_ptr: ?*anyopaque, elem_offset: usize, elem_size: usize, max_arg_size: usize) MemoryError!void {
    if (arg_ptr == null or dest_ptr == null) {
        return error.NullPointer;
    }

    if (elem_offset + elem_size > max_arg_size) {
        std.log.err("Argument copy would overflow: offset={}, size={}, max={}", .{ elem_offset, elem_size, max_arg_size });
        return error.BufferOverflow;
    }

    if (elem_size > 0) {
        const src_slice = try safeSlice(arg_ptr, elem_offset, elem_size, max_arg_size);
        const dst_slice = (@as([*]u8, @ptrCast(dest_ptr.?)))[0..elem_size];
        @memcpy(dst_slice, src_slice);
    }
}

/// Common evaluation logic for both POSIX and Windows platforms
fn evaluateExpression(env_ptr: *ModuleEnv, expr_idx: u32, arg_ptr: ?*anyopaque, ops: *builtins.host_abi.RocOps) EvaluationError!RocStr {
    // Set up the interpreter infrastructure
    var eval_stack = stack.Stack.initCapacity(std.heap.page_allocator, 64 * 1024) catch |err| {
        std.log.err("Stack initialization failed: {s}", .{@errorName(err)});
        return error.StackInitFailed;
    };
    defer eval_stack.deinit();

    var layout_cache = layout_store.Store.init(env_ptr, &env_ptr.types) catch |err| {
        std.log.err("Layout cache initialization failed: {s}", .{@errorName(err)});
        return error.LayoutCacheInitFailed;
    };
    defer layout_cache.deinit();

    var interpreter = eval.Interpreter.init(
        std.heap.page_allocator,
        env_ptr,
        &eval_stack,
        &layout_cache,
        &env_ptr.types,
    ) catch |err| {
        std.log.err("Interpreter initialization failed: {s}", .{@errorName(err)});
        return error.InterpreterInitFailed;
    };
    interpreter.initRocOpsEnv();
    defer interpreter.deinit();

    // Enable tracing to stderr
    interpreter.startTrace(std.io.getStdErr().writer().any());

    const expr_idx_enum: ModuleEnv.Expr.Idx = @enumFromInt(expr_idx);

    // Check if the expression is a closure by getting its layout
    const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_idx_enum));
    const layout_idx = layout_cache.addTypeVar(expr_var) catch |err| {
        std.log.err("Failed to add type variable for layout: {s}", .{@errorName(err)});
        return error.LayoutCacheInitFailed;
    };
    const expr_layout = layout_cache.getLayout(layout_idx);

    // Debug output
    std.log.warn("Expression layout tag: {s}", .{@tagName(expr_layout.tag)});
    std.log.warn("arg_ptr is null: {}", .{arg_ptr == null});

    // If it's a closure and we have arguments, handle the closure call properly
    if (expr_layout.tag == .closure and arg_ptr != null) {
        return evaluateClosure(env_ptr, &interpreter, &layout_cache, expr_idx_enum, arg_ptr, ops) catch |err| {
            std.log.err("Closure evaluation failed: {s}", .{@errorName(err)});
            return error.ClosureCallFailed;
        };
    }

    // Evaluate using the REAL interpreter (for non-closure cases)
    const stack_result = interpreter.eval(expr_idx_enum) catch |err| {
        std.log.err("Expression evaluation failed: {s}", .{@errorName(err)});
        return error.EvaluationFailed;
    };

    // Format the result based on its layout
    var buf: [RESULT_BUFFER_SIZE]u8 = undefined;
    const result_str = formatStackResult(stack_result, &layout_cache, &buf, ops);

    return createRocStrFromData(ops, @constCast(result_str.ptr), result_str.len);
}

/// Common closure evaluation logic
fn evaluateClosure(env_ptr: *ModuleEnv, interpreter: *eval.Interpreter, layout_cache: *layout_store.Store, expr_idx_enum: ModuleEnv.Expr.Idx, arg_ptr: ?*anyopaque, ops: *builtins.host_abi.RocOps) !RocStr {
    var buf: [RESULT_BUFFER_SIZE]u8 = undefined;

    const closure_expr = env_ptr.store.getExpr(expr_idx_enum);
    const lambda_expr = switch (closure_expr) {
        .e_closure => |closure_data| env_ptr.store.getExpr(closure_data.lambda_idx),
        .e_lambda => closure_expr,
        else => {
            std.log.err("Expected closure or lambda expression, got: {s}", .{@tagName(closure_expr)});
            return error.UnexpectedClosureStructure;
        },
    };

    const param_patterns = switch (lambda_expr) {
        .e_lambda => |lambda_data| env_ptr.store.slicePatterns(lambda_data.args),
        else => {
            std.log.err("Expected lambda expression, got: {s}", .{@tagName(lambda_expr)});
            return error.UnexpectedClosureStructure;
        },
    };

    const param_count = param_patterns.len;

    // Validate parameter count
    validateParameterCount(param_count) catch |err| {
        const err_str = std.fmt.bufPrint(&buf, "Invalid parameter count: {s}", .{@errorName(err)}) catch "Parameter validation error";
        return createRocStrFromData(ops, @constCast(err_str.ptr), err_str.len);
    };

    if (param_count > 0) {
        try pushClosureArguments(interpreter, layout_cache, param_patterns, arg_ptr, &buf, ops);
    }

    // Now schedule the closure call
    const closure_result = interpreter.callClosureWithStackArgs(expr_idx_enum, @intCast(param_count)) catch |err| {
        const err_str = std.fmt.bufPrint(&buf, "Closure call error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };

    // Format and return the result
    const result_str = formatStackResult(closure_result, layout_cache, &buf, ops);
    return createRocStrFromData(ops, @constCast(result_str.ptr), result_str.len);
}

/// Push closure arguments onto the interpreter stack
fn pushClosureArguments(interpreter: *eval.Interpreter, layout_cache: *layout_store.Store, param_patterns: []const ModuleEnv.Pattern.Idx, arg_ptr: ?*anyopaque, buf: *[RESULT_BUFFER_SIZE]u8, ops: *builtins.host_abi.RocOps) !void {
    const param_count = param_patterns.len;

    if (param_count == 1) {
        // Single parameter case
        const p0 = param_patterns[0];
        const arg0_var: types.Var = @enumFromInt(@intFromEnum(p0));
        const arg0_layout = blk: {
            const added = layout_cache.addTypeVar(arg0_var) catch |err| {
                return err;
            };
            break :blk layout_cache.getLayout(added);
        };
        const size_bytes = layout_cache.layoutSize(arg0_layout);
        const dest_ptr = interpreter.pushStackValue(arg0_layout) catch |err| {
            return err;
        };

        // Use safe copy with bounds checking for single parameter
        safeCopyArgument(arg_ptr, dest_ptr, 0, size_bytes, size_bytes) catch |err| {
            return err;
        };
    } else {
        // Multiple parameters case
        try pushMultipleArguments(interpreter, layout_cache, param_patterns, arg_ptr, buf, ops);
    }
}

/// Push multiple arguments for closure calls
fn pushMultipleArguments(interpreter: *eval.Interpreter, layout_cache: *layout_store.Store, param_patterns: []const ModuleEnv.Pattern.Idx, arg_ptr: ?*anyopaque, buf: *[RESULT_BUFFER_SIZE]u8, ops: *builtins.host_abi.RocOps) !void {
    _ = ops; // Not used in this function but needed for consistency
    _ = buf; // Not used in this function but needed for consistency
    const param_count = param_patterns.len;
    var param_vars_buf: [MAX_CLOSURE_PARAMS]types.Var = undefined;
    var idx: usize = 0;
    while (idx < param_count) : (idx += 1) {
        const pat_idx = param_patterns[idx];
        param_vars_buf[idx] = @enumFromInt(@intFromEnum(pat_idx));
    }

    // Compute element layouts
    var elem_layouts: [MAX_CLOSURE_PARAMS]layout.Layout = undefined;
    var i_build: usize = 0;
    while (i_build < param_count) : (i_build += 1) {
        const v = param_vars_buf[i_build];
        const idx_v = layout_cache.addTypeVar(v) catch |err| {
            return err;
        };
        elem_layouts[i_build] = layout_cache.getLayout(idx_v);
    }

    // Compute offsets using native target alignment rules
    var running_offset: usize = 0;
    var offsets: [MAX_CLOSURE_PARAMS]usize = undefined;
    i_build = 0;
    while (i_build < param_count) : (i_build += 1) {
        const el = elem_layouts[i_build];
        const el_align = el.alignment(base.target.Target.native.target_usize);
        const mask = el_align.toByteUnits() - 1;
        if ((running_offset & mask) != 0) {
            running_offset = (running_offset + mask) & ~mask;
        }
        offsets[i_build] = running_offset;
        running_offset += layout_cache.layoutSize(el);
    }

    // Calculate total argument tuple size for bounds checking
    const total_tuple_size = running_offset;

    // Copy each element from arg_ptr + computed offset to the interpreter stack
    var i_copy: usize = 0;
    while (i_copy < param_count) : (i_copy += 1) {
        const elem_layout = elem_layouts[i_copy];
        const elem_size = layout_cache.layoutSize(elem_layout);
        const elem_offset = offsets[i_copy];

        const dest_ptr = interpreter.pushStackValue(elem_layout) catch |err| {
            return err;
        };

        // Use safe copy with bounds checking
        safeCopyArgument(arg_ptr, dest_ptr, elem_offset, elem_size, total_tuple_size) catch |err| {
            return err;
        };
    }
}

// Platform-specific shared memory implementation
const is_windows = builtin.target.os.tag == .windows;

// POSIX memory mapping functions to access shared memory - via inherited fd only, NOT `shm_open`,
// which always errors on macOS (by design, for security reasons) when the child process is
// an executable that the parent process created.
const posix = if (!is_windows) struct {
    extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: std.c.off_t) ?*anyopaque;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
    extern "c" fn close(fd: c_int) c_int;
} else struct {};

// Windows shared memory functions
const windows = if (is_windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPVOID = ?*anyopaque;
    const LPCWSTR = [*:0]const u16;
    const SIZE_T = usize;

    extern "kernel32" fn OpenFileMappingW(dwDesiredAccess: DWORD, bInheritHandle: BOOL, lpName: LPCWSTR) ?HANDLE;
    extern "kernel32" fn MapViewOfFile(hFileMappingObject: HANDLE, dwDesiredAccess: DWORD, dwFileOffsetHigh: DWORD, dwFileOffsetLow: DWORD, dwNumberOfBytesToMap: SIZE_T) LPVOID;
    extern "kernel32" fn MapViewOfFileEx(hFileMappingObject: HANDLE, dwDesiredAccess: DWORD, dwFileOffsetHigh: DWORD, dwFileOffsetLow: DWORD, dwNumberOfBytesToMap: SIZE_T, lpBaseAddress: LPVOID) LPVOID;
    extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: LPVOID) BOOL;
    extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
    extern "kernel32" fn GetFileSizeEx(hFile: HANDLE, lpFileSize: *i64) BOOL;
    extern "kernel32" fn GetLastError() DWORD;
    extern "kernel32" fn CreateFileW(lpFileName: LPCWSTR, dwDesiredAccess: DWORD, dwShareMode: DWORD, lpSecurityAttributes: ?*anyopaque, dwCreationDisposition: DWORD, dwFlagsAndAttributes: DWORD, hTemplateFile: ?HANDLE) HANDLE;
    extern "kernel32" fn ReadFile(hFile: HANDLE, lpBuffer: *anyopaque, nNumberOfBytesToRead: DWORD, lpNumberOfBytesRead: *DWORD, lpOverlapped: ?*anyopaque) BOOL;

    const FILE_MAP_READ = 0x0004;
    const FILE_MAP_WRITE = 0x0002;

    const GENERIC_READ = 0x80000000;
    const FILE_SHARE_READ = 0x00000001;
    const OPEN_EXISTING = 3;
    const FILE_ATTRIBUTE_NORMAL = 0x00000080;
    const INVALID_HANDLE_VALUE = @as(HANDLE, @ptrFromInt(std.math.maxInt(usize)));

    // Fixed base address for shared memory mapping to avoid ASLR issues
    // Must match the address used in main.zig
    const SHARED_MEMORY_BASE_ADDR = @as(?*anyopaque, @ptrFromInt(0x10000000));
} else struct {};

/// Info read from the coordination file or command line
const FdInfo = struct {
    fd_str: []u8,
    size: usize,
};

/// Read the fd/handle and size from command line (Windows) or filesystem (POSIX)
fn readFdInfo() SharedMemoryError!FdInfo {
    if (comptime is_windows) {
        return readFdInfoFromCommandLine();
    } else {
        return readFdInfoFromFile();
    }
}

/// Windows: Read handle and size from command line arguments
fn readFdInfoFromCommandLine() SharedMemoryError!FdInfo {
    const args = std.process.argsAlloc(std.heap.page_allocator) catch |err| {
        std.log.err("Failed to allocate memory for command line arguments: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.log.err("Invalid command line arguments: expected at least 3 arguments, got {}", .{args.len});
        return error.FdInfoReadFailed;
    }

    const handle_str = args[1];
    const size_str = args[2];

    const fd_str = std.heap.page_allocator.dupe(u8, handle_str) catch |err| {
        std.log.err("Failed to duplicate handle string: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };
    const size = std.fmt.parseInt(usize, size_str, 10) catch |err| {
        std.log.err("Failed to parse size from '{s}': {s}", .{ size_str, @errorName(err) });
        return error.FdInfoReadFailed;
    };

    return FdInfo{
        .fd_str = fd_str,
        .size = size,
    };
}

/// POSIX: Read fd and size from temporary file (existing approach)
fn readFdInfoFromFile() SharedMemoryError!FdInfo {

    // Get our own executable path
    const exe_path = std.fs.selfExePathAlloc(std.heap.page_allocator) catch |err| {
        std.log.err("Failed to get executable path: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };
    defer std.heap.page_allocator.free(exe_path);

    // Get the directory containing our executable (should be "roc-tmp-<random>")
    const exe_dir = std.fs.path.dirname(exe_path) orelse {
        std.log.err("Invalid executable path: no directory component", .{});
        return error.FdInfoReadFailed;
    };
    const dir_basename = std.fs.path.basename(exe_dir);

    // Verify it has the expected prefix
    if (!std.mem.startsWith(u8, dir_basename, "roc-tmp-")) {
        std.log.err("Unexpected directory name: expected 'roc-tmp-*', got '{s}'", .{dir_basename});
        return error.FdInfoReadFailed;
    }

    // Construct the fd file path by appending .txt to the directory path
    // First, remove any trailing slashes from exe_dir
    var dir_path = exe_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = std.fmt.allocPrint(std.heap.page_allocator, "{s}.txt", .{dir_path}) catch |err| {
        std.log.err("Failed to format fd file path: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };
    defer std.heap.page_allocator.free(fd_file_path);

    // Use standard file operations instead of Windows API
    const file = std.fs.cwd().openFile(fd_file_path, .{}) catch |err| {
        std.log.err("Failed to open fd file: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };
    defer file.close();

    var buffer: [128]u8 = undefined;
    const bytes_read = file.readAll(&buffer) catch |err| {
        std.log.err("Failed to read fd file: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };

    const content = buffer[0..bytes_read];

    // Don't delete the fd file yet - let the parent process clean it up
    // std.fs.cwd().deleteFile(fd_file_path) catch {};

    // Parse the content: first line is fd, second line is size
    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    const fd_line = lines.next() orelse {
        std.log.err("Invalid fd file format: missing fd line", .{});
        return error.FdInfoReadFailed;
    };
    const size_line = lines.next() orelse {
        std.log.err("Invalid fd file format: missing size line", .{});
        return error.FdInfoReadFailed;
    };

    const fd_str = std.heap.page_allocator.dupe(u8, std.mem.trim(u8, fd_line, " \r\t")) catch |err| {
        std.log.err("Failed to duplicate fd string: {s}", .{@errorName(err)});
        return error.FdInfoReadFailed;
    };
    const size = std.fmt.parseInt(usize, std.mem.trim(u8, size_line, " \r\t"), 10) catch |err| {
        std.log.err("Failed to parse size from '{s}': {s}", .{ size_line, @errorName(err) });
        return error.FdInfoReadFailed;
    };

    return FdInfo{
        .fd_str = fd_str,
        .size = size,
    };
}

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][ModuleEnv data]
export fn roc_entrypoint(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void {
    // Use the appropriate evaluation function based on platform
    if (is_windows) {
        evaluateFromWindowsSharedMemoryDirect(ops, ret_ptr, arg_ptr) catch |err| {
            std.log.err("Error evaluating from Windows shared memory: {s}", .{@errorName(err)});
        };
    } else {
        evaluateFromPosixSharedMemoryDirect(ops, ret_ptr, arg_ptr) catch |err| {
            std.log.err("Error evaluating from POSIX shared memory: {s}", .{@errorName(err)});
        };
    }
}

fn formatStackResult(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, buf: *[RESULT_BUFFER_SIZE]u8, ops: *builtins.host_abi.RocOps) []const u8 {
    if (stack_result.layout.tag == .scalar) {
        if (stack_result.layout.data.scalar.tag == .int) {
            const precision = stack_result.layout.data.scalar.data.int;
            const int_val = eval.readIntFromMemory(@ptrCast(stack_result.ptr.?), precision);
            return std.fmt.bufPrint(buf, "{}", .{int_val}) catch "Error formatting";
        } else if (stack_result.layout.data.scalar.tag == .bool) {
            const bool_val = @as(*const bool, @ptrCast(@alignCast(stack_result.ptr.?))).*;
            return std.fmt.bufPrint(buf, "{}", .{bool_val}) catch "Error formatting";
        } else if (stack_result.layout.data.scalar.tag == .frac) {
            const float_precision = stack_result.layout.data.scalar.data.frac;
            switch (float_precision) {
                .f32 => {
                    const float_val = @as(*const f32, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                    return std.fmt.bufPrint(buf, "{d}", .{float_val}) catch "Error formatting";
                },
                .f64 => {
                    const float_val = @as(*const f64, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                    return std.fmt.bufPrint(buf, "{d}", .{float_val}) catch "Error formatting";
                },
                .dec => {
                    // Decimal is a 128-bit fixed-point number with 18 decimal places
                    const dec_ptr = @as(*const builtins.dec.RocDec, @ptrCast(@alignCast(stack_result.ptr.?)));
                    const dec_str = dec_ptr.to_str(ops);
                    defer dec_str.decref(ops);
                    return std.fmt.bufPrint(buf, "{s}", .{dec_str.asSlice()}) catch "Error formatting";
                },
            }
        } else if (stack_result.layout.data.scalar.tag == .str) {
            const str_ptr = @as(*const RocStr, @ptrCast(@alignCast(stack_result.ptr.?)));
            return std.fmt.bufPrint(buf, "{s}", .{str_ptr.asSlice()}) catch "Error formatting";
        } else if (stack_result.layout.data.scalar.tag == .opaque_ptr) {
            const ptr_val = @as(*const ?*anyopaque, @ptrCast(@alignCast(stack_result.ptr.?))).*;
            if (ptr_val) |ptr| {
                return std.fmt.bufPrint(buf, "<opaque pointer: 0x{x}>", .{@intFromPtr(ptr)}) catch "Error formatting";
            } else {
                return std.fmt.bufPrint(buf, "<null opaque pointer>", .{}) catch "Error formatting";
            }
        } else {
            // This should never happen as we've covered all scalar types
            return std.fmt.bufPrint(buf, "Unknown scalar type: {}", .{stack_result.layout.data.scalar.tag}) catch "Error";
        }
    } else if (stack_result.layout.tag == .record) {
        // Format record as: { field1: value1, field2: value2, ... }
        // For now, just show we have a record with its size
        const record_data = layout_cache.getRecordData(stack_result.layout.data.record.idx);
        const num_fields = record_data.fields.count;
        return std.fmt.bufPrint(buf, "<record with {} fields, size {} bytes>", .{ num_fields, record_data.size }) catch "Error";
    } else if (stack_result.layout.tag == .tuple) {
        // Format tuple as: (elem1, elem2, ...)
        // For now, just show we have a tuple with its size
        const tuple_data = layout_cache.getTupleData(stack_result.layout.data.tuple.idx);
        const num_elems = tuple_data.fields.count;
        return std.fmt.bufPrint(buf, "<tuple with {} elements, size {} bytes>", .{ num_elems, tuple_data.size }) catch "Error";
    } else if (stack_result.layout.tag == .list) {
        // Format list as: [elem1, elem2, ...]
        // Lists are more complex as they have runtime length
        const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
        return std.fmt.bufPrint(buf, "<list with {} elements>", .{list_ptr.len()}) catch "Error";
    } else if (stack_result.layout.tag == .list_of_zst) {
        // List of zero-sized types (e.g., List({}))
        const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
        return std.fmt.bufPrint(buf, "<list of {} zero-sized elements>", .{list_ptr.len()}) catch "Error";
    } else if (stack_result.layout.tag == .box) {
        // Box is a heap-allocated value
        return std.fmt.bufPrint(buf, "<box>", .{}) catch "Error";
    } else if (stack_result.layout.tag == .box_of_zst) {
        // Box of zero-sized type
        return std.fmt.bufPrint(buf, "<box of zero-sized type>", .{}) catch "Error";
    } else if (stack_result.layout.tag == .closure) {
        // Function closure
        return std.fmt.bufPrint(buf, "<closure>", .{}) catch "Error";
    } else {
        // Should never happen if we've covered all layout tags
        return std.fmt.bufPrint(buf, "Unknown layout type: {}", .{stack_result.layout.tag}) catch "Error";
    }
}

fn evaluateFromWindowsSharedMemoryDirect(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) !void {
    _ = ops;

    const fd_info = readFdInfo() catch |err| {
        std.log.err("Windows: Failed to read fd info: {s}", .{@errorName(err)});
        return err;
    };
    defer std.heap.page_allocator.free(fd_info.fd_str);

    // Parse the inherited handle from command line argument
    const handle_uint = std.fmt.parseInt(usize, fd_info.fd_str, 10) catch |err| {
        std.log.err("Windows: Failed to parse handle from '{s}': {s}", .{ fd_info.fd_str, @errorName(err) });
        return err;
    };

    const shm_handle = @as(windows.HANDLE, @ptrFromInt(handle_uint));

    // Map the shared memory at the same fixed address as the parent to eliminate ASLR issues
    const mapped_ptr = windows.MapViewOfFileEx(shm_handle, windows.FILE_MAP_READ | windows.FILE_MAP_WRITE, 0, 0, fd_info.size, windows.SHARED_MEMORY_BASE_ADDR) orelse {
        const error_code = windows.GetLastError();
        std.log.err("Windows: Failed to map shared memory view (size: {}, error: {})", .{ fd_info.size, error_code });
        return error.SharedMemoryMappingFailed;
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..fd_info.size];
    defer _ = windows.UnmapViewOfFile(mapped_ptr);
    // Don't close the inherited handle - it belongs to the parent process

    // Validate memory layout before accessing
    if (fd_info.size < FIRST_ALLOC_OFFSET + @sizeOf(u64) + @sizeOf(u32)) {
        std.log.err("Windows: Invalid memory layout: size {} is too small (minimum required: {})", .{ fd_info.size, FIRST_ALLOC_OFFSET + @sizeOf(u64) + @sizeOf(u32) });
        return error.InvalidMemoryLayout;
    }

    // The first allocation in SharedMemoryAllocator starts at offset FIRST_ALLOC_OFFSET
    const data_ptr = mapped_memory.ptr + FIRST_ALLOC_OFFSET;

    // Read the parent's shared memory base address from the first allocation
    const parent_base_addr_ptr: *align(1) const u64 = @ptrCast(data_ptr);
    const parent_base_addr = parent_base_addr_ptr.*;

    // Read the expression index (after the u64)
    const expr_idx_ptr: *align(1) const u32 = @ptrCast(data_ptr + @sizeOf(u64));
    const expr_idx = expr_idx_ptr.*;

    // Calculate relocation offset using safer pointer arithmetic
    const child_base_addr = @intFromPtr(mapped_ptr);
    const parent_base_addr_usize = @as(usize, @intCast(parent_base_addr));

    // Use safer arithmetic to avoid overflow in relocate functions
    // Instead of computing a huge offset, we'll pass the base addresses directly
    const offset = if (child_base_addr >= parent_base_addr_usize)
        @as(isize, @intCast(child_base_addr - parent_base_addr_usize))
    else
        -@as(isize, @intCast(parent_base_addr_usize - child_base_addr));

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        return;
    }

    // Validate that we have enough space for ModuleEnv
    if (fd_info.size < FIRST_ALLOC_OFFSET + MODULE_ENV_OFFSET + @sizeOf(ModuleEnv)) {
        return;
    }

    // The parent stored the ModuleEnv at MODULE_ENV_OFFSET from the first allocation
    const env_addr = @intFromPtr(data_ptr) + MODULE_ENV_OFFSET;
    const env_ptr = @as(*ModuleEnv, @ptrFromInt(env_addr));

    // Set up the environment
    env_ptr.gpa = std.heap.page_allocator;
    env_ptr.relocate(offset);

    // Also relocate the source and module_name strings manually
    if (env_ptr.source.len > 0) {
        const old_source_ptr = @intFromPtr(env_ptr.source.ptr);
        const new_source_ptr = @as(isize, @intCast(old_source_ptr)) + offset;
        env_ptr.source.ptr = @ptrFromInt(@as(usize, @intCast(new_source_ptr)));
    }

    if (env_ptr.module_name.len > 0) {
        const old_module_ptr = @intFromPtr(env_ptr.module_name.ptr);
        const new_module_ptr = @as(isize, @intCast(old_module_ptr)) + offset;
        env_ptr.module_name.ptr = @ptrFromInt(@as(usize, @intCast(new_module_ptr)));
    }

    // Set up the interpreter infrastructure
    var eval_stack = try stack.Stack.initCapacity(std.heap.page_allocator, 64 * 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(env_ptr, &env_ptr.types);
    defer layout_cache.deinit();

    var interpreter = try eval.Interpreter.init(
        std.heap.page_allocator,
        env_ptr,
        &eval_stack,
        &layout_cache,
        &env_ptr.types,
    );
    interpreter.initRocOpsEnv();
    defer interpreter.deinit();

    // Enable tracing to stderr
    interpreter.startTrace(std.io.getStdErr().writer().any());

    const expr_idx_enum: ModuleEnv.Expr.Idx = @enumFromInt(expr_idx);

    // Check if the expression is a closure by getting its layout
    const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_idx_enum));
    const layout_idx = layout_cache.addTypeVar(expr_var) catch {
        return;
    };
    const expr_layout = layout_cache.getLayout(layout_idx);

    // Debug the layout and arguments

    // Handle closure calls
    if (expr_layout.tag == .closure and arg_ptr != null) {
        const closure_expr = env_ptr.store.getExpr(expr_idx_enum);
        const lambda_expr = switch (closure_expr) {
            .e_closure => |closure_data| env_ptr.store.getExpr(closure_data.lambda_idx),
            .e_lambda => closure_expr,
            else => {
                std.log.err("Windows evaluation: Expected closure or lambda expression, got: {s}", .{@tagName(closure_expr)});
                return;
            },
        };

        const param_patterns = switch (lambda_expr) {
            .e_lambda => |lambda_data| env_ptr.store.slicePatterns(lambda_data.args),
            else => {
                std.log.err("Windows evaluation: Expected lambda expression, got: {s}", .{@tagName(lambda_expr)});
                return;
            },
        };

        const param_count = param_patterns.len;

        // Validate parameter count in direct evaluation too
        validateParameterCount(param_count) catch {
            return;
        };

        if (param_count > 0) {
            // Push arguments onto the stack
            if (param_count == 1) {
                // Single parameter - direct push
                const param_pat_idx = param_patterns[0];
                const param_var: types.Var = @enumFromInt(@intFromEnum(param_pat_idx));
                const param_layout_idx = layout_cache.addTypeVar(param_var) catch {
                    return;
                };
                const param_layout = layout_cache.getLayout(param_layout_idx);

                const dest_ptr = interpreter.pushStackValue(param_layout) catch {
                    return;
                };
                const size = layout_cache.layoutSize(param_layout);
                const src = @as([*]u8, @ptrCast(arg_ptr))[0..size];
                const dest = @as([*]u8, @ptrCast(dest_ptr))[0..size];
                @memcpy(dest, src);
            } else {
                // Multiple parameters
                var param_vars_buf: [MAX_CLOSURE_PARAMS]types.Var = undefined;
                std.debug.assert(param_count <= param_vars_buf.len);
                var idx: usize = 0;
                while (idx < param_count) : (idx += 1) {
                    const pat_idx = param_patterns[idx];
                    param_vars_buf[idx] = @enumFromInt(@intFromEnum(pat_idx));
                }

                // Compute element layouts
                var elem_layouts: [MAX_CLOSURE_PARAMS]layout.Layout = undefined;
                var i_build: usize = 0;
                while (i_build < param_count) : (i_build += 1) {
                    const v = param_vars_buf[i_build];
                    const idx_v = try layout_cache.addTypeVar(v);
                    elem_layouts[i_build] = layout_cache.getLayout(idx_v);
                }

                // Compute offsets
                var running_offset: usize = 0;
                var offsets: [MAX_CLOSURE_PARAMS]usize = undefined;
                i_build = 0;
                while (i_build < param_count) : (i_build += 1) {
                    const el = elem_layouts[i_build];
                    const el_align = el.alignment(base.target.Target.native.target_usize);
                    const mask = el_align.toByteUnits() - 1;
                    if ((running_offset & mask) != 0) {
                        running_offset = (running_offset + mask) & ~mask;
                    }
                    offsets[i_build] = running_offset;
                    running_offset += layout_cache.layoutSize(el);
                }

                // Copy each element
                var i_copy: usize = 0;
                while (i_copy < param_count) : (i_copy += 1) {
                    const elem_layout = elem_layouts[i_copy];
                    const elem_size = layout_cache.layoutSize(elem_layout);
                    const elem_offset = offsets[i_copy];

                    const dest_ptr = interpreter.pushStackValue(elem_layout) catch {
                        return;
                    };
                    // Calculate total tuple size for bounds checking
                    const total_tuple_size = if (i_copy == 0) running_offset else running_offset;
                    if (elem_offset + elem_size > total_tuple_size) {
                        return;
                    }
                    const src = @as([*]u8, @ptrCast(arg_ptr))[elem_offset .. elem_offset + elem_size];
                    const dest = @as([*]u8, @ptrCast(dest_ptr))[0..elem_size];
                    @memcpy(dest, src);
                }
            }
        }

        // Call the closure and get the result
        const closure_result = interpreter.callClosureWithStackArgs(expr_idx_enum, @intCast(param_count)) catch |err| {
            std.log.err("Windows evaluation: Closure call failed: {s}", .{@errorName(err)});
            return;
        };

        // Copy result to ret_ptr based on its type (not as a string)
        copyResultToRetPtr(closure_result, &layout_cache, ret_ptr);

        return;
    } else {
        // Evaluate non-closure expression with enhanced error reporting
        const stack_result = try interpreter.eval(expr_idx_enum);

        // Copy result to ret_ptr based on its type
        copyResultToRetPtr(stack_result, &layout_cache, ret_ptr);
    }
}

fn evaluateFromPosixSharedMemoryDirect(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) !void {
    _ = ops; // Currently unused but may be needed for RocOps callbacks
    const fd_info = readFdInfo() catch |err| {
        std.log.err("POSIX: Failed to read fd info: {s}", .{@errorName(err)});
        return;
    };
    defer std.heap.page_allocator.free(fd_info.fd_str);

    const shm_fd = std.fmt.parseInt(c_int, fd_info.fd_str, 10) catch |err| {
        std.log.err("POSIX: Failed to parse fd from '{s}': {s}", .{ fd_info.fd_str, @errorName(err) });
        return;
    };

    defer _ = posix.close(shm_fd);

    // Map the shared memory with read/write permissions and the exact size from the file
    const mapped_ptr = posix.mmap(
        null,
        fd_info.size,
        0x01 | 0x02, // PROT_READ | PROT_WRITE
        0x0001, // MAP_SHARED
        shm_fd,
        0,
    ) orelse {
        std.log.err("POSIX: Failed to map shared memory (size: {})", .{fd_info.size});
        return;
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..fd_info.size];
    defer _ = posix.munmap(mapped_ptr, fd_info.size);

    // The first allocation in SharedMemoryAllocator starts at offset FIRST_ALLOC_OFFSET
    const data_ptr = mapped_memory.ptr + FIRST_ALLOC_OFFSET;

    // Read the parent's shared memory base address from the first allocation
    const parent_base_addr_ptr: *align(1) const u64 = @ptrCast(data_ptr);
    const parent_base_addr = parent_base_addr_ptr.*;

    // Read the expression index (after the u64)
    const expr_idx_ptr: *align(1) const u32 = @ptrCast(data_ptr + @sizeOf(u64));
    const expr_idx = expr_idx_ptr.*;

    // Calculate relocation offset - parent stored base address, child needs to compare with its base
    const child_base_addr = @intFromPtr(mapped_ptr); // mapped_ptr is the base address of child's mapping
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));

    // The parent stored the ModuleEnv at MODULE_ENV_OFFSET from the first allocation
    const env_addr = @intFromPtr(data_ptr) + MODULE_ENV_OFFSET;
    const env_ptr = @as(*ModuleEnv, @ptrFromInt(env_addr));

    // Set up the environment
    env_ptr.gpa = std.heap.page_allocator;
    env_ptr.relocate(offset);

    // Also relocate the source and module_name strings manually
    if (env_ptr.source.len > 0) {
        const old_source_ptr = @intFromPtr(env_ptr.source.ptr);
        const new_source_ptr = @as(isize, @intCast(old_source_ptr)) + offset;
        env_ptr.source.ptr = @ptrFromInt(@as(usize, @intCast(new_source_ptr)));
    }

    if (env_ptr.module_name.len > 0) {
        const old_module_ptr = @intFromPtr(env_ptr.module_name.ptr);
        const new_module_ptr = @as(isize, @intCast(old_module_ptr)) + offset;
        env_ptr.module_name.ptr = @ptrFromInt(@as(usize, @intCast(new_module_ptr)));
    }

    // Set up the interpreter infrastructure
    var eval_stack = try stack.Stack.initCapacity(std.heap.page_allocator, 64 * 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(env_ptr, &env_ptr.types);
    defer layout_cache.deinit();

    var interpreter = eval.Interpreter.init(
        std.heap.page_allocator,
        env_ptr,
        &eval_stack,
        &layout_cache,
        &env_ptr.types,
    ) catch {
        return;
    };
    interpreter.initRocOpsEnv();
    defer interpreter.deinit();

    // Enable tracing to stderr
    interpreter.startTrace(std.io.getStdErr().writer().any());

    const expr_idx_enum: ModuleEnv.Expr.Idx = @enumFromInt(expr_idx);

    // Check if the expression is a closure by getting its layout
    const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_idx_enum));
    const layout_idx = layout_cache.addTypeVar(expr_var) catch {
        return;
    };
    const expr_layout = layout_cache.getLayout(layout_idx);

    // Handle closure calls
    if (expr_layout.tag == .closure and arg_ptr != null) {
        const closure_expr = env_ptr.store.getExpr(expr_idx_enum);
        const lambda_expr = switch (closure_expr) {
            .e_closure => |closure_data| env_ptr.store.getExpr(closure_data.lambda_idx),
            .e_lambda => closure_expr,
            else => {
                std.log.err("POSIX evaluation: Expected closure or lambda expression, got: {s}", .{@tagName(closure_expr)});
                return;
            },
        };

        const param_patterns = switch (lambda_expr) {
            .e_lambda => |lambda_data| env_ptr.store.slicePatterns(lambda_data.args),
            else => {
                std.log.err("POSIX evaluation: Expected lambda expression, got: {s}", .{@tagName(lambda_expr)});
                return;
            },
        };

        const param_count = param_patterns.len;

        // Validate parameter count in direct evaluation too
        validateParameterCount(param_count) catch {
            return;
        };

        if (param_count > 0) {
            // Push arguments onto the stack
            if (param_count == 1) {
                // Single parameter case
                const p0 = param_patterns[0];
                const arg0_var: types.Var = @enumFromInt(@intFromEnum(p0));
                const arg0_layout = blk: {
                    const added = layout_cache.addTypeVar(arg0_var) catch {
                        return;
                    };
                    break :blk layout_cache.getLayout(added);
                };
                const size_bytes = layout_cache.layoutSize(arg0_layout);
                const dest_ptr = interpreter.pushStackValue(arg0_layout) catch {
                    return;
                };
                // Use safe copy with bounds checking for single parameter in direct evaluation
                safeCopyArgument(arg_ptr, dest_ptr, 0, size_bytes, size_bytes) catch {
                    return;
                };
            } else {
                // Multiple parameters
                var param_vars_buf: [MAX_CLOSURE_PARAMS]types.Var = undefined;
                std.debug.assert(param_count <= param_vars_buf.len);
                var idx: usize = 0;
                while (idx < param_count) : (idx += 1) {
                    const pat_idx = param_patterns[idx];
                    param_vars_buf[idx] = @enumFromInt(@intFromEnum(pat_idx));
                }

                // Compute element layouts
                var elem_layouts: [MAX_CLOSURE_PARAMS]layout.Layout = undefined;
                var i_build: usize = 0;
                while (i_build < param_count) : (i_build += 1) {
                    const v = param_vars_buf[i_build];
                    const idx_v = layout_cache.addTypeVar(v) catch {
                        return;
                    };
                    elem_layouts[i_build] = layout_cache.getLayout(idx_v);
                }

                // Compute offsets
                var running_offset: usize = 0;
                var offsets: [MAX_CLOSURE_PARAMS]usize = undefined;
                i_build = 0;
                while (i_build < param_count) : (i_build += 1) {
                    const el = elem_layouts[i_build];
                    const el_align = el.alignment(base.target.Target.native.target_usize);
                    const mask = el_align.toByteUnits() - 1;
                    if ((running_offset & mask) != 0) {
                        running_offset = (running_offset + mask) & ~mask;
                    }
                    offsets[i_build] = running_offset;
                    running_offset += layout_cache.layoutSize(el);
                }

                // Copy each element
                var i_copy: usize = 0;
                while (i_copy < param_count) : (i_copy += 1) {
                    const elem_layout = elem_layouts[i_copy];
                    const elem_size = layout_cache.layoutSize(elem_layout);
                    const elem_offset = offsets[i_copy];

                    const dest_ptr = interpreter.pushStackValue(elem_layout) catch {
                        return;
                    };
                    // Calculate total tuple size for bounds checking
                    const total_tuple_size = if (i_copy == 0) running_offset else running_offset;

                    // Use safe copy with bounds checking in direct evaluation
                    safeCopyArgument(arg_ptr, dest_ptr, elem_offset, elem_size, total_tuple_size) catch {
                        return;
                    };
                }
            }
        }

        // Call the closure
        const closure_result = interpreter.callClosureWithStackArgs(expr_idx_enum, @intCast(param_count)) catch {
            return;
        };

        // Copy result to ret_ptr based on its type
        copyResultToRetPtr(closure_result, &layout_cache, ret_ptr);
    } else {
        // Evaluate non-closure expression
        const stack_result = interpreter.eval(expr_idx_enum) catch {
            return;
        };

        // Copy result to ret_ptr based on its type
        copyResultToRetPtr(stack_result, &layout_cache, ret_ptr);
    }
}

fn copyResultToRetPtr(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, ret_ptr: *anyopaque) void {
    if (stack_result.ptr == null) {
        std.log.warn("Stack result pointer is null, cannot copy result", .{});
        return;
    }

    const result_size = layout_cache.layoutSize(stack_result.layout);
    if (result_size > 0) {
        const src = @as([*]u8, @ptrCast(stack_result.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(ret_ptr))[0..result_size];
        @memcpy(dst, src);
    }
}

fn createRocStrFromData(ops: *builtins.host_abi.RocOps, string_data: [*]u8, string_length: usize) RocStr {
    // For small strings, we can create them directly
    if (string_length <= @sizeOf(RocStr) - 1) {
        var result = RocStr.empty();
        @memcpy(result.asU8ptrMut()[0..string_length], string_data[0..string_length]);
        result.setLen(string_length);
        return result;
    }

    // For larger strings, allocate memory using RocOps
    const alignment = @alignOf(usize);
    var roc_alloc = builtins.host_abi.RocAlloc{
        .alignment = alignment,
        .length = string_length,
        .answer = undefined,
    };
    ops.roc_alloc(&roc_alloc, ops.env);
    const alloc_ptr = roc_alloc.answer;

    // Create a RocStr with the allocated memory
    const result = RocStr{
        .bytes = @ptrCast(alloc_ptr),
        .length = string_length,
        .capacity_or_alloc_ptr = string_length,
    };

    // Copy the data into the allocated memory
    @memcpy(@as([*]u8, @ptrCast(alloc_ptr)), string_data[0..string_length]);

    return result;
}

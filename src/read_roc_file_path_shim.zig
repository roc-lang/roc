//! A shim to read the ModuleEnv from shared memory for the interpreter

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const compile = @import("compile");
const types = @import("types");
const eval = @import("eval/interpreter.zig");
const stack = @import("eval/stack.zig");
const layout_store = @import("layout/store.zig");

const RocStr = builtins.str.RocStr;
const ModuleEnv = compile.ModuleEnv;

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
    extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: LPVOID) BOOL;
    extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
    extern "kernel32" fn GetFileSizeEx(hFile: HANDLE, lpFileSize: *i64) BOOL;

    const FILE_MAP_READ = 0x0004;
} else struct {};

/// Info read from the coordination file
const FdInfo = struct {
    fd_str: []u8,
    size: usize,
};

/// Read the fd/handle and size from the filesystem-based communication mechanism
fn readFdInfo() !FdInfo {
    // Get our own executable path
    const exe_path = try std.fs.selfExePathAlloc(std.heap.page_allocator);
    defer std.heap.page_allocator.free(exe_path);

    // Get the directory containing our executable (should be "roc-tmp-<random>")
    const exe_dir = std.fs.path.dirname(exe_path) orelse return error.InvalidExePath;
    const dir_basename = std.fs.path.basename(exe_dir);

    // Verify it has the expected prefix
    if (!std.mem.startsWith(u8, dir_basename, "roc-tmp-")) {
        return error.UnexpectedDirName;
    }

    // Construct the fd file path by appending .txt to the directory path
    // First, remove any trailing slashes from exe_dir
    var dir_path = exe_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.txt", .{dir_path});
    defer std.heap.page_allocator.free(fd_file_path);

    // Read the fd and size from the file
    const fd_file = std.fs.cwd().openFile(fd_file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.FdFileNotFound,
        else => return err,
    };
    defer fd_file.close();

    const content = try fd_file.readToEndAlloc(std.heap.page_allocator, 128);
    defer std.heap.page_allocator.free(content);

    // Don't delete the fd file yet - let the parent process clean it up
    // std.fs.cwd().deleteFile(fd_file_path) catch {};

    // Parse the content: first line is fd, second line is size
    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    const fd_line = lines.next() orelse return error.InvalidFileFormat;
    const size_line = lines.next() orelse return error.InvalidFileFormat;

    const fd_str = try std.heap.page_allocator.dupe(u8, std.mem.trim(u8, fd_line, " \r\t"));
    const size = try std.fmt.parseInt(usize, std.mem.trim(u8, size_line, " \r\t"), 10);

    return FdInfo{
        .fd_str = fd_str,
        .size = size,
    };
}

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][ModuleEnv data]
export fn roc_entrypoint(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque) callconv(.C) void {
    // Use the appropriate evaluation function based on platform
    const result = if (is_windows)
        evaluateFromWindowsSharedMemory(ops)
    else
        evaluateFromPosixSharedMemory(ops);

    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(ret_ptr));
    roc_str_ptr.* = result;
}

fn evaluateFromWindowsSharedMemory(ops: *builtins.host_abi.RocOps) RocStr {
    const fd_info = readFdInfo() catch {
        return RocStr.empty();
    };
    defer std.heap.page_allocator.free(fd_info.fd_str);

    const handle_int = std.fmt.parseInt(usize, fd_info.fd_str, 10) catch {
        return RocStr.empty();
    };

    const shm_handle = @as(windows.HANDLE, @ptrFromInt(handle_int));

    // Map the shared memory with the exact size
    const mapped_ptr = windows.MapViewOfFile(shm_handle, windows.FILE_MAP_READ, 0, 0, fd_info.size) orelse {
        return RocStr.empty();
    };
    defer _ = windows.UnmapViewOfFile(mapped_ptr);

    // Read the parent address from the beginning
    const parent_addr_ptr: *align(1) const u64 = @ptrCast(mapped_ptr);
    const parent_addr = parent_addr_ptr.*;

    // Read the expression index (after the u64)
    const expr_idx_ptr: *align(1) const u32 = @ptrCast(@as([*]u8, @ptrCast(mapped_ptr)) + @sizeOf(u64));
    const expr_idx = expr_idx_ptr.*;

    // Calculate relocation offset
    const child_addr = @intFromPtr(mapped_ptr);
    const offset = @as(isize, @intCast(child_addr)) - @as(isize, @intCast(parent_addr));

    // Get pointer to ModuleEnv (after the u64 and u32)
    const env_ptr = @as(*ModuleEnv, @ptrCast(@alignCast(@as([*]u8, @ptrCast(mapped_ptr)) + @sizeOf(u64) + @sizeOf(u32))));

    // Relocate the ModuleEnv
    env_ptr.relocate(offset);

    // Set up the real interpreter infrastructure
    var eval_stack = stack.Stack.initCapacity(std.heap.page_allocator, 64 * 1024) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Stack init error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer eval_stack.deinit();

    var layout_cache = layout_store.Store.init(env_ptr, &env_ptr.types) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Layout cache error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer layout_cache.deinit();

    var interpreter = eval.Interpreter.init(
        std.heap.page_allocator,
        env_ptr,
        &eval_stack,
        &layout_cache,
        &env_ptr.types,
    ) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Interpreter init error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer interpreter.deinit();

    // Evaluate using the REAL interpreter
    const expr_idx_enum: ModuleEnv.Expr.Idx = @enumFromInt(expr_idx);
    const stack_result = interpreter.eval(expr_idx_enum) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Evaluation error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };

    // Format the result based on its layout
    var buf: [1024]u8 = undefined;
    const result_str = blk: {
        if (stack_result.layout.tag == .scalar) {
            if (stack_result.layout.data.scalar.tag == .int) {
                const precision = stack_result.layout.data.scalar.data.int;
                const int_val = eval.readIntFromMemory(@ptrCast(stack_result.ptr.?), precision);
                break :blk std.fmt.bufPrint(&buf, "{}", .{int_val}) catch "Error formatting";
            } else if (stack_result.layout.data.scalar.tag == .bool) {
                const bool_val = @as(*const bool, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                break :blk std.fmt.bufPrint(&buf, "{}", .{bool_val}) catch "Error formatting";
            } else if (stack_result.layout.data.scalar.tag == .frac) {
                const float_precision = stack_result.layout.data.scalar.data.frac;
                switch (float_precision) {
                    .f32 => {
                        const float_val = @as(*const f32, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                        break :blk std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch "Error formatting";
                    },
                    .f64 => {
                        const float_val = @as(*const f64, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                        break :blk std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch "Error formatting";
                    },
                    .dec => {
                        // Decimal is a 128-bit fixed-point number with 18 decimal places
                        const dec_ptr = @as(*const builtins.dec.RocDec, @ptrCast(@alignCast(stack_result.ptr.?)));
                        const dec_str = dec_ptr.to_str(ops);
                        defer dec_str.decref(ops);
                        break :blk std.fmt.bufPrint(&buf, "{s}", .{dec_str.asSlice()}) catch "Error formatting";
                    },
                }
            } else if (stack_result.layout.data.scalar.tag == .str) {
                const str_ptr = @as(*const RocStr, @ptrCast(@alignCast(stack_result.ptr.?)));
                // Format the string with quotes to show it's a string
                break :blk std.fmt.bufPrint(&buf, "\"{s}\"", .{str_ptr.asSlice()}) catch "Error formatting";
            } else if (stack_result.layout.data.scalar.tag == .opaque_ptr) {
                const ptr_val = @as(*const ?*anyopaque, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                if (ptr_val) |ptr| {
                    break :blk std.fmt.bufPrint(&buf, "<opaque pointer: 0x{x}>", .{@intFromPtr(ptr)}) catch "Error formatting";
                } else {
                    break :blk std.fmt.bufPrint(&buf, "<null opaque pointer>", .{}) catch "Error formatting";
                }
            } else {
                // This should never happen as we've covered all scalar types
                break :blk std.fmt.bufPrint(&buf, "Unknown scalar type: {}", .{stack_result.layout.data.scalar.tag}) catch "Error";
            }
        } else if (stack_result.layout.tag == .record) {
            // Format record as: { field1: value1, field2: value2, ... }
            // For now, just show we have a record with its size
            const record_data = layout_cache.getRecordData(stack_result.layout.data.record.idx);
            const num_fields = record_data.fields.count;
            break :blk std.fmt.bufPrint(&buf, "<record with {} fields, size {} bytes>", .{ num_fields, record_data.size }) catch "Error";
        } else if (stack_result.layout.tag == .tuple) {
            // Format tuple as: (elem1, elem2, ...)
            // For now, just show we have a tuple with its size
            const tuple_data = layout_cache.getTupleData(stack_result.layout.data.tuple.idx);
            const num_elems = tuple_data.fields.count;
            break :blk std.fmt.bufPrint(&buf, "<tuple with {} elements, size {} bytes>", .{ num_elems, tuple_data.size }) catch "Error";
        } else if (stack_result.layout.tag == .list) {
            // Format list as: [elem1, elem2, ...]
            // Lists are more complex as they have runtime length
            const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
            break :blk std.fmt.bufPrint(&buf, "<list with {} elements>", .{list_ptr.len()}) catch "Error";
        } else if (stack_result.layout.tag == .list_of_zst) {
            // List of zero-sized types (e.g., List({}))
            const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
            break :blk std.fmt.bufPrint(&buf, "<list of {} zero-sized elements>", .{list_ptr.len()}) catch "Error";
        } else if (stack_result.layout.tag == .box) {
            // Box is a heap-allocated value
            break :blk std.fmt.bufPrint(&buf, "<box>", .{}) catch "Error";
        } else if (stack_result.layout.tag == .box_of_zst) {
            // Box of zero-sized type
            break :blk std.fmt.bufPrint(&buf, "<box of zero-sized type>", .{}) catch "Error";
        } else if (stack_result.layout.tag == .closure) {
            // Function closure
            break :blk std.fmt.bufPrint(&buf, "<closure>", .{}) catch "Error";
        } else {
            // Should never happen if we've covered all layout tags
            break :blk std.fmt.bufPrint(&buf, "Unknown layout type: {}", .{stack_result.layout.tag}) catch "Error";
        }
    };

    return createRocStrFromData(ops, @constCast(result_str.ptr), result_str.len);
}

fn evaluateFromPosixSharedMemory(ops: *builtins.host_abi.RocOps) RocStr {
    const fd_info = readFdInfo() catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "readFdInfo error: {}", .{err}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer std.heap.page_allocator.free(fd_info.fd_str);

    const shm_fd = std.fmt.parseInt(c_int, fd_info.fd_str, 10) catch {
        return RocStr.empty();
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
        return RocStr.empty();
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..fd_info.size];
    defer _ = posix.munmap(mapped_ptr, fd_info.size);

    // The first allocation in SharedMemoryAllocator starts at offset 504 (0x1f8)
    // This is 8 bytes before the end of the 512-byte header, likely for alignment
    const first_alloc_offset = 504;
    const data_ptr = mapped_memory.ptr + first_alloc_offset;

    // Read the parent address from the first allocation
    const parent_addr_ptr: *align(1) const u64 = @ptrCast(data_ptr);
    const parent_addr = parent_addr_ptr.*;

    // Read the expression index (after the u64)
    const expr_idx_ptr: *align(1) const u32 = @ptrCast(data_ptr + @sizeOf(u64));
    const expr_idx = expr_idx_ptr.*;

    // Calculate relocation offset (both addresses should be for the data area, not the full mapping)
    const child_addr = @intFromPtr(data_ptr);
    const offset = @as(isize, @intCast(child_addr)) - @as(isize, @intCast(parent_addr));

    // The parent stored the ModuleEnv at offset 0x10 from the first allocation
    // (after the u64 parent address and u32 expr index)
    const module_env_offset = 0x10; // 8 bytes for u64, 4 bytes for u32, 4 bytes padding
    const env_addr = @intFromPtr(data_ptr) + module_env_offset;

    const env_ptr = @as(*ModuleEnv, @ptrFromInt(env_addr));

    // IMPORTANT: Before relocating, we need to set the gpa field to a valid allocator
    // The relocate function expects gpa to be valid and won't relocate it
    env_ptr.gpa = std.heap.page_allocator;

    // Relocate the ModuleEnv - this will adjust all the internal pointers
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

    // Set up the real interpreter infrastructure
    var eval_stack = stack.Stack.initCapacity(std.heap.page_allocator, 64 * 1024) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Stack init error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer eval_stack.deinit();

    var layout_cache = layout_store.Store.init(env_ptr, &env_ptr.types) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Layout cache error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer layout_cache.deinit();

    var interpreter = eval.Interpreter.init(
        std.heap.page_allocator,
        env_ptr,
        &eval_stack,
        &layout_cache,
        &env_ptr.types,
    ) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Interpreter init error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };
    defer interpreter.deinit();

    // Evaluate using the REAL interpreter
    const expr_idx_enum: ModuleEnv.Expr.Idx = @enumFromInt(expr_idx);
    const stack_result = interpreter.eval(expr_idx_enum) catch |err| {
        var buf: [256]u8 = undefined;
        const err_str = std.fmt.bufPrint(&buf, "Evaluation error: {s}", .{@errorName(err)}) catch "Error";
        return createRocStrFromData(ops, @as([*]u8, @ptrCast(&buf)), err_str.len);
    };

    // Format the result based on its layout
    var buf: [1024]u8 = undefined;
    const result_str = blk: {
        if (stack_result.layout.tag == .scalar) {
            if (stack_result.layout.data.scalar.tag == .int) {
                const precision = stack_result.layout.data.scalar.data.int;
                const int_val = eval.readIntFromMemory(@ptrCast(stack_result.ptr.?), precision);
                break :blk std.fmt.bufPrint(&buf, "{}", .{int_val}) catch "Error formatting";
            } else if (stack_result.layout.data.scalar.tag == .bool) {
                const bool_val = @as(*const bool, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                break :blk std.fmt.bufPrint(&buf, "{}", .{bool_val}) catch "Error formatting";
            } else if (stack_result.layout.data.scalar.tag == .frac) {
                const float_precision = stack_result.layout.data.scalar.data.frac;
                switch (float_precision) {
                    .f32 => {
                        const float_val = @as(*const f32, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                        break :blk std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch "Error formatting";
                    },
                    .f64 => {
                        const float_val = @as(*const f64, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                        break :blk std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch "Error formatting";
                    },
                    .dec => {
                        // Decimal is a 128-bit fixed-point number with 18 decimal places
                        const dec_ptr = @as(*const builtins.dec.RocDec, @ptrCast(@alignCast(stack_result.ptr.?)));
                        const dec_str = dec_ptr.to_str(ops);
                        defer dec_str.decref(ops);
                        break :blk std.fmt.bufPrint(&buf, "{s}", .{dec_str.asSlice()}) catch "Error formatting";
                    },
                }
            } else if (stack_result.layout.data.scalar.tag == .str) {
                const str_ptr = @as(*const RocStr, @ptrCast(@alignCast(stack_result.ptr.?)));
                // Format the string with quotes to show it's a string
                break :blk std.fmt.bufPrint(&buf, "\"{s}\"", .{str_ptr.asSlice()}) catch "Error formatting";
            } else if (stack_result.layout.data.scalar.tag == .opaque_ptr) {
                const ptr_val = @as(*const ?*anyopaque, @ptrCast(@alignCast(stack_result.ptr.?))).*;
                if (ptr_val) |ptr| {
                    break :blk std.fmt.bufPrint(&buf, "<opaque pointer: 0x{x}>", .{@intFromPtr(ptr)}) catch "Error formatting";
                } else {
                    break :blk std.fmt.bufPrint(&buf, "<null opaque pointer>", .{}) catch "Error formatting";
                }
            } else {
                // This should never happen as we've covered all scalar types
                break :blk std.fmt.bufPrint(&buf, "Unknown scalar type: {}", .{stack_result.layout.data.scalar.tag}) catch "Error";
            }
        } else if (stack_result.layout.tag == .record) {
            // Format record as: { field1: value1, field2: value2, ... }
            // For now, just show we have a record with its size
            const record_data = layout_cache.getRecordData(stack_result.layout.data.record.idx);
            const num_fields = record_data.fields.count;
            break :blk std.fmt.bufPrint(&buf, "<record with {} fields, size {} bytes>", .{ num_fields, record_data.size }) catch "Error";
        } else if (stack_result.layout.tag == .tuple) {
            // Format tuple as: (elem1, elem2, ...)
            // For now, just show we have a tuple with its size
            const tuple_data = layout_cache.getTupleData(stack_result.layout.data.tuple.idx);
            const num_elems = tuple_data.fields.count;
            break :blk std.fmt.bufPrint(&buf, "<tuple with {} elements, size {} bytes>", .{ num_elems, tuple_data.size }) catch "Error";
        } else if (stack_result.layout.tag == .list) {
            // Format list as: [elem1, elem2, ...]
            // Lists are more complex as they have runtime length
            const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
            break :blk std.fmt.bufPrint(&buf, "<list with {} elements>", .{list_ptr.len()}) catch "Error";
        } else if (stack_result.layout.tag == .list_of_zst) {
            // List of zero-sized types (e.g., List({}))
            const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
            break :blk std.fmt.bufPrint(&buf, "<list of {} zero-sized elements>", .{list_ptr.len()}) catch "Error";
        } else if (stack_result.layout.tag == .box) {
            // Box is a heap-allocated value
            break :blk std.fmt.bufPrint(&buf, "<box>", .{}) catch "Error";
        } else if (stack_result.layout.tag == .box_of_zst) {
            // Box of zero-sized type
            break :blk std.fmt.bufPrint(&buf, "<box of zero-sized type>", .{}) catch "Error";
        } else if (stack_result.layout.tag == .closure) {
            // Function closure
            break :blk std.fmt.bufPrint(&buf, "<closure>", .{}) catch "Error";
        } else {
            // Should never happen if we've covered all layout tags
            break :blk std.fmt.bufPrint(&buf, "Unknown layout type: {}", .{stack_result.layout.tag}) catch "Error";
        }
    };

    return createRocStrFromData(ops, @constCast(result_str.ptr), result_str.len);
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

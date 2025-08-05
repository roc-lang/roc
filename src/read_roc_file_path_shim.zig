//! A shim to read the ModuleEnv from shared memory for the interpreter
//! Refactored to use clean abstractions for cross-platform shared memory,
//! memory safety, and interpreter integration.

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

// New abstractions
const shared_memory = @import("ipc/shared_memory.zig");
const safe_memory = @import("base/safe_memory.zig");
const format_utils = @import("eval/format.zig");
const closure_args = @import("eval/closure_args.zig");

const RocStr = builtins.str.RocStr;
const ModuleEnv = compile.ModuleEnv;
const SharedMemoryHandle = shared_memory.SharedMemoryHandle;
const Interpreter = eval.Interpreter;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset
const MODULE_ENV_OFFSET = 0x10; // 8 bytes for u64, 4 bytes for u32, 4 bytes padding
const RESULT_BUFFER_SIZE = format_utils.RESULT_BUFFER_SIZE;

/// Comprehensive error handling for the shim
const ShimError = error{
    SharedMemoryError,
    InterpreterSetupFailed,
    EvaluationFailed,
    MemoryLayoutInvalid,
    ModuleEnvSetupFailed,
    UnexpectedClosureStructure,
    StackOverflow,
    OutOfMemory,
    ZeroSizedType,
    TypeContainedMismatch,
    InvalidRecordExtension,
    BugUnboxedFlexVar,
    BugUnboxedRigidVar,
    UnsupportedResultType,
} || shared_memory.SharedMemoryError || closure_args.ClosureArgError || safe_memory.MemoryError;

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][ModuleEnv data]
export fn roc_entrypoint(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void {
    evaluateFromSharedMemory(ops, ret_ptr, arg_ptr) catch |err| {
        std.log.err("Error evaluating from shared memory: {s}", .{@errorName(err)});
    };
}

/// Cross-platform shared memory evaluation
fn evaluateFromSharedMemory(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {

    // Read shared memory coordination info
    var fd_info = SharedMemoryHandle.readFdInfo(std.heap.page_allocator) catch |err| {
        std.log.err("Failed to read shared memory coordination info: {s}", .{@errorName(err)});
        return error.SharedMemoryError;
    };
    defer fd_info.deinit(std.heap.page_allocator);

    // Create shared memory handle and map memory
    var shm_handle = SharedMemoryHandle.fromFdInfo(fd_info) catch |err| {
        std.log.err("Failed to create shared memory handle: {s}", .{@errorName(err)});
        return error.SharedMemoryError;
    };
    defer {
        shm_handle.unmap();
        SharedMemoryHandle.closeHandle(shm_handle.platform_handle);
    }

    // Set up ModuleEnv from shared memory
    const env_ptr = try setupModuleEnv(shm_handle);

    // Set up interpreter infrastructure
    var interpreter = try createInterpreter(env_ptr);
    defer interpreter.deinit();

    // Get expression info from shared memory
    const expr_idx: ModuleEnv.Expr.Idx = @enumFromInt(
        safe_memory.safeRead(u32, shm_handle.ptr, FIRST_ALLOC_OFFSET + @sizeOf(u64), shm_handle.size) catch {
            return error.MemoryLayoutInvalid;
        },
    );

    // TODO push args onto the stack for the closure evaluation
    _ = arg_ptr;

    // Evaluate the expression
    try interpreter.evaluateExpression(expr_idx, ret_ptr, ops);
}

/// Set up ModuleEnv from shared memory with proper relocation
fn setupModuleEnv(shm_handle: SharedMemoryHandle) ShimError!*ModuleEnv {
    // Validate memory layout
    const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(u64) + @sizeOf(u32) + MODULE_ENV_OFFSET + @sizeOf(ModuleEnv);
    if (shm_handle.size < min_required_size) {
        std.log.err("Invalid memory layout: size {} is too small (minimum required: {})", .{ shm_handle.size, min_required_size });
        return error.MemoryLayoutInvalid;
    }

    // Read parent's shared memory base address and calculate relocation offset
    const data_ptr = @as([*]u8, @ptrCast(shm_handle.ptr)) + FIRST_ALLOC_OFFSET;
    const parent_base_addr = safe_memory.safeRead(u64, shm_handle.ptr, FIRST_ALLOC_OFFSET, shm_handle.size) catch {
        return error.MemoryLayoutInvalid;
    };

    // Calculate relocation offset
    const child_base_addr = if (builtin.target.os.tag == .windows)
        @intFromPtr(shm_handle.ptr)
    else
        @intFromPtr(shm_handle.ptr);

    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));

    std.log.warn("DEBUG ModuleEnv setup:", .{});
    std.log.warn("  shared memory size: {}", .{shm_handle.size});
    std.log.warn("  parent_base_addr: 0x{x}", .{parent_base_addr});
    std.log.warn("  child_base_addr: 0x{x}", .{child_base_addr});
    std.log.warn("  relocation offset: {}", .{offset});

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        std.log.err("Relocation offset too large: {}", .{offset});
        return error.ModuleEnvSetupFailed;
    }

    // Get ModuleEnv pointer and set it up
    const env_addr = @intFromPtr(data_ptr) + MODULE_ENV_OFFSET;
    const env_ptr = @as(*ModuleEnv, @ptrFromInt(env_addr));

    std.log.warn("  env_ptr address: 0x{x}", .{@intFromPtr(env_ptr)});

    // Set up the environment
    env_ptr.gpa = std.heap.page_allocator;

    std.log.warn("DEBUG: About to relocate ModuleEnv", .{});
    env_ptr.relocate(offset);
    std.log.warn("DEBUG: ModuleEnv relocation completed", .{});

    // Relocate strings manually if they exist
    if (env_ptr.source.len > 0) {
        const old_source_ptr = @intFromPtr(env_ptr.source.ptr);
        const new_source_ptr = @as(isize, @intCast(old_source_ptr)) + offset;
        env_ptr.source.ptr = @ptrFromInt(@as(usize, @intCast(new_source_ptr)));
        std.log.warn("  relocated source string: 0x{x} -> 0x{x}", .{ old_source_ptr, @intFromPtr(env_ptr.source.ptr) });
    }

    if (env_ptr.module_name.len > 0) {
        const old_module_ptr = @intFromPtr(env_ptr.module_name.ptr);
        const new_module_ptr = @as(isize, @intCast(old_module_ptr)) + offset;
        env_ptr.module_name.ptr = @ptrFromInt(@as(usize, @intCast(new_module_ptr)));
        std.log.warn("  relocated module_name string: 0x{x} -> 0x{x}", .{ old_module_ptr, @intFromPtr(env_ptr.module_name.ptr) });
    }

    std.log.warn("DEBUG: ModuleEnv setup completed successfully", .{});
    return env_ptr;
}

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv) ShimError!Interpreter {
    const allocator = std.heap.page_allocator;

    // Allocate stack on heap to ensure stable address
    const eval_stack = allocator.create(stack.Stack) catch {
        std.log.err("Stack allocation failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer allocator.destroy(eval_stack);

    eval_stack.* = stack.Stack.initCapacity(allocator, 64 * 1024) catch {
        std.log.err("Stack initialization failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer eval_stack.deinit();

    // Allocate layout cache on heap to ensure stable address
    const layout_cache = allocator.create(layout_store.Store) catch {
        std.log.err("Layout cache allocation failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer allocator.destroy(layout_cache);

    layout_cache.* = layout_store.Store.init(env_ptr, &env_ptr.types) catch {
        std.log.err("Layout cache initialization failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer layout_cache.deinit();

    // Initialize the interpreter with pointers to the heap-allocated objects
    var interpreter = eval.Interpreter.init(
        allocator,
        env_ptr,
        eval_stack,
        layout_cache,
        &env_ptr.types,
    ) catch {
        std.log.err("Interpreter initialization failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer interpreter.deinit();

    interpreter.initRocOpsEnv();

    // Enable tracing to stderr
    interpreter.startTrace(std.io.getStdErr().writer().any());

    return interpreter;
}
// /// Temporary: Original pushClosureArguments function for debugging
// fn pushClosureArgumentsOriginal(interpreter: *eval.Interpreter, layout_cache: *layout_store.Store, param_patterns: []const ModuleEnv.Pattern.Idx, arg_ptr: ?*anyopaque) !void {
//     const param_count = param_patterns.len;

//     if (param_count == 1) {
//         // Single parameter case
//         const p0 = param_patterns[0];
//         const arg0_var: types.Var = @enumFromInt(@intFromEnum(p0));
//         const arg0_layout = blk: {
//             const added = layout_cache.addTypeVar(arg0_var) catch |err| {
//                 return err;
//             };
//             break :blk layout_cache.getLayout(added);
//         };
//         const size_bytes = layout_cache.layoutSize(arg0_layout);
//         const dest_ptr = interpreter.pushStackValue(arg0_layout) catch |err| {
//             return err;
//         };

//         // Use safe copy with bounds checking for single parameter
//         std.log.warn("  single param: size_bytes = {}", .{size_bytes});
//         safe_memory.safeCopyArgument(arg_ptr, dest_ptr, 0, size_bytes, size_bytes) catch |err| {
//             std.log.err("  safeCopyArgument failed: {s}", .{@errorName(err)});
//             return err;
//         };
//     } else {
//         // Multiple parameters case - inline the original logic
//         var param_vars_buf: [8]types.Var = undefined;
//         var idx: usize = 0;
//         while (idx < param_count) : (idx += 1) {
//             const pat_idx = param_patterns[idx];
//             param_vars_buf[idx] = @enumFromInt(@intFromEnum(pat_idx));
//         }

//         // Compute element layouts
//         var elem_layouts: [8]layout.Layout = undefined;
//         var i_build: usize = 0;
//         while (i_build < param_count) : (i_build += 1) {
//             const v = param_vars_buf[i_build];
//             const idx_v = layout_cache.addTypeVar(v) catch |err| {
//                 return err;
//             };
//             elem_layouts[i_build] = layout_cache.getLayout(idx_v);
//         }

//         // Compute offsets using native target alignment rules
//         var offsets: [8]usize = undefined;
//         var running_offset: usize = 0;
//         var j: usize = 0;
//         while (j < param_count) : (j += 1) {
//             const elem_layout = elem_layouts[j];
//             const elem_align = elem_layout.alignment(base.target.Target.native.target_usize);
//             const elem_size = layout_cache.layoutSize(elem_layout);
//             const mask = elem_align.toByteUnits() - 1;

//             std.log.warn("  layout {}: align={}, size={}, running_offset={}", .{ j, elem_align.toByteUnits(), elem_size, running_offset });

//             // Apply alignment
//             if ((running_offset & mask) != 0) {
//                 running_offset = (running_offset + mask) & ~mask;
//             }

//             offsets[j] = running_offset;
//             running_offset += elem_size;

//             std.log.warn("  layout {}: final offset={}, new running_offset={}", .{ j, offsets[j], running_offset });
//         }

//         // Copy each element from arg_ptr + computed offset to the interpreter stack
//         std.log.warn("  multiple params: total_size = {}", .{running_offset});
//         var k: usize = 0;
//         while (k < param_count) : (k += 1) {
//             const elem_layout = elem_layouts[k];
//             const elem_size = layout_cache.layoutSize(elem_layout);
//             const elem_offset = offsets[k];

//             std.log.warn("  param {}: offset={}, size={}", .{ k, elem_offset, elem_size });

//             std.log.warn("  about to pushStackValue for param {}", .{k});
//             const dest_ptr = interpreter.pushStackValue(elem_layout) catch |err| {
//                 std.log.err("  pushStackValue failed for param {}: {s}", .{ k, @errorName(err) });
//                 return err;
//             };
//             std.log.warn("  pushStackValue succeeded for param {}, dest_ptr = 0x{x}", .{ k, @intFromPtr(dest_ptr) });

//             // Use safe copy with bounds checking
//             const arg_addr = if (arg_ptr) |ptr| @intFromPtr(ptr) else 0;
//             std.log.warn("  about to safeCopyArgument: arg_ptr=0x{x}, dest_ptr=0x{x}, offset={}, size={}, max_size={}", .{ arg_addr, @intFromPtr(dest_ptr), elem_offset, elem_size, running_offset });
//             safe_memory.safeCopyArgument(arg_ptr, dest_ptr, elem_offset, elem_size, running_offset) catch |err| {
//                 std.log.err("  safeCopyArgument failed for param {}: {s}", .{ k, @errorName(err) });
//                 return err;
//             };
//             std.log.warn("  safeCopyArgument succeeded for param {}", .{k});
//         }
//     }
// }

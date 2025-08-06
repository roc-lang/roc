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

const SharedMemoryAllocator = @import("SharedMemoryAllocator.zig");

const RocStr = builtins.str.RocStr;
const ModuleEnv = compile.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const Interpreter = eval.Interpreter;
const safe_memory = base.safe_memory;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset
const MODULE_ENV_OFFSET = 0x10; // 8 bytes for u64, 4 bytes for u32, 4 bytes padding

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
} || safe_memory.MemoryError || eval.EvalError;

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][ModuleEnv data]
export fn roc_entrypoint(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void {
    evaluateFromSharedMemory(ops, ret_ptr, arg_ptr) catch |err| {
        std.log.err("Error evaluating from shared memory: {s}", .{@errorName(err)});
    };
}

/// Cross-platform shared memory evaluation
fn evaluateFromSharedMemory(roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    const allocator = std.heap.page_allocator;

    // Get page size
    const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;

    // Create shared memory allocator from coordination info
    var shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
        std.log.err("Failed to create shared memory allocator: {s}", .{@errorName(err)});
        return error.SharedMemoryError;
    };
    defer shm.deinit(allocator);

    // Set up ModuleEnv from shared memory
    const env_ptr = try setupModuleEnv(&shm);

    // Set up interpreter infrastructure
    var interpreter = try createInterpreter(env_ptr);
    defer interpreter.deinit(roc_ops);

    // Get expression info from shared memory
    const base_ptr = shm.getBasePtr();
    const expr_idx: ModuleEnv.Expr.Idx = @enumFromInt(
        safe_memory.safeRead(u32, base_ptr, FIRST_ALLOC_OFFSET + @sizeOf(u64), shm.total_size) catch {
            return error.MemoryLayoutInvalid;
        },
    );

    // Evaluate the expression (with optional arguments)
    try interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr);
}

/// Set up ModuleEnv from shared memory with proper relocation
fn setupModuleEnv(shm: *SharedMemoryAllocator) ShimError!*ModuleEnv {
    // Validate memory layout
    const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(u64) + @sizeOf(u32) + MODULE_ENV_OFFSET + @sizeOf(ModuleEnv);
    if (shm.total_size < min_required_size) {
        std.log.err("Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size });
        return error.MemoryLayoutInvalid;
    }

    // Get base pointer
    const base_ptr = shm.getBasePtr();

    // Read parent's shared memory base address and calculate relocation offset
    const data_ptr = base_ptr + FIRST_ALLOC_OFFSET;
    const parent_base_addr = safe_memory.safeRead(u64, base_ptr, FIRST_ALLOC_OFFSET, shm.total_size) catch {
        return error.MemoryLayoutInvalid;
    };

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        std.log.err("Relocation offset too large: {}", .{offset});
        return error.ModuleEnvSetupFailed;
    }

    // Get ModuleEnv pointer and set it up
    const env_addr = @intFromPtr(data_ptr) + MODULE_ENV_OFFSET;
    const env_ptr = @as(*ModuleEnv, @ptrFromInt(env_addr));

    // Set up the environment
    env_ptr.gpa = std.heap.page_allocator;
    env_ptr.relocate(offset);

    // Relocate strings manually if they exist
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

    // Enable tracing to stderr
    interpreter.startTrace(std.io.getStdErr().writer().any());

    return interpreter;
}

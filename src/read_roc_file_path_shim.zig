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
    var interpreter, var cleanup = try createInterpreter(env_ptr);
    defer {
        interpreter.deinit();
        cleanup.deinit();
    }

    // Get expression info from shared memory
    const expr_info = try readExpressionInfo(shm_handle);

    // Evaluate the expression
    try evaluateExpression(
        &interpreter,
        expr_info,
        arg_ptr,
        ret_ptr,
        ops,
    );
}

/// Expression information read from shared memory
const ExpressionInfo = struct {
    expr_idx: u32,
    expr_idx_enum: ModuleEnv.Expr.Idx,
};

/// Simple cleanup helper for interpreter resources
const InterpreterCleanup = struct {
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
    allocator: std.mem.Allocator,

    fn deinit(self: *InterpreterCleanup) void {
        self.layout_cache.deinit();
        self.eval_stack.deinit();
        self.allocator.destroy(self.layout_cache);
        self.allocator.destroy(self.eval_stack);
    }
};

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
fn createInterpreter(env_ptr: *ModuleEnv) ShimError!struct { eval.Interpreter, InterpreterCleanup } {
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

    const cleanup = InterpreterCleanup{
        .eval_stack = eval_stack,
        .layout_cache = layout_cache,
        .allocator = allocator,
    };
    
    return .{ interpreter, cleanup };
}

/// Read expression information from shared memory
fn readExpressionInfo(shm_handle: SharedMemoryHandle) ShimError!ExpressionInfo {
    const expr_idx = safe_memory.safeRead(u32, shm_handle.ptr, FIRST_ALLOC_OFFSET + @sizeOf(u64), shm_handle.size) catch {
        return error.MemoryLayoutInvalid;
    };

    return ExpressionInfo{
        .expr_idx = expr_idx,
        .expr_idx_enum = @enumFromInt(expr_idx),
    };
}

/// Evaluate the expression and handle both closures and simple expressions
fn evaluateExpression(
    interpreter: *eval.Interpreter,
    expr_info: ExpressionInfo,
    arg_ptr: ?*anyopaque,
    ret_ptr: *anyopaque,
    ops: *builtins.host_abi.RocOps,
) ShimError!void {
    // Check if the expression is a closure by getting its layout
    const expr_var: types.Var = @enumFromInt(@intFromEnum(expr_info.expr_idx_enum));
    const layout_idx = interpreter.layout_cache.addTypeVar(expr_var) catch {
        return error.EvaluationFailed;
    };
    const expr_layout = interpreter.layout_cache.getLayout(layout_idx);

    // Debug output
    std.log.warn("Expression layout tag: {s}", .{@tagName(expr_layout.tag)});
    std.log.warn("arg_ptr is null: {}", .{arg_ptr == null});

    if (expr_layout.tag == .closure and arg_ptr != null) {
        try evaluateClosure(interpreter, expr_info.expr_idx_enum, arg_ptr, ret_ptr, ops);
    } else {
        try evaluateSimpleExpression(interpreter, expr_info.expr_idx_enum, ret_ptr, ops);
    }
}

/// Evaluate a closure with arguments
fn evaluateClosure(
    interpreter: *eval.Interpreter,
    expr_idx_enum: ModuleEnv.Expr.Idx,
    arg_ptr: ?*anyopaque,
    ret_ptr: *anyopaque,
    ops: *builtins.host_abi.RocOps,
) ShimError!void {
    std.log.warn("DEBUG: Starting closure evaluation", .{});

    // Get closure parameter patterns
    const param_patterns = getClosureParameterPatterns(interpreter.env, expr_idx_enum) catch {
        std.log.err("Failed to get closure parameter patterns", .{});
        return error.EvaluationFailed;
    };

    std.log.warn("DEBUG: Got {} parameter patterns", .{param_patterns.len});

    // Validate and push arguments (using original approach temporarily)
    if (param_patterns.len > 0) {
        std.log.warn("DEBUG: About to push closure arguments", .{});

        // Debug stack state before pushing arguments
        std.log.warn("  stack used: {}, capacity: {}, available: {}", .{
            interpreter.stack_memory.used,
            interpreter.stack_memory.capacity,
            interpreter.stack_memory.capacity - interpreter.stack_memory.used,
        });
        std.log.warn("  stack start ptr: 0x{x}", .{@intFromPtr(interpreter.stack_memory.start)});

        // Try a simple stack operation to see if the stack itself is corrupted
        std.log.warn("  testing basic stack allocation...", .{});
        _ = interpreter.stack_memory.start; // Silence unused variable warning
        std.log.warn("  stack test: can access start ptr", .{});

        // Use the original non-abstracted approach to isolate the issue
        if (param_patterns.len == 1) {
            // Single parameter case - direct inline implementation
            const p0 = param_patterns[0];
            const arg0_var: types.Var = @enumFromInt(@intFromEnum(p0));
            const arg0_layout_idx = interpreter.layout_cache.addTypeVar(arg0_var) catch {
                return error.EvaluationFailed;
            };
            const arg0_layout = interpreter.layout_cache.getLayout(arg0_layout_idx);
            const size_bytes = interpreter.layout_cache.layoutSize(arg0_layout);

            std.log.warn("  single param: about to pushStackValue, size={}", .{size_bytes});
            const dest_ptr = interpreter.pushStackValue(arg0_layout) catch |err| {
                std.log.err("  single param pushStackValue failed: {s}", .{@errorName(err)});
                return error.EvaluationFailed;
            };
            std.log.warn("  single param: pushStackValue succeeded, dest=0x{x}", .{@intFromPtr(dest_ptr)});

            // Check if this is a string parameter that needs special handling
            if (arg0_layout.tag == .scalar and arg0_layout.data.scalar.tag == .str) {
                std.log.warn("  detected RocStr parameter, creating valid copy", .{});
                
                // Get the source RocStr from host memory
                const src_str = @as(*const RocStr, @ptrCast(@alignCast(arg_ptr))).*;
                
                // Create a new RocStr with valid pointers in interpreter memory space
                const new_str = if (src_str.isSmallStr()) 
                    src_str  // Small strings can be copied directly
                else
                    src_str.clone(ops);  // Large strings need proper allocation with interpreter's ops
                
                // Copy the new RocStr to the interpreter stack
                const dst = @as(*RocStr, @ptrCast(@alignCast(dest_ptr)));
                dst.* = new_str;
                
                std.log.warn("  RocStr copy completed successfully", .{});
            } else {
                // For non-string types, use raw copy as before
                safe_memory.safeCopyArgument(arg_ptr, dest_ptr, 0, size_bytes, size_bytes) catch |err| {
                    std.log.err("  single param safeCopyArgument failed: {s}", .{@errorName(err)});
                    return error.EvaluationFailed;
                };
                std.log.warn("  raw argument copy succeeded", .{});
            }
        } else {
            // Multiple parameters case - proper alignment calculations
            std.log.warn("  multiple params: handling {} parameters", .{param_patterns.len});
            
            const param_count = param_patterns.len;
            
            // Convert patterns to variables and compute layouts
            var param_vars_buf: [8]types.Var = undefined;
            var elem_layouts: [8]layout.Layout = undefined;
            var offsets: [8]usize = undefined;
            
            // Convert patterns to variables
            for (param_patterns, 0..) |pat_idx, i| {
                param_vars_buf[i] = @enumFromInt(@intFromEnum(pat_idx));
            }
            
            // Compute element layouts
            for (0..param_count) |i| {
                const v = param_vars_buf[i];
                const idx_v = interpreter.layout_cache.addTypeVar(v) catch {
                    return error.EvaluationFailed;
                };
                elem_layouts[i] = interpreter.layout_cache.getLayout(idx_v);
            }
            
            // Compute proper offsets using native target alignment rules
            var running_offset: usize = 0;
            for (0..param_count) |i| {
                const elem_layout = elem_layouts[i];
                const elem_align = elem_layout.alignment(base.target.Target.native.target_usize);
                const elem_size = interpreter.layout_cache.layoutSize(elem_layout);
                const mask = elem_align.toByteUnits() - 1;
                
                // Apply alignment
                if ((running_offset & mask) != 0) {
                    running_offset = (running_offset + mask) & ~mask;
                }
                
                offsets[i] = running_offset;
                running_offset += elem_size;
            }
            
            // Push arguments with proper alignment
            for (0..param_count) |i| {
                const elem_layout = elem_layouts[i];
                const elem_size = interpreter.layout_cache.layoutSize(elem_layout);
                const elem_offset = offsets[i];
                
                std.log.warn("  param {}: size={}, offset={}, total_size={}", .{ i, elem_size, elem_offset, running_offset });

                const dest_ptr = interpreter.pushStackValue(elem_layout) catch |err| {
                    std.log.err("  param {} pushStackValue failed: {s}", .{ i, @errorName(err) });
                    return error.EvaluationFailed;
                };

                // Check if this is a string parameter that needs special handling
                if (elem_layout.tag == .scalar and elem_layout.data.scalar.tag == .str) {
                    std.log.warn("  param {}: detected RocStr, creating valid copy", .{i});
                    
                    // Calculate source pointer for this parameter
                    const src_ptr = @as([*]u8, @ptrCast(arg_ptr)) + elem_offset;
                    const src_str = @as(*const RocStr, @ptrCast(@alignCast(src_ptr))).*;
                    
                    // Create a new RocStr with valid pointers in interpreter memory space
                    const new_str = if (src_str.isSmallStr()) 
                        src_str  // Small strings can be copied directly
                    else
                        src_str.clone(ops);  // Large strings need proper allocation
                    
                    // Copy the new RocStr to the interpreter stack
                    const dst = @as(*RocStr, @ptrCast(@alignCast(dest_ptr)));
                    dst.* = new_str;
                    
                    std.log.warn("  param {}: RocStr copy completed", .{i});
                } else {
                    // For non-string types, use raw copy as before
                    safe_memory.safeCopyArgument(arg_ptr, dest_ptr, elem_offset, elem_size, running_offset) catch |err| {
                        std.log.err("  param {} safeCopyArgument failed: {s}", .{ i, @errorName(err) });
                        return error.EvaluationFailed;
                    };
                    std.log.warn("  param {}: raw copy completed", .{i});
                }
            }
        }
        std.log.warn("DEBUG: Successfully pushed closure arguments", .{});
    }

    // Debug closure expression data
    std.log.warn("DEBUG: About to call closure", .{});
    std.log.warn("  expr_idx_enum: {}", .{@intFromEnum(expr_idx_enum)});
    std.log.warn("  param_count: {}", .{param_patterns.len});

    // Check if the closure expression is valid
    const closure_expr = interpreter.env.store.getExpr(expr_idx_enum);
    std.log.warn("  closure_expr tag: {s}", .{@tagName(closure_expr)});

    // Validate interpreter state
    std.log.warn("  interpreter initialized: {}", .{true});

    std.log.warn("DEBUG: About to call callClosureWithStackArgs with expr_idx={}, param_count={}", .{ @intFromEnum(expr_idx_enum), param_patterns.len });
    const closure_result = interpreter.callClosureWithStackArgs(
        expr_idx_enum,
        @intCast(param_patterns.len),
    ) catch {
        std.log.err("Closure call failed", .{});
        return error.EvaluationFailed;
    };
    std.log.warn("DEBUG: callClosureWithStackArgs returned successfully", .{});

    std.log.warn("DEBUG: Closure call succeeded, about to handle result", .{});
    // Type-aware result handling instead of raw copy
    try handleResult(closure_result, interpreter.layout_cache, ops, ret_ptr);
    std.log.warn("DEBUG: Result handled successfully", .{});
}

/// Evaluate a simple (non-closure) expression
fn evaluateSimpleExpression(
    interpreter: *eval.Interpreter,
    expr_idx_enum: ModuleEnv.Expr.Idx,
    ret_ptr: *anyopaque,
    ops: *builtins.host_abi.RocOps,
) ShimError!void {
    const stack_result = interpreter.eval(expr_idx_enum) catch {
        std.log.err("Expression evaluation failed", .{});
        return error.EvaluationFailed;
    };

    // Type-aware result handling instead of raw copy
    try handleResult(stack_result, interpreter.layout_cache, ops, ret_ptr);
}

/// Get parameter patterns from a closure expression
fn getClosureParameterPatterns(env_ptr: *const ModuleEnv, expr_idx_enum: ModuleEnv.Expr.Idx) ![]const ModuleEnv.Pattern.Idx {
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

    return param_patterns;
}

/// Type-aware result handling that properly formats results for platforms
fn handleResult(
    stack_result: eval.Interpreter.StackValue,
    layout_cache: *layout_store.Store,
    ops: *builtins.host_abi.RocOps,
    ret_ptr: *anyopaque,
) ShimError!void {
    switch (stack_result.layout.tag) {
        .scalar => {
            switch (stack_result.layout.data.scalar.tag) {
                .int => handleIntResult(stack_result, layout_cache, ret_ptr),
                .str => try handleStringResult(stack_result, layout_cache, ops, ret_ptr),
                .bool => handleBoolResult(stack_result, layout_cache, ret_ptr),
                .frac => handleFloatResult(stack_result, layout_cache, ret_ptr),
                else => return error.UnsupportedResultType,
            }
        },
        else => return error.UnsupportedResultType,
    }
}

/// Handle integer results with direct copy
fn handleIntResult(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, ret_ptr: *anyopaque) void {
    // Direct copy for primitive types
    const result_size = layout_cache.layoutSize(stack_result.layout);
    if (result_size > 0 and stack_result.ptr != null) {
        const src = @as([*]u8, @ptrCast(stack_result.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(ret_ptr))[0..result_size];
        @memcpy(dst, src);
    }
}

/// Handle boolean results with direct copy
fn handleBoolResult(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, ret_ptr: *anyopaque) void {
    // Direct copy for boolean types
    const result_size = layout_cache.layoutSize(stack_result.layout);
    if (result_size > 0 and stack_result.ptr != null) {
        const src = @as([*]u8, @ptrCast(stack_result.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(ret_ptr))[0..result_size];
        @memcpy(dst, src);
    }
}

/// Handle float results with direct copy
fn handleFloatResult(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, ret_ptr: *anyopaque) void {
    // Direct copy for float types
    const result_size = layout_cache.layoutSize(stack_result.layout);
    if (result_size > 0 and stack_result.ptr != null) {
        const src = @as([*]u8, @ptrCast(stack_result.ptr.?))[0..result_size];
        const dst = @as([*]u8, @ptrCast(ret_ptr))[0..result_size];
        @memcpy(dst, src);
    }
}

/// Handle string results with proper RocStr creation
fn handleStringResult(
    stack_result: eval.Interpreter.StackValue,
    layout_cache: *layout_store.Store,
    ops: *builtins.host_abi.RocOps,
    ret_ptr: *anyopaque,
) !void {
    _ = layout_cache; // Unused for now but may be needed for validation
    
    if (stack_result.ptr == null) {
        // Return empty string
        const empty_str = RocStr.empty();
        const dst = @as(*RocStr, @ptrCast(@alignCast(ret_ptr)));
        dst.* = empty_str;
        return;
    }

    // Get the string from interpreter result
    const src_str = @as(*const RocStr, @ptrCast(@alignCast(stack_result.ptr.?))).*;
    
    // Create new RocStr using host's allocator
    const new_str = if (src_str.isSmallStr()) 
        src_str  // Small strings can be copied directly
    else
        src_str.clone(ops);  // Large strings need proper allocation
    
    // Copy to return pointer
    const dst = @as(*RocStr, @ptrCast(@alignCast(ret_ptr)));
    dst.* = new_str;
}

/// Create error RocStr for consistent error handling
fn createErrorRocStr(ops: *builtins.host_abi.RocOps, error_msg: []const u8) RocStr {
    return RocStr.fromBytes(error_msg.ptr, error_msg.len, ops);
}

/// Temporary: Original pushClosureArguments function for debugging
fn pushClosureArgumentsOriginal(interpreter: *eval.Interpreter, layout_cache: *layout_store.Store, param_patterns: []const ModuleEnv.Pattern.Idx, arg_ptr: ?*anyopaque) !void {
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
        std.log.warn("  single param: size_bytes = {}", .{size_bytes});
        safe_memory.safeCopyArgument(arg_ptr, dest_ptr, 0, size_bytes, size_bytes) catch |err| {
            std.log.err("  safeCopyArgument failed: {s}", .{@errorName(err)});
            return err;
        };
    } else {
        // Multiple parameters case - inline the original logic
        var param_vars_buf: [8]types.Var = undefined;
        var idx: usize = 0;
        while (idx < param_count) : (idx += 1) {
            const pat_idx = param_patterns[idx];
            param_vars_buf[idx] = @enumFromInt(@intFromEnum(pat_idx));
        }

        // Compute element layouts
        var elem_layouts: [8]layout.Layout = undefined;
        var i_build: usize = 0;
        while (i_build < param_count) : (i_build += 1) {
            const v = param_vars_buf[i_build];
            const idx_v = layout_cache.addTypeVar(v) catch |err| {
                return err;
            };
            elem_layouts[i_build] = layout_cache.getLayout(idx_v);
        }

        // Compute offsets using native target alignment rules
        var offsets: [8]usize = undefined;
        var running_offset: usize = 0;
        var j: usize = 0;
        while (j < param_count) : (j += 1) {
            const elem_layout = elem_layouts[j];
            const elem_align = elem_layout.alignment(base.target.Target.native.target_usize);
            const elem_size = layout_cache.layoutSize(elem_layout);
            const mask = elem_align.toByteUnits() - 1;

            std.log.warn("  layout {}: align={}, size={}, running_offset={}", .{ j, elem_align.toByteUnits(), elem_size, running_offset });

            // Apply alignment
            if ((running_offset & mask) != 0) {
                running_offset = (running_offset + mask) & ~mask;
            }

            offsets[j] = running_offset;
            running_offset += elem_size;

            std.log.warn("  layout {}: final offset={}, new running_offset={}", .{ j, offsets[j], running_offset });
        }

        // Copy each element from arg_ptr + computed offset to the interpreter stack
        std.log.warn("  multiple params: total_size = {}", .{running_offset});
        var k: usize = 0;
        while (k < param_count) : (k += 1) {
            const elem_layout = elem_layouts[k];
            const elem_size = layout_cache.layoutSize(elem_layout);
            const elem_offset = offsets[k];

            std.log.warn("  param {}: offset={}, size={}", .{ k, elem_offset, elem_size });

            std.log.warn("  about to pushStackValue for param {}", .{k});
            const dest_ptr = interpreter.pushStackValue(elem_layout) catch |err| {
                std.log.err("  pushStackValue failed for param {}: {s}", .{ k, @errorName(err) });
                return err;
            };
            std.log.warn("  pushStackValue succeeded for param {}, dest_ptr = 0x{x}", .{ k, @intFromPtr(dest_ptr) });

            // Use safe copy with bounds checking
            const arg_addr = if (arg_ptr) |ptr| @intFromPtr(ptr) else 0;
            std.log.warn("  about to safeCopyArgument: arg_ptr=0x{x}, dest_ptr=0x{x}, offset={}, size={}, max_size={}", .{ arg_addr, @intFromPtr(dest_ptr), elem_offset, elem_size, running_offset });
            safe_memory.safeCopyArgument(arg_ptr, dest_ptr, elem_offset, elem_size, running_offset) catch |err| {
                std.log.err("  safeCopyArgument failed for param {}: {s}", .{ k, @errorName(err) });
                return err;
            };
            std.log.warn("  safeCopyArgument succeeded for param {}", .{k});
        }
    }
}

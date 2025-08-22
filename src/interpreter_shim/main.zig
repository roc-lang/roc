//! A shim to read the ModuleEnv from shared memory for the interpreter
//! Refactored to use clean abstractions for cross-platform shared memory,
//! memory safety, and interpreter integration.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const eval = @import("eval");
const layout = @import("layout");
const ipc = @import("ipc");

const SharedMemoryAllocator = ipc.SharedMemoryAllocator;
const Stack = eval.Stack;
const LayoutStore = layout.Store;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;
const Interpreter = eval.Interpreter;
const safe_memory = base.safe_memory;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset
const PARENT_ADDR_OFFSET = FIRST_ALLOC_OFFSET;
const ENTRY_COUNT_OFFSET = PARENT_ADDR_OFFSET + @sizeOf(u64);
const MODULE_ENV_ADDR_OFFSET = ENTRY_COUNT_OFFSET + @sizeOf(u32);
const DEF_INDICES_OFFSET = std.mem.alignForward(usize, MODULE_ENV_ADDR_OFFSET + @sizeOf(u64), @sizeOf(u32));

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
    InvalidEntryIndex,
} || safe_memory.MemoryError || eval.EvalError;

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][u32 entry_count][u64 env_addr][u32[] def_indices][ModuleEnv data]
export fn roc_entrypoint(entry_idx: u32, ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void {
    std.log.debug("roc_entrypoint called with entry_idx: {}", .{entry_idx});
    
    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        std.log.err("Error evaluating from shared memory: {s}", .{@errorName(err)});
    };
}

/// Cross-platform shared memory evaluation
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
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

    // Get expression info from shared memory using entry_idx
    const base_ptr = shm.getBasePtr();
    
    // Read and validate entry count
    const entry_count = safe_memory.safeRead(u32, base_ptr, ENTRY_COUNT_OFFSET, shm.total_size) catch {
        return error.MemoryLayoutInvalid;
    };
    if (entry_idx >= entry_count) {
        std.log.err("Invalid entry_idx {} >= entry_count {}", .{entry_idx, entry_count});
        return error.InvalidEntryIndex;
    }

    // Read target definition index from exports
    const def_offset = DEF_INDICES_OFFSET + entry_idx * @sizeOf(u32);
    const def_idx: CIR.Def.Idx = @enumFromInt(
        safe_memory.safeRead(u32, base_ptr, def_offset, shm.total_size) catch {
            return error.MemoryLayoutInvalid;
        },
    );

    // Get the definition and extract its expression
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;
    
    std.log.debug("NEW LOGIC: entry_idx {} -> def_idx {} -> expr_idx {}", .{entry_idx, @intFromEnum(def_idx), @intFromEnum(expr_idx)});
    
    // Evaluate the expression (with optional arguments)
    try interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr);
}

/// Set up ModuleEnv from shared memory with proper relocation
fn setupModuleEnv(shm: *SharedMemoryAllocator) ShimError!*ModuleEnv {
    // Get base pointer
    const base_ptr = shm.getBasePtr();
    
    // Read entry count for validation
    const entry_count = safe_memory.safeRead(u32, base_ptr, ENTRY_COUNT_OFFSET, shm.total_size) catch {
        return error.MemoryLayoutInvalid;
    };
    
    // Read the ModuleEnv address directly
    const parent_env_addr = safe_memory.safeRead(u64, base_ptr, MODULE_ENV_ADDR_OFFSET, shm.total_size) catch {
        return error.MemoryLayoutInvalid;
    };
    
    std.log.debug("Entry count: {}, Parent ModuleEnv addr: 0x{x}, Total size: {}", .{entry_count, parent_env_addr, shm.total_size});

    // Read parent's shared memory base address and calculate relocation offset
    const parent_base_addr = safe_memory.safeRead(u64, base_ptr, FIRST_ALLOC_OFFSET, shm.total_size) catch {
        return error.MemoryLayoutInvalid;
    };

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));
    
    std.log.debug("Relocation: parent_addr=0x{x}, child_addr=0x{x}, offset={}", .{parent_base_addr, child_base_addr, offset});

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        std.log.err("Relocation offset too large: {}", .{offset});
        return error.ModuleEnvSetupFailed;
    }

    // Calculate the ModuleEnv address in child process address space
    const child_env_addr = @as(usize, @intCast(@as(isize, @intCast(parent_env_addr)) + offset));
    const env_ptr: *ModuleEnv = @ptrFromInt(child_env_addr);
    
    std.log.debug("ModuleEnv: parent_env=0x{x}, child_base=0x{x}, offset={}, child_env=0x{x}", .{parent_env_addr, child_base_addr, offset, child_env_addr});

    // Set up the environment
    env_ptr.gpa = std.heap.page_allocator;
    
    // Add some basic validation before relocation
    std.log.debug("ModuleEnv validation: gpa set, attempting relocation with offset {}", .{offset});
    
    // Try to relocate - this might fail with negative offsets
    env_ptr.relocate(offset);

    // TODO Relocate strings manually if they exist
    // if (env_ptr.source.len > 0) {
    //     const old_source_ptr = @intFromPtr(env_ptr.source.ptr);
    //     const new_source_ptr = @as(isize, @intCast(old_source_ptr)) + offset;
    //     env_ptr.source.ptr = @ptrFromInt(@as(usize, @intCast(new_source_ptr)));
    // }

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
    const eval_stack = allocator.create(Stack) catch {
        std.log.err("Stack allocation failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer allocator.destroy(eval_stack);

    eval_stack.* = Stack.initCapacity(allocator, 64 * 1024) catch {
        std.log.err("Stack initialization failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer eval_stack.deinit();

    // Allocate layout cache on heap to ensure stable address
    const layout_cache = allocator.create(LayoutStore) catch {
        std.log.err("Layout cache allocation failed", .{});
        return error.InterpreterSetupFailed;
    };
    errdefer allocator.destroy(layout_cache);

    layout_cache.* = LayoutStore.init(env_ptr, &env_ptr.types) catch {
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

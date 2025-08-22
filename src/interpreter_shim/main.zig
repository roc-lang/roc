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
const MODULE_ENV_OFFSET = 0x10; // 8 bytes for u64, 4 bytes for u32, 4 bytes padding

// Header structure that matches the one in main.zig
const Header = struct {
    parent_base_addr: u64,
    entry_count: u32,
    _padding: u32, // Ensure 8-byte alignment
    def_indices_offset: u64,
    module_env_offset: u64,
};

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
/// Expected format in shared memory: [u64 parent_address][u32 entry_count][ModuleEnv data][u32[] def_indices]
export fn roc_entrypoint(entry_idx: u32, ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void {
    std.log.debug("roc_entrypoint called with entry_idx: {}", .{entry_idx});
    
    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        std.log.err("Error evaluating from shared memory: {s}", .{@errorName(err)});
    };
}

// Global shared memory allocator to persist across entrypoint calls
var global_shm: ?SharedMemoryAllocator = null;
var global_env: ?*ModuleEnv = null;

/// Cross-platform shared memory evaluation
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    std.log.debug("evaluateFromSharedMemory: entry_idx={}", .{entry_idx});
    const allocator = std.heap.page_allocator;

    // Reuse global shared memory allocator or create new one
    if (global_shm == null) {
        // Get page size
        const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;
        std.log.debug("evaluateFromSharedMemory: page_size={}", .{page_size});

        // Create shared memory allocator from coordination info
        global_shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
            std.log.err("Failed to create shared memory allocator: {s}", .{@errorName(err)});
            return error.SharedMemoryError;
        };
        std.log.debug("evaluateFromSharedMemory: created shared memory allocator", .{});
    } else {
        std.log.debug("evaluateFromSharedMemory: reusing existing shared memory allocator", .{});
    }
    
    var shm = &global_shm.?;

    // Reuse global ModuleEnv or set it up from shared memory
    if (global_env == null) {
        std.log.debug("evaluateFromSharedMemory: calling setupModuleEnv", .{});
        global_env = try setupModuleEnv(shm);
        std.log.debug("evaluateFromSharedMemory: setupModuleEnv completed", .{});
    } else {
        std.log.debug("evaluateFromSharedMemory: reusing existing ModuleEnv", .{});
    }
    
    const env_ptr = global_env.?;

    // Set up interpreter infrastructure
    std.log.debug("evaluateFromSharedMemory: calling createInterpreter", .{});
    var interpreter = try createInterpreter(env_ptr);
    defer interpreter.deinit(roc_ops);
    std.log.debug("evaluateFromSharedMemory: createInterpreter completed", .{});

    // Get expression info from shared memory using entry_idx
    const base_ptr = shm.getBasePtr();
    std.log.debug("base_ptr: 0x{x}, total_size: {}", .{@intFromPtr(base_ptr), shm.total_size});
    
    // Read the header structure from shared memory
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    std.log.debug("Reading header from address: 0x{x}", .{header_addr});
    std.log.debug("Header alignment check: 0x{x} % {} = {}", .{header_addr, @alignOf(Header), header_addr % @alignOf(Header)});
    
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    std.log.debug("Read header: parent_base_addr=0x{x}, entry_count={}, def_indices_offset={}", .{header_ptr.parent_base_addr, header_ptr.entry_count, header_ptr.def_indices_offset});
    
    if (entry_idx >= header_ptr.entry_count) {
        std.log.err("Invalid entry_idx {} >= entry_count {}", .{entry_idx, header_ptr.entry_count});
        return error.InvalidEntryIndex;
    }
    
    const def_offset = header_ptr.def_indices_offset + entry_idx * @sizeOf(u32);
    std.log.debug("Reading def_idx from offset: {} (0x{x})", .{def_offset, def_offset});
    
    const def_idx_raw = safe_memory.safeRead(u32, base_ptr, def_offset, shm.total_size) catch |err| {
        std.log.err("Failed to read def_idx: {}", .{err});
        return error.MemoryLayoutInvalid;
    };
    std.log.debug("Read def_idx_raw: {}", .{def_idx_raw});
    
    const def_idx: CIR.Def.Idx = @enumFromInt(def_idx_raw);

    // Get the definition and extract its expression
    std.log.debug("Getting definition for def_idx: {}", .{@intFromEnum(def_idx)});
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;
    std.log.debug("Found expression for def: expr_idx={}", .{@intFromEnum(expr_idx)});
    
    // Multi-entrypoint lookup: entry_idx -> def_idx -> expr_idx
    std.log.debug("About to evaluate expression: entry_idx={} -> def_idx={} -> expr_idx={}", .{entry_idx, @intFromEnum(def_idx), @intFromEnum(expr_idx)});
    
    // Evaluate the expression (with optional arguments)
    std.log.debug("Calling interpreter.evaluateExpression", .{});
    try interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr);
    std.log.debug("interpreter.evaluateExpression completed", .{});
}

/// Set up ModuleEnv from shared memory with proper relocation
fn setupModuleEnv(shm: *SharedMemoryAllocator) ShimError!*ModuleEnv {
    // Validate memory layout - we need at least space for the header
    const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(Header);
    if (shm.total_size < min_required_size) {
        std.log.err("Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size });
        return error.MemoryLayoutInvalid;
    }

    // Get base pointer
    const base_ptr = shm.getBasePtr();

    // Read parent's shared memory base address from header and calculate relocation offset
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    std.log.debug("setupModuleEnv: parent_base_addr=0x{x}, child_base_addr=0x{x}", .{parent_base_addr, child_base_addr});
    
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));
    std.log.debug("setupModuleEnv: calculated offset={}", .{offset});

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        std.log.err("Relocation offset too large: {}", .{offset});
        return error.ModuleEnvSetupFailed;
    }

    // Get ModuleEnv pointer from the offset stored in the header
    const env_addr = @intFromPtr(base_ptr) + header_ptr.module_env_offset;
    const env_ptr: *ModuleEnv = @ptrFromInt(env_addr);
    std.log.debug("setupModuleEnv: ModuleEnv at offset {} (0x{x}), address 0x{x}", .{header_ptr.module_env_offset, header_ptr.module_env_offset, env_addr});

    // Set up the environment
    env_ptr.gpa = std.heap.page_allocator;
    std.log.debug("setupModuleEnv: calling env_ptr.relocate with offset={}", .{offset});
    env_ptr.relocate(offset);
    std.log.debug("setupModuleEnv: env_ptr.relocate completed", .{});

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

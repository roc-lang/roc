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

// Global state for shared memory - initialized once per process
var shared_memory_initialized: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);
var global_shm: ?SharedMemoryAllocator = null;
var global_env_ptr: ?*ModuleEnv = null;
var shm_mutex: std.Thread.Mutex = .{};
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
    var buf: [256]u8 = undefined;
    const msg1 = std.fmt.bufPrint(&buf, "DEBUG: roc_entrypoint called with entry_idx={}\n", .{entry_idx}) catch "DEBUG: roc_entrypoint called\n";
    ops.dbg(msg1);
    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        const msg2 = std.fmt.bufPrint(&buf, "Error evaluating from shared memory: {s}", .{@errorName(err)}) catch "Error evaluating from shared memory";
        ops.crash(msg2);
    };
    ops.dbg("roc_entrypoint completed");
}

/// Initialize shared memory and ModuleEnv once per process
fn initializeSharedMemoryOnce(roc_ops: *RocOps) ShimError!void {
    // Fast path: if already initialized, return immediately
    if (shared_memory_initialized.load(.acquire)) {
        return;
    }

    // Slow path: acquire mutex and check again (double-checked locking)
    shm_mutex.lock();
    defer shm_mutex.unlock();
    
    // Check again in case another thread initialized while we were waiting
    if (shared_memory_initialized.load(.acquire)) {
        return;
    }

    roc_ops.dbg("initializing shared memory (once per process)");
    const allocator = std.heap.page_allocator;
    var buf: [256]u8 = undefined;

    // Get page size
    roc_ops.dbg("getting page size");
    const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;
    const msg = std.fmt.bufPrint(&buf, "DEBUG: page_size = {}\n", .{page_size}) catch "DEBUG: page_size\n";
    roc_ops.dbg(msg);

    // Create shared memory allocator from coordination info
    roc_ops.dbg("creating shared memory allocator");
    var shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
        const msg2 = std.fmt.bufPrint(&buf, "Failed to create shared memory allocator: {s}", .{@errorName(err)}) catch "Failed to create shared memory allocator";
        roc_ops.crash(msg2);
        return error.SharedMemoryError;
    };
    roc_ops.dbg("shared memory allocator created");

    // Set up ModuleEnv from shared memory
    roc_ops.dbg("setting up ModuleEnv");
    const env_ptr = try setupModuleEnv(&shm, roc_ops);
    roc_ops.dbg("ModuleEnv setup complete");

    // Store globals
    global_shm = shm;
    global_env_ptr = env_ptr;
    
    // Mark as initialized (release semantics ensure all writes above are visible)
    shared_memory_initialized.store(true, .release);
    roc_ops.dbg("shared memory initialization complete");
}

/// Cross-platform shared memory evaluation
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    roc_ops.dbg("evaluateFromSharedMemory started");
    
    // Initialize shared memory once per process
    try initializeSharedMemoryOnce(roc_ops);
    
    // Use the global shared memory and environment
    const shm = global_shm.?;
    const env_ptr = global_env_ptr.?;

    // Set up interpreter infrastructure (per-call, as it's lightweight)
    roc_ops.dbg("creating interpreter");
    var interpreter = try createInterpreter(env_ptr, roc_ops);
    defer interpreter.deinit(roc_ops);
    roc_ops.dbg("interpreter created");

    // Get expression info from shared memory using entry_idx
    roc_ops.dbg("getting base pointer");
    const base_ptr = shm.getBasePtr();
    var buf: [256]u8 = undefined;
    const ptr_msg = std.fmt.bufPrint(&buf, "DEBUG: base_ptr = {*}\n", .{base_ptr}) catch "DEBUG: base_ptr\n";
    roc_ops.dbg(ptr_msg);

    // Read the header structure from shared memory
    roc_ops.dbg("reading header structure");
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    const hdr_msg = std.fmt.bufPrint(&buf, "DEBUG: header read - entry_count={}, parent_base_addr=0x{x}\n", .{ header_ptr.entry_count, header_ptr.parent_base_addr }) catch "DEBUG: header read\n";
    roc_ops.dbg(hdr_msg);

    if (entry_idx >= header_ptr.entry_count) {
        const err_msg = std.fmt.bufPrint(&buf, "Invalid entry_idx {} >= entry_count {}", .{ entry_idx, header_ptr.entry_count }) catch "Invalid entry_idx";
        roc_ops.crash(err_msg);
        return error.InvalidEntryIndex;
    }

    const idx_msg = std.fmt.bufPrint(&buf, "DEBUG: calculating def_offset for entry_idx={}\n", .{entry_idx}) catch "DEBUG: calculating def_offset\n";
    roc_ops.dbg(idx_msg);
    const def_offset = header_ptr.def_indices_offset + entry_idx * @sizeOf(u32);
    const off_msg = std.fmt.bufPrint(&buf, "DEBUG: def_offset = {}\n", .{def_offset}) catch "DEBUG: def_offset\n";
    roc_ops.dbg(off_msg);

    roc_ops.dbg("reading def_idx_raw");
    const def_idx_raw = safe_memory.safeRead(u32, base_ptr, @intCast(def_offset), shm.total_size) catch |err| {
        const read_err = std.fmt.bufPrint(&buf, "Failed to read def_idx: {}", .{err}) catch "Failed to read def_idx";
        roc_ops.crash(read_err);
        return error.MemoryLayoutInvalid;
    };
    const raw_msg = std.fmt.bufPrint(&buf, "DEBUG: def_idx_raw = {}\n", .{def_idx_raw}) catch "DEBUG: def_idx_raw\n";
    roc_ops.dbg(raw_msg);

    const def_idx: CIR.Def.Idx = @enumFromInt(def_idx_raw);
    const def_msg = std.fmt.bufPrint(&buf, "DEBUG: def_idx = {}\n", .{def_idx}) catch "DEBUG: def_idx\n";
    roc_ops.dbg(def_msg);

    // Get the definition and extract its expression
    roc_ops.dbg("getting definition from store");
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;
    const expr_msg = std.fmt.bufPrint(&buf, "DEBUG: expr_idx = {}\n", .{expr_idx}) catch "DEBUG: expr_idx\n";
    roc_ops.dbg(expr_msg);

    // Evaluate the expression (with optional arguments)
    roc_ops.dbg("about to evaluate expression");
    try interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr);
    roc_ops.dbg("expression evaluation complete");
}

/// Set up ModuleEnv from shared memory with proper relocation
fn setupModuleEnv(shm: *SharedMemoryAllocator, roc_ops: *RocOps) ShimError!*ModuleEnv {
    roc_ops.dbg("setupModuleEnv started");
    // Validate memory layout - we need at least space for the header
    const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(Header);
    if (shm.total_size < min_required_size) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size }) catch "Invalid memory layout";
        roc_ops.crash(msg);
        return error.MemoryLayoutInvalid;
    }
    var buf: [256]u8 = undefined;
    const size_msg = std.fmt.bufPrint(&buf, "DEBUG: memory layout validated, total_size={}\n", .{shm.total_size}) catch "DEBUG: memory layout validated\n";
    roc_ops.dbg(size_msg);

    // Get base pointer
    const base_ptr = shm.getBasePtr();
    const ptr_msg = std.fmt.bufPrint(&buf, "DEBUG: got base_ptr = {*}\n", .{base_ptr}) catch "DEBUG: got base_ptr\n";
    roc_ops.dbg(ptr_msg);

    // Read parent's shared memory base address from header and calculate relocation offset
    roc_ops.dbg("reading header from shared memory");
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;
    const parent_msg = std.fmt.bufPrint(&buf, "DEBUG: parent_base_addr = 0x{x}\n", .{parent_base_addr}) catch "DEBUG: parent_base_addr\n";
    roc_ops.dbg(parent_msg);

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));
    const offset_msg = std.fmt.bufPrint(&buf, "DEBUG: child_base_addr = 0x{x}, offset = {}\n", .{ child_base_addr, offset }) catch "DEBUG: relocation offset\n";
    roc_ops.dbg(offset_msg);

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        const err_msg = std.fmt.bufPrint(&buf, "Relocation offset too large: {}", .{offset}) catch "Relocation offset too large";
        roc_ops.crash(err_msg);
        return error.ModuleEnvSetupFailed;
    }

    // Get ModuleEnv pointer from the offset stored in the header
    roc_ops.dbg("getting ModuleEnv pointer");
    const env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.module_env_offset));
    const env_ptr: *ModuleEnv = @ptrFromInt(env_addr);
    const env_msg = std.fmt.bufPrint(&buf, "DEBUG: env_ptr = {*}\n", .{env_ptr}) catch "DEBUG: env_ptr\n";
    roc_ops.dbg(env_msg);

    // Set up the environment
    roc_ops.dbg("setting up environment");
    env_ptr.gpa = std.heap.page_allocator;
    const reloc_msg = std.fmt.bufPrint(&buf, "DEBUG: about to call relocate with offset={}\n", .{offset}) catch "DEBUG: about to call relocate\n";
    roc_ops.dbg(reloc_msg);
    env_ptr.relocate(offset);
    roc_ops.dbg("relocate completed");

    if (env_ptr.module_name.len > 0) {
        const old_module_ptr = @intFromPtr(env_ptr.module_name.ptr);
        const new_module_ptr = @as(isize, @intCast(old_module_ptr)) + offset;
        env_ptr.module_name.ptr = @ptrFromInt(@as(usize, @intCast(new_module_ptr)));
    }

    return env_ptr;
}

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv, roc_ops: *RocOps) ShimError!Interpreter {
    const allocator = std.heap.page_allocator;

    // Allocate stack on heap to ensure stable address
    const eval_stack = allocator.create(Stack) catch {
        roc_ops.crash("Stack allocation failed");
        return error.InterpreterSetupFailed;
    };
    errdefer allocator.destroy(eval_stack);

    eval_stack.* = Stack.initCapacity(allocator, 64 * 1024) catch {
        roc_ops.crash("Stack initialization failed");
        return error.InterpreterSetupFailed;
    };
    errdefer eval_stack.deinit();

    // Allocate layout cache on heap to ensure stable address
    const layout_cache = allocator.create(LayoutStore) catch {
        roc_ops.crash("Layout cache allocation failed");
        return error.InterpreterSetupFailed;
    };
    errdefer allocator.destroy(layout_cache);

    layout_cache.* = LayoutStore.init(env_ptr, &env_ptr.types) catch {
        roc_ops.crash("Layout cache initialization failed");
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
        roc_ops.crash("Interpreter initialization failed");
        return error.InterpreterSetupFailed;
    };
    errdefer interpreter.deinit();

    // Enable tracing to stderr
    interpreter.startTrace(std.io.getStdErr().writer().any());

    return interpreter;
}

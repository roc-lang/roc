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
const ipc = @import("ipc");

const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

const is_windows = builtin.target.os.tag == .windows;

var stderr_file_writer: std.fs.File.Writer = .{
    .interface = std.fs.File.Writer.initInterface(&.{}),
    .file = if (is_windows) undefined else std.fs.File.stderr(),
    .mode = .streaming,
};

fn stderrTraceWriter() *std.Io.Writer {
    if (is_windows) stderr_file_writer.file = std.fs.File.stderr();
    return &stderr_file_writer.interface;
}

// Global base pointer for the serialized header + env.
// Is a weak constant that can be overwritten by `roc build`.
extern var roc__serialized_base_ptr: ?[*]align(1) u8;
extern var roc__serialized_size: usize;

// Global state initilization - initialized once per process
var roc_module_initialized: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);
var init_mutex: std.Thread.Mutex = .{};
var global_env_ptr: ?*ModuleEnv = null;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocStr = builtins.str.RocStr;
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
    InvalidEntryIndex,
} || safe_memory.MemoryError || eval.EvalError;

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][u32 entry_count][ModuleEnv data][u32[] def_indices]
export fn roc_entrypoint(entry_idx: u32, ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void {
    evaluate(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        var buf: [256]u8 = undefined;
        const msg2 = std.fmt.bufPrint(&buf, "Error evaluating from shared memory: {s}", .{@errorName(err)}) catch "Error evaluating from shared memory";
        ops.crash(msg2);
    };
}

/// Initialize shared memory and ModuleEnv once per process
fn initializeOnce(roc_ops: *RocOps) ShimError!void {
    // Fast path: if already initialized, return immediately
    if (roc_module_initialized.load(.acquire)) {
        return;
    }

    // Slow path: acquire mutex and check again (double-checked locking)
    init_mutex.lock();
    defer init_mutex.unlock();

    // Check again in case another thread initialized while we were waiting
    if (roc_module_initialized.load(.acquire)) {
        return;
    }

    const allocator = std.heap.page_allocator;
    var buf: [256]u8 = undefined;

    if (roc__serialized_base_ptr == null) {
        // Roc run path: Use the shared memory allocator.

        // Get page size
        const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;

        // Create shared memory allocator from coordination info
        // Note shm last the lifetime of the program and is never freed.
        var shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
            const msg2 = std.fmt.bufPrint(&buf, "Failed to create shared memory allocator: {s}", .{@errorName(err)}) catch "Failed to create shared memory allocator";
            roc_ops.crash(msg2);
            return error.SharedMemoryError;
        };

        // Validate memory layout - we need at least space for the header
        const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(ipc.ModuleEnvHeader);
        if (shm.total_size < min_required_size) {
            const msg = std.fmt.bufPrint(&buf, "Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size }) catch "Invalid memory layout";
            roc_ops.crash(msg);
            return error.MemoryLayoutInvalid;
        }

        // setup base pointer
        roc__serialized_base_ptr = shm.getBasePtr();
        roc__serialized_size = shm.total_size;
    }

    // Set up ModuleEnv from shared memory
    const env_ptr = try setupModuleEnv(roc_ops);

    // Store globals
    global_env_ptr = env_ptr;

    // Mark as initialized (release semantics ensure all writes above are visible)
    roc_module_initialized.store(true, .release);
}

/// Cross-platform evaluation
fn evaluate(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {

    // Initialize shared memory once per process
    try initializeOnce(roc_ops);

    // Use the global shared memory and environment
    const env_ptr = global_env_ptr.?;

    // Set up interpreter infrastructure (per-call, as it's lightweight)
    var interpreter = try createInterpreter(env_ptr, roc_ops);
    defer interpreter.deinit();

    // Get expression info from shared memory using entry_idx
    const base_ptr = roc__serialized_base_ptr.?;
    var buf: [256]u8 = undefined;

    // Read the header structure from shared memory
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const ipc.ModuleEnvHeader = @ptrFromInt(header_addr);
    if (entry_idx >= header_ptr.entry_count) {
        const err_msg = std.fmt.bufPrint(&buf, "Invalid entry_idx {} >= entry_count {}", .{ entry_idx, header_ptr.entry_count }) catch "Invalid entry_idx";
        roc_ops.crash(err_msg);
        return error.InvalidEntryIndex;
    }

    const def_offset = header_ptr.def_indices_offset + entry_idx * @sizeOf(u32);
    const def_idx_raw = safe_memory.safeRead(u32, base_ptr, @intCast(def_offset), roc__serialized_size) catch |err| {
        const read_err = std.fmt.bufPrint(&buf, "Failed to read def_idx: {}", .{err}) catch "Failed to read def_idx";
        roc_ops.crash(read_err);
        return error.MemoryLayoutInvalid;
    };
    const def_idx: CIR.Def.Idx = @enumFromInt(def_idx_raw);

    // Get the definition and extract its expression
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;

    // Evaluate the expression (with optional arguments)
    try interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr);
}

/// Set up ModuleEnv from shared memory with proper relocation
fn setupModuleEnv(roc_ops: *RocOps) ShimError!*ModuleEnv {
    var buf: [256]u8 = undefined;
    const base_ptr = roc__serialized_base_ptr.?;

    // Read parent's shared memory base address from header and calculate relocation offset
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const ipc.ModuleEnvHeader = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        const err_msg = std.fmt.bufPrint(&buf, "Relocation offset too large: {}", .{offset}) catch "Relocation offset too large";
        roc_ops.crash(err_msg);
        return error.ModuleEnvSetupFailed;
    }

    // Get ModuleEnv.Serialized pointer from the offset stored in the header
    const env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.module_env_offset));
    const serialized_ptr: *ModuleEnv.Serialized = @ptrFromInt(env_addr);

    // Deserialize the ModuleEnv, which properly reconstructs runtime fields
    // Empty strings are used for source and module_name since they're not needed in the interpreter
    const env_ptr = serialized_ptr.deserialize(offset, std.heap.page_allocator, "", "");

    return env_ptr;
}

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv, roc_ops: *RocOps) ShimError!Interpreter {
    const allocator = std.heap.page_allocator;

    // Extract builtin statement indices from the builtin_statements span
    // The span contains Bool, Result, and Str statements
    const bool_stmt: CIR.Statement.Idx = @enumFromInt(env_ptr.builtin_statements.span.start);
    const result_stmt: CIR.Statement.Idx = @enumFromInt(env_ptr.builtin_statements.span.start + 1);
    const str_stmt: CIR.Statement.Idx = @enumFromInt(env_ptr.builtin_statements.span.start + 2);

    // In the shim context, builtins are embedded in the main module_env
    const builtin_types = eval.BuiltinTypes{
        .bool_stmt = bool_stmt,
        .result_stmt = result_stmt,
        .str_stmt = str_stmt,
        .bool_env = env_ptr,
        .result_env = env_ptr,
        .str_env = env_ptr,
    };

    const interpreter = eval.Interpreter.init(allocator, env_ptr, builtin_types, &[_]*const can.ModuleEnv{}) catch {
        roc_ops.crash("INTERPRETER SHIM: Interpreter initialization failed");
        return error.InterpreterSetupFailed;
    };
    return interpreter;
}

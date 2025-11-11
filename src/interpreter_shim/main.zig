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

// Global state for shared memory - initialized once per process
var shared_memory_initialized: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);
var global_shm: ?SharedMemoryAllocator = null;
var global_env_ptr: ?*ModuleEnv = null;
var shm_mutex: std.Thread.Mutex = .{};
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;
const Interpreter = eval.Interpreter;
const safe_memory = base.safe_memory;

// Test coordination header structure that follows SharedMemoryAllocator.Header
// This must match the structure created in main.zig
const TestCoordinationHeader = extern struct {
    parent_base_addr: u64,
    entry_count: u32,
    _padding: u32, // Ensure 8-byte alignment
    def_indices_offset: u64,
    module_env_offset: u64,
    // Builtin type statement indices (from Builtin module)
    bool_stmt: u32,
    try_stmt: u32,
    str_stmt: u32,
    _padding2: u32, // Ensure 8-byte alignment
};

// The SharedMemoryAllocator places its own header at offset 0, and starts allocations
// after that. Our test coordination header is the first allocation.
const FIRST_ALLOC_OFFSET = @sizeOf(SharedMemoryAllocator.Header);

// Compile-time checks to ensure proper alignment
comptime {
    const collections = @import("collections");
    const alignment_bytes = collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits();

    // The first allocation must be properly aligned for serialized data
    if (FIRST_ALLOC_OFFSET % alignment_bytes != 0) {
        @compileError("FIRST_ALLOC_OFFSET must be aligned to SERIALIZATION_ALIGNMENT");
    }
}

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
    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        var buf: [256]u8 = undefined;
        const msg2 = std.fmt.bufPrint(&buf, "Error evaluating from shared memory: {s}", .{@errorName(err)}) catch "Error evaluating from shared memory";
        ops.crash(msg2);
    };
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

    const allocator = std.heap.page_allocator;
    var buf: [256]u8 = undefined;

    // Get page size
    const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;

    // Create shared memory allocator from coordination info
    var shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
        const msg2 = std.fmt.bufPrint(&buf, "Failed to create shared memory allocator: {s}", .{@errorName(err)}) catch "Failed to create shared memory allocator";
        roc_ops.crash(msg2);
        return error.SharedMemoryError;
    };

    // Set up ModuleEnv from shared memory
    const env_ptr = try setupModuleEnv(&shm, roc_ops);

    // Store globals
    global_shm = shm;
    global_env_ptr = env_ptr;

    // Mark as initialized (release semantics ensure all writes above are visible)
    shared_memory_initialized.store(true, .release);
}

/// Cross-platform shared memory evaluation
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {

    // Initialize shared memory once per process
    try initializeSharedMemoryOnce(roc_ops);

    // Use the global shared memory and environment
    const shm = global_shm.?;
    const env_ptr = global_env_ptr.?;

    // Get expression info from shared memory using entry_idx
    const base_ptr = shm.getBasePtr();
    var buf: [256]u8 = undefined;

    // Read the header structure from shared memory
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const TestCoordinationHeader = @ptrFromInt(header_addr);

    // Set up interpreter infrastructure (per-call, as it's lightweight)
    var interpreter = try createInterpreter(env_ptr, header_ptr, roc_ops);
    defer interpreter.deinit();

    // Validate entry_idx
    if (entry_idx >= header_ptr.entry_count) {
        const err_msg = std.fmt.bufPrint(&buf, "Invalid entry_idx {} >= entry_count {}", .{ entry_idx, header_ptr.entry_count }) catch "Invalid entry_idx";
        roc_ops.crash(err_msg);
        return error.InvalidEntryIndex;
    }

    const def_offset = header_ptr.def_indices_offset + entry_idx * @sizeOf(u32);
    const def_idx_raw = safe_memory.safeRead(u32, base_ptr, @intCast(def_offset), shm.total_size) catch |err| {
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
fn setupModuleEnv(shm: *SharedMemoryAllocator, roc_ops: *RocOps) ShimError!*ModuleEnv {

    // Validate memory layout - we need at least space for the header
    const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(TestCoordinationHeader);
    if (shm.total_size < min_required_size) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size }) catch "Invalid memory layout";
        roc_ops.crash(msg);
        return error.MemoryLayoutInvalid;
    }

    // Get base pointer
    const base_ptr = shm.getBasePtr();

    // The ModuleEnv in shared memory was created live by the parent process.
    // We need to RELOCATE the pointers, not deserialize from a serialized buffer.
    const child_base_addr = @intFromPtr(base_ptr);

    // Get the test coordination header
    const header_addr = child_base_addr + FIRST_ALLOC_OFFSET;
    const header_ptr: *const TestCoordinationHeader = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;

    // Calculate relocation offset (how much to adjust all pointers by)
    const relocation_offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));

    // Get the ModuleEnv pointer (it's a live struct, not serialized)
    const env_addr = child_base_addr + @as(usize, @intCast(header_ptr.module_env_offset));
    const env_ptr: *ModuleEnv = @ptrFromInt(env_addr);

    // Relocate all pointers in the ModuleEnv to point to the child's address space
    env_ptr.relocate(relocation_offset);

    // IMPORTANT: The gpa allocator fields contain function pointers from the parent process.
    // We must replace them with fresh allocators for the child process.
    env_ptr.gpa = std.heap.page_allocator;
    env_ptr.store.gpa = std.heap.page_allocator;

    return env_ptr;
}

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv, header: *const TestCoordinationHeader, roc_ops: *RocOps) ShimError!Interpreter {
    const allocator = std.heap.page_allocator;

    // Get builtin statement indices from the header (provided by parent process)
    const bool_stmt: CIR.Statement.Idx = @enumFromInt(header.bool_stmt);
    const try_stmt: CIR.Statement.Idx = @enumFromInt(header.try_stmt);
    const str_stmt: CIR.Statement.Idx = @enumFromInt(header.str_stmt);
    // Load the actual builtin modules (Bool, Result, Str) which contain the type definitions
    // We need to load these from the embedded Builtin.bin, not from the user's ModuleEnv
    const compiled_builtins = @import("compiled_builtins");
    const builtin_loading = @import("eval").builtin_loading;

    var bool_module = builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Bool", compiled_builtins.builtin_source) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "INTERPRETER SHIM: Failed to load Bool module: {s}", .{@errorName(err)}) catch "Failed to load Bool module";
        roc_ops.crash(msg);
        return error.InterpreterSetupFailed;
    };
    defer bool_module.deinit();

    var result_module = builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Result", compiled_builtins.builtin_source) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "INTERPRETER SHIM: Failed to load Result module: {s}", .{@errorName(err)}) catch "Failed to load Result module";
        roc_ops.crash(msg);
        return error.InterpreterSetupFailed;
    };
    defer result_module.deinit();

    var str_module = builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Str", compiled_builtins.builtin_source) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "INTERPRETER SHIM: Failed to load Str module: {s}", .{@errorName(err)}) catch "Failed to load Str module";
        roc_ops.crash(msg);
        return error.InterpreterSetupFailed;
    };
    defer str_module.deinit();

    // Use the loaded builtin modules for type lookups
    const builtin_types = eval.BuiltinTypes{
        .bool_stmt = bool_stmt,
        .try_stmt = try_stmt,
        .str_stmt = str_stmt,
        .bool_env = bool_module.env,
        .try_env = result_module.env,
        .str_env = str_module.env,
    };

    const interpreter = eval.Interpreter.init(allocator, env_ptr, builtin_types, &[_]*const can.ModuleEnv{}) catch |err| {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "INTERPRETER SHIM: Interpreter initialization failed: {s}", .{@errorName(err)}) catch "INTERPRETER SHIM: Interpreter initialization failed";
        roc_ops.crash(msg);
        return error.InterpreterSetupFailed;
    };
    return interpreter;
}

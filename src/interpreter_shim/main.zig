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
var global_module_envs: ?[]const *ModuleEnv = null; // All loaded module envs
var shm_mutex: std.Thread.Mutex = .{};
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
    module_count: u32, // Number of ModuleEnvs stored
    entry_count: u32, // Number of entry points
    def_indices_offset: u64,
    module_envs_offset: u64, // Offset to array of module env offsets
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
export fn roc_entrypoint(entry_idx: u32, ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void {
    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        // Use heap allocation for error message to avoid fixed buffer size limits
        const msg2 = std.fmt.allocPrint(std.heap.page_allocator, "Error evaluating from shared memory: {s} (entry_idx={})", .{ @errorName(err), entry_idx }) catch "Error evaluating from shared memory";
        ops.crash(msg2);
        // Note: We're about to crash, so it's ok to leak this allocation
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

    // Get page size
    const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;

    // Create shared memory allocator from coordination info
    var shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
        // Use heap allocation for error message to avoid fixed buffer size limits
        const msg2 = std.fmt.allocPrint(allocator, "Failed to create shared memory allocator: {s}", .{@errorName(err)}) catch "Failed to create shared memory allocator";
        roc_ops.crash(msg2);
        // Note: We're about to crash, so it's ok to leak this allocation
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

    // Set up interpreter infrastructure (per-call, as it's lightweight)
    var interpreter = try createInterpreter(env_ptr, roc_ops);
    defer interpreter.deinit();

    // Get expression info from shared memory using entry_idx
    const base_ptr = shm.getBasePtr();

    // Read the header structure from shared memory
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    if (entry_idx >= header_ptr.entry_count) {
        // Use heap allocation for error message to avoid fixed buffer size limits
        const err_msg = std.fmt.allocPrint(std.heap.page_allocator, "Invalid entry_idx {} >= entry_count {}", .{ entry_idx, header_ptr.entry_count }) catch "Invalid entry_idx";
        roc_ops.crash(err_msg);
        // Note: We're about to crash, so it's ok to leak this allocation
        return error.InvalidEntryIndex;
    }

    const def_offset = header_ptr.def_indices_offset + entry_idx * @sizeOf(u32);
    const def_idx_raw = safe_memory.safeRead(u32, base_ptr, @intCast(def_offset), shm.total_size) catch |err| {
        // Use heap allocation for error message to avoid fixed buffer size limits
        const read_err = std.fmt.allocPrint(std.heap.page_allocator, "Failed to read def_idx: {}", .{err}) catch "Failed to read def_idx";
        roc_ops.crash(read_err);
        // Note: We're about to crash, so it's ok to leak this allocation
        return error.MemoryLayoutInvalid;
    };
    const def_idx: CIR.Def.Idx = @enumFromInt(def_idx_raw);

    // Get the definition and extract its expression
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;

    // Evaluate the expression (with optional arguments)
    interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr) catch |err| {
        return err;
    };
}

/// Set up ModuleEnvs from shared memory with proper relocation
/// Returns the primary (root) module env and stores all envs in global_module_envs
fn setupModuleEnv(shm: *SharedMemoryAllocator, roc_ops: *RocOps) ShimError!*ModuleEnv {

    // Validate memory layout - we need at least space for the header
    const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(Header);
    if (shm.total_size < min_required_size) {
        // Use heap allocation for error message to avoid fixed buffer size limits
        const msg = std.fmt.allocPrint(std.heap.page_allocator, "Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size }) catch "Invalid memory layout";
        roc_ops.crash(msg);
        // Note: We're about to crash, so it's ok to leak this allocation
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
    const offset = @as(isize, @intCast(child_base_addr)) - @as(isize, @intCast(parent_base_addr));

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        // Use heap allocation for error message to avoid fixed buffer size limits
        const err_msg = std.fmt.allocPrint(std.heap.page_allocator, "Relocation offset too large: {}", .{offset}) catch "Relocation offset too large";
        roc_ops.crash(err_msg);
        // Note: We're about to crash, so it's ok to leak this allocation
        return error.ModuleEnvSetupFailed;
    }

    // Get the array of module env offsets
    const module_envs_array_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.module_envs_offset));
    const module_env_offsets: [*]const u64 = @ptrFromInt(module_envs_array_addr);

    if (header_ptr.module_count == 0) {
        roc_ops.crash("No module environments found in shared memory");
        return error.ModuleEnvSetupFailed;
    }

    // Allocate array to store all deserialized module envs
    const module_envs = std.heap.page_allocator.alloc(*ModuleEnv, header_ptr.module_count) catch {
        roc_ops.crash("Failed to allocate module_envs array");
        return error.ModuleEnvSetupFailed;
    };

    // Deserialize ALL module environments
    for (0..header_ptr.module_count) |i| {
        const env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(module_env_offsets[i]));
        const serialized_ptr: *ModuleEnv.Serialized = @ptrFromInt(env_addr);

        // Extract and relocate module_name from serialized data
        const module_name_slice_parts = serialized_ptr.module_name;
        const module_name_ptr = @as(usize, @intCast(module_name_slice_parts[0]));
        const module_name_len = @as(usize, @intCast(module_name_slice_parts[1]));

        const module_name = if (module_name_len > 0 and module_name_ptr > 0) blk: {
            const relocated_module_name_ptr = @as([*]const u8, @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(module_name_ptr)) + offset))));
            break :blk relocated_module_name_ptr[0..module_name_len];
        } else "";

        // Deserialize the ModuleEnv with the relocated module_name
        // Source text is not available because we don't put it in the shared memory.
        module_envs[i] = try serialized_ptr.deserialize(offset, std.heap.page_allocator, "", module_name);
    }

    // Store all module envs globally
    global_module_envs = module_envs;

    // Return the first (primary/root) module
    return module_envs[0];
}

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv, roc_ops: *RocOps) ShimError!Interpreter {
    const allocator = std.heap.page_allocator;

    // Load builtin modules (same as CLI does)
    const builtin_modules = eval.BuiltinModules.init(allocator) catch {
        roc_ops.crash("Failed to initialize builtin modules");
        return error.InterpreterSetupFailed;
    };
    // Note: We intentionally don't deinit builtin_modules because the interpreter needs them

    const builtin_types = eval.BuiltinTypes.init(
        builtin_modules.builtin_indices,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_module.env,
    );

    // Build other_envs array: [Builtin, ...platform modules]
    // The app's imports are [Builtin, Stdout, Stderr], so other_envs must match
    const other_envs: []const *const can.ModuleEnv = if (global_module_envs) |envs| blk: {
        if (envs.len > 1) {
            // Create array with Builtin first, then platform modules (excluding the app itself)
            var envs_with_builtin = allocator.alloc(*const can.ModuleEnv, envs.len) catch {
                roc_ops.crash("Failed to allocate other_envs");
                return error.InterpreterSetupFailed;
            };
            envs_with_builtin[0] = builtin_modules.builtin_module.env;
            // Copy platform modules (skip app at index 0, include Stdout and Stderr)
            for (1..envs.len) |i| {
                envs_with_builtin[i] = envs[i];
            }
            break :blk envs_with_builtin;
        } else {
            // No platform modules, just Builtin
            break :blk &[_]*const can.ModuleEnv{builtin_modules.builtin_module.env};
        }
    } else &[_]*const can.ModuleEnv{builtin_modules.builtin_module.env};

    const interpreter = eval.Interpreter.init(allocator, env_ptr, builtin_types, other_envs) catch {
        roc_ops.crash("INTERPRETER SHIM: Interpreter initialization failed");
        return error.InterpreterSetupFailed;
    };

    return interpreter;
}

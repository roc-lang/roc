//! A shim to read the ModuleEnv from shared memory for the interpreter
//! Refactored to use clean abstractions for cross-platform shared memory,
//! memory safety, and interpreter integration.

const std = @import("std");
const builtins = @import("builtins");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const import_mapping_mod = types.import_mapping;
const eval = @import("eval");
const ipc = @import("ipc");

// Static empty import mapping for shim (no type name resolution needed)
var shim_import_mapping = import_mapping_mod.ImportMapping.init(std.heap.page_allocator);

const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

// Global base pointer for the serialized header + env.
// Is a weak extern that can be overwritten by `roc build` when embedding module data.
// If null at runtime, we're in IPC mode (roc run) and read from shared memory.
// If non-null, we're in embedded mode (roc build) and data is compiled into the binary.
extern var roc__serialized_base_ptr: ?[*]align(1) u8;
extern var roc__serialized_size: usize;

// Global state for shared memory - initialized once per process
var shared_memory_initialized: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);
var global_shm: ?SharedMemoryAllocator = null;
var global_env_ptr: ?*ModuleEnv = null; // Primary env for entry point lookups (platform or app)
var global_app_env_ptr: ?*ModuleEnv = null; // App env for e_lookup_required resolution
var global_builtin_modules: ?eval.BuiltinModules = null;
var global_imported_envs: ?[]*const ModuleEnv = null;
var shm_mutex: std.Thread.Mutex = .{};
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const Interpreter = eval.Interpreter;
const safe_memory = base.safe_memory;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset
const MODULE_ENV_OFFSET = 0x10; // 8 bytes for u64, 4 bytes for u32, 4 bytes padding

// Header structure that matches the one in main.zig (multi-module format)
// For embedded mode: parent_base_addr == 0
// For IPC mode: parent_base_addr == actual parent address
const Header = struct {
    parent_base_addr: u64,
    module_count: u32,
    entry_count: u32,
    def_indices_offset: u64,
    module_envs_offset: u64, // Offset to array of module env offsets
    platform_main_env_offset: u64, // 0 if no platform, entry points are in app
    app_env_offset: u64, // Always present, used for e_lookup_required resolution
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
        // Only show this generic error if we haven't already crashed with a more specific message
        // (errors like Crash already triggered roc_crashed with details)
        if (err != error.Crash) {
            var buf: [256]u8 = undefined;
            const msg2 = std.fmt.bufPrint(&buf, "Error evaluating: {s}", .{@errorName(err)}) catch "Error evaluating";
            ops.crash(msg2);
        }
    };
}

/// Initialize shared memory and ModuleEnv once per process
fn initializeOnce(roc_ops: *RocOps) ShimError!void {
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

    // Set up ModuleEnv from serialized data (embedded or shared memory)
    const setup_result = try setupModuleEnv(roc_ops);

    // Load builtin modules from compiled binary (same as CLI does)
    const builtin_modules = eval.BuiltinModules.init(allocator) catch |err| {
        const msg2 = std.fmt.bufPrint(&buf, "Failed to load builtin modules: {s}", .{@errorName(err)}) catch "Failed to load builtin modules";
        roc_ops.crash(msg2);
        return error.ModuleEnvSetupFailed;
    };

    // Store globals
    global_env_ptr = setup_result.primary_env;
    global_app_env_ptr = setup_result.app_env;
    global_builtin_modules = builtin_modules;

    // Mark as initialized (release semantics ensure all writes above are visible)
    shared_memory_initialized.store(true, .release);
}

/// Cross-platform evaluation (works for both IPC and embedded modes)
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    // Initialize shared memory once per process
    try initializeOnce(roc_ops);

    // Use the global shared memory and environment
    const env_ptr = global_env_ptr.?;
    const app_env = global_app_env_ptr;

    // Get builtin modules
    const builtin_modules = &global_builtin_modules.?;

    // Set up interpreter infrastructure (per-call, as it's lightweight)
    var interpreter = try createInterpreter(env_ptr, app_env, builtin_modules, roc_ops);
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

/// Result of setting up module environments
const SetupResult = struct {
    primary_env: *ModuleEnv, // Platform main env or app env (for entry points)
    app_env: *ModuleEnv, // App env (for e_lookup_required resolution)
};

/// Set up ModuleEnv from serialized data with proper relocation (multi-module format)
/// Works for both IPC mode (roc run) and embedded mode (roc build)
fn setupModuleEnv(roc_ops: *RocOps) ShimError!SetupResult {
    var buf: [256]u8 = undefined;
    const base_ptr = roc__serialized_base_ptr.?;
    const allocator = std.heap.page_allocator;

    // Read parent's shared memory base address from header and calculate relocation offset
    // For embedded mode: parent_base_addr == 0
    // For IPC mode: parent_base_addr == actual parent address
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;
    const module_count = header_ptr.module_count;

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    // Use signed arithmetic to avoid overflow on 64-bit addresses
    const offset: i64 = @as(i64, @intCast(child_base_addr)) - @as(i64, @intCast(parent_base_addr));

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        const err_msg = std.fmt.bufPrint(&buf, "Relocation offset too large: {}", .{offset}) catch "Relocation offset too large";
        roc_ops.crash(err_msg);
        return error.ModuleEnvSetupFailed;
    }

    // Get module env offsets array
    const module_envs_base_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.module_envs_offset));
    const module_env_offsets: [*]const u64 = @ptrFromInt(module_envs_base_addr);

    // Load all module envs (platform modules first, app module last)
    // The app module is always the last one in the array
    var imported_envs = allocator.alloc(*const ModuleEnv, module_count - 1) catch {
        roc_ops.crash("Failed to allocate imported envs array");
        return error.OutOfMemory;
    };

    // Relocate platform modules first (indices 0 to module_count-2)
    for (0..module_count - 1) |i| {
        const module_env_offset = module_env_offsets[i];
        const module_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(module_env_offset));
        const module_env_ptr: *ModuleEnv = @ptrFromInt(module_env_addr);
        module_env_ptr.relocate(@intCast(offset));
        module_env_ptr.gpa = allocator;
        imported_envs[i] = module_env_ptr;
    }

    // Store imported envs globally
    global_imported_envs = imported_envs;

    // Get and relocate the app module using the header's app_env_offset
    const app_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.app_env_offset));
    const app_env_ptr: *ModuleEnv = @ptrFromInt(app_env_addr);
    app_env_ptr.relocate(@intCast(offset));
    app_env_ptr.gpa = allocator;

    // Determine primary env: platform main if available, otherwise app
    const primary_env: *ModuleEnv = if (header_ptr.platform_main_env_offset != 0) blk: {
        const platform_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.platform_main_env_offset));
        const platform_env_ptr: *ModuleEnv = @ptrFromInt(platform_env_addr);
        platform_env_ptr.relocate(@intCast(offset));
        platform_env_ptr.gpa = allocator;
        break :blk platform_env_ptr;
    } else app_env_ptr;

    return SetupResult{
        .primary_env = primary_env,
        .app_env = app_env_ptr,
    };
}

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv, app_env: ?*ModuleEnv, builtin_modules: *const eval.BuiltinModules, roc_ops: *RocOps) ShimError!Interpreter {
    const allocator = std.heap.page_allocator;

    // Use builtin types from the loaded builtin modules
    // This provides the actual definitions of plus, minus, times, etc.
    const builtin_types = builtin_modules.asBuiltinTypes();

    // Pass the builtin module's env so method lookup can find builtin method definitions
    const builtin_module_env = builtin_modules.builtin_module.env;

    // Use the imported envs from platform modules (set up during setupModuleEnv)
    // IMPORTANT: The app's imports include 'Builtin' first, then platform modules.
    // So we need to prepend builtin_module_env to match the positional mapping.
    var all_imported_envs = std.ArrayList(*const can.ModuleEnv).empty;
    // Note: Don't defer deinit - we pass ownership of the slice to the interpreter

    // First add builtin module (to match 'Builtin' import)
    all_imported_envs.append(allocator, builtin_module_env) catch {
        roc_ops.crash("Failed to build imported envs list");
        return error.OutOfMemory;
    };

    // Then add platform modules
    if (global_imported_envs) |platform_envs| {
        for (platform_envs) |penv| {
            all_imported_envs.append(allocator, penv) catch {
                roc_ops.crash("Failed to build imported envs list");
                return error.OutOfMemory;
            };
        }
    }

    // Use toOwnedSlice to transfer ownership to caller
    const imported_envs = all_imported_envs.toOwnedSlice(allocator) catch {
        roc_ops.crash("Failed to get owned slice");
        return error.OutOfMemory;
    };

    // Resolve imports - map each import name to its index in imported_envs
    env_ptr.imports.resolveImports(env_ptr, imported_envs);

    // Also resolve imports for the app env if it's different from the primary env
    // This is needed when the platform calls the app's main! via e_lookup_required
    if (app_env) |a_env| {
        if (a_env != env_ptr) {
            a_env.imports.resolveImports(a_env, imported_envs);
        }
    }

    const interpreter = eval.Interpreter.init(allocator, env_ptr, builtin_types, builtin_module_env, imported_envs, &shim_import_mapping, app_env) catch {
        roc_ops.crash("INTERPRETER SHIM: Interpreter initialization failed");
        return error.InterpreterSetupFailed;
    };

    return interpreter;
}

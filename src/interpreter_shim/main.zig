//! A shim to read the ModuleEnv from shared memory for the interpreter
//! Refactored to use clean abstractions for cross-platform shared memory,
//! memory safety, and interpreter integration.
//!
//! For wasm32-freestanding: Only embedded mode is supported (no IPC).
//! The serialized module data is linked into the binary via roc__serialized_base_ptr.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const builtins = @import("builtins");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const compile = @import("compile");
const types = @import("types");
const collections = @import("collections");
const import_mapping_mod = types.import_mapping;
const eval = @import("eval");
const layout = @import("layout");
const tracy = @import("tracy");
const roc_target = @import("roc_target");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const lir = @import("lir");
const symbol = @import("symbol");

// Module tracing flag - enabled via `zig build -Dtrace-modules`
const trace_modules = if (@hasDecl(build_options, "trace_modules")) build_options.trace_modules else false;

// Helper to emit trace messages when trace_modules is enabled.
// On native platforms, uses std.debug.print. On WASM, uses roc_ops.dbg().
fn traceDbg(roc_ops: *RocOps, comptime fmt: []const u8, args: anytype) void {
    if (comptime trace_modules) {
        if (comptime builtin.cpu.arch == .wasm32) {
            // WASM: use roc_ops.dbg() since std.debug.print is unavailable
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "[TRACE-MODULES] " ++ fmt ++ "\n", args) catch "[TRACE-MODULES] (message too long)\n";
            roc_ops.dbg(msg);
        } else {
            // Native: use std.debug.print
            std.debug.print("[TRACE-MODULES] " ++ fmt ++ "\n", args);
        }
    }
}

// Platform detection
const is_wasm32 = builtin.cpu.arch == .wasm32;

// IPC module only available on native platforms (not wasm32)
// On wasm32, we provide stub types that produce clear compile errors if used.
const ipc = if (is_wasm32) struct {
    /// IPC is not available on wasm32 - modules must be embedded at compile time.
    /// Use `roc build` to create standalone WASM binaries with embedded modules.
    pub const SharedMemoryAllocator = struct {
        pub fn create(_: usize, _: usize) @This() {
            @compileError("IPC/SharedMemory is not supported on wasm32. Use embedded mode via `roc build`.");
        }
        pub fn fromCoordination(_: anytype, _: usize) @This() {
            @compileError("IPC/SharedMemory is not supported on wasm32. Use embedded mode via `roc build`.");
        }
    };
} else @import("ipc");

// Debug allocator for native platforms (not wasm32) - provides leak detection in Debug/ReleaseSafe builds
var debug_allocator: if (is_wasm32) void else std.heap.DebugAllocator(.{}) =
    if (is_wasm32) {} else .{ .backing_allocator = std.heap.c_allocator };

// Get the base allocator based on platform and build mode
fn getBaseAllocator() std.mem.Allocator {
    if (is_wasm32) return wasm_allocator;
    return switch (builtin.mode) {
        .Debug, .ReleaseSafe => debug_allocator.allocator(),
        .ReleaseFast, .ReleaseSmall => std.heap.c_allocator,
    };
}

// TracyAllocator wrapping for allocation profiling
var tracy_allocator: tracy.TracyAllocator(null) = undefined;
var wrapped_allocator: std.mem.Allocator = undefined;
var allocator_initialized: bool = false;

// Wasm32 allocator - uses roc_alloc from host
const wasm_allocator = if (is_wasm32) std.mem.Allocator{
    .ptr = undefined,
    .vtable = &.{
        .alloc = wasmAlloc,
        .resize = wasmResize,
        .remap = wasmRemap,
        .free = wasmFree,
    },
} else undefined;

// Wasm32 allocator vtable implementation
fn wasmAlloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, _: usize) ?[*]u8 {
    // Pass the actual requested alignment to roc_alloc
    const align_bytes: u32 = @intCast(alignment.toByteUnits());
    const ptr = roc_alloc(len, align_bytes);
    return if (ptr) |p| @ptrCast(p) else null;
}

fn wasmResize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
    return false; // roc_realloc doesn't fit the Zig allocator model well
}

fn wasmRemap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
    return null; // remap not supported
}

fn wasmFree(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, _: usize) void {
    const align_bytes: u32 = @intCast(alignment.toByteUnits());
    roc_dealloc(@ptrCast(buf.ptr), align_bytes);
}

// Host-provided allocation functions (for wasm32)
extern fn roc_alloc(size: usize, alignment: u32) callconv(.c) ?*anyopaque;
extern fn roc_realloc(ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.c) ?*anyopaque;
extern fn roc_dealloc(ptr: *anyopaque, alignment: u32) callconv(.c) void;

// Static empty import mapping for shim (no type name resolution needed)
// Lazy-initialized to use the properly wrapped allocator
var shim_import_mapping: ?import_mapping_mod.ImportMapping = null;

fn getShimImportMapping() *import_mapping_mod.ImportMapping {
    if (shim_import_mapping == null) {
        shim_import_mapping = import_mapping_mod.ImportMapping.init(wrapped_allocator);
    }
    return &shim_import_mapping.?;
}

const SharedMemoryAllocator = if (is_wasm32) struct {} else ipc.SharedMemoryAllocator;

/// Thread-safe initialization flag with unified interface.
/// On wasm32: simple bool (single-threaded environment)
/// On native: atomic with proper memory ordering for multi-threaded safety
const InitializationFlag = struct {
    inner: if (is_wasm32) bool else std.atomic.Value(bool),

    const Self = @This();

    pub fn init() Self {
        return .{ .inner = if (is_wasm32) false else std.atomic.Value(bool).init(false) };
    }

    pub fn isSet(self: *const Self) bool {
        return if (is_wasm32) self.inner else self.inner.load(.acquire);
    }

    pub fn set(self: *Self) void {
        if (is_wasm32) {
            self.inner = true;
        } else {
            self.inner.store(true, .release);
        }
    }
};

/// Platform-appropriate mutex with unified interface.
/// On wasm32: no-op (single-threaded environment)
/// On native: actual mutex for thread safety
const PlatformMutex = struct {
    inner: if (is_wasm32) void else std.Thread.Mutex,

    const Self = @This();

    pub fn init() Self {
        return .{ .inner = if (is_wasm32) {} else .{} };
    }

    pub fn lock(self: *Self) void {
        if (!is_wasm32) self.inner.lock();
    }

    pub fn unlock(self: *Self) void {
        if (!is_wasm32) self.inner.unlock();
    }
};

// Global base pointer for the serialized header + env.
// Is a weak extern that can be overwritten by `roc build` when embedding module data.
// If null at runtime, we're in IPC mode (roc run) and read from shared memory.
// If non-null, we're in embedded mode (roc build) and data is compiled into the binary.
extern var roc__serialized_base_ptr: ?[*]align(1) u8;
extern var roc__serialized_size: usize;

// Global state for shared memory - initialized once per process
var shared_memory_initialized = InitializationFlag.init();
var global_shm: if (is_wasm32) void else ?SharedMemoryAllocator = if (is_wasm32)
{} else null;
var global_env_ptr: ?*ModuleEnv = null; // Primary env for entry point lookups (platform or app)
var global_app_env_ptr: ?*ModuleEnv = null; // App env for e_lookup_required resolution
var global_builtin_modules: ?eval.BuiltinModules = null;
var global_imported_envs: ?[]*const ModuleEnv = null;
var global_full_imported_envs: ?[]*const ModuleEnv = null; // Full slice with builtin prepended (for interpreter)
var global_full_mutable_envs: ?[]*ModuleEnv = null; // Exact lowering order consumed by later stages
var global_full_mutable_envs_const: ?[]*const ModuleEnv = null; // Borrowed const view of global_full_mutable_envs
var global_builtin_module_idx: ?u32 = null;
var global_primary_module_idx: ?u32 = null;
var global_app_module_idx: ?u32 = null;
var global_builtin_str: ?base.Ident.Idx = null;
var global_constant_strings_arena: ?*std.heap.ArenaAllocator = null; // Persists across interpreter calls for immortal strings
var shm_mutex = PlatformMutex.init();

// Cached header info (set during initialization, used for evaluation)
var global_entry_count: u32 = 0;
var global_def_indices_offset: u64 = 0;
var global_is_serialized_format: bool = false; // true = portable serialized format, false = legacy format
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const Interpreter = eval.Interpreter;
const safe_memory = base.safe_memory;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset

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

fn appendModuleEnvIfMissing(
    allocator: std.mem.Allocator,
    module_envs: *std.ArrayList(*ModuleEnv),
    module_env: *ModuleEnv,
) std.mem.Allocator.Error!void {
    for (module_envs.items) |existing_env| {
        if (existing_env == module_env) return;
    }

    try module_envs.append(allocator, module_env);
}

fn rebuildLegacyRuntimeState(
    roc_ops: *RocOps,
    module_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) ShimError!void {
    module_env.gpa = allocator;
    module_env.evaluation_order = null;
    module_env.rigid_vars = .{};
    module_env.import_mapping = types.import_mapping.ImportMapping.init(allocator);

    var graph = can.DependencyGraph.buildDependencyGraph(
        module_env,
        module_env.all_defs,
        allocator,
    ) catch {
        roc_ops.crash("Interpreter shim: failed to rebuild dependency graph for legacy module");
        return error.ModuleEnvSetupFailed;
    };
    defer graph.deinit();

    var eval_order = can.DependencyGraph.computeSCCs(&graph, allocator) catch {
        roc_ops.crash("Interpreter shim: failed to rebuild evaluation order for legacy module");
        return error.ModuleEnvSetupFailed;
    };
    const eval_order_ptr = allocator.create(can.DependencyGraph.EvaluationOrder) catch {
        eval_order.deinit();
        roc_ops.crash("Interpreter shim: failed to allocate evaluation order for legacy module");
        return error.OutOfMemory;
    };
    eval_order_ptr.* = eval_order;
    module_env.evaluation_order = eval_order_ptr;
}

// Import serialization types from the shared module
const SERIALIZED_FORMAT_MAGIC = collections.SERIALIZED_FORMAT_MAGIC;

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
} || safe_memory.MemoryError || eval.Interpreter.Error;

/// Exported symbol that reads ModuleEnv from shared memory and evaluates it
/// Returns a RocStr to the caller
/// Expected format in shared memory: [u64 parent_address][u32 entry_count][ModuleEnv data][u32[] def_indices]
export fn roc_entrypoint(entry_idx: u32, ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void {
    const trace = tracy.trace(@src());
    defer trace.end();

    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        // Errors like Crash and StackOverflow already triggered roc_crashed with details
        error.Crash, error.StackOverflow => {},
        // Show generic error for other cases
        else => {
            var buf: [256]u8 = undefined;
            const msg2 = std.fmt.bufPrint(&buf, "Error evaluating: {s}", .{@errorName(err)}) catch "Error evaluating";
            ops.crash(msg2);
        },
    };
}

/// Initialize shared memory and ModuleEnv once per process
fn initializeOnce(roc_ops: *RocOps) ShimError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Fast path: if already initialized, return immediately
    if (shared_memory_initialized.isSet()) return;

    // Slow path: acquire mutex and check again (double-checked locking)
    shm_mutex.lock();
    defer shm_mutex.unlock();

    // Check again in case another thread initialized while we were waiting
    if (shared_memory_initialized.isSet()) return;

    // Set up allocator with optional TracyAllocator wrapping before any allocations
    if (!allocator_initialized) {
        const base_allocator = getBaseAllocator();
        if (tracy.enable_allocation) {
            tracy_allocator = tracy.tracyAllocator(base_allocator);
            wrapped_allocator = tracy_allocator.allocator();
        } else {
            wrapped_allocator = base_allocator;
        }
        allocator_initialized = true;
    }

    const allocator = wrapped_allocator;
    var buf: [256]u8 = undefined;

    // IPC path only available on native platforms (not wasm32)
    if (!is_wasm32 and roc__serialized_base_ptr == null) {
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

        // Ensure memory layout is valid
        if (shm.total_size < FIRST_ALLOC_OFFSET + @sizeOf(Header)) {
            const msg = std.fmt.bufPrint(&buf, "Invalid memory layout: size {} is too small", .{shm.total_size}) catch "Invalid memory layout";
            roc_ops.crash(msg);
            return error.MemoryLayoutInvalid;
        }

        // Store in global for future use
        global_shm = shm;

        // Set base pointer to start of shared memory
        roc__serialized_base_ptr = shm.getBasePtr();
        roc__serialized_size = shm.total_size;
    }

    // Set up ModuleEnv from serialized data (embedded or shared memory)
    const setup_result = try setupModuleEnv(roc_ops);

    // Load builtin modules from compiled binary
    const builtin_modules = eval.BuiltinModules.init(allocator) catch |err| {
        const msg2 = std.fmt.bufPrint(&buf, "Failed to load builtin modules: {s}", .{@errorName(err)}) catch "Failed to load builtin modules";
        roc_ops.crash(msg2);
        return error.ModuleEnvSetupFailed;
    };

    // Store globals
    global_env_ptr = setup_result.primary_env;
    global_app_env_ptr = setup_result.app_env;
    global_builtin_modules = builtin_modules;

    // Build the full imported_envs slice (builtin + platform modules)
    const builtin_module_env = builtin_modules.builtin_module.env;
    var all_imported_envs = std.ArrayList(*const can.ModuleEnv).empty;

    all_imported_envs.append(allocator, builtin_module_env) catch {
        roc_ops.crash("Failed to build imported envs list");
        return error.OutOfMemory;
    };

    if (global_imported_envs) |platform_envs| {
        for (platform_envs) |penv| {
            all_imported_envs.append(allocator, penv) catch {
                roc_ops.crash("Failed to build imported envs list");
                return error.OutOfMemory;
            };
        }
    }

    const full_imported_envs = all_imported_envs.toOwnedSlice(allocator) catch {
        roc_ops.crash("Failed to get owned slice");
        return error.OutOfMemory;
    };
    global_full_imported_envs = full_imported_envs;

    var all_lowering_envs = std.ArrayList(*ModuleEnv).empty;
    defer all_lowering_envs.deinit(allocator);

    for (full_imported_envs) |env| {
        all_lowering_envs.append(allocator, @constCast(env)) catch {
            roc_ops.crash("Failed to build lowering envs list");
            return error.OutOfMemory;
        };
    }

    appendModuleEnvIfMissing(allocator, &all_lowering_envs, setup_result.primary_env) catch {
        roc_ops.crash("Failed to add primary env to lowering envs");
        return error.OutOfMemory;
    };
    appendModuleEnvIfMissing(allocator, &all_lowering_envs, setup_result.app_env) catch {
        roc_ops.crash("Failed to add app env to lowering envs");
        return error.OutOfMemory;
    };

    const mutable_envs = all_lowering_envs.toOwnedSlice(allocator) catch {
        roc_ops.crash("Failed to allocate mutable lowering envs");
        return error.OutOfMemory;
    };
    global_full_mutable_envs = mutable_envs;

    const mutable_envs_const = allocator.alloc(*const ModuleEnv, mutable_envs.len) catch {
        roc_ops.crash("Failed to allocate const lowering envs");
        return error.OutOfMemory;
    };
    for (mutable_envs, 0..) |module_env, i| {
        mutable_envs_const[i] = module_env;
    }
    global_full_mutable_envs_const = mutable_envs_const;

    global_builtin_module_idx = compile.runner.findModuleEnvIdx(mutable_envs, builtin_module_env) orelse {
        roc_ops.crash("INTERPRETER SHIM: builtin module missing from lowering envs");
        return error.InterpreterSetupFailed;
    };
    global_primary_module_idx = compile.runner.findModuleEnvIdx(mutable_envs, setup_result.primary_env) orelse {
        roc_ops.crash("INTERPRETER SHIM: primary module missing from lowering envs");
        return error.InterpreterSetupFailed;
    };
    global_app_module_idx = compile.runner.findModuleEnvIdx(mutable_envs, setup_result.app_env) orelse {
        roc_ops.crash("INTERPRETER SHIM: app module missing from lowering envs");
        return error.InterpreterSetupFailed;
    };
    global_builtin_str = builtin_module_env.idents.builtin_str;

    // Resolve imports for primary and app envs
    const env_ptr = setup_result.primary_env;
    const app_env = setup_result.app_env;

    traceDbg(roc_ops, "Resolving imports for primary env \"{s}\"", .{env_ptr.module_name});
    env_ptr.imports.resolveImports(env_ptr, full_imported_envs);

    if (app_env != env_ptr) {
        traceDbg(roc_ops, "Resolving imports for app env \"{s}\"", .{app_env.module_name});
        app_env.imports.resolveImports(app_env, full_imported_envs);
    }

    traceDbg(roc_ops, "Re-resolving imports for all imported modules", .{});
    for (full_imported_envs) |imp_env| {
        traceDbg(roc_ops, "  Re-resolving for \"{s}\"", .{imp_env.module_name});
        @constCast(imp_env).imports.resolveImports(imp_env, full_imported_envs);
    }

    // Enable runtime inserts on all deserialized module environments
    env_ptr.common.idents.interner.enableRuntimeInserts(allocator) catch {
        roc_ops.crash("INTERPRETER SHIM: Failed to enable runtime inserts on platform env");
        return error.InterpreterSetupFailed;
    };
    if (app_env != env_ptr) {
        @constCast(app_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
            roc_ops.crash("INTERPRETER SHIM: Failed to enable runtime inserts on app env");
            return error.InterpreterSetupFailed;
        };
    }
    if (global_imported_envs) |imported_envs| {
        for (imported_envs) |imp_env| {
            @constCast(imp_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
                roc_ops.crash("INTERPRETER SHIM: Failed to enable runtime inserts on imported env");
                return error.InterpreterSetupFailed;
            };
        }
    }

    // Initialize global arena for immortal strings (used across interpreter calls)
    var arena = std.heap.ArenaAllocator.init(allocator);
    global_constant_strings_arena = &arena;

    // Mark initialization done
    shared_memory_initialized.set();
}

/// Evaluate an entrypoint by reading ModuleEnv from shared memory
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize shared memory + module env (only once)
    try initializeOnce(roc_ops);

    const env_ptr = global_env_ptr orelse return error.ModuleEnvSetupFailed;
    _ = global_app_env_ptr orelse return error.ModuleEnvSetupFailed;
    _ = global_builtin_modules orelse return error.ModuleEnvSetupFailed;
    const lowering_module_envs = global_full_mutable_envs orelse return error.ModuleEnvSetupFailed;
    const lowering_module_envs_const = global_full_mutable_envs_const orelse return error.ModuleEnvSetupFailed;
    const builtin_module_idx = global_builtin_module_idx orelse return error.ModuleEnvSetupFailed;
    const primary_module_idx = global_primary_module_idx orelse return error.ModuleEnvSetupFailed;
    const app_module_idx = global_app_module_idx orelse return error.ModuleEnvSetupFailed;
    const builtin_str = global_builtin_str;

    if (entry_idx >= global_entry_count) {
        roc_ops.crash("Invalid entry index");
        return error.InvalidEntryIndex;
    }

    // Look up entrypoint index from shared memory
    const base_ptr = roc__serialized_base_ptr.?;
    const def_indices_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(global_def_indices_offset));
    const def_indices: [*]const u32 = @ptrFromInt(def_indices_addr);
    const def_idx = def_indices[entry_idx];

    // Find the expression in the primary env (platform or app)
    const def_id: CIR.Def.Idx = @enumFromInt(def_idx);
    const entrypoint_expr = env_ptr.store.getDef(def_id).expr;

    const defs = env_ptr.store.sliceDefs(env_ptr.all_defs);
    var entry_def: ?CIR.Def.Idx = null;
    for (defs) |def_idx_candidate| {
        const def = env_ptr.store.getDef(def_idx_candidate);
        if (def.expr == entrypoint_expr) {
            entry_def = def_idx_candidate;
            break;
        }
    }
    const entry_def_idx = entry_def orelse {
        roc_ops.crash("Interpreter shim: entry def not found");
        return error.EvaluationFailed;
    };

    var source_modules = try wrapped_allocator.alloc(check.TypedCIR.Modules.SourceModule, lowering_module_envs.len);
    defer wrapped_allocator.free(source_modules);
    for (lowering_module_envs, 0..) |module_env, i| {
        source_modules[i] = .{ .precompiled = module_env };
    }
    var typed_modules = try check.TypedCIR.Modules.publish(wrapped_allocator, source_modules);
    defer typed_modules.deinit();

    var mono_lowerer = try monotype.Lower.Lowerer.init(
        wrapped_allocator,
        &typed_modules,
        builtin_module_idx,
        app_module_idx,
    );
    defer mono_lowerer.deinit();
    const entry_symbol = try mono_lowerer.specializeTopLevelDef(primary_module_idx, entry_def_idx);
    const mono = try mono_lowerer.run(primary_module_idx);
    const lifted = try monotype_lifted.Lower.run(wrapped_allocator, mono);
    const solved = try lambdasolved.Lower.run(wrapped_allocator, lifted);
    var executable = try lambdamono.Lower.runWithEntrypoints(wrapped_allocator, solved, &.{entry_symbol});
    const entrypoint_wrappers = executable.entrypoint_wrappers;
    executable.entrypoint_wrappers = &.{};
    defer if (entrypoint_wrappers.len > 0) wrapped_allocator.free(entrypoint_wrappers);
    const entry_symbol_for_lir = if (entrypoint_wrappers.len == 0)
        entry_symbol
    else
        entrypoint_wrappers[0];
    const lowered_ir = try ir.Lower.run(wrapped_allocator, executable);

    var lir_result = try lir.FromIr.run(
        wrapped_allocator,
        lowering_module_envs_const,
        builtin_str,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lir_result.deinit();
    try lir.Ownership.inferProcResultContracts(wrapped_allocator, &lir_result.store, &lir_result.layouts);
    try lir.RcInsert.run(wrapped_allocator, &lir_result.store, &lir_result.layouts);

    const proc_id = lir_result.proc_ids_by_symbol.get(entry_symbol_for_lir.raw()) orelse {
        roc_ops.crash("Interpreter shim: entry proc not found");
        return error.EvaluationFailed;
    };
    const proc = lir_result.store.getProcSpec(proc_id);
    const arg_locals = lir_result.store.getLocalSpan(proc.args);
    var arg_layouts = try wrapped_allocator.alloc(layout.Idx, arg_locals.len);
    defer wrapped_allocator.free(arg_layouts);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = lir_result.store.getLocal(local_id).layout_idx;
    }

    var interpreter = try eval.Interpreter.init(wrapped_allocator, &lir_result.store, &lir_result.layouts, roc_ops);
    defer interpreter.deinit();

    const eval_result = interpreter.eval(.{
        .proc_id = proc_id,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = arg_ptr,
        .ret_ptr = ret_ptr,
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.RuntimeError, error.DivisionByZero, error.Crash => return error.EvaluationFailed,
    };

    interpreter.dropValue(eval_result.value, proc.ret_layout);
}

/// Resolve arg_layouts and ret_layout from the CIR function type of an entrypoint expression.
fn resolveEntrypointLayouts(
    env_ptr: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    allocator: std.mem.Allocator,
) !struct { arg_layouts: []const layout.Idx, ret_layout: layout.Idx } {
    const expr_type_var = can.ModuleEnv.varFrom(expr_idx);
    const resolved = env_ptr.types.resolveVar(expr_type_var);
    const maybe_func = resolved.desc.content.unwrapFunc();

    if (maybe_func) |func| {
        const arg_vars = env_ptr.types.sliceVars(func.args);
        var arg_layouts = try allocator.alloc(layout.Idx, arg_vars.len);
        var type_scope = types.TypeScope.init(allocator);
        defer type_scope.deinit();
        for (arg_vars, 0..) |arg_var, i| {
            arg_layouts[i] = try layout.fromTypeVar(env_ptr, arg_var, &type_scope, null);
        }
        const ret_layout = try layout.fromTypeVar(env_ptr, func.ret, &type_scope, null);
        return .{ .arg_layouts = arg_layouts, .ret_layout = ret_layout };
    }

    // Non-function entrypoint (zero args) — the expression's type is the return type
    var type_scope = types.TypeScope.init(allocator);
    defer type_scope.deinit();
    const ret_layout = try layout.fromTypeVar(env_ptr, expr_type_var, &type_scope, null);
    return .{ .arg_layouts = &.{}, .ret_layout = ret_layout };
}

/// Result of setting up module environments
const SetupResult = struct {
    primary_env: *ModuleEnv,
    app_env: *ModuleEnv,
};

/// Set up ModuleEnv from serialized data with proper relocation (multi-module format)
fn setupModuleEnv(roc_ops: *RocOps) ShimError!SetupResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var buf: [256]u8 = undefined;
    const base_ptr = roc__serialized_base_ptr.?;
    const allocator = wrapped_allocator;

    // Check for portable serialized format
    const magic = std.mem.readInt(u32, base_ptr[0..4], .little);
    if (magic == SERIALIZED_FORMAT_MAGIC) {
        return setupModuleEnvFromSerialized(roc_ops, base_ptr, allocator);
    }

    // Legacy format
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;
    const module_count = header_ptr.module_count;

    // Cache header info
    global_entry_count = header_ptr.entry_count;
    global_def_indices_offset = header_ptr.def_indices_offset;
    global_is_serialized_format = false;

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    const offset: i64 = @as(i64, @intCast(child_base_addr)) - @as(i64, @intCast(parent_base_addr));

    // In debug mode, verify that the offset is aligned to the serialization alignment
    if (comptime builtin.mode == .Debug) {
        const REQUIRED_ALIGNMENT: u64 = collections.SERIALIZATION_ALIGNMENT.toByteUnits();
        const abs_offset: u64 = @abs(offset);
        if (abs_offset % REQUIRED_ALIGNMENT != 0) {
            const err_msg = std.fmt.bufPrint(&buf, "Relocation offset 0x{x} not {}-byte aligned! parent=0x{x} child=0x{x}", .{
                abs_offset,
                REQUIRED_ALIGNMENT,
                parent_base_addr,
                child_base_addr,
            }) catch "Relocation offset misaligned";
            std.debug.print("[MAIN] {s}\n", .{err_msg});
            roc_ops.crash(err_msg);
            return error.MemoryLayoutInvalid;
        }
    }

    // Sanity check for overflow potential
    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        const err_msg = std.fmt.bufPrint(&buf, "Relocation offset too large: {}", .{offset}) catch "Relocation offset too large";
        roc_ops.crash(err_msg);
        return error.ModuleEnvSetupFailed;
    }

    // Get module env offsets array
    const module_envs_base_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.module_envs_offset));

    // Verify alignment before @ptrFromInt
    if (comptime builtin.mode == .Debug) {
        if (module_envs_base_addr % @alignOf(u64) != 0) {
            const err_msg = std.fmt.bufPrint(&buf, "module_envs_base_addr misaligned: addr=0x{x}, base=0x{x}, offset=0x{x}", .{
                module_envs_base_addr,
                @intFromPtr(base_ptr),
                header_ptr.module_envs_offset,
            }) catch "module_envs_base_addr misaligned";
            roc_ops.crash(err_msg);
            return error.MemoryLayoutInvalid;
        }
    }

    const module_env_offsets: [*]const u64 = @ptrFromInt(module_envs_base_addr);

    // Load all module envs (platform modules first, app module last)
    // The app module is always the last one in the array
    var imported_envs = allocator.alloc(*const ModuleEnv, module_count - 1) catch {
        roc_ops.crash("Failed to allocate imported envs array");
        return error.OutOfMemory;
    };
    var unique_module_envs = std.ArrayList(*ModuleEnv).empty;
    defer unique_module_envs.deinit(allocator);

    // Relocate platform modules first (indices 0 to module_count-2)
    for (0..module_count - 1) |i| {
        const module_env_offset = module_env_offsets[i];
        const module_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(module_env_offset));

        // Verify alignment before @ptrFromInt
        if (comptime builtin.mode == .Debug) {
            if (module_env_addr % @alignOf(ModuleEnv) != 0) {
                const err_msg = std.fmt.bufPrint(&buf, "module_env_addr[{}] misaligned: addr=0x{x}, offset=0x{x}", .{
                    i,
                    module_env_addr,
                    module_env_offset,
                }) catch "module_env_addr misaligned";
                roc_ops.crash(err_msg);
                return error.MemoryLayoutInvalid;
            }
        }

        const module_env_ptr: *ModuleEnv = @ptrFromInt(module_env_addr);
        module_env_ptr.relocate(@intCast(offset));
        try appendModuleEnvIfMissing(allocator, &unique_module_envs, module_env_ptr);
        imported_envs[i] = module_env_ptr;
    }

    // Store imported envs globally
    global_imported_envs = imported_envs;

    // Get and relocate the app module using the header's app_env_offset
    const app_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.app_env_offset));

    // Verify alignment before @ptrFromInt
    if (comptime builtin.mode == .Debug) {
        if (app_env_addr % @alignOf(ModuleEnv) != 0) {
            const err_msg = std.fmt.bufPrint(&buf, "app_env_addr misaligned: addr=0x{x}, offset=0x{x}", .{
                app_env_addr,
                header_ptr.app_env_offset,
            }) catch "app_env_addr misaligned";
            roc_ops.crash(err_msg);
            return error.MemoryLayoutInvalid;
        }
    }

    const app_env_ptr: *ModuleEnv = @ptrFromInt(app_env_addr);
    app_env_ptr.relocate(@intCast(offset));
    try appendModuleEnvIfMissing(allocator, &unique_module_envs, app_env_ptr);

    // Determine primary env: platform main if available, otherwise app
    const primary_env: *ModuleEnv = if (header_ptr.platform_main_env_offset != 0) blk: {
        const platform_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.platform_main_env_offset));

        // Verify alignment before @ptrFromInt
        if (comptime builtin.mode == .Debug) {
            if (platform_env_addr % @alignOf(ModuleEnv) != 0) {
                const err_msg = std.fmt.bufPrint(&buf, "platform_env_addr misaligned: addr=0x{x}, offset=0x{x}", .{
                    platform_env_addr,
                    header_ptr.platform_main_env_offset,
                }) catch "platform_env_addr misaligned";
                roc_ops.crash(err_msg);
                return error.MemoryLayoutInvalid;
            }
        }

        const platform_env_ptr: *ModuleEnv = @ptrFromInt(platform_env_addr);
        platform_env_ptr.relocate(@intCast(offset));
        try appendModuleEnvIfMissing(allocator, &unique_module_envs, platform_env_ptr);
        break :blk platform_env_ptr;
    } else app_env_ptr;

    for (unique_module_envs.items) |module_env| {
        try rebuildLegacyRuntimeState(roc_ops, module_env, allocator);
    }

    return SetupResult{
        .primary_env = primary_env,
        .app_env = app_env_ptr,
    };
}

/// Set up ModuleEnv from portable serialized format (cross-architecture builds)
/// This format uses ModuleEnv.Serialized with fixed-size types
fn setupModuleEnvFromSerialized(roc_ops: *RocOps, base_ptr: [*]align(1) u8, allocator: std.mem.Allocator) ShimError!SetupResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var buf: [256]u8 = undefined;

    // Read the serialized header (use unaligned reads since embedded data may not be aligned)
    // Header layout: magic(4) + format_version(4) + module_count(4) + entry_count(4) +
    //                primary_env_index(4) + app_env_index(4) + def_indices_offset(8) + module_infos_offset(8)
    const header_magic = std.mem.readInt(u32, base_ptr[0..4], .little);
    if (header_magic != SERIALIZED_FORMAT_MAGIC) {
        const err_msg = std.fmt.bufPrint(&buf, "Invalid magic number: 0x{x}", .{header_magic}) catch "Invalid magic number";
        roc_ops.crash(err_msg);
        return error.MemoryLayoutInvalid;
    }

    const format_version = std.mem.readInt(u32, base_ptr[4..8], .little);
    if (format_version != 1) {
        const err_msg = std.fmt.bufPrint(&buf, "Unsupported serialized format version: {}", .{format_version}) catch "Unsupported format version";
        roc_ops.crash(err_msg);
        return error.MemoryLayoutInvalid;
    }

    const module_count = std.mem.readInt(u32, base_ptr[8..12], .little);
    const entry_count = std.mem.readInt(u32, base_ptr[12..16], .little);
    const primary_env_index = std.mem.readInt(u32, base_ptr[16..20], .little);
    const app_env_index = std.mem.readInt(u32, base_ptr[20..24], .little);
    const def_indices_offset = std.mem.readInt(u64, base_ptr[24..32], .little);
    const module_infos_offset = std.mem.readInt(u64, base_ptr[32..40], .little);

    // Store header info in globals for use during evaluation
    global_entry_count = entry_count;
    global_def_indices_offset = def_indices_offset;
    global_is_serialized_format = true;

    // Get module infos array address
    const module_infos_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(module_infos_offset));
    // For SerializedModuleInfo, we also need to use unaligned reads
    const module_infos_bytes: [*]const u8 = @ptrFromInt(module_infos_addr);

    // Allocate storage for pointers to deserialized ModuleEnvs
    // Note: deserialize() overwrites the Serialized struct in place, so the returned
    // pointer points to the same memory location (now reinterpreted as ModuleEnv)
    var env_ptrs = allocator.alloc(*ModuleEnv, module_count) catch {
        roc_ops.crash("Failed to allocate ModuleEnv pointer array");
        return error.OutOfMemory;
    };
    // Note: Don't free - these are used for the lifetime of the process

    // Each SerializedModuleInfo is 40 bytes: source_offset(8) + source_len(8) + module_name_offset(8) +
    //                                        module_name_len(8) + env_serialized_offset(8)
    const MODULE_INFO_SIZE: usize = 40;

    // Deserialize each module
    for (0..module_count) |i| {
        const info_base = module_infos_bytes + (i * MODULE_INFO_SIZE);

        // Read SerializedModuleInfo fields using unaligned reads
        const source_offset = std.mem.readInt(u64, info_base[0..8], .little);
        const source_len = std.mem.readInt(u64, info_base[8..16], .little);
        const module_name_offset = std.mem.readInt(u64, info_base[16..24], .little);
        const module_name_len = std.mem.readInt(u64, info_base[24..32], .little);
        const env_serialized_offset = std.mem.readInt(u64, info_base[32..40], .little);

        // Get source bytes
        const source_ptr = base_ptr + @as(usize, @intCast(source_offset));
        const source = source_ptr[0..@as(usize, @intCast(source_len))];

        // Get module name
        const name_ptr = base_ptr + @as(usize, @intCast(module_name_offset));
        const module_name = name_ptr[0..@as(usize, @intCast(module_name_len))];

        // Get serialized env address
        // Note: ModuleEnv.Serialized.deserialize() requires proper alignment.
        // The serialization code should ensure this, but if it doesn't, we'll crash here.
        const env_serialized_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(env_serialized_offset));
        const env_serialized: *ModuleEnv.Serialized = @ptrFromInt(env_serialized_addr);

        // Deserialize the ModuleEnv
        // The base parameter is the buffer base address - serialized offsets are relative to buffer start
        env_ptrs[i] = env_serialized.deserializeInto(
            @intFromPtr(base_ptr), // buffer base address
            allocator,
            source,
            module_name,
        ) catch |err| {
            const err_msg = std.fmt.bufPrint(&buf, "Failed to deserialize module {}: {s}", .{ i, @errorName(err) }) catch "Failed to deserialize module";
            roc_ops.crash(err_msg);
            return error.ModuleEnvSetupFailed;
        };
    }

    // Build imported_envs array (all modules except app)
    if (module_count > 1) {
        var imported_envs = allocator.alloc(*const ModuleEnv, module_count - 1) catch {
            roc_ops.crash("Failed to allocate imported envs array");
            return error.OutOfMemory;
        };
        var j: usize = 0;
        for (0..module_count) |i| {
            if (i != app_env_index) {
                imported_envs[j] = env_ptrs[i];
                j += 1;
            }
        }
        global_imported_envs = imported_envs;
    }

    return SetupResult{
        .primary_env = env_ptrs[primary_env_index],
        .app_env = env_ptrs[app_env_index],
    };
}

/// Create interpreter instance (global setup was done in initializeOnce)
/// This is now lightweight and safe to call per-evaluation since it doesn't modify global state.
fn createInterpreter(env_ptr: *ModuleEnv, app_env: ?*ModuleEnv, builtin_modules: *const eval.BuiltinModules, roc_ops: *RocOps) ShimError!Interpreter {
    const trace = tracy.trace(@src());
    defer trace.end();

    const allocator = wrapped_allocator;

    // Use builtin types from the loaded builtin modules
    const builtin_types = builtin_modules.asBuiltinTypes();
    const builtin_module_env = builtin_modules.builtin_module.env;

    // Create a copy of the global imported_envs slice for this interpreter instance
    // The interpreter takes ownership and will free this on deinit
    const global_envs = global_full_imported_envs.?;
    const imported_envs = allocator.dupe(*const can.ModuleEnv, global_envs) catch {
        roc_ops.crash("Failed to duplicate imported envs slice");
        return error.OutOfMemory;
    };

    traceDbg(roc_ops, "=== Creating Interpreter ===", .{});
    traceDbg(roc_ops, "imported_envs.len={d}, primary=\"{s}\"", .{ imported_envs.len, env_ptr.module_name });

    var interpreter = eval.Interpreter.init(allocator, env_ptr, builtin_types, builtin_module_env, imported_envs, getShimImportMapping(), app_env, global_constant_strings_arena, roc_target.RocTarget.detectNative()) catch {
        roc_ops.crash("INTERPRETER SHIM: Interpreter initialization failed");
        return error.InterpreterSetupFailed;
    };

    // Setup for-clause type mappings from platform to app
    interpreter.setupForClauseTypeMappings(env_ptr) catch {
        roc_ops.crash("INTERPRETER SHIM: Failed to setup for-clause type mappings");
        return error.InterpreterSetupFailed;
    };

    return interpreter;
}

//! A shim to read the ModuleEnv from shared memory and JIT-compile
//! Roc code using the dev backend (CIR → MIR → LIR → native machine code).
//!
//! Adapted from the interpreter shim. Instead of interpreting CIR directly,
//! this shim compiles to native code via the dev backend codegen and executes via
//! ExecutableMemory (mmap/mprotect).
//!
//! No wasm32 support — the dev backend targets x86_64/aarch64 only.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const builtins = @import("builtins");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const types = @import("types");
const collections = @import("collections");
const eval = @import("eval");
const compile = @import("compile");
const layout = @import("layout");
const tracy = @import("tracy");
const backend = @import("backend");
const ir = @import("ir");
const lir = @import("lir");
const lambdamono = @import("lambdamono");
const lambdasolved = @import("lambdasolved");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const symbol = @import("symbol");

// Module tracing flag - enabled via `zig build -Dtrace-modules`
const trace_modules = if (@hasDecl(build_options, "trace_modules")) build_options.trace_modules else false;

fn traceDbg(comptime fmt: []const u8, args: anytype) void {
    if (comptime trace_modules) {
        std.debug.print("[TRACE-MODULES] " ++ fmt ++ "\n", args);
    }
}

const ipc = @import("ipc");

// Debug allocator for native platforms - provides leak detection in Debug/ReleaseSafe builds
var debug_allocator: std.heap.DebugAllocator(.{}) = .{ .backing_allocator = std.heap.c_allocator };

fn getBaseAllocator() std.mem.Allocator {
    return switch (builtin.mode) {
        .Debug, .ReleaseSafe => debug_allocator.allocator(),
        .ReleaseFast, .ReleaseSmall => std.heap.c_allocator,
    };
}

// TracyAllocator wrapping for allocation profiling
var tracy_allocator: tracy.TracyAllocator(null) = undefined;
var wrapped_allocator: std.mem.Allocator = undefined;
var allocator_initialized: bool = false;

const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

/// Thread-safe initialization flag with atomic ordering.
const InitializationFlag = struct {
    inner: std.atomic.Value(bool),

    const Self = @This();

    pub fn init() Self {
        return .{ .inner = std.atomic.Value(bool).init(false) };
    }

    pub fn isSet(self: *const Self) bool {
        return self.inner.load(.acquire);
    }

    pub fn set(self: *Self) void {
        self.inner.store(true, .release);
    }
};

/// Mutex for thread-safe initialization.
const PlatformMutex = struct {
    inner: std.Thread.Mutex,

    const Self = @This();

    pub fn init() Self {
        return .{ .inner = .{} };
    }

    pub fn lock(self: *Self) void {
        self.inner.lock();
    }

    pub fn unlock(self: *Self) void {
        self.inner.unlock();
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
var global_shm: ?SharedMemoryAllocator = null;
var global_env_ptr: ?*ModuleEnv = null; // Primary env for entry point lookups (platform or app)
var global_app_env_ptr: ?*ModuleEnv = null; // App env for e_lookup_required resolution
var global_builtin_modules: ?eval.BuiltinModules = null;
var global_imported_envs: ?[]*const ModuleEnv = null;
var global_full_imported_envs: ?[]*const ModuleEnv = null; // Full slice with builtin prepended (for import resolution)
var global_full_mutable_envs: ?[]*ModuleEnv = null; // All module envs used for MIR lowering/codegen
var global_full_mutable_envs_const: ?[]*const ModuleEnv = null; // Borrowed const view of global_full_mutable_envs
var global_builtin_module_idx: ?u32 = null;
var global_primary_module_idx: ?u32 = null;
var global_app_module_idx: ?u32 = null;
var global_builtin_str: ?base.Ident.Idx = null;
var shm_mutex = PlatformMutex.init();

// Cached header info (set during initialization, used for evaluation)
var global_entry_count: u32 = 0;
var global_def_indices_offset: u64 = 0;
var global_is_serialized_format: bool = false;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const safe_memory = base.safe_memory;

// Constants for shared memory layout
const FIRST_ALLOC_OFFSET = 504; // 0x1f8 - First allocation starts at this offset

// Header structure that matches the one in main.zig (multi-module format)
const Header = struct {
    parent_base_addr: u64,
    module_count: u32,
    entry_count: u32,
    def_indices_offset: u64,
    module_envs_offset: u64,
    platform_main_env_offset: u64,
    app_env_offset: u64,
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
        roc_ops.crash("Dev shim: failed to rebuild dependency graph for legacy module");
        return error.ModuleEnvSetupFailed;
    };
    defer graph.deinit();

    var eval_order = can.DependencyGraph.computeSCCs(&graph, allocator) catch {
        roc_ops.crash("Dev shim: failed to rebuild evaluation order for legacy module");
        return error.ModuleEnvSetupFailed;
    };
    const eval_order_ptr = allocator.create(can.DependencyGraph.EvaluationOrder) catch {
        eval_order.deinit();
        roc_ops.crash("Dev shim: failed to allocate evaluation order for legacy module");
        return error.OutOfMemory;
    };
    eval_order_ptr.* = eval_order;
    module_env.evaluation_order = eval_order_ptr;
}

const SERIALIZED_FORMAT_MAGIC = collections.SERIALIZED_FORMAT_MAGIC;

/// Comprehensive error handling for the shim
const ShimError = error{
    SharedMemoryError,
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
    CodeGenFailed,
    ExecutionFailed,
    EmptyCode,
    MmapFailed,
    MprotectFailed,
    Crash,
} || safe_memory.MemoryError;

/// Exported symbol that reads ModuleEnv from shared memory, compiles to native code, and executes.
export fn roc_entrypoint(entry_idx: u32, ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void {
    const trace = tracy.trace(@src());
    defer trace.end();

    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.Crash, error.StackOverflow => {},
        else => {
            var buf: [256]u8 = undefined;
            const msg2 = std.fmt.bufPrint(&buf, "Dev shim error: {s}", .{@errorName(err)}) catch "Dev shim evaluation error";
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

    // IPC path: read from shared memory
    if (roc__serialized_base_ptr == null) {
        const page_size = SharedMemoryAllocator.getSystemPageSize() catch 4096;

        var shm = SharedMemoryAllocator.fromCoordination(allocator, page_size) catch |err| {
            const msg2 = std.fmt.bufPrint(&buf, "Failed to create shared memory allocator: {s}", .{@errorName(err)}) catch "Failed to create shared memory allocator";
            roc_ops.crash(msg2);
            return error.SharedMemoryError;
        };

        const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(Header);
        if (shm.total_size < min_required_size) {
            const msg = std.fmt.bufPrint(&buf, "Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size }) catch "Invalid memory layout";
            roc_ops.crash(msg);
            return error.MemoryLayoutInvalid;
        }

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

    // Build the codegen slice in import-resolution order, then append the
    // primary and app envs if they are not already present.
    var all_codegen_envs = std.ArrayList(*ModuleEnv).empty;
    defer all_codegen_envs.deinit(allocator);

    for (full_imported_envs) |env| {
        all_codegen_envs.append(allocator, @constCast(env)) catch {
            roc_ops.crash("Failed to build codegen envs list");
            return error.OutOfMemory;
        };
    }

    appendModuleEnvIfMissing(allocator, &all_codegen_envs, setup_result.primary_env) catch {
        roc_ops.crash("Failed to add primary env to codegen envs");
        return error.OutOfMemory;
    };
    appendModuleEnvIfMissing(allocator, &all_codegen_envs, setup_result.app_env) catch {
        roc_ops.crash("Failed to add app env to codegen envs");
        return error.OutOfMemory;
    };

    const mutable_envs = all_codegen_envs.toOwnedSlice(allocator) catch {
        roc_ops.crash("Failed to allocate mutable envs");
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
        roc_ops.crash("DEV SHIM: builtin module missing from lowering envs");
        return error.ModuleEnvSetupFailed;
    };
    global_primary_module_idx = compile.runner.findModuleEnvIdx(mutable_envs, setup_result.primary_env) orelse {
        roc_ops.crash("DEV SHIM: primary module missing from lowering envs");
        return error.ModuleEnvSetupFailed;
    };
    global_app_module_idx = compile.runner.findModuleEnvIdx(mutable_envs, setup_result.app_env) orelse {
        roc_ops.crash("DEV SHIM: app module missing from lowering envs");
        return error.ModuleEnvSetupFailed;
    };
    global_builtin_str = builtin_module_env.idents.builtin_str;

    // Resolve imports for all modules
    const env_ptr = setup_result.primary_env;
    const app_env = setup_result.app_env;

    traceDbg("Resolving imports for primary env \"{s}\"", .{env_ptr.module_name});
    env_ptr.imports.resolveImports(env_ptr, full_imported_envs);

    if (app_env != env_ptr) {
        traceDbg("Resolving imports for app env \"{s}\"", .{app_env.module_name});
        app_env.imports.resolveImports(app_env, full_imported_envs);
    }

    traceDbg("Re-resolving imports for all imported modules", .{});
    for (full_imported_envs) |imp_env| {
        traceDbg("  Re-resolving for \"{s}\"", .{imp_env.module_name});
        @constCast(imp_env).imports.resolveImports(imp_env, full_imported_envs);
    }

    // Enable runtime inserts on all deserialized module environments
    env_ptr.common.idents.interner.enableRuntimeInserts(allocator) catch {
        roc_ops.crash("DEV SHIM: Failed to enable runtime inserts on platform env");
        return error.ModuleEnvSetupFailed;
    };
    if (app_env != env_ptr) {
        @constCast(app_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
            roc_ops.crash("DEV SHIM: Failed to enable runtime inserts on app env");
            return error.ModuleEnvSetupFailed;
        };
    }
    if (global_imported_envs) |imported_envs| {
        for (imported_envs) |imp_env| {
            @constCast(imp_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
                roc_ops.crash("DEV SHIM: Failed to enable runtime inserts on imported env");
                return error.ModuleEnvSetupFailed;
            };
        }
    }

    // Mark initialization done
    shared_memory_initialized.set();
}

/// Evaluate an entrypoint by reading ModuleEnv from shared memory and JIT-compiling it
fn evaluateFromSharedMemory(entry_idx: u32, host_roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize shared memory + module env (only once)
    try initializeOnce(host_roc_ops);

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
        host_roc_ops.crash("Invalid entry index");
        return error.InvalidEntryIndex;
    }

    // Look up entrypoint index from shared memory
    const base_ptr = roc__serialized_base_ptr.?;
    const def_indices_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(global_def_indices_offset));
    const def_indices: [*]const u32 = @ptrFromInt(def_indices_addr);
    const def_idx = def_indices[entry_idx];

    // Find the expression in the primary env (platform or app)
    const entry_def_id: CIR.Def.Idx = @enumFromInt(def_idx);
    const entrypoint_expr = env_ptr.store.getDef(entry_def_id).expr;
    // Find the CIR def index for the entrypoint expression.
    const def_ids = env_ptr.store.sliceDefs(env_ptr.all_defs);
    var entry_def_idx: ?CIR.Def.Idx = null;
    for (def_ids) |def_idx_candidate| {
        const def = env_ptr.store.getDef(def_idx_candidate);
        if (def.expr == entrypoint_expr) {
            entry_def_idx = def_idx_candidate;
            break;
        }
    }
    if (entry_def_idx == null) {
        host_roc_ops.crash("Dev shim: failed to resolve entrypoint def");
        return error.EvaluationFailed;
    }

    var source_modules = try wrapped_allocator.alloc(check.TypedCIR.Modules.SourceModule, lowering_module_envs.len);
    defer wrapped_allocator.free(source_modules);
    for (lowering_module_envs, 0..) |module_env, i| {
        source_modules[i] = .{ .precompiled = module_env };
    }

    var typed_modules = try check.TypedCIR.Modules.init(wrapped_allocator, source_modules);
    defer typed_modules.deinit();

    var mono_lowerer = try monotype.Lower.Lowerer.init(
        wrapped_allocator,
        &typed_modules,
        builtin_module_idx,
        app_module_idx,
    );
    defer mono_lowerer.deinit();
    const entry_symbol = try mono_lowerer.specializeTopLevelDef(primary_module_idx, entry_def_idx.?);
    const mono = try mono_lowerer.run(primary_module_idx);
    const lifted = try monotype_lifted.Lower.run(wrapped_allocator, mono);
    const solved = try lambdasolved.Lower.run(wrapped_allocator, lifted);
    const entrypoints = [_]symbol.Symbol{entry_symbol};
    var mono_executable = try lambdamono.Lower.runWithEntrypoints(wrapped_allocator, solved, &entrypoints);
    const entrypoint_wrappers = mono_executable.entrypoint_wrappers;
    mono_executable.entrypoint_wrappers = &.{};
    defer if (entrypoint_wrappers.len > 0) wrapped_allocator.free(entrypoint_wrappers);
    const entry_symbol_for_lir = if (entrypoint_wrappers.len == 0)
        entry_symbol
    else
        entrypoint_wrappers[0];

    const lowered_ir = try ir.Lower.run(wrapped_allocator, mono_executable);

    var lowered_lir = try lir.FromIr.run(
        wrapped_allocator,
        lowering_module_envs_const,
        builtin_str,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lowered_lir.deinit();
    try lir.Ownership.inferProcResultContracts(wrapped_allocator, &lowered_lir.store, &lowered_lir.layouts);
    try lir.RcInsert.run(wrapped_allocator, &lowered_lir.store, &lowered_lir.layouts);

    const proc_id = lowered_lir.proc_ids_by_symbol.get(entry_symbol_for_lir.raw()) orelse {
        host_roc_ops.crash("Dev shim: entry proc not found");
        return error.CodeGenFailed;
    };

    var codegen = try backend.HostLirCodeGen.init(
        wrapped_allocator,
        &lowered_lir.store,
        &lowered_lir.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(lowered_lir.store.getProcSpecs());

    const proc_spec = lowered_lir.store.getProcSpec(proc_id);
    const arg_locals = lowered_lir.store.getLocalSpan(proc_spec.args);
    var arg_layouts = try wrapped_allocator.alloc(layout.Idx, arg_locals.len);
    defer wrapped_allocator.free(arg_layouts);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = lowered_lir.store.getLocal(local_id).layout_idx;
    }

    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_dev_shim_entrypoint",
        proc_id,
        arg_layouts,
        proc_spec.ret_layout,
    );
    var executable = try backend.ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer executable.deinit();

    executable.callRocABI(@ptrCast(host_roc_ops), ret_ptr, arg_ptr);
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

    global_entry_count = header_ptr.entry_count;
    global_def_indices_offset = header_ptr.def_indices_offset;
    global_is_serialized_format = false;

    const child_base_addr = @intFromPtr(base_ptr);
    const offset: i64 = @as(i64, @intCast(child_base_addr)) - @as(i64, @intCast(parent_base_addr));

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

    if (@abs(offset) > std.math.maxInt(isize) / 2) {
        const err_msg = std.fmt.bufPrint(&buf, "Relocation offset too large: {}", .{offset}) catch "Relocation offset too large";
        roc_ops.crash(err_msg);
        return error.ModuleEnvSetupFailed;
    }

    const module_envs_base_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.module_envs_offset));

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

    var imported_envs = allocator.alloc(*const ModuleEnv, module_count - 1) catch {
        roc_ops.crash("Failed to allocate imported envs array");
        return error.OutOfMemory;
    };
    var unique_module_envs = std.ArrayList(*ModuleEnv).empty;
    defer unique_module_envs.deinit(allocator);

    for (0..module_count - 1) |i| {
        const module_env_offset = module_env_offsets[i];
        const module_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(module_env_offset));

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

    global_imported_envs = imported_envs;

    const app_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.app_env_offset));

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

    const primary_env: *ModuleEnv = if (header_ptr.platform_main_env_offset != 0) blk: {
        const platform_env_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(header_ptr.platform_main_env_offset));

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
fn setupModuleEnvFromSerialized(roc_ops: *RocOps, base_ptr: [*]align(1) u8, allocator: std.mem.Allocator) ShimError!SetupResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var buf: [256]u8 = undefined;

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

    global_entry_count = entry_count;
    global_def_indices_offset = def_indices_offset;
    global_is_serialized_format = true;

    const module_infos_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(module_infos_offset));
    const module_infos_bytes: [*]const u8 = @ptrFromInt(module_infos_addr);

    var env_ptrs = allocator.alloc(*ModuleEnv, module_count) catch {
        roc_ops.crash("Failed to allocate ModuleEnv pointer array");
        return error.OutOfMemory;
    };

    const MODULE_INFO_SIZE: usize = 40;

    for (0..module_count) |i| {
        const info_base = module_infos_bytes + (i * MODULE_INFO_SIZE);

        const source_offset = std.mem.readInt(u64, info_base[0..8], .little);
        const source_len = std.mem.readInt(u64, info_base[8..16], .little);
        const module_name_offset = std.mem.readInt(u64, info_base[16..24], .little);
        const module_name_len = std.mem.readInt(u64, info_base[24..32], .little);
        const env_serialized_offset = std.mem.readInt(u64, info_base[32..40], .little);

        const source_ptr = base_ptr + @as(usize, @intCast(source_offset));
        const source = source_ptr[0..@as(usize, @intCast(source_len))];

        const name_ptr = base_ptr + @as(usize, @intCast(module_name_offset));
        const module_name = name_ptr[0..@as(usize, @intCast(module_name_len))];

        const env_serialized_addr = @intFromPtr(base_ptr) + @as(usize, @intCast(env_serialized_offset));
        const env_serialized: *ModuleEnv.Serialized = @ptrFromInt(env_serialized_addr);

        env_ptrs[i] = env_serialized.deserializeInto(
            @intFromPtr(base_ptr),
            allocator,
            source,
            module_name,
        ) catch |err| {
            const err_msg = std.fmt.bufPrint(&buf, "Failed to deserialize module {}: {s}", .{ i, @errorName(err) }) catch "Failed to deserialize module";
            roc_ops.crash(err_msg);
            return error.ModuleEnvSetupFailed;
        };
    }

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

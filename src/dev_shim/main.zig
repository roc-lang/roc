//! A shim to read the ModuleEnv from shared memory and JIT-compile
//! Roc code using the dev backend (CIR → MIR → LIR → native machine code).
//!
//! Adapted from the interpreter shim. Instead of interpreting CIR directly,
//! this shim compiles to native code via DevEvaluator and executes via
//! ExecutableMemory (mmap/mprotect).
//!
//! No wasm32 support — the dev backend targets x86_64/aarch64 only.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const builtins = @import("builtins");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const collections = @import("collections");
const eval = @import("eval");
const layout = @import("layout");
const tracy = @import("tracy");
const backend = @import("backend");

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
    inner: std.Io.Mutex,

    const Self = @This();

    pub fn init() Self {
        return .{ .inner = std.Io.Mutex.init };
    }

    pub fn lock(self: *Self) void {
        self.inner.lockUncancelable(std.Options.debug_io);
    }

    pub fn unlock(self: *Self) void {
        self.inner.unlock(std.Options.debug_io);
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
var global_dev_evaluator: ?eval.DevEvaluator = null; // Dev evaluator instance
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

const SERIALIZED_FORMAT_MAGIC = collections.SERIALIZED_FORMAT_MAGIC;

/// Comprehensive error handling for the shim
const ShimError = error{
    SharedMemoryError,
    DevEvaluatorSetupFailed,
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

        var shm = SharedMemoryAllocator.fromCoordination(allocator, std.Options.debug_io, page_size) catch |err| {
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
        return error.DevEvaluatorSetupFailed;
    };
    if (app_env != env_ptr) {
        @constCast(app_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
            roc_ops.crash("DEV SHIM: Failed to enable runtime inserts on app env");
            return error.DevEvaluatorSetupFailed;
        };
    }
    for (full_imported_envs) |imp_env| {
        @constCast(imp_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
            roc_ops.crash("DEV SHIM: Failed to enable runtime inserts on imported env");
            return error.DevEvaluatorSetupFailed;
        };
    }

    // Fix up display_module_name_idx for deserialized modules
    if (env_ptr.display_module_name_idx.isNone() and env_ptr.module_name.len > 0) {
        env_ptr.display_module_name_idx = env_ptr.insertIdent(base.Ident.for_text(env_ptr.module_name)) catch {
            roc_ops.crash("DEV SHIM: Failed to insert module name for platform env");
            return error.DevEvaluatorSetupFailed;
        };
    }
    if (env_ptr.qualified_module_ident.isNone() and !env_ptr.display_module_name_idx.isNone()) {
        env_ptr.qualified_module_ident = env_ptr.display_module_name_idx;
    }
    if (app_env != env_ptr) {
        if (app_env.display_module_name_idx.isNone() and app_env.module_name.len > 0) {
            @constCast(app_env).display_module_name_idx = @constCast(app_env).insertIdent(base.Ident.for_text(app_env.module_name)) catch {
                roc_ops.crash("DEV SHIM: Failed to insert module name for app env");
                return error.DevEvaluatorSetupFailed;
            };
        }
        if (app_env.qualified_module_ident.isNone() and !app_env.display_module_name_idx.isNone()) {
            @constCast(app_env).qualified_module_ident = app_env.display_module_name_idx;
        }
    }
    for (full_imported_envs) |imp_env| {
        if (imp_env.display_module_name_idx.isNone() and imp_env.module_name.len > 0) {
            @constCast(imp_env).display_module_name_idx = @constCast(imp_env).insertIdent(base.Ident.for_text(imp_env.module_name)) catch {
                roc_ops.crash("DEV SHIM: Failed to insert module name for imported env");
                return error.DevEvaluatorSetupFailed;
            };
        }
        if (imp_env.qualified_module_ident.isNone() and !imp_env.display_module_name_idx.isNone()) {
            @constCast(imp_env).qualified_module_ident = imp_env.display_module_name_idx;
        }
    }

    // Initialize the DevEvaluator once per process
    global_dev_evaluator = eval.DevEvaluator.init(allocator, null) catch |err| {
        const msg2 = std.fmt.bufPrint(&buf, "Failed to initialize DevEvaluator: {s}", .{@errorName(err)}) catch "Failed to initialize DevEvaluator";
        roc_ops.crash(msg2);
        return error.DevEvaluatorSetupFailed;
    };

    // Mark as initialized (release semantics ensure all writes above are visible)
    shared_memory_initialized.set();
}

/// Compile CIR to native code and execute it
fn evaluateFromSharedMemory(entry_idx: u32, host_roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize shared memory once per process
    try initializeOnce(host_roc_ops);

    const env_ptr = global_env_ptr.?;
    const allocator = wrapped_allocator;

    // Get expression info using entry_idx
    const base_ptr = roc__serialized_base_ptr.?;
    var buf: [256]u8 = undefined;

    if (entry_idx >= global_entry_count) {
        const err_msg = std.fmt.bufPrint(&buf, "Invalid entry_idx {} >= entry_count {}", .{ entry_idx, global_entry_count }) catch "Invalid entry_idx";
        host_roc_ops.crash(err_msg);
        return error.InvalidEntryIndex;
    }

    const def_offset = global_def_indices_offset + entry_idx * @sizeOf(u32);
    const def_idx_raw: u32 = if (global_is_serialized_format) blk: {
        const byte_offset: usize = @intCast(def_offset);
        if (byte_offset + 4 > roc__serialized_size) {
            const err_msg = std.fmt.bufPrint(&buf, "def_idx out of bounds: offset={}, size={}", .{ byte_offset, roc__serialized_size }) catch "def_idx out of bounds";
            host_roc_ops.crash(err_msg);
            return error.MemoryLayoutInvalid;
        }
        const ptr: *const [4]u8 = @ptrCast(base_ptr + byte_offset);
        const val = std.mem.readInt(u32, ptr, .little);
        break :blk val;
    } else blk: {
        break :blk safe_memory.safeRead(u32, base_ptr, @intCast(def_offset), roc__serialized_size) catch |err| {
            const read_err = std.fmt.bufPrint(&buf, "Failed to read def_idx: {}", .{err}) catch "Failed to read def_idx";
            host_roc_ops.crash(read_err);
            return error.MemoryLayoutInvalid;
        };
    };
    const def_idx: CIR.Def.Idx = @enumFromInt(def_idx_raw);

    // Get the definition and extract its expression
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;

    traceDbg("Evaluating entry_idx={d}, def_idx={d}, expr_idx={d}", .{ entry_idx, def_idx_raw, @intFromEnum(expr_idx) });

    // Get all module envs for code generation (needs mutable pointers)
    const all_module_envs = global_full_mutable_envs.?;
    const app_env = global_app_env_ptr.?;

    // Get the global DevEvaluator
    var dev_eval = &global_dev_evaluator.?;

    // Resolve arg/ret layouts from the CIR function type
    const layouts = resolveEntrypointLayouts(env_ptr, expr_idx, dev_eval, all_module_envs, allocator) catch |err| {
        const err_msg = std.fmt.bufPrint(&buf, "Layout resolution failed: {s}", .{@errorName(err)}) catch "Layout resolution failed";
        host_roc_ops.crash(err_msg);
        return error.CodeGenFailed;
    };

    // Compile CIR → native code using entrypoint wrapper (RocCall ABI)
    var code_result = dev_eval.generateEntrypointCode(env_ptr, expr_idx, all_module_envs, app_env, layouts.arg_layouts, layouts.ret_layout) catch |err| {
        const err_msg = std.fmt.bufPrint(&buf, "Code generation failed: {s}", .{@errorName(err)}) catch "Code generation failed";
        host_roc_ops.crash(err_msg);
        return error.CodeGenFailed;
    };
    defer code_result.deinit();

    // Check for crash during code generation
    if (code_result.crash_message) |crash_msg| {
        host_roc_ops.crash(crash_msg);
        return error.Crash;
    }

    if (code_result.code.len == 0) {
        host_roc_ops.crash("Dev shim: code generation produced empty code");
        return error.CodeGenFailed;
    }

    // Create executable memory from generated code
    var executable = backend.ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset) catch {
        host_roc_ops.crash("Dev shim: failed to create executable memory");
        return error.ExecutionFailed;
    };
    defer executable.deinit();

    // Execute using RocCall ABI — pass host's RocOps directly so generated code
    // uses the host's allocator, hosted functions, and crash handlers.
    executable.callRocABI(@ptrCast(host_roc_ops), ret_ptr, arg_ptr);
}

/// Resolve arg_layouts and ret_layout from the CIR function type of an entrypoint expression.
fn resolveEntrypointLayouts(
    env_ptr: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    dev_eval: *eval.DevEvaluator,
    all_module_envs: []const *ModuleEnv,
    allocator: std.mem.Allocator,
) !struct { arg_layouts: []const layout.Idx, ret_layout: layout.Idx } {
    const layout_store_ptr = try dev_eval.ensureGlobalLayoutStore(all_module_envs);

    // Find the module index for this module
    const module_idx: u32 = for (all_module_envs, 0..) |env, i| {
        if (env == env_ptr) break @intCast(i);
    } else return error.ModuleEnvNotFound;

    // Get the expression's type variable and resolve it
    const expr_type_var = can.ModuleEnv.varFrom(expr_idx);
    const resolved = env_ptr.types.resolveVar(expr_type_var);
    const maybe_func = resolved.desc.content.unwrapFunc();

    if (maybe_func) |func| {
        const arg_vars = env_ptr.types.sliceVars(func.args);
        var arg_layouts = try allocator.alloc(layout.Idx, arg_vars.len);
        var type_scope = types.TypeScope.init(allocator);
        defer type_scope.deinit();
        for (arg_vars, 0..) |arg_var, i| {
            arg_layouts[i] = try layout_store_ptr.fromTypeVar(module_idx, arg_var, &type_scope, null);
        }
        const ret_layout = try layout_store_ptr.fromTypeVar(module_idx, func.ret, &type_scope, null);
        return .{ .arg_layouts = arg_layouts, .ret_layout = ret_layout };
    }

    // Non-function entrypoint (zero args) — the expression's type is the return type
    var type_scope = types.TypeScope.init(allocator);
    defer type_scope.deinit();
    const ret_layout = try layout_store_ptr.fromTypeVar(module_idx, expr_type_var, &type_scope, null);
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
        module_env_ptr.gpa = allocator;
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
    app_env_ptr.gpa = allocator;

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
        platform_env_ptr.gpa = allocator;
        break :blk platform_env_ptr;
    } else app_env_ptr;

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

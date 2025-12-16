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
const types = @import("types");
const collections = @import("collections");
const import_mapping_mod = types.import_mapping;
const eval = @import("eval");
const tracy = @import("tracy");

// Compile-time flag for module tracing - enabled via `zig build -Dtrace-modules`
// Disabled on wasm32-freestanding since std.debug.print is not available
const trace_modules = if (builtin.cpu.arch == .wasm32) false else if (@hasDecl(build_options, "trace_modules")) build_options.trace_modules else false;

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

fn wasmFree(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {
    // Bump allocator - no-op
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

// Import serialization types from the shared module
const SERIALIZED_FORMAT_MAGIC = collections.SERIALIZED_FORMAT_MAGIC;
const SerializedHeader = collections.SerializedHeader;
const SerializedModuleInfo = collections.SerializedModuleInfo;

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
    const trace = tracy.trace(@src());
    defer trace.end();

    evaluateFromSharedMemory(entry_idx, ops, ret_ptr, arg_ptr) catch |err| {
        // Only show this generic error if we haven't already crashed with a more specific message
        // (errors like Crash and StackOverflow already triggered roc_crashed with details)
        if (err != error.Crash and err != error.StackOverflow) {
            var buf: [256]u8 = undefined;
            const msg2 = std.fmt.bufPrint(&buf, "Error evaluating: {s}", .{@errorName(err)}) catch "Error evaluating";
            ops.crash(msg2);
        }
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

        // Validate memory layout - we need at least space for the header
        const min_required_size = FIRST_ALLOC_OFFSET + @sizeOf(Header);
        if (shm.total_size < min_required_size) {
            const msg = std.fmt.bufPrint(&buf, "Invalid memory layout: size {} is too small (minimum required: {})", .{ shm.total_size, min_required_size }) catch "Invalid memory layout";
            roc_ops.crash(msg);
            return error.MemoryLayoutInvalid;
        }

        // setup base pointer
        roc__serialized_base_ptr = shm.getBasePtr();
        roc__serialized_size = shm.total_size;
    }

    // For wasm32 embedded mode: roc__serialized_base_ptr must be set by linker
    if (is_wasm32 and roc__serialized_base_ptr == null) {
        roc_ops.crash("wasm32: serialized module data not embedded (roc__serialized_base_ptr is null)");
        return error.SharedMemoryError;
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
    shared_memory_initialized.set();
}

/// Cross-platform evaluation (works for both IPC and embedded modes)
fn evaluateFromSharedMemory(entry_idx: u32, roc_ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) ShimError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

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

    // Get expression info using entry_idx
    // Use the cached globals set during initialization (works for both formats)
    const base_ptr = roc__serialized_base_ptr.?;
    var buf: [256]u8 = undefined;

    if (entry_idx >= global_entry_count) {
        const err_msg = std.fmt.bufPrint(&buf, "Invalid entry_idx {} >= entry_count {}", .{ entry_idx, global_entry_count }) catch "Invalid entry_idx";
        roc_ops.crash(err_msg);
        return error.InvalidEntryIndex;
    }

    const def_offset = global_def_indices_offset + entry_idx * @sizeOf(u32);
    const def_idx_raw: u32 = if (global_is_serialized_format) blk: {
        // For serialized format, use unaligned reads since data may not be aligned
        const byte_offset: usize = @intCast(def_offset);
        if (byte_offset + 4 > roc__serialized_size) {
            const err_msg = std.fmt.bufPrint(&buf, "def_idx out of bounds: offset={}, size={}", .{ byte_offset, roc__serialized_size }) catch "def_idx out of bounds";
            roc_ops.crash(err_msg);
            return error.MemoryLayoutInvalid;
        }
        const ptr: *const [4]u8 = @ptrCast(base_ptr + byte_offset);
        const val = std.mem.readInt(u32, ptr, .little);
        break :blk val;
    } else blk: {
        // For legacy format, use safe aligned read
        break :blk safe_memory.safeRead(u32, base_ptr, @intCast(def_offset), roc__serialized_size) catch |err| {
            const read_err = std.fmt.bufPrint(&buf, "Failed to read def_idx: {}", .{err}) catch "Failed to read def_idx";
            roc_ops.crash(read_err);
            return error.MemoryLayoutInvalid;
        };
    };
    const def_idx: CIR.Def.Idx = @enumFromInt(def_idx_raw);

    // Get the definition and extract its expression
    const def = env_ptr.store.getDef(def_idx);
    const expr_idx = def.expr;

    // Evaluate the expression (with optional arguments)
    interpreter.evaluateExpression(expr_idx, ret_ptr, roc_ops, arg_ptr) catch |err| {
        if (err == error.TypeMismatch) {
            roc_ops.crash("TypeMismatch from evaluateExpression");
        }
        return err;
    };
}

/// Result of setting up module environments
const SetupResult = struct {
    primary_env: *ModuleEnv, // Platform main env or app env (for entry points)
    app_env: *ModuleEnv, // App env (for e_lookup_required resolution)
};

/// Set up ModuleEnv from serialized data with proper relocation (multi-module format)
/// Works for both IPC mode (roc run) and embedded mode (roc build)
/// Detects portable serialized format (cross-architecture) via magic number
fn setupModuleEnv(roc_ops: *RocOps) ShimError!SetupResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var buf: [256]u8 = undefined;
    const base_ptr = roc__serialized_base_ptr.?;
    const allocator = wrapped_allocator;

    // Check for portable serialized format by looking at first 4 bytes
    // The magic number is at the very start of the buffer (no FIRST_ALLOC_OFFSET for portable format)
    // Use unaligned read to avoid alignment issues in debug builds
    const magic = std.mem.readInt(u32, base_ptr[0..4], .little);
    if (magic == SERIALIZED_FORMAT_MAGIC) {
        // Portable serialized format - use deserialize()
        return setupModuleEnvFromSerialized(roc_ops, base_ptr, allocator);
    }

    // Legacy format: Read parent's shared memory base address from header and calculate relocation offset
    // For embedded mode: parent_base_addr == 0
    // For IPC mode: parent_base_addr == actual parent address
    const header_addr = @intFromPtr(base_ptr) + FIRST_ALLOC_OFFSET;
    const header_ptr: *const Header = @ptrFromInt(header_addr);
    const parent_base_addr = header_ptr.parent_base_addr;
    const module_count = header_ptr.module_count;

    // Store header info in globals for use during evaluation (legacy format)
    global_entry_count = header_ptr.entry_count;
    global_def_indices_offset = header_ptr.def_indices_offset;
    global_is_serialized_format = false;

    // Calculate relocation offset
    const child_base_addr = @intFromPtr(base_ptr);
    // Use signed arithmetic to avoid overflow on 64-bit addresses
    const offset: i64 = @as(i64, @intCast(child_base_addr)) - @as(i64, @intCast(parent_base_addr));

    // Verify offset preserves alignment (ASLR can cause misaligned shared memory mapping)
    // Skip debug checks on freestanding as std.debug.print uses stderr which isn't available
    if (comptime builtin.mode == .Debug and !is_wasm32) {
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
        module_env_ptr.gpa = allocator;
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
    app_env_ptr.gpa = allocator;

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
        platform_env_ptr.gpa = allocator;
        break :blk platform_env_ptr;
    } else app_env_ptr;

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
        // The offset parameter is the buffer base address - serialized offsets are relative to buffer start
        env_ptrs[i] = env_serialized.deserialize(
            @as(i64, @intCast(@intFromPtr(base_ptr))), // buffer base address as offset
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

/// Create and initialize interpreter with heap-allocated stable objects
fn createInterpreter(env_ptr: *ModuleEnv, app_env: ?*ModuleEnv, builtin_modules: *const eval.BuiltinModules, roc_ops: *RocOps) ShimError!Interpreter {
    const trace = tracy.trace(@src());
    defer trace.end();

    const allocator = wrapped_allocator;

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

    if (comptime trace_modules) {
        std.debug.print("[TRACE-MODULES] === Interpreter Shim Initialization ===\n", .{});
        std.debug.print("[TRACE-MODULES] Building imported_envs: [", .{});
        for (imported_envs, 0..) |imp_env, idx| {
            if (idx > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{imp_env.module_name});
        }
        std.debug.print("]\n", .{});
        std.debug.print("[TRACE-MODULES] Primary env: \"{s}\"\n", .{env_ptr.module_name});
        if (app_env) |a_env| {
            std.debug.print("[TRACE-MODULES] App env: \"{s}\"\n", .{a_env.module_name});
        }
    }

    // Resolve imports - map each import name to its index in imported_envs
    if (comptime trace_modules) {
        std.debug.print("[TRACE-MODULES] Resolving imports for primary env \"{s}\"\n", .{env_ptr.module_name});
    }
    env_ptr.imports.resolveImports(env_ptr, imported_envs);

    // Also resolve imports for the app env if it's different from the primary env
    // This is needed when the platform calls the app's main! via e_lookup_required
    if (app_env) |a_env| {
        if (a_env != env_ptr) {
            if (comptime trace_modules) {
                std.debug.print("[TRACE-MODULES] Resolving imports for app env \"{s}\"\n", .{a_env.module_name});
            }
            a_env.imports.resolveImports(a_env, imported_envs);
        }
    }

    // Also resolve imports for all imported module environments.
    // This is needed when module A imports module B, and the interpreter evaluates
    // code in A that calls into B (transitive module calls).
    // Without this, A's import of B remains unresolved, causing "unresolved import"
    // or TypeMismatch errors when A's code tries to call B.
    if (comptime trace_modules) {
        std.debug.print("[TRACE-MODULES] Re-resolving imports for all imported modules:\n", .{});
    }
    for (imported_envs) |imp_env| {
        if (comptime trace_modules) {
            std.debug.print("[TRACE-MODULES]   Re-resolving imports for \"{s}\"\n", .{imp_env.module_name});
        }
        @constCast(imp_env).imports.resolveImports(imp_env, imported_envs);
    }

    // Enable runtime inserts on all deserialized module environments.
    // The interpreter needs to insert new identifiers at runtime for:
    // - Translating module names between different ident spaces
    // - Building qualified type names for method lookup
    // - Runtime type introspection
    // Deserialized interners are read-only by default, so we explicitly enable inserts here.
    // This copies the data from the read-only embedded buffer into growable allocated memory.
    env_ptr.common.idents.interner.enableRuntimeInserts(allocator) catch {
        roc_ops.crash("INTERPRETER SHIM: Failed to enable runtime inserts on platform env");
        return error.InterpreterSetupFailed;
    };
    if (app_env) |a_env| {
        @constCast(a_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
            roc_ops.crash("INTERPRETER SHIM: Failed to enable runtime inserts on app env");
            return error.InterpreterSetupFailed;
        };
    }
    for (imported_envs) |imp_env| {
        @constCast(imp_env).common.idents.interner.enableRuntimeInserts(allocator) catch {
            roc_ops.crash("INTERPRETER SHIM: Failed to enable runtime inserts on imported env");
            return error.InterpreterSetupFailed;
        };
    }

    // Fix up module_name_idx for deserialized modules.
    // Deserialized modules have module_name_idx set to NONE because the interner
    // was read-only during deserialization. Now that enableRuntimeInserts has been
    // called, we can insert the module names to enable runtime method lookups.
    // This is critical for nominal type method dispatch (e.g., is_eq).
    if (env_ptr.module_name_idx.isNone() and env_ptr.module_name.len > 0) {
        env_ptr.module_name_idx = env_ptr.insertIdent(base.Ident.for_text(env_ptr.module_name)) catch {
            roc_ops.crash("INTERPRETER SHIM: Failed to insert module name for platform env");
            return error.InterpreterSetupFailed;
        };
    }
    if (app_env) |a_env| {
        if (a_env.module_name_idx.isNone() and a_env.module_name.len > 0) {
            @constCast(a_env).module_name_idx = @constCast(a_env).insertIdent(base.Ident.for_text(a_env.module_name)) catch {
                roc_ops.crash("INTERPRETER SHIM: Failed to insert module name for app env");
                return error.InterpreterSetupFailed;
            };
        }
    }
    for (imported_envs) |imp_env| {
        if (imp_env.module_name_idx.isNone() and imp_env.module_name.len > 0) {
            @constCast(imp_env).module_name_idx = @constCast(imp_env).insertIdent(base.Ident.for_text(imp_env.module_name)) catch {
                roc_ops.crash("INTERPRETER SHIM: Failed to insert module name for imported env");
                return error.InterpreterSetupFailed;
            };
        }
    }

    var interpreter = eval.Interpreter.init(allocator, env_ptr, builtin_types, builtin_module_env, imported_envs, getShimImportMapping(), app_env) catch {
        roc_ops.crash("INTERPRETER SHIM: Interpreter initialization failed");
        return error.InterpreterSetupFailed;
    };

    // Setup for-clause type mappings from platform to app.
    // This maps rigid variable names (like "model") to their concrete app types.
    interpreter.setupForClauseTypeMappings(env_ptr) catch {
        roc_ops.crash("INTERPRETER SHIM: Failed to setup for-clause type mappings");
        return error.InterpreterSetupFailed;
    };

    return interpreter;
}

//! Coordinator for the actor-based compilation model.
//!
//! The Coordinator is the single owner of all mutable state in the compilation pipeline.
//! This eliminates races by design:
//! - Single-threaded state mutations (no locks needed for state)
//! - Workers are pure: they receive tasks, return results
//! - Communication happens via bounded channels
//!
//! Architecture:
//! ```
//!                     ┌─────────────────────────────────────────────────────┐
//!                     │                    COORDINATOR                       │
//!                     │  - Owns all mutable state (modules, phases, deps)   │
//!                     │  - Single-threaded: no races by design              │
//!                     │  - Receives results, updates state, dispatches work │
//!                     └───────────────────────┬─────────────────────────────┘
//!                                             │
//!                          ┌──────────────────┼──────────────────┐
//!                          │                  │                  │
//!                     ┌────▼─────┐      ┌─────▼─────┐     ┌──────▼────────┐
//!                     │  Task    │      │  Result   │     │   Shared      │
//!                     │  Queue   │      │  Channel  │     │   Read-Only   │
//!                     │ (inject) │      │  (MPSC)   │     │   (Builtins)  │
//!                     └────┬─────┘      └─────▲─────┘     └───────────────┘
//!                          │                  │
//!         ┌────────────────┼──────────────────┼────────────────┐
//!         │                │                  │                │
//!    ┌────▼────┐      ┌────▼────┐        ┌────┴────┐      ┌────▼────┐
//!    │ Worker  │      │ Worker  │   ...  │ Worker  │      │ Worker  │
//!    │  Arena  │      │  Arena  │        │  Arena  │      │  Arena  │
//!    └─────────┘      └─────────┘        └─────────┘      └─────────┘
//! ```

const std = @import("std");
const builtin = @import("builtin");
const can = @import("can");
const parse = @import("parse");
const reporting = @import("reporting");
const eval = @import("eval");
const base = @import("base");
const build_options = @import("build_options");

const CIR = can.CIR;

const messages = @import("messages.zig");
const channel = @import("channel.zig");
const compile_package = @import("compile_package.zig");
const compile_build = @import("compile_build.zig");
const module_discovery = @import("module_discovery.zig");
const cache_manager_mod = @import("cache_manager.zig");
const CacheManager = cache_manager_mod.CacheManager;
const ImportInfo = cache_manager_mod.ImportInfo;
const CacheModule = @import("cache_module.zig").CacheModule;
const roc_target = @import("roc_target");

// Compile-time flag for build tracing - enabled via `zig build -Dtrace-build`
const trace_build = if (@hasDecl(build_options, "trace_build")) build_options.trace_build else false;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Report = reporting.Report;
const AST = parse.AST;
const BuiltinModules = eval.BuiltinModules;

const WorkerTask = messages.WorkerTask;
const WorkerResult = messages.WorkerResult;
const ModuleId = messages.ModuleId;
const ParseTask = messages.ParseTask;
const CanonicalizeTask = messages.CanonicalizeTask;
const TypeCheckTask = messages.TypeCheckTask;
const ParsedResult = messages.ParsedResult;
const CanonicalizedResult = messages.CanonicalizedResult;
const TypeCheckedResult = messages.TypeCheckedResult;
const DiscoveredLocalImport = messages.DiscoveredLocalImport;
const DiscoveredExternalImport = messages.DiscoveredExternalImport;
const CacheHitResult = messages.CacheHitResult;

const Channel = channel.Channel;
const FileProvider = compile_package.FileProvider;
const Mode = compile_package.Mode;

/// Threading features aren't available when targeting WebAssembly
const threads_available = builtin.target.cpu.arch != .wasm32;
const Thread = if (threads_available) std.Thread else struct {};
/// Allocators for a worker thread. Each worker has its own instance.
/// This ensures thread-safe allocations without contention.
///
/// Design rationale:
/// - `gpa`: For long-lived data (ModuleEnv, source). In MT mode, this is
///   page_allocator for thread safety. Data allocated here is stored in
///   ModuleEnv.gpa and freed during module cleanup.
/// - `arena`: For temporary allocations within a task (reports, diagnostics).
///   Reset between tasks to reduce allocation pressure.
pub const WorkerAllocators = struct {
    /// General purpose allocator for long-lived data (ModuleEnv, source).
    /// In multi-threaded mode, this is page_allocator for thread safety.
    gpa: Allocator,

    /// Arena for temporary allocations within a task.
    /// Reset between tasks to reduce allocation pressure.
    arena: Allocator,

    /// Underlying arena implementation
    arena_impl: std.heap.ArenaAllocator,

    pub fn init(backing: Allocator) WorkerAllocators {
        var arena_impl = std.heap.ArenaAllocator.init(backing);
        return .{
            .gpa = backing,
            .arena_impl = arena_impl,
            .arena = arena_impl.allocator(),
        };
    }

    pub fn deinit(self: *WorkerAllocators) void {
        self.arena_impl.deinit();
    }

    /// Reset arena between tasks (keeps capacity, frees memory)
    pub fn resetArena(self: *WorkerAllocators) void {
        _ = self.arena_impl.reset(.retain_capacity);
    }
};

/// Module compilation phase
pub const Phase = enum {
    /// Initial phase: needs parsing
    Parse,
    /// Queued for parsing (prevents double-enqueue)
    Parsing,
    /// Parsed, needs canonicalization
    Canonicalize,
    /// Canonicalized, waiting for imports to complete
    WaitingOnImports,
    /// Imports ready, needs type checking
    TypeCheck,
    /// Fully compiled
    Done,
};

/// State of a single module within a package
pub const ModuleState = struct {
    /// Module name (e.g., "Main", "Foo")
    name: []const u8,
    /// Filesystem path to the .roc file
    path: []const u8,
    /// Module environment (owned, null until parsed)
    env: ?*ModuleEnv,
    /// Cached AST from parsing (owned, null after canonicalization)
    cached_ast: ?*AST,
    /// Current compilation phase
    phase: Phase,
    /// Local imports (module IDs within same package)
    imports: std.ArrayList(ModuleId),
    /// External imports (qualified names like "pf.Stdout")
    external_imports: std.ArrayList([]const u8),
    /// Modules that depend on this one (for waking dependents)
    dependents: std.ArrayList(ModuleId),
    /// Diagnostic reports
    reports: std.ArrayList(Report),
    /// Minimum dependency depth from root
    depth: u32,
    /// DFS visit color for cycle detection
    visit_color: VisitColor,
    /// True if this module was loaded from cache in this build.
    /// Used by parent modules to determine if fast path is valid.
    was_cache_hit: bool,
    /// Accumulated compile time for this module (parse + canonicalize + type-check)
    compile_time_ns: u64,

    /// DFS colors for cycle detection during import graph traversal
    pub const VisitColor = enum {
        /// Not yet visited
        white,
        /// Currently being visited (in the DFS stack)
        gray,
        /// Fully processed
        black,
    };

    pub fn init(name: []const u8, path: []const u8) ModuleState {
        return .{
            .name = name,
            .path = path,
            .env = null,
            .cached_ast = null,
            .phase = .Parse,
            .imports = std.ArrayList(ModuleId).empty,
            .external_imports = std.ArrayList([]const u8).empty,
            .dependents = std.ArrayList(ModuleId).empty,
            .reports = std.ArrayList(Report).empty,
            .depth = std.math.maxInt(u32),
            .visit_color = .white,
            .was_cache_hit = false,
            .compile_time_ns = 0,
        };
    }

    pub fn deinit(self: *ModuleState, gpa: Allocator, owns_module_data: bool) void {
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT] {s}: starting, env={}, ast={}, owns={}\n", .{
                self.name,
                if (self.env != null) @as(u8, 1) else @as(u8, 0),
                if (self.cached_ast != null) @as(u8, 1) else @as(u8, 0),
                if (owns_module_data) @as(u8, 1) else @as(u8, 0),
            });
        }
        // Free cached AST if present (always in gpa, not module_allocator)
        if (self.cached_ast) |ast| {
            if (comptime trace_build) {
                std.debug.print("[MOD DEINIT] {s}: freeing ast\n", .{self.name});
            }
            ast.deinit();
        }

        // Free module env if present (only if we own module data)
        if (owns_module_data) {
            if (self.env) |env| {
                // IMPORTANT: env stores its own allocator (env.gpa) which was used to create it.
                // We must use env.gpa for cleanup, not the passed-in gpa, because in multi-threaded
                // mode, env.gpa is page_allocator while gpa is the coordinator's allocator.
                const env_alloc = env.gpa;
                const source = env.common.source;
                if (self.was_cache_hit) {
                    // For cached modules, the env is heap-allocated but some fields
                    // point into the cache buffer. Use deinitCachedModule() which only
                    // frees heap-allocated parts (types, store.regions, imports map).
                    if (comptime trace_build) {
                        std.debug.print("[MOD DEINIT] {s}: deinit cached module env\n", .{self.name});
                    }
                    env.deinitCachedModule();
                    env_alloc.destroy(env);
                } else {
                    if (comptime trace_build) {
                        std.debug.print("[MOD DEINIT] {s}: freeing env\n", .{self.name});
                    }
                    env.deinit();
                    env_alloc.destroy(env);
                }
                // Free the heap-allocated source (it's NOT part of the cache buffer)
                // Source was allocated with the same allocator used for env creation
                if (source.len > 0) env_alloc.free(source);
            }
        }

        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT] {s}: freeing imports\n", .{self.name});
        }
        self.imports.deinit(gpa);
        for (self.external_imports.items) |imp| {
            gpa.free(imp);
        }
        self.external_imports.deinit(gpa);
        self.dependents.deinit(gpa);
        for (self.reports.items) |*rep| {
            rep.deinit();
        }
        self.reports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT] {s}: freeing path and name\n", .{self.name});
        }
        gpa.free(self.path);
        gpa.free(self.name);
    }
};

/// State of a package in the workspace
pub const PackageState = struct {
    /// Package name (alias in workspace)
    name: []const u8,
    /// Root directory for the package
    root_dir: []const u8,
    /// All modules in this package
    modules: std.ArrayList(ModuleState),
    /// Module name -> module ID lookup
    module_names: std.StringHashMap(ModuleId),
    /// Number of modules not yet in Done phase
    remaining_modules: usize,
    /// Root module ID (the module that starts the build)
    root_module_id: ?ModuleId,
    /// Package shorthands (alias -> target package name)
    shorthands: std.StringHashMap([]const u8),

    pub fn init(gpa: Allocator, name: []const u8, root_dir: []const u8) PackageState {
        _ = gpa; // Used for consistency but not needed for empty init
        return .{
            .name = name,
            .root_dir = root_dir,
            .modules = std.ArrayList(ModuleState).empty,
            .module_names = std.StringHashMap(ModuleId).init(std.heap.page_allocator),
            .remaining_modules = 0,
            .root_module_id = null,
            .shorthands = std.StringHashMap([]const u8).init(std.heap.page_allocator),
        };
    }

    pub fn deinit(self: *PackageState, gpa: Allocator, owns_module_data: bool) void {
        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: deiniting {} modules\n", .{ self.name, self.modules.items.len });
        }
        for (self.modules.items, 0..) |*mod, i| {
            if (comptime trace_build) {
                std.debug.print("[PKG DEINIT] {s}: deinit module {} ({s})\n", .{ self.name, i, mod.name });
            }
            mod.deinit(gpa, owns_module_data);
        }
        self.modules.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: modules done, deiniting names\n", .{self.name});
        }
        self.module_names.deinit();

        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: names done, deiniting shorthands\n", .{self.name});
        }
        var sh_it = self.shorthands.iterator();
        while (sh_it.next()) |entry| {
            gpa.free(entry.key_ptr.*);
            gpa.free(entry.value_ptr.*);
        }
        self.shorthands.deinit();

        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: freeing name and root_dir\n", .{self.name});
        }
        gpa.free(self.name);
        gpa.free(self.root_dir);
        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] done\n", .{});
        }
    }

    /// Ensure a module exists, creating it if necessary
    pub fn ensureModule(self: *PackageState, gpa: Allocator, name: []const u8, path: []const u8) !ModuleId {
        if (self.module_names.get(name)) |id| {
            return id;
        }

        const id: ModuleId = @intCast(self.modules.items.len);
        const owned_name = try gpa.dupe(u8, name);
        const owned_path = try gpa.dupe(u8, path);

        try self.modules.append(gpa, ModuleState.init(owned_name, owned_path));
        try self.module_names.put(owned_name, id);

        return id;
    }

    /// Get module state by ID
    pub fn getModule(self: *PackageState, id: ModuleId) ?*ModuleState {
        if (id >= self.modules.items.len) return null;
        return &self.modules.items[id];
    }

    /// Get module ID by name
    pub fn getModuleId(self: *PackageState, name: []const u8) ?ModuleId {
        return self.module_names.get(name);
    }

    /// Get module env if done
    pub fn getEnvIfDone(self: *PackageState, name: []const u8) ?*ModuleEnv {
        const id = self.module_names.get(name) orelse return null;
        const mod = &self.modules.items[id];
        if (mod.phase != .Done) return null;
        return mod.env;
    }

    /// Check if a module is done (regardless of whether it has an env).
    /// This is used to check if dependents can proceed - a module that failed
    /// to parse is still "done" and shouldn't block dependents.
    pub fn isDone(self: *PackageState, name: []const u8) bool {
        const id = self.module_names.get(name) orelse return false;
        const mod = &self.modules.items[id];
        return mod.phase == .Done;
    }
};

/// A reference to a module in a specific package
pub const ModuleRef = struct {
    pkg_name: []const u8,
    module_id: ModuleId,
};

/// Coordinator manages all compilation state and coordinates workers
pub const Coordinator = struct {
    gpa: Allocator,
    mode: Mode,
    max_threads: usize,
    target: roc_target.RocTarget,

    /// All packages in the workspace
    packages: std.StringHashMap(*PackageState),

    /// Result channel from workers to coordinator
    result_channel: Channel(WorkerResult),

    /// Task channel for workers (ring buffer with proper synchronization)
    task_channel: Channel(WorkerTask),

    /// Worker threads
    workers: std.ArrayList(Thread),

    /// Number of tasks currently being processed by workers (atomic for thread safety)
    inflight: std.atomic.Value(usize),

    /// Total modules remaining across all packages
    total_remaining: usize,

    /// Shared read-only builtin modules
    builtin_modules: *const BuiltinModules,

    /// File provider for reading sources (defaults to filesystem)
    file_provider: FileProvider,

    /// Compiler version for cache keys
    compiler_version: []const u8,

    /// Optional cache manager for disk caching (not yet integrated)
    cache_manager: ?*CacheManager,

    /// Cross-package dependents: when module (pkg, id) completes, notify these modules
    /// Key is "pkg_name:module_id", value is list of dependent ModuleRefs
    cross_package_dependents: std.StringHashMap(std.ArrayList(ModuleRef)),

    /// Optional allocator for module data (ModuleEnv, source).
    /// When set, used instead of gpa for module data only.
    /// This supports IPC mode where module data must be in shared memory.
    module_allocator: ?std.mem.Allocator,

    /// Whether this coordinator owns module data (should free on deinit).
    /// Set to false for IPC mode where shared memory will be unmapped.
    owns_module_data: bool,

    /// Whether to run hosted compiler transformation after canonicalization.
    /// Set to true for IPC mode where platform modules need hosted lambdas.
    enable_hosted_transform: bool,

    /// Timing accumulators
    total_parse_ns: u64,
    total_canonicalize_ns: u64,
    total_canonicalize_diag_ns: u64,
    total_typecheck_ns: u64,
    total_typecheck_diag_ns: u64,

    /// Build statistics
    cache_hits: u32,
    cache_misses: u32,
    modules_compiled: u32,
    /// Module compile time tracking (min/max/sum for computing avg)
    module_time_min_ns: u64,
    module_time_max_ns: u64,
    module_time_sum_ns: u64,

    /// Cache buffers that need to be kept alive for cached modules.
    /// These are freed when the coordinator is deinitialized.
    cache_buffers: std.ArrayList(CacheModule.CacheData),

    /// Get allocator for worker thread operations.
    /// In multi-threaded mode, returns page_allocator which is guaranteed thread-safe.
    /// In single-threaded mode, returns gpa for better performance.
    pub fn getWorkerAllocator(self: *const Coordinator) Allocator {
        return if (threads_available and self.mode == .multi_threaded)
            std.heap.page_allocator
        else
            self.gpa;
    }

    pub fn init(
        gpa: Allocator,
        mode: Mode,
        max_threads: usize,
        target: roc_target.RocTarget,
        builtin_modules: *const BuiltinModules,
        compiler_version: []const u8,
        cache_manager: ?*CacheManager,
    ) !Coordinator {
        // Task channel uses a bounded ring buffer with proper mutex + condition variable
        // synchronization, matching the proven result_channel implementation.
        const initial_task_capacity = 256;
        return .{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .packages = std.StringHashMap(*PackageState).init(gpa),
            .result_channel = try Channel(WorkerResult).init(gpa, channel.DEFAULT_CAPACITY),
            .task_channel = try Channel(WorkerTask).init(if (threads_available) std.heap.page_allocator else gpa, initial_task_capacity),
            .workers = std.ArrayList(Thread).empty,
            .inflight = std.atomic.Value(usize).init(0),
            .total_remaining = 0,
            .builtin_modules = builtin_modules,
            .file_provider = FileProvider.filesystem,
            .compiler_version = compiler_version,
            .cache_manager = cache_manager,
            .cross_package_dependents = std.StringHashMap(std.ArrayList(ModuleRef)).init(gpa),
            .module_allocator = null,
            .owns_module_data = true,
            .enable_hosted_transform = false,
            .total_parse_ns = 0,
            .total_canonicalize_ns = 0,
            .total_canonicalize_diag_ns = 0,
            .total_typecheck_ns = 0,
            .total_typecheck_diag_ns = 0,
            .cache_hits = 0,
            .cache_misses = 0,
            .modules_compiled = 0,
            .module_time_min_ns = std.math.maxInt(u64),
            .module_time_max_ns = 0,
            .module_time_sum_ns = 0,
            .cache_buffers = std.ArrayList(CacheModule.CacheData).empty,
        };
    }

    pub fn deinit(self: *Coordinator) void {
        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] starting shutdown...\n", .{});
        }
        // Stop workers
        self.shutdown();

        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] shutdown done, freeing packages...\n", .{});
        }

        // Free packages
        // Note: ModuleEnv stores its own allocator (env.gpa), so deinit will use the correct
        // allocator for each env. In multi-threaded mode, this is page_allocator.
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (comptime trace_build) {
                std.debug.print("[COORD DEINIT] deinit package {s}\n", .{entry.key_ptr.*});
            }
            entry.value_ptr.*.deinit(self.gpa, self.owns_module_data);
            if (comptime trace_build) {
                std.debug.print("[COORD DEINIT] package deinit done, now destroying\n", .{});
            }
            self.gpa.destroy(entry.value_ptr.*);
            if (comptime trace_build) {
                std.debug.print("[COORD DEINIT] package destroyed\n", .{});
            }
        }
        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] all packages done, calling packages.deinit()\n", .{});
        }
        self.packages.deinit();

        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] packages done\n", .{});
        }

        // Free task channel
        self.task_channel.deinit();

        // Free cross-package dependents
        var cpd_it = self.cross_package_dependents.iterator();
        while (cpd_it.next()) |entry| {
            self.gpa.free(entry.key_ptr.*); // Free the "pkg:id" key string
            entry.value_ptr.deinit(self.gpa);
        }
        self.cross_package_dependents.deinit();

        self.result_channel.deinit();
        self.workers.deinit(self.gpa);

        // Free cache buffers that were keeping cached module data alive
        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] freeing {} cache buffers\n", .{self.cache_buffers.items.len});
        }
        for (self.cache_buffers.items) |*buf| {
            buf.deinit(self.gpa);
        }
        self.cache_buffers.deinit(self.gpa);
    }

    /// Set a file provider (or reset to default filesystem provider)
    pub fn setFileProvider(self: *Coordinator, provider: ?FileProvider) void {
        self.file_provider = provider orelse FileProvider.filesystem;
    }

    /// Set a custom allocator for module data (ModuleEnv, source).
    /// Used for IPC mode where module data must be in shared memory.
    pub fn setModuleAllocator(self: *Coordinator, allocator: std.mem.Allocator) void {
        self.module_allocator = allocator;
    }

    /// Get the allocator to use for module data.
    /// Returns module_allocator if set (IPC mode), otherwise:
    /// - In multi-threaded mode: page_allocator (guaranteed thread-safe)
    /// - In single-threaded mode: gpa (better performance)
    pub fn getModuleAllocator(self: *Coordinator) std.mem.Allocator {
        if (self.module_allocator) |alloc| return alloc;
        // Use page_allocator in multi-threaded mode for thread safety.
        // Module data is created by workers and used throughout canonicalization/type-checking.
        return if (threads_available and self.mode == .multi_threaded)
            std.heap.page_allocator
        else
            self.gpa;
    }

    /// Create or get a package
    pub fn ensurePackage(self: *Coordinator, name: []const u8, root_dir: []const u8) !*PackageState {
        if (self.packages.get(name)) |pkg| {
            return pkg;
        }

        const pkg = try self.gpa.create(PackageState);
        pkg.* = PackageState.init(self.gpa, try self.gpa.dupe(u8, name), try self.gpa.dupe(u8, root_dir));
        try self.packages.put(pkg.name, pkg);
        return pkg;
    }

    /// Get a package by name
    pub fn getPackage(self: *Coordinator, name: []const u8) ?*PackageState {
        return self.packages.get(name);
    }

    /// Start the coordinator and spawn worker threads (for multi-threaded mode)
    pub fn start(self: *Coordinator) !void {
        if (!threads_available or self.mode == .single_threaded or self.max_threads <= 1) {
            return;
        }

        const n = if (self.max_threads == 0) (std.Thread.getCpuCount() catch 1) else self.max_threads;

        try self.workers.ensureTotalCapacity(self.gpa, n);
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const th = try std.Thread.spawn(.{}, workerThread, .{self});
            try self.workers.append(self.gpa, th);
        }
    }

    /// Shutdown workers and wait for them to complete
    pub fn shutdown(self: *Coordinator) void {
        if (!threads_available) return;

        // Close both channels to wake any blocked workers
        self.result_channel.close();
        self.task_channel.close();

        // Wait for workers to finish
        for (self.workers.items) |w| {
            w.join();
        }
        self.workers.clearRetainingCapacity();
    }

    /// Enqueue a task for processing
    pub fn enqueueTask(self: *Coordinator, task: WorkerTask) !void {
        if (comptime trace_build) {
            switch (task) {
                .parse => |t| std.debug.print("[COORD] ENQUEUE parse: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .canonicalize => |t| std.debug.print("[COORD] ENQUEUE canonicalize: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .type_check => |t| std.debug.print("[COORD] ENQUEUE type_check: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .shutdown => std.debug.print("[COORD] ENQUEUE shutdown\n", .{}),
            }
        }
        // Increment inflight BEFORE sending to the channel. This ensures there is
        // no window where a worker could recv, execute, and the coordinator could
        // process the result (decrementing inflight) before we increment here.
        if (threads_available and self.mode == .multi_threaded) {
            _ = self.inflight.fetchAdd(1, .acquire);
        }
        self.task_channel.send(task) catch |err| switch (err) {
            error.Closed => {
                // Undo the increment — shutting down, safe to drop
                if (threads_available and self.mode == .multi_threaded) {
                    _ = self.inflight.fetchSub(1, .release);
                }
                return;
            },
            error.Timeout => unreachable, // send() waits indefinitely
            error.OutOfMemory => {
                if (threads_available and self.mode == .multi_threaded) {
                    _ = self.inflight.fetchSub(1, .release);
                }
                return error.OutOfMemory;
            },
        };
    }

    /// Enqueue a parse task for a module
    pub fn enqueueParseTask(self: *Coordinator, pkg_name: []const u8, module_id: ModuleId) !void {
        const pkg = self.packages.get(pkg_name) orelse return;
        const mod = pkg.getModule(module_id) orelse return;

        // Try cache first (works for both single and multi-threaded modes)
        // This runs in the coordinator, so it's safe to access self.packages
        if (self.tryCacheHit(pkg, mod, module_id)) {
            // Cache hit - module is now Done, no need to parse
            // Note: tryCacheHit already handles remaining_modules/total_remaining
            // for child imports, but we need to decrement for this module
            if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
            if (self.total_remaining > 0) self.total_remaining -= 1;
            return;
        }

        // Cache miss - dispatch parse task
        mod.phase = .Parsing;

        try self.enqueueTask(.{
            .parse = .{
                .package_name = pkg.name, // Use pkg's owned name, not the passed-in reference
                .module_id = module_id,
                .module_name = mod.name,
                .path = mod.path,
                .depth = mod.depth,
            },
        });
    }

    /// Main coordinator loop - unified for single and multi-threaded modes
    pub fn coordinatorLoop(self: *Coordinator) !void {
        var iterations_without_progress: usize = 0;
        while (!self.isComplete()) {
            var made_progress = false;

            if (!threads_available or self.mode == .single_threaded or self.max_threads <= 1) {
                // Single-threaded: process tasks inline
                if (self.task_channel.tryRecv()) |task| {
                    const result = self.executeTaskInline(task);
                    try self.handleResult(result);
                    made_progress = true;
                }
            } else {
                // Multi-threaded: receive from workers via channel
                // Use blocking recv with timeout to avoid busy spinning
                if (self.result_channel.recvTimeout(10_000_000)) |result| { // 10ms timeout
                    _ = self.inflight.fetchSub(1, .release);
                    try self.handleResult(result);
                    made_progress = true;
                }
            }

            // If no progress was made, try to unblock modules waiting on external imports
            if (!made_progress) {
                if (try self.tryUnblockAllWaiting()) {
                    made_progress = true;
                }
            }

            if (made_progress) {
                iterations_without_progress = 0;
            } else {
                iterations_without_progress += 1;
                if (iterations_without_progress > 1000) {
                    const task_count = self.task_channel.len();
                    std.debug.print("Coordinator stuck: remaining={}, tasks={}, inflight={}\n", .{
                        self.total_remaining,
                        task_count,
                        self.inflight.load(.acquire),
                    });
                    // Print package/module states with detailed diagnostics
                    var pkg_it = self.packages.iterator();
                    while (pkg_it.next()) |entry| {
                        const pkg = entry.value_ptr.*;
                        std.debug.print("  Package {s}: remaining={}, modules={}\n", .{
                            pkg.name,
                            pkg.remaining_modules,
                            pkg.modules.items.len,
                        });
                        for (pkg.modules.items, 0..) |mod, i| {
                            std.debug.print("    Module {}: {s} phase=.{s} ext_imports={}\n", .{
                                i,
                                mod.name,
                                @tagName(mod.phase),
                                mod.external_imports.items.len,
                            });
                            // For non-Done modules, print additional diagnostics
                            if (mod.phase != .Done) {
                                // Print local imports and their status
                                if (mod.imports.items.len > 0) {
                                    std.debug.print("      local_imports ({}):", .{mod.imports.items.len});
                                    for (mod.imports.items) |imp_id| {
                                        if (pkg.getModule(imp_id)) |imp_mod| {
                                            std.debug.print(" {s}(.{s})", .{ imp_mod.name, @tagName(imp_mod.phase) });
                                        } else {
                                            std.debug.print(" <invalid id={}>", .{imp_id});
                                        }
                                    }
                                    std.debug.print("\n", .{});
                                }
                                // Print external imports and their readiness
                                if (mod.external_imports.items.len > 0) {
                                    std.debug.print("      ext_imports ({}):", .{mod.external_imports.items.len});
                                    for (mod.external_imports.items) |ext_name| {
                                        const ready = self.isExternalReady(pkg.name, ext_name);
                                        std.debug.print(" {s}(ready={})", .{ ext_name, ready });
                                    }
                                    std.debug.print("\n", .{});
                                }
                            }
                        }
                    }
                    @panic("Coordinator stuck in infinite loop");
                }
            }
        }
    }

    /// Try to unblock all modules waiting on external imports
    /// Returns true if any module was unblocked
    fn tryUnblockAllWaiting(self: *Coordinator) !bool {
        var any_unblocked = false;
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items, 0..) |*mod, idx| {
                if (mod.phase == .WaitingOnImports) {
                    const old_phase = mod.phase;
                    try self.tryUnblock(pkg, @intCast(idx));
                    if (mod.phase != old_phase) {
                        any_unblocked = true;
                    }
                }
            }
        }
        return any_unblocked;
    }

    /// Check if all work is complete.
    ///
    /// Thread-safety: This is only called from the coordinator thread.
    /// `total_remaining` is only mutated by the coordinator, so it's always fresh.
    /// `inflight` is incremented *before* a task enters the channel and decremented
    /// only when the coordinator processes the result, so there is no window where
    /// the channel is empty and inflight is 0 while work is still pending.
    pub fn isComplete(self: *Coordinator) bool {
        return self.total_remaining == 0 and self.task_channel.isEmpty() and self.inflight.load(.acquire) == 0;
    }

    /// Execute a task inline (for single-threaded mode)
    fn executeTaskInline(self: *Coordinator, task: WorkerTask) WorkerResult {
        return switch (task) {
            .parse => |t| self.executeParse(t),
            .canonicalize => |t| self.executeCanonicalize(t),
            .type_check => |t| self.executeTypeCheck(t),
            .shutdown => unreachable,
        };
    }

    /// Handle a result from a worker
    fn handleResult(self: *Coordinator, result: WorkerResult) !void {
        // Make a mutable copy so we can deinit after handling
        var res = result;
        // Use worker allocator to match what workers used for allocation
        defer res.deinit(self.getWorkerAllocator());

        // Capture by pointer so handlers can clear reports to transfer ownership
        switch (res) {
            .parsed => |*r| try self.handleParsed(r),
            .canonicalized => |*r| try self.handleCanonicalized(r),
            .type_checked => |*r| try self.handleTypeChecked(r),
            .parse_failed => |*r| try self.handleParseFailed(r),
            .cycle_detected => |*r| try self.handleCycleDetected(r),
            .cache_hit => |*r| try self.handleCacheHit(r),
        }
    }

    /// Handle a successful parse result
    fn handleParsed(self: *Coordinator, result: *ParsedResult) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] PARSED: pkg={s} module={s} result_reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: package '{s}' not found for parsed result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' for parsed result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] PARSED: mod.reports BEFORE: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Take ownership of module env and AST
        mod.env = result.module_env;
        mod.cached_ast = result.cached_ast;

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_build) {
            std.debug.print("[COORD] PARSED: mod.reports AFTER: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Update timing
        self.total_parse_ns += result.parse_ns;
        mod.compile_time_ns += result.parse_ns;

        // Transition to Canonicalize phase
        mod.phase = .Canonicalize;

        // Enqueue canonicalization task
        try self.enqueueTask(.{
            .canonicalize = .{
                .package_name = result.package_name,
                .module_id = result.module_id,
                .module_name = mod.name,
                .path = mod.path,
                .depth = mod.depth,
                .module_env = mod.env.?,
                .cached_ast = mod.cached_ast.?,
                .root_dir = pkg.root_dir,
            },
        });
    }

    /// Handle a successful canonicalization result
    fn handleCanonicalized(self: *Coordinator, result: *CanonicalizedResult) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] CANONICALIZED: pkg={s} module={s} local_imports={} ext_imports={} result_reports={}\n", .{
                result.package_name,
                result.module_name,
                result.discovered_local_imports.items.len,
                result.discovered_external_imports.items.len,
                result.reports.items.len,
            });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: package '{s}' not found for canonicalized result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' for canonicalized result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] CANONICALIZED: mod.reports BEFORE: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Take ownership of module env
        mod.env = result.module_env;
        mod.cached_ast = null; // AST was consumed during canonicalization

        // Run hosted compiler transformation if enabled (for IPC mode)
        // This converts e_anno_only expressions to e_hosted_lambda in platform modules
        // Must be done AFTER canonicalization but BEFORE type checking
        if (self.enable_hosted_transform) {
            if (mod.env) |env| {
                // Only run for platform modules (packages other than "app")
                // The app package doesn't need hosted lambdas
                if (!std.mem.eql(u8, result.package_name, "app")) {
                    // Perform hosted transform and free the returned list of modified defs
                    // (we don't need the list, just the side effect of the transform)
                    // Note: the ArrayList uses env.gpa, not self.gpa
                    if (can.HostedCompiler.replaceAnnoOnlyWithHosted(env)) |modified_defs| {
                        var defs = modified_defs;
                        defs.deinit(env.gpa);
                    } else |_| {}
                }
            }
        }

        // Append reports - we take ownership, so clear result.reports after copying
        // to prevent WorkerResult.deinit from freeing the shared memory
        for (result.reports.items, 0..) |rep, ri| {
            if (comptime trace_build) {
                std.debug.print("[COORD] CANONICALIZED: result report {}: owned_strings.len={}\n", .{ ri, rep.owned_strings.items.len });
                if (rep.owned_strings.items.len > 0) {
                    std.debug.print("[COORD] CANONICALIZED: first owned_string ptr={} len={}\n", .{ @intFromPtr(rep.owned_strings.items[0].ptr), rep.owned_strings.items[0].len });
                }
            }
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_build) {
            std.debug.print("[COORD] CANONICALIZED: mod ptr={} mod.reports AFTER: len={} cap={}\n", .{ @intFromPtr(mod), mod.reports.items.len, mod.reports.capacity });
            if (mod.reports.items.len > 0) {
                const mr = &mod.reports.items[0];
                std.debug.print("[COORD] CANONICALIZED: mod.reports.items.ptr={} owned_strings.len={}\n", .{ @intFromPtr(mod.reports.items.ptr), mr.owned_strings.items.len });
                if (mr.owned_strings.items.len > 0) {
                    std.debug.print("[COORD] CANONICALIZED: mod.reports[0] first owned_string ptr={} len={}\n", .{ @intFromPtr(mr.owned_strings.items[0].ptr), mr.owned_strings.items[0].len });
                }
            }
        }

        // Update timing
        self.total_canonicalize_ns += result.canonicalize_ns;
        self.total_canonicalize_diag_ns += result.canonicalize_diagnostics_ns;
        mod.compile_time_ns += result.canonicalize_ns + result.canonicalize_diagnostics_ns;

        // Mark as gray (visiting) for cycle detection
        mod.visit_color = .gray;

        // Process discovered local imports
        // NOTE: We must refresh the mod pointer after each ensureModule call because
        // ensureModule can resize the modules array, invalidating pointers.
        for (result.discovered_local_imports.items) |imp| {
            const child_id = try pkg.ensureModule(self.gpa, imp.module_name, imp.path);
            // Refresh mod pointer after potential resize
            const current_mod = pkg.getModule(result.module_id) orelse {
                if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' after ensureModule in canonicalized handler (module={s})\n", .{
                    result.module_id, result.package_name, result.module_name,
                });
                unreachable;
            };
            try current_mod.imports.append(self.gpa, child_id);

            const child = pkg.getModule(child_id).?;
            try child.dependents.append(self.gpa, result.module_id);

            // Set depth - use saturating addition to prevent overflow when depth is
            // maxInt (uninitialized, e.g. for modules created via external imports)
            const new_depth = current_mod.depth +| 1;
            if (new_depth < child.depth) {
                child.depth = new_depth;
            }

            // Cycle detection
            if (child.visit_color == .gray or child_id == result.module_id) {
                // Cycle detected - handle it
                try self.handleCycleInline(pkg, result.module_id, child_id);
                return;
            }

            // Queue parse for new modules
            if (child.phase == .Parse) {
                pkg.remaining_modules += 1;
                self.total_remaining += 1;
                try self.enqueueParseTask(result.package_name, child_id);
            }
        }

        // Refresh mod pointer after potential resizes from local imports
        const mod_after_imports = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' after local imports in canonicalized handler (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        // Process discovered external imports
        for (result.discovered_external_imports.items) |ext_imp| {
            // Parse the qualified import name (e.g., "pf.Stdout" -> { .qualifier = "pf", .module = "Stdout" })
            const qualified = base.module_path.parseQualifiedImport(ext_imp.import_name) orelse continue;

            // Only add to external_imports if the shorthand resolves to a valid package.
            // If the shorthand doesn't exist, the import is invalid and should not block
            // this module - the error will be caught during type-checking when the
            // pending lookup for this import cannot be resolved.
            const target_pkg_name = pkg.shorthands.get(qualified.qualifier) orelse continue;
            const target_pkg = self.packages.get(target_pkg_name) orelse continue;

            // Valid shorthand - add to external imports and schedule
            try mod_after_imports.external_imports.append(self.gpa, try self.gpa.dupe(u8, ext_imp.import_name));
            try self.scheduleExternalImport(result.package_name, ext_imp.import_name);

            // Register this module as a cross-package dependent of the target
            const target_module_id = target_pkg.module_names.get(qualified.module) orelse continue;

            try self.registerCrossPackageDependent(
                target_pkg_name,
                target_module_id,
                result.package_name,
                result.module_id,
            );
        }

        // Mark as black (done visiting) to avoid false cycle detection
        // This is necessary because multiple modules can be processed in sequence,
        // and we don't want a module that finished canonicalization to look like
        // it's "currently being visited" when another module imports it.
        mod_after_imports.visit_color = .black;

        // Transition to WaitingOnImports
        mod_after_imports.phase = .WaitingOnImports;

        // Try to unblock immediately
        try self.tryUnblock(pkg, result.module_id);
    }

    /// Handle a successful type-check result
    fn handleTypeChecked(self: *Coordinator, result: *TypeCheckedResult) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] TYPE_CHECKED: pkg={s} module={s} result_reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: package '{s}' not found for type_checked result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' for type_checked result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] TYPE_CHECKED: mod.reports BEFORE append: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Take ownership of module env
        mod.env = result.module_env;

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_build) {
            std.debug.print("[COORD] TYPE_CHECKED: mod.reports AFTER append: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Update timing
        self.total_typecheck_ns += result.type_check_ns;
        self.total_typecheck_diag_ns += result.check_diagnostics_ns;
        mod.compile_time_ns += result.type_check_ns + result.check_diagnostics_ns;

        // Record per-module compile time for min/max/avg stats
        const module_time = mod.compile_time_ns;
        if (module_time < self.module_time_min_ns) self.module_time_min_ns = module_time;
        if (module_time > self.module_time_max_ns) self.module_time_max_ns = module_time;
        self.module_time_sum_ns += module_time;

        // Mark as done
        mod.phase = .Done;
        mod.visit_color = .black;
        mod.was_cache_hit = false; // This was NOT a cache hit (we just compiled it)

        // Update cache stats
        self.cache_misses += 1;
        self.modules_compiled += 1;

        // Store to cache
        if (self.cache_manager) |cache| {
            if (mod.env) |env| {
                // Compute source hash for metadata
                const source_hash = CacheManager.computeSourceHash(env.common.source);

                // Compute full cache key (source + compiler_version)
                const full_cache_key = CacheManager.generateCacheKey(env.common.source, self.compiler_version);

                // Collect import info for metadata
                // Note: We use owned strings that we free after storing
                var imports = std.ArrayList(ImportInfo).empty;
                defer {
                    for (imports.items) |*imp| {
                        imp.deinit(self.gpa);
                    }
                    imports.deinit(self.gpa);
                }

                // Add local imports (dupe strings since we'll free them)
                // Read source files directly to compute hashes (more reliable than env)
                for (mod.imports.items) |imp_id| {
                    if (pkg.getModule(imp_id)) |imp_mod| {
                        // Compute source hash by reading the file
                        var imp_source_hash: [32]u8 = std.mem.zeroes([32]u8);
                        const imp_path = self.resolveModulePath(pkg.root_dir, imp_mod.name) catch null;
                        if (imp_path) |path| {
                            defer self.gpa.free(path);
                            if (self.file_provider.read(self.file_provider.ctx, path, self.gpa) catch null) |source| {
                                defer self.gpa.free(source);
                                imp_source_hash = CacheManager.computeSourceHash(source);
                            }
                        }

                        const mod_name = self.gpa.dupe(u8, imp_mod.name) catch continue;
                        imports.append(self.gpa, .{
                            .package = "", // Local import - empty string, not owned
                            .module = mod_name,
                            .source_hash = imp_source_hash,
                        }) catch {
                            self.gpa.free(mod_name);
                            continue;
                        };
                    }
                }

                // Add external imports (these have format "pkg.Module")
                for (mod.external_imports.items) |ext_name| {
                    if (base.module_path.parseQualifiedImport(ext_name)) |qualified| {
                        // Resolve the external package and compute source hash by reading file
                        var imp_source_hash: [32]u8 = std.mem.zeroes([32]u8);
                        if (pkg.shorthands.get(qualified.qualifier)) |ext_pkg_name| {
                            if (self.packages.get(ext_pkg_name)) |ext_pkg| {
                                const imp_path = self.resolveModulePath(ext_pkg.root_dir, qualified.module) catch null;
                                if (imp_path) |path| {
                                    defer self.gpa.free(path);
                                    if (self.file_provider.read(self.file_provider.ctx, path, self.gpa) catch null) |source| {
                                        defer self.gpa.free(source);
                                        imp_source_hash = CacheManager.computeSourceHash(source);
                                    }
                                }
                            }
                        }

                        const pkg_part = self.gpa.dupe(u8, qualified.qualifier) catch continue;
                        const mod_part = self.gpa.dupe(u8, qualified.module) catch {
                            self.gpa.free(pkg_part);
                            continue;
                        };
                        imports.append(self.gpa, .{
                            .package = pkg_part,
                            .module = mod_part,
                            .source_hash = imp_source_hash,
                        }) catch {
                            self.gpa.free(pkg_part);
                            self.gpa.free(mod_part);
                            continue;
                        };
                    }
                }

                // Count errors and warnings
                var error_count: u32 = 0;
                var warning_count: u32 = 0;
                for (mod.reports.items) |*rep| {
                    if (rep.severity == .fatal or rep.severity == .runtime_error) {
                        error_count += 1;
                    } else if (rep.severity == .warning) {
                        warning_count += 1;
                    }
                }

                // Store metadata for fast path lookup
                cache.storeMetadata(source_hash, full_cache_key, imports.items, error_count, warning_count) catch {};

                // Store full cache
                cache.store(full_cache_key, env, error_count, warning_count) catch {};

                // imports are freed by the defer block above
            }
        }

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake local dependents (same package)
        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }

        // Wake cross-package dependents (other packages that import this module)
        try self.wakeCrossPackageDependents(result.package_name, result.module_id);
    }

    /// Handle a parse failure
    fn handleParseFailed(self: *Coordinator, result: *messages.ParseFailure) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] PARSE FAILED: pkg={s} module={s} reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: package '{s}' not found for parse_failed result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' for parse_failed result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        // Store partial env if available
        if (result.partial_env) |env| {
            mod.env = env;
        }

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        // Mark as done (with errors)
        mod.phase = .Done;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake local dependents (they'll see this module as done with errors)
        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }

        // Wake cross-package dependents
        try self.wakeCrossPackageDependents(result.package_name, result.module_id);
    }

    /// Handle cycle detection
    fn handleCycleDetected(self: *Coordinator, result: *messages.CycleDetected) !void {
        const pkg = self.packages.get(result.package_name) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: package '{s}' not found for cycle_detected result (id={})\n", .{
                result.package_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' for cycle_detected result\n", .{
                result.module_id, result.package_name,
            });
            unreachable;
        };

        // Take ownership of module env
        mod.env = result.module_env;

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        // Mark as done (with cycle error)
        mod.phase = .Done;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;
    }

    /// Handle a cache hit result (fast path)
    fn handleCacheHit(self: *Coordinator, result: *CacheHitResult) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] CACHE HIT (fast path): pkg={s} module={s} imports={}\n", .{
                result.package_name,
                result.module_name,
                result.imports.len,
            });
        }

        const pkg = self.packages.get(result.package_name) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: package '{s}' not found for cache_hit result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' for cache_hit result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        // Store cache buffer to keep it alive for the lifetime of module_env
        // It will be freed when the coordinator is deinitialized
        try self.cache_buffers.append(self.gpa, result.cache_data);

        // Set module from cache - mark as Done immediately since env is complete
        mod.env = result.module_env;
        mod.was_cache_hit = true;
        mod.phase = .Done;
        mod.visit_color = .black;

        // Update cache stats
        self.cache_hits += 1;

        // Decrement counters for this module (it's done)
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Process cached imports - ensure they get loaded too for serialization
        // This is similar to processCanonicalizedResult but uses cached import info.
        // The cached module is already Done, we just need its imports to be present
        // in the coordinator so they can be serialized.
        for (result.imports) |imp| {
            // Skip Builtin - it's always available
            if (std.mem.eql(u8, imp.module, "Builtin")) continue;

            if (imp.package.len == 0) {
                // Local import - same package
                const path = self.resolveModulePath(pkg.root_dir, imp.module) catch continue;
                defer self.gpa.free(path);

                const child_id = try pkg.ensureModule(self.gpa, imp.module, path);
                const child = pkg.getModule(child_id).?;

                // Queue parse for new modules (will go through their own cache check)
                if (child.phase == .Parse) {
                    pkg.remaining_modules += 1;
                    self.total_remaining += 1;
                    try self.enqueueParseTask(result.package_name, child_id);
                }

                if (comptime trace_build) {
                    std.debug.print("[COORD] CACHE HIT queued local import: {s}\n", .{imp.module});
                }
            } else {
                // External import - resolve shorthand to package
                const import_name = try std.fmt.allocPrint(self.gpa, "{s}.{s}", .{ imp.package, imp.module });
                defer self.gpa.free(import_name);

                try self.scheduleExternalImport(result.package_name, import_name);

                if (comptime trace_build) {
                    std.debug.print("[COORD] CACHE HIT queued external import: {s}.{s}\n", .{ imp.package, imp.module });
                }
            }
        }

        // Free the imports slice now that we've processed them
        for (result.imports) |*imp| {
            var import_info = imp.*;
            import_info.deinit(self.gpa);
        }
        self.gpa.free(result.imports);

        // Refresh mod pointer after potential resizes from ensureModule calls
        const mod_after_imports = pkg.getModule(result.module_id) orelse {
            if (builtin.mode == .Debug) std.debug.print("BUG: module id={} not found in package '{s}' after imports in cache_hit handler (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        // Wake dependents (they may now be able to use fast path too)
        for (mod_after_imports.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }
        try self.wakeCrossPackageDependents(result.package_name, result.module_id);
    }

    /// Check if all imports in the list were cache hits in this build.
    /// This is used for the fast path - if all dependencies were cache hits,
    /// we can skip parsing/canonicalizing/type-checking and load directly from cache.
    fn checkAllImportsCached(
        self: *Coordinator,
        source_pkg_name: []const u8,
        imports: []const ImportInfo,
    ) bool {
        // Skip "Builtin" since it's always available and doesn't need caching check
        for (imports) |imp| {
            if (std.mem.eql(u8, imp.module, "Builtin")) continue;

            // Determine the target package name
            const pkg_name = if (imp.package.len == 0)
                // Local import - use source package
                source_pkg_name
            else blk: {
                // External import - resolve shorthand to package name
                const source_pkg = self.packages.get(source_pkg_name) orelse {
                    if (comptime trace_build) {
                        std.debug.print("[COORD] checkAllImportsCached: source pkg {s} not found\n", .{source_pkg_name});
                    }
                    return false;
                };
                break :blk source_pkg.shorthands.get(imp.package) orelse {
                    if (comptime trace_build) {
                        std.debug.print("[COORD] checkAllImportsCached: shorthand {s} not found in {s}\n", .{ imp.package, source_pkg_name });
                    }
                    return false;
                };
            };

            // Look up the package to get its root directory
            const pkg = self.packages.get(pkg_name) orelse {
                if (comptime trace_build) {
                    std.debug.print("[COORD] checkAllImportsCached: pkg {s} not found\n", .{pkg_name});
                }
                return false;
            };

            // Resolve the import's file path
            const module_path = self.resolveModulePath(pkg.root_dir, imp.module) catch {
                if (comptime trace_build) {
                    std.debug.print("[COORD] checkAllImportsCached: failed to resolve path for {s}.{s}\n", .{ pkg_name, imp.module });
                }
                return false;
            };
            defer self.gpa.free(module_path);

            // Read the source file and compute its current hash
            const source = self.file_provider.read(self.file_provider.ctx, module_path, self.gpa) catch {
                if (comptime trace_build) {
                    std.debug.print("[COORD] checkAllImportsCached: failed to read {s}\n", .{module_path});
                }
                return false;
            } orelse {
                if (comptime trace_build) {
                    std.debug.print("[COORD] checkAllImportsCached: file not found {s}\n", .{module_path});
                }
                return false;
            };
            defer self.gpa.free(source);

            // Compute current source hash and compare with stored hash
            const current_hash = CacheManager.computeSourceHash(source);
            if (!std.mem.eql(u8, &current_hash, &imp.source_hash)) {
                // Dependency has changed since we cached
                if (comptime trace_build) {
                    std.debug.print("[COORD] checkAllImportsCached: {s}.{s} hash mismatch (file changed)\n", .{ pkg_name, imp.module });
                }
                return false;
            }

            if (comptime trace_build) {
                std.debug.print("[COORD] checkAllImportsCached: {s}.{s} hash matches\n", .{ pkg_name, imp.module });
            }
        }

        return true;
    }

    /// Try to load a module from cache before dispatching a parse task.
    /// Returns true if cache hit (module is now Done), false if cache miss.
    /// This runs in the coordinator, so it can safely access self.packages.
    fn tryCacheHit(self: *Coordinator, pkg: *PackageState, mod: *ModuleState, module_id: ModuleId) bool {
        const cache = self.cache_manager orelse return false;

        // 1. Read source file
        // Note: We cannot use defer to free source because on cache hit,
        // the ModuleEnv stores a reference to the source.
        const source_opt = self.file_provider.read(
            self.file_provider.ctx,
            mod.path,
            self.gpa,
        ) catch return false;
        const source = source_opt orelse return false;

        // 2. Compute source hash
        const source_hash = CacheManager.computeSourceHash(source);

        // 3. Look up metadata by source hash
        var meta = cache.getMetadata(source_hash) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] tryCacheHit MISS (no metadata): pkg={s} module={s}\n", .{
                    pkg.name,
                    mod.name,
                });
            }
            self.gpa.free(source);
            return false;
        };

        // 4. Verify all dependencies' hashes match their current source
        if (!self.checkAllImportsCached(pkg.name, meta.imports)) {
            if (comptime trace_build) {
                std.debug.print("[COORD] tryCacheHit MISS (deps changed): pkg={s} module={s}\n", .{
                    pkg.name,
                    mod.name,
                });
            }
            meta.deinit(self.gpa);
            self.gpa.free(source);
            return false;
        }

        // 5. Load full cache using the key from metadata
        const cache_result = cache.loadFromCacheByKey(
            meta.full_cache_key,
            source,
            mod.name,
        );

        if (cache_result != .hit) {
            if (comptime trace_build) {
                std.debug.print("[COORD] tryCacheHit MISS (cache load failed): pkg={s} module={s}\n", .{
                    pkg.name,
                    mod.name,
                });
            }
            meta.deinit(self.gpa);
            self.gpa.free(source);
            return false;
        }

        if (comptime trace_build) {
            std.debug.print("[COORD] tryCacheHit HIT: pkg={s} module={s} imports={}\n", .{
                pkg.name,
                mod.name,
                meta.imports.len,
            });
        }

        // 6. Apply cache hit - set module state
        // Note: The module_env stores a reference to source, so we do NOT free source here.
        // The source will be freed when the module is deinitialized.
        mod.env = cache_result.hit.module_env;

        // Override qualified_module_ident for cache correctness.
        // The cache is keyed by file content, so the serialized value may be
        // from a different package alias (e.g., "pf.Color" vs "platform.Color").
        if (mod.env) |env| {
            // Enable runtime inserts on the deserialized interner so we can add the qualified name.
            env.common.idents.interner.enableRuntimeInserts(self.gpa) catch {};
            var qualified_buf: [256]u8 = undefined;
            if (std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ pkg.name, mod.name })) |qname| {
                env.qualified_module_ident = env.insertIdent(base.Ident.for_text(qname)) catch env.display_module_name_idx;
            } else |_| {
                env.qualified_module_ident = env.display_module_name_idx;
            }
        }

        mod.was_cache_hit = true;
        mod.phase = .Done;
        mod.visit_color = .black;

        // Store cache buffer to keep it alive
        self.cache_buffers.append(self.gpa, cache_result.hit.cache_data) catch {};

        // Update stats
        self.cache_hits += 1;

        // 7. Process imports from cached metadata
        self.processCachedImportsForHit(pkg, module_id, meta.imports) catch {};

        // Free metadata (imports have been processed)
        meta.deinit(self.gpa);

        return true;
    }

    /// Process imports from cached metadata after a cache hit.
    /// Similar to handleCacheHit but called directly during enqueueParseTask.
    fn processCachedImportsForHit(self: *Coordinator, pkg: *PackageState, module_id: ModuleId, imports: []const ImportInfo) !void {
        for (imports) |imp| {
            // Skip Builtin - it's always available
            if (std.mem.eql(u8, imp.module, "Builtin")) continue;

            if (imp.package.len == 0) {
                // Local import - same package
                const path = self.resolveModulePath(pkg.root_dir, imp.module) catch continue;
                defer self.gpa.free(path);

                const child_id = try pkg.ensureModule(self.gpa, imp.module, path);
                const child = pkg.getModule(child_id).?;

                // Track dependency for this module
                const mod = pkg.getModule(module_id).?;
                try mod.imports.append(self.gpa, child_id);

                // Queue parse for new modules (will go through their own cache check)
                if (child.phase == .Parse) {
                    pkg.remaining_modules += 1;
                    self.total_remaining += 1;
                    try self.enqueueParseTask(pkg.name, child_id);
                }

                if (comptime trace_build) {
                    std.debug.print("[COORD] tryCacheHit queued local import: {s}\n", .{imp.module});
                }
            } else {
                // External import - resolve shorthand to package
                const import_name = try std.fmt.allocPrint(self.gpa, "{s}.{s}", .{ imp.package, imp.module });
                defer self.gpa.free(import_name);

                try self.scheduleExternalImport(pkg.name, import_name);

                if (comptime trace_build) {
                    std.debug.print("[COORD] tryCacheHit queued external import: {s}.{s}\n", .{ imp.package, imp.module });
                }
            }
        }
    }

    /// Handle cycle detection inline during canonicalization result processing
    fn handleCycleInline(self: *Coordinator, pkg: *PackageState, module_id: ModuleId, child_id: ModuleId) !void {
        const mod = pkg.getModule(module_id).?;
        const child = pkg.getModule(child_id).?;

        // Create cycle error report
        var rep = Report.init(self.gpa, "Import cycle detected", .runtime_error);
        const msg = try rep.addOwnedString("This module participates in an import cycle. Cycles between modules are not allowed.");
        try rep.addErrorMessage(msg);
        try mod.reports.append(self.gpa, rep);

        // Mark both as done
        if (mod.phase != .Done) {
            mod.phase = .Done;
            if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
            if (self.total_remaining > 0) self.total_remaining -= 1;
        }

        if (child.phase != .Done) {
            child.phase = .Done;
            if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
            if (self.total_remaining > 0) self.total_remaining -= 1;

            // Add report to child too
            var child_rep = Report.init(self.gpa, "Import cycle detected", .runtime_error);
            const child_msg = try child_rep.addOwnedString("This module participates in an import cycle.");
            try child_rep.addErrorMessage(child_msg);
            try child.reports.append(self.gpa, child_rep);
        }
    }

    /// Try to unblock a module waiting on imports
    fn tryUnblock(self: *Coordinator, pkg: *PackageState, module_id: ModuleId) !void {
        const mod = pkg.getModule(module_id) orelse return;
        if (mod.phase != .WaitingOnImports) return;

        // Check local imports
        for (mod.imports.items) |imp_id| {
            const imp = pkg.getModule(imp_id) orelse continue;
            if (imp.phase != .Done) {
                if (comptime trace_build) {
                    std.debug.print("[COORD] UNBLOCK WAIT: pkg={s} module={s} waiting on local import {}\n", .{ pkg.name, mod.name, imp_id });
                }
                return; // Not ready yet
            }
        }

        // Check external imports
        for (mod.external_imports.items) |ext_name| {
            if (!self.isExternalReady(pkg.name, ext_name)) {
                if (comptime trace_build) {
                    std.debug.print("[COORD] UNBLOCK WAIT: pkg={s} module={s} waiting on external {s}\n", .{ pkg.name, mod.name, ext_name });
                }
                return;
            }
        }

        if (comptime trace_build) {
            std.debug.print("[COORD] UNBLOCK: pkg={s} module={s} -> TypeCheck\n", .{ pkg.name, mod.name });
        }

        // All imports ready - transition to TypeCheck
        mod.phase = .TypeCheck;
        mod.visit_color = .black;

        // Build imported_envs array
        // Pre-allocate with expected capacity: 1 (builtin) + local imports + external imports
        const expected_capacity = 1 + mod.imports.items.len + mod.external_imports.items.len;
        var imported_envs = try std.ArrayList(*ModuleEnv).initCapacity(self.gpa, expected_capacity);
        defer imported_envs.deinit(self.gpa);

        // Always include builtin first
        try imported_envs.append(self.gpa, self.builtin_modules.builtin_module.env);

        // Add local imports
        for (mod.imports.items) |imp_id| {
            const imp = pkg.getModule(imp_id).?;
            if (imp.env) |env| {
                try imported_envs.append(self.gpa, env);
            }
        }

        // Use a StringHashMap for O(1) duplicate detection when adding transitive deps
        var seen_modules = std.StringHashMap(void).init(self.gpa);
        defer seen_modules.deinit();

        // Mark already-added imports as seen (builtin + local imports)
        for (imported_envs.items) |env| {
            try seen_modules.put(env.module_name, {});
        }

        // Add external imports and their transitive dependencies in one pass.
        // Transitive deps ensure we have access to module environments for types
        // used in where clauses even when not directly imported by this module.
        for (mod.external_imports.items) |ext_name| {
            const ext_env = self.getExternalEnv(pkg.name, ext_name) orelse continue;
            try imported_envs.append(self.gpa, ext_env);

            // Parse "pf.Wrapper" -> { .qualifier = "pf", .module = "Wrapper" }
            const qualified = base.module_path.parseQualifiedImport(ext_name) orelse continue;
            const target_pkg_name = pkg.shorthands.get(qualified.qualifier) orelse continue;
            const target_pkg = self.packages.get(target_pkg_name) orelse continue;

            // Add transitive dependencies from this external module
            for (ext_env.imports.imports.items.items) |trans_str_idx| {
                const trans_name = ext_env.getString(trans_str_idx);

                // Skip if already seen (O(1) lookup)
                if (seen_modules.contains(trans_name)) continue;

                // Resolve the transitive import from the same target package
                if (target_pkg.getEnvIfDone(trans_name)) |trans_env| {
                    try imported_envs.append(self.gpa, trans_env);
                    try seen_modules.put(trans_name, {});
                }
            }
        }

        // DEBUG: Verify all imports are completed before type-checking.
        // This ensures we're not passing pointers to still-being-modified modules.
        if (builtin.mode == .Debug) {
            for (mod.imports.items) |imp_id| {
                const imp = pkg.getModule(imp_id).?;
                std.debug.assert(imp.phase == .Done);
            }
        }

        // Enqueue type-check task
        try self.enqueueTask(.{
            .type_check = .{
                .package_name = pkg.name,
                .module_id = module_id,
                .module_name = mod.name,
                .path = mod.path,
                .module_env = mod.env.?,
                .imported_envs = try imported_envs.toOwnedSlice(self.gpa),
            },
        });
    }

    /// Schedule an external import in its owning package
    /// Also registers the source module as a cross-package dependent of the target
    pub fn scheduleExternalImport(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] SCHEDULE EXT IMPORT: from {s} importing {s}\n", .{ source_pkg, import_name });
        }

        // Parse "pf.Stdout" -> { .qualifier = "pf", .module = "Stdout" }
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return;

        // Resolve shorthand to target package
        const source = self.packages.get(source_pkg) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: source pkg {s} not found\n", .{source_pkg});
            }
            return;
        };
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: shorthand {s} not found in {s}\n", .{ qualified.qualifier, source_pkg });
            }
            return;
        };

        // Get or create module in target package
        const target_pkg = self.packages.get(target_pkg_name) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: target pkg {s} not found\n", .{target_pkg_name});
            }
            return;
        };
        const path = try self.resolveModulePath(target_pkg.root_dir, qualified.module);
        defer self.gpa.free(path);

        const module_id = try target_pkg.ensureModule(self.gpa, qualified.module, path);
        const mod = target_pkg.getModule(module_id).?;

        if (comptime trace_build) {
            std.debug.print("[COORD] SCHEDULE EXT IMPORT: resolved to {s}:{} phase={}\n", .{ target_pkg_name, module_id, mod.phase });
        }

        // Queue parse if new
        if (mod.phase == .Parse) {
            target_pkg.remaining_modules += 1;
            self.total_remaining += 1;
            try self.enqueueParseTask(target_pkg_name, module_id);
        }
    }

    /// Register a cross-package dependent: when target module completes, wake source module
    fn registerCrossPackageDependent(
        self: *Coordinator,
        target_pkg: []const u8,
        target_module_id: ModuleId,
        source_pkg: []const u8,
        source_module_id: ModuleId,
    ) !void {
        if (comptime trace_build) {
            std.debug.print("[COORD] REGISTER CROSS-PKG DEP: {s}:{} depends on {s}:{}\n", .{ source_pkg, source_module_id, target_pkg, target_module_id });
        }

        // Build key: "pkg_name:module_id"
        const key = try std.fmt.allocPrint(self.gpa, "{s}:{d}", .{ target_pkg, target_module_id });
        errdefer self.gpa.free(key);

        const gop = try self.cross_package_dependents.getOrPut(key);
        if (!gop.found_existing) {
            gop.value_ptr.* = std.ArrayList(ModuleRef).empty;
        } else {
            // Key already existed, free our duplicate
            self.gpa.free(key);
        }

        // Add the source module as a dependent
        try gop.value_ptr.append(self.gpa, .{
            .pkg_name = source_pkg,
            .module_id = source_module_id,
        });
    }

    /// Wake all cross-package dependents of a completed module
    fn wakeCrossPackageDependents(self: *Coordinator, pkg_name: []const u8, module_id: ModuleId) !void {
        // Build key
        var key_buf: [256]u8 = undefined;
        const key = std.fmt.bufPrint(&key_buf, "{s}:{d}", .{ pkg_name, module_id }) catch return;

        const dependents = self.cross_package_dependents.get(key) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] WAKE CROSS-PKG: {s}:{} has no dependents\n", .{ pkg_name, module_id });
            }
            return;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] WAKE CROSS-PKG: {s}:{} waking {} dependents\n", .{ pkg_name, module_id, dependents.items.len });
        }

        for (dependents.items) |dep_ref| {
            const dep_pkg = self.packages.get(dep_ref.pkg_name) orelse continue;
            try self.tryUnblock(dep_pkg, dep_ref.module_id);
        }
    }

    /// Check if an external import is ready.
    /// Returns true if:
    /// - The target module is done (has completed compilation), OR
    /// - The import cannot be resolved (invalid shorthand, missing package, etc.)
    ///   In the latter case, we return true to allow the module to proceed to
    ///   type-checking, where the unresolved import will produce a proper error.
    ///   Returning false would cause the coordinator to wait forever for something
    ///   that will never be ready.
    pub fn isExternalReady(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) bool {
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return true;

        const source = self.packages.get(source_pkg) orelse return true;
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse return true;
        const target_pkg = self.packages.get(target_pkg_name) orelse return true;

        // Use isDone instead of getEnvIfDone - a module that failed to parse
        // is still "done" and shouldn't block dependents (even if it has no env)
        return target_pkg.isDone(qualified.module);
    }

    /// Get the ModuleEnv for an external import
    pub fn getExternalEnv(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) ?*ModuleEnv {
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return null;

        const source = self.packages.get(source_pkg) orelse return null;
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse return null;
        const target_pkg = self.packages.get(target_pkg_name) orelse return null;

        return target_pkg.getEnvIfDone(qualified.module);
    }

    /// Get build statistics for this compilation
    pub fn getBuildStats(self: *Coordinator) compile_build.BuildEnv.BuildStats {
        return .{
            .modules_total = self.cache_hits + self.modules_compiled,
            .cache_hits = self.cache_hits,
            .cache_misses = self.cache_misses,
            .modules_compiled = self.modules_compiled,
            .module_time_min_ns = self.module_time_min_ns,
            .module_time_max_ns = self.module_time_max_ns,
            .module_time_sum_ns = self.module_time_sum_ns,
        };
    }

    /// Resolve a module name to a path
    fn resolveModulePath(self: *Coordinator, root_dir: []const u8, mod_name: []const u8) ![]const u8 {
        return self.resolveModulePathWithAllocator(root_dir, mod_name, self.gpa);
    }

    /// Resolve a module name to a path using a specific allocator
    fn resolveModulePathWithAllocator(_: *Coordinator, root_dir: []const u8, mod_name: []const u8, alloc: Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).empty;
        defer buffer.deinit(alloc);

        var it = std.mem.splitScalar(u8, mod_name, '.');
        var first = true;
        while (it.next()) |part| {
            if (!first) try buffer.appendSlice(alloc, std.fs.path.sep_str) else first = false;
            try buffer.appendSlice(alloc, part);
        }
        try buffer.appendSlice(alloc, ".roc");

        const rel = try buffer.toOwnedSlice(alloc);
        defer alloc.free(rel);

        return try std.fs.path.join(alloc, &.{ root_dir, rel });
    }

    /// Execute a parse task (pure function)
    fn executeParse(self: *Coordinator, task: ParseTask) WorkerResult {
        const start_time = if (threads_available) std.time.nanoTimestamp() else 0;

        // Read source
        const src = self.readModuleSource(task.path) catch |err| {
            // Use worker allocator (thread-safe in multi-threaded mode)
            const worker_alloc = self.getWorkerAllocator();
            // Pre-allocate to reduce allocation contention in multi-threaded mode
            var reports = std.ArrayList(Report).initCapacity(worker_alloc, 1) catch std.ArrayList(Report).empty;
            var rep = Report.init(worker_alloc, "FILE NOT FOUND", .fatal);
            // Include the path in the error message for debugging
            const path_msg = std.fmt.allocPrint(worker_alloc, "{s}: {s}", .{ task.path, @errorName(err) }) catch @errorName(err);
            defer if (path_msg.ptr != @errorName(err).ptr) worker_alloc.free(path_msg);
            rep.addErrorMessage(path_msg) catch {};
            reports.append(worker_alloc, rep) catch {};

            return .{
                .parse_failed = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .reports = reports,
                    .partial_env = null,
                },
            };
        };

        // Note: Cache checking now happens in enqueueParseTask (coordinator level)
        // before tasks are dispatched. This works for both single and multi-threaded modes.

        // Parse, canonicalize, type-check

        // Create ModuleEnv using module allocator (for IPC, this is shared memory)
        const module_alloc = self.getModuleAllocator();
        const env = module_alloc.create(ModuleEnv) catch {
            // Note: In IPC mode (SharedMemoryAllocator), free is a no-op
            if (self.owns_module_data) module_alloc.free(src);
            return .{
                .parse_failed = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .reports = std.ArrayList(Report).empty,
                    .partial_env = null,
                },
            };
        };

        env.* = ModuleEnv.init(module_alloc, src) catch {
            if (self.owns_module_data) {
                module_alloc.destroy(env);
                module_alloc.free(src);
            }
            return .{
                .parse_failed = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .reports = std.ArrayList(Report).empty,
                    .partial_env = null,
                },
            };
        };

        // Initialize CIR fields
        // For IPC mode, module_name must be in shared memory (it will be accessed after coordinator deinit)
        const module_name_for_env = if (self.module_allocator != null)
            module_alloc.dupe(u8, task.module_name) catch task.module_name
        else
            task.module_name;
        env.initCIRFields(module_name_for_env) catch {};

        // Set qualified_module_ident to a package-qualified identifier (e.g., "app.main", "pf.Stdout")
        // to ensure module identity is unique across packages. Without this, two modules with
        // the same filename in different packages (e.g., app's main.roc and platform's main.roc)
        // get the same identity, causing nominal type origin_module collisions.
        // display_module_name_idx stays as the bare name (for type module validation, error messages, etc.)
        {
            var qualified_buf: [256]u8 = undefined;
            if (std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ task.package_name, task.module_name })) |qname| {
                env.qualified_module_ident = env.insertIdent(base.Ident.for_text(qname)) catch env.display_module_name_idx;
            } else |_| {
                env.qualified_module_ident = env.display_module_name_idx;
            }
        }
        env.common.calcLineStarts(module_alloc) catch {};

        // Parse
        // Use worker allocator (thread-safe in multi-threaded mode) for all worker allocations
        const worker_alloc = self.getWorkerAllocator();
        // Pre-allocate reports to reduce allocation contention in multi-threaded mode
        var reports = std.ArrayList(Report).initCapacity(worker_alloc, 8) catch std.ArrayList(Report).empty;
        var allocators: base.Allocators = undefined;
        allocators.initInPlace(worker_alloc);
        // NOTE: allocators not freed here - cleanup happens in executeCanonicalize
        const parse_ast = parse.parse(&allocators, &env.common) catch {
            // Parse failed but we still have partial env
            const end_time = if (threads_available) std.time.nanoTimestamp() else 0;
            return .{
                .parsed = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .module_env = env,
                    .cached_ast = undefined, // Will be handled in error case
                    .reports = reports,
                    .parse_ns = if (threads_available) @intCast(end_time - start_time) else 0,
                },
            };
        };
        parse_ast.store.emptyScratch();

        // parse_ast is already heap-allocated by parse.parse

        // Collect parse diagnostics
        for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
            const rep = parse_ast.tokenizeDiagnosticToReport(diagnostic, worker_alloc, task.path) catch continue;
            reports.append(worker_alloc, rep) catch {};
        }
        for (parse_ast.parse_diagnostics.items) |diagnostic| {
            const rep = parse_ast.parseDiagnosticToReport(&env.common, diagnostic, worker_alloc, task.path) catch continue;
            reports.append(worker_alloc, rep) catch {};
        }

        const end_time = if (threads_available) std.time.nanoTimestamp() else 0;

        return .{
            .parsed = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .module_env = env,
                .cached_ast = parse_ast,
                .reports = reports,
                .parse_ns = if (threads_available) @intCast(end_time - start_time) else 0,
            },
        };
    }

    /// Execute a canonicalize task (pure function)
    fn executeCanonicalize(self: *Coordinator, task: CanonicalizeTask) WorkerResult {
        const start_time = if (threads_available) std.time.nanoTimestamp() else 0;

        const env = task.module_env;
        const ast = task.cached_ast;

        // Extract qualified imports from AST to set up placeholders for external modules
        // Use worker allocator for temporary allocations during canonicalization (thread-safe)
        const canon_alloc = self.getWorkerAllocator();
        const qualified_imports = module_discovery.extractQualifiedImportsFromAST(ast, canon_alloc) catch &[_][]const u8{};
        defer {
            for (qualified_imports) |qi| canon_alloc.free(qi);
            canon_alloc.free(qualified_imports);
        }

        // Build KnownModule entries for qualified imports so they get placeholders
        var known_modules = std.ArrayList(compile_package.PackageEnv.KnownModule).empty;
        defer known_modules.deinit(canon_alloc);
        for (qualified_imports) |qi| {
            known_modules.append(canon_alloc, .{
                .qualified_name = qi,
                .import_name = qi,
            }) catch {};
        }

        // Canonicalize using the PackageEnv shared function with sibling awareness
        // This sets up placeholders for external imports that will be resolved during type-checking
        // Use worker allocator for thread safety in multi-threaded mode
        var allocators: base.Allocators = undefined;
        allocators.initInPlace(canon_alloc);
        defer allocators.deinit();
        compile_package.PackageEnv.canonicalizeModuleWithSiblings(
            &allocators,
            env,
            ast,
            self.builtin_modules.builtin_module.env,
            self.builtin_modules.builtin_indices,
            task.root_dir,
            task.package_name,
            null, // Coordinator handles import resolution separately
            known_modules.items,
        ) catch {};

        const canon_end = if (threads_available) std.time.nanoTimestamp() else 0;

        // Collect diagnostics
        const diag_start = if (threads_available) std.time.nanoTimestamp() else 0;
        // Use worker allocator (thread-safe in multi-threaded mode) for result data
        const worker_alloc = self.getWorkerAllocator();
        // Pre-allocate to reduce allocation contention in multi-threaded mode
        var reports = std.ArrayList(Report).initCapacity(worker_alloc, 8) catch std.ArrayList(Report).empty;
        const diags = env.getDiagnostics() catch &[_]CIR.Diagnostic{};
        // Free with env.gpa since that's what getDiagnostics uses for allocation
        // (In IPC mode, this is a no-op since SharedMemoryAllocator.free does nothing)
        defer env.gpa.free(diags);
        for (diags) |d| {
            const rep = env.diagnosticToReport(d, worker_alloc, task.path) catch continue;
            reports.append(worker_alloc, rep) catch {};
        }
        const diag_end = if (threads_available) std.time.nanoTimestamp() else 0;

        // Discover imports from env.imports
        // Pre-allocate to reduce allocation contention in multi-threaded mode
        var local_imports = std.ArrayList(DiscoveredLocalImport).initCapacity(worker_alloc, 16) catch std.ArrayList(DiscoveredLocalImport).empty;
        var external_imports = std.ArrayList(DiscoveredExternalImport).initCapacity(worker_alloc, 16) catch std.ArrayList(DiscoveredExternalImport).empty;

        const import_count = env.imports.imports.items.items.len;
        for (env.imports.imports.items.items[0..import_count]) |str_idx| {
            const mod_name = env.getString(str_idx);

            if (std.mem.eql(u8, mod_name, "Builtin")) continue;

            // Check if qualified (external) import
            if (std.mem.indexOfScalar(u8, mod_name, '.') != null) {
                external_imports.append(worker_alloc, .{
                    .import_name = worker_alloc.dupe(u8, mod_name) catch continue,
                }) catch {};
            } else {
                // Local import - but first check if this is a shorthand alias for an external module
                // Type annotations can use unqualified names (like "Builder" instead of "pf.Builder")
                // which adds both the qualified and unqualified import to the list.
                // Skip the unqualified one if we already have the qualified version.
                var is_external_alias = false;
                for (external_imports.items) |ext| {
                    // Check if any external import ends with .mod_name
                    // e.g., "pf.Builder" ends with ".Builder"
                    if (std.mem.endsWith(u8, ext.import_name, mod_name)) {
                        const dot_idx = ext.import_name.len - mod_name.len - 1;
                        if (dot_idx < ext.import_name.len and ext.import_name[dot_idx] == '.') {
                            is_external_alias = true;
                            break;
                        }
                    }
                }
                if (is_external_alias) continue;

                const path = self.resolveModulePathWithAllocator(task.root_dir, mod_name, worker_alloc) catch continue;
                local_imports.append(worker_alloc, .{
                    .module_name = worker_alloc.dupe(u8, mod_name) catch {
                        worker_alloc.free(path);
                        continue;
                    },
                    .path = path,
                }) catch {
                    worker_alloc.free(path);
                };
            }
        }

        // Free AST - deinit now handles both internal cleanup and self-destruction
        ast.deinit();

        return .{
            .canonicalized = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .module_env = env,
                .discovered_local_imports = local_imports,
                .discovered_external_imports = external_imports,
                .reports = reports,
                .canonicalize_ns = if (threads_available) @intCast(canon_end - start_time) else 0,
                .canonicalize_diagnostics_ns = if (threads_available) @intCast(diag_end - diag_start) else 0,
            },
        };
    }

    /// Execute a type-check task (pure function)
    fn executeTypeCheck(self: *Coordinator, task: TypeCheckTask) WorkerResult {
        const start_time = if (threads_available) std.time.nanoTimestamp() else 0;

        const env = task.module_env;

        // Resolve imports
        env.imports.resolveImports(env, task.imported_envs);
        env.store.resolvePendingLookups(env, task.imported_envs);

        // Type check - use worker allocator for thread safety
        const check_alloc = self.getWorkerAllocator();
        var checker = compile_package.PackageEnv.typeCheckModule(
            check_alloc,
            env,
            self.builtin_modules.builtin_module.env,
            task.imported_envs,
            self.target,
        ) catch {
            return .{
                .type_checked = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .module_env = env,
                    .reports = std.ArrayList(Report).empty,
                    .type_check_ns = 0,
                    .check_diagnostics_ns = 0,
                },
            };
        };
        defer checker.deinit();

        const check_end = if (threads_available) std.time.nanoTimestamp() else 0;

        // Collect diagnostics
        const diag_start = if (threads_available) std.time.nanoTimestamp() else 0;
        // Use worker allocator (thread-safe in multi-threaded mode) for result data
        const worker_alloc = self.getWorkerAllocator();
        // Pre-allocate to reduce allocation contention in multi-threaded mode
        var reports = std.ArrayList(Report).initCapacity(worker_alloc, 8) catch std.ArrayList(Report).empty;

        const check = @import("check");
        var rb = check.ReportBuilder.init(
            worker_alloc,
            env,
            env,
            &checker.snapshots,
            &checker.problems,
            task.path,
            task.imported_envs,
            &checker.import_mapping,
            &checker.regions,
        ) catch {
            // On allocation failure, return result with empty reports
            self.gpa.free(task.imported_envs);
            return .{
                .type_checked = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .module_env = env,
                    .reports = reports,
                    .type_check_ns = 0,
                    .check_diagnostics_ns = 0,
                },
            };
        };
        defer rb.deinit();

        for (checker.problems.problems.items) |prob| {
            const rep = rb.build(prob) catch continue;
            reports.append(worker_alloc, rep) catch {};
        }

        const diag_end = if (threads_available) std.time.nanoTimestamp() else 0;

        // Free imported_envs slice (owned by coordinator)
        self.gpa.free(task.imported_envs);

        return .{
            .type_checked = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .module_env = env,
                .reports = reports,
                .type_check_ns = if (threads_available) @intCast(check_end - start_time) else 0,
                .check_diagnostics_ns = if (threads_available) @intCast(diag_end - diag_start) else 0,
            },
        };
    }

    /// Read module source using the file provider
    fn readModuleSource(self: *Coordinator, path: []const u8) ![]u8 {
        const module_alloc = self.getModuleAllocator();
        const data = try self.file_provider.read(self.file_provider.ctx, path, module_alloc) orelse
            return error.FileNotFound;

        // Normalize line endings
        return base.source_utils.normalizeLineEndingsRealloc(module_alloc, data);
    }

    /// Worker thread main function
    fn workerThread(self: *Coordinator) void {
        // Each worker has its own allocators for thread safety.
        // - gpa: page_allocator for long-lived data (ModuleEnv, source)
        // - arena: for temporary allocations, reset between tasks
        const backing = if (threads_available) std.heap.page_allocator else self.gpa;
        var worker_allocs = WorkerAllocators.init(backing);
        defer worker_allocs.deinit();

        while (true) {
            // Block until a task is available. Returns null when the channel
            // is closed and drained, meaning it's time to shut down.
            const t = self.task_channel.recv() orelse break;

            // Execute task
            const result = self.executeTaskInline(t);

            // Reset arena between tasks to reclaim temporary allocations
            worker_allocs.resetArena();

            // Send result
            self.result_channel.send(result) catch break;
        }
    }
};

test "Coordinator basic initialization" {
    const allocator = std.testing.allocator;

    // Create minimal builtin modules mock
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined, // builtin_modules - not used in this test
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    try std.testing.expect(coord.total_remaining == 0);
    try std.testing.expect(coord.task_channel.isEmpty());
    try std.testing.expect(coord.isComplete());
}

test "Coordinator package creation" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Create a package
    const pkg = try coord.ensurePackage("app", "/test/app");
    try std.testing.expectEqualStrings("app", pkg.name);
    try std.testing.expectEqualStrings("/test/app", pkg.root_dir);

    // Verify package is stored
    const retrieved = coord.packages.get("app");
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqualStrings("app", retrieved.?.name);
}

test "Coordinator module creation" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    const module_id = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    try std.testing.expectEqual(@as(ModuleId, 0), module_id);

    const mod = pkg.getModule(module_id);
    try std.testing.expect(mod != null);
    try std.testing.expectEqualStrings("Main", mod.?.name);
    try std.testing.expectEqualStrings("/test/app/Main.roc", mod.?.path);
    try std.testing.expect(mod.?.phase == .Parse);
}

test "Coordinator task queue" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    _ = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    // Enqueue a task directly
    try coord.enqueueTask(.{
        .parse = .{
            .package_name = "app",
            .module_id = 0,
            .module_name = "Main",
            .path = "/test/app/Main.roc",
            .depth = 0,
        },
    });

    try std.testing.expectEqual(@as(usize, 1), coord.task_channel.len());

    // Receive the task
    const task = coord.task_channel.tryRecv();
    try std.testing.expect(task != null);
    try std.testing.expect(task.? == .parse);
    try std.testing.expectEqualStrings("app", task.?.parse.package_name);
}

test "Coordinator isComplete logic" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Initially complete (nothing to do)
    try std.testing.expect(coord.isComplete());

    // Add a remaining module
    coord.total_remaining = 1;
    try std.testing.expect(!coord.isComplete());

    // Clear remaining but add task
    coord.total_remaining = 0;
    try coord.enqueueTask(.{
        .parse = .{
            .package_name = "test",
            .module_id = 0,
            .module_name = "Test",
            .path = "/test.roc",
            .depth = 0,
        },
    });
    try std.testing.expect(!coord.isComplete());

    // Clear task but add inflight
    _ = coord.task_channel.tryRecv();
    coord.inflight.store(1, .release);
    try std.testing.expect(!coord.isComplete());

    // All clear - should be complete
    coord.inflight.store(0, .release);
    try std.testing.expect(coord.isComplete());
}

test "Channel in coordinator context" {
    // Skip on wasm
    if (builtin.target.cpu.arch == .wasm32) return error.SkipZigTest;

    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .multi_threaded,
        2,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Test that the result channel works
    const test_result = WorkerResult{
        .parse_failed = .{
            .package_name = "test",
            .module_id = 0,
            .module_name = "Test",
            .path = "/test.roc",
            .reports = std.ArrayList(reporting.Report).empty,
            .partial_env = null,
        },
    };

    try coord.result_channel.send(test_result);

    const received = coord.result_channel.tryRecv();
    try std.testing.expect(received != null);
    try std.testing.expect(received.? == .parse_failed);
}

test "Coordinator enqueueParseTask flow" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Create package
    const pkg = try coord.ensurePackage("app", "/test/app");

    // Create module
    const module_id = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    // Set up remaining count (simulating buildWithCoordinator)
    pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Enqueue parse task
    try coord.enqueueParseTask("app", module_id);

    // Verify task was queued
    try std.testing.expectEqual(@as(usize, 1), coord.task_channel.len());

    // Verify it's a parse task for the right module
    const task = coord.task_channel.tryRecv().?;
    try std.testing.expect(task == .parse);
    try std.testing.expectEqualStrings("app", task.parse.package_name);
    try std.testing.expectEqual(@as(ModuleId, 0), task.parse.module_id);
    try std.testing.expectEqualStrings("Main", task.parse.module_name);
}

test "Coordinator single-threaded loop with mock result" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    const module_id = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    // Set up remaining count
    pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Instead of actually parsing, let's manually process a "completed" result
    // This simulates what would happen after a module completes type-checking

    // Mark module as done directly
    const mod = pkg.getModule(module_id).?;
    mod.phase = .Done;
    pkg.remaining_modules = 0;
    coord.total_remaining = 0;

    // Now the coordinator should be complete
    try std.testing.expect(coord.isComplete());
}

test "Coordinator CI failure scenario - app with platform cross-package imports" {
    // This test mirrors the exact module graph from the CI failure:
    //   Package app: 1 module (expect_with_main) with 2 external imports (pf.Stdout, pf.Stderr)
    //   Package pf: 6 modules (main, Stdout, Stderr, Stdin, Builder, Host)
    //     where main imports Stdout, Stderr, Stdin, Builder, Host locally
    //
    // The bug was that result handlers silently dropped results when package/module
    // lookups failed (orelse return), causing modules to get stuck forever.
    // This test verifies the module graph setup and completion logic works
    // for this exact structure.

    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Set up "app" package
    const app_pkg = try coord.ensurePackage("app", "/test/app");

    // Set up shorthand: app's "pf" -> package "pf"
    const sh_key = try allocator.dupe(u8, "pf");
    const sh_val = try allocator.dupe(u8, "pf");
    try app_pkg.shorthands.put(sh_key, sh_val);

    const app_mod_id = try app_pkg.ensureModule(allocator, "expect_with_main", "/test/app/expect_with_main.roc");
    app_pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Set up "pf" package
    const pf_pkg = try coord.ensurePackage("pf", "/test/pf");
    const pf_main_id = try pf_pkg.ensureModule(allocator, "main", "/test/pf/main.roc");
    const pf_stdout_id = try pf_pkg.ensureModule(allocator, "Stdout", "/test/pf/Stdout.roc");
    const pf_stderr_id = try pf_pkg.ensureModule(allocator, "Stderr", "/test/pf/Stderr.roc");
    const pf_stdin_id = try pf_pkg.ensureModule(allocator, "Stdin", "/test/pf/Stdin.roc");
    const pf_builder_id = try pf_pkg.ensureModule(allocator, "Builder", "/test/pf/Builder.roc");
    const pf_host_id = try pf_pkg.ensureModule(allocator, "Host", "/test/pf/Host.roc");
    pf_pkg.remaining_modules = 6;
    coord.total_remaining += 6;

    // Set up local imports for pf.main -> Stdout, Stderr, Stdin, Builder, Host
    const pf_main = pf_pkg.getModule(pf_main_id).?;
    try pf_main.imports.append(allocator, pf_stdout_id);
    try pf_main.imports.append(allocator, pf_stderr_id);
    try pf_main.imports.append(allocator, pf_stdin_id);
    try pf_main.imports.append(allocator, pf_builder_id);
    try pf_main.imports.append(allocator, pf_host_id);
    pf_main.phase = .WaitingOnImports;

    // Set up external imports for app.expect_with_main -> pf.Stdout, pf.Stderr
    const app_mod = app_pkg.getModule(app_mod_id).?;
    try app_mod.external_imports.append(allocator, try allocator.dupe(u8, "pf.Stdout"));
    try app_mod.external_imports.append(allocator, try allocator.dupe(u8, "pf.Stderr"));
    app_mod.phase = .WaitingOnImports;

    // Register pf.main as dependent of its local imports
    const pf_stdout = pf_pkg.getModule(pf_stdout_id).?;
    try pf_stdout.dependents.append(allocator, pf_main_id);
    const pf_stderr = pf_pkg.getModule(pf_stderr_id).?;
    try pf_stderr.dependents.append(allocator, pf_main_id);
    const pf_stdin = pf_pkg.getModule(pf_stdin_id).?;
    try pf_stdin.dependents.append(allocator, pf_main_id);
    const pf_builder = pf_pkg.getModule(pf_builder_id).?;
    try pf_builder.dependents.append(allocator, pf_main_id);
    const pf_host = pf_pkg.getModule(pf_host_id).?;
    try pf_host.dependents.append(allocator, pf_main_id);

    // Verify initial state matches the CI failure scenario
    try std.testing.expectEqual(@as(usize, 7), coord.total_remaining);
    try std.testing.expect(!coord.isComplete());

    // Simulate: complete all pf leaf modules (Stdout, Stderr, Stdin, Builder, Host)
    // Mark them Done and decrement counters (as handleTypeChecked would do)
    const pf_leaf_ids = [_]ModuleId{ pf_stdout_id, pf_stderr_id, pf_stdin_id, pf_builder_id, pf_host_id };
    for (pf_leaf_ids) |leaf_id| {
        const leaf = pf_pkg.getModule(leaf_id).?;
        leaf.phase = .Done;
        pf_pkg.remaining_modules -= 1;
        coord.total_remaining -= 1;
    }

    // pf.main should still be WaitingOnImports (all imports are now Done but
    // nobody called tryUnblock yet - in real code handleTypeChecked wakes dependents)
    try std.testing.expect(pf_pkg.getModule(pf_main_id).?.phase == .WaitingOnImports);

    // Verify external import readiness via the public isExternalReady API
    // pf.Stdout and pf.Stderr should be ready (they're Done)
    try std.testing.expect(coord.isExternalReady("app", "pf.Stdout"));
    try std.testing.expect(coord.isExternalReady("app", "pf.Stderr"));

    // Now simulate completing pf.main
    pf_pkg.getModule(pf_main_id).?.phase = .Done;
    pf_pkg.remaining_modules -= 1;
    coord.total_remaining -= 1;

    // Verify pf is fully complete
    try std.testing.expectEqual(@as(usize, 0), pf_pkg.remaining_modules);

    // Now simulate completing app.expect_with_main
    app_pkg.getModule(app_mod_id).?.phase = .Done;
    app_pkg.remaining_modules -= 1;
    coord.total_remaining -= 1;

    // Verify everything is complete
    try std.testing.expectEqual(@as(usize, 0), coord.total_remaining);
    try std.testing.expectEqual(@as(usize, 0), app_pkg.remaining_modules);
    try std.testing.expectEqual(@as(usize, 0), pf_pkg.remaining_modules);

    // Verify all modules reached Done
    for (app_pkg.modules.items) |m| {
        try std.testing.expect(m.phase == .Done);
    }
    for (pf_pkg.modules.items) |m| {
        try std.testing.expect(m.phase == .Done);
    }

    try std.testing.expect(coord.isComplete());
}

test "Coordinator handleParseFailed advances module to Done" {
    // Verifies that handleParseFailed (which previously had orelse return)
    // correctly transitions a module to Done and decrements counters.
    // If the package/module lookup silently returned, the module would
    // stay in Parsing forever — exactly the bug from CI.
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    const module_id = try pkg.ensureModule(allocator, "Builder", "/test/app/Builder.roc");
    pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Set module to Parsing (as enqueueParseTask would do)
    const mod = pkg.getModule(module_id).?;
    mod.phase = .Parsing;

    // Feed a parse_failed result through handleResult.
    // This exercises the package/module lookup that previously used orelse return.
    const result: WorkerResult = .{
        .parse_failed = .{
            .package_name = "app",
            .module_id = module_id,
            .module_name = "Builder",
            .path = "/test/app/Builder.roc",
            .reports = std.ArrayList(reporting.Report).empty,
            .partial_env = null,
        },
    };
    try coord.handleResult(result);

    // Verify module advanced to Done (not stuck in Parsing)
    const mod_after = pkg.getModule(module_id).?;
    try std.testing.expect(mod_after.phase == .Done);

    // Verify counters decremented
    try std.testing.expectEqual(@as(usize, 0), pkg.remaining_modules);
    try std.testing.expectEqual(@as(usize, 0), coord.total_remaining);
    try std.testing.expect(coord.isComplete());
}

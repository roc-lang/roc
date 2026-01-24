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

// Compile-time flag for cache tracing - enabled via `zig build -Dtrace-cache`
const trace_cache = if (@hasDecl(build_options, "trace_cache")) build_options.trace_cache else false;

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
const Mutex = if (threads_available) std.Thread.Mutex else struct {
    pub fn lock(_: *@This()) void {}
    pub fn unlock(_: *@This()) void {}
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
    /// DFS visit color for cycle detection: 0=white, 1=gray, 2=black
    visit_color: u8,
    /// True if this module was loaded from cache in this build.
    /// Used by parent modules to determine if fast path is valid.
    was_cache_hit: bool,
    /// Accumulated compile time for this module (parse + canonicalize + type-check)
    compile_time_ns: u64,

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
            .visit_color = 0,
            .was_cache_hit = false,
            .compile_time_ns = 0,
        };
    }

    pub fn deinit(self: *ModuleState, gpa: Allocator, owns_module_data: bool) void {
        if (comptime trace_cache) {
            std.debug.print("[MOD DEINIT] {s}: starting, env={}, ast={}, owns={}\n", .{
                self.name,
                if (self.env != null) @as(u8, 1) else @as(u8, 0),
                if (self.cached_ast != null) @as(u8, 1) else @as(u8, 0),
                if (owns_module_data) @as(u8, 1) else @as(u8, 0),
            });
        }
        // Free cached AST if present (always in gpa, not module_allocator)
        if (self.cached_ast) |ast| {
            if (comptime trace_cache) {
                std.debug.print("[MOD DEINIT] {s}: freeing ast\n", .{self.name});
            }
            ast.deinit(gpa);
            gpa.destroy(ast);
        }

        // Free module env if present (only if we own module data)
        if (owns_module_data) {
            if (self.env) |env| {
                // For cached modules, the env struct is deserialized into the cache buffer (don't free it),
                // but the source is heap-allocated separately and MUST be freed.
                if (self.was_cache_hit) {
                    if (comptime trace_cache) {
                        std.debug.print("[MOD DEINIT] {s}: skipping env.deinit (cached module), but freeing source\n", .{self.name});
                    }
                    // Free the heap-allocated source (it's NOT part of the cache buffer)
                    const source = env.common.source;
                    if (source.len > 0) gpa.free(source);
                } else {
                    if (comptime trace_cache) {
                        std.debug.print("[MOD DEINIT] {s}: freeing env\n", .{self.name});
                    }
                    const source = env.common.source;
                    env.deinit();
                    gpa.destroy(env);
                    if (source.len > 0) gpa.free(source);
                }
            }
        }

        if (comptime trace_cache) {
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
        if (comptime trace_cache) {
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
        if (comptime trace_cache) {
            std.debug.print("[PKG DEINIT] {s}: deiniting {} modules\n", .{ self.name, self.modules.items.len });
        }
        for (self.modules.items, 0..) |*mod, i| {
            if (comptime trace_cache) {
                std.debug.print("[PKG DEINIT] {s}: deinit module {} ({s})\n", .{ self.name, i, mod.name });
            }
            mod.deinit(gpa, owns_module_data);
        }
        self.modules.deinit(gpa);
        if (comptime trace_cache) {
            std.debug.print("[PKG DEINIT] {s}: modules done, deiniting names\n", .{self.name});
        }
        self.module_names.deinit();

        if (comptime trace_cache) {
            std.debug.print("[PKG DEINIT] {s}: names done, deiniting shorthands\n", .{self.name});
        }
        var sh_it = self.shorthands.iterator();
        while (sh_it.next()) |entry| {
            gpa.free(entry.key_ptr.*);
            gpa.free(entry.value_ptr.*);
        }
        self.shorthands.deinit();

        if (comptime trace_cache) {
            std.debug.print("[PKG DEINIT] {s}: freeing name and root_dir\n", .{self.name});
        }
        gpa.free(self.name);
        gpa.free(self.root_dir);
        if (comptime trace_cache) {
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

    /// All packages in the workspace
    packages: std.StringHashMap(*PackageState),

    /// Result channel from workers to coordinator
    result_channel: Channel(WorkerResult),

    /// Task queue for workers (protected by mutex for multi-threaded)
    task_queue: std.ArrayList(WorkerTask),
    task_mutex: Mutex,

    /// Worker threads
    workers: std.ArrayList(Thread),

    /// Whether workers should continue running
    running: bool,

    /// Number of tasks currently being processed by workers
    inflight: usize,

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

    pub fn init(
        gpa: Allocator,
        mode: Mode,
        max_threads: usize,
        builtin_modules: *const BuiltinModules,
        compiler_version: []const u8,
        cache_manager: ?*CacheManager,
    ) !Coordinator {
        return .{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .packages = std.StringHashMap(*PackageState).init(gpa),
            .result_channel = try Channel(WorkerResult).init(gpa, channel.DEFAULT_CAPACITY),
            .task_queue = std.ArrayList(WorkerTask).empty,
            .task_mutex = .{},
            .workers = std.ArrayList(Thread).empty,
            .running = false,
            .inflight = 0,
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
        if (comptime trace_cache) {
            std.debug.print("[COORD DEINIT] starting shutdown...\n", .{});
        }
        // Stop workers
        self.shutdown();

        if (comptime trace_cache) {
            std.debug.print("[COORD DEINIT] shutdown done, freeing packages...\n", .{});
        }

        // Free packages
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (comptime trace_cache) {
                std.debug.print("[COORD DEINIT] deinit package {s}\n", .{entry.key_ptr.*});
            }
            entry.value_ptr.*.deinit(self.gpa, self.owns_module_data);
            if (comptime trace_cache) {
                std.debug.print("[COORD DEINIT] package deinit done, now destroying\n", .{});
            }
            self.gpa.destroy(entry.value_ptr.*);
            if (comptime trace_cache) {
                std.debug.print("[COORD DEINIT] package destroyed\n", .{});
            }
        }
        if (comptime trace_cache) {
            std.debug.print("[COORD DEINIT] all packages done, calling packages.deinit()\n", .{});
        }
        self.packages.deinit();

        if (comptime trace_cache) {
            std.debug.print("[COORD DEINIT] packages done\n", .{});
        }

        // Free remaining tasks
        for (self.task_queue.items) |*task| {
            _ = task; // Tasks don't own memory, just references
        }
        self.task_queue.deinit(self.gpa);

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
        if (comptime trace_cache) {
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
    /// Returns module_allocator if set, otherwise gpa.
    pub fn getModuleAllocator(self: *Coordinator) std.mem.Allocator {
        return self.module_allocator orelse self.gpa;
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

        self.running = true;
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

        // Close the result channel to wake any blocked workers
        self.result_channel.close();

        // Set running = false and send shutdown tasks while holding the lock
        self.task_mutex.lock();
        self.running = false;
        for (self.workers.items) |_| {
            self.task_queue.append(self.gpa, .{ .shutdown = {} }) catch {};
        }
        self.task_mutex.unlock();

        // Wait for workers to finish
        for (self.workers.items) |w| {
            w.join();
        }
        self.workers.clearRetainingCapacity();
    }

    /// Enqueue a task for processing
    pub fn enqueueTask(self: *Coordinator, task: WorkerTask) !void {
        if (threads_available and self.mode == .multi_threaded) {
            self.task_mutex.lock();
            defer self.task_mutex.unlock();
        }
        if (comptime trace_cache) {
            switch (task) {
                .parse => |t| std.debug.print("[COORD] ENQUEUE parse: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .canonicalize => |t| std.debug.print("[COORD] ENQUEUE canonicalize: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .type_check => |t| std.debug.print("[COORD] ENQUEUE type_check: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .shutdown => std.debug.print("[COORD] ENQUEUE shutdown\n", .{}),
            }
        }
        try self.task_queue.append(self.gpa, task);
    }

    /// Enqueue a parse task for a module
    pub fn enqueueParseTask(self: *Coordinator, pkg_name: []const u8, module_id: ModuleId) !void {
        const pkg = self.packages.get(pkg_name) orelse return;
        const mod = pkg.getModule(module_id) orelse return;

        // Mark as Parsing to prevent double-enqueue
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
                if (self.task_queue.pop()) |task| {
                    const result = self.executeTaskInline(task);
                    try self.handleResult(result);
                    made_progress = true;
                }
            } else {
                // Multi-threaded: receive from workers via channel
                // Use blocking recv with timeout to avoid busy spinning
                if (self.result_channel.recvTimeout(10_000_000)) |result| { // 10ms timeout
                    self.inflight -= 1;
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
                    std.debug.print("Coordinator stuck: remaining={}, tasks={}, inflight={}\n", .{
                        self.total_remaining,
                        self.task_queue.items.len,
                        self.inflight,
                    });
                    // Print package/module states
                    var pkg_it = self.packages.iterator();
                    while (pkg_it.next()) |entry| {
                        const pkg = entry.value_ptr.*;
                        std.debug.print("  Package {s}: remaining={}, modules={}\n", .{
                            pkg.name,
                            pkg.remaining_modules,
                            pkg.modules.items.len,
                        });
                        for (pkg.modules.items, 0..) |mod, i| {
                            std.debug.print("    Module {}: {s} phase={} ext_imports={}\n", .{
                                i,
                                mod.name,
                                mod.phase,
                                mod.external_imports.items.len,
                            });
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

    /// Check if all work is complete
    pub fn isComplete(self: *Coordinator) bool {
        return self.total_remaining == 0 and self.task_queue.items.len == 0 and self.inflight == 0;
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
        defer res.deinit(self.gpa);

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
        if (comptime trace_cache) {
            std.debug.print("[COORD] PARSED: pkg={s} module={s} result_reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        if (comptime trace_cache) {
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

        if (comptime trace_cache) {
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
        if (comptime trace_cache) {
            std.debug.print("[COORD] CANONICALIZED: pkg={s} module={s} local_imports={} ext_imports={} result_reports={}\n", .{
                result.package_name,
                result.module_name,
                result.discovered_local_imports.items.len,
                result.discovered_external_imports.items.len,
                result.reports.items.len,
            });
        }
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        if (comptime trace_cache) {
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
                    _ = can.HostedCompiler.replaceAnnoOnlyWithHosted(env) catch {};
                }
            }
        }

        // Append reports - we take ownership, so clear result.reports after copying
        // to prevent WorkerResult.deinit from freeing the shared memory
        for (result.reports.items, 0..) |rep, ri| {
            if (comptime trace_cache) {
                std.debug.print("[COORD] CANONICALIZED: result report {}: owned_strings.len={}\n", .{ ri, rep.owned_strings.items.len });
                if (rep.owned_strings.items.len > 0) {
                    std.debug.print("[COORD] CANONICALIZED: first owned_string ptr={} len={}\n", .{ @intFromPtr(rep.owned_strings.items[0].ptr), rep.owned_strings.items[0].len });
                }
            }
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_cache) {
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
        mod.visit_color = 1;

        // Process discovered local imports
        // NOTE: We must refresh the mod pointer after each ensureModule call because
        // ensureModule can resize the modules array, invalidating pointers.
        for (result.discovered_local_imports.items) |imp| {
            const child_id = try pkg.ensureModule(self.gpa, imp.module_name, imp.path);
            // Refresh mod pointer after potential resize
            const current_mod = pkg.getModule(result.module_id) orelse return;
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
            if (child.visit_color == 1 or child_id == result.module_id) {
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
        const mod_after_imports = pkg.getModule(result.module_id) orelse return;

        // Process discovered external imports
        for (result.discovered_external_imports.items) |ext_imp| {
            try mod_after_imports.external_imports.append(self.gpa, try self.gpa.dupe(u8, ext_imp.import_name));
            try self.scheduleExternalImport(result.package_name, ext_imp.import_name);

            // Register this module as a cross-package dependent of the target
            const dot_idx = std.mem.indexOfScalar(u8, ext_imp.import_name, '.') orelse continue;
            const qual = ext_imp.import_name[0..dot_idx];
            const rest = ext_imp.import_name[dot_idx + 1 ..];

            const target_pkg_name = pkg.shorthands.get(qual) orelse continue;
            const target_pkg = self.packages.get(target_pkg_name) orelse continue;
            const target_module_id = target_pkg.module_names.get(rest) orelse continue;

            try self.registerCrossPackageDependent(
                target_pkg_name,
                target_module_id,
                result.package_name,
                result.module_id,
            );
        }

        // Transition to WaitingOnImports
        mod_after_imports.phase = .WaitingOnImports;

        // Try to unblock immediately
        try self.tryUnblock(pkg, result.module_id);
    }

    /// Handle a successful type-check result
    fn handleTypeChecked(self: *Coordinator, result: *TypeCheckedResult) !void {
        if (comptime trace_cache) {
            std.debug.print("[COORD] TYPE_CHECKED: pkg={s} module={s} result_reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        if (comptime trace_cache) {
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

        if (comptime trace_cache) {
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
        mod.visit_color = 2; // Black
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
                    if (std.mem.indexOfScalar(u8, ext_name, '.')) |dot_idx| {
                        const pkg_shorthand = ext_name[0..dot_idx];
                        const mod_name_part = ext_name[dot_idx + 1 ..];

                        // Resolve the external package and compute source hash by reading file
                        var imp_source_hash: [32]u8 = std.mem.zeroes([32]u8);
                        if (pkg.shorthands.get(pkg_shorthand)) |ext_pkg_name| {
                            if (self.packages.get(ext_pkg_name)) |ext_pkg| {
                                const imp_path = self.resolveModulePath(ext_pkg.root_dir, mod_name_part) catch null;
                                if (imp_path) |path| {
                                    defer self.gpa.free(path);
                                    if (self.file_provider.read(self.file_provider.ctx, path, self.gpa) catch null) |source| {
                                        defer self.gpa.free(source);
                                        imp_source_hash = CacheManager.computeSourceHash(source);
                                    }
                                }
                            }
                        }

                        const pkg_part = self.gpa.dupe(u8, pkg_shorthand) catch continue;
                        const mod_part = self.gpa.dupe(u8, mod_name_part) catch {
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
        if (comptime trace_cache) {
            std.debug.print("[COORD] PARSE FAILED: pkg={s} module={s} reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

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
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

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
        if (comptime trace_cache) {
            std.debug.print("[COORD] CACHE HIT (fast path): pkg={s} module={s}\n", .{
                result.package_name,
                result.module_name,
            });
        }

        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        // Store cache buffer to keep it alive for the lifetime of module_env
        // It will be freed when the coordinator is deinitialized
        try self.cache_buffers.append(self.gpa, result.cache_data);

        // Set module from cache
        mod.env = result.module_env;
        mod.was_cache_hit = true; // Mark as cache hit for dependent checks
        mod.phase = .Done;
        mod.visit_color = 2; // Black

        // Update cache stats
        self.cache_hits += 1;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake dependents (they may now be able to use fast path too)
        for (mod.dependents.items) |dep_id| {
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
                    if (comptime trace_cache) {
                        std.debug.print("[COORD] checkAllImportsCached: source pkg {s} not found\n", .{source_pkg_name});
                    }
                    return false;
                };
                break :blk source_pkg.shorthands.get(imp.package) orelse {
                    if (comptime trace_cache) {
                        std.debug.print("[COORD] checkAllImportsCached: shorthand {s} not found in {s}\n", .{ imp.package, source_pkg_name });
                    }
                    return false;
                };
            };

            // Look up the package to get its root directory
            const pkg = self.packages.get(pkg_name) orelse {
                if (comptime trace_cache) {
                    std.debug.print("[COORD] checkAllImportsCached: pkg {s} not found\n", .{pkg_name});
                }
                return false;
            };

            // Resolve the import's file path
            const module_path = self.resolveModulePath(pkg.root_dir, imp.module) catch {
                if (comptime trace_cache) {
                    std.debug.print("[COORD] checkAllImportsCached: failed to resolve path for {s}.{s}\n", .{ pkg_name, imp.module });
                }
                return false;
            };
            defer self.gpa.free(module_path);

            // Read the source file and compute its current hash
            const source = self.file_provider.read(self.file_provider.ctx, module_path, self.gpa) catch {
                if (comptime trace_cache) {
                    std.debug.print("[COORD] checkAllImportsCached: failed to read {s}\n", .{module_path});
                }
                return false;
            } orelse {
                if (comptime trace_cache) {
                    std.debug.print("[COORD] checkAllImportsCached: file not found {s}\n", .{module_path});
                }
                return false;
            };
            defer self.gpa.free(source);

            // Compute current source hash and compare with stored hash
            const current_hash = CacheManager.computeSourceHash(source);
            if (!std.mem.eql(u8, &current_hash, &imp.source_hash)) {
                // Dependency has changed since we cached
                if (comptime trace_cache) {
                    std.debug.print("[COORD] checkAllImportsCached: {s}.{s} hash mismatch (file changed)\n", .{ pkg_name, imp.module });
                }
                return false;
            }

            if (comptime trace_cache) {
                std.debug.print("[COORD] checkAllImportsCached: {s}.{s} hash matches\n", .{ pkg_name, imp.module });
            }
        }

        return true;
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
                if (comptime trace_cache) {
                    std.debug.print("[COORD] UNBLOCK WAIT: pkg={s} module={s} waiting on local import {}\n", .{ pkg.name, mod.name, imp_id });
                }
                return; // Not ready yet
            }
        }

        // Check external imports
        for (mod.external_imports.items) |ext_name| {
            if (!self.isExternalReady(pkg.name, ext_name)) {
                if (comptime trace_cache) {
                    std.debug.print("[COORD] UNBLOCK WAIT: pkg={s} module={s} waiting on external {s}\n", .{ pkg.name, mod.name, ext_name });
                }
                return;
            }
        }

        if (comptime trace_cache) {
            std.debug.print("[COORD] UNBLOCK: pkg={s} module={s} -> TypeCheck\n", .{ pkg.name, mod.name });
        }

        // All imports ready - transition to TypeCheck
        mod.phase = .TypeCheck;
        mod.visit_color = 2; // Black

        // Build imported_envs array
        var imported_envs = std.ArrayList(*ModuleEnv).empty;
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

        // Add external imports
        for (mod.external_imports.items) |ext_name| {
            if (self.getExternalEnv(pkg.name, ext_name)) |ext_env| {
                try imported_envs.append(self.gpa, ext_env);
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
        if (comptime trace_cache) {
            std.debug.print("[COORD] SCHEDULE EXT IMPORT: from {s} importing {s}\n", .{ source_pkg, import_name });
        }

        // Parse "pf.Stdout" -> qual="pf", rest="Stdout"
        const dot_idx = std.mem.indexOfScalar(u8, import_name, '.') orelse return;
        const qual = import_name[0..dot_idx];
        const rest = import_name[dot_idx + 1 ..];

        // Resolve shorthand to target package
        const source = self.packages.get(source_pkg) orelse {
            if (comptime trace_cache) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: source pkg {s} not found\n", .{source_pkg});
            }
            return;
        };
        const target_pkg_name = source.shorthands.get(qual) orelse {
            if (comptime trace_cache) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: shorthand {s} not found in {s}\n", .{ qual, source_pkg });
            }
            return;
        };

        // Get or create module in target package
        const target_pkg = self.packages.get(target_pkg_name) orelse {
            if (comptime trace_cache) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: target pkg {s} not found\n", .{target_pkg_name});
            }
            return;
        };
        const path = try self.resolveModulePath(target_pkg.root_dir, rest);
        defer self.gpa.free(path);

        const module_id = try target_pkg.ensureModule(self.gpa, rest, path);
        const mod = target_pkg.getModule(module_id).?;

        if (comptime trace_cache) {
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
        if (comptime trace_cache) {
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
            if (comptime trace_cache) {
                std.debug.print("[COORD] WAKE CROSS-PKG: {s}:{} has no dependents\n", .{ pkg_name, module_id });
            }
            return;
        };

        if (comptime trace_cache) {
            std.debug.print("[COORD] WAKE CROSS-PKG: {s}:{} waking {} dependents\n", .{ pkg_name, module_id, dependents.items.len });
        }

        for (dependents.items) |dep_ref| {
            const dep_pkg = self.packages.get(dep_ref.pkg_name) orelse continue;
            try self.tryUnblock(dep_pkg, dep_ref.module_id);
        }
    }

    /// Check if an external import is ready
    pub fn isExternalReady(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) bool {
        const dot_idx = std.mem.indexOfScalar(u8, import_name, '.') orelse return false;
        const qual = import_name[0..dot_idx];
        const rest = import_name[dot_idx + 1 ..];

        const source = self.packages.get(source_pkg) orelse return false;
        const target_pkg_name = source.shorthands.get(qual) orelse return false;
        const target_pkg = self.packages.get(target_pkg_name) orelse return false;

        return target_pkg.getEnvIfDone(rest) != null;
    }

    /// Get the ModuleEnv for an external import
    pub fn getExternalEnv(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) ?*ModuleEnv {
        const dot_idx = std.mem.indexOfScalar(u8, import_name, '.') orelse return null;
        const qual = import_name[0..dot_idx];
        const rest = import_name[dot_idx + 1 ..];

        const source = self.packages.get(source_pkg) orelse return null;
        const target_pkg_name = source.shorthands.get(qual) orelse return null;
        const target_pkg = self.packages.get(target_pkg_name) orelse return null;

        return target_pkg.getEnvIfDone(rest);
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
        var buffer = std.ArrayList(u8).empty;
        defer buffer.deinit(self.gpa);

        var it = std.mem.splitScalar(u8, mod_name, '.');
        var first = true;
        while (it.next()) |part| {
            if (!first) try buffer.appendSlice(self.gpa, std.fs.path.sep_str) else first = false;
            try buffer.appendSlice(self.gpa, part);
        }
        try buffer.appendSlice(self.gpa, ".roc");

        const rel = try buffer.toOwnedSlice(self.gpa);
        defer self.gpa.free(rel);

        return try std.fs.path.join(self.gpa, &.{ root_dir, rel });
    }

    /// Execute a parse task (pure function)
    fn executeParse(self: *Coordinator, task: ParseTask) WorkerResult {
        const start_time = if (threads_available) std.time.nanoTimestamp() else 0;

        // Read source
        const src = self.readModuleSource(task.path) catch |err| {
            var reports = std.ArrayList(Report).empty;
            var rep = Report.init(self.gpa, "FILE NOT FOUND", .fatal);
            // Include the path in the error message for debugging
            const path_msg = std.fmt.allocPrint(self.gpa, "{s}: {s}", .{ task.path, @errorName(err) }) catch @errorName(err);
            defer if (path_msg.ptr != @errorName(err).ptr) self.gpa.free(path_msg);
            rep.addErrorMessage(path_msg) catch {};
            reports.append(self.gpa, rep) catch {};

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

        // FAST PATH: Check cache before parsing
        if (self.cache_manager) |cache| {
            // Compute source hash for metadata lookup
            const source_hash = CacheManager.computeSourceHash(src);

            // Look up metadata by source hash
            if (cache.getMetadata(source_hash)) |metadata| {
                defer {
                    var meta = metadata;
                    meta.deinit(self.gpa);
                }

                // Check if ALL imports were cache hits in this build
                const all_deps_cached = self.checkAllImportsCached(task.package_name, metadata.imports);

                if (all_deps_cached) {
                    // Try to load from full cache using the key from metadata
                    const cache_result = cache.loadFromCacheByKey(
                        metadata.full_cache_key,
                        src,
                        task.module_name,
                    );

                    if (cache_result == .hit) {
                        if (comptime trace_cache) {
                            std.debug.print("[COORD] FAST PATH HIT: pkg={s} module={s}\n", .{
                                task.package_name,
                                task.module_name,
                            });
                        }

                        return .{
                            .cache_hit = .{
                                .package_name = task.package_name,
                                .module_id = task.module_id,
                                .module_name = task.module_name,
                                .path = task.path,
                                .module_env = cache_result.hit.module_env,
                                .source = src,
                                .error_count = cache_result.hit.error_count,
                                .warning_count = cache_result.hit.warning_count,
                                .cache_data = cache_result.hit.cache_data,
                            },
                        };
                    }
                }

                if (comptime trace_cache) {
                    std.debug.print("[COORD] FAST PATH MISS (deps not cached): pkg={s} module={s}\n", .{
                        task.package_name,
                        task.module_name,
                    });
                }
            } else {
                if (comptime trace_cache) {
                    std.debug.print("[COORD] FAST PATH MISS (no metadata): pkg={s} module={s}\n", .{
                        task.package_name,
                        task.module_name,
                    });
                }
            }
        }

        // SLOW PATH: Parse, canonicalize, type-check

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
        env.common.calcLineStarts(module_alloc) catch {};

        // Parse
        var reports = std.ArrayList(Report).empty;
        var parse_ast = parse.parse(&env.common, self.gpa) catch {
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

        // Cache AST
        const ast_ptr = self.gpa.create(AST) catch {
            parse_ast.deinit(self.gpa);
            return .{
                .parse_failed = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .reports = reports,
                    .partial_env = env,
                },
            };
        };
        ast_ptr.* = parse_ast;

        // Collect parse diagnostics
        for (ast_ptr.tokenize_diagnostics.items) |diagnostic| {
            const rep = ast_ptr.tokenizeDiagnosticToReport(diagnostic, self.gpa, task.path) catch continue;
            reports.append(self.gpa, rep) catch {};
        }
        for (ast_ptr.parse_diagnostics.items) |diagnostic| {
            const rep = ast_ptr.parseDiagnosticToReport(&env.common, diagnostic, self.gpa, task.path) catch continue;
            reports.append(self.gpa, rep) catch {};
        }

        const end_time = if (threads_available) std.time.nanoTimestamp() else 0;

        return .{
            .parsed = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .module_env = env,
                .cached_ast = ast_ptr,
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
        const qualified_imports = module_discovery.extractQualifiedImportsFromAST(ast, self.gpa) catch &[_][]const u8{};
        defer {
            for (qualified_imports) |qi| self.gpa.free(qi);
            self.gpa.free(qualified_imports);
        }

        // Build KnownModule entries for qualified imports so they get placeholders
        var known_modules = std.ArrayList(compile_package.PackageEnv.KnownModule).empty;
        defer known_modules.deinit(self.gpa);
        for (qualified_imports) |qi| {
            known_modules.append(self.gpa, .{
                .qualified_name = qi,
                .import_name = qi,
            }) catch {};
        }

        // Canonicalize using the PackageEnv shared function with sibling awareness
        // This sets up placeholders for external imports that will be resolved during type-checking
        compile_package.PackageEnv.canonicalizeModuleWithSiblings(
            self.gpa,
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
        var reports = std.ArrayList(Report).empty;
        const diags = env.getDiagnostics() catch &[_]CIR.Diagnostic{};
        // Free with env.gpa since that's what getDiagnostics uses for allocation
        // (In IPC mode, this is a no-op since SharedMemoryAllocator.free does nothing)
        defer env.gpa.free(diags);
        for (diags) |d| {
            const rep = env.diagnosticToReport(d, self.gpa, task.path) catch continue;
            reports.append(self.gpa, rep) catch {};
        }
        const diag_end = if (threads_available) std.time.nanoTimestamp() else 0;

        // Discover imports from env.imports
        var local_imports = std.ArrayList(DiscoveredLocalImport).empty;
        var external_imports = std.ArrayList(DiscoveredExternalImport).empty;

        const import_count = env.imports.imports.items.items.len;
        for (env.imports.imports.items.items[0..import_count]) |str_idx| {
            const mod_name = env.getString(str_idx);

            if (std.mem.eql(u8, mod_name, "Builtin")) continue;

            // Check if qualified (external) import
            if (std.mem.indexOfScalar(u8, mod_name, '.') != null) {
                external_imports.append(self.gpa, .{
                    .import_name = self.gpa.dupe(u8, mod_name) catch continue,
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

                const path = self.resolveModulePath(task.root_dir, mod_name) catch continue;
                local_imports.append(self.gpa, .{
                    .module_name = self.gpa.dupe(u8, mod_name) catch {
                        self.gpa.free(path);
                        continue;
                    },
                    .path = path,
                }) catch {
                    self.gpa.free(path);
                };
            }
        }

        // Free AST
        ast.deinit(self.gpa);
        self.gpa.destroy(ast);

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

        // Type check
        var checker = compile_package.PackageEnv.typeCheckModule(
            self.gpa,
            env,
            self.builtin_modules.builtin_module.env,
            task.imported_envs,
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
        var reports = std.ArrayList(Report).empty;

        const check = @import("check");
        var rb = check.ReportBuilder.init(
            self.gpa,
            env,
            env,
            &checker.snapshots,
            &checker.problems,
            task.path,
            task.imported_envs,
            &checker.import_mapping,
        );
        defer rb.deinit();

        for (checker.problems.problems.items) |prob| {
            const rep = rb.build(prob) catch continue;
            reports.append(self.gpa, rep) catch {};
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
        while (true) {
            // Get next task
            var task: ?WorkerTask = null;

            if (threads_available) {
                self.task_mutex.lock();
                if (self.task_queue.items.len > 0) {
                    task = self.task_queue.pop();
                    self.inflight += 1;
                }
                const running = self.running;
                self.task_mutex.unlock();

                if (task == null and !running) break;
                if (task == null) {
                    // Wait for work or shutdown
                    std.Thread.sleep(1_000_000); // 1ms
                    continue;
                }
            }

            const t = task.?;
            if (t == .shutdown) break;

            // Execute task
            const result = self.executeTaskInline(t);

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
        undefined, // builtin_modules - not used in this test
        "test",
        null, // cache_manager
    );
    defer coord.deinit();

    try std.testing.expect(coord.total_remaining == 0);
    try std.testing.expect(coord.task_queue.items.len == 0);
    try std.testing.expect(coord.isComplete());
}

test "Coordinator package creation" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
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

    try std.testing.expectEqual(@as(usize, 1), coord.task_queue.items.len);

    // Pop the task
    const task = coord.task_queue.pop();
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
    _ = coord.task_queue.pop();
    coord.inflight = 1;
    try std.testing.expect(!coord.isComplete());

    // All clear - should be complete
    coord.inflight = 0;
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
    try std.testing.expectEqual(@as(usize, 1), coord.task_queue.items.len);

    // Verify it's a parse task for the right module
    const task = coord.task_queue.items[0];
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

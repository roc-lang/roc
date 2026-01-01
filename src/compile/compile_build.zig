//! The state for an in-progress build. ("Builds" include `roc check` as well as `roc build`.)
//!
//! Modules are built in parallel unless targeting WebAssembly, which doesn't support threads.
//!
//! Errors are reported as soon as they're encountered, with the only exception being that
//! there is some buffering to make their output order determined by the dependency graph
//! rather than by parallelism races. In other words, if you build the same set of source files
//! repeatedly, you should always see the same errors be reported in the same order - but you
//! shouldn't have to wait until the end of the build to start seeing them appear.

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const can = @import("can");
const builtin = @import("builtin");
const build_options = @import("build_options");
const reporting = @import("reporting");
const eval = @import("eval");
const check = @import("check");
const unbundle = @import("unbundle");

const Report = reporting.Report;
const ReportBuilder = check.ReportBuilder;
const BuiltinModules = eval.BuiltinModules;
const Mode = @import("compile_package.zig").Mode;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const PackageEnv = @import("compile_package.zig").PackageEnv;
const ModuleTimingInfo = @import("compile_package.zig").TimingInfo;
const ImportResolver = @import("compile_package.zig").ImportResolver;
const ScheduleHook = @import("compile_package.zig").ScheduleHook;
const CacheManager = @import("cache_manager.zig").CacheManager;
const FileProvider = @import("compile_package.zig").FileProvider;

// Threading features aren't available when targeting WebAssembly,
// so we disable them at comptime to prevent builds from failing.
const threads_available = builtin.target.cpu.arch != .wasm32;

const Thread = if (threads_available) std.Thread else struct {};
const Mutex = if (threads_available) std.Thread.Mutex else struct {};
const ThreadCondition = if (threads_available) std.Thread.Condition else struct {};

// Inflight counter: atomic usize on non-wasm; no-op struct on wasm
const InflightCounter = if (threads_available) struct {
    value: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),

    pub fn inc(self: *@This()) void {
        _ = self.value.fetchAdd(1, .monotonic);
    }

    pub fn dec(self: *@This()) void {
        _ = self.value.fetchSub(1, .monotonic);
    }

    pub fn load(self: *@This()) usize {
        return self.value.load(.monotonic);
    }
} else struct {};

fn freeSlice(gpa: Allocator, s: []u8) void {
    gpa.free(s);
}

fn freeConstSlice(gpa: Allocator, s: []const u8) void {
    gpa.free(@constCast(s));
}

// Global unified work-stealing queue

const GlobalQueue = struct {
    const Task = struct {
        pkg: []u8,
        module_name: []u8,
    };

    gpa: Allocator,
    tasks: std.array_list.Managed(Task),
    lock: Mutex = .{},
    cond: ThreadCondition = .{},
    workers: std.array_list.Managed(Thread),
    running: bool = false,
    sink_ptr: ?*OrderedSink = null,
    // Pointer back to BuildEnv for dispatch
    build_env: ?*BuildEnv = null,
    // Inflight counter for idle detection (no-op on wasm)
    inflight: InflightCounter = .{},

    pub fn init(gpa: Allocator) GlobalQueue {
        return .{
            .gpa = gpa,
            .tasks = std.array_list.Managed(Task).init(gpa),
            .workers = std.array_list.Managed(std.Thread).init(gpa),
        };
    }

    pub fn deinit(self: *GlobalQueue, gpa: Allocator) void {
        // Stop workers
        self.lock.lock();
        self.running = false;
        self.cond.broadcast();
        self.lock.unlock();

        if (threads_available) {
            for (self.workers.items) |t| t.join();
        }
        self.workers.deinit();

        // Free queued tasks
        for (self.tasks.items) |t| {
            freeSlice(gpa, t.pkg);
            freeSlice(gpa, t.module_name);
        }
        self.tasks.deinit();

        self.sink_ptr = null;
        self.build_env = null;
    }

    pub fn start(self: *GlobalQueue, _: Allocator, max_threads: usize, sink: *OrderedSink) !void {
        self.lock.lock();
        self.sink_ptr = sink;
        // On targets without threads (e.g. wasm32), do nothing.
        if (builtin.target.cpu.arch == .wasm32) {
            self.running = false;
            self.lock.unlock();
            return;
        }
        // Respect requested single-thread mode without spawning threads
        if (max_threads <= 1) {
            self.running = false;
            self.lock.unlock();
            return;
        }
        self.running = true;
        self.lock.unlock();

        const n = if (max_threads == 0) (std.Thread.getCpuCount() catch 1) else max_threads;
        try self.workers.ensureTotalCapacity(n);
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const th = try std.Thread.spawn(.{}, worker, .{self});
            try self.workers.append(th);
        }
    }

    pub fn waitForIdle(self: *GlobalQueue) void {
        // Wait until queue empty and no inflight work (non-wasm).
        // BuildEnv.emitDeterministic() will be called after this.
        while (true) {
            if (threads_available) {
                self.lock.lock();
            }
            const no_tasks = self.tasks.items.len == 0;
            if (threads_available) {
                self.lock.unlock();
            }
            const inflight_zero = if (threads_available) self.inflight.load() == 0 else true;
            if (no_tasks and inflight_zero) break;
        }
    }

    pub fn enqueue(self: *GlobalQueue, pkg: []const u8, module_name: []const u8) !void {
        self.lock.lock();
        defer self.lock.unlock();

        try self.tasks.append(.{
            .pkg = try self.gpa.dupe(u8, pkg),
            .module_name = try self.gpa.dupe(u8, module_name),
        });
        self.cond.signal();
    }

    fn take(self: *GlobalQueue) ?Task {
        if (threads_available) {
            self.lock.lock();
            defer self.lock.unlock();
        }
        if (self.tasks.items.len == 0) return null;
        const idx = self.tasks.items.len - 1;
        const t = self.tasks.items[idx];
        self.tasks.items.len = idx;
        return t;
    }

    fn worker(self: *GlobalQueue) void {
        while (true) {
            if (self.take()) |task| {
                // Dispatch to the corresponding ModuleBuild
                if (self.build_env) |be| {
                    if (be.schedulers.get(task.pkg)) |sched| {
                        // Process module task if present in scheduler
                        const exists = sched.hasModule(task.module_name);
                        if (exists) {
                            // Mark inflight before processing and decrement after
                            if (threads_available) {
                                self.inflight.inc();
                                defer self.inflight.dec();
                            }

                            // Check cache before processing
                            if (be.cache_manager) |cm| {
                                // Get module state to check path
                                const module_state = sched.getModuleState(task.module_name).?;

                                // Read the source file (normalize line endings for consistent behavior on Windows).
                                const source = be.readFile(module_state.path, 10 * 1024 * 1024) catch {
                                    // If we can't read the file, continue with normal processing
                                    sched.processModuleByName(task.module_name) catch {
                                        // Continue processing other modules despite this error
                                    };

                                    freeSlice(self.gpa, task.pkg);
                                    freeSlice(self.gpa, task.module_name);
                                    continue;
                                };
                                defer be.gpa.free(source);

                                const cache_result = cm.loadFromCache("roc-zig-dev", source, task.module_name);
                                switch (cache_result) {
                                    .hit => |hit| {
                                        // Cache hit! Update the module state with cached data
                                        module_state.*.phase = .Done;
                                        module_state.*.env = hit.module_env.*;

                                        // Skip normal processing since we loaded from cache
                                        freeSlice(self.gpa, task.pkg);
                                        freeSlice(self.gpa, task.module_name);
                                        continue;
                                    },
                                    .miss => {
                                        // Continue with normal processing
                                    },
                                    .not_enabled => {
                                        // Continue with normal processing
                                    },
                                }
                            }

                            sched.processModuleByName(task.module_name) catch {
                                // Continue processing other modules despite this error
                            };

                            // After successful processing, store in cache
                            if (be.cache_manager) |cm| {
                                const module_state = sched.getModuleState(task.module_name).?;
                                if (module_state.phase == .Done and module_state.env != null) {
                                    // Read the source file again to generate the cache key
                                    const source = be.readFile(module_state.path, 10 * 1024 * 1024) catch {
                                        // If we can't read the file, skip caching
                                        freeSlice(self.gpa, task.pkg);
                                        freeSlice(self.gpa, task.module_name);
                                        continue;
                                    };
                                    defer be.gpa.free(source);

                                    const cache_key = CacheManager.generateCacheKey(source, "roc-zig-dev");
                                    // For now, just pass 0 for error and warning counts
                                    // TODO: Extract actual error/warning counts from reports
                                    const error_count: u32 = 0;
                                    const warning_count: u32 = 0;

                                    cm.store(
                                        cache_key,
                                        &module_state.env.?,
                                        error_count,
                                        warning_count,
                                    ) catch {
                                        // Cache store failed, but continue
                                    };
                                }
                            }
                        }
                    }
                }
                freeSlice(self.gpa, task.pkg);
                freeSlice(self.gpa, task.module_name);
                continue;
            }

            const keep_running = if (threads_available) blk: {
                self.lock.lock();
                while (self.tasks.items.len == 0 and self.running) {
                    self.cond.wait(&self.lock);
                }
                const running = self.running;
                self.lock.unlock();
                break :blk running;
            } else self.running;

            if (!keep_running) break;
        }
    }

    // Hook from ModuleBuild to enqueue newly discovered/scheduled modules
    pub fn hookOnSchedule(ctx: ?*anyopaque, package_name: []const u8, module_name: []const u8, _: []const u8, _: u32) void {
        var self: *GlobalQueue = @ptrCast(@alignCast(ctx.?));
        // Enqueue to global queue - log but don't fail on error
        self.enqueue(package_name, module_name) catch {
            // Continue anyway - the module will still be processed by local scheduler
        };
    }
};

// Rooted path + normalization helper
const PathUtils = struct {
    fn normalizeAndJoin(gpa: Allocator, root: []const u8, rel: []const u8) ![]const u8 {
        const joined = try std.fs.path.join(gpa, &.{ root, rel });
        errdefer gpa.free(joined);
        // Resolve .. and . components
        const canon = try std.fs.path.resolve(gpa, &.{joined});
        gpa.free(joined);
        return canon;
    }

    fn makeAbsolute(gpa: Allocator, base_dir: []const u8, path: []const u8) ![]const u8 {
        if (std.fs.path.isAbsolute(path)) {
            return try std.fs.path.resolve(gpa, &.{path});
        } else {
            return try normalizeAndJoin(gpa, base_dir, path);
        }
    }

    fn isWithinRoot(candidate: []const u8, roots: []const []const u8) bool {
        for (roots) |root| {
            if (std.mem.startsWith(u8, candidate, root)) return true;
        }
        return false;
    }
};

// BuildEnv: workspace-level orchestrator for multi-package builds with local-only shorthands.
//
// Responsibilities:
// - Parse headers of app/package/platform modules (local paths only)
// - Build a package graph with shorthand alias maps
// - Enforce package rules: only apps may depend on platforms; no package may depend on apps
// - Create per-package ModuleBuild schedulers and wire a shared resolver
// - Aggregate reports deterministically across the workspace (depth then module name)
// - Keep ModuleEnv hermetic: imports remain strings; cross-package resolution happens via resolver at type-check time
//
// This module is designed to be integrated at the compile layer to avoid base<->compile circular dependencies.
/// Main workspace-level build orchestrator that coordinates multiple packages
pub const BuildEnv = struct {
    gpa: Allocator,
    mode: Mode,
    max_threads: usize,
    compiler_version: []const u8 = build_options.compiler_version,

    // Workspace roots for sandboxing (absolute, canonical)
    workspace_roots: std.array_list.Managed([]const u8),

    // Map of package name (alias) -> Package
    packages: std.StringHashMapUnmanaged(Package) = .{},
    // Schedulers per package name
    schedulers: std.StringHashMapUnmanaged(*PackageEnv) = .{},

    // Ordered sink over all packages (thread-safe, deterministic emission)
    sink: OrderedSink,

    // Unified global work-stealing queue (WSQ)
    global_queue: GlobalQueue,

    // Cache manager for compiled modules
    cache_manager: ?*CacheManager = null,
    // Optional virtual file provider
    file_provider: ?FileProvider = null,

    // Builtin modules (Bool, Try, Str) shared across all packages (heap-allocated to prevent moves)
    builtin_modules: *BuiltinModules,

    // Owned resolver ctx pointers for cleanup (typed)
    resolver_ctxs: std.array_list.Managed(*ResolverCtx),
    // Owned per-package sink contexts for fully-qualified emission
    pkg_sink_ctxs: std.array_list.Managed(*PkgSinkCtx),
    // Owned schedule ctxs for pre-registration (one per package)
    schedule_ctxs: std.array_list.Managed(*ScheduleCtx),
    // Pending known module registrations (processed after schedulers are created)
    pending_known_modules: std.array_list.Managed(PendingKnownModule),

    /// Info about a known module registration that needs to be applied after schedulers exist
    const PendingKnownModule = struct {
        target_package: []const u8, // Package to register with (e.g., "app")
        qualified_name: []const u8, // e.g., "pf.Stdout"
        import_name: []const u8, // e.g., "pf.Stdout"
    };

    pub fn init(gpa: Allocator, mode: Mode, max_threads: usize) !BuildEnv {
        // Allocate builtin modules on heap to prevent moves that would invalidate internal pointers
        const builtin_modules = try gpa.create(BuiltinModules);
        errdefer gpa.destroy(builtin_modules);

        builtin_modules.* = try BuiltinModules.init(gpa);
        errdefer builtin_modules.deinit();

        return .{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .workspace_roots = std.array_list.Managed([]const u8).init(gpa),
            .sink = OrderedSink.init(gpa),
            .global_queue = GlobalQueue.init(gpa),
            .builtin_modules = builtin_modules,
            .resolver_ctxs = std.array_list.Managed(*ResolverCtx).init(gpa),
            .pkg_sink_ctxs = std.array_list.Managed(*PkgSinkCtx).init(gpa),
            .schedule_ctxs = std.array_list.Managed(*ScheduleCtx).init(gpa),
            .pending_known_modules = std.array_list.Managed(PendingKnownModule).init(gpa),
        };
    }

    pub fn deinit(self: *BuildEnv) void {
        // Deinit and free builtin modules
        self.builtin_modules.deinit();
        self.gpa.destroy(self.builtin_modules);

        // Stop global queue workers
        self.global_queue.deinit(self.gpa);

        // Deinit cache manager if present
        if (self.cache_manager) |cm| {
            self.gpa.destroy(cm);
        }

        // Free resolver ctxs owned by this BuildEnv (if any)
        for (self.resolver_ctxs.items) |ctx_ptr| {
            self.gpa.destroy(ctx_ptr);
        }
        self.resolver_ctxs.deinit();
        // Free per-package sink contexts
        for (self.pkg_sink_ctxs.items) |p| self.gpa.destroy(p);
        self.pkg_sink_ctxs.deinit();

        // Free schedule ctxs
        for (self.schedule_ctxs.items) |p| self.gpa.destroy(p);
        self.schedule_ctxs.deinit();

        // Free pending known modules
        for (self.pending_known_modules.items) |pkm| {
            self.gpa.free(pkm.target_package);
            self.gpa.free(pkm.qualified_name);
            self.gpa.free(pkm.import_name);
        }
        self.pending_known_modules.deinit();

        // Deinit schedulers
        var sit = self.schedulers.iterator();
        while (sit.next()) |e| {
            const mb_ptr: *PackageEnv = e.value_ptr.*;
            mb_ptr.deinit();
            self.gpa.destroy(mb_ptr);
            freeConstSlice(self.gpa, e.key_ptr.*);
        }
        self.schedulers.deinit(self.gpa);

        // Deinit packages
        var pit = self.packages.iterator();
        while (pit.next()) |e| {
            var p = e.value_ptr.*;
            p.deinit(self.gpa);
            freeConstSlice(self.gpa, e.key_ptr.*);
        }
        self.packages.deinit(self.gpa);

        // Clear back-pointer
        self.global_queue.build_env = null;

        // Deinit roots and free the duplicated strings
        for (self.workspace_roots.items) |root| {
            self.gpa.free(root);
        }
        self.workspace_roots.deinit();

        self.sink.deinit();
    }

    /// Set the cache manager for this build environment
    pub fn setCacheManager(self: *BuildEnv, cache_manager: *CacheManager) void {
        self.cache_manager = cache_manager;
    }

    /// Set a virtual file provider for this BuildEnv.
    pub fn setFileProvider(self: *BuildEnv, provider: ?FileProvider) void {
        self.file_provider = provider;
    }

    /// Build an app file specifically (validates it's an app)
    pub fn buildApp(self: *BuildEnv, app_file: []const u8) !void {
        // Build and let the main function handle everything
        // The build function accepts both apps and modules
        try self.build(app_file);

        // After building, verify it was actually an app
        // Check the package we just created
        const pkg = self.packages.get("app");
        if (pkg == null or pkg.?.kind != .app) {
            // If it wasn't an app, return an error
            return error.NotAnApp;
        }
    }

    // Build the workspace starting from an app root file path.
    // Assumptions:
    // - All header-declared paths are local filesystem paths (no URLs).
    // - Shorthand aliases uniquely identify packages within this workspace.
    pub fn build(self: *BuildEnv, root_file: []const u8) !void {
        // Workspace root is directory containing the app header; normalize/sandbox
        const root_abs = try self.makeAbsolute(root_file);
        defer self.gpa.free(root_abs);
        const root_dir = if (std.fs.path.dirname(root_abs)) |d| try std.fs.path.resolve(self.gpa, &.{d}) else try self.gpa.dupe(u8, ".");
        defer self.gpa.free(root_dir);
        // NOTE: Do not sandbox the app header; the app may reference arbitrary paths.
        // We still record the root_dir for convenience in later checks.
        try self.workspace_roots.append(try self.gpa.dupe(u8, root_dir));

        var header_info = try self.parseHeaderDeps(root_abs);
        defer header_info.deinit(self.gpa);
        // Allow app, module, type_module, and default_app files
        const is_executable = header_info.kind == .app or header_info.kind == .default_app;

        if (!is_executable and header_info.kind != .module and header_info.kind != .type_module) {
            return error.UnsupportedHeader;
        }

        // Create package entry
        const pkg_name = if (is_executable) "app" else "module";
        const key_pkg = try self.gpa.dupe(u8, pkg_name);
        const pkg_root_file = try self.gpa.dupe(u8, root_abs);
        const pkg_root_dir = try self.gpa.dupe(u8, root_dir);

        try self.packages.put(self.gpa, key_pkg, .{
            .name = try self.gpa.dupe(u8, pkg_name),
            .kind = header_info.kind,
            .root_file = pkg_root_file,
            .root_dir = pkg_root_dir,
        });

        // Populate package graph and shorthand maps recursively (only for apps)
        if (header_info.kind == .app) {
            try self.populatePackageShorthands(pkg_name, &header_info);
        }

        // Create per-package schedulers wired with a shared resolver and global queue hook
        try self.createSchedulers();

        // Register pending known modules now that schedulers exist
        try self.processPendingKnownModules();

        // Set back-pointer for dispatch
        self.global_queue.build_env = self;

        // Start global queue workers with ordered sink only when threads are available and requested
        if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded) {
            try self.global_queue.start(self.gpa, self.max_threads, &self.sink);
        }

        // Build platform and other dependency packages BEFORE the app
        // This ensures platform module envs are available when app is canonicalized
        var it = self.schedulers.iterator();
        while (it.next()) |e| {
            const name = e.key_ptr.*;
            if (std.mem.eql(u8, name, pkg_name)) continue;
            const pkg = self.packages.get(name).?;
            try e.value_ptr.*.buildRoot(pkg.root_file);
        }

        // Seed root module into global queue via schedule hook (ModuleBuild will call back)
        const root_sched = self.schedulers.getPtr(pkg_name).?;
        try root_sched.*.buildRoot(pkg_root_file);

        // Wait for all work to complete
        if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded) {
            // Multi-threaded mode: wait for global queue to drain
            self.global_queue.waitForIdle();
        }
        // Give modules stuck on external imports another chance now that all packages are scheduled.
        try self.unblockExternalImports();
        if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded) {
            self.global_queue.waitForIdle();
        }
        // Note: In single-threaded mode, buildRoot() runs synchronously and blocks
        // until all modules are complete, so no additional waiting is needed.

        // Check platform requirements for app modules
        try self.checkPlatformRequirements();

        // Deterministic emission: globally order reports by (min dependency depth from app, then module name)
        try self.emitDeterministic();
    }

    /// Check that app exports match platform requirements.
    /// This is called after all modules are compiled and type-checked.
    fn checkPlatformRequirements(self: *BuildEnv) !void {
        // Find the app and platform packages
        var app_pkg: ?[]const u8 = null;
        var platform_pkg: ?[]const u8 = null;

        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            if (pkg.kind == .app) {
                app_pkg = entry.key_ptr.*;
            } else if (pkg.kind == .platform) {
                platform_pkg = entry.key_ptr.*;
            }
        }

        // If we don't have both an app and a platform, nothing to check
        const app_name = app_pkg orelse return;
        const platform_name = platform_pkg orelse return;

        // Get the schedulers for both packages
        const app_sched = self.schedulers.get(app_name) orelse return;
        const platform_sched = self.schedulers.get(platform_name) orelse return;

        // Get the root module envs for both packages
        const app_root_env = app_sched.getRootEnv() orelse return;
        const platform_root_env = platform_sched.getRootEnv() orelse return;

        // If the platform has no requires_types, nothing to check
        if (platform_root_env.requires_types.items.items.len == 0) {
            return;
        }

        // Get builtin indices and module
        const builtin_indices = self.builtin_modules.builtin_indices;
        const builtin_module_env = self.builtin_modules.builtin_module.env;

        // Build module_envs_map for type resolution
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(self.gpa);
        defer module_envs_map.deinit();

        // Use the shared populateModuleEnvs function to set up auto-imported types
        try Can.populateModuleEnvs(&module_envs_map, app_root_env, builtin_module_env, builtin_indices);

        // Build builtin context for the type checker
        const builtin_ctx = Check.BuiltinContext{
            .module_name = app_root_env.module_name_idx,
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = builtin_module_env,
            .builtin_indices = builtin_indices,
        };

        // Create type checker for the app module
        var checker = try Check.init(
            self.gpa,
            &app_root_env.types,
            app_root_env,
            &.{}, // No imported modules needed for checking exports
            &module_envs_map,
            &app_root_env.store.regions,
            builtin_ctx,
        );
        defer checker.deinit();

        // Build the platform-to-app ident translation map
        // This translates platform requirement idents to app idents by name
        var platform_to_app_idents = std.AutoHashMap(base.Ident.Idx, base.Ident.Idx).init(self.gpa);
        defer platform_to_app_idents.deinit();

        for (platform_root_env.requires_types.items.items) |required_type| {
            const platform_ident_text = platform_root_env.getIdent(required_type.ident);
            if (app_root_env.common.findIdent(platform_ident_text)) |app_ident| {
                try platform_to_app_idents.put(required_type.ident, app_ident);
            }
        }

        // Check platform requirements against app exports
        try checker.checkPlatformRequirements(platform_root_env, &platform_to_app_idents);

        // If there are type problems, convert them to reports and emit via sink
        if (checker.problems.problems.items.len > 0) {
            const app_root_module = app_sched.getRootModule() orelse return;

            var rb = ReportBuilder.init(
                self.gpa,
                app_root_env,
                app_root_env,
                &checker.snapshots,
                app_root_module.path,
                &.{},
                &checker.import_mapping,
            );
            defer rb.deinit();

            for (checker.problems.problems.items) |prob| {
                const rep = rb.build(prob) catch continue;
                // Emit via sink with the module name (not path) to match other reports
                self.sink.emitReport(app_name, app_root_module.name, rep);
            }
        }
    }

    fn unblockExternalImports(self: *BuildEnv) !void {
        var progress = true;
        while (progress) {
            progress = false;

            var sched_it = self.schedulers.iterator();
            while (sched_it.next()) |entry| {
                const sched = entry.value_ptr.*;

                var mod_it = sched.moduleNamesIterator();
                while (mod_it.next()) |m_entry| {
                    const module_name = m_entry.key_ptr.*;
                    const st = sched.getModuleState(module_name) orelse continue;
                    if (st.phase != .WaitingOnImports or st.external_imports.items.len == 0) continue;

                    const prev_remaining = sched.remaining_modules;
                    var last_phase = st.phase;

                    // Drive the module forward until it either finishes or stops making progress.
                    while (true) {
                        try sched.processModuleByName(module_name);
                        const updated = sched.getModuleState(module_name) orelse break;
                        if (updated.phase == .Done) {
                            progress = true;
                            break;
                        }
                        if (updated.phase == last_phase) {
                            break;
                        }
                        last_phase = updated.phase;
                    }

                    if (sched.remaining_modules < prev_remaining) {
                        progress = true;
                    }
                }
            }

            if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded) {
                // If we queued any new work onto the global scheduler, wait for it.
                self.global_queue.waitForIdle();
            }
        }
    }

    // ------------------------
    // Resolver implementation
    // ------------------------

    const ResolverCtx = struct { ws: *BuildEnv };

    const ScheduleCtx = struct {
        gpa: Allocator,
        sink: *OrderedSink,
        pkg: []const u8,
        ws: *BuildEnv,

        // Called by ModuleBuild.schedule_hook when a module is discovered/scheduled
        pub fn onSchedule(_: ?*anyopaque, _: []const u8, _: []const u8, _: []const u8, _: u32) void {
            // Early reports auto-register in OrderedSink.emitReport when they are emitted
        }
    };

    // External import classification heuristic removed.
    // ModuleBuild determines external vs local using CIR qualifier metadata (s_import.qualifier_tok).

    fn splitQualifier(import_name: []const u8) struct { qual: []const u8, rest: []const u8 } {
        if (std.mem.indexOfScalar(u8, import_name, '.')) |dot| {
            return .{ .qual = import_name[0..dot], .rest = import_name[dot + 1 ..] };
        } else {
            return .{ .qual = import_name, .rest = "" };
        }
    }

    fn resolverScheduleExternal(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) void {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return;

        const parts = splitQualifier(import_name);
        const qual = parts.qual;
        const rest = parts.rest;

        const ref = cur_pkg.shorthands.get(qual) orelse {
            return;
        };
        const target_pkg_name = ref.name;
        const target_pkg = self.ws.packages.get(target_pkg_name) orelse {
            return;
        };

        const mod_path = self.ws.dottedToPath(target_pkg.root_dir, rest) catch {
            return;
        };
        defer self.ws.gpa.free(mod_path);

        const sched = self.ws.schedulers.get(target_pkg_name) orelse {
            return;
        };
        sched.*.scheduleModule(rest, mod_path, 1) catch {
            // Continue anyway - dependency resolution will handle missing modules
        };
    }

    fn resolverIsReady(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) bool {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return false;

        const parts = splitQualifier(import_name);
        const qual = parts.qual;
        const rest = parts.rest;

        const ref = cur_pkg.shorthands.get(qual) orelse return false;
        const sched = self.ws.schedulers.get(ref.name) orelse return false;

        return sched.*.getEnvIfDone(rest) != null;
    }

    fn resolverGetEnv(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) ?*ModuleEnv {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return null;

        const parts = splitQualifier(import_name);
        const qual = parts.qual;
        const rest = parts.rest;

        const ref = cur_pkg.shorthands.get(qual) orelse {
            return null;
        };
        const sched = self.ws.schedulers.get(ref.name) orelse {
            return null;
        };

        const result = sched.*.getEnvIfDone(rest);
        return result;
    }

    fn resolverResolveLocalPath(ctx: ?*anyopaque, _: []const u8, root_dir: []const u8, import_name: []const u8) []const u8 {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        return self.ws.dottedToPath(root_dir, import_name) catch import_name;
    }

    fn makeResolver(self: *BuildEnv) ImportResolver {
        const ctx = self.gpa.create(ResolverCtx) catch {
            @panic("Cannot continue without resolver context");
        };
        ctx.* = .{ .ws = self };
        return .{
            .ctx = ctx,
            .scheduleExternal = resolverScheduleExternal,
            .isReady = resolverIsReady,
            .getEnv = resolverGetEnv,
            .resolveLocalPath = resolverResolveLocalPath,
        };
    }

    // ------------------------
    // Package graph construction
    // ------------------------

    const PackageKind = enum { app, package, platform, module, hosted, type_module, default_app };

    const PackageRef = struct {
        name: []const u8, // Package name (alias in workspace)
        root_file: []const u8, // Absolute path to root module of the package
    };

    const Package = struct {
        name: []u8,
        kind: PackageKind,
        root_file: []u8,
        root_dir: []u8,
        shorthands: std.StringHashMapUnmanaged(PackageRef) = .{},

        fn deinit(self: *Package, gpa: Allocator) void {
            var it = self.shorthands.iterator();
            while (it.next()) |e| {
                freeConstSlice(gpa, e.key_ptr.*);
                freeConstSlice(gpa, e.value_ptr.name);
                freeConstSlice(gpa, e.value_ptr.root_file);
            }
            self.shorthands.deinit(gpa);
            freeSlice(gpa, self.name);
            freeSlice(gpa, self.root_file);
            freeSlice(gpa, self.root_dir);
        }
    };

    const HeaderInfo = struct {
        kind: PackageKind,
        platform_alias: ?[]u8 = null,
        platform_path: ?[]u8 = null,
        shorthands: std.StringHashMapUnmanaged([]const u8) = .{},
        /// Platform-exposed modules (e.g., Stdout, Stderr) that apps can import
        exposes: std.ArrayListUnmanaged([]const u8) = .{},

        fn deinit(self: *HeaderInfo, gpa: Allocator) void {
            if (self.platform_alias) |a| freeSlice(gpa, a);
            if (self.platform_path) |p| freeSlice(gpa, p);
            var it = self.shorthands.iterator();
            while (it.next()) |e| {
                freeConstSlice(gpa, e.key_ptr.*);
                freeConstSlice(gpa, e.value_ptr.*);
            }
            self.shorthands.deinit(gpa);
            for (self.exposes.items) |e| {
                freeConstSlice(gpa, e);
            }
            self.exposes.deinit(gpa);
        }
    };

    fn detectPackageCycle(self: *BuildEnv, from_pkg: []const u8, to_pkg: []const u8) bool {
        // Simple DFS walk on shorthand edges to detect if to_pkg reaches from_pkg
        if (std.mem.eql(u8, from_pkg, to_pkg)) return true;

        var stack = std.ArrayList([]const u8).empty;
        defer stack.deinit(self.gpa);
        stack.append(self.gpa, to_pkg) catch {
            return false;
        };

        var visited = std.StringHashMapUnmanaged(void){};
        defer visited.deinit(self.gpa);

        while (stack.items.len > 0) {
            const idx = stack.items.len - 1;
            const cur = stack.items[idx];
            stack.items.len = idx;
            if (visited.contains(cur)) continue;
            visited.put(self.gpa, cur, {}) catch {
                return false;
            };

            if (std.mem.eql(u8, cur, from_pkg)) return true;

            const pkg = self.packages.get(cur) orelse continue;
            var it = pkg.shorthands.iterator();
            while (it.next()) |e| {
                const next = e.value_ptr.name;
                stack.append(self.gpa, next) catch {
                    return false;
                };
            }
        }
        return false;
    }

    fn parseHeaderDeps(self: *BuildEnv, file_path: []const u8) !HeaderInfo {
        // Read source
        const file_abs = try std.fs.path.resolve(self.gpa, &.{file_path});
        defer self.gpa.free(file_abs);
        const src = self.readFile(file_abs, std.math.maxInt(usize)) catch |err| {
            const report = blk: switch (err) {
                error.FileNotFound => {
                    var report = Report.init(self.gpa, "FILE NOT FOUND", .fatal);
                    try report.document.addText("I could not find the file ");
                    try report.document.addAnnotated(file_abs, .path);
                    try report.document.addLineBreak();
                    try report.document.addText("Make sure the file exists and you do not have any typos in its name or path.");
                    break :blk report;
                },

                else => {
                    var report = Report.init(self.gpa, "COULD NOT READ FILE", .fatal);
                    try report.document.addText("I could not read the file ");
                    try report.document.addAnnotated(file_abs, .path);
                    try report.document.addLineBreak();
                    try report.document.addText("I did get the following error: ");
                    try report.addErrorMessage(@errorName(err));
                    try report.document.addText("Make sure the file can be read.");
                    break :blk report;
                },
            };
            self.sink.emitReport("main", file_abs, report);
            try self.sink.buildOrder(&[_][]const u8{"main"}, &[_][]const u8{file_abs}, &[_]u32{0});
            self.sink.tryEmit();
            return err;
        };
        defer self.gpa.free(src);

        var env = try ModuleEnv.init(self.gpa, src);
        defer env.deinit();

        try env.common.calcLineStarts(self.gpa);

        var ast = try parse.parse(&env.common, self.gpa);
        defer ast.deinit(self.gpa);

        // Check for parse errors - if any exist, we cannot proceed
        if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) {
            // Convert diagnostics to reports and emit them
            // Use placeholder package name since we haven't determined it yet
            const pkg_name = "main";
            const module_name = file_abs;

            for (ast.tokenize_diagnostics.items) |diagnostic| {
                const report = try ast.tokenizeDiagnosticToReport(diagnostic, self.gpa, file_abs);
                self.sink.emitReport(pkg_name, module_name, report);
            }
            for (ast.parse_diagnostics.items) |diagnostic| {
                const report = try ast.parseDiagnosticToReport(&env.common, diagnostic, self.gpa, file_abs);
                self.sink.emitReport(pkg_name, module_name, report);
            }

            // Build the order so drainReports can find these reports
            try self.sink.buildOrder(&[_][]const u8{pkg_name}, &[_][]const u8{module_name}, &[_]u32{0});

            // Emit ready entries (marks them as emitted so they can be drained)
            self.sink.tryEmit();

            return error.UnsupportedHeader;
        }

        const file = ast.store.getFile();
        const header = ast.store.getHeader(file.header);

        var info = HeaderInfo{ .kind = .package };
        errdefer info.deinit(self.gpa);

        switch (header) {
            .app => |a| {
                info.kind = .app;

                // Platform field
                const pf = ast.store.getRecordField(a.platform_idx);
                const alias = ast.resolve(pf.name);
                const value_expr = pf.value orelse return error.ExpectedPlatformString;
                const plat_rel = try self.stringFromExpr(&ast, value_expr);
                defer self.gpa.free(plat_rel);

                // Check if this is a URL - if so, resolve it to a cached local path
                const plat_path = if (isUrl(plat_rel)) blk: {
                    const cached_path = try self.resolveUrlPackage(plat_rel);
                    break :blk cached_path;
                } else blk: {
                    const header_dir = std.fs.path.dirname(file_abs) orelse ".";
                    const abs_path = try PathUtils.makeAbsolute(self.gpa, header_dir, plat_rel);
                    // Restrict platform dependency path to be within the workspace root(s) even if declared by app.
                    if (!PathUtils.isWithinRoot(abs_path, self.workspace_roots.items)) {
                        self.gpa.free(abs_path);
                        return error.PathOutsideWorkspace;
                    }
                    break :blk abs_path;
                };

                info.platform_alias = try self.gpa.dupe(u8, alias);
                info.platform_path = @constCast(plat_path);

                // For URL-resolved packages, add the cache directory to workspace roots
                // so that imports within the cached package can be resolved
                if (isUrl(plat_rel)) {
                    if (std.fs.path.dirname(plat_path)) |cache_pkg_dir| {
                        try self.workspace_roots.append(try self.gpa.dupe(u8, cache_pkg_dir));
                    }
                }

                // Packages map
                const coll = ast.store.getCollection(a.packages);
                const fields = ast.store.recordFieldSlice(.{ .span = coll.span });
                for (fields) |idx| {
                    const rf = ast.store.getRecordField(idx);
                    const k = ast.resolve(rf.name);
                    if (rf.value == null) {
                        // If no value is provided for an app field, skip it
                        continue;
                    }
                    const relp = try self.stringFromExpr(&ast, rf.value.?);
                    defer self.gpa.free(relp);

                    // Check if this is a URL - if so, resolve it to a cached local path
                    const v = if (isUrl(relp)) blk: {
                        const cached_path = try self.resolveUrlPackage(relp);
                        // Add cache directory to workspace roots for URL packages
                        if (std.fs.path.dirname(cached_path)) |cache_pkg_dir| {
                            try self.workspace_roots.append(try self.gpa.dupe(u8, cache_pkg_dir));
                        }
                        break :blk cached_path;
                    } else blk: {
                        const header_dir2 = std.fs.path.dirname(file_abs) orelse ".";
                        break :blk try PathUtils.makeAbsolute(self.gpa, header_dir2, relp);
                    };

                    // TODO: actually handle duplicate keys
                    if (info.shorthands.fetchRemove(k)) |e| {
                        self.gpa.free(e.key);
                        self.gpa.free(e.value);
                    }
                    try info.shorthands.put(self.gpa, try self.gpa.dupe(u8, k), v);
                }
            },
            .package => |p| {
                info.kind = .package;
                const coll = ast.store.getCollection(p.packages);
                const fields = ast.store.recordFieldSlice(.{ .span = coll.span });
                for (fields) |idx| {
                    const rf = ast.store.getRecordField(idx);
                    const k = ast.resolve(rf.name);
                    if (rf.value == null) {
                        // If no value is provided for a package field, skip it
                        continue;
                    }
                    const relp = try self.stringFromExpr(&ast, rf.value.?);
                    defer self.gpa.free(relp);

                    // Check if this is a URL - if so, resolve it to a cached local path
                    const v = if (isUrl(relp)) blk: {
                        const cached_path = try self.resolveUrlPackage(relp);
                        // Add cache directory to workspace roots for URL packages
                        if (std.fs.path.dirname(cached_path)) |cache_pkg_dir| {
                            try self.workspace_roots.append(try self.gpa.dupe(u8, cache_pkg_dir));
                        }
                        break :blk cached_path;
                    } else blk: {
                        const header_dir2 = std.fs.path.dirname(file_abs) orelse ".";
                        const abs_path = try PathUtils.makeAbsolute(self.gpa, header_dir2, relp);
                        // Enforce: package header deps must be within workspace roots
                        if (!PathUtils.isWithinRoot(abs_path, self.workspace_roots.items)) {
                            self.gpa.free(abs_path);
                            return error.PathOutsideWorkspace;
                        }
                        break :blk abs_path;
                    };

                    // TODO: actually handle duplicate keys
                    if (info.shorthands.fetchRemove(k)) |e| {
                        self.gpa.free(e.key);
                        self.gpa.free(e.value);
                    }
                    try info.shorthands.put(self.gpa, try self.gpa.dupe(u8, k), v);
                }
            },
            .platform => |p| {
                info.kind = .platform;
                const coll = ast.store.getCollection(p.packages);
                const fields = ast.store.recordFieldSlice(.{ .span = coll.span });
                for (fields) |idx| {
                    const rf = ast.store.getRecordField(idx);
                    const k = ast.resolve(rf.name);
                    if (rf.value == null) {
                        // If no value is provided for a platform field, skip it
                        continue;
                    }
                    const relp = try self.stringFromExpr(&ast, rf.value.?);
                    defer self.gpa.free(relp);

                    // Check if this is a URL - if so, resolve it to a cached local path
                    const v = if (isUrl(relp)) blk: {
                        const cached_path = try self.resolveUrlPackage(relp);
                        // Add cache directory to workspace roots for URL packages
                        if (std.fs.path.dirname(cached_path)) |cache_pkg_dir| {
                            try self.workspace_roots.append(try self.gpa.dupe(u8, cache_pkg_dir));
                        }
                        break :blk cached_path;
                    } else blk: {
                        const header_dir2 = std.fs.path.dirname(file_abs) orelse ".";
                        const abs_path = try PathUtils.makeAbsolute(self.gpa, header_dir2, relp);
                        // Enforce: platform header deps must be within workspace roots
                        if (!PathUtils.isWithinRoot(abs_path, self.workspace_roots.items)) {
                            self.gpa.free(abs_path);
                            return error.PathOutsideWorkspace;
                        }
                        break :blk abs_path;
                    };

                    // TODO: actually handle duplicate keys
                    if (info.shorthands.fetchRemove(k)) |e| {
                        self.gpa.free(e.key);
                        self.gpa.free(e.value);
                    }
                    try info.shorthands.put(self.gpa, try self.gpa.dupe(u8, k), v);
                }

                // Extract platform-exposed modules (e.g., Stdout, Stderr)
                // These are modules that apps can import from the platform
                const exposes_coll = ast.store.getCollection(p.exposes);
                const exposes_items = ast.store.exposedItemSlice(.{ .span = exposes_coll.span });
                for (exposes_items) |item_idx| {
                    const item = ast.store.getExposedItem(item_idx);
                    const token_idx = switch (item) {
                        .upper_ident => |ui| ui.ident,
                        .upper_ident_star => |uis| uis.ident,
                        .lower_ident => |li| li.ident,
                        .malformed => continue, // Skip malformed items
                    };
                    const item_name = ast.resolve(token_idx);
                    try info.exposes.append(self.gpa, try self.gpa.dupe(u8, item_name));
                }
            },
            .module => {
                info.kind = .module;
                // Module headers don't have package dependencies, just exports/imports
                // We'll handle imports during the module build process
            },
            .hosted => {
                info.kind = .hosted;
                // Hosted headers are like modules but for platform-specific code
            },
            .type_module => {
                info.kind = .type_module;
                // Type modules are headerless files with a top-level type matching the filename
                // They don't have package dependencies
            },
            .default_app => {
                info.kind = .default_app;
                // Default app headers are for REPL-style execution
            },
            else => return error.UnsupportedHeader,
        }

        return info;
    }

    fn stringFromExpr(self: *BuildEnv, ast: *parse.AST, expr_idx: parse.AST.Expr.Idx) ![]const u8 {
        const e = ast.store.getExpr(expr_idx);
        return switch (e) {
            .string => |s| blk: {
                var buf = std.ArrayList(u8).empty;
                errdefer buf.deinit(self.gpa);

                // Use exprSlice to properly iterate through string parts
                for (ast.store.exprSlice(s.parts)) |part_idx| {
                    const part = ast.store.getExpr(part_idx);
                    if (part == .string_part) {
                        const tok = part.string_part.token;
                        const slice = ast.resolve(tok);
                        try buf.appendSlice(self.gpa, slice);
                    }
                }

                const result = try buf.toOwnedSlice(self.gpa);

                // Check for null bytes in the string, which are invalid in file paths
                if (std.mem.indexOfScalar(u8, result, 0) != null) {
                    self.gpa.free(result);
                    return error.InvalidNullByteInPath;
                }

                break :blk result;
            },
            else => error.ExpectedString,
        };
    }

    fn makeAbsolute(self: *BuildEnv, path: []const u8) ![]const u8 {
        if (std.fs.path.isAbsolute(path)) return try std.fs.path.resolve(self.gpa, &.{path});

        // Resolve relative to process cwd, then canonicalize
        const cwd_tmp = try std.process.getCwdAlloc(std.heap.page_allocator);
        defer std.heap.page_allocator.free(cwd_tmp);
        return try std.fs.path.resolve(self.gpa, &.{ cwd_tmp, path });
    }

    fn readFile(self: *BuildEnv, path: []const u8, max_bytes: usize) ![]u8 {
        const raw_data = if (self.file_provider) |fp|
            if (try fp.read(fp.ctx, path, self.gpa)) |data| data else null
        else
            null;

        const data = raw_data orelse try std.fs.cwd().readFileAlloc(self.gpa, path, max_bytes);

        // Normalize line endings (CRLF -> LF) for consistent cross-platform behavior.
        // This reallocates to the correct size if normalization occurs, ensuring
        // proper memory management when the buffer is freed later.
        return base.source_utils.normalizeLineEndingsRealloc(self.gpa, data);
    }

    /// Check if a path is a URL (http:// or https://)
    fn isUrl(path: []const u8) bool {
        return std.mem.startsWith(u8, path, "http://") or std.mem.startsWith(u8, path, "https://");
    }

    /// Cross-platform environment variable lookup.
    /// Uses std.process.getEnvVarOwned which works on both POSIX and Windows,
    /// unlike std.posix.getenv which only works on POSIX systems.
    fn getEnvVar(allocator: Allocator, key: []const u8) ?[]const u8 {
        return std.process.getEnvVarOwned(allocator, key) catch null;
    }

    /// Get the roc cache directory for downloaded packages.
    /// Standard cache locations by platform:
    /// - Linux/macOS: ~/.cache/roc/packages/ (respects XDG_CACHE_HOME if set)
    /// - Windows: %LOCALAPPDATA%\roc\packages\
    fn getRocCacheDir(allocator: Allocator) ![]const u8 {
        // Check XDG_CACHE_HOME first (Linux/macOS)
        if (getEnvVar(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
            defer allocator.free(xdg_cache);
            return std.fs.path.join(allocator, &.{ xdg_cache, "roc", "packages" });
        }

        // Fall back to %LOCALAPPDATA%\roc\packages (Windows)
        if (comptime builtin.os.tag == .windows) {
            if (getEnvVar(allocator, "LOCALAPPDATA")) |local_app_data| {
                defer allocator.free(local_app_data);
                return std.fs.path.join(allocator, &.{ local_app_data, "roc", "packages" });
            }
        }

        // Fall back to ~/.cache/roc/packages (Unix)
        if (getEnvVar(allocator, "HOME")) |home| {
            defer allocator.free(home);
            return std.fs.path.join(allocator, &.{ home, ".cache", "roc", "packages" });
        }

        return error.NoCacheDir;
    }

    /// Resolve a URL package by downloading and caching it.
    /// Returns the local path to the cached package's main.roc or platform.roc file.
    fn resolveUrlPackage(self: *BuildEnv, url: []const u8) ![]const u8 {
        const download = unbundle.download;

        // Validate URL and extract hash
        const base58_hash = download.validateUrl(url) catch |err| {
            std.log.err("Invalid package URL: {s} ({})", .{ url, err });
            return error.InvalidUrl;
        };

        // Get cache directory
        const cache_dir_path = getRocCacheDir(self.gpa) catch {
            std.log.err("Could not determine cache directory", .{});
            return error.NoCacheDir;
        };
        defer self.gpa.free(cache_dir_path);

        const package_dir_path = try std.fs.path.join(self.gpa, &.{ cache_dir_path, base58_hash });
        errdefer self.gpa.free(package_dir_path);

        // Check if already cached
        var package_dir = std.fs.cwd().openDir(package_dir_path, .{}) catch |err| switch (err) {
            error.FileNotFound => blk: {
                // Not cached - need to download
                std.log.info("Downloading package from {s}...", .{url});

                // Create cache directory structure
                std.fs.cwd().makePath(cache_dir_path) catch |make_err| {
                    std.log.err("Failed to create cache directory: {}", .{make_err});
                    return error.FileError;
                };

                // Create package directory
                std.fs.cwd().makeDir(package_dir_path) catch |make_err| switch (make_err) {
                    error.PathAlreadyExists => {}, // Race condition, another process created it
                    else => {
                        std.log.err("Failed to create package directory: {}", .{make_err});
                        return error.FileError;
                    },
                };

                var new_package_dir = std.fs.cwd().openDir(package_dir_path, .{}) catch |open_err| {
                    std.log.err("Failed to open package directory: {}", .{open_err});
                    return error.FileError;
                };

                // Download and extract
                var gpa_copy = self.gpa;
                download.downloadAndExtract(&gpa_copy, url, new_package_dir) catch |download_err| {
                    // Clean up failed download
                    new_package_dir.close();
                    std.fs.cwd().deleteTree(package_dir_path) catch {};
                    std.log.err("Failed to download package: {}", .{download_err});
                    return error.DownloadFailed;
                };

                std.log.info("Package cached at {s}", .{package_dir_path});
                break :blk new_package_dir;
            },
            else => {
                std.log.err("Failed to access package directory: {}", .{err});
                return error.FileError;
            },
        };
        defer package_dir.close();

        // Packages must have a main.roc entry point
        const source_path = std.fs.path.join(self.gpa, &.{ package_dir_path, "main.roc" }) catch {
            return error.OutOfMemory;
        };
        std.fs.cwd().access(source_path, .{}) catch {
            self.gpa.free(source_path);
            std.log.err("No main.roc found in package at {s}", .{package_dir_path});
            return error.NoPackageSource;
        };
        self.gpa.free(package_dir_path);
        return source_path;
    }

    fn dottedToPath(self: *BuildEnv, root_dir: []const u8, dotted: []const u8) ![]const u8 {
        var parts = std.mem.splitScalar(u8, dotted, '.');
        var segs = std.ArrayList([]const u8).empty;
        defer segs.deinit(self.gpa);

        try segs.append(self.gpa, root_dir);
        while (parts.next()) |p| {
            if (p.len == 0) continue;
            try segs.append(self.gpa, p);
        }

        const joined = try std.fs.path.join(self.gpa, segs.items);
        defer self.gpa.free(joined);

        const with_ext = try std.fmt.allocPrint(self.gpa, "{s}.roc", .{joined});
        errdefer self.gpa.free(with_ext);

        // Canonicalize and sandbox
        const canon = try std.fs.path.resolve(self.gpa, &.{with_ext});
        self.gpa.free(with_ext);
        // Enforce sandbox for dotted resolution: must be within the current package root (first workspace root).
        // If multiple roots are registered, we still require the resolved path to match at least one.
        if (!PathUtils.isWithinRoot(canon, self.workspace_roots.items)) {
            self.gpa.free(canon);
            return error.PathOutsideWorkspace;
        }
        return canon;
    }

    fn ensurePackage(self: *BuildEnv, name: []const u8, kind: PackageKind, root_file_abs: []const u8) !void {
        if (self.packages.contains(name)) return;

        const dir_raw = std.fs.path.dirname(root_file_abs) orelse ".";
        const dir = try std.fs.path.resolve(self.gpa, &.{dir_raw});
        const name_owned = try self.gpa.dupe(u8, name);
        const key_owned = try self.gpa.dupe(u8, name);
        const file_owned = try std.fs.path.resolve(self.gpa, &.{root_file_abs});
        // If this is the app package, allow arbitrary root file path (no sandbox). Otherwise enforce sandbox.

        // Sandbox check: app is exempt; package/platform must be within workspace roots
        if (!std.mem.eql(u8, name, "app")) {
            if (!PathUtils.isWithinRoot(file_owned, self.workspace_roots.items)) {
                self.gpa.free(dir);
                self.gpa.free(name_owned);
                self.gpa.free(key_owned);
                self.gpa.free(file_owned);
                return error.PathOutsideWorkspace;
            }
        }

        try self.packages.put(self.gpa, key_owned, .{
            .name = name_owned,
            .kind = kind,
            .root_file = file_owned,
            .root_dir = dir,
        });
    }

    const PkgSinkCtx = struct {
        gpa: Allocator,
        sink: *OrderedSink,
        pkg: []const u8,

        fn emit(ctx: ?*anyopaque, module_name: []const u8, report: Report) void {
            var self: *PkgSinkCtx = @ptrCast(@alignCast(ctx.?));
            self.sink.emitReport(self.pkg, module_name, report);
        }
    };

    fn createSchedulers(self: *BuildEnv) !void {
        const resolver = self.makeResolver();
        // Track resolver ctx for cleanup (typed)
        try self.resolver_ctxs.append(@ptrCast(@alignCast(resolver.ctx)));
        var it = self.packages.iterator();
        while (it.next()) |e| {
            const name = e.key_ptr.*;
            const pkg = e.value_ptr.*;

            // Per-package sink context to emit fully-qualified names
            const ps = try self.gpa.create(PkgSinkCtx);
            ps.* = .{ .gpa = self.gpa, .sink = &self.sink, .pkg = name };
            try self.pkg_sink_ctxs.append(ps);

            // Per-package schedule context to pre-register fq names on discovery
            const sc = try self.gpa.create(ScheduleCtx);
            sc.* = .{ .gpa = self.gpa, .sink = &self.sink, .pkg = name, .ws = self };
            try self.schedule_ctxs.append(sc);

            const sched = try self.gpa.create(PackageEnv);
            const schedule_hook: ScheduleHook = if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded)
                ScheduleHook{ .ctx = &self.global_queue, .onSchedule = GlobalQueue.hookOnSchedule }
            else
                ScheduleHook{ .ctx = sc, .onSchedule = ScheduleCtx.onSchedule };
            sched.* = PackageEnv.initWithResolver(
                self.gpa,
                name,
                pkg.root_dir,
                self.mode,
                self.max_threads,
                .{ .ctx = ps, .emitFn = PkgSinkCtx.emit },
                resolver,
                schedule_hook,
                self.compiler_version,
                self.builtin_modules,
                self.file_provider,
            );

            const key = try self.gpa.dupe(u8, name);
            try self.schedulers.put(self.gpa, key, sched);
        }
    }

    /// Register pending known modules with their target schedulers.
    /// Also schedules the external modules so they'll be built before the app.
    /// Called after createSchedulers() to ensure all schedulers exist.
    fn processPendingKnownModules(self: *BuildEnv) !void {
        for (self.pending_known_modules.items) |pkm| {
            if (self.schedulers.get(pkm.target_package)) |sched| {
                try sched.addKnownModule(pkm.qualified_name, pkm.import_name);
                // Also schedule the external module so it gets built
                // This is needed so the module is ready when we populate module_envs_map
                if (sched.resolver) |res| {
                    res.scheduleExternal(res.ctx, pkm.target_package, pkm.import_name);
                }
            }
        }
    }

    fn populatePackageShorthands(self: *BuildEnv, pkg_name: []const u8, info: *HeaderInfo) !void {
        var pack = self.packages.getPtr(pkg_name).?;

        // App-specific platform dependency
        if (info.platform_alias) |alias| {
            if (pack.kind != .app) return error.Internal;

            const p_path = info.platform_path.?;

            const abs = if (isUrl(p_path))
                try self.resolveUrlPackage(p_path)
            else
                try self.makeAbsolute(p_path);
            defer self.gpa.free(abs);

            var child_info = try self.parseHeaderDeps(abs);
            defer child_info.deinit(self.gpa);

            if (child_info.kind != .platform) {
                try self.emitWorkspaceError("Only apps may depend on platforms.");
                return error.InvalidDependency;
            }

            const dep_key = try self.gpa.dupe(u8, alias);
            const dep_name = try self.gpa.dupe(u8, alias);
            try self.ensurePackage(dep_name, .platform, abs);

            // Re-fetch pack pointer since ensurePackage may have caused HashMap reallocation
            pack = self.packages.getPtr(pkg_name).?;

            // If key already exists, free the old value before overwriting
            if (pack.shorthands.fetchRemove(dep_key)) |old_entry| {
                freeConstSlice(self.gpa, old_entry.key);
                freeConstSlice(self.gpa, old_entry.value.name);
                freeConstSlice(self.gpa, old_entry.value.root_file);
            }
            try pack.shorthands.put(self.gpa, dep_key, .{
                .name = dep_name,
                .root_file = try self.gpa.dupe(u8, abs),
            });

            try self.populatePackageShorthands(dep_name, &child_info);

            // Register platform-exposed modules as packages so apps can import them
            // This is necessary for URL platforms where the platform directory is in a cache
            const platform_dir = std.fs.path.dirname(abs) orelse ".";

            for (child_info.exposes.items) |module_name| {
                // Create path to the module file (e.g., Stdout.roc)
                const module_filename = try std.fmt.allocPrint(self.gpa, "{s}.roc", .{module_name});
                defer self.gpa.free(module_filename);

                const module_path = try std.fs.path.join(self.gpa, &.{ platform_dir, module_filename });
                defer self.gpa.free(module_path);

                // Register this module as a package
                // Only allocate if package doesn't exist (ensurePackage makes its own copy)
                if (!self.packages.contains(module_name)) {
                    try self.ensurePackage(module_name, .module, module_path);
                }

                // Re-fetch pack pointer since ensurePackage may have caused HashMap reallocation
                pack = self.packages.getPtr(pkg_name).?;

                // Also add to app's shorthands so imports resolve correctly
                const mod_key = try self.gpa.dupe(u8, module_name);
                if (pack.shorthands.fetchRemove(mod_key)) |old_entry| {
                    freeConstSlice(self.gpa, old_entry.key);
                    freeConstSlice(self.gpa, old_entry.value.name);
                    freeConstSlice(self.gpa, old_entry.value.root_file);
                }
                try pack.shorthands.put(self.gpa, mod_key, .{
                    .name = try self.gpa.dupe(u8, module_name),
                    .root_file = try self.gpa.dupe(u8, module_path),
                });

                // Add to pending list - will be registered after schedulers are created
                // Use the QUALIFIED name (e.g., "pf.Stdout") because that's how imports are tracked
                const qualified_name = try std.fmt.allocPrint(self.gpa, "{s}.{s}", .{ alias, module_name });
                try self.pending_known_modules.append(.{
                    .target_package = try self.gpa.dupe(u8, pkg_name),
                    .qualified_name = qualified_name,
                    .import_name = try self.gpa.dupe(u8, qualified_name),
                });
            }
        }

        // Common package dependencies
        var it = info.shorthands.iterator();
        while (it.next()) |e| {
            const alias = e.key_ptr.*;
            const path = e.value_ptr.*;

            const abs = if (isUrl(path))
                try self.resolveUrlPackage(path)
            else
                try self.makeAbsolute(path);
            defer self.gpa.free(abs);

            var child_info = try self.parseHeaderDeps(abs);
            defer child_info.deinit(self.gpa);

            if (child_info.kind == .app) {
                try self.emitWorkspaceError("Packages may not depend on apps.");
                return error.InvalidDependency;
            }
            if (child_info.kind == .platform and pack.kind != .app) {
                try self.emitWorkspaceError("Only apps may depend on platforms.");
                return error.InvalidDependency;
            }

            // Detect package-level cycles: if alias already on the path, report cycle
            if (self.detectPackageCycle(pkg_name, alias)) {
                // Build a more descriptive cycle message using source-level identifiers (aliases)
                // Do not include file paths here (cycle messages should show source identities).
                const msg = try std.fmt.allocPrint(self.gpa, "Package-level cycle detected involving aliases: {s} ... {s}", .{ pkg_name, alias });
                defer self.gpa.free(msg);
                try self.emitWorkspaceError(msg);
                return error.InvalidDependency;
            }

            const dep_key = try self.gpa.dupe(u8, alias);
            const dep_name = try self.gpa.dupe(u8, alias);

            try self.ensurePackage(dep_name, child_info.kind, abs);

            // Re-fetch pack pointer since ensurePackage may have caused HashMap reallocation
            pack = self.packages.getPtr(pkg_name).?;

            // If key already exists, free the old value before overwriting
            if (pack.shorthands.fetchRemove(dep_key)) |old_entry| {
                freeConstSlice(self.gpa, old_entry.key);
                freeConstSlice(self.gpa, old_entry.value.name);
                freeConstSlice(self.gpa, old_entry.value.root_file);
            }
            try pack.shorthands.put(self.gpa, dep_key, .{
                .name = dep_name,
                .root_file = try self.gpa.dupe(u8, abs),
            });

            try self.populatePackageShorthands(dep_name, &child_info);
        }
    }

    fn emitWorkspaceError(self: *BuildEnv, msg: []const u8) !void {
        var rep = Report.init(self.gpa, "Invalid package dependency", .runtime_error);
        const owned = try rep.addOwnedString(msg);
        try rep.addErrorMessage(owned);
        // Route through OrderedSink with a stable fully-qualified identity so it participates in ordering.
        // We use "workspace:root" as the fq module identity.
        self.sink.emitReport("workspace", "root", rep);
    }

    // Compute global deterministic emission of accumulated reports:
    // sort by (min dependency depth from root app, then package and module names).
    fn emitDeterministic(self: *BuildEnv) !void {
        // Build arrays of package names, module names, and depths
        var pkg_names = std.ArrayList([]const u8).empty;
        defer pkg_names.deinit(self.gpa);
        var module_names = std.ArrayList([]const u8).empty;
        defer module_names.deinit(self.gpa);
        var depths = std.ArrayList(u32).empty;
        defer depths.deinit(self.gpa);

        var it = self.schedulers.iterator();
        while (it.next()) |e| {
            const pkg_name = e.key_ptr.*;
            const sched = e.value_ptr.*;
            _ = self.packages.get(pkg_name).?;
            var mi = sched.moduleNamesIterator();
            while (mi.next()) |me| {
                const mod = me.key_ptr.*;
                const depth = sched.getModuleDepth(mod) orelse @as(u32, std.math.maxInt(u32));

                try pkg_names.append(self.gpa, pkg_name);
                try module_names.append(self.gpa, mod);
                try depths.append(self.gpa, depth);
            }
        }

        // Build the deterministic order
        try self.sink.buildOrder(pkg_names.items, module_names.items, depths.items);

        // Now that order is built, mark ready reports as emitted so they can be drained
        self.sink.lock.lock();
        defer self.sink.lock.unlock();
        self.sink.tryEmitLocked();
    }

    fn moduleToPath(self: *BuildEnv, pkg_name: []const u8, module_name: []const u8) ![]const u8 {
        if (self.packages.get(pkg_name)) |pkg| {
            return try self.dottedToPath(pkg.root_dir, module_name);
        }
        return error.InvalidPackageName;
    }

    pub const DrainedModuleReports = struct {
        abs_path: []const u8,
        reports: []Report,
    };

    pub fn drainReports(self: *BuildEnv) ![]DrainedModuleReports {
        const drained = try self.sink.drainEmitted(self.gpa);
        defer self.gpa.free(drained);

        var out = try self.gpa.alloc(DrainedModuleReports, drained.len);
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            const path = self.moduleToPath(drained[i].pkg_name, drained[i].module_name) catch try self.gpa.dupe(u8, "");
            out[i] = .{
                .abs_path = path,
                .reports = drained[i].reports,
            };
        }
        return out;
    }

    /// Get accumulated timing information from all ModuleBuild instances
    pub fn getTimingInfo(self: *BuildEnv) ModuleTimingInfo {
        var total = ModuleTimingInfo{
            .tokenize_parse_ns = 0,
            .canonicalize_ns = 0,
            .canonicalize_diagnostics_ns = 0,
            .type_checking_ns = 0,
            .check_diagnostics_ns = 0,
        };

        var it = self.schedulers.iterator();
        while (it.next()) |entry| {
            const scheduler = entry.value_ptr.*;
            const timing = scheduler.getTimingInfo();
            total.tokenize_parse_ns += timing.tokenize_parse_ns;
            total.canonicalize_ns += timing.canonicalize_ns;
            total.canonicalize_diagnostics_ns += timing.canonicalize_diagnostics_ns;
            total.type_checking_ns += timing.type_checking_ns;
            total.check_diagnostics_ns += timing.check_diagnostics_ns;
        }

        return total;
    }
};

// OrderedSink buffers reports and emits them in a deterministic global order.
// - Sorting key: (min dependency depth from root app, then package and module names for internal sorting only)
// - Reporting: per-module report aggregation; file paths are provided by BuildEnv at sink time
/// OrderedSink provides deterministic, ordered emission of module reports across
/// the entire workspace. It ensures that reports are always emitted in a consistent
/// order based on (depth, module_name) regardless of the actual completion order.
///
/// Key design principles:
/// - Modules can complete in any order (due to parallelism)
/// - Reports are buffered until they can be emitted in the correct order
/// - Once a module's reports are drained, they cannot be modified
/// - Supports both streaming (during build) and batch (end of build) draining
///
/// Thread safety: All public methods are thread-safe via internal locking.
pub const OrderedSink = struct {
    const ModuleKey = struct {
        pkg: []const u8,
        module: []const u8,
    };

    const ModuleKeyContext = struct {
        pub fn hash(_: @This(), key: ModuleKey) u64 {
            var h = std.hash.Wyhash.init(0);
            h.update(key.pkg);
            h.update(key.module);
            return h.final();
        }

        pub fn eql(_: @This(), a: ModuleKey, b: ModuleKey) bool {
            return std.mem.eql(u8, a.pkg, b.pkg) and std.mem.eql(u8, a.module, b.module);
        }
    };
    const Entry = struct {
        pkg_name: []const u8, // borrowed from BuildEnv.packages
        module_name: []const u8, // borrowed from ModuleBuild
        depth: u32, // min dependency depth
        reports: std.array_list.Managed(Report), // zero or more reports for this module
        ready: bool,
        emitted: bool,
    };

    gpa: Allocator,
    lock: Mutex = .{},
    cond: ThreadCondition = .{},

    // Ordered buffer and index
    entries: std.array_list.Managed(Entry),
    // Precomputed global order - indices into entries array
    order: std.array_list.Managed(usize),
    // Map (pkg, module) -> index in entries
    index: std.HashMap(ModuleKey, usize, ModuleKeyContext, 80),
    // Drain cursor into order[] for emitted entries
    // INVARIANT: drain_cursor tracks the position in order[] up to which all modules
    // have been emitted. Modules at order[0..drain_cursor] are guaranteed to be emitted.
    // Once a module is drained, its reports cannot be modified - any reports added after
    // draining will not be seen by subsequent drain operations. This ensures deterministic
    // ordering but means modules must complete all reporting before being marked as ready.
    drain_cursor: usize = 0,

    // Lifecycle
    pub fn init(gpa: Allocator) OrderedSink {
        return .{
            .gpa = gpa,
            .entries = std.array_list.Managed(Entry).init(gpa),
            .order = std.array_list.Managed(usize).init(gpa),
            .index = std.HashMap(ModuleKey, usize, ModuleKeyContext, 80).init(gpa),
        };
    }

    pub fn deinit(self: *OrderedSink) void {
        // Free entries
        for (self.entries.items) |*e| {
            // pkg_name and module_name are borrowed, don't free
            // Deinit all reports for this module
            for (e.reports.items) |*rep| {
                rep.deinit();
            }
            e.reports.deinit();
        }
        self.entries.deinit();
        self.order.deinit();
        self.index.deinit();
    }

    // Build deterministic order once: caller provides package names, module names, and depths
    pub fn buildOrder(self: *OrderedSink, pkg_names: []const []const u8, module_names: []const []const u8, depths: []const u32) !void {
        try self.order.ensureTotalCapacity(pkg_names.len);
        try self.entries.ensureTotalCapacity(pkg_names.len);
        try self.index.ensureTotalCapacity(@as(u32, @intCast(pkg_names.len)));

        // Rebuild order; allow pre-registered entries (from early emits) and update their metadata
        self.order.items.len = 0;

        var i: usize = 0;
        while (i < pkg_names.len) : (i += 1) {
            const key = ModuleKey{ .pkg = pkg_names[i], .module = module_names[i] };
            var entry_index: usize = undefined;
            if (self.index.get(key)) |idx| {
                entry_index = idx;
                // Update depth
                self.entries.items[entry_index].depth = depths[i];
            } else {
                // New entry
                entry_index = self.entries.items.len;
                const reports_list = std.array_list.Managed(Report).init(self.gpa);
                try self.entries.append(.{
                    .pkg_name = pkg_names[i],
                    .module_name = module_names[i],
                    .depth = depths[i],
                    .reports = reports_list,
                    .ready = false,
                    .emitted = false,
                });
                try self.index.put(key, entry_index);
            }
            // Add index to order
            try self.order.append(entry_index);
        }

        // Sort order array by (depth, name) via a simple stable sort (normalize case on Windows)
        const self_ptr = self;
        std.sort.block(usize, self.order.items, self_ptr, struct {
            fn lessIgnoreCase(a: []const u8, b: []const u8) bool {
                var idx: usize = 0;
                const min_len = if (a.len < b.len) a.len else b.len;
                while (idx < min_len) : (idx += 1) {
                    const ca = std.ascii.toLower(a[idx]);
                    const cb = std.ascii.toLower(b[idx]);
                    if (ca < cb) return true;
                    if (ca > cb) return false;
                }
                return a.len < b.len;
            }

            fn lt(ctx: *OrderedSink, a_idx: usize, b_idx: usize) bool {
                const ea = ctx.entries.items[a_idx];
                const eb = ctx.entries.items[b_idx];
                if (ea.depth != eb.depth) {
                    return ea.depth < eb.depth;
                }
                // Case-insensitive compare to be safe on Windows-like filesystems
                // First compare packages
                const pkg_cmp = lessIgnoreCase(ea.pkg_name, eb.pkg_name);
                if (pkg_cmp != lessIgnoreCase(eb.pkg_name, ea.pkg_name)) {
                    return pkg_cmp;
                }
                // Then compare modules
                return lessIgnoreCase(ea.module_name, eb.module_name);
            }
        }.lt);
    }

    // Emit with package and module names
    pub fn emitReport(self: *OrderedSink, pkg_name: []const u8, module_name: []const u8, report: Report) void {
        self.lock.lock();
        defer self.lock.unlock();

        // Lookup entry; auto-register if needed so we can buffer before order is built
        const key = ModuleKey{ .pkg = pkg_name, .module = module_name };
        var entry_index: usize = undefined;
        if (self.index.get(key)) |idx| {
            entry_index = idx;
        } else {
            entry_index = self.entries.items.len;
            const reports_list = std.array_list.Managed(Report).init(self.gpa);
            self.entries.append(.{
                .pkg_name = pkg_name,
                .module_name = module_name,
                .depth = std.math.maxInt(u32),
                .reports = reports_list,
                .ready = false,
                .emitted = false,
            }) catch return;
            if (self.index.put(key, entry_index) catch null == null) {
                return;
            }
            // Note: do not append to order here; buildOrder will rebuild and sort later
        }

        // Record report; take ownership by appending to per-module list
        self.entries.items[entry_index].reports.append(report) catch {
            var r = report;
            r.deinit();
            return;
        };
        self.entries.items[entry_index].ready = true;

        // Attempt ordered emission only if order has been built
        if (self.order.items.len > 0) self.tryEmitLocked();
    }

    // Attempt to emit entries in order prefix while next entries are ready (with locking).
    pub fn tryEmit(self: *OrderedSink) void {
        self.lock.lock();
        defer self.lock.unlock();
        self.tryEmitLocked();
    }

    // Attempt to emit entries in order prefix while next entries are ready.
    fn tryEmitLocked(self: *OrderedSink) void {
        var i: usize = 0;
        while (i < self.order.items.len) : (i += 1) {
            const entry_idx = self.order.items[i];
            const e = &self.entries.items[entry_idx];

            // Prefix gating: stop at first entry that is not yet ready and not yet emitted
            if (!e.ready and !e.emitted) break;

            // Skip already-emitted entries
            if (e.emitted) continue;

            // Emit this ready entry
            e.emitted = true;
        }
    }
    pub const Drained = struct {
        pkg_name: []const u8,
        module_name: []const u8,
        reports: []Report,
    };

    pub fn drainEmitted(self: *OrderedSink, gpa: Allocator) ![]Drained {
        self.lock.lock();
        defer self.lock.unlock();

        // Identify contiguous emitted prefix starting from current drain cursor
        var i: usize = self.drain_cursor;
        while (i < self.order.items.len) : (i += 1) {
            const entry_idx = self.order.items[i];
            const e = &self.entries.items[entry_idx];
            if (!e.emitted) break;
        }

        const count: usize = if (i >= self.drain_cursor) (i - self.drain_cursor) else 0;
        if (count == 0) {
            return try gpa.alloc(Drained, 0);
        }

        var out = try gpa.alloc(Drained, count);
        var j: usize = 0;
        while (j < count) : (j += 1) {
            const entry_idx = self.order.items[self.drain_cursor + j];
            const e = &self.entries.items[entry_idx];

            // Move reports out; reset readiness for potential future appends
            const reps = e.reports.toOwnedSlice() catch {
                // Back out partially allocated results on failure
                var k: usize = 0;
                while (k < j) : (k += 1) {
                    for (out[k].reports) |*r| r.deinit();
                    gpa.free(out[k].reports);
                }
                gpa.free(out);
                return error.OutOfMemory;
            };

            out[j] = .{
                .pkg_name = e.pkg_name,
                .module_name = e.module_name,
                .reports = reps,
            };

            // Reinitialize the reports ArrayList since toOwnedSlice() moved ownership
            e.reports = std.array_list.Managed(Report).init(self.gpa);
            e.ready = false;
            e.emitted = false;
        }

        self.drain_cursor = i;
        return out;
    }
};

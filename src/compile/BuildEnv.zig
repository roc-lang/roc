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
const reporting = @import("reporting");
const parse = @import("parse");

// Import BuildModule and ModuleEnv directly to avoid circular package dependencies.
const BuildModuleMod = @import("BuildModule.zig");
const ModuleEnvMod = @import("ModuleEnv.zig");

const ModuleBuild = BuildModuleMod.ModuleBuild;
const Mode = BuildModuleMod.Mode;
const Report = reporting.Report;

const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

// Compile-time feature flags and thread/cond wrappers for targets without threads (e.g. wasm32)
const threads_available = builtin.target.cpu.arch != .wasm32;

/// Errors that can occur during build operations
pub const BuildError = error{
    OutOfMemory,
    ModuleProcessingFailed,
    SchedulingFailed,
    InvalidPath,
};

/// Log levels for build operations
pub const LogLevel = enum {
    err,
    warn,
    info,
    debug,
};

/// Simple logger interface - can be overridden for production use
pub const Logger = struct {
    level: LogLevel = .warn,

    pub fn log(self: Logger, level: LogLevel, comptime fmt: []const u8, args: anytype) void {
        if (@intFromEnum(level) <= @intFromEnum(self.level)) {
            const prefix = switch (level) {
                .err => "ERROR",
                .warn => "WARN",
                .info => "INFO",
                .debug => "DEBUG",
            };
            std.debug.print("[{s}] " ++ fmt ++ "\n", .{prefix} ++ args);
        }
    }
};

/// Configuration options for build system behavior
pub const Config = struct {
    /// Timeout for waiting on module processing (microseconds)
    module_wait_timeout_us: u64 = 1_000_000, // 1 second default

    /// Maximum depth for module dependencies (prevent infinite recursion)
    max_module_depth: u32 = 1000,

    /// Sleep time for idle waiting (microseconds)
    idle_sleep_us: u64 = 1_000_000, // 1 ms default

    /// Maximum inflight tasks for global queue
    max_inflight_tasks: usize = 1000,

    /// Logger configuration
    logger: Logger = .{},
};

const StdThread = if (threads_available) std.Thread else struct {};
const StdMutex = if (threads_available) std.Thread.Mutex else struct {
    pub fn lock(self: *@This()) void {
        _ = self;
    }
    pub fn unlock(self: *@This()) void {
        _ = self;
    }
};
const StdCondition = if (threads_available) std.Thread.Condition else struct {
    pub fn wait(self: *@This(), _: *StdMutex) void {
        _ = self;
    }
    pub fn signal(self: *@This()) void {
        _ = self;
    }
    pub fn broadcast(self: *@This()) void {
        _ = self;
    }
};

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
} else struct {
    pub fn inc(self: *@This()) void {
        _ = self;
    }
    pub fn dec(self: *@This()) void {
        _ = self;
    }
    pub fn load(self: *@This()) usize {
        _ = self;
        return 0;
    }
};

fn freeSlice(gpa: Allocator, s: []const u8) void {
    gpa.free(@constCast(s));
}

// ===== Global unified work-stealing queue =====

const GlobalQueue = struct {
    const Task = struct {
        pkg: []const u8,
        module_name: []const u8,
    };

    gpa: Allocator,
    tasks: std.ArrayList(Task),
    lock: StdMutex = .{},
    cond: StdCondition = .{},
    workers: std.ArrayList(StdThread),
    running: bool = false,
    sink_ptr: ?*OrderedSink = null,
    // Pointer back to BuildEnv for dispatch
    build_env: ?*BuildEnv = null,
    // Inflight counter for idle detection (no-op on wasm)
    inflight: InflightCounter = .{},

    pub fn init(gpa: Allocator) GlobalQueue {
        return .{
            .gpa = gpa,
            .tasks = std.ArrayList(Task).init(gpa),
            .workers = std.ArrayList(std.Thread).init(gpa),
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
            self.lock.lock();
            const no_tasks = self.tasks.items.len == 0;
            self.lock.unlock();
            const inflight_zero = self.inflight.load() == 0;
            if (no_tasks and inflight_zero) break;
            const sleep_us = if (self.build_env) |be| be.config.idle_sleep_us else 1_000_000;
            std.time.sleep(sleep_us);
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
        self.lock.lock();
        defer self.lock.unlock();
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
                        const exists = sched.modules.getPtr(task.module_name) != null;
                        if (exists) {
                            // Mark inflight before processing and decrement after
                            self.inflight.inc();
                            defer self.inflight.dec();

                            sched.process(.{ .module_name = task.module_name }) catch |err| {
                                be.config.logger.log(.err, "Failed to process module {s} in package {s}: {}", .{ task.module_name, task.pkg, err });
                                // Continue processing other modules despite this error
                            };
                        }
                    }
                }
                freeSlice(self.gpa, task.pkg);
                freeSlice(self.gpa, task.module_name);
                continue;
            }

            self.lock.lock();
            while (self.tasks.items.len == 0 and self.running) {
                self.cond.wait(&self.lock);
            }
            const keep_running = self.running;
            self.lock.unlock();
            if (!keep_running) break;
        }
    }

    // Hook from ModuleBuild to enqueue newly discovered/scheduled modules
    pub fn hookOnSchedule(ctx: ?*anyopaque, package_name: []const u8, module_name: []const u8, _path: []const u8, _depth: u32) void {
        var self: *GlobalQueue = @ptrCast(@alignCast(ctx.?));
        _ = _path;
        _ = _depth;
        // Enqueue to global queue - log but don't fail on error
        self.enqueue(package_name, module_name) catch |err| {
            if (self.build_env) |be| {
                be.config.logger.log(.warn, "Failed to enqueue module {s} in package {s} to global queue: {}", .{ module_name, package_name, err });
            }
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
    config: Config = .{},

    // Workspace roots for sandboxing (absolute, canonical)
    workspace_roots: std.ArrayList([]const u8),

    // Map of package name (alias) -> Package
    packages: std.StringHashMapUnmanaged(Package) = .{},
    // Schedulers per package name
    schedulers: std.StringHashMapUnmanaged(*ModuleBuild) = .{},

    // Ordered sink over all packages (thread-safe, deterministic emission)
    sink: OrderedSink,

    // Unified global work-stealing queue (WSQ)
    global_queue: GlobalQueue,

    // Owned resolver ctx pointers for cleanup (typed)
    resolver_ctxs: std.ArrayList(*ResolverCtx),
    // Owned per-package sink contexts for fully-qualified emission
    pkg_sink_ctxs: std.ArrayList(*PkgSinkCtx),
    // Owned schedule ctxs for pre-registration (one per package)
    schedule_ctxs: std.ArrayList(*ScheduleCtx),

    pub fn init(gpa: Allocator, mode: Mode, max_threads: usize) BuildEnv {
        return initWithConfig(gpa, mode, max_threads, .{});
    }

    pub fn initWithConfig(gpa: Allocator, mode: Mode, max_threads: usize, config: Config) BuildEnv {
        return .{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .config = config,
            .workspace_roots = std.ArrayList([]const u8).init(gpa),
            .sink = OrderedSink.init(gpa),
            .global_queue = GlobalQueue.init(gpa),
            .resolver_ctxs = std.ArrayList(*ResolverCtx).init(gpa),
            .pkg_sink_ctxs = std.ArrayList(*PkgSinkCtx).init(gpa),
            .schedule_ctxs = std.ArrayList(*ScheduleCtx).init(gpa),
        };
    }

    pub fn deinit(self: *BuildEnv) void {
        // Stop global queue workers
        self.global_queue.deinit(self.gpa);
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

        // Deinit schedulers
        var sit = self.schedulers.iterator();
        while (sit.next()) |e| {
            const mb_ptr: *ModuleBuild = e.value_ptr.*;
            mb_ptr.deinit();
            self.gpa.destroy(mb_ptr);
            freeSlice(self.gpa, e.key_ptr.*);
        }
        self.schedulers.deinit(self.gpa);

        // Deinit packages
        var pit = self.packages.iterator();
        while (pit.next()) |e| {
            var p = e.value_ptr.*;
            p.deinit(self.gpa);
            freeSlice(self.gpa, e.key_ptr.*);
        }
        self.packages.deinit(self.gpa);

        // Clear back-pointer
        self.global_queue.build_env = null;

        // Deinit roots
        for (self.workspace_roots.items) |r| freeSlice(self.gpa, r);
        self.workspace_roots.deinit();

        self.sink.deinit();
    }

    // Build the workspace starting from an app root file path.
    // Assumptions:
    // - All header-declared paths are local filesystem paths (no URLs).
    // - Shorthand aliases uniquely identify packages within this workspace.
    pub fn buildApp(self: *BuildEnv, root_app_file: []const u8) !void {
        // Workspace root is directory containing the app header; normalize/sandbox
        const root_abs = try self.makeAbsolute(root_app_file);
        const root_dir = if (std.fs.path.dirname(root_abs)) |d| try std.fs.path.resolve(self.gpa, &.{d}) else try self.gpa.dupe(u8, ".");
        // NOTE: Do not sandbox the app header; the app may reference arbitrary paths.
        // We still record the root_dir for convenience in later checks.
        try self.workspace_roots.append(root_dir);

        var header_info = try self.parseHeaderDeps(root_abs);
        defer header_info.deinit(self.gpa);

        if (header_info.kind != .app) return error.ExpectedAppHeader;

        // Create app package entry
        const app_name = try self.gpa.dupe(u8, "app");
        const key_app = try self.gpa.dupe(u8, app_name);
        const app_root_file = try self.gpa.dupe(u8, root_abs);
        const app_root_dir = root_dir;

        try self.packages.put(self.gpa, key_app, .{
            .name = app_name,
            .kind = .app,
            .root_file = app_root_file,
            .root_dir = app_root_dir,
        });

        // Populate package graph and shorthand maps recursively
        try self.populatePackageShorthands("app", &header_info);

        // Create per-package schedulers wired with a shared resolver and global queue hook
        try self.createSchedulers();

        // Set back-pointer for dispatch
        self.global_queue.build_env = self;

        // Start global queue workers with ordered sink only when threads are available and requested
        if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded) {
            try self.global_queue.start(self.gpa, self.max_threads, &self.sink);
        }

        // Seed root app module into global queue via schedule hook (ModuleBuild will call back)
        const app_sched = self.schedulers.getPtr("app").?;
        try app_sched.*.buildRoot(app_root_file);

        // Kick remaining packages by seeding their root files too
        var it = self.schedulers.iterator();
        while (it.next()) |e| {
            const name = e.key_ptr.*;
            if (std.mem.eql(u8, name, "app")) continue;
            const pkg = self.packages.get(name).?;
            try e.value_ptr.*.buildRoot(pkg.root_file);
        }

        // Wait for global queue to drain only when using the global queue
        if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded) {
            self.global_queue.waitForIdle();
        }

        // Deterministic emission: globally order reports by (min dependency depth from app, then module name)
        try self.emitDeterministic();
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
        pub fn onSchedule(ctx: ?*anyopaque, package_name: []const u8, module_name: []const u8, _path: []const u8, _depth: u32) void {
            const self: *ScheduleCtx = @ptrCast(@alignCast(ctx.?));
            _ = package_name;
            _ = module_name;
            _ = _path;
            _ = _depth;
            // Early reports auto-register in OrderedSink.emitReport when they are emitted
            _ = self;
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

    fn resolverClassify(ctx: ?*anyopaque, _: []const u8, _: []const u8) bool {
        _ = ctx;
        // Unused: ModuleBuild determines external vs local from CIR (s_import.qualifier_tok)
        return false;
    }

    fn resolverScheduleExternal(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) void {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return;

        const parts = splitQualifier(import_name);
        const qual = parts.qual;
        const rest = parts.rest;

        const ref = cur_pkg.shorthands.get(qual) orelse return;
        const target_pkg_name = ref.name;
        const target_pkg = self.ws.packages.get(target_pkg_name) orelse return;

        const mod_path = self.ws.dottedToPath(target_pkg.root_dir, rest) catch |err| {
            self.ws.config.logger.log(.warn, "Failed to resolve path for module {s} in package {s}: {}", .{ rest, target_pkg_name, err });
            return;
        };
        defer self.ws.gpa.free(mod_path);

        const sched = self.ws.schedulers.get(target_pkg_name) orelse return;
        sched.*.scheduleModule(rest, mod_path, 1) catch |err| {
            self.ws.config.logger.log(.warn, "Failed to schedule external module {s} in package {s}: {}", .{ rest, target_pkg_name, err });
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

    fn resolverGetEnv(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) ?*ModuleEnvMod {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return null;

        const parts = splitQualifier(import_name);
        const qual = parts.qual;
        const rest = parts.rest;

        const ref = cur_pkg.shorthands.get(qual) orelse return null;
        const sched = self.ws.schedulers.get(ref.name) orelse return null;

        return sched.*.getEnvIfDone(rest);
    }

    fn resolverResolveLocalPath(ctx: ?*anyopaque, _current_package: []const u8, root_dir: []const u8, import_name: []const u8) []const u8 {
        _ = _current_package;
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        return self.ws.dottedToPath(root_dir, import_name) catch import_name;
    }

    fn makeResolver(self: *BuildEnv) BuildModuleMod.ImportResolver {
        const ctx = self.gpa.create(ResolverCtx) catch |err| {
            self.config.logger.log(.err, "Critical: Failed to allocate resolver context: {}", .{err});
            @panic("Cannot continue without resolver context");
        };
        ctx.* = .{ .ws = self };
        return .{
            .ctx = ctx,
            .classify = resolverClassify,
            .scheduleExternal = resolverScheduleExternal,
            .isReady = resolverIsReady,
            .getEnv = resolverGetEnv,
            .resolveLocalPath = resolverResolveLocalPath,
        };
    }

    // ------------------------
    // Package graph construction
    // ------------------------

    const PackageKind = enum { app, package, platform };

    const PackageRef = struct {
        name: []const u8, // Package name (alias in workspace)
        root_file: []const u8, // Absolute path to root module of the package
    };

    const Package = struct {
        name: []const u8,
        kind: PackageKind,
        root_file: []const u8,
        root_dir: []const u8,
        shorthands: std.StringHashMapUnmanaged(PackageRef) = .{},

        fn deinit(self: *Package, gpa: Allocator) void {
            var it = self.shorthands.iterator();
            while (it.next()) |e| {
                freeSlice(gpa, e.key_ptr.*);
                freeSlice(gpa, e.value_ptr.name);
                freeSlice(gpa, e.value_ptr.root_file);
            }
            self.shorthands.deinit(gpa);
            freeSlice(gpa, self.name);
            freeSlice(gpa, self.root_file);
            freeSlice(gpa, self.root_dir);
        }
    };

    const HeaderInfo = struct {
        kind: PackageKind,
        platform_alias: ?[]const u8 = null,
        platform_path: ?[]const u8 = null,
        shorthands: std.StringHashMapUnmanaged([]const u8) = .{},

        fn deinit(self: *HeaderInfo, gpa: Allocator) void {
            if (self.platform_alias) |a| freeSlice(gpa, a);
            if (self.platform_path) |p| freeSlice(gpa, p);
            var it = self.shorthands.iterator();
            while (it.next()) |e| {
                freeSlice(gpa, e.key_ptr.*);
                freeSlice(gpa, e.value_ptr.*);
            }
            self.shorthands.deinit(gpa);
        }
    };

    fn detectPackageCycle(self: *BuildEnv, from_pkg: []const u8, to_pkg: []const u8) bool {
        // Simple DFS walk on shorthand edges to detect if to_pkg reaches from_pkg
        if (std.mem.eql(u8, from_pkg, to_pkg)) return true;

        var stack = std.ArrayList([]const u8).init(self.gpa);
        defer stack.deinit();
        stack.append(to_pkg) catch |err| {
            self.config.logger.log(.warn, "Failed to append to cycle detection stack: {}", .{err});
            return false;
        };

        var visited = std.StringHashMapUnmanaged(void){};
        defer visited.deinit(self.gpa);

        while (stack.items.len > 0) {
            const idx = stack.items.len - 1;
            const cur = stack.items[idx];
            stack.items.len = idx;
            if (visited.contains(cur)) continue;
            visited.put(self.gpa, cur, {}) catch |err| {
                self.config.logger.log(.warn, "Failed to mark package as visited in cycle detection: {}", .{err});
                return false;
            };

            if (std.mem.eql(u8, cur, from_pkg)) return true;

            const pkg = self.packages.get(cur) orelse continue;
            var it = pkg.shorthands.iterator();
            while (it.next()) |e| {
                const next = e.value_ptr.name;
                stack.append(next) catch |err| {
                    self.config.logger.log(.warn, "Failed to append to cycle detection stack: {}", .{err});
                    return false;
                };
            }
        }
        return false;
    }

    fn parseHeaderDeps(self: *BuildEnv, file_path: []const u8) !HeaderInfo {
        // Read source; ModuleEnv takes ownership of source
        const file_abs = try std.fs.path.resolve(self.gpa, &.{file_path});
        const src = try std.fs.cwd().readFileAlloc(self.gpa, file_abs, std.math.maxInt(usize));
        var env = try ModuleEnvMod.init(self.gpa, src);
        defer env.deinit();
        try env.calcLineStarts();

        var ast = try parse.parse(&env);
        defer ast.deinit(self.gpa);

        const file = ast.store.getFile();
        const header = ast.store.getHeader(file.header);

        var info = HeaderInfo{ .kind = .package };

        switch (header) {
            .app => |a| {
                info.kind = .app;

                // Platform field
                const pf = ast.store.getRecordField(a.platform_idx);
                const alias = ast.resolve(pf.name);
                const value_expr = pf.value orelse return error.ExpectedPlatformString;
                const plat_rel = try self.stringFromExpr(&ast, value_expr);
                const header_dir = std.fs.path.dirname(file_abs) orelse ".";
                const plat_path = try PathUtils.makeAbsolute(self.gpa, header_dir, plat_rel);
                // Restrict platform dependency path to be within the workspace root(s) even if declared by app.
                if (!PathUtils.isWithinRoot(plat_path, self.workspace_roots.items)) {
                    self.gpa.free(plat_path);
                    return error.PathOutsideWorkspace;
                }

                info.platform_alias = try self.gpa.dupe(u8, alias);
                info.platform_path = plat_path;
                // Seed allowed root for platform/package resolution
                if (!PathUtils.isWithinRoot(plat_path, self.workspace_roots.items)) {
                    // app is allowed arbitrary, but platform/package resolving must be sandboxed; we enforced above
                }

                // Packages map
                const coll = ast.store.getCollection(a.packages);
                const fields = ast.store.recordFieldSlice(.{ .span = coll.span });
                for (fields) |idx| {
                    const rf = ast.store.getRecordField(idx);
                    const k = ast.resolve(rf.name);
                    const relp = try self.stringFromExpr(&ast, rf.value.?);
                    const header_dir2 = std.fs.path.dirname(file_abs) orelse ".";
                    const v = try PathUtils.makeAbsolute(self.gpa, header_dir2, relp);
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
                    const relp = try self.stringFromExpr(&ast, rf.value.?);
                    const header_dir2 = std.fs.path.dirname(file_abs) orelse ".";
                    const v = try PathUtils.makeAbsolute(self.gpa, header_dir2, relp);
                    // Enforce: package header deps must be within workspace roots
                    if (!PathUtils.isWithinRoot(v, self.workspace_roots.items)) {
                        self.gpa.free(v);
                        return error.PathOutsideWorkspace;
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
                    const relp = try self.stringFromExpr(&ast, rf.value.?);
                    const header_dir2 = std.fs.path.dirname(file_abs) orelse ".";
                    const v = try PathUtils.makeAbsolute(self.gpa, header_dir2, relp);
                    // Enforce: platform header deps must be within workspace roots
                    if (!PathUtils.isWithinRoot(v, self.workspace_roots.items)) {
                        self.gpa.free(v);
                        return error.PathOutsideWorkspace;
                    }
                    try info.shorthands.put(self.gpa, try self.gpa.dupe(u8, k), v);
                }
            },
            else => return error.UnsupportedHeader,
        }

        return info;
    }

    fn stringFromExpr(self: *BuildEnv, ast: *parse.AST, expr_idx: parse.AST.Expr.Idx) ![]const u8 {
        const e = ast.store.getExpr(expr_idx);
        return switch (e) {
            .string => |s| blk: {
                const span = s.parts.span;
                var buf = std.ArrayList(u8).init(self.gpa);
                errdefer buf.deinit();

                var i: usize = 0;
                while (i < span.len) : (i += 1) {
                    const sub_idx: parse.AST.Expr.Idx = @enumFromInt(span.start + i);
                    const part = ast.store.getExpr(sub_idx);
                    if (part == .string_part) {
                        const tok = part.string_part.token;
                        const slice = ast.resolve(tok);
                        try buf.appendSlice(slice);
                    }
                }

                break :blk try buf.toOwnedSlice();
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

    fn dottedToPath(self: *BuildEnv, root_dir: []const u8, dotted: []const u8) ![]const u8 {
        var parts = std.mem.splitScalar(u8, dotted, '.');
        var segs = std.ArrayList([]const u8).init(self.gpa);
        defer segs.deinit();

        try segs.append(root_dir);
        while (parts.next()) |p| {
            if (p.len == 0) continue;
            try segs.append(p);
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

            const sched = try self.gpa.create(ModuleBuild);
            sched.* = ModuleBuild{
                .gpa = self.gpa,
                .package_name = name,
                .root_dir = pkg.root_dir,
                .mode = self.mode,
                .max_threads = self.max_threads,
                .sink = .{ .ctx = ps, .emitFn = PkgSinkCtx.emit },
                .resolver = resolver,
                .schedule_hook = if (builtin.target.cpu.arch != .wasm32 and self.mode == .multi_threaded)
                    .{ .ctx = sc, .onSchedule = ScheduleCtx.onSchedule }
                else
                    .{ .ctx = sc, .onSchedule = ScheduleCtx.onSchedule },
            };

            const key = try self.gpa.dupe(u8, name);
            try self.schedulers.put(self.gpa, key, sched);
        }
    }

    fn populatePackageShorthands(self: *BuildEnv, pkg_name: []const u8, info: *HeaderInfo) !void {
        var pack = self.packages.getPtr(pkg_name).?;

        // App-specific platform dependency
        if (info.platform_alias) |alias| {
            if (pack.kind != .app) return error.Internal;

            const p_path = info.platform_path.?;
            const abs = try self.makeAbsolute(p_path);
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

            try pack.shorthands.put(self.gpa, dep_key, .{
                .name = dep_name,
                .root_file = try self.gpa.dupe(u8, abs),
            });

            try self.populatePackageShorthands(dep_name, &child_info);
        }

        // Common package dependencies
        var it = info.shorthands.iterator();
        while (it.next()) |e| {
            const alias = e.key_ptr.*;
            const path = e.value_ptr.*;

            const abs = try self.makeAbsolute(path);
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
        var pkg_names = std.ArrayList([]const u8).init(self.gpa);
        defer pkg_names.deinit();
        var module_names = std.ArrayList([]const u8).init(self.gpa);
        defer module_names.deinit();
        var depths = std.ArrayList(u32).init(self.gpa);
        defer depths.deinit();

        var it = self.schedulers.iterator();
        while (it.next()) |e| {
            const pkg_name = e.key_ptr.*;
            const sched = e.value_ptr.*;
            _ = self.packages.get(pkg_name).?;
            var mi = sched.modules.iterator();
            while (mi.next()) |me| {
                const mod = me.key_ptr.*;
                const depth = sched.getModuleDepth(mod) orelse @as(u32, std.math.maxInt(u32));

                try pkg_names.append(pkg_name);
                try module_names.append(mod);
                try depths.append(depth);
            }
        }

        // Build the deterministic order
        try self.sink.buildOrder(pkg_names.items, module_names.items, depths.items);
    }

    fn depthOf(self: *BuildEnv, pkg_name: []const u8, module_name: []const u8) !u32 {
        if (self.schedulers.get(pkg_name)) |sched| {
            return sched.getModuleDepth(module_name) orelse @as(u32, std.math.maxInt(u32));
        }
        return @as(u32, std.math.maxInt(u32));
    }

    fn moduleToPath(self: *BuildEnv, pkg_name: []const u8, module_name: []const u8) ![]const u8 {
        if (self.packages.get(pkg_name)) |pkg| {
            return try self.dottedToPath(pkg.root_dir, module_name);
        }
        return error.InvalidPackageName;
    }

    const DrainedModuleReports = struct {
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
const OrderedSink = struct {
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
        reports: std.ArrayList(Report), // zero or more reports for this module
        ready: bool,
        emitted: bool,
    };

    gpa: Allocator,
    lock: StdMutex = .{},
    cond: StdCondition = .{},

    // Ordered buffer and index
    entries: std.ArrayList(Entry),
    // Precomputed global order - indices into entries array
    order: std.ArrayList(usize),
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
    fn init(gpa: Allocator) OrderedSink {
        return .{
            .gpa = gpa,
            .entries = std.ArrayList(Entry).init(gpa),
            .order = std.ArrayList(usize).init(gpa),
            .index = std.HashMap(ModuleKey, usize, ModuleKeyContext, 80).init(gpa),
        };
    }

    fn deinit(self: *OrderedSink) void {
        // Free entries
        for (self.entries.items) |e| {
            // pkg_name and module_name are borrowed, don't free
            // Deinit all reports for this module
            var i: usize = 0;
            while (i < e.reports.items.len) : (i += 1) {
                var rep = e.reports.items[i];
                rep.deinit();
            }
            e.reports.deinit();
        }
        self.entries.deinit();
        self.order.deinit();
        self.index.deinit();
    }

    // Build deterministic order once: caller provides package names, module names, and depths
    fn buildOrder(self: *OrderedSink, pkg_names: []const []const u8, module_names: []const []const u8, depths: []const u32) !void {
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
                const reports_list = std.ArrayList(Report).init(self.gpa);
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

    // Package sink wrapper that adds package context
    fn makePkgSink(_: *OrderedSink, _: []const u8) BuildModuleMod.ReportSink {
        // This will be set up by PkgSinkCtx
        return .{ .ctx = undefined, .emitFn = undefined };
    }

    // Emit with package and module names
    fn emitReport(self: *OrderedSink, pkg_name: []const u8, module_name: []const u8, report: Report) void {
        self.lock.lock();
        defer self.lock.unlock();

        // Lookup entry; auto-register if needed so we can buffer before order is built
        const key = ModuleKey{ .pkg = pkg_name, .module = module_name };
        var entry_index: usize = undefined;
        if (self.index.get(key)) |idx| {
            entry_index = idx;
        } else {
            entry_index = self.entries.items.len;
            const reports_list = std.ArrayList(Report).init(self.gpa);
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

            e.ready = false;
            e.emitted = false;
        }

        self.drain_cursor = i;
        return out;
    }
};

// ===========================
// Tests (integration, tempdirs)
// ===========================

fn writeFile(dir: std.fs.Dir, rel: []const u8, contents: []const u8) !void {
    var f = try dir.createFile(rel, .{ .read = true, .truncate = true, .exclusive = false });
    defer f.close();
    try f.writeAll(contents);
}

test "BuildEnv: app with platform and package shorthands (local-only) succeeds - multi-threaded" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;

    try tmp.dir.makePath("platform");
    try tmp.dir.makePath("foo");
    try tmp.dir.makePath("app");

    // Platform header and module
    try writeFile(tmp.dir, "platform/PlatformMain.roc",
        \\platform "Basic"
        \\requires {} {}
        \\exposes []
        \\packages {}
        \\provides []
    );
    try writeFile(tmp.dir, "platform/Stdout.roc",
        \\module [Stdout]
        \\one = 1
    );

    // Foo package header and module
    try writeFile(tmp.dir, "foo/FooMain.roc",
        \\package []
        \\{ }
    );
    try writeFile(tmp.dir, "foo/Util.roc",
        \\module [Util]
        \\two = 2
    );

    // App Main
    const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "PlatformMain.roc" });
    defer gpa.free(plat_path);
    const foo_path = try std.fs.path.join(gpa, &.{ root_dir, "foo", "FooMain.roc" });
    defer gpa.free(foo_path);

    const app_main = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ cli: platform "{s}", foo: "{s}" }}
        \\
        \\import cli.Stdout
        \\import foo.Util
        \\
        \\main! = |_| Stdout.one + Util.two
    , .{ plat_path, foo_path });
    defer gpa.free(app_main);

    try writeFile(tmp.dir, "app/Main.roc", app_main);

    var ws = BuildEnv.init(gpa, .multi_threaded, 4);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    try ws.buildApp(app_path);

    // Expect no reports
    try std.testing.expectEqual(@as(usize, 0), ws.sink.entries.items.len);
}

// OrderedSink should gate emission to a contiguous ready prefix and drain in order.
test "OrderedSink: prefix gating and drain order" {
    const gpa = std.testing.allocator;
    var sink = OrderedSink.init(gpa);
    defer sink.deinit();

    const pkg_names = [_][]const u8{ "pkg", "pkg" };
    const module_names = [_][]const u8{ "First", "Second" };
    const depths = [_]u32{ 1, 1 };
    try sink.buildOrder(&pkg_names, &module_names, &depths);

    // Emit Second first; should not emit anything yet due to prefix gating (First not ready).
    const r2 = Report.init(gpa, "Second report", .runtime_error);
    sink.emitReport("pkg", "Second", r2);

    const drained0 = try sink.drainEmitted(gpa);
    defer gpa.free(drained0);
    try std.testing.expectEqual(@as(usize, 0), drained0.len);

    // Emit First; now both First and Second should emit (First unlocks the prefix, then Second).
    const r1 = Report.init(gpa, "First report", .runtime_error);
    sink.emitReport("pkg", "First", r1);

    const drained1 = try sink.drainEmitted(gpa);
    defer {
        var i: usize = 0;
        while (i < drained1.len) : (i += 1) {
            for (drained1[i].reports) |*rep| rep.deinit();
            gpa.free(drained1[i].reports);
        }
        gpa.free(drained1);
    }
    try std.testing.expectEqual(@as(usize, 2), drained1.len);
    try std.testing.expect(std.mem.eql(u8, drained1[0].pkg_name, "pkg") and std.mem.eql(u8, drained1[0].module_name, "First"));
    try std.testing.expectEqual(@as(usize, 1), drained1[0].reports.len);
    try std.testing.expect(std.mem.eql(u8, drained1[1].pkg_name, "pkg") and std.mem.eql(u8, drained1[1].module_name, "Second"));
    try std.testing.expectEqual(@as(usize, 1), drained1[1].reports.len);
}

// BuildEnv.drainReports should return absolute paths and group multiple reports per module.
test "BuildEnv: drainReports returns abs paths and aggregates multi-report module" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;
    try tmp.dir.makePath("app");

    // Two errors in a single module A.roc to exercise multi-report aggregation
    try writeFile(tmp.dir, "app/A.roc",
        \\module [A]
        \\bad = "oops" + 1
        \\bad2 = if 42 then 1 else 2
    );

    // App Main imports A to force build
    try writeFile(tmp.dir, "app/Main.roc",
        \\app [main!] {}
        \\
        \\import A
        \\main! = |_| A.bad + 1
    );

    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Build the app and then drain reports
    _ = ws.buildApp(app_path) catch {};
    const drained = try ws.drainReports();
    defer {
        // Deinit drained reports and free abs_path strings
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            for (drained[i].reports) |*rep| rep.deinit();
            gpa.free(drained[i].reports);
            gpa.free(drained[i].abs_path);
        }
        gpa.free(drained);
    }

    // Expect at least one drained item, and at least one for A.roc with >= 2 reports
    try std.testing.expect(drained.len >= 1);

    var saw_a = false;
    var i: usize = 0;
    while (i < drained.len) : (i += 1) {
        const p = drained[i].abs_path;
        // abs_path should be absolute and end with A.roc for the A module
        if (std.mem.endsWith(u8, p, std.fs.path.sep_str ++ "app" ++ std.fs.path.sep_str ++ "A.roc") or
            std.mem.endsWith(u8, p, "A.roc"))
        {
            try std.testing.expect(std.fs.path.isAbsolute(p));
            try std.testing.expect(drained[i].reports.len >= 2);
            saw_a = true;
        }
    }
    try std.testing.expect(saw_a);
}

test "OrderedSink: early emit before order still drains after order build" {
    const gpa = std.testing.allocator;
    var sink = OrderedSink.init(gpa);
    defer sink.deinit();

    // Emit before any order is built; should not drain yet
    const early = Report.init(gpa, "Early", .runtime_error);
    sink.emitReport("pkg", "M", early);

    const drained0 = try sink.drainEmitted(gpa);
    defer gpa.free(drained0);
    try std.testing.expectEqual(@as(usize, 0), drained0.len);

    // Build order with the module
    const pkg_names = [_][]const u8{"pkg"};
    const module_names = [_][]const u8{"M"};
    const depths = [_]u32{1};
    try sink.buildOrder(&pkg_names, &module_names, &depths);

    // Emit again to trigger tryEmitLocked; now drain should return the module
    const later = Report.init(gpa, "Later", .runtime_error);
    sink.emitReport("pkg", "M", later);

    const drained1 = try sink.drainEmitted(gpa);
    defer {
        if (drained1.len > 0) {
            var i: usize = 0;
            while (i < drained1[0].reports.len) : (i += 1) {
                var rep = drained1[0].reports[i];
                rep.deinit();
            }
            gpa.free(drained1[0].reports);
        }
        gpa.free(drained1);
    }
    try std.testing.expectEqual(@as(usize, 1), drained1.len);
    try std.testing.expect(std.mem.eql(u8, drained1[0].pkg_name, "pkg") and std.mem.eql(u8, drained1[0].module_name, "M"));
    try std.testing.expectEqual(@as(usize, 2), drained1[0].reports.len);
}

test "OrderedSink: case-insensitive sort of fq names at same depth" {
    const gpa = std.testing.allocator;
    var sink = OrderedSink.init(gpa);
    defer sink.deinit();

    const pkg_names = [_][]const u8{ "pkg", "pkg" };
    const module_names = [_][]const u8{ "B", "a" };
    const depths = [_]u32{ 0, 0 };
    try sink.buildOrder(&pkg_names, &module_names, &depths);

    const r_b = Report.init(gpa, "B", .runtime_error);
    sink.emitReport("pkg", "B", r_b);
    const r_a = Report.init(gpa, "a", .runtime_error);
    sink.emitReport("pkg", "a", r_a);

    const drained = try sink.drainEmitted(gpa);
    defer {
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            var j: usize = 0;
            while (j < drained[i].reports.len) : (j += 1) {
                var rep = drained[i].reports[j];
                rep.deinit();
            }
            gpa.free(drained[i].reports);
        }
        gpa.free(drained);
    }

    try std.testing.expectEqual(@as(usize, 2), drained.len);
    // Case-insensitive compare should place "a" before "B"
    try std.testing.expect(std.mem.eql(u8, drained[0].pkg_name, "pkg") and std.mem.eql(u8, drained[0].module_name, "a"));
    try std.testing.expect(std.mem.eql(u8, drained[1].pkg_name, "pkg") and std.mem.eql(u8, drained[1].module_name, "B"));
}

test "BuildEnv: same-depth alphabetical ordering across packages via drainReports" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root_dir = tmp.dir.path;

    try tmp.dir.makePath("pkgA");
    try tmp.dir.makePath("pkgB");
    try tmp.dir.makePath("app");

    // Package A (error in A/Bad.roc)
    try writeFile(tmp.dir, "pkgA/AHeader.roc",
        \\package []
        \\{ }
    );
    try writeFile(tmp.dir, "pkgA/Bad.roc",
        \\module [Bad]
        \\bad = "oops" + 1
    );

    // Package B (error in B/Bad.roc)
    try writeFile(tmp.dir, "pkgB/BHeader.roc",
        \\package []
        \\{ }
    );
    try writeFile(tmp.dir, "pkgB/Bad.roc",
        \\module [Bad]
        \\bad = if 42 then 1 else 2
    );

    const a_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgA", "AHeader.roc" });
    defer gpa.free(a_path);
    const b_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgB", "BHeader.roc" });
    defer gpa.free(b_path);

    // App imports both A.Bad and B.Bad so both are depth=1 from app
    const app_main = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ A: "{s}", B: "{s}" }}
        \\
        \\import A.Bad
        \\import B.Bad
        \\
        \\main! = |_| Bad.bad
    , .{ a_path, b_path });
    defer gpa.free(app_main);
    try writeFile(tmp.dir, "app/Main.roc", app_main);

    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    _ = ws.buildApp(app_path) catch {};

    const drained = try ws.drainReports();
    defer {
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            var j: usize = 0;
            while (j < drained[i].reports.len) : (j += 1) {
                var rep = drained[i].reports[j];
                rep.deinit();
            }
            gpa.free(drained[i].reports);
            gpa.free(drained[i].abs_path);
        }
        gpa.free(drained);
    }

    // Find indices of A.Bad and B.Bad and ensure A comes before B at same depth
    var idx_a: ?usize = null;
    var idx_b: ?usize = null;
    var i: usize = 0;
    while (i < drained.len) : (i += 1) {
        const fq = drained[i].fq_name;
        if (std.mem.startsWith(u8, fq, "A:")) idx_a = i;
        if (std.mem.startsWith(u8, fq, "B:")) idx_b = i;
    }

    try std.testing.expect(idx_a != null);
    try std.testing.expect(idx_b != null);
    try std.testing.expect(idx_a.? < idx_b.?);
}

test "BuildEnv: multi-threaded global queue end-to-end builds and drains" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;
    try tmp.dir.makePath("app");

    // Create a trivial module with no errors and import it from Main
    try writeFile(tmp.dir, "app/A.roc",
        \\module [A]
        \\val = 1
    );

    try writeFile(tmp.dir, "app/Main.roc",
        \\app [main!] {}
        \\
        \\import A
        \\main! = |_| A.val
    );

    // Build in multi-threaded mode to exercise the global queue wiring
    var ws = BuildEnv.init(gpa, .multi_threaded, 4);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Build should complete without deadlocks; drain should be callable (expect no reports)
    try ws.buildApp(app_path);
    const drained = try ws.drainReports();
    defer {
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            for (drained[i].reports) |*rep| rep.deinit();
            gpa.free(drained[i].reports);
            gpa.free(drained[i].abs_path);
        }
        gpa.free(drained);
    }
    try std.testing.expectEqual(@as(usize, 0), drained.len);
}

test "BuildEnv: multi-threaded global queue drives all phases" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;
    try tmp.dir.makePath("app");

    // Create a multi-module app with dependencies to test phase progression
    try writeFile(tmp.dir, "app/Main.roc",
        \\app [main!] {}
        \\
        \\import Helper
        \\import Utils
        \\
        \\main! = |_| Helper.value + Utils.compute 10
    );

    try writeFile(tmp.dir, "app/Helper.roc",
        \\module [value]
        \\
        \\import Utils
        \\
        \\value = Utils.compute 5
    );

    try writeFile(tmp.dir, "app/Utils.roc",
        \\module [compute]
        \\
        \\compute = \n -> n * 2
    );

    // Build in multi-threaded mode to exercise the global queue phase progression
    var ws = BuildEnv.init(gpa, .multi_threaded, 4);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Build should complete with all modules progressing through all phases
    try ws.buildApp(app_path);

    // Verify all modules reached Done phase
    const app_build = ws.packages.get("app").?;
    const main_sched = app_build.schedulers.get("app").?;

    main_sched.lock.lock();
    defer main_sched.lock.unlock();

    const main_state = main_sched.modules.get("Main").?;
    const helper_state = main_sched.modules.get("Helper").?;
    const utils_state = main_sched.modules.get("Utils").?;

    const Phase = BuildModuleMod.Phase;
    try std.testing.expectEqual(Phase.Done, main_state.phase);
    try std.testing.expectEqual(Phase.Done, helper_state.phase);
    try std.testing.expectEqual(Phase.Done, utils_state.phase);

    // Drain reports (expect no errors)
    const drained = try ws.drainReports();
    defer {
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            for (drained[i].reports) |*rep| rep.deinit();
            gpa.free(drained[i].reports);
            gpa.free(drained[i].abs_path);
        }
        gpa.free(drained);
    }
    try std.testing.expectEqual(@as(usize, 0), drained.len);
}

test "BuildEnv: multi-threaded concurrency stress test" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;
    try tmp.dir.makePath("app");

    // Create many interconnected modules to stress concurrent processing
    const module_count = 10;
    var i: usize = 0;

    // Create modules that all import each other (except Main)
    while (i < module_count) : (i += 1) {
        const mod_name = try std.fmt.allocPrint(gpa, "Mod{}", .{i});
        defer gpa.free(mod_name);

        var imports = std.ArrayList(u8).init(gpa);
        defer imports.deinit();

        // Each module imports the next two modules (circular style)
        var j: usize = 1;
        while (j <= 2) : (j += 1) {
            const import_idx = (i + j) % module_count;
            try imports.appendSlice("import Mod");
            try std.fmt.formatInt(import_idx, 10, .lower, .{}, imports.writer());
            try imports.append('\n');
        }

        const content = try std.fmt.allocPrint(gpa,
            \\module [value{0}]
            \\
            \\{1s}
            \\value{0} = {0}
        , .{ i, imports.items });
        defer gpa.free(content);

        const filename = try std.fmt.allocPrint(gpa, "app/Mod{}.roc", .{i});
        defer gpa.free(filename);

        try writeFile(tmp.dir, filename, content);
    }

    // Create Main that imports several modules
    try writeFile(tmp.dir, "app/Main.roc",
        \\app [main!] {}
        \\
        \\import Mod0
        \\import Mod1
        \\import Mod2
        \\import Mod3
        \\import Mod4
        \\
        \\main! = |_| Mod0.value0 + Mod1.value1 + Mod2.value2 + Mod3.value3 + Mod4.value4
    );

    // Build with many threads to maximize concurrency
    var ws = BuildEnv.init(gpa, .multi_threaded, 8);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Build should complete without deadlocks or race conditions
    try ws.buildApp(app_path);

    // Verify no duplicate processing occurred by checking phase progression
    const app_build = ws.packages.get("app").?;
    const main_sched = app_build.schedulers.get("app").?;

    main_sched.lock.lock();
    defer main_sched.lock.unlock();

    // All modules should reach Done phase exactly once
    var it = main_sched.modules.iterator();
    while (it.next()) |entry| {
        const state = entry.value_ptr.*;
        try std.testing.expectEqual(BuildModuleMod.Phase.Done, state.phase);
        const working_val = if (@import("builtin").target.cpu.arch != .wasm32) state.working.load(.seq_cst) else state.working;
        try std.testing.expectEqual(@as(u8, 0), working_val);
    }

    // Drain reports (may have cycle errors due to circular imports)
    const drained = try ws.drainReports();
    defer {
        var idx: usize = 0;
        while (idx < drained.len) : (idx += 1) {
            for (drained[idx].reports) |*rep| rep.deinit();
            gpa.free(drained[idx].reports);
            gpa.free(drained[idx].abs_path);
        }
        gpa.free(drained);
    }
}

test "BuildEnv: streaming with module chain" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;
    try tmp.dir.makePath("app");

    // Create a chain of modules A -> B -> C -> D
    try writeFile(tmp.dir, "app/D.roc",
        \\module [valueD]
        \\
        \\valueD = 4
    );

    try writeFile(tmp.dir, "app/C.roc",
        \\module [valueC]
        \\
        \\import D
        \\
        \\valueC = D.valueD + 3
    );

    try writeFile(tmp.dir, "app/B.roc",
        \\module [valueB]
        \\
        \\import C
        \\
        \\valueB = C.valueC + 2
    );

    try writeFile(tmp.dir, "app/A.roc",
        \\module [valueA]
        \\
        \\import B
        \\
        \\valueA = B.valueB + 1
    );

    try writeFile(tmp.dir, "app/Main.roc",
        \\app [main!] {}
        \\
        \\import A
        \\
        \\main! = |_| A.valueA
    );

    // Use single-threaded mode for deterministic testing
    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Build the app normally
    try ws.buildApp(app_path);

    // Should have successfully built all modules
    // Verify by checking that we can drain reports
    const drained = try ws.drainReports();
    defer {
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            for (drained[i].reports) |*rep| rep.deinit();
            gpa.free(drained[i].reports);
            gpa.free(drained[i].abs_path);
        }
        gpa.free(drained);
    }

    // Expect no errors from this simple module chain
    try std.testing.expectEqual(@as(usize, 0), drained.len);
}

test "BuildEnv: app with platform and package shorthands (local-only) succeeds - single-threaded" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;

    try tmp.dir.makePath("platform");
    try tmp.dir.makePath("foo");
    try tmp.dir.makePath("app");

    // Platform header and module
    try writeFile(tmp.dir, "platform/PlatformMain.roc",
        \\platform "Basic"
        \\requires {} {}
        \\exposes []
        \\packages {}
        \\provides []
    );
    try writeFile(tmp.dir, "platform/Stdout.roc",
        \\module [Stdout]
        \\one = 1
    );

    // Foo package header and module
    try writeFile(tmp.dir, "foo/FooMain.roc",
        \\package []
        \\{ }
    );
    try writeFile(tmp.dir, "foo/Util.roc",
        \\module [Util]
        \\two = 2
    );

    // App Main
    const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "PlatformMain.roc" });
    defer gpa.free(plat_path);
    const foo_path = try std.fs.path.join(gpa, &.{ root_dir, "foo", "FooMain.roc" });
    defer gpa.free(foo_path);

    const app_main = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ cli: platform "{s}", foo: "{s}" }}
        \\
        \\import cli.Stdout
        \\import foo.Util
        \\
        \\main! = |_| Stdout.one + Util.two
    , .{ plat_path, foo_path });
    defer gpa.free(app_main);

    try writeFile(tmp.dir, "app/Main.roc", app_main);

    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    try ws.buildApp(app_path);

    // Expect no reports
    try std.testing.expectEqual(@as(usize, 0), ws.sink.entries.items.len);
}

test "BuildEnv: enforce rules (only apps -> platforms, no package -> app)" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root_dir = tmp.dir.path;

    try tmp.dir.makePath("platform");
    try tmp.dir.makePath("pkg");
    try tmp.dir.makePath("app");

    // Platform header
    try writeFile(tmp.dir, "platform/PlatformMain.roc",
        \\platform "P"
        \\requires {} {}
        \\exposes []
        \\packages {}
        \\provides []
    );

    // Package header that incorrectly depends on platform
    const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "PlatformMain.roc" });
    defer gpa.free(plat_path);

    const pkg_header = try std.fmt.allocPrint(gpa,
        \\package []
        \\{{ cli: "{s}" }}
    , .{plat_path});
    defer gpa.free(pkg_header);
    try writeFile(tmp.dir, "pkg/PkgMain.roc", pkg_header);

    // App header depends on pkg (which depends on platform) - allowed for app but pkg->platform should be rejected
    const pkg_path = try std.fs.path.join(gpa, &.{ root_dir, "pkg", "PkgMain.roc" });
    defer gpa.free(pkg_path);

    const app_header = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ pkg: "{s}" }}
        \\
        \\main! = |_| 42
    , .{pkg_path});
    defer gpa.free(app_header);
    try writeFile(tmp.dir, "app/Main.roc", app_header);

    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Building should fail the dependency validation (invalid dependency)
    const res = ws.buildApp(app_path);
    try std.testing.expectError(error.InvalidDependency, res);
    try std.testing.expect(ws.sink.entries.items.len >= 1);
}

test "BuildEnv: app header can reference absolute paths; package/platform sandboxed" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root_dir = tmp.dir.path;

    // Create an external directory outside the workspace root
    var external_dir = try std.fs.cwd().makeOpenPath("external_test_dir", .{});
    defer external_dir.close();
    const external_platform = try std.fs.path.join(gpa, &.{ "external_test_dir", "Plat.roc" });
    defer gpa.free(external_platform);

    // Write platform file outside workspace
    try std.fs.cwd().writeFile(external_platform, "platform \"X\"\\nrequires {} {}\\nexposes []\\npackages {}\\nprovides []\\n");

    // App can reference absolute external path for platform (should be allowed by policy)
    const app_header = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ cli: platform "{s}" }}
        \\
        \\main! = |_| 0
    , .{external_platform});
    defer gpa.free(app_header);

    try tmp.dir.makePath("app");
    try writeFile(tmp.dir, "app/Main.roc", app_header);

    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Because we sandbox platform dependencies even when declared by app, this should error PathOutsideWorkspace
    const res = ws.buildApp(app_path);
    try std.testing.expectError(error.PathOutsideWorkspace, res);
}

test "BuildEnv: package header cannot reference paths outside workspace" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root_dir = tmp.dir.path;

    // Exterior dir and file
    var ext_dir = try std.fs.cwd().makeOpenPath("pkg_external_dir", .{});
    defer ext_dir.close();
    const ext_pkg = try std.fs.path.join(gpa, &.{ "pkg_external_dir", "Pkg.roc" });
    defer gpa.free(ext_pkg);
    try std.fs.cwd().writeFile(ext_pkg, "package []\\n{ }\\n");

    try tmp.dir.makePath("app");
    // App points to a local platform so sandbox roots are the app dir
    try tmp.dir.makePath("platform");
    try writeFile(tmp.dir, "platform/P.roc", "platform \"P\"\\nrequires {} {}\\nexposes []\\npackages {}\\nprovides []\\n");

    // Package header attempts to import external path
    try tmp.dir.makePath("pkg");
    const pkg_header = try std.fmt.allocPrint(gpa,
        \\package []
        \\{{ ext: "{s}" }}
    , .{ext_pkg});
    defer gpa.free(pkg_header);
    try writeFile(tmp.dir, "pkg/Main.roc", pkg_header);

    // App imports pkg (which points outside) - should trigger PathOutsideWorkspace
    const pkg_path = try std.fs.path.join(gpa, &.{ root_dir, "pkg", "Main.roc" });
    defer gpa.free(pkg_path);
    const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "P.roc" });
    defer gpa.free(plat_path);

    const app_header = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ pf: platform "{s}", pkg: "{s}" }}
        \\
        \\import pkg.Foo
        \\
        \\main! = |_| 0
    , .{ plat_path, pkg_path });
    defer gpa.free(app_header);
    try writeFile(tmp.dir, "app/Main.roc", app_header);

    var ws = BuildEnv.init(gpa, .single_threaded, 1);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    const res = ws.buildApp(app_path);
    try std.testing.expectError(error.PathOutsideWorkspace, res);
}

test "BuildEnv: deterministic error ordering across packages" {
    const gpa = std.testing.allocator;
    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root_dir = tmp.dir.path;

    try tmp.dir.makePath("pkgA");
    try tmp.dir.makePath("pkgB");
    try tmp.dir.makePath("app");

    // Package A header and an error in module A/Bad.roc (depth 1)
    try writeFile(tmp.dir, "pkgA/AHeader.roc",
        \\package []
        \\{ }
    );
    try writeFile(tmp.dir, "pkgA/Bad.roc",
        \\module [Bad]
        \\bad = "oops" + 1
    );

    // Package B header and an error in module B/Deep/Bad.roc (depth 2 via app -> B -> Deep.Bad)
    try writeFile(tmp.dir, "pkgB/BHeader.roc",
        \\package []
        \\{ }
    );
    try tmp.dir.makePath("pkgB/Deep");
    try writeFile(tmp.dir, "pkgB/Deep/Bad.roc",
        \\module [Deep.Bad]
        \\bad = if 42 then 1 else 2
    );

    // App header depends on A and B, and imports their modules to force build.
    const a_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgA", "AHeader.roc" });
    defer gpa.free(a_path);
    const b_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgB", "BHeader.roc" });
    defer gpa.free(b_path);

    const app_main = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ A: "{s}", B: "{s}" }}
        \\
        \\import A.Bad
        \\import B.Deep.Bad
        \\
        \\main! = |_| Bad.bad + Deep.Bad.bad
    , .{ a_path, b_path });
    defer gpa.free(app_main);
    try writeFile(tmp.dir, "app/Main.roc", app_main);

    var ws = BuildEnv.init(gpa, .multi_threaded, 4);
    defer ws.deinit();

    const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
    defer gpa.free(app_path);

    // Build workspace; expect errors from A.Bad (depth 1) and then B.Deep.Bad (depth 2)
    _ = ws.buildApp(app_path) catch {};

    // Ensure we have at least two reports collected
    try std.testing.expect(ws.sink.entries.items.len >= 2);

    // Verify module names appear in deterministic order by (depth, then name)
    // Depths: A.Bad at depth=1 should appear before B.Deep.Bad at depth=2
    // Note: The aggregated sink stores module names in emission order.
    const first_mod = ws.sink.entries.items[0].fq_name;
    const second_mod = ws.sink.entries.items[1].fq_name;

    try std.testing.expect(std.mem.eql(u8, first_mod, "Bad") or std.mem.eql(u8, first_mod, "A.Bad"));
    // The second module must be the deeper one (B.Deep.Bad)
    // Accept either "Deep.Bad" or "B.Deep.Bad" depending on how names are interned
    try std.testing.expect(std.mem.endsWith(u8, second_mod, "Deep.Bad") or std.mem.eql(u8, second_mod, "B.Deep.Bad"));
}

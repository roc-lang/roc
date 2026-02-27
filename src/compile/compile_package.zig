//! Module build scheduler for a single package.
//!
//! This component manages the concurrent compilation of all modules within a single package,
//! orchestrating the build phases for each module:
//!
//! - Parsing modules to discover their import dependencies
//! - Canonicalizing parsed modules into an intermediate representation
//! - Type-checking modules once their dependencies are ready
//! - Coordinating with BuildEnv's global work queue for cross-package dependencies
//! - Reporting diagnostics through a deterministic sink
//!
//! The scheduler tracks each module's progress through build phases and ensures proper
//! ordering based on import dependencies. It supports both single-threaded and
//! multi-threaded execution modes.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const reporting = @import("reporting");
const eval = @import("eval");
const builtin_loading = eval.builtin_loading;
const compiled_builtins = @import("compiled_builtins");
const build_options = @import("build_options");

// Compile-time flag for build tracing - enabled via `zig build -Dtrace-build`
const trace_build = if (@hasDecl(build_options, "trace_build")) build_options.trace_build else false;
const BuiltinTypes = eval.BuiltinTypes;
const BuiltinModules = eval.BuiltinModules;
const module_discovery = @import("module_discovery.zig");
const roc_target = @import("roc_target");

const Check = check.Check;
const Can = can.Can;
const Report = reporting.Report;
const ModuleEnv = can.ModuleEnv;
const ReportBuilder = check.ReportBuilder;
const AST = parse.AST;

/// Deserialize BuiltinIndices from the binary data generated at build time
/// Timing information for different phases
pub const TimingInfo = struct {
    tokenize_parse_ns: u64,
    canonicalize_ns: u64,
    canonicalize_diagnostics_ns: u64,
    type_checking_ns: u64,
    check_diagnostics_ns: u64,
};
const Allocator = std.mem.Allocator;

const parallel = base.parallel;
const AtomicUsize = std.atomic.Value(usize);
const Mutex = std.Thread.Mutex;
const Condition = std.Thread.Condition;

/// File provider for reading module sources.
/// Implementations must be thread-safe (stateless reads) as they may be called
/// concurrently from multiple worker threads.
pub const FileProvider = struct {
    ctx: ?*anyopaque,
    read: *const fn (ctx: ?*anyopaque, path: []const u8, gpa: Allocator) Allocator.Error!?[]u8,

    /// Default filesystem provider - reads files directly from the filesystem.
    /// Thread-safe as std.fs operations are independent per call.
    pub const filesystem = FileProvider{
        .ctx = null,
        .read = filesystemRead,
    };

    /// Check if a file exists by attempting to read it through the provider.
    /// This ensures virtual/in-memory files are found the same way as disk files.
    pub fn fileExists(self: FileProvider, path: []const u8, gpa: Allocator) bool {
        const data = self.read(self.ctx, path, gpa) catch return false;
        if (data) |d| {
            gpa.free(d);
            return true;
        }
        return false;
    }

    fn filesystemRead(_: ?*anyopaque, path: []const u8, gpa: Allocator) Allocator.Error!?[]u8 {
        return std.fs.cwd().readFileAlloc(gpa, path, std.math.maxInt(usize)) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return null, // File not found or other IO error
        };
    }
};

/// Build execution mode
pub const Mode = enum { single_threaded, multi_threaded };

/// Destination for reports (can be stdout, memory buffer, etc.)
pub const ReportSink = struct {
    ctx: ?*anyopaque,
    emitFn: *const fn (ctx: ?*anyopaque, module_name: []const u8, report: Report) void,
};

/// Optional scheduling hook for observability/integration (e.g. global work-stealing)
pub const ScheduleHook = struct {
    ctx: ?*anyopaque,
    onSchedule: *const fn (ctx: ?*anyopaque, package_name: []const u8, module_name: []const u8, path: []const u8, depth: u32) void,

    /// A no-op hook for testing or when no external scheduling is needed
    pub fn noOp() ScheduleHook {
        return .{ .ctx = null, .onSchedule = noOpSchedule };
    }

    fn noOpSchedule(_: ?*anyopaque, _: []const u8, _: []const u8, _: []const u8, _: u32) void {}

    pub fn isNoOp(self: ScheduleHook) bool {
        return self.onSchedule == noOpSchedule;
    }
};

/// Resolver for handling imports across package boundaries
pub const ImportResolver = struct {
    ctx: ?*anyopaque,
    /// Ensure the external import is scheduled for building in its owning package
    scheduleExternal: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) void,
    /// Return true if the external import is fully type-checked and its ModuleEnv is ready
    isReady: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) bool,
    /// Get a pointer to the external ModuleEnv once ready (null if not ready)
    getEnv: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) ?*ModuleEnv,
    /// Resolve a local module import to a filesystem path within the current package
    resolveLocalPath: *const fn (ctx: ?*anyopaque, current_package: []const u8, root_dir: []const u8, import_name: []const u8) []const u8,
};

/// Module identifier - index into the modules list
const ModuleId = u32;

const Task = struct { module_id: ModuleId };

const Phase = enum { Parse, Canonicalize, WaitingOnImports, TypeCheck, Done };

const ModuleState = struct {
    name: []const u8, // Module name is needed for error reporting and the schedule hook
    path: []const u8,
    env: ?ModuleEnv = null,
    phase: Phase = .Parse,
    imports: std.ArrayList(ModuleId),
    /// External imports qualified via package shorthand (e.g. "cli.Stdout") - still strings as they reference other packages
    external_imports: std.ArrayList([]const u8),
    dependents: std.ArrayList(ModuleId),
    reports: std.ArrayList(Report),
    depth: u32 = std.math.maxInt(u32), // min depth from root
    /// DFS visitation color for cycle detection: 0=white (unvisited), 1=gray (visiting), 2=black (finished)
    visit_color: u8 = 0,
    /// Atomic flag to prevent concurrent processing of the same module (0=free, 1=working)
    working: if (@import("builtin").target.cpu.arch != .wasm32) std.atomic.Value(u8) else u8 = if (@import("builtin").target.cpu.arch != .wasm32) std.atomic.Value(u8).init(0) else 0,
    /// Cached AST from parsing phase - heap-allocated to avoid copy issues with ArrayLists
    cached_ast: ?*parse.AST = null,
    /// True if this module was loaded from cache. Cached modules have their env memory
    /// owned by the cache buffer, so we must NOT call env.deinit() for them.
    was_from_cache: bool = false,

    fn deinit(self: *ModuleState, gpa: Allocator) void {
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: checking cached_ast\n", .{self.name});
        }
        // Free cached AST if present
        if (self.cached_ast) |ast| {
            ast.deinit();
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: getting source ptr (was_from_cache={})\n", .{ self.name, self.was_from_cache });
        }

        // For cached modules:
        // - Call deinitCachedModule() to free only heap-allocated hash maps
        // - The cache buffer is freed separately via cache_buffers cleanup
        // - STILL free the source - it's heap-allocated separately, not part of the cache buffer
        //
        // For non-cached modules:
        // - Call full env.deinit() to free all allocations
        // - Free the source which was heap-allocated
        if (!self.was_from_cache) {
            if (self.env) |*e| {
                // IMPORTANT: Use e.gpa, not the passed-in gpa, because source was allocated
                // with e.gpa (page_allocator in multi-threaded mode).
                const env_alloc = e.gpa;
                const source = e.common.source;
                if (comptime trace_build) {
                    std.debug.print("[MOD DEINIT DETAIL] {s}: source={}, calling env.deinit\n", .{ self.name, @intFromPtr(source.ptr) });
                }
                e.deinit();
                if (comptime trace_build) {
                    std.debug.print("[MOD DEINIT DETAIL] {s}: freeing source\n", .{self.name});
                }
                if (source.len > 0) env_alloc.free(source);
            }
        } else {
            if (self.env) |*e| {
                if (comptime trace_build) {
                    std.debug.print("[MOD DEINIT DETAIL] {s}: calling env.deinitCachedModule (heap-allocated hash maps only)\n", .{self.name});
                }
                // IMPORTANT: Use e.gpa, not the passed-in gpa, because source was allocated
                // with e.gpa (page_allocator in multi-threaded mode).
                const env_alloc = e.gpa;
                // The source is heap-allocated separately (read from file), not part of the cache buffer.
                // We need to free it even for cached modules.
                const source = e.common.source;
                e.deinitCachedModule();
                if (comptime trace_build) {
                    std.debug.print("[MOD DEINIT DETAIL] {s}: freeing source for cached module\n", .{self.name});
                }
                if (source.len > 0) env_alloc.free(source);
            }
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing imports (len={})\n", .{ self.name, self.imports.items.len });
        }
        self.imports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing external_imports (len={})\n", .{ self.name, self.external_imports.items.len });
        }
        self.external_imports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing dependents (len={})\n", .{ self.name, self.dependents.items.len });
        }
        self.dependents.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing reports (len={}, cap={})\n", .{ self.name, self.reports.items.len, self.reports.capacity });
        }
        // If reports were emitted, they've been cleared via clearRetainingCapacity() and ownership
        // transferred to OrderedSink. If not emitted (e.g., module failed before .Done), we must
        // deinit them here to avoid leaks.
        for (self.reports.items, 0..) |*report, i| {
            if (comptime trace_build) {
                std.debug.print("[MOD DEINIT DETAIL] {s}: deinit report {} title=\"{s}\" owned_strings.len={} doc_elements={} allocator_ptr={}\n", .{
                    self.name,
                    i,
                    report.title,
                    report.owned_strings.items.len,
                    report.document.elements.items.len,
                    @intFromPtr(report.allocator.ptr),
                });
                if (report.owned_strings.items.len > 0) {
                    std.debug.print("[MOD DEINIT DETAIL] {s}: first owned_string ptr={} len={}\n", .{
                        self.name,
                        @intFromPtr(report.owned_strings.items[0].ptr),
                        report.owned_strings.items[0].len,
                    });
                }
            }
            report.deinit();
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: calling reports.deinit\n", .{self.name});
        }
        self.reports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing path\n", .{self.name});
        }
        gpa.free(self.path);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: done\n", .{self.name});
        }
    }

    fn init(name: []const u8, path: []const u8) ModuleState {
        return .{
            .name = name,
            .path = path,
            .imports = std.ArrayList(ModuleId).empty,
            .external_imports = std.ArrayList([]const u8).empty,
            .dependents = std.ArrayList(ModuleId).empty,
            .reports = std.ArrayList(Report).empty,
        };
    }
};

/// Per-package module build orchestrator
pub const PackageEnv = struct {
    gpa: Allocator,
    /// Name of the current package (used for shorthand resolution)
    package_name: []const u8,
    root_dir: []const u8,
    mode: Mode,
    max_threads: usize,
    target: roc_target.RocTarget,
    sink: ReportSink,
    /// Optional resolver for cross-package imports; when null, all imports are treated as local
    resolver: ?ImportResolver = null,
    /// Scheduling hook to observe/enqueue scheduled modules in a global orchestrator
    schedule_hook: ScheduleHook,
    /// Compiler version for cache invalidation
    compiler_version: []const u8,
    /// Builtin modules (Bool, Try, Str) for auto-importing in canonicalization (not owned)
    builtin_modules: *const BuiltinModules,
    /// File provider for reading sources (defaults to filesystem)
    file_provider: FileProvider = FileProvider.filesystem,

    lock: Mutex = .{},
    cond: Condition = .{},

    // Work queue
    injector: std.ArrayList(Task),

    // Module storage
    modules: std.ArrayList(ModuleState),
    // String intern table: module name -> module ID
    module_names: std.StringHashMapUnmanaged(ModuleId) = .{},

    // Build status
    remaining_modules: usize = 0,
    /// ID of the root module (the module passed to buildRoot)
    root_module_id: ?ModuleId = null,

    // Track module discovery order and which modules have had their reports emitted
    discovered: std.ArrayList(ModuleId),
    emitted: std.bit_set.DynamicBitSetUnmanaged = .{},

    // Timing collection (accumulated across all modules)
    total_tokenize_parse_ns: u64 = 0,
    total_canonicalize_ns: u64 = 0,
    total_canonicalize_diagnostics_ns: u64 = 0,
    total_type_checking_ns: u64 = 0,
    total_check_diagnostics_ns: u64 = 0,

    // Additional known modules (e.g., from platform exposes) to include in module_envs_map
    // These are modules that exist in external directories (like URL platform cache)
    additional_known_modules: std.ArrayList(KnownModule),

    /// Info about a known module from a platform or other package
    pub const KnownModule = struct {
        /// Qualified module name (e.g., "pf.Stdout")
        qualified_name: []const u8,
        /// Import name for resolver lookup (e.g., "pf.Stdout")
        import_name: []const u8,
    };

    pub fn init(gpa: Allocator, package_name: []const u8, root_dir: []const u8, mode: Mode, max_threads: usize, target: roc_target.RocTarget, sink: ReportSink, schedule_hook: ScheduleHook, compiler_version: []const u8, builtin_modules: *const BuiltinModules, file_provider: ?FileProvider) PackageEnv {
        // Pre-allocate module storage to avoid reallocation during multi-threaded processing
        var modules = std.ArrayList(ModuleState).empty;
        if (mode == .multi_threaded) {
            modules.ensureTotalCapacity(gpa, 256) catch {};
        }
        return .{
            .gpa = gpa,
            .package_name = package_name,
            .root_dir = root_dir,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .sink = sink,
            .schedule_hook = schedule_hook,
            .compiler_version = compiler_version,
            .builtin_modules = builtin_modules,
            .file_provider = file_provider orelse FileProvider.filesystem,
            .injector = std.ArrayList(Task).empty,
            .modules = modules,
            .discovered = std.ArrayList(ModuleId).empty,
            .additional_known_modules = std.ArrayList(KnownModule).empty,
        };
    }

    pub fn initWithResolver(
        gpa: Allocator,
        package_name: []const u8,
        root_dir: []const u8,
        mode: Mode,
        max_threads: usize,
        target: roc_target.RocTarget,
        sink: ReportSink,
        resolver: ImportResolver,
        schedule_hook: ScheduleHook,
        compiler_version: []const u8,
        builtin_modules: *const BuiltinModules,
        file_provider: ?FileProvider,
    ) PackageEnv {
        // Pre-allocate module storage to avoid reallocation during multi-threaded processing
        var modules = std.ArrayList(ModuleState).empty;
        if (mode == .multi_threaded) {
            modules.ensureTotalCapacity(gpa, 256) catch {};
        }
        return .{
            .gpa = gpa,
            .package_name = package_name,
            .root_dir = root_dir,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .sink = sink,
            .resolver = resolver,
            .schedule_hook = schedule_hook,
            .compiler_version = compiler_version,
            .builtin_modules = builtin_modules,
            .file_provider = file_provider orelse FileProvider.filesystem,
            .injector = std.ArrayList(Task).empty,
            .modules = modules,
            .discovered = std.ArrayList(ModuleId).empty,
            .additional_known_modules = std.ArrayList(KnownModule).empty,
        };
    }

    /// Add a module that should be recognized during canonicalization.
    /// This is used for platform-exposed modules in URL platforms where the
    /// modules exist in a cache directory, not the app's directory.
    /// `qualified_name` is the full name like "pf.Stdout"
    /// `import_name` is the import path for resolver lookup (e.g., "pf.Stdout")
    pub fn addKnownModule(self: *PackageEnv, qualified_name: []const u8, import_name: []const u8) !void {
        const qualified_copy = try self.gpa.dupe(u8, qualified_name);
        const import_copy = try self.gpa.dupe(u8, import_name);
        try self.additional_known_modules.append(self.gpa, .{
            .qualified_name = qualified_copy,
            .import_name = import_copy,
        });
    }

    pub fn deinit(self: *PackageEnv) void {
        // NOTE: builtin_modules is not owned by PackageEnv, so we don't deinit it here

        if (comptime trace_build) {
            std.debug.print("[SCHED DEINIT] {s}: starting with {} modules\n", .{ self.package_name, self.modules.items.len });
        }

        // Deinit modules
        for (self.modules.items, 0..) |*ms, idx| {
            if (comptime trace_build) {
                std.debug.print("[SCHED DEINIT] {s}: module {} ({s}) env={} ast={}\n", .{
                    self.package_name,
                    idx,
                    ms.name,
                    @intFromPtr(if (ms.env) |*e| e else null),
                    @intFromPtr(ms.cached_ast),
                });
            }
            ms.deinit(self.gpa);
            if (comptime trace_build) {
                std.debug.print("[SCHED DEINIT] {s}: module {} done\n", .{ self.package_name, idx });
            }
        }
        self.modules.deinit(self.gpa);

        // Free interned strings
        var it = self.module_names.iterator();
        while (it.next()) |e| {
            self.gpa.free(e.key_ptr.*);
        }
        self.module_names.deinit(self.gpa);

        self.injector.deinit(self.gpa);
        self.discovered.deinit(self.gpa);
        self.emitted.deinit(self.gpa);

        // Free additional known module names
        for (self.additional_known_modules.items) |km| {
            self.gpa.free(km.qualified_name);
            self.gpa.free(km.import_name);
        }
        self.additional_known_modules.deinit(self.gpa);
    }

    /// Get the root module's env (the module passed to buildRoot)
    pub fn getRootEnv(self: *PackageEnv) ?*ModuleEnv {
        const root_id = self.root_module_id orelse return null;
        if (root_id >= self.modules.items.len) return null;
        return if (self.modules.items[root_id].env) |*env| env else null;
    }

    /// Get the root module state (the module passed to buildRoot)
    pub fn getRootModule(self: *PackageEnv) ?*ModuleState {
        const root_id = self.root_module_id orelse return null;
        if (root_id >= self.modules.items.len) return null;
        return &self.modules.items[root_id];
    }

    fn internModuleName(self: *PackageEnv, name: []const u8) !ModuleId {
        const gop = try self.module_names.getOrPut(self.gpa, name);
        if (!gop.found_existing) {
            const id: ModuleId = @intCast(self.modules.items.len);
            const owned_name = try self.gpa.dupe(u8, name);
            gop.key_ptr.* = owned_name;
            gop.value_ptr.* = id;
            return id;
        }
        return gop.value_ptr.*;
    }

    pub fn buildRoot(self: *PackageEnv, root_file_path: []const u8) !void {
        const name = moduleNameFromPath(root_file_path);
        const prev_module_count = self.modules.items.len;
        const module_id = try self.ensureModule(name, root_file_path);
        // Track which module is the root (for getRootEnv)
        self.root_module_id = module_id;
        // root depth = 0
        try self.setDepthIfSmaller(module_id, 0);
        // Only increment remaining_modules if the root module is new (wasn't already scheduled)
        // This can happen when external imports schedule modules via scheduleModule before buildRoot is called
        if (module_id >= prev_module_count) {
            self.remaining_modules += 1;
        }
        try self.enqueue(module_id);

        // Note: enqueue() already calls schedule_hook in multi-threaded mode,
        // so we don't call it again here to avoid duplicate enqueues

        // If a global schedule_hook is installed AND we're in multi-threaded mode,
        // the unified global queue will drive processing via process().
        // In single-threaded mode, we always need to run our local processing loop.
        const should_run_local = self.schedule_hook.isNoOp() or self.mode == .single_threaded;
        if (should_run_local) {
            if (@import("builtin").target.cpu.arch == .wasm32) {
                // On wasm32, always run single-threaded at comptime
                try self.runSingleThread();
            } else {
                switch (self.mode) {
                    .single_threaded => try self.runSingleThread(),
                    .multi_threaded => try self.runMultiThread(),
                }
            }
        }

        // Emit any remaining reports deterministically
        try self.tryEmitReady();
    }

    fn runSingleThread(self: *PackageEnv) !void {
        while (true) {
            if (self.injector.items.len > 0) {
                const idx = self.injector.items.len - 1;
                const task = self.injector.items[idx];
                self.injector.items.len = idx;
                try self.process(task);
                try self.tryEmitReady();
            } else if (self.remaining_modules == 0) {
                break;
            } else {
                break; // This should not happen - stuck state
            }
        }
    }

    fn runMultiThread(self: *PackageEnv) !void {
        const options = parallel.ProcessOptions{
            .max_threads = if (self.max_threads == 0) (std.Thread.getCpuCount() catch 1) else self.max_threads,
            .use_per_thread_arenas = false,
        };

        var index = AtomicUsize.init(0);
        while (true) {
            const work_len = self.injector.items.len;
            if (work_len == 0) {
                if (self.remaining_modules == 0) break;
                self.lock.lock();
                defer self.lock.unlock();
                if (self.remaining_modules == 0 and self.injector.items.len == 0) break;
                _ = self.cond.timedWait(&self.lock, 1_000_000) catch {};
                continue;
            }

            index.store(0, .monotonic);
            var ctx = WorkerCtx{ .sched = self, .index = &index, .work_len = work_len };
            try parallel.process(WorkerCtx, &ctx, workerFn, self.gpa, work_len, options);
            try self.tryEmitReady();
        }
    }

    const WorkerCtx = struct { sched: *PackageEnv, index: *AtomicUsize, work_len: usize };

    fn workerFn(_: Allocator, ctx: *WorkerCtx, _: usize) void {
        while (true) {
            const i = ctx.index.fetchAdd(1, .monotonic);
            if (i >= ctx.work_len) break;
            const task = ctx.sched.injector.items[i];
            _ = ctx.sched.process(task) catch {};
        }
        // Compact processed prefix once under lock
        ctx.sched.lock.lock();
        defer ctx.sched.lock.unlock();
        const len = ctx.sched.injector.items.len;
        if (ctx.work_len <= len) {
            std.mem.copyBackwards(Task, ctx.sched.injector.items[0 .. len - ctx.work_len], ctx.sched.injector.items[ctx.work_len..len]);
            ctx.sched.injector.items.len = len - ctx.work_len;
        } else ctx.sched.injector.items.len = 0;
    }

    pub fn ensureModule(self: *PackageEnv, name: []const u8, path: []const u8) !ModuleId {
        // In multi-threaded mode, lock to prevent race conditions when growing arrays
        const needs_lock = self.mode == .multi_threaded and @import("builtin").target.cpu.arch != .wasm32;
        if (needs_lock) self.lock.lock();
        defer if (needs_lock) self.lock.unlock();

        const module_id = try self.internModuleName(name);

        // Check if we need to create a new module
        if (module_id >= self.modules.items.len) {
            // This is a new module
            const owned_path = try self.gpa.dupe(u8, path);
            const owned_name = self.module_names.getKey(name).?; // We just interned it
            try self.modules.append(self.gpa, ModuleState.init(owned_name, owned_path));
            try self.discovered.append(self.gpa, module_id);

            // Note: Don't call schedule_hook here - let scheduleModule/enqueue handle it
            // to avoid duplicate enqueues in multi-threaded mode
        }

        return module_id;
    }

    /// Public API for cross-package schedulers: ensure a module exists, set its depth, and enqueue it
    pub fn scheduleModule(self: *PackageEnv, name: []const u8, path: []const u8, depth: u32) !void {
        const prev_module_count = self.modules.items.len;
        const module_id = try self.ensureModule(name, path);
        const is_new = module_id >= prev_module_count;
        try self.setDepthIfSmaller(module_id, depth);
        if (is_new) {
            self.remaining_modules += 1;
            // Note: schedule_hook is called by enqueue() in multi-threaded mode,
            // so we don't call it here to avoid duplicate enqueues
        }
        try self.enqueue(module_id);
    }

    fn setDepthIfSmaller(self: *PackageEnv, module_id: ModuleId, depth: u32) !void {
        const st = &self.modules.items[module_id];
        if (depth < st.depth) st.depth = depth;
    }

    /// Public API to adjust a module's depth from an external coordinator
    pub fn setModuleDepthIfSmaller(self: *PackageEnv, name: []const u8, depth: u32) !void {
        if (self.module_names.get(name)) |module_id| {
            try self.setDepthIfSmaller(module_id, depth);
        }
    }

    fn enqueue(self: *PackageEnv, module_id: ModuleId) !void {
        const st = &self.modules.items[module_id];
        // In multi_threaded mode with a non-noop schedule_hook, forward to the global queue
        if (self.mode == .multi_threaded and !self.schedule_hook.isNoOp()) {
            // Look up the module to get its path and depth for the hook
            self.lock.lock();
            defer self.lock.unlock();

            self.schedule_hook.onSchedule(self.schedule_hook.ctx, self.package_name, st.name, st.path, st.depth);
        } else {
            // Default behavior: use internal injector
            try self.injector.append(self.gpa, .{ .module_id = module_id });
            if (@import("builtin").target.cpu.arch != .wasm32) self.cond.signal();
        }
    }

    /// Public API to obtain a module's environment if it has completed type-checking
    pub fn getEnvIfDone(self: *PackageEnv, name: []const u8) ?*ModuleEnv {
        if (self.module_names.get(name)) |module_id| {
            const st = &self.modules.items[module_id];
            if (st.phase == .Done) {
                if (st.env) |*e| {
                    return e;
                }
            }
        }
        return null;
    }

    /// Get accumulated timing information
    pub fn getTimingInfo(self: *PackageEnv) TimingInfo {
        return TimingInfo{
            .tokenize_parse_ns = self.total_tokenize_parse_ns,
            .canonicalize_ns = self.total_canonicalize_ns,
            .canonicalize_diagnostics_ns = self.total_canonicalize_diagnostics_ns,
            .type_checking_ns = self.total_type_checking_ns,
            .check_diagnostics_ns = self.total_check_diagnostics_ns,
        };
    }

    /// Public API to read a module's current recorded dependency depth
    pub fn getModuleDepth(self: *PackageEnv, name: []const u8) ?u32 {
        if (self.module_names.get(name)) |module_id| {
            return self.modules.items[module_id].depth;
        }
        return null;
    }

    /// Public API to check if a module exists by name
    pub fn hasModule(self: *PackageEnv, module_name: []const u8) bool {
        return self.module_names.contains(module_name);
    }

    /// Public API to iterate over module names (for BuildEnv compatibility)
    pub fn moduleNamesIterator(self: *PackageEnv) std.StringHashMapUnmanaged(ModuleId).Iterator {
        return self.module_names.iterator();
    }

    /// Public API to get module state by name (for BuildEnv compatibility)
    pub fn getModuleState(self: *PackageEnv, module_name: []const u8) ?*ModuleState {
        if (self.module_names.get(module_name)) |module_id| {
            return &self.modules.items[module_id];
        }
        return null;
    }

    /// Public API to find a module by its filesystem path.
    /// Returns the ModuleState if found, null otherwise.
    pub fn findModuleByPath(self: *PackageEnv, path: []const u8) ?*ModuleState {
        for (self.modules.items) |*module| {
            if (std.mem.eql(u8, module.path, path)) {
                return module;
            }
        }
        return null;
    }

    /// Public API for processing a module by name (used by BuildEnv)
    pub fn processModuleByName(self: *PackageEnv, module_name: []const u8) !void {
        if (self.module_names.get(module_name)) |module_id| {
            try self.process(.{ .module_id = module_id });
        }
    }

    pub fn process(self: *PackageEnv, task: Task) !void {
        // In dispatch-only mode, this method is invoked by the global scheduler.
        // In local mode, it's invoked by the internal run* loops.

        // Acquire lock and atomically check/set working flag
        if (@import("builtin").target.cpu.arch != .wasm32) self.lock.lock();
        const st = &self.modules.items[task.module_id];

        // Atomic compare-and-swap to claim work on this module
        const already_working = if (@import("builtin").target.cpu.arch != .wasm32) blk: {
            // For multi-threaded: use atomic CAS to claim the module (0 -> 1)
            const result = st.working.cmpxchgWeak(0, 1, .seq_cst, .seq_cst);
            break :blk result != null; // null means swap succeeded
        } else blk: {
            // For single-threaded/wasm: simple check and set
            const was_working = st.working != 0;
            if (!was_working) st.working = 1;
            break :blk was_working;
        };

        if (already_working) {
            if (@import("builtin").target.cpu.arch != .wasm32) self.lock.unlock();
            return; // Another worker is already processing this module
        }

        // Snapshot phase while holding lock
        const phase = st.phase;
        if (@import("builtin").target.cpu.arch != .wasm32) self.lock.unlock();

        // Process the module based on its phase
        defer {
            // Atomically clear working flag when done
            if (@import("builtin").target.cpu.arch != .wasm32) {
                self.lock.lock();
                if (task.module_id < self.modules.items.len) {
                    _ = self.modules.items[task.module_id].working.store(0, .seq_cst);
                }
                self.lock.unlock();
            } else {
                // Single-threaded: simple clear
                if (task.module_id < self.modules.items.len) {
                    self.modules.items[task.module_id].working = 0;
                }
            }
        }

        switch (phase) {
            .Parse => {
                try self.doParse(task.module_id);
            },
            .Canonicalize => {
                try self.doCanonicalize(task.module_id);
            },
            .WaitingOnImports => {
                try self.tryUnblock(task.module_id);
            },
            .TypeCheck => {
                try self.doTypeCheck(task.module_id);
            },
            .Done => {
                return;
            },
        }
        try self.tryEmitReady();
    }

    fn doParse(self: *PackageEnv, module_id: ModuleId) !void {
        // Load source and init ModuleEnv
        var st = &self.modules.items[module_id];
        const src = self.readModuleSource(st.path) catch |read_err| {
            // Note: Let the FileNotFound error propagate naturally
            // The existing error handling will report it appropriately
            return read_err;
        };

        // line starts for diagnostics and consistent positions

        var env = try ModuleEnv.init(self.gpa, src);
        // init CIR fields
        try env.initCIRFields(st.name);

        try env.common.calcLineStarts(self.gpa);

        // replace env - save old source to free it after deinit
        const old_source = if (st.env) |*old| old.common.source else null;
        if (st.env) |*old| old.deinit();
        if (old_source) |s| self.gpa.free(s);
        st.env = env;

        // Parse AST and cache for reuse in doCanonicalize (avoids double parsing)
        // IMPORTANT: Use st.env.?.common (not local env.common) so the AST's pointer
        // to CommonEnv remains valid after this function returns.
        var allocators: base.Allocators = undefined;
        allocators.initInPlace(self.gpa);
        // NOTE: allocators is not freed here - cleanup happens in doCanonicalize
        const parse_ast = parse.parse(&allocators, &st.env.?.common) catch {
            // If parsing fails, proceed to canonicalization to report errors
            if (comptime trace_build) {
                std.debug.print("[TRACE-CACHE] PHASE: {s} Parse->Canonicalize (parse error)\n", .{st.name});
            }
            st.phase = .Canonicalize;
            try self.enqueue(module_id);
            return;
        };
        parse_ast.store.emptyScratch();

        // parse_ast is already heap-allocated by parse.parse
        st.cached_ast = parse_ast;

        // Go directly to Canonicalize - sibling discovery happens after canonicalization
        // based on ModuleEnv.imports
        if (comptime trace_build) {
            std.debug.print("[TRACE-CACHE] PHASE: {s} Parse->Canonicalize\n", .{st.name});
        }
        st.phase = .Canonicalize;
        try self.enqueue(module_id);
    }

    fn readModuleSource(self: *PackageEnv, path: []const u8) ![]u8 {
        const data = try self.file_provider.read(self.file_provider.ctx, path, self.gpa) orelse
            return error.FileNotFound;

        // Normalize line endings (CRLF -> LF) for consistent cross-platform behavior.
        // This reallocates to the correct size if normalization occurs, ensuring
        // proper memory management when the buffer is freed later.
        return base.source_utils.normalizeLineEndingsRealloc(self.gpa, data);
    }

    fn doCanonicalize(self: *PackageEnv, module_id: ModuleId) !void {
        var st = &self.modules.items[module_id];
        var env = &st.env.?;

        // Use cached AST from doParse - it should always be available
        const parse_ast: *parse.AST = st.cached_ast orelse
            std.debug.panic("Internal compiler error: cached AST missing for module '{s}'. Please report this bug.", .{st.name});
        st.cached_ast = null; // Take ownership
        defer parse_ast.deinit();

        // Convert parse diagnostics to reports
        for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
            const report = try parse_ast.tokenizeDiagnosticToReport(diagnostic, self.gpa, st.path);
            try st.reports.append(self.gpa, report);
        }
        for (parse_ast.parse_diagnostics.items) |diagnostic| {
            const report = try parse_ast.parseDiagnosticToReport(&env.common, diagnostic, self.gpa, st.path);
            try st.reports.append(self.gpa, report);
        }

        // canonicalize using the AST
        const canon_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;

        // Use shared canonicalization function to ensure consistency with snapshot tool
        // Pass sibling module names from the same directory so MODULE NOT FOUND isn't
        // reported prematurely for modules that exist but haven't been loaded yet.
        // Use the MODULE's directory (not package root) for sibling lookup - this is
        // important for platform modules where siblings are in the same subdir.
        const module_dir = std.fs.path.dirname(st.path) orelse self.root_dir;
        var allocators: base.Allocators = undefined;
        allocators.initInPlace(self.gpa);
        defer allocators.deinit();
        try canonicalizeModuleWithSiblings(
            &allocators,
            env,
            parse_ast,
            self.builtin_modules.builtin_module.env,
            self.builtin_modules.builtin_indices,
            module_dir,
            self.package_name,
            self.resolver,
            self.additional_known_modules.items,
            null, // Use filesystem access check
        );

        const canon_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_canonicalize_ns += @intCast(canon_end - canon_start);
        }

        // Collect canonicalization diagnostics
        const canon_diag_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        const diags = try env.getDiagnostics();
        defer self.gpa.free(diags);
        for (diags) |d| {
            const report = try env.diagnosticToReport(d, self.gpa, st.path);
            try st.reports.append(self.gpa, report);
        }
        const canon_diag_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_canonicalize_diagnostics_ns += @intCast(canon_diag_end - canon_diag_start);
        }

        // Discover imports from env.imports
        const import_count = env.imports.imports.items.items.len;
        var any_new: bool = false;
        // Mark current node as visiting (gray) before exploring imports
        st.visit_color = 1;
        for (env.imports.imports.items.items[0..import_count]) |str_idx| {
            const mod_name = env.getString(str_idx);

            // Skip "Builtin" - it's handled via the precompiled module in module_envs_map
            if (std.mem.eql(u8, mod_name, "Builtin")) {
                continue;
            }

            // Use CIR qualifier metadata instead of heuristic; this allocates nothing and scans only once
            const qualified = hadQualifiedImport(env, mod_name);

            if (qualified) {
                // Qualified imports refer to external packages; track and schedule externally
                try st.external_imports.append(self.gpa, mod_name);
                if (self.resolver) |r| r.scheduleExternal(r.ctx, self.package_name, mod_name);
                // External dependencies are resolved by the workspace; skip local scheduling/cycle detection
                continue;
            }

            // Local import - schedule in this package
            const import_path = try self.resolveModulePath(mod_name);
            defer self.gpa.free(import_path);
            const prev_module_count = self.modules.items.len;
            const child_id = try self.ensureModule(mod_name, import_path);
            // Refresh st and env pointers in case ensureModule grew the modules array
            st = &self.modules.items[module_id];
            env = &st.env.?;
            const is_new_import = child_id >= prev_module_count;
            try st.imports.append(self.gpa, child_id);
            // parent depth + 1
            try self.setDepthIfSmaller(child_id, st.depth + 1);

            // Cycle detection for local deps
            var child = &self.modules.items[child_id];
            try child.dependents.append(self.gpa, module_id);

            if (child.visit_color == 1 or child_id == module_id) {
                // Build a report on the current module describing the cycle
                var rep = Report.init(self.gpa, "Import cycle detected", .runtime_error);
                const msg = try rep.addOwnedString("This module participates in an import cycle. Cycles between modules are not allowed.");
                try rep.addErrorMessage(msg);

                // Build full cycle path lazily (rare path): child_id ... module_id -> child_id
                if (try self.findPath(child_id, module_id)) |path| {
                    defer self.gpa.free(path);
                    const hdr = try rep.addOwnedString("Cycle: ");
                    try rep.document.addText(hdr);
                    var i: usize = 0;
                    while (i < path.len) : (i += 1) {
                        if (i > 0) try rep.document.addText(" -> ");
                        try rep.document.addAnnotated(self.modules.items[path[i]].name, .emphasized);
                    }
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(self.modules.items[path[0]].name, .emphasized);
                    try rep.document.addLineBreak();
                } else {
                    // Fallback: show the detected back-edge
                    const edge_msg = try rep.addOwnedString("Cycle edge: ");
                    try rep.document.addText(edge_msg);
                    try rep.document.addAnnotated(st.name, .emphasized);
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(mod_name, .emphasized);
                    try rep.document.addLineBreak();
                }

                // Store the report on both modules for clarity
                try st.reports.append(self.gpa, rep);
                // Duplicate for child as well so it gets emitted too
                var rep_child = Report.init(self.gpa, "Import cycle detected", .runtime_error);
                const child_msg = try rep_child.addOwnedString("This module participates in an import cycle. Cycles between modules are not allowed.");
                try rep_child.addErrorMessage(child_msg);
                const edge_msg2 = try rep_child.addOwnedString("Cycle edge: ");
                try rep_child.document.addText(edge_msg2);
                try rep_child.document.addAnnotated(st.name, .emphasized);
                try rep_child.document.addText(" -> ");
                try rep_child.document.addAnnotated(mod_name, .emphasized);
                try rep_child.document.addLineBreak();
                try child.reports.append(self.gpa, rep_child);

                // Mark both Done and adjust counters
                if (st.phase != .Done) {
                    if (comptime trace_build) {
                        std.debug.print("[TRACE-CACHE] PHASE: {s} ->Done (CYCLE DETECTED with {s})\n", .{ st.name, mod_name });
                    }
                    st.phase = .Done;
                    self.remaining_modules -= 1;
                }
                if (child.phase != .Done) {
                    if (comptime trace_build) {
                        std.debug.print("[TRACE-CACHE] PHASE: {s} ->Done (CYCLE DETECTED with {s})\n", .{ mod_name, st.name });
                    }
                    child.phase = .Done;
                    if (self.remaining_modules > 0) self.remaining_modules -= 1;
                }

                // Wake dependents and stop
                for (st.dependents.items) |dep| try self.enqueue(dep);
                for (child.dependents.items) |dep| try self.enqueue(dep);
                if (@import("builtin").target.cpu.arch != .wasm32) self.cond.broadcast();
                return;
            }

            if (is_new_import) {
                self.remaining_modules += 1;
                any_new = true;
            }
        }

        if (comptime trace_build) {
            std.debug.print("[TRACE-CACHE] PHASE: {s} Canonicalize->WaitingOnImports (imports={d}, external={d})\n", .{
                st.name,
                st.imports.items.len,
                st.external_imports.items.len,
            });
        }
        st.phase = .WaitingOnImports;
        // Kick off imports if any (locals only)
        if (any_new) {
            for (st.imports.items) |imp| try self.enqueue(imp);
        }
        // Also re-enqueue self to check for unblocking
        try self.enqueue(module_id);
    }

    fn tryUnblock(self: *PackageEnv, module_id: ModuleId) !void {
        var st = &self.modules.items[module_id];
        // If all imports are Done, move to TypeCheck
        var ready = true;

        // Local imports must be done
        for (st.imports.items) |imp| {
            const child = &self.modules.items[imp];
            if (child.phase != .Done) {
                ready = false;
                break;
            }
        }

        // External imports must be ready in the workspace (if resolver is provided)
        if (ready and st.external_imports.items.len > 0) {
            if (self.resolver) |r| {
                var k: usize = 0;
                while (k < st.external_imports.items.len) : (k += 1) {
                    if (!r.isReady(r.ctx, self.package_name, st.external_imports.items[k])) {
                        ready = false;
                        break;
                    }
                }
            } else {
                // No resolver; treat as not ready to avoid proceeding incorrectly
                ready = false;
            }
        }

        if (ready) {
            if (comptime trace_build) {
                std.debug.print("[TRACE-CACHE] PHASE: {s} WaitingOnImports->TypeCheck\n", .{st.name});
            }
            st.phase = .TypeCheck;
            // Mark as finished (black) when all children done
            st.visit_color = 2;
            try self.enqueue(module_id);
        }
    }

    /// Combined canonicalization and type checking function for snapshot tool
    /// This ensures the SAME module_envs map is used for both phases
    /// Note: Does NOT run compile-time evaluation - caller should do that separately if needed
    /// IMPORTANT: The returned checker holds a pointer to module_envs_out, so caller must keep
    /// module_envs_out alive until they're done using the checker (e.g., for type printing)
    pub fn canonicalizeAndTypeCheckModule(
        allocators: *base.Allocators,
        gpa: Allocator,
        env: *ModuleEnv,
        parse_ast: *AST,
        builtin_module_env: *const ModuleEnv,
        builtin_indices: can.CIR.BuiltinIndices,
        imported_envs: []const *ModuleEnv,
        module_envs_out: *std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    ) !Check {
        // Populate module_envs with Bool, Try, Dict, Set using shared function
        try Can.populateModuleEnvs(
            module_envs_out,
            env,
            builtin_module_env,
            builtin_indices,
        );

        // Canonicalize
        var czer = try Can.init(allocators, env, parse_ast, module_envs_out);
        try czer.canonicalizeFile();
        try czer.validateForChecking();
        czer.deinit();

        // Type check using the SAME module_envs_map
        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = env.qualified_module_ident,
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = builtin_module_env,
            .builtin_indices = builtin_indices,
        };

        var checker = try Check.init(
            gpa,
            &env.types,
            env,
            imported_envs,
            module_envs_out,
            &env.store.regions,
            module_builtin_ctx,
        );
        errdefer checker.deinit();

        // For app modules with platform requirements, defer finalizing numeric defaults
        // until after platform requirements are checked, so numeric literals can be
        // constrained by platform types (e.g., I64) before defaulting to Dec.
        if (env.defer_numeric_defaults) {
            try checker.checkFileSkipNumericDefaults();
        } else {
            try checker.checkFile();
        }

        return checker;
    }

    /// Canonicalization function that also discovers sibling .roc files in the same directory
    /// and includes additional known modules (e.g., from platform exposes).
    /// This prevents premature MODULE NOT FOUND errors for modules that exist but haven't been loaded yet.
    pub fn canonicalizeModuleWithSiblings(
        allocators: *base.Allocators,
        env: *ModuleEnv,
        parse_ast: *AST,
        builtin_module_env: *const ModuleEnv,
        builtin_indices: can.CIR.BuiltinIndices,
        root_dir: []const u8,
        package_name: []const u8,
        resolver: ?ImportResolver,
        additional_known_modules: []const KnownModule,
        file_provider: ?FileProvider,
    ) !void {
        const gpa = allocators.gpa;

        // Create module_envs map for auto-importing builtin types
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
        defer module_envs_map.deinit();

        // Populate module_envs with Bool, Try, Dict, Set using shared function
        try Can.populateModuleEnvs(
            &module_envs_map,
            env,
            builtin_module_env,
            builtin_indices,
        );

        // Add sibling modules - use placeholder-based approach for all paths.
        // In canonicalize-first mode, modules use placeholders during canonicalization.
        // Actual module envs are resolved during type-checking after topological sort.
        // The resolver's getEnv may return null for siblings not yet processed, so we
        // always add placeholders first. If the resolver has the actual env, we use it.
        const sibling_imports = try module_discovery.extractImportsFromAST(parse_ast, gpa);
        defer {
            for (sibling_imports) |imp| gpa.free(imp);
            gpa.free(sibling_imports);
        }

        for (sibling_imports) |sibling_name| {
            // Skip Builtin and self
            if (std.mem.eql(u8, sibling_name, "Builtin")) continue;
            if (std.mem.eql(u8, sibling_name, env.module_name)) continue;

            const sibling_ident = try env.insertIdent(base.Ident.for_text(sibling_name));
            const qualified_ident = try env.insertIdent(base.Ident.for_text(sibling_name));

            // Check if sibling file exists (via FileProvider if available, else filesystem)
            const file_name = try std.fmt.allocPrint(gpa, "{s}.roc", .{sibling_name});
            defer gpa.free(file_name);
            const file_path = try std.fs.path.join(gpa, &.{ root_dir, file_name });
            defer gpa.free(file_path);
            const exists = if (file_provider) |fp|
                fp.fileExists(file_path, gpa)
            else blk: {
                std.fs.cwd().access(file_path, .{}) catch break :blk false;
                break :blk true;
            };
            if (!exists) continue;

            // Try to get actual env from resolver if available
            if (resolver) |res| {
                if (res.getEnv(res.ctx, package_name, sibling_name)) |sibling_env| {
                    // Resolver has actual env - use it
                    const statement_idx: ?can.CIR.Statement.Idx = stmt_blk: {
                        const type_ident_in_module = sibling_env.common.findIdent(sibling_name) orelse break :stmt_blk null;
                        const type_node_idx = sibling_env.getExposedNodeIndexById(type_ident_in_module) orelse break :stmt_blk null;
                        break :stmt_blk @enumFromInt(type_node_idx);
                    };

                    try module_envs_map.put(sibling_ident, .{
                        .env = sibling_env,
                        .statement_idx = statement_idx,
                        .qualified_type_ident = qualified_ident,
                    });
                    continue;
                }
            }

            // Resolver doesn't have env yet (or no resolver) - add placeholder
            // Canonicalization will proceed with placeholder, actual env resolved later
            if (!module_envs_map.contains(sibling_ident)) {
                try module_envs_map.put(sibling_ident, .{
                    .env = builtin_module_env, // Placeholder
                    .qualified_type_ident = qualified_ident,
                    .is_placeholder = true, // Mark as placeholder
                });
            }
        }

        // Add additional known modules (e.g., from platform exposes for URL platforms)
        // Use the resolver to get the ACTUAL module env if available
        for (additional_known_modules) |km| {
            // Extract base module name (e.g., "Stdout" from "pf.Stdout")
            const base_module_name = if (std.mem.lastIndexOfScalar(u8, km.qualified_name, '.')) |dot_idx|
                km.qualified_name[dot_idx + 1 ..]
            else
                km.qualified_name;

            // Create identifiers for both the unqualified name and the qualified name
            const base_ident = try env.insertIdent(base.Ident.for_text(base_module_name));
            const qualified_ident = try env.insertIdent(base.Ident.for_text(km.qualified_name));

            // Try to get the actual module env using the resolver
            const actual_env: *const ModuleEnv = if (resolver) |res| blk: {
                if (res.getEnv(res.ctx, package_name, km.import_name)) |mod_env| {
                    break :blk mod_env;
                }
                break :blk builtin_module_env;
            } else builtin_module_env;

            // For platform type modules, set statement_idx so method lookups work correctly
            const statement_idx: ?can.CIR.Statement.Idx = if (actual_env != builtin_module_env) stmt_blk: {
                // Look up the type in the module's exposed_items to get the actual node index
                const type_ident_in_module = actual_env.common.findIdent(base_module_name) orelse break :stmt_blk null;
                const type_node_idx = actual_env.getExposedNodeIndexById(type_ident_in_module) orelse break :stmt_blk null;
                break :stmt_blk @enumFromInt(type_node_idx);
            } else null;

            const entry = Can.AutoImportedType{
                .env = actual_env,
                .statement_idx = statement_idx,
                .qualified_type_ident = base_ident,
                .is_package_qualified = true,
                // Mark as placeholder if using builtin env as fallback (actual env not available yet)
                .is_placeholder = (actual_env == builtin_module_env),
            };

            // Add entry for the UNQUALIFIED name (e.g., "Stdout", "Builder")
            // This is used for type annotations like `my_var : Builder`
            if (!module_envs_map.contains(base_ident)) {
                try module_envs_map.put(base_ident, entry);
            }

            // Also add entry for the QUALIFIED name (e.g., "pf.Stdout", "pf.Builder")
            // This is used when scopeLookupModule returns the qualified module name
            if (!module_envs_map.contains(qualified_ident)) {
                try module_envs_map.put(qualified_ident, entry);
            }
        }

        var czer = try Can.init(allocators, env, parse_ast, &module_envs_map);
        try czer.canonicalizeFile();
        try czer.validateForChecking();
        czer.deinit();
    }

    /// Standalone type checking function that can be called from other tools (e.g., snapshot tool)
    /// This ensures all tools use the exact same type checking logic as production builds
    pub fn typeCheckModule(
        gpa: Allocator,
        env: *ModuleEnv,
        builtin_module_env: *const ModuleEnv,
        imported_envs: []const *ModuleEnv,
        target: roc_target.RocTarget,
    ) !Check {
        // Load builtin indices from the binary data generated at build time
        const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);

        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = env.qualified_module_ident,
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = builtin_module_env,
            .builtin_indices = builtin_indices,
        };

        // Create module_envs map for auto-importing builtin types
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
        errdefer module_envs_map.deinit();

        // Populate module_envs with Bool, Try, Dict, Set using shared function
        try Can.populateModuleEnvs(
            &module_envs_map,
            env,
            builtin_module_env,
            builtin_indices,
        );

        var checker = try Check.init(
            gpa,
            &env.types,
            env,
            imported_envs,
            &module_envs_map,
            &env.store.regions,
            module_builtin_ctx,
        );
        errdefer checker.deinit();

        // For app modules with platform requirements, defer finalizing numeric defaults
        // until after platform requirements are checked, so numeric literals can be
        // constrained by platform types (e.g., I64) before defaulting to Dec.
        if (env.defer_numeric_defaults) {
            try checker.checkFileSkipNumericDefaults();
        } else {
            try checker.checkFile();
        }

        // After type checking, evaluate top-level declarations at compile time
        const builtin_types_for_eval = BuiltinTypes.init(builtin_indices, builtin_module_env, builtin_module_env, builtin_module_env);
        var comptime_evaluator = try eval.ComptimeEvaluator.init(gpa, env, imported_envs, &checker.problems, builtin_types_for_eval, builtin_module_env, &checker.import_mapping, target);
        defer comptime_evaluator.deinit();
        _ = try comptime_evaluator.evalAll();

        module_envs_map.deinit();

        return checker;
    }

    /// Resolve all pending lookups in a module's CIR.
    /// Called before type-checking, when all dependencies are canonicalized.
    /// This converts e_lookup_pending to e_lookup_external (or error).
    fn resolvePendingLookups(env: *ModuleEnv, imported_envs: []const *ModuleEnv) void {
        env.store.resolvePendingLookups(env, imported_envs);
    }

    fn doTypeCheck(self: *PackageEnv, module_id: ModuleId) !void {
        var st = &self.modules.items[module_id];
        var env = &st.env.?;

        // Build the array of all available modules for this module's imports
        const import_count = env.imports.imports.items.items.len;
        var imported_envs = try std.ArrayList(*ModuleEnv).initCapacity(self.gpa, import_count);
        // NOTE: Don't deinit 'imported_envs' yet - comptime_evaluator holds a reference to imported_envs.items

        // Always include Builtin first
        try imported_envs.append(self.gpa, self.builtin_modules.builtin_module.env);

        // Add external and local modules
        for (env.imports.imports.items.items[0..import_count]) |str_idx| {
            const import_name = env.getString(str_idx);

            // Skip Builtin - already added above
            if (std.mem.eql(u8, import_name, "Builtin")) {
                continue;
            }

            // Determine external vs local from CIR s_import qualifier metadata directly
            const is_ext = hadQualifiedImport(env, import_name);

            if (is_ext) {
                if (self.resolver) |r| {
                    if (r.getEnv(r.ctx, self.package_name, import_name)) |ext_env_ptr| {
                        try imported_envs.append(self.gpa, ext_env_ptr);
                    }
                    // External env not ready; skip (tryUnblock should have prevented this)
                }
            } else {
                const child_id = self.module_names.get(import_name).?;
                const child = &self.modules.items[child_id];
                // Get a pointer to the child's env (stored in the modules ArrayList)
                // This is safe because we don't modify the modules ArrayList during type checking
                const child_env_ptr = &child.env.?;
                try imported_envs.append(self.gpa, child_env_ptr);
            }
        }

        // Resolve all imports using the shared function
        // This matches import names to module names in imported_envs
        env.imports.resolveImports(env, imported_envs.items);

        // Resolve pending lookups that were deferred during canonicalization
        // This converts e_lookup_pending to e_lookup_external now that all dependencies are available
        resolvePendingLookups(env, imported_envs.items);

        const check_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        var checker = try typeCheckModule(self.gpa, env, self.builtin_modules.builtin_module.env, imported_envs.items, self.target);
        defer checker.deinit();
        const check_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_type_checking_ns += @intCast(check_end - check_start);
        }

        // Build reports from problems
        const check_diag_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        var rb = try ReportBuilder.init(self.gpa, env, env, &checker.snapshots, &checker.problems, st.path, imported_envs.items, &checker.import_mapping, &checker.regions);
        defer rb.deinit();
        for (checker.problems.problems.items) |prob| {
            const rep = rb.build(prob) catch continue;
            try st.reports.append(self.gpa, rep);
        }
        const check_diag_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_check_diagnostics_ns += @intCast(check_diag_end - check_diag_start);
        }

        // Comptime evaluator is managed inside typeCheckModule, no need to deinit here

        // Now we can safely deinit the 'imported_envs' ArrayList
        imported_envs.deinit(self.gpa);

        // Note: We no longer need to free the 'imported_envs' items because they now point directly
        // to ModuleEnv instances stored in the modules ArrayList, not to heap-allocated copies.

        // Done
        if (comptime trace_build) {
            std.debug.print("[TRACE-CACHE] PHASE: {s} TypeCheck->Done (dependents={d})\n", .{
                st.name,
                st.dependents.items.len,
            });
        }
        st.phase = .Done;
        self.remaining_modules -= 1;

        // Wake dependents to re-check unblock
        for (st.dependents.items) |dep| try self.enqueue(dep);
        if (@import("builtin").target.cpu.arch != .wasm32) self.cond.broadcast();
    }

    fn resolveModulePath(self: *PackageEnv, mod_name: []const u8) ![]const u8 {
        // Allow resolver to provide local path resolution if present
        if (self.resolver) |r| {
            return r.resolveLocalPath(r.ctx, self.package_name, self.root_dir, mod_name);
        }

        // Default: convert dotted module name to path under root_dir
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
        const full = try std.fs.path.join(self.gpa, &.{ self.root_dir, rel });
        self.gpa.free(rel);
        return full;
    }

    // Determine if an import was qualified (e.g. "cli.Stdout") using CIR statements.
    // After canonicalization, qualified imports have their full name (e.g., "pf.Stdout")
    // stored in module_name_tok, and qualifier_tok is null.
    // So we check if the module name contains a dot to determine if it was qualified.
    fn hadQualifiedImport(env: *ModuleEnv, mod_name_text: []const u8) bool {
        const stmts = env.store.sliceStatements(env.all_statements);
        for (stmts) |stmt_idx| {
            const stmt = env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_import => |imp| {
                    const module_name = env.getIdent(imp.module_name_tok);
                    if (!std.mem.eql(u8, module_name, mod_name_text)) continue;
                    // After canonicalization, qualified imports have their full name
                    // stored in module_name_tok. Check if it contains a dot.
                    return std.mem.indexOfScalar(u8, module_name, '.') != null;
                },
                else => {},
            }
        }
        return false;
    }

    // On-demand DFS to find a path from start -> target along import edges.
    // Returns an owned slice of module IDs.
    fn findPath(self: *PackageEnv, start: ModuleId, target: ModuleId) !?[]const ModuleId {
        var visited = std.bit_set.DynamicBitSetUnmanaged{};
        defer visited.deinit(self.gpa);
        try visited.resize(self.gpa, self.modules.items.len, false);

        const Frame = struct { id: ModuleId, next_idx: usize };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(self.gpa);

        var stack_ids = std.ArrayList(ModuleId).empty;
        defer stack_ids.deinit(self.gpa);

        visited.set(start);
        try frames.append(self.gpa, .{ .id = start, .next_idx = 0 });
        try stack_ids.append(self.gpa, start);

        while (frames.items.len > 0) {
            var top = &frames.items[frames.items.len - 1];
            if (top.id == target) {
                const out = try self.gpa.alloc(ModuleId, stack_ids.items.len);
                std.mem.copyForwards(ModuleId, out, stack_ids.items);
                return out;
            }

            const st = &self.modules.items[top.id];
            if (top.next_idx >= st.imports.items.len) {
                visited.unset(top.id);
                _ = stack_ids.pop();
                _ = frames.pop();
                continue;
            }

            const child = st.imports.items[top.next_idx];
            top.next_idx += 1;

            if (!visited.isSet(child)) {
                visited.set(child);
                try frames.append(self.gpa, .{ .id = child, .next_idx = 0 });
                try stack_ids.append(self.gpa, child);
            }
        }
        return null;
    }

    /// Extract the module name from a file path.
    /// Delegates to base.module_path.getModuleName for the implementation.
    pub fn moduleNameFromPath(path: []const u8) []const u8 {
        return base.module_path.getModuleName(path);
    }

    pub fn tryEmitReady(self: *PackageEnv) !void {
        // Sort discovered modules by (depth, name) each time; emit in prefix order
        if (self.discovered.items.len == 0) return;

        // Ensure emitted bitset is big enough
        try self.emitted.resize(self.gpa, self.modules.items.len, false);

        const ids = try self.gpa.alloc(ModuleId, self.discovered.items.len);
        defer self.gpa.free(ids);
        std.mem.copyForwards(ModuleId, ids, self.discovered.items);
        std.sort.block(ModuleId, ids, self, struct {
            fn lessThan(ctx: *PackageEnv, a: ModuleId, b: ModuleId) bool {
                const sa = ctx.modules.items[a].depth;
                const sb = ctx.modules.items[b].depth;
                if (sa == sb) return std.mem.lessThan(u8, ctx.modules.items[a].name, ctx.modules.items[b].name);
                return sa < sb;
            }
        }.lessThan);

        for (ids) |id| {
            if (self.emitted.isSet(id)) continue;
            const st = &self.modules.items[id];
            if (st.phase != .Done) break; // can't emit beyond an unfinished module in order
            // Emit all reports for this module
            for (st.reports.items) |rep| self.sink.emitFn(self.sink.ctx, st.name, rep);
            // Clear reports to transfer ownership - reports are shallow-copied when passed to emitFn,
            // so OrderedSink now owns the heap allocations. We must not free them here.
            st.reports.clearRetainingCapacity();
            // Mark emitted
            self.emitted.set(id);
        }
    }
};

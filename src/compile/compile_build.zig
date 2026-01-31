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
const compile_package = @import("compile_package.zig");
const Mode = compile_package.Mode;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const PackageEnv = compile_package.PackageEnv;
const ModuleTimingInfo = compile_package.TimingInfo;
const ImportResolver = compile_package.ImportResolver;
const ScheduleHook = compile_package.ScheduleHook;
const CacheManager = @import("cache_manager.zig").CacheManager;
const FileProvider = compile_package.FileProvider;

// Actor model components
const coordinator_mod = @import("coordinator.zig");
const Coordinator = coordinator_mod.Coordinator;
const roc_target = @import("roc_target");

// Compile-time flag for build tracing - enabled via `zig build -Dtrace-build`
const trace_build = if (@hasDecl(build_options, "trace_build")) build_options.trace_build else false;

// Threading features aren't available when targeting WebAssembly,
// so we disable them at comptime to prevent builds from failing.
const threads_available = builtin.target.cpu.arch != .wasm32;

const Thread = if (threads_available) std.Thread else struct {};
const Mutex = if (threads_available) std.Thread.Mutex else struct {};
const ThreadCondition = if (threads_available) std.Thread.Condition else struct {};

fn freeSlice(gpa: Allocator, s: []u8) void {
    gpa.free(s);
}

fn freeConstSlice(gpa: Allocator, s: []const u8) void {
    gpa.free(@constCast(s));
}

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
    target: roc_target.RocTarget,
    compiler_version: []const u8 = build_options.compiler_version,

    // Workspace roots for sandboxing (absolute, canonical)
    workspace_roots: std.array_list.Managed([]const u8),

    // Map of package name (alias) -> Package
    packages: std.StringHashMapUnmanaged(Package) = .{},
    // Schedulers per package name
    schedulers: std.StringHashMapUnmanaged(*PackageEnv) = .{},

    // Ordered sink over all packages (thread-safe, deterministic emission)
    sink: OrderedSink,

    // Actor model coordinator (owns all mutable compilation state)
    coordinator: ?*Coordinator = null,

    // Cache manager for compiled modules
    cache_manager: ?*CacheManager = null,
    // File provider for reading sources (defaults to filesystem)
    file_provider: FileProvider = FileProvider.filesystem,

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

    pub fn init(gpa: Allocator, mode: Mode, max_threads: usize, target: roc_target.RocTarget) !BuildEnv {
        // Allocate builtin modules on heap to prevent moves that would invalidate internal pointers
        const builtin_modules = try gpa.create(BuiltinModules);
        errdefer gpa.destroy(builtin_modules);

        builtin_modules.* = try BuiltinModules.init(gpa);
        errdefer builtin_modules.deinit();

        return .{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .workspace_roots = std.array_list.Managed([]const u8).init(gpa),
            .sink = OrderedSink.init(gpa),
            .builtin_modules = builtin_modules,
            .resolver_ctxs = std.array_list.Managed(*ResolverCtx).init(gpa),
            .pkg_sink_ctxs = std.array_list.Managed(*PkgSinkCtx).init(gpa),
            .schedule_ctxs = std.array_list.Managed(*ScheduleCtx).init(gpa),
            .pending_known_modules = std.array_list.Managed(PendingKnownModule).init(gpa),
        };
    }

    pub fn deinit(self: *BuildEnv) void {
        if (comptime trace_build) {
            std.debug.print("[DEINIT] BuildEnv.deinit starting\n", .{});
        }

        // Deinit and free builtin modules
        self.builtin_modules.deinit();
        self.gpa.destroy(self.builtin_modules);

        if (comptime trace_build) {
            std.debug.print("[DEINIT] builtin_modules done\n", .{});
        }

        // Deinit coordinator if present
        if (self.coordinator) |coord| {
            coord.deinit();
            self.gpa.destroy(coord);
        }

        if (comptime trace_build) {
            std.debug.print("[DEINIT] coordinator done\n", .{});
        }

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

        if (comptime trace_build) {
            std.debug.print("[DEINIT] ctxs done, deinitializing schedulers...\n", .{});
        }

        // Deinit schedulers
        var sit = self.schedulers.iterator();
        while (sit.next()) |e| {
            if (comptime trace_build) {
                std.debug.print("[DEINIT] deinit scheduler {s} starting...\n", .{e.key_ptr.*});
            }
            const mb_ptr: *PackageEnv = e.value_ptr.*;
            mb_ptr.deinit();
            if (comptime trace_build) {
                std.debug.print("[DEINIT] deinit scheduler {s} done\n", .{e.key_ptr.*});
            }
            self.gpa.destroy(mb_ptr);
            freeConstSlice(self.gpa, e.key_ptr.*);
        }

        if (comptime trace_build) {
            std.debug.print("[DEINIT] all scheduler deinits done, freeing hashmap...\n", .{});
        }
        self.schedulers.deinit(self.gpa);

        if (comptime trace_build) {
            std.debug.print("[DEINIT] schedulers done, deinitializing packages...\n", .{});
        }

        // Deinit packages
        var pit = self.packages.iterator();
        while (pit.next()) |e| {
            var p = e.value_ptr.*;
            p.deinit(self.gpa);
            freeConstSlice(self.gpa, e.key_ptr.*);
        }
        self.packages.deinit(self.gpa);

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

    /// Set a file provider (or reset to default filesystem provider).
    pub fn setFileProvider(self: *BuildEnv, provider: ?FileProvider) void {
        self.file_provider = provider orelse FileProvider.filesystem;
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
    //
    // Uses the actor model coordinator for both single-threaded and multi-threaded modes.
    // The coordinator uses message passing to eliminate race conditions.
    pub fn build(self: *BuildEnv, root_file: []const u8) !void {
        try self.buildWithCoordinator(root_file);
    }

    /// Initialize the actor model coordinator.
    /// This must be called before buildWithCoordinator().
    pub fn initCoordinator(self: *BuildEnv) !void {
        if (self.coordinator != null) return; // Already initialized

        const coord = try self.gpa.create(Coordinator);
        coord.* = try Coordinator.init(
            self.gpa,
            self.mode,
            self.max_threads,
            self.target,
            self.builtin_modules,
            self.compiler_version,
            self.cache_manager,
        );
        coord.setFileProvider(self.file_provider);
        // Enable hosted transform for platform modules - converts e_anno_only to e_hosted_lambda
        // This is required for roc build so that hosted functions can be called at runtime
        coord.enable_hosted_transform = true;
        self.coordinator = coord;
    }

    /// Build using the actor model coordinator.
    /// This is an alternative to build() that uses message passing instead of shared mutable state.
    pub fn buildWithCoordinator(self: *BuildEnv, root_file: []const u8) !void {
        // Initialize coordinator if not already done
        try self.initCoordinator();
        const coord = self.coordinator.?;

        // Parse root file header
        const root_abs = try self.makeAbsolute(root_file);
        defer self.gpa.free(root_abs);
        const root_dir = if (std.fs.path.dirname(root_abs)) |d| try std.fs.path.resolve(self.gpa, &.{d}) else try self.gpa.dupe(u8, ".");
        defer self.gpa.free(root_dir);

        try self.workspace_roots.append(try self.gpa.dupe(u8, root_dir));

        var header_info = try self.parseHeaderDeps(root_abs);
        defer header_info.deinit(self.gpa);

        const is_executable = header_info.kind == .app or header_info.kind == .default_app;
        // Allow all module types: app, module, type_module, package, platform
        // Package and platform modules can also be tested
        if (!is_executable and header_info.kind != .module and header_info.kind != .type_module and header_info.kind != .package and header_info.kind != .platform) {
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

        // Populate package graph (for apps and packages with dependencies)
        if (header_info.kind == .app or header_info.kind == .package) {
            try self.populatePackageShorthands(pkg_name, &header_info);
        }

        // Create coordinator packages mirroring BuildEnv packages
        // Skip .module kind packages - these are platform-exposed modules (e.g., Stdout)
        // that were incorrectly registered as packages. They should be modules within
        // the platform package, not separate packages.
        if (comptime trace_build) {
            std.debug.print("[BUILD] Creating coordinator packages from {} BuildEnv packages:\n", .{self.packages.count()});
        }
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;

            // Skip module-type packages - they're platform-exposed modules, not real packages
            // BUT don't skip the main package we're building, even if it's a module
            const is_main_pkg = std.mem.eql(u8, entry.key_ptr.*, pkg_name);
            if (!is_main_pkg and (pkg.kind == .module or pkg.kind == .type_module)) {
                if (comptime trace_build) {
                    std.debug.print("[BUILD]   Skipping module-as-package: {s}\n", .{entry.key_ptr.*});
                }
                continue;
            }

            if (comptime trace_build) {
                std.debug.print("[BUILD]   Package: {s} root_dir={s} kind={}\n", .{ entry.key_ptr.*, pkg.root_dir, pkg.kind });
            }
            const coord_pkg = try coord.ensurePackage(entry.key_ptr.*, pkg.root_dir);

            // Copy shorthands to coordinator package
            // Only copy shorthands that map to real packages, not module-as-package entries
            var sh_it = pkg.shorthands.iterator();
            while (sh_it.next()) |sh_entry| {
                const target_name = sh_entry.value_ptr.name;
                // Check if the target is a real package or a module-as-package
                if (self.packages.get(target_name)) |target_pkg| {
                    if (target_pkg.kind == .module or target_pkg.kind == .type_module) {
                        if (comptime trace_build) {
                            std.debug.print("[BUILD]     Skipping shorthand to module: {s} -> {s}\n", .{ sh_entry.key_ptr.*, target_name });
                        }
                        continue;
                    }
                }

                if (comptime trace_build) {
                    std.debug.print("[BUILD]     Shorthand: {s} -> {s}\n", .{ sh_entry.key_ptr.*, target_name });
                }
                try coord_pkg.shorthands.put(
                    try self.gpa.dupe(u8, sh_entry.key_ptr.*),
                    try self.gpa.dupe(u8, target_name),
                );
            }
        }

        // Create schedulers for compatibility with existing code paths
        try self.createSchedulers();
        try self.processPendingKnownModules();

        // Queue root module in coordinator
        const coord_pkg = coord.getPackage(pkg_name).?;
        const module_name = PackageEnv.moduleNameFromPath(pkg_root_file);
        const root_id = try coord_pkg.ensureModule(self.gpa, module_name, pkg_root_file);
        coord_pkg.modules.items[root_id].depth = 0;
        coord_pkg.root_module_id = root_id;
        coord_pkg.remaining_modules += 1;
        coord.total_remaining += 1;

        // Start workers (for multi-threaded mode)
        try coord.start();

        // Queue initial parse task
        try coord.enqueueParseTask(pkg_name, root_id);

        // Also queue the platform's root module if this is an app
        // The platform's root module contains the `requires` clause which must be compiled
        // for type checking against the app's exports
        var platform_root_queued = false;
        var pf_it = self.packages.iterator();
        while (pf_it.next()) |pf_entry| {
            const pf_pkg = pf_entry.value_ptr.*;
            if (pf_pkg.kind == .platform) {
                if (coord.getPackage(pf_entry.key_ptr.*)) |platform_coord_pkg| {
                    const plat_module_name = PackageEnv.moduleNameFromPath(pf_pkg.root_file);
                    const plat_root_id = try platform_coord_pkg.ensureModule(self.gpa, plat_module_name, pf_pkg.root_file);
                    if (platform_coord_pkg.modules.items[plat_root_id].phase == .Parse) {
                        platform_coord_pkg.modules.items[plat_root_id].depth = 1;
                        platform_coord_pkg.root_module_id = plat_root_id;
                        platform_coord_pkg.remaining_modules += 1;
                        coord.total_remaining += 1;
                        try coord.enqueueParseTask(pf_entry.key_ptr.*, plat_root_id);
                        platform_root_queued = true;
                        if (comptime trace_build) {
                            std.debug.print("[BUILD] Queued platform root module: {s} in package {s}\n", .{ plat_module_name, pf_entry.key_ptr.* });
                        }
                    }
                }
            }
        }

        // Run coordinator loop
        try coord.coordinatorLoop();

        if (comptime trace_build) {
            std.debug.print("[BUILD] Coordinator loop complete, transferring results...\n", .{});
        }

        // Transfer results back to PackageEnv for compatibility
        try self.transferCoordinatorResults();

        if (comptime trace_build) {
            std.debug.print("[BUILD] Results transferred, checking platform requirements...\n", .{});
        }

        // Check platform requirements
        try self.checkPlatformRequirements();

        if (comptime trace_build) {
            std.debug.print("[BUILD] Platform requirements checked, emitting...\n", .{});
        }

        // Deterministic emission
        try self.emitDeterministic();

        if (comptime trace_build) {
            std.debug.print("[BUILD] buildWithCoordinator complete\n", .{});
        }
    }

    /// Transfer compilation results from Coordinator to PackageEnv (for compatibility)
    fn transferCoordinatorResults(self: *BuildEnv) !void {
        const coord = self.coordinator orelse return;

        var coord_pkg_it = coord.packages.iterator();
        while (coord_pkg_it.next()) |coord_entry| {
            const coord_pkg = coord_entry.value_ptr.*;
            const sched = self.schedulers.get(coord_entry.key_ptr.*) orelse {
                if (comptime trace_build) {
                    std.debug.print("[TRANSFER] No scheduler for package {s}, skipping\n", .{coord_entry.key_ptr.*});
                }
                continue;
            };

            if (comptime trace_build) {
                std.debug.print("[TRANSFER] Package {s}: {} coord modules, {} sched modules\n", .{
                    coord_entry.key_ptr.*,
                    coord_pkg.modules.items.len,
                    sched.modules.items.len,
                });
            }

            // Transfer each module's results
            for (coord_pkg.modules.items) |*coord_mod| {
                // Ensure module exists in scheduler - if not, create it
                var maybe_sched_mod = sched.getModuleState(coord_mod.name);
                if (maybe_sched_mod == null) {
                    if (comptime trace_build) {
                        std.debug.print("[TRANSFER]   Module {s} not in scheduler, creating\n", .{coord_mod.name});
                    }
                    // Create the module in the scheduler
                    _ = sched.ensureModule(coord_mod.name, coord_mod.path) catch continue;
                    maybe_sched_mod = sched.getModuleState(coord_mod.name);
                }
                const sched_mod = maybe_sched_mod orelse continue;

                // Transfer depth from coordinator to scheduler
                sched.setModuleDepthIfSmaller(coord_mod.name, coord_mod.depth) catch {};
                if (comptime trace_build) {
                    std.debug.print("[TRANSFER]   Transferred depth {} for {s}\n", .{ coord_mod.depth, coord_mod.name });
                }

                if (comptime trace_build) {
                    std.debug.print("[TRANSFER]   Before transfer: sched_mod.reports.len={} cap={}\n", .{ sched_mod.reports.items.len, sched_mod.reports.capacity });
                }

                // Transfer env ownership - move from coordinator to scheduler
                if (coord_mod.env) |env| {
                    if (sched_mod.env == null) {
                        if (comptime trace_build) {
                            std.debug.print("[TRANSFER]   Transferring env for {s} (was_cache_hit={})\n", .{ coord_mod.name, coord_mod.was_cache_hit });
                        }
                        // Copy env content to scheduler (scheduler owns inline, not pointer)
                        sched_mod.env = env.*;
                        // Transfer the cache flag so scheduler knows not to deinit cached envs
                        sched_mod.was_from_cache = coord_mod.was_cache_hit;

                        // Free the heap-allocated struct wrapper.
                        // IMPORTANT: Use env.gpa, not self.gpa, because the env was
                        // allocated with env.gpa (page_allocator in multi-threaded mode).
                        env.gpa.destroy(env);
                        // Clear coordinator's pointer to prevent double-free during deinit
                        coord_mod.env = null;
                    }
                }

                if (comptime trace_build) {
                    std.debug.print("[TRANSFER]   After env transfer: sched_mod.reports.len={} cap={}\n", .{ sched_mod.reports.items.len, sched_mod.reports.capacity });
                    std.debug.print("[TRANSFER]   Coord reports to transfer: len={} cap={}\n", .{ coord_mod.reports.items.len, coord_mod.reports.capacity });
                }

                if (comptime trace_build) {
                    std.debug.print("[TRANSFER]   coord_mod ptr={} reports.items.ptr={}\n", .{ @intFromPtr(coord_mod), @intFromPtr(coord_mod.reports.items.ptr) });
                }

                // Transfer reports
                for (coord_mod.reports.items, 0..) |rep, ri| {
                    if (comptime trace_build) {
                        std.debug.print("[TRANSFER]   BEFORE append report {}: owned_strings.len={}\n", .{ ri, rep.owned_strings.items.len });
                        if (rep.owned_strings.items.len > 0) {
                            std.debug.print("[TRANSFER]   BEFORE append: first owned_string ptr={} len={}\n", .{ @intFromPtr(rep.owned_strings.items[0].ptr), rep.owned_strings.items[0].len });
                        }
                    }
                    try sched_mod.reports.append(self.gpa, rep);
                }
                coord_mod.reports.clearRetainingCapacity();

                if (comptime trace_build) {
                    std.debug.print("[TRANSFER]   After reports transfer: sched_mod.reports.len={} cap={}\n", .{ sched_mod.reports.items.len, sched_mod.reports.capacity });
                    for (sched_mod.reports.items, 0..) |rep, ri| {
                        std.debug.print("[TRANSFER]   Report {}: title=\"{s}\" owned_strings.len={}\n", .{ ri, rep.title, rep.owned_strings.items.len });
                        if (rep.owned_strings.items.len > 0) {
                            std.debug.print("[TRANSFER]   First owned_string ptr={} len={}\n", .{ @intFromPtr(rep.owned_strings.items[0].ptr), rep.owned_strings.items[0].len });
                        }
                    }
                }

                // Update phase
                sched_mod.phase = switch (coord_mod.phase) {
                    .Parse, .Parsing => .Parse,
                    .Canonicalize => .Canonicalize,
                    .WaitingOnImports => .WaitingOnImports,
                    .TypeCheck => .TypeCheck,
                    .Done => .Done,
                };

                // Emit reports to sink for deterministic ordering
                // Then clear scheduler's reports to transfer ownership to sink
                for (sched_mod.reports.items) |rep| {
                    self.sink.emitReport(coord_entry.key_ptr.*, coord_mod.name, rep);
                }
                sched_mod.reports.clearRetainingCapacity();
            }

            // Transfer root_module_id from coordinator to scheduler
            if (coord_pkg.root_module_id) |root_id| {
                if (root_id < coord_pkg.modules.items.len) {
                    const root_name = coord_pkg.modules.items[root_id].name;
                    // Find the corresponding module ID in the scheduler
                    if (sched.module_names.get(root_name)) |sched_root_id| {
                        sched.root_module_id = sched_root_id;
                        if (comptime trace_build) {
                            std.debug.print("[TRANSFER] Set root_module_id={} for package {s} (module: {s})\n", .{ sched_root_id, coord_entry.key_ptr.*, root_name });
                        }
                    }
                }
            }
        }
    }

    /// Check that app exports match platform requirements.
    /// This is called after all modules are compiled and type-checked.
    fn checkPlatformRequirements(self: *BuildEnv) !void {
        // Find the app and platform packages
        var app_pkg_info: ?Package = null;
        var platform_pkg_info: ?Package = null;
        var app_pkg_name: ?[]const u8 = null;
        var platform_pkg_name: ?[]const u8 = null;

        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            if (pkg.kind == .app) {
                app_pkg_info = pkg;
                app_pkg_name = entry.key_ptr.*;
            } else if (pkg.kind == .platform) {
                platform_pkg_info = pkg;
                platform_pkg_name = entry.key_ptr.*;
            }
        }

        // If we don't have both an app and a platform, nothing to check
        const app_name = app_pkg_name orelse {
            if (comptime trace_build) std.debug.print("[PLAT-CHECK] No app package found\n", .{});
            return;
        };
        const platform_name = platform_pkg_name orelse {
            if (comptime trace_build) std.debug.print("[PLAT-CHECK] No platform package found\n", .{});
            return;
        };
        const platform_pkg = platform_pkg_info orelse return;

        if (comptime trace_build) {
            std.debug.print("[PLAT-CHECK] Found app={s} platform={s}\n", .{ app_name, platform_name });
        }

        // Get the schedulers for both packages
        const app_sched = self.schedulers.get(app_name) orelse {
            if (comptime trace_build) std.debug.print("[PLAT-CHECK] No app scheduler found\n", .{});
            return;
        };
        const platform_sched = self.schedulers.get(platform_name) orelse {
            if (comptime trace_build) std.debug.print("[PLAT-CHECK] No platform scheduler found\n", .{});
            return;
        };

        // Get the app's root module env
        const app_root_env = app_sched.getRootEnv() orelse {
            if (comptime trace_build) std.debug.print("[PLAT-CHECK] No app root env found\n", .{});
            return;
        };

        // Get the platform's root module by finding the module that matches the root file
        // Note: getRootEnv() returns modules.items[0], but that may not be the actual platform
        // root file if other modules (like exposed imports) were scheduled first.
        const platform_root_module_name = PackageEnv.moduleNameFromPath(platform_pkg.root_file);
        if (comptime trace_build) {
            std.debug.print("[PLAT-CHECK] Looking for platform root module: {s} (from path: {s})\n", .{ platform_root_module_name, platform_pkg.root_file });
        }
        const platform_module_state = platform_sched.getModuleState(platform_root_module_name) orelse {
            if (comptime trace_build) {
                std.debug.print("[PLAT-CHECK] Platform module state not found\n", .{});
            }
            return;
        };
        const platform_root_env = if (platform_module_state.env) |*env| env else {
            if (comptime trace_build) {
                std.debug.print("[PLAT-CHECK] Platform root env not found\n", .{});
            }
            return;
        };

        if (comptime trace_build) {
            std.debug.print("[PLAT-CHECK] Platform root env found, requires_types.len={}\n", .{platform_root_env.requires_types.items.items.len});
        }

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

        // Enable runtime inserts on the app's interner so we can add new idents from platform
        // (the app's interner may be deserialized from cache and not support inserts by default)
        // Use app_root_env.gpa so the memory is freed by the same allocator during ModuleEnv.deinit()
        try app_root_env.common.idents.interner.enableRuntimeInserts(app_root_env.gpa);

        for (platform_root_env.requires_types.items.items) |required_type| {
            const platform_ident_text = platform_root_env.getIdent(required_type.ident);
            if (app_root_env.common.findIdent(platform_ident_text)) |app_ident| {
                try platform_to_app_idents.put(required_type.ident, app_ident);
            }

            // Also add for-clause type alias names (Model, model) to the translation map
            const all_aliases = platform_root_env.for_clause_aliases.items.items;
            const type_aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
            for (type_aliases_slice) |alias| {
                // Add alias name (e.g., "Model") - look up in app's ident store first,
                // and only insert if not found (avoids error on deserialized interners).
                const alias_name_text = platform_root_env.getIdent(alias.alias_name);
                const alias_app_ident = app_root_env.common.findIdent(alias_name_text) orelse
                    try app_root_env.common.insertIdent(app_root_env.gpa, base.Ident.for_text(alias_name_text));
                try platform_to_app_idents.put(alias.alias_name, alias_app_ident);

                // Add rigid name (e.g., "model") - look up first, only insert if not found.
                const rigid_name_text = platform_root_env.getIdent(alias.rigid_name);
                const rigid_app_ident = app_root_env.common.findIdent(rigid_name_text) orelse
                    try app_root_env.common.insertIdent(app_root_env.gpa, base.Ident.for_text(rigid_name_text));
                try platform_to_app_idents.put(alias.rigid_name, rigid_app_ident);
            }
        }

        // Check platform requirements against app exports
        try checker.checkPlatformRequirements(platform_root_env, &platform_to_app_idents);

        // If there are type problems, convert them to reports and emit via sink
        if (checker.problems.problems.items.len > 0) {
            const app_root_module = app_sched.getRootModule() orelse return;

            var rb = try ReportBuilder.init(
                self.gpa,
                app_root_env,
                app_root_env,
                &checker.snapshots,
                &checker.problems,
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

    fn resolverScheduleExternal(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) void {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return;

        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return;

        const ref = cur_pkg.shorthands.get(qualified.qualifier) orelse {
            return;
        };
        const target_pkg_name = ref.name;
        const target_pkg = self.ws.packages.get(target_pkg_name) orelse {
            return;
        };

        const mod_path = self.ws.dottedToPath(target_pkg.root_dir, qualified.module) catch {
            return;
        };
        defer self.ws.gpa.free(mod_path);

        const sched = self.ws.schedulers.get(target_pkg_name) orelse {
            return;
        };
        sched.*.scheduleModule(qualified.module, mod_path, 1) catch {
            // Continue anyway - dependency resolution will handle missing modules
        };
    }

    fn resolverIsReady(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) bool {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return false;

        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return false;

        const ref = cur_pkg.shorthands.get(qualified.qualifier) orelse return false;
        const sched = self.ws.schedulers.get(ref.name) orelse return false;

        return sched.*.getEnvIfDone(qualified.module) != null;
    }

    fn resolverGetEnv(ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) ?*ModuleEnv {
        var self: *ResolverCtx = @ptrCast(@alignCast(ctx.?));
        const cur_pkg = self.ws.packages.get(current_package) orelse return null;

        // Check if this is a local module (no qualifier)
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse {
            // Local module - look it up in the current package's scheduler
            const cur_sched = self.ws.schedulers.get(current_package) orelse return null;
            return cur_sched.*.getEnvIfDone(import_name);
        };

        // External module - look it up via shorthands
        const ref = cur_pkg.shorthands.get(qualified.qualifier) orelse {
            return null;
        };
        const sched = self.ws.schedulers.get(ref.name) orelse {
            return null;
        };

        return sched.*.getEnvIfDone(qualified.module);
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
                const plat_path = if (base.url.isSafeUrl(plat_rel)) blk: {
                    const cached_path = try self.resolveUrlPackage(plat_rel);
                    break :blk cached_path;
                } else blk: {
                    const header_dir = std.fs.path.dirname(file_abs) orelse ".";
                    const abs_path = try PathUtils.makeAbsolute(self.gpa, header_dir, plat_rel);
                    break :blk abs_path;
                };

                info.platform_alias = try self.gpa.dupe(u8, alias);
                info.platform_path = @constCast(plat_path);

                // Add platform directory to workspace roots so that imports within the platform
                // can be resolved. This is needed for both URL packages (cached paths) and
                // relative paths that may point outside the app directory (e.g., ../platform/main.roc)
                if (std.fs.path.dirname(plat_path)) |plat_dir| {
                    if (!PathUtils.isWithinRoot(plat_dir, self.workspace_roots.items)) {
                        try self.workspace_roots.append(try self.gpa.dupe(u8, plat_dir));
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
                    const v = if (base.url.isSafeUrl(relp)) blk: {
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
                    const v = if (base.url.isSafeUrl(relp)) blk: {
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
                    const v = if (base.url.isSafeUrl(relp)) blk: {
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
        _ = max_bytes; // FileProvider doesn't support max_bytes limit
        const data = try self.file_provider.read(self.file_provider.ctx, path, self.gpa) orelse
            return error.FileNotFound;

        // Normalize line endings (CRLF -> LF) for consistent cross-platform behavior.
        // This reallocates to the correct size if normalization occurs, ensuring
        // proper memory management when the buffer is freed later.
        return base.source_utils.normalizeLineEndingsRealloc(self.gpa, data);
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
            // The coordinator handles all scheduling now, so we use a no-op hook
            const schedule_hook = ScheduleHook{ .ctx = sc, .onSchedule = ScheduleCtx.onSchedule };
            sched.* = PackageEnv.initWithResolver(
                self.gpa,
                name,
                pkg.root_dir,
                self.mode,
                self.max_threads,
                self.target,
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

            const abs = if (base.url.isSafeUrl(p_path))
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

            const abs = if (base.url.isSafeUrl(path))
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
        // Mark entries without reports as emitted BEFORE calling tryEmitLocked
        // so they don't block other entries from being emitted.
        for (self.sink.entries.items) |*e| {
            if (e.reports.items.len == 0) {
                e.ready = true;
                e.emitted = true;
            }
        }
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

    /// Build statistics collected during compilation
    pub const BuildStats = struct {
        /// Total modules processed (cached + compiled)
        modules_total: u32 = 0,
        /// Modules loaded from cache
        cache_hits: u32 = 0,
        /// Modules that needed compilation (cache misses)
        cache_misses: u32 = 0,

        /// Number of modules that were compiled (not cached)
        modules_compiled: u32 = 0,

        /// Module compile time tracking (for non-cached modules)
        /// Time is for full module compilation: parse -> canonicalize -> type-check
        module_time_min_ns: u64 = std.math.maxInt(u64),
        module_time_max_ns: u64 = 0,
        module_time_sum_ns: u64 = 0,

        /// Record a module's compilation time
        pub fn recordModuleTime(self: *BuildStats, time_ns: u64) void {
            self.modules_compiled += 1;
            self.module_time_sum_ns += time_ns;
            if (time_ns < self.module_time_min_ns) self.module_time_min_ns = time_ns;
            if (time_ns > self.module_time_max_ns) self.module_time_max_ns = time_ns;
        }

        /// Get average module compile time in nanoseconds
        pub fn moduleTimeAvgNs(self: BuildStats) u64 {
            if (self.modules_compiled == 0) return 0;
            return self.module_time_sum_ns / self.modules_compiled;
        }

        /// Get module time min in milliseconds (rounded)
        pub fn moduleTimeMinMs(self: BuildStats) u32 {
            if (self.modules_compiled == 0) return 0;
            return @intCast((self.module_time_min_ns + 500_000) / 1_000_000);
        }

        /// Get module time max in milliseconds (rounded)
        pub fn moduleTimeMaxMs(self: BuildStats) u32 {
            if (self.modules_compiled == 0) return 0;
            return @intCast((self.module_time_max_ns + 500_000) / 1_000_000);
        }

        /// Get module time average in milliseconds (rounded)
        pub fn moduleTimeAvgMs(self: BuildStats) u32 {
            if (self.modules_compiled == 0) return 0;
            return @intCast((self.moduleTimeAvgNs() + 500_000) / 1_000_000);
        }

        /// Get cache hit rate as percentage (0-100)
        pub fn cacheHitPercent(self: BuildStats) u32 {
            const total = self.cache_hits + self.cache_misses;
            if (total == 0) return 0;
            return @intCast((@as(u64, self.cache_hits) * 100 + total / 2) / total);
        }
    };

    /// Get build statistics from the coordinator
    pub fn getBuildStats(self: *BuildEnv) BuildStats {
        if (self.coordinator) |coord| {
            return coord.getBuildStats();
        }
        return .{};
    }

    // Keep old name for backwards compatibility during transition
    pub const BuildCacheStats = BuildStats;
    pub fn getCacheStats(self: *BuildEnv) BuildStats {
        return self.getBuildStats();
    }

    /// Information about a compiled module, ready for serialization.
    /// All pointers reference data owned by the BuildEnv/Coordinator.
    pub const CompiledModuleInfo = struct {
        /// Module name (e.g., "Main", "Stdout")
        name: []const u8,
        /// Pointer to the compiled ModuleEnv
        env: *ModuleEnv,
        /// Source code of the module
        source: []const u8,
        /// Package name this module belongs to
        package_name: []const u8,
        /// True if this is the platform's main.roc
        is_platform_main: bool,
        /// True if this is the app module
        is_app: bool,
        /// True if this is a platform sibling module (e.g., Stdout, Stderr)
        is_platform_sibling: bool,
        /// Dependency depth from root
        depth: u32,
    };

    /// Get all compiled modules from the schedulers (after build completes).
    /// Returns modules in arbitrary order - use getModulesInSerializationOrder() for sorted order.
    ///
    /// IMPORTANT: This reads from schedulers, not the coordinator, because
    /// transferCoordinatorResults() moves env ownership to schedulers.
    pub fn getCompiledModules(self: *BuildEnv, allocator: Allocator) ![]CompiledModuleInfo {
        // Assert we have a coordinator (build was called)
        std.debug.assert(self.coordinator != null);

        var modules = std.ArrayList(CompiledModuleInfo).empty;
        errdefer modules.deinit(allocator);

        // Read from schedulers since transferCoordinatorResults moved data there
        var sched_it = self.schedulers.iterator();
        while (sched_it.next()) |sched_entry| {
            const pkg_name = sched_entry.key_ptr.*;
            const sched = sched_entry.value_ptr.*;

            // Determine package kind
            const pkg_info = self.packages.get(pkg_name);
            const is_platform_pkg = pkg_info != null and pkg_info.?.kind == .platform;
            const is_app_pkg = pkg_info != null and (pkg_info.?.kind == .app or pkg_info.?.kind == .default_app);

            for (sched.modules.items, 0..) |*sched_mod, mod_idx| {
                // Skip modules without env (not compiled or failed)
                if (sched_mod.env == null) continue;
                const env_ptr: *ModuleEnv = &sched_mod.env.?;

                const source = env_ptr.common.source;

                // Determine if this is platform main or sibling
                const is_root = sched.root_module_id != null and sched.root_module_id.? == mod_idx;
                const is_platform_main = is_platform_pkg and is_root;
                const is_platform_sibling = is_platform_pkg and !is_root;
                const is_app = is_app_pkg and is_root;

                try modules.append(allocator, .{
                    .name = sched_mod.name,
                    .env = env_ptr,
                    .source = source,
                    .package_name = pkg_name,
                    .is_platform_main = is_platform_main,
                    .is_app = is_app,
                    .is_platform_sibling = is_platform_sibling,
                    .depth = sched_mod.depth,
                });
            }
        }

        return modules.toOwnedSlice(allocator);
    }

    /// Get modules in serialization order: platform siblings → platform main → app siblings → app.
    /// This order ensures dependencies are serialized before dependents.
    pub fn getModulesInSerializationOrder(self: *BuildEnv, allocator: Allocator) ![]CompiledModuleInfo {
        const all_modules = try self.getCompiledModules(allocator);
        errdefer allocator.free(all_modules);

        if (all_modules.len == 0) return all_modules;

        // Separate into categories
        var platform_siblings = std.ArrayList(CompiledModuleInfo).empty;
        defer platform_siblings.deinit(allocator);
        var platform_main: ?CompiledModuleInfo = null;
        var app_siblings = std.ArrayList(CompiledModuleInfo).empty;
        defer app_siblings.deinit(allocator);
        var app_main: ?CompiledModuleInfo = null;

        for (all_modules) |mod| {
            if (mod.is_platform_sibling) {
                try platform_siblings.append(allocator, mod);
            } else if (mod.is_platform_main) {
                platform_main = mod;
            } else if (mod.is_app) {
                app_main = mod;
            } else {
                // App sibling module
                try app_siblings.append(allocator, mod);
            }
        }

        // Sort platform siblings by depth then name
        const SortContext = struct {
            pub fn lessThan(_: void, a: CompiledModuleInfo, b: CompiledModuleInfo) bool {
                if (a.depth != b.depth) return a.depth < b.depth;
                return std.mem.order(u8, a.name, b.name) == .lt;
            }
        };
        std.mem.sort(CompiledModuleInfo, platform_siblings.items, {}, SortContext.lessThan);
        std.mem.sort(CompiledModuleInfo, app_siblings.items, {}, SortContext.lessThan);

        // Build result in order: platform siblings → platform main → app siblings → app
        var result = std.ArrayList(CompiledModuleInfo).empty;
        errdefer result.deinit(allocator);

        for (platform_siblings.items) |mod| {
            try result.append(allocator, mod);
        }
        if (platform_main) |mod| {
            try result.append(allocator, mod);
        }
        for (app_siblings.items) |mod| {
            try result.append(allocator, mod);
        }
        if (app_main) |mod| {
            try result.append(allocator, mod);
        }

        allocator.free(all_modules);
        return result.toOwnedSlice(allocator);
    }

    /// Find the index of the primary module (platform main if present, otherwise app) in a module list.
    pub fn findPrimaryModuleIndex(modules: []const CompiledModuleInfo) ?usize {
        // First look for platform main
        for (modules, 0..) |mod, i| {
            if (mod.is_platform_main) return i;
        }
        // Fall back to app
        for (modules, 0..) |mod, i| {
            if (mod.is_app) return i;
        }
        return null;
    }

    /// Find the index of the app module in a module list.
    pub fn findAppModuleIndex(modules: []const CompiledModuleInfo) ?usize {
        for (modules, 0..) |mod, i| {
            if (mod.is_app) return i;
        }
        return null;
    }

    /// Get the root module env for the app package (convenience method).
    pub fn getAppEnv(self: *BuildEnv) ?*ModuleEnv {
        const sched = self.schedulers.get("app") orelse return null;
        return sched.getRootEnv();
    }

    /// Get the root module env for the platform package (convenience method).
    pub fn getPlatformEnv(self: *BuildEnv) ?*ModuleEnv {
        // Find platform package name
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (entry.value_ptr.kind == .platform) {
                const sched = self.schedulers.get(entry.key_ptr.*) orelse continue;
                return sched.getRootEnv();
            }
        }
        return null;
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
        if (comptime trace_build) {
            std.debug.print("[SINK] buildOrder: {} modules\n", .{pkg_names.len});
        }
        try self.order.ensureTotalCapacity(pkg_names.len);
        try self.entries.ensureTotalCapacity(pkg_names.len);
        try self.index.ensureTotalCapacity(@as(u32, @intCast(pkg_names.len)));

        // Rebuild order; allow pre-registered entries (from early emits) and update their metadata
        self.order.items.len = 0;

        var i: usize = 0;
        while (i < pkg_names.len) : (i += 1) {
            if (comptime trace_build) {
                std.debug.print("[SINK] buildOrder: checking pkg=\"{s}\" module=\"{s}\" depth={}\n", .{ pkg_names[i], module_names[i], depths[i] });
            }
            const key = ModuleKey{ .pkg = pkg_names[i], .module = module_names[i] };
            var entry_index: usize = undefined;
            if (self.index.get(key)) |idx| {
                if (comptime trace_build) {
                    std.debug.print("[SINK] buildOrder: found entry at idx={}, ready={} depth={}\n", .{ idx, self.entries.items[idx].ready, depths[i] });
                }
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

        if (comptime trace_build) {
            std.debug.print("[SINK] emitReport: pkg=\"{s}\" module=\"{s}\" title=\"{s}\"\n", .{ pkg_name, module_name, report.title });
        }

        // Lookup entry; auto-register if needed so we can buffer before order is built
        const key = ModuleKey{ .pkg = pkg_name, .module = module_name };
        var entry_index: usize = undefined;
        if (self.index.get(key)) |idx| {
            if (comptime trace_build) {
                std.debug.print("[SINK] emitReport: found existing entry at idx={}\n", .{idx});
            }
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
        if (comptime trace_build) {
            std.debug.print("[SINK] tryEmitLocked: order.len={}\n", .{self.order.items.len});
        }
        var i: usize = 0;
        while (i < self.order.items.len) : (i += 1) {
            const entry_idx = self.order.items[i];
            const e = &self.entries.items[entry_idx];

            if (comptime trace_build) {
                std.debug.print("[SINK] tryEmitLocked: i={} entry_idx={} pkg={s} mod={s} ready={} emitted={}\n", .{ i, entry_idx, e.pkg_name, e.module_name, e.ready, e.emitted });
            }

            // Prefix gating: stop at first entry that is not yet ready and not yet emitted
            if (!e.ready and !e.emitted) {
                if (comptime trace_build) {
                    std.debug.print("[SINK] tryEmitLocked: breaking at i={} (not ready and not emitted)\n", .{i});
                }
                break;
            }

            // Skip already-emitted entries
            if (e.emitted) continue;

            // Emit this ready entry
            if (comptime trace_build) {
                std.debug.print("[SINK] tryEmitLocked: marking i={} as emitted\n", .{i});
            }
            e.emitted = true;
        }
        if (comptime trace_build) {
            std.debug.print("[SINK] tryEmitLocked: done, processed {} entries\n", .{i});
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

        // Count only entries with reports (skip empty entries)
        var reports_count: usize = 0;
        {
            var k: usize = self.drain_cursor;
            while (k < i) : (k += 1) {
                const entry_idx = self.order.items[k];
                const e = &self.entries.items[entry_idx];
                if (e.reports.items.len > 0) {
                    reports_count += 1;
                }
            }
        }

        if (reports_count == 0) {
            self.drain_cursor = i;
            return try gpa.alloc(Drained, 0);
        }

        var out = try gpa.alloc(Drained, reports_count);
        var j: usize = 0;
        var k: usize = self.drain_cursor;
        while (k < i) : (k += 1) {
            const entry_idx = self.order.items[k];
            const e = &self.entries.items[entry_idx];

            // Skip entries with no reports
            if (e.reports.items.len == 0) {
                e.ready = false;
                e.emitted = false;
                continue;
            }

            // Move reports out; reset readiness for potential future appends
            const reps = e.reports.toOwnedSlice() catch {
                // Back out partially allocated results on failure
                var m: usize = 0;
                while (m < j) : (m += 1) {
                    for (out[m].reports) |*r| r.deinit();
                    gpa.free(out[m].reports);
                }
                gpa.free(out);
                return error.OutOfMemory;
            };

            out[j] = .{
                .pkg_name = e.pkg_name,
                .module_name = e.module_name,
                .reports = reps,
            };
            j += 1;

            // Reinitialize the reports ArrayList since toOwnedSlice() moved ownership
            e.reports = std.array_list.Managed(Report).init(self.gpa);
            e.ready = false;
            e.emitted = false;
        }

        self.drain_cursor = i;
        return out;
    }
};

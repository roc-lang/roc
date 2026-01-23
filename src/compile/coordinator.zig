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

const messages = @import("messages.zig");
const channel = @import("channel.zig");
const compile_package = @import("compile_package.zig");

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
        };
    }

    pub fn deinit(self: *ModuleState, gpa: Allocator) void {
        // Free cached AST if present
        if (self.cached_ast) |ast| {
            ast.deinit(gpa);
            gpa.destroy(ast);
        }

        // Free module env if present
        if (self.env) |env| {
            const source = env.common.source;
            env.deinit();
            gpa.destroy(env);
            if (source.len > 0) gpa.free(source);
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

    pub fn deinit(self: *PackageState, gpa: Allocator) void {
        for (self.modules.items) |*mod| {
            mod.deinit(gpa);
        }
        self.modules.deinit(gpa);
        self.module_names.deinit();

        var sh_it = self.shorthands.iterator();
        while (sh_it.next()) |entry| {
            gpa.free(entry.value_ptr.*);
        }
        self.shorthands.deinit();

        gpa.free(self.name);
        gpa.free(self.root_dir);
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

    /// Optional file provider for virtual files
    file_provider: ?FileProvider,

    /// Compiler version for cache keys
    compiler_version: []const u8,

    /// Timing accumulators
    total_parse_ns: u64,
    total_canonicalize_ns: u64,
    total_canonicalize_diag_ns: u64,
    total_typecheck_ns: u64,
    total_typecheck_diag_ns: u64,

    pub fn init(
        gpa: Allocator,
        mode: Mode,
        max_threads: usize,
        builtin_modules: *const BuiltinModules,
        compiler_version: []const u8,
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
            .file_provider = null,
            .compiler_version = compiler_version,
            .total_parse_ns = 0,
            .total_canonicalize_ns = 0,
            .total_canonicalize_diag_ns = 0,
            .total_typecheck_ns = 0,
            .total_typecheck_diag_ns = 0,
        };
    }

    pub fn deinit(self: *Coordinator) void {
        // Stop workers
        self.shutdown();

        // Free packages
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            entry.value_ptr.*.deinit(self.gpa);
            self.gpa.destroy(entry.value_ptr.*);
        }
        self.packages.deinit();

        // Free remaining tasks
        for (self.task_queue.items) |*task| {
            _ = task; // Tasks don't own memory, just references
        }
        self.task_queue.deinit(self.gpa);

        self.result_channel.deinit();
        self.workers.deinit(self.gpa);
    }

    /// Set a virtual file provider
    pub fn setFileProvider(self: *Coordinator, provider: ?FileProvider) void {
        self.file_provider = provider;
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

        self.task_mutex.lock();
        self.running = false;
        self.task_mutex.unlock();

        // Close the result channel to wake any blocked workers
        self.result_channel.close();

        // Send shutdown tasks to wake workers
        for (self.workers.items) |_| {
            self.task_queue.append(self.gpa, .{ .shutdown = {} }) catch {};
        }

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
        try self.task_queue.append(self.gpa, task);
    }

    /// Enqueue a parse task for a module
    pub fn enqueueParseTask(self: *Coordinator, pkg_name: []const u8, module_id: ModuleId) !void {
        const pkg = self.packages.get(pkg_name) orelse return;
        const mod = pkg.getModule(module_id) orelse return;

        try self.enqueueTask(.{
            .parse = .{
                .package_name = pkg_name,
                .module_id = module_id,
                .module_name = mod.name,
                .path = mod.path,
                .depth = mod.depth,
            },
        });
    }

    /// Main coordinator loop - unified for single and multi-threaded modes
    pub fn coordinatorLoop(self: *Coordinator) !void {
        while (!self.isComplete()) {
            if (!threads_available or self.mode == .single_threaded or self.max_threads <= 1) {
                // Single-threaded: process tasks inline
                if (self.task_queue.items.len > 0) {
                    const task = self.task_queue.pop();
                    const result = self.executeTaskInline(task);
                    try self.handleResult(result);
                }
            } else {
                // Multi-threaded: receive from workers via channel
                if (self.result_channel.tryRecv()) |result| {
                    self.inflight -= 1;
                    try self.handleResult(result);
                }

                // Dispatch pending work to workers
                try self.dispatchPendingWork();
            }
        }
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

    /// Dispatch pending tasks to workers
    fn dispatchPendingWork(self: *Coordinator) !void {
        if (!threads_available) return;

        self.task_mutex.lock();
        defer self.task_mutex.unlock();

        // Move tasks to workers by sending them via the task queue
        // Workers will pick them up
    }

    /// Handle a result from a worker
    fn handleResult(self: *Coordinator, result: WorkerResult) !void {
        switch (result) {
            .parsed => |r| try self.handleParsed(r),
            .canonicalized => |r| try self.handleCanonicalized(r),
            .type_checked => |r| try self.handleTypeChecked(r),
            .parse_failed => |r| try self.handleParseFailed(r),
            .cycle_detected => |r| try self.handleCycleDetected(r),
        }
    }

    /// Handle a successful parse result
    fn handleParsed(self: *Coordinator, result: ParsedResult) !void {
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        // Take ownership of module env and AST
        mod.env = result.module_env;
        mod.cached_ast = result.cached_ast;

        // Append reports
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }

        // Update timing
        self.total_parse_ns += result.parse_ns;

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
    fn handleCanonicalized(self: *Coordinator, result: CanonicalizedResult) !void {
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        // Take ownership of module env
        mod.env = result.module_env;
        mod.cached_ast = null; // AST was consumed during canonicalization

        // Append reports
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }

        // Update timing
        self.total_canonicalize_ns += result.canonicalize_ns;
        self.total_canonicalize_diag_ns += result.canonicalize_diagnostics_ns;

        // Mark as gray (visiting) for cycle detection
        mod.visit_color = 1;

        // Process discovered local imports
        for (result.discovered_local_imports.items) |imp| {
            const child_id = try pkg.ensureModule(self.gpa, imp.module_name, imp.path);
            try mod.imports.append(self.gpa, child_id);

            const child = pkg.getModule(child_id).?;
            try child.dependents.append(self.gpa, result.module_id);

            // Set depth
            if (mod.depth + 1 < child.depth) {
                child.depth = mod.depth + 1;
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

        // Process discovered external imports
        for (result.discovered_external_imports.items) |ext_imp| {
            try mod.external_imports.append(self.gpa, try self.gpa.dupe(u8, ext_imp.import_name));
            try self.scheduleExternalImport(result.package_name, ext_imp.import_name);
        }

        // Transition to WaitingOnImports
        mod.phase = .WaitingOnImports;

        // Try to unblock immediately
        try self.tryUnblock(pkg, result.module_id);
    }

    /// Handle a successful type-check result
    fn handleTypeChecked(self: *Coordinator, result: TypeCheckedResult) !void {
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        // Take ownership of module env
        mod.env = result.module_env;

        // Append reports
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }

        // Update timing
        self.total_typecheck_ns += result.type_check_ns;
        self.total_typecheck_diag_ns += result.check_diagnostics_ns;

        // Mark as done
        mod.phase = .Done;
        mod.visit_color = 2; // Black

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake dependents
        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }
    }

    /// Handle a parse failure
    fn handleParseFailed(self: *Coordinator, result: messages.ParseFailure) !void {
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        // Store partial env if available
        if (result.partial_env) |env| {
            mod.env = env;
        }

        // Append reports
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }

        // Mark as done (with errors)
        mod.phase = .Done;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake dependents (they'll see this module as done with errors)
        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }
    }

    /// Handle cycle detection
    fn handleCycleDetected(self: *Coordinator, result: messages.CycleDetected) !void {
        const pkg = self.packages.get(result.package_name) orelse return;
        const mod = pkg.getModule(result.module_id) orelse return;

        // Take ownership of module env
        mod.env = result.module_env;

        // Append reports
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }

        // Mark as done (with cycle error)
        mod.phase = .Done;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;
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
            if (imp.phase != .Done) return; // Not ready yet
        }

        // Check external imports
        for (mod.external_imports.items) |ext_name| {
            if (!self.isExternalReady(pkg.name, ext_name)) return;
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
    pub fn scheduleExternalImport(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) !void {
        // Parse "pf.Stdout" -> qual="pf", rest="Stdout"
        const dot_idx = std.mem.indexOfScalar(u8, import_name, '.') orelse return;
        const qual = import_name[0..dot_idx];
        const rest = import_name[dot_idx + 1 ..];

        // Resolve shorthand to target package
        const source = self.packages.get(source_pkg) orelse return;
        const target_pkg_name = source.shorthands.get(qual) orelse return;

        // Get or create module in target package
        const target_pkg = self.packages.get(target_pkg_name) orelse return;
        const path = try self.resolveModulePath(target_pkg.root_dir, rest);
        defer self.gpa.free(path);

        const module_id = try target_pkg.ensureModule(self.gpa, rest, path);
        const mod = target_pkg.getModule(module_id).?;

        // Queue parse if new
        if (mod.phase == .Parse) {
            target_pkg.remaining_modules += 1;
            self.total_remaining += 1;
            try self.enqueueParseTask(target_pkg_name, module_id);
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

    // ========================================================================
    // Pure execution functions (used by workers and inline processing)
    // ========================================================================

    /// Execute a parse task (pure function)
    fn executeParse(self: *Coordinator, task: ParseTask) WorkerResult {
        const start_time = if (threads_available) std.time.nanoTimestamp() else 0;

        // Read source
        const src = self.readModuleSource(task.path) catch |err| {
            var reports = std.ArrayList(Report).empty;
            var rep = Report.init(self.gpa, "FILE NOT FOUND", .fatal);
            rep.addErrorMessage(@errorName(err)) catch {};
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

        // Create ModuleEnv
        const env = self.gpa.create(ModuleEnv) catch {
            self.gpa.free(src);
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

        env.* = ModuleEnv.init(self.gpa, src) catch {
            self.gpa.destroy(env);
            self.gpa.free(src);
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
        env.initCIRFields(task.module_name) catch {};
        env.common.calcLineStarts(self.gpa) catch {};

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

        // Canonicalize using the PackageEnv shared function
        compile_package.PackageEnv.canonicalizeModule(
            self.gpa,
            env,
            ast,
            self.builtin_modules.builtin_module.env,
            self.builtin_modules.builtin_indices,
        ) catch {};

        const canon_end = if (threads_available) std.time.nanoTimestamp() else 0;

        // Collect diagnostics
        const diag_start = if (threads_available) std.time.nanoTimestamp() else 0;
        var reports = std.ArrayList(Report).empty;
        const diags = env.getDiagnostics() catch &[_]can.ModuleEnv.Diagnostic{};
        defer self.gpa.free(diags);
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
                // Local import
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

    /// Read module source from filesystem or file provider
    fn readModuleSource(self: *Coordinator, path: []const u8) ![]u8 {
        const raw_data = if (self.file_provider) |fp|
            if (try fp.read(fp.ctx, path, self.gpa)) |data| data else null
        else
            null;

        const data = raw_data orelse try std.fs.cwd().readFileAlloc(self.gpa, path, std.math.maxInt(usize));

        // Normalize line endings
        return base.source_utils.normalizeLineEndingsRealloc(self.gpa, data);
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
                    std.time.sleep(1_000_000); // 1ms
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

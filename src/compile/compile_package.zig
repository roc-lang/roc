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
const Can = @import("can").Can;
const Check = @import("check").Check;
const reporting = @import("reporting");

const Report = reporting.Report;
const ModuleEnv = @import("ModuleEnv.zig");
const problem = Check.problem;

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
    /// Return true if the import_name refers to an external package (e.g. "cli.Stdout")
    classify: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) bool,
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
    imports: std.ArrayListUnmanaged(ModuleId) = .{},
    /// External imports qualified via package shorthand (e.g. "cli.Stdout") - still strings as they reference other packages
    external_imports: std.ArrayListUnmanaged([]const u8) = .{},
    dependents: std.ArrayListUnmanaged(ModuleId) = .{},
    reports: std.ArrayListUnmanaged(Report) = .{},
    depth: u32 = std.math.maxInt(u32), // min depth from root
    /// DFS visitation color for cycle detection: 0=white (unvisited), 1=gray (visiting), 2=black (finished)
    visit_color: u8 = 0,
    /// Atomic flag to prevent concurrent processing of the same module (0=free, 1=working)
    working: if (@import("builtin").target.cpu.arch != .wasm32) std.atomic.Value(u8) else u8 = if (@import("builtin").target.cpu.arch != .wasm32) std.atomic.Value(u8).init(0) else 0,

    fn deinit(self: *ModuleState, gpa: Allocator) void {
        if (self.env) |*e| e.deinit();
        self.imports.deinit(gpa);
        self.external_imports.deinit(gpa);
        self.dependents.deinit(gpa);
        // Free reports
        for (self.reports.items) |*r| r.deinit();
        self.reports.deinit(gpa);
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
    sink: ReportSink,
    /// Optional resolver for cross-package imports; when null, all imports are treated as local
    resolver: ?ImportResolver = null,
    /// Scheduling hook to observe/enqueue scheduled modules in a global orchestrator
    schedule_hook: ScheduleHook,
    /// Compiler version for cache invalidation
    compiler_version: []const u8,

    lock: Mutex = .{},
    cond: Condition = .{},

    // Work queue
    injector: std.ArrayListUnmanaged(Task) = .{},

    // Module storage
    modules: std.ArrayListUnmanaged(ModuleState) = .{},
    // String intern table: module name -> module ID
    module_names: std.StringHashMapUnmanaged(ModuleId) = .{},

    // Build status
    remaining_modules: usize = 0,

    // Track module discovery order and which modules have had their reports emitted
    discovered: std.ArrayListUnmanaged(ModuleId) = .{},
    emitted: std.bit_set.DynamicBitSetUnmanaged = .{},

    // Timing collection (accumulated across all modules)
    total_tokenize_parse_ns: u64 = 0,
    total_canonicalize_ns: u64 = 0,
    total_canonicalize_diagnostics_ns: u64 = 0,
    total_type_checking_ns: u64 = 0,
    total_check_diagnostics_ns: u64 = 0,

    pub fn init(gpa: Allocator, package_name: []const u8, root_dir: []const u8, mode: Mode, max_threads: usize, sink: ReportSink, schedule_hook: ScheduleHook, compiler_version: []const u8) PackageEnv {
        return .{ .gpa = gpa, .package_name = package_name, .root_dir = root_dir, .mode = mode, .max_threads = max_threads, .sink = sink, .schedule_hook = schedule_hook, .compiler_version = compiler_version };
    }

    pub fn initWithResolver(
        gpa: Allocator,
        package_name: []const u8,
        root_dir: []const u8,
        mode: Mode,
        max_threads: usize,
        sink: ReportSink,
        resolver: ImportResolver,
        schedule_hook: ScheduleHook,
        compiler_version: []const u8,
    ) PackageEnv {
        return .{
            .gpa = gpa,
            .package_name = package_name,
            .root_dir = root_dir,
            .mode = mode,
            .max_threads = max_threads,
            .sink = sink,
            .resolver = resolver,
            .schedule_hook = schedule_hook,
            .compiler_version = compiler_version,
        };
    }

    pub fn deinit(self: *PackageEnv) void {
        // Deinit modules
        for (self.modules.items) |*ms| {
            ms.deinit(self.gpa);
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
        const module_id = try self.ensureModule(name, root_file_path);
        // root depth = 0
        try self.setDepthIfSmaller(module_id, 0);
        self.remaining_modules = 1;
        try self.enqueue(module_id);

        // Notify schedule hook so a global queue can pick this up
        self.schedule_hook.onSchedule(self.schedule_hook.ctx, self.package_name, name, root_file_path, 0);

        // If a global schedule_hook is installed, do not start internal scheduling loops.
        // In dispatch-only mode, the unified global queue will drive processing via process().
        if (self.schedule_hook.isNoOp()) {
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
            } else if (self.remaining_modules == 0) break;
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

    fn ensureModule(self: *PackageEnv, name: []const u8, path: []const u8) !ModuleId {
        const module_id = try self.internModuleName(name);

        // Check if we need to create a new module
        if (module_id >= self.modules.items.len) {
            // This is a new module
            const owned_path = try self.gpa.dupe(u8, path);
            const owned_name = self.module_names.getKey(name).?; // We just interned it
            try self.modules.append(self.gpa, .{ .name = owned_name, .path = owned_path });
            try self.discovered.append(self.gpa, module_id);

            // Invoke scheduling hook for new module discovery/scheduling
            self.schedule_hook.onSchedule(self.schedule_hook.ctx, self.package_name, owned_name, owned_path, 0);
        }

        return module_id;
    }

    /// Public API for cross-package schedulers: ensure a module exists, set its depth, and enqueue it
    pub fn scheduleModule(self: *PackageEnv, name: []const u8, path: []const u8, depth: u32) !void {
        const module_id = try self.ensureModule(name, path);
        const existed = module_id < self.modules.items.len - 1; // If it wasn't the last one added
        try self.setDepthIfSmaller(module_id, depth);
        if (!existed) {
            self.remaining_modules += 1;
            // Invoke scheduling hook for external scheduling
            self.schedule_hook.onSchedule(self.schedule_hook.ctx, self.package_name, name, path, depth);
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
        // In multi_threaded mode with a non-noop schedule_hook, forward to the global queue
        if (self.mode == .multi_threaded and !self.schedule_hook.isNoOp()) {
            // Look up the module to get its path and depth for the hook
            self.lock.lock();
            defer self.lock.unlock();

            const st = &self.modules.items[module_id];
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
            .Parse => try self.doParse(task.module_id),
            .Canonicalize => try self.doCanonicalize(task.module_id),
            .WaitingOnImports => try self.tryUnblock(task.module_id),
            .TypeCheck => try self.doTypeCheck(task.module_id),
            .Done => return,
        }
        try self.tryEmitReady();
    }

    fn doParse(self: *PackageEnv, module_id: ModuleId) !void {
        // Load source and init ModuleEnv
        var st = &self.modules.items[module_id];
        const src = try std.fs.cwd().readFileAlloc(self.gpa, st.path, std.math.maxInt(usize));

        var env = try ModuleEnv.init(self.gpa, src);
        // line starts for diagnostics and consistent positions
        try env.calcLineStarts();
        // init CIR fields
        try env.initCIRFields(self.gpa, st.name);

        // replace env
        if (st.env) |*old| old.deinit();
        st.env = env;

        st.phase = .Canonicalize;
        try self.enqueue(module_id);
    }

    fn doCanonicalize(self: *PackageEnv, module_id: ModuleId) !void {
        var st = &self.modules.items[module_id];
        var env = st.env.?;

        // Parse and canonicalize in one step to avoid double parsing
        const parse_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        var parse_ast = try parse.parse(&env);
        defer parse_ast.deinit(self.gpa);
        parse_ast.store.emptyScratch();
        const parse_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_tokenize_parse_ns += @intCast(parse_end - parse_start);
        }

        // canonicalize using the AST
        const canon_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        var can = try Can.init(&env, &parse_ast, null);
        try can.canonicalizeFile();
        can.deinit();
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
            const mod_name = env.strings.get(str_idx);

            // Use CIR qualifier metadata instead of heuristic; this allocates nothing and scans only once
            const qualified = hadQualifiedImport(&env, mod_name);

            if (qualified) {
                // Qualified imports refer to external packages; track and schedule externally
                try st.external_imports.append(self.gpa, mod_name);
                if (self.resolver) |r| r.scheduleExternal(r.ctx, self.package_name, mod_name);
                // External dependencies are resolved by the workspace; skip local scheduling/cycle detection
                continue;
            }

            // Local import - schedule in this package
            const import_path = try self.resolveModulePath(mod_name);
            const child_id = try self.ensureModule(mod_name, import_path);
            const existed = child_id < self.modules.items.len - 1;
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
                    st.phase = .Done;
                    self.remaining_modules -= 1;
                }
                if (child.phase != .Done) {
                    child.phase = .Done;
                    if (self.remaining_modules > 0) self.remaining_modules -= 1;
                }

                // Wake dependents and stop
                for (st.dependents.items) |dep| try self.enqueue(dep);
                for (child.dependents.items) |dep| try self.enqueue(dep);
                if (@import("builtin").target.cpu.arch != .wasm32) self.cond.broadcast();
                return;
            }

            if (!existed) {
                self.remaining_modules += 1;
                any_new = true;
            }
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
            st.phase = .TypeCheck;
            // Mark as finished (black) when all children done
            st.visit_color = 2;
            try self.enqueue(module_id);
        }
    }

    fn doTypeCheck(self: *PackageEnv, module_id: ModuleId) !void {
        var st = &self.modules.items[module_id];
        var env = st.env.?;

        // Build other_modules array according to env.imports order
        const import_count = env.imports.imports.items.items.len;
        var others = try std.ArrayList(*ModuleEnv).initCapacity(self.gpa, import_count);
        defer others.deinit();
        for (env.imports.imports.items.items[0..import_count]) |str_idx| {
            const import_name = env.strings.get(str_idx);
            // Determine external vs local from CIR s_import qualifier metadata directly
            const is_ext = hadQualifiedImport(&env, import_name);

            if (is_ext) {
                if (self.resolver) |r| {
                    if (r.getEnv(r.ctx, self.package_name, import_name)) |ext_env_ptr| {
                        const box = try self.gpa.create(ModuleEnv);
                        box.* = ext_env_ptr.*;
                        try others.append(box);
                    } else {
                        // External env not ready; skip (tryUnblock should have prevented this)
                    }
                }
            } else {
                const child_id = self.module_names.get(import_name).?;
                const child = &self.modules.items[child_id];
                const child_env_ptr = child.env.?; // value copy
                // Ensure pointer stable by storing in a temporary allocation
                const box = try self.gpa.create(ModuleEnv);
                box.* = child_env_ptr;
                try others.append(box);
            }
        }

        var checker = try Check.init(self.gpa, &env.types, &env, others.items, &env.store.regions);
        defer checker.deinit();
        // Note: checkDefs runs type checking for module
        const check_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        try checker.checkDefs();
        const check_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_type_checking_ns += @intCast(check_end - check_start);
        }

        // Build reports from problems
        const check_diag_start = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        var rb = problem.ReportBuilder.init(self.gpa, &env, &env, &checker.snapshots, st.path, others.items);
        defer rb.deinit();
        for (checker.problems.problems.items) |prob| {
            const rep = rb.build(prob) catch continue;
            try st.reports.append(self.gpa, rep);
        }
        const check_diag_end = if (@import("builtin").target.cpu.arch != .wasm32) std.time.nanoTimestamp() else 0;
        if (@import("builtin").target.cpu.arch != .wasm32) {
            self.total_check_diagnostics_ns += @intCast(check_diag_end - check_diag_start);
        }

        // Free temporary ModuleEnv copies created for others
        for (others.items) |ptr| self.gpa.destroy(ptr);

        // Done
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
        var buffer = std.ArrayList(u8).init(self.gpa);
        defer buffer.deinit();
        var it = std.mem.splitScalar(u8, mod_name, '.');
        var first = true;
        while (it.next()) |part| {
            if (!first) try buffer.appendSlice(std.fs.path.sep_str) else first = false;
            try buffer.appendSlice(part);
        }
        try buffer.appendSlice(".roc");
        const rel = try buffer.toOwnedSlice();
        const full = try std.fs.path.join(self.gpa, &.{ self.root_dir, rel });
        self.gpa.free(rel);
        return full;
    }

    // Determine if an import was qualified (e.g. "cli.Stdout") using CIR statements.
    // Scans only top-level s_import statements; no allocations; exits early on match.
    fn hadQualifiedImport(env: *ModuleEnv, mod_name_text: []const u8) bool {
        const stmts = env.store.sliceStatements(env.all_statements);
        for (stmts) |stmt_idx| {
            const stmt = env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_import => |imp| {
                    const imported_text = env.getIdent(imp.module_name_tok);
                    if (!std.mem.eql(u8, imported_text, mod_name_text)) continue;
                    // If qualifier_tok is set, the import was package-qualified in source
                    if (imp.qualifier_tok != null) return true;
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
        var frames = std.ArrayList(Frame).init(self.gpa);
        defer frames.deinit();

        var stack_ids = std.ArrayList(ModuleId).init(self.gpa);
        defer stack_ids.deinit();

        visited.set(start);
        try frames.append(.{ .id = start, .next_idx = 0 });
        try stack_ids.append(start);

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
                try frames.append(.{ .id = child, .next_idx = 0 });
                try stack_ids.append(child);
            }
        }
        return null;
    }

    fn moduleNameFromPath(path: []const u8) []const u8 {
        const base_name = std.fs.path.basename(path);
        if (std.mem.lastIndexOfScalar(u8, base_name, '.')) |dot| return base_name[0..dot];
        return base_name;
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
            // Mark emitted
            self.emitted.set(id);
        }
    }
};

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
const Can = @import("can");
const Check = @import("check");
const problem = Check.problem;
const compile = @import("compile");
pub const ModuleEnv = compile.ModuleEnv;
const Report = @import("reporting").Report;

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

const Task = struct { module_name: []const u8 };

const Phase = enum { Parse, Canonicalize, WaitingOnImports, TypeCheck, Done };

const ModuleState = struct {
    name: []const u8,
    path: []const u8,
    env: ?ModuleEnv = null,
    phase: Phase = .Parse,
    imports: std.ArrayListUnmanaged([]const u8) = .{},
    /// External imports qualified via package shorthand (e.g. "cli.Stdout")
    external_imports: std.ArrayListUnmanaged([]const u8) = .{},
    dependents: std.ArrayListUnmanaged([]const u8) = .{},
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
pub const ModuleBuild = struct {
    gpa: Allocator,
    /// Name of the current package (used for shorthand resolution)
    package_name: []const u8 = "",
    root_dir: []const u8,
    mode: Mode,
    max_threads: usize,
    sink: ReportSink,
    /// Optional resolver for cross-package imports; when null, all imports are treated as local
    resolver: ?ImportResolver = null,
    /// Optional scheduling hook to observe/enqueue scheduled modules in a global orchestrator
    schedule_hook: ?ScheduleHook = null,
    /// Compiler version for cache invalidation
    compiler_version: []const u8 = "roc-zig-dev",

    lock: Mutex = .{},
    cond: Condition = .{},

    // Work queue
    injector: std.ArrayListUnmanaged(Task) = .{},

    // Modules by name
    modules: std.StringHashMapUnmanaged(ModuleState) = .{},

    // Build status
    remaining_modules: usize = 0,

    // Deterministic emission
    discovered: std.ArrayListUnmanaged([]const u8) = .{},
    emitted: std.StringHashMapUnmanaged(void) = .{},

    // Timing collection (accumulated across all modules)
    total_tokenize_parse_ns: u64 = 0,
    total_canonicalize_ns: u64 = 0,
    total_canonicalize_diagnostics_ns: u64 = 0,
    total_type_checking_ns: u64 = 0,
    total_check_diagnostics_ns: u64 = 0,

    pub fn init(gpa: Allocator, root_dir: []const u8, mode: Mode, max_threads: usize, sink: ReportSink) ModuleBuild {
        return .{ .gpa = gpa, .root_dir = root_dir, .mode = mode, .max_threads = max_threads, .sink = sink };
    }

    pub fn initWithResolver(
        gpa: Allocator,
        package_name: []const u8,
        root_dir: []const u8,
        mode: Mode,
        max_threads: usize,
        sink: ReportSink,
        resolver: ImportResolver,
    ) ModuleBuild {
        return .{
            .gpa = gpa,
            .package_name = package_name,
            .root_dir = root_dir,
            .mode = mode,
            .max_threads = max_threads,
            .sink = sink,
            .resolver = resolver,
        };
    }

    pub fn deinit(self: *ModuleBuild) void {
        // Deinit modules
        var it = self.modules.iterator();
        while (it.next()) |e| {
            var ms = e.value_ptr.*;
            ms.deinit(self.gpa);
        }
        self.modules.deinit(self.gpa);
        self.injector.deinit(self.gpa);
        self.discovered.deinit(self.gpa);
        self.emitted.deinit(self.gpa);
    }

    pub fn buildRoot(self: *ModuleBuild, root_file_path: []const u8) !void {
        const name = moduleNameFromPath(root_file_path);
        try self.ensureModule(name, root_file_path);
        // root depth = 0
        try self.setDepthIfSmaller(name, 0);
        self.remaining_modules = 1;
        try self.enqueue(name);

        // Notify schedule hook so a global queue can pick this up
        if (self.schedule_hook) |hook| {
            hook.onSchedule(hook.ctx, self.package_name, name, root_file_path, 0);
        }

        // If a global schedule_hook is installed, do not start internal scheduling loops.
        // In dispatch-only mode, the unified global queue will drive processing via process().
        if (self.schedule_hook == null) {
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

    fn runSingleThread(self: *ModuleBuild) !void {
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

    fn runMultiThread(self: *ModuleBuild) !void {
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

    const WorkerCtx = struct { sched: *ModuleBuild, index: *AtomicUsize, work_len: usize };

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

    fn ensureModule(self: *ModuleBuild, name: []const u8, path: []const u8) !void {
        const gop = try self.modules.getOrPut(self.gpa, name);
        if (!gop.found_existing) {
            // Own the name and path
            const owned_name = try self.gpa.dupe(u8, name);
            const owned_path = try self.gpa.dupe(u8, path);
            gop.key_ptr.* = owned_name;
            gop.value_ptr.* = .{ .name = owned_name, .path = owned_path };
            try self.discovered.append(self.gpa, owned_name);

            // Invoke optional scheduling hook for new module discovery/scheduling
            if (self.schedule_hook) |hook| {
                hook.onSchedule(hook.ctx, self.package_name, owned_name, owned_path, 0);
            }
        }
    }

    /// Public API for cross-package schedulers: ensure a module exists, set its depth, and enqueue it
    pub fn scheduleModule(self: *ModuleBuild, name: []const u8, path: []const u8, depth: u32) !void {
        const existed = self.modules.contains(name);
        try self.ensureModule(name, path);
        try self.setDepthIfSmaller(name, depth);
        if (!existed) {
            self.remaining_modules += 1;
            // Invoke optional scheduling hook for external scheduling
            if (self.schedule_hook) |hook| {
                hook.onSchedule(hook.ctx, self.package_name, name, path, depth);
            }
        }
        try self.enqueue(name);
    }

    fn setDepthIfSmaller(self: *ModuleBuild, name: []const u8, depth: u32) !void {
        const st = self.modules.getPtr(name).?;
        if (depth < st.depth) st.depth = depth;
    }

    /// Public API to adjust a module's depth from an external coordinator
    pub fn setModuleDepthIfSmaller(self: *ModuleBuild, name: []const u8, depth: u32) !void {
        try self.setDepthIfSmaller(name, depth);
    }

    fn enqueue(self: *ModuleBuild, name: []const u8) !void {
        // In multi_threaded mode with a schedule_hook, forward to the global queue
        if (self.mode == .multi_threaded and self.schedule_hook != null) {
            // Look up the module to get its path and depth for the hook
            self.lock.lock();
            defer self.lock.unlock();

            if (self.modules.get(name)) |st| {
                const hook = self.schedule_hook.?;
                hook.onSchedule(hook.ctx, self.package_name, name, st.path, st.depth);
            }
        } else {
            // Default behavior: use internal injector
            try self.injector.append(self.gpa, .{ .module_name = name });
            if (@import("builtin").target.cpu.arch != .wasm32) self.cond.signal();
        }
    }

    /// Public API to obtain a module's environment if it has completed type-checking
    pub fn getEnvIfDone(self: *ModuleBuild, name: []const u8) ?*ModuleEnv {
        if (self.modules.getPtr(name)) |st| {
            if (st.phase == .Done) {
                if (st.env) |*e| {
                    return e;
                }
            }
        }
        return null;
    }

    /// Get accumulated timing information
    pub fn getTimingInfo(self: *ModuleBuild) TimingInfo {
        return TimingInfo{
            .tokenize_parse_ns = self.total_tokenize_parse_ns,
            .canonicalize_ns = self.total_canonicalize_ns,
            .canonicalize_diagnostics_ns = self.total_canonicalize_diagnostics_ns,
            .type_checking_ns = self.total_type_checking_ns,
            .check_diagnostics_ns = self.total_check_diagnostics_ns,
        };
    }

    /// Public API to read a module's current recorded dependency depth
    pub fn getModuleDepth(self: *ModuleBuild, name: []const u8) ?u32 {
        if (self.modules.getPtr(name)) |st| {
            return st.depth;
        }
        return null;
    }

    pub fn process(self: *ModuleBuild, task: Task) !void {
        // In dispatch-only mode, this method is invoked by the global scheduler.
        // In local mode, it's invoked by the internal run* loops.

        // Acquire lock and atomically check/set working flag
        if (@import("builtin").target.cpu.arch != .wasm32) self.lock.lock();
        const st = self.modules.getPtr(task.module_name).?;

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
                if (self.modules.getPtr(task.module_name)) |state| {
                    _ = state.working.store(0, .seq_cst);
                }
                self.lock.unlock();
            } else {
                // Single-threaded: simple clear
                if (self.modules.getPtr(task.module_name)) |state| {
                    state.working = 0;
                }
            }
        }

        switch (phase) {
            .Parse => try self.doParse(task.module_name),
            .Canonicalize => try self.doCanonicalize(task.module_name),
            .WaitingOnImports => try self.tryUnblock(task.module_name),
            .TypeCheck => try self.doTypeCheck(task.module_name),
            .Done => return,
        }
        try self.tryEmitReady();
    }

    fn doParse(self: *ModuleBuild, name: []const u8) !void {
        // Load source and init ModuleEnv
        var st = self.modules.getPtr(name).?;
        const src = try std.fs.cwd().readFileAlloc(self.gpa, st.path, std.math.maxInt(usize));

        var env = try ModuleEnv.init(self.gpa, src);
        // line starts for diagnostics and consistent positions
        try env.calcLineStarts();
        // init CIR fields
        try env.initCIRFields(self.gpa, name);

        // replace env
        if (st.env) |*old| old.deinit();
        st.env = env;

        st.phase = .Canonicalize;
        try self.enqueue(name);
    }

    fn doCanonicalize(self: *ModuleBuild, name: []const u8) !void {
        var st = self.modules.getPtr(name).?;
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
            try st.imports.append(self.gpa, mod_name);
            const import_path = try self.resolveModulePath(mod_name);
            const existed = self.modules.contains(mod_name);
            try self.ensureModule(mod_name, import_path);
            // parent depth + 1
            try self.setDepthIfSmaller(mod_name, st.depth + 1);

            // Cycle detection for local deps
            var child = self.modules.getPtr(mod_name).?;
            try child.dependents.append(self.gpa, name);

            if (child.visit_color == 1 or std.mem.eql(u8, mod_name, name)) {
                // Build a report on the current module describing the cycle
                var rep = Report.init(self.gpa, "Import cycle detected", .runtime_error);
                const msg = try rep.addOwnedString("This module participates in an import cycle. Cycles between modules are not allowed.");
                try rep.addErrorMessage(msg);

                // Build full cycle path lazily (rare path): mod_name ... name -> mod_name
                if (try self.findPath(mod_name, name)) |path| {
                    defer self.gpa.free(path);
                    const hdr = try rep.addOwnedString("Cycle: ");
                    try rep.document.addText(hdr);
                    var i: usize = 0;
                    while (i < path.len) : (i += 1) {
                        if (i > 0) try rep.document.addText(" -> ");
                        try rep.document.addAnnotated(path[i], .emphasized);
                    }
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(path[0], .emphasized);
                    try rep.document.addLineBreak();
                } else {
                    // Fallback: show the detected back-edge
                    const edge_msg = try rep.addOwnedString("Cycle edge: ");
                    try rep.document.addText(edge_msg);
                    try rep.document.addAnnotated(name, .emphasized);
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
                try rep_child.document.addAnnotated(name, .emphasized);
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
        try self.enqueue(name);
    }

    fn tryUnblock(self: *ModuleBuild, name: []const u8) !void {
        var st = self.modules.getPtr(name).?;
        // If all imports are Done, move to TypeCheck
        var ready = true;

        // Local imports must be done
        for (st.imports.items) |imp| {
            const child = self.modules.getPtr(imp).?;
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
            try self.enqueue(name);
        }
    }

    fn doTypeCheck(self: *ModuleBuild, name: []const u8) !void {
        var st = self.modules.getPtr(name).?;
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
                const child = self.modules.getPtr(import_name).?;
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

    fn resolveModulePath(self: *ModuleBuild, mod_name: []const u8) ![]const u8 {
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
                    const imported_text = env.idents.getText(imp.module_name_tok);
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
    // Returns an owned slice of module names (not owning the names themselves).
    // NOTE: This returns strings that live in ModuleEnv.strings; do not free them.
    fn findPath(self: *ModuleBuild, start: []const u8, target: []const u8) !?[]const []const u8 {
        var visited = std.StringHashMapUnmanaged(void){};
        defer visited.deinit(self.gpa);

        const Frame = struct { name: []const u8, next_idx: usize };
        var frames = std.ArrayList(Frame).init(self.gpa);
        defer frames.deinit();

        var stack_names = std.ArrayList([]const u8).init(self.gpa);
        defer stack_names.deinit();

        try visited.put(self.gpa, start, {});
        try frames.append(.{ .name = start, .next_idx = 0 });
        try stack_names.append(start);

        while (frames.items.len > 0) {
            var top = &frames.items[frames.items.len - 1];
            if (std.mem.eql(u8, top.name, target)) {
                const out = try self.gpa.alloc([]const u8, stack_names.items.len);
                std.mem.copyForwards([]const u8, out, stack_names.items);
                return out;
            }

            const st = self.modules.getPtr(top.name) orelse break;
            if (top.next_idx >= st.imports.items.len) {
                _ = visited.remove(top.name);
                _ = stack_names.pop();
                _ = frames.pop();
                continue;
            }

            const child = st.imports.items[top.next_idx];
            top.next_idx += 1;

            if (!visited.contains(child)) {
                try visited.put(self.gpa, child, {});
                try frames.append(.{ .name = child, .next_idx = 0 });
                try stack_names.append(child);
            }
        }
        return null;
    }

    fn moduleNameFromPath(path: []const u8) []const u8 {
        const base_name = std.fs.path.basename(path);
        if (std.mem.lastIndexOfScalar(u8, base_name, '.')) |dot| return base_name[0..dot];
        return base_name;
    }

    pub fn tryEmitReady(self: *ModuleBuild) !void {
        // Sort discovered modules by (depth, name) each time; emit in prefix order
        if (self.discovered.items.len == 0) return;
        const names = try self.gpa.alloc([]const u8, self.discovered.items.len);
        defer self.gpa.free(names);
        std.mem.copyForwards([]const u8, names, self.discovered.items);
        std.sort.block([]const u8, names, self, struct {
            fn lessThan(ctx: *ModuleBuild, a: []const u8, b: []const u8) bool {
                const sa = ctx.modules.getPtr(a).?.depth;
                const sb = ctx.modules.getPtr(b).?.depth;
                if (sa == sb) return std.mem.lessThan(u8, a, b);
                return sa < sb;
            }
        }.lessThan);

        for (names) |n| {
            if (self.emitted.contains(n)) continue;
            const st = self.modules.getPtr(n).?;
            if (st.phase != .Done) break; // can't emit beyond an unfinished module in order
            // Emit all reports for this module
            for (st.reports.items) |rep| self.sink.emitFn(self.sink.ctx, n, rep);
            // Mark emitted
            try self.emitted.put(self.gpa, n, {});
        }
    }
};

// =========================
// Tests using tempdirs
// =========================

test "ModuleBuild: parallel success across modules" {
    const gpa = std.testing.allocator;

    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // Layout:
    // root_dir/Main.roc imports A and B; A imports C. All succeed.
    const root_dir = tmp.dir.path;

    try write(tmp.dir, "Main.roc", "import A\n" ++ "import B\n\n" ++ "main = A.val + B.one\n");

    try write(tmp.dir, "A.roc", "import C\n" ++ "val = C.ten\n");

    try write(tmp.dir, "B.roc", "one = 1\n");

    try write(tmp.dir, "C.roc", "ten = 10\n");

    var sink = TestSink.init(gpa);
    defer sink.deinit();

    var sched = ModuleBuild.init(gpa, root_dir, .multi_threaded, 4, sink.sink());
    defer sched.deinit();

    const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
    defer gpa.free(main_path);

    try sched.buildRoot(main_path);

    // No reports expected
    try std.testing.expectEqual(@as(usize, 0), sink.reports.items.len);
}

test "ModuleBuild: deterministic error ordering by depth then name" {
    const gpa = std.testing.allocator;

    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;

    // Main imports X and A; X imports Y.
    // Introduce errors in A and Y; Y should come before A by depth (Y depth=2, A depth=1) -> actually A should come first (depth 1), then Y (depth 2).

    try write(tmp.dir, "Main.roc", "import X\n" ++ "import A\n\n" ++ "main = X.other + A.bad\n");

    try write(tmp.dir, "X.roc", "import Y\n" ++ "other = Y.val\n");

    // A has a type mismatch: use string where number expected
    try write(tmp.dir, "A.roc", "bad = \"oops\"\n" ++ "bad2 = bad + 1\n");

    // Y has invalid if condition to produce error
    try write(tmp.dir, "Y.roc", "val = if 42 then 1 else 2\n");

    var sink = TestSink.init(gpa);
    defer sink.deinit();

    var sched = ModuleBuild.init(gpa, root_dir, .multi_threaded, 4, sink.sink());
    defer sched.deinit();

    const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
    defer gpa.free(main_path);

    try sched.buildRoot(main_path);

    // Expect reports from A (depth 1) then Y (depth 2) in module-name alphabetical within same depth
    try std.testing.expect(sink.reports.items.len >= 2);
    try std.testing.expect(std.mem.eql(u8, sink.modules.items[0], "A"));
    // Y must appear later
    var found_y = false;
    for (sink.modules.items[1..]) |m| {
        if (std.mem.eql(u8, m, "Y")) {
            found_y = true;
            break;
        }
    }
    try std.testing.expect(found_y);
}

// Simple sink collecting reports and module names in order of emission
test "ModuleBuild: single-threaded success across modules" {
    const gpa = std.testing.allocator;

    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;

    try write(tmp.dir, "Main.roc", "import A\n" ++ "import B\n\n" ++ "main = A.v + B.w\n");
    try write(tmp.dir, "A.roc", "v = 2\n");
    try write(tmp.dir, "B.roc", "w = 3\n");

    var sink = TestSink.init(gpa);
    defer sink.deinit();

    var sched = ModuleBuild.init(gpa, root_dir, .single_threaded, 1, sink.sink());
    defer sched.deinit();

    const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
    defer gpa.free(main_path);

    try sched.buildRoot(main_path);

    // No reports expected
    try std.testing.expectEqual(@as(usize, 0), sink.reports.items.len);
}

test "ModuleBuild: same-depth alphabetical order" {
    const gpa = std.testing.allocator;

    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;

    // Main imports B and A; both have errors; order should be A then B (alphabetical) since same depth.
    try write(tmp.dir, "Main.roc", "import B\n" ++ "import A\n\n" ++ "main = A.bad + B.bad\n");

    // Both A and B produce type errors
    try write(tmp.dir, "A.roc", "bad = \"str\" + 1\n");
    try write(tmp.dir, "B.roc", "bad = if 0 then 1 else 2\n");

    var sink = TestSink.init(gpa);
    defer sink.deinit();

    var sched = ModuleBuild.init(gpa, root_dir, .multi_threaded, 4, sink.sink());
    defer sched.deinit();

    const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
    defer gpa.free(main_path);

    try sched.buildRoot(main_path);

    // Expect at least one report from each, and A before B
    try std.testing.expect(sink.reports.items.len >= 2);
    try std.testing.expect(std.mem.eql(u8, sink.modules.items[0], "A"));

    var saw_b = false;
    for (sink.modules.items[1..]) |m| {
        if (std.mem.eql(u8, m, "B")) {
            saw_b = true;
            break;
        }
    }
    try std.testing.expect(saw_b);
}

test "ModuleBuild: detect import cycle and fail fast" {
    const gpa = std.testing.allocator;

    var tmp = try std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const root_dir = tmp.dir.path;

    // Main imports A; A imports Main -> cycle
    try write(tmp.dir, "Main.roc", "import A\n" ++ "main = A.x\n");
    try write(tmp.dir, "A.roc", "import Main\n" ++ "x = 1\n");

    var sink = TestSink.init(gpa);
    defer sink.deinit();

    // Use multi-threaded to ensure we detect cycles under concurrency
    var sched = ModuleBuild.init(gpa, root_dir, .multi_threaded, 4, sink.sink());
    defer sched.deinit();

    const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
    defer gpa.free(main_path);

    try sched.buildRoot(main_path);

    // We expect cycle reports from both modules, and emission begins with Main (depth 0) then A (depth 1)
    try std.testing.expect(sink.reports.items.len >= 2);
    try std.testing.expect(std.mem.eql(u8, sink.modules.items[0], "Main"));

    var saw_a = false;
    for (sink.modules.items[1..]) |m| {
        if (std.mem.eql(u8, m, "A")) {
            saw_a = true;
            break;
        }
    }
    try std.testing.expect(saw_a);
}

const TestSink = struct {
    gpa: Allocator,
    reports: std.ArrayList(Report),
    modules: std.ArrayList([]const u8),

    fn init(gpa: Allocator) TestSink {
        return .{ .gpa = gpa, .reports = std.ArrayList(Report).init(gpa), .modules = std.ArrayList([]const u8).init(gpa) };
    }

    fn deinit(self: *TestSink) void {
        // Free reports
        for (self.reports.items) |*r| r.deinit();
        self.reports.deinit();
        // Free duplicated module name strings
        for (self.modules.items) |m| self.gpa.free(m);
        self.modules.deinit();
    }

    fn sink(self: *TestSink) ReportSink {
        return .{ .ctx = self, .emitFn = emitCb };
    }

    fn emitCb(ctx: ?*anyopaque, module_name: []const u8, report: Report) void {
        var self: *TestSink = @ptrCast(@alignCast(ctx.?));
        // Record; take ownership of report
        _ = self.reports.append(report) catch return;
        const owned = self.gpa.dupe(u8, module_name) catch return;
        _ = self.modules.append(owned) catch return;
    }
};

fn write(dir: std.fs.Dir, rel: []const u8, contents: []const u8) !void {
    var f = try dir.createFile(rel, .{ .read = true, .truncate = true, .exclusive = false });
    defer f.close();
    try f.writeAll(contents);
}

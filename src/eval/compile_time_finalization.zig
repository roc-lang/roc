//! Compile-time evaluation finalization.
//!
//! This module is intentionally only the checking finalizer boundary.
//! Compile-time values must be stored through the checked `ConstStore` path
//! produced by the post-check pipeline.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const backend = @import("backend");
const builtins = @import("builtins");
const check = @import("check");
const collections = @import("collections");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const CompilerHost = @import("compiler_host.zig");
const ConstStoreWriter = @import("const_store_writer.zig");
const CompileTimeHost = @import("compile_time_host.zig");
const interpreter_mod = @import("interpreter.zig");
const Interpreter = interpreter_mod.Interpreter;
const ExpectFailure = interpreter_mod.ExpectFailure;
const FinalizeError = checked.CompileTimeFinalizer.Error;
const LirProgram = lir.Program;

/// Runtime options for compile-time finalization.
pub const Options = struct {
    pub const StderrWriter = struct {
        context: ?*anyopaque = null,
        write: *const fn (?*anyopaque, []const u8) void = noop,

        fn noop(_: ?*anyopaque, _: []const u8) void {}

        fn writeAll(self: StderrWriter, bytes: []const u8) void {
            self.write(self.context, bytes);
        }
    };

    max_threads: usize = 0,
    stderr: ?StderrWriter = null,
    std_io: ?std.Io = null,
    slow_root_threshold_ns: u64 = 3 * std.time.ns_per_s,
    slow_root_period_ns: u64 = std.time.ns_per_s,
};

const ComptimeCoverage = struct {
    allocator: Allocator,
    entries: std.ArrayList(Entry),

    const Entry = struct {
        kind: lir.LIR.ComptimeSiteKind,
        region: base.Region,
        branch_regions: []base.Region,
        hits: []bool,
    };

    fn init(allocator: Allocator) ComptimeCoverage {
        return .{
            .allocator = allocator,
            .entries = .empty,
        };
    }

    fn deinit(self: *ComptimeCoverage) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.branch_regions);
            self.allocator.free(entry.hits);
        }
        self.entries.deinit(self.allocator);
    }

    fn record(self: *ComptimeCoverage, site: lir.LIR.ComptimeSite, branch_index: u32) Allocator.Error!void {
        if (site.kind != .match and site.kind != .if_) return;
        if (site.branch_regions.len == 0) return;
        if (branch_index >= site.branch_regions.len) {
            finalizationInvariant("compile-time branch hit referenced a branch outside its site");
        }

        const entry = try self.entryFor(site);
        entry.hits[branch_index] = true;
    }

    fn entryFor(self: *ComptimeCoverage, site: lir.LIR.ComptimeSite) Allocator.Error!*Entry {
        for (self.entries.items) |*entry| {
            if (entry.kind == site.kind and regionsEqual(entry.region, site.region)) {
                if (entry.branch_regions.len != site.branch_regions.len) {
                    finalizationInvariant("compile-time site branch count changed for one source region");
                }
                return entry;
            }
        }

        const branch_regions = try self.allocator.dupe(base.Region, site.branch_regions);
        errdefer self.allocator.free(branch_regions);
        const hits = try self.allocator.alloc(bool, site.branch_regions.len);
        errdefer self.allocator.free(hits);
        @memset(hits, false);

        try self.entries.append(self.allocator, .{
            .kind = site.kind,
            .region = site.region,
            .branch_regions = branch_regions,
            .hits = hits,
        });
        return &self.entries.items[self.entries.items.len - 1];
    }

    fn reportUnusedBranches(self: *const ComptimeCoverage, allocator: Allocator, problem_store: ?*check.problem.Store) Allocator.Error!void {
        const store = problem_store orelse return;
        for (self.entries.items) |entry| {
            for (entry.hits, 0..) |hit, index| {
                if (hit) continue;
                _ = try store.appendProblem(allocator, .{ .comptime_unused_branch = .{
                    .kind = switch (entry.kind) {
                        .match => .match,
                        .if_ => .if_,
                        .destructure => unreachable,
                    },
                    .site_region = entry.region,
                    .branch_region = entry.branch_regions[index],
                } });
            }
        }
    }
};

/// Return the checking finalizer that evaluates compile-time roots.
pub fn finalizer() checked.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

/// Return a checking finalizer configured with caller-provided CTFE options.
pub fn finalizerWithOptions(options: *const Options) checked.CompileTimeFinalizer {
    return .{
        .context = @ptrCast(@constCast(options)),
        .finalize = finalize,
    };
}

fn finalize(
    context: ?*anyopaque,
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    imports: []const checked.PublishImportArtifact,
    available_modules: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    problem_store: ?*check.problem.Store,
) FinalizeError!void {
    const options: Options = if (context) |raw|
        @as(*const Options, @ptrCast(@alignCast(raw))).*
    else
        .{};
    const requests = module.root_requests.compile_time_requests;
    var had_problem = false;

    if (requests.len != 0) {
        var coverage = ComptimeCoverage.init(allocator);
        defer coverage.deinit();

        const lowering_imports = try finalizationImports(allocator, checked.importedView(module), imports, available_modules);
        defer allocator.free(lowering_imports);

        var state = try RootCompletionState.init(allocator, module);
        defer state.deinit();

        var batch_requests = std.ArrayList(checked.RootRequest).empty;
        defer batch_requests.deinit(allocator);

        var batch_root_ids = std.ArrayList(checked.ComptimeRootId).empty;
        defer batch_root_ids.deinit(allocator);

        for (requests, 0..) |request, request_index| {
            if (!state.dependenciesComplete(request)) {
                if (batch_requests.items.len == 0) {
                    finalizationInvariant("compile-time root request order referenced an unfinished dependency");
                }
                if (try lowerEvalAndFinishRoots(
                    allocator,
                    module,
                    lowering_imports,
                    relation_modules,
                    batch_requests.items,
                    batch_root_ids.items,
                    &state,
                    problem_store,
                    &coverage,
                    options,
                )) had_problem = true;
                batch_requests.clearRetainingCapacity();
                batch_root_ids.clearRetainingCapacity();

                if (!state.dependenciesComplete(request)) {
                    finalizationInvariant("compile-time root request order referenced a later or cyclic dependency");
                }
            }
            try batch_requests.append(allocator, request);
            try batch_root_ids.append(allocator, state.rootIdForRequestIndex(request_index));
        }

        if (batch_requests.items.len != 0) {
            if (try lowerEvalAndFinishRoots(
                allocator,
                module,
                lowering_imports,
                relation_modules,
                batch_requests.items,
                batch_root_ids.items,
                &state,
                problem_store,
                &coverage,
                options,
            )) had_problem = true;
        }

        try coverage.reportUnusedBranches(allocator, problem_store);
    }

    if (problem_store) |store| {
        if (try store.flushPendingStaticExhaustiveness(allocator) != 0) {
            return error.CompileTimeProblem;
        }
    }

    try module.const_store.verifyComplete();
    if (had_problem) return error.CompileTimeProblem;
}

const RootStatus = enum {
    pending,
    done,
};

const RootCompletionState = struct {
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    statuses: []RootStatus,
    requested_roots: []bool,
    request_root_ids: []checked.ComptimeRootId,
    visited_templates: []u32,
    visit: u32,
    current_root_id: ?checked.ComptimeRootId = null,

    fn init(
        allocator: Allocator,
        module: *checked.CheckedModuleArtifact,
    ) Allocator.Error!RootCompletionState {
        const statuses = try allocator.alloc(RootStatus, module.compile_time_roots.roots.len);
        errdefer allocator.free(statuses);
        @memset(statuses, .pending);

        const requested_roots = try allocator.alloc(bool, module.compile_time_roots.roots.len);
        errdefer allocator.free(requested_roots);
        @memset(requested_roots, false);

        const request_root_ids = try allocator.alloc(checked.ComptimeRootId, module.root_requests.compile_time_requests.len);
        errdefer allocator.free(request_root_ids);
        for (module.root_requests.compile_time_requests, 0..) |request, i| {
            const root_id = compileTimeRootForRequest(module, request);
            const raw = @intFromEnum(root_id);
            if (requested_roots[raw]) {
                finalizationInvariant("compile-time root was requested more than once");
            }
            requested_roots[raw] = true;
            request_root_ids[i] = root_id;
        }

        const visited_templates = try allocator.alloc(u32, module.checked_procedure_templates.templates.len);
        errdefer allocator.free(visited_templates);
        @memset(visited_templates, 0);

        return .{
            .allocator = allocator,
            .module = module,
            .statuses = statuses,
            .requested_roots = requested_roots,
            .request_root_ids = request_root_ids,
            .visited_templates = visited_templates,
            .visit = 0,
        };
    }

    fn deinit(self: *RootCompletionState) void {
        const allocator = self.allocator;
        allocator.free(self.visited_templates);
        allocator.free(self.request_root_ids);
        allocator.free(self.requested_roots);
        allocator.free(self.statuses);
        self.* = undefined;
    }

    fn isDone(self: *const RootCompletionState, root_id: checked.ComptimeRootId) bool {
        return self.statuses[@intFromEnum(root_id)] == .done;
    }

    fn markDone(self: *RootCompletionState, root_id: checked.ComptimeRootId) void {
        self.statuses[@intFromEnum(root_id)] = .done;
    }

    fn rootIdForRequestIndex(self: *const RootCompletionState, request_index: usize) checked.ComptimeRootId {
        if (request_index >= self.request_root_ids.len) {
            finalizationInvariant("compile-time request index was out of range");
        }
        return self.request_root_ids[request_index];
    }

    fn dependenciesComplete(
        self: *RootCompletionState,
        request: checked.RootRequest,
    ) bool {
        const saved_current_root_id = self.current_root_id;
        defer self.current_root_id = saved_current_root_id;
        self.current_root_id = compileTimeRootForRequest(self.module, request);

        self.visit +%= 1;
        if (self.visit == 0) {
            @memset(self.visited_templates, 0);
            self.visit = 1;
        }
        const template_ref = request.procedure_template orelse
            finalizationInvariant("compile-time root had no checked wrapper template");
        return self.templateDependenciesComplete(template_ref);
    }

    fn templateDependenciesComplete(
        self: *RootCompletionState,
        template_ref: canonical.ProcedureTemplateRef,
    ) bool {
        if (!artifactMatches(template_ref.artifact, self.module.key)) return true;
        const index = @intFromEnum(template_ref.template);
        if (index >= self.visited_templates.len) {
            finalizationInvariant("compile-time dependency referenced an unknown local procedure template");
        }
        if (self.visited_templates[index] == self.visit) return true;
        self.visited_templates[index] = self.visit;

        const template = self.module.checked_procedure_templates.get(template_ref.template);
        return self.resolvedRefsDependenciesComplete(template.resolved_value_refs);
    }

    fn resolvedRefsDependenciesComplete(
        self: *RootCompletionState,
        refs: checked.ResolvedValueRefTableRef,
    ) bool {
        const start = refs.start;
        const end = refs.start + refs.len;
        if (end > self.module.resolved_value_refs.template_refs.len) {
            finalizationInvariant("compile-time dependency template-ref span was outside the checked table");
        }
        for (self.module.resolved_value_refs.template_refs[start..end]) |ref_id| {
            const raw = @intFromEnum(ref_id);
            if (raw >= self.module.resolved_value_refs.records.len) {
                finalizationInvariant("compile-time dependency ref id was outside the checked table");
            }
            if (!self.resolvedRefDependenciesComplete(self.module.resolved_value_refs.records[raw].ref)) {
                return false;
            }
        }
        return true;
    }

    fn resolvedRefDependenciesComplete(
        self: *RootCompletionState,
        ref: checked.ResolvedValueRef,
    ) bool {
        return switch (ref) {
            .top_level_const => |const_use| self.constUseComplete(const_use),
            .selected_hoisted_const => |selected| self.constUseComplete(selected.const_use),
            .top_level_proc,
            .promoted_top_level_proc,
            => |proc_use| self.procedureUseDependenciesComplete(proc_use),
            .platform_required_const => |required| self.constUseComplete(required.const_use),
            .platform_required_proc => |required| self.procedureUseDependenciesComplete(required.procedure),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            .imported_const,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            => true,
        };
    }

    fn constUseComplete(
        self: *RootCompletionState,
        const_use: checked.ConstUseTemplate,
    ) bool {
        const root_id = self.rootForConstRef(const_use.const_ref) orelse return true;
        const own_hoisted_root = switch (const_use.const_ref.owner) {
            .hoisted_expr => true,
            .top_level_binding => false,
        };
        if (self.current_root_id != null and root_id == self.current_root_id.? and own_hoisted_root) return true;
        return !self.requested_roots[@intFromEnum(root_id)] or self.isDone(root_id);
    }

    fn rootForConstRef(
        self: *RootCompletionState,
        const_ref: checked.ConstRef,
    ) ?checked.ComptimeRootId {
        if (!artifactMatches(const_ref.artifact, self.module.key)) return null;
        return switch (const_ref.owner) {
            .top_level_binding => |top_level| {
                return self.module.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse
                    finalizationInvariant("local const dependency had no compile-time root");
            },
            .hoisted_expr => |hoisted| {
                if (hoisted.module_idx != self.module.module_identity.module_idx) {
                    finalizationInvariant("local hoisted const dependency had mismatched module index");
                }
                const entry = self.module.hoisted_constants.lookupByExpr(hoisted.expr) orelse
                    finalizationInvariant("local hoisted const dependency had no hoisted const entry");
                return entry.root;
            },
        };
    }

    fn procedureUseDependenciesComplete(
        self: *RootCompletionState,
        proc_use: checked.ProcedureUseTemplate,
    ) bool {
        return switch (proc_use.binding) {
            .top_level => |top_level| self.topLevelProcedureDependenciesComplete(top_level),
            .imported, .hosted => true,
            .platform_required => |required| self.platformRequiredProcedureDependenciesComplete(required),
        };
    }

    fn topLevelProcedureDependenciesComplete(
        self: *RootCompletionState,
        top_level: checked.ArtifactTopLevelProcedureBindingRef,
    ) bool {
        if (!artifactMatches(top_level.artifact, self.module.key)) return true;
        const binding = self.module.top_level_procedure_bindings.get(top_level.binding);
        return self.procedureBindingDependenciesComplete(binding.body);
    }

    fn procedureBindingDependenciesComplete(
        self: *RootCompletionState,
        body: checked.ProcedureBindingBody,
    ) bool {
        return switch (body) {
            .direct_template => |direct| self.callableTemplateDependenciesComplete(direct.template),
            .callable_eval_template => |template_id| blk: {
                const template = self.module.callable_eval_templates.get(template_id);
                break :blk !self.requested_roots[@intFromEnum(template.root)] or self.isDone(template.root);
            },
        };
    }

    fn callableTemplateDependenciesComplete(
        self: *RootCompletionState,
        template: canonical.CallableProcedureTemplateRef,
    ) bool {
        return switch (template) {
            .checked => |checked_template| self.templateDependenciesComplete(checked_template),
            .lifted, .synthetic => finalizationInvariant("checked procedure dependency referenced a post-check template"),
        };
    }

    fn platformRequiredProcedureDependenciesComplete(
        self: *RootCompletionState,
        required: checked.RequiredAppProcedureRef,
    ) bool {
        if (!artifactMatches(required.artifact, self.module.key)) return true;
        const binding = self.module.platform_required_bindings.lookupByBindingId(@intFromEnum(required.procedure_binding)) orelse
            finalizationInvariant("platform-required procedure dependency referenced a missing binding");
        return switch (binding.value_use) {
            .procedure_value => |procedure_use| self.procedureUseDependenciesComplete(procedure_use.procedure),
            .const_value => |const_use| self.constUseComplete(const_use.const_use),
        };
    }
};

fn lowerEvalAndFinishRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    options: Options,
) FinalizeError!bool {
    if (requests.len != root_ids.len) {
        finalizationInvariant("compile-time finalization request/root-id batch length mismatch");
    }

    if (comptime !compilerHostMustUseInterpreterForCtfe()) {
        if (comptime !backend.host_lir_codegen_available) return error.UnsupportedPlatform;
        return lowerDevEvalAndFinishRoots(
            allocator,
            module,
            lowering_imports,
            relation_modules,
            requests,
            root_ids,
            state,
            problem_store,
            coverage,
            options,
        );
    }

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = checked.loweringViewWithRelations(module, relation_modules),
            .imports = lowering_imports,
        },
        .{ .requests = requests },
        .{
            .target_usize = base.target.TargetUsize.native,
            .checked_module_state = .checking_finalization,
        },
    );
    defer lowered.deinit();

    var host = CompilerHost.init(allocator);
    defer host.deinit();

    var interpreter = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        host.ops(),
    );
    defer interpreter.deinit();

    var writer = ConstStoreWriter.Writer.init(allocator, module, &lowered.lir_result);
    defer writer.deinit();

    var had_problem = false;
    if (lowered.lir_result.const_roots.items.len != requests.len) {
        finalizationInvariant("LIR lowering returned a different number of compile-time roots than requested");
    }
    for (lowered.lir_result.const_roots.items, 0..) |root, i| {
        if (!std.meta.eql(root.request, requests[i])) {
            finalizationInvariant("LIR lowering changed compile-time root request order");
        }
        const root_id = root_ids[i];
        const compile_time_root = module.compile_time_roots.root(root_id);
        var payload: checked.CompileTimeRootPayload = blk: {
            if (root.request.kind == .compile_time_constant and problem_store == null) {
                const eval_result = interpreter.eval(.{
                    .proc_id = root.proc,
                    .ret_layout = root.ret_layout,
                }) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.RuntimeError, error.DivisionByZero => {
                        const message = interpreter.getRuntimeErrorMessage() orelse host.crash_message orelse "compile-time evaluation failed";
                        break :blk .{ .const_node = try appendCrashConst(module, message) };
                    },
                    error.ComptimeExhaustiveness => {
                        break :blk .{ .const_node = try appendCrashConst(module, "compile-time exhaustiveness failure") };
                    },
                    error.Crash => {
                        const message = interpreter.getCrashMessage() orelse host.crash_message orelse "Roc crashed";
                        break :blk .{ .const_node = try appendCrashConst(module, message) };
                    },
                    // expect_err statements only occur in top-level expect
                    // test roots, never in compile-time constant roots.
                    error.ExpectErr => unreachable,
                };
                defer interpreter.dropValue(eval_result.value, root.ret_layout);
                break :blk try writer.storeRoot(root, eval_result.value);
            }

            const eval_result = try evalCompileTimeRoot(allocator, &interpreter, problem_store, module, compile_time_root, &lowered.lir_result, root.proc, root.ret_layout);
            try recordComptimeSiteHits(problem_store, coverage, module, compile_time_root, &lowered.lir_result, interpreter.getComptimeBranchHits(), root.proc);
            defer interpreter.dropValue(eval_result.value, root.ret_layout);
            break :blk try writer.storeRoot(root, eval_result.value);
        };

        if (try reportCompileTimeExpectFailures(
            allocator,
            problem_store,
            module,
            compile_time_root,
            interpreter.getExpectFailures(),
        )) had_problem = true;

        switch (compile_time_root.kind) {
            .numeral_conversion, .quote_conversion => {
                payload = try finishLiteralConversionRoot(allocator, module, problem_store, compile_time_root, payload);
            },
            else => {},
        }

        module.compile_time_roots.fillPayload(root_id, payload);
        finishConstRoot(module, compile_time_root, payload);
        state.markDone(root_id);
    }

    return had_problem;
}

fn compilerHostMustUseInterpreterForCtfe() bool {
    return builtin.target.os.tag == .freestanding or
        builtin.target.cpu.arch == .wasm32;
}

const DevRootProgressState = enum(u8) {
    pending,
    running,
    done,
};

const ProgressMillis = usize;

const DevRootResult = enum {
    pending,
    success,
    crashed,
    comptime_exhaustiveness,
    host_oom,
};

const ThreadSafeAllocator = struct {
    child: Allocator,
    mutex: std.atomic.Mutex = .unlocked,

    fn init(child: Allocator) ThreadSafeAllocator {
        return .{ .child = child };
    }

    fn allocator(self: *ThreadSafeAllocator) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn lock(self: *ThreadSafeAllocator) void {
        while (!self.mutex.tryLock()) {
            std.atomic.spinLoopHint();
        }
    }

    fn unlock(self: *ThreadSafeAllocator) void {
        self.mutex.unlock();
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        return self.child.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        return self.child.rawResize(memory, alignment, new_len, ret_addr);
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        return self.child.rawRemap(memory, alignment, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *ThreadSafeAllocator = @ptrCast(@alignCast(ctx));
        self.lock();
        defer self.unlock();
        self.child.rawFree(memory, alignment, ret_addr);
    }
};

const DevRootLabel = struct {
    module_name: []const u8,
    snippet: []u8,
    line: u32,
    column: u32,

    fn deinit(self: *DevRootLabel, allocator: Allocator) void {
        allocator.free(self.snippet);
    }
};

const DevRootJob = struct {
    root: LirProgram.ConstRootPlan,
    root_id: checked.ComptimeRootId,
    compile_time_root: checked.CompileTimeRoot,
    entry_offset: usize,
    ret_buf: []align(collections.max_roc_alignment.toByteUnits()) u8,
    host: CompileTimeHost,
    result: DevRootResult = .pending,
    progress: std.atomic.Value(u8) = std.atomic.Value(u8).init(@intFromEnum(DevRootProgressState.pending)),
    start_ms: std.atomic.Value(ProgressMillis) = std.atomic.Value(ProgressMillis).init(0),
    last_progress_ms: std.atomic.Value(ProgressMillis) = std.atomic.Value(ProgressMillis).init(0),
    label: DevRootLabel,

    fn deinit(self: *DevRootJob, allocator: Allocator) void {
        self.host.deinit();
        allocator.free(self.ret_buf);
        self.label.deinit(allocator);
    }
};

const DevRunContext = struct {
    executable: *const backend.ExecutableMemory,
    jobs: []DevRootJob,
    std_io: ?std.Io,
    progress_reporter: ?*DevProgressReporter,
    had_oom: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
};

const DevProgressThread = if (base.parallel.is_freestanding) struct {} else std.Thread;

const DevProgressReporter = struct {
    options: Options,
    jobs: []DevRootJob,
    stop: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    wake_epoch: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),
    thread: ?DevProgressThread = null,

    fn init(options: Options, jobs: []DevRootJob) DevProgressReporter {
        return .{
            .options = options,
            .jobs = jobs,
        };
    }

    fn deinit(self: *DevProgressReporter) void {
        self.finish();
        self.* = undefined;
    }

    fn start(self: *DevProgressReporter) std.Thread.SpawnError!void {
        if (comptime base.parallel.is_freestanding) return;
        if (self.options.stderr == null) return;
        if (self.options.std_io == null) return;
        if (self.jobs.len == 0) return;
        if (self.options.slow_root_threshold_ns == 0) return;
        if (self.options.slow_root_period_ns == 0) return;
        self.thread = try std.Thread.spawn(.{}, progressThreadMain, .{self});
    }

    fn finish(self: *DevProgressReporter) void {
        if (comptime base.parallel.is_freestanding) return;
        const thread = self.thread orelse return;
        self.stop.store(true, .release);
        self.wake();
        thread.join();
        self.thread = null;
    }

    fn rootStarted(self: *DevProgressReporter) void {
        if (comptime base.parallel.is_freestanding) return;
        if (self.thread == null) return;
        self.wake();
    }

    fn wake(self: *DevProgressReporter) void {
        const io = self.options.std_io orelse return;
        _ = self.wake_epoch.fetchAdd(1, .acq_rel);
        std.Io.futexWake(io, u32, &self.wake_epoch.raw, 1);
    }

    fn progressThreadMain(self: *DevProgressReporter) void {
        const io = self.options.std_io orelse return;
        while (!self.stop.load(.acquire)) {
            const epoch = self.wake_epoch.load(.acquire);
            const now = nowMs(io);
            self.reportDue(now);
            const next_wait_ms = self.nextWaitMs(now);
            if (self.stop.load(.acquire)) break;
            if (next_wait_ms) |wait_ms| {
                if (wait_ms == 0) continue;
                self.waitForWake(io, epoch, .{ .duration = .{
                    .raw = std.Io.Duration.fromMilliseconds(std.math.cast(i64, wait_ms) orelse std.math.maxInt(i64)),
                    .clock = .awake,
                } });
            } else {
                self.waitForWake(io, epoch, .none);
            }
        }
    }

    fn waitForWake(self: *DevProgressReporter, io: std.Io, epoch: u32, timeout: std.Io.Timeout) void {
        std.Io.futexWaitTimeout(io, u32, &self.wake_epoch.raw, epoch, timeout) catch {};
    }

    fn reportDue(self: *DevProgressReporter, now: ProgressMillis) void {
        const stderr = self.options.stderr orelse return;
        const threshold = progressDurationMs(self.options.slow_root_threshold_ns);
        const period = progressDurationMs(self.options.slow_root_period_ns);
        const spinner = spinnerByte(now / 250);

        for (self.jobs) |*job| {
            const progress: DevRootProgressState = @enumFromInt(job.progress.load(.acquire));
            if (progress != .running) continue;
            const started_at = job.start_ms.load(.acquire);
            if (started_at == 0 or now -% started_at < threshold) continue;
            const last = job.last_progress_ms.load(.acquire);
            if (last != 0 and now -% last < period) continue;
            if (job.last_progress_ms.cmpxchgStrong(last, now, .acq_rel, .acquire) != null) continue;
            const elapsed_s = (now -% started_at) / std.time.ms_per_s;
            var line_buf: [4096]u8 = undefined;
            const line = progressLine(&line_buf, spinner, job.label, @intCast(elapsed_s)) catch return;
            stderr.writeAll(line);
        }
    }

    fn nextWaitMs(self: *DevProgressReporter, now: ProgressMillis) ?ProgressMillis {
        const threshold = progressDurationMs(self.options.slow_root_threshold_ns);
        const period = progressDurationMs(self.options.slow_root_period_ns);
        var next: ?ProgressMillis = null;

        for (self.jobs) |*job| {
            const progress: DevRootProgressState = @enumFromInt(job.progress.load(.acquire));
            if (progress != .running) continue;
            const started_at = job.start_ms.load(.acquire);
            if (started_at == 0) continue;
            const wait = blk: {
                const elapsed = now -% started_at;
                if (elapsed < threshold) break :blk threshold - elapsed;
                const last = job.last_progress_ms.load(.acquire);
                if (last == 0) break :blk 0;
                const since_last = now -% last;
                break :blk if (since_last < period) period - since_last else 0;
            };
            if (next == null or wait < next.?) next = wait;
        }

        return next;
    }

    fn progressLine(
        buf: []u8,
        spinner: u8,
        label: DevRootLabel,
        elapsed_s: u64,
    ) std.fmt.BufPrintError![]u8 {
        return std.fmt.bufPrint(
            buf,
            "{c} Evaluating `{s}` at compile time in {s}:{d}:{d} ({d}s)\n",
            .{
                spinner,
                label.snippet,
                label.module_name,
                label.line,
                label.column,
                elapsed_s,
            },
        );
    }
};

fn lowerDevEvalAndFinishRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    root_ids: []const checked.ComptimeRootId,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    options: Options,
) FinalizeError!bool {
    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = checked.loweringViewWithRelations(module, relation_modules),
            .imports = lowering_imports,
        },
        .{ .requests = requests },
        .{
            .target_usize = base.target.TargetUsize.native,
            .checked_module_state = .checking_finalization,
        },
    );
    defer lowered.deinit();

    if (lowered.lir_result.const_roots.items.len != requests.len) {
        finalizationInvariant("LIR lowering returned a different number of compile-time roots than requested");
    }
    var codegen = try backend.HostLirCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &.{},
    );
    defer codegen.deinit();
    codegen.setComptimeHooks(.{
        .branch_taken = CompileTimeHost.rocComptimeBranchTaken,
        .exhaustiveness_failed = CompileTimeHost.rocComptimeExhaustivenessFailed,
        .failure_region = CompileTimeHost.rocComptimeFailureRegion,
        .call_enter = CompileTimeHost.rocComptimeCallEnter,
        .call_exit = CompileTimeHost.rocComptimeCallExit,
    });
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

    var host_allocator_impl = ThreadSafeAllocator.init(allocator);
    const host_allocator = host_allocator_impl.allocator();

    var jobs = try allocator.alloc(DevRootJob, lowered.lir_result.const_roots.items.len);
    var jobs_len: usize = 0;
    {
        errdefer {
            for (jobs[0..jobs_len]) |*job| job.deinit(allocator);
            allocator.free(jobs);
        }

        for (lowered.lir_result.const_roots.items, 0..) |root, i| {
            if (!std.meta.eql(root.request, requests[i])) {
                finalizationInvariant("LIR lowering changed compile-time root request order");
            }

            var name_buf: [64]u8 = undefined;
            const symbol_name = std.fmt.bufPrint(&name_buf, "roc_ctfe_root_{d}", .{i}) catch unreachable;
            const entrypoint = try codegen.generateEntrypointWrapper(
                symbol_name,
                root.proc,
                &.{},
                root.ret_layout,
            );

            const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(root.ret_layout));
            const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
            errdefer allocator.free(ret_buf);
            @memset(ret_buf, 0);

            const root_id = root_ids[i];
            const compile_time_root = module.compile_time_roots.root(root_id);
            const label = try devRootLabel(allocator, module, compile_time_root);
            errdefer {
                var owned = label;
                owned.deinit(allocator);
            }

            jobs[i] = .{
                .root = root,
                .root_id = root_id,
                .compile_time_root = compile_time_root,
                .entry_offset = entrypoint.offset,
                .ret_buf = ret_buf,
                .host = CompileTimeHost.init(host_allocator),
                .label = label,
            };
            jobs_len += 1;
        }
    }
    defer {
        for (jobs[0..jobs_len]) |*job| job.deinit(allocator);
        allocator.free(jobs);
    }

    var executable = try backend.ExecutableMemory.initWithEntryOffset(codegen.getGeneratedCode(), 0);
    defer executable.deinit();

    var progress = DevProgressReporter.init(options, jobs[0..jobs_len]);
    defer progress.deinit();
    try progress.start();
    var run_context = DevRunContext{
        .executable = &executable,
        .jobs = jobs[0..jobs_len],
        .std_io = options.std_io,
        .progress_reporter = if (progress.thread == null) null else &progress,
    };
    const max_threads = if (options.max_threads == 0)
        0
    else
        @max(options.max_threads, 1);
    try base.parallel.process(
        DevRunContext,
        &run_context,
        devRootWorker,
        host_allocator,
        jobs_len,
        .{
            .max_threads = max_threads,
            .use_per_thread_arenas = false,
        },
    );
    progress.finish();

    if (run_context.had_oom.load(.acquire)) return error.OutOfMemory;

    const DevResolver = struct {
        codegen: *const backend.HostLirCodeGen,
        store: *const lir.LirStore,
        executable: *const backend.ExecutableMemory,

        fn resolve(raw: ?*anyopaque, data_ptr: [*]u8) ConstStoreWriter.ErasedCallableResolution {
            const self: *@This() = @ptrCast(@alignCast(raw.?));
            const payload = builtins.erased_callable.payloadPtr(data_ptr);
            const runtime_addr = @intFromPtr(payload.callable_fn_ptr);
            for (self.store.getProcSpecs(), 0..) |_, proc_index| {
                const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(proc_index);
                const symbol = self.codegen.compiledProcSymbol(proc_id) orelse continue;
                const compiled_addr = @intFromPtr(self.executable.codePtr() + symbol.code_start);
                if (compiled_addr == runtime_addr) {
                    return .{
                        .proc = proc_id,
                        .capture_ptr = builtins.erased_callable.capturePtr(data_ptr),
                    };
                }
            }
            finalizationInvariant("dev backend erased callable result did not map to a compiled proc");
        }
    };

    var resolver = DevResolver{
        .codegen = &codegen,
        .store = &lowered.lir_result.store,
        .executable = &executable,
    };

    var writer = ConstStoreWriter.Writer.init(allocator, module, &lowered.lir_result);
    defer writer.deinit();
    writer.setErasedCallableResolver(.{
        .context = @ptrCast(&resolver),
        .resolve = DevResolver.resolve,
    });

    var had_problem = false;
    for (jobs[0..jobs_len]) |*job| {
        var payload: checked.CompileTimeRootPayload = switch (job.result) {
            .pending => finalizationInvariant("dev backend compile-time root was not evaluated"),
            .host_oom => return error.OutOfMemory,
            .comptime_exhaustiveness => try devComptimeExhaustivenessRootPayload(
                allocator,
                problem_store,
                module,
                job.compile_time_root,
                &lowered.lir_result,
                job.root.proc,
                job.host.comptime_failed_site orelse finalizationInvariant("dev backend CTFE exhaustiveness failure had no site"),
                job.root.request,
                &had_problem,
            ),
            .crashed => try devCrashedRootPayload(
                allocator,
                problem_store,
                module,
                job.compile_time_root,
                job.root.request,
                job.host.crashMessage() orelse "Roc crashed",
                job.host.failed_region,
                &had_problem,
            ),
            .success => try writer.storeRoot(job.root, .{ .ptr = job.ret_buf.ptr }),
        };

        try recordComptimeSiteHits(problem_store, coverage, module, job.compile_time_root, &lowered.lir_result, job.host.comptime_branch_hits.items, job.root.proc);

        if (try reportDevHostEvents(allocator, options.stderr, problem_store, module, job.compile_time_root, job.host.events.items)) {
            had_problem = true;
        }

        switch (job.compile_time_root.kind) {
            .numeral_conversion, .quote_conversion => {
                const conversion = try finishLiteralConversionRootDetailed(allocator, module, problem_store, job.compile_time_root, payload);
                payload = conversion.payload;
                if (conversion.had_problem) had_problem = true;
            },
            else => {},
        }

        module.compile_time_roots.fillPayload(job.root_id, payload);
        finishConstRoot(module, job.compile_time_root, payload);
        state.markDone(job.root_id);
    }

    return had_problem;
}

fn devRootWorker(_: Allocator, context: *DevRunContext, item_id: usize) void {
    const job = &context.jobs[item_id];
    job.host.resetForRun();
    job.start_ms.store(if (context.std_io) |io| nowMs(io) else 0, .release);
    job.last_progress_ms.store(0, .release);
    job.progress.store(@intFromEnum(DevRootProgressState.running), .release);
    if (context.progress_reporter) |progress| progress.rootStarted();

    var crash_boundary = job.host.enterCrashBoundary();
    const sj = crash_boundary.set();
    if (sj == 0) {
        context.executable.callRocABIAt(
            job.entry_offset,
            @ptrCast(job.host.ops()),
            @ptrCast(job.ret_buf.ptr),
            null,
        );
    }
    crash_boundary.deinit();

    job.result = switch (job.host.termination) {
        .returned => .success,
        .crashed => .crashed,
        .comptime_exhaustiveness => .comptime_exhaustiveness,
        .host_oom => blk: {
            context.had_oom.store(true, .release);
            break :blk .host_oom;
        },
    };
    job.progress.store(@intFromEnum(DevRootProgressState.done), .release);
}

fn nowNs(io: std.Io) i64 {
    return @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds));
}

fn nowMs(io: std.Io) ProgressMillis {
    const ns: u64 = @intCast(nowNs(io));
    return @truncate(ns / std.time.ns_per_ms);
}

fn progressDurationMs(ns: u64) ProgressMillis {
    const ms = ns / std.time.ns_per_ms;
    return @intCast(@min(ms, std.math.maxInt(ProgressMillis)));
}

fn spinnerByte(tick: usize) u8 {
    const frames = "|/-\\";
    return frames[tick % frames.len];
}

fn devRootLabel(
    allocator: Allocator,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
) Allocator.Error!DevRootLabel {
    const env = module.moduleEnvConst();
    const region = devRootSourceRegion(module, root);
    const info = env.calcRegionInfo(region);
    const source = env.getSourceAll();
    const line_starts = env.getLineStartsAll();
    const raw_line = base.RegionInfo.getLineText(source, line_starts, info.start_line_idx, info.start_line_idx);
    const snippet = try truncateSnippet(allocator, raw_line, 96);
    return .{
        .module_name = env.module_name,
        .snippet = snippet,
        .line = info.start_line_idx + 1,
        .column = info.start_col_idx + 1,
    };
}

fn devRootSourceRegion(
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
) base.Region {
    const env = module.moduleEnvConst();
    return switch (root.source) {
        .def => |def_idx| blk: {
            const def = env.store.getDef(def_idx);
            break :blk env.store.getPatternRegion(def.pattern);
        },
        .expr => |expr_idx| env.store.getExprRegion(expr_idx),
        .statement => |stmt_idx| env.store.getStatementRegion(stmt_idx),
        .hoisted => |hoisted| env.store.getExprRegion(hoisted.expr),
        .required_binding => module.checked_bodies.expr(root.expr).source_region,
    };
}

fn truncateSnippet(allocator: Allocator, bytes: []const u8, max_bytes: usize) Allocator.Error![]u8 {
    const ellipsis = "…";
    if (bytes.len <= max_bytes) return try allocator.dupe(u8, bytes);
    if (max_bytes <= ellipsis.len) return try allocator.dupe(u8, ellipsis);
    const prefix_len = validUtf8PrefixLen(bytes, max_bytes - ellipsis.len);
    return try std.fmt.allocPrint(allocator, "{s}{s}", .{ bytes[0..prefix_len], ellipsis });
}

fn validUtf8PrefixLen(bytes: []const u8, max_bytes: usize) usize {
    var end = @min(bytes.len, max_bytes);
    while (end > 0 and !std.unicode.utf8ValidateSlice(bytes[0..end])) {
        end -= 1;
    }
    return end;
}

fn devComptimeExhaustivenessRootPayload(
    allocator: Allocator,
    problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    site_id: lir.LIR.ComptimeSiteId,
    request: checked.RootRequest,
    had_problem: *bool,
) FinalizeError!checked.CompileTimeRootPayload {
    if (request.kind == .compile_time_constant and problem_store == null) {
        return .{ .const_node = try appendCrashConst(module, "compile-time exhaustiveness failure") };
    }

    const store = problem_store orelse {
        finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
    };
    try appendCompileTimeExhaustivenessProblem(allocator, store, module, root, lir_result, root_proc, site_id);
    had_problem.* = true;
    return .{ .const_node = try appendCrashConst(module, "compile-time exhaustiveness failure") };
}

fn devCrashedRootPayload(
    allocator: Allocator,
    problem_store: ?*check.problem.Store,
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    request: checked.RootRequest,
    message: []const u8,
    failed_region: ?base.Region,
    had_problem: *bool,
) FinalizeError!checked.CompileTimeRootPayload {
    if (request.kind == .compile_time_constant and problem_store == null) {
        return .{ .const_node = try appendCrashConst(module, message) };
    }
    const store = problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try store.putExtraString(message);
    const region = failed_region orelse devRootSourceRegion(module, root);
    _ = try store.appendProblem(allocator, .{ .comptime_crash = .{
        .message = message_idx,
        .region = region,
    } });
    had_problem.* = true;
    return .{ .const_node = try appendCrashConst(module, message) };
}

fn reportDevHostEvents(
    allocator: Allocator,
    stderr: ?Options.StderrWriter,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    events: []const CompileTimeHost.HostEvent,
) FinalizeError!bool {
    var had_problem = false;
    const region = module.checked_bodies.expr(root.expr).source_region;
    for (events) |event| {
        switch (event) {
            .dbg => |msg| if (stderr) |writer| {
                const line = try std.fmt.allocPrint(allocator, "[dbg] {s}\n", .{msg});
                defer allocator.free(line);
                writer.writeAll(line);
            },
            .expect_failed => |msg| if (maybe_problem_store) |store| {
                const message_idx = try store.putExtraString(msg);
                _ = try store.appendProblem(allocator, .{ .comptime_expect_failed = .{
                    .message = message_idx,
                    .region = region,
                } });
                had_problem = true;
            },
            .crashed => {},
        }
    }
    return had_problem;
}

/// Unwrap the `Try` value a literal-conversion root evaluated to. `Ok` payloads
/// become the stored constant; `Err(InvalidNumeral(msg))` / `Err(BadQuotedBytes(msg))`
/// becomes a checking problem carrying the implementation's message.
const LiteralConversionFinish = struct {
    payload: checked.CompileTimeRootPayload,
    had_problem: bool,
};

fn finishLiteralConversionRoot(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) FinalizeError!checked.CompileTimeRootPayload {
    const result = try finishLiteralConversionRootDetailed(allocator, module, problem_store, root, payload);
    if (result.had_problem) return error.CompileTimeProblem;
    return result.payload;
}

fn finishLiteralConversionRootDetailed(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) FinalizeError!LiteralConversionFinish {
    const try_node = switch (payload) {
        .const_node => |node| node,
        else => finalizationInvariant("numeral conversion root did not store a constant"),
    };
    switch (module.const_store.get(try_node)) {
        // The from_numeral implementation itself crashed; that crash was
        // already stored (and reported when a problem store exists).
        .crash => return .{ .payload = payload, .had_problem = false },
        else => {},
    }
    const try_tag = constTagValue(module, try_node);
    if (constTagNameIs(try_tag.tag_name, "Ok")) {
        if (try_tag.payloads.len != 1) finalizationInvariant("numeral conversion Ok did not carry one payload");
        return .{ .payload = .{ .const_node = try_tag.payloads[0] }, .had_problem = false };
    }
    if (!constTagNameIs(try_tag.tag_name, "Err")) {
        finalizationInvariant("numeral conversion result was neither Ok nor Err");
    }
    if (try_tag.payloads.len != 1) finalizationInvariant("numeral conversion Err did not carry one payload");
    const err_tag = constTagValue(module, try_tag.payloads[0]);
    if (err_tag.payloads.len != 1) finalizationInvariant("numeral conversion error tag did not carry one payload");
    const message_str = switch (module.const_store.get(err_tag.payloads[0])) {
        .str => |str| str,
        else => finalizationInvariant("numeral conversion error payload was not a string"),
    };
    const message = module.const_store.strBytes(message_str);
    if (problem_store) |store| {
        const message_idx = try store.putExtraString(message);
        const region = module.checked_bodies.expr(root.expr).source_region;
        switch (root.kind) {
            .numeral_conversion => _ = try store.appendProblem(allocator, .{ .comptime_invalid_numeral = .{
                .message = message_idx,
                .region = region,
            } }),
            .quote_conversion => _ = try store.appendProblem(allocator, .{ .comptime_invalid_quote = .{
                .message = message_idx,
                .region = region,
            } }),
            else => finalizationInvariant("non literal-conversion root reported a conversion problem"),
        }
        return .{
            .payload = .{ .const_node = try appendCrashConst(module, message) },
            .had_problem = true,
        };
    }
    return .{ .payload = .{ .const_node = try appendCrashConst(module, message) }, .had_problem = false };
}

fn constTagValue(
    module: *const checked.CheckedModuleArtifact,
    node: checked.ConstNodeId,
) @FieldType(checked.ConstValue, "tag") {
    var current = node;
    while (true) {
        switch (module.const_store.get(current)) {
            .nominal => |nominal| current = nominal.backing,
            .tag => |tag| return tag,
            else => finalizationInvariant("numeral conversion constant was not a tag value"),
        }
    }
}

fn constTagNameIs(name: []const u8, expected: []const u8) bool {
    if (name.len != expected.len) return false;
    for (name, expected) |actual, wanted| {
        if (actual != wanted) return false;
    }
    return true;
}

fn appendCrashConst(
    module: *checked.CheckedModuleArtifact,
    message: []const u8,
) Allocator.Error!checked.ConstNodeId {
    const data = try module.const_store.addStrData(message);
    return try module.const_store.append(.{ .crash = .{
        .data = data,
        .offset = 0,
        .len = @intCast(message.len),
    } });
}

fn reportCompileTimeExpectFailures(
    allocator: Allocator,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    failures: []const ExpectFailure,
) FinalizeError!bool {
    if (failures.len == 0) return false;
    const problem_store = maybe_problem_store orelse return false;
    const region = module.checked_bodies.expr(root.expr).source_region;
    for (failures) |failure| {
        const message_idx = try problem_store.putExtraString(failure.message);
        _ = try problem_store.appendProblem(allocator, .{ .comptime_expect_failed = .{
            .message = message_idx,
            .region = region,
        } });
    }
    return true;
}

fn evalCompileTimeRoot(
    allocator: Allocator,
    interpreter: *Interpreter,
    problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    proc: lir.LIR.LirProcSpecId,
    ret_layout: @import("layout").Idx,
) FinalizeError!Interpreter.EvalResult {
    return interpreter.eval(.{
        .proc_id = proc,
        .ret_layout = ret_layout,
    }) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.RuntimeError => try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter, interpreter.getRuntimeErrorMessage() orelse "compile-time evaluation failed"),
        error.ComptimeExhaustiveness => try reportCompileTimeExhaustiveness(allocator, problem_store, module, root, lir_result, interpreter, proc),
        error.DivisionByZero => try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter, interpreter.getRuntimeErrorMessage() orelse "Division by zero"),
        error.Crash => try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter, interpreter.getCrashMessage() orelse "Roc crashed"),
        error.ExpectErr => finalizationInvariant("compile-time root reached an expect_err statement"),
    };
}

fn recordComptimeSiteHits(
    maybe_problem_store: ?*check.problem.Store,
    coverage: *ComptimeCoverage,
    module: *const checked.CheckedModuleArtifact,
    compile_time_root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    hits: anytype,
    root_proc: lir.LIR.LirProcSpecId,
) Allocator.Error!void {
    const problem_store = maybe_problem_store orelse return;
    for (hits) |hit| {
        const site = lir_result.comptime_sites.items[@intFromEnum(hit.site)];
        if (comptimeSiteEmpiricalKind(site.kind) != null) {
            if (site.checked_site) |checked_site| {
                if (comptimeSiteMayResolvePending(module, compile_time_root.id, checked_site)) {
                    problem_store.resolvePendingStaticExhaustiveness(checked_site);
                }
            }
        }
        if (reportsUnusedBranches(compile_time_root.kind) and site.proc == root_proc) {
            try coverage.record(site, hit.branch_index);
        }
    }
}

fn reportsUnusedBranches(kind: checked.CompileTimeRootKind) bool {
    return switch (kind) {
        .constant,
        .callable_binding,
        .expect,
        .numeral_conversion,
        .quote_conversion,
        => true,
        .hoisted_constant => false,
    };
}

fn reportCompileTimeExhaustiveness(
    allocator: Allocator,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    interpreter: *const Interpreter,
    root_proc: lir.LIR.LirProcSpecId,
) FinalizeError!Interpreter.EvalResult {
    const problem_store = maybe_problem_store orelse {
        finalizationInvariant("compile-time root reached an empirical exhaustiveness failure without a checking problem store");
    };
    const site_id = interpreter.getComptimeFailedSite() orelse {
        finalizationInvariant("compile-time root reported empirical exhaustiveness failure without a site");
    };
    try appendCompileTimeExhaustivenessProblem(allocator, problem_store, module, root, lir_result, root_proc, site_id);
    return error.CompileTimeProblem;
}

fn appendCompileTimeExhaustivenessProblem(
    allocator: Allocator,
    problem_store: *check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    site_id: lir.LIR.ComptimeSiteId,
) Allocator.Error!void {
    const site = lir_result.comptime_sites.items[@intFromEnum(site_id)];
    _ = comptimeSiteEmpiricalKind(site.kind) orelse switch (site.kind) {
        .if_ => finalizationInvariant("if expression reached empirical exhaustiveness failure"),
        .match, .destructure => finalizationInvariant("compile-time root had no empirical exhaustiveness kind"),
    };
    const checked_site = site.checked_site orelse {
        finalizationInvariant("empirical exhaustiveness failure had no checked site id");
    };
    discardUnreachedRootComptimeSites(problem_store, lir_result, root_proc, checked_site, module, site_id);
    if (!comptimeSiteMayResolvePending(module, root.id, checked_site)) {
        const site_record = module.exhaustiveness_sites.get(checked_site);
        switch (site_record.policy) {
            .runtime_reachable => {},
            .not_pending,
            .compile_time_only,
            .compile_time_replaced_by_root,
            => finalizationInvariant("compile-time exhaustiveness failure had an impossible site policy"),
        }
    }
    const matched = try problem_store.appendEmpiricalExhaustivenessFailure(allocator, checked_site);
    if (!matched) {
        finalizationInvariant("empirical exhaustiveness failure had no pending static diagnostic");
    }
}

fn discardUnreachedRootComptimeSites(
    problem_store: *check.problem.Store,
    lir_result: *const lir.Program.Result,
    root_proc: lir.LIR.LirProcSpecId,
    failed_checked_site: checked.CheckedExhaustivenessSiteId,
    module: *const checked.CheckedModuleArtifact,
    failed_site_id: lir.LIR.ComptimeSiteId,
) void {
    for (lir_result.comptime_sites.items, 0..) |root_site, raw_site_id| {
        if (root_site.proc != root_proc) continue;
        if (raw_site_id == @intFromEnum(failed_site_id)) continue;
        if (comptimeSiteEmpiricalKind(root_site.kind) == null) continue;
        const checked_site = root_site.checked_site orelse continue;
        if (checked_site == failed_checked_site) continue;
        const site = module.exhaustiveness_sites.get(checked_site);
        switch (site.policy) {
            .compile_time_replaced_by_root,
            .compile_time_only,
            => problem_store.discardPendingStaticExhaustiveness(checked_site),
            .runtime_reachable,
            .not_pending,
            => {},
        }
    }
}

fn comptimeSiteMayResolvePending(
    module: *const checked.CheckedModuleArtifact,
    root_id: checked.ComptimeRootId,
    checked_site: checked.CheckedExhaustivenessSiteId,
) bool {
    const site = module.exhaustiveness_sites.get(checked_site);
    return switch (site.policy) {
        .compile_time_replaced_by_root => |owner_root| owner_root == root_id,
        .compile_time_only => true,
        .runtime_reachable,
        .not_pending,
        => false,
    };
}

fn comptimeSiteEmpiricalKind(
    site_kind: lir.LIR.ComptimeSiteKind,
) ?check.problem.Store.EmpiricalSiteKind {
    return switch (site_kind) {
        .match => .match,
        .destructure => .destructure,
        .if_ => null,
    };
}

fn reportCompileTimeCrash(
    allocator: Allocator,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    interpreter: *const Interpreter,
    message: []const u8,
) FinalizeError!Interpreter.EvalResult {
    const problem_store = maybe_problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try problem_store.putExtraString(message);
    const region = compileTimeCrashRegion(module, root, interpreter);
    _ = try problem_store.appendProblem(allocator, .{ .comptime_crash = .{
        .message = message_idx,
        .region = region,
    } });
    return error.CompileTimeProblem;
}

fn compileTimeCrashRegion(
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    interpreter: *const Interpreter,
) base.Region {
    if (interpreter.getFailedCheckedRegion()) |region| return region;
    return module.checked_bodies.expr(root.expr).source_region;
}

fn finalizationImports(
    allocator: Allocator,
    root: checked.ImportedModuleView,
    imports: []const checked.PublishImportArtifact,
    available_modules: []const checked.ImportedModuleView,
) Allocator.Error![]checked.ImportedModuleView {
    var out = std.ArrayList(checked.ImportedModuleView).empty;
    errdefer out.deinit(allocator);

    for (imports) |import| {
        try appendUniqueImport(allocator, root, &out, import.view);
    }
    for (available_modules) |module| {
        try appendUniqueImport(allocator, root, &out, module);
    }

    return try out.toOwnedSlice(allocator);
}

fn appendUniqueImport(
    allocator: Allocator,
    root: checked.ImportedModuleView,
    out: *std.ArrayList(checked.ImportedModuleView),
    module: checked.ImportedModuleView,
) Allocator.Error!void {
    if (sameModuleIdentity(root, module)) return;
    for (out.items) |existing| {
        if (sameModuleIdentity(existing, module)) return;
    }
    try out.append(allocator, module);
}

fn sameModuleIdentity(a: checked.ImportedModuleView, b: checked.ImportedModuleView) bool {
    return std.meta.eql(a.module_identity.stable_hash, b.module_identity.stable_hash);
}

fn regionsEqual(a: base.Region, b: base.Region) bool {
    return a.start.offset == b.start.offset and a.end.offset == b.end.offset;
}

fn compileTimeRootForRequest(
    module: *const checked.CheckedModuleArtifact,
    request: checked.RootRequest,
) checked.ComptimeRootId {
    for (module.compile_time_roots.roots) |root| {
        const kind_matches = switch (request.kind) {
            .compile_time_constant => root.kind == .constant or root.kind == .hoisted_constant or root.kind == .numeral_conversion or root.kind == .quote_conversion,
            .compile_time_callable => root.kind == .callable_binding,
            .runtime_entrypoint,
            .provided_export,
            .platform_required_binding,
            .hosted_export,
            .test_expect,
            .repl_expr,
            .dev_expr,
            => finalizationInvariant("non compile-time request reached compile-time root lookup"),
        };
        if (kind_matches and rootSourceEql(root.source, request.source)) return root.id;
    }

    finalizationInvariant("compile-time root request did not match a checked root");
}

fn finishConstRoot(
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) void {
    if (root.kind != .constant and root.kind != .hoisted_constant) return;
    const node = switch (payload) {
        .const_node => |id| id,
        .pending,
        .fn_value,
        .expect,
        => finalizationInvariant("constant root finalized with non-constant payload"),
    };
    const const_ref = switch (root.kind) {
        .constant => blk: {
            const pattern = root.pattern orelse finalizationInvariant("constant root had no checked pattern");
            const top_level = module.top_level_values.lookupByPattern(pattern) orelse
                finalizationInvariant("constant root had no top-level value");
            break :blk switch (top_level.value) {
                .const_ref => |ref| ref,
                .procedure_binding => finalizationInvariant("constant root top-level value was not a constant"),
            };
        },
        .hoisted_constant => blk: {
            const hoisted = module.hoisted_constants.lookupByRoot(root.id) orelse
                finalizationInvariant("hoisted constant root had no hoisted const entry");
            break :blk hoisted.const_ref;
        },
        .callable_binding,
        .expect,
        .numeral_conversion,
        .quote_conversion,
        => unreachable,
    };
    const stored = checked.StoredConstTemplate{ .node = node };
    module.const_templates.fillStoredConst(const_ref, stored);
    if (root.kind == .constant) {
        module.exported_const_templates.fillStoredConst(const_ref, stored);
    }
}

fn rootSourceEql(a: checked.RootSource, b: checked.RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |left| left == b.def,
        .expr => |left| left == b.expr,
        .statement => |left| left == b.statement,
        .required_binding => |left| left == b.required_binding,
        .hoisted => |left| left.index == b.hoisted.index and left.expr == b.hoisted.expr,
    };
}

fn artifactMatches(a: anytype, b: checked.CheckedModuleArtifactKey) bool {
    return std.meta.eql(a.bytes, b.bytes);
}

fn finalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time finalization invariant violated: {s}", .{message});
    }
    unreachable;
}

test "compile-time finalization declarations are referenced" {
    std.testing.refAllDecls(@This());
}

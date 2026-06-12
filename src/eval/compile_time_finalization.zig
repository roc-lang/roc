//! Compile-time evaluation finalization.
//!
//! This module is intentionally only the checking finalizer boundary.
//! Compile-time values must be stored through the checked `ConstStore` path
//! produced by the post-check pipeline.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const CompilerHost = @import("compiler_host.zig");
const ConstStoreWriter = @import("const_store_writer.zig");
const interpreter_mod = @import("interpreter.zig");
const Interpreter = interpreter_mod.Interpreter;
const ExpectFailure = interpreter_mod.ExpectFailure;

/// Return the checking finalizer that evaluates compile-time roots.
pub fn finalizer() checked.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

fn finalize(
    _: ?*anyopaque,
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    imports: []const checked.PublishImportArtifact,
    available_modules: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    problem_store: ?*check.problem.Store,
) anyerror!void {
    const requests = module.root_requests.compile_time_requests;
    var had_problem = false;

    if (requests.len != 0) {
        const lowering_imports = try finalizationImports(allocator, checked.importedView(module), imports, available_modules);
        defer allocator.free(lowering_imports);

        var state = try RootCompletionState.init(allocator, module);
        defer state.deinit();

        var ready = std.ArrayList(checked.RootRequest).empty;
        defer ready.deinit(allocator);

        var pending = requests.len;
        while (pending > 0) {
            ready.clearRetainingCapacity();
            for (requests) |request| {
                const root_id = compileTimeRootForRequest(module, request);
                if (state.isDone(root_id)) continue;
                if (state.dependenciesComplete(request)) {
                    try ready.append(allocator, request);
                }
            }
            if (ready.items.len == 0) {
                finalizationInvariant("compile-time roots had a cyclic or incomplete local dependency");
            }
            if (try lowerEvalAndFinishRoots(
                allocator,
                module,
                lowering_imports,
                relation_modules,
                ready.items,
                &state,
                problem_store,
            )) had_problem = true;
            pending -= ready.items.len;
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
    module: *checked.CheckedModuleArtifact,
    statuses: []RootStatus,
    visited_templates: []u32,
    visit: u32,

    fn init(
        allocator: Allocator,
        module: *checked.CheckedModuleArtifact,
    ) Allocator.Error!RootCompletionState {
        const statuses = try allocator.alloc(RootStatus, module.compile_time_roots.roots.len);
        errdefer allocator.free(statuses);
        @memset(statuses, .pending);

        const visited_templates = try allocator.alloc(u32, module.checked_procedure_templates.templates.len);
        errdefer allocator.free(visited_templates);
        @memset(visited_templates, 0);

        return .{
            .module = module,
            .statuses = statuses,
            .visited_templates = visited_templates,
            .visit = 0,
        };
    }

    fn deinit(self: *RootCompletionState) void {
        const allocator = self.module.const_store.allocator;
        allocator.free(self.visited_templates);
        allocator.free(self.statuses);
        self.* = undefined;
    }

    fn isDone(self: *const RootCompletionState, root_id: checked.ComptimeRootId) bool {
        return self.statuses[@intFromEnum(root_id)] == .done;
    }

    fn markDone(self: *RootCompletionState, root_id: checked.ComptimeRootId) void {
        self.statuses[@intFromEnum(root_id)] = .done;
    }

    fn dependenciesComplete(
        self: *RootCompletionState,
        request: checked.RootRequest,
    ) bool {
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
        return !self.rootHasCompileTimeRequest(root_id) or self.isDone(root_id);
    }

    fn rootForConstRef(
        self: *RootCompletionState,
        const_ref: checked.ConstRef,
    ) ?checked.ComptimeRootId {
        if (!artifactMatches(const_ref.artifact, self.module.key)) return null;
        const owner = switch (const_ref.owner) {
            .top_level_binding => |top_level| top_level,
        };
        return self.module.compile_time_roots.lookupIdByPattern(owner.pattern) orelse
            finalizationInvariant("local const dependency had no compile-time root");
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
                break :blk !self.rootHasCompileTimeRequest(template.root) or self.isDone(template.root);
            },
        };
    }

    fn rootHasCompileTimeRequest(
        self: *RootCompletionState,
        root_id: checked.ComptimeRootId,
    ) bool {
        for (self.module.root_requests.compile_time_requests) |request| {
            if (compileTimeRootForRequest(self.module, request) == root_id) return true;
        }
        return false;
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
            .procedure_value => |procedure| self.procedureUseDependenciesComplete(procedure.procedure),
            .const_value => |const_value| self.constUseComplete(const_value.const_use),
        };
    }
};

fn lowerEvalAndFinishRoots(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    lowering_imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    requests: []const checked.RootRequest,
    state: *RootCompletionState,
    problem_store: ?*check.problem.Store,
) anyerror!bool {
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
    for (lowered.lir_result.const_roots.items) |root| {
        const root_id = compileTimeRootForRequest(module, root.request);
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

            const eval_result = try evalCompileTimeRoot(allocator, &interpreter, problem_store, module, compile_time_root, root.proc, root.ret_layout);
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

/// Unwrap the `Try` value a literal-conversion root evaluated to. `Ok` payloads
/// become the stored constant; `Err(InvalidNumeral(msg))` / `Err(BadQuotedBytes(msg))`
/// becomes a checking problem carrying the implementation's message.
fn finishLiteralConversionRoot(
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    problem_store: ?*check.problem.Store,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) anyerror!checked.CompileTimeRootPayload {
    const try_node = switch (payload) {
        .const_node => |node| node,
        else => finalizationInvariant("numeral conversion root did not store a constant"),
    };
    switch (module.const_store.get(try_node)) {
        // The from_numeral implementation itself crashed; that crash was
        // already stored (and reported when a problem store exists).
        .crash => return payload,
        else => {},
    }
    const try_tag = constTagValue(module, try_node);
    if (constTagNameIs(try_tag.tag_name, "Ok")) {
        if (try_tag.payloads.len != 1) finalizationInvariant("numeral conversion Ok did not carry one payload");
        return .{ .const_node = try_tag.payloads[0] };
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
        return error.CompileTimeProblem;
    }
    return .{ .const_node = try appendCrashConst(module, message) };
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
) anyerror!bool {
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
    proc: lir.LIR.LirProcSpecId,
    ret_layout: @import("layout").Idx,
) anyerror!Interpreter.EvalResult {
    return interpreter.eval(.{
        .proc_id = proc,
        .ret_layout = ret_layout,
    }) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.RuntimeError => finalizationInvariant("compile-time root produced a runtime error"),
        error.DivisionByZero => try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter.getRuntimeErrorMessage() orelse "Division by zero"),
        error.Crash => try reportCompileTimeCrash(allocator, problem_store, module, root, interpreter.getCrashMessage() orelse "Roc crashed"),
        error.ExpectErr => finalizationInvariant("compile-time root reached an expect_err statement"),
    };
}

fn reportCompileTimeCrash(
    allocator: Allocator,
    maybe_problem_store: ?*check.problem.Store,
    module: *const checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    message: []const u8,
) anyerror!Interpreter.EvalResult {
    const problem_store = maybe_problem_store orelse {
        finalizationInvariant("compile-time root crashed without a checking problem store");
    };
    const message_idx = try problem_store.putExtraString(message);
    const region = module.checked_bodies.expr(root.expr).source_region;
    _ = try problem_store.appendProblem(allocator, .{ .comptime_crash = .{
        .message = message_idx,
        .region = region,
    } });
    return error.CompileTimeProblem;
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

fn compileTimeRootForRequest(
    module: *const checked.CheckedModuleArtifact,
    request: checked.RootRequest,
) checked.ComptimeRootId {
    for (module.compile_time_roots.roots) |root| {
        const kind_matches = switch (request.kind) {
            .compile_time_constant => root.kind == .constant or root.kind == .numeral_conversion or root.kind == .quote_conversion,
            .compile_time_callable => root.kind == .callable_binding,
            else => finalizationInvariant("non compile-time request reached compile-time root lookup"),
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
    if (root.kind != .constant) return;
    const node = switch (payload) {
        .const_node => |id| id,
        else => finalizationInvariant("constant root finalized with non-constant payload"),
    };
    const pattern = root.pattern orelse finalizationInvariant("constant root had no checked pattern");
    const top_level = module.top_level_values.lookupByPattern(pattern) orelse
        finalizationInvariant("constant root had no top-level value");
    const const_ref = switch (top_level.value) {
        .const_ref => |ref| ref,
        .procedure_binding => finalizationInvariant("constant root top-level value was not a constant"),
    };
    const stored = checked.StoredConstTemplate{ .node = node };
    module.const_templates.fillStoredConst(const_ref, stored);
    module.exported_const_templates.fillStoredConst(const_ref, stored);
}

fn rootSourceEql(a: checked.RootSource, b: checked.RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |left| left == b.def,
        .expr => |left| left == b.expr,
        .statement => |left| left == b.statement,
        .required_binding => |left| left == b.required_binding,
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

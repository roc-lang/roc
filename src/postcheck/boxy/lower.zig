//! Boxy checked-to-LIR lowerer.
//!
//! This lowerer consumes checked modules plus an explicit `Plan.ProgramPlan` and
//! produces ownership-neutral LIR. It is the only `.boxy` producer of LIR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const Common = @import("../common.zig");
const Layouts = @import("layouts.zig");
const Plan = @import("plan.zig");
const solved_lir_lower = @import("../solved_lir_lower.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const RootMetadata = lir_core.RootMetadata.RootMetadata;

pub const RuntimeSchemaStore = solved_lir_lower.RuntimeSchemaStore;

pub const Output = struct {
    lir_result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,

    pub fn deinit(self: *Output) void {
        self.runtime_schemas.deinit();
        self.lir_result.deinit();
    }
};

pub const Options = struct {
    target_usize: base.target.TargetUsize = .native,
};

pub fn run(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    plan: *const Plan.ProgramPlan,
    options: Options,
) Common.LowerError!Output {
    var result = try LirProgram.Result.init(allocator, options.target_usize);
    errdefer result.deinit();

    var layout_plan = try Layouts.build(allocator, plan, &result.layouts, .{});
    defer layout_plan.deinit();

    var resolved_workers = try ResolvedWorkers.init(allocator, modules, plan);
    defer resolved_workers.deinit();

    var procedure_builder = ProcedureBuilder.init(allocator, modules, plan, &layout_plan, &resolved_workers, &result);
    defer procedure_builder.deinit();
    try procedure_builder.emitRoots();

    try appendRequestedLayouts(allocator, modules, roots, plan, &layout_plan, &result);

    return .{
        .lir_result = result,
        .runtime_schemas = RuntimeSchemaStore.init(allocator),
    };
}

const ProcedureModuleView = struct {
    key: checked.CheckedModuleArtifactKey,
    canonical_names: *const names.CanonicalNameStore,
    checked_types: checked.CheckedTypeStoreView,
    checked_bodies: checked.CheckedBodyStoreView,
    checked_procedure_templates: *const checked.CheckedProcedureTemplateTable,
    top_level_procedure_bindings: *const checked.TopLevelProcedureBindingTable,
};

const ResolvedWorker = struct {
    worker: Plan.WorkerPlanId,
    module_key: checked.CheckedModuleArtifactKey,
    module: ProcedureModuleView,
    template_ref: names.ProcedureTemplateRef,
    template: checked.CheckedProcedureTemplate,
    body: checked.CheckedBody,
};

const ResolvedWorkers = struct {
    allocator: Allocator,
    items: []ResolvedWorker,

    fn init(
        allocator: Allocator,
        modules: Common.CheckedModules,
        plan: *const Plan.ProgramPlan,
    ) Allocator.Error!ResolvedWorkers {
        const items = try allocator.alloc(ResolvedWorker, plan.workers.items.len);
        errdefer allocator.free(items);

        for (plan.workers.items, items) |worker, *resolved| {
            resolved.* = resolveWorkerProcedure(modules, worker);
        }

        return .{
            .allocator = allocator,
            .items = items,
        };
    }

    fn deinit(self: *ResolvedWorkers) void {
        self.allocator.free(self.items);
        self.* = undefined;
    }
};

fn resolveWorkerProcedure(modules: Common.CheckedModules, worker: Plan.WorkerPlan) ResolvedWorker {
    return switch (worker.source) {
        .procedure_template => |template| resolveProcedureTemplate(modules, worker.id, template),
        .procedure_binding => |binding| resolveProcedureBinding(modules, worker.id, rootProcedureModule(modules), binding),
        .procedure_use => |use| resolveProcedureUse(modules, worker.id, use),
    };
}

fn resolveProcedureUse(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    use: checked.ProcedureUseTemplate,
) ResolvedWorker {
    return switch (use.binding) {
        .top_level => |top_level| resolveProcedureBinding(
            modules,
            worker,
            procedureModuleByKey(modules, top_level.artifact),
            top_level.binding,
        ),
        .platform_required => |required| resolveProcedureBinding(
            modules,
            worker,
            procedureModuleByKey(modules, required.app_value.artifact),
            required.procedure_binding,
        ),
        .imported => boxyLowerInvariant("imported procedure use reached boxy worker resolution before imported body lowering is implemented"),
        .hosted => boxyLowerInvariant("hosted procedure use reached boxy worker resolution before hosted wrapper lowering is implemented"),
    };
}

fn resolveProcedureBinding(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    module: ProcedureModuleView,
    binding_ref: checked.TopLevelProcedureBindingRef,
) ResolvedWorker {
    const binding = module.top_level_procedure_bindings.get(binding_ref);
    return switch (binding.body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template| resolveProcedureTemplate(modules, worker, template),
            .lifted,
            .synthetic,
            => boxyLowerInvariant("non-checked procedure template reached boxy worker resolution"),
        },
        .callable_eval_template => boxyLowerInvariant("callable eval procedure binding reached runtime boxy worker resolution"),
    };
}

fn resolveProcedureTemplate(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    template_ref: names.ProcedureTemplateRef,
) ResolvedWorker {
    const module = procedureModuleByArtifactRef(modules, template_ref.artifact);
    const template = module.checked_procedure_templates.get(template_ref.template);
    const body_id = switch (template.body) {
        .checked_body => |body| body,
        .intrinsic_wrapper => boxyLowerInvariant("intrinsic wrapper reached boxy checked-body worker resolution"),
        .entry_wrapper => boxyLowerInvariant("compile-time entry wrapper reached runtime boxy worker resolution"),
    };

    return .{
        .worker = worker,
        .module_key = module.key,
        .module = module,
        .template_ref = template_ref,
        .template = template,
        .body = module.checked_bodies.body(body_id),
    };
}

fn rootProcedureModule(modules: Common.CheckedModules) ProcedureModuleView {
    const artifact = modules.root.module;
    return .{
        .key = artifact.key,
        .canonical_names = &artifact.canonical_names,
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .checked_procedure_templates = &artifact.checked_procedure_templates,
        .top_level_procedure_bindings = &artifact.top_level_procedure_bindings,
    };
}

fn procedureModuleFromImport(import: checked.ImportedModuleView) ProcedureModuleView {
    return .{
        .key = import.key,
        .canonical_names = import.canonical_names,
        .checked_types = import.checked_types,
        .checked_bodies = import.checked_bodies,
        .checked_procedure_templates = import.checked_procedure_templates,
        .top_level_procedure_bindings = import.top_level_procedure_bindings,
    };
}

fn procedureModuleByArtifactRef(modules: Common.CheckedModules, artifact: names.ArtifactRef) ProcedureModuleView {
    return procedureModuleByKey(modules, .{ .bytes = artifact.bytes });
}

fn procedureModuleByKey(modules: Common.CheckedModules, key: checked.CheckedModuleArtifactKey) ProcedureModuleView {
    if (artifactKeyEqual(modules.root.module.key, key)) return rootProcedureModule(modules);
    for (modules.imports) |import| {
        if (artifactKeyEqual(import.key, key)) return procedureModuleFromImport(import);
    }
    for (modules.root.relation_modules) |relation| {
        if (artifactKeyEqual(relation.key, key)) return procedureModuleFromImport(relation);
    }
    boxyLowerInvariant("boxy worker referenced a checked artifact that was not available to lowering");
}

fn artifactKeyEqual(a: checked.CheckedModuleArtifactKey, b: checked.CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

const ProcedureBuilder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    plan: *const Plan.ProgramPlan,
    layout_plan: *const Layouts.LayoutPlan,
    resolved_workers: *const ResolvedWorkers,
    result: *LirProgram.Result,
    worker_procs: []?LIR.LirProcSpecId,
    symbols: Common.SymbolGen = .{},

    fn init(
        allocator: Allocator,
        modules: Common.CheckedModules,
        plan: *const Plan.ProgramPlan,
        layout_plan: *const Layouts.LayoutPlan,
        resolved_workers: *const ResolvedWorkers,
        result: *LirProgram.Result,
    ) ProcedureBuilder {
        return .{
            .allocator = allocator,
            .modules = modules,
            .plan = plan,
            .layout_plan = layout_plan,
            .resolved_workers = resolved_workers,
            .result = result,
            .worker_procs = &.{},
        };
    }

    fn deinit(self: *ProcedureBuilder) void {
        self.allocator.free(self.worker_procs);
        self.* = undefined;
    }

    fn emitRoots(self: *ProcedureBuilder) Allocator.Error!void {
        self.worker_procs = try self.allocator.alloc(?LIR.LirProcSpecId, self.resolved_workers.items.len);
        @memset(self.worker_procs, null);

        for (self.plan.roots.items, self.layout_plan.roots.items) |root, root_layout| {
            if (root.id != root_layout.root) boxyLowerInvariant("boxy root layout table disagreed with root plan order");
            const worker_proc = try self.emitWorker(root.worker, root_layout);
            const root_proc = switch (root.wrapper_kind) {
                .private_worker_only => worker_proc,
                .host_shaped_wrapper => try self.emitHostWrapper(root_layout, worker_proc),
            };
            try self.result.root_procs.append(self.allocator, root_proc);
            try self.result.root_metadata.append(self.allocator, RootMetadata.fromCheckedRoot(root.request));
        }
    }

    fn emitWorker(
        self: *ProcedureBuilder,
        worker_id: Plan.WorkerPlanId,
        root_layout: Layouts.RootLayouts,
    ) Allocator.Error!LIR.LirProcSpecId {
        const index = @intFromEnum(worker_id);
        if (index >= self.worker_procs.len) boxyLowerInvariant("boxy root referenced a missing worker proc");
        if (self.worker_procs[index]) |existing| return existing;

        const resolved = self.resolved_workers.items[index];
        if (!artifactKeyEqual(resolved.module_key, self.modules.root.module.key)) {
            boxyLowerInvariant("non-root checked body reached boxy body lowering before imported body lowering is implemented");
        }

        var proc = ProcBodyBuilder.init(self, resolved.module, root_layout);
        defer proc.deinit();

        const body_expr = try self.bodyExprForWorker(resolved, &proc);
        const ret_layout = proc.workerReturnLayout();
        const ret_local = try proc.addFrameLocal(ret_layout);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        const body_stmt = try proc.lowerExprInto(ret_local, body_expr, ret_stmt);
        const args_span = try self.result.store.addLocalSpan(proc.arg_locals.items);
        const frame_span = try self.result.store.addLocalSpan(proc.frame_locals.items);
        const proc_id = try self.result.store.addProcSpec(.{
            .name = lirSymbol(self.symbols.fresh()),
            .args = args_span,
            .frame_locals = frame_span,
            .body = body_stmt,
            .ret_layout = ret_layout,
            .stack_probe = self.stackProbeForProc(args_span, frame_span, ret_layout),
        });
        self.worker_procs[index] = proc_id;
        return proc_id;
    }

    fn bodyExprForWorker(
        self: *ProcedureBuilder,
        resolved: ResolvedWorker,
        proc: *ProcBodyBuilder,
    ) Allocator.Error!checked.CheckedExprId {
        const root_expr = resolved.module.checked_bodies.expr(resolved.body.root_expr);
        return switch (root_expr.data) {
            .lambda => |lambda| blk: {
                const worker_args = self.layout_plan.rootLayoutSlice(proc.root_layout.worker_args);
                if (lambda.args.len != worker_args.len) {
                    boxyLowerInvariant("boxy worker lambda arity disagreed with worker root layout");
                }
                try proc.bindLambdaArgs(lambda.args);
                break :blk lambda.body;
            },
            else => resolved.body.root_expr,
        };
    }

    fn emitHostWrapper(
        self: *ProcedureBuilder,
        root_layout: Layouts.RootLayouts,
        worker_proc: LIR.LirProcSpecId,
    ) Allocator.Error!LIR.LirProcSpecId {
        const host_args = self.layout_plan.rootLayoutSlice(root_layout.host_args);
        const worker_args = self.layout_plan.rootLayoutSlice(root_layout.worker_args);
        if (host_args.len != worker_args.len) {
            boxyLowerInvariant("boxy host wrapper needed argument adaptation before adapters were emitted");
        }

        const arg_locals = try self.allocator.alloc(LIR.LocalId, host_args.len);
        defer self.allocator.free(arg_locals);
        for (host_args, worker_args, arg_locals) |host_arg, worker_arg, *local| {
            if (host_arg.layoutIdx() != worker_arg.layoutIdx()) {
                boxyLowerInvariant("boxy host wrapper needed argument layout adaptation before adapters were emitted");
            }
            local.* = try self.addLocal(host_arg.layoutIdx());
        }

        const host_ret = root_layout.host_ret orelse root_layout.host_value orelse
            boxyLowerInvariant("boxy host wrapper had no host return layout");
        const worker_ret = root_layout.worker_ret orelse root_layout.worker_value;
        if (host_ret.layoutIdx() != worker_ret.layoutIdx()) {
            boxyLowerInvariant("boxy host wrapper needed return layout adaptation before adapters were emitted");
        }

        const ret_local = try self.addLocal(host_ret.layoutIdx());
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        const call_stmt = try self.result.store.addCFStmt(.{ .assign_call = .{
            .target = ret_local,
            .proc = worker_proc,
            .args = try self.result.store.addLocalSpan(arg_locals),
            .next = ret_stmt,
        } });
        const args_span = try self.result.store.addLocalSpan(arg_locals);
        const frame_span = try self.result.store.addLocalSpan(&.{ret_local});
        return try self.result.store.addProcSpec(.{
            .name = lirSymbol(self.symbols.fresh()),
            .args = args_span,
            .frame_locals = frame_span,
            .body = call_stmt,
            .ret_layout = host_ret.layoutIdx(),
            .stack_probe = self.stackProbeForProc(args_span, frame_span, host_ret.layoutIdx()),
        });
    }

    fn addLocal(self: *ProcedureBuilder, layout_idx: @import("layout").Idx) Allocator.Error!LIR.LocalId {
        return try self.result.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn stackProbeForProc(
        self: *ProcedureBuilder,
        args: LIR.LocalSpan,
        frame_locals: LIR.LocalSpan,
        ret_layout: @import("layout").Idx,
    ) LIR.StackProbe {
        if (self.result.store.localSpanNeedsStackProbe(&self.result.layouts, args)) return .required;
        if (self.result.store.localSpanNeedsStackProbe(&self.result.layouts, frame_locals)) return .required;
        if (LIR.layoutNeedsStackProbe(&self.result.layouts, ret_layout)) return .required;
        return .default;
    }
};

const ProcBodyBuilder = struct {
    parent: *ProcedureBuilder,
    module: ProcedureModuleView,
    root_layout: Layouts.RootLayouts,
    arg_locals: std.ArrayList(LIR.LocalId),
    frame_locals: std.ArrayList(LIR.LocalId),
    binder_locals: []?LIR.LocalId,

    fn init(parent: *ProcedureBuilder, module: ProcedureModuleView, root_layout: Layouts.RootLayouts) ProcBodyBuilder {
        return .{
            .parent = parent,
            .module = module,
            .root_layout = root_layout,
            .arg_locals = .empty,
            .frame_locals = .empty,
            .binder_locals = &.{},
        };
    }

    fn deinit(self: *ProcBodyBuilder) void {
        self.parent.allocator.free(self.binder_locals);
        self.frame_locals.deinit(self.parent.allocator);
        self.arg_locals.deinit(self.parent.allocator);
        self.* = undefined;
    }

    fn bindLambdaArgs(self: *ProcBodyBuilder, args: []const checked.CheckedPatternId) Allocator.Error!void {
        try self.ensureBinderLocals();

        const worker_args = self.parent.layout_plan.rootLayoutSlice(self.root_layout.worker_args);
        for (args, worker_args) |pattern_id, arg_layout| {
            const local = try self.addArgLocal(arg_layout.layoutIdx());
            if (self.workerRuntimeLayoutForType(self.module.checked_bodies.pattern(pattern_id).ty).layoutIdx() != arg_layout.layoutIdx()) {
                boxyLowerInvariant("boxy worker lambda argument layout disagreed with checked pattern type");
            }
            self.bindPatternToLocal(pattern_id, local);
        }
    }

    fn lowerExprInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        expr_id: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const expr = self.module.checked_bodies.expr(expr_id);
        const saved_region = self.parent.result.store.current_region;
        defer self.parent.result.store.current_region = saved_region;
        self.parent.result.store.current_region = expr.source_region;

        return switch (expr.data) {
            .num => |num| try self.assignIntLiteral(target, num.value.toI128(), next),
            .typed_int => |int| try self.assignIntLiteral(target, int.value.toI128(), next),
            .frac_f32 => |frac| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = frac.value },
                .next = next,
            } }),
            .frac_f64 => |frac| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = frac.value },
                .next = next,
            } }),
            .dec => |dec| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .dec_literal = dec.value.num },
                .next = next,
            } }),
            .empty_record => try self.assignZst(target, next),
            .empty_list => try self.assignList(target, &.{}, next),
            .lookup_local => |lookup| try self.assignLocal(target, self.localForPattern(lookup.pattern), next),
            .field_access => |access| try self.lowerFieldAccessInto(target, access.receiver, access.field_name, next),
            .tuple_access => |access| try self.lowerTupleAccessInto(target, access.tuple, access.elem_index, next),
            .list => |items| try self.lowerListInto(target, items, next),
            .tuple => |items| try self.lowerTupleInto(target, items, next),
            .tag => |tag| try self.lowerTagInto(target, expr.ty, tag.name, tag.args, next),
            .zero_argument_tag => |tag| try self.lowerTagInto(target, expr.ty, tag.name, &.{}, next),
            .record => |record| blk: {
                if (record.ext != null) {
                    boxyLowerInvariant("open record expression reached boxy body lowering before record extension lowering was implemented");
                }
                break :blk try self.lowerRecordInto(target, expr.ty, record.fields, next);
            },
            .nominal => |nominal| try self.lowerNominalInto(target, nominal.backing_expr, next),
            .block => |block| blk: {
                try self.reserveBlockBindings(block.statements);
                var continuation = try self.lowerExprInto(target, block.final_expr, next);
                var index = block.statements.len;
                while (index > 0) {
                    index -= 1;
                    continuation = try self.lowerStatement(block.statements[index], continuation);
                }
                break :blk continuation;
            },
            .lambda => boxyLowerInvariant("nested lambda reached boxy expression lowering before erased callable lowering was emitted"),
            else => boxyLowerInvariant("checked expression form reached boxy body lowering before its LIR lowering was implemented"),
        };
    }

    fn lowerListInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        items: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const elem_layout = self.localListElemLayout(target);
        const elem_locals = try self.parent.allocator.alloc(LIR.LocalId, items.len);
        defer self.parent.allocator.free(elem_locals);

        for (items, elem_locals) |item, *local| {
            const expr = self.module.checked_bodies.expr(item);
            if (self.workerRuntimeLayoutForType(expr.ty).layoutIdx() != elem_layout) {
                boxyLowerInvariant("list expression element layout required box/adapt lowering before list construction");
            }
            local.* = try self.addFrameLocal(elem_layout);
        }

        var continuation = try self.assignList(target, elem_locals, next);
        var index = items.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.lowerExprInto(elem_locals[index], items[index], continuation);
        }
        return continuation;
    }

    fn lowerTupleInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        items: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.lowerExprsAsStructInto(target, items, next);
    }

    fn lowerExprsAsStructInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        items: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const field_locals = try self.parent.allocator.alloc(LIR.LocalId, items.len);
        defer self.parent.allocator.free(field_locals);

        for (items, field_locals) |item, *local| {
            const expr = self.module.checked_bodies.expr(item);
            local.* = try self.addFrameLocal(self.workerRuntimeLayoutForType(expr.ty).layoutIdx());
        }

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.parent.result.store.addLocalSpan(field_locals),
            .next = next,
        } });
        var index = items.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.lowerExprInto(field_locals[index], items[index], continuation);
        }
        return continuation;
    }

    fn lowerTagInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        tag_ty: checked.CheckedTypeId,
        name: names.TagNameId,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep_id = self.repForType(tag_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .tag_union => try self.lowerPlannedTagInto(target, rep, name, args, next),
            .bool_tag_union => try self.lowerBoolTagInto(target, name, args, next),
            .empty_tag_union => boxyLowerInvariant("empty tag union expression reached boxy body lowering"),
            else => boxyLowerInvariant("tag expression checked type did not have a boxy tag-union representation"),
        };
    }

    fn lowerBoolTagInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        name: names.TagNameId,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len != 0) {
            boxyLowerInvariant("builtin Bool tag expression carried a payload");
        }
        const variant_index = self.boolVariantIndex(name);
        return try self.parent.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = variant_index,
            .discriminant = variant_index,
            .payload = null,
            .next = next,
        } });
    }

    fn lowerPlannedTagInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        name: names.TagNameId,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const variant = self.tagVariant(rep, name);
        const payload_children = self.parent.plan.childSlice(variant.payloads);
        if (payload_children.len != args.len) {
            boxyLowerInvariant("tag expression payload count disagreed with its checked type representation");
        }
        for (payload_children, 0..) |child, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != name or payload.index != index) {
                        boxyLowerInvariant("tag variant payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag variant payload span included a non-payload child"),
            }
        }

        if (args.len == 0) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.parent.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = variant.index,
                .discriminant = variant.index,
                .payload = null,
                .next = next,
            } });
        }

        const payload_layout = if (self.isZstLocal(target))
            layout.Idx.zst
        else
            self.tagUnionPayloadLayout(self.parent.result.store.getLocal(target).layout_idx, variant.index);
        const payload_local = try self.addFrameLocal(payload_layout);
        if (self.isZstLocal(target) and !self.isZstLocal(payload_local)) {
            boxyLowerInvariant("zero-sized tag-union layout had a non-zero-sized payload");
        }

        const assign_tag = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.parent.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = variant.index,
                .discriminant = variant.index,
                .payload = payload_local,
                .next = next,
            } });

        if (args.len == 1) {
            return try self.lowerExprInto(payload_local, args[0], assign_tag);
        }
        return try self.lowerExprsAsStructInto(payload_local, args, assign_tag);
    }

    fn lowerNominalInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        backing_id: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing = self.module.checked_bodies.expr(backing_id);
        const backing_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(backing.ty).layoutIdx());
        const backing_layout = self.parent.result.store.getLocal(backing_local).layout_idx;
        const target_layout = self.parent.result.store.getLocal(target).layout_idx;
        const assign = if (target_layout == backing_layout)
            try self.assignLocal(target, backing_local, next)
        else
            try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{ .backing_ref = backing_local } },
                .next = next,
            } });
        return try self.lowerExprInto(backing_local, backing_id, assign);
    }

    fn lowerFieldAccessInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        receiver_id: checked.CheckedExprId,
        field_name: @TypeOf(@as(checked.CheckedRecordExprField, undefined).label),
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const receiver = self.module.checked_bodies.expr(receiver_id);
        const receiver_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(receiver.ty).layoutIdx());
        const read = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .field = .{
                .source = receiver_local,
                .field_idx = self.recordFieldLayoutIndex(receiver.ty, field_name),
            } },
            .next = next,
        } });
        return try self.lowerExprInto(receiver_local, receiver_id, read);
    }

    fn lowerTupleAccessInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        tuple_id: checked.CheckedExprId,
        elem_index: u32,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (elem_index > std.math.maxInt(u16)) {
            boxyLowerInvariant("tuple access element index exceeded LIR field index range");
        }
        const tuple = self.module.checked_bodies.expr(tuple_id);
        const tuple_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(tuple.ty).layoutIdx());
        const read = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .field = .{
                .source = tuple_local,
                .field_idx = @intCast(elem_index),
            } },
            .next = next,
        } });
        return try self.lowerExprInto(tuple_local, tuple_id, read);
    }

    fn lowerRecordInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        record_ty: checked.CheckedTypeId,
        expr_fields: []const checked.CheckedRecordExprField,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep_id = self.repForType(record_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        switch (rep.kind) {
            .record,
            .record_unbound,
            => {},
            else => boxyLowerInvariant("record expression checked type did not have a boxy record representation"),
        }

        const children = self.parent.plan.childSlice(rep.children);
        var field_count: usize = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => field_count += 1,
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("record representation had a non-record child role"),
            }
        }
        if (field_count != expr_fields.len) {
            boxyLowerInvariant("record expression field count disagreed with its checked type representation");
        }

        const field_locals = try self.parent.allocator.alloc(LIR.LocalId, field_count);
        defer self.parent.allocator.free(field_locals);
        const source_field_locals = try self.parent.allocator.alloc(?LIR.LocalId, expr_fields.len);
        defer self.parent.allocator.free(source_field_locals);
        @memset(source_field_locals, null);

        var layout_index: usize = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => |label| {
                    const source_index = recordExprFieldIndex(expr_fields, label);
                    if (source_field_locals[source_index] != null) {
                        boxyLowerInvariant("record expression field was selected more than once by representation order");
                    }
                    const local = try self.addFrameLocal(self.parent.layout_plan.rep_layouts[@intFromEnum(child.rep)].worker.layoutIdx());
                    field_locals[layout_index] = local;
                    source_field_locals[source_index] = local;
                    layout_index += 1;
                },
                .record_ext => {},
                else => unreachable,
            }
        }
        for (source_field_locals) |maybe_local| {
            if (maybe_local == null) {
                boxyLowerInvariant("record expression had a field outside its checked type representation");
            }
        }

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.parent.result.store.addLocalSpan(field_locals),
            .next = next,
        } });
        var source_index = expr_fields.len;
        while (source_index > 0) {
            source_index -= 1;
            continuation = try self.lowerExprInto(source_field_locals[source_index].?, expr_fields[source_index].value, continuation);
        }
        return continuation;
    }

    fn reserveBlockBindings(self: *ProcBodyBuilder, statements: []const checked.CheckedStatementId) Allocator.Error!void {
        for (statements) |statement_id| {
            const statement = self.module.checked_bodies.statement(statement_id);
            switch (statement.data) {
                .decl => |decl| try self.reservePatternBindings(decl.pattern),
                .import_,
                .alias_decl,
                .nominal_decl,
                .type_anno,
                .type_var_alias,
                => {},
                else => {},
            }
        }
    }

    fn lowerDeclPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        expr_id: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const source = switch (pattern.data) {
            .assign => self.localForPattern(pattern_id),
            else => try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx()),
        };
        const bound = try self.bindPatternFromLocal(pattern_id, source, next);
        return try self.lowerExprInto(source, expr_id, bound);
    }

    fn bindPatternFromLocal(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign => blk: {
                const target = self.localForPattern(pattern_id);
                break :blk if (target == source) next else try self.assignLocal(target, source, next);
            },
            .as => |as| blk: {
                const inner = try self.bindPatternFromLocal(as.pattern, source, next);
                const binder_local = self.localForBinder(as.binder);
                break :blk if (binder_local == source) inner else try self.assignLocal(binder_local, source, inner);
            },
            .tuple => |items| try self.bindTuplePattern(items, source, next),
            .record_destructure => |destructs| try self.bindRecordPattern(pattern.ty, destructs, source, next),
            .nominal => |nominal| try self.bindNominalPattern(nominal.backing_pattern, source, next),
            .underscore => next,
            .applied_tag,
            .list,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            .runtime_error,
            .pending,
            => boxyLowerInvariant("refutable or pending checked pattern reached boxy irrefutable declaration binding"),
        };
    }

    fn bindTuplePattern(
        self: *ProcBodyBuilder,
        items: []const checked.CheckedPatternId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = next;
        var index = items.len;
        while (index > 0) {
            index -= 1;
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tuple pattern index exceeded LIR field index range");
            }
            continuation = try self.bindFieldPattern(items[index], source, @intCast(index), continuation);
        }
        return continuation;
    }

    fn bindRecordPattern(
        self: *ProcBodyBuilder,
        record_ty: checked.CheckedTypeId,
        destructs: []const checked.CheckedRecordDestruct,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = next;
        var index = destructs.len;
        while (index > 0) {
            index -= 1;
            const destruct = destructs[index];
            const child_pattern = switch (destruct.kind) {
                .required,
                .sub_pattern,
                => |child| child,
                .rest => boxyLowerInvariant("record rest pattern reached boxy declaration binding before rest-value lowering"),
            };
            continuation = try self.bindFieldPattern(
                child_pattern,
                source,
                self.recordFieldLayoutIndex(record_ty, destruct.label),
                continuation,
            );
        }
        return continuation;
    }

    fn bindNominalPattern(
        self: *ProcBodyBuilder,
        backing_pattern: checked.CheckedPatternId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing = self.module.checked_bodies.pattern(backing_pattern);
        const backing_layout = self.workerRuntimeLayoutForType(backing.ty).layoutIdx();
        const source_layout = self.parent.result.store.getLocal(source).layout_idx;
        if (backing_layout != source_layout) {
            boxyLowerInvariant("nominal pattern required explicit nominal boundary lowering before binding");
        }
        return try self.bindPatternFromLocal(backing_pattern, source, next);
    }

    fn bindFieldPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        field_index: u16,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const field_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.bindPatternFromLocal(pattern_id, field_local, next);
        const read = if (self.isZstLocal(field_local))
            try self.assignZst(field_local, bound)
        else
            try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                .target = field_local,
                .op = .{ .field = .{
                    .source = source,
                    .field_idx = field_index,
                } },
                .next = bound,
            } });
        return read;
    }

    fn lowerStatement(
        self: *ProcBodyBuilder,
        statement_id: checked.CheckedStatementId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const statement = self.module.checked_bodies.statement(statement_id);
        const saved_region = self.parent.result.store.current_region;
        defer self.parent.result.store.current_region = saved_region;
        self.parent.result.store.current_region = statement.source_region;

        return switch (statement.data) {
            .decl => |decl| try self.lowerDeclPattern(decl.pattern, decl.expr, next),
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            => next,
            else => boxyLowerInvariant("checked statement form reached boxy body lowering before its LIR lowering was implemented"),
        };
    }

    fn assignIntLiteral(self: *ProcBodyBuilder, target: LIR.LocalId, value: i128, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i128_literal = .{
                .value = value,
                .layout_idx = self.parent.result.store.getLocal(target).layout_idx,
            } },
            .next = next,
        } });
    }

    fn assignZst(self: *ProcBodyBuilder, target: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = LIR.LocalSpan.empty(),
            .next = next,
        } });
    }

    fn assignList(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        elems: []const LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.parent.result.store.addLocalSpan(elems),
            .next = next,
        } });
    }

    fn assignLocal(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .local = source },
            .next = next,
        } });
    }

    fn addArgLocal(self: *ProcBodyBuilder, layout_idx: @import("layout").Idx) Allocator.Error!LIR.LocalId {
        const local = try self.parent.addLocal(layout_idx);
        try self.arg_locals.append(self.parent.allocator, local);
        return local;
    }

    fn addFrameLocal(self: *ProcBodyBuilder, layout_idx: @import("layout").Idx) Allocator.Error!LIR.LocalId {
        const local = try self.parent.addLocal(layout_idx);
        try self.frame_locals.append(self.parent.allocator, local);
        return local;
    }

    fn ensureBinderLocals(self: *ProcBodyBuilder) Allocator.Error!void {
        if (self.binder_locals.len != 0) return;
        self.binder_locals = try self.parent.allocator.alloc(?LIR.LocalId, self.module.checked_bodies.patternBinderCount());
        @memset(self.binder_locals, null);
    }

    fn reservePatternBindings(self: *ProcBodyBuilder, pattern_id: checked.CheckedPatternId) Allocator.Error!void {
        try self.ensureBinderLocals();
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => |binder| try self.reserveBinderLocal(binder, pattern.ty),
            .as => |as| {
                try self.reserveBinderLocal(as.binder, pattern.ty);
                try self.reservePatternBindings(as.pattern);
            },
            .tuple => |items| for (items) |item| try self.reservePatternBindings(item),
            .record_destructure => |destructs| {
                for (destructs) |destruct| {
                    switch (destruct.kind) {
                        .required,
                        .sub_pattern,
                        .rest,
                        => |child| try self.reservePatternBindings(child),
                    }
                }
            },
            .nominal => |nominal| try self.reservePatternBindings(nominal.backing_pattern),
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            => {},
            .applied_tag,
            .list,
            .runtime_error,
            .pending,
            => boxyLowerInvariant("refutable or pending checked pattern reached boxy irrefutable binder reservation"),
        }
    }

    fn bindPatternToLocal(self: *ProcBodyBuilder, pattern_id: checked.CheckedPatternId, local: LIR.LocalId) void {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => |binder| self.bindLocal(binder, local),
            else => boxyLowerInvariant("boxy pattern required pattern lowering before it was emitted"),
        }
    }

    fn reserveBinderLocal(
        self: *ProcBodyBuilder,
        binder: checked.PatternBinderId,
        ty: checked.CheckedTypeId,
    ) Allocator.Error!void {
        const local = try self.addFrameLocal(self.workerRuntimeLayoutForType(ty).layoutIdx());
        self.bindLocal(binder, local);
    }

    fn bindLocal(self: *ProcBodyBuilder, binder: checked.PatternBinderId, local: LIR.LocalId) void {
        const index = @intFromEnum(binder);
        if (index >= self.binder_locals.len) boxyLowerInvariant("boxy pattern referenced a missing pattern binder");
        if (self.binder_locals[index] != null) boxyLowerInvariant("boxy pattern bound the same pattern binder more than once");
        self.binder_locals[index] = local;
    }

    fn localForBinder(self: *ProcBodyBuilder, binder: checked.PatternBinderId) LIR.LocalId {
        const binder_index = @intFromEnum(binder);
        if (binder_index >= self.binder_locals.len) boxyLowerInvariant("boxy local lookup referenced a missing binder local");
        return self.binder_locals[binder_index] orelse
            boxyLowerInvariant("boxy local lookup referenced a binder before it was bound");
    }

    fn localForPattern(self: *ProcBodyBuilder, pattern: checked.CheckedPatternId) LIR.LocalId {
        const pattern_index = @intFromEnum(pattern);
        if (pattern_index >= self.module.checked_bodies.pattern_binder_by_pattern.len) {
            boxyLowerInvariant("boxy local lookup referenced a pattern without binder metadata");
        }
        const binder = self.module.checked_bodies.pattern_binder_by_pattern[pattern_index] orelse
            boxyLowerInvariant("boxy local lookup referenced a non-binding pattern");
        return self.localForBinder(binder);
    }

    fn workerReturnLayout(self: *const ProcBodyBuilder) @import("layout").Idx {
        if (self.root_layout.worker_ret) |ret| return ret.layoutIdx();
        return self.root_layout.worker_value.layoutIdx();
    }

    fn repForType(self: *const ProcBodyBuilder, ty: checked.CheckedTypeId) Plan.TypeRepId {
        return self.parent.plan.repForSourceType(ty) orelse
            boxyLowerInvariant("checked body referenced a type missing from the boxy representation plan");
    }

    fn workerRuntimeLayoutForType(self: *const ProcBodyBuilder, ty: checked.CheckedTypeId) Layouts.RuntimeLayout {
        return self.parent.layout_plan.rep_layouts[@intFromEnum(self.repForType(ty))].worker;
    }

    fn requireEmptyRecordExtension(self: *const ProcBodyBuilder, rep_id: Plan.TypeRepId) void {
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.kind != .empty_record) {
            boxyLowerInvariant("open record representation reached boxy body lowering without an explicit closed row");
        }
    }

    fn recordExprFieldIndex(
        fields: []const checked.CheckedRecordExprField,
        label: @TypeOf(@as(checked.CheckedRecordExprField, undefined).label),
    ) usize {
        var found: ?usize = null;
        for (fields, 0..) |field, index| {
            if (field.label != label) continue;
            if (found != null) {
                boxyLowerInvariant("record expression contained the same field label more than once");
            }
            found = index;
        }
        return found orelse boxyLowerInvariant("record expression was missing a field from its checked type representation");
    }

    fn recordFieldLayoutIndex(
        self: *const ProcBodyBuilder,
        record_ty: checked.CheckedTypeId,
        field_name: @TypeOf(@as(checked.CheckedRecordExprField, undefined).label),
    ) u16 {
        const rep = self.parent.plan.representations.items[@intFromEnum(self.repForType(record_ty))];
        switch (rep.kind) {
            .record,
            .record_unbound,
            => {},
            else => boxyLowerInvariant("record field access receiver did not have a boxy record representation"),
        }

        var index: u16 = 0;
        for (self.parent.plan.childSlice(rep.children)) |child| {
            switch (child.role) {
                .record_field => |label| {
                    if (label == field_name) return index;
                    index += 1;
                },
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("record field access representation had a non-record child role"),
            }
        }
        boxyLowerInvariant("record field access referenced a field outside its checked type representation");
    }

    const TagVariantLookup = struct {
        index: u16,
        payloads: Plan.Span,
    };

    fn tagVariant(
        self: *const ProcBodyBuilder,
        rep: Plan.TypeRepresentation,
        name: names.TagNameId,
    ) TagVariantLookup {
        const variants = self.parent.plan.tagVariantSlice(rep.tag_variants);
        for (variants, 0..) |variant, index| {
            if (variant.name != name) continue;
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag variant index exceeded LIR variant range");
            }
            return .{
                .index = @intCast(index),
                .payloads = variant.payloads,
            };
        }
        boxyLowerInvariant("tag expression referenced a variant outside its checked type representation");
    }

    fn boolVariantIndex(self: *const ProcBodyBuilder, name: names.TagNameId) u16 {
        const tag_name = self.module.canonical_names.tagLabelText(name);
        if (std.mem.eql(u8, tag_name, "False")) return 0;
        if (std.mem.eql(u8, tag_name, "True")) return 1;
        boxyLowerInvariant("builtin Bool tag expression referenced a non-Bool tag");
    }

    fn tagUnionPayloadLayout(self: *const ProcBodyBuilder, tag_union_layout_idx: layout.Idx, variant_index: u16) layout.Idx {
        const tag_union_layout = self.parent.result.layouts.getLayout(tag_union_layout_idx);
        return switch (tag_union_layout.tag) {
            .tag_union => blk: {
                const data = self.parent.result.layouts.getTagUnionData(tag_union_layout.getTagUnion().idx);
                const variants = self.parent.result.layouts.getTagUnionVariants(data);
                if (variant_index >= variants.len) {
                    boxyLowerInvariant("tag payload variant exceeded committed tag-union layout");
                }
                break :blk variants.get(@intCast(variant_index)).payload_layout;
            },
            .zst, .scalar => .zst,
            else => boxyLowerInvariant("tag payload operation expected tag-union layout"),
        };
    }

    fn localListElemLayout(self: *const ProcBodyBuilder, local: LIR.LocalId) layout.Idx {
        const list_layout_idx = self.parent.result.store.getLocal(local).layout_idx;
        const list_layout = self.parent.result.layouts.getLayout(list_layout_idx);
        return switch (list_layout.tag) {
            .list => list_layout.getIdx(),
            .list_of_zst => .zst,
            else => boxyLowerInvariant("list expression target was not a list layout"),
        };
    }

    fn isZstLocal(self: *const ProcBodyBuilder, local: LIR.LocalId) bool {
        return self.parent.result.layouts.isZeroSized(
            self.parent.result.layouts.getLayout(self.parent.result.store.getLocal(local).layout_idx),
        );
    }
};

fn appendRequestedLayouts(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    plan: *const Plan.ProgramPlan,
    layout_plan: *const Layouts.LayoutPlan,
    result: *LirProgram.Result,
) Allocator.Error!void {
    if (roots.layout_requests.len == 0) return;
    const root_rep_start = plan.roots.items.len;
    if (plan.root_reps.items.len < root_rep_start + roots.layout_requests.len) {
        boxyLowerInvariant("boxy plan root representations did not cover requested layouts");
    }

    var const_plans = try ConstPlanBuilder.init(allocator, modules, plan, result);
    defer const_plans.deinit();

    const root_types = modules.root.module.checked_types.view();
    for (roots.layout_requests, 0..) |checked_type, index| {
        const rep_id = plan.root_reps.items[root_rep_start + index];
        try result.requested_layouts.append(allocator, .{
            .ty = root_types.rootKey(checked_type),
            .checked_type = checked_type,
            .layout_idx = layout_plan.rep_layouts[@intFromEnum(rep_id)].host.layoutIdx(),
            .plan = try const_plans.constPlanForRep(rep_id),
        });
    }
}

const ConstPlanBuilder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    plan: *const Plan.ProgramPlan,
    result: *LirProgram.Result,
    by_rep: []?LirProgram.ConstPlanId,

    fn init(
        allocator: Allocator,
        modules: Common.CheckedModules,
        plan: *const Plan.ProgramPlan,
        result: *LirProgram.Result,
    ) Allocator.Error!ConstPlanBuilder {
        const by_rep = try allocator.alloc(?LirProgram.ConstPlanId, plan.representations.items.len);
        @memset(by_rep, null);
        return .{
            .allocator = allocator,
            .modules = modules,
            .plan = plan,
            .result = result,
            .by_rep = by_rep,
        };
    }

    fn deinit(self: *ConstPlanBuilder) void {
        self.allocator.free(self.by_rep);
        self.* = undefined;
    }

    fn constPlanForRep(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId) Allocator.Error!LirProgram.ConstPlanId {
        const index = @intFromEnum(rep_id);
        if (self.by_rep[index]) |existing| return existing;

        const rep = self.plan.representations.items[index];
        switch (rep.kind) {
            .alias => {
                const child = try self.constPlanForChild(rep_id, .alias_backing);
                self.by_rep[index] = child;
                return child;
            },
            .nominal => |kind| if (kind == .builtin_other) {
                const child = try self.constPlanForChild(rep_id, .nominal_backing);
                self.by_rep[index] = child;
                return child;
            },
            else => {},
        }

        const id: LirProgram.ConstPlanId = @enumFromInt(@as(u32, @intCast(self.result.const_plans.items.len)));
        try self.result.const_plans.append(self.allocator, .pending);
        self.by_rep[index] = id;

        const built = try self.buildConstPlan(rep_id);
        self.result.const_plans.items[@intFromEnum(id)] = built;
        return id;
    }

    fn buildConstPlan(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId) Allocator.Error!LirProgram.ConstPlan {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .in_progress => boxyLowerInvariant("in-progress boxy representation reached const plan output"),
            .dynamic => boxyLowerInvariant("dynamic boxy value reached const plan output before descriptor support"),
            .erased_callable => boxyLowerInvariant("erased callable reached const plan output before callable static-data support"),
            .primitive => |primitive| switch (primitive) {
                .str => .str,
                else => .scalar,
            },
            .bool_tag_union => .scalar,
            .empty_record,
            .empty_tag_union,
            => .zst,
            .alias => boxyLowerInvariant("alias representation was not redirected before const plan output"),
            .list => .{ .list = try self.constPlanForChild(rep_id, .list_elem) },
            .box => .{ .box = try self.constPlanForChild(rep_id, .box_payload) },
            .record,
            .record_unbound,
            => try self.structConstPlan(rep, .record_field, .record),
            .tuple => try self.structConstPlan(rep, .tuple_elem, .tuple),
            .tag_union => try self.tagUnionConstPlan(rep),
            .nominal => |kind| switch (kind) {
                .transparent => .{ .named = .{
                    .named_type = .{
                        .module = moduleDigestFromId(self.modules.root.module.key),
                        .ty = rep.source_type,
                    },
                    .backing = try self.constPlanForChild(rep_id, .nominal_backing),
                } },
                .opaque_nominal => boxyLowerInvariant("opaque nominal reached const plan output before opaque static-data support"),
                .builtin_other => boxyLowerInvariant("builtin nominal representation was not redirected before const plan output"),
            },
        };
    }

    fn constPlanForChild(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) Allocator.Error!LirProgram.ConstPlanId {
        return try self.constPlanForRep(self.requiredSingleChild(rep_id, role).rep);
    }

    const StructPlanKind = enum {
        tuple,
        record,
    };

    fn structConstPlan(
        self: *ConstPlanBuilder,
        rep: Plan.TypeRepresentation,
        comptime role_tag: std.meta.Tag(Plan.ChildRole),
        comptime kind: StructPlanKind,
    ) Allocator.Error!LirProgram.ConstPlan {
        var count: usize = 0;
        for (self.plan.childSlice(rep.children)) |child| {
            switch (child.role) {
                role_tag => count += 1,
                else => {},
            }
        }

        const plans = try self.allocator.alloc(LirProgram.ConstPlanId, count);
        errdefer self.allocator.free(plans);
        var cursor: usize = 0;
        for (self.plan.childSlice(rep.children)) |child| {
            switch (child.role) {
                role_tag => {
                    plans[cursor] = try self.constPlanForRep(child.rep);
                    cursor += 1;
                },
                else => {},
            }
        }

        return switch (kind) {
            .tuple => .{ .tuple = plans },
            .record => .{ .record = plans },
        };
    }

    fn tagUnionConstPlan(self: *ConstPlanBuilder, rep: Plan.TypeRepresentation) Allocator.Error!LirProgram.ConstPlan {
        const tag_variants = self.plan.tagVariantSlice(rep.tag_variants);

        const variants = try self.allocator.alloc(LirProgram.ConstTagVariant, tag_variants.len);
        var initialized: usize = 0;
        errdefer {
            for (variants[0..initialized]) |variant| {
                self.allocator.free(variant.name);
                self.allocator.free(variant.payloads);
            }
            self.allocator.free(variants);
        }

        for (tag_variants, variants, 0..) |tag_variant, *variant, discriminant| {
            variant.* = try self.buildTagVariant(tag_variant, discriminant);
            initialized += 1;
        }

        return .{ .tag_union = variants };
    }

    fn buildTagVariant(
        self: *ConstPlanBuilder,
        variant: Plan.TagVariant,
        discriminant: usize,
    ) Allocator.Error!LirProgram.ConstTagVariant {
        const root_names = &self.modules.root.module.canonical_names;
        const name = try self.allocator.dupe(u8, root_names.tagLabelText(variant.name));
        errdefer self.allocator.free(name);

        const payload_children = self.plan.childSlice(variant.payloads);
        const payloads = try self.allocator.alloc(LirProgram.ConstPlanId, payload_children.len);
        errdefer self.allocator.free(payloads);
        for (payload_children, payloads, 0..) |child, *payload_plan, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != variant.name or payload.index != index) {
                        boxyLowerInvariant("tag variant payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag variant payload span included a non-payload child"),
            }
            payload_plan.* = try self.constPlanForRep(child.rep);
        }

        return .{
            .name = name,
            .checked_name = variant.name,
            .discriminant = @intCast(discriminant),
            .payloads = payloads,
        };
    }

    fn requiredSingleChild(self: *ConstPlanBuilder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) Plan.RepChild {
        var found: ?Plan.RepChild = null;
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        for (self.plan.childSlice(rep.children)) |child| {
            if (sameChildRole(child.role, role)) {
                if (found != null) boxyLowerInvariant("representation had duplicate required child role");
                found = child;
            }
        }
        return found orelse boxyLowerInvariant("representation was missing required child role");
    }
};

fn sameChildRole(a: Plan.ChildRole, b: Plan.ChildRole) bool {
    return switch (a) {
        .alias_backing => b == .alias_backing,
        .nominal_backing => b == .nominal_backing,
        .record_ext => b == .record_ext,
        .tag_ext => b == .tag_ext,
        .list_elem => b == .list_elem,
        .box_payload => b == .box_payload,
        else => false,
    };
}

fn moduleDigestFromId(key: checked.ModuleId) names.CheckedModuleDigest {
    return .{ .bytes = key.bytes };
}

fn lirSymbol(symbol: Common.Symbol) LIR.Symbol {
    return LIR.Symbol.fromRaw(@intCast(@intFromEnum(symbol)));
}

fn boxyLowerInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("boxy lower invariant violated: {s}", .{message});
    }
    unreachable;
}

test "boxy lowerer returns an empty LIR program for an empty plan" {
    const gpa = std.testing.allocator;

    var plan = Plan.ProgramPlan.init(gpa);
    defer plan.deinit();

    var out = try run(gpa, .{ .root = undefined }, .{}, &plan, .{});
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 0), out.lir_result.store.proc_specs.items.len);
    try std.testing.expectEqual(@as(usize, 0), out.lir_result.root_procs.items.len);
}

test "boxy lowerer resolves procedure-template workers to checked bodies" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(3),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(9), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    var plan = Plan.ProgramPlan.init(gpa);
    defer plan.deinit();
    try plan.workers.append(gpa, .{
        .id = @enumFromInt(0),
        .request = dummyRootRequest(),
        .source = .{ .procedure_template = template_ref },
        .checked_type = @enumFromInt(9),
        .rep = @enumFromInt(0),
    });

    var resolved = try ResolvedWorkers.init(gpa, .{ .root = .{ .module = &artifact, .roots = undefined } }, &plan);
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expectEqual(@as(Plan.WorkerPlanId, @enumFromInt(0)), resolved.items[0].worker);
    try std.testing.expect(names.procedureTemplateRefEql(template_ref, resolved.items[0].template_ref));
    try std.testing.expectEqual(@as(checked.CheckedBodyId, @enumFromInt(0)), resolved.items[0].body.id);
    try std.testing.expectEqual(@as(checked.CheckedExprId, @enumFromInt(3)), resolved.items[0].body.root_expr);
}

test "boxy lowerer resolves top-level direct bindings to checked bodies" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(5),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(7), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    var bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = typeSchemeKey(1),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(template_ref),
                .template = .{ .checked = template_ref },
            } },
        },
    };
    artifact.top_level_procedure_bindings = .{ .bindings = &bindings };

    var plan = Plan.ProgramPlan.init(gpa);
    defer plan.deinit();
    try plan.workers.append(gpa, .{
        .id = @enumFromInt(0),
        .request = dummyRootRequest(),
        .source = .{ .procedure_binding = @enumFromInt(0) },
        .checked_type = @enumFromInt(7),
        .rep = @enumFromInt(0),
    });

    var resolved = try ResolvedWorkers.init(gpa, .{ .root = .{ .module = &artifact, .roots = undefined } }, &plan);
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expect(names.procedureTemplateRefEql(template_ref, resolved.items[0].template_ref));
    try std.testing.expectEqual(@as(checked.CheckedBodyId, @enumFromInt(0)), resolved.items[0].body.id);
    try std.testing.expectEqual(@as(checked.CheckedExprId, @enumFromInt(5)), resolved.items[0].body.root_expr);
}

test "boxy lowerer emits private worker proc for zero-arg numeric lambda root" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(42), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(1), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.root_procs.items.len);
    try std.testing.expectEqual(@as(usize, 1), out.lir_result.root_metadata.items.len);
    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    try std.testing.expect(proc.args.isEmpty());
    try std.testing.expectEqual(@as(LIR.LocalSpan, .{ .start = 0, .len = 1 }), proc.frame_locals);
    try std.testing.expectEqual(@as(@TypeOf(proc.ret_layout), .u64), proc.ret_layout);

    const assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (assign.value) {
        .i128_literal => |literal| {
            try std.testing.expectEqual(@as(i128, 42), literal.value);
            try std.testing.expectEqual(@as(@TypeOf(literal.layout_idx), .u64), literal.layout_idx);
        },
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = assign.target } }, out.lir_result.store.getCFStmt(assign.next));
}

test "boxy lowerer emits block declaration bindings with checked type layouts" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .decl = .{
            .pattern = @enumFromInt(0),
            .expr = @enumFromInt(2),
        } },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(3),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(1), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    try std.testing.expectEqual(@as(LIR.LocalSpan, .{ .start = 0, .len = 2 }), proc.frame_locals);

    const decl_assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (decl_assign.value) {
        .i128_literal => |literal| {
            try std.testing.expectEqual(@as(i128, 99), literal.value);
            try std.testing.expectEqual(@as(@TypeOf(literal.layout_idx), .u64), literal.layout_idx);
        },
        else => return error.TestUnexpectedResult,
    }

    const final_copy = out.lir_result.store.getCFStmt(decl_assign.next).assign_ref;
    try std.testing.expectEqual(decl_assign.target, final_copy.op.local);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = final_copy.target } }, out.lir_result.store.getCFStmt(final_copy.next));
}

test "boxy lowerer destructures tuple declaration patterns" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)),
        @as(checked.CheckedTypeId, @enumFromInt(0)),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .tuple = .{ .start = 0, .len = 2 },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(1),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(1),
        .pattern = @enumFromInt(2),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        null,
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        @as(?checked.PatternBinderId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.pattern_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedPatternId, @enumFromInt(1)),
        @as(checked.CheckedPatternId, @enumFromInt(2)),
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .tuple = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(1) },
    });

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .decl = .{
            .pattern = @enumFromInt(0),
            .expr = @enumFromInt(2),
        } },
    });
    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(5)),
        @as(checked.CheckedExprId, @enumFromInt(6)),
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(4),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .tuple = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(0), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(2), .resolved = null } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(6),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(2), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    const tuple = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const read_first = out.lir_result.store.getCFStmt(tuple.next).assign_ref;
    const bind_first = out.lir_result.store.getCFStmt(read_first.next).assign_ref;
    const read_second = out.lir_result.store.getCFStmt(bind_first.next).assign_ref;
    const bind_second = out.lir_result.store.getCFStmt(read_second.next).assign_ref;
    const final_copy = out.lir_result.store.getCFStmt(bind_second.next).assign_ref;

    switch (first.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 1), value.value),
        else => return error.TestUnexpectedResult,
    }
    switch (second.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 2), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(tuple.target, read_first.op.field.source);
    try std.testing.expectEqual(@as(u16, 0), read_first.op.field.field_idx);
    try std.testing.expectEqual(read_first.target, bind_first.op.local);
    try std.testing.expectEqual(tuple.target, read_second.op.field.source);
    try std.testing.expectEqual(@as(u16, 1), read_second.op.field.field_idx);
    try std.testing.expectEqual(read_second.target, bind_second.op.local);
    try std.testing.expectEqual(bind_second.target, final_copy.op.local);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = final_copy.target } }, out.lir_result.store.getCFStmt(final_copy.next));
}

test "boxy lowerer emits tuple construction in element order" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)),
        @as(checked.CheckedTypeId, @enumFromInt(0)),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .tuple = .{ .start = 0, .len = 2 },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .tuple = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(2), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    try std.testing.expectEqual(@as(usize, 3), out.lir_result.store.getLocalSpan(proc.frame_locals).len);

    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (first.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 1), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    switch (second.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 2), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const build = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const fields = out.lir_result.store.getLocalSpan(build.fields);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expectEqual(first.target, fields[0]);
    try std.testing.expectEqual(second.target, fields[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = build.target } }, out.lir_result.store.getCFStmt(build.next));
}

test "boxy lowerer emits tuple access as field read" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)),
        @as(checked.CheckedTypeId, @enumFromInt(0)),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .tuple = .{ .start = 0, .len = 2 },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(3)),
        @as(checked.CheckedExprId, @enumFromInt(4)),
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .tuple_access = .{ .tuple = @enumFromInt(2), .elem_index = 1 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .tuple = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(2), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    const build = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const read = out.lir_result.store.getCFStmt(build.next).assign_ref;
    switch (read.op) {
        .field => |field| {
            try std.testing.expectEqual(build.target, field.source);
            try std.testing.expectEqual(@as(u16, 1), field.field_idx);
        },
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = read.target } }, out.lir_result.store.getCFStmt(read.next));
}

test "boxy lowerer emits record construction in layout order after source-order evaluation" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const field_a: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(1);
    const field_b: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(2);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.record_field_pool.appendSlice(gpa, &.{
        .{ .name = field_a, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
        .{ .name = field_b, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(1) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(2),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.record_expr_field_pool.appendSlice(gpa, &.{
        .{ .label = field_b, .value = @as(checked.CheckedExprId, @enumFromInt(2)) },
        .{ .label = field_a, .value = @as(checked.CheckedExprId, @enumFromInt(3)) },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .record = .{
            .fields = .{ .start = 0, .len = 2 },
            .ext = null,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(3), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(3),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    try std.testing.expectEqual(@as(usize, 3), out.lir_result.store.getLocalSpan(proc.frame_locals).len);

    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (first.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 2), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    switch (second.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 1), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const build = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const fields = out.lir_result.store.getLocalSpan(build.fields);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expectEqual(second.target, fields[0]);
    try std.testing.expectEqual(first.target, fields[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = build.target } }, out.lir_result.store.getCFStmt(build.next));
}

test "boxy lowerer emits record field access using layout field index" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const field_a: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(1);
    const field_b: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(2);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.record_field_pool.appendSlice(gpa, &.{
        .{ .name = field_a, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
        .{ .name = field_b, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(1) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.record_expr_field_pool.appendSlice(gpa, &.{
        .{ .label = field_b, .value = @as(checked.CheckedExprId, @enumFromInt(3)) },
        .{ .label = field_a, .value = @as(checked.CheckedExprId, @enumFromInt(4)) },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .field_access = .{ .receiver = @enumFromInt(2), .field_name = field_b } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .record = .{
            .fields = .{ .start = 0, .len = 2 },
            .ext = null,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(3), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(3),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    const build = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const read = out.lir_result.store.getCFStmt(build.next).assign_ref;
    switch (read.op) {
        .field => |field| {
            try std.testing.expectEqual(build.target, field.source);
            try std.testing.expectEqual(@as(u16, 1), field.field_idx);
        },
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = read.target } }, out.lir_result.store.getCFStmt(read.next));
}

test "boxy lowerer emits nominal construction for representation-equivalent backing" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const nominal_key = names.NominalTypeKey{
        .module_name = @enumFromInt(1),
        .type_name = @enumFromInt(2),
        .source_decl = 3,
    };

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.nominal_declarations.append(gpa, .{
        .id = @enumFromInt(0),
        .nominal = nominal_key,
        .declaration_root = @enumFromInt(1),
        .backing = @enumFromInt(0),
        .pf_start = 0,
        .pf_len = 0,
        .df_start = 0,
        .df_len = 0,
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(0),
            .representation = .{ .local_declaration = @enumFromInt(0) },
        },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .nominal = .{
            .backing_expr = @enumFromInt(2),
            .backing_type = .value,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(5), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(2), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const literal = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (literal.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 5), value.value),
        else => return error.TestUnexpectedResult,
    }
    const copy = out.lir_result.store.getCFStmt(literal.next).assign_ref;
    try std.testing.expectEqual(literal.target, copy.op.local);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = copy.target } }, out.lir_result.store.getCFStmt(copy.next));
}

test "boxy lowerer emits builtin Bool tags by checked Bool names" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const true_tag = try artifact.canonical_names.internTagLabel("True");

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.bool, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .zero_argument_tag = .{
            .closure_name = true_tag,
            .name = true_tag,
        } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(1), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const tag = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), tag.variant_index);
    try std.testing.expectEqual(@as(u16, 1), tag.discriminant);
    try std.testing.expect(tag.payload == null);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = tag.target } }, out.lir_result.store.getCFStmt(tag.next));
}

test "boxy lowerer emits payload tag construction using planned variant payload layout" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const tag_a = try artifact.canonical_names.internTagLabel("A");
    const tag_b = try artifact.canonical_names.internTagLabel("B");

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_a, .args_start = 0, .args_len = 2 });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_b, .args_start = 2, .args_len = 0 });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_tag_union);
    try artifact.checked_types.payloads.append(gpa, .{
        .tag_union = .{ .tags = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(1) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(2),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .tag = .{
            .name = tag_a,
            .args = .{ .start = 0, .len = 2 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(3), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(4), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(3), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(3),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    const payload = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const tag = out.lir_result.store.getCFStmt(payload.next).assign_tag;
    switch (first.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 3), value.value),
        else => return error.TestUnexpectedResult,
    }
    switch (second.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 4), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(first.target, out.lir_result.store.getLocalSpan(payload.fields)[0]);
    try std.testing.expectEqual(second.target, out.lir_result.store.getLocalSpan(payload.fields)[1]);
    try std.testing.expectEqual(@as(u16, 0), tag.variant_index);
    try std.testing.expectEqual(@as(u16, 0), tag.discriminant);
    try std.testing.expectEqual(payload.target, tag.payload.?);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = tag.target } }, out.lir_result.store.getCFStmt(tag.next));
}

test "boxy lowerer emits list construction with committed element layout" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.list, @enumFromInt(1), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .list = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(8), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(9), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(2), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    const list = out.lir_result.store.getCFStmt(second.next).assign_list;
    switch (first.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 8), value.value),
        else => return error.TestUnexpectedResult,
    }
    switch (second.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 9), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(first.target, out.lir_result.store.getLocalSpan(list.elems)[0]);
    try std.testing.expectEqual(second.target, out.lir_result.store.getLocalSpan(list.elems)[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = list.target } }, out.lir_result.store.getCFStmt(list.next));
}

test "boxy lowerer emits empty list construction" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.list, @enumFromInt(1), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .empty_list,
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(2), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const list = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_list;
    try std.testing.expect(list.elems.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = list.target } }, out.lir_result.store.getCFStmt(list.next));
}

test "boxy lowerer publishes host wrapper proc for exported roots" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(7), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(1), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 11,
        .module_idx = 0,
        .kind = .provided_export,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .exported,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .checked_types = artifact.checked_types.view(),
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.root_procs.items.len);
    try std.testing.expectEqual(@as(usize, 2), out.lir_result.store.proc_specs.items.len);
    const wrapper_id = out.lir_result.root_procs.items[0];
    const wrapper = out.lir_result.store.getProcSpec(wrapper_id);
    const call = out.lir_result.store.getCFStmt(wrapper.body orelse return error.TestUnexpectedResult).assign_call;
    try std.testing.expect(call.args.isEmpty());
    try std.testing.expect(call.proc != wrapper_id);
    try std.testing.expectEqual(@as(@TypeOf(wrapper.ret_layout), .u64), wrapper.ret_layout);
    try std.testing.expectEqual(@as(u32, 11), out.lir_result.root_metadata.items[0].order);
    try std.testing.expectEqual(@as(lir_core.RootMetadata.RootExposure, .exported), out.lir_result.root_metadata.items[0].exposure);
}

test "boxy lowerer emits requested layout metadata for layout-only plans" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);

    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(0), .key = typeKey(1) });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });

    var plan = try Plan.analyzeProgram(gpa, .{
        .checked_types = artifact.checked_types.view(),
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(0))},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{ .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(0))} },
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.requested_layouts.items.len);
    const requested = out.lir_result.requested_layouts.items[0];
    try std.testing.expectEqual(typeKey(1), requested.ty);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), requested.checked_type);
    try std.testing.expectEqual(.u64, requested.layout_idx);
    try std.testing.expectEqual(LirProgram.ConstPlan.scalar, out.lir_result.const_plans.items[@intFromEnum(requested.plan)]);
    try std.testing.expectEqual(@as(usize, 0), out.lir_result.root_procs.items.len);
}

test "boxy lowerer emits const plans for zero-payload tag variants" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);

    const tag_a = try artifact.canonical_names.internTagLabel("A");
    const tag_b = try artifact.canonical_names.internTagLabel("B");

    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(0), .key = typeKey(0) });
    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(1), .key = typeKey(1) });
    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(2), .key = typeKey(2) });
    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_a, .args_start = 0, .args_len = 0 });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_b, .args_start = 0, .args_len = 1 });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_tag_union);
    try artifact.checked_types.payloads.append(gpa, .{
        .tag_union = .{ .tags = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(1) },
    });

    var plan = try Plan.analyzeProgram(gpa, .{
        .checked_types = artifact.checked_types.view(),
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(2))},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{ .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(2))} },
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.requested_layouts.items.len);
    const requested = out.lir_result.requested_layouts.items[0];
    const const_plan = out.lir_result.const_plans.items[@intFromEnum(requested.plan)];
    switch (const_plan) {
        .tag_union => |variants| {
            try std.testing.expectEqual(@as(usize, 2), variants.len);
            try std.testing.expectEqualStrings("A", variants[0].name);
            try std.testing.expectEqual(tag_a, variants[0].checked_name);
            try std.testing.expectEqual(@as(u16, 0), variants[0].discriminant);
            try std.testing.expectEqual(@as(usize, 0), variants[0].payloads.len);
            try std.testing.expectEqualStrings("B", variants[1].name);
            try std.testing.expectEqual(tag_b, variants[1].checked_name);
            try std.testing.expectEqual(@as(u16, 1), variants[1].discriminant);
            try std.testing.expectEqual(@as(usize, 1), variants[1].payloads.len);
        },
        else => return error.TestUnexpectedResult,
    }
}

fn minimalCheckedArtifact(allocator: Allocator) checked.CheckedModuleArtifact {
    return .{
        .key = moduleKey(1),
        .canonical_names = names.CanonicalNameStore.init(allocator),
        .module_identity = undefined,
        .checking_context_identity = undefined,
        .module_env = undefined,
        .exports = undefined,
        .provides_requires = undefined,
        .method_registry = undefined,
        .static_dispatch_plans = undefined,
        .resolved_value_refs = undefined,
        .checked_procedure_templates = undefined,
        .top_level_procedure_bindings = undefined,
        .root_requests = undefined,
        .hosted_procs = undefined,
        .platform_required_declarations = undefined,
        .platform_required_bindings = undefined,
        .interface_capabilities = .{},
        .compile_time_roots = undefined,
        .top_level_values = undefined,
        .hoisted_constants = undefined,
        .const_templates = undefined,
        .const_store = undefined,
    };
}

fn builtinNominal(
    builtin: checked.CheckedBuiltinNominal,
    backing: checked.CheckedTypeId,
    args: checked.CheckedTypeRange,
) checked.StoredNominal {
    return .{
        .name = @enumFromInt(0),
        .origin_module = @enumFromInt(0),
        .builtin = builtin,
        .is_opaque = false,
        .backing = backing,
        .representation = .{ .builtin = builtin },
        .args = args,
    };
}

fn moduleKey(byte: u8) checked.ModuleId {
    var key = checked.ModuleId{};
    key.bytes[0] = byte;
    return key;
}

fn typeKey(byte: u8) names.TypeDigest {
    var key = names.TypeDigest{};
    key.bytes[0] = byte;
    return key;
}

fn typeSchemeKey(byte: u8) names.CanonicalTypeSchemeKey {
    var key = names.CanonicalTypeSchemeKey{};
    key.bytes[0] = byte;
    return key;
}

fn intValue(value: i128) can.CIR.IntValue {
    return .{
        .bytes = @bitCast(value),
        .kind = .i128,
    };
}

fn procedureTemplateRef(key: checked.CheckedModuleArtifactKey, raw_template_id: u32) names.ProcedureTemplateRef {
    return .{
        .artifact = .{ .bytes = key.bytes },
        .proc_base = @enumFromInt(raw_template_id),
        .template = @enumFromInt(raw_template_id),
    };
}

fn procedureValueRef(template: names.ProcedureTemplateRef) names.ProcedureValueRef {
    return .{
        .artifact = template.artifact,
        .proc_base = template.proc_base,
    };
}

fn checkedTemplate(
    template_ref: names.ProcedureTemplateRef,
    checked_fn_root: checked.CheckedTypeId,
    body: checked.CheckedBodyId,
) checked.CheckedProcedureTemplate {
    return .{
        .proc_base = template_ref.proc_base,
        .template_id = template_ref.template,
        .body = .{ .checked_body = body },
        .checked_fn_scheme = typeSchemeKey(9),
        .checked_fn_root = checked_fn_root,
        .static_dispatch_plans = .{},
        .resolved_value_refs = .{},
        .top_level_value_uses = .{},
        .nested_proc_sites = .{},
        .target = .roc,
    };
}

fn dummyRootRequest() checked.RootRequest {
    return .{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(0),
        .abi = .roc,
        .exposure = .private,
    };
}

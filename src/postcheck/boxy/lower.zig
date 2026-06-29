//! Boxy checked-to-LIR lowerer.
//!
//! This lowerer consumes checked modules plus an explicit `Plan.ProgramPlan` and
//! produces ownership-neutral LIR. It is the only `.boxy` producer of LIR.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
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
const static_dispatch = check.StaticDispatchRegistry;

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
    list_in_place_map: bool = false,
    proc_debug_names: bool = false,
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

    var procedure_builder = ProcedureBuilder.init(allocator, modules, plan, &layout_plan, &resolved_workers, &result, options);
    defer procedure_builder.deinit();
    try procedure_builder.initHostedCatalog();
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
    resolved_value_refs: *const checked.ResolvedValueRefTable,
    compile_time_roots: *const checked.CompileTimeRootTable,
    entry_wrappers: *const checked.EntryWrapperTable,
    intrinsic_wrappers: *const checked.IntrinsicWrapperTable,
    hosted_procs: *const checked.HostedProcTable,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    checked_procedure_templates: *const checked.CheckedProcedureTemplateTable,
    nested_proc_sites: *const checked.NestedProcSiteTable,
    top_level_procedure_bindings: *const checked.TopLevelProcedureBindingTable,
    callable_eval_templates: checked.CallableEvalTemplateTableView,
    exported_procedure_bindings: checked.ExportedProcedureBindingView,
    const_templates: *const checked.ConstTemplateTable,
    const_store: *const check.ConstStore.ConstStore,
};

const ResolvedWorker = struct {
    worker: Plan.WorkerPlanId,
    module_key: checked.CheckedModuleArtifactKey,
    module: ProcedureModuleView,
    template_ref: names.ProcedureTemplateRef,
    template: checked.CheckedProcedureTemplate,
    body: ResolvedWorkerBody,
};

const ResolvedWorkerBody = union(enum) {
    checked_expr: struct {
        body_id: ?checked.CheckedBodyId,
        root_expr: checked.CheckedExprId,
    },
    intrinsic: checked.IntrinsicId,
    hosted: checked.HostedProc,
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
        .procedure_binding => |binding| resolveProcedureBinding(
            modules,
            worker.id,
            procedureModuleByKey(modules, binding.artifact),
            binding.binding,
        ),
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
        .imported => |imported| resolveImportedProcedureBinding(
            modules,
            worker,
            procedureModuleByKey(modules, imported.artifact),
            imported,
        ),
        .hosted => |hosted| resolveHostedProcedure(modules, worker, hosted),
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
        .callable_eval_template => |template| resolveCallableEvalTemplate(modules, worker, module, template),
    };
}

fn resolveImportedProcedureBinding(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    module: ProcedureModuleView,
    binding_ref: checked.ImportedProcedureBindingRef,
) ResolvedWorker {
    const binding = importedProcedureBinding(module, binding_ref);
    return switch (binding.body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template| resolveProcedureTemplate(modules, worker, template),
            .lifted,
            .synthetic,
            => boxyLowerInvariant("non-checked imported procedure template reached boxy worker resolution"),
        },
        .callable_eval_template => |template| resolveCallableEvalTemplate(modules, worker, module, template),
    };
}

fn importedProcedureBinding(
    module: ProcedureModuleView,
    binding_ref: checked.ImportedProcedureBindingRef,
) checked.ImportedProcedureBindingView {
    for (module.exported_procedure_bindings.bindings) |binding| {
        if (artifactKeyEqual(binding.binding.artifact, binding_ref.artifact) and
            binding.binding.def == binding_ref.def and
            binding.binding.pattern == binding_ref.pattern)
        {
            return binding;
        }
    }
    boxyLowerInvariant("imported procedure binding was not exported by its checked module");
}

fn resolveProcedureTemplate(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    template_ref: names.ProcedureTemplateRef,
) ResolvedWorker {
    const module = procedureModuleByArtifactRef(modules, template_ref.artifact);
    const template = module.checked_procedure_templates.get(template_ref.template);
    const body: ResolvedWorkerBody = if (template.target == .hosted)
        .{ .hosted = hostedProcForTemplate(module, template_ref) }
    else switch (template.body) {
        .checked_body => |body_id| .{ .checked_expr = .{
            .body_id = body_id,
            .root_expr = module.checked_bodies.body(body_id).root_expr,
        } },
        .intrinsic_wrapper => |wrapper_id| .{ .intrinsic = module.intrinsic_wrappers.get(wrapper_id).intrinsic },
        .entry_wrapper => |wrapper_id| .{ .checked_expr = .{
            .body_id = null,
            .root_expr = module.entry_wrappers.get(wrapper_id).body_expr,
        } },
    };

    return .{
        .worker = worker,
        .module_key = module.key,
        .module = module,
        .template_ref = template_ref,
        .template = template,
        .body = body,
    };
}

fn resolveHostedProcedure(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    hosted_ref: checked.HostedProcRef,
) ResolvedWorker {
    const module = procedureModuleByKey(modules, checked.hostedProcedureTemplateModuleId(hosted_ref));
    const template = module.checked_procedure_templates.get(hosted_ref.template.template);
    if (template.target != .hosted) {
        boxyLowerInvariant("hosted procedure ref pointed at a non-hosted checked template");
    }
    return .{
        .worker = worker,
        .module_key = module.key,
        .module = module,
        .template_ref = hosted_ref.template,
        .template = template,
        .body = .{ .hosted = hostedProcForTemplate(module, hosted_ref.template) },
    };
}

fn resolveCallableEvalTemplate(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    module: ProcedureModuleView,
    template_id: checked.CallableEvalTemplateId,
) ResolvedWorker {
    const raw = @intFromEnum(template_id);
    if (raw >= module.callable_eval_templates.templates.len) {
        boxyLowerInvariant("callable eval binding referenced a missing checked template");
    }
    const callable_template = module.callable_eval_templates.templates[raw];
    const root = module.compile_time_roots.root(callable_template.root);
    return switch (root.payload) {
        .fn_value => |fn_id| resolveConstFnValue(modules, worker, module, fn_id),
        .pending => boxyLowerInvariant("pending callable eval root reached runtime boxy worker resolution before compile-time finalization"),
        .const_node,
        .expect,
        => boxyLowerInvariant("callable eval binding root did not output a callable value"),
    };
}

fn resolveConstFnValue(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    store_module: ProcedureModuleView,
    fn_id: checked.ConstFnId,
) ResolvedWorker {
    const raw = @intFromEnum(fn_id);
    if (raw >= store_module.const_store.fns.items.len) {
        boxyLowerInvariant("callable eval function value referenced a missing ConstStore function");
    }
    const fn_value = store_module.const_store.getFn(fn_id);
    if (fn_value.captures.len != 0) {
        boxyLowerInvariant("capturing stored function reached runtime boxy worker resolution before const capture lowering");
    }
    return switch (fn_value.fn_def) {
        .local_template,
        .imported_template,
        .checked_generated,
        => |template| resolveProcedureTemplate(modules, worker, template),
        .nested => |nested| resolveNestedConstFn(modules, worker, nested),
        .local_hosted,
        .imported_hosted,
        => |template| resolveProcedureTemplate(modules, worker, template),
        .parser_runtime,
        .encode_to_runtime,
        => boxyLowerInvariant("generated parser/encoder stored function reached runtime boxy worker resolution before generated runtime support"),
    };
}

fn resolveNestedConstFn(
    modules: Common.CheckedModules,
    worker: Plan.WorkerPlanId,
    nested: anytype,
) ResolvedWorker {
    const module = procedureModuleByKey(modules, .{ .bytes = names.procTemplateModuleDigest(nested.owner).bytes });
    const expr_id = checkedLambdaExprForNestedFn(module, nested);
    const template = module.checked_procedure_templates.get(nested.owner.template);
    return .{
        .worker = worker,
        .module_key = module.key,
        .module = module,
        .template_ref = nested.owner,
        .template = template,
        .body = .{ .checked_expr = .{
            .body_id = null,
            .root_expr = expr_id,
        } },
    };
}

fn checkedLambdaExprForNestedFn(
    module: ProcedureModuleView,
    nested: anytype,
) checked.CheckedExprId {
    for (module.nested_proc_sites.sites) |site| {
        if (site.site != nested.site) continue;
        if (!names.procedureTemplateRefEql(site.owner_template, nested.owner)) continue;
        const expr_id = site.checked_expr orelse
            boxyLowerInvariant("stored nested function had no checked expression site");
        const expr = module.checked_bodies.expr(expr_id);
        return switch (expr.data) {
            .lambda => expr_id,
            .closure => |closure| closure.lambda,
            else => boxyLowerInvariant("stored nested function site did not point at a lambda or closure"),
        };
    }
    boxyLowerInvariant("stored nested function referenced a missing checked nested site");
}

fn hostedProcForTemplate(module: ProcedureModuleView, template_ref: names.ProcedureTemplateRef) checked.HostedProc {
    for (module.hosted_procs.procs) |hosted| {
        if (names.procedureTemplateRefEql(hosted.template, template_ref)) {
            return hosted;
        }
    }
    boxyLowerInvariant("hosted procedure template was missing from the checked hosted proc table");
}

fn rootProcedureModule(modules: Common.CheckedModules) ProcedureModuleView {
    const artifact = modules.root.module;
    return .{
        .key = artifact.key,
        .canonical_names = &artifact.canonical_names,
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .resolved_value_refs = &artifact.resolved_value_refs,
        .compile_time_roots = &artifact.compile_time_roots,
        .entry_wrappers = &artifact.entry_wrappers,
        .intrinsic_wrappers = &artifact.intrinsic_wrappers,
        .hosted_procs = &artifact.hosted_procs,
        .static_dispatch_plans = &artifact.static_dispatch_plans,
        .checked_procedure_templates = &artifact.checked_procedure_templates,
        .nested_proc_sites = &artifact.nested_proc_sites,
        .top_level_procedure_bindings = &artifact.top_level_procedure_bindings,
        .callable_eval_templates = artifact.callable_eval_templates.view(),
        .exported_procedure_bindings = artifact.exported_procedure_bindings.view(),
        .const_templates = &artifact.const_templates,
        .const_store = &artifact.const_store,
    };
}

fn procedureModuleFromImport(import: checked.ImportedModuleView) ProcedureModuleView {
    return .{
        .key = import.key,
        .canonical_names = import.canonical_names,
        .checked_types = import.checked_types,
        .checked_bodies = import.checked_bodies,
        .resolved_value_refs = import.resolved_value_refs,
        .compile_time_roots = import.compile_time_roots,
        .entry_wrappers = import.entry_wrappers,
        .intrinsic_wrappers = import.intrinsic_wrappers,
        .hosted_procs = import.hosted_procs,
        .static_dispatch_plans = import.static_dispatch_plans,
        .checked_procedure_templates = import.checked_procedure_templates,
        .nested_proc_sites = import.nested_proc_sites,
        .top_level_procedure_bindings = import.top_level_procedure_bindings,
        .callable_eval_templates = import.callable_eval_templates,
        .exported_procedure_bindings = import.exported_procedure_bindings,
        .const_templates = import.const_templates,
        .const_store = import.const_store,
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

const HostedCatalogEntry = struct {
    template: names.ProcedureTemplateRef,
    symbol_text: []const u8,
    dispatch_index: u32,
    order: []const u8,
    def_idx: u32,
};

const HostedSectionMap = struct {
    keys: []const []const u8,
    symbols: []const []const u8,

    fn deinit(self: HostedSectionMap, allocator: Allocator) void {
        for (self.keys) |key| allocator.free(key);
        allocator.free(self.keys);
        allocator.free(self.symbols);
    }
};

const ProcedureBuilder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    plan: *const Plan.ProgramPlan,
    layout_plan: *const Layouts.LayoutPlan,
    resolved_workers: *const ResolvedWorkers,
    result: *LirProgram.Result,
    options: Options,
    worker_procs: []?LIR.LirProcSpecId,
    hosted_catalog: []HostedCatalogEntry = &.{},
    symbols: Common.SymbolGen = .{},

    fn init(
        allocator: Allocator,
        modules: Common.CheckedModules,
        plan: *const Plan.ProgramPlan,
        layout_plan: *const Layouts.LayoutPlan,
        resolved_workers: *const ResolvedWorkers,
        result: *LirProgram.Result,
        options: Options,
    ) ProcedureBuilder {
        return .{
            .allocator = allocator,
            .modules = modules,
            .plan = plan,
            .layout_plan = layout_plan,
            .resolved_workers = resolved_workers,
            .result = result,
            .options = options,
            .worker_procs = &.{},
        };
    }

    fn deinit(self: *ProcedureBuilder) void {
        self.allocator.free(self.hosted_catalog);
        self.allocator.free(self.worker_procs);
        self.* = undefined;
    }

    fn initHostedCatalog(self: *ProcedureBuilder) Allocator.Error!void {
        if (self.resolved_workers.items.len == 0) return;

        var entries = std.ArrayList(HostedCatalogEntry).empty;
        errdefer entries.deinit(self.allocator);

        try self.appendHostedCatalogFromModule(&entries, rootProcedureModule(self.modules));
        for (self.modules.imports, 0..) |imported, index| {
            if (self.importModuleAlreadyScanned(imported.key, index)) continue;
            try self.appendHostedCatalogFromModule(&entries, procedureModuleFromImport(imported));
        }
        for (self.modules.root.relation_modules, 0..) |relation, index| {
            if (self.relationModuleAlreadyScanned(relation.key, index)) continue;
            try self.appendHostedCatalogFromModule(&entries, procedureModuleFromImport(relation));
        }

        const SortContext = struct {
            pub fn lessThan(_: void, a: HostedCatalogEntry, b: HostedCatalogEntry) bool {
                return switch (std.mem.order(u8, a.order, b.order)) {
                    .lt => true,
                    .gt => false,
                    .eq => a.def_idx < b.def_idx,
                };
            }
        };
        std.mem.sort(HostedCatalogEntry, entries.items, {}, SortContext.lessThan);

        for (entries.items, 0..) |*entry, index| {
            entry.dispatch_index = @intCast(index);
        }

        if (entries.items.len != 0) {
            if (try self.buildHostedSectionMap()) |map| {
                defer map.deinit(self.allocator);

                if (entries.items.len != map.keys.len) {
                    boxyLowerInvariant("platform hosted section disagrees with the hosted catalog size");
                }
                for (entries.items) |*entry| {
                    const pos = blk: {
                        for (map.keys, 0..) |key, key_index| {
                            if (std.mem.eql(u8, key, entry.order)) break :blk key_index;
                        }
                        boxyLowerInvariant("hosted function is missing from the platform hosted section");
                    };
                    entry.dispatch_index = @intCast(pos);
                    entry.symbol_text = map.symbols[pos];
                }

                const DispatchSort = struct {
                    pub fn lessThan(_: void, a: HostedCatalogEntry, b: HostedCatalogEntry) bool {
                        return a.dispatch_index < b.dispatch_index;
                    }
                };
                std.mem.sort(HostedCatalogEntry, entries.items, {}, DispatchSort.lessThan);
            }
        }

        self.hosted_catalog = try entries.toOwnedSlice(self.allocator);
    }

    fn appendHostedCatalogFromModule(
        self: *ProcedureBuilder,
        entries: *std.ArrayList(HostedCatalogEntry),
        module: ProcedureModuleView,
    ) Allocator.Error!void {
        for (module.hosted_procs.procs) |hosted| {
            try entries.append(self.allocator, .{
                .template = hosted.template,
                .symbol_text = module.canonical_names.externalSymbolNameText(hosted.external_symbol_name),
                .dispatch_index = 0,
                .order = hosted.orderKey(module.hosted_procs),
                .def_idx = @intFromEnum(hosted.def_idx),
            });
        }
    }

    fn buildHostedSectionMap(self: *ProcedureBuilder) Allocator.Error!?HostedSectionMap {
        const platform_env = blk: {
            const root_env = self.modules.root.module.moduleEnvConst();
            if (root_env.hosted_entries.items.items.len != 0) break :blk root_env;
            for (self.modules.imports) |imported| {
                const env = imported.module_env;
                if (env.hosted_entries.items.items.len != 0) break :blk env;
            }
            for (self.modules.root.relation_modules) |relation| {
                const env = relation.module_env;
                if (env.hosted_entries.items.items.len != 0) break :blk env;
            }
            return null;
        };

        const section = platform_env.hosted_entries.items.items;
        var keys = try self.allocator.alloc([]const u8, section.len);
        var key_count: usize = 0;
        errdefer {
            for (keys[0..key_count]) |key| self.allocator.free(key);
            self.allocator.free(keys);
        }
        const symbols = try self.allocator.alloc([]const u8, section.len);
        errdefer self.allocator.free(symbols);

        for (section, 0..) |entry, index| {
            var func_text = platform_env.getIdentText(entry.func_ident);
            if (func_text.len > 0 and func_text[func_text.len - 1] == '!') {
                func_text = func_text[0 .. func_text.len - 1];
            }
            keys[index] = if (entry.module_ident) |module_ident|
                try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ platform_env.getIdentText(module_ident), func_text })
            else
                try self.allocator.dupe(u8, func_text);
            key_count = index + 1;
            symbols[index] = platform_env.getString(entry.symbol);
        }

        return .{ .keys = keys, .symbols = symbols };
    }

    fn importModuleAlreadyScanned(self: *ProcedureBuilder, module_key: checked.CheckedModuleArtifactKey, import_index: usize) bool {
        if (artifactKeyEqual(module_key, self.modules.root.module.key)) return true;
        for (self.modules.imports[0..import_index]) |imported| {
            if (artifactKeyEqual(module_key, imported.key)) return true;
        }
        return false;
    }

    fn relationModuleAlreadyScanned(self: *ProcedureBuilder, module_key: checked.CheckedModuleArtifactKey, relation_index: usize) bool {
        if (artifactKeyEqual(module_key, self.modules.root.module.key)) return true;
        for (self.modules.imports) |imported| {
            if (artifactKeyEqual(module_key, imported.key)) return true;
        }
        for (self.modules.root.relation_modules[0..relation_index]) |relation| {
            if (artifactKeyEqual(module_key, relation.key)) return true;
        }
        return false;
    }

    fn lirHostedProcForTemplate(self: *ProcedureBuilder, template_ref: names.ProcedureTemplateRef) Allocator.Error!LIR.HostedProc {
        for (self.hosted_catalog) |entry| {
            if (!names.procedureTemplateRefEql(entry.template, template_ref)) continue;
            return .{
                .symbol = try self.result.store.insertString(entry.symbol_text),
                .dispatch_index = entry.dispatch_index,
            };
        }
        boxyLowerInvariant("hosted procedure template was not output in the hosted catalog");
    }

    fn emitRoots(self: *ProcedureBuilder) Allocator.Error!void {
        self.worker_procs = try self.allocator.alloc(?LIR.LirProcSpecId, self.resolved_workers.items.len);
        @memset(self.worker_procs, null);

        for (self.plan.roots.items, self.layout_plan.roots.items) |root, root_layout| {
            if (root.id != root_layout.root) boxyLowerInvariant("boxy root layout table disagreed with root plan order");
            if (root.worker != root_layout.worker) boxyLowerInvariant("boxy root layout table disagreed with root worker plan");
            const worker_layout = self.layout_plan.workerLayoutFor(root.worker);
            const worker_proc = try self.emitWorker(root.worker);
            const root_proc = switch (root.wrapper_kind) {
                .private_worker_only => worker_proc,
                .host_shaped_wrapper => try self.emitHostWrapper(root_layout, worker_layout, worker_proc),
            };
            if (self.options.proc_debug_names and root.wrapper_kind == .host_shaped_wrapper) {
                try self.result.store.copyProcDebugInfo(root_proc, worker_proc);
            }
            try self.result.root_procs.append(self.allocator, root_proc);
            try self.result.root_metadata.append(self.allocator, RootMetadata.fromCheckedRoot(root.request));
        }
    }

    fn emitWorker(
        self: *ProcedureBuilder,
        worker_id: Plan.WorkerPlanId,
    ) Allocator.Error!LIR.LirProcSpecId {
        const index = @intFromEnum(worker_id);
        if (index >= self.worker_procs.len) boxyLowerInvariant("boxy root referenced a missing worker proc");
        if (self.worker_procs[index]) |existing| return existing;

        const resolved = self.resolved_workers.items[index];
        switch (resolved.body) {
            .hosted => return try self.emitHostedWorker(worker_id, resolved),
            .checked_expr,
            .intrinsic,
            => {},
        }

        var proc = ProcBodyBuilder.init(self, resolved.module, self.layout_plan.workerLayoutFor(worker_id));
        defer proc.deinit();

        const body_source = try self.bodySourceForWorker(resolved, &proc);
        const ret_layout = proc.workerReturnLayout();
        const ret_local = try proc.addFrameLocal(ret_layout);
        const args_span = try self.result.store.addLocalSpan(proc.arg_locals.items);
        const proc_id = try self.result.store.addProcSpec(.{
            .name = lirSymbol(self.symbols.fresh()),
            .args = args_span,
            .body = null,
            .ret_layout = ret_layout,
            .stack_probe = self.stackProbeForProc(args_span, LIR.LocalSpan.empty(), ret_layout),
        });
        self.worker_procs[index] = proc_id;
        try self.setProcDebugName(proc_id, resolved);

        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        const body_stmt = try self.lowerWorkerBodyInto(resolved, &proc, body_source, ret_local, ret_stmt);
        const frame_span = try self.result.store.addLocalSpan(proc.frame_locals.items);
        const proc_spec = self.result.store.getProcSpecPtr(proc_id);
        proc_spec.frame_locals = frame_span;
        proc_spec.body = body_stmt;
        proc_spec.stack_probe = self.stackProbeForProc(args_span, frame_span, ret_layout);
        return proc_id;
    }

    fn emitHostedWorker(
        self: *ProcedureBuilder,
        worker_id: Plan.WorkerPlanId,
        resolved: ResolvedWorker,
    ) Allocator.Error!LIR.LirProcSpecId {
        const index = @intFromEnum(worker_id);
        if (index >= self.worker_procs.len) boxyLowerInvariant("boxy hosted worker referenced a missing worker proc");

        var proc = ProcBodyBuilder.init(self, resolved.module, self.layout_plan.workerLayoutFor(worker_id));
        defer proc.deinit();

        const function = checkedFunctionPayload(resolved.module, resolved.template.checked_fn_root);
        const worker_args = self.layout_plan.workerLayoutSlice(proc.worker_layout.args);
        if (function.args.len != worker_args.len) {
            boxyLowerInvariant("hosted procedure worker arity disagreed with worker root layout");
        }
        for (function.args, worker_args) |arg_ty, arg_layout| {
            if (proc.workerRuntimeLayoutForType(arg_ty).layoutIdx() != arg_layout.layoutIdx()) {
                boxyLowerInvariant("hosted procedure argument layout disagreed with checked type");
            }
            _ = try proc.addArgLocal(arg_layout.layoutIdx());
        }

        const ret_layout = proc.workerReturnLayout();
        if (proc.workerRuntimeLayoutForType(function.ret).layoutIdx() != ret_layout) {
            boxyLowerInvariant("hosted procedure return layout disagreed with checked type");
        }

        const args_span = try self.result.store.addLocalSpan(proc.arg_locals.items);
        const proc_id = try self.result.store.addProcSpec(.{
            .name = lirSymbol(self.symbols.fresh()),
            .args = args_span,
            .body = null,
            .ret_layout = ret_layout,
            .hosted = try self.lirHostedProcForTemplate(resolved.template_ref),
            .stack_probe = self.stackProbeForProc(args_span, LIR.LocalSpan.empty(), ret_layout),
        });
        self.worker_procs[index] = proc_id;
        try self.setProcDebugName(proc_id, resolved);
        return proc_id;
    }

    fn setProcDebugName(
        self: *ProcedureBuilder,
        proc_id: LIR.LirProcSpecId,
        resolved: ResolvedWorker,
    ) Allocator.Error!void {
        if (!self.options.proc_debug_names) return;
        const proc_base = resolved.module.canonical_names.procBase(resolved.template.proc_base);
        const export_name = proc_base.export_name orelse return;
        try self.result.store.setProcDebugName(proc_id, resolved.module.canonical_names.exportNameText(export_name));
    }

    const WorkerBodySource = union(enum) {
        checked_expr: checked.CheckedExprId,
        str_inspect: struct {
            arg: LIR.LocalId,
            arg_ty: checked.CheckedTypeId,
        },
    };

    fn bodySourceForWorker(
        self: *ProcedureBuilder,
        resolved: ResolvedWorker,
        proc: *ProcBodyBuilder,
    ) Allocator.Error!WorkerBodySource {
        return switch (resolved.body) {
            .checked_expr => |body| blk: {
                const root_expr = resolved.module.checked_bodies.expr(body.root_expr);
                break :blk switch (root_expr.data) {
                    .lambda => |lambda| lambda_blk: {
                        const worker_args = self.layout_plan.workerLayoutSlice(proc.worker_layout.args);
                        if (lambda.args.len != worker_args.len) {
                            boxyLowerInvariant("boxy worker lambda arity disagreed with worker root layout");
                        }
                        proc.current_lambda = body.root_expr;
                        try proc.bindLambdaArgs(lambda.args);
                        break :lambda_blk .{ .checked_expr = lambda.body };
                    },
                    else => .{ .checked_expr = body.root_expr },
                };
            },
            .intrinsic => |intrinsic| try self.bodySourceForIntrinsic(resolved, proc, intrinsic),
            .hosted => boxyLowerInvariant("hosted procedure worker reached body lowering after hosted LIR proc emission"),
        };
    }

    fn bodySourceForIntrinsic(
        self: *ProcedureBuilder,
        resolved: ResolvedWorker,
        proc: *ProcBodyBuilder,
        intrinsic: checked.IntrinsicId,
    ) Allocator.Error!WorkerBodySource {
        const function = checkedFunctionPayload(resolved.module, resolved.template.checked_fn_root);
        return switch (intrinsic) {
            .str_inspect => {
                if (function.args.len != 1) {
                    boxyLowerInvariant("Str.inspect intrinsic wrapper had an unexpected arity");
                }
                const worker_args = self.layout_plan.workerLayoutSlice(proc.worker_layout.args);
                if (worker_args.len != 1) {
                    boxyLowerInvariant("Str.inspect intrinsic worker layout had an unexpected arity");
                }
                if (proc.workerRuntimeLayoutForType(function.args[0]).layoutIdx() != worker_args[0].layoutIdx()) {
                    boxyLowerInvariant("Str.inspect intrinsic argument layout disagreed with checked type");
                }
                if (proc.workerRuntimeLayoutForType(function.ret).layoutIdx() != proc.workerReturnLayout()) {
                    boxyLowerInvariant("Str.inspect intrinsic return layout disagreed with checked type");
                }
                return .{ .str_inspect = .{
                    .arg = try proc.addArgLocal(worker_args[0].layoutIdx()),
                    .arg_ty = function.args[0],
                } };
            },
            .structural_eq => boxyLowerInvariant("structural equality intrinsic wrapper must lower through checked dispatch plans"),
        };
    }

    fn lowerWorkerBodyInto(
        self: *ProcedureBuilder,
        resolved: ResolvedWorker,
        proc: *ProcBodyBuilder,
        body_source: WorkerBodySource,
        ret_local: LIR.LocalId,
        ret_stmt: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        _ = self;
        _ = resolved;
        return switch (body_source) {
            .checked_expr => |body_expr| try proc.lowerExprInto(ret_local, body_expr, ret_stmt),
            .str_inspect => |inspect| try proc.lowerInspectLocalInto(ret_local, inspect.arg, inspect.arg_ty, ret_stmt),
        };
    }

    fn emitHostWrapper(
        self: *ProcedureBuilder,
        root_layout: Layouts.RootLayouts,
        worker_layout: Layouts.WorkerLayouts,
        worker_proc: LIR.LirProcSpecId,
    ) Allocator.Error!LIR.LirProcSpecId {
        const host_args = self.layout_plan.rootLayoutSlice(root_layout.host_args);
        const worker_args = self.layout_plan.workerLayoutSlice(worker_layout.args);
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
        const worker_ret = worker_layout.ret orelse worker_layout.value;
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
    worker_layout: Layouts.WorkerLayouts,
    arg_locals: std.ArrayList(LIR.LocalId),
    frame_locals: std.ArrayList(LIR.LocalId),
    loop_stack: std.ArrayList(LoopContext),
    binder_locals: []?LIR.LocalId,
    next_join_point: u32,
    current_lambda: ?checked.CheckedExprId,

    const LoopContext = struct {
        join_id: LIR.JoinPointId,
        result_target: LIR.LocalId,
        after_loop: LIR.CFStmtId,
    };

    const PatternMiss = struct {
        join_id: LIR.JoinPointId,
    };

    const InspectPart = union(enum) {
        literal: []const u8,
        field: struct {
            source: LIR.LocalId,
            field_index: u16,
            rep: Plan.TypeRepId,
        },
        tag_payload: struct {
            source: LIR.LocalId,
            variant_index: u16,
            payload_index: ?u16,
            rep: Plan.TypeRepId,
        },
    };

    const LiteralPattern = union(enum) {
        int: can.CIR.IntValue,
        dec: builtins.dec.RocDec,
        frac_f32: f32,
        frac_f64: f64,
        str: checked.CheckedStringLiteralId,
    };

    fn init(parent: *ProcedureBuilder, module: ProcedureModuleView, worker_layout: Layouts.WorkerLayouts) ProcBodyBuilder {
        return .{
            .parent = parent,
            .module = module,
            .worker_layout = worker_layout,
            .arg_locals = .empty,
            .frame_locals = .empty,
            .loop_stack = .empty,
            .binder_locals = &.{},
            .next_join_point = 0,
            .current_lambda = null,
        };
    }

    fn deinit(self: *ProcBodyBuilder) void {
        self.parent.allocator.free(self.binder_locals);
        self.loop_stack.deinit(self.parent.allocator);
        self.frame_locals.deinit(self.parent.allocator);
        self.arg_locals.deinit(self.parent.allocator);
        self.* = undefined;
    }

    fn bindLambdaArgs(self: *ProcBodyBuilder, args: []const checked.CheckedPatternId) Allocator.Error!void {
        try self.ensureBinderLocals();

        const worker_args = self.parent.layout_plan.workerLayoutSlice(self.worker_layout.args);
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
            .dec => |dec| try self.assignDecLiteral(target, dec.value, next),
            .dec_small => |dec| try self.assignDecLiteral(target, dec.value.toRocDec(), next),
            .str_segment => |literal| try self.assignStringLiteral(target, literal, next),
            .bytes_literal => |literal| try self.assignStringLiteral(target, literal, next),
            .str => |segments| try self.lowerStrInto(target, segments, next),
            .str_from_quote => |quote| blk: {
                if (quote.plan != null) {
                    boxyLowerInvariant("from_quote conversion reached boxy string literal lowering before static-dispatch call lowering");
                }
                break :blk try self.assignStringLiteral(target, quote.literal, next);
            },
            .empty_record => try self.assignZst(target, next),
            .empty_list => try self.assignList(target, &.{}, next),
            .lookup_local => |lookup| try self.lowerLookupLocalInto(target, expr.ty, lookup, next),
            .lookup_external => |ref_id| try self.lowerResolvedLookupInto(target, expr.ty, ref_id, next),
            .lookup_required => |ref_id| try self.lowerResolvedLookupInto(target, expr.ty, ref_id, next),
            .field_access => |access| try self.lowerFieldAccessInto(target, access.receiver, access.field_name, next),
            .tuple_access => |access| try self.lowerTupleAccessInto(target, access.tuple, access.elem_index, next),
            .list => |items| try self.lowerListInto(target, items, next),
            .tuple => |items| try self.lowerTupleInto(target, items, next),
            .tag => |tag| try self.lowerTagInto(target, expr.ty, tag.name, tag.args, next),
            .zero_argument_tag => |tag| try self.lowerTagInto(target, expr.ty, tag.name, &.{}, next),
            .if_ => |if_| try self.lowerIfInto(target, if_.branches, if_.final_else, next),
            .match_ => |match_| try self.lowerMatchInto(target, match_.cond, match_.branches, next),
            .unary_minus => |child| try self.lowerUnaryLowLevelInto(target, .num_negate, child, next),
            .unary_not => |child| try self.lowerUnaryLowLevelInto(target, .bool_not, child, next),
            .binop => |binop| try self.lowerBoolBinopInto(target, binop.op, binop.lhs, binop.rhs, next),
            .structural_eq => |eq| try self.lowerStructuralEqInto(target, eq.lhs, eq.rhs, eq.negated, next),
            .method_eq => |plan| try self.lowerMethodEqInto(target, plan, next),
            .structural_hash => |hash| try self.lowerStructuralHashInto(target, hash.value, hash.hasher, next),
            .record => |record| try self.lowerRecordExprInto(target, expr.ty, record, next),
            .nominal => |nominal| try self.lowerNominalInto(target, nominal.backing_expr, next),
            .call => |call| try self.lowerDirectCallInto(target, expr_id, call, next),
            .run_low_level => |run_low_level| try self.lowerLowLevelInto(target, run_low_level.op, run_low_level.args, next),
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
            .dbg => |child| try self.lowerDbgExprInto(target, child, next),
            .expect => |child| try self.lowerExpectExprInto(target, child, next),
            .expect_err => |expect_err| try self.lowerExpectErrInto(expect_err.expr, expect_err.snippet),
            .crash => |msg| try self.parent.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.parent.result.store.insertString(self.module.checked_bodies.stringLiteral(msg)),
            } }),
            .ellipsis => try self.parent.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.parent.result.store.insertString("not implemented"),
            } }),
            .break_ => try self.lowerBreak(),
            .return_ => |ret| try self.lowerReturn(ret.expr, ret.lambda),
            .runtime_error => try self.parent.result.store.addCFStmt(.runtime_error),
            .lambda => boxyLowerInvariant("nested lambda reached boxy expression lowering before erased callable lowering was emitted"),
            else => boxyLowerInvariant("checked expression form reached boxy body lowering before its LIR lowering was implemented"),
        };
    }

    fn lowerLookupLocalInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        checked_ty: checked.CheckedTypeId,
        lookup: anytype,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (lookup.resolved) |ref_id| {
            return try self.lowerResolvedLookupInto(target, checked_ty, ref_id, next);
        }
        return try self.assignLocal(target, self.localForPattern(lookup.pattern), next);
    }

    fn lowerResolvedLookupInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        checked_ty: checked.CheckedTypeId,
        maybe_ref: ?checked.ResolvedValueRefId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const ref_id = maybe_ref orelse
            boxyLowerInvariant("checked lookup reached boxy lowering without a resolved value ref");
        const record = self.resolvedValueRecord(ref_id);
        return switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            => |local| try self.assignLocal(target, self.localForBinder(local.binder), next),
            .selected_hoisted_const => |selected| blk: {
                if (self.binderLocalOrNull(selected.local.binder)) |local| {
                    break :blk try self.assignLocal(target, local, next);
                }
                break :blk try self.restoreConstUseInto(target, checked_ty, selected.const_use, next);
            },
            .top_level_const,
            .imported_const,
            => |const_use| try self.restoreConstUseInto(target, checked_ty, const_use, next),
            .platform_required_const => |required| try self.restoreConstUseInto(target, checked_ty, required.const_use, next),
            .local_proc => boxyLowerInvariant("local procedure value reached boxy lookup lowering before erased callable construction"),
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_proc,
            .promoted_top_level_proc,
            => boxyLowerInvariant("procedure value reached boxy lookup lowering before erased callable construction"),
            .platform_required_declaration => boxyLowerInvariant("unbound platform-required declaration reached boxy lookup lowering"),
        };
    }

    fn lowerMethodEqInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const plan = self.staticDispatchPlan(maybe_plan);
        const eq = switch (plan.result_mode) {
            .equality => |eq| eq,
            else => boxyLowerInvariant("checked method equality used a non-equality dispatch plan"),
        };
        if (!eq.structural_allowed) {
            boxyLowerInvariant("checked method equality reached boxy lowering without structural equality permission");
        }
        switch (plan.resolution) {
            .unresolved_checked_plan => {},
            .resolved_target => {
                if (!checkedTypeUsesBuiltinStructuralEquality(self.module, plan.dispatcher_ty)) {
                    boxyLowerInvariant("resolved method equality target reached boxy lowering before dispatch worker calls");
                }
            },
        }

        const operands = plan.argsSlice(self.module.static_dispatch_plans);
        if (operands.len != 2) {
            boxyLowerInvariant("checked method equality dispatch plan did not carry two operands");
        }
        const lhs = switch (operands[0]) {
            .checked_expr => |expr| expr,
            else => boxyLowerInvariant("checked method equality lhs was not a checked expression operand"),
        };
        const rhs = switch (operands[1]) {
            .checked_expr => |expr| expr,
            else => boxyLowerInvariant("checked method equality rhs was not a checked expression operand"),
        };
        return try self.lowerStructuralEqInto(target, lhs, rhs, eq.negated, next);
    }

    fn staticDispatchPlan(
        self: *const ProcBodyBuilder,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) static_dispatch.StaticDispatchCallPlan {
        const plan_id = maybe_plan orelse
            boxyLowerInvariant("checked dispatch expression reached boxy lowering without a dispatch plan");
        const raw = @intFromEnum(plan_id);
        if (raw >= self.module.static_dispatch_plans.plans.len) {
            boxyLowerInvariant("checked dispatch expression referenced a missing dispatch plan");
        }
        return self.module.static_dispatch_plans.plans[raw];
    }

    fn restoreConstUseInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        checked_ty: checked.CheckedTypeId,
        const_use: checked.ConstUseTemplate,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const requested_ty = const_use.requested_source_ty_payload orelse
            boxyLowerInvariant("checked const use reached boxy lowering without a requested checked type");
        if (self.workerRuntimeLayoutForType(requested_ty).layoutIdx() != self.parent.result.store.getLocal(target).layout_idx) {
            boxyLowerInvariant("checked const use target layout disagreed with requested checked type");
        }
        if (self.workerRuntimeLayoutForType(checked_ty).layoutIdx() != self.parent.result.store.getLocal(target).layout_idx) {
            boxyLowerInvariant("checked const lookup expression type disagreed with target layout");
        }

        const store_module = procedureModuleByKey(self.parent.modules, checked.constModuleId(const_use.const_ref));
        const template = store_module.const_templates.get(const_use.const_ref);
        return switch (template.state) {
            .stored_const => |stored| try self.restoreConstNodeInto(target, store_module, self.module, stored.node, requested_ty, next),
            .reserved => boxyLowerInvariant("reserved checked const template reached runtime boxy lowering"),
            .eval_template => boxyLowerInvariant("const eval template reached runtime boxy lowering before compile-time finalization stored its value"),
        };
    }

    fn restoreConstNodeInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        node: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (@intFromEnum(node) >= store_module.const_store.values.items.len) {
            boxyLowerInvariant("ConstStore node id was outside the store");
        }
        return switch (store_module.const_store.get(node)) {
            .pending => boxyLowerInvariant("pending ConstStore node reached runtime boxy lowering"),
            .zst => try self.assignZst(target, next),
            .scalar => |scalar| try self.assignConstScalar(target, scalar, next),
            .str => |str| try self.assignStringBytesView(target, store_module.const_store.strData(str.data), str.offset, str.len, next),
            .crash => |str| try self.parent.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.parent.result.store.insertString(store_module.const_store.strBytes(str)),
            } }),
            .list => |items| try self.restoreConstListInto(target, store_module, type_module, items, checked_ty, next),
            .box => |payload| try self.restoreConstBoxInto(target, store_module, type_module, payload, checked_ty, next),
            .tuple => |items| try self.restoreConstTupleInto(target, store_module, type_module, items, checked_ty, next),
            .record => |items| try self.restoreConstRecordInto(target, store_module, type_module, items, checked_ty, next),
            .tag => |tag| try self.restoreConstTagInto(target, store_module, type_module, tag, checked_ty, next),
            .nominal => |nominal| try self.restoreConstNominalInto(target, store_module, type_module, nominal.backing, checked_ty, next),
            .fn_value => boxyLowerInvariant("ConstStore function value reached boxy const lookup before erased callable construction"),
        };
    }

    fn restoreConstListInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        items: []const checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const elem_ty = constListElemType(type_module, checked_ty);
        const elem_layout = self.localListElemLayout(target);
        const elem_locals = try self.parent.allocator.alloc(LIR.LocalId, items.len);
        defer self.parent.allocator.free(elem_locals);

        for (elem_locals) |*local| {
            if (self.workerRuntimeLayoutForType(elem_ty).layoutIdx() != elem_layout) {
                boxyLowerInvariant("ConstStore list element layout required box/adapt lowering before list construction");
            }
            local.* = try self.addFrameLocal(elem_layout);
        }

        var continuation = try self.assignList(target, elem_locals, next);
        var index = items.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.restoreConstNodeInto(elem_locals[index], store_module, type_module, items[index], elem_ty, continuation);
        }
        return continuation;
    }

    fn restoreConstBoxInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        payload: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const payload_ty = constBoxPayloadType(type_module, checked_ty);
        const payload_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(payload_ty).layoutIdx());
        const boxed = try self.assignBoxBoundary(target, payload_local, .box_box, next);
        return try self.restoreConstNodeInto(payload_local, store_module, type_module, payload, payload_ty, boxed);
    }

    fn restoreConstTupleInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        items: []const checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const item_tys = constTupleItemTypes(type_module, checked_ty);
        if (item_tys.len != items.len) {
            boxyLowerInvariant("ConstStore tuple length differed from checked tuple type");
        }
        const field_locals = try self.parent.allocator.alloc(LIR.LocalId, items.len);
        defer self.parent.allocator.free(field_locals);
        for (item_tys, field_locals) |item_ty, *local| {
            local.* = try self.addFrameLocal(self.workerRuntimeLayoutForType(item_ty).layoutIdx());
        }

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.parent.result.store.addLocalSpan(field_locals),
            .next = next,
        } });
        var index = items.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.restoreConstNodeInto(field_locals[index], store_module, type_module, items[index], item_tys[index], continuation);
        }
        return continuation;
    }

    fn restoreConstRecordInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        items: []const checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const fields = constRecordFields(type_module, checked_ty);
        if (fields.len != items.len) {
            boxyLowerInvariant("ConstStore record field count differed from checked record type");
        }

        const rep_id = self.repForType(checked_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        switch (rep.kind) {
            .record,
            .record_unbound,
            => {},
            else => boxyLowerInvariant("ConstStore record restored with a non-record boxy representation"),
        }

        const children = self.parent.plan.childSlice(rep.children);
        const field_locals = try self.parent.allocator.alloc(LIR.LocalId, fields.len);
        defer self.parent.allocator.free(field_locals);
        const source_field_locals = try self.parent.allocator.alloc(?LIR.LocalId, fields.len);
        defer self.parent.allocator.free(source_field_locals);
        @memset(source_field_locals, null);

        var layout_index: usize = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => |label| {
                    const source_index = constRecordFieldIndex(fields, label) orelse
                        boxyLowerInvariant("ConstStore record representation referenced a field outside checked type");
                    const local = try self.addFrameLocal(self.parent.layout_plan.rep_layouts[@intFromEnum(child.rep)].worker.layoutIdx());
                    field_locals[layout_index] = local;
                    source_field_locals[source_index] = local;
                    layout_index += 1;
                },
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("ConstStore record representation had a non-record child role"),
            }
        }
        if (layout_index != fields.len) {
            boxyLowerInvariant("ConstStore record representation field count differed from checked type");
        }
        for (source_field_locals) |maybe_local| {
            if (maybe_local == null) {
                boxyLowerInvariant("ConstStore record field was not selected by representation order");
            }
        }

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.parent.result.store.addLocalSpan(field_locals),
            .next = next,
        } });
        var source_index = items.len;
        while (source_index > 0) {
            source_index -= 1;
            continuation = try self.restoreConstNodeInto(source_field_locals[source_index].?, store_module, type_module, items[source_index], fields[source_index].ty, continuation);
        }
        return continuation;
    }

    fn restoreConstTagInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        tag: anytype,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep_id = self.repForType(checked_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .bool_tag_union => try self.restoreConstBoolTagInto(target, tag, next),
            .tag_union => try self.restoreConstPlannedTagInto(target, store_module, type_module, rep, tag, checked_ty, next),
            .empty_tag_union => boxyLowerInvariant("ConstStore tag value reached empty tag-union representation"),
            else => boxyLowerInvariant("ConstStore tag restored with a non-tag-union representation"),
        };
    }

    fn restoreConstBoolTagInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        tag: anytype,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (tag.payloads.len != 0) {
            boxyLowerInvariant("ConstStore Bool tag carried a payload");
        }
        if (std.mem.eql(u8, tag.tag_name, "False")) return try self.assignBoolLiteral(target, false, next);
        if (std.mem.eql(u8, tag.tag_name, "True")) return try self.assignBoolLiteral(target, true, next);
        boxyLowerInvariant("ConstStore Bool tag had a non-Bool name");
    }

    fn restoreConstPlannedTagInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        rep: Plan.TypeRepresentation,
        tag: anytype,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const checked_tag = constTagPayloadTypes(type_module, checked_ty, tag.tag_name);
        const name = checked_tag.name;
        const payload_tys = checked_tag.payload_tys;
        if (payload_tys.len != tag.payloads.len) {
            boxyLowerInvariant("ConstStore tag payload count differed from checked tag type");
        }

        const variant = self.tagVariant(rep, name);
        const payload_children = self.parent.plan.childSlice(variant.payloads);
        if (payload_children.len != tag.payloads.len) {
            boxyLowerInvariant("ConstStore tag payload count disagreed with its boxy representation");
        }
        for (payload_children, 0..) |child, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != name or payload.index != index) {
                        boxyLowerInvariant("ConstStore tag variant payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("ConstStore tag variant payload span included a non-payload child"),
            }
        }

        if (tag.payloads.len == 0) {
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
            boxyLowerInvariant("zero-sized ConstStore tag-union layout had a non-zero-sized payload");
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

        if (tag.payloads.len == 1) {
            return try self.restoreConstNodeInto(payload_local, store_module, type_module, tag.payloads[0], payload_tys[0], assign_tag);
        }

        const field_locals = try self.parent.allocator.alloc(LIR.LocalId, tag.payloads.len);
        defer self.parent.allocator.free(field_locals);
        for (payload_children, field_locals) |child, *local| {
            local.* = try self.addFrameLocal(self.parent.layout_plan.rep_layouts[@intFromEnum(child.rep)].worker.layoutIdx());
        }

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = payload_local,
            .fields = try self.parent.result.store.addLocalSpan(field_locals),
            .next = assign_tag,
        } });
        var index = tag.payloads.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.restoreConstNodeInto(field_locals[index], store_module, type_module, tag.payloads[index], payload_tys[index], continuation);
        }
        return continuation;
    }

    fn restoreConstNominalInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        store_module: ProcedureModuleView,
        type_module: ProcedureModuleView,
        backing: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing_ty = constNominalBackingType(type_module, checked_ty);
        const backing_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(backing_ty).layoutIdx());
        const assign = try self.assignNominalBoundary(target, backing_local, next);
        return try self.restoreConstNodeInto(backing_local, store_module, type_module, backing, backing_ty, assign);
    }

    fn lowerDirectCallInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        call_expr: checked.CheckedExprId,
        call: anytype,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (call.direct_target == null) {
            boxyLowerInvariant("indirect checked call reached boxy lowering before erased callable call support");
        }
        const worker_id = self.parent.plan.directWorkerForCall(.{ .module = self.module.key, .expr = call_expr }) orelse
            boxyLowerInvariant("checked direct call reached boxy lowering without a planned worker");
        const worker_layout = self.parent.layout_plan.workerLayoutFor(worker_id);
        const worker_args = self.parent.layout_plan.workerLayoutSlice(worker_layout.args);
        if (call.args.len != worker_args.len) {
            boxyLowerInvariant("boxy direct call needed hidden arguments before descriptor or dictionary lowering");
        }

        for (call.args, worker_args) |arg, arg_layout| {
            const arg_expr = self.module.checked_bodies.expr(arg);
            if (self.workerRuntimeLayoutForType(arg_expr.ty).layoutIdx() != arg_layout.layoutIdx()) {
                boxyLowerInvariant("boxy direct call needed argument adaptation before adapter lowering");
            }
        }

        const ret_layout = if (worker_layout.ret) |ret| ret else worker_layout.value;
        if (self.parent.result.store.getLocal(target).layout_idx != ret_layout.layoutIdx()) {
            boxyLowerInvariant("boxy direct call needed return adaptation before adapter lowering");
        }

        const lowered = try self.lowerExprsToTemps(call.args);
        defer self.parent.allocator.free(lowered);

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = try self.parent.emitWorker(worker_id),
            .args = try self.parent.result.store.addLocalSpan(lowered),
            .next = next,
        } });
        continuation = try self.prependLoweredExprs(call.args, lowered, continuation);
        return continuation;
    }

    fn lowerIfInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        branches: []const checked.CheckedIfBranch,
        final_else: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const done = self.freshJoinPointId();
        var current = try self.lowerExprInto(target, final_else, try self.joinJump(done));
        var index = branches.len;
        while (index > 0) {
            index -= 1;
            const branch = branches[index];
            const body = try self.lowerExprInto(target, branch.body, try self.joinJump(done));
            const cond_expr = self.module.checked_bodies.expr(branch.cond);
            const cond_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(cond_expr.ty).layoutIdx());
            const switch_stmt = try self.boolSwitchNoContinuation(cond_local, body, current);
            current = try self.lowerExprInto(cond_local, branch.cond, switch_stmt);
        }
        const params = [_]LIR.LocalId{target};
        return try self.parent.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.parent.result.store.addLocalSpan(&params),
            .body = next,
            .remainder = current,
        } });
    }

    fn lowerMatchInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        cond: checked.CheckedExprId,
        branches: []const checked.CheckedMatchBranch,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        try self.reserveMatchBranchRepresentativeBindings(branches);

        const cond_expr = self.module.checked_bodies.expr(cond);
        const cond_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(cond_expr.ty).layoutIdx());
        const done = self.freshJoinPointId();

        var current = try self.parent.result.store.addCFStmt(.runtime_error);
        var index = branches.len;
        while (index > 0) {
            index -= 1;
            current = try self.lowerMatchBranchInto(branches[index], cond_local, target, done, current);
        }

        const remainder = try self.lowerExprInto(cond_local, cond, current);
        const params = [_]LIR.LocalId{target};
        return try self.parent.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.parent.result.store.addLocalSpan(&params),
            .body = next,
            .remainder = remainder,
        } });
    }

    fn reserveMatchBranchRepresentativeBindings(
        self: *ProcBodyBuilder,
        branches: []const checked.CheckedMatchBranch,
    ) Allocator.Error!void {
        for (branches) |branch| {
            const patterns = branch.patternsSlice(self.module.checked_bodies);
            if (patterns.len == 0) {
                boxyLowerInvariant("checked match branch reached boxy lowering without a representative pattern");
            }
            if (patterns[0].degenerate) {
                boxyLowerInvariant("checked match branch representative pattern was marked degenerate");
            }
            try self.reserveMatchPatternBindings(patterns[0].pattern);
        }
    }

    fn lowerMatchBranchInto(
        self: *ProcBodyBuilder,
        branch: checked.CheckedMatchBranch,
        source: LIR.LocalId,
        target: LIR.LocalId,
        done: LIR.JoinPointId,
        next_branch: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const patterns = branch.patternsSlice(self.module.checked_bodies);
        if (patterns.len == 0) {
            boxyLowerInvariant("checked match branch reached boxy lowering without patterns");
        }

        const branch_done = try self.joinJump(done);
        const branch_body = try self.lowerExprInto(target, branch.value, branch_done);
        const on_match = if (branch.guard) |guard| blk: {
            const guard_expr = self.module.checked_bodies.expr(guard);
            const guard_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(guard_expr.ty).layoutIdx());
            const guard_switch = try self.boolSwitchNoContinuation(guard_local, branch_body, next_branch);
            break :blk try self.lowerExprInto(guard_local, guard, guard_switch);
        } else branch_body;

        var current = next_branch;
        var index = patterns.len;
        while (index > 0) {
            index -= 1;
            const branch_pattern = patterns[index];
            if (branch_pattern.degenerate) {
                boxyLowerInvariant("degenerate checked match alternative reached boxy lowering");
            }
            const remaps = branch_pattern.binderRemapsSlice(self.module.checked_bodies);
            const needs_miss_join = self.patternCanMiss(branch_pattern.pattern);
            const miss = if (needs_miss_join)
                PatternMiss{ .join_id = self.freshJoinPointId() }
            else
                null;
            const branch_start = try self.lowerPatternThen(branch_pattern.pattern, source, on_match, miss, remaps);
            current = if (miss) |miss_info|
                try self.parent.result.store.addCFStmt(.{ .join = .{
                    .id = miss_info.join_id,
                    .params = LIR.LocalSpan.empty(),
                    .body = current,
                    .remainder = branch_start,
                } })
            else
                branch_start;
        }
        return current;
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
        const assign = try self.assignNominalBoundary(target, backing_local, next);
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

    fn lowerStrInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        segments: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (segments.len == 0) return try self.assignStringBytesLiteral(target, "", next);
        if (segments.len == 1) return try self.lowerExprInto(target, segments[0], next);

        const str_layout = self.parent.result.store.getLocal(target).layout_idx;

        const segment_locals = try self.parent.allocator.alloc(LIR.LocalId, segments.len);
        defer self.parent.allocator.free(segment_locals);
        for (segments, segment_locals) |segment, *local| {
            const expr = self.module.checked_bodies.expr(segment);
            const layout_idx = self.workerRuntimeLayoutForType(expr.ty).layoutIdx();
            if (layout_idx != str_layout) {
                boxyLowerInvariant("checked string segment required explicit box/adapt lowering before string concatenation");
            }
            local.* = try self.addFrameLocal(layout_idx);
        }

        const concat_results = try self.parent.allocator.alloc(LIR.LocalId, segments.len - 1);
        defer self.parent.allocator.free(concat_results);
        for (concat_results[0 .. concat_results.len - 1]) |*local| {
            local.* = try self.addFrameLocal(str_layout);
        }
        concat_results[concat_results.len - 1] = target;

        var continuation = next;
        var concat_index = concat_results.len;
        while (concat_index > 0) {
            concat_index -= 1;
            const lhs = if (concat_index == 0) segment_locals[0] else concat_results[concat_index - 1];
            const rhs = segment_locals[concat_index + 1];
            const args = [_]LIR.LocalId{ lhs, rhs };
            continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = concat_results[concat_index],
                .op = .str_concat,
                .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(&args),
                .next = continuation,
            } });
            continuation = try self.lowerExprInto(rhs, segments[concat_index + 1], continuation);
            if (concat_index == 0) {
                continuation = try self.lowerExprInto(lhs, segments[0], continuation);
            }
        }
        return continuation;
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

    fn lowerRecordExprInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        record_ty: checked.CheckedTypeId,
        record: anytype,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = try self.lowerRecordInto(target, record_ty, record.fields, next);
        if (record.ext) |ext| {
            const ext_expr = self.module.checked_bodies.expr(ext);
            const ext_rep = self.repForType(ext_expr.ty);
            self.requireEmptyRecordExtension(ext_rep);
            const ext_layout = self.parent.layout_plan.rep_layouts[@intFromEnum(ext_rep)].worker.layoutIdx();
            const ext_local = try self.addFrameLocal(ext_layout);
            continuation = try self.lowerExprInto(ext_local, ext, continuation);
        }
        return continuation;
    }

    fn reserveBlockBindings(self: *ProcBodyBuilder, statements: []const checked.CheckedStatementId) Allocator.Error!void {
        for (statements) |statement_id| {
            const statement = self.module.checked_bodies.statement(statement_id);
            switch (statement.data) {
                .decl => |decl| try self.reservePatternBindings(decl.pattern),
                .var_ => |decl| try self.reservePatternBindings(decl.pattern),
                .var_uninitialized => |decl| try self.reservePatternBindings(decl.pattern),
                .reassign => |reassign| try self.reserveReassignPatternBindings(reassign.pattern),
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

    fn lowerReassignPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        expr_id: checked.CheckedExprId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const source = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.bindReassignPatternFromLocal(pattern_id, source, reassigned_binders, next);
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
            .list => |list| try self.bindIrrefutableListPattern(list, source, next),
            .underscore => next,
            .applied_tag,
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

    fn lowerPatternThen(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign => |binder| try self.bindMatchBinder(binder, source, remaps, on_match),
            .as => |as| blk: {
                const bound_outer = try self.bindMatchBinder(as.binder, source, remaps, on_match);
                break :blk try self.lowerPatternThen(as.pattern, source, bound_outer, miss, remaps);
            },
            .tuple => |items| try self.lowerTuplePatternThen(items, source, on_match, miss, remaps),
            .record_destructure => |destructs| try self.lowerRecordPatternThen(pattern.ty, destructs, source, on_match, miss, remaps),
            .nominal => |nominal| try self.lowerNominalPatternThen(nominal.backing_pattern, source, on_match, miss, remaps),
            .applied_tag => |tag| try self.lowerAppliedTagPatternThen(pattern.ty, tag.name, tag.args, source, on_match, miss, remaps),
            .num_literal => |literal| blk: {
                if (literal.conversion != null) {
                    boxyLowerInvariant("non-builtin numeral pattern reached boxy match lowering before dictionary conversion lowering");
                }
                break :blk try self.lowerLiteralPatternThen(pattern.ty, source, .{ .int = literal.value }, on_match, miss);
            },
            .small_dec_literal => |literal| blk: {
                if (literal.conversion != null) {
                    boxyLowerInvariant("non-builtin small decimal pattern reached boxy match lowering before dictionary conversion lowering");
                }
                break :blk try self.lowerLiteralPatternThen(pattern.ty, source, .{ .dec = literal.value.toRocDec() }, on_match, miss);
            },
            .dec_literal => |literal| blk: {
                if (literal.conversion != null) {
                    boxyLowerInvariant("non-builtin decimal pattern reached boxy match lowering before dictionary conversion lowering");
                }
                break :blk try self.lowerLiteralPatternThen(pattern.ty, source, .{ .dec = literal.value }, on_match, miss);
            },
            .frac_f32_literal => |value| try self.lowerLiteralPatternThen(pattern.ty, source, .{ .frac_f32 = value }, on_match, miss),
            .frac_f64_literal => |value| try self.lowerLiteralPatternThen(pattern.ty, source, .{ .frac_f64 = value }, on_match, miss),
            .str_literal => |literal| blk: {
                if (literal.conversion != null) {
                    boxyLowerInvariant("non-builtin string pattern reached boxy match lowering before dictionary conversion lowering");
                }
                break :blk try self.lowerLiteralPatternThen(pattern.ty, source, .{ .str = literal.literal }, on_match, miss);
            },
            .underscore => on_match,
            .list => |list| try self.lowerListPatternThen(list, source, on_match, miss, remaps),
            .str_interpolation => |str| try self.lowerStrPatternThen(str, source, on_match, miss),
            .runtime_error,
            .pending,
            => boxyLowerInvariant("runtime-error or pending checked pattern reached boxy match lowering"),
        };
    }

    fn lowerTuplePatternThen(
        self: *ProcBodyBuilder,
        items: []const checked.CheckedPatternId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = on_match;
        var index = items.len;
        while (index > 0) {
            index -= 1;
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tuple match pattern index exceeded LIR field index range");
            }
            continuation = try self.lowerFieldPatternThen(items[index], source, @intCast(index), continuation, miss, remaps);
        }
        return continuation;
    }

    fn lowerRecordPatternThen(
        self: *ProcBodyBuilder,
        record_ty: checked.CheckedTypeId,
        destructs: []const checked.CheckedRecordDestruct,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = on_match;
        var index = destructs.len;
        while (index > 0) {
            index -= 1;
            const destruct = destructs[index];
            switch (destruct.kind) {
                .required,
                .sub_pattern,
                => |child_pattern| {
                    continuation = try self.lowerFieldPatternThen(
                        child_pattern,
                        source,
                        self.recordFieldLayoutIndex(record_ty, destruct.label),
                        continuation,
                        miss,
                        remaps,
                    );
                },
                .rest => |child_pattern| {
                    if (!self.patternIsIgnored(child_pattern)) {
                        continuation = try self.lowerRecordRestPatternThen(
                            child_pattern,
                            source,
                            record_ty,
                            continuation,
                            miss,
                            remaps,
                        );
                    }
                },
            }
        }
        return continuation;
    }

    fn lowerNominalPatternThen(
        self: *ProcBodyBuilder,
        backing_pattern: checked.CheckedPatternId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        const backing = self.module.checked_bodies.pattern(backing_pattern);
        const backing_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(backing.ty).layoutIdx());
        const matched = try self.lowerPatternThen(backing_pattern, backing_local, on_match, miss, remaps);
        return try self.assignNominalBoundary(backing_local, source, matched);
    }

    fn lowerListPatternThen(
        self: *ProcBodyBuilder,
        list: anytype,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        const elems = list.patterns;
        const fixed_count: i64 = @intCast(elems.len);

        if (elems.len == 0) {
            if (list.rest) |rest| {
                if (rest.pattern) |rest_pattern| {
                    return try self.lowerPatternThen(rest_pattern, source, on_match, miss, remaps);
                }
                return on_match;
            }
        }

        const len_local = try self.addFrameLocal(.u64);
        var current = on_match;

        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                if (!self.patternIsIgnored(rest_pattern)) {
                    if (elems.len == 0) {
                        current = try self.lowerPatternThen(rest_pattern, source, current, miss, remaps);
                    } else {
                        const source_layout = self.parent.result.store.getLocal(source).layout_idx;
                        const rest_local = try self.addFrameLocal(source_layout);
                        current = try self.lowerPatternThen(rest_pattern, rest_local, current, miss, remaps);
                        const front_dropped = try self.addFrameLocal(source_layout);
                        const keep_len = try self.addFrameLocal(.u64);
                        current = try self.assignBinaryLowLevel(rest_local, .list_take_first, front_dropped, keep_len, current);
                        current = try self.lenMinusConst(keep_len, len_local, fixed_count, current);
                        const keep_after_front = try self.addFrameLocal(.u64);
                        current = try self.assignBinaryLowLevel(front_dropped, .list_take_last, source, keep_after_front, current);
                        current = try self.lenMinusConst(keep_after_front, len_local, @intCast(rest.index), current);
                    }
                }
            }
        }

        const rest_index: ?u32 = if (list.rest) |rest| rest.index else null;
        var index = elems.len;
        while (index > 0) {
            index -= 1;
            const elem_pattern = elems[index];
            if (self.patternIsIgnored(elem_pattern)) continue;

            const pattern = self.module.checked_bodies.pattern(elem_pattern);
            const elem_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
            current = try self.lowerPatternThen(elem_pattern, elem_local, current, miss, remaps);

            const index_local = try self.addFrameLocal(.u64);
            current = try self.assignBinaryLowLevel(elem_local, .list_get_unsafe, source, index_local, current);
            const matches_from_back = if (rest_index) |ri| index >= ri else false;
            if (matches_from_back) {
                current = try self.lenMinusConst(index_local, len_local, fixed_count - @as(i64, @intCast(index)), current);
            } else {
                current = try self.assignU64Literal(index_local, @intCast(index), current);
            }
        }

        const required = try self.addFrameLocal(.u64);
        const cond = try self.addFrameLocal(.bool);
        const cmp_op: LIR.LowLevel = if (list.rest == null) .num_is_eq else .num_is_gte;
        const length_check = try self.boolSwitchNoContinuation(cond, current, try self.patternMissJump(miss));
        var head = try self.assignBinaryLowLevel(cond, cmp_op, len_local, required, length_check);
        head = try self.assignU64Literal(required, fixed_count, head);
        head = try self.assignUnaryLowLevel(len_local, .list_len, source, head);
        return head;
    }

    fn lowerStrPatternThen(
        self: *ProcBodyBuilder,
        str: anytype,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
    ) Allocator.Error!LIR.CFStmtId {
        const arm = try self.lowerStrPatternArm(str, on_match);
        return try self.parent.result.store.addCFStmt(.{ .str_match = .{
            .source = source,
            .prefix = arm.prefix,
            .steps = arm.steps,
            .end = arm.end,
            .on_match = arm.on_match,
            .on_miss = try self.patternMissJump(miss),
        } });
    }

    fn lowerStrPatternArm(
        self: *ProcBodyBuilder,
        str: anytype,
        on_match: LIR.CFStmtId,
    ) Allocator.Error!LIR.StrMatchArm {
        const lir_steps = try self.parent.allocator.alloc(LIR.StrMatchStep, str.steps.len);
        defer self.parent.allocator.free(lir_steps);

        for (str.steps, lir_steps) |input_step, *lir_step| {
            lir_step.* = .{
                .capture = if (input_step.capture) |capture| blk: {
                    const pattern = self.module.checked_bodies.pattern(capture);
                    break :blk .{ .view = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx()) };
                } else .discard,
                .delimiter = try self.lirStrLiteral(input_step.delimiter),
            };
        }

        var match_body = on_match;
        var index = str.steps.len;
        while (index > 0) {
            index -= 1;
            if (str.steps[index].capture) |capture| {
                const capture_local = switch (lir_steps[index].capture) {
                    .view => |local| local,
                    .discard => boxyLowerInvariant("string-pattern capture step lowered without a capture local"),
                };
                match_body = try self.bindPatternFromLocal(capture, capture_local, match_body);
            }
        }

        return .{
            .prefix = try self.lirStrLiteral(str.prefix),
            .steps = try self.parent.result.store.addStrMatchSteps(lir_steps),
            .end = switch (str.end) {
                .exact => .exact,
                .tail => .tail,
            },
            .on_match = match_body,
        };
    }

    fn lowerFieldPatternThen(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        field_index: u16,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.patternIsIgnored(pattern_id)) return on_match;
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const field_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.lowerPatternThen(pattern_id, field_local, on_match, miss, remaps);
        if (self.isZstLocal(field_local)) return try self.assignZst(field_local, bound);
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = field_local,
            .op = .{ .field = .{
                .source = source,
                .field_idx = field_index,
            } },
            .next = bound,
        } });
    }

    fn lowerAppliedTagPatternThen(
        self: *ProcBodyBuilder,
        tag_ty: checked.CheckedTypeId,
        name: names.TagNameId,
        args: []const checked.CheckedPatternId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        const rep_id = self.repForType(tag_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .bool_tag_union => blk: {
                if (args.len != 0) {
                    boxyLowerInvariant("builtin Bool match pattern carried a payload");
                }
                break :blk try self.lowerTagDiscriminantSwitch(source, self.boolVariantIndex(name), on_match, miss);
            },
            .tag_union => blk: {
                const variants = self.parent.plan.tagVariantSlice(rep.tag_variants);
                const variant = self.tagVariant(rep, name);
                try self.validateTagPatternPayloads(name, variant.payloads, args);
                const payloads_bound = try self.lowerTagPayloadPatterns(variant.index, args, source, on_match, miss, remaps);
                if (variants.len == 1) break :blk payloads_bound;
                break :blk try self.lowerTagDiscriminantSwitch(source, variant.index, payloads_bound, miss);
            },
            .empty_tag_union => boxyLowerInvariant("empty tag-union match pattern reached boxy lowering"),
            else => boxyLowerInvariant("tag match pattern checked type did not have a boxy tag-union representation"),
        };
    }

    fn validateTagPatternPayloads(
        self: *ProcBodyBuilder,
        tag_name: names.TagNameId,
        payload_span: Plan.Span,
        args: []const checked.CheckedPatternId,
    ) Allocator.Error!void {
        const payloads = self.parent.plan.childSlice(payload_span);
        if (payloads.len != args.len) {
            boxyLowerInvariant("tag match pattern payload count disagreed with its checked type representation");
        }
        for (payloads, 0..) |child, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != tag_name or payload.index != index) {
                        boxyLowerInvariant("tag match pattern payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag match pattern payload span included a non-payload child"),
            }
        }
    }

    fn lowerTagPayloadPatterns(
        self: *ProcBodyBuilder,
        variant_index: u16,
        args: []const checked.CheckedPatternId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len == 0) return on_match;

        if (args.len == 1) {
            const payload_pattern = self.module.checked_bodies.pattern(args[0]);
            const payload_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(payload_pattern.ty).layoutIdx());
            const bound = try self.lowerPatternThen(args[0], payload_local, on_match, miss, remaps);
            if (self.isZstLocal(payload_local)) return bound;
            return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                .target = payload_local,
                .op = .{ .tag_payload_struct = .{
                    .source = source,
                    .variant_index = variant_index,
                    .tag_discriminant = variant_index,
                } },
                .next = bound,
            } });
        }

        var continuation = on_match;
        var index = args.len;
        while (index > 0) {
            index -= 1;
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag match pattern payload index exceeded LIR payload index range");
            }
            const payload_pattern = self.module.checked_bodies.pattern(args[index]);
            const payload_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(payload_pattern.ty).layoutIdx());
            const bound = try self.lowerPatternThen(args[index], payload_local, continuation, miss, remaps);
            continuation = if (self.isZstLocal(payload_local))
                bound
            else
                try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = payload_local,
                    .op = .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = @intCast(index),
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = bound,
                } });
        }
        return continuation;
    }

    fn lowerTagDiscriminantSwitch(
        self: *ProcBodyBuilder,
        source: LIR.LocalId,
        variant_index: u16,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.isZstLocal(source)) {
            if (variant_index != 0) {
                boxyLowerInvariant("zero-sized tag-union pattern expected a nonzero discriminant");
            }
            return on_match;
        }

        const discriminant = try self.addFrameLocal(.u16);
        const branches = [_]LIR.CFSwitchBranch{.{ .value = variant_index, .body = on_match }};
        const switch_stmt = try self.parent.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = discriminant,
            .branches = try self.parent.result.store.addCFSwitchBranches(&branches),
            .default_branch = try self.patternMissJump(miss),
            .continuation = null,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn lowerLiteralPatternThen(
        self: *ProcBodyBuilder,
        pattern_ty: checked.CheckedTypeId,
        source: LIR.LocalId,
        literal: LiteralPattern,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
    ) Allocator.Error!LIR.CFStmtId {
        const literal_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern_ty).layoutIdx());
        const eq = try self.addFrameLocal(.bool);
        const switch_stmt = try self.boolSwitchNoContinuation(eq, on_match, try self.patternMissJump(miss));
        const compare = try self.lowerEqRepLocalsInto(eq, source, literal_local, self.repForType(pattern_ty), false, switch_stmt);
        return try self.assignLiteralPattern(literal_local, literal, compare);
    }

    fn assignLiteralPattern(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        literal: LiteralPattern,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return switch (literal) {
            .int => |value| try self.assignIntLiteral(target, value.toI128(), next),
            .dec => |value| try self.assignDecLiteral(target, value, next),
            .frac_f32 => |value| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = value },
                .next = next,
            } }),
            .frac_f64 => |value| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = value },
                .next = next,
            } }),
            .str => |literal_id| try self.assignStringLiteral(target, literal_id, next),
        };
    }

    fn patternMissJump(self: *ProcBodyBuilder, miss: ?PatternMiss) Allocator.Error!LIR.CFStmtId {
        const miss_info = miss orelse boxyLowerInvariant("refutable checked match pattern reached boxy lowering without a miss target");
        return try self.parent.result.store.addCFStmt(.{ .jump = .{ .target = miss_info.join_id } });
    }

    fn assignU64Literal(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: i64,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i64_literal = .{ .value = value, .layout_idx = .u64 } },
            .next = next,
        } });
    }

    fn assignDecLiteral(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: builtins.dec.RocDec,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .dec_literal = value.num },
            .next = next,
        } });
    }

    fn assignUnaryLowLevel(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        arg: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const args = [_]LIR.LocalId{arg};
        return try self.assignLowLevelLocals(target, op, &args, next);
    }

    fn assignBinaryLowLevel(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const args = [_]LIR.LocalId{ lhs, rhs };
        return try self.assignLowLevelLocals(target, op, &args, next);
    }

    fn assignLowLevelLocals(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(args),
            .next = next,
        } });
    }

    fn lenMinusConst(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        len_local: LIR.LocalId,
        value: i64,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const operand = try self.addFrameLocal(.u64);
        const subtract = try self.assignBinaryLowLevel(target, .num_minus, len_local, operand, next);
        return try self.assignU64Literal(operand, value, subtract);
    }

    fn bindMatchBinder(
        self: *ProcBodyBuilder,
        binder: checked.PatternBinderId,
        source: LIR.LocalId,
        remaps: []const checked.CheckedAlternativeBinderRemap,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const target = self.matchBinderLocal(binder, remaps);
        if (target == source) return next;
        if (self.parent.result.store.getLocal(target).layout_idx != self.parent.result.store.getLocal(source).layout_idx) {
            boxyLowerInvariant("checked match binder required explicit box/adapt lowering before binding");
        }
        return try self.assignLocal(target, source, next);
    }

    fn matchBinderLocal(
        self: *ProcBodyBuilder,
        binder: checked.PatternBinderId,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) LIR.LocalId {
        for (remaps) |remap| {
            if (remap.candidate_binder == binder) return self.localForBinder(remap.representative_binder);
        }
        return self.localForBinder(binder);
    }

    fn bindReassignPatternFromLocal(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign => |binder| try self.bindReassignBinder(binder, source, reassigned_binders, next),
            .as => |as| blk: {
                const inner = try self.bindReassignPatternFromLocal(as.pattern, source, reassigned_binders, next);
                break :blk try self.bindReassignBinder(as.binder, source, reassigned_binders, inner);
            },
            .tuple => |items| try self.bindReassignTuplePattern(items, source, reassigned_binders, next),
            .record_destructure => |destructs| try self.bindReassignRecordPattern(pattern.ty, destructs, source, reassigned_binders, next),
            .nominal => |nominal| try self.bindReassignNominalPattern(nominal.backing_pattern, source, reassigned_binders, next),
            .list => |list| try self.bindReassignIrrefutableListPattern(list, source, reassigned_binders, next),
            .underscore => next,
            .applied_tag,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            .runtime_error,
            .pending,
            => boxyLowerInvariant("refutable or pending checked pattern reached boxy reassign binding"),
        };
    }

    fn bindReassignBinder(
        self: *ProcBodyBuilder,
        binder: checked.PatternBinderId,
        source: LIR.LocalId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const target = self.localForBinder(binder);
        if (target == source) return next;
        if (self.binderIsReassigned(reassigned_binders, binder)) {
            return try self.setLocalReplace(target, source, next);
        }
        return try self.assignLocal(target, source, next);
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

    fn bindReassignTuplePattern(
        self: *ProcBodyBuilder,
        items: []const checked.CheckedPatternId,
        source: LIR.LocalId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = next;
        var index = items.len;
        while (index > 0) {
            index -= 1;
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tuple reassign pattern index exceeded LIR field index range");
            }
            continuation = try self.bindReassignFieldPattern(items[index], source, @intCast(index), reassigned_binders, continuation);
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
            switch (destruct.kind) {
                .required,
                .sub_pattern,
                => |child_pattern| {
                    continuation = try self.bindFieldPattern(
                        child_pattern,
                        source,
                        self.recordFieldLayoutIndex(record_ty, destruct.label),
                        continuation,
                    );
                },
                .rest => |child_pattern| {
                    if (!self.patternIsIgnored(child_pattern)) {
                        continuation = try self.bindRecordRestPattern(
                            child_pattern,
                            source,
                            record_ty,
                            continuation,
                        );
                    }
                },
            }
        }
        return continuation;
    }

    fn bindReassignRecordPattern(
        self: *ProcBodyBuilder,
        record_ty: checked.CheckedTypeId,
        destructs: []const checked.CheckedRecordDestruct,
        source: LIR.LocalId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var continuation = next;
        var index = destructs.len;
        while (index > 0) {
            index -= 1;
            const destruct = destructs[index];
            switch (destruct.kind) {
                .required,
                .sub_pattern,
                => |child_pattern| {
                    continuation = try self.bindReassignFieldPattern(
                        child_pattern,
                        source,
                        self.recordFieldLayoutIndex(record_ty, destruct.label),
                        reassigned_binders,
                        continuation,
                    );
                },
                .rest => |child_pattern| {
                    if (!self.patternIsIgnored(child_pattern)) {
                        continuation = try self.bindReassignRecordRestPattern(
                            child_pattern,
                            source,
                            record_ty,
                            reassigned_binders,
                            continuation,
                        );
                    }
                },
            }
        }
        return continuation;
    }

    fn bindIrrefutableListPattern(
        self: *ProcBodyBuilder,
        list: anytype,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (list.patterns.len != 0) {
            boxyLowerInvariant("refutable list pattern reached boxy irrefutable declaration binding");
        }
        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                return try self.bindPatternFromLocal(rest_pattern, source, next);
            }
        }
        return next;
    }

    fn bindReassignIrrefutableListPattern(
        self: *ProcBodyBuilder,
        list: anytype,
        source: LIR.LocalId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (list.patterns.len != 0) {
            boxyLowerInvariant("refutable list pattern reached boxy irrefutable reassign binding");
        }
        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                return try self.bindReassignPatternFromLocal(rest_pattern, source, reassigned_binders, next);
            }
        }
        return next;
    }

    fn lowerRecordRestPatternThen(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        source_record_ty: checked.CheckedTypeId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const rest_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.lowerPatternThen(pattern_id, rest_local, on_match, miss, remaps);
        return try self.lowerRecordRestValueInto(rest_local, source, source_record_ty, pattern.ty, bound);
    }

    fn bindRecordRestPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        source_record_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const rest_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.bindPatternFromLocal(pattern_id, rest_local, next);
        return try self.lowerRecordRestValueInto(rest_local, source, source_record_ty, pattern.ty, bound);
    }

    fn bindReassignRecordRestPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        source_record_ty: checked.CheckedTypeId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const rest_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.bindReassignPatternFromLocal(pattern_id, rest_local, reassigned_binders, next);
        return try self.lowerRecordRestValueInto(rest_local, source, source_record_ty, pattern.ty, bound);
    }

    fn lowerRecordRestValueInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        source_record_ty: checked.CheckedTypeId,
        rest_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep_id = self.repForType(rest_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        switch (rep.kind) {
            .empty_record => return try self.assignZst(target, next),
            .record,
            .record_unbound,
            => {},
            else => boxyLowerInvariant("record rest pattern child did not have a boxy record representation"),
        }

        const children = self.parent.plan.childSlice(rep.children);
        var field_count: usize = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => field_count += 1,
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("record rest representation had a non-record child role"),
            }
        }

        const field_locals = try self.parent.allocator.alloc(LIR.LocalId, field_count);
        defer self.parent.allocator.free(field_locals);
        const field_indexes = try self.parent.allocator.alloc(u16, field_count);
        defer self.parent.allocator.free(field_indexes);

        var layout_index: usize = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => |label| {
                    field_locals[layout_index] = try self.addFrameLocal(self.parent.layout_plan.rep_layouts[@intFromEnum(child.rep)].worker.layoutIdx());
                    field_indexes[layout_index] = self.recordFieldLayoutIndex(source_record_ty, label);
                    layout_index += 1;
                },
                .record_ext => {},
                else => unreachable,
            }
        }

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.parent.result.store.addLocalSpan(field_locals),
            .next = next,
        } });
        var index = field_count;
        while (index > 0) {
            index -= 1;
            continuation = if (self.isZstLocal(field_locals[index]))
                try self.assignZst(field_locals[index], continuation)
            else
                try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = field_locals[index],
                    .op = .{ .field = .{
                        .source = source,
                        .field_idx = field_indexes[index],
                    } },
                    .next = continuation,
                } });
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
        const backing_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(backing.ty).layoutIdx());
        const bound = try self.bindPatternFromLocal(backing_pattern, backing_local, next);
        return try self.assignNominalBoundary(backing_local, source, bound);
    }

    fn bindReassignNominalPattern(
        self: *ProcBodyBuilder,
        backing_pattern: checked.CheckedPatternId,
        source: LIR.LocalId,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing = self.module.checked_bodies.pattern(backing_pattern);
        const backing_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(backing.ty).layoutIdx());
        const bound = try self.bindReassignPatternFromLocal(backing_pattern, backing_local, reassigned_binders, next);
        return try self.assignNominalBoundary(backing_local, source, bound);
    }

    fn bindFieldPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        field_index: u16,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.patternIsIgnored(pattern_id)) return next;
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

    fn bindReassignFieldPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        source: LIR.LocalId,
        field_index: u16,
        reassigned_binders: []const checked.PatternBinderId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.patternIsIgnored(pattern_id)) return next;
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        const field_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(pattern.ty).layoutIdx());
        const bound = try self.bindReassignPatternFromLocal(pattern_id, field_local, reassigned_binders, next);
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

    fn lowerUninitializedPattern(
        self: *ProcBodyBuilder,
        pattern_id: checked.CheckedPatternId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign => try self.initUninitializedLocal(self.localForPattern(pattern_id), next),
            .as => |as| blk: {
                const inner = try self.lowerUninitializedPattern(as.pattern, next);
                break :blk try self.initUninitializedLocal(self.localForBinder(as.binder), inner);
            },
            .tuple => |items| blk: {
                var continuation = next;
                var index = items.len;
                while (index > 0) {
                    index -= 1;
                    continuation = try self.lowerUninitializedPattern(items[index], continuation);
                }
                break :blk continuation;
            },
            .record_destructure => |destructs| blk: {
                var continuation = next;
                var index = destructs.len;
                while (index > 0) {
                    index -= 1;
                    const child = switch (destructs[index].kind) {
                        .required,
                        .sub_pattern,
                        .rest,
                        => |child_pattern| child_pattern,
                    };
                    continuation = try self.lowerUninitializedPattern(child, continuation);
                }
                break :blk continuation;
            },
            .nominal => |nominal| try self.lowerUninitializedPattern(nominal.backing_pattern, next),
            .list => |list| blk: {
                if (list.patterns.len != 0) {
                    boxyLowerInvariant("refutable list pattern reached boxy uninitialized binding");
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| {
                        break :blk try self.lowerUninitializedPattern(rest_pattern, next);
                    }
                }
                break :blk next;
            },
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            => next,
            .applied_tag,
            .runtime_error,
            .pending,
            => boxyLowerInvariant("refutable or pending checked pattern reached boxy uninitialized binding"),
        };
    }

    fn initUninitializedLocal(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.isZstLocal(target)) return next;
        return try self.parent.result.store.addCFStmt(.{ .init_uninitialized = .{
            .target = target,
            .next = next,
        } });
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
            .var_ => |decl| try self.lowerDeclPattern(decl.pattern, decl.expr, next),
            .var_uninitialized => |decl| try self.lowerUninitializedPattern(decl.pattern, next),
            .reassign => |reassign| try self.lowerReassignPattern(reassign.pattern, reassign.expr, reassign.reassigned_binders, next),
            .expr => |expr_id| blk: {
                const expr = self.module.checked_bodies.expr(expr_id);
                const temp = try self.addFrameLocal(self.workerRuntimeLayoutForType(expr.ty).layoutIdx());
                break :blk try self.lowerExprInto(temp, expr_id, next);
            },
            .expect => |expr_id| try self.lowerExpectStmt(expr_id, next),
            .return_ => |ret| try self.lowerReturn(ret.expr, ret.lambda),
            .while_ => |while_| try self.lowerWhileStatement(while_.cond, while_.body, next),
            .infinite_loop => |loop| try self.lowerWhileStatement(loop.cond, loop.body, next),
            .breakable_loop => |loop| try self.lowerWhileStatement(loop.cond, loop.body, next),
            .break_ => try self.lowerBreak(),
            .dbg => |expr_id| blk: {
                const message = try self.addFrameLocal(.str);
                const debug_stmt = try self.parent.result.store.addCFStmt(.{ .debug = .{
                    .message = message,
                    .next = next,
                } });
                break :blk try self.lowerInspectExprInto(message, expr_id, debug_stmt);
            },
            .crash => |msg| try self.parent.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.parent.result.store.insertString(self.module.checked_bodies.stringLiteral(msg)),
            } }),
            .runtime_error => try self.parent.result.store.addCFStmt(.runtime_error),
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            => next,
            else => boxyLowerInvariant("checked statement form reached boxy body lowering before its LIR lowering was implemented"),
        };
    }

    fn lowerWhileStatement(
        self: *ProcBodyBuilder,
        cond_id: checked.CheckedExprId,
        body_id: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const cond_expr = self.module.checked_bodies.expr(cond_id);
        const cond_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(cond_expr.ty).layoutIdx());
        if (self.parent.result.store.getLocal(cond_local).layout_idx != .bool) {
            boxyLowerInvariant("checked while condition did not lower to Bool layout");
        }

        const body_expr = self.module.checked_bodies.expr(body_id);
        const loop_result = try self.addFrameLocal(self.workerRuntimeLayoutForType(body_expr.ty).layoutIdx());
        if (!self.isZstLocal(loop_result)) {
            boxyLowerInvariant("checked while body did not lower to Unit layout");
        }

        const join_id = self.freshJoinPointId();
        try self.loop_stack.append(self.parent.allocator, .{
            .join_id = join_id,
            .result_target = loop_result,
            .after_loop = next,
        });
        defer _ = self.loop_stack.pop();

        const continue_stmt = try self.parent.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const body = try self.lowerExprInto(loop_result, body_id, continue_stmt);
        const switch_stmt = try self.boolSwitchNoContinuation(cond_local, body, next);
        const header = try self.lowerExprInto(cond_local, cond_id, switch_stmt);
        const initial_jump = try self.parent.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });

        return try self.parent.result.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = LIR.LocalSpan.empty(),
            .body = header,
            .remainder = initial_jump,
        } });
    }

    fn lowerBreak(self: *ProcBodyBuilder) Allocator.Error!LIR.CFStmtId {
        const loop = self.currentLoop();
        return try self.assignZst(loop.result_target, loop.after_loop);
    }

    fn currentLoop(self: *ProcBodyBuilder) LoopContext {
        if (self.loop_stack.items.len == 0) {
            boxyLowerInvariant("checked break reached boxy lowering outside a loop");
        }
        return self.loop_stack.items[self.loop_stack.items.len - 1];
    }

    fn lowerLowLevelInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: can.CIR.Expr.LowLevel,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        switch (op) {
            .box_box,
            .box_unbox,
            => return try self.lowerBoxBoundaryLowLevelInto(target, op, args, next),
            .list_map_can_reuse => return try self.lowerListMapCanReuseInto(target, args, next),
            else => {},
        }
        if (lowLevelNeedsIntegerMultiplicationOverflowCheck(op)) {
            return try self.lowerIntegerOverflowCheckedMultiplicationLowLevelInto(target, op, args, next);
        }
        if (lowLevelNeedsSignedIntegerLowestCheck(op)) {
            return try self.lowerSignedIntegerLowestCheckedUnaryLowLevelInto(target, op, args, next);
        }
        if (lowLevelNeedsIntegerZeroDenominatorCheck(op)) {
            return try self.lowerIntegerZeroDenominatorCheckedLowLevelInto(target, op, args, next);
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.parent.allocator.free(lowered);

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(lowered),
            .next = next,
        } });
        continuation = try self.prependLoweredExprs(args, lowered, continuation);
        return continuation;
    }

    fn lowerBoxBoundaryLowLevelInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: can.CIR.Expr.LowLevel,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len != 1) boxyLowerInvariant("Box low-level operation reached boxy lowering with the wrong arity");
        const source_expr = self.module.checked_bodies.expr(args[0]);
        const source = try self.addFrameLocal(self.workerRuntimeLayoutForType(source_expr.ty).layoutIdx());
        const assign = try self.assignBoxBoundary(target, source, op, next);
        return try self.lowerExprInto(source, args[0], assign);
    }

    fn assignBoxBoundary(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        op: can.CIR.Expr.LowLevel,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const target_layout = self.parent.result.store.getLocal(target).layout_idx;
        const source_layout = self.parent.result.store.getLocal(source).layout_idx;
        if (target_layout == source_layout) return try self.assignLocal(target, source, next);

        const target_content = self.parent.result.layouts.getLayout(target_layout);
        const source_content = self.parent.result.layouts.getLayout(source_layout);
        if (target_content.eql(source_content)) return try self.assignLocal(target, source, next);

        switch (op) {
            .box_box => {
                if (target_content.tag == .box and self.parent.result.layouts.getLayout(target_content.getIdx()).eql(source_content)) {
                    return try self.assignUnaryLowLevel(target, .box_box, source, next);
                }
                if (target_content.tag == .box_of_zst and self.parent.result.layouts.isZeroSized(source_content)) {
                    return try self.assignUnaryLowLevel(target, .box_box, source, next);
                }
            },
            .box_unbox => {
                if (source_content.tag == .box and self.parent.result.layouts.getLayout(source_content.getIdx()).eql(target_content)) {
                    return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
                }
                if (source_content.tag == .box_of_zst and self.parent.result.layouts.isZeroSized(target_content)) {
                    return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
                }
            },
            else => unreachable,
        }

        boxyLowerInvariant("Box boundary low-level operation required descriptor-backed box adaptation lowering");
    }

    fn lowerListMapCanReuseInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const interchangeable = self.listMapLayoutsInterchangeable(args);
        if (!interchangeable.get(.u32) and !interchangeable.get(.u64)) {
            return try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i64_literal = .{
                    .value = 0,
                    .layout_idx = self.parent.result.store.getLocal(target).layout_idx,
                } },
                .next = next,
            } });
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.parent.allocator.free(lowered);

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .list_map_can_reuse,
            .rc_effect = LIR.LowLevel.list_map_can_reuse.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(lowered),
            .interchangeable = interchangeable,
            .next = next,
        } });
        continuation = try self.prependLoweredExprs(args, lowered, continuation);
        return continuation;
    }

    fn listMapLayoutsInterchangeable(
        self: *ProcBodyBuilder,
        args: []const checked.CheckedExprId,
    ) layout.WidthValues(bool) {
        const none = layout.WidthValues(bool).both(false, false);
        if (!self.parent.options.list_in_place_map) return none;
        if (args.len != 2) boxyLowerInvariant("list_map_can_reuse reached boxy lowering with the wrong arity");

        const list_expr = self.module.checked_bodies.expr(args[0]);
        const list_layout_idx = self.workerRuntimeLayoutForType(list_expr.ty).layoutIdx();
        const list_layout = self.parent.result.layouts.getLayout(list_layout_idx);
        if (list_layout.tag != .list) return none;
        const in_elem_idx = self.parent.result.layouts.runtimeRepresentationLayoutIdx(list_layout.getIdx());

        const transform_expr = self.module.checked_bodies.expr(args[1]);
        const out_ret_rep = self.functionReturnRepForType(transform_expr.ty);
        const out_elem_idx = self.parent.result.layouts.runtimeRepresentationLayoutIdx(
            self.workerRuntimeLayoutForRep(out_ret_rep).layoutIdx(),
        );

        return layout.WidthValues(bool).both(
            self.listMapInterchangeableAtWidth(in_elem_idx, out_elem_idx, .u32),
            self.listMapInterchangeableAtWidth(in_elem_idx, out_elem_idx, .u64),
        );
    }

    fn listMapInterchangeableAtWidth(
        self: *ProcBodyBuilder,
        in_elem_idx: layout.Idx,
        out_elem_idx: layout.Idx,
        target_usize: base.target.TargetUsize,
    ) bool {
        const layouts = &self.parent.result.layouts;
        const in_elem = layouts.getLayout(in_elem_idx);
        const in_size = layouts.sizeAt(in_elem, target_usize);
        if (in_size == 0) return false;

        const out_elem = layouts.getLayout(out_elem_idx);
        if (layouts.sizeAt(out_elem, target_usize) != in_size) return false;

        const ptr_width = target_usize.size();
        const in_alignment: u32 = @intCast(in_elem.alignment(target_usize).toByteUnits());
        const out_alignment: u32 = @intCast(out_elem.alignment(target_usize).toByteUnits());
        if (@max(ptr_width, in_alignment) != @max(ptr_width, out_alignment)) return false;

        if (layouts.layoutContainsRefcounted(in_elem) != layouts.layoutContainsRefcounted(out_elem)) return false;

        return true;
    }

    fn lowerExprsToTemps(
        self: *ProcBodyBuilder,
        args: []const checked.CheckedExprId,
    ) Allocator.Error![]LIR.LocalId {
        const lowered = try self.parent.allocator.alloc(LIR.LocalId, args.len);
        errdefer self.parent.allocator.free(lowered);
        for (args, lowered) |arg, *local| {
            const expr = self.module.checked_bodies.expr(arg);
            local.* = try self.addFrameLocal(self.workerRuntimeLayoutForType(expr.ty).layoutIdx());
        }
        return lowered;
    }

    fn prependLoweredExprs(
        self: *ProcBodyBuilder,
        args: []const checked.CheckedExprId,
        lowered: []const LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len != lowered.len) {
            boxyLowerInvariant("boxy lowered expression locals disagreed with source expression count");
        }
        var continuation = next;
        var index = args.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.lowerExprInto(lowered[index], args[index], continuation);
        }
        return continuation;
    }

    fn lowLevelNeedsIntegerZeroDenominatorCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            => true,
            else => false,
        };
    }

    fn lowLevelNeedsIntegerMultiplicationOverflowCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_times => true,
            else => false,
        };
    }

    fn lowLevelNeedsSignedIntegerLowestCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_negate,
            .num_abs,
            => true,
            else => false,
        };
    }

    fn integerDivisionByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
        return switch (layout_idx) {
            .u8 => "U8 division by zero",
            .i8 => "I8 division by zero",
            .u16 => "U16 division by zero",
            .i16 => "I16 division by zero",
            .u32 => "U32 division by zero",
            .i32 => "I32 division by zero",
            .u64 => "U64 division by zero",
            .i64 => "I64 division by zero",
            .u128 => "U128 division by zero",
            .i128 => "I128 division by zero",
            else => null,
        };
    }

    fn signedIntegerLowestValue(layout_idx: layout.Idx) ?i128 {
        return switch (layout_idx) {
            .i8 => std.math.minInt(i8),
            .i16 => std.math.minInt(i16),
            .i32 => std.math.minInt(i32),
            .i64 => std.math.minInt(i64),
            .i128 => std.math.minInt(i128),
            else => null,
        };
    }

    fn signedIntegerLowestOverflowMessage(op: LIR.LowLevel, layout_idx: layout.Idx) ?[]const u8 {
        if (signedIntegerLowestValue(layout_idx) == null) return null;

        return switch (op) {
            .num_negate => "signed integer negation overflowed",
            .num_abs => "signed integer absolute value overflowed",
            .num_div_by,
            .num_div_trunc_by,
            => "signed integer division overflowed",
            else => null,
        };
    }

    fn lowerIntegerOverflowCheckedMultiplicationLowLevelInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len != 2) {
            boxyLowerInvariant("integer multiplication reached boxy LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.parent.allocator.free(lowered);

        const lhs = lowered[0];
        const rhs = lowered[1];
        const lhs_layout = self.parent.result.store.getLocal(lhs).layout_idx;
        const rhs_layout = self.parent.result.store.getLocal(rhs).layout_idx;
        if (integerDivisionByZeroMessage(lhs_layout) == null) {
            var continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(lowered),
                .next = next,
            } });
            continuation = try self.prependLoweredExprs(args, lowered, continuation);
            return continuation;
        }

        const zero = try self.addFrameLocal(rhs_layout);
        const rhs_is_zero = try self.addFrameLocal(.bool);
        const quotient = try self.addFrameLocal(lhs_layout);
        const quotient_matches_lhs = try self.addFrameLocal(.bool);

        const crash_stmt = try self.parent.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.parent.result.store.insertString("integer multiplication overflowed"),
        } });

        const quotient_match_switch = try self.boolSwitchNoContinuation(quotient_matches_lhs, next, crash_stmt);
        const quotient_eq_args = [_]LIR.LocalId{ quotient, lhs };
        const quotient_eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = quotient_matches_lhs,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&quotient_eq_args),
            .next = quotient_match_switch,
        } });
        const div_args = [_]LIR.LocalId{ target, rhs };
        const division_check = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = quotient,
            .op = .num_div_trunc_by,
            .rc_effect = LIR.LowLevel.num_div_trunc_by.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&div_args),
            .next = quotient_eq_stmt,
        } });

        const nonzero_check = if (signedIntegerLowestValue(lhs_layout)) |lowest_value| blk: {
            const lowest = try self.addFrameLocal(lhs_layout);
            const neg_one = try self.addFrameLocal(rhs_layout);
            const lhs_is_lowest = try self.addFrameLocal(.bool);
            const rhs_is_neg_one = try self.addFrameLocal(.bool);

            const rhs_neg_one_switch = try self.boolSwitchNoContinuation(rhs_is_neg_one, crash_stmt, division_check);
            const rhs_eq_args = [_]LIR.LocalId{ rhs, neg_one };
            const rhs_eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = rhs_is_neg_one,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(&rhs_eq_args),
                .next = rhs_neg_one_switch,
            } });
            const assign_neg_one = try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = neg_one,
                .value = .{ .i128_literal = .{
                    .value = -1,
                    .layout_idx = rhs_layout,
                } },
                .next = rhs_eq_stmt,
            } });

            const lhs_lowest_switch = try self.boolSwitchNoContinuation(lhs_is_lowest, assign_neg_one, division_check);
            const lhs_eq_args = [_]LIR.LocalId{ lhs, lowest };
            const lhs_eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = lhs_is_lowest,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(&lhs_eq_args),
                .next = lhs_lowest_switch,
            } });
            break :blk try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = lowest,
                .value = .{ .i128_literal = .{
                    .value = lowest_value,
                    .layout_idx = lhs_layout,
                } },
                .next = lhs_eq_stmt,
            } });
        } else division_check;

        const zero_switch = try self.boolSwitchNoContinuation(rhs_is_zero, next, nonzero_check);
        const zero_eq_args = [_]LIR.LocalId{ rhs, zero };
        const zero_eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = rhs_is_zero,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&zero_eq_args),
            .next = zero_switch,
        } });
        const assign_zero = try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = zero,
            .value = .{ .i128_literal = .{
                .value = 0,
                .layout_idx = rhs_layout,
            } },
            .next = zero_eq_stmt,
        } });

        const raw_args = [_]LIR.LocalId{ lhs, rhs };
        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&raw_args),
            .next = assign_zero,
        } });
        continuation = try self.prependLoweredExprs(args, lowered, continuation);
        return continuation;
    }

    fn lowerSignedIntegerLowestCheckedUnaryLowLevelInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len != 1) {
            boxyLowerInvariant("signed integer unary operation reached boxy LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.parent.allocator.free(lowered);

        const source = lowered[0];
        const source_layout = self.parent.result.store.getLocal(source).layout_idx;
        const lowest_value = signedIntegerLowestValue(source_layout) orelse {
            var continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(lowered),
                .next = next,
            } });
            continuation = try self.prependLoweredExprs(args, lowered, continuation);
            return continuation;
        };
        const crash_message = signedIntegerLowestOverflowMessage(op, source_layout) orelse {
            boxyLowerInvariant("signed integer unary operation did not have an overflow message");
        };

        const lowest = try self.addFrameLocal(source_layout);
        const is_lowest = try self.addFrameLocal(.bool);

        const raw_args = [_]LIR.LocalId{source};
        const raw_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&raw_args),
            .next = next,
        } });

        const crash_stmt = try self.parent.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.parent.result.store.insertString(crash_message),
        } });

        const switch_stmt = try self.boolSwitchNoContinuation(is_lowest, crash_stmt, raw_stmt);

        const eq_args = [_]LIR.LocalId{ source, lowest };
        const eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = is_lowest,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&eq_args),
            .next = switch_stmt,
        } });

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = lowest,
            .value = .{ .i128_literal = .{
                .value = lowest_value,
                .layout_idx = source_layout,
            } },
            .next = eq_stmt,
        } });
        continuation = try self.prependLoweredExprs(args, lowered, continuation);
        return continuation;
    }

    fn lowerIntegerZeroDenominatorCheckedLowLevelInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (args.len != 2) {
            boxyLowerInvariant("integer division/remainder reached boxy LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.parent.allocator.free(lowered);

        const lhs = lowered[0];
        const rhs = lowered[1];
        const rhs_layout = self.parent.result.store.getLocal(rhs).layout_idx;
        const lhs_layout = self.parent.result.store.getLocal(lhs).layout_idx;
        const crash_message = integerDivisionByZeroMessage(rhs_layout) orelse {
            var continuation = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(lowered),
                .next = next,
            } });
            continuation = try self.prependLoweredExprs(args, lowered, continuation);
            return continuation;
        };

        const zero = try self.addFrameLocal(rhs_layout);
        const is_zero = try self.addFrameLocal(.bool);

        const div_args = [_]LIR.LocalId{ lhs, rhs };
        const div_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&div_args),
            .next = next,
        } });

        const normal_stmt = if (signedIntegerLowestValue(lhs_layout)) |lowest_value| blk: {
            const overflow_body = switch (op) {
                .num_div_by,
                .num_div_trunc_by,
                => try self.parent.result.store.addCFStmt(.{ .crash = .{
                    .msg = try self.parent.result.store.insertString(signedIntegerLowestOverflowMessage(op, lhs_layout) orelse {
                        boxyLowerInvariant("signed integer division operation did not have an overflow message");
                    }),
                } }),
                .num_rem_by,
                .num_mod_by,
                => try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .i128_literal = .{
                        .value = 0,
                        .layout_idx = lhs_layout,
                    } },
                    .next = next,
                } }),
                else => div_stmt,
            };

            const lowest = try self.addFrameLocal(lhs_layout);
            const neg_one = try self.addFrameLocal(rhs_layout);
            const lhs_is_lowest = try self.addFrameLocal(.bool);
            const rhs_is_neg_one = try self.addFrameLocal(.bool);

            const rhs_neg_one_switch = try self.boolSwitchNoContinuation(rhs_is_neg_one, overflow_body, div_stmt);
            const rhs_eq_args = [_]LIR.LocalId{ rhs, neg_one };
            const rhs_eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = rhs_is_neg_one,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(&rhs_eq_args),
                .next = rhs_neg_one_switch,
            } });
            const assign_neg_one = try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = neg_one,
                .value = .{ .i128_literal = .{
                    .value = -1,
                    .layout_idx = rhs_layout,
                } },
                .next = rhs_eq_stmt,
            } });

            const lhs_lowest_switch = try self.boolSwitchNoContinuation(lhs_is_lowest, assign_neg_one, div_stmt);
            const lhs_eq_args = [_]LIR.LocalId{ lhs, lowest };
            const lhs_eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = lhs_is_lowest,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(&lhs_eq_args),
                .next = lhs_lowest_switch,
            } });
            break :blk try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = lowest,
                .value = .{ .i128_literal = .{
                    .value = lowest_value,
                    .layout_idx = lhs_layout,
                } },
                .next = lhs_eq_stmt,
            } });
        } else div_stmt;

        const crash_stmt = try self.parent.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.parent.result.store.insertString(crash_message),
        } });

        const switch_stmt = try self.boolSwitchNoContinuation(is_zero, crash_stmt, normal_stmt);

        const eq_args = [_]LIR.LocalId{ rhs, zero };
        const eq_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = is_zero,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&eq_args),
            .next = switch_stmt,
        } });

        var continuation = try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = zero,
            .value = .{ .i128_literal = .{
                .value = 0,
                .layout_idx = rhs_layout,
            } },
            .next = eq_stmt,
        } });
        continuation = try self.prependLoweredExprs(args, lowered, continuation);
        return continuation;
    }

    fn boolSwitchNoContinuation(
        self: *ProcBodyBuilder,
        cond: LIR.LocalId,
        true_body: LIR.CFStmtId,
        false_body: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const branches = [_]LIR.CFSwitchBranch{.{ .value = 1, .body = true_body }};
        return try self.parent.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try self.parent.result.store.addCFSwitchBranches(&branches),
            .default_branch = false_body,
            .continuation = null,
        } });
    }

    fn lowerDbgExprInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        child: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const after_dbg = try self.assignZst(target, next);
        const message = try self.addFrameLocal(.str);
        const debug_stmt = try self.parent.result.store.addCFStmt(.{ .debug = .{
            .message = message,
            .next = after_dbg,
        } });
        return try self.lowerInspectExprInto(message, child, debug_stmt);
    }

    fn lowerExpectExprInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        child: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const after = try self.assignZst(target, next);
        return try self.lowerExpectStmt(child, after);
    }

    fn lowerExpectStmt(
        self: *ProcBodyBuilder,
        child: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const child_expr = self.module.checked_bodies.expr(child);
        const cond = try self.addFrameLocal(self.workerRuntimeLayoutForType(child_expr.ty).layoutIdx());
        const expect_stmt = try self.parent.result.store.addCFStmt(.{ .expect = .{
            .condition = cond,
            .next = next,
        } });
        return try self.lowerExprInto(cond, child, expect_stmt);
    }

    fn lowerExpectErrInto(
        self: *ProcBodyBuilder,
        child: checked.CheckedExprId,
        snippet: checked.CheckedStringLiteralId,
    ) Allocator.Error!LIR.CFStmtId {
        const child_expr = self.module.checked_bodies.expr(child);
        const child_local = try self.addFrameLocal(self.workerRuntimeLayoutForType(child_expr.ty).layoutIdx());
        const rendered = try self.addFrameLocal(.str);
        const prefix = try self.addFrameLocal(.str);
        const with_value = try self.addFrameLocal(.str);
        const suffix = try self.addFrameLocal(.str);
        const message = try self.addFrameLocal(.str);

        const snippet_index = @intFromEnum(snippet);
        if (snippet_index >= self.module.checked_bodies.stringLiteralCount()) {
            boxyLowerInvariant("checked expect_err snippet referenced a missing string literal");
        }
        const snippet_text = self.module.checked_bodies.stringLiteral(@enumFromInt(snippet_index));
        const prefix_text = try std.fmt.allocPrint(
            self.parent.allocator,
            "The `?` operator in `{s}` evaluated an `Err` inside an `expect`. The value was: Err(",
            .{snippet_text},
        );
        defer self.parent.allocator.free(prefix_text);

        var continuation = try self.parent.result.store.addCFStmt(.{ .expect_err = .{
            .message = message,
            .region = self.parent.result.store.current_region,
        } });
        continuation = try self.assignStrConcat(message, with_value, suffix, continuation);
        continuation = try self.assignStringBytesLiteral(suffix, ")", continuation);
        continuation = try self.assignStrConcat(with_value, prefix, rendered, continuation);
        continuation = try self.lowerInspectLocalInto(rendered, child_local, child_expr.ty, continuation);
        continuation = try self.assignStringBytesLiteral(prefix, prefix_text, continuation);
        return try self.lowerExprInto(child_local, child, continuation);
    }

    fn lowerInspectExprInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        expr_id: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const expr = self.module.checked_bodies.expr(expr_id);
        const value = try self.addFrameLocal(self.workerRuntimeLayoutForType(expr.ty).layoutIdx());
        const inspect = try self.lowerInspectLocalInto(target, value, expr.ty, next);
        return try self.lowerExprInto(value, expr_id, inspect);
    }

    fn lowerInspectLocalInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        checked_ty: checked.CheckedTypeId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.parent.result.store.getLocal(target).layout_idx != .str) {
            boxyLowerInvariant("boxy inspect target did not have Str layout");
        }
        return try self.lowerInspectRepLocalInto(target, source, self.repForType(checked_ty), next);
    }

    fn lowerInspectRepLocalInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .in_progress => boxyLowerInvariant("in-progress boxy representation reached inspect lowering"),
            .dynamic => boxyLowerInvariant("dynamic inspect reached boxy lowering before descriptor inspect support"),
            .erased_callable => try self.assignStringBytesLiteral(target, "<function>", next),
            .list => try self.lowerListInspectLocalsInto(target, source, rep_id, next),
            .box => try self.lowerBoxInspectLocalsInto(target, source, rep_id, next),
            .primitive => |primitive| try self.lowerPrimitiveInspectLocalsInto(target, source, primitive, next),
            .bool_tag_union => try self.lowerBoolInspectLocalsInto(target, source, next),
            .empty_record => try self.assignStringBytesLiteral(target, "{}", next),
            .empty_tag_union => try self.parent.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.parent.result.store.insertString("uninhabited value reached Str.inspect"),
            } }),
            .alias => try self.lowerInspectRepLocalInto(target, source, self.requiredSingleChild(rep_id, .alias_backing).rep, next),
            .nominal => |kind| switch (kind) {
                .transparent => if (rep.declared_fields.len != 0)
                    try self.lowerNominalBackingInspectLocalsInto(target, source, rep_id, next)
                else
                    try self.lowerInspectRepLocalInto(target, source, self.requiredSingleChild(rep_id, .nominal_backing).rep, next),
                .opaque_nominal => try self.assignStringBytesLiteral(target, "<opaque>", next),
                .builtin_other => try self.lowerInspectRepLocalInto(target, source, self.requiredSingleChild(rep_id, .nominal_backing).rep, next),
            },
            .record,
            .record_unbound,
            => try self.lowerRecordInspectLocalsInto(target, source, rep, next),
            .tuple => try self.lowerTupleInspectLocalsInto(target, source, rep, next),
            .tag_union => try self.lowerTagUnionInspectLocalsInto(target, source, rep, next),
        };
    }

    fn lowerNominalBackingInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing_rep = self.requiredSingleChild(rep_id, .nominal_backing).rep;
        const backing = try self.addFrameLocal(self.workerRuntimeLayoutForRep(backing_rep).layoutIdx());
        const inspect = try self.lowerInspectRepLocalInto(target, backing, backing_rep, next);
        return try self.assignNominalBoundary(backing, source, inspect);
    }

    fn lowerPrimitiveInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        primitive: checked.CheckedPrimitive,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (primitive == .bool) {
            return try self.lowerBoolInspectLocalsInto(target, source, next);
        }
        const op = primitiveInspectLowLevelOp(primitive);
        return try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{source}),
            .next = next,
        } });
    }

    fn lowerBoolInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const false_body = try self.assignStringBytesLiteral(target, "False", next);
        const true_body = try self.assignStringBytesLiteral(target, "True", next);
        const discriminant = try self.addFrameLocal(.u16);
        const branches = [_]LIR.CFSwitchBranch{.{ .value = 1, .body = true_body }};
        const switch_stmt = try self.parent.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = discriminant,
            .branches = try self.parent.result.store.addCFSwitchBranches(&branches),
            .default_branch = false_body,
            .continuation = null,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn lowerRecordInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const children = self.parent.plan.childSlice(rep.children);
        const field_count = recordEqualityFieldCount(children);
        if (field_count == 0) return try self.assignStringBytesLiteral(target, "{}", next);

        var parts = std.ArrayList(InspectPart).empty;
        defer parts.deinit(self.parent.allocator);
        try parts.append(self.parent.allocator, .{ .literal = "{ " });

        var field_index: u16 = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => |label| {
                    if (field_index != 0) try parts.append(self.parent.allocator, .{ .literal = ", " });
                    try parts.append(self.parent.allocator, .{ .literal = self.module.canonical_names.recordFieldLabelText(label) });
                    try parts.append(self.parent.allocator, .{ .literal = ": " });
                    try parts.append(self.parent.allocator, .{ .field = .{
                        .source = source,
                        .field_index = field_index,
                        .rep = child.rep,
                    } });
                    field_index += 1;
                },
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("record inspect representation had a non-record child role"),
            }
        }
        try parts.append(self.parent.allocator, .{ .literal = " }" });
        return try self.lowerInspectPartsInto(target, parts.items, next);
    }

    fn lowerTupleInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const children = self.parent.plan.childSlice(rep.children);
        if (children.len == 0) return try self.assignStringBytesLiteral(target, "()", next);

        var parts = std.ArrayList(InspectPart).empty;
        defer parts.deinit(self.parent.allocator);
        try parts.append(self.parent.allocator, .{ .literal = "(" });
        for (children, 0..) |child, ordinal| {
            switch (child.role) {
                .tuple_elem => |index| {
                    if (ordinal != 0) try parts.append(self.parent.allocator, .{ .literal = ", " });
                    if (index > std.math.maxInt(u16)) {
                        boxyLowerInvariant("tuple inspect element index exceeded LIR field index range");
                    }
                    try parts.append(self.parent.allocator, .{ .field = .{
                        .source = source,
                        .field_index = @intCast(index),
                        .rep = child.rep,
                    } });
                },
                else => boxyLowerInvariant("tuple inspect representation had a non-tuple child role"),
            }
        }
        try parts.append(self.parent.allocator, .{ .literal = ")" });
        return try self.lowerInspectPartsInto(target, parts.items, next);
    }

    fn lowerTagUnionInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const variants = self.parent.plan.tagVariantSlice(rep.tag_variants);
        if (variants.len == 0) {
            return try self.parent.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.parent.result.store.insertString("uninhabited value reached Str.inspect"),
            } });
        }
        if (variants.len == 1 and self.isZstLocal(source)) {
            return try self.lowerTagInspectVariant(target, source, variants[0], 0, next);
        }

        const discriminant = try self.addFrameLocal(.u16);
        const branches = try self.parent.allocator.alloc(LIR.CFSwitchBranch, variants.len);
        defer self.parent.allocator.free(branches);
        for (variants, branches, 0..) |variant, *branch, index| {
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag inspect variant index exceeded LIR variant range");
            }
            branch.* = .{
                .value = @intCast(index),
                .body = try self.lowerTagInspectVariant(target, source, variant, @intCast(index), next),
            };
        }

        const bad_discriminant = try self.parent.result.store.addCFStmt(.runtime_error);
        const switch_stmt = try self.parent.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = discriminant,
            .branches = try self.parent.result.store.addCFSwitchBranches(branches),
            .default_branch = bad_discriminant,
            .continuation = null,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn lowerBoxInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const payload_rep = self.requiredSingleChild(rep_id, .box_payload).rep;
        const payload = try self.addFrameLocal(self.workerRuntimeLayoutForRep(payload_rep).layoutIdx());
        const prefix = try self.addFrameLocal(.str);
        const rendered = try self.addFrameLocal(.str);
        const with_value = try self.addFrameLocal(.str);
        const suffix = try self.addFrameLocal(.str);

        var continuation = try self.assignStrConcat(target, with_value, suffix, next);
        continuation = try self.assignStringBytesLiteral(suffix, ")", continuation);
        continuation = try self.assignStrConcat(with_value, prefix, rendered, continuation);
        continuation = try self.lowerInspectRepLocalInto(rendered, payload, payload_rep, continuation);
        continuation = try self.assignBoxBoundary(payload, source, .box_unbox, continuation);
        return try self.assignStringBytesLiteral(prefix, "Box(", continuation);
    }

    fn lowerListInspectLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const elem_rep = self.requiredSingleChild(rep_id, .list_elem).rep;
        const len = try self.addFrameLocal(.u64);
        const index = try self.addFrameLocal(.u64);
        const out = try self.addFrameLocal(.str);
        const zero_index = try self.addFrameLocal(.u64);
        const initial_out = try self.addFrameLocal(.str);
        const join_id = self.freshJoinPointId();

        const body = try self.lowerListInspectLoopBody(target, source, elem_rep, len, index, out, join_id, next);
        var initial_jump = try self.parent.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        initial_jump = try self.setLocalInitializeJoinParam(out, initial_out, initial_jump);
        initial_jump = try self.setLocalInitializeJoinParam(index, zero_index, initial_jump);
        initial_jump = try self.assignStringBytesLiteral(initial_out, "[", initial_jump);
        initial_jump = try self.assignIntLiteral(zero_index, 0, initial_jump);
        initial_jump = try self.assignUnaryLowLevel(len, .list_len, source, initial_jump);

        return try self.parent.result.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{ index, out }),
            .body = body,
            .remainder = initial_jump,
        } });
    }

    fn lowerListInspectLoopBody(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        elem_rep: Plan.TypeRepId,
        len: LIR.LocalId,
        index: LIR.LocalId,
        out: LIR.LocalId,
        join_id: LIR.JoinPointId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const done = try self.addFrameLocal(.bool);
        const finish = try self.lowerListInspectFinish(target, out, next);
        const step = try self.lowerListInspectStep(source, elem_rep, index, out, join_id);
        const switch_stmt = try self.boolSwitchNoContinuation(done, finish, step);
        return try self.assignBinaryLowLevel(done, .num_is_eq, index, len, switch_stmt);
    }

    fn lowerListInspectFinish(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        out: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const close = try self.addFrameLocal(.str);
        const concat = try self.assignStrConcat(target, out, close, next);
        return try self.assignStringBytesLiteral(close, "]", concat);
    }

    fn lowerListInspectStep(
        self: *ProcBodyBuilder,
        source: LIR.LocalId,
        elem_rep: Plan.TypeRepId,
        index: LIR.LocalId,
        out: LIR.LocalId,
        join_id: LIR.JoinPointId,
    ) Allocator.Error!LIR.CFStmtId {
        const sep = try self.addFrameLocal(.str);
        const zero = try self.addFrameLocal(.u64);
        const is_first = try self.addFrameLocal(.bool);
        const elem = try self.addFrameLocal(self.workerRuntimeLayoutForRep(elem_rep).layoutIdx());
        const elem_str = try self.addFrameLocal(.str);
        const with_sep = try self.addFrameLocal(.str);
        const next_out = try self.addFrameLocal(.str);
        const one = try self.addFrameLocal(.u64);
        const next_index = try self.addFrameLocal(.u64);

        var continuation = try self.parent.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        continuation = try self.setLocalInitializeJoinParam(out, next_out, continuation);
        continuation = try self.setLocalInitializeJoinParam(index, next_index, continuation);
        continuation = try self.assignBinaryLowLevel(next_index, .num_plus, index, one, continuation);
        continuation = try self.assignIntLiteral(one, 1, continuation);
        continuation = try self.assignStrConcat(next_out, with_sep, elem_str, continuation);
        continuation = try self.lowerInspectRepLocalInto(elem_str, elem, elem_rep, continuation);
        if (!self.isZstLocal(elem)) {
            continuation = try self.assignBinaryLowLevel(elem, .list_get_unsafe, source, index, continuation);
        }
        continuation = try self.assignStrConcat(with_sep, out, sep, continuation);

        const empty_sep = try self.assignStringBytesLiteral(sep, "", continuation);
        const comma_sep = try self.assignStringBytesLiteral(sep, ", ", continuation);
        const sep_switch = try self.boolSwitchNoContinuation(is_first, empty_sep, comma_sep);
        const compare_first = try self.assignBinaryLowLevel(is_first, .num_is_eq, index, zero, sep_switch);
        return try self.assignIntLiteral(zero, 0, compare_first);
    }

    fn lowerTagInspectVariant(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        variant: Plan.TagVariant,
        variant_index: u16,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const payloads = self.parent.plan.childSlice(variant.payloads);
        if (payloads.len == 0) {
            return try self.assignStringBytesLiteral(target, self.module.canonical_names.tagLabelText(variant.name), next);
        }

        var parts = std.ArrayList(InspectPart).empty;
        defer parts.deinit(self.parent.allocator);
        try parts.append(self.parent.allocator, .{ .literal = self.module.canonical_names.tagLabelText(variant.name) });
        try parts.append(self.parent.allocator, .{ .literal = "(" });
        for (payloads, 0..) |child, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != variant.name or payload.index != index) {
                        boxyLowerInvariant("tag inspect payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag inspect variant payload span included a non-payload child"),
            }
            if (index != 0) try parts.append(self.parent.allocator, .{ .literal = ", " });
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag inspect payload index exceeded LIR payload index range");
            }
            try parts.append(self.parent.allocator, .{ .tag_payload = .{
                .source = source,
                .variant_index = variant_index,
                .payload_index = if (payloads.len == 1) null else @as(u16, @intCast(index)),
                .rep = child.rep,
            } });
        }
        try parts.append(self.parent.allocator, .{ .literal = ")" });
        return try self.lowerInspectPartsInto(target, parts.items, next);
    }

    fn lowerInspectPartsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        parts: []const InspectPart,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (parts.len == 0) return try self.assignStringBytesLiteral(target, "", next);

        const part_locals = try self.parent.allocator.alloc(LIR.LocalId, parts.len);
        defer self.parent.allocator.free(part_locals);
        for (part_locals) |*local| {
            local.* = try self.addFrameLocal(.str);
        }

        var continuation = try self.concatStringLocalsInto(target, part_locals, next);
        var index = parts.len;
        while (index > 0) {
            index -= 1;
            continuation = try self.lowerInspectPartInto(part_locals[index], parts[index], continuation);
        }
        return continuation;
    }

    fn lowerInspectPartInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        part: InspectPart,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return switch (part) {
            .literal => |text| try self.assignStringBytesLiteral(target, text, next),
            .field => |field| blk: {
                const value = try self.addFrameLocal(self.workerRuntimeLayoutForRep(field.rep).layoutIdx());
                var continuation = try self.lowerInspectRepLocalInto(target, value, field.rep, next);
                if (!self.isZstLocal(value)) {
                    continuation = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                        .target = value,
                        .op = .{ .field = .{
                            .source = field.source,
                            .field_idx = field.field_index,
                        } },
                        .next = continuation,
                    } });
                }
                break :blk continuation;
            },
            .tag_payload => |payload| blk: {
                const value = try self.addFrameLocal(self.workerRuntimeLayoutForRep(payload.rep).layoutIdx());
                var continuation = try self.lowerInspectRepLocalInto(target, value, payload.rep, next);
                if (!self.isZstLocal(value)) {
                    continuation = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                        .target = value,
                        .op = if (payload.payload_index) |index| .{ .tag_payload = .{
                            .source = payload.source,
                            .payload_idx = index,
                            .variant_index = payload.variant_index,
                            .tag_discriminant = payload.variant_index,
                        } } else .{ .tag_payload_struct = .{
                            .source = payload.source,
                            .variant_index = payload.variant_index,
                            .tag_discriminant = payload.variant_index,
                        } },
                        .next = continuation,
                    } });
                }
                break :blk continuation;
            },
        };
    }

    fn concatStringLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        parts: []const LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        switch (parts.len) {
            0 => return try self.assignStringBytesLiteral(target, "", next),
            1 => return try self.assignLocal(target, parts[0], next),
            else => {},
        }

        const concat_results = try self.parent.allocator.alloc(LIR.LocalId, parts.len - 1);
        defer self.parent.allocator.free(concat_results);
        for (concat_results[0 .. concat_results.len - 1]) |*local| {
            local.* = try self.addFrameLocal(.str);
        }
        concat_results[concat_results.len - 1] = target;

        var continuation = next;
        var concat_index = concat_results.len;
        while (concat_index > 0) {
            concat_index -= 1;
            const lhs = if (concat_index == 0) parts[0] else concat_results[concat_index - 1];
            const rhs = parts[concat_index + 1];
            continuation = try self.assignStrConcat(concat_results[concat_index], lhs, rhs, continuation);
        }
        return continuation;
    }

    fn assignStrConcat(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .str_concat,
            .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{ lhs, rhs }),
            .next = next,
        } });
    }

    fn lowerReturn(
        self: *ProcBodyBuilder,
        expr_id: checked.CheckedExprId,
        lambda_id: checked.CheckedExprId,
    ) Allocator.Error!LIR.CFStmtId {
        const current = self.current_lambda orelse
            boxyLowerInvariant("checked return reached boxy lowering outside a lambda worker");
        if (current != lambda_id) {
            boxyLowerInvariant("checked return target lambda differed from current boxy worker lambda");
        }

        const expr = self.module.checked_bodies.expr(expr_id);
        const ret_layout = self.workerReturnLayout();
        if (self.workerRuntimeLayoutForType(expr.ty).layoutIdx() != ret_layout) {
            boxyLowerInvariant("checked return value required explicit box/adapt lowering before return");
        }

        const ret_local = try self.addFrameLocal(ret_layout);
        const ret_stmt = try self.parent.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        return try self.lowerExprInto(ret_local, expr_id, ret_stmt);
    }

    fn lowerBoolBinopInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: can.CIR.Expr.Binop.Op,
        lhs: checked.CheckedExprId,
        rhs: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const true_body, const false_body = switch (op) {
            .@"and" => .{
                try self.lowerExprInto(target, rhs, next),
                try self.assignBoolLiteral(target, false, next),
            },
            .@"or" => .{
                try self.assignBoolLiteral(target, true, next),
                try self.lowerExprInto(target, rhs, next),
            },
            else => boxyLowerInvariant("non-short-circuit checked binop reached boxy binop lowering before dispatch lowering"),
        };

        const lhs_expr = self.module.checked_bodies.expr(lhs);
        const cond = try self.addFrameLocal(self.workerRuntimeLayoutForType(lhs_expr.ty).layoutIdx());
        const switch_stmt = try self.boolSwitchNoContinuation(cond, true_body, false_body);
        return try self.lowerExprInto(cond, lhs, switch_stmt);
    }

    fn lowerStructuralEqInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs_id: checked.CheckedExprId,
        rhs_id: checked.CheckedExprId,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const lhs_expr = self.module.checked_bodies.expr(lhs_id);
        const rhs_expr = self.module.checked_bodies.expr(rhs_id);
        if (lhs_expr.ty != rhs_expr.ty) {
            boxyLowerInvariant("checked structural equality operands had different checked types");
        }

        const rep_id = self.repForType(lhs_expr.ty);
        const operand_layout = self.workerRuntimeLayoutForType(lhs_expr.ty).layoutIdx();
        if (self.workerRuntimeLayoutForType(rhs_expr.ty).layoutIdx() != operand_layout) {
            boxyLowerInvariant("checked structural equality operand layouts disagreed");
        }

        const lhs = try self.addFrameLocal(operand_layout);
        const rhs = try self.addFrameLocal(operand_layout);
        var continuation = try self.lowerEqRepLocalsInto(target, lhs, rhs, rep_id, negated, next);
        continuation = try self.lowerExprInto(rhs, rhs_id, continuation);
        return try self.lowerExprInto(lhs, lhs_id, continuation);
    }

    fn lowerEqRepLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .in_progress => boxyLowerInvariant("in-progress boxy representation reached structural equality lowering"),
            .dynamic => boxyLowerInvariant("dynamic structural equality reached boxy lowering before descriptor equality support"),
            .erased_callable => boxyLowerInvariant("erased callable structural equality reached boxy lowering"),
            .list,
            .box,
            => boxyLowerInvariant("owned structural equality reached boxy lowering before dictionary equality support"),
            .primitive => |primitive| try self.lowerPrimitiveEqLocalsInto(target, lhs, rhs, primitive, negated, next),
            .bool_tag_union => try self.lowerBoolEqLocalsInto(target, lhs, rhs, negated, next),
            .empty_record,
            .empty_tag_union,
            => try self.assignBoolLiteral(target, !negated, next),
            .alias => try self.lowerEqRepLocalsInto(target, lhs, rhs, self.requiredSingleChild(rep_id, .alias_backing).rep, negated, next),
            .nominal => |kind| switch (kind) {
                .transparent => if (rep.declared_fields.len != 0)
                    try self.lowerNominalBackingEqLocalsInto(target, lhs, rhs, rep_id, negated, next)
                else
                    try self.lowerEqRepLocalsInto(target, lhs, rhs, self.requiredSingleChild(rep_id, .nominal_backing).rep, negated, next),
                .opaque_nominal => boxyLowerInvariant("opaque nominal structural equality reached boxy lowering before descriptor equality support"),
                .builtin_other => try self.lowerEqRepLocalsInto(target, lhs, rhs, self.requiredSingleChild(rep_id, .nominal_backing).rep, negated, next),
            },
            .record,
            .record_unbound,
            => try self.lowerRecordEqLocalsInto(target, lhs, rhs, rep, negated, next),
            .tuple => try self.lowerTupleEqLocalsInto(target, lhs, rhs, rep, negated, next),
            .tag_union => try self.lowerTagUnionEqLocalsInto(target, lhs, rhs, rep, negated, next),
        };
    }

    fn lowerNominalBackingEqLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing_rep = self.requiredSingleChild(rep_id, .nominal_backing).rep;
        const backing_layout = self.workerRuntimeLayoutForRep(backing_rep).layoutIdx();
        const lhs_backing = try self.addFrameLocal(backing_layout);
        const rhs_backing = try self.addFrameLocal(backing_layout);
        var continuation = try self.lowerEqRepLocalsInto(target, lhs_backing, rhs_backing, backing_rep, negated, next);
        continuation = try self.assignNominalBoundary(rhs_backing, rhs, continuation);
        return try self.assignNominalBoundary(lhs_backing, lhs, continuation);
    }

    fn lowerPrimitiveEqLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        primitive: checked.CheckedPrimitive,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (primitive == .bool) {
            return try self.lowerBoolEqLocalsInto(target, lhs, rhs, negated, next);
        }

        const eq_op: LIR.LowLevel = switch (primitive) {
            .str => .str_is_eq,
            .u8,
            .i8,
            .u16,
            .i16,
            .u32,
            .i32,
            .u64,
            .i64,
            .u128,
            .i128,
            .f32,
            .f64,
            .dec,
            => .num_is_eq,
            .bool => unreachable,
        };
        const args = [_]LIR.LocalId{ lhs, rhs };
        if (!negated) {
            return try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = eq_op,
                .rc_effect = eq_op.rcEffect(),
                .args = try self.parent.result.store.addLocalSpan(&args),
                .next = next,
            } });
        }

        const raw = try self.addFrameLocal(.bool);
        const not_stmt = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .bool_not,
            .rc_effect = LIR.LowLevel.bool_not.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{raw}),
            .next = next,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = raw,
            .op = eq_op,
            .rc_effect = eq_op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&args),
            .next = not_stmt,
        } });
    }

    fn lowerBoolEqLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const lhs_disc = try self.addFrameLocal(.u16);
        const rhs_disc = try self.addFrameLocal(.u16);
        const compare = try self.lowerPrimitiveEqLocalsInto(target, lhs_disc, rhs_disc, .u16, negated, next);
        const read_rhs = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = rhs_disc,
            .op = .{ .discriminant = .{ .source = rhs } },
            .next = compare,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = lhs_disc,
            .op = .{ .discriminant = .{ .source = lhs } },
            .next = read_rhs,
        } });
    }

    fn lowerRecordEqLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var current = try self.assignBoolLiteral(target, !negated, next);
        const failed = try self.assignBoolLiteral(target, negated, next);
        const children = self.parent.plan.childSlice(rep.children);
        var field_index = recordEqualityFieldCount(children);
        var i = children.len;
        while (i > 0) {
            i -= 1;
            const child = children[i];
            switch (child.role) {
                .record_field => {
                    field_index -= 1;
                    current = try self.lowerFieldEqStep(lhs, rhs, child.rep, field_index, current, failed);
                },
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("record equality representation had a non-record child role"),
            }
        }
        return current;
    }

    fn lowerTupleEqLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var current = try self.assignBoolLiteral(target, !negated, next);
        const failed = try self.assignBoolLiteral(target, negated, next);
        const children = self.parent.plan.childSlice(rep.children);
        var i = children.len;
        while (i > 0) {
            i -= 1;
            const child = children[i];
            switch (child.role) {
                .tuple_elem => |index| {
                    if (index > std.math.maxInt(u16)) {
                        boxyLowerInvariant("tuple equality element index exceeded LIR field index range");
                    }
                    current = try self.lowerFieldEqStep(lhs, rhs, child.rep, @intCast(index), current, failed);
                },
                else => boxyLowerInvariant("tuple equality representation had a non-tuple child role"),
            }
        }
        return current;
    }

    fn lowerFieldEqStep(
        self: *ProcBodyBuilder,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        field_rep: Plan.TypeRepId,
        field_index: u16,
        on_equal: LIR.CFStmtId,
        on_not_equal: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const field_layout = self.workerRuntimeLayoutForRep(field_rep).layoutIdx();
        const lhs_field = try self.addFrameLocal(field_layout);
        const rhs_field = try self.addFrameLocal(field_layout);
        const eq = try self.addFrameLocal(.bool);
        var current = try self.boolSwitchNoContinuation(eq, on_equal, on_not_equal);
        current = try self.lowerEqRepLocalsInto(eq, lhs_field, rhs_field, field_rep, false, current);
        if (!self.isZstLocal(rhs_field)) {
            current = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                .target = rhs_field,
                .op = .{ .field = .{
                    .source = rhs,
                    .field_idx = field_index,
                } },
                .next = current,
            } });
        }
        if (!self.isZstLocal(lhs_field)) {
            current = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                .target = lhs_field,
                .op = .{ .field = .{
                    .source = lhs,
                    .field_idx = field_index,
                } },
                .next = current,
            } });
        }
        return current;
    }

    fn lowerTagUnionEqLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        negated: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.isZstLocal(lhs) and self.isZstLocal(rhs)) {
            return try self.assignBoolLiteral(target, !negated, next);
        }

        const success = try self.assignBoolLiteral(target, !negated, next);
        const failed = try self.assignBoolLiteral(target, negated, next);

        const lhs_disc = try self.addFrameLocal(.u16);
        const rhs_disc = try self.addFrameLocal(.u16);
        const same_disc = try self.addFrameLocal(.bool);

        const variants = self.parent.plan.tagVariantSlice(rep.tag_variants);
        const branches = try self.parent.allocator.alloc(LIR.CFSwitchBranch, variants.len);
        defer self.parent.allocator.free(branches);
        for (variants, branches, 0..) |variant, *branch, index| {
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag-union equality variant index exceeded LIR variant range");
            }
            branch.* = .{
                .value = @intCast(index),
                .body = try self.lowerTagPayloadEqVariant(lhs, rhs, variant, @intCast(index), success, failed),
            };
        }

        const payload_switch = try self.parent.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = lhs_disc,
            .branches = try self.parent.result.store.addCFSwitchBranches(branches),
            .default_branch = failed,
            .continuation = null,
        } });
        const disc_switch = try self.boolSwitchNoContinuation(same_disc, payload_switch, failed);
        const compare_disc = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = same_disc,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{ lhs_disc, rhs_disc }),
            .next = disc_switch,
        } });
        const read_rhs = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = rhs_disc,
            .op = .{ .discriminant = .{ .source = rhs } },
            .next = compare_disc,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = lhs_disc,
            .op = .{ .discriminant = .{ .source = lhs } },
            .next = read_rhs,
        } });
    }

    fn lowerTagPayloadEqVariant(
        self: *ProcBodyBuilder,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        variant: Plan.TagVariant,
        variant_index: u16,
        on_equal: LIR.CFStmtId,
        on_not_equal: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        var current = on_equal;
        const payloads = self.parent.plan.childSlice(variant.payloads);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const child = payloads[i];
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != variant.name or payload.index != @as(u32, @intCast(i))) {
                        boxyLowerInvariant("tag equality payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag equality variant payload span included a non-payload child"),
            }
            const payload_layout = self.workerRuntimeLayoutForRep(child.rep).layoutIdx();
            const lhs_payload = try self.addFrameLocal(payload_layout);
            const rhs_payload = try self.addFrameLocal(payload_layout);
            const eq = try self.addFrameLocal(.bool);
            current = try self.boolSwitchNoContinuation(eq, current, on_not_equal);
            current = try self.lowerEqRepLocalsInto(eq, lhs_payload, rhs_payload, child.rep, false, current);

            if (i > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag equality payload index exceeded LIR payload index range");
            }
            const payload_idx: ?u16 = if (payloads.len == 1) null else @as(u16, @intCast(i));
            if (!self.isZstLocal(rhs_payload)) {
                current = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = rhs_payload,
                    .op = if (payload_idx) |index| .{ .tag_payload = .{
                        .source = rhs,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = rhs,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
            if (!self.isZstLocal(lhs_payload)) {
                current = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = lhs_payload,
                    .op = if (payload_idx) |index| .{ .tag_payload = .{
                        .source = lhs,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = lhs,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn lowerStructuralHashInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value_id: checked.CheckedExprId,
        hasher_id: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const value_expr = self.module.checked_bodies.expr(value_id);
        const hasher_expr = self.module.checked_bodies.expr(hasher_id);
        const value = try self.addFrameLocal(self.workerRuntimeLayoutForType(value_expr.ty).layoutIdx());
        const hasher = try self.addFrameLocal(self.workerRuntimeLayoutForType(hasher_expr.ty).layoutIdx());
        if (self.parent.result.store.getLocal(target).layout_idx != self.parent.result.store.getLocal(hasher).layout_idx) {
            boxyLowerInvariant("checked structural hash target layout differed from input hasher layout");
        }
        var continuation = try self.lowerHashRepLocalsInto(target, value, hasher, self.repForType(value_expr.ty), next);
        continuation = try self.lowerExprInto(hasher, hasher_id, continuation);
        return try self.lowerExprInto(value, value_id, continuation);
    }

    fn lowerHashRepLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .in_progress => boxyLowerInvariant("in-progress boxy representation reached structural hash lowering"),
            .dynamic => boxyLowerInvariant("dynamic structural hash reached boxy lowering before descriptor hash support"),
            .erased_callable => boxyLowerInvariant("erased callable structural hash reached boxy lowering"),
            .list,
            .box,
            => boxyLowerInvariant("owned structural hash reached boxy lowering before dictionary hash support"),
            .primitive => |primitive| try self.lowerPrimitiveHashLocalsInto(target, value, hasher, primitive, next),
            .bool_tag_union => try self.lowerPrimitiveHashLocalsInto(target, value, hasher, .bool, next),
            .empty_record,
            .empty_tag_union,
            => try self.assignLocal(target, hasher, next),
            .alias => try self.lowerHashRepLocalsInto(target, value, hasher, self.requiredSingleChild(rep_id, .alias_backing).rep, next),
            .nominal => |kind| switch (kind) {
                .transparent => if (rep.declared_fields.len != 0)
                    try self.lowerNominalBackingHashLocalsInto(target, value, hasher, rep_id, next)
                else
                    try self.lowerHashRepLocalsInto(target, value, hasher, self.requiredSingleChild(rep_id, .nominal_backing).rep, next),
                .opaque_nominal => boxyLowerInvariant("opaque nominal structural hash reached boxy lowering before descriptor hash support"),
                .builtin_other => try self.lowerHashRepLocalsInto(target, value, hasher, self.requiredSingleChild(rep_id, .nominal_backing).rep, next),
            },
            .record,
            .record_unbound,
            => try self.lowerRecordHashLocalsInto(target, value, hasher, rep, next),
            .tuple => try self.lowerTupleHashLocalsInto(target, value, hasher, rep, next),
            .tag_union => try self.lowerTagUnionHashLocalsInto(target, value, hasher, rep, next),
        };
    }

    fn lowerNominalBackingHashLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        rep_id: Plan.TypeRepId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const backing_rep = self.requiredSingleChild(rep_id, .nominal_backing).rep;
        const backing = try self.addFrameLocal(self.workerRuntimeLayoutForRep(backing_rep).layoutIdx());
        const hash = try self.lowerHashRepLocalsInto(target, backing, hasher, backing_rep, next);
        return try self.assignNominalBoundary(backing, value, hash);
    }

    fn lowerPrimitiveHashLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        primitive: checked.CheckedPrimitive,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const op = hasherWriteOp(primitive);
        return try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{ hasher, value }),
            .next = next,
        } });
    }

    const HashComponent = struct {
        rep: Plan.TypeRepId,
        field_index: u16,
    };

    fn lowerRecordHashLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const children = self.parent.plan.childSlice(rep.children);
        const components = try self.parent.allocator.alloc(HashComponent, recordEqualityFieldCount(children));
        defer self.parent.allocator.free(components);
        var field_index: u16 = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => {
                    components[field_index] = .{
                        .rep = child.rep,
                        .field_index = field_index,
                    };
                    field_index += 1;
                },
                .record_ext => self.requireEmptyRecordExtension(child.rep),
                else => boxyLowerInvariant("record hash representation had a non-record child role"),
            }
        }
        return try self.lowerFieldHashComponentsInto(target, value, hasher, components, next);
    }

    fn lowerTupleHashLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const children = self.parent.plan.childSlice(rep.children);
        const components = try self.parent.allocator.alloc(HashComponent, children.len);
        defer self.parent.allocator.free(components);
        for (children, components) |child, *component| {
            switch (child.role) {
                .tuple_elem => |index| {
                    if (index > std.math.maxInt(u16)) {
                        boxyLowerInvariant("tuple hash element index exceeded LIR field index range");
                    }
                    component.* = .{
                        .rep = child.rep,
                        .field_index = @intCast(index),
                    };
                },
                else => boxyLowerInvariant("tuple hash representation had a non-tuple child role"),
            }
        }
        return try self.lowerFieldHashComponentsInto(target, value, hasher, components, next);
    }

    fn lowerFieldHashComponentsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        components: []const HashComponent,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (components.len == 0) return try self.assignLocal(target, hasher, next);

        const hasher_layout = self.parent.result.store.getLocal(hasher).layout_idx;
        if (self.parent.result.store.getLocal(target).layout_idx != hasher_layout) {
            boxyLowerInvariant("structural hash target layout differed from input hasher layout");
        }

        const intermediate_count = components.len - 1;
        const intermediate_hashers = try self.parent.allocator.alloc(LIR.LocalId, intermediate_count);
        defer self.parent.allocator.free(intermediate_hashers);
        for (intermediate_hashers) |*local| {
            local.* = try self.addFrameLocal(hasher_layout);
        }

        var current = next;
        var i = components.len;
        while (i > 0) {
            i -= 1;
            const input_hasher = if (i == 0) hasher else intermediate_hashers[i - 1];
            const output_hasher = if (i == components.len - 1) target else intermediate_hashers[i];
            const component = components[i];
            const field = try self.addFrameLocal(self.workerRuntimeLayoutForRep(component.rep).layoutIdx());
            current = try self.lowerHashRepLocalsInto(output_hasher, field, input_hasher, component.rep, current);
            if (!self.isZstLocal(field)) {
                current = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = field,
                    .op = .{ .field = .{
                        .source = value,
                        .field_idx = component.field_index,
                    } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn lowerTagUnionHashLocalsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        rep: Plan.TypeRepresentation,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const variants = self.parent.plan.tagVariantSlice(rep.tag_variants);
        if (variants.len == 1 and self.isZstLocal(value)) {
            return try self.lowerTagPayloadHashVariant(target, value, hasher, variants[0], 0, next);
        }

        const discriminant = try self.addFrameLocal(.u16);
        const branches = try self.parent.allocator.alloc(LIR.CFSwitchBranch, variants.len);
        defer self.parent.allocator.free(branches);
        for (variants, branches, 0..) |variant, *branch, index| {
            if (index > std.math.maxInt(u16)) {
                boxyLowerInvariant("tag-union hash variant index exceeded LIR variant range");
            }
            branch.* = .{
                .value = @intCast(index),
                .body = try self.lowerTagPayloadHashVariant(target, value, hasher, variant, @intCast(index), next),
            };
        }
        const bad_discriminant = try self.parent.result.store.addCFStmt(.runtime_error);
        const switch_stmt = try self.parent.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = discriminant,
            .branches = try self.parent.result.store.addCFSwitchBranches(branches),
            .default_branch = bad_discriminant,
            .continuation = null,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = value } },
            .next = switch_stmt,
        } });
    }

    fn lowerTagPayloadHashVariant(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        variant: Plan.TagVariant,
        variant_index: u16,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const payloads = self.parent.plan.childSlice(variant.payloads);
        const hasher_layout = self.parent.result.store.getLocal(hasher).layout_idx;
        const after_discriminant = if (payloads.len == 0) target else try self.addFrameLocal(hasher_layout);

        const components = try self.parent.allocator.alloc(Plan.RepChild, payloads.len);
        defer self.parent.allocator.free(components);
        for (payloads, components, 0..) |child, *component, index| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (payload.tag != variant.name or payload.index != index) {
                        boxyLowerInvariant("tag hash payload span did not match its payload child roles");
                    }
                },
                else => boxyLowerInvariant("tag hash variant payload span included a non-payload child"),
            }
            component.* = child;
        }

        var current = if (payloads.len == 0)
            next
        else
            try self.lowerTagPayloadHashComponentsInto(target, value, after_discriminant, components, variant_index, next);

        const discriminant_value = try self.addFrameLocal(.u64);
        current = try self.parent.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = after_discriminant,
            .op = .hasher_write_u64,
            .rc_effect = LIR.LowLevel.hasher_write_u64.rcEffect(),
            .args = try self.parent.result.store.addLocalSpan(&[_]LIR.LocalId{ hasher, discriminant_value }),
            .next = current,
        } });
        return try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = discriminant_value,
            .value = .{ .i128_literal = .{
                .value = variant_index,
                .layout_idx = .u64,
            } },
            .next = current,
        } });
    }

    fn lowerTagPayloadHashComponentsInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        payloads: []const Plan.RepChild,
        variant_index: u16,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const hasher_layout = self.parent.result.store.getLocal(hasher).layout_idx;
        const intermediate_count = payloads.len - 1;
        const intermediate_hashers = try self.parent.allocator.alloc(LIR.LocalId, intermediate_count);
        defer self.parent.allocator.free(intermediate_hashers);
        for (intermediate_hashers) |*local| {
            local.* = try self.addFrameLocal(hasher_layout);
        }

        var current = next;
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const input_hasher = if (i == 0) hasher else intermediate_hashers[i - 1];
            const output_hasher = if (i == payloads.len - 1) target else intermediate_hashers[i];
            const payload = payloads[i];
            const payload_local = try self.addFrameLocal(self.workerRuntimeLayoutForRep(payload.rep).layoutIdx());
            current = try self.lowerHashRepLocalsInto(output_hasher, payload_local, input_hasher, payload.rep, current);
            if (!self.isZstLocal(payload_local)) {
                if (i > std.math.maxInt(u16)) {
                    boxyLowerInvariant("tag hash payload index exceeded LIR payload index range");
                }
                const payload_idx: ?u16 = if (payloads.len == 1) null else @as(u16, @intCast(i));
                current = try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = payload_local,
                    .op = if (payload_idx) |index| .{ .tag_payload = .{
                        .source = value,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = value,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn lowerUnaryLowLevelInto(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        child: checked.CheckedExprId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const args = [_]checked.CheckedExprId{child};
        return try self.lowerLowLevelInto(target, op, &args, next);
    }

    fn joinJump(self: *ProcBodyBuilder, join_id: LIR.JoinPointId) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    }

    fn freshJoinPointId(self: *ProcBodyBuilder) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
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

    fn assignConstScalar(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        scalar: checked.ConstScalar,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return switch (scalar) {
            .i8 => |value| try self.assignIntLiteral(target, value, next),
            .i16 => |value| try self.assignIntLiteral(target, value, next),
            .i32 => |value| try self.assignIntLiteral(target, value, next),
            .i64 => |value| try self.assignIntLiteral(target, value, next),
            .i128 => |value| try self.assignIntLiteral(target, value, next),
            .u8 => |value| try self.assignIntLiteral(target, value, next),
            .u16 => |value| try self.assignIntLiteral(target, value, next),
            .u32 => |value| try self.assignIntLiteral(target, value, next),
            .u64 => |value| try self.assignIntLiteral(target, value, next),
            .u128 => |value| try self.assignIntLiteral(target, @bitCast(value), next),
            .f32_bits => |bits| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = @bitCast(bits) },
                .next = next,
            } }),
            .f64_bits => |bits| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = @bitCast(bits) },
                .next = next,
            } }),
            .dec_bits => |bits| try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .dec_literal = bits },
                .next = next,
            } }),
        };
    }

    fn assignStringLiteral(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        literal_id: checked.CheckedStringLiteralId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const text = self.module.checked_bodies.stringLiteral(literal_id);
        return try self.assignStringBytesLiteral(target, text, next);
    }

    fn lirStrLiteral(
        self: *ProcBodyBuilder,
        literal_id: checked.CheckedStringLiteralId,
    ) Allocator.Error!LIR.StrLiteral {
        const text = self.module.checked_bodies.stringLiteral(literal_id);
        return try self.parent.result.store.insertStringView(text, 0, @intCast(text.len));
    }

    fn assignStringBytesLiteral(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        text: []const u8,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.assignStringBytesView(target, text, 0, @intCast(text.len), next);
    }

    fn assignStringBytesView(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        backing: []const u8,
        offset: u32,
        len: u32,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.parent.result.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .str_literal = try self.parent.result.store.insertStringView(backing, offset, len) },
            .next = next,
        } });
    }

    fn assignBoolLiteral(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        value: bool,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const variant: u16 = if (value) 1 else 0;
        return try self.parent.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = variant,
            .discriminant = variant,
            .payload = null,
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

    fn assignNominalBoundary(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const target_layout = self.parent.result.store.getLocal(target).layout_idx;
        const source_layout = self.parent.result.store.getLocal(source).layout_idx;
        if (target_layout == source_layout) {
            return if (target == source) next else try self.assignLocal(target, source, next);
        }

        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(source)) {
                boxyLowerInvariant("nominal boundary tried to store non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }

        return try self.parent.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .nominal = .{ .backing_ref = source } },
            .next = next,
        } });
    }

    fn setLocalReplace(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.isZstLocal(target)) return next;
        if (self.parent.result.store.getLocal(target).layout_idx != self.parent.result.store.getLocal(source).layout_idx) {
            boxyLowerInvariant("boxy reassignment required explicit box/adapt lowering before set_local");
        }
        return try self.parent.result.store.addCFStmt(.{ .set_local = .{
            .target = target,
            .value = source,
            .mode = .replace_existing,
            .next = next,
        } });
    }

    fn setLocalInitializeJoinParam(
        self: *ProcBodyBuilder,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        if (self.parent.result.store.getLocal(target).layout_idx != self.parent.result.store.getLocal(source).layout_idx) {
            boxyLowerInvariant("boxy join parameter initialization required matching layouts");
        }
        return try self.parent.result.store.addCFStmt(.{ .set_local = .{
            .target = target,
            .value = source,
            .mode = .initialize_join_param,
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
            .list => |list| {
                if (list.patterns.len != 0) {
                    boxyLowerInvariant("refutable list pattern reached boxy irrefutable binder reservation");
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| try self.reservePatternBindings(rest_pattern);
                }
            },
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
            .runtime_error,
            .pending,
            => boxyLowerInvariant("refutable or pending checked pattern reached boxy irrefutable binder reservation"),
        }
    }

    fn reserveMatchPatternBindings(self: *ProcBodyBuilder, pattern_id: checked.CheckedPatternId) Allocator.Error!void {
        try self.ensureBinderLocals();
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => |binder| try self.reserveBinderLocal(binder, pattern.ty),
            .as => |as| {
                try self.reserveBinderLocal(as.binder, pattern.ty);
                try self.reserveMatchPatternBindings(as.pattern);
            },
            .applied_tag => |tag| for (tag.args) |arg| try self.reserveMatchPatternBindings(arg),
            .tuple => |items| for (items) |item| try self.reserveMatchPatternBindings(item),
            .record_destructure => |destructs| {
                for (destructs) |destruct| {
                    switch (destruct.kind) {
                        .required,
                        .sub_pattern,
                        .rest,
                        => |child| try self.reserveMatchPatternBindings(child),
                    }
                }
            },
            .nominal => |nominal| try self.reserveMatchPatternBindings(nominal.backing_pattern),
            .list => |list| {
                for (list.patterns) |child| try self.reserveMatchPatternBindings(child);
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| try self.reserveMatchPatternBindings(rest_pattern);
                }
            },
            .str_interpolation => |str| {
                for (str.steps) |step| {
                    if (step.capture) |capture| try self.reserveMatchPatternBindings(capture);
                }
            },
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => {},
            .runtime_error,
            .pending,
            => boxyLowerInvariant("runtime-error or pending checked pattern reached boxy match binder reservation"),
        }
    }

    fn patternCanMiss(self: *ProcBodyBuilder, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign,
            .underscore,
            => false,
            .as => |as| self.patternCanMiss(as.pattern),
            .tuple => |items| blk: {
                for (items) |item| {
                    if (self.patternCanMiss(item)) break :blk true;
                }
                break :blk false;
            },
            .record_destructure => |destructs| blk: {
                for (destructs) |destruct| {
                    const child = switch (destruct.kind) {
                        .required,
                        .sub_pattern,
                        .rest,
                        => |child| child,
                    };
                    if (self.patternCanMiss(child)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |nominal| self.patternCanMiss(nominal.backing_pattern),
            .applied_tag => |tag| self.appliedTagPatternCanMiss(pattern.ty, tag.name, tag.args),
            .list,
            .str_interpolation,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            => true,
            .runtime_error,
            .pending,
            => boxyLowerInvariant("runtime-error or pending checked pattern reached boxy match miss analysis"),
        };
    }

    fn patternIsIgnored(self: *ProcBodyBuilder, pattern_id: checked.CheckedPatternId) bool {
        return switch (self.module.checked_bodies.pattern(pattern_id).data) {
            .underscore => true,
            else => false,
        };
    }

    fn appliedTagPatternCanMiss(
        self: *ProcBodyBuilder,
        tag_ty: checked.CheckedTypeId,
        name: names.TagNameId,
        args: []const checked.CheckedPatternId,
    ) bool {
        var payload_can_miss = false;
        for (args) |arg| {
            if (self.patternCanMiss(arg)) {
                payload_can_miss = true;
                break;
            }
        }

        const rep_id = self.repForType(tag_ty);
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .bool_tag_union => blk: {
                if (args.len != 0) {
                    boxyLowerInvariant("builtin Bool match pattern carried a payload during miss analysis");
                }
                _ = self.boolVariantIndex(name);
                break :blk true;
            },
            .tag_union => blk: {
                const variants = self.parent.plan.tagVariantSlice(rep.tag_variants);
                const variant = self.tagVariant(rep, name);
                const payloads = self.parent.plan.childSlice(variant.payloads);
                if (payloads.len != args.len) {
                    boxyLowerInvariant("tag match pattern payload count disagreed during miss analysis");
                }
                break :blk variants.len > 1 or payload_can_miss;
            },
            .empty_tag_union => boxyLowerInvariant("empty tag-union match pattern reached boxy miss analysis"),
            else => boxyLowerInvariant("tag match pattern checked type did not have a boxy tag-union representation during miss analysis"),
        };
    }

    fn reserveReassignPatternBindings(self: *ProcBodyBuilder, pattern_id: checked.CheckedPatternId) Allocator.Error!void {
        try self.ensureBinderLocals();
        const pattern = self.module.checked_bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => |binder| try self.reserveBinderLocalIfFresh(binder, pattern.ty),
            .as => |as| {
                try self.reserveBinderLocalIfFresh(as.binder, pattern.ty);
                try self.reserveReassignPatternBindings(as.pattern);
            },
            .tuple => |items| for (items) |item| try self.reserveReassignPatternBindings(item),
            .record_destructure => |destructs| {
                for (destructs) |destruct| {
                    switch (destruct.kind) {
                        .required,
                        .sub_pattern,
                        .rest,
                        => |child| try self.reserveReassignPatternBindings(child),
                    }
                }
            },
            .nominal => |nominal| try self.reserveReassignPatternBindings(nominal.backing_pattern),
            .list => |list| {
                if (list.patterns.len != 0) {
                    boxyLowerInvariant("refutable list pattern reached boxy reassign binder reservation");
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| try self.reserveReassignPatternBindings(rest_pattern);
                }
            },
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
            .runtime_error,
            .pending,
            => boxyLowerInvariant("refutable or pending checked pattern reached boxy reassign binder reservation"),
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

    fn reserveBinderLocalIfFresh(
        self: *ProcBodyBuilder,
        binder: checked.PatternBinderId,
        ty: checked.CheckedTypeId,
    ) Allocator.Error!void {
        if (self.binderLocalOrNull(binder) != null) return;
        try self.reserveBinderLocal(binder, ty);
    }

    fn bindLocal(self: *ProcBodyBuilder, binder: checked.PatternBinderId, local: LIR.LocalId) void {
        const index = @intFromEnum(binder);
        if (index >= self.binder_locals.len) boxyLowerInvariant("boxy pattern referenced a missing pattern binder");
        if (self.binder_locals[index] != null) boxyLowerInvariant("boxy pattern bound the same pattern binder more than once");
        self.binder_locals[index] = local;
    }

    fn binderLocalOrNull(self: *const ProcBodyBuilder, binder: checked.PatternBinderId) ?LIR.LocalId {
        const index = @intFromEnum(binder);
        if (index >= self.binder_locals.len) boxyLowerInvariant("boxy pattern referenced a missing pattern binder");
        return self.binder_locals[index];
    }

    fn binderIsReassigned(
        self: *const ProcBodyBuilder,
        reassigned_binders: []const checked.PatternBinderId,
        binder: checked.PatternBinderId,
    ) bool {
        _ = self;
        for (reassigned_binders) |reassigned| {
            if (reassigned == binder) return true;
        }
        return false;
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

    fn resolvedValueRecord(self: *const ProcBodyBuilder, ref_id: checked.ResolvedValueRefId) checked.ResolvedValueRefRecord {
        const raw = @intFromEnum(ref_id);
        if (raw >= self.module.resolved_value_refs.records.len) {
            boxyLowerInvariant("checked lookup referenced a missing resolved value");
        }
        return self.module.resolved_value_refs.records[raw];
    }

    fn workerReturnLayout(self: *const ProcBodyBuilder) @import("layout").Idx {
        if (self.worker_layout.ret) |ret| return ret.layoutIdx();
        return self.worker_layout.value.layoutIdx();
    }

    fn repForType(self: *const ProcBodyBuilder, ty: checked.CheckedTypeId) Plan.TypeRepId {
        return self.parent.plan.repForSourceType(.{ .module = self.module.key, .ty = ty }) orelse
            boxyLowerInvariant("checked body referenced a type missing from the boxy representation plan");
    }

    fn workerRuntimeLayoutForType(self: *const ProcBodyBuilder, ty: checked.CheckedTypeId) Layouts.RuntimeLayout {
        return self.parent.layout_plan.rep_layouts[@intFromEnum(self.repForType(ty))].worker;
    }

    fn workerRuntimeLayoutForRep(self: *const ProcBodyBuilder, rep_id: Plan.TypeRepId) Layouts.RuntimeLayout {
        return self.parent.layout_plan.rep_layouts[@intFromEnum(rep_id)].worker;
    }

    fn functionReturnRepForType(self: *const ProcBodyBuilder, ty: checked.CheckedTypeId) Plan.TypeRepId {
        return self.functionReturnRepForRep(self.repForType(ty));
    }

    fn functionReturnRepForRep(self: *const ProcBodyBuilder, rep_id: Plan.TypeRepId) Plan.TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyLowerInvariant("function representation alias chain exceeded boxy lowerer limit");
            depth += 1;

            const rep = self.parent.plan.representations.items[@intFromEnum(current)];
            switch (rep.kind) {
                .alias => current = self.requiredSingleChild(current, .alias_backing).rep,
                .nominal => |kind| switch (kind) {
                    .transparent => current = self.requiredSingleChild(current, .nominal_backing).rep,
                    .opaque_nominal, .builtin_other => boxyLowerInvariant("opaque or unsupported nominal reached function return layout lowering"),
                },
                .erased_callable => {
                    for (self.parent.plan.childSlice(rep.children)) |child| {
                        switch (child.role) {
                            .function_ret => return child.rep,
                            else => {},
                        }
                    }
                    boxyLowerInvariant("function representation had no return child");
                },
                else => boxyLowerInvariant("list_map_can_reuse transform argument is not a function"),
            }
        }
    }

    fn requiredSingleChild(self: *const ProcBodyBuilder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) Plan.RepChild {
        var found: ?Plan.RepChild = null;
        const rep = self.parent.plan.representations.items[@intFromEnum(rep_id)];
        for (self.parent.plan.childSlice(rep.children)) |child| {
            if (sameChildRole(child.role, role)) {
                if (found != null) boxyLowerInvariant("representation had duplicate required child role");
                found = child;
            }
        }
        return found orelse boxyLowerInvariant("representation was missing required child role");
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

    fn recordEqualityFieldCount(children: []const Plan.RepChild) u16 {
        var count: usize = 0;
        for (children) |child| {
            switch (child.role) {
                .record_field => count += 1,
                .record_ext => {},
                else => boxyLowerInvariant("record equality representation had a non-record child role"),
            }
        }
        if (count > std.math.maxInt(u16)) {
            boxyLowerInvariant("record equality field count exceeded LIR field index range");
        }
        return @intCast(count);
    }

    fn hasherWriteOp(primitive: checked.CheckedPrimitive) LIR.LowLevel {
        return switch (primitive) {
            .bool => .hasher_write_bool,
            .str => .hasher_write_str,
            .u8 => .hasher_write_u8,
            .i8 => .hasher_write_i8,
            .u16 => .hasher_write_u16,
            .i16 => .hasher_write_i16,
            .u32 => .hasher_write_u32,
            .i32 => .hasher_write_i32,
            .u64 => .hasher_write_u64,
            .i64 => .hasher_write_i64,
            .u128 => .hasher_write_u128,
            .i128 => .hasher_write_i128,
            .f32 => .hasher_write_f32,
            .f64 => .hasher_write_f64,
            .dec => .hasher_write_dec,
        };
    }

    fn primitiveInspectLowLevelOp(primitive: checked.CheckedPrimitive) LIR.LowLevel {
        return switch (primitive) {
            .bool => boxyLowerInvariant("Bool inspect must lower through tag-union inspect"),
            .str => .str_inspect,
            .u8 => .u8_to_str,
            .i8 => .i8_to_str,
            .u16 => .u16_to_str,
            .i16 => .i16_to_str,
            .u32 => .u32_to_str,
            .i32 => .i32_to_str,
            .u64 => .u64_to_str,
            .i64 => .i64_to_str,
            .u128 => .u128_to_str,
            .i128 => .i128_to_str,
            .f32 => .f32_to_str,
            .f64 => .f64_to_str,
            .dec => .dec_to_str,
        };
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

fn constListElemType(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypeId {
    const nominal = resolvedNominalPayload(module, checked_ty);
    if (nominal.builtin != .list or nominal.args.len != 1) {
        boxyLowerInvariant("ConstStore list restored with a non-list checked type");
    }
    return nominal.args[0];
}

fn constBoxPayloadType(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypeId {
    const nominal = resolvedNominalPayload(module, checked_ty);
    if (nominal.builtin != .box or nominal.args.len != 1) {
        boxyLowerInvariant("ConstStore box restored with a non-Box checked type");
    }
    return nominal.args[0];
}

fn checkedFunctionPayload(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedFunctionType {
    return switch (resolvedTypePayload(module, checked_ty)) {
        .function => |function| function,
        else => boxyLowerInvariant("checked intrinsic wrapper did not have a function type"),
    };
}

fn checkedTypeUsesBuiltinStructuralEquality(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) bool {
    return switch (resolvedTypePayload(module, checked_ty)) {
        .nominal => |nominal| nominal.builtin != null,
        .record,
        .record_unbound,
        .tuple,
        .empty_record,
        .tag_union,
        .empty_tag_union,
        => true,
        .pending,
        .flex,
        .rigid,
        .alias,
        .function,
        => false,
    };
}

fn constNominalBackingType(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypeId {
    return resolvedNominalPayload(module, checked_ty).backing;
}

fn constTupleItemTypes(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) []const checked.CheckedTypeId {
    return switch (resolvedTypePayload(module, checked_ty)) {
        .tuple => |items| items,
        else => boxyLowerInvariant("ConstStore tuple restored with a non-tuple checked type"),
    };
}

fn constRecordFields(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) []const checked.CheckedRecordField {
    return switch (resolvedTypePayload(module, checked_ty)) {
        .record => |record| blk: {
            constRecordExtensionIsEmpty(module, record.ext);
            break :blk record.fields;
        },
        .record_unbound => |fields| fields,
        .empty_record => &.{},
        else => boxyLowerInvariant("ConstStore record restored with a non-record checked type"),
    };
}

fn constRecordExtensionIsEmpty(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) void {
    switch (resolvedTypePayload(module, checked_ty)) {
        .empty_record => {},
        else => boxyLowerInvariant("ConstStore record restored with an open checked row"),
    }
}

fn constRecordFieldIndex(fields: []const checked.CheckedRecordField, label: names.RecordFieldLabelId) ?usize {
    for (fields, 0..) |field, index| {
        if (field.name == label) return index;
    }
    return null;
}

const ConstTagPayloadTypes = struct {
    name: names.TagNameId,
    payload_tys: []const checked.CheckedTypeId,
};

fn constTagPayloadTypes(
    module: ProcedureModuleView,
    checked_ty: checked.CheckedTypeId,
    tag_name: []const u8,
) ConstTagPayloadTypes {
    const tag_union = switch (resolvedTypePayload(module, checked_ty)) {
        .tag_union => |tag_union| tag_union,
        else => boxyLowerInvariant("ConstStore tag restored with a non-tag-union checked type"),
    };
    constRecordExtensionIsEmpty(module, tag_union.ext);
    for (tag_union.tags) |tag| {
        if (std.mem.eql(u8, module.canonical_names.tagLabelText(tag.name), tag_name)) {
            return .{
                .name = tag.name,
                .payload_tys = tag.argsSlice(module.checked_types),
            };
        }
    }
    boxyLowerInvariant("ConstStore tag name was missing from checked tag-union type");
}

fn resolvedNominalPayload(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedNominalType {
    return switch (resolvedTypePayload(module, checked_ty)) {
        .nominal => |nominal| nominal,
        else => boxyLowerInvariant("ConstStore nominal child lookup reached a non-nominal checked type"),
    };
}

fn resolvedTypePayload(module: ProcedureModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypePayload {
    var current = checked_ty;
    var depth: u16 = 0;
    while (true) {
        if (depth == 1024) boxyLowerInvariant("checked type alias chain exceeded boxy const lowering limit");
        depth += 1;

        switch (module.checked_types.payload(current)) {
            .pending => boxyLowerInvariant("pending checked type reached boxy const lowering"),
            .alias => |alias| {
                current = alias.backing;
                continue;
            },
            else => |payload| return payload,
        }
    }
}

fn appendRequestedLayouts(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    plan: *const Plan.ProgramPlan,
    layout_plan: *const Layouts.LayoutPlan,
    result: *LirProgram.Result,
) Allocator.Error!void {
    const request_count = roots.layout_requests.len + roots.static_data_requests.len;
    if (request_count == 0) return;
    const root_rep_start = plan.roots.items.len;
    if (plan.root_reps.items.len < root_rep_start + request_count) {
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
    for (roots.static_data_requests, 0..) |request, index| {
        const checked_type = request.data.checked_type;
        const rep_id = plan.root_reps.items[root_rep_start + roots.layout_requests.len + index];
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
                        .module = moduleDigestFromId(rep.source_type.module),
                        .ty = rep.source_type.ty,
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

fn expectResolvedWorkerCheckedExpr(
    worker: ResolvedWorker,
    expected_body: ?checked.CheckedBodyId,
    expected_root: checked.CheckedExprId,
) !void {
    const body = switch (worker.body) {
        .checked_expr => |checked_body| checked_body,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(expected_body, body.body_id);
    try std.testing.expectEqual(expected_root, body.root_expr);
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
        .root_request = dummyRootRequest(),
        .source = .{ .procedure_template = template_ref },
        .checked_type = .{ .ty = @enumFromInt(9) },
        .rep = @enumFromInt(0),
    });

    var resolved = try ResolvedWorkers.init(gpa, .{ .root = .{ .module = &artifact, .roots = undefined } }, &plan);
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expectEqual(@as(Plan.WorkerPlanId, @enumFromInt(0)), resolved.items[0].worker);
    try std.testing.expect(names.procedureTemplateRefEql(template_ref, resolved.items[0].template_ref));
    try expectResolvedWorkerCheckedExpr(resolved.items[0], @enumFromInt(0), @enumFromInt(3));
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
        .root_request = dummyRootRequest(),
        .source = .{ .procedure_binding = .{ .artifact = artifact.key, .binding = @enumFromInt(0) } },
        .checked_type = .{ .ty = @enumFromInt(7) },
        .rep = @enumFromInt(0),
    });

    var resolved = try ResolvedWorkers.init(gpa, .{ .root = .{ .module = &artifact, .roots = undefined } }, &plan);
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expect(names.procedureTemplateRefEql(template_ref, resolved.items[0].template_ref));
    try expectResolvedWorkerCheckedExpr(resolved.items[0], @enumFromInt(0), @enumFromInt(5));
}

test "boxy lowerer resolves callable eval bindings to finalized const function expressions" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);
    artifact.const_store = check.ConstStore.ConstStore.init(gpa);
    defer artifact.const_store.deinit();

    const template_ref = procedureTemplateRef(artifact.key, 0);
    var callable_templates = [_]checked.CallableEvalTemplate{
        .{
            .id = @enumFromInt(0),
            .module_idx = 0,
            .pattern = @enumFromInt(0),
            .root = @enumFromInt(0),
            .source_scheme = typeSchemeKey(1),
            .checked_fn_root = @enumFromInt(1),
        },
    };
    artifact.callable_eval_templates = .{ .templates = &callable_templates };

    const fn_id = try artifact.const_store.appendFn(.{
        .fn_def = .{ .nested = .{
            .owner = template_ref,
            .site = @enumFromInt(0),
        } },
        .source_fn_ty = @enumFromInt(1),
        .source_fn_key = typeKey(1),
    });
    var compile_time_roots = [_]checked.CompileTimeRoot{
        .{
            .id = @enumFromInt(0),
            .module_idx = 0,
            .kind = .callable_binding,
            .source = .{ .def = @enumFromInt(0) },
            .pattern = @enumFromInt(0),
            .expr = @enumFromInt(0),
            .checked_type = @enumFromInt(1),
            .payload = .{ .fn_value = fn_id },
        },
    };
    artifact.compile_time_roots = .{ .roots = &compile_time_roots };

    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    var nested_sites = [_]checked.NestedProcSite{
        .{
            .site = @enumFromInt(0),
            .owner_template = template_ref,
            .path_start = 0,
            .path_len = 0,
            .kind = .local_function,
            .checked_expr = @enumFromInt(0),
            .checked_pattern = null,
        },
    };
    artifact.nested_proc_sites = .{ .sites = &nested_sites };

    var templates = [_]checked.CheckedProcedureTemplate{
        .{
            .proc_base = template_ref.proc_base,
            .template_id = template_ref.template,
            .body = .{ .entry_wrapper = @enumFromInt(0) },
            .checked_fn_scheme = typeSchemeKey(2),
            .checked_fn_root = @enumFromInt(2),
            .static_dispatch_plans = .{},
            .resolved_value_refs = .{},
            .top_level_value_uses = .{},
            .nested_proc_sites = .{},
            .target = .comptime_only,
        },
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    var bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = typeSchemeKey(1),
            .body = .{ .callable_eval_template = @enumFromInt(0) },
        },
    };
    artifact.top_level_procedure_bindings = .{ .bindings = &bindings };

    var plan = Plan.ProgramPlan.init(gpa);
    defer plan.deinit();
    try plan.workers.append(gpa, .{
        .id = @enumFromInt(0),
        .root_request = dummyRootRequest(),
        .source = .{ .procedure_binding = .{ .artifact = artifact.key, .binding = @enumFromInt(0) } },
        .checked_type = .{ .ty = @enumFromInt(1) },
        .rep = @enumFromInt(0),
    });

    var resolved = try ResolvedWorkers.init(gpa, .{ .root = .{ .module = &artifact, .roots = undefined } }, &plan);
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expect(names.procedureTemplateRefEql(template_ref, resolved.items[0].template_ref));
    try expectResolvedWorkerCheckedExpr(resolved.items[0], null, @enumFromInt(0));
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

test "boxy lowerer restores top-level consts from resolved local lookups" {
    try expectBoxyTopLevelConstLookup(.local);
}

test "boxy lowerer restores top-level consts from resolved external lookups" {
    try expectBoxyTopLevelConstLookup(.external);
}

const ConstLookupExprKind = enum {
    local,
    external,
};

fn expectBoxyTopLevelConstLookup(kind: ConstLookupExprKind) !void {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);
    artifact.const_templates = .{};
    defer artifact.const_templates.deinit(gpa);
    artifact.const_store = check.ConstStore.ConstStore.init(gpa);
    defer artifact.const_store.deinit();

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

    const const_ref = try artifact.const_templates.reserveTopLevel(
        gpa,
        artifact.key,
        0,
        @enumFromInt(0),
        typeSchemeKey(7),
    );
    const const_node = try artifact.const_store.append(.{ .scalar = .{ .u64 = 5 } });
    artifact.const_templates.fillStoredConst(const_ref, .{ .node = const_node });

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
        .data = switch (kind) {
            .local => .{ .lookup_local = .{
                .pattern = @enumFromInt(0),
                .resolved = @enumFromInt(0),
            } },
            .external => .{ .lookup_external = @enumFromInt(0) },
        },
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

    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(1),
            .ref = .{ .top_level_const = .{
                .const_ref = const_ref,
                .requested_source_ty_template = canonicalTypeKey(1),
                .requested_source_ty_payload = @enumFromInt(0),
            } },
            .checked_ty = @enumFromInt(0),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
    };
    artifact.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

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
    const assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (assign.value) {
        .i128_literal => |literal| {
            try std.testing.expectEqual(@as(i128, 5), literal.value);
            try std.testing.expectEqual(@as(@TypeOf(literal.layout_idx), .u64), literal.layout_idx);
        },
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = assign.target } }, out.lir_result.store.getCFStmt(assign.next));
}

test "boxy lowerer emits small decimal expressions as Dec literals" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const value = can.CIR.SmallDecValue{ .numerator = 314, .denominator_power_of_ten = 2 };

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.dec, @enumFromInt(0), .{}),
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
        .data = .{ .dec_small = .{ .value = value, .has_suffix = false } },
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
    const literal = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (literal.value) {
        .dec_literal => |dec| try std.testing.expectEqual(value.toRocDec().num, dec),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = literal.target } }, out.lir_result.store.getCFStmt(literal.next));
}

test "boxy lowerer emits direct calls to planned private workers" {
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
        .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
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
    try artifact.checked_bodies.pattern_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.expr_id_pool.append(gpa, @enumFromInt(3));

    const root_template = procedureTemplateRef(artifact.key, 0);
    const callee_template = procedureTemplateRef(artifact.key, 1);
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
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{ .start = 0, .len = 1 },
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(1),
            .direct_target = @enumFromInt(0),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_external = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(41), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{ .start = 0, .len = 1 }, .body = @enumFromInt(5) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = root_template,
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(1),
        .root_expr = @enumFromInt(4),
        .owner_template = callee_template,
    });

    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(root_template, @enumFromInt(2), @enumFromInt(0)),
        checkedTemplate(callee_template, @enumFromInt(1), @enumFromInt(1)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    var bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = typeSchemeKey(1),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(callee_template),
                .template = .{ .checked = callee_template },
            } },
        },
    };
    artifact.top_level_procedure_bindings = .{ .bindings = &bindings };

    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .top_level_proc = .{
                .binding = .{ .top_level = .{ .artifact = artifact.key, .binding = @enumFromInt(0) } },
                .source_fn_ty_template = canonicalTypeKey(1),
                .source_fn_ty_payload = @enumFromInt(1),
            } },
            .checked_ty = @enumFromInt(1),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
        null,
        null,
        null,
    };
    artifact.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(2),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = root_template,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 2), plan.workers.items.len);
    const callee_worker = plan.directWorkerForCall(.{ .module = artifact.key, .expr = @enumFromInt(1) }) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(
        Plan.WorkerSource{ .procedure_binding = .{ .artifact = artifact.key, .binding = @enumFromInt(0) } },
        plan.workers.items[@intFromEnum(callee_worker)].source,
    );

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

    const root_proc_id = out.lir_result.root_procs.items[0];
    const root_proc = out.lir_result.store.getProcSpec(root_proc_id);
    const arg = out.lir_result.store.getCFStmt(root_proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (arg.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 41), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const call = out.lir_result.store.getCFStmt(arg.next).assign_call;
    try std.testing.expect(call.proc != root_proc_id);
    const call_args = out.lir_result.store.getLocalSpan(call.args);
    try std.testing.expectEqual(@as(usize, 1), call_args.len);
    try std.testing.expectEqual(arg.target, call_args[0]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = call.target } }, out.lir_result.store.getCFStmt(call.next));

    const callee_proc = out.lir_result.store.getProcSpec(call.proc);
    const callee_args = out.lir_result.store.getLocalSpan(callee_proc.args);
    try std.testing.expectEqual(@as(usize, 1), callee_args.len);
    const callee_copy = out.lir_result.store.getCFStmt(callee_proc.body orelse return error.TestUnexpectedResult).assign_ref;
    switch (callee_copy.op) {
        .local => |local| try std.testing.expectEqual(callee_args[0], local),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = callee_copy.target } }, out.lir_result.store.getCFStmt(callee_copy.next));
}

test "boxy lowerer emits direct calls to planned imported workers" {
    const gpa = std.testing.allocator;

    var root_artifact = minimalCheckedArtifact(gpa);
    defer root_artifact.canonical_names.deinit();
    defer root_artifact.checked_types.deinit(gpa);
    defer root_artifact.checked_bodies.deinit(gpa);

    var import_artifact = minimalCheckedArtifact(gpa);
    import_artifact.key = moduleKey(2);
    defer import_artifact.canonical_names.deinit();
    defer import_artifact.checked_types.deinit(gpa);
    defer import_artifact.checked_bodies.deinit(gpa);

    try import_artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try import_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const import_template = procedureTemplateRef(import_artifact.key, 0);
    const import_helper_template = procedureTemplateRef(import_artifact.key, 1);
    try import_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try import_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{},
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(1),
            .direct_target = @enumFromInt(0),
        } },
    });
    try import_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_external = @enumFromInt(0) },
    });
    try import_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(4) } },
    });
    try import_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
    });
    try import_artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = import_template,
    });
    try import_artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(1),
        .root_expr = @enumFromInt(3),
        .owner_template = import_helper_template,
    });
    var import_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(import_template, @enumFromInt(1), @enumFromInt(0)),
        checkedTemplate(import_helper_template, @enumFromInt(1), @enumFromInt(1)),
    };
    import_artifact.checked_procedure_templates = .{ .templates = &import_templates };
    var import_bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = typeSchemeKey(3),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(import_helper_template),
                .template = .{ .checked = import_helper_template },
            } },
        },
    };
    import_artifact.top_level_procedure_bindings = .{ .bindings = &import_bindings };
    const import_helper_use = checked.ProcedureUseTemplate{
        .binding = .{ .top_level = .{ .artifact = import_artifact.key, .binding = @enumFromInt(0) } },
        .source_fn_ty_template = canonicalTypeKey(3),
        .source_fn_ty_payload = @enumFromInt(1),
    };
    var import_resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .top_level_proc = import_helper_use },
            .checked_ty = @enumFromInt(1),
            .scope_depth = 0,
        },
    };
    var import_refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
        null,
        null,
    };
    import_artifact.resolved_value_refs = .{
        .records = &import_resolved_records,
        .by_checked_expr = &import_refs_by_expr,
    };

    const imported_binding = checked.ImportedProcedureBindingRef{
        .artifact = import_artifact.key,
        .def = @enumFromInt(7),
        .pattern = @enumFromInt(0),
    };
    var exported_bindings = [_]checked.ImportedProcedureBindingView{
        .{
            .binding = imported_binding,
            .source_scheme = typeSchemeKey(2),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(import_template),
                .template = .{ .checked = import_template },
            } },
            .template_closure = .{},
        },
    };
    import_artifact.exported_procedure_bindings = .{ .bindings = &exported_bindings };

    try root_artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try root_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const root_template = procedureTemplateRef(root_artifact.key, 0);
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{},
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(1),
            .direct_target = @enumFromInt(0),
        } },
    });
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_external = @enumFromInt(0) },
    });
    try root_artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = root_template,
    });

    var root_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(root_template, @enumFromInt(1), @enumFromInt(0)),
    };
    root_artifact.checked_procedure_templates = .{ .templates = &root_templates };

    const imported_use = checked.ProcedureUseTemplate{
        .binding = .{ .imported = imported_binding },
        .source_fn_ty_template = canonicalTypeKey(2),
        .source_fn_ty_payload = @enumFromInt(1),
    };
    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .imported_proc = imported_use },
            .checked_ty = @enumFromInt(1),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
    };
    root_artifact.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const imports = [_]checked.ImportedModuleView{
        .{
            .key = import_artifact.key,
            .module_env = undefined,
            .canonical_names = &import_artifact.canonical_names,
            .module_identity = undefined,
            .exports = .{},
            .checked_types = import_artifact.checked_types.view(),
            .checked_bodies = import_artifact.checked_bodies.view(),
            .exhaustiveness_sites = undefined,
            .checked_const_bodies = undefined,
            .checked_procedure_templates = &import_artifact.checked_procedure_templates,
            .compile_time_roots = undefined,
            .entry_wrappers = undefined,
            .intrinsic_wrappers = undefined,
            .resolved_value_refs = &import_artifact.resolved_value_refs,
            .nested_proc_sites = undefined,
            .static_dispatch_plans = undefined,
            .hosted_procs = &import_artifact.hosted_procs,
            .exported_procedure_templates = .{},
            .exported_procedure_bindings = import_artifact.exported_procedure_bindings.view(),
            .exported_const_templates = .{},
            .provided_exports = undefined,
            .top_level_procedure_bindings = &import_artifact.top_level_procedure_bindings,
            .platform_required_declarations = undefined,
            .platform_required_bindings = undefined,
            .callable_eval_templates = .{},
            .hoisted_constants = undefined,
            .const_templates = undefined,
            .method_registry = undefined,
            .interface_capabilities = &import_artifact.interface_capabilities,
            .const_store = undefined,
        },
    };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = root_template,
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &root_artifact, .roots = undefined },
        .imports = &imports,
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 3), plan.workers.items.len);
    const callee_worker = plan.directWorkerForCall(.{ .module = root_artifact.key, .expr = @enumFromInt(1) }) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(Plan.WorkerSource{ .procedure_use = imported_use }, plan.workers.items[@intFromEnum(callee_worker)].source);
    const helper_worker = plan.directWorkerForCall(.{ .module = import_artifact.key, .expr = @enumFromInt(1) }) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(
        Plan.WorkerSource{ .procedure_binding = .{ .artifact = import_artifact.key, .binding = @enumFromInt(0) } },
        plan.workers.items[@intFromEnum(helper_worker)].source,
    );

    var out = try run(
        gpa,
        .{
            .root = .{ .module = &root_artifact, .roots = undefined },
            .imports = &imports,
        },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.root_procs.items.len);
    try std.testing.expectEqual(@as(usize, 3), out.lir_result.store.proc_specs.items.len);

    const root_proc_id = out.lir_result.root_procs.items[0];
    const root_proc = out.lir_result.store.getProcSpec(root_proc_id);
    const call = out.lir_result.store.getCFStmt(root_proc.body orelse return error.TestUnexpectedResult).assign_call;
    try std.testing.expect(call.proc != root_proc_id);
    try std.testing.expect(call.args.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = call.target } }, out.lir_result.store.getCFStmt(call.next));

    const imported_proc_id = call.proc;
    const imported_proc = out.lir_result.store.getProcSpec(imported_proc_id);
    try std.testing.expect(imported_proc.args.isEmpty());
    const helper_call = out.lir_result.store.getCFStmt(imported_proc.body orelse return error.TestUnexpectedResult).assign_call;
    try std.testing.expect(helper_call.proc != root_proc_id);
    try std.testing.expect(helper_call.proc != imported_proc_id);
    try std.testing.expect(helper_call.args.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = helper_call.target } }, out.lir_result.store.getCFStmt(helper_call.next));

    const helper_proc = out.lir_result.store.getProcSpec(helper_call.proc);
    try std.testing.expect(helper_proc.args.isEmpty());
    const literal = out.lir_result.store.getCFStmt(helper_proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (literal.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 99), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = literal.target } }, out.lir_result.store.getCFStmt(literal.next));
}

test "boxy lowerer emits recursive direct calls to the current private worker" {
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
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{},
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(1),
            .direct_target = @enumFromInt(0),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_external = @enumFromInt(0) },
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

    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .top_level_proc = .{
                .binding = .{ .top_level = .{ .artifact = artifact.key, .binding = @enumFromInt(0) } },
                .source_fn_ty_template = canonicalTypeKey(1),
                .source_fn_ty_payload = @enumFromInt(1),
            } },
            .checked_ty = @enumFromInt(1),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
    };
    artifact.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
        .procedure_binding = @enumFromInt(0),
    };
    var plan = try Plan.analyzeProgram(gpa, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.workers.items.len);
    try std.testing.expectEqual(plan.roots.items[0].worker, plan.directWorkerForCall(.{ .module = artifact.key, .expr = @enumFromInt(1) }) orelse return error.TestUnexpectedResult);

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.root_procs.items.len);
    try std.testing.expectEqual(@as(usize, 1), out.lir_result.store.proc_specs.items.len);

    const proc_id = out.lir_result.root_procs.items[0];
    const proc = out.lir_result.store.getProcSpec(proc_id);
    const call = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_call;
    try std.testing.expectEqual(proc_id, call.proc);
    try std.testing.expect(call.args.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = call.target } }, out.lir_result.store.getCFStmt(call.next));
}

test "boxy lowerer emits checked crash as terminal LIR crash" {
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

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "boom");
    try artifact.checked_bodies.string_ranges.append(gpa, .{ .start = 0, .len = 4 });

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
        .data = .{ .crash = @enumFromInt(0) },
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
    const crash = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).crash;
    try std.testing.expectEqualStrings("boom", out.lir_result.store.getString(crash.msg));
}

test "boxy lowerer emits checked ellipsis as canonical not-implemented crash" {
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
        .data = .ellipsis,
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
    const crash = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).crash;
    try std.testing.expectEqualStrings("not implemented", out.lir_result.store.getString(crash.msg));
}

test "boxy lowerer emits checked return expressions as terminal ret" {
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
        .data = .{ .return_ = .{
            .expr = @enumFromInt(2),
            .lambda = @enumFromInt(0),
            .context = .return_expr,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
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
    const assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (assign.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 7), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = assign.target } }, out.lir_result.store.getCFStmt(assign.next));
}

test "boxy lowerer emits checked return statements as terminal ret" {
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

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .return_ = .{
            .expr = @enumFromInt(2),
            .lambda = @enumFromInt(0),
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
        .data = .{ .num = .{ .value = intValue(7), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
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
    const assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (assign.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 7), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = assign.target } }, out.lir_result.store.getCFStmt(assign.next));
}

test "boxy lowerer emits checked runtime error expressions as terminal runtime_error" {
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
        .data = .runtime_error,
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
    try std.testing.expectEqual(LIR.CFStmt.runtime_error, out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult));
}

test "boxy lowerer emits checked runtime error statements as terminal runtime_error" {
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

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .runtime_error,
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
            .final_expr = @enumFromInt(2),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
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
    try std.testing.expectEqual(LIR.CFStmt.runtime_error, out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult));
}

test "boxy lowerer emits checked while statements as join-backed loops" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const false_tag = try artifact.canonical_names.internTagLabel("False");

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.bool, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .while_ = .{
            .cond = @enumFromInt(2),
            .body = @enumFromInt(3),
        } },
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
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(4),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .zero_argument_tag = .{
            .closure_name = false_tag,
            .name = false_tag,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .empty_record,
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
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
    const loop_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    try std.testing.expect(loop_join.params.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .jump = .{ .target = loop_join.id } }, out.lir_result.store.getCFStmt(loop_join.remainder));

    const cond = out.lir_result.store.getCFStmt(loop_join.body).assign_tag;
    try std.testing.expectEqual(@as(u16, 0), cond.variant_index);
    const switch_stmt = out.lir_result.store.getCFStmt(cond.next).switch_stmt;
    try std.testing.expectEqual(cond.target, switch_stmt.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);

    const body_unit = out.lir_result.store.getCFStmt(branches[0].body).assign_struct;
    try std.testing.expect(body_unit.fields.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .jump = .{ .target = loop_join.id } }, out.lir_result.store.getCFStmt(body_unit.next));

    const after_loop = out.lir_result.store.getCFStmt(switch_stmt.default_branch).assign_literal;
    switch (after_loop.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 99), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = after_loop.target } }, out.lir_result.store.getCFStmt(after_loop.next));
}

test "boxy lowerer emits checked break as the active loop exit" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const true_tag = try artifact.canonical_names.internTagLabel("True");

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.bool, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .breakable_loop = .{
            .cond = @enumFromInt(2),
            .body = @enumFromInt(3),
        } },
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
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(4),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .zero_argument_tag = .{
            .closure_name = true_tag,
            .name = true_tag,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .break_,
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(41), .kind = .u64 } },
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
    const loop_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const cond = out.lir_result.store.getCFStmt(loop_join.body).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), cond.variant_index);
    const switch_stmt = out.lir_result.store.getCFStmt(cond.next).switch_stmt;
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);

    const break_unit = out.lir_result.store.getCFStmt(branches[0].body).assign_struct;
    try std.testing.expect(break_unit.fields.isEmpty());
    try std.testing.expectEqual(switch_stmt.default_branch, break_unit.next);

    const after_loop = out.lir_result.store.getCFStmt(break_unit.next).assign_literal;
    switch (after_loop.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 41), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = after_loop.target } }, out.lir_result.store.getCFStmt(after_loop.next));
}

test "boxy lowerer emits checked if expressions with a shared continuation join" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const true_tag = try artifact.canonical_names.internTagLabel("True");

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
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

    try artifact.checked_bodies.if_branch_pool.append(gpa, .{
        .cond = @enumFromInt(2),
        .body = @enumFromInt(3),
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
        .data = .{ .if_ = .{
            .branches = .{ .start = 0, .len = 1 },
            .final_else = @enumFromInt(4),
            .warn_unused_branches = false,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .zero_argument_tag = .{
            .closure_name = true_tag,
            .name = true_tag,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(11), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(22), .kind = .u64 } },
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
    const join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const join_params = out.lir_result.store.getLocalSpan(join.params);
    try std.testing.expectEqual(@as(usize, 1), join_params.len);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = join_params[0] } }, out.lir_result.store.getCFStmt(join.body));

    const cond = out.lir_result.store.getCFStmt(join.remainder).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), cond.variant_index);
    try std.testing.expectEqual(@as(u16, 1), cond.discriminant);

    const switch_stmt = out.lir_result.store.getCFStmt(cond.next).switch_stmt;
    try std.testing.expectEqual(cond.target, switch_stmt.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);

    const then_value = out.lir_result.store.getCFStmt(branches[0].body).assign_literal;
    switch (then_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 11), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .jump = .{ .target = join.id } }, out.lir_result.store.getCFStmt(then_value.next));

    const else_value = out.lir_result.store.getCFStmt(switch_stmt.default_branch).assign_literal;
    switch (else_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 22), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .jump = .{ .target = join.id } }, out.lir_result.store.getCFStmt(else_value.next));
}

test "boxy lowerer emits checked tag matches as ordered discriminant tests" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const tag_a = try artifact.canonical_names.internTagLabel("A");
    const tag_b = try artifact.canonical_names.internTagLabel("B");

    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_a, .args_start = 0, .args_len = 0 });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_b, .args_start = 0, .args_len = 0 });
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
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{ null, null });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .applied_tag = .{ .name = tag_a, .args = .{} } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .applied_tag = .{ .name = tag_b, .args = .{} } },
    });
    try artifact.checked_bodies.match_branch_pattern_pool.appendSlice(gpa, &.{
        .{ .pattern = @enumFromInt(0), .degenerate = false },
        .{ .pattern = @enumFromInt(1), .degenerate = false },
    });
    try artifact.checked_bodies.match_branch_pool.appendSlice(gpa, &.{
        .{ .pt_start = 0, .pt_len = 1, .value = @enumFromInt(3), .guard = null },
        .{ .pt_start = 1, .pt_len = 1, .value = @enumFromInt(4), .guard = null },
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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 2 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .zero_argument_tag = .{
            .closure_name = tag_b,
            .name = tag_b,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(11), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(22), .kind = .u64 } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const cond_tag = out.lir_result.store.getCFStmt(outer_join.remainder).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), cond_tag.variant_index);

    const first_join = out.lir_result.store.getCFStmt(cond_tag.next).join;
    const first_disc = out.lir_result.store.getCFStmt(first_join.remainder).assign_ref;
    switch (first_disc.op) {
        .discriminant => |disc| try std.testing.expectEqual(cond_tag.target, disc.source),
        else => return error.TestUnexpectedResult,
    }
    const first_switch = out.lir_result.store.getCFStmt(first_disc.next).switch_stmt;
    const first_branches = out.lir_result.store.getCFSwitchBranches(first_switch.branches);
    try std.testing.expectEqual(@as(usize, 1), first_branches.len);
    try std.testing.expectEqual(@as(u64, 0), first_branches[0].value);
    const first_value = out.lir_result.store.getCFStmt(first_branches[0].body).assign_literal;
    switch (first_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 11), literal.value),
        else => return error.TestUnexpectedResult,
    }

    const second_join = out.lir_result.store.getCFStmt(first_join.body).join;
    const second_disc = out.lir_result.store.getCFStmt(second_join.remainder).assign_ref;
    switch (second_disc.op) {
        .discriminant => |disc| try std.testing.expectEqual(cond_tag.target, disc.source),
        else => return error.TestUnexpectedResult,
    }
    const second_switch = out.lir_result.store.getCFStmt(second_disc.next).switch_stmt;
    const second_branches = out.lir_result.store.getCFSwitchBranches(second_switch.branches);
    try std.testing.expectEqual(@as(usize, 1), second_branches.len);
    try std.testing.expectEqual(@as(u64, 1), second_branches[0].value);
    const second_value = out.lir_result.store.getCFStmt(second_branches[0].body).assign_literal;
    switch (second_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 22), literal.value),
        else => return error.TestUnexpectedResult,
    }
}

test "boxy lowerer binds checked tag payload match patterns before branch bodies" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const tag_a = try artifact.canonical_names.internTagLabel("A");
    const tag_b = try artifact.canonical_names.internTagLabel("B");

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_a, .args_start = 0, .args_len = 1 });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_b, .args_start = 1, .args_len = 0 });
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
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(1),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        null,
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        null,
    });
    try artifact.checked_bodies.pattern_id_pool.append(gpa, @enumFromInt(1));
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .applied_tag = .{ .name = tag_a, .args = .{ .start = 0, .len = 1 } } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .applied_tag = .{ .name = tag_b, .args = .{} } },
    });
    try artifact.checked_bodies.match_branch_pattern_pool.appendSlice(gpa, &.{
        .{ .pattern = @enumFromInt(0), .degenerate = false },
        .{ .pattern = @enumFromInt(2), .degenerate = false },
    });
    try artifact.checked_bodies.match_branch_pool.appendSlice(gpa, &.{
        .{ .pt_start = 0, .pt_len = 1, .value = @enumFromInt(3), .guard = null },
        .{ .pt_start = 1, .pt_len = 1, .value = @enumFromInt(4), .guard = null },
    });
    try artifact.checked_bodies.expr_id_pool.append(gpa, @enumFromInt(5));

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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 2 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .tag = .{
            .name = tag_a,
            .args = .{ .start = 0, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(1), .resolved = null } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(0), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(41), .kind = .u64 } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const payload_literal = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    switch (payload_literal.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 41), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const cond_tag = out.lir_result.store.getCFStmt(payload_literal.next).assign_tag;
    try std.testing.expect(cond_tag.payload != null);

    const first_join = out.lir_result.store.getCFStmt(cond_tag.next).join;
    const disc = out.lir_result.store.getCFStmt(first_join.remainder).assign_ref;
    const switch_stmt = out.lir_result.store.getCFStmt(disc.next).switch_stmt;
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(u64, 0), branches[0].value);

    const payload_read = out.lir_result.store.getCFStmt(branches[0].body).assign_ref;
    switch (payload_read.op) {
        .tag_payload_struct => |payload| {
            try std.testing.expectEqual(cond_tag.target, payload.source);
            try std.testing.expectEqual(@as(u16, 0), payload.variant_index);
            try std.testing.expectEqual(@as(u16, 0), payload.tag_discriminant);
        },
        else => return error.TestUnexpectedResult,
    }

    const binder_assign = out.lir_result.store.getCFStmt(payload_read.next).assign_ref;
    switch (binder_assign.op) {
        .local => |source| try std.testing.expectEqual(payload_read.target, source),
        else => return error.TestUnexpectedResult,
    }

    const branch_result = out.lir_result.store.getCFStmt(binder_assign.next).assign_ref;
    switch (branch_result.op) {
        .local => |source| try std.testing.expectEqual(binder_assign.target, source),
        else => return error.TestUnexpectedResult,
    }
}

test "boxy lowerer emits checked list match patterns as length checks and element reads" {
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
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(2),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        null,
        null,
        @as(?checked.PatternBinderId, @enumFromInt(0)),
    });
    try artifact.checked_bodies.pattern_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedPatternId, @enumFromInt(1)),
        @as(checked.CheckedPatternId, @enumFromInt(2)),
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .list = .{
            .patterns = .{ .start = 0, .len = 2 },
            .rest = null,
        } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .underscore,
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.match_branch_pattern_pool.append(gpa, .{
        .pattern = @enumFromInt(0),
        .degenerate = false,
    });
    try artifact.checked_bodies.match_branch_pool.append(gpa, .{
        .pt_start = 0,
        .pt_len = 1,
        .value = @enumFromInt(3),
        .guard = null,
    });
    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(4)),
        @as(checked.CheckedExprId, @enumFromInt(5)),
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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 1 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .list = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(2), .resolved = null } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(7), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const first = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    const list = out.lir_result.store.getCFStmt(second.next).assign_list;

    const miss_join = out.lir_result.store.getCFStmt(list.next).join;
    const len = out.lir_result.store.getCFStmt(miss_join.remainder).assign_low_level;
    try std.testing.expectEqual(LIR.LowLevel.list_len, len.op);
    try std.testing.expectEqual(list.target, out.lir_result.store.getLocalSpan(len.args)[0]);

    const required = out.lir_result.store.getCFStmt(len.next).assign_literal;
    switch (required.value) {
        .i64_literal => |literal| {
            try std.testing.expectEqual(@as(i64, 2), literal.value);
            try std.testing.expectEqual(@as(layout.Idx, .u64), literal.layout_idx);
        },
        else => return error.TestUnexpectedResult,
    }

    const cmp = out.lir_result.store.getCFStmt(required.next).assign_low_level;
    try std.testing.expectEqual(LIR.LowLevel.num_is_eq, cmp.op);
    const cmp_args = out.lir_result.store.getLocalSpan(cmp.args);
    try std.testing.expectEqual(len.target, cmp_args[0]);
    try std.testing.expectEqual(required.target, cmp_args[1]);

    const switch_stmt = out.lir_result.store.getCFStmt(cmp.next).switch_stmt;
    try std.testing.expectEqual(cmp.target, switch_stmt.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);

    const item_index = out.lir_result.store.getCFStmt(branches[0].body).assign_literal;
    switch (item_index.value) {
        .i64_literal => |literal| try std.testing.expectEqual(@as(i64, 1), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const item = out.lir_result.store.getCFStmt(item_index.next).assign_low_level;
    try std.testing.expectEqual(LIR.LowLevel.list_get_unsafe, item.op);
    const item_args = out.lir_result.store.getLocalSpan(item.args);
    try std.testing.expectEqual(list.target, item_args[0]);
    try std.testing.expectEqual(item_index.target, item_args[1]);

    const bind = out.lir_result.store.getCFStmt(item.next).assign_ref;
    try std.testing.expectEqual(item.target, bind.op.local);
    const branch_result = out.lir_result.store.getCFStmt(bind.next).assign_ref;
    try std.testing.expectEqual(bind.target, branch_result.op.local);
    try std.testing.expectEqual(LIR.CFStmt{ .jump = .{ .target = outer_join.id } }, out.lir_result.store.getCFStmt(branch_result.next));
}

test "boxy lowerer emits checked string interpolation match patterns" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.str, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "preMIDpostprepost");
    try artifact.checked_bodies.string_ranges.appendSlice(gpa, &.{
        .{ .start = 0, .len = 10 },
        .{ .start = 10, .len = 3 },
        .{ .start = 13, .len = 4 },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(1),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        null,
        @as(?checked.PatternBinderId, @enumFromInt(0)),
    });
    try artifact.checked_bodies.str_pattern_step_pool.append(gpa, .{
        .capture = @enumFromInt(1),
        .delimiter = @enumFromInt(2),
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_interpolation = .{
            .prefix = @enumFromInt(1),
            .steps = .{ .start = 0, .len = 1 },
            .end = .exact,
        } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.match_branch_pattern_pool.append(gpa, .{
        .pattern = @enumFromInt(0),
        .degenerate = false,
    });
    try artifact.checked_bodies.match_branch_pool.append(gpa, .{
        .pt_start = 0,
        .pt_len = 1,
        .value = @enumFromInt(3),
        .guard = null,
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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 1 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_segment = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(1), .resolved = null } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const source = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    const miss_join = out.lir_result.store.getCFStmt(source.next).join;
    const str_match = out.lir_result.store.getCFStmt(miss_join.remainder).str_match;

    try std.testing.expectEqual(source.target, str_match.source);
    try std.testing.expectEqualStrings("pre", out.lir_result.store.getStringLiteral(str_match.prefix));
    try std.testing.expectEqual(LIR.StrPatternEnd.exact, str_match.end);

    const steps = out.lir_result.store.getStrMatchSteps(str_match.steps);
    try std.testing.expectEqual(@as(usize, 1), steps.len);
    try std.testing.expectEqualStrings("post", out.lir_result.store.getStringLiteral(steps[0].delimiter));
    const capture = switch (steps[0].capture) {
        .view => |local| local,
        .discard => return error.TestUnexpectedResult,
    };

    const bind = out.lir_result.store.getCFStmt(str_match.on_match).assign_ref;
    try std.testing.expectEqual(capture, bind.op.local);
    const branch_result = out.lir_result.store.getCFStmt(bind.next).assign_ref;
    try std.testing.expectEqual(bind.target, branch_result.op.local);
    try std.testing.expectEqual(LIR.CFStmt{ .jump = .{ .target = outer_join.id } }, out.lir_result.store.getCFStmt(branch_result.next));
}

test "boxy lowerer maps checked alternative binders onto representative match locals" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const tag_a = try artifact.canonical_names.internTagLabel("A");
    const tag_c = try artifact.canonical_names.internTagLabel("C");

    try artifact.checked_types.type_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)),
        @as(checked.CheckedTypeId, @enumFromInt(0)),
    });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_a, .args_start = 0, .args_len = 1 });
    try artifact.checked_types.tag_pool.append(gpa, .{ .name = tag_c, .args_start = 1, .args_len = 1 });
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
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.appendSlice(gpa, &.{
        .{ .id = @enumFromInt(0), .pattern = @enumFromInt(1), .reassignable = false },
        .{ .id = @enumFromInt(1), .pattern = @enumFromInt(3), .reassignable = false },
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        null,
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        null,
        @as(?checked.PatternBinderId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.pattern_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedPatternId, @enumFromInt(1)),
        @as(checked.CheckedPatternId, @enumFromInt(3)),
    });
    try artifact.checked_bodies.binder_remap_pool.appendSlice(gpa, &.{
        .{ .candidate_binder = @enumFromInt(0), .representative_binder = @enumFromInt(0) },
        .{ .candidate_binder = @enumFromInt(1), .representative_binder = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .applied_tag = .{ .name = tag_a, .args = .{ .start = 0, .len = 1 } } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .applied_tag = .{ .name = tag_c, .args = .{ .start = 1, .len = 1 } } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(1) },
    });
    try artifact.checked_bodies.match_branch_pattern_pool.appendSlice(gpa, &.{
        .{ .pattern = @enumFromInt(0), .degenerate = false, .bn_start = 0, .bn_len = 1 },
        .{ .pattern = @enumFromInt(2), .degenerate = false, .bn_start = 1, .bn_len = 1 },
    });
    try artifact.checked_bodies.match_branch_pool.append(gpa, .{
        .pt_start = 0,
        .pt_len = 2,
        .value = @enumFromInt(3),
        .guard = null,
    });
    try artifact.checked_bodies.expr_id_pool.append(gpa, @enumFromInt(4));

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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 1 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .tag = .{
            .name = tag_c,
            .args = .{ .start = 0, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(1), .resolved = null } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(41), .kind = .u64 } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const payload_literal = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    const cond_tag = out.lir_result.store.getCFStmt(payload_literal.next).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), cond_tag.variant_index);

    const first_alt_join = out.lir_result.store.getCFStmt(cond_tag.next).join;
    const second_alt_join = out.lir_result.store.getCFStmt(first_alt_join.body).join;
    const second_disc = out.lir_result.store.getCFStmt(second_alt_join.remainder).assign_ref;
    const second_switch = out.lir_result.store.getCFStmt(second_disc.next).switch_stmt;
    const second_branches = out.lir_result.store.getCFSwitchBranches(second_switch.branches);
    try std.testing.expectEqual(@as(u64, 1), second_branches[0].value);

    const payload_read = out.lir_result.store.getCFStmt(second_branches[0].body).assign_ref;
    switch (payload_read.op) {
        .tag_payload_struct => |payload| try std.testing.expectEqual(cond_tag.target, payload.source),
        else => return error.TestUnexpectedResult,
    }
    const remapped_assign = out.lir_result.store.getCFStmt(payload_read.next).assign_ref;
    switch (remapped_assign.op) {
        .local => |source| try std.testing.expectEqual(payload_read.target, source),
        else => return error.TestUnexpectedResult,
    }
    const branch_result = out.lir_result.store.getCFStmt(remapped_assign.next).assign_ref;
    switch (branch_result.op) {
        .local => |source| try std.testing.expectEqual(remapped_assign.target, source),
        else => return error.TestUnexpectedResult,
    }
}

test "boxy lowerer emits checked numeric literal match patterns as equality tests" {
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

    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{ null, null });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num_literal = .{
            .value = intValue(42),
            .kind = .u64,
            .conversion = null,
        } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .underscore,
    });
    try artifact.checked_bodies.match_branch_pattern_pool.appendSlice(gpa, &.{
        .{ .pattern = @enumFromInt(0), .degenerate = false },
        .{ .pattern = @enumFromInt(1), .degenerate = false },
    });
    try artifact.checked_bodies.match_branch_pool.appendSlice(gpa, &.{
        .{ .pt_start = 0, .pt_len = 1, .value = @enumFromInt(3), .guard = null },
        .{ .pt_start = 1, .pt_len = 1, .value = @enumFromInt(4), .guard = null },
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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 2 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(42), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(11), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(22), .kind = .u64 } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const cond_literal = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    const first_join = out.lir_result.store.getCFStmt(cond_literal.next).join;
    const pattern_literal = out.lir_result.store.getCFStmt(first_join.remainder).assign_literal;
    const compare = out.lir_result.store.getCFStmt(pattern_literal.next).assign_low_level;
    try std.testing.expectEqual(LIR.LowLevel.num_is_eq, compare.op);
    const args = out.lir_result.store.getLocalSpan(compare.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(cond_literal.target, args[0]);
    try std.testing.expectEqual(pattern_literal.target, args[1]);

    const switch_stmt = out.lir_result.store.getCFStmt(compare.next).switch_stmt;
    try std.testing.expectEqual(compare.target, switch_stmt.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);
    const matched_value = out.lir_result.store.getCFStmt(branches[0].body).assign_literal;
    switch (matched_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 11), literal.value),
        else => return error.TestUnexpectedResult,
    }
}

test "boxy lowerer emits checked small decimal match patterns as Dec equality tests" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const value = can.CIR.SmallDecValue{ .numerator = 314, .denominator_power_of_ten = 2 };
    const expected_dec = value.toRocDec().num;

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.dec, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{ null, null });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .small_dec_literal = .{
            .value = value,
            .has_suffix = false,
            .conversion = null,
        } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .underscore,
    });
    try artifact.checked_bodies.match_branch_pattern_pool.appendSlice(gpa, &.{
        .{ .pattern = @enumFromInt(0), .degenerate = false },
        .{ .pattern = @enumFromInt(1), .degenerate = false },
    });
    try artifact.checked_bodies.match_branch_pool.appendSlice(gpa, &.{
        .{ .pt_start = 0, .pt_len = 1, .value = @enumFromInt(3), .guard = null },
        .{ .pt_start = 1, .pt_len = 1, .value = @enumFromInt(4), .guard = null },
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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 2 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .dec_small = .{ .value = value, .has_suffix = false } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(11), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(22), .kind = .u64 } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const cond_literal = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    switch (cond_literal.value) {
        .dec_literal => |literal| try std.testing.expectEqual(expected_dec, literal),
        else => return error.TestUnexpectedResult,
    }

    const first_join = out.lir_result.store.getCFStmt(cond_literal.next).join;
    const pattern_literal = out.lir_result.store.getCFStmt(first_join.remainder).assign_literal;
    switch (pattern_literal.value) {
        .dec_literal => |literal| try std.testing.expectEqual(expected_dec, literal),
        else => return error.TestUnexpectedResult,
    }

    const compare = out.lir_result.store.getCFStmt(pattern_literal.next).assign_low_level;
    try std.testing.expectEqual(LIR.LowLevel.num_is_eq, compare.op);
    const args = out.lir_result.store.getLocalSpan(compare.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(cond_literal.target, args[0]);
    try std.testing.expectEqual(pattern_literal.target, args[1]);

    const switch_stmt = out.lir_result.store.getCFStmt(compare.next).switch_stmt;
    try std.testing.expectEqual(compare.target, switch_stmt.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);
    const matched_value = out.lir_result.store.getCFStmt(branches[0].body).assign_literal;
    switch (matched_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 11), literal.value),
        else => return error.TestUnexpectedResult,
    }
}

test "boxy lowerer emits checked string literal match patterns as string equality tests" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "ok");
    try artifact.checked_bodies.string_ranges.append(gpa, .{ .start = 0, .len = 2 });

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.str, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{ null, null });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_literal = .{
            .literal = @enumFromInt(0),
            .conversion = null,
        } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .underscore,
    });
    try artifact.checked_bodies.match_branch_pattern_pool.appendSlice(gpa, &.{
        .{ .pattern = @enumFromInt(0), .degenerate = false },
        .{ .pattern = @enumFromInt(1), .degenerate = false },
    });
    try artifact.checked_bodies.match_branch_pool.appendSlice(gpa, &.{
        .{ .pt_start = 0, .pt_len = 1, .value = @enumFromInt(3), .guard = null },
        .{ .pt_start = 1, .pt_len = 1, .value = @enumFromInt(4), .guard = null },
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
        .data = .{ .match_ = .{
            .cond = @enumFromInt(2),
            .branches = .{ .start = 0, .len = 2 },
            .is_try_suffix = false,
            .skip_exhaustiveness = false,
            .comptime_site_kind = .match,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_segment = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(11), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(22), .kind = .u64 } },
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
    const outer_join = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).join;
    const cond_literal = out.lir_result.store.getCFStmt(outer_join.remainder).assign_literal;
    const first_join = out.lir_result.store.getCFStmt(cond_literal.next).join;
    const pattern_literal = out.lir_result.store.getCFStmt(first_join.remainder).assign_literal;
    const compare = out.lir_result.store.getCFStmt(pattern_literal.next).assign_low_level;
    try std.testing.expectEqual(LIR.LowLevel.str_is_eq, compare.op);
    const args = out.lir_result.store.getLocalSpan(compare.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(cond_literal.target, args[0]);
    try std.testing.expectEqual(pattern_literal.target, args[1]);
}

test "boxy lowerer emits checked unary not as bool low-level call" {
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
        .data = .{ .unary_not = @enumFromInt(2) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
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
    const literal = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), literal.variant_index);
    try std.testing.expectEqual(@as(u16, 1), literal.discriminant);

    const not = out.lir_result.store.getCFStmt(literal.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .bool_not), not.op);
    const args = out.lir_result.store.getLocalSpan(not.args);
    try std.testing.expectEqual(@as(usize, 1), args.len);
    try std.testing.expectEqual(literal.target, args[0]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = not.target } }, out.lir_result.store.getCFStmt(not.next));
}

test "boxy lowerer emits short-circuit checked boolean and" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const false_tag = try artifact.canonical_names.internTagLabel("False");
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
        .data = .{ .binop = .{
            .op = .@"and",
            .lhs = @enumFromInt(2),
            .rhs = @enumFromInt(3),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .zero_argument_tag = .{
            .closure_name = false_tag,
            .name = false_tag,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
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
    const lhs = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_tag;
    try std.testing.expectEqual(@as(u16, 0), lhs.variant_index);

    const switch_stmt = out.lir_result.store.getCFStmt(lhs.next).switch_stmt;
    try std.testing.expectEqual(lhs.target, switch_stmt.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);

    const true_branch = out.lir_result.store.getCFStmt(branches[0].body).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), true_branch.variant_index);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = true_branch.target } }, out.lir_result.store.getCFStmt(true_branch.next));

    const false_branch = out.lir_result.store.getCFStmt(switch_stmt.default_branch).assign_tag;
    try std.testing.expectEqual(@as(u16, 0), false_branch.variant_index);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = false_branch.target } }, out.lir_result.store.getCFStmt(false_branch.next));
}

test "boxy lowerer emits primitive structural equality as low-level equality" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.bool, @enumFromInt(0), .{}),
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
        .data = .{ .structural_eq = .{
            .lhs = @enumFromInt(2),
            .rhs = @enumFromInt(3),
            .negated = false,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(42), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
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
    const lhs = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (lhs.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 42), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const rhs = out.lir_result.store.getCFStmt(lhs.next).assign_literal;
    switch (rhs.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 42), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const eq = out.lir_result.store.getCFStmt(rhs.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_is_eq), eq.op);
    const args = out.lir_result.store.getLocalSpan(eq.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(lhs.target, args[0]);
    try std.testing.expectEqual(rhs.target, args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = eq.target } }, out.lir_result.store.getCFStmt(eq.next));
}

test "boxy lowerer emits tuple structural equality with field short-circuiting" {
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
        .nominal = builtinNominal(.bool, @enumFromInt(0), .{}),
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
        @as(checked.CheckedExprId, @enumFromInt(3)),
        @as(checked.CheckedExprId, @enumFromInt(4)),
        @as(checked.CheckedExprId, @enumFromInt(6)),
        @as(checked.CheckedExprId, @enumFromInt(7)),
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
        .data = .{ .structural_eq = .{
            .lhs = @enumFromInt(2),
            .rhs = @enumFromInt(5),
            .negated = false,
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
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .tuple = .{ .start = 2, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(6),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(7),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(3), .kind = .u64 } },
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
    const lhs_first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const lhs_second = out.lir_result.store.getCFStmt(lhs_first.next).assign_literal;
    const lhs_tuple = out.lir_result.store.getCFStmt(lhs_second.next).assign_struct;
    const rhs_first = out.lir_result.store.getCFStmt(lhs_tuple.next).assign_literal;
    const rhs_second = out.lir_result.store.getCFStmt(rhs_first.next).assign_literal;
    const rhs_tuple = out.lir_result.store.getCFStmt(rhs_second.next).assign_struct;

    const read_lhs_field0 = out.lir_result.store.getCFStmt(rhs_tuple.next).assign_ref;
    try std.testing.expectEqual(lhs_tuple.target, read_lhs_field0.op.field.source);
    try std.testing.expectEqual(@as(u16, 0), read_lhs_field0.op.field.field_idx);
    const read_rhs_field0 = out.lir_result.store.getCFStmt(read_lhs_field0.next).assign_ref;
    try std.testing.expectEqual(rhs_tuple.target, read_rhs_field0.op.field.source);
    try std.testing.expectEqual(@as(u16, 0), read_rhs_field0.op.field.field_idx);

    const compare_field0 = out.lir_result.store.getCFStmt(read_rhs_field0.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_is_eq), compare_field0.op);
    const switch_field0 = out.lir_result.store.getCFStmt(compare_field0.next).switch_stmt;
    try std.testing.expectEqual(compare_field0.target, switch_field0.cond);
    const branches = out.lir_result.store.getCFSwitchBranches(switch_field0.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u64, 1), branches[0].value);

    const failed = out.lir_result.store.getCFStmt(switch_field0.default_branch).assign_tag;
    try std.testing.expectEqual(@as(u16, 0), failed.variant_index);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = failed.target } }, out.lir_result.store.getCFStmt(failed.next));

    const read_lhs_field1 = out.lir_result.store.getCFStmt(branches[0].body).assign_ref;
    try std.testing.expectEqual(lhs_tuple.target, read_lhs_field1.op.field.source);
    try std.testing.expectEqual(@as(u16, 1), read_lhs_field1.op.field.field_idx);
}

test "boxy lowerer emits primitive structural hash as hasher low-level" {
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
        .data = .{ .structural_hash = .{
            .value = @enumFromInt(2),
            .hasher = @enumFromInt(3),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(5), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
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
    const value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 5), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const seed = out.lir_result.store.getCFStmt(value.next).assign_literal;
    switch (seed.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 99), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const hash = out.lir_result.store.getCFStmt(seed.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .hasher_write_u64), hash.op);
    const args = out.lir_result.store.getLocalSpan(hash.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(seed.target, args[0]);
    try std.testing.expectEqual(value.target, args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = hash.target } }, out.lir_result.store.getCFStmt(hash.next));
}

test "boxy lowerer emits tuple structural hash by threading hasher through fields" {
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
        .data = .{ .structural_hash = .{
            .value = @enumFromInt(2),
            .hasher = @enumFromInt(5),
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
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(2), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
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
    const seed = out.lir_result.store.getCFStmt(tuple.next).assign_literal;

    const read_field0 = out.lir_result.store.getCFStmt(seed.next).assign_ref;
    try std.testing.expectEqual(tuple.target, read_field0.op.field.source);
    try std.testing.expectEqual(@as(u16, 0), read_field0.op.field.field_idx);
    const hash_field0 = out.lir_result.store.getCFStmt(read_field0.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .hasher_write_u64), hash_field0.op);
    const first_args = out.lir_result.store.getLocalSpan(hash_field0.args);
    try std.testing.expectEqual(seed.target, first_args[0]);
    try std.testing.expectEqual(read_field0.target, first_args[1]);

    const read_field1 = out.lir_result.store.getCFStmt(hash_field0.next).assign_ref;
    try std.testing.expectEqual(tuple.target, read_field1.op.field.source);
    try std.testing.expectEqual(@as(u16, 1), read_field1.op.field.field_idx);
    const hash_field1 = out.lir_result.store.getCFStmt(read_field1.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .hasher_write_u64), hash_field1.op);
    const second_args = out.lir_result.store.getLocalSpan(hash_field1.args);
    try std.testing.expectEqual(hash_field0.target, second_args[0]);
    try std.testing.expectEqual(read_field1.target, second_args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = hash_field1.target } }, out.lir_result.store.getCFStmt(hash_field1.next));
}

test "boxy lowerer emits checked string segment literals" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.str, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "hello");
    try artifact.checked_bodies.string_ranges.append(gpa, .{ .start = 0, .len = 5 });

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
        .data = .{ .str_segment = @enumFromInt(0) },
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
    const assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (assign.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("hello", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = assign.target } }, out.lir_result.store.getCFStmt(assign.next));
}

test "boxy lowerer emits checked bytes literals as byte-backed LIR literals" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u8, @enumFromInt(0), .{}),
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

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "abc");
    try artifact.checked_bodies.string_ranges.append(gpa, .{ .start = 0, .len = 3 });

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
        .data = .{ .bytes_literal = @enumFromInt(0) },
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
    const assign = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (assign.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("abc", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = assign.target } }, out.lir_result.store.getCFStmt(assign.next));
}

test "boxy lowerer emits checked string interpolation segments as concat chain" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.str, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "abc");
    try artifact.checked_bodies.string_ranges.appendSlice(gpa, &.{
        .{ .start = 0, .len = 1 },
        .{ .start = 1, .len = 1 },
        .{ .start = 2, .len = 1 },
    });
    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
        @as(checked.CheckedExprId, @enumFromInt(4)),
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
        .data = .{ .str = .{ .start = 0, .len = 3 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_segment = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_segment = @enumFromInt(1) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .str_segment = @enumFromInt(2) },
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
    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (first.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("a", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }

    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    switch (second.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("b", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }

    const first_concat = out.lir_result.store.getCFStmt(second.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), first_concat.op);
    const first_args = out.lir_result.store.getLocalSpan(first_concat.args);
    try std.testing.expectEqual(@as(usize, 2), first_args.len);
    try std.testing.expectEqual(first.target, first_args[0]);
    try std.testing.expectEqual(second.target, first_args[1]);

    const third = out.lir_result.store.getCFStmt(first_concat.next).assign_literal;
    switch (third.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("c", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }

    const second_concat = out.lir_result.store.getCFStmt(third.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), second_concat.op);
    const second_args = out.lir_result.store.getLocalSpan(second_concat.args);
    try std.testing.expectEqual(@as(usize, 2), second_args.len);
    try std.testing.expectEqual(first_concat.target, second_args[0]);
    try std.testing.expectEqual(third.target, second_args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = second_concat.target } }, out.lir_result.store.getCFStmt(second_concat.next));
}

test "boxy lowerer emits checked dbg expressions before unit result" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .empty_record);
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
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .dbg = @enumFromInt(2) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(7), .kind = .u64 } },
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
    const value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 7), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const message = out.lir_result.store.getCFStmt(value.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .u64_to_str), message.op);
    const args = out.lir_result.store.getLocalSpan(message.args);
    try std.testing.expectEqual(@as(usize, 1), args.len);
    try std.testing.expectEqual(value.target, args[0]);
    const debug = out.lir_result.store.getCFStmt(message.next).debug;
    try std.testing.expectEqual(message.target, debug.message);
    const unit = out.lir_result.store.getCFStmt(debug.next).assign_struct;
    try std.testing.expect(unit.fields.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = unit.target } }, out.lir_result.store.getCFStmt(unit.next));
}

test "boxy lowerer emits checked expect expressions before unit result" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const true_tag = try artifact.canonical_names.internTagLabel("True");

    try artifact.checked_types.payloads.append(gpa, .empty_record);
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
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .expect = @enumFromInt(2) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
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
    const cond = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_tag;
    try std.testing.expectEqual(@as(u16, 1), cond.variant_index);
    const expect = out.lir_result.store.getCFStmt(cond.next).expect;
    try std.testing.expectEqual(cond.target, expect.condition);
    const unit = out.lir_result.store.getCFStmt(expect.next).assign_struct;
    try std.testing.expect(unit.fields.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = unit.target } }, out.lir_result.store.getCFStmt(unit.next));
}

test "boxy lowerer emits expect_err messages from inspected payloads" {
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

    try artifact.checked_bodies.string_bytes.appendSlice(gpa, "fallible?");
    try artifact.checked_bodies.string_ranges.append(gpa, .{ .start = 0, .len = 9 });

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
        .data = .{ .expect_err = .{
            .expr = @enumFromInt(2),
            .snippet = @enumFromInt(0),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
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

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const payload = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (payload.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 42), literal.value),
        else => return error.TestUnexpectedResult,
    }

    const prefix = out.lir_result.store.getCFStmt(payload.next).assign_literal;
    switch (prefix.value) {
        .str_literal => |literal| {
            try std.testing.expectEqualStrings(
                "The `?` operator in `fallible?` evaluated an `Err` inside an `expect`. The value was: Err(",
                out.lir_result.store.getStringLiteral(literal),
            );
        },
        else => return error.TestUnexpectedResult,
    }

    const rendered = out.lir_result.store.getCFStmt(prefix.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .u64_to_str), rendered.op);
    const rendered_args = out.lir_result.store.getLocalSpan(rendered.args);
    try std.testing.expectEqual(@as(usize, 1), rendered_args.len);
    try std.testing.expectEqual(payload.target, rendered_args[0]);

    const with_value = out.lir_result.store.getCFStmt(rendered.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), with_value.op);
    const with_value_args = out.lir_result.store.getLocalSpan(with_value.args);
    try std.testing.expectEqual(@as(usize, 2), with_value_args.len);
    try std.testing.expectEqual(prefix.target, with_value_args[0]);
    try std.testing.expectEqual(rendered.target, with_value_args[1]);

    const suffix = out.lir_result.store.getCFStmt(with_value.next).assign_literal;
    switch (suffix.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings(")", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }

    const message = out.lir_result.store.getCFStmt(suffix.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), message.op);
    const message_args = out.lir_result.store.getLocalSpan(message.args);
    try std.testing.expectEqual(@as(usize, 2), message_args.len);
    try std.testing.expectEqual(with_value.target, message_args[0]);
    try std.testing.expectEqual(suffix.target, message_args[1]);

    const terminal = out.lir_result.store.getCFStmt(message.next).expect_err;
    try std.testing.expectEqual(message.target, terminal.message);
    try std.testing.expectEqual(base.Region.zero(), terminal.region);
}

test "boxy lowerer emits checked dbg statements in block order" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.payloads.append(gpa, .empty_record);
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

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .dbg = @enumFromInt(2) },
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
            .final_expr = @enumFromInt(3),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(13), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .empty_record,
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
    const value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 13), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const message = out.lir_result.store.getCFStmt(value.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .u64_to_str), message.op);
    const args = out.lir_result.store.getLocalSpan(message.args);
    try std.testing.expectEqual(@as(usize, 1), args.len);
    try std.testing.expectEqual(value.target, args[0]);
    const debug = out.lir_result.store.getCFStmt(message.next).debug;
    try std.testing.expectEqual(message.target, debug.message);
    const final_unit = out.lir_result.store.getCFStmt(debug.next).assign_struct;
    try std.testing.expect(final_unit.fields.isEmpty());
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = final_unit.target } }, out.lir_result.store.getCFStmt(final_unit.next));
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

test "boxy lowerer emits uninitialized mutable block bindings" {
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
        .reassignable = true,
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
        .data = .{ .var_uninitialized = .{ .pattern = @enumFromInt(0) } },
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
            .final_expr = @enumFromInt(2),
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

    const init = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).init_uninitialized;
    const final_assign = out.lir_result.store.getCFStmt(init.next).assign_literal;
    try std.testing.expect(init.target != final_assign.target);
    switch (final_assign.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 5), literal.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = final_assign.target } }, out.lir_result.store.getCFStmt(final_assign.next));
}

test "boxy lowerer emits mutable reassignment as set_local replace" {
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
        .reassignable = true,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.statement_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedStatementId, @enumFromInt(0)),
        @as(checked.CheckedStatementId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.pattern_binder_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .var_ = .{
            .pattern = @enumFromInt(0),
            .expr = @enumFromInt(2),
        } },
    });
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .reassign = .{
            .pattern = @enumFromInt(0),
            .expr = @enumFromInt(3),
            .reassigned_binders = .{ .start = 0, .len = 1 },
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
            .statements = .{ .start = 0, .len = 2 },
            .final_expr = @enumFromInt(4),
        } },
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
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
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
    try std.testing.expectEqual(@as(LIR.LocalSpan, .{ .start = 0, .len = 3 }), proc.frame_locals);

    const initial = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (initial.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 1), literal.value),
        else => return error.TestUnexpectedResult,
    }

    const replacement = out.lir_result.store.getCFStmt(initial.next).assign_literal;
    switch (replacement.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 2), literal.value),
        else => return error.TestUnexpectedResult,
    }

    const write = out.lir_result.store.getCFStmt(replacement.next).set_local;
    try std.testing.expectEqual(initial.target, write.target);
    try std.testing.expectEqual(replacement.target, write.value);
    try std.testing.expectEqual(LIR.SetLocalWriteMode.replace_existing, write.mode);

    const final_copy = out.lir_result.store.getCFStmt(write.next).assign_ref;
    try std.testing.expectEqual(write.target, final_copy.op.local);
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

test "boxy lowerer materializes record rest declaration patterns" {
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
        .record = .{ .fields = .{ .start = 1, .len = 1 }, .ext = @enumFromInt(1) },
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
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        null,
        null,
    });
    try artifact.checked_bodies.record_destruct_pool.appendSlice(gpa, &.{
        .{ .label = field_a, .kind = .{ .required = @enumFromInt(1) } },
        .{ .label = field_b, .kind = .{ .rest = @enumFromInt(0) } },
    });
    try artifact.checked_bodies.record_expr_field_pool.appendSlice(gpa, &.{
        .{ .label = field_a, .value = @as(checked.CheckedExprId, @enumFromInt(3)) },
        .{ .label = field_b, .value = @as(checked.CheckedExprId, @enumFromInt(4)) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .underscore,
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .record_destructure = .{ .start = 0, .len = 2 } },
    });

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .decl = .{
            .pattern = @enumFromInt(2),
            .expr = @enumFromInt(2),
        } },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(4),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(5),
        } },
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
        .data = .{ .num = .{ .value = intValue(11), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(22), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .field_access = .{
            .receiver = @enumFromInt(6),
            .field_name = field_b,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(6),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(4), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(4),
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
    const source_record = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const read_rest_field = out.lir_result.store.getCFStmt(source_record.next).assign_ref;
    const rest_record = out.lir_result.store.getCFStmt(read_rest_field.next).assign_struct;
    const bind_rest = out.lir_result.store.getCFStmt(rest_record.next).assign_ref;
    const final_receiver = out.lir_result.store.getCFStmt(bind_rest.next).assign_ref;
    const final_read = out.lir_result.store.getCFStmt(final_receiver.next).assign_ref;

    switch (first.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 11), value.value),
        else => return error.TestUnexpectedResult,
    }
    switch (second.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 22), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(source_record.target, read_rest_field.op.field.source);
    try std.testing.expectEqual(@as(u16, 1), read_rest_field.op.field.field_idx);
    try std.testing.expectEqual(read_rest_field.target, out.lir_result.store.getLocalSpan(rest_record.fields)[0]);
    try std.testing.expectEqual(rest_record.target, bind_rest.op.local);
    try std.testing.expectEqual(bind_rest.target, final_receiver.op.local);
    try std.testing.expectEqual(final_receiver.target, final_read.op.field.source);
    try std.testing.expectEqual(@as(u16, 0), final_read.op.field.field_idx);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = final_read.target } }, out.lir_result.store.getCFStmt(final_read.next));
}

test "boxy lowerer binds irrefutable list rest declaration patterns" {
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

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        null,
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .list = .{
            .patterns = .{},
            .rest = .{ .index = 0, .pattern = @enumFromInt(0) },
        } },
    });
    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .decl = .{
            .pattern = @enumFromInt(1),
            .expr = @enumFromInt(2),
        } },
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
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(5),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .list = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(3), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(4), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
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
    const bind_rest = out.lir_result.store.getCFStmt(list.next).assign_ref;
    const final_copy = out.lir_result.store.getCFStmt(bind_rest.next).assign_ref;

    switch (first.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 3), value.value),
        else => return error.TestUnexpectedResult,
    }
    switch (second.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 4), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(list.target, bind_rest.op.local);
    try std.testing.expectEqual(bind_rest.target, final_copy.op.local);
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

test "boxy lowerer evaluates empty record extensions before explicit fields" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const field_a: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(1);

    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.record_field_pool.append(gpa, .{
        .name = field_a,
        .ty = @enumFromInt(0),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .record = .{ .fields = .{ .start = 0, .len = 1 }, .ext = @enumFromInt(1) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(2),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.record_expr_field_pool.append(gpa, .{
        .label = field_a,
        .value = @enumFromInt(3),
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
            .fields = .{ .start = 0, .len = 1 },
            .ext = @enumFromInt(2),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .dbg = @enumFromInt(4) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(9), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
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
    const extension_value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (extension_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 5), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const extension_message = out.lir_result.store.getCFStmt(extension_value.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .u64_to_str), extension_message.op);
    const extension_args = out.lir_result.store.getLocalSpan(extension_message.args);
    try std.testing.expectEqual(@as(usize, 1), extension_args.len);
    try std.testing.expectEqual(extension_value.target, extension_args[0]);
    const extension_debug = out.lir_result.store.getCFStmt(extension_message.next).debug;
    try std.testing.expectEqual(extension_message.target, extension_debug.message);
    const extension_unit = out.lir_result.store.getCFStmt(extension_debug.next).assign_struct;
    try std.testing.expect(extension_unit.fields.isEmpty());
    const field_value = out.lir_result.store.getCFStmt(extension_unit.next).assign_literal;
    switch (field_value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 9), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const build = out.lir_result.store.getCFStmt(field_value.next).assign_struct;
    const fields = out.lir_result.store.getLocalSpan(build.fields);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expectEqual(field_value.target, fields[0]);
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

test "boxy lowerer emits nominal boundary before backing record pattern binding" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const field_a: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(1);
    const field_b: @TypeOf(@as(checked.CheckedRecordField, undefined).name) = @enumFromInt(2);
    const nominal_key = names.NominalTypeKey{
        .module_name = @enumFromInt(3),
        .type_name = @enumFromInt(4),
        .source_decl = 5,
    };

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u8, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u16, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.record_field_pool.appendSlice(gpa, &.{
        .{ .name = field_a, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
        .{ .name = field_b, .ty = @as(checked.CheckedTypeId, @enumFromInt(1)) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) },
    });
    try artifact.checked_types.declared_field_pool.appendSlice(gpa, &.{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    });
    try artifact.checked_types.nominal_declarations.append(gpa, .{
        .id = @enumFromInt(0),
        .nominal = nominal_key,
        .declaration_root = @enumFromInt(4),
        .backing = @enumFromInt(3),
        .pf_start = 0,
        .pf_len = 1,
        .df_start = 0,
        .df_len = 3,
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_declaration = @enumFromInt(0) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        },
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
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        null,
        null,
    });
    try artifact.checked_bodies.record_destruct_pool.append(gpa, .{
        .label = field_a,
        .kind = .{ .required = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .record_destructure = .{ .start = 0, .len = 1 } },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(4),
        .source_region = base.Region.zero(),
        .data = .{ .nominal = .{
            .backing_pattern = @enumFromInt(1),
            .backing_type = .value,
        } },
    });

    try artifact.checked_bodies.statement_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_statements.append(gpa, .{
        .id = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .decl = .{
            .pattern = @enumFromInt(2),
            .expr = @enumFromInt(2),
        } },
    });
    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(4)),
        @as(checked.CheckedExprId, @enumFromInt(5)),
    });
    try artifact.checked_bodies.record_expr_field_pool.appendSlice(gpa, &.{
        .{ .label = field_a, .value = @as(checked.CheckedExprId, @enumFromInt(4)) },
        .{ .label = field_b, .value = @as(checked.CheckedExprId, @enumFromInt(5)) },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(5),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .block = .{
            .statements = .{ .start = 0, .len = 1 },
            .final_expr = @enumFromInt(6),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(4),
        .source_region = base.Region.zero(),
        .data = .{ .nominal = .{
            .backing_expr = @enumFromInt(3),
            .backing_type = .value,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .record = .{
            .fields = .{ .start = 0, .len = 2 },
            .ext = null,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(7), .kind = .u8 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(500), .kind = .u16 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(6),
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
        checkedTemplate(template_ref, @enumFromInt(5), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(5),
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
    const backing_record = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const construct_nominal = out.lir_result.store.getCFStmt(backing_record.next).assign_ref;
    const destruct_nominal = out.lir_result.store.getCFStmt(construct_nominal.next).assign_ref;
    const read_field = out.lir_result.store.getCFStmt(destruct_nominal.next).assign_ref;
    const bind_field = out.lir_result.store.getCFStmt(read_field.next).assign_ref;
    const final_copy = out.lir_result.store.getCFStmt(bind_field.next).assign_ref;

    switch (first.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 7), value.value),
        else => return error.TestUnexpectedResult,
    }
    switch (second.value) {
        .i128_literal => |value| try std.testing.expectEqual(@as(i128, 500), value.value),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(backing_record.target, construct_nominal.op.nominal.backing_ref);
    try std.testing.expectEqual(construct_nominal.target, destruct_nominal.op.nominal.backing_ref);
    try std.testing.expectEqual(destruct_nominal.target, read_field.op.field.source);
    try std.testing.expectEqual(@as(u16, 0), read_field.op.field.field_idx);
    try std.testing.expectEqual(read_field.target, bind_field.op.local);
    try std.testing.expectEqual(bind_field.target, final_copy.op.local);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = final_copy.target } }, out.lir_result.store.getCFStmt(final_copy.next));
}

test "boxy lowerer inspects declared-field nominals through backing projection" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const field_a = try artifact.canonical_names.internRecordFieldLabel("a");
    const field_b = try artifact.canonical_names.internRecordFieldLabel("b");
    const nominal_key = names.NominalTypeKey{
        .module_name = try artifact.canonical_names.internModuleName("Test"),
        .type_name = try artifact.canonical_names.internTypeName("WithPadding"),
        .source_decl = 5,
    };

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u8, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u16, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.record_field_pool.appendSlice(gpa, &.{
        .{ .name = field_a, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
        .{ .name = field_b, .ty = @as(checked.CheckedTypeId, @enumFromInt(1)) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) },
    });
    try artifact.checked_types.declared_field_pool.appendSlice(gpa, &.{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    });
    try artifact.checked_types.nominal_declarations.append(gpa, .{
        .id = @enumFromInt(0),
        .nominal = nominal_key,
        .declaration_root = @enumFromInt(4),
        .backing = @enumFromInt(3),
        .pf_start = 0,
        .pf_len = 1,
        .df_start = 0,
        .df_len = 3,
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_declaration = @enumFromInt(0) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        },
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
        .{ .label = field_a, .value = @as(checked.CheckedExprId, @enumFromInt(4)) },
        .{ .label = field_b, .value = @as(checked.CheckedExprId, @enumFromInt(5)) },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(5),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .dbg = @enumFromInt(2) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(4),
        .source_region = base.Region.zero(),
        .data = .{ .nominal = .{
            .backing_expr = @enumFromInt(3),
            .backing_type = .value,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .record = .{
            .fields = .{ .start = 0, .len = 2 },
            .ext = null,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(7), .kind = .u8 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(500), .kind = .u16 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(5), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(5),
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
    const backing_record = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const construct_nominal = out.lir_result.store.getCFStmt(backing_record.next).assign_ref;
    const destruct_nominal = out.lir_result.store.getCFStmt(construct_nominal.next).assign_ref;

    try std.testing.expectEqual(backing_record.target, construct_nominal.op.nominal.backing_ref);
    try std.testing.expectEqual(construct_nominal.target, destruct_nominal.op.nominal.backing_ref);

    var cursor = destruct_nominal.next;
    var reads: [2]u16 = undefined;
    var read_count: usize = 0;
    var guard: usize = 0;
    while (guard < 100) : (guard += 1) {
        switch (out.lir_result.store.getCFStmt(cursor)) {
            .assign_ref => |assign| {
                switch (assign.op) {
                    .field => |field| {
                        try std.testing.expectEqual(destruct_nominal.target, field.source);
                        if (read_count >= reads.len) return error.TestUnexpectedResult;
                        reads[read_count] = field.field_idx;
                        read_count += 1;
                    },
                    else => {},
                }
                cursor = assign.next;
            },
            .assign_literal => |assign| cursor = assign.next,
            .assign_low_level => |assign| cursor = assign.next,
            .debug => |debug| {
                try std.testing.expectEqual(@as(usize, 2), read_count);
                try std.testing.expectEqual(@as(u16, 0), reads[0]);
                try std.testing.expectEqual(@as(u16, 1), reads[1]);
                try std.testing.expect(debug.message != construct_nominal.target);
                break;
            },
            else => return error.TestUnexpectedResult,
        }
    } else return error.TestUnexpectedResult;
}

test "boxy lowerer hashes declared-field nominals through backing projection" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    const field_a = try artifact.canonical_names.internRecordFieldLabel("a");
    const field_b = try artifact.canonical_names.internRecordFieldLabel("b");
    const nominal_key = names.NominalTypeKey{
        .module_name = try artifact.canonical_names.internModuleName("Test"),
        .type_name = try artifact.canonical_names.internTypeName("WithPadding"),
        .source_decl = 5,
    };

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u8, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u16, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.record_field_pool.appendSlice(gpa, &.{
        .{ .name = field_a, .ty = @as(checked.CheckedTypeId, @enumFromInt(0)) },
        .{ .name = field_b, .ty = @as(checked.CheckedTypeId, @enumFromInt(1)) },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) },
    });
    try artifact.checked_types.declared_field_pool.appendSlice(gpa, &.{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    });
    try artifact.checked_types.nominal_declarations.append(gpa, .{
        .id = @enumFromInt(0),
        .nominal = nominal_key,
        .declaration_root = @enumFromInt(4),
        .backing = @enumFromInt(3),
        .pf_start = 0,
        .pf_len = 1,
        .df_start = 0,
        .df_len = 3,
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_declaration = @enumFromInt(0) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        },
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(5), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(5),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.record_expr_field_pool.appendSlice(gpa, &.{
        .{ .label = field_a, .value = @as(checked.CheckedExprId, @enumFromInt(4)) },
        .{ .label = field_b, .value = @as(checked.CheckedExprId, @enumFromInt(5)) },
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(6),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(5),
        .source_region = base.Region.zero(),
        .data = .{ .structural_hash = .{
            .value = @enumFromInt(2),
            .hasher = @enumFromInt(6),
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(4),
        .source_region = base.Region.zero(),
        .data = .{ .nominal = .{
            .backing_expr = @enumFromInt(3),
            .backing_type = .value,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .record = .{
            .fields = .{ .start = 0, .len = 2 },
            .ext = null,
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(7), .kind = .u8 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(5),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(500), .kind = .u16 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(6),
        .ty = @enumFromInt(5),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
    });
    try artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(6), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(6),
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
    const backing_record = out.lir_result.store.getCFStmt(second.next).assign_struct;
    const construct_nominal = out.lir_result.store.getCFStmt(backing_record.next).assign_ref;
    const seed = out.lir_result.store.getCFStmt(construct_nominal.next).assign_literal;
    const destruct_nominal = out.lir_result.store.getCFStmt(seed.next).assign_ref;

    try std.testing.expectEqual(backing_record.target, construct_nominal.op.nominal.backing_ref);
    try std.testing.expectEqual(construct_nominal.target, destruct_nominal.op.nominal.backing_ref);

    var cursor = destruct_nominal.next;
    var reads: [2]u16 = undefined;
    var read_count: usize = 0;
    var guard: usize = 0;
    while (guard < 32) : (guard += 1) {
        switch (out.lir_result.store.getCFStmt(cursor)) {
            .assign_ref => |assign| {
                switch (assign.op) {
                    .field => |field| {
                        try std.testing.expectEqual(destruct_nominal.target, field.source);
                        if (read_count >= reads.len) return error.TestUnexpectedResult;
                        reads[read_count] = field.field_idx;
                        read_count += 1;
                    },
                    else => {},
                }
                cursor = assign.next;
            },
            .assign_low_level => |assign| cursor = assign.next,
            .ret => |ret| {
                try std.testing.expectEqual(@as(usize, 2), read_count);
                try std.testing.expectEqual(@as(u16, 0), reads[0]);
                try std.testing.expectEqual(@as(u16, 1), reads[1]);
                try std.testing.expect(ret.value != seed.target);
                break;
            },
            else => return error.TestUnexpectedResult,
        }
    } else return error.TestUnexpectedResult;
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

test "boxy lowerer stores dynamic list elements with boxy storage layout" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)), // List(a) element.
        @as(checked.CheckedTypeId, @enumFromInt(0)), // First function argument.
        @as(checked.CheckedTypeId, @enumFromInt(0)), // Second function argument.
    });
    try artifact.checked_types.payloads.append(gpa, .{ .flex = .{} });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.list, @enumFromInt(1), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 2 },
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(1),
        .pattern = @enumFromInt(1),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(gpa, &.{
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        @as(?checked.PatternBinderId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.pattern_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedPatternId, @enumFromInt(0)),
        @as(checked.CheckedPatternId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(1) },
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
        .data = .{ .lambda = .{ .args = .{ .start = 0, .len = 2 }, .body = @enumFromInt(1) } },
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
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(1), .resolved = null } },
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
    const args = out.lir_result.store.getLocalSpan(proc.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);

    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_ref;
    switch (first.op) {
        .local => |local| try std.testing.expectEqual(args[0], local),
        else => return error.TestUnexpectedResult,
    }
    const second = out.lir_result.store.getCFStmt(first.next).assign_ref;
    switch (second.op) {
        .local => |local| try std.testing.expectEqual(args[1], local),
        else => return error.TestUnexpectedResult,
    }
    const list = out.lir_result.store.getCFStmt(second.next).assign_list;
    const list_layout = out.lir_result.layouts.getLayout(out.lir_result.store.getLocal(list.target).layout_idx);
    try std.testing.expectEqual(layout.LayoutTag.list, list_layout.tag);
    try std.testing.expectEqual(list_layout.getIdx(), out.lir_result.store.getLocal(first.target).layout_idx);
    try std.testing.expectEqual(list_layout.getIdx(), out.lir_result.store.getLocal(second.target).layout_idx);
    try std.testing.expectEqual(first.target, out.lir_result.store.getLocalSpan(list.elems)[0]);
    try std.testing.expectEqual(second.target, out.lir_result.store.getLocalSpan(list.elems)[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = list.target } }, out.lir_result.store.getCFStmt(list.next));
}

test "boxy lowerer inspects concrete lists with an index and string accumulator loop" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(1));
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.list, @enumFromInt(2), .{ .start = 0, .len = 1 }),
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
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .dbg = @enumFromInt(2) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .list = .{ .start = 0, .len = 2 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(8), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(4),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(9), .kind = .u64 } },
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
    const list = out.lir_result.store.getCFStmt(second.next).assign_list;
    const join = out.lir_result.store.getCFStmt(list.next).join;
    const params = out.lir_result.store.getLocalSpan(join.params);
    try std.testing.expectEqual(@as(usize, 2), params.len);
    try std.testing.expectEqual(@as(layout.Idx, .u64), out.lir_result.store.getLocal(params[0]).layout_idx);
    try std.testing.expectEqual(@as(layout.Idx, .str), out.lir_result.store.getLocal(params[1]).layout_idx);

    const len = out.lir_result.store.getCFStmt(join.remainder).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .list_len), len.op);
    const len_args = out.lir_result.store.getLocalSpan(len.args);
    try std.testing.expectEqual(@as(usize, 1), len_args.len);
    try std.testing.expectEqual(list.target, len_args[0]);

    const done = out.lir_result.store.getCFStmt(join.body).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_is_eq), done.op);
    const done_args = out.lir_result.store.getLocalSpan(done.args);
    try std.testing.expectEqual(@as(usize, 2), done_args.len);
    try std.testing.expectEqual(params[0], done_args[0]);
    try std.testing.expectEqual(len.target, done_args[1]);

    const switch_stmt = out.lir_result.store.getCFStmt(done.next).switch_stmt;
    const branches = out.lir_result.store.getCFSwitchBranches(switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), branches.len);
    try std.testing.expectEqual(@as(u128, 1), branches[0].value);

    const close = out.lir_result.store.getCFStmt(branches[0].body).assign_literal;
    switch (close.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("]", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }
    const finish = out.lir_result.store.getCFStmt(close.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), finish.op);
    const debug = out.lir_result.store.getCFStmt(finish.next).debug;
    try std.testing.expectEqual(finish.target, debug.message);

    const step_zero = out.lir_result.store.getCFStmt(switch_stmt.default_branch).assign_literal;
    switch (step_zero.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 0), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const separator_check = out.lir_result.store.getCFStmt(step_zero.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_is_eq), separator_check.op);
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

test "boxy lowerer emits ordinary low-level calls after source-order argument lowering" {
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

    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
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
        .data = .{ .run_low_level = .{
            .op = .num_plus,
            .args = .{ .start = 0, .len = 2 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(10), .kind = .u64 } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(20), .kind = .u64 } },
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
    try std.testing.expectEqual(@as(usize, 3), out.lir_result.store.getLocalSpan(proc.frame_locals).len);

    const first = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (first.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 10), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const second = out.lir_result.store.getCFStmt(first.next).assign_literal;
    switch (second.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 20), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const add = out.lir_result.store.getCFStmt(second.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_plus), add.op);
    try std.testing.expectEqual(LIR.LowLevel.num_plus.rcEffect(), add.rc_effect);
    const args = out.lir_result.store.getLocalSpan(add.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(first.target, args[0]);
    try std.testing.expectEqual(second.target, args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = add.target } }, out.lir_result.store.getCFStmt(add.next));
}

test "boxy lowerer boxes concrete values with ordinary box low-level" {
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
        .nominal = builtinNominal(.box, @enumFromInt(1), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.expr_id_pool.append(gpa, @enumFromInt(2));

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
        .data = .{ .run_low_level = .{
            .op = .box_box,
            .args = .{ .start = 0, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
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
    const value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 42), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const boxed = out.lir_result.store.getCFStmt(value.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .box_box), boxed.op);
    const args = out.lir_result.store.getLocalSpan(boxed.args);
    try std.testing.expectEqual(@as(usize, 1), args.len);
    try std.testing.expectEqual(value.target, args[0]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = boxed.target } }, out.lir_result.store.getCFStmt(boxed.next));
}

test "boxy lowerer reuses dynamic boxes for Box(a)" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)), // Box(a) payload.
        @as(checked.CheckedTypeId, @enumFromInt(0)), // Function argument.
    });
    try artifact.checked_types.payloads.append(gpa, .{ .flex = .{} });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.box, @enumFromInt(1), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 1 },
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.pattern_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.expr_id_pool.append(gpa, @enumFromInt(2));

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{ .start = 0, .len = 1 }, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .run_low_level = .{
            .op = .box_box,
            .args = .{ .start = 0, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
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
    const args = out.lir_result.store.getLocalSpan(proc.args);
    try std.testing.expectEqual(@as(usize, 1), args.len);

    const from_arg = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_ref;
    switch (from_arg.op) {
        .local => |local| try std.testing.expectEqual(args[0], local),
        else => return error.TestUnexpectedResult,
    }

    const reused = out.lir_result.store.getCFStmt(from_arg.next).assign_ref;
    switch (reused.op) {
        .local => |local| try std.testing.expectEqual(from_arg.target, local),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = reused.target } }, out.lir_result.store.getCFStmt(reused.next));
}

test "boxy lowerer unboxes concrete values with ordinary box low-level" {
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
        .nominal = builtinNominal(.box, @enumFromInt(1), .{ .start = 0, .len = 1 }),
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
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .run_low_level = .{
            .op = .box_unbox,
            .args = .{ .start = 0, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .run_low_level = .{
            .op = .box_box,
            .args = .{ .start = 1, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(99), .kind = .u64 } },
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
    const value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 99), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const boxed = out.lir_result.store.getCFStmt(value.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .box_box), boxed.op);
    const boxed_args = out.lir_result.store.getLocalSpan(boxed.args);
    try std.testing.expectEqual(@as(usize, 1), boxed_args.len);
    try std.testing.expectEqual(value.target, boxed_args[0]);

    const unboxed = out.lir_result.store.getCFStmt(boxed.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .box_unbox), unboxed.op);
    const unboxed_args = out.lir_result.store.getLocalSpan(unboxed.args);
    try std.testing.expectEqual(@as(usize, 1), unboxed_args.len);
    try std.testing.expectEqual(boxed.target, unboxed_args[0]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = unboxed.target } }, out.lir_result.store.getCFStmt(unboxed.next));
}

test "boxy lowerer inspects concrete Box payloads" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);
    defer artifact.checked_bodies.deinit(gpa);

    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(1));
    try artifact.checked_types.payloads.append(gpa, .empty_record);
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.box, @enumFromInt(2), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.expr_id_pool.append(gpa, @enumFromInt(3));

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
        .data = .{ .dbg = @enumFromInt(2) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .run_low_level = .{
            .op = .box_box,
            .args = .{ .start = 0, .len = 1 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(42), .kind = .u64 } },
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
    const value = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (value.value) {
        .i128_literal => |literal| try std.testing.expectEqual(@as(i128, 42), literal.value),
        else => return error.TestUnexpectedResult,
    }
    const boxed = out.lir_result.store.getCFStmt(value.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .box_box), boxed.op);

    const prefix = out.lir_result.store.getCFStmt(boxed.next).assign_literal;
    switch (prefix.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings("Box(", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }
    const unboxed = out.lir_result.store.getCFStmt(prefix.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .box_unbox), unboxed.op);
    const unbox_args = out.lir_result.store.getLocalSpan(unboxed.args);
    try std.testing.expectEqual(@as(usize, 1), unbox_args.len);
    try std.testing.expectEqual(boxed.target, unbox_args[0]);

    const rendered = out.lir_result.store.getCFStmt(unboxed.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .u64_to_str), rendered.op);
    const rendered_args = out.lir_result.store.getLocalSpan(rendered.args);
    try std.testing.expectEqual(@as(usize, 1), rendered_args.len);
    try std.testing.expectEqual(unboxed.target, rendered_args[0]);

    const with_value = out.lir_result.store.getCFStmt(rendered.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), with_value.op);
    const suffix = out.lir_result.store.getCFStmt(with_value.next).assign_literal;
    switch (suffix.value) {
        .str_literal => |literal| try std.testing.expectEqualStrings(")", out.lir_result.store.getStringLiteral(literal)),
        else => return error.TestUnexpectedResult,
    }
    const message = out.lir_result.store.getCFStmt(suffix.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .str_concat), message.op);
    const debug = out.lir_result.store.getCFStmt(message.next).debug;
    try std.testing.expectEqual(message.target, debug.message);
}

test "boxy lowerer folds list_map_can_reuse to false when in-place map is disabled" {
    const gpa = std.testing.allocator;

    var out = try lowerListMapCanReuseFixture(gpa, .u64, .{});
    defer out.deinit();

    try expectListMapCanReuseFalse(&out);
}

test "boxy lowerer emits list_map_can_reuse when list map layouts are interchangeable" {
    const gpa = std.testing.allocator;

    var out = try lowerListMapCanReuseFixture(gpa, .u64, .{ .list_in_place_map = true });
    defer out.deinit();

    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const proc_args = out.lir_result.store.getLocalSpan(proc.args);
    try std.testing.expectEqual(@as(usize, 2), proc_args.len);

    const list_arg = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_ref;
    switch (list_arg.op) {
        .local => |local| try std.testing.expectEqual(proc_args[0], local),
        else => return error.TestUnexpectedResult,
    }

    const transform_arg = out.lir_result.store.getCFStmt(list_arg.next).assign_ref;
    switch (transform_arg.op) {
        .local => |local| try std.testing.expectEqual(proc_args[1], local),
        else => return error.TestUnexpectedResult,
    }

    const reuse = out.lir_result.store.getCFStmt(transform_arg.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .list_map_can_reuse), reuse.op);
    try std.testing.expectEqual(LIR.LowLevel.list_map_can_reuse.rcEffect(), reuse.rc_effect);
    try std.testing.expect(reuse.interchangeable.get(.u32));
    try std.testing.expect(reuse.interchangeable.get(.u64));

    const reuse_args = out.lir_result.store.getLocalSpan(reuse.args);
    try std.testing.expectEqual(@as(usize, 2), reuse_args.len);
    try std.testing.expectEqual(list_arg.target, reuse_args[0]);
    try std.testing.expectEqual(transform_arg.target, reuse_args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = reuse.target } }, out.lir_result.store.getCFStmt(reuse.next));
}

test "boxy lowerer folds list_map_can_reuse to false when list map layouts are not interchangeable" {
    const gpa = std.testing.allocator;

    var out = try lowerListMapCanReuseFixture(gpa, .u8, .{ .list_in_place_map = true });
    defer out.deinit();

    try expectListMapCanReuseFalse(&out);
}

test "boxy lowerer expands checked integer division low-level calls" {
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

    try artifact.checked_bodies.expr_id_pool.appendSlice(gpa, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
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
        .data = .{ .run_low_level = .{
            .op = .num_div_trunc_by,
            .args = .{ .start = 0, .len = 2 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .num = .{ .value = intValue(84), .kind = .u64 } },
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
    const lhs = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    const rhs = out.lir_result.store.getCFStmt(lhs.next).assign_literal;
    const zero = out.lir_result.store.getCFStmt(rhs.next).assign_literal;
    switch (zero.value) {
        .i128_literal => |literal| {
            try std.testing.expectEqual(@as(i128, 0), literal.value);
            try std.testing.expectEqual(@as(@TypeOf(literal.layout_idx), .u64), literal.layout_idx);
        },
        else => return error.TestUnexpectedResult,
    }

    const eq = out.lir_result.store.getCFStmt(zero.next).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_is_eq), eq.op);
    const eq_args = out.lir_result.store.getLocalSpan(eq.args);
    try std.testing.expectEqual(@as(usize, 2), eq_args.len);
    try std.testing.expectEqual(rhs.target, eq_args[0]);
    try std.testing.expectEqual(zero.target, eq_args[1]);

    const zero_check = out.lir_result.store.getCFStmt(eq.next).switch_stmt;
    const zero_check_branches = out.lir_result.store.getCFSwitchBranches(zero_check.branches);
    try std.testing.expectEqual(eq.target, zero_check.cond);
    try std.testing.expectEqual(@as(usize, 1), zero_check_branches.len);
    try std.testing.expectEqual(@as(u64, 1), zero_check_branches[0].value);
    switch (out.lir_result.store.getCFStmt(zero_check_branches[0].body)) {
        .crash => {},
        else => return error.TestUnexpectedResult,
    }

    const div = out.lir_result.store.getCFStmt(zero_check.default_branch).assign_low_level;
    try std.testing.expectEqual(@as(LIR.LowLevel, .num_div_trunc_by), div.op);
    const div_args = out.lir_result.store.getLocalSpan(div.args);
    try std.testing.expectEqual(@as(usize, 2), div_args.len);
    try std.testing.expectEqual(lhs.target, div_args[0]);
    try std.testing.expectEqual(rhs.target, div_args[1]);
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = div.target } }, out.lir_result.store.getCFStmt(div.next));
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
    try artifact.checked_types.type_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const template_ref = try namedProcedureTemplateRef(&artifact, 0, "exported_main");
    try artifact.checked_bodies.pattern_binders.append(gpa, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.pattern_id_pool.append(gpa, @enumFromInt(0));
    try artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{ .start = 0, .len = 1 }, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
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
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        .{ .proc_debug_names = true },
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 1), out.lir_result.root_procs.items.len);
    try std.testing.expectEqual(@as(usize, 2), out.lir_result.store.proc_specs.items.len);
    const wrapper_id = out.lir_result.root_procs.items[0];
    const wrapper = out.lir_result.store.getProcSpec(wrapper_id);
    const wrapper_args = out.lir_result.store.getLocalSpan(wrapper.args);
    try std.testing.expectEqual(@as(usize, 1), wrapper_args.len);
    const call = out.lir_result.store.getCFStmt(wrapper.body orelse return error.TestUnexpectedResult).assign_call;
    const call_args = out.lir_result.store.getLocalSpan(call.args);
    try std.testing.expectEqual(@as(usize, 1), call_args.len);
    try std.testing.expectEqual(wrapper_args[0], call_args[0]);
    try std.testing.expect(call.proc != wrapper_id);
    try std.testing.expectEqual(@as(@TypeOf(wrapper.ret_layout), .u64), wrapper.ret_layout);
    try std.testing.expectEqualStrings("exported_main", out.lir_result.store.procDebugName(wrapper_id) orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings("exported_main", out.lir_result.store.procDebugName(call.proc) orelse return error.TestUnexpectedResult);
    const worker = out.lir_result.store.getProcSpec(call.proc);
    const worker_args = out.lir_result.store.getLocalSpan(worker.args);
    try std.testing.expectEqual(@as(usize, 1), worker_args.len);
    const worker_copy = out.lir_result.store.getCFStmt(worker.body orelse return error.TestUnexpectedResult).assign_ref;
    switch (worker_copy.op) {
        .local => |local| try std.testing.expectEqual(worker_args[0], local),
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = worker_copy.target } }, out.lir_result.store.getCFStmt(worker_copy.next));
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

test "boxy lowerer emits requested layout metadata for static data requests" {
    const gpa = std.testing.allocator;

    var artifact = minimalCheckedArtifact(gpa);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(gpa);

    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(0), .key = typeKey(1) });
    try artifact.checked_types.roots.append(gpa, .{ .id = @enumFromInt(1), .key = typeKey(2) });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.str, @enumFromInt(1), .{}),
    });

    const data = checked.ProvidedDataExport{
        .source_name = @enumFromInt(0),
        .ffi_symbol = @enumFromInt(0),
        .def = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .checked_type = @enumFromInt(1),
        .source_scheme = typeSchemeKey(2),
        .const_ref = .{
            .artifact = artifact.key,
            .owner = .{ .top_level_binding = .{
                .module_idx = 0,
                .pattern = @enumFromInt(0),
            } },
            .template = @enumFromInt(0),
            .source_scheme = typeSchemeKey(2),
        },
    };

    var plan = try Plan.analyzeProgram(gpa, .{
        .checked_types = artifact.checked_types.view(),
        .layout_requests = &.{
            @as(checked.CheckedTypeId, @enumFromInt(0)),
            @as(checked.CheckedTypeId, @enumFromInt(1)),
        },
    }, .{});
    defer plan.deinit();

    var out = try run(
        gpa,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{
            .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(0))},
            .static_data_requests = &.{.{ .data = data }},
        },
        &plan,
        .{},
    );
    defer out.deinit();

    try std.testing.expectEqual(@as(usize, 2), out.lir_result.requested_layouts.items.len);

    const explicit = out.lir_result.requested_layouts.items[0];
    try std.testing.expectEqual(typeKey(1), explicit.ty);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), explicit.checked_type);
    try std.testing.expectEqual(.u64, explicit.layout_idx);
    try std.testing.expectEqual(LirProgram.ConstPlan.scalar, out.lir_result.const_plans.items[@intFromEnum(explicit.plan)]);

    const static_data = out.lir_result.requested_layouts.items[1];
    try std.testing.expectEqual(typeKey(2), static_data.ty);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(1)), static_data.checked_type);
    try std.testing.expectEqual(.str, static_data.layout_idx);
    try std.testing.expectEqual(LirProgram.ConstPlan.str, out.lir_result.const_plans.items[@intFromEnum(static_data.plan)]);
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
        .hosted_procs = .{},
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

fn canonicalTypeKey(byte: u8) names.CanonicalTypeKey {
    var key = names.CanonicalTypeKey{};
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

const ListMapCanReuseTransformRet = enum {
    u64,
    u8,
};

fn lowerListMapCanReuseFixture(
    allocator: Allocator,
    transform_ret: ListMapCanReuseTransformRet,
    options: Options,
) !Output {
    var artifact = minimalCheckedArtifact(allocator);
    defer artifact.canonical_names.deinit();
    defer artifact.checked_types.deinit(allocator);
    defer artifact.checked_bodies.deinit(allocator);

    const transform_ret_ty: checked.CheckedTypeId = switch (transform_ret) {
        .u64 => @enumFromInt(0),
        .u8 => @enumFromInt(1),
    };

    try artifact.checked_types.type_id_pool.appendSlice(allocator, &.{
        @as(checked.CheckedTypeId, @enumFromInt(0)), // List(U64) element.
        @as(checked.CheckedTypeId, @enumFromInt(0)), // Transform argument.
        @as(checked.CheckedTypeId, @enumFromInt(2)), // Root list argument.
        @as(checked.CheckedTypeId, @enumFromInt(3)), // Root transform argument.
    });
    try artifact.checked_types.payloads.append(allocator, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try artifact.checked_types.payloads.append(allocator, .{
        .nominal = builtinNominal(.u8, @enumFromInt(1), .{}),
    });
    try artifact.checked_types.payloads.append(allocator, .{
        .nominal = builtinNominal(.list, @enumFromInt(2), .{ .start = 0, .len = 1 }),
    });
    try artifact.checked_types.payloads.append(allocator, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 1 },
            .ret = transform_ret_ty,
            .needs_instantiation = false,
        },
    });
    try artifact.checked_types.payloads.append(allocator, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 2, .len = 2 },
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        },
    });

    try artifact.checked_bodies.pattern_binders.append(allocator, .{
        .id = @enumFromInt(0),
        .pattern = @enumFromInt(0),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binders.append(allocator, .{
        .id = @enumFromInt(1),
        .pattern = @enumFromInt(1),
        .reassignable = false,
    });
    try artifact.checked_bodies.pattern_binder_by_pattern.appendSlice(allocator, &.{
        @as(?checked.PatternBinderId, @enumFromInt(0)),
        @as(?checked.PatternBinderId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.pattern_id_pool.appendSlice(allocator, &.{
        @as(checked.CheckedPatternId, @enumFromInt(0)),
        @as(checked.CheckedPatternId, @enumFromInt(1)),
    });
    try artifact.checked_bodies.stored_patterns.append(allocator, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try artifact.checked_bodies.stored_patterns.append(allocator, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .assign = @enumFromInt(1) },
    });

    try artifact.checked_bodies.expr_id_pool.appendSlice(allocator, &.{
        @as(checked.CheckedExprId, @enumFromInt(2)),
        @as(checked.CheckedExprId, @enumFromInt(3)),
    });

    const template_ref = procedureTemplateRef(artifact.key, 0);
    try artifact.checked_bodies.stored_exprs.append(allocator, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(4),
        .source_region = base.Region.zero(),
        .data = .{ .lambda = .{ .args = .{ .start = 0, .len = 2 }, .body = @enumFromInt(1) } },
    });
    try artifact.checked_bodies.stored_exprs.append(allocator, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(1),
        .source_region = base.Region.zero(),
        .data = .{ .run_low_level = .{
            .op = .list_map_can_reuse,
            .args = .{ .start = 0, .len = 2 },
        } },
    });
    try artifact.checked_bodies.stored_exprs.append(allocator, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
    });
    try artifact.checked_bodies.stored_exprs.append(allocator, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(3),
        .source_region = base.Region.zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(1), .resolved = null } },
    });
    try artifact.checked_bodies.bodies.append(allocator, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = template_ref,
    });
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(4), @enumFromInt(0)),
    };
    artifact.checked_procedure_templates = .{ .templates = &templates };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(4),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = template_ref,
    };
    var plan = try Plan.analyzeProgram(allocator, .{
        .root_module = .{ .module = &artifact, .roots = undefined },
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    return try run(
        allocator,
        .{ .root = .{ .module = &artifact, .roots = undefined } },
        .{},
        &plan,
        options,
    );
}

fn expectListMapCanReuseFalse(out: *Output) !void {
    const proc = out.lir_result.store.getProcSpec(out.lir_result.root_procs.items[0]);
    const literal = out.lir_result.store.getCFStmt(proc.body orelse return error.TestUnexpectedResult).assign_literal;
    switch (literal.value) {
        .i64_literal => |value| {
            try std.testing.expectEqual(@as(i64, 0), value.value);
            try std.testing.expectEqual(out.lir_result.store.getLocal(literal.target).layout_idx, value.layout_idx);
        },
        else => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(LIR.CFStmt{ .ret = .{ .value = literal.target } }, out.lir_result.store.getCFStmt(literal.next));
}

fn procedureTemplateRef(key: checked.CheckedModuleArtifactKey, raw_template_id: u32) names.ProcedureTemplateRef {
    return .{
        .artifact = .{ .bytes = key.bytes },
        .proc_base = @enumFromInt(raw_template_id),
        .template = @enumFromInt(raw_template_id),
    };
}

fn namedProcedureTemplateRef(
    artifact: *checked.CheckedModuleArtifact,
    raw_template_id: u32,
    proc_name: []const u8,
) Allocator.Error!names.ProcedureTemplateRef {
    const module_name = try artifact.canonical_names.internModuleName("Test");
    const export_name = try artifact.canonical_names.internExportName(proc_name);
    const proc_base = try artifact.canonical_names.internProcBase(.{
        .module_name = module_name,
        .export_name = export_name,
        .kind = .checked_source,
        .ordinal = raw_template_id,
        .source_def_idx = raw_template_id,
    });
    return .{
        .artifact = .{ .bytes = artifact.key.bytes },
        .proc_base = proc_base,
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

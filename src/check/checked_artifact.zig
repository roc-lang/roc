//! Checked module artifact boundary.
//!
//! Public post-check lowering is moving toward consuming immutable artifacts and
//! narrowed read-only views instead of loose checked modules plus side stores.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const can = @import("can");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const static_dispatch = @import("static_dispatch_registry.zig");
const canonical = @import("canonical_names.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

pub const CheckedModuleArtifactKey = struct {
    source_hash: [32]u8 = [_]u8{0} ** 32,
    compiler_artifact_hash: [32]u8 = [_]u8{0} ** 32,
    module_identity_hash: [32]u8 = [_]u8{0} ** 32,
    checking_context_identity_hash: [32]u8 = [_]u8{0} ** 32,
    direct_import_artifact_keys_hash: [32]u8 = [_]u8{0} ** 32,
    bytes: [32]u8 = [_]u8{0} ** 32,

    pub fn compute(
        source: []const u8,
        module_identity: ModuleIdentity,
        checking_context_identity: CheckingContextIdentity,
        direct_import_artifact_keys: []const CheckedModuleArtifactKey,
    ) CheckedModuleArtifactKey {
        const source_hash = hashBytes(source);
        const compiler_artifact_hash = build_options.compiler_artifact_hash;
        const module_identity_hash = hashModuleIdentity(module_identity);
        const checking_context_identity_hash = hashCheckingContextIdentity(checking_context_identity);
        const direct_import_artifact_keys_hash = hashDirectImportArtifactKeys(direct_import_artifact_keys);

        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(&source_hash);
        hasher.update(&compiler_artifact_hash);
        hasher.update(&module_identity_hash);
        hasher.update(&checking_context_identity_hash);
        hasher.update(&direct_import_artifact_keys_hash);

        return .{
            .source_hash = source_hash,
            .compiler_artifact_hash = compiler_artifact_hash,
            .module_identity_hash = module_identity_hash,
            .checking_context_identity_hash = checking_context_identity_hash,
            .direct_import_artifact_keys_hash = direct_import_artifact_keys_hash,
            .bytes = hasher.finalResult(),
        };
    }
};

pub const ModuleIdentity = struct {
    module_idx: u32,
    module_name: canonical.ModuleNameId,
    display_module_name: canonical.ModuleNameId,
    qualified_module_name: canonical.ModuleNameId,
    kind: ModuleEnv.ModuleKind,
};

pub const ImportIdentity = struct {
    import_idx: CIR.Import.Idx,
    resolved_module_idx: ?u32,
};

fn hashBytes(bytes: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
}

fn hashU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

fn hashModuleIdentity(identity: ModuleIdentity) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashU32(&hasher, identity.module_idx);
    hashU32(&hasher, @intFromEnum(identity.module_name));
    hashU32(&hasher, @intFromEnum(identity.display_module_name));
    hashU32(&hasher, @intFromEnum(identity.qualified_module_name));
    hasher.update(@tagName(identity.kind));
    return hasher.finalResult();
}

fn hashCheckingContextIdentity(identity: CheckingContextIdentity) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashU32(&hasher, @intCast(identity.imports.len));
    for (identity.imports) |import_identity| {
        hashU32(&hasher, @intFromEnum(import_identity.import_idx));
        if (import_identity.resolved_module_idx) |resolved| {
            hasher.update(&[_]u8{1});
            hashU32(&hasher, resolved);
        } else {
            hasher.update(&[_]u8{0});
        }
    }
    return hasher.finalResult();
}

fn hashDirectImportArtifactKeys(keys: []const CheckedModuleArtifactKey) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashU32(&hasher, @intCast(keys.len));
    for (keys) |key| hasher.update(&key.bytes);
    return hasher.finalResult();
}

pub const CheckingContextIdentity = struct {
    imports: []ImportIdentity = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!CheckingContextIdentity {
        const module_env = module.moduleEnvConst();
        const imported_names = module_env.imports.imports.items.items;
        const imports = try allocator.alloc(ImportIdentity, imported_names.len);
        errdefer allocator.free(imports);

        for (imported_names, 0..) |_, i| {
            const import_idx: CIR.Import.Idx = @enumFromInt(@as(u32, @intCast(i)));
            imports[i] = .{
                .import_idx = import_idx,
                .resolved_module_idx = module.resolvedImportModule(import_idx),
            };
        }

        return .{ .imports = imports };
    }

    pub fn deinit(self: *CheckingContextIdentity, allocator: Allocator) void {
        allocator.free(self.imports);
        self.* = .{};
    }
};

pub const PublishImportArtifact = struct {
    module_idx: u32,
    key: CheckedModuleArtifactKey,
};

pub const PublishInputs = struct {
    imports: []const PublishImportArtifact = &.{},
};

pub const ExportTable = struct {
    defs: []CIR.Def.Idx = &.{},

    pub fn deinit(self: *ExportTable, allocator: Allocator) void {
        allocator.free(self.defs);
        self.* = .{};
    }
};

pub const ProvidesRequiresMetadata = struct {
    provides: []ModuleEnv.ProvidesEntry = &.{},
    requires: []ModuleEnv.RequiredType = &.{},

    pub fn deinit(self: *ProvidesRequiresMetadata, allocator: Allocator) void {
        allocator.free(self.provides);
        allocator.free(self.requires);
        self.* = .{};
    }
};

pub const RootRequestKind = enum {
    runtime_entrypoint,
    provided_export,
    platform_required_binding,
    hosted_export,
    test_expect,
    repl_expr,
    dev_expr,
    compile_time_constant,
};

pub const RootAbi = enum {
    roc,
    platform,
    hosted,
    test_expect,
    compile_time,
};

pub const RootExposure = enum {
    private,
    exported,
    platform_required,
    hosted,
};

pub const RootSource = union(enum) {
    def: CIR.Def.Idx,
    expr: CIR.Expr.Idx,
    statement: CIR.Statement.Idx,
    required_binding: u32,
};

pub const RootRequest = struct {
    order: u32,
    module_idx: u32,
    kind: RootRequestKind,
    source: RootSource,
    checked_type: Var,
    abi: RootAbi,
    exposure: RootExposure,
};

pub const RootRequestTable = struct {
    requests: []RootRequest = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!RootRequestTable {
        var requests = std.ArrayList(RootRequest).empty;
        errdefer requests.deinit(allocator);

        const module_env = module.moduleEnvConst();
        const exports = module_env.store.sliceDefs(module_env.exports);
        for (exports) |def_idx| {
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .provided_export,
                .source = .{ .def = def_idx },
                .checked_type = module.defType(def_idx),
                .abi = .roc,
                .exposure = .exported,
            });
        }

        for (module_env.requires_types.items.items, 0..) |required_type, i| {
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .platform_required_binding,
                .source = .{ .required_binding = @intCast(i) },
                .checked_type = ModuleEnv.varFrom(required_type.type_anno),
                .abi = .platform,
                .exposure = .platform_required,
            });
        }

        for (module_env.store.sliceStatements(module_env.all_statements)) |statement_idx| {
            const stmt = module_env.store.getStatement(statement_idx);
            if (stmt != .s_expect) continue;
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .test_expect,
                .source = .{ .statement = statement_idx },
                .checked_type = ModuleEnv.varFrom(stmt.s_expect.body),
                .abi = .test_expect,
                .exposure = .private,
            });
        }

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            if (topLevelExprIsAlreadyProcedure(def.expr.data)) continue;
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .compile_time_constant,
                .source = .{ .def = def_idx },
                .checked_type = module.defType(def_idx),
                .abi = .compile_time,
                .exposure = .private,
            });
        }

        return .{ .requests = try requests.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *RootRequestTable, allocator: Allocator) void {
        allocator.free(self.requests);
        self.* = .{};
    }
};

const RootRequestWithoutOrder = struct {
    module_idx: u32,
    kind: RootRequestKind,
    source: RootSource,
    checked_type: Var,
    abi: RootAbi,
    exposure: RootExposure,
};

fn appendRoot(
    requests: *std.ArrayList(RootRequest),
    allocator: Allocator,
    request: RootRequestWithoutOrder,
) Allocator.Error!void {
    try requests.append(allocator, .{
        .order = @intCast(requests.items.len),
        .module_idx = request.module_idx,
        .kind = request.kind,
        .source = request.source,
        .checked_type = request.checked_type,
        .abi = request.abi,
        .exposure = request.exposure,
    });
}

fn topLevelExprIsAlreadyProcedure(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_lambda, .e_closure, .e_anno_only, .e_hosted_lambda => true,
        else => false,
    };
}

fn isHostedProcedureExpr(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_hosted_lambda => true,
        else => false,
    };
}

pub const CheckedProcedureBody = union(enum) {
    source_expr: CIR.Expr.Idx,
    promoted_callable_wrapper: canonical.PromotedCallableWrapperId,
    hosted_wrapper: canonical.HostedWrapperId,
    platform_required_wrapper: u32,
    intrinsic_wrapper: canonical.IntrinsicWrapperId,
    entry_wrapper: canonical.EntryWrapperId,
};

pub const StaticDispatchPlanTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const ResolvedValueRefTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const TopLevelUseSummaryRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const NestedProcSiteTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const ProcTarget = union(enum) {
    roc,
    hosted,
    platform_required,
    intrinsic,
    entry,
    promoted_callable,
};

pub const CheckedProcedureTemplate = struct {
    proc_base: canonical.ProcBaseKeyRef,
    template_id: canonical.CheckedProcedureTemplateId,
    body: CheckedProcedureBody,
    checked_fn_var: Var,
    static_dispatch_plans: StaticDispatchPlanTableRef,
    resolved_value_refs: ResolvedValueRefTableRef,
    top_level_value_uses: TopLevelUseSummaryRef,
    nested_proc_sites: NestedProcSiteTableRef,
    target: ProcTarget,
};

pub const CheckedProcedureTemplateTable = struct {
    templates: []CheckedProcedureTemplate = &.{},
    by_def: []?canonical.ProcedureTemplateRef = &.{},
    by_required_binding: []?canonical.ProcedureTemplateRef = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
    ) Allocator.Error!CheckedProcedureTemplateTable {
        var templates = std.ArrayList(CheckedProcedureTemplate).empty;
        errdefer templates.deinit(allocator);

        const by_def = try allocator.alloc(?canonical.ProcedureTemplateRef, module.nodeCount());
        errdefer allocator.free(by_def);
        @memset(by_def, null);

        const by_required_binding = try allocator.alloc(?canonical.ProcedureTemplateRef, module.requiresTypes().len);
        errdefer allocator.free(by_required_binding);
        @memset(by_required_binding, null);

        const module_name = try names.internModuleIdent(module.identStoreConst(), module.qualifiedModuleIdent());

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            if (!topLevelExprIsAlreadyProcedure(def.expr.data)) continue;

            const export_name = if (def.patternName()) |name|
                try names.internExportIdent(module.identStoreConst(), name)
            else
                null;
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = export_name,
                .kind = if (isHostedProcedureExpr(def.expr.data)) .hosted_wrapper else .checked_source,
                .ordinal = @intFromEnum(def_idx),
            });
            const template_id: canonical.CheckedProcedureTemplateId = @enumFromInt(@as(u32, @intCast(templates.items.len)));
            const template_ref = canonical.ProcedureTemplateRef{
                .proc_base = proc_base,
                .template = template_id,
            };
            by_def[@intFromEnum(def_idx)] = template_ref;

            try templates.append(allocator, .{
                .proc_base = proc_base,
                .template_id = template_id,
                .body = .{ .source_expr = def.expr.idx },
                .checked_fn_var = module.defType(def_idx),
                .static_dispatch_plans = .{},
                .resolved_value_refs = .{},
                .top_level_value_uses = .{},
                .nested_proc_sites = .{},
                .target = if (isHostedProcedureExpr(def.expr.data)) .hosted else .roc,
            });
        }

        for (module.requiresTypes(), 0..) |required_type, i| {
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = null,
                .kind = .platform_required_wrapper,
                .ordinal = @intCast(i),
            });
            const template_id: canonical.CheckedProcedureTemplateId = @enumFromInt(@as(u32, @intCast(templates.items.len)));
            const template_ref = canonical.ProcedureTemplateRef{
                .proc_base = proc_base,
                .template = template_id,
            };
            by_required_binding[i] = template_ref;

            try templates.append(allocator, .{
                .proc_base = proc_base,
                .template_id = template_id,
                .body = .{ .platform_required_wrapper = @intCast(i) },
                .checked_fn_var = ModuleEnv.varFrom(required_type.type_anno),
                .static_dispatch_plans = .{},
                .resolved_value_refs = .{},
                .top_level_value_uses = .{},
                .nested_proc_sites = .{},
                .target = .platform_required,
            });
        }

        return .{
            .templates = try templates.toOwnedSlice(allocator),
            .by_def = by_def,
            .by_required_binding = by_required_binding,
        };
    }

    pub fn lookupByDef(self: *const CheckedProcedureTemplateTable, def_idx: CIR.Def.Idx) ?canonical.ProcedureTemplateRef {
        const raw = @intFromEnum(def_idx);
        if (raw >= self.by_def.len) return null;
        return self.by_def[raw];
    }

    pub fn lookupByRequiredBinding(self: *const CheckedProcedureTemplateTable, requires_idx: u32) ?canonical.ProcedureTemplateRef {
        if (requires_idx >= self.by_required_binding.len) return null;
        return self.by_required_binding[requires_idx];
    }

    pub fn asLookup(self: *const CheckedProcedureTemplateTable, module_idx: u32) static_dispatch.ProcedureTemplateLookup {
        return .{
            .module_idx = module_idx,
            .by_def = self.by_def,
        };
    }

    pub fn deinit(self: *CheckedProcedureTemplateTable, allocator: Allocator) void {
        allocator.free(self.by_required_binding);
        allocator.free(self.by_def);
        allocator.free(self.templates);
        self.* = .{};
    }
};

pub const HostedProc = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    expr_idx: CIR.Expr.Idx,
    external_symbol_name: canonical.ExternalSymbolNameId,
    deterministic_index: u32,
    proc: canonical.ProcedureValueRef,
};

pub const HostedProcTable = struct {
    procs: []HostedProc = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        templates: *const CheckedProcedureTemplateTable,
    ) Allocator.Error!HostedProcTable {
        var procs = std.ArrayList(HostedProc).empty;
        errdefer procs.deinit(allocator);

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            switch (def.expr.data) {
                .e_hosted_lambda => |hosted| try procs.append(allocator, .{
                    .module_idx = module.moduleIndex(),
                    .def_idx = def_idx,
                    .expr_idx = def.expr.idx,
                    .external_symbol_name = try names.internExternalSymbolIdent(module.identStoreConst(), hosted.symbol_name),
                    .deterministic_index = @intCast(procs.items.len),
                    .proc = .{ .proc_base = templates.lookupByDef(def_idx).?.proc_base },
                }),
                else => {},
            }
        }

        return .{ .procs = try procs.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *HostedProcTable, allocator: Allocator) void {
        allocator.free(self.procs);
        self.* = .{};
    }
};

pub const PlatformRequiredBinding = struct {
    module_idx: u32,
    requires_idx: u32,
    ident: canonical.ExternalSymbolNameId,
    type_anno: CIR.TypeAnno.Idx,
    proc: canonical.ProcedureValueRef,
};

pub const PlatformRequiredBindingTable = struct {
    bindings: []PlatformRequiredBinding = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
    ) Allocator.Error!PlatformRequiredBindingTable {
        const module_env = module.moduleEnvConst();
        const bindings = try allocator.alloc(PlatformRequiredBinding, module_env.requires_types.items.items.len);
        errdefer allocator.free(bindings);
        const module_name = try names.internModuleIdent(module.identStoreConst(), module.qualifiedModuleIdent());

        for (module_env.requires_types.items.items, 0..) |required_type, i| {
            const external_name = try names.internExternalSymbolIdent(module.identStoreConst(), required_type.ident);
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = null,
                .kind = .platform_required_wrapper,
                .ordinal = @intCast(i),
            });
            bindings[i] = .{
                .module_idx = module.moduleIndex(),
                .requires_idx = @intCast(i),
                .ident = external_name,
                .type_anno = required_type.type_anno,
                .proc = .{ .proc_base = proc_base },
            };
        }

        return .{ .bindings = bindings };
    }

    pub fn deinit(self: *PlatformRequiredBindingTable, allocator: Allocator) void {
        allocator.free(self.bindings);
        self.* = .{};
    }
};

pub const ModuleInterfaceCapabilities = struct {
    exported_def_count: u32,
    method_count: u32,
    provides_count: u32,
    requires_count: u32,

    pub fn fromModule(module: TypedCIR.Module) ModuleInterfaceCapabilities {
        const module_env = module.moduleEnvConst();
        return .{
            .exported_def_count = @intCast(module_env.store.sliceDefs(module_env.exports).len),
            .method_count = @intCast(module.methodIdentEntries().len),
            .provides_count = @intCast(module_env.provides_entries.items.items.len),
            .requires_count = @intCast(module_env.requires_types.items.items.len),
        };
    }
};

pub const ComptimeSchemaId = enum(u32) { _ };
pub const ComptimeValueId = enum(u32) { _ };

pub const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    source_type: Var,
};

pub const TopLevelProcedureBindingRef = struct {
    proc: canonical.ProcedureValueRef,
    template: ?canonical.ProcedureTemplateRef,
};

pub const TopLevelValueKind = union(enum) {
    const_ref: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
    callable_eval_template: u32,
};

pub const TopLevelValueEntry = struct {
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
    source_type: Var,
    value: TopLevelValueKind,
};

pub const TopLevelValueTable = struct {
    entries: []TopLevelValueEntry = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        templates: *const CheckedProcedureTemplateTable,
    ) Allocator.Error!TopLevelValueTable {
        var entries = std.ArrayList(TopLevelValueEntry).empty;
        errdefer entries.deinit(allocator);

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            const value: TopLevelValueKind = if (topLevelExprIsAlreadyProcedure(def.expr.data)) blk: {
                const template = templates.lookupByDef(def_idx) orelse unreachable;
                break :blk .{ .procedure_binding = .{
                    .proc = .{ .proc_base = template.proc_base },
                    .template = template,
                } };
            } else .{ .const_ref = .{
                .artifact = .{},
                .module_idx = module.moduleIndex(),
                .pattern = def.pattern.idx,
                .schema = @enumFromInt(@intFromEnum(def_idx)),
                .value = @enumFromInt(@intFromEnum(def_idx)),
                .source_type = module.defType(def_idx),
            } };

            try entries.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .pattern = def.pattern.idx,
                .source_type = module.defType(def_idx),
                .value = value,
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *TopLevelValueTable, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }
};

pub const PromotedProcedure = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    source_binding: CIR.Pattern.Idx,
    source_type: Var,
};

pub const PromotedProcedureTable = struct {
    procedures: []PromotedProcedure = &.{},

    pub fn deinit(self: *PromotedProcedureTable, allocator: Allocator) void {
        allocator.free(self.procedures);
        self.* = .{};
    }
};

pub const CompileTimeValueStore = struct {
    schemas_len: u32 = 0,
    values_len: u32 = 0,

    pub fn deinit(_: *CompileTimeValueStore, _: Allocator) void {}
};

pub const CheckedModuleArtifact = struct {
    key: CheckedModuleArtifactKey,
    canonical_names: canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    checking_context_identity: CheckingContextIdentity,
    direct_import_artifact_keys: []CheckedModuleArtifactKey = &.{},
    module_env: *const ModuleEnv,
    exports: ExportTable,
    provides_requires: ProvidesRequiresMetadata,
    method_registry: static_dispatch.MethodRegistry,
    static_dispatch_plans: static_dispatch.StaticDispatchPlanTable,
    checked_procedure_templates: CheckedProcedureTemplateTable,
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    top_level_values: TopLevelValueTable,
    promoted_procedures: PromotedProcedureTable,
    compile_time_roots_present: bool = false,
    comptime_values: CompileTimeValueStore,

    pub fn deinit(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.comptime_values.deinit(allocator);
        self.promoted_procedures.deinit(allocator);
        self.top_level_values.deinit(allocator);
        self.platform_required_bindings.deinit(allocator);
        self.hosted_procs.deinit(allocator);
        self.root_requests.deinit(allocator);
        self.checked_procedure_templates.deinit(allocator);
        self.static_dispatch_plans.deinit(allocator);
        self.method_registry.deinit(allocator);
        self.provides_requires.deinit(allocator);
        self.exports.deinit(allocator);
        allocator.free(self.direct_import_artifact_keys);
        self.checking_context_identity.deinit(allocator);
        self.canonical_names.deinit();
    }

    pub fn verifyPublished(self: *const CheckedModuleArtifact) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.module_identity.module_idx != std.math.maxInt(u32));

        for (self.root_requests.requests, 0..) |request, i| {
            std.debug.assert(request.order == i);
            std.debug.assert(request.module_idx == self.module_identity.module_idx);
        }

        std.debug.assert(std.mem.eql(
            u8,
            &self.key.direct_import_artifact_keys_hash,
            &hashDirectImportArtifactKeys(self.direct_import_artifact_keys),
        ));

        for (self.hosted_procs.procs, 0..) |proc, i| {
            std.debug.assert(proc.deterministic_index == i);
            std.debug.assert(proc.module_idx == self.module_identity.module_idx);
        }

        for (self.platform_required_bindings.bindings, 0..) |binding, i| {
            std.debug.assert(binding.requires_idx == i);
            std.debug.assert(binding.module_idx == self.module_identity.module_idx);
        }

        for (self.top_level_values.entries) |entry| {
            switch (entry.value) {
                .const_ref => |const_ref| std.debug.assert(const_ref.module_idx == self.module_identity.module_idx),
                .procedure_binding => |proc_binding| {
                    _ = self.canonical_names.procBase(proc_binding.proc.proc_base);
                    if (proc_binding.template) |template| std.debug.assert(template.proc_base == proc_binding.proc.proc_base);
                },
                .callable_eval_template => {},
            }
        }
    }
};

pub const ImportedModuleView = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    canonical_names: *const canonical.CanonicalNameStore,
    exports: *const ExportTable,
    checked_procedure_templates: *const CheckedProcedureTemplateTable,
    method_registry: *const static_dispatch.MethodRegistry,
    interface_capabilities: *const ModuleInterfaceCapabilities,
    top_level_values: *const TopLevelValueTable,
    comptime_values: *const CompileTimeValueStore,
};

pub const LoweringModuleView = struct {
    artifact: *const CheckedModuleArtifact,
    roots: *const RootRequestTable,
};

pub fn importedView(artifact: *const CheckedModuleArtifact) ImportedModuleView {
    return .{
        .key = artifact.key,
        .module_identity = artifact.module_identity,
        .canonical_names = &artifact.canonical_names,
        .exports = &artifact.exports,
        .checked_procedure_templates = &artifact.checked_procedure_templates,
        .method_registry = &artifact.method_registry,
        .interface_capabilities = &artifact.interface_capabilities,
        .top_level_values = &artifact.top_level_values,
        .comptime_values = &artifact.comptime_values,
    };
}

fn directImportArtifactKeysFromModule(
    allocator: Allocator,
    module: TypedCIR.Module,
    imports: []const PublishImportArtifact,
) Allocator.Error![]CheckedModuleArtifactKey {
    const module_env = module.moduleEnvConst();
    const imported_names = module_env.imports.imports.items.items;
    var keys = std.ArrayList(CheckedModuleArtifactKey).empty;
    errdefer keys.deinit(allocator);

    for (imported_names, 0..) |_, i| {
        const import_idx: CIR.Import.Idx = @enumFromInt(@as(u32, @intCast(i)));
        const resolved_module_idx = module.resolvedImportModule(import_idx) orelse continue;
        const key = publishImportKeyForModule(imports, resolved_module_idx) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact publication invariant violated: import {d} resolved to module {d} without a published artifact key",
                    .{ i, resolved_module_idx },
                );
            }
            unreachable;
        };
        try keys.append(allocator, key);
    }

    return try keys.toOwnedSlice(allocator);
}

fn publishImportKeyForModule(imports: []const PublishImportArtifact, module_idx: u32) ?CheckedModuleArtifactKey {
    for (imports) |import_artifact| {
        if (import_artifact.module_idx == module_idx) return import_artifact.key;
    }
    return null;
}

pub fn loweringView(artifact: *const CheckedModuleArtifact) LoweringModuleView {
    return .{
        .artifact = artifact,
        .roots = &artifact.root_requests,
    };
}

pub fn publishFromTypedModule(
    allocator: Allocator,
    modules: *const TypedCIR.Modules,
    module_idx: u32,
    inputs: PublishInputs,
) Allocator.Error!CheckedModuleArtifact {
    const module = modules.module(module_idx);
    const module_env = module.moduleEnvConst();
    const idents = module.identStoreConst();

    var canonical_names = canonical.CanonicalNameStore.init(allocator);
    errdefer canonical_names.deinit();
    const module_name = try canonical_names.internModuleName(module_env.module_name);
    const display_module_name = try canonical_names.internModuleIdent(idents, module_env.display_module_name_idx);
    const qualified_module_name = try canonical_names.internModuleIdent(idents, module_env.qualified_module_ident);
    const module_identity = ModuleIdentity{
        .module_idx = module_idx,
        .module_name = module_name,
        .display_module_name = display_module_name,
        .qualified_module_name = qualified_module_name,
        .kind = module_env.module_kind,
    };

    var checking_context_identity = try CheckingContextIdentity.fromModule(allocator, module);
    errdefer checking_context_identity.deinit(allocator);

    const direct_import_artifact_keys = try directImportArtifactKeysFromModule(allocator, module, inputs.imports);
    errdefer allocator.free(direct_import_artifact_keys);
    const artifact_key = CheckedModuleArtifactKey.compute(
        module_env.getSourceAll(),
        module_identity,
        checking_context_identity,
        direct_import_artifact_keys,
    );

    const exports = try allocator.dupe(CIR.Def.Idx, module_env.store.sliceDefs(module_env.exports));
    errdefer allocator.free(exports);

    const provides = try allocator.dupe(ModuleEnv.ProvidesEntry, module_env.provides_entries.items.items);
    errdefer allocator.free(provides);

    const requires = try allocator.dupe(ModuleEnv.RequiredType, module_env.requires_types.items.items);
    errdefer allocator.free(requires);

    var checked_procedure_templates = try CheckedProcedureTemplateTable.fromModule(allocator, module, &canonical_names);
    errdefer checked_procedure_templates.deinit(allocator);
    const template_lookup = checked_procedure_templates.asLookup(module_idx);

    var method_registry = try static_dispatch.MethodRegistry.fromTypedModules(allocator, modules, &canonical_names, &template_lookup);
    errdefer method_registry.deinit(allocator);

    var static_dispatch_plans = try static_dispatch.StaticDispatchPlanTable.fromModule(allocator, module, &canonical_names);
    errdefer static_dispatch_plans.deinit(allocator);

    var root_requests = try RootRequestTable.fromModule(allocator, module);
    errdefer root_requests.deinit(allocator);

    var hosted_procs = try HostedProcTable.fromModule(allocator, module, &canonical_names, &checked_procedure_templates);
    errdefer hosted_procs.deinit(allocator);

    var platform_required_bindings = try PlatformRequiredBindingTable.fromModule(allocator, module, &canonical_names);
    errdefer platform_required_bindings.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(allocator, module, &checked_procedure_templates);
    errdefer top_level_values.deinit(allocator);

    var artifact = CheckedModuleArtifact{
        .key = artifact_key,
        .canonical_names = canonical_names,
        .module_identity = module_identity,
        .checking_context_identity = checking_context_identity,
        .direct_import_artifact_keys = direct_import_artifact_keys,
        .module_env = module_env,
        .exports = .{ .defs = exports },
        .provides_requires = .{
            .provides = provides,
            .requires = requires,
        },
        .method_registry = method_registry,
        .static_dispatch_plans = static_dispatch_plans,
        .checked_procedure_templates = checked_procedure_templates,
        .root_requests = root_requests,
        .hosted_procs = hosted_procs,
        .platform_required_bindings = platform_required_bindings,
        .interface_capabilities = ModuleInterfaceCapabilities.fromModule(module),
        .top_level_values = top_level_values,
        .promoted_procedures = .{},
        .comptime_values = .{},
    };
    artifact.verifyPublished();
    return artifact;
}

test "artifact views are read-only projections" {
    var names = canonical.CanonicalNameStore.init(std.testing.allocator);
    const test_module = try names.internModuleName("Test");

    var artifact = CheckedModuleArtifact{
        .key = .{},
        .canonical_names = names,
        .module_identity = .{
            .module_idx = 0,
            .module_name = test_module,
            .display_module_name = test_module,
            .qualified_module_name = test_module,
            .kind = .package,
        },
        .checking_context_identity = .{},
        .module_env = undefined,
        .exports = .{},
        .provides_requires = .{},
        .method_registry = .{},
        .static_dispatch_plans = .{},
        .checked_procedure_templates = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{
            .exported_def_count = 0,
            .method_count = 0,
            .provides_count = 0,
            .requires_count = 0,
        },
        .top_level_values = .{},
        .promoted_procedures = .{},
        .comptime_values = .{},
    };
    defer artifact.deinit(std.testing.allocator);

    const imported = importedView(&artifact);
    const lowering = loweringView(&artifact);
    try std.testing.expect(imported.exports == &artifact.exports);
    try std.testing.expect(lowering.roots == &artifact.root_requests);
}

//! Checked module artifact boundary.
//!
//! Public post-check lowering is moving toward consuming immutable artifacts and
//! narrowed read-only views instead of loose checked modules plus side stores.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const checked_ids = @import("checked_ids.zig");
const static_dispatch = @import("static_dispatch_registry.zig");
const canonical = @import("canonical_names.zig");
const canonical_type_keys = @import("canonical_type_keys.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const CompactWriter = collections.CompactWriter;
const StringLiteral = base.StringLiteral;

/// Public `ModuleEnvStorage` declaration.
pub const ModuleEnvStorage = union(enum) {
    checked_source: *ModuleEnv,
    compiled_buffer: struct {
        env: *ModuleEnv,
        buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    },
    cached_buffer: struct {
        env: *ModuleEnv,
        buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
        source: []const u8,
    },

    pub fn env(self: *const ModuleEnvStorage) *ModuleEnv {
        return switch (self.*) {
            .checked_source => |module_env| module_env,
            .compiled_buffer => |compiled| compiled.env,
            .cached_buffer => |cached| cached.env,
        };
    }

    pub fn envConst(self: *const ModuleEnvStorage) *const ModuleEnv {
        return self.env();
    }

    pub fn deinit(self: *ModuleEnvStorage) void {
        switch (self.*) {
            .checked_source => |module_env| {
                const env_alloc = module_env.gpa;
                const source = module_env.common.source;
                module_env.deinit();
                if (source.len > 0) env_alloc.free(@constCast(source));
                env_alloc.destroy(module_env);
            },
            .compiled_buffer => |compiled| {
                const env_alloc = compiled.env.gpa;
                compiled.env.common.idents.interner.deinit(env_alloc);
                compiled.env.imports.deinitMapOnly(env_alloc);
                env_alloc.destroy(compiled.env);
                env_alloc.free(compiled.buffer);
            },
            .cached_buffer => |cached| {
                const env_alloc = cached.env.gpa;
                cached.env.deinitCachedModule();
                if (cached.source.len > 0) env_alloc.free(@constCast(cached.source));
                env_alloc.destroy(cached.env);
                env_alloc.free(cached.buffer);
            },
        }
        self.* = undefined;
    }
};

/// Public `CheckedModuleArtifactKey` declaration.
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

/// Public `ModuleIdentity` declaration.
pub const ModuleIdentity = struct {
    stable_hash: [32]u8 = [_]u8{0} ** 32,
    module_idx: u32,
    module_name: canonical.ModuleNameId,
    display_module_name: canonical.ModuleNameId,
    qualified_module_name: canonical.ModuleNameId,
    kind: ModuleEnv.ModuleKind,
};

/// Public `ImportIdentity` declaration.
pub const ImportIdentity = struct {
    import_name_hash: [32]u8 = [_]u8{0} ** 32,
    artifact_key: ?CheckedModuleArtifactKey = null,
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

fn hashByteSlice(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    hashU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn hashModuleIdentity(identity: ModuleIdentity) [32]u8 {
    return identity.stable_hash;
}

fn computeStableModuleIdentityHash(module_env: *const ModuleEnv) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashByteSlice(&hasher, module_env.module_name);
    hashByteSlice(&hasher, module_env.getIdentText(module_env.display_module_name_idx));
    hashByteSlice(&hasher, module_env.getIdentText(module_env.qualified_module_ident));
    hasher.update(@tagName(module_env.module_kind));
    return hasher.finalResult();
}

fn hashCheckingContextIdentity(identity: CheckingContextIdentity) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    if (identity.platform_requirement_context) |context| {
        hasher.update(&[_]u8{1});
        hasher.update(&context.bytes);
    } else {
        hasher.update(&[_]u8{0});
    }
    if (identity.platform_app_relation) |relation| {
        hasher.update(&[_]u8{1});
        hasher.update(&relation.bytes);
    } else {
        hasher.update(&[_]u8{0});
    }
    hashU32(&hasher, @intCast(identity.imports.len));
    for (identity.imports) |import_identity| {
        hasher.update(&import_identity.import_name_hash);
        if (import_identity.artifact_key) |key| {
            hasher.update(&[_]u8{1});
            hasher.update(&key.bytes);
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

fn artifactRef(key: CheckedModuleArtifactKey) canonical.ArtifactRef {
    return .{ .bytes = key.bytes };
}

/// Public `PlatformRequirementContextKey` declaration.
pub const PlatformRequirementContextKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,

    pub fn compute(
        platform_identity: ModuleIdentity,
        platform_required_declarations_hash: [32]u8,
    ) PlatformRequirementContextKey {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        const platform_identity_hash = hashModuleIdentity(platform_identity);
        hasher.update(&platform_identity_hash);
        hasher.update(&platform_required_declarations_hash);
        return .{ .bytes = hasher.finalResult() };
    }
};

/// Public `CheckingContextIdentity` declaration.
pub const CheckingContextIdentity = struct {
    imports: []ImportIdentity = &.{},
    platform_requirement_context: ?PlatformRequirementContextKey = null,
    platform_app_relation: ?PlatformAppRelationKey = null,

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        publish_imports: []const PublishImportArtifact,
        platform_requirement_context: ?PlatformRequirementContextKey,
        platform_app_relation: ?PlatformAppRelationKey,
    ) Allocator.Error!CheckingContextIdentity {
        const module_env = module.moduleEnvConst();
        const imported_names = module_env.imports.imports.items.items;
        const imports = try allocator.alloc(ImportIdentity, imported_names.len);
        errdefer allocator.free(imports);

        for (imported_names, 0..) |str_idx, i| {
            const import_idx: CIR.Import.Idx = @enumFromInt(@as(u32, @intCast(i)));
            const resolved_module_idx = module.resolvedImportModule(import_idx);
            imports[i] = .{
                .import_name_hash = hashBytes(module_env.getString(str_idx)),
                .artifact_key = if (resolved_module_idx) |resolved|
                    publishImportKeyForModule(publish_imports, resolved)
                else
                    null,
            };
        }

        return .{
            .imports = imports,
            .platform_requirement_context = platform_requirement_context,
            .platform_app_relation = platform_app_relation,
        };
    }

    pub fn deinit(self: *CheckingContextIdentity, allocator: Allocator) void {
        allocator.free(self.imports);
        self.* = .{};
    }
};

/// Public `PublishImportArtifact` declaration.
pub const PublishImportArtifact = struct {
    module_idx: u32,
    key: CheckedModuleArtifactKey,
    view: ImportedModuleView,
};

/// Public `PublishInputs` declaration.
pub const PublishInputs = struct {
    module_env_storage: ModuleEnvStorage,
    imports: []const PublishImportArtifact = &.{},
    available_artifacts: []const ImportedModuleView = &.{},
    relation_artifacts: []const ImportedModuleView = &.{},
    platform_requirement_context: ?PlatformRequirementContextKey = null,
    platform_app_relation: ?PlatformAppRelation = null,
    explicit_roots: []const ExplicitRootRequestInput = &.{},
    compile_time_finalizer: CompileTimeFinalizer,
};

/// Public `CompileTimeFinalizer` declaration.
pub const CompileTimeFinalizer = struct {
    context: ?*anyopaque = null,
    finalize: *const fn (
        context: ?*anyopaque,
        allocator: Allocator,
        artifact: *CheckedModuleArtifact,
        imports: []const PublishImportArtifact,
        available_artifacts: []const ImportedModuleView,
        relation_artifacts: []const ImportedModuleView,
    ) anyerror!void,

    pub fn run(
        self: CompileTimeFinalizer,
        allocator: Allocator,
        artifact: *CheckedModuleArtifact,
        imports: []const PublishImportArtifact,
        available_artifacts: []const ImportedModuleView,
        relation_artifacts: []const ImportedModuleView,
    ) anyerror!void {
        try self.finalize(self.context, allocator, artifact, imports, available_artifacts, relation_artifacts);
    }
};

/// Public `ExplicitRootRequestInput` declaration.
pub const ExplicitRootRequestInput = struct {
    kind: RootRequestKind,
    source: RootSource,
    abi: RootAbi,
    exposure: RootExposure,
};

/// Public `ExportTable` declaration.
pub const ExportTable = struct {
    defs: []CIR.Def.Idx = &.{},

    pub fn view(self: *const ExportTable) ExportTableView {
        return .{ .defs = self.defs };
    }

    pub fn deinit(self: *ExportTable, allocator: Allocator) void {
        allocator.free(self.defs);
        self.* = .{};
    }
};

/// Public `ExportTableView` declaration.
pub const ExportTableView = struct {
    defs: []const CIR.Def.Idx = &.{},
};

/// Public `ProvidesEntry` declaration.
pub const ProvidesEntry = struct {
    source_name: canonical.ExportNameId,
    ffi_symbol: canonical.ExternalSymbolNameId,
};

/// Public `RequiresEntry` declaration.
pub const RequiresEntry = struct {
    platform_name: canonical.ExportNameId,
    declared_source_ty: canonical.CanonicalTypeSchemeKey,
};

/// Public `ProvidesRequiresMetadata` declaration.
pub const ProvidesRequiresMetadata = struct {
    provides: []ProvidesEntry = &.{},
    requires: []RequiresEntry = &.{},

    pub fn deinit(self: *ProvidesRequiresMetadata, allocator: Allocator) void {
        allocator.free(self.provides);
        allocator.free(self.requires);
        self.* = .{};
    }
};

/// Public `RootRequestKind` declaration.
pub const RootRequestKind = enum {
    runtime_entrypoint,
    provided_export,
    platform_required_binding,
    hosted_export,
    test_expect,
    repl_expr,
    dev_expr,
    compile_time_constant,
    compile_time_callable,
};

/// Public `RootAbi` declaration.
pub const RootAbi = enum {
    roc,
    platform,
    hosted,
    test_expect,
    compile_time,
};

/// Public `RootExposure` declaration.
pub const RootExposure = enum {
    private,
    exported,
    platform_required,
    hosted,
};

/// Public `RootSource` declaration.
pub const RootSource = union(enum) {
    def: CIR.Def.Idx,
    expr: CIR.Expr.Idx,
    statement: CIR.Statement.Idx,
    required_binding: u32,
};

/// Public `RootRequest` declaration.
pub const RootRequest = struct {
    order: u32,
    module_idx: u32,
    kind: RootRequestKind,
    source: RootSource,
    checked_type: CheckedTypeId,
    abi: RootAbi,
    exposure: RootExposure,
    procedure_template: ?canonical.ProcedureTemplateRef = null,
};

/// Public `LoweringEntrypointRequest` declaration.
pub const LoweringEntrypointRequest = union(enum) {
    root: RootRequest,
    const_instance: ConstInstantiationRequest,
    callable_binding_instance: CallableBindingInstantiationRequest,
};

/// Public `CompileTimeEvaluationRequest` declaration.
pub const CompileTimeEvaluationRequest = union(enum) {
    local_root: RootRequest,
    const_instance: ConstInstantiationRequest,
    callable_binding_instance: CallableBindingInstantiationRequest,
};

/// Public `CompileTimeEvaluationPayload` declaration.
pub const CompileTimeEvaluationPayload = union(enum) {
    local_root: CompileTimeRootPayload,
    const_instance: ConstGraphReificationPlanId,
    callable_binding_instance: CallableResultPlanId,
};

/// Public `RootRequestTable` declaration.
pub const RootRequestTable = struct {
    requests: []RootRequest = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypePublication,
        compile_time_roots: *const CompileTimeRootTable,
        entry_wrappers: *const EntryWrapperTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        checked_bodies: *const CheckedBodyStore,
        resolved_value_refs: *const ResolvedValueRefTable,
        explicit_roots: []const ExplicitRootRequestInput,
    ) Allocator.Error!RootRequestTable {
        var requests = std.ArrayList(RootRequest).empty;
        errdefer requests.deinit(allocator);

        const relation_blocked_exprs = try allocator.alloc(?bool, checked_bodies.exprs.len);
        defer allocator.free(relation_blocked_exprs);
        @memset(relation_blocked_exprs, null);

        for (explicit_roots) |root| {
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = root.kind,
                .source = root.source,
                .checked_type = try checkedTypeIdForRootSource(allocator, module, checked_types, root.source),
                .abi = root.abi,
                .exposure = root.exposure,
            });
        }

        try appendPublishedEntrypointRoots(&requests, allocator, module, checked_types);

        for (platform_required_bindings.bindings, 0..) |binding, i| {
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .platform_required_binding,
                .source = .{ .required_binding = @intCast(i) },
                .checked_type = platformRequiredBindingCheckedType(binding),
                .abi = .platform,
                .exposure = .platform_required,
            });
        }

        for (compile_time_roots.roots) |root| {
            if (root.kind != .expect and !try checkedTypeIsConcreteCompileTimeRoot(allocator, &checked_types.store, root.checked_type)) {
                continue;
            }
            if (compileTimeRootDependsOnUnboundPlatformRequirement(
                checked_bodies,
                resolved_value_refs,
                root,
                relation_blocked_exprs,
            )) {
                continue;
            }
            try appendRoot(&requests, allocator, .{
                .module_idx = root.module_idx,
                .kind = switch (root.kind) {
                    .constant => .compile_time_constant,
                    .callable_binding => .compile_time_callable,
                    .expect => .test_expect,
                },
                .source = root.source,
                .checked_type = entryWrapperForRoot(entry_wrappers, root.id).checked_fn_root,
                .abi = switch (root.kind) {
                    .expect => .test_expect,
                    .constant, .callable_binding => .compile_time,
                },
                .exposure = .private,
                .procedure_template = templateForEntryWrapperRoot(entry_wrappers, root.id),
            });
        }

        return .{ .requests = try requests.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *RootRequestTable, allocator: Allocator) void {
        allocator.free(self.requests);
        self.* = .{};
    }
};

fn checkedTypeIsConcreteCompileTimeRoot(
    allocator: Allocator,
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active.deinit();
    return try checkedTypeIsConcreteCompileTimeRootInner(checked_types, root, &active);
}

fn checkedTypeIsConcreteCompileTimeRootInner(
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    if (active.contains(root)) return true;
    try active.put(root, {});
    defer _ = active.remove(root);

    const index = @intFromEnum(root);
    if (index >= checked_types.payloads.len) {
        checkedArtifactInvariant("compile-time root checked type id is out of range", .{});
    }
    return switch (checked_types.payloads[index]) {
        .pending => checkedArtifactInvariant("compile-time root checked type was pending", .{}),
        .flex,
        .rigid,
        => false,
        .empty_record,
        .empty_tag_union,
        => true,
        .alias => |alias| checkedTypeIsConcreteCompileTimeRootInner(checked_types, alias.backing, active),
        .record => |record| (try checkedFieldTypesAreConcreteCompileTimeRoots(checked_types, record.fields, active)) and
            try checkedTypeIsConcreteCompileTimeRootInner(checked_types, record.ext, active),
        .record_unbound => |fields| checkedFieldTypesAreConcreteCompileTimeRoots(checked_types, fields, active),
        .tuple => |items| checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, items, active),
        .nominal => |nominal| checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, nominal.args, active),
        .function => |function| !function.needs_instantiation and
            (try checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, function.args, active)) and
            try checkedTypeIsConcreteCompileTimeRootInner(checked_types, function.ret, active),
        .tag_union => |tag_union| (try checkedTagsAreConcreteCompileTimeRoots(checked_types, tag_union.tags, active)) and
            try checkedTypeIsConcreteCompileTimeRootInner(checked_types, tag_union.ext, active),
    };
}

fn checkedTypeSpanIsConcreteCompileTimeRoot(
    checked_types: *const CheckedTypeStore,
    items: []const CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (items) |item| {
        if (!try checkedTypeIsConcreteCompileTimeRootInner(checked_types, item, active)) return false;
    }
    return true;
}

fn checkedFieldTypesAreConcreteCompileTimeRoots(
    checked_types: *const CheckedTypeStore,
    fields: []const CheckedRecordField,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (fields) |field| {
        if (!try checkedTypeIsConcreteCompileTimeRootInner(checked_types, field.ty, active)) return false;
    }
    return true;
}

fn checkedTagsAreConcreteCompileTimeRoots(
    checked_types: *const CheckedTypeStore,
    tags: []const CheckedTag,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (tags) |tag| {
        if (!try checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, tag.args, active)) return false;
    }
    return true;
}

fn compileTimeRootHasRootRequest(
    requests: []const RootRequest,
    root: CompileTimeRoot,
) bool {
    for (requests) |request| {
        if (request.abi != .compile_time) continue;
        if (!compileTimeRootKindMatchesRequest(root.kind, request.kind)) continue;
        if (!rootSourceMatches(root.source, request.source)) continue;
        return true;
    }
    return false;
}

fn compileTimeRootKindMatchesRequest(
    root_kind: CompileTimeRootKind,
    request_kind: RootRequestKind,
) bool {
    return switch (root_kind) {
        .constant => request_kind == .compile_time_constant,
        .callable_binding => request_kind == .compile_time_callable,
        .expect => request_kind == .test_expect,
    };
}

fn rootSourceMatches(a: RootSource, b: RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |def| def == b.def,
        .expr => |expr| expr == b.expr,
        .statement => |statement| statement == b.statement,
        .required_binding => |binding| binding == b.required_binding,
    };
}

fn compileTimeRootDependsOnUnboundPlatformRequirement(
    checked_bodies: *const CheckedBodyStore,
    resolved_value_refs: *const ResolvedValueRefTable,
    root: CompileTimeRoot,
    relation_blocked_exprs: []?bool,
) bool {
    return switch (root.kind) {
        .constant,
        .callable_binding,
        => exprDependsOnUnboundPlatformRequirement(
            checked_bodies,
            resolved_value_refs,
            root.expr,
            relation_blocked_exprs,
        ),
        .expect => false,
    };
}

fn exprDependsOnUnboundPlatformRequirement(
    checked_bodies: *const CheckedBodyStore,
    resolved_value_refs: *const ResolvedValueRefTable,
    expr_id: CheckedExprId,
    relation_blocked_exprs: []?bool,
) bool {
    const index = @intFromEnum(expr_id);
    if (relation_blocked_exprs[index]) |cached| return cached;

    const data = checked_bodies.exprs[index].data;
    const result = switch (data) {
        .lookup_local => |lookup| resolvedRefIsUnboundPlatformRequirement(resolved_value_refs, lookup.resolved),
        .lookup_external,
        .lookup_required,
        => |ref_id| resolvedRefIsUnboundPlatformRequirement(resolved_value_refs, ref_id),
        .str,
        .list,
        .tuple,
        => |items| exprSpanDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, items, relation_blocked_exprs),
        .match_ => |match| blk: {
            if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, match.cond, relation_blocked_exprs)) break :blk true;
            for (match.branches) |branch| {
                if (branch.guard) |guard| {
                    if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, guard, relation_blocked_exprs)) break :blk true;
                }
                if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, branch.value, relation_blocked_exprs)) break :blk true;
            }
            break :blk false;
        },
        .if_ => |if_| blk: {
            for (if_.branches) |branch| {
                if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, branch.cond, relation_blocked_exprs)) break :blk true;
                if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, branch.body, relation_blocked_exprs)) break :blk true;
            }
            break :blk exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, if_.final_else, relation_blocked_exprs);
        },
        .call => |call| blk: {
            if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, call.func, relation_blocked_exprs)) break :blk true;
            break :blk exprSpanDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, call.args, relation_blocked_exprs);
        },
        .record => |record| blk: {
            if (record.ext) |ext| {
                if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, ext, relation_blocked_exprs)) break :blk true;
            }
            for (record.fields) |field| {
                if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, field.value, relation_blocked_exprs)) break :blk true;
            }
            break :blk false;
        },
        .block => |block| blk: {
            for (block.statements) |statement| {
                if (statementDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, statement, relation_blocked_exprs)) break :blk true;
            }
            break :blk exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, block.final_expr, relation_blocked_exprs);
        },
        .tag => |tag| exprSpanDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, tag.args, relation_blocked_exprs),
        .nominal => |nominal| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, nominal.backing_expr, relation_blocked_exprs),
        .closure => |closure| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, closure.lambda, relation_blocked_exprs),
        .lambda => |lambda| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, lambda.body, relation_blocked_exprs),
        .binop => |binop| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, binop.lhs, relation_blocked_exprs) or
            exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, binop.rhs, relation_blocked_exprs),
        .unary_minus,
        .unary_not,
        .dbg,
        .expect,
        => |child| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, child, relation_blocked_exprs),
        .field_access => |access| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, access.receiver, relation_blocked_exprs),
        .structural_eq => |eq| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, eq.lhs, relation_blocked_exprs) or
            exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, eq.rhs, relation_blocked_exprs),
        .tuple_access => |access| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, access.tuple, relation_blocked_exprs),
        .return_ => |ret| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, ret.expr, relation_blocked_exprs),
        .for_ => |for_| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, for_.expr, relation_blocked_exprs) or
            exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, for_.body, relation_blocked_exprs),
        .run_low_level => |run| exprSpanDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, run.args, relation_blocked_exprs),
        .pending,
        .num,
        .frac_f32,
        .frac_f64,
        .dec,
        .dec_small,
        .typed_int,
        .typed_frac,
        .str_segment,
        .bytes_literal,
        .empty_list,
        .empty_record,
        .zero_argument_tag,
        .dispatch_call,
        .method_eq,
        .type_dispatch_call,
        .runtime_error,
        .crash,
        .ellipsis,
        .anno_only,
        .hosted_lambda,
        => false,
    };

    relation_blocked_exprs[index] = result;
    return result;
}

fn exprSpanDependsOnUnboundPlatformRequirement(
    checked_bodies: *const CheckedBodyStore,
    resolved_value_refs: *const ResolvedValueRefTable,
    exprs: []const CheckedExprId,
    relation_blocked_exprs: []?bool,
) bool {
    for (exprs) |expr_id| {
        if (exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, expr_id, relation_blocked_exprs)) return true;
    }
    return false;
}

fn statementDependsOnUnboundPlatformRequirement(
    checked_bodies: *const CheckedBodyStore,
    resolved_value_refs: *const ResolvedValueRefTable,
    statement_id: CheckedStatementId,
    relation_blocked_exprs: []?bool,
) bool {
    return switch (checked_bodies.statements[@intFromEnum(statement_id)].data) {
        .decl => |statement| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, statement.expr, relation_blocked_exprs),
        .var_ => |statement| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, statement.expr, relation_blocked_exprs),
        .reassign => |statement| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, statement.expr, relation_blocked_exprs),
        .dbg,
        .expr,
        .expect,
        => |expr| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, expr, relation_blocked_exprs),
        .for_ => |for_| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, for_.expr, relation_blocked_exprs) or
            exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, for_.body, relation_blocked_exprs),
        .while_ => |while_| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, while_.cond, relation_blocked_exprs) or
            exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, while_.body, relation_blocked_exprs),
        .return_ => |ret| exprDependsOnUnboundPlatformRequirement(checked_bodies, resolved_value_refs, ret.expr, relation_blocked_exprs),
        .pending,
        .crash,
        .break_,
        .import_,
        .alias_decl,
        .nominal_decl,
        .type_anno,
        .type_var_alias,
        .runtime_error,
        => false,
    };
}

fn resolvedRefIsUnboundPlatformRequirement(
    resolved_value_refs: *const ResolvedValueRefTable,
    maybe_ref: ?ResolvedValueRefId,
) bool {
    const ref_id = maybe_ref orelse return false;
    const index = @intFromEnum(ref_id);
    std.debug.assert(index < resolved_value_refs.records.len);
    return switch (resolved_value_refs.records[index].ref) {
        .platform_required_declaration => true,
        else => false,
    };
}

fn appendPublishedEntrypointRoots(
    requests: *std.ArrayList(RootRequest),
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: *const CheckedTypePublication,
) Allocator.Error!void {
    const module_env = module.moduleEnvConst();

    for (module_env.provides_entries.items.items) |provides_entry| {
        const def_node_idx = module_env.getExposedNodeIndexById(provides_entry.ident) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: provided entry {s} has no top-level definition",
                    .{module_env.getIdent(provides_entry.ident)},
                );
            }
            unreachable;
        };
        const def_idx: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(def_node_idx)));
        try appendRoot(requests, allocator, .{
            .module_idx = module.moduleIndex(),
            .kind = .provided_export,
            .source = .{ .def = def_idx },
            .checked_type = try checkedTypeIdForRootSource(allocator, module, checked_types, .{ .def = def_idx }),
            .abi = .platform,
            .exposure = .exported,
        });
    }

    switch (module_env.module_kind) {
        .default_app => {
            const main_ident = module_env.common.findIdent("main!") orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: default app has no main! identifier",
                        .{},
                    );
                }
                unreachable;
            };
            const main_node_idx = module_env.getExposedNodeIndexById(main_ident) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: default app main! has no published root definition",
                        .{},
                    );
                }
                unreachable;
            };
            const main_def: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(main_node_idx)));
            try appendRoot(requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .runtime_entrypoint,
                .source = .{ .def = main_def },
                .checked_type = try checkedTypeIdForRootSource(allocator, module, checked_types, .{ .def = main_def }),
                .abi = .roc,
                .exposure = .exported,
            });
        },
        else => {},
    }
}

fn checkedTypeIdForRootSource(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: *const CheckedTypePublication,
    source: RootSource,
) Allocator.Error!CheckedTypeId {
    const var_ = switch (source) {
        .def => |def_idx| module.defType(def_idx),
        .expr => |expr_idx| module.exprType(expr_idx),
        .statement => |statement_idx| ModuleEnv.varFrom(statement_idx),
        .required_binding => |binding_idx| blk: {
            const module_env = module.moduleEnvConst();
            if (binding_idx >= module_env.requires_types.items.items.len) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: explicit required-binding root {d} is out of range",
                        .{binding_idx},
                    );
                }
                unreachable;
            }
            break :blk ModuleEnv.varFrom(module_env.requires_types.items.items[binding_idx].type_anno);
        },
    };
    return checkedTypeIdForVar(allocator, module, checked_types, var_);
}

fn platformRequiredBindingCheckedType(binding: PlatformRequiredBinding) CheckedTypeId {
    return switch (binding.value_use) {
        .const_value => |const_use| const_use.const_use.requested_source_ty_payload orelse {
            checkedArtifactInvariant("platform-required const binding missing relation-owned requested payload", .{});
        },
        .procedure_value => |proc_use| proc_use.procedure.source_fn_ty_payload orelse {
            checkedArtifactInvariant("platform-required procedure binding missing relation-owned requested payload", .{});
        },
    };
}

fn checkedTypeIdForVar(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: *const CheckedTypePublication,
    var_: Var,
) Allocator.Error!CheckedTypeId {
    _ = allocator;
    return checked_types.rootForSourceVar(module, var_) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: root request type was not published", .{});
        }
        unreachable;
    };
}

const RootRequestWithoutOrder = struct {
    module_idx: u32,
    kind: RootRequestKind,
    source: RootSource,
    checked_type: CheckedTypeId,
    abi: RootAbi,
    exposure: RootExposure,
    procedure_template: ?canonical.ProcedureTemplateRef = null,
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
        .procedure_template = request.procedure_template,
    });
}

fn templateForEntryWrapperRoot(
    entry_wrappers: *const EntryWrapperTable,
    root: ComptimeRootId,
) canonical.ProcedureTemplateRef {
    return entryWrapperForRoot(entry_wrappers, root).template;
}

fn entryWrapperForRoot(
    entry_wrappers: *const EntryWrapperTable,
    root: ComptimeRootId,
) EntryWrapper {
    const wrapper = entry_wrappers.lookupByRoot(root) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: compile-time/test root has no entry wrapper", .{});
        }
        unreachable;
    };
    return wrapper;
}

fn topLevelExprIsAlreadyProcedure(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_lambda, .e_closure, .e_anno_only, .e_hosted_lambda => true,
        else => false,
    };
}

fn sourceTypeIsFunction(module: TypedCIR.Module, var_: Var) bool {
    return sourceVarIsFunction(module.typeStoreConst(), var_);
}

fn sourceVarIsFunction(store: *const types.Store, var_: Var) bool {
    var current = var_;
    while (true) {
        const resolved = store.resolveVar(current);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current = store.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat| return switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => true,
                else => false,
            },
            .err => return false,
            .flex, .rigid => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: top-level source type {d} was not fully resolved before publication",
                        .{@intFromEnum(var_)},
                    );
                }
                unreachable;
            },
        }
    }
}

fn isHostedProcedureExpr(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_hosted_lambda => true,
        else => false,
    };
}

fn intrinsicForProcedureDef(module: TypedCIR.Module, def_idx: CIR.Def.Idx) ?IntrinsicId {
    const def = module.def(def_idx);
    const expr_ident = switch (def.expr.data) {
        .e_anno_only => |anno| anno.ident,
        else => return null,
    };
    const expr_name = module.getIdent(expr_ident);
    if (std.mem.eql(u8, expr_name, "Builtin.Str.inspect")) return .str_inspect;
    if (std.mem.endsWith(u8, expr_name, ".is_eq")) return .structural_eq;

    if (def.patternName()) |pattern_ident| {
        const pattern_name = module.getIdent(pattern_ident);
        if (std.mem.eql(u8, pattern_name, "Builtin.Str.inspect")) return .str_inspect;
        if (std.mem.endsWith(u8, pattern_name, ".is_eq")) return .structural_eq;
    }

    return null;
}

pub const CheckedBodyId = checked_ids.CheckedBodyId;
pub const CheckedExprId = checked_ids.CheckedExprId;
pub const CheckedPatternId = checked_ids.CheckedPatternId;
pub const CheckedStatementId = checked_ids.CheckedStatementId;
pub const CheckedTypeId = checked_ids.CheckedTypeId;
pub const CheckedTypeSchemeId = checked_ids.CheckedTypeSchemeId;
pub const StaticDispatchPlanId = static_dispatch.StaticDispatchPlanId;
/// Public `PatternBinderId` declaration.
pub const PatternBinderId = enum(u32) { _ };

/// Public `CheckedTypeRoot` declaration.
pub const CheckedTypeRoot = struct {
    id: CheckedTypeId,
    key: canonical.CanonicalTypeKey,
};

/// Public `CheckedTypeScheme` declaration.
pub const CheckedTypeScheme = struct {
    id: CheckedTypeSchemeId,
    key: canonical.CanonicalTypeSchemeKey,
    root: CheckedTypeId,
    generalized_vars: []const CheckedTypeId = &.{},
};

/// Public `CheckedStaticDispatchConstraint` declaration.
pub const CheckedStaticDispatchConstraint = struct {
    fn_name: canonical.MethodNameId,
    fn_ty: CheckedTypeId,
    origin: types.StaticDispatchConstraint.Origin,
    binop_negated: bool = false,
    num_literal: ?types.NumeralInfo = null,
};

/// Public `NumericDefaultPhase` declaration.
pub const NumericDefaultPhase = enum {
    checking_finalized,
    mono_specialization,
};

/// Public `CheckedTypeVariable` declaration.
pub const CheckedTypeVariable = struct {
    name: ?[]const u8 = null,
    constraints: []const CheckedStaticDispatchConstraint = &.{},
    numeric_default_phase: ?NumericDefaultPhase = null,
};

/// Public `CheckedRecordField` declaration.
pub const CheckedRecordField = struct {
    name: canonical.RecordFieldLabelId,
    ty: CheckedTypeId,
};

/// Public `CheckedRecordType` declaration.
pub const CheckedRecordType = struct {
    fields: []const CheckedRecordField,
    ext: CheckedTypeId,
};

/// Public `CheckedTag` declaration.
pub const CheckedTag = struct {
    name: canonical.TagLabelId,
    args: []const CheckedTypeId = &.{},
};

/// Public `CheckedTagUnionType` declaration.
pub const CheckedTagUnionType = struct {
    tags: []const CheckedTag,
    ext: CheckedTypeId,
};

/// Public `CheckedFunctionKind` declaration.
pub const CheckedFunctionKind = enum {
    pure,
    effectful,
    unbound,
};

/// Finalize checker-only function-kind state for post-check source type identity.
pub fn finalizedFunctionKind(kind: CheckedFunctionKind) CheckedFunctionKind {
    return switch (kind) {
        .pure, .unbound => .pure,
        .effectful => .effectful,
    };
}

/// Public `CheckedFunctionType` declaration.
pub const CheckedFunctionType = struct {
    kind: CheckedFunctionKind,
    args: []const CheckedTypeId = &.{},
    ret: CheckedTypeId,
    needs_instantiation: bool,
};

/// Public `CheckedBuiltinNominal` declaration.
pub const CheckedBuiltinNominal = enum {
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    list,
    box,
};

/// Public `CheckedAliasType` declaration.
pub const CheckedAliasType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleNameId,
    backing: CheckedTypeId,
    args: []const CheckedTypeId = &.{},
};

/// Public `CheckedNominalType` declaration.
pub const CheckedNominalType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleNameId,
    builtin: ?CheckedBuiltinNominal = null,
    is_opaque: bool,
    backing: CheckedTypeId,
    args: []const CheckedTypeId = &.{},
};

/// Public `CheckedTypePayload` declaration.
pub const CheckedTypePayload = union(enum) {
    pending,
    flex: CheckedTypeVariable,
    rigid: CheckedTypeVariable,
    alias: CheckedAliasType,
    record: CheckedRecordType,
    record_unbound: []const CheckedRecordField,
    tuple: []const CheckedTypeId,
    nominal: CheckedNominalType,
    function: CheckedFunctionType,
    empty_record,
    tag_union: CheckedTagUnionType,
    empty_tag_union,
};

/// Public `CheckedTypeStoreView` declaration.
pub const CheckedTypeStoreView = struct {
    roots: []const CheckedTypeRoot = &.{},
    schemes: []const CheckedTypeScheme = &.{},
    payloads: []const CheckedTypePayload = &.{},
    nominal_declarations: []const CheckedNominalDeclaration = &.{},

    /// Looks up a published checked type root by canonical source type key.
    pub fn rootForKey(self: CheckedTypeStoreView, key: canonical.CanonicalTypeKey) ?CheckedTypeId {
        for (self.roots) |root| {
            if (std.mem.eql(u8, &root.key.bytes, &key.bytes)) return root.id;
        }
        return null;
    }

    /// Looks up a published checked source scheme by canonical scheme key.
    pub fn schemeForKey(self: CheckedTypeStoreView, key: canonical.CanonicalTypeSchemeKey) ?CheckedTypeScheme {
        for (self.schemes) |scheme| {
            if (std.mem.eql(u8, &scheme.key.bytes, &key.bytes)) return scheme;
        }
        return null;
    }

    /// Returns the canonical key for a checked type root in this view.
    pub fn rootKey(self: CheckedTypeStoreView, root: CheckedTypeId) canonical.CanonicalTypeKey {
        const index: usize = @intFromEnum(root);
        if (index >= self.roots.len) {
            checkedArtifactInvariant("checked type view root key lookup referenced a missing root", .{});
        }
        return self.roots[index].key;
    }

    /// Returns whether a const producer root is already concrete for constant
    /// instantiation keys: no unresolved variables remain in any reachable
    /// compile-time value slot, including function argument and return types.
    pub fn isConcreteConstProducerScheme(
        self: CheckedTypeStoreView,
        allocator: Allocator,
        root: CheckedTypeId,
    ) Allocator.Error!bool {
        var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
        defer active.deinit();
        return try checkedTypeViewIsConcreteConstProducerSchemeInner(self, root, &active);
    }

    pub fn nominalDeclaration(
        self: CheckedTypeStoreView,
        nominal: canonical.NominalTypeKey,
    ) ?CheckedNominalDeclaration {
        for (self.nominal_declarations) |declaration| {
            if (canonicalNominalTypeKeyEql(declaration.nominal, nominal)) return declaration;
        }
        return null;
    }
};

fn checkedTypeViewIsConcreteConstProducerSchemeInner(
    checked_types: CheckedTypeStoreView,
    root: CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    if (active.contains(root)) return true;
    try active.put(root, {});
    defer _ = active.remove(root);

    const index: usize = @intFromEnum(root);
    if (index >= checked_types.payloads.len) {
        checkedArtifactInvariant("const producer checked type view id is out of range", .{});
    }
    return switch (checked_types.payloads[index]) {
        .pending => checkedArtifactInvariant("const producer checked type view was pending", .{}),
        .flex,
        .rigid,
        => false,
        .empty_record,
        .empty_tag_union,
        => true,
        .alias => |alias| (try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, alias.backing, active)) and
            try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, alias.args, active),
        .record => |record| (try checkedTypeViewRecordFieldsAreConcreteConstProducerScheme(checked_types, record.fields, active)) and
            try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, record.ext, active),
        .record_unbound => |fields| checkedTypeViewRecordFieldsAreConcreteConstProducerScheme(checked_types, fields, active),
        .tuple => |items| checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, items, active),
        .nominal => |nominal| blk: {
            if (!try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, nominal.args, active)) break :blk false;
            if (nominal.builtin != null) break :blk true;
            break :blk try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, nominal.backing, active);
        },
        .function => |function| !function.needs_instantiation and
            (try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, function.args, active)) and
            try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, function.ret, active),
        .tag_union => |tag_union| (try checkedTypeViewTagsAreConcreteConstProducerScheme(checked_types, tag_union.tags, active)) and
            try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, tag_union.ext, active),
    };
}

fn checkedTypeViewSpanIsConcreteConstProducerScheme(
    checked_types: CheckedTypeStoreView,
    items: []const CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (items) |item| {
        if (!try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, item, active)) return false;
    }
    return true;
}

fn checkedTypeViewRecordFieldsAreConcreteConstProducerScheme(
    checked_types: CheckedTypeStoreView,
    fields: []const CheckedRecordField,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (fields) |field| {
        if (!try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, field.ty, active)) return false;
    }
    return true;
}

fn checkedTypeViewTagsAreConcreteConstProducerScheme(
    checked_types: CheckedTypeStoreView,
    tags: []const CheckedTag,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (tags) |tag| {
        if (!try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, tag.args, active)) return false;
    }
    return true;
}

/// Public `CheckedNominalDeclaration` declaration.
pub const CheckedNominalDeclaration = struct {
    nominal: canonical.NominalTypeKey,
    declaration_root: CheckedTypeId,
    backing: CheckedTypeId,
    formal_args: []const CheckedTypeId = &.{},
};

const CheckedSourceTypeRoot = struct {
    source_var: Var,
    checked_root: CheckedTypeId,
};

const CheckedTypePublication = struct {
    store: CheckedTypeStore,
    source_type_roots: []CheckedSourceTypeRoot = &.{},

    pub fn rootForSourceVar(self: *const CheckedTypePublication, module: TypedCIR.Module, var_: Var) ?CheckedTypeId {
        const resolved = module.typeStoreConst().resolveVar(var_).var_;
        for (self.source_type_roots) |entry| {
            if (entry.source_var == resolved) return entry.checked_root;
        }
        return null;
    }

    fn deinitIndex(self: *CheckedTypePublication, allocator: Allocator) void {
        allocator.free(self.source_type_roots);
        self.source_type_roots = &.{};
    }

    fn deinit(self: *CheckedTypePublication, allocator: Allocator) void {
        self.deinitIndex(allocator);
        self.store.deinit(allocator);
    }
};

/// Public `CheckedTypeStore` declaration.
pub const CheckedTypeStore = struct {
    roots: []CheckedTypeRoot = &.{},
    schemes: []CheckedTypeScheme = &.{},
    payloads: []CheckedTypePayload = &.{},
    nominal_declarations: []CheckedNominalDeclaration = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
    ) Allocator.Error!CheckedTypePublication {
        var roots = std.ArrayList(CheckedTypeRoot).empty;
        errdefer roots.deinit(allocator);
        var payloads = std.ArrayList(CheckedTypePayload).empty;
        errdefer {
            for (payloads.items) |*payload| deinitCheckedTypePayload(allocator, payload);
            payloads.deinit(allocator);
        }
        var schemes = std.ArrayList(CheckedTypeScheme).empty;
        errdefer {
            for (schemes.items) |scheme| allocator.free(scheme.generalized_vars);
            schemes.deinit(allocator);
        }
        var nominal_declarations = std.ArrayList(CheckedNominalDeclaration).empty;
        errdefer nominal_declarations.deinit(allocator);
        var active = std.AutoHashMap(Var, CheckedTypeId).init(allocator);
        defer active.deinit();
        var local_type_declarations = try LocalTypeDeclarationIndex.init(allocator, module);
        defer local_type_declarations.deinit();

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            const tag = module.nodeTag(node);
            if (isExprNodeTag(tag)) {
                const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
                _ = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, module.exprType(expr_idx));
                switch (module.expr(expr_idx).data) {
                    .e_call => |call| if (call.constraint_fn_var) |constraint_fn_var| {
                        _ = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, constraint_fn_var);
                    },
                    else => {},
                }
            } else if (isPatternNodeTag(tag)) {
                const pattern_source_var = checkedPatternSourceTypeVar(module, @enumFromInt(node_idx));
                _ = try appendCheckedTypeRoot(
                    allocator,
                    module,
                    names,
                    &roots,
                    &payloads,
                    &active,
                    pattern_source_var,
                );
            } else if (isStatementNodeTag(tag)) {
                const statement_idx: CIR.Statement.Idx = @enumFromInt(node_idx);
                switch (module.getStatement(statement_idx)) {
                    .s_alias_decl => _ = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, ModuleEnv.varFrom(statement_idx)),
                    .s_nominal_decl => |nominal| try appendCheckedNominalDeclarationFromStatement(
                        allocator,
                        module,
                        names,
                        &nominal_declarations,
                        &roots,
                        &payloads,
                        &active,
                        &local_type_declarations,
                        statement_idx,
                        nominal.header,
                        nominal.anno,
                        nominal.is_opaque,
                    ),
                    else => {},
                }
            }
        }

        for (module.requiresTypes()) |required_type| {
            const required_var = ModuleEnv.varFrom(required_type.type_anno);
            const root = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, required_var);
            const scheme_key = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                required_var,
            );
            if (findCheckedTypeScheme(schemes.items, scheme_key) == null) {
                const scheme_id: CheckedTypeSchemeId = @enumFromInt(@as(u32, @intCast(schemes.items.len)));
                try schemes.append(allocator, .{
                    .id = scheme_id,
                    .key = scheme_key,
                    .root = root,
                    .generalized_vars = &.{},
                });
            }
        }

        try appendStaticDispatchTypeRoots(allocator, module, names, &roots, &payloads, &active);

        for (module.allDefs()) |def_idx| {
            const root = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, module.defType(def_idx));
            const scheme_key = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                module.defType(def_idx),
            );
            if (findCheckedTypeScheme(schemes.items, scheme_key) == null) {
                const scheme_id: CheckedTypeSchemeId = @enumFromInt(@as(u32, @intCast(schemes.items.len)));
                try schemes.append(allocator, .{
                    .id = scheme_id,
                    .key = scheme_key,
                    .root = root,
                    .generalized_vars = &.{},
                });
            }
        }

        const source_type_roots = try sourceTypeRootsFromIndex(allocator, &active);
        errdefer allocator.free(source_type_roots);

        return .{
            .store = .{
                .roots = try roots.toOwnedSlice(allocator),
                .schemes = try schemes.toOwnedSlice(allocator),
                .payloads = try payloads.toOwnedSlice(allocator),
                .nominal_declarations = try nominal_declarations.toOwnedSlice(allocator),
            },
            .source_type_roots = source_type_roots,
        };
    }

    pub fn view(self: *const CheckedTypeStore) CheckedTypeStoreView {
        return .{
            .roots = self.roots,
            .schemes = self.schemes,
            .payloads = self.payloads,
            .nominal_declarations = self.nominal_declarations,
        };
    }

    pub fn rootForKey(self: *const CheckedTypeStore, key: canonical.CanonicalTypeKey) ?CheckedTypeId {
        for (self.roots) |root| {
            if (std.mem.eql(u8, root.key.bytes[0..], key.bytes[0..])) return root.id;
        }
        return null;
    }

    pub fn schemeForKey(self: *const CheckedTypeStore, key: canonical.CanonicalTypeSchemeKey) ?CheckedTypeScheme {
        for (self.schemes) |scheme| {
            if (std.mem.eql(u8, scheme.key.bytes[0..], key.bytes[0..])) return scheme;
        }
        return null;
    }

    pub fn appendSyntheticFunctionRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        kind: CheckedFunctionKind,
        args: []const CheckedTypeId,
        ret: CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        const finalized_kind = finalizedFunctionKind(kind);
        const key = syntheticFunctionTypeKey(self, finalized_kind, args, ret);
        if (self.rootForKey(key)) |existing| return existing;

        const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.roots.len)));
        const owned_args = try allocator.dupe(CheckedTypeId, args);
        errdefer allocator.free(owned_args);

        const old_roots = self.roots;
        const new_roots = try allocator.alloc(CheckedTypeRoot, old_roots.len + 1);
        @memcpy(new_roots[0..old_roots.len], old_roots);
        new_roots[old_roots.len] = .{ .id = id, .key = key };
        errdefer allocator.free(new_roots);

        const old_payloads = self.payloads;
        const new_payloads = try allocator.alloc(CheckedTypePayload, old_payloads.len + 1);
        @memcpy(new_payloads[0..old_payloads.len], old_payloads);
        new_payloads[old_payloads.len] = .{ .function = .{
            .kind = finalized_kind,
            .args = owned_args,
            .ret = ret,
            .needs_instantiation = false,
        } };
        errdefer allocator.free(new_payloads);

        allocator.free(old_roots);
        allocator.free(old_payloads);
        self.roots = new_roots;
        self.payloads = new_payloads;

        try self.ensureSyntheticSchemeForRoot(allocator, id, key);
        return id;
    }

    /// Reserve a checked type root whose payload will be filled after recursive
    /// cloning has finished.
    pub fn reserveSyntheticTypeRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        key: canonical.CanonicalTypeKey,
    ) Allocator.Error!CheckedTypeId {
        if (self.rootForKey(key)) |existing| return existing;

        const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.roots.len)));

        const old_roots = self.roots;
        const new_roots = try allocator.alloc(CheckedTypeRoot, old_roots.len + 1);
        @memcpy(new_roots[0..old_roots.len], old_roots);
        new_roots[old_roots.len] = .{ .id = id, .key = key };
        errdefer allocator.free(new_roots);

        const old_payloads = self.payloads;
        const new_payloads = try allocator.alloc(CheckedTypePayload, old_payloads.len + 1);
        @memcpy(new_payloads[0..old_payloads.len], old_payloads);
        new_payloads[old_payloads.len] = .pending;
        errdefer allocator.free(new_payloads);

        allocator.free(old_roots);
        allocator.free(old_payloads);
        self.roots = new_roots;
        self.payloads = new_payloads;

        return id;
    }

    /// Fill a previously reserved checked type root with its explicit payload.
    pub fn fillSyntheticTypeRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        root: CheckedTypeId,
        payload: CheckedTypePayload,
    ) Allocator.Error!void {
        const index: usize = @intFromEnum(root);
        if (index >= self.payloads.len) {
            checkedArtifactInvariant("synthetic checked type fill referenced a missing root", .{});
        }
        switch (self.payloads[index]) {
            .pending => {},
            else => checkedArtifactInvariant("synthetic checked type fill referenced an already-filled root", .{}),
        }

        self.payloads[index] = payload;
        errdefer deinitCheckedTypePayload(allocator, &self.payloads[index]);
        try self.ensureSyntheticSchemeForRoot(allocator, root, self.roots[index].key);
    }

    pub fn nominalDeclaration(
        self: *const CheckedTypeStore,
        nominal: canonical.NominalTypeKey,
    ) ?CheckedNominalDeclaration {
        for (self.nominal_declarations) |declaration| {
            if (canonicalNominalTypeKeyEql(declaration.nominal, nominal)) return declaration;
        }
        return null;
    }

    pub fn ensureInstantiatedNominalBackingRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        declaration: CheckedNominalDeclaration,
        actual_args: []const CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        if (declaration.formal_args.len != actual_args.len) {
            checkedArtifactInvariant("nominal backing instantiation arity did not match declaration", .{});
        }
        if (checkedTypeIdSliceEql(declaration.formal_args, actual_args)) return declaration.backing;

        var active = std.AutoHashMap(CheckedTypeId, CheckedTypeId).init(allocator);
        defer active.deinit();

        return try self.cloneCheckedTypeRootSubstituting(
            allocator,
            names,
            declaration.backing,
            declaration.formal_args,
            actual_args,
            &active,
        );
    }

    pub fn ensureSchemeForRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        root: CheckedTypeId,
    ) Allocator.Error!canonical.CanonicalTypeSchemeKey {
        const key = self.roots[@intFromEnum(root)].key;
        const scheme_key = syntheticSchemeKeyForType(key);
        try self.ensureSyntheticSchemeForRoot(allocator, root, key);
        return scheme_key;
    }

    fn ensureSyntheticSchemeForRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        root: CheckedTypeId,
        key: canonical.CanonicalTypeKey,
    ) Allocator.Error!void {
        const scheme_key = syntheticSchemeKeyForType(key);
        if (self.schemeForKey(scheme_key) != null) return;

        const old = self.schemes;
        const next = try allocator.alloc(CheckedTypeScheme, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = .{
            .id = @enumFromInt(@as(u32, @intCast(old.len))),
            .key = scheme_key,
            .root = root,
            .generalized_vars = &.{},
        };
        allocator.free(old);
        self.schemes = next;
    }

    pub fn deinit(self: *CheckedTypeStore, allocator: Allocator) void {
        for (self.payloads) |*payload| deinitCheckedTypePayload(allocator, payload);
        for (self.schemes) |scheme| allocator.free(scheme.generalized_vars);
        allocator.free(self.nominal_declarations);
        allocator.free(self.payloads);
        allocator.free(self.schemes);
        allocator.free(self.roots);
        self.* = .{};
    }

    fn cloneCheckedTypeRootSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        source: CheckedTypeId,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error!CheckedTypeId {
        for (formals, actuals) |formal, actual| {
            if (source == formal) return actual;
        }
        if (active.get(source)) |existing| return existing;

        const source_index: usize = @intFromEnum(source);
        if (source_index >= self.payloads.len or source_index >= self.roots.len) {
            checkedArtifactInvariant("checked type substitution referenced a missing source root {} with {} payloads and {} roots", .{
                source_index,
                self.payloads.len,
                self.roots.len,
            });
        }

        const key = try substitutedCheckedTypeKey(allocator, names, self, source, formals, actuals);
        if (self.rootForKey(key)) |existing| return existing;

        const target = try self.reserveSyntheticTypeRoot(allocator, key);
        errdefer deinitCheckedTypePayload(allocator, &self.payloads[@intFromEnum(target)]);
        try active.put(source, target);
        errdefer _ = active.remove(source);

        const source_payload = self.payloads[source_index];
        if (source_payload == .function and @intFromEnum(source_payload.function.ret) >= self.payloads.len) {
            checkedArtifactInvariant("checked type substitution reached function root {} with missing ret {} and {} payloads", .{
                source_index,
                @intFromEnum(source_payload.function.ret),
                self.payloads.len,
            });
        }
        const payload = try self.cloneCheckedTypePayloadSubstituting(
            allocator,
            names,
            source_payload,
            formals,
            actuals,
            active,
        );
        try self.fillSyntheticTypeRoot(allocator, target, payload);
        _ = active.remove(source);
        return target;
    }

    fn cloneCheckedTypePayloadSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        payload: CheckedTypePayload,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error!CheckedTypePayload {
        return switch (payload) {
            .pending => checkedArtifactInvariant("checked type substitution reached pending payload", .{}),
            .empty_record => .empty_record,
            .empty_tag_union => .empty_tag_union,
            .flex => |flex| .{ .flex = .{
                .name = if (flex.name) |name| try allocator.dupe(u8, name) else null,
                .constraints = try self.cloneCheckedStaticDispatchConstraintsSubstituting(allocator, names, flex.constraints, formals, actuals, active),
                .numeric_default_phase = flex.numeric_default_phase,
            } },
            .rigid => |rigid| .{ .rigid = .{
                .name = if (rigid.name) |name| try allocator.dupe(u8, name) else null,
                .constraints = try self.cloneCheckedStaticDispatchConstraintsSubstituting(allocator, names, rigid.constraints, formals, actuals, active),
                .numeric_default_phase = rigid.numeric_default_phase,
            } },
            .alias => |alias| .{ .alias = .{
                .name = alias.name,
                .origin_module = alias.origin_module,
                .backing = try self.cloneCheckedTypeRootSubstituting(allocator, names, alias.backing, formals, actuals, active),
                .args = try self.cloneCheckedTypeIdSliceSubstituting(allocator, names, alias.args, formals, actuals, active),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.cloneCheckedRecordFieldsSubstituting(allocator, names, record.fields, formals, actuals, active),
                .ext = try self.cloneCheckedTypeRootSubstituting(allocator, names, record.ext, formals, actuals, active),
            } },
            .record_unbound => |fields| .{
                .record_unbound = try self.cloneCheckedRecordFieldsSubstituting(allocator, names, fields, formals, actuals, active),
            },
            .tuple => |elems| .{
                .tuple = try self.cloneCheckedTypeIdSliceSubstituting(allocator, names, elems, formals, actuals, active),
            },
            .nominal => |nominal| .{ .nominal = .{
                .name = nominal.name,
                .origin_module = nominal.origin_module,
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.cloneCheckedTypeRootSubstituting(allocator, names, nominal.backing, formals, actuals, active),
                .args = try self.cloneCheckedTypeIdSliceSubstituting(allocator, names, nominal.args, formals, actuals, active),
            } },
            .function => |function| try self.cloneCheckedFunctionTypeSubstituting(allocator, names, function, formals, actuals, active),
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.cloneCheckedTagsSubstituting(allocator, names, tag_union.tags, formals, actuals, active),
                .ext = try self.cloneCheckedTypeRootSubstituting(allocator, names, tag_union.ext, formals, actuals, active),
            } },
        };
    }

    fn cloneCheckedTypeIdSliceSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        ids: []const CheckedTypeId,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try allocator.alloc(CheckedTypeId, ids.len);
        errdefer allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.cloneCheckedTypeRootSubstituting(allocator, names, id, formals, actuals, active);
        }
        return out;
    }

    fn cloneCheckedRecordFieldsSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        fields: []const CheckedRecordField,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try allocator.alloc(CheckedRecordField, fields.len);
        errdefer allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = field.name,
                .ty = try self.cloneCheckedTypeRootSubstituting(allocator, names, field.ty, formals, actuals, active),
            };
        }
        return out;
    }

    fn cloneCheckedTagsSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        tags: []const CheckedTag,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try allocator.alloc(CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
        errdefer {
            for (out) |tag| allocator.free(tag.args);
            allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = tag.name,
                .args = try self.cloneCheckedTypeIdSliceSubstituting(allocator, names, tag.args, formals, actuals, active),
            };
        }
        return out;
    }

    fn cloneCheckedStaticDispatchConstraintsSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        constraints: []const CheckedStaticDispatchConstraint,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        const out = try allocator.alloc(CheckedStaticDispatchConstraint, constraints.len);
        errdefer allocator.free(out);
        for (constraints, 0..) |constraint, i| {
            out[i] = .{
                .fn_name = constraint.fn_name,
                .fn_ty = try self.cloneCheckedTypeRootSubstituting(allocator, names, constraint.fn_ty, formals, actuals, active),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            };
        }
        return out;
    }

    fn cloneCheckedFunctionTypeSubstituting(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        function: CheckedFunctionType,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error!CheckedTypePayload {
        const args = try self.cloneCheckedTypeIdSliceSubstituting(allocator, names, function.args, formals, actuals, active);
        errdefer allocator.free(args);
        const ret = try self.cloneCheckedTypeRootSubstituting(allocator, names, function.ret, formals, actuals, active);

        return .{ .function = .{
            .kind = finalizedFunctionKind(function.kind),
            .args = args,
            .ret = ret,
            .needs_instantiation = try self.checkedTypeSliceContainsIdentityVariables(allocator, args) or
                try self.checkedTypeContainsIdentityVariables(allocator, ret),
        } };
    }

    fn checkedTypeSliceContainsIdentityVariables(
        self: *const CheckedTypeStore,
        allocator: Allocator,
        roots: []const CheckedTypeId,
    ) Allocator.Error!bool {
        for (roots) |root| {
            if (try self.checkedTypeContainsIdentityVariables(allocator, root)) return true;
        }
        return false;
    }

    fn checkedTypeContainsIdentityVariables(
        self: *const CheckedTypeStore,
        allocator: Allocator,
        root: CheckedTypeId,
    ) Allocator.Error!bool {
        var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
        defer active.deinit();
        return try self.checkedTypeContainsIdentityVariablesHelp(root, &active);
    }

    fn checkedTypeContainsIdentityVariablesHelp(
        self: *const CheckedTypeStore,
        root: CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, void),
    ) Allocator.Error!bool {
        const index: usize = @intFromEnum(root);
        if (index >= self.payloads.len) {
            checkedArtifactInvariant("checked type identity scan referenced a missing payload", .{});
        }
        if (active.contains(root)) return false;
        try active.put(root, {});
        defer _ = active.remove(root);

        return switch (self.payloads[index]) {
            .pending,
            .flex,
            .rigid,
            => true,
            .empty_record,
            .empty_tag_union,
            => false,
            .alias => |alias| blk: {
                if (try self.checkedTypeContainsIdentityVariablesHelp(alias.backing, active)) break :blk true;
                for (alias.args) |arg| {
                    if (try self.checkedTypeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.checkedTypeContainsIdentityVariablesHelp(field.ty, active)) break :blk true;
                }
                break :blk try self.checkedTypeContainsIdentityVariablesHelp(record.ext, active);
            },
            .record_unbound => |fields| blk: {
                for (fields) |field| {
                    if (try self.checkedTypeContainsIdentityVariablesHelp(field.ty, active)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |items| blk: {
                for (items) |item| {
                    if (try self.checkedTypeContainsIdentityVariablesHelp(item, active)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |nominal| blk: {
                for (nominal.args) |arg| {
                    if (try self.checkedTypeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                }
                break :blk false;
            },
            .function => |function| blk: {
                for (function.args) |arg| {
                    if (try self.checkedTypeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                }
                break :blk try self.checkedTypeContainsIdentityVariablesHelp(function.ret, active);
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (try self.checkedTypeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                    }
                }
                break :blk try self.checkedTypeContainsIdentityVariablesHelp(tag_union.ext, active);
            },
        };
    }
};

const LocalTypeDeclarationIndex = struct {
    finalized_by_relative_name: std.AutoHashMap(Ident.Idx, CIR.Statement.Idx),

    fn init(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!LocalTypeDeclarationIndex {
        var finalized_by_relative_name = std.AutoHashMap(Ident.Idx, CIR.Statement.Idx).init(allocator);
        errdefer finalized_by_relative_name.deinit();

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            if (!isStatementNodeTag(module.nodeTag(node))) continue;

            const statement_idx: CIR.Statement.Idx = @enumFromInt(node_idx);
            const statement = module.getStatement(statement_idx);
            const header_idx, const anno_idx = switch (statement) {
                .s_alias_decl => |alias| .{ alias.header, alias.anno },
                .s_nominal_decl => |nominal| .{ nominal.header, nominal.anno },
                else => continue,
            };
            if (anno_idx == .placeholder) continue;

            const header = module.moduleEnvConst().store.getTypeHeader(header_idx);
            if (finalized_by_relative_name.get(header.relative_name)) |existing| {
                if (existing != statement_idx) {
                    checkedArtifactInvariant("checked artifact found duplicate finalized type declarations for one relative name", .{});
                }
                continue;
            }
            try finalized_by_relative_name.put(header.relative_name, statement_idx);
        }

        return .{ .finalized_by_relative_name = finalized_by_relative_name };
    }

    fn deinit(self: *LocalTypeDeclarationIndex) void {
        self.finalized_by_relative_name.deinit();
    }

    fn finalizedStatementForReference(
        self: *const LocalTypeDeclarationIndex,
        module: TypedCIR.Module,
        statement_idx: CIR.Statement.Idx,
    ) CIR.Statement.Idx {
        const statement = module.getStatement(statement_idx);
        const header_idx, const anno_idx = switch (statement) {
            .s_alias_decl => |alias| .{ alias.header, alias.anno },
            .s_nominal_decl => |nominal| .{ nominal.header, nominal.anno },
            else => checkedArtifactInvariant("checked declaration template lookup referenced a non-type declaration", .{}),
        };
        if (anno_idx != .placeholder) return statement_idx;

        const header = module.moduleEnvConst().store.getTypeHeader(header_idx);
        return self.finalized_by_relative_name.get(header.relative_name) orelse {
            checkedArtifactInvariant("checked declaration template lookup referenced an unfinalized associated type placeholder", .{});
        };
    }
};

fn appendCheckedNominalDeclarationFromStatement(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    declarations: *std.ArrayList(CheckedNominalDeclaration),
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    statement_idx: CIR.Statement.Idx,
    header_idx: CIR.TypeHeader.Idx,
    anno_idx: CIR.TypeAnno.Idx,
    is_opaque: bool,
) Allocator.Error!void {
    if (anno_idx == .placeholder) return;

    const statement_root = try appendCheckedTypeRoot(
        allocator,
        module,
        names,
        roots,
        payloads,
        active,
        ModuleEnv.varFrom(statement_idx),
    );
    const statement_root_index: usize = @intFromEnum(statement_root);
    if (statement_root_index >= payloads.items.len) {
        checkedArtifactInvariant("nominal declaration referenced a missing checked type root", .{});
    }

    const statement_nominal = switch (payloads.items[statement_root_index]) {
        .nominal => |nominal| nominal,
        else => checkedArtifactInvariant("nominal declaration statement root was not a nominal checked type", .{}),
    };

    const module_env = module.moduleEnvConst();
    const header = module_env.store.getTypeHeader(header_idx);
    const header_args = module_env.store.sliceTypeAnnos(header.args);

    const formal_args = if (header_args.len == 0) &.{} else blk: {
        const out = try allocator.alloc(CheckedTypeId, header_args.len);
        errdefer allocator.free(out);
        for (header_args, 0..) |arg_anno, i| {
            out[i] = try appendCheckedTypeRoot(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                ModuleEnv.varFrom(arg_anno),
            );
        }
        break :blk out;
    };
    var formal_args_owned = formal_args.len != 0;
    errdefer if (formal_args_owned) allocator.free(formal_args);

    const backing = try appendCheckedTypeRootFromDeclarationAnno(
        allocator,
        module,
        names,
        roots,
        payloads,
        active,
        local_type_declarations,
        anno_idx,
    );

    const nominal_payload = CheckedTypePayload{ .nominal = .{
        .name = statement_nominal.name,
        .origin_module = statement_nominal.origin_module,
        .builtin = statement_nominal.builtin,
        .is_opaque = statement_nominal.is_opaque,
        .backing = backing,
        .args = formal_args,
    } };
    _ = is_opaque;
    formal_args_owned = false;

    const declaration_root = try appendNominalDeclarationRootPayload(
        allocator,
        names,
        roots,
        payloads,
        nominal_payload,
    );
    try appendCheckedNominalDeclarationFromPayload(allocator, declarations, payloads.items, declaration_root);
}

fn appendCheckedTypeRootFromDeclarationAnno(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    anno_idx: CIR.TypeAnno.Idx,
) Allocator.Error!CheckedTypeId {
    const module_env = module.moduleEnvConst();
    const anno = module_env.store.getTypeAnno(anno_idx);
    return switch (anno) {
        .tag_union => |tag_union| blk: {
            const tags = try checkedTagsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                tag_union.tags,
            );
            errdefer deinitCheckedTags(allocator, tags);
            const ext = if (tag_union.ext) |ext_anno|
                try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, roots, payloads, active, local_type_declarations, ext_anno)
            else
                try appendExplicitCheckedTypePayload(allocator, names, roots, payloads, .empty_tag_union);
            break :blk try appendExplicitCheckedTypePayload(allocator, names, roots, payloads, .{ .tag_union = .{
                .tags = tags,
                .ext = ext,
            } });
        },
        .record => |record| blk: {
            const fields = try checkedRecordFieldsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                record.fields,
            );
            errdefer allocator.free(fields);
            const ext = if (record.ext) |ext_anno|
                try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, roots, payloads, active, local_type_declarations, ext_anno)
            else
                try appendExplicitCheckedTypePayload(allocator, names, roots, payloads, .empty_record);
            break :blk try appendExplicitCheckedTypePayload(allocator, names, roots, payloads, .{ .record = .{
                .fields = fields,
                .ext = ext,
            } });
        },
        .tuple => |tuple| blk: {
            const elems = try checkedTypeIdsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                tuple.elems,
            );
            errdefer allocator.free(elems);
            break :blk try appendExplicitCheckedTypePayload(allocator, names, roots, payloads, .{ .tuple = elems });
        },
        .@"fn" => |func| blk: {
            const args = try checkedTypeIdsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                func.args,
            );
            errdefer allocator.free(args);
            const ret = try appendCheckedTypeRootFromDeclarationAnno(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                func.ret,
            );
            const args_need_instantiation = try checkedTypeIdsContainIdentityVariables(allocator, payloads.items, args);
            const needs_instantiation = if (args_need_instantiation)
                true
            else
                try checkedTypeContainsIdentityVariablesPayloads(allocator, payloads.items, ret);
            break :blk try appendExplicitCheckedTypePayload(allocator, names, roots, payloads, .{ .function = .{
                .kind = if (func.effectful) .effectful else .pure,
                .args = args,
                .ret = ret,
                .needs_instantiation = needs_instantiation,
            } });
        },
        .parens => |parens| try appendCheckedTypeRootFromDeclarationAnno(
            allocator,
            module,
            names,
            roots,
            payloads,
            active,
            local_type_declarations,
            parens.anno,
        ),
        .lookup => |lookup| switch (lookup.base) {
            .local => |local| blk: {
                const finalized = local_type_declarations.finalizedStatementForReference(module, local.decl_idx);
                const result = try appendCheckedTypeRoot(
                    allocator,
                    module,
                    names,
                    roots,
                    payloads,
                    active,
                    ModuleEnv.varFrom(finalized),
                );
                break :blk result;
            },
            .builtin,
            .external,
            => try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, ModuleEnv.varFrom(anno_idx)),
            .pending => checkedArtifactInvariant("checked declaration template still contained pending lookup", .{}),
        },
        .apply => |apply| blk: {
            const actual_args = try checkedTypeIdsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                apply.args,
            );
            var actual_args_owned = actual_args.len > 0;
            errdefer if (actual_args_owned) allocator.free(actual_args);
            switch (apply.base) {
                .local => |local| {
                    const finalized = local_type_declarations.finalizedStatementForReference(module, local.decl_idx);
                    if (actual_args.len == 0) {
                        break :blk try appendCheckedTypeRoot(
                            allocator,
                            module,
                            names,
                            roots,
                            payloads,
                            active,
                            ModuleEnv.varFrom(finalized),
                        );
                    }
                    if (finalized != local.decl_idx) {
                        checkedArtifactInvariant("checked declaration template generic application referenced an associated-type placeholder", .{});
                    }
                },
                .builtin,
                .external,
                => {},
                .pending => checkedArtifactInvariant("checked declaration template still contained pending apply", .{}),
            }
            if (actual_args_owned) {
                allocator.free(actual_args);
                actual_args_owned = false;
            }
            break :blk try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, ModuleEnv.varFrom(anno_idx));
        },
        .rigid_var,
        .rigid_var_lookup,
        .underscore,
        => try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, ModuleEnv.varFrom(anno_idx)),
        .tag,
        .malformed,
        => checkedArtifactInvariant("nominal declaration annotation was not a valid checked template", .{}),
    };
}

fn checkedTypeIdsFromDeclarationAnnoSpan(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    span: CIR.TypeAnno.Span,
) Allocator.Error![]const CheckedTypeId {
    const annos = module.moduleEnvConst().store.sliceTypeAnnos(span);
    if (annos.len == 0) return &.{};
    const out = try allocator.alloc(CheckedTypeId, annos.len);
    errdefer allocator.free(out);
    for (annos, 0..) |anno, i| {
        out[i] = try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, roots, payloads, active, local_type_declarations, anno);
    }
    return out;
}

fn checkedRecordFieldsFromDeclarationAnnoSpan(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    span: CIR.TypeAnno.RecordField.Span,
) Allocator.Error![]const CheckedRecordField {
    const fields = module.moduleEnvConst().store.sliceAnnoRecordFields(span);
    if (fields.len == 0) return &.{};
    const out = try allocator.alloc(CheckedRecordField, fields.len);
    errdefer allocator.free(out);
    for (fields, 0..) |field_idx, i| {
        const field = module.moduleEnvConst().store.getAnnoRecordField(field_idx);
        out[i] = .{
            .name = try names.internRecordFieldIdent(module.identStoreConst(), field.name),
            .ty = try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, roots, payloads, active, local_type_declarations, field.ty),
        };
    }
    return out;
}

fn checkedTagsFromDeclarationAnnoSpan(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    span: CIR.TypeAnno.Span,
) Allocator.Error![]const CheckedTag {
    const annos = module.moduleEnvConst().store.sliceTypeAnnos(span);
    if (annos.len == 0) return &.{};
    const out = try allocator.alloc(CheckedTag, annos.len);
    for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
    errdefer deinitCheckedTags(allocator, out);

    for (annos, 0..) |anno_idx, i| {
        const anno = module.moduleEnvConst().store.getTypeAnno(anno_idx);
        const tag = switch (anno) {
            .tag => |tag| tag,
            else => checkedArtifactInvariant("nominal declaration tag union contained a non-tag annotation", .{}),
        };
        out[i] = .{
            .name = try names.internTagIdent(module.identStoreConst(), tag.name),
            .args = try checkedTypeIdsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                roots,
                payloads,
                active,
                local_type_declarations,
                tag.args,
            ),
        };
    }
    return out;
}

fn deinitCheckedTags(allocator: Allocator, tags: []const CheckedTag) void {
    for (tags) |tag| allocator.free(tag.args);
    allocator.free(tags);
}

fn checkedTypeIdsContainIdentityVariables(
    allocator: Allocator,
    payloads: []const CheckedTypePayload,
    ids: []const CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active.deinit();
    for (ids) |id| {
        if (try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, id, &active)) return true;
    }
    return false;
}

fn checkedTypeContainsIdentityVariablesPayloads(
    allocator: Allocator,
    payloads: []const CheckedTypePayload,
    root: CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active.deinit();
    return try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, root, &active);
}

fn checkedTypeContainsIdentityVariablesPayloadsHelp(
    payloads: []const CheckedTypePayload,
    root: CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    const index: usize = @intFromEnum(root);
    if (index >= payloads.len) checkedArtifactInvariant("checked type identity scan referenced a missing payload", .{});
    if (active.contains(root)) return false;
    try active.put(root, {});
    defer _ = active.remove(root);

    return switch (payloads[index]) {
        .pending,
        .flex,
        .rigid,
        => true,
        .empty_record,
        .empty_tag_union,
        => false,
        .alias => |alias| blk: {
            if (try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, alias.backing, active)) break :blk true;
            break :blk try checkedTypeIdsContainIdentityVariablesPayloadsHelp(payloads, alias.args, active);
        },
        .record => |record| blk: {
            for (record.fields) |field| {
                if (try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, field.ty, active)) break :blk true;
            }
            break :blk try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, record.ext, active);
        },
        .record_unbound => |fields| blk: {
            for (fields) |field| {
                if (try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, field.ty, active)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |items| try checkedTypeIdsContainIdentityVariablesPayloadsHelp(payloads, items, active),
        .nominal => |nominal| try checkedTypeIdsContainIdentityVariablesPayloadsHelp(payloads, nominal.args, active),
        .function => |function| blk: {
            if (try checkedTypeIdsContainIdentityVariablesPayloadsHelp(payloads, function.args, active)) break :blk true;
            break :blk try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, function.ret, active);
        },
        .tag_union => |tag_union| blk: {
            for (tag_union.tags) |tag| {
                if (try checkedTypeIdsContainIdentityVariablesPayloadsHelp(payloads, tag.args, active)) break :blk true;
            }
            break :blk try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, tag_union.ext, active);
        },
    };
}

fn checkedTypeIdsContainIdentityVariablesPayloadsHelp(
    payloads: []const CheckedTypePayload,
    ids: []const CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (ids) |id| {
        if (try checkedTypeContainsIdentityVariablesPayloadsHelp(payloads, id, active)) return true;
    }
    return false;
}

fn appendExplicitCheckedTypePayload(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    payload: CheckedTypePayload,
) Allocator.Error!CheckedTypeId {
    var owned_payload = payload;
    var payload_owned = true;
    errdefer if (payload_owned) deinitCheckedTypePayload(allocator, &owned_payload);

    const key = try checkedTypePayloadKey(allocator, names, roots.items, payloads.items, owned_payload);
    if (findCheckedTypeRoot(roots.items, key)) |existing| {
        deinitCheckedTypePayload(allocator, &owned_payload);
        payload_owned = false;
        return existing;
    }

    const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
    try roots.append(allocator, .{ .id = id, .key = key });
    errdefer _ = roots.pop();
    try payloads.append(allocator, owned_payload);
    payload_owned = false;
    return id;
}

fn appendNominalDeclarationRootPayload(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    payload: CheckedTypePayload,
) Allocator.Error!CheckedTypeId {
    var owned_payload = payload;
    var payload_owned = true;
    errdefer if (payload_owned) deinitCheckedTypePayload(allocator, &owned_payload);

    const nominal = switch (owned_payload) {
        .nominal => |nominal_payload| nominal_payload,
        else => checkedArtifactInvariant("nominal declaration root payload was not nominal", .{}),
    };

    const key = try checkedTypePayloadKey(allocator, names, roots.items, payloads.items, owned_payload);
    if (findCheckedTypeRoot(roots.items, key)) |existing| {
        const index: usize = @intFromEnum(existing);
        if (index >= payloads.items.len) {
            checkedArtifactInvariant("nominal declaration root key referenced a missing payload", .{});
        }
        const existing_nominal = switch (payloads.items[index]) {
            .nominal => |existing_payload| existing_payload,
            else => checkedArtifactInvariant("nominal declaration key collided with a non-nominal payload", .{}),
        };
        if (existing_nominal.name != nominal.name or
            existing_nominal.origin_module != nominal.origin_module or
            existing_nominal.is_opaque != nominal.is_opaque or
            !checkedTypeIdSliceEql(existing_nominal.args, nominal.args))
        {
            checkedArtifactInvariant("nominal declaration key collided with a different nominal payload", .{});
        }

        if (existing_nominal.backing == nominal.backing and existing_nominal.builtin == nominal.builtin) {
            deinitCheckedTypePayload(allocator, &owned_payload);
            payload_owned = false;
            return existing;
        }

        deinitCheckedTypePayload(allocator, &payloads.items[index]);
        payloads.items[index] = owned_payload;
        payload_owned = false;
        return existing;
    }

    const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
    try roots.append(allocator, .{ .id = id, .key = key });
    errdefer _ = roots.pop();
    try payloads.append(allocator, owned_payload);
    payload_owned = false;
    return id;
}

fn checkedTypePayloadKey(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    roots: []const CheckedTypeRoot,
    payloads: []const CheckedTypePayload,
    payload: CheckedTypePayload,
) Allocator.Error!canonical.CanonicalTypeKey {
    const store = CheckedTypeStore{
        .roots = @constCast(roots),
        .payloads = @constCast(payloads),
        .schemes = &.{},
        .nominal_declarations = &.{},
    };
    var builder = SubstitutedCheckedTypeKeyBuilder.init(allocator, names, &store, &.{}, &.{});
    defer builder.deinit();
    try builder.writePayload(payload);
    return .{ .bytes = builder.hasher.finalResult() };
}

fn appendCheckedNominalDeclarationFromPayload(
    allocator: Allocator,
    declarations: *std.ArrayList(CheckedNominalDeclaration),
    payloads: []const CheckedTypePayload,
    root: CheckedTypeId,
) Allocator.Error!void {
    const index: usize = @intFromEnum(root);
    if (index >= payloads.len) {
        checkedArtifactInvariant("nominal declaration referenced a missing checked type root", .{});
    }
    const nominal = switch (payloads[index]) {
        .nominal => |nominal| nominal,
        else => checkedArtifactInvariant("nominal declaration root was not a nominal checked type", .{}),
    };

    const nominal_key = canonical.NominalTypeKey{
        .module_name = nominal.origin_module,
        .type_name = nominal.name,
    };
    for (declarations.items) |existing| {
        if (canonicalNominalTypeKeyEql(existing.nominal, nominal_key)) {
            if (existing.backing == nominal.backing and checkedTypeIdSliceEql(existing.formal_args, nominal.args)) {
                return;
            }
            checkedArtifactInvariant("checked artifact attempted to publish conflicting nominal declarations", .{});
        }
    }

    try declarations.append(allocator, .{
        .nominal = nominal_key,
        .declaration_root = root,
        .backing = nominal.backing,
        .formal_args = nominal.args,
    });
}

fn checkedTypeIdSliceEql(a: []const CheckedTypeId, b: []const CheckedTypeId) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left != right) return false;
    }
    return true;
}

fn substitutedCheckedTypeKey(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    store: *const CheckedTypeStore,
    source: CheckedTypeId,
    formals: []const CheckedTypeId,
    actuals: []const CheckedTypeId,
) Allocator.Error!canonical.CanonicalTypeKey {
    if (formals.len != actuals.len) {
        checkedArtifactInvariant("checked type substitution key arity mismatch", .{});
    }

    var builder = SubstitutedCheckedTypeKeyBuilder.init(allocator, names, store, formals, actuals);
    defer builder.deinit();
    try builder.writeType(source);
    return .{ .bytes = builder.hasher.finalResult() };
}

const SubstitutedCheckedTypeKeyBuilder = struct {
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    store: *const CheckedTypeStore,
    formals: []const CheckedTypeId,
    actuals: []const CheckedTypeId,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.AutoHashMap(CheckedTypeId, u32),
    identity_variables: std.AutoHashMap(CheckedTypeId, u32),

    const RecordFieldForKey = struct {
        name: canonical.RecordFieldLabelId,
        ty: CheckedTypeId,
    };

    const TagForKey = struct {
        name: canonical.TagLabelId,
        args: []const CheckedTypeId,
    };

    fn init(
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        store: *const CheckedTypeStore,
        formals: []const CheckedTypeId,
        actuals: []const CheckedTypeId,
    ) SubstitutedCheckedTypeKeyBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .store = store,
            .formals = formals,
            .actuals = actuals,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(CheckedTypeId, u32).init(allocator),
            .identity_variables = std.AutoHashMap(CheckedTypeId, u32).init(allocator),
        };
    }

    fn deinit(self: *SubstitutedCheckedTypeKeyBuilder) void {
        self.identity_variables.deinit();
        self.active.deinit();
    }

    fn substitutedRoot(self: *const SubstitutedCheckedTypeKeyBuilder, source: CheckedTypeId) CheckedTypeId {
        for (self.formals, self.actuals) |formal, actual| {
            if (source == formal and source != actual) return actual;
        }
        return source;
    }

    fn writeType(self: *SubstitutedCheckedTypeKeyBuilder, source: CheckedTypeId) Allocator.Error!void {
        const id = self.substitutedRoot(source);
        const raw: usize = @intFromEnum(id);
        if (raw >= self.store.payloads.len) {
            checkedArtifactInvariant("checked type substitution key referenced a missing payload", .{});
        }

        switch (self.store.payloads[raw]) {
            .flex => |flex| return try self.writeIdentityVariable(id, "flex", flex.name, flex.constraints),
            .rigid => |rigid| return try self.writeIdentityVariable(id, "rigid", rigid.name, rigid.constraints),
            else => {},
        }

        if (self.active.get(id)) |slot| {
            self.writeTag("cycle");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.active.count());
        try self.active.put(id, slot);
        try self.writePayload(self.store.payloads[raw]);
        _ = self.active.remove(id);
    }

    fn writeIdentityVariable(
        self: *SubstitutedCheckedTypeKeyBuilder,
        root: CheckedTypeId,
        comptime tag: []const u8,
        name: ?[]const u8,
        constraints: []const CheckedStaticDispatchConstraint,
    ) Allocator.Error!void {
        if (self.identity_variables.get(root)) |slot| {
            self.writeTag("identity_var_ref");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.identity_variables.count());
        try self.identity_variables.put(root, slot);
        self.writeTag(tag);
        self.writeU32(slot);
        self.writeBool(name != null);
        if (name) |text| self.writeBytes(text);
        try self.writeConstraints(constraints);
    }

    fn writePayload(self: *SubstitutedCheckedTypeKeyBuilder, payload: CheckedTypePayload) Allocator.Error!void {
        switch (payload) {
            .pending => checkedArtifactInvariant("checked type substitution key reached pending payload", .{}),
            .flex,
            .rigid,
            => checkedArtifactInvariant("checked type substitution key reached identity payload without root identity", .{}),
            .alias => |alias| {
                self.writeTag("alias");
                self.writeBytes(self.names.typeNameText(alias.name));
                self.writeBytes(self.names.moduleNameText(alias.origin_module));
                try self.writeType(alias.backing);
                self.writeU32(@intCast(alias.args.len));
                for (alias.args) |arg| try self.writeType(arg);
            },
            .record_unbound => |fields| {
                self.writeTag("record_unbound");
                try self.writeNormalizedRecordFields(fields, null);
            },
            .record => |record| {
                self.writeTag("record");
                try self.writeNormalizedRecordFields(record.fields, record.ext);
            },
            .tuple => |tuple| {
                self.writeTag("tuple");
                self.writeU32(@intCast(tuple.len));
                for (tuple) |elem| try self.writeType(elem);
            },
            .nominal => |nominal| {
                self.writeTag("nominal");
                self.writeBytes(self.names.typeNameText(nominal.name));
                self.writeBytes(self.names.moduleNameText(nominal.origin_module));
                self.writeBool(nominal.is_opaque);
                self.writeU32(@intCast(nominal.args.len));
                for (nominal.args) |arg| try self.writeType(arg);
            },
            .function => |func| {
                switch (finalizedFunctionKind(func.kind)) {
                    .pure => self.writeTag("fn_pure"),
                    .effectful => self.writeTag("fn_effectful"),
                    .unbound => unreachable,
                }
                self.writeBool(try self.typeSliceContainsIdentityVariables(func.args) or
                    try self.typeContainsIdentityVariables(func.ret));
                self.writeU32(@intCast(func.args.len));
                for (func.args) |arg| try self.writeType(arg);
                try self.writeType(func.ret);
            },
            .empty_record => self.writeTag("empty_record"),
            .tag_union => |tag_union| {
                self.writeTag("tag_union");
                try self.writeNormalizedTags(tag_union.tags, tag_union.ext);
            },
            .empty_tag_union => self.writeTag("empty_tag_union"),
        }
    }

    fn appendRecordFieldsForKey(
        self: *SubstitutedCheckedTypeKeyBuilder,
        fields: *std.ArrayList(RecordFieldForKey),
        source: []const CheckedRecordField,
    ) Allocator.Error!void {
        for (source) |field| {
            try fields.append(self.allocator, .{
                .name = field.name,
                .ty = self.substitutedRoot(field.ty),
            });
        }
    }

    fn writeNormalizedRecordFields(
        self: *SubstitutedCheckedTypeKeyBuilder,
        head: []const CheckedRecordField,
        ext: ?CheckedTypeId,
    ) Allocator.Error!void {
        var fields = std.ArrayList(RecordFieldForKey).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFieldsForKey(&fields, head);

        var tail = if (ext) |tail_id| self.substitutedRoot(tail_id) else null;
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();
        while (tail) |tail_id| {
            if (self.active.contains(tail_id)) break;
            if (seen.contains(tail_id)) {
                checkedArtifactInvariant("checked type substitution key row normalization reached a cyclic record row", .{});
            }
            try seen.put(tail_id, {});
            const raw: usize = @intFromEnum(tail_id);
            if (raw >= self.store.payloads.len) {
                checkedArtifactInvariant("checked type substitution key row normalization referenced missing record tail", .{});
            }
            switch (self.store.payloads[raw]) {
                .empty_record => {
                    tail = null;
                    break;
                },
                .record => |record| {
                    try self.appendRecordFieldsForKey(&fields, record.fields);
                    tail = self.substitutedRoot(record.ext);
                },
                .record_unbound => |record_fields| {
                    try self.appendRecordFieldsForKey(&fields, record_fields);
                    tail = null;
                },
                else => break,
            }
        }

        std.mem.sort(RecordFieldForKey, fields.items, self, recordFieldForKeyLessThan);
        self.writeU32(@intCast(fields.items.len));
        for (fields.items, 0..) |field, index| {
            if (index > 0 and std.mem.eql(u8, self.names.recordFieldLabelText(fields.items[index - 1].name), self.names.recordFieldLabelText(field.name))) {
                checkedArtifactInvariant("checked type substitution key row normalization found duplicate record fields", .{});
            }
            self.writeBytes(self.names.recordFieldLabelText(field.name));
            try self.writeType(field.ty);
        }
        if (tail) |tail_id| {
            try self.writeType(tail_id);
        } else {
            self.writeTag("empty_record");
        }
    }

    fn appendTagsForKey(
        self: *SubstitutedCheckedTypeKeyBuilder,
        tags: *std.ArrayList(TagForKey),
        source: []const CheckedTag,
    ) Allocator.Error!void {
        for (source) |tag| {
            try tags.append(self.allocator, .{
                .name = tag.name,
                .args = tag.args,
            });
        }
    }

    fn writeNormalizedTags(
        self: *SubstitutedCheckedTypeKeyBuilder,
        head: []const CheckedTag,
        ext: CheckedTypeId,
    ) Allocator.Error!void {
        var tags = std.ArrayList(TagForKey).empty;
        defer tags.deinit(self.allocator);
        try self.appendTagsForKey(&tags, head);

        var tail: ?CheckedTypeId = self.substitutedRoot(ext);
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();
        while (tail) |tail_id| {
            if (self.active.contains(tail_id)) break;
            if (seen.contains(tail_id)) {
                checkedArtifactInvariant("checked type substitution key row normalization reached a cyclic tag row", .{});
            }
            try seen.put(tail_id, {});
            const raw: usize = @intFromEnum(tail_id);
            if (raw >= self.store.payloads.len) {
                checkedArtifactInvariant("checked type substitution key row normalization referenced missing tag tail", .{});
            }
            switch (self.store.payloads[raw]) {
                .empty_tag_union => {
                    tail = null;
                    break;
                },
                .tag_union => |tag_union| {
                    try self.appendTagsForKey(&tags, tag_union.tags);
                    tail = self.substitutedRoot(tag_union.ext);
                },
                else => break,
            }
        }

        std.mem.sort(TagForKey, tags.items, self, tagForKeyLessThan);
        self.writeU32(@intCast(tags.items.len));
        for (tags.items, 0..) |tag, index| {
            if (index > 0 and std.mem.eql(u8, self.names.tagLabelText(tags.items[index - 1].name), self.names.tagLabelText(tag.name))) {
                checkedArtifactInvariant("checked type substitution key row normalization found duplicate tags", .{});
            }
            self.writeBytes(self.names.tagLabelText(tag.name));
            self.writeU32(@intCast(tag.args.len));
            for (tag.args) |arg| try self.writeType(arg);
        }
        if (tail) |tail_id| {
            try self.writeType(tail_id);
        } else {
            self.writeTag("empty_tag_union");
        }
    }

    fn typeSliceContainsIdentityVariables(
        self: *SubstitutedCheckedTypeKeyBuilder,
        roots: []const CheckedTypeId,
    ) Allocator.Error!bool {
        for (roots) |root| {
            if (try self.typeContainsIdentityVariables(root)) return true;
        }
        return false;
    }

    fn typeContainsIdentityVariables(
        self: *SubstitutedCheckedTypeKeyBuilder,
        root: CheckedTypeId,
    ) Allocator.Error!bool {
        var active = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer active.deinit();
        return try self.typeContainsIdentityVariablesHelp(root, &active);
    }

    fn typeContainsIdentityVariablesHelp(
        self: *SubstitutedCheckedTypeKeyBuilder,
        source: CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, void),
    ) Allocator.Error!bool {
        const id = self.substitutedRoot(source);
        const raw: usize = @intFromEnum(id);
        if (raw >= self.store.payloads.len) {
            checkedArtifactInvariant("checked type substitution key identity scan referenced missing payload", .{});
        }
        if (active.contains(id)) return false;
        try active.put(id, {});
        defer _ = active.remove(id);

        return switch (self.store.payloads[raw]) {
            .pending,
            .flex,
            .rigid,
            => true,
            .empty_record,
            .empty_tag_union,
            => false,
            .alias => |alias| blk: {
                if (try self.typeContainsIdentityVariablesHelp(alias.backing, active)) break :blk true;
                for (alias.args) |arg| {
                    if (try self.typeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.typeContainsIdentityVariablesHelp(field.ty, active)) break :blk true;
                }
                break :blk try self.typeContainsIdentityVariablesHelp(record.ext, active);
            },
            .record_unbound => |fields| blk: {
                for (fields) |field| {
                    if (try self.typeContainsIdentityVariablesHelp(field.ty, active)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |items| blk: {
                for (items) |item| {
                    if (try self.typeContainsIdentityVariablesHelp(item, active)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |nominal| blk: {
                for (nominal.args) |arg| {
                    if (try self.typeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                }
                break :blk false;
            },
            .function => |function| blk: {
                for (function.args) |arg| {
                    if (try self.typeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                }
                break :blk try self.typeContainsIdentityVariablesHelp(function.ret, active);
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (try self.typeContainsIdentityVariablesHelp(arg, active)) break :blk true;
                    }
                }
                break :blk try self.typeContainsIdentityVariablesHelp(tag_union.ext, active);
            },
        };
    }

    fn recordFieldForKeyLessThan(self: *SubstitutedCheckedTypeKeyBuilder, lhs: RecordFieldForKey, rhs: RecordFieldForKey) bool {
        return std.mem.lessThan(u8, self.names.recordFieldLabelText(lhs.name), self.names.recordFieldLabelText(rhs.name));
    }

    fn tagForKeyLessThan(self: *SubstitutedCheckedTypeKeyBuilder, lhs: TagForKey, rhs: TagForKey) bool {
        return std.mem.lessThan(u8, self.names.tagLabelText(lhs.name), self.names.tagLabelText(rhs.name));
    }

    fn writeConstraints(
        self: *SubstitutedCheckedTypeKeyBuilder,
        constraints: []const CheckedStaticDispatchConstraint,
    ) Allocator.Error!void {
        self.writeU32(@intCast(constraints.len));
        for (constraints) |constraint| {
            self.writeBytes(self.names.methodNameText(constraint.fn_name));
            try self.writeType(constraint.fn_ty);
            self.writeTag(@tagName(constraint.origin));
            self.writeBool(constraint.binop_negated);
            self.writeBool(constraint.num_literal != null);
            if (constraint.num_literal) |num_literal| {
                self.hasher.update(&num_literal.bytes);
                self.writeBool(num_literal.is_u128);
                self.writeBool(num_literal.is_negative);
                self.writeBool(num_literal.is_fractional);
            }
        }
    }

    fn writeTag(self: *SubstitutedCheckedTypeKeyBuilder, tag: []const u8) void {
        self.writeBytes(tag);
    }

    fn writeBytes(self: *SubstitutedCheckedTypeKeyBuilder, bytes: []const u8) void {
        self.writeU32(@intCast(bytes.len));
        self.hasher.update(bytes);
    }

    fn writeBool(self: *SubstitutedCheckedTypeKeyBuilder, value: bool) void {
        const byte: [1]u8 = if (value) .{1} else .{0};
        self.hasher.update(&byte);
    }

    fn writeU32(self: *SubstitutedCheckedTypeKeyBuilder, value: u32) void {
        var bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &bytes, value, .little);
        self.hasher.update(&bytes);
    }
};

fn appendStaticDispatchTypeRoots(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
) Allocator.Error!void {
    var node_idx: u32 = 0;
    while (node_idx < module.nodeCount()) : (node_idx += 1) {
        const tag = module.nodeTag(@enumFromInt(node_idx));
        switch (tag) {
            .expr_dispatch_call,
            .expr_type_dispatch_call,
            .expr_method_eq,
            => {},
            else => continue,
        }

        const expr = module.expr(@enumFromInt(node_idx));
        switch (expr.data) {
            .e_dispatch_call => |dispatch_call| {
                _ = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, module.exprType(dispatch_call.receiver));
                _ = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, dispatch_call.constraint_fn_var);
            },
            .e_type_dispatch_call => |dispatch_call| {
                const alias_stmt = module.getStatement(dispatch_call.type_var_alias_stmt);
                _ = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, ModuleEnv.varFrom(alias_stmt.s_type_var_alias.type_var_anno));
                _ = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, dispatch_call.constraint_fn_var);
            },
            .e_method_eq => |eq| {
                _ = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, module.exprType(eq.lhs));
                _ = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, eq.constraint_fn_var);
            },
            else => unreachable,
        }
    }
}

fn syntheticFunctionTypeKey(
    store: *const CheckedTypeStore,
    kind: CheckedFunctionKind,
    args: []const CheckedTypeId,
    ret: CheckedTypeId,
) canonical.CanonicalTypeKey {
    const finalized_kind = finalizedFunctionKind(kind);
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashByteSlice(&hasher, "checked_synthetic_function");
    hashByteSlice(&hasher, @tagName(finalized_kind));
    hashU32(&hasher, @intCast(args.len));
    for (args) |arg| {
        hasher.update(&store.roots[@intFromEnum(arg)].key.bytes);
    }
    hasher.update(&store.roots[@intFromEnum(ret)].key.bytes);
    return .{ .bytes = hasher.finalResult() };
}

fn syntheticSchemeKeyForType(key: canonical.CanonicalTypeKey) canonical.CanonicalTypeSchemeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashByteSlice(&hasher, "checked_synthetic_type_scheme");
    hasher.update(&key.bytes);
    return .{ .bytes = hasher.finalResult() };
}

fn appendCheckedTypeRoot(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    var_: Var,
) Allocator.Error!CheckedTypeId {
    const resolved = module.typeStoreConst().resolveVar(var_);
    const resolved_var = resolved.var_;
    if (active.get(resolved_var)) |id| return id;

    const key_info = try canonical_type_keys.fromVarInfo(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        resolved_var,
    );
    if (!key_info.contains_identity_variables) {
        if (findCheckedTypeRoot(roots.items, key_info.key)) |id| {
            try active.put(resolved_var, id);
            return id;
        }
    }

    const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
    try roots.append(allocator, .{ .id = id, .key = key_info.key });
    errdefer _ = roots.pop();
    try payloads.append(allocator, .pending);
    errdefer _ = payloads.pop();

    try active.put(resolved_var, id);
    errdefer _ = active.remove(resolved_var);
    const payload = try copyCheckedTypePayload(
        allocator,
        module,
        names,
        roots,
        payloads,
        active,
        resolved.desc.content,
    );

    deinitCheckedTypePayload(allocator, &payloads.items[@intFromEnum(id)]);
    payloads.items[@intFromEnum(id)] = payload;
    return id;
}

fn sourceTypeRootsFromIndex(
    allocator: Allocator,
    index: *const std.AutoHashMap(Var, CheckedTypeId),
) Allocator.Error![]CheckedSourceTypeRoot {
    if (index.count() == 0) return &.{};
    const out = try allocator.alloc(CheckedSourceTypeRoot, index.count());
    var it = index.iterator();
    var i: usize = 0;
    while (it.next()) |entry| : (i += 1) {
        out[i] = .{
            .source_var = entry.key_ptr.*,
            .checked_root = entry.value_ptr.*,
        };
    }
    return out;
}

fn copyCheckedTypePayload(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    content: types.Content,
) Allocator.Error!CheckedTypePayload {
    return switch (content) {
        .err => {
            if (builtin.mode == .Debug) {
                std.debug.panic("checked artifact invariant violated: erroneous checked type reached artifact publication", .{});
            }
            unreachable;
        },
        .flex => |flex| .{ .flex = .{
            .name = try copyOptionalIdentText(allocator, module, flex.name),
            .constraints = try copyCheckedStaticDispatchConstraints(allocator, module, names, roots, payloads, active, flex.constraints),
            .numeric_default_phase = numericDefaultPhaseForFlex(module, flex),
        } },
        .rigid => |rigid| .{ .rigid = .{
            .name = try copyIdentText(allocator, module, rigid.name),
            .constraints = try copyCheckedStaticDispatchConstraints(allocator, module, names, roots, payloads, active, rigid.constraints),
            .numeric_default_phase = numericDefaultPhaseForConstraints(module, rigid.constraints),
        } },
        .alias => |alias| .{ .alias = .{
            .name = try names.internTypeIdent(module.identStoreConst(), alias.ident.ident_idx),
            .origin_module = try names.internModuleIdent(module.identStoreConst(), alias.origin_module),
            .backing = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, module.typeStoreConst().getAliasBackingVar(alias)),
            .args = try copyCheckedTypeRange(allocator, module, names, roots, payloads, active, module.typeStoreConst().sliceAliasArgs(alias)),
        } },
        .structure => |flat| try copyCheckedFlatType(allocator, module, names, roots, payloads, active, flat),
    };
}

fn numericDefaultPhaseForFlex(module: TypedCIR.Module, flex: types.Flex) ?NumericDefaultPhase {
    return numericDefaultPhaseForConstraints(module, flex.constraints);
}

fn numericDefaultPhaseForConstraints(
    module: TypedCIR.Module,
    constraints_range: types.StaticDispatchConstraint.SafeList.Range,
) ?NumericDefaultPhase {
    const constraints = module.typeStoreConst().sliceStaticDispatchConstraints(constraints_range);
    for (constraints) |constraint| {
        if (constraint.origin == .from_numeral) return .mono_specialization;
        if (isDefaultableArithmeticConstraint(module, constraint)) return .mono_specialization;
    }
    return null;
}

fn isDefaultableArithmeticConstraint(
    module: TypedCIR.Module,
    constraint: types.StaticDispatchConstraint,
) bool {
    const idents = module.commonIdents();
    return switch (constraint.origin) {
        .desugared_binop => constraint.fn_name.eql(idents.plus) or
            constraint.fn_name.eql(idents.minus) or
            constraint.fn_name.eql(idents.times) or
            constraint.fn_name.eql(idents.div_by) or
            constraint.fn_name.eql(idents.div_trunc_by) or
            constraint.fn_name.eql(idents.rem_by),
        .desugared_unaryop => constraint.fn_name.eql(idents.negate),
        .from_numeral,
        .method_call,
        .where_clause,
        => false,
    };
}

fn copyCheckedFlatType(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    flat: types.FlatType,
) Allocator.Error!CheckedTypePayload {
    return switch (flat) {
        .empty_record => .empty_record,
        .empty_tag_union => .empty_tag_union,
        .record_unbound => |fields| .{
            .record_unbound = try copyCheckedRecordFields(allocator, module, names, roots, payloads, active, fields),
        },
        .record => |record| .{ .record = .{
            .fields = try copyCheckedRecordFields(allocator, module, names, roots, payloads, active, record.fields),
            .ext = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, record.ext),
        } },
        .tuple => |tuple| .{
            .tuple = try copyCheckedTypeRange(allocator, module, names, roots, payloads, active, module.typeStoreConst().sliceVars(tuple.elems)),
        },
        .nominal_type => |nominal| .{ .nominal = .{
            .name = try names.internTypeIdent(module.identStoreConst(), nominal.ident.ident_idx),
            .origin_module = try names.internModuleIdent(module.identStoreConst(), nominal.origin_module),
            .builtin = classifyBuiltinNominal(module, nominal),
            .is_opaque = nominal.is_opaque,
            .backing = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, module.typeStoreConst().getNominalBackingVar(nominal)),
            .args = try copyCheckedTypeRange(allocator, module, names, roots, payloads, active, module.typeStoreConst().sliceNominalArgs(nominal)),
        } },
        .fn_pure => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, roots, payloads, active, .pure, func) },
        .fn_effectful => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, roots, payloads, active, .effectful, func) },
        .fn_unbound => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, roots, payloads, active, .pure, func) },
        .tag_union => |tag_union| .{ .tag_union = .{
            .tags = try copyCheckedTags(allocator, module, names, roots, payloads, active, tag_union.tags),
            .ext = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, tag_union.ext),
        } },
    };
}

fn copyCheckedFunctionType(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    kind: CheckedFunctionKind,
    func: types.Func,
) Allocator.Error!CheckedFunctionType {
    return .{
        .kind = finalizedFunctionKind(kind),
        .args = try copyCheckedTypeRange(allocator, module, names, roots, payloads, active, module.typeStoreConst().sliceVars(func.args)),
        .ret = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, func.ret),
        .needs_instantiation = func.needs_instantiation,
    };
}

fn copyCheckedTypeRange(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    vars: []const Var,
) Allocator.Error![]const CheckedTypeId {
    if (vars.len == 0) return &.{};
    const out = try allocator.alloc(CheckedTypeId, vars.len);
    errdefer allocator.free(out);
    for (vars, 0..) |var_, i| {
        out[i] = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, var_);
    }
    return out;
}

fn copyCheckedRecordFields(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    range: types.RecordField.SafeMultiList.Range,
) Allocator.Error![]const CheckedRecordField {
    const fields = module.typeStoreConst().getRecordFieldsSlice(range);
    const field_names = fields.items(.name);
    const field_vars = fields.items(.var_);
    if (field_names.len == 0) return &.{};

    const out = try allocator.alloc(CheckedRecordField, field_names.len);
    errdefer allocator.free(out);
    for (field_names, field_vars, 0..) |field_name, field_var, i| {
        out[i] = .{
            .name = try names.internRecordFieldIdent(module.identStoreConst(), field_name),
            .ty = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, field_var),
        };
    }
    return out;
}

fn copyCheckedTags(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    range: types.Tag.SafeMultiList.Range,
) Allocator.Error![]const CheckedTag {
    const tags = module.typeStoreConst().getTagsSlice(range);
    const tag_names = tags.items(.name);
    const tag_args = tags.items(.args);
    if (tag_names.len == 0) return &.{};

    const out = try allocator.alloc(CheckedTag, tag_names.len);
    for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
    errdefer {
        for (out[0..tag_names.len]) |tag| allocator.free(tag.args);
        allocator.free(out);
    }
    for (tag_names, tag_args, 0..) |tag_name, arg_range, i| {
        out[i] = .{
            .name = try names.internTagIdent(module.identStoreConst(), tag_name),
            .args = try copyCheckedTypeRange(allocator, module, names, roots, payloads, active, module.typeStoreConst().sliceVars(arg_range)),
        };
    }
    return out;
}

fn copyCheckedStaticDispatchConstraints(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    range: types.StaticDispatchConstraint.SafeList.Range,
) Allocator.Error![]const CheckedStaticDispatchConstraint {
    const constraints = module.typeStoreConst().sliceStaticDispatchConstraints(range);
    if (constraints.len == 0) return &.{};

    const out = try allocator.alloc(CheckedStaticDispatchConstraint, constraints.len);
    errdefer allocator.free(out);
    for (constraints, 0..) |constraint, i| {
        out[i] = .{
            .fn_name = try names.internMethodIdent(module.identStoreConst(), constraint.fn_name),
            .fn_ty = try appendCheckedTypeRoot(allocator, module, names, roots, payloads, active, constraint.fn_var),
            .origin = constraint.origin,
            .binop_negated = constraint.binop_negated,
            .num_literal = constraint.num_literal,
        };
    }
    return out;
}

fn classifyBuiltinNominal(module: TypedCIR.Module, nominal: types.NominalType) ?CheckedBuiltinNominal {
    const common = module.moduleEnvConst().idents;
    const is_builtin_origin = nominal.origin_module.eql(common.builtin_module) or
        std.mem.eql(u8, module.getIdent(nominal.origin_module), module.getIdent(common.builtin_module));
    if (!is_builtin_origin) return null;

    const ident = nominal.ident.ident_idx;
    if (ident.eql(common.bool) or ident.eql(common.bool_type)) return .bool;
    if (ident.eql(common.str) or ident.eql(common.builtin_str)) return .str;
    if (ident.eql(common.u8) or ident.eql(common.u8_type)) return .u8;
    if (ident.eql(common.i8) or ident.eql(common.i8_type)) return .i8;
    if (ident.eql(common.u16) or ident.eql(common.u16_type)) return .u16;
    if (ident.eql(common.i16) or ident.eql(common.i16_type)) return .i16;
    if (ident.eql(common.u32) or ident.eql(common.u32_type)) return .u32;
    if (ident.eql(common.i32) or ident.eql(common.i32_type)) return .i32;
    if (ident.eql(common.u64) or ident.eql(common.u64_type)) return .u64;
    if (ident.eql(common.i64) or ident.eql(common.i64_type)) return .i64;
    if (ident.eql(common.u128) or ident.eql(common.u128_type)) return .u128;
    if (ident.eql(common.i128) or ident.eql(common.i128_type)) return .i128;
    if (ident.eql(common.f32) or ident.eql(common.f32_type)) return .f32;
    if (ident.eql(common.f64) or ident.eql(common.f64_type)) return .f64;
    if (ident.eql(common.dec) or ident.eql(common.dec_type)) return .dec;
    if (ident.eql(common.list)) return .list;
    if (ident.eql(common.box)) return .box;
    return null;
}

fn copyOptionalIdentText(
    allocator: Allocator,
    module: TypedCIR.Module,
    ident: ?Ident.Idx,
) Allocator.Error!?[]const u8 {
    const idx = ident orelse return null;
    return try copyIdentText(allocator, module, idx);
}

fn copyIdentText(
    allocator: Allocator,
    module: TypedCIR.Module,
    idx: Ident.Idx,
) Allocator.Error![]const u8 {
    return try allocator.dupe(u8, module.getIdent(idx));
}

fn deinitCheckedTypePayload(allocator: Allocator, payload: *CheckedTypePayload) void {
    switch (payload.*) {
        .pending,
        .empty_record,
        .empty_tag_union,
        => {},
        .flex => |flex| {
            if (flex.name) |name| allocator.free(name);
            allocator.free(flex.constraints);
        },
        .rigid => |rigid| {
            if (rigid.name) |name| allocator.free(name);
            allocator.free(rigid.constraints);
        },
        .alias => |alias| allocator.free(alias.args),
        .record => |record| allocator.free(record.fields),
        .record_unbound => |fields| allocator.free(fields),
        .tuple => |elems| allocator.free(elems),
        .nominal => |nominal| allocator.free(nominal.args),
        .function => |function| allocator.free(function.args),
        .tag_union => |tag_union| {
            for (tag_union.tags) |tag| allocator.free(tag.args);
            allocator.free(tag_union.tags);
        },
    }
    payload.* = .pending;
}

fn findCheckedTypeRoot(roots: []const CheckedTypeRoot, key: canonical.CanonicalTypeKey) ?CheckedTypeId {
    for (roots) |root| {
        if (std.mem.eql(u8, root.key.bytes[0..], key.bytes[0..])) return root.id;
    }
    return null;
}

fn findCheckedTypeScheme(schemes: []const CheckedTypeScheme, key: canonical.CanonicalTypeSchemeKey) ?CheckedTypeSchemeId {
    for (schemes) |scheme| {
        if (std.mem.eql(u8, scheme.key.bytes[0..], key.bytes[0..])) return scheme.id;
    }
    return null;
}

/// Public `CheckedBody` declaration.
pub const CheckedBody = struct {
    id: CheckedBodyId,
    root_expr: CheckedExprId,
    owner_template: canonical.ProcedureTemplateRef,
};

/// Public `CheckedPatternBinder` declaration.
pub const CheckedPatternBinder = struct {
    id: PatternBinderId,
    pattern: CheckedPatternId,
};

/// Public `CheckedStringLiteralId` declaration.
pub const CheckedStringLiteralId = enum(u32) { _ };

/// Public `CheckedRecordExprField` declaration.
pub const CheckedRecordExprField = struct {
    label: canonical.RecordFieldLabelId,
    value: CheckedExprId,
};

/// Public `CheckedIfBranch` declaration.
pub const CheckedIfBranch = struct {
    cond: CheckedExprId,
    body: CheckedExprId,
};

/// Public `CheckedMatchBranchPattern` declaration.
pub const CheckedMatchBranchPattern = struct {
    pattern: CheckedPatternId,
    degenerate: bool,
    binder_remaps: []const CheckedAlternativeBinderRemap,
};

/// Public `CheckedAlternativeBinderRemap` declaration.
pub const CheckedAlternativeBinderRemap = struct {
    candidate_binder: PatternBinderId,
    representative_binder: PatternBinderId,
};

/// Public `CheckedMatchBranch` declaration.
pub const CheckedMatchBranch = struct {
    patterns: []const CheckedMatchBranchPattern,
    value: CheckedExprId,
    guard: ?CheckedExprId,
};

/// Public `CheckedCapture` declaration.
pub const CheckedCapture = struct {
    pattern: CheckedPatternId,
    scope_depth: u32,
};

/// Public `CheckedRecordDestructKind` declaration.
pub const CheckedRecordDestructKind = union(enum) {
    required: CheckedPatternId,
    sub_pattern: CheckedPatternId,
    rest: CheckedPatternId,
};

/// Public `CheckedRecordDestruct` declaration.
pub const CheckedRecordDestruct = struct {
    label: canonical.RecordFieldLabelId,
    kind: CheckedRecordDestructKind,
};

/// Public `CheckedListRestPattern` declaration.
pub const CheckedListRestPattern = struct {
    index: u32,
    pattern: ?CheckedPatternId,
};

/// Public `CheckedStatementData` declaration.
pub const CheckedStatementData = union(enum) {
    pending,
    decl: struct { pattern: CheckedPatternId, expr: CheckedExprId },
    var_: struct { pattern: CheckedPatternId, expr: CheckedExprId },
    reassign: struct { pattern: CheckedPatternId, expr: CheckedExprId },
    crash: CheckedStringLiteralId,
    dbg: CheckedExprId,
    expr: CheckedExprId,
    expect: CheckedExprId,
    for_: struct { pattern: CheckedPatternId, expr: CheckedExprId, body: CheckedExprId },
    while_: struct { cond: CheckedExprId, body: CheckedExprId },
    break_,
    return_: struct { expr: CheckedExprId, lambda: CheckedExprId },
    import_,
    alias_decl,
    nominal_decl,
    type_anno,
    type_var_alias,
    runtime_error,
};

/// Public `CheckedPatternData` declaration.
pub const CheckedPatternData = union(enum) {
    pending,
    assign: PatternBinderId,
    as: struct {
        pattern: CheckedPatternId,
        binder: PatternBinderId,
    },
    applied_tag: struct {
        name: canonical.TagLabelId,
        args: []const CheckedPatternId,
    },
    nominal: struct {
        backing_pattern: CheckedPatternId,
        backing_type: CIR.Expr.NominalBackingType,
    },
    record_destructure: []const CheckedRecordDestruct,
    list: struct {
        patterns: []const CheckedPatternId,
        rest: ?CheckedListRestPattern,
    },
    tuple: []const CheckedPatternId,
    num_literal: struct {
        value: CIR.IntValue,
        kind: CIR.NumKind,
    },
    small_dec_literal: struct {
        value: CIR.SmallDecValue,
        has_suffix: bool,
    },
    dec_literal: struct {
        value: builtins.dec.RocDec,
        has_suffix: bool,
    },
    frac_f32_literal: f32,
    frac_f64_literal: f64,
    str_literal: CheckedStringLiteralId,
    underscore,
    runtime_error,
};

/// Public `CheckedExprData` declaration.
pub const CheckedExprData = union(enum) {
    pending,
    num: struct {
        value: CIR.IntValue,
        kind: CIR.NumKind,
    },
    frac_f32: struct {
        value: f32,
        has_suffix: bool,
    },
    frac_f64: struct {
        value: f64,
        has_suffix: bool,
    },
    dec: struct {
        value: builtins.dec.RocDec,
        has_suffix: bool,
    },
    dec_small: struct {
        value: CIR.SmallDecValue,
        has_suffix: bool,
    },
    typed_int: struct {
        value: CIR.IntValue,
        type_name: canonical.TypeNameId,
    },
    typed_frac: struct {
        value: CIR.IntValue,
        type_name: canonical.TypeNameId,
    },
    str_segment: CheckedStringLiteralId,
    str: []const CheckedExprId,
    bytes_literal: CheckedStringLiteralId,
    lookup_local: struct {
        pattern: CheckedPatternId,
        resolved: ?ResolvedValueRefId,
    },
    lookup_external: ?ResolvedValueRefId,
    lookup_required: ?ResolvedValueRefId,
    list: []const CheckedExprId,
    empty_list,
    tuple: []const CheckedExprId,
    match_: struct {
        cond: CheckedExprId,
        branches: []const CheckedMatchBranch,
        is_try_suffix: bool,
    },
    if_: struct {
        branches: []const CheckedIfBranch,
        final_else: CheckedExprId,
    },
    call: struct {
        func: CheckedExprId,
        args: []const CheckedExprId,
        called_via: base.CalledVia,
        source_fn_ty_payload: CheckedTypeId,
    },
    record: struct {
        fields: []const CheckedRecordExprField,
        ext: ?CheckedExprId,
    },
    empty_record,
    block: struct {
        statements: []const CheckedStatementId,
        final_expr: CheckedExprId,
    },
    tag: struct {
        name: canonical.TagLabelId,
        args: []const CheckedExprId,
    },
    nominal: struct {
        backing_expr: CheckedExprId,
        backing_type: CIR.Expr.NominalBackingType,
    },
    zero_argument_tag: struct {
        closure_name: canonical.TagLabelId,
        name: canonical.TagLabelId,
    },
    closure: struct {
        lambda: CheckedExprId,
        captures: []const CheckedCapture,
        tag_name: canonical.TagLabelId,
    },
    lambda: struct {
        args: []const CheckedPatternId,
        body: CheckedExprId,
    },
    binop: struct {
        op: CIR.Expr.Binop.Op,
        lhs: CheckedExprId,
        rhs: CheckedExprId,
    },
    unary_minus: CheckedExprId,
    unary_not: CheckedExprId,
    field_access: struct {
        receiver: CheckedExprId,
        field_name: canonical.RecordFieldLabelId,
    },
    dispatch_call: ?StaticDispatchPlanId,
    structural_eq: struct {
        lhs: CheckedExprId,
        rhs: CheckedExprId,
        negated: bool,
    },
    method_eq: ?StaticDispatchPlanId,
    type_dispatch_call: ?StaticDispatchPlanId,
    tuple_access: struct {
        tuple: CheckedExprId,
        elem_index: u32,
    },
    runtime_error,
    crash: CheckedStringLiteralId,
    dbg: CheckedExprId,
    expect: CheckedExprId,
    ellipsis,
    anno_only,
    return_: struct {
        expr: CheckedExprId,
        lambda: CheckedExprId,
        context: CheckedReturnContext,
    },
    for_: struct {
        pattern: CheckedPatternId,
        expr: CheckedExprId,
        body: CheckedExprId,
    },
    hosted_lambda: struct {
        symbol_name: canonical.ExternalSymbolNameId,
        args: []const CheckedPatternId,
    },
    run_low_level: struct {
        op: CIR.Expr.LowLevel,
        args: []const CheckedExprId,
    },
};

/// Public `CheckedReturnContext` declaration.
pub const CheckedReturnContext = enum {
    return_expr,
    try_suffix,
};

/// Public `CheckedExpr` declaration.
pub const CheckedExpr = struct {
    id: CheckedExprId,
    ty: CheckedTypeId,
    source_region: base.Region,
    data: CheckedExprData,
};

/// Public `CheckedPattern` declaration.
pub const CheckedPattern = struct {
    id: CheckedPatternId,
    ty: CheckedTypeId,
    source_region: base.Region,
    data: CheckedPatternData,
};

/// Public `CheckedStatement` declaration.
pub const CheckedStatement = struct {
    id: CheckedStatementId,
    source_region: base.Region,
    data: CheckedStatementData,
};

/// Public `CheckedBodyStoreView` declaration.
pub const CheckedBodyStoreView = struct {
    bodies: []const CheckedBody = &.{},
    exprs: []const CheckedExpr = &.{},
    patterns: []const CheckedPattern = &.{},
    statements: []const CheckedStatement = &.{},
    string_literals: []const []const u8 = &.{},
    pattern_binders: []const CheckedPatternBinder = &.{},
    pattern_binder_by_pattern: []const ?PatternBinderId = &.{},
};

/// Public `CheckedBodyStore` declaration.
pub const CheckedBodyStore = struct {
    bodies: []CheckedBody = &.{},
    exprs: []CheckedExpr = &.{},
    patterns: []CheckedPattern = &.{},
    statements: []CheckedStatement = &.{},
    string_literals: []const []const u8 = &.{},
    pattern_binders: []CheckedPatternBinder = &.{},
    pattern_binder_by_pattern: []?PatternBinderId = &.{},
    expr_by_node: []?CheckedExprId = &.{},
    pattern_by_node: []?CheckedPatternId = &.{},
    statement_by_node: []?CheckedStatementId = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        checked_types: *const CheckedTypePublication,
    ) Allocator.Error!CheckedBodyStore {
        var exprs = std.ArrayList(CheckedExpr).empty;
        errdefer exprs.deinit(allocator);
        errdefer deinitCheckedExprList(allocator, exprs.items);
        var patterns = std.ArrayList(CheckedPattern).empty;
        errdefer patterns.deinit(allocator);
        errdefer deinitCheckedPatternList(allocator, patterns.items);
        var pattern_binders = std.ArrayList(CheckedPatternBinder).empty;
        errdefer pattern_binders.deinit(allocator);
        var statements = std.ArrayList(CheckedStatement).empty;
        errdefer statements.deinit(allocator);
        errdefer deinitCheckedStatementList(allocator, statements.items);
        var bodies = std.ArrayList(CheckedBody).empty;
        errdefer bodies.deinit(allocator);
        var string_builder = CheckedStringLiteralBuilder.init(allocator, module);
        errdefer string_builder.deinitAll();
        defer string_builder.deinitScratch();
        const expr_by_node = try allocator.alloc(?CheckedExprId, module.nodeCount());
        errdefer allocator.free(expr_by_node);
        const pattern_by_node = try allocator.alloc(?CheckedPatternId, module.nodeCount());
        errdefer allocator.free(pattern_by_node);
        const statement_by_node = try allocator.alloc(?CheckedStatementId, module.nodeCount());
        errdefer allocator.free(statement_by_node);
        @memset(expr_by_node, null);
        @memset(pattern_by_node, null);
        @memset(statement_by_node, null);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            const tag = module.nodeTag(node);
            if (isExprNodeTag(tag)) {
                const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
                const ty = checked_types.rootForSourceVar(module, module.exprType(expr_idx)) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("checked artifact invariant violated: checked expr type root was not published", .{});
                    }
                    unreachable;
                };
                const id: CheckedExprId = @enumFromInt(@as(u32, @intCast(exprs.items.len)));
                try exprs.append(allocator, .{
                    .id = id,
                    .ty = ty,
                    .source_region = module.regionAt(node),
                    .data = .pending,
                });
                expr_by_node[node_idx] = id;
            } else if (isPatternNodeTag(tag)) {
                const pattern_idx: CIR.Pattern.Idx = @enumFromInt(node_idx);
                const ty = checked_types.rootForSourceVar(module, checkedPatternSourceTypeVar(module, pattern_idx)) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("checked artifact invariant violated: checked pattern type root was not published", .{});
                    }
                    unreachable;
                };
                const id: CheckedPatternId = @enumFromInt(@as(u32, @intCast(patterns.items.len)));
                try patterns.append(allocator, .{
                    .id = id,
                    .ty = ty,
                    .source_region = module.regionAt(node),
                    .data = .pending,
                });
                pattern_by_node[node_idx] = id;
            } else if (isStatementNodeTag(tag)) {
                const id: CheckedStatementId = @enumFromInt(@as(u32, @intCast(statements.items.len)));
                try statements.append(allocator, .{
                    .id = id,
                    .source_region = module.regionAt(node),
                    .data = .pending,
                });
                statement_by_node[node_idx] = id;
            }
        }

        const pattern_binder_by_pattern = try allocator.alloc(?PatternBinderId, patterns.items.len);
        errdefer allocator.free(pattern_binder_by_pattern);
        @memset(pattern_binder_by_pattern, null);

        var copier = CheckedBodyPayloadCopier{
            .allocator = allocator,
            .module = module,
            .names = names,
            .expr_by_node = expr_by_node,
            .pattern_by_node = pattern_by_node,
            .statement_by_node = statement_by_node,
            .string_builder = &string_builder,
            .pattern_binders = &pattern_binders,
            .pattern_binder_by_pattern = pattern_binder_by_pattern,
            .checked_types = checked_types,
        };

        node_idx = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            const tag = module.nodeTag(node);
            if (isExprNodeTag(tag)) {
                const id = expr_by_node[node_idx] orelse unreachable;
                exprs.items[@intFromEnum(id)].data = try copier.copyExprData(@enumFromInt(node_idx));
            } else if (isPatternNodeTag(tag)) {
                const id = pattern_by_node[node_idx] orelse unreachable;
                patterns.items[@intFromEnum(id)].data = try copier.copyPatternData(@enumFromInt(node_idx));
            } else if (isStatementNodeTag(tag)) {
                const id = statement_by_node[node_idx] orelse unreachable;
                statements.items[@intFromEnum(id)].data = try copier.copyStatementData(@enumFromInt(node_idx));
            }
        }

        return .{
            .bodies = try bodies.toOwnedSlice(allocator),
            .exprs = try exprs.toOwnedSlice(allocator),
            .patterns = try patterns.toOwnedSlice(allocator),
            .statements = try statements.toOwnedSlice(allocator),
            .string_literals = try string_builder.toOwnedSlice(),
            .pattern_binders = try pattern_binders.toOwnedSlice(allocator),
            .pattern_binder_by_pattern = pattern_binder_by_pattern,
            .expr_by_node = expr_by_node,
            .pattern_by_node = pattern_by_node,
            .statement_by_node = statement_by_node,
        };
    }

    pub fn view(self: *const CheckedBodyStore) CheckedBodyStoreView {
        return .{
            .bodies = self.bodies,
            .exprs = self.exprs,
            .patterns = self.patterns,
            .statements = self.statements,
            .string_literals = self.string_literals,
            .pattern_binders = self.pattern_binders,
            .pattern_binder_by_pattern = self.pattern_binder_by_pattern,
        };
    }

    pub fn body(self: *const CheckedBodyStore, id: CheckedBodyId) CheckedBody {
        return self.bodies[@intFromEnum(id)];
    }

    pub fn expr(self: *const CheckedBodyStore, id: CheckedExprId) CheckedExpr {
        return self.exprs[@intFromEnum(id)];
    }

    pub fn exprIdForSource(self: *const CheckedBodyStore, source_expr: CIR.Expr.Idx) ?CheckedExprId {
        const raw = @intFromEnum(source_expr);
        if (raw >= self.expr_by_node.len) return null;
        return self.expr_by_node[raw];
    }

    pub fn patternIdForSource(self: *const CheckedBodyStore, pattern: CIR.Pattern.Idx) ?CheckedPatternId {
        const raw = @intFromEnum(pattern);
        if (raw >= self.pattern_by_node.len) return null;
        return self.pattern_by_node[raw];
    }

    pub fn patternBinderForCheckedPattern(self: *const CheckedBodyStore, pattern: CheckedPatternId) ?PatternBinderId {
        const raw = @intFromEnum(pattern);
        if (raw >= self.pattern_binder_by_pattern.len) return null;
        return self.pattern_binder_by_pattern[raw];
    }

    pub fn patternBinderForSource(self: *const CheckedBodyStore, pattern: CIR.Pattern.Idx) ?PatternBinderId {
        const checked_pattern = self.patternIdForSource(pattern) orelse return null;
        return self.patternBinderForCheckedPattern(checked_pattern);
    }

    pub fn attachStaticDispatchPlans(
        self: *CheckedBodyStore,
        plans: *const static_dispatch.StaticDispatchPlanTable,
    ) void {
        var iter = plans.by_expr.iterator();
        while (iter.next()) |entry| {
            const checked_expr = self.exprIdForSource(entry.key_ptr.*) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: static dispatch expression {d} has no checked expression id",
                        .{@intFromEnum(entry.key_ptr.*)},
                    );
                }
                unreachable;
            };
            const data = &self.exprs[@intFromEnum(checked_expr)].data;
            switch (data.*) {
                .dispatch_call => data.* = .{ .dispatch_call = entry.value_ptr.* },
                .method_eq => data.* = .{ .method_eq = entry.value_ptr.* },
                .type_dispatch_call => data.* = .{ .type_dispatch_call = entry.value_ptr.* },
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "checked artifact invariant violated: static dispatch plan {d} points at non-dispatch checked expression {d}",
                            .{ @intFromEnum(entry.value_ptr.*), @intFromEnum(checked_expr) },
                        );
                    }
                    unreachable;
                },
            }
        }
    }

    pub fn attachResolvedValueRefs(
        self: *CheckedBodyStore,
        refs: *const ResolvedValueRefTable,
    ) void {
        for (refs.records, 0..) |record, i| {
            const ref_id: ResolvedValueRefId = @enumFromInt(@as(u32, @intCast(i)));
            const indexed = refs.lookupIdByCheckedExpr(record.expr) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: resolved value ref {d} is missing from checked expression index",
                        .{i},
                    );
                }
                unreachable;
            };
            std.debug.assert(ref_id == indexed);
            const data = &self.exprs[@intFromEnum(record.expr)].data;
            switch (data.*) {
                .lookup_local => |lookup| data.* = .{ .lookup_local = .{
                    .pattern = lookup.pattern,
                    .resolved = ref_id,
                } },
                .lookup_external => data.* = .{ .lookup_external = ref_id },
                .lookup_required => data.* = .{ .lookup_required = ref_id },
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "checked artifact invariant violated: resolved value ref {d} points at non-lookup checked expression {d}",
                            .{ i, @intFromEnum(record.expr) },
                        );
                    }
                    unreachable;
                },
            }
        }
    }

    pub fn appendBody(
        self: *CheckedBodyStore,
        allocator: Allocator,
        root_expr: CheckedExprId,
        owner_template: canonical.ProcedureTemplateRef,
    ) Allocator.Error!CheckedBodyId {
        const id: CheckedBodyId = @enumFromInt(@as(u32, @intCast(self.bodies.len)));
        const next = try allocator.alloc(CheckedBody, self.bodies.len + 1);
        @memcpy(next[0..self.bodies.len], self.bodies);
        next[self.bodies.len] = .{
            .id = id,
            .root_expr = root_expr,
            .owner_template = owner_template,
        };
        allocator.free(self.bodies);
        self.bodies = next;
        return id;
    }

    pub fn deinit(self: *CheckedBodyStore, allocator: Allocator) void {
        allocator.free(self.statement_by_node);
        allocator.free(self.pattern_by_node);
        allocator.free(self.expr_by_node);
        allocator.free(self.pattern_binder_by_pattern);
        allocator.free(self.pattern_binders);
        for (self.string_literals) |literal| allocator.free(literal);
        allocator.free(self.string_literals);
        deinitCheckedStatementList(allocator, self.statements);
        deinitCheckedPatternList(allocator, self.patterns);
        deinitCheckedExprList(allocator, self.exprs);
        allocator.free(self.statements);
        allocator.free(self.patterns);
        allocator.free(self.exprs);
        allocator.free(self.bodies);
        self.* = .{};
    }
};

const CheckedStringLiteralBuilder = struct {
    allocator: Allocator,
    module: TypedCIR.Module,
    strings: std.ArrayList([]const u8),
    by_literal: std.AutoHashMapUnmanaged(StringLiteral.Idx, CheckedStringLiteralId) = .{},

    fn init(allocator: Allocator, module: TypedCIR.Module) CheckedStringLiteralBuilder {
        return .{
            .allocator = allocator,
            .module = module,
            .strings = .empty,
        };
    }

    fn intern(self: *CheckedStringLiteralBuilder, literal: StringLiteral.Idx) Allocator.Error!CheckedStringLiteralId {
        if (self.by_literal.get(literal)) |existing| return existing;

        const id: CheckedStringLiteralId = @enumFromInt(@as(u32, @intCast(self.strings.items.len)));
        const owned = try self.allocator.dupe(u8, self.module.getString(literal));
        errdefer self.allocator.free(owned);
        try self.strings.append(self.allocator, owned);
        try self.by_literal.put(self.allocator, literal, id);
        return id;
    }

    fn toOwnedSlice(self: *CheckedStringLiteralBuilder) Allocator.Error![]const []const u8 {
        return try self.strings.toOwnedSlice(self.allocator);
    }

    fn deinitScratch(self: *CheckedStringLiteralBuilder) void {
        self.by_literal.deinit(self.allocator);
    }

    fn deinitAll(self: *CheckedStringLiteralBuilder) void {
        for (self.strings.items) |literal| self.allocator.free(literal);
        self.strings.deinit(self.allocator);
        self.by_literal.deinit(self.allocator);
        self.* = CheckedStringLiteralBuilder.init(self.allocator, self.module);
    }
};

const CheckedBodyPayloadCopier = struct {
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    expr_by_node: []const ?CheckedExprId,
    pattern_by_node: []const ?CheckedPatternId,
    statement_by_node: []const ?CheckedStatementId,
    string_builder: *CheckedStringLiteralBuilder,
    pattern_binders: *std.ArrayList(CheckedPatternBinder),
    pattern_binder_by_pattern: []?PatternBinderId,
    checked_types: *const CheckedTypePublication,

    fn copyExprData(self: *@This(), expr_idx: CIR.Expr.Idx) Allocator.Error!CheckedExprData {
        const expr = self.module.expr(expr_idx).data;
        return switch (expr) {
            .e_num => |num| .{ .num = .{ .value = num.value, .kind = num.kind } },
            .e_frac_f32 => |frac| .{ .frac_f32 = .{ .value = frac.value, .has_suffix = frac.has_suffix } },
            .e_frac_f64 => |frac| .{ .frac_f64 = .{ .value = frac.value, .has_suffix = frac.has_suffix } },
            .e_dec => |dec| .{ .dec = .{ .value = dec.value, .has_suffix = dec.has_suffix } },
            .e_dec_small => |dec| .{ .dec_small = .{ .value = dec.value, .has_suffix = dec.has_suffix } },
            .e_typed_int => |typed| .{ .typed_int = .{
                .value = typed.value,
                .type_name = try self.names.internTypeIdent(self.module.identStoreConst(), typed.type_name),
            } },
            .e_typed_frac => |typed| .{ .typed_frac = .{
                .value = typed.value,
                .type_name = try self.names.internTypeIdent(self.module.identStoreConst(), typed.type_name),
            } },
            .e_str_segment => |str| .{ .str_segment = try self.string_builder.intern(str.literal) },
            .e_str => |str| .{ .str = try self.copyExprSpan(str.span) },
            .e_bytes_literal => |bytes| .{ .bytes_literal = try self.string_builder.intern(bytes.literal) },
            .e_lookup_local => |lookup| .{ .lookup_local = .{
                .pattern = self.checkedPattern(lookup.pattern_idx),
                .resolved = null,
            } },
            .e_lookup_external => .{ .lookup_external = null },
            .e_lookup_required => .{ .lookup_required = null },
            .e_list => |list| .{ .list = try self.copyExprSpan(list.elems) },
            .e_empty_list => .empty_list,
            .e_tuple => |tuple| .{ .tuple = try self.copyExprSpan(tuple.elems) },
            .e_match => |match| .{ .match_ = .{
                .cond = self.checkedExpr(match.cond),
                .branches = try self.copyMatchBranches(match.branches),
                .is_try_suffix = match.is_try_suffix,
            } },
            .e_if => |if_| .{ .if_ = .{
                .branches = try self.copyIfBranches(if_.branches),
                .final_else = self.checkedExpr(if_.final_else),
            } },
            .e_call => |call| .{ .call = .{
                .func = self.checkedExpr(call.func),
                .args = try self.copyExprSpan(call.args),
                .called_via = call.called_via,
                .source_fn_ty_payload = try self.checkedTypeForRequiredVar(
                    call.constraint_fn_var orelse checkedArtifactInvariant("checked call expression had no published function constraint type", .{}),
                    "checked call function constraint type root was not published",
                ),
            } },
            .e_record => |record| .{ .record = .{
                .fields = try self.copyRecordFields(record.fields),
                .ext = if (record.ext) |ext| self.checkedExpr(ext) else null,
            } },
            .e_empty_record => .empty_record,
            .e_block => |block| .{ .block = .{
                .statements = try self.copyStatementSpan(block.stmts),
                .final_expr = self.checkedExpr(block.final_expr),
            } },
            .e_tag => |tag| .{ .tag = .{
                .name = try self.names.internTagIdent(self.module.identStoreConst(), tag.name),
                .args = try self.copyExprSpan(tag.args),
            } },
            .e_nominal => |nominal| .{ .nominal = .{
                .backing_expr = self.checkedExpr(nominal.backing_expr),
                .backing_type = nominal.backing_type,
            } },
            .e_nominal_external => |nominal| .{ .nominal = .{
                .backing_expr = self.checkedExpr(nominal.backing_expr),
                .backing_type = nominal.backing_type,
            } },
            .e_zero_argument_tag => |tag| .{ .zero_argument_tag = .{
                .closure_name = try self.names.internTagIdent(self.module.identStoreConst(), tag.closure_name),
                .name = try self.names.internTagIdent(self.module.identStoreConst(), tag.name),
            } },
            .e_closure => |closure| .{ .closure = .{
                .lambda = self.checkedExpr(closure.lambda_idx),
                .captures = try self.copyCaptures(closure.captures),
                .tag_name = try self.names.internTagIdent(self.module.identStoreConst(), closure.tag_name),
            } },
            .e_lambda => |lambda| .{ .lambda = .{
                .args = try self.copyPatternSpan(lambda.args),
                .body = self.checkedExpr(lambda.body),
            } },
            .e_binop => |binop| .{ .binop = .{
                .op = binop.op,
                .lhs = self.checkedExpr(binop.lhs),
                .rhs = self.checkedExpr(binop.rhs),
            } },
            .e_unary_minus => |unary| .{ .unary_minus = self.checkedExpr(unary.expr) },
            .e_unary_not => |unary| .{ .unary_not = self.checkedExpr(unary.expr) },
            .e_field_access => |field| .{ .field_access = .{
                .receiver = self.checkedExpr(field.receiver),
                .field_name = try self.names.internRecordFieldIdent(self.module.identStoreConst(), field.field_name),
            } },
            .e_method_call => checkedArtifactInvariant(
                "ordinary method call reached artifact publication after checking; expected explicit static-dispatch plan",
                .{},
            ),
            .e_dispatch_call => .{ .dispatch_call = null },
            .e_structural_eq => |eq| .{ .structural_eq = .{
                .lhs = self.checkedExpr(eq.lhs),
                .rhs = self.checkedExpr(eq.rhs),
                .negated = eq.negated,
            } },
            .e_method_eq => .{ .method_eq = null },
            .e_type_method_call => checkedArtifactInvariant(
                "type method call reached artifact publication after checking; expected explicit static-dispatch plan",
                .{},
            ),
            .e_type_dispatch_call => .{ .type_dispatch_call = null },
            .e_tuple_access => |access| .{ .tuple_access = .{
                .tuple = self.checkedExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .e_runtime_error => .runtime_error,
            .e_crash => |crash| .{ .crash = try self.string_builder.intern(crash.msg) },
            .e_dbg => |dbg| .{ .dbg = self.checkedExpr(dbg.expr) },
            .e_expect => |expect| .{ .expect = self.checkedExpr(expect.body) },
            .e_ellipsis => .ellipsis,
            .e_anno_only => .anno_only,
            .e_return => |ret| .{ .return_ = .{
                .expr = self.checkedExpr(ret.expr),
                .lambda = self.checkedExpr(ret.lambda),
                .context = switch (ret.context) {
                    .return_expr => .return_expr,
                    .try_suffix => .try_suffix,
                },
            } },
            .e_for => |for_| .{ .for_ = .{
                .pattern = self.checkedPattern(for_.patt),
                .expr = self.checkedExpr(for_.expr),
                .body = self.checkedExpr(for_.body),
            } },
            .e_hosted_lambda => |hosted| .{ .hosted_lambda = .{
                .symbol_name = try self.names.internExternalSymbolIdent(self.module.identStoreConst(), hosted.symbol_name),
                .args = try self.copyPatternSpan(hosted.args),
            } },
            .e_run_low_level => |run| .{ .run_low_level = .{
                .op = run.op,
                .args = try self.copyExprSpan(run.args),
            } },
        };
    }

    fn copyPatternData(self: *@This(), pattern_idx: CIR.Pattern.Idx) Allocator.Error!CheckedPatternData {
        const pattern = self.module.pattern(pattern_idx).data;
        return switch (pattern) {
            .assign => .{ .assign = try self.patternBinder(pattern_idx) },
            .as => |as| .{ .as = .{
                .pattern = self.checkedPattern(as.pattern),
                .binder = try self.patternBinder(pattern_idx),
            } },
            .applied_tag => |tag| .{ .applied_tag = .{
                .name = try self.names.internTagIdent(self.module.identStoreConst(), tag.name),
                .args = try self.copyPatternSpan(tag.args),
            } },
            .nominal => |nominal| .{ .nominal = .{
                .backing_pattern = self.checkedPattern(nominal.backing_pattern),
                .backing_type = nominal.backing_type,
            } },
            .nominal_external => |nominal| .{ .nominal = .{
                .backing_pattern = self.checkedPattern(nominal.backing_pattern),
                .backing_type = nominal.backing_type,
            } },
            .record_destructure => |record| .{ .record_destructure = try self.copyRecordDestructs(record.destructs) },
            .list => |list| .{ .list = .{
                .patterns = try self.copyPatternSpan(list.patterns),
                .rest = if (list.rest_info) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |rest_pattern| self.checkedPattern(rest_pattern) else null,
                } else null,
            } },
            .tuple => |tuple| .{ .tuple = try self.copyPatternSpan(tuple.patterns) },
            .num_literal => |num| .{ .num_literal = .{ .value = num.value, .kind = num.kind } },
            .small_dec_literal => |dec| .{ .small_dec_literal = .{ .value = dec.value, .has_suffix = dec.has_suffix } },
            .dec_literal => |dec| .{ .dec_literal = .{ .value = dec.value, .has_suffix = dec.has_suffix } },
            .frac_f32_literal => |frac| .{ .frac_f32_literal = frac.value },
            .frac_f64_literal => |frac| .{ .frac_f64_literal = frac.value },
            .str_literal => |str| .{ .str_literal = try self.string_builder.intern(str.literal) },
            .underscore => .underscore,
            .runtime_error => .runtime_error,
        };
    }

    fn copyStatementData(self: *@This(), statement_idx: CIR.Statement.Idx) Allocator.Error!CheckedStatementData {
        const statement = self.module.getStatement(statement_idx);
        return switch (statement) {
            .s_decl => |decl| .{ .decl = .{ .pattern = self.checkedPattern(decl.pattern), .expr = self.checkedExpr(decl.expr) } },
            .s_var => |var_| .{ .var_ = .{ .pattern = self.checkedPattern(var_.pattern_idx), .expr = self.checkedExpr(var_.expr) } },
            .s_reassign => |reassign| .{ .reassign = .{ .pattern = self.checkedPattern(reassign.pattern_idx), .expr = self.checkedExpr(reassign.expr) } },
            .s_crash => |crash| .{ .crash = try self.string_builder.intern(crash.msg) },
            .s_dbg => |dbg| .{ .dbg = self.checkedExpr(dbg.expr) },
            .s_expr => |expr| .{ .expr = self.checkedExpr(expr.expr) },
            .s_expect => |expect| .{ .expect = self.checkedExpr(expect.body) },
            .s_for => |for_| .{ .for_ = .{
                .pattern = self.checkedPattern(for_.patt),
                .expr = self.checkedExpr(for_.expr),
                .body = self.checkedExpr(for_.body),
            } },
            .s_while => |while_| .{ .while_ = .{
                .cond = self.checkedExpr(while_.cond),
                .body = self.checkedExpr(while_.body),
            } },
            .s_break => .break_,
            .s_return => |ret| .{ .return_ = .{
                .expr = self.checkedExpr(ret.expr),
                .lambda = self.checkedExpr(ret.lambda),
            } },
            .s_import => .import_,
            .s_alias_decl => .alias_decl,
            .s_nominal_decl => .nominal_decl,
            .s_type_anno => .type_anno,
            .s_type_var_alias => .type_var_alias,
            .s_runtime_error => .runtime_error,
        };
    }

    fn copyExprSpan(self: *@This(), span: CIR.Expr.Span) Allocator.Error![]const CheckedExprId {
        const source = self.module.sliceExpr(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedExprId, source.len);
        for (source, 0..) |expr, i| out[i] = self.checkedExpr(expr);
        return out;
    }

    fn copyPatternSpan(self: *@This(), span: CIR.Pattern.Span) Allocator.Error![]const CheckedPatternId {
        const source = self.module.slicePatterns(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedPatternId, source.len);
        for (source, 0..) |pattern, i| out[i] = self.checkedPattern(pattern);
        return out;
    }

    fn copyStatementSpan(self: *@This(), span: CIR.Statement.Span) Allocator.Error![]const CheckedStatementId {
        const source = self.module.sliceStatements(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedStatementId, source.len);
        for (source, 0..) |statement, i| out[i] = self.checkedStatement(statement);
        return out;
    }

    fn copyRecordFields(self: *@This(), span: CIR.RecordField.Span) Allocator.Error![]const CheckedRecordExprField {
        const source = self.module.sliceRecordFields(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedRecordExprField, source.len);
        for (source, 0..) |field_idx, i| {
            const field = self.module.getRecordField(field_idx);
            out[i] = .{
                .label = try self.names.internRecordFieldIdent(self.module.identStoreConst(), field.name),
                .value = self.checkedExpr(field.value),
            };
        }
        return out;
    }

    fn copyIfBranches(self: *@This(), span: CIR.Expr.IfBranch.Span) Allocator.Error![]const CheckedIfBranch {
        const source = self.module.sliceIfBranches(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedIfBranch, source.len);
        for (source, 0..) |branch_idx, i| {
            const branch = self.module.getIfBranch(branch_idx);
            out[i] = .{
                .cond = self.checkedExpr(branch.cond),
                .body = self.checkedExpr(branch.body),
            };
        }
        return out;
    }

    fn copyMatchBranches(self: *@This(), span: CIR.Expr.Match.Branch.Span) Allocator.Error![]const CheckedMatchBranch {
        const source = self.module.matchBranchSlice(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedMatchBranch, source.len);
        var initialized: usize = 0;
        errdefer {
            for (out[0..initialized]) |branch| self.allocator.free(branch.patterns);
            self.allocator.free(out);
        }
        for (source, 0..) |branch_idx, i| {
            const branch = self.module.getMatchBranch(branch_idx);
            out[i] = .{
                .patterns = try self.copyMatchBranchPatterns(branch.patterns),
                .value = self.checkedExpr(branch.value),
                .guard = if (branch.guard) |guard| self.checkedExpr(guard) else null,
            };
            initialized += 1;
        }
        return out;
    }

    const SourcePatternBinder = struct {
        ident: Ident.Idx,
        binder: PatternBinderId,
    };

    fn copyMatchBranchPatterns(self: *@This(), span: CIR.Expr.Match.BranchPattern.Span) Allocator.Error![]const CheckedMatchBranchPattern {
        const source = self.module.sliceMatchBranchPatterns(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedMatchBranchPattern, source.len);
        var initialized: usize = 0;
        errdefer {
            for (out[0..initialized]) |pattern| self.allocator.free(pattern.binder_remaps);
            self.allocator.free(out);
        }

        var representative_binders = std.ArrayList(SourcePatternBinder).empty;
        defer representative_binders.deinit(self.allocator);
        const representative_pattern = self.module.getMatchBranchPattern(source[0]).pattern;
        try self.collectSourcePatternBinders(representative_pattern, &representative_binders);

        for (source, 0..) |branch_pattern_idx, i| {
            const branch_pattern = self.module.getMatchBranchPattern(branch_pattern_idx);
            out[i] = .{
                .pattern = self.checkedPattern(branch_pattern.pattern),
                .degenerate = branch_pattern.degenerate,
                .binder_remaps = if (branch_pattern.degenerate)
                    &.{}
                else
                    try self.copyAlternativeBinderRemaps(branch_pattern.pattern, representative_binders.items),
            };
            initialized += 1;
        }
        return out;
    }

    fn copyAlternativeBinderRemaps(
        self: *@This(),
        pattern: CIR.Pattern.Idx,
        representative_binders: []const SourcePatternBinder,
    ) Allocator.Error![]const CheckedAlternativeBinderRemap {
        if (representative_binders.len == 0) return &.{};

        var candidate_binders = std.ArrayList(SourcePatternBinder).empty;
        defer candidate_binders.deinit(self.allocator);
        try self.collectSourcePatternBinders(pattern, &candidate_binders);

        if (candidate_binders.items.len != representative_binders.len) {
            if (builtin.mode == .Debug) {
                std.debug.panic("checked artifact invariant violated: non-degenerate alternative binder count differs from representative", .{});
            }
            unreachable;
        }

        const remaps = try self.allocator.alloc(CheckedAlternativeBinderRemap, candidate_binders.items.len);
        errdefer self.allocator.free(remaps);

        for (candidate_binders.items, 0..) |candidate, i| {
            const representative = self.representativeBinderForIdent(representative_binders, candidate.ident) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: non-degenerate alternative binder has no representative binder", .{});
                }
                unreachable;
            };
            remaps[i] = .{
                .candidate_binder = candidate.binder,
                .representative_binder = representative,
            };
        }

        return remaps;
    }

    fn representativeBinderForIdent(
        self: *@This(),
        representative_binders: []const SourcePatternBinder,
        ident: Ident.Idx,
    ) ?PatternBinderId {
        _ = self;
        for (representative_binders) |representative| {
            if (representative.ident.eql(ident)) return representative.binder;
        }
        return null;
    }

    fn collectSourcePatternBinders(
        self: *@This(),
        pattern_idx: CIR.Pattern.Idx,
        out: *std.ArrayList(SourcePatternBinder),
    ) Allocator.Error!void {
        const pattern = self.module.pattern(pattern_idx).data;
        switch (pattern) {
            .assign => |assign| try out.append(self.allocator, .{
                .ident = assign.ident,
                .binder = try self.patternBinder(pattern_idx),
            }),
            .as => |as| {
                try self.collectSourcePatternBinders(as.pattern, out);
                try out.append(self.allocator, .{
                    .ident = as.ident,
                    .binder = try self.patternBinder(pattern_idx),
                });
            },
            .applied_tag => |tag| {
                for (self.module.slicePatterns(tag.args)) |child| {
                    try self.collectSourcePatternBinders(child, out);
                }
            },
            .nominal => |nominal| try self.collectSourcePatternBinders(nominal.backing_pattern, out),
            .nominal_external => |nominal| try self.collectSourcePatternBinders(nominal.backing_pattern, out),
            .record_destructure => |record| {
                for (self.module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = self.module.getRecordDestruct(destruct_idx);
                    switch (destruct.kind) {
                        .Required,
                        .SubPattern,
                        .Rest,
                        => |child| try self.collectSourcePatternBinders(child, out),
                    }
                }
            },
            .list => |list| {
                for (self.module.slicePatterns(list.patterns)) |child| {
                    try self.collectSourcePatternBinders(child, out);
                }
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern| try self.collectSourcePatternBinders(rest_pattern, out);
                }
            },
            .tuple => |tuple| {
                for (self.module.slicePatterns(tuple.patterns)) |child| {
                    try self.collectSourcePatternBinders(child, out);
                }
            },
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn copyCaptures(self: *@This(), span: CIR.Expr.Capture.Span) Allocator.Error![]const CheckedCapture {
        const source = self.module.moduleEnvConst().store.sliceCaptures(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedCapture, source.len);
        for (source, 0..) |capture_idx, i| {
            const capture = self.module.moduleEnvConst().store.getCapture(capture_idx);
            out[i] = .{
                .pattern = self.checkedPattern(capture.pattern_idx),
                .scope_depth = capture.scope_depth,
            };
        }
        return out;
    }

    fn copyRecordDestructs(self: *@This(), span: CIR.Pattern.RecordDestruct.Span) Allocator.Error![]const CheckedRecordDestruct {
        const source = self.module.sliceRecordDestructs(span);
        if (source.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedRecordDestruct, source.len);
        for (source, 0..) |destruct_idx, i| {
            const destruct = self.module.getRecordDestruct(destruct_idx);
            out[i] = .{
                .label = try self.names.internRecordFieldIdent(self.module.identStoreConst(), destruct.label),
                .kind = switch (destruct.kind) {
                    .Required => |pattern| .{ .required = self.checkedPattern(pattern) },
                    .SubPattern => |pattern| .{ .sub_pattern = self.checkedPattern(pattern) },
                    .Rest => |pattern| .{ .rest = self.checkedPattern(pattern) },
                },
            };
        }
        return out;
    }

    fn checkedExpr(self: *const @This(), expr: CIR.Expr.Idx) CheckedExprId {
        const raw = @intFromEnum(expr);
        if (raw < self.expr_by_node.len) {
            if (self.expr_by_node[raw]) |id| return id;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: expression {d} was not copied into checked body store", .{raw});
        }
        unreachable;
    }

    fn checkedTypeForRequiredVar(
        self: *@This(),
        var_: Var,
        comptime message: []const u8,
    ) Allocator.Error!CheckedTypeId {
        _ = self.allocator;
        return self.checked_types.rootForSourceVar(self.module, var_) orelse checkedArtifactInvariant(message, .{});
    }

    fn checkedPattern(self: *const @This(), pattern: CIR.Pattern.Idx) CheckedPatternId {
        const raw = @intFromEnum(pattern);
        if (raw < self.pattern_by_node.len) {
            if (self.pattern_by_node[raw]) |id| return id;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: pattern {d} was not copied into checked body store", .{raw});
        }
        unreachable;
    }

    fn patternBinder(self: *@This(), pattern: CIR.Pattern.Idx) Allocator.Error!PatternBinderId {
        const checked_pattern = self.checkedPattern(pattern);
        const raw = @intFromEnum(checked_pattern);
        if (self.pattern_binder_by_pattern[raw]) |existing| return existing;

        const id: PatternBinderId = @enumFromInt(@as(u32, @intCast(self.pattern_binders.items.len)));
        try self.pattern_binders.append(self.allocator, .{
            .id = id,
            .pattern = checked_pattern,
        });
        self.pattern_binder_by_pattern[raw] = id;
        return id;
    }

    fn checkedStatement(self: *const @This(), statement: CIR.Statement.Idx) CheckedStatementId {
        const raw = @intFromEnum(statement);
        if (raw < self.statement_by_node.len) {
            if (self.statement_by_node[raw]) |id| return id;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: statement {d} was not copied into checked body store", .{raw});
        }
        unreachable;
    }
};

fn deinitCheckedExprList(allocator: Allocator, exprs: []CheckedExpr) void {
    for (exprs) |*expr| deinitCheckedExprData(allocator, &expr.data);
}

fn deinitCheckedPatternList(allocator: Allocator, patterns: []CheckedPattern) void {
    for (patterns) |*pattern| deinitCheckedPatternData(allocator, &pattern.data);
}

fn deinitCheckedStatementList(allocator: Allocator, statements: []CheckedStatement) void {
    for (statements) |*statement| deinitCheckedStatementData(allocator, &statement.data);
}

fn deinitCheckedExprData(allocator: Allocator, data: *CheckedExprData) void {
    switch (data.*) {
        .pending,
        .num,
        .frac_f32,
        .frac_f64,
        .dec,
        .dec_small,
        .typed_int,
        .typed_frac,
        .str_segment,
        .bytes_literal,
        .lookup_local,
        .lookup_external,
        .lookup_required,
        .empty_list,
        .empty_record,
        .zero_argument_tag,
        .dispatch_call,
        .structural_eq,
        .method_eq,
        .type_dispatch_call,
        .tuple_access,
        .runtime_error,
        .crash,
        .dbg,
        .expect,
        .ellipsis,
        .anno_only,
        .return_,
        .for_,
        => {},
        .str => |items| allocator.free(items),
        .list => |items| allocator.free(items),
        .tuple => |items| allocator.free(items),
        .match_ => |match| {
            for (match.branches) |branch| {
                for (branch.patterns) |pattern| allocator.free(pattern.binder_remaps);
                allocator.free(branch.patterns);
            }
            allocator.free(match.branches);
        },
        .if_ => |if_| allocator.free(if_.branches),
        .call => |call| allocator.free(call.args),
        .record => |record| allocator.free(record.fields),
        .block => |block| allocator.free(block.statements),
        .tag => |tag| allocator.free(tag.args),
        .nominal => {},
        .closure => |closure| allocator.free(closure.captures),
        .lambda => |lambda| allocator.free(lambda.args),
        .binop => {},
        .unary_minus,
        .unary_not,
        => {},
        .field_access => {},
        .hosted_lambda => |hosted| allocator.free(hosted.args),
        .run_low_level => |run| allocator.free(run.args),
    }
    data.* = .pending;
}

fn deinitCheckedPatternData(allocator: Allocator, data: *CheckedPatternData) void {
    switch (data.*) {
        .pending,
        .assign,
        .as,
        .nominal,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => {},
        .applied_tag => |tag| allocator.free(tag.args),
        .record_destructure => |destructs| allocator.free(destructs),
        .list => |list| allocator.free(list.patterns),
        .tuple => |patterns| allocator.free(patterns),
    }
    data.* = .pending;
}

fn deinitCheckedStatementData(_: Allocator, data: *CheckedStatementData) void {
    data.* = .pending;
}

fn verifyCheckedExprDataPublished(data: CheckedExprData) void {
    switch (data) {
        .pending => std.debug.panic("checked artifact invariant violated: checked expression payload was not filled", .{}),
        .lookup_local => |lookup| std.debug.assert(lookup.resolved != null),
        .lookup_external => |ref| std.debug.assert(ref != null),
        .lookup_required => |ref| std.debug.assert(ref != null),
        .dispatch_call => |plan| std.debug.assert(plan != null),
        .method_eq => |plan| std.debug.assert(plan != null),
        .type_dispatch_call => |plan| std.debug.assert(plan != null),
        else => {},
    }
}

fn verifyCheckedPatternDataPublished(data: CheckedPatternData) void {
    switch (data) {
        .pending => std.debug.panic("checked artifact invariant violated: checked pattern payload was not filled", .{}),
        else => {},
    }
}

fn verifyCheckedStatementDataPublished(data: CheckedStatementData) void {
    switch (data) {
        .pending => std.debug.panic("checked artifact invariant violated: checked statement payload was not filled", .{}),
        else => {},
    }
}

fn isExprNodeTag(tag: CIR.Node.Tag) bool {
    return std.mem.startsWith(u8, @tagName(tag), "expr_");
}

fn isPatternNodeTag(tag: CIR.Node.Tag) bool {
    return std.mem.startsWith(u8, @tagName(tag), "pattern_");
}

fn isStatementNodeTag(tag: CIR.Node.Tag) bool {
    return std.mem.startsWith(u8, @tagName(tag), "statement_");
}

/// Public `CheckedProcedureBody` declaration.
pub const CheckedProcedureBody = union(enum) {
    checked_body: CheckedBodyId,
    promoted_callable_wrapper: canonical.PromotedCallableWrapperId,
    intrinsic_wrapper: canonical.IntrinsicWrapperId,
    entry_wrapper: canonical.EntryWrapperId,
};

/// Public `IntrinsicId` declaration.
pub const IntrinsicId = enum {
    str_inspect,
    structural_eq,
};

/// Public `IntrinsicWrapper` declaration.
pub const IntrinsicWrapper = struct {
    id: canonical.IntrinsicWrapperId,
    template: canonical.ProcedureTemplateRef,
    checked_fn_root: CheckedTypeId,
    intrinsic: IntrinsicId,
};

/// Public `IntrinsicWrapperTable` declaration.
pub const IntrinsicWrapperTable = struct {
    wrappers: []IntrinsicWrapper = &.{},

    pub fn append(
        self: *IntrinsicWrapperTable,
        allocator: Allocator,
        template: canonical.ProcedureTemplateRef,
        checked_fn_root: CheckedTypeId,
        intrinsic: IntrinsicId,
    ) Allocator.Error!canonical.IntrinsicWrapperId {
        const id: canonical.IntrinsicWrapperId = @enumFromInt(@as(u32, @intCast(self.wrappers.len)));
        const next = try allocator.alloc(IntrinsicWrapper, self.wrappers.len + 1);
        @memcpy(next[0..self.wrappers.len], self.wrappers);
        next[self.wrappers.len] = .{
            .id = id,
            .template = template,
            .checked_fn_root = checked_fn_root,
            .intrinsic = intrinsic,
        };
        allocator.free(self.wrappers);
        self.wrappers = next;
        return id;
    }

    pub fn get(self: *const IntrinsicWrapperTable, id: canonical.IntrinsicWrapperId) IntrinsicWrapper {
        return self.wrappers[@intFromEnum(id)];
    }

    pub fn deinit(self: *IntrinsicWrapperTable, allocator: Allocator) void {
        allocator.free(self.wrappers);
        self.* = .{};
    }
};

/// Public `EntryWrapper` declaration.
pub const EntryWrapper = struct {
    id: canonical.EntryWrapperId,
    root: ComptimeRootId,
    template: canonical.ProcedureTemplateRef,
    checked_fn_root: CheckedTypeId,
    body_expr: CheckedExprId,
};

/// Public `EntryWrapperTable` declaration.
pub const EntryWrapperTable = struct {
    wrappers: []EntryWrapper = &.{},

    pub fn append(
        self: *EntryWrapperTable,
        allocator: Allocator,
        root: ComptimeRootId,
        template: canonical.ProcedureTemplateRef,
        checked_fn_root: CheckedTypeId,
        body_expr: CheckedExprId,
    ) Allocator.Error!canonical.EntryWrapperId {
        const id: canonical.EntryWrapperId = @enumFromInt(@as(u32, @intCast(self.wrappers.len)));
        const old = self.wrappers;
        const next = try allocator.alloc(EntryWrapper, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = .{
            .id = id,
            .root = root,
            .template = template,
            .checked_fn_root = checked_fn_root,
            .body_expr = body_expr,
        };
        allocator.free(old);
        self.wrappers = next;
        return id;
    }

    pub fn get(self: *const EntryWrapperTable, id: canonical.EntryWrapperId) EntryWrapper {
        return self.wrappers[@intFromEnum(id)];
    }

    pub fn lookupByRoot(self: *const EntryWrapperTable, root: ComptimeRootId) ?EntryWrapper {
        for (self.wrappers) |wrapper| {
            if (wrapper.root == root) return wrapper;
        }
        return null;
    }

    pub fn deinit(self: *EntryWrapperTable, allocator: Allocator) void {
        allocator.free(self.wrappers);
        self.* = .{};
    }
};

/// Public `PromotedWrapperParam` declaration.
pub const PromotedWrapperParam = struct {
    index: u32,
    checked_ty: CheckedTypeId,
    source_ty: canonical.CanonicalTypeKey,
};

/// Public `PrivateCaptureRef` declaration.
pub const PrivateCaptureRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: PromotedCaptureId,
    node: PrivateCaptureNodeId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
};

/// Public `PrivateCaptureInstantiationKey` declaration.
pub const PrivateCaptureInstantiationKey = struct {
    capture_ref: PrivateCaptureRef,
    requested_source_ty: canonical.CanonicalTypeKey,
};

/// Public `PromotedWrapperArg` declaration.
pub const PromotedWrapperArg = union(enum) {
    param: u32,
    private_capture: PrivateCaptureRef,
};

/// Public `ExecutableValueTransformPlanId` declaration.
pub const ExecutableValueTransformPlanId = enum(u32) { _ };
/// Public `SessionExecutableValueTransformId` declaration.
pub const SessionExecutableValueTransformId = enum(u32) { _ };

/// Public `PublishedExecutableValueTransformRef` declaration.
pub const PublishedExecutableValueTransformRef = struct {
    artifact: CheckedModuleArtifactKey,
    transform: ExecutableValueTransformPlanId,
};

/// Public `ExecutableValueTransformRef` declaration.
pub const ExecutableValueTransformRef = union(enum) {
    session: SessionExecutableValueTransformId,
    published: PublishedExecutableValueTransformRef,
};

/// Public `ExecutableValueEndpoint` declaration.
pub const ExecutableValueEndpoint = struct {
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ValueTransformRecordField` declaration.
pub const ValueTransformRecordField = struct {
    field: canonical.RecordFieldLabelId,
    transform: ExecutableValueTransformPlanId,
};

/// Public `ValueTransformTupleElem` declaration.
pub const ValueTransformTupleElem = struct {
    index: u32,
    transform: ExecutableValueTransformPlanId,
};

/// Public `ValueTransformTagPayloadEdge` declaration.
pub const ValueTransformTagPayloadEdge = struct {
    source_payload_index: u32,
    target_payload_index: u32,
    transform: ExecutableValueTransformPlanId,
};

/// Public `ValueTransformTagCase` declaration.
pub const ValueTransformTagCase = struct {
    source_tag: canonical.TagLabelId,
    target_tag: canonical.TagLabelId,
    payloads: []const ValueTransformTagPayloadEdge = &.{},
};

/// Public `BoxPayloadTransformKind` declaration.
pub const BoxPayloadTransformKind = enum {
    payload_to_box,
    box_to_payload,
    box_to_box,
};

/// Public `BoxPayloadTransformPlan` declaration.
pub const BoxPayloadTransformPlan = struct {
    boundary: ?canonical.BoxBoundaryId = null,
    kind: BoxPayloadTransformKind,
    payload: ExecutableValueTransformPlanId,
};

/// Public `ExecutableValueTransformOp` declaration.
pub const ExecutableValueTransformOp = union(enum) {
    identity,
    structural_bridge: ExecutableStructuralBridgePlan,
    record: []const ValueTransformRecordField,
    tuple: []const ValueTransformTupleElem,
    tag_union: []const ValueTransformTagCase,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        source_ty: canonical.CanonicalTypeKey,
        backing: ExecutableValueTransformPlanId,
    },
    list: struct {
        elem: ExecutableValueTransformPlanId,
    },
    box_payload: BoxPayloadTransformPlan,
    callable_to_erased: CallableToErasedTransformPlan,
    already_erased_callable: AlreadyErasedCallableTransformPlan,
};

/// Public `ExecutableStructuralBridgePlan` declaration.
pub const ExecutableStructuralBridgePlan = union(enum) {
    direct,
    zst,
    list_reinterpret,
    nominal_reinterpret,
    box_unbox: ExecutableValueTransformPlanId,
    box_box: ExecutableValueTransformPlanId,
    singleton_to_tag_union: struct {
        source_tag: canonical.TagLabelId,
        target_tag: canonical.TagLabelId,
        value_transform: ?ExecutableValueTransformPlanId = null,
    },
    tag_union_to_singleton: struct {
        source_tag: canonical.TagLabelId,
        target_tag: canonical.TagLabelId,
        value_transform: ?ExecutableValueTransformPlanId = null,
    },
};

/// Public `CallableToErasedTransformPlan` declaration.
pub const CallableToErasedTransformPlan = union(enum) {
    finite_value: FiniteCallableValueToErasedPlan,
    proc_value: ProcValueToErasedPlan,
};

/// Public `FiniteCallableValueToErasedPlan` declaration.
pub const FiniteCallableValueToErasedPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    adapter_key: canonical.ErasedAdapterKey,
    adapter_branches: []const PublishedFiniteSetEraseAdapterBranchPlan = &.{},
};

/// Public `PublishedFiniteSetEraseAdapterBranchPlan` declaration.
pub const PublishedFiniteSetEraseAdapterBranchPlan = struct {
    member: canonical.CallableSetMemberRef,
    member_proc_source_fn_ty_payload: CheckedTypeId,
    member_lifted_owner_source_fn_ty_payload: ?CheckedTypeId = null,
    target_key: canonical.ExecutableSpecializationKey,
    arg_transforms: []const PublishedExecutableValueTransformRef = &.{},
    capture_transforms: []const PublishedExecutableValueTransformRef = &.{},
    result_transform: PublishedExecutableValueTransformRef,
};

/// Public `ProcValueToErasedPlan` declaration.
pub const ProcValueToErasedPlan = struct {
    proc_value: canonical.ProcedureCallableRef,
    erased_fn_sig_key: canonical.ErasedFnSigKey,
    capture_shape_key: canonical.CaptureShapeKey,
    executable_specialization_key: canonical.ExecutableSpecializationKey,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

/// Public `AlreadyErasedCallableTransformPlan` declaration.
pub const AlreadyErasedCallableTransformPlan = struct {
    sig_key: canonical.ErasedFnSigKey,
};

/// Public `BoxErasureProvenance` declaration.
pub const BoxErasureProvenance = union(enum) {
    /// Local Box(T) boundary from the representation solve session that created the erased value.
    local_box_boundary: canonical.BoxBoundaryId,
    /// Promoted executable wrapper whose sealed plan already carries Box(T) erasure authorization.
    promoted_wrapper: canonical.MirProcedureRef,
};

/// Public `ValueTransformProvenance` declaration.
pub const ValueTransformProvenance = union(enum) {
    none,
    box_erasure: []const BoxErasureProvenance,
};

/// Public `ExecutableValueTransformPlan` declaration.
pub const ExecutableValueTransformPlan = struct {
    from: ExecutableValueEndpoint,
    to: ExecutableValueEndpoint,
    provenance: ValueTransformProvenance = .none,
    op: ExecutableValueTransformOp,
};

/// Public `ExecutableValueTransformPlanStore` declaration.
pub const ExecutableValueTransformPlanStore = struct {
    plans: []ExecutableValueTransformPlan = &.{},

    pub fn append(
        self: *ExecutableValueTransformPlanStore,
        allocator: Allocator,
        plan: ExecutableValueTransformPlan,
    ) Allocator.Error!ExecutableValueTransformPlanId {
        const id: ExecutableValueTransformPlanId = @enumFromInt(@as(u32, @intCast(self.plans.len)));
        const old = self.plans;
        const next = try allocator.alloc(ExecutableValueTransformPlan, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = plan;
        allocator.free(old);
        self.plans = next;
        return id;
    }

    pub fn get(self: *const ExecutableValueTransformPlanStore, id: ExecutableValueTransformPlanId) ExecutableValueTransformPlan {
        const index = @intFromEnum(id);
        if (index >= self.plans.len) {
            checkedArtifactInvariant("executable value transform id is out of range", .{});
        }
        return self.plans[index];
    }

    pub fn verifyPublished(
        self: *const ExecutableValueTransformPlanStore,
        payloads: *const ExecutableTypePayloadStore,
        artifact_key: CheckedModuleArtifactKey,
    ) void {
        if (builtin.mode != .Debug) return;
        for (self.plans) |plan| {
            verifyExecutableTypePayloadRefKey(payloads, artifact_key, plan.from.ty, plan.from.key);
            verifyExecutableTypePayloadRefKey(payloads, artifact_key, plan.to.ty, plan.to.key);
            verifyValueTransformProvenance(plan.provenance);
            switch (plan.op) {
                .callable_to_erased => switch (plan.provenance) {
                    .box_erasure => {},
                    .none => std.debug.panic("checked artifact invariant violated: callable-to-erased transform has no Box(T) provenance", .{}),
                },
                else => {},
            }
            verifyExecutableValueTransformOp(self, plan.op);
        }
    }

    pub fn deinit(self: *ExecutableValueTransformPlanStore, allocator: Allocator) void {
        for (self.plans) |*plan| deinitExecutableValueTransformPlan(allocator, plan);
        allocator.free(self.plans);
        self.* = .{};
    }
};

/// Public `ExecutableTypePayloadId` declaration.
pub const ExecutableTypePayloadId = enum(u32) { _ };

/// Public `ExecutableTypePayloadRef` declaration.
pub const ExecutableTypePayloadRef = struct {
    artifact: canonical.ArtifactRef,
    payload: ExecutableTypePayloadId,
};

/// Public `ExecutablePrimitive` declaration.
pub const ExecutablePrimitive = enum {
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    erased,
};

/// Public `ExecutableTypePayloadChild` declaration.
pub const ExecutableTypePayloadChild = struct {
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ExecutableRecordFieldPayload` declaration.
pub const ExecutableRecordFieldPayload = struct {
    field: canonical.RecordFieldLabelId,
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ExecutableTupleElemPayload` declaration.
pub const ExecutableTupleElemPayload = struct {
    index: u32,
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ExecutableTagPayload` declaration.
pub const ExecutableTagPayload = struct {
    index: u32,
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ExecutableTagVariantPayload` declaration.
pub const ExecutableTagVariantPayload = struct {
    tag: canonical.TagLabelId,
    payloads: []const ExecutableTagPayload = &.{},
};

/// Public `ExecutableNominalPayload` declaration.
pub const ExecutableNominalPayload = struct {
    nominal: canonical.NominalTypeKey,
    source_ty: canonical.CanonicalTypeKey,
    backing: ExecutableTypePayloadRef,
    backing_key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ExecutableCallableSetMemberPayload` declaration.
pub const ExecutableCallableSetMemberPayload = struct {
    member: canonical.CallableSetMemberId,
    payload_ty: ?ExecutableTypePayloadRef = null,
    payload_ty_key: ?canonical.CanonicalExecValueTypeKey = null,
};

/// Public `ExecutableCallableSetPayload` declaration.
pub const ExecutableCallableSetPayload = struct {
    key: canonical.CanonicalCallableSetKey,
    members: []const ExecutableCallableSetMemberPayload = &.{},
};

/// Public `ExecutableErasedFnPayload` declaration.
pub const ExecutableErasedFnPayload = struct {
    sig_key: canonical.ErasedFnSigKey,
    capture_shape_key: canonical.CaptureShapeKey,
    capture_ty: ?ExecutableTypePayloadRef = null,
    capture_ty_key: ?canonical.CanonicalExecValueTypeKey = null,
};

/// Public `ExecutableTypePayload` declaration.
pub const ExecutableTypePayload = union(enum) {
    pending,
    primitive: ExecutablePrimitive,
    record: []const ExecutableRecordFieldPayload,
    tuple: []const ExecutableTupleElemPayload,
    tag_union: []const ExecutableTagVariantPayload,
    list: ExecutableTypePayloadChild,
    box: ExecutableTypePayloadChild,
    nominal: ExecutableNominalPayload,
    callable_set: ExecutableCallableSetPayload,
    erased_fn: ExecutableErasedFnPayload,
    vacant_callable_slot,
    recursive_ref: ExecutableTypePayloadId,
};

/// Public `ExecutableTypePayloadEntry` declaration.
pub const ExecutableTypePayloadEntry = struct {
    key: canonical.CanonicalExecValueTypeKey,
    payload: ExecutableTypePayload,
};

/// Public `ExecutableTypePayloadStore` declaration.
pub const ExecutableTypePayloadStore = struct {
    entries: []ExecutableTypePayloadEntry = &.{},
    by_key: std.AutoHashMap(canonical.CanonicalExecValueTypeKey, ExecutableTypePayloadId),

    pub fn init(allocator: Allocator) ExecutableTypePayloadStore {
        return .{
            .by_key = std.AutoHashMap(canonical.CanonicalExecValueTypeKey, ExecutableTypePayloadId).init(allocator),
        };
    }

    pub fn reserve(
        self: *ExecutableTypePayloadStore,
        allocator: Allocator,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!ExecutableTypePayloadId {
        if (self.by_key.get(key) != null) checkedArtifactInvariant("executable type payload reserve saw duplicate key", .{});
        return try self.appendNew(allocator, key, .pending);
    }

    pub fn append(
        self: *ExecutableTypePayloadStore,
        allocator: Allocator,
        key: canonical.CanonicalExecValueTypeKey,
        payload: ExecutableTypePayload,
    ) Allocator.Error!ExecutableTypePayloadId {
        if (self.by_key.get(key)) |existing| {
            var duplicate = payload;
            deinitExecutableTypePayload(allocator, &duplicate);
            return existing;
        }
        return try self.appendNew(allocator, key, payload);
    }

    pub fn replaceDerived(
        self: *ExecutableTypePayloadStore,
        allocator: Allocator,
        key: canonical.CanonicalExecValueTypeKey,
        payload: ExecutableTypePayload,
    ) Allocator.Error!ExecutableTypePayloadId {
        if (self.by_key.get(key)) |existing| {
            const index = @intFromEnum(existing);
            if (index >= self.entries.len) {
                checkedArtifactInvariant("executable type payload id is out of range", .{});
            }
            deinitExecutableTypePayload(allocator, &self.entries[index].payload);
            self.entries[index].payload = payload;
            return existing;
        }
        return try self.appendNew(allocator, key, payload);
    }

    fn appendNew(
        self: *ExecutableTypePayloadStore,
        allocator: Allocator,
        key: canonical.CanonicalExecValueTypeKey,
        payload: ExecutableTypePayload,
    ) Allocator.Error!ExecutableTypePayloadId {
        const id: ExecutableTypePayloadId = @enumFromInt(@as(u32, @intCast(self.entries.len)));
        const old = self.entries;
        const next = try allocator.alloc(ExecutableTypePayloadEntry, old.len + 1);
        errdefer allocator.free(next);
        @memcpy(next[0..old.len], old);
        next[old.len] = .{
            .key = key,
            .payload = payload,
        };
        try self.by_key.put(key, id);
        allocator.free(old);
        self.entries = next;
        return id;
    }

    pub fn fill(
        self: *ExecutableTypePayloadStore,
        id: ExecutableTypePayloadId,
        payload: ExecutableTypePayload,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) {
            checkedArtifactInvariant("executable type payload id is out of range", .{});
        }
        switch (payload) {
            .pending => checkedArtifactInvariant("cannot fill executable type payload with pending", .{}),
            else => {},
        }
        switch (self.entries[index].payload) {
            .pending => self.entries[index].payload = payload,
            else => checkedArtifactInvariant("executable type payload was filled twice", .{}),
        }
    }

    pub fn get(self: *const ExecutableTypePayloadStore, id: ExecutableTypePayloadId) ExecutableTypePayload {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) {
            checkedArtifactInvariant("executable type payload id is out of range", .{});
        }
        return self.entries[index].payload;
    }

    pub fn keyFor(self: *const ExecutableTypePayloadStore, id: ExecutableTypePayloadId) canonical.CanonicalExecValueTypeKey {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) {
            checkedArtifactInvariant("executable type payload id is out of range", .{});
        }
        return self.entries[index].key;
    }

    pub fn refForKey(
        self: *const ExecutableTypePayloadStore,
        artifact: canonical.ArtifactRef,
        key: canonical.CanonicalExecValueTypeKey,
    ) ?ExecutableTypePayloadRef {
        const id = self.by_key.get(key) orelse return null;
        return .{
            .artifact = artifact,
            .payload = id,
        };
    }

    pub fn verifyPublished(
        self: *const ExecutableTypePayloadStore,
        artifact_key: CheckedModuleArtifactKey,
        erased_fn_abis: *const canonical.ErasedFnAbiStore,
    ) void {
        if (builtin.mode != .Debug) return;
        for (self.entries, 0..) |entry, i| {
            const indexed = self.by_key.get(entry.key) orelse {
                std.debug.panic("checked artifact invariant violated: executable type payload key was missing from index", .{});
            };
            if (@intFromEnum(indexed) != i) {
                std.debug.panic("checked artifact invariant violated: executable type payload key index pointed at the wrong entry", .{});
            }
            verifyExecutableTypePayload(self, artifact_key, erased_fn_abis, entry.payload);
        }
        for (erased_fn_abis.abis) |abi| {
            if (self.by_key.get(abi.ret_exec_key) == null) {
                std.debug.panic("checked artifact invariant violated: erased ABI result key has no executable type payload", .{});
            }
            for (abi.arg_exec_keys) |arg_key| {
                if (self.by_key.get(arg_key) == null) {
                    std.debug.panic("checked artifact invariant violated: erased ABI argument key has no executable type payload", .{});
                }
            }
        }
    }

    pub fn deinit(self: *ExecutableTypePayloadStore, allocator: Allocator) void {
        for (self.entries) |*entry| deinitExecutableTypePayload(allocator, &entry.payload);
        allocator.free(self.entries);
        self.by_key.deinit();
        self.* = ExecutableTypePayloadStore.init(allocator);
    }
};

/// Public `CallableSetDescriptorStore` declaration.
pub const CallableSetDescriptorStore = struct {
    descriptors: []const canonical.CanonicalCallableSetDescriptor = &.{},

    pub fn descriptorFor(
        self: *const CallableSetDescriptorStore,
        key: canonical.CanonicalCallableSetKey,
    ) ?*const canonical.CanonicalCallableSetDescriptor {
        for (self.descriptors) |*descriptor| {
            if (canonicalCallableSetKeyEql(descriptor.key, key)) return descriptor;
        }
        return null;
    }

    pub fn publishFromDescriptors(
        self: *CallableSetDescriptorStore,
        allocator: Allocator,
        source_descriptors: []const canonical.CanonicalCallableSetDescriptor,
    ) Allocator.Error!void {
        if (source_descriptors.len == 0) return;

        var additions = std.ArrayList(canonical.CanonicalCallableSetDescriptor).empty;
        defer additions.deinit(allocator);
        for (source_descriptors) |source| {
            if (self.descriptorFor(source.key)) |existing| {
                if (!canonicalCallableSetDescriptorEql(existing.*, source)) {
                    checkedArtifactInvariant("callable-set descriptor store was already published with different descriptor contents", .{});
                }
                continue;
            }

            var duplicate_addition = false;
            for (additions.items) |existing| {
                if (!canonicalCallableSetKeyEql(existing.key, source.key)) continue;
                duplicate_addition = true;
                if (!canonicalCallableSetDescriptorEql(existing, source)) {
                    checkedArtifactInvariant("duplicate callable-set descriptor key has different descriptor contents", .{});
                }
                break;
            }
            if (!duplicate_addition) try additions.append(allocator, source);
        }
        if (additions.items.len == 0) {
            return;
        }

        const old = self.descriptors;
        const copied = try allocator.alloc(canonical.CanonicalCallableSetDescriptor, old.len + additions.items.len);
        errdefer allocator.free(copied);
        @memcpy(copied[0..old.len], old);

        var descriptor_count: usize = old.len;
        errdefer {
            for (copied[old.len..descriptor_count]) |descriptor| {
                for (descriptor.members) |member| allocator.free(member.capture_slots);
                allocator.free(descriptor.members);
            }
        }

        for (additions.items) |descriptor| {
            const members = try cloneCallableSetMembers(allocator, descriptor.members);
            copied[descriptor_count] = .{
                .key = descriptor.key,
                .members = members,
            };
            descriptor_count += 1;
        }

        self.descriptors = copied;
        if (old.len != 0) allocator.free(old);
    }

    pub fn deinit(self: *CallableSetDescriptorStore, allocator: Allocator) void {
        for (self.descriptors) |descriptor| {
            for (descriptor.members) |member| allocator.free(member.capture_slots);
            allocator.free(descriptor.members);
        }
        allocator.free(self.descriptors);
        self.* = .{};
    }

    pub fn verifyPublished(self: *const CallableSetDescriptorStore) void {
        if (builtin.mode != .Debug) return;

        for (self.descriptors, 0..) |descriptor, i| {
            verifyCallableSetDescriptor(descriptor);
            for (self.descriptors[i + 1 ..]) |other| {
                if (!canonicalCallableSetKeyEql(descriptor.key, other.key)) continue;
                if (!canonicalCallableSetDescriptorEql(descriptor, other)) {
                    std.debug.panic("checked artifact invariant violated: duplicate callable-set descriptor key has different descriptor", .{});
                }
            }
        }
    }
};

fn cloneCallableSetMembers(
    allocator: Allocator,
    source_members: []const canonical.CanonicalCallableSetMember,
) Allocator.Error![]const canonical.CanonicalCallableSetMember {
    const members = try allocator.alloc(canonical.CanonicalCallableSetMember, source_members.len);
    errdefer allocator.free(members);
    var member_count: usize = 0;
    errdefer {
        for (members[0..member_count]) |member| allocator.free(member.capture_slots);
    }

    for (source_members, 0..) |member, i| {
        const capture_slots = try allocator.dupe(canonical.CallableSetCaptureSlot, member.capture_slots);
        members[i] = .{
            .member = member.member,
            .proc_value = member.proc_value,
            .source_proc = member.source_proc,
            .capture_slots = capture_slots,
            .capture_shape_key = member.capture_shape_key,
        };
        member_count += 1;
    }

    return members;
}

/// Public `ExecutableProcedureParamPayload` declaration.
pub const ExecutableProcedureParamPayload = struct {
    param: PromotedWrapperParam,
    exec_ty: ExecutableTypePayloadRef,
    exec_ty_key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ExecutableHiddenCapturePayload` declaration.
pub const ExecutableHiddenCapturePayload = struct {
    exec_ty: ExecutableTypePayloadRef,
    exec_ty_key: canonical.CanonicalExecValueTypeKey,
};

/// Public `ErasedPromotedProcedureExecutableSignaturePayloads` declaration.
pub const ErasedPromotedProcedureExecutableSignaturePayloads = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    param_exec_tys: []const ExecutableTypePayloadRef = &.{},
    param_exec_ty_keys: []const canonical.CanonicalExecValueTypeKey = &.{},
    wrapper_ret: ExecutableTypePayloadRef,
    wrapper_ret_key: canonical.CanonicalExecValueTypeKey,
    erased_call_args: []const ExecutableTypePayloadRef = &.{},
    erased_call_arg_keys: []const canonical.CanonicalExecValueTypeKey = &.{},
    erased_call_ret: ExecutableTypePayloadRef,
    erased_call_ret_key: canonical.CanonicalExecValueTypeKey,
    hidden_capture: ?ExecutableHiddenCapturePayload = null,
    capture_shape_key: canonical.CaptureShapeKey,
};

/// Public `ErasedPromotedProcedureExecutableSignature` declaration.
pub const ErasedPromotedProcedureExecutableSignature = struct {
    specialization_key: canonical.ExecutableSpecializationKey,
    source_fn_ty: canonical.CanonicalTypeKey,
    wrapper_params: []const ExecutableProcedureParamPayload = &.{},
    wrapper_ret: ExecutableTypePayloadRef,
    wrapper_ret_key: canonical.CanonicalExecValueTypeKey,
    erased_call_args: []const ExecutableTypePayloadRef = &.{},
    erased_call_arg_keys: []const canonical.CanonicalExecValueTypeKey = &.{},
    erased_call_ret: ExecutableTypePayloadRef,
    erased_call_ret_key: canonical.CanonicalExecValueTypeKey,
    hidden_capture: ?ExecutableHiddenCapturePayload = null,
};

/// Public `ExecutableSpecializationEndpoint` declaration.
pub const ExecutableSpecializationEndpoint = struct {
    requested_fn_ty: canonical.CanonicalTypeKey,
    exec_arg_tys: []const canonical.CanonicalExecValueTypeKey,
    exec_ret_ty: canonical.CanonicalExecValueTypeKey,
    callable_repr_mode: canonical.CallableReprMode,
    capture_shape_key: canonical.CaptureShapeKey,
};

/// Public `CallableResultMemberTargetPlan` declaration.
pub const CallableResultMemberTargetPlan = union(enum) {
    artifact_owned: canonical.ExecutableSpecializationKey,
    member_proc_relative: ExecutableSpecializationEndpoint,
};

/// Public `FinitePromotedWrapperBodyPlan` declaration.
pub const FinitePromotedWrapperBodyPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    member: canonical.CallableSetMemberId,
    member_proc: canonical.ProcedureCallableRef,
    member_proc_source_fn_ty_payload: CheckedTypeId,
    member_lifted_owner_source_fn_ty_payload: ?CheckedTypeId = null,
    member_target: CallableResultMemberTargetPlan,
    member_target_promoted_wrapper: ?canonical.MirProcedureRef = null,
    member_capture_shape: canonical.CaptureShapeKey,
    member_capture_slots: []const canonical.CallableSetCaptureSlot = &.{},
    captures: []const PrivateCaptureRef = &.{},
    params: []const PromotedWrapperParam = &.{},
    call_args: []const PromotedWrapperArg = &.{},
};

/// Public `ErasedHiddenCaptureArgPlan` declaration.
pub const ErasedHiddenCaptureArgPlan = union(enum) {
    none,
    materialized_capture: ErasedCaptureExecutableMaterializationPlan,
};

/// Public `ErasedCaptureExecutableMaterializationPlan` declaration.
pub const ErasedCaptureExecutableMaterializationPlan = union(enum) {
    none,
    zero_sized_typed: canonical.CanonicalExecValueTypeKey,
    node: ErasedCaptureExecutableMaterializationNodeId,
};

/// Public `NoReachableCallableSlotsProof` declaration.
pub const NoReachableCallableSlotsProof = enum {
    checked_artifact_verified,
};

/// Public `PureConstInstanceRef` declaration.
pub const PureConstInstanceRef = struct {
    const_instance: ConstInstanceRef,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

/// Public `PureComptimeValueRef` declaration.
pub const PureComptimeValueRef = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

/// Public `ErasedCaptureExecutableMaterializationRecordField` declaration.
pub const ErasedCaptureExecutableMaterializationRecordField = struct {
    field: canonical.RecordFieldLabelId,
    value: ErasedCaptureExecutableMaterializationPlan,
};

/// Public `ErasedCaptureExecutableMaterializationTagPayload` declaration.
pub const ErasedCaptureExecutableMaterializationTagPayload = struct {
    index: u32,
    value: ErasedCaptureExecutableMaterializationPlan,
};

/// Public `ErasedCaptureExecutableMaterializationTagNode` declaration.
pub const ErasedCaptureExecutableMaterializationTagNode = struct {
    tag: canonical.TagLabelId,
    payloads: []const ErasedCaptureExecutableMaterializationTagPayload,
};

/// Public `MaterializedFiniteCallableSetValue` declaration.
pub const MaterializedFiniteCallableSetValue = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    selected_member: canonical.CallableSetMemberId,
    captures: []const ErasedCaptureExecutableMaterializationPlan = &.{},
};

/// Public `MaterializedErasedCallableValue` declaration.
pub const MaterializedErasedCallableValue = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    sig_key: canonical.ErasedFnSigKey,
    code: canonical.ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
    provenance: []const BoxErasureProvenance,
};

/// Public `ErasedCaptureExecutableMaterializationNode` declaration.
pub const ErasedCaptureExecutableMaterializationNode = union(enum) {
    pending,
    const_instance: ConstInstanceRef,
    pure_const: PureConstInstanceRef,
    pure_value: PureComptimeValueRef,
    finite_callable_set: MaterializedFiniteCallableSetValue,
    erased_callable: MaterializedErasedCallableValue,
    record: []const ErasedCaptureExecutableMaterializationRecordField,
    tuple: []const ErasedCaptureExecutableMaterializationPlan,
    tag_union: ErasedCaptureExecutableMaterializationTagNode,
    list: []const ErasedCaptureExecutableMaterializationPlan,
    box: ErasedCaptureExecutableMaterializationPlan,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: ErasedCaptureExecutableMaterializationPlan,
    },
    recursive_ref: ErasedCaptureExecutableMaterializationNodeId,
};

/// Public `ErasedPromotedWrapperBodyPlan` declaration.
pub const ErasedPromotedWrapperBodyPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    params: []const PromotedWrapperParam = &.{},
    executable_signature: ErasedPromotedProcedureExecutableSignature,
    sig_key: canonical.ErasedFnSigKey,
    code: canonical.ErasedCallableCodeRef,
    finite_adapter_member_targets: []const canonical.ExecutableSpecializationKey = &.{},
    finite_adapter_branches: []const PublishedFiniteSetEraseAdapterBranchPlan = &.{},
    capture: ErasedCaptureExecutableMaterializationPlan,
    arg_transforms: []const PublishedExecutableValueTransformRef = &.{},
    hidden_capture_arg: ErasedHiddenCaptureArgPlan = .none,
    result_transform: PublishedExecutableValueTransformRef,
    provenance: []const BoxErasureProvenance,
};

/// Public `PromotedCallableBodyPlan` declaration.
pub const PromotedCallableBodyPlan = union(enum) {
    pending,
    finite: FinitePromotedWrapperBodyPlan,
    erased: ErasedPromotedWrapperBodyPlan,
};

/// Public `PromotedCallableBodyPlanTable` declaration.
pub const PromotedCallableBodyPlanTable = struct {
    plans: []PromotedCallableBodyPlan = &.{},

    pub fn append(
        self: *PromotedCallableBodyPlanTable,
        allocator: Allocator,
        plan: PromotedCallableBodyPlan,
    ) Allocator.Error!canonical.PromotedCallableBodyPlanId {
        const id: canonical.PromotedCallableBodyPlanId = @enumFromInt(@as(u32, @intCast(self.plans.len)));
        const old = self.plans;
        const next = try allocator.alloc(PromotedCallableBodyPlan, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = plan;
        allocator.free(old);
        self.plans = next;
        return id;
    }

    pub fn reserve(
        self: *PromotedCallableBodyPlanTable,
        allocator: Allocator,
    ) Allocator.Error!canonical.PromotedCallableBodyPlanId {
        return try self.append(allocator, .pending);
    }

    pub fn fill(
        self: *PromotedCallableBodyPlanTable,
        id: canonical.PromotedCallableBodyPlanId,
        plan: PromotedCallableBodyPlan,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.plans.len) {
            checkedArtifactInvariant("promoted callable body plan id is out of range", .{});
        }
        switch (plan) {
            .pending => checkedArtifactInvariant("cannot fill promoted callable body plan with pending", .{}),
            .finite, .erased => {},
        }
        switch (self.plans[index]) {
            .pending => self.plans[index] = plan,
            .finite, .erased => checkedArtifactInvariant("promoted callable body plan was filled twice", .{}),
        }
    }

    pub fn get(self: *const PromotedCallableBodyPlanTable, id: canonical.PromotedCallableBodyPlanId) PromotedCallableBodyPlan {
        const index = @intFromEnum(id);
        if (index >= self.plans.len) {
            checkedArtifactInvariant("promoted callable body plan id is out of range", .{});
        }
        return self.plans[index];
    }

    pub fn verifyPublished(
        self: *const PromotedCallableBodyPlanTable,
        plans: *const CompileTimePlanStore,
        checked_types: *const CheckedTypeStore,
        executable_type_payloads: *const ExecutableTypePayloadStore,
        executable_value_transforms: *const ExecutableValueTransformPlanStore,
        erased_fn_abis: *const canonical.ErasedFnAbiStore,
        artifact_key: CheckedModuleArtifactKey,
    ) void {
        if (builtin.mode != .Debug) return;

        for (self.plans) |plan| verifyPromotedCallableBodyPlan(
            plans,
            checked_types,
            executable_type_payloads,
            executable_value_transforms,
            erased_fn_abis,
            artifact_key,
            plan,
        );
    }

    pub fn deinit(self: *PromotedCallableBodyPlanTable, allocator: Allocator) void {
        for (self.plans) |*plan| deinitPromotedCallableBodyPlan(allocator, plan);
        allocator.free(self.plans);
        self.* = .{};
    }
};

/// Public `PromotedCallableWrapper` declaration.
pub const PromotedCallableWrapper = struct {
    id: canonical.PromotedCallableWrapperId,
    promoted_proc: canonical.ProcedureValueRef,
    proc_base_key: canonical.ProcBaseKeyRef,
    callable_node: canonical.PromotedCallableNodeId,
    source_binding: ?CheckedPatternId,
    source_fn_ty: canonical.CanonicalTypeKey,
    provenance: PromotedProcedureProvenance,
    checked_fn_root: CheckedTypeId,
    body_plan: canonical.PromotedCallableBodyPlanId,
};

/// Public `PromotedCallableWrapperTable` declaration.
pub const PromotedCallableWrapperTable = struct {
    wrappers: []PromotedCallableWrapper = &.{},

    pub fn append(
        self: *PromotedCallableWrapperTable,
        allocator: Allocator,
        wrapper: PromotedCallableWrapper,
    ) Allocator.Error!canonical.PromotedCallableWrapperId {
        const id: canonical.PromotedCallableWrapperId = @enumFromInt(@as(u32, @intCast(self.wrappers.len)));
        if (wrapper.id != id) {
            checkedArtifactInvariant("promoted callable wrapper id does not match append slot", .{});
        }
        for (self.wrappers) |existing| {
            if (canonical.procedureValueRefEql(existing.promoted_proc, wrapper.promoted_proc)) {
                checkedArtifactInvariant("promoted callable wrapper procedure was published twice", .{});
            }
        }
        const old = self.wrappers;
        const next = try allocator.alloc(PromotedCallableWrapper, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = wrapper;
        allocator.free(old);
        self.wrappers = next;
        return id;
    }

    pub fn get(self: *const PromotedCallableWrapperTable, id: canonical.PromotedCallableWrapperId) PromotedCallableWrapper {
        const index = @intFromEnum(id);
        if (index >= self.wrappers.len) {
            checkedArtifactInvariant("promoted callable wrapper id is out of range", .{});
        }
        return self.wrappers[index];
    }

    pub fn verifyPublished(
        self: *const PromotedCallableWrapperTable,
        artifact_key: CheckedModuleArtifactKey,
        checked_types: *const CheckedTypeStore,
        checked_bodies: *const CheckedBodyStore,
        body_plans: *const PromotedCallableBodyPlanTable,
    ) void {
        if (builtin.mode != .Debug) return;

        for (self.wrappers, 0..) |wrapper, i| {
            std.debug.assert(@intFromEnum(wrapper.id) == i);
            if (!std.mem.eql(u8, &wrapper.promoted_proc.artifact.bytes, &artifact_key.bytes)) {
                std.debug.panic("checked artifact invariant violated: promoted callable wrapper procedure belongs to a different artifact", .{});
            }
            if (wrapper.promoted_proc.proc_base != wrapper.proc_base_key) {
                std.debug.panic("checked artifact invariant violated: promoted callable wrapper procedure/base mismatch", .{});
            }
            if (@intFromEnum(wrapper.checked_fn_root) >= checked_types.roots.len) {
                std.debug.panic("checked artifact invariant violated: promoted callable wrapper checked function root is out of range", .{});
            }
            if (wrapper.source_binding) |source_binding| {
                if (@intFromEnum(source_binding) >= checked_bodies.patterns.len) {
                    std.debug.panic("checked artifact invariant violated: promoted callable wrapper source binding is out of range", .{});
                }
            }
            if (!std.mem.eql(u8, &wrapper.source_fn_ty.bytes, &checked_types.roots[@intFromEnum(wrapper.checked_fn_root)].key.bytes)) {
                std.debug.panic("checked artifact invariant violated: promoted callable wrapper source function type differs from checked root", .{});
            }
            if (@intFromEnum(wrapper.body_plan) >= body_plans.plans.len) {
                std.debug.panic("checked artifact invariant violated: promoted callable wrapper body plan is out of range", .{});
            }
        }
    }

    pub fn deinit(self: *PromotedCallableWrapperTable, allocator: Allocator) void {
        allocator.free(self.wrappers);
        self.* = .{};
    }
};

/// Public `StaticDispatchPlanTableRef` declaration.
pub const StaticDispatchPlanTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

/// Public `ResolvedValueRefTableRef` declaration.
pub const ResolvedValueRefTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

/// Public `ResolvedValueRefId` declaration.
pub const ResolvedValueRefId = enum(u32) { _ };

/// Public `LocalBindingRef` declaration.
pub const LocalBindingRef = struct {
    binder: PatternBinderId,
};

/// Public `TopLevelBindingRef` declaration.
pub const TopLevelBindingRef = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
};

/// Public `ImportedTopLevelValueRef` declaration.
pub const ImportedTopLevelValueRef = struct {
    artifact: CheckedModuleArtifactKey,
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
};

/// Public `HostedProcRef` declaration.
pub const HostedProcRef = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

/// Public `TopLevelValueRef` declaration.
pub const TopLevelValueRef = struct {
    artifact: CheckedModuleArtifactKey,
    pattern: CheckedPatternId,
};

/// Public `RequiredAppProcedureRef` declaration.
pub const RequiredAppProcedureRef = struct {
    artifact: CheckedModuleArtifactKey,
    app_value: TopLevelValueRef,
    procedure_binding: TopLevelProcedureBindingRef,
};

/// Public `PromotedProcedureRef` declaration.
pub const PromotedProcedureRef = struct {
    module_idx: u32,
    proc: canonical.ProcedureValueRef,
};

/// Public `ConstUseTemplate` declaration.
pub const ConstUseTemplate = struct {
    const_ref: ConstRef,
    requested_source_ty_template: canonical.CanonicalTypeKey,
    requested_source_ty_payload: ?CheckedTypeId = null,
};

/// Public `ProcedureBindingRef` declaration.
pub const ProcedureBindingRef = union(enum) {
    top_level: TopLevelProcedureBindingRef,
    imported: ImportedProcedureBindingRef,
    hosted: HostedProcRef,
    platform_required: RequiredAppProcedureRef,
    promoted: PromotedProcedureRef,
};

/// Public `TopLevelProcedureBindingRef` declaration.
pub const TopLevelProcedureBindingRef = enum(u32) { _ };
/// Public `CallableEvalTemplateId` declaration.
pub const CallableEvalTemplateId = enum(u32) { _ };

/// Public `DirectProcedureBinding` declaration.
pub const DirectProcedureBinding = struct {
    proc_value: canonical.ProcedureValueRef,
    template: canonical.CallableProcedureTemplateRef,
};

/// Public `ProcedureBindingBody` declaration.
pub const ProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateId,
};

/// Public `TopLevelProcedureBinding` declaration.
pub const TopLevelProcedureBinding = struct {
    source_scheme: canonical.CanonicalTypeSchemeKey,
    body: ProcedureBindingBody,
};

/// Public `TopLevelProcedureBindingTable` declaration.
pub const TopLevelProcedureBindingTable = struct {
    bindings: []TopLevelProcedureBinding = &.{},

    pub fn initEmpty() TopLevelProcedureBindingTable {
        return .{};
    }

    pub fn appendDirect(
        self: *TopLevelProcedureBindingTable,
        allocator: Allocator,
        source_scheme: canonical.CanonicalTypeSchemeKey,
        proc_value: canonical.ProcedureValueRef,
        template: canonical.ProcedureTemplateRef,
    ) Allocator.Error!TopLevelProcedureBindingRef {
        const old = self.bindings;
        const next = try allocator.alloc(TopLevelProcedureBinding, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) allocator.free(old);
        self.bindings = next;
        const ref: TopLevelProcedureBindingRef = @enumFromInt(@as(u32, @intCast(old.len)));
        self.bindings[old.len] = .{
            .source_scheme = source_scheme,
            .body = .{ .direct_template = .{
                .proc_value = proc_value,
                .template = .{ .checked = template },
            } },
        };
        return ref;
    }

    pub fn appendCallableEval(
        self: *TopLevelProcedureBindingTable,
        allocator: Allocator,
        source_scheme: canonical.CanonicalTypeSchemeKey,
        template: CallableEvalTemplateId,
    ) Allocator.Error!TopLevelProcedureBindingRef {
        const old = self.bindings;
        const next = try allocator.alloc(TopLevelProcedureBinding, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) allocator.free(old);
        self.bindings = next;
        const ref: TopLevelProcedureBindingRef = @enumFromInt(@as(u32, @intCast(old.len)));
        self.bindings[old.len] = .{
            .source_scheme = source_scheme,
            .body = .{ .callable_eval_template = template },
        };
        return ref;
    }

    pub fn get(self: *const TopLevelProcedureBindingTable, ref: TopLevelProcedureBindingRef) TopLevelProcedureBinding {
        return self.bindings[@intFromEnum(ref)];
    }

    pub fn deinit(self: *TopLevelProcedureBindingTable, allocator: Allocator) void {
        if (self.bindings.len > 0) allocator.free(self.bindings);
        self.* = .{};
    }
};

/// Public `CallableEvalTemplate` declaration.
pub const CallableEvalTemplate = struct {
    id: CallableEvalTemplateId,
    module_idx: u32,
    pattern: CheckedPatternId,
    root: ComptimeRootId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    checked_fn_root: CheckedTypeId,
};

/// Public `CallableEvalTemplateTableView` declaration.
pub const CallableEvalTemplateTableView = struct {
    templates: []const CallableEvalTemplate = &.{},
};

/// Public `CallableEvalTemplateTable` declaration.
pub const CallableEvalTemplateTable = struct {
    templates: []CallableEvalTemplate = &.{},

    pub fn append(
        self: *CallableEvalTemplateTable,
        allocator: Allocator,
        module_idx: u32,
        pattern: CheckedPatternId,
        root: ComptimeRootId,
        source_scheme: canonical.CanonicalTypeSchemeKey,
        checked_fn_root: CheckedTypeId,
    ) Allocator.Error!CallableEvalTemplateId {
        const old = self.templates;
        const next = try allocator.alloc(CallableEvalTemplate, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) allocator.free(old);
        self.templates = next;

        const id: CallableEvalTemplateId = @enumFromInt(@as(u32, @intCast(old.len)));
        self.templates[old.len] = .{
            .id = id,
            .module_idx = module_idx,
            .pattern = pattern,
            .root = root,
            .source_scheme = source_scheme,
            .checked_fn_root = checked_fn_root,
        };
        return id;
    }

    pub fn get(self: *const CallableEvalTemplateTable, id: CallableEvalTemplateId) CallableEvalTemplate {
        return self.templates[@intFromEnum(id)];
    }

    pub fn view(self: *const CallableEvalTemplateTable) CallableEvalTemplateTableView {
        return .{ .templates = self.templates };
    }

    pub fn deinit(self: *CallableEvalTemplateTable, allocator: Allocator) void {
        if (self.templates.len > 0) allocator.free(self.templates);
        self.* = .{};
    }
};

/// Public `ImportedProcedureBindingRef` declaration.
pub const ImportedProcedureBindingRef = struct {
    artifact: CheckedModuleArtifactKey,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
};

/// Public `ProcedureUseTemplate` declaration.
pub const ProcedureUseTemplate = struct {
    binding: ProcedureBindingRef,
    source_fn_ty_template: canonical.CanonicalTypeKey,
    source_fn_ty_payload: ?CheckedTypeId = null,
};

/// Public `PlatformRequiredConstResolvedRef` declaration.
pub const PlatformRequiredConstResolvedRef = struct {
    binding: PlatformRequiredBindingId,
    const_use: ConstUseTemplate,
};

/// Public `PlatformRequiredProcedureResolvedRef` declaration.
pub const PlatformRequiredProcedureResolvedRef = struct {
    binding: PlatformRequiredBindingId,
    procedure: ProcedureUseTemplate,
};

/// Public `ResolvedValueRef` declaration.
pub const ResolvedValueRef = union(enum) {
    local_param: LocalBindingRef,
    local_value: LocalBindingRef,
    local_mutable_version: LocalBindingRef,
    pattern_binder: LocalBindingRef,
    local_proc: LocalBindingRef,

    top_level_const: ConstUseTemplate,
    imported_const: ConstUseTemplate,

    top_level_proc: ProcedureUseTemplate,
    imported_proc: ProcedureUseTemplate,
    hosted_proc: ProcedureUseTemplate,
    platform_required_declaration: PlatformRequiredDeclarationId,
    platform_required_const: PlatformRequiredConstResolvedRef,
    platform_required_proc: PlatformRequiredProcedureResolvedRef,
    promoted_top_level_proc: ProcedureUseTemplate,
};

/// Public `ResolvedValueRefRecord` declaration.
pub const ResolvedValueRefRecord = struct {
    expr: CheckedExprId,
    ref: ResolvedValueRef,
    checked_ty: CheckedTypeId,
    scope_depth: u32,
};

/// Public `ResolvedValueRefTable` declaration.
pub const ResolvedValueRefTable = struct {
    records: []ResolvedValueRefRecord = &.{},
    by_checked_expr: []?ResolvedValueRefId = &.{},
    template_refs: []ResolvedValueRefId = &.{},

    pub fn fromModule(
        allocator: Allocator,
        modules: *const TypedCIR.Modules,
        module_idx: u32,
        artifact_key: CheckedModuleArtifactKey,
        imports: []const PublishImportArtifact,
        templates: *const CheckedProcedureTemplateTable,
        hosted_procs: *const HostedProcTable,
        platform_required_declarations: *const PlatformRequiredDeclarationTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        top_level_values: *const TopLevelValueTable,
        checked_types: *const CheckedTypePublication,
        checked_bodies: *const CheckedBodyStore,
    ) Allocator.Error!ResolvedValueRefTable {
        const module = modules.module(module_idx);
        var records = std.ArrayList(ResolvedValueRefRecord).empty;
        errdefer records.deinit(allocator);

        const by_checked_expr = try allocator.alloc(?ResolvedValueRefId, checked_bodies.exprs.len);
        errdefer allocator.free(by_checked_expr);
        @memset(by_checked_expr, null);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const tag = module.nodeTag(@enumFromInt(node_idx));
            switch (tag) {
                .expr_var,
                .expr_external_lookup,
                .expr_required_lookup,
                => {},
                else => continue,
            }

            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: resolved value ref expression {d} has no checked expression id",
                        .{@intFromEnum(expr_idx)},
                    );
                }
                unreachable;
            };
            var resolved_ref = try classifyValueRef(
                allocator,
                module,
                expr_idx,
                imports,
                templates,
                hosted_procs,
                platform_required_declarations,
                platform_required_bindings,
                top_level_values,
                checked_bodies,
            );
            const checked_type_key = try canonical_type_keys.fromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                module.exprType(expr_idx),
            );
            const checked_ty = checked_types.rootForSourceVar(module, module.exprType(expr_idx)) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: resolved value ref type root was not published", .{});
                }
                unreachable;
            };
            try attachUseTypePayload(allocator, artifact_key, &checked_types.store, &resolved_ref, checked_type_key, checked_ty);

            const id: ResolvedValueRefId = @enumFromInt(@as(u32, @intCast(records.items.len)));
            try records.append(allocator, .{
                .expr = checked_expr,
                .ref = resolved_ref,
                .checked_ty = checked_ty,
                .scope_depth = 0,
            });
            by_checked_expr[@intFromEnum(checked_expr)] = id;
        }

        return .{
            .records = try records.toOwnedSlice(allocator),
            .by_checked_expr = by_checked_expr,
        };
    }

    pub fn lookupIdByCheckedExpr(self: *const ResolvedValueRefTable, expr: CheckedExprId) ?ResolvedValueRefId {
        const raw = @intFromEnum(expr);
        if (raw >= self.by_checked_expr.len) return null;
        return self.by_checked_expr[raw];
    }

    pub fn appendTemplateRefSpan(
        self: *ResolvedValueRefTable,
        allocator: Allocator,
        refs: []const ResolvedValueRefId,
    ) Allocator.Error!ResolvedValueRefTableRef {
        const start: u32 = @intCast(self.template_refs.len);
        if (refs.len == 0) return .{ .start = start, .len = 0 };
        const old = self.template_refs;
        const next = try allocator.alloc(ResolvedValueRefId, old.len + refs.len);
        @memcpy(next[0..old.len], old);
        @memcpy(next[old.len..], refs);
        allocator.free(old);
        self.template_refs = next;
        return .{ .start = start, .len = @intCast(refs.len) };
    }

    pub fn deinit(self: *ResolvedValueRefTable, allocator: Allocator) void {
        allocator.free(self.template_refs);
        allocator.free(self.by_checked_expr);
        allocator.free(self.records);
        self.* = .{};
    }
};

fn classifyValueRef(
    allocator: Allocator,
    module: TypedCIR.Module,
    expr_idx: CIR.Expr.Idx,
    imports: []const PublishImportArtifact,
    templates: *const CheckedProcedureTemplateTable,
    hosted_procs: *const HostedProcTable,
    platform_required_declarations: *const PlatformRequiredDeclarationTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    top_level_values: *const TopLevelValueTable,
    checked_bodies: *const CheckedBodyStore,
) Allocator.Error!ResolvedValueRef {
    _ = templates;
    _ = allocator;
    const expr = module.expr(expr_idx);
    return switch (expr.data) {
        .e_lookup_local => |local| classifyLocalValueRef(
            module,
            local.pattern_idx,
            hosted_procs,
            top_level_values,
            checked_bodies,
        ),
        .e_lookup_external => |external| classifyImportedValueRef(
            module,
            external.module_idx,
            external.target_node_idx,
            imports,
        ),
        .e_lookup_required => |required| classifyRequiredValueRef(
            required.requires_idx.toU32(),
            platform_required_declarations,
            platform_required_bindings,
        ),
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: expression {d} is not a value reference",
                    .{@intFromEnum(expr_idx)},
                );
            }
            unreachable;
        },
    };
}

fn attachUseTypePayload(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    ref: *ResolvedValueRef,
    key: canonical.CanonicalTypeKey,
    checked_ty: CheckedTypeId,
) Allocator.Error!void {
    switch (ref.*) {
        .top_level_const => |*use| {
            const request = try constUseRequestPayload(allocator, artifact_key, checked_types, use.const_ref, key, checked_ty);
            use.requested_source_ty_template = request.key;
            use.requested_source_ty_payload = request.payload;
        },
        .imported_const => |*use| {
            const request = try constUseRequestPayload(allocator, artifact_key, checked_types, use.const_ref, key, checked_ty);
            use.requested_source_ty_template = request.key;
            use.requested_source_ty_payload = request.payload;
        },
        .platform_required_const => |*required| {
            if (required.const_use.requested_source_ty_payload == null) {
                checkedArtifactInvariant("platform-required const use missing relation-owned requested payload", .{});
            }
        },
        .top_level_proc => |*use| {
            use.source_fn_ty_template = key;
            use.source_fn_ty_payload = checked_ty;
        },
        .imported_proc => |*use| {
            use.source_fn_ty_template = key;
            use.source_fn_ty_payload = checked_ty;
        },
        .hosted_proc => |*use| {
            use.source_fn_ty_template = key;
            use.source_fn_ty_payload = checked_ty;
        },
        .platform_required_proc => |*required| {
            if (required.procedure.source_fn_ty_payload == null) {
                checkedArtifactInvariant("platform-required procedure use missing relation-owned requested payload", .{});
            }
        },
        .promoted_top_level_proc => |*use| {
            use.source_fn_ty_template = key;
            use.source_fn_ty_payload = checked_ty;
        },
        .local_param,
        .local_value,
        .local_mutable_version,
        .pattern_binder,
        .local_proc,
        .platform_required_declaration,
        => {},
    }
}

const ConstUseRequestPayload = struct {
    key: canonical.CanonicalTypeKey,
    payload: CheckedTypeId,
};

fn constUseRequestPayload(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    const_ref: ConstRef,
    use_key: canonical.CanonicalTypeKey,
    use_payload: CheckedTypeId,
) Allocator.Error!ConstUseRequestPayload {
    if (!std.mem.eql(u8, &const_ref.artifact.bytes, &artifact_key.bytes)) {
        return .{ .key = use_key, .payload = use_payload };
    }

    const scheme = checked_types.schemeForKey(const_ref.source_scheme) orelse {
        checkedArtifactInvariant("local const use referenced a missing producer source scheme", .{});
    };
    const concrete_producer = try checkedTypeIsConcreteConstProducerScheme(allocator, checked_types, scheme.root);
    if (!concrete_producer) {
        return .{ .key = use_key, .payload = use_payload };
    }

    return .{
        .key = checked_types.roots[@intFromEnum(scheme.root)].key,
        .payload = scheme.root,
    };
}

fn checkedTypeIsConcreteConstProducerScheme(
    allocator: Allocator,
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active.deinit();
    return try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, root, &active);
}

fn checkedTypeIsConcreteConstProducerSchemeInner(
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    if (active.contains(root)) return true;
    try active.put(root, {});
    defer _ = active.remove(root);

    const index = @intFromEnum(root);
    if (index >= checked_types.payloads.len) {
        checkedArtifactInvariant("const producer checked type id is out of range", .{});
    }
    return switch (checked_types.payloads[index]) {
        .pending => checkedArtifactInvariant("const producer checked type was pending", .{}),
        .flex,
        .rigid,
        => false,
        .empty_record,
        .empty_tag_union,
        => true,
        .alias => |alias| (try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, alias.backing, active)) and
            try checkedTypeSpanIsConcreteConstProducerScheme(checked_types, alias.args, active),
        .record => |record| (try checkedFieldTypesAreConcreteConstProducerScheme(checked_types, record.fields, active)) and
            try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, record.ext, active),
        .record_unbound => |fields| checkedFieldTypesAreConcreteConstProducerScheme(checked_types, fields, active),
        .tuple => |items| checkedTypeSpanIsConcreteConstProducerScheme(checked_types, items, active),
        .nominal => |nominal| blk: {
            if (!try checkedTypeSpanIsConcreteConstProducerScheme(checked_types, nominal.args, active)) break :blk false;
            if (nominal.builtin != null) break :blk true;
            break :blk try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, nominal.backing, active);
        },
        .function => |function| !function.needs_instantiation and
            (try checkedTypeSpanIsConcreteConstProducerScheme(checked_types, function.args, active)) and
            try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, function.ret, active),
        .tag_union => |tag_union| (try checkedTagsAreConcreteConstProducerScheme(checked_types, tag_union.tags, active)) and
            try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, tag_union.ext, active),
    };
}

fn checkedTypeSpanIsConcreteConstProducerScheme(
    checked_types: *const CheckedTypeStore,
    items: []const CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (items) |item| {
        if (!try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, item, active)) return false;
    }
    return true;
}

fn checkedFieldTypesAreConcreteConstProducerScheme(
    checked_types: *const CheckedTypeStore,
    fields: []const CheckedRecordField,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (fields) |field| {
        if (!try checkedTypeIsConcreteConstProducerSchemeInner(checked_types, field.ty, active)) return false;
    }
    return true;
}

fn checkedTagsAreConcreteConstProducerScheme(
    checked_types: *const CheckedTypeStore,
    tags: []const CheckedTag,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (tags) |tag| {
        if (!try checkedTypeSpanIsConcreteConstProducerScheme(checked_types, tag.args, active)) return false;
    }
    return true;
}

fn classifyLocalValueRef(
    module: TypedCIR.Module,
    pattern: CIR.Pattern.Idx,
    hosted_procs: *const HostedProcTable,
    top_level_values: *const TopLevelValueTable,
    checked_bodies: *const CheckedBodyStore,
) ResolvedValueRef {
    const checked_pattern = checked_bodies.patternIdForSource(pattern) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: local lookup pattern {d} has no checked pattern id",
                .{@intFromEnum(pattern)},
            );
        }
        unreachable;
    };

    if (topLevelDefByPattern(module, pattern)) |def_idx| {
        const entry = topLevelValueForPattern(top_level_values, checked_pattern) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: top-level pattern {d} has no top-level value entry",
                    .{@intFromEnum(pattern)},
                );
            }
            unreachable;
        };

        switch (entry.value) {
            .const_ref => |const_ref| return .{ .top_level_const = .{
                .const_ref = const_ref,
                .requested_source_ty_template = .{},
            } },
            .procedure_binding => |binding| {
                if (hostedProcForDef(hosted_procs, def_idx)) |hosted| {
                    return .{ .hosted_proc = .{
                        .binding = .{ .hosted = .{
                            .module_idx = hosted.module_idx,
                            .def = hosted.def_idx,
                            .proc = hosted.proc,
                            .template = hosted.template,
                        } },
                        .source_fn_ty_template = .{},
                    } };
                }
                return .{ .top_level_proc = .{
                    .binding = .{ .top_level = binding },
                    .source_fn_ty_template = .{},
                } };
            },
        }
    }

    const binder = checked_bodies.patternBinderForSource(pattern) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: local lookup pattern {d} has no checked pattern binder",
                .{@intFromEnum(pattern)},
            );
        }
        unreachable;
    };

    if (patternIsLambdaArg(module, pattern)) {
        return .{ .local_param = .{ .binder = binder } };
    }

    if (localStatementForPattern(module, pattern)) |statement| {
        switch (statement) {
            .s_var => return .{ .local_mutable_version = .{ .binder = binder } },
            .s_decl => |decl| {
                if (isLocalProcExpr(module, decl.expr)) {
                    return .{ .local_proc = .{ .binder = binder } };
                }
                return .{ .local_value = .{ .binder = binder } };
            },
            else => {},
        }
    }

    if (patternIsBinder(module, pattern)) {
        return .{ .pattern_binder = .{ .binder = binder } };
    }

    if (builtin.mode == .Debug) {
        std.debug.panic(
            "checked artifact invariant violated: local lookup pattern {d} has no classified binding",
            .{@intFromEnum(pattern)},
        );
    }
    unreachable;
}

fn classifyImportedValueRef(
    module: TypedCIR.Module,
    import_idx: CIR.Import.Idx,
    target_node_idx: u16,
    imports: []const PublishImportArtifact,
) ResolvedValueRef {
    const resolved_module_idx = module.resolvedImportModule(import_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: external lookup import {d} has no resolved module",
                .{@intFromEnum(import_idx)},
            );
        }
        unreachable;
    };
    const import_artifact = publishImportForModule(imports, resolved_module_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: external lookup import {d} resolved to module {d} without a published artifact key",
                .{ @intFromEnum(import_idx), resolved_module_idx },
            );
        }
        unreachable;
    };

    const target_def: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(target_node_idx)));
    if (importedProcedureBindingForDef(import_artifact.view, target_def)) |binding| {
        return .{ .imported_proc = .{
            .binding = .{ .imported = binding.binding },
            .source_fn_ty_template = .{},
        } };
    }

    if (importedConstTemplateForDef(import_artifact.view, target_def)) |const_template| {
        return .{ .imported_const = .{
            .const_ref = const_template.const_ref,
            .requested_source_ty_template = .{},
        } };
    }

    if (builtin.mode == .Debug) {
        std.debug.panic(
            "checked artifact invariant violated: external lookup target {d} in module {d} was not exported by the imported checked artifact",
            .{ target_node_idx, resolved_module_idx },
        );
    }
    unreachable;
}

fn importedProcedureBindingForDef(view: ImportedModuleView, def: CIR.Def.Idx) ?ImportedProcedureBindingView {
    for (view.exported_procedure_bindings.bindings) |binding| {
        if (binding.binding.def == def) return binding;
    }
    return null;
}

fn importedConstTemplateForDef(view: ImportedModuleView, def: CIR.Def.Idx) ?ImportedConstTemplateView {
    for (view.exported_const_templates.templates) |template| {
        if (template.def == def) return template;
    }
    return null;
}

fn collectPublishedExportDefs(
    allocator: Allocator,
    module: TypedCIR.Module,
) Allocator.Error![]CIR.Def.Idx {
    const module_env = module.moduleEnvConst();
    var defs = std.ArrayList(CIR.Def.Idx).empty;
    errdefer defs.deinit(allocator);

    const node_count = module.nodeCount();
    const seen = try allocator.alloc(bool, node_count);
    defer allocator.free(seen);
    @memset(seen, false);

    for (module_env.store.sliceDefs(module_env.exports)) |def_idx| {
        try appendPublishedExportDef(allocator, &defs, seen, def_idx);
    }

    var exposed_iter = module_env.common.exposed_items.iterator();
    while (exposed_iter.next()) |entry| {
        if (entry.node_idx == 0) continue;

        const raw_node_idx: u32 = entry.node_idx;
        if (raw_node_idx >= node_count) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: exposed item {s} points at out-of-range node {d}",
                    .{ module_env.getIdent(@bitCast(entry.ident_idx)), raw_node_idx },
                );
            }
            unreachable;
        }

        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (module.nodeTag(node_idx) != .def) continue;
        try appendPublishedExportDef(allocator, &defs, seen, @enumFromInt(raw_node_idx));
    }

    return try defs.toOwnedSlice(allocator);
}

fn appendPublishedExportDef(
    allocator: Allocator,
    defs: *std.ArrayList(CIR.Def.Idx),
    seen: []bool,
    def_idx: CIR.Def.Idx,
) Allocator.Error!void {
    const raw = @intFromEnum(def_idx);
    if (raw >= seen.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: published export def {d} is outside the module node store",
                .{raw},
            );
        }
        unreachable;
    }
    if (seen[raw]) return;
    seen[raw] = true;
    try defs.append(allocator, def_idx);
}

fn publishImportForModule(imports: []const PublishImportArtifact, module_idx: u32) ?PublishImportArtifact {
    for (imports) |import_artifact| {
        if (import_artifact.module_idx == module_idx) return import_artifact;
    }
    return null;
}

fn classifyRequiredValueRef(
    requires_idx: u32,
    platform_required_declarations: *const PlatformRequiredDeclarationTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
) ResolvedValueRef {
    const binding = platformBindingForRequiredIndex(platform_required_bindings, requires_idx) orelse {
        const declaration = platform_required_declarations.lookupByRequiredIndex(requires_idx) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: required lookup {d} has no platform declaration",
                    .{requires_idx},
                );
            }
            unreachable;
        };
        return .{ .platform_required_declaration = declaration.id };
    };

    return switch (binding.value_use) {
        .const_value => |const_use| .{ .platform_required_const = .{
            .binding = binding.id,
            .const_use = const_use.const_use,
        } },
        .procedure_value => |proc_use| .{ .platform_required_proc = .{
            .binding = binding.id,
            .procedure = proc_use.procedure,
        } },
    };
}

fn topLevelDefByPattern(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) ?CIR.Def.Idx {
    for (module.allDefs()) |def_idx| {
        if (module.def(def_idx).pattern.idx == pattern) return def_idx;
    }
    return null;
}

fn checkedPatternSourceTypeVar(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) Var {
    if (topLevelDefByPattern(module, pattern)) |def_idx| {
        return module.defType(def_idx);
    }
    return module.patternType(pattern);
}

fn topLevelValueForPattern(table: *const TopLevelValueTable, pattern: CheckedPatternId) ?TopLevelValueEntry {
    for (table.entries) |entry| {
        if (entry.pattern == pattern) return entry;
    }
    return null;
}

fn hostedProcForDef(table: *const HostedProcTable, def_idx: CIR.Def.Idx) ?HostedProc {
    for (table.procs) |proc| {
        if (proc.def_idx == def_idx) return proc;
    }
    return null;
}

fn platformBindingForRequiredIndex(table: *const PlatformRequiredBindingTable, requires_idx: u32) ?PlatformRequiredBinding {
    return table.lookupByRequiredIndex(requires_idx);
}

fn localStatementForPattern(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) ?CIR.Statement {
    const statements = module.moduleEnvConst().store.sliceStatements(module.moduleEnvConst().all_statements);
    for (statements) |statement_idx| {
        const statement = module.getStatement(statement_idx);
        switch (statement) {
            .s_decl => |decl| if (decl.pattern == pattern) return statement,
            .s_var => |var_| if (var_.pattern_idx == pattern) return statement,
            else => {},
        }
    }
    return null;
}

fn patternIsLambdaArg(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) bool {
    var node_idx: u32 = 0;
    while (node_idx < module.nodeCount()) : (node_idx += 1) {
        if (module.nodeTag(@enumFromInt(node_idx)) != .expr_lambda) continue;
        const expr = module.expr(@enumFromInt(node_idx));
        const lambda = switch (expr.data) {
            .e_lambda => |lambda| lambda,
            else => unreachable,
        };
        for (module.slicePatterns(lambda.args)) |arg| {
            if (arg == pattern) return true;
        }
    }
    return false;
}

fn patternIsBinder(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) bool {
    var node_idx: u32 = 0;
    while (node_idx < module.nodeCount()) : (node_idx += 1) {
        const tag = module.nodeTag(@enumFromInt(node_idx));
        switch (tag) {
            .pattern_identifier,
            .pattern_as,
            .pattern_applied_tag,
            .pattern_nominal,
            .pattern_nominal_external,
            .pattern_record_destructure,
            .pattern_list,
            .pattern_tuple,
            => {},
            else => continue,
        }
        if (node_idx == @intFromEnum(pattern)) return true;
    }
    return false;
}

fn isLocalProcExpr(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) bool {
    const expr = module.expr(expr_idx);
    return switch (expr.data) {
        .e_lambda, .e_closure => true,
        else => false,
    };
}

fn sealCheckedProcedureTemplateRefs(
    allocator: Allocator,
    checked_bodies: *const CheckedBodyStore,
    entry_wrappers: *const EntryWrapperTable,
    templates: *CheckedProcedureTemplateTable,
    static_dispatch_plans: *static_dispatch.StaticDispatchPlanTable,
    resolved_value_refs: *ResolvedValueRefTable,
) Allocator.Error!void {
    var collector = CheckedTemplateRefCollector.init(allocator, checked_bodies, static_dispatch_plans);
    defer collector.deinit();

    for (templates.templates) |*template| {
        collector.clear();

        switch (template.body) {
            .checked_body => |body_id| {
                const body = checked_bodies.body(body_id);
                try collector.collectExpr(body.root_expr);
            },
            .entry_wrapper => |wrapper_id| {
                const wrapper = entry_wrappers.get(wrapper_id);
                try collector.collectExpr(wrapper.body_expr);
            },
            .intrinsic_wrapper => {},
            .promoted_callable_wrapper => {},
        }

        template.resolved_value_refs = try resolved_value_refs.appendTemplateRefSpan(allocator, collector.value_refs.items);
        const dispatch_span = try static_dispatch_plans.appendTemplateRefSpan(allocator, collector.dispatch_refs.items);
        template.static_dispatch_plans = .{
            .start = dispatch_span.start,
            .len = dispatch_span.len,
        };
    }
}

fn sealConstEvalTemplatesForRoots(
    const_templates: *ConstTemplateTable,
    compile_time_roots: *const CompileTimeRootTable,
    checked_const_bodies: *const CheckedConstBodyTable,
    entry_wrappers: *const EntryWrapperTable,
    checked_procedure_templates: *const CheckedProcedureTemplateTable,
    top_level_values: *const TopLevelValueTable,
) void {
    for (compile_time_roots.roots) |root| {
        if (root.kind != .constant) continue;
        const pattern = root.pattern orelse checkedArtifactInvariant("constant root has no top-level pattern", .{});
        const top_level = top_level_values.lookupByPattern(pattern) orelse {
            checkedArtifactInvariant("constant root has no top-level value entry", .{});
        };
        const const_ref = switch (top_level.value) {
            .const_ref => |ref| ref,
            .procedure_binding => checkedArtifactInvariant("constant root top-level value is not a ConstRef", .{}),
        };
        const body = checked_const_bodies.bodyForRoot(root.id) orelse {
            checkedArtifactInvariant("constant root has no checked const body", .{});
        };
        const wrapper = entryWrapperForRoot(entry_wrappers, root.id);
        const template = checked_procedure_templates.get(wrapper.template.template);
        const_templates.fillEval(const_ref, .{
            .body = body,
            .entry_template = wrapper.template,
            .source_scheme = const_ref.source_scheme,
            .resolved_value_refs = template.resolved_value_refs,
            .static_dispatch_plans = template.static_dispatch_plans,
            .nested_proc_sites = template.nested_proc_sites,
        });
    }
}

const CheckedTemplateRefCollector = struct {
    allocator: Allocator,
    checked_bodies: *const CheckedBodyStore,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    value_refs: std.ArrayList(ResolvedValueRefId),
    dispatch_refs: std.ArrayList(static_dispatch.StaticDispatchPlanId),
    visited_exprs: std.AutoHashMap(CheckedExprId, void),
    visited_patterns: std.AutoHashMap(CheckedPatternId, void),
    visited_statements: std.AutoHashMap(CheckedStatementId, void),

    fn init(
        allocator: Allocator,
        checked_bodies: *const CheckedBodyStore,
        static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    ) CheckedTemplateRefCollector {
        return .{
            .allocator = allocator,
            .checked_bodies = checked_bodies,
            .static_dispatch_plans = static_dispatch_plans,
            .value_refs = .empty,
            .dispatch_refs = .empty,
            .visited_exprs = std.AutoHashMap(CheckedExprId, void).init(allocator),
            .visited_patterns = std.AutoHashMap(CheckedPatternId, void).init(allocator),
            .visited_statements = std.AutoHashMap(CheckedStatementId, void).init(allocator),
        };
    }

    fn deinit(self: *CheckedTemplateRefCollector) void {
        self.visited_statements.deinit();
        self.visited_patterns.deinit();
        self.visited_exprs.deinit();
        self.dispatch_refs.deinit(self.allocator);
        self.value_refs.deinit(self.allocator);
    }

    fn clear(self: *CheckedTemplateRefCollector) void {
        self.value_refs.clearRetainingCapacity();
        self.dispatch_refs.clearRetainingCapacity();
        self.visited_exprs.clearRetainingCapacity();
        self.visited_patterns.clearRetainingCapacity();
        self.visited_statements.clearRetainingCapacity();
    }

    fn collectExpr(self: *CheckedTemplateRefCollector, expr_id: CheckedExprId) Allocator.Error!void {
        const entry = try self.visited_exprs.getOrPut(expr_id);
        if (entry.found_existing) return;

        const expr = self.checked_bodies.expr(expr_id);
        switch (expr.data) {
            .lookup_local => |lookup| {
                if (lookup.resolved) |ref_id| try self.value_refs.append(self.allocator, ref_id);
            },
            .lookup_external => |ref_id| {
                if (ref_id) |id| try self.value_refs.append(self.allocator, id);
            },
            .lookup_required => |ref_id| {
                if (ref_id) |id| try self.value_refs.append(self.allocator, id);
            },
            .dispatch_call,
            .method_eq,
            .type_dispatch_call,
            => |plan_id| {
                const id = plan_id orelse checkedArtifactInvariant("checked dispatch expression reached template closure collection without a static-dispatch plan", .{});
                try self.dispatch_refs.append(self.allocator, id);
                try self.collectStaticDispatchPlanArgs(id);
            },
            .str,
            .list,
            .tuple,
            => |items| {
                for (items) |item| try self.collectExpr(item);
            },
            .match_ => |match| {
                try self.collectExpr(match.cond);
                for (match.branches) |branch| {
                    for (branch.patterns) |branch_pattern| try self.collectPattern(branch_pattern.pattern);
                    if (branch.guard) |guard| try self.collectExpr(guard);
                    try self.collectExpr(branch.value);
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch| {
                    try self.collectExpr(branch.cond);
                    try self.collectExpr(branch.body);
                }
                try self.collectExpr(if_.final_else);
            },
            .call => |call| {
                try self.collectExpr(call.func);
                for (call.args) |arg| try self.collectExpr(arg);
            },
            .record => |record| {
                if (record.ext) |ext| try self.collectExpr(ext);
                for (record.fields) |field| try self.collectExpr(field.value);
            },
            .block => |block| {
                for (block.statements) |statement| try self.collectStatement(statement);
                try self.collectExpr(block.final_expr);
            },
            .tag => |tag| {
                for (tag.args) |arg| try self.collectExpr(arg);
            },
            .nominal => |nominal| try self.collectExpr(nominal.backing_expr),
            .closure => |closure| try self.collectExpr(closure.lambda),
            .lambda => |lambda| {
                for (lambda.args) |arg| try self.collectPattern(arg);
                try self.collectExpr(lambda.body);
            },
            .binop => |binop| {
                try self.collectExpr(binop.lhs);
                try self.collectExpr(binop.rhs);
            },
            .unary_minus => |child| try self.collectExpr(child),
            .unary_not => |child| try self.collectExpr(child),
            .field_access => |field| try self.collectExpr(field.receiver),
            .structural_eq => |eq| {
                try self.collectExpr(eq.lhs);
                try self.collectExpr(eq.rhs);
            },
            .tuple_access => |access| try self.collectExpr(access.tuple),
            .dbg => |child| try self.collectExpr(child),
            .expect => |child| try self.collectExpr(child),
            .return_ => |ret| {
                try self.collectExpr(ret.expr);
                // `ret.lambda` is the enclosing lambda context for early-return
                // lowering, not an owned child expression.
            },
            .for_ => |for_| {
                try self.collectPattern(for_.pattern);
                try self.collectExpr(for_.expr);
                try self.collectExpr(for_.body);
            },
            .hosted_lambda => |hosted| {
                for (hosted.args) |arg| try self.collectPattern(arg);
            },
            .run_low_level => |run| {
                for (run.args) |arg| try self.collectExpr(arg);
            },
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .typed_int,
            .typed_frac,
            .str_segment,
            .bytes_literal,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .pending,
            => {},
        }
    }

    fn collectStaticDispatchPlanArgs(
        self: *CheckedTemplateRefCollector,
        plan_id: static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!void {
        const raw = @intFromEnum(plan_id);
        if (raw >= self.static_dispatch_plans.plans.len) {
            checkedArtifactInvariant("checked template static-dispatch plan id was outside the plan table", .{});
        }
        const plan = self.static_dispatch_plans.plans[raw];
        for (plan.args) |arg| try self.collectExpr(arg);
    }

    fn collectPattern(self: *CheckedTemplateRefCollector, pattern_id: CheckedPatternId) Allocator.Error!void {
        const entry = try self.visited_patterns.getOrPut(pattern_id);
        if (entry.found_existing) return;

        const pattern = self.checked_bodies.patterns[@intFromEnum(pattern_id)];
        switch (pattern.data) {
            .as => |as| try self.collectPattern(as.pattern),
            .applied_tag => |tag| {
                for (tag.args) |arg| try self.collectPattern(arg);
            },
            .nominal => |nominal| try self.collectPattern(nominal.backing_pattern),
            .record_destructure => |destructs| {
                for (destructs) |destruct| switch (destruct.kind) {
                    .required => |child| try self.collectPattern(child),
                    .sub_pattern => |child| try self.collectPattern(child),
                    .rest => |child| try self.collectPattern(child),
                };
            },
            .list => |list| {
                for (list.patterns) |child| try self.collectPattern(child);
                if (list.rest) |rest| {
                    if (rest.pattern) |child| try self.collectPattern(child);
                }
            },
            .tuple => |items| {
                for (items) |child| try self.collectPattern(child);
            },
            .pending,
            .assign,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn collectStatement(self: *CheckedTemplateRefCollector, statement_id: CheckedStatementId) Allocator.Error!void {
        const entry = try self.visited_statements.getOrPut(statement_id);
        if (entry.found_existing) return;

        const statement = self.checked_bodies.statements[@intFromEnum(statement_id)];
        switch (statement.data) {
            .decl => |decl| {
                try self.collectPattern(decl.pattern);
                try self.collectExpr(decl.expr);
            },
            .var_ => |var_| {
                try self.collectPattern(var_.pattern);
                try self.collectExpr(var_.expr);
            },
            .reassign => |reassign| {
                try self.collectPattern(reassign.pattern);
                try self.collectExpr(reassign.expr);
            },
            .dbg => |child| try self.collectExpr(child),
            .expr => |child| try self.collectExpr(child),
            .expect => |child| try self.collectExpr(child),
            .return_ => |ret| {
                try self.collectExpr(ret.expr);
                // `ret.lambda` is the enclosing lambda context for early-return
                // lowering, not an owned child expression.
            },
            .for_ => |for_| {
                try self.collectPattern(for_.pattern);
                try self.collectExpr(for_.expr);
                try self.collectExpr(for_.body);
            },
            .while_ => |while_| {
                try self.collectExpr(while_.cond);
                try self.collectExpr(while_.body);
            },
            .pending,
            .crash,
            .break_,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            => {},
        }
    }
};

/// Public `TopLevelUseSummaryRef` declaration.
pub const TopLevelUseSummaryRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

/// Public `NestedProcSiteTableRef` declaration.
pub const NestedProcSiteTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

/// Public `NestedProcKind` declaration.
pub const NestedProcKind = enum {
    local_function,
    closure,
    desugared_closure,
};

/// Public `NestedProcPathComponent` declaration.
pub const NestedProcPathComponent = union(enum) {
    expr: CheckedExprId,
    pattern: CheckedPatternId,
    statement: CheckedStatementId,
    branch: u32,
    desugar: u32,
};

/// Public `NestedProcSite` declaration.
pub const NestedProcSite = struct {
    site: canonical.NestedProcSiteId,
    owner_template: canonical.ProcedureTemplateRef,
    site_path: []const NestedProcPathComponent,
    kind: NestedProcKind,
    checked_expr: ?CheckedExprId,
    checked_pattern: ?CheckedPatternId,
};

/// Public `NestedProcSiteTable` declaration.
pub const NestedProcSiteTable = struct {
    sites: []NestedProcSite = &.{},
    template_refs: []canonical.NestedProcSiteId = &.{},

    pub fn fromTemplates(
        allocator: Allocator,
        checked_bodies: *const CheckedBodyStore,
        static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
        entry_wrappers: *const EntryWrapperTable,
        templates: *CheckedProcedureTemplateTable,
    ) Allocator.Error!NestedProcSiteTable {
        var builder = NestedProcSiteBuilder.init(allocator, checked_bodies, static_dispatch_plans);
        defer builder.deinitScratch();
        errdefer builder.deinitAll();

        for (templates.templates) |*template| {
            const start: u32 = @intCast(builder.template_refs.items.len);
            switch (template.body) {
                .checked_body => |body_id| try builder.scanCheckedBody(body_id, template),
                .entry_wrapper => |wrapper_id| try builder.scanEntryWrapper(entry_wrappers.get(wrapper_id), template),
                .intrinsic_wrapper,
                .promoted_callable_wrapper,
                => {},
            }
            template.nested_proc_sites = .{
                .start = start,
                .len = @intCast(builder.template_refs.items.len - start),
            };
        }

        const sites = try builder.sites.toOwnedSlice(allocator);
        errdefer {
            for (sites) |site| allocator.free(site.site_path);
            allocator.free(sites);
        }

        return .{
            .sites = sites,
            .template_refs = try builder.template_refs.toOwnedSlice(allocator),
        };
    }

    pub fn deinit(self: *NestedProcSiteTable, allocator: Allocator) void {
        for (self.sites) |site| allocator.free(site.site_path);
        allocator.free(self.sites);
        allocator.free(self.template_refs);
        self.* = .{};
    }
};

/// Public `ProcTarget` declaration.
pub const ProcTarget = union(enum) {
    roc,
    hosted,
    intrinsic,
    entry,
    comptime_only,
    promoted_callable,
};

/// Public `CheckedProcedureTemplate` declaration.
pub const CheckedProcedureTemplate = struct {
    proc_base: canonical.ProcBaseKeyRef,
    template_id: canonical.CheckedProcedureTemplateId,
    body: CheckedProcedureBody,
    checked_fn_scheme: canonical.CanonicalTypeSchemeKey,
    checked_fn_root: CheckedTypeId,
    static_dispatch_plans: StaticDispatchPlanTableRef,
    resolved_value_refs: ResolvedValueRefTableRef,
    top_level_value_uses: TopLevelUseSummaryRef,
    nested_proc_sites: NestedProcSiteTableRef,
    target: ProcTarget,
};

/// Public `CheckedProcedureTemplateTable` declaration.
pub const CheckedProcedureTemplateTable = struct {
    templates: []CheckedProcedureTemplate = &.{},
    by_def: []?canonical.ProcedureTemplateRef = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        owner_artifact: canonical.ArtifactRef,
        checked_type_publication: *const CheckedTypePublication,
        checked_bodies: *CheckedBodyStore,
        intrinsic_wrappers: *IntrinsicWrapperTable,
    ) Allocator.Error!CheckedProcedureTemplateTable {
        var templates = std.ArrayList(CheckedProcedureTemplate).empty;
        errdefer templates.deinit(allocator);

        const by_def = try allocator.alloc(?canonical.ProcedureTemplateRef, module.nodeCount());
        errdefer allocator.free(by_def);
        @memset(by_def, null);

        const module_name = try names.internModuleIdent(module.identStoreConst(), module.qualifiedModuleIdent());

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            if (!topLevelExprIsAlreadyProcedure(def.expr.data)) continue;

            const export_name = if (def.patternName()) |name|
                try names.internExportIdent(module.identStoreConst(), name)
            else
                null;
            const intrinsic = intrinsicForProcedureDef(module, def_idx);
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = export_name,
                .kind = if (intrinsic != null) .intrinsic_wrapper else if (isHostedProcedureExpr(def.expr.data)) .hosted_wrapper else .checked_source,
                .ordinal = @intFromEnum(def_idx),
                .source_def_idx = @intFromEnum(def_idx),
            });
            const template_id: canonical.CheckedProcedureTemplateId = @enumFromInt(@as(u32, @intCast(templates.items.len)));
            const template_ref = canonical.ProcedureTemplateRef{
                .artifact = owner_artifact,
                .proc_base = proc_base,
                .template = template_id,
            };
            by_def[@intFromEnum(def_idx)] = template_ref;
            const checked_fn_root = checked_type_publication.rootForSourceVar(module, module.defType(def_idx)) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: checked procedure function root was not published", .{});
                }
                unreachable;
            };
            const checked_fn_scheme = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                module.defType(def_idx),
            );
            const body: CheckedProcedureBody = if (intrinsic) |intrinsic_id| blk: {
                const wrapper_id = try intrinsic_wrappers.append(allocator, template_ref, checked_fn_root, intrinsic_id);
                break :blk .{ .intrinsic_wrapper = wrapper_id };
            } else blk: {
                const root_expr = checked_bodies.exprIdForSource(def.expr.idx) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("checked artifact invariant violated: checked procedure body root expression was not published", .{});
                    }
                    unreachable;
                };
                break :blk .{ .checked_body = try checked_bodies.appendBody(allocator, root_expr, template_ref) };
            };

            try templates.append(allocator, .{
                .proc_base = proc_base,
                .template_id = template_id,
                .body = body,
                .checked_fn_scheme = checked_fn_scheme,
                .checked_fn_root = checked_fn_root,
                .static_dispatch_plans = .{},
                .resolved_value_refs = .{},
                .top_level_value_uses = .{},
                .nested_proc_sites = .{},
                .target = if (intrinsic != null)
                    .intrinsic
                else if (isHostedProcedureExpr(def.expr.data))
                    .hosted
                else
                    .roc,
            });
        }

        return .{
            .templates = try templates.toOwnedSlice(allocator),
            .by_def = by_def,
        };
    }

    pub fn lookupByDef(self: *const CheckedProcedureTemplateTable, def_idx: CIR.Def.Idx) ?canonical.ProcedureTemplateRef {
        const raw = @intFromEnum(def_idx);
        if (raw >= self.by_def.len) return null;
        return self.by_def[raw];
    }

    pub fn appendEntryWrappersForRoots(
        self: *CheckedProcedureTemplateTable,
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        owner_artifact: canonical.ArtifactRef,
        checked_types: *CheckedTypeStore,
        entry_wrappers: *EntryWrapperTable,
        compile_time_roots: *const CompileTimeRootTable,
    ) Allocator.Error!void {
        const module_name = try names.internModuleIdent(module.identStoreConst(), module.qualifiedModuleIdent());

        for (compile_time_roots.roots) |root| {
            const checked_fn_root = try checked_types.appendSyntheticFunctionRoot(
                allocator,
                .pure,
                &.{},
                root.checked_type,
            );
            const checked_fn_scheme = syntheticSchemeKeyForType(checked_types.roots[@intFromEnum(checked_fn_root)].key);
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = null,
                .kind = .entry_wrapper,
                .ordinal = @intFromEnum(root.id),
                .source_def_idx = null,
            });
            const template_id: canonical.CheckedProcedureTemplateId = @enumFromInt(@as(u32, @intCast(self.templates.len)));
            const template_ref = canonical.ProcedureTemplateRef{
                .artifact = owner_artifact,
                .proc_base = proc_base,
                .template = template_id,
            };
            const wrapper_id = try entry_wrappers.append(allocator, root.id, template_ref, checked_fn_root, root.expr);
            try self.appendTemplate(allocator, .{
                .proc_base = proc_base,
                .template_id = template_id,
                .body = .{ .entry_wrapper = wrapper_id },
                .checked_fn_scheme = checked_fn_scheme,
                .checked_fn_root = checked_fn_root,
                .static_dispatch_plans = .{},
                .resolved_value_refs = .{},
                .top_level_value_uses = .{},
                .nested_proc_sites = .{},
                .target = switch (root.kind) {
                    .expect => .entry,
                    .constant, .callable_binding => .comptime_only,
                },
            });
        }
    }

    fn appendTemplate(
        self: *CheckedProcedureTemplateTable,
        allocator: Allocator,
        template: CheckedProcedureTemplate,
    ) Allocator.Error!void {
        const old = self.templates;
        const next = try allocator.alloc(CheckedProcedureTemplate, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = template;
        allocator.free(old);
        self.templates = next;
    }

    pub fn get(self: *const CheckedProcedureTemplateTable, id: canonical.CheckedProcedureTemplateId) CheckedProcedureTemplate {
        return self.templates[@intFromEnum(id)];
    }

    pub fn view(self: *const CheckedProcedureTemplateTable) CheckedProcedureTemplateTableView {
        return .{ .templates = self.templates };
    }

    pub fn asLookup(self: *const CheckedProcedureTemplateTable, module_idx: u32) static_dispatch.ProcedureTemplateLookup {
        return .{
            .module_idx = module_idx,
            .by_def = self.by_def,
        };
    }

    pub fn deinit(self: *CheckedProcedureTemplateTable, allocator: Allocator) void {
        allocator.free(self.by_def);
        allocator.free(self.templates);
        self.* = .{};
    }
};

/// Public `CheckedProcedureTemplateTableView` declaration.
pub const CheckedProcedureTemplateTableView = struct {
    templates: []const CheckedProcedureTemplate = &.{},
};

const NestedProcSiteBuilder = struct {
    allocator: Allocator,
    checked_bodies: *const CheckedBodyStore,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    sites: std.ArrayList(NestedProcSite),
    template_refs: std.ArrayList(canonical.NestedProcSiteId),
    path: std.ArrayList(NestedProcPathComponent),

    fn init(
        allocator: Allocator,
        checked_bodies: *const CheckedBodyStore,
        static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    ) NestedProcSiteBuilder {
        return .{
            .allocator = allocator,
            .checked_bodies = checked_bodies,
            .static_dispatch_plans = static_dispatch_plans,
            .sites = .empty,
            .template_refs = .empty,
            .path = .empty,
        };
    }

    fn deinitScratch(self: *NestedProcSiteBuilder) void {
        self.path.deinit(self.allocator);
    }

    fn deinitAll(self: *NestedProcSiteBuilder) void {
        for (self.sites.items) |site| self.allocator.free(site.site_path);
        self.sites.deinit(self.allocator);
        self.template_refs.deinit(self.allocator);
        self.path.deinit(self.allocator);
        self.* = NestedProcSiteBuilder.init(self.allocator, self.checked_bodies, self.static_dispatch_plans);
    }

    fn scanCheckedBody(
        self: *NestedProcSiteBuilder,
        body_id: CheckedBodyId,
        _: *const CheckedProcedureTemplate,
    ) Allocator.Error!void {
        const body = self.checked_bodies.body(body_id);
        self.path.clearRetainingCapacity();
        try self.scanExpr(body.root_expr, body.owner_template, true);
    }

    fn scanEntryWrapper(
        self: *NestedProcSiteBuilder,
        wrapper: EntryWrapper,
        _: *const CheckedProcedureTemplate,
    ) Allocator.Error!void {
        self.path.clearRetainingCapacity();
        try self.scanExpr(wrapper.body_expr, wrapper.template, false);
    }

    fn addSite(
        self: *NestedProcSiteBuilder,
        owner: canonical.ProcedureTemplateRef,
        kind: NestedProcKind,
        checked_expr: ?CheckedExprId,
        checked_pattern: ?CheckedPatternId,
    ) Allocator.Error!void {
        const site: canonical.NestedProcSiteId = @enumFromInt(@as(u32, @intCast(self.sites.items.len)));
        const copied_path = try self.allocator.dupe(NestedProcPathComponent, self.path.items);
        errdefer self.allocator.free(copied_path);

        try self.sites.append(self.allocator, .{
            .site = site,
            .owner_template = owner,
            .site_path = copied_path,
            .kind = kind,
            .checked_expr = checked_expr,
            .checked_pattern = checked_pattern,
        });
        try self.template_refs.append(self.allocator, site);
    }

    fn scanExpr(
        self: *NestedProcSiteBuilder,
        expr_id: CheckedExprId,
        owner: canonical.ProcedureTemplateRef,
        suppress_current_site: bool,
    ) Allocator.Error!void {
        try self.path.append(self.allocator, .{ .expr = expr_id });
        defer self.path.items.len -= 1;

        const expr = self.checked_bodies.expr(expr_id);
        switch (expr.data) {
            .closure => |closure| {
                if (!suppress_current_site) {
                    try self.addSite(owner, .closure, expr_id, null);
                }
                try self.scanExpr(closure.lambda, owner, true);
            },
            .lambda => |lambda| {
                if (!suppress_current_site) {
                    try self.addSite(owner, .local_function, expr_id, null);
                }
                for (lambda.args) |arg| try self.scanPattern(arg, owner);
                try self.scanExpr(lambda.body, owner, false);
            },
            .str,
            .list,
            .tuple,
            => |items| {
                for (items) |item| try self.scanExpr(item, owner, false);
            },
            .match_ => |match| {
                try self.scanExpr(match.cond, owner, false);
                for (match.branches, 0..) |branch, i| {
                    try self.path.append(self.allocator, .{ .branch = @intCast(i) });
                    for (branch.patterns) |branch_pattern| try self.scanPattern(branch_pattern.pattern, owner);
                    if (branch.guard) |guard| try self.scanExpr(guard, owner, false);
                    try self.scanExpr(branch.value, owner, false);
                    self.path.items.len -= 1;
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch| {
                    try self.scanExpr(branch.cond, owner, false);
                    try self.scanExpr(branch.body, owner, false);
                }
                try self.scanExpr(if_.final_else, owner, false);
            },
            .call => |call| {
                try self.scanExpr(call.func, owner, false);
                for (call.args) |arg| try self.scanExpr(arg, owner, false);
            },
            .record => |record| {
                if (record.ext) |ext| try self.scanExpr(ext, owner, false);
                for (record.fields) |field| try self.scanExpr(field.value, owner, false);
            },
            .block => |block| {
                for (block.statements) |statement| try self.scanStatement(statement, owner);
                try self.scanExpr(block.final_expr, owner, false);
            },
            .tag => |tag| {
                for (tag.args) |arg| try self.scanExpr(arg, owner, false);
            },
            .nominal => |nominal| try self.scanExpr(nominal.backing_expr, owner, false),
            .binop => |binop| {
                try self.scanExpr(binop.lhs, owner, false);
                try self.scanExpr(binop.rhs, owner, false);
            },
            .unary_minus => |child| try self.scanExpr(child, owner, false),
            .unary_not => |child| try self.scanExpr(child, owner, false),
            .dbg => |child| try self.scanExpr(child, owner, false),
            .expect => |child| try self.scanExpr(child, owner, false),
            .return_ => |ret| {
                try self.scanExpr(ret.expr, owner, false);
                // `ret.lambda` is the enclosing lambda context for early-return
                // lowering, not an owned child expression.
            },
            .field_access => |field| try self.scanExpr(field.receiver, owner, false),
            .dispatch_call,
            .method_eq,
            .type_dispatch_call,
            => |plan_id| try self.scanStaticDispatchPlanArgs(plan_id orelse checkedArtifactInvariant("checked dispatch expression reached nested procedure site collection without a static-dispatch plan", .{}), owner),
            .structural_eq => |eq| {
                try self.scanExpr(eq.lhs, owner, false);
                try self.scanExpr(eq.rhs, owner, false);
            },
            .tuple_access => |access| try self.scanExpr(access.tuple, owner, false),
            .for_ => |for_| {
                try self.scanPattern(for_.pattern, owner);
                try self.scanExpr(for_.expr, owner, false);
                try self.scanExpr(for_.body, owner, false);
            },
            .hosted_lambda => |hosted| {
                if (!suppress_current_site) {
                    try self.addSite(owner, .local_function, expr_id, null);
                }
                for (hosted.args) |arg| try self.scanPattern(arg, owner);
            },
            .run_low_level => |run| {
                for (run.args) |arg| try self.scanExpr(arg, owner, false);
            },
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .typed_int,
            .typed_frac,
            .str_segment,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .pending,
            => {},
        }
    }

    fn scanStaticDispatchPlanArgs(
        self: *NestedProcSiteBuilder,
        plan_id: static_dispatch.StaticDispatchPlanId,
        owner: canonical.ProcedureTemplateRef,
    ) Allocator.Error!void {
        const raw = @intFromEnum(plan_id);
        if (raw >= self.static_dispatch_plans.plans.len) {
            checkedArtifactInvariant("checked template static-dispatch plan id was outside the plan table", .{});
        }
        const plan = self.static_dispatch_plans.plans[raw];
        for (plan.args) |arg| try self.scanExpr(arg, owner, false);
    }

    fn scanPattern(
        self: *NestedProcSiteBuilder,
        pattern_id: CheckedPatternId,
        owner: canonical.ProcedureTemplateRef,
    ) Allocator.Error!void {
        try self.path.append(self.allocator, .{ .pattern = pattern_id });
        defer self.path.items.len -= 1;

        const pattern = self.checked_bodies.patterns[@intFromEnum(pattern_id)];
        switch (pattern.data) {
            .as => |as| try self.scanPattern(as.pattern, owner),
            .applied_tag => |tag| {
                for (tag.args) |arg| try self.scanPattern(arg, owner);
            },
            .nominal => |nominal| try self.scanPattern(nominal.backing_pattern, owner),
            .record_destructure => |destructs| {
                for (destructs) |destruct| switch (destruct.kind) {
                    .required => |child| try self.scanPattern(child, owner),
                    .sub_pattern => |child| try self.scanPattern(child, owner),
                    .rest => |child| try self.scanPattern(child, owner),
                };
            },
            .list => |list| {
                for (list.patterns) |child| try self.scanPattern(child, owner);
                if (list.rest) |rest| {
                    if (rest.pattern) |child| try self.scanPattern(child, owner);
                }
            },
            .tuple => |items| {
                for (items) |child| try self.scanPattern(child, owner);
            },
            .pending,
            .assign,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn scanStatement(
        self: *NestedProcSiteBuilder,
        statement_id: CheckedStatementId,
        owner: canonical.ProcedureTemplateRef,
    ) Allocator.Error!void {
        try self.path.append(self.allocator, .{ .statement = statement_id });
        defer self.path.items.len -= 1;

        const statement = self.checked_bodies.statements[@intFromEnum(statement_id)];
        switch (statement.data) {
            .decl => |decl| {
                try self.scanPattern(decl.pattern, owner);
                try self.scanExpr(decl.expr, owner, false);
            },
            .var_ => |var_| {
                try self.scanPattern(var_.pattern, owner);
                try self.scanExpr(var_.expr, owner, false);
            },
            .reassign => |reassign| {
                try self.scanPattern(reassign.pattern, owner);
                try self.scanExpr(reassign.expr, owner, false);
            },
            .dbg => |child| try self.scanExpr(child, owner, false),
            .expr => |child| try self.scanExpr(child, owner, false),
            .expect => |child| try self.scanExpr(child, owner, false),
            .return_ => |ret| {
                try self.scanExpr(ret.expr, owner, false);
                // `ret.lambda` is the enclosing lambda context for early-return
                // lowering, not an owned child expression.
            },
            .for_ => |for_| {
                try self.scanPattern(for_.pattern, owner);
                try self.scanExpr(for_.expr, owner, false);
                try self.scanExpr(for_.body, owner, false);
            },
            .while_ => |while_| {
                try self.scanExpr(while_.cond, owner, false);
                try self.scanExpr(while_.body, owner, false);
            },
            .pending,
            .crash,
            .break_,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            => {},
        }
    }
};

/// Public `HostedProc` declaration.
pub const HostedProc = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    expr_idx: CIR.Expr.Idx,
    external_symbol_name: canonical.ExternalSymbolNameId,
    deterministic_index: u32,
    order_key: []const u8,
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

/// Public `HostedProcTable` declaration.
pub const HostedProcTable = struct {
    procs: []HostedProc = &.{},

    const Candidate = struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        expr_idx: CIR.Expr.Idx,
        external_symbol_name: canonical.ExternalSymbolNameId,
        proc: canonical.ProcedureValueRef,
        template: canonical.ProcedureTemplateRef,
        sort_key: []const u8,
    };

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        templates: *const CheckedProcedureTemplateTable,
    ) Allocator.Error!HostedProcTable {
        var candidates = std.ArrayList(Candidate).empty;
        defer {
            for (candidates.items) |candidate| allocator.free(candidate.sort_key);
            candidates.deinit(allocator);
        }

        var procs = std.ArrayList(HostedProc).empty;
        errdefer {
            for (procs.items) |proc| allocator.free(proc.order_key);
            procs.deinit(allocator);
        }

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            switch (def.expr.data) {
                .e_hosted_lambda => |hosted| {
                    const template_ref = templates.lookupByDef(def_idx) orelse {
                        if (builtin.mode == .Debug) {
                            std.debug.panic("checked artifact invariant violated: hosted procedure def has no checked template", .{});
                        }
                        unreachable;
                    };

                    try candidates.append(allocator, .{
                        .module_idx = module.moduleIndex(),
                        .def_idx = def_idx,
                        .expr_idx = def.expr.idx,
                        .external_symbol_name = try names.internExternalSymbolIdent(module.identStoreConst(), hosted.symbol_name),
                        .proc = .{
                            .artifact = template_ref.artifact,
                            .proc_base = template_ref.proc_base,
                        },
                        .template = template_ref,
                        .sort_key = try hostedProcSortKey(allocator, module, hosted.symbol_name),
                    });
                },
                else => {},
            }
        }

        const SortContext = struct {
            pub fn lessThan(_: void, a: Candidate, b: Candidate) bool {
                return switch (std.mem.order(u8, a.sort_key, b.sort_key)) {
                    .lt => true,
                    .gt => false,
                    .eq => @intFromEnum(a.def_idx) < @intFromEnum(b.def_idx),
                };
            }
        };
        std.mem.sort(Candidate, candidates.items, {}, SortContext.lessThan);

        for (candidates.items, 0..) |candidate, index| {
            try procs.append(allocator, .{
                .module_idx = candidate.module_idx,
                .def_idx = candidate.def_idx,
                .expr_idx = candidate.expr_idx,
                .external_symbol_name = candidate.external_symbol_name,
                .deterministic_index = @intCast(index),
                .order_key = try allocator.dupe(u8, candidate.sort_key),
                .proc = candidate.proc,
                .template = candidate.template,
            });
        }

        return .{ .procs = try procs.toOwnedSlice(allocator) };
    }

    fn hostedProcSortKey(
        allocator: Allocator,
        module: TypedCIR.Module,
        symbol_name: Ident.Idx,
    ) Allocator.Error![]const u8 {
        const module_env = module.moduleEnvConst();
        var module_name = module_env.module_name;
        if (std.mem.endsWith(u8, module_name, ".roc")) {
            module_name = module_name[0 .. module_name.len - 4];
        }

        const local_name = module.getIdent(symbol_name);
        const qualified_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ module_name, local_name });
        if (!std.mem.endsWith(u8, qualified_name, "!")) return qualified_name;

        const stripped = try allocator.dupe(u8, qualified_name[0 .. qualified_name.len - 1]);
        allocator.free(qualified_name);
        return stripped;
    }

    pub fn deinit(self: *HostedProcTable, allocator: Allocator) void {
        for (self.procs) |proc| allocator.free(proc.order_key);
        allocator.free(self.procs);
        self.* = .{};
    }
};

/// Public `PlatformAppRelationKey` declaration.
pub const PlatformAppRelationKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,

    pub fn compute(
        app_artifact: CheckedModuleArtifactKey,
        requirement_context: PlatformRequirementContextKey,
    ) PlatformAppRelationKey {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(&app_artifact.bytes);
        hasher.update(&requirement_context.bytes);
        return .{ .bytes = hasher.finalResult() };
    }
};

/// Public `PlatformRequiredDeclarationId` declaration.
pub const PlatformRequiredDeclarationId = enum(u32) { _ };
/// Public `PlatformRequiredBindingId` declaration.
pub const PlatformRequiredBindingId = enum(u32) { _ };
/// Public `PlatformRequirementRelationId` declaration.
pub const PlatformRequirementRelationId = enum(u32) { _ };

/// Public `PlatformRequiredValueKind` declaration.
pub const PlatformRequiredValueKind = enum {
    const_value,
    procedure_value,
};

/// Public `PlatformRequiredDeclaration` declaration.
pub const PlatformRequiredDeclaration = struct {
    id: PlatformRequiredDeclarationId,
    module_idx: u32,
    requires_idx: u32,
    platform_name: canonical.ExportNameId,
    declared_source_ty: canonical.CanonicalTypeSchemeKey,
    type_anno: CIR.TypeAnno.Idx,
};

/// Public `PlatformRequiredDeclarationTable` declaration.
pub const PlatformRequiredDeclarationTable = struct {
    declarations: []PlatformRequiredDeclaration = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
    ) Allocator.Error!PlatformRequiredDeclarationTable {
        const required_types = module.requiresTypes();
        const declarations = try allocator.alloc(PlatformRequiredDeclaration, required_types.len);
        errdefer allocator.free(declarations);

        for (required_types, 0..) |required_type, i| {
            declarations[i] = .{
                .id = @enumFromInt(@as(u32, @intCast(i))),
                .module_idx = module.moduleIndex(),
                .requires_idx = @intCast(i),
                .platform_name = try names.internExportIdent(module.identStoreConst(), required_type.ident),
                .declared_source_ty = try canonical_type_keys.schemeFromVar(
                    allocator,
                    module.typeStoreConst(),
                    module.identStoreConst(),
                    ModuleEnv.varFrom(required_type.type_anno),
                ),
                .type_anno = required_type.type_anno,
            };
        }

        return .{ .declarations = declarations };
    }

    pub fn lookupByRequiredIndex(
        self: *const PlatformRequiredDeclarationTable,
        requires_idx: u32,
    ) ?PlatformRequiredDeclaration {
        for (self.declarations) |declaration| {
            if (declaration.requires_idx == requires_idx) return declaration;
        }
        return null;
    }

    pub fn lookupByDeclarationId(
        self: *const PlatformRequiredDeclarationTable,
        declaration_id: PlatformRequiredDeclarationId,
    ) ?PlatformRequiredDeclaration {
        const raw = @intFromEnum(declaration_id);
        if (raw >= self.declarations.len) return null;
        return self.declarations[raw];
    }

    pub fn identityHash(
        self: *const PlatformRequiredDeclarationTable,
        names: *const canonical.CanonicalNameStore,
    ) [32]u8 {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hashByteSlice(&hasher, "platform_required_declarations");
        hashU32(&hasher, @intCast(self.declarations.len));
        for (self.declarations) |declaration| {
            hashU32(&hasher, declaration.requires_idx);
            hashByteSlice(&hasher, names.exportNameText(declaration.platform_name));
            hasher.update(&declaration.declared_source_ty.bytes);
        }
        return hasher.finalResult();
    }

    pub fn deinit(self: *PlatformRequiredDeclarationTable, allocator: Allocator) void {
        allocator.free(self.declarations);
        self.* = .{};
    }
};

/// Public `PlatformRequiredProcedureUse` declaration.
pub const PlatformRequiredProcedureUse = struct {
    procedure: ProcedureUseTemplate,
    relation_template_closure: ImportedTemplateClosureView = .{},
};

/// Public `PlatformRequiredConstUse` declaration.
pub const PlatformRequiredConstUse = struct {
    const_use: ConstUseTemplate,
    relation_template_closure: ImportedTemplateClosureView = .{},
};

/// Public `PlatformRequiredValueUse` declaration.
pub const PlatformRequiredValueUse = union(enum) {
    const_value: PlatformRequiredConstUse,
    procedure_value: PlatformRequiredProcedureUse,
};

/// Public `PlatformRequirementRelationInput` declaration.
pub const PlatformRequirementRelationInput = struct {
    id: PlatformRequirementRelationId,
    declaration: PlatformRequiredDeclarationId,
    requires_idx: u32,
    app_value: TopLevelValueRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    app_value_source_scheme: canonical.CanonicalTypeSchemeKey,
    value_kind: PlatformRequiredValueKind,
};

/// Public `PlatformRequiredBindingInput` declaration.
pub const PlatformRequiredBindingInput = struct {
    declaration: PlatformRequiredDeclarationId,
    requires_idx: u32,
    app_value: TopLevelValueRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    checked_relation: PlatformRequirementRelationId,
    value_use: PlatformRequiredValueUse,
};

/// Public `PlatformAppRelation` declaration.
pub const PlatformAppRelation = struct {
    key: PlatformAppRelationKey,
    requirement_context: PlatformRequirementContextKey,
    platform_module_idx: u32,
    app_artifact: CheckedModuleArtifactKey,
    relations: []const PlatformRequirementRelationInput,
    bindings: []const PlatformRequiredBindingInput,

    pub fn deinit(self: *PlatformAppRelation, allocator: Allocator) void {
        for (self.bindings) |*binding| {
            var value_use = binding.value_use;
            deinitPlatformRequiredValueUse(allocator, &value_use);
        }
        allocator.free(self.relations);
        allocator.free(self.bindings);
        self.* = .{
            .key = .{},
            .requirement_context = .{},
            .platform_module_idx = 0,
            .app_artifact = .{},
            .relations = &.{},
            .bindings = &.{},
        };
    }
};

/// Public `PlatformRequirementRelation` declaration.
pub const PlatformRequirementRelation = struct {
    id: PlatformRequirementRelationId,
    relation: PlatformAppRelationKey,
    module_idx: u32,
    declaration: PlatformRequiredDeclarationId,
    requires_idx: u32,
    app_value: TopLevelValueRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    requested_source_ty_payload: CheckedTypeId,
    app_value_source_scheme: canonical.CanonicalTypeSchemeKey,
    value_kind: PlatformRequiredValueKind,
};

/// Public `PlatformRequirementRelationTable` declaration.
pub const PlatformRequirementRelationTable = struct {
    relations: []PlatformRequirementRelation = &.{},

    pub fn fromRelation(
        allocator: Allocator,
        module: TypedCIR.Module,
        module_identity: ModuleIdentity,
        names: *const canonical.CanonicalNameStore,
        checked_types: *const CheckedTypePublication,
        declarations: *const PlatformRequiredDeclarationTable,
        relation: ?PlatformAppRelation,
    ) Allocator.Error!PlatformRequirementRelationTable {
        const active_relation = relation orelse return .{};
        validatePlatformAppRelationForModule(
            module,
            module_identity,
            names,
            declarations,
            active_relation,
        );
        if (active_relation.relations.len != declarations.declarations.len) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: platform/app relation has {d} checked relation rows for {d} platform requirements",
                    .{ active_relation.relations.len, declarations.declarations.len },
                );
            }
            unreachable;
        }

        var seen_declarations: []bool = &.{};
        if (builtin.mode == .Debug) {
            seen_declarations = try allocator.alloc(bool, declarations.declarations.len);
            @memset(seen_declarations, false);
        }
        defer {
            if (builtin.mode == .Debug) allocator.free(seen_declarations);
        }

        const rows = try allocator.alloc(PlatformRequirementRelation, active_relation.relations.len);
        errdefer allocator.free(rows);

        for (active_relation.relations, 0..) |input, i| {
            const declaration = declarations.lookupByDeclarationId(input.declaration) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app checked relation {d} references unknown requirement declaration",
                        .{i},
                    );
                }
                unreachable;
            };
            const declaration_index: usize = @intCast(@intFromEnum(input.declaration));
            if (builtin.mode == .Debug) {
                if (seen_declarations[declaration_index]) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app checked relation binds declaration {d} more than once",
                        .{declaration_index},
                    );
                }
                seen_declarations[declaration_index] = true;
            }
            if (input.requires_idx != declaration.requires_idx) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app checked relation {d} maps declaration {d} to required index {d}, expected {d}",
                        .{ i, declaration_index, input.requires_idx, declaration.requires_idx },
                    );
                }
                unreachable;
            }
            if (!std.mem.eql(u8, &input.app_value.artifact.bytes, &active_relation.app_artifact.bytes)) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app checked relation {d} points at a value outside the app artifact",
                        .{i},
                    );
                }
                unreachable;
            }
            const payload = platformRequiredPayloadForDeclaration(module, checked_types, declaration);
            const payload_key = checked_types.store.roots[@intFromEnum(payload)].key;

            rows[i] = .{
                .id = input.id,
                .relation = active_relation.key,
                .module_idx = module.moduleIndex(),
                .declaration = input.declaration,
                .requires_idx = input.requires_idx,
                .app_value = input.app_value,
                .requested_source_ty = payload_key,
                .requested_source_ty_payload = payload,
                .app_value_source_scheme = input.app_value_source_scheme,
                .value_kind = input.value_kind,
            };
        }

        return .{ .relations = rows };
    }

    pub fn deinit(self: *PlatformRequirementRelationTable, allocator: Allocator) void {
        allocator.free(self.relations);
        self.* = .{};
    }

    pub fn lookupByRelationId(self: *const PlatformRequirementRelationTable, relation_id: PlatformRequirementRelationId) ?PlatformRequirementRelation {
        const raw = @intFromEnum(relation_id);
        if (raw >= self.relations.len) return null;
        return self.relations[raw];
    }
};

/// Public `PlatformRequiredBinding` declaration.
pub const PlatformRequiredBinding = struct {
    id: PlatformRequiredBindingId,
    relation: PlatformAppRelationKey,
    module_idx: u32,
    declaration: PlatformRequiredDeclarationId,
    requires_idx: u32,
    app_value: TopLevelValueRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    checked_relation: PlatformRequirementRelationId,
    value_use: PlatformRequiredValueUse,
};

/// Public `PlatformRequiredBindingTable` declaration.
pub const PlatformRequiredBindingTable = struct {
    bindings: []PlatformRequiredBinding = &.{},

    pub fn fromRelation(
        allocator: Allocator,
        module: TypedCIR.Module,
        module_identity: ModuleIdentity,
        names: *const canonical.CanonicalNameStore,
        declarations: *const PlatformRequiredDeclarationTable,
        relations: *const PlatformRequirementRelationTable,
        relation: ?PlatformAppRelation,
    ) Allocator.Error!PlatformRequiredBindingTable {
        const active_relation = relation orelse return .{};
        validatePlatformAppRelationForModule(
            module,
            module_identity,
            names,
            declarations,
            active_relation,
        );
        if (active_relation.bindings.len != declarations.declarations.len) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: platform/app relation has {d} bindings for {d} platform requirements",
                    .{ active_relation.bindings.len, declarations.declarations.len },
                );
            }
            unreachable;
        }
        var seen_declarations: []bool = &.{};
        if (builtin.mode == .Debug) {
            seen_declarations = try allocator.alloc(bool, declarations.declarations.len);
            @memset(seen_declarations, false);
        }
        defer {
            if (builtin.mode == .Debug) allocator.free(seen_declarations);
        }

        const bindings = try allocator.alloc(PlatformRequiredBinding, active_relation.bindings.len);
        var initialized_bindings: usize = 0;
        errdefer {
            for (bindings[0..initialized_bindings]) |*owned_binding| {
                deinitPlatformRequiredValueUse(allocator, &owned_binding.value_use);
            }
            allocator.free(bindings);
        }

        for (active_relation.bindings, 0..) |binding, i| {
            const declaration = declarations.lookupByDeclarationId(binding.declaration) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app binding {d} references unknown requirement declaration",
                        .{i},
                    );
                }
                unreachable;
            };
            const declaration_index: usize = @intCast(@intFromEnum(binding.declaration));
            if (builtin.mode == .Debug) {
                if (seen_declarations[declaration_index]) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app relation binds platform requirement declaration {d} more than once",
                        .{declaration_index},
                    );
                }
                seen_declarations[declaration_index] = true;
            }
            if (declaration.requires_idx != binding.requires_idx) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app binding {d} maps declaration {d} to required index {d}, expected {d}",
                        .{ i, declaration_index, binding.requires_idx, declaration.requires_idx },
                    );
                }
                unreachable;
            }
            if (!std.mem.eql(u8, &binding.app_value.artifact.bytes, &active_relation.app_artifact.bytes)) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app binding {d} points at a value outside the app artifact",
                        .{i},
                    );
                }
                unreachable;
            }
            const checked_relation = relations.lookupByRelationId(binding.checked_relation) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app binding {d} references missing checked relation",
                        .{i},
                    );
                }
                unreachable;
            };
            validatePlatformBindingRelation(binding, checked_relation, i);
            bindings[i] = .{
                .id = @enumFromInt(@as(u32, @intCast(i))),
                .relation = active_relation.key,
                .module_idx = module.moduleIndex(),
                .declaration = binding.declaration,
                .requires_idx = binding.requires_idx,
                .app_value = binding.app_value,
                .requested_source_ty = checked_relation.requested_source_ty,
                .checked_relation = binding.checked_relation,
                .value_use = try clonePlatformRequiredValueUseWithRelation(allocator, binding.value_use, checked_relation),
            };
            initialized_bindings += 1;
        }

        return .{ .bindings = bindings };
    }

    pub fn deinit(self: *PlatformRequiredBindingTable, allocator: Allocator) void {
        for (self.bindings) |*binding| deinitPlatformRequiredValueUse(allocator, &binding.value_use);
        allocator.free(self.bindings);
        self.* = .{};
    }

    pub fn lookupByRequiredIndex(self: *const PlatformRequiredBindingTable, requires_idx: u32) ?PlatformRequiredBinding {
        for (self.bindings) |binding| {
            if (binding.requires_idx == requires_idx) return binding;
        }
        return null;
    }

    pub fn lookupByBindingId(self: *const PlatformRequiredBindingTable, binding_id: u32) ?PlatformRequiredBinding {
        if (binding_id >= self.bindings.len) return null;
        return self.bindings[binding_id];
    }
};

fn validatePlatformAppRelationForModule(
    module: TypedCIR.Module,
    module_identity: ModuleIdentity,
    names: *const canonical.CanonicalNameStore,
    declarations: *const PlatformRequiredDeclarationTable,
    active_relation: PlatformAppRelation,
) void {
    if (active_relation.platform_module_idx != module.moduleIndex()) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform/app relation belongs to module {d}, not platform module {d}",
                .{ active_relation.platform_module_idx, module.moduleIndex() },
            );
        }
        unreachable;
    }
    const expected_requirement_context = PlatformRequirementContextKey.compute(
        module_identity,
        declarations.identityHash(names),
    );
    if (!std.mem.eql(u8, &active_relation.requirement_context.bytes, &expected_requirement_context.bytes)) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform/app relation requirement context does not match the current platform requirement declarations",
                .{},
            );
        }
        unreachable;
    }
    const expected_key = PlatformAppRelationKey.compute(
        active_relation.app_artifact,
        active_relation.requirement_context,
    );
    if (!std.mem.eql(u8, &active_relation.key.bytes, &expected_key.bytes)) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform/app relation key does not match the current platform requirement declarations",
                .{},
            );
        }
        unreachable;
    }
}

fn platformRequiredPayloadForDeclaration(
    module: TypedCIR.Module,
    checked_types: *const CheckedTypePublication,
    declaration: PlatformRequiredDeclaration,
) CheckedTypeId {
    const module_env = module.moduleEnvConst();
    if (declaration.requires_idx >= module_env.requires_types.items.items.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform requirement declaration {d} has out-of-range required index {d}",
                .{ @intFromEnum(declaration.id), declaration.requires_idx },
            );
        }
        unreachable;
    }
    const required_type = module_env.requires_types.items.items[declaration.requires_idx];
    return checked_types.rootForSourceVar(module, ModuleEnv.varFrom(required_type.type_anno)) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform requirement declaration {d} has no platform-owned checked payload",
                .{@intFromEnum(declaration.id)},
            );
        }
        unreachable;
    };
}

fn applyPlatformForClauseSubstitutions(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    checked_types: *CheckedTypePublication,
    relation_artifacts: []const ImportedModuleView,
    relation: ?PlatformAppRelation,
) Allocator.Error!void {
    const active_relation = relation orelse return;
    const app_view = relationArtifactByKey(relation_artifacts, active_relation.app_artifact) orelse {
        checkedArtifactInvariant("platform for-clause substitution missing app relation artifact", .{});
    };

    const module_env = module.moduleEnvConst();
    if (module_env.for_clause_aliases.len() == 0) return;

    var projector = CheckedTypeStoreImportProjector.init(allocator, &checked_types.store, names, app_view);
    defer projector.deinit();

    var formals = std.ArrayList(CheckedTypeId).empty;
    defer formals.deinit(allocator);
    var actuals = std.ArrayList(CheckedTypeId).empty;
    defer actuals.deinit(allocator);

    for (module.requiresTypes()) |required_type| {
        const aliases = module_env.for_clause_aliases.sliceRange(required_type.type_aliases);
        for (aliases) |alias| {
            const formal = checked_types.rootForSourceVar(module, ModuleEnv.varFrom(alias.alias_stmt_idx)) orelse {
                checkedArtifactInvariant("platform for-clause substitution missing platform alias checked root", .{});
            };
            const alias_name = module_env.getIdent(alias.alias_name);
            const app_alias = (try appAliasCheckedRootForName(allocator, app_view, alias_name)) orelse {
                checkedArtifactInvariant("platform for-clause substitution missing matching app alias", .{});
            };
            const actual = try projector.project(app_alias);
            try appendUniquePlatformForClauseSubstitution(&formals, &actuals, allocator, formal, actual);
        }
    }

    if (formals.items.len == 0) return;

    var active = std.AutoHashMap(CheckedTypeId, CheckedTypeId).init(allocator);
    defer active.deinit();

    for (checked_types.source_type_roots) |*entry| {
        active.clearRetainingCapacity();
        entry.checked_root = try checked_types.store.cloneCheckedTypeRootSubstituting(
            allocator,
            names,
            entry.checked_root,
            formals.items,
            actuals.items,
            &active,
        );
    }
}

fn appendUniquePlatformForClauseSubstitution(
    formals: *std.ArrayList(CheckedTypeId),
    actuals: *std.ArrayList(CheckedTypeId),
    allocator: Allocator,
    formal: CheckedTypeId,
    actual: CheckedTypeId,
) Allocator.Error!void {
    for (formals.items, actuals.items) |existing_formal, existing_actual| {
        if (existing_formal != formal) continue;
        if (existing_actual != actual) {
            checkedArtifactInvariant("platform for-clause substitution mapped one formal to multiple actuals", .{});
        }
        return;
    }
    try formals.append(allocator, formal);
    try actuals.append(allocator, actual);
}

fn relationArtifactByKey(
    relation_artifacts: []const ImportedModuleView,
    key: CheckedModuleArtifactKey,
) ?ImportedModuleView {
    for (relation_artifacts) |artifact| {
        if (std.mem.eql(u8, &artifact.key.bytes, &key.bytes)) return artifact;
    }
    return null;
}

fn appAliasCheckedRootForName(
    allocator: Allocator,
    app_view: ImportedModuleView,
    alias_name: []const u8,
) Allocator.Error!?CheckedTypeId {
    const app_env = app_view.module_env;
    for (app_env.store.sliceStatements(app_env.all_statements)) |statement_idx| {
        const statement = app_env.store.getStatement(statement_idx);
        const alias = switch (statement) {
            .s_alias_decl => |alias| alias,
            else => continue,
        };
        const header = app_env.store.getTypeHeader(alias.header);
        if (!std.mem.eql(u8, app_env.getIdent(header.relative_name), alias_name)) continue;

        const key = try canonical_type_keys.fromVar(
            allocator,
            &app_env.types,
            app_env.getIdentStoreConst(),
            ModuleEnv.varFrom(statement_idx),
        );
        for (app_view.checked_types.roots) |root| {
            if (canonicalTypeKeyEql(root.key, key)) return root.id;
        }
        checkedArtifactInvariant("platform for-clause substitution app alias was not published in app checked types", .{});
    }
    return null;
}

fn validatePlatformBindingRelation(
    binding: PlatformRequiredBindingInput,
    relation: PlatformRequirementRelation,
    binding_index: usize,
) void {
    if (relation.declaration != binding.declaration or relation.requires_idx != binding.requires_idx) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform/app binding {d} points at a checked relation for a different requirement",
                .{binding_index},
            );
        }
        unreachable;
    }
    if (!std.mem.eql(u8, &relation.app_value.artifact.bytes, &binding.app_value.artifact.bytes) or
        relation.app_value.pattern != binding.app_value.pattern)
    {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: platform/app binding {d} points at a checked relation for a different app value",
                .{binding_index},
            );
        }
        unreachable;
    }
    switch (binding.value_use) {
        .const_value => if (relation.value_kind != .const_value) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: platform/app binding {d} has const value use but procedure checked relation",
                    .{binding_index},
                );
            }
            unreachable;
        },
        .procedure_value => if (relation.value_kind != .procedure_value) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: platform/app binding {d} has procedure value use but const checked relation",
                    .{binding_index},
                );
            }
            unreachable;
        },
    }
}

fn clonePlatformRequiredValueUseWithRelation(
    allocator: Allocator,
    value_use: PlatformRequiredValueUse,
    relation: PlatformRequirementRelation,
) Allocator.Error!PlatformRequiredValueUse {
    return switch (value_use) {
        .const_value => |const_use| .{ .const_value = .{
            .const_use = .{
                .const_ref = const_use.const_use.const_ref,
                .requested_source_ty_template = relation.requested_source_ty,
                .requested_source_ty_payload = relation.requested_source_ty_payload,
            },
            .relation_template_closure = try cloneImportedTemplateClosure(allocator, const_use.relation_template_closure),
        } },
        .procedure_value => |proc_use| .{ .procedure_value = .{
            .procedure = .{
                .binding = proc_use.procedure.binding,
                .source_fn_ty_template = relation.requested_source_ty,
                .source_fn_ty_payload = relation.requested_source_ty_payload,
            },
            .relation_template_closure = try cloneImportedTemplateClosure(allocator, proc_use.relation_template_closure),
        } },
    };
}

/// Public `platformRequirementContextKey` function.
pub fn platformRequirementContextKey(artifact: *const CheckedModuleArtifact) PlatformRequirementContextKey {
    return PlatformRequirementContextKey.compute(
        artifact.module_identity,
        artifact.platform_required_declarations.identityHash(&artifact.canonical_names),
    );
}

/// Public `buildPlatformAppRelation` function.
pub fn buildPlatformAppRelation(
    allocator: Allocator,
    platform_declaration_artifact: *const CheckedModuleArtifact,
    platform_module_env: *const ModuleEnv,
    app_artifact: *const CheckedModuleArtifact,
) Allocator.Error!PlatformAppRelation {
    const declarations = platform_declaration_artifact.platform_required_declarations.declarations;
    const relations = try allocator.alloc(PlatformRequirementRelationInput, declarations.len);
    errdefer allocator.free(relations);
    const bindings = try allocator.alloc(PlatformRequiredBindingInput, declarations.len);
    var initialized_bindings: usize = 0;
    errdefer {
        for (bindings[0..initialized_bindings]) |*binding| deinitPlatformRequiredValueUse(allocator, &binding.value_use);
        allocator.free(bindings);
    }

    const requirement_context = platformRequirementContextKey(platform_declaration_artifact);
    const relation_key = PlatformAppRelationKey.compute(app_artifact.key, requirement_context);

    for (declarations, 0..) |declaration, i| {
        const required_name = platform_declaration_artifact.canonical_names.exportNameText(declaration.platform_name);
        const app_value = appTopLevelValueByName(app_artifact, required_name) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: app artifact does not publish a top-level value for platform requirement {s}",
                    .{required_name},
                );
            }
            unreachable;
        };

        const requested_source_ty = try canonical_type_keys.fromVar(
            allocator,
            &platform_module_env.types,
            platform_module_env.getIdentStoreConst(),
            ModuleEnv.varFrom(declaration.type_anno),
        );
        const app_value_ref = TopLevelValueRef{
            .artifact = app_artifact.key,
            .pattern = app_value.pattern,
        };
        const required_ty_is_function = sourceVarIsFunction(&platform_module_env.types, ModuleEnv.varFrom(declaration.type_anno));
        const value_kind: PlatformRequiredValueKind = if (required_ty_is_function) .procedure_value else .const_value;

        relations[i] = .{
            .id = @enumFromInt(@as(u32, @intCast(i))),
            .declaration = declaration.id,
            .requires_idx = declaration.requires_idx,
            .app_value = app_value_ref,
            .requested_source_ty = requested_source_ty,
            .app_value_source_scheme = app_value.source_scheme,
            .value_kind = value_kind,
        };

        bindings[i] = .{
            .declaration = declaration.id,
            .requires_idx = declaration.requires_idx,
            .app_value = app_value_ref,
            .requested_source_ty = requested_source_ty,
            .checked_relation = relations[i].id,
            .value_use = if (required_ty_is_function) blk: {
                const procedure_binding = switch (app_value.value) {
                    .procedure_binding => |binding| binding,
                    .const_ref => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "checked artifact invariant violated: platform requirement {s} needs a procedure but app value is a const",
                                .{required_name},
                            );
                        }
                        unreachable;
                    },
                };
                _ = app_artifact.top_level_procedure_bindings.get(procedure_binding);
                var template_closure = try cloneImportedTemplateClosure(
                    allocator,
                    exportedProcedureBindingClosureForAppValue(app_artifact, app_value_ref),
                );
                errdefer deinitImportedTemplateClosure(allocator, &template_closure);

                break :blk .{ .procedure_value = .{
                    .procedure = .{
                        .binding = .{ .platform_required = .{
                            .artifact = app_artifact.key,
                            .app_value = app_value_ref,
                            .procedure_binding = procedure_binding,
                        } },
                        .source_fn_ty_template = requested_source_ty,
                        .source_fn_ty_payload = null,
                    },
                    .relation_template_closure = template_closure,
                } };
            } else blk: {
                const const_ref = switch (app_value.value) {
                    .const_ref => |ref| ref,
                    .procedure_binding => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "checked artifact invariant violated: platform requirement {s} needs a const but app value is a procedure",
                                .{required_name},
                            );
                        }
                        unreachable;
                    },
                };
                var template_closure = try cloneImportedTemplateClosure(
                    allocator,
                    exportedConstTemplateClosureForAppValue(app_artifact, app_value_ref, const_ref),
                );
                errdefer deinitImportedTemplateClosure(allocator, &template_closure);

                break :blk .{ .const_value = .{
                    .const_use = .{
                        .const_ref = const_ref,
                        .requested_source_ty_template = requested_source_ty,
                        .requested_source_ty_payload = null,
                    },
                    .relation_template_closure = template_closure,
                } };
            },
        };
        initialized_bindings += 1;
    }

    return .{
        .key = relation_key,
        .requirement_context = requirement_context,
        .platform_module_idx = platform_declaration_artifact.module_identity.module_idx,
        .app_artifact = app_artifact.key,
        .relations = relations,
        .bindings = bindings,
    };
}

fn appTopLevelValueByName(
    app_artifact: *const CheckedModuleArtifact,
    required_name: []const u8,
) ?TopLevelValueEntry {
    for (app_artifact.top_level_values.entries) |entry| {
        const app_name = app_artifact.canonical_names.exportNameText(entry.source_name);
        if (std.mem.eql(u8, app_name, required_name)) return entry;
    }
    return null;
}

fn exportedProcedureBindingClosureForAppValue(
    app_artifact: *const CheckedModuleArtifact,
    app_value: TopLevelValueRef,
) ImportedTemplateClosureView {
    for (app_artifact.exported_procedure_bindings.bindings) |binding| {
        if (binding.binding.pattern != app_value.pattern) continue;
        switch (binding.body) {
            .direct_template => |direct| {
                if (checkedTemplateFromCallableTemplateForClosure(direct.template)) |template_ref| {
                    return exportedProcedureTemplateClosureForRef(app_artifact, template_ref);
                }
            },
            .callable_eval_template => {},
        }
        return binding.template_closure;
    }
    checkedArtifactInvariant("platform-required app procedure was not exported by the app artifact", .{});
}

fn exportedProcedureTemplateClosureForRef(
    app_artifact: *const CheckedModuleArtifact,
    template_ref: canonical.ProcedureTemplateRef,
) ImportedTemplateClosureView {
    for (app_artifact.exported_procedure_templates.templates) |template| {
        if (canonical.procedureTemplateRefEql(template.template, template_ref)) return template.template_closure;
    }
    checkedArtifactInvariant("platform-required app procedure template was not exported by the app artifact", .{});
}

fn exportedConstTemplateClosureForAppValue(
    app_artifact: *const CheckedModuleArtifact,
    app_value: TopLevelValueRef,
    const_ref: ConstRef,
) ImportedTemplateClosureView {
    for (app_artifact.exported_const_templates.templates) |template| {
        if (template.pattern != app_value.pattern) continue;
        if (!constRefEql(template.const_ref, const_ref)) {
            checkedArtifactInvariant("platform-required app const export disagreed with top-level const ref", .{});
        }
        return template.template_closure;
    }
    checkedArtifactInvariant("platform-required app const was not exported by the app artifact", .{});
}

fn topLevelDefSourceName(
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    def: TypedCIR.Def,
) Allocator.Error!canonical.ExportNameId {
    switch (def.pattern.data) {
        .assign => |assign| return try names.internExportIdent(module.identStoreConst(), assign.ident),
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("checked artifact invariant violated: top-level value has non-assign pattern", .{});
            }
            unreachable;
        },
    }
}

fn publishProvidesMetadata(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    names: *canonical.CanonicalNameStore,
) Allocator.Error![]ProvidesEntry {
    const source = module_env.provides_entries.items.items;
    const provides = try allocator.alloc(ProvidesEntry, source.len);
    errdefer allocator.free(provides);

    for (source, 0..) |entry, i| {
        provides[i] = .{
            .source_name = try names.internExportIdent(module_env.getIdentStoreConst(), entry.ident),
            .ffi_symbol = try names.internExternalSymbolName(module_env.getString(entry.ffi_symbol)),
        };
    }

    return provides;
}

fn publishRequiresMetadata(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
) Allocator.Error![]RequiresEntry {
    const source = module.requiresTypes();
    const requires = try allocator.alloc(RequiresEntry, source.len);
    errdefer allocator.free(requires);

    for (source, 0..) |entry, i| {
        requires[i] = .{
            .platform_name = try names.internExportIdent(module.identStoreConst(), entry.ident),
            .declared_source_ty = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                ModuleEnv.varFrom(entry.type_anno),
            ),
        };
    }

    return requires;
}

/// Public `BoxPayloadCapabilityId` declaration.
pub const BoxPayloadCapabilityId = enum(u32) { _ };
/// Public `OpaqueAtomicProofId` declaration.
pub const OpaqueAtomicProofId = enum(u32) { _ };
/// Public `HostedRepresentationCapabilityId` declaration.
pub const HostedRepresentationCapabilityId = enum(u32) { _ };
/// Public `PlatformRepresentationCapabilityId` declaration.
pub const PlatformRepresentationCapabilityId = enum(u32) { _ };
/// Public `ExportedNominalRepresentationId` declaration.
pub const ExportedNominalRepresentationId = enum(u32) { _ };

/// Public `BoxPayloadCapabilityEntry` declaration.
pub const BoxPayloadCapabilityEntry = struct {
    id: BoxPayloadCapabilityId,
    nominal: canonical.NominalTypeKey,
    source_ty: canonical.CanonicalTypeKey,
    backing_ty: CheckedTypeId,
    backing_ty_key: canonical.CanonicalTypeKey,
    instantiated_args: []const canonical.CanonicalTypeKey = &.{},
    is_opaque: bool,
};

/// Public `OpaqueAtomicProofEntry` declaration.
pub const OpaqueAtomicProofEntry = struct {
    id: OpaqueAtomicProofId,
    nominal: canonical.NominalTypeKey,
    source_ty: canonical.CanonicalTypeKey,
    instantiated_args: []const canonical.CanonicalTypeKey = &.{},
    proof: NoReachableCallableSlotsProof,
};

/// Public `HostedRepresentationCapability` declaration.
pub const HostedRepresentationCapability = struct {
    id: HostedRepresentationCapabilityId,
    external_symbol_name: canonical.ExternalSymbolNameId,
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

/// Public `PlatformRepresentationCapability` declaration.
pub const PlatformRepresentationCapability = struct {
    id: PlatformRepresentationCapabilityId,
    requirement: PlatformRequiredDeclarationId,
    platform_name: canonical.ExportNameId,
    declared_source_ty: canonical.CanonicalTypeSchemeKey,
};

/// Public `ExportedNominalRepresentation` declaration.
pub const ExportedNominalRepresentation = struct {
    id: ExportedNominalRepresentationId,
    nominal: canonical.NominalTypeKey,
    source_ty: canonical.CanonicalTypeKey,
    box_payload_capability: BoxPayloadCapabilityId,
    opaque_atomic_proof: ?OpaqueAtomicProofId = null,
};

/// Public `ModuleInterfaceCapabilities` declaration.
pub const ModuleInterfaceCapabilities = struct {
    boxed_payload_templates: []const BoxPayloadCapabilityEntry = &.{},
    opaque_atomic_proofs: []const OpaqueAtomicProofEntry = &.{},
    hosted_representations: []const HostedRepresentationCapability = &.{},
    platform_representations: []const PlatformRepresentationCapability = &.{},
    exported_nominal_representations: []const ExportedNominalRepresentation = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *CheckedTypeStore,
        hosted_procs: *const HostedProcTable,
        platform_required_declarations: *const PlatformRequiredDeclarationTable,
        names: *const canonical.CanonicalNameStore,
    ) Allocator.Error!ModuleInterfaceCapabilities {
        const current_module = names.lookupModuleIdent(module.identStoreConst(), module.qualifiedModuleIdent()) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("checked artifact invariant violated: module identity was not interned before interface capability publication", .{});
            }
            unreachable;
        };

        var boxed_payload_templates = std.ArrayList(BoxPayloadCapabilityEntry).empty;
        errdefer {
            for (boxed_payload_templates.items) |entry| freeConstSlice(allocator, entry.instantiated_args);
            boxed_payload_templates.deinit(allocator);
        }
        var opaque_atomic_proofs = std.ArrayList(OpaqueAtomicProofEntry).empty;
        errdefer {
            for (opaque_atomic_proofs.items) |entry| freeConstSlice(allocator, entry.instantiated_args);
            opaque_atomic_proofs.deinit(allocator);
        }
        var exported_nominal_representations = std.ArrayList(ExportedNominalRepresentation).empty;
        errdefer exported_nominal_representations.deinit(allocator);

        var seen_nominals = std.AutoHashMap(NominalCapabilitySeenKey, void).init(allocator);
        defer seen_nominals.deinit();

        const published_payload_count = checked_types.payloads.len;
        var i: usize = 0;
        while (i < published_payload_count) : (i += 1) {
            const payload = checked_types.payloads[i];
            const nominal = switch (payload) {
                .nominal => |nominal| nominal,
                else => continue,
            };
            if (nominal.builtin != null) continue;
            if (nominal.origin_module != current_module) continue;

            const source_key = checked_types.roots[i].key;
            const nominal_key = canonical.NominalTypeKey{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
            };
            const seen_key = NominalCapabilitySeenKey{
                .source_ty = source_key,
                .nominal = nominal_key,
            };
            if (seen_nominals.contains(seen_key)) continue;
            try seen_nominals.put(seen_key, {});

            var args = try checkedTypeKeysForIds(allocator, checked_types, nominal.args);
            errdefer allocator.free(args);

            const declaration = checked_types.nominalDeclaration(nominal_key) orelse {
                checkedArtifactInvariant("nominal representation publication could not find the checked nominal declaration", .{});
            };
            const backing_ty = try checked_types.ensureInstantiatedNominalBackingRoot(
                allocator,
                names,
                declaration,
                nominal.args,
            );

            const capability_id: BoxPayloadCapabilityId = @enumFromInt(@as(u32, @intCast(boxed_payload_templates.items.len)));
            try boxed_payload_templates.append(allocator, .{
                .id = capability_id,
                .nominal = nominal_key,
                .source_ty = source_key,
                .backing_ty = backing_ty,
                .backing_ty_key = checkedTypeKeyForId(checked_types, backing_ty),
                .instantiated_args = args,
                .is_opaque = nominal.is_opaque,
            });
            const capability_args = boxed_payload_templates.items[@intFromEnum(capability_id)].instantiated_args;
            args = &.{};

            const proof_id: ?OpaqueAtomicProofId = blk: {
                if (!nominal.is_opaque) break :blk null;
                if (!try checkedTypeHasNoReachableCallableSlots(allocator, checked_types, backing_ty)) break :blk null;

                var owned_args = try allocator.dupe(canonical.CanonicalTypeKey, capability_args);
                errdefer allocator.free(owned_args);
                const id: OpaqueAtomicProofId = @enumFromInt(@as(u32, @intCast(opaque_atomic_proofs.items.len)));
                try opaque_atomic_proofs.append(allocator, .{
                    .id = id,
                    .nominal = nominal_key,
                    .source_ty = source_key,
                    .instantiated_args = owned_args,
                    .proof = .checked_artifact_verified,
                });
                owned_args = &.{};
                break :blk id;
            };

            const exported_id: ExportedNominalRepresentationId = @enumFromInt(@as(u32, @intCast(exported_nominal_representations.items.len)));
            try exported_nominal_representations.append(allocator, .{
                .id = exported_id,
                .nominal = nominal_key,
                .source_ty = source_key,
                .box_payload_capability = capability_id,
                .opaque_atomic_proof = proof_id,
            });
        }

        const hosted_representations = try allocator.alloc(HostedRepresentationCapability, hosted_procs.procs.len);
        errdefer allocator.free(hosted_representations);
        for (hosted_procs.procs, 0..) |hosted, hosted_index| {
            hosted_representations[hosted_index] = .{
                .id = @enumFromInt(@as(u32, @intCast(hosted_index))),
                .external_symbol_name = hosted.external_symbol_name,
                .proc = hosted.proc,
                .template = hosted.template,
            };
        }

        const platform_representations = try allocator.alloc(PlatformRepresentationCapability, platform_required_declarations.declarations.len);
        errdefer allocator.free(platform_representations);
        for (platform_required_declarations.declarations, 0..) |declaration, platform_index| {
            platform_representations[platform_index] = .{
                .id = @enumFromInt(@as(u32, @intCast(platform_index))),
                .requirement = declaration.id,
                .platform_name = declaration.platform_name,
                .declared_source_ty = declaration.declared_source_ty,
            };
        }

        return .{
            .boxed_payload_templates = try boxed_payload_templates.toOwnedSlice(allocator),
            .opaque_atomic_proofs = try opaque_atomic_proofs.toOwnedSlice(allocator),
            .hosted_representations = hosted_representations,
            .platform_representations = platform_representations,
            .exported_nominal_representations = try exported_nominal_representations.toOwnedSlice(allocator),
        };
    }

    pub fn deinit(self: *ModuleInterfaceCapabilities, allocator: Allocator) void {
        for (self.boxed_payload_templates) |entry| freeConstSlice(allocator, entry.instantiated_args);
        for (self.opaque_atomic_proofs) |entry| freeConstSlice(allocator, entry.instantiated_args);
        freeConstSlice(allocator, self.boxed_payload_templates);
        freeConstSlice(allocator, self.opaque_atomic_proofs);
        freeConstSlice(allocator, self.hosted_representations);
        freeConstSlice(allocator, self.platform_representations);
        freeConstSlice(allocator, self.exported_nominal_representations);
        self.* = .{};
    }

    pub fn boxPayloadCapabilityForSource(
        self: *const ModuleInterfaceCapabilities,
        source_ty: canonical.CanonicalTypeKey,
    ) ?BoxPayloadCapabilityEntry {
        for (self.boxed_payload_templates) |entry| {
            if (canonicalTypeKeyEql(entry.source_ty, source_ty)) return entry;
        }
        return null;
    }

    pub fn boxPayloadCapabilityForNominal(
        self: *const ModuleInterfaceCapabilities,
        nominal: canonical.NominalTypeKey,
        instantiated_args: []const canonical.CanonicalTypeKey,
    ) ?BoxPayloadCapabilityEntry {
        for (self.boxed_payload_templates) |entry| {
            if (canonicalNominalTypeKeyEql(entry.nominal, nominal) and
                canonicalTypeKeySliceEql(entry.instantiated_args, instantiated_args))
            {
                return entry;
            }
        }
        return null;
    }

    pub fn boxPayloadCapability(
        self: *const ModuleInterfaceCapabilities,
        id: BoxPayloadCapabilityId,
    ) BoxPayloadCapabilityEntry {
        const index: usize = @intFromEnum(id);
        if (index >= self.boxed_payload_templates.len) {
            checkedArtifactInvariant("interface capability lookup referenced missing boxed payload capability", .{});
        }
        return self.boxed_payload_templates[index];
    }

    pub fn opaqueAtomicProofForSource(
        self: *const ModuleInterfaceCapabilities,
        source_ty: canonical.CanonicalTypeKey,
    ) ?OpaqueAtomicProofEntry {
        for (self.opaque_atomic_proofs) |entry| {
            if (canonicalTypeKeyEql(entry.source_ty, source_ty)) return entry;
        }
        return null;
    }

    pub fn opaqueAtomicProofForNominal(
        self: *const ModuleInterfaceCapabilities,
        nominal: canonical.NominalTypeKey,
        instantiated_args: []const canonical.CanonicalTypeKey,
    ) ?OpaqueAtomicProofEntry {
        for (self.opaque_atomic_proofs) |entry| {
            if (canonicalNominalTypeKeyEql(entry.nominal, nominal) and
                canonicalTypeKeySliceEql(entry.instantiated_args, instantiated_args))
            {
                return entry;
            }
        }
        return null;
    }

    pub fn opaqueAtomicProof(
        self: *const ModuleInterfaceCapabilities,
        id: OpaqueAtomicProofId,
    ) OpaqueAtomicProofEntry {
        const index: usize = @intFromEnum(id);
        if (index >= self.opaque_atomic_proofs.len) {
            checkedArtifactInvariant("interface capability lookup referenced missing opaque atomic proof", .{});
        }
        return self.opaque_atomic_proofs[index];
    }

    pub fn nominalRepresentationForSource(
        self: *const ModuleInterfaceCapabilities,
        source_ty: canonical.CanonicalTypeKey,
    ) ?ExportedNominalRepresentation {
        for (self.exported_nominal_representations) |entry| {
            if (canonicalTypeKeyEql(entry.source_ty, source_ty)) return entry;
        }
        return null;
    }

    pub fn nominalRepresentationForNominal(
        self: *const ModuleInterfaceCapabilities,
        nominal: canonical.NominalTypeKey,
        instantiated_args: []const canonical.CanonicalTypeKey,
    ) ?ExportedNominalRepresentation {
        for (self.exported_nominal_representations) |entry| {
            const capability = self.boxPayloadCapability(entry.box_payload_capability);
            if (canonicalNominalTypeKeyEql(entry.nominal, nominal) and
                canonicalTypeKeySliceEql(capability.instantiated_args, instantiated_args))
            {
                return entry;
            }
        }
        return null;
    }

    pub fn verifyPublished(self: *const ModuleInterfaceCapabilities) void {
        if (builtin.mode != .Debug) return;

        for (self.boxed_payload_templates, 0..) |entry, i| {
            std.debug.assert(@intFromEnum(entry.id) == i);
            if (entry.is_opaque) {
                for (self.exported_nominal_representations) |representation| {
                    if (representation.box_payload_capability == entry.id) break;
                } else {
                    std.debug.panic("checked artifact invariant violated: opaque boxed-payload capability has no nominal representation row", .{});
                }
            }
        }
        for (self.opaque_atomic_proofs, 0..) |entry, i| {
            std.debug.assert(@intFromEnum(entry.id) == i);
            _ = entry.proof;
        }
        for (self.hosted_representations, 0..) |entry, i| {
            std.debug.assert(@intFromEnum(entry.id) == i);
        }
        for (self.platform_representations, 0..) |entry, i| {
            std.debug.assert(@intFromEnum(entry.id) == i);
        }
        for (self.exported_nominal_representations, 0..) |entry, i| {
            std.debug.assert(@intFromEnum(entry.id) == i);
            const capability_index: usize = @intFromEnum(entry.box_payload_capability);
            if (capability_index >= self.boxed_payload_templates.len) {
                std.debug.panic("checked artifact invariant violated: nominal representation references missing boxed-payload capability", .{});
            }
            const capability = self.boxed_payload_templates[capability_index];
            if (!canonicalNominalTypeKeyEql(entry.nominal, capability.nominal) or
                !canonicalTypeKeyEql(entry.source_ty, capability.source_ty))
            {
                std.debug.panic("checked artifact invariant violated: nominal representation disagrees with boxed-payload capability identity", .{});
            }
            if (entry.opaque_atomic_proof) |proof| {
                const proof_index: usize = @intFromEnum(proof);
                if (proof_index >= self.opaque_atomic_proofs.len) {
                    std.debug.panic("checked artifact invariant violated: nominal representation references missing opaque atomic proof", .{});
                }
                const proof_entry = self.opaque_atomic_proofs[proof_index];
                if (!canonicalNominalTypeKeyEql(entry.nominal, proof_entry.nominal) or
                    !canonicalTypeKeyEql(entry.source_ty, proof_entry.source_ty))
                {
                    std.debug.panic("checked artifact invariant violated: nominal representation disagrees with opaque atomic proof identity", .{});
                }
            }
        }
    }
};

const NominalCapabilitySeenKey = struct {
    source_ty: canonical.CanonicalTypeKey,
    nominal: canonical.NominalTypeKey,
};

fn canonicalTypeKeyEql(a: canonical.CanonicalTypeKey, b: canonical.CanonicalTypeKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn canonicalNominalTypeKeyEql(a: canonical.NominalTypeKey, b: canonical.NominalTypeKey) bool {
    return a.module_name == b.module_name and a.type_name == b.type_name;
}

fn canonicalTypeKeySliceEql(
    a: []const canonical.CanonicalTypeKey,
    b: []const canonical.CanonicalTypeKey,
) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (!canonicalTypeKeyEql(left, right)) return false;
    }
    return true;
}

fn checkedTypeKeyForId(
    checked_types: *const CheckedTypeStore,
    id: CheckedTypeId,
) canonical.CanonicalTypeKey {
    const index: usize = @intFromEnum(id);
    if (index >= checked_types.roots.len) {
        checkedArtifactInvariant("checked type key lookup referenced a missing root", .{});
    }
    return checked_types.roots[index].key;
}

fn checkedTypeKeysForIds(
    allocator: Allocator,
    checked_types: *const CheckedTypeStore,
    ids: []const CheckedTypeId,
) Allocator.Error![]const canonical.CanonicalTypeKey {
    if (ids.len == 0) return &.{};
    const out = try allocator.alloc(canonical.CanonicalTypeKey, ids.len);
    errdefer allocator.free(out);
    for (ids, 0..) |id, i| {
        out[i] = checkedTypeKeyForId(checked_types, id);
    }
    return out;
}

fn checkedTypeHasNoReachableCallableSlots(
    allocator: Allocator,
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active.deinit();
    return try checkedTypeHasNoReachableCallableSlotsInner(checked_types, root, &active);
}

fn checkedTypeHasNoReachableCallableSlotsInner(
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    const index: usize = @intFromEnum(root);
    if (index >= checked_types.payloads.len) {
        checkedArtifactInvariant("callable-slot proof referenced a missing checked type", .{});
    }
    if (active.contains(root)) return true;
    try active.put(root, {});
    defer _ = active.remove(root);

    return switch (checked_types.payloads[index]) {
        .pending => checkedArtifactInvariant("callable-slot proof reached pending checked type", .{}),
        .flex,
        .rigid,
        .function,
        => false,
        .empty_record,
        .empty_tag_union,
        => true,
        .alias => |alias| try checkedTypeHasNoReachableCallableSlotsInner(checked_types, alias.backing, active),
        .nominal => |nominal| blk: {
            if (nominal.builtin) |builtin_nominal| {
                switch (builtin_nominal) {
                    .str,
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
                    .bool,
                    => break :blk true,
                    .list,
                    .box,
                    => {
                        if (nominal.args.len != 1) checkedArtifactInvariant("builtin container nominal had non-unary args", .{});
                        break :blk try checkedTypeHasNoReachableCallableSlotsInner(checked_types, nominal.args[0], active);
                    },
                }
            }
            break :blk try checkedTypeHasNoReachableCallableSlotsInner(checked_types, nominal.backing, active);
        },
        .record => |record| blk: {
            if (!try checkedRecordHasNoReachableCallableSlots(checked_types, record.fields, active)) break :blk false;
            break :blk try checkedTypeHasNoReachableCallableSlotsInner(checked_types, record.ext, active);
        },
        .record_unbound => |fields| try checkedRecordHasNoReachableCallableSlots(checked_types, fields, active),
        .tuple => |items| try checkedTypeSpanHasNoReachableCallableSlots(checked_types, items, active),
        .tag_union => |tag_union| blk: {
            if (!try checkedTagsHaveNoReachableCallableSlots(checked_types, tag_union.tags, active)) break :blk false;
            break :blk try checkedTypeHasNoReachableCallableSlotsInner(checked_types, tag_union.ext, active);
        },
    };
}

fn checkedRecordHasNoReachableCallableSlots(
    checked_types: *const CheckedTypeStore,
    fields: []const CheckedRecordField,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (fields) |field| {
        if (!try checkedTypeHasNoReachableCallableSlotsInner(checked_types, field.ty, active)) return false;
    }
    return true;
}

fn checkedTypeSpanHasNoReachableCallableSlots(
    checked_types: *const CheckedTypeStore,
    items: []const CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (items) |item| {
        if (!try checkedTypeHasNoReachableCallableSlotsInner(checked_types, item, active)) return false;
    }
    return true;
}

fn checkedTagsHaveNoReachableCallableSlots(
    checked_types: *const CheckedTypeStore,
    tags: []const CheckedTag,
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!bool {
    for (tags) |tag| {
        if (!try checkedTypeSpanHasNoReachableCallableSlots(checked_types, tag.args, active)) return false;
    }
    return true;
}

/// Public `ComptimeSchemaId` declaration.
pub const ComptimeSchemaId = enum(u32) { _ };
/// Public `ComptimeValueId` declaration.
pub const ComptimeValueId = enum(u32) { _ };
/// Public `ComptimeRootId` declaration.
pub const ComptimeRootId = enum(u32) { _ };
/// Public `ComptimeDependencySummaryRequestId` declaration.
pub const ComptimeDependencySummaryRequestId = enum(u32) { _ };
/// Public `ConstGraphReificationPlanId` declaration.
pub const ConstGraphReificationPlanId = enum(u32) { _ };

/// Public `CompileTimeRootKind` declaration.
pub const CompileTimeRootKind = enum {
    constant,
    callable_binding,
    expect,
};

/// Public `CompileTimeRootPayload` declaration.
pub const CompileTimeRootPayload = union(enum) {
    pending,
    const_graph: ConstGraphReificationPlanId,
    callable_result: CallableResultPlanId,
    expect,
};

/// Public `CompileTimeRoot` declaration.
pub const CompileTimeRoot = struct {
    id: ComptimeRootId,
    module_idx: u32,
    kind: CompileTimeRootKind,
    source: RootSource,
    pattern: ?CheckedPatternId,
    expr: CheckedExprId,
    checked_type: CheckedTypeId,
    dependency_summary_request: ComptimeDependencySummaryRequestId,
    payload: CompileTimeRootPayload,
};

/// Public `CompileTimeRootTable` declaration.
pub const CompileTimeRootTable = struct {
    roots: []CompileTimeRoot = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypePublication,
        checked_bodies: *const CheckedBodyStore,
        procedure_templates: *const CheckedProcedureTemplateTable,
    ) Allocator.Error!CompileTimeRootTable {
        var roots = std.ArrayList(CompileTimeRoot).empty;
        errdefer roots.deinit(allocator);

        const module_env = module.moduleEnvConst();
        for (module_env.store.sliceStatements(module_env.all_statements)) |statement_idx| {
            const stmt = module_env.store.getStatement(statement_idx);
            if (stmt != .s_expect) continue;
            try appendCompileTimeRoot(&roots, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .expect,
                .source = .{ .statement = statement_idx },
                .pattern = null,
                .expr = checkedExprIdForSource(checked_bodies, stmt.s_expect.body),
                .checked_type = try checkedTypeIdForVar(allocator, module, checked_types, ModuleEnv.varFrom(stmt.s_expect.body)),
                .payload = .expect,
            });
        }

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            if (procedure_templates.lookupByDef(def_idx) != null) continue;

            const source_ty = module.defType(def_idx);
            const is_callable = sourceTypeIsFunction(module, source_ty);
            try appendCompileTimeRoot(&roots, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = if (is_callable) .callable_binding else .constant,
                .source = .{ .def = def_idx },
                .pattern = checkedPatternIdForSource(checked_bodies, def.pattern.idx),
                .expr = checkedExprIdForSource(checked_bodies, def.expr.idx),
                .checked_type = try checkedTypeIdForVar(allocator, module, checked_types, source_ty),
                .payload = .pending,
            });
        }

        return .{ .roots = try roots.toOwnedSlice(allocator) };
    }

    pub fn lookupIdByPattern(self: *const CompileTimeRootTable, pattern: CheckedPatternId) ?ComptimeRootId {
        for (self.roots) |entry| {
            if (entry.pattern != null and entry.pattern.? == pattern) return entry.id;
        }
        return null;
    }

    pub fn root(self: *const CompileTimeRootTable, id: ComptimeRootId) CompileTimeRoot {
        return self.roots[@intFromEnum(id)];
    }

    pub fn fillPayload(
        self: *CompileTimeRootTable,
        id: ComptimeRootId,
        payload: CompileTimeRootPayload,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.roots.len) {
            checkedArtifactInvariant("compile-time root id is out of range", .{});
        }
        verifyCompileTimeRootPayloadMatchesKind(self.roots[index].kind, payload);
        self.roots[index].payload = payload;
    }

    pub fn deinit(self: *CompileTimeRootTable, allocator: Allocator) void {
        allocator.free(self.roots);
        self.* = .{};
    }

    const RootWithoutId = struct {
        module_idx: u32,
        kind: CompileTimeRootKind,
        source: RootSource,
        pattern: ?CheckedPatternId,
        expr: CheckedExprId,
        checked_type: CheckedTypeId,
        payload: CompileTimeRootPayload,
    };

    fn appendCompileTimeRoot(
        roots: *std.ArrayList(CompileTimeRoot),
        allocator: Allocator,
        entry: RootWithoutId,
    ) Allocator.Error!void {
        const id: ComptimeRootId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
        try roots.append(allocator, .{
            .id = id,
            .module_idx = entry.module_idx,
            .kind = entry.kind,
            .source = entry.source,
            .pattern = entry.pattern,
            .expr = entry.expr,
            .checked_type = entry.checked_type,
            .dependency_summary_request = @enumFromInt(@intFromEnum(id)),
            .payload = entry.payload,
        });
    }
};

fn verifyCompileTimeRootPayloadMatchesKind(kind: CompileTimeRootKind, payload: CompileTimeRootPayload) void {
    const matches = switch (kind) {
        .constant => switch (payload) {
            .const_graph => true,
            else => false,
        },
        .callable_binding => switch (payload) {
            .callable_result => true,
            else => false,
        },
        .expect => switch (payload) {
            .expect => true,
            else => false,
        },
    };
    if (matches) return;
    checkedArtifactInvariant("compile-time root payload does not match root kind", .{});
}

fn checkedExprIdForSource(checked_bodies: *const CheckedBodyStore, expr: CIR.Expr.Idx) CheckedExprId {
    return checked_bodies.exprIdForSource(expr) orelse {
        checkedArtifactInvariant(
            "checked artifact publication could not map CIR expression {d} to a checked expression id",
            .{@intFromEnum(expr)},
        );
    };
}

fn checkedPatternIdForSource(checked_bodies: *const CheckedBodyStore, pattern: CIR.Pattern.Idx) CheckedPatternId {
    return checked_bodies.patternIdForSource(pattern) orelse {
        checkedArtifactInvariant(
            "checked artifact publication could not map CIR pattern {d} to a checked pattern id",
            .{@intFromEnum(pattern)},
        );
    };
}

/// Public `FiniteCallableLeafInstance` declaration.
pub const FiniteCallableLeafInstance = struct {
    proc_value: canonical.ProcedureCallableRef,
};

/// Public `ErasedCallableLeafInstance` declaration.
pub const ErasedCallableLeafInstance = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    sig_key: canonical.ErasedFnSigKey,
    provenance: []const BoxErasureProvenance,
    code: canonical.ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

/// Public `CallableLeafInstance` declaration.
pub const CallableLeafInstance = union(enum) {
    finite: FiniteCallableLeafInstance,
    erased_boxed: ErasedCallableLeafInstance,
};

/// Public `CallableLeafReificationPlan` declaration.
pub const CallableLeafReificationPlan = union(enum) {
    finite: CallableResultPlanId,
    erased_boxed: CallableResultPlanId,
    already_resolved: CallableLeafInstance,
};

/// Public `ConstRecordFieldPlan` declaration.
pub const ConstRecordFieldPlan = struct {
    field: canonical.RecordFieldLabelId,
    value: ConstGraphReificationPlanId,
};

/// Public `ConstTupleElemPlan` declaration.
pub const ConstTupleElemPlan = struct {
    index: u32,
    value: ConstGraphReificationPlanId,
};

/// Public `ConstTagPayloadPlan` declaration.
pub const ConstTagPayloadPlan = struct {
    index: u32,
    value: ConstGraphReificationPlanId,
};

/// Public `ConstTagVariantPlan` declaration.
pub const ConstTagVariantPlan = struct {
    tag: canonical.TagLabelId,
    payloads: []const ConstTagPayloadPlan,
};

/// Public `ConstGraphReificationPlan` declaration.
pub const ConstGraphReificationPlan = union(enum) {
    pending,
    scalar: CheckedTypeId,
    string: CheckedTypeId,
    list: struct {
        elem: ConstGraphReificationPlanId,
    },
    box: struct {
        payload: ConstGraphReificationPlanId,
    },
    tuple: []const ConstTupleElemPlan,
    record: []const ConstRecordFieldPlan,
    tag_union: []const ConstTagVariantPlan,
    transparent_alias: struct {
        alias: canonical.NominalTypeKey,
        backing: ConstGraphReificationPlanId,
    },
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: ConstGraphReificationPlanId,
    },
    callable_leaf: CallableLeafReificationPlan,
    callable_schema: canonical.CanonicalTypeKey,
    recursive_ref: ConstGraphReificationPlanId,
};

/// Public `SerializableCaptureLeafPlan` declaration.
pub const SerializableCaptureLeafPlan = struct {
    requested_source_ty: canonical.CanonicalTypeKey,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    schema: ComptimeSchemaId,
    reification_plan: ConstGraphReificationPlanId,
};

/// Public `PrivateCaptureConstMode` declaration.
pub const PrivateCaptureConstMode = enum {
    pure_no_callable_slots,
    general_may_contain_callable_slots,
};

/// Public `PrivateCaptureConstLeaf` declaration.
pub const PrivateCaptureConstLeaf = struct {
    const_ref: ConstRef,
    const_instance: ConstInstanceRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    schema: ComptimeSchemaId,
    mode: PrivateCaptureConstMode,
};

/// Public `CaptureRecordFieldPlan` declaration.
pub const CaptureRecordFieldPlan = struct {
    field: canonical.RecordFieldLabelId,
    value: CaptureSlotReificationPlanId,
};

/// Public `CaptureTupleElemPlan` declaration.
pub const CaptureTupleElemPlan = struct {
    index: u32,
    value: CaptureSlotReificationPlanId,
};

/// Public `CaptureTagPayloadPlan` declaration.
pub const CaptureTagPayloadPlan = struct {
    index: u32,
    value: CaptureSlotReificationPlanId,
};

/// Public `CaptureTagVariantPlan` declaration.
pub const CaptureTagVariantPlan = struct {
    tag: canonical.TagLabelId,
    payloads: []const CaptureTagPayloadPlan,
};

/// Public `PrivateCaptureRecordField` declaration.
pub const PrivateCaptureRecordField = struct {
    field: canonical.RecordFieldLabelId,
    value: PrivateCaptureNodeId,
};

/// Public `PrivateCaptureTagPayload` declaration.
pub const PrivateCaptureTagPayload = struct {
    index: u32,
    value: PrivateCaptureNodeId,
};

/// Public `PrivateCaptureTagNode` declaration.
pub const PrivateCaptureTagNode = struct {
    tag: canonical.TagLabelId,
    payloads: []const PrivateCaptureTagPayload,
};

/// Public `CaptureSlotReificationPlan` declaration.
pub const CaptureSlotReificationPlan = union(enum) {
    pending,
    serializable_leaf: SerializableCaptureLeafPlan,
    callable_leaf: CallableResultPlanId,
    callable_schema: canonical.CanonicalTypeKey,
    record: []const CaptureRecordFieldPlan,
    tuple: []const CaptureTupleElemPlan,
    tag_union: []const CaptureTagVariantPlan,
    list: struct {
        elem: CaptureSlotReificationPlanId,
    },
    box: CaptureSlotReificationPlanId,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: CaptureSlotReificationPlanId,
    },
    recursive_ref: CaptureSlotReificationPlanId,
};

/// Public `CallableResultMemberPlan` declaration.
pub const CallableResultMemberPlan = struct {
    member: canonical.CallableSetMemberId,
    member_proc: canonical.ProcedureCallableRef,
    member_proc_source_fn_ty_payload: CheckedTypeId,
    member_lifted_owner_source_fn_ty_payload: ?CheckedTypeId = null,
    target: CallableResultMemberTargetPlan,
    capture_slots: []const CaptureSlotReificationPlanId,
};

/// Public `FiniteCallableResultPlan` declaration.
pub const FiniteCallableResultPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    members: []const CallableResultMemberPlan,
};

/// Public `ErasedCaptureReificationPlan` declaration.
pub const ErasedCaptureReificationPlan = union(enum) {
    none,
    zero_sized_typed: canonical.CanonicalExecValueTypeKey,
    whole_hidden_capture_value: ErasedCaptureSlotReificationRef,
    proc_capture_tuple: []const ErasedCaptureSlotReificationRef,
    finite_callable_set_value: CallableResultPlanId,
};

/// Public `ErasedCaptureSlotReificationRef` declaration.
pub const ErasedCaptureSlotReificationRef = struct {
    source_ty: canonical.CanonicalTypeKey,
    plan: CaptureSlotReificationPlanId,
};

/// Public `ErasedCallableResultCodePlan` declaration.
pub const ErasedCallableResultCodePlan = union(enum) {
    materialized_by_lowering: canonical.ErasedCallableCodeRef,
    read_from_interpreted_erased_value,
};

/// Public `ErasedCallableResultPlan` declaration.
pub const ErasedCallableResultPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    sig_key: canonical.ErasedFnSigKey,
    provenance: []const BoxErasureProvenance,
    code_plan: ErasedCallableResultCodePlan,
    capture: ErasedCaptureReificationPlan,
    result_ty: canonical.CanonicalExecValueTypeKey,
    executable_signature_payloads: ErasedPromotedProcedureExecutableSignaturePayloads,
};

/// Public `CallableResultPlan` declaration.
pub const CallableResultPlan = union(enum) {
    finite: FiniteCallableResultPlan,
    erased: ErasedCallableResultPlan,
};

/// Public `FiniteCallablePromotionPlan` declaration.
pub const FiniteCallablePromotionPlan = struct {
    result_plan: CallableResultPlanId,
    selected_member: canonical.CallableSetMemberId,
    promoted_proc: PromotedProcedureRef,
};

/// Public `ErasedCallablePromotionPlan` declaration.
pub const ErasedCallablePromotionPlan = struct {
    result_plan: CallableResultPlanId,
    promoted_proc: PromotedProcedureRef,
};

/// Public `CallablePromotionPlan` declaration.
pub const CallablePromotionPlan = union(enum) {
    finite: FiniteCallablePromotionPlan,
    erased: ErasedCallablePromotionPlan,
};

/// Public `PrivateCaptureNode` declaration.
pub const PrivateCaptureNode = union(enum) {
    pending,
    const_instance_leaf: PrivateCaptureConstLeaf,
    finite_callable_leaf: FiniteCallableLeafInstance,
    record: []const PrivateCaptureRecordField,
    tuple: []const PrivateCaptureNodeId,
    tag_union: PrivateCaptureTagNode,
    list: []const PrivateCaptureNodeId,
    box: PrivateCaptureNodeId,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: PrivateCaptureNodeId,
    },
    recursive_ref: PrivateCaptureNodeId,
};

/// Public `CompileTimePlanStore` declaration.
pub const CompileTimePlanStore = struct {
    const_graphs: std.ArrayList(ConstGraphReificationPlan) = .empty,
    callable_results: std.ArrayList(CallableResultPlan) = .empty,
    callable_promotions: std.ArrayList(CallablePromotionPlan) = .empty,
    capture_slots: std.ArrayList(CaptureSlotReificationPlan) = .empty,
    private_captures: std.ArrayList(PrivateCaptureNode) = .empty,
    erased_capture_executable_materialization_nodes: std.ArrayList(ErasedCaptureExecutableMaterializationNode) = .empty,

    pub fn reserveConstGraph(
        self: *CompileTimePlanStore,
        allocator: Allocator,
    ) Allocator.Error!ConstGraphReificationPlanId {
        const id: ConstGraphReificationPlanId = @enumFromInt(@as(u32, @intCast(self.const_graphs.items.len)));
        try self.const_graphs.append(allocator, .pending);
        return id;
    }

    pub fn fillConstGraph(
        self: *CompileTimePlanStore,
        id: ConstGraphReificationPlanId,
        plan: ConstGraphReificationPlan,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.const_graphs.items.len) {
            checkedArtifactInvariant("const graph reification plan id is out of range", .{});
        }
        switch (self.const_graphs.items[index]) {
            .pending => self.const_graphs.items[index] = plan,
            else => checkedArtifactInvariant("const graph reification plan was filled twice", .{}),
        }
    }

    pub fn constGraph(self: *const CompileTimePlanStore, id: ConstGraphReificationPlanId) ConstGraphReificationPlan {
        const index = @intFromEnum(id);
        if (index >= self.const_graphs.items.len) {
            checkedArtifactInvariant("const graph reification plan id is out of range", .{});
        }
        return self.const_graphs.items[index];
    }

    pub fn appendCallableResult(
        self: *CompileTimePlanStore,
        allocator: Allocator,
        plan: CallableResultPlan,
    ) Allocator.Error!CallableResultPlanId {
        const id: CallableResultPlanId = @enumFromInt(@as(u32, @intCast(self.callable_results.items.len)));
        try self.callable_results.append(allocator, plan);
        return id;
    }

    pub fn callableResult(self: *const CompileTimePlanStore, id: CallableResultPlanId) CallableResultPlan {
        const index = @intFromEnum(id);
        if (index >= self.callable_results.items.len) {
            checkedArtifactInvariant("callable result plan id is out of range", .{});
        }
        return self.callable_results.items[index];
    }

    pub fn appendCallablePromotion(
        self: *CompileTimePlanStore,
        allocator: Allocator,
        plan: CallablePromotionPlan,
    ) Allocator.Error!CallablePromotionPlanId {
        const id: CallablePromotionPlanId = @enumFromInt(@as(u32, @intCast(self.callable_promotions.items.len)));
        try self.callable_promotions.append(allocator, plan);
        return id;
    }

    pub fn callablePromotion(self: *const CompileTimePlanStore, id: CallablePromotionPlanId) CallablePromotionPlan {
        const index = @intFromEnum(id);
        if (index >= self.callable_promotions.items.len) {
            checkedArtifactInvariant("callable promotion plan id is out of range", .{});
        }
        return self.callable_promotions.items[index];
    }

    pub fn appendCaptureSlot(
        self: *CompileTimePlanStore,
        allocator: Allocator,
        plan: CaptureSlotReificationPlan,
    ) Allocator.Error!CaptureSlotReificationPlanId {
        const id: CaptureSlotReificationPlanId = @enumFromInt(@as(u32, @intCast(self.capture_slots.items.len)));
        try self.capture_slots.append(allocator, plan);
        return id;
    }

    pub fn reserveCaptureSlot(
        self: *CompileTimePlanStore,
        allocator: Allocator,
    ) Allocator.Error!CaptureSlotReificationPlanId {
        return try self.appendCaptureSlot(allocator, .pending);
    }

    pub fn fillCaptureSlot(
        self: *CompileTimePlanStore,
        id: CaptureSlotReificationPlanId,
        plan: CaptureSlotReificationPlan,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.capture_slots.items.len) {
            checkedArtifactInvariant("capture slot reification plan id is out of range", .{});
        }
        switch (plan) {
            .pending => checkedArtifactInvariant("cannot fill capture slot reification plan with pending", .{}),
            else => {},
        }
        switch (self.capture_slots.items[index]) {
            .pending => self.capture_slots.items[index] = plan,
            else => checkedArtifactInvariant("capture slot reification plan was filled twice", .{}),
        }
    }

    pub fn captureSlot(self: *const CompileTimePlanStore, id: CaptureSlotReificationPlanId) CaptureSlotReificationPlan {
        const index = @intFromEnum(id);
        if (index >= self.capture_slots.items.len) {
            checkedArtifactInvariant("capture slot reification plan id is out of range", .{});
        }
        return self.capture_slots.items[index];
    }

    pub fn appendPrivateCapture(
        self: *CompileTimePlanStore,
        allocator: Allocator,
        node: PrivateCaptureNode,
    ) Allocator.Error!PrivateCaptureNodeId {
        const id: PrivateCaptureNodeId = @enumFromInt(@as(u32, @intCast(self.private_captures.items.len)));
        try self.private_captures.append(allocator, node);
        return id;
    }

    pub fn reservePrivateCapture(
        self: *CompileTimePlanStore,
        allocator: Allocator,
    ) Allocator.Error!PrivateCaptureNodeId {
        return try self.appendPrivateCapture(allocator, .pending);
    }

    pub fn fillPrivateCapture(
        self: *CompileTimePlanStore,
        id: PrivateCaptureNodeId,
        node: PrivateCaptureNode,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.private_captures.items.len) {
            checkedArtifactInvariant("private capture node id is out of range", .{});
        }
        switch (node) {
            .pending => checkedArtifactInvariant("cannot fill private capture node with pending", .{}),
            else => {},
        }
        switch (self.private_captures.items[index]) {
            .pending => self.private_captures.items[index] = node,
            else => checkedArtifactInvariant("private capture node was filled twice", .{}),
        }
    }

    pub fn privateCapture(self: *const CompileTimePlanStore, id: PrivateCaptureNodeId) PrivateCaptureNode {
        const index = @intFromEnum(id);
        if (index >= self.private_captures.items.len) {
            checkedArtifactInvariant("private capture node id is out of range", .{});
        }
        return self.private_captures.items[index];
    }

    pub fn appendErasedCaptureExecutableMaterializationNode(
        self: *CompileTimePlanStore,
        allocator: Allocator,
        node: ErasedCaptureExecutableMaterializationNode,
    ) Allocator.Error!ErasedCaptureExecutableMaterializationNodeId {
        const id: ErasedCaptureExecutableMaterializationNodeId = @enumFromInt(@as(u32, @intCast(self.erased_capture_executable_materialization_nodes.items.len)));
        try self.erased_capture_executable_materialization_nodes.append(allocator, node);
        return id;
    }

    pub fn reserveErasedCaptureExecutableMaterializationNode(
        self: *CompileTimePlanStore,
        allocator: Allocator,
    ) Allocator.Error!ErasedCaptureExecutableMaterializationNodeId {
        return try self.appendErasedCaptureExecutableMaterializationNode(allocator, .pending);
    }

    pub fn fillErasedCaptureExecutableMaterializationNode(
        self: *CompileTimePlanStore,
        id: ErasedCaptureExecutableMaterializationNodeId,
        node: ErasedCaptureExecutableMaterializationNode,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.erased_capture_executable_materialization_nodes.items.len) {
            checkedArtifactInvariant("erased capture materialization node id is out of range", .{});
        }
        switch (node) {
            .pending => checkedArtifactInvariant("cannot fill erased capture materialization node with pending", .{}),
            else => {},
        }
        switch (self.erased_capture_executable_materialization_nodes.items[index]) {
            .pending => self.erased_capture_executable_materialization_nodes.items[index] = node,
            else => checkedArtifactInvariant("erased capture materialization node was filled twice", .{}),
        }
    }

    pub fn erasedCaptureExecutableMaterializationNode(
        self: *const CompileTimePlanStore,
        id: ErasedCaptureExecutableMaterializationNodeId,
    ) ErasedCaptureExecutableMaterializationNode {
        const index = @intFromEnum(id);
        if (index >= self.erased_capture_executable_materialization_nodes.items.len) {
            checkedArtifactInvariant("erased capture materialization node id is out of range", .{});
        }
        return self.erased_capture_executable_materialization_nodes.items[index];
    }

    pub fn deinit(self: *CompileTimePlanStore, allocator: Allocator) void {
        for (self.const_graphs.items) |*plan| deinitConstGraphReificationPlan(allocator, plan);
        for (self.callable_results.items) |*plan| deinitCallableResultPlan(allocator, plan);
        for (self.capture_slots.items) |*plan| deinitCaptureSlotReificationPlan(allocator, plan);
        for (self.private_captures.items) |*node| deinitPrivateCaptureNode(allocator, node);
        for (self.erased_capture_executable_materialization_nodes.items) |*node| deinitErasedCaptureExecutableMaterializationNode(allocator, node);
        self.erased_capture_executable_materialization_nodes.deinit(allocator);
        self.private_captures.deinit(allocator);
        self.capture_slots.deinit(allocator);
        self.callable_promotions.deinit(allocator);
        self.callable_results.deinit(allocator);
        self.const_graphs.deinit(allocator);
        self.* = .{};
    }

    pub fn verifySealed(
        self: *const CompileTimePlanStore,
        checked_types: *const CheckedTypeStore,
        callable_set_descriptors: *const CallableSetDescriptorStore,
    ) void {
        if (builtin.mode != .Debug) return;

        for (self.const_graphs.items) |plan| verifyConstGraphReificationPlan(self, plan);
        for (self.callable_results.items) |plan| verifyCallableResultPlan(self, checked_types, plan);
        for (self.callable_promotions.items) |plan| verifyCallablePromotionPlan(self, plan);
        for (self.capture_slots.items) |plan| verifyCaptureSlotReificationPlan(self, plan);
        for (self.private_captures.items) |node| verifyPrivateCaptureNode(self, callable_set_descriptors, node);
        for (self.erased_capture_executable_materialization_nodes.items) |node| verifyErasedCaptureExecutableMaterializationNode(self, callable_set_descriptors, node);
    }
};

fn deinitConstGraphReificationPlan(allocator: Allocator, plan: *ConstGraphReificationPlan) void {
    switch (plan.*) {
        .pending,
        .scalar,
        .string,
        .list,
        .box,
        .transparent_alias,
        .nominal,
        .callable_schema,
        .recursive_ref,
        => {},
        .callable_leaf => |*leaf| deinitCallableLeafReificationPlan(allocator, leaf),
        .tuple => |items| allocator.free(items),
        .record => |fields| allocator.free(fields),
        .tag_union => |variants| {
            for (variants) |variant| allocator.free(variant.payloads);
            allocator.free(variants);
        },
    }
}

fn deinitCallableResultPlan(allocator: Allocator, plan: *CallableResultPlan) void {
    switch (plan.*) {
        .finite => |finite| {
            for (finite.members) |member| {
                var target = member.target;
                deinitCallableResultMemberTargetPlan(allocator, &target);
                allocator.free(member.capture_slots);
            }
            allocator.free(finite.members);
        },
        .erased => |erased| {
            var payloads = erased.executable_signature_payloads;
            deinitErasedPromotedProcedureExecutableSignaturePayloads(allocator, &payloads);
            allocator.free(erased.provenance);
            deinitErasedCaptureReificationPlan(allocator, erased.capture);
        },
    }
}

fn deinitCallableResultMemberTargetPlan(
    allocator: Allocator,
    target: *CallableResultMemberTargetPlan,
) void {
    switch (target.*) {
        .artifact_owned => |*key| deinitExecutableSpecializationKey(allocator, key),
        .member_proc_relative => |endpoint| if (endpoint.exec_arg_tys.len != 0) allocator.free(endpoint.exec_arg_tys),
    }
}

fn deinitCaptureSlotReificationPlan(allocator: Allocator, plan: *CaptureSlotReificationPlan) void {
    switch (plan.*) {
        .pending,
        .serializable_leaf,
        .callable_leaf,
        .callable_schema,
        .box,
        .nominal,
        .recursive_ref,
        => {},
        .record => |fields| allocator.free(fields),
        .tuple => |items| allocator.free(items),
        .tag_union => |variants| {
            for (variants) |variant| allocator.free(variant.payloads);
            allocator.free(variants);
        },
        .list => {},
    }
}

fn deinitPrivateCaptureNode(allocator: Allocator, node: *PrivateCaptureNode) void {
    switch (node.*) {
        .pending,
        .const_instance_leaf,
        .finite_callable_leaf,
        .box,
        .nominal,
        .recursive_ref,
        => {},
        .record => |fields| allocator.free(fields),
        .tuple => |items| allocator.free(items),
        .tag_union => |tag| allocator.free(tag.payloads),
        .list => |items| allocator.free(items),
    }
}

fn deinitCallableLeafReificationPlan(allocator: Allocator, leaf: *CallableLeafReificationPlan) void {
    switch (leaf.*) {
        .finite,
        .erased_boxed,
        => {},
        .already_resolved => |*instance| deinitCallableLeafInstance(allocator, instance),
    }
}

fn deinitCallableLeafInstance(allocator: Allocator, leaf: *CallableLeafInstance) void {
    switch (leaf.*) {
        .finite => {},
        .erased_boxed => |erased| {
            allocator.free(erased.provenance);
            deinitErasedCaptureExecutableMaterializationPlan(allocator, erased.capture);
        },
    }
}

fn deinitErasedCaptureReificationPlan(allocator: Allocator, capture: ErasedCaptureReificationPlan) void {
    switch (capture) {
        .none,
        .zero_sized_typed,
        .whole_hidden_capture_value,
        .finite_callable_set_value,
        => {},
        .proc_capture_tuple => |values| allocator.free(values),
    }
}

fn deinitErasedCaptureExecutableMaterializationPlan(_: Allocator, capture: ErasedCaptureExecutableMaterializationPlan) void {
    switch (capture) {
        .none,
        .zero_sized_typed,
        .node,
        => {},
    }
}

fn deinitErasedHiddenCaptureArgPlan(allocator: Allocator, hidden: ErasedHiddenCaptureArgPlan) void {
    switch (hidden) {
        .none => {},
        .materialized_capture => |capture| deinitErasedCaptureExecutableMaterializationPlan(allocator, capture),
    }
}

fn deinitMaterializedFiniteCallableSetValue(allocator: Allocator, finite: *MaterializedFiniteCallableSetValue) void {
    allocator.free(finite.captures);
}

fn deinitMaterializedErasedCallableValue(allocator: Allocator, erased: *MaterializedErasedCallableValue) void {
    allocator.free(erased.provenance);
    deinitErasedCaptureExecutableMaterializationPlan(allocator, erased.capture);
}

fn deinitErasedCaptureExecutableMaterializationNode(allocator: Allocator, node: *ErasedCaptureExecutableMaterializationNode) void {
    switch (node.*) {
        .pending,
        .const_instance,
        .pure_const,
        .pure_value,
        .box,
        .nominal,
        .recursive_ref,
        => {},
        .finite_callable_set => |*finite| deinitMaterializedFiniteCallableSetValue(allocator, finite),
        .erased_callable => |*erased| deinitMaterializedErasedCallableValue(allocator, erased),
        .record => |fields| allocator.free(fields),
        .tuple => |items| allocator.free(items),
        .tag_union => |tag| allocator.free(tag.payloads),
        .list => |items| allocator.free(items),
    }
}

fn deinitExecutableTypePayload(allocator: Allocator, payload: *ExecutableTypePayload) void {
    switch (payload.*) {
        .pending,
        .primitive,
        .list,
        .box,
        .nominal,
        .erased_fn,
        .vacant_callable_slot,
        .recursive_ref,
        => {},
        .record => |fields| allocator.free(fields),
        .tuple => |items| allocator.free(items),
        .tag_union => |variants| {
            for (variants) |variant| allocator.free(variant.payloads);
            allocator.free(variants);
        },
        .callable_set => |callable_set| allocator.free(callable_set.members),
    }
    payload.* = .pending;
}

fn deinitValueTransformProvenance(allocator: Allocator, provenance: ValueTransformProvenance) void {
    switch (provenance) {
        .none => {},
        .box_erasure => |boundaries| allocator.free(boundaries),
    }
}

fn deinitCallableToErasedTransformPlan(allocator: Allocator, plan: CallableToErasedTransformPlan) void {
    switch (plan) {
        .finite_value => |finite| deinitPublishedFiniteSetEraseAdapterBranches(allocator, finite.adapter_branches),
        .proc_value => |proc| deinitErasedCaptureExecutableMaterializationPlan(allocator, proc.capture),
    }
}

fn deinitExecutableValueTransformPlan(allocator: Allocator, plan: *ExecutableValueTransformPlan) void {
    deinitValueTransformProvenance(allocator, plan.provenance);
    switch (plan.op) {
        .identity,
        .structural_bridge,
        .nominal,
        .list,
        .box_payload,
        .already_erased_callable,
        => {},
        .record => |fields| allocator.free(fields),
        .tuple => |items| allocator.free(items),
        .tag_union => |tags| {
            for (tags) |tag| allocator.free(tag.payloads);
            allocator.free(tags);
        },
        .callable_to_erased => |callable| deinitCallableToErasedTransformPlan(allocator, callable),
    }
}

fn deinitExecutableSpecializationKey(allocator: Allocator, key: *canonical.ExecutableSpecializationKey) void {
    allocator.free(key.exec_arg_tys);
    key.exec_arg_tys = &.{};
}

fn deinitErasedPromotedProcedureExecutableSignaturePayloads(
    allocator: Allocator,
    payloads: *ErasedPromotedProcedureExecutableSignaturePayloads,
) void {
    allocator.free(payloads.param_exec_tys);
    allocator.free(payloads.param_exec_ty_keys);
    allocator.free(payloads.erased_call_args);
    allocator.free(payloads.erased_call_arg_keys);
    payloads.param_exec_tys = &.{};
    payloads.param_exec_ty_keys = &.{};
    payloads.erased_call_args = &.{};
    payloads.erased_call_arg_keys = &.{};
}

fn deinitErasedPromotedProcedureExecutableSignature(
    allocator: Allocator,
    signature: *ErasedPromotedProcedureExecutableSignature,
) void {
    deinitExecutableSpecializationKey(allocator, &signature.specialization_key);
    allocator.free(signature.wrapper_params);
    allocator.free(signature.erased_call_args);
    allocator.free(signature.erased_call_arg_keys);
    signature.wrapper_params = &.{};
    signature.erased_call_args = &.{};
    signature.erased_call_arg_keys = &.{};
}

fn deinitPromotedCallableBodyPlan(allocator: Allocator, plan: *PromotedCallableBodyPlan) void {
    switch (plan.*) {
        .pending => {},
        .finite => |finite| {
            var member_target = finite.member_target;
            deinitCallableResultMemberTargetPlan(allocator, &member_target);
            allocator.free(finite.member_capture_slots);
            allocator.free(finite.captures);
            allocator.free(finite.params);
            allocator.free(finite.call_args);
        },
        .erased => |erased| {
            var signature = erased.executable_signature;
            deinitErasedPromotedProcedureExecutableSignature(allocator, &signature);
            deinitExecutableSpecializationKeySlice(allocator, erased.finite_adapter_member_targets);
            deinitPublishedFiniteSetEraseAdapterBranches(allocator, erased.finite_adapter_branches);
            allocator.free(erased.params);
            allocator.free(erased.arg_transforms);
            allocator.free(erased.provenance);
            deinitErasedCaptureExecutableMaterializationPlan(allocator, erased.capture);
            deinitErasedHiddenCaptureArgPlan(allocator, erased.hidden_capture_arg);
        },
    }
}

fn verifyConstGraphRef(store: *const CompileTimePlanStore, id: ConstGraphReificationPlanId) void {
    std.debug.assert(@intFromEnum(id) < store.const_graphs.items.len);
}

fn verifyCallableResultRef(store: *const CompileTimePlanStore, id: CallableResultPlanId) void {
    std.debug.assert(@intFromEnum(id) < store.callable_results.items.len);
}

fn verifyCallablePromotionRef(store: *const CompileTimePlanStore, id: CallablePromotionPlanId) void {
    std.debug.assert(@intFromEnum(id) < store.callable_promotions.items.len);
}

fn verifyCaptureSlotRef(store: *const CompileTimePlanStore, id: CaptureSlotReificationPlanId) void {
    std.debug.assert(@intFromEnum(id) < store.capture_slots.items.len);
}

fn verifyPrivateCaptureRef(store: *const CompileTimePlanStore, id: PrivateCaptureNodeId) void {
    std.debug.assert(@intFromEnum(id) < store.private_captures.items.len);
}

fn verifyErasedCaptureExecutableMaterializationRef(store: *const CompileTimePlanStore, id: ErasedCaptureExecutableMaterializationNodeId) void {
    std.debug.assert(@intFromEnum(id) < store.erased_capture_executable_materialization_nodes.items.len);
}

fn verifyPrivateCaptureHandle(store: *const CompileTimePlanStore, ref: PrivateCaptureRef) void {
    verifyPrivateCaptureRef(store, ref.node);
}

fn canonicalCallableSetKeyEql(a: canonical.CanonicalCallableSetKey, b: canonical.CanonicalCallableSetKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn canonicalExecValueTypeKeyEql(a: canonical.CanonicalExecValueTypeKey, b: canonical.CanonicalExecValueTypeKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn captureShapeKeyEql(a: canonical.CaptureShapeKey, b: canonical.CaptureShapeKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn callableSetCaptureSlotEql(a: canonical.CallableSetCaptureSlot, b: canonical.CallableSetCaptureSlot) bool {
    return a.slot == b.slot and
        std.mem.eql(u8, &a.source_ty.bytes, &b.source_ty.bytes) and
        canonicalExecValueTypeKeyEql(a.exec_value_ty, b.exec_value_ty);
}

fn callableSetMemberEql(a: canonical.CanonicalCallableSetMember, b: canonical.CanonicalCallableSetMember) bool {
    if (a.member != b.member) return false;
    if (!canonical.procedureCallableRefEql(a.proc_value, b.proc_value)) return false;
    if (!canonical.mirProcedureRefEql(a.source_proc, b.source_proc)) return false;
    if (!captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key)) return false;
    if (a.capture_slots.len != b.capture_slots.len) return false;
    for (a.capture_slots, b.capture_slots) |left, right| {
        if (!callableSetCaptureSlotEql(left, right)) return false;
    }
    return true;
}

fn canonicalCallableSetDescriptorEql(a: canonical.CanonicalCallableSetDescriptor, b: canonical.CanonicalCallableSetDescriptor) bool {
    if (!canonicalCallableSetKeyEql(a.key, b.key)) return false;
    if (a.members.len != b.members.len) return false;
    for (a.members, b.members) |left, right| {
        if (!callableSetMemberEql(left, right)) return false;
    }
    return true;
}

fn verifyCallableSetDescriptor(descriptor: canonical.CanonicalCallableSetDescriptor) void {
    if (descriptor.members.len == 0) {
        std.debug.panic("checked artifact invariant violated: callable-set descriptor has no members", .{});
    }
    for (descriptor.members, 0..) |member, i| {
        if (@as(usize, @intFromEnum(member.member)) != i) {
            std.debug.panic("checked artifact invariant violated: callable-set descriptor members are not dense canonical ids", .{});
        }
        for (member.capture_slots, 0..) |slot, slot_index| {
            if (@as(usize, slot.slot) != slot_index) {
                std.debug.panic("checked artifact invariant violated: callable-set descriptor capture slots are not canonical", .{});
            }
        }
    }
}

fn verifyErasedCaptureSlotReificationRef(store: *const CompileTimePlanStore, ref: ErasedCaptureSlotReificationRef) void {
    _ = ref.source_ty;
    verifyCaptureSlotRef(store, ref.plan);
}

fn verifyConstGraphReificationPlan(
    store: *const CompileTimePlanStore,
    plan: ConstGraphReificationPlan,
) void {
    switch (plan) {
        .pending => std.debug.panic("checked artifact invariant violated: published const graph plan is pending", .{}),
        .scalar,
        .string,
        => {},
        .list => |list| verifyConstGraphRef(store, list.elem),
        .box => |box| verifyConstGraphRef(store, box.payload),
        .tuple => |items| for (items) |item| verifyConstGraphRef(store, item.value),
        .record => |fields| for (fields) |field| verifyConstGraphRef(store, field.value),
        .tag_union => |variants| for (variants) |variant| {
            for (variant.payloads) |payload| verifyConstGraphRef(store, payload.value);
        },
        .transparent_alias => |alias| verifyConstGraphRef(store, alias.backing),
        .nominal => |nominal| verifyConstGraphRef(store, nominal.backing),
        .callable_leaf => |leaf| switch (leaf) {
            .finite,
            .erased_boxed,
            => |result| verifyCallableResultRef(store, result),
            .already_resolved => |resolved| verifyCallableLeafInstance(store, resolved),
        },
        .callable_schema => {},
        .recursive_ref => |ref| verifyConstGraphRef(store, ref),
    }
}

fn verifyCallableResultPlan(
    store: *const CompileTimePlanStore,
    checked_types: *const CheckedTypeStore,
    plan: CallableResultPlan,
) void {
    switch (plan) {
        .finite => |finite| {
            if (finite.members.len == 0) {
                std.debug.panic("checked artifact invariant violated: finite callable result plan has no members", .{});
            }
            for (finite.members) |member| {
                verifyCheckedTypePayloadKey(checked_types, member.member_proc_source_fn_ty_payload, member.member_proc.source_fn_ty, "finite callable result member proc source type payload differs from member proc source type");
                switch (member.member_proc.template) {
                    .lifted => |lifted| {
                        const owner_payload = member.member_lifted_owner_source_fn_ty_payload orelse {
                            std.debug.panic("checked artifact invariant violated: finite callable result lifted member has no owner source type payload", .{});
                        };
                        verifyCheckedTypePayloadKey(checked_types, owner_payload, lifted.owner_mono_specialization.requested_mono_fn_ty, "finite callable result lifted owner source type payload differs from owner specialization source type");
                    },
                    .checked, .synthetic => {
                        if (member.member_lifted_owner_source_fn_ty_payload != null) {
                            std.debug.panic("checked artifact invariant violated: non-lifted finite callable result member carried lifted owner source type payload", .{});
                        }
                    },
                }
                if (!std.mem.eql(u8, &callableResultMemberTargetSourceTy(member.target).bytes, &finite.source_fn_ty.bytes)) {
                    std.debug.panic("checked artifact invariant violated: finite callable result member target source type differs from result plan", .{});
                }
                for (member.capture_slots) |capture| verifyCaptureSlotRef(store, capture);
            }
        },
        .erased => |erased| {
            if (erased.provenance.len == 0) {
                std.debug.panic("checked artifact invariant violated: erased callable result plan has no Box(T) provenance", .{});
            }
            if (!std.mem.eql(u8, &erased.source_fn_ty.bytes, &erased.sig_key.source_fn_ty.bytes)) {
                std.debug.panic("checked artifact invariant violated: erased callable result source type differs from signature source type", .{});
            }
            if (!std.mem.eql(u8, &erased.executable_signature_payloads.source_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
                std.debug.panic("checked artifact invariant violated: erased callable result signature payload source type differs from result source type", .{});
            }
            switch (erased.code_plan) {
                .materialized_by_lowering => |code| switch (code) {
                    .direct_proc_value => |direct| {
                        if (!std.mem.eql(u8, &direct.proc_value.source_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
                            std.debug.panic("checked artifact invariant violated: direct erased result code source type differs from result source type", .{});
                        }
                    },
                    .finite_set_adapter => |adapter| {
                        if (!std.mem.eql(u8, &adapter.source_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
                            std.debug.panic("checked artifact invariant violated: finite adapter erased result code source type differs from result source type", .{});
                        }
                        if (!std.mem.eql(u8, &adapter.erased_fn_sig_key.source_fn_ty.bytes, &erased.sig_key.source_fn_ty.bytes) or
                            !std.mem.eql(u8, &adapter.erased_fn_sig_key.abi.bytes, &erased.sig_key.abi.bytes))
                        {
                            std.debug.panic("checked artifact invariant violated: finite adapter erased result code signature differs from result signature", .{});
                        }
                    },
                },
                .read_from_interpreted_erased_value => {},
            }
            switch (erased.capture) {
                .none,
                .zero_sized_typed,
                => {},
                .whole_hidden_capture_value => |value| verifyErasedCaptureSlotReificationRef(store, value),
                .proc_capture_tuple => |values| for (values) |value| verifyErasedCaptureSlotReificationRef(store, value),
                .finite_callable_set_value => |result| verifyCallableResultRef(store, result),
            }
        },
    }
}

fn callableResultMemberTargetSourceTy(target: CallableResultMemberTargetPlan) canonical.CanonicalTypeKey {
    return switch (target) {
        .artifact_owned => |key| key.requested_fn_ty,
        .member_proc_relative => |endpoint| endpoint.requested_fn_ty,
    };
}

fn verifyCheckedTypePayloadKey(
    checked_types: *const CheckedTypeStore,
    ty: CheckedTypeId,
    key: canonical.CanonicalTypeKey,
    comptime message: []const u8,
) void {
    const raw = @intFromEnum(ty);
    if (raw >= checked_types.roots.len) {
        std.debug.panic("checked artifact invariant violated: checked type payload id is out of range", .{});
    }
    if (!std.mem.eql(u8, &checked_types.roots[raw].key.bytes, &key.bytes)) {
        std.debug.panic("checked artifact invariant violated: " ++ message, .{});
    }
}

fn verifyCallablePromotionPlan(store: *const CompileTimePlanStore, plan: CallablePromotionPlan) void {
    switch (plan) {
        .finite => |finite| {
            verifyCallableResultRef(store, finite.result_plan);
            switch (store.callableResult(finite.result_plan)) {
                .finite => |result| {
                    for (result.members) |member| {
                        if (member.member == finite.selected_member) break;
                    } else {
                        std.debug.panic("checked artifact invariant violated: finite callable promotion selects a member outside its result plan", .{});
                    }
                },
                .erased => std.debug.panic("checked artifact invariant violated: finite callable promotion points at an erased result plan", .{}),
            }
        },
        .erased => |erased| {
            verifyCallableResultRef(store, erased.result_plan);
            switch (store.callableResult(erased.result_plan)) {
                .erased => {},
                .finite => std.debug.panic("checked artifact invariant violated: erased callable promotion points at a finite result plan", .{}),
            }
        },
    }
}

fn verifyCaptureSlotReificationPlan(store: *const CompileTimePlanStore, plan: CaptureSlotReificationPlan) void {
    switch (plan) {
        .pending => std.debug.panic("checked artifact invariant violated: published capture slot reification plan is pending", .{}),
        .serializable_leaf => {},
        .callable_leaf => |callable| verifyCallableResultRef(store, callable),
        .callable_schema => {},
        .record => |fields| for (fields) |field| verifyCaptureSlotRef(store, field.value),
        .tuple => |items| for (items) |item| verifyCaptureSlotRef(store, item.value),
        .tag_union => |variants| for (variants) |variant| {
            for (variant.payloads) |payload| verifyCaptureSlotRef(store, payload.value);
        },
        .list => |list| verifyCaptureSlotRef(store, list.elem),
        .box => |payload| verifyCaptureSlotRef(store, payload),
        .nominal => |nominal| verifyCaptureSlotRef(store, nominal.backing),
        .recursive_ref => |ref| verifyCaptureSlotRef(store, ref),
    }
}

fn verifyPrivateCaptureNode(
    store: *const CompileTimePlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
    node: PrivateCaptureNode,
) void {
    _ = callable_set_descriptors;
    switch (node) {
        .pending => std.debug.panic("checked artifact invariant violated: published private capture node is pending", .{}),
        .const_instance_leaf => {},
        .finite_callable_leaf => {},
        .record => |fields| for (fields) |field| verifyPrivateCaptureRef(store, field.value),
        .tuple => |items| for (items) |item| verifyPrivateCaptureRef(store, item),
        .tag_union => |tag| {
            for (tag.payloads) |payload| verifyPrivateCaptureRef(store, payload.value);
        },
        .list => |items| for (items) |item| verifyPrivateCaptureRef(store, item),
        .box => |payload| verifyPrivateCaptureRef(store, payload),
        .nominal => |nominal| verifyPrivateCaptureRef(store, nominal.backing),
        .recursive_ref => |ref| verifyPrivateCaptureRef(store, ref),
    }
}

fn verifyCallableLeafInstance(
    store: *const CompileTimePlanStore,
    leaf: CallableLeafInstance,
) void {
    switch (leaf) {
        .finite => {},
        .erased_boxed => |erased| {
            if (erased.provenance.len == 0) {
                std.debug.panic("checked artifact invariant violated: erased callable leaf has no Box(T) provenance", .{});
            }
            verifyErasedCaptureExecutableMaterializationPlan(store, erased.capture);
        },
    }
}

fn verifyMaterializedFiniteCallableSetValue(
    store: *const CompileTimePlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
    finite: MaterializedFiniteCallableSetValue,
) void {
    const descriptor = callable_set_descriptors.descriptorFor(finite.callable_set_key) orelse {
        std.debug.panic("checked artifact invariant violated: materialized finite erased capture references missing callable-set descriptor", .{});
    };
    var selected: ?canonical.CanonicalCallableSetMember = null;
    for (descriptor.members) |member| {
        if (member.member == finite.selected_member) {
            selected = member;
            break;
        }
    }
    const member = selected orelse {
        std.debug.panic("checked artifact invariant violated: materialized finite erased capture selects missing callable-set member", .{});
    };
    if (!std.mem.eql(u8, &member.proc_value.source_fn_ty.bytes, &finite.source_fn_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: materialized finite erased capture member source type differs from capture source type", .{});
    }
    if (member.capture_slots.len != finite.captures.len) {
        std.debug.panic("checked artifact invariant violated: materialized finite erased capture capture count differs from member schema", .{});
    }
    for (member.capture_slots, 0..) |slot, i| {
        if (slot.slot != i) {
            std.debug.panic("checked artifact invariant violated: materialized finite erased capture slots are not canonical", .{});
        }
    }
    for (finite.captures) |capture| verifyErasedCaptureExecutableMaterializationPlan(store, capture);
}

fn verifyMaterializedErasedCallableValue(
    store: *const CompileTimePlanStore,
    erased: MaterializedErasedCallableValue,
) void {
    if (erased.provenance.len == 0) {
        std.debug.panic("checked artifact invariant violated: materialized erased callable value has no Box(T) provenance", .{});
    }
    verifyErasedCaptureExecutableMaterializationPlan(store, erased.capture);
}

fn verifyErasedCaptureExecutableMaterializationPlan(
    store: *const CompileTimePlanStore,
    capture: ErasedCaptureExecutableMaterializationPlan,
) void {
    switch (capture) {
        .none,
        .zero_sized_typed,
        => {},
        .node => |node| verifyErasedCaptureExecutableMaterializationRef(store, node),
    }
}

fn verifyErasedCaptureExecutableMaterializationNode(
    store: *const CompileTimePlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
    node: ErasedCaptureExecutableMaterializationNode,
) void {
    switch (node) {
        .pending => std.debug.panic("checked artifact invariant violated: published erased capture materialization node is pending", .{}),
        .const_instance => {},
        .pure_const => {},
        .pure_value => |pure| {
            _ = pure.no_reachable_callable_slots;
        },
        .finite_callable_set => |finite| verifyMaterializedFiniteCallableSetValue(store, callable_set_descriptors, finite),
        .erased_callable => |erased| verifyMaterializedErasedCallableValue(store, erased),
        .record => |fields| for (fields) |field| verifyErasedCaptureExecutableMaterializationPlan(store, field.value),
        .tuple => |items| for (items) |item| verifyErasedCaptureExecutableMaterializationPlan(store, item),
        .tag_union => |tag| {
            for (tag.payloads) |payload| verifyErasedCaptureExecutableMaterializationPlan(store, payload.value);
        },
        .list => |items| for (items) |item| verifyErasedCaptureExecutableMaterializationPlan(store, item),
        .box => |payload| verifyErasedCaptureExecutableMaterializationPlan(store, payload),
        .nominal => |nominal| verifyErasedCaptureExecutableMaterializationPlan(store, nominal.backing),
        .recursive_ref => |ref| verifyErasedCaptureExecutableMaterializationRef(store, ref),
    }
}

fn verifyPromotedWrapperArg(store: *const CompileTimePlanStore, arg: PromotedWrapperArg) void {
    switch (arg) {
        .param => {},
        .private_capture => |capture| verifyPrivateCaptureHandle(store, capture),
    }
}

fn verifyExecutableTypePayloadRef(
    payloads: *const ExecutableTypePayloadStore,
    artifact_key: CheckedModuleArtifactKey,
    ref: ExecutableTypePayloadRef,
) void {
    if (!std.mem.eql(u8, &ref.artifact.bytes, &artifact_key.bytes)) {
        std.debug.panic("checked artifact invariant violated: executable type payload ref belongs to a different artifact", .{});
    }
    if (@intFromEnum(ref.payload) >= payloads.entries.len) {
        std.debug.panic("checked artifact invariant violated: executable type payload ref is out of range", .{});
    }
}

fn verifyExecutableTypePayloadRefKey(
    payloads: *const ExecutableTypePayloadStore,
    artifact_key: CheckedModuleArtifactKey,
    ref: ExecutableTypePayloadRef,
    expected_key: canonical.CanonicalExecValueTypeKey,
) void {
    verifyExecutableTypePayloadRef(payloads, artifact_key, ref);
    const actual_key = payloads.keyFor(ref.payload);
    if (!std.mem.eql(u8, &actual_key.bytes, &expected_key.bytes)) {
        std.debug.panic("checked artifact invariant violated: executable type payload ref key differs from endpoint key", .{});
    }
}

fn verifyExecutableTypePayload(
    payloads: *const ExecutableTypePayloadStore,
    artifact_key: CheckedModuleArtifactKey,
    erased_fn_abis: *const canonical.ErasedFnAbiStore,
    payload: ExecutableTypePayload,
) void {
    switch (payload) {
        .pending => std.debug.panic("checked artifact invariant violated: executable type payload was not filled", .{}),
        .primitive => {},
        .record => |fields| for (fields) |field| verifyExecutableTypePayloadRefKey(payloads, artifact_key, field.ty, field.key),
        .tuple => |items| for (items) |item| verifyExecutableTypePayloadRefKey(payloads, artifact_key, item.ty, item.key),
        .tag_union => |variants| for (variants) |variant| {
            for (variant.payloads) |tag_payload| verifyExecutableTypePayloadRefKey(payloads, artifact_key, tag_payload.ty, tag_payload.key);
        },
        .list => |child| verifyExecutableTypePayloadRefKey(payloads, artifact_key, child.ty, child.key),
        .box => |child| verifyExecutableTypePayloadRefKey(payloads, artifact_key, child.ty, child.key),
        .nominal => |nominal| verifyExecutableTypePayloadRefKey(payloads, artifact_key, nominal.backing, nominal.backing_key),
        .vacant_callable_slot => {},
        .callable_set => |callable_set| for (callable_set.members) |member| {
            if ((member.payload_ty == null) != (member.payload_ty_key == null)) {
                std.debug.panic("checked artifact invariant violated: callable-set executable payload member has mismatched payload ref/key presence", .{});
            }
            if (member.payload_ty) |payload_ty| verifyExecutableTypePayloadRefKey(payloads, artifact_key, payload_ty, member.payload_ty_key.?);
        },
        .erased_fn => |erased| {
            const abi = erased_fn_abis.abiFor(erased.sig_key.abi) orelse {
                std.debug.panic("checked artifact invariant violated: erased executable payload signature ABI is not published", .{});
            };
            if ((erased.capture_ty == null) != (erased.capture_ty_key == null)) {
                std.debug.panic("checked artifact invariant violated: erased executable payload has mismatched capture ref/key presence", .{});
            }
            if (erased.capture_ty) |capture| verifyExecutableTypePayloadRefKey(payloads, artifact_key, capture, erased.capture_ty_key.?);
            if (erased.sig_key.capture_ty == null and erased.capture_ty != null) {
                std.debug.panic("checked artifact invariant violated: erased executable payload has capture payload but signature has no capture", .{});
            }
            if (erased.sig_key.capture_ty != null and erased.capture_ty == null) {
                std.debug.panic("checked artifact invariant violated: erased executable payload signature has capture but payload is missing", .{});
            }
            if (abi.capture_arg == null) {
                std.debug.panic("checked artifact invariant violated: erased executable payload ABI has no opaque capture argument", .{});
            }
        },
        .recursive_ref => |ref| {
            if (@intFromEnum(ref) >= payloads.entries.len) {
                std.debug.panic("checked artifact invariant violated: executable recursive type payload ref is out of range", .{});
            }
        },
    }
}

fn verifyErasedPromotedProcedureExecutableSignature(
    payloads: *const ExecutableTypePayloadStore,
    erased_fn_abis: *const canonical.ErasedFnAbiStore,
    artifact_key: CheckedModuleArtifactKey,
    signature: ErasedPromotedProcedureExecutableSignature,
    erased: ErasedPromotedWrapperBodyPlan,
) void {
    const abi = erased_fn_abis.abiFor(erased.sig_key.abi) orelse {
        std.debug.panic("checked artifact invariant violated: erased promoted executable signature ABI is not published", .{});
    };
    if (!std.mem.eql(u8, &signature.source_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable signature source type differs from wrapper source type", .{});
    }
    if (!std.mem.eql(u8, &erased.sig_key.source_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable signature key source type differs from wrapper source type", .{});
    }
    if (!std.mem.eql(u8, &signature.specialization_key.requested_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable specialization source type differs from wrapper source type", .{});
    }
    if (signature.specialization_key.callable_repr_mode != .erased_callable) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable signature was not marked erased-callable", .{});
    }
    if (signature.wrapper_params.len != erased.params.len) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable signature param count differs from wrapper params", .{});
    }
    if (signature.specialization_key.exec_arg_tys.len != signature.wrapper_params.len) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable specialization arg count differs from wrapper params", .{});
    }
    for (signature.wrapper_params, erased.params, signature.specialization_key.exec_arg_tys) |param_payload, wrapper_param, arg_key| {
        if (param_payload.param.index != wrapper_param.index) {
            std.debug.panic("checked artifact invariant violated: erased promoted executable signature param order differs from wrapper params", .{});
        }
        if (!std.mem.eql(u8, &param_payload.exec_ty_key.bytes, &arg_key.bytes)) {
            std.debug.panic("checked artifact invariant violated: erased promoted executable param key differs from specialization key", .{});
        }
        verifyExecutableTypePayloadRefKey(payloads, artifact_key, param_payload.exec_ty, param_payload.exec_ty_key);
    }
    verifyExecutableTypePayloadRefKey(payloads, artifact_key, signature.wrapper_ret, signature.wrapper_ret_key);
    verifyExecutableTypePayloadRefKey(payloads, artifact_key, signature.erased_call_ret, signature.erased_call_ret_key);
    if (signature.erased_call_args.len != signature.erased_call_arg_keys.len) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable erased-call arg refs/keys differ in length", .{});
    }
    for (signature.erased_call_args, signature.erased_call_arg_keys) |arg, arg_key| {
        verifyExecutableTypePayloadRefKey(payloads, artifact_key, arg, arg_key);
    }
    if (!std.mem.eql(u8, &signature.wrapper_ret_key.bytes, &signature.specialization_key.exec_ret_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable wrapper return key differs from specialization key", .{});
    }
    if (signature.erased_call_arg_keys.len != abi.fixed_arity) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable erased-call arity differs from ABI payload", .{});
    }
    for (signature.erased_call_arg_keys, abi.arg_exec_keys) |arg_key, abi_arg_key| {
        if (!std.mem.eql(u8, &arg_key.bytes, &abi_arg_key.bytes)) {
            std.debug.panic("checked artifact invariant violated: erased promoted executable erased-call arg key differs from ABI payload", .{});
        }
    }
    if (!std.mem.eql(u8, &signature.erased_call_ret_key.bytes, &abi.ret_exec_key.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable erased-call return key differs from ABI payload", .{});
    }
    if ((erased.sig_key.capture_ty == null) != (signature.hidden_capture == null)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable hidden capture presence differs from signature key", .{});
    }
    if (abi.capture_arg == null) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable ABI has no opaque capture argument", .{});
    }
    if (signature.hidden_capture) |hidden| {
        const capture_ty = erased.sig_key.capture_ty orelse unreachable;
        if (!std.mem.eql(u8, &hidden.exec_ty_key.bytes, &capture_ty.bytes)) {
            std.debug.panic("checked artifact invariant violated: erased promoted executable hidden capture key differs from signature key", .{});
        }
        verifyExecutableTypePayloadRefKey(payloads, artifact_key, hidden.exec_ty, hidden.exec_ty_key);
    }
}

fn verifyExecutableValueTransformRef(store: *const ExecutableValueTransformPlanStore, transform: ExecutableValueTransformPlanId) void {
    if (@intFromEnum(transform) >= store.plans.len) {
        std.debug.panic("checked artifact invariant violated: executable value transform id is out of range", .{});
    }
}

fn verifyPublishedExecutableValueTransformRef(
    store: *const ExecutableValueTransformPlanStore,
    artifact_key: CheckedModuleArtifactKey,
    transform: PublishedExecutableValueTransformRef,
) void {
    if (!std.mem.eql(u8, &transform.artifact.bytes, &artifact_key.bytes)) {
        std.debug.panic("checked artifact invariant violated: published executable value transform points at a different artifact", .{});
    }
    verifyExecutableValueTransformRef(store, transform.transform);
}

fn verifyValueTransformProvenance(provenance: ValueTransformProvenance) void {
    switch (provenance) {
        .none => {},
        .box_erasure => |boundaries| {
            if (boundaries.len == 0) {
                std.debug.panic("checked artifact invariant violated: executable value transform has empty Box(T) erasure provenance", .{});
            }
        },
    }
}

fn verifyExecutableStructuralBridgePlan(store: *const ExecutableValueTransformPlanStore, plan: ExecutableStructuralBridgePlan) void {
    switch (plan) {
        .direct,
        .zst,
        .list_reinterpret,
        .nominal_reinterpret,
        => {},
        .box_unbox => |child| verifyExecutableValueTransformRef(store, child),
        .box_box => |child| verifyExecutableValueTransformRef(store, child),
        .singleton_to_tag_union => |bridge| if (bridge.value_transform) |payload| verifyExecutableValueTransformRef(store, payload),
        .tag_union_to_singleton => |bridge| if (bridge.value_transform) |payload| verifyExecutableValueTransformRef(store, payload),
    }
}

fn verifyExecutableValueTransformOp(store: *const ExecutableValueTransformPlanStore, op: ExecutableValueTransformOp) void {
    switch (op) {
        .identity,
        .already_erased_callable,
        => {},
        .structural_bridge => |bridge| verifyExecutableStructuralBridgePlan(store, bridge),
        .record => |fields| for (fields) |field| verifyExecutableValueTransformRef(store, field.transform),
        .tuple => |items| for (items) |item| verifyExecutableValueTransformRef(store, item.transform),
        .tag_union => |tags| for (tags) |tag| {
            for (tag.payloads) |payload| verifyExecutableValueTransformRef(store, payload.transform);
        },
        .nominal => |nominal| verifyExecutableValueTransformRef(store, nominal.backing),
        .list => |list| verifyExecutableValueTransformRef(store, list.elem),
        .box_payload => |box| verifyExecutableValueTransformRef(store, box.payload),
        .callable_to_erased => {},
    }
}

fn verifyPromotedCallableBodyPlan(
    store: *const CompileTimePlanStore,
    checked_types: *const CheckedTypeStore,
    executable_type_payloads: *const ExecutableTypePayloadStore,
    executable_value_transforms: *const ExecutableValueTransformPlanStore,
    erased_fn_abis: *const canonical.ErasedFnAbiStore,
    artifact_key: CheckedModuleArtifactKey,
    plan: PromotedCallableBodyPlan,
) void {
    switch (plan) {
        .pending => std.debug.panic("checked artifact invariant violated: published promoted callable body plan is pending", .{}),
        .finite => |finite| {
            verifyCheckedTypePayloadKey(checked_types, finite.member_proc_source_fn_ty_payload, finite.member_proc.source_fn_ty, "finite promoted callable body member proc source type payload differs from member proc source type");
            switch (finite.member_proc.template) {
                .lifted => |lifted| {
                    const owner_payload = finite.member_lifted_owner_source_fn_ty_payload orelse {
                        std.debug.panic("checked artifact invariant violated: finite promoted callable body lifted member has no owner source type payload", .{});
                    };
                    verifyCheckedTypePayloadKey(checked_types, owner_payload, lifted.owner_mono_specialization.requested_mono_fn_ty, "finite promoted callable body lifted owner source type payload differs from owner specialization source type");
                },
                .checked, .synthetic => {
                    if (finite.member_lifted_owner_source_fn_ty_payload != null) {
                        std.debug.panic("checked artifact invariant violated: non-lifted finite promoted callable body carried lifted owner source type payload", .{});
                    }
                },
            }
            if (!std.mem.eql(u8, &callableResultMemberTargetSourceTy(finite.member_target).bytes, &finite.source_fn_ty.bytes)) {
                std.debug.panic("checked artifact invariant violated: finite promoted callable body member target source type differs from wrapper source type", .{});
            }
            if (finite.member_capture_slots.len != finite.captures.len) {
                std.debug.panic("checked artifact invariant violated: finite promoted callable body capture count differs from selected member schema", .{});
            }
            for (finite.member_capture_slots, 0..) |slot, i| {
                if (slot.slot != i) {
                    std.debug.panic("checked artifact invariant violated: finite promoted callable body member capture slots are not canonical slot order", .{});
                }
            }
            for (finite.captures) |capture| verifyPrivateCaptureHandle(store, capture);
            for (finite.call_args) |arg| verifyPromotedWrapperArg(store, arg);
        },
        .erased => |erased| {
            if (erased.provenance.len == 0) {
                std.debug.panic("checked artifact invariant violated: erased promoted callable body has no Box(T) provenance", .{});
            }
            switch (erased.code) {
                .direct_proc_value => {
                    if (erased.finite_adapter_member_targets.len != 0) {
                        std.debug.panic("checked artifact invariant violated: direct erased promoted callable body carried finite adapter member targets", .{});
                    }
                    if (erased.finite_adapter_branches.len != 0) {
                        std.debug.panic("checked artifact invariant violated: direct erased promoted callable body carried finite adapter branches", .{});
                    }
                },
                .finite_set_adapter => {
                    if (erased.finite_adapter_member_targets.len == 0) {
                        std.debug.panic("checked artifact invariant violated: finite erased promoted callable body has no adapter member targets", .{});
                    }
                    if (erased.finite_adapter_branches.len != erased.finite_adapter_member_targets.len) {
                        std.debug.panic("checked artifact invariant violated: finite erased promoted callable body branch count differs from member target count", .{});
                    }
                },
            }
            for (erased.finite_adapter_branches) |branch| {
                for (branch.arg_transforms) |transform| verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, transform);
                for (branch.capture_transforms) |transform| verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, transform);
                verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, branch.result_transform);
            }
            if (erased.arg_transforms.len != erased.params.len) {
                std.debug.panic("checked artifact invariant violated: erased promoted callable arg transform count differs from wrapper params", .{});
            }
            for (erased.arg_transforms) |transform| verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, transform);
            verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, erased.result_transform);
            verifyErasedPromotedProcedureExecutableSignature(
                executable_type_payloads,
                erased_fn_abis,
                artifact_key,
                erased.executable_signature,
                erased,
            );
            verifyErasedCaptureExecutableMaterializationPlan(store, erased.capture);
            switch (erased.hidden_capture_arg) {
                .none => {},
                .materialized_capture => |capture| verifyErasedCaptureExecutableMaterializationPlan(store, capture),
            }
        },
    }
}

/// Public `ConstRef` declaration.
pub const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: ConstOwner,
    template: ConstTemplateId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
};

/// Public `ConstOwner` declaration.
pub const ConstOwner = union(enum) {
    top_level_binding: ConstTopLevelOwner,
    promoted_capture: PromotedCaptureId,
};

/// Public `ConstTopLevelOwner` declaration.
pub const ConstTopLevelOwner = struct {
    module_idx: u32,
    pattern: CheckedPatternId,
};

/// Public `PromotedCaptureId` declaration.
pub const PromotedCaptureId = struct {
    promoted_proc: PromotedProcedureRef,
    capture_index: u32,
};

/// Public `TopLevelValueKind` declaration.
pub const TopLevelValueKind = union(enum) {
    const_ref: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
};

/// Public `TopLevelValueEntry` declaration.
pub const TopLevelValueEntry = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
    source_name: canonical.ExportNameId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    value: TopLevelValueKind,
};

/// Public `TopLevelValueTable` declaration.
pub const TopLevelValueTable = struct {
    entries: []TopLevelValueEntry = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        checked_bodies: *const CheckedBodyStore,
        templates: *const CheckedProcedureTemplateTable,
        callable_eval_templates: *CallableEvalTemplateTable,
        procedure_bindings: *TopLevelProcedureBindingTable,
        const_templates: *ConstTemplateTable,
        artifact_key: CheckedModuleArtifactKey,
        compile_time_roots: *const CompileTimeRootTable,
    ) Allocator.Error!TopLevelValueTable {
        var entries = std.ArrayList(TopLevelValueEntry).empty;
        errdefer entries.deinit(allocator);

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            const checked_pattern = checkedPatternIdForSource(checked_bodies, def.pattern.idx);
            const source_name = try topLevelDefSourceName(module, names, def);
            const source_ty = module.defType(def_idx);
            const source_scheme = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                source_ty,
            );
            const value: TopLevelValueKind = if (templates.lookupByDef(def_idx)) |template| blk: {
                const binding = try procedure_bindings.appendDirect(
                    allocator,
                    source_scheme,
                    .{ .artifact = template.artifact, .proc_base = template.proc_base },
                    template,
                );
                break :blk .{ .procedure_binding = binding };
            } else if (sourceTypeIsFunction(module, source_ty)) blk: {
                const root_id = compile_time_roots.lookupIdByPattern(checked_pattern) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "checked artifact invariant violated: function-valued binding {d} has no compile-time callable root",
                            .{@intFromEnum(def.pattern.idx)},
                        );
                    }
                    unreachable;
                };
                const root = compile_time_roots.root(root_id);
                if (root.kind != .callable_binding) {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "checked artifact invariant violated: function-valued binding {d} mapped to non-callable compile-time root",
                            .{@intFromEnum(def.pattern.idx)},
                        );
                    }
                    unreachable;
                }
                const checked_fn_root = root.checked_type;
                const callable_template = try callable_eval_templates.append(
                    allocator,
                    module.moduleIndex(),
                    checked_pattern,
                    root_id,
                    source_scheme,
                    checked_fn_root,
                );
                const binding = try procedure_bindings.appendCallableEval(
                    allocator,
                    source_scheme,
                    callable_template,
                );
                break :blk .{ .procedure_binding = binding };
            } else .{ .const_ref = try const_templates.reserveTopLevel(
                allocator,
                artifact_key,
                module.moduleIndex(),
                checked_pattern,
                source_scheme,
            ) };

            try entries.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .def = def_idx,
                .pattern = checked_pattern,
                .source_name = source_name,
                .source_scheme = source_scheme,
                .value = value,
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }

    pub fn lookupByPattern(self: *const TopLevelValueTable, pattern: CheckedPatternId) ?TopLevelValueEntry {
        for (self.entries) |entry| {
            if (entry.pattern == pattern) return entry;
        }
        return null;
    }

    pub fn lookupByDef(self: *const TopLevelValueTable, def: CIR.Def.Idx) ?TopLevelValueEntry {
        for (self.entries) |entry| {
            if (entry.def == def) return entry;
        }
        return null;
    }

    pub fn deinit(self: *TopLevelValueTable, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }
};

/// Public `PromotedProcedureProvenance` declaration.
pub const PromotedProcedureProvenance = union(enum) {
    local_callable_root_result: struct {
        root: ComptimeRootId,
        result_plan: CallableResultPlanId,
    },
    local_const_root_callable_leaf: struct {
        root: ComptimeRootId,
        instance: ConstInstantiationKey,
        result_plan: CallableResultPlanId,
        value_path: ComptimeValuePathKey,
    },
    callable_binding_instance_result: struct {
        instance: CallableBindingInstantiationKey,
        result_plan: CallableResultPlanId,
        callable_path: PromotedCallablePathKey,
    },
    const_instance_callable_leaf: struct {
        instance: ConstInstantiationKey,
        result_plan: CallableResultPlanId,
        value_path: ComptimeValuePathKey,
    },
    private_capture_callable_leaf: struct {
        promoted_proc: PromotedProcedureRef,
        result_plan: CallableResultPlanId,
        capture_path: PrivateCapturePathKey,
    },
};

/// Public `PromotedProcedure` declaration.
pub const PromotedProcedure = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    source_binding: ?CheckedPatternId,
    source_fn_ty: canonical.CanonicalTypeKey,
    provenance: PromotedProcedureProvenance,
};

/// Public `ReservedPromotedCallableWrapper` declaration.
pub const ReservedPromotedCallableWrapper = struct {
    promoted_ref: PromotedProcedureRef,
    proc_value: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    wrapper: canonical.PromotedCallableWrapperId,
    body_plan: canonical.PromotedCallableBodyPlanId,
    source_fn_ty: canonical.CanonicalTypeKey,
    provenance: PromotedProcedureProvenance,
};

/// Public `PromotedProcedureTable` declaration.
pub const PromotedProcedureTable = struct {
    procedures: []PromotedProcedure = &.{},

    pub fn append(
        self: *PromotedProcedureTable,
        allocator: Allocator,
        module_idx: u32,
        procedure: PromotedProcedure,
    ) Allocator.Error!PromotedProcedureRef {
        for (self.procedures) |existing| {
            if (canonical.procedureValueRefEql(existing.proc, procedure.proc)) {
                checkedArtifactInvariant("promoted procedure was published twice", .{});
            }
            if (canonical.procedureTemplateRefEql(existing.template, procedure.template)) {
                checkedArtifactInvariant("promoted procedure template was published twice", .{});
            }
        }

        const old = self.procedures;
        const next = try allocator.alloc(PromotedProcedure, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = procedure;
        if (old.len > 0) allocator.free(old);
        self.procedures = next;

        return .{
            .module_idx = module_idx,
            .proc = procedure.proc,
        };
    }

    pub fn get(self: *const PromotedProcedureTable, ref: PromotedProcedureRef) ?PromotedProcedure {
        for (self.procedures) |procedure| {
            if (canonical.procedureValueRefEql(procedure.proc, ref.proc)) return procedure;
        }
        return null;
    }

    pub fn verifyPublished(
        self: *const PromotedProcedureTable,
        artifact_key: CheckedModuleArtifactKey,
        templates: *const CheckedProcedureTemplateTable,
        checked_bodies: *const CheckedBodyStore,
        wrappers: *const PromotedCallableWrapperTable,
    ) void {
        if (builtin.mode != .Debug) return;

        for (self.procedures, 0..) |procedure, i| {
            for (self.procedures[0..i]) |previous| {
                if (canonical.procedureValueRefEql(previous.proc, procedure.proc)) {
                    std.debug.panic("checked artifact invariant violated: promoted procedure value appears more than once", .{});
                }
                if (canonical.procedureTemplateRefEql(previous.template, procedure.template)) {
                    std.debug.panic("checked artifact invariant violated: promoted procedure template appears more than once", .{});
                }
            }
            if (!std.mem.eql(u8, &procedure.proc.artifact.bytes, &artifact_key.bytes)) {
                std.debug.panic("checked artifact invariant violated: promoted procedure value belongs to a different artifact", .{});
            }
            if (!std.mem.eql(u8, &procedure.template.artifact.bytes, &artifact_key.bytes)) {
                std.debug.panic("checked artifact invariant violated: promoted procedure template belongs to a different artifact", .{});
            }
            if (procedure.source_binding) |source_binding| {
                if (@intFromEnum(source_binding) >= checked_bodies.patterns.len) {
                    std.debug.panic("checked artifact invariant violated: promoted procedure source binding is out of range", .{});
                }
            }
            if (procedure.proc.proc_base != procedure.template.proc_base) {
                std.debug.panic("checked artifact invariant violated: promoted procedure proc base differs from its template", .{});
            }
            const template_index = @intFromEnum(procedure.template.template);
            if (template_index >= templates.templates.len) {
                std.debug.panic("checked artifact invariant violated: promoted procedure template id is out of range", .{});
            }
            const template = templates.templates[template_index];
            if (template.proc_base != procedure.template.proc_base) {
                std.debug.panic("checked artifact invariant violated: promoted procedure table references a template with a different proc base", .{});
            }
            switch (template.body) {
                .promoted_callable_wrapper => |wrapper_id| {
                    const wrapper = wrappers.get(wrapper_id);
                    if (!std.mem.eql(u8, &wrapper.source_fn_ty.bytes, &procedure.source_fn_ty.bytes)) {
                        std.debug.panic("checked artifact invariant violated: promoted procedure source function type differs from wrapper", .{});
                    }
                    if (!optionalCheckedPatternIdEql(wrapper.source_binding, procedure.source_binding)) {
                        std.debug.panic("checked artifact invariant violated: promoted procedure source binding differs from wrapper", .{});
                    }
                    if (!promotedProcedureProvenanceEql(wrapper.provenance, procedure.provenance)) {
                        std.debug.panic("checked artifact invariant violated: promoted procedure provenance differs from wrapper", .{});
                    }
                },
                else => std.debug.panic("checked artifact invariant violated: promoted procedure table row does not point at a promoted callable wrapper", .{}),
            }
        }
    }

    pub fn deinit(self: *PromotedProcedureTable, allocator: Allocator) void {
        allocator.free(self.procedures);
        self.* = .{};
    }
};

/// Public `ComptimeWrappedSchema` declaration.
pub const ComptimeWrappedSchema = struct {
    type_name: canonical.NominalTypeKey,
    backing: ComptimeSchemaId,
    is_opaque: bool = false,
};

/// Public `ComptimeFieldSchema` declaration.
pub const ComptimeFieldSchema = struct {
    name: canonical.RecordFieldLabelId,
    schema: ComptimeSchemaId,
};

/// Public `ComptimeVariantSchema` declaration.
pub const ComptimeVariantSchema = struct {
    name: canonical.TagLabelId,
    payloads: []ComptimeSchemaId,
};

/// Public `ComptimeSchema` declaration.
pub const ComptimeSchema = union(enum) {
    pending,
    zst,
    int: types.Int.Precision,
    frac: types.Frac.Precision,
    str,
    list: ComptimeSchemaId,
    box: ComptimeSchemaId,
    tuple: []ComptimeSchemaId,
    record: []ComptimeFieldSchema,
    tag_union: []ComptimeVariantSchema,
    alias: ComptimeWrappedSchema,
    nominal: ComptimeWrappedSchema,
    callable: canonical.CanonicalTypeKey,
};

/// Public `ComptimeVariantValue` declaration.
pub const ComptimeVariantValue = struct {
    variant_index: u32,
    payloads: []ComptimeValueId,
};

/// Public `ComptimeValue` declaration.
pub const ComptimeValue = union(enum) {
    pending,
    zst,
    int_bytes: [16]u8,
    f32: f32,
    f64: f64,
    dec: [16]u8,
    str: []u8,
    list: []ComptimeValueId,
    box: ComptimeValueId,
    tuple: []ComptimeValueId,
    record: []ComptimeValueId,
    tag_union: ComptimeVariantValue,
    alias: ComptimeValueId,
    nominal: ComptimeValueId,
    callable: CallableLeafInstance,
};

/// Public `ComptimeBinding` declaration.
pub const ComptimeBinding = struct {
    pattern: CheckedPatternId,
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
};

/// Public `CompileTimeValueStore` declaration.
pub const CompileTimeValueStore = struct {
    allocator: Allocator,
    schemas: std.ArrayList(ComptimeSchema),
    values: std.ArrayList(ComptimeValue),
    bindings: []ComptimeBinding = &.{},
    by_pattern: std.AutoHashMapUnmanaged(CheckedPatternId, ComptimeBinding) = .{},

    pub fn init(allocator: Allocator) CompileTimeValueStore {
        return .{
            .allocator = allocator,
            .schemas = .empty,
            .values = .empty,
        };
    }

    pub fn addSchema(self: *CompileTimeValueStore, schema: ComptimeSchema) Allocator.Error!ComptimeSchemaId {
        const id: ComptimeSchemaId = @enumFromInt(@as(u32, @intCast(self.schemas.items.len)));
        try self.schemas.append(self.allocator, schema);
        return id;
    }

    pub fn overwriteSchema(self: *CompileTimeValueStore, id: ComptimeSchemaId, schema: ComptimeSchema) void {
        self.deinitSchema(&self.schemas.items[@intFromEnum(id)]);
        self.schemas.items[@intFromEnum(id)] = schema;
    }

    pub fn addValue(self: *CompileTimeValueStore, value: ComptimeValue) Allocator.Error!ComptimeValueId {
        const id: ComptimeValueId = @enumFromInt(@as(u32, @intCast(self.values.items.len)));
        try self.values.append(self.allocator, value);
        return id;
    }

    pub fn overwriteValue(self: *CompileTimeValueStore, id: ComptimeValueId, value: ComptimeValue) void {
        self.deinitValue(&self.values.items[@intFromEnum(id)]);
        self.values.items[@intFromEnum(id)] = value;
    }

    pub fn bind(
        self: *CompileTimeValueStore,
        pattern: CheckedPatternId,
        schema: ComptimeSchemaId,
        value: ComptimeValueId,
    ) Allocator.Error!void {
        try self.by_pattern.put(self.allocator, pattern, .{
            .pattern = pattern,
            .schema = schema,
            .value = value,
        });
    }

    pub fn sealBindings(self: *CompileTimeValueStore) Allocator.Error!void {
        self.allocator.free(self.bindings);
        self.bindings = &.{};

        const count = self.by_pattern.count();
        const bindings = try self.allocator.alloc(ComptimeBinding, count);
        errdefer self.allocator.free(bindings);

        var it = self.by_pattern.valueIterator();
        var i: usize = 0;
        while (it.next()) |binding| : (i += 1) {
            bindings[i] = binding.*;
        }

        std.mem.sort(ComptimeBinding, bindings, {}, struct {
            fn lessThan(_: void, a: ComptimeBinding, b: ComptimeBinding) bool {
                return @intFromEnum(a.pattern) < @intFromEnum(b.pattern);
            }
        }.lessThan);

        self.bindings = bindings;
    }

    pub fn lookupBinding(self: *const CompileTimeValueStore, pattern: CheckedPatternId) ?ComptimeBinding {
        return self.by_pattern.get(pattern);
    }

    pub fn verifySealed(self: *const CompileTimeValueStore) void {
        if (builtin.mode != .Debug) return;

        for (self.schemas.items) |schema| {
            switch (schema) {
                .pending => std.debug.panic(
                    "checked artifact invariant violated: published compile-time schema store contains a pending schema",
                    .{},
                ),
                else => {},
            }
        }

        for (self.values.items) |value| {
            switch (value) {
                .pending => std.debug.panic(
                    "checked artifact invariant violated: published compile-time value store contains a pending value",
                    .{},
                ),
                else => {},
            }
        }

        for (self.bindings) |binding| {
            std.debug.assert(@intFromEnum(binding.schema) < self.schemas.items.len);
            std.debug.assert(@intFromEnum(binding.value) < self.values.items.len);
            const by_pattern = self.by_pattern.get(binding.pattern) orelse unreachable;
            std.debug.assert(by_pattern.schema == binding.schema);
            std.debug.assert(by_pattern.value == binding.value);
        }
    }

    fn deinitSchema(self: *CompileTimeValueStore, schema: *ComptimeSchema) void {
        switch (schema.*) {
            .pending,
            .zst,
            .int,
            .frac,
            .str,
            .list,
            .box,
            .alias,
            .nominal,
            .callable,
            => {},
            .tuple => |items| self.allocator.free(items),
            .record => |fields| self.allocator.free(fields),
            .tag_union => |variants| {
                for (variants) |variant| self.allocator.free(variant.payloads);
                self.allocator.free(variants);
            },
        }
    }

    fn deinitValue(self: *CompileTimeValueStore, value: *ComptimeValue) void {
        switch (value.*) {
            .pending,
            .zst,
            .int_bytes,
            .f32,
            .f64,
            .dec,
            .box,
            .alias,
            .nominal,
            => {},
            .callable => |*leaf| deinitCallableLeafInstance(self.allocator, leaf),
            .str => |bytes| self.allocator.free(bytes),
            .list => |items| self.allocator.free(items),
            .tuple => |items| self.allocator.free(items),
            .record => |items| self.allocator.free(items),
            .tag_union => |variant| self.allocator.free(variant.payloads),
        }
    }

    pub fn deinit(self: *CompileTimeValueStore, _: Allocator) void {
        for (self.values.items) |*value| self.deinitValue(value);
        for (self.schemas.items) |*schema| self.deinitSchema(schema);
        self.allocator.free(self.bindings);
        self.by_pattern.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.schemas.deinit(self.allocator);
        self.* = CompileTimeValueStore.init(self.allocator);
    }
};

/// Public `CheckedCallableBodyRef` declaration.
pub const CheckedCallableBodyRef = enum(u32) { _ };
/// Public `CheckedConstBodyRef` declaration.
pub const CheckedConstBodyRef = enum(u32) { _ };
/// Public `ConstTemplateId` declaration.
pub const ConstTemplateId = enum(u32) { _ };
/// Public `ConstInstanceId` declaration.
pub const ConstInstanceId = enum(u32) { _ };
/// Public `CallableBindingInstanceId` declaration.
pub const CallableBindingInstanceId = enum(u32) { _ };
/// Public `SemanticInstantiationProcedureId` declaration.
pub const SemanticInstantiationProcedureId = enum(u32) { _ };
/// Public `CallableResultPlanId` declaration.
pub const CallableResultPlanId = enum(u32) { _ };
/// Public `CallablePromotionPlanId` declaration.
pub const CallablePromotionPlanId = enum(u32) { _ };
/// Public `ConstReificationPlanId` declaration.
pub const ConstReificationPlanId = ConstGraphReificationPlanId;
/// Public `CaptureSlotReificationPlanId` declaration.
pub const CaptureSlotReificationPlanId = enum(u32) { _ };
/// Public `ErasedCaptureExecutableMaterializationNodeId` declaration.
pub const ErasedCaptureExecutableMaterializationNodeId = enum(u32) { _ };
/// Public `ComptimeDependencySummaryId` declaration.
pub const ComptimeDependencySummaryId = enum(u32) { _ };
/// Public `ComptimeProcDependencySummaryId` declaration.
pub const ComptimeProcDependencySummaryId = enum(u32) { _ };
/// Public `ComptimeCallSiteId` declaration.
pub const ComptimeCallSiteId = enum(u32) { _ };

/// Public `CheckedConstBody` declaration.
pub const CheckedConstBody = struct {
    id: CheckedConstBodyRef,
    root: ComptimeRootId,
    body_expr: CheckedExprId,
    checked_type: CheckedTypeId,
};

/// Public `CheckedConstBodyTable` declaration.
pub const CheckedConstBodyTable = struct {
    bodies: []CheckedConstBody = &.{},
    by_root: []?CheckedConstBodyRef = &.{},

    pub fn fromRoots(
        allocator: Allocator,
        roots: *const CompileTimeRootTable,
    ) Allocator.Error!CheckedConstBodyTable {
        var bodies = std.ArrayList(CheckedConstBody).empty;
        errdefer bodies.deinit(allocator);

        const by_root = try allocator.alloc(?CheckedConstBodyRef, roots.roots.len);
        errdefer allocator.free(by_root);
        @memset(by_root, null);

        for (roots.roots) |root| {
            if (root.kind != .constant) continue;
            const id: CheckedConstBodyRef = @enumFromInt(@as(u32, @intCast(bodies.items.len)));
            try bodies.append(allocator, .{
                .id = id,
                .root = root.id,
                .body_expr = root.expr,
                .checked_type = root.checked_type,
            });
            by_root[@intFromEnum(root.id)] = id;
        }

        return .{
            .bodies = try bodies.toOwnedSlice(allocator),
            .by_root = by_root,
        };
    }

    pub fn bodyForRoot(self: *const CheckedConstBodyTable, root: ComptimeRootId) ?CheckedConstBodyRef {
        const idx = @intFromEnum(root);
        if (idx >= self.by_root.len) return null;
        return self.by_root[idx];
    }

    pub fn get(self: *const CheckedConstBodyTable, id: CheckedConstBodyRef) CheckedConstBody {
        const idx = @intFromEnum(id);
        if (idx >= self.bodies.len) checkedArtifactInvariant("checked const body id is out of range", .{});
        return self.bodies[idx];
    }

    pub fn deinit(self: *CheckedConstBodyTable, allocator: Allocator) void {
        allocator.free(self.bodies);
        allocator.free(self.by_root);
        self.* = .{};
    }
};

/// Public `ComptimeAvailabilityUse` declaration.
pub const ComptimeAvailabilityUse = union(enum) {
    local_root: ComptimeRootId,
    imported_value: TopLevelValueRef,
    const_template: ConstRef,
    procedure_binding: ProcedureBindingRef,
};

/// Public `ProcedureCallableDependency` declaration.
pub const ProcedureCallableDependency = struct {
    proc_value: canonical.ProcedureCallableRef,
    source_fn_ty_payload: CheckedTypeId,
    lifted_owner_source_fn_ty_payload: ?CheckedTypeId = null,
};

/// Public `ComptimeConcreteValueUse` declaration.
pub const ComptimeConcreteValueUse = union(enum) {
    const_instance: ConstInstantiationKey,
    callable_binding_instance: CallableBindingInstantiationKey,
    procedure_callable: canonical.ProcedureCallableRef,
    procedure_callable_with_payloads: ProcedureCallableDependency,
};

/// Public `ComptimeDependencySummary` declaration.
pub const ComptimeDependencySummary = struct {
    availability_values: []const ComptimeAvailabilityUse = &.{},
    concrete_values: []const ComptimeConcreteValueUse = &.{},
};

/// Public `ComptimeCallDependency` declaration.
pub const ComptimeCallDependency = union(enum) {
    call_proc: canonical.ExecutableSpecializationKey,
    call_value_finite: ComptimeFiniteCallValueDependency,
    call_value_erased: ComptimeErasedCallValueDependency,
};

/// Public `ComptimeFiniteCallValueDependency` declaration.
pub const ComptimeFiniteCallValueDependency = struct {
    call_site: ComptimeCallSiteId,
    callable_set: canonical.CanonicalCallableSetKey,
    members: []const canonical.ExecutableSpecializationKey = &.{},
};

/// Public `ComptimeErasedCallValueDependency` declaration.
pub const ComptimeErasedCallValueDependency = struct {
    call_site: ComptimeCallSiteId,
    code: ErasedCallableCodeDependency,
    capture_availability: []const ComptimeAvailabilityUse = &.{},
    capture_concrete_values: []const ComptimeConcreteValueUse = &.{},
    provenance: []const BoxErasureProvenance = &.{},
};

/// Public `ErasedCallableCodeDependency` declaration.
pub const ErasedCallableCodeDependency = union(enum) {
    direct_proc_value: ErasedDirectProcCodeDependency,
    finite_set_adapter: ErasedFiniteAdapterDependency,
    supplied_erased_value: SuppliedErasedValueDependency,
};

/// Public `ErasedDirectProcCodeDependency` declaration.
pub const ErasedDirectProcCodeDependency = struct {
    erase_plan: ProcValueEraseDependencyPlan,
};

/// Public `ProcValueEraseDependencyPlan` declaration.
pub const ProcValueEraseDependencyPlan = struct {
    proc_value: canonical.ProcedureCallableRef,
    erased_fn_sig_key: canonical.ErasedFnSigKey,
    capture_shape_key: canonical.CaptureShapeKey,
    executable_specialization_key: canonical.ExecutableSpecializationKey,
    capture_slots: []const canonical.CallableSetCaptureSlot = &.{},
};

/// Public `ErasedFiniteAdapterDependency` declaration.
pub const ErasedFiniteAdapterDependency = struct {
    adapter_key: canonical.ErasedAdapterKey,
    member_targets: []const canonical.ExecutableSpecializationKey = &.{},
    branches: []const PublishedFiniteSetEraseAdapterBranchPlan = &.{},
};

/// Public `SuppliedErasedValueDependency` declaration.
///
/// This records a call through an already-erased callable value whose concrete
/// code is owned by the value producer, not by the procedure being summarized.
pub const SuppliedErasedValueDependency = struct {
    sig_key: canonical.ErasedFnSigKey,
};

/// Public `ConstGraphDependency` declaration.
pub const ConstGraphDependency = struct {
    plan: ConstGraphReificationPlanId,
    availability_values: []const ComptimeAvailabilityUse = &.{},
    concrete_values: []const ComptimeConcreteValueUse = &.{},
    callable_leaves: []const CallableLeafDependency = &.{},
};

/// Public `CallableResultDependency` declaration.
pub const CallableResultDependency = struct {
    plan: CallableResultPlanId,
    members: []const canonical.ExecutableSpecializationKey = &.{},
    capture_availability: []const ComptimeAvailabilityUse = &.{},
    capture_concrete_values: []const ComptimeConcreteValueUse = &.{},
    erased: ?ErasedCallableDependency = null,
};

/// Public `CallableLeafDependency` declaration.
pub const CallableLeafDependency = union(enum) {
    resolved_finite: FiniteCallableLeafInstance,
    promoted_callable: CallableResultPlanId,
    erased_boxed_callable: ErasedCallableDependency,
};

/// Public `ErasedCallableDependency` declaration.
pub const ErasedCallableDependency = struct {
    code: ErasedCallableCodeDependency,
    capture_availability: []const ComptimeAvailabilityUse = &.{},
    capture_concrete_values: []const ComptimeConcreteValueUse = &.{},
    provenance: []const BoxErasureProvenance = &.{},
};

/// Public `ComptimeProcDependencySummary` declaration.
pub const ComptimeProcDependencySummary = struct {
    proc: canonical.ExecutableSpecializationKey,
    availability_values: []const ComptimeAvailabilityUse = &.{},
    concrete_values: []const ComptimeConcreteValueUse = &.{},
    call_deps: []const ComptimeCallDependency = &.{},
    const_graph_deps: []const ConstGraphDependency = &.{},
    callable_result_deps: []const CallableResultDependency = &.{},
};

/// Public `ComptimeDependencySummaryStoreView` declaration.
pub const ComptimeDependencySummaryStoreView = struct {
    owner: CheckedModuleArtifactKey,
    root_requests: []const ?ComptimeDependencySummaryId = &.{},
    summaries: []const ComptimeDependencySummary = &.{},
    proc_summaries: []const ComptimeProcDependencySummary = &.{},
};

/// Public `ComptimeDependencySummaryStore` declaration.
pub const ComptimeDependencySummaryStore = struct {
    owner: CheckedModuleArtifactKey = .{},
    root_requests: []?ComptimeDependencySummaryId = &.{},
    summaries: std.ArrayList(ComptimeDependencySummary) = .empty,
    proc_summaries: std.ArrayList(ComptimeProcDependencySummary) = .empty,

    pub fn init(owner: CheckedModuleArtifactKey) ComptimeDependencySummaryStore {
        return .{ .owner = owner };
    }

    pub fn view(self: *const ComptimeDependencySummaryStore) ComptimeDependencySummaryStoreView {
        return .{
            .owner = self.owner,
            .root_requests = self.root_requests,
            .summaries = self.summaries.items,
            .proc_summaries = self.proc_summaries.items,
        };
    }

    pub fn reserveRootRequests(
        self: *ComptimeDependencySummaryStore,
        allocator: Allocator,
        count: usize,
    ) Allocator.Error!void {
        if (self.root_requests.len != 0) {
            checkedArtifactInvariant("compile-time dependency root requests were reserved twice", .{});
        }
        if (count == 0) return;
        self.root_requests = try allocator.alloc(?ComptimeDependencySummaryId, count);
        @memset(self.root_requests, null);
    }

    pub fn fillRootRequest(
        self: *ComptimeDependencySummaryStore,
        request: ComptimeDependencySummaryRequestId,
        summary: ComptimeDependencySummaryId,
    ) void {
        const idx = @intFromEnum(request);
        if (idx >= self.root_requests.len) {
            checkedArtifactInvariant("compile-time dependency root request id is out of range", .{});
        }
        if (self.root_requests[idx] != null) {
            checkedArtifactInvariant("compile-time dependency root request was filled twice", .{});
        }
        self.root_requests[idx] = summary;
    }

    pub fn summaryForRootRequest(
        self: *const ComptimeDependencySummaryStore,
        request: ComptimeDependencySummaryRequestId,
    ) ComptimeDependencySummary {
        return self.getSummary(self.summaryIdForRootRequest(request));
    }

    pub fn summaryIdForRootRequest(
        self: *const ComptimeDependencySummaryStore,
        request: ComptimeDependencySummaryRequestId,
    ) ComptimeDependencySummaryId {
        const idx = @intFromEnum(request);
        if (idx >= self.root_requests.len) {
            checkedArtifactInvariant("compile-time dependency root request id is out of range", .{});
        }
        return self.root_requests[idx] orelse {
            checkedArtifactInvariant("compile-time dependency root request was consumed before it was filled", .{});
        };
    }

    pub fn appendSummary(
        self: *ComptimeDependencySummaryStore,
        allocator: Allocator,
        summary: ComptimeDependencySummary,
    ) Allocator.Error!ComptimeDependencySummaryId {
        const id: ComptimeDependencySummaryId = @enumFromInt(@as(u32, @intCast(self.summaries.items.len)));
        const availability_values = try allocator.dupe(ComptimeAvailabilityUse, summary.availability_values);
        errdefer allocator.free(availability_values);
        const concrete_values = try allocator.dupe(ComptimeConcreteValueUse, summary.concrete_values);
        errdefer allocator.free(concrete_values);
        try self.summaries.append(allocator, .{
            .availability_values = availability_values,
            .concrete_values = concrete_values,
        });
        return id;
    }

    /// Append an already-owned callable-aware procedure summary.
    ///
    /// The summary-only MIR-family path allocates the nested slices in this
    /// record. The store takes ownership so it can remain an immutable checked
    /// artifact input for later mono dependency reservation.
    pub fn appendProcSummary(
        self: *ComptimeDependencySummaryStore,
        allocator: Allocator,
        summary: ComptimeProcDependencySummary,
    ) Allocator.Error!ComptimeProcDependencySummaryId {
        const id: ComptimeProcDependencySummaryId = @enumFromInt(@as(u32, @intCast(self.proc_summaries.items.len)));
        try self.proc_summaries.append(allocator, summary);
        return id;
    }

    pub fn getSummary(
        self: *const ComptimeDependencySummaryStore,
        id: ComptimeDependencySummaryId,
    ) ComptimeDependencySummary {
        const idx = @intFromEnum(id);
        if (idx >= self.summaries.items.len) checkedArtifactInvariant("compile-time dependency summary id is out of range", .{});
        return self.summaries.items[idx];
    }

    pub fn getProcSummary(
        self: *const ComptimeDependencySummaryStore,
        id: ComptimeProcDependencySummaryId,
    ) ComptimeProcDependencySummary {
        const idx = @intFromEnum(id);
        if (idx >= self.proc_summaries.items.len) checkedArtifactInvariant("compile-time procedure dependency summary id is out of range", .{});
        return self.proc_summaries.items[idx];
    }

    pub fn verifySealed(self: *const ComptimeDependencySummaryStore) void {
        if (builtin.mode != .Debug) return;

        for (self.summaries.items, 0..) |summary, i| {
            _ = summary;
            std.debug.assert(i <= std.math.maxInt(u32));
        }
        for (self.proc_summaries.items, 0..) |summary, i| {
            _ = summary;
            std.debug.assert(i <= std.math.maxInt(u32));
        }
        for (self.root_requests, 0..) |summary_id, i| {
            std.debug.assert(i <= std.math.maxInt(u32));
            const id = summary_id orelse continue;
            if (@intFromEnum(id) >= self.summaries.items.len) {
                std.debug.panic(
                    "checked artifact invariant violated: compile-time dependency root request {d} references missing summary",
                    .{i},
                );
            }
        }
    }

    pub fn deinit(self: *ComptimeDependencySummaryStore, allocator: Allocator) void {
        for (self.proc_summaries.items) |*summary| deinitComptimeProcDependencySummary(allocator, summary);
        self.proc_summaries.deinit(allocator);
        for (self.summaries.items) |summary| {
            allocator.free(summary.availability_values);
            allocator.free(summary.concrete_values);
        }
        allocator.free(self.root_requests);
        self.summaries.deinit(allocator);
        self.* = .{};
    }
};

fn deinitComptimeProcDependencySummary(
    allocator: Allocator,
    summary: *ComptimeProcDependencySummary,
) void {
    deinitExecutableSpecializationKey(allocator, &summary.proc);
    allocator.free(summary.availability_values);
    allocator.free(summary.concrete_values);
    for (summary.call_deps) |*dep| deinitComptimeCallDependency(allocator, dep);
    allocator.free(summary.call_deps);
    for (summary.const_graph_deps) |*dep| deinitConstGraphDependency(allocator, dep);
    allocator.free(summary.const_graph_deps);
    for (summary.callable_result_deps) |*dep| deinitCallableResultDependency(allocator, dep);
    allocator.free(summary.callable_result_deps);
}

fn deinitComptimeCallDependency(allocator: Allocator, dep: *const ComptimeCallDependency) void {
    switch (dep.*) {
        .call_proc => |key| {
            var owned_key = key;
            deinitExecutableSpecializationKey(allocator, &owned_key);
        },
        .call_value_finite => |finite| deinitExecutableSpecializationKeySlice(allocator, finite.members),
        .call_value_erased => |erased| deinitErasedCallableDependencyFields(
            allocator,
            erased.code,
            erased.capture_availability,
            erased.capture_concrete_values,
            erased.provenance,
        ),
    }
}

fn deinitConstGraphDependency(allocator: Allocator, dep: *const ConstGraphDependency) void {
    allocator.free(dep.availability_values);
    allocator.free(dep.concrete_values);
    for (dep.callable_leaves) |*leaf| deinitCallableLeafDependency(allocator, leaf);
    allocator.free(dep.callable_leaves);
}

fn deinitCallableResultDependency(allocator: Allocator, dep: *const CallableResultDependency) void {
    deinitExecutableSpecializationKeySlice(allocator, dep.members);
    allocator.free(dep.capture_availability);
    allocator.free(dep.capture_concrete_values);
    if (dep.erased) |erased| deinitErasedCallableDependency(allocator, erased);
}

fn deinitCallableLeafDependency(allocator: Allocator, leaf: *const CallableLeafDependency) void {
    switch (leaf.*) {
        .resolved_finite,
        .promoted_callable,
        => {},
        .erased_boxed_callable => |erased| deinitErasedCallableDependency(allocator, erased),
    }
}

fn deinitErasedCallableDependency(allocator: Allocator, erased: ErasedCallableDependency) void {
    deinitErasedCallableDependencyFields(
        allocator,
        erased.code,
        erased.capture_availability,
        erased.capture_concrete_values,
        erased.provenance,
    );
}

fn deinitErasedCallableDependencyFields(
    allocator: Allocator,
    code: ErasedCallableCodeDependency,
    availability: []const ComptimeAvailabilityUse,
    concrete: []const ComptimeConcreteValueUse,
    provenance: []const BoxErasureProvenance,
) void {
    deinitErasedCallableCodeDependency(allocator, code);
    allocator.free(availability);
    allocator.free(concrete);
    allocator.free(provenance);
}

fn deinitErasedCallableCodeDependency(allocator: Allocator, code: ErasedCallableCodeDependency) void {
    switch (code) {
        .direct_proc_value => |direct| deinitProcValueEraseDependencyPlan(allocator, direct.erase_plan),
        .finite_set_adapter => |adapter| {
            deinitExecutableSpecializationKeySlice(allocator, adapter.member_targets);
            deinitPublishedFiniteSetEraseAdapterBranches(allocator, adapter.branches);
        },
        .supplied_erased_value => {},
    }
}

fn deinitPublishedFiniteSetEraseAdapterBranches(
    allocator: Allocator,
    branches: []const PublishedFiniteSetEraseAdapterBranchPlan,
) void {
    for (branches) |branch| {
        var target_key = branch.target_key;
        deinitExecutableSpecializationKey(allocator, &target_key);
        allocator.free(branch.arg_transforms);
        allocator.free(branch.capture_transforms);
    }
    allocator.free(branches);
}

fn deinitProcValueEraseDependencyPlan(allocator: Allocator, plan: ProcValueEraseDependencyPlan) void {
    var key = plan.executable_specialization_key;
    deinitExecutableSpecializationKey(allocator, &key);
    allocator.free(plan.capture_slots);
}

fn deinitExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const canonical.ExecutableSpecializationKey,
) void {
    for (keys) |key| {
        var owned_key = key;
        deinitExecutableSpecializationKey(allocator, &owned_key);
    }
    allocator.free(keys);
}

/// Public `ComptimeValuePathKey` declaration.
pub const ComptimeValuePathKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};
/// Public `PromotedCallablePathKey` declaration.
pub const PromotedCallablePathKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};
/// Public `PrivateCapturePathKey` declaration.
pub const PrivateCapturePathKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};
/// Public `PrivateCaptureId` declaration.
pub const PrivateCaptureId = enum(u32) { _ };
/// Public `PrivateCaptureNodeId` declaration.
pub const PrivateCaptureNodeId = enum(u32) { _ };
/// Public `MethodRegistryEntryRef` declaration.
pub const MethodRegistryEntryRef = enum(u32) { _ };

/// Public `ArtifactCheckedBodyRef` declaration.
pub const ArtifactCheckedBodyRef = struct {
    artifact: CheckedModuleArtifactKey,
    body: CheckedBodyId,
};

/// Public `ArtifactCheckedTypeRef` declaration.
pub const ArtifactCheckedTypeRef = struct {
    artifact: CheckedModuleArtifactKey,
    ty: CheckedTypeId,
};

/// Public `ArtifactCheckedTypeSchemeRef` declaration.
pub const ArtifactCheckedTypeSchemeRef = struct {
    artifact: CheckedModuleArtifactKey,
    scheme: CheckedTypeSchemeId,
};

/// Public `ArtifactCheckedCallableBodyRef` declaration.
pub const ArtifactCheckedCallableBodyRef = struct {
    artifact: CheckedModuleArtifactKey,
    body: CheckedCallableBodyRef,
};

/// Public `ArtifactCheckedConstBodyRef` declaration.
pub const ArtifactCheckedConstBodyRef = struct {
    artifact: CheckedModuleArtifactKey,
    body: CheckedConstBodyRef,
};

pub const ArtifactProcedureTemplateRef = canonical.ProcedureTemplateRef;

/// Public `ArtifactCallableEvalTemplateRef` declaration.
pub const ArtifactCallableEvalTemplateRef = struct {
    artifact: CheckedModuleArtifactKey,
    template: CallableEvalTemplateId,
};

/// Public `ArtifactResolvedValueRefTableRef` declaration.
pub const ArtifactResolvedValueRefTableRef = struct {
    artifact: CheckedModuleArtifactKey,
    table: ResolvedValueRefTableRef,
};

/// Public `ArtifactStaticDispatchPlanTableRef` declaration.
pub const ArtifactStaticDispatchPlanTableRef = struct {
    artifact: CheckedModuleArtifactKey,
    table: StaticDispatchPlanTableRef,
};

/// Public `ArtifactNestedProcSiteTableRef` declaration.
pub const ArtifactNestedProcSiteTableRef = struct {
    artifact: CheckedModuleArtifactKey,
    table: NestedProcSiteTableRef,
};

/// Public `ArtifactModuleInterfaceCapabilitiesRef` declaration.
pub const ArtifactModuleInterfaceCapabilitiesRef = struct {
    artifact: CheckedModuleArtifactKey,
};

/// Public `ArtifactCallableResultPlanRef` declaration.
pub const ArtifactCallableResultPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: CallableResultPlanId,
};

/// Public `ArtifactCallablePromotionPlanRef` declaration.
pub const ArtifactCallablePromotionPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: CallablePromotionPlanId,
};

/// Public `ArtifactConstGraphReificationPlanRef` declaration.
pub const ArtifactConstGraphReificationPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: ConstReificationPlanId,
};

/// Public `ArtifactPrivateCaptureNodeRef` declaration.
pub const ArtifactPrivateCaptureNodeRef = struct {
    artifact: CheckedModuleArtifactKey,
    node: PrivateCaptureNodeId,
};

/// Public `ArtifactPromotedCallableWrapperRef` declaration.
pub const ArtifactPromotedCallableWrapperRef = struct {
    artifact: CheckedModuleArtifactKey,
    wrapper: canonical.PromotedCallableWrapperId,
};

/// Public `ArtifactPromotedCallableBodyPlanRef` declaration.
pub const ArtifactPromotedCallableBodyPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: canonical.PromotedCallableBodyPlanId,
};

/// Public `ImportedTemplateClosureView` declaration.
pub const ImportedTemplateClosureView = struct {
    checked_bodies: []const ArtifactCheckedBodyRef = &.{},
    checked_type_roots: []const ArtifactCheckedTypeRef = &.{},
    checked_type_schemes: []const ArtifactCheckedTypeSchemeRef = &.{},
    checked_callable_bodies: []const ArtifactCheckedCallableBodyRef = &.{},
    checked_const_bodies: []const ArtifactCheckedConstBodyRef = &.{},
    checked_procedure_templates: []const ArtifactProcedureTemplateRef = &.{},
    callable_eval_templates: []const ArtifactCallableEvalTemplateRef = &.{},
    const_templates: []const ConstRef = &.{},
    promoted_procedures: []const PromotedProcedureRef = &.{},
    semantic_instantiation_procedures: []const SemanticInstantiationProcedureId = &.{},
    promoted_callable_wrappers: []const ArtifactPromotedCallableWrapperRef = &.{},
    promoted_callable_body_plans: []const ArtifactPromotedCallableBodyPlanRef = &.{},
    private_capture_roots: []const PrivateCaptureId = &.{},
    private_capture_nodes: []const ArtifactPrivateCaptureNodeRef = &.{},
    private_capture_const_templates: []const ConstRef = &.{},
    callable_result_plans: []const ArtifactCallableResultPlanRef = &.{},
    callable_promotion_plans: []const ArtifactCallablePromotionPlanRef = &.{},
    const_reification_plans: []const ArtifactConstGraphReificationPlanRef = &.{},
    nested_proc_sites: []const ArtifactNestedProcSiteTableRef = &.{},
    resolved_value_refs: []const ArtifactResolvedValueRefTableRef = &.{},
    static_dispatch_plans: []const ArtifactStaticDispatchPlanTableRef = &.{},
    method_registry_entries: []const MethodRegistryEntryRef = &.{},
    interface_capabilities: []const ArtifactModuleInterfaceCapabilitiesRef = &.{},
};

/// Public `appendImportedTemplateClosureArtifactKeys` function.
///
/// Appends every checked-artifact key explicitly referenced by an imported
/// template closure. This is used when an executable root needs to assemble the
/// complete set of read-only artifact views required by platform/app relation
/// closures without scanning source or rediscovering dependencies from syntax.
pub fn appendImportedTemplateClosureArtifactKeys(
    allocator: Allocator,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    closure: ImportedTemplateClosureView,
) Allocator.Error!void {
    for (closure.checked_bodies) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.checked_type_roots) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.checked_type_schemes) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.checked_callable_bodies) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.checked_const_bodies) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.checked_procedure_templates) |value| try appendClosureArtifactKey(allocator, keys, checkedArtifactKeyFromArtifactRef(value.artifact));
    for (closure.callable_eval_templates) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.const_templates) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.promoted_procedures) |value| try appendClosureArtifactKey(allocator, keys, checkedArtifactKeyFromArtifactRef(value.proc.artifact));
    for (closure.promoted_callable_wrappers) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.promoted_callable_body_plans) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.private_capture_nodes) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.private_capture_const_templates) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.callable_result_plans) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.callable_promotion_plans) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.const_reification_plans) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.nested_proc_sites) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.resolved_value_refs) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.static_dispatch_plans) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
    for (closure.interface_capabilities) |value| try appendClosureArtifactKey(allocator, keys, value.artifact);
}

/// Public `appendPlatformRelationDependencyArtifactKeys` function.
///
/// Appends every checked-artifact key a platform/app relation row may need
/// during post-check lowering. The source is the already-published relation row
/// plus the relation artifact's exported closure data; callers must not derive
/// these dependencies from source imports or declaration scans.
pub fn appendPlatformRelationDependencyArtifactKeys(
    allocator: Allocator,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    relation_artifact: *const CheckedModuleArtifact,
    binding: PlatformRequiredBinding,
) Allocator.Error!void {
    try appendPlatformRelationDependencyArtifactKeysFromView(
        allocator,
        keys,
        importedView(relation_artifact),
        binding,
    );
}

/// Public `appendPlatformRelationDependencyArtifactKeysFromView` function.
///
/// Same dependency collection as `appendPlatformRelationDependencyArtifactKeys`,
/// but consumes the read-only relation artifact view already passed across the
/// post-check lowering boundary.
pub fn appendPlatformRelationDependencyArtifactKeysFromView(
    allocator: Allocator,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    relation_artifact: ImportedModuleView,
    binding: PlatformRequiredBinding,
) Allocator.Error!void {
    for (relation_artifact.direct_import_artifact_keys) |key| {
        try appendClosureArtifactKey(allocator, keys, key);
    }

    const relation_closure = switch (binding.value_use) {
        .const_value => |const_value| const_value.relation_template_closure,
        .procedure_value => |procedure_value| procedure_value.relation_template_closure,
    };
    try appendImportedTemplateClosureArtifactKeys(allocator, keys, relation_closure);
    try appendRelationArtifactExportedValueClosureKeysFromView(allocator, keys, relation_artifact, binding);
}

fn appendRelationArtifactExportedValueClosureKeysFromView(
    allocator: Allocator,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    relation_artifact: ImportedModuleView,
    binding: PlatformRequiredBinding,
) Allocator.Error!void {
    switch (binding.value_use) {
        .procedure_value => {
            var found = false;
            for (relation_artifact.exported_procedure_bindings.bindings) |exported| {
                if (exported.binding.pattern != binding.app_value.pattern) continue;
                found = true;
                try appendImportedTemplateClosureArtifactKeys(allocator, keys, exported.template_closure);
                for (relation_artifact.exported_procedure_templates.templates) |template| {
                    if (template.def == exported.binding.def) {
                        try appendImportedTemplateClosureArtifactKeys(allocator, keys, template.template_closure);
                    }
                }
            }
            if (!found) {
                checkedArtifactInvariant("platform relation dependency collection could not find exported app procedure binding", .{});
            }
        },
        .const_value => |const_use| {
            var found = false;
            for (relation_artifact.exported_const_templates.templates) |template| {
                if (template.pattern != binding.app_value.pattern) continue;
                if (!constRefEql(template.const_ref, const_use.const_use.const_ref)) continue;
                found = true;
                try appendImportedTemplateClosureArtifactKeys(allocator, keys, template.template_closure);
            }
            if (!found) {
                checkedArtifactInvariant("platform relation dependency collection could not find exported app const template", .{});
            }
        },
    }
}

fn checkedArtifactKeyFromArtifactRef(ref: canonical.ArtifactRef) CheckedModuleArtifactKey {
    return .{ .bytes = ref.bytes };
}

fn appendClosureArtifactKey(
    allocator: Allocator,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    key: CheckedModuleArtifactKey,
) Allocator.Error!void {
    for (keys.items) |existing| {
        if (checkedArtifactKeyEql(existing, key)) return;
    }
    try keys.append(allocator, key);
}

/// Public `ExportedProcedureTemplate` declaration.
pub const ExportedProcedureTemplate = struct {
    export_name: ?canonical.ExportNameId,
    def: CIR.Def.Idx,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    template: canonical.ProcedureTemplateRef,
    template_data: CheckedProcedureTemplate,
    template_closure: ImportedTemplateClosureView = .{},
};

/// Public `ExportedProcedureTemplateView` declaration.
pub const ExportedProcedureTemplateView = struct {
    templates: []const ExportedProcedureTemplate = &.{},
};

/// Public `ExportedProcedureTemplateTable` declaration.
pub const ExportedProcedureTemplateTable = struct {
    templates: []ExportedProcedureTemplate = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        published_exports: []const CIR.Def.Idx,
        names: *canonical.CanonicalNameStore,
        artifact_key: CheckedModuleArtifactKey,
        checked_types: *const CheckedTypeStore,
        checked_templates: *const CheckedProcedureTemplateTable,
        const_templates: *const ConstTemplateTable,
        resolved_value_refs: *const ResolvedValueRefTable,
        top_level_bindings: *const TopLevelProcedureBindingTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        imports: []const PublishImportArtifact,
    ) Allocator.Error!ExportedProcedureTemplateTable {
        var templates = std.ArrayList(ExportedProcedureTemplate).empty;
        errdefer {
            for (templates.items) |*template| deinitImportedTemplateClosure(allocator, &template.template_closure);
            templates.deinit(allocator);
        }

        for (published_exports) |def_idx| {
            const template = checked_templates.lookupByDef(def_idx) orelse continue;
            const def = module.def(def_idx);
            const export_name = if (def.patternName()) |name|
                try names.internExportIdent(module.identStoreConst(), name)
            else
                null;
            const source_scheme = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                module.defType(def_idx),
            );
            const template_data = checked_templates.get(template.template);
            var template_closure = try buildImportedTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                const_templates,
                resolved_value_refs,
                top_level_bindings,
                platform_required_bindings,
                imports,
                template,
                template_data,
            );
            errdefer deinitImportedTemplateClosure(allocator, &template_closure);

            try templates.append(allocator, .{
                .export_name = export_name,
                .def = def_idx,
                .source_scheme = source_scheme,
                .template = template,
                .template_data = template_data,
                .template_closure = template_closure,
            });
            template_closure = .{};
        }

        return .{ .templates = try templates.toOwnedSlice(allocator) };
    }

    pub fn view(self: *const ExportedProcedureTemplateTable) ExportedProcedureTemplateView {
        return .{ .templates = self.templates };
    }

    pub fn deinit(self: *ExportedProcedureTemplateTable, allocator: Allocator) void {
        for (self.templates) |*template| deinitImportedTemplateClosure(allocator, &template.template_closure);
        allocator.free(self.templates);
        self.* = .{};
    }
};

fn buildImportedTemplateClosure(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    checked_templates: *const CheckedProcedureTemplateTable,
    const_templates: *const ConstTemplateTable,
    resolved_value_refs: *const ResolvedValueRefTable,
    top_level_bindings: *const TopLevelProcedureBindingTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    imports: []const PublishImportArtifact,
    template_ref: canonical.ProcedureTemplateRef,
    template: CheckedProcedureTemplate,
) Allocator.Error!ImportedTemplateClosureView {
    var builder = ImportedTemplateClosureBuilder.init(
        allocator,
        artifact_key,
        checked_types,
        checked_templates,
        const_templates,
        resolved_value_refs,
        top_level_bindings,
        platform_required_bindings,
        imports,
    );
    defer builder.deinit();

    try builder.appendInterfaceCapabilities(.{ .artifact = artifact_key });
    try builder.appendTemplate(template_ref, template);

    return .{
        .checked_bodies = try builder.checked_bodies.toOwnedSlice(allocator),
        .checked_type_roots = try builder.checked_type_roots.toOwnedSlice(allocator),
        .checked_type_schemes = try builder.checked_type_schemes.toOwnedSlice(allocator),
        .checked_callable_bodies = try builder.checked_callable_bodies.toOwnedSlice(allocator),
        .checked_const_bodies = try builder.checked_const_bodies.toOwnedSlice(allocator),
        .checked_procedure_templates = try builder.checked_procedure_templates.toOwnedSlice(allocator),
        .callable_eval_templates = try builder.callable_eval_templates.toOwnedSlice(allocator),
        .const_templates = try builder.const_templates.toOwnedSlice(allocator),
        .promoted_procedures = try builder.promoted_procedures.toOwnedSlice(allocator),
        .semantic_instantiation_procedures = try builder.semantic_instantiation_procedures.toOwnedSlice(allocator),
        .promoted_callable_wrappers = try builder.promoted_callable_wrappers.toOwnedSlice(allocator),
        .promoted_callable_body_plans = try builder.promoted_callable_body_plans.toOwnedSlice(allocator),
        .private_capture_roots = try builder.private_capture_roots.toOwnedSlice(allocator),
        .private_capture_nodes = try builder.private_capture_nodes.toOwnedSlice(allocator),
        .private_capture_const_templates = try builder.private_capture_const_templates.toOwnedSlice(allocator),
        .callable_result_plans = try builder.callable_result_plans.toOwnedSlice(allocator),
        .callable_promotion_plans = try builder.callable_promotion_plans.toOwnedSlice(allocator),
        .const_reification_plans = try builder.const_reification_plans.toOwnedSlice(allocator),
        .nested_proc_sites = try builder.nested_proc_sites.toOwnedSlice(allocator),
        .resolved_value_refs = try builder.resolved_value_refs.toOwnedSlice(allocator),
        .static_dispatch_plans = try builder.static_dispatch_plans.toOwnedSlice(allocator),
        .method_registry_entries = try builder.method_registry_entries.toOwnedSlice(allocator),
        .interface_capabilities = try builder.interface_capabilities.toOwnedSlice(allocator),
    };
}

const ImportedTemplateClosureBuilder = struct {
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    checked_templates: *const CheckedProcedureTemplateTable,
    const_templates_table: *const ConstTemplateTable,
    resolved_value_refs_table: *const ResolvedValueRefTable,
    top_level_bindings: *const TopLevelProcedureBindingTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    imports: []const PublishImportArtifact,
    checked_bodies: std.ArrayList(ArtifactCheckedBodyRef),
    checked_type_roots: std.ArrayList(ArtifactCheckedTypeRef),
    checked_type_schemes: std.ArrayList(ArtifactCheckedTypeSchemeRef),
    checked_callable_bodies: std.ArrayList(ArtifactCheckedCallableBodyRef),
    checked_const_bodies: std.ArrayList(ArtifactCheckedConstBodyRef),
    checked_procedure_templates: std.ArrayList(ArtifactProcedureTemplateRef),
    callable_eval_templates: std.ArrayList(ArtifactCallableEvalTemplateRef),
    const_templates: std.ArrayList(ConstRef),
    promoted_procedures: std.ArrayList(PromotedProcedureRef),
    semantic_instantiation_procedures: std.ArrayList(SemanticInstantiationProcedureId),
    promoted_callable_wrappers: std.ArrayList(ArtifactPromotedCallableWrapperRef),
    promoted_callable_body_plans: std.ArrayList(ArtifactPromotedCallableBodyPlanRef),
    private_capture_roots: std.ArrayList(PrivateCaptureId),
    private_capture_nodes: std.ArrayList(ArtifactPrivateCaptureNodeRef),
    private_capture_const_templates: std.ArrayList(ConstRef),
    callable_result_plans: std.ArrayList(ArtifactCallableResultPlanRef),
    callable_promotion_plans: std.ArrayList(ArtifactCallablePromotionPlanRef),
    const_reification_plans: std.ArrayList(ArtifactConstGraphReificationPlanRef),
    nested_proc_sites: std.ArrayList(ArtifactNestedProcSiteTableRef),
    resolved_value_refs: std.ArrayList(ArtifactResolvedValueRefTableRef),
    static_dispatch_plans: std.ArrayList(ArtifactStaticDispatchPlanTableRef),
    method_registry_entries: std.ArrayList(MethodRegistryEntryRef),
    interface_capabilities: std.ArrayList(ArtifactModuleInterfaceCapabilitiesRef),

    fn init(
        allocator: Allocator,
        artifact_key: CheckedModuleArtifactKey,
        checked_types: *const CheckedTypeStore,
        checked_templates: *const CheckedProcedureTemplateTable,
        const_templates: *const ConstTemplateTable,
        resolved_value_refs: *const ResolvedValueRefTable,
        top_level_bindings: *const TopLevelProcedureBindingTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        imports: []const PublishImportArtifact,
    ) ImportedTemplateClosureBuilder {
        return .{
            .allocator = allocator,
            .artifact_key = artifact_key,
            .checked_types = checked_types,
            .checked_templates = checked_templates,
            .const_templates_table = const_templates,
            .resolved_value_refs_table = resolved_value_refs,
            .top_level_bindings = top_level_bindings,
            .platform_required_bindings = platform_required_bindings,
            .imports = imports,
            .checked_bodies = .empty,
            .checked_type_roots = .empty,
            .checked_type_schemes = .empty,
            .checked_callable_bodies = .empty,
            .checked_const_bodies = .empty,
            .checked_procedure_templates = .empty,
            .callable_eval_templates = .empty,
            .const_templates = .empty,
            .promoted_procedures = .empty,
            .semantic_instantiation_procedures = .empty,
            .promoted_callable_wrappers = .empty,
            .promoted_callable_body_plans = .empty,
            .private_capture_roots = .empty,
            .private_capture_nodes = .empty,
            .private_capture_const_templates = .empty,
            .callable_result_plans = .empty,
            .callable_promotion_plans = .empty,
            .const_reification_plans = .empty,
            .nested_proc_sites = .empty,
            .resolved_value_refs = .empty,
            .static_dispatch_plans = .empty,
            .method_registry_entries = .empty,
            .interface_capabilities = .empty,
        };
    }

    fn deinit(self: *ImportedTemplateClosureBuilder) void {
        self.interface_capabilities.deinit(self.allocator);
        self.method_registry_entries.deinit(self.allocator);
        self.static_dispatch_plans.deinit(self.allocator);
        self.resolved_value_refs.deinit(self.allocator);
        self.nested_proc_sites.deinit(self.allocator);
        self.const_reification_plans.deinit(self.allocator);
        self.callable_promotion_plans.deinit(self.allocator);
        self.callable_result_plans.deinit(self.allocator);
        self.private_capture_const_templates.deinit(self.allocator);
        self.private_capture_nodes.deinit(self.allocator);
        self.private_capture_roots.deinit(self.allocator);
        self.promoted_callable_body_plans.deinit(self.allocator);
        self.promoted_callable_wrappers.deinit(self.allocator);
        self.semantic_instantiation_procedures.deinit(self.allocator);
        self.promoted_procedures.deinit(self.allocator);
        self.const_templates.deinit(self.allocator);
        self.callable_eval_templates.deinit(self.allocator);
        self.checked_procedure_templates.deinit(self.allocator);
        self.checked_const_bodies.deinit(self.allocator);
        self.checked_callable_bodies.deinit(self.allocator);
        self.checked_type_schemes.deinit(self.allocator);
        self.checked_type_roots.deinit(self.allocator);
        self.checked_bodies.deinit(self.allocator);
    }

    fn appendTemplate(
        self: *ImportedTemplateClosureBuilder,
        template_ref: canonical.ProcedureTemplateRef,
        template: CheckedProcedureTemplate,
    ) Allocator.Error!void {
        if (self.containsProcedureTemplate(template_ref)) return;

        try self.checked_procedure_templates.append(self.allocator, template_ref);
        switch (template.body) {
            .checked_body => |body| try self.appendCheckedBody(body),
            .promoted_callable_wrapper,
            .intrinsic_wrapper,
            .entry_wrapper,
            => {},
        }
        try self.appendCheckedTypeRoot(template.checked_fn_root);
        try self.appendCheckedTypeScheme(template.checked_fn_scheme);
        try self.appendNestedProcSites(template.nested_proc_sites);
        try self.appendResolvedValueRefs(template.resolved_value_refs);
        try self.appendStaticDispatchPlans(template.static_dispatch_plans);
        try self.appendProcedureDependencies(template.resolved_value_refs);
    }

    fn containsProcedureTemplate(
        self: *const ImportedTemplateClosureBuilder,
        template_ref: canonical.ProcedureTemplateRef,
    ) bool {
        for (self.checked_procedure_templates.items) |existing| {
            if (canonical.procedureTemplateRefEql(existing, template_ref)) return true;
        }
        return false;
    }

    fn appendCheckedBody(self: *ImportedTemplateClosureBuilder, body: CheckedBodyId) Allocator.Error!void {
        const ref = ArtifactCheckedBodyRef{ .artifact = self.artifact_key, .body = body };
        for (self.checked_bodies.items) |existing| {
            if (existing.body == ref.body and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.checked_bodies.append(self.allocator, ref);
    }

    fn appendCheckedConstBody(self: *ImportedTemplateClosureBuilder, body: CheckedConstBodyRef) Allocator.Error!void {
        const ref = ArtifactCheckedConstBodyRef{ .artifact = self.artifact_key, .body = body };
        for (self.checked_const_bodies.items) |existing| {
            if (existing.body == ref.body and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.checked_const_bodies.append(self.allocator, ref);
    }

    fn appendCheckedTypeRoot(self: *ImportedTemplateClosureBuilder, ty: CheckedTypeId) Allocator.Error!void {
        const ref = ArtifactCheckedTypeRef{ .artifact = self.artifact_key, .ty = ty };
        for (self.checked_type_roots.items) |existing| {
            if (existing.ty == ref.ty and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.checked_type_roots.append(self.allocator, ref);
    }

    fn appendCheckedTypeScheme(
        self: *ImportedTemplateClosureBuilder,
        scheme_key: canonical.CanonicalTypeSchemeKey,
    ) Allocator.Error!void {
        const scheme = self.checked_types.schemeForKey(scheme_key) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("checked artifact invariant violated: exported template references missing checked type scheme", .{});
            }
            unreachable;
        };
        const ref = ArtifactCheckedTypeSchemeRef{ .artifact = self.artifact_key, .scheme = scheme.id };
        for (self.checked_type_schemes.items) |existing| {
            if (existing.scheme == ref.scheme and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.checked_type_schemes.append(self.allocator, ref);
    }

    fn appendNestedProcSites(
        self: *ImportedTemplateClosureBuilder,
        table: NestedProcSiteTableRef,
    ) Allocator.Error!void {
        if (table.len == 0) return;
        const ref = ArtifactNestedProcSiteTableRef{ .artifact = self.artifact_key, .table = table };
        for (self.nested_proc_sites.items) |existing| {
            if (existing.table.start == table.start and existing.table.len == table.len and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.nested_proc_sites.append(self.allocator, ref);
    }

    fn appendResolvedValueRefs(
        self: *ImportedTemplateClosureBuilder,
        table: ResolvedValueRefTableRef,
    ) Allocator.Error!void {
        if (table.len == 0) return;
        const ref = ArtifactResolvedValueRefTableRef{ .artifact = self.artifact_key, .table = table };
        for (self.resolved_value_refs.items) |existing| {
            if (existing.table.start == table.start and existing.table.len == table.len and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.resolved_value_refs.append(self.allocator, ref);
    }

    fn appendStaticDispatchPlans(
        self: *ImportedTemplateClosureBuilder,
        table: StaticDispatchPlanTableRef,
    ) Allocator.Error!void {
        if (table.len == 0) return;
        const ref = ArtifactStaticDispatchPlanTableRef{ .artifact = self.artifact_key, .table = table };
        for (self.static_dispatch_plans.items) |existing| {
            if (existing.table.start == table.start and existing.table.len == table.len and std.mem.eql(u8, &existing.artifact.bytes, &ref.artifact.bytes)) return;
        }
        try self.static_dispatch_plans.append(self.allocator, ref);
    }

    fn appendInterfaceCapabilities(
        self: *ImportedTemplateClosureBuilder,
        ref: ArtifactModuleInterfaceCapabilitiesRef,
    ) Allocator.Error!void {
        try appendUniqueValue(ArtifactModuleInterfaceCapabilitiesRef, self.allocator, &self.interface_capabilities, ref);
    }

    fn appendImportedTemplateClosure(
        self: *ImportedTemplateClosureBuilder,
        closure: ImportedTemplateClosureView,
    ) Allocator.Error!void {
        for (closure.checked_bodies) |value| try appendUniqueValue(ArtifactCheckedBodyRef, self.allocator, &self.checked_bodies, value);
        for (closure.checked_type_roots) |value| try appendUniqueValue(ArtifactCheckedTypeRef, self.allocator, &self.checked_type_roots, value);
        for (closure.checked_type_schemes) |value| try appendUniqueValue(ArtifactCheckedTypeSchemeRef, self.allocator, &self.checked_type_schemes, value);
        for (closure.checked_callable_bodies) |value| try appendUniqueValue(ArtifactCheckedCallableBodyRef, self.allocator, &self.checked_callable_bodies, value);
        for (closure.checked_const_bodies) |value| try appendUniqueValue(ArtifactCheckedConstBodyRef, self.allocator, &self.checked_const_bodies, value);
        for (closure.checked_procedure_templates) |value| try appendUniqueValue(ArtifactProcedureTemplateRef, self.allocator, &self.checked_procedure_templates, value);
        for (closure.callable_eval_templates) |value| try appendUniqueValue(ArtifactCallableEvalTemplateRef, self.allocator, &self.callable_eval_templates, value);
        for (closure.const_templates) |value| try appendUniqueValue(ConstRef, self.allocator, &self.const_templates, value);
        for (closure.promoted_procedures) |value| try appendUniqueValue(PromotedProcedureRef, self.allocator, &self.promoted_procedures, value);
        for (closure.semantic_instantiation_procedures) |value| try appendUniqueValue(SemanticInstantiationProcedureId, self.allocator, &self.semantic_instantiation_procedures, value);
        for (closure.promoted_callable_wrappers) |value| try appendUniqueValue(ArtifactPromotedCallableWrapperRef, self.allocator, &self.promoted_callable_wrappers, value);
        for (closure.promoted_callable_body_plans) |value| try appendUniqueValue(ArtifactPromotedCallableBodyPlanRef, self.allocator, &self.promoted_callable_body_plans, value);
        for (closure.private_capture_roots) |value| try appendUniqueValue(PrivateCaptureId, self.allocator, &self.private_capture_roots, value);
        for (closure.private_capture_nodes) |value| try appendUniqueValue(ArtifactPrivateCaptureNodeRef, self.allocator, &self.private_capture_nodes, value);
        for (closure.private_capture_const_templates) |value| try appendUniqueValue(ConstRef, self.allocator, &self.private_capture_const_templates, value);
        for (closure.callable_result_plans) |value| try appendUniqueValue(ArtifactCallableResultPlanRef, self.allocator, &self.callable_result_plans, value);
        for (closure.callable_promotion_plans) |value| try appendUniqueValue(ArtifactCallablePromotionPlanRef, self.allocator, &self.callable_promotion_plans, value);
        for (closure.const_reification_plans) |value| try appendUniqueValue(ArtifactConstGraphReificationPlanRef, self.allocator, &self.const_reification_plans, value);
        for (closure.nested_proc_sites) |value| try appendUniqueValue(ArtifactNestedProcSiteTableRef, self.allocator, &self.nested_proc_sites, value);
        for (closure.resolved_value_refs) |value| try appendUniqueValue(ArtifactResolvedValueRefTableRef, self.allocator, &self.resolved_value_refs, value);
        for (closure.static_dispatch_plans) |value| try appendUniqueValue(ArtifactStaticDispatchPlanTableRef, self.allocator, &self.static_dispatch_plans, value);
        for (closure.method_registry_entries) |value| try appendUniqueValue(MethodRegistryEntryRef, self.allocator, &self.method_registry_entries, value);
        for (closure.interface_capabilities) |value| try appendUniqueValue(ArtifactModuleInterfaceCapabilitiesRef, self.allocator, &self.interface_capabilities, value);
    }

    fn appendProcedureDependencies(
        self: *ImportedTemplateClosureBuilder,
        table: ResolvedValueRefTableRef,
    ) Allocator.Error!void {
        const end = table.start + table.len;
        if (end > self.resolved_value_refs_table.template_refs.len) checkedArtifactInvariant("checked template resolved-ref span was outside table", .{});
        for (self.resolved_value_refs_table.template_refs[table.start..end]) |ref_id| {
            const raw = @intFromEnum(ref_id);
            if (raw >= self.resolved_value_refs_table.records.len) checkedArtifactInvariant("checked template resolved-ref id was outside table", .{});
            const resolved = self.resolved_value_refs_table.records[raw].ref;
            if (try self.appendImportedTemplateClosureForResolvedRef(resolved)) continue;
            if (try self.appendPlatformRequiredRelationClosureForResolvedRef(resolved)) continue;
            if (self.templateForResolvedValueRef(resolved)) |dependency_ref| {
                const template = self.checked_templates.get(dependency_ref.template);
                try self.appendTemplate(dependency_ref, template);
            }
            if (self.constRefForResolvedValueRef(resolved)) |dependency_ref| {
                try self.appendConstTemplate(dependency_ref);
            }
        }
    }

    fn appendImportedTemplateClosureForResolvedRef(
        self: *ImportedTemplateClosureBuilder,
        ref: ResolvedValueRef,
    ) Allocator.Error!bool {
        return switch (ref) {
            .imported_const => |const_use| blk: {
                const template = self.importedConstTemplate(const_use.const_ref);
                try self.appendImportedTemplateClosure(template.template_closure);
                break :blk true;
            },
            .imported_proc => |proc_use| blk: {
                const imported = switch (proc_use.binding) {
                    .imported => |imported| imported,
                    .top_level,
                    .hosted,
                    .platform_required,
                    .promoted,
                    => checkedArtifactInvariant("imported procedure ref did not carry imported binding", .{}),
                };
                const binding = self.importedProcedureBinding(imported);
                try self.appendImportedTemplateClosure(binding.template_closure);
                break :blk true;
            },
            else => false,
        };
    }

    fn importedProcedureBinding(
        self: *ImportedTemplateClosureBuilder,
        ref: ImportedProcedureBindingRef,
    ) ImportedProcedureBindingView {
        for (self.imports) |import| {
            if (!std.mem.eql(u8, &import.key.bytes, &ref.artifact.bytes)) continue;
            for (import.view.exported_procedure_bindings.bindings) |binding| {
                if (binding.binding.def == ref.def and
                    binding.binding.pattern == ref.pattern)
                {
                    return binding;
                }
            }
        }
        checkedArtifactInvariant("imported procedure dependency had no published imported closure", .{});
    }

    fn importedConstTemplate(
        self: *ImportedTemplateClosureBuilder,
        ref: ConstRef,
    ) ImportedConstTemplateView {
        for (self.imports) |import| {
            if (!std.mem.eql(u8, &import.key.bytes, &ref.artifact.bytes)) continue;
            for (import.view.exported_const_templates.templates) |template| {
                if (constRefEql(template.const_ref, ref)) return template;
            }
        }
        checkedArtifactInvariant("imported const dependency had no published imported closure", .{});
    }

    fn appendPlatformRequiredRelationClosureForResolvedRef(
        self: *ImportedTemplateClosureBuilder,
        ref: ResolvedValueRef,
    ) Allocator.Error!bool {
        return switch (ref) {
            .platform_required_const => |required| blk: {
                const binding = self.platformRequiredBinding(required.binding);
                const const_use = switch (binding.value_use) {
                    .const_value => |const_value| const_value,
                    .procedure_value => checkedArtifactInvariant("platform-required const ref pointed at procedure binding {d}", .{@intFromEnum(required.binding)}),
                };
                try self.appendImportedTemplateClosure(const_use.relation_template_closure);
                break :blk true;
            },
            .platform_required_proc => |required| blk: {
                const binding = self.platformRequiredBinding(required.binding);
                const proc_use = switch (binding.value_use) {
                    .procedure_value => |procedure_value| procedure_value,
                    .const_value => checkedArtifactInvariant("platform-required procedure ref pointed at const binding {d}", .{@intFromEnum(required.binding)}),
                };
                try self.appendImportedTemplateClosure(proc_use.relation_template_closure);
                break :blk true;
            },
            else => false,
        };
    }

    fn platformRequiredBinding(
        self: *ImportedTemplateClosureBuilder,
        binding_id: PlatformRequiredBindingId,
    ) PlatformRequiredBinding {
        return self.platform_required_bindings.lookupByBindingId(@intFromEnum(binding_id)) orelse {
            checkedArtifactInvariant("resolved platform-required value referenced missing binding {d}", .{@intFromEnum(binding_id)});
        };
    }

    fn appendConstTemplate(
        self: *ImportedTemplateClosureBuilder,
        const_ref: ConstRef,
    ) Allocator.Error!void {
        for (self.const_templates.items) |existing| {
            if (constRefEql(existing, const_ref)) return;
        }
        try self.const_templates.append(self.allocator, const_ref);

        if (!std.mem.eql(u8, &const_ref.artifact.bytes, &self.artifact_key.bytes)) return;

        const scheme = self.checked_types.schemeForKey(const_ref.source_scheme) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("checked artifact invariant violated: const template references missing checked type scheme", .{});
            }
            unreachable;
        };
        try self.appendCheckedTypeRoot(scheme.root);
        try self.appendCheckedTypeScheme(const_ref.source_scheme);

        const template = self.const_templates_table.get(const_ref);
        switch (template.state) {
            .eval_template => |eval| {
                try self.appendCheckedConstBody(eval.body);
                try self.appendNestedProcSites(eval.nested_proc_sites);
                try self.appendResolvedValueRefs(eval.resolved_value_refs);
                try self.appendStaticDispatchPlans(eval.static_dispatch_plans);
                try self.appendProcedureDependencies(eval.resolved_value_refs);
                const entry_template = self.checked_templates.get(eval.entry_template.template);
                try self.appendTemplate(eval.entry_template, entry_template);
            },
            .value_graph_template => {},
            .reserved => checkedArtifactInvariant("imported template closure reached unsealed const template", .{}),
        }
    }

    fn templateForResolvedValueRef(
        self: *ImportedTemplateClosureBuilder,
        ref: ResolvedValueRef,
    ) ?canonical.ProcedureTemplateRef {
        const use: ProcedureUseTemplate = switch (ref) {
            .top_level_proc,
            .hosted_proc,
            .promoted_top_level_proc,
            => |procedure| procedure,
            .platform_required_proc => |required| required.procedure,
            else => return null,
        };
        return switch (use.binding) {
            .top_level => |binding_ref| self.templateForTopLevelBinding(binding_ref),
            .hosted => |hosted| hosted.template,
            .platform_required => checkedArtifactInvariant("platform-required procedure dependency must be provided by its relation template closure", .{}),
            .promoted => null,
            .imported => null,
        };
    }

    fn constRefForResolvedValueRef(
        self: *ImportedTemplateClosureBuilder,
        ref: ResolvedValueRef,
    ) ?ConstRef {
        _ = self;
        return switch (ref) {
            .top_level_const,
            => |const_use| const_use.const_ref,
            else => null,
        };
    }

    fn templateForTopLevelBinding(
        self: *ImportedTemplateClosureBuilder,
        binding_ref: TopLevelProcedureBindingRef,
    ) ?canonical.ProcedureTemplateRef {
        const binding = self.top_level_bindings.get(binding_ref);
        return switch (binding.body) {
            .direct_template => |direct| checkedTemplateFromCallableTemplateForClosure(direct.template),
            .callable_eval_template => null,
        };
    }
};

fn checkedTemplateFromCallableTemplateForClosure(
    template: canonical.CallableProcedureTemplateRef,
) ?canonical.ProcedureTemplateRef {
    return switch (template) {
        .checked => |checked| checked,
        .synthetic => |synthetic| synthetic.template,
        .lifted => null,
    };
}

fn buildImportedConstTemplateClosure(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    checked_templates: *const CheckedProcedureTemplateTable,
    const_templates: *const ConstTemplateTable,
    resolved_value_refs: *const ResolvedValueRefTable,
    top_level_bindings: *const TopLevelProcedureBindingTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    imports: []const PublishImportArtifact,
    const_ref: ConstRef,
) Allocator.Error!ImportedTemplateClosureView {
    var builder = ImportedTemplateClosureBuilder.init(
        allocator,
        artifact_key,
        checked_types,
        checked_templates,
        const_templates,
        resolved_value_refs,
        top_level_bindings,
        platform_required_bindings,
        imports,
    );
    defer builder.deinit();

    try builder.appendInterfaceCapabilities(.{ .artifact = artifact_key });
    try builder.appendConstTemplate(const_ref);

    return .{
        .checked_bodies = try builder.checked_bodies.toOwnedSlice(allocator),
        .checked_type_roots = try builder.checked_type_roots.toOwnedSlice(allocator),
        .checked_type_schemes = try builder.checked_type_schemes.toOwnedSlice(allocator),
        .checked_callable_bodies = try builder.checked_callable_bodies.toOwnedSlice(allocator),
        .checked_const_bodies = try builder.checked_const_bodies.toOwnedSlice(allocator),
        .checked_procedure_templates = try builder.checked_procedure_templates.toOwnedSlice(allocator),
        .callable_eval_templates = try builder.callable_eval_templates.toOwnedSlice(allocator),
        .const_templates = try builder.const_templates.toOwnedSlice(allocator),
        .promoted_procedures = try builder.promoted_procedures.toOwnedSlice(allocator),
        .semantic_instantiation_procedures = try builder.semantic_instantiation_procedures.toOwnedSlice(allocator),
        .promoted_callable_wrappers = try builder.promoted_callable_wrappers.toOwnedSlice(allocator),
        .promoted_callable_body_plans = try builder.promoted_callable_body_plans.toOwnedSlice(allocator),
        .private_capture_roots = try builder.private_capture_roots.toOwnedSlice(allocator),
        .private_capture_nodes = try builder.private_capture_nodes.toOwnedSlice(allocator),
        .private_capture_const_templates = try builder.private_capture_const_templates.toOwnedSlice(allocator),
        .callable_result_plans = try builder.callable_result_plans.toOwnedSlice(allocator),
        .callable_promotion_plans = try builder.callable_promotion_plans.toOwnedSlice(allocator),
        .const_reification_plans = try builder.const_reification_plans.toOwnedSlice(allocator),
        .nested_proc_sites = try builder.nested_proc_sites.toOwnedSlice(allocator),
        .resolved_value_refs = try builder.resolved_value_refs.toOwnedSlice(allocator),
        .static_dispatch_plans = try builder.static_dispatch_plans.toOwnedSlice(allocator),
        .method_registry_entries = try builder.method_registry_entries.toOwnedSlice(allocator),
        .interface_capabilities = try builder.interface_capabilities.toOwnedSlice(allocator),
    };
}

fn freeConstSlice(allocator: Allocator, slice: anytype) void {
    if (slice.len > 0) allocator.free(slice);
}

fn appendUniqueValue(
    comptime T: type,
    allocator: Allocator,
    list: *std.ArrayList(T),
    value: T,
) Allocator.Error!void {
    for (list.items) |existing| {
        if (std.meta.eql(existing, value)) return;
    }
    try list.append(allocator, value);
}

fn deinitImportedTemplateClosure(
    allocator: Allocator,
    closure: *ImportedTemplateClosureView,
) void {
    freeConstSlice(allocator, closure.checked_bodies);
    freeConstSlice(allocator, closure.checked_type_roots);
    freeConstSlice(allocator, closure.checked_type_schemes);
    freeConstSlice(allocator, closure.checked_callable_bodies);
    freeConstSlice(allocator, closure.checked_const_bodies);
    freeConstSlice(allocator, closure.checked_procedure_templates);
    freeConstSlice(allocator, closure.callable_eval_templates);
    freeConstSlice(allocator, closure.const_templates);
    freeConstSlice(allocator, closure.promoted_procedures);
    freeConstSlice(allocator, closure.semantic_instantiation_procedures);
    freeConstSlice(allocator, closure.promoted_callable_wrappers);
    freeConstSlice(allocator, closure.promoted_callable_body_plans);
    freeConstSlice(allocator, closure.private_capture_roots);
    freeConstSlice(allocator, closure.private_capture_nodes);
    freeConstSlice(allocator, closure.private_capture_const_templates);
    freeConstSlice(allocator, closure.callable_result_plans);
    freeConstSlice(allocator, closure.callable_promotion_plans);
    freeConstSlice(allocator, closure.const_reification_plans);
    freeConstSlice(allocator, closure.nested_proc_sites);
    freeConstSlice(allocator, closure.resolved_value_refs);
    freeConstSlice(allocator, closure.static_dispatch_plans);
    freeConstSlice(allocator, closure.method_registry_entries);
    freeConstSlice(allocator, closure.interface_capabilities);
    closure.* = .{};
}

fn cloneImportedTemplateClosure(
    allocator: Allocator,
    closure: ImportedTemplateClosureView,
) Allocator.Error!ImportedTemplateClosureView {
    var out = ImportedTemplateClosureView{};
    errdefer deinitImportedTemplateClosure(allocator, &out);

    out.checked_bodies = try cloneConstSlice(allocator, ArtifactCheckedBodyRef, closure.checked_bodies);
    out.checked_type_roots = try cloneConstSlice(allocator, ArtifactCheckedTypeRef, closure.checked_type_roots);
    out.checked_type_schemes = try cloneConstSlice(allocator, ArtifactCheckedTypeSchemeRef, closure.checked_type_schemes);
    out.checked_callable_bodies = try cloneConstSlice(allocator, ArtifactCheckedCallableBodyRef, closure.checked_callable_bodies);
    out.checked_const_bodies = try cloneConstSlice(allocator, ArtifactCheckedConstBodyRef, closure.checked_const_bodies);
    out.checked_procedure_templates = try cloneConstSlice(allocator, ArtifactProcedureTemplateRef, closure.checked_procedure_templates);
    out.callable_eval_templates = try cloneConstSlice(allocator, ArtifactCallableEvalTemplateRef, closure.callable_eval_templates);
    out.const_templates = try cloneConstSlice(allocator, ConstRef, closure.const_templates);
    out.promoted_procedures = try cloneConstSlice(allocator, PromotedProcedureRef, closure.promoted_procedures);
    out.semantic_instantiation_procedures = try cloneConstSlice(allocator, SemanticInstantiationProcedureId, closure.semantic_instantiation_procedures);
    out.promoted_callable_wrappers = try cloneConstSlice(allocator, ArtifactPromotedCallableWrapperRef, closure.promoted_callable_wrappers);
    out.promoted_callable_body_plans = try cloneConstSlice(allocator, ArtifactPromotedCallableBodyPlanRef, closure.promoted_callable_body_plans);
    out.private_capture_roots = try cloneConstSlice(allocator, PrivateCaptureId, closure.private_capture_roots);
    out.private_capture_nodes = try cloneConstSlice(allocator, ArtifactPrivateCaptureNodeRef, closure.private_capture_nodes);
    out.private_capture_const_templates = try cloneConstSlice(allocator, ConstRef, closure.private_capture_const_templates);
    out.callable_result_plans = try cloneConstSlice(allocator, ArtifactCallableResultPlanRef, closure.callable_result_plans);
    out.callable_promotion_plans = try cloneConstSlice(allocator, ArtifactCallablePromotionPlanRef, closure.callable_promotion_plans);
    out.const_reification_plans = try cloneConstSlice(allocator, ArtifactConstGraphReificationPlanRef, closure.const_reification_plans);
    out.nested_proc_sites = try cloneConstSlice(allocator, ArtifactNestedProcSiteTableRef, closure.nested_proc_sites);
    out.resolved_value_refs = try cloneConstSlice(allocator, ArtifactResolvedValueRefTableRef, closure.resolved_value_refs);
    out.static_dispatch_plans = try cloneConstSlice(allocator, ArtifactStaticDispatchPlanTableRef, closure.static_dispatch_plans);
    out.method_registry_entries = try cloneConstSlice(allocator, MethodRegistryEntryRef, closure.method_registry_entries);
    out.interface_capabilities = try cloneConstSlice(allocator, ArtifactModuleInterfaceCapabilitiesRef, closure.interface_capabilities);

    return out;
}

fn cloneConstSlice(
    allocator: Allocator,
    comptime T: type,
    slice: []const T,
) Allocator.Error![]const T {
    if (slice.len == 0) return &.{};
    return try allocator.dupe(T, slice);
}

fn deinitPlatformRequiredValueUse(
    allocator: Allocator,
    value_use: *PlatformRequiredValueUse,
) void {
    switch (value_use.*) {
        .const_value => |*const_use| deinitImportedTemplateClosure(allocator, &const_use.relation_template_closure),
        .procedure_value => |*procedure| deinitImportedTemplateClosure(allocator, &procedure.relation_template_closure),
    }
    value_use.* = undefined;
}

/// Public `ImportedProcedureBindingBody` declaration.
pub const ImportedProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateId,
};

/// Public `ImportedProcedureBindingView` declaration.
pub const ImportedProcedureBindingView = struct {
    binding: ImportedProcedureBindingRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    body: ImportedProcedureBindingBody,
    template_closure: ImportedTemplateClosureView = .{},
};

/// Public `ExportedProcedureBindingView` declaration.
pub const ExportedProcedureBindingView = struct {
    bindings: []const ImportedProcedureBindingView = &.{},
};

/// Public `ExportedProcedureBindingTable` declaration.
pub const ExportedProcedureBindingTable = struct {
    bindings: []ImportedProcedureBindingView = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        published_exports: []const CIR.Def.Idx,
        checked_types: *const CheckedTypeStore,
        checked_templates: *const CheckedProcedureTemplateTable,
        const_templates: *const ConstTemplateTable,
        top_level_values: *const TopLevelValueTable,
        procedure_bindings: *const TopLevelProcedureBindingTable,
        callable_eval_templates: *const CallableEvalTemplateTable,
        entry_wrappers: *const EntryWrapperTable,
        resolved_value_refs: *const ResolvedValueRefTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        imports: []const PublishImportArtifact,
        artifact_key: CheckedModuleArtifactKey,
    ) Allocator.Error!ExportedProcedureBindingTable {
        _ = module;
        var bindings = std.ArrayList(ImportedProcedureBindingView).empty;
        errdefer {
            for (bindings.items) |*binding| deinitImportedTemplateClosure(allocator, &binding.template_closure);
            bindings.deinit(allocator);
        }

        for (published_exports) |def_idx| {
            const top_level = top_level_values.lookupByDef(def_idx) orelse continue;
            const binding_ref = switch (top_level.value) {
                .procedure_binding => |binding| binding,
                .const_ref => continue,
            };
            const binding = procedure_bindings.get(binding_ref);
            const body: ImportedProcedureBindingBody = switch (binding.body) {
                .direct_template => |direct| .{ .direct_template = direct },
                .callable_eval_template => |template| .{ .callable_eval_template = template },
            };
            var template_closure = try buildProcedureBindingClosure(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                const_templates,
                resolved_value_refs,
                procedure_bindings,
                platform_required_bindings,
                imports,
                callable_eval_templates,
                entry_wrappers,
                binding.body,
            );
            errdefer deinitImportedTemplateClosure(allocator, &template_closure);

            try bindings.append(allocator, .{
                .binding = .{
                    .artifact = artifact_key,
                    .def = def_idx,
                    .pattern = top_level.pattern,
                },
                .source_scheme = binding.source_scheme,
                .body = body,
                .template_closure = template_closure,
            });
            template_closure = .{};
        }

        return .{ .bindings = try bindings.toOwnedSlice(allocator) };
    }

    pub fn view(self: *const ExportedProcedureBindingTable) ExportedProcedureBindingView {
        return .{ .bindings = self.bindings };
    }

    pub fn deinit(self: *ExportedProcedureBindingTable, allocator: Allocator) void {
        for (self.bindings) |*binding| deinitImportedTemplateClosure(allocator, &binding.template_closure);
        allocator.free(self.bindings);
        self.* = .{};
    }
};

fn buildProcedureBindingClosure(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    checked_templates: *const CheckedProcedureTemplateTable,
    const_templates: *const ConstTemplateTable,
    resolved_value_refs: *const ResolvedValueRefTable,
    top_level_bindings: *const TopLevelProcedureBindingTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    imports: []const PublishImportArtifact,
    callable_eval_templates: *const CallableEvalTemplateTable,
    entry_wrappers: *const EntryWrapperTable,
    body: ProcedureBindingBody,
) Allocator.Error!ImportedTemplateClosureView {
    return switch (body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template_ref| buildImportedTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                const_templates,
                resolved_value_refs,
                top_level_bindings,
                platform_required_bindings,
                imports,
                template_ref,
                checked_templates.get(template_ref.template),
            ),
            .synthetic => |synthetic| buildImportedTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                const_templates,
                resolved_value_refs,
                top_level_bindings,
                platform_required_bindings,
                imports,
                synthetic.template,
                checked_templates.get(synthetic.template.template),
            ),
            .lifted => {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: exported checked binding cannot reference lifted templates before mono", .{});
                }
                unreachable;
            },
        },
        .callable_eval_template => |template_id| blk: {
            const template = callable_eval_templates.get(template_id);
            const wrapper = entryWrapperForRoot(entry_wrappers, template.root);
            const entry_template = checked_templates.get(wrapper.template.template);

            var builder = ImportedTemplateClosureBuilder.init(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                const_templates,
                resolved_value_refs,
                top_level_bindings,
                platform_required_bindings,
                imports,
            );
            defer builder.deinit();

            try builder.appendInterfaceCapabilities(.{ .artifact = artifact_key });
            try builder.appendCheckedTypeRoot(template.checked_fn_root);
            try builder.appendCheckedTypeScheme(template.source_scheme);
            try builder.appendTemplate(wrapper.template, entry_template);

            try builder.callable_eval_templates.append(allocator, .{
                .artifact = artifact_key,
                .template = template_id,
            });

            break :blk .{
                .checked_bodies = try builder.checked_bodies.toOwnedSlice(allocator),
                .checked_type_roots = try builder.checked_type_roots.toOwnedSlice(allocator),
                .checked_type_schemes = try builder.checked_type_schemes.toOwnedSlice(allocator),
                .checked_callable_bodies = try builder.checked_callable_bodies.toOwnedSlice(allocator),
                .checked_const_bodies = try builder.checked_const_bodies.toOwnedSlice(allocator),
                .checked_procedure_templates = try builder.checked_procedure_templates.toOwnedSlice(allocator),
                .callable_eval_templates = try builder.callable_eval_templates.toOwnedSlice(allocator),
                .const_templates = try builder.const_templates.toOwnedSlice(allocator),
                .promoted_procedures = try builder.promoted_procedures.toOwnedSlice(allocator),
                .semantic_instantiation_procedures = try builder.semantic_instantiation_procedures.toOwnedSlice(allocator),
                .promoted_callable_wrappers = try builder.promoted_callable_wrappers.toOwnedSlice(allocator),
                .promoted_callable_body_plans = try builder.promoted_callable_body_plans.toOwnedSlice(allocator),
                .private_capture_roots = try builder.private_capture_roots.toOwnedSlice(allocator),
                .private_capture_nodes = try builder.private_capture_nodes.toOwnedSlice(allocator),
                .private_capture_const_templates = try builder.private_capture_const_templates.toOwnedSlice(allocator),
                .callable_result_plans = try builder.callable_result_plans.toOwnedSlice(allocator),
                .callable_promotion_plans = try builder.callable_promotion_plans.toOwnedSlice(allocator),
                .const_reification_plans = try builder.const_reification_plans.toOwnedSlice(allocator),
                .nested_proc_sites = try builder.nested_proc_sites.toOwnedSlice(allocator),
                .resolved_value_refs = try builder.resolved_value_refs.toOwnedSlice(allocator),
                .static_dispatch_plans = try builder.static_dispatch_plans.toOwnedSlice(allocator),
                .method_registry_entries = try builder.method_registry_entries.toOwnedSlice(allocator),
                .interface_capabilities = try builder.interface_capabilities.toOwnedSlice(allocator),
            };
        },
    };
}

/// Public `ConstEvalTemplate` declaration.
pub const ConstEvalTemplate = struct {
    body: CheckedConstBodyRef,
    entry_template: canonical.ProcedureTemplateRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    resolved_value_refs: ResolvedValueRefTableRef = .{},
    static_dispatch_plans: StaticDispatchPlanTableRef = .{},
    nested_proc_sites: NestedProcSiteTableRef = .{},
};

/// Public `ConstValueGraphTemplate` declaration.
pub const ConstValueGraphTemplate = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
};

/// Public `ConstTemplateState` declaration.
pub const ConstTemplateState = union(enum) {
    reserved,
    eval_template: ConstEvalTemplate,
    value_graph_template: ConstValueGraphTemplate,
};

/// Public `ConstTemplate` declaration.
pub const ConstTemplate = struct {
    id: ConstTemplateId,
    owner: ConstOwner,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    state: ConstTemplateState,
};

/// Public `ConstTemplateTable` declaration.
pub const ConstTemplateTable = struct {
    templates: std.ArrayList(ConstTemplate) = .empty,

    pub fn reserveTopLevel(
        self: *ConstTemplateTable,
        allocator: Allocator,
        artifact_key: CheckedModuleArtifactKey,
        module_idx: u32,
        pattern: CheckedPatternId,
        source_scheme: canonical.CanonicalTypeSchemeKey,
    ) Allocator.Error!ConstRef {
        const id: ConstTemplateId = @enumFromInt(@as(u32, @intCast(self.templates.items.len)));
        const owner: ConstOwner = .{ .top_level_binding = .{
            .module_idx = module_idx,
            .pattern = pattern,
        } };
        try self.templates.append(allocator, .{
            .id = id,
            .owner = owner,
            .source_scheme = source_scheme,
            .state = .reserved,
        });
        return .{
            .artifact = artifact_key,
            .owner = owner,
            .template = id,
            .source_scheme = source_scheme,
        };
    }

    pub fn reservePromotedCapture(
        self: *ConstTemplateTable,
        allocator: Allocator,
        artifact_key: CheckedModuleArtifactKey,
        promoted_proc: PromotedProcedureRef,
        capture_index: u32,
        source_scheme: canonical.CanonicalTypeSchemeKey,
    ) Allocator.Error!ConstRef {
        const id: ConstTemplateId = @enumFromInt(@as(u32, @intCast(self.templates.items.len)));
        const owner: ConstOwner = .{ .promoted_capture = .{
            .promoted_proc = promoted_proc,
            .capture_index = capture_index,
        } };
        try self.templates.append(allocator, .{
            .id = id,
            .owner = owner,
            .source_scheme = source_scheme,
            .state = .reserved,
        });
        return .{
            .artifact = artifact_key,
            .owner = owner,
            .template = id,
            .source_scheme = source_scheme,
        };
    }

    pub fn fillEval(
        self: *ConstTemplateTable,
        ref: ConstRef,
        template: ConstEvalTemplate,
    ) void {
        const record = self.recordForRef(ref);
        if (!std.mem.eql(u8, &record.source_scheme.bytes, &template.source_scheme.bytes)) {
            checkedArtifactInvariant("constant eval template source scheme does not match reserved ConstRef", .{});
        }
        switch (record.state) {
            .reserved => record.state = .{ .eval_template = template },
            .eval_template, .value_graph_template => checkedArtifactInvariant("constant template was filled twice", .{}),
        }
    }

    pub fn fillValueGraph(
        self: *ConstTemplateTable,
        ref: ConstRef,
        template: ConstValueGraphTemplate,
    ) void {
        const record = self.recordForRef(ref);
        switch (record.state) {
            .reserved => record.state = .{ .value_graph_template = template },
            .eval_template, .value_graph_template => checkedArtifactInvariant("constant template was filled twice", .{}),
        }
    }

    pub fn get(self: *const ConstTemplateTable, ref: ConstRef) ConstTemplate {
        const idx = @intFromEnum(ref.template);
        if (idx >= self.templates.items.len) {
            checkedArtifactInvariant("ConstRef template id is out of range", .{});
        }
        const template = self.templates.items[idx];
        if (!constOwnerEql(template.owner, ref.owner) or
            !std.mem.eql(u8, &template.source_scheme.bytes, &ref.source_scheme.bytes))
        {
            checkedArtifactInvariant("ConstRef does not match constant template row", .{});
        }
        return template;
    }

    pub fn verifySealed(self: *const ConstTemplateTable) void {
        if (builtin.mode != .Debug) return;

        for (self.templates.items, 0..) |template, i| {
            std.debug.assert(@intFromEnum(template.id) == i);
            switch (template.state) {
                .eval_template, .value_graph_template => {},
                .reserved => std.debug.panic(
                    "checked artifact invariant violated: constant template {d} was not sealed before publication",
                    .{i},
                ),
            }
        }
    }

    fn recordForRef(self: *ConstTemplateTable, ref: ConstRef) *ConstTemplate {
        const idx = @intFromEnum(ref.template);
        if (idx >= self.templates.items.len) {
            checkedArtifactInvariant("ConstRef template id is out of range", .{});
        }
        const record = &self.templates.items[idx];
        if (!constOwnerEql(record.owner, ref.owner) or
            !std.mem.eql(u8, &record.source_scheme.bytes, &ref.source_scheme.bytes))
        {
            checkedArtifactInvariant("ConstRef does not match constant template row", .{});
        }
        return record;
    }

    pub fn deinit(self: *ConstTemplateTable, allocator: Allocator) void {
        self.templates.deinit(allocator);
        self.* = .{};
    }
};

/// Public `ImportedConstTemplateView` declaration.
pub const ImportedConstTemplateView = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
    const_ref: ConstRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    template: ConstTemplate,
    template_closure: ImportedTemplateClosureView = .{},
};

/// Public `ExportedConstTemplateView` declaration.
pub const ExportedConstTemplateView = struct {
    templates: []const ImportedConstTemplateView = &.{},
};

/// Public `ExportedConstTemplateTable` declaration.
pub const ExportedConstTemplateTable = struct {
    templates: []ImportedConstTemplateView = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        published_exports: []const CIR.Def.Idx,
        artifact_key: CheckedModuleArtifactKey,
        checked_types: *const CheckedTypeStore,
        checked_templates: *const CheckedProcedureTemplateTable,
        top_level_values: *const TopLevelValueTable,
        const_templates: *const ConstTemplateTable,
        resolved_value_refs: *const ResolvedValueRefTable,
        top_level_bindings: *const TopLevelProcedureBindingTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        imports: []const PublishImportArtifact,
    ) Allocator.Error!ExportedConstTemplateTable {
        var templates = std.ArrayList(ImportedConstTemplateView).empty;
        errdefer {
            for (templates.items) |*template| deinitImportedTemplateClosure(allocator, &template.template_closure);
            templates.deinit(allocator);
        }

        for (published_exports) |def_idx| {
            const top_level = top_level_values.lookupByDef(def_idx) orelse continue;
            const const_ref = switch (top_level.value) {
                .const_ref => |ref| ref,
                .procedure_binding => continue,
            };
            const template = const_templates.get(const_ref);
            var template_closure = try buildImportedConstTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                const_templates,
                resolved_value_refs,
                top_level_bindings,
                platform_required_bindings,
                imports,
                const_ref,
            );
            errdefer deinitImportedTemplateClosure(allocator, &template_closure);
            try templates.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .def = def_idx,
                .pattern = top_level.pattern,
                .const_ref = const_ref,
                .source_scheme = top_level.source_scheme,
                .template = template,
                .template_closure = template_closure,
            });
            template_closure = .{};
        }

        return .{ .templates = try templates.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *ExportedConstTemplateTable, allocator: Allocator) void {
        for (self.templates) |*template| deinitImportedTemplateClosure(allocator, &template.template_closure);
        allocator.free(self.templates);
        self.* = .{};
    }

    pub fn view(self: *const ExportedConstTemplateTable) ExportedConstTemplateView {
        return .{ .templates = self.templates };
    }
};

/// Public `ConstInstantiationStoreView` declaration.
pub const ConstInstantiationStoreView = struct {
    owner: CheckedModuleArtifactKey = .{},
    instances: []const ConstInstantiationRecord = &.{},
};

/// Public `CallableBindingInstantiationStoreView` declaration.
pub const CallableBindingInstantiationStoreView = struct {
    owner: CheckedModuleArtifactKey = .{},
    instances: []const CallableBindingInstantiationRecord = &.{},
};

/// Public `SemanticInstantiationProcedureTableView` declaration.
pub const SemanticInstantiationProcedureTableView = struct {
    owner: CheckedModuleArtifactKey = .{},
    procedures: []const SemanticInstantiationProcedureRecord = &.{},
};

/// Public `ConstInstantiationKey` declaration.
pub const ConstInstantiationKey = struct {
    const_ref: ConstRef,
    requested_source_ty: canonical.CanonicalTypeKey,
};

/// Public `ConstInstantiationRequest` declaration.
pub const ConstInstantiationRequest = struct {
    key: ConstInstantiationKey,
    requested_source_ty_payload: CheckedTypeId,
};

/// Public `ConstInstanceRef` declaration.
pub const ConstInstanceRef = struct {
    owner: CheckedModuleArtifactKey,
    key: ConstInstantiationKey,
    instance: ConstInstanceId,
};

/// Public `ConstInstance` declaration.
pub const ConstInstance = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    dependency_summary: ?ComptimeDependencySummaryId = null,
    reification_plan: ?ConstReificationPlanId = null,
    generated_procedures: []const SemanticInstantiationProcedureId = &.{},
};

/// Public `ConstInstantiationState` declaration.
pub const ConstInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: ConstInstance,
};

/// Public `ConstInstantiationRecord` declaration.
pub const ConstInstantiationRecord = struct {
    id: ConstInstanceId,
    key: ConstInstantiationKey,
    state: ConstInstantiationState,
};

/// Public `ConstInstantiationStore` declaration.
pub const ConstInstantiationStore = struct {
    owner: CheckedModuleArtifactKey = .{},
    instances: std.ArrayList(ConstInstantiationRecord) = .empty,
    by_key: std.AutoHashMapUnmanaged([32]u8, ConstInstanceId) = .{},

    pub fn init(owner: CheckedModuleArtifactKey) ConstInstantiationStore {
        return .{ .owner = owner };
    }

    pub fn view(self: *const ConstInstantiationStore) ConstInstantiationStoreView {
        return .{
            .owner = self.owner,
            .instances = self.instances.items,
        };
    }

    pub fn reserveRequest(
        self: *ConstInstantiationStore,
        allocator: Allocator,
        checked_types: *const CheckedTypeStore,
        request: ConstInstantiationRequest,
    ) Allocator.Error!ConstInstanceRef {
        verifyConstInstantiationRequest(checked_types, request);
        return try self.reserveKey(allocator, request.key);
    }

    fn reserveKey(
        self: *ConstInstantiationStore,
        allocator: Allocator,
        key: ConstInstantiationKey,
    ) Allocator.Error!ConstInstanceRef {
        const key_hash = hashConstInstantiationKey(key);
        if (self.by_key.get(key_hash)) |existing| {
            return .{ .owner = self.owner, .key = key, .instance = existing };
        }

        const id: ConstInstanceId = @enumFromInt(@as(u32, @intCast(self.instances.items.len)));
        try self.instances.append(allocator, .{
            .id = id,
            .key = key,
            .state = .reserved,
        });
        errdefer _ = self.instances.pop();
        try self.by_key.put(allocator, key_hash, id);

        return .{ .owner = self.owner, .key = key, .instance = id };
    }

    pub fn markEvaluating(self: *ConstInstantiationStore, ref: ConstInstanceRef) void {
        const record = self.recordForRef(ref);
        switch (record.state) {
            .reserved => record.state = .evaluating,
            .evaluating => {},
            .evaluated => checkedArtifactInvariant("constant instance was evaluated twice", .{}),
        }
    }

    pub fn fill(
        self: *ConstInstantiationStore,
        ref: ConstInstanceRef,
        instance: ConstInstance,
    ) void {
        const record = self.recordForRef(ref);
        switch (record.state) {
            .reserved, .evaluating => record.state = .{ .evaluated = instance },
            .evaluated => checkedArtifactInvariant("constant instance was filled twice", .{}),
        }
    }

    pub fn lookup(self: *const ConstInstantiationStore, key: ConstInstantiationKey) ?ConstInstanceRef {
        const id = self.by_key.get(hashConstInstantiationKey(key)) orelse return null;
        return .{ .owner = self.owner, .key = key, .instance = id };
    }

    pub fn stateForRef(self: *const ConstInstantiationStore, ref: ConstInstanceRef) ConstInstantiationState {
        return self.recordForConstRef(ref).state;
    }

    pub fn get(self: *const ConstInstantiationStore, ref: ConstInstanceRef) ConstInstance {
        const record = self.recordForConstRef(ref);
        return switch (record.state) {
            .evaluated => |instance| instance,
            .reserved, .evaluating => checkedArtifactInvariant("constant instance was consumed before it was sealed", .{}),
        };
    }

    pub fn verifySealed(
        self: *const ConstInstantiationStore,
        const_templates: *const ConstTemplateTable,
        comptime_dependencies: *const ComptimeDependencySummaryStore,
        semantic_instantiation_procedures: *const SemanticInstantiationProcedureTable,
    ) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.by_key.count() == self.instances.items.len);
        for (self.instances.items, 0..) |record, i| {
            std.debug.assert(@intFromEnum(record.id) == i);
            const key_hash = hashConstInstantiationKey(record.key);
            const indexed = self.by_key.get(key_hash) orelse {
                std.debug.panic("checked artifact invariant violated: constant instance key was not indexed", .{});
            };
            std.debug.assert(indexed == record.id);
            switch (record.state) {
                .evaluated => |instance| {
                    if (instance.dependency_summary == null) {
                        std.debug.panic(
                            "checked artifact invariant violated: constant instance {d} has no dependency summary",
                            .{i},
                        );
                    }
                    if (@intFromEnum(instance.dependency_summary.?) >= comptime_dependencies.summaries.items.len) {
                        std.debug.panic(
                            "checked artifact invariant violated: constant instance {d} references missing dependency summary",
                            .{i},
                        );
                    }
                    if (std.mem.eql(u8, &record.key.const_ref.artifact.bytes, &self.owner.bytes)) {
                        const template = const_templates.get(record.key.const_ref);
                        switch (template.state) {
                            .eval_template => {
                                if (instance.reification_plan == null) {
                                    std.debug.panic(
                                        "checked artifact invariant violated: eval constant instance {d} has no reification plan",
                                        .{i},
                                    );
                                }
                            },
                            .value_graph_template => {},
                            .reserved => std.debug.panic(
                                "checked artifact invariant violated: constant instance {d} references an unsealed local template",
                                .{i},
                            ),
                        }
                    }
                    for (instance.generated_procedures) |procedure| {
                        const proc_index = @intFromEnum(procedure);
                        if (proc_index >= semantic_instantiation_procedures.procedures.items.len) {
                            std.debug.panic(
                                "checked artifact invariant violated: constant instance {d} references missing generated procedure {d}",
                                .{ i, proc_index },
                            );
                        }
                        const procedure_record = semantic_instantiation_procedures.procedures.items[proc_index];
                        switch (procedure_record.key) {
                            .const_instance_callable_leaf => |leaf| {
                                if (!constInstantiationKeyEql(leaf.instance, record.key)) {
                                    std.debug.panic(
                                        "checked artifact invariant violated: constant instance {d} generated procedure belongs to a different instance",
                                        .{i},
                                    );
                                }
                            },
                            else => std.debug.panic(
                                "checked artifact invariant violated: constant instance {d} generated procedure has the wrong key shape",
                                .{i},
                            ),
                        }
                        switch (procedure_record.state) {
                            .sealed => {},
                            .reserved => std.debug.panic(
                                "checked artifact invariant violated: constant instance {d} references unsealed generated procedure {d}",
                                .{ i, proc_index },
                            ),
                        }
                    }
                },
                .reserved, .evaluating => std.debug.panic(
                    "checked artifact invariant violated: constant instance {d} was not sealed before publication",
                    .{i},
                ),
            }
        }
    }

    fn recordForRef(self: *ConstInstantiationStore, ref: ConstInstanceRef) *ConstInstantiationRecord {
        if (!std.mem.eql(u8, &ref.owner.bytes, &self.owner.bytes)) {
            checkedArtifactInvariant("constant instance ref names the wrong owning artifact", .{});
        }
        const idx = @intFromEnum(ref.instance);
        if (idx >= self.instances.items.len) {
            checkedArtifactInvariant("constant instance ref is out of range", .{});
        }
        const record = &self.instances.items[idx];
        if (!constInstantiationKeyEql(record.key, ref.key)) {
            checkedArtifactInvariant("constant instance ref key does not match reserved row", .{});
        }
        return record;
    }

    fn recordForConstRef(self: *const ConstInstantiationStore, ref: ConstInstanceRef) *const ConstInstantiationRecord {
        if (!std.mem.eql(u8, &ref.owner.bytes, &self.owner.bytes)) {
            checkedArtifactInvariant("constant instance ref names the wrong owning artifact", .{});
        }
        const idx = @intFromEnum(ref.instance);
        if (idx >= self.instances.items.len) {
            checkedArtifactInvariant("constant instance ref is out of range", .{});
        }
        const record = &self.instances.items[idx];
        if (!constInstantiationKeyEql(record.key, ref.key)) {
            checkedArtifactInvariant("constant instance ref key does not match reserved row", .{});
        }
        return record;
    }

    pub fn deinit(self: *ConstInstantiationStore, allocator: Allocator) void {
        for (self.instances.items) |*record| switch (record.state) {
            .evaluated => |instance| allocator.free(instance.generated_procedures),
            .reserved, .evaluating => {},
        };
        self.by_key.deinit(allocator);
        self.instances.deinit(allocator);
        self.* = .{};
    }
};

fn verifyConstInstantiationRequest(
    checked_types: *const CheckedTypeStore,
    request: ConstInstantiationRequest,
) void {
    const idx = @intFromEnum(request.requested_source_ty_payload);
    if (idx >= checked_types.roots.len) {
        checkedArtifactInvariant("constant instantiation request type payload is out of range", .{});
    }
    const payload_key = checked_types.roots[idx].key;
    if (!std.mem.eql(u8, &payload_key.bytes, &request.key.requested_source_ty.bytes)) {
        checkedArtifactInvariant("constant instantiation request key disagrees with checked type payload", .{});
    }
}

/// Public `CallableBindingInstantiationKey` declaration.
pub const CallableBindingInstantiationKey = struct {
    binding: ProcedureBindingRef,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
};

/// Public `CallableBindingInstantiationRequest` declaration.
pub const CallableBindingInstantiationRequest = struct {
    key: CallableBindingInstantiationKey,
    requested_source_fn_ty_payload: CheckedTypeId,
};

/// Public `CallableBindingInstanceRef` declaration.
pub const CallableBindingInstanceRef = struct {
    owner: CheckedModuleArtifactKey,
    key: CallableBindingInstantiationKey,
    instance: CallableBindingInstanceId,
};

/// Public `CallablePromotionOutput` declaration.
pub const CallablePromotionOutput = union(enum) {
    existing_procedure: canonical.ProcedureCallableRef,
    promoted_procedure: PromotedProcedureRef,
};

/// Public `CallableBindingExecutableRoot` declaration.
pub const CallableBindingExecutableRoot = union(enum) {
    local_root: ComptimeRootId,
    concrete_request: CallableBindingInstantiationKey,
};

/// Public `DirectCallableBindingInstance` declaration.
pub const DirectCallableBindingInstance = struct {
    binding: ProcedureBindingRef,
    template: canonical.CallableProcedureTemplateRef,
};

/// Public `EvaluatedCallableBindingInstance` declaration.
pub const EvaluatedCallableBindingInstance = struct {
    executable_root: CallableBindingExecutableRoot,
    result_plan: CallableResultPlanId,
    promotion_plan: ?CallablePromotionPlanId = null,
    promotion_output: CallablePromotionOutput,
};

/// Public `CallableBindingInstanceBody` declaration.
pub const CallableBindingInstanceBody = union(enum) {
    direct: DirectCallableBindingInstance,
    evaluated: EvaluatedCallableBindingInstance,
};

/// Public `CallableBindingInstance` declaration.
pub const CallableBindingInstance = struct {
    key: CallableBindingInstantiationKey,
    dependency_summary: ComptimeDependencySummaryId,
    proc_value: canonical.ProcedureCallableRef,
    body: CallableBindingInstanceBody,
    generated_procedures: []const SemanticInstantiationProcedureId = &.{},
};

/// Public `CallableBindingInstantiationState` declaration.
pub const CallableBindingInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: CallableBindingInstance,
};

/// Public `CallableBindingInstantiationRecord` declaration.
pub const CallableBindingInstantiationRecord = struct {
    id: CallableBindingInstanceId,
    key: CallableBindingInstantiationKey,
    state: CallableBindingInstantiationState,
};

/// Public `CallableBindingInstantiationStore` declaration.
pub const CallableBindingInstantiationStore = struct {
    owner: CheckedModuleArtifactKey = .{},
    instances: std.ArrayList(CallableBindingInstantiationRecord) = .empty,
    by_key: std.AutoHashMapUnmanaged([32]u8, CallableBindingInstanceId) = .{},

    pub fn init(owner: CheckedModuleArtifactKey) CallableBindingInstantiationStore {
        return .{ .owner = owner };
    }

    pub fn view(self: *const CallableBindingInstantiationStore) CallableBindingInstantiationStoreView {
        return .{
            .owner = self.owner,
            .instances = self.instances.items,
        };
    }

    pub fn reserveRequest(
        self: *CallableBindingInstantiationStore,
        allocator: Allocator,
        checked_types: *const CheckedTypeStore,
        request: CallableBindingInstantiationRequest,
    ) Allocator.Error!CallableBindingInstanceRef {
        verifyCallableBindingInstantiationRequest(checked_types, request);
        return try self.reserveKey(allocator, request.key);
    }

    fn reserveKey(
        self: *CallableBindingInstantiationStore,
        allocator: Allocator,
        key: CallableBindingInstantiationKey,
    ) Allocator.Error!CallableBindingInstanceRef {
        const key_hash = hashCallableBindingInstantiationKey(key);
        if (self.by_key.get(key_hash)) |existing| {
            return .{ .owner = self.owner, .key = key, .instance = existing };
        }

        const id: CallableBindingInstanceId = @enumFromInt(@as(u32, @intCast(self.instances.items.len)));
        try self.instances.append(allocator, .{
            .id = id,
            .key = key,
            .state = .reserved,
        });
        errdefer _ = self.instances.pop();
        try self.by_key.put(allocator, key_hash, id);

        return .{ .owner = self.owner, .key = key, .instance = id };
    }

    pub fn markEvaluating(self: *CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) void {
        const record = self.recordForRef(ref);
        switch (record.state) {
            .reserved => record.state = .evaluating,
            .evaluating => {},
            .evaluated => checkedArtifactInvariant("callable binding instance was evaluated twice", .{}),
        }
    }

    pub fn fill(
        self: *CallableBindingInstantiationStore,
        ref: CallableBindingInstanceRef,
        instance: CallableBindingInstance,
    ) void {
        const record = self.recordForRef(ref);
        if (!callableBindingInstantiationKeyEql(instance.key, ref.key)) {
            checkedArtifactInvariant("callable binding instance payload key does not match reserved key", .{});
        }
        switch (record.state) {
            .reserved, .evaluating => record.state = .{ .evaluated = instance },
            .evaluated => checkedArtifactInvariant("callable binding instance was filled twice", .{}),
        }
    }

    pub fn lookup(self: *const CallableBindingInstantiationStore, key: CallableBindingInstantiationKey) ?CallableBindingInstanceRef {
        const id = self.by_key.get(hashCallableBindingInstantiationKey(key)) orelse return null;
        return .{ .owner = self.owner, .key = key, .instance = id };
    }

    pub fn stateForRef(self: *const CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) CallableBindingInstantiationState {
        return self.recordForConstRef(ref).state;
    }

    pub fn get(self: *const CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) CallableBindingInstance {
        const record = self.recordForConstRef(ref);
        return switch (record.state) {
            .evaluated => |instance| instance,
            .reserved, .evaluating => checkedArtifactInvariant("callable binding instance was consumed before it was sealed", .{}),
        };
    }

    pub fn verifySealed(
        self: *const CallableBindingInstantiationStore,
        comptime_dependencies: *const ComptimeDependencySummaryStore,
        plans: *const CompileTimePlanStore,
        roots: *const CompileTimeRootTable,
        promoted_procedures: *const PromotedProcedureTable,
        semantic_instantiation_procedures: *const SemanticInstantiationProcedureTable,
    ) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.by_key.count() == self.instances.items.len);
        for (self.instances.items, 0..) |record, i| {
            std.debug.assert(@intFromEnum(record.id) == i);
            const key_hash = hashCallableBindingInstantiationKey(record.key);
            const indexed = self.by_key.get(key_hash) orelse {
                std.debug.panic("checked artifact invariant violated: callable binding instance key was not indexed", .{});
            };
            std.debug.assert(indexed == record.id);
            switch (record.state) {
                .evaluated => |instance| verifyCallableBindingInstance(
                    i,
                    record.key,
                    instance,
                    comptime_dependencies,
                    plans,
                    roots,
                    promoted_procedures,
                    semantic_instantiation_procedures,
                ),
                .reserved, .evaluating => std.debug.panic(
                    "checked artifact invariant violated: callable binding instance {d} was not sealed before publication",
                    .{i},
                ),
            }
        }
    }

    fn recordForRef(self: *CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) *CallableBindingInstantiationRecord {
        if (!std.mem.eql(u8, &ref.owner.bytes, &self.owner.bytes)) {
            checkedArtifactInvariant("callable binding instance ref names the wrong owning artifact", .{});
        }
        const idx = @intFromEnum(ref.instance);
        if (idx >= self.instances.items.len) {
            checkedArtifactInvariant("callable binding instance ref is out of range", .{});
        }
        const record = &self.instances.items[idx];
        if (!callableBindingInstantiationKeyEql(record.key, ref.key)) {
            checkedArtifactInvariant("callable binding instance ref key does not match reserved row", .{});
        }
        return record;
    }

    fn recordForConstRef(self: *const CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) *const CallableBindingInstantiationRecord {
        if (!std.mem.eql(u8, &ref.owner.bytes, &self.owner.bytes)) {
            checkedArtifactInvariant("callable binding instance ref names the wrong owning artifact", .{});
        }
        const idx = @intFromEnum(ref.instance);
        if (idx >= self.instances.items.len) {
            checkedArtifactInvariant("callable binding instance ref is out of range", .{});
        }
        const record = &self.instances.items[idx];
        if (!callableBindingInstantiationKeyEql(record.key, ref.key)) {
            checkedArtifactInvariant("callable binding instance ref key does not match reserved row", .{});
        }
        return record;
    }

    pub fn deinit(self: *CallableBindingInstantiationStore, allocator: Allocator) void {
        for (self.instances.items) |*record| switch (record.state) {
            .evaluated => |instance| allocator.free(instance.generated_procedures),
            .reserved, .evaluating => {},
        };
        self.by_key.deinit(allocator);
        self.instances.deinit(allocator);
        self.* = .{};
    }
};

fn verifyCallableBindingInstantiationRequest(
    checked_types: *const CheckedTypeStore,
    request: CallableBindingInstantiationRequest,
) void {
    const idx = @intFromEnum(request.requested_source_fn_ty_payload);
    if (idx >= checked_types.roots.len) {
        checkedArtifactInvariant("callable binding instantiation request type payload is out of range", .{});
    }
    const payload_key = checked_types.roots[idx].key;
    if (!std.mem.eql(u8, &payload_key.bytes, &request.key.requested_source_fn_ty.bytes)) {
        checkedArtifactInvariant("callable binding instantiation request key disagrees with checked type payload", .{});
    }
}

fn verifyCallableBindingInstance(
    index: usize,
    key: CallableBindingInstantiationKey,
    instance: CallableBindingInstance,
    comptime_dependencies: *const ComptimeDependencySummaryStore,
    plans: *const CompileTimePlanStore,
    roots: *const CompileTimeRootTable,
    promoted_procedures: *const PromotedProcedureTable,
    semantic_instantiation_procedures: *const SemanticInstantiationProcedureTable,
) void {
    if (!callableBindingInstantiationKeyEql(instance.key, key)) {
        std.debug.panic("checked artifact invariant violated: callable binding instance {d} payload key does not match row key", .{index});
    }
    if (@intFromEnum(instance.dependency_summary) >= comptime_dependencies.summaries.items.len) {
        std.debug.panic("checked artifact invariant violated: callable binding instance {d} references missing dependency summary", .{index});
    }
    if (!std.mem.eql(u8, &instance.proc_value.source_fn_ty.bytes, &key.requested_source_fn_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: callable binding instance {d} proc value type differs from row key", .{index});
    }

    switch (instance.body) {
        .direct => |direct| verifyDirectCallableBindingInstance(index, key, instance.proc_value, direct),
        .evaluated => |evaluated| verifyEvaluatedCallableBindingInstance(
            index,
            key,
            instance.proc_value,
            evaluated,
            plans,
            roots,
            promoted_procedures,
        ),
    }

    for (instance.generated_procedures) |procedure| {
        const proc_index = @intFromEnum(procedure);
        if (proc_index >= semantic_instantiation_procedures.procedures.items.len) {
            std.debug.panic(
                "checked artifact invariant violated: callable binding instance {d} references missing generated procedure {d}",
                .{ index, proc_index },
            );
        }
        const record = semantic_instantiation_procedures.procedures.items[proc_index];
        switch (record.key) {
            .callable_binding_promoted_leaf => |leaf| {
                if (!callableBindingInstantiationKeyEql(leaf.instance, key)) {
                    std.debug.panic(
                        "checked artifact invariant violated: callable binding instance {d} generated procedure belongs to a different instance",
                        .{index},
                    );
                }
            },
            else => std.debug.panic(
                "checked artifact invariant violated: callable binding instance {d} generated procedure has the wrong key shape",
                .{index},
            ),
        }
        switch (record.state) {
            .sealed => {},
            .reserved => std.debug.panic(
                "checked artifact invariant violated: callable binding instance {d} references unsealed generated procedure {d}",
                .{ index, proc_index },
            ),
        }
    }
}

fn verifyDirectCallableBindingInstance(
    index: usize,
    key: CallableBindingInstantiationKey,
    proc_value: canonical.ProcedureCallableRef,
    direct: DirectCallableBindingInstance,
) void {
    if (!procedureBindingRefEql(direct.binding, key.binding)) {
        std.debug.panic("checked artifact invariant violated: direct callable binding instance {d} body binding differs from row key", .{index});
    }
    if (!canonical.callableProcedureTemplateRefEql(direct.template, proc_value.template)) {
        std.debug.panic("checked artifact invariant violated: direct callable binding instance {d} body template differs from proc value", .{index});
    }
}

fn verifyEvaluatedCallableBindingInstance(
    index: usize,
    key: CallableBindingInstantiationKey,
    proc_value: canonical.ProcedureCallableRef,
    evaluated: EvaluatedCallableBindingInstance,
    plans: *const CompileTimePlanStore,
    roots: *const CompileTimeRootTable,
    promoted_procedures: *const PromotedProcedureTable,
) void {
    verifyCallableResultRef(plans, evaluated.result_plan);

    switch (evaluated.executable_root) {
        .local_root => |root| {
            const root_index = @intFromEnum(root);
            if (root_index >= roots.roots.len) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} executable root is out of range", .{index});
            }
            if (roots.roots[root_index].kind != .callable_binding) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} executable root is not callable", .{index});
            }
        },
        .concrete_request => |request_key| {
            if (!callableBindingInstantiationKeyEql(request_key, key)) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} concrete executable request key differs from row key", .{index});
            }
        },
    }

    switch (evaluated.promotion_output) {
        .existing_procedure => |proc| {
            if (evaluated.promotion_plan != null) {
                std.debug.panic("checked artifact invariant violated: existing callable binding instance {d} unexpectedly has a promotion plan", .{index});
            }
            if (!canonical.procedureCallableRefEql(proc_value, proc)) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} final proc value differs from existing procedure output", .{index});
            }
        },
        .promoted_procedure => |promoted| {
            const plan = evaluated.promotion_plan orelse {
                std.debug.panic("checked artifact invariant violated: promoted callable binding instance {d} has no promotion plan", .{index});
            };
            verifyCallablePromotionRef(plans, plan);
            const promoted_record = promoted_procedures.get(promoted) orelse {
                std.debug.panic("checked artifact invariant violated: promoted callable binding instance {d} references a missing promoted procedure", .{index});
            };
            const expected = canonical.ProcedureCallableRef{
                .template = .{ .synthetic = .{ .template = promoted_record.template } },
                .source_fn_ty = key.requested_source_fn_ty,
            };
            if (!canonical.procedureCallableRefEql(proc_value, expected)) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} final proc value differs from promoted procedure output", .{index});
            }
        },
    }
}

/// Public `SemanticInstantiationProcedureKey` declaration.
pub const SemanticInstantiationProcedureKey = union(enum) {
    const_instance_callable_leaf: struct {
        instance: ConstInstantiationKey,
        value_path: ComptimeValuePathKey,
        source_fn_ty: canonical.CanonicalTypeKey,
    },
    callable_binding_promoted_leaf: struct {
        instance: CallableBindingInstantiationKey,
        callable_path: PromotedCallablePathKey,
        source_fn_ty: canonical.CanonicalTypeKey,
    },
    private_capture_callable_leaf: struct {
        promoted_proc: PromotedProcedureRef,
        capture_path: PrivateCapturePathKey,
        source_fn_ty: canonical.CanonicalTypeKey,
    },
};

/// Public `SemanticInstantiationProcedure` declaration.
pub const SemanticInstantiationProcedure = struct {
    template: canonical.CallableProcedureTemplateRef,
    proc_value: canonical.ProcedureValueRef,
    promoted: ?PromotedProcedureRef = null,
};

/// Public `SemanticInstantiationProcedureState` declaration.
pub const SemanticInstantiationProcedureState = union(enum) {
    reserved,
    sealed: SemanticInstantiationProcedure,
};

/// Public `SemanticInstantiationProcedureRecord` declaration.
pub const SemanticInstantiationProcedureRecord = struct {
    id: SemanticInstantiationProcedureId,
    key: SemanticInstantiationProcedureKey,
    state: SemanticInstantiationProcedureState,
};

/// Public `SemanticInstantiationProcedureTable` declaration.
pub const SemanticInstantiationProcedureTable = struct {
    owner: CheckedModuleArtifactKey = .{},
    procedures: std.ArrayList(SemanticInstantiationProcedureRecord) = .empty,
    by_key: std.AutoHashMapUnmanaged([32]u8, SemanticInstantiationProcedureId) = .{},

    pub fn init(owner: CheckedModuleArtifactKey) SemanticInstantiationProcedureTable {
        return .{ .owner = owner };
    }

    pub fn view(self: *const SemanticInstantiationProcedureTable) SemanticInstantiationProcedureTableView {
        return .{
            .owner = self.owner,
            .procedures = self.procedures.items,
        };
    }

    pub fn reserve(
        self: *SemanticInstantiationProcedureTable,
        allocator: Allocator,
        key: SemanticInstantiationProcedureKey,
    ) Allocator.Error!SemanticInstantiationProcedureId {
        const key_hash = hashSemanticInstantiationProcedureKey(key);
        if (self.by_key.get(key_hash)) |existing| return existing;

        const id: SemanticInstantiationProcedureId = @enumFromInt(@as(u32, @intCast(self.procedures.items.len)));
        try self.procedures.append(allocator, .{
            .id = id,
            .key = key,
            .state = .reserved,
        });
        errdefer _ = self.procedures.pop();
        try self.by_key.put(allocator, key_hash, id);
        return id;
    }

    pub fn fill(
        self: *SemanticInstantiationProcedureTable,
        id: SemanticInstantiationProcedureId,
        key: SemanticInstantiationProcedureKey,
        procedure: SemanticInstantiationProcedure,
    ) void {
        const record = self.recordFor(id);
        if (!semanticInstantiationProcedureKeyEql(record.key, key)) {
            checkedArtifactInvariant("semantic instantiation procedure key does not match reserved row", .{});
        }
        switch (record.state) {
            .reserved => record.state = .{ .sealed = procedure },
            .sealed => checkedArtifactInvariant("semantic instantiation procedure was filled twice", .{}),
        }
    }

    pub fn publish(
        self: *SemanticInstantiationProcedureTable,
        allocator: Allocator,
        key: SemanticInstantiationProcedureKey,
        procedure: SemanticInstantiationProcedure,
    ) Allocator.Error!SemanticInstantiationProcedureId {
        const id = try self.reserve(allocator, key);
        const record = self.recordFor(id);
        if (!semanticInstantiationProcedureKeyEql(record.key, key)) {
            checkedArtifactInvariant("semantic instantiation procedure key does not match reserved row", .{});
        }
        switch (record.state) {
            .reserved => record.state = .{ .sealed = procedure },
            .sealed => |existing| if (!semanticInstantiationProcedureEql(existing, procedure)) {
                checkedArtifactInvariant("semantic instantiation procedure was republished with different data", .{});
            },
        }
        return id;
    }

    pub fn lookup(self: *const SemanticInstantiationProcedureTable, key: SemanticInstantiationProcedureKey) ?SemanticInstantiationProcedureId {
        return self.by_key.get(hashSemanticInstantiationProcedureKey(key));
    }

    pub fn get(self: *const SemanticInstantiationProcedureTable, id: SemanticInstantiationProcedureId) SemanticInstantiationProcedure {
        const idx = @intFromEnum(id);
        if (idx >= self.procedures.items.len) {
            checkedArtifactInvariant("semantic instantiation procedure id is out of range", .{});
        }
        return switch (self.procedures.items[idx].state) {
            .sealed => |procedure| procedure,
            .reserved => checkedArtifactInvariant("semantic instantiation procedure was consumed before it was sealed", .{}),
        };
    }

    pub fn verifySealed(
        self: *const SemanticInstantiationProcedureTable,
        promoted_procedures: *const PromotedProcedureTable,
    ) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.by_key.count() == self.procedures.items.len);
        for (self.procedures.items, 0..) |record, i| {
            std.debug.assert(@intFromEnum(record.id) == i);
            const key_hash = hashSemanticInstantiationProcedureKey(record.key);
            const indexed = self.by_key.get(key_hash) orelse {
                std.debug.panic("checked artifact invariant violated: semantic instantiation procedure key was not indexed", .{});
            };
            std.debug.assert(indexed == record.id);
            switch (record.state) {
                .sealed => |procedure| verifySemanticInstantiationProcedure(
                    i,
                    record.key,
                    procedure,
                    promoted_procedures,
                ),
                .reserved => std.debug.panic(
                    "checked artifact invariant violated: semantic instantiation procedure {d} was not sealed before publication",
                    .{i},
                ),
            }
        }
    }

    fn recordFor(self: *SemanticInstantiationProcedureTable, id: SemanticInstantiationProcedureId) *SemanticInstantiationProcedureRecord {
        const idx = @intFromEnum(id);
        if (idx >= self.procedures.items.len) {
            checkedArtifactInvariant("semantic instantiation procedure id is out of range", .{});
        }
        return &self.procedures.items[idx];
    }

    pub fn deinit(self: *SemanticInstantiationProcedureTable, allocator: Allocator) void {
        self.by_key.deinit(allocator);
        self.procedures.deinit(allocator);
        self.* = .{};
    }
};

fn verifySemanticInstantiationProcedure(
    index: usize,
    key: SemanticInstantiationProcedureKey,
    procedure: SemanticInstantiationProcedure,
    promoted_procedures: *const PromotedProcedureTable,
) void {
    if (procedure.promoted) |promoted| {
        const promoted_record = promoted_procedures.get(promoted) orelse {
            std.debug.panic(
                "checked artifact invariant violated: semantic instantiation procedure {d} references a missing promoted procedure",
                .{index},
            );
        };
        if (!canonical.procedureValueRefEql(procedure.proc_value, promoted_record.proc)) {
            std.debug.panic(
                "checked artifact invariant violated: semantic instantiation procedure {d} proc value differs from promoted procedure",
                .{index},
            );
        }
        switch (procedure.template) {
            .synthetic => |synthetic| {
                if (!canonical.procedureTemplateRefEql(synthetic.template, promoted_record.template)) {
                    std.debug.panic(
                        "checked artifact invariant violated: semantic instantiation procedure {d} template differs from promoted procedure",
                        .{index},
                    );
                }
            },
            .checked,
            .lifted,
            => std.debug.panic(
                "checked artifact invariant violated: promoted semantic instantiation procedure {d} did not use a synthetic template",
                .{index},
            ),
        }
        if (!std.mem.eql(u8, &semanticInstantiationProcedureKeySourceTy(key).bytes, &promoted_record.source_fn_ty.bytes)) {
            std.debug.panic(
                "checked artifact invariant violated: semantic instantiation procedure {d} source type differs from promoted procedure",
                .{index},
            );
        }
    }

    switch (key) {
        .private_capture_callable_leaf => |private| {
            _ = promoted_procedures.get(private.promoted_proc) orelse {
                std.debug.panic(
                    "checked artifact invariant violated: private-capture semantic instantiation procedure {d} references a missing owner promoted procedure",
                    .{index},
                );
            };
        },
        .const_instance_callable_leaf,
        .callable_binding_promoted_leaf,
        => {},
    }
}

fn semanticInstantiationProcedureKeySourceTy(key: SemanticInstantiationProcedureKey) canonical.CanonicalTypeKey {
    return switch (key) {
        .const_instance_callable_leaf => |leaf| leaf.source_fn_ty,
        .callable_binding_promoted_leaf => |leaf| leaf.source_fn_ty,
        .private_capture_callable_leaf => |leaf| leaf.source_fn_ty,
    };
}

fn checkedArtifactInvariant(comptime message: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("checked artifact invariant violated: " ++ message, args);
    }
    unreachable;
}

fn checkedArtifactKeyEql(a: CheckedModuleArtifactKey, b: CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn closureArtifactRefIsLocal(
    artifact: *const CheckedModuleArtifact,
    referenced: CheckedModuleArtifactKey,
) bool {
    return checkedArtifactKeyEql(referenced, artifact.key);
}

fn hashEnumValue(hasher: *std.crypto.hash.sha2.Sha256, value: anytype) void {
    hashU32(hasher, @as(u32, @intCast(@intFromEnum(value))));
}

fn hashCheckedModuleArtifactKey(hasher: *std.crypto.hash.sha2.Sha256, key: CheckedModuleArtifactKey) void {
    hasher.update(&key.bytes);
}

fn hashArtifactRef(hasher: *std.crypto.hash.sha2.Sha256, ref: canonical.ArtifactRef) void {
    hasher.update(&ref.bytes);
}

fn hashCanonicalTypeKey(hasher: *std.crypto.hash.sha2.Sha256, key: canonical.CanonicalTypeKey) void {
    hasher.update(&key.bytes);
}

fn hashCanonicalTypeSchemeKey(hasher: *std.crypto.hash.sha2.Sha256, key: canonical.CanonicalTypeSchemeKey) void {
    hasher.update(&key.bytes);
}

fn hashProcedureValueRef(hasher: *std.crypto.hash.sha2.Sha256, ref: canonical.ProcedureValueRef) void {
    hashArtifactRef(hasher, ref.artifact);
    hashEnumValue(hasher, ref.proc_base);
}

fn hashProcedureTemplateRef(hasher: *std.crypto.hash.sha2.Sha256, ref: canonical.ProcedureTemplateRef) void {
    hashArtifactRef(hasher, ref.artifact);
    hashEnumValue(hasher, ref.proc_base);
    hashEnumValue(hasher, ref.template);
}

fn hashTopLevelValueRef(hasher: *std.crypto.hash.sha2.Sha256, ref: TopLevelValueRef) void {
    hashCheckedModuleArtifactKey(hasher, ref.artifact);
    hashEnumValue(hasher, ref.pattern);
}

fn hashHostedProcRef(hasher: *std.crypto.hash.sha2.Sha256, ref: HostedProcRef) void {
    hashU32(hasher, ref.module_idx);
    hashEnumValue(hasher, ref.def);
    hashProcedureValueRef(hasher, ref.proc);
    hashProcedureTemplateRef(hasher, ref.template);
}

fn hashImportedProcedureBindingRef(hasher: *std.crypto.hash.sha2.Sha256, ref: ImportedProcedureBindingRef) void {
    hashCheckedModuleArtifactKey(hasher, ref.artifact);
    hashEnumValue(hasher, ref.def);
    hashEnumValue(hasher, ref.pattern);
}

fn hashRequiredAppProcedureRef(hasher: *std.crypto.hash.sha2.Sha256, ref: RequiredAppProcedureRef) void {
    hashCheckedModuleArtifactKey(hasher, ref.artifact);
    hashTopLevelValueRef(hasher, ref.app_value);
    hashEnumValue(hasher, ref.procedure_binding);
}

fn hashPromotedProcedureRef(hasher: *std.crypto.hash.sha2.Sha256, ref: PromotedProcedureRef) void {
    hashU32(hasher, ref.module_idx);
    hashProcedureValueRef(hasher, ref.proc);
}

fn hashPromotedCaptureId(hasher: *std.crypto.hash.sha2.Sha256, capture: PromotedCaptureId) void {
    hashPromotedProcedureRef(hasher, capture.promoted_proc);
    hashU32(hasher, capture.capture_index);
}

fn hashConstOwner(hasher: *std.crypto.hash.sha2.Sha256, owner: ConstOwner) void {
    switch (owner) {
        .top_level_binding => |top_level| {
            hasher.update(&[_]u8{0});
            hashU32(hasher, top_level.module_idx);
            hashEnumValue(hasher, top_level.pattern);
        },
        .promoted_capture => |capture| {
            hasher.update(&[_]u8{1});
            hashPromotedCaptureId(hasher, capture);
        },
    }
}

fn hashProcedureBindingRef(hasher: *std.crypto.hash.sha2.Sha256, ref: ProcedureBindingRef) void {
    switch (ref) {
        .top_level => |binding| {
            hasher.update(&[_]u8{0});
            hashEnumValue(hasher, binding);
        },
        .imported => |imported| {
            hasher.update(&[_]u8{1});
            hashImportedProcedureBindingRef(hasher, imported);
        },
        .hosted => |hosted| {
            hasher.update(&[_]u8{2});
            hashHostedProcRef(hasher, hosted);
        },
        .platform_required => |required| {
            hasher.update(&[_]u8{3});
            hashRequiredAppProcedureRef(hasher, required);
        },
        .promoted => |promoted| {
            hasher.update(&[_]u8{4});
            hashPromotedProcedureRef(hasher, promoted);
        },
    }
}

fn hashConstRef(hasher: *std.crypto.hash.sha2.Sha256, ref: ConstRef) void {
    hashCheckedModuleArtifactKey(hasher, ref.artifact);
    hashConstOwner(hasher, ref.owner);
    hashEnumValue(hasher, ref.template);
    hashCanonicalTypeSchemeKey(hasher, ref.source_scheme);
}

fn hashConstInstantiationKey(key: ConstInstantiationKey) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashConstRef(&hasher, key.const_ref);
    hashCanonicalTypeKey(&hasher, key.requested_source_ty);
    return hasher.finalResult();
}

fn hashCallableBindingInstantiationKey(key: CallableBindingInstantiationKey) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashProcedureBindingRef(&hasher, key.binding);
    hashCanonicalTypeKey(&hasher, key.requested_source_fn_ty);
    return hasher.finalResult();
}

fn hashSemanticInstantiationProcedureKey(key: SemanticInstantiationProcedureKey) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    switch (key) {
        .const_instance_callable_leaf => |leaf| {
            hasher.update(&[_]u8{0});
            const instance_key = hashConstInstantiationKey(leaf.instance);
            hasher.update(&instance_key);
            hasher.update(&leaf.value_path.bytes);
            hashCanonicalTypeKey(&hasher, leaf.source_fn_ty);
        },
        .callable_binding_promoted_leaf => |leaf| {
            hasher.update(&[_]u8{1});
            const instance_key = hashCallableBindingInstantiationKey(leaf.instance);
            hasher.update(&instance_key);
            hasher.update(&leaf.callable_path.bytes);
            hashCanonicalTypeKey(&hasher, leaf.source_fn_ty);
        },
        .private_capture_callable_leaf => |leaf| {
            hasher.update(&[_]u8{2});
            hashPromotedProcedureRef(&hasher, leaf.promoted_proc);
            hasher.update(&leaf.capture_path.bytes);
            hashCanonicalTypeKey(&hasher, leaf.source_fn_ty);
        },
    }
    return hasher.finalResult();
}

fn constRefEql(a: ConstRef, b: ConstRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        constOwnerEql(a.owner, b.owner) and
        a.template == b.template and
        std.mem.eql(u8, &a.source_scheme.bytes, &b.source_scheme.bytes);
}

/// Public `constInstantiationKeyEql` function.
pub fn constInstantiationKeyEql(a: ConstInstantiationKey, b: ConstInstantiationKey) bool {
    return constRefEql(a.const_ref, b.const_ref) and
        std.mem.eql(u8, &a.requested_source_ty.bytes, &b.requested_source_ty.bytes);
}

fn importedProcedureBindingRefEql(a: ImportedProcedureBindingRef, b: ImportedProcedureBindingRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        a.def == b.def and
        a.pattern == b.pattern;
}

fn topLevelValueRefEql(a: TopLevelValueRef, b: TopLevelValueRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and a.pattern == b.pattern;
}

fn hostedProcRefEql(a: HostedProcRef, b: HostedProcRef) bool {
    return a.module_idx == b.module_idx and
        a.def == b.def and
        canonical.procedureValueRefEql(a.proc, b.proc) and
        canonical.procedureTemplateRefEql(a.template, b.template);
}

fn requiredAppProcedureRefEql(a: RequiredAppProcedureRef, b: RequiredAppProcedureRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        topLevelValueRefEql(a.app_value, b.app_value) and
        a.procedure_binding == b.procedure_binding;
}

fn promotedProcedureRefEql(a: PromotedProcedureRef, b: PromotedProcedureRef) bool {
    return a.module_idx == b.module_idx and canonical.procedureValueRefEql(a.proc, b.proc);
}

fn optionalCheckedPatternIdEql(a: ?CheckedPatternId, b: ?CheckedPatternId) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return a.? == b.?;
}

fn promotedProcedureProvenanceEql(
    a: PromotedProcedureProvenance,
    b: PromotedProcedureProvenance,
) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .local_callable_root_result => |left| blk: {
            const right = b.local_callable_root_result;
            break :blk left.root == right.root and left.result_plan == right.result_plan;
        },
        .local_const_root_callable_leaf => |left| blk: {
            const right = b.local_const_root_callable_leaf;
            break :blk left.root == right.root and
                constInstantiationKeyEql(left.instance, right.instance) and
                left.result_plan == right.result_plan and
                std.mem.eql(u8, &left.value_path.bytes, &right.value_path.bytes);
        },
        .callable_binding_instance_result => |left| blk: {
            const right = b.callable_binding_instance_result;
            break :blk callableBindingInstantiationKeyEql(left.instance, right.instance) and
                left.result_plan == right.result_plan and
                std.mem.eql(u8, &left.callable_path.bytes, &right.callable_path.bytes);
        },
        .const_instance_callable_leaf => |left| blk: {
            const right = b.const_instance_callable_leaf;
            break :blk constInstantiationKeyEql(left.instance, right.instance) and
                left.result_plan == right.result_plan and
                std.mem.eql(u8, &left.value_path.bytes, &right.value_path.bytes);
        },
        .private_capture_callable_leaf => |left| blk: {
            const right = b.private_capture_callable_leaf;
            break :blk promotedProcedureRefEql(left.promoted_proc, right.promoted_proc) and
                left.result_plan == right.result_plan and
                std.mem.eql(u8, &left.capture_path.bytes, &right.capture_path.bytes);
        },
    };
}

fn promotedCaptureIdEql(a: PromotedCaptureId, b: PromotedCaptureId) bool {
    return promotedProcedureRefEql(a.promoted_proc, b.promoted_proc) and a.capture_index == b.capture_index;
}

fn constOwnerEql(a: ConstOwner, b: ConstOwner) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .top_level_binding => |left| blk: {
            const right = b.top_level_binding;
            break :blk left.module_idx == right.module_idx and left.pattern == right.pattern;
        },
        .promoted_capture => |left| promotedCaptureIdEql(left, b.promoted_capture),
    };
}

fn constRefTopLevelOwner(ref: ConstRef) ?ConstTopLevelOwner {
    return switch (ref.owner) {
        .top_level_binding => |owner| owner,
        .promoted_capture => null,
    };
}

/// Public `procedureBindingRefEql` function.
pub fn procedureBindingRefEql(a: ProcedureBindingRef, b: ProcedureBindingRef) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .top_level => |left| left == b.top_level,
        .imported => |left| importedProcedureBindingRefEql(left, b.imported),
        .hosted => |left| hostedProcRefEql(left, b.hosted),
        .platform_required => |left| requiredAppProcedureRefEql(left, b.platform_required),
        .promoted => |left| promotedProcedureRefEql(left, b.promoted),
    };
}

/// Public `callableBindingInstantiationKeyEql` function.
pub fn callableBindingInstantiationKeyEql(a: CallableBindingInstantiationKey, b: CallableBindingInstantiationKey) bool {
    return procedureBindingRefEql(a.binding, b.binding) and
        std.mem.eql(u8, &a.requested_source_fn_ty.bytes, &b.requested_source_fn_ty.bytes);
}

fn semanticInstantiationProcedureKeyEql(a: SemanticInstantiationProcedureKey, b: SemanticInstantiationProcedureKey) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .const_instance_callable_leaf => |left| blk: {
            const right = b.const_instance_callable_leaf;
            break :blk constInstantiationKeyEql(left.instance, right.instance) and
                std.mem.eql(u8, &left.value_path.bytes, &right.value_path.bytes) and
                std.mem.eql(u8, &left.source_fn_ty.bytes, &right.source_fn_ty.bytes);
        },
        .callable_binding_promoted_leaf => |left| blk: {
            const right = b.callable_binding_promoted_leaf;
            break :blk callableBindingInstantiationKeyEql(left.instance, right.instance) and
                std.mem.eql(u8, &left.callable_path.bytes, &right.callable_path.bytes) and
                std.mem.eql(u8, &left.source_fn_ty.bytes, &right.source_fn_ty.bytes);
        },
        .private_capture_callable_leaf => |left| blk: {
            const right = b.private_capture_callable_leaf;
            break :blk promotedProcedureRefEql(left.promoted_proc, right.promoted_proc) and
                std.mem.eql(u8, &left.capture_path.bytes, &right.capture_path.bytes) and
                std.mem.eql(u8, &left.source_fn_ty.bytes, &right.source_fn_ty.bytes);
        },
    };
}

fn semanticInstantiationProcedureEql(a: SemanticInstantiationProcedure, b: SemanticInstantiationProcedure) bool {
    const promoted_matches = if (a.promoted == null and b.promoted == null)
        true
    else if (a.promoted == null or b.promoted == null)
        false
    else
        promotedProcedureRefEql(a.promoted.?, b.promoted.?);

    return canonical.callableProcedureTemplateRefEql(a.template, b.template) and
        canonical.procedureValueRefEql(a.proc_value, b.proc_value) and
        promoted_matches;
}

/// Public `CheckedModuleArtifact` declaration.
pub const CheckedModuleArtifact = struct {
    key: CheckedModuleArtifactKey,
    canonical_names: canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    checking_context_identity: CheckingContextIdentity,
    direct_import_artifact_keys: []CheckedModuleArtifactKey = &.{},
    module_env: ModuleEnvStorage,
    exports: ExportTable,
    checked_types: CheckedTypeStore = .{},
    checked_bodies: CheckedBodyStore = .{},
    checked_const_bodies: CheckedConstBodyTable = .{},
    exported_procedure_templates: ExportedProcedureTemplateTable = .{},
    exported_procedure_bindings: ExportedProcedureBindingTable = .{},
    exported_const_templates: ExportedConstTemplateTable = .{},
    provides_requires: ProvidesRequiresMetadata,
    method_registry: static_dispatch.MethodRegistry,
    static_dispatch_plans: static_dispatch.StaticDispatchPlanTable,
    resolved_value_refs: ResolvedValueRefTable,
    nested_proc_sites: NestedProcSiteTable = .{},
    checked_procedure_templates: CheckedProcedureTemplateTable,
    entry_wrappers: EntryWrapperTable = .{},
    intrinsic_wrappers: IntrinsicWrapperTable = .{},
    promoted_callable_wrappers: PromotedCallableWrapperTable = .{},
    promoted_callable_body_plans: PromotedCallableBodyPlanTable = .{},
    executable_type_payloads: ExecutableTypePayloadStore,
    executable_value_transforms: ExecutableValueTransformPlanStore = .{},
    callable_set_descriptors: CallableSetDescriptorStore = .{},
    erased_fn_abis: canonical.ErasedFnAbiStore = .{},
    top_level_procedure_bindings: TopLevelProcedureBindingTable,
    callable_eval_templates: CallableEvalTemplateTable = .{},
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_declarations: PlatformRequiredDeclarationTable,
    platform_requirement_relations: PlatformRequirementRelationTable = .{},
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    compile_time_roots: CompileTimeRootTable,
    top_level_values: TopLevelValueTable,
    comptime_plans: CompileTimePlanStore = .{},
    comptime_dependencies: ComptimeDependencySummaryStore = .{},
    promoted_procedures: PromotedProcedureTable,
    const_templates: ConstTemplateTable,
    comptime_values: CompileTimeValueStore,
    const_instances: ConstInstantiationStore,
    callable_binding_instances: CallableBindingInstantiationStore,
    semantic_instantiation_procedures: SemanticInstantiationProcedureTable,

    pub fn moduleEnv(self: *CheckedModuleArtifact) *ModuleEnv {
        return self.module_env.env();
    }

    pub fn moduleEnvConst(self: *const CheckedModuleArtifact) *const ModuleEnv {
        return self.module_env.envConst();
    }

    pub fn platformRequirementContextKey(self: *const CheckedModuleArtifact) PlatformRequirementContextKey {
        return PlatformRequirementContextKey.compute(
            self.module_identity,
            self.platform_required_declarations.identityHash(&self.canonical_names),
        );
    }

    pub fn appendPromotedCallableWrapper(
        self: *CheckedModuleArtifact,
        allocator: Allocator,
        source_binding: ?CheckedPatternId,
        checked_fn_root: CheckedTypeId,
        checked_fn_scheme: canonical.CanonicalTypeSchemeKey,
        provenance: PromotedProcedureProvenance,
        body_plan: PromotedCallableBodyPlan,
    ) Allocator.Error!PromotedProcedureRef {
        const reserved = try self.reservePromotedCallableWrapper(
            allocator,
            source_binding,
            checked_fn_root,
            checked_fn_scheme,
            provenance,
        );
        self.promoted_callable_body_plans.fill(reserved.body_plan, body_plan);
        try self.publishPromotedCallableWrapper(allocator, reserved);
        return reserved.promoted_ref;
    }

    pub fn reservePromotedCallableWrapper(
        self: *CheckedModuleArtifact,
        allocator: Allocator,
        source_binding: ?CheckedPatternId,
        checked_fn_root: CheckedTypeId,
        checked_fn_scheme: canonical.CanonicalTypeSchemeKey,
        provenance: PromotedProcedureProvenance,
    ) Allocator.Error!ReservedPromotedCallableWrapper {
        const body_plan_id = try self.promoted_callable_body_plans.reserve(allocator);
        const wrapper_id: canonical.PromotedCallableWrapperId = @enumFromInt(@as(u32, @intCast(self.promoted_callable_wrappers.wrappers.len)));
        const proc_base = try self.canonical_names.internProcBase(.{
            .module_name = self.module_identity.module_name,
            .export_name = null,
            .kind = .promoted_callable_wrapper,
            .ordinal = @intFromEnum(wrapper_id),
            .source_def_idx = null,
        });
        const owner_artifact = artifactRef(self.key);
        const proc_value = canonical.ProcedureValueRef{
            .artifact = owner_artifact,
            .proc_base = proc_base,
        };
        const template_id: canonical.CheckedProcedureTemplateId = @enumFromInt(@as(u32, @intCast(self.checked_procedure_templates.templates.len)));
        const template_ref = canonical.ProcedureTemplateRef{
            .artifact = owner_artifact,
            .proc_base = proc_base,
            .template = template_id,
        };
        const source_fn_ty = self.checked_types.roots[@intFromEnum(checked_fn_root)].key;

        const appended_wrapper = try self.promoted_callable_wrappers.append(allocator, .{
            .id = wrapper_id,
            .promoted_proc = proc_value,
            .proc_base_key = proc_base,
            .callable_node = @enumFromInt(@intFromEnum(wrapper_id)),
            .source_binding = source_binding,
            .source_fn_ty = source_fn_ty,
            .provenance = provenance,
            .checked_fn_root = checked_fn_root,
            .body_plan = body_plan_id,
        });
        if (appended_wrapper != wrapper_id) {
            checkedArtifactInvariant("promoted callable wrapper append returned the wrong id", .{});
        }

        try self.checked_procedure_templates.appendTemplate(allocator, .{
            .proc_base = proc_base,
            .template_id = template_id,
            .body = .{ .promoted_callable_wrapper = wrapper_id },
            .checked_fn_scheme = checked_fn_scheme,
            .checked_fn_root = checked_fn_root,
            .static_dispatch_plans = .{},
            .resolved_value_refs = .{},
            .top_level_value_uses = .{},
            .nested_proc_sites = .{},
            .target = .promoted_callable,
        });

        return .{
            .promoted_ref = .{
                .module_idx = self.module_identity.module_idx,
                .proc = proc_value,
            },
            .proc_value = proc_value,
            .template = template_ref,
            .wrapper = wrapper_id,
            .body_plan = body_plan_id,
            .source_fn_ty = source_fn_ty,
            .provenance = provenance,
        };
    }

    pub fn fillPromotedCallableWrapperBody(
        self: *CheckedModuleArtifact,
        reserved: ReservedPromotedCallableWrapper,
        body_plan: PromotedCallableBodyPlan,
    ) void {
        const wrapper = self.promoted_callable_wrappers.get(reserved.wrapper);
        if (!canonical.procedureValueRefEql(wrapper.promoted_proc, reserved.proc_value) or
            wrapper.body_plan != reserved.body_plan)
        {
            checkedArtifactInvariant("reserved promoted callable wrapper does not match artifact tables", .{});
        }
        self.promoted_callable_body_plans.fill(reserved.body_plan, body_plan);
    }

    pub fn publishPromotedCallableWrapper(
        self: *CheckedModuleArtifact,
        allocator: Allocator,
        reserved: ReservedPromotedCallableWrapper,
    ) Allocator.Error!void {
        const wrapper = self.promoted_callable_wrappers.get(reserved.wrapper);
        if (!canonical.procedureValueRefEql(wrapper.promoted_proc, reserved.proc_value) or
            wrapper.body_plan != reserved.body_plan)
        {
            checkedArtifactInvariant("reserved promoted callable wrapper does not match artifact tables", .{});
        }
        const published = try self.promoted_procedures.append(allocator, self.module_identity.module_idx, .{
            .proc = reserved.proc_value,
            .template = reserved.template,
            .source_binding = wrapper.source_binding,
            .source_fn_ty = reserved.source_fn_ty,
            .provenance = reserved.provenance,
        });
        if (!promotedProcedureRefEql(published, reserved.promoted_ref)) {
            checkedArtifactInvariant("published promoted procedure ref differed from reserved ref", .{});
        }
    }

    pub fn deinit(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.deinitInternal(allocator, true);
    }

    pub fn deinitRetainingModuleEnv(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.deinitInternal(allocator, false);
    }

    fn deinitInternal(self: *CheckedModuleArtifact, allocator: Allocator, comptime deinit_module_env: bool) void {
        self.comptime_values.deinit(allocator);
        self.semantic_instantiation_procedures.deinit(allocator);
        self.callable_binding_instances.deinit(allocator);
        self.const_instances.deinit(allocator);
        self.const_templates.deinit(allocator);
        self.promoted_procedures.deinit(allocator);
        self.comptime_dependencies.deinit(allocator);
        self.comptime_plans.deinit(allocator);
        self.top_level_values.deinit(allocator);
        self.compile_time_roots.deinit(allocator);
        self.interface_capabilities.deinit(allocator);
        self.platform_required_bindings.deinit(allocator);
        self.platform_requirement_relations.deinit(allocator);
        self.platform_required_declarations.deinit(allocator);
        self.hosted_procs.deinit(allocator);
        self.root_requests.deinit(allocator);
        self.callable_eval_templates.deinit(allocator);
        self.top_level_procedure_bindings.deinit(allocator);
        self.erased_fn_abis.deinit(allocator);
        self.callable_set_descriptors.deinit(allocator);
        self.executable_value_transforms.deinit(allocator);
        self.executable_type_payloads.deinit(allocator);
        self.promoted_callable_body_plans.deinit(allocator);
        self.promoted_callable_wrappers.deinit(allocator);
        self.intrinsic_wrappers.deinit(allocator);
        self.entry_wrappers.deinit(allocator);
        self.checked_procedure_templates.deinit(allocator);
        self.nested_proc_sites.deinit(allocator);
        self.resolved_value_refs.deinit(allocator);
        self.static_dispatch_plans.deinit(allocator);
        self.method_registry.deinit(allocator);
        self.provides_requires.deinit(allocator);
        self.exported_const_templates.deinit(allocator);
        self.exported_procedure_bindings.deinit(allocator);
        self.exported_procedure_templates.deinit(allocator);
        self.checked_const_bodies.deinit(allocator);
        self.checked_bodies.deinit(allocator);
        self.checked_types.deinit(allocator);
        self.exports.deinit(allocator);
        allocator.free(self.direct_import_artifact_keys);
        self.checking_context_identity.deinit(allocator);
        self.canonical_names.deinit();
        if (deinit_module_env) {
            self.module_env.deinit();
        } else {
            self.module_env = undefined;
        }
    }

    pub fn verifyReadyForCompileTimeLowering(self: *const CheckedModuleArtifact) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.module_identity.module_idx != std.math.maxInt(u32));
        std.debug.assert(self.checked_types.roots.len == self.checked_types.payloads.len);

        for (self.checked_types.payloads, 0..) |payload, i| {
            switch (payload) {
                .pending => std.debug.panic("checked artifact invariant violated: checked type payload {d} was not filled before compile-time lowering", .{i}),
                else => {},
            }
        }

        for (self.checked_bodies.exprs, 0..) |expr, i| {
            std.debug.assert(@intFromEnum(expr.id) == i);
            std.debug.assert(@intFromEnum(expr.ty) < self.checked_types.roots.len);
            verifyCheckedExprDataPublished(expr.data);
        }

        for (self.checked_const_bodies.bodies, 0..) |body, i| {
            std.debug.assert(@intFromEnum(body.id) == i);
            std.debug.assert(@intFromEnum(body.root) < self.compile_time_roots.roots.len);
            const root = self.compile_time_roots.root(body.root);
            std.debug.assert(root.kind == .constant);
            std.debug.assert(root.expr == body.body_expr);
            std.debug.assert(root.checked_type == body.checked_type);
            std.debug.assert(@intFromEnum(body.body_expr) < self.checked_bodies.exprs.len);
            std.debug.assert(@intFromEnum(body.checked_type) < self.checked_types.roots.len);
        }

        for (self.root_requests.requests, 0..) |request, i| {
            std.debug.assert(request.order == i);
            std.debug.assert(request.module_idx == self.module_identity.module_idx);
            std.debug.assert(@intFromEnum(request.checked_type) < self.checked_types.roots.len);
            if (request.abi == .compile_time) {
                const template_ref = request.procedure_template orelse {
                    std.debug.panic("checked artifact invariant violated: compile-time root has no private wrapper template", .{});
                };
                std.debug.assert(@intFromEnum(template_ref.template) < self.checked_procedure_templates.templates.len);
                const template = self.checked_procedure_templates.get(template_ref.template);
                switch (template.target) {
                    .comptime_only => {},
                    else => std.debug.panic("checked artifact invariant violated: compile-time root wrapper was not marked comptime_only", .{}),
                }
            }
        }

        for (self.compile_time_roots.roots, 0..) |root, i| {
            std.debug.assert(@intFromEnum(root.id) == i);
            std.debug.assert(root.module_idx == self.module_identity.module_idx);
            std.debug.assert(@intFromEnum(root.expr) < self.checked_bodies.exprs.len);
            if (root.pattern) |pattern| std.debug.assert(@intFromEnum(pattern) < self.checked_bodies.patterns.len);
            switch (root.kind) {
                .constant, .callable_binding => switch (root.payload) {
                    .pending => {},
                    else => verifyCompileTimeRootPayloadMatchesKind(root.kind, root.payload),
                },
                .expect => switch (root.payload) {
                    .expect => {},
                    else => std.debug.panic("checked artifact invariant violated: expect root has non-expect payload before compile-time lowering", .{}),
                },
            }
        }
    }

    pub fn verifyPublished(self: *const CheckedModuleArtifact) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.module_identity.module_idx != std.math.maxInt(u32));

        for (self.root_requests.requests, 0..) |request, i| {
            std.debug.assert(request.order == i);
            std.debug.assert(request.module_idx == self.module_identity.module_idx);
            std.debug.assert(@intFromEnum(request.checked_type) < self.checked_types.roots.len);
            if (request.kind == .test_expect or
                request.kind == .compile_time_constant or
                request.kind == .compile_time_callable)
            {
                const template_ref = request.procedure_template orelse {
                    std.debug.panic("checked artifact invariant violated: compile-time/test root has no entry wrapper template", .{});
                };
                std.debug.assert(@intFromEnum(template_ref.template) < self.checked_procedure_templates.templates.len);
            }
        }

        for (self.compile_time_roots.roots, 0..) |root, i| {
            std.debug.assert(@intFromEnum(root.id) == i);
            std.debug.assert(root.module_idx == self.module_identity.module_idx);
            std.debug.assert(@intFromEnum(root.expr) < self.checked_bodies.exprs.len);
            if (root.pattern) |pattern| std.debug.assert(@intFromEnum(pattern) < self.checked_bodies.patterns.len);
            if (root.kind == .expect) {
                switch (root.payload) {
                    .expect => {},
                    else => std.debug.panic("checked artifact invariant violated: expect root has non-expect payload", .{}),
                }
                continue;
            }
            const has_request = compileTimeRootHasRootRequest(self.root_requests.requests, root);
            if (has_request) {
                _ = self.comptime_dependencies.summaryIdForRootRequest(root.dependency_summary_request);
            }
            switch (root.payload) {
                .pending => {
                    if (has_request) {
                        std.debug.panic("checked artifact invariant violated: requested compile-time root has pending payload", .{});
                    }
                },
                else => {
                    if (!has_request) {
                        std.debug.panic("checked artifact invariant violated: non-requested compile-time root has concrete payload", .{});
                    }
                    verifyCompileTimeRootPayloadMatchesKind(root.kind, root.payload);
                },
            }
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

        for (self.checked_types.roots, 0..) |root, i| {
            std.debug.assert(@intFromEnum(root.id) == i);
            std.debug.assert(self.checked_types.payloads.len == self.checked_types.roots.len);
            switch (self.checked_types.payloads[i]) {
                .pending => std.debug.panic("checked artifact invariant violated: checked type payload {d} was not filled", .{i}),
                else => {},
            }
        }

        for (self.checked_bodies.exprs, 0..) |expr, i| {
            std.debug.assert(@intFromEnum(expr.id) == i);
            std.debug.assert(@intFromEnum(expr.ty) < self.checked_types.roots.len);
            verifyCheckedExprDataPublished(expr.data);
        }

        for (self.checked_bodies.patterns, 0..) |pattern, i| {
            std.debug.assert(@intFromEnum(pattern.id) == i);
            std.debug.assert(@intFromEnum(pattern.ty) < self.checked_types.roots.len);
            verifyCheckedPatternDataPublished(pattern.data);
        }

        for (self.checked_bodies.pattern_binders, 0..) |binder, i| {
            std.debug.assert(@intFromEnum(binder.id) == i);
            std.debug.assert(@intFromEnum(binder.pattern) < self.checked_bodies.patterns.len);
            const indexed = self.checked_bodies.pattern_binder_by_pattern[@intFromEnum(binder.pattern)] orelse {
                std.debug.panic("checked artifact invariant violated: pattern binder was not indexed by pattern", .{});
            };
            std.debug.assert(indexed == binder.id);
        }

        for (self.checked_bodies.statements, 0..) |statement, i| {
            std.debug.assert(@intFromEnum(statement.id) == i);
            verifyCheckedStatementDataPublished(statement.data);
        }

        for (self.checked_const_bodies.bodies, 0..) |body, i| {
            std.debug.assert(@intFromEnum(body.id) == i);
            std.debug.assert(@intFromEnum(body.root) < self.compile_time_roots.roots.len);
            const root = self.compile_time_roots.root(body.root);
            std.debug.assert(root.kind == .constant);
            std.debug.assert(root.expr == body.body_expr);
            std.debug.assert(root.checked_type == body.checked_type);
            std.debug.assert(@intFromEnum(body.body_expr) < self.checked_bodies.exprs.len);
            std.debug.assert(@intFromEnum(body.checked_type) < self.checked_types.roots.len);
        }

        for (self.checked_procedure_templates.templates, 0..) |template, i| {
            std.debug.assert(@intFromEnum(template.template_id) == i);
            std.debug.assert(@intFromEnum(template.checked_fn_root) < self.checked_types.roots.len);
            _ = self.checked_types.schemeForKey(template.checked_fn_scheme) orelse {
                std.debug.panic("checked artifact invariant violated: checked procedure template references missing type scheme", .{});
            };
            switch (template.body) {
                .checked_body => |body| {
                    const checked_body = self.checked_bodies.body(body);
                    std.debug.assert(checked_body.owner_template.template == template.template_id);
                    std.debug.assert(checked_body.owner_template.proc_base == template.proc_base);
                    std.debug.assert(@intFromEnum(checked_body.root_expr) < self.checked_bodies.exprs.len);
                },
                .promoted_callable_wrapper => |wrapper_id| {
                    const wrapper = self.promoted_callable_wrappers.get(wrapper_id);
                    std.debug.assert(wrapper.proc_base_key == template.proc_base);
                    std.debug.assert(wrapper.checked_fn_root == template.checked_fn_root);
                    std.debug.assert(wrapper.promoted_proc.proc_base == template.proc_base);
                    std.debug.assert(std.mem.eql(u8, &wrapper.promoted_proc.artifact.bytes, &self.key.bytes));
                    std.debug.assert(@intFromEnum(wrapper.body_plan) < self.promoted_callable_body_plans.plans.len);
                },
                .intrinsic_wrapper => |wrapper_id| {
                    const wrapper = self.intrinsic_wrappers.get(wrapper_id);
                    std.debug.assert(wrapper.checked_fn_root == template.checked_fn_root);
                    std.debug.assert(wrapper.template.template == template.template_id);
                    std.debug.assert(wrapper.template.proc_base == template.proc_base);
                    std.debug.assert(std.mem.eql(u8, &wrapper.template.artifact.bytes, &self.key.bytes));
                },
                .entry_wrapper => |wrapper_id| {
                    const wrapper = self.entry_wrappers.get(wrapper_id);
                    std.debug.assert(@intFromEnum(wrapper.body_expr) < self.checked_bodies.exprs.len);
                    std.debug.assert(@intFromEnum(wrapper.checked_fn_root) < self.checked_types.roots.len);
                    std.debug.assert(wrapper.checked_fn_root == template.checked_fn_root);
                    std.debug.assert(wrapper.template.template == template.template_id);
                    std.debug.assert(wrapper.template.proc_base == template.proc_base);
                    std.debug.assert(std.mem.eql(u8, &wrapper.template.artifact.bytes, &self.key.bytes));
                },
            }

            const nested_end = template.nested_proc_sites.start + template.nested_proc_sites.len;
            std.debug.assert(nested_end <= self.nested_proc_sites.template_refs.len);
            for (self.nested_proc_sites.template_refs[template.nested_proc_sites.start..nested_end]) |site_id| {
                std.debug.assert(@intFromEnum(site_id) < self.nested_proc_sites.sites.len);
                const site = self.nested_proc_sites.sites[@intFromEnum(site_id)];
                std.debug.assert(site.owner_template.template == template.template_id);
                std.debug.assert(site.owner_template.proc_base == template.proc_base);
                std.debug.assert(std.mem.eql(u8, &site.owner_template.artifact.bytes, &self.key.bytes));
            }
        }

        for (self.nested_proc_sites.sites, 0..) |site, i| {
            std.debug.assert(@intFromEnum(site.site) == i);
            std.debug.assert(site.site_path.len > 0);
            std.debug.assert(@intFromEnum(site.owner_template.template) < self.checked_procedure_templates.templates.len);
            if (site.checked_expr) |expr| std.debug.assert(@intFromEnum(expr) < self.checked_bodies.exprs.len);
            if (site.checked_pattern) |pattern| std.debug.assert(@intFromEnum(pattern) < self.checked_bodies.patterns.len);
        }

        for (self.exported_procedure_templates.templates) |exported| {
            std.debug.assert(std.mem.eql(u8, &exported.template.artifact.bytes, &self.key.bytes));
            std.debug.assert(@intFromEnum(exported.template.template) < self.checked_procedure_templates.templates.len);
            std.debug.assert(exported.template_closure.checked_procedure_templates.len > 0);
            std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
            std.debug.assert(exported.template_closure.checked_type_schemes.len > 0);
            std.debug.assert(exported.template_closure.interface_capabilities.len > 0);
            for (exported.template_closure.checked_bodies) |body_ref| {
                if (closureArtifactRefIsLocal(self, body_ref.artifact)) {
                    std.debug.assert(@intFromEnum(body_ref.body) < self.checked_bodies.bodies.len);
                }
            }
            for (exported.template_closure.checked_type_roots) |type_ref| {
                if (closureArtifactRefIsLocal(self, type_ref.artifact)) {
                    std.debug.assert(@intFromEnum(type_ref.ty) < self.checked_types.roots.len);
                }
            }
            for (exported.template_closure.checked_type_schemes) |scheme_ref| {
                if (closureArtifactRefIsLocal(self, scheme_ref.artifact)) {
                    std.debug.assert(@intFromEnum(scheme_ref.scheme) < self.checked_types.schemes.len);
                }
            }
            for (exported.template_closure.interface_capabilities) |capability_ref| {
                _ = capability_ref;
            }
        }

        for (self.exported_procedure_bindings.bindings) |exported| {
            std.debug.assert(std.mem.eql(u8, &exported.binding.artifact.bytes, &self.key.bytes));
            switch (exported.body) {
                .direct_template => {
                    std.debug.assert(exported.template_closure.checked_procedure_templates.len > 0);
                    std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
                    std.debug.assert(exported.template_closure.interface_capabilities.len > 0);
                },
                .callable_eval_template => |template_id| {
                    std.debug.assert(@intFromEnum(template_id) < self.callable_eval_templates.templates.len);
                    std.debug.assert(exported.template_closure.callable_eval_templates.len > 0);
                    std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
                    std.debug.assert(exported.template_closure.interface_capabilities.len > 0);
                },
            }
            for (exported.template_closure.interface_capabilities) |capability_ref| {
                _ = capability_ref;
            }
        }

        for (self.exported_const_templates.templates) |exported| {
            std.debug.assert(std.mem.eql(u8, &exported.const_ref.artifact.bytes, &self.key.bytes));
            std.debug.assert(@intFromEnum(exported.const_ref.template) < self.const_templates.templates.items.len);
            std.debug.assert(exported.template_closure.const_templates.len > 0);
            std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
            std.debug.assert(exported.template_closure.checked_type_schemes.len > 0);
            std.debug.assert(exported.template_closure.interface_capabilities.len > 0);
            switch (exported.template.state) {
                .eval_template => |eval| {
                    std.debug.assert(@intFromEnum(eval.body) < self.checked_const_bodies.bodies.len);
                    std.debug.assert(std.mem.eql(u8, &eval.entry_template.artifact.bytes, &self.key.bytes));
                    std.debug.assert(@intFromEnum(eval.entry_template.template) < self.checked_procedure_templates.templates.len);
                    std.debug.assert(exported.template_closure.checked_const_bodies.len > 0);
                    std.debug.assert(exported.template_closure.checked_procedure_templates.len > 0);
                },
                .value_graph_template => |graph| {
                    std.debug.assert(@intFromEnum(graph.schema) < self.comptime_values.schemas.items.len);
                    std.debug.assert(@intFromEnum(graph.value) < self.comptime_values.values.items.len);
                },
                .reserved => std.debug.panic(
                    "checked artifact invariant violated: exported const template was not sealed",
                    .{},
                ),
            }
            for (exported.template_closure.const_templates) |const_ref| {
                if (closureArtifactRefIsLocal(self, const_ref.artifact)) {
                    std.debug.assert(@intFromEnum(const_ref.template) < self.const_templates.templates.items.len);
                }
            }
            for (exported.template_closure.checked_type_roots) |type_ref| {
                if (closureArtifactRefIsLocal(self, type_ref.artifact)) {
                    std.debug.assert(@intFromEnum(type_ref.ty) < self.checked_types.roots.len);
                }
            }
            for (exported.template_closure.checked_type_schemes) |scheme_ref| {
                if (closureArtifactRefIsLocal(self, scheme_ref.artifact)) {
                    std.debug.assert(@intFromEnum(scheme_ref.scheme) < self.checked_types.schemes.len);
                }
            }
            for (exported.template_closure.checked_const_bodies) |body_ref| {
                if (closureArtifactRefIsLocal(self, body_ref.artifact)) {
                    std.debug.assert(@intFromEnum(body_ref.body) < self.checked_const_bodies.bodies.len);
                }
            }
            for (exported.template_closure.interface_capabilities) |capability_ref| {
                _ = capability_ref;
            }
        }

        for (self.platform_required_declarations.declarations, 0..) |declaration, i| {
            std.debug.assert(@intFromEnum(declaration.id) == i);
            std.debug.assert(declaration.requires_idx == i);
            std.debug.assert(declaration.module_idx == self.module_identity.module_idx);
        }

        for (self.platform_requirement_relations.relations, 0..) |relation, i| {
            std.debug.assert(@intFromEnum(relation.id) == i);
            std.debug.assert(relation.module_idx == self.module_identity.module_idx);
            const declaration = self.platform_required_declarations.lookupByDeclarationId(relation.declaration) orelse {
                std.debug.panic(
                    "checked artifact invariant violated: platform requirement relation {d} has no declaration",
                    .{i},
                );
            };
            std.debug.assert(declaration.requires_idx == relation.requires_idx);
            const payload_index = @intFromEnum(relation.requested_source_ty_payload);
            if (payload_index >= self.checked_types.roots.len) {
                std.debug.panic(
                    "checked artifact invariant violated: platform requirement relation {d} requested payload is out of range",
                    .{i},
                );
            }
            if (!canonicalTypeKeyEql(self.checked_types.roots[payload_index].key, relation.requested_source_ty)) {
                std.debug.panic(
                    "checked artifact invariant violated: platform requirement relation {d} requested payload key disagrees with relation key",
                    .{i},
                );
            }
            switch (relation.value_kind) {
                .const_value, .procedure_value => {},
            }
        }

        for (self.platform_required_bindings.bindings, 0..) |binding, i| {
            std.debug.assert(@intFromEnum(binding.id) == i);
            std.debug.assert(binding.module_idx == self.module_identity.module_idx);
            _ = self.platform_required_declarations.lookupByRequiredIndex(binding.requires_idx) orelse {
                std.debug.panic(
                    "checked artifact invariant violated: platform required binding {d} has no declaration",
                    .{i},
                );
            };
            const relation = self.platform_requirement_relations.lookupByRelationId(binding.checked_relation) orelse {
                std.debug.panic(
                    "checked artifact invariant violated: platform required binding {d} has no checked relation",
                    .{i},
                );
            };
            validatePlatformBindingRelation(.{
                .declaration = binding.declaration,
                .requires_idx = binding.requires_idx,
                .app_value = binding.app_value,
                .requested_source_ty = binding.requested_source_ty,
                .checked_relation = binding.checked_relation,
                .value_use = binding.value_use,
            }, relation, i);
            verifyPlatformRequiredValueUse(binding);
        }

        for (self.callable_eval_templates.templates, 0..) |template, i| {
            std.debug.assert(@intFromEnum(template.id) == i);
            std.debug.assert(template.module_idx == self.module_identity.module_idx);
            std.debug.assert(@intFromEnum(template.pattern) < self.checked_bodies.patterns.len);
            std.debug.assert(@intFromEnum(template.root) < self.compile_time_roots.roots.len);
            const root = self.compile_time_roots.root(template.root);
            std.debug.assert(root.kind == .callable_binding);
            std.debug.assert(root.pattern != null and root.pattern.? == template.pattern);
            std.debug.assert(@intFromEnum(template.checked_fn_root) < self.checked_types.roots.len);
            _ = self.checked_types.schemeForKey(template.source_scheme) orelse {
                std.debug.panic("checked artifact invariant violated: callable eval template references missing type scheme", .{});
            };
        }

        for (self.top_level_values.entries) |entry| {
            std.debug.assert(@intFromEnum(entry.pattern) < self.checked_bodies.patterns.len);
            _ = self.canonical_names.exportNameText(entry.source_name);
            switch (entry.value) {
                .const_ref => |const_ref| {
                    const owner = constRefTopLevelOwner(const_ref) orelse {
                        std.debug.panic("checked artifact invariant violated: top-level value table referenced a non-top-level ConstRef", .{});
                    };
                    std.debug.assert(owner.module_idx == self.module_identity.module_idx);
                    std.debug.assert(owner.pattern == entry.pattern);
                    std.debug.assert(std.mem.eql(u8, &const_ref.source_scheme.bytes, &entry.source_scheme.bytes));
                },
                .procedure_binding => |binding_ref| {
                    const binding = self.top_level_procedure_bindings.get(binding_ref);
                    switch (binding.body) {
                        .direct_template => |direct| {
                            _ = self.canonical_names.procBase(direct.proc_value.proc_base);
                            switch (direct.template) {
                                .checked => |template| {
                                    std.debug.assert(template.proc_base == direct.proc_value.proc_base);
                                    std.debug.assert(std.mem.eql(u8, &template.artifact.bytes, &direct.proc_value.artifact.bytes));
                                },
                                .synthetic => |synthetic| {
                                    std.debug.assert(synthetic.template.proc_base == direct.proc_value.proc_base);
                                    std.debug.assert(std.mem.eql(u8, &synthetic.template.artifact.bytes, &direct.proc_value.artifact.bytes));
                                    std.debug.assert(@intFromEnum(synthetic.template.template) < self.checked_procedure_templates.templates.len);
                                },
                                .lifted => std.debug.panic(
                                    "checked artifact invariant violated: direct top-level binding cannot use lifted template before mono",
                                    .{},
                                ),
                            }
                        },
                        .callable_eval_template => |template| {
                            std.debug.assert(@intFromEnum(template) < self.callable_eval_templates.templates.len);
                        },
                    }
                },
            }
        }

        for (self.provides_requires.requires) |entry| {
            _ = self.canonical_names.exportNameText(entry.platform_name);
            _ = self.checked_types.schemeForKey(entry.declared_source_ty) orelse {
                std.debug.panic("checked artifact invariant violated: require metadata source type was not published", .{});
            };
        }

        self.const_templates.verifySealed();
        self.executable_type_payloads.verifyPublished(self.key, &self.erased_fn_abis);
        self.executable_value_transforms.verifyPublished(&self.executable_type_payloads, self.key);
        self.callable_set_descriptors.verifyPublished();
        self.erased_fn_abis.verifyPublished();
        self.interface_capabilities.verifyPublished();
        self.promoted_callable_body_plans.verifyPublished(
            &self.comptime_plans,
            &self.checked_types,
            &self.executable_type_payloads,
            &self.executable_value_transforms,
            &self.erased_fn_abis,
            self.key,
        );
        self.promoted_callable_wrappers.verifyPublished(
            self.key,
            &self.checked_types,
            &self.checked_bodies,
            &self.promoted_callable_body_plans,
        );
        self.promoted_procedures.verifyPublished(
            self.key,
            &self.checked_procedure_templates,
            &self.checked_bodies,
            &self.promoted_callable_wrappers,
        );
        self.comptime_plans.verifySealed(&self.checked_types, &self.callable_set_descriptors);
        self.comptime_dependencies.verifySealed();
        self.comptime_values.verifySealed();
        self.const_instances.verifySealed(
            &self.const_templates,
            &self.comptime_dependencies,
            &self.semantic_instantiation_procedures,
        );
        self.callable_binding_instances.verifySealed(
            &self.comptime_dependencies,
            &self.comptime_plans,
            &self.compile_time_roots,
            &self.promoted_procedures,
            &self.semantic_instantiation_procedures,
        );
        self.semantic_instantiation_procedures.verifySealed(&self.promoted_procedures);

        for (self.resolved_value_refs.records) |record| {
            std.debug.assert(@intFromEnum(record.expr) < self.checked_bodies.exprs.len);
            if (self.platform_required_bindings.bindings.len > 0) {
                switch (record.ref) {
                    .platform_required_declaration => std.debug.panic(
                        "checked artifact invariant violated: executable platform artifact kept a declaration-only required lookup",
                        .{},
                    ),
                    else => {},
                }
            }
        }

        verifyLoweringVisibleNamesInterned(self.moduleEnvConst(), &self.canonical_names);
    }
};

fn verifyPlatformRequiredValueUse(binding: PlatformRequiredBinding) void {
    if (builtin.mode != .Debug) return;

    switch (binding.value_use) {
        .const_value => |const_use| {
            std.debug.assert(std.mem.eql(
                u8,
                &const_use.const_use.const_ref.artifact.bytes,
                &binding.app_value.artifact.bytes,
            ));
            const owner = constRefTopLevelOwner(const_use.const_use.const_ref) orelse {
                std.debug.panic("checked artifact invariant violated: platform-required const use referenced a non-top-level ConstRef", .{});
            };
            std.debug.assert(owner.pattern == binding.app_value.pattern);
        },
        .procedure_value => |proc_use| switch (proc_use.procedure.binding) {
            .platform_required => |required| {
                std.debug.assert(std.mem.eql(
                    u8,
                    &required.artifact.bytes,
                    &binding.app_value.artifact.bytes,
                ));
                std.debug.assert(required.app_value.pattern == binding.app_value.pattern);
                if (proc_use.relation_template_closure.interface_capabilities.len == 0) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform-required procedure use has no relation template closure",
                        .{},
                    );
                }
            },
            .top_level,
            .imported,
            .hosted,
            .promoted,
            => std.debug.panic(
                "checked artifact invariant violated: platform-required procedure use must reference the app requirement binding explicitly",
                .{},
            ),
        },
    }
}

/// Public `ImportedModuleView` declaration.
pub const ImportedModuleView = struct {
    key: CheckedModuleArtifactKey,
    module_env: *const ModuleEnv,
    canonical_names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    direct_import_artifact_keys: []const CheckedModuleArtifactKey = &.{},
    exports: ExportTableView,
    checked_types: CheckedTypeStoreView,
    checked_bodies: CheckedBodyStoreView,
    checked_const_bodies: *const CheckedConstBodyTable,
    checked_procedure_templates: *const CheckedProcedureTemplateTable,
    entry_wrappers: *const EntryWrapperTable,
    intrinsic_wrappers: *const IntrinsicWrapperTable,
    resolved_value_refs: *const ResolvedValueRefTable,
    nested_proc_sites: *const NestedProcSiteTable,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    hosted_procs: *const HostedProcTable,
    promoted_callable_wrappers: *const PromotedCallableWrapperTable,
    promoted_callable_body_plans: *const PromotedCallableBodyPlanTable,
    executable_type_payloads: *const ExecutableTypePayloadStore,
    executable_value_transforms: *const ExecutableValueTransformPlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
    erased_fn_abis: *const canonical.ErasedFnAbiStore,
    exported_procedure_templates: ExportedProcedureTemplateView,
    exported_procedure_bindings: ExportedProcedureBindingView,
    exported_const_templates: ExportedConstTemplateView,
    top_level_procedure_bindings: *const TopLevelProcedureBindingTable,
    callable_eval_templates: CallableEvalTemplateTableView,
    const_templates: *const ConstTemplateTable,
    promoted_procedures: *const PromotedProcedureTable,
    method_registry: *const static_dispatch.MethodRegistry,
    interface_capabilities: *const ModuleInterfaceCapabilities,
    comptime_values: *const CompileTimeValueStore,
    comptime_plans: *const CompileTimePlanStore,
    comptime_dependencies: ComptimeDependencySummaryStoreView,
    const_instances: ConstInstantiationStoreView,
    callable_binding_instances: CallableBindingInstantiationStoreView,
    semantic_instantiation_procedures: SemanticInstantiationProcedureTableView,
};

/// Public `LoweringModuleView` declaration.
pub const LoweringModuleView = struct {
    artifact: *const CheckedModuleArtifact,
    roots: *const RootRequestTable,
    relation_artifacts: []const ImportedModuleView = &.{},
};

/// Public `importedView` function.
pub fn importedView(artifact: *const CheckedModuleArtifact) ImportedModuleView {
    return .{
        .key = artifact.key,
        .module_env = artifact.moduleEnvConst(),
        .canonical_names = &artifact.canonical_names,
        .module_identity = artifact.module_identity,
        .direct_import_artifact_keys = artifact.direct_import_artifact_keys,
        .exports = artifact.exports.view(),
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .checked_const_bodies = &artifact.checked_const_bodies,
        .checked_procedure_templates = &artifact.checked_procedure_templates,
        .entry_wrappers = &artifact.entry_wrappers,
        .intrinsic_wrappers = &artifact.intrinsic_wrappers,
        .resolved_value_refs = &artifact.resolved_value_refs,
        .nested_proc_sites = &artifact.nested_proc_sites,
        .static_dispatch_plans = &artifact.static_dispatch_plans,
        .hosted_procs = &artifact.hosted_procs,
        .promoted_callable_wrappers = &artifact.promoted_callable_wrappers,
        .promoted_callable_body_plans = &artifact.promoted_callable_body_plans,
        .executable_type_payloads = &artifact.executable_type_payloads,
        .executable_value_transforms = &artifact.executable_value_transforms,
        .callable_set_descriptors = &artifact.callable_set_descriptors,
        .erased_fn_abis = &artifact.erased_fn_abis,
        .exported_procedure_templates = artifact.exported_procedure_templates.view(),
        .exported_procedure_bindings = artifact.exported_procedure_bindings.view(),
        .exported_const_templates = artifact.exported_const_templates.view(),
        .top_level_procedure_bindings = &artifact.top_level_procedure_bindings,
        .callable_eval_templates = artifact.callable_eval_templates.view(),
        .const_templates = &artifact.const_templates,
        .promoted_procedures = &artifact.promoted_procedures,
        .method_registry = &artifact.method_registry,
        .interface_capabilities = &artifact.interface_capabilities,
        .comptime_values = &artifact.comptime_values,
        .comptime_plans = &artifact.comptime_plans,
        .comptime_dependencies = artifact.comptime_dependencies.view(),
        .const_instances = artifact.const_instances.view(),
        .callable_binding_instances = artifact.callable_binding_instances.view(),
        .semantic_instantiation_procedures = artifact.semantic_instantiation_procedures.view(),
    };
}

const ProjectedCheckedTypeKey = struct {
    artifact: [32]u8,
    ty: u32,
};

/// Public `ArtifactNamePublisher` declaration.
///
/// Checking finalization uses this boundary when artifact-owned data must record
/// canonical names that came from a MIR-family lowering run. The caller stores
/// only ids owned by `target`; no lowering-run label id may cross this boundary
/// into checked artifact data.
pub const ArtifactNamePublisher = struct {
    target: *CheckedModuleArtifact,

    pub fn init(target: *CheckedModuleArtifact) ArtifactNamePublisher {
        return .{ .target = target };
    }

    pub fn recordFieldFromLowering(
        self: *ArtifactNamePublisher,
        lowering_names: *const canonical.CanonicalNameStore,
        id: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.target.canonical_names.internRecordFieldLabel(lowering_names.recordFieldLabelText(id));
    }

    pub fn tagFromLowering(
        self: *ArtifactNamePublisher,
        lowering_names: *const canonical.CanonicalNameStore,
        id: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.target.canonical_names.internTagLabel(lowering_names.tagLabelText(id));
    }

    pub fn recordFieldMatchesLowering(
        self: *const ArtifactNamePublisher,
        artifact_label: canonical.RecordFieldLabelId,
        lowering_names: *const canonical.CanonicalNameStore,
        lowering_label: canonical.RecordFieldLabelId,
    ) bool {
        return std.mem.eql(
            u8,
            self.target.canonical_names.recordFieldLabelText(artifact_label),
            lowering_names.recordFieldLabelText(lowering_label),
        );
    }

    pub fn tagMatchesLowering(
        self: *const ArtifactNamePublisher,
        artifact_label: canonical.TagLabelId,
        lowering_names: *const canonical.CanonicalNameStore,
        lowering_label: canonical.TagLabelId,
    ) bool {
        return std.mem.eql(
            u8,
            self.target.canonical_names.tagLabelText(artifact_label),
            lowering_names.tagLabelText(lowering_label),
        );
    }
};

/// Public `CheckedTypeProjector` declaration.
///
/// Projects checked type graphs from imported artifacts into the artifact that
/// owns the current checked-finalization result. This is semantic publication
/// work, not target/layout caching, and it is the only checked-artifact boundary
/// that may clone imported checked type payloads for compile-time constant and
/// capture reification plans.
pub const CheckedTypeProjector = struct {
    allocator: Allocator,
    target: *CheckedModuleArtifact,
    imports: []const ImportedModuleView,
    active: std.AutoHashMap(ProjectedCheckedTypeKey, CheckedTypeId),

    pub fn init(
        allocator: Allocator,
        target: *CheckedModuleArtifact,
        imports: []const ImportedModuleView,
    ) CheckedTypeProjector {
        return .{
            .allocator = allocator,
            .target = target,
            .imports = imports,
            .active = std.AutoHashMap(ProjectedCheckedTypeKey, CheckedTypeId).init(allocator),
        };
    }

    pub fn deinit(self: *CheckedTypeProjector) void {
        self.active.deinit();
    }

    pub fn publishedNominalBacking(
        self: *CheckedTypeProjector,
        nominal: CheckedNominalType,
    ) Allocator.Error!?CheckedTypeId {
        const nominal_key = canonical.NominalTypeKey{
            .module_name = nominal.origin_module,
            .type_name = nominal.name,
        };

        for (self.target.interface_capabilities.exported_nominal_representations) |representation| {
            if (!canonicalNominalTypeKeyEql(representation.nominal, nominal_key)) continue;
            const capability = self.target.interface_capabilities.boxPayloadCapability(representation.box_payload_capability);
            if (!self.nominalArgsMatchTarget(capability.instantiated_args, nominal.args)) continue;
            return capability.backing_ty;
        }

        if (self.target.checked_types.nominalDeclaration(nominal_key)) |declaration| {
            return try self.target.checked_types.ensureInstantiatedNominalBackingRoot(
                self.allocator,
                &self.target.canonical_names,
                declaration,
                nominal.args,
            );
        }

        for (self.imports) |imported| {
            for (imported.interface_capabilities.exported_nominal_representations) |representation| {
                if (!self.importedNominalMatches(imported, nominal_key, representation.nominal)) continue;
                const capability = imported.interface_capabilities.boxPayloadCapability(representation.box_payload_capability);
                if (!self.nominalArgsMatchTarget(capability.instantiated_args, nominal.args)) continue;
                return try self.projectImportedCheckedType(imported, capability.backing_ty);
            }
            if (try self.instantiateImportedNominalDeclaration(imported, nominal_key, nominal.args)) |backing| {
                return backing;
            }
        }

        return null;
    }

    pub fn projectImportedCheckedTypeForKey(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        key: canonical.CanonicalTypeKey,
    ) Allocator.Error!?CheckedTypeId {
        const imported_root = for (imported.checked_types.roots) |root| {
            if (std.mem.eql(u8, &root.key.bytes, &key.bytes)) break root.id;
        } else return null;

        return try self.projectImportedCheckedType(imported, imported_root);
    }

    pub fn projectCheckedTypeViewRoot(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        ty: CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        return try self.projectCheckedTypeViewRootWithNames(source, null, ty);
    }

    pub fn projectCheckedTypeViewRootWithNames(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        ty: CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        var active = std.AutoHashMap(CheckedTypeId, CheckedTypeId).init(self.allocator);
        defer active.deinit();
        return try self.projectCheckedTypeViewRootInner(source, source_names, ty, &active);
    }

    pub fn projectCheckedTypeViewForKey(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        key: canonical.CanonicalTypeKey,
    ) Allocator.Error!?CheckedTypeId {
        const source_root = for (source.roots) |root| {
            if (std.mem.eql(u8, &root.key.bytes, &key.bytes)) break root.id;
        } else return null;

        return try self.projectCheckedTypeViewRoot(source, source_root);
    }

    fn projectCheckedTypeViewRootInner(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        ty: CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error!CheckedTypeId {
        const index: usize = @intFromEnum(ty);
        if (index >= source.roots.len or index >= source.payloads.len) {
            checkedArtifactInvariant("checked type view projection referenced a missing source root", .{});
        }

        const source_root = source.roots[index];
        if (self.target.checked_types.rootForKey(source_root.key)) |existing| return existing;
        if (active.get(ty)) |reserved| return reserved;

        const reserved = try self.target.checked_types.reserveSyntheticTypeRoot(self.allocator, source_root.key);
        try active.put(ty, reserved);
        errdefer _ = active.remove(ty);

        const payload = try self.projectCheckedTypeViewPayload(source, source_names, source.payloads[index], active);
        try self.target.checked_types.fillSyntheticTypeRoot(self.allocator, reserved, payload);
        _ = active.remove(ty);
        return reserved;
    }

    fn projectCheckedTypeViewPayload(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        payload: CheckedTypePayload,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error!CheckedTypePayload {
        return switch (payload) {
            .pending => checkedArtifactInvariant("checked type view projection reached pending payload", .{}),
            .empty_record => .empty_record,
            .empty_tag_union => .empty_tag_union,
            .flex => |flex| .{ .flex = try self.projectCheckedTypeViewVariable(source, source_names, flex, active) },
            .rigid => |rigid| .{ .rigid = try self.projectCheckedTypeViewVariable(source, source_names, rigid, active) },
            .alias => |alias| .{ .alias = .{
                .name = try self.remapViewTypeName(source_names, alias.name),
                .origin_module = try self.remapViewModuleName(source_names, alias.origin_module),
                .backing = try self.projectCheckedTypeViewRootInner(source, source_names, alias.backing, active),
                .args = try self.projectCheckedTypeViewIds(source, source_names, alias.args, active),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.projectCheckedTypeViewRecordFields(source, source_names, record.fields, active),
                .ext = try self.projectCheckedTypeViewRootInner(source, source_names, record.ext, active),
            } },
            .record_unbound => |fields| .{
                .record_unbound = try self.projectCheckedTypeViewRecordFields(source, source_names, fields, active),
            },
            .tuple => |items| .{ .tuple = try self.projectCheckedTypeViewIds(source, source_names, items, active) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.remapViewTypeName(source_names, nominal.name),
                .origin_module = try self.remapViewModuleName(source_names, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.projectCheckedTypeViewRootInner(source, source_names, nominal.backing, active),
                .args = try self.projectCheckedTypeViewIds(source, source_names, nominal.args, active),
            } },
            .function => |function| .{ .function = .{
                .kind = finalizedFunctionKind(function.kind),
                .args = try self.projectCheckedTypeViewIds(source, source_names, function.args, active),
                .ret = try self.projectCheckedTypeViewRootInner(source, source_names, function.ret, active),
                .needs_instantiation = function.needs_instantiation,
            } },
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.projectCheckedTypeViewTags(source, source_names, tag_union.tags, active),
                .ext = try self.projectCheckedTypeViewRootInner(source, source_names, tag_union.ext, active),
            } },
        };
    }

    fn projectCheckedTypeViewVariable(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        variable: CheckedTypeVariable,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error!CheckedTypeVariable {
        const name = if (variable.name) |name_text|
            try self.allocator.dupe(u8, name_text)
        else
            null;
        errdefer if (name) |owned| self.allocator.free(owned);

        const constraints = try self.projectCheckedTypeViewConstraints(source, source_names, variable.constraints, active);
        errdefer self.allocator.free(constraints);

        return .{
            .name = name,
            .constraints = constraints,
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn projectCheckedTypeViewConstraints(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        constraints: []const CheckedStaticDispatchConstraint,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedStaticDispatchConstraint, constraints.len);
        errdefer self.allocator.free(out);
        for (constraints, 0..) |constraint, i| {
            out[i] = .{
                .fn_name = try self.remapViewMethodName(source_names, constraint.fn_name),
                .fn_ty = try self.projectCheckedTypeViewRootInner(source, source_names, constraint.fn_ty, active),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            };
        }
        return out;
    }

    fn projectCheckedTypeViewIds(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        ids: []const CheckedTypeId,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.projectCheckedTypeViewRootInner(source, source_names, id, active);
        }
        return out;
    }

    fn projectCheckedTypeViewRecordFields(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        fields: []const CheckedRecordField,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.remapViewRecordField(source_names, field.name),
                .ty = try self.projectCheckedTypeViewRootInner(source, source_names, field.ty, active),
            };
        }
        return out;
    }

    fn projectCheckedTypeViewTags(
        self: *CheckedTypeProjector,
        source: CheckedTypeStoreView,
        source_names: ?*const canonical.CanonicalNameStore,
        tags: []const CheckedTag,
        active: *std.AutoHashMap(CheckedTypeId, CheckedTypeId),
    ) Allocator.Error![]const CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.remapViewTag(source_names, tag.name),
                .args = try self.projectCheckedTypeViewIds(source, source_names, tag.args, active),
            };
        }
        return out;
    }

    fn remapViewModuleName(
        self: *CheckedTypeProjector,
        source_names: ?*const canonical.CanonicalNameStore,
        id: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        const names = source_names orelse return id;
        return try self.target.canonical_names.internModuleName(names.moduleNameText(id));
    }

    fn remapViewTypeName(
        self: *CheckedTypeProjector,
        source_names: ?*const canonical.CanonicalNameStore,
        id: canonical.TypeNameId,
    ) Allocator.Error!canonical.TypeNameId {
        const names = source_names orelse return id;
        return try self.target.canonical_names.internTypeName(names.typeNameText(id));
    }

    fn remapViewMethodName(
        self: *CheckedTypeProjector,
        source_names: ?*const canonical.CanonicalNameStore,
        id: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        const names = source_names orelse return id;
        return try self.target.canonical_names.internMethodName(names.methodNameText(id));
    }

    fn remapViewRecordField(
        self: *CheckedTypeProjector,
        source_names: ?*const canonical.CanonicalNameStore,
        id: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        const names = source_names orelse return id;
        return try self.target.canonical_names.internRecordFieldLabel(names.recordFieldLabelText(id));
    }

    fn remapViewTag(
        self: *CheckedTypeProjector,
        source_names: ?*const canonical.CanonicalNameStore,
        id: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        const names = source_names orelse return id;
        return try self.target.canonical_names.internTagLabel(names.tagLabelText(id));
    }

    fn instantiateImportedNominalDeclaration(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        target_nominal: canonical.NominalTypeKey,
        actual_args: []const CheckedTypeId,
    ) Allocator.Error!?CheckedTypeId {
        const declaration = self.importedNominalDeclaration(imported, target_nominal) orelse return null;
        if (declaration.formal_args.len != actual_args.len) {
            checkedArtifactInvariant("imported nominal declaration arity did not match target nominal args", .{});
        }

        const projected_declaration_root = try self.projectImportedCheckedType(imported, declaration.declaration_root);
        const projected_backing = try self.projectImportedCheckedType(imported, declaration.backing);
        const projected_formals = try self.allocator.alloc(CheckedTypeId, declaration.formal_args.len);
        defer self.allocator.free(projected_formals);
        for (declaration.formal_args, 0..) |formal, i| {
            projected_formals[i] = try self.projectImportedCheckedType(imported, formal);
        }

        return try self.target.checked_types.ensureInstantiatedNominalBackingRoot(
            self.allocator,
            &self.target.canonical_names,
            .{
                .nominal = target_nominal,
                .declaration_root = projected_declaration_root,
                .backing = projected_backing,
                .formal_args = projected_formals,
            },
            actual_args,
        );
    }

    fn importedNominalDeclaration(
        self: *const CheckedTypeProjector,
        imported: ImportedModuleView,
        target_nominal: canonical.NominalTypeKey,
    ) ?CheckedNominalDeclaration {
        for (imported.checked_types.nominal_declarations) |declaration| {
            if (self.importedNominalMatches(imported, target_nominal, declaration.nominal)) return declaration;
        }
        return null;
    }

    fn nominalArgsMatchTarget(
        self: *const CheckedTypeProjector,
        expected: []const canonical.CanonicalTypeKey,
        args: []const CheckedTypeId,
    ) bool {
        if (expected.len != args.len) return false;
        for (expected, args) |expected_key, arg| {
            const index: usize = @intFromEnum(arg);
            if (index >= self.target.checked_types.roots.len) {
                checkedArtifactInvariant("nominal argument referenced a missing checked type root", .{});
            }
            if (!canonicalTypeKeyEql(expected_key, self.target.checked_types.roots[index].key)) return false;
        }
        return true;
    }

    fn importedNominalMatches(
        self: *const CheckedTypeProjector,
        imported: ImportedModuleView,
        target_nominal: canonical.NominalTypeKey,
        imported_nominal: canonical.NominalTypeKey,
    ) bool {
        return std.mem.eql(
            u8,
            self.target.canonical_names.moduleNameText(target_nominal.module_name),
            imported.canonical_names.moduleNameText(imported_nominal.module_name),
        ) and std.mem.eql(
            u8,
            self.target.canonical_names.typeNameText(target_nominal.type_name),
            imported.canonical_names.typeNameText(imported_nominal.type_name),
        );
    }

    fn projectImportedCheckedType(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        ty: CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        const index: usize = @intFromEnum(ty);
        if (index >= imported.checked_types.roots.len or index >= imported.checked_types.payloads.len) {
            checkedArtifactInvariant("imported checked type projection referenced a missing imported type root", .{});
        }

        const key = ProjectedCheckedTypeKey{
            .artifact = imported.key.bytes,
            .ty = @intCast(index),
        };
        if (self.active.get(key)) |active| return active;

        const imported_root = imported.checked_types.roots[index];
        if (self.target.checked_types.rootForKey(imported_root.key)) |existing| return existing;

        const reserved = try self.target.checked_types.reserveSyntheticTypeRoot(self.allocator, imported_root.key);
        try self.active.put(key, reserved);
        errdefer _ = self.active.remove(key);

        const payload = try self.projectImportedCheckedTypePayload(imported, imported.checked_types.payloads[index]);
        try self.target.checked_types.fillSyntheticTypeRoot(self.allocator, reserved, payload);
        _ = self.active.remove(key);
        return reserved;
    }

    fn projectImportedCheckedTypePayload(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        payload: CheckedTypePayload,
    ) Allocator.Error!CheckedTypePayload {
        return switch (payload) {
            .pending => checkedArtifactInvariant("imported checked type projection reached pending payload", .{}),
            .empty_record => .empty_record,
            .empty_tag_union => .empty_tag_union,
            .flex => |flex| .{ .flex = try self.projectImportedTypeVariable(imported, flex) },
            .rigid => |rigid| .{ .rigid = try self.projectImportedTypeVariable(imported, rigid) },
            .alias => |alias| try self.projectImportedAlias(imported, alias),
            .record => |record| .{ .record = try self.projectImportedRecord(imported, record) },
            .record_unbound => |fields| .{ .record_unbound = try self.projectImportedRecordFields(imported, fields) },
            .tuple => |items| .{ .tuple = try self.projectImportedTypeIds(imported, items) },
            .nominal => |nominal| try self.projectImportedNominal(imported, nominal),
            .function => |function| .{ .function = try self.projectImportedFunction(imported, function) },
            .tag_union => |tag_union| .{ .tag_union = try self.projectImportedTagUnion(imported, tag_union) },
        };
    }

    fn projectImportedRecord(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        record: CheckedRecordType,
    ) Allocator.Error!CheckedRecordType {
        const fields = try self.projectImportedRecordFields(imported, record.fields);
        errdefer self.allocator.free(fields);
        return .{
            .fields = fields,
            .ext = try self.projectImportedCheckedType(imported, record.ext),
        };
    }

    fn projectImportedFunction(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        function: CheckedFunctionType,
    ) Allocator.Error!CheckedFunctionType {
        const args = try self.projectImportedTypeIds(imported, function.args);
        errdefer self.allocator.free(args);
        return .{
            .kind = finalizedFunctionKind(function.kind),
            .args = args,
            .ret = try self.projectImportedCheckedType(imported, function.ret),
            .needs_instantiation = function.needs_instantiation,
        };
    }

    fn projectImportedTagUnion(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        tag_union: CheckedTagUnionType,
    ) Allocator.Error!CheckedTagUnionType {
        const tags = try self.projectImportedTags(imported, tag_union.tags);
        errdefer {
            for (tags) |tag| self.allocator.free(tag.args);
            self.allocator.free(tags);
        }
        return .{
            .tags = tags,
            .ext = try self.projectImportedCheckedType(imported, tag_union.ext),
        };
    }

    fn projectImportedTypeVariable(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        variable: CheckedTypeVariable,
    ) Allocator.Error!CheckedTypeVariable {
        const name = if (variable.name) |name_text|
            try self.allocator.dupe(u8, name_text)
        else
            null;
        errdefer if (name) |owned| self.allocator.free(owned);

        const constraints = try self.projectImportedConstraints(imported, variable.constraints);
        errdefer self.allocator.free(constraints);

        return .{
            .name = name,
            .constraints = constraints,
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn projectImportedConstraints(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        constraints: []const CheckedStaticDispatchConstraint,
    ) Allocator.Error![]const CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedStaticDispatchConstraint, constraints.len);
        errdefer self.allocator.free(out);
        for (constraints, 0..) |constraint, i| {
            out[i] = .{
                .fn_name = try self.remapMethodName(imported, constraint.fn_name),
                .fn_ty = try self.projectImportedCheckedType(imported, constraint.fn_ty),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            };
        }
        return out;
    }

    fn projectImportedAlias(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        alias: CheckedAliasType,
    ) Allocator.Error!CheckedTypePayload {
        const args = try self.projectImportedTypeIds(imported, alias.args);
        errdefer self.allocator.free(args);
        return .{ .alias = .{
            .name = try self.remapTypeName(imported, alias.name),
            .origin_module = try self.remapModuleName(imported, alias.origin_module),
            .backing = try self.projectImportedCheckedType(imported, alias.backing),
            .args = args,
        } };
    }

    fn projectImportedNominal(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        nominal: CheckedNominalType,
    ) Allocator.Error!CheckedTypePayload {
        const args = try self.projectImportedTypeIds(imported, nominal.args);
        errdefer self.allocator.free(args);
        return .{ .nominal = .{
            .name = try self.remapTypeName(imported, nominal.name),
            .origin_module = try self.remapModuleName(imported, nominal.origin_module),
            .builtin = nominal.builtin,
            .is_opaque = nominal.is_opaque,
            .backing = try self.projectImportedCheckedType(imported, nominal.backing),
            .args = args,
        } };
    }

    fn projectImportedTypeIds(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        ids: []const CheckedTypeId,
    ) Allocator.Error![]const CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.projectImportedCheckedType(imported, id);
        }
        return out;
    }

    fn projectImportedRecordFields(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        fields: []const CheckedRecordField,
    ) Allocator.Error![]const CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.remapRecordField(imported, field.name),
                .ty = try self.projectImportedCheckedType(imported, field.ty),
            };
        }
        return out;
    }

    fn projectImportedTags(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        tags: []const CheckedTag,
    ) Allocator.Error![]const CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.remapTag(imported, tag.name),
                .args = try self.projectImportedTypeIds(imported, tag.args),
            };
        }
        return out;
    }

    fn remapModuleName(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        id: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        return try self.target.canonical_names.internModuleName(imported.canonical_names.moduleNameText(id));
    }

    fn remapTypeName(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        id: canonical.TypeNameId,
    ) Allocator.Error!canonical.TypeNameId {
        return try self.target.canonical_names.internTypeName(imported.canonical_names.typeNameText(id));
    }

    fn remapMethodName(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        id: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        return try self.target.canonical_names.internMethodName(imported.canonical_names.methodNameText(id));
    }

    fn remapRecordField(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        id: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.target.canonical_names.internRecordFieldLabel(imported.canonical_names.recordFieldLabelText(id));
    }

    fn remapTag(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        id: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.target.canonical_names.internTagLabel(imported.canonical_names.tagLabelText(id));
    }
};

const CheckedTypeStoreImportProjector = struct {
    allocator: Allocator,
    target_store: *CheckedTypeStore,
    target_names: *canonical.CanonicalNameStore,
    imported: ImportedModuleView,
    active: std.AutoHashMap(CheckedTypeId, CheckedTypeId),

    fn init(
        allocator: Allocator,
        target_store: *CheckedTypeStore,
        target_names: *canonical.CanonicalNameStore,
        imported: ImportedModuleView,
    ) CheckedTypeStoreImportProjector {
        return .{
            .allocator = allocator,
            .target_store = target_store,
            .target_names = target_names,
            .imported = imported,
            .active = std.AutoHashMap(CheckedTypeId, CheckedTypeId).init(allocator),
        };
    }

    fn deinit(self: *CheckedTypeStoreImportProjector) void {
        self.active.deinit();
    }

    fn project(self: *CheckedTypeStoreImportProjector, ty: CheckedTypeId) Allocator.Error!CheckedTypeId {
        const index: usize = @intFromEnum(ty);
        if (index >= self.imported.checked_types.roots.len or index >= self.imported.checked_types.payloads.len) {
            checkedArtifactInvariant("platform for-clause projection referenced a missing app checked type root", .{});
        }

        if (self.active.get(ty)) |reserved| return reserved;

        const imported_root = self.imported.checked_types.roots[index];
        if (self.target_store.rootForKey(imported_root.key)) |existing| return existing;

        const reserved = try self.target_store.reserveSyntheticTypeRoot(self.allocator, imported_root.key);
        try self.active.put(ty, reserved);
        errdefer _ = self.active.remove(ty);

        const payload = try self.projectPayload(self.imported.checked_types.payloads[index]);
        try self.target_store.fillSyntheticTypeRoot(self.allocator, reserved, payload);
        _ = self.active.remove(ty);
        return reserved;
    }

    fn projectPayload(
        self: *CheckedTypeStoreImportProjector,
        payload: CheckedTypePayload,
    ) Allocator.Error!CheckedTypePayload {
        return switch (payload) {
            .pending => checkedArtifactInvariant("platform for-clause projection reached pending app checked type payload", .{}),
            .empty_record => .empty_record,
            .empty_tag_union => .empty_tag_union,
            .flex => |flex| .{ .flex = try self.projectVariable(flex) },
            .rigid => |rigid| .{ .rigid = try self.projectVariable(rigid) },
            .alias => |alias| .{ .alias = .{
                .name = try self.remapTypeName(alias.name),
                .origin_module = try self.remapModuleName(alias.origin_module),
                .backing = try self.project(alias.backing),
                .args = try self.projectIds(alias.args),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.projectRecordFields(record.fields),
                .ext = try self.project(record.ext),
            } },
            .record_unbound => |fields| .{
                .record_unbound = try self.projectRecordFields(fields),
            },
            .tuple => |items| .{ .tuple = try self.projectIds(items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.remapTypeName(nominal.name),
                .origin_module = try self.remapModuleName(nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.project(nominal.backing),
                .args = try self.projectIds(nominal.args),
            } },
            .function => |function| .{ .function = .{
                .kind = finalizedFunctionKind(function.kind),
                .args = try self.projectIds(function.args),
                .ret = try self.project(function.ret),
                .needs_instantiation = function.needs_instantiation,
            } },
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.projectTags(tag_union.tags),
                .ext = try self.project(tag_union.ext),
            } },
        };
    }

    fn projectVariable(
        self: *CheckedTypeStoreImportProjector,
        variable: CheckedTypeVariable,
    ) Allocator.Error!CheckedTypeVariable {
        const name = if (variable.name) |name_text|
            try self.allocator.dupe(u8, name_text)
        else
            null;
        errdefer if (name) |owned| self.allocator.free(owned);

        return .{
            .name = name,
            .constraints = try self.projectConstraints(variable.constraints),
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn projectConstraints(
        self: *CheckedTypeStoreImportProjector,
        constraints: []const CheckedStaticDispatchConstraint,
    ) Allocator.Error![]const CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedStaticDispatchConstraint, constraints.len);
        errdefer self.allocator.free(out);
        for (constraints, 0..) |constraint, i| {
            out[i] = .{
                .fn_name = try self.remapMethodName(constraint.fn_name),
                .fn_ty = try self.project(constraint.fn_ty),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            };
        }
        return out;
    }

    fn projectIds(
        self: *CheckedTypeStoreImportProjector,
        ids: []const CheckedTypeId,
    ) Allocator.Error![]const CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.project(id);
        }
        return out;
    }

    fn projectRecordFields(
        self: *CheckedTypeStoreImportProjector,
        fields: []const CheckedRecordField,
    ) Allocator.Error![]const CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.remapRecordField(field.name),
                .ty = try self.project(field.ty),
            };
        }
        return out;
    }

    fn projectTags(
        self: *CheckedTypeStoreImportProjector,
        tags: []const CheckedTag,
    ) Allocator.Error![]const CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.remapTag(tag.name),
                .args = try self.projectIds(tag.args),
            };
        }
        return out;
    }

    fn remapModuleName(
        self: *CheckedTypeStoreImportProjector,
        id: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        return try self.target_names.internModuleName(self.imported.canonical_names.moduleNameText(id));
    }

    fn remapTypeName(
        self: *CheckedTypeStoreImportProjector,
        id: canonical.TypeNameId,
    ) Allocator.Error!canonical.TypeNameId {
        return try self.target_names.internTypeName(self.imported.canonical_names.typeNameText(id));
    }

    fn remapMethodName(
        self: *CheckedTypeStoreImportProjector,
        id: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        return try self.target_names.internMethodName(self.imported.canonical_names.methodNameText(id));
    }

    fn remapRecordField(
        self: *CheckedTypeStoreImportProjector,
        id: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.target_names.internRecordFieldLabel(self.imported.canonical_names.recordFieldLabelText(id));
    }

    fn remapTag(
        self: *CheckedTypeStoreImportProjector,
        id: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.target_names.internTagLabel(self.imported.canonical_names.tagLabelText(id));
    }
};

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

const InternLoweringVisibleNameVisitor = struct {
    names: *canonical.CanonicalNameStore,
    idents: *const Ident.Store,

    fn recordField(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        _ = try self.names.internRecordFieldIdent(self.idents, ident);
    }

    fn tag(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        _ = try self.names.internTagIdent(self.idents, ident);
    }

    fn method(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        _ = try self.names.internMethodIdent(self.idents, ident);
    }

    fn typeName(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        _ = try self.names.internTypeIdent(self.idents, ident);
    }

    fn exportName(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        _ = try self.names.internExportIdent(self.idents, ident);
    }

    fn externalSymbol(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        _ = try self.names.internExternalSymbolIdent(self.idents, ident);
    }
};

const VerifyLoweringVisibleNameVisitor = struct {
    names: *const canonical.CanonicalNameStore,
    idents: *const Ident.Store,

    fn recordField(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        std.debug.assert(self.names.lookupRecordFieldIdent(self.idents, ident) != null);
    }

    fn tag(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        std.debug.assert(self.names.lookupTagIdent(self.idents, ident) != null);
    }

    fn method(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        std.debug.assert(self.names.lookupMethodIdent(self.idents, ident) != null);
    }

    fn typeName(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        std.debug.assert(self.names.lookupTypeIdent(self.idents, ident) != null);
    }

    fn exportName(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        std.debug.assert(self.names.lookupExportIdent(self.idents, ident) != null);
    }

    fn externalSymbol(self: *@This(), ident: Ident.Idx) Allocator.Error!void {
        std.debug.assert(self.names.lookupExternalSymbolIdent(self.idents, ident) != null);
    }
};

fn internLoweringVisibleNames(
    module_env: *const ModuleEnv,
    names: *canonical.CanonicalNameStore,
) Allocator.Error!void {
    var visitor = InternLoweringVisibleNameVisitor{
        .names = names,
        .idents = module_env.getIdentStoreConst(),
    };
    try scanLoweringVisibleNames(module_env, &visitor);
}

fn verifyLoweringVisibleNamesInterned(
    module_env: *const ModuleEnv,
    names: *const canonical.CanonicalNameStore,
) void {
    if (builtin.mode != .Debug) return;
    var visitor = VerifyLoweringVisibleNameVisitor{
        .names = names,
        .idents = module_env.getIdentStoreConst(),
    };
    scanLoweringVisibleNames(module_env, &visitor) catch unreachable;
}

fn scanLoweringVisibleNames(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    const store = &module_env.store;
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        const tag = store.nodes.get(node_idx).tag;
        switch (tag) {
            .record_field => {
                const field = store.getRecordField(@enumFromInt(raw_node_idx));
                try visitor.recordField(field.name);
            },
            .record_destruct => {
                const destruct = store.getRecordDestruct(@enumFromInt(raw_node_idx));
                switch (destruct.kind) {
                    .Rest => {},
                    .Required, .SubPattern => try visitor.recordField(destruct.label),
                }
            },
            .expr_typed_int,
            .expr_typed_frac,
            .expr_tag,
            .expr_zero_argument_tag,
            .expr_closure,
            .expr_field_access,
            .expr_method_call,
            .expr_dispatch_call,
            .expr_method_eq,
            .expr_type_method_call,
            .expr_type_dispatch_call,
            .expr_hosted_lambda,
            => {
                const expr = store.getExpr(@enumFromInt(raw_node_idx));
                switch (expr) {
                    .e_typed_int => |typed| try visitor.typeName(typed.type_name),
                    .e_typed_frac => |typed| try visitor.typeName(typed.type_name),
                    .e_tag => |tag_expr| try visitor.tag(tag_expr.name),
                    .e_zero_argument_tag => |tag_expr| {
                        try visitor.tag(tag_expr.name);
                        try visitor.tag(tag_expr.closure_name);
                    },
                    .e_closure => |closure| try visitor.tag(closure.tag_name),
                    .e_field_access => |field_access| try visitor.recordField(field_access.field_name),
                    .e_method_call => |method_call| try visitor.method(method_call.method_name),
                    .e_dispatch_call => |dispatch_call| try visitor.method(dispatch_call.method_name),
                    .e_method_eq => try visitor.method(module_env.idents.is_eq),
                    .e_type_method_call => |type_method_call| try visitor.method(type_method_call.method_name),
                    .e_type_dispatch_call => |type_dispatch_call| try visitor.method(type_dispatch_call.method_name),
                    .e_hosted_lambda => |hosted| try visitor.externalSymbol(hosted.symbol_name),
                    else => {},
                }
            },
            .pattern_applied_tag => {
                const pattern = store.getPattern(@enumFromInt(raw_node_idx));
                switch (pattern) {
                    .applied_tag => |tag_pattern| try visitor.tag(tag_pattern.name),
                    else => {},
                }
            },
            .type_header => {
                const header = store.getTypeHeader(@enumFromInt(raw_node_idx));
                try visitor.typeName(header.name);
                try visitor.typeName(header.relative_name);
            },
            .ty_apply,
            .ty_lookup,
            .ty_tag,
            => {
                const anno = store.getTypeAnno(@enumFromInt(raw_node_idx));
                switch (anno) {
                    .apply => |apply| try visitor.typeName(apply.name),
                    .lookup => |lookup| try visitor.typeName(lookup.name),
                    .tag => |tag_anno| try visitor.tag(tag_anno.name),
                    else => {},
                }
            },
            .ty_record_field => {
                const field = store.getAnnoRecordField(@enumFromInt(raw_node_idx));
                try visitor.recordField(field.name);
            },
            .where_method,
            .where_alias,
            => {
                const where_clause = store.getWhereClause(@enumFromInt(raw_node_idx));
                switch (where_clause) {
                    .w_method => |method| try visitor.method(method.method_name),
                    .w_alias => |alias| try visitor.typeName(alias.alias_name),
                    .w_malformed => {},
                }
            },
            .exposed_item => {
                const item = store.getExposedItem(@enumFromInt(raw_node_idx));
                try visitor.exportName(item.name);
                if (item.alias) |alias| try visitor.exportName(alias);
            },
            else => {},
        }
    }
}

/// Public `loweringView` function.
pub fn loweringView(artifact: *const CheckedModuleArtifact) LoweringModuleView {
    return .{
        .artifact = artifact,
        .roots = &artifact.root_requests,
        .relation_artifacts = &.{},
    };
}

/// Public `loweringViewWithRelations` function.
pub fn loweringViewWithRelations(
    artifact: *const CheckedModuleArtifact,
    relation_artifacts: []const ImportedModuleView,
) LoweringModuleView {
    return .{
        .artifact = artifact,
        .roots = &artifact.root_requests,
        .relation_artifacts = relation_artifacts,
    };
}

/// Public `publishFromTypedModule` function.
pub fn publishFromTypedModule(
    allocator: Allocator,
    modules: *const TypedCIR.Modules,
    module_idx: u32,
    inputs: PublishInputs,
) anyerror!CheckedModuleArtifact {
    const module = modules.module(module_idx);
    const module_env = module.moduleEnvConst();
    const idents = module.identStoreConst();

    var canonical_names = canonical.CanonicalNameStore.init(allocator);
    errdefer canonical_names.deinit();
    const module_name = try canonical_names.internModuleName(module_env.module_name);
    const display_module_name = try canonical_names.internModuleIdent(idents, module_env.display_module_name_idx);
    const qualified_module_name = try canonical_names.internModuleIdent(idents, module_env.qualified_module_ident);
    const module_identity = ModuleIdentity{
        .stable_hash = computeStableModuleIdentityHash(module_env),
        .module_idx = module_idx,
        .module_name = module_name,
        .display_module_name = display_module_name,
        .qualified_module_name = qualified_module_name,
        .kind = module_env.module_kind,
    };

    try internLoweringVisibleNames(module_env, &canonical_names);

    var platform_required_declarations = try PlatformRequiredDeclarationTable.fromModule(allocator, module, &canonical_names);
    errdefer platform_required_declarations.deinit(allocator);

    var checking_context_identity = try CheckingContextIdentity.fromModule(
        allocator,
        module,
        inputs.imports,
        inputs.platform_requirement_context,
        if (inputs.platform_app_relation) |relation| relation.key else null,
    );
    errdefer checking_context_identity.deinit(allocator);

    const direct_import_artifact_keys = try directImportArtifactKeysFromModule(allocator, module, inputs.imports);
    errdefer allocator.free(direct_import_artifact_keys);
    const artifact_key = CheckedModuleArtifactKey.compute(
        module_env.getSourceAll(),
        module_identity,
        checking_context_identity,
        direct_import_artifact_keys,
    );

    const exports = try collectPublishedExportDefs(allocator, module);
    errdefer allocator.free(exports);

    const provides = try publishProvidesMetadata(allocator, module_env, &canonical_names);
    errdefer allocator.free(provides);

    const requires = try publishRequiresMetadata(allocator, module, &canonical_names);
    errdefer allocator.free(requires);

    const owner_artifact = artifactRef(artifact_key);

    var checked_type_publication = try CheckedTypeStore.fromModule(allocator, module, &canonical_names);
    defer checked_type_publication.deinitIndex(allocator);
    errdefer checked_type_publication.store.deinit(allocator);
    try applyPlatformForClauseSubstitutions(
        allocator,
        module,
        &canonical_names,
        &checked_type_publication,
        inputs.relation_artifacts,
        inputs.platform_app_relation,
    );
    const checked_types = &checked_type_publication.store;

    var checked_bodies = try CheckedBodyStore.fromModule(allocator, module, &canonical_names, &checked_type_publication);
    errdefer checked_bodies.deinit(allocator);

    var intrinsic_wrappers = IntrinsicWrapperTable{};
    errdefer intrinsic_wrappers.deinit(allocator);

    var checked_procedure_templates = try CheckedProcedureTemplateTable.fromModule(
        allocator,
        module,
        &canonical_names,
        owner_artifact,
        &checked_type_publication,
        &checked_bodies,
        &intrinsic_wrappers,
    );
    errdefer checked_procedure_templates.deinit(allocator);
    const template_lookup = checked_procedure_templates.asLookup(module_idx);

    var method_registry = try static_dispatch.MethodRegistry.fromModule(allocator, module, &canonical_names, &template_lookup, &checked_type_publication);
    errdefer method_registry.deinit(allocator);

    var static_dispatch_plans = try static_dispatch.StaticDispatchPlanTable.fromModule(allocator, module, &canonical_names, &checked_type_publication, &checked_bodies);
    errdefer static_dispatch_plans.deinit(allocator);
    checked_bodies.attachStaticDispatchPlans(&static_dispatch_plans);

    var hosted_procs = try HostedProcTable.fromModule(allocator, module, &canonical_names, &checked_procedure_templates);
    errdefer hosted_procs.deinit(allocator);

    var platform_requirement_relations = try PlatformRequirementRelationTable.fromRelation(
        allocator,
        module,
        module_identity,
        &canonical_names,
        &checked_type_publication,
        &platform_required_declarations,
        inputs.platform_app_relation,
    );
    errdefer platform_requirement_relations.deinit(allocator);

    var platform_required_bindings = try PlatformRequiredBindingTable.fromRelation(
        allocator,
        module,
        module_identity,
        &canonical_names,
        &platform_required_declarations,
        &platform_requirement_relations,
        inputs.platform_app_relation,
    );
    errdefer platform_required_bindings.deinit(allocator);

    var compile_time_roots = try CompileTimeRootTable.fromModule(allocator, module, &checked_type_publication, &checked_bodies, &checked_procedure_templates);
    errdefer compile_time_roots.deinit(allocator);

    var checked_const_bodies = try CheckedConstBodyTable.fromRoots(allocator, &compile_time_roots);
    errdefer checked_const_bodies.deinit(allocator);

    var entry_wrappers = EntryWrapperTable{};
    errdefer entry_wrappers.deinit(allocator);
    try checked_procedure_templates.appendEntryWrappersForRoots(
        allocator,
        module,
        &canonical_names,
        owner_artifact,
        checked_types,
        &entry_wrappers,
        &compile_time_roots,
    );

    var comptime_values = CompileTimeValueStore.init(allocator);
    errdefer comptime_values.deinit(allocator);

    var const_instances = ConstInstantiationStore.init(artifact_key);
    errdefer const_instances.deinit(allocator);

    var callable_binding_instances = CallableBindingInstantiationStore.init(artifact_key);
    errdefer callable_binding_instances.deinit(allocator);

    var semantic_instantiation_procedures = SemanticInstantiationProcedureTable.init(artifact_key);
    errdefer semantic_instantiation_procedures.deinit(allocator);

    var comptime_dependencies = ComptimeDependencySummaryStore.init(artifact_key);
    errdefer comptime_dependencies.deinit(allocator);
    try comptime_dependencies.reserveRootRequests(allocator, compile_time_roots.roots.len);

    var top_level_procedure_bindings = TopLevelProcedureBindingTable.initEmpty();
    errdefer top_level_procedure_bindings.deinit(allocator);

    var callable_eval_templates = CallableEvalTemplateTable{};
    errdefer callable_eval_templates.deinit(allocator);

    var const_templates = ConstTemplateTable{};
    errdefer const_templates.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(
        allocator,
        module,
        &canonical_names,
        &checked_bodies,
        &checked_procedure_templates,
        &callable_eval_templates,
        &top_level_procedure_bindings,
        &const_templates,
        artifact_key,
        &compile_time_roots,
    );
    errdefer top_level_values.deinit(allocator);
    try comptime_values.sealBindings();

    var resolved_value_refs = try ResolvedValueRefTable.fromModule(
        allocator,
        modules,
        module_idx,
        artifact_key,
        inputs.imports,
        &checked_procedure_templates,
        &hosted_procs,
        &platform_required_declarations,
        &platform_required_bindings,
        &top_level_values,
        &checked_type_publication,
        &checked_bodies,
    );
    errdefer resolved_value_refs.deinit(allocator);
    checked_bodies.attachResolvedValueRefs(&resolved_value_refs);

    var root_requests = try RootRequestTable.fromModule(
        allocator,
        module,
        &checked_type_publication,
        &compile_time_roots,
        &entry_wrappers,
        &platform_required_bindings,
        &checked_bodies,
        &resolved_value_refs,
        inputs.explicit_roots,
    );
    errdefer root_requests.deinit(allocator);

    try sealCheckedProcedureTemplateRefs(
        allocator,
        &checked_bodies,
        &entry_wrappers,
        &checked_procedure_templates,
        &static_dispatch_plans,
        &resolved_value_refs,
    );

    var nested_proc_sites = try NestedProcSiteTable.fromTemplates(allocator, &checked_bodies, &static_dispatch_plans, &entry_wrappers, &checked_procedure_templates);
    errdefer nested_proc_sites.deinit(allocator);

    sealConstEvalTemplatesForRoots(
        &const_templates,
        &compile_time_roots,
        &checked_const_bodies,
        &entry_wrappers,
        &checked_procedure_templates,
        &top_level_values,
    );

    var exported_procedure_templates = try ExportedProcedureTemplateTable.fromModule(
        allocator,
        module,
        exports,
        &canonical_names,
        artifact_key,
        checked_types,
        &checked_procedure_templates,
        &const_templates,
        &resolved_value_refs,
        &top_level_procedure_bindings,
        &platform_required_bindings,
        inputs.imports,
    );
    errdefer exported_procedure_templates.deinit(allocator);

    var exported_procedure_bindings = try ExportedProcedureBindingTable.fromModule(
        allocator,
        module,
        exports,
        checked_types,
        &checked_procedure_templates,
        &const_templates,
        &top_level_values,
        &top_level_procedure_bindings,
        &callable_eval_templates,
        &entry_wrappers,
        &resolved_value_refs,
        &platform_required_bindings,
        inputs.imports,
        artifact_key,
    );
    errdefer exported_procedure_bindings.deinit(allocator);

    var exported_const_templates = try ExportedConstTemplateTable.fromModule(
        allocator,
        module,
        exports,
        artifact_key,
        checked_types,
        &checked_procedure_templates,
        &top_level_values,
        &const_templates,
        &resolved_value_refs,
        &top_level_procedure_bindings,
        &platform_required_bindings,
        inputs.imports,
    );
    errdefer exported_const_templates.deinit(allocator);

    var interface_capabilities = try ModuleInterfaceCapabilities.fromModule(
        allocator,
        module,
        checked_types,
        &hosted_procs,
        &platform_required_declarations,
        &canonical_names,
    );
    errdefer interface_capabilities.deinit(allocator);

    var artifact = CheckedModuleArtifact{
        .key = artifact_key,
        .canonical_names = canonical_names,
        .module_identity = module_identity,
        .checking_context_identity = checking_context_identity,
        .direct_import_artifact_keys = direct_import_artifact_keys,
        .module_env = inputs.module_env_storage,
        .exports = .{ .defs = exports },
        .checked_types = checked_types.*,
        .checked_bodies = checked_bodies,
        .checked_const_bodies = checked_const_bodies,
        .exported_procedure_templates = exported_procedure_templates,
        .exported_procedure_bindings = exported_procedure_bindings,
        .exported_const_templates = exported_const_templates,
        .provides_requires = .{
            .provides = provides,
            .requires = requires,
        },
        .method_registry = method_registry,
        .static_dispatch_plans = static_dispatch_plans,
        .resolved_value_refs = resolved_value_refs,
        .nested_proc_sites = nested_proc_sites,
        .checked_procedure_templates = checked_procedure_templates,
        .entry_wrappers = entry_wrappers,
        .intrinsic_wrappers = intrinsic_wrappers,
        .promoted_callable_wrappers = .{},
        .promoted_callable_body_plans = .{},
        .executable_type_payloads = ExecutableTypePayloadStore.init(allocator),
        .top_level_procedure_bindings = top_level_procedure_bindings,
        .callable_eval_templates = callable_eval_templates,
        .root_requests = root_requests,
        .hosted_procs = hosted_procs,
        .platform_required_declarations = platform_required_declarations,
        .platform_requirement_relations = platform_requirement_relations,
        .platform_required_bindings = platform_required_bindings,
        .interface_capabilities = interface_capabilities,
        .compile_time_roots = compile_time_roots,
        .top_level_values = top_level_values,
        .comptime_dependencies = comptime_dependencies,
        .promoted_procedures = .{},
        .const_templates = const_templates,
        .comptime_values = comptime_values,
        .const_instances = const_instances,
        .callable_binding_instances = callable_binding_instances,
        .semantic_instantiation_procedures = semantic_instantiation_procedures,
    };
    try inputs.compile_time_finalizer.run(
        allocator,
        &artifact,
        inputs.imports,
        inputs.available_artifacts,
        inputs.relation_artifacts,
    );
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
        .resolved_value_refs = .{},
        .checked_procedure_templates = .{},
        .promoted_callable_wrappers = .{},
        .promoted_callable_body_plans = .{},
        .intrinsic_wrappers = .{},
        .executable_type_payloads = ExecutableTypePayloadStore.init(std.testing.allocator),
        .top_level_procedure_bindings = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_declarations = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{},
        .compile_time_roots = .{},
        .top_level_values = .{},
        .comptime_dependencies = ComptimeDependencySummaryStore.init(.{}),
        .promoted_procedures = .{},
        .const_templates = .{},
        .comptime_values = CompileTimeValueStore.init(std.testing.allocator),
        .const_instances = ConstInstantiationStore.init(.{}),
        .callable_binding_instances = CallableBindingInstantiationStore.init(.{}),
        .semantic_instantiation_procedures = SemanticInstantiationProcedureTable.init(.{}),
    };
    defer {
        artifact.semantic_instantiation_procedures.deinit(std.testing.allocator);
        artifact.callable_binding_instances.deinit(std.testing.allocator);
        artifact.const_instances.deinit(std.testing.allocator);
        artifact.const_templates.deinit(std.testing.allocator);
        artifact.comptime_dependencies.deinit(std.testing.allocator);
        artifact.comptime_values.deinit(std.testing.allocator);
        artifact.executable_type_payloads.deinit(std.testing.allocator);
        artifact.canonical_names.deinit();
    }

    const imported = importedView(&artifact);
    const lowering = loweringView(&artifact);
    try std.testing.expect(imported.exports.defs.ptr == artifact.exports.defs.ptr);
    try std.testing.expect(lowering.roots == &artifact.root_requests);
}

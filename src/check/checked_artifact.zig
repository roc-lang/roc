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

fn hashByteSlice(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    hashU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
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

fn artifactRef(key: CheckedModuleArtifactKey) canonical.ArtifactRef {
    return .{ .bytes = key.bytes };
}

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

pub const CheckingContextIdentity = struct {
    imports: []ImportIdentity = &.{},
    platform_requirement_context: ?PlatformRequirementContextKey = null,
    platform_app_relation: ?PlatformAppRelationKey = null,

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        platform_requirement_context: ?PlatformRequirementContextKey,
        platform_app_relation: ?PlatformAppRelationKey,
    ) Allocator.Error!CheckingContextIdentity {
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

pub const PublishImportArtifact = struct {
    module_idx: u32,
    key: CheckedModuleArtifactKey,
    view: ImportedModuleView,
};

pub const PublishInputs = struct {
    module_env_storage: ModuleEnvStorage,
    imports: []const PublishImportArtifact = &.{},
    platform_requirement_context: ?PlatformRequirementContextKey = null,
    platform_app_relation: ?PlatformAppRelation = null,
    explicit_roots: []const ExplicitRootRequestInput = &.{},
    compile_time_finalizer: CompileTimeFinalizer,
};

pub const CompileTimeFinalizer = struct {
    context: ?*anyopaque = null,
    finalize: *const fn (
        context: ?*anyopaque,
        allocator: Allocator,
        artifact: *CheckedModuleArtifact,
        imports: []const PublishImportArtifact,
    ) anyerror!void,

    pub fn run(
        self: CompileTimeFinalizer,
        allocator: Allocator,
        artifact: *CheckedModuleArtifact,
        imports: []const PublishImportArtifact,
    ) anyerror!void {
        try self.finalize(self.context, allocator, artifact, imports);
    }
};

pub const ExplicitRootRequestInput = struct {
    kind: RootRequestKind,
    source: RootSource,
    abi: RootAbi,
    exposure: RootExposure,
};

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

pub const ExportTableView = struct {
    defs: []const CIR.Def.Idx = &.{},
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
    compile_time_callable,
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
    checked_type: CheckedTypeId,
    abi: RootAbi,
    exposure: RootExposure,
    procedure_template: ?canonical.ProcedureTemplateRef = null,
};

pub const RootRequestTable = struct {
    requests: []RootRequest = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypeStore,
        compile_time_roots: *const CompileTimeRootTable,
        entry_wrappers: *const EntryWrapperTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        explicit_roots: []const ExplicitRootRequestInput,
    ) Allocator.Error!RootRequestTable {
        var requests = std.ArrayList(RootRequest).empty;
        errdefer requests.deinit(allocator);

        const module_env = module.moduleEnvConst();

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
                .checked_type = try checkedTypeIdForVar(
                    allocator,
                    module,
                    checked_types,
                    ModuleEnv.varFrom(module_env.requires_types.items.items[binding.requires_idx].type_anno),
                ),
                .abi = .platform,
                .exposure = .platform_required,
            });
        }

        for (compile_time_roots.roots) |root| {
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

fn appendPublishedEntrypointRoots(
    requests: *std.ArrayList(RootRequest),
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: *const CheckedTypeStore,
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
    checked_types: *const CheckedTypeStore,
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

fn checkedTypeIdForVar(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: *const CheckedTypeStore,
    var_: Var,
) Allocator.Error!CheckedTypeId {
    const key = try canonical_type_keys.fromVar(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        var_,
    );
    return checked_types.rootForKey(key) orelse {
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

pub const CheckedBodyId = checked_ids.CheckedBodyId;
pub const CheckedExprId = checked_ids.CheckedExprId;
pub const CheckedPatternId = checked_ids.CheckedPatternId;
pub const CheckedStatementId = checked_ids.CheckedStatementId;
pub const CheckedTypeId = checked_ids.CheckedTypeId;
pub const CheckedTypeSchemeId = checked_ids.CheckedTypeSchemeId;
pub const StaticDispatchPlanId = static_dispatch.StaticDispatchPlanId;
pub const PatternBinderId = enum(u32) { _ };

pub const CheckedTypeRoot = struct {
    id: CheckedTypeId,
    key: canonical.CanonicalTypeKey,
};

pub const CheckedTypeScheme = struct {
    id: CheckedTypeSchemeId,
    key: canonical.CanonicalTypeSchemeKey,
    root: CheckedTypeId,
    generalized_vars: []const CheckedTypeId = &.{},
};

pub const CheckedStaticDispatchConstraint = struct {
    fn_name: canonical.MethodNameId,
    fn_ty: CheckedTypeId,
    origin: types.StaticDispatchConstraint.Origin,
    binop_negated: bool = false,
    num_literal: ?types.NumeralInfo = null,
};

pub const CheckedTypeVariable = struct {
    name: ?[]const u8 = null,
    constraints: []const CheckedStaticDispatchConstraint = &.{},
};

pub const CheckedRecordField = struct {
    name: canonical.RecordFieldLabelId,
    ty: CheckedTypeId,
};

pub const CheckedRecordType = struct {
    fields: []const CheckedRecordField,
    ext: CheckedTypeId,
};

pub const CheckedTag = struct {
    name: canonical.TagLabelId,
    args: []const CheckedTypeId = &.{},
};

pub const CheckedTagUnionType = struct {
    tags: []const CheckedTag,
    ext: CheckedTypeId,
};

pub const CheckedFunctionKind = enum {
    pure,
    effectful,
    unbound,
};

pub const CheckedFunctionType = struct {
    kind: CheckedFunctionKind,
    args: []const CheckedTypeId = &.{},
    ret: CheckedTypeId,
    needs_instantiation: bool,
};

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

pub const CheckedAliasType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleNameId,
    backing: CheckedTypeId,
    args: []const CheckedTypeId = &.{},
};

pub const CheckedNominalType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleNameId,
    builtin: ?CheckedBuiltinNominal = null,
    is_opaque: bool,
    backing: CheckedTypeId,
    args: []const CheckedTypeId = &.{},
};

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

pub const CheckedTypeStoreView = struct {
    roots: []const CheckedTypeRoot = &.{},
    schemes: []const CheckedTypeScheme = &.{},
    payloads: []const CheckedTypePayload = &.{},
};

pub const CheckedTypeStore = struct {
    roots: []CheckedTypeRoot = &.{},
    schemes: []CheckedTypeScheme = &.{},
    payloads: []CheckedTypePayload = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
    ) Allocator.Error!CheckedTypeStore {
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
        var active = std.AutoHashMap(Var, CheckedTypeId).init(allocator);
        defer active.deinit();

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            const tag = module.nodeTag(node);
            if (isExprNodeTag(tag)) {
                _ = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, module.exprType(@enumFromInt(node_idx)));
            } else if (isPatternNodeTag(tag)) {
                _ = try appendCheckedTypeRoot(allocator, module, names, &roots, &payloads, &active, module.patternType(@enumFromInt(node_idx)));
            }
        }

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

        return .{
            .roots = try roots.toOwnedSlice(allocator),
            .schemes = try schemes.toOwnedSlice(allocator),
            .payloads = try payloads.toOwnedSlice(allocator),
        };
    }

    pub fn view(self: *const CheckedTypeStore) CheckedTypeStoreView {
        return .{ .roots = self.roots, .schemes = self.schemes, .payloads = self.payloads };
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
        const key = syntheticFunctionTypeKey(self, kind, args, ret);
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
            .kind = kind,
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
        allocator.free(self.payloads);
        allocator.free(self.schemes);
        allocator.free(self.roots);
        self.* = .{};
    }
};

fn syntheticFunctionTypeKey(
    store: *const CheckedTypeStore,
    kind: CheckedFunctionKind,
    args: []const CheckedTypeId,
    ret: CheckedTypeId,
) canonical.CanonicalTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashByteSlice(&hasher, "checked_synthetic_function");
    hashByteSlice(&hasher, @tagName(kind));
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
    const key = try canonical_type_keys.fromVar(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        resolved_var,
    );
    if (findCheckedTypeRoot(roots.items, key)) |id| return id;
    if (active.get(resolved_var)) |id| return id;

    const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
    try roots.append(allocator, .{ .id = id, .key = key });
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
    _ = active.remove(resolved_var);

    deinitCheckedTypePayload(allocator, &payloads.items[@intFromEnum(id)]);
    payloads.items[@intFromEnum(id)] = payload;
    return id;
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
        } },
        .rigid => |rigid| .{ .rigid = .{
            .name = try copyIdentText(allocator, module, rigid.name),
            .constraints = try copyCheckedStaticDispatchConstraints(allocator, module, names, roots, payloads, active, rigid.constraints),
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
        .fn_unbound => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, roots, payloads, active, .unbound, func) },
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
        .kind = kind,
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

pub const CheckedBody = struct {
    id: CheckedBodyId,
    root_expr: CheckedExprId,
    owner_template: canonical.ProcedureTemplateRef,
};

pub const CheckedPatternBinder = struct {
    id: PatternBinderId,
    pattern: CheckedPatternId,
};

pub const CheckedStringLiteralId = enum(u32) { _ };

pub const CheckedRecordExprField = struct {
    label: canonical.RecordFieldLabelId,
    value: CheckedExprId,
};

pub const CheckedIfBranch = struct {
    cond: CheckedExprId,
    body: CheckedExprId,
};

pub const CheckedMatchBranchPattern = struct {
    pattern: CheckedPatternId,
    degenerate: bool,
    binder_remaps: []const CheckedAlternativeBinderRemap,
};

pub const CheckedAlternativeBinderRemap = struct {
    candidate_binder: PatternBinderId,
    representative_binder: PatternBinderId,
};

pub const CheckedMatchBranch = struct {
    patterns: []const CheckedMatchBranchPattern,
    value: CheckedExprId,
    guard: ?CheckedExprId,
};

pub const CheckedCapture = struct {
    pattern: CheckedPatternId,
    scope_depth: u32,
};

pub const CheckedRecordDestructKind = union(enum) {
    required: CheckedPatternId,
    sub_pattern: CheckedPatternId,
    rest: CheckedPatternId,
};

pub const CheckedRecordDestruct = struct {
    label: canonical.RecordFieldLabelId,
    kind: CheckedRecordDestructKind,
};

pub const CheckedListRestPattern = struct {
    index: u32,
    pattern: ?CheckedPatternId,
};

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

pub const CheckedReturnContext = enum {
    return_expr,
    try_suffix,
};

pub const CheckedExpr = struct {
    id: CheckedExprId,
    ty: CheckedTypeId,
    source_region: base.Region,
    data: CheckedExprData,
};

pub const CheckedPattern = struct {
    id: CheckedPatternId,
    ty: CheckedTypeId,
    source_region: base.Region,
    data: CheckedPatternData,
};

pub const CheckedStatement = struct {
    id: CheckedStatementId,
    source_region: base.Region,
    data: CheckedStatementData,
};

pub const CheckedBodyStoreView = struct {
    bodies: []const CheckedBody = &.{},
    exprs: []const CheckedExpr = &.{},
    patterns: []const CheckedPattern = &.{},
    statements: []const CheckedStatement = &.{},
    string_literals: []const []const u8 = &.{},
    pattern_binders: []const CheckedPatternBinder = &.{},
    pattern_binder_by_pattern: []const ?PatternBinderId = &.{},
};

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
        checked_types: *const CheckedTypeStore,
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
                const key = try canonical_type_keys.fromVar(
                    allocator,
                    module.typeStoreConst(),
                    module.identStoreConst(),
                    module.exprType(expr_idx),
                );
                const ty = checked_types.rootForKey(key) orelse {
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
                const key = try canonical_type_keys.fromVar(
                    allocator,
                    module.typeStoreConst(),
                    module.identStoreConst(),
                    module.patternType(pattern_idx),
                );
                const ty = checked_types.rootForKey(key) orelse {
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

    pub fn exprIdForSource(self: *const CheckedBodyStore, expr: CIR.Expr.Idx) ?CheckedExprId {
        const raw = @intFromEnum(expr);
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
            switch (&self.exprs[@intFromEnum(checked_expr)].data) {
                .dispatch_call => |slot| slot.* = entry.value_ptr.*,
                .method_eq => |slot| slot.* = entry.value_ptr.*,
                .type_dispatch_call => |slot| slot.* = entry.value_ptr.*,
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
            switch (&self.exprs[@intFromEnum(record.expr)].data) {
                .lookup_local => |lookup| lookup.resolved = ref_id,
                .lookup_external => |slot| slot.* = ref_id,
                .lookup_required => |slot| slot.* = ref_id,
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
                for (self.module.sliceRecordDestructs(record.destructs)) |destruct| {
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

pub const CheckedProcedureBody = union(enum) {
    checked_body: CheckedBodyId,
    promoted_callable_wrapper: canonical.PromotedCallableWrapperId,
    hosted_wrapper: canonical.HostedWrapperId,
    intrinsic_wrapper: canonical.IntrinsicWrapperId,
    entry_wrapper: canonical.EntryWrapperId,
};

pub const EntryWrapper = struct {
    id: canonical.EntryWrapperId,
    root: ComptimeRootId,
    template: canonical.ProcedureTemplateRef,
    checked_fn_root: CheckedTypeId,
    body_expr: CheckedExprId,
};

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

pub const PromotedWrapperParam = struct {
    index: u32,
    checked_ty: CheckedTypeId,
    source_ty: canonical.CanonicalTypeKey,
};

pub const PrivateCaptureRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: PromotedCaptureId,
    node: PrivateCaptureNodeId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
};

pub const PrivateCaptureInstantiationKey = struct {
    capture_ref: PrivateCaptureRef,
    requested_source_ty: canonical.CanonicalTypeKey,
};

pub const PromotedWrapperArg = union(enum) {
    param: u32,
    private_capture: PrivateCaptureRef,
};

pub const ExecutableValueTransformPlanId = enum(u32) { _ };
pub const SessionExecutableValueTransformId = enum(u32) { _ };

pub const PublishedExecutableValueTransformRef = struct {
    artifact: CheckedModuleArtifactKey,
    transform: ExecutableValueTransformPlanId,
};

pub const ExecutableValueTransformRef = union(enum) {
    session: SessionExecutableValueTransformId,
    published: PublishedExecutableValueTransformRef,
};

pub const ExecutableValueEndpoint = struct {
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

pub const ValueTransformRecordField = struct {
    field: canonical.RecordFieldLabelId,
    transform: ExecutableValueTransformPlanId,
};

pub const ValueTransformTupleElem = struct {
    index: u32,
    transform: ExecutableValueTransformPlanId,
};

pub const ValueTransformTagPayloadEdge = struct {
    source_payload_index: u32,
    target_payload_index: u32,
    transform: ExecutableValueTransformPlanId,
};

pub const ValueTransformTagCase = struct {
    source_tag: canonical.TagLabelId,
    target_tag: canonical.TagLabelId,
    payloads: []const ValueTransformTagPayloadEdge = &.{},
};

pub const BoxPayloadTransformKind = enum {
    payload_to_box,
    box_to_payload,
    box_to_box,
};

pub const BoxPayloadTransformPlan = struct {
    boundary: canonical.BoxBoundaryId,
    kind: BoxPayloadTransformKind,
    payload: ExecutableValueTransformPlanId,
};

pub const ExecutableValueTransformOp = union(enum) {
    identity,
    structural_bridge: ExecutableStructuralBridgePlan,
    record: []const ValueTransformRecordField,
    tuple: []const ValueTransformTupleElem,
    tag_union: []const ValueTransformTagCase,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: ExecutableValueTransformPlanId,
    },
    list: struct {
        elem: ExecutableValueTransformPlanId,
    },
    box_payload: BoxPayloadTransformPlan,
    callable_to_erased: CallableToErasedTransformPlan,
    already_erased_callable: AlreadyErasedCallableTransformPlan,
};

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

pub const CallableToErasedTransformPlan = union(enum) {
    finite_value: FiniteCallableValueToErasedPlan,
    proc_value: ProcValueToErasedPlan,
};

pub const FiniteCallableValueToErasedPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    adapter_key: canonical.ErasedAdapterKey,
};

pub const ProcValueToErasedPlan = struct {
    proc_value: canonical.ProcedureCallableRef,
    erased_fn_sig_key: canonical.ErasedFnSigKey,
    capture_shape_key: canonical.CaptureShapeKey,
    executable_specialization_key: canonical.ExecutableSpecializationKey,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

pub const AlreadyErasedCallableTransformPlan = struct {
    sig_key: canonical.ErasedFnSigKey,
};

pub const ValueTransformProvenance = union(enum) {
    none,
    box_erasure: []const canonical.BoxBoundaryId,
};

pub const ExecutableValueTransformPlan = struct {
    from: ExecutableValueEndpoint,
    to: ExecutableValueEndpoint,
    provenance: ValueTransformProvenance = .none,
    op: ExecutableValueTransformOp,
};

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
            verifyExecutableTypePayloadRef(payloads, artifact_key, plan.from.ty);
            verifyExecutableTypePayloadRef(payloads, artifact_key, plan.to.ty);
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

pub const ExecutableTypePayloadId = enum(u32) { _ };

pub const ExecutableTypePayloadRef = struct {
    artifact: canonical.ArtifactRef,
    payload: ExecutableTypePayloadId,
};

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

pub const ExecutableTypePayloadChild = struct {
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

pub const ExecutableRecordFieldPayload = struct {
    field: canonical.RecordFieldLabelId,
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

pub const ExecutableTupleElemPayload = struct {
    index: u32,
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

pub const ExecutableTagPayload = struct {
    index: u32,
    ty: ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

pub const ExecutableTagVariantPayload = struct {
    tag: canonical.TagLabelId,
    payloads: []const ExecutableTagPayload = &.{},
};

pub const ExecutableNominalPayload = struct {
    nominal: canonical.NominalTypeKey,
    backing: ExecutableTypePayloadRef,
    backing_key: canonical.CanonicalExecValueTypeKey,
};

pub const ExecutableCallableSetMemberPayload = struct {
    member: canonical.CallableSetMemberId,
    payload_ty: ?ExecutableTypePayloadRef = null,
    payload_ty_key: ?canonical.CanonicalExecValueTypeKey = null,
};

pub const ExecutableCallableSetPayload = struct {
    key: canonical.CanonicalCallableSetKey,
    members: []const ExecutableCallableSetMemberPayload = &.{},
};

pub const ExecutableErasedFnPayload = struct {
    sig_key: canonical.ErasedFnSigKey,
    capture_shape_key: canonical.CaptureShapeKey,
    capture_ty: ?ExecutableTypePayloadRef = null,
    capture_ty_key: ?canonical.CanonicalExecValueTypeKey = null,
};

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
    recursive_ref: ExecutableTypePayloadId,
};

pub const ExecutableTypePayloadStore = struct {
    payloads: []ExecutableTypePayload = &.{},

    pub fn reserve(
        self: *ExecutableTypePayloadStore,
        allocator: Allocator,
    ) Allocator.Error!ExecutableTypePayloadId {
        return try self.append(allocator, .pending);
    }

    pub fn append(
        self: *ExecutableTypePayloadStore,
        allocator: Allocator,
        payload: ExecutableTypePayload,
    ) Allocator.Error!ExecutableTypePayloadId {
        const id: ExecutableTypePayloadId = @enumFromInt(@as(u32, @intCast(self.payloads.len)));
        const old = self.payloads;
        const next = try allocator.alloc(ExecutableTypePayload, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = payload;
        allocator.free(old);
        self.payloads = next;
        return id;
    }

    pub fn fill(
        self: *ExecutableTypePayloadStore,
        id: ExecutableTypePayloadId,
        payload: ExecutableTypePayload,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.payloads.len) {
            checkedArtifactInvariant("executable type payload id is out of range", .{});
        }
        switch (payload) {
            .pending => checkedArtifactInvariant("cannot fill executable type payload with pending", .{}),
            else => {},
        }
        switch (self.payloads[index]) {
            .pending => self.payloads[index] = payload,
            else => checkedArtifactInvariant("executable type payload was filled twice", .{}),
        }
    }

    pub fn get(self: *const ExecutableTypePayloadStore, id: ExecutableTypePayloadId) ExecutableTypePayload {
        const index = @intFromEnum(id);
        if (index >= self.payloads.len) {
            checkedArtifactInvariant("executable type payload id is out of range", .{});
        }
        return self.payloads[index];
    }

    pub fn verifyPublished(self: *const ExecutableTypePayloadStore, artifact_key: CheckedModuleArtifactKey) void {
        if (builtin.mode != .Debug) return;
        for (self.payloads) |payload| verifyExecutableTypePayload(self, artifact_key, payload);
    }

    pub fn deinit(self: *ExecutableTypePayloadStore, allocator: Allocator) void {
        for (self.payloads) |*payload| deinitExecutableTypePayload(allocator, payload);
        allocator.free(self.payloads);
        self.* = .{};
    }
};

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
        if (self.descriptors.len != 0) {
            for (source_descriptors) |source| {
                const existing = self.descriptorFor(source.key) orelse {
                    checkedArtifactInvariant("callable-set descriptor store was already published with different descriptor keys", .{});
                };
                if (!canonicalCallableSetDescriptorEql(existing.*, source)) {
                    checkedArtifactInvariant("callable-set descriptor store was already published with different descriptor contents", .{});
                }
            }
            return;
        }
        if (source_descriptors.len == 0) return;

        var unique = std.ArrayList(canonical.CanonicalCallableSetDescriptor).empty;
        defer unique.deinit(allocator);
        for (source_descriptors) |source| {
            var found = false;
            for (unique.items) |existing| {
                if (!canonicalCallableSetKeyEql(existing.key, source.key)) continue;
                found = true;
                if (!canonicalCallableSetDescriptorEql(existing, source)) {
                    checkedArtifactInvariant("duplicate callable-set descriptor key has different descriptor contents", .{});
                }
                break;
            }
            if (!found) try unique.append(allocator, source);
        }

        const copied = try allocator.alloc(canonical.CanonicalCallableSetDescriptor, unique.items.len);
        errdefer allocator.free(copied);

        var descriptor_count: usize = 0;
        errdefer {
            for (copied[0..descriptor_count]) |descriptor| {
                for (descriptor.members) |member| allocator.free(member.capture_slots);
                allocator.free(descriptor.members);
            }
        }

        for (unique.items, 0..) |descriptor, i| {
            const members = try cloneCallableSetMembers(allocator, descriptor.members);
            copied[i] = .{
                .key = descriptor.key,
                .members = members,
            };
            descriptor_count += 1;
        }

        self.descriptors = copied;
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

pub const ExecutableProcedureParamPayload = struct {
    param: PromotedWrapperParam,
    exec_ty: ExecutableTypePayloadRef,
    exec_ty_key: canonical.CanonicalExecValueTypeKey,
};

pub const ExecutableHiddenCapturePayload = struct {
    exec_ty: ExecutableTypePayloadRef,
    exec_ty_key: canonical.CanonicalExecValueTypeKey,
};

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

pub const FinitePromotedWrapperBodyPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    member: canonical.CallableSetMemberId,
    member_proc: canonical.ProcedureCallableRef,
    member_capture_shape: canonical.CaptureShapeKey,
    member_capture_slots: []const canonical.CallableSetCaptureSlot = &.{},
    captures: []const PrivateCaptureRef = &.{},
    params: []const PromotedWrapperParam = &.{},
    call_args: []const PromotedWrapperArg = &.{},
};

pub const ErasedHiddenCaptureArgPlan = union(enum) {
    none,
    materialized_capture: ErasedCaptureExecutableMaterializationPlan,
};

pub const ErasedCaptureExecutableMaterializationPlan = union(enum) {
    none,
    zero_sized_typed: canonical.CanonicalExecValueTypeKey,
    node: ErasedCaptureExecutableMaterializationNodeId,
};

pub const NoReachableCallableSlotsProof = enum {
    checked_artifact_verified,
};

pub const PureConstInstanceRef = struct {
    const_instance: ConstInstanceRef,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

pub const PureComptimeValueRef = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

pub const ErasedCaptureExecutableMaterializationRecordField = struct {
    field: canonical.RecordFieldLabelId,
    value: ErasedCaptureExecutableMaterializationPlan,
};

pub const ErasedCaptureExecutableMaterializationTagPayload = struct {
    index: u32,
    value: ErasedCaptureExecutableMaterializationPlan,
};

pub const ErasedCaptureExecutableMaterializationTagNode = struct {
    tag: canonical.TagLabelId,
    payloads: []const ErasedCaptureExecutableMaterializationTagPayload,
};

pub const MaterializedFiniteCallableSetValue = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    selected_member: canonical.CallableSetMemberId,
    captures: []const ErasedCaptureExecutableMaterializationPlan = &.{},
};

pub const MaterializedErasedCallableValue = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    sig_key: canonical.ErasedFnSigKey,
    code: canonical.ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
    provenance: []const canonical.BoxBoundaryId,
};

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

pub const ErasedPromotedWrapperBodyPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    params: []const PromotedWrapperParam = &.{},
    executable_signature: ErasedPromotedProcedureExecutableSignature,
    sig_key: canonical.ErasedFnSigKey,
    code: canonical.ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
    arg_transforms: []const PublishedExecutableValueTransformRef = &.{},
    hidden_capture_arg: ErasedHiddenCaptureArgPlan = .none,
    result_transform: PublishedExecutableValueTransformRef,
    provenance: []const canonical.BoxBoundaryId,
};

pub const PromotedCallableBodyPlan = union(enum) {
    pending,
    finite: FinitePromotedWrapperBodyPlan,
    erased: ErasedPromotedWrapperBodyPlan,
};

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
        callable_set_descriptors: *const CallableSetDescriptorStore,
        executable_type_payloads: *const ExecutableTypePayloadStore,
        artifact_key: CheckedModuleArtifactKey,
    ) void {
        if (builtin.mode != .Debug) return;

        for (self.plans) |plan| verifyPromotedCallableBodyPlan(
            plans,
            callable_set_descriptors,
            executable_type_payloads,
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

pub const PromotedCallableWrapper = struct {
    id: canonical.PromotedCallableWrapperId,
    promoted_proc: canonical.ProcedureValueRef,
    proc_base_key: canonical.ProcBaseKeyRef,
    callable_node: canonical.PromotedCallableNodeId,
    source_binding: CIR.Pattern.Idx,
    checked_fn_root: CheckedTypeId,
    body_plan: canonical.PromotedCallableBodyPlanId,
};

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

pub const StaticDispatchPlanTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const ResolvedValueRefTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const ResolvedValueRefId = enum(u32) { _ };

pub const LocalBindingRef = struct {
    binder: PatternBinderId,
};

pub const TopLevelBindingRef = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CIR.Pattern.Idx,
};

pub const ImportedTopLevelValueRef = struct {
    artifact: CheckedModuleArtifactKey,
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CIR.Pattern.Idx,
};

pub const HostedProcRef = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

pub const TopLevelValueRef = struct {
    artifact: CheckedModuleArtifactKey,
    pattern: CIR.Pattern.Idx,
};

pub const RequiredAppProcedureRef = struct {
    artifact: CheckedModuleArtifactKey,
    app_value: TopLevelValueRef,
    procedure_binding: TopLevelProcedureBindingRef,
};

pub const PromotedProcedureRef = struct {
    module_idx: u32,
    proc: canonical.ProcedureValueRef,
};

pub const ConstUseTemplate = struct {
    const_ref: ConstRef,
    requested_source_ty_template: canonical.CanonicalTypeKey,
};

pub const ProcedureBindingRef = union(enum) {
    top_level: TopLevelProcedureBindingRef,
    imported: ImportedProcedureBindingRef,
    hosted: HostedProcRef,
    platform_required: RequiredAppProcedureRef,
    promoted: PromotedProcedureRef,
};

pub const TopLevelProcedureBindingRef = enum(u32) { _ };
pub const CallableEvalTemplateId = enum(u32) { _ };

pub const DirectProcedureBinding = struct {
    proc_value: canonical.ProcedureValueRef,
    template: canonical.CallableProcedureTemplateRef,
};

pub const ProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateId,
};

pub const TopLevelProcedureBinding = struct {
    source_scheme: canonical.CanonicalTypeSchemeKey,
    body: ProcedureBindingBody,
};

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

pub const CallableEvalTemplate = struct {
    id: CallableEvalTemplateId,
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
    root: ComptimeRootId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    checked_fn_root: CheckedTypeId,
};

pub const CallableEvalTemplateTableView = struct {
    templates: []const CallableEvalTemplate = &.{},
};

pub const CallableEvalTemplateTable = struct {
    templates: []CallableEvalTemplate = &.{},

    pub fn append(
        self: *CallableEvalTemplateTable,
        allocator: Allocator,
        module_idx: u32,
        pattern: CIR.Pattern.Idx,
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

pub const ImportedProcedureBindingRef = struct {
    artifact: CheckedModuleArtifactKey,
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CIR.Pattern.Idx,
};

pub const ProcedureUseTemplate = struct {
    binding: ProcedureBindingRef,
    source_fn_ty_template: canonical.CanonicalTypeKey,
};

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
    platform_required_const: ConstUseTemplate,
    platform_required_proc: ProcedureUseTemplate,
    promoted_top_level_proc: ProcedureUseTemplate,
};

pub const ResolvedValueRefRecord = struct {
    expr: CheckedExprId,
    ref: ResolvedValueRef,
    checked_ty: CheckedTypeId,
    scope_depth: u32,
};

pub const ResolvedValueRefTable = struct {
    records: []ResolvedValueRefRecord = &.{},
    by_checked_expr: []?ResolvedValueRefId = &.{},
    template_refs: []ResolvedValueRefId = &.{},

    pub fn fromModule(
        allocator: Allocator,
        modules: *const TypedCIR.Modules,
        module_idx: u32,
        imports: []const PublishImportArtifact,
        templates: *const CheckedProcedureTemplateTable,
        hosted_procs: *const HostedProcTable,
        platform_required_declarations: *const PlatformRequiredDeclarationTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        top_level_values: *const TopLevelValueTable,
        checked_types: *const CheckedTypeStore,
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
            attachUseTypeKey(&resolved_ref, checked_type_key);
            const checked_ty = checked_types.rootForKey(checked_type_key) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: resolved value ref type root was not published", .{});
                }
                unreachable;
            };

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

fn attachUseTypeKey(ref: *ResolvedValueRef, key: canonical.CanonicalTypeKey) void {
    switch (ref.*) {
        .top_level_const => |*use| use.requested_source_ty_template = key,
        .imported_const => |*use| use.requested_source_ty_template = key,
        .platform_required_const => |*use| use.requested_source_ty_template = key,
        .top_level_proc => |*use| use.source_fn_ty_template = key,
        .imported_proc => |*use| use.source_fn_ty_template = key,
        .hosted_proc => |*use| use.source_fn_ty_template = key,
        .platform_required_proc => |*use| use.source_fn_ty_template = key,
        .promoted_top_level_proc => |*use| use.source_fn_ty_template = key,
        .local_param,
        .local_value,
        .local_mutable_version,
        .pattern_binder,
        .local_proc,
        .platform_required_declaration,
        => {},
    }
}

fn classifyLocalValueRef(
    module: TypedCIR.Module,
    pattern: CIR.Pattern.Idx,
    hosted_procs: *const HostedProcTable,
    top_level_values: *const TopLevelValueTable,
    checked_bodies: *const CheckedBodyStore,
) ResolvedValueRef {
    _ = checked_bodies.patternIdForSource(pattern) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: local lookup pattern {d} has no checked pattern id",
                .{@intFromEnum(pattern)},
            );
        }
        unreachable;
    };

    if (topLevelDefByPattern(module, pattern)) |def_idx| {
        const entry = topLevelValueForPattern(top_level_values, pattern) orelse {
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
        .const_value => |const_use| .{ .platform_required_const = const_use },
        .procedure_value => |proc_use| .{ .platform_required_proc = proc_use },
    };
}

fn topLevelDefByPattern(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) ?CIR.Def.Idx {
    for (module.allDefs()) |def_idx| {
        if (module.def(def_idx).pattern.idx == pattern) return def_idx;
    }
    return null;
}

fn topLevelValueForPattern(table: *const TopLevelValueTable, pattern: CIR.Pattern.Idx) ?TopLevelValueEntry {
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
    var collector = CheckedTemplateRefCollector.init(allocator, checked_bodies);
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
            .promoted_callable_wrapper,
            .hosted_wrapper,
            .intrinsic_wrapper,
            => {},
        }

        template.resolved_value_refs = try resolved_value_refs.appendTemplateRefSpan(allocator, collector.value_refs.items);
        const dispatch_span = try static_dispatch_plans.appendTemplateRefSpan(allocator, collector.dispatch_refs.items);
        template.static_dispatch_plans = .{
            .start = dispatch_span.start,
            .len = dispatch_span.len,
        };
    }
}

const CheckedTemplateRefCollector = struct {
    allocator: Allocator,
    checked_bodies: *const CheckedBodyStore,
    value_refs: std.ArrayList(ResolvedValueRefId),
    dispatch_refs: std.ArrayList(static_dispatch.StaticDispatchPlanId),
    visited_exprs: std.AutoHashMap(CheckedExprId, void),
    visited_patterns: std.AutoHashMap(CheckedPatternId, void),
    visited_statements: std.AutoHashMap(CheckedStatementId, void),

    fn init(allocator: Allocator, checked_bodies: *const CheckedBodyStore) CheckedTemplateRefCollector {
        return .{
            .allocator = allocator,
            .checked_bodies = checked_bodies,
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
                if (plan_id) |id| try self.dispatch_refs.append(self.allocator, id);
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
                try self.collectExpr(ret.lambda);
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
                try self.collectExpr(ret.lambda);
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

pub const TopLevelUseSummaryRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const NestedProcSiteTableRef = struct {
    start: u32 = 0,
    len: u32 = 0,
};

pub const NestedProcKind = enum {
    local_function,
    closure,
    desugared_closure,
};

pub const NestedProcPathComponent = union(enum) {
    expr: CheckedExprId,
    pattern: CheckedPatternId,
    statement: CheckedStatementId,
    branch: u32,
    desugar: u32,
};

pub const NestedProcSite = struct {
    site: canonical.NestedProcSiteId,
    owner_template: canonical.ProcedureTemplateRef,
    site_path: []const NestedProcPathComponent,
    kind: NestedProcKind,
    checked_expr: ?CheckedExprId,
    checked_pattern: ?CheckedPatternId,
};

pub const NestedProcSiteTable = struct {
    sites: []NestedProcSite = &.{},
    template_refs: []canonical.NestedProcSiteId = &.{},

    pub fn fromTemplates(
        allocator: Allocator,
        checked_bodies: *const CheckedBodyStore,
        entry_wrappers: *const EntryWrapperTable,
        templates: *CheckedProcedureTemplateTable,
    ) Allocator.Error!NestedProcSiteTable {
        var builder = NestedProcSiteBuilder.init(allocator, checked_bodies);
        defer builder.deinitScratch();
        errdefer builder.deinitAll();

        for (templates.templates) |*template| {
            const start: u32 = @intCast(builder.template_refs.items.len);
            switch (template.body) {
                .checked_body => |body_id| try builder.scanCheckedBody(body_id, template),
                .entry_wrapper => |wrapper_id| try builder.scanEntryWrapper(entry_wrappers.get(wrapper_id), template),
                .promoted_callable_wrapper,
                .hosted_wrapper,
                .intrinsic_wrapper,
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

pub const ProcTarget = union(enum) {
    roc,
    hosted,
    intrinsic,
    entry,
    comptime_only,
    promoted_callable,
};

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

pub const CheckedProcedureTemplateTable = struct {
    templates: []CheckedProcedureTemplate = &.{},
    by_def: []?canonical.ProcedureTemplateRef = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        owner_artifact: canonical.ArtifactRef,
        checked_types: *const CheckedTypeStore,
        checked_bodies: *CheckedBodyStore,
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
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = export_name,
                .kind = if (isHostedProcedureExpr(def.expr.data)) .hosted_wrapper else .checked_source,
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
            const checked_fn_key = try canonical_type_keys.fromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                module.defType(def_idx),
            );
            const checked_fn_root = checked_types.rootForKey(checked_fn_key) orelse {
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
            const root_expr = checked_bodies.exprIdForSource(def.expr.idx) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: checked procedure body root expression was not published", .{});
                }
                unreachable;
            };
            const body = try checked_bodies.appendBody(allocator, root_expr, template_ref);

            try templates.append(allocator, .{
                .proc_base = proc_base,
                .template_id = template_id,
                .body = .{ .checked_body = body },
                .checked_fn_scheme = checked_fn_scheme,
                .checked_fn_root = checked_fn_root,
                .static_dispatch_plans = .{},
                .resolved_value_refs = .{},
                .top_level_value_uses = .{},
                .nested_proc_sites = .{},
                .target = if (isHostedProcedureExpr(def.expr.data)) .hosted else .roc,
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

pub const CheckedProcedureTemplateTableView = struct {
    templates: []const CheckedProcedureTemplate = &.{},
};

const NestedProcSiteBuilder = struct {
    allocator: Allocator,
    checked_bodies: *const CheckedBodyStore,
    sites: std.ArrayList(NestedProcSite),
    template_refs: std.ArrayList(canonical.NestedProcSiteId),
    path: std.ArrayList(NestedProcPathComponent),

    fn init(allocator: Allocator, checked_bodies: *const CheckedBodyStore) NestedProcSiteBuilder {
        return .{
            .allocator = allocator,
            .checked_bodies = checked_bodies,
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
        self.* = NestedProcSiteBuilder.init(self.allocator, self.checked_bodies);
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
                try self.scanExpr(ret.lambda, owner, true);
            },
            .field_access => |field| try self.scanExpr(field.receiver, owner, false),
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
            .dispatch_call,
            .method_eq,
            .type_dispatch_call,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .pending,
            => {},
        }
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
                try self.scanExpr(ret.lambda, owner, true);
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

pub const HostedProc = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    expr_idx: CIR.Expr.Idx,
    external_symbol_name: canonical.ExternalSymbolNameId,
    deterministic_index: u32,
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

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
        errdefer procs.deinit(allocator);

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
        allocator.free(self.procs);
        self.* = .{};
    }
};

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

pub const PlatformRequiredDeclarationId = enum(u32) { _ };
pub const PlatformRequiredBindingId = enum(u32) { _ };
pub const PlatformRequirementRelationId = enum(u32) { _ };

pub const PlatformRequiredDeclaration = struct {
    id: PlatformRequiredDeclarationId,
    module_idx: u32,
    requires_idx: u32,
    platform_name: canonical.ExportNameId,
    declared_source_ty: canonical.CanonicalTypeSchemeKey,
    type_anno: CIR.TypeAnno.Idx,
};

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

pub const PlatformRequiredValueUse = union(enum) {
    const_value: ConstUseTemplate,
    procedure_value: ProcedureUseTemplate,
};

pub const PlatformRequiredBindingInput = struct {
    declaration: PlatformRequiredDeclarationId,
    requires_idx: u32,
    app_value: TopLevelValueRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    checked_relation: PlatformRequirementRelationId,
    value_use: PlatformRequiredValueUse,
};

pub const PlatformAppRelation = struct {
    key: PlatformAppRelationKey,
    requirement_context: PlatformRequirementContextKey,
    platform_module_idx: u32,
    app_artifact: CheckedModuleArtifactKey,
    bindings: []const PlatformRequiredBindingInput,

    pub fn deinit(self: *PlatformAppRelation, allocator: Allocator) void {
        allocator.free(self.bindings);
        self.* = .{
            .key = .{},
            .requirement_context = .{},
            .platform_module_idx = 0,
            .app_artifact = .{},
            .bindings = &.{},
        };
    }
};

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

pub const PlatformRequiredBindingTable = struct {
    bindings: []PlatformRequiredBinding = &.{},

    pub fn fromRelation(
        allocator: Allocator,
        module: TypedCIR.Module,
        module_identity: ModuleIdentity,
        names: *const canonical.CanonicalNameStore,
        declarations: *const PlatformRequiredDeclarationTable,
        relation: ?PlatformAppRelation,
    ) Allocator.Error!PlatformRequiredBindingTable {
        const active_relation = relation orelse return .{};
        if (active_relation.platform_module_idx != module.moduleIndex()) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: platform/app relation belongs to module {d}, not platform module {d}",
                    .{ active_relation.platform_module_idx, module.moduleIndex() },
                );
            }
            unreachable;
        }
        if (active_relation.bindings.len != declarations.declarations.len) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: platform/app relation has {d} bindings for {d} platform requirements",
                    .{ active_relation.bindings.len, declarations.declarations.len },
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

        var seen_declarations: []bool = &.{};
        if (builtin.mode == .Debug) {
            seen_declarations = try allocator.alloc(bool, declarations.declarations.len);
            @memset(seen_declarations, false);
        }
        defer {
            if (builtin.mode == .Debug) allocator.free(seen_declarations);
        }

        const bindings = try allocator.alloc(PlatformRequiredBinding, active_relation.bindings.len);
        errdefer allocator.free(bindings);

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
            bindings[i] = .{
                .id = @enumFromInt(@as(u32, @intCast(i))),
                .relation = active_relation.key,
                .module_idx = module.moduleIndex(),
                .declaration = binding.declaration,
                .requires_idx = binding.requires_idx,
                .app_value = binding.app_value,
                .requested_source_ty = binding.requested_source_ty,
                .checked_relation = binding.checked_relation,
                .value_use = binding.value_use,
            };
        }

        return .{ .bindings = bindings };
    }

    pub fn deinit(self: *PlatformRequiredBindingTable, allocator: Allocator) void {
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

pub fn platformRequirementContextKey(artifact: *const CheckedModuleArtifact) PlatformRequirementContextKey {
    return PlatformRequirementContextKey.compute(
        artifact.module_identity,
        artifact.platform_required_declarations.identityHash(&artifact.canonical_names),
    );
}

pub fn buildPlatformAppRelation(
    allocator: Allocator,
    platform_declaration_artifact: *const CheckedModuleArtifact,
    platform_module_env: *const ModuleEnv,
    app_artifact: *const CheckedModuleArtifact,
) Allocator.Error!PlatformAppRelation {
    const declarations = platform_declaration_artifact.platform_required_declarations.declarations;
    const bindings = try allocator.alloc(PlatformRequiredBindingInput, declarations.len);
    errdefer allocator.free(bindings);

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

        bindings[i] = .{
            .declaration = declaration.id,
            .requires_idx = declaration.requires_idx,
            .app_value = app_value_ref,
            .requested_source_ty = requested_source_ty,
            .checked_relation = @enumFromInt(@as(u32, @intCast(i))),
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
                break :blk .{ .procedure_value = .{
                    .binding = .{ .platform_required = .{
                        .artifact = app_artifact.key,
                        .app_value = app_value_ref,
                        .procedure_binding = procedure_binding,
                    } },
                    .source_fn_ty_template = requested_source_ty,
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
                break :blk .{ .const_value = .{
                    .const_ref = const_ref,
                    .requested_source_ty_template = requested_source_ty,
                } };
            },
        };
    }

    return .{
        .key = relation_key,
        .requirement_context = requirement_context,
        .platform_module_idx = platform_declaration_artifact.module_identity.module_idx,
        .app_artifact = app_artifact.key,
        .bindings = bindings,
    };
}

fn appTopLevelValueByName(
    app_artifact: *const CheckedModuleArtifact,
    required_name: []const u8,
) ?TopLevelValueEntry {
    const app_env = app_artifact.moduleEnv();
    for (app_artifact.top_level_values.entries) |entry| {
        const pattern = app_env.store.getPattern(entry.pattern);
        switch (pattern) {
            .assign => |assign| {
                if (std.mem.eql(u8, app_env.getIdent(assign.ident), required_name)) return entry;
            },
            else => {},
        }
    }
    return null;
}

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
pub const ComptimeRootId = enum(u32) { _ };
pub const ComptimeDependencySummaryRequestId = enum(u32) { _ };
pub const ConstGraphReificationPlanId = enum(u32) { _ };

pub const CompileTimeRootKind = enum {
    constant,
    callable_binding,
    expect,
};

pub const CompileTimeRootPayload = union(enum) {
    pending,
    const_graph: ConstGraphReificationPlanId,
    callable_result: CallableResultPlanId,
    expect,
};

pub const CompileTimeRoot = struct {
    id: ComptimeRootId,
    module_idx: u32,
    kind: CompileTimeRootKind,
    source: RootSource,
    pattern: ?CIR.Pattern.Idx,
    expr: CIR.Expr.Idx,
    checked_type: CheckedTypeId,
    dependency_summary_request: ComptimeDependencySummaryRequestId,
    payload: CompileTimeRootPayload,
};

pub const CompileTimeRootTable = struct {
    roots: []CompileTimeRoot = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypeStore,
    ) Allocator.Error!CompileTimeRootTable {
        var roots = std.ArrayList(CompileTimeRoot).empty;
        errdefer roots.deinit(allocator);

        const module_env = module.moduleEnvConst();
        for (module_env.store.sliceStatements(module_env.all_statements)) |statement_idx| {
            const stmt = module_env.store.getStatement(statement_idx);
            if (stmt != .s_expect) continue;
            try appendRoot(&roots, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .expect,
                .source = .{ .statement = statement_idx },
                .pattern = null,
                .expr = stmt.s_expect.body,
                .checked_type = try checkedTypeIdForVar(allocator, module, checked_types, ModuleEnv.varFrom(stmt.s_expect.body)),
                .payload = .expect,
            });
        }

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            if (topLevelExprIsAlreadyProcedure(def.expr.data)) continue;

            const source_ty = module.defType(def_idx);
            const is_callable = sourceTypeIsFunction(module, source_ty);
            try appendRoot(&roots, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = if (is_callable) .callable_binding else .constant,
                .source = .{ .def = def_idx },
                .pattern = def.pattern.idx,
                .expr = def.expr.idx,
                .checked_type = try checkedTypeIdForVar(allocator, module, checked_types, source_ty),
                .payload = .pending,
            });
        }

        return .{ .roots = try roots.toOwnedSlice(allocator) };
    }

    pub fn lookupIdByPattern(self: *const CompileTimeRootTable, pattern: CIR.Pattern.Idx) ?ComptimeRootId {
        for (self.roots) |root| {
            if (root.pattern != null and root.pattern.? == pattern) return root.id;
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
        pattern: ?CIR.Pattern.Idx,
        expr: CIR.Expr.Idx,
        checked_type: CheckedTypeId,
        payload: CompileTimeRootPayload,
    };

    fn appendRoot(
        roots: *std.ArrayList(CompileTimeRoot),
        allocator: Allocator,
        root: RootWithoutId,
    ) Allocator.Error!void {
        const id: ComptimeRootId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
        try roots.append(allocator, .{
            .id = id,
            .module_idx = root.module_idx,
            .kind = root.kind,
            .source = root.source,
            .pattern = root.pattern,
            .expr = root.expr,
            .checked_type = root.checked_type,
            .dependency_summary_request = @enumFromInt(@intFromEnum(id)),
            .payload = root.payload,
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

pub const FiniteCallableLeafInstance = struct {
    proc_value: canonical.ProcedureCallableRef,
};

pub const ErasedCallableLeafInstance = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    sig_key: canonical.ErasedFnSigKey,
    provenance: []const canonical.BoxBoundaryId,
    code: canonical.ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

pub const CallableLeafInstance = union(enum) {
    finite: FiniteCallableLeafInstance,
    erased_boxed: ErasedCallableLeafInstance,
};

pub const CallableLeafReificationPlan = union(enum) {
    finite: CallableResultPlanId,
    erased_boxed: CallableResultPlanId,
    already_resolved: CallableLeafInstance,
};

pub const ConstRecordFieldPlan = struct {
    field: canonical.RecordFieldLabelId,
    value: ConstGraphReificationPlanId,
};

pub const ConstTupleElemPlan = struct {
    index: u32,
    value: ConstGraphReificationPlanId,
};

pub const ConstTagPayloadPlan = struct {
    index: u32,
    value: ConstGraphReificationPlanId,
};

pub const ConstTagVariantPlan = struct {
    tag: canonical.TagLabelId,
    payloads: []const ConstTagPayloadPlan,
};

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

pub const SerializableCaptureLeafPlan = struct {
    requested_source_ty: canonical.CanonicalTypeKey,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    schema: ComptimeSchemaId,
    reification_plan: ConstGraphReificationPlanId,
};

pub const PrivateCaptureConstMode = enum {
    pure_no_callable_slots,
    general_may_contain_callable_slots,
};

pub const PrivateCaptureConstLeaf = struct {
    const_ref: ConstRef,
    const_instance: ConstInstanceRef,
    requested_source_ty: canonical.CanonicalTypeKey,
    schema: ComptimeSchemaId,
    mode: PrivateCaptureConstMode,
};

pub const CaptureRecordFieldPlan = struct {
    field: canonical.RecordFieldLabelId,
    value: CaptureSlotReificationPlanId,
};

pub const CaptureTupleElemPlan = struct {
    index: u32,
    value: CaptureSlotReificationPlanId,
};

pub const CaptureTagPayloadPlan = struct {
    index: u32,
    value: CaptureSlotReificationPlanId,
};

pub const CaptureTagVariantPlan = struct {
    tag: canonical.TagLabelId,
    payloads: []const CaptureTagPayloadPlan,
};

pub const PrivateCaptureRecordField = struct {
    field: canonical.RecordFieldLabelId,
    value: PrivateCaptureNodeId,
};

pub const PrivateCaptureTagPayload = struct {
    index: u32,
    value: PrivateCaptureNodeId,
};

pub const PrivateCaptureTagNode = struct {
    tag: canonical.TagLabelId,
    payloads: []const PrivateCaptureTagPayload,
};

pub const CaptureSlotReificationPlan = union(enum) {
    pending,
    serializable_leaf: SerializableCaptureLeafPlan,
    callable_leaf: CallableResultPlanId,
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

pub const CallableResultMemberPlan = struct {
    member: canonical.CallableSetMemberId,
    capture_slots: []const CaptureSlotReificationPlanId,
};

pub const FiniteCallableResultPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: canonical.CanonicalCallableSetKey,
    members: []const CallableResultMemberPlan,
};

pub const ErasedCaptureReificationPlan = union(enum) {
    none,
    zero_sized_typed: canonical.CanonicalExecValueTypeKey,
    whole_hidden_capture_value: ErasedCaptureSlotReificationRef,
    proc_capture_tuple: []const ErasedCaptureSlotReificationRef,
    finite_callable_set_value: CallableResultPlanId,
};

pub const ErasedCaptureSlotReificationRef = struct {
    source_ty: canonical.CanonicalTypeKey,
    plan: CaptureSlotReificationPlanId,
};

pub const ErasedCallableResultPlan = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    sig_key: canonical.ErasedFnSigKey,
    provenance: []const canonical.BoxBoundaryId,
    code: canonical.ErasedCallableCodeRef,
    capture: ErasedCaptureReificationPlan,
    result_ty: canonical.CanonicalExecValueTypeKey,
    executable_signature_payloads: ErasedPromotedProcedureExecutableSignaturePayloads,
};

pub const CallableResultPlan = union(enum) {
    finite: FiniteCallableResultPlan,
    erased: ErasedCallableResultPlan,
};

pub const FiniteCallablePromotionPlan = struct {
    result_plan: CallableResultPlanId,
    selected_member: canonical.CallableSetMemberId,
    promoted_proc: PromotedProcedureRef,
};

pub const ErasedCallablePromotionPlan = struct {
    result_plan: CallableResultPlanId,
    promoted_proc: PromotedProcedureRef,
};

pub const CallablePromotionPlan = union(enum) {
    finite: FiniteCallablePromotionPlan,
    erased: ErasedCallablePromotionPlan,
};

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
        callable_set_descriptors: *const CallableSetDescriptorStore,
    ) void {
        if (builtin.mode != .Debug) return;

        for (self.const_graphs.items) |plan| verifyConstGraphReificationPlan(self, callable_set_descriptors, plan);
        for (self.callable_results.items) |plan| verifyCallableResultPlan(self, plan);
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
            for (finite.members) |member| allocator.free(member.capture_slots);
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

fn deinitCaptureSlotReificationPlan(allocator: Allocator, plan: *CaptureSlotReificationPlan) void {
    switch (plan.*) {
        .pending,
        .serializable_leaf,
        .callable_leaf,
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
        .finite_value => {},
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
            allocator.free(finite.member_capture_slots);
            allocator.free(finite.captures);
            allocator.free(finite.params);
            allocator.free(finite.call_args);
        },
        .erased => |erased| {
            var signature = erased.executable_signature;
            deinitErasedPromotedProcedureExecutableSignature(allocator, &signature);
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
    callable_set_descriptors: *const CallableSetDescriptorStore,
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
            .already_resolved => |resolved| verifyCallableLeafInstance(store, callable_set_descriptors, resolved),
        },
        .callable_schema => {},
        .recursive_ref => |ref| verifyConstGraphRef(store, ref),
    }
}

fn verifyCallableResultPlan(store: *const CompileTimePlanStore, plan: CallableResultPlan) void {
    switch (plan) {
        .finite => |finite| {
            if (finite.members.len == 0) {
                std.debug.panic("checked artifact invariant violated: finite callable result plan has no members", .{});
            }
            for (finite.members) |member| {
                for (member.capture_slots) |capture| verifyCaptureSlotRef(store, capture);
            }
        },
        .erased => |erased| {
            if (erased.provenance.len == 0) {
                std.debug.panic("checked artifact invariant violated: erased callable result plan has no Box(T) provenance", .{});
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
    callable_set_descriptors: *const CallableSetDescriptorStore,
    leaf: CallableLeafInstance,
) void {
    switch (leaf) {
        .finite => {},
        .erased_boxed => |erased| {
            if (erased.provenance.len == 0) {
                std.debug.panic("checked artifact invariant violated: erased callable leaf has no Box(T) provenance", .{});
            }
            verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, erased.capture);
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
    for (finite.captures) |capture| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, capture);
}

fn verifyMaterializedErasedCallableValue(
    store: *const CompileTimePlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
    erased: MaterializedErasedCallableValue,
) void {
    if (erased.provenance.len == 0) {
        std.debug.panic("checked artifact invariant violated: materialized erased callable value has no Box(T) provenance", .{});
    }
    verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, erased.capture);
}

fn verifyErasedCaptureExecutableMaterializationPlan(
    store: *const CompileTimePlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
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
        .erased_callable => |erased| verifyMaterializedErasedCallableValue(store, callable_set_descriptors, erased),
        .record => |fields| for (fields) |field| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, field.value),
        .tuple => |items| for (items) |item| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, item),
        .tag_union => |tag| {
            for (tag.payloads) |payload| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, payload.value);
        },
        .list => |items| for (items) |item| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, item),
        .box => |payload| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, payload),
        .nominal => |nominal| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, nominal.backing),
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
    if (@intFromEnum(ref.payload) >= payloads.payloads.len) {
        std.debug.panic("checked artifact invariant violated: executable type payload ref is out of range", .{});
    }
}

fn verifyExecutableTypePayload(
    payloads: *const ExecutableTypePayloadStore,
    artifact_key: CheckedModuleArtifactKey,
    payload: ExecutableTypePayload,
) void {
    switch (payload) {
        .pending => std.debug.panic("checked artifact invariant violated: executable type payload was not filled", .{}),
        .primitive => {},
        .record => |fields| for (fields) |field| verifyExecutableTypePayloadRef(payloads, artifact_key, field.ty),
        .tuple => |items| for (items) |item| verifyExecutableTypePayloadRef(payloads, artifact_key, item.ty),
        .tag_union => |variants| for (variants) |variant| {
            for (variant.payloads) |tag_payload| verifyExecutableTypePayloadRef(payloads, artifact_key, tag_payload.ty);
        },
        .list => |child| verifyExecutableTypePayloadRef(payloads, artifact_key, child.ty),
        .box => |child| verifyExecutableTypePayloadRef(payloads, artifact_key, child.ty),
        .nominal => |nominal| verifyExecutableTypePayloadRef(payloads, artifact_key, nominal.backing),
        .callable_set => |callable_set| for (callable_set.members) |member| {
            if ((member.payload_ty == null) != (member.payload_ty_key == null)) {
                std.debug.panic("checked artifact invariant violated: callable-set executable payload member has mismatched payload ref/key presence", .{});
            }
            if (member.payload_ty) |payload_ty| verifyExecutableTypePayloadRef(payloads, artifact_key, payload_ty);
        },
        .erased_fn => |erased| {
            if ((erased.capture_ty == null) != (erased.capture_ty_key == null)) {
                std.debug.panic("checked artifact invariant violated: erased executable payload has mismatched capture ref/key presence", .{});
            }
            if (erased.capture_ty) |capture| verifyExecutableTypePayloadRef(payloads, artifact_key, capture);
            if (erased.sig_key.capture_ty == null and erased.capture_ty != null) {
                std.debug.panic("checked artifact invariant violated: erased executable payload has capture payload but signature has no capture", .{});
            }
            if (erased.sig_key.capture_ty != null and erased.capture_ty == null) {
                std.debug.panic("checked artifact invariant violated: erased executable payload signature has capture but payload is missing", .{});
            }
        },
        .recursive_ref => |ref| {
            if (@intFromEnum(ref) >= payloads.payloads.len) {
                std.debug.panic("checked artifact invariant violated: executable recursive type payload ref is out of range", .{});
            }
        },
    }
}

fn verifyErasedPromotedProcedureExecutableSignature(
    payloads: *const ExecutableTypePayloadStore,
    artifact_key: CheckedModuleArtifactKey,
    signature: ErasedPromotedProcedureExecutableSignature,
    erased: ErasedPromotedWrapperBodyPlan,
) void {
    if (!std.mem.eql(u8, &signature.source_fn_ty.bytes, &erased.source_fn_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable signature source type differs from wrapper source type", .{});
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
        verifyExecutableTypePayloadRef(payloads, artifact_key, param_payload.exec_ty);
    }
    verifyExecutableTypePayloadRef(payloads, artifact_key, signature.wrapper_ret);
    verifyExecutableTypePayloadRef(payloads, artifact_key, signature.erased_call_ret);
    for (signature.erased_call_args) |arg| verifyExecutableTypePayloadRef(payloads, artifact_key, arg);
    if (!std.mem.eql(u8, &signature.wrapper_ret_key.bytes, &signature.specialization_key.exec_ret_ty.bytes)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable wrapper return key differs from specialization key", .{});
    }
    if (signature.erased_call_args.len != signature.erased_call_arg_keys.len) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable erased-call arg refs/keys differ in length", .{});
    }
    if ((erased.sig_key.capture_ty == null) != (signature.hidden_capture == null)) {
        std.debug.panic("checked artifact invariant violated: erased promoted executable hidden capture presence differs from signature key", .{});
    }
    if (signature.hidden_capture) |hidden| {
        const capture_ty = erased.sig_key.capture_ty orelse unreachable;
        if (!std.mem.eql(u8, &hidden.exec_ty_key.bytes, &capture_ty.bytes)) {
            std.debug.panic("checked artifact invariant violated: erased promoted executable hidden capture key differs from signature key", .{});
        }
        verifyExecutableTypePayloadRef(payloads, artifact_key, hidden.exec_ty);
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
        .singleton_to_tag_union => |singleton| if (singleton.value_transform) |payload| verifyExecutableValueTransformRef(store, payload),
        .tag_union_to_singleton => |singleton| if (singleton.value_transform) |payload| verifyExecutableValueTransformRef(store, payload),
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
    callable_set_descriptors: *const CallableSetDescriptorStore,
    executable_type_payloads: *const ExecutableTypePayloadStore,
    executable_value_transforms: *const ExecutableValueTransformPlanStore,
    artifact_key: CheckedModuleArtifactKey,
    plan: PromotedCallableBodyPlan,
) void {
    switch (plan) {
        .pending => std.debug.panic("checked artifact invariant violated: published promoted callable body plan is pending", .{}),
        .finite => |finite| {
            if (!std.mem.eql(u8, &finite.member_proc.source_fn_ty.bytes, &finite.source_fn_ty.bytes)) {
                std.debug.panic("checked artifact invariant violated: finite promoted callable body member procedure source type differs from wrapper source type", .{});
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
            if (erased.arg_transforms.len != erased.params.len) {
                std.debug.panic("checked artifact invariant violated: erased promoted callable arg transform count differs from wrapper params", .{});
            }
            for (erased.arg_transforms) |transform| verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, transform);
            verifyPublishedExecutableValueTransformRef(executable_value_transforms, artifact_key, erased.result_transform);
            verifyErasedPromotedProcedureExecutableSignature(
                executable_type_payloads,
                artifact_key,
                erased.executable_signature,
                erased,
            );
            verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, erased.capture);
            switch (erased.hidden_capture_arg) {
                .none => {},
                .materialized_capture => |capture| verifyErasedCaptureExecutableMaterializationPlan(store, callable_set_descriptors, capture),
            }
        },
    }
}

pub const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: ConstOwner,
    template: ConstTemplateId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
};

pub const ConstOwner = union(enum) {
    top_level_binding: ConstTopLevelOwner,
    promoted_capture: PromotedCaptureId,
};

pub const ConstTopLevelOwner = struct {
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
};

pub const PromotedCaptureId = struct {
    promoted_proc: PromotedProcedureRef,
    capture_index: u32,
};

pub const TopLevelValueKind = union(enum) {
    const_ref: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
};

pub const TopLevelValueEntry = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CIR.Pattern.Idx,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    value: TopLevelValueKind,
};

pub const TopLevelValueTable = struct {
    entries: []TopLevelValueEntry = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
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
            const source_ty = module.defType(def_idx);
            const source_scheme = try canonical_type_keys.schemeFromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                source_ty,
            );
            const value: TopLevelValueKind = if (topLevelExprIsAlreadyProcedure(def.expr.data)) blk: {
                const template = templates.lookupByDef(def_idx) orelse unreachable;
                const binding = try procedure_bindings.appendDirect(
                    allocator,
                    source_scheme,
                    .{ .artifact = template.artifact, .proc_base = template.proc_base },
                    template,
                );
                break :blk .{ .procedure_binding = binding };
            } else if (sourceTypeIsFunction(module, source_ty)) blk: {
                const root_id = compile_time_roots.lookupIdByPattern(def.pattern.idx) orelse {
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
                    def.pattern.idx,
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
                def.pattern.idx,
                source_scheme,
            ) };

            try entries.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .def = def_idx,
                .pattern = def.pattern.idx,
                .source_scheme = source_scheme,
                .value = value,
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }

    pub fn lookupByPattern(self: *const TopLevelValueTable, pattern: CIR.Pattern.Idx) ?TopLevelValueEntry {
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

pub const PromotedProcedure = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    source_binding: CIR.Pattern.Idx,
};

pub const ReservedPromotedCallableWrapper = struct {
    promoted_ref: PromotedProcedureRef,
    proc_value: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    wrapper: canonical.PromotedCallableWrapperId,
    body_plan: canonical.PromotedCallableBodyPlanId,
};

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
                .promoted_callable_wrapper => {},
                else => std.debug.panic("checked artifact invariant violated: promoted procedure table row does not point at a promoted callable wrapper", .{}),
            }
        }
    }

    pub fn deinit(self: *PromotedProcedureTable, allocator: Allocator) void {
        allocator.free(self.procedures);
        self.* = .{};
    }
};

pub const ComptimeWrappedSchema = struct {
    type_name: canonical.NominalTypeKey,
    backing: ComptimeSchemaId,
    is_opaque: bool = false,
};

pub const ComptimeFieldSchema = struct {
    name: canonical.RecordFieldLabelId,
    schema: ComptimeSchemaId,
};

pub const ComptimeVariantSchema = struct {
    name: canonical.TagLabelId,
    payloads: []ComptimeSchemaId,
};

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

pub const ComptimeVariantValue = struct {
    variant_index: u32,
    payloads: []ComptimeValueId,
};

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

pub const ComptimeBinding = struct {
    pattern: CIR.Pattern.Idx,
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
};

pub const CompileTimeValueStore = struct {
    allocator: Allocator,
    schemas: std.ArrayList(ComptimeSchema),
    values: std.ArrayList(ComptimeValue),
    bindings: []ComptimeBinding = &.{},
    by_pattern: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, ComptimeBinding) = .{},

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
        pattern: CIR.Pattern.Idx,
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

    pub fn lookupBinding(self: *const CompileTimeValueStore, pattern: CIR.Pattern.Idx) ?ComptimeBinding {
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

pub const CheckedCallableBodyRef = enum(u32) { _ };
pub const CheckedConstBodyRef = enum(u32) { _ };
pub const ConstTemplateId = enum(u32) { _ };
pub const ConstInstanceId = enum(u32) { _ };
pub const CallableBindingInstanceId = enum(u32) { _ };
pub const SemanticInstantiationProcedureId = enum(u32) { _ };
pub const CallableResultPlanId = enum(u32) { _ };
pub const CallablePromotionPlanId = enum(u32) { _ };
pub const ConstReificationPlanId = ConstGraphReificationPlanId;
pub const CaptureSlotReificationPlanId = enum(u32) { _ };
pub const ErasedCaptureExecutableMaterializationNodeId = enum(u32) { _ };
pub const ComptimeDependencySummaryTemplateId = enum(u32) { _ };
pub const ComptimeDependencySummaryId = enum(u32) { _ };
pub const ComptimeValuePathKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};
pub const PromotedCallablePathKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};
pub const PrivateCapturePathKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};
pub const PrivateCaptureId = enum(u32) { _ };
pub const PrivateCaptureNodeId = enum(u32) { _ };
pub const MethodRegistryEntryRef = enum(u32) { _ };

pub const ArtifactCheckedBodyRef = struct {
    artifact: CheckedModuleArtifactKey,
    body: CheckedBodyId,
};

pub const ArtifactCheckedTypeRef = struct {
    artifact: CheckedModuleArtifactKey,
    ty: CheckedTypeId,
};

pub const ArtifactCheckedTypeSchemeRef = struct {
    artifact: CheckedModuleArtifactKey,
    scheme: CheckedTypeSchemeId,
};

pub const ArtifactCheckedCallableBodyRef = struct {
    artifact: CheckedModuleArtifactKey,
    body: CheckedCallableBodyRef,
};

pub const ArtifactCheckedConstBodyRef = struct {
    artifact: CheckedModuleArtifactKey,
    body: CheckedConstBodyRef,
};

pub const ArtifactProcedureTemplateRef = canonical.ProcedureTemplateRef;

pub const ArtifactCallableEvalTemplateRef = struct {
    artifact: CheckedModuleArtifactKey,
    template: CallableEvalTemplateId,
};

pub const ArtifactResolvedValueRefTableRef = struct {
    artifact: CheckedModuleArtifactKey,
    table: ResolvedValueRefTableRef,
};

pub const ArtifactStaticDispatchPlanTableRef = struct {
    artifact: CheckedModuleArtifactKey,
    table: StaticDispatchPlanTableRef,
};

pub const ArtifactNestedProcSiteTableRef = struct {
    artifact: CheckedModuleArtifactKey,
    table: NestedProcSiteTableRef,
};

pub const ArtifactCallableResultPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: CallableResultPlanId,
};

pub const ArtifactCallablePromotionPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: CallablePromotionPlanId,
};

pub const ArtifactConstGraphReificationPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: ConstReificationPlanId,
};

pub const ArtifactComptimeDependencySummaryTemplateRef = struct {
    artifact: CheckedModuleArtifactKey,
    template: ComptimeDependencySummaryTemplateId,
};

pub const ArtifactPrivateCaptureNodeRef = struct {
    artifact: CheckedModuleArtifactKey,
    node: PrivateCaptureNodeId,
};

pub const ArtifactPromotedCallableWrapperRef = struct {
    artifact: CheckedModuleArtifactKey,
    wrapper: canonical.PromotedCallableWrapperId,
};

pub const ArtifactPromotedCallableBodyPlanRef = struct {
    artifact: CheckedModuleArtifactKey,
    plan: canonical.PromotedCallableBodyPlanId,
};

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
    dependency_summary_templates: []const ArtifactComptimeDependencySummaryTemplateRef = &.{},
    nested_proc_sites: []const ArtifactNestedProcSiteTableRef = &.{},
    resolved_value_refs: []const ArtifactResolvedValueRefTableRef = &.{},
    static_dispatch_plans: []const ArtifactStaticDispatchPlanTableRef = &.{},
    method_registry_entries: []const MethodRegistryEntryRef = &.{},
    interface_capabilities: ModuleInterfaceCapabilities = .{
        .exported_def_count = 0,
        .method_count = 0,
        .provides_count = 0,
        .requires_count = 0,
    },
};

pub const ExportedProcedureTemplate = struct {
    export_name: ?canonical.ExportNameId,
    def: CIR.Def.Idx,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    template: canonical.ProcedureTemplateRef,
    template_data: CheckedProcedureTemplate,
    template_closure: ImportedTemplateClosureView = .{},
};

pub const ExportedProcedureTemplateView = struct {
    templates: []const ExportedProcedureTemplate = &.{},
};

pub const ExportedProcedureTemplateTable = struct {
    templates: []ExportedProcedureTemplate = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        artifact_key: CheckedModuleArtifactKey,
        checked_types: *const CheckedTypeStore,
        checked_templates: *const CheckedProcedureTemplateTable,
    ) Allocator.Error!ExportedProcedureTemplateTable {
        var templates = std.ArrayList(ExportedProcedureTemplate).empty;
        errdefer {
            for (templates.items) |*template| deinitImportedTemplateClosure(allocator, &template.template_closure);
            templates.deinit(allocator);
        }

        const module_env = module.moduleEnvConst();
        for (module_env.store.sliceDefs(module_env.exports)) |def_idx| {
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
    template_ref: canonical.ProcedureTemplateRef,
    template: CheckedProcedureTemplate,
) Allocator.Error!ImportedTemplateClosureView {
    const checked_bodies: []const ArtifactCheckedBodyRef = switch (template.body) {
        .checked_body => |body| try singleton(allocator, ArtifactCheckedBodyRef, .{
            .artifact = artifact_key,
            .body = body,
        }),
        .promoted_callable_wrapper,
        .hosted_wrapper,
        .intrinsic_wrapper,
        .entry_wrapper,
        => &.{},
    };
    errdefer freeConstSlice(allocator, checked_bodies);

    const checked_type_roots = try singleton(allocator, ArtifactCheckedTypeRef, .{
        .artifact = artifact_key,
        .ty = template.checked_fn_root,
    });
    errdefer freeConstSlice(allocator, checked_type_roots);

    const scheme = checked_types.schemeForKey(template.checked_fn_scheme) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: exported template references missing checked type scheme", .{});
        }
        unreachable;
    };
    const checked_type_schemes = try singleton(allocator, ArtifactCheckedTypeSchemeRef, .{
        .artifact = artifact_key,
        .scheme = scheme.id,
    });
    errdefer freeConstSlice(allocator, checked_type_schemes);

    const checked_procedure_templates = try singleton(allocator, ArtifactProcedureTemplateRef, template_ref);
    errdefer freeConstSlice(allocator, checked_procedure_templates);

    const nested_proc_sites: []const ArtifactNestedProcSiteTableRef = if (template.nested_proc_sites.len == 0)
        &.{}
    else
        try singleton(allocator, ArtifactNestedProcSiteTableRef, .{
            .artifact = artifact_key,
            .table = template.nested_proc_sites,
        });
    errdefer freeConstSlice(allocator, nested_proc_sites);

    const resolved_value_refs: []const ArtifactResolvedValueRefTableRef = if (template.resolved_value_refs.len == 0)
        &.{}
    else
        try singleton(allocator, ArtifactResolvedValueRefTableRef, .{
            .artifact = artifact_key,
            .table = template.resolved_value_refs,
        });
    errdefer freeConstSlice(allocator, resolved_value_refs);

    const static_dispatch_plans: []const ArtifactStaticDispatchPlanTableRef = if (template.static_dispatch_plans.len == 0)
        &.{}
    else
        try singleton(allocator, ArtifactStaticDispatchPlanTableRef, .{
            .artifact = artifact_key,
            .table = template.static_dispatch_plans,
        });
    errdefer freeConstSlice(allocator, static_dispatch_plans);

    return .{
        .checked_bodies = checked_bodies,
        .checked_type_roots = checked_type_roots,
        .checked_type_schemes = checked_type_schemes,
        .checked_procedure_templates = checked_procedure_templates,
        .nested_proc_sites = nested_proc_sites,
        .resolved_value_refs = resolved_value_refs,
        .static_dispatch_plans = static_dispatch_plans,
    };
}

fn singleton(
    allocator: Allocator,
    comptime T: type,
    value: T,
) Allocator.Error![]const T {
    const out = try allocator.alloc(T, 1);
    out[0] = value;
    return out;
}

fn freeConstSlice(allocator: Allocator, slice: anytype) void {
    if (slice.len > 0) allocator.free(slice);
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
    freeConstSlice(allocator, closure.dependency_summary_templates);
    freeConstSlice(allocator, closure.nested_proc_sites);
    freeConstSlice(allocator, closure.resolved_value_refs);
    freeConstSlice(allocator, closure.static_dispatch_plans);
    freeConstSlice(allocator, closure.method_registry_entries);
    closure.* = .{};
}

pub const ImportedProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateId,
};

pub const ImportedProcedureBindingView = struct {
    binding: ImportedProcedureBindingRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    body: ImportedProcedureBindingBody,
    template_closure: ImportedTemplateClosureView = .{},
};

pub const ExportedProcedureBindingView = struct {
    bindings: []const ImportedProcedureBindingView = &.{},
};

pub const ExportedProcedureBindingTable = struct {
    bindings: []ImportedProcedureBindingView = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypeStore,
        checked_templates: *const CheckedProcedureTemplateTable,
        top_level_values: *const TopLevelValueTable,
        procedure_bindings: *const TopLevelProcedureBindingTable,
        callable_eval_templates: *const CallableEvalTemplateTable,
        artifact_key: CheckedModuleArtifactKey,
    ) Allocator.Error!ExportedProcedureBindingTable {
        var bindings = std.ArrayList(ImportedProcedureBindingView).empty;
        errdefer {
            for (bindings.items) |*binding| deinitImportedTemplateClosure(allocator, &binding.template_closure);
            bindings.deinit(allocator);
        }

        const module_env = module.moduleEnvConst();
        for (module_env.store.sliceDefs(module_env.exports)) |def_idx| {
            const def = module.def(def_idx);
            const top_level = top_level_values.lookupByPattern(def.pattern.idx) orelse continue;
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
                callable_eval_templates,
                binding.body,
            );
            errdefer deinitImportedTemplateClosure(allocator, &template_closure);

            try bindings.append(allocator, .{
                .binding = .{
                    .artifact = artifact_key,
                    .module_idx = module.moduleIndex(),
                    .def = def_idx,
                    .pattern = def.pattern.idx,
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
    callable_eval_templates: *const CallableEvalTemplateTable,
    body: ProcedureBindingBody,
) Allocator.Error!ImportedTemplateClosureView {
    return switch (body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template_ref| buildImportedTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
                template_ref,
                checked_templates.get(template_ref.template),
            ),
            .synthetic => |synthetic| buildImportedTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
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
            const callable_eval_template = try singleton(allocator, ArtifactCallableEvalTemplateRef, .{
                .artifact = artifact_key,
                .template = template_id,
            });
            errdefer freeConstSlice(allocator, callable_eval_template);

            const checked_type_roots = try singleton(allocator, ArtifactCheckedTypeRef, .{
                .artifact = artifact_key,
                .ty = template.checked_fn_root,
            });
            errdefer freeConstSlice(allocator, checked_type_roots);

            const scheme = checked_types.schemeForKey(template.source_scheme) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("checked artifact invariant violated: callable eval template references missing checked type scheme", .{});
                }
                unreachable;
            };
            const checked_type_schemes = try singleton(allocator, ArtifactCheckedTypeSchemeRef, .{
                .artifact = artifact_key,
                .scheme = scheme.id,
            });
            errdefer freeConstSlice(allocator, checked_type_schemes);

            break :blk .{
                .checked_type_roots = checked_type_roots,
                .checked_type_schemes = checked_type_schemes,
                .callable_eval_templates = callable_eval_template,
            };
        },
    };
}

pub const ConstEvalTemplate = struct {
    body: CheckedConstBodyRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    resolved_value_refs: ResolvedValueRefTableRef = .{},
    static_dispatch_plans: StaticDispatchPlanTableRef = .{},
    nested_proc_sites: NestedProcSiteTableRef = .{},
    dependency_template: ComptimeDependencySummaryTemplateId,
};

pub const ConstValueGraphTemplate = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
};

pub const ConstTemplateState = union(enum) {
    reserved,
    eval_template: ConstEvalTemplate,
    value_graph_template: ConstValueGraphTemplate,
};

pub const ConstTemplate = struct {
    id: ConstTemplateId,
    owner: ConstOwner,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    state: ConstTemplateState,
};

pub const ConstTemplateTable = struct {
    templates: std.ArrayList(ConstTemplate) = .empty,

    pub fn reserveTopLevel(
        self: *ConstTemplateTable,
        allocator: Allocator,
        artifact_key: CheckedModuleArtifactKey,
        module_idx: u32,
        pattern: CIR.Pattern.Idx,
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

pub const ImportedConstTemplateView = struct {
    module_idx: u32,
    def: CIR.Def.Idx,
    pattern: CIR.Pattern.Idx,
    const_ref: ConstRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    template: ConstTemplate,
    template_closure: ImportedTemplateClosureView = .{},
};

pub const ExportedConstTemplateView = struct {
    templates: []const ImportedConstTemplateView = &.{},
};

pub const ExportedConstTemplateTable = struct {
    templates: []ImportedConstTemplateView = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        top_level_values: *const TopLevelValueTable,
        const_templates: *const ConstTemplateTable,
    ) Allocator.Error!ExportedConstTemplateTable {
        var templates = std.ArrayList(ImportedConstTemplateView).empty;
        errdefer templates.deinit(allocator);

        const module_env = module.moduleEnvConst();
        for (module_env.store.sliceDefs(module_env.exports)) |def_idx| {
            const def = module.def(def_idx);
            const top_level = top_level_values.lookupByPattern(def.pattern.idx) orelse continue;
            const const_ref = switch (top_level.value) {
                .const_ref => |ref| ref,
                .procedure_binding => continue,
            };
            const template = const_templates.get(const_ref);
            try templates.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .def = def_idx,
                .pattern = def.pattern.idx,
                .const_ref = const_ref,
                .source_scheme = top_level.source_scheme,
                .template = template,
            });
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

pub const ConstInstantiationStoreView = struct {
    owner: CheckedModuleArtifactKey = .{},
    instances: []const ConstInstantiationRecord = &.{},
};

pub const CallableBindingInstantiationStoreView = struct {
    owner: CheckedModuleArtifactKey = .{},
    instances: []const CallableBindingInstantiationRecord = &.{},
};

pub const SemanticInstantiationProcedureTableView = struct {
    owner: CheckedModuleArtifactKey = .{},
    procedures: []const SemanticInstantiationProcedureRecord = &.{},
};

pub const ConstInstantiationKey = struct {
    const_ref: ConstRef,
    requested_source_ty: canonical.CanonicalTypeKey,
};

pub const ConstInstanceRef = struct {
    owner: CheckedModuleArtifactKey,
    key: ConstInstantiationKey,
    instance: ConstInstanceId,
};

pub const ConstInstance = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    dependency_summary: ?ComptimeDependencySummaryId = null,
    reification_plan: ?ConstReificationPlanId = null,
    generated_procedures: []const SemanticInstantiationProcedureId = &.{},
};

pub const ConstInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: ConstInstance,
};

pub const ConstInstantiationRecord = struct {
    id: ConstInstanceId,
    key: ConstInstantiationKey,
    state: ConstInstantiationState,
};

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

    pub fn reserve(
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

    pub fn get(self: *const ConstInstantiationStore, ref: ConstInstanceRef) ConstInstance {
        const record = self.recordForConstRef(ref);
        return switch (record.state) {
            .evaluated => |instance| instance,
            .reserved, .evaluating => checkedArtifactInvariant("constant instance was consumed before it was sealed", .{}),
        };
    }

    pub fn verifySealed(self: *const ConstInstantiationStore) void {
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
                    if (instance.reification_plan == null) {
                        std.debug.panic(
                            "checked artifact invariant violated: constant instance {d} has no reification plan",
                            .{i},
                        );
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

pub const CallableBindingInstantiationKey = struct {
    binding: ProcedureBindingRef,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
};

pub const CallableBindingInstanceRef = struct {
    owner: CheckedModuleArtifactKey,
    key: CallableBindingInstantiationKey,
    instance: CallableBindingInstanceId,
};

pub const CallablePromotionOutput = union(enum) {
    existing_procedure: canonical.ProcedureCallableRef,
    promoted_procedure: PromotedProcedureRef,
};

pub const CallableBindingInstance = struct {
    key: CallableBindingInstantiationKey,
    dependency_summary: ComptimeDependencySummaryId,
    executable_root: ComptimeRootId,
    result_plan: CallableResultPlanId,
    promotion_plan: ?CallablePromotionPlanId = null,
    promotion_output: CallablePromotionOutput,
    proc_value: canonical.ProcedureCallableRef,
};

pub const CallableBindingInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: CallableBindingInstance,
};

pub const CallableBindingInstantiationRecord = struct {
    id: CallableBindingInstanceId,
    key: CallableBindingInstantiationKey,
    state: CallableBindingInstantiationState,
};

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

    pub fn reserve(
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

    pub fn get(self: *const CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) CallableBindingInstance {
        const record = self.recordForConstRef(ref);
        return switch (record.state) {
            .evaluated => |instance| instance,
            .reserved, .evaluating => checkedArtifactInvariant("callable binding instance was consumed before it was sealed", .{}),
        };
    }

    pub fn verifySealed(
        self: *const CallableBindingInstantiationStore,
        plans: *const CompileTimePlanStore,
        roots: *const CompileTimeRootTable,
        promoted_procedures: *const PromotedProcedureTable,
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
                    plans,
                    roots,
                    promoted_procedures,
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
        self.by_key.deinit(allocator);
        self.instances.deinit(allocator);
        self.* = .{};
    }
};

fn verifyCallableBindingInstance(
    index: usize,
    key: CallableBindingInstantiationKey,
    instance: CallableBindingInstance,
    plans: *const CompileTimePlanStore,
    roots: *const CompileTimeRootTable,
    promoted_procedures: *const PromotedProcedureTable,
) void {
    if (!callableBindingInstantiationKeyEql(instance.key, key)) {
        std.debug.panic("checked artifact invariant violated: callable binding instance {d} payload key does not match row key", .{index});
    }
    verifyCallableResultRef(plans, instance.result_plan);

    const root_index = @intFromEnum(instance.executable_root);
    if (root_index >= roots.roots.len) {
        std.debug.panic("checked artifact invariant violated: callable binding instance {d} executable root is out of range", .{index});
    }
    if (roots.roots[root_index].kind != .callable_binding) {
        std.debug.panic("checked artifact invariant violated: callable binding instance {d} executable root is not callable", .{index});
    }

    switch (instance.promotion_output) {
        .existing_procedure => |proc| {
            if (instance.promotion_plan != null) {
                std.debug.panic("checked artifact invariant violated: existing callable binding instance {d} unexpectedly has a promotion plan", .{index});
            }
            if (!canonical.procedureCallableRefEql(instance.proc_value, proc)) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} final proc value differs from existing procedure output", .{index});
            }
        },
        .promoted_procedure => |promoted| {
            const plan = instance.promotion_plan orelse {
                std.debug.panic("checked artifact invariant violated: promoted callable binding instance {d} has no promotion plan", .{index});
            };
            verifyCallablePromotionRef(plans, plan);
            const promoted_record = promoted_procedures.get(promoted) orelse {
                std.debug.panic("checked artifact invariant violated: promoted callable binding instance {d} references a missing promoted procedure", .{index});
            };
            const expected = canonical.ProcedureCallableRef{
                .template = .{ .synthetic = .{ .template = promoted_record.template } },
                .source_fn_ty = instance.key.requested_source_fn_ty,
            };
            if (!canonical.procedureCallableRefEql(instance.proc_value, expected)) {
                std.debug.panic("checked artifact invariant violated: callable binding instance {d} final proc value differs from promoted procedure output", .{index});
            }
        },
    }
}

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

pub const SemanticInstantiationProcedure = struct {
    template: canonical.CallableProcedureTemplateRef,
    proc_value: canonical.ProcedureValueRef,
    promoted: ?PromotedProcedureRef = null,
};

pub const SemanticInstantiationProcedureState = union(enum) {
    reserved,
    sealed: SemanticInstantiationProcedure,
};

pub const SemanticInstantiationProcedureRecord = struct {
    id: SemanticInstantiationProcedureId,
    key: SemanticInstantiationProcedureKey,
    state: SemanticInstantiationProcedureState,
};

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

    pub fn verifySealed(self: *const SemanticInstantiationProcedureTable) void {
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
                .sealed => {},
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

fn checkedArtifactInvariant(comptime message: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("checked artifact invariant violated: " ++ message, args);
    }
    unreachable;
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

fn hashMonoSpecializationKey(hasher: *std.crypto.hash.sha2.Sha256, key: canonical.MonoSpecializationKey) void {
    hashProcedureTemplateRef(hasher, key.template);
    hashCanonicalTypeKey(hasher, key.requested_mono_fn_ty);
}

fn hashCallableProcedureTemplateRef(hasher: *std.crypto.hash.sha2.Sha256, ref: canonical.CallableProcedureTemplateRef) void {
    switch (ref) {
        .checked => |checked| {
            hasher.update(&[_]u8{0});
            hashProcedureTemplateRef(hasher, checked);
        },
        .lifted => |lifted| {
            hasher.update(&[_]u8{1});
            hashMonoSpecializationKey(hasher, lifted.owner_mono_specialization);
            hashEnumValue(hasher, lifted.site);
        },
        .synthetic => |synthetic| {
            hasher.update(&[_]u8{2});
            hashProcedureTemplateRef(hasher, synthetic.template);
        },
    }
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
    hashU32(hasher, ref.module_idx);
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

fn constInstantiationKeyEql(a: ConstInstantiationKey, b: ConstInstantiationKey) bool {
    return constRefEql(a.const_ref, b.const_ref) and
        std.mem.eql(u8, &a.requested_source_ty.bytes, &b.requested_source_ty.bytes);
}

fn importedProcedureBindingRefEql(a: ImportedProcedureBindingRef, b: ImportedProcedureBindingRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        a.module_idx == b.module_idx and
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
    promoted_callable_wrappers: PromotedCallableWrapperTable = .{},
    promoted_callable_body_plans: PromotedCallableBodyPlanTable = .{},
    executable_type_payloads: ExecutableTypePayloadStore = .{},
    executable_value_transforms: ExecutableValueTransformPlanStore = .{},
    callable_set_descriptors: CallableSetDescriptorStore = .{},
    top_level_procedure_bindings: TopLevelProcedureBindingTable,
    callable_eval_templates: CallableEvalTemplateTable = .{},
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_declarations: PlatformRequiredDeclarationTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    compile_time_roots: CompileTimeRootTable,
    top_level_values: TopLevelValueTable,
    comptime_plans: CompileTimePlanStore = .{},
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
        source_binding: CIR.Pattern.Idx,
        checked_fn_root: CheckedTypeId,
        checked_fn_scheme: canonical.CanonicalTypeSchemeKey,
        body_plan: PromotedCallableBodyPlan,
    ) Allocator.Error!PromotedProcedureRef {
        const reserved = try self.reservePromotedCallableWrapper(
            allocator,
            source_binding,
            checked_fn_root,
            checked_fn_scheme,
        );
        self.promoted_callable_body_plans.fill(reserved.body_plan, body_plan);
        try self.publishPromotedCallableWrapper(allocator, reserved);
        return reserved.promoted_ref;
    }

    pub fn reservePromotedCallableWrapper(
        self: *CheckedModuleArtifact,
        allocator: Allocator,
        source_binding: CIR.Pattern.Idx,
        checked_fn_root: CheckedTypeId,
        checked_fn_scheme: canonical.CanonicalTypeSchemeKey,
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

        const appended_wrapper = try self.promoted_callable_wrappers.append(allocator, .{
            .id = wrapper_id,
            .promoted_proc = proc_value,
            .proc_base_key = proc_base,
            .callable_node = @enumFromInt(@intFromEnum(wrapper_id)),
            .source_binding = source_binding,
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
        });
        if (!promotedProcedureRefEql(published, reserved.promoted_ref)) {
            checkedArtifactInvariant("published promoted procedure ref differed from reserved ref", .{});
        }
    }

    pub fn deinit(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.comptime_values.deinit(allocator);
        self.semantic_instantiation_procedures.deinit(allocator);
        self.callable_binding_instances.deinit(allocator);
        self.const_instances.deinit(allocator);
        self.const_templates.deinit(allocator);
        self.promoted_procedures.deinit(allocator);
        self.comptime_plans.deinit(allocator);
        self.top_level_values.deinit(allocator);
        self.compile_time_roots.deinit(allocator);
        self.platform_required_bindings.deinit(allocator);
        self.platform_required_declarations.deinit(allocator);
        self.hosted_procs.deinit(allocator);
        self.root_requests.deinit(allocator);
        self.callable_eval_templates.deinit(allocator);
        self.top_level_procedure_bindings.deinit(allocator);
        self.callable_set_descriptors.deinit(allocator);
        self.executable_value_transforms.deinit(allocator);
        self.executable_type_payloads.deinit(allocator);
        self.promoted_callable_body_plans.deinit(allocator);
        self.promoted_callable_wrappers.deinit(allocator);
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
        self.checked_bodies.deinit(allocator);
        self.checked_types.deinit(allocator);
        self.exports.deinit(allocator);
        allocator.free(self.direct_import_artifact_keys);
        self.checking_context_identity.deinit(allocator);
        self.canonical_names.deinit();
        self.module_env.deinit();
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
            switch (root.kind) {
                .constant,
                .callable_binding,
                => switch (root.payload) {
                    .pending => {},
                    else => std.debug.panic("checked artifact invariant violated: compile-time root payload was filled before checking finalization completed", .{}),
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
            switch (root.payload) {
                .pending => std.debug.panic("checked artifact invariant violated: published compile-time root has pending payload", .{}),
                else => verifyCompileTimeRootPayloadMatchesKind(root.kind, root.payload),
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
                .hosted_wrapper,
                .intrinsic_wrapper,
                => {},
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
            for (exported.template_closure.checked_bodies) |body_ref| {
                std.debug.assert(std.mem.eql(u8, &body_ref.artifact.bytes, &self.key.bytes));
                std.debug.assert(@intFromEnum(body_ref.body) < self.checked_bodies.bodies.len);
            }
            for (exported.template_closure.checked_type_roots) |type_ref| {
                std.debug.assert(std.mem.eql(u8, &type_ref.artifact.bytes, &self.key.bytes));
                std.debug.assert(@intFromEnum(type_ref.ty) < self.checked_types.roots.len);
            }
            for (exported.template_closure.checked_type_schemes) |scheme_ref| {
                std.debug.assert(std.mem.eql(u8, &scheme_ref.artifact.bytes, &self.key.bytes));
                std.debug.assert(@intFromEnum(scheme_ref.scheme) < self.checked_types.schemes.len);
            }
        }

        for (self.exported_procedure_bindings.bindings) |exported| {
            std.debug.assert(std.mem.eql(u8, &exported.binding.artifact.bytes, &self.key.bytes));
            switch (exported.body) {
                .direct_template => {
                    std.debug.assert(exported.template_closure.checked_procedure_templates.len > 0);
                    std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
                },
                .callable_eval_template => |template_id| {
                    std.debug.assert(@intFromEnum(template_id) < self.callable_eval_templates.templates.len);
                    std.debug.assert(exported.template_closure.callable_eval_templates.len > 0);
                    std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
                },
            }
        }

        for (self.platform_required_declarations.declarations, 0..) |declaration, i| {
            std.debug.assert(@intFromEnum(declaration.id) == i);
            std.debug.assert(declaration.requires_idx == i);
            std.debug.assert(declaration.module_idx == self.module_identity.module_idx);
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
            verifyPlatformRequiredValueUse(binding);
        }

        for (self.callable_eval_templates.templates, 0..) |template, i| {
            std.debug.assert(@intFromEnum(template.id) == i);
            std.debug.assert(template.module_idx == self.module_identity.module_idx);
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

        self.const_templates.verifySealed();
        self.executable_type_payloads.verifyPublished(self.key);
        self.executable_value_transforms.verifyPublished(&self.executable_type_payloads, self.key);
        self.callable_set_descriptors.verifyPublished();
        self.promoted_callable_body_plans.verifyPublished(
            &self.comptime_plans,
            &self.callable_set_descriptors,
            &self.executable_type_payloads,
            &self.executable_value_transforms,
            self.key,
        );
        self.promoted_callable_wrappers.verifyPublished(
            self.key,
            &self.checked_types,
            &self.promoted_callable_body_plans,
        );
        self.promoted_procedures.verifyPublished(self.key, &self.checked_procedure_templates);
        self.comptime_plans.verifySealed(&self.callable_set_descriptors);
        self.comptime_values.verifySealed();
        self.const_instances.verifySealed();
        self.callable_binding_instances.verifySealed(
            &self.comptime_plans,
            &self.compile_time_roots,
            &self.promoted_procedures,
        );
        self.semantic_instantiation_procedures.verifySealed();

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
                &const_use.const_ref.artifact.bytes,
                &binding.app_value.artifact.bytes,
            ));
            const owner = constRefTopLevelOwner(const_use.const_ref) orelse {
                std.debug.panic("checked artifact invariant violated: platform-required const use referenced a non-top-level ConstRef", .{});
            };
            std.debug.assert(owner.pattern == binding.app_value.pattern);
        },
        .procedure_value => |proc_use| switch (proc_use.binding) {
            .platform_required => |required| {
                std.debug.assert(std.mem.eql(
                    u8,
                    &required.artifact.bytes,
                    &binding.app_value.artifact.bytes,
                ));
                std.debug.assert(required.app_value.pattern == binding.app_value.pattern);
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

pub const ImportedModuleView = struct {
    key: CheckedModuleArtifactKey,
    canonical_names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    exports: ExportTableView,
    checked_types: CheckedTypeStoreView,
    checked_bodies: CheckedBodyStoreView,
    resolved_value_refs: *const ResolvedValueRefTable,
    nested_proc_sites: *const NestedProcSiteTable,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    hosted_procs: *const HostedProcTable,
    promoted_callable_wrappers: *const PromotedCallableWrapperTable,
    promoted_callable_body_plans: *const PromotedCallableBodyPlanTable,
    executable_type_payloads: *const ExecutableTypePayloadStore,
    executable_value_transforms: *const ExecutableValueTransformPlanStore,
    callable_set_descriptors: *const CallableSetDescriptorStore,
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
    const_instances: ConstInstantiationStoreView,
    callable_binding_instances: CallableBindingInstantiationStoreView,
    semantic_instantiation_procedures: SemanticInstantiationProcedureTableView,
};

pub const LoweringModuleView = struct {
    artifact: *const CheckedModuleArtifact,
    roots: *const RootRequestTable,
    relation_artifacts: []const ImportedModuleView = &.{},
};

pub fn importedView(artifact: *const CheckedModuleArtifact) ImportedModuleView {
    return .{
        .key = artifact.key,
        .canonical_names = &artifact.canonical_names,
        .module_identity = artifact.module_identity,
        .exports = artifact.exports.view(),
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .resolved_value_refs = &artifact.resolved_value_refs,
        .nested_proc_sites = &artifact.nested_proc_sites,
        .static_dispatch_plans = &artifact.static_dispatch_plans,
        .hosted_procs = &artifact.hosted_procs,
        .promoted_callable_wrappers = &artifact.promoted_callable_wrappers,
        .promoted_callable_body_plans = &artifact.promoted_callable_body_plans,
        .executable_type_payloads = &artifact.executable_type_payloads,
        .executable_value_transforms = &artifact.executable_value_transforms,
        .callable_set_descriptors = &artifact.callable_set_descriptors,
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
        .const_instances = artifact.const_instances.view(),
        .callable_binding_instances = artifact.callable_binding_instances.view(),
        .semantic_instantiation_procedures = artifact.semantic_instantiation_procedures.view(),
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

pub fn loweringView(artifact: *const CheckedModuleArtifact) LoweringModuleView {
    return .{
        .artifact = artifact,
        .roots = &artifact.root_requests,
        .relation_artifacts = &.{},
    };
}

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

    const exports = try allocator.dupe(CIR.Def.Idx, module_env.store.sliceDefs(module_env.exports));
    errdefer allocator.free(exports);

    const provides = try allocator.dupe(ModuleEnv.ProvidesEntry, module_env.provides_entries.items.items);
    errdefer allocator.free(provides);

    const requires = try allocator.dupe(ModuleEnv.RequiredType, module_env.requires_types.items.items);
    errdefer allocator.free(requires);

    const owner_artifact = artifactRef(artifact_key);

    var checked_types = try CheckedTypeStore.fromModule(allocator, module, &canonical_names);
    errdefer checked_types.deinit(allocator);

    var checked_bodies = try CheckedBodyStore.fromModule(allocator, module, &canonical_names, &checked_types);
    errdefer checked_bodies.deinit(allocator);

    var checked_procedure_templates = try CheckedProcedureTemplateTable.fromModule(
        allocator,
        module,
        &canonical_names,
        owner_artifact,
        &checked_types,
        &checked_bodies,
    );
    errdefer checked_procedure_templates.deinit(allocator);
    const template_lookup = checked_procedure_templates.asLookup(module_idx);

    var method_registry = try static_dispatch.MethodRegistry.fromModule(allocator, module, &canonical_names, &template_lookup, &checked_types);
    errdefer method_registry.deinit(allocator);

    var static_dispatch_plans = try static_dispatch.StaticDispatchPlanTable.fromModule(allocator, module, &canonical_names, &checked_types, &checked_bodies);
    errdefer static_dispatch_plans.deinit(allocator);
    checked_bodies.attachStaticDispatchPlans(&static_dispatch_plans);

    var hosted_procs = try HostedProcTable.fromModule(allocator, module, &canonical_names, &checked_procedure_templates);
    errdefer hosted_procs.deinit(allocator);

    var platform_required_bindings = try PlatformRequiredBindingTable.fromRelation(
        allocator,
        module,
        module_identity,
        &canonical_names,
        &platform_required_declarations,
        inputs.platform_app_relation,
    );
    errdefer platform_required_bindings.deinit(allocator);

    var compile_time_roots = try CompileTimeRootTable.fromModule(allocator, module, &checked_types);
    errdefer compile_time_roots.deinit(allocator);

    var entry_wrappers = EntryWrapperTable{};
    errdefer entry_wrappers.deinit(allocator);
    try checked_procedure_templates.appendEntryWrappersForRoots(
        allocator,
        module,
        &canonical_names,
        owner_artifact,
        &checked_types,
        &entry_wrappers,
        &compile_time_roots,
    );

    var root_requests = try RootRequestTable.fromModule(
        allocator,
        module,
        &checked_types,
        &compile_time_roots,
        &entry_wrappers,
        &platform_required_bindings,
        inputs.explicit_roots,
    );
    errdefer root_requests.deinit(allocator);

    var comptime_values = CompileTimeValueStore.init(allocator);
    errdefer comptime_values.deinit(allocator);

    var const_instances = ConstInstantiationStore.init(artifact_key);
    errdefer const_instances.deinit(allocator);

    var callable_binding_instances = CallableBindingInstantiationStore.init(artifact_key);
    errdefer callable_binding_instances.deinit(allocator);

    var semantic_instantiation_procedures = SemanticInstantiationProcedureTable.init(artifact_key);
    errdefer semantic_instantiation_procedures.deinit(allocator);

    var top_level_procedure_bindings = TopLevelProcedureBindingTable.initEmpty();
    errdefer top_level_procedure_bindings.deinit(allocator);

    var callable_eval_templates = CallableEvalTemplateTable{};
    errdefer callable_eval_templates.deinit(allocator);

    var const_templates = ConstTemplateTable{};
    errdefer const_templates.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(
        allocator,
        module,
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
        inputs.imports,
        &checked_procedure_templates,
        &hosted_procs,
        &platform_required_declarations,
        &platform_required_bindings,
        &top_level_values,
        &checked_types,
        &checked_bodies,
    );
    errdefer resolved_value_refs.deinit(allocator);
    checked_bodies.attachResolvedValueRefs(&resolved_value_refs);

    try sealCheckedProcedureTemplateRefs(
        allocator,
        &checked_bodies,
        &entry_wrappers,
        &checked_procedure_templates,
        &static_dispatch_plans,
        &resolved_value_refs,
    );

    var nested_proc_sites = try NestedProcSiteTable.fromTemplates(allocator, &checked_bodies, &entry_wrappers, &checked_procedure_templates);
    errdefer nested_proc_sites.deinit(allocator);

    var exported_procedure_templates = try ExportedProcedureTemplateTable.fromModule(
        allocator,
        module,
        &canonical_names,
        artifact_key,
        &checked_types,
        &checked_procedure_templates,
    );
    errdefer exported_procedure_templates.deinit(allocator);

    var exported_procedure_bindings = try ExportedProcedureBindingTable.fromModule(
        allocator,
        module,
        &checked_types,
        &checked_procedure_templates,
        &top_level_values,
        &top_level_procedure_bindings,
        &callable_eval_templates,
        artifact_key,
    );
    errdefer exported_procedure_bindings.deinit(allocator);

    var exported_const_templates = try ExportedConstTemplateTable.fromModule(
        allocator,
        module,
        &top_level_values,
        &const_templates,
    );
    errdefer exported_const_templates.deinit(allocator);

    var artifact = CheckedModuleArtifact{
        .key = artifact_key,
        .canonical_names = canonical_names,
        .module_identity = module_identity,
        .checking_context_identity = checking_context_identity,
        .direct_import_artifact_keys = direct_import_artifact_keys,
        .module_env = inputs.module_env_storage,
        .exports = .{ .defs = exports },
        .checked_types = checked_types,
        .checked_bodies = checked_bodies,
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
        .promoted_callable_wrappers = .{},
        .promoted_callable_body_plans = .{},
        .top_level_procedure_bindings = top_level_procedure_bindings,
        .callable_eval_templates = callable_eval_templates,
        .root_requests = root_requests,
        .hosted_procs = hosted_procs,
        .platform_required_declarations = platform_required_declarations,
        .platform_required_bindings = platform_required_bindings,
        .interface_capabilities = ModuleInterfaceCapabilities.fromModule(module),
        .compile_time_roots = compile_time_roots,
        .top_level_values = top_level_values,
        .promoted_procedures = .{},
        .const_templates = const_templates,
        .comptime_values = comptime_values,
        .const_instances = const_instances,
        .callable_binding_instances = callable_binding_instances,
        .semantic_instantiation_procedures = semantic_instantiation_procedures,
    };
    try inputs.compile_time_finalizer.run(allocator, &artifact, inputs.imports);
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
        .top_level_procedure_bindings = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_declarations = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{
            .exported_def_count = 0,
            .method_count = 0,
            .provides_count = 0,
            .requires_count = 0,
        },
        .compile_time_roots = .{},
        .top_level_values = .{},
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
        artifact.comptime_values.deinit(std.testing.allocator);
        artifact.canonical_names.deinit();
    }

    const imported = importedView(&artifact);
    const lowering = loweringView(&artifact);
    try std.testing.expect(imported.exports.defs.ptr == artifact.exports.defs.ptr);
    try std.testing.expect(lowering.roots == &artifact.root_requests);
}

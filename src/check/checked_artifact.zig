//! Checked module artifact boundary.
//!
//! Public post-check lowering is moving toward consuming immutable artifacts and
//! narrowed read-only views instead of loose checked modules plus side stores.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const static_dispatch = @import("static_dispatch_registry.zig");
const canonical = @import("canonical_names.zig");
const canonical_type_keys = @import("canonical_type_keys.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const CompactWriter = collections.CompactWriter;

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
};

pub const PublishInputs = struct {
    module_env_storage: ModuleEnvStorage,
    imports: []const PublishImportArtifact = &.{},
    platform_requirement_context: ?PlatformRequirementContextKey = null,
    platform_app_relation: ?PlatformAppRelation = null,
    explicit_roots: []const ExplicitRootRequestInput = &.{},
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
};

pub const RootRequestTable = struct {
    requests: []RootRequest = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypeStore,
        compile_time_roots: *const CompileTimeRootTable,
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
                .checked_type = try checkedTypeIdForVar(allocator, module, checked_types, root.checked_type),
                .abi = switch (root.kind) {
                    .expect => .test_expect,
                    .constant, .callable_binding => .compile_time,
                },
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

fn sourceTypeIsFunction(module: TypedCIR.Module, var_: Var) bool {
    const store = module.typeStoreConst();
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

pub const CheckedBodyId = enum(u32) { _ };
pub const CheckedExprId = enum(u32) { _ };
pub const CheckedPatternId = enum(u32) { _ };
pub const CheckedStatementId = enum(u32) { _ };
pub const CheckedTypeId = enum(u32) { _ };
pub const CheckedTypeSchemeId = enum(u32) { _ };

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

pub const CheckedTypeStoreView = struct {
    roots: []const CheckedTypeRoot = &.{},
    schemes: []const CheckedTypeScheme = &.{},
};

pub const CheckedTypeStore = struct {
    roots: []CheckedTypeRoot = &.{},
    schemes: []CheckedTypeScheme = &.{},
    artifact_vars: []Var = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
    ) Allocator.Error!CheckedTypeStore {
        var roots = std.ArrayList(CheckedTypeRoot).empty;
        errdefer roots.deinit(allocator);
        var artifact_vars = std.ArrayList(Var).empty;
        errdefer artifact_vars.deinit(allocator);
        var schemes = std.ArrayList(CheckedTypeScheme).empty;
        errdefer {
            for (schemes.items) |scheme| allocator.free(scheme.generalized_vars);
            schemes.deinit(allocator);
        }

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            const tag = module.nodeTag(node);
            if (isExprNodeTag(tag)) {
                _ = try appendCheckedTypeRoot(allocator, module, &roots, &artifact_vars, module.exprType(@enumFromInt(node_idx)));
            } else if (isPatternNodeTag(tag)) {
                _ = try appendCheckedTypeRoot(allocator, module, &roots, &artifact_vars, module.patternType(@enumFromInt(node_idx)));
            }
        }

        for (module.allDefs()) |def_idx| {
            const root = try appendCheckedTypeRoot(allocator, module, &roots, &artifact_vars, module.defType(def_idx));
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
            .artifact_vars = try artifact_vars.toOwnedSlice(allocator),
        };
    }

    pub fn view(self: *const CheckedTypeStore) CheckedTypeStoreView {
        return .{ .roots = self.roots, .schemes = self.schemes };
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

    pub fn varForRoot(self: *const CheckedTypeStore, id: CheckedTypeId) Var {
        return self.artifact_vars[@intFromEnum(id)];
    }

    pub fn deinit(self: *CheckedTypeStore, allocator: Allocator) void {
        for (self.schemes) |scheme| allocator.free(scheme.generalized_vars);
        allocator.free(self.artifact_vars);
        allocator.free(self.schemes);
        allocator.free(self.roots);
        self.* = .{};
    }
};

fn appendCheckedTypeRoot(
    allocator: Allocator,
    module: TypedCIR.Module,
    roots: *std.ArrayList(CheckedTypeRoot),
    artifact_vars: *std.ArrayList(Var),
    var_: Var,
) Allocator.Error!CheckedTypeId {
    const key = try canonical_type_keys.fromVar(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        var_,
    );
    if (findCheckedTypeRoot(roots.items, key)) |id| return id;
    const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(roots.items.len)));
    try roots.append(allocator, .{ .id = id, .key = key });
    try artifact_vars.append(allocator, var_);
    return id;
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

pub const CheckedExpr = struct {
    id: CheckedExprId,
    ty: CheckedTypeId,
    source_region: base.Region,
};

pub const CheckedPattern = struct {
    id: CheckedPatternId,
    ty: CheckedTypeId,
    source_region: base.Region,
};

pub const CheckedStatement = struct {
    id: CheckedStatementId,
    source_region: base.Region,
};

pub const CheckedBodyStoreView = struct {
    bodies: []const CheckedBody = &.{},
    exprs: []const CheckedExpr = &.{},
    patterns: []const CheckedPattern = &.{},
    statements: []const CheckedStatement = &.{},
};

pub const CheckedBodyStore = struct {
    bodies: []CheckedBody = &.{},
    exprs: []CheckedExpr = &.{},
    patterns: []CheckedPattern = &.{},
    statements: []CheckedStatement = &.{},
    expr_by_node: []?CheckedExprId = &.{},
    pattern_by_node: []?CheckedPatternId = &.{},
    statement_by_node: []?CheckedStatementId = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypeStore,
    ) Allocator.Error!CheckedBodyStore {
        var exprs = std.ArrayList(CheckedExpr).empty;
        errdefer exprs.deinit(allocator);
        var patterns = std.ArrayList(CheckedPattern).empty;
        errdefer patterns.deinit(allocator);
        var statements = std.ArrayList(CheckedStatement).empty;
        errdefer statements.deinit(allocator);
        var bodies = std.ArrayList(CheckedBody).empty;
        errdefer bodies.deinit(allocator);
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
                });
                pattern_by_node[node_idx] = id;
            } else if (isStatementNodeTag(tag)) {
                const id: CheckedStatementId = @enumFromInt(@as(u32, @intCast(statements.items.len)));
                try statements.append(allocator, .{
                    .id = id,
                    .source_region = module.regionAt(node),
                });
                statement_by_node[node_idx] = id;
            }
        }

        return .{
            .bodies = try bodies.toOwnedSlice(allocator),
            .exprs = try exprs.toOwnedSlice(allocator),
            .patterns = try patterns.toOwnedSlice(allocator),
            .statements = try statements.toOwnedSlice(allocator),
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
        allocator.free(self.statements);
        allocator.free(self.patterns);
        allocator.free(self.exprs);
        allocator.free(self.bodies);
        self.* = .{};
    }
};

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
    pattern: CIR.Pattern.Idx,
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
    expr: CIR.Expr.Idx,
    ref: ResolvedValueRef,
    checked_ty: Var,
    scope_depth: u32,
};

pub const ResolvedValueRefTable = struct {
    records: []ResolvedValueRefRecord = &.{},
    by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, ResolvedValueRefId) = .{},
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
    ) Allocator.Error!ResolvedValueRefTable {
        const module = modules.module(module_idx);
        var records = std.ArrayList(ResolvedValueRefRecord).empty;
        errdefer records.deinit(allocator);

        var by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, ResolvedValueRefId) = .{};
        errdefer by_expr.deinit(allocator);

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
            var resolved_ref = try classifyValueRef(
                allocator,
                modules,
                module,
                expr_idx,
                imports,
                templates,
                hosted_procs,
                platform_required_declarations,
                platform_required_bindings,
                top_level_values,
            );
            const checked_type_key = try canonical_type_keys.fromVar(
                allocator,
                module.typeStoreConst(),
                module.identStoreConst(),
                module.exprType(expr_idx),
            );
            attachUseTypeKey(&resolved_ref, checked_type_key);

            const id: ResolvedValueRefId = @enumFromInt(@as(u32, @intCast(records.items.len)));
            try records.append(allocator, .{
                .expr = expr_idx,
                .ref = resolved_ref,
                .checked_ty = module.exprType(expr_idx),
                .scope_depth = 0,
            });
            try by_expr.put(allocator, expr_idx, id);
        }

        return .{
            .records = try records.toOwnedSlice(allocator),
            .by_expr = by_expr,
        };
    }

    pub fn lookupByExpr(self: *const ResolvedValueRefTable, expr: CIR.Expr.Idx) ?ResolvedValueRefRecord {
        const id = self.by_expr.get(expr) orelse return null;
        return self.records[@intFromEnum(id)];
    }

    pub fn lookupIdByExpr(self: *const ResolvedValueRefTable, expr: CIR.Expr.Idx) ?ResolvedValueRefId {
        return self.by_expr.get(expr);
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
        self.by_expr.deinit(allocator);
        allocator.free(self.records);
        self.* = .{};
    }
};

fn classifyValueRef(
    allocator: Allocator,
    modules: *const TypedCIR.Modules,
    module: TypedCIR.Module,
    expr_idx: CIR.Expr.Idx,
    imports: []const PublishImportArtifact,
    templates: *const CheckedProcedureTemplateTable,
    hosted_procs: *const HostedProcTable,
    platform_required_declarations: *const PlatformRequiredDeclarationTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    top_level_values: *const TopLevelValueTable,
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
        ),
        .e_lookup_external => |external| classifyImportedValueRef(
            modules,
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
) ResolvedValueRef {
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

    if (patternIsLambdaArg(module, pattern)) {
        return .{ .local_param = .{ .pattern = pattern } };
    }

    if (localStatementForPattern(module, pattern)) |statement| {
        switch (statement) {
            .s_var => return .{ .local_mutable_version = .{ .pattern = pattern } },
            .s_decl => |decl| {
                if (isLocalProcExpr(module, decl.expr)) {
                    return .{ .local_proc = .{ .pattern = pattern } };
                }
                return .{ .local_value = .{ .pattern = pattern } };
            },
            else => {},
        }
    }

    if (patternIsBinder(module, pattern)) {
        return .{ .pattern_binder = .{ .pattern = pattern } };
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
    modules: *const TypedCIR.Modules,
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
    const import_key = publishImportKeyForModule(imports, resolved_module_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: external lookup import {d} resolved to module {d} without a published artifact key",
                .{ @intFromEnum(import_idx), resolved_module_idx },
            );
        }
        unreachable;
    };

    const imported_module = modules.module(resolved_module_idx);
    const target_def: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(target_node_idx)));
    if (imported_module.nodeTag(@enumFromInt(@as(u32, @intCast(target_node_idx)))) != .def) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "checked artifact invariant violated: external value lookup target {d} in module {d} is not a definition",
                .{ target_node_idx, resolved_module_idx },
            );
        }
        unreachable;
    }
    const def = imported_module.def(target_def);
    if (sourceTypeIsFunction(imported_module, imported_module.defType(target_def))) {
        return .{ .imported_proc = .{
            .binding = .{ .imported = .{
                .artifact = import_key,
                .module_idx = resolved_module_idx,
                .def = target_def,
                .pattern = def.pattern.idx,
            } },
            .source_fn_ty_template = .{},
        } };
    }

    return .{ .imported_const = .{
        .const_ref = .{
            .artifact = import_key,
            .module_idx = resolved_module_idx,
            .pattern = def.pattern.idx,
            .schema = @enumFromInt(@intFromEnum(target_def)),
            .value = @enumFromInt(@intFromEnum(target_def)),
            .source_type = imported_module.defType(target_def),
        },
        .requested_source_ty_template = .{},
    } };
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
    module: TypedCIR.Module,
    templates: *CheckedProcedureTemplateTable,
    static_dispatch_plans: *static_dispatch.StaticDispatchPlanTable,
    resolved_value_refs: *ResolvedValueRefTable,
) Allocator.Error!void {
    var traversal = ExprTraversal.init(allocator, module);
    defer traversal.deinit();

    var value_refs = std.ArrayList(ResolvedValueRefId).empty;
    defer value_refs.deinit(allocator);

    var dispatch_refs = std.ArrayList(static_dispatch.StaticDispatchPlanId).empty;
    defer dispatch_refs.deinit(allocator);

    for (templates.templates) |*template| {
        traversal.clear();
        value_refs.clearRetainingCapacity();
        dispatch_refs.clearRetainingCapacity();

        switch (template.body) {
            .checked_body => |_| {},
            .promoted_callable_wrapper,
            .hosted_wrapper,
            .intrinsic_wrapper,
            .entry_wrapper,
            => {},
        }

        for (traversal.exprs.items) |expr| {
            if (resolved_value_refs.lookupIdByExpr(expr)) |ref_id| {
                try value_refs.append(allocator, ref_id);
            }
            if (static_dispatch_plans.lookupByExpr(expr)) |plan_id| {
                try dispatch_refs.append(allocator, plan_id);
            }
        }

        template.resolved_value_refs = try resolved_value_refs.appendTemplateRefSpan(allocator, value_refs.items);
        const dispatch_span = try static_dispatch_plans.appendTemplateRefSpan(allocator, dispatch_refs.items);
        template.static_dispatch_plans = .{
            .start = dispatch_span.start,
            .len = dispatch_span.len,
        };
    }
}

const ExprTraversal = struct {
    allocator: Allocator,
    module: TypedCIR.Module,
    visited: std.AutoHashMap(CIR.Expr.Idx, void),
    exprs: std.ArrayList(CIR.Expr.Idx),

    fn init(allocator: Allocator, module: TypedCIR.Module) ExprTraversal {
        return .{
            .allocator = allocator,
            .module = module,
            .visited = std.AutoHashMap(CIR.Expr.Idx, void).init(allocator),
            .exprs = .empty,
        };
    }

    fn deinit(self: *ExprTraversal) void {
        self.exprs.deinit(self.allocator);
        self.visited.deinit();
    }

    fn clear(self: *ExprTraversal) void {
        self.exprs.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
    }

    fn visitExpr(self: *ExprTraversal, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        const entry = try self.visited.getOrPut(expr_idx);
        if (entry.found_existing) return;
        try self.exprs.append(self.allocator, expr_idx);

        const expr = self.module.expr(expr_idx).data;
        switch (expr) {
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_str_segment,
            .e_bytes_literal,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_required,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_crash,
            .e_ellipsis,
            .e_anno_only,
            .e_runtime_error,
            .e_hosted_lambda,
            => {},
            .e_str => |str| try self.visitExprSpan(str.span),
            .e_list => |list| try self.visitExprSpan(list.elems),
            .e_tuple => |tuple| try self.visitExprSpan(tuple.elems),
            .e_match => |match_expr| {
                try self.visitExpr(match_expr.cond);
                for (self.module.matchBranchSlice(match_expr.branches)) |branch_idx| {
                    const branch = self.module.getMatchBranch(branch_idx);
                    if (branch.guard) |guard| try self.visitExpr(guard);
                    try self.visitExpr(branch.value);
                }
            },
            .e_if => |if_expr| {
                for (self.module.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = self.module.getIfBranch(branch_idx);
                    try self.visitExpr(branch.cond);
                    try self.visitExpr(branch.body);
                }
                try self.visitExpr(if_expr.final_else);
            },
            .e_call => |call| {
                try self.visitExpr(call.func);
                try self.visitExprSpan(call.args);
            },
            .e_record => |record| {
                if (record.ext) |ext| try self.visitExpr(ext);
                for (self.module.sliceRecordFields(record.fields)) |field_idx| {
                    const field = self.module.getRecordField(field_idx);
                    try self.visitExpr(field.value);
                }
            },
            .e_block => |block| {
                for (self.module.sliceStatements(block.stmts)) |stmt_idx| {
                    try self.visitStmt(self.module.getStatement(stmt_idx));
                }
                try self.visitExpr(block.final_expr);
            },
            .e_tag => |tag| try self.visitExprSpan(tag.args),
            .e_nominal => |nominal| try self.visitExpr(nominal.backing_expr),
            .e_nominal_external => |nominal| try self.visitExpr(nominal.backing_expr),
            .e_closure => |closure| try self.visitExpr(closure.lambda_idx),
            .e_lambda => |lambda| try self.visitExpr(lambda.body),
            .e_binop => |binop| {
                try self.visitExpr(binop.lhs);
                try self.visitExpr(binop.rhs);
            },
            .e_unary_minus => |unary| try self.visitExpr(unary.expr),
            .e_unary_not => |unary| try self.visitExpr(unary.expr),
            .e_field_access => |access| try self.visitExpr(access.receiver),
            .e_method_call => |call| {
                try self.visitExpr(call.receiver);
                try self.visitExprSpan(call.args);
            },
            .e_dispatch_call => |call| {
                try self.visitExpr(call.receiver);
                try self.visitExprSpan(call.args);
            },
            .e_structural_eq => |eq| {
                try self.visitExpr(eq.lhs);
                try self.visitExpr(eq.rhs);
            },
            .e_method_eq => |eq| {
                try self.visitExpr(eq.lhs);
                try self.visitExpr(eq.rhs);
            },
            .e_type_method_call => |call| try self.visitExprSpan(call.args),
            .e_type_dispatch_call => |call| try self.visitExprSpan(call.args),
            .e_tuple_access => |access| try self.visitExpr(access.tuple),
            .e_dbg => |dbg| try self.visitExpr(dbg.expr),
            .e_expect => |expect| try self.visitExpr(expect.body),
            .e_return => |return_expr| try self.visitExpr(return_expr.expr),
            .e_for => |for_expr| {
                try self.visitExpr(for_expr.expr);
                try self.visitExpr(for_expr.body);
            },
            .e_run_low_level => |low_level| try self.visitExprSpan(low_level.args),
        }
    }

    fn visitExprSpan(self: *ExprTraversal, span: CIR.Expr.Span) Allocator.Error!void {
        for (self.module.sliceExpr(span)) |expr| try self.visitExpr(expr);
    }

    fn visitStmt(self: *ExprTraversal, stmt: CIR.Statement) Allocator.Error!void {
        switch (stmt) {
            .s_decl => |decl| try self.visitExpr(decl.expr),
            .s_var => |var_| try self.visitExpr(var_.expr),
            .s_reassign => |reassign| try self.visitExpr(reassign.expr),
            .s_dbg => |dbg| try self.visitExpr(dbg.expr),
            .s_expr => |expr| try self.visitExpr(expr.expr),
            .s_expect => |expect| try self.visitExpr(expect.body),
            .s_for => |for_stmt| {
                try self.visitExpr(for_stmt.expr);
                try self.visitExpr(for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.visitExpr(while_stmt.cond);
                try self.visitExpr(while_stmt.body);
            },
            .s_return => |return_stmt| try self.visitExpr(return_stmt.expr),
            .s_crash,
            .s_break,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
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

pub const ProcTarget = union(enum) {
    roc,
    hosted,
    intrinsic,
    entry,
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

    const Candidate = struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
        expr_idx: CIR.Expr.Idx,
        external_symbol_name: canonical.ExternalSymbolNameId,
        proc: canonical.ProcedureValueRef,
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
pub const CallableResultRecordId = enum(u32) { _ };

pub const CompileTimeRootKind = enum {
    constant,
    callable_binding,
    expect,
};

pub const CompileTimeRootPayload = union(enum) {
    const_graph: ConstGraphReificationPlanId,
    callable_result: CallableResultRecordId,
    expect,
};

pub const CompileTimeRoot = struct {
    id: ComptimeRootId,
    module_idx: u32,
    kind: CompileTimeRootKind,
    source: RootSource,
    pattern: ?CIR.Pattern.Idx,
    expr: CIR.Expr.Idx,
    checked_type: Var,
    dependency_summary_request: ComptimeDependencySummaryRequestId,
    payload: CompileTimeRootPayload,
};

pub const CompileTimeRootTable = struct {
    roots: []CompileTimeRoot = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!CompileTimeRootTable {
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
                .checked_type = ModuleEnv.varFrom(stmt.s_expect.body),
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
                .checked_type = source_ty,
                .payload = if (is_callable)
                    .{ .callable_result = @enumFromInt(@as(u32, @intCast(roots.items.len))) }
                else
                    .{ .const_graph = @enumFromInt(@as(u32, @intCast(roots.items.len))) },
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
        checked_type: Var,
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

pub const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    source_type: Var,
};

pub const TopLevelValueKind = union(enum) {
    const_ref: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
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
        checked_types: *const CheckedTypeStore,
        templates: *const CheckedProcedureTemplateTable,
        callable_eval_templates: *CallableEvalTemplateTable,
        procedure_bindings: *TopLevelProcedureBindingTable,
        artifact_key: CheckedModuleArtifactKey,
        compile_time_roots: *const CompileTimeRootTable,
        comptime_values: *CompileTimeValueStore,
    ) Allocator.Error!TopLevelValueTable {
        var entries = std.ArrayList(TopLevelValueEntry).empty;
        errdefer entries.deinit(allocator);

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            const source_ty = module.defType(def_idx);
            const value: TopLevelValueKind = if (topLevelExprIsAlreadyProcedure(def.expr.data)) blk: {
                const template = templates.lookupByDef(def_idx) orelse unreachable;
                const source_scheme = try canonical_type_keys.schemeFromVar(
                    allocator,
                    module.typeStoreConst(),
                    module.identStoreConst(),
                    source_ty,
                );
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
                const source_scheme = try canonical_type_keys.schemeFromVar(
                    allocator,
                    module.typeStoreConst(),
                    module.identStoreConst(),
                    source_ty,
                );
                const checked_fn_root = try checkedTypeIdForVar(allocator, module, checked_types, root.checked_type);
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
            } else .{ .const_ref = try comptime_values.reserveConst(
                artifact_key,
                module.moduleIndex(),
                def.pattern.idx,
                source_ty,
            ) };

            try entries.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .pattern = def.pattern.idx,
                .source_type = source_ty,
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
    callable: canonical.ProcedureCallableRef,
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

    pub fn reserveConst(
        self: *CompileTimeValueStore,
        artifact_key: CheckedModuleArtifactKey,
        module_idx: u32,
        pattern: CIR.Pattern.Idx,
        source_type: Var,
    ) Allocator.Error!ConstRef {
        const schema = try self.addSchema(.pending);
        const value = try self.addValue(.pending);
        try self.bind(pattern, schema, value);
        return .{
            .artifact = artifact_key,
            .module_idx = module_idx,
            .pattern = pattern,
            .schema = schema,
            .value = value,
            .source_type = source_type,
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
            .callable,
            => {},
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
pub const ConstReificationPlanId = enum(u32) { _ };
pub const ComptimeDependencySummaryTemplateId = enum(u32) { _ };
pub const PrivateCaptureId = enum(u32) { _ };
pub const PrivateCaptureNodeId = enum(u32) { _ };
pub const MethodRegistryEntryRef = enum(u32) { _ };

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

pub const ImportedTemplateClosureView = struct {
    checked_callable_bodies: []const ArtifactCheckedCallableBodyRef = &.{},
    checked_const_bodies: []const ArtifactCheckedConstBodyRef = &.{},
    checked_procedure_templates: []const ArtifactProcedureTemplateRef = &.{},
    callable_eval_templates: []const ArtifactCallableEvalTemplateRef = &.{},
    const_templates: []const ConstRef = &.{},
    promoted_procedures: []const PromotedProcedureRef = &.{},
    semantic_instantiation_procedures: []const SemanticInstantiationProcedureId = &.{},
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
        checked_templates: *const CheckedProcedureTemplateTable,
    ) Allocator.Error!ExportedProcedureTemplateTable {
        var templates = std.ArrayList(ExportedProcedureTemplate).empty;
        errdefer templates.deinit(allocator);

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
            try templates.append(allocator, .{
                .export_name = export_name,
                .def = def_idx,
                .source_scheme = source_scheme,
                .template = template,
            });
        }

        return .{ .templates = try templates.toOwnedSlice(allocator) };
    }

    pub fn view(self: *const ExportedProcedureTemplateTable) ExportedProcedureTemplateView {
        return .{ .templates = self.templates };
    }

    pub fn deinit(self: *ExportedProcedureTemplateTable, allocator: Allocator) void {
        allocator.free(self.templates);
        self.* = .{};
    }
};

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
        top_level_values: *const TopLevelValueTable,
        procedure_bindings: *const TopLevelProcedureBindingTable,
        artifact_key: CheckedModuleArtifactKey,
    ) Allocator.Error!ExportedProcedureBindingTable {
        var bindings = std.ArrayList(ImportedProcedureBindingView).empty;
        errdefer bindings.deinit(allocator);

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
            try bindings.append(allocator, .{
                .binding = .{
                    .artifact = artifact_key,
                    .module_idx = module.moduleIndex(),
                    .def = def_idx,
                    .pattern = def.pattern.idx,
                },
                .source_scheme = binding.source_scheme,
                .body = body,
            });
        }

        return .{ .bindings = try bindings.toOwnedSlice(allocator) };
    }

    pub fn view(self: *const ExportedProcedureBindingTable) ExportedProcedureBindingView {
        return .{ .bindings = self.bindings };
    }

    pub fn deinit(self: *ExportedProcedureBindingTable, allocator: Allocator) void {
        allocator.free(self.bindings);
        self.* = .{};
    }
};

pub const ConstTemplate = struct {
    body: CheckedConstBodyRef,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    resolved_value_refs: ResolvedValueRefTableRef = .{},
    static_dispatch_plans: StaticDispatchPlanTableRef = .{},
    nested_proc_sites: NestedProcSiteTableRef = .{},
    dependency_template: ComptimeDependencySummaryTemplateId,
};

pub const ImportedConstTemplateView = struct {
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

    pub fn deinit(self: *ExportedConstTemplateTable, allocator: Allocator) void {
        allocator.free(self.templates);
        self.* = .{};
    }

    pub fn view(self: *const ExportedConstTemplateTable) ExportedConstTemplateView {
        return .{ .templates = self.templates };
    }
};

pub const ConstInstantiationStoreView = struct {
    instances: []const ConstInstanceId = &.{},
};

pub const CallableBindingInstantiationStoreView = struct {
    instances: []const CallableBindingInstanceId = &.{},
};

pub const SemanticInstantiationProcedureTableView = struct {
    procedures: []const SemanticInstantiationProcedureId = &.{},
};

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
    checked_procedure_templates: CheckedProcedureTemplateTable,
    top_level_procedure_bindings: TopLevelProcedureBindingTable,
    callable_eval_templates: CallableEvalTemplateTable = .{},
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_declarations: PlatformRequiredDeclarationTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    compile_time_roots: CompileTimeRootTable,
    top_level_values: TopLevelValueTable,
    promoted_procedures: PromotedProcedureTable,
    comptime_values: CompileTimeValueStore,
    const_instances: ConstInstantiationStoreView = .{},
    callable_binding_instances: CallableBindingInstantiationStoreView = .{},
    semantic_instantiation_procedures: SemanticInstantiationProcedureTableView = .{},

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

    pub fn deinit(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.comptime_values.deinit(allocator);
        _ = self.semantic_instantiation_procedures;
        _ = self.callable_binding_instances;
        _ = self.const_instances;
        self.promoted_procedures.deinit(allocator);
        self.top_level_values.deinit(allocator);
        self.compile_time_roots.deinit(allocator);
        self.platform_required_bindings.deinit(allocator);
        self.platform_required_declarations.deinit(allocator);
        self.hosted_procs.deinit(allocator);
        self.root_requests.deinit(allocator);
        self.callable_eval_templates.deinit(allocator);
        self.top_level_procedure_bindings.deinit(allocator);
        self.checked_procedure_templates.deinit(allocator);
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

    pub fn verifyPublished(self: *const CheckedModuleArtifact) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.module_identity.module_idx != std.math.maxInt(u32));

        for (self.root_requests.requests, 0..) |request, i| {
            std.debug.assert(request.order == i);
            std.debug.assert(request.module_idx == self.module_identity.module_idx);
        }

        for (self.compile_time_roots.roots, 0..) |root, i| {
            std.debug.assert(@intFromEnum(root.id) == i);
            std.debug.assert(root.module_idx == self.module_identity.module_idx);
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
            std.debug.assert(self.checked_types.artifact_vars.len == self.checked_types.roots.len);
        }

        for (self.checked_bodies.exprs, 0..) |expr, i| {
            std.debug.assert(@intFromEnum(expr.id) == i);
            std.debug.assert(@intFromEnum(expr.ty) < self.checked_types.roots.len);
        }

        for (self.checked_bodies.patterns, 0..) |pattern, i| {
            std.debug.assert(@intFromEnum(pattern.id) == i);
            std.debug.assert(@intFromEnum(pattern.ty) < self.checked_types.roots.len);
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
                .promoted_callable_wrapper,
                .hosted_wrapper,
                .intrinsic_wrapper,
                .entry_wrapper,
                => {},
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
                .const_ref => |const_ref| std.debug.assert(const_ref.module_idx == self.module_identity.module_idx),
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
                                .lifted, .synthetic => std.debug.panic(
                                    "checked artifact invariant violated: direct top-level binding cannot use lifted or synthetic template",
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

        self.comptime_values.verifySealed();

        for (self.resolved_value_refs.records) |record| {
            std.debug.assert(self.resolved_value_refs.by_expr.get(record.expr) != null);
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
            std.debug.assert(const_use.const_ref.pattern == binding.app_value.pattern);
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
    module_identity: ModuleIdentity,
    exports: ExportTableView,
    checked_types: CheckedTypeStoreView,
    checked_bodies: CheckedBodyStoreView,
    exported_procedure_templates: ExportedProcedureTemplateView,
    exported_procedure_bindings: ExportedProcedureBindingView,
    exported_const_templates: ExportedConstTemplateView,
    callable_eval_templates: CallableEvalTemplateTableView,
    method_registry: *const static_dispatch.MethodRegistry,
    interface_capabilities: *const ModuleInterfaceCapabilities,
    comptime_values: *const CompileTimeValueStore,
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
        .module_identity = artifact.module_identity,
        .exports = artifact.exports.view(),
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .exported_procedure_templates = artifact.exported_procedure_templates.view(),
        .exported_procedure_bindings = artifact.exported_procedure_bindings.view(),
        .exported_const_templates = artifact.exported_const_templates.view(),
        .callable_eval_templates = artifact.callable_eval_templates.view(),
        .method_registry = &artifact.method_registry,
        .interface_capabilities = &artifact.interface_capabilities,
        .comptime_values = &artifact.comptime_values,
        .const_instances = artifact.const_instances,
        .callable_binding_instances = artifact.callable_binding_instances,
        .semantic_instantiation_procedures = artifact.semantic_instantiation_procedures,
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

    var checked_types = try CheckedTypeStore.fromModule(allocator, module);
    errdefer checked_types.deinit(allocator);

    var checked_bodies = try CheckedBodyStore.fromModule(allocator, module, &checked_types);
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

    var method_registry = try static_dispatch.MethodRegistry.fromModule(allocator, module, &canonical_names, &template_lookup);
    errdefer method_registry.deinit(allocator);

    var static_dispatch_plans = try static_dispatch.StaticDispatchPlanTable.fromModule(allocator, module, &canonical_names);
    errdefer static_dispatch_plans.deinit(allocator);

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

    var compile_time_roots = try CompileTimeRootTable.fromModule(allocator, module);
    errdefer compile_time_roots.deinit(allocator);

    var root_requests = try RootRequestTable.fromModule(
        allocator,
        module,
        &checked_types,
        &compile_time_roots,
        &platform_required_bindings,
        inputs.explicit_roots,
    );
    errdefer root_requests.deinit(allocator);

    var comptime_values = CompileTimeValueStore.init(allocator);
    errdefer comptime_values.deinit(allocator);

    var top_level_procedure_bindings = TopLevelProcedureBindingTable.initEmpty();
    errdefer top_level_procedure_bindings.deinit(allocator);

    var callable_eval_templates = CallableEvalTemplateTable{};
    errdefer callable_eval_templates.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(
        allocator,
        module,
        &checked_types,
        &checked_procedure_templates,
        &callable_eval_templates,
        &top_level_procedure_bindings,
        artifact_key,
        &compile_time_roots,
        &comptime_values,
    );
    errdefer top_level_values.deinit(allocator);
    try comptime_values.sealBindings();

    var exported_procedure_templates = try ExportedProcedureTemplateTable.fromModule(
        allocator,
        module,
        &canonical_names,
        &checked_procedure_templates,
    );
    errdefer exported_procedure_templates.deinit(allocator);

    var exported_procedure_bindings = try ExportedProcedureBindingTable.fromModule(
        allocator,
        module,
        &top_level_values,
        &top_level_procedure_bindings,
        artifact_key,
    );
    errdefer exported_procedure_bindings.deinit(allocator);

    var exported_const_templates = ExportedConstTemplateTable{};
    errdefer exported_const_templates.deinit(allocator);

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
    );
    errdefer resolved_value_refs.deinit(allocator);

    try sealCheckedProcedureTemplateRefs(
        allocator,
        module,
        &checked_procedure_templates,
        &static_dispatch_plans,
        &resolved_value_refs,
    );

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
        .checked_procedure_templates = checked_procedure_templates,
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
        .comptime_values = comptime_values,
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
        .resolved_value_refs = .{},
        .checked_procedure_templates = .{},
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
        .comptime_values = CompileTimeValueStore.init(std.testing.allocator),
    };
    defer {
        artifact.comptime_values.deinit(std.testing.allocator);
        artifact.canonical_names.deinit();
    }

    const imported = importedView(&artifact);
    const lowering = loweringView(&artifact);
    try std.testing.expect(imported.exports.defs.ptr == artifact.exports.defs.ptr);
    try std.testing.expect(lowering.roots == &artifact.root_requests);
}

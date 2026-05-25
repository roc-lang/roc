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
const const_store = @import("const_store.zig");

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

        return .{
            .source_hash = source_hash,
            .compiler_artifact_hash = compiler_artifact_hash,
            .module_identity_hash = module_identity_hash,
            .checking_context_identity_hash = checking_context_identity_hash,
            .direct_import_artifact_keys_hash = direct_import_artifact_keys_hash,
            .bytes = computeCheckedArtifactKeyBytes(
                source_hash,
                compiler_artifact_hash,
                module_identity_hash,
                checking_context_identity_hash,
                direct_import_artifact_keys_hash,
            ),
        };
    }
};

/// Stable identifier for a checked module.
pub const ModuleId = CheckedModuleArtifactKey;

fn computeCheckedArtifactKeyBytes(
    source_hash: [32]u8,
    compiler_artifact_hash: [32]u8,
    module_identity_hash: [32]u8,
    checking_context_identity_hash: [32]u8,
    direct_import_artifact_keys_hash: [32]u8,
) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(&source_hash);
    hasher.update(&compiler_artifact_hash);
    hasher.update(&module_identity_hash);
    hasher.update(&checking_context_identity_hash);
    hasher.update(&direct_import_artifact_keys_hash);
    return hasher.finalResult();
}

test "checked artifact key final bytes include compiler artifact hash" {
    const source_hash = [_]u8{1} ** 32;
    const module_identity_hash = [_]u8{2} ** 32;
    const checking_context_identity_hash = [_]u8{3} ** 32;
    const direct_import_artifact_keys_hash = [_]u8{4} ** 32;

    const first = computeCheckedArtifactKeyBytes(
        source_hash,
        [_]u8{5} ** 32,
        module_identity_hash,
        checking_context_identity_hash,
        direct_import_artifact_keys_hash,
    );
    const second = computeCheckedArtifactKeyBytes(
        source_hash,
        [_]u8{6} ** 32,
        module_identity_hash,
        checking_context_identity_hash,
        direct_import_artifact_keys_hash,
    );

    try std.testing.expect(!std.meta.eql(first, second));
}

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
    bytes = .{
        @as(u8, @truncate(value)),
        @as(u8, @truncate(value >> 8)),
        @as(u8, @truncate(value >> 16)),
        @as(u8, @truncate(value >> 24)),
    };
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

const CheckedImportViews = struct {
    direct: []const PublishImportArtifact,
    available: []const ImportedModuleView = &.{},
};

/// Checked artifacts that must be available to consume this module's public API.
/// This is semantic visibility, not lexical import visibility.
pub const PublicApiDependencies = struct {
    artifacts: []const CheckedModuleArtifactKey = &.{},

    pub fn deinit(self: *PublicApiDependencies, allocator: Allocator) void {
        allocator.free(self.artifacts);
        self.* = .{};
    }
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

/// Public `ProvidedProcedureExport` declaration.
pub const ProvidedProcedureExport = struct {
    source_name: canonical.ExportNameId,
    ffi_symbol: canonical.ExternalSymbolNameId,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
    checked_type: CheckedTypeId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    binding: TopLevelProcedureBindingRef,
};

/// Public `ProvidedDataExport` declaration.
pub const ProvidedDataExport = struct {
    source_name: canonical.ExportNameId,
    ffi_symbol: canonical.ExternalSymbolNameId,
    def: CIR.Def.Idx,
    pattern: CheckedPatternId,
    checked_type: CheckedTypeId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
    const_ref: ConstRef,
};

/// Public `ProvidedExport` declaration.
pub const ProvidedExport = union(enum) {
    procedure: ProvidedProcedureExport,
    data: ProvidedDataExport,
};

/// Public `ProvidedExportTable` declaration.
pub const ProvidedExportTable = struct {
    exports: []ProvidedExport = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypePublication,
        top_level_values: *const TopLevelValueTable,
        published_provides: []const ProvidesEntry,
    ) Allocator.Error!ProvidedExportTable {
        const module_env = module.moduleEnvConst();
        const source = module_env.provides_entries.items.items;
        if (source.len != published_provides.len) {
            checkedArtifactInvariant("published provides metadata disagrees with ModuleEnv provides count", .{});
        }

        var exports = std.ArrayList(ProvidedExport).empty;
        errdefer exports.deinit(allocator);

        for (source, published_provides) |provides_entry, published| {
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
            const top_level = top_level_values.lookupByDef(def_idx) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: provided entry {s} has no top-level value",
                        .{module_env.getIdent(provides_entry.ident)},
                    );
                }
                unreachable;
            };
            const checked_type = try checkedTypeIdForRootSource(
                allocator,
                module,
                checked_types,
                .{ .def = def_idx },
            );
            switch (top_level.value) {
                .procedure_binding => |binding| try exports.append(allocator, .{ .procedure = .{
                    .source_name = published.source_name,
                    .ffi_symbol = published.ffi_symbol,
                    .def = def_idx,
                    .pattern = top_level.pattern,
                    .checked_type = checked_type,
                    .source_scheme = top_level.source_scheme,
                    .binding = binding,
                } }),
                .const_ref => |const_ref| try exports.append(allocator, .{ .data = .{
                    .source_name = published.source_name,
                    .ffi_symbol = published.ffi_symbol,
                    .def = def_idx,
                    .pattern = top_level.pattern,
                    .checked_type = checked_type,
                    .source_scheme = top_level.source_scheme,
                    .const_ref = const_ref,
                } }),
            }
        }

        return .{ .exports = try exports.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *ProvidedExportTable, allocator: Allocator) void {
        allocator.free(self.exports);
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
    procedure_binding: ?TopLevelProcedureBindingRef = null,
    procedure_use: ?ProcedureUseTemplate = null,
};

/// Public `LoweringEntrypointRequest` declaration.
pub const LoweringEntrypointRequest = union(enum) {
    root: RootRequest,
};

/// Public `RootRequestTable` declaration.
pub const RootRequestTable = struct {
    requests: []RootRequest = &.{},
    runtime_requests: []RootRequest = &.{},
    compile_time_requests: []RootRequest = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_types: *const CheckedTypePublication,
        compile_time_roots: *const CompileTimeRootTable,
        procedure_templates: *const CheckedProcedureTemplateTable,
        entry_wrappers: *const EntryWrapperTable,
        platform_required_bindings: *const PlatformRequiredBindingTable,
        provided_exports: *const ProvidedExportTable,
        checked_bodies: *const CheckedBodyStore,
        resolved_value_refs: *const ResolvedValueRefTable,
        top_level_procedure_bindings: *const TopLevelProcedureBindingTable,
        explicit_roots: []const ExplicitRootRequestInput,
    ) Allocator.Error!RootRequestTable {
        var requests = std.ArrayList(RootRequest).empty;
        errdefer requests.deinit(allocator);

        const relation_blocked_exprs = try allocator.alloc(?bool, checked_bodies.exprs.len);
        defer allocator.free(relation_blocked_exprs);
        @memset(relation_blocked_exprs, null);

        for (explicit_roots) |root| {
            const source_checked_type = try checkedTypeIdForRootSource(allocator, module, checked_types, root.source);
            const backing = explicitRootBackingProcedure(
                procedure_templates,
                compile_time_roots,
                entry_wrappers,
                root.source,
                source_checked_type,
            );
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = root.kind,
                .source = root.source,
                .checked_type = backing.checked_type,
                .abi = root.abi,
                .exposure = root.exposure,
                .procedure_template = backing.template,
            });
        }

        try appendPublishedEntrypointRoots(&requests, allocator, module, checked_types, procedure_templates, provided_exports, top_level_procedure_bindings);

        for (platform_required_bindings.bindings, 0..) |binding, i| {
            switch (binding.value_use) {
                .procedure_value => |procedure| try appendRoot(&requests, allocator, .{
                    .module_idx = module.moduleIndex(),
                    .kind = .platform_required_binding,
                    .source = .{ .required_binding = @intCast(i) },
                    .checked_type = platformRequiredBindingCheckedType(binding),
                    .abi = .platform,
                    .exposure = .platform_required,
                    .procedure_use = procedure.procedure,
                }),
                .const_value => {},
            }
        }

        for (compile_time_roots.roots) |root| {
            const concrete = root.kind == .expect or try checkedTypeIsConcreteCompileTimeRoot(allocator, &checked_types.store, root.checked_type);
            if (!concrete) {
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

        const all_requests = try requests.toOwnedSlice(allocator);
        errdefer allocator.free(all_requests);

        const runtime_requests = try collectRuntimeRootRequests(allocator, all_requests);
        errdefer allocator.free(runtime_requests);

        const compile_time_requests = try collectCompileTimeRootRequests(allocator, all_requests);

        return .{
            .requests = all_requests,
            .runtime_requests = runtime_requests,
            .compile_time_requests = compile_time_requests,
        };
    }

    pub fn deinit(self: *RootRequestTable, allocator: Allocator) void {
        allocator.free(self.compile_time_requests);
        allocator.free(self.runtime_requests);
        allocator.free(self.requests);
        self.* = .{};
    }
};

fn collectRuntimeRootRequests(
    allocator: Allocator,
    requests: []const RootRequest,
) Allocator.Error![]RootRequest {
    var runtime_requests = std.ArrayList(RootRequest).empty;
    errdefer runtime_requests.deinit(allocator);

    for (requests) |request| {
        if (request.abi == .compile_time) continue;
        try runtime_requests.append(allocator, request);
    }

    return try runtime_requests.toOwnedSlice(allocator);
}

fn collectCompileTimeRootRequests(
    allocator: Allocator,
    requests: []const RootRequest,
) Allocator.Error![]RootRequest {
    var compile_time_requests = std.ArrayList(RootRequest).empty;
    errdefer compile_time_requests.deinit(allocator);

    for (requests) |request| {
        if (request.abi != .compile_time) continue;
        try compile_time_requests.append(allocator, request);
    }

    return try compile_time_requests.toOwnedSlice(allocator);
}

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
        .alias => |alias| (try checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, alias.args, active)) and
            try checkedTypeIsConcreteCompileTimeRootInner(checked_types, alias.backing, active),
        .record => |record| (try checkedFieldTypesAreConcreteCompileTimeRoots(checked_types, record.fields, active)) and
            try checkedTypeIsConcreteCompileTimeRootInner(checked_types, record.ext, active),
        .record_unbound => |fields| checkedFieldTypesAreConcreteCompileTimeRoots(checked_types, fields, active),
        .tuple => |items| checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, items, active),
        .nominal => |nominal| blk: {
            if (!try checkedTypeSpanIsConcreteCompileTimeRoot(checked_types, nominal.args, active)) break :blk false;
            switch (nominal.representation) {
                .builtin => |builtin_type| switch (builtinRuntimeEncoding(builtin_type)) {
                    .primitive,
                    .list,
                    .box,
                    => break :blk true,
                    .bool_tag_union => {},
                },
                .opaque_without_backing => break :blk true,
                else => {},
            }
            break :blk try checkedTypeIsConcreteCompileTimeRootInner(checked_types, nominal.backing, active);
        },
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

fn verifyRootRequestSubsets(root_requests: RootRequestTable) void {
    if (builtin.mode != .Debug) return;

    var runtime_index: usize = 0;
    var compile_time_index: usize = 0;

    for (root_requests.requests) |request| {
        if (request.abi == .compile_time) {
            if (compile_time_index >= root_requests.compile_time_requests.len) {
                std.debug.panic("checked artifact invariant violated: compile-time root request subset is missing an entry", .{});
            }
            if (!std.meta.eql(root_requests.compile_time_requests[compile_time_index], request)) {
                std.debug.panic("checked artifact invariant violated: compile-time root request subset is out of order", .{});
            }
            compile_time_index += 1;
        } else {
            if (runtime_index >= root_requests.runtime_requests.len) {
                std.debug.panic("checked artifact invariant violated: runtime root request subset is missing an entry", .{});
            }
            if (!std.meta.eql(root_requests.runtime_requests[runtime_index], request)) {
                std.debug.panic("checked artifact invariant violated: runtime root request subset is out of order", .{});
            }
            runtime_index += 1;
        }
    }

    if (runtime_index != root_requests.runtime_requests.len) {
        std.debug.panic("checked artifact invariant violated: runtime root request subset has extra entries", .{});
    }
    if (compile_time_index != root_requests.compile_time_requests.len) {
        std.debug.panic("checked artifact invariant violated: compile-time root request subset has extra entries", .{});
    }
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
        .num_from_numeral,
        .typed_int,
        .typed_frac,
        .typed_num_from_numeral,
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
    procedure_templates: *const CheckedProcedureTemplateTable,
    provided_exports: *const ProvidedExportTable,
    top_level_procedure_bindings: *const TopLevelProcedureBindingTable,
) Allocator.Error!void {
    const module_env = module.moduleEnvConst();

    for (provided_exports.exports) |provided| {
        switch (provided) {
            .procedure => |procedure| try appendRoot(requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .provided_export,
                .source = .{ .def = procedure.def },
                .checked_type = procedure.checked_type,
                .abi = .platform,
                .exposure = .exported,
                .procedure_template = procedureTemplateForTopLevelBinding(top_level_procedure_bindings, procedure.binding),
                .procedure_binding = procedure.binding,
            }),
            .data => {},
        }
    }

    switch (module_env.module_kind) {
        .default_app => {
            const main_ident = module_env.idents.main_bang;
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
                .procedure_template = requiredProcedureTemplateForRootSource(procedure_templates, .{ .def = main_def }),
            });
        },
        else => {},
    }
}

fn procedureTemplateForRootSource(
    procedure_templates: *const CheckedProcedureTemplateTable,
    source: RootSource,
) ?canonical.ProcedureTemplateRef {
    return switch (source) {
        .def => |def_idx| procedure_templates.lookupByDef(def_idx),
        else => null,
    };
}

fn procedureTemplateForTopLevelBinding(
    top_level_procedure_bindings: *const TopLevelProcedureBindingTable,
    binding_ref: TopLevelProcedureBindingRef,
) ?canonical.ProcedureTemplateRef {
    const binding = top_level_procedure_bindings.get(binding_ref);
    return switch (binding.body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template| template,
            .synthetic => |synthetic| synthetic.template,
            .lifted => checkedArtifactInvariant("checked root binding referenced lifted procedure before post-check lowering", .{}),
        },
        .callable_eval_template => null,
    };
}

fn requiredProcedureTemplateForRootSource(
    procedure_templates: *const CheckedProcedureTemplateTable,
    source: RootSource,
) canonical.ProcedureTemplateRef {
    return procedureTemplateForRootSource(procedure_templates, source) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("checked artifact invariant violated: root procedure source has no checked procedure template", .{});
        }
        unreachable;
    };
}

const RootBackingProcedure = struct {
    checked_type: CheckedTypeId,
    template: canonical.ProcedureTemplateRef,
};

fn explicitRootBackingProcedure(
    procedure_templates: *const CheckedProcedureTemplateTable,
    compile_time_roots: *const CompileTimeRootTable,
    entry_wrappers: *const EntryWrapperTable,
    source: RootSource,
    source_checked_type: CheckedTypeId,
) RootBackingProcedure {
    if (procedureTemplateForRootSource(procedure_templates, source)) |template| {
        return .{ .checked_type = source_checked_type, .template = template };
    }

    const root_id = compile_time_roots.lookupIdBySource(source) orelse {
        checkedArtifactInvariant("explicit root has no checked procedure template or entry wrapper", .{});
    };
    const wrapper = entryWrapperForRoot(entry_wrappers, root_id);
    return .{ .checked_type = wrapper.checked_fn_root, .template = wrapper.template };
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
    _: Allocator,
    module: TypedCIR.Module,
    checked_types: *const CheckedTypePublication,
    var_: Var,
) Allocator.Error!CheckedTypeId {
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
    procedure_binding: ?TopLevelProcedureBindingRef = null,
    procedure_use: ?ProcedureUseTemplate = null,
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
        .procedure_binding = request.procedure_binding,
        .procedure_use = request.procedure_use,
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
    const common = module.commonIdents();
    if (expr_ident.eql(common.builtin_str_inspect)) return .str_inspect;
    if (Ident.textEndsWith(module.getIdent(expr_ident), ".is_eq")) return .structural_eq;

    if (def.patternName()) |pattern_ident| {
        if (pattern_ident.eql(common.builtin_str_inspect)) return .str_inspect;
        if (Ident.textEndsWith(module.getIdent(pattern_ident), ".is_eq")) return .structural_eq;
    }

    return null;
}

pub const CheckedBodyId = checked_ids.CheckedBodyId;
pub const CheckedExprId = checked_ids.CheckedExprId;
pub const CheckedPatternId = checked_ids.CheckedPatternId;
pub const CheckedStatementId = checked_ids.CheckedStatementId;
pub const CheckedTypeId = checked_ids.CheckedTypeId;
pub const ConstStore = const_store.ConstStore;
pub const ConstNodeId = const_store.ConstNodeId;
pub const ConstFnId = const_store.ConstFnId;
pub const ConstValue = const_store.ConstValue;
pub const ConstScalar = const_store.ConstScalar;
pub const ConstNamedType = const_store.NamedType;
pub const CheckedTypeSchemeId = checked_ids.CheckedTypeSchemeId;
pub const StaticDispatchPlanId = static_dispatch.StaticDispatchPlanId;
pub const IteratorForPlanId = static_dispatch.IteratorForPlanId;
/// Public `PatternBinderId` declaration.
pub const PatternBinderId = checked_ids.PatternBinderId;

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

/// Public `RowDefault` declaration.
pub const RowDefault = enum {
    empty_record,
    empty_tag_union,
};

/// Public `CheckedTypeVariable` declaration.
pub const CheckedTypeVariable = struct {
    name: ?[]const u8 = null,
    constraints: []const CheckedStaticDispatchConstraint = &.{},
    numeric_default_phase: ?NumericDefaultPhase = null,
    row_default: ?RowDefault = null,
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

/// Public `CheckedPrimitive` declaration.
pub const CheckedPrimitive = enum {
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
};

/// Public `CheckedBuiltinRuntimeEncoding` declaration.
pub const CheckedBuiltinRuntimeEncoding = union(enum) {
    primitive: CheckedPrimitive,
    bool_tag_union,
    list,
    box,
};

/// Public `builtinRuntimeEncoding` function.
pub fn builtinRuntimeEncoding(builtin_nominal: CheckedBuiltinNominal) CheckedBuiltinRuntimeEncoding {
    return switch (builtin_nominal) {
        .bool => .bool_tag_union,
        .str => .{ .primitive = .str },
        .u8 => .{ .primitive = .u8 },
        .i8 => .{ .primitive = .i8 },
        .u16 => .{ .primitive = .u16 },
        .i16 => .{ .primitive = .i16 },
        .u32 => .{ .primitive = .u32 },
        .i32 => .{ .primitive = .i32 },
        .u64 => .{ .primitive = .u64 },
        .i64 => .{ .primitive = .i64 },
        .u128 => .{ .primitive = .u128 },
        .i128 => .{ .primitive = .i128 },
        .f32 => .{ .primitive = .f32 },
        .f64 => .{ .primitive = .f64 },
        .dec => .{ .primitive = .dec },
        .list => .list,
        .box => .box,
    };
}

/// Public `CheckedAliasType` declaration.
pub const CheckedAliasType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleNameId,
    backing: CheckedTypeId,
    args: []const CheckedTypeId = &.{},
};

/// Public `CheckedNominalDeclarationId` declaration.
pub const CheckedNominalDeclarationId = enum(u32) { _ };

/// Public `ImportedNominalDeclarationRef` declaration.
pub const ImportedNominalDeclarationRef = struct {
    artifact: CheckedModuleArtifactKey,
    declaration: CheckedNominalDeclarationId,
};

/// Public `LocalBoxPayloadCapabilityRef` declaration.
pub const LocalBoxPayloadCapabilityRef = struct {
    capability: BoxPayloadCapabilityId,
    opaque_atomic_proof: ?OpaqueAtomicProofId = null,
};

/// Public `ImportedBoxPayloadCapabilityRef` declaration.
pub const ImportedBoxPayloadCapabilityRef = struct {
    artifact: CheckedModuleArtifactKey,
    capability: BoxPayloadCapabilityId,
    opaque_atomic_proof: ?OpaqueAtomicProofId = null,
};

/// Return the checked module id that owns an imported nominal declaration.
pub fn importedNominalDeclarationModuleId(ref: ImportedNominalDeclarationRef) ModuleId {
    return ref.artifact;
}

/// Return the checked module id that owns an imported box payload capability.
pub fn importedBoxPayloadCapabilityModuleId(ref: ImportedBoxPayloadCapabilityRef) ModuleId {
    return ref.artifact;
}

/// Public `CheckedNominalRepresentationRef` declaration.
pub const CheckedNominalRepresentationRef = union(enum) {
    builtin: CheckedBuiltinNominal,
    local_declaration: CheckedNominalDeclarationId,
    imported_declaration: ImportedNominalDeclarationRef,
    local_box_payload_capability: LocalBoxPayloadCapabilityRef,
    imported_box_payload_capability: ImportedBoxPayloadCapabilityRef,
    opaque_without_backing,
};

/// Public `CheckedNominalType` declaration.
pub const CheckedNominalType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleNameId,
    builtin: ?CheckedBuiltinNominal = null,
    is_opaque: bool,
    backing: CheckedTypeId,
    representation: CheckedNominalRepresentationRef,
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
            if (std.meta.eql(root.key.bytes, key.bytes)) return root.id;
        }
        return null;
    }

    /// Looks up a published checked source scheme by canonical scheme key.
    pub fn schemeForKey(self: CheckedTypeStoreView, key: canonical.CanonicalTypeSchemeKey) ?CheckedTypeScheme {
        for (self.schemes) |scheme| {
            if (std.meta.eql(scheme.key.bytes, key.bytes)) return scheme;
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

    pub fn nominalDeclarationById(
        self: CheckedTypeStoreView,
        id: CheckedNominalDeclarationId,
    ) CheckedNominalDeclaration {
        const index: usize = @intFromEnum(id);
        if (index >= self.nominal_declarations.len) {
            checkedArtifactInvariant("checked nominal declaration id is out of range", .{});
        }
        const declaration = self.nominal_declarations[index];
        if (declaration.id != id) {
            checkedArtifactInvariant("checked nominal declaration id disagrees with declaration slot", .{});
        }
        return declaration;
    }
};

/// A checked type graph together with the canonical names that own its labels.
pub const CheckedTypeSourceView = struct {
    names: *const canonical.CanonicalNameStore,
    view: CheckedTypeStoreView,
};

/// Compare record field labels that may come from different canonical name stores.
pub fn recordFieldLabelsMatch(
    source_names: *const canonical.CanonicalNameStore,
    source_field: canonical.RecordFieldLabelId,
    target_names: *const canonical.CanonicalNameStore,
    target_field: canonical.RecordFieldLabelId,
) bool {
    if (source_names == target_names and source_field == target_field) return true;
    return Ident.textEql(
        source_names.recordFieldLabelText(source_field),
        target_names.recordFieldLabelText(target_field),
    );
}

/// Compare tag labels that may come from different canonical name stores.
pub fn tagLabelsMatch(
    source_names: *const canonical.CanonicalNameStore,
    source_tag: canonical.TagLabelId,
    target_names: *const canonical.CanonicalNameStore,
    target_tag: canonical.TagLabelId,
) bool {
    if (source_names == target_names and source_tag == target_tag) return true;
    return Ident.textEql(
        source_names.tagLabelText(source_tag),
        target_names.tagLabelText(target_tag),
    );
}

/// Find a record field payload child by label while walking aliases and row tails.
pub fn checkedTypeRecordFieldChild(
    source: CheckedTypeSourceView,
    root: CheckedTypeId,
    target_names: *const canonical.CanonicalNameStore,
    target_field: canonical.RecordFieldLabelId,
) ?CheckedTypeId {
    var current = root;
    while (true) {
        const payload = checkedTypeViewResolvedPayload(source.view, current) orelse return null;
        current = payload.root;
        switch (payload.payload) {
            .record => |record| {
                for (record.fields) |field| {
                    if (recordFieldLabelsMatch(source.names, field.name, target_names, target_field)) return field.ty;
                }
                current = record.ext;
            },
            .record_unbound => |fields| {
                for (fields) |field| {
                    if (recordFieldLabelsMatch(source.names, field.name, target_names, target_field)) return field.ty;
                }
                return null;
            },
            else => return null,
        }
    }
}

/// Find a tag payload child by tag label and payload index while walking aliases and row tails.
pub fn checkedTypeTagPayloadChild(
    source: CheckedTypeSourceView,
    root: CheckedTypeId,
    target_names: *const canonical.CanonicalNameStore,
    target_tag: canonical.TagLabelId,
    payload_index: u32,
) ?CheckedTypeId {
    const raw_payload_index: usize = @intCast(payload_index);
    var current = root;
    while (true) {
        const payload = checkedTypeViewResolvedPayload(source.view, current) orelse return null;
        current = payload.root;
        switch (payload.payload) {
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    if (!tagLabelsMatch(source.names, tag.name, target_names, target_tag)) continue;
                    if (raw_payload_index >= tag.args.len) return null;
                    return tag.args[raw_payload_index];
                }
                current = tag_union.ext;
            },
            else => return null,
        }
    }
}

const ResolvedCheckedTypePayload = struct {
    root: CheckedTypeId,
    payload: CheckedTypePayload,
};

fn checkedTypeViewResolvedPayload(
    view: CheckedTypeStoreView,
    root: CheckedTypeId,
) ?ResolvedCheckedTypePayload {
    var current = root;
    while (true) {
        const index: usize = @intFromEnum(current);
        if (index >= view.payloads.len) {
            checkedArtifactInvariant("checked type source child lookup referenced a missing root", .{});
        }
        switch (view.payloads[index]) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| current = nominal.backing,
            .pending => checkedArtifactInvariant("checked type source child lookup reached a pending payload", .{}),
            else => |payload| return .{
                .root = current,
                .payload = payload,
            },
        }
    }
}

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
        .flex, .rigid => |variable| variable.row_default != null,
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
    id: CheckedNominalDeclarationId,
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
        var low: usize = 0;
        var high: usize = self.source_type_roots.len;
        const needle = @intFromEnum(resolved);
        while (low < high) {
            const mid = low + (high - low) / 2;
            const entry = self.source_type_roots[mid];
            const candidate = @intFromEnum(entry.source_var);
            if (candidate == needle) return entry.checked_root;
            if (candidate < needle) {
                low = mid + 1;
            } else {
                high = mid;
            }
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
        imports: []const PublishImportArtifact,
        available: []const ImportedModuleView,
    ) Allocator.Error!CheckedTypePublication {
        const import_views = CheckedImportViews{
            .direct = imports,
            .available = available,
        };
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
        var top_level_defs = try TopLevelDefPatternIndex.init(allocator, module);
        defer top_level_defs.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            const tag = module.nodeTag(node);
            if (isExprNodeTag(tag)) {
                const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
                _ = try appendCheckedTypeRoot(allocator, module, names, import_views, &roots, &payloads, &active, module.exprType(expr_idx));
                switch (module.expr(expr_idx).data) {
                    .e_call => |call| if (call.constraint_fn_var) |constraint_fn_var| {
                        _ = try appendCheckedTypeRoot(allocator, module, names, import_views, &roots, &payloads, &active, constraint_fn_var);
                    },
                    else => {},
                }
            } else if (isPatternNodeTag(tag)) {
                const pattern_source_var = checkedPatternSourceTypeVar(module, &top_level_defs, @enumFromInt(node_idx));
                _ = try appendCheckedTypeRoot(
                    allocator,
                    module,
                    names,
                    import_views,
                    &roots,
                    &payloads,
                    &active,
                    pattern_source_var,
                );
            } else if (isStatementNodeTag(tag)) {
                const statement_idx: CIR.Statement.Idx = @enumFromInt(node_idx);
                switch (module.getStatement(statement_idx)) {
                    .s_alias_decl => _ = try appendCheckedTypeRoot(allocator, module, names, import_views, &roots, &payloads, &active, ModuleEnv.varFrom(statement_idx)),
                    .s_nominal_decl => |nominal| try appendCheckedNominalDeclarationFromStatement(
                        allocator,
                        module,
                        names,
                        import_views,
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
            const root = try appendCheckedTypeRoot(allocator, module, names, import_views, &roots, &payloads, &active, required_var);
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

        try appendStaticDispatchTypeRoots(allocator, module, names, import_views, &roots, &payloads, &active);

        for (module.allDefs()) |def_idx| {
            const root = try appendCheckedTypeRoot(allocator, module, names, import_views, &roots, &payloads, &active, module.defType(def_idx));
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
            if (std.meta.eql(root.key.bytes, key.bytes)) return root.id;
        }
        return null;
    }

    pub fn schemeForKey(self: *const CheckedTypeStore, key: canonical.CanonicalTypeSchemeKey) ?CheckedTypeScheme {
        for (self.schemes) |scheme| {
            if (std.meta.eql(scheme.key.bytes, key.bytes)) return scheme;
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

    pub fn appendSyntheticPayloadRoot(
        self: *CheckedTypeStore,
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        payload: CheckedTypePayload,
    ) Allocator.Error!CheckedTypeId {
        var owned_payload = payload;
        var payload_owned = true;
        errdefer if (payload_owned) deinitCheckedTypePayload(allocator, &owned_payload);

        const key = try checkedTypePayloadKey(allocator, names, self.roots, self.payloads, owned_payload);
        if (self.rootForKey(key)) |existing| {
            deinitCheckedTypePayload(allocator, &owned_payload);
            payload_owned = false;
            return existing;
        }

        const id: CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.roots.len)));

        const old_roots = self.roots;
        const new_roots = try allocator.alloc(CheckedTypeRoot, old_roots.len + 1);
        @memcpy(new_roots[0..old_roots.len], old_roots);
        new_roots[old_roots.len] = .{ .id = id, .key = key };
        errdefer allocator.free(new_roots);

        const old_payloads = self.payloads;
        const new_payloads = try allocator.alloc(CheckedTypePayload, old_payloads.len + 1);
        @memcpy(new_payloads[0..old_payloads.len], old_payloads);
        new_payloads[old_payloads.len] = owned_payload;
        payload_owned = false;
        errdefer {
            deinitCheckedTypePayload(allocator, &new_payloads[old_payloads.len]);
            allocator.free(new_payloads);
        }

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
        errdefer {
            deinitCheckedTypePayload(allocator, &self.payloads[index]);
            self.payloads[index] = .pending;
        }
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

    pub fn nominalDeclarationById(
        self: *const CheckedTypeStore,
        id: CheckedNominalDeclarationId,
    ) CheckedNominalDeclaration {
        return self.view().nominalDeclarationById(id);
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
                .row_default = flex.row_default,
            } },
            .rigid => |rigid| .{ .rigid = .{
                .name = if (rigid.name) |name| try allocator.dupe(u8, name) else null,
                .constraints = try self.cloneCheckedStaticDispatchConstraintsSubstituting(allocator, names, rigid.constraints, formals, actuals, active),
                .numeric_default_phase = rigid.numeric_default_phase,
                .row_default = rigid.row_default,
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
                .representation = nominal.representation,
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
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
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
    imports: CheckedImportViews,
    declarations: *std.ArrayList(CheckedNominalDeclaration),
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    statement_idx: CIR.Statement.Idx,
    header_idx: CIR.TypeHeader.Idx,
    anno_idx: CIR.TypeAnno.Idx,
    _: bool,
) Allocator.Error!void {
    if (anno_idx == .placeholder) return;

    const statement_root = try appendCheckedTypeRoot(
        allocator,
        module,
        names,
        imports,
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
                imports,
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
        imports,
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
        .representation = if (statement_nominal.builtin) |builtin_id|
            .{ .builtin = builtin_id }
        else
            .{ .local_declaration = localNominalDeclarationIdForStatement(module, statement_idx) },
        .args = formal_args,
    } };
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
    imports: CheckedImportViews,
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
                imports,
                roots,
                payloads,
                active,
                local_type_declarations,
                tag_union.tags,
            );
            errdefer deinitCheckedTags(allocator, tags);
            const ext = if (tag_union.ext) |ext_anno|
                try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, imports, roots, payloads, active, local_type_declarations, ext_anno)
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
                imports,
                roots,
                payloads,
                active,
                local_type_declarations,
                record.fields,
            );
            errdefer allocator.free(fields);
            const ext = if (record.ext) |ext_anno|
                try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, imports, roots, payloads, active, local_type_declarations, ext_anno)
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
                imports,
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
                imports,
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
                imports,
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
            imports,
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
                    imports,
                    roots,
                    payloads,
                    active,
                    ModuleEnv.varFrom(finalized),
                );
                break :blk result;
            },
            .builtin,
            .external,
            => try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, ModuleEnv.varFrom(anno_idx)),
            .pending => checkedArtifactInvariant("checked declaration template still contained pending lookup", .{}),
        },
        .apply => |apply| blk: {
            const actual_args = try checkedTypeIdsFromDeclarationAnnoSpan(
                allocator,
                module,
                names,
                imports,
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
                            imports,
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
            break :blk try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, ModuleEnv.varFrom(anno_idx));
        },
        .rigid_var,
        .rigid_var_lookup,
        .underscore,
        => try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, ModuleEnv.varFrom(anno_idx)),
        .tag,
        .malformed,
        => checkedArtifactInvariant("nominal declaration annotation was not a valid checked template", .{}),
    };
}

fn checkedTypeIdsFromDeclarationAnnoSpan(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
        out[i] = try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, imports, roots, payloads, active, local_type_declarations, anno);
    }
    return out;
}

fn checkedRecordFieldsFromDeclarationAnnoSpan(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
            .ty = try appendCheckedTypeRootFromDeclarationAnno(allocator, module, names, imports, roots, payloads, active, local_type_declarations, field.ty),
        };
    }
    return out;
}

fn checkedTagsFromDeclarationAnnoSpan(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    local_type_declarations: *const LocalTypeDeclarationIndex,
    span: CIR.TypeAnno.Span,
) Allocator.Error![]const CheckedTag {
    const annos = module.moduleEnvConst().store.sliceTypeAnnos(span);
    if (annos.len == 0) return &.{};
    const out = try allocator.alloc(CheckedTag, annos.len);
    for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
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
                imports,
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
        // The canonical key already includes the semantic formal-arg shapes.
        // The checked root ids can differ when the same nominal declaration is
        // reached through a declaration root and a use-site root.
        if (existing_nominal.name != nominal.name or
            existing_nominal.origin_module != nominal.origin_module or
            existing_nominal.is_opaque != nominal.is_opaque)
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
        .id = @enumFromInt(@as(u32, @intCast(declarations.items.len))),
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
            .record => |record| try self.writeNormalizedRecordPayload(record.fields, record.ext),
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
            .tag_union => |tag_union| try self.writeNormalizedTagUnionPayload(tag_union.tags, tag_union.ext),
            .empty_tag_union => self.writeTag("[]"),
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
            if (seen.contains(tail_id)) break;
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
            if (index > 0 and self.names.recordFieldLabelTextEql(fields.items[index - 1].name, field.name)) {
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

    fn writeNormalizedRecordPayload(
        self: *SubstitutedCheckedTypeKeyBuilder,
        head: []const CheckedRecordField,
        ext: CheckedTypeId,
    ) Allocator.Error!void {
        var fields = std.ArrayList(RecordFieldForKey).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFieldsForKey(&fields, head);

        var tail: ?CheckedTypeId = self.substitutedRoot(ext);
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();
        while (tail) |tail_id| {
            if (self.active.contains(tail_id)) break;
            if (seen.contains(tail_id)) break;
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
        if (tail == null and fields.items.len == 0) {
            self.writeTag("empty_record");
            return;
        }

        self.writeTag("record");
        self.writeU32(@intCast(fields.items.len));
        for (fields.items, 0..) |field, index| {
            if (index > 0 and self.names.recordFieldLabelTextEql(fields.items[index - 1].name, field.name)) {
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

    fn writeNormalizedTagUnionPayload(
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
            if (seen.contains(tail_id)) break;
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
        if (tail == null and tags.items.len == 0) {
            self.writeTag("[]");
            return;
        }

        self.writeTag("tag_union");
        self.writeU32(@intCast(tags.items.len));
        for (tags.items, 0..) |tag, index| {
            if (index > 0 and self.names.tagLabelTextEql(tags.items[index - 1].name, tag.name)) {
                checkedArtifactInvariant("checked type substitution key row normalization found duplicate tags", .{});
            }
            self.writeBytes(self.names.tagLabelText(tag.name));
            self.writeU32(@intCast(tag.args.len));
            for (tag.args) |arg| try self.writeType(arg);
        }
        if (tail) |tail_id| {
            try self.writeType(tail_id);
        } else {
            self.writeTag("[]");
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
        return self.names.recordFieldLabelTextLessThan(lhs.name, rhs.name);
    }

    fn tagForKeyLessThan(self: *SubstitutedCheckedTypeKeyBuilder, lhs: TagForKey, rhs: TagForKey) bool {
        return self.names.tagLabelTextLessThan(lhs.name, rhs.name);
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
        bytes = .{
            @as(u8, @truncate(value)),
            @as(u8, @truncate(value >> 8)),
            @as(u8, @truncate(value >> 16)),
            @as(u8, @truncate(value >> 24)),
        };
        self.hasher.update(&bytes);
    }
};

fn appendStaticDispatchTypeRoots(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
                _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, module.exprType(dispatch_call.receiver));
                _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, dispatch_call.constraint_fn_var);
            },
            .e_type_dispatch_call => |dispatch_call| {
                const alias_stmt = module.getStatement(dispatch_call.type_var_alias_stmt);
                _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, ModuleEnv.varFrom(alias_stmt.s_type_var_alias.type_var_anno));
                _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, dispatch_call.constraint_fn_var);
            },
            .e_method_eq => |eq| {
                _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, module.exprType(eq.lhs));
                _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, eq.constraint_fn_var);
            },
            else => unreachable,
        }
    }

    for (module.moduleEnvConst().for_loop_dispatch_plans.items.items) |plan| {
        _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, @enumFromInt(plan.iter_fn_var));
        _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, @enumFromInt(plan.next_fn_var));
    }

    for (module.moduleEnvConst().numeral_dispatch_plans.items.items) |plan| {
        _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, @enumFromInt(plan.target_var));
        _ = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, @enumFromInt(plan.fn_var));
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
    imports: CheckedImportViews,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    var_: Var,
) Allocator.Error!CheckedTypeId {
    return appendCheckedTypeRootWithRowDefault(allocator, module, names, imports, roots, payloads, active, var_, null);
}

fn appendCheckedTypeRootWithRowDefault(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    var_: Var,
    row_default: ?RowDefault,
) Allocator.Error!CheckedTypeId {
    const resolved = module.typeStoreConst().resolveVar(var_);
    const resolved_var = resolved.var_;
    if (active.get(resolved_var)) |id| {
        applyCheckedTypeRowDefault(payloads.items, id, row_default);
        return id;
    }

    const key_info = try canonical_type_keys.fromVarInfo(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        resolved_var,
    );
    if (!key_info.contains_identity_variables) {
        if (findCheckedTypeRoot(roots.items, key_info.key)) |id| {
            applyCheckedTypeRowDefault(payloads.items, id, row_default);
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
        imports,
        roots,
        payloads,
        active,
        resolved.desc.content,
    );

    deinitCheckedTypePayload(allocator, &payloads.items[@intFromEnum(id)]);
    payloads.items[@intFromEnum(id)] = payload;
    applyCheckedTypeRowDefault(payloads.items, id, row_default);
    return id;
}

fn applyCheckedTypeRowDefault(
    payloads: []CheckedTypePayload,
    id: CheckedTypeId,
    row_default: ?RowDefault,
) void {
    const default = row_default orelse return;
    const index = @intFromEnum(id);
    if (index >= payloads.len) checkedArtifactInvariant("checked row default referenced a missing type payload", .{});
    switch (payloads[index]) {
        .pending => {},
        .flex => |*variable| setCheckedTypeVariableRowDefault(variable, default),
        .rigid => |*variable| setCheckedTypeVariableRowDefault(variable, default),
        else => {},
    }
}

fn setCheckedTypeVariableRowDefault(variable: *CheckedTypeVariable, row_default: RowDefault) void {
    if (variable.constraints.len != 0) {
        checkedArtifactInvariant("checked row default was assigned to a constrained type variable", .{});
    }
    if (variable.row_default) |existing| {
        if (existing != row_default) checkedArtifactInvariant("checked row variable received incompatible defaults", .{});
        return;
    }
    variable.row_default = row_default;
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
    std.mem.sort(CheckedSourceTypeRoot, out, {}, checkedSourceTypeRootLessThan);
    return out;
}

fn checkedSourceTypeRootLessThan(_: void, a: CheckedSourceTypeRoot, b: CheckedSourceTypeRoot) bool {
    return @intFromEnum(a.source_var) < @intFromEnum(b.source_var);
}

fn copyCheckedTypePayload(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
            .constraints = try copyCheckedStaticDispatchConstraints(allocator, module, names, imports, roots, payloads, active, flex.constraints),
            .numeric_default_phase = numericDefaultPhaseForFlex(module, flex),
            .row_default = null,
        } },
        .rigid => |rigid| .{ .rigid = .{
            .name = try copyIdentText(allocator, module, rigid.name),
            .constraints = try copyCheckedStaticDispatchConstraints(allocator, module, names, imports, roots, payloads, active, rigid.constraints),
            .numeric_default_phase = numericDefaultPhaseForConstraints(module, rigid.constraints),
            .row_default = null,
        } },
        .alias => |alias| .{ .alias = .{
            .name = try names.internTypeIdent(module.identStoreConst(), alias.ident.ident_idx),
            .origin_module = try names.internModuleIdent(module.identStoreConst(), alias.origin_module),
            .backing = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().getAliasBackingVar(alias)),
            .args = try copyCheckedTypeRange(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().sliceAliasArgs(alias)),
        } },
        .structure => |flat| try copyCheckedFlatType(allocator, module, names, imports, roots, payloads, active, flat),
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
    imports: CheckedImportViews,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    flat: types.FlatType,
) Allocator.Error!CheckedTypePayload {
    return switch (flat) {
        .empty_record => .empty_record,
        .empty_tag_union => .empty_tag_union,
        .record_unbound => |fields| .{
            .record_unbound = try copyCheckedRecordFields(allocator, module, names, imports, roots, payloads, active, fields),
        },
        .record => |record| .{ .record = .{
            .fields = try copyCheckedRecordFields(allocator, module, names, imports, roots, payloads, active, record.fields),
            .ext = try appendCheckedTypeRootWithRowDefault(allocator, module, names, imports, roots, payloads, active, record.ext, .empty_record),
        } },
        .tuple => |tuple| .{
            .tuple = try copyCheckedTypeRange(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().sliceVars(tuple.elems)),
        },
        .nominal_type => |nominal| blk: {
            const builtin_nominal = categorizeBuiltinNominal(module, nominal);
            break :blk .{ .nominal = .{
                .name = try names.internTypeIdent(module.identStoreConst(), nominal.ident.ident_idx),
                .origin_module = try names.internModuleIdent(module.identStoreConst(), nominal.origin_module),
                .builtin = builtin_nominal,
                .is_opaque = nominal.is_opaque,
                .backing = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().getNominalBackingVar(nominal)),
                .representation = try checkedNominalRepresentationForSourceNominal(module, names, imports, nominal, builtin_nominal),
                .args = try copyCheckedTypeRange(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().sliceNominalArgs(nominal)),
            } };
        },
        .fn_pure => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, imports, roots, payloads, active, .pure, func) },
        .fn_effectful => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, imports, roots, payloads, active, .effectful, func) },
        .fn_unbound => |func| .{ .function = try copyCheckedFunctionType(allocator, module, names, imports, roots, payloads, active, .pure, func) },
        .tag_union => |tag_union| .{ .tag_union = .{
            .tags = try copyCheckedTags(allocator, module, names, imports, roots, payloads, active, tag_union.tags),
            .ext = try appendCheckedTypeRootWithRowDefault(allocator, module, names, imports, roots, payloads, active, tag_union.ext, .empty_tag_union),
        } },
    };
}

fn copyCheckedFunctionType(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    kind: CheckedFunctionKind,
    func: types.Func,
) Allocator.Error!CheckedFunctionType {
    return .{
        .kind = finalizedFunctionKind(kind),
        .args = try copyCheckedTypeRange(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().sliceVars(func.args)),
        .ret = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, func.ret),
        .needs_instantiation = func.needs_instantiation,
    };
}

fn copyCheckedTypeRange(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    roots: *std.ArrayList(CheckedTypeRoot),
    payloads: *std.ArrayList(CheckedTypePayload),
    active: *std.AutoHashMap(Var, CheckedTypeId),
    vars: []const Var,
) Allocator.Error![]const CheckedTypeId {
    if (vars.len == 0) return &.{};
    const out = try allocator.alloc(CheckedTypeId, vars.len);
    errdefer allocator.free(out);
    for (vars, 0..) |var_, i| {
        out[i] = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, var_);
    }
    return out;
}

fn copyCheckedRecordFields(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
            .ty = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, field_var),
        };
    }
    return out;
}

fn copyCheckedTags(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
    for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
    errdefer {
        for (out[0..tag_names.len]) |tag| allocator.free(tag.args);
        allocator.free(out);
    }
    for (tag_names, tag_args, 0..) |tag_name, arg_range, i| {
        out[i] = .{
            .name = try names.internTagIdent(module.identStoreConst(), tag_name),
            .args = try copyCheckedTypeRange(allocator, module, names, imports, roots, payloads, active, module.typeStoreConst().sliceVars(arg_range)),
        };
    }
    return out;
}

fn copyCheckedStaticDispatchConstraints(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
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
            .fn_ty = try appendCheckedTypeRoot(allocator, module, names, imports, roots, payloads, active, constraint.fn_var),
            .origin = constraint.origin,
            .binop_negated = constraint.binop_negated,
            .num_literal = constraint.num_literal,
        };
    }
    return out;
}

fn categorizeBuiltinNominal(module: TypedCIR.Module, nominal: types.NominalType) ?CheckedBuiltinNominal {
    const common = module.moduleEnvConst().idents;
    const is_builtin_origin = checkedIdentTextEql(module, nominal.origin_module, common.builtin_module);
    if (!is_builtin_origin) return null;

    const ident = nominal.ident.ident_idx;
    if (checkedIdentTextEql(module, ident, common.bool) or checkedIdentTextEql(module, ident, common.bool_type)) return .bool;
    if (checkedIdentTextEql(module, ident, common.str) or checkedIdentTextEql(module, ident, common.builtin_str)) return .str;
    if (checkedIdentTextEql(module, ident, common.u8) or checkedIdentTextEql(module, ident, common.u8_type)) return .u8;
    if (checkedIdentTextEql(module, ident, common.i8) or checkedIdentTextEql(module, ident, common.i8_type)) return .i8;
    if (checkedIdentTextEql(module, ident, common.u16) or checkedIdentTextEql(module, ident, common.u16_type)) return .u16;
    if (checkedIdentTextEql(module, ident, common.i16) or checkedIdentTextEql(module, ident, common.i16_type)) return .i16;
    if (checkedIdentTextEql(module, ident, common.u32) or checkedIdentTextEql(module, ident, common.u32_type)) return .u32;
    if (checkedIdentTextEql(module, ident, common.i32) or checkedIdentTextEql(module, ident, common.i32_type)) return .i32;
    if (checkedIdentTextEql(module, ident, common.u64) or checkedIdentTextEql(module, ident, common.u64_type)) return .u64;
    if (checkedIdentTextEql(module, ident, common.i64) or checkedIdentTextEql(module, ident, common.i64_type)) return .i64;
    if (checkedIdentTextEql(module, ident, common.u128) or checkedIdentTextEql(module, ident, common.u128_type)) return .u128;
    if (checkedIdentTextEql(module, ident, common.i128) or checkedIdentTextEql(module, ident, common.i128_type)) return .i128;
    if (checkedIdentTextEql(module, ident, common.f32) or checkedIdentTextEql(module, ident, common.f32_type)) return .f32;
    if (checkedIdentTextEql(module, ident, common.f64) or checkedIdentTextEql(module, ident, common.f64_type)) return .f64;
    if (checkedIdentTextEql(module, ident, common.dec) or checkedIdentTextEql(module, ident, common.dec_type)) return .dec;
    if (checkedIdentTextEql(module, ident, common.list)) return .list;
    if (checkedIdentTextEql(module, ident, common.box)) return .box;
    return null;
}

fn checkedIdentTextEql(module: TypedCIR.Module, a: base.Ident.Idx, b: base.Ident.Idx) bool {
    return a.eql(b) or module.identStoreConst().idxTextEql(a, b);
}

fn checkedNominalRepresentationForSourceNominal(
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    nominal: types.NominalType,
    builtin_nominal: ?CheckedBuiltinNominal,
) Allocator.Error!CheckedNominalRepresentationRef {
    if (builtin_nominal) |builtin_id| return .{ .builtin = builtin_id };

    const origin_module = try names.internModuleIdent(module.identStoreConst(), nominal.origin_module);
    const type_name = try names.internTypeIdent(module.identStoreConst(), nominal.ident.ident_idx);
    const current_module = try names.internModuleIdent(module.identStoreConst(), module.qualifiedModuleIdent());
    if (origin_module == current_module) {
        return .{ .local_declaration = localNominalDeclarationIdForIdent(module, nominal.ident.ident_idx) };
    }

    return .{ .imported_declaration = importedNominalDeclarationRefForSourceNominal(
        names,
        imports,
        origin_module,
        type_name,
    ) };
}

fn importedNominalDeclarationRefForSourceNominal(
    names: *const canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    origin_module: canonical.ModuleNameId,
    type_name: canonical.TypeNameId,
) ImportedNominalDeclarationRef {
    const origin_text = names.moduleNameText(origin_module);
    const type_text = names.typeNameText(type_name);
    var found: ?ImportedNominalDeclarationRef = null;

    for (imports.direct) |import| {
        if (!importedViewModuleNameMatches(import.view, origin_text)) continue;
        for (import.view.checked_types.nominal_declarations) |declaration| {
            if (!Ident.textEql(import.view.canonical_names.typeNameText(declaration.nominal.type_name), type_text)) continue;
            const next = ImportedNominalDeclarationRef{
                .artifact = import.key,
                .declaration = declaration.id,
            };
            if (found) |existing| {
                if (!checkedArtifactKeyEql(existing.artifact, next.artifact) or existing.declaration != next.declaration) {
                    checkedArtifactInvariant("checked nominal representation import declaration resolution was ambiguous", .{});
                }
            } else {
                found = next;
            }
        }
    }
    for (imports.available) |view| {
        if (!importedViewModuleNameMatches(view, origin_text)) continue;
        for (view.checked_types.nominal_declarations) |declaration| {
            if (!Ident.textEql(view.canonical_names.typeNameText(declaration.nominal.type_name), type_text)) continue;
            const next = ImportedNominalDeclarationRef{
                .artifact = view.key,
                .declaration = declaration.id,
            };
            if (found) |existing| {
                if (!checkedArtifactKeyEql(existing.artifact, next.artifact) or existing.declaration != next.declaration) {
                    checkedArtifactInvariant("checked nominal representation import declaration resolution was ambiguous", .{});
                }
            } else {
                found = next;
            }
        }
    }

    return found orelse checkedArtifactInvariant("checked nominal representation referenced a missing imported nominal declaration", .{});
}

fn localNominalDeclarationIdForIdent(
    module: TypedCIR.Module,
    ident: Ident.Idx,
) CheckedNominalDeclarationId {
    var next_id: u32 = 0;
    var node_idx: u32 = 0;
    while (node_idx < module.nodeCount()) : (node_idx += 1) {
        const node: CIR.Node.Idx = @enumFromInt(node_idx);
        if (!isStatementNodeTag(module.nodeTag(node))) continue;
        const statement_idx: CIR.Statement.Idx = @enumFromInt(node_idx);
        const nominal = switch (module.getStatement(statement_idx)) {
            .s_nominal_decl => |nominal| nominal,
            else => continue,
        };
        if (nominal.anno == .placeholder) continue;
        const header = module.moduleEnvConst().store.getTypeHeader(nominal.header);
        const id: CheckedNominalDeclarationId = @enumFromInt(next_id);
        next_id += 1;
        if (header.relative_name.eql(ident) or module.identStoreConst().idxTextEql(header.relative_name, ident)) return id;
    }
    checkedArtifactInvariant("checked nominal representation referenced a missing local nominal declaration", .{});
}

fn localNominalDeclarationIdForStatement(
    module: TypedCIR.Module,
    statement_idx: CIR.Statement.Idx,
) CheckedNominalDeclarationId {
    var next_id: u32 = 0;
    var node_idx: u32 = 0;
    while (node_idx < module.nodeCount()) : (node_idx += 1) {
        const node: CIR.Node.Idx = @enumFromInt(node_idx);
        if (!isStatementNodeTag(module.nodeTag(node))) continue;
        const candidate: CIR.Statement.Idx = @enumFromInt(node_idx);
        const nominal = switch (module.getStatement(candidate)) {
            .s_nominal_decl => |nominal| nominal,
            else => continue,
        };
        if (nominal.anno == .placeholder) continue;
        const id: CheckedNominalDeclarationId = @enumFromInt(next_id);
        next_id += 1;
        if (candidate == statement_idx) return id;
    }
    checkedArtifactInvariant("checked nominal declaration statement had no declaration id", .{});
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
        if (std.meta.eql(root.key.bytes, key.bytes)) return root.id;
    }
    return null;
}

fn findCheckedTypeScheme(schemes: []const CheckedTypeScheme, key: canonical.CanonicalTypeSchemeKey) ?CheckedTypeSchemeId {
    for (schemes) |scheme| {
        if (std.meta.eql(scheme.key.bytes, key.bytes)) return scheme.id;
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
    reassignable: bool,
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
    reassign: struct { pattern: CheckedPatternId, expr: CheckedExprId, reassigned_binders: []const PatternBinderId },
    crash: CheckedStringLiteralId,
    dbg: CheckedExprId,
    expr: CheckedExprId,
    expect: CheckedExprId,
    for_: struct {
        pattern: CheckedPatternId,
        expr: CheckedExprId,
        body: CheckedExprId,
        plan: ?static_dispatch.IteratorForPlanId,
    },
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
    num_from_numeral: ?StaticDispatchPlanId,
    typed_int: struct {
        value: CIR.IntValue,
        type_name: canonical.TypeNameId,
    },
    typed_frac: struct {
        value: CIR.IntValue,
        type_name: canonical.TypeNameId,
    },
    typed_num_from_numeral: ?StaticDispatchPlanId,
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
        direct_target: ?ResolvedValueId = null,
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
        plan: ?static_dispatch.IteratorForPlanId,
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
    diverges: bool,
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
    diverges: bool,
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

        var top_level_defs = try TopLevelDefPatternIndex.init(allocator, module);
        defer top_level_defs.deinit(allocator);

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
                    .diverges = false,
                    .source_region = module.regionAt(node),
                    .data = .pending,
                });
                expr_by_node[node_idx] = id;
            } else if (isPatternNodeTag(tag)) {
                const pattern_idx: CIR.Pattern.Idx = @enumFromInt(node_idx);
                const ty = checked_types.rootForSourceVar(module, checkedPatternSourceTypeVar(module, &top_level_defs, pattern_idx)) orelse {
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
                    .diverges = false,
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

        try publishCheckedBodyDivergence(allocator, exprs.items, statements.items);

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

    pub fn patternBinderIsReassignable(self: *const CheckedBodyStore, binder: PatternBinderId) bool {
        const raw = @intFromEnum(binder);
        if (raw >= self.pattern_binders.len) checkedArtifactInvariant("checked artifact invariant violated: pattern binder id out of range", .{});
        return self.pattern_binders[raw].reassignable;
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

    pub fn attachIteratorForPlans(
        self: *CheckedBodyStore,
        plans: *const static_dispatch.StaticDispatchPlanTable,
    ) void {
        var iter = plans.iterator_for_by_node.iterator();
        while (iter.next()) |entry| {
            const raw_node = @intFromEnum(entry.key_ptr.*);

            if (raw_node < self.expr_by_node.len) {
                if (self.expr_by_node[raw_node]) |checked_expr| {
                    const data = &self.exprs[@intFromEnum(checked_expr)].data;
                    switch (data.*) {
                        .for_ => |*for_| for_.plan = entry.value_ptr.*,
                        else => checkedArtifactInvariant(
                            "iterator-for plan {d} points at non-for checked expression {d}",
                            .{ @intFromEnum(entry.value_ptr.*), @intFromEnum(checked_expr) },
                        ),
                    }
                    continue;
                }
            }

            if (raw_node < self.statement_by_node.len) {
                if (self.statement_by_node[raw_node]) |checked_statement| {
                    const data = &self.statements[@intFromEnum(checked_statement)].data;
                    switch (data.*) {
                        .for_ => |*for_| for_.plan = entry.value_ptr.*,
                        else => checkedArtifactInvariant(
                            "iterator-for plan {d} points at non-for checked statement {d}",
                            .{ @intFromEnum(entry.value_ptr.*), @intFromEnum(checked_statement) },
                        ),
                    }
                    continue;
                }
            }

            checkedArtifactInvariant(
                "iterator-for plan {d} points at source node {d} with no checked loop",
                .{ @intFromEnum(entry.value_ptr.*), raw_node },
            );
        }
    }

    pub fn attachNumeralPlans(
        self: *CheckedBodyStore,
        plans: *const static_dispatch.StaticDispatchPlanTable,
    ) void {
        var iter = plans.numeral_by_node.iterator();
        while (iter.next()) |entry| {
            const raw_node = @intFromEnum(entry.key_ptr.*);
            if (raw_node >= self.expr_by_node.len) {
                checkedArtifactInvariant(
                    "from_numeral plan {d} points at source node {d} outside checked expression table",
                    .{ @intFromEnum(entry.value_ptr.*), raw_node },
                );
            }
            const checked_expr = self.expr_by_node[raw_node] orelse {
                checkedArtifactInvariant(
                    "from_numeral plan {d} points at source node {d} with no checked expression",
                    .{ @intFromEnum(entry.value_ptr.*), raw_node },
                );
            };
            const data = &self.exprs[@intFromEnum(checked_expr)].data;
            switch (data.*) {
                .num_from_numeral => data.* = .{ .num_from_numeral = entry.value_ptr.* },
                .typed_num_from_numeral => data.* = .{ .typed_num_from_numeral = entry.value_ptr.* },
                .num,
                .typed_int,
                .frac_f32,
                .frac_f64,
                .dec,
                .dec_small,
                .typed_frac,
                => {},
                else => checkedArtifactInvariant(
                    "from_numeral plan {d} points at non-numeral checked expression {d}",
                    .{ @intFromEnum(entry.value_ptr.*), @intFromEnum(checked_expr) },
                ),
            }
        }
    }

    pub fn attachResolvedValueRefs(
        self: *CheckedBodyStore,
        refs: *const ResolvedValueRefTable,
        local_module: CheckedModuleArtifactKey,
        local_procedure_bindings: *const TopLevelProcedureBindingTable,
        imports: []const PublishImportArtifact,
        relation_modules: []const ImportedModuleView,
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

        for (self.exprs) |*checked_expr| {
            switch (checked_expr.data) {
                .call => |*call| {
                    call.direct_target = directProcedureTargetForCall(
                        refs,
                        call.func,
                        local_module,
                        local_procedure_bindings,
                        imports,
                        relation_modules,
                    );
                },
                else => {},
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

const DivergenceVisitState = enum { fresh, active, done };

fn publishCheckedBodyDivergence(
    allocator: Allocator,
    exprs: []CheckedExpr,
    statements: []CheckedStatement,
) Allocator.Error!void {
    const expr_states = try allocator.alloc(DivergenceVisitState, exprs.len);
    defer allocator.free(expr_states);
    const statement_states = try allocator.alloc(DivergenceVisitState, statements.len);
    defer allocator.free(statement_states);

    @memset(expr_states, .fresh);
    @memset(statement_states, .fresh);

    for (exprs) |*expr| {
        expr.diverges = checkedExprDiverges(exprs, statements, expr.id, expr_states, statement_states);
    }
    for (statements) |*statement| {
        statement.diverges = checkedStatementDiverges(exprs, statements, statement.id, expr_states, statement_states);
    }
}

fn checkedExprDiverges(
    exprs: []CheckedExpr,
    statements: []CheckedStatement,
    expr_id: CheckedExprId,
    expr_states: []DivergenceVisitState,
    statement_states: []DivergenceVisitState,
) bool {
    const index = @intFromEnum(expr_id);
    if (index >= exprs.len) checkedArtifactInvariant("checked divergence referenced a missing expression", .{});
    switch (expr_states[index]) {
        .done => return exprs[index].diverges,
        .active => checkedArtifactInvariant("checked expression divergence contains a cycle", .{}),
        .fresh => {},
    }
    expr_states[index] = .active;
    const result = checkedExprDataDiverges(exprs, statements, exprs[index].data, expr_states, statement_states);
    exprs[index].diverges = result;
    expr_states[index] = .done;
    return result;
}

fn checkedStatementDiverges(
    exprs: []CheckedExpr,
    statements: []CheckedStatement,
    statement_id: CheckedStatementId,
    expr_states: []DivergenceVisitState,
    statement_states: []DivergenceVisitState,
) bool {
    const index = @intFromEnum(statement_id);
    if (index >= statements.len) checkedArtifactInvariant("checked divergence referenced a missing statement", .{});
    switch (statement_states[index]) {
        .done => return statements[index].diverges,
        .active => checkedArtifactInvariant("checked statement divergence contains a cycle", .{}),
        .fresh => {},
    }
    statement_states[index] = .active;
    const result = checkedStatementDataDiverges(exprs, statements, statements[index].data, expr_states, statement_states);
    statements[index].diverges = result;
    statement_states[index] = .done;
    return result;
}

fn checkedExprDataDiverges(
    exprs: []CheckedExpr,
    statements: []CheckedStatement,
    data: CheckedExprData,
    expr_states: []DivergenceVisitState,
    statement_states: []DivergenceVisitState,
) bool {
    return switch (data) {
        .crash,
        .return_,
        => true,
        .str => |items| checkedAnyExprDiverges(exprs, statements, items, expr_states, statement_states),
        .list => |items| checkedAnyExprDiverges(exprs, statements, items, expr_states, statement_states),
        .tuple => |items| checkedAnyExprDiverges(exprs, statements, items, expr_states, statement_states),
        .match_ => |match| blk: {
            if (checkedExprDiverges(exprs, statements, match.cond, expr_states, statement_states)) break :blk true;
            if (match.branches.len == 0) break :blk false;
            for (match.branches) |branch| {
                if (branch.guard != null) break :blk false;
                if (!checkedExprDiverges(exprs, statements, branch.value, expr_states, statement_states)) break :blk false;
            }
            break :blk true;
        },
        .if_ => |if_| blk: {
            if (if_.branches.len > 0 and checkedExprDiverges(exprs, statements, if_.branches[0].cond, expr_states, statement_states)) {
                break :blk true;
            }
            for (if_.branches) |branch| {
                if (!checkedExprDiverges(exprs, statements, branch.body, expr_states, statement_states)) break :blk false;
            }
            break :blk checkedExprDiverges(exprs, statements, if_.final_else, expr_states, statement_states);
        },
        .call => |call| blk: {
            if (checkedExprDiverges(exprs, statements, call.func, expr_states, statement_states)) break :blk true;
            break :blk checkedAnyExprDiverges(exprs, statements, call.args, expr_states, statement_states);
        },
        .record => |record| blk: {
            if (record.ext) |ext| {
                if (checkedExprDiverges(exprs, statements, ext, expr_states, statement_states)) break :blk true;
            }
            for (record.fields) |field| {
                if (checkedExprDiverges(exprs, statements, field.value, expr_states, statement_states)) break :blk true;
            }
            break :blk false;
        },
        .block => |block| blk: {
            for (block.statements) |statement| {
                if (checkedStatementDiverges(exprs, statements, statement, expr_states, statement_states)) break :blk true;
            }
            break :blk checkedExprDiverges(exprs, statements, block.final_expr, expr_states, statement_states);
        },
        .tag => |tag| checkedAnyExprDiverges(exprs, statements, tag.args, expr_states, statement_states),
        .nominal => |nominal| checkedExprDiverges(exprs, statements, nominal.backing_expr, expr_states, statement_states),
        .closure => false,
        .lambda => false,
        .binop => |binop| checkedExprDiverges(exprs, statements, binop.lhs, expr_states, statement_states) or
            checkedExprDiverges(exprs, statements, binop.rhs, expr_states, statement_states),
        .unary_minus,
        .unary_not,
        .dbg,
        .expect,
        => |child| checkedExprDiverges(exprs, statements, child, expr_states, statement_states),
        .field_access => |field| checkedExprDiverges(exprs, statements, field.receiver, expr_states, statement_states),
        .structural_eq => |eq| checkedExprDiverges(exprs, statements, eq.lhs, expr_states, statement_states) or
            checkedExprDiverges(exprs, statements, eq.rhs, expr_states, statement_states),
        .tuple_access => |access| checkedExprDiverges(exprs, statements, access.tuple, expr_states, statement_states),
        .for_ => |for_| checkedExprDiverges(exprs, statements, for_.expr, expr_states, statement_states),
        .hosted_lambda => false,
        .run_low_level => |run| checkedAnyExprDiverges(exprs, statements, run.args, expr_states, statement_states),
        .pending,
        .num,
        .frac_f32,
        .frac_f64,
        .dec,
        .dec_small,
        .num_from_numeral,
        .typed_int,
        .typed_frac,
        .typed_num_from_numeral,
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
        .ellipsis,
        .anno_only,
        => false,
    };
}

fn checkedStatementDataDiverges(
    exprs: []CheckedExpr,
    statements: []CheckedStatement,
    data: CheckedStatementData,
    expr_states: []DivergenceVisitState,
    statement_states: []DivergenceVisitState,
) bool {
    return switch (data) {
        .crash,
        .break_,
        .return_,
        => true,
        .decl => |decl| checkedExprDiverges(exprs, statements, decl.expr, expr_states, statement_states),
        .var_ => |var_| checkedExprDiverges(exprs, statements, var_.expr, expr_states, statement_states),
        .reassign => |reassign| checkedExprDiverges(exprs, statements, reassign.expr, expr_states, statement_states),
        .dbg,
        .expr,
        .expect,
        => |expr| checkedExprDiverges(exprs, statements, expr, expr_states, statement_states),
        .for_ => |for_| checkedExprDiverges(exprs, statements, for_.expr, expr_states, statement_states),
        .while_ => |while_| checkedExprDiverges(exprs, statements, while_.cond, expr_states, statement_states),
        .pending,
        .import_,
        .alias_decl,
        .nominal_decl,
        .type_anno,
        .type_var_alias,
        .runtime_error,
        => false,
    };
}

fn checkedAnyExprDiverges(
    exprs: []CheckedExpr,
    statements: []CheckedStatement,
    items: []const CheckedExprId,
    expr_states: []DivergenceVisitState,
    statement_states: []DivergenceVisitState,
) bool {
    for (items) |item| {
        if (checkedExprDiverges(exprs, statements, item, expr_states, statement_states)) return true;
    }
    return false;
}

fn directProcedureTargetForCall(
    refs: *const ResolvedValueRefTable,
    callee: CheckedExprId,
    local_module: CheckedModuleArtifactKey,
    local_procedure_bindings: *const TopLevelProcedureBindingTable,
    imports: []const PublishImportArtifact,
    relation_modules: []const ImportedModuleView,
) ?ResolvedValueId {
    const ref_id = refs.lookupIdByCheckedExpr(callee) orelse return null;
    const raw = @intFromEnum(ref_id);
    if (raw >= refs.records.len) {
        checkedArtifactInvariant("checked direct call target referenced a missing resolved value", .{});
    }
    return if (resolvedValueCanBeCalledDirectly(
        refs.records[raw].ref,
        local_module,
        local_procedure_bindings,
        imports,
        relation_modules,
    )) ref_id else null;
}

fn resolvedValueCanBeCalledDirectly(
    ref: ResolvedValueRef,
    local_module: CheckedModuleArtifactKey,
    local_procedure_bindings: *const TopLevelProcedureBindingTable,
    imports: []const PublishImportArtifact,
    relation_modules: []const ImportedModuleView,
) bool {
    return switch (ref) {
        .local_proc, .hosted_proc => true,
        .top_level_proc, .promoted_top_level_proc => |proc| procedureUseCanBeCalledDirectly(
            proc,
            local_module,
            local_procedure_bindings,
            imports,
            relation_modules,
        ),
        .imported_proc => |proc| procedureUseCanBeCalledDirectly(
            proc,
            local_module,
            local_procedure_bindings,
            imports,
            relation_modules,
        ),
        .platform_required_proc => |required| procedureUseCanBeCalledDirectly(
            required.procedure,
            local_module,
            local_procedure_bindings,
            imports,
            relation_modules,
        ),
        .local_param,
        .local_value,
        .local_mutable_version,
        .pattern_binder,
        .top_level_const,
        .imported_const,
        .platform_required_declaration,
        .platform_required_const,
        => false,
    };
}

fn procedureUseCanBeCalledDirectly(
    proc: ProcedureUseTemplate,
    local_module: CheckedModuleArtifactKey,
    local_procedure_bindings: *const TopLevelProcedureBindingTable,
    imports: []const PublishImportArtifact,
    relation_modules: []const ImportedModuleView,
) bool {
    return switch (proc.binding) {
        .top_level => |top_level| topLevelProcedureCanBeCalledDirectly(
            top_level,
            local_module,
            local_procedure_bindings,
            imports,
            relation_modules,
        ),
        .imported => |imported| importedProcedureCanBeCalledDirectly(imported, imports, relation_modules),
        .hosted => true,
        .platform_required => |required| topLevelProcedureCanBeCalledDirectly(
            .{ .artifact = required.artifact, .binding = required.procedure_binding },
            local_module,
            local_procedure_bindings,
            imports,
            relation_modules,
        ),
    };
}

fn topLevelProcedureCanBeCalledDirectly(
    top_level: ArtifactTopLevelProcedureBindingRef,
    local_module: CheckedModuleArtifactKey,
    local_procedure_bindings: *const TopLevelProcedureBindingTable,
    imports: []const PublishImportArtifact,
    relation_modules: []const ImportedModuleView,
) bool {
    const body = if (checkedArtifactKeyEql(top_level.artifact, local_module))
        local_procedure_bindings.get(top_level.binding).body
    else blk: {
        const view = moduleViewForKey(imports, relation_modules, top_level.artifact) orelse
            checkedArtifactInvariant("direct-call target referenced an unavailable checked module", .{});
        break :blk view.top_level_procedure_bindings.get(top_level.binding).body;
    };
    return procedureBodyCanBeCalledDirectly(body);
}

fn importedProcedureCanBeCalledDirectly(
    imported: ImportedProcedureBindingRef,
    imports: []const PublishImportArtifact,
    relation_modules: []const ImportedModuleView,
) bool {
    const view = moduleViewForKey(imports, relation_modules, imported.artifact) orelse
        checkedArtifactInvariant("imported direct-call target referenced an unavailable checked module", .{});
    for (view.exported_procedure_bindings.bindings) |binding| {
        if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
            return importedProcedureBodyCanBeCalledDirectly(binding.body);
        }
    }
    checkedArtifactInvariant("imported direct-call target was not exported by its checked module", .{});
}

fn moduleViewForKey(
    imports: []const PublishImportArtifact,
    relation_modules: []const ImportedModuleView,
    key: CheckedModuleArtifactKey,
) ?ImportedModuleView {
    for (imports) |import| {
        if (checkedArtifactKeyEql(import.key, key)) return import.view;
    }
    for (relation_modules) |view| {
        if (checkedArtifactKeyEql(view.key, key)) return view;
    }
    return null;
}

fn procedureBodyCanBeCalledDirectly(body: ProcedureBindingBody) bool {
    return switch (body) {
        .direct_template => true,
        .callable_eval_template => false,
    };
}

fn importedProcedureBodyCanBeCalledDirectly(body: ImportedProcedureBindingBody) bool {
    return switch (body) {
        .direct_template => true,
        .callable_eval_template => false,
    };
}

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
            .e_num => |num| self.copyIntLiteral(expr_idx, num.value, num.kind),
            .e_frac_f32 => |frac| self.copyFracLiteral(expr_idx, .{ .f32 = frac.value }, frac.has_suffix),
            .e_frac_f64 => |frac| self.copyFracLiteral(expr_idx, .{ .f64 = frac.value }, frac.has_suffix),
            .e_dec => |dec| self.copyFracLiteral(expr_idx, .{ .dec = dec.value }, dec.has_suffix),
            .e_dec_small => |dec| self.copyFracLiteral(expr_idx, .{ .small = dec.value }, dec.has_suffix),
            .e_num_from_numeral => .{ .num_from_numeral = null },
            .e_typed_int => |typed| try self.copyTypedIntLiteral(expr_idx, typed.value, typed.type_name),
            .e_typed_frac => |typed| try self.copyTypedFracLiteral(expr_idx, typed.value, typed.type_name),
            .e_typed_num_from_numeral => .{ .typed_num_from_numeral = null },
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
                .plan = null,
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

    fn copyTypedIntLiteral(
        self: *@This(),
        expr_idx: CIR.Expr.Idx,
        value: CIR.IntValue,
        type_name: Ident.Idx,
    ) Allocator.Error!CheckedExprData {
        if (self.checkedBuiltinForExpr(expr_idx) == null) return .{ .typed_num_from_numeral = null };
        return .{ .typed_int = .{
            .value = value,
            .type_name = try self.names.internTypeIdent(self.module.identStoreConst(), type_name),
        } };
    }

    fn copyTypedFracLiteral(
        self: *@This(),
        expr_idx: CIR.Expr.Idx,
        value: CIR.IntValue,
        type_name: Ident.Idx,
    ) Allocator.Error!CheckedExprData {
        if (self.checkedBuiltinForExpr(expr_idx) == null) return .{ .typed_num_from_numeral = null };
        const builtin_nominal = self.checkedBuiltinForExpr(expr_idx) orelse return .{ .typed_frac = .{
            .value = value,
            .type_name = try self.names.internTypeIdent(self.module.identStoreConst(), type_name),
        } };
        switch (builtin_nominal) {
            .f32 => return .{ .frac_f32 = .{ .value = try self.floatLiteralForExpr(f32, expr_idx), .has_suffix = true } },
            .f64 => return .{ .frac_f64 = .{ .value = try self.floatLiteralForExpr(f64, expr_idx), .has_suffix = true } },
            else => {},
        }
        return fracLiteralForBuiltin(.{ .scaled_dec = value }, builtin_nominal, true) orelse .{ .typed_frac = .{
            .value = value,
            .type_name = try self.names.internTypeIdent(self.module.identStoreConst(), type_name),
        } };
    }

    fn copyIntLiteral(
        self: *@This(),
        expr_idx: CIR.Expr.Idx,
        value: CIR.IntValue,
        kind: CIR.NumKind,
    ) CheckedExprData {
        const literal_builtin = self.checkedBuiltinForExpr(expr_idx);
        if (literal_builtin == null) return .{ .num_from_numeral = null };
        return .{ .num = .{ .value = value, .kind = kind } };
    }

    fn copyFracLiteral(
        self: *@This(),
        expr_idx: CIR.Expr.Idx,
        literal: FracLit,
        has_suffix: bool,
    ) CheckedExprData {
        if (self.checkedBuiltinForExpr(expr_idx) == null) return .{ .num_from_numeral = null };
        const builtin_nominal = self.checkedBuiltinForExpr(expr_idx) orelse return originalFracLiteral(literal, has_suffix);
        return fracLiteralForBuiltin(literal, builtin_nominal, has_suffix) orelse originalFracLiteral(literal, has_suffix);
    }

    fn checkedBuiltinForExpr(self: *@This(), expr_idx: CIR.Expr.Idx) ?CheckedBuiltinNominal {
        const checked_ty = self.checked_types.rootForSourceVar(self.module, self.module.exprType(expr_idx)) orelse {
            checkedArtifactInvariant("checked numeric expression type root was not published", .{});
        };
        return checkedBuiltinForLiteralTarget(self.checked_types.store.view(), checked_ty);
    }

    fn floatLiteralForExpr(self: *@This(), comptime Float: type, expr_idx: CIR.Expr.Idx) Allocator.Error!Float {
        const literal = self.module.moduleEnvConst().numeralLiteralForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse {
            checkedArtifactInvariant("checked typed float literal had no parser-owned numeral facts", .{});
        };
        const text = try numeralLiteralDecimalText(self.allocator, self.module.moduleEnvConst(), literal);
        defer self.allocator.free(text);
        return std.fmt.parseFloat(Float, text) catch {
            checkedArtifactInvariant("checked typed float literal could not be converted from parser-owned numeral facts", .{});
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
            .s_var => |var_| blk: {
                try self.markSourcePatternBindersReassignable(var_.pattern_idx);
                break :blk .{ .var_ = .{ .pattern = self.checkedPattern(var_.pattern_idx), .expr = self.checkedExpr(var_.expr) } };
            },
            .s_reassign => |reassign| .{ .reassign = .{
                .pattern = self.checkedPattern(reassign.pattern_idx),
                .expr = self.checkedExpr(reassign.expr),
                .reassigned_binders = try self.copyReassignedBinders(reassign.pattern_idx),
            } },
            .s_crash => |crash| .{ .crash = try self.string_builder.intern(crash.msg) },
            .s_dbg => |dbg| .{ .dbg = self.checkedExpr(dbg.expr) },
            .s_expr => |expr| .{ .expr = self.checkedExpr(expr.expr) },
            .s_expect => |expect| .{ .expect = self.checkedExpr(expect.body) },
            .s_for => |for_| .{ .for_ = .{
                .pattern = self.checkedPattern(for_.patt),
                .expr = self.checkedExpr(for_.expr),
                .body = self.checkedExpr(for_.body),
                .plan = null,
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
        _: *@This(),
        representative_binders: []const SourcePatternBinder,
        ident: Ident.Idx,
    ) ?PatternBinderId {
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

    fn markSourcePatternBindersReassignable(self: *@This(), pattern_idx: CIR.Pattern.Idx) Allocator.Error!void {
        const pattern = self.module.pattern(pattern_idx).data;
        switch (pattern) {
            .assign => {
                const binder = try self.patternBinder(pattern_idx);
                self.pattern_binders.items[@intFromEnum(binder)].reassignable = true;
            },
            .as => |as| {
                const binder = try self.patternBinder(pattern_idx);
                self.pattern_binders.items[@intFromEnum(binder)].reassignable = true;
                try self.markSourcePatternBindersReassignable(as.pattern);
            },
            .applied_tag => |tag| {
                for (self.module.slicePatterns(tag.args)) |child| try self.markSourcePatternBindersReassignable(child);
            },
            .nominal => |nominal| try self.markSourcePatternBindersReassignable(nominal.backing_pattern),
            .nominal_external => |nominal| try self.markSourcePatternBindersReassignable(nominal.backing_pattern),
            .record_destructure => |record| {
                for (self.module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = self.module.getRecordDestruct(destruct_idx);
                    try self.markSourcePatternBindersReassignable(destruct.kind.toPatternIdx());
                }
            },
            .list => |list| {
                for (self.module.slicePatterns(list.patterns)) |child| try self.markSourcePatternBindersReassignable(child);
                if (list.rest_info) |rest| if (rest.pattern) |child| try self.markSourcePatternBindersReassignable(child);
            },
            .tuple => |tuple| {
                for (self.module.slicePatterns(tuple.patterns)) |child| try self.markSourcePatternBindersReassignable(child);
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

    fn copyReassignedBinders(self: *@This(), pattern_idx: CIR.Pattern.Idx) Allocator.Error![]const PatternBinderId {
        var binders = std.ArrayList(PatternBinderId).empty;
        defer binders.deinit(self.allocator);
        try self.collectReassignedBinders(pattern_idx, &binders);
        if (binders.items.len == 0) return &.{};
        return try binders.toOwnedSlice(self.allocator);
    }

    fn collectReassignedBinders(
        self: *@This(),
        pattern_idx: CIR.Pattern.Idx,
        out: *std.ArrayList(PatternBinderId),
    ) Allocator.Error!void {
        const pattern = self.module.pattern(pattern_idx).data;
        switch (pattern) {
            .assign => try self.appendReassignedBinder(pattern_idx, out),
            .as => |as| {
                try self.appendReassignedBinder(pattern_idx, out);
                try self.collectReassignedBinders(as.pattern, out);
            },
            .applied_tag => |tag| {
                for (self.module.slicePatterns(tag.args)) |child| try self.collectReassignedBinders(child, out);
            },
            .nominal => |nominal| try self.collectReassignedBinders(nominal.backing_pattern, out),
            .nominal_external => |nominal| try self.collectReassignedBinders(nominal.backing_pattern, out),
            .record_destructure => |record| {
                for (self.module.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    const destruct = self.module.getRecordDestruct(destruct_idx);
                    try self.collectReassignedBinders(destruct.kind.toPatternIdx(), out);
                }
            },
            .list => |list| {
                for (self.module.slicePatterns(list.patterns)) |child| try self.collectReassignedBinders(child, out);
                if (list.rest_info) |rest| if (rest.pattern) |child| try self.collectReassignedBinders(child, out);
            },
            .tuple => |tuple| {
                for (self.module.slicePatterns(tuple.patterns)) |child| try self.collectReassignedBinders(child, out);
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

    fn appendReassignedBinder(
        self: *@This(),
        pattern_idx: CIR.Pattern.Idx,
        out: *std.ArrayList(PatternBinderId),
    ) Allocator.Error!void {
        const binder = try self.patternBinder(pattern_idx);
        if (!self.pattern_binders.items[@intFromEnum(binder)].reassignable) return;
        for (out.items) |existing| {
            if (existing == binder) return;
        }
        try out.append(self.allocator, binder);
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
            .reassignable = self.sourcePatternBinderIsReassignable(pattern),
        });
        self.pattern_binder_by_pattern[raw] = id;
        return id;
    }

    fn sourcePatternBinderIsReassignable(self: *const @This(), pattern: CIR.Pattern.Idx) bool {
        return switch (self.module.pattern(pattern).data) {
            .assign => |assign| assign.ident.attributes.reassignable,
            .as => |as| as.ident.attributes.reassignable,
            else => checkedArtifactInvariant("checked artifact invariant violated: non-binder pattern requested a pattern binder", .{}),
        };
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

const FracLit = union(enum) {
    f32: f32,
    f64: f64,
    dec: builtins.dec.RocDec,
    small: CIR.SmallDecValue,
    scaled_dec: CIR.IntValue,
};

fn fracLiteralForBuiltin(literal: FracLit, builtin_nominal: CheckedBuiltinNominal, has_suffix: bool) ?CheckedExprData {
    return switch (builtin_nominal) {
        .f32 => .{ .frac_f32 = .{ .value = @floatCast(fracLitToF64(literal)), .has_suffix = has_suffix } },
        .f64 => .{ .frac_f64 = .{ .value = fracLitToF64(literal), .has_suffix = has_suffix } },
        .dec => .{ .dec = .{ .value = fracLitToDec(literal), .has_suffix = has_suffix } },
        else => null,
    };
}

fn originalFracLiteral(literal: FracLit, has_suffix: bool) CheckedExprData {
    return switch (literal) {
        .f32 => |value| .{ .frac_f32 = .{ .value = value, .has_suffix = has_suffix } },
        .f64 => |value| .{ .frac_f64 = .{ .value = value, .has_suffix = has_suffix } },
        .dec => |value| .{ .dec = .{ .value = value, .has_suffix = has_suffix } },
        .small => |value| .{ .dec_small = .{ .value = value, .has_suffix = has_suffix } },
        .scaled_dec => |value| .{ .dec = .{ .value = scaledDecToDec(value), .has_suffix = has_suffix } },
    };
}

fn checkedBuiltinForLiteralTarget(view: CheckedTypeStoreView, root: CheckedTypeId) ?CheckedBuiltinNominal {
    var current = root;
    while (true) {
        const index: usize = @intFromEnum(current);
        if (index >= view.payloads.len) {
            checkedArtifactInvariant("checked builtin lookup referenced a missing type root", .{});
        }
        switch (view.payloads[index]) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| return nominal.builtin,
            .flex => |variable| return checkedBuiltinForDefaultedNumericVariable(variable),
            .rigid => |variable| return checkedBuiltinForDefaultedNumericVariable(variable),
            .pending => checkedArtifactInvariant("checked builtin lookup reached a pending type payload", .{}),
            else => return null,
        }
    }
}

fn checkedBuiltinForDefaultedNumericVariable(variable: CheckedTypeVariable) ?CheckedBuiltinNominal {
    return switch (variable.numeric_default_phase orelse return null) {
        .mono_specialization => .dec,
        .checking_finalized => checkedArtifactInvariant("checking-finalized numeric variable reached checked literal publication", .{}),
    };
}

fn numeralLiteralDecimalText(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    literal: ModuleEnv.NumeralLiteral,
) Allocator.Error![]const u8 {
    const before = try base256DecimalText(allocator, module_env.numeralDigitsBefore(literal), 1);
    defer allocator.free(before);

    const after_min_digits: usize = std.math.cast(usize, literal.after_decimal_digit_count) orelse {
        checkedArtifactInvariant("checked numeral literal decimal digit count exceeded host usize", .{});
    };
    const after = if (after_min_digits == 0)
        try allocator.alloc(u8, 0)
    else
        try base256DecimalText(allocator, module_env.numeralDigitsAfter(literal), after_min_digits);
    defer allocator.free(after);

    const sign_len: usize = @intFromBool(literal.isNegative());
    const dot_len: usize = @intFromBool(after_min_digits > 0);
    const total_len = sign_len + before.len + dot_len + after.len;
    const text = try allocator.alloc(u8, total_len);
    var offset: usize = 0;
    if (literal.isNegative()) {
        text[offset] = '-';
        offset += 1;
    }
    @memcpy(text[offset..][0..before.len], before);
    offset += before.len;
    if (after_min_digits > 0) {
        text[offset] = '.';
        offset += 1;
        @memcpy(text[offset..][0..after.len], after);
    }
    return text;
}

fn base256DecimalText(allocator: Allocator, bytes_be: []const u8, min_digits: usize) Allocator.Error![]const u8 {
    var first_nonzero: usize = 0;
    while (first_nonzero < bytes_be.len and bytes_be[first_nonzero] == 0) : (first_nonzero += 1) {}

    if (first_nonzero == bytes_be.len) {
        const len = @max(min_digits, 1);
        const out = try allocator.alloc(u8, len);
        @memset(out, '0');
        return out;
    }

    var current_buf = try allocator.dupe(u8, bytes_be[first_nonzero..]);
    defer allocator.free(current_buf);
    var current_len = current_buf.len;
    var digits_rev = std.ArrayList(u8).empty;
    defer digits_rev.deinit(allocator);

    while (current_len > 0) {
        const current = current_buf[0..current_len];
        var quotient = try allocator.alloc(u8, current.len);
        var quotient_len: usize = 0;
        var remainder: u16 = 0;
        for (current) |byte| {
            const value = remainder * 256 + byte;
            const digit: u8 = @intCast(value / 10);
            remainder = value % 10;
            if (digit != 0 or quotient_len != 0) {
                quotient[quotient_len] = digit;
                quotient_len += 1;
            }
        }
        try digits_rev.append(allocator, '0' + @as(u8, @intCast(remainder)));
        allocator.free(current_buf);
        current_buf = quotient;
        current_len = quotient_len;
    }

    const digit_count = digits_rev.items.len;
    const total_len = @max(digit_count, min_digits);
    const out = try allocator.alloc(u8, total_len);
    const pad = total_len - digit_count;
    @memset(out[0..pad], '0');
    for (digits_rev.items, 0..) |digit, i| {
        out[pad + digit_count - 1 - i] = digit;
    }
    return out;
}

fn fracLitToF64(literal: FracLit) f64 {
    return switch (literal) {
        .f32 => |value| @floatCast(value),
        .f64 => |value| value,
        .dec => |value| value.toF64(),
        .small => |value| value.toF64(),
        .scaled_dec => |value| scaledDecToDec(value).toF64(),
    };
}

fn fracLitToDec(literal: FracLit) builtins.dec.RocDec {
    return switch (literal) {
        .f32 => |value| builtins.dec.RocDec.fromF64(@as(f64, @floatCast(value))) orelse {
            checkedArtifactInvariant("F32 literal solved as Dec exceeded Dec range", .{});
        },
        .f64 => |value| builtins.dec.RocDec.fromF64(value) orelse {
            checkedArtifactInvariant("F64 literal solved as Dec exceeded Dec range", .{});
        },
        .dec => |value| value,
        .small => |value| value.toRocDec(),
        .scaled_dec => |value| scaledDecToDec(value),
    };
}

fn scaledDecToDec(value: CIR.IntValue) builtins.dec.RocDec {
    return .{ .num = value.toI128() };
}

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
        .num_from_numeral,
        .typed_int,
        .typed_frac,
        .typed_num_from_numeral,
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

fn deinitCheckedStatementData(allocator: Allocator, data: *CheckedStatementData) void {
    switch (data.*) {
        .reassign => |reassign| allocator.free(reassign.reassigned_binders),
        else => {},
    }
    data.* = .pending;
}

fn verifyCheckedExprDataComplete(data: CheckedExprData) void {
    switch (data) {
        .pending => std.debug.panic("checked artifact invariant violated: checked expression payload was not filled", .{}),
        .lookup_local => |lookup| std.debug.assert(lookup.resolved != null),
        .lookup_external => |ref| std.debug.assert(ref != null),
        .lookup_required => |ref| std.debug.assert(ref != null),
        .dispatch_call => |plan| std.debug.assert(plan != null),
        .method_eq => |plan| std.debug.assert(plan != null),
        .type_dispatch_call => |plan| std.debug.assert(plan != null),
        .num_from_numeral => |plan| std.debug.assert(plan != null),
        .typed_num_from_numeral => |plan| std.debug.assert(plan != null),
        .for_ => |for_| std.debug.assert(for_.plan != null),
        else => {},
    }
}

fn verifyCheckedPatternDataComplete(data: CheckedPatternData) void {
    switch (data) {
        .pending => std.debug.panic("checked artifact invariant violated: checked pattern payload was not filled", .{}),
        else => {},
    }
}

fn verifyCheckedStatementDataComplete(data: CheckedStatementData) void {
    switch (data) {
        .pending => std.debug.panic("checked artifact invariant violated: checked statement payload was not filled", .{}),
        .for_ => |for_| std.debug.assert(for_.plan != null),
        else => {},
    }
}

fn isExprNodeTag(tag: CIR.Node.Tag) bool {
    return Ident.textStartsWith(@tagName(tag), "expr_");
}

fn isPatternNodeTag(tag: CIR.Node.Tag) bool {
    return Ident.textStartsWith(@tagName(tag), "pattern_");
}

fn isStatementNodeTag(tag: CIR.Node.Tag) bool {
    return Ident.textStartsWith(@tagName(tag), "statement_");
}

/// Public `CheckedProcedureBody` declaration.
pub const CheckedProcedureBody = union(enum) {
    checked_body: CheckedBodyId,
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
/// Short name for a resolved checked value id.
pub const ResolvedValueId = ResolvedValueRefId;

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

/// Public `ConstUseTemplate` declaration.
pub const ConstUseTemplate = struct {
    const_ref: ConstRef,
    requested_source_ty_template: canonical.CanonicalTypeKey,
    requested_source_ty_payload: ?CheckedTypeId = null,
};

/// Public `ArtifactTopLevelProcedureBindingRef` declaration.
pub const ArtifactTopLevelProcedureBindingRef = struct {
    artifact: CheckedModuleArtifactKey,
    binding: TopLevelProcedureBindingRef,
};

/// Public `ProcedureBindingRef` declaration.
pub const ProcedureBindingRef = union(enum) {
    top_level: ArtifactTopLevelProcedureBindingRef,
    imported: ImportedProcedureBindingRef,
    hosted: HostedProcRef,
    platform_required: RequiredAppProcedureRef,
};

/// Public `TopLevelProcedureBindingRef` declaration.
pub const TopLevelProcedureBindingRef = enum(u32) { _ };
/// Id for a top-level procedure binding.
pub const TopLevelProcedureBindingId = TopLevelProcedureBindingRef;
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

/// Public `LocalProcedureBinding` declaration.
pub const LocalProcedureBinding = struct {
    binder: PatternBinderId,
    expr: CheckedExprId,
};

/// Return the checked module id that owns a top-level procedure binding.
pub fn topLevelProcedureModuleId(ref: ArtifactTopLevelProcedureBindingRef) ModuleId {
    return ref.artifact;
}

/// Return the checked module id that owns an imported procedure binding.
pub fn importedProcedureModuleId(ref: ImportedProcedureBindingRef) ModuleId {
    return ref.artifact;
}

/// Return the checked module id that owns a hosted procedure template.
pub fn hostedProcedureTemplateModuleId(ref: HostedProcRef) ModuleId {
    return .{ .bytes = canonical.procTemplateModuleDigest(ref.template).bytes };
}

/// Return the checked module id that owns a required application procedure.
pub fn requiredProcedureModuleId(ref: RequiredAppProcedureRef) ModuleId {
    return ref.artifact;
}

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
    local_proc: LocalProcedureBinding,

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

        var local_pattern_roles = try LocalPatternRoleIndex.init(allocator, module, checked_bodies);
        defer local_pattern_roles.deinit(allocator);

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
            var resolved_ref = try categorizeValueRef(
                allocator,
                module,
                artifact_key,
                expr_idx,
                imports,
                templates,
                hosted_procs,
                platform_required_declarations,
                platform_required_bindings,
                top_level_values,
                &local_pattern_roles,
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
            try attachUseTypePayload(&resolved_ref, checked_type_key, checked_ty);

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
/// Short name for the resolved checked value table.
pub const ResolvedValueTable = ResolvedValueRefTable;

fn categorizeValueRef(
    _: Allocator,
    module: TypedCIR.Module,
    artifact_key: CheckedModuleArtifactKey,
    expr_idx: CIR.Expr.Idx,
    imports: []const PublishImportArtifact,
    _: *const CheckedProcedureTemplateTable,
    hosted_procs: *const HostedProcTable,
    platform_required_declarations: *const PlatformRequiredDeclarationTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    top_level_values: *const TopLevelValueTable,
    local_pattern_roles: *const LocalPatternRoleIndex,
    checked_bodies: *const CheckedBodyStore,
) Allocator.Error!ResolvedValueRef {
    const expr = module.expr(expr_idx);
    return switch (expr.data) {
        .e_lookup_local => |local| categorizeLocalValueRef(
            module,
            artifact_key,
            local.pattern_idx,
            hosted_procs,
            top_level_values,
            local_pattern_roles,
            checked_bodies,
        ),
        .e_lookup_external => |external| categorizeImportedValueRef(
            module,
            external.module_idx,
            external.target_node_idx,
            imports,
        ),
        .e_lookup_required => |required| categorizeRequiredValueRef(
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
    ref: *ResolvedValueRef,
    key: canonical.CanonicalTypeKey,
    checked_ty: CheckedTypeId,
) Allocator.Error!void {
    switch (ref.*) {
        .top_level_const => |*use| {
            use.requested_source_ty_template = key;
            use.requested_source_ty_payload = checked_ty;
        },
        .imported_const => |*use| {
            use.requested_source_ty_template = key;
            use.requested_source_ty_payload = checked_ty;
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

fn categorizeLocalValueRef(
    module: TypedCIR.Module,
    artifact_key: CheckedModuleArtifactKey,
    pattern: CIR.Pattern.Idx,
    hosted_procs: *const HostedProcTable,
    top_level_values: *const TopLevelValueTable,
    local_pattern_roles: *const LocalPatternRoleIndex,
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

    if (top_level_values.lookupByPattern(checked_pattern)) |entry| {
        if (entry.pattern != checked_pattern) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "checked artifact invariant violated: top-level pattern index {d} resolved to mismatched entry",
                    .{@intFromEnum(pattern)},
                );
            }
            unreachable;
        }

        switch (entry.value) {
            .const_ref => |const_ref| return .{ .top_level_const = .{
                .const_ref = const_ref,
                .requested_source_ty_template = .{},
            } },
            .procedure_binding => |binding| {
                if (hostedProcForDef(hosted_procs, entry.def)) |hosted| {
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
                    .binding = .{ .top_level = .{
                        .artifact = artifact_key,
                        .binding = binding,
                    } },
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

    if (local_pattern_roles.isLambdaArg(pattern)) {
        return .{ .local_param = .{ .binder = binder } };
    }

    if (local_pattern_roles.statementRole(pattern)) |role| {
        switch (role) {
            .mutable_version => return .{ .local_mutable_version = .{ .binder = binder } },
            .local_proc => |expr| return .{ .local_proc = .{ .binder = binder, .expr = expr } },
            .local_value => return .{ .local_value = .{ .binder = binder } },
        }
    }

    if (patternIsBinder(module, pattern)) {
        return .{ .pattern_binder = .{ .binder = binder } };
    }

    if (builtin.mode == .Debug) {
        std.debug.panic(
            "checked artifact invariant violated: local lookup pattern {d} has no categorized binding",
            .{@intFromEnum(pattern)},
        );
    }
    unreachable;
}

fn categorizeImportedValueRef(
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

fn categorizeRequiredValueRef(
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

const TopLevelDefPatternIndex = struct {
    by_pattern: []?CIR.Def.Idx = &.{},

    fn init(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!TopLevelDefPatternIndex {
        const by_pattern = try allocator.alloc(?CIR.Def.Idx, module.nodeCount());
        @memset(by_pattern, null);
        for (module.allDefs()) |def_idx| {
            by_pattern[@intFromEnum(module.def(def_idx).pattern.idx)] = def_idx;
        }
        return .{ .by_pattern = by_pattern };
    }

    fn defForPattern(self: *const TopLevelDefPatternIndex, pattern: CIR.Pattern.Idx) ?CIR.Def.Idx {
        const raw = @intFromEnum(pattern);
        if (raw >= self.by_pattern.len) return null;
        return self.by_pattern[raw];
    }

    fn deinit(self: *TopLevelDefPatternIndex, allocator: Allocator) void {
        allocator.free(self.by_pattern);
        self.* = .{};
    }
};

fn checkedPatternSourceTypeVar(module: TypedCIR.Module, top_level_defs: *const TopLevelDefPatternIndex, pattern: CIR.Pattern.Idx) Var {
    if (top_level_defs.defForPattern(pattern)) |def_idx| {
        return module.defType(def_idx);
    }
    return module.patternType(pattern);
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

const LocalPatternStatementRole = union(enum) {
    mutable_version,
    local_proc: CheckedExprId,
    local_value,
};

const LocalPatternRoleIndex = struct {
    lambda_args: []bool = &.{},
    statement_roles: []?LocalPatternStatementRole = &.{},

    fn init(
        allocator: Allocator,
        module: TypedCIR.Module,
        checked_bodies: *const CheckedBodyStore,
    ) Allocator.Error!LocalPatternRoleIndex {
        const node_count = module.nodeCount();
        const lambda_args = try allocator.alloc(bool, node_count);
        errdefer allocator.free(lambda_args);
        const statement_roles = try allocator.alloc(?LocalPatternStatementRole, node_count);
        errdefer allocator.free(statement_roles);
        @memset(lambda_args, false);
        @memset(statement_roles, null);

        var node_idx: u32 = 0;
        while (node_idx < node_count) : (node_idx += 1) {
            const node: CIR.Node.Idx = @enumFromInt(node_idx);
            switch (module.nodeTag(node)) {
                .statement_decl => {
                    const statement: CIR.Statement.Idx = @enumFromInt(node_idx);
                    const decl = switch (module.getStatement(statement)) {
                        .s_decl => |decl| decl,
                        else => unreachable,
                    };
                    const role: LocalPatternStatementRole = if (isLocalProcExpr(module, decl.expr))
                        .{ .local_proc = checked_bodies.exprIdForSource(decl.expr) orelse
                            checkedArtifactInvariant("checked local procedure declaration has no checked expression", .{}) }
                    else
                        .local_value;
                    putStatementRole(statement_roles, node_count, decl.pattern, role);
                },
                .statement_var => {
                    const statement: CIR.Statement.Idx = @enumFromInt(node_idx);
                    const var_ = switch (module.getStatement(statement)) {
                        .s_var => |var_| var_,
                        else => unreachable,
                    };
                    putStatementRole(statement_roles, node_count, var_.pattern_idx, .mutable_version);
                },
                .expr_lambda => {
                    const expr = module.expr(@enumFromInt(node_idx));
                    const lambda = switch (expr.data) {
                        .e_lambda => |lambda| lambda,
                        else => unreachable,
                    };
                    for (module.slicePatterns(lambda.args)) |arg| {
                        const raw_pattern = @intFromEnum(arg);
                        if (raw_pattern >= node_count) {
                            checkedArtifactInvariant("checked artifact invariant violated: lambda argument pattern is out of range", .{});
                        }
                        lambda_args[raw_pattern] = true;
                    }
                },
                else => {},
            }
        }

        verifyLocalPatternRoles(lambda_args, statement_roles);

        return .{
            .lambda_args = lambda_args,
            .statement_roles = statement_roles,
        };
    }

    fn putStatementRole(
        statement_roles: []?LocalPatternStatementRole,
        node_count: usize,
        pattern: CIR.Pattern.Idx,
        role: LocalPatternStatementRole,
    ) void {
        const raw_pattern = @intFromEnum(pattern);
        if (raw_pattern >= node_count) {
            checkedArtifactInvariant("checked artifact invariant violated: local statement pattern is out of range", .{});
        }
        if (statement_roles[raw_pattern]) |existing| {
            if (!localPatternStatementRoleEql(existing, role)) {
                checkedArtifactInvariant("checked artifact invariant violated: local statement pattern has conflicting roles", .{});
            }
            return;
        }
        statement_roles[raw_pattern] = role;
    }

    fn localPatternStatementRoleEql(a: LocalPatternStatementRole, b: LocalPatternStatementRole) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .mutable_version,
            .local_value,
            => true,
            .local_proc => |a_expr| a_expr == b.local_proc,
        };
    }

    fn verifyLocalPatternRoles(
        lambda_args: []const bool,
        statement_roles: []const ?LocalPatternStatementRole,
    ) void {
        if (builtin.mode != .Debug) return;

        for (lambda_args, statement_roles, 0..) |is_lambda_arg, maybe_statement_role, raw_pattern| {
            if (is_lambda_arg and maybe_statement_role != null) {
                std.debug.panic(
                    "checked artifact invariant violated: local pattern {d} is both a lambda argument and statement binding",
                    .{raw_pattern},
                );
            }
        }
    }

    fn isLambdaArg(self: *const LocalPatternRoleIndex, pattern: CIR.Pattern.Idx) bool {
        const raw = @intFromEnum(pattern);
        if (raw >= self.lambda_args.len) {
            checkedArtifactInvariant("checked artifact invariant violated: lambda argument lookup pattern is out of range", .{});
        }
        return self.lambda_args[raw];
    }

    fn statementRole(self: *const LocalPatternRoleIndex, pattern: CIR.Pattern.Idx) ?LocalPatternStatementRole {
        const raw = @intFromEnum(pattern);
        if (raw >= self.statement_roles.len) {
            checkedArtifactInvariant("checked artifact invariant violated: local statement lookup pattern is out of range", .{});
        }
        return self.statement_roles[raw];
    }

    fn deinit(self: *LocalPatternRoleIndex, allocator: Allocator) void {
        allocator.free(self.statement_roles);
        allocator.free(self.lambda_args);
        self.* = .{};
    }
};

fn patternIsBinder(module: TypedCIR.Module, pattern: CIR.Pattern.Idx) bool {
    const raw = @intFromEnum(pattern);
    if (raw >= module.nodeCount()) {
        checkedArtifactInvariant("checked artifact invariant violated: binder lookup pattern is out of range", .{});
    }
    return switch (module.nodeTag(@enumFromInt(raw))) {
        .pattern_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_nominal_external,
        .pattern_record_destructure,
        .pattern_list,
        .pattern_tuple,
        => true,
        else => false,
    };
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
            .num_from_numeral,
            .typed_num_from_numeral,
            => |plan_id| {
                const id = plan_id orelse checkedArtifactInvariant("checked from_numeral expression reached template closure collection without a dispatch plan", .{});
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
                if (call.direct_target) |target| try self.value_refs.append(self.allocator, target);
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
                const plan_id = for_.plan orelse checkedArtifactInvariant("checked for expression reached template closure collection without an iterator-for plan", .{});
                try self.collectIteratorForPlan(plan_id);
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
        for (plan.args) |arg| switch (arg) {
            .checked_expr => |expr| try self.collectExpr(expr),
            .generated_numeral => {},
        };
    }

    fn collectIteratorForPlan(
        self: *CheckedTemplateRefCollector,
        plan_id: static_dispatch.IteratorForPlanId,
    ) Allocator.Error!void {
        const raw = @intFromEnum(plan_id);
        if (raw >= self.static_dispatch_plans.iterator_for_plans.len) {
            checkedArtifactInvariant("checked template iterator-for plan id was outside the plan table", .{});
        }
        const plan = self.static_dispatch_plans.iterator_for_plans[raw];
        try self.collectIteratorDispatchCall(plan.iter);
        try self.collectIteratorDispatchCall(plan.next);
    }

    fn collectIteratorDispatchCall(
        self: *CheckedTemplateRefCollector,
        call: static_dispatch.IteratorDispatchCall,
    ) Allocator.Error!void {
        for (call.args) |arg| switch (arg) {
            .checked_expr => |expr| try self.collectExpr(expr),
            .loop_iterator_state => {},
        };
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
                const plan_id = for_.plan orelse checkedArtifactInvariant("checked for statement reached template closure collection without an iterator-for plan", .{});
                try self.collectIteratorForPlan(plan_id);
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
                .intrinsic_wrapper => {},
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
        global_value_defs: []const CIR.Def.Idx,
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

        for (global_value_defs) |def_idx| {
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
            .num_from_numeral,
            .typed_num_from_numeral,
            => |plan_id| try self.scanStaticDispatchPlanArgs(plan_id orelse checkedArtifactInvariant("checked from_numeral expression reached nested procedure site collection without a dispatch plan", .{}), owner),
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
        for (plan.args) |arg| switch (arg) {
            .checked_expr => |expr| try self.scanExpr(expr, owner, false),
            .generated_numeral => {},
        };
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
        global_value_defs: []const CIR.Def.Idx,
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

        for (global_value_defs) |def_idx| {
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
        if (Ident.textEndsWith(module_name, ".roc")) {
            module_name = module_name[0 .. module_name.len - 4];
        }

        const local_name = module.getIdent(symbol_name);
        const qualified_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ module_name, local_name });
        if (!Ident.textEndsWith(qualified_name, "!")) return qualified_name;

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
    declared_source_ty: canonical.CanonicalTypeKey,
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

/// Public `PlatformRequirementTypeMismatch` declaration.
pub const PlatformRequirementTypeMismatch = struct {
    declaration: PlatformRequiredDeclaration,
    app_value: TopLevelValueRef,
    expected: CheckedTypeId,
    actual: CheckedTypeId,
};

/// Public `PlatformAppRelationBuildResult` declaration.
pub const PlatformAppRelationBuildResult = union(enum) {
    relation: PlatformAppRelation,
    type_mismatch: PlatformRequirementTypeMismatch,

    pub fn deinit(self: *PlatformAppRelationBuildResult, allocator: Allocator) void {
        switch (self.*) {
            .relation => |*relation| relation.deinit(allocator),
            .type_mismatch => {},
        }
        self.* = undefined;
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
    declared_source_ty: canonical.CanonicalTypeKey,
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
        names: *canonical.CanonicalNameStore,
        checked_types: *CheckedTypePublication,
        declarations: *const PlatformRequiredDeclarationTable,
        relation_artifacts: []const ImportedModuleView,
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
            if (!std.meta.eql(input.app_value.artifact.bytes, active_relation.app_artifact.bytes)) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "checked artifact invariant violated: platform/app checked relation {d} points at a value outside the app artifact",
                        .{i},
                    );
                }
                unreachable;
            }
            const payload = try platformRequiredResolvedPayloadForRelation(
                allocator,
                module,
                names,
                checked_types,
                declarations,
                relation_artifacts,
                active_relation,
                input,
                declaration,
            );
            const payload_key = checked_types.store.roots[@intFromEnum(payload)].key;

            rows[i] = .{
                .id = input.id,
                .relation = active_relation.key,
                .module_idx = module.moduleIndex(),
                .declaration = input.declaration,
                .requires_idx = input.requires_idx,
                .app_value = input.app_value,
                .declared_source_ty = input.declared_source_ty,
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
            if (!std.meta.eql(binding.app_value.artifact.bytes, active_relation.app_artifact.bytes)) {
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
    if (!std.meta.eql(active_relation.requirement_context.bytes, expected_requirement_context.bytes)) {
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
    if (!std.meta.eql(active_relation.key.bytes, expected_key.bytes)) {
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
        if (std.meta.eql(artifact.key.bytes, key.bytes)) return artifact;
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
        if (!Ident.textEql(app_env.getIdent(header.relative_name), alias_name)) continue;

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
    if (!std.meta.eql(relation.app_value.artifact.bytes, binding.app_value.artifact.bytes) or
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
) Allocator.Error!PlatformAppRelationBuildResult {
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
        const expected_root = platformRequiredDeclarationRoot(platform_declaration_artifact, declaration);
        const actual_root = checkedTypeRootForScheme(app_artifact, app_value.source_scheme);
        if (!try platformRequirementTypesCompatible(
            allocator,
            platform_declaration_artifact,
            expected_root,
            app_artifact,
            actual_root,
        )) {
            allocator.free(relations);
            for (bindings[0..initialized_bindings]) |*binding| deinitPlatformRequiredValueUse(allocator, &binding.value_use);
            allocator.free(bindings);
            initialized_bindings = 0;
            return .{ .type_mismatch = .{
                .declaration = declaration,
                .app_value = app_value_ref,
                .expected = expected_root,
                .actual = actual_root,
            } };
        }
        const required_ty_is_function = sourceVarIsFunction(&platform_module_env.types, ModuleEnv.varFrom(declaration.type_anno));
        const value_kind: PlatformRequiredValueKind = if (required_ty_is_function) .procedure_value else .const_value;

        relations[i] = .{
            .id = @enumFromInt(@as(u32, @intCast(i))),
            .declaration = declaration.id,
            .requires_idx = declaration.requires_idx,
            .app_value = app_value_ref,
            .declared_source_ty = requested_source_ty,
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

    return .{ .relation = .{
        .key = relation_key,
        .requirement_context = requirement_context,
        .platform_module_idx = platform_declaration_artifact.module_identity.module_idx,
        .app_artifact = app_artifact.key,
        .relations = relations,
        .bindings = bindings,
    } };
}

fn platformRequiredDeclarationRoot(
    artifact: *const CheckedModuleArtifact,
    declaration: PlatformRequiredDeclaration,
) CheckedTypeId {
    const module_env = artifact.moduleEnvConst();
    if (declaration.requires_idx >= module_env.requires_types.items.items.len) {
        checkedArtifactInvariant("platform requirement declaration references an out-of-range required type", .{});
    }
    return (artifact.checked_types.schemeForKey(declaration.declared_source_ty) orelse
        checkedArtifactInvariant("platform requirement declaration has no published checked type scheme", .{})).root;
}

fn checkedTypeRootForScheme(
    artifact: *const CheckedModuleArtifact,
    scheme_key: canonical.CanonicalTypeSchemeKey,
) CheckedTypeId {
    return (artifact.checked_types.schemeForKey(scheme_key) orelse
        checkedArtifactInvariant("checked type scheme missing from artifact", .{})).root;
}

fn platformRequirementTypesCompatible(
    allocator: Allocator,
    platform_artifact: *const CheckedModuleArtifact,
    expected: CheckedTypeId,
    app_artifact: *const CheckedModuleArtifact,
    actual: CheckedTypeId,
) Allocator.Error!bool {
    var scratch_names = canonical.CanonicalNameStore.init(allocator);
    defer scratch_names.deinit();

    var scratch_store = CheckedTypeStore{};
    defer scratch_store.deinit(allocator);

    var platform_projector = CheckedTypeStoreImportProjector.init(
        allocator,
        &scratch_store,
        &scratch_names,
        importedView(platform_artifact),
    );
    defer platform_projector.deinit();
    const scratch_expected = try platform_projector.project(expected);

    var app_projector = CheckedTypeStoreImportProjector.init(
        allocator,
        &scratch_store,
        &scratch_names,
        importedView(app_artifact),
    );
    defer app_projector.deinit();
    const scratch_actual = try app_projector.project(actual);

    var checker = PlatformRequirementTypeCompatibilityChecker.init(allocator, &scratch_store);
    defer checker.deinit();
    return try checker.compatible(scratch_expected, scratch_actual);
}

const PlatformRequirementTypePair = struct {
    expected: u32,
    actual: u32,
};

const PlatformRequirementTypeCompatibilityChecker = struct {
    allocator: Allocator,
    store: *const CheckedTypeStore,
    active: std.AutoHashMap(PlatformRequirementTypePair, void),

    fn init(
        allocator: Allocator,
        store: *const CheckedTypeStore,
    ) PlatformRequirementTypeCompatibilityChecker {
        return .{
            .allocator = allocator,
            .store = store,
            .active = std.AutoHashMap(PlatformRequirementTypePair, void).init(allocator),
        };
    }

    fn deinit(self: *PlatformRequirementTypeCompatibilityChecker) void {
        self.active.deinit();
    }

    fn compatible(
        self: *PlatformRequirementTypeCompatibilityChecker,
        expected: CheckedTypeId,
        actual: CheckedTypeId,
    ) Allocator.Error!bool {
        const pair = PlatformRequirementTypePair{
            .expected = @intFromEnum(expected),
            .actual = @intFromEnum(actual),
        };
        if (self.active.contains(pair)) return true;
        try self.active.put(pair, {});
        defer _ = self.active.remove(pair);

        const expected_payload = self.payload(expected);
        const actual_payload = self.payload(actual);
        if (checkedTypePayloadIsIdentity(expected_payload) or checkedTypePayloadIsIdentity(actual_payload)) {
            return true;
        }

        switch (expected_payload) {
            .alias => |alias| return try self.compatible(alias.backing, actual),
            else => {},
        }
        switch (actual_payload) {
            .alias => |alias| return try self.compatible(expected, alias.backing),
            else => {},
        }

        return switch (expected_payload) {
            .pending => checkedArtifactInvariant("platform requirement type compatibility reached pending expected payload", .{}),
            .flex, .rigid, .alias => unreachable,
            .empty_record => self.compatibleRecord(expected_payload, actual_payload),
            .record, .record_unbound => self.compatibleRecord(expected_payload, actual_payload),
            .empty_tag_union => self.compatibleTagUnion(expected_payload, actual_payload),
            .tag_union => self.compatibleTagUnion(expected_payload, actual_payload),
            .tuple => |expected_items| blk: {
                const actual_items = switch (actual_payload) {
                    .tuple => |items| items,
                    else => break :blk false,
                };
                if (expected_items.len != actual_items.len) break :blk false;
                for (expected_items, actual_items) |expected_item, actual_item| {
                    if (!try self.compatible(expected_item, actual_item)) break :blk false;
                }
                break :blk true;
            },
            .function => |expected_fn| blk: {
                const actual_fn = switch (actual_payload) {
                    .function => |function| function,
                    else => break :blk false,
                };
                if (expected_fn.args.len != actual_fn.args.len) break :blk false;
                if (!functionKindsCompatible(expected_fn.kind, actual_fn.kind)) break :blk false;
                for (expected_fn.args, actual_fn.args) |expected_arg, actual_arg| {
                    if (!try self.compatible(expected_arg, actual_arg)) break :blk false;
                }
                break :blk try self.compatible(expected_fn.ret, actual_fn.ret);
            },
            .nominal => |expected_nominal| self.compatibleNominal(expected, expected_nominal, actual, actual_payload),
        };
    }

    fn compatibleNominal(
        self: *PlatformRequirementTypeCompatibilityChecker,
        expected: CheckedTypeId,
        expected_nominal: CheckedNominalType,
        actual: CheckedTypeId,
        actual_payload: CheckedTypePayload,
    ) Allocator.Error!bool {
        const actual_nominal = switch (actual_payload) {
            .nominal => |nominal| nominal,
            else => {
                if (expected_nominal.is_opaque) return false;
                return try self.compatible(expected_nominal.backing, actual);
            },
        };
        if (expected_nominal.name != actual_nominal.name) return false;
        if (expected_nominal.origin_module != actual_nominal.origin_module) return false;
        if (expected_nominal.builtin != actual_nominal.builtin) return false;
        if (expected_nominal.is_opaque != actual_nominal.is_opaque) return false;
        if (expected_nominal.args.len != actual_nominal.args.len) return false;
        for (expected_nominal.args, actual_nominal.args) |expected_arg, actual_arg| {
            if (!try self.compatible(expected_arg, actual_arg)) return false;
        }
        if (expected_nominal.is_opaque) return true;
        if (try self.compatible(expected_nominal.backing, actual_nominal.backing)) return true;
        return canonicalTypeKeyEql(
            self.store.roots[@intFromEnum(expected)].key,
            self.store.roots[@intFromEnum(actual)].key,
        );
    }

    fn compatibleRecord(
        self: *PlatformRequirementTypeCompatibilityChecker,
        expected_payload: CheckedTypePayload,
        actual_payload: CheckedTypePayload,
    ) Allocator.Error!bool {
        const expected_parts = recordParts(expected_payload) orelse return false;
        const actual_parts = recordParts(actual_payload) orelse return false;
        const expected_row = try self.flattenRecordRow(expected_parts.fields, expected_parts.ext);
        defer expected_row.deinit(self.allocator);
        const actual_row = try self.flattenRecordRow(actual_parts.fields, actual_parts.ext);
        defer actual_row.deinit(self.allocator);

        for (expected_row.fields) |expected_field| {
            const actual_field = findRecordFieldById(actual_row.fields, expected_field.name) orelse {
                if (actual_row.tail) |tail| {
                    if (checkedTypePayloadIsIdentity(self.payload(tail))) continue;
                }
                return false;
            };
            if (!try self.compatible(expected_field.ty, actual_field.ty)) return false;
        }

        if (expected_row.tail) |tail| {
            if (checkedTypePayloadIsIdentity(self.payload(tail))) return true;
        }
        for (actual_row.fields) |actual_field| {
            if (findRecordFieldById(expected_row.fields, actual_field.name) != null) continue;
            return false;
        }
        return self.rowTailCanClose(actual_row.tail);
    }

    fn compatibleTagUnion(
        self: *PlatformRequirementTypeCompatibilityChecker,
        expected_payload: CheckedTypePayload,
        actual_payload: CheckedTypePayload,
    ) Allocator.Error!bool {
        const expected_union = tagUnionParts(expected_payload) orelse return false;
        const actual_union = tagUnionParts(actual_payload) orelse return false;
        const expected_row = try self.flattenTagRow(expected_union.tags, expected_union.ext);
        defer expected_row.deinit(self.allocator);
        const actual_row = try self.flattenTagRow(actual_union.tags, actual_union.ext);
        defer actual_row.deinit(self.allocator);

        for (expected_row.tags) |expected_tag| {
            const actual_tag = findTagById(actual_row.tags, expected_tag.name) orelse {
                if (actual_row.tail) |tail| {
                    if (checkedTypePayloadIsIdentity(self.payload(tail))) continue;
                }
                return false;
            };
            if (expected_tag.args.len != actual_tag.args.len) return false;
            for (expected_tag.args, actual_tag.args) |expected_arg, actual_arg| {
                if (!try self.compatible(expected_arg, actual_arg)) return false;
            }
        }

        if (expected_row.tail) |tail| {
            if (checkedTypePayloadIsIdentity(self.payload(tail))) return true;
        }
        for (actual_row.tags) |actual_tag| {
            if (findTagById(expected_row.tags, actual_tag.name) != null) continue;
            return false;
        }
        return self.rowTailCanClose(actual_row.tail);
    }

    fn rowTailCanClose(self: *const PlatformRequirementTypeCompatibilityChecker, tail: ?CheckedTypeId) bool {
        const tail_id = tail orelse return true;
        return checkedTypePayloadIsIdentity(self.payload(tail_id));
    }

    const FlattenedRecordRow = struct {
        fields: []const CheckedRecordField,
        tail: ?CheckedTypeId,

        fn deinit(self: @This(), allocator: Allocator) void {
            if (self.fields.len > 0) allocator.free(self.fields);
        }
    };

    fn flattenRecordRow(
        self: *PlatformRequirementTypeCompatibilityChecker,
        head: []const CheckedRecordField,
        ext: ?CheckedTypeId,
    ) Allocator.Error!FlattenedRecordRow {
        var fields = std.ArrayList(CheckedRecordField).empty;
        errdefer fields.deinit(self.allocator);
        try fields.appendSlice(self.allocator, head);
        var tail = ext;
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();
        while (tail) |tail_id| {
            if (seen.contains(tail_id)) {
                tail = null;
                break;
            }
            try seen.put(tail_id, {});
            switch (self.payload(tail_id)) {
                .empty_record => {
                    tail = null;
                    break;
                },
                .record => |record| {
                    try fields.appendSlice(self.allocator, record.fields);
                    tail = record.ext;
                },
                .record_unbound => |tail_fields| {
                    try fields.appendSlice(self.allocator, tail_fields);
                    tail = null;
                    break;
                },
                .alias => |alias| tail = alias.backing,
                else => break,
            }
        }
        return .{ .fields = try fields.toOwnedSlice(self.allocator), .tail = tail };
    }

    const FlattenedTagRow = struct {
        tags: []const CheckedTag,
        tail: ?CheckedTypeId,

        fn deinit(self: @This(), allocator: Allocator) void {
            if (self.tags.len > 0) allocator.free(self.tags);
        }
    };

    fn flattenTagRow(
        self: *PlatformRequirementTypeCompatibilityChecker,
        head: []const CheckedTag,
        ext: ?CheckedTypeId,
    ) Allocator.Error!FlattenedTagRow {
        var tags = std.ArrayList(CheckedTag).empty;
        errdefer tags.deinit(self.allocator);
        try tags.appendSlice(self.allocator, head);
        var tail = ext;
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();
        while (tail) |tail_id| {
            if (seen.contains(tail_id)) {
                tail = null;
                break;
            }
            try seen.put(tail_id, {});
            switch (self.payload(tail_id)) {
                .empty_tag_union => {
                    tail = null;
                    break;
                },
                .tag_union => |tag_union| {
                    try tags.appendSlice(self.allocator, tag_union.tags);
                    tail = tag_union.ext;
                },
                .alias => |alias| tail = alias.backing,
                else => break,
            }
        }
        return .{ .tags = try tags.toOwnedSlice(self.allocator), .tail = tail };
    }

    fn payload(self: *const PlatformRequirementTypeCompatibilityChecker, root: CheckedTypeId) CheckedTypePayload {
        const index: usize = @intFromEnum(root);
        if (index >= self.store.payloads.len) {
            checkedArtifactInvariant("platform requirement type compatibility referenced missing checked type payload", .{});
        }
        return self.store.payloads[index];
    }
};

fn findRecordFieldById(
    fields: []const CheckedRecordField,
    name: canonical.RecordFieldLabelId,
) ?CheckedRecordField {
    for (fields) |field| {
        if (field.name == name) return field;
    }
    return null;
}

fn findTagById(
    tags: []const CheckedTag,
    name: canonical.TagLabelId,
) ?CheckedTag {
    for (tags) |tag| {
        if (tag.name == name) return tag;
    }
    return null;
}

fn functionKindsCompatible(expected: CheckedFunctionKind, actual: CheckedFunctionKind) bool {
    const finalized_expected = finalizedFunctionKind(expected);
    const finalized_actual = finalizedFunctionKind(actual);
    return finalized_expected == finalized_actual or finalized_expected == .effectful and finalized_actual == .pure;
}

const RelationTagUnionParts = struct {
    tags: []const CheckedTag,
    ext: ?CheckedTypeId,
};

fn tagUnionParts(payload: CheckedTypePayload) ?RelationTagUnionParts {
    return switch (payload) {
        .tag_union => |tag_union| .{ .tags = tag_union.tags, .ext = tag_union.ext },
        .empty_tag_union => .{ .tags = &.{}, .ext = null },
        else => null,
    };
}

fn platformRequiredResolvedPayloadForRelation(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    checked_types: *CheckedTypePublication,
    _: *const PlatformRequiredDeclarationTable,
    relation_artifacts: []const ImportedModuleView,
    active_relation: PlatformAppRelation,
    input: PlatformRequirementRelationInput,
    declaration: PlatformRequiredDeclaration,
) Allocator.Error!CheckedTypeId {
    const platform_payload = platformRequiredPayloadForDeclaration(module, checked_types, declaration);
    const app_view = relationArtifactByKey(relation_artifacts, active_relation.app_artifact) orelse {
        checkedArtifactInvariant("platform/app relation resolution missing app relation artifact", .{});
    };
    const app_scheme = app_view.checked_types.schemeForKey(input.app_value_source_scheme) orelse {
        checkedArtifactInvariant("platform/app relation resolution could not find app value source scheme", .{});
    };

    var projector = CheckedTypeStoreImportProjector.init(allocator, &checked_types.store, names, app_view);
    defer projector.deinit();
    const projected_app_root = try projector.project(app_scheme.root);

    var resolver = PlatformAppRelationTypeResolver.init(allocator, names, &checked_types.store);
    defer resolver.deinit();
    return try resolver.merge(platform_payload, projected_app_root, .value);
}

const PlatformAppRelationMergeContext = enum {
    value,
    record_tail,
    tag_tail,
};

const PlatformAppRelationTypeResolver = struct {
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    store: *CheckedTypeStore,
    finalizing: std.AutoHashMap(CheckedTypeId, CheckedTypeId),

    fn init(
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        store: *CheckedTypeStore,
    ) PlatformAppRelationTypeResolver {
        return .{
            .allocator = allocator,
            .names = names,
            .store = store,
            .finalizing = std.AutoHashMap(CheckedTypeId, CheckedTypeId).init(allocator),
        };
    }

    fn deinit(self: *PlatformAppRelationTypeResolver) void {
        self.finalizing.deinit();
    }

    fn merge(
        self: *PlatformAppRelationTypeResolver,
        platform_root: CheckedTypeId,
        app_root: CheckedTypeId,
        context: PlatformAppRelationMergeContext,
    ) Allocator.Error!CheckedTypeId {
        const platform_payload = self.payload(platform_root);
        const app_payload = self.payload(app_root);

        if (checkedTypePayloadIsIdentity(platform_payload)) {
            return try self.mergeIdentityWith(platform_root, app_root, app_payload, context);
        }
        if (checkedTypePayloadIsIdentity(app_payload)) {
            return try self.mergeIdentityWith(app_root, platform_root, platform_payload, context);
        }

        switch (platform_payload) {
            .alias => {},
            else => switch (app_payload) {
                .alias => |alias| return try self.merge(platform_root, alias.backing, context),
                else => {},
            },
        }

        return switch (platform_payload) {
            .pending => checkedArtifactInvariant("platform/app relation merge reached pending platform payload", .{}),
            .flex, .rigid => unreachable,
            .empty_record => switch (app_payload) {
                .empty_record => platform_root,
                .record, .record_unbound => try self.finalize(app_root, context),
                else => checkedArtifactInvariant("platform/app relation expected record-compatible app payload", .{}),
            },
            .empty_tag_union => switch (app_payload) {
                .empty_tag_union => platform_root,
                .tag_union => try self.finalize(app_root, context),
                else => checkedArtifactInvariant("platform/app relation expected tag-compatible app payload", .{}),
            },
            .record, .record_unbound => try self.mergeRecordRoots(platform_root, app_root),
            .tag_union => try self.mergeTagUnionRoots(platform_root, app_root),
            .tuple => |platform_items| blk: {
                const app_items = switch (app_payload) {
                    .tuple => |items| items,
                    else => checkedArtifactInvariant("platform/app relation expected tuple-compatible app payload", .{}),
                };
                if (platform_items.len != app_items.len) {
                    checkedArtifactInvariant("platform/app relation tuple arity mismatch", .{});
                }
                const items = try self.mergeRootSlices(platform_items, app_items);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .tuple = items });
            },
            .function => |platform_fn| blk: {
                const app_fn = switch (app_payload) {
                    .function => |function| function,
                    else => checkedArtifactInvariant("platform/app relation expected function-compatible app payload", .{}),
                };
                if (platform_fn.args.len != app_fn.args.len) {
                    checkedArtifactInvariant("platform/app relation function arity mismatch", .{});
                }
                const args = try self.mergeRootSlices(platform_fn.args, app_fn.args);
                errdefer self.allocator.free(args);
                const ret = try self.merge(platform_fn.ret, app_fn.ret, .value);
                const needs_instantiation = try self.store.checkedTypeSliceContainsIdentityVariables(self.allocator, args) or
                    try self.store.checkedTypeContainsIdentityVariables(self.allocator, ret);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .function = .{
                    .kind = finalizedFunctionKind(platform_fn.kind),
                    .args = args,
                    .ret = ret,
                    .needs_instantiation = needs_instantiation,
                } });
            },
            .alias => |platform_alias| try self.mergeAlias(platform_alias, app_root, app_payload),
            .nominal => |platform_nominal| try self.mergeNominal(platform_root, platform_nominal, app_root, app_payload),
        };
    }

    fn mergeIdentityWith(
        self: *PlatformAppRelationTypeResolver,
        _: CheckedTypeId,
        other_root: CheckedTypeId,
        other_payload: CheckedTypePayload,
        context: PlatformAppRelationMergeContext,
    ) Allocator.Error!CheckedTypeId {
        if (checkedTypePayloadIsIdentity(other_payload)) {
            return switch (context) {
                .record_tail => try self.emptyRecordRoot(),
                .tag_tail => try self.emptyTagUnionRoot(),
                .value => other_root,
            };
        }
        return try self.finalize(other_root, context);
    }

    fn finalize(
        self: *PlatformAppRelationTypeResolver,
        root: CheckedTypeId,
        context: PlatformAppRelationMergeContext,
    ) Allocator.Error!CheckedTypeId {
        const root_payload = self.payload(root);
        if (checkedTypePayloadIsIdentity(root_payload)) {
            return switch (context) {
                .record_tail => try self.emptyRecordRoot(),
                .tag_tail => try self.emptyTagUnionRoot(),
                .value => root,
            };
        }
        if (self.finalizing.get(root)) |existing| return existing;

        return switch (root_payload) {
            .pending => checkedArtifactInvariant("platform/app relation finalization reached pending payload", .{}),
            .flex, .rigid => unreachable,
            .empty_record,
            .empty_tag_union,
            => root,
            .tuple => |items| blk: {
                const finalized = try self.finalizeRootSlice(items);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .tuple = finalized });
            },
            .function => |function| blk: {
                const args = try self.finalizeRootSlice(function.args);
                errdefer self.allocator.free(args);
                const ret = try self.finalize(function.ret, .value);
                const needs_instantiation = try self.store.checkedTypeSliceContainsIdentityVariables(self.allocator, args) or
                    try self.store.checkedTypeContainsIdentityVariables(self.allocator, ret);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .function = .{
                    .kind = finalizedFunctionKind(function.kind),
                    .args = args,
                    .ret = ret,
                    .needs_instantiation = needs_instantiation,
                } });
            },
            .alias => |alias| blk: {
                const backing = try self.finalize(alias.backing, .value);
                const args = try self.finalizeRootSlice(alias.args);
                errdefer self.allocator.free(args);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .alias = .{
                    .name = alias.name,
                    .origin_module = alias.origin_module,
                    .backing = backing,
                    .args = args,
                } });
            },
            .record => |record| blk: {
                const row = try self.flattenRecordRow(record.fields, record.ext);
                defer row.deinit(self.allocator);
                const fields = try self.finalizeRecordFields(row.fields);
                errdefer self.allocator.free(fields);
                const ext = if (row.tail) |tail| try self.finalize(tail, .record_tail) else try self.emptyRecordRoot();
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .record = .{
                    .fields = fields,
                    .ext = ext,
                } });
            },
            .record_unbound => |fields| blk: {
                const finalized = try self.finalizeRecordFields(fields);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .record_unbound = finalized });
            },
            .tag_union => |tag_union| blk: {
                const row = try self.flattenTagRow(tag_union.tags, tag_union.ext);
                defer row.deinit(self.allocator);
                const tags = try self.finalizeTags(row.tags);
                errdefer deinitCheckedTags(self.allocator, tags);
                const ext = if (row.tail) |tail| try self.finalize(tail, .tag_tail) else try self.emptyTagUnionRoot();
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .tag_union = .{
                    .tags = tags,
                    .ext = ext,
                } });
            },
            .nominal => |nominal| blk: {
                const args = try self.finalizeRootSlice(nominal.args);
                errdefer self.allocator.free(args);
                const backing = try self.finalize(nominal.backing, .value);
                break :blk try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .nominal = .{
                    .name = nominal.name,
                    .origin_module = nominal.origin_module,
                    .builtin = nominal.builtin,
                    .is_opaque = nominal.is_opaque,
                    .backing = backing,
                    .representation = nominal.representation,
                    .args = args,
                } });
            },
        };
    }

    fn mergeAlias(
        self: *PlatformAppRelationTypeResolver,
        platform_alias: CheckedAliasType,
        app_root: CheckedTypeId,
        app_payload: CheckedTypePayload,
    ) Allocator.Error!CheckedTypeId {
        const app_backing = switch (app_payload) {
            .alias => |alias| alias.backing,
            else => app_root,
        };
        const backing = try self.merge(platform_alias.backing, app_backing, .value);
        const args = try self.finalizeRootSlice(platform_alias.args);
        errdefer self.allocator.free(args);
        return try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .alias = .{
            .name = platform_alias.name,
            .origin_module = platform_alias.origin_module,
            .backing = backing,
            .args = args,
        } });
    }

    fn mergeNominal(
        self: *PlatformAppRelationTypeResolver,
        platform_root: CheckedTypeId,
        platform_nominal: CheckedNominalType,
        app_root: CheckedTypeId,
        app_payload: CheckedTypePayload,
    ) Allocator.Error!CheckedTypeId {
        const app_nominal = switch (app_payload) {
            .nominal => |nominal| nominal,
            .alias => |alias| return try self.merge(platform_root, alias.backing, .value),
            else => {
                if (platform_nominal.is_opaque) {
                    checkedArtifactInvariant("platform/app relation expected nominal-compatible app payload", .{});
                }
                return try self.merge(platform_nominal.backing, app_root, .value);
            },
        };
        if (platform_nominal.name != app_nominal.name or
            platform_nominal.origin_module != app_nominal.origin_module or
            platform_nominal.is_opaque != app_nominal.is_opaque or
            platform_nominal.args.len != app_nominal.args.len)
        {
            checkedArtifactInvariant("platform/app relation nominal mismatch", .{});
        }
        const args = try self.mergeRootSlices(platform_nominal.args, app_nominal.args);
        errdefer self.allocator.free(args);
        const backing = try self.merge(platform_nominal.backing, app_nominal.backing, .value);
        return try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .nominal = .{
            .name = platform_nominal.name,
            .origin_module = platform_nominal.origin_module,
            .builtin = platform_nominal.builtin,
            .is_opaque = platform_nominal.is_opaque,
            .backing = backing,
            .representation = platform_nominal.representation,
            .args = args,
        } });
    }

    fn mergeRecordRoots(
        self: *PlatformAppRelationTypeResolver,
        platform_root: CheckedTypeId,
        app_root: CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        const platform_payload = self.payload(platform_root);
        const app_payload = self.payload(app_root);
        const platform_parts = recordParts(platform_payload) orelse {
            checkedArtifactInvariant("platform/app relation expected platform record payload", .{});
        };
        const app_parts = recordParts(app_payload) orelse switch (app_payload) {
            .alias => |alias| return try self.mergeRecordRoots(platform_root, alias.backing),
            else => checkedArtifactInvariant("platform/app relation expected app record payload", .{}),
        };
        const platform_row = try self.flattenRecordRow(platform_parts.fields, platform_parts.ext);
        defer platform_row.deinit(self.allocator);
        const app_row = try self.flattenRecordRow(app_parts.fields, app_parts.ext);
        defer app_row.deinit(self.allocator);

        const fields = try self.mergeRecordFields(platform_row.fields, app_row.fields);
        errdefer self.allocator.free(fields);
        const ext = try self.mergeOptionalRecordExt(platform_row.tail, app_row.tail);
        return try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .record = .{
            .fields = fields,
            .ext = ext,
        } });
    }

    fn mergeTagUnionRoots(
        self: *PlatformAppRelationTypeResolver,
        platform_root: CheckedTypeId,
        app_root: CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        const platform_payload = self.payload(platform_root);
        const app_payload = self.payload(app_root);
        const platform_union = switch (platform_payload) {
            .tag_union => |tag_union| tag_union,
            else => checkedArtifactInvariant("platform/app relation expected platform tag union payload", .{}),
        };
        const app_union = switch (app_payload) {
            .tag_union => |tag_union| tag_union,
            .alias => |alias| return try self.mergeTagUnionRoots(platform_root, alias.backing),
            .empty_tag_union => CheckedTagUnionType{
                .tags = &.{},
                .ext = try self.emptyTagUnionRoot(),
            },
            else => checkedArtifactInvariant("platform/app relation expected app tag union payload", .{}),
        };
        const platform_row = try self.flattenTagRow(platform_union.tags, platform_union.ext);
        defer platform_row.deinit(self.allocator);
        const app_row = try self.flattenTagRow(app_union.tags, app_union.ext);
        defer app_row.deinit(self.allocator);

        const tags = try self.mergeTags(platform_row.tags, app_row.tags);
        errdefer deinitCheckedTags(self.allocator, tags);
        const ext = try self.mergeOptionalTagExt(platform_row.tail, app_row.tail);
        return try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .{ .tag_union = .{
            .tags = tags,
            .ext = ext,
        } });
    }

    fn mergeOptionalRecordExt(
        self: *PlatformAppRelationTypeResolver,
        platform_ext: ?CheckedTypeId,
        app_ext: ?CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        if (platform_ext) |left| {
            if (app_ext) |right| return try self.merge(left, right, .record_tail);
            return try self.finalize(left, .record_tail);
        }
        if (app_ext) |right| return try self.finalize(right, .record_tail);
        return try self.emptyRecordRoot();
    }

    fn mergeOptionalTagExt(
        self: *PlatformAppRelationTypeResolver,
        platform_ext: ?CheckedTypeId,
        app_ext: ?CheckedTypeId,
    ) Allocator.Error!CheckedTypeId {
        if (platform_ext) |left| {
            if (app_ext) |right| return try self.merge(left, right, .tag_tail);
            return try self.finalize(left, .tag_tail);
        }
        if (app_ext) |right| return try self.finalize(right, .tag_tail);
        return try self.emptyTagUnionRoot();
    }

    const FlattenedRecordRow = struct {
        fields: []const CheckedRecordField,
        tail: ?CheckedTypeId,

        fn deinit(self: @This(), allocator: Allocator) void {
            if (self.fields.len > 0) allocator.free(self.fields);
        }
    };

    fn flattenRecordRow(
        self: *PlatformAppRelationTypeResolver,
        head: []const CheckedRecordField,
        ext: ?CheckedTypeId,
    ) Allocator.Error!FlattenedRecordRow {
        var fields = std.ArrayList(CheckedRecordField).empty;
        errdefer fields.deinit(self.allocator);

        try fields.appendSlice(self.allocator, head);
        var tail = ext;
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        while (tail) |tail_id| {
            if (seen.contains(tail_id)) {
                tail = null;
                break;
            }
            try seen.put(tail_id, {});

            switch (self.payload(tail_id)) {
                .empty_record => {
                    tail = null;
                    break;
                },
                .record => |record| {
                    try fields.appendSlice(self.allocator, record.fields);
                    tail = record.ext;
                },
                .record_unbound => |tail_fields| {
                    try fields.appendSlice(self.allocator, tail_fields);
                    tail = null;
                    break;
                },
                .alias => |alias| tail = alias.backing,
                else => break,
            }
        }

        return .{
            .fields = try fields.toOwnedSlice(self.allocator),
            .tail = tail,
        };
    }

    const FlattenedTagRow = struct {
        tags: []const CheckedTag,
        tail: ?CheckedTypeId,

        fn deinit(self: @This(), allocator: Allocator) void {
            if (self.tags.len > 0) allocator.free(self.tags);
        }
    };

    fn flattenTagRow(
        self: *PlatformAppRelationTypeResolver,
        head: []const CheckedTag,
        ext: CheckedTypeId,
    ) Allocator.Error!FlattenedTagRow {
        var tags = std.ArrayList(CheckedTag).empty;
        errdefer tags.deinit(self.allocator);

        try tags.appendSlice(self.allocator, head);
        var tail: ?CheckedTypeId = ext;
        var seen = std.AutoHashMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        while (tail) |tail_id| {
            if (seen.contains(tail_id)) {
                tail = null;
                break;
            }
            try seen.put(tail_id, {});

            switch (self.payload(tail_id)) {
                .empty_tag_union => {
                    tail = null;
                    break;
                },
                .tag_union => |tag_union| {
                    try tags.appendSlice(self.allocator, tag_union.tags);
                    tail = tag_union.ext;
                },
                .alias => |alias| tail = alias.backing,
                else => break,
            }
        }

        return .{
            .tags = try tags.toOwnedSlice(self.allocator),
            .tail = tail,
        };
    }

    fn mergeRootSlices(
        self: *PlatformAppRelationTypeResolver,
        platform_items: []const CheckedTypeId,
        app_items: []const CheckedTypeId,
    ) Allocator.Error![]const CheckedTypeId {
        if (platform_items.len != app_items.len) {
            checkedArtifactInvariant("platform/app relation arity mismatch", .{});
        }
        if (platform_items.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTypeId, platform_items.len);
        errdefer self.allocator.free(out);
        for (platform_items, app_items, 0..) |platform_item, app_item, i| {
            out[i] = try self.merge(platform_item, app_item, .value);
        }
        return out;
    }

    fn finalizeRootSlice(
        self: *PlatformAppRelationTypeResolver,
        items: []const CheckedTypeId,
    ) Allocator.Error![]const CheckedTypeId {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTypeId, items.len);
        errdefer self.allocator.free(out);
        for (items, 0..) |item, i| {
            out[i] = try self.finalize(item, .value);
        }
        return out;
    }

    fn mergeRecordFields(
        self: *PlatformAppRelationTypeResolver,
        platform_fields: []const CheckedRecordField,
        app_fields: []const CheckedRecordField,
    ) Allocator.Error![]const CheckedRecordField {
        var fields = std.ArrayList(CheckedRecordField).empty;
        errdefer fields.deinit(self.allocator);

        for (platform_fields) |platform_field| {
            if (findRecordField(self.names, app_fields, platform_field.name)) |app_field| {
                try fields.append(self.allocator, .{
                    .name = platform_field.name,
                    .ty = try self.merge(platform_field.ty, app_field.ty, .value),
                });
            } else {
                try fields.append(self.allocator, .{
                    .name = platform_field.name,
                    .ty = try self.finalize(platform_field.ty, .value),
                });
            }
        }
        for (app_fields) |app_field| {
            if (findRecordField(self.names, platform_fields, app_field.name) != null) continue;
            try fields.append(self.allocator, .{
                .name = app_field.name,
                .ty = try self.finalize(app_field.ty, .value),
            });
        }
        std.mem.sort(CheckedRecordField, fields.items, self.names, recordFieldLessThanByName);
        return try fields.toOwnedSlice(self.allocator);
    }

    fn finalizeRecordFields(
        self: *PlatformAppRelationTypeResolver,
        fields: []const CheckedRecordField,
    ) Allocator.Error![]const CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = field.name,
                .ty = try self.finalize(field.ty, .value),
            };
        }
        return out;
    }

    fn mergeTags(
        self: *PlatformAppRelationTypeResolver,
        platform_tags: []const CheckedTag,
        app_tags: []const CheckedTag,
    ) Allocator.Error![]const CheckedTag {
        var tags = std.ArrayList(CheckedTag).empty;
        errdefer deinitCheckedTags(self.allocator, tags.items);

        for (platform_tags) |platform_tag| {
            const args = if (findTag(self.names, app_tags, platform_tag.name)) |app_tag| blk: {
                if (platform_tag.args.len != app_tag.args.len) {
                    checkedArtifactInvariant("platform/app relation tag payload arity mismatch", .{});
                }
                break :blk try self.mergeRootSlices(platform_tag.args, app_tag.args);
            } else try self.finalizeRootSlice(platform_tag.args);
            errdefer self.allocator.free(args);
            try tags.append(self.allocator, .{
                .name = platform_tag.name,
                .args = args,
            });
        }
        for (app_tags) |app_tag| {
            if (findTag(self.names, platform_tags, app_tag.name) != null) continue;
            const args = try self.finalizeRootSlice(app_tag.args);
            errdefer self.allocator.free(args);
            try tags.append(self.allocator, .{
                .name = app_tag.name,
                .args = args,
            });
        }
        std.mem.sort(CheckedTag, tags.items, self.names, tagLessThanByName);
        return try tags.toOwnedSlice(self.allocator);
    }

    fn finalizeTags(
        self: *PlatformAppRelationTypeResolver,
        tags: []const CheckedTag,
    ) Allocator.Error![]const CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer deinitCheckedTags(self.allocator, out);
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = tag.name,
                .args = try self.finalizeRootSlice(tag.args),
            };
        }
        return out;
    }

    fn emptyRecordRoot(self: *PlatformAppRelationTypeResolver) Allocator.Error!CheckedTypeId {
        return try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .empty_record);
    }

    fn emptyTagUnionRoot(self: *PlatformAppRelationTypeResolver) Allocator.Error!CheckedTypeId {
        return try self.store.appendSyntheticPayloadRoot(self.allocator, self.names, .empty_tag_union);
    }

    fn payload(self: *const PlatformAppRelationTypeResolver, root: CheckedTypeId) CheckedTypePayload {
        const index: usize = @intFromEnum(root);
        if (index >= self.store.payloads.len) {
            checkedArtifactInvariant("platform/app relation referenced missing checked type payload", .{});
        }
        return self.store.payloads[index];
    }
};

fn checkedTypePayloadIsIdentity(payload: CheckedTypePayload) bool {
    return switch (payload) {
        .flex, .rigid => true,
        else => false,
    };
}

/// Public `formatCheckedTypeAlloc` function.
pub fn formatCheckedTypeAlloc(
    allocator: Allocator,
    artifact: *const CheckedModuleArtifact,
    root: CheckedTypeId,
) Allocator.Error![]const u8 {
    var buf = std.ArrayList(u8).empty;
    errdefer buf.deinit(allocator);
    var active = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active.deinit();
    try writeCheckedType(allocator, artifact, root, &buf, &active);
    return try buf.toOwnedSlice(allocator);
}

fn writeCheckedType(
    allocator: Allocator,
    artifact: *const CheckedModuleArtifact,
    root: CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!void {
    if (active.contains(root)) {
        try buf.appendSlice(allocator, "<recursive>");
        return;
    }
    try active.put(root, {});
    defer _ = active.remove(root);

    const index: usize = @intFromEnum(root);
    if (index >= artifact.checked_types.payloads.len) {
        checkedArtifactInvariant("checked type formatter referenced a missing payload", .{});
    }
    switch (artifact.checked_types.payloads[index]) {
        .pending => checkedArtifactInvariant("checked type formatter reached pending payload", .{}),
        .flex => |flex| try writeCheckedTypeVar(allocator, flex, buf),
        .rigid => |rigid| try writeCheckedTypeVar(allocator, rigid, buf),
        .alias => |alias| try writeCheckedType(allocator, artifact, alias.backing, buf, active),
        .empty_record => try buf.appendSlice(allocator, "{}"),
        .record => |record| try writeCheckedRecordType(allocator, artifact, record.fields, record.ext, buf, active),
        .record_unbound => |fields| try writeCheckedRecordType(allocator, artifact, fields, null, buf, active),
        .tuple => |items| {
            try buf.append(allocator, '(');
            for (items, 0..) |item, i| {
                if (i > 0) try buf.appendSlice(allocator, ", ");
                try writeCheckedType(allocator, artifact, item, buf, active);
            }
            try buf.append(allocator, ')');
        },
        .nominal => |nominal| {
            try buf.appendSlice(allocator, artifact.canonical_names.typeNameText(nominal.name));
            if (nominal.args.len > 0) {
                try buf.append(allocator, '(');
                for (nominal.args, 0..) |arg, i| {
                    if (i > 0) try buf.appendSlice(allocator, ", ");
                    try writeCheckedType(allocator, artifact, arg, buf, active);
                }
                try buf.append(allocator, ')');
            }
        },
        .function => |function| {
            if (function.args.len == 0) {
                try buf.appendSlice(allocator, "{}");
            } else {
                for (function.args, 0..) |arg, i| {
                    if (i > 0) try buf.appendSlice(allocator, ", ");
                    try writeCheckedType(allocator, artifact, arg, buf, active);
                }
            }
            try buf.appendSlice(allocator, if (finalizedFunctionKind(function.kind) == .effectful) " => " else " -> ");
            try writeCheckedType(allocator, artifact, function.ret, buf, active);
        },
        .empty_tag_union => try buf.appendSlice(allocator, "[]"),
        .tag_union => |tag_union| try writeCheckedTagUnionType(allocator, artifact, tag_union.tags, tag_union.ext, buf, active),
    }
}

fn writeCheckedTypeVar(
    allocator: Allocator,
    variable: CheckedTypeVariable,
    buf: *std.ArrayList(u8),
) Allocator.Error!void {
    if (variable.name) |name| {
        try buf.appendSlice(allocator, name);
    } else {
        try buf.appendSlice(allocator, "_");
    }
}

fn writeCheckedRecordType(
    allocator: Allocator,
    artifact: *const CheckedModuleArtifact,
    fields: []const CheckedRecordField,
    ext: ?CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!void {
    try buf.appendSlice(allocator, "{");
    var first = true;
    for (fields) |field| {
        if (!first) try buf.appendSlice(allocator, ",");
        first = false;
        try buf.append(allocator, ' ');
        try buf.appendSlice(allocator, artifact.canonical_names.recordFieldLabelText(field.name));
        try buf.appendSlice(allocator, " : ");
        try writeCheckedType(allocator, artifact, field.ty, buf, active);
    }
    if (ext) |ext_id| {
        if (!first) try buf.appendSlice(allocator, ",");
        try buf.appendSlice(allocator, " .. ");
        try writeCheckedType(allocator, artifact, ext_id, buf, active);
    } else if (!first) {
        try buf.append(allocator, ' ');
    }
    try buf.append(allocator, '}');
}

fn writeCheckedTagUnionType(
    allocator: Allocator,
    artifact: *const CheckedModuleArtifact,
    tags: []const CheckedTag,
    ext: CheckedTypeId,
    buf: *std.ArrayList(u8),
    active: *std.AutoHashMap(CheckedTypeId, void),
) Allocator.Error!void {
    try buf.append(allocator, '[');
    for (tags, 0..) |tag, i| {
        if (i > 0) try buf.appendSlice(allocator, ", ");
        try buf.appendSlice(allocator, artifact.canonical_names.tagLabelText(tag.name));
        if (tag.args.len > 0) {
            try buf.append(allocator, '(');
            for (tag.args, 0..) |arg, arg_i| {
                if (arg_i > 0) try buf.appendSlice(allocator, ", ");
                try writeCheckedType(allocator, artifact, arg, buf, active);
            }
            try buf.append(allocator, ')');
        }
    }
    const has_ext = switch (artifact.checked_types.payloads[@intFromEnum(ext)]) {
        .empty_tag_union => false,
        else => true,
    };
    if (has_ext) {
        if (tags.len > 0) try buf.appendSlice(allocator, ", ");
        try buf.appendSlice(allocator, ".. ");
        try writeCheckedType(allocator, artifact, ext, buf, active);
    }
    try buf.append(allocator, ']');
}

const RelationRecordParts = struct {
    fields: []const CheckedRecordField,
    ext: ?CheckedTypeId,
};

fn recordParts(payload: CheckedTypePayload) ?RelationRecordParts {
    return switch (payload) {
        .record => |record| .{ .fields = record.fields, .ext = record.ext },
        .record_unbound => |fields| .{ .fields = fields, .ext = null },
        .empty_record => .{ .fields = &.{}, .ext = null },
        else => null,
    };
}

fn findRecordField(
    names: *const canonical.CanonicalNameStore,
    fields: []const CheckedRecordField,
    name: canonical.RecordFieldLabelId,
) ?CheckedRecordField {
    for (fields) |field| {
        if (names.recordFieldLabelTextEql(field.name, name)) return field;
    }
    return null;
}

fn findTag(
    names: *const canonical.CanonicalNameStore,
    tags: []const CheckedTag,
    name: canonical.TagLabelId,
) ?CheckedTag {
    for (tags) |tag| {
        if (names.tagLabelTextEql(tag.name, name)) return tag;
    }
    return null;
}

fn recordFieldLessThanByName(
    names: *const canonical.CanonicalNameStore,
    lhs: CheckedRecordField,
    rhs: CheckedRecordField,
) bool {
    return names.recordFieldLabelTextLessThan(lhs.name, rhs.name);
}

fn tagLessThanByName(
    names: *const canonical.CanonicalNameStore,
    lhs: CheckedTag,
    rhs: CheckedTag,
) bool {
    return names.tagLabelTextLessThan(lhs.name, rhs.name);
}

fn appTopLevelValueByName(
    app_artifact: *const CheckedModuleArtifact,
    required_name: []const u8,
) ?TopLevelValueEntry {
    for (app_artifact.top_level_values.entries) |entry| {
        const app_name = app_artifact.canonical_names.exportNameText(entry.source_name);
        if (Ident.textEql(app_name, required_name)) return entry;
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
    source_ty_payload: CheckedTypeId,
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
    source_ty_payload: CheckedTypeId,
    source_ty: canonical.CanonicalTypeKey,
    instantiated_args: []const canonical.CanonicalTypeKey = &.{},
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
                .source_ty_payload = checked_types.roots[i].id,
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
                    .source_ty_payload = checked_types.roots[i].id,
                    .source_ty = source_key,
                    .instantiated_args = owned_args,
                });
                owned_args = &.{};
                break :blk id;
            };

            const published_payload = &checked_types.payloads[i];
            switch (published_payload.*) {
                .nominal => |*published_nominal| {
                    published_nominal.representation = .{ .local_box_payload_capability = .{
                        .capability = capability_id,
                        .opaque_atomic_proof = proof_id,
                    } };
                },
                else => checkedArtifactInvariant("nominal representation publication source payload stopped being nominal", .{}),
            }

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

    pub fn verifyComplete(self: *const ModuleInterfaceCapabilities) void {
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
    return std.meta.eql(a.bytes, b.bytes);
}

fn canonicalNominalTypeKeyEql(a: canonical.NominalTypeKey, b: canonical.NominalTypeKey) bool {
    return a.module_name == b.module_name and a.type_name == b.type_name;
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

/// Public `ComptimeRootId` declaration.
pub const ComptimeRootId = enum(u32) { _ };

/// Public `CompileTimeRootKind` declaration.
pub const CompileTimeRootKind = enum {
    constant,
    callable_binding,
    expect,
};

/// Public `CompileTimeRootPayload` declaration.
pub const CompileTimeRootPayload = union(enum) {
    pending,
    const_node: ConstNodeId,
    fn_value: ConstFnId,
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
    payload: CompileTimeRootPayload,
};

/// Public `CompileTimeRootTable` declaration.
pub const CompileTimeRootTable = struct {
    roots: []CompileTimeRoot = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        global_value_defs: []const CIR.Def.Idx,
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

        for (global_value_defs) |def_idx| {
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

    pub fn lookupIdBySource(self: *const CompileTimeRootTable, source: RootSource) ?ComptimeRootId {
        for (self.roots) |entry| {
            if (rootSourceMatches(entry.source, source)) return entry.id;
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
            .payload = entry.payload,
        });
    }
};

fn verifyCompileTimeRootPayloadMatchesKind(kind: CompileTimeRootKind, payload: CompileTimeRootPayload) void {
    const matches = switch (kind) {
        .constant => switch (payload) {
            .const_node => true,
            else => false,
        },
        .callable_binding => switch (payload) {
            .fn_value => true,
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

/// Public `ConstRef` declaration.
pub const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: ConstOwner,
    template: ConstTemplateId,
    source_scheme: canonical.CanonicalTypeSchemeKey,
};

/// Return the checked module id that owns a compile-time constant.
pub fn constModuleId(ref: ConstRef) ModuleId {
    return ref.artifact;
}

/// Public `ConstOwner` declaration.
pub const ConstOwner = union(enum) {
    top_level_binding: ConstTopLevelOwner,
};

/// Public `ConstTopLevelOwner` declaration.
pub const ConstTopLevelOwner = struct {
    module_idx: u32,
    pattern: CheckedPatternId,
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
    by_pattern: []?u32 = &.{},
    by_def: []?u32 = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        global_value_defs: []const CIR.Def.Idx,
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

        const by_pattern = try allocator.alloc(?u32, checked_bodies.patterns.len);
        errdefer allocator.free(by_pattern);
        @memset(by_pattern, null);

        const by_def = try allocator.alloc(?u32, module.nodeCount());
        errdefer allocator.free(by_def);
        @memset(by_def, null);

        for (global_value_defs) |def_idx| {
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

            const entry_idx: u32 = @intCast(entries.items.len);
            try entries.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .def = def_idx,
                .pattern = checked_pattern,
                .source_name = source_name,
                .source_scheme = source_scheme,
                .value = value,
            });
            by_pattern[@intFromEnum(checked_pattern)] = entry_idx;
            by_def[@intFromEnum(def_idx)] = entry_idx;
        }

        return .{
            .entries = try entries.toOwnedSlice(allocator),
            .by_pattern = by_pattern,
            .by_def = by_def,
        };
    }

    pub fn lookupByPattern(self: *const TopLevelValueTable, pattern: CheckedPatternId) ?TopLevelValueEntry {
        const raw = @intFromEnum(pattern);
        if (raw >= self.by_pattern.len) return null;
        const idx = self.by_pattern[raw] orelse return null;
        return self.entries[idx];
    }

    pub fn lookupByDef(self: *const TopLevelValueTable, def: CIR.Def.Idx) ?TopLevelValueEntry {
        const raw = @intFromEnum(def);
        if (raw >= self.by_def.len) return null;
        const idx = self.by_def[raw] orelse return null;
        return self.entries[idx];
    }

    pub fn deinit(self: *TopLevelValueTable, allocator: Allocator) void {
        allocator.free(self.by_def);
        allocator.free(self.by_pattern);
        allocator.free(self.entries);
        self.* = .{};
    }
};

/// Public `CheckedCallableBodyRef` declaration.
pub const CheckedCallableBodyRef = enum(u32) { _ };
/// Public `CheckedConstBodyRef` declaration.
pub const CheckedConstBodyRef = enum(u32) { _ };
/// Public `ConstTemplateId` declaration.
pub const ConstTemplateId = enum(u32) { _ };

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

fn collectPublicApiDependencies(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
    exported_defs: []const CIR.Def.Idx,
    checked_type_publication: *const CheckedTypePublication,
    checked_types: *const CheckedTypeStore,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    exported_procedure_templates: *const ExportedProcedureTemplateTable,
    exported_procedure_bindings: *const ExportedProcedureBindingTable,
    exported_const_templates: *const ExportedConstTemplateTable,
) Allocator.Error!PublicApiDependencies {
    var keys = std.ArrayList(CheckedModuleArtifactKey).empty;
    errdefer keys.deinit(allocator);

    var active_types = std.AutoHashMap(CheckedTypeId, void).init(allocator);
    defer active_types.deinit();

    for (exported_defs) |def_idx| {
        const root = checked_type_publication.rootForSourceVar(module, module.defType(def_idx)) orelse {
            checkedArtifactInvariant("exported def type root was not published", .{});
        };
        try appendPublicApiTypeDependencies(
            allocator,
            names,
            module_identity,
            artifact_key,
            checked_types,
            root,
            &active_types,
            imports,
            available_artifacts,
            &keys,
        );
    }

    try appendExposedTypeDeclarationPublicApiDependencies(
        allocator,
        module,
        names,
        module_identity,
        artifact_key,
        checked_type_publication,
        checked_types,
        imports,
        available_artifacts,
        &active_types,
        &keys,
    );

    try appendExportedClosurePublicApiDependencies(allocator, artifact_key, imports, available_artifacts, &keys, exported_procedure_templates.*);
    for (exported_procedure_bindings.bindings) |binding| {
        try appendTemplateClosurePublicApiDependencies(allocator, artifact_key, imports, available_artifacts, &keys, binding.template_closure);
    }
    for (exported_const_templates.templates) |template| {
        try appendTemplateClosurePublicApiDependencies(allocator, artifact_key, imports, available_artifacts, &keys, template.template_closure);
    }

    return .{ .artifacts = try keys.toOwnedSlice(allocator) };
}

fn appendExposedTypeDeclarationPublicApiDependencies(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
    checked_type_publication: *const CheckedTypePublication,
    checked_types: *const CheckedTypeStore,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    active_types: *std.AutoHashMap(CheckedTypeId, void),
    keys: *std.ArrayList(CheckedModuleArtifactKey),
) Allocator.Error!void {
    const module_env = module.moduleEnvConst();
    var exposed_iter = module_env.common.exposed_items.iterator();
    while (exposed_iter.next()) |entry| {
        if (entry.node_idx == 0) continue;
        const raw_node_idx: u32 = entry.node_idx;
        if (raw_node_idx >= module.nodeCount()) {
            checkedArtifactInvariant("exposed type item points at out-of-range node", .{});
        }

        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (!isStatementNodeTag(module.nodeTag(node_idx))) continue;

        const statement_idx: CIR.Statement.Idx = @enumFromInt(raw_node_idx);
        switch (module.getStatement(statement_idx)) {
            .s_alias_decl, .s_nominal_decl => {
                const root = checked_type_publication.rootForSourceVar(module, ModuleEnv.varFrom(statement_idx)) orelse {
                    checkedArtifactInvariant("exposed type declaration root was not published", .{});
                };
                try appendPublicApiTypeDependencies(
                    allocator,
                    names,
                    module_identity,
                    artifact_key,
                    checked_types,
                    root,
                    active_types,
                    imports,
                    available_artifacts,
                    keys,
                );
            },
            else => {},
        }
    }
}

fn appendPublicApiTypeDependencies(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    root: CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
) Allocator.Error!void {
    const index: usize = @intFromEnum(root);
    if (index >= checked_types.payloads.len) {
        checkedArtifactInvariant("public API dependency scan referenced a missing checked type payload", .{});
    }
    if (active.contains(root)) return;
    try active.put(root, {});
    defer _ = active.remove(root);

    switch (checked_types.payloads[index]) {
        .pending => checkedArtifactInvariant("public API dependency scan reached pending checked type payload", .{}),
        .empty_record, .empty_tag_union => {},
        .flex => |flex| try appendPublicApiConstraintDependencies(
            allocator,
            names,
            module_identity,
            artifact_key,
            checked_types,
            flex.constraints,
            active,
            imports,
            available_artifacts,
            keys,
        ),
        .rigid => |rigid| try appendPublicApiConstraintDependencies(
            allocator,
            names,
            module_identity,
            artifact_key,
            checked_types,
            rigid.constraints,
            active,
            imports,
            available_artifacts,
            keys,
        ),
        .alias => |alias| {
            try appendPublicApiModuleDependency(
                allocator,
                names,
                module_identity,
                artifact_key,
                alias.origin_module,
                imports,
                available_artifacts,
                keys,
            );
            try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, alias.backing, active, imports, available_artifacts, keys);
            try appendPublicApiTypeDependencyRange(allocator, names, module_identity, artifact_key, checked_types, alias.args, active, imports, available_artifacts, keys);
        },
        .nominal => |nominal| {
            try appendPublicApiModuleDependency(
                allocator,
                names,
                module_identity,
                artifact_key,
                nominal.origin_module,
                imports,
                available_artifacts,
                keys,
            );
            try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, nominal.backing, active, imports, available_artifacts, keys);
            try appendPublicApiTypeDependencyRange(allocator, names, module_identity, artifact_key, checked_types, nominal.args, active, imports, available_artifacts, keys);
        },
        .record => |record| {
            for (record.fields) |field| {
                try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, field.ty, active, imports, available_artifacts, keys);
            }
            try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, record.ext, active, imports, available_artifacts, keys);
        },
        .record_unbound => |fields| {
            for (fields) |field| {
                try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, field.ty, active, imports, available_artifacts, keys);
            }
        },
        .tuple => |items| try appendPublicApiTypeDependencyRange(allocator, names, module_identity, artifact_key, checked_types, items, active, imports, available_artifacts, keys),
        .function => |function| {
            try appendPublicApiTypeDependencyRange(allocator, names, module_identity, artifact_key, checked_types, function.args, active, imports, available_artifacts, keys);
            try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, function.ret, active, imports, available_artifacts, keys);
        },
        .tag_union => |tag_union| {
            for (tag_union.tags) |tag| {
                try appendPublicApiTypeDependencyRange(allocator, names, module_identity, artifact_key, checked_types, tag.args, active, imports, available_artifacts, keys);
            }
            try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, tag_union.ext, active, imports, available_artifacts, keys);
        },
    }
}

fn appendPublicApiConstraintDependencies(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    constraints: []const CheckedStaticDispatchConstraint,
    active: *std.AutoHashMap(CheckedTypeId, void),
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
) Allocator.Error!void {
    for (constraints) |constraint| {
        try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, constraint.fn_ty, active, imports, available_artifacts, keys);
    }
}

fn appendPublicApiTypeDependencyRange(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
    checked_types: *const CheckedTypeStore,
    roots: []const CheckedTypeId,
    active: *std.AutoHashMap(CheckedTypeId, void),
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
) Allocator.Error!void {
    for (roots) |child| {
        try appendPublicApiTypeDependencies(allocator, names, module_identity, artifact_key, checked_types, child, active, imports, available_artifacts, keys);
    }
}

fn appendPublicApiModuleDependency(
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
    origin_module: canonical.ModuleNameId,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
) Allocator.Error!void {
    const origin_name = names.moduleNameText(origin_module);
    if (isSelfPublicApiModuleName(names, module_identity, origin_name)) return;
    if (Ident.textEql(origin_name, "Builtin")) return;

    const view = publicApiDependencyViewByModuleName(origin_name, imports, available_artifacts) orelse {
        checkedArtifactInvariant("public API dependency scan could not find checked artifact for module {s}", .{origin_name});
    };
    try appendPublicApiDependencyView(allocator, artifact_key, keys, view);
}

fn isSelfPublicApiModuleName(
    names: *const canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    origin_name: []const u8,
) bool {
    return Ident.textEql(origin_name, names.moduleNameText(module_identity.module_name)) or
        Ident.textEql(origin_name, names.moduleNameText(module_identity.display_module_name)) or
        Ident.textEql(origin_name, names.moduleNameText(module_identity.qualified_module_name));
}

fn publicApiDependencyViewByModuleName(
    module_name: []const u8,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
) ?ImportedModuleView {
    for (imports) |import_artifact| {
        if (importedViewModuleNameMatches(import_artifact.view, module_name)) return import_artifact.view;
    }
    for (available_artifacts) |view| {
        if (importedViewModuleNameMatches(view, module_name)) return view;
    }
    return null;
}

fn importedViewModuleNameMatches(view: ImportedModuleView, module_name: []const u8) bool {
    return Ident.textEql(module_name, view.canonical_names.moduleNameText(view.module_identity.module_name)) or
        Ident.textEql(module_name, view.canonical_names.moduleNameText(view.module_identity.display_module_name)) or
        Ident.textEql(module_name, view.canonical_names.moduleNameText(view.module_identity.qualified_module_name));
}

fn appendPublicApiDependencyView(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    view: ImportedModuleView,
) Allocator.Error!void {
    if (checkedArtifactKeyEql(view.key, artifact_key)) return;
    try appendClosureArtifactKey(allocator, keys, view.key);
    for (view.public_api_dependencies.artifacts) |dependency| {
        if (checkedArtifactKeyEql(dependency, artifact_key)) continue;
        try appendClosureArtifactKey(allocator, keys, dependency);
    }
}

fn appendExportedClosurePublicApiDependencies(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    exported_procedure_templates: ExportedProcedureTemplateTable,
) Allocator.Error!void {
    for (exported_procedure_templates.templates) |template| {
        try appendTemplateClosurePublicApiDependencies(allocator, artifact_key, imports, available_artifacts, keys, template.template_closure);
    }
}

fn appendTemplateClosurePublicApiDependencies(
    allocator: Allocator,
    artifact_key: CheckedModuleArtifactKey,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
    keys: *std.ArrayList(CheckedModuleArtifactKey),
    closure: ImportedTemplateClosureView,
) Allocator.Error!void {
    var closure_keys = std.ArrayList(CheckedModuleArtifactKey).empty;
    defer closure_keys.deinit(allocator);
    try appendImportedTemplateClosureArtifactKeys(allocator, &closure_keys, closure);
    for (closure_keys.items) |key| {
        if (checkedArtifactKeyEql(key, artifact_key)) continue;
        const view = publicApiDependencyViewByKey(key, imports, available_artifacts) orelse {
            checkedArtifactInvariant("public API closure dependency scan could not find checked artifact for referenced key", .{});
        };
        try appendPublicApiDependencyView(allocator, artifact_key, keys, view);
    }
}

fn publicApiDependencyViewByKey(
    key: CheckedModuleArtifactKey,
    imports: []const PublishImportArtifact,
    available_artifacts: []const ImportedModuleView,
) ?ImportedModuleView {
    for (imports) |import_artifact| {
        if (checkedArtifactKeyEql(import_artifact.key, key)) return import_artifact.view;
    }
    for (available_artifacts) |view| {
        if (checkedArtifactKeyEql(view.key, key)) return view;
    }
    return null;
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
        callable_eval_templates: *const CallableEvalTemplateTable,
        entry_wrappers: *const EntryWrapperTable,
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
                callable_eval_templates,
                entry_wrappers,
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
    callable_eval_templates: *const CallableEvalTemplateTable,
    entry_wrappers: *const EntryWrapperTable,
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
        callable_eval_templates,
        entry_wrappers,
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
    callable_eval_template_table: *const CallableEvalTemplateTable,
    entry_wrappers: *const EntryWrapperTable,
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
        callable_eval_templates: *const CallableEvalTemplateTable,
        entry_wrappers: *const EntryWrapperTable,
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
            .callable_eval_template_table = callable_eval_templates,
            .entry_wrappers = entry_wrappers,
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
            if (existing.body == ref.body and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
        }
        try self.checked_bodies.append(self.allocator, ref);
    }

    fn appendCheckedConstBody(self: *ImportedTemplateClosureBuilder, body: CheckedConstBodyRef) Allocator.Error!void {
        const ref = ArtifactCheckedConstBodyRef{ .artifact = self.artifact_key, .body = body };
        for (self.checked_const_bodies.items) |existing| {
            if (existing.body == ref.body and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
        }
        try self.checked_const_bodies.append(self.allocator, ref);
    }

    fn appendCheckedTypeRoot(self: *ImportedTemplateClosureBuilder, ty: CheckedTypeId) Allocator.Error!void {
        const ref = ArtifactCheckedTypeRef{ .artifact = self.artifact_key, .ty = ty };
        for (self.checked_type_roots.items) |existing| {
            if (existing.ty == ref.ty and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
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
            if (existing.scheme == ref.scheme and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
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
            if (existing.table.start == table.start and existing.table.len == table.len and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
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
            if (existing.table.start == table.start and existing.table.len == table.len and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
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
            if (existing.table.start == table.start and existing.table.len == table.len and std.meta.eql(existing.artifact.bytes, ref.artifact.bytes)) return;
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
            if (try self.appendCallableEvalBindingClosureForResolvedRef(resolved)) continue;
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
            if (!std.meta.eql(import.key.bytes, ref.artifact.bytes)) continue;
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
            if (!std.meta.eql(import.key.bytes, ref.artifact.bytes)) continue;
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

        if (!std.meta.eql(const_ref.artifact.bytes, self.artifact_key.bytes)) return;

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
            .stored_const => {},
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
            .imported => null,
        };
    }

    fn appendCallableEvalBindingClosureForResolvedRef(
        self: *ImportedTemplateClosureBuilder,
        ref: ResolvedValueRef,
    ) Allocator.Error!bool {
        const use: ProcedureUseTemplate = switch (ref) {
            .top_level_proc,
            .promoted_top_level_proc,
            => |procedure| procedure,
            else => return false,
        };
        const top_level = switch (use.binding) {
            .top_level => |binding| binding,
            else => return false,
        };
        if (!std.meta.eql(top_level.artifact.bytes, self.artifact_key.bytes)) {
            checkedArtifactInvariant("top-level callable-eval closure dependency referenced a different artifact", .{});
        }
        const binding = self.top_level_bindings.get(top_level.binding);
        const template_id = switch (binding.body) {
            .direct_template => return false,
            .callable_eval_template => |template| template,
        };
        try self.appendCallableEvalBindingTemplate(template_id);
        return true;
    }

    fn appendCallableEvalBindingTemplate(
        self: *ImportedTemplateClosureBuilder,
        template_id: CallableEvalTemplateId,
    ) Allocator.Error!void {
        const template = self.callable_eval_template_table.get(template_id);
        const wrapper = entryWrapperForRoot(self.entry_wrappers, template.root);
        const entry_template = self.checked_templates.get(wrapper.template.template);

        try self.appendCheckedTypeRoot(template.checked_fn_root);
        try self.appendCheckedTypeScheme(template.source_scheme);
        try self.appendTemplate(wrapper.template, entry_template);
        try appendUniqueValue(ArtifactCallableEvalTemplateRef, self.allocator, &self.callable_eval_templates, .{
            .artifact = self.artifact_key,
            .template = template_id,
        });
    }

    fn constRefForResolvedValueRef(
        _: *ImportedTemplateClosureBuilder,
        ref: ResolvedValueRef,
    ) ?ConstRef {
        return switch (ref) {
            .top_level_const,
            => |const_use| const_use.const_ref,
            else => null,
        };
    }

    fn templateForTopLevelBinding(
        self: *ImportedTemplateClosureBuilder,
        binding_ref: ArtifactTopLevelProcedureBindingRef,
    ) ?canonical.ProcedureTemplateRef {
        if (!std.meta.eql(binding_ref.artifact.bytes, self.artifact_key.bytes)) {
            checkedArtifactInvariant("top-level procedure closure dependency referenced a different artifact", .{});
        }
        const binding = self.top_level_bindings.get(binding_ref.binding);
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
    callable_eval_templates: *const CallableEvalTemplateTable,
    entry_wrappers: *const EntryWrapperTable,
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
        callable_eval_templates,
        entry_wrappers,
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

pub fn deinitImportedTemplateClosure(
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
    freeConstSlice(allocator, closure.nested_proc_sites);
    freeConstSlice(allocator, closure.resolved_value_refs);
    freeConstSlice(allocator, closure.static_dispatch_plans);
    freeConstSlice(allocator, closure.method_registry_entries);
    freeConstSlice(allocator, closure.interface_capabilities);
    closure.* = .{};
}

/// Clone an imported-template closure view so it can be owned by a new artifact record.
pub fn cloneImportedTemplateClosure(
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
        _: TypedCIR.Module,
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
                callable_eval_templates,
                entry_wrappers,
                const_templates,
                resolved_value_refs,
                procedure_bindings,
                platform_required_bindings,
                imports,
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
    callable_eval_templates: *const CallableEvalTemplateTable,
    entry_wrappers: *const EntryWrapperTable,
    const_templates: *const ConstTemplateTable,
    resolved_value_refs: *const ResolvedValueRefTable,
    top_level_bindings: *const TopLevelProcedureBindingTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    imports: []const PublishImportArtifact,
    body: ProcedureBindingBody,
) Allocator.Error!ImportedTemplateClosureView {
    return switch (body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template_ref| buildImportedTemplateClosure(
                allocator,
                artifact_key,
                checked_types,
                checked_templates,
                callable_eval_templates,
                entry_wrappers,
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
                callable_eval_templates,
                entry_wrappers,
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
                callable_eval_templates,
                entry_wrappers,
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

/// Public `StoredConstTemplate` declaration.
pub const StoredConstTemplate = struct {
    node: ConstNodeId,
};

/// Public `ConstTemplateState` declaration.
pub const ConstTemplateState = union(enum) {
    reserved,
    eval_template: ConstEvalTemplate,
    stored_const: StoredConstTemplate,
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

    pub fn fillEval(
        self: *ConstTemplateTable,
        ref: ConstRef,
        template: ConstEvalTemplate,
    ) void {
        const record = self.recordForRef(ref);
        if (!std.meta.eql(record.source_scheme.bytes, template.source_scheme.bytes)) {
            checkedArtifactInvariant("constant eval template source scheme does not match reserved ConstRef", .{});
        }
        switch (record.state) {
            .reserved => record.state = .{ .eval_template = template },
            .eval_template, .stored_const => checkedArtifactInvariant("constant template was filled twice", .{}),
        }
    }

    pub fn fillStoredConst(
        self: *ConstTemplateTable,
        ref: ConstRef,
        template: StoredConstTemplate,
    ) void {
        const record = self.recordForRef(ref);
        switch (record.state) {
            .reserved, .eval_template => record.state = .{ .stored_const = template },
            .stored_const => checkedArtifactInvariant("constant template was filled twice", .{}),
        }
    }

    pub fn get(self: *const ConstTemplateTable, ref: ConstRef) ConstTemplate {
        const idx = @intFromEnum(ref.template);
        if (idx >= self.templates.items.len) {
            checkedArtifactInvariant("ConstRef template id is out of range", .{});
        }
        const template = self.templates.items[idx];
        if (!constOwnerEql(template.owner, ref.owner) or
            !std.meta.eql(template.source_scheme.bytes, ref.source_scheme.bytes))
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
                .eval_template, .stored_const => {},
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
            !std.meta.eql(record.source_scheme.bytes, ref.source_scheme.bytes))
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
        callable_eval_templates: *const CallableEvalTemplateTable,
        entry_wrappers: *const EntryWrapperTable,
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
                callable_eval_templates,
                entry_wrappers,
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

    pub fn fillStoredConst(
        self: *ExportedConstTemplateTable,
        ref: ConstRef,
        template: StoredConstTemplate,
    ) void {
        for (self.templates) |*entry| {
            if (!constRefEql(entry.const_ref, ref)) continue;
            entry.template.state = .{ .stored_const = template };
            return;
        }
    }

    pub fn view(self: *const ExportedConstTemplateTable) ExportedConstTemplateView {
        return .{ .templates = self.templates };
    }
};

fn checkedArtifactInvariant(comptime message: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("checked artifact invariant violated: " ++ message, args);
    }
    unreachable;
}

fn checkedArtifactKeyEql(a: CheckedModuleArtifactKey, b: CheckedModuleArtifactKey) bool {
    return std.meta.eql(a.bytes, b.bytes);
}

fn closureArtifactRefIsLocal(
    artifact: *const CheckedModuleArtifact,
    referenced: CheckedModuleArtifactKey,
) bool {
    return checkedArtifactKeyEql(referenced, artifact.key);
}

fn constRefEql(a: ConstRef, b: ConstRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        constOwnerEql(a.owner, b.owner) and
        a.template == b.template and
        std.meta.eql(a.source_scheme.bytes, b.source_scheme.bytes);
}

fn importedProcedureBindingRefEql(a: ImportedProcedureBindingRef, b: ImportedProcedureBindingRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        a.def == b.def and
        a.pattern == b.pattern;
}

fn artifactTopLevelProcedureBindingRefEql(a: ArtifactTopLevelProcedureBindingRef, b: ArtifactTopLevelProcedureBindingRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and a.binding == b.binding;
}

fn topLevelValueRefEql(a: TopLevelValueRef, b: TopLevelValueRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and a.pattern == b.pattern;
}

fn hostedProcRefEql(a: HostedProcRef, b: HostedProcRef) bool {
    return a.module_idx == b.module_idx and
        a.def == b.def and
        canonical.procedureValueRefEql(a.proc, b.proc) and
        canonical.procedureTemplateRefEql(a.template, b.template);
}

fn requiredAppProcedureRefEql(a: RequiredAppProcedureRef, b: RequiredAppProcedureRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        topLevelValueRefEql(a.app_value, b.app_value) and
        a.procedure_binding == b.procedure_binding;
}

fn constOwnerEql(a: ConstOwner, b: ConstOwner) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .top_level_binding => |left| blk: {
            const right = b.top_level_binding;
            break :blk left.module_idx == right.module_idx and left.pattern == right.pattern;
        },
    };
}

fn constRefTopLevelOwner(ref: ConstRef) ?ConstTopLevelOwner {
    return switch (ref.owner) {
        .top_level_binding => |owner| owner,
    };
}

/// Public `procedureBindingRefEql` function.
pub fn procedureBindingRefEql(a: ProcedureBindingRef, b: ProcedureBindingRef) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .top_level => |left| artifactTopLevelProcedureBindingRefEql(left, b.top_level),
        .imported => |left| importedProcedureBindingRefEql(left, b.imported),
        .hosted => |left| hostedProcRefEql(left, b.hosted),
        .platform_required => |left| requiredAppProcedureRefEql(left, b.platform_required),
    };
}

/// Public `CheckedModuleArtifact` declaration.
pub const CheckedModuleArtifact = struct {
    key: CheckedModuleArtifactKey,
    canonical_names: canonical.CanonicalNameStore,
    module_identity: ModuleIdentity,
    checking_context_identity: CheckingContextIdentity,
    direct_import_artifact_keys: []CheckedModuleArtifactKey = &.{},
    public_api_dependencies: PublicApiDependencies = .{},
    module_env: ModuleEnvStorage,
    exports: ExportTable,
    checked_types: CheckedTypeStore = .{},
    checked_bodies: CheckedBodyStore = .{},
    checked_const_bodies: CheckedConstBodyTable = .{},
    exported_procedure_templates: ExportedProcedureTemplateTable = .{},
    exported_procedure_bindings: ExportedProcedureBindingTable = .{},
    exported_const_templates: ExportedConstTemplateTable = .{},
    provides_requires: ProvidesRequiresMetadata,
    provided_exports: ProvidedExportTable = .{},
    method_registry: static_dispatch.MethodRegistry,
    static_dispatch_plans: static_dispatch.StaticDispatchPlanTable,
    resolved_value_refs: ResolvedValueRefTable,
    nested_proc_sites: NestedProcSiteTable = .{},
    checked_procedure_templates: CheckedProcedureTemplateTable,
    entry_wrappers: EntryWrapperTable = .{},
    intrinsic_wrappers: IntrinsicWrapperTable = .{},
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
    const_templates: ConstTemplateTable,
    const_store: ConstStore,

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

    /// Look up the root request with the given metadata order.
    /// Returns null if not found (callers that rely on an invariant can assert).
    pub fn lookupRootRequestByOrder(self: *const CheckedModuleArtifact, order: u32) ?RootRequest {
        for (self.root_requests.requests) |request| {
            if (request.order == order) return request;
        }
        return null;
    }

    /// Name of a `provided_export` root's published FFI symbol, or null if absent.
    pub fn providedEntrypointName(self: *const CheckedModuleArtifact, root: RootRequest) ?[]const u8 {
        const def_idx = switch (root.source) {
            .def => |def| def,
            else => return null,
        };
        const top_level = self.top_level_values.lookupByDef(def_idx) orelse return null;
        for (self.provides_requires.provides) |entry| {
            if (entry.source_name == top_level.source_name) {
                return self.canonical_names.externalSymbolNameText(entry.ffi_symbol);
            }
        }
        return null;
    }

    /// Name of a `platform_required_binding` root's exported symbol, or null if absent.
    pub fn requiredEntrypointName(self: *const CheckedModuleArtifact, root: RootRequest) ?[]const u8 {
        const binding_id = switch (root.source) {
            .required_binding => |id| id,
            else => return null,
        };
        const binding = self.platform_required_bindings.lookupByBindingId(binding_id) orelse return null;
        const declaration = self.platform_required_declarations.lookupByDeclarationId(binding.declaration) orelse return null;
        return self.canonical_names.exportNameText(declaration.platform_name);
    }

    /// Dispatch to the right name lookup based on the root kind. Returns null
    /// if the root kind isn't an entrypoint kind or the lookup fails.
    pub fn entrypointNameForRoot(self: *const CheckedModuleArtifact, root: RootRequest) ?[]const u8 {
        return switch (root.kind) {
            .provided_export => self.providedEntrypointName(root),
            .platform_required_binding => self.requiredEntrypointName(root),
            else => null,
        };
    }

    pub fn deinit(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.deinitInternal(allocator, true);
    }

    pub fn deinitRetainingModuleEnv(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.deinitInternal(allocator, false);
    }

    fn deinitInternal(self: *CheckedModuleArtifact, allocator: Allocator, comptime deinit_module_env: bool) void {
        self.const_store.deinit();
        self.const_templates.deinit(allocator);
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
        self.intrinsic_wrappers.deinit(allocator);
        self.entry_wrappers.deinit(allocator);
        self.checked_procedure_templates.deinit(allocator);
        self.nested_proc_sites.deinit(allocator);
        self.resolved_value_refs.deinit(allocator);
        self.static_dispatch_plans.deinit(allocator);
        self.method_registry.deinit(allocator);
        self.provided_exports.deinit(allocator);
        self.provides_requires.deinit(allocator);
        self.exported_const_templates.deinit(allocator);
        self.exported_procedure_bindings.deinit(allocator);
        self.exported_procedure_templates.deinit(allocator);
        self.checked_const_bodies.deinit(allocator);
        self.checked_bodies.deinit(allocator);
        self.checked_types.deinit(allocator);
        self.exports.deinit(allocator);
        self.public_api_dependencies.deinit(allocator);
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
        verifyRootRequestSubsets(self.root_requests);

        for (self.checked_types.payloads, 0..) |payload, i| {
            switch (payload) {
                .pending => std.debug.panic("checked artifact invariant violated: checked type payload {d} was not filled before compile-time lowering", .{i}),
                else => {},
            }
        }

        for (self.checked_bodies.exprs, 0..) |expr, i| {
            std.debug.assert(@intFromEnum(expr.id) == i);
            std.debug.assert(@intFromEnum(expr.ty) < self.checked_types.roots.len);
            verifyCheckedExprDataComplete(expr.data);
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

    pub fn verifyComplete(self: *const CheckedModuleArtifact) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.module_identity.module_idx != std.math.maxInt(u32));
        verifyRootRequestSubsets(self.root_requests);

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

        std.debug.assert(std.meta.eql(self.key.direct_import_artifact_keys_hash, hashDirectImportArtifactKeys(self.direct_import_artifact_keys)));

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
            verifyCheckedExprDataComplete(expr.data);
        }

        for (self.checked_bodies.patterns, 0..) |pattern, i| {
            std.debug.assert(@intFromEnum(pattern.id) == i);
            std.debug.assert(@intFromEnum(pattern.ty) < self.checked_types.roots.len);
            verifyCheckedPatternDataComplete(pattern.data);
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
            verifyCheckedStatementDataComplete(statement.data);
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
                .intrinsic_wrapper => |wrapper_id| {
                    const wrapper = self.intrinsic_wrappers.get(wrapper_id);
                    std.debug.assert(wrapper.checked_fn_root == template.checked_fn_root);
                    std.debug.assert(wrapper.template.template == template.template_id);
                    std.debug.assert(wrapper.template.proc_base == template.proc_base);
                    std.debug.assert(std.meta.eql(wrapper.template.artifact.bytes, self.key.bytes));
                },
                .entry_wrapper => |wrapper_id| {
                    const wrapper = self.entry_wrappers.get(wrapper_id);
                    std.debug.assert(@intFromEnum(wrapper.body_expr) < self.checked_bodies.exprs.len);
                    std.debug.assert(@intFromEnum(wrapper.checked_fn_root) < self.checked_types.roots.len);
                    std.debug.assert(wrapper.checked_fn_root == template.checked_fn_root);
                    std.debug.assert(wrapper.template.template == template.template_id);
                    std.debug.assert(wrapper.template.proc_base == template.proc_base);
                    std.debug.assert(std.meta.eql(wrapper.template.artifact.bytes, self.key.bytes));
                },
            }

            const nested_end = template.nested_proc_sites.start + template.nested_proc_sites.len;
            std.debug.assert(nested_end <= self.nested_proc_sites.template_refs.len);
            for (self.nested_proc_sites.template_refs[template.nested_proc_sites.start..nested_end]) |site_id| {
                std.debug.assert(@intFromEnum(site_id) < self.nested_proc_sites.sites.len);
                const site = self.nested_proc_sites.sites[@intFromEnum(site_id)];
                std.debug.assert(site.owner_template.template == template.template_id);
                std.debug.assert(site.owner_template.proc_base == template.proc_base);
                std.debug.assert(std.meta.eql(site.owner_template.artifact.bytes, self.key.bytes));
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
            std.debug.assert(std.meta.eql(exported.template.artifact.bytes, self.key.bytes));
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
        }

        for (self.exported_procedure_bindings.bindings) |exported| {
            std.debug.assert(std.meta.eql(exported.binding.artifact.bytes, self.key.bytes));
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
        }

        for (self.exported_const_templates.templates) |exported| {
            std.debug.assert(std.meta.eql(exported.const_ref.artifact.bytes, self.key.bytes));
            std.debug.assert(@intFromEnum(exported.const_ref.template) < self.const_templates.templates.items.len);
            std.debug.assert(exported.template_closure.const_templates.len > 0);
            std.debug.assert(exported.template_closure.checked_type_roots.len > 0);
            std.debug.assert(exported.template_closure.checked_type_schemes.len > 0);
            std.debug.assert(exported.template_closure.interface_capabilities.len > 0);
            switch (exported.template.state) {
                .eval_template => |eval| {
                    std.debug.assert(@intFromEnum(eval.body) < self.checked_const_bodies.bodies.len);
                    std.debug.assert(std.meta.eql(eval.entry_template.artifact.bytes, self.key.bytes));
                    std.debug.assert(@intFromEnum(eval.entry_template.template) < self.checked_procedure_templates.templates.len);
                    std.debug.assert(exported.template_closure.checked_const_bodies.len > 0);
                    std.debug.assert(exported.template_closure.checked_procedure_templates.len > 0);
                },
                .stored_const => |stored| {
                    std.debug.assert(@intFromEnum(stored.node) < self.const_store.values.items.len);
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
                    std.debug.assert(std.meta.eql(const_ref.source_scheme.bytes, entry.source_scheme.bytes));
                },
                .procedure_binding => |binding_ref| {
                    const binding = self.top_level_procedure_bindings.get(binding_ref);
                    switch (binding.body) {
                        .direct_template => |direct| {
                            _ = self.canonical_names.procBase(direct.proc_value.proc_base);
                            switch (direct.template) {
                                .checked => |template| {
                                    std.debug.assert(template.proc_base == direct.proc_value.proc_base);
                                    std.debug.assert(std.meta.eql(template.artifact.bytes, direct.proc_value.artifact.bytes));
                                },
                                .synthetic => |synthetic| {
                                    std.debug.assert(synthetic.template.proc_base == direct.proc_value.proc_base);
                                    std.debug.assert(std.meta.eql(synthetic.template.artifact.bytes, direct.proc_value.artifact.bytes));
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

        if (self.provided_exports.exports.len != self.provides_requires.provides.len) {
            std.debug.panic("checked artifact invariant violated: provided export table does not match provides metadata", .{});
        }
        for (self.provided_exports.exports, self.provides_requires.provides) |provided, metadata| {
            switch (provided) {
                .procedure => |procedure| {
                    std.debug.assert(procedure.source_name == metadata.source_name);
                    std.debug.assert(procedure.ffi_symbol == metadata.ffi_symbol);
                    std.debug.assert(@intFromEnum(procedure.checked_type) < self.checked_types.roots.len);
                    const top_level = self.top_level_values.lookupByDef(procedure.def) orelse {
                        std.debug.panic("checked artifact invariant violated: provided procedure export references missing top-level value", .{});
                    };
                    std.debug.assert(top_level.pattern == procedure.pattern);
                    std.debug.assert(top_level.source_name == procedure.source_name);
                    std.debug.assert(std.meta.eql(top_level.source_scheme.bytes, procedure.source_scheme.bytes));
                    switch (top_level.value) {
                        .procedure_binding => |binding| std.debug.assert(binding == procedure.binding),
                        .const_ref => std.debug.panic("checked artifact invariant violated: provided procedure export references const top-level value", .{}),
                    }
                },
                .data => |data| {
                    std.debug.assert(data.source_name == metadata.source_name);
                    std.debug.assert(data.ffi_symbol == metadata.ffi_symbol);
                    std.debug.assert(@intFromEnum(data.checked_type) < self.checked_types.roots.len);
                    const top_level = self.top_level_values.lookupByDef(data.def) orelse {
                        std.debug.panic("checked artifact invariant violated: provided data export references missing top-level value", .{});
                    };
                    std.debug.assert(top_level.pattern == data.pattern);
                    std.debug.assert(top_level.source_name == data.source_name);
                    std.debug.assert(std.meta.eql(top_level.source_scheme.bytes, data.source_scheme.bytes));
                    switch (top_level.value) {
                        .const_ref => |const_ref| std.debug.assert(constRefEql(const_ref, data.const_ref)),
                        .procedure_binding => std.debug.panic("checked artifact invariant violated: provided data export references procedure top-level value", .{}),
                    }
                },
            }
        }

        self.const_templates.verifySealed();
        self.const_store.verifyComplete();
        self.interface_capabilities.verifyComplete();
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
/// Short name for checked module data.
pub const Module = CheckedModuleArtifact;

fn verifyPlatformRequiredValueUse(binding: PlatformRequiredBinding) void {
    if (builtin.mode != .Debug) return;

    switch (binding.value_use) {
        .const_value => |const_use| {
            std.debug.assert(std.meta.eql(const_use.const_use.const_ref.artifact.bytes, binding.app_value.artifact.bytes));
            const owner = constRefTopLevelOwner(const_use.const_use.const_ref) orelse {
                std.debug.panic("checked artifact invariant violated: platform-required const use referenced a non-top-level ConstRef", .{});
            };
            std.debug.assert(owner.pattern == binding.app_value.pattern);
        },
        .procedure_value => |proc_use| switch (proc_use.procedure.binding) {
            .platform_required => |required| {
                std.debug.assert(std.meta.eql(required.artifact.bytes, binding.app_value.artifact.bytes));
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
    public_api_dependencies: PublicApiDependencies = .{},
    exports: ExportTableView,
    checked_types: CheckedTypeStoreView,
    checked_bodies: CheckedBodyStoreView,
    checked_const_bodies: *const CheckedConstBodyTable,
    checked_procedure_templates: *const CheckedProcedureTemplateTable,
    compile_time_roots: *const CompileTimeRootTable,
    entry_wrappers: *const EntryWrapperTable,
    intrinsic_wrappers: *const IntrinsicWrapperTable,
    resolved_value_refs: *const ResolvedValueRefTable,
    nested_proc_sites: *const NestedProcSiteTable,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    hosted_procs: *const HostedProcTable,
    exported_procedure_templates: ExportedProcedureTemplateView,
    exported_procedure_bindings: ExportedProcedureBindingView,
    exported_const_templates: ExportedConstTemplateView,
    provided_exports: *const ProvidedExportTable,
    top_level_procedure_bindings: *const TopLevelProcedureBindingTable,
    platform_required_bindings: *const PlatformRequiredBindingTable,
    callable_eval_templates: CallableEvalTemplateTableView,
    const_templates: *const ConstTemplateTable,
    method_registry: *const static_dispatch.MethodRegistry,
    interface_capabilities: *const ModuleInterfaceCapabilities,
    const_store: *const ConstStore,
};

/// Public `LoweringModuleView` declaration.
pub const LoweringModuleView = struct {
    module: *const CheckedModuleArtifact,
    roots: *const RootRequestTable,
    relation_modules: []const ImportedModuleView = &.{},
};

/// Public `importedView` function.
pub fn importedView(artifact: *const CheckedModuleArtifact) ImportedModuleView {
    return .{
        .key = artifact.key,
        .module_env = artifact.moduleEnvConst(),
        .canonical_names = &artifact.canonical_names,
        .module_identity = artifact.module_identity,
        .direct_import_artifact_keys = artifact.direct_import_artifact_keys,
        .public_api_dependencies = artifact.public_api_dependencies,
        .exports = artifact.exports.view(),
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .checked_const_bodies = &artifact.checked_const_bodies,
        .checked_procedure_templates = &artifact.checked_procedure_templates,
        .compile_time_roots = &artifact.compile_time_roots,
        .entry_wrappers = &artifact.entry_wrappers,
        .intrinsic_wrappers = &artifact.intrinsic_wrappers,
        .resolved_value_refs = &artifact.resolved_value_refs,
        .nested_proc_sites = &artifact.nested_proc_sites,
        .static_dispatch_plans = &artifact.static_dispatch_plans,
        .hosted_procs = &artifact.hosted_procs,
        .exported_procedure_templates = artifact.exported_procedure_templates.view(),
        .exported_procedure_bindings = artifact.exported_procedure_bindings.view(),
        .exported_const_templates = artifact.exported_const_templates.view(),
        .provided_exports = &artifact.provided_exports,
        .top_level_procedure_bindings = &artifact.top_level_procedure_bindings,
        .platform_required_bindings = &artifact.platform_required_bindings,
        .callable_eval_templates = artifact.callable_eval_templates.view(),
        .const_templates = &artifact.const_templates,
        .method_registry = &artifact.method_registry,
        .interface_capabilities = &artifact.interface_capabilities,
        .const_store = &artifact.const_store,
    };
}

/// Return checked boundary names for an imported module view.
pub fn importedNames(view: ImportedModuleView) *const canonical.NameStore {
    return view.canonical_names;
}

const ProjectedCheckedTypeKey = struct {
    artifact: [32]u8,
    ty: u32,
};

/// Public `ArtifactNamePublisher` declaration.
///
/// Checking finalization uses this boundary when checked module data must record
/// names that came from post-check lowering. The caller stores only ids owned by
/// `target`; no lowering-run label id may cross this boundary into checked
/// module data.
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
        return Ident.textEql(
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
        return Ident.textEql(
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
/// capture resolution plans.
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

    pub fn publishedNominalBackingForType(
        self: *const CheckedTypeProjector,
        _: CheckedTypeId,
        nominal: CheckedNominalType,
    ) ?CheckedTypeId {
        return switch (nominal.representation) {
            .builtin,
            .local_declaration,
            .imported_declaration,
            => nominal.backing,
            .local_box_payload_capability => |capability| self.target.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty,
            .imported_box_payload_capability,
            .opaque_without_backing,
            => null,
        };
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
            if (std.meta.eql(root.key.bytes, key.bytes)) break root.id;
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
                .representation = remapViewNominalRepresentation(nominal.representation),
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
            .row_default = variable.row_default,
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
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
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

    fn remapViewNominalRepresentation(
        representation: CheckedNominalRepresentationRef,
    ) CheckedNominalRepresentationRef {
        return switch (representation) {
            .builtin => |builtin_id| .{ .builtin = builtin_id },
            .local_declaration => |declaration| .{ .local_declaration = declaration },
            .imported_declaration => |imported| .{ .imported_declaration = imported },
            .local_box_payload_capability => |capability| .{ .local_box_payload_capability = capability },
            .imported_box_payload_capability => |capability| .{ .imported_box_payload_capability = capability },
            .opaque_without_backing => .opaque_without_backing,
        };
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

    pub fn projectImportedCheckedType(
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
            .row_default = variable.row_default,
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
            .representation = try self.remapImportedNominalRepresentation(imported, nominal),
            .args = args,
        } };
    }

    fn remapImportedNominalRepresentation(
        _: *CheckedTypeProjector,
        imported: ImportedModuleView,
        nominal: CheckedNominalType,
    ) Allocator.Error!CheckedNominalRepresentationRef {
        return switch (nominal.representation) {
            .builtin => |builtin_id| .{ .builtin = builtin_id },
            .local_declaration => |declaration| .{ .imported_declaration = .{
                .artifact = imported.key,
                .declaration = declaration,
            } },
            .imported_declaration => |imported_decl| .{ .imported_declaration = imported_decl },
            .local_box_payload_capability => |capability| .{ .imported_box_payload_capability = .{
                .artifact = imported.key,
                .capability = capability.capability,
                .opaque_atomic_proof = capability.opaque_atomic_proof,
            } },
            .imported_box_payload_capability => |capability| .{ .imported_box_payload_capability = capability },
            .opaque_without_backing => .opaque_without_backing,
        };
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
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
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

    fn remapMethodOwner(
        self: *CheckedTypeProjector,
        imported: ImportedModuleView,
        owner: static_dispatch.MethodOwner,
    ) Allocator.Error!static_dispatch.MethodOwner {
        return switch (owner) {
            .builtin => |builtin_owner| .{ .builtin = builtin_owner },
            .nominal => |nominal| .{ .nominal = .{
                .module_name = try self.remapModuleName(imported, nominal.module_name),
                .type_name = try self.remapTypeName(imported, nominal.type_name),
            } },
        };
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
                .representation = try self.remapNominalRepresentation(nominal),
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
            .row_default = variable.row_default,
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
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
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

    fn remapMethodOwner(
        self: *CheckedTypeStoreImportProjector,
        owner: static_dispatch.MethodOwner,
    ) Allocator.Error!static_dispatch.MethodOwner {
        return switch (owner) {
            .builtin => |builtin_owner| .{ .builtin = builtin_owner },
            .nominal => |nominal| .{ .nominal = .{
                .module_name = try self.remapModuleName(nominal.module_name),
                .type_name = try self.remapTypeName(nominal.type_name),
            } },
        };
    }

    fn remapNominalRepresentation(
        self: *CheckedTypeStoreImportProjector,
        nominal: CheckedNominalType,
    ) Allocator.Error!CheckedNominalRepresentationRef {
        return switch (nominal.representation) {
            .builtin => |builtin_id| .{ .builtin = builtin_id },
            .local_declaration => |declaration| .{ .imported_declaration = .{
                .artifact = self.imported.key,
                .declaration = declaration,
            } },
            .imported_declaration => |imported_decl| .{ .imported_declaration = imported_decl },
            .local_box_payload_capability => |capability| .{ .imported_box_payload_capability = .{
                .artifact = self.imported.key,
                .capability = capability.capability,
                .opaque_atomic_proof = capability.opaque_atomic_proof,
            } },
            .imported_box_payload_capability => |capability| .{ .imported_box_payload_capability = capability },
            .opaque_without_backing => .opaque_without_backing,
        };
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
            .expr_typed_num_from_numeral,
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
                    .e_typed_num_from_numeral => |typed| try visitor.typeName(typed.type_name),
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
        .module = artifact,
        .roots = &artifact.root_requests,
        .relation_modules = &.{},
    };
}

/// Public `loweringViewWithRelations` function.
pub fn loweringViewWithRelations(
    artifact: *const CheckedModuleArtifact,
    relation_artifacts: []const ImportedModuleView,
) LoweringModuleView {
    return .{
        .module = artifact,
        .roots = &artifact.root_requests,
        .relation_modules = relation_artifacts,
    };
}

pub const CheckedModuleKeyInputs = struct {
    imports: []const PublishImportArtifact = &.{},
    platform_requirement_context: ?PlatformRequirementContextKey = null,
    platform_app_relation: ?PlatformAppRelationKey = null,
};

pub fn checkedModuleKeyFromTypedModule(
    allocator: Allocator,
    modules: *const TypedCIR.Modules,
    module_idx: u32,
    inputs: CheckedModuleKeyInputs,
) Allocator.Error!CheckedModuleArtifactKey {
    const module = modules.module(module_idx);
    const module_env = module.moduleEnvConst();
    const idents = module.identStoreConst();

    var canonical_names = canonical.CanonicalNameStore.init(allocator);
    defer canonical_names.deinit();
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

    var checking_context_identity = try CheckingContextIdentity.fromModule(
        allocator,
        module,
        inputs.imports,
        inputs.platform_requirement_context,
        inputs.platform_app_relation,
    );
    defer checking_context_identity.deinit(allocator);

    const direct_import_artifact_keys = try directImportArtifactKeysFromModule(allocator, module, inputs.imports);
    defer allocator.free(direct_import_artifact_keys);

    return CheckedModuleArtifactKey.compute(
        module_env.getSourceAll(),
        module_identity,
        checking_context_identity,
        direct_import_artifact_keys,
    );
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

    var checked_type_publication = try CheckedTypeStore.fromModule(allocator, module, &canonical_names, inputs.imports, inputs.available_artifacts);
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

    const global_value_defs = module_env.store.sliceDefs(module_env.global_value_defs);

    var intrinsic_wrappers = IntrinsicWrapperTable{};
    errdefer intrinsic_wrappers.deinit(allocator);

    var checked_procedure_templates = try CheckedProcedureTemplateTable.fromModule(
        allocator,
        module,
        global_value_defs,
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
    checked_bodies.attachNumeralPlans(&static_dispatch_plans);
    checked_bodies.attachIteratorForPlans(&static_dispatch_plans);

    var hosted_procs = try HostedProcTable.fromModule(allocator, module, global_value_defs, &canonical_names, &checked_procedure_templates);
    errdefer hosted_procs.deinit(allocator);

    var platform_requirement_relations = try PlatformRequirementRelationTable.fromRelation(
        allocator,
        module,
        module_identity,
        &canonical_names,
        &checked_type_publication,
        &platform_required_declarations,
        inputs.relation_artifacts,
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

    var compile_time_roots = try CompileTimeRootTable.fromModule(allocator, module, global_value_defs, &checked_type_publication, &checked_bodies, &checked_procedure_templates);
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

    var checked_const_store = ConstStore.init(allocator);
    errdefer checked_const_store.deinit();

    var top_level_procedure_bindings = TopLevelProcedureBindingTable.initEmpty();
    errdefer top_level_procedure_bindings.deinit(allocator);

    var callable_eval_templates = CallableEvalTemplateTable{};
    errdefer callable_eval_templates.deinit(allocator);

    var const_templates = ConstTemplateTable{};
    errdefer const_templates.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(
        allocator,
        module,
        global_value_defs,
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
    checked_bodies.attachResolvedValueRefs(
        &resolved_value_refs,
        artifact_key,
        &top_level_procedure_bindings,
        inputs.imports,
        inputs.relation_artifacts,
    );

    var provided_exports = try ProvidedExportTable.fromModule(
        allocator,
        module,
        &checked_type_publication,
        &top_level_values,
        provides,
    );
    errdefer provided_exports.deinit(allocator);

    var root_requests = try RootRequestTable.fromModule(
        allocator,
        module,
        &checked_type_publication,
        &compile_time_roots,
        &checked_procedure_templates,
        &entry_wrappers,
        &platform_required_bindings,
        &provided_exports,
        &checked_bodies,
        &resolved_value_refs,
        &top_level_procedure_bindings,
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
        &callable_eval_templates,
        &entry_wrappers,
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
        &callable_eval_templates,
        &entry_wrappers,
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

    var public_api_dependencies = try collectPublicApiDependencies(
        allocator,
        module,
        &canonical_names,
        module_identity,
        artifact_key,
        exports,
        &checked_type_publication,
        checked_types,
        inputs.imports,
        inputs.available_artifacts,
        &exported_procedure_templates,
        &exported_procedure_bindings,
        &exported_const_templates,
    );
    errdefer public_api_dependencies.deinit(allocator);

    var artifact = CheckedModuleArtifact{
        .key = artifact_key,
        .canonical_names = canonical_names,
        .module_identity = module_identity,
        .checking_context_identity = checking_context_identity,
        .direct_import_artifact_keys = direct_import_artifact_keys,
        .public_api_dependencies = public_api_dependencies,
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
        .provided_exports = provided_exports,
        .method_registry = method_registry,
        .static_dispatch_plans = static_dispatch_plans,
        .resolved_value_refs = resolved_value_refs,
        .nested_proc_sites = nested_proc_sites,
        .checked_procedure_templates = checked_procedure_templates,
        .entry_wrappers = entry_wrappers,
        .intrinsic_wrappers = intrinsic_wrappers,
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
        .const_templates = const_templates,
        .const_store = checked_const_store,
    };
    try inputs.compile_time_finalizer.run(
        allocator,
        &artifact,
        inputs.imports,
        inputs.available_artifacts,
        inputs.relation_artifacts,
    );
    artifact.verifyComplete();
    return artifact;
}

const ProvidedExportKindExpectation = struct {
    procedure_roots: usize,
    data_exports: usize,
    procedure_exports: usize,
};

fn expectProvidedExportKind(
    source: []const u8,
    expected: ProvidedExportKindExpectation,
) !void {
    const testing = std.testing;
    const TestEnv = @import("test/TestEnv.zig");
    const allocator = testing.allocator;

    var test_env = try TestEnv.init("PlatformDataExportTest", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const source_modules = [_]TypedCIR.Modules.SourceModule{
        .{ .precompiled = test_env.module_env },
        .{ .precompiled = test_env.builtin_module.env },
    };
    var modules = try TypedCIR.Modules.init(allocator, &source_modules);
    defer modules.deinit();

    const module = modules.module(0);
    const module_env = module.moduleEnvConst();

    const builtin_module = modules.module(1);
    const builtin_env = builtin_module.moduleEnvConst();
    var builtin_names = canonical.CanonicalNameStore.init(allocator);
    defer builtin_names.deinit();
    try internLoweringVisibleNames(builtin_env, &builtin_names);
    const builtin_module_name = try builtin_names.internModuleName(builtin_env.module_name);
    const builtin_identity = ModuleIdentity{
        .stable_hash = computeStableModuleIdentityHash(builtin_env),
        .module_idx = builtin_module.moduleIndex(),
        .module_name = builtin_module_name,
        .display_module_name = builtin_module_name,
        .qualified_module_name = builtin_module_name,
        .kind = builtin_env.module_kind,
    };
    const builtin_key = CheckedModuleArtifactKey.compute(
        builtin_env.getSourceAll(),
        builtin_identity,
        .{},
        &.{},
    );
    var builtin_checked_type_publication = try CheckedTypeStore.fromModule(allocator, builtin_module, &builtin_names, &.{}, &.{});
    defer builtin_checked_type_publication.deinit(allocator);
    const empty_checked_bodies = CheckedBodyStore{};
    const empty_checked_const_bodies = CheckedConstBodyTable{};
    const empty_checked_procedure_templates = CheckedProcedureTemplateTable{};
    const empty_compile_time_roots = CompileTimeRootTable{};
    const empty_entry_wrappers = EntryWrapperTable{};
    const empty_intrinsic_wrappers = IntrinsicWrapperTable{};
    const empty_resolved_value_refs = ResolvedValueRefTable{};
    const empty_nested_proc_sites = NestedProcSiteTable{};
    const empty_static_dispatch_plans = static_dispatch.StaticDispatchPlanTable{};
    const empty_hosted_procs = HostedProcTable{};
    const empty_exported_procedure_templates = ExportedProcedureTemplateTable{};
    const empty_exported_procedure_bindings = ExportedProcedureBindingTable{};
    const empty_exported_const_templates = ExportedConstTemplateTable{};
    const empty_provided_exports = ProvidedExportTable{};
    const empty_top_level_procedure_bindings = TopLevelProcedureBindingTable{};
    const empty_platform_required_bindings = PlatformRequiredBindingTable{};
    const empty_callable_eval_templates = CallableEvalTemplateTable{};
    const empty_const_templates = ConstTemplateTable{};
    const empty_method_registry = static_dispatch.MethodRegistry{};
    const empty_interface_capabilities = ModuleInterfaceCapabilities{};
    var empty_const_store = ConstStore.init(allocator);
    defer empty_const_store.deinit();

    const builtin_view = ImportedModuleView{
        .key = builtin_key,
        .module_env = builtin_env,
        .canonical_names = &builtin_names,
        .module_identity = builtin_identity,
        .exports = .{},
        .checked_types = builtin_checked_type_publication.store.view(),
        .checked_bodies = empty_checked_bodies.view(),
        .checked_const_bodies = &empty_checked_const_bodies,
        .checked_procedure_templates = &empty_checked_procedure_templates,
        .compile_time_roots = &empty_compile_time_roots,
        .entry_wrappers = &empty_entry_wrappers,
        .intrinsic_wrappers = &empty_intrinsic_wrappers,
        .resolved_value_refs = &empty_resolved_value_refs,
        .nested_proc_sites = &empty_nested_proc_sites,
        .static_dispatch_plans = &empty_static_dispatch_plans,
        .hosted_procs = &empty_hosted_procs,
        .exported_procedure_templates = empty_exported_procedure_templates.view(),
        .exported_procedure_bindings = empty_exported_procedure_bindings.view(),
        .exported_const_templates = empty_exported_const_templates.view(),
        .provided_exports = &empty_provided_exports,
        .top_level_procedure_bindings = &empty_top_level_procedure_bindings,
        .platform_required_bindings = &empty_platform_required_bindings,
        .callable_eval_templates = empty_callable_eval_templates.view(),
        .const_templates = &empty_const_templates,
        .method_registry = &empty_method_registry,
        .interface_capabilities = &empty_interface_capabilities,
        .const_store = &empty_const_store,
    };

    const builtin_imports = [_]PublishImportArtifact{.{
        .module_idx = 1,
        .key = builtin_key,
        .view = builtin_view,
    }};

    var canonical_names = canonical.CanonicalNameStore.init(allocator);
    defer canonical_names.deinit();
    try internLoweringVisibleNames(module_env, &canonical_names);

    const module_name = try canonical_names.internModuleName(module_env.module_name);
    const display_module_name = try canonical_names.internModuleIdent(module.identStoreConst(), module_env.display_module_name_idx);
    const qualified_module_name = try canonical_names.internModuleIdent(module.identStoreConst(), module_env.qualified_module_ident);
    const module_identity = ModuleIdentity{
        .stable_hash = computeStableModuleIdentityHash(module_env),
        .module_idx = module.moduleIndex(),
        .module_name = module_name,
        .display_module_name = display_module_name,
        .qualified_module_name = qualified_module_name,
        .kind = module_env.module_kind,
    };

    const artifact_key = CheckedModuleArtifactKey{};
    const owner_artifact = artifactRef(artifact_key);

    var platform_required_declarations = try PlatformRequiredDeclarationTable.fromModule(allocator, module, &canonical_names);
    defer platform_required_declarations.deinit(allocator);

    var checked_type_publication = try CheckedTypeStore.fromModule(allocator, module, &canonical_names, &builtin_imports, &.{});
    defer checked_type_publication.deinit(allocator);
    const checked_types = &checked_type_publication.store;

    var checked_bodies = try CheckedBodyStore.fromModule(allocator, module, &canonical_names, &checked_type_publication);
    defer checked_bodies.deinit(allocator);

    const global_value_defs = module_env.store.sliceDefs(module_env.global_value_defs);

    var intrinsic_wrappers = IntrinsicWrapperTable{};
    defer intrinsic_wrappers.deinit(allocator);

    var checked_procedure_templates = try CheckedProcedureTemplateTable.fromModule(
        allocator,
        module,
        global_value_defs,
        &canonical_names,
        owner_artifact,
        &checked_type_publication,
        &checked_bodies,
        &intrinsic_wrappers,
    );
    defer checked_procedure_templates.deinit(allocator);

    var hosted_procs = try HostedProcTable.fromModule(allocator, module, global_value_defs, &canonical_names, &checked_procedure_templates);
    defer hosted_procs.deinit(allocator);

    var platform_requirement_relations = try PlatformRequirementRelationTable.fromRelation(
        allocator,
        module,
        module_identity,
        &canonical_names,
        &checked_type_publication,
        &platform_required_declarations,
        &.{},
        null,
    );
    defer platform_requirement_relations.deinit(allocator);

    var platform_required_bindings = try PlatformRequiredBindingTable.fromRelation(
        allocator,
        module,
        module_identity,
        &canonical_names,
        &platform_required_declarations,
        &platform_requirement_relations,
        null,
    );
    defer platform_required_bindings.deinit(allocator);

    var compile_time_roots = try CompileTimeRootTable.fromModule(
        allocator,
        module,
        global_value_defs,
        &checked_type_publication,
        &checked_bodies,
        &checked_procedure_templates,
    );
    defer compile_time_roots.deinit(allocator);

    var entry_wrappers = EntryWrapperTable{};
    defer entry_wrappers.deinit(allocator);
    try checked_procedure_templates.appendEntryWrappersForRoots(
        allocator,
        module,
        &canonical_names,
        owner_artifact,
        checked_types,
        &entry_wrappers,
        &compile_time_roots,
    );

    var callable_eval_templates = CallableEvalTemplateTable{};
    defer callable_eval_templates.deinit(allocator);

    var top_level_procedure_bindings = TopLevelProcedureBindingTable.initEmpty();
    defer top_level_procedure_bindings.deinit(allocator);

    var const_templates = ConstTemplateTable{};
    defer const_templates.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(
        allocator,
        module,
        global_value_defs,
        &canonical_names,
        &checked_bodies,
        &checked_procedure_templates,
        &callable_eval_templates,
        &top_level_procedure_bindings,
        &const_templates,
        artifact_key,
        &compile_time_roots,
    );
    defer top_level_values.deinit(allocator);

    const provides = try publishProvidesMetadata(allocator, module_env, &canonical_names);
    defer allocator.free(provides);

    var resolved_value_refs = try ResolvedValueRefTable.fromModule(
        allocator,
        &modules,
        module.moduleIndex(),
        artifact_key,
        &builtin_imports,
        &checked_procedure_templates,
        &hosted_procs,
        &platform_required_declarations,
        &platform_required_bindings,
        &top_level_values,
        &checked_type_publication,
        &checked_bodies,
    );
    defer resolved_value_refs.deinit(allocator);

    var provided_exports = try ProvidedExportTable.fromModule(
        allocator,
        module,
        &checked_type_publication,
        &top_level_values,
        provides,
    );
    defer provided_exports.deinit(allocator);

    var root_requests = try RootRequestTable.fromModule(
        allocator,
        module,
        &checked_type_publication,
        &compile_time_roots,
        &checked_procedure_templates,
        &entry_wrappers,
        &platform_required_bindings,
        &provided_exports,
        &checked_bodies,
        &resolved_value_refs,
        &top_level_procedure_bindings,
        &.{},
    );
    defer root_requests.deinit(allocator);

    var provided_runtime_roots: usize = 0;
    for (root_requests.requests) |root| {
        if (root.kind == .provided_export) provided_runtime_roots += 1;
    }

    var provided_data_exports: usize = 0;
    var provided_procedure_exports: usize = 0;
    for (provided_exports.exports) |provided| {
        switch (provided) {
            .data => provided_data_exports += 1,
            .procedure => provided_procedure_exports += 1,
        }
    }

    try std.testing.expectEqual(expected.procedure_roots, provided_runtime_roots);
    try std.testing.expectEqual(expected.data_exports, provided_data_exports);
    try std.testing.expectEqual(expected.procedure_exports, provided_procedure_exports);
}

fn unionFieldCount(comptime Union: type) usize {
    return @typeInfo(Union).@"union".fields.len;
}

test "compile-time finalization route is explicit and non-optional" {
    try std.testing.expect(@hasField(PublishInputs, "compile_time_finalizer"));
    try std.testing.expect(@FieldType(PublishInputs, "compile_time_finalizer") == CompileTimeFinalizer);
    try std.testing.expect(@hasField(CompileTimeFinalizer, "finalize"));
    try std.testing.expect(@hasField(CompileTimeFinalizer, "context"));

    try std.testing.expect(std.meta.stringToEnum(RootRequestKind, "compile_time_constant") != null);
    try std.testing.expect(std.meta.stringToEnum(RootRequestKind, "compile_time_callable") != null);
    try std.testing.expect(std.meta.stringToEnum(RootAbi, "compile_time") != null);

    try std.testing.expect(@hasField(RootRequestTable, "runtime_requests"));
    try std.testing.expect(@hasField(RootRequestTable, "compile_time_requests"));
}

test "compile-time roots and top-level values store final checked output only" {
    try std.testing.expect(std.meta.stringToEnum(CompileTimeRootKind, "constant") != null);
    try std.testing.expect(std.meta.stringToEnum(CompileTimeRootKind, "callable_binding") != null);
    try std.testing.expect(std.meta.stringToEnum(CompileTimeRootKind, "expect") != null);

    try std.testing.expect(@hasField(CompileTimeRootPayload, "pending"));
    try std.testing.expect(@hasField(CompileTimeRootPayload, "const_node"));
    try std.testing.expect(@hasField(CompileTimeRootPayload, "fn_value"));
    try std.testing.expect(@hasField(CompileTimeRootPayload, "expect"));

    try std.testing.expectEqual(@as(usize, 2), unionFieldCount(TopLevelValueKind));
    try std.testing.expect(@hasField(TopLevelValueKind, "const_ref"));
    try std.testing.expect(@hasField(TopLevelValueKind, "procedure_binding"));
    try std.testing.expect(!@hasField(TopLevelValueKind, "runtime_thunk"));
    try std.testing.expect(!@hasField(TopLevelValueKind, "global_initializer"));
    try std.testing.expect(!@hasField(TopLevelValueKind, "top_level_closure_object"));
}

test "constant template states contain sealed value data but no runtime initializer concepts" {
    try std.testing.expectEqual(@as(usize, 3), unionFieldCount(ConstTemplateState));
    try std.testing.expect(@hasField(ConstTemplateState, "reserved"));
    try std.testing.expect(@hasField(ConstTemplateState, "eval_template"));
    try std.testing.expect(@hasField(ConstTemplateState, "stored_const"));

    try std.testing.expect(!@hasField(ConstTemplateState, "runtime_thunk"));
    try std.testing.expect(!@hasField(ConstTemplateState, "global_initializer"));
    try std.testing.expect(!@hasField(ConstTemplateState, "initializer_proc"));
    try std.testing.expect(!@hasField(ConstTemplateState, "top_level_closure_object"));
}

test "checked module owns post-check inputs at the checked boundary" {
    try std.testing.expect(@hasField(CheckedModuleArtifact, "checked_types"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "checked_bodies"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "checked_const_bodies"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "exported_procedure_templates"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "exported_procedure_bindings"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "exported_const_templates"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "method_registry"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "static_dispatch_plans"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "resolved_value_refs"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "nested_proc_sites"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "checked_procedure_templates"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "entry_wrappers"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "intrinsic_wrappers"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "top_level_procedure_bindings"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "callable_eval_templates"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "root_requests"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "hosted_procs"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "platform_required_declarations"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "platform_required_bindings"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "compile_time_roots"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "top_level_values"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "const_templates"));
    try std.testing.expect(@hasField(CheckedModuleArtifact, "const_store"));
}

test "checked module keeps current compile-time ownership tables" {
    try std.testing.expect(@hasField(CallableEvalTemplateTable, "templates"));
    try std.testing.expect(@hasField(CallableEvalTemplate, "root"));
    try std.testing.expect(@hasField(CallableEvalTemplate, "checked_fn_root"));

    try std.testing.expect(@hasField(TopLevelProcedureBindingTable, "bindings"));
    try std.testing.expect(@hasField(ProcedureBindingBody, "direct_template"));
    try std.testing.expect(@hasField(ProcedureBindingBody, "callable_eval_template"));

    try std.testing.expect(@hasField(RootRequestTable, "requests"));
    try std.testing.expect(@hasField(RootRequestTable, "runtime_requests"));
    try std.testing.expect(@hasField(RootRequestTable, "compile_time_requests"));

    try std.testing.expect(@hasField(CompileTimeRootTable, "roots"));
    try std.testing.expect(@hasField(TopLevelValueTable, "entries"));
    try std.testing.expect(@hasField(ConstTemplateTable, "templates"));
    try std.testing.expect(@hasField(ConstStore, "values"));
    try std.testing.expect(@hasField(ConstStore, "fns"));
    try std.testing.expect(@hasField(ConstStore, "str_data"));
}

test "provided primitive constant is a data export, not a runtime root" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { answer_for_host: "answer" }
        \\
        \\answer_for_host : I64
        \\answer_for_host = 42
    ;

    try expectProvidedExportKind(source, .{
        .procedure_roots = 0,
        .data_exports = 1,
        .procedure_exports = 0,
    });
}

test "provided nested record constant is a data export, not a runtime root" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { profile_for_host: "profile" }
        \\
        \\profile_for_host : {
        \\    user : { name : Str, scores : List(I64) },
        \\    meta : { active : Bool, label : Str },
        \\}
        \\profile_for_host = {
        \\    user: { name: "Ada", scores: [10, 20, 30] },
        \\    meta: { active: True, label: "founder" },
        \\}
    ;

    try expectProvidedExportKind(source, .{
        .procedure_roots = 0,
        .data_exports = 1,
        .procedure_exports = 0,
    });
}

test "provided nested heap constant is a data export, not a runtime root" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { table_for_host: "table" }
        \\
        \\table_for_host : List(List(Str))
        \\table_for_host = [
        \\    ["alpha", "beta"],
        \\    [],
        \\    ["gamma", "delta", "epsilon"],
        \\]
    ;

    try expectProvidedExportKind(source, .{
        .procedure_roots = 0,
        .data_exports = 1,
        .procedure_exports = 0,
    });
}

test "provided recursive nominal constant is a data export, not a runtime root" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { tree_for_host: "tree" }
        \\
        \\Tree := [Leaf(I64), Node(Tree, Tree)]
        \\
        \\tree_for_host : Tree
        \\tree_for_host = Node(
        \\    Leaf(5),
        \\    Node(
        \\        Leaf(7),
        \\        Leaf(11),
        \\    ),
        \\)
    ;

    try expectProvidedExportKind(source, .{
        .procedure_roots = 0,
        .data_exports = 1,
        .procedure_exports = 0,
    });
}

test "provided callable-containing record constant is a data export, not a runtime root" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { table_for_host: "table" }
        \\
        \\I64ToI64 : I64 -> I64
        \\
        \\table_for_host : { f: I64ToI64 }
        \\table_for_host = { f: |value| value + 1 }
    ;

    try expectProvidedExportKind(source, .{
        .procedure_roots = 0,
        .data_exports = 1,
        .procedure_exports = 0,
    });
}

test "provided procedure remains a runtime root" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { add_one_for_host: "add_one" }
        \\
        \\add_one_for_host : I64 -> I64
        \\add_one_for_host = |value| value + 1
    ;

    try expectProvidedExportKind(source, .{
        .procedure_roots = 1,
        .data_exports = 0,
        .procedure_exports = 1,
    });
}

test "artifact views are read-only projections" {
    var names = canonical.CanonicalNameStore.init(std.testing.allocator);
    const test_module = try names.internModuleName("Test");

    var module_env = try ModuleEnv.init(std.testing.allocator, "");
    defer module_env.deinit();

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
        .module_env = .{ .checked_source = &module_env },
        .exports = .{},
        .provides_requires = .{},
        .method_registry = .{},
        .static_dispatch_plans = .{},
        .resolved_value_refs = .{},
        .checked_procedure_templates = .{},
        .intrinsic_wrappers = .{},
        .top_level_procedure_bindings = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_declarations = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{},
        .compile_time_roots = .{},
        .top_level_values = .{},
        .const_templates = .{},
        .const_store = ConstStore.init(std.testing.allocator),
    };
    defer {
        artifact.const_templates.deinit(std.testing.allocator);
        artifact.const_store.deinit();
        artifact.canonical_names.deinit();
    }

    const imported = importedView(&artifact);
    const lowering = loweringView(&artifact);
    try std.testing.expect(imported.exports.defs.ptr == artifact.exports.defs.ptr);
    try std.testing.expect(lowering.roots == &artifact.root_requests);
}

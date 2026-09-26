//! Stateless canonicalization, checking, and checked-artifact publication
//! operations used by the compilation coordinator and snapshot tooling.

const std = @import("std");
const build_options = @import("build_options");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const eval = @import("eval");
const compiled_builtins = @import("compiled_builtins");

const Check = check.Check;
const CheckedArtifact = check.CheckedArtifact;
const CheckedModules = check.TypedCIR.Modules;
const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const AST = parse.AST;
/// Deserialize BuiltinIndices from the binary data generated at build time
/// Timing information for different phases
pub const TimingInfo = struct {
    tokenize_parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    canonicalize_diagnostics_ns: u64 = 0,
    type_checking_ns: u64 = 0,
    check_diagnostics_ns: u64 = 0,
    compile_time_evaluation: eval.CompileTimeFinalization.TimingSnapshot = .{},
};
const Allocator = std.mem.Allocator;

const CoreCtx = @import("ctx").CoreCtx;

/// Errors that can occur while publishing compile-time finalization results.
pub const PublishError = CheckedArtifact.CompileTimeFinalizer.Error;
/// Errors that can occur while type-checking a module.
pub const TypeCheckModuleError = Allocator.Error || PublishError || error{Internal};

/// Build CTFE finalization options from the package compiler context.
pub fn compileTimeFinalizationOptions(
    max_threads: usize,
    roc_ctx: *CoreCtx,
    timing: ?*eval.CompileTimeFinalization.Timing,
) eval.CompileTimeFinalization.Options {
    return .{
        .max_threads = max_threads,
        .std_io = roc_ctx.std_io,
        .timing = timing,
        .stderr = .{
            .context = @ptrCast(roc_ctx),
            .write = writeCtfeStderr,
        },
    };
}

fn writeCtfeStderr(raw: ?*anyopaque, bytes: []const u8) void {
    const roc_ctx: *CoreCtx = @ptrCast(@alignCast(raw.?));
    roc_ctx.writeStderr(bytes) catch {};
}

// FileProvider was removed in favour of the unified Io abstraction (src/io/Io.zig).
// Callers that previously used FileProvider now use Io.readFile / Io.fileExists directly.

/// Build execution mode
pub const Mode = enum { single_threaded, multi_threaded };

/// Semantic facts retained for a checked module.
pub const SemanticModuleData = struct {
    env: *ModuleEnv,
    checked_artifact: ?*const CheckedArtifact.CheckedModuleArtifact,
};

/// Owned output from type checking before the coordinator takes publication.
pub const TypeCheckOutput = struct {
    checker: Check,
    publication: CheckedArtifact.CheckedModuleArtifact,
    publication_owned: bool = true,

    pub fn deinit(self: *TypeCheckOutput) void {
        if (self.publication_owned) self.publication.deinit(self.publication.canonical_names.allocator);
        self.checker.deinit();
    }

    pub fn takeCheckedArtifact(self: *TypeCheckOutput) CheckedArtifact.CheckedModuleArtifact {
        std.debug.assert(self.publication_owned);
        self.publication_owned = false;
        return self.publication;
    }
};

/// Owned output from checking a platform's requires surface.
pub const PlatformRequirementsCheckOutput = struct {
    checker: Check,

    pub fn deinit(self: *PlatformRequirementsCheckOutput) void {
        self.checker.deinit();
    }
};

/// Public `ArtifactPublicationInputs` declaration.
pub const ArtifactPublicationInputs = struct {
    available_artifacts: []const CheckedArtifact.ImportedModuleView = &.{},
    relation_artifacts: []const CheckedArtifact.ImportedModuleView = &.{},
    platform_requirement_context: ?CheckedArtifact.PlatformRequirementContextKey = null,
    platform_app_relation: ?CheckedArtifact.PlatformAppRelation = null,
    platform_requirement_solutions: []const check.RequirementSolution.SolutionInput = &.{},
    explicit_roots: []const CheckedArtifact.ExplicitRootRequestInput = &.{},
    hoisted_roots: []const check.HoistRoots.SelectedHoistedRoot = &.{},
    promoted_local_procedures: []const check.HoistRoots.PromotedLocalProcedure = &.{},
    problem_store: ?*check.problem.Store = null,
    ctfe_options: eval.CompileTimeFinalization.Options = .{},
    evaluation_phase: @FieldType(CheckedArtifact.PublishInputs, "evaluation_phase") = .immediate,
    /// How this module's compile-time roots were established. Part of the
    /// checked-artifact cache identity. See `Can.Validation`.
    validation: Can.Validation = .checking,
};

fn importedArtifactsCoverImportedEnvs(
    imported_envs: []const *ModuleEnv,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
) bool {
    for (imported_envs, 0..) |_, module_idx| {
        var found = false;
        for (imported_artifacts) |artifact| {
            if (artifact.module_idx == module_idx) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}

/// Build the semantic module-owner closure available while checking a module.
pub fn buildCheckOwnerEnvs(
    allocator: Allocator,
    imported_envs: []const *ModuleEnv,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    available_artifacts: []const CheckedArtifact.ImportedModuleView,
    platform_requirements: ?Check.PlatformRequirementInput,
) Allocator.Error![]const *const ModuleEnv {
    var owner_envs = std.ArrayList(*const ModuleEnv).empty;
    errdefer owner_envs.deinit(allocator);

    for (imported_envs) |env| {
        try appendCheckOwnerEnvIfMissing(allocator, &owner_envs, env);
    }

    if (platform_requirements) |requirements| {
        try appendCheckOwnerEnvIfMissing(allocator, &owner_envs, requirements.env);
        for (requirements.owner_modules) |owner_env| {
            try appendCheckOwnerEnvIfMissing(allocator, &owner_envs, owner_env);
        }
    }

    var seen_public_dependencies = std.AutoHashMap(CheckedArtifact.CheckedModuleArtifactKey, void).init(allocator);
    defer seen_public_dependencies.deinit();

    for (imported_artifacts) |imported_artifact| {
        try appendCheckOwnerEnvPublicDependencies(
            allocator,
            &owner_envs,
            available_artifacts,
            &seen_public_dependencies,
            imported_artifact.view,
        );
    }

    return try owner_envs.toOwnedSlice(allocator);
}

fn appendCheckOwnerEnvPublicDependencies(
    allocator: Allocator,
    owner_envs: *std.ArrayList(*const ModuleEnv),
    available_artifacts: []const CheckedArtifact.ImportedModuleView,
    seen_public_dependencies: *std.AutoHashMap(CheckedArtifact.CheckedModuleArtifactKey, void),
    view: CheckedArtifact.ImportedModuleView,
) Allocator.Error!void {
    for (view.public_api_dependencies.type_owner_artifacts) |dependency_key| {
        const entry = try seen_public_dependencies.getOrPut(dependency_key);
        if (entry.found_existing) continue;
        entry.value_ptr.* = {};

        const dependency = availableArtifactByKey(available_artifacts, dependency_key) orelse {
            std.debug.panic("compile.typeCheckModule missing public API dependency artifact for imported module", .{});
        };
        try appendCheckOwnerEnvIfMissing(allocator, owner_envs, dependency.module_env);
        try appendCheckOwnerEnvPublicDependencies(
            allocator,
            owner_envs,
            available_artifacts,
            seen_public_dependencies,
            dependency,
        );
    }
}

fn appendCheckOwnerEnvIfMissing(
    allocator: Allocator,
    owner_envs: *std.ArrayList(*const ModuleEnv),
    module_env: *const ModuleEnv,
) Allocator.Error!void {
    for (owner_envs.items) |existing| {
        if (moduleEnvIdentitiesMatch(existing, module_env)) return;
    }
    try owner_envs.append(allocator, module_env);
}

/// Two owner envs are duplicates exactly when their deep content identities
/// match: byte-identical transitive module content is interchangeable as a
/// type owner. No name text participates.
fn moduleEnvIdentitiesMatch(a: *const ModuleEnv, b: *const ModuleEnv) bool {
    if (@intFromPtr(a) == @intFromPtr(b)) return true;
    const a_hash = a.contentIdentityHash() orelse return false;
    const b_hash = b.contentIdentityHash() orelse return false;
    return base.ModuleIdentity.eql(a_hash, b_hash);
}

fn availableArtifactByKey(
    available_artifacts: []const CheckedArtifact.ImportedModuleView,
    key: CheckedArtifact.CheckedModuleArtifactKey,
) ?CheckedArtifact.ImportedModuleView {
    for (available_artifacts) |artifact| {
        if (std.mem.eql(u8, &artifact.key.bytes, &key.bytes)) return artifact;
    }
    return null;
}

/// Whether snapshot-tool canonicalization performs checking validation.
pub const SnapshotValidationMode = enum {
    none,
    checking,
};

/// Combined canonicalization and type checking function for snapshot tool
/// This ensures the SAME module_envs map is used for both phases
/// Snapshot-only type inspection must not publish post-check lowering input.
/// Checked-artifact publication owns compile-time evaluation for real builds.
/// IMPORTANT: The returned checker holds a pointer to module_envs_out, so caller must keep
/// module_envs_out alive until they're done using the checker (e.g., for type printing)
pub fn canonicalizeAndTypeCheckModule(
    roc_ctx: CoreCtx,
    gpa: Allocator,
    env: *ModuleEnv,
    parse_ast: *AST,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: can.CIR.BuiltinIndices,
    imported_envs: []const *ModuleEnv,
    module_envs_out: *std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    source_dir: ?[]const u8,
    validation_mode: SnapshotValidationMode,
) Allocator.Error!Check {
    // Canonicalize
    var czer = try Can.initModule(roc_ctx, env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module_env,
            .builtin_indices = builtin_indices,
        },
        .is_entry_module = true,
    });
    try czer.canonicalizeFile();
    switch (validation_mode) {
        .none => {},
        .checking => try czer.validateForChecking(),
    }
    czer.deinit();

    // File imports depend on no other module, so they are read as soon as
    // canonicalization has recorded them.
    try can.resolveDeferredFileImports(env, .{ .read = .{ .ctx = roc_ctx, .source_dir = source_dir } });

    env.imports.clearResolvedModules();
    try env.imports.resolveImportsByExactModuleName(env, imported_envs);
    env.imports.markUnresolvedImportsFailedBeforeChecking();
    try can.resolveDeferredImports(env, .{
        .imports = .{ .resolved_store = imported_envs },
    });

    // Type check using the SAME module_envs_map
    const module_builtin_ctx: Check.BuiltinContext = .{
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module_env,
        .builtin_indices = builtin_indices,
    };

    var checker = try Check.init(
        gpa,
        &env.types,
        env,
        imported_envs,
        module_envs_out,
        &env.store.regions,
        module_builtin_ctx,
    );
    checker.fixupTypeWriter();
    errdefer checker.deinit();

    try checker.checkFile();

    _ = try checker.problems.flushAllPendingStaticExhaustiveness(gpa);

    return checker;
}

const ImportedTypeModule = struct {
    source_ident: base.Ident.Idx,
    statement_idx: can.CIR.Statement.Idx,
};

/// Return the exact type identity and declaration owned by an imported type
/// module. The module name is dependency identity and may include a normalized
/// source path, so it is not a source type name to parse or look up.
fn importedTypeModule(sibling_env: *const ModuleEnv) ?ImportedTypeModule {
    // Only type modules store associated functions under qualified names.
    // Regular modules (deprecated_module, etc.) store them under plain names.
    const type_ident_in_module = switch (sibling_env.module_kind) {
        .type_module => |type_ident| type_ident,
        .default_app,
        .app,
        .package,
        .platform,
        .hosted,
        .module,
        .malformed,
        => return null,
    };
    const type_node_idx = sibling_env.getExposedTypeNodeIndexById(type_ident_in_module) orelse return null;
    return .{
        .source_ident = type_ident_in_module,
        .statement_idx = @enumFromInt(type_node_idx),
    };
}

/// Return the main declaration of a type module, if this is one.
pub fn resolveMainType(sibling_env: *const ModuleEnv) ?can.CIR.Statement.Idx {
    return if (importedTypeModule(sibling_env)) |selected| selected.statement_idx else null;
}

/// Resolve a nested public selection to its exact declaration in this CIR.
pub fn resolveSelectedType(
    sibling_env: *const ModuleEnv,
    qualified_name: []const u8,
) ?can.CIR.Statement.Idx {
    const source_ident = sibling_env.common.findIdent(qualified_name) orelse return null;
    const type_node_idx = sibling_env.getExposedTypeNodeIndexById(source_ident) orelse return null;
    return @enumFromInt(type_node_idx);
}

/// Canonicalize one module of a package.
///
/// Canonicalization is a pure function of the module's own source, so this
/// reads no other module's environment: every reference through an import is
/// deferred to `can`'s import-resolution drain, which the type-check task runs
/// once the imports have completed.
pub fn canonicalizeModuleWithSiblings(
    roc_ctx: CoreCtx,
    env: *ModuleEnv,
    parse_ast: *AST,
    builtin_module_env: *const ModuleEnv,
    builtin_indices: can.CIR.BuiltinIndices,
    validation: Can.Validation,
    is_entry_module: bool,
) Allocator.Error!void {
    var czer = try Can.initModule(roc_ctx, env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module_env,
            .builtin_indices = builtin_indices,
        },
        .compiler_version = build_options.compiler_version,
        .validation = validation,
        .is_entry_module = is_entry_module,
    });
    try czer.canonicalizeFile();
    try czer.runValidation();
    czer.deinit();
}

/// Standalone type checking function that can be called from other tools (e.g., snapshot tool)
/// This ensures all tools use the exact same type checking logic as production builds
///
/// `check_alloc` owns checker/session data that dies with `TypeCheckOutput.deinit`.
/// `artifact_alloc` owns any published checked artifact returned in `TypeCheckOutput`.
pub fn typeCheckModule(
    check_alloc: Allocator,
    artifact_alloc: Allocator,
    env: *ModuleEnv,
    builtin_module_env: *const ModuleEnv,
    imported_envs: []const *ModuleEnv,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    available_artifacts: []const CheckedArtifact.ImportedModuleView,
    platform_requirements: ?Check.PlatformRequirementInput,
    platform_requirement_context: ?CheckedArtifact.PlatformRequirementContextKey,
    explicit_roots: []const CheckedArtifact.ExplicitRootRequestInput,
    validation: Can.Validation,
    ctfe_options: eval.CompileTimeFinalization.Options,
    deferred_imports: can.ImportResolution.Imports,
) TypeCheckModuleError!TypeCheckOutput {
    const builtin_indices = compiled_builtins.builtinIndices(can.CIR);

    const module_builtin_ctx: Check.BuiltinContext = .{
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module_env,
        .builtin_indices = builtin_indices,
    };

    // Create module_envs map for explicit imported modules used during canonicalization
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(check_alloc);
    errdefer module_envs_map.deinit();

    const owner_envs = try buildCheckOwnerEnvs(
        check_alloc,
        imported_envs,
        imported_artifacts,
        available_artifacts,
        platform_requirements,
    );
    defer check_alloc.free(owner_envs);

    // Resolve this module's deferred references into its imports before any
    // checking reads them. The imports have completed and the checked-module
    // cache has already been probed and missed.
    try can.resolveDeferredImports(env, .{
        .imports = deferred_imports,
        .reachable_envs = owner_envs,
    });

    var checker = try Check.initWithOwnerModules(
        check_alloc,
        &env.types,
        env,
        imported_envs,
        owner_envs,
        &module_envs_map,
        &env.store.regions,
        module_builtin_ctx,
    );
    checker.platform_requirements = platform_requirements;
    checker.validation = validation;
    checker.fixupTypeWriter();
    errdefer checker.deinit();

    // For app modules with platform requirements, defer finalizing numeric defaults
    // until after platform requirements are checked, so numeric literals can be
    // constrained by platform types (e.g., I64) before defaulting to Dec.
    // TODO: re-enable defer_numeric_defaults once ModuleEnv has the field
    try checker.checkFile();

    module_envs_map.deinit();

    if (!importedArtifactsCoverImportedEnvs(imported_envs, imported_artifacts)) {
        std.debug.panic("compile.typeCheckModule received an imported module environment without its checked artifact", .{});
    }

    var checked_artifact = try publishCheckedArtifactFromCheckedModule(
        artifact_alloc,
        env,
        imported_envs,
        imported_artifacts,
        .{
            .platform_requirement_context = platform_requirement_context,
            .platform_app_relation = null,
            .platform_requirement_solutions = checker.platformRequirementSolutions(),
            .explicit_roots = explicit_roots,
            .hoisted_roots = checker.selectedHoistedRoots(),
            .promoted_local_procedures = checker.promotedLocalProcedures(),
            .available_artifacts = available_artifacts,
            .problem_store = &checker.problems,
            .ctfe_options = ctfe_options,
            .evaluation_phase = .post_frontend,
            .validation = validation,
        },
    );
    errdefer checked_artifact.deinit(artifact_alloc);

    return .{
        .checker = checker,
        .publication = checked_artifact,
    };
}

/// Publish a checked artifact whose module environment is owned by checked source.
pub fn publishCheckedArtifactFromCheckedModule(
    gpa: Allocator,
    env: *ModuleEnv,
    imported_envs: []const *ModuleEnv,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    publication: ArtifactPublicationInputs,
) PublishError!CheckedArtifact.CheckedModuleArtifact {
    return publishCheckedArtifactFromCheckedModuleWithStorage(
        gpa,
        env,
        .{ .checked_source = env },
        imported_envs,
        imported_artifacts,
        publication,
    );
}

/// Publish a checked artifact with an explicit module-environment storage owner.
pub fn publishCheckedArtifactFromCheckedModuleWithStorage(
    gpa: Allocator,
    env: *ModuleEnv,
    module_env_storage: CheckedArtifact.ModuleEnvStorage,
    imported_envs: []const *ModuleEnv,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    publication: ArtifactPublicationInputs,
) PublishError!CheckedArtifact.CheckedModuleArtifact {
    var typed = try CheckedModules.initForRootModule(gpa, env, imported_envs);
    defer typed.modules.deinit();
    return publishFromPrebuiltModules(gpa, &typed.modules, typed.module_idx, module_env_storage, imported_artifacts, publication);
}

/// Publish from an already-built `Modules` graph. The cache-key probe builds the
/// root graph to compute the key; on a miss the same graph is reused here instead of
/// rebuilding it (the build runs `prepareRuntimeEnv` over every env, so rebuilding is
/// real, redundant work).
pub fn publishFromPrebuiltModules(
    gpa: Allocator,
    modules: *const CheckedModules,
    module_idx: u32,
    module_env_storage: CheckedArtifact.ModuleEnvStorage,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    publication: ArtifactPublicationInputs,
) PublishError!CheckedArtifact.CheckedModuleArtifact {
    var ctfe_options = publication.ctfe_options;
    return try CheckedArtifact.publishFromTypedModule(
        gpa,
        modules,
        module_idx,
        .{
            .module_env_storage = module_env_storage,
            .imports = imported_artifacts,
            .available_artifacts = publication.available_artifacts,
            .relation_artifacts = publication.relation_artifacts,
            .platform_requirement_context = publication.platform_requirement_context,
            .platform_app_relation = publication.platform_app_relation,
            .platform_requirement_solutions = publication.platform_requirement_solutions,
            .explicit_roots = publication.explicit_roots,
            .hoisted_roots = publication.hoisted_roots,
            .promoted_local_procedures = publication.promoted_local_procedures,
            .compile_time_finalizer = eval.CompileTimeFinalization.finalizerWithOptions(&ctfe_options),
            .evaluation_phase = publication.evaluation_phase,
            .problem_store = publication.problem_store,
            .validation = publication.validation,
        },
    );
}

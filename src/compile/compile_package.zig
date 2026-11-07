//! Stateless canonicalization, checking, and opening checked modules
//! operations used by the compilation coordinator and snapshot tooling.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const eval = @import("eval");
const compiled_builtins = @import("compiled_builtins");

const Check = check.Check;
const Checked = check.Checked;
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
    /// Wall time of whole-program finalization, which runs after every module
    /// has been checked.
    program_finalization_ns: u64 = 0,
    /// Compile-time evaluation and shared lowering performed by whole-program
    /// finalization.
    compile_time_evaluation: eval.CompileTimeFinalization.TimingSnapshot = .{},
    /// Deterministic lowering and emission counters from all compile-time
    /// evaluation.
    compile_time_counters: eval.CompileTimeFinalization.TimingSnapshot = .{},
    /// The modules whose type checking took longest.
    slowest_type_checks: SlowestTypeChecks = .{},
};

/// One module's type-checking time: checking and opening the checked
/// module.
/// The name is stored inline because the timing outlives the coordinator.
pub const ModuleTypeCheckTiming = struct {
    pub const max_name_len = 256;

    name_buf: [max_name_len]u8 = undefined,
    name_len: u16 = 0,
    /// Bytes of `name_buf` holding the package label.
    package_len: u16 = 0,
    ns: u64,

    /// `package.Module`, cut to `max_name_len` bytes.
    pub fn init(package: []const u8, module: []const u8, ns: u64) ModuleTypeCheckTiming {
        var result: ModuleTypeCheckTiming = .{ .ns = ns };
        const written = std.fmt.bufPrint(&result.name_buf, "{s}.{s}", .{ package, module }) catch |err| switch (err) {
            error.NoSpaceLeft => result.name_buf[0..],
        };
        result.name_len = @intCast(written.len);
        result.package_len = @intCast(@min(package.len, written.len));
        return result;
    }

    pub fn name(self: *const ModuleTypeCheckTiming) []const u8 {
        return self.name_buf[0..self.name_len];
    }

    /// The package label: a shorthand, or the identity of a package that has
    /// none, which for a local package is its path.
    pub fn packageLabel(self: *const ModuleTypeCheckTiming) []const u8 {
        return self.name_buf[0..self.package_len];
    }

    /// The module name, after the package label and its `.` separator.
    pub fn moduleName(self: *const ModuleTypeCheckTiming) []const u8 {
        return self.name_buf[@min(self.package_len + 1, self.name_len)..self.name_len];
    }
};

/// The `capacity` slowest module type checks, slowest first. Ties keep the
/// module that was offered first.
pub const SlowestTypeChecks = struct {
    pub const capacity = 10;

    items: [capacity]ModuleTypeCheckTiming = undefined,
    len: usize = 0,

    pub fn offer(self: *SlowestTypeChecks, package: []const u8, module: []const u8, ns: u64) void {
        var index = self.len;
        while (index > 0 and self.items[index - 1].ns < ns) index -= 1;
        if (index == capacity) return;
        const last = @min(self.len, capacity - 1);
        var shift = last;
        while (shift > index) : (shift -= 1) self.items[shift] = self.items[shift - 1];
        self.items[index] = .init(package, module, ns);
        self.len = last + 1;
    }

    pub fn slice(self: *const SlowestTypeChecks) []const ModuleTypeCheckTiming {
        return self.items[0..self.len];
    }
};

test "SlowestTypeChecks keeps the slowest modules in descending order" {
    var slowest: SlowestTypeChecks = .{};
    var ns: u64 = 0;
    while (ns < 2 * SlowestTypeChecks.capacity) : (ns += 1) {
        // Interleave fast and slow modules so insertion order is not sorted.
        const value = if (ns % 2 == 0) ns else 100 + ns;
        slowest.offer("pkg", "Mod", value);
    }
    const items = slowest.slice();
    try std.testing.expectEqual(@as(usize, SlowestTypeChecks.capacity), items.len);
    try std.testing.expectEqual(@as(u64, 119), items[0].ns);
    for (items[1..], items[0 .. items.len - 1]) |current, previous| {
        try std.testing.expect(current.ns <= previous.ns);
    }
    try std.testing.expectEqual(@as(u64, 101), items[items.len - 1].ns);
    try std.testing.expectEqualStrings("pkg.Mod", items[0].name());
}
const Allocator = std.mem.Allocator;

const CoreCtx = @import("ctx").CoreCtx;

/// Errors that can occur while evaluating compile-time roots.
pub const CompileTimeError = Checked.CompileTimeFinalizer.Error;
/// Errors that can occur while type-checking a module.
pub const TypeCheckModuleError = Allocator.Error || error{Internal};

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
    checked_module: ?*const Checked.CheckedModule,
};

/// Output of a module that completed successfully, which always includes its
/// checked module.
pub const CheckedModuleData = struct {
    env: *ModuleEnv,
    checked_module: *const Checked.CheckedModule,
};

/// Owned output from type checking before the coordinator takes the checked module.
pub const TypeCheckOutput = struct {
    checker: Check,
    checked_module: Checked.CheckedModule,
    checked_module_owned: bool = true,
    inference_reused: bool = false,
    owner_bindings: u32 = 0,
    reference_bindings: u32 = 0,
    diagnostics_replayed: u32 = 0,

    pub fn deinit(self: *TypeCheckOutput) void {
        if (self.checked_module_owned) self.checked_module.deinit(self.checked_module.canonical_names.allocator);
        self.checker.deinit();
    }

    pub fn takeCheckedModule(self: *TypeCheckOutput) Checked.CheckedModule {
        std.debug.assert(self.checked_module_owned);
        self.checked_module_owned = false;
        return self.checked_module;
    }
};

/// Owned output from checking a platform's requires surface.
pub const PlatformRequirementsCheckOutput = struct {
    checker: Check,

    pub fn deinit(self: *PlatformRequirementsCheckOutput) void {
        self.checker.deinit();
    }
};

/// What opening a module right after checking it reads beside its
/// environment and imports.
pub const OpenAfterCheckingInputs = struct {
    available_modules: []const Checked.ImportedModuleView = &.{},
    platform_requirement_context: ?Checked.PlatformRequirementContextKey = null,
    explicit_roots: []const Checked.ExplicitRootRequestInput = &.{},
    /// Waits on the module's key slots.
    io: std.Io,
    /// How this module's compile-time roots were established. Part of the
    /// checked module's cache identity. See `Can.Validation`.
    validation: Can.Validation = .checking,
    /// What checking the module output beside its solved source.
    inference: check.InferenceOutput,
};

fn importedModulesCoverImportedEnvs(
    imported_envs: []const *ModuleEnv,
    imported_modules: []const Checked.CheckedImport,
) bool {
    for (imported_envs, 0..) |_, module_idx| {
        var found = false;
        for (imported_modules) |checked| {
            if (checked.module_idx == module_idx) {
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
    imported_modules: []const Checked.CheckedImport,
    available_modules: []const Checked.ImportedModuleView,
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

    var seen_public_dependencies = std.AutoHashMap(Checked.CheckedModuleDigest, void).init(allocator);
    defer seen_public_dependencies.deinit();

    for (imported_modules) |imported_module| {
        try appendCheckOwnerEnvPublicDependencies(
            allocator,
            &owner_envs,
            available_modules,
            &seen_public_dependencies,
            imported_module.view,
        );
    }

    return try owner_envs.toOwnedSlice(allocator);
}

fn appendCheckOwnerEnvPublicDependencies(
    allocator: Allocator,
    owner_envs: *std.ArrayList(*const ModuleEnv),
    available_modules: []const Checked.ImportedModuleView,
    seen_public_dependencies: *std.AutoHashMap(Checked.CheckedModuleDigest, void),
    view: Checked.ImportedModuleView,
) Allocator.Error!void {
    const resolver_views = Checked.CheckedImportViews{ .current_owner = view.key.digest(), .direct = &.{}, .available = available_modules };
    for ((try view.publicApiDependencies(resolver_views.resolver())).type_owner_modules) |dependency_key| {
        const entry = try seen_public_dependencies.getOrPut(dependency_key);
        if (entry.found_existing) continue;
        entry.value_ptr.* = {};

        const dependency = availableModuleByKey(available_modules, dependency_key) orelse {
            base.invariant("compile.typeCheckModule missing public API dependency module for imported module", .{});
        };
        try appendCheckOwnerEnvIfMissing(allocator, owner_envs, dependency.module_env);
        try appendCheckOwnerEnvPublicDependencies(
            allocator,
            owner_envs,
            available_modules,
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

fn availableModuleByKey(
    available_modules: []const Checked.ImportedModuleView,
    key: Checked.ModuleId,
) ?Checked.ImportedModuleView {
    for (available_modules) |checked| {
        if (std.mem.eql(u8, &checked.key.bytes, &key.bytes)) return checked;
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
/// Snapshot-only type inspection opens no checked module for post-check
/// lowering; only real builds open one and evaluate its roots.
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

    _ = try checker.problems.flushAllPendingStaticExhaustiveness();

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
        .statement_idx = @fromBackingInt(@intCast(type_node_idx)),
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
    return @fromBackingInt(@intCast(type_node_idx));
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
    source_pin_version: ?[]const u8,
) Allocator.Error!void {
    var czer = try Can.initModule(roc_ctx, env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module_env,
            .builtin_indices = builtin_indices,
        },
        .compiler_version = source_pin_version,
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
/// `checked_alloc` owns any checked module returned in `TypeCheckOutput`.
pub fn typeCheckModule(
    check_alloc: Allocator,
    checked_alloc: Allocator,
    env: *ModuleEnv,
    builtin_module_env: *const ModuleEnv,
    imported_envs: []const *ModuleEnv,
    imported_modules: []const Checked.CheckedImport,
    available_modules: []const Checked.ImportedModuleView,
    platform_requirements: ?Check.PlatformRequirementInput,
    platform_requirement_context: ?Checked.PlatformRequirementContextKey,
    explicit_roots: []const Checked.ExplicitRootRequestInput,
    validation: Can.Validation,
    source_pin_validation: [32]u8,
    io: std.Io,
    deferred_imports: can.ImportResolution.Imports,
    missing_public_types: []const []const u8,
    inference_cache: ?@import("inference_cache.zig").Cache,
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
        imported_modules,
        available_modules,
        platform_requirements,
    );
    defer check_alloc.free(owner_envs);

    // Resolve this module's deferred references into its imports before any
    // checking reads them. The imports have completed and the checked-module
    // cache has already been probed and missed.
    try can.resolveDeferredImports(env, .{
        .imports = deferred_imports,
        .reachable_envs = owner_envs,
        .missing_public_types = missing_public_types,
    });

    var checking_scope = try check.CheckingInterface.makeScope(check_alloc, env, builtin_module_env, imported_envs, owner_envs);
    defer checking_scope.deinit();
    // The platform's complete checking surface already belongs to the scope.
    // Select its owner parameter here; full platform implementation identity
    // remains an input to the checked module's key, never inference admission.
    const requirement_owner = if (platform_requirements) |requirements|
        checking_scope.ordinal(requirements.env.contentIdentityHash().?.*)
    else
        null;
    const inference_key = Checked.inferenceKeyFromModuleEnv(env, checking_scope.digest(), requirement_owner, validation, source_pin_validation, missing_public_types);
    const loaded = if (inference_cache) |cache| try cache.load(inference_key, env, &checking_scope) else null;
    defer if (loaded) |capsule| capsule.deinit();

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
    if (loaded) |capsule| {
        try capsule.metadata.install(&checker, &checking_scope);
        env.types.seal();
    } else {
        try checker.checkFile();
    }
    // The module's inference output, read from the checker against the
    // current dependency scope whether checking ran or a capsule restored
    // it, borrows the checker until opening the module copies it.
    var inference_arena = base.SingleThreadArena.init(check_alloc);
    defer inference_arena.deinit();
    const inference = try check.InferenceOutput.fromChecker(inference_arena.allocator(), &checker, &checking_scope);
    if (loaded == null) if (inference_cache) |cache| cache.store(inference_key, env, &inference);

    module_envs_map.deinit();

    if (!importedModulesCoverImportedEnvs(imported_envs, imported_modules)) {
        base.invariant("compile.typeCheckModule received an imported module environment without its checked module", .{});
    }

    var checked_module = try openCheckedModuleAfterChecking(
        checked_alloc,
        env,
        imported_envs,
        imported_modules,
        .{
            .platform_requirement_context = platform_requirement_context,
            .explicit_roots = explicit_roots,
            .available_modules = available_modules,
            .io = io,
            .validation = validation,
            .inference = inference,
        },
    );
    errdefer checked_module.deinit(checked_alloc);

    return .{
        .checker = checker,
        .checked_module = checked_module,
        .inference_reused = loaded != null,
        .owner_bindings = if (loaded != null) @intCast(env.module_identities.count()) else 0,
        .reference_bindings = if (loaded) |capsule| std.math.cast(u32, capsule.metadata.foreign_references.len) orelse return error.OutOfMemory else 0,
        .diagnostics_replayed = if (loaded) |capsule| std.math.cast(u32, capsule.metadata.outcome.problems.len + env.diagnosticCount()) orelse return error.OutOfMemory else 0,
    };
}

/// Open the live checked module of `env`, which checking just sealed and
/// whose storage the checked source owns.
pub fn openCheckedModuleAfterChecking(
    gpa: Allocator,
    env: *ModuleEnv,
    imported_envs: []const *ModuleEnv,
    imported_modules: []const Checked.CheckedImport,
    inputs: OpenAfterCheckingInputs,
) Allocator.Error!Checked.CheckedModule {
    // The module set prepares every environment for the module's readers.
    var typed = try CheckedModules.initForRootModule(gpa, env, imported_envs);
    defer typed.modules.deinit();
    return try Checked.openCheckedModule(gpa, .{
        .module_idx = typed.module_idx,
        .module_env_storage = .{ .checked_source = env },
        .io = inputs.io,
        .imports = imported_modules,
        .available_modules = inputs.available_modules,
        .platform_requirement_context = inputs.platform_requirement_context,
        .explicit_roots = inputs.explicit_roots,
        .validation = inputs.validation,
        .outputs = .{ .checked = inputs.inference },
    });
}

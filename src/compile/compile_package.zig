//! Module build scheduler for a single package.
//!
//! This component manages the concurrent compilation of all modules within a single package,
//! orchestrating the build phases for each module:
//!
//! - Parsing modules to discover their import dependencies
//! - Canonicalizing parsed modules into an intermediate representation
//! - Type-checking modules once their dependencies are ready
//! - Coordinating with BuildEnv's global work queue for cross-package dependencies
//! - Reporting diagnostics through a deterministic sink
//!
//! The scheduler tracks each module's progress through build phases and ensures proper
//! ordering based on import dependencies. It supports both single-threaded and
//! multi-threaded execution modes.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const reporting = @import("reporting");
const eval = @import("eval");
const compiled_builtins = @import("compiled_builtins");
const build_options = @import("build_options");

// Compile-time flag for build tracing - enabled via `zig build -Dtrace-build`
const trace_build = if (@hasDecl(build_options, "trace_build")) build_options.trace_build else false;
const BuiltinModules = eval.BuiltinModules;
const module_discovery = @import("module_discovery.zig");
const messages = @import("messages.zig");
const roc_target = @import("roc_target");
const watch_inputs = @import("watch_inputs.zig");

const Check = check.Check;
const CheckedArtifact = check.CheckedArtifact;
const CheckedModules = check.TypedCIR.Modules;
const Can = can.Can;
const Report = reporting.Report;
const ModuleEnv = can.ModuleEnv;
const ReportBuilder = check.ReportBuilder;
const AST = parse.AST;
const OwnedSemanticState = struct {
    module_env: ?*ModuleEnv,
    checked_artifact: ?CheckedArtifact.CheckedModuleArtifact = null,
};

/// Deserialize BuiltinIndices from the binary data generated at build time
/// Timing information for different phases
pub const TimingInfo = struct {
    tokenize_parse_ns: u64,
    canonicalize_ns: u64,
    canonicalize_diagnostics_ns: u64,
    type_checking_ns: u64,
    check_diagnostics_ns: u64,
};
const Allocator = std.mem.Allocator;

const threading = @import("threading.zig");
const CoreCtx = @import("ctx").CoreCtx;

const parallel = base.parallel;
const AtomicUsize = std.atomic.Value(usize);

/// Errors that can occur while publishing compile-time finalization results.
pub const PublishError = CheckedArtifact.CompileTimeFinalizer.Error;
/// Errors that can occur while type-checking a module.
pub const TypeCheckModuleError = Allocator.Error || PublishError || error{Internal};
/// Errors that can occur while processing a package module.
pub const ProcessError = Allocator.Error || error{FileNotFound} || TypeCheckModuleError;

const Mutex = threading.Mutex;
const Condition = threading.Condition;

const stage_timers_supported = !threading.is_freestanding;

const StageTimer = if (stage_timers_supported) std.Io.Timestamp else void;

/// Build CTFE finalization options from the package compiler context.
pub fn compileTimeFinalizationOptions(max_threads: usize, roc_ctx: *CoreCtx) eval.CompileTimeFinalization.Options {
    return .{
        .max_threads = max_threads,
        .std_io = roc_ctx.std_io,
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

fn startStageTimer(io: std.Io) ?StageTimer {
    if (comptime !stage_timers_supported) return null;
    return std.Io.Timestamp.now(io, .awake);
}

fn readStageTimer(io: std.Io, timer: *?StageTimer) u64 {
    if (comptime !stage_timers_supported) return 0;
    if (timer.*) |active| {
        const elapsed = active.untilNow(io, .awake).nanoseconds;
        return if (elapsed > 0) @intCast(elapsed) else 0;
    }
    return 0;
}

// FileProvider was removed in favour of the unified Io abstraction (src/io/Io.zig).
// Callers that previously used FileProvider now use Io.readFile / Io.fileExists directly.

/// Build execution mode
pub const Mode = enum { single_threaded, multi_threaded };

/// Destination for reports (can be stdout, memory buffer, etc.)
pub const ReportSink = struct {
    ctx: ?*anyopaque,
    emitFn: *const fn (ctx: ?*anyopaque, module_name: []const u8, report: Report) Allocator.Error!void,
};

/// Optional scheduling hook for observability/integration (e.g. global work-stealing)
pub const ScheduleHook = struct {
    ctx: ?*anyopaque,
    onSchedule: *const fn (ctx: ?*anyopaque, package_name: []const u8, module_name: []const u8, path: []const u8, depth: u32) void,

    /// A no-op hook for testing or when no external scheduling is needed
    pub fn noOp() ScheduleHook {
        return .{ .ctx = null, .onSchedule = noOpSchedule };
    }

    fn noOpSchedule(_: ?*anyopaque, _: []const u8, _: []const u8, _: []const u8, _: u32) void {}

    pub fn isNoOp(self: ScheduleHook) bool {
        return self.onSchedule == noOpSchedule;
    }
};

/// Resolver for handling imports across package boundaries
pub const ImportResolver = struct {
    ctx: ?*anyopaque,
    /// Ensure the external import is scheduled for building in its owning package
    scheduleExternal: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) Allocator.Error!void,
    /// Return true if the external import is fully type-checked and its ModuleEnv is ready
    isReady: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) bool,
    /// Get a pointer to the external ModuleEnv once ready (null if not ready)
    getEnv: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) ?*ModuleEnv,
    /// Get the published checked artifact for the external import once ready (null if not ready)
    getArtifact: *const fn (ctx: ?*anyopaque, current_package: []const u8, import_name: []const u8) ?*const CheckedArtifact.CheckedModuleArtifact,
    /// Resolve a local module import to a filesystem path within the current package
    resolveLocalPath: *const fn (ctx: ?*anyopaque, current_package: []const u8, root_dir: []const u8, import_name: []const u8) Allocator.Error![]const u8,
};

/// Module identifier - index into the modules list
const ModuleId = u32;

const Task = struct { module_id: ModuleId };

const Phase = enum { Parse, Canonicalize, WaitingOnImports, TypeCheck, Done };

const ModuleState = struct {
    name: []const u8, // Module name is needed for error reporting and the schedule hook
    path: []const u8,
    /// Optional source directory used to resolve imports from this module.
    source_dir_override: ?[]const u8 = null,
    /// Raw source file state observed before line-ending normalization.
    source_file_state: ?watch_inputs.State = null,
    semantic: ?OwnedSemanticState = null,
    phase: Phase = .Parse,
    imports: std.ArrayList(ModuleId),
    /// External imports qualified via package shorthand (e.g. "cli.Stdout") - still strings as they reference other packages
    external_imports: std.ArrayList([]const u8),
    dependents: std.ArrayList(ModuleId),
    reports: std.ArrayList(Report),
    depth: u32 = std.math.maxInt(u32), // min depth from root
    /// DFS visitation color for cycle detection: 0=white (unvisited), 1=gray (visiting), 2=black (finished)
    visit_color: u8 = 0,
    /// Atomic flag to prevent concurrent processing of the same module (0=free, 1=working)
    working: if (!threading.is_freestanding) std.atomic.Value(u8) else u8 = if (!threading.is_freestanding) std.atomic.Value(u8).init(0) else 0,
    /// Cached AST from parsing phase - heap-allocated to avoid copy issues with ArrayLists
    cached_ast: ?*parse.AST = null,

    pub fn moduleEnv(self: *ModuleState) ?*ModuleEnv {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |*artifact| return artifact.moduleEnv();
            return semantic.module_env;
        }
        return null;
    }

    pub fn checkedArtifact(self: *ModuleState) ?*CheckedArtifact.CheckedModuleArtifact {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |*artifact| return artifact;
        }
        return null;
    }

    pub fn semanticData(self: *ModuleState) ?SemanticModuleData {
        const env = self.moduleEnv() orelse return null;
        return .{
            .env = env,
            .checked_artifact = self.checkedArtifact(),
        };
    }

    pub fn canonicalSourceDir(self: *const ModuleState, fallback_root_dir: []const u8) []const u8 {
        return self.source_dir_override orelse (std.fs.path.dirname(self.path) orelse fallback_root_dir);
    }

    fn replaceModuleEnv(self: *ModuleState, env: *ModuleEnv) void {
        if (self.semantic) |*semantic| {
            semantic.module_env = env;
        } else {
            self.semantic = .{
                .module_env = env,
                .checked_artifact = null,
            };
        }
    }

    fn replaceCheckedArtifact(self: *ModuleState, artifact: CheckedArtifact.CheckedModuleArtifact) void {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |*existing| existing.deinit(existing.canonical_names.allocator);
            semantic.module_env = null;
            semantic.checked_artifact = artifact;
            return;
        }
        std.debug.panic("compile_package.ModuleState.replaceCheckedArtifact missing module env for {s}", .{self.name});
    }

    fn deinit(self: *ModuleState, gpa: Allocator) void {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |*artifact| artifact.deinit(artifact.canonical_names.allocator);
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: checking cached_ast\n", .{self.name});
        }
        // Free cached AST if present
        if (self.cached_ast) |ast| {
            ast.deinit();
        }
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact == null) {
                if (semantic.module_env) |e| {
                    // IMPORTANT: Use e.gpa, not the passed-in gpa, because source was allocated
                    // with e.gpa (smp_allocator in multi-threaded mode).
                    const env_alloc = e.gpa;
                    const source = e.common.source;
                    if (comptime trace_build) {
                        std.debug.print("[MOD DEINIT DETAIL] {s}: source={}, calling env.deinit\n", .{ self.name, @intFromPtr(source.ptr) });
                    }
                    e.deinit();
                    if (comptime trace_build) {
                        std.debug.print("[MOD DEINIT DETAIL] {s}: freeing source\n", .{self.name});
                    }
                    if (source.len > 0) env_alloc.free(@constCast(source));
                    env_alloc.destroy(e);
                }
            }
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing imports (len={})\n", .{ self.name, self.imports.items.len });
        }
        self.imports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing external_imports (len={})\n", .{ self.name, self.external_imports.items.len });
        }
        for (self.external_imports.items) |import_name| {
            gpa.free(import_name);
        }
        self.external_imports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing dependents (len={})\n", .{ self.name, self.dependents.items.len });
        }
        self.dependents.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing reports (len={}, cap={})\n", .{ self.name, self.reports.items.len, self.reports.capacity });
        }
        // If reports were emitted, they've been cleared via clearRetainingCapacity() and ownership
        // transferred to OrderedSink. If not emitted (e.g., module failed before .Done), we must
        // deinit them here to avoid leaks.
        for (self.reports.items, 0..) |*report, i| {
            if (comptime trace_build) {
                std.debug.print("[MOD DEINIT DETAIL] {s}: deinit report {} title=\"{s}\" owned_strings.len={} doc_elements={} allocator_ptr={}\n", .{
                    self.name,
                    i,
                    report.title,
                    report.owned_strings.items.len,
                    report.document.elements.items.len,
                    @intFromPtr(report.allocator.ptr),
                });
                if (report.owned_strings.items.len > 0) {
                    std.debug.print("[MOD DEINIT DETAIL] {s}: first owned_string ptr={} len={}\n", .{
                        self.name,
                        @intFromPtr(report.owned_strings.items[0].ptr),
                        report.owned_strings.items[0].len,
                    });
                }
            }
            report.deinit();
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: calling reports.deinit\n", .{self.name});
        }
        self.reports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: freeing path\n", .{self.name});
        }
        gpa.free(self.path);
        if (self.source_dir_override) |source_dir| {
            gpa.free(source_dir);
        }
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT DETAIL] {s}: done\n", .{self.name});
        }
    }

    fn init(name: []const u8, path: []const u8) ModuleState {
        return .{
            .name = name,
            .path = path,
            .imports = std.ArrayList(ModuleId).empty,
            .external_imports = std.ArrayList([]const u8).empty,
            .dependents = std.ArrayList(ModuleId).empty,
            .reports = std.ArrayList(Report).empty,
        };
    }
};

/// Semantic facts retained for a checked module.
pub const SemanticModuleData = struct {
    env: *ModuleEnv,
    checked_artifact: ?*const CheckedArtifact.CheckedModuleArtifact,
};

/// Owned output from type checking before module state takes retained facts.
pub const TypeCheckOutput = struct {
    checker: Check,
    checked_artifact: ?CheckedArtifact.CheckedModuleArtifact = null,
    user_errors_allow_lowering: bool = false,

    pub fn deinit(self: *TypeCheckOutput) void {
        if (self.checked_artifact) |*artifact| artifact.deinit(artifact.canonical_names.allocator);
        self.checker.deinit();
    }

    pub fn takeCheckedArtifact(self: *TypeCheckOutput) CheckedArtifact.CheckedModuleArtifact {
        const artifact = self.checked_artifact orelse
            std.debug.panic("compile.typeCheckOutput missing checked artifact", .{});
        self.checked_artifact = null;
        return artifact;
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
    explicit_roots: []const CheckedArtifact.ExplicitRootRequestInput = &.{},
    hoisted_roots: []const check.HoistRoots.SelectedHoistedRoot = &.{},
    problem_store: ?*check.problem.Store = null,
    ctfe_options: eval.CompileTimeFinalization.Options = .{},
};

fn problemBlocksCheckedArtifact(problem: check.problem.Problem) bool {
    return switch (problem) {
        .static_dispatch => |static_dispatch| switch (static_dispatch) {
            .unresolved_dispatcher => |unresolved| !unresolved.runtime_error_inserted,
            .dispatcher_not_nominal,
            .dispatcher_does_not_impl_method,
            .type_does_not_support_equality,
            .recursive_dispatch,
            => true,
        },
        .redundant_pattern, .unmatchable_pattern, .comptime_unused_branch, .comptime_condition, .literal_defaulted => false,
        else => true,
    };
}

fn problemAllowsLoweringWithUserErrors(problem: check.problem.Problem) bool {
    return switch (problem) {
        .static_dispatch => |static_dispatch| switch (static_dispatch) {
            .unresolved_dispatcher => |unresolved| unresolved.runtime_error_inserted,
            .dispatcher_not_nominal,
            .dispatcher_does_not_impl_method,
            .type_does_not_support_equality,
            .recursive_dispatch,
            => false,
        },
        else => false,
    };
}

fn checkerHasArtifactBlockingProblems(checker: *const Check) bool {
    for (checker.problems.problems.items) |problem| {
        if (problemBlocksCheckedArtifact(problem)) return true;
    }
    return false;
}

fn checkerProblemsAllowLoweringWithUserErrors(checker: *const Check) bool {
    for (checker.problems.problems.items) |problem| {
        if (!problemAllowsLoweringWithUserErrors(problem)) return false;
    }
    return true;
}

fn moduleHasArtifactBlockingCanonicalizeDiagnostics(env: *const ModuleEnv) bool {
    const diagnostics = env.store.sliceDiagnostics(env.diagnostics);
    for (diagnostics) |diagnostic_idx| {
        const diagnostic = env.store.getDiagnostic(diagnostic_idx);
        switch (diagnostic) {
            .shadowing_warning,
            .unused_variable,
            .used_underscore_variable,
            .type_shadowed_warning,
            .unused_type_var_name,
            .type_var_marked_unused,
            .underscore_in_type_declaration,
            .module_header_deprecated,
            .deprecated_number_suffix,
            .unreachable_string_pattern_capture,
            => {},
            else => return true,
        }
    }
    return false;
}

fn moduleHasDuplicateTopLevelValueDefs(gpa: Allocator, env: *const ModuleEnv) Allocator.Error!bool {
    var seen = std.AutoHashMapUnmanaged(base.Ident.Idx, void){};
    defer seen.deinit(gpa);

    for (env.store.sliceDefs(env.global_value_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        const ident = switch (pattern) {
            .assign => |assign| assign.ident,
            .as => |as_pattern| as_pattern.ident,
            else => continue,
        };

        const entry = try seen.getOrPut(gpa, ident);
        if (entry.found_existing) return true;
        entry.value_ptr.* = {};
    }

    return false;
}

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

fn buildCheckOwnerEnvs(
    allocator: Allocator,
    imported_envs: []const *ModuleEnv,
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    available_artifacts: []const CheckedArtifact.ImportedModuleView,
) Allocator.Error![]const *const ModuleEnv {
    var owner_envs = std.ArrayList(*const ModuleEnv).empty;
    errdefer owner_envs.deinit(allocator);

    for (imported_envs) |env| {
        try appendCheckOwnerEnvIfMissing(allocator, &owner_envs, env);
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
        if (moduleEnvNamesMatch(existing, module_env)) return;
    }
    try owner_envs.append(allocator, module_env);
}

fn moduleEnvNamesMatch(a: *const ModuleEnv, b: *const ModuleEnv) bool {
    if (@intFromPtr(a) == @intFromPtr(b)) return true;
    if (!a.qualified_module_ident.isNone() and !b.qualified_module_ident.isNone()) {
        return std.mem.eql(u8, a.getIdent(a.qualified_module_ident), b.getIdent(b.qualified_module_ident));
    }
    if (!a.display_module_name_idx.isNone() and !b.display_module_name_idx.isNone()) {
        return std.mem.eql(u8, a.getIdent(a.display_module_name_idx), b.getIdent(b.display_module_name_idx));
    }
    if (a.qualified_module_ident.isNone() or b.qualified_module_ident.isNone()) {
        if (std.mem.eql(u8, a.module_name, b.module_name)) return true;
    }
    return false;
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

fn checkedModuleArtifactKeyEql(
    a: CheckedArtifact.CheckedModuleArtifactKey,
    b: CheckedArtifact.CheckedModuleArtifactKey,
) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

/// Per-package module build orchestrator
pub const PackageEnv = struct {
    gpa: Allocator,
    /// Name of the current package (used for shorthand resolution)
    package_name: []const u8,
    root_dir: []const u8,
    mode: Mode,
    max_threads: usize,
    target: roc_target.RocTarget,
    sink: ReportSink,
    /// Optional resolver for cross-package imports; when null, all imports are treated as local
    resolver: ?ImportResolver = null,
    /// Scheduling hook to observe/enqueue scheduled modules in a global orchestrator
    schedule_hook: ScheduleHook,
    /// Compiler version for cache invalidation
    compiler_version: []const u8,
    /// Builtin modules (Bool, Try, Str) for auto-importing in canonicalization (not owned)
    builtin_modules: *const BuiltinModules,
    /// I/O abstraction for reading sources and other filesystem/stdio operations.
    roc_ctx: CoreCtx,
    /// Whether to retain exact source byte states for watch-mode refreshes.
    track_watch_inputs: bool = false,

    lock: Mutex = Mutex.init,
    cond: Condition = Condition.init,

    // Work queue
    injector: std.ArrayList(Task),

    // Module storage
    modules: std.ArrayList(ModuleState),
    // String intern table: module name -> module ID
    module_names: std.StringHashMapUnmanaged(ModuleId) = .{},

    // Build status
    remaining_modules: usize = 0,
    /// ID of the root module (the module passed to buildRoot)
    root_module_id: ?ModuleId = null,
    /// First error reported by worker threads during multi-threaded processing
    worker_error: ?ProcessError = null,

    // Track module discovery order and which modules have had their reports emitted
    discovered: std.ArrayList(ModuleId),
    emitted: std.bit_set.DynamicBitSetUnmanaged = .{},

    // Timing collection (accumulated across all modules)
    total_tokenize_parse_ns: u64 = 0,
    total_canonicalize_ns: u64 = 0,
    total_canonicalize_diagnostics_ns: u64 = 0,
    total_type_checking_ns: u64 = 0,
    total_check_diagnostics_ns: u64 = 0,

    // Additional known modules (e.g., from platform exposes) to include in module_envs_map
    // These are modules that exist in external directories (like URL platform cache)
    additional_known_modules: std.ArrayList(KnownModule),

    /// Info about a known module from a platform or other package
    pub const KnownModule = struct {
        /// Qualified module name (e.g., "pf.Stdout")
        qualified_name: []const u8,
        /// Import name for resolver lookup (e.g., "pf.Stdout")
        import_name: []const u8,
    };

    pub fn init(gpa: Allocator, package_name: []const u8, root_dir: []const u8, mode: Mode, max_threads: usize, target: roc_target.RocTarget, sink: ReportSink, schedule_hook: ScheduleHook, compiler_version: []const u8, builtin_modules: *const BuiltinModules, roc_ctx: CoreCtx) PackageEnv {
        // Pre-allocate module storage to avoid reallocation during multi-threaded processing
        var modules = std.ArrayList(ModuleState).empty;
        if (mode == .multi_threaded) {
            // This is only a performance hint; failing to reserve this much up front
            // is not fatal because later appends still report allocation failures.
            modules.ensureTotalCapacity(gpa, 256) catch {};
        }
        return .{
            .gpa = gpa,
            .package_name = package_name,
            .root_dir = root_dir,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .sink = sink,
            .schedule_hook = schedule_hook,
            .compiler_version = compiler_version,
            .builtin_modules = builtin_modules,
            .roc_ctx = roc_ctx,
            .injector = std.ArrayList(Task).empty,
            .modules = modules,
            .discovered = std.ArrayList(ModuleId).empty,
            .additional_known_modules = std.ArrayList(KnownModule).empty,
        };
    }

    pub fn setWatchInputTracking(self: *PackageEnv, enabled: bool) void {
        self.track_watch_inputs = enabled;
    }

    pub fn initWithResolver(
        gpa: Allocator,
        package_name: []const u8,
        root_dir: []const u8,
        mode: Mode,
        max_threads: usize,
        target: roc_target.RocTarget,
        sink: ReportSink,
        resolver: ImportResolver,
        schedule_hook: ScheduleHook,
        compiler_version: []const u8,
        builtin_modules: *const BuiltinModules,
        roc_ctx: CoreCtx,
    ) PackageEnv {
        // Pre-allocate module storage to avoid reallocation during multi-threaded processing
        var modules = std.ArrayList(ModuleState).empty;
        if (mode == .multi_threaded) {
            // This is only a performance hint; failing to reserve this much up front
            // is not fatal because later appends still report allocation failures.
            modules.ensureTotalCapacity(gpa, 256) catch {};
        }
        return .{
            .gpa = gpa,
            .package_name = package_name,
            .root_dir = root_dir,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .sink = sink,
            .resolver = resolver,
            .schedule_hook = schedule_hook,
            .compiler_version = compiler_version,
            .builtin_modules = builtin_modules,
            .roc_ctx = roc_ctx,
            .injector = std.ArrayList(Task).empty,
            .modules = modules,
            .discovered = std.ArrayList(ModuleId).empty,
            .additional_known_modules = std.ArrayList(KnownModule).empty,
        };
    }

    /// Add a module that should be recognized during canonicalization.
    /// This is used for platform-exposed modules in URL platforms where the
    /// modules exist in a cache directory, not the app's directory.
    /// `qualified_name` is the full name like "pf.Stdout"
    /// `import_name` is the import path for resolver lookup (e.g., "pf.Stdout")
    pub fn addKnownModule(self: *PackageEnv, qualified_name: []const u8, import_name: []const u8) Allocator.Error!void {
        const qualified_copy = try self.gpa.dupe(u8, qualified_name);
        const import_copy = try self.gpa.dupe(u8, import_name);
        try self.additional_known_modules.append(self.gpa, .{
            .qualified_name = qualified_copy,
            .import_name = import_copy,
        });
    }

    pub fn deinit(self: *PackageEnv) void {
        // NOTE: builtin_modules is not owned by PackageEnv, so we don't deinit it here

        if (comptime trace_build) {
            std.debug.print("[SCHED DEINIT] {s}: starting with {} modules\n", .{ self.package_name, self.modules.items.len });
        }

        // Deinit modules
        for (self.modules.items, 0..) |*ms, idx| {
            if (comptime trace_build) {
                std.debug.print("[SCHED DEINIT] {s}: module {} ({s}) env={} ast={}\n", .{
                    self.package_name,
                    idx,
                    ms.name,
                    @intFromPtr(ms.moduleEnv()),
                    @intFromPtr(ms.cached_ast),
                });
            }
            ms.deinit(self.gpa);
            if (comptime trace_build) {
                std.debug.print("[SCHED DEINIT] {s}: module {} done\n", .{ self.package_name, idx });
            }
        }
        self.modules.deinit(self.gpa);

        // Free interned strings
        var it = self.module_names.iterator();
        while (it.next()) |e| {
            self.gpa.free(e.key_ptr.*);
        }
        self.module_names.deinit(self.gpa);

        self.injector.deinit(self.gpa);
        self.discovered.deinit(self.gpa);
        self.emitted.deinit(self.gpa);

        // Free additional known module names
        for (self.additional_known_modules.items) |km| {
            self.gpa.free(km.qualified_name);
            self.gpa.free(km.import_name);
        }
        self.additional_known_modules.deinit(self.gpa);
    }

    /// Get the root module's semantic data (the module passed to buildRoot)
    pub fn getRootSemanticData(self: *PackageEnv) ?SemanticModuleData {
        const root_id = self.root_module_id orelse return null;
        if (root_id >= self.modules.items.len) return null;
        const module = &self.modules.items[root_id];
        return module.semanticData();
    }

    /// Get the root module state (the module passed to buildRoot)
    pub fn getRootModule(self: *PackageEnv) ?*ModuleState {
        const root_id = self.root_module_id orelse return null;
        if (root_id >= self.modules.items.len) return null;
        return &self.modules.items[root_id];
    }

    pub fn getSemanticDataIfDone(self: *PackageEnv, name: []const u8) ?SemanticModuleData {
        const id = self.module_names.get(name) orelse return null;
        const module = &self.modules.items[id];
        if (module.phase != .Done) return null;
        return module.semanticData();
    }

    fn internModuleName(self: *PackageEnv, name: []const u8) Allocator.Error!ModuleId {
        const gop = try self.module_names.getOrPut(self.gpa, name);
        if (!gop.found_existing) {
            const id: ModuleId = @intCast(self.modules.items.len);
            const owned_name = try self.gpa.dupe(u8, name);
            gop.key_ptr.* = owned_name;
            gop.value_ptr.* = id;
            return id;
        }
        return gop.value_ptr.*;
    }

    pub fn buildRoot(self: *PackageEnv, root_file_path: []const u8) Allocator.Error!void {
        const name = moduleNameFromPath(root_file_path);
        const prev_module_count = self.modules.items.len;
        const module_id = try self.ensureModule(name, root_file_path);
        // Track which module is the root (for getRootEnv)
        self.root_module_id = module_id;
        // root depth = 0
        try self.setDepthIfSmaller(module_id, 0);
        // Only increment remaining_modules if the root module is new (wasn't already scheduled)
        // This can happen when external imports schedule modules via scheduleModule before buildRoot is called
        if (module_id >= prev_module_count) {
            self.remaining_modules += 1;
        }
        try self.enqueue(module_id);

        // Note: enqueue() already calls schedule_hook in multi-threaded mode,
        // so we don't call it again here to avoid duplicate enqueues

        // If a global schedule_hook is installed AND we're in multi-threaded mode,
        // the unified global queue will drive processing via process().
        // In single-threaded mode, we always need to run our local processing loop.
        const should_run_local = self.schedule_hook.isNoOp() or self.mode == .single_threaded;
        if (should_run_local) {
            if (threading.is_freestanding) {
                // On wasm32, always run single-threaded at comptime
                try self.runSingleThread();
            } else {
                switch (self.mode) {
                    .single_threaded => try self.runSingleThread(),
                    .multi_threaded => try self.runMultiThread(),
                }
            }
        }

        // Emit any remaining reports deterministically
        try self.tryEmitReady();
    }

    fn runSingleThread(self: *PackageEnv) Allocator.Error!void {
        while (true) {
            if (self.injector.items.len > 0) {
                const idx = self.injector.items.len - 1;
                const task = self.injector.items[idx];
                self.injector.items.len = idx;
                try self.process(task);
                try self.tryEmitReady();
            } else if (self.remaining_modules == 0) {
                break;
            } else {
                break; // This should not happen - stuck state
            }
        }
    }

    fn runMultiThread(self: *PackageEnv) Allocator.Error!void {
        const options = parallel.ProcessOptions{
            .max_threads = if (self.max_threads == 0)
                threading.getCpuCount()
            else
                self.max_threads,
            .use_per_thread_arenas = false,
        };

        var index = AtomicUsize.init(0);
        while (true) {
            const work_len = self.injector.items.len;
            if (work_len == 0) {
                if (self.remaining_modules == 0) break;
                self.lock.lockUncancelable(self.roc_ctx.std_io);
                defer self.lock.unlock(self.roc_ctx.std_io);
                if (self.remaining_modules == 0 and self.injector.items.len == 0) break;
                self.cond.waitUncancelable(self.roc_ctx.std_io, &self.lock);
                continue;
            }

            index.store(0, .monotonic);
            var ctx = WorkerCtx{ .sched = self, .index = &index, .work_len = work_len };
            try parallel.process(WorkerCtx, &ctx, workerFn, self.gpa, work_len, options);
            if (self.worker_error) |err| return err;
            try self.tryEmitReady();
        }
    }

    const WorkerCtx = struct { sched: *PackageEnv, index: *AtomicUsize, work_len: usize };

    fn workerFn(_: Allocator, ctx: *WorkerCtx, _: usize) void {
        while (true) {
            const i = ctx.index.fetchAdd(1, .monotonic);
            if (i >= ctx.work_len) break;
            const task = ctx.sched.injector.items[i];
            ctx.sched.process(task) catch |err| {
                ctx.sched.lock.lock();
                if (ctx.sched.worker_error == null) {
                    ctx.sched.worker_error = err;
                }
                ctx.sched.lock.unlock();
                return;
            };
        }
        // Compact processed prefix once under lock
        ctx.sched.lock.lock();
        defer ctx.sched.lock.unlock();
        const len = ctx.sched.injector.items.len;
        if (ctx.work_len <= len) {
            std.mem.copyBackwards(Task, ctx.sched.injector.items[0 .. len - ctx.work_len], ctx.sched.injector.items[ctx.work_len..len]);
            ctx.sched.injector.items.len = len - ctx.work_len;
        } else ctx.sched.injector.items.len = 0;
    }

    pub fn ensureModule(self: *PackageEnv, name: []const u8, path: []const u8) Allocator.Error!ModuleId {
        // In multi-threaded mode, lock to prevent race conditions when growing arrays
        const needs_lock = self.mode == .multi_threaded and !threading.is_freestanding;
        if (needs_lock) self.lock.lockUncancelable(self.roc_ctx.std_io);
        defer if (needs_lock) self.lock.unlock(self.roc_ctx.std_io);

        const module_id = try self.internModuleName(name);

        // Check if we need to create a new module
        if (module_id >= self.modules.items.len) {
            // This is a new module
            const owned_path = try self.gpa.dupe(u8, path);
            const owned_name = self.module_names.getKey(name).?; // We just interned it
            try self.modules.append(self.gpa, ModuleState.init(owned_name, owned_path));
            try self.discovered.append(self.gpa, module_id);

            // Note: Don't call schedule_hook here - let scheduleModule/enqueue handle it
            // to avoid duplicate enqueues in multi-threaded mode
        }

        return module_id;
    }

    /// Public API for cross-package schedulers: ensure a module exists, set its depth, and enqueue it
    pub fn scheduleModule(self: *PackageEnv, name: []const u8, path: []const u8, depth: u32) Allocator.Error!void {
        const prev_module_count = self.modules.items.len;
        const module_id = try self.ensureModule(name, path);
        const is_new = module_id >= prev_module_count;
        try self.setDepthIfSmaller(module_id, depth);
        if (is_new) {
            self.remaining_modules += 1;
            // Note: schedule_hook is called by enqueue() in multi-threaded mode,
            // so we don't call it here to avoid duplicate enqueues
        }
        try self.enqueue(module_id);
    }

    fn setDepthIfSmaller(self: *PackageEnv, module_id: ModuleId, depth: u32) Allocator.Error!void {
        const st = &self.modules.items[module_id];
        if (depth < st.depth) st.depth = depth;
    }

    /// Public API to adjust a module's depth from an external coordinator
    pub fn setModuleDepthIfSmaller(self: *PackageEnv, name: []const u8, depth: u32) Allocator.Error!void {
        if (self.module_names.get(name)) |module_id| {
            try self.setDepthIfSmaller(module_id, depth);
        }
    }

    fn enqueue(self: *PackageEnv, module_id: ModuleId) Allocator.Error!void {
        const st = &self.modules.items[module_id];
        // In multi_threaded mode with a non-noop schedule_hook, forward to the global queue
        if (self.mode == .multi_threaded and !self.schedule_hook.isNoOp()) {
            // Look up the module to get its path and depth for the hook
            self.lock.lockUncancelable(self.roc_ctx.std_io);
            defer self.lock.unlock(self.roc_ctx.std_io);

            self.schedule_hook.onSchedule(self.schedule_hook.ctx, self.package_name, st.name, st.path, st.depth);
        } else {
            // Default behavior: use internal injector
            try self.injector.append(self.gpa, .{ .module_id = module_id });
            if (!threading.is_freestanding) self.cond.signal(self.roc_ctx.std_io);
        }
    }

    /// Get accumulated timing information
    pub fn getTimingInfo(self: *PackageEnv) TimingInfo {
        return TimingInfo{
            .tokenize_parse_ns = self.total_tokenize_parse_ns,
            .canonicalize_ns = self.total_canonicalize_ns,
            .canonicalize_diagnostics_ns = self.total_canonicalize_diagnostics_ns,
            .type_checking_ns = self.total_type_checking_ns,
            .check_diagnostics_ns = self.total_check_diagnostics_ns,
        };
    }

    /// Public API to read a module's current recorded dependency depth
    pub fn getModuleDepth(self: *PackageEnv, name: []const u8) ?u32 {
        if (self.module_names.get(name)) |module_id| {
            return self.modules.items[module_id].depth;
        }
        return null;
    }

    /// Public API to check if a module exists by name
    pub fn hasModule(self: *PackageEnv, module_name: []const u8) bool {
        return self.module_names.contains(module_name);
    }

    /// Iterate over module names.
    pub fn moduleNamesIterator(self: *PackageEnv) std.StringHashMapUnmanaged(ModuleId).Iterator {
        return self.module_names.iterator();
    }

    /// Get module state by name.
    pub fn getModuleState(self: *PackageEnv, module_name: []const u8) ?*ModuleState {
        if (self.module_names.get(module_name)) |module_id| {
            return &self.modules.items[module_id];
        }
        return null;
    }

    /// Public API to find a module by its filesystem path.
    /// Returns the ModuleState if found, null otherwise.
    pub fn findModuleByPath(self: *PackageEnv, path: []const u8) ?*ModuleState {
        for (self.modules.items) |*module| {
            if (std.mem.eql(u8, module.path, path)) {
                return module;
            }
        }
        return null;
    }

    /// Public API for processing a module by name (used by BuildEnv)
    pub fn processModuleByName(self: *PackageEnv, module_name: []const u8) ProcessError!void {
        if (self.module_names.get(module_name)) |module_id| {
            try self.process(.{ .module_id = module_id });
        }
    }

    pub fn process(self: *PackageEnv, task: Task) ProcessError!void {
        // In dispatch-only mode, this method is invoked by the global scheduler.
        // In local mode, it's invoked by the internal run* loops.

        // Acquire lock and atomically check/set working flag
        if (!threading.is_freestanding) self.lock.lockUncancelable(self.roc_ctx.std_io);
        const st = &self.modules.items[task.module_id];

        // Atomic compare-and-swap to claim work on this module
        const already_working = if (!threading.is_freestanding) blk: {
            // For multi-threaded: use atomic CAS to claim the module (0 -> 1)
            const result = st.working.cmpxchgWeak(0, 1, .seq_cst, .seq_cst);
            break :blk result != null; // null means swap succeeded
        } else blk: {
            // For single-threaded/wasm: simple check and set
            const was_working = st.working != 0;
            if (!was_working) st.working = 1;
            break :blk was_working;
        };

        if (already_working) {
            if (!threading.is_freestanding) self.lock.unlock(self.roc_ctx.std_io);
            return; // Another worker is already processing this module
        }

        // Snapshot phase while holding lock
        const phase = st.phase;
        if (!threading.is_freestanding) self.lock.unlock(self.roc_ctx.std_io);

        // Process the module based on its phase
        defer {
            // Atomically clear working flag when done
            if (!threading.is_freestanding) {
                self.lock.lockUncancelable(self.roc_ctx.std_io);
                if (task.module_id < self.modules.items.len) {
                    self.modules.items[task.module_id].working.store(0, .seq_cst);
                }
                self.lock.unlock(self.roc_ctx.std_io);
            } else {
                // Single-threaded: simple clear
                if (task.module_id < self.modules.items.len) {
                    self.modules.items[task.module_id].working = 0;
                }
            }
        }

        switch (phase) {
            .Parse => {
                try self.doParse(task.module_id);
            },
            .Canonicalize => {
                try self.doCanonicalize(task.module_id);
            },
            .WaitingOnImports => {
                try self.tryUnblock(task.module_id);
            },
            .TypeCheck => {
                try self.doTypeCheck(task.module_id);
            },
            .Done => {
                return;
            },
        }
        try self.tryEmitReady();
    }

    fn doParse(self: *PackageEnv, module_id: ModuleId) (Allocator.Error || error{FileNotFound})!void {
        // Load source and init ModuleEnv
        var st = &self.modules.items[module_id];
        const source_read = self.readModuleSourceForParse(st.path) catch |read_err| {
            // Note: Let the FileNotFound error propagate naturally
            // The existing error handling will report it appropriately
            st.source_file_state = if (self.track_watch_inputs) .missing else null;
            return read_err;
        };
        const src = source_read.source;
        st.source_file_state = source_read.file_state;

        // line starts for diagnostics and consistent positions

        const env = try self.gpa.create(ModuleEnv);
        errdefer self.gpa.destroy(env);
        env.* = try ModuleEnv.init(self.gpa, src);
        var env_owned_by_state = false;
        errdefer {
            if (!env_owned_by_state) {
                env.deinit();
                if (src.len > 0) self.gpa.free(src);
            }
        }
        // init CIR fields
        try env.initCIRFields(st.name);

        try env.common.calcLineStarts(self.gpa);

        // replace env - save old source to free it after deinit
        const old_source = if (st.moduleEnv()) |old| old.common.source else null;
        if (st.moduleEnv()) |old| {
            old.deinit();
            old.gpa.destroy(old);
        }
        if (old_source) |s| self.gpa.free(@constCast(s));
        st.replaceModuleEnv(env);
        env_owned_by_state = true;

        // Parse AST and cache for reuse in doCanonicalize (avoids double parsing)
        // IMPORTANT: Use st.moduleEnv().?.common (not local env.common) so the AST's pointer
        // to CommonEnv remains valid after this function returns.
        const parse_ast = parse.file(self.gpa, &st.moduleEnv().?.common) catch {
            // If parsing fails, proceed to canonicalization to report errors
            st.phase = .Canonicalize;
            try self.enqueue(module_id);
            return;
        };
        parse_ast.store.emptyScratch();

        // parse_ast is already heap-allocated by parse.file
        st.cached_ast = parse_ast;

        const local_imports = try module_discovery.extractImportsFromDeclIndex(parse_ast, self.gpa);
        defer {
            for (local_imports) |imp| self.gpa.free(imp);
            self.gpa.free(local_imports);
        }
        const external_imports = try module_discovery.extractQualifiedImportsFromDeclIndex(parse_ast, self.gpa);
        defer {
            for (external_imports) |imp| self.gpa.free(imp);
            self.gpa.free(external_imports);
        }

        var any_new: bool = false;
        for (local_imports) |mod_name| {
            const import_path = try self.resolveModulePath(mod_name);
            defer self.gpa.free(import_path);
            const prev_module_count = self.modules.items.len;
            const child_id = try self.ensureModule(mod_name, import_path);
            st = &self.modules.items[module_id];
            const is_new_import = child_id >= prev_module_count;
            try st.imports.append(self.gpa, child_id);
            try self.setDepthIfSmaller(child_id, st.depth + 1);

            var child = &self.modules.items[child_id];
            try child.dependents.append(self.gpa, module_id);

            if (child_id == module_id or (try self.findPath(child_id, module_id)) != null) {
                var rep = try Report.init(self.gpa, "Import Cycle Detected", "This module participates in an import cycle. Cycles between modules are not allowed.", .runtime_error);

                if (try self.findPath(child_id, module_id)) |path| {
                    defer self.gpa.free(path);
                    const hdr = try rep.addOwnedString("Cycle: ");
                    try rep.document.addText(hdr);
                    var i: usize = 0;
                    while (i < path.len) : (i += 1) {
                        if (i > 0) try rep.document.addText(" -> ");
                        try rep.document.addAnnotated(self.modules.items[path[i]].name, .emphasized);
                    }
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(self.modules.items[path[0]].name, .emphasized);
                    try rep.document.addLineBreak();
                } else {
                    const edge_msg = try rep.addOwnedString("Cycle edge: ");
                    try rep.document.addText(edge_msg);
                    try rep.document.addAnnotated(st.name, .emphasized);
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(mod_name, .emphasized);
                    try rep.document.addLineBreak();
                }

                try st.reports.append(self.gpa, rep);
                var rep_child = try Report.init(self.gpa, "Import Cycle Detected", "This module participates in an import cycle. Cycles between modules are not allowed.", .runtime_error);
                const edge_msg2 = try rep_child.addOwnedString("Cycle edge: ");
                try rep_child.document.addText(edge_msg2);
                try rep_child.document.addAnnotated(st.name, .emphasized);
                try rep_child.document.addText(" -> ");
                try rep_child.document.addAnnotated(mod_name, .emphasized);
                try rep_child.document.addLineBreak();
                try child.reports.append(self.gpa, rep_child);

                if (st.phase != .Done) {
                    st.phase = .Done;
                    self.remaining_modules -= 1;
                }
                if (child.phase != .Done) {
                    child.phase = .Done;
                    if (self.remaining_modules > 0) self.remaining_modules -= 1;
                }

                for (st.dependents.items) |dep| try self.enqueue(dep);
                for (child.dependents.items) |dep| try self.enqueue(dep);
                if (!threading.is_freestanding) self.cond.broadcast(self.roc_ctx.std_io);
                return;
            }

            if (is_new_import) {
                self.remaining_modules += 1;
                any_new = true;
            }
        }

        for (external_imports) |import_name| {
            try st.external_imports.append(self.gpa, try self.gpa.dupe(u8, import_name));
            if (self.resolver) |r| try r.scheduleExternal(r.ctx, self.package_name, import_name);
        }
        for (self.additional_known_modules.items) |km| {
            var exists = false;
            for (st.external_imports.items) |existing| {
                if (std.mem.eql(u8, existing, km.import_name)) {
                    exists = true;
                    break;
                }
            }
            if (!exists) {
                try st.external_imports.append(self.gpa, try self.gpa.dupe(u8, km.import_name));
                if (self.resolver) |r| try r.scheduleExternal(r.ctx, self.package_name, km.import_name);
            }
        }

        st.phase = .WaitingOnImports;
        if (any_new) {
            for (st.imports.items) |imp| try self.enqueue(imp);
        }
        try self.enqueue(module_id);
    }

    const SourceRead = struct {
        source: []u8,
        file_state: ?watch_inputs.State,
    };

    fn readModuleSourceForParse(self: *PackageEnv, path: []const u8) (Allocator.Error || error{FileNotFound})!SourceRead {
        if (!self.track_watch_inputs) {
            return .{
                .source = try self.readModuleSource(path),
                .file_state = null,
            };
        }

        return try self.readModuleSourceWithState(path);
    }

    fn readModuleSource(self: *PackageEnv, path: []const u8) (Allocator.Error || error{FileNotFound})![]u8 {
        const data = self.roc_ctx.readFile(path, self.gpa) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.FileNotFound,
        };

        // Normalize line endings (CRLF -> LF) for consistent cross-platform behavior.
        // This reallocates to the correct size if normalization occurs, ensuring
        // proper memory management when the buffer is freed later.
        return base.source_utils.normalizeLineEndingsRealloc(self.gpa, data);
    }

    fn readModuleSourceWithState(self: *PackageEnv, path: []const u8) (Allocator.Error || error{FileNotFound})!SourceRead {
        const data = self.roc_ctx.readFile(path, self.gpa) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.FileNotFound,
        };
        const file_state: watch_inputs.State = .{ .hash = watch_inputs.hashBytes(data) };

        // Normalize line endings (CRLF -> LF) for consistent cross-platform behavior.
        // This reallocates to the correct size if normalization occurs, ensuring
        // proper memory management when the buffer is freed later.
        const source = base.source_utils.normalizeLineEndingsRealloc(self.gpa, data) catch |err| {
            self.gpa.free(data);
            return err;
        };

        return .{
            .source = source,
            .file_state = file_state,
        };
    }

    fn doCanonicalize(self: *PackageEnv, module_id: ModuleId) Allocator.Error!void {
        var st = &self.modules.items[module_id];
        var env = st.moduleEnv().?;

        // Use cached AST from doParse - it should always be available
        const parse_ast: *parse.AST = st.cached_ast orelse
            std.debug.panic("Internal compiler error: cached AST missing for module '{s}'. Please report this bug.", .{st.name});
        st.cached_ast = null; // Take ownership
        defer parse_ast.deinit();

        // Convert parse diagnostics to reports
        for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
            const report = try parse_ast.tokenizeDiagnosticToReport(diagnostic, self.gpa, st.path);
            try st.reports.append(self.gpa, report);
        }
        for (parse_ast.parse_diagnostics.items) |diagnostic| {
            const report = try parse_ast.parseDiagnosticToReport(&env.common, diagnostic, self.gpa, st.path);
            try st.reports.append(self.gpa, report);
        }

        // canonicalize using the AST
        var canonicalize_timer = startStageTimer(self.roc_ctx.std_io);

        const module_dir = st.canonicalSourceDir(self.root_dir);
        try canonicalizeModuleWithSiblings(
            self.roc_ctx,
            env,
            parse_ast,
            self.builtin_modules.builtin_module.env,
            self.builtin_modules.builtin_indices,
            module_dir,
            self.package_name,
            self.resolver,
            self.additional_known_modules.items,
            &.{},
        );

        self.total_canonicalize_ns += readStageTimer(self.roc_ctx.std_io, &canonicalize_timer);

        // Collect canonicalization diagnostics
        var canonicalize_diagnostics_timer = startStageTimer(self.roc_ctx.std_io);
        const diags = try env.getDiagnostics();
        defer self.gpa.free(diags);
        for (diags) |d| {
            const report = try env.diagnosticToReport(d, self.gpa, st.path);
            try st.reports.append(self.gpa, report);
        }
        self.total_canonicalize_diagnostics_ns += readStageTimer(self.roc_ctx.std_io, &canonicalize_diagnostics_timer);

        // Discover imports from env.imports
        const import_count = env.imports.imports.items.items.len;
        var any_new: bool = false;
        // Mark current node as visiting (gray) before exploring imports
        st.visit_color = 1;
        for (env.imports.imports.items.items[0..import_count]) |str_idx| {
            const mod_name = env.getString(str_idx);

            if (can.CIR.Import.isCompilerBuiltinImportName(mod_name)) {
                continue;
            }

            // Use CIR qualifier metadata; this allocates nothing and scans only once
            const qualified = hadQualifiedImport(env, mod_name);

            if (qualified) {
                // Qualified imports refer to external packages; track and schedule externally
                try st.external_imports.append(self.gpa, mod_name);
                if (self.resolver) |r| try r.scheduleExternal(r.ctx, self.package_name, mod_name);
                // External dependencies are resolved by the workspace; skip local scheduling/cycle detection
                continue;
            }

            // Local import - schedule in this package
            const import_path = try self.resolveModulePath(mod_name);
            defer self.gpa.free(import_path);
            const prev_module_count = self.modules.items.len;
            const child_id = try self.ensureModule(mod_name, import_path);
            // Refresh st and env pointers in case ensureModule grew the modules array
            st = &self.modules.items[module_id];
            env = st.moduleEnv().?;
            const is_new_import = child_id >= prev_module_count;
            try st.imports.append(self.gpa, child_id);
            // parent depth + 1
            try self.setDepthIfSmaller(child_id, st.depth + 1);

            // Cycle detection for local deps
            var child = &self.modules.items[child_id];
            try child.dependents.append(self.gpa, module_id);

            if (child.visit_color == 1 or child_id == module_id) {
                // Build a report on the current module describing the cycle
                var rep = try Report.init(self.gpa, "Import Cycle Detected", "This module participates in an import cycle. Cycles between modules are not allowed.", .runtime_error);

                // Build full cycle path lazily (rare path): child_id ... module_id -> child_id
                if (try self.findPath(child_id, module_id)) |path| {
                    defer self.gpa.free(path);
                    const hdr = try rep.addOwnedString("Cycle: ");
                    try rep.document.addText(hdr);
                    var i: usize = 0;
                    while (i < path.len) : (i += 1) {
                        if (i > 0) try rep.document.addText(" -> ");
                        try rep.document.addAnnotated(self.modules.items[path[i]].name, .emphasized);
                    }
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(self.modules.items[path[0]].name, .emphasized);
                    try rep.document.addLineBreak();
                } else {
                    // Path not found; show the detected back-edge
                    const edge_msg = try rep.addOwnedString("Cycle edge: ");
                    try rep.document.addText(edge_msg);
                    try rep.document.addAnnotated(st.name, .emphasized);
                    try rep.document.addText(" -> ");
                    try rep.document.addAnnotated(mod_name, .emphasized);
                    try rep.document.addLineBreak();
                }

                // Store the report on both modules for clarity
                try st.reports.append(self.gpa, rep);
                // Duplicate for child as well so it gets emitted too
                var rep_child = try Report.init(self.gpa, "Import Cycle Detected", "This module participates in an import cycle. Cycles between modules are not allowed.", .runtime_error);
                const edge_msg2 = try rep_child.addOwnedString("Cycle edge: ");
                try rep_child.document.addText(edge_msg2);
                try rep_child.document.addAnnotated(st.name, .emphasized);
                try rep_child.document.addText(" -> ");
                try rep_child.document.addAnnotated(mod_name, .emphasized);
                try rep_child.document.addLineBreak();
                try child.reports.append(self.gpa, rep_child);

                // Mark both Done and adjust counters
                if (st.phase != .Done) {
                    if (comptime trace_build) {
                        std.debug.print("[PKG-CACHE] PHASE: {s} ->Done (CYCLE DETECTED with {s})\n", .{ st.name, mod_name });
                    }
                    st.phase = .Done;
                    self.remaining_modules -= 1;
                }
                if (child.phase != .Done) {
                    if (comptime trace_build) {
                        std.debug.print("[PKG-CACHE] PHASE: {s} ->Done (CYCLE DETECTED with {s})\n", .{ mod_name, st.name });
                    }
                    child.phase = .Done;
                    if (self.remaining_modules > 0) self.remaining_modules -= 1;
                }

                // Wake dependents and stop
                for (st.dependents.items) |dep| try self.enqueue(dep);
                for (child.dependents.items) |dep| try self.enqueue(dep);
                if (!threading.is_freestanding) self.cond.broadcast(self.roc_ctx.std_io);
                return;
            }

            if (is_new_import) {
                self.remaining_modules += 1;
                any_new = true;
            }
        }

        if (comptime trace_build) {
            std.debug.print("[PKG-CACHE] PHASE: {s} Canonicalize->WaitingOnImports (imports={d}, external={d})\n", .{
                st.name,
                st.imports.items.len,
                st.external_imports.items.len,
            });
        }
        st.phase = .WaitingOnImports;
        // Kick off imports if any (locals only)
        if (any_new) {
            for (st.imports.items) |imp| try self.enqueue(imp);
        }
        // Also re-enqueue self to check for unblocking
        try self.enqueue(module_id);
    }

    fn tryUnblock(self: *PackageEnv, module_id: ModuleId) Allocator.Error!void {
        var st = &self.modules.items[module_id];
        // If all imports are Done, move to Canonicalize
        var ready = true;

        // Local imports must be done
        for (st.imports.items) |imp| {
            const child = &self.modules.items[imp];
            if (child.phase != .Done) {
                ready = false;
                break;
            }
        }

        // External imports must be ready in the workspace (if resolver is provided)
        if (ready and st.external_imports.items.len > 0) {
            if (self.resolver) |r| {
                var k: usize = 0;
                while (k < st.external_imports.items.len) : (k += 1) {
                    if (!r.isReady(r.ctx, self.package_name, st.external_imports.items[k])) {
                        ready = false;
                        break;
                    }
                }
            } else {
                // No resolver; treat as not ready to avoid proceeding incorrectly
                ready = false;
            }
        }

        if (ready) {
            st.phase = .Canonicalize;
            // Mark as finished (black) when all children done
            st.visit_color = 2;
            try self.enqueue(module_id);
        }
    }

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
            .imported_modules = module_envs_out,
        });
        czer.source_dir = source_dir;
        try czer.canonicalizeFile();
        switch (validation_mode) {
            .none => {},
            .checking => try czer.validateForChecking(),
        }
        czer.deinit();

        env.imports.clearResolvedModules();
        try env.imports.resolveImportsByExactModuleName(env, imported_envs);
        env.imports.markUnresolvedImportsFailedBeforeChecking();

        // Type check using the SAME module_envs_map
        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = env.qualified_module_ident,
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

    /// Determine the statement_idx for a sibling module in the module_envs_map.
    /// For type modules (where methods are stored under qualified names like "Color.to_str"),
    /// returns the type declaration statement index so Can.zig uses qualified lookup.
    /// For regular modules (where members are stored under plain names like "to_str"),
    /// returns null so Can.zig uses bare-name lookup.
    fn computeSiblingStatementIdx(sibling_env: *const ModuleEnv, sibling_name: []const u8) ?can.CIR.Statement.Idx {
        // Only type modules store associated functions under qualified names.
        // Regular modules (deprecated_module, etc.) store them under plain names.
        switch (sibling_env.module_kind) {
            .type_module => {},
            else => return null,
        }
        const type_ident_in_module = sibling_env.common.findIdent(sibling_name) orelse return null;
        const type_node_idx = sibling_env.getExposedTypeNodeIndexById(type_ident_in_module) orelse return null;
        return @enumFromInt(type_node_idx);
    }

    /// Canonicalization function that also discovers sibling .roc files in the same directory
    /// and includes additional known modules (e.g., from platform exposes).
    /// This prevents premature MODULE NOT FOUND errors for modules that exist but haven't been loaded yet.
    pub fn canonicalizeModuleWithSiblings(
        roc_ctx: CoreCtx,
        env: *ModuleEnv,
        parse_ast: *AST,
        builtin_module_env: *const ModuleEnv,
        builtin_indices: can.CIR.BuiltinIndices,
        root_dir: []const u8,
        package_name: []const u8,
        resolver: ?ImportResolver,
        additional_known_modules: []const KnownModule,
        pre_resolved_imports: []const messages.CanonicalizeImport,
    ) Allocator.Error!void {
        const gpa = roc_ctx.gpa;

        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
        defer module_envs_map.deinit();

        // Add sibling modules whose environments are already available.
        // Canonicalization consumes concrete exposed-node data from dependencies.
        const sibling_imports = try module_discovery.extractImportsFromDeclIndex(parse_ast, gpa);
        defer {
            for (sibling_imports) |imp| gpa.free(imp);
            gpa.free(sibling_imports);
        }

        for (sibling_imports) |sibling_name| {
            // Skip self
            if (std.mem.eql(u8, sibling_name, env.module_name)) continue;

            const sibling_ident = try env.insertIdent(base.Ident.for_text(sibling_name));
            const qualified_ident = try env.insertIdent(base.Ident.for_text(sibling_name));

            // Check if sibling file exists (via Io abstraction)
            const file_name = try std.fmt.allocPrint(gpa, "{s}.roc", .{sibling_name});
            defer gpa.free(file_name);
            const file_path = try std.fs.path.join(gpa, &.{ root_dir, file_name });
            defer gpa.free(file_path);
            const exists = roc_ctx.fileExists(file_path);
            if (!exists) continue;

            // Check pre-resolved imports first (e.g., from coordinator's built dependency list)
            const pre_resolved_env: ?*const ModuleEnv = blk: {
                for (pre_resolved_imports) |pre| {
                    if (std.mem.eql(u8, pre.import_name, sibling_name)) break :blk pre.module_env;
                }
                break :blk null;
            };

            if (pre_resolved_env) |sibling_env| {
                const statement_idx = computeSiblingStatementIdx(sibling_env, sibling_name);
                try module_envs_map.put(sibling_ident, .{
                    .env = sibling_env,
                    .statement_idx = statement_idx,
                    .qualified_type_ident = qualified_ident,
                });
                continue;
            }

            // Try to get actual env from resolver if available
            if (resolver) |res| {
                if (res.getEnv(res.ctx, package_name, sibling_name)) |sibling_env| {
                    const statement_idx = computeSiblingStatementIdx(sibling_env, sibling_name);
                    try module_envs_map.put(sibling_ident, .{
                        .env = sibling_env,
                        .statement_idx = statement_idx,
                        .qualified_type_ident = qualified_ident,
                    });
                    continue;
                }
            }
        }

        // Add additional known modules (e.g., from platform exposes for URL platforms)
        // Use the resolver to get the ACTUAL module env if available
        for (additional_known_modules) |km| {
            // Extract base module name (e.g., "Stdout" from "pf.Stdout")
            const base_module_name = if (std.mem.findScalarLast(u8, km.qualified_name, '.')) |dot_idx|
                km.qualified_name[dot_idx + 1 ..]
            else
                km.qualified_name;

            // Create identifiers for both the unqualified name and the qualified name
            const base_ident = try env.insertIdent(base.Ident.for_text(base_module_name));
            const qualified_ident = try env.insertIdent(base.Ident.for_text(km.qualified_name));

            // Try to get the actual module env. Prefer an already-built env the
            // coordinator supplied via pre_resolved_imports (matching the sibling
            // path above), then ask the resolver.
            const actual_env: *const ModuleEnv = blk: {
                for (pre_resolved_imports) |pre| {
                    if (std.mem.eql(u8, pre.import_name, km.import_name) or
                        std.mem.eql(u8, pre.import_name, base_module_name))
                    {
                        break :blk pre.module_env;
                    }
                    // Match on base module name too (e.g. pre "pf.Host" vs base "Host").
                    const pre_base = if (std.mem.findScalarLast(u8, pre.import_name, '.')) |d|
                        pre.import_name[d + 1 ..]
                    else
                        pre.import_name;
                    if (std.mem.eql(u8, pre_base, base_module_name)) break :blk pre.module_env;
                }
                if (resolver) |res| {
                    if (res.getEnv(res.ctx, package_name, km.import_name)) |mod_env| {
                        break :blk mod_env;
                    }
                }
                continue;
            };

            // For platform type modules, set statement_idx so method lookups work correctly
            const statement_idx: ?can.CIR.Statement.Idx = stmt_blk: {
                // Look up the type in the module's exposed_items to get the actual node index
                const type_ident_in_module = actual_env.common.findIdent(base_module_name) orelse break :stmt_blk null;
                const type_node_idx = actual_env.getExposedTypeNodeIndexById(type_ident_in_module) orelse break :stmt_blk null;
                break :stmt_blk @enumFromInt(type_node_idx);
            };

            const entry = Can.AutoImportedType{
                .env = actual_env,
                .statement_idx = statement_idx,
                .qualified_type_ident = base_ident,
                .is_package_qualified = true,
            };

            // Add entry for the UNQUALIFIED name (e.g., "Stdout", "Builder")
            // This is used for type annotations like `my_var : Builder`
            if (!module_envs_map.contains(base_ident)) {
                try module_envs_map.put(base_ident, entry);
            }

            // Also add entry for the QUALIFIED name (e.g., "pf.Stdout", "pf.Builder")
            // This is used when scopeLookupModule returns the qualified module name
            if (!module_envs_map.contains(qualified_ident)) {
                try module_envs_map.put(qualified_ident, entry);
            }
        }

        var czer = try Can.initModule(roc_ctx, env, parse_ast, .{
            .builtin_types = .{
                .builtin_module_env = builtin_module_env,
                .builtin_indices = builtin_indices,
            },
            .imported_modules = &module_envs_map,
        });
        czer.source_dir = root_dir;
        try czer.canonicalizeFile();
        try czer.validateForChecking();
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
        ctfe_options: eval.CompileTimeFinalization.Options,
    ) TypeCheckModuleError!TypeCheckOutput {
        const builtin_indices = compiled_builtins.builtinIndices(can.CIR);

        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = env.qualified_module_ident,
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = builtin_module_env,
            .builtin_indices = builtin_indices,
        };

        // Create module_envs map for explicit imported modules used during canonicalization
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(check_alloc);
        errdefer module_envs_map.deinit();

        const owner_envs = try buildCheckOwnerEnvs(check_alloc, imported_envs, imported_artifacts, available_artifacts);
        defer check_alloc.free(owner_envs);

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
        checker.fixupTypeWriter();
        errdefer checker.deinit();

        // For app modules with platform requirements, defer finalizing numeric defaults
        // until after platform requirements are checked, so numeric literals can be
        // constrained by platform types (e.g., I64) before defaulting to Dec.
        // TODO: re-enable defer_numeric_defaults once ModuleEnv has the field
        try checker.checkFile();

        module_envs_map.deinit();

        const has_artifact_blocking_canonicalize_diagnostics = moduleHasArtifactBlockingCanonicalizeDiagnostics(env);
        const has_duplicate_top_level_value_defs = try moduleHasDuplicateTopLevelValueDefs(check_alloc, env);
        const has_artifact_blocking_check_problems = checkerHasArtifactBlockingProblems(&checker);
        const imported_artifacts_cover_imports = importedArtifactsCoverImportedEnvs(imported_envs, imported_artifacts);
        const user_errors_allow_lowering =
            !has_artifact_blocking_canonicalize_diagnostics and
            !has_duplicate_top_level_value_defs and
            !has_artifact_blocking_check_problems and
            imported_artifacts_cover_imports and
            checkerProblemsAllowLoweringWithUserErrors(&checker);

        if (has_artifact_blocking_canonicalize_diagnostics or
            has_duplicate_top_level_value_defs or
            has_artifact_blocking_check_problems or
            !imported_artifacts_cover_imports)
        {
            _ = try checker.problems.flushPendingStaticExhaustiveness(check_alloc);
            return .{
                .checker = checker,
                .checked_artifact = null,
                .user_errors_allow_lowering = false,
            };
        }

        var checked_artifact = publishCheckedArtifactFromCheckedModule(
            artifact_alloc,
            env,
            imported_envs,
            imported_artifacts,
            .{
                .platform_requirement_context = platform_requirement_context,
                .platform_app_relation = null,
                .explicit_roots = explicit_roots,
                .hoisted_roots = checker.selectedHoistedRoots(),
                .available_artifacts = available_artifacts,
                .problem_store = &checker.problems,
                .ctfe_options = ctfe_options,
            },
        ) catch |err| switch (err) {
            error.CompileTimeProblem => {
                _ = try checker.problems.flushPendingStaticExhaustiveness(check_alloc);
                return .{
                    .checker = checker,
                    .checked_artifact = null,
                    .user_errors_allow_lowering = false,
                };
            },
            else => |other| return other,
        };
        errdefer checked_artifact.deinit(artifact_alloc);

        return .{
            .checker = checker,
            .checked_artifact = checked_artifact,
            .user_errors_allow_lowering = user_errors_allow_lowering,
        };
    }

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
                .explicit_roots = publication.explicit_roots,
                .hoisted_roots = publication.hoisted_roots,
                .compile_time_finalizer = eval.CompileTimeFinalization.finalizerWithOptions(&ctfe_options),
                .problem_store = publication.problem_store,
            },
        );
    }

    fn doTypeCheck(self: *PackageEnv, module_id: ModuleId) TypeCheckModuleError!void {
        var st = &self.modules.items[module_id];
        var env = st.moduleEnv().?;

        // Build the array of all available modules for this module's imports
        const import_count = env.imports.imports.items.items.len;
        var imported_envs = try std.ArrayList(*ModuleEnv).initCapacity(self.gpa, import_count);
        var imported_artifacts = std.ArrayList(CheckedArtifact.PublishImportArtifact).empty;
        // Always include Builtin first
        try imported_envs.append(self.gpa, self.builtin_modules.builtin_module.env);
        try imported_artifacts.append(self.gpa, .{
            .module_idx = 0,
            .key = self.builtin_modules.checked_artifact.key,
            .view = CheckedArtifact.importedView(&self.builtin_modules.checked_artifact),
        });
        env.imports.clearResolvedModules();

        // Add external and local modules
        for (env.imports.imports.items.items[0..import_count], 0..) |str_idx, i| {
            const import_idx: can.CIR.Import.Idx = @enumFromInt(i);
            const import_name = env.getString(str_idx);

            if (can.CIR.Import.isCompilerBuiltinImportName(import_name)) {
                env.imports.setResolvedModule(import_idx, 0);
                continue;
            }

            // Determine external vs local from CIR s_import qualifier metadata directly
            const is_ext = hadQualifiedImport(env, import_name);

            if (is_ext) {
                if (self.resolver) |r| {
                    if (r.getEnv(r.ctx, self.package_name, import_name)) |ext_env_ptr| {
                        const resolved_module_idx: u32 = @intCast(imported_envs.items.len);
                        try imported_envs.append(self.gpa, ext_env_ptr);
                        env.imports.setResolvedModule(import_idx, resolved_module_idx);
                        if (r.getArtifact(r.ctx, self.package_name, import_name)) |artifact| {
                            try imported_artifacts.append(self.gpa, .{
                                .module_idx = resolved_module_idx,
                                .key = artifact.key,
                                .view = CheckedArtifact.importedView(artifact),
                            });
                        }
                    }
                    // External env not ready; skip (tryUnblock should have prevented this)
                }
            } else {
                const child_id = self.module_names.get(import_name).?;
                const child = &self.modules.items[child_id];
                const child_env_ptr = child.moduleEnv() orelse
                    std.debug.panic("compile.doTypeCheck missing local env for ready import '{s}'", .{child.name});
                const resolved_module_idx: u32 = @intCast(imported_envs.items.len);
                try imported_envs.append(self.gpa, child_env_ptr);
                env.imports.setResolvedModule(import_idx, resolved_module_idx);
                if (child.checkedArtifact()) |artifact| {
                    try imported_artifacts.append(self.gpa, .{
                        .module_idx = resolved_module_idx,
                        .key = artifact.key,
                        .view = CheckedArtifact.importedView(artifact),
                    });
                }
            }
        }

        const available_artifacts = try self.buildAvailableArtifactViewsForImports(imported_artifacts.items);
        defer self.gpa.free(available_artifacts);

        var check_timer = startStageTimer(self.roc_ctx.std_io);
        var typecheck_output = try typeCheckModule(
            self.gpa,
            self.gpa,
            env,
            self.builtin_modules.builtin_module.env,
            imported_envs.items,
            imported_artifacts.items,
            available_artifacts,
            null,
            null,
            &.{},
            compileTimeFinalizationOptions(self.max_threads, &self.roc_ctx),
        );
        defer typecheck_output.deinit();
        if (typecheck_output.checked_artifact != null) {
            st.replaceCheckedArtifact(typecheck_output.takeCheckedArtifact());
        }
        self.total_type_checking_ns += readStageTimer(self.roc_ctx.std_io, &check_timer);

        // Build reports from problems
        var check_diagnostics_timer = startStageTimer(self.roc_ctx.std_io);
        var rb = try ReportBuilder.init(
            self.gpa,
            env,
            env,
            &typecheck_output.checker.snapshots,
            &typecheck_output.checker.problems,
            st.path,
            imported_envs.items,
            &typecheck_output.checker.import_mapping,
            &typecheck_output.checker.regions,
            null,
        );
        defer rb.deinit();
        for (typecheck_output.checker.problems.problems.items) |prob| {
            var rep = try rb.build(prob);
            errdefer rep.deinit();
            try st.reports.append(self.gpa, rep);
        }
        self.total_check_diagnostics_ns += readStageTimer(self.roc_ctx.std_io, &check_diagnostics_timer);

        // Comptime evaluator is managed inside typeCheckModule, no need to deinit here

        // Now we can safely deinit the 'imported_envs' ArrayList
        imported_envs.deinit(self.gpa);
        imported_artifacts.deinit(self.gpa);

        // Note: We no longer need to free the 'imported_envs' items because they now point directly
        // to ModuleEnv instances stored in the modules ArrayList, not to heap-allocated copies.

        // Done
        st.phase = .Done;
        self.remaining_modules -= 1;

        // Wake dependents to re-check unblock
        for (st.dependents.items) |dep| try self.enqueue(dep);
        if (!threading.is_freestanding) self.cond.broadcast(self.roc_ctx.std_io);
    }

    fn buildAvailableArtifactViewsForImports(
        self: *PackageEnv,
        imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    ) Allocator.Error![]const CheckedArtifact.ImportedModuleView {
        var views = std.ArrayList(CheckedArtifact.ImportedModuleView).empty;
        errdefer views.deinit(self.gpa);

        var pending = std.ArrayList(CheckedArtifact.ImportedModuleView).empty;
        defer pending.deinit(self.gpa);

        var seen = std.AutoHashMap(CheckedArtifact.CheckedModuleArtifactKey, void).init(self.gpa);
        defer seen.deinit();

        for (imported_artifacts) |imported| {
            try pending.append(self.gpa, imported.view);
        }

        while (pending.pop()) |view| {
            const entry = try seen.getOrPut(view.key);
            if (entry.found_existing) continue;
            entry.value_ptr.* = {};

            try views.append(self.gpa, view);

            for (view.public_api_dependencies.type_owner_artifacts) |dependency_key| {
                const dependency = self.availableArtifactViewByKey(imported_artifacts, dependency_key) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("compile.PackageEnv missing type-owner dependency checked artifact", .{});
                    }
                    unreachable;
                };
                try pending.append(self.gpa, dependency);
            }
        }

        return try views.toOwnedSlice(self.gpa);
    }

    fn availableArtifactViewByKey(
        self: *PackageEnv,
        imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
        key: CheckedArtifact.CheckedModuleArtifactKey,
    ) ?CheckedArtifact.ImportedModuleView {
        if (checkedModuleArtifactKeyEql(self.builtin_modules.checked_artifact.key, key)) {
            return CheckedArtifact.importedView(&self.builtin_modules.checked_artifact);
        }
        for (imported_artifacts) |imported| {
            if (checkedModuleArtifactKeyEql(imported.key, key)) return imported.view;
        }
        for (self.modules.items) |*module_state| {
            const artifact = module_state.checkedArtifact() orelse continue;
            if (checkedModuleArtifactKeyEql(artifact.key, key)) return CheckedArtifact.importedView(artifact);
        }
        return null;
    }

    fn resolveModulePath(self: *PackageEnv, mod_name: []const u8) Allocator.Error![]const u8 {
        // Allow resolver to provide local path resolution if present
        if (self.resolver) |r| {
            return try r.resolveLocalPath(r.ctx, self.package_name, self.root_dir, mod_name);
        }

        // Default: convert dotted module name to path under root_dir
        var buffer = std.ArrayList(u8).empty;
        defer buffer.deinit(self.gpa);
        var it = std.mem.splitScalar(u8, mod_name, '.');
        var first = true;
        while (it.next()) |part| {
            if (!first) try buffer.appendSlice(self.gpa, std.fs.path.sep_str) else first = false;
            try buffer.appendSlice(self.gpa, part);
        }
        try buffer.appendSlice(self.gpa, ".roc");
        const rel = try buffer.toOwnedSlice(self.gpa);
        const full = try std.fs.path.join(self.gpa, &.{ self.root_dir, rel });
        self.gpa.free(rel);
        return full;
    }

    // Determine if an import was qualified (e.g. "cli.Stdout") using CIR statements.
    // After canonicalization, qualified imports have their full name (e.g., "pf.Stdout")
    // stored in module_name_tok, and qualifier_tok is null.
    // So we check if the module name contains a dot to determine if it was qualified.
    fn hadQualifiedImport(env: *ModuleEnv, mod_name_text: []const u8) bool {
        const stmts = env.store.sliceStatements(env.all_statements);
        for (stmts) |stmt_idx| {
            const stmt = env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_import => |imp| {
                    const module_name = env.getIdent(imp.module_name_tok);
                    if (!std.mem.eql(u8, module_name, mod_name_text)) continue;
                    // After canonicalization, qualified imports have their full name
                    // stored in module_name_tok. Check if it contains a dot.
                    return std.mem.findScalar(u8, module_name, '.') != null;
                },
                else => {},
            }
        }
        return false;
    }

    // On-demand DFS to find a path from start -> target along import edges.
    // Returns an owned slice of module IDs.
    fn findPath(self: *PackageEnv, start: ModuleId, target: ModuleId) Allocator.Error!?[]const ModuleId {
        var visited = std.bit_set.DynamicBitSetUnmanaged{};
        defer visited.deinit(self.gpa);
        try visited.resize(self.gpa, self.modules.items.len, false);

        const Frame = struct { id: ModuleId, next_idx: usize };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(self.gpa);

        var stack_ids = std.ArrayList(ModuleId).empty;
        defer stack_ids.deinit(self.gpa);

        visited.set(start);
        try frames.append(self.gpa, .{ .id = start, .next_idx = 0 });
        try stack_ids.append(self.gpa, start);

        while (frames.items.len > 0) {
            var top = &frames.items[frames.items.len - 1];
            if (top.id == target) {
                const out = try self.gpa.alloc(ModuleId, stack_ids.items.len);
                std.mem.copyForwards(ModuleId, out, stack_ids.items);
                return out;
            }

            const st = &self.modules.items[top.id];
            if (top.next_idx >= st.imports.items.len) {
                visited.unset(top.id);
                _ = stack_ids.pop();
                _ = frames.pop();
                continue;
            }

            const child = st.imports.items[top.next_idx];
            top.next_idx += 1;

            if (!visited.isSet(child)) {
                visited.set(child);
                try frames.append(self.gpa, .{ .id = child, .next_idx = 0 });
                try stack_ids.append(self.gpa, child);
            }
        }
        return null;
    }

    /// Extract the module name from a file path.
    /// Delegates to base.module_path.getModuleName for the implementation.
    pub fn moduleNameFromPath(path: []const u8) []const u8 {
        return base.module_path.getModuleName(path);
    }

    pub fn tryEmitReady(self: *PackageEnv) Allocator.Error!void {
        // Sort discovered modules by (depth, name) each time; emit in prefix order
        if (self.discovered.items.len == 0) return;

        // Ensure emitted bitset is big enough
        try self.emitted.resize(self.gpa, self.modules.items.len, false);

        const ids = try self.gpa.alloc(ModuleId, self.discovered.items.len);
        defer self.gpa.free(ids);
        std.mem.copyForwards(ModuleId, ids, self.discovered.items);
        std.sort.block(ModuleId, ids, self, struct {
            fn lessThan(ctx: *PackageEnv, a: ModuleId, b: ModuleId) bool {
                const sa = ctx.modules.items[a].depth;
                const sb = ctx.modules.items[b].depth;
                if (sa == sb) return std.mem.lessThan(u8, ctx.modules.items[a].name, ctx.modules.items[b].name);
                return sa < sb;
            }
        }.lessThan);

        for (ids) |id| {
            if (self.emitted.isSet(id)) continue;
            const st = &self.modules.items[id];
            if (st.phase != .Done) break; // can't emit beyond an unfinished module in order
            // Emit all reports for this module
            for (st.reports.items) |rep| try self.sink.emitFn(self.sink.ctx, st.name, rep);
            // Clear reports to transfer ownership - reports are shallow-copied when passed to emitFn,
            // so OrderedSink now owns the heap allocations. We must not free them here.
            st.reports.clearRetainingCapacity();
            // Mark emitted
            self.emitted.set(id);
        }
    }
};

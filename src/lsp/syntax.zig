//! Syntax checking integration that runs the Roc compiler and converts
//! reports to LSP diagnostics.

const std = @import("std");
const builtin = @import("builtin");
const compile = @import("compile");
const reporting = @import("reporting");
const build_options = @import("build_options");
const CoreCtx = @import("ctx").CoreCtx;
const Allocator = std.mem.Allocator;
const base = @import("base");
const can = @import("can");
const eval = @import("eval");
const types = @import("types");

const Diagnostics = @import("diagnostics.zig");
const uri_util = @import("uri.zig");
const DependencyGraph = @import("dependency_graph.zig").DependencyGraph;
const compiled_builtins = @import("compiled_builtins");
const scope_map = @import("scope_map.zig");
const BuildSession = @import("build_session.zig").BuildSession;
const cir_queries = @import("cir_queries.zig");
const module_lookup = @import("module_lookup.zig");
const completion_context = @import("completion/context.zig");
const completion_builtins = @import("completion/builtins.zig");
const completion_builder = @import("completion/builder.zig");
const BuildEnvHandle = @import("build_env_handle.zig").BuildEnvHandle;
const doc_comments = @import("doc_comments.zig");
const rename_rules = @import("rename.zig");

const BuildEnv = compile.BuildEnv;
const CacheManager = compile.CacheManager;
const roc_target = @import("roc_target");
const CacheConfig = compile.CacheConfig;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Region = base.Region;

pub const DebugFlags = @import("debug.zig").DebugFlags;

/// Errors that can occur while preparing the syntax check build environment.
pub const SyntaxBuildEnvError = Allocator.Error || std.Io.Dir.RealPathFileAllocError || eval.BuiltinModules.InitError;
/// Errors that can occur while preparing a document for syntax checking.
pub const SyntaxPrepareDocumentError = SyntaxBuildEnvError;
/// Errors that can occur while checking syntax for a document.
pub const SyntaxCheckError = SyntaxPrepareDocumentError || error{WriteFailed};
/// Errors that can occur while answering syntax-backed LSP queries.
pub const SyntaxQueryError = SyntaxPrepareDocumentError || error{WriteFailed};

const MethodOwnerLookup = struct {
    owner: CIR.Statement.Idx,
    type_ident: base.Ident.Idx,
    builtin_origin: bool,
};

fn statementTypeAnno(module_env: *const ModuleEnv, statement: CIR.Statement) ?CIR.TypeAnno.Idx {
    return switch (statement) {
        .s_decl => |decl| if (decl.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
        .s_var => |var_stmt| if (var_stmt.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
        .s_var_uninitialized => |var_stmt| if (var_stmt.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
        .s_type_anno => |type_anno| type_anno.anno,
        .s_alias_decl => |alias| alias.anno,
        .s_nominal_decl => |nominal| nominal.anno,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_where_alias_decl,
        .s_type_var_alias,
        .s_runtime_error,
        => null,
    };
}

/// Runs BuildEnv-backed syntax/type checks and converts reports to LSP diagnostics.
pub const SyntaxChecker = struct {
    allocator: std.mem.Allocator,
    std_io: std.Io,
    mutex: std.Io.Mutex = std.Io.Mutex.init,
    /// Current build environment owned by the live check path.
    build_env: ?*BuildEnvHandle = null,
    /// Previous successful BuildEnv kept for module lookups (e.g., semantic tokens).
    /// This is swapped with build_env after each successful build.
    previous_build_env: ?*BuildEnvHandle = null,
    /// Snapshot of the most recent successful build per module (kept for completions).
    snapshot_envs: std.StringHashMapUnmanaged(*BuildEnvHandle) = .{},
    /// Pre-published Builtin module reused by each fresh LSP BuildEnv.
    builtin_modules: ?*eval.BuiltinModules = null,
    /// Dependency graph for tracking module relationships and invalidation.
    dependency_graph: DependencyGraph,
    /// Absolute workspace root from LSP initialize (`rootUri`), used to prefer
    /// `{workspace_root}/main.roc` for package-alias resolution.
    workspace_root: ?[]u8 = null,
    /// URIs for which we last published a non-empty diagnostic set. Used to
    /// emit empty clears when those diagnostics are no longer valid.
    published_diagnostic_uris: std.StringHashMapUnmanaged(void) = .{},
    cache_config: CacheConfig,
    log_file: ?std.Io.File = null,
    debug: DebugFlags,

    // Owner tags used for BuildEnvHandle debugging.
    const owner_build = "build_env";
    const owner_previous = "previous_build_env";
    const owner_snapshot = "snapshot";
    pub const CheckError = SyntaxCheckError;
    pub const QueryError = SyntaxQueryError;

    const DocumentIdentity = struct {
        absolute_path: [:0]u8,
        content_hash: [32]u8,

        fn deinit(self: *DocumentIdentity, allocator: Allocator) void {
            allocator.free(self.absolute_path);
        }
    };

    const DocumentBuild = struct {
        checker: *SyntaxChecker,
        identity: ?DocumentIdentity,
        session: ?BuildSession,
        env: *BuildEnv,
        absolute_path: []const u8,
        build_succeeded: bool,
        has_reports: bool,
        reused: bool,

        fn deinit(self: *DocumentBuild) void {
            if (self.session) |*session| {
                session.deinit();
            }
            if (self.identity) |*identity| {
                identity.deinit(self.checker.allocator);
            }
            self.checker.allocator.free(self.absolute_path);
        }

        fn getModuleEnv(self: *DocumentBuild) ?*ModuleEnv {
            if (self.reused) {
                return self.checker.getModuleEnvByPathInEnv(self.env, self.absolute_path);
            }
            if (self.session) |*s| {
                return s.getModuleEnv();
            }
            return null;
        }
    };

    fn isCompilerOwnedBuiltin(self: *const SyntaxChecker, path: []const u8) bool {
        const filename = std.fs.path.basename(path);
        if (!std.mem.eql(u8, filename, "Builtin.roc")) return false;
        if (self.cache_config.getModuleCacheDir(self.allocator)) |cache_dir| {
            defer self.allocator.free(cache_dir);
            const builtin_cache_path = std.fs.path.join(self.allocator, &.{ cache_dir, "Builtin.roc" }) catch return false;
            defer self.allocator.free(builtin_cache_path);
            return std.mem.eql(u8, path, builtin_cache_path);
        } else |_| {}
        return false;
    }

    pub fn init(allocator: std.mem.Allocator, std_io: std.Io, debug: DebugFlags, log_file: ?std.Io.File) SyntaxChecker {
        var cache_config = CacheConfig{ .roc_ctx = CoreCtx.default(allocator, allocator, std_io) };
        if (builtin.is_test) {
            cache_config.enabled = false;
        }

        return .{
            .allocator = allocator,
            .std_io = std_io,
            .dependency_graph = DependencyGraph.init(allocator),
            .cache_config = cache_config,
            .debug = debug,
            .log_file = log_file,
        };
    }

    pub fn deinit(self: *SyntaxChecker) void {
        // Release live handles first, then snapshots.
        // Handles guarantee the BuildEnv is freed exactly once.
        if (self.build_env) |handle| {
            handle.release(owner_build);
            self.build_env = null;
        }
        if (self.previous_build_env) |handle| {
            handle.release(owner_previous);
            self.previous_build_env = null;
        }

        self.clearSnapshots();

        // Free hashmap allocations
        self.snapshot_envs.deinit(self.allocator);

        if (self.workspace_root) |root| {
            self.allocator.free(root);
            self.workspace_root = null;
        }
        self.clearPublishedDiagnosticUris();
        self.published_diagnostic_uris.deinit(self.allocator);

        if (self.builtin_modules) |builtin_modules| {
            builtin_modules.deinit();
            self.allocator.destroy(builtin_modules);
            self.builtin_modules = null;
        }

        self.dependency_graph.deinit();
    }

    fn clearPublishedDiagnosticUris(self: *SyntaxChecker) void {
        var it = self.published_diagnostic_uris.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.published_diagnostic_uris.clearRetainingCapacity();
    }

    /// Test helper: record a URI as having published non-empty diagnostics.
    ///
    /// Not gated on `builtin.is_test` (unlike `getDocumentForTesting` in server.zig):
    /// the LSP integration harness runs as a plain executable (`addExecutable`, not
    /// `addTest`), so `builtin.is_test` is false there and such a guard would make
    /// this helper silently inert for the integration specs that rely on it.
    pub fn seedPublishedDiagnosticUriForTesting(self: *SyntaxChecker, uri: []const u8) Allocator.Error!void {
        const owned = try self.allocator.dupe(u8, uri);
        const gop = try self.published_diagnostic_uris.getOrPut(self.allocator, owned);
        if (gop.found_existing) self.allocator.free(owned);
    }

    /// Test helper: whether a URI is tracked as having published non-empty diagnostics.
    /// See `seedPublishedDiagnosticUriForTesting` for why this isn't gated on `builtin.is_test`.
    pub fn hasPublishedDiagnosticUriForTesting(self: *SyntaxChecker, uri: []const u8) bool {
        return self.published_diagnostic_uris.contains(uri);
    }

    fn updateWorkspaceRoot(self: *SyntaxChecker, workspace_root: ?[]const u8) Allocator.Error!void {
        if (self.workspace_root) |old| {
            self.allocator.free(old);
            self.workspace_root = null;
        }
        if (workspace_root) |root| {
            self.workspace_root = try self.allocator.dupe(u8, root);
        }
    }

    fn documentIdentityFromText(self: *SyntaxChecker, uri: []const u8, text: []const u8) std.mem.Allocator.Error!DocumentIdentity {
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path: [:0]u8 = std.Io.Dir.cwd().realPathFileAlloc(self.std_io, path, self.allocator) catch
            try self.allocator.dupeZ(u8, path);

        return .{
            .absolute_path = absolute_path,
            .content_hash = DependencyGraph.computeContentHash(text),
        };
    }

    fn matchingBuildEnvHandle(self: *SyntaxChecker, absolute_path: []const u8, content_hash: [32]u8) ?*BuildEnvHandle {
        if (self.build_env) |handle| {
            if (handle.matchesDocumentContent(absolute_path, content_hash)) {
                return handle;
            }
        }

        if (self.snapshot_envs.get(absolute_path)) |handle| {
            if (handle.matchesDocumentContent(absolute_path, content_hash)) {
                return handle;
            }
        }

        if (self.previous_build_env) |handle| {
            if (handle.matchesDocumentContent(absolute_path, content_hash)) {
                return handle;
            }
        }

        return null;
    }

    fn documentHasReports(_: *SyntaxChecker, absolute_path: []const u8, drained_reports: ?[]BuildEnv.DrainedModuleReports) bool {
        const drained = drained_reports orelse return true;
        for (drained) |entry| {
            if (std.mem.eql(u8, entry.abs_path, absolute_path) and entry.reports.len > 0) {
                return true;
            }
        }
        return false;
    }

    fn prepareDocumentBuild(self: *SyntaxChecker, uri: []const u8, override_text: ?[]const u8) SyntaxPrepareDocumentError!DocumentBuild {
        var identity: ?DocumentIdentity = if (override_text) |text|
            try self.documentIdentityFromText(uri, text)
        else
            null;
        errdefer if (identity) |*document| document.deinit(self.allocator);

        if (identity) |document| {
            if (self.matchingBuildEnvHandle(document.absolute_path, document.content_hash)) |handle| {
                const env = handle.envPtr();
                const abs_path = try self.allocator.dupe(u8, document.absolute_path);
                return .{
                    .checker = self,
                    .identity = identity,
                    .session = null,
                    .env = env,
                    .absolute_path = abs_path,
                    .build_succeeded = self.getModuleEnvByPathInEnv(env, document.absolute_path) != null,
                    .has_reports = handle.hasDocumentReports(),
                    .reused = true,
                };
            }
        }

        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        if (self.isCompilerOwnedBuiltin(path)) {
            const env_handle = try self.createFreshBuildEnv();
            const env = env_handle.envPtr();
            const abs_path = try self.allocator.dupe(u8, path);
            return .{
                .checker = self,
                .identity = identity,
                .session = null,
                .env = env,
                .absolute_path = abs_path,
                .build_succeeded = false,
                .has_reports = false,
                .reused = false,
            };
        }

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, self.std_io, env, uri, override_text, self.workspace_root);
        errdefer session.deinit();

        const has_reports = self.documentHasReports(session.absolute_path, session.drained_reports);

        if (identity) |document| {
            env_handle.setDocumentContent(document.absolute_path, document.content_hash, has_reports) catch |err| {
                self.logDebug(.build, "Failed to record document content: {s}", .{@errorName(err)});
            };
        }

        const abs_path = try self.allocator.dupe(u8, session.absolute_path);
        return .{
            .checker = self,
            .identity = identity,
            .session = session,
            .env = env,
            .absolute_path = abs_path,
            .build_succeeded = session.build_succeeded,
            .has_reports = has_reports,
            .reused = false,
        };
    }

    /// Check the file referenced by the URI and return diagnostics grouped by URI.
    pub fn check(self: *SyntaxChecker, uri: []const u8, override_text: ?[]const u8, workspace_root: ?[]const u8) CheckError![]Diagnostics.PublishDiagnostics {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        try self.updateWorkspaceRoot(workspace_root);

        const check_path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(check_path);

        if (self.isCompilerOwnedBuiltin(check_path)) {
            return try self.allocator.alloc(Diagnostics.PublishDiagnostics, 0);
        }

        // Check if content has changed using hash comparison BEFORE building.
        // This avoids unnecessary rebuilds on focus/blur events.
        if (override_text) |text| {
            const path = try uri_util.uriToPath(self.allocator, uri);
            defer self.allocator.free(path);

            const abs_path: ?[:0]u8 = std.Io.Dir.cwd().realPathFileAlloc(self.std_io, path, self.allocator) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AccessDenied,
                error.AntivirusInterference,
                error.BadPathName,
                error.Canceled,
                error.DeviceBusy,
                error.FileBusy,
                error.FileNotFound,
                error.FileSystem,
                error.FileTooBig,
                error.InputOutput,
                error.IsDir,
                error.NameTooLong,
                error.NetworkNotFound,
                error.NoDevice,
                error.NoSpaceLeft,
                error.NotDir,
                error.OperationUnsupported,
                error.PathAlreadyExists,
                error.PermissionDenied,
                error.PipeBusy,
                error.ProcessFdQuotaExceeded,
                error.SymLinkLoop,
                error.SystemFdQuotaExceeded,
                error.SystemResources,
                error.Unexpected,
                error.UnrecognizedVolume,
                => null,
            };
            defer if (abs_path) |a| self.allocator.free(a);

            if (abs_path) |ap| {
                const new_hash = DependencyGraph.computeContentHash(text);
                const old_hash = self.dependency_graph.getContentHash(ap);

                if (old_hash) |existing| {
                    if (std.mem.eql(u8, &existing, &new_hash)) {
                        self.logDebug(.build, "[INCREMENTAL] SKIP rebuild for {s}: content hash unchanged ({x}...)", .{
                            ap,
                            new_hash[0..4].*,
                        });
                        return &[_]Diagnostics.PublishDiagnostics{};
                    }
                    self.logDebug(.build, "[INCREMENTAL] REBUILD {s}: content hash changed ({x}... -> {x}...)", .{
                        ap,
                        existing[0..4].*,
                        new_hash[0..4].*,
                    });
                } else {
                    self.logDebug(.build, "[INCREMENTAL] INITIAL build for {s}: no previous hash (new hash: {x}...)", .{
                        ap,
                        new_hash[0..4].*,
                    });
                }

                try self.dependency_graph.setContentHash(ap, new_hash);
            }
        }

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, self.std_io, env, uri, override_text, self.workspace_root);
        defer session.deinit();

        const absolute_path = session.absolute_path;

        if (override_text) |text| {
            env_handle.setDocumentContent(absolute_path, DependencyGraph.computeContentHash(text), self.documentHasReports(absolute_path, session.drained_reports)) catch |err| {
                self.logDebug(.build, "Failed to record document content: {s}", .{@errorName(err)});
            };
        }

        // Update dependency graph from successful build
        try self.updateDependencyGraph(env);

        var publish_list: std.ArrayList(Diagnostics.PublishDiagnostics) = .empty;
        errdefer {
            for (publish_list.items) |*set| set.deinit(self.allocator);
            publish_list.deinit(self.allocator);
        }
        if (session.drained_reports) |drained_reports| {
            // if the build succeeded, consider snapshotting the BuildEnv for completions
            if (self.shouldSnapshotBuild(env, session.absolute_path, drained_reports)) {
                try self.storeSnapshotEnv(env_handle, session.absolute_path);
            }
            for (drained_reports) |entry| {
                const mapped_path = if (entry.abs_path.len == 0) session.absolute_path else entry.abs_path;
                const module_uri = try uri_util.pathToUri(self.allocator, mapped_path);

                var diags: std.ArrayList(Diagnostics.Diagnostic) = .empty;
                errdefer {
                    for (diags.items) |diag| {
                        self.allocator.free(diag.message);
                    }
                    diags.deinit(self.allocator);
                }

                for (entry.reports) |*rep| {
                    const report = rep.*;
                    //we don't deinit here because BuildSession will free later

                    if (self.shouldSuppressReport(report)) continue;

                    const diag = try self.reportToDiagnostic(report);
                    try diags.append(self.allocator, diag);
                }
                //we also don't don't deinit the entries because buildSession will free them
                //self.allocator.free(entry.reports);

                try publish_list.append(self.allocator, .{
                    .uri = module_uri,
                    .diagnostics = try diags.toOwnedSlice(self.allocator),
                });
                diags.deinit(self.allocator);
            }

            if (publish_list.items.len == 0) {
                try publish_list.append(self.allocator, .{
                    .uri = try self.allocator.dupe(u8, uri),
                    .diagnostics = &.{},
                });
            }

            try self.finishDiagnosticPublishes(&publish_list, uri);
            return publish_list.toOwnedSlice(self.allocator);
        } else {
            // No reports drained, return a diagnostic showing the failure to get diagnostics
            try publish_list.append(self.allocator, .{
                .uri = try self.allocator.dupe(u8, uri),
                .diagnostics = try self.allocator.dupe(Diagnostics.Diagnostic, &.{
                    .{
                        .range = .{
                            .start = .{ .line = 0, .character = 0 },
                            .end = .{ .line = 0, .character = 1 },
                        },
                        .severity = 1,
                        .source = "roc",
                        .message = try std.fmt.allocPrint(self.allocator, "Failed to retrieve diagnostics for {s}", .{absolute_path}),
                    },
                }),
            });
            try self.finishDiagnosticPublishes(&publish_list, uri);
            return publish_list.toOwnedSlice(self.allocator);
        }
    }

    fn publishListContainsUri(publish_list: *const std.ArrayList(Diagnostics.PublishDiagnostics), uri: []const u8) bool {
        for (publish_list.items) |set| {
            if (std.mem.eql(u8, set.uri, uri)) return true;
        }
        return false;
    }

    /// Ensure the checked URI is published, clear stale URIs from prior publishes,
    /// and refresh the set of URIs that currently have non-empty diagnostics.
    fn finishDiagnosticPublishes(
        self: *SyntaxChecker,
        publish_list: *std.ArrayList(Diagnostics.PublishDiagnostics),
        checked_uri: []const u8,
    ) Allocator.Error!void {
        if (!publishListContainsUri(publish_list, checked_uri)) {
            try publish_list.append(self.allocator, .{
                .uri = try self.allocator.dupe(u8, checked_uri),
                .diagnostics = &.{},
            });
        }

        var stale_uris: std.ArrayList([]const u8) = .empty;
        defer {
            for (stale_uris.items) |stale_uri| self.allocator.free(stale_uri);
            stale_uris.deinit(self.allocator);
        }

        var it = self.published_diagnostic_uris.keyIterator();
        while (it.next()) |prev| {
            if (!publishListContainsUri(publish_list, prev.*)) {
                try stale_uris.append(self.allocator, try self.allocator.dupe(u8, prev.*));
            }
        }
        for (stale_uris.items) |stale_uri| {
            try publish_list.append(self.allocator, .{
                .uri = try self.allocator.dupe(u8, stale_uri),
                .diagnostics = &.{},
            });
        }

        self.clearPublishedDiagnosticUris();
        for (publish_list.items) |set| {
            if (set.diagnostics.len == 0) continue;
            const owned = try self.allocator.dupe(u8, set.uri);
            const gop = try self.published_diagnostic_uris.getOrPut(self.allocator, owned);
            if (gop.found_existing) {
                self.allocator.free(owned);
            } else {
                gop.key_ptr.* = owned;
            }
        }
    }

    /// Creates a fresh BuildEnv for a new build.
    /// The previous build_env is moved to previous_build_env for module lookups.
    fn createFreshBuildEnv(self: *SyntaxChecker) SyntaxBuildEnvError!*BuildEnvHandle {
        self.logDebug(.build, "createFreshBuildEnv: prev_build_env={any} build_env={any}", .{ self.previous_build_env != null, self.build_env != null });

        // Release the previous_build_env owner first.
        if (self.previous_build_env) |old_prev| {
            old_prev.release(owner_previous);
            self.previous_build_env = null;
        }

        // Move build_env to previous_build_env, transferring ownership tag.
        if (self.build_env) |current| {
            current.retain(owner_previous);
            current.release(owner_build);
            self.previous_build_env = current;
            self.build_env = null;
        }

        // Create a fresh BuildEnv. The LSP reuses one pre-published Builtin
        // across checks; each BuildEnv borrows it and never deinitializes it.
        const cwd = try std.Io.Dir.cwd().realPathFileAlloc(self.std_io, ".", self.allocator);
        errdefer self.allocator.free(cwd);
        const builtin_modules = try self.sharedBuiltinModules();
        var env = BuildEnv.initBorrowingBuiltinModules(
            self.allocator,
            .single_threaded,
            1,
            roc_target.RocTarget.detectNative(),
            cwd,
            self.std_io,
            builtin_modules,
        );
        errdefer env.deinit();
        env.compiler_version = build_options.compiler_version;
        env.setFinalizeExecutableArtifacts(false);

        if (self.cache_config.enabled) {
            const cache_manager = try self.allocator.create(CacheManager);
            cache_manager.* = CacheManager.init(self.allocator, self.cache_config, CoreCtx.default(self.allocator, self.allocator, self.std_io));
            env.setCacheManager(cache_manager);
        }

        const debug_handles = self.debug.build or self.debug.syntax or self.debug.server;
        const handle = try BuildEnvHandle.create(self.allocator, env, cwd, owner_build, debug_handles);
        self.build_env = handle;
        return handle;
    }

    fn sharedBuiltinModules(self: *SyntaxChecker) (Allocator.Error || eval.BuiltinModules.InitError)!*eval.BuiltinModules {
        if (self.builtin_modules) |builtin_modules| return builtin_modules;

        const builtin_modules = try self.allocator.create(eval.BuiltinModules);
        errdefer self.allocator.destroy(builtin_modules);

        builtin_modules.* = try eval.BuiltinModules.init(self.allocator);
        self.builtin_modules = builtin_modules;
        return builtin_modules;
    }

    fn shouldSnapshotBuild(self: *SyntaxChecker, env: *BuildEnv, absolute_path: []const u8, drained: []BuildEnv.DrainedModuleReports) bool {
        // Check if module was processed - if not, don't snapshot
        if (self.getModuleEnvByPathInEnv(env, absolute_path) == null) {
            return false;
        }

        // Check for any error-level reports for this file - warnings/info are OK to snapshot
        for (drained) |entry| {
            if (std.mem.eql(u8, entry.abs_path, absolute_path)) {
                for (entry.reports) |report| {
                    switch (report.severity) {
                        .runtime_error, .fatal => return false,
                        .warning => {},
                    }
                }
            }
        }

        // Module processed with no reports → snapshot
        return true;
    }

    fn storeSnapshotEnv(self: *SyntaxChecker, env_handle: *BuildEnvHandle, absolute_path: []const u8) std.mem.Allocator.Error!void {
        self.logDebug(.completion, "storeSnapshotEnv: path={s}", .{absolute_path});
        if (self.snapshot_envs.fetchRemove(absolute_path)) |removed| {
            self.logDebug(.completion, "storeSnapshotEnv: replacing existing snapshot", .{});
            removed.value.release(owner_snapshot);
            self.allocator.free(removed.key);
        }

        const owned_path = try self.allocator.dupe(u8, absolute_path);
        self.snapshot_envs.put(self.allocator, owned_path, env_handle) catch |err| {
            self.allocator.free(owned_path);
            return err;
        };
        env_handle.retain(owner_snapshot);
        self.logDebug(.completion, "storeSnapshotEnv: stored snapshot count={d}", .{self.snapshot_envs.count()});
    }

    fn clearSnapshots(self: *SyntaxChecker) void {
        // Collect all handles and keys before clearing the map so we can
        // release snapshot ownership without mutating the map mid-iteration.
        const count = self.snapshot_envs.count();
        var envs: std.ArrayListUnmanaged(*BuildEnvHandle) = .empty;
        defer envs.deinit(self.allocator);
        var keys: std.ArrayListUnmanaged([]const u8) = .empty;
        defer keys.deinit(self.allocator);

        // Pre-reserve so the appends below cannot fail. If reserving fails we
        // fall back to releasing each entry as we iterate; this still avoids
        // leaking the handle refcount and key on OOM.
        const reserved = blk: {
            envs.ensureTotalCapacity(self.allocator, count) catch break :blk false;
            keys.ensureTotalCapacity(self.allocator, count) catch break :blk false;
            break :blk true;
        };

        if (!reserved) {
            // OOM path: release/free directly while iterating, then clear.
            var it = self.snapshot_envs.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.release(owner_snapshot);
                self.allocator.free(entry.key_ptr.*);
            }
            self.snapshot_envs.clearRetainingCapacity();
            return;
        }

        var it = self.snapshot_envs.iterator();
        while (it.next()) |entry| {
            envs.appendAssumeCapacity(entry.value_ptr.*);
            keys.appendAssumeCapacity(entry.key_ptr.*);
        }

        // Clear the map FIRST so snapshot ownership is only represented by handles.
        self.snapshot_envs.clearRetainingCapacity();

        // Now release all handles (with empty snapshot_envs map)
        for (envs.items) |handle| {
            handle.release(owner_snapshot);
        }

        // Free all keys
        for (keys.items) |key| {
            self.allocator.free(key);
        }
    }

    /// Get the BuildEnv that should be used for module lookups (semantic tokens, etc.).
    /// Prefers the current build_env if it has modules, otherwise falls back to previous_build_env.
    pub fn getModuleLookupEnv(self: *SyntaxChecker) ?*BuildEnv {
        // Prefer current build_env if it exists and has modules
        if (self.build_env) |handle| {
            const env = handle.envPtr();
            if (env.hasCompiledModules()) {
                return env;
            }
        }
        // Fall back to previous_build_env
        if (self.previous_build_env) |handle| return handle.envPtr();
        return null;
    }

    /// Get the cached snapshot BuildEnv for completions.
    /// Returns the first available snapshot env if any exist.
    pub fn getSnapshotEnv(self: *SyntaxChecker) ?*BuildEnv {
        var it = self.snapshot_envs.iterator();
        if (it.next()) |entry| {
            return entry.value_ptr.*.envPtr();
        }
        return null;
    }

    /// Look up a ModuleEnv by its file path from the cached BuildEnv.
    /// Returns null if no matching module is found.
    pub fn getModuleEnvByPath(self: *SyntaxChecker, path: []const u8) ?*ModuleEnv {
        const env = self.getModuleLookupEnv() orelse return null;
        return self.getModuleEnvByPathInEnv(env, path);
    }

    /// Look up a ModuleEnv by its file path from a specific BuildEnv.
    fn getModuleEnvByPathInEnv(_: *SyntaxChecker, env: *BuildEnv, path: []const u8) ?*ModuleEnv {
        const module_state = env.findModuleByPath(path) orelse return null;
        return module_state.moduleEnv();
    }

    /// Get all imported ModuleEnvs for a given module.
    /// Returns a slice of ModuleEnv pointers for the module's imports.
    /// Caller must free the returned slice.
    pub fn getImportedModuleEnvs(self: *SyntaxChecker, module_path: []const u8) Allocator.Error!?[]*ModuleEnv {
        const env = self.getModuleLookupEnv() orelse return null;

        // First, find the module and its coordinator package.
        var target_pkg: ?*compile.coordinator.PackageState = null;
        var target_module_imports: ?[]const compile.coordinator.LocalImportEdge = null;

        const coord = env.coordinator orelse return null;
        var pkg_it = coord.packages.iterator();
        outer: while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*module_state| {
                if (std.mem.eql(u8, module_state.path, module_path)) {
                    target_pkg = pkg;
                    target_module_imports = module_state.imports.items;
                    break :outer;
                }
            }
        }

        const pkg = target_pkg orelse return null;
        const imports = target_module_imports orelse return null;

        // Collect ModuleEnvs for all imports
        var imported_envs: std.ArrayListUnmanaged(*ModuleEnv) = .empty;
        errdefer imported_envs.deinit(self.allocator);

        // Local imports (within same package)
        for (imports) |edge| {
            if (edge.module_id < pkg.modules.items.len) {
                const imported_module = &pkg.modules.items[edge.module_id];
                if (imported_module.moduleEnv()) |imp_env| {
                    try imported_envs.append(self.allocator, imp_env);
                }
            }
        }

        // TODO: Handle external_imports (cross-package) when needed

        return try imported_envs.toOwnedSlice(self.allocator);
    }

    /// Update the dependency graph from a successful build.
    fn updateDependencyGraph(self: *SyntaxChecker, env: *BuildEnv) Allocator.Error!void {
        self.logDebug(.build, "[DEPS] Updating dependency graph...", .{});

        // Clear only relationships, preserving content/exports hashes for incremental detection
        self.dependency_graph.clearRelationships();

        var total_modules: usize = 0;
        var exports_computed: usize = 0;

        const coord = env.coordinator orelse return;
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg_name = entry.key_ptr.*;
            const pkg = entry.value_ptr.*;

            self.logDebug(.build, "[DEPS] Processing package '{s}' with {d} modules", .{ pkg_name, pkg.modules.items.len });

            try self.dependency_graph.buildFromPackageState(pkg);

            // Compute and store exports hash for each module with a valid ModuleEnv
            for (pkg.modules.items) |*module_state| {
                total_modules += 1;

                if (module_state.moduleEnv()) |module_env| {
                    const new_exports_hash = try DependencyGraph.computeExportsHash(self.allocator, module_env);

                    // Check if exports changed (for future smart invalidation)
                    const old_exports_hash = self.dependency_graph.getExportsHash(module_state.path);
                    if (old_exports_hash) |existing| {
                        if (!std.mem.eql(u8, &existing, &new_exports_hash)) {
                            self.logDebug(.build, "[DEPS] EXPORTS CHANGED for {s}: {x}... -> {x}...", .{
                                module_state.path,
                                existing[0..4].*,
                                new_exports_hash[0..4].*,
                            });
                        }
                    }

                    self.dependency_graph.setExportsHash(module_state.path, new_exports_hash);
                    exports_computed += 1;

                    // Log module with its dependencies
                    if (self.dependency_graph.getModule(module_state.path)) |node| {
                        if (node.imports.items.len > 0) {
                            self.logDebug(.build, "[DEPS]   {s} imports {d} modules", .{ module_state.name, node.imports.items.len });
                        }
                    }
                }
            }
        }

        self.logDebug(.build, "[DEPS] Graph complete: {d} modules tracked, {d} exports hashes computed", .{
            self.dependency_graph.count(),
            exports_computed,
        });
    }

    fn reportToDiagnostic(self: *SyntaxChecker, rep: reporting.Report) (Allocator.Error || error{WriteFailed})!Diagnostics.Diagnostic {
        const range = self.rangeFromReport(rep);
        const severity: u32 = switch (rep.severity) {
            .warning => 2,
            .runtime_error, .fatal => 1,
        };

        var writer: std.Io.Writer.Allocating = .init(self.allocator);
        defer writer.deinit();
        try reporting.renderReportToLsp(&rep, &writer.writer, reporting.ReportingConfig.initLsp());
        const message = writer.toOwnedSlice() catch return error.OutOfMemory;

        self.logDebug(.syntax, "report: {s}", .{rep.title});

        return .{
            .range = range,
            .severity = severity,
            .source = "roc",
            .message = message,
        };
    }

    fn rangeFromReport(_: *SyntaxChecker, rep: reporting.Report) Diagnostics.Range {
        var start = Diagnostics.Position{ .line = 0, .character = 0 };
        var end = Diagnostics.Position{ .line = 0, .character = 0 };

        var idx: usize = 0;
        while (idx < rep.document.elementCount()) : (idx += 1) {
            const maybe_element = rep.document.getElement(idx) orelse break;
            switch (maybe_element) {
                .source_code_region => |region| {
                    start = .{ .line = saturatingMinusOne(region.start_line), .character = saturatingMinusOne(region.start_column) };
                    end = .{ .line = saturatingMinusOne(region.end_line), .character = saturatingMinusOne(region.end_column) };
                    break;
                },
                .source_code_with_underlines => |region| {
                    start = .{ .line = saturatingMinusOne(region.display_region.start_line), .character = saturatingMinusOne(region.display_region.start_column) };
                    end = .{ .line = saturatingMinusOne(region.display_region.end_line), .character = saturatingMinusOne(region.display_region.end_column) };
                    break;
                },
                .source_code_multi_region => |multi| {
                    if (multi.regions.len > 0) {
                        const region = multi.regions[0];
                        start = .{ .line = saturatingMinusOne(region.start_line), .character = saturatingMinusOne(region.start_column) };
                        end = .{ .line = saturatingMinusOne(region.end_line), .character = saturatingMinusOne(region.end_column) };
                        break;
                    }
                },
                .line_break,
                .indent,
                .space,
                .horizontal_rule,
                .annotation_start,
                .annotation_end,
                .text,
                .annotated,
                .raw,
                .reflowing_text,
                .link,
                .vertical_stack,
                .horizontal_concat,
                .source_location,
                => {},
            }
        }

        return .{ .start = start, .end = end };
    }

    fn saturatingMinusOne(value: u32) u32 {
        return if (value == 0) 0 else value - 1;
    }

    fn logDebug(self: *SyntaxChecker, kind: enum { build, syntax, completion }, comptime fmt: []const u8, args: anytype) void {
        const enabled = switch (kind) {
            .build => self.debug.build,
            .syntax => self.debug.syntax,
            .completion => self.debug.completion,
        };
        if (!enabled) return;
        var log_file = self.log_file orelse return;
        var buffer: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buffer, fmt, args) catch return;
        log_file.writeStreamingAll(self.std_io, msg) catch return;
        log_file.writeStreamingAll(self.std_io, "\n") catch {};
        log_file.sync(self.std_io) catch {};
    }

    /// Temporary suppression to avoid noisy name-not-in-scope diagnostics from BuildEnv.
    fn shouldSuppressReport(_: *SyntaxChecker, rep: reporting.Report) bool {
        if (!std.mem.startsWith(u8, rep.title, "Name Not In Scope")) return false;

        const disallowed = [_][]const u8{ "Stderr", "Stdin", "Stdout" };
        return reportContainsAny(rep, &disallowed);
    }

    fn reportContainsAny(rep: reporting.Report, needles: []const []const u8) bool {
        var idx: usize = 0;
        while (rep.document.getElement(idx)) |element| : (idx += 1) {
            if (elementContainsAny(element, needles)) return true;
        }
        return false;
    }

    fn elementContainsAny(element: reporting.DocumentElement, needles: []const []const u8) bool {
        switch (element) {
            .text => |t| return textHasAny(t, needles),
            .annotated => |a| return textHasAny(a.content, needles),
            .raw => |r| return textHasAny(r, needles),
            .reflowing_text => |t| return textHasAny(t, needles),
            .link => |l| return textHasAny(l, needles),
            .vertical_stack => |stack| {
                for (stack) |el| if (elementContainsAny(el, needles)) return true;
            },
            .horizontal_concat => |concat| {
                for (concat) |el| if (elementContainsAny(el, needles)) return true;
            },
            .source_code_region => |region| return textHasAny(region.line_text, needles),
            .source_code_multi_region => |multi| return textHasAny(multi.source, needles),
            .source_code_with_underlines => |with_underlines| return textHasAny(with_underlines.display_region.line_text, needles),
            .source_location => {},
            .line_break,
            .indent,
            .space,
            .horizontal_rule,
            .annotation_start,
            .annotation_end,
            => {},
        }
        return false;
    }

    fn textHasAny(text: []const u8, needles: []const []const u8) bool {
        for (needles) |needle| {
            if (std.mem.find(u8, text, needle) != null) return true;
        }
        return false;
    }

    /// Range in LSP coordinates
    // LspRange moved to cir_queries.zig
    pub const LspRange = cir_queries.LspRange;

    /// Result of a hover query containing type information
    pub const HoverResult = struct {
        type_str: []u8,
        range: ?LspRange,
    };

    /// Result of a definition query containing location information
    pub const DefinitionResult = struct {
        uri: []const u8,
        range: LspRange,
        origin_selection_range: ?LspRange = null,

        pub fn deinit(self: DefinitionResult, allocator: std.mem.Allocator) void {
            allocator.free(self.uri);
        }
    };

    /// Returns true when a byte can be part of a Roc identifier token used for
    /// hover symbol fallback resolution.
    fn isSymbolByte(b: u8) bool {
        return std.ascii.isAlphanumeric(b) or b == '_' or b == '.';
    }

    /// Extract the symbol token under (or immediately before) an offset.
    ///
    /// This is a resilient fallback for hover when CIR lookup queries miss the
    /// exact identifier region (for example, when the cursor lands on a nearby
    /// delimiter).
    fn symbolAtOffset(source: []const u8, offset: u32) ?[]const u8 {
        if (source.len == 0) return null;

        var i: usize = @intCast(@min(offset, @as(u32, @intCast(source.len))));
        if (i >= source.len or !isSymbolByte(source[i])) {
            if (i == 0 or !isSymbolByte(source[i - 1])) return null;
            i -= 1;
        }

        var start = i;
        while (start > 0 and isSymbolByte(source[start - 1])) : (start -= 1) {}

        var end = i + 1;
        while (end < source.len and isSymbolByte(source[end])) : (end += 1) {}

        if (end <= start) return null;
        return source[start..end];
    }

    /// Get type information at a specific position in a document.
    /// Returns the type as a formatted string, or null if no type info is available.
    pub fn getTypeAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
    ) QueryError!?HoverResult {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        const env = build.env;

        self.logDebug(.build, "hover: document {s} reused={}", .{ build.absolute_path, build.reused });

        if (!build.build_succeeded) {
            self.logDebug(.build, "hover: build unavailable for {s}", .{build.absolute_path});
            return null;
        }

        // Get module environment
        const module_env = build.getModuleEnv() orelse return null;

        // Convert LSP position (0-based line/col) to byte offset
        // LSP uses 0-based line and UTF-16 code units for character
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        // Find the expression at this position
        const result = cir_queries.findTypeAtOffset(module_env, target_offset) orelse return null;

        // Prefer lookup expression semantics when hovering over identifiers in
        // calls (e.g. `multiply(…)`): callers expect the callee's function type
        // and docs, not the enclosing call expression's return type.
        var lookup_result_opt = cir_queries.findLookupAtOffset(module_env, target_offset);
        if (lookup_result_opt == null and result.region.start.offset != target_offset) {
            lookup_result_opt = cir_queries.findLookupAtOffset(module_env, result.region.start.offset);
        }

        var hover_type_var = if (lookup_result_opt) |lookup_result|
            lookup_result.typeVar()
        else
            result.type_var;

        // Optional textual override for hover type rendering. When we can
        // resolve an explicit annotation for a symbol, prefer that exact text.
        var hover_type_text_opt: ?[]const u8 = null;

        if (lookup_result_opt) |lookup_result| {
            switch (lookup_result) {
                .expr => |lookup_expr_idx| {
                    const lookup_expr = module_env.store.getExpr(lookup_expr_idx);
                    if (lookup_expr == .e_method_call) {
                        const method_call = lookup_expr.e_method_call;
                        const receiver_type_var = ModuleEnv.varFrom(method_call.receiver);
                        if (resolveMethodOwnerForLookup(module_env, receiver_type_var)) |method_owner| {
                            if (findMethodQualifiedIdent(module_env, method_owner.owner, method_call.method_name)) |qualified_ident| {
                                if (findTypeForQualifiedIdent(module_env, qualified_ident)) |method_type_var| {
                                    hover_type_var = method_type_var;
                                }
                            }
                        }
                    } else if (lookup_expr == .e_dispatch_call) {
                        const method_call = lookup_expr.e_dispatch_call;
                        const receiver_type_var = ModuleEnv.varFrom(method_call.receiver);
                        if (resolveMethodOwnerForLookup(module_env, receiver_type_var)) |method_owner| {
                            if (findMethodQualifiedIdent(module_env, method_owner.owner, method_call.method_name)) |qualified_ident| {
                                if (findTypeForQualifiedIdent(module_env, qualified_ident)) |method_type_var| {
                                    hover_type_var = method_type_var;
                                }
                            }
                        }
                    }
                },
                .field_access => {},
            }
        }

        // Format the type as a string
        var type_writer = try module_env.initTypeWriter();
        defer type_writer.deinit();

        try type_writer.write(hover_type_var, .one_line);
        const type_str = type_writer.get();

        // Extract documentation for the definition/pattern at this location.
        // When we already have a lookup expression, resolve directly to avoid
        // region/offset ambiguity around delimiters.
        var documentation = if (lookup_result_opt) |lookup_result|
            try self.resolveDocForLookup(env, module_env, build.absolute_path, lookup_result)
        else
            try self.findDocumentationForRegion(env, module_env, build.absolute_path, result.region, target_offset);

        // Final fallback: reuse definition-resolution to recover the symbol at
        // call sites where direct lookup queries can miss the identifier region.
        // This keeps hover aligned with go-to-definition behavior.
        if (documentation == null) {
            var def_oom: ?Allocator.Error = null;
            const def_loc_opt = self.findDefinitionAtOffset(build.env, module_env, build.absolute_path, target_offset, uri, &def_oom);
            if (def_oom) |e| return e;
            if (def_loc_opt) |def_loc| {
                defer def_loc.deinit(self.allocator);
                if (std.mem.eql(u8, def_loc.uri, uri)) {
                    if (pos.positionToOffset(module_env, def_loc.range.start_line, def_loc.range.start_col)) |def_offset| {
                        if (cir_queries.findPatternAtOffset(module_env, def_offset)) |pattern_idx| {
                            hover_type_var = ModuleEnv.varFrom(pattern_idx);
                            documentation = try doc_comments.extractDocCommentBefore(
                                self.allocator,
                                module_env.common.source,
                                module_env.store.getPatternRegion(pattern_idx).start.offset,
                            );
                        }
                    }
                }
            }
        }

        // Text-token fallback: resolve symbol directly by source token under
        // the cursor. This recovers hover on call identifiers even when CIR
        // lookup matching is ambiguous for that exact offset.
        if (symbolAtOffset(module_env.common.source, target_offset)) |symbol| {
            if (module_lookup.findDefinitionByUnqualifiedName(module_env, symbol)) |def_info| {
                hover_type_var = if (def_info.expr_idx) |expr_idx|
                    ModuleEnv.varFrom(expr_idx)
                else
                    ModuleEnv.varFrom(def_info.pattern_idx);

                if (module_lookup.findDefOwningPattern(module_env, def_info.pattern_idx)) |def| {
                    if (def.annotation) |anno_idx| {
                        const anno = module_env.store.getAnnotation(anno_idx);
                        const anno_region = module_env.store.getTypeAnnoRegion(anno.anno);
                        hover_type_text_opt = module_env.getSource(anno_region);
                    }

                    const extracted = try doc_comments.extractDocForDef(
                        self.allocator,
                        module_env.common.source,
                        &module_env.store,
                        def,
                    );
                    if (extracted != null) {
                        if (documentation) |doc| self.allocator.free(doc);
                        documentation = extracted;
                    }
                } else if (module_lookup.findStatementOwningPattern(module_env, def_info.pattern_idx)) |stmt_owner| {
                    const extracted = try doc_comments.extractDocForStatement(
                        self.allocator,
                        module_env.common.source,
                        &module_env.store,
                        stmt_owner.stmt,
                        stmt_owner.idx,
                    );
                    if (extracted != null) {
                        if (documentation) |doc| self.allocator.free(doc);
                        documentation = extracted;
                    }
                } else {
                    const extracted = try doc_comments.extractDocCommentBefore(
                        self.allocator,
                        module_env.common.source,
                        module_env.store.getPatternRegion(def_info.pattern_idx).start.offset,
                    );
                    if (extracted != null) {
                        if (documentation) |doc| self.allocator.free(doc);
                        documentation = extracted;
                    }
                }
            }
        }
        defer if (documentation) |doc| self.allocator.free(doc);

        // Create markdown-formatted output with type and optional documentation
        const type_text = hover_type_text_opt orelse type_str;
        const markdown = if (documentation) |doc|
            try std.fmt.allocPrint(self.allocator, "{s}\n\n```roc\n{s}\n```", .{ doc, type_text })
        else
            try std.fmt.allocPrint(self.allocator, "```roc\n{s}\n```", .{type_text});

        // Convert the region back to LSP positions
        const range = cir_queries.regionToRange(module_env, result.region);

        return HoverResult{
            .type_str = markdown,
            .range = range,
        };
    }

    /// Find documentation comments for the symbol at the given region/offset.
    /// First checks if the cursor is on a lookup expression and resolves it to
    /// the actual definition. Otherwise searches defs and statements by region.
    fn findDocumentationForRegion(
        self: *SyntaxChecker,
        env: *BuildEnv,
        module_env: *ModuleEnv,
        doc_path: []const u8,
        region: Region,
        target_offset: u32,
    ) Allocator.Error!?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;

        // First, check if this is a lookup expression (e.g., a function call)
        // If so, resolve it to the definition and extract docs from there
        if (cir_queries.findLookupAtOffset(module_env, target_offset)) |lookup_result| {
            if (try self.resolveDocForLookup(env, module_env, doc_path, lookup_result)) |doc| return doc;
        }

        // Hover positions can land on delimiters around the symbol (e.g. `(` in
        // `foo(...)`) depending on UTF-16 cursor conversion. As a fallback, try
        // the start of the selected type region as an anchor for lookup-based
        // documentation resolution.
        if (region.start.offset != target_offset) {
            if (cir_queries.findLookupAtOffset(module_env, region.start.offset)) |lookup_result| {
                if (try self.resolveDocForLookup(env, module_env, doc_path, lookup_result)) |doc| return doc;
            }
        }

        // Not a lookup, or lookup resolution failed - fall back to region-based search

        // Check top-level definitions
        const defs_slice = store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = store.getDef(def_idx);
            const pattern_region = store.getPatternRegion(def.pattern);

            if (cir_queries.regionContainsOffset(pattern_region, region.start.offset) or
                pattern_region.start.offset == region.start.offset)
            {
                return try doc_comments.extractDocForDef(self.allocator, source, store, def);
            }

            // Also check if the expression region matches (for hovering over expressions)
            const expr_region = store.getExprRegion(def.expr);
            if (cir_queries.regionContainsOffset(expr_region, region.start.offset)) {
                return try doc_comments.extractDocForDef(self.allocator, source, store, def);
            }
        }

        // Check statements
        const statements_slice = store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = store.getStatement(stmt_idx);
            const stmt_region = store.getStatementRegion(stmt_idx);

            if (cir_queries.regionContainsOffset(stmt_region, region.start.offset)) {
                return try doc_comments.extractDocForStatement(self.allocator, source, store, stmt, stmt_idx);
            }
        }

        return null;
    }

    /// Resolve documentation for a local lookup, external lookup, or attached method dispatch.
    fn resolveDocForLookup(
        self: *SyntaxChecker,
        env: *BuildEnv,
        module_env: *ModuleEnv,
        doc_path: []const u8,
        lookup_result: cir_queries.LookupResult,
    ) Allocator.Error!?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;
        const expr_idx = switch (lookup_result) {
            .expr => |idx| idx,
            .field_access => return null,
        };
        const expr = store.getExpr(expr_idx);
        const importing_pkg = env.findPackageForModulePath(doc_path);

        const expr_tag = std.meta.activeTag(expr);
        if (expr_tag == .e_lookup_local) {
            const lookup = expr.e_lookup_local;
            // Local lookup - resolve to the owning def or statement
            if (module_lookup.findDefOwningPattern(module_env, lookup.pattern_idx)) |def| {
                return try doc_comments.extractDocForDef(self.allocator, source, store, def);
            }
            if (module_lookup.findStatementOwningPattern(module_env, lookup.pattern_idx)) |result| {
                return try doc_comments.extractDocForStatement(self.allocator, source, store, result.stmt, result.idx);
            }

            // Some local bindings are nested inside expressions (e.g. block
            // locals) and are not owned by top-level defs/statements. Fall
            // back to doc extraction directly from the bound pattern region.
            return try doc_comments.extractDocCommentBefore(
                self.allocator,
                source,
                store.getPatternRegion(lookup.pattern_idx).start.offset,
            );
        }
        if (expr_tag == .e_lookup_external) {
            const lookup = expr.e_lookup_external;
            // External lookup - parse "Module.function" and find docs in that module
            const region_text = module_env.getSource(lookup.region);
            if (std.mem.find(u8, region_text, ".")) |dot_pos| {
                const module_name = region_text[0..dot_pos];
                const function_name = region_text[dot_pos + 1 ..];

                if (findExternalModuleEnv(env, importing_pkg, module_name)) |external_env| {
                    return try findDocInModule(self.allocator, external_env, function_name);
                }
            }
        }
        if (expr_tag == .e_dispatch_call) {
            const method_call = expr.e_dispatch_call;
            const method_name = module_env.getIdentText(method_call.method_name);
            const receiver_type_var = ModuleEnv.varFrom(method_call.receiver);
            if (resolveMethodOwnerForLookup(module_env, receiver_type_var)) |method_owner| {
                if (try findMethodDocForOwnerAndName(self.allocator, module_env, method_owner.owner, method_name)) |local_doc| {
                    return local_doc;
                }

                const type_name = module_env.getIdentText(method_owner.type_ident);
                if (findExternalModuleEnvForMethodOwner(env, importing_pkg, method_owner, type_name)) |external_env| {
                    const qualified_name = try std.fmt.allocPrint(
                        self.allocator,
                        "{s}.{s}",
                        .{ type_name, method_name },
                    );
                    defer self.allocator.free(qualified_name);
                    return try findDocInModule(self.allocator, external_env, qualified_name);
                }
            }
        }
        return null;
    }

    /// Resolve a source declaration owner for method lookup from a receiver type var.
    ///
    /// This follows aliases/nominal wrappers so hover can map `value.method()` to
    /// the `(owner statement, method_ident)` entries in `method_idents`.
    fn resolveMethodOwnerForLookup(module_env: *ModuleEnv, type_var: types.Var) ?MethodOwnerLookup {
        const resolved = module_env.types.resolveVar(type_var);
        const content = resolved.desc.content;
        // Aliases carry a nominal ident that can participate in method_idents lookup.
        if (std.meta.activeTag(content) == .alias) {
            const source_decl = content.alias.source_decl.toOptional() orelse return null;
            return .{
                .owner = @enumFromInt(source_decl),
                .type_ident = content.alias.ident.ident_idx,
                .builtin_origin = content.alias.source_decl.originIsBuiltin(),
            };
        }
        if (std.meta.activeTag(content) != .structure or std.meta.activeTag(content.structure) != .nominal_type) return null;
        const nominal = content.structure.nominal_type;
        const source_decl = nominal.sourceDeclOptional() orelse return null;
        return .{
            .owner = @enumFromInt(source_decl),
            .type_ident = nominal.ident.ident_idx,
            .builtin_origin = nominal.originIsBuiltin(),
        };
    }

    fn findMethodQualifiedIdent(
        module_env: *ModuleEnv,
        owner: CIR.Statement.Idx,
        method_ident: base.Ident.Idx,
    ) ?base.Ident.Idx {
        return module_env.lookupMethodIdentForOwnerConst(owner, method_ident);
    }

    fn findTypeForQualifiedIdent(module_env: *ModuleEnv, qualified_ident: base.Ident.Idx) ?types.Var {
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const ident_idx = module_lookup.extractIdentFromPattern(&module_env.store, def.pattern) orelse continue;

            if (ident_idx.eql(qualified_ident)) {
                return ModuleEnv.varFrom(def.pattern);
            }
        }

        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const pattern_idx = module_lookup.getDeclarationPattern(stmt) orelse continue;

            const ident_idx = module_lookup.extractIdentFromPattern(&module_env.store, pattern_idx) orelse continue;

            if (ident_idx.eql(qualified_ident)) {
                return ModuleEnv.varFrom(pattern_idx);
            }
        }

        return null;
    }

    /// Find local method documentation by `(owner statement, method_name)`.
    fn findMethodDocForOwnerAndName(
        allocator: Allocator,
        module_env: *ModuleEnv,
        owner: CIR.Statement.Idx,
        method_name: []const u8,
    ) Allocator.Error!?[]const u8 {
        const entries = module_env.method_idents.entries.items;
        for (entries) |entry| {
            if (entry.key.owner != owner) continue;

            const entry_method_name = module_env.getIdentText(entry.key.methodIdent());
            if (!std.mem.eql(u8, entry_method_name, method_name)) continue;

            return try findDocForQualifiedIdent(allocator, module_env, entry.value);
        }

        return null;
    }

    fn findExternalModuleEnvForMethodOwner(
        env: *BuildEnv,
        importing_pkg: ?*compile.coordinator.PackageState,
        method_owner: MethodOwnerLookup,
        module_name: []const u8,
    ) ?*ModuleEnv {
        if (method_owner.builtin_origin) {
            const base_name = if (std.mem.findLast(u8, module_name, ".")) |dot_pos|
                module_name[dot_pos + 1 ..]
            else
                module_name;
            if (completion_builtins.isBuiltinType(base_name)) {
                return env.builtin_modules.builtin_module.env;
            }
        }
        if (env.findModuleByQualifiedNameInPackage(importing_pkg, module_name)) |mod_state| {
            return mod_state.moduleEnv();
        }
        return null;
    }

    /// Find a module environment by name (handles builtins and regular modules).
    fn findExternalModuleEnv(
        env: *BuildEnv,
        importing_pkg: ?*compile.coordinator.PackageState,
        module_name: []const u8,
    ) ?*ModuleEnv {
        const base_name = if (std.mem.findLast(u8, module_name, ".")) |dot_pos|
            module_name[dot_pos + 1 ..]
        else
            module_name;

        // Check builtins first
        if (completion_builtins.isBuiltinType(base_name)) {
            return env.builtin_modules.builtin_module.env;
        }

        if (env.findModuleByQualifiedNameInPackage(importing_pkg, module_name)) |mod_state| {
            return mod_state.moduleEnv();
        }
        return null;
    }

    /// Find documentation for a definition by name in a module.
    /// Uses module_lookup infrastructure for the search, with qualified-name fallback.
    fn findDocInModule(allocator: Allocator, module_env: *ModuleEnv, name: []const u8) Allocator.Error!?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;

        // Use module_lookup to find by exact or unqualified name
        if (module_lookup.findDefinitionByUnqualifiedName(module_env, name)) |def_info| {
            // Try to find the full Def for annotation-aware offset
            if (module_lookup.findDefOwningPattern(module_env, def_info.pattern_idx)) |def| {
                return try doc_comments.extractDocForDef(allocator, source, store, def);
            }
            // Fall back to statement-based extraction
            if (module_lookup.findStatementOwningPattern(module_env, def_info.pattern_idx)) |result| {
                return try doc_comments.extractDocForStatement(allocator, source, store, result.stmt, result.idx);
            }
            // Last resort: use pattern region directly
            return try doc_comments.extractDocCommentBefore(
                allocator,
                source,
                store.getPatternRegion(def_info.pattern_idx).start.offset,
            );
        }
        return null;
    }

    /// Find documentation for a specific qualified identifier in a module.
    fn findDocForQualifiedIdent(allocator: Allocator, module_env: *ModuleEnv, qualified_ident: base.Ident.Idx) Allocator.Error!?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;

        // Search defs first.
        const defs_slice = store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = store.getDef(def_idx);
            const ident_idx = module_lookup.extractIdentFromPattern(store, def.pattern) orelse continue;

            if (ident_idx.eql(qualified_ident)) {
                return try doc_comments.extractDocForDef(allocator, source, store, def);
            }
        }

        // Fall back to statements.
        const statements_slice = store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = store.getStatement(stmt_idx);
            if (std.meta.activeTag(stmt) != .s_decl) continue;
            const pattern_idx = stmt.s_decl.pattern;

            const ident_idx = module_lookup.extractIdentFromPattern(store, pattern_idx) orelse continue;

            if (ident_idx.eql(qualified_ident)) {
                return try doc_comments.extractDocForStatement(allocator, source, store, stmt, stmt_idx);
            }
        }

        return null;
    }

    /// Get definition location at a specific position in a document.
    /// Returns the location where the symbol is defined, or null if not found.
    pub fn getDefinitionAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
    ) QueryError!?DefinitionResult {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        if (!build.build_succeeded) {
            self.logDebug(.build, "definition: build unavailable for {s}", .{build.absolute_path});
            return null;
        }

        // Get module environment
        const module_env = build.getModuleEnv() orelse return null;

        // Convert LSP position to byte offset
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        // Find the definition at this position
        var oom: ?Allocator.Error = null;
        const result = self.findDefinitionAtOffset(build.env, module_env, build.absolute_path, target_offset, uri, &oom) orelse {
            if (oom) |e| return e;
            return null;
        };

        return result;
    }

    // positionToOffset moved to position.zig

    // regionToRange moved to cir_queries module

    /// Find the definition location for the expression at the given byte offset.
    /// Looks for lookups (e_lookup_local, e_lookup_external) and returns the definition location.
    fn findDefinitionAtOffset(self: *SyntaxChecker, build_env: *BuildEnv, module_env: *ModuleEnv, doc_path: []const u8, target_offset: u32, current_uri: []const u8, oom: *?Allocator.Error) ?DefinitionResult {
        var best_lookup: ?cir_queries.LookupResult = null;

        // Iterate through all definitions
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);

            // Check type annotation on this definition
            if (def.annotation) |anno_idx| {
                const annotation = module_env.store.getAnnotation(anno_idx);
                if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, annotation.anno, target_offset, oom)) |result| {
                    // If URI is empty, it's a local type - use current file
                    if (result.uri.len == 0) {
                        const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                            oom.* = err;
                            return null;
                        };
                        return DefinitionResult{
                            .uri = uri_copy,
                            .range = result.range,
                        };
                    }
                    return result;
                }
            }

            const expr_idx = def.expr;
            const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
            const expr_region = module_env.store.getRegionAt(expr_node_idx);

            if (cir_queries.regionContainsOffset(expr_region, target_offset)) {
                // First check for type annotations in nested blocks
                if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, expr_idx, target_offset, current_uri, oom)) |result| {
                    return result;
                }
                // Then search for lookup expressions
                if (cir_queries.findLookupAtOffset(module_env, target_offset)) |found| {
                    best_lookup = found;
                }
            }
        }

        const file_deps = module_env.file_dependencies.items.items;
        for (file_deps) |dep| {
            if (cir_queries.regionContainsOffset(dep.region(), target_offset)) {
                const path_text = module_env.getString(dep.relative_path);
                const doc_dir = std.fs.path.dirname(doc_path) orelse "";
                const target_path = std.fs.path.resolve(self.allocator, &.{ doc_dir, path_text }) catch |err| {
                    oom.* = err;
                    return null;
                };
                defer self.allocator.free(target_path);

                const target_uri = uri_util.pathToUri(self.allocator, target_path) catch |err| {
                    oom.* = err;
                    return null;
                };

                const origin_range = cir_queries.regionToRange(module_env, dep.region());

                return DefinitionResult{
                    .uri = target_uri,
                    .range = .{
                        .start_line = 0,
                        .start_col = 0,
                        .end_line = 0,
                        .end_col = 0,
                    },
                    .origin_selection_range = origin_range,
                };
            }
        }

        // Iterate through all statements to check imports
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);

            // Handle import statements specially - navigate to the imported module or exposed item
            if (stmt == .s_import) {
                const import_stmt = stmt.s_import;
                const import_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(stmt_idx));
                const import_region = module_env.store.getRegionAt(import_node_idx);

                if (cir_queries.regionContainsOffset(import_region, target_offset)) {
                    // Get the module name from the import, including package qualifier if present
                    const raw_module_name = module_env.common.idents.getText(import_stmt.module_name_tok);
                    const qualified_module_name = if (import_stmt.qualifier_tok) |pkg_tok|
                        std.fmt.allocPrint(self.allocator, "{s}.{s}", .{
                            module_env.common.idents.getText(pkg_tok),
                            raw_module_name,
                        }) catch null
                    else
                        null;
                    defer if (qualified_module_name) |q_name| self.allocator.free(q_name);
                    const module_name = qualified_module_name orelse raw_module_name;

                    // Check if the click is on one of the exposed items
                    const exposed_slice = module_env.store.sliceExposedItems(import_stmt.exposes);
                    for (exposed_slice) |exposed_item_idx| {
                        const exposed_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(exposed_item_idx));
                        const item_region = module_env.store.getRegionAt(exposed_node_idx);
                        if (cir_queries.regionContainsOffset(item_region, target_offset)) {
                            const exposed_item = module_env.store.getExposedItem(exposed_item_idx);
                            const member_name = module_env.getIdentText(exposed_item.name);
                            return self.findDefinitionInModule(build_env, doc_path, module_name, member_name, oom);
                        }
                    }

                    // Try to find the module in the coordinator state.
                    if (self.findDefinitionInModule(build_env, doc_path, module_name, null, oom)) |result| {
                        return result;
                    }
                }
            }

            // Check type annotations in statements
            const maybe_type_anno = statementTypeAnno(module_env, stmt);

            if (maybe_type_anno) |type_anno_idx| {
                if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, type_anno_idx, target_offset, oom)) |result| {
                    // If URI is empty, it's a local type - use current file
                    if (result.uri.len == 0) {
                        const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                            oom.* = err;
                            return null;
                        };
                        return DefinitionResult{
                            .uri = uri_copy,
                            .range = result.range,
                        };
                    }
                    return result;
                }
            }

            const stmt_parts = module_lookup.getStatementParts(stmt);

            if (stmt_parts.expr) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (cir_queries.regionContainsOffset(expr_region, target_offset)) {
                    if (cir_queries.findLookupAtOffset(module_env, target_offset)) |found| {
                        best_lookup = found;
                    }
                }
            }

            if (stmt_parts.expr2) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (cir_queries.regionContainsOffset(expr_region, target_offset)) {
                    if (cir_queries.findLookupAtOffset(module_env, target_offset)) |found| {
                        best_lookup = found;
                    }
                }
            }
        }

        // If we found a lookup expression, resolve it to a definition
        if (best_lookup) |lookup_result| {
            const expr_idx = switch (lookup_result) {
                .expr => |idx| idx,
                .field_access => return null,
            };
            const expr = module_env.store.getExpr(expr_idx);
            const expr_tag = std.meta.activeTag(expr);
            if (expr_tag == .e_lookup_local) {
                const lookup = expr.e_lookup_local;
                // Get the pattern's region - that's where it's defined
                const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(lookup.pattern_idx));
                const def_region = module_env.store.getRegionAt(pattern_node_idx);
                const range = cir_queries.regionToRange(module_env, def_region) orelse return null;
                const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                    oom.* = err;
                    return null;
                };
                return DefinitionResult{
                    .uri = uri_copy,
                    .range = range,
                };
            }
            if (expr_tag == .e_lookup_external) {
                const lookup = expr.e_lookup_external;
                const import_idx_int = @intFromEnum(lookup.module_idx);
                if (import_idx_int >= module_env.imports.imports.len()) return null;

                const string_idx = module_env.imports.imports.items.items[import_idx_int];
                const module_name = module_env.common.getString(string_idx);
                const member_name = module_env.getIdentText(lookup.ident_idx);

                const region_text = module_env.getSource(lookup.region);
                if (std.mem.find(u8, region_text, ".")) |dot_pos| {
                    const prefix = region_text[0..dot_pos];
                    const suffix = region_text[dot_pos + 1 ..];
                    const dot_offset = lookup.region.start.offset + @as(u32, @intCast(dot_pos));
                    if (target_offset < dot_offset) {
                        if (completion_builtins.isBuiltinType(prefix)) {
                            return self.findBuiltinDefinition(prefix, null, oom);
                        }
                        return self.findDefinitionInModule(build_env, doc_path, module_name, null, oom);
                    } else {
                        if (completion_builtins.isBuiltinType(prefix)) {
                            return self.findBuiltinDefinition(prefix, suffix, oom);
                        }
                    }
                }
                return self.findDefinitionInModule(build_env, doc_path, module_name, member_name, oom);
            }
            if (expr_tag == .e_lookup_associated) {
                const lookup = expr.e_lookup_associated;
                const member_name = module_env.getIdentText(lookup.item_ident);
                const import_idx_int = @intFromEnum(lookup.module_idx);
                if (import_idx_int < module_env.imports.imports.len()) {
                    const string_idx = module_env.imports.imports.items.items[import_idx_int];
                    const module_name = module_env.common.getString(string_idx);
                    const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                    const expr_region = module_env.store.getRegionAt(expr_node_idx);
                    const region_text = module_env.getSource(expr_region);
                    if (std.mem.find(u8, region_text, ".")) |dot_pos| {
                        const prefix = region_text[0..dot_pos];
                        const suffix = region_text[dot_pos + 1 ..];
                        const dot_offset = expr_region.start.offset + @as(u32, @intCast(dot_pos));
                        if (target_offset < dot_offset) {
                            if (completion_builtins.isBuiltinType(prefix)) {
                                return self.findBuiltinDefinition(prefix, null, oom);
                            }
                            return self.findDefinitionInModule(build_env, doc_path, module_name, null, oom);
                        } else {
                            if (completion_builtins.isBuiltinType(prefix)) {
                                return self.findBuiltinDefinition(prefix, suffix, oom);
                            }
                        }
                    }
                    return self.findDefinitionInModule(build_env, doc_path, module_name, member_name, oom);
                }
                return null;
            }
            if (expr_tag == .e_dispatch_call) {
                const method_call = expr.e_dispatch_call;
                const method_name = module_env.common.idents.getText(method_call.method_name);
                const receiver_type_var = ModuleEnv.varFrom(method_call.receiver);
                const resolved = module_env.types.resolveVar(receiver_type_var);
                const base_type_opt: ?[]const u8 = switch (resolved.desc.content) {
                    .alias => |alias| module_env.common.idents.getText(alias.ident.ident_idx),
                    .structure => |flat| switch (flat) {
                        .nominal_type => |nom| module_env.common.idents.getText(nom.ident.ident_idx),
                        .record,
                        .record_unbound,
                        .tuple,
                        .fn_pure,
                        .fn_effectful,
                        .fn_unbound,
                        .empty_record,
                        .tag_union,
                        .empty_tag_union,
                        => null,
                    },
                    .flex,
                    .rigid,
                    .field_presence,
                    .err,
                    => null,
                };
                if (base_type_opt) |base_type| {
                    return self.findDefinitionInModule(build_env, doc_path, base_type, method_name, oom);
                }
                return null;
            }
            return null;
        }

        // Check for tag expression or pattern (e.g. `WaitingForInit` in pattern `WaitingForInit =>` or expr `WaitingForInit`)
        if (cir_queries.findTagAtOffset(module_env, target_offset)) |tag_ref| {
            return self.findTagDefinition(build_env, module_env, doc_path, tag_ref, current_uri, oom);
        }

        return null;
    }

    const TagOriginInfo = struct {
        origin_module: base.ModuleIdentity.Idx,
        source_decl: types.SourceDecl,
    };

    fn resolveTagOrigin(module_env: *ModuleEnv, type_var: types.Var) ?TagOriginInfo {
        const resolved = module_env.types.resolveVar(type_var);
        const content = resolved.desc.content;
        switch (content) {
            .alias => |alias| {
                return .{
                    .origin_module = alias.origin_module,
                    .source_decl = alias.source_decl,
                };
            },
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| {
                    return .{
                        .origin_module = nominal.origin_module,
                        .source_decl = nominal.source.sourceDecl(),
                    };
                },
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .record,
                .record_unbound,
                .tuple,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => return null,
            },
            .flex,
            .rigid,
            .field_presence,
            .err,
            => return null,
        }
    }

    fn findModuleByContentIdentity(env: *BuildEnv, target_hash: *const [32]u8) ?*compile.coordinator.ModuleState {
        const coord = env.coordinator orelse return null;
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*mod| {
                if (mod.moduleEnv()) |mod_env| {
                    if (mod_env.contentIdentityHash()) |hash| {
                        if (std.mem.eql(u8, hash, target_hash)) {
                            return mod;
                        }
                    }
                }
            }
        }
        return null;
    }

    fn findTagInTypeAnno(store: *const can.NodeStore, common: *const base.CommonEnv, type_anno_idx: CIR.TypeAnno.Idx, tag_name: []const u8) ?Region {
        const type_anno = store.getTypeAnno(type_anno_idx);
        switch (type_anno) {
            .tag_union => |tu| {
                const tags_slice = store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    const tag = store.getTypeAnno(tag_idx);
                    if (tag == .tag) {
                        const name = common.idents.getText(tag.tag.name);
                        if (std.mem.eql(u8, name, tag_name)) {
                            return store.getTypeAnnoRegion(tag_idx);
                        }
                    }
                }
                if (tu.ext) |ext_idx| {
                    if (findTagInTypeAnno(store, common, ext_idx, tag_name)) |r| return r;
                }
            },
            .tag => |t| {
                const name = common.idents.getText(t.name);
                if (std.mem.eql(u8, name, tag_name)) {
                    return store.getTypeAnnoRegion(type_anno_idx);
                }
            },
            .apply,
            .rigid_var,
            .rigid_var_lookup,
            .underscore,
            .lookup,
            .tuple,
            .record,
            .@"fn",
            .parens,
            .malformed,
            => {},
        }
        return null;
    }

    fn findTagInModuleEnv(mod_env: *ModuleEnv, tag_name: []const u8) ?Region {
        const statements_slice = mod_env.store.sliceStatements(mod_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = mod_env.store.getStatement(stmt_idx);
            const maybe_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                .s_alias_decl => |a| a.anno,
                .s_nominal_decl => |n| n.anno,
                .s_decl,
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_crash,
                .s_dbg,
                .s_expr,
                .s_expect,
                .s_for,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_break,
                .s_return,
                .s_import,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => null,
            };
            if (maybe_anno) |anno_idx| {
                if (findTagInTypeAnno(&mod_env.store, &mod_env.common, anno_idx, tag_name)) |r| {
                    return r;
                }
            }
        }
        return null;
    }

    /// Helper function to find a tag declaration (e.g., `WaitingForInit` in `LoopState : [WaitingForInit, ...]`)
    fn findTagDefinition(
        self: *SyntaxChecker,
        build_env: *BuildEnv,
        module_env: *ModuleEnv,
        doc_path: []const u8,
        tag_ref: cir_queries.TagRef,
        current_uri: []const u8,
        oom: *?Allocator.Error,
    ) ?DefinitionResult {
        const tag_name = tag_ref.name;

        // 1. If the tag reference carries an explicit external nominal import identity,
        // navigate directly to that imported module and declaration.
        if (tag_ref.nominal_external) |nom_ext| {
            const import_idx_int = @intFromEnum(nom_ext.module_idx);
            if (import_idx_int < module_env.imports.imports.len()) {
                const string_idx = module_env.imports.imports.items.items[import_idx_int];
                const module_name = module_env.common.getString(string_idx);
                const env = build_env;
                const importing_pkg = env.findPackageForModulePath(doc_path);
                const mod_state_opt = if (std.mem.find(u8, module_name, ".") != null)
                    env.findModuleByQualifiedNameInPackage(importing_pkg, module_name)
                else
                    env.findModuleByNameInPackage(importing_pkg, module_name);
                if (mod_state_opt) |mod_state| {
                    if (mod_state.moduleEnv()) |target_mod_env| {
                        const target_node_idx: CIR.Node.Idx = @enumFromInt(nom_ext.target_node_idx);
                        const node_tag = target_mod_env.store.nodes.get(target_node_idx).tag;
                        if (node_tag == .statement_nominal_decl or node_tag == .statement_alias_decl) {
                            const stmt = target_mod_env.store.getStatement(@enumFromInt(@intFromEnum(target_node_idx)));
                            const maybe_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                                .s_alias_decl => |a| a.anno,
                                .s_nominal_decl => |n| n.anno,
                                .s_decl,
                                .s_var,
                                .s_var_uninitialized,
                                .s_reassign,
                                .s_crash,
                                .s_dbg,
                                .s_expr,
                                .s_expect,
                                .s_for,
                                .s_while,
                                .s_infinite_loop,
                                .s_breakable_loop,
                                .s_break,
                                .s_return,
                                .s_import,
                                .s_where_alias_decl,
                                .s_type_anno,
                                .s_type_var_alias,
                                .s_runtime_error,
                                => null,
                            };
                            if (maybe_anno) |anno_idx| {
                                if (findTagInTypeAnno(&target_mod_env.store, &target_mod_env.common, anno_idx, tag_name)) |tag_region| {
                                    const range = cir_queries.regionToRange(target_mod_env, tag_region) orelse return null;
                                    const module_uri = uri_util.pathToUri(self.allocator, mod_state.path) catch |err| {
                                        oom.* = err;
                                        return null;
                                    };
                                    return DefinitionResult{
                                        .uri = module_uri,
                                        .range = range,
                                    };
                                }
                            }
                        }
                        if (findTagInModuleEnv(target_mod_env, tag_name)) |tag_region| {
                            const range = cir_queries.regionToRange(target_mod_env, tag_region) orelse return null;
                            const module_uri = uri_util.pathToUri(self.allocator, mod_state.path) catch |err| {
                                oom.* = err;
                                return null;
                            };
                            return DefinitionResult{
                                .uri = module_uri,
                                .range = range,
                            };
                        }
                    }
                }
            }
            return null;
        }

        // 2. If the tag reference carries an explicit local nominal declaration identity,
        // navigate directly to that local statement.
        if (tag_ref.nominal_decl) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const maybe_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                .s_alias_decl => |a| a.anno,
                .s_nominal_decl => |n| n.anno,
                .s_decl,
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_crash,
                .s_dbg,
                .s_expr,
                .s_expect,
                .s_for,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_break,
                .s_return,
                .s_import,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => null,
            };
            if (maybe_anno) |anno_idx| {
                if (findTagInTypeAnno(&module_env.store, &module_env.common, anno_idx, tag_name)) |tag_region| {
                    const range = cir_queries.regionToRange(module_env, tag_region) orelse return null;
                    const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                        oom.* = err;
                        return null;
                    };
                    return DefinitionResult{
                        .uri = uri_copy,
                        .range = range,
                    };
                }
            }
        }

        // 3. If type inference resolved the tag's type (or the matched condition's type for a pattern tag)
        // to a nominal type or alias, use the exact declaring module identity and source declaration locator.
        const origin_info_opt = resolveTagOrigin(module_env, tag_ref.type_var) orelse (if (tag_ref.match_cond_type_var) |cond_var| resolveTagOrigin(module_env, cond_var) else null);
        if (origin_info_opt) |origin_info| {
            if (origin_info.origin_module == module_env.selfModuleIdentity()) {
                // Defined in current module
                if (origin_info.source_decl.toOptional()) |stmt_num| {
                    const stmt = module_env.store.getStatement(@enumFromInt(stmt_num));
                    const maybe_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                        .s_alias_decl => |a| a.anno,
                        .s_nominal_decl => |n| n.anno,
                        .s_decl,
                        .s_var,
                        .s_var_uninitialized,
                        .s_reassign,
                        .s_crash,
                        .s_dbg,
                        .s_expr,
                        .s_expect,
                        .s_for,
                        .s_while,
                        .s_infinite_loop,
                        .s_breakable_loop,
                        .s_break,
                        .s_return,
                        .s_import,
                        .s_where_alias_decl,
                        .s_type_anno,
                        .s_type_var_alias,
                        .s_runtime_error,
                        => null,
                    };
                    if (maybe_anno) |anno_idx| {
                        if (findTagInTypeAnno(&module_env.store, &module_env.common, anno_idx, tag_name)) |tag_region| {
                            const range = cir_queries.regionToRange(module_env, tag_region) orelse return null;
                            const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                                oom.* = err;
                                return null;
                            };
                            return DefinitionResult{
                                .uri = uri_copy,
                                .range = range,
                            };
                        }
                    }
                }
                if (findTagInModuleEnv(module_env, tag_name)) |tag_region| {
                    const range = cir_queries.regionToRange(module_env, tag_region) orelse return null;
                    const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                        oom.* = err;
                        return null;
                    };
                    return DefinitionResult{
                        .uri = uri_copy,
                        .range = range,
                    };
                }
            } else {
                // Defined in a specific external module
                const origin_hash = module_env.moduleIdentityHash(origin_info.origin_module);
                if (findModuleByContentIdentity(build_env, origin_hash)) |target_mod_state| {
                    if (target_mod_state.moduleEnv()) |target_mod_env| {
                        if (origin_info.source_decl.toOptional()) |stmt_num| {
                            const stmt = target_mod_env.store.getStatement(@enumFromInt(stmt_num));
                            const maybe_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                                .s_alias_decl => |a| a.anno,
                                .s_nominal_decl => |n| n.anno,
                                .s_decl,
                                .s_var,
                                .s_var_uninitialized,
                                .s_reassign,
                                .s_crash,
                                .s_dbg,
                                .s_expr,
                                .s_expect,
                                .s_for,
                                .s_while,
                                .s_infinite_loop,
                                .s_breakable_loop,
                                .s_break,
                                .s_return,
                                .s_import,
                                .s_where_alias_decl,
                                .s_type_anno,
                                .s_type_var_alias,
                                .s_runtime_error,
                                => null,
                            };
                            if (maybe_anno) |anno_idx| {
                                if (findTagInTypeAnno(&target_mod_env.store, &target_mod_env.common, anno_idx, tag_name)) |tag_region| {
                                    const range = cir_queries.regionToRange(target_mod_env, tag_region) orelse return null;
                                    const module_uri = uri_util.pathToUri(self.allocator, target_mod_state.path) catch |err| {
                                        oom.* = err;
                                        return null;
                                    };
                                    return DefinitionResult{
                                        .uri = module_uri,
                                        .range = range,
                                    };
                                }
                            }
                        }
                        if (findTagInModuleEnv(target_mod_env, tag_name)) |tag_region| {
                            const range = cir_queries.regionToRange(target_mod_env, tag_region) orelse return null;
                            const module_uri = uri_util.pathToUri(self.allocator, target_mod_state.path) catch |err| {
                                oom.* = err;
                                return null;
                            };
                            return DefinitionResult{
                                .uri = module_uri,
                                .range = range,
                            };
                        }
                    }
                }
            }
            return null;
        }

        // 4. If tag has no nominal/alias type origin (e.g. pure open tag), check current module
        if (findTagInModuleEnv(module_env, tag_name)) |tag_region| {
            const range = cir_queries.regionToRange(module_env, tag_region) orelse return null;
            const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                oom.* = err;
                return null;
            };
            return DefinitionResult{
                .uri = uri_copy,
                .range = range,
            };
        }

        return null;
    }

    /// Helper function to find a builtin definition/type in Builtin.roc
    fn findBuiltinDefinition(
        self: *SyntaxChecker,
        base_name: []const u8,
        member_name: ?[]const u8,
        oom: *?Allocator.Error,
    ) ?DefinitionResult {
        // Write embedded builtin source to roc cache
        const cache_dir = self.cache_config.getModuleCacheDir(self.allocator) catch |err| switch (err) {
            error.OutOfMemory => {
                oom.* = error.OutOfMemory;
                return null;
            },
            error.NoHomeDirectory => return null,
        };
        const builtin_cache_path = std.fs.path.join(self.allocator, &.{ cache_dir, "Builtin.roc" }) catch |err| {
            self.allocator.free(cache_dir);
            oom.* = err;
            return null;
        };
        self.allocator.free(cache_dir);

        // Write file if it doesn't exist
        if (std.Io.Dir.cwd().access(self.std_io, builtin_cache_path, .{})) |_| {
            // Already exists
        } else |_| {
            // Create parent dirs and write embedded source
            if (std.fs.path.dirname(builtin_cache_path)) |dir| {
                std.Io.Dir.cwd().createDirPath(self.std_io, dir) catch {};
            }
            const file = std.Io.Dir.cwd().createFile(self.std_io, builtin_cache_path, .{}) catch {
                self.allocator.free(builtin_cache_path);
                return null;
            };
            defer file.close(self.std_io);
            file.writeStreamingAll(self.std_io, compiled_builtins.builtin_source) catch {
                self.allocator.free(builtin_cache_path);
                return null;
            };
        }

        const module_uri = uri_util.pathToUri(self.allocator, builtin_cache_path) catch |err| {
            self.allocator.free(builtin_cache_path);
            oom.* = err;
            return null;
        };
        self.allocator.free(builtin_cache_path);

        var builtin_module = can.BuiltinStatic.moduleView(
            self.allocator,
            compiled_builtins.builtin_bin[0..],
            "Builtin",
            compiled_builtins.builtin_source,
        ) catch {
            self.allocator.free(module_uri);
            return null;
        };
        defer builtin_module.deinit();

        const benv = builtin_module.env;

        // Find the type declaration region from the generated builtin CIR.
        var type_decl_range: ?LspRange = null;
        var type_decl_start: ?u32 = null;
        var type_decl_end: u32 = std.math.maxInt(u32);

        const builtin_indices = compiled_builtins.builtinIndices(CIR);
        inline for (CIR.builtin_type_specs) |spec| {
            if (std.mem.eql(u8, spec.display_name, base_name) or std.mem.eql(u8, spec.qualified_name, base_name)) {
                const stmt_idx = @field(builtin_indices, spec.type_field);
                const decl_region = benv.store.getStatementRegion(stmt_idx);
                type_decl_range = cir_queries.regionToRange(benv, decl_region);
                type_decl_start = decl_region.start.offset;
            }
        }

        if (type_decl_start) |start| {
            inline for (CIR.builtin_type_specs) |spec| {
                if (spec.lookup == .top_level) {
                    const stmt_idx = @field(builtin_indices, spec.type_field);
                    const decl_region = benv.store.getStatementRegion(stmt_idx);
                    if (decl_region.start.offset > start and decl_region.start.offset < type_decl_end) {
                        type_decl_end = decl_region.start.offset;
                    }
                }
            }
        }

        // If a member is requested (e.g., "is_empty" in "Str.is_empty"), find that member in the builtin module
        if (member_name) |member| {
            const lines = std.mem.splitScalar(u8, compiled_builtins.builtin_source, '\n');
            var line_it = lines;
            var offset: usize = 0;
            while (line_it.next()) |line| : (offset += line.len + 1) {
                if (std.mem.find(u8, line, base_name) != null and std.mem.find(u8, line, member) != null) {
                    var col: usize = 0;
                    while (col < line.len and (line[col] == ' ' or line[col] == '\t')) : (col += 1) {}
                    const rest = line[col..];
                    if (std.mem.startsWith(u8, rest, member) and rest.len > member.len and
                        (rest[member.len] == ' ' or rest[member.len] == '\t' or rest[member.len] == ':' or rest[member.len] == '='))
                    {
                        const region = Region{
                            .start = .{ .offset = @intCast(offset + col) },
                            .end = .{ .offset = @intCast(offset + col + member.len) },
                        };
                        if (cir_queries.regionToRange(benv, region)) |range| {
                            return DefinitionResult{ .uri = module_uri, .range = range };
                        }
                    }
                }
            }
            if (findMemberRangeInModuleEnv(benv, base_name, member)) |range| {
                return DefinitionResult{
                    .uri = module_uri,
                    .range = range,
                };
            }
            self.allocator.free(module_uri);
            return null;
        }

        if (type_decl_range) |r| {
            return DefinitionResult{
                .uri = module_uri,
                .range = r,
            };
        }

        self.allocator.free(module_uri);
        return null;
    }

    fn findMemberRangeInModuleEnv(mod_env: *ModuleEnv, base_name: []const u8, member: []const u8) ?LspRange {
        const maybe_def = module_lookup.findDefinitionByName(mod_env, member);

        if (maybe_def) |def_info| {
            const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def_info.pattern_idx));
            const def_region = mod_env.store.getRegionAt(pattern_node_idx);
            return cir_queries.regionToRange(mod_env, def_region);
        }

        if (module_lookup.findTypeDeclarationByModuleMember(mod_env, base_name, member)) |stmt_idx| {
            const decl_region = mod_env.store.getStatementRegion(stmt_idx);
            return cir_queries.regionToRange(mod_env, decl_region);
        }
        for (mod_env.store.sliceStatements(mod_env.all_statements)) |stmt_idx| {
            const stmt = mod_env.store.getStatement(stmt_idx);
            const header_idx: ?CIR.TypeHeader.Idx = switch (stmt) {
                .s_alias_decl => |a| a.header,
                .s_nominal_decl => |n| n.header,
                .s_decl,
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_crash,
                .s_dbg,
                .s_expr,
                .s_expect,
                .s_for,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_break,
                .s_return,
                .s_import,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => null,
            };
            if (header_idx) |h_idx| {
                const header = mod_env.store.getTypeHeader(h_idx);
                if (std.mem.eql(u8, mod_env.getIdentText(header.name), member)) {
                    return cir_queries.regionToRange(mod_env, mod_env.store.getStatementRegion(stmt_idx));
                }
            }
        }
        return null;
    }

    /// Helper function to find a definition in a module by module name and optional member name
    fn findDefinitionInModule(
        self: *SyntaxChecker,
        build_env: *BuildEnv,
        doc_path: []const u8,
        module_name: []const u8,
        member_name: ?[]const u8,
        oom: *?Allocator.Error,
    ) ?DefinitionResult {
        const is_qualified = std.mem.find(u8, module_name, ".") != null;
        const base_name = if (std.mem.findLast(u8, module_name, ".")) |dot_pos|
            module_name[dot_pos + 1 ..]
        else
            module_name;

        const is_builtin_module = std.mem.eql(u8, module_name, "Builtin") or CIR.Import.isCompilerBuiltinImportName(module_name);
        const is_pkg_qualified = is_qualified and !is_builtin_module;

        // Check if member is a builtin type member (e.g., "Str.is_empty" or "List.is_empty")
        if (member_name) |member| {
            if (std.mem.find(u8, member, ".")) |dot_pos| {
                const prefix = member[0..dot_pos];
                const suffix = member[dot_pos + 1 ..];
                if (!is_pkg_qualified and completion_builtins.isBuiltinType(prefix)) {
                    return self.findBuiltinDefinition(prefix, suffix, oom);
                }
            }
        }

        // Check if this is a builtin type - use embedded Builtin.roc source
        if (!is_pkg_qualified and completion_builtins.isBuiltinType(base_name)) {
            self.logDebug(.build, "[DEF] '{s}' is a builtin type", .{base_name});
            return self.findBuiltinDefinition(base_name, member_name, oom);
        }

        if (is_builtin_module) {
            if (member_name) |member| {
                if (std.mem.find(u8, member, ".")) |dot_pos| {
                    const prefix = member[0..dot_pos];
                    const suffix = member[dot_pos + 1 ..];
                    return self.findBuiltinDefinition(prefix, suffix, oom);
                }
                if (completion_builtins.isBuiltinType(member)) {
                    return self.findBuiltinDefinition(member, null, oom);
                }
            }
        }

        const env = build_env;
        const importing_pkg = env.findPackageForModulePath(doc_path);
        const module_state = if (is_qualified)
            env.findModuleByQualifiedNameInPackage(importing_pkg, module_name)
        else
            env.findModuleByNameInPackage(importing_pkg, base_name);
        if (module_state) |mod_state| {
            const module_uri = uri_util.pathToUri(self.allocator, mod_state.path) catch |err| {
                oom.* = err;
                return null;
            };

            var range = LspRange{
                .start_line = 0,
                .start_col = 0,
                .end_line = 0,
                .end_col = 0,
            };

            if (member_name) |member| {
                var found_range: ?LspRange = null;
                const mod_env_opt = mod_state.moduleEnv() orelse self.getModuleEnvByPathInEnv(build_env, mod_state.path);
                if (mod_env_opt) |mod_env| {
                    found_range = findMemberRangeInModuleEnv(mod_env, base_name, member);
                }
                if (found_range == null) {
                    if (self.getSnapshotEnv()) |snap_env| {
                        const snap_importing_pkg = snap_env.findPackageForModulePath(doc_path);
                        const snap_module_state = if (is_qualified)
                            snap_env.findModuleByQualifiedNameInPackage(snap_importing_pkg, module_name)
                        else
                            snap_env.findModuleByNameInPackage(snap_importing_pkg, base_name);
                        if (snap_module_state) |snap_mod_state| {
                            if (snap_mod_state.moduleEnv()) |snap_mod_env| {
                                found_range = findMemberRangeInModuleEnv(snap_mod_env, base_name, member);
                            }
                        }
                    }
                }
                if (found_range) |r| {
                    range = r;
                } else {
                    self.allocator.free(module_uri);
                    return null;
                }
            }

            return DefinitionResult{
                .uri = module_uri,
                .range = range,
            };
        }

        return null;
    }

    /// Helper function to find a module by name and return a DefinitionResult pointing to it
    fn findModuleByName(self: *SyntaxChecker, build_env: *BuildEnv, doc_path: []const u8, module_name: []const u8, oom: *?Allocator.Error) ?DefinitionResult {
        return self.findDefinitionInModule(build_env, doc_path, module_name, null, oom);
    }

    // findLookupAtOffset moved to cir_queries module

    fn resolveTypeBase(
        self: *SyntaxChecker,
        build_env: *BuildEnv,
        module_env: *ModuleEnv,
        doc_path: []const u8,
        base_info: CIR.TypeAnno.LocalOrExternal,
        type_name: []const u8,
        oom: *?Allocator.Error,
    ) ?DefinitionResult {
        switch (base_info) {
            .local => |local| {
                // Local type definition - navigate to the statement where it's declared
                const decl_region = module_env.store.getStatementRegion(local.decl_idx);
                const range = cir_queries.regionToRange(module_env, decl_region) orelse return null;
                return DefinitionResult{
                    .uri = "", // Empty URI means same file - caller should fill in
                    .range = range,
                };
            },
            .external => |ext| {
                const import_idx_int = @intFromEnum(ext.module_idx);
                if (import_idx_int < module_env.imports.imports.len()) {
                    const string_idx = module_env.imports.imports.items.items[import_idx_int];
                    const module_name = module_env.common.getString(string_idx);
                    return self.findDefinitionInModule(build_env, doc_path, module_name, type_name, oom);
                }
                return self.findModuleByName(build_env, doc_path, type_name, oom);
            },
            .pending => |pend| {
                const import_idx_int = @intFromEnum(pend.module_idx);
                if (import_idx_int < module_env.imports.imports.len()) {
                    const string_idx = module_env.imports.imports.items.items[import_idx_int];
                    const module_name = module_env.common.getString(string_idx);
                    return self.findDefinitionInModule(build_env, doc_path, module_name, type_name, oom);
                }
                return self.findModuleByName(build_env, doc_path, type_name, oom);
            },
            .builtin => {
                if (completion_builtins.isBuiltinType(type_name)) {
                    return self.findBuiltinDefinition(type_name, null, oom);
                }
                return self.findModuleByName(build_env, doc_path, type_name, oom);
            },
        }
    }

    /// Find the type annotation at the given offset and return a DefinitionResult.
    /// This recursively walks type annotation trees to find the most specific type at the cursor.
    fn findTypeAnnoAtOffset(
        self: *SyntaxChecker,
        build_env: *BuildEnv,
        module_env: *ModuleEnv,
        doc_path: []const u8,
        type_anno_idx: CIR.TypeAnno.Idx,
        target_offset: u32,
        oom: *?Allocator.Error,
    ) ?DefinitionResult {
        const region = module_env.store.getTypeAnnoRegion(type_anno_idx);
        if (!cir_queries.regionContainsOffset(region, target_offset)) return null;

        const type_anno = module_env.store.getTypeAnno(type_anno_idx);
        switch (type_anno) {
            .lookup => |lookup| {
                const type_name = module_env.common.idents.getText(lookup.name);
                self.logDebug(.build, "[DEF] TypeAnno.lookup: type='{s}', base={s}", .{
                    type_name,
                    @tagName(lookup.base),
                });
                return self.resolveTypeBase(build_env, module_env, doc_path, lookup.base, type_name, oom);
            },
            .apply => |apply| {
                // Type with args like `List(Str)` - check args first, then the base type
                const args_slice = module_env.store.sliceTypeAnnos(apply.args);
                for (args_slice) |arg_idx| {
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, arg_idx, target_offset, oom)) |result| {
                        return result;
                    }
                }
                // If not in args, return the base type
                const type_name = module_env.common.idents.getText(apply.name);
                self.logDebug(.build, "[DEF] TypeAnno.apply: type='{s}', base={s}", .{
                    type_name,
                    @tagName(apply.base),
                });
                return self.resolveTypeBase(build_env, module_env, doc_path, apply.base, type_name, oom);
            },
            .record => |rec| {
                // Check record field types
                const fields_slice = module_env.store.sliceAnnoRecordFields(rec.fields);
                for (fields_slice) |field_idx| {
                    const field = module_env.store.getAnnoRecordField(field_idx);
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, field.ty, target_offset, oom)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .tag_union => |tu| {
                // Check tag types
                const tags_slice = module_env.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, tag_idx, target_offset, oom)) |result| {
                        return result;
                    }
                }
                if (tu.ext) |ext_idx| {
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, ext_idx, target_offset, oom)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .tag => |t| {
                // Check tag argument types
                const args_slice = module_env.store.sliceTypeAnnos(t.args);
                for (args_slice) |arg_idx| {
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, arg_idx, target_offset, oom)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .@"fn" => |f| {
                // Check function argument and return types
                const args_slice = module_env.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, arg_idx, target_offset, oom)) |result| {
                        return result;
                    }
                }
                if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, f.ret, target_offset, oom)) |result| {
                    return result;
                }
                return null;
            },
            .tuple => |t| {
                // Check tuple element types
                const elems_slice = module_env.store.sliceTypeAnnos(t.elems);
                for (elems_slice) |elem_idx| {
                    if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, elem_idx, target_offset, oom)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .parens => |p| {
                // Unwrap and recurse
                return self.findTypeAnnoAtOffset(build_env, module_env, doc_path, p.anno, target_offset, oom);
            },
            .rigid_var, .rigid_var_lookup, .underscore, .malformed => {
                // These don't have type definitions to navigate to
                return null;
            },
        }
    }

    /// Recursively search for type annotations in nested expressions (blocks, lambdas, etc.)
    fn findTypeAnnoInExpr(
        self: *SyntaxChecker,
        build_env: *BuildEnv,
        module_env: *ModuleEnv,
        doc_path: []const u8,
        expr_idx: CIR.Expr.Idx,
        target_offset: u32,
        current_uri: []const u8,
        oom: *?Allocator.Error,
    ) ?DefinitionResult {
        const expr = module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_block => |block| {
                // Check statements in the block for type annotations
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);

                    // Extract type annotation from statement
                    const maybe_type_anno = statementTypeAnno(module_env, stmt);

                    if (maybe_type_anno) |type_anno_idx| {
                        if (self.findTypeAnnoAtOffset(build_env, module_env, doc_path, type_anno_idx, target_offset, oom)) |result| {
                            if (result.uri.len == 0) {
                                const uri_copy = self.allocator.dupe(u8, current_uri) catch |err| {
                                    oom.* = err;
                                    return null;
                                };
                                return DefinitionResult{
                                    .uri = uri_copy,
                                    .range = result.range,
                                };
                            }
                            return result;
                        }
                    }

                    // Recurse into expressions within the statement
                    const stmt_parts = module_lookup.getStatementParts(stmt);
                    if (stmt_parts.expr) |stmt_expr| {
                        if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, stmt_expr, target_offset, current_uri, oom)) |result| {
                            return result;
                        }
                    }
                    if (stmt_parts.expr2) |stmt_expr| {
                        if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, stmt_expr, target_offset, current_uri, oom)) |result| {
                            return result;
                        }
                    }
                }
                // Also check final expression
                return self.findTypeAnnoInExpr(build_env, module_env, doc_path, block.final_expr, target_offset, current_uri, oom);
            },
            .e_lambda => |lambda| {
                return self.findTypeAnnoInExpr(build_env, module_env, doc_path, lambda.body, target_offset, current_uri, oom);
            },
            .e_closure => |closure| {
                return self.findTypeAnnoInExpr(build_env, module_env, doc_path, closure.lambda_idx, target_offset, current_uri, oom);
            },
            .e_if => |if_expr| {
                const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, branch.cond, target_offset, current_uri, oom)) |result| {
                        return result;
                    }
                    if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, branch.body, target_offset, current_uri, oom)) |result| {
                        return result;
                    }
                }
                return self.findTypeAnnoInExpr(build_env, module_env, doc_path, if_expr.final_else, target_offset, current_uri, oom);
            },
            .e_match => |match_expr| {
                if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, match_expr.cond, target_offset, current_uri, oom)) |result| {
                    return result;
                }
                const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, branch.value, target_offset, current_uri, oom)) |result| {
                        return result;
                    }
                    if (branch.guard) |guard| {
                        if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, guard, target_offset, current_uri, oom)) |result| {
                            return result;
                        }
                    }
                }
                return null;
            },
            .e_call => |call| {
                if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, call.func, target_offset, current_uri, oom)) |result| {
                    return result;
                }
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg| {
                    if (self.findTypeAnnoInExpr(build_env, module_env, doc_path, arg, target_offset, current_uri, oom)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_str,
            .e_bytes_literal,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_associated_resolved,
            .e_lookup_required,
            .e_list,
            .e_empty_list,
            .e_tuple,
            .e_record,
            .e_empty_record,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_zero_argument_tag,
            .e_hosted_lambda,
            .e_binop,
            .e_unary_minus,
            .e_unary_not,
            .e_field_access,
            .e_method_call,
            .e_dispatch_call,
            .e_interpolation,
            .e_structural_eq,
            .e_structural_hash,
            .e_method_eq,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_tuple_access,
            .e_runtime_error,
            .e_crash,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_return,
            .e_break,
            .e_for,
            .e_run_low_level,
            => return null,
        }
    }

    /// Result of finding highlights for a symbol
    pub const HighlightResult = struct {
        regions: []LspRange,

        pub fn deinit(self: HighlightResult, allocator: std.mem.Allocator) void {
            allocator.free(self.regions);
        }
    };

    /// Get all occurrences of the symbol at the given position.
    /// Uses CIR to properly handle scoped variables (shadowing).
    pub fn getHighlightsAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
    ) QueryError!?HighlightResult {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        self.logDebug(.build, "highlights: document {s} reused={}", .{ build.absolute_path, build.reused });

        if (!build.build_succeeded) {
            self.logDebug(.build, "highlights: build unavailable for {s}", .{build.absolute_path});
            return null;
        }

        // Get module environment
        const module_env = build.getModuleEnv() orelse return null;

        // Convert LSP position to byte offset
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        // Resolve the symbol at this position. The cursor may sit on the
        // definition or on any reference to it; both resolve to the pattern
        // that owns the binding.
        const target_pattern = cir_queries.resolveSymbolAtOffset(module_env, target_offset) orelse return null;

        return HighlightResult{
            .regions = try self.collectSymbolRegions(module_env, target_pattern),
        };
    }

    /// Collect every source range that names `target_pattern`: the binding
    /// itself, the name on its type annotation if it has one, and every
    /// reference to it.
    ///
    /// This is the set a rename must rewrite, so highlighting and renaming
    /// share it rather than each deciding what counts as an occurrence.
    /// Caller owns the returned slice.
    fn collectSymbolRegions(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        target_pattern: CIR.Pattern.Idx,
    ) Allocator.Error![]LspRange {
        var regions = try cir_queries.collectDeclarationRegions(module_env, target_pattern, self.allocator);
        errdefer regions.deinit(self.allocator);

        var lookup_regions = try cir_queries.collectLookupReferences(module_env, target_pattern, self.allocator);
        defer lookup_regions.deinit(self.allocator);
        try regions.appendSlice(self.allocator, lookup_regions.items);

        return regions.toOwnedSlice(self.allocator);
    }

    /// Every place a symbol is written, split the way LSP asks for it.
    pub const ReferencesResult = struct {
        regions: []LspRange,

        pub fn deinit(self: ReferencesResult, allocator: std.mem.Allocator) void {
            allocator.free(self.regions);
        }
    };

    /// Find every occurrence of the symbol at the given position.
    ///
    /// Unlike rename, this reports rather than rewrites, so it is not limited
    /// to plain `assign` bindings: any pattern the cursor resolves to can have
    /// its uses listed.
    ///
    /// When `include_declaration` is false the binding site and its annotation
    /// name are left out, keeping only the places the symbol is read.
    pub fn getReferencesAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) QueryError!?ReferencesResult {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        self.logDebug(.build, "references: document {s} reused={}", .{ build.absolute_path, build.reused });

        if (!build.build_succeeded) {
            self.logDebug(.build, "references: build unavailable for {s}", .{build.absolute_path});
            return null;
        }

        const module_env = build.getModuleEnv() orelse return null;
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;
        const target_pattern = cir_queries.resolveSymbolAtOffset(module_env, target_offset) orelse return null;

        var regions: std.ArrayList(LspRange) = .empty;
        errdefer regions.deinit(self.allocator);

        if (include_declaration) {
            var declarations = try cir_queries.collectDeclarationRegions(module_env, target_pattern, self.allocator);
            defer declarations.deinit(self.allocator);
            try regions.appendSlice(self.allocator, declarations.items);
        }

        var lookups = try cir_queries.collectLookupReferences(module_env, target_pattern, self.allocator);
        defer lookups.deinit(self.allocator);
        try regions.appendSlice(self.allocator, lookups.items);

        return ReferencesResult{
            .regions = try regions.toOwnedSlice(self.allocator),
        };
    }

    /// What checking a new name against the surrounding scopes established.
    const ScopeCheck = enum {
        /// No other binding of that name is live where the renamed one is.
        clear,
        /// Another such binding is live, so the rewrite would capture or shadow.
        taken,
        /// The renamed binding's extent was not found, so nothing was checked.
        scope_unavailable,
    };

    /// Whether renaming `target_pattern` to `new_name` would collide with
    /// another binding of that name.
    ///
    /// Rename rewrites text, but a name means whichever binding is live where
    /// it is written. If a different `new_name` binding is live anywhere the
    /// renamed one is, the rewrite silently repoints a reference: renaming `k`
    /// in `|v| { helper = |k| k + v }` to `v` yields `|v| v + v`, which
    /// compiles and computes something else.
    ///
    /// Two bindings whose live ranges overlap cannot both be called
    /// `new_name`, so an overlap is refused. Bindings in unrelated scopes do
    /// not overlap and stay renameable.
    fn checkNewNameAgainstScope(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        target_pattern: CIR.Pattern.Idx,
        new_name: []const u8,
    ) Allocator.Error!ScopeCheck {
        var scopes = scope_map.ScopeMap.init(self.allocator);
        defer scopes.deinit();
        try scopes.build(module_env);

        var target_binding: ?scope_map.Binding = null;
        for (scopes.bindings.items) |binding| {
            if (@intFromEnum(binding.pattern_idx) == @intFromEnum(target_pattern)) {
                target_binding = binding;
                break;
            }
        }

        // Without the renamed binding's own extent there is nothing to compare
        // against. `ScopeMap` records a destructured field under the extent of
        // the pattern that destructures it, for one, so not every binding is
        // found by its own index. Refuse, but say which of the two happened:
        // "the name is taken" would be a claim about the code that has not
        // been established.
        const target = target_binding orelse return .scope_unavailable;

        for (scopes.bindings.items) |binding| {
            if (@intFromEnum(binding.pattern_idx) == @intFromEnum(target_pattern)) continue;
            if (!std.mem.eql(u8, module_env.common.idents.getText(binding.ident), new_name)) continue;
            if (binding.visible_from <= target.visible_to and target.visible_from <= binding.visible_to) {
                return .taken;
            }
        }
        return .clear;
    }

    /// Why a rename request could not be answered with edits.
    pub const RenameRejection = union(enum) {
        /// The position does not name a binding this server can rename.
        not_a_local_binding,
        /// The requested new name is not usable in place of the old one.
        bad_new_name: rename_rules.Rejection,
        /// Another binding of the requested name is live where the renamed one
        /// is, so the rewrite would capture or shadow it.
        name_already_in_scope,
        /// The renamed binding's extent could not be determined, so the rename
        /// could not be checked for capture at all.
        scope_unavailable,
        /// The binding's own declaration could not be pinned to exactly its
        /// name, so rewriting it would take neighbouring source with it.
        declaration_not_isolated,
    };

    /// The edits a rename produces, all within the requested document.
    pub const RenameResult = struct {
        regions: []LspRange,
        /// The name being replaced, owned by this result.
        old_name: []u8,

        pub fn deinit(self: RenameResult, allocator: std.mem.Allocator) void {
            allocator.free(self.regions);
            allocator.free(self.old_name);
        }
    };

    /// A rename request either produces edits or is refused with a reason.
    pub const RenameOutcome = union(enum) {
        edits: RenameResult,
        rejected: RenameRejection,
    };

    /// What the editor needs to open its rename prompt.
    pub const PrepareRenameResult = struct {
        /// The occurrence under the cursor, which the editor highlights.
        range: LspRange,
        /// The current name, pre-filled into the prompt. Owned by this result.
        placeholder: []u8,

        pub fn deinit(self: PrepareRenameResult, allocator: std.mem.Allocator) void {
            allocator.free(self.placeholder);
        }
    };

    /// Report whether the symbol at the given position can be renamed, and
    /// under what name the editor should prompt.
    ///
    /// Returns null when there is nothing renameable there, which the editor
    /// shows by refusing to open its rename prompt at all.
    pub fn prepareRenameAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
    ) QueryError!?PrepareRenameResult {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        self.logDebug(.build, "prepareRename: document {s} reused={}", .{ build.absolute_path, build.reused });

        // Rename must never be answered from a partial build: rewriting every
        // occurrence but one silently breaks the program, so refusing is the
        // only honest answer when the CIR is unavailable.
        if (!build.build_succeeded) {
            self.logDebug(.build, "prepareRename: build unavailable for {s}", .{build.absolute_path});
            return null;
        }

        const module_env = build.getModuleEnv() orelse return null;
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;
        const target = renameTargetAt(module_env, target_offset) orelse return null;
        if (cir_queries.declarationNameRegion(module_env, target.pattern) == null) return null;

        const regions = try self.collectSymbolRegions(module_env, target.pattern);
        defer self.allocator.free(regions);

        // Prompt on the occurrence the cursor is actually in, not on the
        // definition, so the editor highlights what the user clicked.
        const cursor_range = rangeContaining(regions, line, character) orelse return null;

        const name = module_env.common.idents.getText(target.ident);
        return PrepareRenameResult{
            .range = cursor_range,
            .placeholder = try self.allocator.dupe(u8, name),
        };
    }

    /// Produce the edits that rename the symbol at the given position.
    ///
    /// Returns null when the document could not be built or the position maps
    /// nowhere; a built document with nothing renameable at that position is
    /// reported as a rejection instead, so the editor can say why.
    pub fn getRenameEditsAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
        new_name: []const u8,
    ) QueryError!?RenameOutcome {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        self.logDebug(.build, "rename: document {s} reused={}", .{ build.absolute_path, build.reused });

        if (!build.build_succeeded) {
            self.logDebug(.build, "rename: build unavailable for {s}", .{build.absolute_path});
            return null;
        }

        const module_env = build.getModuleEnv() orelse return null;
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        const target = renameTargetAt(module_env, target_offset) orelse
            return RenameOutcome{ .rejected = .not_a_local_binding };

        // Renaming the uses while leaving the declaration behind is exactly the
        // partial rewrite this must not produce.
        if (cir_queries.declarationNameRegion(module_env, target.pattern) == null) {
            return RenameOutcome{ .rejected = .declaration_not_isolated };
        }

        const old_name = module_env.common.idents.getText(target.ident);
        if (try rename_rules.checkNewName(self.allocator, old_name, new_name)) |rejection| {
            return RenameOutcome{ .rejected = .{ .bad_new_name = rejection } };
        }

        switch (try self.checkNewNameAgainstScope(module_env, target.pattern, new_name)) {
            .clear => {},
            .taken => return RenameOutcome{ .rejected = .name_already_in_scope },
            .scope_unavailable => return RenameOutcome{ .rejected = .scope_unavailable },
        }

        const regions = try self.collectSymbolRegions(module_env, target.pattern);
        errdefer self.allocator.free(regions);

        return RenameOutcome{ .edits = .{
            .regions = regions,
            .old_name = try self.allocator.dupe(u8, old_name),
        } };
    }

    /// Find the pattern_idx at the given offset.
    /// Returns the pattern being defined or referenced at that position.
    pub fn getDocumentSymbols(
        self: *SyntaxChecker,
        allocator: std.mem.Allocator,
        uri: []const u8,
        source: []const u8,
    ) QueryError![]document_symbol_handler.SymbolInformation {
        const SymbolInformation = document_symbol_handler.SymbolInformation;

        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, source);
        defer build.deinit();

        self.logDebug(.build, "symbols: document {s} reused={}", .{ build.absolute_path, build.reused });

        if (!build.build_succeeded) {
            self.logDebug(.build, "symbols: build unavailable for {s}", .{build.absolute_path});
            return &[_]SymbolInformation{};
        }

        const module_env = build.getModuleEnv() orelse return &[_]SymbolInformation{};

        // Build line offset table
        const line_offsets = try pos.buildLineOffsets(allocator, source);
        defer line_offsets.deinit();

        var symbols: std.ArrayList(SymbolInformation) = .empty;
        errdefer {
            for (symbols.items) |*sym| {
                allocator.free(sym.name);
            }
            symbols.deinit(allocator);
        }

        // Check top-level definitions (modules/apps store functions here)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        self.logDebug(.build, "symbols: all_defs.len={}, all_statements.len={}", .{
            defs_slice.len,
            module_env.store.sliceStatements(module_env.all_statements).len,
        });
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (extractSymbolFromDecl(module_env, def.pattern, def.expr, source, uri, &line_offsets)) |symbol| {
                self.logDebug(.build, "symbols: found def symbol '{s}'", .{symbol.name});
                try appendOwnedSymbol(allocator, &symbols, symbol);
            }
        }

        // Also check top-level statements (some module types use these)
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_tag = std.meta.activeTag(stmt);
            if (stmt_tag == .s_alias_decl) {
                if (extractSymbolFromTypeDecl(module_env, stmt.s_alias_decl.header, stmt_idx, uri, &line_offsets, .class)) |symbol| {
                    self.logDebug(.build, "symbols: found alias symbol '{s}'", .{symbol.name});
                    try appendOwnedSymbol(allocator, &symbols, symbol);
                }
                continue;
            }
            if (stmt_tag == .s_nominal_decl) {
                if (extractSymbolFromTypeDecl(module_env, stmt.s_nominal_decl.header, stmt_idx, uri, &line_offsets, .@"struct")) |symbol| {
                    self.logDebug(.build, "symbols: found nominal symbol '{s}'", .{symbol.name});
                    try appendOwnedSymbol(allocator, &symbols, symbol);
                }
                continue;
            }
            if (stmt_tag == .s_where_alias_decl) {
                if (extractSymbolFromTypeDecl(module_env, stmt.s_where_alias_decl.header, stmt_idx, uri, &line_offsets, .interface)) |symbol| {
                    self.logDebug(.build, "symbols: found where alias symbol '{s}'", .{symbol.name});
                    try appendOwnedSymbol(allocator, &symbols, symbol);
                }
                continue;
            }

            const stmt_parts = module_lookup.getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                if (stmt_parts.expr) |expr_idx| {
                    if (extractSymbolFromDecl(module_env, pattern_idx, expr_idx, source, uri, &line_offsets)) |symbol| {
                        self.logDebug(.build, "symbols: found stmt symbol '{s}'", .{symbol.name});
                        try appendOwnedSymbol(allocator, &symbols, symbol);
                    }
                } else if (extractSymbolFromPattern(module_env, pattern_idx, uri, &line_offsets, .variable)) |symbol| {
                    self.logDebug(.build, "symbols: found pattern symbol '{s}'", .{symbol.name});
                    try appendOwnedSymbol(allocator, &symbols, symbol);
                }
            }
        }
        self.logDebug(.build, "symbols: returning {} symbols", .{symbols.items.len});
        return symbols.toOwnedSlice(allocator);
    }

    // CompletionContext moved to completion/context.zig
    pub const CompletionContext = completion_context.CompletionContext;

    // detectCompletionContext and computeOffset moved to completion/context.zig

    /// Resolve a module alias to its real module name using import statements.
    /// Returns the input name if no alias match is found.
    fn resolveModuleAlias(module_env: *ModuleEnv, name: []const u8) []const u8 {
        if (std.mem.eql(u8, module_env.module_name, name)) return name;

        const import_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (import_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            if (stmt != .s_import) continue;

            const import_stmt = stmt.s_import;
            if (import_stmt.alias_tok) |alias_tok| {
                const alias_name = module_env.common.idents.getText(alias_tok);
                if (std.mem.eql(u8, alias_name, name)) {
                    return module_env.common.idents.getText(import_stmt.module_name_tok);
                }
            }
        }

        return name;
    }

    /// Resolve a local binding's type var for chained access completion.
    fn resolveLocalBindingTypeVar(self: *SyntaxChecker, module_env: *ModuleEnv, name: []const u8, name_start: u32, oom: *?Allocator.Error) ?types.Var {
        var scope = scope_map.ScopeMap.init(self.allocator);
        defer scope.deinit();
        scope.build(module_env) catch |err| {
            oom.* = err;
            return null;
        };

        for (scope.bindings.items) |binding| {
            const binding_name = module_env.getIdentText(binding.ident);
            if (!scope_map.ScopeMap.isVisibleAt(binding, name_start)) continue;
            if (std.mem.eql(u8, binding_name, name)) {
                return ModuleEnv.varFrom(binding.pattern_idx);
            }
        }

        if (module_lookup.findDefinitionByName(module_env, name)) |def_info| {
            return ModuleEnv.varFrom(def_info.pattern_idx);
        }

        return null;
    }

    /// Find the module env that should back module member resolution.
    fn findModuleEnvForCompletion(module_lookup_env: *BuildEnv, env: *BuildEnv, module_name: []const u8) ?*ModuleEnv {
        if (completion_builtins.isBuiltinType(module_name)) {
            return env.builtin_modules.builtin_module.env;
        }

        if (module_lookup_env.findModuleByName(module_name)) |mod_state| {
            if (mod_state.moduleEnv()) |mod_env| return mod_env;
        }

        return null;
    }

    /// Resolve the type variable for a dotted access chain (e.g., myrec.subrec).
    fn resolveAccessChainTypeVar(
        self: *SyntaxChecker,
        builder: *completion_builder.CompletionBuilder,
        module_env: *ModuleEnv,
        module_lookup_env: *BuildEnv,
        env: *BuildEnv,
        access_chain: []const u8,
        chain_start: u32,
        oom: *?Allocator.Error,
    ) ?struct { module_env: *ModuleEnv, type_var: types.Var } {
        var idx: usize = 0;
        const first = nextChainSegment(access_chain, idx) orelse return null;
        idx = first.next;

        if (first.segment.len == 0) return null;

        if (std.ascii.isUpper(first.segment[0])) {
            const resolved_module = resolveModuleAlias(module_env, first.segment);
            const resolved_env = findModuleEnvForCompletion(module_lookup_env, env, resolved_module) orelse module_env;
            const member = nextChainSegment(access_chain, idx) orelse return null;
            idx = member.next;

            // NOTE: Nested nominal/module members are often stored as qualified
            // identifiers (e.g. `MyType.Sub`). Prefer exact lookup first, then
            // try the qualified path and finally unqualified suffix matching.
            const def_info = findDefinitionForNamespaceMember(
                self.allocator,
                resolved_env,
                first.segment,
                member.segment,
                oom,
            ) orelse return null;
            var type_var = ModuleEnv.varFrom(def_info.pattern_idx);
            var namespace_prefix = std.ArrayList(u8).empty;
            defer namespace_prefix.deinit(self.allocator);
            namespace_prefix.appendSlice(self.allocator, first.segment) catch |err| {
                oom.* = err;
                return null;
            };
            namespace_prefix.append(self.allocator, '.') catch |err| {
                oom.* = err;
                return null;
            };
            namespace_prefix.appendSlice(self.allocator, member.segment) catch |err| {
                oom.* = err;
                return null;
            };

            while (nextChainSegment(access_chain, idx)) |segment| {
                idx = segment.next;

                // Prefer namespace/member traversal for uppercase segments before
                // falling back to structural field traversal.
                if (findDefinitionByQualifiedPrefix(self.allocator, resolved_env, namespace_prefix.items, segment.segment, oom)) |next_def| {
                    type_var = ModuleEnv.varFrom(next_def.pattern_idx);
                    namespace_prefix.append(self.allocator, '.') catch |err| {
                        oom.* = err;
                        return null;
                    };
                    namespace_prefix.appendSlice(self.allocator, segment.segment) catch |err| {
                        oom.* = err;
                        return null;
                    };
                    continue;
                }
                if (oom.* != null) return null;

                const next_var = builder.getFieldTypeVarFromTypeVar(resolved_env, type_var, segment.segment) orelse return null;
                type_var = next_var;

                namespace_prefix.append(self.allocator, '.') catch |err| {
                    oom.* = err;
                    return null;
                };
                namespace_prefix.appendSlice(self.allocator, segment.segment) catch |err| {
                    oom.* = err;
                    return null;
                };
            }

            return .{ .module_env = resolved_env, .type_var = type_var };
        }

        var type_var = self.resolveLocalBindingTypeVar(module_env, first.segment, chain_start, oom) orelse return null;
        while (nextChainSegment(access_chain, idx)) |segment| {
            idx = segment.next;
            const next_var = builder.getFieldTypeVarFromTypeVar(module_env, type_var, segment.segment) orelse return null;
            type_var = next_var;
        }

        return .{ .module_env = module_env, .type_var = type_var };
    }

    /// Resolve a namespace member definition with progressively broader matching.
    ///
    /// This is tailored for completion on nested nominal/module namespaces where
    /// definitions may be stored either as unqualified names (`Sub`) or as
    /// qualified names (`MyType.Sub`).
    fn findDefinitionForNamespaceMember(
        allocator: Allocator,
        module_env: *ModuleEnv,
        namespace_prefix: []const u8,
        member_name: []const u8,
        oom: *?Allocator.Error,
    ) ?module_lookup.DefinitionInfo {
        if (module_lookup.findDefinitionByName(module_env, member_name)) |def_info| {
            return def_info;
        }

        if (findDefinitionByQualifiedPrefix(allocator, module_env, namespace_prefix, member_name, oom)) |def_info| {
            return def_info;
        }
        if (oom.* != null) return null;

        return module_lookup.findDefinitionByUnqualifiedName(module_env, member_name);
    }

    /// Find a definition by building `prefix.member` and doing exact lookup.
    fn findDefinitionByQualifiedPrefix(
        allocator: Allocator,
        module_env: *ModuleEnv,
        prefix: []const u8,
        member_name: []const u8,
        oom: *?Allocator.Error,
    ) ?module_lookup.DefinitionInfo {
        const qualified = std.fmt.allocPrint(allocator, "{s}.{s}", .{ prefix, member_name }) catch |err| {
            oom.* = err;
            return null;
        };
        defer allocator.free(qualified);
        return module_lookup.findDefinitionByName(module_env, qualified);
    }

    /// Get the next segment in a dotted access chain.
    fn nextChainSegment(chain: []const u8, start: usize) ?struct { segment: []const u8, next: usize } {
        if (start >= chain.len) return null;
        const dot_idx = std.mem.findScalarPos(u8, chain, start, '.') orelse chain.len;
        const segment = chain[start..dot_idx];
        const next = if (dot_idx < chain.len) dot_idx + 1 else chain.len;
        return .{ .segment = segment, .next = next };
    }

    /// Get the last segment in a dotted access chain.
    fn lastChainSegment(chain: []const u8) []const u8 {
        const dot_idx = std.mem.findScalarLast(u8, chain, '.') orelse return chain;
        if (dot_idx + 1 >= chain.len) return chain;
        return chain[dot_idx + 1 ..];
    }

    /// Extract the return type from a type variable.
    /// If the type is a function, returns its return type.
    /// Otherwise returns the type as-is (e.g., for tag constructors that are already the result type).
    fn extractReturnType(module_env: *ModuleEnv, type_var: types.Var) types.Var {
        const type_store = &module_env.types;
        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        // Unwrap aliases first
        var steps: usize = 0;
        while (steps < 8) : (steps += 1) {
            if (std.meta.activeTag(content) != .alias) break;
            const backing_var = type_store.getAliasBackingVar(content.alias);
            resolved = type_store.resolveVar(backing_var);
            content = resolved.desc.content;
        }

        // If it's a function, return the return type
        if (content.unwrapFunc()) |func| {
            return func.ret;
        }

        // Otherwise return as-is (tag constructors, etc.)
        return type_var;
    }

    /// Get completion suggestions at a specific position in a document.
    /// Returns completions from the current module's exposed items and imports.
    /// If the build fails, still provides basic completions (builtin modules, types).
    pub fn getCompletionsAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
    ) QueryError!?completion_handler.CompletionResult {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);

        var build = try self.prepareDocumentBuild(uri, override_text);
        defer build.deinit();

        const env = build.env;

        self.logDebug(.completion, "completion: document {s} reused={}", .{ build.absolute_path, build.reused });
        self.logDebug(.completion, "completion: build_succeeded={}", .{build.build_succeeded});

        const build_has_reports = build.has_reports;

        // Detect completion context from source
        const source = override_text orelse "";
        const context = completion_context.detectCompletionContext(source, line, character);

        // Compute cursor offset for scope-based completions
        const cursor_offset = completion_context.computeOffset(source, line, character);

        // Collect completions based on context
        var items: std.ArrayList(completion_handler.CompletionItem) = .empty;
        errdefer {
            for (items.items) |item| {
                self.allocator.free(item.label);
                if (item.detail) |d| self.allocator.free(d);
                if (item.documentation) |doc| self.allocator.free(doc);
                if (item.sortText) |sort_text| self.allocator.free(sort_text);
                if (item.insertText) |insert_text| self.allocator.free(insert_text);
            }
            items.deinit(self.allocator);
        }

        // Try to get the module environment for richer completions
        // ALWAYS try snapshot first for completion - typing usually produces incomplete code
        var used_snapshot = false;
        // Track which BuildEnv backs the chosen module_env_opt so module member
        // lookups stay consistent with snapshot/previous envs.
        var module_lookup_env: *BuildEnv = env;
        const module_env_opt: ?*ModuleEnv = blk: {
            if (!build_has_reports) {
                if (build.getModuleEnv()) |module_env| {
                    break :blk module_env;
                }
            }

            if (self.snapshot_envs.get(build.absolute_path)) |snapshot_handle| {
                const snapshot_module_env = self.getModuleEnvByPathInEnv(snapshot_handle.envPtr(), build.absolute_path);
                if (snapshot_module_env) |module_env| {
                    used_snapshot = true;
                    module_lookup_env = snapshot_handle.envPtr();
                    break :blk module_env;
                }
            }

            // Fall back to previous build env if snapshot not available
            if (self.previous_build_env) |previous_handle| {
                const prev_module_env = self.getModuleEnvByPathInEnv(previous_handle.envPtr(), build.absolute_path);
                if (prev_module_env) |module_env| {
                    used_snapshot = true;
                    module_lookup_env = previous_handle.envPtr();
                    break :blk module_env;
                }
            }

            // Fall back to current build only if no snapshot available and build succeeded
            if (build.build_succeeded and !build_has_reports) {
                if (self.getModuleEnvByPath(build.absolute_path)) |module_env| {
                    module_lookup_env = env;
                    break :blk module_env;
                }

                module_lookup_env = env;
                break :blk build.getModuleEnv();
            }

            break :blk null;
        };

        self.logDebug(.completion, "completion: context={any}, module_env_opt={any}, build_succeeded={}, used_snapshot={}", .{ context, module_env_opt != null, build.build_succeeded, used_snapshot });

        // Initialize CompletionBuilder for deduplication and organized completion item building
        // Provide the builtin module env so completion can resolve builtin method data.
        var builder = completion_builder.CompletionBuilder.initWithDebug(self.allocator, self.std_io, &items, env.builtin_modules.builtin_module.env, self.debug, self.log_file);
        defer builder.deinit();

        switch (context) {
            .after_module_dot => |module_name| {
                self.logDebug(.completion, "completion: after_module_dot for '{s}'", .{module_name});
                var resolved_module_name = module_name;
                if (module_env_opt) |module_env| {
                    resolved_module_name = resolveModuleAlias(module_env, module_name);
                }
                // Get completions from the specified module
                try builder.addModuleMemberCompletions(module_lookup_env, resolved_module_name, module_env_opt);

                // Always add tag completions for nominal types, not just as fallback.
                // This handles e.g. `Record.` where Record is both a module and a nominal type.
                if (module_env_opt) |module_env| {
                    const added = try builder.addTagCompletionsForNominalType(module_env, module_name, null);
                    if (added) {} else {}
                }
            },
            .after_value_dot => |value_dot| {
                self.logDebug(.completion, "completion: after_value_dot for '{s}' at offset {d}", .{ value_dot.access_chain, value_dot.receiver_segment_start });
                if (module_env_opt) |module_env| {
                    var chain_resolved = false;
                    var chain_oom: ?Allocator.Error = null;
                    if (resolveAccessChainTypeVar(self, &builder, module_env, module_lookup_env, env, value_dot.access_chain, value_dot.chain_start, &chain_oom)) |resolved| {
                        chain_resolved = true;
                        try builder.addFieldsFromTypeVar(resolved.module_env, resolved.type_var);
                        try builder.addTupleIndexCompletions(resolved.module_env, resolved.type_var);
                        try builder.addMethodsFromTypeVar(resolved.module_env, resolved.type_var);
                    }
                    if (chain_oom) |err| return err;

                    // When the chain starts with an uppercase identifier and
                    // type-based traversal fails, try namespace-style member
                    // completion from qualified definition names.
                    var namespace_resolved = false;
                    if (!chain_resolved and value_dot.access_chain.len > 0 and std.ascii.isUpper(value_dot.access_chain[0])) {
                        namespace_resolved = try builder.addNamespaceMemberCompletions(module_env, value_dot.access_chain);
                    }

                    if (!chain_resolved and !namespace_resolved) {
                        const variable_name = lastChainSegment(value_dot.access_chain);
                        const variable_start = value_dot.receiver_segment_start;

                        // A prior complete snapshot may have a field-access node at
                        // this position. Its receiver type remains useful for this
                        // incomplete value-dot prefix; it does not select field or
                        // method semantics. Otherwise, query the preceding expression.
                        var resolved_type_var: ?types.Var = null;
                        if (cir_queries.findFieldAccessReceiverTypeVar(module_env, cursor_offset)) |type_var| {
                            resolved_type_var = type_var;
                        }
                        if (resolved_type_var == null and value_dot.dot_offset > 0) {
                            if (cir_queries.findExprEndingAt(module_env, value_dot.dot_offset)) |type_at| {
                                resolved_type_var = type_at.type_var;
                            }
                        }
                        // When using snapshot, cursor positions don't correspond to snapshot CIR
                        // So we must look up by name instead of analyzing the dot expression
                        if (used_snapshot or resolved_type_var == null) {
                            self.logDebug(.completion, "completion: using name-based lookup (snapshot={}, or receiver-type lookup failed)", .{used_snapshot});
                            try builder.addRecordFieldCompletions(module_env, variable_name, variable_start);
                            self.logDebug(.completion, "completion: after addRecordFieldCompletions, items={d}", .{items.items.len});
                            try builder.addMethodCompletions(module_env, variable_name, variable_start);
                            self.logDebug(.completion, "completion: after addMethodCompletions, items={d}", .{items.items.len});
                        } else if (resolved_type_var) |type_var| {
                            self.logDebug(.completion, "completion: using CIR-based lookup with type_var={}", .{type_var});
                            try builder.addFieldsFromTypeVar(module_env, type_var);
                            try builder.addTupleIndexCompletions(module_env, type_var);
                            try builder.addMethodsFromTypeVar(module_env, type_var);
                        }
                    }
                } else {
                    self.logDebug(.completion, "completion: NO module_env for value-dot completions", .{});
                }
            },
            .after_receiver_dot => |info| {
                // Use CIR to resolve receiver types for chained calls (e.g., value.func().).
                // This avoids brittle text parsing and keeps completion tied to the AST.
                if (module_env_opt) |module_env| {
                    // A prior complete snapshot may have a field access at this
                    // position. Use only its receiver type for the unfinished
                    // prefix; field and method candidates remain independent.
                    var resolved_type_var: ?types.Var = null;
                    if (cir_queries.findFieldAccessReceiverTypeVar(module_env, cursor_offset)) |type_var| {
                        resolved_type_var = type_var;
                    }
                    if (resolved_type_var == null and info.dot_offset > 0) {
                        if (cir_queries.findExprEndingAt(module_env, info.dot_offset)) |type_at| {
                            resolved_type_var = type_at.type_var;
                        }
                    }

                    if (used_snapshot or resolved_type_var == null) {
                        // CIR-based lookup failed or used snapshot (offsets don't match).
                        // Fall back to resolving the call chain textually.
                        if (info.call_chain) |call_chain| {
                            self.logDebug(.completion, "completion: after_receiver_dot fallback using call_chain='{s}'", .{call_chain});
                            var chain_oom: ?Allocator.Error = null;
                            if (resolveAccessChainTypeVar(self, &builder, module_env, module_lookup_env, env, call_chain, info.chain_start, &chain_oom)) |resolved| {
                                const ret_type = extractReturnType(resolved.module_env, resolved.type_var);
                                try builder.addFieldsFromTypeVar(resolved.module_env, ret_type);
                                try builder.addTupleIndexCompletions(resolved.module_env, ret_type);
                                try builder.addMethodsFromTypeVar(resolved.module_env, ret_type);
                            }
                            if (chain_oom) |err| return err;
                        } else if (resolved_type_var) |type_var| {
                            try builder.addFieldsFromTypeVar(module_env, type_var);
                            try builder.addTupleIndexCompletions(module_env, type_var);
                            try builder.addMethodsFromTypeVar(module_env, type_var);
                        } else {
                            self.logDebug(.completion, "completion: after_receiver_dot no CIR receiver type found and no call_chain", .{});
                        }
                    } else if (resolved_type_var) |type_var| {
                        self.logDebug(.completion, "completion: after_receiver_dot using CIR type_var={}", .{type_var});
                        try builder.addFieldsFromTypeVar(module_env, type_var);
                        try builder.addTupleIndexCompletions(module_env, type_var);
                        try builder.addMethodsFromTypeVar(module_env, type_var);
                    }
                } else {
                    self.logDebug(.completion, "completion: NO module_env for receiver dot completions", .{});
                }
            },
            .after_colon => {
                // Type annotation context - add type names
                if (module_env_opt) |module_env| {
                    try builder.addTypeCompletions(module_env);
                    try builder.addModuleNameCompletions(module_env);
                }
                try builder.addTypeCompletionsFromEnv(env);
                try builder.addModuleNameCompletionsFromEnv(env);
            },
            .expression => {
                // General expression context - add local definitions + module names + structural tags + nominal types
                if (module_env_opt) |module_env| {
                    try builder.addLocalCompletions(module_env, cursor_offset);
                    try builder.addModuleNameCompletions(module_env);
                    try builder.addAmbientTagCompletions(module_env);
                    try builder.addTypeCompletions(module_env);
                }
                try builder.addModuleNameCompletionsFromEnv(env);
            },
        }

        // Keep completion payloads bounded so request/response processing
        // remains robust even when environments expose very large symbol sets.
        const max_completion_items: usize = 512;
        if (items.items.len > max_completion_items) {
            // Free the allocated strings in items being dropped
            for (items.items[max_completion_items..]) |item| {
                self.allocator.free(item.label);
                if (item.detail) |d| self.allocator.free(d);
                if (item.documentation) |doc| self.allocator.free(doc);
                if (item.sortText) |sort_text| self.allocator.free(sort_text);
                if (item.insertText) |insert_text| self.allocator.free(insert_text);
            }
            items.items.len = max_completion_items;
        }

        return .{
            .items = try items.toOwnedSlice(self.allocator),
            .is_incomplete = false,
        };
    }
};

const completion_handler = @import("handlers/completion.zig");
const document_symbol_handler = @import("handlers/document_symbol.zig");
const pos = @import("position.zig");

// Position utilities moved to position.zig

fn appendOwnedSymbol(
    allocator: Allocator,
    symbols: *std.ArrayList(document_symbol_handler.SymbolInformation),
    symbol: document_symbol_handler.SymbolInformation,
) Allocator.Error!void {
    const owned_name = try allocator.dupe(u8, symbol.name);
    try symbols.append(allocator, .{
        .name = owned_name,
        .kind = symbol.kind,
        .location = .{
            .uri = symbol.location.uri,
            .range = symbol.location.range,
        },
    });
}

fn symbolFromRegion(
    name: []const u8,
    kind: document_symbol_handler.SymbolKind,
    uri: []const u8,
    region: Region,
    line_offsets: *const pos.LineOffsets,
) document_symbol_handler.SymbolInformation {
    return .{
        .name = name,
        .kind = kind,
        .location = .{
            .uri = uri,
            .range = .{
                .start = pos.offsetToPosition(region.start.offset, line_offsets),
                .end = pos.offsetToPosition(region.end.offset, line_offsets),
            },
        },
    };
}

fn extractSymbolFromPattern(
    module_env: *ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    uri: []const u8,
    line_offsets: *const pos.LineOffsets,
    kind: document_symbol_handler.SymbolKind,
) ?document_symbol_handler.SymbolInformation {
    const ident_idx = module_lookup.extractIdentFromPattern(&module_env.store, pattern_idx) orelse return null;
    const name = module_env.getIdentText(ident_idx);
    if (name.len == 0) return null;

    return symbolFromRegion(
        name,
        kind,
        uri,
        module_env.store.getPatternRegion(pattern_idx),
        line_offsets,
    );
}

fn extractSymbolFromTypeDecl(
    module_env: *ModuleEnv,
    header_idx: CIR.TypeHeader.Idx,
    stmt_idx: CIR.Statement.Idx,
    uri: []const u8,
    line_offsets: *const pos.LineOffsets,
    kind: document_symbol_handler.SymbolKind,
) ?document_symbol_handler.SymbolInformation {
    const header = module_env.store.getTypeHeader(header_idx);
    const name = module_env.getIdentText(header.relative_name);
    if (name.len == 0) return null;

    return symbolFromRegion(
        name,
        kind,
        uri,
        module_env.store.getStatementRegion(stmt_idx),
        line_offsets,
    );
}

fn extractSymbolFromDecl(
    module_env: *ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    _: []const u8,
    uri: []const u8,
    line_offsets: *const pos.LineOffsets,
) ?document_symbol_handler.SymbolInformation {
    // Check if RHS is a function
    const expr = module_env.store.getExpr(expr_idx);
    const expr_tag = std.meta.activeTag(expr);
    const is_function = expr_tag == .e_closure or expr_tag == .e_lambda or expr_tag == .e_hosted_lambda;

    return extractSymbolFromPattern(
        module_env,
        pattern_idx,
        uri,
        line_offsets,
        if (is_function) .function else .variable,
    );
}

/// The binding a rename request refers to.
const RenameTarget = struct {
    pattern: CIR.Pattern.Idx,
    ident: base.Ident.Idx,
};

/// Resolve a source offset to a binding that can be renamed.
///
/// Only a plain `assign` pattern names a single binding whose every occurrence
/// this server can account for. Destructuring, literal, and tag patterns bind
/// through structure that a name-for-name rewrite cannot express, so they are
/// refused rather than half-renamed.
fn renameTargetAt(module_env: *ModuleEnv, offset: u32) ?RenameTarget {
    const pattern_idx = cir_queries.resolveSymbolAtOffset(module_env, offset) orelse return null;
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign => |assign| .{ .pattern = pattern_idx, .ident = assign.ident },
        .as,
        .applied_tag,
        .nominal,
        .nominal_external,
        .record_destructure,
        .list,
        .tuple,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .num_from_numeral_literal,
        .str_literal,
        .str_interpolation,
        .underscore,
        .runtime_error,
        => null,
    };
}

/// Find the collected range that contains the given position.
fn rangeContaining(regions: []const cir_queries.LspRange, line: u32, character: u32) ?cir_queries.LspRange {
    for (regions) |range| {
        if (range.start_line != line or range.end_line != line) continue;
        if (character >= range.start_col and character <= range.end_col) return range;
    }
    return null;
}

//! Syntax checking integration that runs the Roc compiler and converts
//! reports to LSP diagnostics.

const std = @import("std");
const compile = @import("compile");
const reporting = @import("reporting");
const build_options = @import("build_options");
const Filesystem = @import("fs").Filesystem;
const Allocator = std.mem.Allocator;
const base = @import("base");
const can = @import("can");
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

const BuildEnv = compile.BuildEnv;
const CacheManager = compile.CacheManager;
const roc_target = @import("roc_target");
const CacheConfig = compile.CacheConfig;
const FileProvider = compile.package.FileProvider;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Region = base.Region;

/// Flags allowing granular debugging
pub const DebugFlags = struct {
    build: bool = false,
    syntax: bool = false,
    server: bool = false,
    completion: bool = false,
};

/// Runs BuildEnv-backed syntax/type checks and converts reports to LSP diagnostics.
pub const SyntaxChecker = struct {
    allocator: std.mem.Allocator,
    mutex: std.Thread.Mutex = .{},
    /// Current build environment owned by the live check path.
    build_env: ?*BuildEnvHandle = null,
    /// Previous successful BuildEnv kept for module lookups (e.g., semantic tokens).
    /// This is swapped with build_env after each successful build.
    previous_build_env: ?*BuildEnvHandle = null,
    /// Snapshot of the most recent successful build per module (kept for completions).
    snapshot_envs: std.StringHashMapUnmanaged(*BuildEnvHandle) = .{},
    /// Dependency graph for tracking module relationships and invalidation.
    dependency_graph: DependencyGraph,
    cache_config: CacheConfig = .{},
    log_file: ?std.fs.File = null,
    debug: DebugFlags,

    // Owner tags used for BuildEnvHandle debugging.
    const owner_build = "build_env";
    const owner_previous = "previous_build_env";
    const owner_snapshot = "snapshot";

    pub fn init(allocator: std.mem.Allocator, debug: DebugFlags, log_file: ?std.fs.File) SyntaxChecker {
        return .{
            .allocator = allocator,
            .dependency_graph = DependencyGraph.init(allocator),
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

        self.dependency_graph.deinit();
    }

    /// Check the file referenced by the URI and return diagnostics grouped by URI.
    pub fn check(self: *SyntaxChecker, uri: []const u8, override_text: ?[]const u8, workspace_root: ?[]const u8) ![]Diagnostics.PublishDiagnostics {
        _ = workspace_root; // Reserved for future use

        self.mutex.lock();
        defer self.mutex.unlock();

        // Check if content has changed using hash comparison BEFORE building.
        // This avoids unnecessary rebuilds on focus/blur events.
        if (override_text) |text| {
            const path = uri_util.uriToPath(self.allocator, uri) catch null;
            defer if (path) |p| self.allocator.free(p);

            const abs_path = if (path) |p|
                std.fs.cwd().realpathAlloc(self.allocator, p) catch self.allocator.dupe(u8, p) catch null
            else
                null;
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

                self.dependency_graph.setContentHash(ap, new_hash) catch |err| {
                    self.logDebug(.build, "Failed to set content hash: {s}", .{@errorName(err)});
                };
            }
        }

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, env, uri, override_text);
        defer session.deinit();

        const absolute_path = session.absolute_path;

        // Update dependency graph from successful build
        self.updateDependencyGraph(env);

        var publish_list = std.ArrayList(Diagnostics.PublishDiagnostics){};
        errdefer {
            for (publish_list.items) |*set| set.deinit(self.allocator);
            publish_list.deinit(self.allocator);
        }
        if (session.drained_reports) |drained_reports| {
            // if the build succeeded, consider snapshotting the BuildEnv for completions
            if (self.shouldSnapshotBuild(env, session.absolute_path, drained_reports)) {
                self.storeSnapshotEnv(env_handle, session.absolute_path);
            }
            for (drained_reports) |entry| {
                const mapped_path = if (entry.abs_path.len == 0) session.absolute_path else entry.abs_path;
                const module_uri = try uri_util.pathToUri(self.allocator, mapped_path);

                var diags = std.ArrayList(Diagnostics.Diagnostic){};
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
            return publish_list.toOwnedSlice(self.allocator);
        }
    }

    /// Creates a fresh BuildEnv for a new build.
    /// The previous build_env is moved to previous_build_env for module lookups.
    fn createFreshBuildEnv(self: *SyntaxChecker) !*BuildEnvHandle {
        self.logDebug(.build, "createFreshBuildEnv: prev_build_env={any} build_env={any}", .{ self.previous_build_env != null, self.build_env != null });

        // Release the previous_build_env owner first.
        if (self.previous_build_env) |old_prev| {
            old_prev.release(owner_previous);
        }

        // Move build_env to previous_build_env, transferring ownership tag.
        if (self.build_env) |current| {
            current.retain(owner_previous);
            current.release(owner_build);
            self.previous_build_env = current;
            self.build_env = null;
        }

        // Create a fresh BuildEnv
        var env = try BuildEnv.init(self.allocator, .single_threaded, 1, roc_target.RocTarget.detectNative());
        env.compiler_version = build_options.compiler_version;

        if (self.cache_config.enabled) {
            const cache_manager = try self.allocator.create(CacheManager);
            cache_manager.* = CacheManager.init(self.allocator, self.cache_config, Filesystem.default());
            env.setCacheManager(cache_manager);
        }

        const debug_handles = self.debug.build or self.debug.syntax or self.debug.server;
        const handle = try BuildEnvHandle.create(self.allocator, env, owner_build, debug_handles);
        self.build_env = handle;
        return handle;
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
                        .info, .warning => {},
                    }
                }
            }
        }

        // Module processed with no reports → snapshot
        return true;
    }

    fn storeSnapshotEnv(self: *SyntaxChecker, env_handle: *BuildEnvHandle, absolute_path: []const u8) void {
        self.logDebug(.completion, "storeSnapshotEnv: path={s}", .{absolute_path});
        if (self.snapshot_envs.fetchRemove(absolute_path)) |removed| {
            self.logDebug(.completion, "storeSnapshotEnv: replacing existing snapshot", .{});
            removed.value.release(owner_snapshot);
            self.allocator.free(removed.key);
        }

        const owned_path = self.allocator.dupe(u8, absolute_path) catch return;
        self.snapshot_envs.put(self.allocator, owned_path, env_handle) catch {
            self.allocator.free(owned_path);
            return;
        };
        env_handle.retain(owner_snapshot);
        self.logDebug(.completion, "storeSnapshotEnv: stored snapshot count={d}", .{self.snapshot_envs.count()});
    }

    fn clearSnapshots(self: *SyntaxChecker) void {
        // Collect all handles and keys before clearing the map so we can
        // release snapshot ownership without mutating the map mid-iteration.
        var envs: std.ArrayListUnmanaged(*BuildEnvHandle) = .{};
        defer envs.deinit(self.allocator);
        var keys: std.ArrayListUnmanaged([]const u8) = .{};
        defer keys.deinit(self.allocator);

        var it = self.snapshot_envs.iterator();
        while (it.next()) |entry| {
            envs.append(self.allocator, entry.value_ptr.*) catch {};
            keys.append(self.allocator, entry.key_ptr.*) catch {};
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
            if (env.schedulers.count() > 0) {
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
        // Iterate through all schedulers (packages)
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            // Iterate through all modules in this package
            for (sched.modules.items) |*module_state| {
                if (std.mem.eql(u8, module_state.path, path)) {
                    if (module_state.env) |*mod_env| {
                        return mod_env;
                    }
                }
            }
        }
        return null;
    }

    /// Get all imported ModuleEnvs for a given module.
    /// Returns a slice of ModuleEnv pointers for the module's imports.
    /// Caller must free the returned slice.
    pub fn getImportedModuleEnvs(self: *SyntaxChecker, module_path: []const u8) !?[]*ModuleEnv {
        const env = self.getModuleLookupEnv() orelse return null;

        // First, find the module and its scheduler
        var target_sched: ?*compile.package.PackageEnv = null;
        var target_module_imports: ?[]const u32 = null;

        var sched_it = env.schedulers.iterator();
        outer: while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                if (std.mem.eql(u8, module_state.path, module_path)) {
                    target_sched = sched;
                    target_module_imports = module_state.imports.items;
                    break :outer;
                }
            }
        }

        const sched = target_sched orelse return null;
        const imports = target_module_imports orelse return null;

        // Collect ModuleEnvs for all imports
        var imported_envs: std.ArrayListUnmanaged(*ModuleEnv) = .{};
        errdefer imported_envs.deinit(self.allocator);

        // Local imports (within same package)
        for (imports) |import_id| {
            if (import_id < sched.modules.items.len) {
                const imported_module = &sched.modules.items[import_id];
                if (imported_module.env) |*imp_env| {
                    try imported_envs.append(self.allocator, imp_env);
                }
            }
        }

        // TODO: Handle external_imports (cross-package) when needed

        return try imported_envs.toOwnedSlice(self.allocator);
    }

    /// Free drained reports. If `free_reports` is true, also deinit each report.
    /// The `check` function processes reports itself, so uses free_reports=false.
    /// Other functions like `getDefinitionAtPosition` don't process reports, so use free_reports=true.
    fn freeDrainedEx(self: *SyntaxChecker, drained: []BuildEnv.DrainedModuleReports, free_reports: bool) void {
        for (drained) |*entry| {
            self.allocator.free(entry.abs_path);
            if (free_reports) {
                // Free the reports themselves - each Report has owned allocations
                for (entry.reports) |*report| {
                    @constCast(report).deinit();
                }
                self.allocator.free(entry.reports);
            }
        }
        self.allocator.free(drained);
    }

    fn freeDrainedWithReports(self: *SyntaxChecker, drained: []BuildEnv.DrainedModuleReports) void {
        // Free reports too (for functions that don't process reports)
        self.freeDrainedEx(drained, true);
    }

    /// Update the dependency graph from a successful build.
    fn updateDependencyGraph(self: *SyntaxChecker, env: *BuildEnv) void {
        self.logDebug(.build, "[DEPS] Updating dependency graph...", .{});

        // Clear only relationships, preserving content/exports hashes for incremental detection
        self.dependency_graph.clearRelationships();

        var total_modules: usize = 0;
        var exports_computed: usize = 0;

        // Iterate through all schedulers (packages) and build the graph
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const pkg_name = entry.key_ptr.*;
            const sched = entry.value_ptr.*;

            self.logDebug(.build, "[DEPS] Processing package '{s}' with {d} modules", .{ pkg_name, sched.modules.items.len });

            self.dependency_graph.buildFromPackageEnv(sched) catch |err| {
                self.logDebug(.build, "[DEPS] Failed to build dependency graph for '{s}': {s}", .{ pkg_name, @errorName(err) });
                continue;
            };

            // Compute and store exports hash for each module with a valid ModuleEnv
            for (sched.modules.items) |*module_state| {
                total_modules += 1;

                if (module_state.env) |*module_env| {
                    const new_exports_hash = DependencyGraph.computeExportsHash(self.allocator, module_env) catch |err| {
                        self.logDebug(.build, "[DEPS] Failed to compute exports hash for {s}: {s}", .{ module_state.path, @errorName(err) });
                        continue;
                    };

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

    fn reportToDiagnostic(self: *SyntaxChecker, rep: reporting.Report) !Diagnostics.Diagnostic {
        const range = self.rangeFromReport(rep);
        const severity: u32 = switch (rep.severity) {
            .warning => 2,
            .info => 3,
            .runtime_error, .fatal => 1,
        };

        var writer: std.io.Writer.Allocating = .init(self.allocator);
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
                else => {},
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
        log_file.writeAll(msg) catch return;
        log_file.writeAll("\n") catch {};
        log_file.sync() catch {};
    }

    /// Temporary suppression to avoid noisy undefined-variable diagnostics from BuildEnv.
    fn shouldSuppressReport(_: *SyntaxChecker, rep: reporting.Report) bool {
        if (!std.mem.startsWith(u8, rep.title, "UNDEFINED VARIABLE")) return false;

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
            else => {},
        }
        return false;
    }

    fn textHasAny(text: []const u8, needles: []const []const u8) bool {
        for (needles) |needle| {
            if (std.mem.indexOf(u8, text, needle) != null) return true;
        }
        return false;
    }

    const OverrideProvider = struct {
        override_path: []const u8,
        override_text: ?[]const u8,

        fn read(ctx: ?*anyopaque, path: []const u8, gpa: std.mem.Allocator) Allocator.Error!?[]u8 {
            const self: *OverrideProvider = @ptrCast(@alignCast(ctx.?));
            if (std.mem.eql(u8, path, self.override_path)) {
                if (self.override_text) |text| {
                    return try gpa.dupe(u8, text);
                }
            }
            // Fall back to filesystem for non-override files (e.g., platform modules)
            return FileProvider.filesystem.read(null, path, gpa);
        }
    };

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
    ) !?HoverResult {
        self.mutex.lock();
        defer self.mutex.unlock();

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, env, uri, override_text);
        defer session.deinit();

        self.logDebug(.build, "hover: building {s}", .{session.absolute_path});

        if (!session.build_succeeded) {
            self.logDebug(.build, "hover: build failed for {s}", .{session.absolute_path});
            return null;
        }

        // Get module environment
        const module_env = session.getModuleEnv() orelse return null;

        // Convert LSP position (0-based line/col) to byte offset
        // LSP uses 0-based line and UTF-16 code units for character
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        // Find the expression at this position
        const result = cir_queries.findTypeAtOffset(module_env, target_offset) orelse return null;

        // Prefer lookup expression semantics when hovering over identifiers in
        // calls (e.g. `multiply(…)`): callers expect the callee's function type
        // and docs, not the enclosing call expression's return type.
        var lookup_expr_idx_opt = cir_queries.findLookupAtOffset(module_env, target_offset);
        if (lookup_expr_idx_opt == null and result.region.start.offset != target_offset) {
            lookup_expr_idx_opt = cir_queries.findLookupAtOffset(module_env, result.region.start.offset);
        }

        var hover_type_var = if (lookup_expr_idx_opt) |lookup_expr_idx|
            ModuleEnv.varFrom(lookup_expr_idx)
        else
            result.type_var;

        // Optional textual override for hover type rendering. When we can
        // resolve an explicit annotation for a symbol, prefer that exact text.
        var hover_type_text_opt: ?[]const u8 = null;

        // Format the type as a string
        var type_writer = try module_env.initTypeWriter();
        defer type_writer.deinit();

        try type_writer.write(hover_type_var, .one_line);
        const type_str = type_writer.get();

        // Extract documentation for the definition/pattern at this location.
        // When we already have a lookup expression, resolve directly to avoid
        // region/offset ambiguity around delimiters.
        var documentation = if (lookup_expr_idx_opt) |lookup_expr_idx|
            self.resolveDocForLookup(env, module_env, lookup_expr_idx)
        else
            try self.findDocumentationForRegion(env, module_env, result.region, target_offset);

        // Final fallback: reuse definition-resolution to recover the symbol at
        // call sites where direct lookup queries can miss the identifier region.
        // This keeps hover aligned with go-to-definition behavior.
        if (documentation == null) {
            if (self.findDefinitionAtOffset(module_env, target_offset, uri)) |def_loc| {
                if (std.mem.eql(u8, def_loc.uri, uri)) {
                    if (pos.positionToOffset(module_env, def_loc.range.start_line, def_loc.range.start_col)) |def_offset| {
                        if (cir_queries.findPatternAtOffset(module_env, def_offset)) |pattern_idx| {
                            hover_type_var = ModuleEnv.varFrom(pattern_idx);
                            documentation = doc_comments.extractDocCommentBefore(
                                self.allocator,
                                module_env.common.source,
                                module_env.store.getPatternRegion(pattern_idx).start.offset,
                            ) catch null;
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

                    const extracted = doc_comments.extractDocForDef(
                        self.allocator,
                        module_env.common.source,
                        &module_env.store,
                        def,
                    ) catch documentation;
                    if (extracted != null) {
                        if (documentation) |doc| self.allocator.free(doc);
                        documentation = extracted;
                    }
                } else if (module_lookup.findStatementOwningPattern(module_env, def_info.pattern_idx)) |stmt_owner| {
                    const extracted = doc_comments.extractDocForStatement(
                        self.allocator,
                        module_env.common.source,
                        &module_env.store,
                        stmt_owner.stmt,
                        stmt_owner.idx,
                    ) catch documentation;
                    if (extracted != null) {
                        if (documentation) |doc| self.allocator.free(doc);
                        documentation = extracted;
                    }
                } else {
                    const extracted = doc_comments.extractDocCommentBefore(
                        self.allocator,
                        module_env.common.source,
                        module_env.store.getPatternRegion(def_info.pattern_idx).start.offset,
                    ) catch documentation;
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
    fn findDocumentationForRegion(self: *SyntaxChecker, env: *BuildEnv, module_env: *ModuleEnv, region: Region, target_offset: u32) !?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;

        // First, check if this is a lookup expression (e.g., a function call)
        // If so, resolve it to the definition and extract docs from there
        if (cir_queries.findLookupAtOffset(module_env, target_offset)) |expr_idx| {
            if (self.resolveDocForLookup(env, module_env, expr_idx)) |doc| return doc;
        }

        // Hover positions can land on delimiters around the symbol (e.g. `(` in
        // `foo(...)`) depending on UTF-16 cursor conversion. As a fallback, try
        // the start of the selected type region as an anchor for lookup-based
        // documentation resolution.
        if (region.start.offset != target_offset) {
            if (cir_queries.findLookupAtOffset(module_env, region.start.offset)) |expr_idx| {
                if (self.resolveDocForLookup(env, module_env, expr_idx)) |doc| return doc;
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
                return doc_comments.extractDocForDef(self.allocator, source, store, def) catch null;
            }

            // Also check if the expression region matches (for hovering over expressions)
            const expr_region = store.getExprRegion(def.expr);
            if (cir_queries.regionContainsOffset(expr_region, region.start.offset)) {
                return doc_comments.extractDocForDef(self.allocator, source, store, def) catch null;
            }
        }

        // Check statements
        const statements_slice = store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = store.getStatement(stmt_idx);
            const stmt_region = store.getStatementRegion(stmt_idx);

            if (cir_queries.regionContainsOffset(stmt_region, region.start.offset)) {
                return doc_comments.extractDocForStatement(self.allocator, source, store, stmt, stmt_idx) catch null;
            }
        }

        return null;
    }

    /// Resolve documentation for a lookup expression (local, external, or dot access).
    fn resolveDocForLookup(self: *SyntaxChecker, env: *BuildEnv, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) ?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;
        const expr = store.getExpr(expr_idx);

        switch (expr) {
            .e_lookup_local => |lookup| {
                // Local lookup - resolve to the owning def or statement
                if (module_lookup.findDefOwningPattern(module_env, lookup.pattern_idx)) |def| {
                    return doc_comments.extractDocForDef(self.allocator, source, store, def) catch null;
                }
                if (module_lookup.findStatementOwningPattern(module_env, lookup.pattern_idx)) |result| {
                    return doc_comments.extractDocForStatement(self.allocator, source, store, result.stmt, result.idx) catch null;
                }

                // Some local bindings are nested inside expressions (e.g. block
                // locals) and are not owned by top-level defs/statements. Fall
                // back to doc extraction directly from the bound pattern region.
                return doc_comments.extractDocCommentBefore(
                    self.allocator,
                    source,
                    store.getPatternRegion(lookup.pattern_idx).start.offset,
                ) catch null;
            },
            .e_lookup_external => |lookup| {
                // External lookup - parse "Module.function" and find docs in that module
                const region_text = module_env.getSource(lookup.region);
                if (std.mem.indexOf(u8, region_text, ".")) |dot_pos| {
                    const module_name = region_text[0..dot_pos];
                    const function_name = region_text[dot_pos + 1 ..];

                    if (findExternalModuleEnv(env, module_name)) |external_env| {
                        return findDocInModule(self.allocator, external_env, function_name);
                    }
                }
            },
            .e_lookup_pending => {
                // Pending lookups can still be resolved for hover by symbol text.
                const lookup_region = store.getExprRegion(expr_idx);
                const region_text = module_env.getSource(lookup_region);

                // Try local resolution first (covers local functions/values).
                if (findDocInModule(self.allocator, module_env, region_text)) |doc| {
                    return doc;
                }

                // If the pending lookup is qualified, try external module docs.
                if (std.mem.indexOfScalar(u8, region_text, '.')) |dot_pos| {
                    const module_name = region_text[0..dot_pos];
                    const function_name = region_text[dot_pos + 1 ..];
                    if (findExternalModuleEnv(env, module_name)) |external_env| {
                        return findDocInModule(self.allocator, external_env, function_name);
                    }
                }
            },
            .e_dot_access => |dot| {
                // Method call - resolve receiver type to find the providing module
                const field_name = module_env.getSource(dot.field_name_region);
                const receiver_type_var = ModuleEnv.varFrom(dot.receiver);
                if (resolveTypeIdentForMethodLookup(module_env, receiver_type_var)) |type_ident| {
                    // Prefer local method docs first (e.g. static-dispatch methods
                    // defined in the current module), then fall back to external
                    // module lookup for builtin/qualified providers.
                    if (findMethodDocForTypeAndName(self.allocator, module_env, type_ident, field_name)) |local_doc| {
                        return local_doc;
                    }

                    const type_name = module_env.getIdentText(type_ident);
                    if (findExternalModuleEnv(env, type_name)) |external_env| {
                        const qualified_name = std.fmt.allocPrint(
                            self.allocator,
                            "{s}.{s}",
                            .{ type_name, field_name },
                        ) catch return null;
                        defer self.allocator.free(qualified_name);
                        return findDocInModule(self.allocator, external_env, qualified_name);
                    }
                }
            },
            else => {},
        }
        return null;
    }

    /// Resolve a nominal type identifier for method lookup from a receiver type var.
    ///
    /// This follows aliases/nominal wrappers so hover can map `value.method()` to
    /// the `(type_ident, method_ident)` entries in `method_idents`.
    fn resolveTypeIdentForMethodLookup(module_env: *ModuleEnv, type_var: types.Var) ?base.Ident.Idx {
        const resolved = module_env.types.resolveVar(type_var);
        switch (resolved.desc.content) {
            // Aliases carry a nominal ident that can participate in
            // method_idents lookup.
            .alias => |alias| return @as(?base.Ident.Idx, alias.ident.ident_idx),
            .structure => |flat_type| {
                switch (flat_type) {
                    .nominal_type => |nominal| return @as(?base.Ident.Idx, nominal.ident.ident_idx),
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Find local method documentation by `(type_ident, method_name)`.
    fn findMethodDocForTypeAndName(
        allocator: Allocator,
        module_env: *ModuleEnv,
        type_ident: base.Ident.Idx,
        method_name: []const u8,
    ) ?[]const u8 {
        const entries = module_env.method_idents.entries.items;
        for (entries) |entry| {
            if (entry.key.type_ident != type_ident) continue;

            const entry_method_name = module_env.getIdentText(entry.key.method_ident);
            if (!std.mem.eql(u8, entry_method_name, method_name)) continue;

            return findDocForQualifiedIdent(allocator, module_env, entry.value);
        }

        return null;
    }

    /// Find a module environment by name (handles builtins and regular modules).
    fn findExternalModuleEnv(env: *BuildEnv, module_name: []const u8) ?*ModuleEnv {
        const base_name = if (std.mem.lastIndexOf(u8, module_name, ".")) |dot_pos|
            module_name[dot_pos + 1 ..]
        else
            module_name;

        // Check builtins first
        if (completion_builtins.isBuiltinType(base_name)) {
            return env.builtin_modules.builtin_module.env;
        }

        // Delegate to module_lookup for scheduler-based lookup
        if (module_lookup.findModuleByName(env, module_name)) |info| {
            return info.module_env;
        }
        return null;
    }

    /// Find documentation for a definition by name in a module.
    /// Uses module_lookup infrastructure for the search, with qualified-name fallback.
    fn findDocInModule(allocator: Allocator, module_env: *ModuleEnv, name: []const u8) ?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;

        // Use module_lookup to find by exact or unqualified name
        if (module_lookup.findDefinitionByUnqualifiedName(module_env, name)) |def_info| {
            // Try to find the full Def for annotation-aware offset
            if (module_lookup.findDefOwningPattern(module_env, def_info.pattern_idx)) |def| {
                return doc_comments.extractDocForDef(allocator, source, store, def) catch null;
            }
            // Fall back to statement-based extraction
            if (module_lookup.findStatementOwningPattern(module_env, def_info.pattern_idx)) |result| {
                return doc_comments.extractDocForStatement(allocator, source, store, result.stmt, result.idx) catch null;
            }
            // Last resort: use pattern region directly
            return doc_comments.extractDocCommentBefore(
                allocator,
                source,
                store.getPatternRegion(def_info.pattern_idx).start.offset,
            ) catch null;
        }
        return null;
    }

    /// Find documentation for a specific qualified identifier in a module.
    fn findDocForQualifiedIdent(allocator: Allocator, module_env: *ModuleEnv, qualified_ident: base.Ident.Idx) ?[]const u8 {
        const source = module_env.common.source;
        const store = &module_env.store;

        // Search defs first.
        const defs_slice = store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = store.getDef(def_idx);
            const pattern = store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return doc_comments.extractDocForDef(allocator, source, store, def) catch null;
            }
        }

        // Fall back to statements.
        const statements_slice = store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = store.getStatement(stmt_idx);
            const pattern_idx = switch (stmt) {
                .s_decl => |decl| decl.pattern,
                else => continue,
            };

            const pattern = store.getPattern(pattern_idx);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return doc_comments.extractDocForStatement(allocator, source, store, stmt, stmt_idx) catch null;
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
    ) !?DefinitionResult {
        self.mutex.lock();
        defer self.mutex.unlock();

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, env, uri, override_text);
        defer session.deinit();

        self.logDebug(.build, "definition: building {s}", .{session.absolute_path});

        if (!session.build_succeeded) {
            self.logDebug(.build, "definition: build failed for {s}", .{session.absolute_path});
            return null;
        }

        // Get module environment
        const module_env = session.getModuleEnv() orelse return null;

        // Convert LSP position to byte offset
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        // Find the definition at this position
        const result = self.findDefinitionAtOffset(module_env, target_offset, uri) orelse return null;

        return result;
    }

    // positionToOffset moved to position.zig

    // regionToRange moved to cir_queries module

    /// Find the definition location for the expression at the given byte offset.
    /// Looks for lookups (e_lookup_local, e_lookup_external) and returns the definition location.
    fn findDefinitionAtOffset(self: *SyntaxChecker, module_env: *ModuleEnv, target_offset: u32, current_uri: []const u8) ?DefinitionResult {
        var best_expr: ?CIR.Expr.Idx = null;
        var best_size: u32 = std.math.maxInt(u32);

        // Iterate through all definitions
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);

            // Check type annotation on this definition
            if (def.annotation) |anno_idx| {
                const annotation = module_env.store.getAnnotation(anno_idx);
                if (self.findTypeAnnoAtOffset(module_env, annotation.anno, target_offset)) |result| {
                    // If URI is empty, it's a local type - use current file
                    if (result.uri.len == 0) {
                        return DefinitionResult{
                            .uri = current_uri,
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
                if (self.findTypeAnnoInExpr(module_env, expr_idx, target_offset, current_uri)) |result| {
                    return result;
                }
                // Then search for lookup expressions
                if (cir_queries.findLookupAtOffset(module_env, target_offset)) |found| {
                    best_expr = found;
                    best_size = 0; // found the narrowest match
                }
            }
        }

        // Also check statements
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);

            // Handle import statements specially - navigate to the imported module
            if (stmt == .s_import) {
                const import_stmt = stmt.s_import;
                const stmt_region = module_env.store.getStatementRegion(stmt_idx);

                if (cir_queries.regionContainsOffset(stmt_region, target_offset)) {
                    // Get the module name from the import
                    const module_name = module_env.common.idents.getText(import_stmt.module_name_tok);

                    // Try to find the module in the schedulers
                    if (self.findModuleByName(module_name)) |result| {
                        return result;
                    }
                }
            }

            // Check type annotations in statements
            const maybe_type_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                .s_decl => |d| if (d.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
                .s_var => |d| if (d.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
                .s_type_anno => |t| t.anno,
                .s_alias_decl => |a| a.anno,
                .s_nominal_decl => |n| n.anno,
                else => null,
            };

            if (maybe_type_anno) |type_anno_idx| {
                if (self.findTypeAnnoAtOffset(module_env, type_anno_idx, target_offset)) |result| {
                    // If URI is empty, it's a local type - use current file
                    if (result.uri.len == 0) {
                        return DefinitionResult{
                            .uri = current_uri,
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
                        best_expr = found;
                        best_size = 0;
                    }
                }
            }

            if (stmt_parts.expr2) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (cir_queries.regionContainsOffset(expr_region, target_offset)) {
                    if (cir_queries.findLookupAtOffset(module_env, target_offset)) |found| {
                        best_expr = found;
                        best_size = 0;
                    }
                }
            }
        }

        // If we found a lookup expression, resolve it to a definition
        if (best_expr) |expr_idx| {
            const expr = module_env.store.getExpr(expr_idx);
            switch (expr) {
                .e_lookup_local => |lookup| {
                    // Get the pattern's region - that's where it's defined
                    const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(lookup.pattern_idx));
                    const def_region = module_env.store.getRegionAt(pattern_node_idx);
                    const range = cir_queries.regionToRange(module_env, def_region) orelse return null;
                    return DefinitionResult{
                        .uri = current_uri,
                        .range = range,
                    };
                },
                .e_lookup_external => |lookup| {
                    // External lookup - resolve to the module file
                    // Extract module name from source text (handles builtins correctly)
                    const region_text = module_env.getSource(lookup.region);
                    // Module.function format - extract the module name (before the dot)
                    if (std.mem.indexOf(u8, region_text, ".")) |dot_pos| {
                        const module_name = region_text[0..dot_pos];
                        self.logDebug(.build, "[DEF] e_lookup_external: extracted module='{s}' from '{s}'", .{ module_name, region_text });
                        return self.findModuleByName(module_name);
                    }
                    self.logDebug(.build, "[DEF] e_lookup_external: could not extract module name from '{s}'", .{region_text});
                    return null;
                },
                .e_lookup_pending => {
                    // Resolve pending lookup by source text. This keeps
                    // go-to-definition/hover robust in partially-resolved states.
                    const lookup_region = module_env.store.getExprRegion(expr_idx);
                    const region_text = module_env.getSource(lookup_region);

                    if (module_lookup.findDefinitionByUnqualifiedName(module_env, region_text)) |def_info| {
                        const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def_info.pattern_idx));
                        const def_region = module_env.store.getRegionAt(pattern_node_idx);
                        const range = cir_queries.regionToRange(module_env, def_region) orelse return null;
                        return DefinitionResult{
                            .uri = current_uri,
                            .range = range,
                        };
                    }

                    if (std.mem.indexOfScalar(u8, region_text, '.')) |dot_pos| {
                        const module_name = region_text[0..dot_pos];
                        return self.findModuleByName(module_name);
                    }

                    return null;
                },
                .e_dot_access => |dot| {
                    // Static dispatch - cursor is on method name
                    // Get the type of the receiver to find which module provides the method
                    const receiver_type_var = ModuleEnv.varFrom(dot.receiver);
                    var type_writer = module_env.initTypeWriter() catch |err| {
                        self.logDebug(.build, "[DEF] initTypeWriter failed: {s}", .{@errorName(err)});
                        return null;
                    };
                    defer type_writer.deinit();

                    type_writer.write(receiver_type_var, .one_line) catch |err| {
                        self.logDebug(.build, "[DEF] type_writer.write failed: {s}", .{@errorName(err)});
                        return null;
                    };
                    const type_str = type_writer.get();

                    // Extract the base type name (e.g., "Str" from complex type)
                    const base_type = extractBaseTypeName(type_str);

                    self.logDebug(.build, "[DEF] e_dot_access type_str='{s}', base_type='{s}'", .{ type_str, base_type });

                    // Find the module for this type
                    // TODO: Also navigate to the specific method definition within the module
                    const result = self.findModuleByName(base_type);
                    if (result == null) {
                        self.logDebug(.build, "[DEF] findModuleByName returned null for '{s}'", .{base_type});
                    }
                    return result;
                },
                else => return null,
            }
        }

        return null;
    }

    // isBuiltinType moved to completion/builtins.zig module

    /// Helper function to find a module by name and return a DefinitionResult pointing to it
    fn findModuleByName(self: *SyntaxChecker, module_name: []const u8) ?DefinitionResult {
        const env = self.getModuleLookupEnv() orelse return null;

        // Extract the base module name (e.g., "Stdout" from "pf.Stdout")
        const base_name = if (std.mem.lastIndexOf(u8, module_name, ".")) |dot_pos|
            module_name[dot_pos + 1 ..]
        else
            module_name;

        // Check if this is a builtin type - use embedded Builtin.roc source
        if (completion_builtins.isBuiltinType(base_name)) {
            self.logDebug(.build, "[DEF] '{s}' is a builtin type", .{base_name});

            // Write embedded builtin source to roc cache
            const cache_dir = self.cache_config.getCacheEntriesDir(self.allocator) catch return null;
            const builtin_cache_path = std.fs.path.join(self.allocator, &.{ cache_dir, "Builtin.roc" }) catch {
                self.allocator.free(cache_dir);
                return null;
            };
            self.allocator.free(cache_dir);

            // Write file if it doesn't exist
            if (std.fs.cwd().access(builtin_cache_path, .{})) |_| {
                // Already exists
            } else |_| {
                // Create parent dirs and write embedded source
                if (std.fs.path.dirname(builtin_cache_path)) |dir| {
                    std.fs.cwd().makePath(dir) catch {};
                }
                const file = std.fs.cwd().createFile(builtin_cache_path, .{}) catch {
                    self.allocator.free(builtin_cache_path);
                    return null;
                };
                defer file.close();
                file.writeAll(compiled_builtins.builtin_source) catch {
                    self.allocator.free(builtin_cache_path);
                    return null;
                };
            }

            const module_uri = uri_util.pathToUri(self.allocator, builtin_cache_path) catch {
                self.allocator.free(builtin_cache_path);
                return null;
            };
            self.allocator.free(builtin_cache_path);

            return DefinitionResult{
                .uri = module_uri,
                .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 },
            };
        }

        // Search all schedulers for a module matching this name
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            if (sched.getModuleState(base_name)) |mod_state| {
                const module_uri = uri_util.pathToUri(self.allocator, mod_state.path) catch return null;
                return DefinitionResult{
                    .uri = module_uri,
                    .range = .{
                        .start_line = 0,
                        .start_col = 0,
                        .end_line = 0,
                        .end_col = 0,
                    },
                };
            }
        }
        return null;
    }

    /// Extract the base type name from a type string (e.g., "Str" from "Str", "List a" -> "List")
    fn extractBaseTypeName(type_str: []const u8) []const u8 {
        // Skip leading whitespace
        var start: usize = 0;
        while (start < type_str.len and (type_str[start] == ' ' or type_str[start] == '\t')) {
            start += 1;
        }

        // Find end of the type name (stop at space, bracket, or end)
        var end = start;
        while (end < type_str.len) {
            const c = type_str[end];
            if (c == ' ' or c == '[' or c == '(' or c == '{' or c == '<') break;
            end += 1;
        }

        return type_str[start..end];
    }

    // findLookupAtOffset moved to cir_queries module

    /// Find the type annotation at the given offset and return a DefinitionResult.
    /// This recursively walks type annotation trees to find the most specific type at the cursor.
    fn findTypeAnnoAtOffset(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        type_anno_idx: CIR.TypeAnno.Idx,
        target_offset: u32,
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

                switch (lookup.base) {
                    .local => |local| {
                        // Local type definition - navigate to the statement where it's declared
                        const decl_region = module_env.store.getStatementRegion(local.decl_idx);
                        const range = cir_queries.regionToRange(module_env, decl_region) orelse return null;
                        // For local definitions, we need the current file URI
                        // Since we don't have it here, we return null and let the caller handle it
                        // Actually, we can construct a result with a marker that means "same file"
                        return DefinitionResult{
                            .uri = "", // Empty URI means same file - caller should fill in
                            .range = range,
                        };
                    },
                    .builtin, .external, .pending => {
                        // Builtin, external, or pending type - find the module
                        return self.findModuleByName(type_name);
                    },
                }
            },
            .apply => |apply| {
                // Type with args like `List(Str)` - check args first, then the base type
                const args_slice = module_env.store.sliceTypeAnnos(apply.args);
                for (args_slice) |arg_idx| {
                    if (self.findTypeAnnoAtOffset(module_env, arg_idx, target_offset)) |result| {
                        return result;
                    }
                }
                // If not in args, return the base type
                const type_name = module_env.common.idents.getText(apply.name);
                self.logDebug(.build, "[DEF] TypeAnno.apply: type='{s}', base={s}", .{
                    type_name,
                    @tagName(apply.base),
                });

                switch (apply.base) {
                    .local => |local| {
                        // Local type definition - navigate to the statement where it's declared
                        const decl_region = module_env.store.getStatementRegion(local.decl_idx);
                        const range = cir_queries.regionToRange(module_env, decl_region) orelse return null;
                        return DefinitionResult{
                            .uri = "", // Empty URI means same file - caller should fill in
                            .range = range,
                        };
                    },
                    .builtin, .external, .pending => {
                        // Builtin, external, or pending type - find the module
                        return self.findModuleByName(type_name);
                    },
                }
            },
            .record => |rec| {
                // Check record field types
                const fields_slice = module_env.store.sliceAnnoRecordFields(rec.fields);
                for (fields_slice) |field_idx| {
                    const field = module_env.store.getAnnoRecordField(field_idx);
                    if (self.findTypeAnnoAtOffset(module_env, field.ty, target_offset)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .tag_union => |tu| {
                // Check tag types
                const tags_slice = module_env.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    if (self.findTypeAnnoAtOffset(module_env, tag_idx, target_offset)) |result| {
                        return result;
                    }
                }
                if (tu.ext) |ext_idx| {
                    if (self.findTypeAnnoAtOffset(module_env, ext_idx, target_offset)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .tag => |t| {
                // Check tag argument types
                const args_slice = module_env.store.sliceTypeAnnos(t.args);
                for (args_slice) |arg_idx| {
                    if (self.findTypeAnnoAtOffset(module_env, arg_idx, target_offset)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .@"fn" => |f| {
                // Check function argument and return types
                const args_slice = module_env.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    if (self.findTypeAnnoAtOffset(module_env, arg_idx, target_offset)) |result| {
                        return result;
                    }
                }
                if (self.findTypeAnnoAtOffset(module_env, f.ret, target_offset)) |result| {
                    return result;
                }
                return null;
            },
            .tuple => |t| {
                // Check tuple element types
                const elems_slice = module_env.store.sliceTypeAnnos(t.elems);
                for (elems_slice) |elem_idx| {
                    if (self.findTypeAnnoAtOffset(module_env, elem_idx, target_offset)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .parens => |p| {
                // Unwrap and recurse
                return self.findTypeAnnoAtOffset(module_env, p.anno, target_offset);
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
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        target_offset: u32,
        current_uri: []const u8,
    ) ?DefinitionResult {
        const expr = module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_block => |block| {
                // Check statements in the block for type annotations
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);

                    // Extract type annotation from statement
                    const maybe_type_anno: ?CIR.TypeAnno.Idx = switch (stmt) {
                        .s_decl => |d| if (d.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
                        .s_var => |d| if (d.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
                        .s_type_anno => |t| t.anno,
                        .s_alias_decl => |a| a.anno,
                        .s_nominal_decl => |n| n.anno,
                        else => null,
                    };

                    if (maybe_type_anno) |type_anno_idx| {
                        if (self.findTypeAnnoAtOffset(module_env, type_anno_idx, target_offset)) |result| {
                            if (result.uri.len == 0) {
                                return DefinitionResult{
                                    .uri = current_uri,
                                    .range = result.range,
                                };
                            }
                            return result;
                        }
                    }

                    // Recurse into expressions within the statement
                    const stmt_parts = module_lookup.getStatementParts(stmt);
                    if (stmt_parts.expr) |stmt_expr| {
                        if (self.findTypeAnnoInExpr(module_env, stmt_expr, target_offset, current_uri)) |result| {
                            return result;
                        }
                    }
                    if (stmt_parts.expr2) |stmt_expr| {
                        if (self.findTypeAnnoInExpr(module_env, stmt_expr, target_offset, current_uri)) |result| {
                            return result;
                        }
                    }
                }
                // Also check final expression
                return self.findTypeAnnoInExpr(module_env, block.final_expr, target_offset, current_uri);
            },
            .e_lambda => |lambda| {
                return self.findTypeAnnoInExpr(module_env, lambda.body, target_offset, current_uri);
            },
            .e_closure => |closure| {
                return self.findTypeAnnoInExpr(module_env, closure.lambda_idx, target_offset, current_uri);
            },
            .e_if => |if_expr| {
                const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    if (self.findTypeAnnoInExpr(module_env, branch.cond, target_offset, current_uri)) |result| {
                        return result;
                    }
                    if (self.findTypeAnnoInExpr(module_env, branch.body, target_offset, current_uri)) |result| {
                        return result;
                    }
                }
                return self.findTypeAnnoInExpr(module_env, if_expr.final_else, target_offset, current_uri);
            },
            .e_match => |match_expr| {
                if (self.findTypeAnnoInExpr(module_env, match_expr.cond, target_offset, current_uri)) |result| {
                    return result;
                }
                const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    if (self.findTypeAnnoInExpr(module_env, branch.value, target_offset, current_uri)) |result| {
                        return result;
                    }
                    if (branch.guard) |guard| {
                        if (self.findTypeAnnoInExpr(module_env, guard, target_offset, current_uri)) |result| {
                            return result;
                        }
                    }
                }
                return null;
            },
            .e_call => |call| {
                if (self.findTypeAnnoInExpr(module_env, call.func, target_offset, current_uri)) |result| {
                    return result;
                }
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg| {
                    if (self.findTypeAnnoInExpr(module_env, arg, target_offset, current_uri)) |result| {
                        return result;
                    }
                }
                return null;
            },
            else => return null,
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
    ) !?HighlightResult {
        self.mutex.lock();
        defer self.mutex.unlock();

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, env, uri, override_text);
        defer session.deinit();

        self.logDebug(.build, "highlights: building {s}", .{session.absolute_path});

        if (!session.build_succeeded) {
            self.logDebug(.build, "highlights: build failed for {s}", .{session.absolute_path});
            return null;
        }

        // Get module environment
        const module_env = session.getModuleEnv() orelse return null;

        // Convert LSP position to byte offset
        const target_offset = pos.positionToOffset(module_env, line, character) orelse return null;

        // Find the pattern_idx at this position
        const target_pattern = cir_queries.findPatternAtOffset(module_env, target_offset) orelse return null;

        // Collect all references to this pattern
        var regions = std.ArrayList(LspRange){};
        errdefer regions.deinit(self.allocator);

        // Add the definition itself
        const def_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(target_pattern));
        const def_region = module_env.store.getRegionAt(def_node_idx);
        if (cir_queries.regionToRange(module_env, def_region)) |range| {
            try regions.append(self.allocator, range);
        }

        // Find all lookups that reference this pattern
        var lookup_regions = cir_queries.collectLookupReferences(module_env, target_pattern, self.allocator);
        defer lookup_regions.deinit(self.allocator);
        try regions.appendSlice(self.allocator, lookup_regions.items);

        return HighlightResult{
            .regions = try regions.toOwnedSlice(self.allocator),
        };
    }

    /// Find the pattern_idx at the given offset.
    /// Returns the pattern being defined or referenced at that position.
    pub fn getDocumentSymbols(
        self: *SyntaxChecker,
        allocator: std.mem.Allocator,
        uri: []const u8,
        source: []const u8,
    ) ![]document_symbol_handler.SymbolInformation {
        const SymbolInformation = document_symbol_handler.SymbolInformation;

        self.mutex.lock();
        defer self.mutex.unlock();

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        // Convert URI to absolute path to match against module paths
        const path = uri_util.uriToPath(allocator, uri) catch return &[_]SymbolInformation{};
        defer allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(allocator, path) catch
            allocator.dupe(u8, path) catch return &[_]SymbolInformation{};
        defer allocator.free(absolute_path);

        // Set up file provider with source as override text
        var provider_state = OverrideProvider{
            .override_path = absolute_path,
            .override_text = source,
        };
        const provider: FileProvider = .{
            .ctx = &provider_state,
            .read = OverrideProvider.read,
        };
        env.setFileProvider(provider);
        defer env.setFileProvider(null);

        self.logDebug(.build, "symbols: building {s}", .{absolute_path});
        env.build(absolute_path) catch |err| {
            self.logDebug(.build, "symbols: build failed for {s}: {s}", .{ absolute_path, @errorName(err) });
            return &[_]SymbolInformation{};
        };

        // Drain reports but ignore them for symbols (must still free to avoid leaks)
        const drained = env.drainReports() catch return &[_]SymbolInformation{};
        defer self.freeDrainedWithReports(drained);

        // Get the module env from the scheduler
        const module_env = blk: {
            // Try "app" scheduler first
            if (env.schedulers.get("app")) |sched| {
                if (sched.getRootModule()) |rm| {
                    if (rm.env) |*e| {
                        break :blk e;
                    }
                }
            }
            // Fallback: try any scheduler with a root module
            var sched_it = env.schedulers.iterator();
            while (sched_it.next()) |entry| {
                const sched = entry.value_ptr.*;
                if (sched.getRootModule()) |rm| {
                    if (rm.env) |*e| {
                        break :blk e;
                    }
                }
            }
            return &[_]SymbolInformation{};
        };

        // Build line offset table
        const line_offsets = pos.buildLineOffsets(allocator, source) catch return &[_]SymbolInformation{};
        defer line_offsets.deinit();

        var symbols = std.ArrayList(SymbolInformation){};
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
                const owned_name = try allocator.dupe(u8, symbol.name);
                try symbols.append(allocator, .{
                    .name = owned_name,
                    .kind = symbol.kind,
                    .location = .{
                        .uri = uri,
                        .range = symbol.location.range,
                    },
                });
            }
        }

        // Also check top-level statements (some module types use these)
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = module_lookup.getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                if (stmt_parts.expr) |expr_idx| {
                    if (extractSymbolFromDecl(module_env, pattern_idx, expr_idx, source, uri, &line_offsets)) |symbol| {
                        self.logDebug(.build, "symbols: found stmt symbol '{s}'", .{symbol.name});
                        const owned_name = try allocator.dupe(u8, symbol.name);
                        try symbols.append(allocator, .{
                            .name = owned_name,
                            .kind = symbol.kind,
                            .location = .{
                                .uri = uri,
                                .range = symbol.location.range,
                            },
                        });
                    }
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
    fn resolveLocalBindingTypeVar(self: *SyntaxChecker, module_env: *ModuleEnv, name: []const u8, name_start: u32) ?types.Var {
        var scope = scope_map.ScopeMap.init(self.allocator);
        defer scope.deinit();
        scope.build(module_env) catch return null;

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

        var sched_it = module_lookup_env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            if (sched.getModuleState(module_name)) |mod_state| {
                if (mod_state.env) |*mod_env| return mod_env;
            }
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
            ) orelse return null;
            var type_var = ModuleEnv.varFrom(def_info.pattern_idx);
            var namespace_prefix = std.ArrayList(u8).empty;
            defer namespace_prefix.deinit(self.allocator);
            namespace_prefix.appendSlice(self.allocator, first.segment) catch return null;
            namespace_prefix.append(self.allocator, '.') catch return null;
            namespace_prefix.appendSlice(self.allocator, member.segment) catch return null;

            while (nextChainSegment(access_chain, idx)) |segment| {
                idx = segment.next;

                // Prefer namespace/member traversal for uppercase segments before
                // falling back to structural field traversal.
                if (findDefinitionByQualifiedPrefix(self.allocator, resolved_env, namespace_prefix.items, segment.segment)) |next_def| {
                    type_var = ModuleEnv.varFrom(next_def.pattern_idx);
                    namespace_prefix.append(self.allocator, '.') catch return null;
                    namespace_prefix.appendSlice(self.allocator, segment.segment) catch return null;
                    continue;
                }

                const next_var = builder.getFieldTypeVarFromTypeVar(resolved_env, type_var, segment.segment) orelse return null;
                type_var = next_var;

                namespace_prefix.append(self.allocator, '.') catch return null;
                namespace_prefix.appendSlice(self.allocator, segment.segment) catch return null;
            }

            return .{ .module_env = resolved_env, .type_var = type_var };
        }

        var type_var = self.resolveLocalBindingTypeVar(module_env, first.segment, chain_start) orelse return null;
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
    ) ?module_lookup.DefinitionInfo {
        if (module_lookup.findDefinitionByName(module_env, member_name)) |def_info| {
            return def_info;
        }

        if (findDefinitionByQualifiedPrefix(allocator, module_env, namespace_prefix, member_name)) |def_info| {
            return def_info;
        }

        return module_lookup.findDefinitionByUnqualifiedName(module_env, member_name);
    }

    /// Find a definition by building `prefix.member` and doing exact lookup.
    fn findDefinitionByQualifiedPrefix(
        allocator: Allocator,
        module_env: *ModuleEnv,
        prefix: []const u8,
        member_name: []const u8,
    ) ?module_lookup.DefinitionInfo {
        const qualified = std.fmt.allocPrint(allocator, "{s}.{s}", .{ prefix, member_name }) catch return null;
        defer allocator.free(qualified);
        return module_lookup.findDefinitionByName(module_env, qualified);
    }

    /// Get the next segment in a dotted access chain.
    fn nextChainSegment(chain: []const u8, start: usize) ?struct { segment: []const u8, next: usize } {
        if (start >= chain.len) return null;
        const dot_idx = std.mem.indexOfScalarPos(u8, chain, start, '.') orelse chain.len;
        const segment = chain[start..dot_idx];
        const next = if (dot_idx < chain.len) dot_idx + 1 else chain.len;
        return .{ .segment = segment, .next = next };
    }

    /// Get the last segment in a dotted access chain.
    fn lastChainSegment(chain: []const u8) []const u8 {
        const dot_idx = std.mem.lastIndexOfScalar(u8, chain, '.') orelse return chain;
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
            switch (content) {
                .alias => |alias| {
                    const backing_var = type_store.getAliasBackingVar(alias);
                    resolved = type_store.resolveVar(backing_var);
                    content = resolved.desc.content;
                    continue;
                },
                else => break,
            }
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
    ) !?completion_handler.CompletionResult {
        self.mutex.lock();
        defer self.mutex.unlock();

        const env_handle = try self.createFreshBuildEnv();
        const env = env_handle.envPtr();

        var session = try BuildSession.init(self.allocator, env, uri, override_text);
        defer session.deinit();

        self.logDebug(.completion, "completion: building {s}", .{session.absolute_path});
        self.logDebug(.completion, "completion: build_succeeded={}", .{session.build_succeeded});

        var build_has_reports = false;

        // Check if we have reports for this file
        if (session.drained_reports) |drained| {
            for (drained) |entry| {
                if (std.mem.eql(u8, entry.abs_path, session.absolute_path) and entry.reports.len > 0) {
                    build_has_reports = true;
                    break;
                }
            }
        }

        // Detect completion context from source
        const source = override_text orelse "";
        const context = completion_context.detectCompletionContext(source, line, character);

        // Compute cursor offset for scope-based completions
        const cursor_offset = completion_context.computeOffset(source, line, character);

        // Collect completions based on context
        var items = std.ArrayList(completion_handler.CompletionItem){};
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
            if (self.snapshot_envs.get(session.absolute_path)) |snapshot_handle| {
                const snapshot_module_env = self.getModuleEnvByPathInEnv(snapshot_handle.envPtr(), session.absolute_path);
                if (snapshot_module_env) |module_env| {
                    used_snapshot = true;
                    module_lookup_env = snapshot_handle.envPtr();
                    break :blk module_env;
                }
            }

            // Fall back to previous build env if snapshot not available
            if (self.previous_build_env) |previous_handle| {
                const prev_module_env = self.getModuleEnvByPathInEnv(previous_handle.envPtr(), session.absolute_path);
                if (prev_module_env) |module_env| {
                    used_snapshot = true;
                    module_lookup_env = previous_handle.envPtr();
                    break :blk module_env;
                }
            }

            // Fall back to current build only if no snapshot available and build succeeded
            if (session.build_succeeded and !build_has_reports) {
                if (self.getModuleEnvByPath(session.absolute_path)) |module_env| {
                    module_lookup_env = env;
                    break :blk module_env;
                }

                module_lookup_env = env;
                break :blk session.getModuleEnv();
            }

            break :blk null;
        };

        self.logDebug(.completion, "completion: context={any}, module_env_opt={any}, build_succeeded={}, used_snapshot={}", .{ context, module_env_opt != null, session.build_succeeded, used_snapshot });

        // Initialize CompletionBuilder for deduplication and organized completion item building
        // Provide the builtin module env so completion can resolve builtin method data.
        var builder = completion_builder.CompletionBuilder.initWithDebug(self.allocator, &items, env.builtin_modules.builtin_module.env, self.debug, self.log_file);
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
                    _ = try builder.addTagCompletionsForNominalType(module_env, module_name, null);
                }
            },
            .after_value_dot => |record_access| {
                self.logDebug(.completion, "completion: after_record_dot for '{s}' at offset {d}", .{ record_access.access_chain, record_access.member_start });
                if (module_env_opt) |module_env| {
                    var chain_resolved = false;
                    if (resolveAccessChainTypeVar(self, &builder, module_env, module_lookup_env, env, record_access.access_chain, record_access.chain_start)) |resolved| {
                        chain_resolved = true;
                        try builder.addFieldsFromTypeVar(resolved.module_env, resolved.type_var);
                        try builder.addTupleIndexCompletions(resolved.module_env, resolved.type_var);
                        try builder.addMethodsFromTypeVar(resolved.module_env, resolved.type_var);
                    }

                    // When the chain starts with an uppercase identifier and
                    // type-based traversal fails, try namespace-style member
                    // completion from qualified definition names.
                    var namespace_resolved = false;
                    if (!chain_resolved and record_access.access_chain.len > 0 and std.ascii.isUpper(record_access.access_chain[0])) {
                        namespace_resolved = try builder.addNamespaceMemberCompletions(module_env, record_access.access_chain);
                    }

                    if (!chain_resolved and !namespace_resolved) {
                        const variable_name = lastChainSegment(record_access.access_chain);
                        const variable_start = record_access.member_start;

                        // Try the precise CIR-based lookup first: findDotReceiverTypeVar
                        // specifically looks for e_dot_access nodes and returns the
                        // receiver's type, which is semantically correct for dot
                        // completions. Fall back to findExprEndingAt for cases where
                        // the CIR lacks a dot access node (e.g., incomplete code).
                        var resolved_type_var: ?types.Var = null;
                        if (cir_queries.findDotReceiverTypeVar(module_env, cursor_offset)) |type_var| {
                            resolved_type_var = type_var;
                        }
                        if (resolved_type_var == null and record_access.dot_offset > 0) {
                            if (cir_queries.findExprEndingAt(module_env, record_access.dot_offset)) |type_at| {
                                resolved_type_var = type_at.type_var;
                            }
                        }
                        // When using snapshot, cursor positions don't correspond to snapshot CIR
                        // So we must look up by name instead of analyzing the dot expression
                        if (used_snapshot or resolved_type_var == null) {
                            self.logDebug(.completion, "completion: using name-based lookup (snapshot={}, or findDotReceiverTypeVar failed)", .{used_snapshot});
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
                    self.logDebug(.completion, "completion: NO module_env for record/method completions", .{});
                }
            },
            .after_receiver_dot => |info| {
                // Use CIR to resolve receiver types for chained calls (e.g., value.func().).
                // This avoids brittle text parsing and keeps completion tied to the AST.
                if (module_env_opt) |module_env| {
                    // Prefer findDotReceiverTypeVar (semantic dot-access lookup)
                    // and fall back to findExprEndingAt (position-based) when the
                    // CIR doesn't have a dot access node for this position.
                    var resolved_type_var: ?types.Var = null;
                    if (cir_queries.findDotReceiverTypeVar(module_env, cursor_offset)) |type_var| {
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
                            if (resolveAccessChainTypeVar(self, &builder, module_env, module_lookup_env, env, call_chain, info.chain_start)) |resolved| {
                                const ret_type = extractReturnType(resolved.module_env, resolved.type_var);
                                try builder.addFieldsFromTypeVar(resolved.module_env, ret_type);
                                try builder.addTupleIndexCompletions(resolved.module_env, ret_type);
                                try builder.addMethodsFromTypeVar(resolved.module_env, ret_type);
                            }
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

fn extractSymbolFromDecl(
    module_env: *ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    source: []const u8,
    uri: []const u8,
    line_offsets: *const pos.LineOffsets,
) ?document_symbol_handler.SymbolInformation {
    _ = source; // We use getIdentText instead of extracting from source

    // Check if RHS is a function
    const expr = module_env.store.getExpr(expr_idx);
    const is_function = switch (expr) {
        .e_closure, .e_lambda, .e_hosted_lambda => true,
        else => false,
    };

    // Get the pattern and extract the identifier name
    const pattern = module_env.store.getPattern(pattern_idx);
    const ident_idx = switch (pattern) {
        .assign => |p| p.ident,
        .as => |p| p.ident,
        else => return null, // Only handle assign and as patterns
    };

    // Get the identifier text from the module's ident table
    const name = module_env.getIdentText(ident_idx);

    // Skip empty or placeholder names
    if (name.len == 0) {
        return null;
    }

    // Get the pattern region for position info
    const pattern_region = module_env.store.getPatternRegion(pattern_idx);
    const start_offset = pattern_region.start.offset;
    const end_offset = pattern_region.end.offset;

    // Convert offsets to positions
    const start_pos = pos.offsetToPosition(start_offset, line_offsets);
    const end_pos = pos.offsetToPosition(end_offset, line_offsets);

    return .{
        .name = name,
        .kind = if (is_function) .function else .variable,
        .location = .{
            .uri = uri,
            .range = .{ .start = start_pos, .end = end_pos },
        },
    };
}

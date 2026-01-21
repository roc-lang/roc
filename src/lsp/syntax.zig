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

const BuildEnv = compile.BuildEnv;
const CacheManager = compile.CacheManager;
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
};

/// Runs BuildEnv-backed syntax/type checks and converts reports to LSP diagnostics.
pub const SyntaxChecker = struct {
    allocator: std.mem.Allocator,
    mutex: std.Thread.Mutex = .{},
    build_env: ?*BuildEnv = null,
    /// Previous successful BuildEnv kept for module lookups (e.g., semantic tokens).
    /// This is swapped with build_env after each successful build.
    previous_build_env: ?*BuildEnv = null,
    /// Snapshot of the most recent successful build per module (kept for completions).
    snapshot_envs: std.StringHashMapUnmanaged(*BuildEnv) = .{},
    snapshot_env_ref_counts: std.AutoHashMapUnmanaged(*BuildEnv, usize) = .{},
    /// Dependency graph for tracking module relationships and invalidation.
    dependency_graph: DependencyGraph,
    cache_config: CacheConfig = .{},
    log_file: ?std.fs.File = null,
    debug: DebugFlags,

    pub fn init(allocator: std.mem.Allocator, debug: DebugFlags, log_file: ?std.fs.File) SyntaxChecker {
        return .{
            .allocator = allocator,
            .dependency_graph = DependencyGraph.init(allocator),
            .debug = debug,
            .log_file = log_file,
        };
    }

    pub fn deinit(self: *SyntaxChecker) void {
        self.clearSnapshots();

        if (self.build_env) |env| {
            env.deinit();
            self.allocator.destroy(env);
            self.build_env = null;
        }
        if (self.previous_build_env) |env| {
            env.deinit();
            self.allocator.destroy(env);
            self.previous_build_env = null;
        }

        self.dependency_graph.deinit();
    }

    /// Check the file referenced by the URI and return diagnostics grouped by URI.
    pub fn check(self: *SyntaxChecker, uri: []const u8, override_text: ?[]const u8, workspace_root: ?[]const u8) ![]Diagnostics.PublishDiagnostics {
        std.debug.print("syntax check: uri={s}\n", .{uri});
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
        defer self.allocator.free(absolute_path);

        _ = workspace_root; // Reserved for future use

        self.mutex.lock();
        defer self.mutex.unlock();

        std.debug.print("completion: create fresh build env\n", .{});
        var env = try self.createFreshBuildEnv();

        var provider_state = OverrideProvider{
            .override_path = absolute_path,
            .override_text = override_text,
        };
        const provider: ?FileProvider = if (override_text != null) .{
            .ctx = &provider_state,
            .read = OverrideProvider.read,
        } else null;
        env.setFileProvider(provider);
        defer env.setFileProvider(null);

        const dir_slice = std.fs.path.dirname(absolute_path) orelse ".";
        const dir_owned = try self.allocator.dupe(u8, dir_slice);
        defer self.allocator.free(dir_owned);
        const prev_cwd = std.process.getCwdAlloc(self.allocator) catch null;
        defer if (prev_cwd) |cwd| {
            std.process.changeCurDir(cwd) catch {};
            self.allocator.free(cwd);
        };
        std.process.changeCurDir(dir_owned) catch {};

        self.logDebug(.build, "building {s}", .{absolute_path});
        std.debug.print("syntax check: build {s}\n", .{absolute_path});
        env.build(absolute_path) catch |err| {
            self.logDebug(.build, "build failed for {s}: {s}", .{ absolute_path, @errorName(err) });
        };

        const drained = env.drainReports() catch |err| {
            self.logDebug(.build, "drain reports failed: {s}", .{@errorName(err)});
            return err;
        };
        std.debug.print("syntax check: drained reports={d}\n", .{drained.len});
        defer self.freeDrained(drained);

        // Update dependency graph from successful build
        self.updateDependencyGraph(env);

        if (self.shouldSnapshotBuild(env, absolute_path, drained)) {
            self.storeSnapshotEnv(env, absolute_path);
        }

        var publish_list = std.ArrayList(Diagnostics.PublishDiagnostics){};
        errdefer {
            for (publish_list.items) |*set| set.deinit(self.allocator);
            publish_list.deinit(self.allocator);
        }

        for (drained) |entry| {
            const mapped_path = if (entry.abs_path.len == 0) absolute_path else entry.abs_path;
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
                defer rep.deinit();

                if (self.shouldSuppressReport(report)) continue;

                const diag = try self.reportToDiagnostic(report);
                try diags.append(self.allocator, diag);
            }
            self.allocator.free(entry.reports);

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
    }

    /// Creates a fresh BuildEnv for a new build.
    /// The previous build_env is moved to previous_build_env for module lookups.
    fn createFreshBuildEnv(self: *SyntaxChecker) !*BuildEnv {
        std.debug.print("createFreshBuildEnv: prev_build_env={any} build_env={any}\n", .{ self.previous_build_env != null, self.build_env != null });
        // Move current build_env to previous_build_env (release old previous if exists)
        if (self.previous_build_env) |old_prev| {
            // Only free if no snapshot is still referencing it.
            // Snapshot ref counts are owned by snapshot_envs entries, so don't release here.
            const snapshot_ref = self.snapshot_env_ref_counts.get(old_prev) != null;
            const snapshot_map_ref = self.isEnvReferencedInSnapshots(old_prev);
            if (!snapshot_ref and !snapshot_map_ref) {
                std.debug.print("createFreshBuildEnv: freeing old previous env\n", .{});
                old_prev.deinit();
                self.allocator.destroy(old_prev);
            } else {
                std.debug.print("createFreshBuildEnv: keeping old previous env (snapshot still references)\n", .{});
            }
        }
        self.previous_build_env = self.build_env;
        self.build_env = null;

        // Create a fresh BuildEnv
        const env_ptr = try self.allocator.create(BuildEnv);
        errdefer self.allocator.destroy(env_ptr);

        env_ptr.* = try BuildEnv.init(self.allocator, .single_threaded, 1);
        env_ptr.compiler_version = build_options.compiler_version;

        if (self.cache_config.enabled) {
            const cache_manager = try self.allocator.create(CacheManager);
            cache_manager.* = CacheManager.init(self.allocator, self.cache_config, Filesystem.default());
            env_ptr.setCacheManager(cache_manager);
        }

        self.build_env = env_ptr;
        return env_ptr;
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

    fn storeSnapshotEnv(self: *SyntaxChecker, env: *BuildEnv, absolute_path: []const u8) void {
        std.debug.print("storeSnapshotEnv: path={s}\n", .{absolute_path});
        if (self.snapshot_envs.get(absolute_path)) |existing| {
            std.debug.print("storeSnapshotEnv: replacing existing snapshot\n", .{});
            self.releaseSnapshotEnv(existing);
            _ = self.snapshot_envs.remove(absolute_path);
        }

        const owned_path = self.allocator.dupe(u8, absolute_path) catch return;
        self.snapshot_envs.put(self.allocator, owned_path, env) catch {
            self.allocator.free(owned_path);
            return;
        };
        self.retainSnapshotEnv(env);
        std.debug.print("storeSnapshotEnv: stored snapshot count={d}\n", .{self.snapshot_envs.count()});
    }

    fn retainSnapshotEnv(self: *SyntaxChecker, env: *BuildEnv) void {
        const entry = self.snapshot_env_ref_counts.get(env);
        const next_count = if (entry) |count| count + 1 else 1;
        _ = self.snapshot_env_ref_counts.put(self.allocator, env, next_count) catch {};
        std.debug.print("retainSnapshotEnv: count={d}\n", .{next_count});
    }

    fn releaseSnapshotEnv(self: *SyntaxChecker, env: *BuildEnv) void {
        if (self.snapshot_env_ref_counts.get(env)) |count| {
            if (count <= 1) {
                if (self.isEnvReferencedInSnapshots(env)) {
                    std.debug.print("releaseSnapshotEnv: env still referenced by snapshots\n", .{});
                    _ = self.snapshot_env_ref_counts.put(self.allocator, env, 1) catch {};
                    return;
                }
                std.debug.print("releaseSnapshotEnv: freeing env\n", .{});
                _ = self.snapshot_env_ref_counts.remove(env);
                env.deinit();
                self.allocator.destroy(env);
            } else {
                std.debug.print("releaseSnapshotEnv: decrementing env count to {d}\n", .{count - 1});
                _ = self.snapshot_env_ref_counts.put(self.allocator, env, count - 1) catch {};
            }
        } else {
            std.debug.print("releaseSnapshotEnv: env not tracked\n", .{});
        }
    }

    fn clearSnapshots(self: *SyntaxChecker) void {
        var it = self.snapshot_envs.iterator();
        while (it.next()) |entry| {
            const env = entry.value_ptr.*;
            self.releaseSnapshotEnv(env);
            self.allocator.free(entry.key_ptr.*);
        }
        self.snapshot_envs.clearRetainingCapacity();
        self.snapshot_env_ref_counts.clearRetainingCapacity();
    }

    fn isEnvReferencedInSnapshots(self: *SyntaxChecker, env: *BuildEnv) bool {
        var it = self.snapshot_envs.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == env) return true;
        }
        return false;
    }

    /// Get the BuildEnv that should be used for module lookups (semantic tokens, etc.).
    /// Prefers the current build_env if it has modules, otherwise falls back to previous_build_env.
    pub fn getModuleLookupEnv(self: *SyntaxChecker) ?*BuildEnv {
        // Prefer current build_env if it exists and has modules
        if (self.build_env) |env| {
            if (env.schedulers.count() > 0) {
                return env;
            }
        }
        // Fall back to previous_build_env
        return self.previous_build_env;
    }

    /// Get the cached snapshot BuildEnv for completions.
    pub fn getSnapshotEnv(self: *SyntaxChecker) ?*BuildEnv {
        return self.snapshot_build_env;
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

    fn freeDrained(self: *SyntaxChecker, drained: []BuildEnv.DrainedModuleReports) void {
        // Legacy behavior - don't free reports (for check() which processes them manually)
        self.freeDrainedEx(drained, false);
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

    fn logDebug(self: *SyntaxChecker, kind: enum { build, syntax }, comptime fmt: []const u8, args: anytype) void {
        const enabled = switch (kind) {
            .build => self.debug.build,
            .syntax => self.debug.syntax,
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
            return null;
        }
    };

    /// Range in LSP coordinates
    pub const LspRange = struct {
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    };

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

    /// Get type information at a specific position in a document.
    /// Returns the type as a formatted string, or null if no type info is available.
    pub fn getTypeAtPosition(
        self: *SyntaxChecker,
        uri: []const u8,
        override_text: ?[]const u8,
        line: u32,
        character: u32,
    ) !?HoverResult {
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
        defer self.allocator.free(absolute_path);

        self.mutex.lock();
        defer self.mutex.unlock();

        var env = try self.createFreshBuildEnv();

        var provider_state = OverrideProvider{
            .override_path = absolute_path,
            .override_text = override_text,
        };
        const provider: ?FileProvider = if (override_text != null) .{
            .ctx = &provider_state,
            .read = OverrideProvider.read,
        } else null;
        env.setFileProvider(provider);
        defer env.setFileProvider(null);

        const dir_slice = std.fs.path.dirname(absolute_path) orelse ".";
        const dir_owned = try self.allocator.dupe(u8, dir_slice);
        defer self.allocator.free(dir_owned);
        const prev_cwd = std.process.getCwdAlloc(self.allocator) catch null;
        defer if (prev_cwd) |cwd| {
            std.process.changeCurDir(cwd) catch {};
            self.allocator.free(cwd);
        };
        std.process.changeCurDir(dir_owned) catch {};

        self.logDebug(.build, "hover: building {s}", .{absolute_path});
        env.build(absolute_path) catch |err| {
            self.logDebug(.build, "hover: build failed for {s}: {s}", .{ absolute_path, @errorName(err) });
            return null;
        };

        // Drain reports but ignore them for hover (must still free them to avoid leaks)
        const drained = env.drainReports() catch return null;
        defer self.freeDrainedWithReports(drained);

        // Get any available scheduler and find the root module
        // Try "app" first, then fall back to iterating through all schedulers
        const app_sched = env.schedulers.get("app") orelse blk: {
            // Try to find any scheduler with a valid root module
            var sched_it = env.schedulers.iterator();
            while (sched_it.next()) |entry| {
                const sched = entry.value_ptr.*;
                if (sched.getRootModule()) |rm| {
                    if (rm.env != null) {
                        break :blk sched;
                    }
                }
            }
            return null;
        };
        const root_module = app_sched.getRootModule() orelse return null;
        const module_env = if (root_module.env) |*e| e else return null;

        // Convert LSP position (0-based line/col) to byte offset
        // LSP uses 0-based line and UTF-16 code units for character
        const target_offset = positionToOffset(module_env, line, character) orelse return null;

        // Find the expression at this position
        const result = self.findTypeAtOffset(module_env, target_offset) orelse return null;

        // Format the type as a string
        var type_writer = try module_env.initTypeWriter();
        defer type_writer.deinit();

        try type_writer.write(result.type_var, .one_line);
        const type_str = type_writer.get();

        // Create markdown-formatted output
        const markdown = try std.fmt.allocPrint(self.allocator, "```roc\n{s}\n```", .{type_str});

        // Convert the region back to LSP positions
        const range = regionToRange(module_env, result.region);

        return HoverResult{
            .type_str = markdown,
            .range = range,
        };
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
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
        defer self.allocator.free(absolute_path);

        self.mutex.lock();
        defer self.mutex.unlock();

        var env = try self.createFreshBuildEnv();

        var provider_state = OverrideProvider{
            .override_path = absolute_path,
            .override_text = override_text,
        };
        const provider: ?FileProvider = if (override_text != null) .{
            .ctx = &provider_state,
            .read = OverrideProvider.read,
        } else null;
        env.setFileProvider(provider);
        defer env.setFileProvider(null);

        const dir_slice = std.fs.path.dirname(absolute_path) orelse ".";
        const dir_owned = try self.allocator.dupe(u8, dir_slice);
        defer self.allocator.free(dir_owned);
        const prev_cwd = std.process.getCwdAlloc(self.allocator) catch null;
        defer if (prev_cwd) |cwd| {
            std.process.changeCurDir(cwd) catch {};
            self.allocator.free(cwd);
        };
        std.process.changeCurDir(dir_owned) catch {};

        self.logDebug(.build, "definition: building {s}", .{absolute_path});
        env.build(absolute_path) catch |err| {
            self.logDebug(.build, "definition: build failed for {s}: {s}", .{ absolute_path, @errorName(err) });
            return null;
        };

        // Drain reports but ignore them for definition (must still free them to avoid leaks)
        const drained = env.drainReports() catch return null;
        defer self.freeDrainedWithReports(drained);

        // Get any available scheduler and find the root module
        const app_sched = env.schedulers.get("app") orelse blk: {
            var sched_it = env.schedulers.iterator();
            while (sched_it.next()) |entry| {
                const sched = entry.value_ptr.*;
                if (sched.getRootModule()) |rm| {
                    if (rm.env != null) {
                        break :blk sched;
                    }
                }
            }
            return null;
        };
        const root_module = app_sched.getRootModule() orelse return null;
        const module_env = if (root_module.env) |*e| e else return null;

        // Convert LSP position to byte offset
        const target_offset = positionToOffset(module_env, line, character) orelse return null;

        // Find the definition at this position
        const result = self.findDefinitionAtOffset(module_env, target_offset, uri) orelse return null;

        return result;
    }

    /// Convert LSP position (line, character) to byte offset in source
    fn positionToOffset(module_env: *ModuleEnv, line: u32, character: u32) ?u32 {
        const line_starts = module_env.getLineStartsAll();
        if (line >= line_starts.len) return null;

        const line_start = line_starts[line];
        // For simplicity, treat character as byte offset within line
        // (proper UTF-16 handling would require more work)
        return line_start + character;
    }

    /// Convert a Region to LSP range (line/character positions)
    fn regionToRange(module_env: *ModuleEnv, region: Region) ?LspRange {
        const line_starts = module_env.getLineStartsAll();
        if (line_starts.len == 0) return null;

        const start_offset = region.start.offset;
        const end_offset = region.end.offset;

        // Find line for start offset
        var start_line: u32 = 0;
        for (line_starts, 0..) |ls, i| {
            if (ls > start_offset) break;
            start_line = @intCast(i);
        }

        // Find line for end offset
        var end_line: u32 = 0;
        for (line_starts, 0..) |ls, i| {
            if (ls > end_offset) break;
            end_line = @intCast(i);
        }

        const start_col = start_offset - line_starts[start_line];
        const end_col = end_offset - line_starts[end_line];

        return .{
            .start_line = start_line,
            .start_col = start_col,
            .end_line = end_line,
            .end_col = end_col,
        };
    }

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

            if (regionContainsOffset(expr_region, target_offset)) {
                // First check for type annotations in nested blocks
                if (self.findTypeAnnoInExpr(module_env, expr_idx, target_offset, current_uri)) |result| {
                    return result;
                }
                // Then search for lookup expressions
                if (self.findLookupAtOffset(module_env, expr_idx, target_offset, &best_size)) |found| {
                    best_expr = found;
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

                if (regionContainsOffset(stmt_region, target_offset)) {
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
                .s_decl_gen => |d| if (d.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
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

            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.expr) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (regionContainsOffset(expr_region, target_offset)) {
                    if (self.findLookupAtOffset(module_env, expr_idx, target_offset, &best_size)) |found| {
                        best_expr = found;
                    }
                }
            }

            if (stmt_parts.expr2) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (regionContainsOffset(expr_region, target_offset)) {
                    if (self.findLookupAtOffset(module_env, expr_idx, target_offset, &best_size)) |found| {
                        best_expr = found;
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
                    const range = regionToRange(module_env, def_region) orelse return null;
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

    /// Known builtin type names that are part of the Builtin module
    const BUILTIN_TYPES = [_][]const u8{ "Str", "List", "Bool", "Try", "Dict", "Set", "Box", "U8", "U16", "U32", "U64", "U128", "I8", "I16", "I32", "I64", "I128", "F32", "F64", "Dec", "Num" };

    /// Check if a type name is a builtin type
    fn isBuiltinType(type_name: []const u8) bool {
        for (BUILTIN_TYPES) |builtin| {
            if (std.mem.eql(u8, type_name, builtin)) return true;
        }
        return false;
    }

    /// Helper function to find a module by name and return a DefinitionResult pointing to it
    fn findModuleByName(self: *SyntaxChecker, module_name: []const u8) ?DefinitionResult {
        const env = self.build_env orelse return null;

        // Extract the base module name (e.g., "Stdout" from "pf.Stdout")
        const base_name = if (std.mem.lastIndexOf(u8, module_name, ".")) |dot_pos|
            module_name[dot_pos + 1 ..]
        else
            module_name;

        // Check if this is a builtin type - use embedded Builtin.roc source
        if (isBuiltinType(base_name)) {
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

    /// Find the narrowest lookup expression at the given offset
    fn findLookupAtOffset(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        target_offset: u32,
        best_size: *u32,
    ) ?CIR.Expr.Idx {
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        const region = module_env.store.getRegionAt(node_idx);

        if (!regionContainsOffset(region, target_offset)) {
            return null;
        }

        const expr = module_env.store.getExpr(expr_idx);
        var result: ?CIR.Expr.Idx = null;

        // Check if this expression itself is a lookup or a dot access with cursor on method name
        switch (expr) {
            .e_lookup_local => {
                const size = region.end.offset - region.start.offset;
                if (size < best_size.*) {
                    best_size.* = size;
                    result = expr_idx;
                }
            },
            .e_lookup_external => {
                const size = region.end.offset - region.start.offset;
                if (size < best_size.*) {
                    best_size.* = size;
                    result = expr_idx;
                }
            },
            .e_dot_access => |dot| {
                // Check if cursor is on the field/method name (for static dispatch)
                self.logDebug(.build, "[DEF] e_dot_access found: field_name_region={d}-{d}, target_offset={d}", .{
                    dot.field_name_region.start.offset,
                    dot.field_name_region.end.offset,
                    target_offset,
                });
                if (regionContainsOffset(dot.field_name_region, target_offset)) {
                    const size = dot.field_name_region.end.offset - dot.field_name_region.start.offset;
                    if (size < best_size.*) {
                        best_size.* = size;
                        result = expr_idx;
                        self.logDebug(.build, "[DEF] e_dot_access MATCHED!", .{});
                    }
                }
            },
            else => {},
        }

        // Recurse into child expressions
        switch (expr) {
            .e_lambda => |lambda| {
                if (self.findLookupAtOffset(module_env, lambda.body, target_offset, best_size)) |found| {
                    result = found;
                }
            },
            .e_closure => |closure| {
                if (self.findLookupAtOffset(module_env, closure.lambda_idx, target_offset, best_size)) |found| {
                    result = found;
                }
            },
            .e_block => |block| {
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);
                    const stmt_parts = getStatementParts(stmt);
                    if (stmt_parts.expr) |stmt_expr| {
                        if (self.findLookupAtOffset(module_env, stmt_expr, target_offset, best_size)) |found| {
                            result = found;
                        }
                    }
                    if (stmt_parts.expr2) |stmt_expr| {
                        if (self.findLookupAtOffset(module_env, stmt_expr, target_offset, best_size)) |found| {
                            result = found;
                        }
                    }
                }
                if (self.findLookupAtOffset(module_env, block.final_expr, target_offset, best_size)) |found| {
                    result = found;
                }
            },
            .e_if => |if_expr| {
                const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    if (self.findLookupAtOffset(module_env, branch.cond, target_offset, best_size)) |found| {
                        result = found;
                    }
                    if (self.findLookupAtOffset(module_env, branch.body, target_offset, best_size)) |found| {
                        result = found;
                    }
                }
                if (self.findLookupAtOffset(module_env, if_expr.final_else, target_offset, best_size)) |found| {
                    result = found;
                }
            },
            .e_match => |match_expr| {
                // Check the condition (e.g., 'port' in 'match WebServer.listen!(port)')
                if (self.findLookupAtOffset(module_env, match_expr.cond, target_offset, best_size)) |found| {
                    result = found;
                }
                // Check all branches
                const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    if (self.findLookupAtOffset(module_env, branch.value, target_offset, best_size)) |found| {
                        result = found;
                    }
                    if (branch.guard) |guard| {
                        if (self.findLookupAtOffset(module_env, guard, target_offset, best_size)) |found| {
                            result = found;
                        }
                    }
                }
            },
            .e_call => |call| {
                if (self.findLookupAtOffset(module_env, call.func, target_offset, best_size)) |found| {
                    result = found;
                }
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg| {
                    if (self.findLookupAtOffset(module_env, arg, target_offset, best_size)) |found| {
                        result = found;
                    }
                }
            },
            .e_binop => |binop| {
                if (self.findLookupAtOffset(module_env, binop.lhs, target_offset, best_size)) |found| {
                    result = found;
                }
                if (self.findLookupAtOffset(module_env, binop.rhs, target_offset, best_size)) |found| {
                    result = found;
                }
            },
            .e_dot_access => |dot| {
                // Check the receiver (e.g., 'trimmed' in 'trimmed.starts_with(...)')
                if (self.findLookupAtOffset(module_env, dot.receiver, target_offset, best_size)) |found| {
                    result = found;
                }
                // Check the arguments if present
                if (dot.args) |args_span| {
                    const args = module_env.store.sliceExpr(args_span);
                    for (args) |arg| {
                        if (self.findLookupAtOffset(module_env, arg, target_offset, best_size)) |found| {
                            result = found;
                        }
                    }
                }
            },
            .e_str => |str| {
                // String with interpolation - search through all segments
                // Interpolated segments contain expressions that may have lookups
                const segments = module_env.store.sliceExpr(str.span);
                for (segments) |segment| {
                    if (self.findLookupAtOffset(module_env, segment, target_offset, best_size)) |found| {
                        result = found;
                    }
                }
            },
            .e_list => |list| {
                // Check list elements for lookups
                const elems = module_env.store.sliceExpr(list.elems);
                for (elems) |elem| {
                    if (self.findLookupAtOffset(module_env, elem, target_offset, best_size)) |found| {
                        result = found;
                    }
                }
            },
            .e_tuple => |tuple| {
                // Check tuple elements for lookups
                const elems = module_env.store.sliceExpr(tuple.elems);
                for (elems) |elem| {
                    if (self.findLookupAtOffset(module_env, elem, target_offset, best_size)) |found| {
                        result = found;
                    }
                }
            },
            .e_record => |rec| {
                // Check record field values for lookups
                const fields = module_env.store.sliceRecordFields(rec.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (self.findLookupAtOffset(module_env, field.value, target_offset, best_size)) |found| {
                        result = found;
                    }
                }
            },
            else => {},
        }

        return result;
    }

    /// Find the type annotation at the given offset and return a DefinitionResult.
    /// This recursively walks type annotation trees to find the most specific type at the cursor.
    fn findTypeAnnoAtOffset(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        type_anno_idx: CIR.TypeAnno.Idx,
        target_offset: u32,
    ) ?DefinitionResult {
        const region = module_env.store.getTypeAnnoRegion(type_anno_idx);
        if (!regionContainsOffset(region, target_offset)) return null;

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
                        const range = regionToRange(module_env, decl_region) orelse return null;
                        // For local definitions, we need the current file URI
                        // Since we don't have it here, we return null and let the caller handle it
                        // Actually, we can construct a result with a marker that means "same file"
                        return DefinitionResult{
                            .uri = "", // Empty URI means same file - caller should fill in
                            .range = range,
                        };
                    },
                    .builtin, .external => {
                        // Builtin or external type - find the module
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
                        const range = regionToRange(module_env, decl_region) orelse return null;
                        return DefinitionResult{
                            .uri = "", // Empty URI means same file - caller should fill in
                            .range = range,
                        };
                    },
                    .builtin, .external => {
                        // Builtin or external type - find the module
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
                        .s_decl_gen => |d| if (d.anno) |anno_idx| module_env.store.getAnnotation(anno_idx).anno else null,
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
                    const stmt_parts = getStatementParts(stmt);
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

    /// Result of finding a type at an offset
    const TypeAtOffsetResult = struct {
        type_var: types.Var,
        region: Region,
    };

    /// Find the type variable for the expression at the given byte offset.
    /// Traverses all expressions in the module to find the narrowest one containing the offset.
    fn findTypeAtOffset(self: *SyntaxChecker, module_env: *ModuleEnv, target_offset: u32) ?TypeAtOffsetResult {
        var best_result: ?TypeAtOffsetResult = null;
        var best_size: u32 = std.math.maxInt(u32);

        // Iterate through all definitions in the module
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            // Check the expression
            const expr_idx = def.expr;
            const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
            const expr_region = module_env.store.getRegionAt(expr_node_idx);

            if (regionContainsOffset(expr_region, target_offset)) {
                const size = expr_region.end.offset - expr_region.start.offset;
                if (size < best_size) {
                    best_size = size;
                    best_result = .{
                        .type_var = ModuleEnv.varFrom(expr_idx),
                        .region = expr_region,
                    };
                }

                // Also check nested expressions within this definition
                if (self.findNestedTypeAtOffset(module_env, expr_idx, target_offset, &best_size)) |nested| {
                    best_result = nested;
                }
            }

            // Check the pattern
            const pattern_idx = def.pattern;
            const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
            const pattern_region = module_env.store.getRegionAt(pattern_node_idx);

            if (regionContainsOffset(pattern_region, target_offset)) {
                const size = pattern_region.end.offset - pattern_region.start.offset;
                if (size < best_size) {
                    best_size = size;
                    best_result = .{
                        .type_var = ModuleEnv.varFrom(pattern_idx),
                        .region = pattern_region,
                    };
                }
            }

            // Check if cursor is in the annotation's TypeAnno region (for defs with type annotations)
            if (def.annotation) |anno_idx| {
                const annotation = module_env.store.getAnnotation(anno_idx);
                const type_anno_region = module_env.store.getTypeAnnoRegion(annotation.anno);
                if (regionContainsOffset(type_anno_region, target_offset)) {
                    const size = type_anno_region.end.offset - type_anno_region.start.offset;
                    if (size < best_size) {
                        best_size = size;
                        best_result = .{
                            .type_var = ModuleEnv.varFrom(pattern_idx),
                            .region = type_anno_region,
                        };
                    }
                }
                // Also check the Annotation's region (the identifier part like "myFunc" in "myFunc : T")
                const anno_region = module_env.store.getAnnotationRegion(anno_idx);
                if (regionContainsOffset(anno_region, target_offset)) {
                    const size = anno_region.end.offset - anno_region.start.offset;
                    if (size < best_size) {
                        best_size = size;
                        best_result = .{
                            .type_var = ModuleEnv.varFrom(pattern_idx),
                            .region = anno_region,
                        };
                    }
                }
            }
        }

        // Also iterate through all statements (apps use statements for their main code)
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            // Extract pattern and expression from the statement based on its type
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.expr) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (regionContainsOffset(expr_region, target_offset)) {
                    const size = expr_region.end.offset - expr_region.start.offset;
                    if (size < best_size) {
                        best_size = size;
                        best_result = .{
                            .type_var = ModuleEnv.varFrom(expr_idx),
                            .region = expr_region,
                        };
                    }

                    // Also check nested expressions within this statement
                    if (self.findNestedTypeAtOffset(module_env, expr_idx, target_offset, &best_size)) |nested| {
                        best_result = nested;
                    }
                }
            }

            if (stmt_parts.expr2) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);

                if (regionContainsOffset(expr_region, target_offset)) {
                    const size = expr_region.end.offset - expr_region.start.offset;
                    if (size < best_size) {
                        best_size = size;
                        best_result = .{
                            .type_var = ModuleEnv.varFrom(expr_idx),
                            .region = expr_region,
                        };
                    }

                    // Also check nested expressions within this statement
                    if (self.findNestedTypeAtOffset(module_env, expr_idx, target_offset, &best_size)) |nested| {
                        best_result = nested;
                    }
                }
            }

            if (stmt_parts.pattern) |pattern_idx| {
                const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
                const pattern_region = module_env.store.getRegionAt(pattern_node_idx);

                if (regionContainsOffset(pattern_region, target_offset)) {
                    const size = pattern_region.end.offset - pattern_region.start.offset;
                    if (size < best_size) {
                        best_size = size;
                        best_result = .{
                            .type_var = ModuleEnv.varFrom(pattern_idx),
                            .region = pattern_region,
                        };
                    }
                }
            }

            // For declarations with annotations, check if cursor is in the TypeAnno region
            // The TypeAnno region covers the actual type annotation (e.g., "Animal" in "dog : Animal")
            const stmt_anno_idx: ?CIR.Annotation.Idx = switch (stmt) {
                .s_decl => |d| d.anno,
                .s_decl_gen => |d| d.anno,
                else => null,
            };
            if (stmt_anno_idx) |actual_anno_idx| {
                // Get the TypeAnno's region directly - this covers the type on the annotation line
                const annotation = module_env.store.getAnnotation(actual_anno_idx);
                const type_anno_region = module_env.store.getTypeAnnoRegion(annotation.anno);
                if (regionContainsOffset(type_anno_region, target_offset)) {
                    if (stmt_parts.pattern) |pattern_idx| {
                        const size = type_anno_region.end.offset - type_anno_region.start.offset;
                        if (size < best_size) {
                            best_size = size;
                            best_result = .{
                                .type_var = ModuleEnv.varFrom(pattern_idx),
                                .region = type_anno_region,
                            };
                        }
                    }
                }
                // Also check the Annotation's region (covers the identifier like "dog" in "dog : Animal")
                const anno_region = module_env.store.getAnnotationRegion(actual_anno_idx);
                if (regionContainsOffset(anno_region, target_offset)) {
                    if (stmt_parts.pattern) |pattern_idx| {
                        const size = anno_region.end.offset - anno_region.start.offset;
                        if (size < best_size) {
                            best_size = size;
                            best_result = .{
                                .type_var = ModuleEnv.varFrom(pattern_idx),
                                .region = anno_region,
                            };
                        }
                    }
                }
            }

            // Handle type annotation statements specially (orphaned annotations without matching declarations)
            // When hovering on a type annotation like "dog : Animal", find the corresponding declaration
            if (stmt == .s_type_anno) {
                const type_anno = stmt.s_type_anno;
                const stmt_region = module_env.store.getStatementRegion(stmt_idx);

                if (regionContainsOffset(stmt_region, target_offset)) {
                    const anno_ident = type_anno.name;

                    // Search through all statements for a matching declaration
                    for (local_statements_slice) |other_stmt_idx| {
                        const other_stmt = module_env.store.getStatement(other_stmt_idx);
                        const pattern_idx = switch (other_stmt) {
                            .s_decl => |d| d.pattern,
                            .s_decl_gen => |d| d.pattern,
                            .s_var => |v| v.pattern_idx,
                            else => continue,
                        };

                        const pattern = module_env.store.getPattern(pattern_idx);
                        const pattern_ident = switch (pattern) {
                            .assign => |a| a.ident,
                            else => continue,
                        };

                        // Compare identifier indices
                        if (pattern_ident.idx == anno_ident.idx) {
                            // Found matching declaration - return the pattern's type
                            const size = stmt_region.end.offset - stmt_region.start.offset;
                            if (size < best_size) {
                                best_size = size;
                                best_result = .{
                                    .type_var = ModuleEnv.varFrom(pattern_idx),
                                    .region = stmt_region,
                                };
                            }
                            break;
                        }
                    }

                    // Also check definitions (modules use defs, apps use statements)
                    const anno_defs_slice = module_env.store.sliceDefs(module_env.all_defs);
                    for (anno_defs_slice) |def_idx| {
                        const def = module_env.store.getDef(def_idx);
                        const pattern = module_env.store.getPattern(def.pattern);
                        const pattern_ident = switch (pattern) {
                            .assign => |a| a.ident,
                            else => continue,
                        };

                        if (pattern_ident.idx == anno_ident.idx) {
                            const size = stmt_region.end.offset - stmt_region.start.offset;
                            if (size < best_size) {
                                best_size = size;
                                best_result = .{
                                    .type_var = ModuleEnv.varFrom(def.pattern),
                                    .region = stmt_region,
                                };
                            }
                            break;
                        }
                    }
                }
            }
        }

        return best_result;
    }

    /// Helper to extract pattern and expression from a statement
    const StatementParts = struct {
        pattern: ?CIR.Pattern.Idx,
        expr: ?CIR.Expr.Idx,
        /// Second expression for statements that have multiple (e.g., while has cond + body)
        expr2: ?CIR.Expr.Idx,
    };

    fn getStatementParts(stmt: CIR.Statement) StatementParts {
        return switch (stmt) {
            .s_decl => |d| .{ .pattern = d.pattern, .expr = d.expr, .expr2 = null },
            .s_decl_gen => |d| .{ .pattern = d.pattern, .expr = d.expr, .expr2 = null },
            .s_var => |d| .{ .pattern = d.pattern_idx, .expr = d.expr, .expr2 = null },
            .s_reassign => |d| .{ .pattern = d.pattern_idx, .expr = d.expr, .expr2 = null },
            .s_expr => |e| .{ .pattern = null, .expr = e.expr, .expr2 = null },
            .s_for => |f| .{ .pattern = f.patt, .expr = f.expr, .expr2 = f.body },
            .s_while => |w| .{ .pattern = null, .expr = w.cond, .expr2 = w.body },
            .s_dbg => |d| .{ .pattern = null, .expr = d.expr, .expr2 = null },
            .s_expect => |e| .{ .pattern = null, .expr = e.body, .expr2 = null },
            .s_crash => |_| .{ .pattern = null, .expr = null, .expr2 = null },
            .s_break => .{ .pattern = null, .expr = null, .expr2 = null },
            .s_return => |r| .{ .pattern = null, .expr = r.expr, .expr2 = null },
            .s_import => |_| .{ .pattern = null, .expr = null, .expr2 = null },
            .s_alias_decl => |_| .{ .pattern = null, .expr = null, .expr2 = null },
            .s_nominal_decl => |_| .{ .pattern = null, .expr = null, .expr2 = null },
            .s_type_anno => |_| .{ .pattern = null, .expr = null, .expr2 = null },
            .s_type_var_alias => |_| .{ .pattern = null, .expr = null, .expr2 = null },
            .s_runtime_error => |_| .{ .pattern = null, .expr = null, .expr2 = null },
        };
    }

    /// Recursively find the narrowest expression containing the target offset
    fn findNestedTypeAtOffset(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        target_offset: u32,
        best_size: *u32,
    ) ?TypeAtOffsetResult {
        const expr = module_env.store.getExpr(expr_idx);
        var result: ?TypeAtOffsetResult = null;

        switch (expr) {
            .e_lambda => |lambda| {
                // Check lambda body
                result = self.checkExprAndRecurse(module_env, lambda.body, target_offset, best_size);
            },
            .e_closure => |closure| {
                // Closure contains a reference to the lambda - check that
                result = self.checkExprAndRecurse(module_env, closure.lambda_idx, target_offset, best_size);
            },
            .e_block => |block| {
                // Check statements in the block
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);
                    const stmt_parts = getStatementParts(stmt);

                    if (stmt_parts.expr) |stmt_expr| {
                        if (self.checkExprAndRecurse(module_env, stmt_expr, target_offset, best_size)) |r| {
                            result = r;
                        }
                    }

                    if (stmt_parts.expr2) |stmt_expr| {
                        if (self.checkExprAndRecurse(module_env, stmt_expr, target_offset, best_size)) |r| {
                            result = r;
                        }
                    }

                    if (stmt_parts.pattern) |pattern_idx| {
                        const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
                        const pattern_region = module_env.store.getRegionAt(pattern_node_idx);
                        if (regionContainsOffset(pattern_region, target_offset)) {
                            const size = pattern_region.end.offset - pattern_region.start.offset;
                            if (size < best_size.*) {
                                best_size.* = size;
                                result = .{
                                    .type_var = ModuleEnv.varFrom(pattern_idx),
                                    .region = pattern_region,
                                };
                            }
                        }
                    }

                    // For declarations with annotations, check if cursor is in the TypeAnno region
                    // The TypeAnno region covers the actual type annotation (e.g., "Animal" in "dog : Animal")
                    const block_anno_idx: ?CIR.Annotation.Idx = switch (stmt) {
                        .s_decl => |d| d.anno,
                        .s_decl_gen => |d| d.anno,
                        else => null,
                    };
                    if (block_anno_idx) |anno_idx| {
                        // Get the TypeAnno's region directly - this covers the type on the annotation line
                        const annotation = module_env.store.getAnnotation(anno_idx);
                        const type_anno_region = module_env.store.getTypeAnnoRegion(annotation.anno);
                        if (regionContainsOffset(type_anno_region, target_offset)) {
                            if (stmt_parts.pattern) |pattern_idx| {
                                const size = type_anno_region.end.offset - type_anno_region.start.offset;
                                if (size < best_size.*) {
                                    best_size.* = size;
                                    result = .{
                                        .type_var = ModuleEnv.varFrom(pattern_idx),
                                        .region = type_anno_region,
                                    };
                                }
                            }
                        }

                        // Check if cursor is in the statement's region (which now includes the annotation line)
                        // but NOT in the expression, pattern, or type annotation regions.
                        // This means cursor is on the identifier part of the annotation line (e.g., "dog" in "dog : Animal")
                        const stmt_region = module_env.store.getStatementRegion(stmt_idx);
                        if (regionContainsOffset(stmt_region, target_offset)) {
                            const in_type_anno = regionContainsOffset(type_anno_region, target_offset);
                            const in_expr = if (stmt_parts.expr) |check_expr_idx| blk: {
                                const check_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(check_expr_idx));
                                break :blk regionContainsOffset(module_env.store.getRegionAt(check_node_idx), target_offset);
                            } else false;
                            const in_pattern = if (stmt_parts.pattern) |check_pat_idx| blk: {
                                const check_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(check_pat_idx));
                                break :blk regionContainsOffset(module_env.store.getRegionAt(check_node_idx), target_offset);
                            } else false;

                            // If not in any of those regions, we're on the annotation identifier
                            if (!in_type_anno and !in_expr and !in_pattern) {
                                if (stmt_parts.pattern) |pattern_idx| {
                                    // Use a small size to prefer this match over larger containing regions
                                    const ident_approx_size: u32 = 10; // Approximate identifier size
                                    if (ident_approx_size < best_size.*) {
                                        best_size.* = ident_approx_size;
                                        result = .{
                                            .type_var = ModuleEnv.varFrom(pattern_idx),
                                            .region = stmt_region,
                                        };
                                    }
                                }
                            }
                        }

                        // Also check the Annotation's region (covers the identifier like "dog" in "dog : Animal")
                        const anno_region = module_env.store.getAnnotationRegion(anno_idx);
                        if (regionContainsOffset(anno_region, target_offset)) {
                            if (stmt_parts.pattern) |pattern_idx| {
                                const size = anno_region.end.offset - anno_region.start.offset;
                                if (size < best_size.*) {
                                    best_size.* = size;
                                    result = .{
                                        .type_var = ModuleEnv.varFrom(pattern_idx),
                                        .region = anno_region,
                                    };
                                }
                            }
                        }
                    }

                    // Handle type annotation statements in blocks (orphaned annotations)
                    if (stmt == .s_type_anno) {
                        const type_anno = stmt.s_type_anno;
                        const stmt_region = module_env.store.getStatementRegion(stmt_idx);
                        if (regionContainsOffset(stmt_region, target_offset)) {
                            const anno_ident = type_anno.name;

                            // Search through block statements for matching declaration
                            for (stmts) |other_stmt_idx| {
                                const other_stmt = module_env.store.getStatement(other_stmt_idx);
                                const pattern_idx = switch (other_stmt) {
                                    .s_decl => |d| d.pattern,
                                    .s_decl_gen => |d| d.pattern,
                                    .s_var => |v| v.pattern_idx,
                                    else => continue,
                                };

                                const pattern = module_env.store.getPattern(pattern_idx);
                                const pattern_ident = switch (pattern) {
                                    .assign => |a| a.ident,
                                    else => continue,
                                };

                                if (pattern_ident.idx == anno_ident.idx) {
                                    const size = stmt_region.end.offset - stmt_region.start.offset;
                                    if (size < best_size.*) {
                                        best_size.* = size;
                                        result = .{
                                            .type_var = ModuleEnv.varFrom(pattern_idx),
                                            .region = stmt_region,
                                        };
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                // Check final expression
                if (self.checkExprAndRecurse(module_env, block.final_expr, target_offset, best_size)) |r| {
                    result = r;
                }
            },
            .e_if => |if_expr| {
                // Check all branches and final else
                const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    if (self.checkExprAndRecurse(module_env, branch.cond, target_offset, best_size)) |r| {
                        result = r;
                    }
                    if (self.checkExprAndRecurse(module_env, branch.body, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
                if (self.checkExprAndRecurse(module_env, if_expr.final_else, target_offset, best_size)) |r| {
                    result = r;
                }
            },
            .e_match => |match_expr| {
                // Check the condition
                if (self.checkExprAndRecurse(module_env, match_expr.cond, target_offset, best_size)) |r| {
                    result = r;
                }
                // Check all branches
                const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);

                    // Check branch patterns
                    const pattern_indices = module_env.store.sliceMatchBranchPatterns(branch.patterns);
                    for (pattern_indices) |pat_idx| {
                        const branch_pattern = module_env.store.getMatchBranchPattern(pat_idx);
                        const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(branch_pattern.pattern));
                        const pattern_region = module_env.store.getRegionAt(pattern_node_idx);
                        if (regionContainsOffset(pattern_region, target_offset)) {
                            const size = pattern_region.end.offset - pattern_region.start.offset;
                            if (size < best_size.*) {
                                best_size.* = size;
                                result = .{
                                    .type_var = ModuleEnv.varFrom(branch_pattern.pattern),
                                    .region = pattern_region,
                                };
                            }
                            // Also check nested patterns (e.g., Ok(id) -> check id)
                            if (checkPatternAndRecurse(module_env, branch_pattern.pattern, target_offset, best_size)) |r| {
                                result = r;
                            }
                        }
                    }

                    if (self.checkExprAndRecurse(module_env, branch.value, target_offset, best_size)) |r| {
                        result = r;
                    }
                    if (branch.guard) |guard| {
                        if (self.checkExprAndRecurse(module_env, guard, target_offset, best_size)) |r| {
                            result = r;
                        }
                    }
                }
            },
            .e_call => |call| {
                // Check function and arguments
                if (self.checkExprAndRecurse(module_env, call.func, target_offset, best_size)) |r| {
                    result = r;
                }
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg| {
                    if (self.checkExprAndRecurse(module_env, arg, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
            },
            .e_list => |list| {
                const elems = module_env.store.sliceExpr(list.elems);
                for (elems) |elem| {
                    if (self.checkExprAndRecurse(module_env, elem, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
            },
            .e_record => |record| {
                const field_indices = module_env.store.sliceRecordFields(record.fields);
                for (field_indices) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (self.checkExprAndRecurse(module_env, field.value, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
                if (record.ext) |ext| {
                    if (self.checkExprAndRecurse(module_env, ext, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
            },
            .e_tuple => |tuple| {
                const elems = module_env.store.sliceExpr(tuple.elems);
                for (elems) |elem| {
                    if (self.checkExprAndRecurse(module_env, elem, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
            },
            .e_binop => |binop| {
                if (self.checkExprAndRecurse(module_env, binop.lhs, target_offset, best_size)) |r| {
                    result = r;
                }
                if (self.checkExprAndRecurse(module_env, binop.rhs, target_offset, best_size)) |r| {
                    result = r;
                }
            },
            .e_unary_minus => |unary| {
                if (self.checkExprAndRecurse(module_env, unary.expr, target_offset, best_size)) |r| {
                    result = r;
                }
            },
            .e_unary_not => |unary| {
                if (self.checkExprAndRecurse(module_env, unary.expr, target_offset, best_size)) |r| {
                    result = r;
                }
            },
            .e_str => |str| {
                // String with interpolation - check all segments
                const segments = module_env.store.sliceExpr(str.span);
                for (segments) |segment| {
                    if (self.checkExprAndRecurse(module_env, segment, target_offset, best_size)) |r| {
                        result = r;
                    }
                }
            },
            .e_dot_access => |dot| {
                // Check the receiver
                if (self.checkExprAndRecurse(module_env, dot.receiver, target_offset, best_size)) |r| {
                    result = r;
                }
                // Check the arguments if present
                if (dot.args) |args_span| {
                    const args = module_env.store.sliceExpr(args_span);
                    for (args) |arg| {
                        if (self.checkExprAndRecurse(module_env, arg, target_offset, best_size)) |r| {
                            result = r;
                        }
                    }
                }
            },
            else => {
                // Other expression types don't have nested expressions to traverse
            },
        }

        return result;
    }

    /// Helper to check if an expression contains the offset and recurse into it
    fn checkExprAndRecurse(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        target_offset: u32,
        best_size: *u32,
    ) ?TypeAtOffsetResult {
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        const region = module_env.store.getRegionAt(node_idx);

        if (regionContainsOffset(region, target_offset)) {
            const size = region.end.offset - region.start.offset;
            var result: ?TypeAtOffsetResult = null;

            if (size < best_size.*) {
                best_size.* = size;
                result = .{
                    .type_var = ModuleEnv.varFrom(expr_idx),
                    .region = region,
                };
            }

            // Recurse into this expression
            if (self.findNestedTypeAtOffset(module_env, expr_idx, target_offset, best_size)) |nested| {
                result = nested;
            }

            return result;
        }

        return null;
    }

    /// Helper to check if a pattern contains the offset and recurse into nested patterns
    fn checkPatternAndRecurse(
        module_env: *ModuleEnv,
        pattern_idx: CIR.Pattern.Idx,
        target_offset: u32,
        best_size: *u32,
    ) ?TypeAtOffsetResult {
        const pattern = module_env.store.getPattern(pattern_idx);
        var result: ?TypeAtOffsetResult = null;

        switch (pattern) {
            .applied_tag => |tag| {
                // Check nested patterns in tag args (e.g., Ok(id) -> check id)
                const args = module_env.store.slicePatterns(tag.args);
                for (args) |arg_idx| {
                    const arg_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(arg_idx));
                    const arg_region = module_env.store.getRegionAt(arg_node_idx);
                    if (regionContainsOffset(arg_region, target_offset)) {
                        const size = arg_region.end.offset - arg_region.start.offset;
                        if (size < best_size.*) {
                            best_size.* = size;
                            result = .{
                                .type_var = ModuleEnv.varFrom(arg_idx),
                                .region = arg_region,
                            };
                        }
                    }
                }
            },
            .as => |as_pat| {
                // Check the nested pattern in an `as` pattern
                const nested_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(as_pat.pattern));
                const nested_region = module_env.store.getRegionAt(nested_node_idx);
                if (regionContainsOffset(nested_region, target_offset)) {
                    const size = nested_region.end.offset - nested_region.start.offset;
                    if (size < best_size.*) {
                        best_size.* = size;
                        result = .{
                            .type_var = ModuleEnv.varFrom(as_pat.pattern),
                            .region = nested_region,
                        };
                    }
                }
            },
            .record_destructure => |record| {
                // Check each destructured field
                const destructs = module_env.store.sliceRecordDestructs(record.destructs);
                for (destructs) |destruct_idx| {
                    const destruct = module_env.store.getRecordDestruct(destruct_idx);
                    const nested_pattern_idx = destruct.kind.toPatternIdx();
                    const nested_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(nested_pattern_idx));
                    const nested_region = module_env.store.getRegionAt(nested_node_idx);
                    if (regionContainsOffset(nested_region, target_offset)) {
                        const size = nested_region.end.offset - nested_region.start.offset;
                        if (size < best_size.*) {
                            best_size.* = size;
                            result = .{
                                .type_var = ModuleEnv.varFrom(nested_pattern_idx),
                                .region = nested_region,
                            };
                        }
                    }
                }
            },
            .tuple => |tuple| {
                // Check each pattern in the tuple
                const patterns = module_env.store.slicePatterns(tuple.patterns);
                for (patterns) |pat_idx| {
                    const pat_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pat_idx));
                    const pat_region = module_env.store.getRegionAt(pat_node_idx);
                    if (regionContainsOffset(pat_region, target_offset)) {
                        const size = pat_region.end.offset - pat_region.start.offset;
                        if (size < best_size.*) {
                            best_size.* = size;
                            result = .{
                                .type_var = ModuleEnv.varFrom(pat_idx),
                                .region = pat_region,
                            };
                        }
                    }
                }
            },
            .list => |list| {
                // Check each pattern in the list
                const patterns = module_env.store.slicePatterns(list.patterns);
                for (patterns) |pat_idx| {
                    const pat_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pat_idx));
                    const pat_region = module_env.store.getRegionAt(pat_node_idx);
                    if (regionContainsOffset(pat_region, target_offset)) {
                        const size = pat_region.end.offset - pat_region.start.offset;
                        if (size < best_size.*) {
                            best_size.* = size;
                            result = .{
                                .type_var = ModuleEnv.varFrom(pat_idx),
                                .region = pat_region,
                            };
                        }
                    }
                }
                // Also check the rest pattern if it exists
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pat_idx| {
                        const rest_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(rest_pat_idx));
                        const rest_region = module_env.store.getRegionAt(rest_node_idx);
                        if (regionContainsOffset(rest_region, target_offset)) {
                            const size = rest_region.end.offset - rest_region.start.offset;
                            if (size < best_size.*) {
                                best_size.* = size;
                                result = .{
                                    .type_var = ModuleEnv.varFrom(rest_pat_idx),
                                    .region = rest_region,
                                };
                            }
                        }
                    }
                }
            },
            .nominal => |nom| {
                // Check the backing pattern
                const backing_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(nom.backing_pattern));
                const backing_region = module_env.store.getRegionAt(backing_node_idx);
                if (regionContainsOffset(backing_region, target_offset)) {
                    const size = backing_region.end.offset - backing_region.start.offset;
                    if (size < best_size.*) {
                        best_size.* = size;
                        result = .{
                            .type_var = ModuleEnv.varFrom(nom.backing_pattern),
                            .region = backing_region,
                        };
                    }
                }
            },
            .nominal_external => |nom_ext| {
                // Check the backing pattern
                const backing_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(nom_ext.backing_pattern));
                const backing_region = module_env.store.getRegionAt(backing_node_idx);
                if (regionContainsOffset(backing_region, target_offset)) {
                    const size = backing_region.end.offset - backing_region.start.offset;
                    if (size < best_size.*) {
                        best_size.* = size;
                        result = .{
                            .type_var = ModuleEnv.varFrom(nom_ext.backing_pattern),
                            .region = backing_region,
                        };
                    }
                }
            },
            // Simple patterns with no nested patterns
            .assign, .num_literal, .small_dec_literal, .dec_literal, .frac_f32_literal, .frac_f64_literal, .str_literal, .underscore, .runtime_error => {},
        }

        return result;
    }

    /// Check if a region contains the given byte offset
    fn regionContainsOffset(region: Region, offset: u32) bool {
        return offset >= region.start.offset and offset <= region.end.offset;
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
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
        defer self.allocator.free(absolute_path);

        self.mutex.lock();
        defer self.mutex.unlock();

        var env = try self.createFreshBuildEnv();

        var provider_state = OverrideProvider{
            .override_path = absolute_path,
            .override_text = override_text,
        };
        const provider: ?FileProvider = if (override_text != null) .{
            .ctx = &provider_state,
            .read = OverrideProvider.read,
        } else null;
        env.setFileProvider(provider);
        defer env.setFileProvider(null);

        const dir_slice = std.fs.path.dirname(absolute_path) orelse ".";
        const dir_owned = try self.allocator.dupe(u8, dir_slice);
        defer self.allocator.free(dir_owned);
        const prev_cwd = std.process.getCwdAlloc(self.allocator) catch null;
        defer if (prev_cwd) |cwd| {
            std.process.changeCurDir(cwd) catch {};
            self.allocator.free(cwd);
        };
        std.process.changeCurDir(dir_owned) catch {};

        self.logDebug(.build, "highlights: building {s}", .{absolute_path});
        env.build(absolute_path) catch |err| {
            self.logDebug(.build, "highlights: build failed for {s}: {s}", .{ absolute_path, @errorName(err) });
            return null;
        };

        // Drain reports but ignore them (must still free them to avoid leaks)
        const drained = env.drainReports() catch return null;
        defer self.freeDrainedWithReports(drained);

        // Get any available scheduler and find the root module
        const app_sched = env.schedulers.get("app") orelse blk: {
            var sched_it = env.schedulers.iterator();
            while (sched_it.next()) |entry| {
                const sched = entry.value_ptr.*;
                if (sched.getRootModule()) |rm| {
                    if (rm.env != null) {
                        break :blk sched;
                    }
                }
            }
            return null;
        };
        const root_module = app_sched.getRootModule() orelse return null;
        const module_env = if (root_module.env) |*e| e else return null;

        // Convert LSP position to byte offset
        const target_offset = positionToOffset(module_env, line, character) orelse return null;

        // Find the pattern_idx at this position
        const target_pattern = self.findPatternAtOffset(module_env, target_offset) orelse return null;

        // Collect all references to this pattern
        var regions = std.ArrayList(LspRange){};
        errdefer regions.deinit(self.allocator);

        // Add the definition itself
        const def_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(target_pattern));
        const def_region = module_env.store.getRegionAt(def_node_idx);
        if (regionToRange(module_env, def_region)) |range| {
            try regions.append(self.allocator, range);
        }

        // Find all lookups that reference this pattern
        try self.collectLookupReferences(module_env, target_pattern, &regions);

        return HighlightResult{
            .regions = try regions.toOwnedSlice(self.allocator),
        };
    }

    /// Find the pattern_idx at the given offset.
    /// Returns the pattern being defined or referenced at that position.
    fn findPatternAtOffset(self: *SyntaxChecker, module_env: *ModuleEnv, target_offset: u32) ?CIR.Pattern.Idx {
        // First, check if we're on a lookup expression
        var best_expr: ?CIR.Expr.Idx = null;
        var best_size: u32 = std.math.maxInt(u32);

        // Check definitions
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);

            // Check if cursor is on the pattern (definition site)
            const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.pattern));
            const pattern_region = module_env.store.getRegionAt(pattern_node_idx);
            if (regionContainsOffset(pattern_region, target_offset)) {
                const size = pattern_region.end.offset - pattern_region.start.offset;
                if (size < best_size) {
                    return def.pattern; // Return the pattern directly
                }
            }

            // Check if cursor is on a lookup within this expression
            const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.expr));
            const expr_region = module_env.store.getRegionAt(expr_node_idx);
            if (regionContainsOffset(expr_region, target_offset)) {
                if (self.findLookupAtOffset(module_env, def.expr, target_offset, &best_size)) |found| {
                    best_expr = found;
                }
            }
        }

        // Check statements
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);

            // Check if cursor is on a pattern in a statement
            if (stmt_parts.pattern) |pattern_idx| {
                const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
                const pattern_region = module_env.store.getRegionAt(pattern_node_idx);
                if (regionContainsOffset(pattern_region, target_offset)) {
                    const size = pattern_region.end.offset - pattern_region.start.offset;
                    if (size < best_size) {
                        return pattern_idx;
                    }
                }
            }

            // Check expressions in the statement
            if (stmt_parts.expr) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);
                if (regionContainsOffset(expr_region, target_offset)) {
                    if (self.findLookupAtOffset(module_env, expr_idx, target_offset, &best_size)) |found| {
                        best_expr = found;
                    }
                }
            }

            if (stmt_parts.expr2) |expr_idx| {
                const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const expr_region = module_env.store.getRegionAt(expr_node_idx);
                if (regionContainsOffset(expr_region, target_offset)) {
                    if (self.findLookupAtOffset(module_env, expr_idx, target_offset, &best_size)) |found| {
                        best_expr = found;
                    }
                }
            }
        }

        // If we found a lookup, extract the pattern it references
        if (best_expr) |expr_idx| {
            const expr = module_env.store.getExpr(expr_idx);
            switch (expr) {
                .e_lookup_local => |lookup| return lookup.pattern_idx,
                else => {},
            }
        }

        return null;
    }

    /// Collect all e_lookup_local expressions that reference the given pattern.
    fn collectLookupReferences(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        target_pattern: CIR.Pattern.Idx,
        regions: *std.ArrayList(LspRange),
    ) !void {
        // Check all definitions
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            try self.collectLookupsInExpr(module_env, def.expr, target_pattern, regions);
        }

        // Check all statements
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.expr) |expr_idx| {
                try self.collectLookupsInExpr(module_env, expr_idx, target_pattern, regions);
            }
            if (stmt_parts.expr2) |expr_idx| {
                try self.collectLookupsInExpr(module_env, expr_idx, target_pattern, regions);
            }
        }
    }

    /// Recursively collect lookups to target_pattern within an expression.
    fn collectLookupsInExpr(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        target_pattern: CIR.Pattern.Idx,
        regions: *std.ArrayList(LspRange),
    ) !void {
        const expr = module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_lookup_local => |lookup| {
                if (@intFromEnum(lookup.pattern_idx) == @intFromEnum(target_pattern)) {
                    const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                    const region = module_env.store.getRegionAt(node_idx);
                    if (regionToRange(module_env, region)) |range| {
                        try regions.append(self.allocator, range);
                    }
                }
            },
            .e_closure => |closure| {
                try self.collectLookupsInExpr(module_env, closure.lambda_idx, target_pattern, regions);
            },
            .e_lambda => |lambda| {
                try self.collectLookupsInExpr(module_env, lambda.body, target_pattern, regions);
            },
            .e_call => |call| {
                try self.collectLookupsInExpr(module_env, call.func, target_pattern, regions);
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg| {
                    try self.collectLookupsInExpr(module_env, arg, target_pattern, regions);
                }
            },
            .e_block => |block| {
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);
                    const stmt_parts = getStatementParts(stmt);
                    if (stmt_parts.expr) |stmt_expr| {
                        try self.collectLookupsInExpr(module_env, stmt_expr, target_pattern, regions);
                    }
                    if (stmt_parts.expr2) |stmt_expr| {
                        try self.collectLookupsInExpr(module_env, stmt_expr, target_pattern, regions);
                    }
                }
                try self.collectLookupsInExpr(module_env, block.final_expr, target_pattern, regions);
            },
            .e_if => |if_expr| {
                const branches = module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.collectLookupsInExpr(module_env, branch.cond, target_pattern, regions);
                    try self.collectLookupsInExpr(module_env, branch.body, target_pattern, regions);
                }
                try self.collectLookupsInExpr(module_env, if_expr.final_else, target_pattern, regions);
            },
            .e_match => |match_expr| {
                try self.collectLookupsInExpr(module_env, match_expr.cond, target_pattern, regions);
                const branches = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.collectLookupsInExpr(module_env, branch.value, target_pattern, regions);
                    if (branch.guard) |guard| {
                        try self.collectLookupsInExpr(module_env, guard, target_pattern, regions);
                    }
                }
            },
            .e_list => |list| {
                const elems = module_env.store.sliceExpr(list.elems);
                for (elems) |elem| {
                    try self.collectLookupsInExpr(module_env, elem, target_pattern, regions);
                }
            },
            .e_tuple => |tuple| {
                const elems = module_env.store.sliceExpr(tuple.elems);
                for (elems) |elem| {
                    try self.collectLookupsInExpr(module_env, elem, target_pattern, regions);
                }
            },
            .e_record => |rec| {
                const fields = module_env.store.sliceRecordFields(rec.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try self.collectLookupsInExpr(module_env, field.value, target_pattern, regions);
                }
            },
            else => {},
        }
    }

    /// Get document symbols (outline) for a file.
    pub fn getDocumentSymbols(
        self: *SyntaxChecker,
        allocator: std.mem.Allocator,
        uri: []const u8,
        source: []const u8,
    ) ![]document_symbol_handler.SymbolInformation {
        const SymbolInformation = document_symbol_handler.SymbolInformation;

        const env = self.build_env orelse return &[_]SymbolInformation{};

        // Convert URI to absolute path to match against module paths
        const path = uri_util.uriToPath(allocator, uri) catch return &[_]SymbolInformation{};
        defer allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(allocator, path) catch
            allocator.dupe(u8, path) catch return &[_]SymbolInformation{};
        defer allocator.free(absolute_path);

        // Find the module matching this file path across all schedulers
        const module_env = blk: {
            var sched_it = env.schedulers.iterator();
            while (sched_it.next()) |entry| {
                const sched = entry.value_ptr.*;
                // Search for module by path in this scheduler
                if (sched.findModuleByPath(absolute_path)) |module| {
                    if (module.env) |*e| {
                        break :blk e;
                    }
                }
            }
            // Fallback: try the root module of the "app" scheduler
            if (env.schedulers.get("app")) |sched| {
                if (sched.getRootModule()) |rm| {
                    if (rm.env) |*e| {
                        break :blk e;
                    }
                }
            }
            return &[_]SymbolInformation{};
        };

        // Build line offset table
        const line_offsets = buildLineOffsets(source);

        var symbols = std.ArrayList(SymbolInformation){};
        errdefer {
            for (symbols.items) |*sym| {
                allocator.free(sym.name);
            }
            symbols.deinit(allocator);
        }

        // Check top-level definitions (modules/apps store functions here)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (extractSymbolFromDecl(module_env, def.pattern, def.expr, source, uri, &line_offsets)) |symbol| {
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
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                if (stmt_parts.expr) |expr_idx| {
                    if (extractSymbolFromDecl(module_env, pattern_idx, expr_idx, source, uri, &line_offsets)) |symbol| {
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
        return symbols.toOwnedSlice(allocator);
    }

    /// Completion context types
    pub const CompletionContext = union(enum) {
        /// After a dot with a module prefix: "Str." or "List."
        after_module_dot: []const u8,
        /// After a dot with a record variable: "myRecord."
        after_record_dot: struct {
            /// The variable name before the dot
            variable_name: []const u8,
            /// Byte offset of the start of the variable name
            variable_start: u32,
        },
        /// After a colon (type annotation context)
        after_colon,
        /// General expression context
        expression,
    };

    /// Detect completion context from source text at the given position
    pub fn detectCompletionContext(source: []const u8, line: u32, character: u32) CompletionContext {
        std.debug.print("detectCompletionContext: line={d}, character={d}, source.len={d}\n", .{ line, character, source.len });

        // Convert line/character to byte offset
        var current_line: u32 = 0;
        var line_start: usize = 0;

        for (source, 0..) |c, i| {
            if (current_line == line) {
                line_start = i;
                break;
            }
            if (c == '\n') {
                current_line += 1;
            }
        }

        const cursor_offset = line_start + character;
        std.debug.print("detectCompletionContext: line_start={d}, cursor_offset={d}\n", .{ line_start, cursor_offset });

        if (cursor_offset == 0 or cursor_offset > source.len) {
            std.debug.print("detectCompletionContext: cursor out of bounds, returning .expression\n", .{});
            return .expression;
        }

        // Look backwards from cursor to find context
        var pos = cursor_offset;

        // Show what's around the cursor
        const start_show = if (pos > 20) pos - 20 else 0;
        const end_show = if (pos + 5 < source.len) pos + 5 else source.len;
        std.debug.print("detectCompletionContext: source around cursor [{d}..{d}]: '{s}'\n", .{ start_show, end_show, source[start_show..end_show] });

        // Skip any partial identifier being typed
        while (pos > 0 and (std.ascii.isAlphanumeric(source[pos - 1]) or source[pos - 1] == '_')) {
            pos -= 1;
        }
        std.debug.print("detectCompletionContext: after skipping identifier, pos={d}\n", .{pos});

        // Skip whitespace to find the actual context character
        while (pos > 0 and (source[pos - 1] == ' ' or source[pos - 1] == '\t')) {
            pos -= 1;
        }
        std.debug.print("detectCompletionContext: after skipping whitespace, pos={d}\n", .{pos});

        // Check what's immediately before (after skipping whitespace)
        if (pos > 0) {
            const prev_char = source[pos - 1];
            std.debug.print("detectCompletionContext: prev_char='{c}' (0x{x})\n", .{ prev_char, prev_char });

            if (prev_char == '.') {
                // After a dot - could be module access or record field access
                // Look for identifier before the dot
                const ident_end = pos - 1;
                var ident_start = ident_end;

                while (ident_start > 0 and (std.ascii.isAlphanumeric(source[ident_start - 1]) or source[ident_start - 1] == '_')) {
                    ident_start -= 1;
                }

                if (ident_start < ident_end) {
                    const ident_name = source[ident_start..ident_end];
                    if (ident_name.len > 0) {
                        if (std.ascii.isUpper(ident_name[0])) {
                            // Uppercase - module access (e.g., "Str.")
                            return .{ .after_module_dot = ident_name };
                        } else {
                            // Lowercase - record field access (e.g., "myRecord.")
                            return .{ .after_record_dot = .{
                                .variable_name = ident_name,
                                .variable_start = @intCast(ident_start),
                            } };
                        }
                    }
                }
            } else if (prev_char == ':') {
                return .after_colon;
            }
        }

        return .expression;
    }

    /// Compute byte offset from line and character position in source text
    fn computeOffset(source: []const u8, line: u32, character: u32) u32 {
        var current_line: u32 = 0;
        var line_start: usize = 0;

        for (source, 0..) |c, i| {
            if (current_line == line) {
                line_start = i;
                break;
            }
            if (c == '\n') {
                current_line += 1;
            }
        }

        const offset = line_start + character;
        return @intCast(@min(offset, source.len));
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
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
        defer self.allocator.free(absolute_path);

        self.mutex.lock();
        defer self.mutex.unlock();

        var env = try self.createFreshBuildEnv();

        var provider_state = OverrideProvider{
            .override_path = absolute_path,
            .override_text = override_text,
        };
        const provider: ?FileProvider = if (override_text != null) .{
            .ctx = &provider_state,
            .read = OverrideProvider.read,
        } else null;
        env.setFileProvider(provider);
        defer env.setFileProvider(null);

        const dir_slice = std.fs.path.dirname(absolute_path) orelse ".";
        const dir_owned = try self.allocator.dupe(u8, dir_slice);
        defer self.allocator.free(dir_owned);
        const prev_cwd = std.process.getCwdAlloc(self.allocator) catch null;
        defer if (prev_cwd) |cwd| {
            std.process.changeCurDir(cwd) catch {};
            self.allocator.free(cwd);
        };
        std.process.changeCurDir(dir_owned) catch {};

        std.debug.print("completion: building {s}\n", .{absolute_path});
        const build_succeeded = blk: {
            env.build(absolute_path) catch |err| {
                std.debug.print("completion: build FAILED for {s}: {s}\n", .{ absolute_path, @errorName(err) });
                break :blk false;
            };
            std.debug.print("completion: build SUCCEEDED\n", .{});
            break :blk true;
        };
        std.debug.print("completion: build_succeeded={}\n", .{build_succeeded});

        var build_has_reports = false;

        // Drain reports even on failure to avoid leaks
        if (build_succeeded) {
            const drained = env.drainReports() catch return null;
            std.debug.print("completion: drained reports={d}\n", .{drained.len});
            for (drained) |entry| {
                if (std.mem.eql(u8, entry.abs_path, absolute_path) and entry.reports.len > 0) {
                    build_has_reports = true;
                    break;
                }
            }
            self.freeDrainedWithReports(drained);
        }

        // Detect completion context from source
        const source = override_text orelse "";
        const context = detectCompletionContext(source, line, character);

        // Compute cursor offset for scope-based completions
        const cursor_offset = computeOffset(source, line, character);

        // Collect completions based on context
        var items = std.ArrayList(completion_handler.CompletionItem){};
        errdefer {
            for (items.items) |item| {
                if (item.detail) |d| self.allocator.free(d);
            }
            items.deinit(self.allocator);
        }

        // Try to get the module environment for richer completions
        // ALWAYS try snapshot first for completion - typing usually produces incomplete code
        var used_snapshot = false;
        const module_env_opt: ?*ModuleEnv = blk: {
            if (self.snapshot_envs.get(absolute_path)) |snapshot_env| {
                const snapshot_module_env = self.getModuleEnvByPathInEnv(snapshot_env, absolute_path);
                if (snapshot_module_env) |module_env| {
                    used_snapshot = true;
                    break :blk module_env;
                }
            }

            // Fall back to previous build env if snapshot not available
            if (self.previous_build_env) |previous_env| {
                const prev_module_env = self.getModuleEnvByPathInEnv(previous_env, absolute_path);
                if (prev_module_env) |module_env| {
                    used_snapshot = true;
                    break :blk module_env;
                }
            }

            // Fall back to current build only if no snapshot available and build succeeded
            if (build_succeeded and !build_has_reports) {
                if (self.getModuleEnvByPath(absolute_path)) |module_env| {
                    break :blk module_env;
                }

                const app_sched = env.schedulers.get("app") orelse inner: {
                    var sched_it = env.schedulers.iterator();
                    while (sched_it.next()) |entry| {
                        const sched = entry.value_ptr.*;
                        if (sched.getRootModule()) |rm| {
                            if (rm.env != null) {
                                break :inner sched;
                            }
                        }
                    }
                    break :blk null;
                };
                const root_module = app_sched.getRootModule() orelse break :blk null;
                break :blk if (root_module.env) |*e| e else null;
            }

            break :blk null;
        };

        std.debug.print("completion: context={any}, module_env_opt={any}, build_succeeded={}, used_snapshot={}\n", .{ context, module_env_opt != null, build_succeeded, used_snapshot });

        switch (context) {
            .after_module_dot => |module_name| {
                std.debug.print("completion: after_module_dot for '{s}'", .{module_name});
                // Get completions from the specified module
                try self.addModuleMemberCompletions(&items, env, module_name, module_env_opt);
            },
            .after_record_dot => |record_access| {
                std.debug.print("completion: after_record_dot for '{s}' at offset {d}\n", .{ record_access.variable_name, record_access.variable_start });
                if (module_env_opt) |module_env| {
                    // When using snapshot, cursor positions don't correspond to snapshot CIR
                    // So we must look up by name instead of analyzing the dot expression
                    if (used_snapshot or self.findDotReceiverTypeVar(module_env, cursor_offset) == null) {
                        std.debug.print("completion: using name-based lookup (snapshot={}, or findDotReceiverTypeVar failed)\n", .{used_snapshot});
                        try self.addRecordFieldCompletions(&items, module_env, record_access.variable_name, record_access.variable_start);
                        std.debug.print("completion: after addRecordFieldCompletions, items={d}\n", .{items.items.len});
                        try self.addMethodCompletions(&items, module_env, record_access.variable_name, record_access.variable_start);
                        std.debug.print("completion: after addMethodCompletions, items={d}\n", .{items.items.len});
                    } else if (self.findDotReceiverTypeVar(module_env, cursor_offset)) |type_var| {
                        std.debug.print("completion: using CIR-based lookup with type_var={}", .{type_var});
                        try self.addFieldsFromTypeVar(&items, module_env, type_var);
                        try self.addMethodsFromTypeVar(&items, module_env, type_var);
                    }
                } else {
                    std.debug.print("completion: NO module_env for record/method completions", .{});
                }
            },
            .after_colon => {
                // Type annotation context - add type names
                if (module_env_opt) |module_env| {
                    try self.addTypeCompletions(&items, module_env);
                }
                try self.addTypeCompletionsFromEnv(&items, env);
            },
            .expression => {
                // General expression context - add local definitions + module names
                if (module_env_opt) |module_env| {
                    try self.addLocalCompletions(&items, module_env, cursor_offset);
                    try self.addModuleNameCompletions(&items, module_env);
                }
                try self.addModuleNameCompletionsFromEnv(&items, env);
            },
        }

        return .{
            .items = try items.toOwnedSlice(self.allocator),
            .is_incomplete = false,
        };
    }

    fn addModuleNameCompletionsFromEnv(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        env: *BuildEnv,
    ) !void {
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |module_state| {
                const name = module_state.name;
                if (name.len == 0) continue;

                var already_added = false;
                for (items.items) |item| {
                    if (std.mem.eql(u8, item.label, name)) {
                        already_added = true;
                        break;
                    }
                }
                if (already_added) continue;

                try items.append(self.allocator, .{
                    .label = name,
                    .kind = @intFromEnum(completion_handler.CompletionItemKind.module),
                    .detail = null,
                });

                if (module_state.env) |*module_env| {
                    try self.addModuleLikeNameCompletionsFromModuleEnv(items, module_env);
                }
            }
        }
    }

    fn addModuleLikeNameCompletionsFromModuleEnv(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *const ModuleEnv,
    ) !void {
        const exposed = &module_env.common.exposed_items;
        var iter = exposed.iterator();
        while (iter.next()) |exp_entry| {
            const ident_idx: base.Ident.Idx = @bitCast(exp_entry.ident_idx);
            const name = module_env.common.idents.getText(ident_idx);
            if (name.len == 0) continue;

            const without_module = stripModulePrefix(name, module_env.module_name);
            const label = firstSegment(without_module);
            if (label.len == 0 or !std.ascii.isUpper(label[0])) continue;

            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, label)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            try items.append(self.allocator, .{
                .label = label,
                .kind = @intFromEnum(completion_handler.CompletionItemKind.module),
                .detail = null,
            });
        }
    }

    fn addTypeNamesFromModuleEnv(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
    ) !void {
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_alias_decl => |alias| {
                    const header = module_env.store.getTypeHeader(alias.header);
                    const name = module_env.getIdentText(header.name);
                    if (name.len == 0) continue;

                    var already_added = false;
                    for (items.items) |item| {
                        if (std.mem.eql(u8, item.label, name)) {
                            already_added = true;
                            break;
                        }
                    }
                    if (already_added) continue;

                    try items.append(self.allocator, .{
                        .label = name,
                        .kind = @intFromEnum(completion_handler.CompletionItemKind.class),
                        .detail = null,
                    });
                },
                .s_nominal_decl => |nominal| {
                    const header = module_env.store.getTypeHeader(nominal.header);
                    const name = module_env.getIdentText(header.name);
                    if (name.len == 0) continue;

                    var already_added = false;
                    for (items.items) |item| {
                        if (std.mem.eql(u8, item.label, name)) {
                            already_added = true;
                            break;
                        }
                    }
                    if (already_added) continue;

                    try items.append(self.allocator, .{
                        .label = name,
                        .kind = @intFromEnum(completion_handler.CompletionItemKind.class),
                        .detail = null,
                    });
                },
                else => {},
            }
        }
    }

    fn addTypeCompletionsFromEnv(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        env: *BuildEnv,
    ) !void {
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                if (module_state.env) |*module_env| {
                    try self.addTypeNamesFromModuleEnv(items, module_env);
                }
            }
        }
    }

    fn stripModulePrefix(name: []const u8, module_name: []const u8) []const u8 {
        var i: usize = 0;
        while (i < name.len) {
            const seg_start = i;
            const dot_idx = std.mem.indexOfScalarPos(u8, name, seg_start, '.') orelse name.len;
            const seg = name[seg_start..dot_idx];

            if (std.mem.eql(u8, seg, module_name)) {
                if (dot_idx < name.len) return name[dot_idx + 1 ..];
                return "";
            }

            if (dot_idx == name.len) break;
            i = dot_idx + 1;
        }

        return name;
    }

    fn firstSegment(name: []const u8) []const u8 {
        const dot_idx = std.mem.indexOfScalar(u8, name, '.') orelse name.len;
        return name[0..dot_idx];
    }

    fn lastSegment(name: []const u8) []const u8 {
        const dot_idx = std.mem.lastIndexOfScalar(u8, name, '.') orelse return name;
        if (dot_idx + 1 >= name.len) return name;
        return name[dot_idx + 1 ..];
    }

    /// Add completions for members of a specific module (e.g., Str.concat)
    fn addModuleMemberCompletions(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        env: *BuildEnv,
        module_name: []const u8,
        module_env_opt: ?*ModuleEnv,
    ) !void {
        // Try to find the module in imported modules
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                // Check if this module's name matches
                if (std.mem.eql(u8, module_state.name, module_name)) {
                    if (module_state.env) |*imported_env| {
                        var type_writer = imported_env.initTypeWriter() catch null;
                        defer if (type_writer) |*tw| tw.deinit();

                        // Get exposed items from this module
                        const exposed = &imported_env.common.exposed_items;
                        var iter = exposed.iterator();
                        while (iter.next()) |exp_entry| {
                            const ident_idx: base.Ident.Idx = @bitCast(exp_entry.ident_idx);
                            const name = imported_env.common.idents.getText(ident_idx);
                            const without_module = stripModulePrefix(name, module_name);
                            const label = firstSegment(without_module);
                            if (label.len == 0) continue;

                            // Avoid duplicates when multiple items share a prefix (e.g. nested items)
                            var already_added = false;
                            for (items.items) |item| {
                                if (std.mem.eql(u8, item.label, label)) {
                                    already_added = true;
                                    break;
                                }
                            }
                            if (already_added) continue;

                            // Determine kind based on name convention
                            const kind: u32 = if (label.len > 0 and std.ascii.isUpper(label[0]))
                                @intFromEnum(completion_handler.CompletionItemKind.class)
                            else
                                @intFromEnum(completion_handler.CompletionItemKind.function);

                            var detail: ?[]const u8 = null;
                            if (type_writer) |*tw| {
                                if (imported_env.common.getNodeIndexById(self.allocator, ident_idx)) |node_idx| {
                                    if (node_idx != 0) {
                                        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
                                        const def = imported_env.store.getDef(def_idx);
                                        const type_var = ModuleEnv.varFrom(def.pattern);
                                        tw.write(type_var, .one_line) catch {};
                                        const type_str = tw.get();
                                        if (type_str.len > 0) {
                                            detail = self.allocator.dupe(u8, type_str) catch null;
                                        }
                                        tw.reset();
                                    }
                                }
                            }

                            try items.append(self.allocator, .{
                                .label = label,
                                .kind = kind,
                                .detail = detail,
                            });
                        }
                    }
                    return;
                }
            }
        }

        // Try to resolve module-like names (e.g., Str) from exposed items in any module
        var sched_it2 = env.schedulers.iterator();
        while (sched_it2.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                if (module_state.env) |*imported_env| {
                    try self.addModuleMemberCompletionsFromModuleEnv(items, imported_env, module_name);
                }
            }
        }

        // Fall back to local module env for associated items in the current module
        if (module_env_opt) |module_env| {
            try self.addLocalModuleMemberCompletions(items, module_env, module_name);
        }
    }

    fn addModuleMemberCompletionsFromModuleEnv(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        module_name: []const u8,
    ) !void {
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        const exposed = &module_env.common.exposed_items;
        var iter = exposed.iterator();
        while (iter.next()) |exp_entry| {
            const ident_idx: base.Ident.Idx = @bitCast(exp_entry.ident_idx);
            const name = module_env.common.idents.getText(ident_idx);
            if (name.len == 0) continue;

            const without_module = stripModulePrefix(name, module_env.module_name);
            if (!std.mem.startsWith(u8, without_module, module_name)) continue;
            if (without_module.len <= module_name.len or without_module[module_name.len] != '.') continue;

            const remainder = without_module[module_name.len + 1 ..];
            const label = firstSegment(remainder);
            if (label.len == 0) continue;

            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, label)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            const kind: u32 = if (label.len > 0 and std.ascii.isUpper(label[0]))
                @intFromEnum(completion_handler.CompletionItemKind.class)
            else
                @intFromEnum(completion_handler.CompletionItemKind.function);

            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                if (module_env.common.getNodeIndexById(self.allocator, ident_idx)) |node_idx| {
                    if (node_idx != 0) {
                        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
                        const def = module_env.store.getDef(def_idx);
                        const type_var = ModuleEnv.varFrom(def.pattern);
                        tw.write(type_var, .one_line) catch {};
                        const type_str = tw.get();
                        if (type_str.len > 0) {
                            detail = self.allocator.dupe(u8, type_str) catch null;
                        }
                        tw.reset();
                    }
                }
            }

            try items.append(self.allocator, .{
                .label = label,
                .kind = kind,
                .detail = detail,
            });
        }
    }

    fn addLocalModuleMemberCompletions(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        module_name: []const u8,
    ) !void {
        // Add members defined as qualified identifiers in this module (e.g., Basic.to_str)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            if (name.len == 0) continue;

            const without_module = stripModulePrefix(name, module_name);
            if (std.mem.eql(u8, without_module, name) or without_module.len == 0) continue;

            const label = firstSegment(without_module);
            if (label.len == 0) continue;

            // Avoid duplicates
            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, label)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            const kind: u32 = if (label.len > 0 and std.ascii.isUpper(label[0]))
                @intFromEnum(completion_handler.CompletionItemKind.class)
            else
                @intFromEnum(completion_handler.CompletionItemKind.function);

            try items.append(self.allocator, .{
                .label = label,
                .kind = kind,
                .detail = null,
            });
        }

        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                const pattern = module_env.store.getPattern(pattern_idx);
                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (name.len == 0) continue;

                const without_module = stripModulePrefix(name, module_name);
                if (std.mem.eql(u8, without_module, name) or without_module.len == 0) continue;

                const label = firstSegment(without_module);
                if (label.len == 0) continue;

                var already_added = false;
                for (items.items) |item| {
                    if (std.mem.eql(u8, item.label, label)) {
                        already_added = true;
                        break;
                    }
                }
                if (already_added) continue;

                const kind: u32 = if (label.len > 0 and std.ascii.isUpper(label[0]))
                    @intFromEnum(completion_handler.CompletionItemKind.class)
                else
                    @intFromEnum(completion_handler.CompletionItemKind.function);

                try items.append(self.allocator, .{
                    .label = label,
                    .kind = kind,
                    .detail = null,
                });
            }
        }
    }

    /// Add type completions for type annotation context
    fn addTypeCompletions(self: *SyntaxChecker, items: *std.ArrayList(completion_handler.CompletionItem), module_env: *ModuleEnv) !void {
        // Add builtin types
        try self.addTypeNamesFromModuleEnv(items, module_env);
    }

    /// Add record field completions for a record variable access (e.g., "myRecord.")
    /// This finds the variable's type and extracts field names from record types.
    fn addRecordFieldCompletions(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        variable_name: []const u8,
        variable_start: u32,
    ) !void {
        std.debug.print("addRecordFieldCompletions: looking for '{s}' at offset {d}", .{ variable_name, variable_start });

        // Find the binding for this variable name
        var scope = scope_map.ScopeMap.init(self.allocator);
        defer scope.deinit();
        scope.build(module_env) catch |err| {
            std.debug.print("addRecordFieldCompletions: scope.build failed: {}", .{err});
            return;
        };

        std.debug.print("addRecordFieldCompletions: scope has {d} bindings", .{scope.bindings.items.len});

        // Find the binding with matching name that's visible at the variable position
        var found_binding: ?scope_map.Binding = null;
        for (scope.bindings.items) |binding| {
            const binding_name = module_env.getIdentText(binding.ident);
            const is_visible = scope_map.ScopeMap.isVisibleAt(binding, variable_start);
            std.debug.print("addRecordFieldCompletions: binding '{s}' visible_from={d} visible_to={d} is_visible={}", .{ binding_name, binding.visible_from, binding.visible_to, is_visible });
            if (!is_visible) continue;
            if (std.mem.eql(u8, binding_name, variable_name)) {
                std.debug.print("addRecordFieldCompletions: FOUND binding '{s}'", .{binding_name});
                found_binding = binding;
                break;
            }
        }

        // Even if we found a binding in the local scope, we need to look up the definition
        // by name in top-level defs/statements, because the binding's pattern_idx may be
        // from incomplete code and not have full type info (especially when using snapshots).
        // So we ALWAYS try def/stmt lookup below instead of using the binding directly.
        if (found_binding) |_| {
            std.debug.print("addRecordFieldCompletions: found binding, but will use def/stmt lookup for better types", .{});
        } else {
            std.debug.print("addRecordFieldCompletions: NO binding found for '{s}'", .{variable_name});
        }

        // Check top-level definitions (not just when found_binding==null)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        std.debug.print("addRecordFieldCompletions: checking {d} top-level defs\n", .{defs_slice.len});

        // First list ALL defs to understand what we have
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };
            const name = module_env.getIdentText(ident_idx);
            std.debug.print("  ALL DEFS: '{s}' pattern={} def_idx={}\n", .{ name, def.pattern, def_idx });
        }

        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            std.debug.print("addRecordFieldCompletions: def '{s}' pattern={} has_annotation={}\n", .{ name, def.pattern, def.annotation != null });
            if (std.mem.eql(u8, name, variable_name)) {
                std.debug.print("addRecordFieldCompletions: FOUND def '{s}' pattern={} annotation={?}\n", .{ name, def.pattern, def.annotation });

                // If there's a type annotation, try to get the type from it first (more accurate than inferred type)
                if (def.annotation) |anno_idx| {
                    std.debug.print("addRecordFieldCompletions: def has annotation, trying to get type from it\n", .{});
                    const annotation = module_env.store.getAnnotation(anno_idx);
                    const type_anno = module_env.store.getTypeAnno(annotation.anno);
                    std.debug.print("addRecordFieldCompletions: type_anno={any}\n", .{type_anno});

                    // Try to get the type var from the annotation
                    // Type annotations should have been checked and have associated type vars
                    // For now, fall through to using pattern's type var
                }

                // Found the definition - get its type and extract record fields
                const type_var = ModuleEnv.varFrom(def.pattern);
                std.debug.print("addRecordFieldCompletions: type_var for def={}\n", .{type_var});
                try self.addFieldsFromTypeVar(items, module_env, type_var);
                return;
            }
        }

        // Check statements (apps use statements for definitions)
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        std.debug.print("addRecordFieldCompletions: checking {d} statements", .{statements_slice.len});
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const pattern_idx = switch (stmt) {
                .s_decl => |decl| decl.pattern,
                .s_decl_gen => |decl| decl.pattern,
                else => continue,
            };

            const pattern = module_env.store.getPattern(pattern_idx);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            std.debug.print("addRecordFieldCompletions: stmt '{s}'", .{name});
            if (std.mem.eql(u8, name, variable_name)) {
                std.debug.print("addRecordFieldCompletions: FOUND stmt '{s}'", .{name});
                // Found the definition - get its type and extract record fields
                const type_var = ModuleEnv.varFrom(pattern_idx);
                try self.addFieldsFromTypeVar(items, module_env, type_var);
                return;
            }
        }
    }

    /// Extract and add record fields from a type variable
    fn addFieldsFromTypeVar(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        type_var: types.Var,
    ) !void {
        const type_store = &module_env.types;

        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        std.debug.print("addFieldsFromTypeVar: type_var={}, content tag={s}", .{ type_var, @tagName(content) });

        var steps: usize = 0;
        while (true) : (steps += 1) {
            if (steps > 8) break;

            if (content.unwrapRecord()) |record| {
                std.debug.print("addFieldsFromTypeVar: found record after resolve", .{});
                try self.addFieldsFromRecord(items, module_env, record);
                return;
            }

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

        // Try to get record fields, handling aliases that wrap records
        try self.addFieldsFromContent(items, module_env, content, 0);
    }

    /// Recursively extract fields from type content, unwrapping aliases
    fn addFieldsFromContent(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        content: types.Content,
        depth: usize,
    ) !void {
        const type_store = &module_env.types;

        std.debug.print("addFieldsFromContent: content tag={s}", .{@tagName(content)});

        if (depth > 16) return;

        // Check if this is directly a record
        if (content.unwrapRecord()) |record| {
            std.debug.print("addFieldsFromContent: found record!", .{});
            try self.addFieldsFromRecord(items, module_env, record);
            return;
        }

        // Check if this is an alias (e.g., a type alias wrapping a record)
        switch (content) {
            .alias => |alias| {
                std.debug.print("addFieldsFromContent: unwrapping alias", .{});
                // Get the backing type of the alias
                const backing_var = type_store.getAliasBackingVar(alias);
                const backing_resolved = type_store.resolveVar(backing_var);
                try self.addFieldsFromContent(items, module_env, backing_resolved.desc.content, depth + 1);
            },
            .structure => |flat_type| {
                std.debug.print("addFieldsFromContent: structure, flat_type tag={s}", .{@tagName(flat_type)});
            },
            else => {
                std.debug.print("addFieldsFromContent: not a record or alias, tag={s}", .{@tagName(content)});
            },
        }
    }

    /// Add completion items for fields in a record type
    fn addFieldsFromRecord(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        record: types.Record,
    ) !void {
        const type_store = &module_env.types;

        // Get the record fields
        const fields_slice = type_store.getRecordFieldsSlice(record.fields);
        const field_names = fields_slice.items(.name);
        const field_vars = fields_slice.items(.var_);

        std.debug.print("addFieldsFromRecord: record.fields={}, fields_slice.len={}, field_names.len={d}\n", .{ record.fields, fields_slice.len, field_names.len });

        // Initialize type writer for formatting field types
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        // Iterate over record fields
        for (field_names, field_vars) |field_name_idx, field_var| {
            const field_name = module_env.getIdentText(field_name_idx);
            std.debug.print("addFieldsFromRecord: field '{s}'\n", .{field_name});
            if (field_name.len == 0) continue;

            // Get field type for detail
            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                tw.write(field_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            try items.append(self.allocator, .{
                .label = field_name,
                .kind = @intFromEnum(completion_handler.CompletionItemKind.field),
                .detail = detail,
            });
        }
    }

    /// Add local definition completions
    fn addLocalCompletions(self: *SyntaxChecker, items: *std.ArrayList(completion_handler.CompletionItem), module_env: *ModuleEnv, cursor_offset: u32) !void {
        // Initialize type writer for formatting types
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        // Build scope map for local variable completions
        var scope = scope_map.ScopeMap.init(self.allocator);
        defer scope.deinit();
        scope.build(module_env) catch {};

        // Add local variables in scope at cursor position
        for (scope.bindings.items) |binding| {
            if (!scope_map.ScopeMap.isVisibleAt(binding, cursor_offset)) continue;

            const name = module_env.getIdentText(binding.ident);
            if (name.len == 0) continue;

            const label = lastSegment(name);
            if (label.len == 0) continue;

            // Check if already in items (avoid duplicates)
            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, label)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            // Determine kind - parameters and local variables
            const kind: u32 = if (binding.is_parameter)
                @intFromEnum(completion_handler.CompletionItemKind.variable)
            else
                @intFromEnum(completion_handler.CompletionItemKind.variable);

            // Get type information for the binding
            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                const type_var = ModuleEnv.varFrom(binding.pattern_idx);
                tw.write(type_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            try items.append(self.allocator, .{
                .label = label,
                .kind = kind,
                .detail = detail,
            });
        }

        // Add definitions from all_defs (top-level)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            if (name.len == 0) continue;
            if (std.mem.indexOfScalar(u8, name, '.') != null) continue;

            // Check if already in items (avoid duplicates)
            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, name)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            // Determine completion kind based on the expression type
            const expr = module_env.store.getExpr(def.expr);
            const kind: u32 = switch (expr) {
                .e_closure, .e_lambda, .e_hosted_lambda => @intFromEnum(completion_handler.CompletionItemKind.function),
                else => @intFromEnum(completion_handler.CompletionItemKind.variable),
            };

            // Get type information for the definition
            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                const type_var = ModuleEnv.varFrom(def.pattern);
                tw.write(type_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            try items.append(self.allocator, .{
                .label = name,
                .kind = kind,
                .detail = detail,
            });
        }

        // Also check statements (apps use statements for definitions)
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                const pattern = module_env.store.getPattern(pattern_idx);

                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (name.len == 0) continue;
                if (std.mem.indexOfScalar(u8, name, '.') != null) continue;

                // Check if already in items
                var already_added = false;
                for (items.items) |item| {
                    if (std.mem.eql(u8, item.label, name)) {
                        already_added = true;
                        break;
                    }
                }
                if (already_added) continue;

                // Determine completion kind
                var kind: u32 = @intFromEnum(completion_handler.CompletionItemKind.variable);
                if (stmt_parts.expr) |expr_idx| {
                    const expr = module_env.store.getExpr(expr_idx);
                    kind = switch (expr) {
                        .e_closure, .e_lambda, .e_hosted_lambda => @intFromEnum(completion_handler.CompletionItemKind.function),
                        else => @intFromEnum(completion_handler.CompletionItemKind.variable),
                    };
                }

                // Get type information
                var detail: ?[]const u8 = null;
                if (type_writer) |*tw| {
                    const type_var = ModuleEnv.varFrom(pattern_idx);
                    tw.write(type_var, .one_line) catch {};
                    const type_str = tw.get();
                    if (type_str.len > 0) {
                        detail = self.allocator.dupe(u8, type_str) catch null;
                    }
                    tw.reset();
                }

                try items.append(self.allocator, .{
                    .label = name,
                    .kind = kind,
                    .detail = detail,
                });
            }
        }
    }

    /// Add module name completions
    fn addModuleNameCompletions(self: *SyntaxChecker, items: *std.ArrayList(completion_handler.CompletionItem), module_env: *ModuleEnv) !void {
        // Add builtin module names
        // Add imported module names from import statements
        const import_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (import_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            if (stmt == .s_import) {
                const import_stmt = stmt.s_import;
                // Use alias if available, otherwise use the module name
                const name_idx = import_stmt.alias_tok orelse import_stmt.module_name_tok;
                const name = module_env.common.idents.getText(name_idx);

                if (name.len > 0) {
                    // Check if already added (could be a builtin)
                    var already_added = false;
                    for (items.items) |item| {
                        if (std.mem.eql(u8, item.label, name)) {
                            already_added = true;
                            break;
                        }
                    }
                    if (!already_added) {
                        try items.append(self.allocator, .{
                            .label = name,
                            .kind = @intFromEnum(completion_handler.CompletionItemKind.module),
                            .detail = null,
                        });
                    }
                }
            }
        }

        // Add local module-like names from associated items (e.g., Basic from Basic.to_str)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            if (name.len == 0) continue;

            const without_module = stripModulePrefix(name, module_env.module_name);
            const label = firstSegment(without_module);
            if (label.len == 0 or !std.ascii.isUpper(label[0])) continue;

            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, label)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            try items.append(self.allocator, .{
                .label = label,
                .kind = @intFromEnum(completion_handler.CompletionItemKind.module),
                .detail = null,
            });
        }

        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                const pattern = module_env.store.getPattern(pattern_idx);
                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (name.len == 0) continue;

                const without_module = stripModulePrefix(name, module_env.module_name);
                const label = firstSegment(without_module);
                if (label.len == 0 or !std.ascii.isUpper(label[0])) continue;

                var already_added = false;
                for (items.items) |item| {
                    if (std.mem.eql(u8, item.label, label)) {
                        already_added = true;
                        break;
                    }
                }
                if (already_added) continue;

                try items.append(self.allocator, .{
                    .label = label,
                    .kind = @intFromEnum(completion_handler.CompletionItemKind.module),
                    .detail = null,
                });
            }
        }
    }

    fn findDotReceiverTypeVar(self: *SyntaxChecker, module_env: *ModuleEnv, cursor_offset: u32) ?types.Var {
        var best_size: u32 = std.math.maxInt(u32);
        var best_var: ?types.Var = null;

        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (self.findDotReceiverTypeVarInExpr(module_env, def.expr, cursor_offset, &best_size)) |var_| {
                best_var = var_;
            }
        }

        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);
            if (stmt_parts.expr) |expr_idx| {
                if (self.findDotReceiverTypeVarInExpr(module_env, expr_idx, cursor_offset, &best_size)) |var_| {
                    best_var = var_;
                }
            }
            if (stmt_parts.expr2) |expr_idx| {
                if (self.findDotReceiverTypeVarInExpr(module_env, expr_idx, cursor_offset, &best_size)) |var_| {
                    best_var = var_;
                }
            }
        }

        return best_var;
    }

    fn findDotReceiverTypeVarInExpr(
        self: *SyntaxChecker,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        cursor_offset: u32,
        best_size: *u32,
    ) ?types.Var {
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        const region = module_env.store.getRegionAt(node_idx);

        if (!regionContainsOffset(region, cursor_offset)) {
            return null;
        }

        const expr = module_env.store.getExpr(expr_idx);
        var result: ?types.Var = null;

        if (expr == .e_dot_access) {
            const dot = expr.e_dot_access;
            if (regionContainsOffset(dot.field_name_region, cursor_offset)) {
                const size = dot.field_name_region.end.offset - dot.field_name_region.start.offset;
                if (size < best_size.*) {
                    best_size.* = size;
                    result = ModuleEnv.varFrom(dot.receiver);
                }
            }
        }

        switch (expr) {
            .e_lambda => |lambda| {
                if (self.findDotReceiverTypeVarInExpr(module_env, lambda.body, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_closure => |closure| {
                if (self.findDotReceiverTypeVarInExpr(module_env, closure.lambda_idx, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_block => |block| {
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    const stmt = module_env.store.getStatement(stmt_idx);
                    const stmt_parts = getStatementParts(stmt);
                    if (stmt_parts.expr) |stmt_expr| {
                        if (self.findDotReceiverTypeVarInExpr(module_env, stmt_expr, cursor_offset, best_size)) |var_| {
                            result = var_;
                        }
                    }
                    if (stmt_parts.expr2) |stmt_expr| {
                        if (self.findDotReceiverTypeVarInExpr(module_env, stmt_expr, cursor_offset, best_size)) |var_| {
                            result = var_;
                        }
                    }
                }
                if (self.findDotReceiverTypeVarInExpr(module_env, block.final_expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_if => |if_expr| {
                const branches = module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    if (self.findDotReceiverTypeVarInExpr(module_env, branch.cond, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                    if (self.findDotReceiverTypeVarInExpr(module_env, branch.body, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
                if (self.findDotReceiverTypeVarInExpr(module_env, if_expr.final_else, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_match => |match_expr| {
                if (self.findDotReceiverTypeVarInExpr(module_env, match_expr.cond, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
                const branches = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    if (self.findDotReceiverTypeVarInExpr(module_env, branch.value, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                    if (branch.guard) |guard| {
                        if (self.findDotReceiverTypeVarInExpr(module_env, guard, cursor_offset, best_size)) |var_| {
                            result = var_;
                        }
                    }
                }
            },
            .e_call => |call| {
                if (self.findDotReceiverTypeVarInExpr(module_env, call.func, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, arg, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            .e_binop => |binop| {
                if (self.findDotReceiverTypeVarInExpr(module_env, binop.lhs, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
                if (self.findDotReceiverTypeVarInExpr(module_env, binop.rhs, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_unary_minus => |unary| {
                if (self.findDotReceiverTypeVarInExpr(module_env, unary.expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_unary_not => |unary| {
                if (self.findDotReceiverTypeVarInExpr(module_env, unary.expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_list => |list| {
                const elems = module_env.store.sliceExpr(list.elems);
                for (elems) |elem| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, elem, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            .e_tuple => |tuple| {
                const elems = module_env.store.sliceExpr(tuple.elems);
                for (elems) |elem| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, elem, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            .e_record => |record| {
                const fields = module_env.store.sliceRecordFields(record.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (self.findDotReceiverTypeVarInExpr(module_env, field.value, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
                if (record.ext) |ext| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, ext, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            .e_dot_access => |dot| {
                if (self.findDotReceiverTypeVarInExpr(module_env, dot.receiver, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
                if (dot.args) |args_span| {
                    const args = module_env.store.sliceExpr(args_span);
                    for (args) |arg| {
                        if (self.findDotReceiverTypeVarInExpr(module_env, arg, cursor_offset, best_size)) |var_| {
                            result = var_;
                        }
                    }
                }
            },
            .e_str => |str| {
                const segments = module_env.store.sliceExpr(str.span);
                for (segments) |segment| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, segment, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            .e_tag => |tag| {
                const args = module_env.store.sliceExpr(tag.args);
                for (args) |arg| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, arg, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            .e_nominal => |nominal| {
                if (self.findDotReceiverTypeVarInExpr(module_env, nominal.backing_expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_nominal_external => |nominal| {
                if (self.findDotReceiverTypeVarInExpr(module_env, nominal.backing_expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_dbg => |dbg| {
                if (self.findDotReceiverTypeVarInExpr(module_env, dbg.expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_expect => |expect| {
                if (self.findDotReceiverTypeVarInExpr(module_env, expect.body, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_return => |ret| {
                if (self.findDotReceiverTypeVarInExpr(module_env, ret.expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_for => |for_expr| {
                if (self.findDotReceiverTypeVarInExpr(module_env, for_expr.expr, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
                if (self.findDotReceiverTypeVarInExpr(module_env, for_expr.body, cursor_offset, best_size)) |var_| {
                    result = var_;
                }
            },
            .e_type_var_dispatch => |dispatch| {
                const args = module_env.store.sliceExpr(dispatch.args);
                for (args) |arg| {
                    if (self.findDotReceiverTypeVarInExpr(module_env, arg, cursor_offset, best_size)) |var_| {
                        result = var_;
                    }
                }
            },
            else => {},
        }

        return result;
    }

    /// Add method completions for static dispatch (e.g., "value.method()")
    /// This finds methods available on the type of the variable before the dot.
    fn addMethodCompletions(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        variable_name: []const u8,
        variable_start: u32,
    ) !void {
        std.debug.print("addMethodCompletions: looking for '{s}' at offset {d}\n", .{ variable_name, variable_start });
        // Find the binding for this variable name (same as record field completion)
        var scope = scope_map.ScopeMap.init(self.allocator);
        defer scope.deinit();
        scope.build(module_env) catch |err| {
            std.debug.print("addMethodCompletions: scope.build failed: {s}\n", .{@errorName(err)});
            return;
        };
        std.debug.print("addMethodCompletions: scope has {d} bindings\n", .{scope.bindings.items.len});

        // Find the binding with matching name that's visible at the variable position
        var found_binding: ?scope_map.Binding = null;
        for (scope.bindings.items) |binding| {
            const name = module_env.getIdentText(binding.ident);
            const is_visible = scope_map.ScopeMap.isVisibleAt(binding, variable_start);
            if (!is_visible) continue;
            if (std.mem.eql(u8, name, variable_name)) {
                std.debug.print("addMethodCompletions: FOUND binding '{s}'\n", .{name});
                found_binding = binding;
                break;
            }
        }

        // Also check top-level definitions
        if (found_binding == null) {
            const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
            std.debug.print("addMethodCompletions: checking {d} top-level defs\n", .{defs_slice.len});
            for (defs_slice) |def_idx| {
                const def = module_env.store.getDef(def_idx);
                const pattern = module_env.store.getPattern(def.pattern);

                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (std.mem.eql(u8, name, variable_name)) {
                    std.debug.print("addMethodCompletions: FOUND def '{s}'\n", .{name});
                    // Found the definition - get its type and find methods
                    const type_var = ModuleEnv.varFrom(def.pattern);
                    try self.addMethodsFromTypeVar(items, module_env, type_var);
                    return;
                }
            }
        }

        // Also check statements (apps use statements for definitions)
        if (found_binding == null) {
            const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
            std.debug.print("addMethodCompletions: checking {d} statements\n", .{statements_slice.len});
            for (statements_slice) |stmt_idx| {
                const stmt = module_env.store.getStatement(stmt_idx);
                const pattern_idx = switch (stmt) {
                    .s_decl => |decl| decl.pattern,
                    .s_decl_gen => |decl| decl.pattern,
                    else => continue,
                };

                const pattern = module_env.store.getPattern(pattern_idx);
                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (std.mem.eql(u8, name, variable_name)) {
                    std.debug.print("addMethodCompletions: FOUND stmt '{s}'\n", .{name});
                    // Found the definition - get its type and find methods
                    const type_var = ModuleEnv.varFrom(pattern_idx);
                    try self.addMethodsFromTypeVar(items, module_env, type_var);
                    return;
                }
            }
        }

        if (found_binding) |binding| {
            std.debug.print("addMethodCompletions: using binding pattern_idx={}\n", .{binding.pattern_idx});
            if (self.findExprTypeForPattern(module_env, binding.pattern_idx)) |type_var| {
                try self.addMethodsFromTypeVar(items, module_env, type_var);
                return;
            }
            // Get the type of this binding and find methods
            const type_var = ModuleEnv.varFrom(binding.pattern_idx);
            try self.addMethodsFromTypeVar(items, module_env, type_var);
        }
    }

    /// Extract methods available for a type variable and add them as completions.
    fn addMethodsFromTypeVar(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        type_var: types.Var,
    ) !void {
        const type_store = &module_env.types;

        std.debug.print("addMethodsFromTypeVar: type_var={}\n", .{type_var});
        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        var steps: usize = 0;
        while (true) : (steps += 1) {
            if (steps > 8) {
                std.debug.print("addMethodsFromTypeVar: hit step limit\n", .{});
                break;
            }

            const type_ident_opt: ?base.Ident.Idx = switch (content) {
                .alias => |alias| alias.ident.ident_idx,
                .structure => |flat_type| switch (flat_type) {
                    .nominal_type => |nominal| nominal.ident.ident_idx,
                    else => null,
                },
                else => null,
            };

            if (type_ident_opt) |type_ident| {
                std.debug.print("addMethodsFromTypeVar: type_ident={}\n", .{type_ident});
                try self.addMethodsForTypeIdent(items, module_env, type_ident);
            }

            switch (content) {
                .flex => |flex| {
                    std.debug.print("addMethodsFromTypeVar: flex constraints\n", .{});
                    // Extract method names from flex constraints
                    try self.addMethodsFromConstraints(items, module_env, flex.constraints);
                    break;
                },
                .rigid => |rigid| {
                    std.debug.print("addMethodsFromTypeVar: rigid constraints\n", .{});
                    // Extract method names from rigid constraints
                    try self.addMethodsFromConstraints(items, module_env, rigid.constraints);
                    break;
                },
                .alias => |alias| {
                    std.debug.print("addMethodsFromTypeVar: alias unwrap\n", .{});
                    const backing_var = type_store.getAliasBackingVar(alias);
                    resolved = type_store.resolveVar(backing_var);
                    content = resolved.desc.content;
                    continue;
                },
                else => break,
            }
        }
    }

    /// Extract method names from static dispatch constraints and add them as completions
    fn addMethodsFromConstraints(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) !void {
        if (constraints.isEmpty()) {
            std.debug.print("addMethodsFromConstraints: empty\n", .{});
            return;
        }

        const constraints_slice = module_env.types.sliceStaticDispatchConstraints(constraints);
        std.debug.print("addMethodsFromConstraints: count={d}\n", .{constraints_slice.len});
        for (constraints_slice) |constraint| {
            const method_name = module_env.getIdentText(constraint.fn_name);

            if (method_name.len == 0) continue;

            // Skip if already added (avoid duplicates)
            var already_added = false;
            for (items.items) |item| {
                if (std.mem.eql(u8, item.label, method_name)) {
                    already_added = true;
                    break;
                }
            }
            if (already_added) continue;

            // Add the method completion
            const label = try self.allocator.dupe(u8, method_name);
            errdefer self.allocator.free(label);

            // Try to get detail from the constraint's function type
            var detail: ?[]const u8 = null;
            var type_writer = module_env.initTypeWriter() catch null;
            defer if (type_writer) |*tw| tw.deinit();

            if (type_writer) |*tw| {
                tw.write(constraint.fn_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            try items.append(self.allocator, .{
                .label = label,
                .kind = @intFromEnum(completion_handler.CompletionItemKind.method),
                .detail = detail,
            });
        }
    }

    fn findExprTypeForPattern(_: *SyntaxChecker, module_env: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?types.Var {
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (def.pattern == pattern_idx) {
                std.debug.print("findExprTypeForPattern: def expr for pattern_idx={} expr_idx={}\n", .{ pattern_idx, def.expr });
                return ModuleEnv.varFrom(def.expr);
            }
        }

        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);
            if (stmt_parts.pattern) |stmt_pattern_idx| {
                if (stmt_pattern_idx == pattern_idx) {
                    if (stmt_parts.expr) |expr_idx| {
                        std.debug.print("findExprTypeForPattern: stmt expr for pattern_idx={} expr_idx={}\n", .{ pattern_idx, expr_idx });
                        return ModuleEnv.varFrom(expr_idx);
                    }
                }
            }
        }

        std.debug.print("findExprTypeForPattern: no expr for pattern_idx={}\n", .{pattern_idx});
        return null;
    }

    /// Add methods for a specific type identifier by searching method_idents.
    fn addMethodsForTypeIdent(
        self: *SyntaxChecker,
        items: *std.ArrayList(completion_handler.CompletionItem),
        module_env: *ModuleEnv,
        type_ident: base.Ident.Idx,
    ) !void {
        // Initialize type writer for formatting method signatures
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        // Get the type name for display purposes
        const type_name = module_env.getIdentText(type_ident);

        // Iterate through method_idents to find all methods for this type.
        // The method_idents maps (type_ident, method_ident) -> qualified_ident.
        // We need to find all entries where the type_ident matches.
        const entries = module_env.method_idents.entries.items;
        for (entries) |entry| {
            // Check if this method is for our type
            if (entry.key.type_ident == type_ident) {
                const method_ident = entry.key.method_ident;
                const method_name = module_env.getIdentText(method_ident);

                if (method_name.len == 0) continue;

                // Skip if already added (avoid duplicates)
                var already_added = false;
                for (items.items) |item| {
                    if (std.mem.eql(u8, item.label, method_name)) {
                        already_added = true;
                        break;
                    }
                }
                if (already_added) continue;

                // Try to get the method's type signature
                var detail: ?[]const u8 = null;
                const qualified_ident = entry.value;
                const qualified_name = module_env.getIdentText(qualified_ident);

                // Look up the method definition to get its type
                if (self.findMethodType(module_env, qualified_ident)) |method_type_var| {
                    if (type_writer) |*tw| {
                        tw.write(method_type_var, .one_line) catch {};
                        const type_str = tw.get();
                        if (type_str.len > 0) {
                            detail = self.allocator.dupe(u8, type_str) catch null;
                        }
                        tw.reset();
                    }
                }

                // If we couldn't get the type signature, at least show which type it's from
                if (detail == null and type_name.len > 0 and qualified_name.len > 0) {
                    detail = std.fmt.allocPrint(self.allocator, "method on {s}", .{type_name}) catch null;
                }

                try items.append(self.allocator, .{
                    .label = method_name,
                    .kind = @intFromEnum(completion_handler.CompletionItemKind.method),
                    .detail = detail,
                });
            }
        }
    }

    /// Find the type of a method definition by its qualified identifier.
    fn findMethodType(_: *SyntaxChecker, module_env: *ModuleEnv, qualified_ident: base.Ident.Idx) ?types.Var {
        // Look through definitions to find the method
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return ModuleEnv.varFrom(def.pattern);
            }
        }

        // Also check statements
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const pattern_idx = switch (stmt) {
                .s_decl => |decl| decl.pattern,
                .s_decl_gen => |decl| decl.pattern,
                else => continue,
            };

            const pattern = module_env.store.getPattern(pattern_idx);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return ModuleEnv.varFrom(pattern_idx);
            }
        }

        return null;
    }
};

const completion_handler = @import("handlers/completion.zig");
const document_symbol_handler = @import("handlers/document_symbol.zig");

fn buildLineOffsets(source: []const u8) LineOffsets {
    var result = LineOffsets{ .offsets = undefined, .count = 0 };
    result.offsets[0] = 0;
    result.count = 1;

    for (source, 0..) |c, i| {
        if (c == '\n' and result.count < 1024) {
            result.offsets[result.count] = @intCast(i + 1);
            result.count += 1;
        }
    }
    return result;
}

const LineOffsets = struct {
    offsets: [1024]u32,
    count: usize,
};

fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) document_symbol_handler.Position {
    var line: u32 = 0;
    for (0..line_offsets.count) |i| {
        if (line_offsets.offsets[i] > offset) break;
        line = @intCast(i);
    }
    const line_start = line_offsets.offsets[line];
    return .{
        .line = line,
        .character = offset - line_start,
    };
}

fn extractSymbolFromDecl(
    module_env: *ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    source: []const u8,
    uri: []const u8,
    line_offsets: *const LineOffsets,
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
    const start_pos = offsetToPosition(start_offset, line_offsets);
    const end_pos = offsetToPosition(end_offset, line_offsets);

    return .{
        .name = name,
        .kind = if (is_function) .function else .variable,
        .location = .{
            .uri = uri,
            .range = .{ .start = start_pos, .end = end_pos },
        },
    };
}

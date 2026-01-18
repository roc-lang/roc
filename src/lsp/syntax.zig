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
    cache_config: CacheConfig = .{},
    log_file: ?std.fs.File = null,
    debug: DebugFlags,

    pub fn init(allocator: std.mem.Allocator, debug: DebugFlags, log_file: ?std.fs.File) SyntaxChecker {
        return .{
            .allocator = allocator,
            .debug = debug,
            .log_file = log_file,
        };
    }

    pub fn deinit(self: *SyntaxChecker) void {
        if (self.build_env) |env| {
            env.deinit();
            self.allocator.destroy(env);
            self.build_env = null;
        }
    }

    /// Check the file referenced by the URI and return diagnostics grouped by URI.
    pub fn check(self: *SyntaxChecker, uri: []const u8, override_text: ?[]const u8, workspace_root: ?[]const u8) ![]Diagnostics.PublishDiagnostics {
        const path = try uri_util.uriToPath(self.allocator, uri);
        defer self.allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
        defer self.allocator.free(absolute_path);

        _ = workspace_root; // reserved for future workspace-root-aware overrides

        self.mutex.lock();
        defer self.mutex.unlock();

        var env = try self.resetBuildEnv();

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
        env.build(absolute_path) catch |err| {
            self.logDebug(.build, "build failed for {s}: {s}", .{ absolute_path, @errorName(err) });
        };

        const drained = env.drainReports() catch |err| {
            self.logDebug(.build, "drain reports failed: {s}", .{@errorName(err)});
            return err;
        };
        defer self.freeDrained(drained);

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

    fn resetBuildEnv(self: *SyntaxChecker) !*BuildEnv {
        var env_ptr = self.build_env orelse try self.allocator.create(BuildEnv);
        if (self.build_env != null) {
            env_ptr.deinit();
        }
        errdefer {
            self.build_env = null;
            self.allocator.destroy(env_ptr);
        }

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

    fn freeDrained(self: *SyntaxChecker, drained: []BuildEnv.DrainedModuleReports) void {
        for (drained) |entry| {
            self.allocator.free(entry.abs_path);
        }
        self.allocator.free(drained);
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

        var env = try self.resetBuildEnv();

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

        // Drain reports but ignore them for hover
        const drained = env.drainReports() catch return null;
        defer self.freeDrained(drained);

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

        var env = try self.resetBuildEnv();

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

        // Drain reports but ignore them for definition
        const drained = env.drainReports() catch return null;
        defer self.freeDrained(drained);

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
            const expr_idx = def.expr;
            const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
            const expr_region = module_env.store.getRegionAt(expr_node_idx);

            if (regionContainsOffset(expr_region, target_offset)) {
                // Search within this expression for the most specific lookup
                if (self.findLookupAtOffset(module_env, expr_idx, target_offset, &best_size)) |found| {
                    best_expr = found;
                }
            }
        }

        // Also check statements
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
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
                    // Get the module name from the imports store
                    const import_idx = @intFromEnum(lookup.module_idx);
                    if (import_idx >= module_env.imports.imports.items.items.len) return null;
                    const string_idx = module_env.imports.imports.items.items[import_idx];
                    const import_name = module_env.common.getString(string_idx);

                    // Try to find the module path from BuildEnv schedulers
                    const env = self.build_env orelse return null;

                    // Extract the base module name (e.g., "Stdout" from "pf.Stdout")
                    const base_name = if (std.mem.lastIndexOf(u8, import_name, ".")) |dot_pos|
                        import_name[dot_pos + 1 ..]
                    else
                        import_name;

                    // Search all schedulers for a module matching this name
                    var sched_it = env.schedulers.iterator();
                    while (sched_it.next()) |entry| {
                        const sched = entry.value_ptr.*;
                        // Look for module by name
                        if (sched.getModuleState(base_name)) |mod_state| {
                            // Found the module - convert its path to a URI
                            const module_uri = uri_util.pathToUri(self.allocator, mod_state.path) catch return null;
                            // Return definition at start of file (line 0, char 0)
                            // TODO: Use target_node_idx to navigate to specific definition
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
                },
                else => return null,
            }
        }

        return null;
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

        // Check if this expression itself is a lookup
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
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
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
                    for (statements_slice) |other_stmt_idx| {
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
        return offset >= region.start.offset and offset < region.end.offset;
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

        var env = try self.resetBuildEnv();

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

        // Drain reports but ignore them
        const drained = env.drainReports() catch return null;
        defer self.freeDrained(drained);

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
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
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
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
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
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
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
};

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

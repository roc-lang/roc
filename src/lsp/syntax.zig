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
const TypeWriter = types.TypeWriter;

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
};

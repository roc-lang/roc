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
        const target_offset = self.positionToOffset(module_env, line, character) orelse return null;

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
        const range = self.regionToRange(module_env, result.region);

        return HoverResult{
            .type_str = markdown,
            .range = range,
        };
    }

    /// Convert LSP position (line, character) to byte offset in source
    fn positionToOffset(self: *SyntaxChecker, module_env: *ModuleEnv, line: u32, character: u32) ?u32 {
        _ = self;
        const line_starts = module_env.getLineStartsAll();
        if (line >= line_starts.len) return null;

        const line_start = line_starts[line];
        // For simplicity, treat character as byte offset within line
        // (proper UTF-16 handling would require more work)
        return line_start + character;
    }

    /// Convert a Region to LSP range (line/character positions)
    fn regionToRange(self: *SyntaxChecker, module_env: *ModuleEnv, region: Region) ?LspRange {
        _ = self;
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

        return best_result;
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

        // Get child expressions based on the expression type
        const child_exprs = getChildExprs(expr);

        for (child_exprs) |child_idx| {
            const child_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(child_idx));
            const child_region = module_env.store.getRegionAt(child_node_idx);

            if (regionContainsOffset(child_region, target_offset)) {
                const size = child_region.end.offset - child_region.start.offset;
                if (size < best_size.*) {
                    best_size.* = size;
                    result = .{
                        .type_var = ModuleEnv.varFrom(child_idx),
                        .region = child_region,
                    };
                }

                // Recurse into this child
                if (self.findNestedTypeAtOffset(module_env, child_idx, target_offset, best_size)) |nested| {
                    result = nested;
                }
            }
        }

        return result;
    }

    /// Check if a region contains the given byte offset
    fn regionContainsOffset(region: Region, offset: u32) bool {
        return offset >= region.start.offset and offset < region.end.offset;
    }

    /// Get child expression indices from an expression (for traversal)
    /// This is a simplified version that returns empty for now - a full implementation
    /// would traverse into nested expressions like function bodies, if branches, etc.
    fn getChildExprs(expr: CIR.Expr) []const CIR.Expr.Idx {
        _ = expr;
        // For the initial implementation, we only look at top-level expressions
        // A full implementation would extract child expressions from:
        // - e_call args
        // - e_if branches
        // - e_when branches
        // - e_lambda body
        // - etc.
        return &.{};
    }
};

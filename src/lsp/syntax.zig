//! Syntax checking integration that runs the Roc compiler and converts
//! reports to LSP diagnostics.

const std = @import("std");
const compile = @import("compile");
const reporting = @import("reporting");
const build_options = @import("build_options");
const Filesystem = @import("fs").Filesystem;
const Allocator = std.mem.Allocator;

const Diagnostics = @import("diagnostics.zig");
const uri_util = @import("uri.zig");

const BuildEnv = compile.BuildEnv;
const CacheManager = compile.CacheManager;
const CacheConfig = compile.CacheConfig;
const FileProvider = compile.package.FileProvider;

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
};

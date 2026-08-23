//! CLI Context
//!
//! Provides shared context for CLI operations including allocators and error
//! accumulation. This enables:
//! - Consistent resource management across all CLI commands
//! - Structured error reporting with the Report system
//! - Testable CLI code by capturing output with custom writers
//!
//! The key design principle is that `error.CliError` is the ONLY error type
//! that CLI functions should return. This ensures:
//! - Every error is properly reported (no silent failures)
//! - Consistent error formatting across all commands
//! - The type system enforces proper error handling
//!
//! Usage:
//!   fn doSomething(ctx: *CliCtx, path: []const u8) CliError!void {
//!       const source = std.Io.Dir.cwd().readFileAlloc(ctx.gpa, path, ...) catch |err| {
//!           return ctx.fail(.{ .file_not_found = .{ .path = path } });
//!       };
//!       defer ctx.gpa.free(source);
//!       // Use ctx.arena for temporary allocations...
//!   }
//!
//!   // At top level:
//!   var io = Io.create(std_io);
//!   var ctx = CliCtx.init(gpa, arena, &io, .build);
//!   ctx.initIo();  // Initialize I/O writers after ctx is at its final location
//!   defer ctx.deinit();
//!
//!   doSomething(&ctx, "app.roc") catch |err| switch (err) {
//!       error.CliError => {}, // Problems already recorded
//!   };
//!
//!   try ctx.renderProblemsTo(ctx.io.stderr());
//!   return ctx.exitCode();

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const reporting = @import("reporting");
const problem_mod = @import("CliProblem.zig");
const CoreCtx = @import("ctx").CoreCtx;

const CliProblem = problem_mod.CliProblem;
const ColorPalette = reporting.ColorPalette;
const ReportingConfig = reporting.ReportingConfig;

/// I/O interface for CLI operations.
/// Owns the buffered writer state (per-stream byte buffers and File.Writer
/// instances) layered on top of `std.Io`, so the CLI can flush and rebind
/// stdout/stderr without each call site re-creating that state.
pub const Io = struct {
    std_io: std.Io,
    stdout_writer: std.Io.File.Writer,
    stderr_writer: std.Io.File.Writer,
    stdout_buffer: [4096]u8,
    stderr_buffer: [4096]u8,

    const Self = @This();

    /// Create an uninitialized Io struct.
    /// MUST call initWriters() after placing the struct at its final location.
    pub fn create(std_io: std.Io) Self {
        return Self{
            .std_io = std_io,
            .stdout_writer = undefined,
            .stderr_writer = undefined,
            .stdout_buffer = undefined,
            .stderr_buffer = undefined,
        };
    }

    /// Initialize the writers after the struct is at its final memory location.
    /// This MUST be called before using stdout() or stderr().
    /// Also enables ANSI escape sequences for colored output.
    pub fn initWriters(self: *Self) void {
        const stdout_file = std.Io.File.stdout();
        const stderr_file = std.Io.File.stderr();

        // Enable ANSI escape sequences for colored output (needed on Windows)
        stdout_file.enableAnsiEscapeCodes(self.std_io) catch {};
        stderr_file.enableAnsiEscapeCodes(self.std_io) catch {};

        self.stdout_writer = stdout_file.writerStreaming(self.std_io, &self.stdout_buffer);
        self.stderr_writer = stderr_file.writerStreaming(self.std_io, &self.stderr_buffer);
    }

    /// Get the stdout writer interface
    pub fn stdout(self: *Self) *std.Io.Writer {
        return &self.stdout_writer.interface;
    }

    /// Get the stderr writer interface
    pub fn stderr(self: *Self) *std.Io.Writer {
        return &self.stderr_writer.interface;
    }

    /// Total bytes handed to stdout and stderr so far, counting both what has
    /// already reached the OS and what is still sitting in the buffers. The top
    /// level uses this to tell "the command explained itself" apart from "the
    /// command exited non-zero in silence", without every reporting site having
    /// to announce that it reported.
    pub fn bytesWritten(self: *const Self) u64 {
        return self.stdout_writer.pos + self.stdout_writer.interface.end +
            self.stderr_writer.pos + self.stderr_writer.interface.end;
    }

    /// Flush both stdout and stderr buffers
    pub fn flush(self: *Self) void {
        self.stdout_writer.interface.flush() catch {};
        self.stderr_writer.interface.flush() catch {};
    }
};

/// Standard output streams whose ANSI capabilities can differ when one is
/// redirected and the other remains attached to a terminal.
pub const OutputStream = enum {
    stdout,
    stderr,
};

/// If output should use colors
pub const ColorMode = enum {
    auto,
    always,
    never,
};

/// Process-wide color preferences. Stream capability is applied separately so
/// `.auto` can make the correct decision for stdout and stderr independently.
pub const ColorPolicy = struct {
    mode: ColorMode,
    high_contrast: bool,

    fn resolve(env: ColorEnvironment) ColorPolicy {
        if (env.no_color) {
            return .{ .mode = .never, .high_contrast = false };
        }

        if (env.force_color) {
            return .{ .mode = .always, .high_contrast = env.high_contrast };
        }

        if (env.dumb_terminal) {
            return .{ .mode = .never, .high_contrast = false };
        }

        return .{ .mode = .auto, .high_contrast = env.high_contrast };
    }

    fn usesColor(self: ColorPolicy, supports_ansi: bool) bool {
        return switch (self.mode) {
            .auto => supports_ansi,
            .always => true,
            .never => false,
        };
    }
};

const ColorEnvironment = struct {
    no_color: bool,
    force_color: bool,
    dumb_terminal: bool,
    high_contrast: bool,
};

/// The single error type for CLI operations.
/// When a function returns this error, it means a problem has been recorded
/// in the CliCtx and will be rendered at the top level.
pub const CliError = error{CliError};

/// CLI commands that can generate errors
pub const Command = enum {
    build,
    run,
    check,
    test_cmd,
    dev,
    fmt,
    bundle,
    unbundle,
    docs,
    bump,
    repl,
    install,
    unknown,

    pub fn name(self: Command) []const u8 {
        return switch (self) {
            .build => "build",
            .run => "run",
            .check => "check",
            .test_cmd => "test",
            .dev => "dev",
            .fmt => "fmt",
            .bundle => "bundle",
            .unbundle => "unbundle",
            .docs => "docs",
            .bump => "bump",
            .repl => "repl",
            .install => "install",
            .unknown => "unknown",
        };
    }
};

/// Shared context for CLI operations.
/// Contains allocators, I/O, and accumulated problems.
pub const CliCtx = struct {
    /// General purpose allocator for long-lived allocations
    gpa: Allocator,
    /// Arena allocator for temporary/scoped allocations
    arena: Allocator,
    /// I/O interface for stdout/stderr
    io: *Io,
    /// Accumulated problems during CLI operations
    problems: std.ArrayList(CliProblem),
    /// The CLI command being executed
    command: Command,
    /// Exit code based on problem severity
    exit_code: u8,
    /// Explicit CLI override. This is folded into `color_policy` on first use.
    no_color: bool,
    /// Lazily loaded once because color environment variables are process-wide.
    color_policy: ?ColorPolicy,

    const Self = @This();

    /// Initialize a new CLI context.
    /// After init, call initIo() once the context is at its final memory location.
    pub fn init(gpa: Allocator, arena: Allocator, io: *Io, command: Command) Self {
        return .{
            .gpa = gpa,
            .arena = arena,
            .io = io,
            .problems = std.ArrayList(CliProblem).empty,
            .command = command,
            .exit_code = 0,
            .no_color = false,
            .color_policy = null,
        };
    }

    /// Initialize the I/O writers. Must be called after the context is at its
    /// final memory location (i.e., after init() returns and the result is stored).
    pub fn initIo(self: *Self) void {
        self.io.initWriters();
    }

    /// Create a CoreCtx from this CLI context's allocators and I/O.
    pub fn coreCtx(self: *const Self) CoreCtx {
        return CoreCtx.default(self.gpa, self.arena, self.io.std_io);
    }

    /// Set the parser-owned CLI override before the color policy is consumed.
    pub fn setNoColor(self: *Self, no_color: bool) void {
        std.debug.assert(self.color_policy == null);
        self.no_color = no_color;
    }

    /// Build terminal-layout defaults before applying color policy. Diagnostic
    /// headers use the detected width, capped by the renderer at 120 columns.
    fn baseReportConfig(self: *const Self) ReportingConfig {
        var config = ReportingConfig.initColorTerminal();
        if (self.coreCtx().terminalWidth()) |cols| {
            if (cols > 0) config.max_line_width = cols;
        }
        return config;
    }

    /// Resolve process-wide color settings once through CoreCtx so native,
    /// testing, and freestanding environments use the same explicit I/O
    /// abstraction.
    pub fn colorPolicy(self: *Self) ColorPolicy {
        if (self.color_policy) |policy| return policy;

        if (comptime builtin.target.cpu.arch == .wasm32 or builtin.target.os.tag == .freestanding) {
            const policy = ColorPolicy{ .mode = .never, .high_contrast = false };
            self.color_policy = policy;
            return policy;
        }

        const core_ctx = self.coreCtx();
        const policy = ColorPolicy.resolve(.{
            .no_color = self.no_color or core_ctx.envVarIsNonEmpty("NO_COLOR"),
            .force_color = core_ctx.envVarIsNonEmpty("FORCE_COLOR"),
            .dumb_terminal = core_ctx.envVarEquals("TERM", "dumb"),
            .high_contrast = core_ctx.envVarEquals("ROC_HIGH_CONTRAST", "1"),
        });
        self.color_policy = policy;
        return policy;
    }

    /// Whether a particular standard stream should receive ANSI color.
    pub fn usesColor(self: *Self, stream: OutputStream) bool {
        const policy = self.colorPolicy();
        return policy.usesColor(self.streamSupportsAnsi(stream));
    }

    /// Build the terminal reporting configuration while applying the shared
    /// color policy for the selected output stream.
    pub fn reportConfig(self: *Self, stream: OutputStream) ReportingConfig {
        const policy = self.colorPolicy();
        const supports_ansi = self.streamSupportsAnsi(stream);
        const use_color = policy.usesColor(supports_ansi);

        var config = self.baseReportConfig();
        config.is_tty = supports_ansi;
        config.color_preference = if (!use_color)
            .never
        else if (policy.high_contrast)
            .high_contrast
        else
            .always;
        return config;
    }

    fn streamSupportsAnsi(self: *const Self, stream: OutputStream) bool {
        if (comptime builtin.target.cpu.arch == .wasm32 or builtin.target.os.tag == .freestanding) return false;

        const file = switch (stream) {
            .stdout => std.Io.File.stdout(),
            .stderr => std.Io.File.stderr(),
        };
        return file.supportsAnsiEscapeCodes(self.io.std_io) catch false;
    }

    /// Clean up resources and flush I/O
    pub fn deinit(self: *Self) void {
        self.io.flush();
        self.problems.deinit(self.gpa);
    }

    /// Add a problem to the context
    pub fn addProblem(self: *Self, problem: CliProblem) Allocator.Error!void {
        try self.problems.append(self.gpa, problem);

        // Update exit code based on severity
        const sev = problem.severity();
        switch (sev) {
            .fatal => self.exit_code = 1,
            .runtime_error => if (self.exit_code == 0) {
                self.exit_code = 1;
            },
            .warning => {},
        }
    }

    /// Add a problem, ignoring allocation failures (for use in error paths)
    pub fn addProblemIgnoreError(self: *Self, problem: CliProblem) void {
        self.addProblem(problem) catch {};
    }

    /// Add a problem and return CliError.
    /// This is the primary way to report errors - it ensures every error
    /// is properly recorded before the function returns.
    ///
    /// Usage:
    ///   const file = std.Io.Dir.cwd().openFile(path, .{}) catch |err| {
    ///       return ctx.fail(.{ .file_not_found = .{ .path = path } });
    ///   };
    pub fn fail(self: *Self, problem: CliProblem) CliError {
        self.addProblemIgnoreError(problem);
        return error.CliError;
    }

    /// Check if any problems have been recorded
    pub fn hasProblems(self: *const Self) bool {
        return self.problems.items.len > 0;
    }

    /// Check if any errors (not just warnings) have been recorded
    pub fn hasErrors(self: *const Self) bool {
        for (self.problems.items) |problem| {
            const sev = problem.severity();
            if (sev == .fatal or sev == .runtime_error) {
                return true;
            }
        }
        return false;
    }

    /// Get the number of problems
    pub fn problemCount(self: *const Self) usize {
        return self.problems.items.len;
    }

    /// Get the number of errors (fatal + runtime_error)
    pub fn errorCount(self: *const Self) usize {
        var count: usize = 0;
        for (self.problems.items) |problem| {
            const sev = problem.severity();
            if (sev == .fatal or sev == .runtime_error) {
                count += 1;
            }
        }
        return count;
    }

    /// Get the number of warnings
    pub fn warningCount(self: *const Self) usize {
        var count: usize = 0;
        for (self.problems.items) |problem| {
            if (problem.severity() == .warning) {
                count += 1;
            }
        }
        return count;
    }

    /// Render all problems to a writer
    pub fn renderProblemsTo(self: *Self, writer: anytype) (Allocator.Error || error{WriteFailed})!void {
        const config = self.reportConfig(.stderr);

        for (self.problems.items) |problem| {
            var report = try problem.toReport(self.gpa);
            defer report.deinit();
            try reporting.renderReportToTerminal(&report, writer, reporting.ColorUtils.getPaletteForConfig(config), config);
        }
    }

    /// Render all problems and return whether there were any errors
    pub fn renderAndCheck(self: *Self, writer: anytype) (Allocator.Error || error{WriteFailed})!bool {
        try self.renderProblemsTo(writer);
        return self.hasErrors();
    }

    /// Clear all problems
    pub fn clear(self: *Self) void {
        self.problems.clearRetainingCapacity();
        self.exit_code = 0;
    }

    /// Get exit code based on recorded problems
    pub fn exitCode(self: *const Self) u8 {
        return self.exit_code;
    }
};

// Helper Functions

/// Create a context, add a single problem, render it, and return the exit code.
/// Convenience function for simple error cases.
pub fn reportSingleProblem(
    allocator: Allocator,
    io: *Io,
    command: Command,
    problem: CliProblem,
) u8 {
    var ctx = CliCtx.init(allocator, allocator, io, command);
    defer ctx.deinit();

    ctx.addProblemIgnoreError(problem);
    ctx.renderProblemsTo(io.stderr()) catch {};

    return ctx.exitCode();
}

/// Render a single problem without adding it to the context's accumulated
/// problem list.
pub fn renderProblem(ctx: *CliCtx, problem: CliProblem) Allocator.Error!void {
    var report = try problem.toReport(ctx.gpa);
    defer report.deinit();

    const config = ctx.reportConfig(.stderr);
    reporting.renderReportToTerminal(&report, ctx.io.stderr(), reporting.ColorUtils.getPaletteForConfig(config), config) catch {};
}

// Tests

const merged_stdio_helper_path_env = "ROC_CLI_IO_WRITER_TEST_HELPER";
const merged_stdout_payload = "stdout \u{2713} issue-10465\n" ** 256;
const merged_stderr_payload = "stderr \u{2713} issue-10465\n" ** 256;

test "issue 10465 merged standard streams preserve both buffered outputs" {
    const allocator = std.testing.allocator;
    const test_io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const combined_path = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp.sub_path, "combined.log" });
    defer allocator.free(combined_path);

    const helper_path_z = std.c.getenv(merged_stdio_helper_path_env) orelse return error.TestUnexpectedResult;
    const helper_path = helper_path_z[0..std.mem.len(helper_path_z)];

    var child = try std.process.spawn(test_io, .{
        .argv = &.{ helper_path, combined_path },
        .stdin = .ignore,
        .stdout = .ignore,
        .stderr = .ignore,
    });
    errdefer child.kill(test_io);

    const term = try child.wait(test_io);
    switch (term) {
        .exited => |code| try std.testing.expectEqual(@as(u8, 0), code),
        .signal, .stopped, .unknown => return error.TestUnexpectedResult,
    }

    const combined = try tmp.dir.readFileAlloc(test_io, "combined.log", allocator, .limited(64 * 1024));
    defer allocator.free(combined);

    try std.testing.expect(std.mem.find(u8, combined, merged_stderr_payload) != null);
    try std.testing.expect(std.mem.find(u8, combined, merged_stdout_payload) != null);
}

test "CliCtx accumulates problems" {
    const allocator = std.testing.allocator;
    var io = Io.create(std.testing.io);

    var ctx = CliCtx.init(allocator, allocator, &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    try std.testing.expect(!ctx.hasProblems());
    try std.testing.expect(!ctx.hasErrors());
    try std.testing.expectEqual(@as(usize, 0), ctx.problemCount());

    try ctx.addProblem(.{ .file_not_found = .{ .path = "app.roc" } });

    try std.testing.expect(ctx.hasProblems());
    try std.testing.expect(ctx.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ctx.problemCount());
    try std.testing.expectEqual(@as(u8, 1), ctx.exitCode());
}

test "CliCtx no_color selects plain reporting" {
    const allocator = std.testing.allocator;
    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(allocator, allocator, &io, .build);
    ctx.setNoColor(true);
    ctx.initIo();
    defer ctx.deinit();

    try std.testing.expectEqual(ColorMode.never, ctx.colorPolicy().mode);
    const config = ctx.reportConfig(.stderr);
    try std.testing.expect(!config.shouldUseColors());
    try std.testing.expectEqual(ColorPalette.NO_COLOR, reporting.ColorUtils.getPaletteForConfig(config));
}

test "CliCtx counts errors vs warnings correctly" {
    const allocator = std.testing.allocator;
    var io = Io.create(std.testing.io);

    var ctx = CliCtx.init(allocator, allocator, &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    try ctx.addProblem(.{ .file_not_found = .{ .path = "a.roc" } }); // fatal
    try ctx.addProblem(.{ .file_read_failed = .{ .path = "b.roc", .err = error.OutOfMemory } }); // runtime_error

    try std.testing.expectEqual(@as(usize, 2), ctx.errorCount());
    try std.testing.expectEqual(@as(usize, 0), ctx.warningCount());
}

test "CliCtx clear resets state" {
    const allocator = std.testing.allocator;
    var io = Io.create(std.testing.io);

    var ctx = CliCtx.init(allocator, allocator, &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    try ctx.addProblem(.{ .file_not_found = .{ .path = "app.roc" } });
    try std.testing.expect(ctx.hasErrors());

    ctx.clear();

    try std.testing.expect(!ctx.hasProblems());
    try std.testing.expectEqual(@as(u8, 0), ctx.exitCode());
}

test "Command names are correct" {
    try std.testing.expectEqualStrings("build", Command.build.name());
    try std.testing.expectEqualStrings("run", Command.run.name());
    try std.testing.expectEqualStrings("test", Command.test_cmd.name());
}

test "color policy resolves environment precedence" {
    const automatic = ColorPolicy.resolve(.{
        .no_color = false,
        .force_color = false,
        .dumb_terminal = false,
        .high_contrast = false,
    });
    try std.testing.expectEqual(ColorMode.auto, automatic.mode);
    try std.testing.expect(automatic.usesColor(true));
    try std.testing.expect(!automatic.usesColor(false));

    const no_color_wins = ColorPolicy.resolve(.{
        .no_color = true,
        .force_color = true,
        .dumb_terminal = false,
        .high_contrast = true,
    });
    try std.testing.expectEqual(ColorMode.never, no_color_wins.mode);
    try std.testing.expect(!no_color_wins.high_contrast);

    const force_color_wins_over_dumb_terminal = ColorPolicy.resolve(.{
        .no_color = false,
        .force_color = true,
        .dumb_terminal = true,
        .high_contrast = true,
    });
    try std.testing.expectEqual(ColorMode.always, force_color_wins_over_dumb_terminal.mode);
    try std.testing.expect(force_color_wins_over_dumb_terminal.high_contrast);
    try std.testing.expect(force_color_wins_over_dumb_terminal.usesColor(false));

    const dumb_terminal = ColorPolicy.resolve(.{
        .no_color = false,
        .force_color = false,
        .dumb_terminal = true,
        .high_contrast = true,
    });
    try std.testing.expectEqual(ColorMode.never, dumb_terminal.mode);
}

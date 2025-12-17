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
//!   fn doSomething(ctx: *CliContext, path: []const u8) CliError!void {
//!       const source = std.fs.cwd().readFileAlloc(ctx.gpa, path, ...) catch |err| {
//!           return ctx.fail(.{ .file_not_found = .{ .path = path } });
//!       };
//!       defer ctx.gpa.free(source);
//!       // Use ctx.arena for temporary allocations...
//!   }
//!
//!   // At top level:
//!   var io = Io.init();
//!   var ctx = CliContext.init(gpa, arena, &io, .build);
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
const Allocator = std.mem.Allocator;
const reporting = @import("reporting");
const problem_mod = @import("problem.zig");

const CliProblem = problem_mod.CliProblem;
const Report = reporting.Report;
const Severity = reporting.Severity;
const ColorPalette = reporting.ColorPalette;
const ReportingConfig = reporting.ReportingConfig;

/// I/O interface for CLI operations.
/// Wraps stdout/stderr with buffered writers. When Zig's std.Io interface
/// becomes available, this struct will be replaced with std.Io.
pub const Io = struct {
    stdout_writer: std.fs.File.Writer,
    stderr_writer: std.fs.File.Writer,
    stdout_buffer: [4096]u8,
    stderr_buffer: [4096]u8,

    const Self = @This();

    /// Create an uninitialized Io struct.
    /// MUST call initWriters() after placing the struct at its final location.
    pub fn init() Self {
        return Self{
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
        const stdout_file = std.fs.File.stdout();
        const stderr_file = std.fs.File.stderr();

        // Enable ANSI escape sequences for colored output (needed on Windows)
        _ = stdout_file.getOrEnableAnsiEscapeSupport();
        _ = stderr_file.getOrEnableAnsiEscapeSupport();

        self.stdout_writer = stdout_file.writer(&self.stdout_buffer);
        self.stderr_writer = stderr_file.writer(&self.stderr_buffer);
    }

    /// Get the stdout writer interface
    pub fn stdout(self: *Self) *std.Io.Writer {
        return &self.stdout_writer.interface;
    }

    /// Get the stderr writer interface
    pub fn stderr(self: *Self) *std.Io.Writer {
        return &self.stderr_writer.interface;
    }

    /// Flush both stdout and stderr buffers
    pub fn flush(self: *Self) void {
        self.stdout_writer.interface.flush() catch {};
        self.stderr_writer.interface.flush() catch {};
    }
};

/// The single error type for CLI operations.
/// When a function returns this error, it means a problem has been recorded
/// in the CliContext and will be rendered at the top level.
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
    repl,
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
            .repl => "repl",
            .unknown => "unknown",
        };
    }
};

/// Shared context for CLI operations.
/// Contains allocators, I/O, and accumulated problems.
pub const CliContext = struct {
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
        };
    }

    /// Initialize the I/O writers. Must be called after the context is at its
    /// final memory location (i.e., after init() returns and the result is stored).
    pub fn initIo(self: *Self) void {
        self.io.initWriters();
    }

    /// Clean up resources and flush I/O
    pub fn deinit(self: *Self) void {
        self.io.flush();
        self.problems.deinit(self.gpa);
    }

    /// Add a problem to the context
    pub fn addProblem(self: *Self, problem: CliProblem) !void {
        try self.problems.append(self.gpa, problem);

        // Update exit code based on severity
        const sev = problem.severity();
        switch (sev) {
            .fatal => self.exit_code = 1,
            .runtime_error => if (self.exit_code == 0) {
                self.exit_code = 1;
            },
            .warning, .info => {},
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
    ///   const file = std.fs.cwd().openFile(path, .{}) catch |err| {
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
    pub fn renderProblemsTo(self: *Self, writer: anytype) !void {
        const config = ReportingConfig.initColorTerminal();

        for (self.problems.items) |problem| {
            var report = try problem.toReport(self.gpa);
            defer report.deinit();
            try reporting.renderReportToTerminal(&report, writer, ColorPalette.ANSI, config);
        }
    }

    /// Render all problems and return whether there were any errors
    pub fn renderAndCheck(self: *Self, writer: anytype) !bool {
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

    // Backward compatibility aliases
    pub const suggestedExitCode = exitCode;
    pub const renderAll = renderProblemsTo;
};

/// Backward compatibility alias
pub const CliErrorContext = CliContext;

// Helper Functions

/// Create a context, add a single problem, render it, and return the exit code.
/// Convenience function for simple error cases.
pub fn reportSingleProblem(
    allocator: Allocator,
    io: *Io,
    command: Command,
    problem: CliProblem,
) u8 {
    var ctx = CliContext.init(allocator, allocator, io, command);
    defer ctx.deinit();

    ctx.addProblemIgnoreError(problem);
    ctx.renderProblemsTo(io.stderr()) catch {};

    return ctx.exitCode();
}

/// Render a single problem without creating a full context.
/// Useful for one-off errors that don't need accumulation.
pub fn renderProblem(
    allocator: Allocator,
    writer: anytype,
    problem: CliProblem,
) void {
    var report = problem.toReport(allocator) catch return;
    defer report.deinit();

    const config = ReportingConfig.initColorTerminal();
    reporting.renderReportToTerminal(&report, writer, ColorPalette.ANSI, config) catch {};
}

// Tests

test "CliContext accumulates problems" {
    const allocator = std.testing.allocator;
    var io = Io.init();

    var ctx = CliContext.init(allocator, allocator, &io, .build);
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

test "CliContext counts errors vs warnings correctly" {
    const allocator = std.testing.allocator;
    var io = Io.init();

    var ctx = CliContext.init(allocator, allocator, &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    try ctx.addProblem(.{ .file_not_found = .{ .path = "a.roc" } }); // fatal
    try ctx.addProblem(.{ .file_read_failed = .{ .path = "b.roc", .err = error.OutOfMemory } }); // runtime_error

    try std.testing.expectEqual(@as(usize, 2), ctx.errorCount());
    try std.testing.expectEqual(@as(usize, 0), ctx.warningCount());
}

test "CliContext clear resets state" {
    const allocator = std.testing.allocator;
    var io = Io.init();

    var ctx = CliContext.init(allocator, allocator, &io, .build);
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

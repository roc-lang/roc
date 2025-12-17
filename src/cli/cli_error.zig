//! CLI Context and Error Handling
//!
//! Provides the shared CLI context and structured error types for CLI operations.
//!
//! The key design principle is that `CliError` is the ONLY error type that
//! CLI functions should return. This ensures:
//! - Every error is properly reported (no silent failures)
//! - Consistent error formatting across all commands
//! - The type system enforces proper error handling
//!
//! This module exports:
//! - `CliContext`: Shared context with allocators, writers, and error accumulation
//! - `CliError`: The single error type for CLI operations
//! - `CliProblem`: Union type representing all CLI error conditions
//! - `FileContext`: Context enum for file operation errors
//! - `Command`: CLI command enum
//!
//! Usage:
//! ```zig
//! const cli = @import("cli_error.zig");
//!
//! fn doSomething(ctx: *cli.CliContext, path: []const u8) cli.CliError!void {
//!     const source = std.fs.cwd().readFileAlloc(ctx.gpa, path, ...) catch {
//!         return ctx.fail(.{ .file_not_found = .{ .path = path } });
//!     };
//!     defer ctx.gpa.free(source);
//!     // Use ctx.arena for temporary allocations...
//! }
//!
//! // At top level:
//! var ctx = cli.CliContext.init(gpa, arena, .build);
//! defer ctx.deinit();
//!
//! doSomething(&ctx, "app.roc") catch |err| switch (err) {
//!     error.CliError => {}, // Problems already recorded
//! };
//!
//! try ctx.renderProblems();
//! return ctx.exitCode();
//! ```

const problem = @import("cli_error/problem.zig");
const context = @import("cli_error/context.zig");

// Re-export error type
pub const CliError = context.CliError;

// Re-export main types - CliContext is the primary type
pub const CliContext = context.CliContext;
pub const CliProblem = problem.CliProblem;
pub const FileContext = problem.FileContext;
pub const Command = context.Command;

// Backward compatibility alias
pub const CliErrorContext = context.CliErrorContext;

// Re-export helper functions
pub const reportSingleProblem = context.reportSingleProblem;
pub const renderProblem = context.renderProblem;

const std = @import("std");

test "cli_error tests" {
    std.testing.refAllDecls(@import("cli_error/problem.zig"));
    std.testing.refAllDecls(@import("cli_error/context.zig"));
}

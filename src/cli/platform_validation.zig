//! Platform header validation utilities.
//!
//! Provides shared validation logic for platform headers, including:
//! - Parsing platform headers to extract TargetsConfig
//! - Validating targets section exists
//! - Validating target files exist on disk
//! - Validating a specific target is supported
//!
//! This module is used by both `roc build` and `roc bundle` commands.

const std = @import("std");
const builtin = @import("builtin");
const parse = @import("parse");
const base = @import("base");
const reporting = @import("reporting");
const target_mod = @import("target.zig");
pub const targets_validator = @import("targets_validator.zig");

const TargetsConfig = target_mod.TargetsConfig;
const RocTarget = target_mod.RocTarget;

const is_windows = builtin.target.os.tag == .windows;

var stderr_file_writer: std.Io.File.Writer = .{
    .io = std.Io.Threaded.global_single_threaded.io(),
    .interface = std.Io.File.Writer.initInterface(&.{}),
    .file = if (is_windows) undefined else std.Io.File.stderr(),
    .mode = .streaming,
};

fn stderrWriter() *std.Io.Writer {
    if (is_windows) stderr_file_writer.file = std.Io.File.stderr();
    return &stderr_file_writer.interface;
}

/// Re-export ValidationResult for callers that need to create reports
pub const ValidationResult = targets_validator.ValidationResult;

/// Errors that can occur during platform validation
pub const ValidationError = error{
    /// Platform header is missing required targets section
    MissingTargetsSection,
    /// Requested target is not declared in platform's targets section
    UnsupportedTarget,
    /// A file declared in targets section doesn't exist
    MissingTargetFile,
    /// Files directory specified in targets section doesn't exist
    MissingFilesDirectory,
    /// Failed to parse platform header
    ParseError,
    /// Failed to read platform source file
    FileReadError,
    /// Out of memory
    OutOfMemory,
};

/// Result of platform validation with parsed config
pub const PlatformValidation = struct {
    /// Parsed targets configuration
    config: TargetsConfig,
    /// Directory containing the platform (dirname of platform source)
    platform_dir: []const u8,
};

/// Parse and validate a platform header.
/// Returns the TargetsConfig if valid, or an error with details.
pub fn validatePlatformHeader(
    allocator: std.mem.Allocator,
    std_io: std.Io,
    platform_source_path: []const u8,
) ValidationError!PlatformValidation {
    // Read platform source
    var source = std.Io.Dir.cwd().readFileAlloc(std_io, platform_source_path, allocator, .unlimited) catch {
        try renderFileReadError(allocator, platform_source_path);
        return error.FileReadError;
    };
    source = base.source_utils.normalizeLineEndingsRealloc(allocator, source) catch {
        allocator.free(source);
        return error.OutOfMemory;
    };

    // Parse platform header
    var env = base.CommonEnv.init(allocator, source) catch {
        std.log.err("Failed to initialize parse environment for: {s}", .{platform_source_path});
        return error.ParseError;
    };

    const ast = parse.file(allocator, &env) catch {
        try renderParseError(allocator, platform_source_path);
        return error.ParseError;
    };
    defer ast.deinit();

    // Extract TargetsConfig
    const config = TargetsConfig.fromAST(allocator, ast) catch {
        return error.ParseError;
    } orelse {
        try renderMissingTargetsError(allocator, platform_source_path);
        return error.MissingTargetsSection;
    };

    return .{
        .config = config,
        .platform_dir = std.fs.path.dirname(platform_source_path) orelse ".",
    };
}

/// Render a file read error report to stderr.
fn renderFileReadError(allocator: std.mem.Allocator, path: []const u8) std.mem.Allocator.Error!void {
    var report = try reporting.Report.init(allocator, "File Read Error", "Failed to read platform source file.", .fatal);
    defer report.deinit();

    try report.document.addText("    ");
    try report.document.addAnnotated(path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Check that the file exists and you have read permissions.");
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

/// Render a parse error report to stderr.
fn renderParseError(allocator: std.mem.Allocator, path: []const u8) std.mem.Allocator.Error!void {
    var report = try reporting.Report.init(allocator, "Parse Error", "Failed to parse platform header.", .fatal);
    defer report.deinit();

    try report.document.addText("    ");
    try report.document.addAnnotated(path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Check that the file contains valid Roc syntax.");
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

/// Render a missing targets section error report to stderr.
fn renderMissingTargetsError(allocator: std.mem.Allocator, path: []const u8) std.mem.Allocator.Error!void {
    const headline = try std.fmt.allocPrint(allocator, "Platform at {s} does not have a 'targets:' section.", .{path});
    defer allocator.free(headline);
    var report = try reporting.Report.init(allocator, "Missing Targets Section", headline, .fatal);
    defer report.deinit();

    try report.document.addText("Platform headers must declare supported targets. Example:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\    targets: {
        \\        inputs_dir: "targets/",
        \\        x64linux: { inputs: ["host.o", app] },
        \\        arm64linux: { inputs: ["host.o", app] },
        \\    }
    );
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

/// Validate that a specific target is supported by the platform.
/// Returns error.UnsupportedTarget if the target is not in the config.
/// Does not log - caller should handle error reporting.
pub fn validateTargetSupported(
    config: TargetsConfig,
    target: RocTarget,
) ValidationError!void {
    if (!config.supportsTarget(target)) {
        return error.UnsupportedTarget;
    }
}

/// Create a ValidationResult for an unsupported target error.
/// This can be passed to targets_validator.createValidationReport for nice error formatting.
pub fn createUnsupportedTargetResult(
    platform_path: []const u8,
    requested_target: RocTarget,
    config: TargetsConfig,
) ValidationResult {
    return .{
        .unsupported_target = .{
            .platform_path = platform_path,
            .requested_target = requested_target,
            .supported_targets = config.getSupportedTargets(),
        },
    };
}

/// Render a validation error to stderr using the reporting infrastructure.
/// Returns true if a report was rendered, false if no report was needed.
pub fn renderValidationError(
    allocator: std.mem.Allocator,
    result: ValidationResult,
    stderr: anytype,
) bool {
    switch (result) {
        .valid => return false,
        else => {
            var report = targets_validator.createValidationReport(allocator, result) catch {
                // Fallback to simple logging if report creation fails
                std.log.err("Platform validation failed", .{});
                return true;
            };
            defer report.deinit();

            reporting.renderReportToTerminal(
                &report,
                stderr,
                .ANSI,
                reporting.ReportingConfig.initColorTerminal(),
            ) catch {};
            return true;
        },
    }
}

/// Validate all files declared in targets section exist on disk.
/// Uses existing targets_validator infrastructure.
/// Returns the ValidationResult for nice error reporting, or null if validation passed.
pub fn validateAllTargetFilesExist(
    allocator: std.mem.Allocator,
    std_io: std.Io,
    config: TargetsConfig,
    platform_dir_path: []const u8,
) ?ValidationResult {
    var platform_dir = std.Io.Dir.cwd().openDir(std_io, platform_dir_path, .{}) catch {
        return .{
            .missing_files_directory = .{
                .platform_path = platform_dir_path,
                .files_dir = config.inputs_dir orelse "targets",
            },
        };
    };
    defer platform_dir.close(std_io);

    const result = targets_validator.validateTargetFilesExist(allocator, std_io, config, platform_dir) catch {
        return .{
            .missing_files_directory = .{
                .platform_path = platform_dir_path,
                .files_dir = config.inputs_dir orelse "targets",
            },
        };
    };

    switch (result) {
        .valid => return null,
        else => return result,
    }
}

// Tests
const testing = std.testing;

test "validateTargetSupported returns error for unsupported target" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .x64mac, .output = .exe, .items = &.{.app} },
            .{ .target = .arm64mac, .output = .exe, .items = &.{.app} },
        },
    };

    // x64musl is not in the config, should error
    const result = validateTargetSupported(config, .x64musl);
    try testing.expectError(error.UnsupportedTarget, result);
}

test "validateTargetSupported succeeds for supported target" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .x64mac, .output = .exe, .items = &.{.app} },
            .{ .target = .arm64mac, .output = .exe, .items = &.{.app} },
        },
    };

    // x64mac is in the config, should succeed
    try validateTargetSupported(config, .x64mac);
}

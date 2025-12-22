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
const LinkType = target_mod.LinkType;
const LinkItem = target_mod.LinkItem;
const TargetLinkSpec = target_mod.TargetLinkSpec;

const is_windows = builtin.target.os.tag == .windows;

var stderr_file_writer: std.fs.File.Writer = .{
    .interface = std.fs.File.Writer.initInterface(&.{}),
    .file = if (is_windows) undefined else std.fs.File.stderr(),
    .mode = .streaming,
};

fn stderrWriter() *std.Io.Writer {
    if (is_windows) stderr_file_writer.file = std.fs.File.stderr();
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

/// Check if a file is a platform header (has `platform` at the start).
/// This is a quick check without side effects - useful for finding which file
/// in a set of .roc files is the actual platform file.
/// Returns true if the file is a platform, false if it's a module/app, or null on error.
pub fn isPlatformFile(
    allocator: std.mem.Allocator,
    source_path: []const u8,
) ?bool {
    // Read source file
    var source = std.fs.cwd().readFileAlloc(allocator, source_path, std.math.maxInt(usize)) catch {
        return null;
    };
    source = base.source_utils.normalizeLineEndingsRealloc(allocator, source) catch {
        allocator.free(source);
        return null;
    };
    defer allocator.free(source);

    // Initialize parse environment
    var env = base.CommonEnv.init(allocator, source) catch {
        return null;
    };

    // Parse the file
    const ast = parse.parse(&env, allocator) catch {
        return null;
    };

    // Check the header type
    const file = ast.store.getFile();
    const header = ast.store.getHeader(file.header);

    return switch (header) {
        .platform => true,
        else => false,
    };
}

/// Parse and validate a platform header.
/// Returns the TargetsConfig if valid, or an error with details.
pub fn validatePlatformHeader(
    allocator: std.mem.Allocator,
    platform_source_path: []const u8,
) ValidationError!PlatformValidation {
    // Read platform source
    var source = std.fs.cwd().readFileAlloc(allocator, platform_source_path, std.math.maxInt(usize)) catch {
        renderFileReadError(allocator, platform_source_path);
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

    const ast = parse.parse(&env, allocator) catch {
        renderParseError(allocator, platform_source_path);
        return error.ParseError;
    };

    // Extract TargetsConfig
    const config = TargetsConfig.fromAST(allocator, ast) catch {
        return error.ParseError;
    } orelse {
        renderMissingTargetsError(allocator, platform_source_path);
        return error.MissingTargetsSection;
    };

    return .{
        .config = config,
        .platform_dir = std.fs.path.dirname(platform_source_path) orelse ".",
    };
}

/// Render a file read error report to stderr.
fn renderFileReadError(allocator: std.mem.Allocator, path: []const u8) void {
    var report = reporting.Report.init(allocator, "FILE READ ERROR", .fatal);
    defer report.deinit();

    report.document.addText("Failed to read platform source file:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    ") catch return;
    report.document.addAnnotated(path, .path) catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("Check that the file exists and you have read permissions.") catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

/// Render a parse error report to stderr.
fn renderParseError(allocator: std.mem.Allocator, path: []const u8) void {
    var report = reporting.Report.init(allocator, "PARSE ERROR", .fatal);
    defer report.deinit();

    report.document.addText("Failed to parse platform header:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    ") catch return;
    report.document.addAnnotated(path, .path) catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("Check that the file contains valid Roc syntax.") catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

/// Render a missing targets section error report to stderr.
fn renderMissingTargetsError(allocator: std.mem.Allocator, path: []const u8) void {
    var report = reporting.Report.init(allocator, "MISSING TARGETS SECTION", .fatal);
    defer report.deinit();

    report.document.addText("Platform at ") catch return;
    report.document.addAnnotated(path, .path) catch return;
    report.document.addText(" does not have a 'targets:' section.") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("Platform headers must declare supported targets. Example:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addCodeBlock(
        \\    targets: {
        \\        files: "targets/",
        \\        exe: {
        \\            x64linux: ["host.o", app],
        \\            arm64linux: ["host.o", app],
        \\        }
        \\    }
    ) catch return;
    report.document.addLineBreak() catch return;

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
    link_type: LinkType,
) ValidationError!void {
    if (!config.supportsTarget(target, link_type)) {
        return error.UnsupportedTarget;
    }
}

/// Create a ValidationResult for an unsupported target error.
/// This can be passed to targets_validator.createValidationReport for nice error formatting.
pub fn createUnsupportedTargetResult(
    platform_path: []const u8,
    requested_target: RocTarget,
    link_type: LinkType,
    config: TargetsConfig,
) ValidationResult {
    return .{
        .unsupported_target = .{
            .platform_path = platform_path,
            .requested_target = requested_target,
            .link_type = link_type,
            .supported_targets = config.getSupportedTargets(link_type),
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
    config: TargetsConfig,
    platform_dir_path: []const u8,
) ?ValidationResult {
    var platform_dir = std.fs.cwd().openDir(platform_dir_path, .{}) catch {
        return .{
            .missing_files_directory = .{
                .platform_path = platform_dir_path,
                .files_dir = config.files_dir orelse "targets",
            },
        };
    };
    defer platform_dir.close();

    const result = targets_validator.validateTargetFilesExist(allocator, config, platform_dir) catch {
        return .{
            .missing_files_directory = .{
                .platform_path = platform_dir_path,
                .files_dir = config.files_dir orelse "targets",
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
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{.app} },
            .{ .target = .arm64mac, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // x64musl is not in the config, should error
    const result = validateTargetSupported(config, .x64musl, .exe);
    try testing.expectError(error.UnsupportedTarget, result);
}

test "validateTargetSupported succeeds for supported target" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{.app} },
            .{ .target = .arm64mac, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // x64mac is in the config, should succeed
    try validateTargetSupported(config, .x64mac, .exe);
}

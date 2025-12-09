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
    platform_source_path: []const u8,
) ValidationError!PlatformValidation {
    // Read platform source
    const source = std.fs.cwd().readFileAlloc(allocator, platform_source_path, std.math.maxInt(usize)) catch {
        std.log.err("Failed to read platform source: {s}", .{platform_source_path});
        return error.FileReadError;
    };

    // Parse platform header
    var env = base.CommonEnv.init(allocator, source) catch {
        std.log.err("Failed to initialize parse environment for: {s}", .{platform_source_path});
        return error.ParseError;
    };

    const ast = parse.parse(&env, allocator) catch {
        std.log.err("Failed to parse platform header: {s}", .{platform_source_path});
        return error.ParseError;
    };

    // Extract TargetsConfig
    const config = TargetsConfig.fromAST(allocator, ast) catch {
        return error.ParseError;
    } orelse {
        std.log.err("Platform at '{s}' does not have a 'targets:' section", .{platform_source_path});
        std.log.err("Platform headers must declare supported targets. Example:", .{});
        std.log.err("  targets: {{ exe: {{ x64musl: [app], arm64musl: [app] }} }}", .{});
        return error.MissingTargetsSection;
    };

    return .{
        .config = config,
        .platform_dir = std.fs.path.dirname(platform_source_path) orelse ".",
    };
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

/// Get the full path to a target's host library.
/// Validates the file exists or returns an error.
pub fn getHostLibraryPath(
    allocator: std.mem.Allocator,
    config: TargetsConfig,
    platform_dir: []const u8,
    target: RocTarget,
) ValidationError![]const u8 {
    const link_spec = config.getLinkSpec(target, .exe) orelse {
        return error.UnsupportedTarget;
    };

    const files_dir = config.files_dir orelse "targets";
    const target_name = @tagName(target);

    // Find host library in link spec
    for (link_spec.items) |item| {
        switch (item) {
            .file_path => |file| {
                if (std.mem.endsWith(u8, file, "libhost.a") or std.mem.endsWith(u8, file, "host.o")) {
                    const full_path = std.fs.path.join(allocator, &.{
                        platform_dir, files_dir, target_name, file,
                    }) catch return error.OutOfMemory;

                    std.fs.cwd().access(full_path, .{}) catch {
                        std.log.err("Missing required file: {s}", .{full_path});
                        return error.MissingTargetFile;
                    };
                    return full_path;
                }
            },
            .app, .win_gui => {},
        }
    }

    // No explicit host library in spec - try default location
    const default_path = std.fs.path.join(allocator, &.{
        platform_dir, files_dir, target_name, "libhost.a",
    }) catch return error.OutOfMemory;

    std.fs.cwd().access(default_path, .{}) catch {
        std.log.err("Missing host library: {s}", .{default_path});
        return error.MissingTargetFile;
    };

    return default_path;
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

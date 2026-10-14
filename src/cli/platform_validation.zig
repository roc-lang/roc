//! Platform target validation utilities.
//!
//! Provides shared validation and reporting for a platform's targets section:
//! - Validating target files exist on disk
//! - Reporting an unsupported target
//! - Rendering validation results

const std = @import("std");
const reporting = @import("reporting");
const target_mod = @import("target.zig");
pub const targets_validator = @import("targets_validator.zig");

const TargetsConfig = target_mod.TargetsConfig;
const RocTarget = target_mod.RocTarget;

/// Re-export ValidationResult for callers that need to create reports
pub const ValidationResult = targets_validator.ValidationResult;

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
    report_config: reporting.ReportingConfig,
) bool {
    if (result == .valid) return false;

    var report = targets_validator.createValidationReport(allocator, result) catch {
        // Fallback to simple logging if report creation fails
        std.log.err("Platform validation failed", .{});
        return true;
    };
    defer report.deinit();

    reporting.renderReportToTerminal(
        &report,
        stderr,
        reporting.ColorUtils.getPaletteForConfig(report_config),
        report_config,
    ) catch {};
    return true;
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

    return if (result == .valid) null else result;
}

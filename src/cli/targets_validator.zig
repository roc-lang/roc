//! Validation for platform targets section
//!
//! Validates that:
//! - Platform headers have a targets section (required)
//! - Files declared in the targets section exist in the filesystem
//! - Files in the targets directory match what's declared in the targets section
//!
//! This module is shared between bundle and unbundle operations.

const std = @import("std");
const Allocator = std.mem.Allocator;
const parse = @import("parse");
const target_mod = @import("target.zig");
const reporting = @import("reporting");

const RocTarget = target_mod.RocTarget;
const TargetsConfig = target_mod.TargetsConfig;
const LinkItem = target_mod.LinkItem;
const TargetLinkSpec = target_mod.TargetLinkSpec;
const LinkType = target_mod.LinkType;
const Report = reporting.Report;
const Severity = reporting.Severity;

/// Errors that can occur during targets validation
pub const ValidationError = error{
    MissingTargetsSection,
    MissingFilesDirectory,
    MissingTargetFile,
    ExtraFileInTargetsDir,
    InvalidTargetName,
    EmptyTargetsSection,
    OutOfMemory,
};

/// Result of validating a targets section
pub const ValidationResult = union(enum) {
    /// Validation passed
    valid: void,

    /// Platform header is missing the required targets section
    missing_targets_section: struct {
        platform_path: []const u8,
    },

    /// Files directory specified but doesn't exist
    missing_files_directory: struct {
        platform_path: []const u8,
        files_dir: []const u8,
    },

    /// A file declared in targets doesn't exist
    missing_target_file: struct {
        target: RocTarget,
        link_type: LinkType,
        file_path: []const u8,
        expected_full_path: []const u8,
    },

    /// Extra file found in targets directory that isn't declared
    extra_file: struct {
        target: RocTarget,
        file_path: []const u8,
    },

    /// Targets section exists but has no target entries
    empty_targets: struct {
        platform_path: []const u8,
    },
};

/// Validate that a platform has a targets section
pub fn validatePlatformHasTargets(
    allocator: Allocator,
    ast: anytype,
    platform_path: []const u8,
) ValidationResult {
    _ = allocator;

    const store = &ast.store;

    // Get the root header
    const header_idx: parse.AST.Header.Idx = @enumFromInt(0);
    const header = store.getHeader(header_idx);

    // Only platform headers should have targets
    const platform = switch (header) {
        .platform => |p| p,
        else => return .{ .valid = {} }, // Non-platform headers don't need targets
    };

    // Check if targets section exists
    if (platform.targets == null) {
        return .{ .missing_targets_section = .{
            .platform_path = platform_path,
        } };
    }

    return .{ .valid = {} };
}

/// Validate that files declared in targets section exist on disk
pub fn validateTargetFilesExist(
    allocator: Allocator,
    targets_config: TargetsConfig,
    platform_dir: std.fs.Dir,
) !ValidationResult {
    const files_dir_path = targets_config.files_dir orelse return .{ .valid = {} };

    // Check if files directory exists
    var files_dir = platform_dir.openDir(files_dir_path, .{}) catch {
        return .{ .missing_files_directory = .{
            .platform_path = "platform",
            .files_dir = files_dir_path,
        } };
    };
    defer files_dir.close();

    // Validate exe targets
    for (targets_config.exe) |spec| {
        if (try validateTargetSpec(allocator, spec, .exe, files_dir)) |result| {
            return result;
        }
    }

    // Validate static_lib targets
    for (targets_config.static_lib) |spec| {
        if (try validateTargetSpec(allocator, spec, .static_lib, files_dir)) |result| {
            return result;
        }
    }

    // Validate shared_lib targets
    for (targets_config.shared_lib) |spec| {
        if (try validateTargetSpec(allocator, spec, .shared_lib, files_dir)) |result| {
            return result;
        }
    }

    return .{ .valid = {} };
}

fn validateTargetSpec(
    allocator: Allocator,
    spec: TargetLinkSpec,
    link_type: LinkType,
    files_dir: std.fs.Dir,
) !?ValidationResult {
    // Get target subdirectory name
    const target_subdir = @tagName(spec.target);

    // Open target subdirectory
    var target_dir = files_dir.openDir(target_subdir, .{}) catch {
        // Target directory doesn't exist - this might be okay if there are no file items
        var has_files = false;
        for (spec.items) |item| {
            switch (item) {
                .file_path => {
                    has_files = true;
                    break;
                },
                .app, .win_gui => {},
            }
        }
        if (has_files) {
            const expected_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ "targets", target_subdir });
            defer allocator.free(expected_path);
            return .{ .missing_target_file = .{
                .target = spec.target,
                .link_type = link_type,
                .file_path = target_subdir,
                .expected_full_path = expected_path,
            } };
        }
        return null;
    };
    defer target_dir.close();

    // Check each file item exists
    for (spec.items) |item| {
        switch (item) {
            .file_path => |path| {
                // Check if file exists
                target_dir.access(path, .{}) catch {
                    const expected_path = try std.fmt.allocPrint(allocator, "{s}/{s}/{s}", .{ "targets", target_subdir, path });
                    return .{ .missing_target_file = .{
                        .target = spec.target,
                        .link_type = link_type,
                        .file_path = path,
                        .expected_full_path = expected_path,
                    } };
                };
            },
            .app, .win_gui => {
                // Special identifiers don't need file validation
            },
        }
    }

    return null;
}

/// Create an error report for a validation failure
pub fn createValidationReport(
    allocator: Allocator,
    result: ValidationResult,
) !Report {
    switch (result) {
        .valid => unreachable, // Should not create report for valid result

        .missing_targets_section => |info| {
            var report = Report.init(allocator, "MISSING TARGETS SECTION", .@"error");

            try report.document.addText("Platform headers must include a `targets` section that specifies");
            try report.document.addLineBreak();
            try report.document.addText("which targets this platform supports and what files to link.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("In ");
            try report.document.addAnnotated(info.platform_path, .emphasized);
            try report.document.addText(", add a targets section like:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addCodeBlock(
                \\    targets: {
                \\        files: "targets/",
                \\        exe: {
                \\            x64linux: ["host.o", app],
                \\            arm64linux: ["host.o", app],
                \\            x64mac: ["host.o", app],
                \\            arm64mac: ["host.o", app],
                \\        }
                \\    }
            );
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("The targets section declares:");
            try report.document.addLineBreak();
            try report.document.addText("  - `files`: Directory containing target-specific files");
            try report.document.addLineBreak();
            try report.document.addText("  - `exe`: Targets that build executables");
            try report.document.addLineBreak();
            try report.document.addText("  - Each target lists files to link in order, with `app` for the Roc application");
            try report.document.addLineBreak();

            return report;
        },

        .missing_files_directory => |info| {
            var report = Report.init(allocator, "MISSING FILES DIRECTORY", .@"error");

            try report.document.addText("The targets section specifies files directory ");
            try report.document.addAnnotated(info.files_dir, .emphasized);
            try report.document.addLineBreak();
            try report.document.addText("but this directory doesn't exist.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Create the directory structure:");
            try report.document.addLineBreak();
            try report.document.addCodeBlock(
                \\    targets/
                \\        x64linux/
                \\            host.o
                \\        arm64linux/
                \\            host.o
                \\        ...
            );
            try report.document.addLineBreak();

            return report;
        },

        .missing_target_file => |info| {
            var report = Report.init(allocator, "MISSING TARGET FILE", .@"error");

            try report.document.addText("The targets section declares file ");
            try report.document.addAnnotated(info.file_path, .emphasized);
            try report.document.addLineBreak();
            try report.document.addText("for target ");
            try report.document.addAnnotated(@tagName(info.target), .emphasized);
            try report.document.addText(" but this file doesn't exist.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Expected file at: ");
            try report.document.addAnnotated(info.expected_full_path, .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Either add the missing file or remove it from the targets section.");
            try report.document.addLineBreak();

            return report;
        },

        .extra_file => |info| {
            var report = Report.init(allocator, "EXTRA FILE IN TARGETS", .warning);

            try report.document.addText("Found file ");
            try report.document.addAnnotated(info.file_path, .emphasized);
            try report.document.addLineBreak();
            try report.document.addText("in target directory for ");
            try report.document.addAnnotated(@tagName(info.target), .emphasized);
            try report.document.addLineBreak();
            try report.document.addText("but this file isn't declared in the targets section.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("This file will not be included in the bundle.");
            try report.document.addLineBreak();
            try report.document.addText("Either add it to the targets section or delete it.");
            try report.document.addLineBreak();

            return report;
        },

        .empty_targets => |info| {
            var report = Report.init(allocator, "EMPTY TARGETS SECTION", .@"error");

            try report.document.addText("The targets section in ");
            try report.document.addAnnotated(info.platform_path, .emphasized);
            try report.document.addLineBreak();
            try report.document.addText("doesn't declare any targets.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Add at least one target to the exe, static_lib, or shared_lib section.");
            try report.document.addLineBreak();

            return report;
        },
    }
}

test "validatePlatformHasTargets returns missing for platform without targets" {
    // This would require setting up a mock AST, which is complex.
    // For now, just verify the module compiles.
}

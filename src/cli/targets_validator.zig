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

    // Get the file node first, then get the header from it
    const file = store.getFile();
    const header = store.getHeader(file.header);

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
            var report = Report.init(allocator, "MISSING TARGETS SECTION", .runtime_error);

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
            var report = Report.init(allocator, "MISSING FILES DIRECTORY", .runtime_error);

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
            var report = Report.init(allocator, "MISSING TARGET FILE", .runtime_error);

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
            var report = Report.init(allocator, "EMPTY TARGETS SECTION", .runtime_error);

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

test "createValidationReport generates correct report for missing_targets_section" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .missing_targets_section = .{ .platform_path = "test/platform/main.roc" },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("MISSING TARGETS SECTION", report.title);
    try std.testing.expectEqual(Severity.runtime_error, report.severity);
}

test "createValidationReport generates correct report for missing_files_directory" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .missing_files_directory = .{
            .platform_path = "test/platform/main.roc",
            .files_dir = "targets/",
        },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("MISSING FILES DIRECTORY", report.title);
    try std.testing.expectEqual(Severity.runtime_error, report.severity);
}

test "createValidationReport generates correct report for missing_target_file" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .missing_target_file = .{
            .target = .x64linux,
            .link_type = .exe,
            .file_path = "host.o",
            .expected_full_path = "targets/x64linux/host.o",
        },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("MISSING TARGET FILE", report.title);
    try std.testing.expectEqual(Severity.runtime_error, report.severity);
}

test "createValidationReport generates correct report for extra_file" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .extra_file = .{
            .target = .x64linux,
            .file_path = "unused.o",
        },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("EXTRA FILE IN TARGETS", report.title);
    try std.testing.expectEqual(Severity.warning, report.severity);
}

test "createValidationReport generates correct report for empty_targets" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .empty_targets = .{ .platform_path = "test/platform/main.roc" },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("EMPTY TARGETS SECTION", report.title);
    try std.testing.expectEqual(Severity.runtime_error, report.severity);
}

test "validateTargetFilesExist returns valid when no files_dir specified" {
    const allocator = std.testing.allocator;

    const config = TargetsConfig{
        .files_dir = null,
        .exe = &.{},
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    const result = try validateTargetFilesExist(allocator, config, std.fs.cwd());
    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets detects missing targets section" {
    const allocator = std.testing.allocator;
    const base = @import("base");

    // Platform without targets section
    const source =
        \\platform ""
        \\    requires {} { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { main_for_host: "main" }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    var ast = try parse.parse(&env, allocator);
    defer ast.deinit(allocator);

    const result = validatePlatformHasTargets(allocator, ast, "test/platform/main.roc");

    switch (result) {
        .missing_targets_section => |info| {
            try std.testing.expectEqualStrings("test/platform/main.roc", info.platform_path);
        },
        else => {
            std.debug.print("Expected missing_targets_section but got {}\n", .{result});
            return error.UnexpectedResult;
        },
    }
}

test "validatePlatformHasTargets accepts platform with targets section" {
    const allocator = std.testing.allocator;
    const base = @import("base");

    // Platform with targets section
    const source =
        \\platform ""
        \\    requires {} { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { main_for_host: "main" }
        \\    targets: {
        \\        exe: {
        \\            x64linux: [app],
        \\            arm64linux: [app],
        \\        }
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    var ast = try parse.parse(&env, allocator);
    defer ast.deinit(allocator);

    const result = validatePlatformHasTargets(allocator, ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets skips non-platform headers" {
    const allocator = std.testing.allocator;
    const base = @import("base");

    // App module (not a platform)
    const source =
        \\app [main] { pf: platform "some-platform" }
        \\
        \\main = {}
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    var ast = try parse.parse(&env, allocator);
    defer ast.deinit(allocator);

    const result = validatePlatformHasTargets(allocator, ast, "app/main.roc");

    // Non-platform headers should return valid (they don't need targets)
    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets accepts platform with multiple target types" {
    const allocator = std.testing.allocator;
    const base = @import("base");

    // Platform with exe and static_lib targets
    const source =
        \\platform ""
        \\    requires {} { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { main_for_host: "main" }
        \\    targets: {
        \\        files: "targets/",
        \\        exe: {
        \\            x64linux: ["host.o", app],
        \\            arm64mac: [app],
        \\        },
        \\        static_lib: {
        \\            x64mac: ["libhost.a"],
        \\        }
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    var ast = try parse.parse(&env, allocator);
    defer ast.deinit(allocator);

    const result = validatePlatformHasTargets(allocator, ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets accepts platform with win_gui target" {
    const allocator = std.testing.allocator;
    const base = @import("base");

    // Platform with win_gui special identifier
    const source =
        \\platform ""
        \\    requires {} { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { main_for_host: "main" }
        \\    targets: {
        \\        exe: {
        \\            x64win: [win_gui],
        \\        }
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    var ast = try parse.parse(&env, allocator);
    defer ast.deinit(allocator);

    const result = validatePlatformHasTargets(allocator, ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "TargetsConfig.fromAST extracts targets configuration" {
    const allocator = std.testing.allocator;
    const base = @import("base");

    // Platform with various targets
    const source =
        \\platform ""
        \\    requires {} { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { main_for_host: "main" }
        \\    targets: {
        \\        files: "targets/",
        \\        exe: {
        \\            x64linux: ["host.o", app],
        \\            arm64linux: [app],
        \\        }
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    var ast = try parse.parse(&env, allocator);
    defer ast.deinit(allocator);

    // Try to extract targets config from the AST
    const maybe_config = try TargetsConfig.fromAST(allocator, ast);
    try std.testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer {
        for (config.exe) |spec| {
            allocator.free(spec.items);
        }
        allocator.free(config.exe);
    }

    // Check files_dir
    try std.testing.expect(config.files_dir != null);
    try std.testing.expectEqualStrings("targets/", config.files_dir.?);

    // Check exe targets
    try std.testing.expectEqual(@as(usize, 2), config.exe.len);
}

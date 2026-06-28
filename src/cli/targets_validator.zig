//! Validation for platform targets section
//!
//! Validates that:
//! - Platform headers have a targets section (required)
//! - Files declared in the targets section exist in the filesystem
//! - Files in the targets directory match what's declared in the targets section
//! - `targets: {}` is accepted for hostless platforms consumed by tools such as `roc glue`
//!
//! This module is shared between bundle and unbundle operations.

const std = @import("std");
const Allocator = std.mem.Allocator;
const parse = @import("parse");
const base = @import("base");
const target_mod = @import("target.zig");
const reporting = @import("reporting");

const RocTarget = target_mod.RocTarget;
const TargetsConfig = target_mod.TargetsConfig;
const TargetLinkSpec = target_mod.TargetLinkSpec;
const OutputKind = target_mod.OutputKind;
const LinkItem = target_mod.LinkItem;
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
        output: OutputKind,
        file_path: []const u8,
        expected_full_path: []const u8,
    },

    /// Extra file found in targets directory that isn't declared
    extra_file: struct {
        target: RocTarget,
        file_path: []const u8,
    },

    /// Targets section exists but has no target entries, so `roc build` and the
    /// default `roc` command cannot produce a host-linked output.
    empty_targets: struct {
        platform_path: []const u8,
    },

    /// Requested target is not supported by this platform
    unsupported_target: struct {
        platform_path: []const u8,
        requested_target: RocTarget,
        supported_targets: []const TargetLinkSpec,
    },

    /// Cross-compilation requested but platform doesn't have host library for target
    missing_cross_compile_host: struct {
        platform_path: []const u8,
        target: RocTarget,
        expected_path: []const u8,
        files_dir: []const u8,
    },

    /// glibc cross-compilation is not supported on non-Linux hosts
    unsupported_glibc_cross: struct {
        target: RocTarget,
        host_os: []const u8,
    },

    /// App file doesn't have a platform
    no_platform_found: struct {
        app_path: []const u8,
    },

    /// Invalid target string provided
    invalid_target: struct {
        target_str: []const u8,
    },

    /// Linker failed to create executable
    linker_failed: struct {
        reason: []const u8,
    },

    /// Linker not available (LLVM not built)
    linker_not_available: void,

    /// Process crashed during execution (Windows)
    process_crashed: struct {
        exit_code: u32,
        is_access_violation: bool,
    },

    /// Process killed by signal (Unix)
    process_signaled: struct {
        signal: u32,
    },
};

/// Validate that a platform has a targets section
pub fn validatePlatformHasTargets(
    ast: anytype,
    platform_path: []const u8,
) ValidationResult {
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
    std_io: std.Io,
    targets_config: TargetsConfig,
    platform_dir: std.Io.Dir,
) Allocator.Error!ValidationResult {
    const files_dir_path = targets_config.inputs_dir orelse return .{ .valid = {} };

    // Check if files directory exists
    var files_dir = platform_dir.openDir(std_io, files_dir_path, .{}) catch {
        return .{ .missing_files_directory = .{
            .platform_path = "platform",
            .files_dir = files_dir_path,
        } };
    };
    defer files_dir.close(std_io);

    for (targets_config.targets) |spec| {
        if (try validateTargetSpec(allocator, std_io, spec, files_dir)) |result| {
            return result;
        }
    }

    return .{ .valid = {} };
}

fn validateTargetSpec(
    allocator: Allocator,
    std_io: std.Io,
    spec: TargetLinkSpec,
    files_dir: std.Io.Dir,
) Allocator.Error!?ValidationResult {
    // Get target subdirectory name
    const target_subdir = @tagName(spec.target);

    // Open target subdirectory
    var target_dir = files_dir.openDir(std_io, target_subdir, .{}) catch {
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
            return .{ .missing_target_file = .{
                .target = spec.target,
                .output = spec.output,
                .file_path = target_subdir,
                .expected_full_path = expected_path,
            } };
        }
        return null;
    };
    defer target_dir.close(std_io);

    // Check each file item exists
    for (spec.items) |item| {
        switch (item) {
            .file_path => |path| {
                // Check if file exists
                target_dir.access(std_io, path, .{}) catch {
                    const expected_path = try std.fmt.allocPrint(allocator, "{s}/{s}/{s}", .{ "targets", target_subdir, path });
                    return .{ .missing_target_file = .{
                        .target = spec.target,
                        .output = spec.output,
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
) Allocator.Error!Report {
    switch (result) {
        .valid => unreachable, // Should not create report for valid result

        .missing_targets_section => |info| {
            var report = try Report.init(allocator, "Missing Targets Section", "Platform headers must include a targets section that specifies which targets this platform supports and what files to link.", .runtime_error);

            try report.document.addText("In ");
            try report.document.addAnnotated(info.platform_path, .emphasized);
            try report.document.addText(", add a targets section like:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addCodeBlock(
                \\    targets: {
                \\        inputs_dir: "targets/",
                \\        x64linux: { inputs: ["host.o", app] },
                \\        arm64linux: { inputs: ["host.o", app] },
                \\        x64mac: { inputs: ["host.o", app] },
                \\        arm64mac: { inputs: ["host.o", app] },
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
            const headline = try std.fmt.allocPrint(allocator, "The targets section specifies files directory {s} but this directory doesn't exist.", .{info.files_dir});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Missing Files Directory", headline, .runtime_error);

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
            const headline = try std.fmt.allocPrint(allocator, "The targets section declares file {s} for target {s} but this file doesn't exist.", .{ info.file_path, @tagName(info.target) });
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Missing Target File", headline, .runtime_error);

            try report.document.addText("Expected file at: ");
            try report.document.addAnnotated(info.expected_full_path, .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Either add the missing file or remove it from the targets section.");
            try report.document.addLineBreak();

            return report;
        },

        .extra_file => |info| {
            const headline = try std.fmt.allocPrint(allocator, "Found file {s} in target directory for {s} but this file isn't declared in the targets section.", .{ info.file_path, @tagName(info.target) });
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Extra File In Targets", headline, .warning);

            try report.document.addText("This file will not be included in the bundle.");
            try report.document.addLineBreak();
            try report.document.addText("Either add it to the targets section or delete it.");
            try report.document.addLineBreak();

            return report;
        },

        .empty_targets => |info| {
            const headline = try std.fmt.allocPrint(allocator, "The targets section in {s} is empty, so this platform does not declare any compiler-built targets.", .{info.platform_path});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Empty Targets Section", headline, .runtime_error);

            try report.document.addText("An empty targets section is only valid for tools that supply their own host, such as `roc glue`.");
            try report.document.addLineBreak();
            try report.document.addText("Add target entries before running `roc build` or `roc` directly with this platform.");
            try report.document.addLineBreak();

            return report;
        },

        .unsupported_target => |info| {
            const headline = try std.fmt.allocPrint(allocator, "The platform at {s} does not support the {s} target.", .{ info.platform_path, @tagName(info.requested_target) });
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Unsupported Target", headline, .runtime_error);

            if (info.supported_targets.len > 0) {
                try report.document.addText("Supported targets:");
                try report.document.addLineBreak();
                for (info.supported_targets) |spec| {
                    try report.document.addText("  - ");
                    try report.document.addAnnotated(@tagName(spec.target), .emphasized);
                    try report.document.addLineBreak();
                }
                try report.document.addLineBreak();
            } else {
                try report.document.addText("This platform has no targets configured.");
                try report.document.addLineBreak();
                try report.document.addLineBreak();
            }

            try report.document.addText("To add support, update the targets section in the platform header.");
            try report.document.addLineBreak();

            return report;
        },

        .missing_cross_compile_host => |info| {
            const headline = try std.fmt.allocPrint(allocator, "Cannot cross-compile for {s}: the platform doesn't provide a pre-built host library for this target.", .{@tagName(info.target)});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Missing Host Library For Cross Compilation", headline, .runtime_error);

            try report.document.addText("Expected host library at:");
            try report.document.addLineBreak();
            try report.document.addText("  ");
            try report.document.addAnnotated(info.expected_path, .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Platform authors: build your host for this target and place it at:");
            try report.document.addLineBreak();
            try report.document.addText("  <platform>/");
            // Trim trailing slash from files_dir for cleaner display
            const trimmed_files_dir = std.mem.trimEnd(u8, info.files_dir, "/");
            try report.document.addAnnotated(trimmed_files_dir, .emphasized);
            try report.document.addText("/");
            try report.document.addAnnotated(@tagName(info.target), .emphasized);
            try report.document.addText("/libhost.a");
            try report.document.addLineBreak();

            return report;
        },

        .unsupported_glibc_cross => |info| {
            const headline = try std.fmt.allocPrint(allocator, "Cross-compilation to glibc targets ({s}) is not supported on {s}.", .{ @tagName(info.target), info.host_os });
            defer allocator.free(headline);
            var report = try Report.init(allocator, "GLIBC Cross Compilation Not Supported", headline, .runtime_error);

            try report.document.addText("glibc targets require dynamic linking with libc symbols that");
            try report.document.addLineBreak();
            try report.document.addText("are only available on Linux.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Use a statically-linked musl target instead:");
            try report.document.addLineBreak();
            try report.document.addText("  ");
            try report.document.addAnnotated("x64musl", .emphasized);
            try report.document.addText(" or ");
            try report.document.addAnnotated("arm64musl", .emphasized);
            try report.document.addLineBreak();

            return report;
        },

        .no_platform_found => |info| {
            const headline = try std.fmt.allocPrint(allocator, "The file {s} doesn't have a platform.", .{info.app_path});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "No Platform Found", headline, .runtime_error);

            try report.document.addText("Every Roc application needs a platform. Add a platform declaration:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addCodeBlock(
                \\app [main!] { pf: platform "../path/to/platform/main.roc" }
            );
            try report.document.addLineBreak();

            return report;
        },

        .invalid_target => |info| {
            const headline = try std.fmt.allocPrint(allocator, "The target {s} is not a valid build target.", .{info.target_str});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Invalid Target", headline, .runtime_error);

            try report.document.addText("Valid targets are:");
            try report.document.addLineBreak();
            try report.document.addText("  x64musl, arm64musl    - Linux (static, portable)");
            try report.document.addLineBreak();
            try report.document.addText("  x64glibc, arm64glibc  - Linux (dynamic, faster)");
            try report.document.addLineBreak();
            try report.document.addText("  x64mac, arm64mac      - macOS");
            try report.document.addLineBreak();
            try report.document.addText("  x64win, arm64win      - Windows");
            try report.document.addLineBreak();

            return report;
        },

        .linker_failed => |info| {
            const headline = try std.fmt.allocPrint(allocator, "Failed to create executable: {s}.", .{info.reason});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Linker Failed", headline, .runtime_error);

            try report.document.addText("This may indicate:");
            try report.document.addLineBreak();
            try report.document.addText("  - Missing platform host library (libhost.a)");
            try report.document.addLineBreak();
            try report.document.addText("  - Incompatible object files for the target");
            try report.document.addLineBreak();
            try report.document.addText("  - Missing system libraries");
            try report.document.addLineBreak();

            return report;
        },

        .linker_not_available => {
            var report = try Report.init(allocator, "Linker Not Available", "The LLD linker is not available.", .runtime_error);

            try report.document.addText("This typically occurs when running a test executable");
            try report.document.addLineBreak();
            try report.document.addText("that was built without LLVM support.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("To fix this, rebuild with LLVM enabled.");
            try report.document.addLineBreak();

            return report;
        },

        .process_crashed => |info| {
            const headline = if (info.is_access_violation)
                try allocator.dupe(u8, "The program crashed with an access violation (segmentation fault).")
            else
                try std.fmt.allocPrint(allocator, "The program crashed with exception code: 0x{X}.", .{info.exit_code});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Process Crashed", headline, .runtime_error);

            try report.document.addText("This is likely a bug in the Roc compiler.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Please report this issue at:");
            try report.document.addLineBreak();
            try report.document.addText("  ");
            try report.document.addAnnotated("https://github.com/roc-lang/roc/issues", .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Include a small reproduction of the code that causes this crash.");
            try report.document.addLineBreak();

            return report;
        },

        .process_signaled => |info| {
            const signal_name: []const u8 = switch (info.signal) {
                11 => "SIGSEGV (Segmentation fault)",
                6 => "SIGABRT (Aborted)",
                9 => "SIGKILL (Killed)",
                8 => "SIGFPE (Floating point exception)",
                4 => "SIGILL (Illegal instruction)",
                7 => "SIGBUS (Bus error)",
                else => "Unknown signal",
            };

            const headline = try std.fmt.allocPrint(allocator, "The program was killed by signal {d}: {s}.", .{ info.signal, signal_name });
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Process Killed By Signal", headline, .runtime_error);

            try report.document.addText("This is likely a bug in the Roc compiler.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Please report this issue at:");
            try report.document.addLineBreak();
            try report.document.addText("  ");
            try report.document.addAnnotated("https://github.com/roc-lang/roc/issues", .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Include a small reproduction of the code that causes this crash.");
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

    try std.testing.expectEqualStrings("Missing Targets Section", report.title);
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

    try std.testing.expectEqualStrings("Missing Files Directory", report.title);
    try std.testing.expectEqual(Severity.runtime_error, report.severity);
}

test "createValidationReport generates correct report for missing_target_file" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .missing_target_file = .{
            .target = .x64linux,
            .output = .exe,
            .file_path = "host.o",
            .expected_full_path = "targets/x64linux/host.o",
        },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("Missing Target File", report.title);
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

    try std.testing.expectEqualStrings("Extra File In Targets", report.title);
    try std.testing.expectEqual(Severity.warning, report.severity);
}

test "createValidationReport generates correct report for hostless platforms" {
    const allocator = std.testing.allocator;

    var report = try createValidationReport(allocator, .{
        .empty_targets = .{ .platform_path = "test/platform/main.roc" },
    });
    defer report.deinit();

    try std.testing.expectEqualStrings("Empty Targets Section", report.title);
    try std.testing.expectEqual(Severity.runtime_error, report.severity);
}

test "validateTargetFilesExist returns valid when no files_dir specified" {
    const allocator = std.testing.allocator;

    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{},
    };

    const result = try validateTargetFilesExist(allocator, std.testing.io, config, std.Io.Dir.cwd());
    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets detects missing targets section" {
    const allocator = std.testing.allocator;

    // Platform without targets section
    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    const result = validatePlatformHasTargets(ast, "test/platform/main.roc");

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

    // Platform with targets section
    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\    targets: {
        \\        x64linux: { inputs: [app] },
        \\        arm64linux: { inputs: [app] },
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    const result = validatePlatformHasTargets(ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets skips non-platform headers" {
    const allocator = std.testing.allocator;

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

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    const result = validatePlatformHasTargets(ast, "app/main.roc");

    // Non-platform headers should return valid (they don't need targets)
    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets accepts platform with multiple target types" {
    const allocator = std.testing.allocator;

    // Platform with mixed output kinds
    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\    targets: {
        \\        inputs_dir: "targets/",
        \\        x64linux: { inputs: ["host.o", app] },
        \\        arm64mac: { inputs: [app] },
        \\        x64mac: { inputs: ["libhost.a", app], output: Shared },
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    const result = validatePlatformHasTargets(ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets accepts hostless platform with empty targets section" {
    const allocator = std.testing.allocator;

    const source =
        \\platform ""
        \\    requires { make_glue : List({}) -> Try(List({}), Str) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_make_glue": make_glue_for_host }
        \\    targets: {}
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    const result = validatePlatformHasTargets(ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "validatePlatformHasTargets accepts platform with win_gui target" {
    const allocator = std.testing.allocator;

    // Platform with win_gui special identifier
    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\    targets: {
        \\        x64win: { inputs: [win_gui] },
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    const result = validatePlatformHasTargets(ast, "test/platform/main.roc");

    try std.testing.expectEqual(ValidationResult{ .valid = {} }, result);
}

test "TargetsConfig.fromAST extracts targets configuration" {
    const allocator = std.testing.allocator;

    // Platform with various targets
    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\    targets: {
        \\        inputs_dir: "targets/",
        \\        x64linux: { inputs: ["host.o", app] },
        \\        arm64linux: { inputs: [app] },
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    // Try to extract targets config from the AST
    const maybe_config = try TargetsConfig.fromAST(allocator, ast);
    try std.testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    // Check inputs_dir
    try std.testing.expect(config.inputs_dir != null);
    try std.testing.expectEqualStrings("targets/", config.inputs_dir.?);

    // Check targets
    try std.testing.expectEqual(@as(usize, 2), config.targets.len);
}

test "validateTargetFilesExist reports missing target file with valid path" {
    const allocator = std.testing.allocator;

    // Create a temporary directory structure
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a files directory but without the expected target subdirectory
    tmp_dir.dir.createDir(std.testing.io, "targets", .default_dir) catch {};

    // Create a config that references a file that doesn't exist
    const items: []const LinkItem = &.{
        .{ .file_path = "host.o" },
        .app,
    };
    const exe_specs: []const TargetLinkSpec = &.{
        .{ .target = .x64mac, .output = .exe, .items = items },
    };

    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = exe_specs,
    };

    // This should return a missing_target_file result with a valid expected_full_path
    const result = try validateTargetFilesExist(allocator, std.testing.io, config, tmp_dir.dir);

    switch (result) {
        .missing_target_file => |info| {
            // The expected_full_path should be a valid string, not garbage
            // If it's garbage due to use-after-free, this will likely fail or crash
            try std.testing.expectEqualStrings("targets/x64mac", info.expected_full_path);
            // Also check that it's still accessible after the function returns
            try std.testing.expect(info.expected_full_path.len > 0);
            // Clean up the allocated path
            allocator.free(info.expected_full_path);
        },
        else => {
            std.debug.print("Expected missing_target_file but got {}\n", .{result});
            return error.UnexpectedResult;
        },
    }
}

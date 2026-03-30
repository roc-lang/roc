//! CLI Problem Types
//!
//! Structured error types for CLI operations. Each variant contains
//! the context needed to generate a helpful error message.
//!
//! Usage:
//!   const problem = CliProblem{ .file_not_found = .{ .path = "app.roc" } };
//!   var report = try problem.toReport(allocator);
//!   defer report.deinit();

const std = @import("std");
const Allocator = std.mem.Allocator;
const reporting = @import("reporting");
const Report = reporting.Report;
const Severity = reporting.Severity;

/// Structured CLI errors with context for helpful error messages
pub const CliProblem = union(enum) {
    // File I/O Problems

    /// File was not found at the specified path
    file_not_found: struct {
        path: []const u8,
        context: FileContext = .source_file,
    },

    /// Failed to read file contents
    file_read_failed: struct {
        path: []const u8,
        err: anyerror,
    },

    /// Failed to write file
    file_write_failed: struct {
        path: []const u8,
        err: anyerror,
    },

    /// Failed to create directory
    directory_create_failed: struct {
        path: []const u8,
        err: anyerror,
    },

    /// Directory does not exist
    directory_not_found: struct {
        path: []const u8,
    },

    /// Failed to create temporary directory
    temp_dir_failed: struct {
        err: anyerror,
    },

    /// Cache directory unavailable
    cache_dir_unavailable: struct {
        reason: []const u8,
    },

    // Platform Problems

    /// App file doesn't specify a platform
    no_platform_found: struct {
        app_path: []const u8,
    },

    /// Platform file not found
    platform_not_found: struct {
        app_path: []const u8,
        platform_path: []const u8,
    },

    /// Platform source file missing
    platform_source_not_found: struct {
        platform_path: []const u8,
        searched_paths: []const []const u8,
    },

    /// Missing required module in platform
    missing_platform_module: struct {
        module_name: []const u8,
        platform_path: []const u8,
    },

    /// Type not exposed by module
    missing_type_in_module: struct {
        module_name: []const u8,
        type_name: []const u8,
    },

    /// Circular dependency in platform modules
    circular_platform_dependency: struct {
        module_chain: []const []const u8,
    },

    /// Platform validation failed (wraps targets_validator result)
    platform_validation_failed: struct {
        message: []const u8,
    },

    /// Absolute path used for platform (not allowed)
    absolute_platform_path: struct {
        platform_spec: []const u8,
    },

    /// Invalid app header - missing platform package declaration
    invalid_app_header: struct {
        app_path: []const u8,
    },

    // Build/Compilation Problems

    /// `roc build` is not supported for headerless apps
    build_not_supported_for_headerless: struct {
        app_path: []const u8,
    },

    /// Compilation produced errors
    compilation_failed: struct {
        path: []const u8,
        error_count: usize,
    },

    /// Linker failed
    linker_failed: struct {
        err: anyerror,
        target: []const u8,
    },

    /// Object compilation failed
    object_compilation_failed: struct {
        path: []const u8,
        err: anyerror,
    },

    /// Shim generation failed
    shim_generation_failed: struct {
        err: anyerror,
    },

    /// Entrypoint extraction failed
    entrypoint_extraction_failed: struct {
        path: []const u8,
        reason: []const u8,
    },

    // URL/Download Problems

    /// Invalid URL format
    invalid_url: struct {
        url: []const u8,
        reason: []const u8,
    },

    /// Download failed
    download_failed: struct {
        url: []const u8,
        err: anyerror,
    },

    /// Package not found in cache
    package_cache_error: struct {
        package: []const u8,
        reason: []const u8,
    },

    // Process/Runtime Problems

    /// Child process failed to spawn
    child_process_spawn_failed: struct {
        command: []const u8,
        err: anyerror,
    },

    /// Child process exited with error
    child_process_failed: struct {
        command: []const u8,
        exit_code: u32,
    },

    /// Child process was signaled
    child_process_signaled: struct {
        command: []const u8,
        signal: u32,
    },

    /// Child process wait failed
    child_process_wait_failed: struct {
        command: []const u8,
        err: anyerror,
    },

    /// Shared memory operation failed
    shared_memory_failed: struct {
        operation: []const u8,
        err: anyerror,
    },

    // Module/Header Problems

    /// Expected app header but found something else
    expected_app_header: struct {
        path: []const u8,
        found: []const u8,
    },

    /// Expected platform string in app header
    expected_platform_string: struct {
        path: []const u8,
    },

    /// Module initialization failed
    module_init_failed: struct {
        path: []const u8,
        err: anyerror,
    },

    /// No exports found in module
    no_exports_found: struct {
        path: []const u8,
    },

    // Methods

    /// Returns the severity level for this problem
    pub fn severity(self: CliProblem) Severity {
        return switch (self) {
            // Fatal errors - cannot continue
            .file_not_found,
            .platform_not_found,
            .no_platform_found,
            .build_not_supported_for_headerless,
            .compilation_failed,
            .linker_failed,
            => .fatal,

            // Runtime errors - operation failed
            .file_read_failed,
            .file_write_failed,
            .directory_create_failed,
            .directory_not_found,
            .temp_dir_failed,
            .cache_dir_unavailable,
            .platform_source_not_found,
            .missing_platform_module,
            .missing_type_in_module,
            .circular_platform_dependency,
            .platform_validation_failed,
            .absolute_platform_path,
            .invalid_app_header,
            .object_compilation_failed,
            .shim_generation_failed,
            .entrypoint_extraction_failed,
            .invalid_url,
            .download_failed,
            .package_cache_error,
            .child_process_spawn_failed,
            .child_process_failed,
            .child_process_signaled,
            .child_process_wait_failed,
            .shared_memory_failed,
            .expected_app_header,
            .expected_platform_string,
            .module_init_failed,
            .no_exports_found,
            => .runtime_error,
        };
    }

    /// Generate a Report from this problem
    pub fn toReport(self: CliProblem, allocator: Allocator) !Report {
        return switch (self) {
            .file_not_found => |info| try createFileNotFoundReport(allocator, info),
            .file_read_failed => |info| try createFileReadFailedReport(allocator, info),
            .file_write_failed => |info| try createFileWriteFailedReport(allocator, info),
            .directory_create_failed => |info| try createDirectoryCreateFailedReport(allocator, info),
            .directory_not_found => |info| try createDirectoryNotFoundReport(allocator, info),
            .temp_dir_failed => |info| try createTempDirFailedReport(allocator, info),
            .cache_dir_unavailable => |info| try createCacheDirUnavailableReport(allocator, info),
            .no_platform_found => |info| try createNoPlatformFoundReport(allocator, info),
            .platform_not_found => |info| try createPlatformNotFoundReport(allocator, info),
            .platform_source_not_found => |info| try createPlatformSourceNotFoundReport(allocator, info),
            .missing_platform_module => |info| try createMissingPlatformModuleReport(allocator, info),
            .missing_type_in_module => |info| try createMissingTypeInModuleReport(allocator, info),
            .circular_platform_dependency => |info| try createCircularPlatformDependencyReport(allocator, info),
            .platform_validation_failed => |info| try createPlatformValidationFailedReport(allocator, info),
            .absolute_platform_path => |info| try createAbsolutePlatformPathReport(allocator, info),
            .invalid_app_header => |info| try createInvalidAppHeaderReport(allocator, info),
            .build_not_supported_for_headerless => |info| try createBuildNotSupportedForHeaderlessReport(allocator, info),
            .compilation_failed => |info| try createCompilationFailedReport(allocator, info),
            .linker_failed => |info| try createLinkerFailedReport(allocator, info),
            .object_compilation_failed => |info| try createObjectCompilationFailedReport(allocator, info),
            .shim_generation_failed => |info| try createShimGenerationFailedReport(allocator, info),
            .entrypoint_extraction_failed => |info| try createEntrypointExtractionFailedReport(allocator, info),
            .invalid_url => |info| try createInvalidUrlReport(allocator, info),
            .download_failed => |info| try createDownloadFailedReport(allocator, info),
            .package_cache_error => |info| try createPackageCacheErrorReport(allocator, info),
            .child_process_spawn_failed => |info| try createChildProcessSpawnFailedReport(allocator, info),
            .child_process_failed => |info| try createChildProcessFailedReport(allocator, info),
            .child_process_signaled => |info| try createChildProcessSignaledReport(allocator, info),
            .child_process_wait_failed => |info| try createChildProcessWaitFailedReport(allocator, info),
            .shared_memory_failed => |info| try createSharedMemoryFailedReport(allocator, info),
            .expected_app_header => |info| try createExpectedAppHeaderReport(allocator, info),
            .expected_platform_string => |info| try createExpectedPlatformStringReport(allocator, info),
            .module_init_failed => |info| try createModuleInitFailedReport(allocator, info),
            .no_exports_found => |info| try createNoExportsFoundReport(allocator, info),
        };
    }
};

/// Context for file operations - helps generate better error messages
pub const FileContext = enum {
    source_file,
    platform_file,
    module_file,
    output_file,
    cache_file,
    target_file,

    pub fn description(self: FileContext) []const u8 {
        return switch (self) {
            .source_file => "source file",
            .platform_file => "platform file",
            .module_file => "module file",
            .output_file => "output file",
            .cache_file => "cache file",
            .target_file => "target file",
        };
    }
};

// Report Generation Functions

fn createFileNotFoundReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "FILE NOT FOUND", .fatal);

    try report.document.addText("I could not find the ");
    try report.document.addText(info.context.description());
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Please check that the path is correct and the file exists.");

    return report;
}

fn createFileReadFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "FILE READ FAILED", .runtime_error);

    try report.document.addText("I could not read the file ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createFileWriteFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "FILE WRITE FAILED", .runtime_error);

    try report.document.addText("I could not write to the file ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createDirectoryCreateFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "DIRECTORY CREATE FAILED", .runtime_error);

    try report.document.addText("I could not create the directory ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createDirectoryNotFoundReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "DIRECTORY NOT FOUND", .runtime_error);

    try report.document.addText("The directory does not exist: ");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    ");
    try report.document.addAnnotated(info.path, .path);

    return report;
}

fn createTempDirFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "TEMPORARY DIRECTORY FAILED", .runtime_error);

    try report.document.addText("I could not create a temporary directory.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createCacheDirUnavailableReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "CACHE DIRECTORY UNAVAILABLE", .runtime_error);

    try report.document.addText("The cache directory is not available.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createNoPlatformFoundReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "NO PLATFORM FOUND", .fatal);

    try report.document.addText("The app file ");
    try report.document.addAnnotated(info.app_path, .path);
    try report.document.addText(" does not specify a platform.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Add a platform to your app header, for example:");
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\app [main] { pf: platform "https://..." }
    );

    return report;
}

fn createPlatformNotFoundReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PLATFORM NOT FOUND", .fatal);

    try report.document.addText("I could not find the platform file:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    ");
    try report.document.addAnnotated(info.platform_path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Please check that the platform path is correct and the file exists.");

    return report;
}

fn createPlatformSourceNotFoundReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PLATFORM SOURCE NOT FOUND", .runtime_error);

    try report.document.addText("Could not find the platform source file.");
    try report.document.addLineBreak();
    try report.document.addText("Platform path: ");
    try report.document.addAnnotated(info.platform_path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Searched in:");
    for (info.searched_paths) |path| {
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(path, .path);
    }

    return report;
}

fn createMissingPlatformModuleReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "MISSING PLATFORM MODULE", .runtime_error);

    try report.document.addText("The platform at ");
    try report.document.addAnnotated(info.platform_path, .path);
    try report.document.addLineBreak();
    try report.document.addText("is missing the required module: ");
    try report.document.addAnnotated(info.module_name, .emphasized);

    return report;
}

fn createMissingTypeInModuleReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "MISSING TYPE IN MODULE", .runtime_error);

    try report.document.addText("Module ");
    try report.document.addAnnotated(info.module_name, .emphasized);
    try report.document.addText(" does not expose a type named ");
    try report.document.addAnnotated(info.type_name, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Platform modules must expose a type with the same name as the module.");

    return report;
}

fn createCircularPlatformDependencyReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "CIRCULAR PLATFORM DEPENDENCY", .runtime_error);

    try report.document.addText("A circular dependency was detected in the platform modules:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    for (info.module_chain) |mod| {
        try report.document.addText("    ");
        try report.document.addAnnotated(mod, .emphasized);
        try report.document.addText(" -> ");
    }
    try report.document.addText("(cycle)");

    return report;
}

fn createPlatformValidationFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PLATFORM VALIDATION FAILED", .runtime_error);

    try report.document.addText(info.message);

    return report;
}

fn createAbsolutePlatformPathReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "ABSOLUTE PLATFORM PATH", .runtime_error);

    try report.document.addText("Absolute paths are not allowed for platform specifications:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    ");
    try report.document.addAnnotated(info.platform_spec, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Tip: Use a relative path like ");
    try report.document.addAnnotated("../path/to/platform", .emphasized);
    try report.document.addText(" or a URL.");

    return report;
}

fn createInvalidAppHeaderReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "INVALID APP HEADER", .runtime_error);

    try report.document.addText("The file ");
    try report.document.addAnnotated(info.app_path, .path);
    try report.document.addText(" does not have a valid app header with a platform declaration.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Expected an app header like:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    app [main] { pf: platform \"...\" }");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("The platform package name (e.g., ");
    try report.document.addAnnotated("pf", .emphasized);
    try report.document.addText(") is used to qualify imports from the package like ");
    try report.document.addAnnotated("pf.Stdout", .emphasized);
    try report.document.addText(".");

    return report;
}

fn createBuildNotSupportedForHeaderlessReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "BUILD NOT SUPPORTED", .fatal);

    try report.document.addText("The file ");
    try report.document.addAnnotated(info.app_path, .path);
    try report.document.addText(" is a headerless app, which uses a simple builtin platform");
    try report.document.addLineBreak();
    try report.document.addText("designed for tutorials and cannot be compiled to a standalone executable.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("To run this file, use:");
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\roc <path>
    );
    try report.document.addLineBreak();
    try report.document.addText("To build a standalone executable, add an app header with a platform:");
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\app [main!] { pf: platform "https://..." }
    );

    return report;
}

fn createCompilationFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "COMPILATION FAILED", .fatal);

    try report.document.addText("Compilation of ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addText(" failed.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();

    var buf: [32]u8 = undefined;
    const count_str = std.fmt.bufPrint(&buf, "{}", .{info.error_count}) catch "?";
    try report.document.addText("Found ");
    try report.document.addAnnotated(count_str, .error_highlight);
    try report.document.addText(" error(s). See above for details.");

    return report;
}

fn createLinkerFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "LINKER FAILED", .fatal);

    try report.document.addText("The linker failed while building for target ");
    try report.document.addAnnotated(info.target, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createObjectCompilationFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "OBJECT COMPILATION FAILED", .runtime_error);

    try report.document.addText("Failed to compile object file for ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createShimGenerationFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "SHIM GENERATION FAILED", .runtime_error);

    try report.document.addText("Failed to generate the platform shim.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createEntrypointExtractionFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "ENTRYPOINT EXTRACTION FAILED", .runtime_error);

    try report.document.addText("Failed to extract entrypoint from ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createInvalidUrlReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "INVALID URL", .runtime_error);

    try report.document.addText("The URL is invalid: ");
    try report.document.addAnnotated(info.url, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createDownloadFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "DOWNLOAD FAILED", .runtime_error);

    try report.document.addText("Failed to download from ");
    try report.document.addAnnotated(info.url, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createPackageCacheErrorReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PACKAGE CACHE ERROR", .runtime_error);

    try report.document.addText("Error with cached package ");
    try report.document.addAnnotated(info.package, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createChildProcessSpawnFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PROCESS SPAWN FAILED", .runtime_error);

    try report.document.addText("Failed to start process: ");
    try report.document.addAnnotated(info.command, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createChildProcessFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PROCESS FAILED", .runtime_error);

    try report.document.addText("Process ");
    try report.document.addAnnotated(info.command, .emphasized);
    try report.document.addText(" exited with code ");

    var buf: [16]u8 = undefined;
    const code_str = std.fmt.bufPrint(&buf, "{}", .{info.exit_code}) catch "?";
    try report.document.addAnnotated(code_str, .error_highlight);

    return report;
}

fn createChildProcessSignaledReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PROCESS SIGNALED", .runtime_error);

    try report.document.addText("Process ");
    try report.document.addAnnotated(info.command, .emphasized);
    try report.document.addText(" was terminated by signal ");

    var buf: [16]u8 = undefined;
    const sig_str = std.fmt.bufPrint(&buf, "{}", .{info.signal}) catch "?";
    try report.document.addAnnotated(sig_str, .error_highlight);

    return report;
}

fn createChildProcessWaitFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "PROCESS WAIT FAILED", .runtime_error);

    try report.document.addText("Failed to wait for process ");
    try report.document.addAnnotated(info.command, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createSharedMemoryFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "SHARED MEMORY FAILED", .runtime_error);

    try report.document.addText("Shared memory operation '");
    try report.document.addText(info.operation);
    try report.document.addText("' failed.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createExpectedAppHeaderReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "EXPECTED APP HEADER", .runtime_error);

    try report.document.addText("Expected an app header in ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addText("but found: ");
    try report.document.addAnnotated(info.found, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("An app header looks like:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\app [main!] { pf: platform "..." }
    );
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Tip: Maybe you wanted to run ");
    try report.document.addAnnotated("roc test", .emphasized);
    try report.document.addText(" or ");
    try report.document.addAnnotated("roc check", .emphasized);
    try report.document.addText("?");

    return report;
}

fn createExpectedPlatformStringReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "EXPECTED PLATFORM STRING", .runtime_error);

    try report.document.addText("Expected a platform string in the app header of ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Example:");
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\app [main] { pf: platform "path/to/platform" }
    );

    return report;
}

fn createModuleInitFailedReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "MODULE INITIALIZATION FAILED", .runtime_error);

    try report.document.addText("Failed to initialize module ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createNoExportsFoundReport(allocator: Allocator, info: anytype) !Report {
    var report = Report.init(allocator, "NO EXPORTS FOUND", .runtime_error);

    try report.document.addText("No exports were found in ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Ensure the module exports at least one definition.");

    return report;
}

// Tests

test "file_not_found generates correct report" {
    const allocator = std.testing.allocator;

    const problem = CliProblem{ .file_not_found = .{
        .path = "app.roc",
        .context = .source_file,
    } };

    var report = try problem.toReport(allocator);
    defer report.deinit();

    try std.testing.expectEqualStrings("FILE NOT FOUND", report.title);
    try std.testing.expectEqual(Severity.fatal, report.severity);
}

test "compilation_failed generates correct report" {
    const allocator = std.testing.allocator;

    const problem = CliProblem{ .compilation_failed = .{
        .path = "app.roc",
        .error_count = 3,
    } };

    var report = try problem.toReport(allocator);
    defer report.deinit();

    try std.testing.expectEqualStrings("COMPILATION FAILED", report.title);
    try std.testing.expectEqual(Severity.fatal, report.severity);
}

test "no_platform_found generates correct report" {
    const allocator = std.testing.allocator;

    const problem = CliProblem{ .no_platform_found = .{
        .app_path = "app.roc",
    } };

    var report = try problem.toReport(allocator);
    defer report.deinit();

    try std.testing.expectEqualStrings("NO PLATFORM FOUND", report.title);
    try std.testing.expectEqual(Severity.fatal, report.severity);
}

test "severity returns correct values" {
    try std.testing.expectEqual(
        Severity.fatal,
        (CliProblem{ .file_not_found = .{ .path = "" } }).severity(),
    );
    try std.testing.expectEqual(
        Severity.runtime_error,
        (CliProblem{ .file_read_failed = .{ .path = "", .err = error.OutOfMemory } }).severity(),
    );
}

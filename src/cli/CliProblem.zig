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
const backend = @import("backend");
const linker = @import("linker.zig");
const lir = @import("lir");
const unbundle = @import("unbundle");

/// Errors that can be attached to structured CLI problem reports.
pub const ReportedError =
    Allocator.Error ||
    backend.wasm.WasmModule.MergeError ||
    backend.wasm.WasmModule.ParseError ||
    backend.wasm.ObjectArchive.ParseError ||
    linker.LinkError ||
    lir.LirImage.ImageError ||
    std.zig.system.DetectError ||
    unbundle.download.DownloadError ||
    std.Io.Dir.AccessError ||
    std.Io.Dir.CreateDirError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.CopyFileError ||
    std.Io.Dir.DeleteFileError ||
    std.Io.Dir.OpenError ||
    std.Io.Dir.ReadFileAllocError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.StatFileError ||
    std.Io.Dir.WriteFileError ||
    std.Io.File.OpenError ||
    std.Io.File.ReadPositionalError ||
    std.Io.File.Reader.Error ||
    std.Io.File.StatError ||
    std.Io.File.SyncError ||
    std.Io.File.Writer.Error ||
    std.process.Child.WaitError ||
    std.process.RunError ||
    std.process.SpawnError ||
    error{
        ArchiveWriteFailed,
        BrokenDocLinks,
        BuiltinsExtractionFailed,
        CheckFailed,
        CliError,
        CompilationFailed,
        ComptimeExhaustiveness,
        Crash,
        DivisionByZero,
        DocsFailed,
        EmptyArchive,
        EndOfStream,
        EntrypointNotFound,
        ExpectErr,
        ExpectedAppHeader,
        ExpectedPlatformString,
        ExpectedString,
        FailedToCreateUniqueTempDir,
        FdConfigFailed,
        FileNotFound,
        FormattingFailed,
        HandleInheritanceFailed,
        HashMismatch,
        Internal,
        InvalidArguments,
        InvalidDependency,
        InvalidFilename,
        InvalidHash,
        InvalidLirImage,
        InvalidMagic,
        InvalidPackageName,
        InvalidPath,
        InvalidTarget,
        InvalidUtf8,
        LLVMCompilationFailed,
        LLVMNotAvailable,
        MissingBundleFiles,
        MissingFilesDirectory,
        MissingTargetFile,
        MissingTargetsSection,
        NativeCompilationFailed,
        NoCacheDir,
        NoPlatformSource,
        NotAnAppHeader,
        PathAlreadyExists,
        PathOutsideWorkspace,
        PlatformNotSupported,
        ProcessCreationFailed,
        ProcessExitCodeFailed,
        ProcessWaitFailed,
        ReadFailed,
        ResolutionFailed,
        RuntimeError,
        TempDirCreation,
        TestsFailed,
        TypeCheckingFailed,
        UnbundleFailed,
        Unexpected,
        UnexpectedResult,
        UnsupportedCrossCompilation,
        UnsupportedHeader,
        UnsupportedLowLevel,
        UnsupportedTarget,
        UnsupportedWatchMode,
        WasmOutputWriteFailed,
    };

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
        err: ReportedError,
    },

    /// Failed to write file
    file_write_failed: struct {
        path: []const u8,
        err: ReportedError,
    },

    /// Failed to create directory
    directory_create_failed: struct {
        path: []const u8,
        err: ReportedError,
    },

    /// Directory does not exist
    directory_not_found: struct {
        path: []const u8,
    },

    /// Failed to create temporary directory
    temp_dir_failed: struct {
        err: ReportedError,
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

    /// The requested optimization level is not implemented for this command
    unsupported_opt_level: struct {
        command: []const u8,
        opt: []const u8,
    },

    /// Compilation produced errors
    compilation_failed: struct {
        path: []const u8,
        error_count: usize,
    },

    /// Linker failed
    linker_failed: struct {
        err: ReportedError,
        target: []const u8,
    },

    /// The platform's host inputs do not define symbols the app references
    missing_host_symbols: struct {
        symbols: []const []const u8,
        target: []const u8,
    },

    /// Object compilation failed
    object_compilation_failed: struct {
        path: []const u8,
        err: ReportedError,
    },

    /// Shim generation failed
    shim_generation_failed: struct {
        err: ReportedError,
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
        err: ReportedError,
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
        err: ReportedError,
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
        err: ReportedError,
    },

    /// Shared memory operation failed
    shared_memory_failed: struct {
        operation: []const u8,
        err: ReportedError,
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
        err: ReportedError,
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
            .unsupported_opt_level,
            .compilation_failed,
            .linker_failed,
            .missing_host_symbols,
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
    pub fn toReport(self: CliProblem, allocator: Allocator) Allocator.Error!Report {
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
            .unsupported_opt_level => |info| try createUnsupportedOptLevelReport(allocator, info),
            .compilation_failed => |info| try createCompilationFailedReport(allocator, info),
            .linker_failed => |info| try createLinkerFailedReport(allocator, info),
            .missing_host_symbols => |info| try createMissingHostSymbolsReport(allocator, info),
            .object_compilation_failed => |info| try createObjectCompilationFailedReport(allocator, info),
            .shim_generation_failed => |info| try createShimGenerationFailedReport(allocator, info),
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

fn createFileNotFoundReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "I could not find the {s}.", .{info.context.description()});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "File Not Found", headline, .fatal);

    try report.document.addText("    ");
    try report.document.addAnnotated(info.path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Please check that the path is correct and the file exists.");

    return report;
}

fn createFileReadFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "I could not read the file {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "File Read Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createFileWriteFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "I could not write to the file {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "File Write Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createDirectoryCreateFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "I could not create the directory {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Directory Create Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createDirectoryNotFoundReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Directory Not Found", "The directory does not exist.", .runtime_error);

    try report.document.addText("    ");
    try report.document.addAnnotated(info.path, .path);

    return report;
}

fn createTempDirFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Temporary Directory Failed", "I could not create a temporary directory.", .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createCacheDirUnavailableReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Cache Directory Unavailable", "The cache directory is not available.", .runtime_error);

    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createNoPlatformFoundReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The app file {s} does not specify a platform.", .{info.app_path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "No Platform Found", headline, .fatal);

    try report.document.addText("Add a platform to your app header, for example:");
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\app [main] { pf: platform "https://..." }
    );

    return report;
}

fn createPlatformNotFoundReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Platform Not Found", "I could not find the platform file.", .fatal);

    try report.document.addText("    ");
    try report.document.addAnnotated(info.platform_path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Please check that the platform path is correct and the file exists.");

    return report;
}

fn createPlatformSourceNotFoundReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Platform Source Not Found", "Could not find the platform source file.", .runtime_error);

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

fn createMissingPlatformModuleReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The platform at {s}.", .{info.platform_path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Missing Platform Module", headline, .runtime_error);

    try report.document.addText("is missing the required module: ");
    try report.document.addAnnotated(info.module_name, .emphasized);

    return report;
}

fn createMissingTypeInModuleReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Module {s} does not expose a type named {s}.", .{ info.module_name, info.type_name });
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Missing Type In Module", headline, .runtime_error);

    try report.document.addText("Platform modules must expose a type with the same name as the module.");

    return report;
}

fn createCircularPlatformDependencyReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Circular Platform Dependency", "A circular dependency was detected in the platform modules.", .runtime_error);

    for (info.module_chain) |mod| {
        try report.document.addText("    ");
        try report.document.addAnnotated(mod, .emphasized);
        try report.document.addText(" -> ");
    }
    try report.document.addText("(cycle)");

    return report;
}

fn createPlatformValidationFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "{s}.", .{info.message});
    defer allocator.free(headline);
    const report = try Report.init(allocator, "Platform Validation Failed", headline, .runtime_error);

    return report;
}

fn createAbsolutePlatformPathReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Absolute Platform Path", "Absolute paths are not allowed for platform specifications.", .runtime_error);

    try report.document.addText("    ");
    try report.document.addAnnotated(info.platform_spec, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Tip: Use a relative path like ");
    try report.document.addAnnotated("../path/to/platform", .emphasized);
    try report.document.addText(" or a URL.");

    return report;
}

fn createInvalidAppHeaderReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The file {s} does not have a valid app header with a platform declaration.", .{info.app_path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Invalid App Header", headline, .runtime_error);

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

fn createBuildNotSupportedForHeaderlessReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The file {s} is a headerless app, which uses a simple builtin platform.", .{info.app_path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Build Not Supported", headline, .fatal);

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

fn createUnsupportedOptLevelReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The optimization mode {s} is not implemented for {s} yet.", .{ info.opt, info.command });
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Optimization Mode Not Implemented", headline, .fatal);

    try report.document.addText("Use ");
    try report.document.addAnnotated("--opt=dev", .emphasized);
    try report.document.addText(" for compiled builds, or ");
    try report.document.addAnnotated("--opt=interpreter", .emphasized);
    try report.document.addText(" for interpreter builds.");

    return report;
}

fn createCompilationFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Compilation of {s} failed.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Compilation Failed", headline, .fatal);

    var buf: [32]u8 = undefined;
    const count_str = std.fmt.bufPrint(&buf, "{}", .{info.error_count}) catch "?";
    try report.document.addText("Found ");
    try report.document.addAnnotated(count_str, .error_highlight);
    try report.document.addText(" error(s). See above for details.");

    return report;
}

fn createMissingHostSymbolsReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The platform's host inputs for target {s} do not define these symbols the application references.", .{info.target});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Missing Host Symbols", headline, .fatal);

    for (info.symbols) |symbol| {
        try report.document.addLineBreak();
        try report.document.addText("    ");
        try report.document.addAnnotated(symbol, .emphasized);
    }
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Every linker symbol named in the platform header's ");
    try report.document.addAnnotated("hosted", .emphasized);
    try report.document.addText(" section, plus the fixed runtime set (roc_alloc, roc_dealloc, roc_realloc, roc_dbg, roc_expect_failed, roc_crashed), must be defined by the host inputs listed in the platform's ");
    try report.document.addAnnotated("targets", .emphasized);
    try report.document.addText(" section.");

    return report;
}

fn createLinkerFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The linker failed while building for target {s}.", .{info.target});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Linker Failed", headline, .fatal);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createObjectCompilationFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Failed to compile object file for {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Object Compilation Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createShimGenerationFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    var report = try Report.init(allocator, "Shim Generation Failed", "Failed to generate the platform shim.", .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createInvalidUrlReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "The URL is invalid: {s}.", .{info.url});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Invalid URL", headline, .runtime_error);

    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createDownloadFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = switch (info.err) {
        error.InvalidHash => try std.fmt.allocPrint(allocator, "Error: {s}.", .{@errorName(info.err)}),
        else => try std.fmt.allocPrint(allocator, "Failed to download from {s}.", .{info.url}),
    };
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Download Failed", headline, .runtime_error);

    switch (info.err) {
        error.InvalidHash => {
            try report.document.addText("The url contains an invalid hash.");
            try report.document.addLineBreaks(2);

            try report.document.addText("Platform Url: ");
            try report.document.addAnnotated(info.url, .emphasized);
            try report.document.addLineBreaks(2);

            try report.document.addText("Possible Reasons: ");
            try report.document.addLineBreak();
            try report.document.addText("1. The ");
            try report.document.addAnnotated("platform was built with the old Roc", .error_highlight);
            try report.document.addText(" (Rust) compiler (alpha4 or older), instead of the new Roc (Zig) compiler. The new compiler is available at https://github.com/roc-lang/nightlies");
            try report.document.addLineBreak();
            try report.document.addText("2. The Hash portion of the URL is malformed.");

            try report.document.addLineBreaks(2);
            try report.document.addText("Tips:");
            try report.document.addLineBreak();
            try report.document.addSuggestion("1. If there is a newer version of the platform available, try updating.");
            try report.document.addLineBreak();
            try report.document.addSuggestion("2. Verify the URL and ensure it matches a valid platform release.");
            try report.document.addLineBreak();
        },
        else => {
            try report.document.addText("Error: ");
            try report.document.addText(@errorName(info.err));
        },
    }

    return report;
}

fn createPackageCacheErrorReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Error with cached package {s}.", .{info.package});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Package Cache Error", headline, .runtime_error);

    try report.document.addText("Reason: ");
    try report.document.addText(info.reason);

    return report;
}

fn createChildProcessSpawnFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Failed to start process: {s}.", .{info.command});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Process Spawn Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createChildProcessFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Process {s} exited with code {d}.", .{ info.command, info.exit_code });
    defer allocator.free(headline);
    const report = try Report.init(allocator, "Process Failed", headline, .runtime_error);

    return report;
}

fn createChildProcessSignaledReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Process {s} was terminated by signal {d}.", .{ info.command, info.signal });
    defer allocator.free(headline);
    const report = try Report.init(allocator, "Process Signaled", headline, .runtime_error);

    return report;
}

fn createChildProcessWaitFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Failed to wait for process {s}.", .{info.command});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Process Wait Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createSharedMemoryFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Shared memory operation '{s}' failed.", .{info.operation});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Shared Memory Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createExpectedAppHeaderReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Expected an app header in {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Expected App Header", headline, .runtime_error);

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

fn createExpectedPlatformStringReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Expected a platform string in the app header of {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Expected Platform String", headline, .runtime_error);

    try report.document.addText("Example:");
    try report.document.addLineBreak();
    try report.document.addCodeBlock(
        \\app [main] { pf: platform "path/to/platform" }
    );

    return report;
}

fn createModuleInitFailedReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "Failed to initialize module {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "Module Initialization Failed", headline, .runtime_error);

    try report.document.addText("Error: ");
    try report.document.addText(@errorName(info.err));

    return report;
}

fn createNoExportsFoundReport(allocator: Allocator, info: anytype) Allocator.Error!Report {
    const headline = try std.fmt.allocPrint(allocator, "No exports were found in {s}.", .{info.path});
    defer allocator.free(headline);
    var report = try Report.init(allocator, "No Exports Found", headline, .runtime_error);

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

    try std.testing.expectEqualStrings("File Not Found", report.title);
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

    try std.testing.expectEqualStrings("Compilation Failed", report.title);
    try std.testing.expectEqual(Severity.fatal, report.severity);
}

test "unsupported_opt_level generates correct report" {
    const allocator = std.testing.allocator;

    const problem = CliProblem{ .unsupported_opt_level = .{
        .command = "roc build",
        .opt = "size",
    } };

    var report = try problem.toReport(allocator);
    defer report.deinit();

    try std.testing.expectEqualStrings("Optimization Mode Not Implemented", report.title);
    try std.testing.expectEqual(Severity.fatal, report.severity);
}

test "no_platform_found generates correct report" {
    const allocator = std.testing.allocator;

    const problem = CliProblem{ .no_platform_found = .{
        .app_path = "app.roc",
    } };

    var report = try problem.toReport(allocator);
    defer report.deinit();

    try std.testing.expectEqualStrings("No Platform Found", report.title);
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

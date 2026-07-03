//! Parallel CLI integration test runner.
//!
//! Runs platform fixtures and command-style CLI suites using a fork-based
//! process pool (via src/build/test_harness.zig).
//!
//! Usage:
//!   parallel_cli_runner <roc_binary> [options]
//!
//! Options:
//!   --suite <name>      Run suite: platforms, subcommands, echo, glue, or all (repeatable)
//!   --filter <pattern>   Run only tests whose name contains <pattern> (repeatable)
//!   --threads <N>        Max concurrent child processes (default: CPU count)
//!   --timeout <ms>       Per-test timeout in ms (default: 120000, 240000 with glue)
//!   --include-llvm       Include size and speed LLVM backend jobs
//!   --glue-roc <path>    Roc binary to use for glue generation (default: <roc_binary>)
//!   --glue-opt <opt>     Glue execution mode; supported value: interpreter
//!   --glue-full-targets  Run opt-in non-default glue compile targets
//!   --verbose            Print PASS results and timing details

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;
const Allocator = std.mem.Allocator;

const harness = @import("test_harness");
const platform_config = @import("platform_config.zig");
const util = @import("util.zig");
const collections = @import("collections");

const child_command_timeout_reserve_ms: u64 = 1_000;
const timeout_result_grace_ms: u64 = 5_000;
const default_timeout_ms: u64 = 120_000;
const glue_timeout_ms: u64 = 240_000;

const CliRunnerError = util.RocRunError ||
    util.ChildTimeoutError ||
    util.TestDirError ||
    std.mem.Allocator.Error ||
    std.process.Args.ToSliceError ||
    std.process.ExecutablePathError ||
    std.Io.Dir.AccessError ||
    std.Io.Dir.CopyFileError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.DeleteFileError ||
    std.Io.Dir.OpenError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.SelectiveWalker.Error ||
    std.Io.Dir.StatFileError ||
    std.Io.Dir.WriteFileError ||
    std.Io.File.OpenError ||
    std.Io.File.Reader.Error ||
    std.Io.File.Writer.Error ||
    error{
        InvalidArgs,
        InvalidGeneratedGraphConfig,
    };

// Test spec types

const Suite = enum(u8) {
    platforms,
    subcommands,
    echo,
    glue,

    fn cliName(self: Suite) []const u8 {
        return switch (self) {
            .platforms => "platforms",
            .subcommands => "subcommands",
            .echo => "echo",
            .glue => "glue",
        };
    }

    fn displayName(self: Suite) []const u8 {
        return switch (self) {
            .platforms => "platforms",
            .subcommands => "subcommands",
            .echo => "echo",
            .glue => "glue",
        };
    }
};

const suite_count = @typeInfo(Suite).@"enum".fields.len;
const all_suites = [_]Suite{ .platforms, .subcommands, .echo, .glue };

const SuiteSelection = struct {
    enabled: [suite_count]bool = [_]bool{false} ** suite_count,

    fn all() SuiteSelection {
        var result = SuiteSelection{};
        for (&result.enabled) |*slot| slot.* = true;
        return result;
    }

    fn add(self: *SuiteSelection, suite: Suite) void {
        self.enabled[@intFromEnum(suite)] = true;
    }

    fn addAll(self: *SuiteSelection) void {
        self.* = SuiteSelection.all();
    }

    fn includes(self: SuiteSelection, suite: Suite) bool {
        return self.enabled[@intFromEnum(suite)];
    }

    fn isEmpty(self: SuiteSelection) bool {
        for (self.enabled) |enabled| {
            if (enabled) return false;
        }
        return true;
    }
};

const OptMode = enum(u8) {
    interpreter,
    dev,
    size,
    speed,

    fn cliName(self: OptMode) []const u8 {
        return switch (self) {
            .interpreter => "interpreter",
            .dev => "dev",
            .size => "size",
            .speed => "speed",
        };
    }
};

const base_test_opts = [_]OptMode{ .interpreter, .dev };
const llvm_test_opts = [_]OptMode{ .size, .speed };

const GlueExecutionMode = enum(u8) {
    default,
    interpreter,

    fn cliName(self: GlueExecutionMode) []const u8 {
        return switch (self) {
            .default => "default",
            .interpreter => "interpreter",
        };
    }

    fn optArg(self: GlueExecutionMode) ?[]const u8 {
        return switch (self) {
            .default => null,
            .interpreter => "--opt=interpreter",
        };
    }
};

const GlueLanguage = enum(u8) {
    zig,
    rust,
    c,

    fn displayName(self: GlueLanguage) []const u8 {
        return switch (self) {
            .zig => "ZigGlue",
            .rust => "RustGlue",
            .c => "CGlue",
        };
    }

    fn glueSpec(self: GlueLanguage) []const u8 {
        return switch (self) {
            .zig => "src/glue/src/ZigGlue.roc",
            .rust => "src/glue/src/RustGlue.roc",
            .c => "src/glue/src/CGlue.roc",
        };
    }

    fn generatedFileName(self: GlueLanguage) []const u8 {
        return switch (self) {
            .zig => "roc_platform_abi.zig",
            .rust => "roc_platform_abi.rs",
            .c => "roc_platform_abi.h",
        };
    }
};

const GlueTarget = enum(u8) {
    native,
    wasm32,
    x86_64_linux_musl,
    aarch64_linux_musl,
    x86_64_macos,
    aarch64_macos,
    x86_64_windows,
    aarch64_windows,

    fn displayName(self: GlueTarget) []const u8 {
        return switch (self) {
            .native => "native",
            .wasm32 => "wasm32",
            .x86_64_linux_musl => "x86_64-linux-musl",
            .aarch64_linux_musl => "aarch64-linux-musl",
            .x86_64_macos => "x86_64-macos",
            .aarch64_macos => "aarch64-macos",
            .x86_64_windows => "x86_64-windows",
            .aarch64_windows => "aarch64-windows",
        };
    }

    fn zigTargetArg(self: GlueTarget) ?[]const u8 {
        return switch (self) {
            .native => null,
            .wasm32 => "wasm32-freestanding-none",
            .x86_64_linux_musl => "x86_64-linux-musl",
            .aarch64_linux_musl => "aarch64-linux-musl",
            .x86_64_macos => "x86_64-macos",
            .aarch64_macos => "aarch64-macos",
            .x86_64_windows => "x86_64-windows",
            .aarch64_windows => "aarch64-windows",
        };
    }
};

const GlueRunnerOptions = struct {
    execution_mode: GlueExecutionMode = .default,
    full_targets: bool = false,
};

const GluePlatformShapeFixture = struct {
    name: []const u8,
    platform_path: []const u8,
};

const glue_platform_shape_fixtures = [_]GluePlatformShapeFixture{
    .{ .name = "cli-main", .platform_path = "test/glue/platform-shapes/cli-main/main.roc" },
    .{ .name = "app-model", .platform_path = "test/glue/platform-shapes/app-model/main.roc" },
    .{ .name = "type-catalog", .platform_path = "test/glue/platform-shapes/type-catalog/main.roc" },
};

const default_zig_glue_targets = [_]GlueTarget{ .native, .wasm32 };
const default_c_glue_targets = [_]GlueTarget{ .native, .wasm32 };
const default_rust_glue_targets = [_]GlueTarget{.native};

const full_zig_glue_targets = [_]GlueTarget{
    .native,
    .wasm32,
    .x86_64_linux_musl,
    .aarch64_linux_musl,
    .x86_64_macos,
    .aarch64_macos,
    .x86_64_windows,
    .aarch64_windows,
};
const full_c_glue_targets = full_zig_glue_targets;

const Stream = enum {
    stdout,
    stderr,
};

const OutputNeedle = struct {
    stream: Stream,
    text: []const u8,
};

const OutputOccurrence = struct {
    stream: Stream,
    text: []const u8,
    count: usize,
};

const OutputNeedleSet = struct {
    needles: []const OutputNeedle,
};

const ExitExpectation = union(enum) {
    success,
    failure,
    code: u32,
    not_panic,
    any,
};

const FilePathMode = enum {
    absolute,
    relative,
};

const CommandCase = struct {
    args: []const []const u8,
    roc_file: ?[]const u8 = null,
    file_path_mode: FilePathMode = .absolute,
    stdin: ?[]const u8 = null,
    exit: ExitExpectation = .success,
    stdout_exact: ?[]const u8 = null,
    stderr_exact: ?[]const u8 = null,
    stdout_min_len: ?usize = null,
    stderr_min_len: ?usize = null,
    contains: []const OutputNeedle = &.{},
    not_contains: []const OutputNeedle = &.{},
    occurrences: []const OutputOccurrence = &.{},
    contains_any: []const OutputNeedleSet = &.{},
};

const PlatformCase = struct {
    /// Path to .roc file (relative to project root)
    roc_file: []const u8,
    /// Platform name (for display grouping)
    platform: []const u8,
    /// Stderr substrings expected during optimized build steps. A build that
    /// exits 2 because of expected warning diagnostics still produces an
    /// executable.
    expected_build_stderr_contains: []const []const u8 = &.{},
    /// What kind of test to run
    test_kind: TestKind,

    const TestKind = union(enum) {
        /// Build natively and run; check exit code 0
        native_run,
        /// Build natively, run with --test <spec>; check exit code 0
        io_spec: []const u8,
    };
};

const GlueMatrixCase = struct {
    language: GlueLanguage,
    fixture: GluePlatformShapeFixture,
    target: GlueTarget,
    execution_mode: GlueExecutionMode,
};

const CustomCase = enum {
    noop,
    default_app_all_syntax_checked_cache,
    cli_cache_roots_distinct,
    watch_inputs_reject_absolute_import,
    watch_completed_run_refresh_reruns,
    hot_reload_dev_shim,
    hot_reload_model_boundary,
    hot_reload_default_app,
    platform_requires_checker_diagnostics,
    generated_graph_1_1,
    generated_graph_5_5,
    generated_graph_2_100,
    generated_graph_200_5,
    list_builtin_inlined,
    default_platform_linux_disassembly,
    default_platform_build_x64glibc,
    default_platform_build_arm64glibc,
    default_platform_build_wasm32,
    default_platform_wasm32_archive_reproducible,
    macos_output_basename_reproducible,
    default_platform_crash_x64musl,
    default_platform_crash_arm64musl,
    default_platform_crash_x64mac,
    default_platform_crash_arm64mac,
    default_platform_crash_x64win,
    default_platform_crash_arm64win,
    default_platform_stack_overflow_x64musl,
    default_platform_stack_overflow_arm64musl,
    default_platform_stack_overflow_x64mac,
    default_platform_stack_overflow_arm64mac,
    default_platform_stack_overflow_x64win,
    default_platform_stack_overflow_arm64win,
    fmt_reformats_file,
    fmt_does_not_change_file,
    fmt_stdin_formats,
    fmt_stdin_does_not_change,
    build_int_interpreter_creates_output,
    build_int_interpreter_output_runs,
    build_int_dev_output_runs,
    build_glibc_target_non_linux_error,
    build_windows_shared_library,
    cache_passing_results,
    cache_failing_results,
    cache_invalidated_by_source_change,
    verbose_works_from_cache,
    verbose_caches_failure_reports,
    non_verbose_caches_verbose_reports,
    verbose_and_non_verbose_failure_format_match,
    build_warning_interpreter,
    issue_9392_deterministic_no_cache,
    build_issue_9435_hosted_nominal_return,
    bundle_complex_package,
    glue_debug,
    glue_debug_interpreter,
    glue_c_header,
    glue_c_header_compiles,
    glue_zig,
    glue_zig_compiles,
    glue_zig_native_wasm_layouts,
    glue_zig_opaque_box,
    glue_zig_box_payload_alignment,
    glue_rust,
    glue_zig_duplicate_tag_unions,
    glue_rust_duplicate_tag_unions,
    glue_rust_box_payload_alignment,
    glue_zig_bang_record_fields,
    glue_package_nominal_api_alias,
    glue_c_tests,
};

const Skip = union(enum) {
    never,
    always: []const u8,
    windows: []const u8,
};

/// A single CLI test operation — one matrix cell of work.
const CliCase = struct {
    /// Unique id within this runner invocation. This keeps generated binary
    /// names distinct even on hosts that run all specs in the same process.
    id: usize,
    suite: Suite,
    /// Human-readable name, e.g. "test/fx/hello_world.roc [dev]"
    name: []const u8,
    /// Execution mode when the case has one.
    backend: ?OptMode = null,
    skip: Skip = .never,
    body: Body,

    const Body = union(enum) {
        platform: PlatformCase,
        command: CommandCase,
        custom: CustomCase,
        glue_matrix: GlueMatrixCase,
    };
};

// Spec generation

fn buildCases(
    allocator: Allocator,
    filters: []const []const u8,
    include_llvm: bool,
    suites: SuiteSelection,
    glue_options: GlueRunnerOptions,
) CliRunnerError![]const CliCase {
    var cases: std.ArrayListUnmanaged(CliCase) = .empty;

    if (suites.includes(.platforms)) {
        for (&platform_config.platforms) |platform| {
            for (&base_test_opts) |opt| {
                try appendPlatformSpecs(allocator, &cases, platform, opt, filters);
            }
            if (include_llvm) {
                for (&llvm_test_opts) |opt| {
                    try appendPlatformSpecs(allocator, &cases, platform, opt, filters);
                }
            }
        }
    }

    if (suites.includes(.echo)) {
        try appendStaticCases(allocator, &cases, &echo_cases, filters);
    }
    if (suites.includes(.glue)) {
        try appendStaticCases(allocator, &cases, &glue_cases, filters);
        try appendGlueMatrixCases(allocator, &cases, filters, glue_options);
    }
    if (suites.includes(.subcommands)) {
        try appendStaticCases(allocator, &cases, &subcommand_cases, filters);
    }

    return try cases.toOwnedSlice(allocator);
}

fn appendGlueMatrixCases(
    allocator: Allocator,
    cases: *std.ArrayListUnmanaged(CliCase),
    filters: []const []const u8,
    glue_options: GlueRunnerOptions,
) CliRunnerError!void {
    const zig_targets = if (glue_options.full_targets) full_zig_glue_targets[0..] else default_zig_glue_targets[0..];
    const c_targets = if (glue_options.full_targets) full_c_glue_targets[0..] else default_c_glue_targets[0..];

    for (glue_platform_shape_fixtures) |fixture| {
        try appendGlueLanguageMatrixCases(allocator, cases, filters, glue_options, .zig, fixture, zig_targets);
        try appendGlueLanguageMatrixCases(allocator, cases, filters, glue_options, .rust, fixture, default_rust_glue_targets[0..]);
        try appendGlueLanguageMatrixCases(allocator, cases, filters, glue_options, .c, fixture, c_targets);
    }
}

fn appendGlueLanguageMatrixCases(
    allocator: Allocator,
    cases: *std.ArrayListUnmanaged(CliCase),
    filters: []const []const u8,
    glue_options: GlueRunnerOptions,
    language: GlueLanguage,
    fixture: GluePlatformShapeFixture,
    targets: []const GlueTarget,
) CliRunnerError!void {
    for (targets) |target| {
        const name = try std.fmt.allocPrint(
            allocator,
            "glue matrix: {s} {s} [{s}, glue-opt={s}]",
            .{ language.displayName(), fixture.name, target.displayName(), glue_options.execution_mode.cliName() },
        );
        const case = CliCase{
            .id = cases.items.len,
            .suite = .glue,
            .name = name,
            .body = .{ .glue_matrix = .{
                .language = language,
                .fixture = fixture,
                .target = target,
                .execution_mode = glue_options.execution_mode,
            } },
        };
        if (matchesFilters(case, filters)) {
            try cases.append(allocator, case);
        }
    }
}

fn appendStaticCases(
    allocator: Allocator,
    cases: *std.ArrayListUnmanaged(CliCase),
    source: []const CliCase,
    filters: []const []const u8,
) CliRunnerError!void {
    for (source) |source_case| {
        if (!matchesFilters(source_case, filters)) continue;
        var case = source_case;
        case.id = cases.items.len;
        try cases.append(allocator, case);
    }
}

fn appendPlatformSpecs(
    allocator: Allocator,
    cases: *std.ArrayListUnmanaged(CliCase),
    platform: platform_config.PlatformConfig,
    opt: OptMode,
    filters: []const []const u8,
) CliRunnerError!void {
    switch (platform.test_apps) {
        .single => |app_name| {
            const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
            const name = try fmtTestName(allocator, roc_file, opt);
            const case = CliCase{
                .id = cases.items.len,
                .suite = .platforms,
                .name = name,
                .backend = opt,
                .body = .{ .platform = .{
                    .roc_file = roc_file,
                    .platform = platform.name,
                    .test_kind = .native_run,
                } },
            };
            if (matchesFilters(case, filters)) {
                try cases.append(allocator, case);
            }
        },
        .spec_list => |io_specs| {
            for (io_specs) |spec| {
                if (skipIoSpecOnHost(spec)) continue;

                const name = try fmtTestName(allocator, spec.roc_file, opt);
                const case = CliCase{
                    .id = cases.items.len,
                    .suite = .platforms,
                    .name = name,
                    .backend = opt,
                    .body = .{ .platform = .{
                        .roc_file = spec.roc_file,
                        .platform = platform.name,
                        .expected_build_stderr_contains = spec.expected_build_stderr_contains,
                        .test_kind = .{ .io_spec = spec.io_spec },
                    } },
                };
                if (matchesFilters(case, filters)) {
                    try cases.append(allocator, case);
                }
            }
        },
        .simple_list => |simple_specs| {
            for (simple_specs) |spec| {
                const name = try fmtTestName(allocator, spec.roc_file, opt);
                const case = CliCase{
                    .id = cases.items.len,
                    .suite = .platforms,
                    .name = name,
                    .backend = opt,
                    .body = .{ .platform = .{
                        .roc_file = spec.roc_file,
                        .platform = platform.name,
                        .test_kind = .native_run,
                    } },
                };
                if (matchesFilters(case, filters)) {
                    try cases.append(allocator, case);
                }
            }
        },
    }
}

fn skipIoSpecOnHost(spec: @import("fx_test_specs.zig").TestSpec) bool {
    if (spec.skip) return true;
    return spec.skip_on_windows and builtin.os.tag == .windows;
}

fn fmtTestName(allocator: Allocator, roc_file: []const u8, opt: OptMode) CliRunnerError![]const u8 {
    return std.fmt.allocPrint(allocator, "{s} [{s}]", .{ roc_file, opt.cliName() });
}

fn caseRocFile(case: CliCase) ?[]const u8 {
    return switch (case.body) {
        .platform => |platform| platform.roc_file,
        .command => |command| command.roc_file,
        .custom => null,
        .glue_matrix => |matrix| matrix.fixture.platform_path,
    };
}

fn matchesFilters(case: CliCase, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |f| {
        if (std.mem.find(u8, case.name, f) != null) return true;
        if (std.mem.find(u8, case.suite.cliName(), f) != null) return true;
        if (case.backend) |backend| {
            if (std.mem.find(u8, backend.cliName(), f) != null) return true;
        }
        if (caseRocFile(case)) |roc_file| {
            if (std.mem.find(u8, roc_file, f) != null) return true;
        }
    }
    return false;
}

// Echo suite cases

// The Unicode escape sequence line contains a literal NBSP before the newline.
const all_syntax_expected_stdout =
    \\Hello, world!
    \\Hello, world! (using alias)
    \\{ diff: 5, div: 2, div_trunc: 2, eq: False, gt: True, gteq: True, lt: False, lteq: False, neg: -10, neq: True, prod: 50, rem: 0, sum: 15 }
    \\{ bool_and_keyword: False, bool_or_keyword: True, not_a: False }
    \\"One Two"
    \\"Three Four"
    \\The color is red.
    \\78
    \\Success
    \\Line 1
    \\Line 2
    \\Line 3
    \\Unicode escape sequence:  
    \\This is an effectful function!
    \\Ok(1)
    \\Err(NoFirstError(ListWasEmpty))
    \\Err(NoFirstError(ListWasEmpty))
    \\15.0
    \\False
    \\10.0
    \\42.0
    \\NotOneTwoNotFive
    \\("Roc", 1.0)
    \\["a", "b"]
    \\("Roc", 1.0, 1.0, 1.0)
    \\10.0
    \\{ age: 31, name: "Alice" }
    \\{ binary: 5.0, explicit_i128: 5, explicit_i16: 5, explicit_i32: 5, explicit_i64: 5, explicit_i8: 5, explicit_u128: 5, explicit_u16: 5, explicit_u32: 5, explicit_u64: 5, explicit_u8: 5, hex: 5.0, octal: 5.0, usage_based: 5.0 }
    \\<opaque>
    \\"The secret key is: my_secret_key"
    \\False
    \\99
    \\"12345.0"
    \\"Foo with 42 and hello"
    \\"other color"
    \\"Names: Alice, Bob, Charlie"
    \\"A"
    \\"other letter"
    \\True
    \\
;

const all_syntax_expected_stderr =
    \\[dbg] 42.0
    \\
;

const echo_cases = [_]CliCase{
    .{ .id = 0, .suite = .echo, .name = "echo platform: hello (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/hello.roc", .stdout_exact = "Hello, World!\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: hello (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/hello.roc", .stdout_exact = "Hello, World!\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: multiple echo calls (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/multi.roc", .stdout_exact = "Hello, \nWorld!\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: multiple echo calls (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/multi.roc", .stdout_exact = "Hello, \nWorld!\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: exit ok (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/exit_ok.roc", .stdout_exact = "success\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: exit ok (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/exit_ok.roc", .stdout_exact = "success\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: exit code (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/exit_code.roc", .exit = .{ .code = 255 } } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: exit code (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/exit_code.roc", .exit = .{ .code = 255 } } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: custom error issue 9255 repro (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/exit_custom_error.roc", .exit = .{ .code = 1 }, .stdout_exact = "Program exited with error: SomeCustomError(41.0)\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: list concat with refcounted elements issue 9316 (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/issue_9316.roc", .stdout_exact = "[\"BAZ\", \"DUCK\", \"XYZ\", \"ABC\"]\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: list concat with refcounted elements issue 9316 (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/issue_9316.roc", .stdout_exact = "[\"BAZ\", \"DUCK\", \"XYZ\", \"ABC\"]\n" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: cmd-test OOM repro compiles and runs (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/repro_oom_cmd_test.roc", .stdout_exact = "" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: cmd-test OOM repro compiles and runs (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/repro_oom_cmd_test.roc", .stdout_exact = "" } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: no main is not a default app (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/no_main.roc", .exit = .failure } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: no main is not a default app (dev)", .backend = .dev, .skip = .{ .always = "TODO: dev backend crashes test runner" }, .body = .{ .custom = .noop } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: all_syntax_test.roc prints expected output (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/echo/all_syntax_test.roc", .stdout_exact = all_syntax_expected_stdout, .stderr_exact = all_syntax_expected_stderr } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: all_syntax_test.roc run populates checked module cache", .backend = .interpreter, .body = .{ .custom = .default_app_all_syntax_checked_cache } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: all_syntax_test.roc prints expected output (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not preserve the original source directory" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/all_syntax_test.roc", .stdout_exact = all_syntax_expected_stdout, .stderr_exact = all_syntax_expected_stderr } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: roc test all_syntax_test.roc passes", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/echo/all_syntax_test.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: statically dispatched, propagated, open error union does not crash (regression test #9588)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/echo/issue_9588.roc", .exit = .success, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "invariant violated" } } } } },
};

// Glue suite cases

const glue_cases = [_]CliCase{
    .{ .id = 0, .suite = .glue, .name = "glue command with DebugGlue succeeds", .body = .{ .custom = .glue_debug } },
    .{ .id = 0, .suite = .glue, .name = "glue command with DebugGlue succeeds with --opt=interpreter", .body = .{ .custom = .glue_debug_interpreter } },
    .{ .id = 0, .suite = .glue, .name = "glue command with CGlue generates expected C header", .body = .{ .custom = .glue_c_header } },
    .{ .id = 0, .suite = .glue, .name = "glue command generated C header compiles with zig cc", .body = .{ .custom = .glue_c_header_compiles } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue succeeds on fx platform", .body = .{ .custom = .glue_zig } },
    .{ .id = 0, .suite = .glue, .name = "glue command generated Zig compiles with zig build-obj", .body = .{ .custom = .glue_zig_compiles } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue native and wasm layouts compile", .body = .{ .custom = .glue_zig_native_wasm_layouts } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue uses RocBox for opaque boxed app types", .body = .{ .custom = .glue_zig_opaque_box } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue decrefs non-refcounted boxed payloads with payload alignment", .body = .{ .custom = .glue_zig_box_payload_alignment } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: RustGlue succeeds on fx platform", .body = .{ .custom = .glue_rust } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue handles duplicate tag-union names", .body = .{ .custom = .glue_zig_duplicate_tag_unions } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: RustGlue handles duplicate tag-union names", .body = .{ .custom = .glue_rust_duplicate_tag_unions } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: RustGlue decrefs non-refcounted boxed payloads with payload alignment", .body = .{ .custom = .glue_rust_box_payload_alignment } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue quotes bang record fields", .body = .{ .custom = .glue_zig_bang_record_fields } },
    .{ .id = 0, .suite = .glue, .name = "issue 9865: RustGlue does not panic for package nominal record API alias", .body = .{ .custom = .glue_package_nominal_api_alias } },
    .{ .id = 0, .suite = .glue, .name = "CGlue.roc expect tests pass", .body = .{ .custom = .glue_c_tests } },
};

// Subcommand suite cases

const parse_error_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "Failed to check" },
    .{ .stream = .stderr, .text = "error" },
    .{ .stream = .stderr, .text = "Unsupported" },
};

const type_error_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "TYPE MISMATCH" },
    .{ .stream = .stderr, .text = "error" },
    .{ .stream = .stderr, .text = "Found" },
};

const break_outside_loop_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "BREAK OUTSIDE LOOP" },
    .{ .stream = .stderr, .text = "break" },
};

const plus_type_error_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "MISSING METHOD" },
    .{ .stream = .stderr, .text = "TYPE MISMATCH" },
    .{ .stream = .stderr, .text = "error" },
    .{ .stream = .stderr, .text = "Found" },
};

const non_exhaustive_destructure_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "NON EXHAUSTIVE DESTRUCTURE" },
    .{ .stream = .stderr, .text = "Missing patterns:" },
    .{ .stream = .stderr, .text = "[]" },
};

const crash_expression_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "CRASH IN EXPRESSION" },
    .{ .stream = .stderr, .text = "Wrap it in a block expression" },
};

const warning_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "UNUSED VARIABLE" },
    .{ .stream = .stderr, .text = "warning" },
};

const repl_parse_diagnostic_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "PARSE ERROR" },
    .{ .stream = .stderr, .text = "UNEXPECTED TOKEN" },
};

const format_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "needs_formatting.roc" },
    .{ .stream = .stdout, .text = "needs_formatting.roc" },
    .{ .stream = .stderr, .text = "formatted" },
    .{ .stream = .stdout, .text = "formatted" },
};

const no_errors_needles = [_]OutputNeedle{
    .{ .stream = .stdout, .text = "No errors found" },
    .{ .stream = .stderr, .text = "No errors found" },
};

const invalid_llvm_debug_info_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "invalid #dbg record" },
    .{ .stream = .stderr, .text = "invalid debug info" },
    .{ .stream = .stderr, .text = "#dbg_declare" },
    .{ .stream = .stderr, .text = "DILocation" },
};

const subcommand_cases = [_]CliCase{
    .{ .id = 0, .suite = .subcommands, .name = "CLI test cache roots are distinct", .body = .{ .custom = .cli_cache_roots_distinct } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check watch inputs reject absolute file imports", .body = .{ .custom = .watch_inputs_reject_absolute_import } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check --watch reruns when completed child snapshot is stale", .skip = .{ .windows = "watch refresh race test uses a POSIX wrapper script" }, .body = .{ .custom = .watch_completed_run_refresh_reruns } },
    .{ .id = 0, .suite = .subcommands, .name = "roc --watch hot reloads dev shim code", .skip = .{ .windows = "generated hot-reload test platform uses POSIX host code" }, .body = .{ .custom = .hot_reload_dev_shim } },
    .{ .id = 0, .suite = .subcommands, .name = "roc --watch hot reloads app-provided Model through Box", .skip = .{ .windows = "generated hot-reload model test platform uses POSIX host code" }, .body = .{ .custom = .hot_reload_model_boundary } },
    .{ .id = 0, .suite = .subcommands, .name = "roc --watch runs headerless default app", .body = .{ .custom = .hot_reload_default_app } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build reports missing host symbols before linking", .body = .{ .command = .{ .args = &.{ "build", "--no-cache", "--target=x64musl" }, .roc_file = "test/missing-host-symbol/app.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING HOST SYMBOLS" }, .{ .stream = .stderr, .text = "roc_host_vanish" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check writes parse errors to stderr", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_parse_error.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &parse_error_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check displays correct file path in parse error messages", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_parse_error.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "has_parse_error.roc" }}, .not_contains = &.{.{ .stream = .stderr, .text = "\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects invalid hosted sections", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/hosted-section-errors/platform/main.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "INVALID HOSTED SECTION" }, .{ .stream = .stderr, .text = "Host.nonexistent" }, .{ .stream = .stderr, .text = "Host.quadruple" }, .{ .stream = .stderr, .text = "roc-host-bad" }, .{ .stream = .stderr, .text = "roc_alloc" }, .{ .stream = .stderr, .text = "roc__sneaky" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts a valid hosted section", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/fx/platform/main.roc", .not_contains = &.{.{ .stream = .stderr, .text = "INVALID HOSTED SECTION" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9826: roc check rejects open rows in hosted signatures", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9826_open_host_boundary/hosted/app.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "HOST BOUNDARY REQUIRES CLOSED ROWS" }, .{ .stream = .stderr, .text = "open record or tag-union rows" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "[ROC CRASHED]" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9826: roc check rejects open rows in provides signatures", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9826_open_host_boundary/provides/app.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "HOST BOUNDARY REQUIRES CLOSED ROWS" }, .{ .stream = .stderr, .text = "open record or tag-union rows" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "[ROC CRASHED]" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds on valid file", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/simple_success.roc", .not_contains = &.{ .{ .stream = .stderr, .text = "Failed to check" }, .{ .stream = .stderr, .text = "error" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc run prints warning diagnostics once (issue 9509)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/Issue9509WarningOnly.roc", .exit = .{ .code = 2 }, .stderr_min_len = 1, .occurrences = &.{ .{ .stream = .stderr, .text = "UNUSED VARIABLE", .count = 1 }, .{ .stream = .stderr, .text = "Found 0 error(s) and 1 warning(s)", .count = 1 } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build --opt=speed emits no invalid LLVM debug info", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/simple_success.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &invalid_llvm_debug_info_needles } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build --opt=speed --debug emits valid LLVM debug info", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--debug", "--no-cache" }, .roc_file = "test/cli/simple_success.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &invalid_llvm_debug_info_needles } } },
    // repro for https://github.com/roc-lang/roc/issues/9690: a self-recursive
    // closure that captures an enclosing value must compile through the LLVM
    // size/speed backend. The crash guard inside the program makes a wrong
    // result fail too, so a clean exit means it both built and computed 25.
    .{ .id = 0, .suite = .subcommands, .name = "issue 9690: recursive capturing closure builds and runs on LLVM size backend", .backend = .size, .body = .{ .command = .{ .args = &.{ "--opt=size", "--no-cache" }, .roc_file = "test/cli/Issue9690RecursiveCaptureClosure.roc", .exit = .success } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9897: nested callback capture count matches target", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/Issue9897NestedCaptureCount.roc", .exit = .success, .not_contains = &.{ .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "function reference capture count differs from its target" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test finalizes nested closure captures by identity", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/CaptureOrderFinalization.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test lowers opaque generic Try function wrappers", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/OpaqueTryFunction.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9717: spec-constr record cloning reaches target validation on LLVM speed backend", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/Issue9717SpecConstrSpanInvalidation.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "MISSING TARGET FILE" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "SIGSEGV" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9801: spec-constr call-pattern collection survives program.fns reallocation on LLVM size backend", .backend = .size, .body = .{ .command = .{ .args = &.{ "build", "--target=wasm32", "--opt=size", "--no-cache" }, .roc_file = "test/wasm/issue_9801_spec_constr_realloc/app.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "index out of bounds" }, .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "SIGSEGV" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "direct LIR callable calls survive variant table growth on LLVM speed backend", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/direct_lir_callable_variant_span_invalidation.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "direct LIR reachability referenced a missing function spec" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9815: roc run turns discarded user where-clause error into ordinary crash", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9815_discarded_user_where_clause_output.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "from_thing" }, .{ .stream = .stderr, .text = "Roc application crashed with this message:" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "unresolved `where`-clause method dispatch on a polymorphic value" }, .{ .stream = .stderr, .text = "dispatch plan had no method owner" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9858: default app args.get value times reports missing method without panic", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9858_default_app_args_get_times.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "times" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked method registry is missing resolved dispatch target" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "spec-constr preserves List.find_first return targets on LLVM size wasm backend", .backend = .size, .body = .{ .command = .{ .args = &.{ "build", "--target=wasm32", "--opt=size", "--no-cache" }, .roc_file = "test/wasm/spec_constr_return_target_app.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "return target type differed" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9548: record function fields can be called with method syntax", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9548_record_function_field_method.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9533: List(Num) annotation reports an undeclared type", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9533_list_num_annotation.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "UNDECLARED TYPE" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "postcheck invariant" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9508: anonymous recursive tag type reports an error", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9508_anonymous_recursion.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "ANONYMOUS RECURSION" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "overflowed its stack" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9210: ambiguous tuple access in a lambda", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9210_tuple_access_lambda.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "AMBIGUOUS TUPLE ACCESS" }}, .not_contains = &.{.{ .stream = .stderr, .text = "TYPE MISMATCH" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 2686: bare List tag payload reports its missing type argument", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_2686_bare_list_payload.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "TOO FEW ARGS" }, .{ .stream = .stderr, .text = "expects 1 argument" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9499: crash in match branch reports one targeted parse error", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/Issue9499CrashMatchBranch.roc", .exit = .failure, .contains = &crash_expression_needles, .not_contains = &.{ .{ .stream = .stderr, .text = "UNEXPECTED TOKEN IN EXPRESSION" }, .{ .stream = .stderr, .text = "match_branch_missing_arrow" }, .{ .stream = .stderr, .text = "expected_close_curly_at_end_of_match" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9544: list function parameter destructure must be exhaustive", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/Issue9544NonExhaustiveParam.roc", .exit = .failure, .contains = &non_exhaustive_destructure_needles } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph succeeds with 1 file and 1 symbol", .body = .{ .custom = .generated_graph_1_1 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph succeeds with 5 files and 5 symbols", .body = .{ .custom = .generated_graph_5_5 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph handles many symbols per file", .body = .{ .custom = .generated_graph_2_100 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph handles many imported files", .body = .{ .custom = .generated_graph_200_5 } },
    .{ .id = 0, .suite = .subcommands, .name = "list builtins inline in native --opt=speed build", .body = .{ .custom = .list_builtin_inlined } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform x64musl matches direct write assembly", .skip = .{ .always = "TODO: direct-write default-platform codegen" }, .body = .{ .custom = .default_platform_linux_disassembly } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform x64glibc succeeds", .body = .{ .custom = .default_platform_build_x64glibc } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform arm64glibc succeeds", .body = .{ .custom = .default_platform_build_arm64glibc } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform wasm32 archive succeeds", .body = .{ .custom = .default_platform_build_wasm32 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform wasm32 archive output is reproducible", .body = .{ .custom = .default_platform_wasm32_archive_reproducible } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build macOS output basename does not affect bytes", .body = .{ .custom = .macos_output_basename_reproducible } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform crash prints debug backtrace on x64musl", .body = .{ .custom = .default_platform_crash_x64musl } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform crash prints debug backtrace on arm64musl", .body = .{ .custom = .default_platform_crash_arm64musl } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform crash prints debug backtrace on x64mac", .body = .{ .custom = .default_platform_crash_x64mac } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform crash prints debug backtrace on arm64mac", .body = .{ .custom = .default_platform_crash_arm64mac } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform crash prints debug backtrace on x64win", .body = .{ .custom = .default_platform_crash_x64win } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform crash prints debug backtrace on arm64win", .body = .{ .custom = .default_platform_crash_arm64win } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform stack overflow prints debug backtrace on x64musl", .body = .{ .custom = .default_platform_stack_overflow_x64musl } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform stack overflow prints debug backtrace on arm64musl", .body = .{ .custom = .default_platform_stack_overflow_arm64musl } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform stack overflow prints debug backtrace on x64mac", .body = .{ .custom = .default_platform_stack_overflow_x64mac } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform stack overflow prints debug backtrace on arm64mac", .body = .{ .custom = .default_platform_stack_overflow_arm64mac } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform stack overflow prints debug backtrace on x64win", .body = .{ .custom = .default_platform_stack_overflow_x64win } },
    .{ .id = 0, .suite = .subcommands, .name = "default platform stack overflow prints debug backtrace on arm64win", .body = .{ .custom = .default_platform_stack_overflow_arm64win } },
    .{ .id = 0, .suite = .subcommands, .name = "roc version outputs at least 5 chars to stdout", .body = .{ .command = .{ .args = &.{"version"}, .stdout_min_len = 5 } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl batch mode suppresses welcome banner", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "", .stdout_exact = "", .stderr_exact = "" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl evaluates simple expression", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "1 + 1\n", .contains = &.{.{ .stream = .stdout, .text = "2" }}, .not_contains = &.{ .{ .stream = .stdout, .text = "Roc REPL" }, .{ .stream = .stdout, .text = ">" }, .{ .stream = .stdout, .text = "Goodbye" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl evaluates final stdin line without trailing newline", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "1 + 1", .contains = &.{.{ .stream = .stdout, .text = "2" }}, .stderr_exact = "" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl :help command works", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = ":help\n", .contains_any = &.{.{ .needles = &.{ .{ .stream = .stdout, .text = ":exit" }, .{ .stream = .stdout, .text = ":quit" } } }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl :exit command exits cleanly in batch mode", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = ":exit\n", .stdout_exact = "", .stderr_exact = "" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl parse diagnostics go to stderr in batch mode", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "1+\\n\n", .exit = .failure, .stdout_exact = "", .contains_any = &.{.{ .needles = &repl_parse_diagnostic_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "Error: ParseError" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl type diagnostics go to stderr without ANSI in batch mode", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "x = 1\nx + \"a\"\nx + 1\n", .exit = .failure, .contains = &.{ .{ .stream = .stdout, .text = "assigned `x`" }, .{ .stream = .stdout, .text = "2.0" }, .{ .stream = .stderr, .text = "TYPE MISMATCH" } }, .not_contains = &.{.{ .stream = .stderr, .text = "\x1b" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl variable definition and usage", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "x = 5\nx + 3\n", .contains = &.{.{ .stream = .stdout, .text = "8" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl string expression", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "\"hello\"\n", .contains = &.{.{ .stream = .stdout, .text = "hello" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc help contains Usage:", .body = .{ .command = .{ .args = &.{"help"}, .contains = &.{.{ .stream = .stdout, .text = "Usage:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc licenses contains =====", .body = .{ .command = .{ .args = &.{"licenses"}, .contains = &.{.{ .stream = .stdout, .text = "=====" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --check fails on unformatted file", .body = .{ .command = .{ .args = &.{ "fmt", "--check" }, .roc_file = "test/cli/needs_formatting.roc", .exit = .failure, .contains_any = &.{.{ .needles = &format_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --check succeeds on well-formatted file", .body = .{ .command = .{ .args = &.{ "fmt", "--check" }, .roc_file = "test/cli/well_formatted.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --check succeeds on expression break", .body = .{ .command = .{ .args = &.{ "fmt", "--check" }, .roc_file = "test/cli/BreakExpressionInLoop.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt reformats file in place", .body = .{ .custom = .fmt_reformats_file } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt does not change well-formatted file", .body = .{ .custom = .fmt_does_not_change_file } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --stdin formats unformatted input", .body = .{ .custom = .fmt_stdin_formats } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --stdin does not change well-formatted input", .body = .{ .custom = .fmt_stdin_does_not_change } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports type error - annotation mismatch", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_type_error_annotation.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports type error - plus operator with incompatible types", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_type_error_plus_operator.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &plus_type_error_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects expression break across lambda boundary", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/BreakAcrossLambdaExpression.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &break_outside_loop_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects statement break across lambda boundary", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/BreakAcrossLambdaStatement.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &break_outside_loop_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts expression break inside loop", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/BreakExpressionInLoop.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "BREAK OUTSIDE LOOP" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test covers Str.is_eq edge cases", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/StrIsEqEdgeCases.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser without curried Try record result", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserInvalidReturn.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser for empty tag union", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserEmptyTagUnion.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks rename_field", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingRenameFieldMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "rename_field" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks parse_record_field", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingRecordMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "parse_record_field" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks skip_record_field", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingSkipRecordMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "skip_record_field" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks parse_str", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingStrMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "parse_str" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks parse_tag_union", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingTagUnionMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "parse_tag_union" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks missing_optional_field", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingOptionalMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "missing_optional_field" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when format lacks missing_record_field", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingRequiredMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "missing_record_field" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived parser when custom nominal lacks parser_for", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserMissingCustomNominalMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "parser_for" }, .{ .stream = .stderr, .text = "Token" } }, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects recursive nominal parser without parser_for method", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserRecursiveNominalMissingMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "parser_for" }, .{ .stream = .stderr, .text = "Tree" } }, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived encode_to when format lacks encode_str", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/EncodeToMissingStrMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "encode_str" }}, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects derived encode_to when custom nominal lacks encode_to", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/EncodeToMissingCustomNominalMethod.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "encode_to" }, .{ .stream = .stderr, .text = "Token" } }, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects JSON parser function fields", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/JsonUnsupportedFunctionParserField.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects JSON encode_to function fields", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/JsonUnsupportedFunctionEncodeField.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects JSON parser composite dict keys", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/JsonUnsupportedCompositeDictParserKey.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects JSON encode_to composite dict keys", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/JsonUnsupportedCompositeDictEncodeKey.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects JSON encode_to empty tag unions", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/JsonUnsupportedEmptyTagUnionEncode.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects unsupported numeric parser field before postcheck", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserUnsupportedNumericField.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects old structural parser method name", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserOldStructuralMethodRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects old parse_record_field event API", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserOldRecordFieldApi.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects old ParseTagUnionSpec.parse matcher API", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserOldTagSpecApi.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser Field direct construction", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserFieldConstructionRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser Field backing access", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserFieldAccessRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects hidden Encoding parser state types", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserHiddenEncodingTypesRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "MISSING NESTED TYPE" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser Field pattern matching", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserFieldPatternRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser FieldNames backing access", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserFieldsAccessRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser FieldNames pattern matching", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserFieldsPatternRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects parser Field phantom mismatch", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserFieldShapeMismatchRejected.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts direct nominal record construction", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/NominalRecordConstruction.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts optional-only parser record without missing_record_field", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserOptionalOnlyRecordNoMissingMethod.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts derived parser on structural alias", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/ParserStructuralAlias.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test uses renamed FieldNames metadata in derived parser", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserRenamedFieldsMetadata.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test uses renamed FieldNames name bounds in derived parser", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserRenamedFieldBounds.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports userspace FieldNames.rename_fields", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserRuntimeRenameFields.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test restores stored parser FieldNames metadata", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserStoredTryFieldCaseless.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test derives optional parser field with format-defined absence tag", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserOptionalAbsentTagName.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test derives optional non-Str parser field", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserOptionalNonStrField.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test uses custom nominal parser_for inside derived record", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserCustomNominalField.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports structural encode_to on records", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/EncodeToStructuralRecord.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports structural encode_to on empty records without field methods", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/EncodeToEmptyRecordNoFieldMethods.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored top-level encode_to value", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/EncodeToTopLevelStored.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test round-trips JSON parse and encode", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/JsonEncodeRoundTrip.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test covers JSON integer edge cases", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/JsonEncodeEdgeCases.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test covers JSON numeric edge cases", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/JsonEncodeNumberEdgeCases.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test covers JSON null container edge cases", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/JsonEncodeNullContainerEdgeCases.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test covers JSON nested container edge cases", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/JsonEncodeNestedContainerEdgeCases.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test derives opaque JSON parse and encode methods", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/JsonEncodeOpaqueDerivation.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports top-level parser construction", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserTopLevelConstructor.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored top-level parser value", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserTopLevelStoredParser.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored top-level parser constructor", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserTopLevelStoredParserConstructor.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test stores boxed recursive parser constant", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserBoxedRecursiveConst.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "struct const plan had non-struct layout" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored parser input wrapper", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserTopLevelStoredInputWrapper.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored renamed parser input wrapper", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserTopLevelStoredRenamedInputWrapper.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored and runtime prepared parser fields", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserStoredAndRuntimePreparedFields.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test supports stored parser input wrapper validation", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/ParserTopLevelStoredValidatedWrapper.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check test/int/app.roc does not panic", .skip = .{ .windows = "test/int platform does not have Windows host libraries" }, .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/int/app.roc", .exit = .not_panic, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test/int/app.roc runs successfully (interpreter)", .backend = .interpreter, .skip = .{ .windows = "test/int platform does not have Windows host libraries" }, .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/int/app.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test/int/app.roc runs successfully (dev)", .backend = .dev, .skip = .{ .always = "TODO: dev backend compilation fails for test/int/app.roc" }, .body = .{ .custom = .noop } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test/str/app.roc runs successfully (interpreter)", .backend = .interpreter, .skip = .{ .windows = "test/str platform does not have Windows host libraries" }, .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/str/app.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test/str/app.roc runs successfully (default dev)", .backend = .dev, .skip = .{ .windows = "test/str platform does not have Windows host libraries" }, .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/str/app.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test/str/app.roc runs successfully (dev)", .backend = .dev, .skip = .{ .windows = "test/str platform does not have Windows host libraries" }, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--no-cache" }, .roc_file = "test/str/app.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test/str/app_static_24_byte_string.roc does not panic", .skip = .{ .windows = "test/str platform does not have Windows host libraries" }, .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/str/app_static_24_byte_string.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "reached unreachable code" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build creates executable from test/int/app.roc (interpreter)", .backend = .interpreter, .skip = .{ .windows = "test/int platform does not have Windows host libraries" }, .body = .{ .custom = .build_int_interpreter_creates_output } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build creates executable from test/int/app.roc (dev)", .backend = .dev, .skip = .{ .always = "TODO: dev backend compilation fails for test/int/app.roc" }, .body = .{ .custom = .noop } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build archive output lowers platform required init consts", .body = .{ .command = .{ .args = &.{ "build", "--no-cache" }, .roc_file = "test/postcheck/platform_required_init/app.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build divergent if with all crash branches does not panic", .body = .{ .command = .{ .args = &.{ "build", "--no-cache" }, .roc_file = "test/fx/divergent_if_all_branches_crash_repro.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build executable runs correctly (interpreter)", .backend = .interpreter, .skip = .{ .windows = "test/int platform does not have Windows host libraries" }, .body = .{ .custom = .build_int_interpreter_output_runs } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build --opt=dev executable runs correctly for test/int/app.roc", .backend = .dev, .skip = .{ .windows = "test/int platform does not have Windows host libraries" }, .body = .{ .custom = .build_int_dev_output_runs } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build fails with file not found error", .body = .{ .command = .{ .args = &.{"build"}, .roc_file = "nonexistent_file.roc", .exit = .failure, .contains_any = &.{.{ .needles = &.{ .{ .stream = .stderr, .text = "FileNotFound" }, .{ .stream = .stderr, .text = "not found" }, .{ .stream = .stderr, .text = "NOT FOUND" }, .{ .stream = .stderr, .text = "Failed" } } }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build fails with invalid target error", .body = .{ .command = .{ .args = &.{ "build", "--target=invalid_target_name" }, .roc_file = "test/int/app.roc", .exit = .failure, .contains_any = &.{.{ .needles = &.{ .{ .stream = .stderr, .text = "Invalid target" }, .{ .stream = .stderr, .text = "invalid" } } }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build wasm32 shared module succeeds for list builtins", .body = .{ .command = .{ .args = &.{ "build", "--target=wasm32", "--no-cache" }, .roc_file = "test/wasm/list_builtin_static_lib_app.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "FunctionTypeMismatch" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build glibc target gives helpful error on non-Linux", .body = .{ .custom = .build_glibc_target_non_linux_error } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build Shared output links a Windows DLL", .body = .{ .custom = .build_windows_shared_library } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test caches passing results (interpreter)", .backend = .interpreter, .body = .{ .custom = .cache_passing_results } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test caches passing results (dev)", .backend = .dev, .body = .{ .custom = .cache_passing_results } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test caches failing results (interpreter)", .backend = .interpreter, .body = .{ .custom = .cache_failing_results } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test caches failing results (dev)", .backend = .dev, .body = .{ .custom = .cache_failing_results } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test cache invalidated by source change (interpreter)", .backend = .interpreter, .body = .{ .custom = .cache_invalidated_by_source_change } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test cache invalidated by source change (dev)", .backend = .dev, .body = .{ .custom = .cache_invalidated_by_source_change } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test --verbose works from cache (interpreter)", .backend = .interpreter, .body = .{ .custom = .verbose_works_from_cache } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test --verbose works from cache (dev)", .backend = .dev, .body = .{ .custom = .verbose_works_from_cache } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test --verbose caches failure reports (interpreter)", .backend = .interpreter, .body = .{ .custom = .verbose_caches_failure_reports } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test --verbose caches failure reports (dev)", .backend = .dev, .body = .{ .custom = .verbose_caches_failure_reports } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test non-verbose run caches verbose failure reports for later verbose run (interpreter)", .backend = .interpreter, .body = .{ .custom = .non_verbose_caches_verbose_reports } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test non-verbose run caches verbose failure reports for later verbose run (dev)", .backend = .dev, .body = .{ .custom = .non_verbose_caches_verbose_reports } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test with nested list chunks does not panic on layout upgrade (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter" }, .roc_file = "test/cli/issue8699.roc", .exit = .{ .code = 1 }, .contains = &.{.{ .stream = .stderr, .text = "FAIL" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "overflow" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test with nested list chunks does not panic on layout upgrade (dev)", .backend = .dev, .skip = .{ .always = "TODO: dev backend compilation fails for test/cli/issue8699.roc" }, .body = .{ .custom = .noop } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test failure output contains source snippet (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter" }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "\u{2502}" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test failure output contains source snippet (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev" }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "\u{2502}" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 4633: failed expect prints mentioned values (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/Issue4633ExpectContext.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "Mentioned values:" }, .{ .stream = .stderr, .text = "x = 5" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 4633: failed expect prints mentioned values (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/Issue4633ExpectContext.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "Mentioned values:" }, .{ .stream = .stderr, .text = "x = 5" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test failure output contains doc comment (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter" }, .roc_file = "test/cli/FailWithDocComment.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "## This test should fail" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" }, .{ .stream = .stderr, .text = "\u{2502}" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test failure output contains doc comment (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev" }, .roc_file = "test/cli/FailWithDocComment.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "## This test should fail" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" }, .{ .stream = .stderr, .text = "\u{2502}" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test verbose and non-verbose failure format match (interpreter)", .backend = .interpreter, .body = .{ .custom = .verbose_and_non_verbose_failure_format_match } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test verbose and non-verbose failure format match (dev)", .backend = .dev, .body = .{ .custom = .verbose_and_non_verbose_failure_format_match } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check returns exit code 2 for warnings", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/fx/run_warning_only.roc", .exit = .{ .code = 2 }, .contains = &.{.{ .stream = .stderr, .text = "0 error" }}, .contains_any = &.{.{ .needles = &warning_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check warns for adjacent string pattern captures", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/fx/string_pattern_adjacent_capture_warning.roc", .exit = .{ .code = 2 }, .contains = &.{ .{ .stream = .stderr, .text = "UNREACHABLE PATTERN CAPTURE" }, .{ .stream = .stderr, .text = "0 error" } }, .contains_any = &.{.{ .needles = &warning_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check returns exit code 0 for no warnings or errors", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/simple_success.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check returns exit code 1 for errors", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_type_error_annotation.roc", .exit = .{ .code = 1 } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports comptime division by zero without panicking", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/comptime_div_zero.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "COMPILE TIME CRASH" }, .{ .stream = .stderr, .text = "I64 division by zero" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports comptime modulo by zero without panicking", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/comptime_mod_zero.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "COMPILE TIME CRASH" }, .{ .stream = .stderr, .text = "I64 division by zero" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports large default Dec scientific literal without panicking", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/large_scientific_default_dec.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "INVALID NUMBER" }, .{ .stream = .stderr, .text = "Dec" }, .{ .stream = .stderr, .text = "large_scientific_default_dec.roc:1:" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "panic:" }, .{ .stream = .stderr, .text = ".zig-cache/tmp" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts huge integral scientific literal without slow conversion", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/huge_scientific_default_dec.roc", .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check preserves numeric literal constraints before reporting large default Dec scientific literal", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/large_scientific_list_default_dec.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "INVALID NUMBER" }, .{ .stream = .stderr, .text = "Dec" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9565: resolved open numeral literal cannot overflow I8", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9565_i8_overflow.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "INVALID NUMBER" }, .{ .stream = .stderr, .text = "I8" } }, .not_contains = &.{.{ .stream = .stderr, .text = "No errors found" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9565: default platform Exit I8 validates loop bound", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9565_default_platform_exit_overflow.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "INVALID NUMBER" }, .{ .stream = .stderr, .text = "I8" }, .{ .stream = .stderr, .text = "issue_9565_default_platform_exit_overflow.roc:4:" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "No errors found" }, .{ .stream = .stderr, .text = ".zig-cache/tmp" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check treats integral scientific notation as integer syntax sugar", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/scientific_integer_u8.roc", .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc returns exit code 2 for warnings (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/fx/run_warning_only.roc", .exit = .{ .code = 2 }, .contains_any = &.{.{ .needles = &warning_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc --opt=dev returns exit code 2 for warnings", .backend = .dev, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--no-cache" }, .roc_file = "test/fx/run_warning_only.roc", .exit = .{ .code = 2 }, .contains_any = &.{.{ .needles = &warning_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc returns exit code 1 for old platform download", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/old_hello_world.roc", .exit = .{ .code = 1 }, .contains = &.{.{ .stream = .stderr, .text = "platform was built with the old Roc" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc --opt=dev rejects non executable targets", .backend = .dev, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--target=wasm32" }, .roc_file = "test/wasm/app.roc", .exit = .failure, .contains_any = &.{.{ .needles = &.{ .{ .stream = .stderr, .text = "only produces static libraries" }, .{ .stream = .stderr, .text = "TARGET NOT SUPPORTED" }, .{ .stream = .stderr, .text = "unsupported target" } } }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build returns exit code 2 for warnings (interpreter)", .backend = .interpreter, .body = .{ .custom = .build_warning_interpreter } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build returns exit code 2 for warnings (dev)", .backend = .dev, .skip = .{ .always = "TODO: dev backend compilation fails for test/fx/run_warning_only.roc" }, .body = .{ .custom = .noop } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check with -j1 succeeds on valid file", .body = .{ .command = .{ .args = &.{ "check", "--no-cache", "-j1" }, .roc_file = "test/cli/simple_success.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check with --jobs=1 succeeds on valid file", .body = .{ .command = .{ .args = &.{ "check", "--no-cache", "--jobs=1" }, .roc_file = "test/cli/simple_success.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check with --jobs=2 succeeds on valid file", .body = .{ .command = .{ .args = &.{ "check", "--no-cache", "--jobs=2" }, .roc_file = "test/cli/simple_success.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check with invalid --jobs value returns error", .body = .{ .command = .{ .args = &.{ "check", "--jobs=abc" }, .roc_file = "test/cli/simple_success.roc", .exit = .{ .code = 1 }, .contains = &.{.{ .stream = .stderr, .text = "not a valid value" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check does not panic on invalid package shorthand import (issue 9084)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/invalid_package_shorthand.roc", .exit = .not_panic, .stderr_min_len = 1, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "Coordinator timeout" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds with unused app package shorthand (issue 9488)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/package_shorthand_check_app/main.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check resolves and checks a used sibling package shorthand (issue 9488)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/package_shorthand_used_app/main.roc", .exit = .not_panic, .contains = &.{ .{ .stream = .stderr, .text = "package_shorthand_used_pkg" }, .{ .stream = .stderr, .text = "TYPE MISMATCH" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check does not hang on tag union type alias inside List (issue 9481)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/tag_union_alias_hang.roc", .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "Coordinator stuck" }, .{ .stream = .stderr, .text = "Infinite loop" }, .{ .stream = .stderr, .text = "INFINITE TYPE" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check infers recursive field method result type (issue 9632)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9632_recursive_field_method.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "unresolved type variable" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test eq on annotated recursive type does not overflow (issue 9633)", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/issue_9633_recursive_eq_annotation.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "overflowed its stack" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test expect with unannotated helper returning Try (issue 9691, interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/issue_9691_expect_helper_returning_try.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test expect with unannotated helper returning Try (issue 9691, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue_9691_expect_helper_returning_try.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stdout, .text = "failed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test expect matches Ok arm of unannotated Try helper (issue 9691, interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/issue_9691_expect_try_tag_discriminant.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test expect matches Ok arm of unannotated Try helper (issue 9691, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue_9691_expect_try_tag_discriminant.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stdout, .text = "failed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test recursive nested-list accumulator passes (issue 9742, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue_9742_recursive_nested_list_accumulator.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{.{ .stream = .stdout, .text = "failed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds on string interpolation in Try.map_err (issue 9650)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9650_checked_interpolation_map_err.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "ordinary method call reached artifact publication" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9879: roc check succeeds when package function accepts List of imported tag union", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9879_imported_tag_app/main.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "SIGSEGV" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc default app numeric addition lowers without postcheck panic (issue 9706)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9706_dispatch_plan_addition.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "dispatch plan had no method owner" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc platform-required args len reports type mismatch (issue 9540)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9540_platform_required_list_len.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "TYPE MISMATCH" }, .{ .stream = .stderr, .text = "U64" }, .{ .stream = .stderr, .text = "Str" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked dispatch target return type conflicted" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check default app missing method on args.len reports diagnostic (issue 9782)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9782_default_arg_len_missing_method.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "non_existent" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked method registry is missing resolved dispatch target" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc default app missing method on args.len reports diagnostic (issue 9782)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9782_default_arg_len_missing_method.roc", .exit = .not_panic, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "non_existent" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked method registry is missing resolved dispatch target" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check platform-required missing method reports diagnostic (issue 9541)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9541_platform_required_missing_method.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "not_a_method" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked method registry is missing resolved dispatch target" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check platform-required record field reports nested method diagnostic", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9762_platform_required_record_field.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "not_a_method" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked method registry is missing resolved dispatch target" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check platform-required destructured arg reports iterator diagnostic (issue 9542)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9542_platform_required_for_args.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "iter" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "checked iterator dispatch method registry is missing resolved target" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check platform-required match args reports type mismatch (issue 9559)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9559_platform_required_match_args.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "TYPE MISMATCH" }, .{ .stream = .stderr, .text = "Str" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "scalar immediate cannot stand in for RocStr layout" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc U8 addition overflow crashes (issue 9360, interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/issue9360_integer_add_overflow_u8.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "Integer addition overflowed!" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc U8 addition overflow crashes (issue 9360, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue9360_integer_add_overflow_u8.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "Integer addition overflowed!" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc U8 subtraction underflow crashes (issue 9361, interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/issue9361_integer_sub_underflow_u8.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "Integer subtraction overflowed!" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc U8 subtraction underflow crashes (issue 9361, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue9361_integer_sub_underflow_u8.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "Integer subtraction overflowed!" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc U128 addition overflow crashes (issue 9360, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue9360_integer_add_overflow_u128.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "Integer addition overflowed!" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc I128 subtraction underflow crashes (issue 9361, dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue9361_integer_sub_underflow_i128.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "Integer subtraction overflowed!" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc default app numeric addition wrapped in Err lowers without postcheck panic (issue 9734)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9734_dispatch_plan_err.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "dispatch plan had no method owner" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc default app args.get value multiplied by numeral reports diagnostic (issue 9857)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9857_args_get_question_multiply.roc", .exit = .not_panic, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "TYPE MISMATCH" } } }}, .not_contains = &.{ .{ .stream = .stderr, .text = "instantiation unified two different primitive types" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl Dict.join_map with undetermined key reports ambiguous to_hash (issue 9644)", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "source = Dict.single(\"a\", 1).insert(\"b\", 2)\nDict.join_map(source, |_, _| Dict.empty()).len()\n", .exit = .not_panic, .contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "to_hash" } }, .not_contains = &.{ .{ .stream = .stderr, .text = "dispatch plan had no method owner" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc repl polymorphic where-clause helper through nested generalized defs is accepted, not ambiguous", .body = .{ .command = .{ .args = &.{"repl"}, .stdin = "run : a -> a where [a.go : a -> a]\nrun = |x| x.go()\nwrap = |y| {\n  go2 = |z| run(z)\n  go2(y)\n}\nwrap\n", .exit = .not_panic, .contains = &.{.{ .stream = .stdout, .text = "<function>" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "MISSING METHOD" }, .{ .stream = .stderr, .text = "dispatch plan had no method owner" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check custom interpolation in tuple annotation reports type mismatch (issue 9711)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9711_checked_interpolation_tuple.roc", .exit = .not_panic, .contains = &.{.{ .stream = .stderr, .text = "TYPE MISMATCH" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "instantiation unified a primitive type with a non-primitive type" }, .{ .stream = .stderr, .text = "checked interpolation constraint had no generated item type" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check open error row callback via exposed platform module alias does not panic (issue 9655)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9655_open_init_row_platform/app.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "missing platform declaration artifact" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts nominal app type bound by platform for-clause (issue 9731)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9731_nominal_for_clause/app.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "MISSING PLATFORM REQUIRED TYPE" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts alias app type bound by platform for-clause", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9731_nominal_for_clause/app_alias.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "MISSING PLATFORM REQUIRED TYPE" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build nominal app type bound by platform for-clause does not panic (issue 9731)", .body = .{ .command = .{ .args = &.{ "build", "--no-cache" }, .roc_file = "test/cli/issue_9731_nominal_for_clause/app.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "platform for-clause substitution missing matching app type declaration" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check and roc run report platform requires diagnostics identically", .body = .{ .custom = .platform_requires_checker_diagnostics } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check platform boundary generic State alias succeeds (issue 9767)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/postcheck/issue_9767_platform_generic_state/app.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "reached unreachable code" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build opaque method returning backing with imported nested alias does not panic (issue 9750)", .body = .{ .command = .{ .args = &.{ "build", "--no-cache" }, .roc_file = "test/postcheck/issue_9750_opaque_imported_nested_alias/app.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "imported nominal declaration formal was not projected" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test mutable Str scanner over many vars does not diverge in ARC certifier (issue 9658)", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/Issue9658ArcDiverge.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "diverge across jumps" }, .{ .stream = .stderr, .text = "ARC borrow certifier" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build of Err with unannotated numeric payload is a clean type mismatch, not a dev-backend unreachable (issue 9735)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "build", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/issue_9735_err_literal.roc", .exit = .not_panic, .not_contains = &.{ .{ .stream = .stderr, .text = "reached unreachable code" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds on Parser type module", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/package_simple_parser/Parser.roc", .not_contains = &.{.{ .stream = .stderr, .text = "error" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds when block-local associated value captures local value", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/block_local_assoc_capture/Test.roc", .exit = .success } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test runs expects in Parser type module (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/package_simple_parser/Parser.roc", .contains = &.{ .{ .stream = .stdout, .text = "passed" }, .{ .stream = .stdout, .text = "(7)" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test runs expects in Parser type module (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/package_simple_parser/Parser.roc", .contains = &.{ .{ .stream = .stdout, .text = "passed" }, .{ .stream = .stdout, .text = "(7)" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test polymorphic list reverse with numeric literal does not overflow (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter" }, .roc_file = "test/cli/polymorphic_list_reverse.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "overflow" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test polymorphic list reverse with numeric literal does not overflow (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev" }, .roc_file = "test/cli/polymorphic_list_reverse.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "overflow" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test polymorphic list reverse within same module", .body = .{ .command = .{ .args = &.{"test"}, .roc_file = "test/cli/PolymorphicListReverseMod.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9388 List.sort_with top-level expect does not overflow", .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/Issue9388SortWithTopLevelExpect.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "overflowed its stack" }, .{ .stream = .stderr, .text = "Segmentation fault" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9769 derived record equality is stable across expects", .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/Issue9769DefaultRecordEq.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "failed" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9487 static dispatch result compares to tag literal", .skip = .{ .windows = "issue 9487 static dispatch repro is run on POSIX only" }, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/Issue9487StaticDispatchEq.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9636 F64 to_u64_try in expect does not crash", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/Issue9636FloatToU64TryExpect.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Unreachable" }, .{ .stream = .stderr, .text = "reached unreachable code" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test eq on tag union with list payload does not panic", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/EqTagWithListPayload.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test draft sealing coverage for nested closures loops equality and hashing", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/DraftSealingCoverage.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "postcheck invariant" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test list prepend retains its element (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/RcListPrepend.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test list prepend retains its element (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/RcListPrepend.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test list set retains its element (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/RcListSet.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test list set retains its element (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/RcListSet.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test list replace retains its element (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/RcListReplace.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test list replace retains its element (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/RcListReplace.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9392 numeric utility expects are deterministic with no cache", .body = .{ .custom = .issue_9392_deterministic_no_cache } },
    .{ .id = 0, .suite = .subcommands, .name = "roc issue 9208 open union tag before Exit matches wildcard", .body = .{ .command = .{ .args = &.{ "--opt=interpreter", "--no-cache" }, .roc_file = "test/fx-open/test_bar_error.roc", .exit = .{ .code = 1 }, .contains = &.{.{ .stream = .stderr, .text = "exited with other error: Bar" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build issue 9435 hosted nominal return builds without mono panic", .body = .{ .custom = .build_issue_9435_hosted_nominal_return } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check Builtin.roc succeeds", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "src/build/roc/Builtin.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc docs Builtin.roc succeeds", .body = .{ .command = .{ .args = &.{ "docs", "--no-cache" }, .roc_file = "src/build/roc/Builtin.roc", .contains = &.{.{ .stream = .stdout, .text = "Generated docs for" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test complex_package --verbose passes all tests", .body = .{ .command = .{ .args = &.{ "test", "--no-cache", "--verbose" }, .roc_file = "test/complex_package/main.roc", .contains = &.{ .{ .stream = .stdout, .text = "tests passed" }, .{ .stream = .stdout, .text = "PASS" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc bundle complex_package includes all transitively imported modules", .body = .{ .custom = .bundle_complex_package } },
    .{ .id = 0, .suite = .subcommands, .name = "failed inline expect exits with code 1 and continues program (dev)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not provide roc_default_echo_line" }, .body = .{ .command = .{ .args = &.{}, .roc_file = "test/cli/failed_inline_expect.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stdout, .text = "Hello, World!" }, .{ .stream = .stderr, .text = "expect failed" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "failed inline expect exits with code 1 and continues program (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{"--opt=interpreter"}, .roc_file = "test/cli/failed_inline_expect.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stdout, .text = "Hello, World!" }, .{ .stream = .stderr, .text = "Expect failed" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "failed inline expect is omitted from roc --opt=size", .body = .{ .command = .{ .args = &.{ "--opt=size", "--no-cache" }, .roc_file = "test/cli/failed_inline_expect.roc", .contains = &.{.{ .stream = .stdout, .text = "Hello, World!" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "expect failed" }, .{ .stream = .stderr, .text = "Expect failed" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "failed inline expect is omitted from roc --opt=speed", .body = .{ .command = .{ .args = &.{ "--opt=speed", "--no-cache" }, .roc_file = "test/cli/failed_inline_expect.roc", .contains = &.{.{ .stream = .stdout, .text = "Hello, World!" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "expect failed" }, .{ .stream = .stderr, .text = "Expect failed" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "inline expect condition is not run when omitted from roc --opt=size", .body = .{ .command = .{ .args = &.{ "--opt=size", "--no-cache" }, .roc_file = "test/cli/issue_7348_inline_expect_condition_omitted.roc", .contains = &.{.{ .stream = .stdout, .text = "Hello, World!" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "inline expect condition was evaluated" }, .{ .stream = .stderr, .text = "inline expect final expression was evaluated" }, .{ .stream = .stderr, .text = "omitted dbg output" }, .{ .stream = .stderr, .text = "omitted final-expression dbg output" }, .{ .stream = .stderr, .text = "`DBG` IN OPTIMIZED BUILD" }, .{ .stream = .stderr, .text = "reached code after checked control transfer" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "dbg runs with a warning from roc --opt=size", .body = .{ .command = .{ .args = &.{ "--opt=size", "--no-cache" }, .roc_file = "test/cli/optimized_dbg_warning.roc", .exit = .{ .code = 2 }, .contains = &.{ .{ .stream = .stdout, .text = "Done" }, .{ .stream = .stderr, .text = "`DBG` IN OPTIMIZED BUILD" }, .{ .stream = .stderr, .text = "optimized_dbg_warning.roc:3:5" }, .{ .stream = .stderr, .text = "runtime dbg output" } }, .not_contains = &.{.{ .stream = .stderr, .text = "/tmp/roc" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Ok inside top-level expect passes (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/QuestionInExpect.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Ok inside top-level expect passes (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/QuestionInExpect.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Err inside top-level expect fails with snippet and value (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/QuestionInExpectFail.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "evaluated an `Err` inside an" }, .{ .stream = .stderr, .text = "The value was: Err(IsNegative)" }, .{ .stream = .stderr, .text = "result = to_positive(-3)?" } }, .not_contains = &.{.{ .stream = .stderr, .text = "crash" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Err inside top-level expect fails with snippet and value (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/QuestionInExpectFail.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "evaluated an `Err` inside an" }, .{ .stream = .stderr, .text = "The value was: Err(IsNegative)" }, .{ .stream = .stderr, .text = "result = to_positive(-3)?" } }, .not_contains = &.{.{ .stream = .stderr, .text = "crash" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Err inside top-level expect fails with snippet and value (llvm)", .backend = .speed, .body = .{ .command = .{ .args = &.{ "test", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/QuestionInExpectFail.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "evaluated an `Err` inside an" }, .{ .stream = .stderr, .text = "The value was: Err(IsNegative)" }, .{ .stream = .stderr, .text = "result = to_positive(-3)?" } }, .not_contains = &.{.{ .stream = .stderr, .text = "crash" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build imported nominal type static dispatch does not crash (issue 9716)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "build", "--opt=dev", "--no-cache", "--target=x64musl" }, .roc_file = "test/cli/issue_9716_imported_type_static_dispatch/main.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "segmentation fault" }, .{ .stream = .stderr, .text = "reached unreachable code" } } } } },
};

// Wire protocol (child -> parent via pipe)

const TestStatus = enum(u8) {
    pass = 0,
    build_failed = 1,
    run_failed = 2,
    timeout = 3,
    crash = 4,
    infra_error = 5,
    skip = 6,
};

const TestPhase = enum(u8) {
    setup = 0,
    build = 1,
    run = 2,
    cleanup = 3,
    harness = 4,
};

const WireHeader = extern struct {
    status: u8,
    phase: u8,
    duration_ns: u64,
    build_ns: u64,
    run_ns: u64,
    exit_code: u32,
    stderr_len: u32,
    stdout_len: u32,
    message_len: u32,
};

const TestResult = struct {
    status: TestStatus = .crash,
    phase: TestPhase = .harness,
    duration_ns: u64 = 0,
    build_ns: u64 = 0,
    run_ns: u64 = 0,
    exit_code: u32 = 0,
    stderr_capture: ?[]const u8 = null,
    stdout_capture: ?[]const u8 = null,
    message: ?[]const u8 = null,
};

fn serializeResult(fd: posix.fd_t, result: TestResult) void {
    const stderr_data = result.stderr_capture orelse "";
    const stdout_data = result.stdout_capture orelse "";
    const message_data = result.message orelse "";

    const max_capture = 8192;
    const stderr_out = stderr_data[0..@min(stderr_data.len, max_capture)];
    const stdout_out = stdout_data[0..@min(stdout_data.len, max_capture)];
    const message_out = message_data[0..@min(message_data.len, max_capture)];

    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .phase = @intFromEnum(result.phase),
        .duration_ns = result.duration_ns,
        .build_ns = result.build_ns,
        .run_ns = result.run_ns,
        .exit_code = result.exit_code,
        .stderr_len = @intCast(stderr_out.len),
        .stdout_len = @intCast(stdout_out.len),
        .message_len = @intCast(message_out.len),
    };

    harness.writeAll(fd, std.mem.asBytes(&header));
    harness.writeAll(fd, stderr_out);
    harness.writeAll(fd, stdout_out);
    harness.writeAll(fd, message_out);
}

/// Streamed variant for persistent worker mode: writes a `u32` length prefix
/// before the wire bytes so the parent can frame multiple results sharing
/// the same stdout pipe.
fn serializeResultStreamed(fd: posix.fd_t, result: TestResult) void {
    const stderr_data = result.stderr_capture orelse "";
    const stdout_data = result.stdout_capture orelse "";
    const message_data = result.message orelse "";

    const max_capture = 8192;
    const stderr_out = stderr_data[0..@min(stderr_data.len, max_capture)];
    const stdout_out = stdout_data[0..@min(stdout_data.len, max_capture)];
    const message_out = message_data[0..@min(message_data.len, max_capture)];

    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .phase = @intFromEnum(result.phase),
        .duration_ns = result.duration_ns,
        .build_ns = result.build_ns,
        .run_ns = result.run_ns,
        .exit_code = result.exit_code,
        .stderr_len = @intCast(stderr_out.len),
        .stdout_len = @intCast(stdout_out.len),
        .message_len = @intCast(message_out.len),
    };

    const length: u32 = @intCast(@sizeOf(WireHeader) + stderr_out.len + stdout_out.len + message_out.len);
    harness.writeAll(fd, std.mem.asBytes(&length));
    harness.writeAll(fd, std.mem.asBytes(&header));
    harness.writeAll(fd, stderr_out);
    harness.writeAll(fd, stdout_out);
    harness.writeAll(fd, message_out);
}

fn deserializeResult(buf: []const u8, gpa: Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);

    const stderr_capture = harness.readStr(buf, &offset, header.stderr_len, gpa);
    const stdout_capture = harness.readStr(buf, &offset, header.stdout_len, gpa);
    const message = harness.readStr(buf, &offset, header.message_len, gpa);

    return .{
        .status = @enumFromInt(header.status),
        .phase = @enumFromInt(header.phase),
        .duration_ns = header.duration_ns,
        .build_ns = header.build_ns,
        .run_ns = header.run_ns,
        .exit_code = header.exit_code,
        .stderr_capture = stderr_capture,
        .stdout_capture = stdout_capture,
        .message = message,
    };
}

// Child test execution

var roc_binary_path: []const u8 = "";
var glue_roc_binary_path: []const u8 = "";
var glue_execution_mode: GlueExecutionMode = .default;
var project_root_path: []const u8 = "";

const CaseEnv = struct {
    dirs: util.TestProcessDirs,
    env_map: std.process.Environ.Map,

    fn deinit(self: *CaseEnv, allocator: Allocator) void {
        self.env_map.deinit();
        self.dirs.deinit(allocator);
    }
};

fn buildCaseEnv(io: std.Io, allocator: Allocator) CliRunnerError!CaseEnv {
    const dirs = try util.createIsolatedTestDirs(io, allocator);
    errdefer dirs.deinit(allocator);

    const environ: std.process.Environ = if (builtin.os.tag == .windows) .{
        .block = .global,
    } else blk: {
        const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
        break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    };
    var env_map = try environ.createMap(allocator);
    errdefer env_map.deinit();
    try env_map.put("ROC_CACHE_DIR", dirs.roc_cache_dir);
    try env_map.put("XDG_CACHE_HOME", dirs.roc_cache_dir);
    try env_map.put("ZIG_LOCAL_CACHE_DIR", dirs.zig_local_cache_dir);
    try util.putIsolatedTempEnv(&env_map, dirs.temp_dir);

    return .{
        .dirs = dirs,
        .env_map = env_map,
    };
}

fn deleteIfExists(io: std.Io, path: []const u8) CliRunnerError!void {
    std.Io.Dir.cwd().deleteFile(io, path) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };
}

fn deleteOutputArtifacts(io: std.Io, allocator: Allocator, output_name: []const u8) CliRunnerError!void {
    try deleteIfExists(io, output_name);

    if (comptime builtin.os.tag == .windows) {
        const exe_name = try std.fmt.allocPrint(allocator, "{s}.exe", .{output_name});
        defer allocator.free(exe_name);
        try deleteIfExists(io, exe_name);

        const pdb_name = try std.fmt.allocPrint(allocator, "{s}.pdb", .{output_name});
        defer allocator.free(pdb_name);
        try deleteIfExists(io, pdb_name);
    }
}

fn absoluteFromProjectRoot(allocator: Allocator, path: []const u8) CliRunnerError![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        return allocator.dupe(u8, path);
    }
    return std.fs.path.join(allocator, &.{ project_root_path, path });
}

fn skipReason(skip: Skip) ?[]const u8 {
    return switch (skip) {
        .never => null,
        .always => |reason| reason,
        .windows => |reason| if (builtin.os.tag == .windows) reason else null,
    };
}

fn runSingleTest(io: std.Io, allocator: Allocator, spec: CliCase, timeout_ms: u64) TestResult {
    if (skipReason(spec.skip)) |reason| {
        var timer = harness.Timer.start() catch return .{ .status = .skip, .phase = .setup, .message = reason };
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = reason };
    }

    return switch (spec.body) {
        .platform => runPlatformCase(io, allocator, spec, timeout_ms),
        .command => |command| runCommandCase(io, allocator, command, timeout_ms),
        .custom => |custom| runCustomCase(io, allocator, spec, custom, timeout_ms),
        .glue_matrix => |matrix| runGlueMatrixCase(io, allocator, matrix, timeout_ms),
    };
}

fn runPlatformCase(io: std.Io, allocator: Allocator, spec: CliCase, timeout_ms: u64) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .setup, .message = "no clock" };
    const platform = spec.body.platform;
    const backend = spec.backend orelse
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "platform case missing backend" };

    const dirs = util.createIsolatedTestDirs(io, allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to create test directories" };
    defer dirs.deinit(allocator);

    const roc_file = absoluteFromProjectRoot(allocator, platform.roc_file) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to resolve Roc file path" };

    const output_name = std.fs.path.join(allocator, &.{ dirs.work_dir, "app" }) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate output path" };

    deleteOutputArtifacts(io, allocator, output_name) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "failed to remove stale output file: {}", .{err}) catch "failed to remove stale output file";
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = msg };
    };

    const environ: std.process.Environ = if (@import("builtin").os.tag == .windows) .{
        .block = .global,
    } else blk: {
        const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
        break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    };
    var env_map = environ.createMap(allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to copy environment" };
    defer env_map.deinit();
    env_map.put("ROC_CACHE_DIR", dirs.roc_cache_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set ROC_CACHE_DIR" };
    env_map.put("XDG_CACHE_HOME", dirs.roc_cache_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set XDG_CACHE_HOME" };
    env_map.put("ZIG_LOCAL_CACHE_DIR", dirs.zig_local_cache_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set ZIG_LOCAL_CACHE_DIR" };
    // Isolate the temp dir so roc's background temp-cleanup thread cannot race
    // other concurrent roc processes on the shared system temp dir. This is the
    // fix for the non-deterministic interpreter-backend access-violation flake.
    util.putIsolatedTempEnv(&env_map, dirs.temp_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set isolated temp dir" };

    const result = switch (backend) {
        .interpreter => runInterpreterTest(io, allocator, backend, platform, roc_file, &env_map, dirs.work_dir, &timer, timeout_ms),
        .dev, .size, .speed => runCompiledTest(io, allocator, backend, platform, roc_file, output_name, &env_map, dirs.work_dir, &timer, timeout_ms),
    };

    if (result.status == .pass) {
        util.cleanupTestWorkDir(io, dirs.work_dir);
        return result;
    }
    return addPreservedWorkDirMessage(allocator, result, dirs.work_dir);
}

fn runInterpreterTest(
    io: std.Io,
    allocator: Allocator,
    backend: OptMode,
    platform: PlatformCase,
    roc_file: []const u8,
    env_map: *const std.process.Environ.Map,
    work_dir: []const u8,
    timer: *harness.Timer,
    timeout_ms: u64,
) TestResult {
    const opt_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{backend.cliName()}) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate opt arg" };

    var argv_buf: [5][]const u8 = undefined;
    var argc: usize = 0;
    argv_buf[argc] = roc_binary_path;
    argc += 1;
    argv_buf[argc] = "run";
    argc += 1;
    argv_buf[argc] = opt_arg;
    argc += 1;
    switch (platform.test_kind) {
        .native_run => {},
        .io_spec => |io_spec| {
            const test_arg = std.fmt.allocPrint(allocator, "--test={s}", .{io_spec}) catch
                return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate IO spec arg" };
            argv_buf[argc] = test_arg;
            argc += 1;
        },
    }
    argv_buf[argc] = roc_file;
    argc += 1;

    var run_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .message = "no clock" };
    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before run command started");
    const run_result = util.runChildWithTimeout(io, allocator, argv_buf[0..argc], .{
        .cwd = work_dir,
        .env_map = env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = child_timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
        return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .run_ns = run_timer.read(), .message = msg };
    };
    const run_ns = run_timer.read();
    return resultFromProcess(run_result, timer, .run, 0, run_ns, "run failed");
}

fn runCompiledTest(
    io: std.Io,
    allocator: Allocator,
    backend: OptMode,
    platform: PlatformCase,
    roc_file: []const u8,
    output_name: []const u8,
    env_map: *const std.process.Environ.Map,
    work_dir: []const u8,
    timer: *harness.Timer,
    timeout_ms: u64,
) TestResult {
    const output_arg = std.fmt.allocPrint(allocator, "--output={s}", .{output_name}) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate output arg" };
    const opt_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{backend.cliName()}) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate opt arg" };

    const build_argv = &[_][]const u8{ roc_binary_path, "build", output_arg, opt_arg, roc_file };

    var build_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .build, .duration_ns = timer.read(), .message = "no clock" };
    const build_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .build, "case timeout exhausted before build command started");
    const build_result = util.runChildWithTimeout(io, allocator, build_argv, .{
        .cwd = work_dir,
        .env_map = env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = build_timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "build spawn error: {}", .{err}) catch "build spawn error";
        return .{ .status = .infra_error, .phase = .build, .duration_ns = timer.read(), .build_ns = build_timer.read(), .message = msg };
    };
    const build_ns = build_timer.read();
    const expected_build_stderr = expectedBuildStderrForBackend(backend, platform.expected_build_stderr_contains);
    if (processTimedOut(build_result.stderr) or hasMemoryErrors(build_result.stderr) != null) {
        return resultFromProcess(build_result, timer, .build, build_ns, 0, "build failed");
    }
    if (!buildSucceededOrExpectedDiagnostics(build_result, expected_build_stderr)) {
        return resultFromProcess(build_result, timer, .build, build_ns, 0, "build failed");
    }
    if (missingExpectedStderr(build_result.stderr, expected_build_stderr)) |needle| {
        const msg = std.fmt.allocPrint(allocator, "missing expected build stderr: {s}", .{needle}) catch "missing expected build stderr";
        return .{
            .status = .build_failed,
            .phase = .build,
            .duration_ns = timer.read(),
            .build_ns = build_ns,
            .exit_code = exitCode(build_result.term),
            .stderr_capture = build_result.stderr,
            .stdout_capture = build_result.stdout,
            .message = msg,
        };
    }

    if (!builtOutputExists(io, allocator, output_name)) {
        return .{ .status = .build_failed, .phase = .build, .duration_ns = timer.read(), .build_ns = build_ns, .message = "build succeeded but output file was not created" };
    }

    var run_argv_buf: [3][]const u8 = undefined;
    var argc: usize = 0;
    run_argv_buf[argc] = output_name;
    argc += 1;
    switch (platform.test_kind) {
        .native_run => {},
        .io_spec => |io_spec| {
            run_argv_buf[argc] = "--test";
            argc += 1;
            run_argv_buf[argc] = io_spec;
            argc += 1;
        },
    }

    var run_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .build_ns = build_ns, .message = "no clock" };
    const run_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before compiled output started");
    const run_result = util.runChildWithTimeout(io, allocator, run_argv_buf[0..argc], .{
        .cwd = work_dir,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = run_timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
        return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .build_ns = build_ns, .run_ns = run_timer.read(), .message = msg };
    };
    const run_ns = run_timer.read();
    return resultFromProcess(run_result, timer, .run, build_ns, run_ns, "run failed");
}

fn builtOutputExists(io: std.Io, allocator: Allocator, output_name: []const u8) bool {
    std.Io.Dir.cwd().access(io, output_name, .{}) catch {
        if (builtin.os.tag == .windows) {
            const exe_name = std.fmt.allocPrint(allocator, "{s}.exe", .{output_name}) catch return false;
            defer allocator.free(exe_name);
            std.Io.Dir.cwd().access(io, exe_name, .{}) catch return false;
        } else {
            return false;
        }
    };
    return true;
}

fn resultFromProcess(
    result: std.process.RunResult,
    timer: *harness.Timer,
    phase: TestPhase,
    build_ns: u64,
    run_ns: u64,
    fail_msg: []const u8,
) TestResult {
    if (processTimedOut(result.stderr)) {
        return .{
            .status = .timeout,
            .phase = phase,
            .duration_ns = timer.read(),
            .build_ns = build_ns,
            .run_ns = run_ns,
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = "child command timed out",
        };
    }
    if (hasMemoryErrors(result.stderr)) |mem_msg| {
        return .{
            .status = if (phase == .build) .build_failed else .run_failed,
            .phase = phase,
            .duration_ns = timer.read(),
            .build_ns = build_ns,
            .run_ns = run_ns,
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = mem_msg,
        };
    }
    switch (result.term) {
        .exited => |code| {
            if (code == 0) {
                return .{ .status = .pass, .phase = phase, .duration_ns = timer.read(), .build_ns = build_ns, .run_ns = run_ns };
            }
            return .{
                .status = if (phase == .build) .build_failed else .run_failed,
                .phase = phase,
                .duration_ns = timer.read(),
                .build_ns = build_ns,
                .run_ns = run_ns,
                .exit_code = @intCast(code),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = fail_msg,
            };
        },
        .signal => {
            return .{
                .status = .crash,
                .phase = phase,
                .duration_ns = timer.read(),
                .build_ns = build_ns,
                .run_ns = run_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = fail_msg,
            };
        },
        else => {
            return .{
                .status = .crash,
                .phase = phase,
                .duration_ns = timer.read(),
                .build_ns = build_ns,
                .run_ns = run_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = fail_msg,
            };
        },
    }
}

fn processSucceeded(term: std.process.Child.Term) bool {
    return switch (term) {
        .exited => |code| code == 0,
        else => false,
    };
}

fn expectedBuildStderrForBackend(backend: OptMode, expected_stderr_contains: []const []const u8) []const []const u8 {
    return switch (backend) {
        .size, .speed => expected_stderr_contains,
        .interpreter, .dev => &.{},
    };
}

fn buildSucceededOrExpectedDiagnostics(result: std.process.RunResult, expected_stderr_contains: []const []const u8) bool {
    if (processSucceeded(result.term)) return true;
    if (expected_stderr_contains.len == 0) return false;

    return switch (result.term) {
        .exited => |code| code == 2,
        else => false,
    };
}

fn missingExpectedStderr(stderr: []const u8, expected_stderr_contains: []const []const u8) ?[]const u8 {
    for (expected_stderr_contains) |needle| {
        if (std.mem.find(u8, stderr, needle) == null) return needle;
    }

    return null;
}

fn processTimedOut(stderr: []const u8) bool {
    return std.mem.find(u8, stderr, "child command timed out") != null;
}

fn statusLabel(status: TestStatus) []const u8 {
    return switch (status) {
        .pass => "passed",
        .build_failed => "build failed",
        .run_failed => "run failed",
        .timeout => "timed out",
        .crash => "crashed",
        .infra_error => "infrastructure error",
        .skip => "skipped",
    };
}

fn phaseLabel(phase: TestPhase) []const u8 {
    return switch (phase) {
        .setup => "setup",
        .build => "build",
        .run => "run",
        .cleanup => "cleanup",
        .harness => "harness",
    };
}

fn addPreservedWorkDirMessage(allocator: Allocator, result: TestResult, work_dir: []const u8) TestResult {
    var updated = result;
    const prefix = result.message orelse statusLabel(result.status);
    updated.message = std.fmt.allocPrint(allocator, "{s}; preserved work dir: {s}", .{ prefix, work_dir }) catch result.message;
    return updated;
}

fn exitCode(term: std.process.Child.Term) u32 {
    return switch (term) {
        .exited => |code| @intCast(code),
        .signal => |sig| @as(u32, @intFromEnum(sig)) | 0x80000000,
        else => 0xFFFFFFFF,
    };
}

fn hasMemoryErrors(stderr: []const u8) ?[]const u8 {
    if (std.mem.find(u8, stderr, "error(gpa):") != null) return "memory error detected";
    if (std.mem.find(u8, stderr, "allocation(s) not freed") != null) return "memory leak detected";
    return null;
}

fn runCommandCase(
    io: std.Io,
    allocator: Allocator,
    command: CommandCase,
    timeout_ms: u64,
) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .setup, .message = "no clock" };
    var env = buildCaseEnv(io, allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to create test environment" };
    defer env.deinit(allocator);

    var run_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .message = "no clock" };
    const child_timeout_ms = childCommandTimeoutMs(&timer, timeout_ms) orelse
        return addPreservedWorkDirMessage(allocator, timeoutFailure(allocator, &timer, .run, "case timeout exhausted before command started"), env.dirs.work_dir);
    const result = runRocInEnv(io, allocator, &env, command.args, command.roc_file, command.file_path_mode, command.stdin, child_timeout_ms) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
        return addPreservedWorkDirMessage(allocator, .{
            .status = .infra_error,
            .phase = .run,
            .duration_ns = timer.read(),
            .run_ns = run_timer.read(),
            .message = msg,
        }, env.dirs.work_dir);
    };
    const run_ns = run_timer.read();

    if (checkCommandExpectation(allocator, result, command)) |message| {
        return addPreservedWorkDirMessage(allocator, .{
            .status = if (processTimedOut(result.stderr)) .timeout else .run_failed,
            .phase = .run,
            .duration_ns = timer.read(),
            .run_ns = run_ns,
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = message,
        }, env.dirs.work_dir);
    }

    util.cleanupTestWorkDir(io, env.dirs.work_dir);
    return .{ .status = .pass, .phase = .run, .duration_ns = timer.read(), .run_ns = run_ns };
}

fn runRocInEnv(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    args: []const []const u8,
    roc_file: ?[]const u8,
    file_path_mode: FilePathMode,
    stdin: ?[]const u8,
    timeout_ms: u64,
) CliRunnerError!std.process.RunResult {
    const argv = try buildRocArgv(allocator, args, roc_file, file_path_mode);
    return util.runChildWithTimeout(io, allocator, argv, .{
        .cwd = project_root_path,
        .env_map = &env.env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .stdin = stdin,
        .timeout_ms = timeout_ms,
    });
}

fn runRawInEnv(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    argv: []const []const u8,
    cwd: []const u8,
    stdin: ?[]const u8,
    timeout_ms: u64,
) CliRunnerError!std.process.RunResult {
    return util.runChildWithTimeout(io, allocator, argv, .{
        .cwd = cwd,
        .env_map = &env.env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .stdin = stdin,
        .timeout_ms = timeout_ms,
    });
}

fn buildRocArgv(
    allocator: Allocator,
    args: []const []const u8,
    roc_file: ?[]const u8,
    file_path_mode: FilePathMode,
) CliRunnerError![]const []const u8 {
    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    const is_glue_command = args.len > 0 and std.mem.eql(u8, args[0], "glue");
    try argv.append(allocator, if (is_glue_command) glue_roc_binary_path else roc_binary_path);
    for (args, 0..) |arg, idx| {
        try argv.append(allocator, arg);
        if (idx == 0 and is_glue_command) {
            if (glue_execution_mode.optArg()) |opt_arg| {
                if (!glueArgsContainOpt(args)) {
                    try argv.append(allocator, opt_arg);
                }
            }
        }
    }
    if (roc_file) |path| {
        const resolved = switch (file_path_mode) {
            .absolute => try absoluteFromProjectRoot(allocator, path),
            .relative => path,
        };
        try argv.append(allocator, resolved);
    }
    return try argv.toOwnedSlice(allocator);
}

fn glueArgsContainOpt(args: []const []const u8) bool {
    for (args) |arg| {
        if (std.mem.startsWith(u8, arg, "--opt")) return true;
    }
    return false;
}

fn checkCommandExpectation(
    allocator: Allocator,
    result: std.process.RunResult,
    command: CommandCase,
) ?[]const u8 {
    if (hasMemoryErrors(result.stderr)) |message| return message;
    if (processTimedOut(result.stderr)) return "child command timed out";

    if (checkExitExpectation(allocator, result, command.exit)) |message| return message;

    if (command.stdout_exact) |expected| {
        if (!std.mem.eql(u8, expected, result.stdout)) {
            return std.fmt.allocPrint(allocator, "stdout mismatch: expected {d} bytes, got {d}", .{ expected.len, result.stdout.len }) catch "stdout mismatch";
        }
    }
    if (command.stderr_exact) |expected| {
        if (!std.mem.eql(u8, expected, result.stderr)) {
            return std.fmt.allocPrint(allocator, "stderr mismatch: expected {d} bytes, got {d}", .{ expected.len, result.stderr.len }) catch "stderr mismatch";
        }
    }
    if (command.stdout_min_len) |min_len| {
        if (result.stdout.len < min_len) {
            return std.fmt.allocPrint(allocator, "stdout too short: expected at least {d} bytes, got {d}", .{ min_len, result.stdout.len }) catch "stdout too short";
        }
    }
    if (command.stderr_min_len) |min_len| {
        if (result.stderr.len < min_len) {
            return std.fmt.allocPrint(allocator, "stderr too short: expected at least {d} bytes, got {d}", .{ min_len, result.stderr.len }) catch "stderr too short";
        }
    }
    for (command.contains) |needle| {
        if (std.mem.find(u8, streamBytes(result, needle.stream), needle.text) == null) {
            return std.fmt.allocPrint(allocator, "{s} did not contain expected text: {s}", .{ streamLabel(needle.stream), needle.text }) catch "missing expected output";
        }
    }
    for (command.not_contains) |needle| {
        if (std.mem.find(u8, streamBytes(result, needle.stream), needle.text) != null) {
            return std.fmt.allocPrint(allocator, "{s} contained forbidden text: {s}", .{ streamLabel(needle.stream), needle.text }) catch "forbidden output";
        }
    }
    for (command.occurrences) |expected| {
        const actual = std.mem.count(u8, streamBytes(result, expected.stream), expected.text);
        if (actual != expected.count) {
            return std.fmt.allocPrint(allocator, "{s} contained text {s} {d} time(s), expected {d}", .{
                streamLabel(expected.stream),
                expected.text,
                actual,
                expected.count,
            }) catch "unexpected output count";
        }
    }
    for (command.contains_any) |set| {
        var matched = false;
        for (set.needles) |needle| {
            if (std.mem.find(u8, streamBytes(result, needle.stream), needle.text) != null) {
                matched = true;
                break;
            }
        }
        if (!matched) return "output did not contain any expected text";
    }
    return null;
}

fn checkExitExpectation(
    allocator: Allocator,
    result: std.process.RunResult,
    expected: ExitExpectation,
) ?[]const u8 {
    return switch (expected) {
        .success => switch (result.term) {
            .exited => |code| if (code == 0) null else std.fmt.allocPrint(allocator, "expected exit code 0, got {d}", .{code}) catch "unexpected exit code",
            .signal => "process terminated by signal",
            else => "process terminated abnormally",
        },
        .failure => switch (result.term) {
            .exited => |code| if (code != 0) null else "expected non-zero exit code, got 0",
            .signal => "process terminated by signal",
            else => "process terminated abnormally",
        },
        .code => |expected_code| switch (result.term) {
            .exited => |code| if (code == expected_code) null else std.fmt.allocPrint(allocator, "expected exit code {d}, got {d}", .{ expected_code, code }) catch "unexpected exit code",
            .signal => "process terminated by signal",
            else => "process terminated abnormally",
        },
        .not_panic => {
            const did_panic = result.term == .signal or
                (result.term == .exited and result.term.exited == 134);
            return if (did_panic) "process panicked or aborted" else null;
        },
        .any => null,
    };
}

fn streamBytes(result: std.process.RunResult, stream: Stream) []const u8 {
    return switch (stream) {
        .stdout => result.stdout,
        .stderr => result.stderr,
    };
}

fn streamLabel(stream: Stream) []const u8 {
    return switch (stream) {
        .stdout => "stdout",
        .stderr => "stderr",
    };
}

fn runCustomCase(
    io: std.Io,
    allocator: Allocator,
    spec: CliCase,
    custom: CustomCase,
    timeout_ms: u64,
) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .setup, .message = "no clock" };
    var env = buildCaseEnv(io, allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to create test environment" };
    defer env.deinit(allocator);

    const result: ?TestResult = switch (custom) {
        .noop => null,
        .default_app_all_syntax_checked_cache => customDefaultAppAllSyntaxCheckedCache(io, allocator, &env, &timer, timeout_ms),
        .cli_cache_roots_distinct => customCliCacheRootsDistinct(io, allocator, &timer),
        .watch_inputs_reject_absolute_import => customWatchInputsRejectAbsoluteImport(io, allocator, &env, &timer, timeout_ms),
        .watch_completed_run_refresh_reruns => customWatchCompletedRunRefreshReruns(io, allocator, &env, &timer, timeout_ms),
        .hot_reload_dev_shim => customHotReloadDevShim(io, allocator, &env, &timer, timeout_ms),
        .hot_reload_model_boundary => customHotReloadModelBoundary(io, allocator, &env, &timer, timeout_ms),
        .hot_reload_default_app => customHotReloadDefaultApp(io, allocator, &env, &timer, timeout_ms),
        .platform_requires_checker_diagnostics => customPlatformRequiresCheckerDiagnostics(io, allocator, &env, &timer, timeout_ms),
        .generated_graph_1_1 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 1, .symbols_per_file = 1 }),
        .generated_graph_5_5 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 5, .symbols_per_file = 5 }),
        .generated_graph_2_100 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 2, .symbols_per_file = 100 }),
        .generated_graph_200_5 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 200, .symbols_per_file = 5 }),
        .list_builtin_inlined => customListBuiltinInlined(io, allocator, &env, &timer, timeout_ms),
        .default_platform_linux_disassembly => customDefaultPlatformLinuxDisassembly(io, allocator, &env, &timer, timeout_ms),
        .default_platform_build_x64glibc => customDefaultPlatformBuild(io, allocator, &env, &timer, timeout_ms, .x64glibc),
        .default_platform_build_arm64glibc => customDefaultPlatformBuild(io, allocator, &env, &timer, timeout_ms, .arm64glibc),
        .default_platform_build_wasm32 => customDefaultPlatformBuild(io, allocator, &env, &timer, timeout_ms, .wasm32),
        .default_platform_wasm32_archive_reproducible => customDefaultPlatformWasm32ArchiveReproducible(io, allocator, &env, &timer, timeout_ms),
        .macos_output_basename_reproducible => customMacosOutputBasenameReproducible(io, allocator, &env, &timer, timeout_ms),
        .default_platform_crash_x64musl => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .x64musl, .crash),
        .default_platform_crash_arm64musl => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .arm64musl, .crash),
        .default_platform_crash_x64mac => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .x64mac, .crash),
        .default_platform_crash_arm64mac => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .arm64mac, .crash),
        .default_platform_crash_x64win => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .x64win, .crash),
        .default_platform_crash_arm64win => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .arm64win, .crash),
        .default_platform_stack_overflow_x64musl => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .x64musl, .stack_overflow),
        .default_platform_stack_overflow_arm64musl => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .arm64musl, .stack_overflow),
        .default_platform_stack_overflow_x64mac => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .x64mac, .stack_overflow),
        .default_platform_stack_overflow_arm64mac => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .arm64mac, .stack_overflow),
        .default_platform_stack_overflow_x64win => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .x64win, .stack_overflow),
        .default_platform_stack_overflow_arm64win => customDefaultPlatformDebugBacktrace(io, allocator, &env, &timer, timeout_ms, .arm64win, .stack_overflow),
        .fmt_reformats_file => customFmtReformatsFile(io, allocator, &env, &timer, timeout_ms),
        .fmt_does_not_change_file => customFmtDoesNotChangeFile(io, allocator, &env, &timer, timeout_ms),
        .fmt_stdin_formats => customFmtStdin(io, allocator, &env, &timer, timeout_ms, false),
        .fmt_stdin_does_not_change => customFmtStdin(io, allocator, &env, &timer, timeout_ms, true),
        .build_int_interpreter_creates_output => customBuildIntCreatesOutput(io, allocator, &env, &timer, timeout_ms),
        .build_int_interpreter_output_runs => customBuildIntOutputRuns(io, allocator, &env, &timer, timeout_ms, .interpreter),
        .build_int_dev_output_runs => customBuildIntOutputRuns(io, allocator, &env, &timer, timeout_ms, .dev),
        .build_glibc_target_non_linux_error => customGlibcTargetNonLinux(io, allocator, &env, &timer, timeout_ms),
        .build_windows_shared_library => customWindowsSharedLibrary(io, allocator, &env, &timer, timeout_ms),
        .cache_passing_results => customCachePassingResults(io, allocator, &env, &timer, timeout_ms, spec.backend orelse .interpreter),
        .cache_failing_results => customCacheFailingResults(io, allocator, &env, &timer, timeout_ms, spec.backend orelse .interpreter),
        .cache_invalidated_by_source_change => customCacheInvalidated(io, allocator, &env, &timer, timeout_ms, spec.backend orelse .interpreter),
        .verbose_works_from_cache => customVerboseWorksFromCache(io, allocator, &env, &timer, timeout_ms, spec.backend orelse .interpreter),
        .verbose_caches_failure_reports => customVerboseCachesFailureReports(io, allocator, &env, &timer, timeout_ms, spec.backend orelse .interpreter),
        .non_verbose_caches_verbose_reports => customNonVerboseCachesVerboseReports(io, allocator, &env, &timer, timeout_ms, spec.backend orelse .interpreter),
        .verbose_and_non_verbose_failure_format_match => customVerboseAndNonVerboseFailureFormatMatch(io, allocator, &timer, timeout_ms, spec.backend orelse .interpreter),
        .build_warning_interpreter => customBuildWarningInterpreter(io, allocator, &env, &timer, timeout_ms),
        .issue_9392_deterministic_no_cache => customIssue9392Deterministic(io, allocator, &env, &timer, timeout_ms),
        .build_issue_9435_hosted_nominal_return => customBuildIssue9435(io, allocator, &env, &timer, timeout_ms),
        .bundle_complex_package => customBundleComplexPackage(io, allocator, &env, &timer, timeout_ms),
        .glue_debug => customGlueDebug(io, allocator, &env, &timer, timeout_ms),
        .glue_debug_interpreter => customGlueDebugInterpreter(io, allocator, &env, &timer, timeout_ms),
        .glue_c_header => customGlueCHeader(io, allocator, &env, &timer, timeout_ms),
        .glue_c_header_compiles => customGlueCHeaderCompiles(io, allocator, &env, &timer, timeout_ms),
        .glue_zig => customGlueZig(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_compiles => customGlueZigCompiles(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_native_wasm_layouts => customGlueZigNativeWasmLayouts(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_opaque_box => customGlueZigOpaqueBox(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_box_payload_alignment => customGlueZigBoxPayloadAlignment(io, allocator, &env, &timer, timeout_ms),
        .glue_rust => customGlueRust(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_duplicate_tag_unions => customGlueZigDuplicateTagUnions(io, allocator, &env, &timer, timeout_ms),
        .glue_rust_duplicate_tag_unions => customGlueRustDuplicateTagUnions(io, allocator, &env, &timer, timeout_ms),
        .glue_rust_box_payload_alignment => customGlueRustBoxPayloadAlignment(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_bang_record_fields => customGlueZigBangRecordFieldNames(io, allocator, &env, &timer, timeout_ms),
        .glue_package_nominal_api_alias => customGluePackageNominalApiAlias(io, allocator, &env, &timer, timeout_ms),
        .glue_c_tests => customGlueCTests(io, allocator, &env, &timer, timeout_ms),
    };

    if (result) |failure| {
        return addPreservedWorkDirMessage(allocator, failure, env.dirs.work_dir);
    }

    util.cleanupTestWorkDir(io, env.dirs.work_dir);
    return .{ .status = .pass, .phase = .run, .duration_ns = timer.read(), .run_ns = timer.read() };
}

fn customFailure(allocator: Allocator, timer: *harness.Timer, comptime fmt: []const u8, args: anytype) TestResult {
    const message = std.fmt.allocPrint(allocator, fmt, args) catch "custom case failed";
    return .{ .status = .run_failed, .phase = .run, .duration_ns = timer.read(), .run_ns = timer.read(), .message = message };
}

fn customInfraFailure(allocator: Allocator, timer: *harness.Timer, comptime fmt: []const u8, args: anytype) TestResult {
    const message = std.fmt.allocPrint(allocator, fmt, args) catch "custom case infrastructure failed";
    return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .run_ns = timer.read(), .message = message };
}

fn timeoutFailure(allocator: Allocator, timer: *harness.Timer, phase: TestPhase, message: []const u8) TestResult {
    return .{
        .status = .timeout,
        .phase = phase,
        .duration_ns = timer.read(),
        .run_ns = timer.read(),
        .message = allocator.dupe(u8, message) catch message,
    };
}

fn childCommandTimeoutMs(timer: *harness.Timer, timeout_ms: u64) ?u64 {
    if (timeout_ms == 0) return 0;

    const elapsed_ms = timer.read() / std.time.ns_per_ms;
    if (elapsed_ms >= timeout_ms) return null;

    const remaining_ms = timeout_ms - elapsed_ms;
    if (remaining_ms > child_command_timeout_reserve_ms) {
        return remaining_ms - child_command_timeout_reserve_ms;
    }
    return remaining_ms;
}

fn failureFromRun(allocator: Allocator, timer: *harness.Timer, result: std.process.RunResult, message: []const u8) TestResult {
    return .{
        .status = if (processTimedOut(result.stderr)) .timeout else .run_failed,
        .phase = .run,
        .duration_ns = timer.read(),
        .run_ns = timer.read(),
        .exit_code = exitCode(result.term),
        .stderr_capture = result.stderr,
        .stdout_capture = result.stdout,
        .message = allocator.dupe(u8, message) catch message,
    };
}

fn runRocAndCheck(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    command: CommandCase,
) ?TestResult {
    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before command started");
    const result = runRocInEnv(io, allocator, env, command.args, command.roc_file, command.file_path_mode, command.stdin, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "run spawn error: {}", .{err});

    if (checkCommandExpectation(allocator, result, command)) |message| {
        return failureFromRun(allocator, timer, result, message);
    }

    return null;
}

fn runRawAndCheck(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    argv: []const []const u8,
    cwd: []const u8,
    command: CommandCase,
) ?TestResult {
    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before command started");
    const result = runRawInEnv(io, allocator, env, argv, cwd, command.stdin, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "run spawn error: {}", .{err});

    if (checkCommandExpectation(allocator, result, command)) |message| {
        return failureFromRun(allocator, timer, result, message);
    }

    return null;
}

fn backendOptArg(allocator: Allocator, backend: OptMode) CliRunnerError![]const u8 {
    return std.fmt.allocPrint(allocator, "--opt={s}", .{backend.cliName()});
}

fn outputArg(allocator: Allocator, path: []const u8) CliRunnerError![]const u8 {
    return std.fmt.allocPrint(allocator, "--output={s}", .{path});
}

fn fileExistsWithSize(io: std.Io, path: []const u8) CliRunnerError!u64 {
    const stat = try std.Io.Dir.cwd().statFile(io, path, .{});
    return stat.size;
}

fn customCliCacheRootsDistinct(io: std.Io, allocator: Allocator, timer: *harness.Timer) ?TestResult {
    const first = util.createIsolatedTestCacheDirs(io, allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to create first cache dirs: {}", .{err});
    defer first.deinit(allocator);
    const second = util.createIsolatedTestCacheDirs(io, allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to create second cache dirs: {}", .{err});
    defer second.deinit(allocator);

    if (std.mem.eql(u8, first.roc_cache_dir, second.roc_cache_dir)) {
        return customFailure(allocator, timer, "ROC_CACHE_DIR paths were not distinct", .{});
    }
    if (std.mem.eql(u8, first.zig_local_cache_dir, second.zig_local_cache_dir)) {
        return customFailure(allocator, timer, "ZIG_LOCAL_CACHE_DIR paths were not distinct", .{});
    }

    var first_dir = std.Io.Dir.openDirAbsolute(io, first.roc_cache_dir, .{}) catch |err|
        return customInfraFailure(allocator, timer, "failed to open first cache dir: {}", .{err});
    first_dir.close(io);
    var second_dir = std.Io.Dir.openDirAbsolute(io, second.roc_cache_dir, .{}) catch |err|
        return customInfraFailure(allocator, timer, "failed to open second cache dir: {}", .{err});
    second_dir.close(io);
    return null;
}

fn customWatchInputsRejectAbsoluteImport(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "absolute_file_import.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate app path: {}", .{err});
    defer allocator.free(app_path);

    const watch_inputs_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "watch.inputs" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate watch-input path: {}", .{err});
    defer allocator.free(watch_inputs_path);

    const rejected_import_path = "/tmp/roc-watch-absolute-import-data.txt";
    const app_source = std.fmt.allocPrint(
        allocator,
        "import \"{s}\" as data : Str\n\nmain = data\n",
        .{rejected_import_path},
    ) catch |err| return customInfraFailure(allocator, timer, "failed to render app source: {}", .{err});
    defer allocator.free(app_source);

    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = app_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write app: {}", .{err});

    const watch_inputs_arg = std.fmt.allocPrint(allocator, "--watch-inputs-file={s}", .{watch_inputs_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate watch-input arg: {}", .{err});
    defer allocator.free(watch_inputs_arg);

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before command started");
    const result = runRawInEnv(
        io,
        allocator,
        env,
        &.{ roc_binary_path, "check", "--no-cache", watch_inputs_arg, app_path },
        project_root_path,
        null,
        child_timeout_ms,
    ) catch |err| return customInfraFailure(allocator, timer, "roc check spawn error: {}", .{err});

    if (checkExitExpectation(allocator, result, .failure)) |message| {
        return failureFromRun(allocator, timer, result, message);
    }
    if (std.mem.find(u8, result.stderr, "ABSOLUTE FILE IMPORT") == null) {
        return failureFromRun(allocator, timer, result, "stderr did not contain ABSOLUTE FILE IMPORT");
    }

    const watch_inputs = std.Io.Dir.cwd().readFileAlloc(io, watch_inputs_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read watch-input file: {}", .{err});
    defer allocator.free(watch_inputs);

    if (std.mem.find(u8, watch_inputs, app_path) == null) {
        return customFailure(allocator, timer, "watch-input file did not contain root source path", .{});
    }
    if (std.mem.find(u8, watch_inputs, rejected_import_path) != null) {
        return customFailure(allocator, timer, "watch-input file contained rejected absolute import path", .{});
    }

    return null;
}

const watch_refresh_initial_source =
    \\import Dep
    \\
    \\main! = |_| {
    \\    _ = Dep.value
    \\    Ok({})
    \\}
    \\
;

const watch_refresh_dep_initial_source =
    \\Dep :: [].{
    \\    value : Str
    \\    value = "before"
    \\}
    \\
;

const watch_refresh_wrapper_source =
    \\#!/usr/bin/env bash
    \\real="./zig-out/bin/roc"
    \\is_child=0
    \\for arg in "$@"; do
    \\    case "$arg" in
    \\        --watch-inputs-file=*) is_child=1 ;;
    \\    esac
    \\done
    \\
    \\if [ "$is_child" = "0" ]; then
    \\    exec -a "$0" "$real" "$@"
    \\fi
    \\
    \\"$real" "$@"
    \\status=$?
    \\app="${@: -1}"
    \\dep="${app%/*}/Dep.roc"
    \\marker="${dep}.watch-refresh-done"
    \\if [ ! -e "$marker" ]; then
    \\    : > "$marker"
    \\    cat > "$dep" <<'ROC_WATCH_REFRESH_SOURCE'
    \\Dep :: [].{
    \\    value : Str
    \\    value = "after"
    \\}
    \\ROC_WATCH_REFRESH_SOURCE
    \\fi
    \\exit "$status"
    \\
;

fn customWatchCompletedRunRefreshReruns(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "watch_refresh.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate watch refresh app path: {}", .{err});
    defer allocator.free(app_path);

    const dep_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "Dep.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate watch refresh dependency path: {}", .{err});
    defer allocator.free(dep_path);

    const wrapper_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "roc-watch-wrapper.sh" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate watch wrapper path: {}", .{err});
    defer allocator.free(wrapper_path);

    const marker_path = std.fmt.allocPrint(allocator, "{s}.watch-refresh-done", .{dep_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate watch marker path: {}", .{err});
    defer allocator.free(marker_path);

    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = watch_refresh_initial_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write watch refresh app: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = dep_path, .data = watch_refresh_dep_initial_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write watch refresh dependency: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = wrapper_path, .data = watch_refresh_wrapper_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write watch wrapper: {}", .{err});

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{ "chmod", "755", wrapper_path }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    const remaining_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc check --watch started");
    const child_timeout_ms = @min(remaining_timeout_ms, 8_000);
    const result = runRawInEnv(
        io,
        allocator,
        env,
        &.{ wrapper_path, "check", "--watch", "--no-cache", app_path },
        project_root_path,
        null,
        child_timeout_ms,
    ) catch |err| return customInfraFailure(allocator, timer, "roc check --watch wrapper spawn error: {}", .{err});

    if (!processTimedOut(result.stderr)) {
        return customFailure(allocator, timer, "roc check --watch wrapper exited before timeout", .{});
    }
    if (countOccurrences(result.stdout, "No errors found") < 2) {
        return customFailure(allocator, timer, "watch output did not contain two successful check runs", .{});
    }
    if (std.mem.find(u8, result.stderr, "change detected; rerunning") == null) {
        return customFailure(allocator, timer, "watch stderr did not contain rerun separator", .{});
    }
    if (std.mem.find(u8, result.stderr, "panic") != null) {
        return customFailure(allocator, timer, "watch stderr contained panic", .{});
    }
    std.Io.Dir.cwd().access(io, marker_path, .{}) catch |err| {
        return customFailure(allocator, timer, "watch wrapper did not record the post-child source edit: {}", .{err});
    };

    return null;
}

const HotReloadNativeTarget = struct {
    roc_target: []const u8,
    zig_target: []const u8,
};

fn hotReloadNativeTarget() ?HotReloadNativeTarget {
    if (builtin.os.tag != .linux) return null;
    return switch (builtin.cpu.arch) {
        .x86_64 => .{ .roc_target = "x64musl", .zig_target = "x86_64-linux-musl" },
        .aarch64 => .{ .roc_target = "arm64musl", .zig_target = "aarch64-linux-musl" },
        else => null,
    };
}

const hot_reload_app_header =
    "app [main!] { pf: platform \"./platform/main.roc\" }\n\n";

const hot_reload_app_data_body =
    \\import "data.txt" as data : Str
    \\
    \\main! : U64 => U64
    \\main! = |seed|
    \\    if seed == 0 and data == "one" {
    \\        1
    \\    } else if seed == 0 and data == "two" {
    \\        2
    \\    } else {
    \\        99
    \\    }
    \\
;

const hot_reload_extra_five_source =
    \\Extra :: [].{
    \\    value : U64
    \\    value = 5
    \\}
    \\
;

fn writeHotReloadApp(io: std.Io, allocator: Allocator, path: []const u8, body: []const u8) CliRunnerError!void {
    const source = try std.mem.concat(allocator, u8, &.{ hot_reload_app_header, body });
    defer allocator.free(source);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = source });
}

fn hotReloadPlatformSource(allocator: Allocator, target: HotReloadNativeTarget) CliRunnerError![]const u8 {
    return try std.fmt.allocPrint(
        allocator,
        \\platform ""
        \\    requires {{}} {{ main! : U64 => U64 }}
        \\    exposes [Host]
        \\    packages {{}}
        \\    provides {{ "roc_main": main_for_host! }}
        \\    hosted {{
        \\        "roc_host_add": Host.add!,
        \\        "roc_host_edit_app_and_sleep": Host.edit_app_and_sleep!,
        \\        "roc_host_store_boxed": Host.store_boxed!,
        \\        "roc_host_stored_boxed_call": Host.stored_boxed_call!,
        \\    }}
        \\    targets: {{
        \\        inputs_dir: "targets/",
        \\        {s}: {{ inputs: ["crt1.o", "libhost.a", app, "libc.a"] }},
        \\    }}
        \\
        \\import Host
        \\
        \\main_for_host! : U64 => U64
        \\main_for_host! = main!
        \\
    ,
        .{target.roc_target},
    );
}

const hot_reload_platform_host_source =
    \\I64ToI64 : I64 -> I64
    \\
    \\Host := [].{
    \\    add! : U64, U64 => U64
    \\
    \\    edit_app_and_sleep! : U64 => U64
    \\
    \\    store_boxed! : Box(I64ToI64) => {}
    \\
    \\    stored_boxed_call! : I64 => I64
    \\}
    \\
;

const hot_reload_host_c_source =
    \\#include <errno.h>
    \\#include <pthread.h>
    \\#include <stddef.h>
    \\#include <stdint.h>
    \\#include <stdio.h>
    \\#include <stdlib.h>
    \\#include <sys/stat.h>
    \\#include <unistd.h>
    \\
    \\extern uint64_t roc_main(uint64_t);
    \\extern void *roc_shim_get_ops(void);
    \\extern void roc_builtins_erased_callable_incref(unsigned char *, intptr_t, void *);
    \\extern void roc_builtins_erased_callable_decref(unsigned char *, void *);
    \\
    \\#ifndef ROC_TARGET_NAME
    \\#error "ROC_TARGET_NAME must be defined"
    \\#endif
    \\
    \\typedef void (*RocCallable)(void *, void *, const void *, unsigned char *);
    \\struct RocErasedCallablePayload {
    \\    RocCallable callable;
    \\    void (*on_drop)(unsigned char *, void *);
    \\};
    \\struct I64Args {
    \\    int64_t arg0;
    \\};
    \\
    \\static unsigned char *stored_boxed = NULL;
    \\static unsigned char *retained_boxed = NULL;
    \\static const char *app_path_for_host_effects = NULL;
    \\static volatile int edit_on_sleep = 0;
    \\
    \\void *roc_alloc(size_t length, size_t alignment) {
    \\    if (alignment < sizeof(void *)) alignment = sizeof(void *);
    \\    void *ptr = NULL;
    \\    if (posix_memalign(&ptr, alignment, length == 0 ? 1 : length) != 0) return NULL;
    \\    return ptr;
    \\}
    \\
    \\void roc_dealloc(void *ptr, size_t alignment) {
    \\    (void)alignment;
    \\    free(ptr);
    \\}
    \\
    \\void *roc_realloc(void *ptr, size_t new_length, size_t alignment) {
    \\    (void)alignment;
    \\    return realloc(ptr, new_length == 0 ? 1 : new_length);
    \\}
    \\
    \\void roc_dbg(const unsigned char *bytes, size_t len) {
    \\    fwrite(bytes, 1, len, stderr);
    \\    fputc('\n', stderr);
    \\}
    \\
    \\void roc_expect_failed(const unsigned char *bytes, size_t len) {
    \\    fwrite(bytes, 1, len, stderr);
    \\    fputc('\n', stderr);
    \\}
    \\
    \\void roc_crashed(const unsigned char *bytes, size_t len) {
    \\    fwrite(bytes, 1, len, stderr);
    \\    fputc('\n', stderr);
    \\    abort();
    \\}
    \\
    \\uint64_t roc_host_add(uint64_t a, uint64_t b) {
    \\    return a + b;
    \\}
    \\
    \\static int64_t call_boxed_i64_to_i64(unsigned char *boxed, int64_t value) {
    \\    if (boxed == NULL) {
    \\        fprintf(stderr, "stored boxed callable was null\n");
    \\        abort();
    \\    }
    \\    struct RocErasedCallablePayload *payload = (struct RocErasedCallablePayload *)boxed;
    \\    struct I64Args args = { .arg0 = value };
    \\    int64_t result = 0;
    \\    payload->callable(roc_shim_get_ops(), &result, &args, boxed + 16);
    \\    return result;
    \\}
    \\
    \\void roc_host_store_boxed(unsigned char *boxed) {
    \\    void *ops = roc_shim_get_ops();
    \\    if (stored_boxed != NULL) {
    \\        roc_builtins_erased_callable_decref(stored_boxed, ops);
    \\    }
    \\    if (boxed != NULL) {
    \\        roc_builtins_erased_callable_incref(boxed, 1, ops);
    \\    }
    \\    stored_boxed = boxed;
    \\}
    \\
    \\int64_t roc_host_stored_boxed_call(int64_t value) {
    \\    return call_boxed_i64_to_i64(stored_boxed, value);
    \\}
    \\
    \\static int retain_stored_boxed(void) {
    \\    void *ops = roc_shim_get_ops();
    \\    if (stored_boxed == NULL) return 1;
    \\    if (retained_boxed != NULL) {
    \\        roc_builtins_erased_callable_decref(retained_boxed, ops);
    \\    }
    \\    roc_builtins_erased_callable_incref(stored_boxed, 1, ops);
    \\    retained_boxed = stored_boxed;
    \\    return 0;
    \\}
    \\
    \\static const char *app_header =
    \\"app [main!] { pf: platform \"./platform/main.roc\" }\n\n";
    \\
    \\static const char *app_const_three =
    \\"main! : U64 => U64\n"
    \\"main! = |_| 3\n";
    \\
    \\static const char *app_invalid =
    \\"main! : U64 => U64\n"
    \\"main! = |_| \"bad\"\n";
    \\
    \\static const char *app_import_extra =
    \\"import Extra\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| Extra.value\n";
    \\
    \\static const char *app_const_seven =
    \\"main! : U64 => U64\n"
    \\"main! = |_| 7\n";
    \\
    \\static const char *app_import_slow =
    \\"import \"slow.txt\" as slow : Str\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| if slow == \"done\" { 8 } else { 8 }\n";
    \\
    \\static const char *app_const_eleven =
    \\"main! : U64 => U64\n"
    \\"main! = |_| 11\n";
    \\
    \\static const char *app_effect_add =
    \\"import pf.Host\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| Host.add!(20, 22)\n";
    \\
    \\static const char *app_store_boxed =
    \\"import pf.Host\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| {\n"
    \\"    offset = 100\n"
    \\"    boxed = Box.box(|x| x + offset)\n"
    \\"    Host.store_boxed!(boxed)\n"
    \\"    Host.stored_boxed_call!(1).to_u64_wrap()\n"
    \\"}\n";
    \\
    \\static const char *app_store_boxed_wide_capture =
    \\"import pf.Host\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| {\n"
    \\"    record = { a: 40, b: 50, c: 60 }\n"
    \\"    boxed = Box.box(|x| x + record.a + record.b + record.c)\n"
    \\"    Host.store_boxed!(boxed)\n"
    \\"    Host.stored_boxed_call!(1).to_u64_wrap()\n"
    \\"}\n";
    \\
    \\static const char *app_store_boxed_empty_capture =
    \\"import pf.Host\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| {\n"
    \\"    boxed = Box.box(|x| x + 3)\n"
    \\"    Host.store_boxed!(boxed)\n"
    \\"    Host.stored_boxed_call!(4).to_u64_wrap()\n"
    \\"}\n";
    \\
    \\static const char *app_const_thirteen =
    \\"main! : U64 => U64\n"
    \\"main! = |_| 13\n";
    \\
    \\static const char *app_inflight_old =
    \\"import pf.Host\n\n"
    \\"main! : U64 => U64\n"
    \\"main! = |_| Host.edit_app_and_sleep!(15)\n";
    \\
    \\static const char *app_const_seventeen =
    \\"main! : U64 => U64\n"
    \\"main! = |_| 17\n";
    \\
    \\static const char *platform_plus_one =
    \\"platform \"\"\n"
    \\"    requires {} { main! : U64 => U64 }\n"
    \\"    exposes [Host]\n"
    \\"    packages {}\n"
    \\"    provides { \"roc_main\": main_for_host! }\n"
    \\"    hosted {\n"
    \\"        \"roc_host_add\": Host.add!,\n"
    \\"        \"roc_host_edit_app_and_sleep\": Host.edit_app_and_sleep!,\n"
    \\"        \"roc_host_store_boxed\": Host.store_boxed!,\n"
    \\"        \"roc_host_stored_boxed_call\": Host.stored_boxed_call!,\n"
    \\"    }\n"
    \\"    targets: {\n"
    \\"        " ROC_TARGET_NAME ": { inputs: [\"crt1.o\", \"libhost.a\", app, \"libc.a\"] },\n"
    \\"    }\n"
    \\"\n"
    \\"import Host\n"
    \\"\n"
    \\"main_for_host! : U64 => U64\n"
    \\"main_for_host! = |arg| main!(arg) + 1\n";
    \\
    \\static const char *extra_five =
    \\"Extra :: [].{\n"
    \\"    value : U64\n"
    \\"    value = 5\n"
    \\"}\n";
    \\
    \\static const char *extra_six =
    \\"Extra :: [].{\n"
    \\"    value : U64\n"
    \\"    value = 6\n"
    \\"}\n";
    \\
    \\static const char *extra_eight =
    \\"Extra :: [].{\n"
    \\"    value : U64\n"
    \\"    value = 8\n"
    \\"}\n";
    \\
    \\static int write_bytes(const char *path, const char *bytes) {
    \\    FILE *file = fopen(path, "wb");
    \\    if (file == NULL) {
    \\        perror("fopen");
    \\        return 1;
    \\    }
    \\    if (fputs(bytes, file) < 0) {
    \\        perror("fputs");
    \\        fclose(file);
    \\        return 1;
    \\    }
    \\    if (fclose(file) != 0) {
    \\        perror("fclose");
    \\        return 1;
    \\    }
    \\    return 0;
    \\}
    \\
    \\static int write_app(const char *path, const char *body) {
    \\    FILE *file = fopen(path, "wb");
    \\    if (file == NULL) {
    \\        perror("fopen app");
    \\        return 1;
    \\    }
    \\    if (fputs(app_header, file) < 0 || fputs(body, file) < 0) {
    \\        perror("fputs app");
    \\        fclose(file);
    \\        return 1;
    \\    }
    \\    if (fclose(file) != 0) {
    \\        perror("fclose app");
    \\        return 1;
    \\    }
    \\    return 0;
    \\}
    \\
    \\uint64_t roc_host_edit_app_and_sleep(uint64_t value) {
    \\    if (!edit_on_sleep) return value;
    \\    if (app_path_for_host_effects == NULL) return 0;
    \\    if (write_app(app_path_for_host_effects, app_const_seventeen)) return 0;
    \\    usleep(2500000);
    \\    return value;
    \\}
    \\
    \\static void *call_roc_main_thread(void *arg) {
    \\    uint64_t *result = (uint64_t *)arg;
    \\    *result = roc_main(0);
    \\    return NULL;
    \\}
    \\
    \\static int append_bytes(const char *path, const char *bytes) {
    \\    FILE *file = fopen(path, "ab");
    \\    if (file == NULL) {
    \\        perror("fopen append");
    \\        return 1;
    \\    }
    \\    if (fputs(bytes, file) < 0) {
    \\        perror("fputs append");
    \\        fclose(file);
    \\        return 1;
    \\    }
    \\    if (fclose(file) != 0) {
    \\        perror("fclose append");
    \\        return 1;
    \\    }
    \\    return 0;
    \\}
    \\
    \\static int wait_for_value(const char *label, uint64_t expected) {
    \\    for (int i = 0; i < 120; i += 1) {
    \\        uint64_t value = roc_main(0);
    \\        if (value == expected) {
    \\            printf("%s:%llu\n", label, (unsigned long long)value);
    \\            fflush(stdout);
    \\            return 0;
    \\        }
    \\        usleep(100000);
    \\    }
    \\    fprintf(stderr, "timed out waiting for %s=%llu, last=%llu\n",
    \\        label,
    \\        (unsigned long long)expected,
    \\        (unsigned long long)roc_main(0));
    \\    return 1;
    \\}
    \\
    \\int main(int argc, char **argv) {
    \\    if (argc < 6) {
    \\        fprintf(stderr, "expected app, data, Extra, platform, and slow file paths\n");
    \\        return 1;
    \\    }
    \\
    \\    const char *app_path = argv[1];
    \\    const char *data_path = argv[2];
    \\    const char *extra_path = argv[3];
    \\    const char *platform_path = argv[4];
    \\    const char *slow_path = argv[5];
    \\    app_path_for_host_effects = app_path;
    \\
    \\    if (wait_for_value("initial", 1)) return 1;
    \\
    \\    if (write_bytes(data_path, "one")) return 1;
    \\    usleep(600000);
    \\
    \\    if (write_bytes(data_path, "two")) return 1;
    \\    if (wait_for_value("file-import", 2)) return 1;
    \\
    \\    if (write_app(app_path, app_const_three)) return 1;
    \\    if (wait_for_value("source-edit", 3)) return 1;
    \\
    \\    if (write_app(app_path, app_invalid)) return 1;
    \\    usleep(2000000);
    \\    uint64_t after_invalid = roc_main(0);
    \\    printf("failed-rebuild:%llu\n", (unsigned long long)after_invalid);
    \\    fflush(stdout);
    \\    if (after_invalid != 3) return 1;
    \\
    \\    if (write_bytes(extra_path, extra_five)) return 1;
    \\    if (write_app(app_path, app_import_extra)) return 1;
    \\    if (wait_for_value("add-import", 5)) return 1;
    \\
    \\    if (write_bytes(extra_path, extra_six)) return 1;
    \\    if (wait_for_value("module-edit", 6)) return 1;
    \\
    \\    if (write_app(app_path, app_const_seven)) return 1;
    \\    if (wait_for_value("remove-import", 7)) return 1;
    \\
    \\    if (write_bytes(extra_path, extra_eight)) return 1;
    \\    usleep(1200000);
    \\    uint64_t after_removed = roc_main(0);
    \\    printf("removed-module-edit:%llu\n", (unsigned long long)after_removed);
    \\    fflush(stdout);
    \\    if (after_removed != 7) return 1;
    \\
    \\    if (mkfifo(slow_path, 0600) != 0 && errno != EEXIST) {
    \\        perror("mkfifo slow import");
    \\        return 1;
    \\    }
    \\    if (write_app(app_path, app_import_slow)) return 1;
    \\    usleep(1500000);
    \\    uint64_t while_blocked = roc_main(0);
    \\    printf("in-progress-rebuild:%llu\n", (unsigned long long)while_blocked);
    \\    fflush(stdout);
    \\    if (while_blocked != 7) return 1;
    \\    if (write_app(app_path, app_const_eleven)) return 1;
    \\    if (wait_for_value("cancelled-rebuild", 11)) return 1;
    \\
    \\    if (write_app(app_path, app_effect_add)) return 1;
    \\    if (wait_for_value("host-effect", 42)) return 1;
    \\
    \\    if (write_app(app_path, app_store_boxed)) return 1;
    \\    if (wait_for_value("boxed-store", 101)) return 1;
    \\
    \\    if (write_app(app_path, app_store_boxed_wide_capture)) return 1;
    \\    if (wait_for_value("boxed-wide-capture", 151)) return 1;
    \\    if (retain_stored_boxed()) return 1;
    \\
    \\    if (write_app(app_path, app_store_boxed_empty_capture)) return 1;
    \\    if (wait_for_value("boxed-empty-capture", 7)) return 1;
    \\    int64_t old_wide_result = call_boxed_i64_to_i64(retained_boxed, 9);
    \\    printf("boxed-wide-old-after-shrink:%lld\n", (long long)old_wide_result);
    \\    fflush(stdout);
    \\    if (old_wide_result != 159) return 1;
    \\    roc_builtins_erased_callable_decref(retained_boxed, roc_shim_get_ops());
    \\    retained_boxed = NULL;
    \\
    \\    if (write_app(app_path, app_const_thirteen)) return 1;
    \\    if (wait_for_value("boxed-post-reload", 13)) return 1;
    \\    int64_t old_boxed_result = call_boxed_i64_to_i64(stored_boxed, 9);
    \\    printf("boxed-old-after-reload:%lld\n", (long long)old_boxed_result);
    \\    fflush(stdout);
    \\    if (old_boxed_result != 12) return 1;
    \\    roc_builtins_erased_callable_decref(stored_boxed, roc_shim_get_ops());
    \\    stored_boxed = NULL;
    \\    puts("boxed-released");
    \\    fflush(stdout);
    \\
    \\    if (write_app(app_path, app_inflight_old)) return 1;
    \\    edit_on_sleep = 0;
    \\    if (wait_for_value("in-flight-loaded", 15)) return 1;
    \\    edit_on_sleep = 1;
    \\    uint64_t in_flight_old_result = 0;
    \\    pthread_t in_flight_old_thread;
    \\    if (pthread_create(&in_flight_old_thread, NULL, call_roc_main_thread, &in_flight_old_result) != 0) {
    \\        perror("pthread_create");
    \\        return 1;
    \\    }
    \\    usleep(900000);
    \\    if (wait_for_value("in-flight-new-generation", 17)) return 1;
    \\    if (pthread_join(in_flight_old_thread, NULL) != 0) {
    \\        perror("pthread_join");
    \\        return 1;
    \\    }
    \\    edit_on_sleep = 0;
    \\    printf("in-flight-old-return:%llu\n", (unsigned long long)in_flight_old_result);
    \\    fflush(stdout);
    \\    if (in_flight_old_result != 15) return 1;
    \\
    \\    if (write_bytes(platform_path, platform_plus_one)) return 1;
    \\    if (wait_for_value("platform-edit", 18)) return 1;
    \\
    \\    puts("done");
    \\    fflush(stdout);
    \\    return 0;
    \\}
    \\
;

fn countOccurrences(haystack: []const u8, needle: []const u8) usize {
    if (needle.len == 0) return 0;

    var count: usize = 0;
    var offset: usize = 0;
    while (std.mem.find(u8, haystack[offset..], needle)) |relative| {
        count += 1;
        offset += relative + needle.len;
    }
    return count;
}

fn copyHotReloadTargetFile(
    io: std.Io,
    allocator: Allocator,
    target: HotReloadNativeTarget,
    filename: []const u8,
    dest_dir: []const u8,
) CliRunnerError!void {
    const src = try std.fs.path.join(allocator, &.{ project_root_path, "test", "fx", "platform", "targets", target.roc_target, filename });
    defer allocator.free(src);
    const dest = try std.fs.path.join(allocator, &.{ dest_dir, filename });
    defer allocator.free(dest);

    std.Io.Dir.cwd().copyFile(src, std.Io.Dir.cwd(), dest, io, .{}) catch |err| {
        return err;
    };
}

fn customHotReloadDevShim(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const target = hotReloadNativeTarget() orelse {
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = "hot-reload dev-shim integration runs only on native Linux x64/arm64 hosts" };
    };

    const platform_dir = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "platform" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform dir: {}", .{err});
    const target_dir = std.fs.path.join(allocator, &.{ platform_dir, "targets", target.roc_target }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform target dir: {}", .{err});
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "app.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate app path: {}", .{err});
    const data_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "data.txt" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate data path: {}", .{err});
    const extra_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "Extra.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate module path: {}", .{err});
    const platform_path = std.fs.path.join(allocator, &.{ platform_dir, "main.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform path: {}", .{err});
    const platform_host_path = std.fs.path.join(allocator, &.{ platform_dir, "Host.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform Host module path: {}", .{err});
    const slow_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "slow.txt" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate slow file-import path: {}", .{err});
    const host_c_path = std.fs.path.join(allocator, &.{ platform_dir, "host.c" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate host C path: {}", .{err});
    const host_o_path = std.fs.path.join(allocator, &.{ target_dir, "host.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate host object path: {}", .{err});
    const host_lib_path = std.fs.path.join(allocator, &.{ target_dir, "libhost.a" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate host archive path: {}", .{err});
    const target_arg = std.fmt.allocPrint(allocator, "--target={s}", .{target.roc_target}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate target arg: {}", .{err});
    const target_define_arg = std.fmt.allocPrint(allocator, "-DROC_TARGET_NAME=\"{s}\"", .{target.roc_target}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate target define: {}", .{err});

    std.Io.Dir.cwd().createDirPath(io, target_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to create platform target dir: {}", .{err});
    copyHotReloadTargetFile(io, allocator, target, "crt1.o", target_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to copy crt1.o: {}", .{err});
    copyHotReloadTargetFile(io, allocator, target, "libc.a", target_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to copy libc.a: {}", .{err});

    const platform_source = hotReloadPlatformSource(allocator, target) catch |err|
        return customInfraFailure(allocator, timer, "failed to render platform source: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = platform_path, .data = platform_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write platform source: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = platform_host_path, .data = hot_reload_platform_host_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write platform Host module: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = host_c_path, .data = hot_reload_host_c_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write host C source: {}", .{err});
    writeHotReloadApp(io, allocator, app_path, hot_reload_app_data_body) catch |err|
        return customInfraFailure(allocator, timer, "failed to write app source: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = data_path, .data = "one" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write data import: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = extra_path, .data = hot_reload_extra_five_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write imported module: {}", .{err});

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "cc",
        "-target",
        target.zig_target,
        target_define_arg,
        "-O2",
        "-c",
        host_c_path,
        "-o",
        host_o_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "ar",
        "rcs",
        host_lib_path,
        host_o_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc --watch started");
    const result = runRawInEnv(io, allocator, env, &.{
        roc_binary_path,
        "--watch",
        "--opt=dev",
        "--no-cache",
        target_arg,
        app_path,
        "--",
        app_path,
        data_path,
        extra_path,
        platform_path,
        slow_path,
    }, project_root_path, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "roc --watch spawn error: {}", .{err});

    if (checkCommandExpectation(allocator, result, .{
        .args = &.{"--watch"},
        .exit = .success,
        .contains = &.{
            .{ .stream = .stdout, .text = "initial:1\n" },
            .{ .stream = .stdout, .text = "file-import:2\n" },
            .{ .stream = .stdout, .text = "source-edit:3\n" },
            .{ .stream = .stdout, .text = "failed-rebuild:3\n" },
            .{ .stream = .stdout, .text = "add-import:5\n" },
            .{ .stream = .stdout, .text = "module-edit:6\n" },
            .{ .stream = .stdout, .text = "remove-import:7\n" },
            .{ .stream = .stdout, .text = "removed-module-edit:7\n" },
            .{ .stream = .stdout, .text = "in-progress-rebuild:7\n" },
            .{ .stream = .stdout, .text = "cancelled-rebuild:11\n" },
            .{ .stream = .stdout, .text = "host-effect:42\n" },
            .{ .stream = .stdout, .text = "boxed-store:101\n" },
            .{ .stream = .stdout, .text = "boxed-wide-capture:151\n" },
            .{ .stream = .stdout, .text = "boxed-empty-capture:7\n" },
            .{ .stream = .stdout, .text = "boxed-wide-old-after-shrink:159\n" },
            .{ .stream = .stdout, .text = "boxed-post-reload:13\n" },
            .{ .stream = .stdout, .text = "boxed-old-after-reload:12\n" },
            .{ .stream = .stdout, .text = "boxed-released\n" },
            .{ .stream = .stdout, .text = "in-flight-loaded:15\n" },
            .{ .stream = .stdout, .text = "in-flight-old-return:15\n" },
            .{ .stream = .stdout, .text = "in-flight-new-generation:17\n" },
            .{ .stream = .stdout, .text = "platform-edit:18\n" },
            .{ .stream = .stdout, .text = "done\n" },
            .{ .stream = .stderr, .text = "TYPE MISMATCH" },
            .{ .stream = .stderr, .text = "hot reload generation" },
            .{ .stream = .stderr, .text = "accepted by host" },
        },
        .not_contains = &.{
            .{ .stream = .stderr, .text = "timed out waiting" },
            .{ .stream = .stderr, .text = "FILE IMPORT" },
            .{ .stream = .stderr, .text = "panic" },
        },
    })) |message| return failureFromRun(allocator, timer, result, message);

    const in_flight_new_idx = std.mem.find(u8, result.stdout, "in-flight-new-generation:17\n") orelse {
        return failureFromRun(allocator, timer, result, "missing in-flight new-generation output");
    };
    const in_flight_old_idx = std.mem.find(u8, result.stdout, "in-flight-old-return:15\n") orelse {
        return failureFromRun(allocator, timer, result, "missing in-flight old-return output");
    };
    if (in_flight_new_idx > in_flight_old_idx) {
        return failureFromRun(allocator, timer, result, "new generation did not complete before old in-flight call returned");
    }

    const published_count = countOccurrences(result.stderr, "published");
    if (published_count != 14) {
        return failureFromRun(
            allocator,
            timer,
            result,
            std.fmt.allocPrint(allocator, "expected 14 published hot reload generations, got {d}", .{published_count}) catch "unexpected hot reload publish count",
        );
    }

    const accepted_count = countOccurrences(result.stderr, "accepted by host");
    if (accepted_count != 14) {
        return failureFromRun(
            allocator,
            timer,
            result,
            std.fmt.allocPrint(allocator, "expected 14 accepted hot reload generations, got {d}", .{accepted_count}) catch "unexpected hot reload ack count",
        );
    }

    return null;
}

const hot_reload_model_app_header =
    "app [Model, main] { pf: platform \"./platform/main.roc\" }\n\n";

const hot_reload_model_app_initial =
    \\Model : { value : U64 }
    \\
    \\main = { init, update, value }
    \\
    \\init : U64 -> Model
    \\init = |seed| { value: seed }
    \\
    \\update : Model, U64 -> Model
    \\update = |model, delta| { value: model.value + delta }
    \\
    \\value : Model -> U64
    \\value = |model| model.value
    \\
;

fn writeHotReloadModelApp(io: std.Io, allocator: Allocator, path: []const u8, body: []const u8) CliRunnerError!void {
    const source = try std.mem.concat(allocator, u8, &.{ hot_reload_model_app_header, body });
    defer allocator.free(source);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = source });
}

fn hotReloadModelPlatformSource(allocator: Allocator, target: HotReloadNativeTarget) CliRunnerError![]const u8 {
    return try std.fmt.allocPrint(
        allocator,
        \\platform ""
        \\    requires {{
        \\        [Model : model] for main : {{
        \\            init : U64 -> model,
        \\            update : model, U64 -> model,
        \\            value : model -> U64,
        \\        }}
        \\    }}
        \\    exposes []
        \\    packages {{}}
        \\    provides {{
        \\        "roc_init_model": init_model_for_host,
        \\        "roc_update_model": update_model_for_host,
        \\        "roc_model_value": model_value_for_host,
        \\    }}
        \\    targets: {{
        \\        inputs_dir: "targets/",
        \\        {s}: {{ inputs: ["crt1.o", "libhost.a", app, "libc.a"] }},
        \\    }}
        \\
        \\init_model_for_host : U64 -> Box(Model)
        \\init_model_for_host = |seed| {{
        \\    init_fn = main.init
        \\
        \\    Box.box(init_fn(seed))
        \\}}
        \\
        \\update_model_for_host : Box(Model), U64 -> Box(Model)
        \\update_model_for_host = |boxed_model, delta| {{
        \\    model = Box.unbox(boxed_model)
        \\    update_fn = main.update
        \\
        \\    Box.box(update_fn(model, delta))
        \\}}
        \\
        \\model_value_for_host : Box(Model) -> U64
        \\model_value_for_host = |boxed_model| {{
        \\    value_fn = main.value
        \\
        \\    value_fn(Box.unbox(boxed_model))
        \\}}
        \\
    ,
        .{target.roc_target},
    );
}

const hot_reload_model_host_c_source =
    \\#include <stdint.h>
    \\#include <stdio.h>
    \\#include <stdlib.h>
    \\#include <unistd.h>
    \\
    \\extern unsigned char *roc_init_model(uint64_t);
    \\extern unsigned char *roc_update_model(unsigned char *, uint64_t);
    \\extern uint64_t roc_model_value(unsigned char *);
    \\
    \\void *roc_alloc(size_t length, size_t alignment) {
    \\    if (alignment < sizeof(void *)) alignment = sizeof(void *);
    \\    void *ptr = NULL;
    \\    if (posix_memalign(&ptr, alignment, length == 0 ? 1 : length) != 0) return NULL;
    \\    return ptr;
    \\}
    \\
    \\void roc_dealloc(void *ptr, size_t alignment) {
    \\    (void)alignment;
    \\    free(ptr);
    \\}
    \\
    \\void *roc_realloc(void *ptr, size_t new_length, size_t alignment) {
    \\    (void)alignment;
    \\    return realloc(ptr, new_length == 0 ? 1 : new_length);
    \\}
    \\
    \\void roc_dbg(const unsigned char *bytes, size_t len) {
    \\    fwrite(bytes, 1, len, stderr);
    \\    fputc('\n', stderr);
    \\}
    \\
    \\void roc_expect_failed(const unsigned char *bytes, size_t len) {
    \\    fwrite(bytes, 1, len, stderr);
    \\    fputc('\n', stderr);
    \\}
    \\
    \\void roc_crashed(const unsigned char *bytes, size_t len) {
    \\    fwrite(bytes, 1, len, stderr);
    \\    fputc('\n', stderr);
    \\    abort();
    \\}
    \\
    \\static const char *app_header =
    \\"app [Model, main] { pf: platform \"./platform/main.roc\" }\n\n";
    \\
    \\static const char *app_updated =
    \\"Model : { value : U64 }\n\n"
    \\"main = { init, update, value }\n\n"
    \\"init : U64 -> Model\n"
    \\"init = |seed| { value: seed }\n\n"
    \\"update : Model, U64 -> Model\n"
    \\"update = |model, delta| { value: model.value + (delta * 10) }\n\n"
    \\"value : Model -> U64\n"
    \\"value = |model| model.value\n";
    \\
    \\static const char *app_incompatible_same_layout =
    \\"Model : U64\n\n"
    \\"main = { init, update, value }\n\n"
    \\"init : U64 -> Model\n"
    \\"init = |seed| seed\n\n"
    \\"update : Model, U64 -> Model\n"
    \\"update = |model, delta| model + (delta * 100)\n\n"
    \\"value : Model -> U64\n"
    \\"value = |model| model\n";
    \\
    \\static const char *app_incompatible_larger =
    \\"Model : { value : U64, extra : U64 }\n\n"
    \\"main = { init, update, value }\n\n"
    \\"init : U64 -> Model\n"
    \\"init = |seed| { value: seed, extra: 100 }\n\n"
    \\"update : Model, U64 -> Model\n"
    \\"update = |model, delta| { value: model.value + (delta * 1000), extra: model.extra }\n\n"
    \\"value : Model -> U64\n"
    \\"value = |model| model.value + model.extra\n";
    \\
    \\static int write_app(const char *path, const char *body) {
    \\    FILE *file = fopen(path, "wb");
    \\    if (file == NULL) {
    \\        perror("fopen app");
    \\        return 1;
    \\    }
    \\    if (fputs(app_header, file) < 0 || fputs(body, file) < 0) {
    \\        perror("fputs app");
    \\        fclose(file);
    \\        return 1;
    \\    }
    \\    if (fclose(file) != 0) {
    \\        perror("fclose app");
    \\        return 1;
    \\    }
    \\    return 0;
    \\}
    \\
    \\static uint64_t run_model_pipeline(uint64_t seed, uint64_t delta) {
    \\    unsigned char *model = roc_init_model(seed);
    \\    model = roc_update_model(model, delta);
    \\    return roc_model_value(model);
    \\}
    \\
    \\static int wait_for_model_value(const char *label, uint64_t expected) {
    \\    for (int i = 0; i < 120; i += 1) {
    \\        uint64_t value = run_model_pipeline(10, 2);
    \\        if (value == expected) {
    \\            printf("%s:%llu\n", label, (unsigned long long)value);
    \\            fflush(stdout);
    \\            return 0;
    \\        }
    \\        usleep(100000);
    \\    }
    \\    fprintf(stderr, "timed out waiting for %s=%llu, last=%llu\n",
    \\        label,
    \\        (unsigned long long)expected,
    \\        (unsigned long long)run_model_pipeline(10, 2));
    \\    return 1;
    \\}
    \\
    \\int main(int argc, char **argv) {
    \\    if (argc < 2) {
    \\        fprintf(stderr, "expected app path\n");
    \\        return 1;
    \\    }
    \\
    \\    const char *app_path = argv[1];
    \\    if (wait_for_model_value("model-initial", 12)) return 1;
    \\    if (write_app(app_path, app_updated)) return 1;
    \\    if (wait_for_model_value("model-reload", 30)) return 1;
    \\
    \\    if (write_app(app_path, app_incompatible_same_layout)) return 1;
    \\    usleep(2000000);
    \\    uint64_t after_same_layout = run_model_pipeline(10, 2);
    \\    printf("model-same-layout-rejected:%llu\n", (unsigned long long)after_same_layout);
    \\    fflush(stdout);
    \\    if (after_same_layout != 30) return 1;
    \\
    \\    if (write_app(app_path, app_incompatible_larger)) return 1;
    \\    usleep(2000000);
    \\    uint64_t after_larger = run_model_pipeline(10, 2);
    \\    printf("model-larger-rejected:%llu\n", (unsigned long long)after_larger);
    \\    fflush(stdout);
    \\    if (after_larger != 30) return 1;
    \\
    \\    puts("model-done");
    \\    fflush(stdout);
    \\    return 0;
    \\}
    \\
;

fn customHotReloadModelBoundary(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const target = hotReloadNativeTarget() orelse {
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = "hot-reload Model integration runs only on native Linux x64/arm64 hosts" };
    };

    const platform_dir = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "platform" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model platform dir: {}", .{err});
    const target_dir = std.fs.path.join(allocator, &.{ platform_dir, "targets", target.roc_target }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model platform target dir: {}", .{err});
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "model_app.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model app path: {}", .{err});
    const platform_path = std.fs.path.join(allocator, &.{ platform_dir, "main.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model platform path: {}", .{err});
    const host_c_path = std.fs.path.join(allocator, &.{ platform_dir, "host.c" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model host C path: {}", .{err});
    const host_o_path = std.fs.path.join(allocator, &.{ target_dir, "host.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model host object path: {}", .{err});
    const host_lib_path = std.fs.path.join(allocator, &.{ target_dir, "libhost.a" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model host archive path: {}", .{err});
    const target_arg = std.fmt.allocPrint(allocator, "--target={s}", .{target.roc_target}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate model target arg: {}", .{err});

    std.Io.Dir.cwd().createDirPath(io, target_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to create model platform target dir: {}", .{err});
    copyHotReloadTargetFile(io, allocator, target, "crt1.o", target_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to copy model crt1.o: {}", .{err});
    copyHotReloadTargetFile(io, allocator, target, "libc.a", target_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to copy model libc.a: {}", .{err});

    const platform_source = hotReloadModelPlatformSource(allocator, target) catch |err|
        return customInfraFailure(allocator, timer, "failed to render model platform source: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = platform_path, .data = platform_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write model platform source: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = host_c_path, .data = hot_reload_model_host_c_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write model host C source: {}", .{err});
    writeHotReloadModelApp(io, allocator, app_path, hot_reload_model_app_initial) catch |err|
        return customInfraFailure(allocator, timer, "failed to write model app source: {}", .{err});

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "cc",
        "-target",
        target.zig_target,
        "-O2",
        "-c",
        host_c_path,
        "-o",
        host_o_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "ar",
        "rcs",
        host_lib_path,
        host_o_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before model roc --watch started");
    const result = runRawInEnv(io, allocator, env, &.{
        roc_binary_path,
        "--watch",
        "--opt=dev",
        "--no-cache",
        target_arg,
        app_path,
        "--",
        app_path,
    }, project_root_path, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "model roc --watch spawn error: {}", .{err});

    if (checkCommandExpectation(allocator, result, .{
        .args = &.{"--watch"},
        .exit = .success,
        .contains = &.{
            .{ .stream = .stdout, .text = "model-initial:12\n" },
            .{ .stream = .stdout, .text = "model-reload:30\n" },
            .{ .stream = .stdout, .text = "model-same-layout-rejected:30\n" },
            .{ .stream = .stdout, .text = "model-larger-rejected:30\n" },
            .{ .stream = .stdout, .text = "model-done\n" },
            .{ .stream = .stderr, .text = "hot reload generation" },
            .{ .stream = .stderr, .text = "accepted by host" },
            .{ .stream = .stderr, .text = "changed the platform host interface" },
        },
        .not_contains = &.{
            .{ .stream = .stderr, .text = "timed out waiting" },
            .{ .stream = .stderr, .text = "panic" },
        },
    })) |message| return failureFromRun(allocator, timer, result, message);

    const accepted_count = countOccurrences(result.stderr, "accepted by host");
    if (accepted_count != 1) {
        return failureFromRun(
            allocator,
            timer,
            result,
            std.fmt.allocPrint(allocator, "expected 1 accepted model hot reload generation, got {d}", .{accepted_count}) catch "unexpected model hot reload ack count",
        );
    }

    return null;
}

const hot_reload_default_app_source =
    \\main! = |_| {
    \\    echo!("headerless watch")
    \\    Ok({})
    \\}
    \\
;

fn customHotReloadDefaultApp(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    if (hotReloadNativeTarget() == null) {
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = "headerless hot-reload default app test runs only on native Linux x64/arm64 hosts" };
    }

    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "headerless_watch.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default app path: {}", .{err});

    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = hot_reload_default_app_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write default app source: {}", .{err});

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before headerless roc --watch started");
    const result = runRawInEnv(io, allocator, env, &.{
        roc_binary_path,
        "--watch",
        "--opt=dev",
        "--no-cache",
        app_path,
    }, project_root_path, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "headerless roc --watch spawn error: {}", .{err});

    if (checkCommandExpectation(allocator, result, .{
        .args = &.{"--watch"},
        .exit = .success,
        .stdout_exact = "headerless watch\n",
        .not_contains = &.{
            .{ .stream = .stderr, .text = "unsupported" },
            .{ .stream = .stderr, .text = "panic" },
        },
    })) |message| return failureFromRun(allocator, timer, result, message);

    return null;
}

const platform_requires_checker_platform_source =
    \\platform ""
    \\    requires {
    \\        [Model : model] for program : {
    \\            init : model,
    \\            value : model -> U64,
    \\        }
    \\    }
    \\    exposes []
    \\    packages {}
    \\    provides { "roc_main": main_for_host }
    \\    targets: {
    \\        inputs_dir: "targets/",
    \\        x64mac: { inputs: ["libhost.a", app] },
    \\        arm64mac: { inputs: ["libhost.a", app] },
    \\        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
    \\        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
    \\        x64win: { inputs: ["host.lib", app] },
    \\        arm64win: { inputs: ["host.lib", app] },
    \\    }
    \\
    \\main_for_host : {} -> I32
    \\main_for_host = |_| 0
    \\
;

const PlatformRequiresDiagnosticCase = struct {
    dir_name: []const u8,
    app_source: []const u8,
    sibling_source: ?[]const u8 = null,
    expected_stderr: []const []const u8,
};

const PlatformRequiresTargetPlaceholder = struct {
    dir_name: []const u8,
    files: []const []const u8,
};

const platform_requires_target_placeholders = [_]PlatformRequiresTargetPlaceholder{
    .{ .dir_name = "x64mac", .files = &.{"libhost.a"} },
    .{ .dir_name = "arm64mac", .files = &.{"libhost.a"} },
    .{ .dir_name = "x64musl", .files = &.{ "crt1.o", "libhost.a", "libc.a" } },
    .{ .dir_name = "arm64musl", .files = &.{ "crt1.o", "libhost.a", "libc.a" } },
    .{ .dir_name = "x64win", .files = &.{"host.lib"} },
    .{ .dir_name = "arm64win", .files = &.{"host.lib"} },
};

fn customPlatformRequiresCheckerDiagnostics(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const cases = [_]PlatformRequiresDiagnosticCase{
        .{
            .dir_name = "missing-type",
            .app_source =
            \\app [program, main!] { pf: platform "./platform.roc" }
            \\
            \\program = {
            \\    init: {},
            \\    value: |_| 0,
            \\}
            \\
            \\main! = |_| {}
            \\
            ,
            .expected_stderr = &.{ "MISSING PLATFORM REQUIRED TYPE", "Model", "type alias or nominal type" },
        },
        .{
            .dir_name = "value-named-model",
            .app_source =
            \\app [program, main!] { pf: platform "./platform.roc" }
            \\
            \\Other : [Model]
            \\
            \\program = {
            \\    init: {},
            \\    value: |_| 0,
            \\}
            \\
            \\main! = |_| {}
            \\
            ,
            .expected_stderr = &.{ "MISSING PLATFORM REQUIRED TYPE", "Model", "value named", "type declaration" },
        },
        .{
            .dir_name = "sibling-only-type",
            .app_source =
            \\app [program, main!] { pf: platform "./platform.roc" }
            \\
            \\import Model
            \\
            \\program = {
            \\    init: {},
            \\    value: |_| 0,
            \\}
            \\
            \\main! = |_| {}
            \\
            ,
            .sibling_source =
            \\Model := { count : U64 }
            \\
            ,
            .expected_stderr = &.{ "MISSING PLATFORM REQUIRED TYPE", "Model", "type alias or nominal type" },
        },
        .{
            .dir_name = "missing-required-def",
            .app_source =
            \\app [Model, main!] { pf: platform "./platform.roc" }
            \\
            \\Model : { count : U64 }
            \\
            \\main! = |_| {}
            \\
            ,
            .expected_stderr = &.{ "MISSING PLATFORM REQUIRED DEFINITION", "program", "Define and expose" },
        },
        .{
            .dir_name = "required-def-unexposed",
            .app_source =
            \\app [Model, main!] { pf: platform "./platform.roc" }
            \\
            \\Model : { count : U64 }
            \\
            \\program = {
            \\    init: { count: 0 },
            \\    value: |model| model.count,
            \\}
            \\
            \\main! = |_| {}
            \\
            ,
            .expected_stderr = &.{ "MISSING PLATFORM REQUIRED DEFINITION", "program", "not listed in your", "header" },
        },
        .{
            .dir_name = "required-def-type-mismatch",
            .app_source =
            \\app [Model, program, main!] { pf: platform "./platform.roc" }
            \\
            \\Model : { count : U64 }
            \\
            \\program = {
            \\    init: { count: 0 },
            \\    value: |_| "not a U64",
            \\}
            \\
            \\main! = |_| {}
            \\
            ,
            .expected_stderr = &.{ "TYPE MISMATCH", "string literal", "non-string", "type is needed" },
        },
    };

    for (&cases) |case| {
        if (runPlatformRequiresDiagnosticCase(io, allocator, env, timer, timeout_ms, case)) |failure| {
            return failure;
        }
    }

    return null;
}

fn runPlatformRequiresDiagnosticCase(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    case: PlatformRequiresDiagnosticCase,
) ?TestResult {
    const case_dir = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "platform-requires", case.dir_name }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform requires case dir: {}", .{err});
    const platform_path = std.fs.path.join(allocator, &.{ case_dir, "platform.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform requires platform path: {}", .{err});
    const app_path = std.fs.path.join(allocator, &.{ case_dir, "app.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform requires app path: {}", .{err});
    const sibling_path = std.fs.path.join(allocator, &.{ case_dir, "Model.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform requires sibling path: {}", .{err});

    std.Io.Dir.cwd().createDirPath(io, case_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to create platform requires case dir: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = platform_path, .data = platform_requires_checker_platform_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write platform requires platform: {}", .{err});
    if (verifyGeneratedFile(io, allocator, timer, platform_path, platform_requires_checker_platform_source)) |failure| {
        return failure;
    }
    if (writePlatformRequiresTargetPlaceholders(io, allocator, timer, case_dir)) |failure| {
        return failure;
    }
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = case.app_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write platform requires app: {}", .{err});
    if (verifyGeneratedFile(io, allocator, timer, app_path, case.app_source)) |failure| {
        return failure;
    }
    if (case.sibling_source) |source| {
        std.Io.Dir.cwd().writeFile(io, .{ .sub_path = sibling_path, .data = source }) catch |err|
            return customInfraFailure(allocator, timer, "failed to write platform requires sibling: {}", .{err});
        if (verifyGeneratedFile(io, allocator, timer, sibling_path, source)) |failure| {
            return failure;
        }
    }

    const check_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc check started");
    const check_result = runRocInCaseEnv(io, allocator, env, case_dir, &.{ "check", "--no-cache" }, app_path, check_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "roc check spawn error for {s}: {}", .{ case.dir_name, err });
    if (checkExitExpectation(allocator, check_result, .failure)) |message| {
        return failureFromRun(allocator, timer, check_result, message);
    }

    const run_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc run started");
    const run_result = runRocInCaseEnv(io, allocator, env, case_dir, &.{"--no-cache"}, app_path, run_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "roc run spawn error for {s}: {}", .{ case.dir_name, err });
    if (checkExitExpectation(allocator, run_result, .failure)) |message| {
        return failureFromRun(allocator, timer, run_result, message);
    }

    for (case.expected_stderr) |needle| {
        if (std.mem.find(u8, check_result.stderr, needle) == null) {
            return failureFromRun(
                allocator,
                timer,
                check_result,
                std.fmt.allocPrint(allocator, "{s}: roc check stderr did not contain expected text: {s}", .{ case.dir_name, needle }) catch "roc check stderr missing expected text",
            );
        }
    }

    const forbidden = [_][]const u8{
        "panic",
        "postcheck invariant",
        "platform for-clause substitution missing matching app type declaration",
        "missing platform declaration artifact",
    };
    for (&forbidden) |needle| {
        if (std.mem.find(u8, check_result.stderr, needle) != null) {
            return failureFromRun(
                allocator,
                timer,
                check_result,
                std.fmt.allocPrint(allocator, "{s}: roc check stderr contained forbidden text: {s}", .{ case.dir_name, needle }) catch "roc check stderr contained forbidden text",
            );
        }
        if (std.mem.find(u8, run_result.stderr, needle) != null) {
            return failureFromRun(
                allocator,
                timer,
                run_result,
                std.fmt.allocPrint(allocator, "{s}: roc run stderr contained forbidden text: {s}", .{ case.dir_name, needle }) catch "roc run stderr contained forbidden text",
            );
        }
    }

    const check_diagnostic = platformRequiresDiagnosticBody(check_result.stderr);
    const run_diagnostic = platformRequiresDiagnosticBody(run_result.stderr);
    if (!std.mem.eql(u8, check_diagnostic, run_diagnostic)) {
        return failureFromRun(
            allocator,
            timer,
            run_result,
            std.fmt.allocPrint(
                allocator,
                "{s}: roc check and roc run diagnostics differed ({d} vs {d} bytes)",
                .{ case.dir_name, check_diagnostic.len, run_diagnostic.len },
            ) catch "roc check and roc run diagnostics differed",
        );
    }

    return null;
}

fn verifyGeneratedFile(
    io: std.Io,
    allocator: Allocator,
    timer: *harness.Timer,
    path: []const u8,
    expected: []const u8,
) ?TestResult {
    const actual = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(1024 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read generated platform requires file: {}", .{err});
    defer allocator.free(actual);
    if (!std.mem.eql(u8, actual, expected)) {
        return customInfraFailure(
            allocator,
            timer,
            "generated platform requires file was not visible before spawn: {s}",
            .{path},
        );
    }
    return null;
}

fn writePlatformRequiresTargetPlaceholders(
    io: std.Io,
    allocator: Allocator,
    timer: *harness.Timer,
    case_dir: []const u8,
) ?TestResult {
    const targets_dir = std.fs.path.join(allocator, &.{ case_dir, "targets" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform requires targets dir: {}", .{err});

    for (&platform_requires_target_placeholders) |target| {
        const target_dir = std.fs.path.join(allocator, &.{ targets_dir, target.dir_name }) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate platform requires target dir: {}", .{err});
        std.Io.Dir.cwd().createDirPath(io, target_dir) catch |err|
            return customInfraFailure(allocator, timer, "failed to create platform requires target dir: {}", .{err});

        for (target.files) |file_name| {
            const file_path = std.fs.path.join(allocator, &.{ target_dir, file_name }) catch |err|
                return customInfraFailure(allocator, timer, "failed to allocate platform requires target file path: {}", .{err});
            std.Io.Dir.cwd().writeFile(io, .{ .sub_path = file_path, .data = "" }) catch |err|
                return customInfraFailure(allocator, timer, "failed to write platform requires target file: {}", .{err});
        }
    }

    return null;
}

fn runRocInCaseEnv(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    case_dir: []const u8,
    args: []const []const u8,
    roc_file: []const u8,
    timeout_ms: u64,
) CliRunnerError!std.process.RunResult {
    const cache_dir = try std.fs.path.join(allocator, &.{ case_dir, "roc-cache" });
    const zig_cache_dir = try std.fs.path.join(allocator, &.{ case_dir, "zig-cache" });
    const temp_dir = try std.fs.path.join(allocator, &.{ case_dir, "tmp" });
    try std.Io.Dir.cwd().createDirPath(io, cache_dir);
    try std.Io.Dir.cwd().createDirPath(io, zig_cache_dir);
    try std.Io.Dir.cwd().createDirPath(io, temp_dir);

    var case_env_map = try env.env_map.clone(allocator);
    defer case_env_map.deinit();
    try case_env_map.put("ROC_CACHE_DIR", cache_dir);
    try case_env_map.put("XDG_CACHE_HOME", cache_dir);
    try case_env_map.put("ZIG_LOCAL_CACHE_DIR", zig_cache_dir);
    try util.putIsolatedTempEnv(&case_env_map, temp_dir);

    const argv = try buildRocArgv(allocator, args, roc_file, .absolute);
    return util.runChildWithTimeout(io, allocator, argv, .{
        .cwd = project_root_path,
        .env_map = &case_env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = timeout_ms,
    });
}

fn platformRequiresDiagnosticBody(stderr: []const u8) []const u8 {
    const without_summary = if (std.mem.findLast(u8, stderr, "\n\nFound ")) |summary_idx|
        stderr[0..summary_idx]
    else
        stderr;
    var end = without_summary.len;
    while (end > 0 and without_summary[end - 1] == '\n') {
        end -= 1;
    }
    return without_summary[0..end];
}

const default_platform_linux_disassembly_app =
    \\main! = |_| {
    \\    echo!("Hello, World!")
    \\    Ok({})
    \\}
    \\
;

const expected_default_platform_linux_disassembly =
    \\movq $0x1, %rax
    \\movq $0x1, %rdi
    \\leaq msg(%rip), %rsi
    \\movq $0xe, %rdx
    \\syscall
    \\movq $0x3c, %rax
    \\xorq %rdi, %rdi
    \\syscall
    \\
;

fn customDefaultPlatformLinuxDisassembly(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    if (builtin.os.tag != .linux) {
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = "Linux disassembly assertion runs only on Linux CI hosts" };
    }

    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "default_echo.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default app path: {}", .{err});
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "default_echo_linux" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default app output path: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});

    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = default_platform_linux_disassembly_app }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write default app: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--opt=speed", "--no-cache", "--target=x64musl", out_arg },
        .roc_file = app_path,
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
    })) |failure| return failure;

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before llvm-objdump started");
    const objdump_result = runLlvmObjdump(io, allocator, env, output_path, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "llvm-objdump spawn error: {}", .{err});
    if (objdump_result == null) {
        return .{ .status = .skip, .phase = .run, .duration_ns = timer.read(), .run_ns = timer.read(), .message = "llvm-objdump unavailable on this Linux runner" };
    }
    if (checkCommandExpectation(allocator, objdump_result.?, .{ .args = &.{} })) |message| {
        return failureFromRun(allocator, timer, objdump_result.?, message);
    }

    const actual = normalizedObjdumpInstructions(allocator, objdump_result.?.stdout) catch |err|
        return customInfraFailure(allocator, timer, "failed to normalize llvm-objdump output: {}", .{err});

    if (!std.mem.eql(u8, expected_default_platform_linux_disassembly, actual)) {
        return customFailure(
            allocator,
            timer,
            "default platform linux disassembly mismatch\nexpected:\n{s}\nactual:\n{s}",
            .{ expected_default_platform_linux_disassembly, actual },
        );
    }

    return null;
}

fn runLlvmObjdump(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    output_path: []const u8,
    timeout_ms: u64,
) CliRunnerError!?std.process.RunResult {
    const candidates = [_][]const []const u8{
        &.{ "llvm-objdump", "-d", "--no-show-raw-insn", "--symbolize-operands", output_path },
        &.{ "/usr/lib/llvm-18/bin/llvm-objdump", "-d", "--no-show-raw-insn", "--symbolize-operands", output_path },
        &.{ "/usr/bin/llvm-objdump", "-d", "--no-show-raw-insn", "--symbolize-operands", output_path },
    };

    for (candidates) |argv| {
        return runRawInEnv(io, allocator, env, argv, project_root_path, null, timeout_ms) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => |other| return other,
        };
    }
    return null;
}

fn normalizedObjdumpInstructions(allocator: Allocator, objdump_stdout: []const u8) CliRunnerError![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .empty;
    errdefer result.deinit(allocator);

    var lines = std.mem.splitScalar(u8, objdump_stdout, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (line.len == 0 or !isHexDigit(line[0])) continue;

        const colon = std.mem.findScalar(u8, line, ':') orelse continue;
        var instruction = std.mem.trim(u8, line[colon + 1 ..], " \t\r");
        if (std.mem.findScalar(u8, instruction, '#')) |comment| {
            instruction = std.mem.trim(u8, instruction[0..comment], " \t\r");
        }
        if (instruction.len == 0) continue;

        try appendCanonicalInstruction(allocator, &result, instruction);
    }

    return try result.toOwnedSlice(allocator);
}

fn appendCanonicalInstruction(allocator: Allocator, result: *std.ArrayListUnmanaged(u8), instruction: []const u8) CliRunnerError!void {
    var canonical: std.ArrayListUnmanaged(u8) = .empty;
    defer canonical.deinit(allocator);

    var tokens = std.mem.tokenizeAny(u8, instruction, " \t");
    var first = true;
    while (tokens.next()) |token| {
        if (!first) try canonical.append(allocator, ' ');
        try canonical.appendSlice(allocator, token);
        first = false;
    }

    if (std.mem.startsWith(u8, canonical.items, "leaq ") and std.mem.find(u8, canonical.items, "(%rip), %rsi") != null) {
        try result.appendSlice(allocator, "leaq msg(%rip), %rsi\n");
        return;
    }

    try result.appendSlice(allocator, canonical.items);
    try result.append(allocator, '\n');
}

fn isHexDigit(byte: u8) bool {
    return (byte >= '0' and byte <= '9') or
        (byte >= 'a' and byte <= 'f') or
        (byte >= 'A' and byte <= 'F');
}

const DefaultPlatformTarget = enum {
    x64musl,
    arm64musl,
    x64glibc,
    arm64glibc,
    x64mac,
    arm64mac,
    x64win,
    arm64win,
    wasm32,

    fn cliName(self: DefaultPlatformTarget) []const u8 {
        return @tagName(self);
    }

    fn canBuildOnHost(self: DefaultPlatformTarget) bool {
        return switch (self) {
            .x64glibc, .arm64glibc => builtin.os.tag == .linux,
            else => true,
        };
    }

    fn canRunOnHost(self: DefaultPlatformTarget) bool {
        return switch (builtin.os.tag) {
            .linux => switch (builtin.cpu.arch) {
                .x86_64 => self == .x64musl or self == .x64glibc,
                .aarch64 => self == .arm64musl or self == .arm64glibc,
                else => false,
            },
            .macos => switch (builtin.cpu.arch) {
                .x86_64 => self == .x64mac,
                .aarch64 => self == .arm64mac,
                else => false,
            },
            .windows => switch (builtin.cpu.arch) {
                .x86_64 => self == .x64win,
                .aarch64 => self == .arm64win,
                else => false,
            },
            .freestanding => false,
            else => false,
        };
    }
};

const DefaultPlatformDiagnosticKind = enum {
    crash,
    stack_overflow,

    fn fileStem(self: DefaultPlatformDiagnosticKind) []const u8 {
        return switch (self) {
            .crash => "crash",
            .stack_overflow => "stack_overflow",
        };
    }

    fn source(self: DefaultPlatformDiagnosticKind) []const u8 {
        return switch (self) {
            .crash => default_platform_crash_debug_app,
            .stack_overflow => default_platform_stack_overflow_debug_app,
        };
    }
};

const default_platform_crash_debug_app =
    \\trigger! : {} => {}
    \\trigger! = |_| {
    \\    crash "default platform crash contract"
    \\}
    \\
    \\main! = |_| {
    \\    trigger!({})
    \\    Ok({})
    \\}
    \\
;

const default_platform_stack_overflow_debug_app =
    \\recurse : U64 => U64
    \\recurse = |n|
    \\    1 + recurse(n + 1)
    \\
    \\main! = |_| {
    \\    value = recurse(0)
    \\
    \\    if value == 0 {
    \\        crash "unreachable after recursive overflow"
    \\    } else {
    \\        Ok({})
    \\    }
    \\}
    \\
;

const default_platform_echo_app =
    \\main! = |_| {
    \\    echo!("Hello, World!")
    \\    Ok({})
    \\}
    \\
;

fn customDefaultPlatformBuild(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    target: DefaultPlatformTarget,
) ?TestResult {
    if (!target.canBuildOnHost()) {
        const message = std.fmt.allocPrint(
            allocator,
            "{s} default-platform build requires Linux host support",
            .{target.cliName()},
        ) catch "default-platform build requires Linux host support";
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = message };
    }

    const app_filename = std.fmt.allocPrint(allocator, "default_platform_build_{s}.roc", .{target.cliName()}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform app filename: {}", .{err});
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, app_filename }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform app path: {}", .{err});
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "default_platform_build" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform output path: {}", .{err});
    const target_arg = std.fmt.allocPrint(allocator, "--target={s}", .{target.cliName()}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate target arg: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});

    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = default_platform_echo_app }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write default platform app: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--opt=speed", "--no-cache", target_arg, out_arg },
        .roc_file = app_path,
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
    })) |failure| return failure;

    if (target == .wasm32) {
        var file = std.Io.Dir.cwd().openFile(io, output_path, .{ .mode = .read_only }) catch |err|
            return customInfraFailure(allocator, timer, "failed to open built wasm archive: {}", .{err});
        defer file.close(io);

        var magic: [8]u8 = undefined;
        const bytes_read = file.readPositionalAll(io, &magic, 0) catch |err|
            return customInfraFailure(allocator, timer, "failed to read built wasm archive: {}", .{err});
        if (bytes_read != magic.len or !std.mem.eql(u8, magic[0..], "!<arch>\n")) {
            return customFailure(allocator, timer, "wasm32 default platform output was not an archive", .{});
        }
    }

    if (target.canRunOnHost()) {
        const executable_path = runnableOutputPath(io, allocator, output_path) catch |err|
            return customInfraFailure(allocator, timer, "failed to find built executable: {}", .{err});

        if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{executable_path}, env.dirs.work_dir, .{
            .args = &.{},
            .stdout_exact = "Hello, World!\n",
            .stderr_exact = "",
        })) |failure| return failure;
    }

    return null;
}

fn customDefaultPlatformWasm32ArchiveReproducible(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "default_platform_wasm32_repro.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform app path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = default_platform_echo_app }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write default platform app: {}", .{err});

    const opts = [_][]const u8{ "dev", "speed", "size" };
    for (opts) |opt| {
        const output_path = std.fmt.allocPrint(allocator, "{s}/default_platform_wasm32_repro_{s}.a", .{ env.dirs.work_dir, opt }) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate default platform output path: {}", .{err});
        const opt_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{opt}) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
        const out_arg = outputArg(allocator, output_path) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});

        if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
            .args = &.{ "build", "--no-cache", "--target=wasm32", opt_arg, out_arg },
            .roc_file = app_path,
            .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
        })) |failure| return failure;

        const first = std.Io.Dir.cwd().readFileAlloc(io, output_path, allocator, .limited(64 * 1024 * 1024)) catch |err|
            return customInfraFailure(allocator, timer, "failed to read first archive {s}: {}", .{ output_path, err });
        defer allocator.free(first);

        const member_basename = if (std.mem.eql(u8, opt, "dev"))
            "roc_app_wasm32.o"
        else
            std.fmt.allocPrint(allocator, "roc_app_llvm_wasm32_{s}.o", .{opt}) catch |err|
                return customInfraFailure(allocator, timer, "failed to allocate archive member check: {}", .{err});
        const bad_forward_member = std.fmt.allocPrint(allocator, "/{s}/", .{member_basename}) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate archive member check: {}", .{err});
        const bad_backslash_member = std.fmt.allocPrint(allocator, "\\{s}/", .{member_basename}) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate archive member check: {}", .{err});
        if (std.mem.find(u8, first, bad_forward_member) != null or
            std.mem.find(u8, first, bad_backslash_member) != null)
        {
            return customFailure(allocator, timer, "default wasm {s} archive leaked an object path into its member name", .{opt});
        }

        std.Io.Dir.cwd().deleteFile(io, output_path) catch |err|
            return customInfraFailure(allocator, timer, "failed to delete first archive {s}: {}", .{ output_path, err });

        if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
            .args = &.{ "build", "--no-cache", "--target=wasm32", opt_arg, out_arg },
            .roc_file = app_path,
            .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
        })) |failure| return failure;

        const second = std.Io.Dir.cwd().readFileAlloc(io, output_path, allocator, .limited(64 * 1024 * 1024)) catch |err|
            return customInfraFailure(allocator, timer, "failed to read second archive {s}: {}", .{ output_path, err });
        defer allocator.free(second);

        if (!std.mem.eql(u8, first, second)) {
            return customFailure(allocator, timer, "default wasm {s} archive bytes were not reproducible", .{opt});
        }
    }

    return null;
}

fn customMacosOutputBasenameReproducible(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    if (builtin.os.tag != .macos) {
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = "macOS Mach-O reproducibility test only runs on macOS" };
    }

    const default_app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "default_platform_macos_repro.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform app path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = default_app_path, .data = default_platform_echo_app }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write default platform app: {}", .{err});

    const opts = [_][]const u8{ "dev", "speed", "size" };
    for (opts) |opt| {
        if (checkMacosOutputBasenameReproducible(io, allocator, env, timer, timeout_ms, opt, "real_platform", "test/fx/hello_world.roc", "Hello, world!\n")) |failure| return failure;
        if (checkMacosOutputBasenameReproducible(io, allocator, env, timer, timeout_ms, opt, "default_platform", default_app_path, "Hello, World!\n")) |failure| return failure;
    }

    return null;
}

fn checkMacosOutputBasenameReproducible(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    opt: []const u8,
    label: []const u8,
    roc_file: []const u8,
    expected_stdout: []const u8,
) ?TestResult {
    const short_output = std.fmt.allocPrint(allocator, "{s}/{s}_a_{s}", .{ env.dirs.work_dir, label, opt }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate short macOS output path: {}", .{err});
    const long_output = std.fmt.allocPrint(allocator, "{s}/very-long-{s}-output-name-for-macos-repro-{s}", .{ env.dirs.work_dir, label, opt }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate long macOS output path: {}", .{err});
    const opt_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{opt}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    const short_out_arg = outputArg(allocator, short_output) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate short output arg: {}", .{err});
    const long_out_arg = outputArg(allocator, long_output) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate long output arg: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--no-cache", opt_arg, short_out_arg },
        .roc_file = roc_file,
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
    })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--no-cache", opt_arg, long_out_arg },
        .roc_file = roc_file,
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
    })) |failure| return failure;

    const short_bytes = std.Io.Dir.cwd().readFileAlloc(io, short_output, allocator, .limited(256 * 1024 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read short macOS output {s}: {}", .{ short_output, err });
    defer allocator.free(short_bytes);
    const long_bytes = std.Io.Dir.cwd().readFileAlloc(io, long_output, allocator, .limited(256 * 1024 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read long macOS output {s}: {}", .{ long_output, err });
    defer allocator.free(long_bytes);

    if (!std.mem.eql(u8, short_bytes, long_bytes)) {
        return customFailure(allocator, timer, "macOS {s} {s} output bytes changed when only the output basename changed", .{ label, opt });
    }

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{short_output}, env.dirs.work_dir, .{
        .args = &.{},
        .stdout_exact = expected_stdout,
        .stderr_exact = "",
    })) |failure| return failure;

    return null;
}

fn customDefaultPlatformDebugBacktrace(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    target: DefaultPlatformTarget,
    kind: DefaultPlatformDiagnosticKind,
) ?TestResult {
    if (!target.canRunOnHost()) {
        const message = std.fmt.allocPrint(
            allocator,
            "{s} debug-backtrace check runs only on a matching host",
            .{target.cliName()},
        ) catch "debug-backtrace check runs only on a matching host";
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = message };
    }

    if (target != .x64musl and target != .arm64musl) {
        const message = std.fmt.allocPrint(
            allocator,
            "{s} default-platform diagnostics runtime is not implemented yet",
            .{target.cliName()},
        ) catch "default-platform diagnostics runtime is not implemented yet";
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = message };
    }

    if (target == .arm64musl) {
        return .{ .status = .skip, .phase = .setup, .duration_ns = timer.read(), .message = "arm64musl default-platform diagnostics need ARM64 unwinding support" };
    }

    const app_filename = std.fmt.allocPrint(allocator, "default_platform_{s}_{s}.roc", .{ kind.fileStem(), target.cliName() }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform app filename: {}", .{err});
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, app_filename }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform app path: {}", .{err});
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "default_platform_diagnostic" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate default platform output path: {}", .{err});
    const target_arg = std.fmt.allocPrint(allocator, "--target={s}", .{target.cliName()}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate target arg: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});

    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = kind.source() }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write default platform app: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--opt=speed", "--debug", "--no-cache", target_arg, out_arg },
        .roc_file = app_path,
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
    })) |failure| return failure;

    const executable_path = runnableOutputPath(io, allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to find built executable: {}", .{err});
    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before default platform app started");
    const run_result = runRawInEnv(io, allocator, env, &.{executable_path}, env.dirs.work_dir, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "default platform app spawn error: {}", .{err});

    const expected_contains: []const OutputNeedle = switch (kind) {
        .crash => &.{
            .{ .stream = .stderr, .text = "Roc application crashed with this message:\n\n\tdefault platform crash contract\n\n" },
            .{ .stream = .stderr, .text = "Backtrace:" },
            .{ .stream = .stderr, .text = "\x1b[94mtrigger!\x1b[0m" },
            .{ .stream = .stderr, .text = "\x1b[94mmain!\x1b[0m" },
            .{ .stream = .stderr, .text = " main:" },
        },
        .stack_overflow => &.{
            .{ .stream = .stderr, .text = "Roc application overflowed its stack memory\n\n" },
            .{ .stream = .stderr, .text = "Backtrace:" },
            .{ .stream = .stderr, .text = "\x1b[94mrecurse\x1b[0m" },
        },
    };

    if (checkCommandExpectation(allocator, run_result, .{
        .args = &.{},
        .exit = .failure,
        .stderr_min_len = 1,
        .contains = expected_contains,
        .not_contains = &.{
            .{ .stream = .stderr, .text = "Segmentation fault" },
            .{ .stream = .stderr, .text = "panic" },
            .{ .stream = .stderr, .text = "Roc " ++ "crashed:" },
            .{ .stream = .stderr, .text = "Stack overflow" },
            .{ .stream = .stderr, .text = " at " },
            .{ .stream = .stderr, .text = "  0x" },
        },
    })) |message| return failureFromRun(allocator, timer, run_result, message);

    return null;
}

fn runnableOutputPath(io: std.Io, allocator: Allocator, output_path: []const u8) CliRunnerError![]const u8 {
    std.Io.Dir.cwd().access(io, output_path, .{}) catch |err| {
        if (builtin.os.tag != .windows) return err;
        const exe_path = try std.fmt.allocPrint(allocator, "{s}.exe", .{output_path});
        std.Io.Dir.cwd().access(io, exe_path, .{}) catch return err;
        return exe_path;
    };
    return output_path;
}

const GeneratedModuleGraphConfig = struct {
    roc_file_count: usize,
    symbols_per_file: usize,
};

/// Regression test for builtin inlining: a native `--opt=speed` archive build must
/// inline list builtins (link builtins.bc) rather than leave them as opaque external
/// calls. If the builtin symbol naming ever drifts between the codegen and the
/// bitcode, the inlining silently stops; this catches that by asserting the archive's
/// app object has no remaining reference to `roc_builtins_list_append_unsafe`.
fn customListBuiltinInlined(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
) ?TestResult {
    const plat_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "inline_plat.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate platform path: {}", .{err});
    const app_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "inline_app.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate app path: {}", .{err});
    const archive_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "inline_app.a" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate archive path: {}", .{err});
    const output_arg = std.fmt.allocPrint(allocator, "--output={s}", .{archive_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});

    const plat_src =
        \\platform ""
        \\    requires {} { main! : () => List(I32) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    targets: {
        \\        inputs_dir: "targets/",
        \\        arm64mac: { inputs: [app], output: Archive },
        \\        x64mac: { inputs: [app], output: Archive },
        \\        arm64musl: { inputs: [app], output: Archive },
        \\        x64musl: { inputs: [app], output: Archive },
        \\        arm64glibc: { inputs: [app], output: Archive },
        \\        x64glibc: { inputs: [app], output: Archive },
        \\        arm64win: { inputs: [app], output: Archive },
        \\        x64win: { inputs: [app], output: Archive },
        \\    }
        \\
        \\main_for_host! : () => List(I32)
        \\main_for_host! = || main!()
        \\
    ;
    const app_src =
        \\app [main!] { pf: platform "./inline_plat.roc" }
        \\
        \\main! : () => List(I32)
        \\main! = || [1.I32, 2, 3].map(|x| x + 1)
        \\
    ;
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = plat_path, .data = plat_src }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write platform: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = app_path, .data = app_src }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write app: {}", .{err});

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc build started");
    const result = runRocInEnv(io, allocator, env, &.{ "build", "--opt=speed", "--no-cache", output_arg }, app_path, .absolute, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "roc build spawn error: {}", .{err});
    if (checkCommandExpectation(allocator, result, .{ .args = &.{"build"}, .exit = .success })) |message| {
        return failureFromRun(allocator, timer, result, message);
    }

    const archive_bytes = std.Io.Dir.cwd().readFileAlloc(io, archive_path, allocator, .limited(64 * 1024 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read archive {s}: {}", .{ archive_path, err });
    if (std.mem.find(u8, archive_bytes, "roc_builtins_list_append_unsafe") != null) {
        return customFailure(allocator, timer, "list_append_unsafe was not inlined into the --opt=speed archive object (it still references the builtin symbol)", .{});
    }
    return null;
}

fn customGeneratedModuleGraph(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    config: GeneratedModuleGraphConfig,
) ?TestResult {
    if (writeGeneratedModuleGraphProject(io, allocator, env.dirs.work_dir, config)) |main_path| {
        const cache_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "roc-cache" }) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate cache path: {}", .{err});
        const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
            return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc check started");
        const result = runRocInEnv(io, allocator, env, &.{"check"}, main_path, .absolute, null, child_timeout_ms) catch |err|
            return customInfraFailure(allocator, timer, "roc check spawn error: {}", .{err});
        if (checkCommandExpectation(allocator, result, .{ .args = &.{"check"}, .exit = .success })) |message| {
            return failureFromRun(allocator, timer, result, message);
        }
        const cached_module_count = countModuleCacheFiles(io, allocator, cache_path) catch |err|
            return customInfraFailure(allocator, timer, "failed to count module cache files: {}", .{err});
        if (cached_module_count != config.roc_file_count) {
            return customFailure(allocator, timer, "expected {d} cached module files, found {d}", .{ config.roc_file_count, cached_module_count });
        }
    } else |err| {
        return customInfraFailure(allocator, timer, "failed to write generated module graph: {}", .{err});
    }
    return null;
}

fn writeGeneratedModuleGraphProject(
    io: std.Io,
    allocator: Allocator,
    dir_path: []const u8,
    config: GeneratedModuleGraphConfig,
) CliRunnerError![]const u8 {
    if (config.roc_file_count == 0 or config.symbols_per_file == 0) return error.InvalidGeneratedGraphConfig;

    var dir = try std.Io.Dir.openDirAbsolute(io, dir_path, .{});
    defer dir.close(io);

    try writeGeneratedPackageModule(io, dir, config);

    var module_idx: usize = 1;
    while (module_idx < config.roc_file_count) : (module_idx += 1) {
        try writeGeneratedTypeModule(io, allocator, dir, config, module_idx);
    }

    return try std.fs.path.join(allocator, &.{ dir_path, "main.roc" });
}

fn writeGeneratedPackageModule(io: std.Io, dir: std.Io.Dir, config: GeneratedModuleGraphConfig) CliRunnerError!void {
    var file = try dir.createFile(io, "main.roc", .{});
    defer file.close(io);

    var write_buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &write_buffer);
    const out = &writer.interface;

    const type_module_count = config.roc_file_count - 1;
    try out.writeAll("package [\n");
    var module_idx: usize = 1;
    while (module_idx <= type_module_count) : (module_idx += 1) {
        try out.print("    T{d},\n", .{module_idx});
    }
    try out.writeAll("] {}\n\n");

    module_idx = 1;
    while (module_idx <= type_module_count) : (module_idx += 1) {
        try out.print("import T{d}\n", .{module_idx});
    }
    try out.writeAll("\n");

    var symbol_idx: usize = 1;
    while (symbol_idx <= config.symbols_per_file) : (symbol_idx += 1) {
        try out.print("p{d} : {{}}\n", .{symbol_idx});
        if (symbol_idx == 1) {
            if (type_module_count > 0) {
                try out.writeAll("p1 = T1.s1\n\n");
            } else {
                try out.writeAll("p1 = {}\n\n");
            }
        } else {
            try out.print("p{d} = p{d}\n\n", .{ symbol_idx, symbol_idx - 1 });
        }
    }
    try out.flush();
}

fn writeGeneratedTypeModule(
    io: std.Io,
    allocator: Allocator,
    dir: std.Io.Dir,
    config: GeneratedModuleGraphConfig,
    module_idx: usize,
) CliRunnerError!void {
    const file_name = try std.fmt.allocPrint(allocator, "T{d}.roc", .{module_idx});
    var file = try dir.createFile(io, file_name, .{});
    defer file.close(io);

    var write_buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &write_buffer);
    const out = &writer.interface;

    if (module_idx > 1) {
        try out.writeAll("import T1\n\n");
    }

    try out.print("T{d} := [].{{\n", .{module_idx});
    var symbol_idx: usize = 1;
    while (symbol_idx <= config.symbols_per_file) : (symbol_idx += 1) {
        try out.print("    s{d} : {{}}\n", .{symbol_idx});
        if (symbol_idx == 1) {
            if (module_idx > 1) {
                try out.writeAll("    s1 = T1.s1\n\n");
            } else {
                try out.writeAll("    s1 = {}\n\n");
            }
        } else {
            try out.print("    s{d} = s{d}\n\n", .{ symbol_idx, symbol_idx - 1 });
        }
    }
    try out.writeAll("}\n");
    try out.flush();
}

fn countModuleCacheFiles(io: std.Io, allocator: Allocator, cache_path: []const u8) CliRunnerError!usize {
    var cache_dir = std.Io.Dir.cwd().openDir(io, cache_path, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => return 0,
        else => return err,
    };
    defer cache_dir.close(io);

    var walker = try cache_dir.walk(allocator);
    defer walker.deinit();

    var count: usize = 0;
    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (std.mem.endsWith(u8, entry.basename, ".meta")) continue;
        if (std.mem.endsWith(u8, entry.basename, ".tmp")) continue;
        count += 1;
    }
    return count;
}

fn customFmtReformatsFile(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const source = std.Io.Dir.cwd().readFileAlloc(io, "test/cli/needs_formatting.roc", allocator, .limited(10 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read source: {}", .{err});
    const temp_file_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "temp_format.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate temp file path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = temp_file_path, .data = source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write temp file: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{"fmt"}, .roc_file = temp_file_path })) |failure| return failure;

    const formatted = std.Io.Dir.cwd().readFileAlloc(io, temp_file_path, allocator, .limited(10 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read formatted temp file: {}", .{err});
    if (formatted.len == source.len) {
        return customFailure(allocator, timer, "formatting did not change file size", .{});
    }
    if (formatted.len == 0) {
        return customFailure(allocator, timer, "formatted file was empty", .{});
    }
    return null;
}

fn customFmtDoesNotChangeFile(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const source = std.Io.Dir.cwd().readFileAlloc(io, "test/cli/well_formatted.roc", allocator, .limited(10 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read source: {}", .{err});
    const temp_file_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "well_formatted.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate temp file path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = temp_file_path, .data = source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write temp file: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{"fmt"}, .roc_file = temp_file_path })) |failure| return failure;

    const after = std.Io.Dir.cwd().readFileAlloc(io, temp_file_path, allocator, .limited(10 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read temp file after formatting: {}", .{err});
    if (!std.mem.eql(u8, source, after)) {
        return customFailure(allocator, timer, "well-formatted file changed after roc fmt", .{});
    }
    return null;
}

fn customFmtStdin(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    expect_same: bool,
) ?TestResult {
    const source_path = if (expect_same) "test/cli/well_formatted.roc" else "test/cli/needs_formatting.roc";
    const input = std.Io.Dir.cwd().readFileAlloc(io, source_path, allocator, .limited(10 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read stdin source: {}", .{err});
    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc fmt --stdin started");
    const result = runRocInEnv(io, allocator, env, &.{ "fmt", "--stdin" }, null, .absolute, input, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "roc fmt --stdin spawn error: {}", .{err});
    if (checkCommandExpectation(allocator, result, .{ .args = &.{ "fmt", "--stdin" } })) |message| {
        return failureFromRun(allocator, timer, result, message);
    }
    const same = std.mem.eql(u8, result.stdout, input);
    if (expect_same and !same) return customFailure(allocator, timer, "formatted stdin changed unexpectedly", .{});
    if (!expect_same and same) return customFailure(allocator, timer, "unformatted stdin was not changed", .{});
    if (result.stdout.len == 0) return customFailure(allocator, timer, "roc fmt --stdin produced empty output", .{});
    return null;
}

fn customBuildIntCreatesOutput(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "test_app" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output path: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--opt=interpreter", out_arg },
        .roc_file = "test/int/app.roc",
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
        .stdout_min_len = 6,
    })) |failure| return failure;
    const size = fileExistsWithSize(io, output_path) catch |err|
        return customFailure(allocator, timer, "failed to stat output file: {}", .{err});
    if (size == 0) return customFailure(allocator, timer, "output file was empty", .{});
    return null;
}

fn customBuildIntOutputRuns(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    backend: OptMode,
) ?TestResult {
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, if (backend == .dev) "test_app_dev" else "test_app" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output path: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});

    const args: []const []const u8 = if (backend == .dev)
        &.{ "build", opt_arg, "--no-cache", out_arg }
    else
        &.{ "build", opt_arg, out_arg };
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = args, .roc_file = "test/int/app.roc" })) |failure| return failure;
    const size = fileExistsWithSize(io, output_path) catch |err|
        return customFailure(allocator, timer, "failed to stat output file: {}", .{err});
    if (size == 0) return customFailure(allocator, timer, "output file was empty", .{});

    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before built app started");
    const run_result = runRawInEnv(io, allocator, env, &.{output_path}, env.dirs.work_dir, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "built app spawn error: {}", .{err});
    const expected_text = if (backend == .dev) "ALL TESTS PASSED" else "SUCCESS";
    const alternate_text = if (backend == .dev) "ALL TESTS PASSED" else "PASSED";
    if (checkCommandExpectation(allocator, run_result, .{
        .args = &.{},
        .contains_any = &.{.{ .needles = &.{ .{ .stream = .stdout, .text = expected_text }, .{ .stream = .stdout, .text = alternate_text } } }},
    })) |message| return failureFromRun(allocator, timer, run_result, message);
    return null;
}

fn customGlibcTargetNonLinux(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    if (builtin.os.tag == .linux) return null;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--target=x64glibc" },
        .roc_file = "test/int/app.roc",
        .exit = .failure,
        .contains = &.{ .{ .stream = .stderr, .text = "glibc" }, .{ .stream = .stderr, .text = "musl" } },
    })) |failure| return failure;
    return null;
}

/// Shared output on COFF: link the dylib test app and its host into one DLL.
/// lld-link resolves the app/host symbol references in a single pass, the
/// same as the ELF and Mach-O shared-library links. Windows-only: the link
/// needs the native Windows SDK.
fn customWindowsSharedLibrary(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    if (builtin.os.tag != .windows) return null;

    const dll_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "app.dll" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate dll path: {}", .{err});
    const output_arg = std.fmt.allocPrint(allocator, "--output={s}", .{dll_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--no-cache", output_arg },
        .roc_file = "test/dylib/app.roc",
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
    })) |failure| return failure;

    const dll_bytes = std.Io.Dir.cwd().readFileAlloc(io, dll_path, allocator, .limited(256 * 1024 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read built DLL {s}: {}", .{ dll_path, err });

    // The host's unused canary blob must be dead-stripped from the DLL. A
    // presence check on the used hosted symbol's name would prove nothing
    // here: linked PE images carry no symbol table, so internal names vanish
    // even though the code survives; the ELF and Mach-O shared-library tests
    // cover the positive side.
    if (std.mem.find(u8, dll_bytes, "ROC_DCE_CANARY_BLOB_7f3a9c") != null) {
        return customFailure(allocator, timer, "unused host canary blob was not dead-stripped from the DLL", .{});
    }
    return null;
}

fn customDefaultAppAllSyntaxCheckedCache(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const command = CommandCase{
        .args = &.{"--opt=interpreter"},
        .roc_file = "test/echo/all_syntax_test.roc",
        .stdout_exact = all_syntax_expected_stdout,
        .stderr_exact = all_syntax_expected_stderr,
    };

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, command)) |failure| return failure;

    const cached_module_count_after_first_run = countModuleCacheFiles(io, allocator, env.dirs.roc_cache_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to count module cache files: {}", .{err});
    if (cached_module_count_after_first_run == 0) {
        return customFailure(allocator, timer, "expected default app run to populate checked module cache entries before the second run, found 0", .{});
    }

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, command)) |failure| return failure;

    const cached_module_count_after_second_run = countModuleCacheFiles(io, allocator, env.dirs.roc_cache_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to count module cache files after second run: {}", .{err});
    if (cached_module_count_after_second_run != cached_module_count_after_first_run) {
        return customFailure(allocator, timer, "expected second default app run to reuse {d} checked module cache entries, found {d}", .{ cached_module_count_after_first_run, cached_module_count_after_second_run });
    }

    return null;
}

fn customCachePassingResults(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/AllPassTests.roc" })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/AllPassTests.roc", .contains = &.{.{ .stream = .stdout, .text = "(cached)" }} })) |failure| return failure;
    return null;
}

fn customCacheFailingResults(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 } })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{.{ .stream = .stderr, .text = "(cached)" }} })) |failure| return failure;
    return null;
}

fn customCacheInvalidated(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    const file_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "CacheTest.roc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate cache test path: {}", .{err});
    const source_content =
        \\CacheTest := {}
        \\add = |a, b| a + b
        \\expect { add(1, 2) == 3 }
        \\
    ;
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = file_path, .data = source_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write cache test file: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = file_path })) |failure| return failure;

    const updated_content =
        \\CacheTest := {}
        \\add = |a, b| a + b
        \\expect { add(2, 3) == 5 }
        \\
    ;
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = file_path, .data = updated_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to update cache test file: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = file_path, .not_contains = &.{.{ .stream = .stdout, .text = "(cached)" }} })) |failure| return failure;
    return null;
}

fn customVerboseWorksFromCache(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/AllPassTests.roc" })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg, "--verbose" }, .roc_file = "test/cli/AllPassTests.roc", .contains = &.{ .{ .stream = .stdout, .text = "(cached)" }, .{ .stream = .stdout, .text = "PASS" } } })) |failure| return failure;
    return null;
}

fn customVerboseCachesFailureReports(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg, "--verbose" }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{.{ .stream = .stderr, .text = "FAIL" }} })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg, "--verbose" }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "(cached)" }, .{ .stream = .stderr, .text = "FAIL" } } })) |failure| return failure;
    return null;
}

fn customNonVerboseCachesVerboseReports(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .not_contains = &.{.{ .stream = .stderr, .text = "expect failed" }} })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{ .args = &.{ "test", opt_arg, "--verbose" }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "(cached)" }, .{ .stream = .stderr, .text = "expect" }, .{ .stream = .stderr, .text = "TEST FAILURE" } } })) |failure| return failure;
    return null;
}

fn customVerboseAndNonVerboseFailureFormatMatch(io: std.Io, allocator: Allocator, timer: *harness.Timer, timeout_ms: u64, backend: OptMode) ?TestResult {
    const opt_arg = backendOptArg(allocator, backend) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate opt arg: {}", .{err});
    var env1 = buildCaseEnv(io, allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to create first environment: {}", .{err});
    defer env1.deinit(allocator);
    var env2 = buildCaseEnv(io, allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to create second environment: {}", .{err});
    defer env2.deinit(allocator);

    if (runRocAndCheck(io, allocator, &env1, timer, timeout_ms, .{ .args = &.{ "test", opt_arg }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "\u{2502}" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" } } })) |failure| return failure;
    if (runRocAndCheck(io, allocator, &env2, timer, timeout_ms, .{ .args = &.{ "test", opt_arg, "--verbose" }, .roc_file = "test/cli/SomeFailTests.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "\u{2502}" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" } } })) |failure| return failure;
    return null;
}

fn customBuildWarningInterpreter(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "test_app_warning" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output path: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "build", "--opt=interpreter", out_arg },
        .roc_file = "test/fx/run_warning_only.roc",
        .exit = .{ .code = 2 },
        .contains = &.{.{ .stream = .stdout, .text = "successfully building" }},
        .contains_any = &.{.{ .needles = &warning_needles }},
    })) |failure| return failure;
    const size = fileExistsWithSize(io, output_path) catch |err|
        return customFailure(allocator, timer, "failed to stat warning output file: {}", .{err});
    if (size == 0) return customFailure(allocator, timer, "warning output file was empty", .{});
    return null;
}

fn customIssue9392Deterministic(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const path = "test/cli/Issue9392NumUtilsDeterministic.roc";
    const command = CommandCase{
        .args = &.{ "test", "--opt=interpreter", "--no-cache" },
        .roc_file = path,
        .contains = &.{.{ .stream = .stdout, .text = "All (11) tests passed" }},
        .not_contains = &.{ .{ .stream = .stdout, .text = "failed" }, .{ .stream = .stderr, .text = "FAIL:" } },
    };
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, command)) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, command)) |failure| return failure;
    return null;
}

fn customBuildIssue9435(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_path = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "hosted_nominal_return" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output path: {}", .{err});
    const out_arg = outputArg(allocator, output_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate output arg: {}", .{err});
    const child_timeout_ms = childCommandTimeoutMs(timer, timeout_ms) orelse
        return timeoutFailure(allocator, timer, .run, "case timeout exhausted before roc build started");
    const result = runRocInEnv(io, allocator, env, &.{ "build", "--opt=dev", "--no-cache", out_arg }, "test/hosted_nominal_return/repro.roc", .absolute, null, child_timeout_ms) catch |err|
        return customInfraFailure(allocator, timer, "roc build spawn error: {}", .{err});
    if (result.term == .signal or (result.term == .exited and result.term.exited == 134)) {
        return failureFromRun(allocator, timer, result, "roc build panicked or aborted");
    }
    if (std.mem.find(u8, result.stderr, "panic") != null or
        std.mem.find(u8, result.stderr, "mono nominal materialization") != null or
        std.mem.find(u8, result.stderr, "published instantiated nominal backing") != null)
    {
        return failureFromRun(allocator, timer, result, "roc build output contained forbidden panic/regression text");
    }
    return null;
}

fn customBundleComplexPackage(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const out_dir = std.fs.path.join(allocator, &.{ env.dirs.work_dir, "bundle-out" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate bundle output dir: {}", .{err});
    std.Io.Dir.cwd().createDirPath(io, out_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to create bundle output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "bundle", "--output-dir", out_dir, "test/complex_package/main.roc" },
        .contains = &.{.{ .stream = .stdout, .text = "Created:" }},
        .not_contains = &.{.{ .stream = .stderr, .text = "missing from bundle" }},
    })) |failure| return failure;
    return null;
}

fn createWorkSubdir(io: std.Io, allocator: Allocator, env: *const CaseEnv, name: []const u8) CliRunnerError![]const u8 {
    const path = try std.fs.path.join(allocator, &.{ env.dirs.work_dir, name });
    try std.Io.Dir.cwd().createDirPath(io, path);
    return path;
}

fn runGlueCommandInEnv(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    glue_spec: []const u8,
    output_dir: []const u8,
) ?TestResult {
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", glue_spec, output_dir, "test/fx/platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;
    return null;
}

fn runGlueMatrixCase(
    io: std.Io,
    allocator: Allocator,
    matrix: GlueMatrixCase,
    timeout_ms: u64,
) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .setup, .message = "no clock" };
    var env = buildCaseEnv(io, allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to create test environment" };
    defer env.deinit(allocator);

    const output_dir_name = std.fmt.allocPrint(
        allocator,
        "glue-matrix-{s}-{s}-{s}",
        .{ matrix.language.displayName(), matrix.fixture.name, matrix.target.displayName() },
    ) catch |err|
        return addPreservedWorkDirMessage(allocator, customInfraFailure(allocator, &timer, "failed to allocate glue output dir name: {}", .{err}), env.dirs.work_dir);
    const output_dir = createWorkSubdir(io, allocator, &env, output_dir_name) catch |err|
        return addPreservedWorkDirMessage(allocator, customInfraFailure(allocator, &timer, "failed to create glue output dir: {}", .{err}), env.dirs.work_dir);

    if (runGlueMatrixCommand(io, allocator, &env, &timer, timeout_ms, matrix, output_dir)) |failure| {
        return addPreservedWorkDirMessage(allocator, failure, env.dirs.work_dir);
    }

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, matrix.language.generatedFileName() }) catch |err|
        return addPreservedWorkDirMessage(allocator, customInfraFailure(allocator, &timer, "failed to allocate generated glue path: {}", .{err}), env.dirs.work_dir);

    const compile_failure = switch (matrix.language) {
        .zig => compileGeneratedZigGlue(io, allocator, &env, &timer, timeout_ms, matrix, output_dir, generated_path),
        .rust => compileGeneratedRustGlue(io, allocator, &env, &timer, timeout_ms, matrix, output_dir, generated_path),
        .c => compileGeneratedCGlue(io, allocator, &env, &timer, timeout_ms, matrix, output_dir, generated_path),
    };
    if (compile_failure) |failure| {
        return addPreservedWorkDirMessage(allocator, failure, env.dirs.work_dir);
    }

    util.cleanupTestWorkDir(io, env.dirs.work_dir);
    const elapsed = timer.read();
    return .{ .status = .pass, .phase = .run, .duration_ns = elapsed, .run_ns = elapsed };
}

fn runGlueMatrixCommand(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    matrix: GlueMatrixCase,
    output_dir: []const u8,
) ?TestResult {
    var args: std.ArrayListUnmanaged([]const u8) = .empty;
    args.append(allocator, "glue") catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate glue command args: {}", .{err});
    if (matrix.execution_mode.optArg()) |opt_arg| {
        args.append(allocator, opt_arg) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate glue command args: {}", .{err});
    }
    args.append(allocator, matrix.language.glueSpec()) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate glue command args: {}", .{err});
    args.append(allocator, output_dir) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate glue command args: {}", .{err});
    args.append(allocator, matrix.fixture.platform_path) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate glue command args: {}", .{err});

    const owned_args = args.toOwnedSlice(allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate glue command args: {}", .{err});

    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = owned_args,
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    return null;
}

fn compileGeneratedZigGlue(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    matrix: GlueMatrixCase,
    output_dir: []const u8,
    _: []const u8,
) ?TestResult {
    const test_zig_content = std.fmt.allocPrint(allocator,
        \\const abi = @import("{s}");
        \\
        \\comptime {{
        \\    _ = abi.RocStr;
        \\    _ = abi.RocList;
        \\    _ = abi.RocBox;
        \\    _ = abi.RocHost;
        \\}}
        \\
        \\export fn _roc_glue_matrix_check() void {{
        \\    var host: abi.RocHost = undefined;
        \\    var str: abi.RocStr = undefined;
        \\    var list: abi.RocList(abi.RocStr) = undefined;
        \\    var box: abi.RocBox = null;
        \\    _ = &host;
        \\    _ = &str;
        \\    _ = &list;
        \\    _ = &box;
        \\}}
    , .{matrix.language.generatedFileName()}) catch |err|
        return customInfraFailure(allocator, timer, "failed to render Zig matrix stub: {}", .{err});

    const test_zig_path = std.fs.path.join(allocator, &.{ output_dir, "matrix_check.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig matrix stub path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = test_zig_path, .data = test_zig_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write Zig matrix stub: {}", .{err});

    const test_o_path = std.fs.path.join(allocator, &.{ output_dir, "matrix_check.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig matrix object path: {}", .{err});
    const emit_flag = std.fmt.allocPrint(allocator, "-femit-bin={s}", .{test_o_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig emit flag: {}", .{err});

    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    argv.appendSlice(allocator, &.{ "zig", "build-obj" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig compile args: {}", .{err});
    if (matrix.target.zigTargetArg()) |target_arg| {
        argv.appendSlice(allocator, &.{ "-target", target_arg }) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate Zig compile args: {}", .{err});
    }
    argv.appendSlice(allocator, &.{ test_zig_path, emit_flag }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig compile args: {}", .{err});

    const owned_argv = argv.toOwnedSlice(allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig compile args: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, owned_argv, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn compileGeneratedRustGlue(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    matrix: GlueMatrixCase,
    output_dir: []const u8,
    generated_path: []const u8,
) ?TestResult {
    if (matrix.target != .native) {
        return customInfraFailure(allocator, timer, "Rust glue matrix target {s} is not configured; install-aware cross-target checks should add it explicitly", .{matrix.target.displayName()});
    }

    const test_rlib_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rlib" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Rust matrix rlib path: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "rustc",
        "--edition=2021",
        "-D",
        "warnings",
        "--crate-type",
        "lib",
        generated_path,
        "-o",
        test_rlib_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn compileGeneratedCGlue(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    matrix: GlueMatrixCase,
    output_dir: []const u8,
    _: []const u8,
) ?TestResult {
    const test_c_content =
        \\#include "roc_platform_abi.h"
        \\
        \\void _roc_glue_matrix_check(void) {
        \\    RocStr str = {0};
        \\    RocList list = {0};
        \\    HostedFunctions *funcs = 0;
        \\    (void)str;
        \\    (void)list;
        \\    (void)funcs;
        \\}
    ;
    const test_c_path = std.fs.path.join(allocator, &.{ output_dir, "matrix_check.c" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate C matrix stub path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = test_c_path, .data = test_c_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write C matrix stub: {}", .{err});

    const test_o_path = std.fs.path.join(allocator, &.{ output_dir, "matrix_check.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate C matrix object path: {}", .{err});
    const include_flag = std.fmt.allocPrint(allocator, "-I{s}", .{output_dir}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate C include flag: {}", .{err});

    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    argv.appendSlice(allocator, &.{ "zig", "cc" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate C compile args: {}", .{err});
    if (matrix.target.zigTargetArg()) |target_arg| {
        argv.appendSlice(allocator, &.{ "-target", target_arg }) catch |err|
            return customInfraFailure(allocator, timer, "failed to allocate C compile args: {}", .{err});
    }
    argv.appendSlice(allocator, &.{
        "-std=c11",
        "-Wall",
        "-Werror",
        "-c",
        include_flag,
        test_c_path,
        "-o",
        test_o_path,
    }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate C compile args: {}", .{err});

    const owned_argv = argv.toOwnedSlice(allocator) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate C compile args: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, owned_argv, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueDebug(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/DebugGlue.roc", output_dir, "test/fx/platform/main.roc" },
        .contains = &.{.{ .stream = .stderr, .text = "name: \"main!\"" }},
        .not_contains = &.{
            .{ .stream = .stderr, .text = "PANIC" },
            .{ .stream = .stderr, .text = "unreachable" },
            .{ .stream = .stderr, .text = "name: \"\"" },
        },
    })) |failure| return failure;
    return null;
}

fn customGlueDebugInterpreter(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "--opt=interpreter", "src/glue/src/DebugGlue.roc", output_dir, "test/fx/platform/main.roc" },
        .contains = &.{.{ .stream = .stderr, .text = "name: \"main!\"" }},
        .not_contains = &.{
            .{ .stream = .stderr, .text = "PANIC" },
            .{ .stream = .stderr, .text = "unreachable" },
            .{ .stream = .stderr, .text = "name: \"\"" },
        },
    })) |failure| return failure;
    return null;
}

fn customGluePackageNominalApiAlias(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{"check"},
        .roc_file = "test/glue/package-nominal-api/repro_app/main.roc",
        .not_contains = &.{
            .{ .stream = .stderr, .text = "PANIC" },
            .{ .stream = .stderr, .text = "unreachable" },
        },
    })) |failure| return failure;
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/RustGlue.roc", output_dir, "test/glue/package-nominal-api/platform/main.roc" },
        .exit = .not_panic,
        .not_contains = &.{
            .{ .stream = .stderr, .text = "PANIC" },
            .{ .stream = .stderr, .text = "unreachable" },
        },
    })) |failure| return failure;
    return null;
}

fn customGlueCHeader(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runGlueCommandInEnv(io, allocator, env, timer, timeout_ms, "src/glue/src/CGlue.roc", output_dir)) |failure| return failure;
    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.h" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated header path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated C header: {}", .{err});
    const expected = std.Io.Dir.cwd().readFileAlloc(io, "test/glue/fx_platform_cglue_expected.h", allocator, .limited(1024 * 1024)) catch |err|
        return customInfraFailure(allocator, timer, "failed to read expected C header: {}", .{err});
    if (!std.mem.eql(u8, generated, expected)) {
        return customFailure(allocator, timer, "generated C header mismatch: expected {d} bytes, got {d}", .{ expected.len, generated.len });
    }
    return null;
}

fn customGlueCHeaderCompiles(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runGlueCommandInEnv(io, allocator, env, timer, timeout_ms, "src/glue/src/CGlue.roc", output_dir)) |failure| return failure;
    const test_c_content =
        \\#include "roc_platform_abi.h"
        \\
        \\void test_types(void) {
        \\    RocStr str = {0};
        \\    RocList list = {0};
        \\    HostedFunctions *funcs = 0;
        \\    (void)str;
        \\    (void)list;
        \\    (void)funcs;
        \\}
    ;
    const test_c_path = std.fs.path.join(allocator, &.{ output_dir, "test_header.c" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate test C path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = test_c_path, .data = test_c_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write test C file: {}", .{err});
    const test_o_path = std.fs.path.join(allocator, &.{ output_dir, "test_header.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate test object path: {}", .{err});
    const include_flag = std.fmt.allocPrint(allocator, "-I{s}", .{output_dir}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate include flag: {}", .{err});

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "cc",
        "-c",
        "-std=c11",
        "-Wall",
        "-Werror",
        include_flag,
        test_c_path,
        "-o",
        test_o_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueZigCompiles(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runGlueCommandInEnv(io, allocator, env, timer, timeout_ms, "src/glue/src/ZigGlue.roc", output_dir)) |failure| return failure;

    // Reference the key generated ABI helper types so they are semantically analyzed.
    // Importing the file also runs its comptime size/alignment assertions. Then compile
    // to an object to confirm the generated Zig is well-formed.
    // The generated ABI file name is interpolated rather than written as a literal import
    // path so the dead-files lint does not mistake this test fixture for a real import of an
    // untracked source file.
    const test_zig_content = std.fmt.allocPrint(allocator,
        \\const abi = @import("{s}");
        \\comptime {{
        \\    if (@sizeOf(usize) == 8) {{
        \\        if (@sizeOf(abi.Padded) != 12) @compileError("Padded nominal record size regression");
        \\        if (@alignOf(abi.Padded) != 4) @compileError("Padded nominal record alignment regression");
        \\        if (@offsetOf(abi.Padded, "z") != 0) @compileError("Padded.z offset regression");
        \\        if (@offsetOf(abi.Padded, "a") != 8) @compileError("Padded.a offset regression");
        \\        if (@sizeOf(abi.PaddedCheckArgs) != 12) @compileError("Padded.check args size regression");
        \\        if (@alignOf(abi.PaddedCheckArgs) != 4) @compileError("Padded.check args alignment regression");
        \\        if (@offsetOf(abi.PaddedCheckArgs, "z") != 0) @compileError("PaddedCheckArgs.z offset regression");
        \\        if (@offsetOf(abi.PaddedCheckArgs, "a") != 8) @compileError("PaddedCheckArgs.a offset regression");
        \\    }}
        \\}}
        \\export fn _roc_glue_abi_check() void {{
        \\    var host: abi.RocHost = undefined;
        \\    var box: abi.RocBox = null;
        \\    var str: abi.RocStr = undefined;
        \\    var builder_args: abi.BuilderPrint_valueArgs = undefined;
        \\    const tree: abi.HostTree = undefined;
        \\    // Reference the nominal record `Padded` and its args struct so their
        \\    // comptime size/alignment assertions run. `Padded := {{ z, _, a }}` must
        \\    // lay out in declared order with the unnamed field reserved as padding
        \\    // (z@0, _pad0@4, a@8, size 12) for these to hold.
        \\    var padded: abi.Padded = undefined;
        \\    var padded_args: abi.PaddedCheckArgs = undefined;
        \\    _ = &host;
        \\    _ = &box;
        \\    _ = &str;
        \\    _ = &builder_args;
        \\    _ = &padded;
        \\    _ = &padded_args;
        \\    abi.increfHostTree(tree, 1);
        \\    abi.decrefHostTree(tree, &host);
        \\}}
    , .{"roc_platform_abi.zig"}) catch |err|
        return customInfraFailure(allocator, timer, "failed to render test Zig source: {}", .{err});
    const test_zig_path = std.fs.path.join(allocator, &.{ output_dir, "test_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate test Zig path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = test_zig_path, .data = test_zig_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write test Zig file: {}", .{err});
    const test_o_path = std.fs.path.join(allocator, &.{ output_dir, "test_abi.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate test object path: {}", .{err});
    const emit_flag = std.fmt.allocPrint(allocator, "-femit-bin={s}", .{test_o_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate emit flag: {}", .{err});

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "build-obj",
        test_zig_path,
        emit_flag,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueRustDuplicateTagUnions(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "rust-duplicate-tag-glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/RustGlue.roc", output_dir, "test/glue/rust-duplicate-tag-platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rs" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Rust path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Rust file: {}", .{err});

    for ([_][]const u8{
        "pub struct TryType",
        "pub struct IOErrType",
        "pub fn roc_a_nested",
        "pub fn roc_d_nested",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Rust duplicate-tag fixture missing {s}", .{needle});
        }
    }

    return null;
}

fn customGlueZigDuplicateTagUnions(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "zig-duplicate-tag-glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/ZigGlue.roc", output_dir, "test/glue/rust-duplicate-tag-platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Zig file: {}", .{err});

    for ([_][]const u8{
        "pub const TryType",
        "pub const IOErrType",
        "pub fn decrefTryType",
        "pub extern fn roc_a_nested",
        "pub extern fn roc_d_nested",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig duplicate-tag fixture missing {s}", .{needle});
        }
    }

    const test_o_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig object path: {}", .{err});
    const emit_flag = std.fmt.allocPrint(allocator, "-femit-bin={s}", .{test_o_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig emit flag: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "build-obj",
        generated_path,
        emit_flag,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    return null;
}

fn customGlueRustBoxPayloadAlignment(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    // Regression test for non-refcounted boxed payload teardown alignment.
    //
    // RustGlue.roc previously emitted `decref_box(expr as RocBox, roc_host)` for
    // boxed payloads that are known and contain no refcounted values (e.g.
    // `Box(I64)`). `decref_box` hardcodes pointer alignment
    // (`align_of::<usize>()`), so on small-pointer targets like wasm32 an
    // 8-aligned payload is freed from `base + 4` instead of `base`. The fix
    // emits `decref_box_with(expr as RocBox, align_of::<payload>(), None, roc_host)`
    // so the payload's real alignment is used to recover the allocation base.
    //
    // test/static-data-host exposes `BranchPair(Box(I64), Box(I64))`, which
    // exercises exactly this path.
    const output_dir = createWorkSubdir(io, allocator, env, "rust-box-align-glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/RustGlue.roc", output_dir, "test/static-data-host/platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rs" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Rust path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Rust file: {}", .{err});

    for ([_][]const u8{
        "decref_box_with(payload._0 as RocBox, core::mem::align_of::<i64>(), false, None, roc_host);",
        "decref_box_with(payload._1 as RocBox, core::mem::align_of::<i64>(), false, None, roc_host);",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Rust file missing payload-aligned box decref {s}", .{needle});
        }
    }
    // No boxed payload in this platform is opaque, so the pointer-aligned
    // `decref_box(payload... as RocBox, roc_host)` form must never appear; its
    // presence means a known non-refcounted boxed payload is being freed with
    // pointer alignment instead of the payload's own alignment.
    if (std.mem.find(u8, generated, "decref_box(payload") != null) {
        return customFailure(allocator, timer, "generated Rust file uses pointer-aligned decref_box(payload...) for a known boxed payload", .{});
    }
    // The box header size must come from an explicit `payload_contains_refcounted`
    // flag, NOT inferred from whether a teardown callback exists. Conflating the
    // two (`payload_decref.is_some()`) frees a `Box(U64)` host handle (non-
    // refcounted payload + teardown) from the wrong allocation base. Guard against
    // the inference creeping back into the emitted helper.
    if (std.mem.find(u8, generated, "payload_decref.is_some()") != null) {
        return customFailure(allocator, timer, "generated Rust glue infers box header size from payload_decref.is_some() instead of an explicit payload_contains_refcounted flag", .{});
    }
    if (std.mem.find(u8, generated, "free_box_allocation(data, payload_alignment, payload_contains_refcounted, roc_host)") == null) {
        return customFailure(allocator, timer, "generated Rust glue decref_box_with does not thread payload_contains_refcounted into free_box_allocation", .{});
    }

    const test_rlib_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rlib" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Rust rlib path: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "rustc",
        "--edition=2021",
        "-D",
        "warnings",
        "--crate-type",
        "lib",
        generated_path,
        "-o",
        test_rlib_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueZigNativeWasmLayouts(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-layout-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/ZigGlue.roc", output_dir, "test/glue/zig-layout-platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Zig file: {}", .{err});
    for ([_][]const u8{
        "pub const ProbeLayoutProbe = if (@sizeOf(usize) == 4) extern struct",
        "payload: [44]u8 align(8)",
        "pub fn payload_wide",
        "pub fn payload_aligned",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig file missing layout ABI text {s}", .{needle});
        }
    }

    const test_zig_content = std.fmt.allocPrint(allocator,
        \\const abi = @import("{s}");
        \\
        \\comptime {{
        \\    if (@sizeOf(usize) == 8) {{
        \\        if (@offsetOf(abi.ProbeLayoutProbe, "tag") != 88) @compileError("native tag offset mismatch");
        \\        if (@sizeOf(abi.ProbeLayoutProbe) != 96) @compileError("native tag union size mismatch");
        \\        if (@alignOf(abi.ProbeLayoutProbe) != 8) @compileError("native tag union alignment mismatch");
        \\    }} else if (@sizeOf(usize) == 4) {{
        \\        if (@offsetOf(abi.ProbeLayoutProbe, "tag") != 44) @compileError("wasm tag offset mismatch");
        \\        if (@sizeOf(abi.ProbeLayoutProbe) != 48) @compileError("wasm tag union size mismatch");
        \\        if (@alignOf(abi.ProbeLayoutProbe) != 8) @compileError("wasm tag union alignment mismatch");
        \\    }} else {{
        \\        @compileError("unsupported pointer width");
        \\    }}
        \\}}
        \\
        \\export fn _roc_glue_layout_accessor_check(value: abi.ProbeLayoutProbe) void {{
        \\    switch (value.tag) {{
        \\        .Aligned => {{
        \\            const payload = value.payload_aligned();
        \\            _ = payload.marker;
        \\            _ = payload.token;
        \\        }},
        \\        .Wide => {{
        \\            const payload = value.payload_wide();
        \\            _ = payload.label;
        \\            _ = payload.a;
        \\            _ = payload.b;
        \\            _ = payload.c;
        \\            _ = payload.d;
        \\            _ = payload.e;
        \\            _ = payload.f;
        \\            _ = payload.g;
        \\            _ = payload.h;
        \\        }},
        \\        .Empty => {{}},
        \\    }}
        \\}}
    , .{"roc_platform_abi.zig"}) catch |err|
        return customInfraFailure(allocator, timer, "failed to render layout test Zig source: {}", .{err});
    const test_zig_path = std.fs.path.join(allocator, &.{ output_dir, "test_layout_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate layout test Zig path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = test_zig_path, .data = test_zig_content }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write layout test Zig file: {}", .{err});

    const native_o_path = std.fs.path.join(allocator, &.{ output_dir, "test_layout_abi_native.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate native layout object path: {}", .{err});
    const native_emit_flag = std.fmt.allocPrint(allocator, "-femit-bin={s}", .{native_o_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate native layout emit flag: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "build-obj",
        test_zig_path,
        native_emit_flag,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    const wasm_o_path = std.fs.path.join(allocator, &.{ output_dir, "test_layout_abi_wasm.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate wasm layout object path: {}", .{err});
    const wasm_emit_flag = std.fmt.allocPrint(allocator, "-femit-bin={s}", .{wasm_o_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate wasm layout emit flag: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "zig",
        "build-obj",
        "-target",
        "wasm32-freestanding-none",
        test_zig_path,
        wasm_emit_flag,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;

    return null;
}

fn customGlueRust(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runGlueCommandInEnv(io, allocator, env, timer, timeout_ms, "src/glue/src/RustGlue.roc", output_dir)) |failure| return failure;
    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rs" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Rust path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Rust file: {}", .{err});
    for ([_][]const u8{
        "pub struct RocStr",
        "pub struct RocHost",
        "pub type RocBox = *mut c_void;",
        "pub fn incref_box",
        "pub fn decref_box",
        "pub fn decref_box_with",
        "pub fn allocate_box",
        "pub fn decref_erased_callable",
        "pub fn decref_host_tree(value: HostTree, roc_host: &RocHost)",
        "extern \"C\" fn decref_box_payload_type",
        "pub fn roc_alloc(length: usize, alignment: usize) -> *mut c_void;",
        "pub struct BuilderPrintValueArgs",
        "pub fn roc_stdout_line(arg0: RocStr);",
        "pub fn roc_main();",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Rust file missing {s}", .{needle});
        }
    }
    for ([_][]const u8{
        "ret_ptr",
        "arg_ptr",
        "RocOps",
        "HostedFunctions",
        "PlatformHostedFns",
        "pub struct RocAlloc",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) != null) {
            return customFailure(allocator, timer, "generated Rust file still contains obsolete ABI text {s}", .{needle});
        }
    }

    const test_rlib_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rlib" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Rust rlib path: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{
        "rustc",
        "--edition=2021",
        "-D",
        "warnings",
        "--crate-type",
        "lib",
        generated_path,
        "-o",
        test_rlib_path,
    }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueZig(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runGlueCommandInEnv(io, allocator, env, timer, timeout_ms, "src/glue/src/ZigGlue.roc", output_dir)) |failure| return failure;
    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Zig file: {}", .{err});
    for ([_][]const u8{
        "pub const RocStr",
        "pub const RocHost",
        "pub const RocBox = ?*anyopaque;",
        "pub fn increfBox",
        "pub fn decrefBox",
        "pub fn decrefBoxWith",
        "pub fn allocateBox",
        "pub fn decrefErasedCallable",
        "pub fn decrefHostTree(value: HostTree, roc_host: *RocHost) void",
        "fn decrefBoxPayloadType",
        "pub extern fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque;",
        "pub const BuilderPrint_valueArgs = extern struct",
        "pub extern fn roc_stdout_line(arg0: RocStr) callconv(.c) void;",
        "pub extern fn roc_main() callconv(.c) void;",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig file missing {s}", .{needle});
        }
    }
    for ([_][]const u8{
        "ret_ptr",
        "arg_ptr",
        "RocOps",
        "HostedFunctions",
        "PlatformHostedFns",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) != null) {
            return customFailure(allocator, timer, "generated Zig file still contains obsolete ABI text {s}", .{needle});
        }
    }
    if (customGlueZigBoxHelperTest(io, allocator, env, timer, timeout_ms, output_dir, generated_path)) |failure| return failure;
    return null;
}

fn customGlueZigOpaqueBox(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-int-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/ZigGlue.roc", output_dir, "test/int/platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Zig file: {}", .{err});

    for ([_][]const u8{
        "pub const RocBox = ?*anyopaque;",
        "pub extern fn roc_init() callconv(.c) RocBox;",
        "pub extern fn roc_update(arg0: RocBox, arg1: i64) callconv(.c) RocBox;",
        "pub extern fn roc_render(arg0: RocBox)",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig file missing opaque-box ABI text {s}", .{needle});
        }
    }
    if (std.mem.find(u8, generated, "**anyopaque") != null) {
        return customFailure(allocator, timer, "generated Zig file still uses **anyopaque for opaque boxed app types", .{});
    }
    return null;
}

fn customGlueZigBoxPayloadAlignment(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    // Regression test for non-refcounted boxed payload teardown alignment.
    //
    // ZigGlue.roc previously emitted `decrefBox(@ptrCast(expr), roc_host)` for
    // boxed payloads that are known and contain no refcounted values (e.g.
    // `Box(I64)`). `decrefBox` hardcodes pointer alignment (`@alignOf(usize)`),
    // so on small-pointer targets like wasm32 an 8-aligned payload is freed from
    // `base + 4` instead of `base`. The fix emits
    // `decrefBoxWith(@ptrCast(expr), @alignOf(payload), null, roc_host)` so the
    // payload's real alignment is used to recover the allocation base.
    //
    // test/static-data-host exposes `BranchPair(Box(I64), Box(I64))`, which
    // exercises exactly this path.
    const output_dir = createWorkSubdir(io, allocator, env, "glue-box-align-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/ZigGlue.roc", output_dir, "test/static-data-host/platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Zig file: {}", .{err});

    for ([_][]const u8{
        "decrefBoxWith(@ptrCast(payload._0), @alignOf(i64), false, null, roc_host);",
        "decrefBoxWith(@ptrCast(payload._1), @alignOf(i64), false, null, roc_host);",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig file missing payload-aligned box decref {s}", .{needle});
        }
    }
    // No boxed payload in this platform is opaque, so the pointer-aligned
    // `decrefBox(@ptrCast(...))` form must never appear; its presence means a
    // known non-refcounted boxed payload is being freed with pointer alignment
    // instead of the payload's own alignment.
    if (std.mem.find(u8, generated, "decrefBox(@ptrCast(") != null) {
        return customFailure(allocator, timer, "generated Zig file uses pointer-aligned decrefBox(@ptrCast(...)) for a known boxed payload", .{});
    }
    // The box header size must come from an explicit `payload_contains_refcounted`
    // flag, NOT inferred from whether a teardown callback exists. Conflating the
    // two (`payload_decref != null`) frees a `Box(U64)` host handle (non-
    // refcounted payload + teardown) from the wrong allocation base. Guard against
    // the inference creeping back into the emitted helper.
    if (std.mem.find(u8, generated, "payload_decref != null") != null) {
        return customFailure(allocator, timer, "generated Zig glue infers box header size from `payload_decref != null` instead of an explicit payload_contains_refcounted flag", .{});
    }
    if (std.mem.find(u8, generated, "freeBoxAllocation(data, payload_alignment, payload_contains_refcounted, roc_host)") == null) {
        return customFailure(allocator, timer, "generated Zig glue decrefBoxWith does not thread payload_contains_refcounted into freeBoxAllocation", .{});
    }
    return null;
}

fn customGlueZigBoxHelperTest(
    io: std.Io,
    allocator: Allocator,
    env: *const CaseEnv,
    timer: *harness.Timer,
    timeout_ms: u64,
    output_dir: []const u8,
    generated_path: []const u8,
) ?TestResult {
    const test_source =
        \\const std = @import("std");
        \\const abi = @import("abi");
        \\
        \\const Env = struct {
        \\    callback_count: usize = 0,
        \\    callback_rc: isize = -1,
        \\    alloc_count: usize = 0,
        \\    alloc_length: usize = 0,
        \\    alloc_alignment: usize = 0,
        \\    dealloc_count: usize = 0,
        \\    dealloc_ptr: usize = 0,
        \\    dealloc_alignment: usize = 0,
        \\    backing: [256]u8 align(16) = undefined,
        \\};
        \\
        \\fn rocAlloc(host: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        \\    const env_ref: *Env = @ptrCast(@alignCast(host.env));
        \\    env_ref.alloc_count += 1;
        \\    env_ref.alloc_length = length;
        \\    env_ref.alloc_alignment = alignment;
        \\    if (length > env_ref.backing.len or alignment > 16) return null;
        \\    return @ptrCast(&env_ref.backing);
        \\}
        \\
        \\fn rocDealloc(host: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
        \\    const env_ref: *Env = @ptrCast(@alignCast(host.env));
        \\    env_ref.dealloc_count += 1;
        \\    env_ref.dealloc_ptr = @intFromPtr(ptr);
        \\    env_ref.dealloc_alignment = alignment;
        \\}
        \\
        \\fn rocRealloc(_: *abi.RocHost, _: *anyopaque, _: usize, _: usize) callconv(.c) ?*anyopaque {
        \\    unreachable;
        \\}
        \\
        \\fn rocDbg(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {}
        \\fn rocExpectFailed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {}
        \\fn rocCrashed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {}
        \\
        \\fn makeHost(env_ref: *Env) abi.RocHost {
        \\    return .{
        \\        .env = @ptrCast(env_ref),
        \\        .roc_alloc = &rocAlloc,
        \\        .roc_dealloc = &rocDealloc,
        \\        .roc_realloc = &rocRealloc,
        \\        .roc_dbg = &rocDbg,
        \\        .roc_expect_failed = &rocExpectFailed,
        \\        .roc_crashed = &rocCrashed,
        \\    };
        \\}
        \\
        \\fn dataPtr(comptime payload_contains_refcounted: bool, backing: *align(16) [64]u8) *anyopaque {
        \\    const header_bytes = if (payload_contains_refcounted) 2 * @sizeOf(usize) else @sizeOf(usize);
        \\    const base: [*]u8 = @ptrCast(backing);
        \\    return @ptrCast(base + header_bytes);
        \\}
        \\
        \\fn refcountPtr(data_ptr: *anyopaque) *isize {
        \\    return @ptrFromInt(@intFromPtr(data_ptr) - @sizeOf(isize));
        \\}
        \\
        \\fn payloadDrop(data_ptr: ?*anyopaque, host: *abi.RocHost) callconv(.c) void {
        \\    const env_ref: *Env = @ptrCast(@alignCast(host.env));
        \\    env_ref.callback_count += 1;
        \\    env_ref.callback_rc = refcountPtr(data_ptr orelse unreachable).*;
        \\}
        \\
        \\fn erasedCallableFn(_: *abi.RocHost, _: ?[*]u8, _: ?[*]const u8, _: ?[*]u8) callconv(.c) void {}
        \\
        \\fn erasedDrop(_: ?[*]u8, host: *abi.RocHost) callconv(.c) void {
        \\    const env_ref: *Env = @ptrCast(@alignCast(host.env));
        \\    env_ref.callback_count += 1;
        \\}
        \\
        \\test "decrefBoxWith runs payload callback after final atomic decrement" {
        \\    var env_value = Env{};
        \\    var host = makeHost(&env_value);
        \\    var backing: [64]u8 align(16) = undefined;
        \\    const ptr = dataPtr(true, &backing);
        \\
        \\    refcountPtr(ptr).* = 1;
        \\    abi.decrefBoxWith(ptr, @alignOf(usize), true, &payloadDrop, &host);
        \\
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.callback_count);
        \\    try std.testing.expectEqual(@as(isize, 0), env_value.callback_rc);
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.dealloc_count);
        \\    try std.testing.expectEqual(@intFromPtr(&backing), env_value.dealloc_ptr);
        \\    try std.testing.expectEqual(@as(usize, @alignOf(usize)), env_value.dealloc_alignment);
        \\}
        \\
        \\// Regression test for a `Box(U64)`-style host resource handle: the box's
        \\// payload is NOT Roc-refcounted (header = one pointer word) but it carries a
        \\// teardown callback to free the underlying resource. The box header size must
        \\// come from the explicit `payload_contains_refcounted = false` argument, not
        \\// from "is there a teardown callback?". A previous version inferred the header
        \\// size from `payload_decref != null`, so this exact shape (non-refcounted
        \\// payload + teardown) was freed from `base - @sizeOf(usize)` instead of `base`,
        \\// corrupting the host allocator. dataPtr(false, ...) lays out a one-word header,
        \\// so the freed pointer must equal &backing.
        \\test "decrefBoxWith frees non-refcounted payload+teardown at the allocation base" {
        \\    var env_value = Env{};
        \\    var host = makeHost(&env_value);
        \\    var backing: [64]u8 align(16) = undefined;
        \\    const ptr = dataPtr(false, &backing);
        \\
        \\    refcountPtr(ptr).* = 1;
        \\    abi.decrefBoxWith(ptr, @alignOf(usize), false, &payloadDrop, &host);
        \\
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.callback_count);
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.dealloc_count);
        \\    try std.testing.expectEqual(@intFromPtr(&backing), env_value.dealloc_ptr);
        \\}
        \\
        \\test "allocateBox uses Roc box header layout" {
        \\    var env_value = Env{};
        \\    var host = makeHost(&env_value);
        \\
        \\    const ptr = abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, &host);
        \\    const payload: *u64 = @ptrCast(@alignCast(ptr));
        \\    payload.* = 42;
        \\
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.alloc_count);
        \\    try std.testing.expectEqual(@as(usize, @sizeOf(usize) + @sizeOf(u64)), env_value.alloc_length);
        \\    try std.testing.expectEqual(@as(usize, @alignOf(usize)), env_value.alloc_alignment);
        \\    try std.testing.expectEqual(@as(isize, 1), refcountPtr(ptr).*);
        \\    try std.testing.expectEqual(@as(u64, 42), payload.*);
        \\
        \\    abi.decrefBox(ptr, &host);
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.dealloc_count);
        \\    try std.testing.expectEqual(@intFromPtr(&env_value.backing), env_value.dealloc_ptr);
        \\}
        \\
        \\test "erased callable incref defers capture drop until final decref" {
        \\    var env_value = Env{};
        \\    var host = makeHost(&env_value);
        \\
        \\    const callable = abi.rocErasedCallableAllocate(&host, &erasedCallableFn, &erasedDrop, @sizeOf(u64));
        \\    abi.increfErasedCallable(callable, 1);
        \\    abi.decrefErasedCallable(callable, &host);
        \\    try std.testing.expectEqual(@as(usize, 0), env_value.callback_count);
        \\
        \\    abi.decrefErasedCallable(callable, &host);
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.callback_count);
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.dealloc_count);
        \\}
        \\
        \\test "isUniqueBox returns false for static refcount" {
        \\    var env_value = Env{};
        \\    var host = makeHost(&env_value);
        \\    var backing: [64]u8 align(16) = undefined;
        \\    const ptr = dataPtr(false, &backing);
        \\
        \\    refcountPtr(ptr).* = 0;
        \\
        \\    try std.testing.expect(!abi.isUniqueBox(ptr));
        \\    abi.decrefBox(ptr, &host);
        \\    try std.testing.expectEqual(@as(usize, 0), env_value.dealloc_count);
        \\}
        \\
        \\test "DefaultAllocators realloc preserves data and frees old allocation" {
        \\    var env_value = abi.RocEnv{
        \\        .allocator = std.testing.allocator,
        \\        .roc_io = abi.RocIo.freestanding(),
        \\    };
        \\    var roc_host = abi.makeRocHost(&env_value);
        \\
        \\    const alloc_ptr = abi.DefaultAllocators.rocAlloc(&roc_host, 8, 4) orelse return error.OutOfMemory;
        \\
        \\    const old_bytes: [*]u8 = @ptrCast(alloc_ptr);
        \\    old_bytes[0] = 0xaa;
        \\    old_bytes[1] = 0xbb;
        \\    old_bytes[7] = 0xcc;
        \\
        \\    const realloc_ptr = abi.DefaultAllocators.rocRealloc(&roc_host, alloc_ptr, 16, 4) orelse return error.OutOfMemory;
        \\
        \\    const new_bytes: [*]u8 = @ptrCast(realloc_ptr);
        \\    try std.testing.expectEqual(@as(u8, 0xaa), new_bytes[0]);
        \\    try std.testing.expectEqual(@as(u8, 0xbb), new_bytes[1]);
        \\    try std.testing.expectEqual(@as(u8, 0xcc), new_bytes[7]);
        \\
        \\    abi.DefaultAllocators.rocDealloc(&roc_host, realloc_ptr, 4);
        \\}
    ;

    const test_path = std.fs.path.join(allocator, &.{ output_dir, "box_helper_test.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig helper test path: {}", .{err});
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = test_path, .data = test_source }) catch |err|
        return customInfraFailure(allocator, timer, "failed to write generated Zig helper test: {}", .{err});

    const root_module_arg = std.fmt.allocPrint(allocator, "-Mroot={s}", .{test_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate Zig helper test root module arg: {}", .{err});
    const abi_module_arg = std.fmt.allocPrint(allocator, "-Mabi={s}", .{generated_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig ABI module arg: {}", .{err});
    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{ "zig", "test", "--dep", "abi", root_module_arg, abi_module_arg }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueZigBangRecordFieldNames(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-bang-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "glue", "src/glue/src/ZigGlue.roc", output_dir, "test/postcheck/platform_required_init/platform/main.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;

    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.zig" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Zig path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Zig file: {}", .{err});

    for ([_][]const u8{
        "@\"init!\": *anyopaque",
        "@\"render!\": *anyopaque",
        "pub const HostSet_mouseArgs = extern struct",
        "pub extern fn roc_host_set_mouse(arg0: HostSet_mouseArgs) callconv(.c) void;",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig file missing {s}", .{needle});
        }
    }
    for ([_][]const u8{
        "pub extern fn roc_init_for_host(arg0:",
        "pub extern fn roc_render_for_host(arg0: RocBox",
    }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Zig file missing natural entrypoint declaration {s}", .{needle});
        }
    }
    for ([_][]const u8{ "arg0: **anyopaque", "ret_ptr:", "arg_ptr:" }) |needle| {
        if (std.mem.find(u8, generated, needle) != null) {
            return customFailure(allocator, timer, "generated Zig file contained obsolete entrypoint ABI text {s}", .{needle});
        }
    }
    for ([_][]const u8{ "    init!:", "    render!:" }) |needle| {
        if (std.mem.find(u8, generated, needle) != null) {
            return customFailure(allocator, timer, "generated Zig file contained unquoted bang field {s}", .{needle});
        }
    }

    const test_o_path = std.fs.path.join(allocator, &.{ output_dir, "bang_record_abi.o" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate bang-record object path: {}", .{err});
    const emit_flag = std.fmt.allocPrint(allocator, "-femit-bin={s}", .{test_o_path}) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate emit flag: {}", .{err});

    if (runRawAndCheck(io, allocator, env, timer, timeout_ms, &.{ "zig", "build-obj", generated_path, emit_flag }, project_root_path, .{ .args = &.{} })) |failure| return failure;
    return null;
}

fn customGlueCTests(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    if (runRocAndCheck(io, allocator, env, timer, timeout_ms, .{
        .args = &.{ "test", "--opt=interpreter", "src/glue/src/CGlue.roc" },
        .not_contains = &.{ .{ .stream = .stderr, .text = "PANIC" }, .{ .stream = .stderr, .text = "unreachable" } },
    })) |failure| return failure;
    return null;
}

/// Build argv used by the Windows ChildProcessPool to spawn worker copies of
/// this runner. Starts with `selfExePath`, then preserves every original arg
/// *except* `--worker N` / `--worker-backend NAME` (stripped to avoid
/// duplication when the harness appends `--worker <idx>` per spawn).
fn buildCliWorkerArgvTemplate(io: std.Io, arena: Allocator, process_args: std.process.Args) CliRunnerError![]const []const u8 {
    var self_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path_len = try std.process.executablePath(io, &self_path_buf);
    const self_path = try arena.dupe(u8, self_path_buf[0..self_path_len]);

    const raw = try process_args.toSlice(arena);
    const original_args: []const []const u8 = @ptrCast(raw);

    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    try argv.append(arena, self_path);

    var i: usize = 1;
    while (i < original_args.len) : (i += 1) {
        const arg = original_args[i];
        if (harness.workerTemplateArgConsumesValue(arg)) {
            i += 1;
            continue;
        }
        if (harness.workerTemplateDropsFlag(arg)) {
            continue;
        }
        try argv.append(arena, arg);
    }

    return try argv.toOwnedSlice(arena);
}

fn getTestName(spec: CliCase) []const u8 {
    return spec.name;
}

fn dupeOptional(gpa: Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| gpa.dupe(u8, slice) catch null else null;
}

fn stabilizeResult(gpa: Allocator, result: TestResult) TestResult {
    return .{
        .status = result.status,
        .phase = result.phase,
        .duration_ns = result.duration_ns,
        .build_ns = result.build_ns,
        .run_ns = result.run_ns,
        .exit_code = result.exit_code,
        .stderr_capture = dupeOptional(gpa, result.stderr_capture),
        .stdout_capture = dupeOptional(gpa, result.stdout_capture),
        .message = dupeOptional(gpa, result.message),
    };
}

// Process pool (via harness)

const Pool = harness.ProcessPool(CliCase, TestResult, .{
    .runTest = &runSingleTest,
    .serialize = &serializeResult,
    .deserialize = &deserializeResult,
    .default_result = .{ .status = .crash },
    .timeout_result = .{ .status = .timeout },
    .stabilizeResult = &stabilizeResult,
    .getName = &getTestName,
    .use_process_groups = true,
    .timeout_report_grace_ms = timeout_result_grace_ms,
    .windows_persistent_workers = false,
});

// Output

fn printResults(
    tests: []const CliCase,
    results: []const TestResult,
    verbose: bool,
    gpa: Allocator,
    wall_ns: u64,
    max_children: usize,
) void {
    const status_count = 7;
    const opt_count = 4;
    const all_opts = [_]OptMode{ .interpreter, .dev, .size, .speed };
    var status_counts = [_]usize{0} ** status_count;
    var opt_counts = [_]usize{0} ** opt_count;
    var opt_failures = [_]usize{0} ** opt_count;
    var suite_counts = [_]usize{0} ** suite_count;
    var suite_failures = [_]usize{0} ** suite_count;
    var suite_skips = [_]usize{0} ** suite_count;

    for (tests, 0..) |tc, i| {
        const r = results[i];
        const ms = harness.nsToMs(r.duration_ns);
        status_counts[@intFromEnum(r.status)] += 1;
        suite_counts[@intFromEnum(tc.suite)] += 1;
        if (r.status != .pass and r.status != .skip) {
            suite_failures[@intFromEnum(tc.suite)] += 1;
        }
        if (r.status == .skip) {
            suite_skips[@intFromEnum(tc.suite)] += 1;
        }
        if (tc.backend) |backend| {
            opt_counts[@intFromEnum(backend)] += 1;
            if (r.status != .pass and r.status != .skip) {
                opt_failures[@intFromEnum(backend)] += 1;
            }
        }

        switch (r.status) {
            .pass => {
                if (verbose) std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
            },
            .build_failed, .run_failed, .timeout, .crash, .infra_error => printProblemResult(tc, r, ms),
            .skip => {
                if (verbose) std.debug.print("  SKIP  {s}\n", .{tc.name});
            },
        }
    }

    const wall_ms = harness.nsToMs(wall_ns);
    std.debug.print("\n{d} passed", .{status_counts[@intFromEnum(TestStatus.pass)]});
    if (status_counts[@intFromEnum(TestStatus.build_failed)] > 0) std.debug.print(", {d} build failed", .{status_counts[@intFromEnum(TestStatus.build_failed)]});
    if (status_counts[@intFromEnum(TestStatus.run_failed)] > 0) std.debug.print(", {d} run failed", .{status_counts[@intFromEnum(TestStatus.run_failed)]});
    if (status_counts[@intFromEnum(TestStatus.crash)] > 0) std.debug.print(", {d} crashed", .{status_counts[@intFromEnum(TestStatus.crash)]});
    if (status_counts[@intFromEnum(TestStatus.timeout)] > 0) std.debug.print(", {d} timed out", .{status_counts[@intFromEnum(TestStatus.timeout)]});
    if (status_counts[@intFromEnum(TestStatus.infra_error)] > 0) std.debug.print(", {d} infra errors", .{status_counts[@intFromEnum(TestStatus.infra_error)]});
    if (status_counts[@intFromEnum(TestStatus.skip)] > 0) std.debug.print(", {d} skipped", .{status_counts[@intFromEnum(TestStatus.skip)]});
    std.debug.print(" ({d} total) in {d:.0}ms using {d} worker(s)\n", .{ tests.len, wall_ms, max_children });

    std.debug.print("\n=== Suite Summary ===\n", .{});
    for (all_suites) |suite| {
        const suite_idx = @intFromEnum(suite);
        if (suite_counts[suite_idx] == 0) continue;
        std.debug.print("  {s:<12} {d:>4} run, {d:>4} failed, {d:>4} skipped\n", .{
            suite.displayName(),
            suite_counts[suite_idx],
            suite_failures[suite_idx],
            suite_skips[suite_idx],
        });
    }

    std.debug.print("\n=== Backend Matrix ===\n", .{});
    for (all_opts) |opt| {
        const opt_idx = @intFromEnum(opt);
        if (opt_counts[opt_idx] == 0) continue;
        std.debug.print("  {s:<11} {d:>4} run, {d:>4} failed\n", .{ opt.cliName(), opt_counts[opt_idx], opt_failures[opt_idx] });
    }

    // Timing summary
    var durations: std.ArrayListUnmanaged(u64) = .empty;
    var build_durations: std.ArrayListUnmanaged(u64) = .empty;
    var run_durations: std.ArrayListUnmanaged(u64) = .empty;
    var opt_durations = [_]std.ArrayListUnmanaged(u64){ .empty, .empty, .empty, .empty };
    var suite_durations = [_]std.ArrayListUnmanaged(u64){ .empty, .empty, .empty, .empty };
    defer durations.deinit(gpa);
    defer build_durations.deinit(gpa);
    defer run_durations.deinit(gpa);
    defer {
        for (&opt_durations) |*list| list.deinit(gpa);
    }
    defer {
        for (&suite_durations) |*list| list.deinit(gpa);
    }
    for (results) |r| {
        if (r.duration_ns > 0) durations.append(gpa, r.duration_ns) catch continue;
        if (r.build_ns > 0) build_durations.append(gpa, r.build_ns) catch {};
        if (r.run_ns > 0) run_durations.append(gpa, r.run_ns) catch {};
    }
    for (tests, results) |tc, r| {
        if (r.duration_ns > 0) {
            suite_durations[@intFromEnum(tc.suite)].append(gpa, r.duration_ns) catch {};
            if (tc.backend) |backend| {
                opt_durations[@intFromEnum(backend)].append(gpa, r.duration_ns) catch {};
            }
        }
    }
    if (harness.computeTimingStats(durations.items)) |_| {
        std.debug.print("\n=== Timing Summary (ms) ===\n", .{});
        harness.printStatsHeader();
        harness.printStatsRow("total", harness.computeTimingStats(durations.items));
        harness.printStatsRow("build", harness.computeTimingStats(build_durations.items));
        harness.printStatsRow("run", harness.computeTimingStats(run_durations.items));
        for (all_suites) |suite| {
            harness.printStatsRow(suite.cliName(), harness.computeTimingStats(suite_durations[@intFromEnum(suite)].items));
        }
        for (all_opts) |opt| {
            harness.printStatsRow(opt.cliName(), harness.computeTimingStats(opt_durations[@intFromEnum(opt)].items));
        }
    }

    var duration_arr = gpa.alloc(u64, results.len) catch return;
    defer gpa.free(duration_arr);
    for (results, 0..) |r, i| duration_arr[i] = r.duration_ns;
    harness.printSlowestN(CliCase, tests, duration_arr, 5, gpa, getTestName);
}

fn printProblemResult(tc: CliCase, r: TestResult, ms: f64) void {
    std.debug.print("  {s:<12} {s}  ({d:.1}ms, phase={s})\n", .{ statusLabel(r.status), tc.name, ms, phaseLabel(r.phase) });
    if (r.message) |msg| std.debug.print("        {s}\n", .{msg});
    if (r.exit_code != 0) {
        if (r.exit_code & 0x80000000 != 0) {
            std.debug.print("        signal {d}\n", .{r.exit_code & 0x7FFFFFFF});
        } else {
            std.debug.print("        exit code {d}\n", .{r.exit_code});
        }
    }
    printCapturedOutput("stderr", r.stderr_capture);
    printCapturedOutput("stdout", r.stdout_capture);
    printRepro(tc);
}

fn printCapturedOutput(label: []const u8, capture: ?[]const u8) void {
    const data = capture orelse return;
    if (data.len == 0) return;
    var lines = std.mem.splitScalar(u8, data, '\n');
    var count: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (count == 0) {
            std.debug.print("        {s}: {s}\n", .{ label, line });
        } else if (count < 5) {
            std.debug.print("        {s}\n", .{line});
        } else {
            std.debug.print("        ... ({s} truncated)\n", .{label});
            break;
        }
        count += 1;
    }
}

fn printRepro(tc: CliCase) void {
    std.debug.print("        Repro: zig build run-test-cli -- --suite {s} --filter \"{s}\"\n\n", .{ tc.suite.cliName(), tc.name });
}

fn statsStatus(status: TestStatus) []const u8 {
    return switch (status) {
        .pass => "pass",
        .build_failed, .run_failed, .infra_error => "fail",
        .timeout => "timeout",
        .crash => "crash",
        .skip => "skip",
    };
}

fn statsSummary(results: []const TestResult) harness.StatsSummary {
    var summary: harness.StatsSummary = .{ .total = results.len };
    for (results) |result| {
        switch (result.status) {
            .pass => summary.passed += 1,
            .build_failed, .run_failed, .infra_error => summary.failed += 1,
            .timeout => summary.timed_out += 1,
            .crash => summary.crashed += 1,
            .skip => summary.skipped += 1,
        }
    }
    return summary;
}

fn caseStatsData(
    gpa: Allocator,
    tc: CliCase,
    result: TestResult,
) []const harness.StatsData {
    var count: usize = 2;
    if (result.message != null) count += 1;
    if (result.stderr_capture != null) count += 1;
    if (result.stdout_capture != null) count += 1;
    if (result.exit_code != 0) count += 1;

    const data = gpa.alloc(harness.StatsData, count) catch return &.{};
    var next: usize = 0;
    data[next] = .{ .key = "suite", .value = tc.suite.cliName() };
    next += 1;
    data[next] = .{ .key = "backend", .value = if (tc.backend) |backend| backend.cliName() else "none" };
    next += 1;
    if (result.message) |message| {
        data[next] = .{ .key = "message", .value = message };
        next += 1;
    }
    if (result.stderr_capture) |stderr| {
        data[next] = .{ .key = "stderr", .value = stderr };
        next += 1;
    }
    if (result.stdout_capture) |stdout| {
        data[next] = .{ .key = "stdout", .value = stdout };
        next += 1;
    }
    if (result.exit_code != 0) {
        const exit_text = std.fmt.allocPrint(gpa, "{d}", .{result.exit_code}) catch "unknown";
        data[next] = .{ .key = "exit_code", .value = exit_text };
    }
    return data;
}

fn appendStatsEvent(
    gpa: Allocator,
    events: *std.ArrayListUnmanaged(harness.StatsEvent),
    id: []const u8,
    parent_id: ?[]const u8,
    kind: []const u8,
    name: []const u8,
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    data: []const harness.StatsData,
) void {
    events.append(gpa, .{
        .id = id,
        .parent_id = parent_id,
        .kind = kind,
        .name = name,
        .status = status,
        .start_ns = start_ns,
        .end_ns = end_ns,
        .data = data,
    }) catch {};
}

fn appendCaseStatsEvent(
    gpa: Allocator,
    events: *std.ArrayListUnmanaged(harness.StatsEvent),
    id: []const u8,
    name: []const u8,
    status: []const u8,
    duration_ns: u64,
    maybe_span: ?harness.PoolSpan,
    data: []const harness.StatsData,
) void {
    const start_ns = if (maybe_span) |span| span.start_ns else 0;
    const end_ns = if (maybe_span) |span| span.end_ns else duration_ns;
    const worker_index = if (maybe_span) |span| span.worker_index else null;
    events.append(gpa, .{
        .id = id,
        .parent_id = null,
        .kind = "case",
        .name = name,
        .status = status,
        .start_ns = start_ns,
        .end_ns = end_ns,
        .worker_index = worker_index,
        .data = data,
    }) catch {};
}

fn writeStatsJson(
    gpa: Allocator,
    io: std.Io,
    path: []const u8,
    tests: []const CliCase,
    results: []const TestResult,
    spans: []const ?harness.PoolSpan,
) CliRunnerError!void {
    var stats_arena = std.heap.ArenaAllocator.init(gpa);
    defer stats_arena.deinit();
    const stats_allocator = stats_arena.allocator();

    var events: std.ArrayListUnmanaged(harness.StatsEvent) = .empty;

    for (tests, results, 0..) |tc, result, i| {
        const case_id = try std.fmt.allocPrint(stats_allocator, "case-{d}", .{i});
        const status = statsStatus(result.status);
        const total_ns = result.duration_ns;
        const build_ns = result.build_ns;
        const run_ns = result.run_ns;
        const setup_ns = total_ns -| (build_ns +| run_ns);
        const maybe_span = if (i < spans.len) spans[i] else null;

        appendCaseStatsEvent(stats_allocator, &events, case_id, tc.name, status, total_ns, maybe_span, caseStatsData(stats_allocator, tc, result));

        if (setup_ns > 0) {
            const id = try std.fmt.allocPrint(stats_allocator, "case-{d}-setup", .{i});
            appendStatsEvent(stats_allocator, &events, id, case_id, "setup", "setup", "pass", 0, setup_ns, &.{});
        }

        if (build_ns > 0) {
            const id = try std.fmt.allocPrint(stats_allocator, "case-{d}-build", .{i});
            const build_status = if (result.phase == .build) status else "pass";
            appendStatsEvent(stats_allocator, &events, id, case_id, "roc build", "roc build", build_status, setup_ns, setup_ns + build_ns, &.{});
        }

        if (run_ns > 0) {
            const id = try std.fmt.allocPrint(stats_allocator, "case-{d}-run", .{i});
            const run_status = if (result.phase == .run) status else "pass";
            appendStatsEvent(stats_allocator, &events, id, case_id, "run", "run", run_status, setup_ns + build_ns, setup_ns + build_ns + run_ns, &.{});
        }
    }

    try harness.writeRunnerStatsJson(stats_allocator, io, path, .{
        .runner = "cli",
        .summary = statsSummary(results),
        .events = events.items,
    });
}

// Main

fn printUsage() void {
    std.debug.print(
        \\Usage: parallel_cli_runner <roc_binary> [options]
        \\
        \\Options:
        \\  --suite <name>      Run suite: platforms, subcommands, echo, glue, or all (repeatable)
        \\  --filter <pattern>   Run tests matching pattern (repeatable)
        \\  --threads <N>        Max concurrent workers (default: CPU count)
        \\  --timeout <ms>       Per-test timeout in ms (default: 120000, 240000 with glue)
        \\  --include-llvm       Include size and speed LLVM backend jobs
        \\  --glue-roc <path>    Roc binary to use for glue generation (default: <roc_binary>)
        \\  --glue-opt <opt>     Glue execution mode; supported value: interpreter
        \\  --glue-full-targets  Run opt-in non-default glue compile targets
        \\  --verbose            Show PASS results with timing
        \\
    , .{});
}

const ParsedRunnerArgs = struct {
    standard: harness.StandardArgs,
    suites: SuiteSelection,
    glue_options: GlueRunnerOptions,
    glue_roc: ?[]const u8 = null,
};

fn parseSuiteName(value: []const u8) ?Suite {
    for (all_suites) |suite| {
        if (std.mem.eql(u8, value, suite.cliName())) return suite;
    }
    return null;
}

fn parseGlueExecutionMode(value: []const u8) ?GlueExecutionMode {
    if (std.mem.eql(u8, value, "interpreter")) return .interpreter;
    return null;
}

fn parseRunnerArgs(allocator: Allocator, process_args: std.process.Args) CliRunnerError!ParsedRunnerArgs {
    const raw_z = try process_args.toSlice(allocator);
    const raw_args: []const []const u8 = @ptrCast(raw_z);

    var standard_args: std.ArrayListUnmanaged([]const u8) = .empty;
    try standard_args.append(allocator, raw_args[0]);

    var suites = SuiteSelection{};
    var glue_options = GlueRunnerOptions{};
    var glue_roc: ?[]const u8 = null;
    var saw_suite = false;
    var i: usize = 1;
    while (i < raw_args.len) : (i += 1) {
        const arg = raw_args[i];
        if (std.mem.eql(u8, arg, "--suite")) {
            saw_suite = true;
            i += 1;
            if (i >= raw_args.len) {
                std.debug.print("missing value for --suite\n", .{});
                return error.InvalidArgs;
            }
            const value = raw_args[i];
            if (std.mem.eql(u8, value, "all")) {
                suites.addAll();
            } else if (parseSuiteName(value)) |suite| {
                suites.add(suite);
            } else {
                std.debug.print("unknown suite: {s}\n", .{value});
                return error.InvalidArgs;
            }
            continue;
        }
        if (std.mem.eql(u8, arg, "--glue-roc")) {
            i += 1;
            if (i >= raw_args.len) {
                std.debug.print("missing value for --glue-roc\n", .{});
                return error.InvalidArgs;
            }
            glue_roc = raw_args[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--glue-roc=")) {
            glue_roc = arg["--glue-roc=".len..];
            continue;
        }
        if (std.mem.eql(u8, arg, "--glue-opt")) {
            i += 1;
            if (i >= raw_args.len) {
                std.debug.print("missing value for --glue-opt\n", .{});
                return error.InvalidArgs;
            }
            const value = raw_args[i];
            glue_options.execution_mode = parseGlueExecutionMode(value) orelse {
                std.debug.print("unknown glue opt: {s}\n", .{value});
                return error.InvalidArgs;
            };
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--glue-opt=")) {
            const value = arg["--glue-opt=".len..];
            glue_options.execution_mode = parseGlueExecutionMode(value) orelse {
                std.debug.print("unknown glue opt: {s}\n", .{value});
                return error.InvalidArgs;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--glue-full-targets")) {
            glue_options.full_targets = true;
            continue;
        }
        try standard_args.append(allocator, arg);
    }

    if (!saw_suite or suites.isEmpty()) {
        suites.addAll();
    }

    return .{
        .standard = try harness.parseStandardArgsFromSlice(try standard_args.toOwnedSlice(allocator), allocator),
        .suites = suites,
        .glue_options = glue_options,
        .glue_roc = glue_roc,
    };
}

fn effectiveTimeoutMs(args: harness.StandardArgs, suites: SuiteSelection) u64 {
    if (args.timeout_provided) return args.timeout_ms;
    if (suites.includes(.glue)) return glue_timeout_ms;
    return default_timeout_ms;
}

test "effectiveTimeoutMs extends default for glue suite only" {
    var default_args = harness.StandardArgs{};

    var suites = SuiteSelection{};
    suites.add(.platforms);
    try std.testing.expectEqual(default_timeout_ms, effectiveTimeoutMs(default_args, suites));

    suites.add(.glue);
    try std.testing.expectEqual(glue_timeout_ms, effectiveTimeoutMs(default_args, suites));

    default_args.timeout_provided = true;
    default_args.timeout_ms = 15_000;
    try std.testing.expectEqual(@as(u64, 15_000), effectiveTimeoutMs(default_args, suites));
}

/// Entry point for the parallel CLI test runner.
pub fn main(init: std.process.Init) CliRunnerError!void {
    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var spec_arena = collections.SingleThreadArena.init(gpa);
    defer spec_arena.deinit();

    const parsed = parseRunnerArgs(spec_arena.allocator(), init.minimal.args) catch |err| {
        printUsage();
        return err;
    };
    const args = parsed.standard;

    if (args.help_requested) {
        printUsage();
        return;
    }

    if (args.positional.len < 1) {
        printUsage();
        std.process.exit(1);
    }

    project_root_path = try std.Io.Dir.cwd().realPathFileAlloc(init.io, ".", spec_arena.allocator());
    roc_binary_path = if (std.fs.path.isAbsolute(args.positional[0]))
        args.positional[0]
    else
        try std.fs.path.join(spec_arena.allocator(), &.{ project_root_path, args.positional[0] });
    glue_roc_binary_path = if (parsed.glue_roc) |path|
        if (std.fs.path.isAbsolute(path))
            path
        else
            try std.fs.path.join(spec_arena.allocator(), &.{ project_root_path, path })
    else
        roc_binary_path;
    glue_execution_mode = parsed.glue_options.execution_mode;

    const tests = try buildCases(spec_arena.allocator(), args.filters, args.include_llvm, parsed.suites, parsed.glue_options);
    if (tests.len == 0) return;
    const timeout_ms = effectiveTimeoutMs(args, parsed.suites);

    // Worker mode: parent spawned us with `--worker <idx>` to run a single
    // test and serialize the result to stdout. Used on Windows where the
    // harness runs N worker processes in parallel instead of forking.
    if (args.worker_index) |idx| {
        if (idx >= tests.len) std.process.exit(2);
        var arena = collections.SingleThreadArena.init(std.heap.smp_allocator);
        defer arena.deinit();
        const result = runSingleTest(init.io, arena.allocator(), tests[idx], timeout_ms);
        serializeResult(std.Io.File.stdout().handle, result);
        return;
    }

    // Persistent worker mode: read test indices from stdin (one decimal per
    // line), run each, write a u32-length-prefixed result to stdout, loop
    // until stdin EOFs. Amortizes the per-Child process-boot cost across
    // many tests on the same worker. Without this branch, a worker spawned
    // with `--worker-stream` would fall through to the parent path below
    // and reentrantly spawn its own pool of workers — fork-bombing the box.
    if (args.worker_stream) {
        var arena = collections.SingleThreadArena.init(std.heap.smp_allocator);
        defer arena.deinit();

        const stdin_handle = std.Io.File.stdin().handle;
        const stdout_handle = std.Io.File.stdout().handle;

        var line_buf: [32]u8 = undefined;
        outer: while (true) {
            var line_len: usize = 0;
            while (true) {
                if (line_len >= line_buf.len) break :outer;
                const n = harness.posixRead(stdin_handle, line_buf[line_len .. line_len + 1]) catch break :outer;
                if (n == 0) break :outer;
                if (line_buf[line_len] == '\n') break;
                line_len += 1;
            }
            const idx = std.fmt.parseInt(usize, line_buf[0..line_len], 10) catch continue;
            if (idx >= tests.len) continue;

            _ = arena.reset(.retain_capacity);
            const result = runSingleTest(init.io, arena.allocator(), tests[idx], timeout_ms);
            serializeResultStreamed(stdout_handle, result);
        }
        return;
    }

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const max_children = args.max_threads orelse @min(cpu_count, tests.len);

    std.debug.print("=== CLI Test Runner ===\n", .{});
    std.debug.print("{d} tests, {d} workers, {d}s timeout", .{ tests.len, max_children, timeout_ms / 1000 });
    if (args.include_llvm) {
        std.debug.print(", backends: interpreter, dev, size, speed\n\n", .{});
    } else {
        std.debug.print(", backends: interpreter, dev\n\n", .{});
    }
    if (parsed.suites.includes(.glue)) {
        std.debug.print("Glue generator: {s}, glue-opt={s}, full-targets={}\n\n", .{
            glue_roc_binary_path,
            parsed.glue_options.execution_mode.cliName(),
            parsed.glue_options.full_targets,
        });
    }

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash });
    const spans = try gpa.alloc(?harness.PoolSpan, tests.len);
    defer gpa.free(spans);
    @memset(spans, null);

    // Build a worker_argv_template so Windows can re-invoke this binary as a
    // single-test Child worker. On POSIX it's unused (fork path doesn't
    // re-exec). Always pass the positional `roc_binary` path through so the
    // child uses the same binary.
    const worker_argv_template = try buildCliWorkerArgvTemplate(init.io, spec_arena.allocator(), init.minimal.args);

    var wall_timer = harness.Timer.start() catch @panic("no clock");
    Pool.runWithSpans(init.io, tests, results, spans, max_children, timeout_ms, gpa, worker_argv_template);
    const wall_ns = wall_timer.read();

    printResults(tests, results, args.verbose, gpa, wall_ns, max_children);

    if (args.stats_json_path) |path| {
        try writeStatsJson(gpa, init.io, path, tests, results, spans);
    }

    for (results) |r| {
        if (r.stderr_capture) |s| gpa.free(s);
        if (r.stdout_capture) |s| gpa.free(s);
        if (r.message) |m| gpa.free(m);
    }

    for (results) |r| {
        switch (r.status) {
            .build_failed, .run_failed, .crash, .timeout, .infra_error => std.process.exit(1),
            else => {},
        }
    }
}

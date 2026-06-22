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

const Stream = enum {
    stdout,
    stderr,
};

const OutputNeedle = struct {
    stream: Stream,
    text: []const u8,
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
    contains_any: []const OutputNeedleSet = &.{},
};

const PlatformCase = struct {
    /// Path to .roc file (relative to project root)
    roc_file: []const u8,
    /// Platform name (for display grouping)
    platform: []const u8,
    /// What kind of test to run
    test_kind: TestKind,

    const TestKind = union(enum) {
        /// Build natively and run; check exit code 0
        native_run,
        /// Build natively, run with --test <spec>; check exit code 0
        io_spec: []const u8,
    };
};

const CustomCase = enum {
    noop,
    cli_cache_roots_distinct,
    generated_graph_1_1,
    generated_graph_5_5,
    generated_graph_2_100,
    generated_graph_200_5,
    list_builtin_inlined,
    default_platform_linux_disassembly,
    default_platform_build_x64glibc,
    default_platform_build_arm64glibc,
    default_platform_build_wasm32,
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
    glue_c_header,
    glue_c_header_compiles,
    glue_zig,
    glue_zig_compiles,
    glue_zig_opaque_box,
    glue_rust,
    glue_zig_bang_record_fields,
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
    };
};

// Spec generation

fn buildCases(allocator: Allocator, filters: []const []const u8, include_llvm: bool, suites: SuiteSelection) anyerror![]const CliCase {
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
    }
    if (suites.includes(.subcommands)) {
        try appendStaticCases(allocator, &cases, &subcommand_cases, filters);
    }

    return try cases.toOwnedSlice(allocator);
}

fn appendStaticCases(
    allocator: Allocator,
    cases: *std.ArrayListUnmanaged(CliCase),
    source: []const CliCase,
    filters: []const []const u8,
) anyerror!void {
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
) anyerror!void {
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

fn fmtTestName(allocator: Allocator, roc_file: []const u8, opt: OptMode) anyerror![]const u8 {
    return std.fmt.allocPrint(allocator, "{s} [{s}]", .{ roc_file, opt.cliName() });
}

fn caseRocFile(case: CliCase) ?[]const u8 {
    return switch (case.body) {
        .platform => |platform| platform.roc_file,
        .command => |command| command.roc_file,
        .custom => null,
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
    .{ .id = 0, .suite = .echo, .name = "echo platform: all_syntax_test.roc prints expected output (dev backend)", .backend = .dev, .skip = .{ .always = "TODO: dev backend default platform build does not preserve the original source directory" }, .body = .{ .command = .{ .args = &.{"--opt=dev"}, .roc_file = "test/echo/all_syntax_test.roc", .stdout_exact = all_syntax_expected_stdout, .stderr_exact = all_syntax_expected_stderr } } },
    .{ .id = 0, .suite = .echo, .name = "echo platform: roc test all_syntax_test.roc passes", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/echo/all_syntax_test.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
};

// Glue suite cases

const glue_cases = [_]CliCase{
    .{ .id = 0, .suite = .glue, .name = "glue command with DebugGlue succeeds", .body = .{ .custom = .glue_debug } },
    .{ .id = 0, .suite = .glue, .name = "glue command with CGlue generates expected C header", .body = .{ .custom = .glue_c_header } },
    .{ .id = 0, .suite = .glue, .name = "glue command generated C header compiles with zig cc", .body = .{ .custom = .glue_c_header_compiles } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue succeeds on fx platform", .body = .{ .custom = .glue_zig } },
    .{ .id = 0, .suite = .glue, .name = "glue command generated Zig compiles with zig build-obj", .body = .{ .custom = .glue_zig_compiles } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue uses RocBox for opaque boxed app types", .body = .{ .custom = .glue_zig_opaque_box } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: RustGlue succeeds on fx platform", .body = .{ .custom = .glue_rust } },
    .{ .id = 0, .suite = .glue, .name = "glue regression: ZigGlue quotes bang record fields", .body = .{ .custom = .glue_zig_bang_record_fields } },
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

const plus_type_error_needles = [_]OutputNeedle{
    .{ .stream = .stderr, .text = "MISSING METHOD" },
    .{ .stream = .stderr, .text = "TYPE MISMATCH" },
    .{ .stream = .stderr, .text = "error" },
    .{ .stream = .stderr, .text = "Found" },
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
    .{ .id = 0, .suite = .subcommands, .name = "roc build reports missing host symbols before linking", .body = .{ .command = .{ .args = &.{ "build", "--no-cache", "--target=x64musl" }, .roc_file = "test/missing-host-symbol/app.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "MISSING HOST SYMBOLS" }, .{ .stream = .stderr, .text = "roc_host_vanish" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check writes parse errors to stderr", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_parse_error.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &parse_error_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check displays correct file path in parse error messages", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_parse_error.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{.{ .stream = .stderr, .text = "has_parse_error.roc" }}, .not_contains = &.{.{ .stream = .stderr, .text = "\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check rejects invalid hosted sections", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/hosted-section-errors/platform/main.roc", .exit = .failure, .stderr_min_len = 1, .contains = &.{ .{ .stream = .stderr, .text = "INVALID HOSTED SECTION" }, .{ .stream = .stderr, .text = "Host.nonexistent" }, .{ .stream = .stderr, .text = "Host.quadruple" }, .{ .stream = .stderr, .text = "roc_alloc" }, .{ .stream = .stderr, .text = "roc__sneaky" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check accepts a valid hosted section", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/fx/platform/main.roc", .not_contains = &.{.{ .stream = .stderr, .text = "INVALID HOSTED SECTION" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds on valid file", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/simple_success.roc", .not_contains = &.{ .{ .stream = .stderr, .text = "Failed to check" }, .{ .stream = .stderr, .text = "error" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build --opt=speed emits no invalid LLVM debug info", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/simple_success.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &invalid_llvm_debug_info_needles } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build --opt=speed --debug emits valid LLVM debug info", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--debug", "--no-cache" }, .roc_file = "test/cli/simple_success.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &invalid_llvm_debug_info_needles } } },
    // repro for https://github.com/roc-lang/roc/issues/9690: a self-recursive
    // closure that captures an enclosing value must compile through the LLVM
    // size/speed backend. The crash guard inside the program makes a wrong
    // result fail too, so a clean exit means it both built and computed 25.
    .{ .id = 0, .suite = .subcommands, .name = "issue 9690: recursive capturing closure builds and runs on LLVM size backend", .backend = .size, .body = .{ .command = .{ .args = &.{ "--opt=size", "--no-cache" }, .roc_file = "test/cli/Issue9690RecursiveCaptureClosure.roc", .exit = .success } } },
    .{ .id = 0, .suite = .subcommands, .name = "issue 9717: spec-constr record cloning reaches target validation on LLVM speed backend", .backend = .speed, .body = .{ .command = .{ .args = &.{ "build", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/Issue9717SpecConstrSpanInvalidation.roc", .exit = .failure, .contains = &.{.{ .stream = .stderr, .text = "MISSING TARGET FILE" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "SIGSEGV" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "rocci-bird: full wasm4 app builds on LLVM size backend", .backend = .size, .body = .{ .command = .{ .args = &.{ "build", "--opt=size", "--no-cache" }, .roc_file = "test/cli/rocci_bird_postcheck_panic/main.roc", .contains = &.{.{ .stream = .stdout, .text = "successfully building" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "MISSING TARGET FILE" }, .{ .stream = .stderr, .text = "postcheck invariant violated" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph succeeds with 1 file and 1 symbol", .body = .{ .custom = .generated_graph_1_1 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph succeeds with 5 files and 5 symbols", .body = .{ .custom = .generated_graph_5_5 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph handles many symbols per file", .body = .{ .custom = .generated_graph_2_100 } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check generated module graph handles many imported files", .body = .{ .custom = .generated_graph_200_5 } },
    .{ .id = 0, .suite = .subcommands, .name = "list builtins inline in native --opt=speed build", .body = .{ .custom = .list_builtin_inlined } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform x64musl matches direct write assembly", .skip = .{ .always = "TODO: direct-write default-platform codegen" }, .body = .{ .custom = .default_platform_linux_disassembly } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform x64glibc succeeds", .body = .{ .custom = .default_platform_build_x64glibc } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform arm64glibc succeeds", .body = .{ .custom = .default_platform_build_arm64glibc } },
    .{ .id = 0, .suite = .subcommands, .name = "roc build default platform wasm32 archive succeeds", .body = .{ .custom = .default_platform_build_wasm32 } },
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
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt reformats file in place", .body = .{ .custom = .fmt_reformats_file } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt does not change well-formatted file", .body = .{ .custom = .fmt_does_not_change_file } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --stdin formats unformatted input", .body = .{ .custom = .fmt_stdin_formats } },
    .{ .id = 0, .suite = .subcommands, .name = "roc fmt --stdin does not change well-formatted input", .body = .{ .custom = .fmt_stdin_does_not_change } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports type error - annotation mismatch", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_type_error_annotation.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &type_error_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports type error - plus operator with incompatible types", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_type_error_plus_operator.roc", .exit = .failure, .stderr_min_len = 1, .contains_any = &.{.{ .needles = &plus_type_error_needles }} } } },
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
    .{ .id = 0, .suite = .subcommands, .name = "roc test failure output contains doc comment (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter" }, .roc_file = "test/cli/FailWithDocComment.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "## This test should fail" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" }, .{ .stream = .stderr, .text = "\u{2502}" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test failure output contains doc comment (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev" }, .roc_file = "test/cli/FailWithDocComment.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "## This test should fail" }, .{ .stream = .stderr, .text = "add(1, 1) == 3" }, .{ .stream = .stderr, .text = "\u{2502}" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test verbose and non-verbose failure format match (interpreter)", .backend = .interpreter, .body = .{ .custom = .verbose_and_non_verbose_failure_format_match } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test verbose and non-verbose failure format match (dev)", .backend = .dev, .body = .{ .custom = .verbose_and_non_verbose_failure_format_match } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check returns exit code 2 for warnings", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/fx/run_warning_only.roc", .exit = .{ .code = 2 }, .contains = &.{.{ .stream = .stderr, .text = "0 error" }}, .contains_any = &.{.{ .needles = &warning_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check warns for adjacent string pattern captures", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/fx/string_pattern_adjacent_capture_warning.roc", .exit = .{ .code = 2 }, .contains = &.{ .{ .stream = .stderr, .text = "UNREACHABLE PATTERN CAPTURE" }, .{ .stream = .stderr, .text = "0 error" } }, .contains_any = &.{.{ .needles = &warning_needles }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check returns exit code 0 for no warnings or errors", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/simple_success.roc" } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check returns exit code 1 for errors", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/has_type_error_annotation.roc", .exit = .{ .code = 1 } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports comptime division by zero without panicking", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/comptime_div_zero.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "COMPTIME CRASH" }, .{ .stream = .stderr, .text = "I64 division by zero" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports comptime modulo by zero without panicking", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/comptime_mod_zero.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "COMPTIME CRASH" }, .{ .stream = .stderr, .text = "I64 division by zero" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check reports large default Dec scientific literal without panicking", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/large_scientific_default_dec.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "INVALID NUMBER" }, .{ .stream = .stderr, .text = "Dec" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check preserves numeric literal constraints before reporting large default Dec scientific literal", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/large_scientific_list_default_dec.roc", .exit = .failure, .contains = &.{ .{ .stream = .stderr, .text = "INVALID NUMBER" }, .{ .stream = .stderr, .text = "Dec" } }, .not_contains = &.{.{ .stream = .stderr, .text = "panic:" }} } } },
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
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds on string interpolation in Try.map_err (issue 9650)", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/issue_9650_checked_interpolation_map_err.roc", .exit = .success, .contains_any = &.{.{ .needles = &no_errors_needles }}, .not_contains = &.{ .{ .stream = .stderr, .text = "ordinary method call reached artifact publication" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc run default app numeric addition lowers without postcheck panic (issue 9706)", .body = .{ .command = .{ .args = &.{"--no-cache"}, .roc_file = "test/cli/issue_9706_dispatch_plan_addition.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "dispatch plan had no method owner" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds on Parser type module", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/package_simple_parser/Parser.roc", .not_contains = &.{.{ .stream = .stderr, .text = "error" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc check succeeds when block-local associated value captures local value", .body = .{ .command = .{ .args = &.{ "check", "--no-cache" }, .roc_file = "test/cli/block_local_assoc_capture/Test.roc", .exit = .success } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test runs expects in Parser type module (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/package_simple_parser/Parser.roc", .contains = &.{ .{ .stream = .stdout, .text = "passed" }, .{ .stream = .stdout, .text = "(7)" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test runs expects in Parser type module (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/package_simple_parser/Parser.roc", .contains = &.{ .{ .stream = .stdout, .text = "passed" }, .{ .stream = .stdout, .text = "(7)" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test polymorphic list reverse with numeric literal does not overflow (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter" }, .roc_file = "test/cli/polymorphic_list_reverse.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "overflow" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test polymorphic list reverse with numeric literal does not overflow (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev" }, .roc_file = "test/cli/polymorphic_list_reverse.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "panic" }, .{ .stream = .stderr, .text = "overflow" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test polymorphic list reverse within same module", .body = .{ .command = .{ .args = &.{"test"}, .roc_file = "test/cli/PolymorphicListReverseMod.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9388 List.sort_with top-level expect does not overflow", .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/Issue9388SortWithTopLevelExpect.roc", .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "overflowed its stack" }, .{ .stream = .stderr, .text = "Segmentation fault" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9487 static dispatch result compares to tag literal", .skip = .{ .windows = "issue 9487 static dispatch repro is run on POSIX only" }, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/Issue9487StaticDispatchEq.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Segmentation fault" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test issue 9636 F64 to_u64_try in expect does not crash", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/Issue9636FloatToU64TryExpect.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }}, .not_contains = &.{ .{ .stream = .stderr, .text = "Unreachable" }, .{ .stream = .stderr, .text = "reached unreachable code" }, .{ .stream = .stderr, .text = "panic" } } } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test eq on tag union with list payload does not panic", .body = .{ .command = .{ .args = &.{ "test", "--no-cache" }, .roc_file = "test/cli/EqTagWithListPayload.roc", .exit = .success, .not_contains = &.{.{ .stream = .stderr, .text = "panic" }} } } },
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
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Ok inside top-level expect passes (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/QuestionInExpect.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Ok inside top-level expect passes (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/QuestionInExpect.roc", .exit = .success, .contains = &.{.{ .stream = .stdout, .text = "passed" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Err inside top-level expect fails with snippet and value (interpreter)", .backend = .interpreter, .body = .{ .command = .{ .args = &.{ "test", "--opt=interpreter", "--no-cache" }, .roc_file = "test/cli/QuestionInExpectFail.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "The `?` operator in `to_positive(-3)?` evaluated an `Err` inside an `expect`. The value was: Err(IsNegative)" }, .{ .stream = .stderr, .text = "result = to_positive(-3)?" } }, .not_contains = &.{.{ .stream = .stderr, .text = "crash" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Err inside top-level expect fails with snippet and value (dev)", .backend = .dev, .body = .{ .command = .{ .args = &.{ "test", "--opt=dev", "--no-cache" }, .roc_file = "test/cli/QuestionInExpectFail.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "The `?` operator in `to_positive(-3)?` evaluated an `Err` inside an `expect`. The value was: Err(IsNegative)" }, .{ .stream = .stderr, .text = "result = to_positive(-3)?" } }, .not_contains = &.{.{ .stream = .stderr, .text = "crash" }} } } },
    .{ .id = 0, .suite = .subcommands, .name = "roc test ? on Err inside top-level expect fails with snippet and value (llvm)", .backend = .speed, .body = .{ .command = .{ .args = &.{ "test", "--opt=speed", "--no-cache" }, .roc_file = "test/cli/QuestionInExpectFail.roc", .exit = .{ .code = 1 }, .contains = &.{ .{ .stream = .stderr, .text = "The `?` operator in `to_positive(-3)?` evaluated an `Err` inside an `expect`. The value was: Err(IsNegative)" }, .{ .stream = .stderr, .text = "result = to_positive(-3)?" } }, .not_contains = &.{.{ .stream = .stderr, .text = "crash" }} } } },
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
var project_root_path: []const u8 = "";

const CaseEnv = struct {
    dirs: util.TestProcessDirs,
    env_map: std.process.Environ.Map,

    fn deinit(self: *CaseEnv, allocator: Allocator) void {
        self.env_map.deinit();
        self.dirs.deinit(allocator);
    }
};

fn buildCaseEnv(io: std.Io, allocator: Allocator) anyerror!CaseEnv {
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

    return .{
        .dirs = dirs,
        .env_map = env_map,
    };
}

fn deleteIfExists(io: std.Io, path: []const u8) anyerror!void {
    std.Io.Dir.cwd().deleteFile(io, path) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };
}

fn deleteOutputArtifacts(io: std.Io, allocator: Allocator, output_name: []const u8) anyerror!void {
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

fn absoluteFromProjectRoot(allocator: Allocator, path: []const u8) anyerror![]u8 {
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
    if (!processSucceeded(build_result.term)) {
        return resultFromProcess(build_result, timer, .build, build_ns, 0, "build failed");
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
) anyerror!std.process.RunResult {
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
) anyerror!std.process.RunResult {
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
) anyerror![]const []const u8 {
    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    try argv.append(allocator, roc_binary_path);
    try argv.appendSlice(allocator, args);
    if (roc_file) |path| {
        const resolved = switch (file_path_mode) {
            .absolute => try absoluteFromProjectRoot(allocator, path),
            .relative => path,
        };
        try argv.append(allocator, resolved);
    }
    return try argv.toOwnedSlice(allocator);
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
        .cli_cache_roots_distinct => customCliCacheRootsDistinct(io, allocator, &timer),
        .generated_graph_1_1 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 1, .symbols_per_file = 1 }),
        .generated_graph_5_5 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 5, .symbols_per_file = 5 }),
        .generated_graph_2_100 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 2, .symbols_per_file = 100 }),
        .generated_graph_200_5 => customGeneratedModuleGraph(io, allocator, &env, &timer, timeout_ms, .{ .roc_file_count = 200, .symbols_per_file = 5 }),
        .list_builtin_inlined => customListBuiltinInlined(io, allocator, &env, &timer, timeout_ms),
        .default_platform_linux_disassembly => customDefaultPlatformLinuxDisassembly(io, allocator, &env, &timer, timeout_ms),
        .default_platform_build_x64glibc => customDefaultPlatformBuild(io, allocator, &env, &timer, timeout_ms, .x64glibc),
        .default_platform_build_arm64glibc => customDefaultPlatformBuild(io, allocator, &env, &timer, timeout_ms, .arm64glibc),
        .default_platform_build_wasm32 => customDefaultPlatformBuild(io, allocator, &env, &timer, timeout_ms, .wasm32),
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
        .glue_c_header => customGlueCHeader(io, allocator, &env, &timer, timeout_ms),
        .glue_c_header_compiles => customGlueCHeaderCompiles(io, allocator, &env, &timer, timeout_ms),
        .glue_zig => customGlueZig(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_compiles => customGlueZigCompiles(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_opaque_box => customGlueZigOpaqueBox(io, allocator, &env, &timer, timeout_ms),
        .glue_rust => customGlueRust(io, allocator, &env, &timer, timeout_ms),
        .glue_zig_bang_record_fields => customGlueZigBangRecordFields(io, allocator, &env, &timer, timeout_ms),
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

fn backendOptArg(allocator: Allocator, backend: OptMode) anyerror![]const u8 {
    return std.fmt.allocPrint(allocator, "--opt={s}", .{backend.cliName()});
}

fn outputArg(allocator: Allocator, path: []const u8) anyerror![]const u8 {
    return std.fmt.allocPrint(allocator, "--output={s}", .{path});
}

fn fileExistsWithSize(io: std.Io, path: []const u8) anyerror!u64 {
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
) anyerror!?std.process.RunResult {
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

fn normalizedObjdumpInstructions(allocator: Allocator, objdump_stdout: []const u8) anyerror![]const u8 {
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

fn appendCanonicalInstruction(allocator: Allocator, result: *std.ArrayListUnmanaged(u8), instruction: []const u8) anyerror!void {
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

fn runnableOutputPath(io: std.Io, allocator: Allocator, output_path: []const u8) anyerror![]const u8 {
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
        \\        inputs: "targets/",
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
) anyerror![]const u8 {
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

fn writeGeneratedPackageModule(io: std.Io, dir: std.Io.Dir, config: GeneratedModuleGraphConfig) anyerror!void {
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
) anyerror!void {
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

fn countModuleCacheFiles(io: std.Io, allocator: Allocator, cache_path: []const u8) anyerror!usize {
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

fn createWorkSubdir(io: std.Io, allocator: Allocator, env: *const CaseEnv, name: []const u8) anyerror![]const u8 {
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
        \\    HostedFunctions funcs = {0};
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

fn customGlueRust(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
    const output_dir = createWorkSubdir(io, allocator, env, "glue-out") catch |err|
        return customInfraFailure(allocator, timer, "failed to create glue output dir: {}", .{err});
    if (runGlueCommandInEnv(io, allocator, env, timer, timeout_ms, "src/glue/src/RustGlue.roc", output_dir)) |failure| return failure;
    const generated_path = std.fs.path.join(allocator, &.{ output_dir, "roc_platform_abi.rs" }) catch |err|
        return customInfraFailure(allocator, timer, "failed to allocate generated Rust path: {}", .{err});
    const generated = std.Io.Dir.cwd().readFileAlloc(io, generated_path, allocator, .limited(1024 * 1024)) catch |err|
        return customFailure(allocator, timer, "failed to read generated Rust file: {}", .{err});
    for ([_][]const u8{ "pub struct RocOps", "pub struct RocStr", "PlatformHostedFns" }) |needle| {
        if (std.mem.find(u8, generated, needle) == null) {
            return customFailure(allocator, timer, "generated Rust file missing {s}", .{needle});
        }
    }
    // The old uniform-ABI argument struct must be gone (confirms RustGlue tracks the new
    // register-style host ABI rather than the struct-by-pointer callbacks).
    if (std.mem.find(u8, generated, "pub struct RocAlloc") != null) {
        return customFailure(allocator, timer, "generated Rust file still defines the obsolete RocAlloc struct", .{});
    }
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
        \\    abi.decrefBoxWith(ptr, @alignOf(usize), &payloadDrop, &host);
        \\
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.callback_count);
        \\    try std.testing.expectEqual(@as(isize, 0), env_value.callback_rc);
        \\    try std.testing.expectEqual(@as(usize, 1), env_value.dealloc_count);
        \\    try std.testing.expectEqual(@intFromPtr(&backing), env_value.dealloc_ptr);
        \\    try std.testing.expectEqual(@as(usize, @alignOf(usize)), env_value.dealloc_alignment);
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

fn customGlueZigBangRecordFields(io: std.Io, allocator: Allocator, env: *const CaseEnv, timer: *harness.Timer, timeout_ms: u64) ?TestResult {
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
fn buildCliWorkerArgvTemplate(io: std.Io, arena: Allocator, process_args: std.process.Args) anyerror![]const []const u8 {
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
) anyerror!void {
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
        \\  --verbose            Show PASS results with timing
        \\
    , .{});
}

const ParsedRunnerArgs = struct {
    standard: harness.StandardArgs,
    suites: SuiteSelection,
};

fn parseSuiteName(value: []const u8) ?Suite {
    for (all_suites) |suite| {
        if (std.mem.eql(u8, value, suite.cliName())) return suite;
    }
    return null;
}

fn parseRunnerArgs(allocator: Allocator, process_args: std.process.Args) anyerror!ParsedRunnerArgs {
    const raw_z = try process_args.toSlice(allocator);
    const raw_args: []const []const u8 = @ptrCast(raw_z);

    var standard_args: std.ArrayListUnmanaged([]const u8) = .empty;
    try standard_args.append(allocator, raw_args[0]);

    var suites = SuiteSelection{};
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
        try standard_args.append(allocator, arg);
    }

    if (!saw_suite or suites.isEmpty()) {
        suites.addAll();
    }

    return .{
        .standard = try harness.parseStandardArgsFromSlice(try standard_args.toOwnedSlice(allocator), allocator),
        .suites = suites,
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
pub fn main(init: std.process.Init) anyerror!void {
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

    const tests = try buildCases(spec_arena.allocator(), args.filters, args.include_llvm, parsed.suites);
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

//! Command line argument parsing for the CLI
const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const mem = std.mem;

/// Errors that can occur while parsing CLI arguments.
pub const ParseError = Allocator.Error || std.Io.Dir.OpenError || std.Io.Dir.Iterator.Error;

/// The core type representing a parsed command
/// We could use anonymous structs for the argument types instead of defining one for each command to be more concise,
/// but defining a struct per command means that we can easily take that type and pass it into the function that implements each command.
pub const CliArgs = union(enum) {
    run: RunArgs,
    check: CheckArgs,
    build: BuildArgs,
    fmt: FormatArgs,
    test_cmd: TestArgs,
    bundle: BundleArgs,
    unbundle: UnbundleArgs,
    repl: ReplArgs,
    glue: GlueArgs,
    version,
    docs: DocsArgs,
    experimental_lsp: ExperimentalLspArgs,
    help: []const u8,
    licenses,
    problem: ArgProblem,

    pub fn deinit(self: CliArgs, alloc: mem.Allocator) void {
        switch (self) {
            .fmt => |fmt| alloc.free(fmt.paths),
            .run => |run| alloc.free(run.app_args),
            .bundle => |bundle| alloc.free(bundle.paths),
            .unbundle => |unbundle| alloc.free(unbundle.paths),
            else => return,
        }
    }
};

/// Errors that can occur due to bad input while parsing the arguments
pub const ArgProblem = union(enum) {
    missing_flag_value: struct {
        flag: []const u8,
    },
    unexpected_argument: struct { cmd: []const u8, arg: []const u8 },
    invalid_flag_value: struct {
        value: []const u8,
        flag: []const u8,
        valid_options: []const u8,
    },
};

/// The optimization strategy for the compilation of a Roc program
pub const OptLevel = enum {
    size, // binary size (LLVM)
    speed, // execution speed (LLVM)
    dev, // speed of compilation (dev backend)
    interpreter,

    pub fn from_str(str: []const u8) ?OptLevel {
        if (mem.eql(u8, str, "speed")) return .speed;
        if (mem.eql(u8, str, "size")) return .size;
        if (mem.eql(u8, str, "dev")) return .dev;
        if (mem.eql(u8, str, "interpreter")) return .interpreter;
        return null;
    }

    /// Convert to the backend evaluation enum used by internal modules
    pub fn toBackend(self: OptLevel) @import("eval").EvalBackend {
        return switch (self) {
            .interpreter => .interpreter,
            .dev => .dev,
            .size, .speed => .llvm,
        };
    }
};

/// Package download size limits for commands that resolve dependencies.
/// Values are in megabytes; 0 means unlimited; null uses the default.
pub const ResolveLimitArgs = struct {
    max_package_mb: ?u32 = null, // per-package decompressed size limit (default 10)
    max_transitive_mb: ?u32 = null, // per-direct-dependency transitive size limit (default 100)
};

const ResolveLimitParse = union(enum) {
    not_matched,
    ok,
    problem: ArgProblem,
};

fn parseResolveLimitFlag(arg: []const u8, limits: *ResolveLimitArgs) ResolveLimitParse {
    const flags = .{
        .{ "--max-package-mb", &limits.max_package_mb },
        .{ "--max-transitive-mb", &limits.max_transitive_mb },
    };
    inline for (flags) |flag| {
        if (mem.startsWith(u8, arg, flag[0])) {
            if (getFlagValue(arg)) |value| {
                flag[1].* = std.fmt.parseInt(u32, value, 10) catch {
                    return .{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = flag[0], .value = value, .valid_options = "size in MB (0 for unlimited)" } } };
                };
                return .ok;
            }
            return .{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = flag[0] } } };
        }
    }
    return .not_matched;
}

const resolve_limit_help =
    \\      --max-package-mb=<N>     Per-package decompressed size limit in MB (default: 10, 0 for unlimited)
    \\      --max-transitive-mb=<N>  Combined size limit in MB for each direct dependency's transitive packages (default: 100, 0 for unlimited)
;

/// Arguments for the default `roc` command
pub const RunArgs = struct {
    path: []const u8, // the path of the roc file to be executed
    opt: OptLevel = .dev, // the optimization level (dev, interpreter, size, speed)
    target: ?[]const u8 = null, // the target to compile for (e.g., x64musl, x64glibc)
    app_args: []const []const u8 = &[_][]const u8{}, // any arguments to be passed to roc application being run
    no_cache: bool = false, // bypass the executable cache
    allow_errors: bool = false, // allow execution even if there are type errors
    watch: bool = false, // hot reload when source inputs change
    timings: bool = false, // always show the per-phase timing breakdown
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
};

/// Arguments for `roc check`
pub const CheckArgs = struct {
    path: []const u8, // the path of the roc file to be checked
    main: ?[]const u8, // the path to a roc file with an app header to be used to resolved dependencies
    time: bool = false, // whether to print timing information
    timings: bool = false, // always show the per-phase timing breakdown
    no_cache: bool = false, // disable cache
    verbose: bool = false, // enable verbose output
    watch: bool = false, // rerun check when source inputs change
    watch_inputs_file: ?[]const u8 = null, // internal: write watch input paths and byte states here
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
};

/// Arguments for `roc build`
pub const BuildArgs = struct {
    path: []const u8, // the path to the roc file to be built
    opt: OptLevel, // the optimization level (dev, interpreter, size, speed)
    target: ?[]const u8 = null, // the target to compile for (e.g., x64musl, x64glibc)
    output: ?[]const u8 = null, // the path where the output binary should be created
    debug: bool = false, // include debug information in the output binary
    allow_errors: bool = false, // allow building even if there are type errors
    verbose: bool = false, // enable verbose output including cache statistics
    timings: bool = false, // always show the per-phase timing breakdown
    no_cache: bool = false, // disable compilation caching
    watch: bool = false, // rebuild when source inputs change
    watch_inputs_file: ?[]const u8 = null, // internal: write watch input paths and byte states here
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    wasm_memory: ?usize = null, // initial memory size for WASM targets (default: 64MB)
    wasm_stack_size: ?usize = null, // stack size for WASM targets (default: 8MB)
    exit_on_warnings: bool = true, // exit with code 2 when warnings are emitted
    warning_count_out: ?*usize = null, // optionally receive the total warning count
    require_executable_output: bool = false, // reject static/shared library targets
    require_host_runnable_output: bool = false, // internal: reject targets that cannot run on this host
    suppress_build_status: bool = false, // suppress "Built..." output (used by `roc` execution)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
    synthetic_default_platform: bool = false, // internal: build rewrote a headerless app to the default platform
    source_dir_override: ?[]const u8 = null, // internal: resolve root sibling imports from this directory
    synthetic_root_original_path: ?[]const u8 = null, // internal: original path for a synthetic default-app root
    synthetic_root_original_source: ?[]const u8 = null, // internal: normalized original source for synthetic-root diagnostics
    synthetic_root_header_len: usize = 0, // internal: byte length of the header prepended to synthetic_root_original_source
};

/// Arguments for `roc test`
pub const TestArgs = struct {
    path: []const u8, // the path to the file to be tested
    opt: OptLevel, // the optimization level (dev, interpreter, size, speed)
    main: ?[]const u8, // the path to a roc file with an app header to be used to resolve dependencies
    verbose: bool = false, // enable verbose output showing individual test results
    no_cache: bool = false, // disable compilation caching, force re-run all tests
    watch: bool = false, // rerun tests when source inputs change
    watch_inputs_file: ?[]const u8 = null, // internal: write watch input paths and byte states here
    max_threads: ?usize = null, // max worker threads (null = auto, 1 = single-threaded)
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
};

/// Arguments for `roc format`
pub const FormatArgs = struct {
    paths: []const []const u8, // the paths of files to be formatted
    stdin: bool = false, // if the input should be read in from stdin and output to stdout
    check: bool = false, // if the command should only check formatting rather than applying it
};

/// Arguments for `roc bundle`
pub const BundleArgs = struct {
    paths: []const []const u8, // the paths of roc files to bundle
    output_dir: ?[]const u8 = null, // the directory to output the bundle to
    compression_level: i32 = 3, // zstd compression level (1-22, default 3)
};

/// Arguments for `roc unbundle`
pub const UnbundleArgs = struct {
    paths: []const []const u8, // the paths of .tar.zst files to unbundle
};

/// Arguments for `roc docs`
pub const DocsArgs = struct {
    path: []const u8, // the path of the roc file to generate docs for
    main: ?[]const u8 = null, // the path to a roc file with an app header to be used to resolved dependencies
    output: []const u8 = "generated-docs", // the output directory for generated documentation
    time: bool = false, // whether to print timing information
    no_cache: bool = false, // disable cache
    verbose: bool = false, // enable verbose output
    serve: bool = false, // start an HTTP server after generating docs
    with_lang_ref: bool = false, // include the language reference articles from docs/langref
    resolve_limits: ResolveLimitArgs = .{}, // package download size limits
};

/// Arguments for `roc experimental-lsp`
pub const ExperimentalLspArgs = struct {
    debug_io: bool = false, // log the LSP messages to a temporary log file
    debug_build: bool = false,
    debug_syntax: bool = false,
    debug_server: bool = false,
};

/// Arguments for `roc repl`
pub const ReplArgs = struct {
    opt: OptLevel = .dev,
    no_color: bool = false,
};

/// Arguments for `roc glue`
pub const GlueArgs = struct {
    glue_spec: []const u8, // path to the glue spec .roc file (REQUIRED)
    output_dir: []const u8, // path to the output directory for generated glue files (REQUIRED)
    platform_path: []const u8, // path to the platform .roc file (default: main.roc)
    opt: OptLevel = .dev,
};

/// Parse a list of arguments.
pub fn parse(alloc: mem.Allocator, std_io: std.Io, args: []const []const u8) ParseError!CliArgs {
    if (args.len == 0) return try parseRun(alloc, args);

    // "run" is not a valid subcommand - give a helpful error
    // The correct usage is: roc path/to/app.roc (without "run")
    if (mem.eql(u8, args[0], "run")) {
        return CliArgs{ .help = run_not_a_command_help };
    }
    if (mem.eql(u8, args[0], "check")) return parseCheck(args[1..]);
    if (mem.eql(u8, args[0], "build")) return parseBuild(args[1..]);
    if (mem.eql(u8, args[0], "bundle")) return try parseBundle(alloc, args[1..]);
    if (mem.eql(u8, args[0], "unbundle")) return try parseUnbundle(alloc, std_io, args[1..]);
    if (mem.eql(u8, args[0], "fmt")) return try parseFormat(alloc, args[1..]);
    if (mem.eql(u8, args[0], "test")) return parseTest(args[1..]);
    if (mem.eql(u8, args[0], "repl")) return parseRepl(args[1..]);
    if (mem.eql(u8, args[0], "glue")) return parseGlue(args[1..]);
    if (mem.eql(u8, args[0], "version")) return parseVersion(args[1..]);
    if (mem.eql(u8, args[0], "docs")) return parseDocs(args[1..]);
    if (mem.eql(u8, args[0], "experimental-lsp")) return parseExperimentalLsp(args[1..]);
    if (mem.eql(u8, args[0], "help")) return CliArgs{ .help = main_help };
    if (mem.eql(u8, args[0], "licenses")) return parseLicenses(args[1..]);

    return try parseRun(alloc, args);
}

const main_help =
    \\Run the given .roc file
    \\You can use one of the COMMANDS below to do something else!
    \\
    \\Usage: roc [OPTIONS] [ROC_FILE] [ARGS_FOR_APP]...
    \\       roc <COMMAND>
    \\
    \\Commands:
    \\  build            Build a binary from the given .roc file, but don't run it
    \\  bundle           Bundle .roc files into a compressed archive
    \\  unbundle         Extract files from compressed .tar.zst archives
    \\  test             Run all top-level `expect`s in a main module and any modules it imports
    \\  repl             Launch the interactive Read Eval Print Loop (REPL)
    \\  fmt              Format a .roc file or the .roc files contained in a directory using standard Roc formatting
    \\  glue             Generate native glue code from a Roc platform using a language-specific glue spec
    \\  version          Print the Roc compiler's version
    \\  check            Check the code for problems, but don't build or run it
    \\  docs             Generate documentation for a Roc package or platform
    \\  experimental-lsp Start the experimental language server (LSP) implementation
    \\  help             Print this message
    \\  licenses         Prints license info for Roc as well as attributions to other projects used by Roc
    \\
    \\Arguments:
    \\  [ROC_FILE]         The .roc file of an app to run [default: main.roc]
    \\  [ARGS_FOR_APP]...  Arguments to pass into the app being run
    \\                     e.g. `roc app.roc -- arg1 arg2`
    \\Options:
    \\      --opt=<opt>                    Execution mode: dev (default, fast compilation), interpreter, size (LLVM) or speed (LLVM)
    \\      --target=<target>              Target to compile for (e.g., x64musl, x64glibc, arm64musl). Defaults to native target with musl for static linking
    \\      --no-cache                     Disable compilation and executable caches (useful for compiler and platform developers)
    \\      --allow-errors                 Allow execution even if there are type errors (warnings are always allowed)
    \\  -j, --jobs=<N>                     Max worker threads for parallel compilation (default: auto-detect CPU count)
    \\
;

const run_not_a_command_help =
    \\Error: 'run' is not a valid subcommand.
    \\
    \\To run a Roc application, use:
    \\    roc path/to/app.roc
    \\
    \\For example:
    \\    roc main.roc           Run main.roc in the current directory
    \\    roc examples/hello.roc Run hello.roc from the examples folder
    \\
    \\Use 'roc help' to see all available commands.
    \\
;

fn parseCheck(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var main: ?[]const u8 = null;
    var time: bool = false;
    var timings: bool = false;
    var no_cache: bool = false;
    var verbose: bool = false;
    var watch: bool = false;
    var watch_inputs_file: ?[]const u8 = null;
    var max_threads: ?usize = null;
    var resolve_limits: ResolveLimitArgs = .{};

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Check the code for problems, but don't build or run it
            \\
            \\Usage: roc check [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE]  The .roc file to check [default: main.roc]
            \\
            \\Options:
            \\      --main=<main>  The .roc file of the main app/package module to resolve dependencies from
            \\      --time         Print timing information for each compilation phase. Will not print anything if everything is cached.
            \\      --timings      Show how long each compilation phase took (shown automatically when checking is slow)
            \\      --no-cache     Disable caching
            \\      --verbose      Enable verbose output including cache statistics
            \\      --watch        Re-run when source inputs change
            \\  -j, --jobs=<N>     Max worker threads for parallel compilation (default: auto-detect CPU count)
            ++ "\n" ++ resolve_limit_help ++ "\n" ++
                \\  -h, --help         Print help
                \\
            };
        } else if (mem.startsWith(u8, arg, "--max-package-mb") or mem.startsWith(u8, arg, "--max-transitive-mb")) {
            switch (parseResolveLimitFlag(arg, &resolve_limits)) {
                .problem => |problem| return CliArgs{ .problem = problem },
                else => {},
            }
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (getFlagValue(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.eql(u8, arg, "--time")) {
            time = true;
        } else if (mem.eql(u8, arg, "--timings")) {
            timings = true;
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else if (mem.eql(u8, arg, "--watch")) {
            watch = true;
        } else if (mem.startsWith(u8, arg, "--watch-inputs-file")) {
            if (getFlagValue(arg)) |value| {
                watch_inputs_file = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--watch-inputs-file" } } };
            }
        } else if (mem.startsWith(u8, arg, "--jobs")) {
            if (getFlagValue(arg)) |value| {
                max_threads = std.fmt.parseInt(usize, value, 10) catch {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--jobs", .value = value, .valid_options = "positive integer" } } };
                };
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--jobs" } } };
            }
        } else if (mem.startsWith(u8, arg, "-j")) {
            // Handle -jN format (e.g., -j4)
            const value = arg[2..];
            if (value.len == 0) {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "-j" } } };
            }
            max_threads = std.fmt.parseInt(usize, value, 10) catch {
                return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "-j", .value = value, .valid_options = "positive integer" } } };
            };
        } else {
            if (path != null) {
                return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "check", .arg = arg } } };
            }
            path = arg;
        }
    }

    return CliArgs{ .check = CheckArgs{ .path = path orelse "main.roc", .main = main, .time = time, .timings = timings, .no_cache = no_cache, .verbose = verbose, .watch = watch, .watch_inputs_file = watch_inputs_file, .max_threads = max_threads, .resolve_limits = resolve_limits } };
}

fn parseBuild(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .speed;
    var target: ?[]const u8 = null;
    var output: ?[]const u8 = null;
    var debug: bool = false;
    var allow_errors: bool = false;
    var verbose: bool = false;
    var timings: bool = false;
    var no_cache: bool = false;
    var max_threads: ?usize = null;
    var wasm_memory: ?usize = null;
    var wasm_stack_size: ?usize = null;
    var watch: bool = false;
    var watch_inputs_file: ?[]const u8 = null;
    var resolve_limits: ResolveLimitArgs = .{};
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Build a binary from the given .roc file, but don't run it
            \\
            \\Usage: roc build [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE] The .roc file to build [default: main.roc]
            \\
            \\Options:
            \\      --output=<output>              The full path to the output binary, including filename. To specify directory only, specify a path that ends in a directory separator (e.g. a slash)
            \\      --opt=<opt>                    Build mode: speed (default LLVM optimized), size (LLVM optimized for binary size), dev (native dev backend), or interpreter (embedded interpreter backend)
            \\      --target=<target>              Target to compile for (e.g., x64musl, x64glibc, arm64musl). Defaults to native target with musl for static linking
            \\      --debug                        Include debug information in the output binary
            \\      --allow-errors                 Allow building even if there are type errors (warnings are always allowed)
            \\      --verbose                      Enable verbose output including cache statistics
            \\      --timings                      Show how long each compilation phase took (shown automatically when a build is slow)
            \\      --no-cache                     Disable compilation caching
            \\      --watch                        Rebuild when source inputs change
            \\  -j, --jobs=<N>                     Max worker threads for parallel compilation (default: auto-detect CPU count)
            \\      --wasm-memory=<bytes>          Initial memory size for WASM targets in bytes (default: 67108864 = 64MB)
            \\      --wasm-stack-size=<bytes>      Stack size for WASM targets in bytes (default: 8388608 = 8MB)
            \\      -h, --help                     Print help
            \\
            };
        } else if (mem.startsWith(u8, arg, "--max-package-mb") or mem.startsWith(u8, arg, "--max-transitive-mb")) {
            switch (parseResolveLimitFlag(arg, &resolve_limits)) {
                .problem => |problem| return CliArgs{ .problem = problem },
                else => {},
            }
        } else if (mem.startsWith(u8, arg, "--target")) {
            if (getFlagValue(arg)) |value| {
                target = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--target" } } };
            }
        } else if (mem.startsWith(u8, arg, "--output")) {
            if (getFlagValue(arg)) |value| {
                output = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--output" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "dev,interpreter,speed,size" } } };
                }
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else if (mem.eql(u8, arg, "--debug")) {
            debug = true;
        } else if (mem.eql(u8, arg, "--allow-errors")) {
            allow_errors = true;
        } else if (mem.startsWith(u8, arg, "--wasm-memory")) {
            if (getFlagValue(arg)) |value| {
                wasm_memory = std.fmt.parseInt(usize, value, 10) catch {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--wasm-memory", .value = value, .valid_options = "positive integer (bytes)" } } };
                };
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--wasm-memory" } } };
            }
        } else if (mem.startsWith(u8, arg, "--wasm-stack-size")) {
            if (getFlagValue(arg)) |value| {
                wasm_stack_size = std.fmt.parseInt(usize, value, 10) catch {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--wasm-stack-size", .value = value, .valid_options = "positive integer (bytes)" } } };
                };
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--wasm-stack-size" } } };
            }
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else if (mem.eql(u8, arg, "--timings")) {
            timings = true;
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--watch")) {
            watch = true;
        } else if (mem.startsWith(u8, arg, "--watch-inputs-file")) {
            if (getFlagValue(arg)) |value| {
                watch_inputs_file = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--watch-inputs-file" } } };
            }
        } else if (mem.startsWith(u8, arg, "--jobs")) {
            if (getFlagValue(arg)) |value| {
                max_threads = std.fmt.parseInt(usize, value, 10) catch {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--jobs", .value = value, .valid_options = "positive integer" } } };
                };
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--jobs" } } };
            }
        } else if (mem.startsWith(u8, arg, "-j")) {
            // Handle -j<N> (no space) or -j <N> (with space handled by next iteration)
            const value = arg[2..];
            if (value.len > 0) {
                max_threads = std.fmt.parseInt(usize, value, 10) catch {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "-j", .value = value, .valid_options = "positive integer" } } };
                };
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "-j" } } };
            }
        } else {
            if (path != null) {
                return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "build", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .build = BuildArgs{ .path = path orelse "main.roc", .opt = opt, .target = target, .output = output, .debug = debug, .allow_errors = allow_errors, .verbose = verbose, .timings = timings, .no_cache = no_cache, .watch = watch, .watch_inputs_file = watch_inputs_file, .max_threads = max_threads, .wasm_memory = wasm_memory, .wasm_stack_size = wasm_stack_size, .resolve_limits = resolve_limits } };
}

fn parseBundle(alloc: mem.Allocator, args: []const []const u8) std.mem.Allocator.Error!CliArgs {
    var paths = try std.array_list.Managed([]const u8).initCapacity(alloc, 16);
    var output_dir: ?[]const u8 = null;
    var compression_level: i32 = 3;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (isHelpFlag(arg)) {
            paths.deinit();
            return CliArgs{ .help =
            \\Bundle .roc files into a compressed archive
            \\
            \\Usage: roc bundle [OPTIONS] [ROC_FILES]...
            \\
            \\Arguments:
            \\  [ROC_FILES]...  The .roc files to bundle [default: main.roc]
            \\
            \\Options:
            \\      --output-dir <PATH>  Directory to output the bundle to [default: current directory]
            \\      --compression <N>    Compression level (1-22) [default: 3]
            \\  -h, --help               Print help
            \\
            };
        } else if (mem.eql(u8, arg, "--output-dir")) {
            if (i + 1 >= args.len) {
                paths.deinit();
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--output-dir" } } };
            }
            i += 1;
            output_dir = args[i];
        } else if (mem.eql(u8, arg, "--compression")) {
            if (i + 1 >= args.len) {
                paths.deinit();
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--compression" } } };
            }
            i += 1;
            compression_level = std.fmt.parseInt(i32, args[i], 10) catch {
                paths.deinit();
                return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .value = args[i], .flag = "--compression", .valid_options = "integer between 1 and 22" } } };
            };
            if (compression_level < 1 or compression_level > 22) {
                paths.deinit();
                return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .value = args[i], .flag = "--compression", .valid_options = "integer between 1 and 22" } } };
            }
        } else if (mem.startsWith(u8, arg, "--")) {
            paths.deinit();
            return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "bundle", .arg = arg } } };
        } else {
            try paths.append(arg);
        }
    }

    // Default to main.roc if no files specified
    if (paths.items.len == 0) {
        try paths.append("main.roc");
    }

    return CliArgs{ .bundle = BundleArgs{
        .paths = try paths.toOwnedSlice(),
        .output_dir = output_dir,
        .compression_level = compression_level,
    } };
}

fn parseUnbundle(alloc: mem.Allocator, std_io: std.Io, args: []const []const u8) ParseError!CliArgs {
    var paths = try std.array_list.Managed([]const u8).initCapacity(alloc, 16);

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            paths.deinit();
            return CliArgs{ .help =
            \\Extract files from compressed .tar.zst archives
            \\
            \\Usage: roc unbundle [OPTIONS] [ARCHIVE_FILES]...
            \\
            \\Arguments:
            \\  [ARCHIVE_FILES]...  The .tar.zst files to unbundle
            \\                      [default: all .tar.zst files in current directory]
            \\
            \\Options:
            \\  -h, --help  Print help
            \\
            };
        } else if (mem.startsWith(u8, arg, "-")) {
            paths.deinit();
            return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "unbundle", .arg = arg } } };
        } else {
            try paths.append(arg);
        }
    }

    // If no paths specified, default to all .tar.zst files in current directory
    if (paths.items.len == 0) {
        var cwd = try std.Io.Dir.cwd().openDir(std_io, ".", .{ .iterate = true });
        defer cwd.close(std_io);
        var iter = cwd.iterate();
        while (try iter.next(std_io)) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".tar.zst")) {
                try paths.append(try alloc.dupe(u8, entry.name));
            }
        }

        // If still no files found, show help
        if (paths.items.len == 0) {
            paths.deinit();
            return CliArgs{ .help =
            \\Extract files from compressed .tar.zst archives
            \\
            \\Usage: roc unbundle [OPTIONS] [ARCHIVE_FILES]...
            \\
            \\Arguments:
            \\  [ARCHIVE_FILES]...  The .tar.zst files to unbundle
            \\                      [default: all .tar.zst files in current directory]
            \\
            \\Options:
            \\  -h, --help  Print help
            \\
            \\Error: No .tar.zst files found in current directory
            \\
            };
        }
    }

    return CliArgs{ .unbundle = UnbundleArgs{
        .paths = try paths.toOwnedSlice(),
    } };
}

fn parseFormat(alloc: mem.Allocator, args: []const []const u8) std.mem.Allocator.Error!CliArgs {
    var paths = try std.array_list.Managed([]const u8).initCapacity(alloc, 16);
    var stdin = false;
    var check = false;
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            // We need to free the paths here because we aren't returning the .format variant
            paths.deinit();
            return CliArgs{ .help =
            \\Format a .roc file or the .roc files contained in a directory using standard Roc formatting
            \\
            \\Usage: roc fmt [OPTIONS] [DIRECTORY_OR_FILES]
            \\
            \\Arguments:
            \\  [DIRECTORY_OR_FILES]
            \\
            \\Options:
            \\      --check  Checks that specified files are formatted
            \\               (If formatting is needed, return a non-zero exit code.)
            \\      --stdin  Format code from stdin; output to stdout
            \\  -h, --help   Print help
            \\
            \\If DIRECTORY_OR_FILES is omitted, the .roc files in the current working directory are formatted.
            \\
            };
        } else if (mem.eql(u8, arg, "--stdin")) {
            stdin = true;
        } else if (mem.eql(u8, arg, "--check")) {
            check = true;
        } else {
            try paths.append(arg);
        }
    }
    if (paths.items.len == 0) {
        try paths.append("main.roc");
    }
    return CliArgs{ .fmt = FormatArgs{ .paths = try paths.toOwnedSlice(), .stdin = stdin, .check = check } };
}

fn parseTest(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var main: ?[]const u8 = null;
    var verbose: bool = false;
    var no_cache: bool = false;
    var watch: bool = false;
    var watch_inputs_file: ?[]const u8 = null;
    var max_threads: ?usize = null;
    var resolve_limits: ResolveLimitArgs = .{};
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Run all top-level `expect`s in a main module and any modules it imports
            \\
            \\Usage: roc test [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE] The .roc file to test [default: main.roc]
            \\
            \\Options:
            \\      --opt=<opt>                     Execution mode: dev (default, fast compilation), interpreter, size (LLVM) or speed (LLVM)
            \\      --main <main>                   The .roc file of the main app/package module to resolve dependencies from
            \\      --verbose                       Enable verbose output showing individual test results
            \\      --no-cache                      Disable compilation caching, force re-run all tests
            \\      --watch                         Re-run when source inputs change
            \\  -j, --jobs=<N>                      Max worker threads for parallel compilation (default: auto-detect CPU count)
            \\  -h, --help                          Print help
            \\
            };
        } else if (mem.startsWith(u8, arg, "--max-package-mb") or mem.startsWith(u8, arg, "--max-transitive-mb")) {
            switch (parseResolveLimitFlag(arg, &resolve_limits)) {
                .problem => |problem| return CliArgs{ .problem = problem },
                else => {},
            }
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (getFlagValue(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "dev,interpreter,speed,size" } } };
                }
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--watch")) {
            watch = true;
        } else if (mem.startsWith(u8, arg, "--watch-inputs-file")) {
            if (getFlagValue(arg)) |value| {
                watch_inputs_file = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--watch-inputs-file" } } };
            }
        } else if (mem.startsWith(u8, arg, "--jobs")) {
            if (getFlagValue(arg)) |value| {
                max_threads = std.fmt.parseInt(usize, value, 10) catch {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--jobs", .value = value, .valid_options = "positive integer" } } };
                };
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--jobs" } } };
            }
        } else if (mem.startsWith(u8, arg, "-j")) {
            // Handle -jN format (e.g., -j4)
            const value = arg[2..];
            if (value.len == 0) {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "-j" } } };
            }
            max_threads = std.fmt.parseInt(usize, value, 10) catch {
                return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "-j", .value = value, .valid_options = "positive integer" } } };
            };
        } else {
            if (path != null) {
                return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "test", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .test_cmd = TestArgs{ .path = path orelse "main.roc", .opt = opt, .main = main, .verbose = verbose, .no_cache = no_cache, .watch = watch, .watch_inputs_file = watch_inputs_file, .max_threads = max_threads, .resolve_limits = resolve_limits } };
}

fn parseRepl(args: []const []const u8) CliArgs {
    var opt: OptLevel = .dev;
    var no_color: bool = false;

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Launch the interactive Read Eval Print Loop (REPL)
            \\
            \\Usage: roc repl [OPTIONS]
            \\
            \\Options:
            \\      --opt=<opt>  Execution mode: dev (default, fast compilation), interpreter, size (LLVM) or speed (LLVM)
            \\      --no-color   Do not use ANSI color codes in REPL diagnostics
            \\  -h, --help       Print help
            \\
            };
        } else if (mem.eql(u8, arg, "--no-color")) {
            no_color = true;
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "dev,interpreter,speed,size" } } };
                }
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else {
            return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "repl", .arg = arg } } };
        }
    }
    return CliArgs{ .repl = .{ .opt = opt, .no_color = no_color } };
}

fn parseGlue(args: []const []const u8) CliArgs {
    var glue_spec: ?[]const u8 = null;
    var output_dir: ?[]const u8 = null;
    var platform_path: ?[]const u8 = null;
    var opt: OptLevel = .dev;

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Generate glue code from a platform using a glue spec
            \\
            \\Usage: roc glue [OPTIONS] <GLUE_SPEC> <GLUE_DIR> [ROC_FILE]
            \\
            \\Arguments:
            \\  <GLUE_SPEC>  The glue spec .roc file that defines how to generate glue code
            \\  <GLUE_DIR>   The output directory for generated glue files
            \\  [ROC_FILE]   The platform .roc file to analyze [default: main.roc]
            \\
            \\Options:
            \\  --opt=<level>  Run the glue spec with dev or interpreter [default: dev]
            \\  -h, --help     Print help
            \\
            };
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    switch (level) {
                        .dev, .interpreter => opt = level,
                        .speed, .size => return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "dev,interpreter" } } },
                    }
                } else {
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "dev,interpreter" } } };
                }
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else {
            if (glue_spec == null) {
                glue_spec = arg;
            } else if (output_dir == null) {
                output_dir = arg;
            } else if (platform_path == null) {
                platform_path = arg;
            } else {
                return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "glue", .arg = arg } } };
            }
        }
    }

    // glue_spec is required
    if (glue_spec == null) {
        return CliArgs{ .help =
        \\Error: Missing required argument <GLUE_SPEC>
        \\
        \\Generate glue code from a platform using a glue spec
        \\
        \\Usage: roc glue [OPTIONS] <GLUE_SPEC> <GLUE_DIR> [ROC_FILE]
        \\
        \\Arguments:
        \\  <GLUE_SPEC>  The glue spec .roc file that defines how to generate glue code
        \\  <GLUE_DIR>   The output directory for generated glue files
        \\  [ROC_FILE]   The platform .roc file to analyze [default: main.roc]
        \\
        \\Options:
        \\  --opt=<level>  Run the glue spec with dev or interpreter [default: dev]
        \\  -h, --help     Print help
        \\
        };
    }

    // output_dir is required
    if (output_dir == null) {
        return CliArgs{ .help =
        \\Error: Missing required argument <GLUE_DIR>
        \\
        \\Generate glue code from a platform using a glue spec
        \\
        \\Usage: roc glue [OPTIONS] <GLUE_SPEC> <GLUE_DIR> [ROC_FILE]
        \\
        \\Arguments:
        \\  <GLUE_SPEC>  The glue spec .roc file that defines how to generate glue code
        \\  <GLUE_DIR>   The output directory for generated glue files
        \\  [ROC_FILE]   The platform .roc file to analyze [default: main.roc]
        \\
        \\Options:
        \\  --opt=<level>  Run the glue spec with dev or interpreter [default: dev]
        \\  -h, --help     Print help
        \\
        };
    }

    return CliArgs{ .glue = GlueArgs{
        .glue_spec = glue_spec.?,
        .output_dir = output_dir.?,
        .platform_path = platform_path orelse "main.roc",
        .opt = opt,
    } };
}

fn parseVersion(args: []const []const u8) CliArgs {
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Print the Roc compiler’s version
            \\
            \\Usage: roc version
            \\
            \\Options:
            \\  -h, --help  Print help
            \\
            };
        } else {
            return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "version", .arg = arg } } };
        }
    }
    return CliArgs.version;
}

fn parseLicenses(args: []const []const u8) CliArgs {
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Prints license info for Roc as well as attributions to other projects used by Roc
            \\
            \\Usage: roc licenses
            \\
            \\Options:
            \\  -h, --help  Print help
            \\
            };
        } else {
            return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "licenses", .arg = arg } } };
        }
    }
    return CliArgs.licenses;
}

fn parseDocs(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var main: ?[]const u8 = null;
    var output: ?[]const u8 = null;
    var time: bool = false;
    var no_cache: bool = false;
    var verbose: bool = false;
    var serve: bool = false;
    var with_lang_ref: bool = false;
    var resolve_limits: ResolveLimitArgs = .{};

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Generate documentation for a Roc package
            \\
            \\Usage: roc docs [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE]  The .roc file to generate docs for [default: main.roc]
            \\
            \\Options:
            \\      --main=<main>    The .roc file of the main app/package module to resolve dependencies from
            \\      --output=<dir>   Output directory for generated documentation [default: generated-docs]
            \\      --serve          Start an HTTP server to view the documentation
            \\      --with-lang-ref  Include the language reference articles from docs/langref
            \\      --time           Print timing information for each compilation phase. Will not print anything if everything is cached.
            \\      --no-cache       Disable caching
            \\      --verbose        Enable verbose output including cache statistics
            \\  -h, --help           Print help
            \\
            };
        } else if (mem.startsWith(u8, arg, "--max-package-mb") or mem.startsWith(u8, arg, "--max-transitive-mb")) {
            switch (parseResolveLimitFlag(arg, &resolve_limits)) {
                .problem => |problem| return CliArgs{ .problem = problem },
                else => {},
            }
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (getFlagValue(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.startsWith(u8, arg, "--output")) {
            if (getFlagValue(arg)) |value| {
                output = value;
            } else {
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--output" } } };
            }
        } else if (mem.eql(u8, arg, "--serve")) {
            serve = true;
        } else if (mem.eql(u8, arg, "--with-lang-ref")) {
            with_lang_ref = true;
        } else if (mem.eql(u8, arg, "--time")) {
            time = true;
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else {
            if (path != null) {
                return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "docs", .arg = arg } } };
            }
            path = arg;
        }
    }

    return CliArgs{ .docs = DocsArgs{ .path = path orelse "main.roc", .main = main, .output = output orelse "generated-docs", .time = time, .no_cache = no_cache, .verbose = verbose, .serve = serve, .with_lang_ref = with_lang_ref, .resolve_limits = resolve_limits } };
}

fn parseExperimentalLsp(args: []const []const u8) CliArgs {
    var debug_io = false;
    var debug_build = false;
    var debug_syntax = false;
    var debug_server = false;

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help =
            \\Start the experimental Roc language server (LSP)
            \\
            \\Usage: roc experimental-lsp [OPTIONS]
            \\
            \\Options:
            \\      --stdio            Communicate over stdio (the default and only
            \\                         transport; accepted for compatibility with LSP
            \\                         clients that pass it explicitly)
            \\      --debug-transport  Mirror all JSON-RPC traffic to a temp log file
            \\      --debug-build      Log build environment actions to the debug log
            \\      --debug-syntax     Log syntax/type checking steps to the debug log
            \\      --debug-server     Log server lifecycle details to the debug log
            \\  -h, --help            Print help
            \\
            };
        } else if (mem.eql(u8, arg, "--stdio")) {
            // LSP clients (e.g. vscode-languageclient) append a transport flag to
            // the server command line. We only ever speak LSP over stdio, so accept
            // `--stdio` as a no-op rather than rejecting it as an unexpected argument.
        } else if (mem.eql(u8, arg, "--debug-transport")) {
            debug_io = true;
        } else if (mem.eql(u8, arg, "--debug-build")) {
            debug_build = true;
        } else if (mem.eql(u8, arg, "--debug-syntax")) {
            debug_syntax = true;
        } else if (mem.eql(u8, arg, "--debug-server")) {
            debug_server = true;
        } else {
            return CliArgs{ .problem = ArgProblem{ .unexpected_argument = .{ .cmd = "experimental-lsp", .arg = arg } } };
        }
    }

    return CliArgs{ .experimental_lsp = .{
        .debug_io = debug_io,
        .debug_build = debug_build,
        .debug_syntax = debug_syntax,
        .debug_server = debug_server,
    } };
}

fn parseRun(alloc: mem.Allocator, args: []const []const u8) std.mem.Allocator.Error!CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var target: ?[]const u8 = null;
    var no_cache: bool = false;
    var allow_errors: bool = false;
    var watch: bool = false;
    var timings: bool = false;
    var max_threads: ?usize = null;
    var resolve_limits: ResolveLimitArgs = .{};
    var app_args = try std.array_list.Managed([]const u8).initCapacity(alloc, 16);
    var past_double_dash = false;

    for (args) |arg| {
        // After "--", all remaining args go to the app (no flag processing)
        if (past_double_dash) {
            try app_args.append(arg);
            continue;
        }

        // Check for "--" separator
        if (mem.eql(u8, arg, "--")) {
            past_double_dash = true;
            continue;
        }

        if (isHelpFlag(arg)) {
            // We need to free the paths here because we aren't returning the .run variant
            app_args.deinit();
            return CliArgs{ .help = main_help };
        } else if (mem.startsWith(u8, arg, "--max-package-mb") or mem.startsWith(u8, arg, "--max-transitive-mb")) {
            switch (parseResolveLimitFlag(arg, &resolve_limits)) {
                .problem => |problem| return CliArgs{ .problem = problem },
                else => {},
            }
        } else if (mem.eql(u8, arg, "-v") or mem.eql(u8, arg, "--version")) {
            // We need to free the paths here because we aren't returning the .format variant
            app_args.deinit();
            return CliArgs.version;
        } else if (mem.startsWith(u8, arg, "--target")) {
            if (getFlagValue(arg)) |value| {
                target = value;
            } else {
                app_args.deinit();
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--target" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    app_args.deinit();
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "dev,interpreter,speed,size" } } };
                }
            } else {
                app_args.deinit();
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--allow-errors")) {
            allow_errors = true;
        } else if (mem.eql(u8, arg, "--watch")) {
            watch = true;
        } else if (mem.eql(u8, arg, "--timings")) {
            timings = true;
        } else if (mem.startsWith(u8, arg, "--jobs")) {
            if (getFlagValue(arg)) |value| {
                max_threads = std.fmt.parseInt(usize, value, 10) catch {
                    app_args.deinit();
                    return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "--jobs", .value = value, .valid_options = "positive integer" } } };
                };
            } else {
                app_args.deinit();
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "--jobs" } } };
            }
        } else if (mem.startsWith(u8, arg, "-j")) {
            // Handle -jN format (e.g., -j4)
            const value = arg[2..];
            if (value.len == 0) {
                app_args.deinit();
                return CliArgs{ .problem = ArgProblem{ .missing_flag_value = .{ .flag = "-j" } } };
            }
            max_threads = std.fmt.parseInt(usize, value, 10) catch {
                app_args.deinit();
                return CliArgs{ .problem = ArgProblem{ .invalid_flag_value = .{ .flag = "-j", .value = value, .valid_options = "positive integer" } } };
            };
        } else {
            if (path != null) {
                try app_args.append(arg);
            } else {
                path = arg;
            }
        }
    }
    return CliArgs{ .run = RunArgs{ .path = path orelse "main.roc", .opt = opt, .target = target, .app_args = try app_args.toOwnedSlice(), .no_cache = no_cache, .allow_errors = allow_errors, .watch = watch, .timings = timings, .max_threads = max_threads, .resolve_limits = resolve_limits } };
}

fn isHelpFlag(arg: []const u8) bool {
    return mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help");
}

fn getFlagValue(arg: []const u8) ?[]const u8 {
    var iter = mem.splitScalar(u8, arg, '=');
    // ignore the flag key
    _ = iter.next();
    return iter.next();
}

test "default roc command" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.run.path);
        try testing.expectEqual(.dev, result.run.opt);
        try testing.expectEqualSlices([]const u8, &[_][]const u8{}, result.run.app_args);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "apparg1", "apparg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqualStrings("apparg1", result.run.app_args[0]);
        try testing.expectEqualStrings("apparg2", result.run.app_args[1]);
        try testing.expectEqual(false, result.run.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(true, result.run.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expect(result.run.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"foo.roc"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expect(!result.run.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--opt=speed", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expect(!result.run.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"-v"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "ignored.roc", "--version" });
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"-h"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "ignored.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(.speed, result.run.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--opt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"--opt=notreal"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    // Test -- separator: args after -- should go to app_args
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "arg1", "arg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(@as(usize, 2), result.run.app_args.len);
        try testing.expectEqualStrings("arg1", result.run.app_args[0]);
        try testing.expectEqualStrings("arg2", result.run.app_args[1]);
    }
    // Test -- separator is not included in app_args
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "onlyarg" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(usize, 1), result.run.app_args.len);
        try testing.expectEqualStrings("onlyarg", result.run.app_args[0]);
    }
    // Test flags after -- are treated as app args, not roc flags
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--", "--help", "-v", "--version" });
        defer result.deinit(gpa);
        try testing.expectEqual(.run, std.meta.activeTag(result));
        try testing.expectEqual(@as(usize, 3), result.run.app_args.len);
        try testing.expectEqualStrings("--help", result.run.app_args[0]);
        try testing.expectEqualStrings("-v", result.run.app_args[1]);
        try testing.expectEqualStrings("--version", result.run.app_args[2]);
    }
    // Test -- with flags before it still parses roc flags
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "--opt=speed", "foo.roc", "--", "arg1" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(.speed, result.run.opt);
        try testing.expectEqual(@as(usize, 1), result.run.app_args.len);
        try testing.expectEqualStrings("arg1", result.run.app_args[0]);
    }
    // Test -- without any args after it
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "foo.roc", "--" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(@as(usize, 0), result.run.app_args.len);
    }
}

test "roc build" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"build"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(.speed, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expectEqual(.speed, result.build.opt);
        try testing.expectEqual(false, result.build.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expectEqual(true, result.build.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"build"});
        defer result.deinit(gpa);
        try testing.expect(!result.build.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expect(result.build.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--watch-inputs-file=/tmp/roc-watch-inputs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("/tmp/roc-watch-inputs", result.build.watch_inputs_file.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=size" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.size, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=dev" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.dev, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=interpreter" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.interpreter, result.build.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo/bar.roc", result.build.path);
        try testing.expectEqual(OptLevel.speed, result.build.opt);
        try testing.expectEqualStrings("mypath", result.build.output.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--opt=invalid" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.invalid_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        // Test --debug flag
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--debug", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
        try testing.expect(result.build.debug);
    }
    {
        // Test that debug defaults to false
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expect(!result.build.debug);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "foo.roc", "--opt=size", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "build", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.build.path);
    }
}

test "roc fmt" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"fmt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(!result.fmt.stdin);
        try testing.expect(!result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--check" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(!result.fmt.stdin);
        try testing.expect(result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--stdin" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(result.fmt.stdin);
        try testing.expect(!result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--stdin", "--check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.fmt.paths[0]);
        try testing.expect(result.fmt.stdin);
        try testing.expect(result.fmt.check);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.fmt.paths[0]);
        try testing.expectEqualStrings("bar.roc", result.fmt.paths[1]);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "fmt", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.fmt.paths[0]);
    }
}

test "roc test" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"test"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.test_cmd.path);
        try testing.expectEqual(null, result.test_cmd.main);
        try testing.expectEqual(.dev, result.test_cmd.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
        try testing.expectEqual(.speed, result.test_cmd.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
        try testing.expect(result.test_cmd.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--watch-inputs-file=/tmp/roc-watch-inputs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("/tmp/roc-watch-inputs", result.test_cmd.watch_inputs_file.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "test", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc check" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(null, result.check.main);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqual(false, result.check.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--timings", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqual(true, result.check.timings);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--watch", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expect(result.check.watch);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--watch-inputs-file=/tmp/roc-watch-inputs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("/tmp/roc-watch-inputs", result.check.watch_inputs_file.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("mymain.roc", result.check.main.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--time" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(true, result.check.time);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "foo.roc", "--time", "--main=bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("bar.roc", result.check.main.?);
        try testing.expectEqual(true, result.check.time);
    }
    // --jobs flag tests
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-j1" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 1), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-j4" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 4), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs=2" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 2), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs=8" });
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, 8), result.check.max_threads);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs=abc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--jobs", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("abc", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-jabc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("-j", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("abc", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "--jobs" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--jobs", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "check", "-j" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("-j", result.problem.missing_flag_value.flag);
    }
    {
        // default is null (auto-detect)
        const result = try parse(gpa, testing.io, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqual(@as(?usize, null), result.check.max_threads);
    }
}

test "roc repl" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"repl"});
        defer result.deinit(gpa);
        try testing.expectEqual(.repl, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "repl", "--no-color" });
        defer result.deinit(gpa);
        try testing.expectEqual(.repl, std.meta.activeTag(result));
        try testing.expect(result.repl.no_color);
    }
}

test "roc glue" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("Glue.roc", result.glue.glue_spec);
        try testing.expectEqualStrings("glue-out", result.glue.output_dir);
        try testing.expectEqualStrings("main.roc", result.glue.platform_path);
        try testing.expectEqual(OptLevel.dev, result.glue.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "Glue.roc", "glue-out", "platform/main.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("platform/main.roc", result.glue.platform_path);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "--opt=interpreter", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("Glue.roc", result.glue.glue_spec);
        try testing.expectEqualStrings("glue-out", result.glue.output_dir);
        try testing.expectEqual(OptLevel.interpreter, result.glue.opt);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "--opt=size", "Glue.roc", "glue-out" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.invalid_flag_value.flag);
        try testing.expectEqualStrings("size", result.problem.invalid_flag_value.value);
        try testing.expectEqualStrings("dev,interpreter", result.problem.invalid_flag_value.valid_options);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "glue", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc experimental-lsp" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"experimental-lsp"});
        defer result.deinit(gpa);
        try testing.expectEqual(.experimental_lsp, std.meta.activeTag(result));
        try testing.expect(!result.experimental_lsp.debug_io);
    }
    {
        // LSP clients (e.g. vscode-languageclient) append `--stdio`; it must be
        // accepted as a no-op since stdio is the only transport we support.
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "--stdio" });
        defer result.deinit(gpa);
        try testing.expectEqual(.experimental_lsp, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "--debug-transport", "--stdio" });
        defer result.deinit(gpa);
        try testing.expectEqual(.experimental_lsp, std.meta.activeTag(result));
        try testing.expect(result.experimental_lsp.debug_io);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "--bogus" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("experimental-lsp", result.problem.unexpected_argument.cmd);
        try testing.expectEqualStrings("--bogus", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "experimental-lsp", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc version" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "version", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "version", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "version", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc docs" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"docs"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqual(null, result.docs.main);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
        try testing.expectEqual(false, result.docs.time);
        try testing.expectEqual(false, result.docs.no_cache);
        try testing.expectEqual(false, result.docs.verbose);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("mymain.roc", result.docs.main.?);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--output=my-docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("my-docs", result.docs.output);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--time" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqual(true, result.docs.time);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc", "--time", "--main=bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("bar.roc", result.docs.main.?);
        try testing.expectEqual(true, result.docs.time);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--no-cache" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.no_cache);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--verbose" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.verbose);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "--with-lang-ref" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.with_lang_ref);
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqual(false, result.docs.with_lang_ref);
    }
}

test "roc help" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "help", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc licenses" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{"licenses"});
        defer result.deinit(gpa);
        try testing.expectEqual(.licenses, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, testing.io, &[_][]const u8{ "licenses", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("extrastuff", result.problem.unexpected_argument.arg);
    }
}

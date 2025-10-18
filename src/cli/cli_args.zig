//! Command line argument parsing for the CLI
const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const collections = @import("collections");

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
    repl,
    version,
    docs: DocsArgs,
    help: []const u8,
    licenses,
    problem: CliProblem,

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
pub const CliProblem = union(enum) {
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
    size, // binary size
    speed, // execution speed
    dev, // speed of compilation

    fn from_str(str: []const u8) ?OptLevel {
        if (mem.eql(u8, str, "speed")) return .speed;
        if (mem.eql(u8, str, "size")) return .size;
        if (mem.eql(u8, str, "dev")) return .dev;
        return null;
    }
};

/// Arguments for the default `roc` command
pub const RunArgs = struct {
    path: []const u8, // the path of the roc file to be executed
    opt: OptLevel = .dev, // the optimization level
    target: ?[]const u8 = null, // the target to compile for (e.g., x64musl, x64glibc)
    app_args: []const []const u8 = &[_][]const u8{}, // any arguments to be passed to roc application being run
    no_cache: bool = false, // bypass the executable cache
};

/// Arguments for `roc check`
pub const CheckArgs = struct {
    path: []const u8, // the path of the roc file to be checked
    main: ?[]const u8, // the path to a roc file with an app header to be used to resolved dependencies
    time: bool = false, // whether to print timing information
    no_cache: bool = false, // disable cache
    verbose: bool = false, // enable verbose output
};

/// Arguments for `roc build`
pub const BuildArgs = struct {
    path: []const u8, // the path to the roc file to be built
    opt: OptLevel, // the optimization level
    target: ?[]const u8 = null, // the target to compile for (e.g., x64musl, x64glibc)
    output: ?[]const u8 = null, // the path where the output binary should be created
    z_bench_tokenize: ?[]const u8 = null, // benchmark tokenizer on a file or directory
    z_bench_parse: ?[]const u8 = null, // benchmark parser on a file or directory
};

/// Arguments for `roc test`
pub const TestArgs = struct {
    path: []const u8, // the path to the file to be tested
    opt: OptLevel, // the optimization level to be used for test execution
    main: ?[]const u8, // the path to a roc file with an app header to be used to resolve dependencies
    verbose: bool = false, // enable verbose output showing individual test results
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
};

/// Parse a list of arguments.
pub fn parse(alloc: mem.Allocator, args: []const []const u8) !CliArgs {
    if (args.len == 0) return try parseRun(alloc, args);

    if (mem.eql(u8, args[0], "check")) return parseCheck(args[1..]);
    if (mem.eql(u8, args[0], "build")) return parseBuild(args[1..]);
    if (mem.eql(u8, args[0], "bundle")) return try parseBundle(alloc, args[1..]);
    if (mem.eql(u8, args[0], "unbundle")) return try parseUnbundle(alloc, args[1..]);
    if (mem.eql(u8, args[0], "fmt")) return try parseFormat(alloc, args[1..]);
    if (mem.eql(u8, args[0], "test")) return parseTest(args[1..]);
    if (mem.eql(u8, args[0], "repl")) return parseRepl(args[1..]);
    if (mem.eql(u8, args[0], "version")) return parseVersion(args[1..]);
    if (mem.eql(u8, args[0], "docs")) return parseDocs(args[1..]);
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
    \\  version          Print the Roc compiler's version
    \\  check            Check the code for problems, but don't build or run it
    \\  docs             Generate documentation for a Roc package or platform
    \\  help             Print this message
    \\  licenses         Prints license info for Roc as well as attributions to other projects used by Roc
    \\
    \\Arguments:
    \\  [ROC_FILE]         The .roc file of an app to run [default: main.roc]
    \\  [ARGS_FOR_APP]...  Arguments to pass into the app being run
    \\                     e.g. `roc run -- arg1 arg2`
    \\Options:
    \\      --opt=<size|speed|dev> Optimize the build process for binary size, execution speed, or compilation speed. Defaults to compilation speed (dev)
    \\      --target=<target>      Target to compile for (e.g., x64musl, x64glibc, arm64musl). Defaults to native target with musl for static linking
    \\
;

fn parseCheck(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var main: ?[]const u8 = null;
    var time: bool = false;
    var no_cache: bool = false;
    var verbose: bool = false;

    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help = 
            \\Check the code for problems, but don’t build or run it
            \\
            \\Usage: roc check [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE]  The .roc file to check [default: main.roc]
            \\
            \\Options:
            \\      --main=<main>  The .roc file of the main app/package module to resolve dependencies from
            \\      --time         Print timing information for each compilation phase. Will not print anything if everything is cached.
            \\      --no-cache     Disable caching
            \\      --verbose      Enable verbose output including cache statistics
            \\  -h, --help         Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (getFlagValue(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.eql(u8, arg, "--time")) {
            time = true;
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "check", .arg = arg } } };
            }
            path = arg;
        }
    }

    return CliArgs{ .check = CheckArgs{ .path = path orelse "main.roc", .main = main, .time = time, .no_cache = no_cache, .verbose = verbose } };
}

fn parseBuild(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var target: ?[]const u8 = null;
    var output: ?[]const u8 = null;
    var z_bench_tokenize: ?[]const u8 = null;
    var z_bench_parse: ?[]const u8 = null;
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
            \\      --opt=<size|speed|dev>         Optimize the build process for binary size, execution speed, or compilation speed. Defaults to compilation speed (dev)
            \\      --target=<target>              Target to compile for (e.g., x64musl, x64glibc, arm64musl). Defaults to native target with musl for static linking
            \\      --z-bench-tokenize=<path>      Benchmark tokenizer on a file or directory
            \\      --z-bench-parse=<path>         Benchmark parser on a file or directory
            \\      -h, --help                     Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--target")) {
            if (getFlagValue(arg)) |value| {
                target = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--target" } } };
            }
        } else if (mem.startsWith(u8, arg, "--output")) {
            if (getFlagValue(arg)) |value| {
                output = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--output" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "speed,size,dev" } } };
                }
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else if (mem.startsWith(u8, arg, "--z-bench-tokenize")) {
            if (getFlagValue(arg)) |value| {
                z_bench_tokenize = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--z-bench-tokenize" } } };
            }
        } else if (mem.startsWith(u8, arg, "--z-bench-parse")) {
            if (getFlagValue(arg)) |value| {
                z_bench_parse = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--z-bench-parse" } } };
            }
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "build", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .build = BuildArgs{ .path = path orelse "main.roc", .opt = opt, .target = target, .output = output, .z_bench_tokenize = z_bench_tokenize, .z_bench_parse = z_bench_parse } };
}

fn parseBundle(alloc: mem.Allocator, args: []const []const u8) std.mem.Allocator.Error!CliArgs {
    var paths = try std.ArrayList([]const u8).initCapacity(alloc, 16);
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
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--output-dir" } } };
            }
            i += 1;
            output_dir = args[i];
        } else if (mem.eql(u8, arg, "--compression")) {
            if (i + 1 >= args.len) {
                paths.deinit();
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--compression" } } };
            }
            i += 1;
            compression_level = std.fmt.parseInt(i32, args[i], 10) catch {
                paths.deinit();
                return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .value = args[i], .flag = "--compression", .valid_options = "integer between 1 and 22" } } };
            };
            if (compression_level < 1 or compression_level > 22) {
                paths.deinit();
                return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .value = args[i], .flag = "--compression", .valid_options = "integer between 1 and 22" } } };
            }
        } else if (mem.startsWith(u8, arg, "--")) {
            paths.deinit();
            return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "bundle", .arg = arg } } };
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

fn parseUnbundle(alloc: mem.Allocator, args: []const []const u8) !CliArgs {
    var paths = try std.ArrayList([]const u8).initCapacity(alloc, 16);

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
            return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "unbundle", .arg = arg } } };
        } else {
            try paths.append(arg);
        }
    }

    // If no paths specified, default to all .tar.zst files in current directory
    if (paths.items.len == 0) {
        var cwd = try std.fs.cwd().openDir(".", .{ .iterate = true });
        defer cwd.close();
        var iter = cwd.iterate();
        while (try iter.next()) |entry| {
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
    var paths = try std.ArrayList([]const u8).initCapacity(alloc, 16);
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
            \\      --opt=<size|speed|dev>  Optimize the build process for binary size, execution speed, or compilation speed. Defaults to compilation speed dev
            \\      --main <main>           The .roc file of the main app/package module to resolve dependencies from
            \\      --verbose               Enable verbose output showing individual test results
            \\  -h, --help                  Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (getFlagValue(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "speed,size,dev" } } };
                }
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "test", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .test_cmd = TestArgs{ .path = path orelse "main.roc", .opt = opt, .main = main, .verbose = verbose } };
}

fn parseRepl(args: []const []const u8) CliArgs {
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            return CliArgs{ .help = 
            \\Launch the interactive Read Eval Print Loop (REPL)
            \\
            \\Usage: roc repl [OPTIONS]
            \\
            \\Options:
            \\  -h, --help  Print help
            \\
        };
        } else {
            return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "repl", .arg = arg } } };
        }
    }
    return CliArgs.repl;
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
            return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "version", .arg = arg } } };
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
            return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "licenses", .arg = arg } } };
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
            \\      --time           Print timing information for each compilation phase. Will not print anything if everything is cached.
            \\      --no-cache       Disable caching
            \\      --verbose        Enable verbose output including cache statistics
            \\  -h, --help           Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (getFlagValue(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.startsWith(u8, arg, "--output")) {
            if (getFlagValue(arg)) |value| {
                output = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--output" } } };
            }
        } else if (mem.eql(u8, arg, "--serve")) {
            serve = true;
        } else if (mem.eql(u8, arg, "--time")) {
            time = true;
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else if (mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "docs", .arg = arg } } };
            }
            path = arg;
        }
    }

    return CliArgs{ .docs = DocsArgs{ .path = path orelse "main.roc", .main = main, .output = output orelse "generated-docs", .time = time, .no_cache = no_cache, .verbose = verbose, .serve = serve } };
}

fn parseRun(alloc: mem.Allocator, args: []const []const u8) std.mem.Allocator.Error!CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var target: ?[]const u8 = null;
    var no_cache: bool = false;
    var app_args = try std.ArrayList([]const u8).initCapacity(alloc, 16);
    for (args) |arg| {
        if (isHelpFlag(arg)) {
            // We need to free the paths here because we aren't returning the .run variant
            app_args.deinit();
            return CliArgs{ .help = main_help };
        } else if (mem.eql(u8, arg, "-v") or mem.eql(u8, arg, "--version")) {
            // We need to free the paths here because we aren't returning the .format variant
            app_args.deinit();
            return CliArgs.version;
        } else if (mem.startsWith(u8, arg, "--target")) {
            if (getFlagValue(arg)) |value| {
                target = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--target" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (getFlagValue(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "speed,size,dev" } } };
                }
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else if (mem.eql(u8, arg, "--no-cache")) {
            no_cache = true;
        } else {
            if (path != null) {
                try app_args.append(arg);
            } else {
                path = arg;
            }
        }
    }
    return CliArgs{ .run = RunArgs{ .path = path orelse "main.roc", .opt = opt, .target = target, .app_args = try app_args.toOwnedSlice(), .no_cache = no_cache } };
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

test "roc run" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.run.path);
        try testing.expectEqual(.dev, result.run.opt);
        try testing.expectEqualSlices([]const u8, &[_][]const u8{}, result.run.app_args);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "foo.roc", "apparg1", "apparg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqualStrings("apparg1", result.run.app_args[0]);
        try testing.expectEqualStrings("apparg2", result.run.app_args[1]);
    }
    {
        const result = try parse(gpa, &[_][]const u8{"-v"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{"--version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "ignored.roc", "--version" });
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{"-h"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{"--help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "ignored.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(.speed, result.run.opt);
    }
    {
        const result = try parse(gpa, &[_][]const u8{"--opt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, &[_][]const u8{"--opt=notreal"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
}

test "roc build" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"build"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(.dev, result.build.opt);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--opt=size" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.size, result.build.opt);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--opt=dev" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.dev, result.build.opt);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo/bar.roc", result.build.path);
        try testing.expectEqual(OptLevel.speed, result.build.opt);
        try testing.expectEqualStrings("mypath", result.build.output.?);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--opt=invalid" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.invalid_flag_value.flag);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "foo.roc", "--opt=size", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "build", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.build.path);
    }
}

test "roc fmt" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"fmt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(!result.fmt.stdin);
        try testing.expect(!result.fmt.check);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "--check" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(!result.fmt.stdin);
        try testing.expect(result.fmt.check);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "--stdin" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.fmt.paths[0]);
        try testing.expect(result.fmt.stdin);
        try testing.expect(!result.fmt.check);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "--stdin", "--check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.fmt.paths[0]);
        try testing.expect(result.fmt.stdin);
        try testing.expect(result.fmt.check);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.fmt.paths[0]);
        try testing.expectEqualStrings("bar.roc", result.fmt.paths[1]);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "fmt", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.fmt.paths[0]);
    }
}

test "roc test" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"test"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.test_cmd.path);
        try testing.expectEqual(null, result.test_cmd.main);
        try testing.expectEqual(.dev, result.test_cmd.opt);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
        try testing.expectEqual(.speed, result.test_cmd.opt);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "test", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc check" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(null, result.check.main);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("mymain.roc", result.check.main.?);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "--time" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(true, result.check.time);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "check", "foo.roc", "--time", "--main=bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("bar.roc", result.check.main.?);
        try testing.expectEqual(true, result.check.time);
    }
}

test "roc repl" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"repl"});
        defer result.deinit(gpa);
        try testing.expectEqual(.repl, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "repl", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "repl", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "repl", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc version" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "version", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "version", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "version", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc docs" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"docs"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqual(null, result.docs.main);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
        try testing.expectEqual(false, result.docs.time);
        try testing.expectEqual(false, result.docs.no_cache);
        try testing.expectEqual(false, result.docs.verbose);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("mymain.roc", result.docs.main.?);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "--output=my-docs", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("my-docs", result.docs.output);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "--time" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqual(true, result.docs.time);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "foo.roc", "--time", "--main=bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.docs.path);
        try testing.expectEqualStrings("bar.roc", result.docs.main.?);
        try testing.expectEqual(true, result.docs.time);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "--no-cache" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.no_cache);
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "docs", "--verbose" });
        defer result.deinit(gpa);
        try testing.expectEqual(true, result.docs.verbose);
    }
}

test "roc help" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "help", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}
test "roc licenses" {
    const gpa = testing.allocator;
    {
        const result = try parse(gpa, &[_][]const u8{"licenses"});
        defer result.deinit(gpa);
        try testing.expectEqual(.licenses, std.meta.activeTag(result));
    }
    {
        const result = try parse(gpa, &[_][]const u8{ "licenses", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("extrastuff", result.problem.unexpected_argument.arg);
    }
}

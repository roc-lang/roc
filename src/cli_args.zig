//! Command line argument parsing for the CLI
const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const collections = @import("collections.zig");
const exitOnOom = collections.utils.exitOnOom;

/// The core type representing a parsed command
/// We could use anonymous structs for the argument types instead of defining one for each command to be more concise,
/// but defining a struct per command means that we can easily take that type and pass it into the function that implements each command.
pub const CliArgs = union(enum) {
    run: RunArgs,
    check: CheckArgs,
    build: BuildArgs,
    format: FormatArgs,
    test_cmd: TestArgs,
    repl,
    version,
    docs: DocsArgs,
    help: []const u8,
    licenses,
    problem: CliProblem,

    pub fn deinit(self: CliArgs, gpa: mem.Allocator) void {
        switch (self) {
            .format => |fmt| gpa.free(fmt.paths),
            .run => |run| gpa.free(run.app_args),
            else => return,
        }
    }
};

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

pub const OptLevel = enum {
    size,
    speed,
    dev,

    fn from_str(str: []const u8) ?OptLevel {
        if (mem.eql(u8, str, "speed")) return .speed;
        if (mem.eql(u8, str, "size")) return .size;
        if (mem.eql(u8, str, "dev")) return .dev;
        return null;
    }
};

pub const RunArgs = struct {
    path: []const u8,
    opt: OptLevel = .dev,
    app_args: []const []const u8 = &[_][]const u8{},
};

pub const CheckArgs = struct {
    path: []const u8,
    main: ?[]const u8,
};

pub const BuildArgs = struct {
    path: []const u8,
    opt: OptLevel,
    output: ?[]const u8 = null,
};

pub const TestArgs = struct { path: []const u8, opt: OptLevel, main: ?[]const u8 };

pub const FormatArgs = struct {
    paths: []const []const u8,
    stdin: bool = false,
    check: bool = false,
};

pub const DocsArgs = struct {
    path: []const u8,
    output: []const u8,
    root_dir: ?[]const u8 = null,
};

/// Parse a list of arguments.
pub fn parse(gpa: mem.Allocator, args: []const []const u8) CliArgs {
    if (args.len == 0) return parse_run(gpa, args);

    if (mem.eql(u8, args[0], "check")) return parse_check(args[1..]);
    if (mem.eql(u8, args[0], "build")) return parse_build(args[1..]);
    if (mem.eql(u8, args[0], "format")) return parse_format(gpa, args[1..]);
    if (mem.eql(u8, args[0], "test")) return parse_test(args[1..]);
    if (mem.eql(u8, args[0], "repl")) return parse_repl(args[1..]);
    if (mem.eql(u8, args[0], "version")) return parse_version(args[1..]);
    if (mem.eql(u8, args[0], "docs")) return parse_docs(args[1..]);
    if (mem.eql(u8, args[0], "help")) return CliArgs{ .help = main_help };
    if (mem.eql(u8, args[0], "licenses")) return parse_licenses(args[1..]);

    return parse_run(gpa, args);
}

const main_help =
    \\Run the given .roc file
    \\You can use one of the SUBCOMMANDS below to do something else!
    \\
    \\Usage: roc [OPTIONS] [ROC_FILE] [ARGS_FOR_APP]...
    \\       roc <COMMAND>
    \\
    \\Commands:
    \\  build            Build a binary from the given .roc file, but don't run it
    \\  test             Run all top-level `expect`s in a main module and any modules it imports
    \\  repl             Launch the interactive Read Eval Print Loop (REPL)
    \\  format           Format a .roc file or the .roc files contained in a directory using standard Roc formatting
    \\  version          Print the Roc compiler’s version
    \\  check            Check the code for problems, but don’t build or run it
    \\  docs             Generate documentation for a Roc package
    \\  help             Print this message
    \\  licenses         Prints license info for Roc as well as attributions to other projects used by Roc
    \\
    \\Arguments:
    \\  [ROC_FILE]         The .roc file of an app to run [default: main.roc]
    \\  [ARGS_FOR_APP]...  Arguments to pass into the app being run
    \\                     e.g. `roc run -- arg1 arg2`
    \\Options:
    \\      --opt=<size|speed|dev> Optimize the build process for binary size, binary speed, or compilation speed. Defaults to compilation speed (dev)
    \\
;

fn parse_check(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var main: ?[]const u8 = null;
    for (args) |arg| {
        if (is_help_flag(arg)) {
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
            \\  -h, --help         Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (get_flag_value(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "check", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .check = CheckArgs{ .path = path orelse "main.roc", .main = main } };
}

fn parse_build(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var output: ?[]const u8 = null;
    for (args) |arg| {
        if (is_help_flag(arg)) {
            return CliArgs{ .help = 
            \\Build a binary from the given .roc file, but don't run it
            \\
            \\Usage: roc build [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE] The .roc file to build [default: main.roc]
            \\
            \\Options:
            \\      --output=<output>       The full path to the output binary, including filename. To specify directory only, specify a path that ends in a directory separator (e.g. a slash)
            \\      --opt=<size|speed|dev>  Optimize the build process for binary size, binary speed, or compilation speed. Defaults to compilation speed (dev)
            \\      -h, --help              Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--output")) {
            if (get_flag_value(arg)) |value| {
                output = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--output" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (get_flag_value(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "speed,size,dev" } } };
                }
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "build", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .build = BuildArgs{ .path = path orelse "main.roc", .opt = opt, .output = output } };
}

fn parse_format(gpa: mem.Allocator, args: []const []const u8) CliArgs {
    var paths = std.ArrayList([]const u8).init(gpa);
    var stdin = false;
    var check = false;
    for (args) |arg| {
        if (is_help_flag(arg)) {
            // We need to free the paths here becuase we aren't returning the .format variant
            paths.deinit();
            return CliArgs{ .help = 
            \\Format a .roc file or the .roc files contained in a directory using standard Roc formatting
            \\
            \\Usage: roc format [OPTIONS] [DIRECTORY_OR_FILES]
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
            paths.append(arg) catch |err| exitOnOom(err);
        }
    }
    if (paths.items.len == 0) {
        paths.append("main.roc") catch |err| exitOnOom(err);
    }
    return CliArgs{ .format = FormatArgs{ .paths = paths.toOwnedSlice() catch |err| exitOnOom(err), .stdin = stdin, .check = check } };
}

fn parse_test(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var main: ?[]const u8 = null;
    for (args) |arg| {
        if (is_help_flag(arg)) {
            return CliArgs{ .help = 
            \\Run all top-level `expect`s in a main module and any modules it imports
            \\
            \\Usage: roc test [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE] The .roc file to test [default: main.roc]
            \\
            \\Options:
            \\      --opt=<size|speed|dev>  Optimize the build process for binary size, binary speed, or compilation speed. Defaults to compilation speed dev
            \\      --main <main>           The .roc file of the main app/package module to resolve dependencies from
            \\  -h, --help                  Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--main")) {
            if (get_flag_value(arg)) |value| {
                main = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--main" } } };
            }
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (get_flag_value(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "speed,size,dev" } } };
                }
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "test", .arg = arg } } };
            }
            path = arg;
        }
    }
    return CliArgs{ .test_cmd = TestArgs{ .path = path orelse "main.roc", .opt = opt, .main = main } };
}

fn parse_repl(args: []const []const u8) CliArgs {
    for (args) |arg| {
        if (is_help_flag(arg)) {
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

fn parse_version(args: []const []const u8) CliArgs {
    for (args) |arg| {
        if (is_help_flag(arg)) {
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

fn parse_licenses(args: []const []const u8) CliArgs {
    for (args) |arg| {
        if (is_help_flag(arg)) {
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

fn parse_docs(args: []const []const u8) CliArgs {
    var output: ?[]const u8 = null;
    var root_dir: ?[]const u8 = null;
    var path: ?[]const u8 = null;
    for (args) |arg| {
        if (is_help_flag(arg)) {
            return CliArgs{ .help = 
            \\Generate documentation for a Roc package
            \\
            \\Usage: roc docs [OPTIONS] [ROC_FILE]
            \\
            \\Arguments:
            \\  [ROC_FILE]  The package's main .roc file [default: main.roc]
            \\
            \\Options:
            \\      --output=<output>      Output directory for the generated documentation files. [default: generated-docs]
            \\      --root-dir=<root-dir>  Set a root directory path to be used as a prefix for URL links in the generated documentation files.
            \\  -h, --help                 Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--output")) {
            if (get_flag_value(arg)) |value| {
                output = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--output" } } };
            }
        } else if (mem.startsWith(u8, arg, "--root-dir")) {
            if (get_flag_value(arg)) |value| {
                root_dir = value;
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--root-dir" } } };
            }
        } else {
            if (path != null) {
                return CliArgs{ .problem = CliProblem{ .unexpected_argument = .{ .cmd = "docs", .arg = arg } } };
            }
            path = arg;
        }
    }

    return CliArgs{ .docs = DocsArgs{ .path = path orelse "main.roc", .output = output orelse "generated-docs", .root_dir = root_dir } };
}

fn parse_run(gpa: mem.Allocator, args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .dev;
    var app_args = std.ArrayList([]const u8).init(gpa);
    for (args) |arg| {
        if (is_help_flag(arg)) {
            // We need to free the paths here becuase we aren't returning the .run variant
            app_args.deinit();
            return CliArgs{ .help = main_help };
        } else if (mem.eql(u8, arg, "-v") or mem.eql(u8, arg, "--version")) {
            // We need to free the paths here becuase we aren't returning the .format variant
            app_args.deinit();
            return CliArgs.version;
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (get_flag_value(arg)) |value| {
                if (OptLevel.from_str(value)) |level| {
                    opt = level;
                } else {
                    return CliArgs{ .problem = CliProblem{ .invalid_flag_value = .{ .flag = "--opt", .value = value, .valid_options = "speed,size,dev" } } };
                }
            } else {
                return CliArgs{ .problem = CliProblem{ .missing_flag_value = .{ .flag = "--opt" } } };
            }
        } else {
            if (path != null) {
                app_args.append(arg) catch |err| exitOnOom(err);
            } else {
                path = arg;
            }
        }
    }
    return CliArgs{ .run = RunArgs{ .path = path orelse "main.roc", .opt = opt, .app_args = app_args.toOwnedSlice() catch |err| exitOnOom(err) } };
}

fn is_help_flag(arg: []const u8) bool {
    return mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help");
}

fn get_flag_value(arg: []const u8) ?[]const u8 {
    var iter = mem.splitScalar(u8, arg, '=');
    // ignore the flag key
    _ = iter.next();
    return iter.next();
}

test "roc run" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.run.path);
        try testing.expectEqual(.dev, result.run.opt);
        try testing.expectEqualSlices([]const u8, &[_][]const u8{}, result.run.app_args);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "foo.roc", "apparg1", "apparg2" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqualStrings("apparg1", result.run.app_args[0]);
        try testing.expectEqualStrings("apparg2", result.run.app_args[1]);
    }
    {
        const result = parse(gpa, &[_][]const u8{"-v"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{"--version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "ignored.roc", "--version" });
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{"-h"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{"--help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "ignored.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.run.path);
        try testing.expectEqual(.speed, result.run.opt);
    }
    {
        const result = parse(gpa, &[_][]const u8{"--opt"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = parse(gpa, &[_][]const u8{"--opt=notreal"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
}

test "roc build" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"build"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(.dev, result.build.opt);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.build.path);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=size" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.size, result.build.opt);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=dev" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.dev, result.build.opt);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo/bar.roc", result.build.path);
        try testing.expectEqual(OptLevel.speed, result.build.opt);
        try testing.expectEqualStrings("mypath", result.build.output.?);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=invalid" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.invalid_flag_value.flag);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "foo.roc", "--opt=size", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.build.path);
    }
}

test "roc format" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"format"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.format.paths[0]);
        try testing.expect(!result.format.stdin);
        try testing.expect(!result.format.check);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "--check" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.format.paths[0]);
        try testing.expect(!result.format.stdin);
        try testing.expect(result.format.check);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "--stdin" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.format.paths[0]);
        try testing.expect(result.format.stdin);
        try testing.expect(!result.format.check);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "--stdin", "--check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.format.paths[0]);
        try testing.expect(result.format.stdin);
        try testing.expect(result.format.check);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.format.paths[0]);
        try testing.expectEqualStrings("bar.roc", result.format.paths[1]);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "format", "--thisisactuallyafile" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--thisisactuallyafile", result.format.paths[0]);
    }
}

test "roc test" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"test"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.test_cmd.path);
        try testing.expectEqual(null, result.test_cmd.main);
        try testing.expectEqual(.dev, result.test_cmd.opt);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "foo.roc", "--opt=speed" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.test_cmd.path);
        try testing.expectEqual(.speed, result.test_cmd.opt);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "--opt" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt", result.problem.missing_flag_value.flag);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "--opt=notreal" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("notreal", result.problem.invalid_flag_value.value);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "test", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc check" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
        try testing.expectEqual(null, result.check.main);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "--main=mymain.roc", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.check.path);
        try testing.expectEqualStrings("mymain.roc", result.check.main.?);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("bar.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc repl" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"repl"});
        defer result.deinit(gpa);
        try testing.expectEqual(.repl, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "repl", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "repl", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "repl", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc version" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"version"});
        defer result.deinit(gpa);
        try testing.expectEqual(.version, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "version", "foo.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo.roc", result.problem.unexpected_argument.arg);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "version", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "version", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc docs" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"docs"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.docs.path);
        try testing.expectEqualStrings("generated-docs", result.docs.output);
        try testing.expectEqual(null, result.docs.root_dir);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "docs", "foo/bar.roc", "--root-dir=/root/dir", "--output=my_output_dir" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo/bar.roc", result.docs.path);
        try testing.expectEqualStrings("my_output_dir", result.docs.output);
        try testing.expectEqualStrings("/root/dir", result.docs.root_dir.?);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "docs", "foo.roc", "--madeup" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--madeup", result.problem.unexpected_argument.arg);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "docs", "-h" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "docs", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "docs", "foo.roc", "--help" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}

test "roc help" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"help"});
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "help", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
}
test "roc licenses" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"licenses"});
        defer result.deinit(gpa);
        try testing.expectEqual(.licenses, std.meta.activeTag(result));
    }
    {
        const result = parse(gpa, &[_][]const u8{ "licenses", "extrastuff" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("extrastuff", result.problem.unexpected_argument.arg);
    }
}

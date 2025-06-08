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
    invalid: []const u8, // TODO: improve the error messages

    pub fn deinit(self: CliArgs, gpa: mem.Allocator) void {
        switch (self) {
            .format => |fmt| gpa.free(fmt.paths),
            .run => |run| gpa.free(run.app_args),
            else => return,
        }
    }
};

pub const OptLevel = enum {
    size,
    speed,
    dev,
};

pub const RunArgs = struct {
    path: []const u8,
    opt: OptLevel = .dev,
    app_args: []const []const u8 = &[_][]const u8{},
};

pub const CheckArgs = struct {
    path: []const u8,
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
    if (mem.eql(u8, args[0], "licenses")) return CliArgs.licenses;

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
    if (args.len == 0) return CliArgs{ .check = CheckArgs{ .path = "main.roc" } };
    return CliArgs{ .check = CheckArgs{ .path = args[0] } };
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
            \\      --output=<output>      The full path to the output binary, including filename. To specify directory only, specify a path that ends in a directory separator (e.g. a slash)
            \\      --opt=<size|speed|dev> Optimize the build process for binary size, binary speed, or compilation speed. Defaults to compilation speed (dev)
            \\      -h, --help             Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--output")) {
            var iter = mem.splitScalar(u8, arg, '=');
            _ = iter.next();
            const value = iter.next().?;
            output = value;
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (parse_opt_level(arg)) |level| {
                opt = level;
            } else {
                return CliArgs{ .invalid = "--opt can be either speed or size" };
            }
        } else {
            if (path != null) {
                return CliArgs{ .invalid = "unexpected argument" };
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
            \\      --check    Checks that specified files are formatted
            \\                 (If formatting is needed, return a non-zero exit code.)
            \\      --stdin    Format code from stdin; output to stdout
            \\  -h, --help     Print help
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
            \\      --opt=<size|speed|dev> Optimize the build process for binary size, binary speed, or compilation speed. Defaults to compilation speed dev
            \\      --main <main>          The .roc file of the main app/package module to resolve dependencies from
            \\  -h, --help                 Print help
            \\
        };
        } else if (mem.startsWith(u8, arg, "--main")) {
            var iter = mem.splitScalar(u8, arg, '=');
            _ = iter.next();
            main = iter.next().?;
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (parse_opt_level(arg)) |level| {
                opt = level;
            } else {
                return CliArgs{ .invalid = "--opt can be either speed or size" };
            }
        } else {
            if (path != null) {
                return CliArgs{ .invalid = "unexpected argument" };
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
            \\  -h, --help       Print help
            \\
        };
        } else {
            return CliArgs{ .invalid = "unexpected argument" };
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
            return CliArgs{ .invalid = "unexpected argument" };
        }
    }
    return CliArgs.version;
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
            var iter = mem.splitScalar(u8, arg, '=');
            _ = iter.next();
            output = iter.next().?;
        } else if (mem.startsWith(u8, arg, "--root-dir")) {
            var iter = mem.splitScalar(u8, arg, '=');
            _ = iter.next();
            root_dir = iter.next().?;
        } else {
            if (path != null) {
                return CliArgs{ .invalid = "unexpected argument" };
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
            app_args.deinit();
            return CliArgs{ .help = main_help };
        } else if (mem.eql(u8, arg, "-v") or mem.eql(u8, arg, "--version")) {
            app_args.deinit();
            return CliArgs.version;
        } else if (mem.startsWith(u8, arg, "--opt")) {
            if (parse_opt_level(arg)) |level| {
                opt = level;
            } else {
                return CliArgs{ .invalid = "--opt can be either speed or size" };
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

fn parse_opt_level(arg: []const u8) ?OptLevel {
    var iter = mem.splitScalar(u8, arg, '=');
    _ = iter.next();
    const value = iter.next().?;
    if (mem.eql(u8, value, "speed")) return .speed;
    if (mem.eql(u8, value, "size")) return .size;
    if (mem.eql(u8, value, "dev")) return .dev;
    return null;
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
}

test "roc check" {
    const gpa = testing.allocator;
    {
        const result = parse(gpa, &[_][]const u8{"check"});
        defer result.deinit(gpa);
        try testing.expectEqualStrings("main.roc", result.check.path);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "check", "some/file.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("some/file.roc", result.check.path);
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
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("foo/bar.roc", result.build.path);
        try testing.expectEqual(OptLevel.speed, result.build.opt);
        try testing.expectEqualStrings("mypath", result.build.output.?);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "--opt=invalid" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("--opt can be either speed or size", result.invalid);
    }
    {
        const result = parse(gpa, &[_][]const u8{ "build", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("unexpected argument", result.invalid);
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
        const result = parse(gpa, &[_][]const u8{ "test", "foo.roc", "bar.roc" });
        defer result.deinit(gpa);
        try testing.expectEqualStrings("unexpected argument", result.invalid);
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
        try testing.expectEqualStrings("unexpected argument", result.invalid);
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
        try testing.expectEqualStrings("unexpected argument", result.invalid);
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
        try testing.expectEqualStrings("unexpected argument", result.invalid);
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
        try testing.expectEqual(.licenses, std.meta.activeTag(result));
    }
}

//! Command line argument parsing for the CLI
const std = @import("std");
const testing = std.testing;
const mem = std.mem;

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
};

pub const OptLevel = enum {
    none,
    speed,
    size,
};

pub const RunArgs = struct {
    path: []const u8,
    opt: OptLevel = .none,
};

pub const CheckArgs = struct {
    path: []const u8,
};

pub const BuildArgs = struct {
    path: []const u8,
    opt: OptLevel = .none,
    output: ?[]const u8 = null,
};

pub const TestArgs = struct {
    path: []const u8,
    opt: OptLevel = .none,
};

pub const FormatArgs = struct {
    path: []const u8,
    stdin: bool = false,
    check: bool = false,
};

pub const DocsArgs = struct {
    package: ?[]const u8 = null,
    output: ?[]const u8 = null,
};

/// Parse a list of arguments.
// TODO: should we ignore extra arguments or return errors when they are included?
pub fn parse(args: []const []const u8) CliArgs {
    if (args.len == 0) return CliArgs{ .run = RunArgs{ .path = "main.roc" } };
    if (mem.eql(u8, args[0], "check")) return parseCheck(args[1..]);
    if (mem.eql(u8, args[0], "build")) return parseBuild(args[1..]);
    if (mem.eql(u8, args[0], "format")) return parseFormat(args[1..]);
    if (mem.eql(u8, args[0], "test")) return parseTest(args[1..]);

    return parseRun(args[1..]);
}

fn parseCheck(args: []const []const u8) CliArgs {
    if (args.len == 0) return CliArgs{ .check = CheckArgs{ .path = "main.roc" } };
    return CliArgs{ .check = CheckArgs{ .path = args[0] } };
}

fn parseBuild(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var opt: OptLevel = .none;
    var output: ?[]const u8 = null;
    for (args) |arg| {
        if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
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
        };
        } else if (mem.startsWith(u8, arg, "--output")) {
            var iter = mem.splitScalar(u8, arg, '=');
            _ = iter.next();
            const value = iter.next().?;
            output = value;
        } else if (mem.startsWith(u8, arg, "--opt")) {
            var iter = mem.splitScalar(u8, arg, '=');
            _ = iter.next();
            const value = iter.next().?;
            if (mem.eql(u8, value, "size")) {
                opt = .size;
            } else if (mem.eql(u8, value, "speed")) {
                opt = .speed;
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

fn parseFormat(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var stdin = false;
    var check = false;
    for (args) |arg| {
        if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
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
        };
        } else if (mem.eql(u8, arg, "--stdin")) {
            stdin = true;
        } else if (mem.eql(u8, arg, "--check")) {
            check = true;
        } else {
            if (path != null) {
                return CliArgs{ .invalid = "unexpected argument" };
            }
            path = arg;
        }
    }
    return CliArgs{ .format = FormatArgs{ .path = path orelse "main.roc", .stdin = stdin, .check = check } };
}

fn parseTest(args: []const []const u8) CliArgs {
    var path: ?[]const u8 = null;
    var stdin = false;
    var check = false;
    for (args) |arg| {
        if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
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
        };
        } else if (mem.eql(u8, arg, "--stdin")) {
            stdin = true;
        } else if (mem.eql(u8, arg, "--check")) {
            check = true;
        } else {
            if (path != null) {
                return CliArgs{ .invalid = "unexpected argument" };
            }
            path = arg;
        }
    }
    return CliArgs{ .format = FormatArgs{ .path = path orelse "main.roc", .stdin = stdin, .check = check } };
}

fn parseRun(args: []const []const u8) CliArgs {
    _ = args;
    return CliArgs{ .run = RunArgs{ .path = "main.roc" } };
}

test "roc run" {
    {
        const result = parse(&[_][]const u8{});
        try testing.expectEqualStrings("main.roc", result.run.path);
    }
    {
        const result = parse(&[_][]const u8{""});
        try testing.expectEqualStrings("main.roc", result.run.path);
    }
    {
        const result = parse(&[_][]const u8{ "", "", "" });
        try testing.expectEqualStrings("main.roc", result.run.path);
    }
}

test "roc check" {
    {
        const result = parse(&[_][]const u8{"check"});
        try testing.expectEqualStrings("main.roc", result.check.path);
    }
    {
        const result = parse(&[_][]const u8{ "check", "some/file.roc" });
        try testing.expectEqualStrings("some/file.roc", result.check.path);
    }
}

test "roc build" {
    {
        const result = parse(&[_][]const u8{"build"});
        try testing.expectEqualStrings("main.roc", result.build.path);
    }
    {
        const result = parse(&[_][]const u8{ "build", "foo.roc" });
        try testing.expectEqualStrings("foo.roc", result.build.path);
    }
    {
        const result = parse(&[_][]const u8{ "build", "--opt=size" });
        try testing.expectEqualStrings("main.roc", result.build.path);
        try testing.expectEqual(OptLevel.size, result.build.opt);
    }
    {
        const result = parse(&[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        try testing.expectEqualStrings("foo/bar.roc", result.build.path);
        try testing.expectEqual(OptLevel.speed, result.build.opt);
        try testing.expectEqualStrings("mypath", result.build.output.?);
    }
    {
        const result = parse(&[_][]const u8{ "build", "--opt=invalid" });
        try testing.expectEqualStrings("--opt can be either speed or size", result.invalid);
    }
    {
        const result = parse(&[_][]const u8{ "build", "foo.roc", "bar.roc" });
        try testing.expectEqualStrings("unexpected argument", result.invalid);
    }
    {
        const result = parse(&[_][]const u8{ "build", "-h" });
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(&[_][]const u8{ "build", "--help" });
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(&[_][]const u8{ "build", "foo.roc", "--opt=size", "--help" });
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(&[_][]const u8{ "build", "--thisisactuallyafile" });
        try testing.expectEqualStrings("--thisisactuallyafile", result.build.path);
    }
}

test "roc format" {
    {
        const result = parse(&[_][]const u8{"format"});
        try testing.expectEqualStrings("main.roc", result.format.path);
        try testing.expect(!result.format.stdin);
        try testing.expect(!result.format.check);
    }
    {
        const result = parse(&[_][]const u8{ "format", "--check" });
        try testing.expectEqualStrings("main.roc", result.format.path);
        try testing.expect(!result.format.stdin);
        try testing.expect(result.format.check);
    }
    {
        const result = parse(&[_][]const u8{ "format", "--stdin" });
        try testing.expectEqualStrings("main.roc", result.format.path);
        try testing.expect(result.format.stdin);
        try testing.expect(!result.format.check);
    }
    {
        const result = parse(&[_][]const u8{ "format", "--stdin", "--check", "foo.roc" });
        try testing.expectEqualStrings("foo.roc", result.format.path);
        try testing.expect(result.format.stdin);
        try testing.expect(result.format.check);
    }
    {
        const result = parse(&[_][]const u8{ "format", "foo.roc" });
        try testing.expectEqualStrings("foo.roc", result.format.path);
    }
    {
        const result = parse(&[_][]const u8{ "format", "foo.roc", "bar.roc" });
        try testing.expectEqualStrings("unexpected argument", result.invalid);
    }
    {
        const result = parse(&[_][]const u8{ "format", "-h" });
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(&[_][]const u8{ "format", "--help" });
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(&[_][]const u8{ "format", "foo.roc", "--help" });
        try testing.expectEqual(.help, std.meta.activeTag(result));
    }
    {
        const result = parse(&[_][]const u8{ "format", "--thisisactuallyafile" });
        try testing.expectEqualStrings("--thisisactuallyafile", result.format.path);
    }
}

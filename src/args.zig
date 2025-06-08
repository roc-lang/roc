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
    file: []const u8,
    opt: OptLevel = .none,
};

pub const CheckArgs = struct {
    file: []const u8,
};

pub const BuildArgs = struct {
    file: []const u8,
    opt: OptLevel = .none,
    output: ?[]const u8 = null,
};

pub const TestArgs = struct {
    file: []const u8,
    opt: OptLevel = .none,
};

pub const FormatArgs = struct {
    path: ?[]const u8 = null,
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
    if (args.len == 0) return CliArgs{ .run = RunArgs{ .file = "main.roc" } };
    if (mem.eql(u8, args[0], "check")) return parseCheck(args[1..]);
    if (mem.eql(u8, args[0], "build")) return parseBuild(args[1..]);

    return parseRun(args[1..]);
}

fn parseCheck(args: []const []const u8) CliArgs {
    if (args.len == 0) return CliArgs{ .check = CheckArgs{ .file = "main.roc" } };
    return CliArgs{ .check = CheckArgs{ .file = args[0] } };
}

fn parseBuild(args: []const []const u8) CliArgs {
    var file: ?[]const u8 = null;
    var opt: OptLevel = .none;
    var output: ?[]const u8 = null;
    for (args) |arg| {
        if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
            return CliArgs{ .help = 
            \\TODO build help message here
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
            if (file != null) {
                return CliArgs{ .invalid = "unexpected argument" };
            }
            file = arg;
        }
    }
    return CliArgs{ .build = BuildArgs{ .file = file orelse "main.roc", .opt = opt, .output = output } };
}

fn parseRun(args: []const []const u8) CliArgs {
    _ = args;
    return CliArgs{ .run = RunArgs{ .file = "main.roc" } };
}

test "roc run" {
    {
        const result = parse(&[_][]const u8{});
        try testing.expectEqualStrings("main.roc", result.run.file);
    }
    {
        const result = parse(&[_][]const u8{""});
        try testing.expectEqualStrings("main.roc", result.run.file);
    }
    {
        const result = parse(&[_][]const u8{ "", "", "" });
        try testing.expectEqualStrings("main.roc", result.run.file);
    }
}

test "roc check" {
    {
        const result = parse(&[_][]const u8{"check"});
        try testing.expectEqualStrings("main.roc", result.check.file);
    }
    {
        const result = parse(&[_][]const u8{ "check", "some/file.roc" });
        try testing.expectEqualStrings("some/file.roc", result.check.file);
    }
}

test "roc build" {
    {
        const result = parse(&[_][]const u8{"build"});
        try testing.expectEqualStrings("main.roc", result.build.file);
    }
    {
        const result = parse(&[_][]const u8{ "build", "foo.roc" });
        try testing.expectEqualStrings("foo.roc", result.build.file);
    }
    {
        const result = parse(&[_][]const u8{ "build", "--opt=size" });
        try testing.expectEqualStrings("main.roc", result.build.file);
        try testing.expectEqual(OptLevel.size, result.build.opt);
    }
    {
        const result = parse(&[_][]const u8{ "build", "--opt=speed", "foo/bar.roc", "--output=mypath" });
        try testing.expectEqualStrings("foo/bar.roc", result.build.file);
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
        try testing.expectEqualStrings("TODO build help message here", result.help);
    }
    {
        const result = parse(&[_][]const u8{ "build", "--help" });
        try testing.expectEqualStrings("TODO build help message here", result.help);
    }
    {
        const result = parse(&[_][]const u8{ "build", "foo.roc", "--opt=size", "--help" });
        try testing.expectEqualStrings("TODO build help message here", result.help);
    }
}

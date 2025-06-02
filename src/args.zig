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
    invalid: []const u8, // error message
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
    if (args.len == 0) return CliArgs{ .build = BuildArgs{ .file = "main.roc" } };
    // var file = "main.roc";
    // for (args) |arg| {
    //     if
    // }
    return CliArgs{ .build = BuildArgs{ .file = args[0] } };
}

// fn get_flag_name([]const u8) ?[]const u8 {}

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
}

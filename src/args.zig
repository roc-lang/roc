//! Command line argument parsing for the CLI
const std = @import("std");
const testing = std.testing;
const mem = std.mem;

// The core type representing a parsed command
pub const CliArgs = union(enum) {
    run: struct {
        file: []const u8,
        opt: OptLevel = .none,
    },
    check: struct {
        file: []const u8,
    },
    build: struct {
        file: []const u8,
        opt: OptLevel = .none,
        output: ?[]const u8 = null,
    },
    format: struct {
        path: ?[]const u8 = null,
        stdin: bool = false,
        check: bool = false,
    },
    test_cmd: struct {
        file: []const u8,
        opt: OptLevel = .none,
    },
    repl,
    version,
    docs: struct {
        package: ?[]const u8 = null,
        output: ?[]const u8 = null,
    },
    help: []const u8,
    licenses,
    invalid: []const u8, // error message
};

pub const OptLevel = enum {
    none,
    speed,
    size,
};

/// Parse a list of arguments.
// TODO: should we ignore extra arguments or return errors when they are included?
pub fn parse(args: []const []const u8) CliArgs {
    if (args.len == 0) return CliArgs{ .run = .{ .file = "main.roc" } };
    if (mem.eql(u8, args[0], "check")) return parseCheck(args[1..]);
    if (mem.eql(u8, args[0], "build")) return parseBuild(args[1..]);
    return parseRun(args[1..]);
}

fn parseCheck(args: []const []const u8) CliArgs {
    if (args.len == 0) return CliArgs{ .check = .{ .file = "main.roc" } };
    return CliArgs{ .check = .{ .file = args[0] } };
}

fn parseBuild(args: []const []const u8) CliArgs {
    if (args.len == 0) return CliArgs{ .build = .{ .file = "main.roc" } };
    return CliArgs{ .build = .{ .file = args[0] } };
}

fn parseRun(args: []const []const u8) CliArgs {
    _ = args;
    return CliArgs{ .run = .{ .file = "main.roc" } };
}

test "roc run" {
    const result = parse(&[_][]const u8{""});
    try testing.expectEqual(result.run.file, "main.roc");
}

test "roc check" {
    {
        const result = parse(&[_][]const u8{"check"});
        try testing.expectEqual(result.check.file, "main.roc");
    }
    {
        const result = parse(&[_][]const u8{ "check", "some/file.roc" });
        try testing.expectEqual(result.check.file, "some/file.roc");
    }
}

test "roc build" {
    {
        const result = parse(&[_][]const u8{"build"});
        try testing.expectEqual(result.build.file, "main.roc");
    }
    {
        const result = parse(&[_][]const u8{ "build", "foo.roc" });
        try testing.expectEqual(result.build.file, "foo.roc");
    }
}

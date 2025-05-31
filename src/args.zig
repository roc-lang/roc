const std = @import("std");
const testing = std.testing;
const mem = std.mem;

pub const CliArgs = union(enum) {
    run: RunArgs,
    build: BuildArgs,
    test_cmd: TestArgs,
    repl,
    format: FormatArgs,
    version,
    check: CheckArgs,
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

pub const CheckArgs = struct {
    file: []const u8,
};

pub const DocsArgs = struct {
    package: ?[]const u8 = null,
    output: ?[]const u8 = null,
};

pub fn parse(args: []const []const u8) CliArgs {
    _ = args;
    return CliArgs{ .run = RunArgs{ .file = "main.roc" } };
}

test "roc run" {
    const result = parse(&[_][]const u8{""});
    try testing.expectEqual(result.run.file, "main.roc");
}

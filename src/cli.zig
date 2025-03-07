const std = @import("std");
const testing = std.testing;
const mem = std.mem;

/// A sub-command for the roc cli tool
pub const RocCmd = enum {
    roc_run,
    roc_build,
    roc_test,
    roc_repl,
    roc_format,
    roc_version,
    roc_check,
    roc_docs,
    roc_glue,
    roc_help,

    pub fn parse(arg_str: []const u8) ?RocCmd {
        const map = std.static_string_map.StaticStringMap(RocCmd).initComptime(.{
            .{ "run", .roc_run },
            .{ "build", .roc_build },
            .{ "test", .roc_test },
            .{ "repl", .roc_repl },
            .{ "format", .roc_format },
            .{ "version", .roc_version },
            .{ "check", .roc_check },
            .{ "docs", .roc_docs },
            .{ "glue", .roc_glue },
            .{ "help", .roc_help },
        });

        if (map.get(arg_str)) |cmd| {
            return cmd;
        }
        return null;
    }
};

test "parse cli subcommands" {
    try testing.expectEqual(RocCmd.parse("build").?, .roc_build);
    try testing.expectEqual(RocCmd.parse(""), null);
}

/// Option flags for the roc cli tool
pub const RocOpt = struct {
    opt: enum {
        none,
        size,
        speed,
    } = .none,
    emit_llvm_ir: bool = false,
    profiling: bool = false,
    timing: bool = false,
    fuzzing: bool = false,

    pub fn parse(args: []const []const u8) !struct { opt: RocOpt, next_index: usize } {
        var opt = RocOpt{};
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            const arg = args[i];

            // If argument doesn't start with '-', we're done parsing options
            if (arg.len == 0 or arg[0] != '-') {
                return .{ .opt = opt, .next_index = i };
            }

            if (mem.eql(u8, arg, "--opt")) {
                // Check if there's a next argument
                if (i + 1 >= args.len) {
                    return error.MissingOptValue;
                }
                i += 1;
                const value = args[i];

                if (mem.eql(u8, value, "none")) {
                    opt.opt = .none;
                } else if (mem.eql(u8, value, "size")) {
                    opt.opt = .size;
                } else if (mem.eql(u8, value, "speed")) {
                    opt.opt = .speed;
                } else {
                    return error.InvalidOptValue;
                }
            } else if (mem.eql(u8, arg, "--emit-llvm-ir")) {
                opt.emit_llvm_ir = true;
            } else if (mem.eql(u8, arg, "--profiling")) {
                opt.profiling = true;
            } else if (mem.eql(u8, arg, "--time")) {
                opt.timing = true;
            } else if (mem.eql(u8, arg, "--fuzz")) {
                opt.fuzzing = true;
            } else {
                return error.InvalidArgument;
            }
        }

        return .{ .opt = opt, .next_index = i };
    }
};

test "default options" {
    try testing.expectEqual(RocOpt{}, RocOpt{
        .opt = .none,
        .emit_llvm_ir = false,
        .profiling = false,
        .timing = false,
        .fuzzing = false,
    });
}

// Testing helper, split a string into arguments ignoring whitespace and empty strings
fn splitArgs(allocator: std.mem.Allocator, str: []const u8) ![]const []const u8 {
    var args = std.ArrayList([]const u8).init(allocator);
    errdefer args.deinit();

    var iter = std.mem.split(u8, str, " ");
    while (iter.next()) |arg| {
        if (arg.len > 0) {
            try args.append(arg);
        }
    }

    return args.toOwnedSlice();
}

test "parsing cli options" {
    const TestCase = struct {
        args: []const u8,
        expected: union(enum) {
            ok: struct {
                opt: RocOpt,
                next_index: usize,
            },
            err: anyerror,
        },
    };

    const test_cases = &[_]TestCase{
        .{
            .args = "",
            .expected = .{ .ok = .{ .opt = RocOpt{}, .next_index = 0 } },
        },
        .{
            .args = "--opt size",
            .expected = .{ .ok = .{ .opt = RocOpt{ .opt = .size }, .next_index = 2 } },
        },
        .{
            .args = "--opt speed app.roc",
            .expected = .{ .ok = .{ .opt = RocOpt{ .opt = .speed }, .next_index = 2 } },
        },
        .{
            .args = "--time build app.roc",
            .expected = .{ .ok = .{ .opt = RocOpt{ .timing = true }, .next_index = 1 } },
        },
        .{
            .args = "--opt size --time app.roc",
            .expected = .{ .ok = .{
                .opt = RocOpt{ .opt = .size, .timing = true },
                .next_index = 3,
            } },
        },
        .{
            .args = "build --time",
            .expected = .{ .ok = .{ .opt = RocOpt{}, .next_index = 0 } },
        },
        .{
            .args = "--opt",
            .expected = .{ .err = error.MissingOptValue },
        },
        .{
            .args = "--opt invalid",
            .expected = .{ .err = error.InvalidOptValue },
        },
        .{
            .args = "--unknown-flag",
            .expected = .{ .err = error.InvalidArgument },
        },
        .{
            .args = "app.roc --invalid",
            .expected = .{ .ok = .{ .opt = RocOpt{}, .next_index = 0 } },
        },
    };

    for (test_cases) |tc| {
        const args = try splitArgs(testing.allocator, tc.args);
        defer testing.allocator.free(args);

        switch (tc.expected) {
            .ok => |expected| {
                const result = try RocOpt.parse(args);
                try testing.expectEqual(expected.opt, result.opt);
                try testing.expectEqual(expected.next_index, result.next_index);
            },
            .err => |expected_err| {
                const result = RocOpt.parse(args);
                try testing.expectError(expected_err, result);
            },
        }
    }
}

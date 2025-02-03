const std = @import("std");
const testing = std.testing;
const mem = std.mem;

pub const RocCmd = enum {
    roc_build,
    roc_test,
    roc_repl,
    roc_format,
    roc_version,
    roc_check,
    roc_docs,
    roc_glue,
    roc_help,

    pub fn parse(str: []const u8) ?RocCmd {
        const map = std.static_string_map.StaticStringMap(RocCmd).initComptime(.{
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

        if (map.get(str)) |cmd| {
            return cmd;
        }
        return null;
    }
};

test "parse cli subcommands" {
    try testing.expectEqual(RocCmd.parse("build").?, .roc_build);
    try testing.expectEqual(RocCmd.parse(""), null);
}

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

    pub fn parse(args: []const []const u8) !RocOpt {
        var opt = RocOpt{};
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
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
        return opt;
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

// Split a string into arguments ignoring whitespace and empty strings
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
            ok: RocOpt,
            err: anyerror,
        },
    };

    const test_cases = &[_]TestCase{
        .{
            .args = "",
            .expected = .{ .ok = RocOpt{} },
        },
        .{
            .args = "--opt size",
            .expected = .{ .ok = RocOpt{ .opt = .size } },
        },
        .{
            .args = "--opt speed",
            .expected = .{ .ok = RocOpt{ .opt = .speed } },
        },
        .{
            .args = "--time",
            .expected = .{ .ok = RocOpt{ .timing = true } },
        },
        .{
            .args = "--opt   size    --time",
            .expected = .{ .ok = RocOpt{ .opt = .size, .timing = true } },
        },
        .{
            .args = "  --time  --opt speed   --fuzz  ",
            .expected = .{ .ok = RocOpt{ .opt = .speed, .timing = true, .fuzzing = true } },
        },
        .{
            .args = "--emit-llvm-ir",
            .expected = .{ .ok = RocOpt{ .emit_llvm_ir = true } },
        },
        .{
            .args = "--profiling",
            .expected = .{ .ok = RocOpt{ .profiling = true } },
        },
        .{
            .args = "--opt none",
            .expected = .{ .ok = RocOpt{ .opt = .none } },
        },
        .{
            .args = "--emit-llvm-ir --profiling --time --fuzz",
            .expected = .{ .ok = RocOpt{
                .emit_llvm_ir = true,
                .profiling = true,
                .timing = true,
                .fuzzing = true,
            } },
        },
        .{
            .args = "--opt speed --emit-llvm-ir",
            .expected = .{ .ok = RocOpt{
                .opt = .speed,
                .emit_llvm_ir = true,
            } },
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
            .args = "--opt size --opt speed",
            .expected = .{ .ok = RocOpt{ .opt = .speed } },
        },
        .{
            .args = "--opt none --invalid",
            .expected = .{ .err = error.InvalidArgument },
        },
        .{
            .args = "--time --profiling --invalid-flag",
            .expected = .{ .err = error.InvalidArgument },
        },
    };

    for (test_cases) |tc| {
        const args = try splitArgs(testing.allocator, tc.args);
        defer testing.allocator.free(args);

        switch (tc.expected) {
            .ok => |expected| {
                const result = try RocOpt.parse(args);
                try testing.expectEqual(expected, result);
            },
            .err => |expected_err| {
                const result = RocOpt.parse(args);
                try testing.expectError(expected_err, result);
            },
        }
    }
}

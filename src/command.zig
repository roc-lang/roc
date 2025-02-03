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

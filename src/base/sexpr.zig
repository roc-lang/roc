const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

pub const Node = union(enum) {
    node: struct {
        value: []const u8,
        children: []const Node,
    },
    string: []const u8,
    signed_int: i128,
    unsigned_int: u128,
    float: f64,

    pub fn format(self: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .node => |n| {
                try writer.print("(", .{});
                try writer.print("{s}", .{n.value});
                try writer.print(" ", .{});

                for (n.children, 0..) |child, i| {
                    try child.format(fmt, options, writer);
                    if (i + 1 < n.children.len) {
                        try writer.print(" ", .{});
                    }
                }
                try writer.print(")", .{});
            },
            .string => |s| try writer.print("'{s}'", .{s}),
            .signed_int => |i| try writer.print("{d}", .{i}),
            .unsigned_int => |u| try writer.print("{d}", .{u}),
            .float => |f| try writer.print("{any}", .{f}),
        }
    }

    pub fn toString(node: Node, writer: std.io.AnyWriter) !void {
        switch (node) {
            .node => |n| {
                try writer.print("(", .{});
                try writer.print("{s}", .{n.value});
                try writer.print(" ", .{});

                for (n.children, 0..) |child, i| {
                    try child.toString(writer);
                    if (i + 1 < n.children.len) {
                        try writer.print(" ", .{});
                    }
                }
                try writer.print(")", .{});
            },
            .string => |s| try writer.print("'{s}'", .{s}),
            .signed_int => |i| try writer.print("{d}", .{i}),
            .unsigned_int => |u| try writer.print("{d}", .{u}),
            .float => |f| try writer.print("{any}", .{f}),
        }
    }

    pub fn deinit(self: *const Node, gpa: Allocator) void {
        switch (self.*) {
            .node => |n| {
                gpa.free(n.value);
                for (n.children) |child| {
                    child.deinit(gpa);
                }
            },
            .string => |str| gpa.free(str),
            .signed_int, .unsigned_int, .float => {
                // no-op - notinh to deinit
            },
        }
    }
};

test "s-expression" {
    const node: Node = .{
        .node = .{
            .value = "foo",
            .children = &.{
                .{ .string = "bar" },
                .{ .signed_int = -123 },
                .{
                    .node = .{
                        .value = "baz",
                        .children = &.{
                            .{ .unsigned_int = 456 },
                            .{ .float = 789.0 },
                        },
                    },
                },
            },
        },
    };

    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    try node.toString(buf.writer().any());

    try testing.expectEqualStrings("(foo 'bar' -123 (baz 456 7.89e2))", buf.items);
}

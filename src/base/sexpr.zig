const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

pub const Node = union(enum) {
    node: struct {
        value: []const u8,
        children: std.ArrayListUnmanaged(Node),
    },
    string: []const u8,
    signed_int: i128,
    unsigned_int: u128,
    float: f64,

    pub fn newNode(gpa: Allocator, value: []const u8) !Node {
        const owned_value = try gpa.dupe(u8, value);
        return .{
            .node = .{
                .value = owned_value,
                .children = try std.ArrayListUnmanaged(Node).initCapacity(gpa, 4),
            },
        };
    }

    fn appendChildUnsafe(self: *Node, gpa: Allocator, child: Node) !void {
        switch (self.*) {
            .node => |*n| try n.children.append(gpa, child),
            else => std.log.err("called appendChildUnsafe on a Node that is not a node, it is a {}", .{self}),
        }
    }

    pub fn appendStringChild(self: *Node, gpa: Allocator, value: []const u8) !void {
        const owned_value = try gpa.dupe(u8, value);
        try self.appendChildUnsafe(gpa, .{ .string = owned_value });
    }

    pub fn appendSignedIntChild(self: *Node, gpa: Allocator, value: i128) !void {
        try self.appendChildUnsafe(gpa, .{ .signed_int = value });
    }

    pub fn appendUnsignedIntChild(self: *Node, gpa: Allocator, value: u128) !void {
        try self.appendChildUnsafe(gpa, .{ .unsigned_int = value });
    }

    pub fn appendFloatChild(self: *Node, gpa: Allocator, value: f64) !void {
        try self.appendChildUnsafe(gpa, .{ .float = value });
    }

    pub fn appendNodeChild(self: *Node, gpa: Allocator, child_node: *Node) !void {
        try self.appendChildUnsafe(gpa, child_node.*);
    }

    pub fn format(self: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .node => |n| {
                try writer.print("(", .{});
                try writer.print("{s}", .{n.value});
                try writer.print(" ", .{});

                for (n.children.items, 0..) |child, i| {
                    try child.format(fmt, options, writer);
                    if (i + 1 < n.children.items.len) {
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

                for (n.children.items, 0..) |child, i| {
                    try child.toString(writer);
                    if (i + 1 < n.children.items.len) {
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

    pub fn deinit(self: *Node, gpa: Allocator) void {
        switch (self.*) {
            .node => |*n| {
                // Free the value string
                gpa.free(n.value);

                // Recursively free all children
                for (n.children.items) |*child| {
                    child.deinit(gpa);
                }

                // Free the children array itself
                n.children.deinit(gpa);
            },
            .string => |str| gpa.free(str),
            .signed_int, .unsigned_int, .float => {
                // no-op, nothing to deinit
            },
        }
    }
};

test "s-expression" {
    const gpa = testing.allocator;

    var baz = try Node.newNode(gpa, "baz");
    try baz.appendUnsignedIntChild(gpa, 456);
    try baz.appendFloatChild(gpa, 789.0);

    var foo = try Node.newNode(gpa, "foo");
    try foo.appendStringChild(gpa, "bar");
    try foo.appendSignedIntChild(gpa, -123);
    try foo.appendNodeChild(gpa, &baz);

    var buf = std.ArrayList(u8).init(gpa);
    defer buf.deinit();

    try foo.toString(buf.writer().any());

    try testing.expectEqualStrings("(foo 'bar' -123 (baz 456 7.89e2))", buf.items);

    // clean up -- foo is our root and will deinit all children
    foo.deinit(gpa);
}

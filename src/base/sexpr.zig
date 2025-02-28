const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

/// Represents a node in an S-expression tree.
pub const Node = union(enum) {
    node: struct {
        value: []const u8,
        children: std.ArrayListUnmanaged(Node),
    },
    string: []const u8,
    signed_int: i128,
    unsigned_int: u128,
    float: f64,

    /// Create a new node with the given value.
    /// The value will be duplicated so that it is owned by the node
    /// and will be freed when the node is destroyed.
    pub fn newNode(gpa: Allocator, value: []const u8) !Node {
        const owned_value = try gpa.dupe(u8, value);
        return .{
            .node = .{
                .value = owned_value,
                .children = try std.ArrayListUnmanaged(Node).initCapacity(gpa, 4),
            },
        };
    }

    // Helper function to append a child node to a parent node
    fn appendChildUnsafe(self: *Node, gpa: Allocator, child: Node) !void {
        switch (self.*) {
            .node => |*n| try n.children.append(gpa, child),
            else => std.debug.panic("called appendChildUnsafe on a Node that is not a node, it is a {}", .{self}),
        }
    }

    /// Helper function to append a string child
    /// The value will be duplicated so that it is owned by the node
    /// and will be freed when the node is destroyed.
    pub fn appendStringChild(self: *Node, gpa: Allocator, value: []const u8) !void {
        const owned_value = try gpa.dupe(u8, value);
        try self.appendChildUnsafe(gpa, .{ .string = owned_value });
    }

    /// Helper function to append a signed integer child
    pub fn appendSignedIntChild(self: *Node, gpa: Allocator, value: i128) !void {
        try self.appendChildUnsafe(gpa, .{ .signed_int = value });
    }

    /// Helper function to append an unsigned integer child
    pub fn appendUnsignedIntChild(self: *Node, gpa: Allocator, value: u128) !void {
        try self.appendChildUnsafe(gpa, .{ .unsigned_int = value });
    }

    /// Helper function to append a float child
    pub fn appendFloatChild(self: *Node, gpa: Allocator, value: f64) !void {
        try self.appendChildUnsafe(gpa, .{ .float = value });
    }

    /// Helper function to append a node child
    pub fn appendNodeChild(self: *Node, gpa: Allocator, child_node: *Node) !void {
        try self.appendChildUnsafe(gpa, child_node.*);
    }

    /// Format the node as an S-expression formatted string
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

    /// Write the node as an S-expression formatted string to the given writer.
    fn toString(node: Node, writer: std.io.AnyWriter, options: struct {
        indent: ?usize = null,
        current_level: usize = 0,
    }) !void {
        const indent_level = options.indent orelse 0;
        const has_indent = indent_level > 0;

        switch (node) {
            .node => |n| {
                try writer.print("(", .{});
                try writer.print("{s}", .{n.value});

                if (n.children.items.len > 0) {
                    // If we have indentation and children, format with newlines
                    if (has_indent) {
                        const next_level = options.current_level + 1;

                        for (n.children.items) |child| {
                            try writer.print("\n", .{});

                            // Print indentation
                            try writeIndent(writer, next_level * indent_level);

                            try child.toString(writer, .{
                                .indent = indent_level,
                                .current_level = next_level,
                            });
                        }
                    } else {
                        // No indentation, use the original compact format
                        try writer.print(" ", .{});

                        for (n.children.items, 0..) |child, i| {
                            try child.toString(writer, .{});
                            if (i + 1 < n.children.items.len) {
                                try writer.print(" ", .{});
                            }
                        }
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

    pub fn toStringCompact(node: Node, writer: std.io.AnyWriter) !void {
        return toString(node, writer, .{});
    }

    pub fn toStringPretty(node: Node, writer: std.io.AnyWriter, indent_size: usize) !void {
        return toString(node, writer, .{ .indent = indent_size });
    }

    /// Deinitialize the node and all its children.
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

fn writeIndent(writer: std.io.AnyWriter, spaces: usize) !void {
    for (0..spaces) |_| {
        try writer.writeAll(" ");
    }
}

test "s-expression" {
    const gpa = testing.allocator;

    var baz = try Node.newNode(gpa, "baz");
    try baz.appendUnsignedIntChild(gpa, 456);
    try baz.appendFloatChild(gpa, 789.0);

    var foo = try Node.newNode(gpa, "foo");
    try foo.appendStringChild(gpa, "bar");
    try foo.appendSignedIntChild(gpa, -123);
    try foo.appendNodeChild(gpa, &baz);

    // Test compact formatting
    {
        var buf = std.ArrayList(u8).init(gpa);
        defer buf.deinit();
        try foo.toStringCompact(buf.writer().any());
        const expected =
            \\(foo 'bar' -123 (baz 456 7.89e2))
        ;
        try testing.expectEqualStrings(expected, buf.items);
    }

    // Test pretty formatting
    {
        var buf = std.ArrayList(u8).init(gpa);
        defer buf.deinit();
        const spaces = 4;
        try foo.toStringPretty(buf.writer().any(), spaces);
        const expected =
            \\(foo
            \\    'bar'
            \\    -123
            \\    (baz
            \\        456
            \\        7.89e2))
        ;
        try testing.expectEqualStrings(expected, buf.items);
    }

    // clean up -- foo is our root and will deinit all children
    foo.deinit(gpa);
}

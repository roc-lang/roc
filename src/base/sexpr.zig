//! A simple format for representing tree-like data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const RegionInfo = @import("RegionInfo.zig");

/// How many child nodes before breaking to a newline
const CHILD_COUNT_BREAKPOINT = 5;

/// Represents a node in an S-expression tree.
pub const Expr = union(enum) {
    node: struct {
        value: []const u8,
        children: std.ArrayListUnmanaged(Expr),
    },
    region: RegionInfo,
    string: []const u8,
    signed_int: i128,
    unsigned_int: u128,
    float: f64,

    /// Create a new node with the given value.
    /// The value will be duplicated so that it is owned by the node
    /// and will be freed when the node is destroyed.
    pub fn init(gpa: Allocator, value: []const u8) Expr {
        const owned_value = gpa.dupe(u8, value) catch {
            @panic("Failed to duplicate value");
        };
        return .{
            .node = .{
                .value = owned_value,
                .children = std.ArrayListUnmanaged(Expr).initCapacity(gpa, 4) catch {
                    @panic("Failed to initialize children array");
                },
            },
        };
    }

    /// Deinitialize the node and all its children.
    pub fn deinit(self: *Expr, gpa: Allocator) void {
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
            .region => |region| {
                gpa.free(region.line_text);
            },
            .string => |str| gpa.free(str),
            .signed_int, .unsigned_int, .float => {
                // no-op, nothing to deinit
            },
        }
    }

    // Helper function to append a child node to a parent node
    fn appendNodeUnsafe(self: *Expr, gpa: Allocator, child: Expr) void {
        switch (self.*) {
            .node => |*n| n.children.append(gpa, child) catch {
                @panic("Failed to append child node");
            },
            else => @panic("called appendNodeUnsafe on a Expr that is not a .node"),
        }
    }

    /// Helper function to append a string child
    /// The value will be duplicated so that it is owned by the node
    /// and will be freed when the node is destroyed.
    pub fn appendString(self: *Expr, gpa: Allocator, value: []const u8) void {
        const owned_value = gpa.dupe(u8, value) catch {
            @panic("Failed to duplicate string value");
        };
        self.appendNodeUnsafe(gpa, .{ .string = owned_value });
    }

    pub fn appendRegionInfo(self: *Expr, gpa: Allocator, region: RegionInfo) void {
        self.appendNodeUnsafe(gpa, .{ .region = RegionInfo{
            .start_line_idx = region.start_line_idx,
            .start_col_idx = region.start_col_idx,
            .end_line_idx = region.end_line_idx,
            .end_col_idx = region.end_col_idx,
            .line_text = gpa.dupe(u8, region.line_text) catch {
                @panic("Failed to duplicate string value");
            },
        } });
    }

    /// Helper function to append a signed integer child
    pub fn appendSignedInt(self: *Expr, gpa: Allocator, value: i128) void {
        self.appendNodeUnsafe(gpa, .{ .signed_int = value });
    }

    /// Helper function to append an unsigned integer child
    pub fn appendUnsignedInt(self: *Expr, gpa: Allocator, value: u128) void {
        self.appendNodeUnsafe(gpa, .{ .unsigned_int = value });
    }

    /// Helper function to append a float child
    pub fn appendFloat(self: *Expr, gpa: Allocator, value: f64) void {
        self.appendNodeUnsafe(gpa, .{ .float = value });
    }

    /// Helper function to append a node child
    pub fn appendNode(self: *Expr, gpa: Allocator, child_node: *Expr) void {
        self.appendNodeUnsafe(gpa, child_node.*);
    }

    /// Format the node as an S-expression formatted string
    pub fn format(self: Expr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try toStringPretty(self, writer);
    }

    fn countItems(node: *const Expr) usize {
        switch (node.*) {
            .node => |n| {
                var count: usize = 1;
                for (n.children.items) |child| {
                    count += countItems(&child);
                }
                return count;
            },
            else => return 1,
        }
    }

    fn isSimple(node: *const Expr) bool {
        return countItems(node) <= CHILD_COUNT_BREAKPOINT;
    }

    /// Write the node as an S-expression formatted string to the given writer.
    fn toString(node: Expr, writer: std.io.AnyWriter, indent: usize) !void {
        switch (node) {
            .node => |n| {
                try writer.print("(", .{});
                try writer.print("{s}", .{n.value});

                if (n.children.items.len > 0) {
                    // If we have indentation and children, format with newlines
                    if (node.isSimple()) {
                        // No indentation, use the original compact format
                        try writer.print(" ", .{});

                        for (n.children.items, 0..) |child, i| {
                            try child.toString(writer, indent + 1);
                            if (i + 1 < n.children.items.len) {
                                try writer.print(" ", .{});
                            }
                        }
                    } else {
                        for (n.children.items) |child| {
                            switch (child) {
                                .region => {
                                    try writer.print(" ", .{});
                                    try child.toString(writer, indent + 1);
                                },
                                else => {
                                    try writer.print("\n", .{});

                                    // Print indentation
                                    try writeIndent(writer, indent + 1);

                                    try child.toString(writer, indent + 1);
                                },
                            }
                        }
                    }
                }

                try writer.print(")", .{});
            },
            .region => |region| {
                try writer.print("({d}:{d}-{d}:{d})", .{
                    // add one to display numbers instead of index
                    region.start_line_idx + 1,
                    region.start_col_idx + 1,
                    region.end_line_idx + 1,
                    region.end_col_idx + 1,
                });
            },
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .signed_int => |i| try writer.print("{d}", .{i}),
            .unsigned_int => |u| try writer.print("{d}", .{u}),
            .float => |f| try writer.print("{any}", .{f}),
        }
    }

    /// Render this S-Expr to a writer with pleasing indentation.
    pub fn toStringPretty(node: Expr, writer: std.io.AnyWriter) void {
        return toString(node, writer, 0) catch {
            @panic("Ran out of memory writing S-expression to writer");
        };
    }
};

fn writeIndent(writer: std.io.AnyWriter, tabs: usize) !void {
    for (0..tabs) |_| {
        try writer.writeByte('\t');
    }
}

test "s-expression" {
    const gpa = testing.allocator;

    var baz = Expr.init(gpa, "baz");
    baz.appendUnsignedInt(gpa, 456);
    baz.appendFloat(gpa, 789.0);

    var foo = Expr.init(gpa, "foo");
    foo.appendString(gpa, "bar");
    foo.appendSignedInt(gpa, -123);
    foo.appendNode(gpa, &baz);

    // Test pretty formatting
    {
        var buf = std.ArrayList(u8).init(gpa);
        defer buf.deinit();
        foo.toStringPretty(buf.writer().any());
        const expected =
            "(foo\n" ++
            "\t\"bar\"\n" ++
            "\t-123\n" ++
            "\t(baz 456 7.89e2))";
        try testing.expectEqualStrings(expected, buf.items);
    }

    // clean up -- foo is our root and will deinit all children
    foo.deinit(gpa);
}

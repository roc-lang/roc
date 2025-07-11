//! A S-expression tree representation

const std = @import("std");
const testing = std.testing;
const RegionInfo = @import("RegionInfo.zig");

const SExprTree = @This();

/// Newtype to represent a node begin marker, prevents confusion with raw indices
pub const NodeBegin = struct {
    stack_idx: u32,
};

/// Colors for syntax highlighting S-expressions
pub const Color = enum {
    default,
    node_name,
    string,
    number,
    punctuation,
};

/// Helper function to escape HTML characters
fn escapeHtmlChar(writer: anytype, char: u8) !void {
    switch (char) {
        '<' => try writer.writeAll("&lt;"),
        '>' => try writer.writeAll("&gt;"),
        '&' => try writer.writeAll("&amp;"),
        '"' => try writer.writeAll("&quot;"),
        '\'' => try writer.writeAll("&#x27;"),
        else => try writer.writeByte(char),
    }
}

/// Plain text writer implementation
const PlainTextSExprWriter = struct {
    writer: std.io.AnyWriter,

    pub fn print(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        try self.writer.print(fmt, args);
    }

    pub fn setColor(self: *@This(), color: Color) !void {
        _ = self;
        _ = color;
        // No-op for plain text
    }

    pub fn beginSourceRange(self: *@This(), start_byte: u32, end_byte: u32) !void {
        _ = self;
        _ = start_byte;
        _ = end_byte;
        // No-op for plain text
    }

    pub fn endSourceRange(self: *@This()) !void {
        _ = self;
        // No-op for plain text
    }

    pub fn writeIndent(self: *@This(), tabs: usize) !void {
        for (0..tabs) |_| {
            try self.writer.writeAll("\t");
        }
    }
};

/// HTML writer implementation with syntax highlighting
const HtmlSExprWriter = struct {
    writer: std.io.AnyWriter,
    current_color: Color = .default,
    color_active: bool = false,
    scratch_buffer: std.ArrayList(u8),

    pub fn init(writer: std.io.AnyWriter) HtmlSExprWriter {
        return HtmlSExprWriter{
            .writer = writer,
            .current_color = .default,
            .color_active = false,
            .scratch_buffer = std.ArrayList(u8).init(std.heap.page_allocator),
        };
    }

    pub fn print(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        self.scratch_buffer.clearRetainingCapacity();
        try std.fmt.format(self.scratch_buffer.writer(), fmt, args);

        for (self.scratch_buffer.items) |char| {
            try escapeHtmlChar(self.writer, char);
        }
    }

    pub fn setColor(self: *@This(), color: Color) !void {
        if (self.color_active and self.current_color != color) {
            try self.writer.writeAll("</span>");
            self.color_active = false;
        }

        if (color != .default and (!self.color_active or self.current_color != color)) {
            const css_class = switch (color) {
                .default => "token-default",
                .node_name => "token-keyword", // Node names are like keywords in S-expressions
                .string => "token-string",
                .number => "token-number",
                .punctuation => "token-punctuation",
            };
            try self.writer.print("<span class=\"{s}\">", .{css_class});
            self.color_active = true;
        }

        self.current_color = color;
    }

    pub fn beginSourceRange(self: *@This(), start_byte: u32, end_byte: u32) !void {
        try self.writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\" >", .{ start_byte, end_byte });
    }

    pub fn endSourceRange(self: *@This()) !void {
        try self.writer.writeAll("</span>");
    }

    pub fn writeIndent(self: *@This(), tabs: usize) !void {
        for (0..tabs) |_| {
            try self.writer.writeAll("  ");
        }
    }

    pub fn deinit(self: *@This()) !void {
        if (self.color_active) {
            try self.writer.writeAll("</span>");
            self.color_active = false;
        }
        self.scratch_buffer.deinit();
    }
};

const Node = union(enum) {
    StaticAtom: []const u8,
    DynamicAtom: struct { begin: u32, end: u32 },
    List: struct { begin: u32, attrs_marker: u32, end: u32 },
    String: struct { begin: u32, end: u32 },
    Boolean: bool,
    NodeIdx: u32,
    BytesRange: struct { begin: u32, end: u32, region: RegionInfo },
};

children: std.ArrayListUnmanaged(Node),
data: std.ArrayListUnmanaged(u8),
stack: std.ArrayListUnmanaged(Node),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) SExprTree {
    return SExprTree{
        .children = .{},
        .data = .{},
        .stack = .{},
        .allocator = allocator,
    };
}

pub fn deinit(self: *SExprTree) void {
    self.children.deinit(self.allocator);
    self.data.deinit(self.allocator);
    self.stack.deinit(self.allocator);
}

/// Push a static atom (e.g. node name) onto the stack
pub fn pushStaticAtom(self: *SExprTree, value: []const u8) void {
    self.stack.append(self.allocator, Node{ .StaticAtom = value }) catch unreachable;
}

/// Push a string (copied into data buffer) onto the stack
pub fn pushString(self: *SExprTree, value: []const u8) void {
    const begin: u32 = @intCast(self.data.items.len);
    self.data.appendSlice(self.allocator, value) catch unreachable;
    const end: u32 = @intCast(self.data.items.len);
    self.stack.append(self.allocator, Node{ .String = .{ .begin = begin, .end = end } }) catch unreachable;
}

/// Push a string key-value pair onto the stack
pub fn pushStringPair(self: *SExprTree, key: []const u8, value: []const u8) void {
    const begin = self.beginNode();
    self.pushStaticAtom(key);
    self.pushString(value);
    const attrs = self.beginNode();
    self.endNode(begin, attrs);
}

/// Push a dynamic atom (copied into data buffer) onto the stack
pub fn pushDynamicAtom(self: *SExprTree, value: []const u8) void {
    const begin: u32 = @intCast(self.data.items.len);
    self.data.appendSlice(self.allocator, value) catch unreachable;
    const end: u32 = @intCast(self.data.items.len);
    self.stack.append(self.allocator, Node{ .DynamicAtom = .{ .begin = begin, .end = end } }) catch unreachable;
}

/// Push a dynamic atom key-value pair onto the stack
pub fn pushDynamicAtomPair(self: *SExprTree, key: []const u8, value: []const u8) void {
    const begin = self.beginNode();
    self.pushStaticAtom(key);
    self.pushDynamicAtom(value);
    const attrs = self.beginNode();
    self.endNode(begin, attrs);
}

/// Push a boolean node onto the stack
pub fn pushBool(self: *SExprTree, value: bool) void {
    self.stack.append(self.allocator, Node{ .Boolean = value }) catch unreachable;
}

/// Push a boolean key-value pair onto the stack
pub fn pushBoolPair(self: *SExprTree, key: []const u8, value: bool) void {
    const begin = self.beginNode();
    self.pushStaticAtom(key);
    self.pushBool(value);
    const attrs = self.beginNode();
    self.endNode(begin, attrs);
}

/// Push a NodeIdx node onto the stack
pub fn pushNodeIdx(self: *SExprTree, idx: u32) void {
    self.stack.append(self.allocator, Node{ .NodeIdx = idx }) catch unreachable;
}

/// Push a BytesRange node onto the stack
pub fn pushBytesRange(self: *SExprTree, begin: u32, end: u32, region: RegionInfo) void {
    self.stack.append(self.allocator, Node{ .BytesRange = .{ .begin = begin, .end = end, .region = region } }) catch unreachable;
}

/// Begin a new node, returning a marker for the current stack position
pub fn beginNode(self: *SExprTree) NodeBegin {
    return NodeBegin{ .stack_idx = @intCast(self.stack.items.len) };
}

/// End a node by popping all items since the begin marker and creating a List node
pub fn endNode(self: *SExprTree, begin: NodeBegin, attrsMarker: NodeBegin) void {
    const total = self.stack.items.len;
    const start_idx = begin.stack_idx;
    if (start_idx > total) unreachable;

    const attrs_end_idx: u32 = @intCast(attrsMarker.stack_idx);
    if (attrs_end_idx > total) unreachable;
    if (start_idx > attrs_end_idx) unreachable;

    const children_begin: u32 = @intCast(self.children.items.len);
    for (self.stack.items[start_idx..total]) |node| {
        self.children.append(self.allocator, node) catch unreachable;
    }
    const children_end: u32 = @intCast(self.children.items.len);
    const attrs_end = children_begin + (attrs_end_idx - start_idx);

    // Remove items from stack
    self.stack.shrinkRetainingCapacity(start_idx);
    self.stack.append(self.allocator, Node{ .List = .{ .begin = children_begin, .attrs_marker = attrs_end, .end = children_end } }) catch unreachable;
}

/// Internal method that writes the node using a writer implementation
fn toStringImpl(self: *const SExprTree, node: Node, writer_impl: anytype, indent: usize) !void {
    switch (node) {
        .StaticAtom => |s| {
            try writer_impl.setColor(.node_name);
            try writer_impl.print("{s}", .{s});
            try writer_impl.setColor(.default);
        },
        .DynamicAtom => |range| {
            const s = self.data.items[range.begin..range.end];
            try writer_impl.setColor(.node_name);
            try writer_impl.print("{s}", .{s});
            try writer_impl.setColor(.default);
        },
        .String => |range| {
            const s = self.data.items[range.begin..range.end];
            try writer_impl.setColor(.punctuation);
            try writer_impl.print("\"", .{});
            try writer_impl.setColor(.string);
            try writer_impl.print("{s}", .{s});
            try writer_impl.setColor(.punctuation);
            try writer_impl.print("\"", .{});
            try writer_impl.setColor(.default);
        },
        .Boolean => |b| {
            try writer_impl.setColor(.number);
            try writer_impl.print("{}", .{b});
            try writer_impl.setColor(.default);
        },
        .NodeIdx => |idx| {
            try writer_impl.setColor(.number);
            try writer_impl.print("node_{d}", .{idx});
            try writer_impl.setColor(.default);
        },
        .BytesRange => |range| {
            try writer_impl.beginSourceRange(range.begin, range.end);
            // try writer_impl.print("@{d}-{d}", .{ range.begin, range.end });
            try writer_impl.print("@{d}.{d}-{d}.{d}", .{
                // add one to display numbers instead of index
                range.region.start_line_idx + 1,
                range.region.start_col_idx + 1,
                range.region.end_line_idx + 1,
                range.region.end_col_idx + 1,
            });
            try writer_impl.endSourceRange();
        },
        .List => |range| {
            try writer_impl.setColor(.punctuation);
            try writer_impl.print("(", .{});
            try writer_impl.setColor(.default);

            var first = true;
            for (range.begin..range.attrs_marker) |i| {
                if (!first) {
                    try writer_impl.print(" ", .{});
                }
                first = false;
                try self.toStringImpl(self.children.items[i], writer_impl, indent + 1);
            }

            for (range.attrs_marker..range.end) |i| {
                if (!first) {
                    try writer_impl.print("\n", .{});
                    try writer_impl.writeIndent(indent + 1);
                }
                first = false;
                try self.toStringImpl(self.children.items[i], writer_impl, indent + 1);
            }

            try writer_impl.setColor(.punctuation);
            try writer_impl.print(")", .{});
            try writer_impl.setColor(.default);
        },
    }
}

/// Pretty-print the root node (top of stack) to the writer
pub fn printTree(self: *const SExprTree, writer: anytype) !void {
    if (self.stack.items.len == 0) return;
    var plain_writer = PlainTextSExprWriter{ .writer = writer.any() };
    try self.toStringImpl(self.stack.items[self.stack.items.len - 1], &plain_writer, 0);
}

/// Render this SExprTree to a writer with pleasing indentation.
pub fn toStringPretty(self: *const SExprTree, writer: std.io.AnyWriter) void {
    if (self.stack.items.len == 0) return;
    var plain_writer = PlainTextSExprWriter{ .writer = writer };
    self.toStringImpl(self.stack.items[self.stack.items.len - 1], &plain_writer, 0) catch {
        @panic("Ran out of memory writing SExprTree to writer");
    };
}

/// Render this SExprTree to HTML with syntax highlighting.
pub fn toHtml(self: *const SExprTree, writer: std.io.AnyWriter) void {
    if (self.stack.items.len == 0) return;
    var html_writer = HtmlSExprWriter.init(writer);
    self.toStringImpl(self.stack.items[self.stack.items.len - 1], &html_writer, 0) catch {
        @panic("Ran out of memory writing SExprTree to HTML writer");
    };
    html_writer.deinit() catch {
        @panic("Error finalizing HTML writer");
    };
}

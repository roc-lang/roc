//! A simple format for representing tree-like data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const RegionInfo = @import("RegionInfo.zig");
const exitOnOom = @import("../collections.zig").exitOnOom;

/// A simple format for representing tree-like data.
pub const SExpr = @This();

/// name of the s-expression node
value: []const u8,
/// list of attributes `:name=value`
attributes: std.ArrayListUnmanaged(Attribute),
/// list of children nodes
children: std.ArrayListUnmanaged(SExpr),

/// Colors for syntax highlighting S-expressions
pub const Color = enum {
    default,
    node_name,
    string,
    number,
    region,
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

    pub fn beginSourceRange(self: *@This(), start_token: u32, end_token: u32) !void {
        _ = self;
        _ = start_token;
        _ = end_token;
        // No-op for plain text
    }

    pub fn endSourceRange(self: *@This()) !void {
        _ = self;
        // No-op for plain text
    }
};

/// HTML writer implementation with syntax highlighting
const HtmlSExprWriter = struct {
    writer: std.io.AnyWriter,
    current_color: Color = .default,
    color_active: bool = false,

    pub fn print(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        const formatted = std.fmt.allocPrint(std.heap.page_allocator, fmt, args) catch return;
        defer std.heap.page_allocator.free(formatted);

        for (formatted) |char| {
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
                .region => "token-comment", // Regions are metadata, similar to comments
                .punctuation => "token-punctuation",
            };
            try self.writer.print("<span class=\"{s}\">", .{css_class});
            self.color_active = true;
        }

        self.current_color = color;
    }

    pub fn beginSourceRange(self: *@This(), start_token: u32, end_token: u32) !void {
        try self.writer.print("<span class=\"source-range\" data-start-token=\"{d}\" data-end-token=\"{d}\" >", .{ start_token, end_token });
    }

    pub fn endSourceRange(self: *@This()) !void {
        try self.writer.writeAll("</span>");
    }

    pub fn deinit(self: *@This()) !void {
        if (self.color_active) {
            try self.writer.writeAll("</span>");
            self.color_active = false;
        }
    }
};

/// Attribute value can be different types
pub const AttributeValue = union(enum) {
    string: []const u8,
    boolean: bool,
    node_idx: u32,
    region: RegionInfo,
    raw_string: []const u8, // for unquoted strings
    tokens_range: struct {
        region: RegionInfo,
        start_token: u32,
        end_token: u32,
    },
};

/// Represents a key-value attribute pair
pub const Attribute = struct {
    key: []const u8,
    value: AttributeValue,
};

/// Create a new node with the given value.
/// The value will be duplicated so that it is owned by the node
/// and will be freed when the node is destroyed.
pub fn init(gpa: Allocator, value: []const u8) SExpr {
    const owned_value = gpa.dupe(u8, value) catch |err| exitOnOom(err);
    return .{
        .value = owned_value,
        .attributes = std.ArrayListUnmanaged(Attribute){},
        .children = std.ArrayListUnmanaged(SExpr){},
    };
}

/// Deinitialize the node and all its children.
pub fn deinit(self: *SExpr, gpa: Allocator) void {
    // Free the value string if it's not empty
    if (self.value.len > 0) {
        gpa.free(self.value);
    }

    // Free all attributes
    for (self.attributes.items) |attr| {
        gpa.free(attr.key);
        switch (attr.value) {
            .string, .raw_string => |str| gpa.free(str),
            .region => |r| {
                // Free the region line text if it's not empty
                if (r.line_text.len > 0) {
                    gpa.free(r.line_text);
                }
            },
            .tokens_range => |tr| {
                // Free the region line text if it's not empty
                if (tr.region.line_text.len > 0) {
                    gpa.free(tr.region.line_text);
                }
            },
            .boolean, .node_idx => {
                // no-op
            },
        }
    }
    self.attributes.deinit(gpa);

    // Recursively free all children
    for (self.children.items) |*child| {
        child.deinit(gpa);
    }

    // Free the children array itself
    self.children.deinit(gpa);
}

/// Helper function to append a child node to a parent node
fn appendNodeUnsafe(self: *SExpr, gpa: Allocator, child: SExpr) void {
    self.children.append(gpa, child) catch |err| exitOnOom(err);
}

/// Add an attribute to the node
///
/// Clones the "key" bytes
fn addAttribute(self: *SExpr, gpa: Allocator, key: []const u8, value: AttributeValue) void {
    const owned_key = gpa.dupe(u8, key) catch |err| exitOnOom(err);
    const attr = Attribute{
        .key = owned_key,
        .value = value,
    };
    self.attributes.append(gpa, attr) catch |err| exitOnOom(err);
}

/// Helper function to append a node
pub fn appendNode(self: *SExpr, gpa: Allocator, child_node: *const SExpr) void {
    self.appendNodeUnsafe(gpa, child_node.*);
}

/// Append a region attribute
pub fn appendRegion(self: *SExpr, gpa: Allocator, region: RegionInfo) void {
    const owned_value = gpa.dupe(u8, region.line_text) catch |err| exitOnOom(err);
    self.addAttribute(gpa, "region", .{ .region = RegionInfo{
        .end_col_idx = region.end_col_idx,
        .end_line_idx = region.end_line_idx,
        .start_col_idx = region.start_col_idx,
        .start_line_idx = region.start_line_idx,
        .line_text = owned_value,
    } });
}

/// Append a raw string attribute in the format :key=value
pub fn appendRawAttr(self: *SExpr, gpa: Allocator, key: []const u8, value: []const u8) void {
    const owned_value = gpa.dupe(u8, value) catch |err| exitOnOom(err);
    self.addAttribute(gpa, key, .{ .raw_string = owned_value });
}

/// Append a string attribute in the format :key="value"
pub fn appendStringAttr(self: *SExpr, gpa: Allocator, key: []const u8, string: []const u8) void {
    const owned_string = gpa.dupe(u8, string) catch |err| exitOnOom(err);
    self.addAttribute(gpa, key, .{ .string = owned_string });
}

/// Append a node index
pub fn appendIdx(self: *SExpr, gpa: Allocator, node_idx: anytype) void {
    const node_idx_u32 = @intFromEnum(node_idx);
    self.addAttribute(gpa, "id", .{ .node_idx = node_idx_u32 });
}

/// Append a type variable attribute in the format :key=#123
pub fn appendTypeVar(self: *SExpr, gpa: Allocator, key: []const u8, type_var: anytype) void {
    const var_value = @intFromEnum(type_var);
    self.addAttribute(gpa, key, .{ .node_idx = var_value });
}

/// Append a boolean attribute in the format :key=true/false
pub fn appendBoolAttr(self: *SExpr, gpa: Allocator, key: []const u8, value: bool) void {
    self.addAttribute(gpa, key, .{ .boolean = value });
}

/// Append a token range attribute with region information
pub fn appendTokenRange(self: *SExpr, gpa: Allocator, region: RegionInfo, start_token: u32, end_token: u32) void {
    const owned_value = gpa.dupe(u8, region.line_text) catch |err| exitOnOom(err);
    self.addAttribute(gpa, "tokens", .{ .tokens_range = .{
        .region = RegionInfo{
            .start_line_idx = region.start_line_idx,
            .start_col_idx = region.start_col_idx,
            .end_line_idx = region.end_line_idx,
            .end_col_idx = region.end_col_idx,
            .line_text = owned_value,
        },
        .start_token = start_token,
        .end_token = end_token,
    } });
}

/// Format the node as an S-expression formatted string
pub fn format(self: SExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;
    try toStringPretty(self, writer);
}

/// Internal method that writes the node using a writer implementation
fn toStringImpl(node: SExpr, writer_impl: anytype, indent: usize) !void {
    // Handle regular nodes
    try writer_impl.setColor(.punctuation);
    try writer_impl.print("(", .{});
    try writer_impl.setColor(.node_name);
    try writer_impl.print("{s}", .{node.value});
    try writer_impl.setColor(.default);

    // Print attributes if present
    for (node.attributes.items) |attr| {
        switch (attr.value) {
            .string => |s| {
                try writer_impl.print(" (", .{});
                try writer_impl.setColor(.node_name);
                try writer_impl.print("{s}", .{attr.key});
                try writer_impl.setColor(.default);
                try writer_impl.print(" ", .{});
                try writer_impl.setColor(.punctuation);
                try writer_impl.print("\"", .{});
                try writer_impl.setColor(.string);
                try writer_impl.print("{s}", .{s});
                try writer_impl.setColor(.punctuation);
                try writer_impl.print("\")", .{});
                try writer_impl.setColor(.default);
            },
            .boolean => |b| {
                try writer_impl.print(" (", .{});
                try writer_impl.setColor(.node_name);
                try writer_impl.print("{s}", .{attr.key});
                try writer_impl.setColor(.default);
                try writer_impl.print(" ", .{});
                try writer_impl.setColor(.number);
                try writer_impl.print("{}", .{b});
                try writer_impl.setColor(.default);
                try writer_impl.print(")", .{});
            },
            .node_idx => |idx| {
                try writer_impl.print(" (", .{});
                try writer_impl.setColor(.node_name);
                try writer_impl.print("{s}", .{attr.key});
                try writer_impl.setColor(.default);
                try writer_impl.print(" ", .{});
                try writer_impl.setColor(.number);
                try writer_impl.print("{d}", .{idx});
                try writer_impl.setColor(.default);
                try writer_impl.print(")", .{});
            },
            .raw_string => |s| {
                try writer_impl.print(" (", .{});
                try writer_impl.setColor(.node_name);
                try writer_impl.print("{s}", .{attr.key});
                try writer_impl.setColor(.default);
                try writer_impl.print(" ", .{});
                try writer_impl.setColor(.string);
                try writer_impl.print("{s}", .{s});
                try writer_impl.setColor(.default);
                try writer_impl.print(")", .{});
            },
            .region => |r| {
                try writer_impl.print(" ", .{});
                try writer_impl.setColor(.region);
                try writer_impl.print("@{d}.{d}-{d}.{d}", .{
                    // add one to display numbers instead of index
                    r.start_line_idx + 1,
                    r.start_col_idx + 1,
                    r.end_line_idx + 1,
                    r.end_col_idx + 1,
                });
                try writer_impl.setColor(.default);
            },
            .tokens_range => |tr| {
                try writer_impl.print(" ", .{});
                try writer_impl.beginSourceRange(tr.start_token, tr.end_token);
                try writer_impl.setColor(.region);
                try writer_impl.print("@{d}.{d}-{d}.{d}", .{
                    // add one to display numbers instead of index
                    tr.region.start_line_idx + 1,
                    tr.region.start_col_idx + 1,
                    tr.region.end_line_idx + 1,
                    tr.region.end_col_idx + 1,
                });
                try writer_impl.setColor(.default);
                try writer_impl.endSourceRange();
            },
        }
    }

    if (node.children.items.len > 0) {
        for (node.children.items) |child| {
            try writer_impl.print("\n", .{});

            // Print indentation
            try writeIndentImpl(writer_impl, indent + 1);

            try child.toStringImpl(writer_impl, indent + 1);
        }
    }

    try writer_impl.setColor(.punctuation);
    try writer_impl.print(")", .{});
    try writer_impl.setColor(.default);
}

/// Write the node as an S-expression formatted string to the given writer.
fn toString(node: SExpr, writer: std.io.AnyWriter, indent: usize) !void {
    var plain_writer = PlainTextSExprWriter{ .writer = writer };
    try node.toStringImpl(&plain_writer, indent);
}

/// Render this S-Expr to a writer with pleasing indentation.
pub fn toStringPretty(node: SExpr, writer: std.io.AnyWriter) void {
    return toString(node, writer, 0) catch {
        @panic("Ran out of memory writing S-expression to writer");
    };
}

/// Render this S-Expr to HTML with syntax highlighting.
pub fn toHtml(node: SExpr, writer: std.io.AnyWriter) void {
    var html_writer = HtmlSExprWriter{ .writer = writer };
    node.toStringImpl(&html_writer, 0) catch {
        @panic("Ran out of memory writing S-expression to HTML writer");
    };
    html_writer.deinit() catch {
        @panic("Error finalizing HTML writer");
    };
}

fn writeIndentImpl(writer_impl: anytype, tabs: usize) !void {
    for (0..tabs) |_| {
        try writer_impl.print("\t", .{});
    }
}

fn writeIndent(writer: std.io.AnyWriter, tabs: usize) !void {
    for (0..tabs) |_| {
        try writer.writeByte('\t');
    }
}

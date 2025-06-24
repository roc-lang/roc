//! A simple format for representing tree-like data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const RegionInfo = @import("RegionInfo.zig");
const exitOnOom = @import("../collections.zig").exitOnOom;

pub const SExpr = @This();

/// name of the s-expression node
value: []const u8,
/// list of attributes `:name=value`
attributes: std.ArrayListUnmanaged(Attribute),
/// list of children nodes
children: std.ArrayListUnmanaged(SExpr),

/// Attribute value can be different types
pub const AttributeValue = union(enum) {
    string: []const u8,
    boolean: bool,
    node_idx: u32,
    region: RegionInfo,
    raw_string: []const u8, // for unquoted strings
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

/// Format the node as an S-expression formatted string
pub fn format(self: SExpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;
    try toStringPretty(self, writer);
}

/// Write the node as an S-expression formatted string to the given writer.
fn toString(node: SExpr, writer: std.io.AnyWriter, indent: usize) !void {

    // Handle regular nodes
    try writer.print("(", .{});
    try writer.print("{s}", .{node.value});

    // Print attributes if present
    for (node.attributes.items) |attr| {
        switch (attr.value) {
            .string => |s| try writer.print(
                " ({s} \"{s}\")",
                .{ attr.key, s },
            ),
            .boolean => |b| try writer.print(
                " ({s} {})",
                .{ attr.key, b },
            ),
            .node_idx => |idx| try writer.print(
                " ({s} {d})",
                .{ attr.key, idx },
            ),
            .raw_string => |s| try writer.print(
                " ({s} {s})",
                .{ attr.key, s },
            ),
            .region => |r| {
                try writer.print(" @{d}-{d}-{d}-{d}", .{
                    // add one to display numbers instead of index
                    r.start_line_idx + 1,
                    r.start_col_idx + 1,
                    r.end_line_idx + 1,
                    r.end_col_idx + 1,
                });
            },
        }
    }

    if (node.children.items.len > 0) {
        for (node.children.items) |child| {
            try writer.print("\n", .{});

            // Print indentation
            try writeIndent(writer, indent + 1);

            try child.toString(writer, indent + 1);
        }
    }

    try writer.print(")", .{});
}

/// Render this S-Expr to a writer with pleasing indentation.
pub fn toStringPretty(node: SExpr, writer: std.io.AnyWriter) void {
    return toString(node, writer, 0) catch {
        @panic("Ran out of memory writing S-expression to writer");
    };
}

fn writeIndent(writer: std.io.AnyWriter, tabs: usize) !void {
    for (0..tabs) |_| {
        try writer.writeByte('\t');
    }
}

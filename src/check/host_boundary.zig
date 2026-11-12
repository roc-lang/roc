//! Read-only validation of host-visible callable values.
//!
//! A function, `Iter`, or `Stream` the host passes or receives must be the
//! immediate payload of a `Box`: a boxed function is a callable the host may
//! invoke, and a boxed sequence is an opaque handle. Any other `Box` payload is
//! held by the host as one allocation, and every function a `Box` holds is an
//! erased callable, so functions anywhere inside such a payload are accepted
//! and their signatures are not host-visible. Sequences have their handle
//! representation only as an immediate `Box` payload, so a held payload still
//! needs each `Iter` and `Stream` directly boxed. Scoped type views preserve
//! every occurrence's position through generic declarations without mutating
//! the solver. Paths are retained as parent links and rendered only on failure.

const std = @import("std");
const types = @import("types");
const base = @import("base");
const TypeView = @import("type_view.zig");

/// Builtin identities whose payload visibility differs from a user nominal.
pub const Builtin = enum { box, list, iter, stream, other };
/// The value that needs its own immediate Box.
pub const Kind = enum { function, iter, stream };
/// A rejected occurrence and its allocator-owned source-level path.
pub const Violation = struct { kind: Kind, path: []u8 };
const Position = enum {
    /// The root of a host-visible declaration's type.
    signature,
    /// A value the host passes, receives, or reads.
    value,
    /// The immediate payload of a `Box`.
    box_payload,
    /// A part of a non-function `Box` payload, which the host only holds.
    held,

    /// The position of a value nested inside a value at this position.
    fn part(self: Position) Position {
        return switch (self) {
            .signature, .value => .value,
            .box_payload, .held => .held,
        };
    }
};
const Key = struct { var_: types.Var, position: Position };
const Part = union(enum) {
    root,
    argument: usize,
    result,
    field: base.Ident.Idx,
    element: usize,
    payload,
    backing,
};
const Node = struct {
    key: Key,
    parent: ?usize,
    part: Part,
};

/// `context` supplies compiler-owned builtin identities and source field names.
/// The returned diagnostic path belongs to `allocator`.
pub fn findViolation(
    allocator: std.mem.Allocator,
    source: *const types.Store,
    root: types.Var,
    signature: bool,
    context: anytype,
) std.mem.Allocator.Error!?Violation {
    var view = TypeView.init(allocator, source);
    defer view.deinit();
    var nodes: std.ArrayList(Node) = .empty;
    defer nodes.deinit(allocator);
    var visited: std.AutoHashMapUnmanaged(Key, void) = .empty;
    defer visited.deinit(allocator);
    try nodes.append(allocator, .{
        .key = .{ .var_ = root, .position = if (signature) .signature else .value },
        .parent = null,
        .part = .root,
    });
    var cursor: usize = 0;
    while (cursor < nodes.items.len) : (cursor += 1) {
        const node = nodes.items[cursor];
        const resolved = try view.resolveVar(node.key.var_);
        const key = Key{ .var_ = resolved.var_, .position = node.key.position };
        if ((try visited.getOrPut(allocator, key)).found_existing) continue;
        switch (resolved.desc.content) {
            .flex, .rigid, .err, .field_presence => {},
            .alias => |alias| try append(allocator, &nodes, cursor, view.getAliasBackingVar(alias), key.position, .backing),
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    switch (key.position) {
                        .value => return .{
                            .kind = .function,
                            .path = try renderPath(allocator, nodes.items, cursor, context),
                        },
                        // The enclosing Box erases this function, and the host
                        // never calls it, so its signature is Roc's alone.
                        .held => continue,
                        .signature, .box_payload => {},
                    }
                    for (view.sliceVars(func.args), 0..) |arg, index|
                        try append(allocator, &nodes, cursor, arg, .value, .{ .argument = index });
                    try append(allocator, &nodes, cursor, func.ret, .value, .result);
                },
                .nominal_type => |nominal| {
                    const builtin = context.builtin(nominal);
                    if (builtin == .iter or builtin == .stream) {
                        if (key.position != .box_payload) return .{
                            .kind = if (builtin == .iter) .iter else .stream,
                            .path = try renderPath(allocator, nodes.items, cursor, context),
                        };
                        // The handle's state, including its item type and step
                        // closure, is private to Roc.
                        continue;
                    }
                    const args = view.sliceNominalArgs(nominal);
                    if (builtin == .box or builtin == .list) {
                        for (args) |arg| try append(allocator, &nodes, cursor, arg, if (builtin == .box) .box_payload else key.position.part(), .payload);
                    } else if (context.hasBacking(nominal)) {
                        if (try view.openNominalBacking(nominal)) |backing|
                            try append(allocator, &nodes, cursor, backing, key.position.part(), .backing);
                    }
                },
                .record => |record| {
                    const fields = view.getRecordFieldsSlice(record.fields);
                    for (fields.items(.name), fields.items(.presence)) |name, presence|
                        try append(allocator, &nodes, cursor, presence.typeVar(), key.position.part(), .{ .field = name });
                    try append(allocator, &nodes, cursor, record.ext, key.position.part(), .backing);
                },
                .tuple => |tuple| {
                    for (view.sliceVars(tuple.elems), 0..) |elem, index|
                        try append(allocator, &nodes, cursor, elem, key.position.part(), .{ .element = index });
                },
                .tag_union => |tags| {
                    const slice = view.getTagsSlice(tags.tags);
                    for (slice.items(.args)) |args| {
                        for (view.sliceVars(args), 0..) |arg, index|
                            try append(allocator, &nodes, cursor, arg, key.position.part(), .{ .element = index });
                    }
                    try append(allocator, &nodes, cursor, tags.ext, key.position.part(), .backing);
                },
                .empty_record, .empty_tag_union => {},
            },
        }
    }
    return null;
}

fn append(allocator: std.mem.Allocator, nodes: *std.ArrayList(Node), parent: usize, var_: types.Var, position: Position, part: Part) std.mem.Allocator.Error!void {
    try nodes.append(allocator, .{ .key = .{ .var_ = var_, .position = position }, .parent = parent, .part = part });
}

fn renderPath(allocator: std.mem.Allocator, nodes: []const Node, end: usize, context: anytype) std.mem.Allocator.Error![]u8 {
    var path: std.ArrayList(Part) = .empty;
    defer path.deinit(allocator);
    var cursor: ?usize = end;
    while (cursor) |index| {
        try path.append(allocator, nodes[index].part);
        cursor = nodes[index].parent;
    }
    var text: std.Io.Writer.Allocating = .init(allocator);
    errdefer text.deinit();
    var index = path.items.len;
    while (index > 0) {
        index -= 1;
        (switch (path.items[index]) {
            .root => text.writer.writeAll("signature"),
            .argument => |arg| text.writer.print(" -> argument {d}", .{arg + 1}),
            .result => text.writer.writeAll(" -> result"),
            .field => |field| text.writer.print(" -> field {s}", .{context.fieldName(field)}),
            .element => |elem| text.writer.print(" -> element {d}", .{elem + 1}),
            .payload => text.writer.writeAll(" -> payload"),
            .backing => {},
        }) catch return error.OutOfMemory;
    }
    return text.toOwnedSlice();
}

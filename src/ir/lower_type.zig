//! Lower executable lambdamono types into the shared logical layout graph that
//! IR carries forward to the single `IR -> LIR/layout` commit boundary.

const std = @import("std");
const mono = @import("lambdamono");
const ir = @import("layout.zig");
const layout_mod = @import("layout");

pub const LayoutCache = struct {
    resolved_by_type: std.AutoHashMap(mono.Type.TypeId, ir.Ref),
    active_by_type: std.AutoHashMap(mono.Type.TypeId, ir.Ref),

    pub fn init(allocator: std.mem.Allocator) LayoutCache {
        return .{
            .resolved_by_type = std.AutoHashMap(mono.Type.TypeId, ir.Ref).init(allocator),
            .active_by_type = std.AutoHashMap(mono.Type.TypeId, ir.Ref).init(allocator),
        };
    }

    pub fn deinit(self: *LayoutCache) void {
        self.active_by_type.deinit();
        self.resolved_by_type.deinit();
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

pub fn lowerType(
    allocator: std.mem.Allocator,
    mono_types: *mono.Type.Store,
    graph: *ir.Graph,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
) std.mem.Allocator.Error!ir.Ref {
    return try lowerTypeRec(allocator, mono_types, graph, cache, ty);
}

fn lowerTypeRec(
    allocator: std.mem.Allocator,
    mono_types: *mono.Type.Store,
    graph: *ir.Graph,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
) std.mem.Allocator.Error!ir.Ref {
    const keyed_ty = try mono_types.keyId(ty);
    if (cache.resolved_by_type.get(keyed_ty)) |cached| return cached;
    if (cache.active_by_type.get(keyed_ty)) |active| return active;

    return switch (mono_types.getTypePreservingNominal(keyed_ty)) {
        .placeholder => debugPanic("ir.lower_type.lowerTypeRec unresolved executable type"),
        .link => unreachable,
        .primitive => |prim| blk: {
            const resolved: ir.Ref = .{ .canonical = lowerPrim(prim) };
            try cache.resolved_by_type.put(keyed_ty, resolved);
            break :blk resolved;
        },
        else => blk: {
            const node_id = try graph.reserveNode(allocator);
            const local_ref: ir.Ref = .{ .local = node_id };
            try cache.active_by_type.put(keyed_ty, local_ref);

            graph.setNode(node_id, try lowerNode(allocator, mono_types, graph, cache, keyed_ty));

            _ = cache.active_by_type.remove(keyed_ty);
            try cache.resolved_by_type.put(keyed_ty, local_ref);
            break :blk local_ref;
        },
    };
}

fn lowerNode(
    allocator: std.mem.Allocator,
    mono_types: *mono.Type.Store,
    graph: *ir.Graph,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
) std.mem.Allocator.Error!ir.Node {
    return switch (mono_types.getTypePreservingNominal(ty)) {
        .placeholder => debugPanic("ir.lower_type.lowerNode unresolved executable type"),
        .link => unreachable,
        .primitive => debugPanic("ir.lower_type.lowerNode primitive should have been returned directly"),
        .nominal => |backing| .{ .nominal = try lowerTypeRec(allocator, mono_types, graph, cache, backing) },
        .list => |elem| .{ .list = try lowerTypeRec(allocator, mono_types, graph, cache, elem) },
        .box => |elem| .{ .box = try lowerTypeRec(allocator, mono_types, graph, cache, elem) },
        .tuple => |tuple| .{ .struct_ = try lowerTupleLikeSpan(allocator, mono_types, graph, cache, mono_types.sliceTypeSpan(tuple)) },
        .record => |record| blk: {
            const fields = mono_types.sliceFields(record.fields);
            var graph_fields = std.ArrayList(ir.Field).empty;
            defer graph_fields.deinit(allocator);
            try graph_fields.ensureTotalCapacity(allocator, fields.len);
            for (fields, 0..) |field, i| {
                graph_fields.appendAssumeCapacity(.{
                    .index = @intCast(i),
                    .child = try lowerTypeRec(allocator, mono_types, graph, cache, field.ty),
                });
            }
            break :blk .{ .struct_ = try graph.appendFields(allocator, graph_fields.items) };
        },
        .tag_union => |tag_union| blk: {
            const tags = mono_types.sliceTags(tag_union.tags);
            var variants = std.ArrayList(ir.Ref).empty;
            defer variants.deinit(allocator);
            try variants.ensureTotalCapacity(allocator, tags.len);
            for (tags) |tag| {
                const args = mono_types.sliceTypeSpan(tag.args);
                if (args.len == 0) {
                    variants.appendAssumeCapacity(.{ .canonical = .zst });
                    continue;
                }

                const payload_node = try graph.reserveNode(allocator);
                const payload_ref: ir.Ref = .{ .local = payload_node };
                variants.appendAssumeCapacity(payload_ref);
                graph.setNode(payload_node, .{ .struct_ = try lowerTupleLikeSpan(allocator, mono_types, graph, cache, args) });
            }
            break :blk .{ .tag_union = try graph.appendRefs(allocator, variants.items) };
        },
    };
}

fn lowerTupleLikeSpan(
    allocator: std.mem.Allocator,
    mono_types: *mono.Type.Store,
    graph: *ir.Graph,
    cache: *LayoutCache,
    elems: []const mono.Type.TypeId,
) std.mem.Allocator.Error!ir.FieldSpan {
    var graph_fields = std.ArrayList(ir.Field).empty;
    defer graph_fields.deinit(allocator);
    try graph_fields.ensureTotalCapacity(allocator, elems.len);
    for (elems, 0..) |elem, i| {
        graph_fields.appendAssumeCapacity(.{
            .index = @intCast(i),
            .child = try lowerTypeRec(allocator, mono_types, graph, cache, elem),
        });
    }
    return try graph.appendFields(allocator, graph_fields.items);
}

fn lowerPrim(prim: mono.Type.Prim) layout_mod.Idx {
    return switch (prim) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .erased => .opaque_ptr,
    };
}

test "ir lower_type tests" {
    std.testing.refAllDecls(@This());
}

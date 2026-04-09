//! Builds the explicit logical executable-layout facts that later IR lowering
//! consumes. This is the one place where `lambdamono.TypeId` is lowered into
//! the shared logical layout graph before `IR -> LIR/layout` commits physical
//! layout exactly once.

const std = @import("std");
const builtin = @import("builtin");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");

const layout_mod = @import("layout");

const LayoutCache = struct {
    resolved_by_type: std.AutoHashMap(type_mod.TypeId, layout_mod.GraphRef),
    active_by_type: std.AutoHashMap(type_mod.TypeId, layout_mod.GraphRef),

    fn init(allocator: std.mem.Allocator) LayoutCache {
        return .{
            .resolved_by_type = std.AutoHashMap(type_mod.TypeId, layout_mod.GraphRef).init(allocator),
            .active_by_type = std.AutoHashMap(type_mod.TypeId, layout_mod.GraphRef).init(allocator),
        };
    }

    fn deinit(self: *LayoutCache) void {
        self.active_by_type.deinit();
        self.resolved_by_type.deinit();
    }
};

pub const Facts = struct {
    graph: layout_mod.Graph,
    cache: LayoutCache,
    type_layouts: std.AutoHashMap(type_mod.TypeId, layout_mod.GraphRef),
    expr_layouts: []layout_mod.GraphRef,
    pat_layouts: []layout_mod.GraphRef,
    typed_symbol_layouts: []layout_mod.GraphRef,
    def_ret_layouts: []layout_mod.GraphRef,
    expr_field_layouts: []?layout_mod.GraphRef,
    expr_discriminant_layouts: []?layout_mod.GraphRef,
    expr_tag_payload_layouts: []?layout_mod.GraphRef,
    pat_tag_payload_layouts: []?layout_mod.GraphRef,

    pub fn initEmpty(
        allocator: std.mem.Allocator,
        store: *ast.Store,
    ) std.mem.Allocator.Error!Facts {
        const facts = Facts{
            .graph = .{},
            .cache = LayoutCache.init(allocator),
            .type_layouts = std.AutoHashMap(type_mod.TypeId, layout_mod.GraphRef).init(allocator),
            .expr_layouts = try allocator.alloc(layout_mod.GraphRef, store.exprs.items.len),
            .pat_layouts = try allocator.alloc(layout_mod.GraphRef, store.pats.items.len),
            .typed_symbol_layouts = try allocator.alloc(layout_mod.GraphRef, store.typed_symbols.items.len),
            .def_ret_layouts = try allocator.alloc(layout_mod.GraphRef, store.defs.items.len),
            .expr_field_layouts = try allocator.alloc(?layout_mod.GraphRef, store.exprs.items.len),
            .expr_discriminant_layouts = try allocator.alloc(?layout_mod.GraphRef, store.exprs.items.len),
            .expr_tag_payload_layouts = try allocator.alloc(?layout_mod.GraphRef, store.exprs.items.len),
            .pat_tag_payload_layouts = try allocator.alloc(?layout_mod.GraphRef, store.pats.items.len),
        };
        errdefer facts.deinit(allocator);

        @memset(facts.expr_field_layouts, null);
        @memset(facts.expr_discriminant_layouts, null);
        @memset(facts.expr_tag_payload_layouts, null);
        @memset(facts.pat_tag_payload_layouts, null);
        return facts;
    }

    pub fn deinit(self: *Facts, allocator: std.mem.Allocator) void {
        allocator.free(self.pat_tag_payload_layouts);
        allocator.free(self.expr_tag_payload_layouts);
        allocator.free(self.expr_discriminant_layouts);
        allocator.free(self.expr_field_layouts);
        allocator.free(self.def_ret_layouts);
        allocator.free(self.typed_symbol_layouts);
        allocator.free(self.pat_layouts);
        allocator.free(self.expr_layouts);
        self.type_layouts.deinit();
        self.cache.deinit();
        self.graph.deinit(allocator);
    }

    pub fn layoutForType(self: *const Facts, ty: type_mod.TypeId) layout_mod.GraphRef {
        return self.type_layouts.get(ty) orelse debugPanic("lambdamono.layout_facts.layoutForType missing lowered type");
    }

    pub fn exprLayout(self: *const Facts, expr_id: ast.ExprId) layout_mod.GraphRef {
        return self.expr_layouts[@intFromEnum(expr_id)];
    }

    pub fn patLayout(self: *const Facts, pat_id: ast.PatId) layout_mod.GraphRef {
        return self.pat_layouts[@intFromEnum(pat_id)];
    }

    pub fn typedSymbolLayout(self: *const Facts, typed_symbol_idx: usize) layout_mod.GraphRef {
        return self.typed_symbol_layouts[typed_symbol_idx];
    }

    pub fn defRetLayout(self: *const Facts, def_id: ast.DefId) layout_mod.GraphRef {
        return self.def_ret_layouts[@intFromEnum(def_id)];
    }

    pub fn exprFieldLayout(self: *const Facts, expr_id: ast.ExprId) layout_mod.GraphRef {
        return self.expr_field_layouts[@intFromEnum(expr_id)] orelse
            debugPanic("lambdamono.layout_facts.exprFieldLayout missing explicit field layout");
    }

    pub fn exprDiscriminantLayout(self: *const Facts, expr_id: ast.ExprId) ?layout_mod.GraphRef {
        return self.expr_discriminant_layouts[@intFromEnum(expr_id)];
    }

    pub fn exprTagPayloadLayout(self: *const Facts, expr_id: ast.ExprId) layout_mod.GraphRef {
        return self.expr_tag_payload_layouts[@intFromEnum(expr_id)] orelse
            debugPanic("lambdamono.layout_facts.exprTagPayloadLayout missing explicit payload layout");
    }

    pub fn patTagPayloadLayout(self: *const Facts, pat_id: ast.PatId) layout_mod.GraphRef {
        return self.pat_tag_payload_layouts[@intFromEnum(pat_id)] orelse
            debugPanic("lambdamono.layout_facts.patTagPayloadLayout missing explicit payload layout");
    }

    pub fn recordTypedSymbol(
        self: *Facts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        index: usize,
        value: ast.TypedSymbol,
    ) std.mem.Allocator.Error!void {
        self.typed_symbol_layouts[index] = try self.layoutForPublishedType(allocator, mono_types, value.ty);
    }

    pub fn recordExpr(
        self: *Facts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        store: *ast.Store,
        expr_id: ast.ExprId,
        expr: ast.Expr,
    ) std.mem.Allocator.Error!void {
        const i = @intFromEnum(expr_id);
        self.expr_layouts[i] = try self.layoutForPublishedType(allocator, mono_types, expr.ty);
        self.expr_discriminant_layouts[i] = try self.maybeDiscriminantLayout(self.exprLayout(expr_id));
        switch (expr.data) {
            .tag => |tag| {
                if (store.sliceExprSpan(tag.args).len == 0) return;
                self.expr_tag_payload_layouts[i] = try self.unionPayloadLayout(self.exprLayout(expr_id), tag.discriminant);
            },
            .access => |access| {
                self.expr_field_layouts[i] = try self.structFieldLayout(
                    self.exprLayout(access.record),
                    access.field_index,
                );
            },
            .tuple_access => |tuple_access| {
                self.expr_field_layouts[i] = try self.structFieldLayout(
                    self.exprLayout(tuple_access.tuple),
                    @intCast(tuple_access.elem_index),
                );
            },
            else => {},
        }
    }

    fn maybeDiscriminantLayout(
        self: *Facts,
        layout_ref: layout_mod.GraphRef,
    ) std.mem.Allocator.Error!?layout_mod.GraphRef {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => return null,
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    .tag_union => |variants| {
                        const variant_count = self.graph.getRefs(variants).len;
                        const prim: layout_mod.Idx = switch (variant_count) {
                            0...0xff => .u8,
                            0x100...0xffff => .u16,
                            0x1_0000...0xffff_ffff => .u32,
                            else => .u64,
                        };
                        return .{ .canonical = prim };
                    },
                    else => return null,
                },
            }
        }
    }

    pub fn recordPat(
        self: *Facts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        store: *ast.Store,
        pat_id: ast.PatId,
        pat: ast.Pat,
    ) std.mem.Allocator.Error!void {
        const i = @intFromEnum(pat_id);
        self.pat_layouts[i] = try self.layoutForPublishedType(allocator, mono_types, pat.ty);
        switch (pat.data) {
            .tag => |tag| {
                if (store.slicePatSpan(tag.args).len == 0) return;
                self.pat_tag_payload_layouts[i] = try self.unionPayloadLayout(self.patLayout(pat_id), tag.discriminant);
            },
            else => {},
        }
    }

    pub fn recordDefRet(
        self: *Facts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        def_id: ast.DefId,
        ret_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        self.def_ret_layouts[@intFromEnum(def_id)] = try self.layoutForPublishedType(allocator, mono_types, ret_ty);
    }

    fn layoutForPublishedType(
        self: *Facts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        if (self.type_layouts.get(ty)) |existing| return existing;
        const lowered = try lowerType(allocator, mono_types, &self.graph, &self.cache, ty);
        try self.type_layouts.put(ty, lowered);
        return lowered;
    }

    fn unionPayloadLayout(
        self: *Facts,
        union_layout: layout_mod.GraphRef,
        discriminant: u16,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        const resolved = try self.resolveUnionLayout(union_layout);
        return switch (resolved) {
            .canonical => debugPanic("lambdamono.layout_facts.unionPayloadLayout expected local union layout"),
            .local => |node_id| switch (self.graph.getNode(node_id)) {
                .tag_union => |variants| self.graph.getRefs(variants)[discriminant],
                else => debugPanic("lambdamono.layout_facts.unionPayloadLayout expected tag union layout"),
            },
        };
    }

    pub fn structFieldLayout(
        self: *Facts,
        struct_layout: layout_mod.GraphRef,
        field_index: u16,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        const resolved = try self.resolveStructLayout(struct_layout);
        return switch (resolved) {
            .canonical => debugPanic("lambdamono.layout_facts.structFieldLayout expected local struct layout"),
            .local => |node_id| switch (self.graph.getNode(node_id)) {
                .struct_ => |fields| self.graph.getFields(fields)[field_index].child,
                else => debugPanic("lambdamono.layout_facts.structFieldLayout expected struct layout"),
            },
        };
    }

    fn resolveUnionLayout(self: *Facts, layout_ref: layout_mod.GraphRef) std.mem.Allocator.Error!layout_mod.GraphRef {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => debugPanic("lambdamono.layout_facts.resolveUnionLayout expected local layout"),
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    .tag_union => return current,
                    else => debugPanic("lambdamono.layout_facts.resolveUnionLayout expected nominal or tag union"),
                },
            }
        }
    }

    fn resolveStructLayout(self: *Facts, layout_ref: layout_mod.GraphRef) std.mem.Allocator.Error!layout_mod.GraphRef {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => debugPanic("lambdamono.layout_facts.resolveStructLayout expected local layout"),
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    .struct_ => return current,
                    else => debugPanic("lambdamono.layout_facts.resolveStructLayout expected nominal or struct"),
                },
            }
        }
    }
};

fn lowerType(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    graph: *layout_mod.Graph,
    cache: *LayoutCache,
    ty: type_mod.TypeId,
) std.mem.Allocator.Error!layout_mod.GraphRef {
    return try lowerTypeRec(allocator, mono_types, graph, cache, ty);
}

fn lowerTypeRec(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    graph: *layout_mod.Graph,
    cache: *LayoutCache,
    ty: type_mod.TypeId,
) std.mem.Allocator.Error!layout_mod.GraphRef {
    const keyed_ty = ty;
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(try mono_types.keyId(ty) == ty);
    }
    if (cache.resolved_by_type.get(keyed_ty)) |cached| return cached;
    if (cache.active_by_type.get(keyed_ty)) |active| return active;

    return switch (mono_types.getTypePreservingNominal(keyed_ty)) {
        .placeholder => debugPanic("lambdamono.layout_facts.lowerTypeRec unresolved executable type"),
        .link => unreachable,
        .primitive => |prim| blk: {
            const resolved: layout_mod.GraphRef = .{ .canonical = lowerPrim(prim) };
            try cache.resolved_by_type.put(keyed_ty, resolved);
            break :blk resolved;
        },
        else => blk: {
            const node_id = try graph.reserveNode(allocator);
            const local_ref: layout_mod.GraphRef = .{ .local = node_id };
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
    mono_types: *type_mod.Store,
    graph: *layout_mod.Graph,
    cache: *LayoutCache,
    ty: type_mod.TypeId,
) std.mem.Allocator.Error!layout_mod.GraphNode {
    return switch (mono_types.getTypePreservingNominal(ty)) {
        .placeholder => debugPanic("lambdamono.layout_facts.lowerNode unresolved executable type"),
        .link => unreachable,
        .primitive => debugPanic("lambdamono.layout_facts.lowerNode primitive should have been returned directly"),
        .nominal => |backing| .{ .nominal = try lowerTypeRec(allocator, mono_types, graph, cache, backing) },
        .list => |elem| .{ .list = try lowerTypeRec(allocator, mono_types, graph, cache, elem) },
        .box => |elem| .{ .box = try lowerTypeRec(allocator, mono_types, graph, cache, elem) },
        .tuple => |tuple| .{ .struct_ = try lowerTupleLikeSpan(allocator, mono_types, graph, cache, mono_types.sliceTypeSpan(tuple)) },
        .record => |record| blk: {
            const fields = mono_types.sliceFields(record.fields);
            var graph_fields = std.ArrayList(layout_mod.GraphField).empty;
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
            var variants = std.ArrayList(layout_mod.GraphRef).empty;
            defer variants.deinit(allocator);
            try variants.ensureTotalCapacity(allocator, tags.len);
            for (tags) |tag| {
                const args = mono_types.sliceTypeSpan(tag.args);
                if (args.len == 0) {
                    variants.appendAssumeCapacity(.{ .canonical = .zst });
                    continue;
                }

                const payload_node = try graph.reserveNode(allocator);
                const payload_ref: layout_mod.GraphRef = .{ .local = payload_node };
                variants.appendAssumeCapacity(payload_ref);
                graph.setNode(payload_node, .{ .struct_ = try lowerTupleLikeSpan(allocator, mono_types, graph, cache, args) });
            }
            break :blk .{ .tag_union = try graph.appendRefs(allocator, variants.items) };
        },
    };
}

fn lowerTupleLikeSpan(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    graph: *layout_mod.Graph,
    cache: *LayoutCache,
    elems: []const type_mod.TypeId,
) std.mem.Allocator.Error!layout_mod.GraphFieldSpan {
    var graph_fields = std.ArrayList(layout_mod.GraphField).empty;
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

fn lowerPrim(prim: type_mod.Prim) layout_mod.Idx {
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

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

test "layout facts tests" {
    std.testing.refAllDecls(@This());
}

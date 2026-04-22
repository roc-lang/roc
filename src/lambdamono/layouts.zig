//! Builds the explicit logical executable layouts that later IR lowering
//! consumes. This is the one place where `lambdamono.TypeId` is lowered into
//! the shared logical layout graph before `IR -> LIR/layout` commits physical
//! layout exactly once.

const std = @import("std");
const builtin = @import("builtin");
const ast = @import("ast.zig");
const base = @import("base");
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

fn containsLayoutAbstractLeaf(mono_types: *type_mod.Store, ty: type_mod.TypeId) bool {
    var visited = std.AutoHashMap(type_mod.TypeId, void).init(mono_types.allocator);
    defer visited.deinit();
    return containsLayoutAbstractLeafVisited(mono_types, ty, &visited) catch true;
}

fn containsLayoutAbstractLeafVisited(
    mono_types: *type_mod.Store,
    ty: type_mod.TypeId,
    visited: *std.AutoHashMap(type_mod.TypeId, void),
) std.mem.Allocator.Error!bool {
    var root = ty;
    while (true) switch (mono_types.types.items[@intFromEnum(root)]) {
        .link => |next| root = next,
        else => break,
    };
    if (visited.contains(root)) return false;
    try visited.put(root, {});

    return switch (mono_types.types.items[@intFromEnum(root)]) {
        .placeholder, .unbd => true,
        .link => unreachable,
        .primitive => false,
        .nominal => |nominal| try containsLayoutAbstractLeafVisited(mono_types, nominal.backing, visited),
        .list => |elem| try containsLayoutAbstractLeafVisited(mono_types, elem, visited),
        .box => |elem| try containsLayoutAbstractLeafVisited(mono_types, elem, visited),
        .erased_fn => |erased_fn| blk: {
            if (erased_fn.capture) |capture| {
                if (try containsLayoutAbstractLeafVisited(mono_types, capture, visited)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |tuple| blk: {
            for (tuple) |elem| {
                if (try containsLayoutAbstractLeafVisited(mono_types, elem, visited)) break :blk true;
            }
            break :blk false;
        },
        .record => |record| blk: {
            for (record.fields) |field| {
                if (try containsLayoutAbstractLeafVisited(mono_types, field.ty, visited)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tag_union| blk: {
            for (tag_union.tags) |tag| {
                for (tag.args) |arg| {
                    if (try containsLayoutAbstractLeafVisited(mono_types, arg, visited)) break :blk true;
                }
            }
            break :blk false;
        },
    };
}

/// Explicit logical layouts derived from executable lambdamono types and nodes.
pub const Layouts = struct {
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
    pat_source_layouts: []?layout_mod.GraphRef,

    /// Allocate empty layout tables sized for an executable store.
    pub fn initEmpty(
        allocator: std.mem.Allocator,
        store: *ast.Store,
    ) std.mem.Allocator.Error!Layouts {
        const layouts = Layouts{
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
            .pat_source_layouts = try allocator.alloc(?layout_mod.GraphRef, store.pats.items.len),
        };
        errdefer layouts.deinit(allocator);

        @memset(layouts.expr_field_layouts, null);
        @memset(layouts.expr_discriminant_layouts, null);
        @memset(layouts.expr_tag_payload_layouts, null);
        @memset(layouts.pat_tag_payload_layouts, null);
        @memset(layouts.pat_source_layouts, null);
        return layouts;
    }

    pub fn deinit(self: *Layouts, allocator: std.mem.Allocator) void {
        allocator.free(self.pat_source_layouts);
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

    pub fn layoutForType(self: *const Layouts, ty: type_mod.TypeId) layout_mod.GraphRef {
        return self.type_layouts.get(ty) orelse debugPanic("lambdamono.layouts.layoutForType missing lowered type");
    }

    pub fn exprLayout(self: *const Layouts, expr_id: ast.ExprId) layout_mod.GraphRef {
        return self.expr_layouts[@intFromEnum(expr_id)];
    }

    pub fn patLayout(self: *const Layouts, pat_id: ast.PatId) layout_mod.GraphRef {
        return self.pat_layouts[@intFromEnum(pat_id)];
    }

    pub fn typedSymbolLayout(self: *const Layouts, typed_symbol_idx: usize) layout_mod.GraphRef {
        return self.typed_symbol_layouts[typed_symbol_idx];
    }

    pub fn defRetLayout(self: *const Layouts, def_id: ast.DefId) layout_mod.GraphRef {
        return self.def_ret_layouts[@intFromEnum(def_id)];
    }

    pub fn exprFieldLayout(self: *const Layouts, expr_id: ast.ExprId) layout_mod.GraphRef {
        return self.expr_field_layouts[@intFromEnum(expr_id)] orelse
            debugPanic("lambdamono.layouts.exprFieldLayout missing explicit field layout");
    }

    pub fn exprDiscriminantLayout(self: *const Layouts, expr_id: ast.ExprId) ?layout_mod.GraphRef {
        return self.expr_discriminant_layouts[@intFromEnum(expr_id)];
    }

    pub fn exprTagPayloadLayout(self: *const Layouts, expr_id: ast.ExprId) layout_mod.GraphRef {
        return self.expr_tag_payload_layouts[@intFromEnum(expr_id)] orelse
            debugPanic("lambdamono.layouts.exprTagPayloadLayout missing explicit payload layout");
    }

    pub fn payloadLayoutForUnionLayout(
        self: *Layouts,
        union_layout: layout_mod.GraphRef,
        discriminant: u16,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        return self.unionPayloadLayout(union_layout, discriminant);
    }

    pub fn patTagPayloadLayout(self: *const Layouts, pat_id: ast.PatId) layout_mod.GraphRef {
        return self.pat_tag_payload_layouts[@intFromEnum(pat_id)] orelse
            debugPanic("lambdamono.layouts.patTagPayloadLayout missing explicit payload layout");
    }

    pub fn patSourceLayout(self: *const Layouts, pat_id: ast.PatId) layout_mod.GraphRef {
        return self.pat_source_layouts[@intFromEnum(pat_id)] orelse
            debugPanic("lambdamono.layouts.patSourceLayout missing explicit source layout");
    }

    pub fn recordTypedSymbol(
        self: *Layouts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        idents: *const base.Ident.Store,
        index: usize,
        value: ast.TypedSymbol,
    ) std.mem.Allocator.Error!void {
        if (containsLayoutAbstractLeaf(mono_types, value.ty)) {
            debugPanicFmt(
                "lambdamono.layouts.recordTypedSymbol abstract executable type leaked before layout typed_symbol={d} symbol={d} ty={d} ({s})",
                .{
                    index,
                    value.symbol.raw(),
                    @intFromEnum(value.ty),
                    @tagName(mono_types.getTypePreservingNominal(value.ty)),
                },
            );
        }
        self.typed_symbol_layouts[index] = try self.layoutForExecutableType(allocator, mono_types, idents, value.ty);
    }

    pub fn recordType(
        self: *Layouts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        idents: *const base.Ident.Store,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        const layout_ref = try self.layoutForExecutableType(allocator, mono_types, idents, ty);
        try self.type_layouts.put(ty, layout_ref);
    }

    pub fn recordExpr(
        self: *Layouts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        idents: *const base.Ident.Store,
        store: *ast.Store,
        expr_id: ast.ExprId,
        expr: ast.Expr,
    ) std.mem.Allocator.Error!void {
        const i = @intFromEnum(expr_id);
        if (containsLayoutAbstractLeaf(mono_types, expr.ty)) {
            if (expr.data == .low_level) {
                debugPanicFmt(
                    "lambdamono.layouts.recordExpr abstract executable type leaked before layout expr={d} tag={s} ty={d} ({s}) low_level={s}",
                    .{
                        i,
                        @tagName(expr.data),
                        @intFromEnum(expr.ty),
                        @tagName(mono_types.getTypePreservingNominal(expr.ty)),
                        @tagName(expr.data.low_level.op),
                    },
                );
            }
            debugPanicFmt(
                "lambdamono.layouts.recordExpr abstract executable type leaked before layout expr={d} tag={s} ty={d} ({s})",
                .{
                    i,
                    @tagName(expr.data),
                    @intFromEnum(expr.ty),
                    @tagName(mono_types.getTypePreservingNominal(expr.ty)),
                },
            );
        }
        self.expr_layouts[i] = try self.layoutForExecutableType(allocator, mono_types, idents, expr.ty);
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
            .tag_payload => |tag_payload| {
                self.expr_tag_payload_layouts[i] = try self.unionPayloadLayout(
                    self.exprLayout(tag_payload.tag_union),
                    tag_payload.tag_discriminant,
                );
                self.expr_field_layouts[i] = try self.structFieldLayout(
                    self.expr_tag_payload_layouts[i].?,
                    tag_payload.payload_index,
                );
            },
            .packed_fn => |packed_fn| {
                if (packed_fn.capture_ty) |capture_ty| {
                    try self.recordType(allocator, mono_types, idents, capture_ty);
                    const capture_box_ty = try mono_types.internResolved(.{ .box = capture_ty });
                    try self.recordType(allocator, mono_types, idents, capture_box_ty);
                } else {
                    const unit_ty = try mono_types.internResolved(.{ .record = .{
                        .fields = &.{},
                    } });
                    try self.recordType(allocator, mono_types, idents, unit_ty);
                    const capture_box_ty = try mono_types.internResolved(.{ .box = unit_ty });
                    try self.recordType(allocator, mono_types, idents, capture_box_ty);
                }
            },
            .call_erased => |call| {
                if (call.capture_ty) |capture_ty| {
                    try self.recordType(allocator, mono_types, idents, capture_ty);
                }
            },
            else => {},
        }
    }

    fn maybeDiscriminantLayout(
        self: *Layouts,
        layout_ref: layout_mod.GraphRef,
    ) std.mem.Allocator.Error!?layout_mod.GraphRef {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => return null,
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .tag_union => |variants| {
                        const variant_count = self.graph.getRefs(variants).len;
                        const prim: layout_mod.Idx = if (@bitSizeOf(usize) <= 32) switch (variant_count) {
                            0...0xff => .u8,
                            0x100...0xffff => .u16,
                            0x1_0000...std.math.maxInt(u32) => .u32,
                        } else switch (variant_count) {
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
        self: *Layouts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        idents: *const base.Ident.Store,
        store: *ast.Store,
        pat_id: ast.PatId,
        pat: ast.Pat,
    ) std.mem.Allocator.Error!void {
        const i = @intFromEnum(pat_id);
        if (containsLayoutAbstractLeaf(mono_types, pat.ty)) {
            debugPanicFmt(
                "lambdamono.layouts.recordPat abstract executable type leaked before layout pat={d} tag={s} ty={d} ({s})",
                .{
                    i,
                    @tagName(pat.data),
                    @intFromEnum(pat.ty),
                    @tagName(mono_types.getTypePreservingNominal(pat.ty)),
                },
            );
        }
        self.pat_layouts[i] = try self.layoutForExecutableType(allocator, mono_types, idents, pat.ty);
        switch (pat.data) {
            .tag => |tag| {
                const args = store.slicePatSpan(tag.args);
                if (args.len == 0) return;
                self.pat_tag_payload_layouts[i] = try self.unionPayloadLayout(self.patLayout(pat_id), tag.discriminant);
                for (args, 0..) |arg_pat_id, field_index| {
                    self.pat_source_layouts[@intFromEnum(arg_pat_id)] = try self.structFieldLayout(
                        self.pat_tag_payload_layouts[i].?,
                        @intCast(field_index),
                    );
                }
            },
            else => {},
        }
    }

    pub fn recordDefRet(
        self: *Layouts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        idents: *const base.Ident.Store,
        def_id: ast.DefId,
        ret_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!void {
        self.def_ret_layouts[@intFromEnum(def_id)] = try self.layoutForExecutableType(allocator, mono_types, idents, ret_ty);
    }

    fn unwrapNominalRef(self: *Layouts, ref: layout_mod.GraphRef) layout_mod.GraphRef {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => return current,
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    else => return current,
                },
            }
        }
    }

    fn layoutForExecutableType(
        self: *Layouts,
        allocator: std.mem.Allocator,
        mono_types: *type_mod.Store,
        idents: *const base.Ident.Store,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        if (self.type_layouts.get(ty)) |existing| return existing;
        const lowered = try lowerType(allocator, mono_types, idents, &self.graph, &self.cache, ty);
        try self.type_layouts.put(ty, lowered);
        return lowered;
    }

    fn unionPayloadLayout(
        self: *Layouts,
        union_layout: layout_mod.GraphRef,
        discriminant: u16,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        const resolved = try self.resolveUnionLayout(union_layout);
        return switch (resolved) {
            .canonical => debugPanic("lambdamono.layouts.unionPayloadLayout expected local union layout"),
            .local => |node_id| switch (self.graph.getNode(node_id)) {
                .tag_union => |variants| self.graph.getRefs(variants)[discriminant],
                else => debugPanic("lambdamono.layouts.unionPayloadLayout expected tag union layout"),
            },
        };
    }

    pub fn structFieldLayout(
        self: *Layouts,
        struct_layout: layout_mod.GraphRef,
        field_index: u16,
    ) std.mem.Allocator.Error!layout_mod.GraphRef {
        const resolved = try self.resolveStructLayout(struct_layout);
        return switch (resolved) {
            .canonical => debugPanic("lambdamono.layouts.structFieldLayout expected local struct layout"),
            .local => |node_id| switch (self.graph.getNode(node_id)) {
                .struct_ => |fields| self.unwrapNominalRef(self.graph.getFields(fields)[field_index].child),
                else => debugPanic("lambdamono.layouts.structFieldLayout expected struct layout"),
            },
        };
    }

    fn resolveUnionLayout(self: *Layouts, layout_ref: layout_mod.GraphRef) std.mem.Allocator.Error!layout_mod.GraphRef {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => debugPanic("lambdamono.layouts.resolveUnionLayout expected local layout"),
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .tag_union => return current,
                    else => debugPanic("lambdamono.layouts.resolveUnionLayout expected nominal or tag union"),
                },
            }
        }
    }

    fn resolveStructLayout(self: *Layouts, layout_ref: layout_mod.GraphRef) std.mem.Allocator.Error!layout_mod.GraphRef {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => debugPanic("lambdamono.layouts.resolveStructLayout expected local layout"),
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .struct_ => return current,
                    else => debugPanic("lambdamono.layouts.resolveStructLayout expected nominal or struct"),
                },
            }
        }
    }
};

fn lowerType(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    idents: *const base.Ident.Store,
    graph: *layout_mod.Graph,
    cache: *LayoutCache,
    ty: type_mod.TypeId,
) std.mem.Allocator.Error!layout_mod.GraphRef {
    return try lowerTypeRec(allocator, mono_types, idents, graph, cache, ty);
}

fn lowerTypeRec(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    idents: *const base.Ident.Store,
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
        .placeholder => debugPanic("lambdamono.layouts.lowerTypeRec unresolved executable type"),
        .unbd => debugPanic("lambdamono.layouts.lowerTypeRec abstract executable type leaked to layouts"),
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

            graph.setNode(node_id, try lowerNode(allocator, mono_types, idents, graph, cache, keyed_ty));

            const removed = cache.active_by_type.remove(keyed_ty);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(removed);
            } else if (!removed) {
                unreachable;
            }
            try cache.resolved_by_type.put(keyed_ty, local_ref);
            break :blk local_ref;
        },
    };
}

fn lowerNode(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    idents: *const base.Ident.Store,
    graph: *layout_mod.Graph,
    cache: *LayoutCache,
    ty: type_mod.TypeId,
) std.mem.Allocator.Error!layout_mod.GraphNode {
    return switch (mono_types.getTypePreservingNominal(ty)) {
        .placeholder => debugPanic("lambdamono.layouts.lowerNode unresolved executable type"),
        .unbd => debugPanic("lambdamono.layouts.lowerNode abstract executable type leaked to layouts"),
        .link => unreachable,
        .primitive => debugPanic("lambdamono.layouts.lowerNode primitive should have been returned directly"),
        .nominal => |nominal| .{ .nominal = try lowerTypeRec(allocator, mono_types, idents, graph, cache, nominal.backing) },
        .list => |elem| .{ .list = try lowerTypeRec(allocator, mono_types, idents, graph, cache, elem) },
        .box => |elem| .{ .box = try lowerTypeRec(allocator, mono_types, idents, graph, cache, elem) },
        .erased_fn => |erased_fn| blk: {
            const capture_ty = erased_fn.capture orelse blk_capture: {
                const unit_ty = try mono_types.internResolved(.{ .record = .{ .fields = &.{} } });
                break :blk_capture unit_ty;
            };
            const capture_box_ty = try mono_types.internResolved(.{ .box = capture_ty });
            var fields = [_]layout_mod.GraphField{
                .{ .index = 0, .child = .{ .canonical = .opaque_ptr } },
                .{ .index = 1, .child = try lowerTypeRec(allocator, mono_types, idents, graph, cache, capture_box_ty) },
                .{ .index = 2, .child = .{ .canonical = .u32 } },
            };
            break :blk .{ .struct_ = try graph.appendFields(allocator, &fields) };
        },
        .tuple => |tuple| .{ .struct_ = try lowerTupleLikeSpan(allocator, mono_types, idents, graph, cache, tuple) },
        .record => |record| blk: {
            const fields = record.fields;
            var graph_fields = std.ArrayList(layout_mod.GraphField).empty;
            defer graph_fields.deinit(allocator);
            try graph_fields.ensureTotalCapacity(allocator, fields.len);
            for (fields, 0..) |field, i| {
                graph_fields.appendAssumeCapacity(.{
                    .index = @intCast(i),
                    .child = try lowerTypeRec(allocator, mono_types, idents, graph, cache, field.ty),
                });
            }
            break :blk .{ .struct_ = try graph.appendFields(allocator, graph_fields.items) };
        },
        .tag_union => |tag_union| blk: {
            const tags = tag_union.tags;
            var variants = std.ArrayList(layout_mod.GraphRef).empty;
            defer variants.deinit(allocator);
            try variants.ensureTotalCapacity(allocator, tags.len);
            for (tags) |tag| {
                const args = tag.args;
                if (args.len == 0) {
                    variants.appendAssumeCapacity(.{ .canonical = .zst });
                    continue;
                }

                const payload_node = try graph.reserveNode(allocator);
                const payload_ref: layout_mod.GraphRef = .{ .local = payload_node };
                variants.appendAssumeCapacity(payload_ref);
                graph.setNode(payload_node, .{ .struct_ = try lowerTupleLikeSpan(allocator, mono_types, idents, graph, cache, args) });
            }
            break :blk .{ .tag_union = try graph.appendRefs(allocator, variants.items) };
        },
    };
}

fn lowerTupleLikeSpan(
    allocator: std.mem.Allocator,
    mono_types: *type_mod.Store,
    idents: *const base.Ident.Store,
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
            .child = try lowerTypeRec(allocator, mono_types, idents, graph, cache, elem),
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

fn debugPanicFmt(comptime msg: []const u8, args: anytype) noreturn {
    @branchHint(.cold);
    std.debug.panic(msg, args);
}

test "layouts tests" {
    std.testing.refAllDecls(@This());
}

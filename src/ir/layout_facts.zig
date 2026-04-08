//! Builds the explicit logical executable-layout facts that IR lowering
//! consumes. This is the one place where `lambdamono.TypeId` is lowered into
//! the shared logical layout graph before `IR -> LIR/layout` commits physical
//! layout exactly once.

const std = @import("std");
const lambdamono = @import("lambdamono");
const ir_layout = @import("layout.zig");
const lower_type = @import("lower_type.zig");

const layout_mod = @import("layout");

pub const Facts = struct {
    graph: ir_layout.Graph,
    cache: lower_type.LayoutCache,
    type_layouts: std.AutoHashMap(lambdamono.Type.TypeId, ir_layout.Ref),
    expr_layouts: []ir_layout.Ref,
    pat_layouts: []ir_layout.Ref,
    typed_symbol_layouts: []ir_layout.Ref,
    def_ret_layouts: []ir_layout.Ref,
    expr_field_layouts: []?ir_layout.Ref,
    expr_tag_payload_layouts: []?ir_layout.Ref,
    pat_tag_payload_layouts: []?ir_layout.Ref,

    pub fn init(
        allocator: std.mem.Allocator,
        input: *lambdamono.Lower.Result,
    ) std.mem.Allocator.Error!Facts {
        var facts = Facts{
            .graph = .{},
            .cache = lower_type.LayoutCache.init(allocator),
            .type_layouts = std.AutoHashMap(lambdamono.Type.TypeId, ir_layout.Ref).init(allocator),
            .expr_layouts = try allocator.alloc(ir_layout.Ref, input.store.exprs.items.len),
            .pat_layouts = try allocator.alloc(ir_layout.Ref, input.store.pats.items.len),
            .typed_symbol_layouts = try allocator.alloc(ir_layout.Ref, input.store.typed_symbols.items.len),
            .def_ret_layouts = try allocator.alloc(ir_layout.Ref, input.store.defs.items.len),
            .expr_field_layouts = try allocator.alloc(?ir_layout.Ref, input.store.exprs.items.len),
            .expr_tag_payload_layouts = try allocator.alloc(?ir_layout.Ref, input.store.exprs.items.len),
            .pat_tag_payload_layouts = try allocator.alloc(?ir_layout.Ref, input.store.pats.items.len),
        };
        errdefer facts.deinit(allocator);

        @memset(facts.expr_field_layouts, null);
        @memset(facts.expr_tag_payload_layouts, null);
        @memset(facts.pat_tag_payload_layouts, null);

        try facts.lowerAllTypes(allocator, input);
        try facts.attachDerivedExprFacts(input);
        try facts.attachDerivedPatFacts(input);
        return facts;
    }

    pub fn deinit(self: *Facts, allocator: std.mem.Allocator) void {
        allocator.free(self.pat_tag_payload_layouts);
        allocator.free(self.expr_tag_payload_layouts);
        allocator.free(self.expr_field_layouts);
        allocator.free(self.def_ret_layouts);
        allocator.free(self.typed_symbol_layouts);
        allocator.free(self.pat_layouts);
        allocator.free(self.expr_layouts);
        self.type_layouts.deinit();
        self.cache.deinit();
        self.graph.deinit(allocator);
    }

    pub fn layoutForType(self: *const Facts, ty: lambdamono.Type.TypeId) ir_layout.Ref {
        return self.type_layouts.get(ty) orelse debugPanic("ir.layout_facts.layoutForType missing lowered type");
    }

    pub fn exprLayout(self: *const Facts, expr_id: lambdamono.Ast.ExprId) ir_layout.Ref {
        return self.expr_layouts[@intFromEnum(expr_id)];
    }

    pub fn patLayout(self: *const Facts, pat_id: lambdamono.Ast.PatId) ir_layout.Ref {
        return self.pat_layouts[@intFromEnum(pat_id)];
    }

    pub fn typedSymbolLayout(self: *const Facts, typed_symbol_idx: usize) ir_layout.Ref {
        return self.typed_symbol_layouts[typed_symbol_idx];
    }

    pub fn defRetLayout(self: *const Facts, def_id: lambdamono.Ast.DefId) ir_layout.Ref {
        return self.def_ret_layouts[@intFromEnum(def_id)];
    }

    pub fn exprFieldLayout(self: *const Facts, expr_id: lambdamono.Ast.ExprId) ir_layout.Ref {
        return self.expr_field_layouts[@intFromEnum(expr_id)] orelse
            debugPanic("ir.layout_facts.exprFieldLayout missing explicit field layout");
    }

    pub fn exprTagPayloadLayout(self: *const Facts, expr_id: lambdamono.Ast.ExprId) ir_layout.Ref {
        return self.expr_tag_payload_layouts[@intFromEnum(expr_id)] orelse
            debugPanic("ir.layout_facts.exprTagPayloadLayout missing explicit payload layout");
    }

    pub fn patTagPayloadLayout(self: *const Facts, pat_id: lambdamono.Ast.PatId) ir_layout.Ref {
        return self.pat_tag_payload_layouts[@intFromEnum(pat_id)] orelse
            debugPanic("ir.layout_facts.patTagPayloadLayout missing explicit payload layout");
    }

    fn lowerAllTypes(
        self: *Facts,
        allocator: std.mem.Allocator,
        input: *lambdamono.Lower.Result,
    ) std.mem.Allocator.Error!void {
        for (input.store.exprs.items, 0..) |expr, i| {
            self.expr_layouts[i] = try self.lowerAndRecordType(allocator, &input.types, expr.ty);
        }
        for (input.store.pats.items, 0..) |pat, i| {
            self.pat_layouts[i] = try self.lowerAndRecordType(allocator, &input.types, pat.ty);
        }
        for (input.store.typed_symbols.items, 0..) |value, i| {
            self.typed_symbol_layouts[i] = try self.lowerAndRecordType(allocator, &input.types, value.ty);
        }
        for (input.store.defs.items, 0..) |def, i| {
            const ret_ty = switch (def.value) {
                .fn_ => |fn_def| input.store.getExpr(fn_def.body).ty,
                .val => |expr_id| def.result_ty orelse input.store.getExpr(expr_id).ty,
                .run => |run_def| def.result_ty orelse input.store.getExpr(run_def.body).ty,
            };
            self.def_ret_layouts[i] = try self.lowerAndRecordType(allocator, &input.types, ret_ty);
        }
    }

    fn lowerAndRecordType(
        self: *Facts,
        allocator: std.mem.Allocator,
        mono_types: *lambdamono.Type.Store,
        ty: lambdamono.Type.TypeId,
    ) std.mem.Allocator.Error!ir_layout.Ref {
        if (self.type_layouts.get(ty)) |existing| return existing;
        const lowered = try lower_type.lowerType(allocator, mono_types, &self.graph, &self.cache, ty);
        try self.type_layouts.put(ty, lowered);
        return lowered;
    }

    fn attachDerivedExprFacts(
        self: *Facts,
        input: *lambdamono.Lower.Result,
    ) std.mem.Allocator.Error!void {
        for (input.store.exprs.items, 0..) |expr, i| {
            const expr_id: lambdamono.Ast.ExprId = @enumFromInt(@as(u32, @intCast(i)));
            switch (expr.data) {
                .tag => |tag| {
                    if (input.store.sliceExprSpan(tag.args).len == 0) continue;
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
    }

    fn attachDerivedPatFacts(
        self: *Facts,
        input: *lambdamono.Lower.Result,
    ) std.mem.Allocator.Error!void {
        for (input.store.pats.items, 0..) |pat, i| {
            switch (pat.data) {
                .tag => |tag| {
                    if (input.store.slicePatSpan(tag.args).len == 0) continue;
                    const pat_id: lambdamono.Ast.PatId = @enumFromInt(@as(u32, @intCast(i)));
                    self.pat_tag_payload_layouts[i] = try self.unionPayloadLayout(self.patLayout(pat_id), tag.discriminant);
                },
                else => {},
            }
        }
    }

    fn unionPayloadLayout(
        self: *Facts,
        union_layout: ir_layout.Ref,
        discriminant: u16,
    ) std.mem.Allocator.Error!ir_layout.Ref {
        const resolved = try self.resolveUnionLayout(union_layout);
        return switch (resolved) {
            .canonical => debugPanic("ir.layout_facts.unionPayloadLayout expected local union layout"),
            .local => |node_id| switch (self.graph.getNode(node_id)) {
                .tag_union => |variants| self.graph.getRefs(variants)[discriminant],
                else => debugPanic("ir.layout_facts.unionPayloadLayout expected tag union layout"),
            },
        };
    }

    pub fn structFieldLayout(
        self: *Facts,
        struct_layout: ir_layout.Ref,
        field_index: u16,
    ) std.mem.Allocator.Error!ir_layout.Ref {
        const resolved = try self.resolveStructLayout(struct_layout);
        return switch (resolved) {
            .canonical => debugPanic("ir.layout_facts.structFieldLayout expected local struct layout"),
            .local => |node_id| switch (self.graph.getNode(node_id)) {
                .struct_ => |fields| self.graph.getFields(fields)[field_index].child,
                else => debugPanic("ir.layout_facts.structFieldLayout expected struct layout"),
            },
        };
    }

    fn resolveUnionLayout(self: *Facts, layout_ref: ir_layout.Ref) std.mem.Allocator.Error!ir_layout.Ref {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => debugPanic("ir.layout_facts.resolveUnionLayout expected local layout"),
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    .tag_union => return current,
                    else => debugPanic("ir.layout_facts.resolveUnionLayout expected nominal or tag union"),
                },
            }
        }
    }

    fn resolveStructLayout(self: *Facts, layout_ref: ir_layout.Ref) std.mem.Allocator.Error!ir_layout.Ref {
        var current = layout_ref;
        while (true) {
            switch (current) {
                .canonical => debugPanic("ir.layout_facts.resolveStructLayout expected local layout"),
                .local => |node_id| switch (self.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    .struct_ => return current,
                    else => debugPanic("ir.layout_facts.resolveStructLayout expected nominal or struct"),
                },
            }
        }
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

test "layout facts tests" {
    std.testing.refAllDecls(@This());
}

//! Exact tuple-result demand in a finalized lifted function. Discovery and use
//! collection share one source-order walk; no continuation is scanned per field.
const std = @import("std");
const collections = @import("collections");
const Ast = @import("ast.zig");
const Type = @import("../monotype/type.zig");
const Common = @import("../common.zig");
const GuardedList = collections.GuardedList;

/// Source tuple ABI and the exact component demand of its result binding.
pub const Plan = struct {
    source_ty: Type.TypeId,
    items: []Item,
    aggregate: ?Ast.LocalId,
    used_count: usize = 0,

    pub const Item = struct {
        ty: Type.TypeId,
        local: ?Ast.LocalId,
        used: bool = false,
    };

    pub fn selected(self: Plan) bool {
        return self.used_count != 0 and self.used_count != self.items.len;
    }

    fn useItem(self: *Plan, index: usize) void {
        if (!self.items[index].used) {
            self.items[index].used = true;
            self.used_count += 1;
        }
    }
};

/// Function-local demand plans, immutable while the exit rewrite consumes them.
pub const Inventory = struct {
    allocator: std.mem.Allocator,
    program: *const Ast.Program,
    arena: std.heap.ArenaAllocator,
    plans: collections.DenseMap(Ast.PatId, *Plan),
    uses: collections.DenseMap(Ast.LocalId, Use),
    /// Deterministic work counter for scaling tests.
    expr_visits: usize = 0,

    const Use = struct { plan: *Plan, item: ?usize };

    pub fn init(allocator: std.mem.Allocator, program: *const Ast.Program) Inventory {
        return .{
            .allocator = allocator,
            .program = program,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .plans = .init(allocator),
            .uses = .init(allocator),
        };
    }

    pub fn deinit(self: *Inventory) void {
        self.uses.deinit();
        self.plans.deinit();
        self.arena.deinit();
    }

    pub fn get(self: *const Inventory, pat: Ast.PatId) ?*const Plan {
        const plan = self.plans.get(pat) orelse return null;
        return if (plan.selected()) plan else null;
    }

    pub fn hasSelection(self: *const Inventory) bool {
        for (self.plans.values.items) |plan| if (plan.selected()) return true;
        return false;
    }

    fn binding(self: *Inventory, pat_id: Ast.PatId, value: Ast.ExprId) std.mem.Allocator.Error!void {
        const expr = self.program.getExpr(value);
        if (expr.data != .loop_) return;
        const ty = self.program.types.get(expr.ty);
        const pat = self.program.getPat(pat_id);
        const arity = switch (pat.data) {
            .bind => blk: {
                if (ty != .tuple or ty.tuple.len < 2) return;
                break :blk ty.tuple.len;
            },
            .tuple => |span| blk: {
                const pats = self.program.patSpan(span);
                if (pats.len < 2) return;
                for (0..pats.len) |i| {
                    if (self.program.getPat(GuardedList.at(pats, i)).data != .bind) return;
                }
                // The typed pattern supplies the components even when the
                // loop's result root is a transparent tuple alias.
                break :blk span.len;
            },
            .wildcard, .as, .record, .list, .tag, .nominal, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => return,
        };
        if (self.plans.contains(pat_id)) return;
        const plan = try self.arena.allocator().create(Plan);
        plan.* = .{
            .source_ty = expr.ty,
            .items = try self.arena.allocator().alloc(Plan.Item, arity),
            .aggregate = if (pat.data == .bind) pat.data.bind else null,
        };
        for (plan.items, 0..) |*item, i| {
            item.* = .{
                .ty = if (pat.data == .tuple)
                    self.program.getPat(GuardedList.at(self.program.patSpan(pat.data.tuple), i)).ty
                else
                    GuardedList.at(self.program.types.span(ty.tuple), i),
                .local = if (pat.data == .tuple)
                    self.program.getPat(GuardedList.at(self.program.patSpan(pat.data.tuple), i)).data.bind
                else
                    null,
            };
            if (item.local) |local| try self.uses.put(local, .{ .plan = plan, .item = i });
        }
        if (plan.aggregate) |local| try self.uses.put(local, .{ .plan = plan, .item = null });
        try self.plans.put(pat_id, plan);
    }

    fn useLocal(self: *Inventory, local: Ast.LocalId, field: ?u32) void {
        const use = self.uses.get(local) orelse return;
        if (use.plan.used_count == use.plan.items.len) return;
        if (use.item) |i| {
            use.plan.useItem(i);
        } else if (field) |i| {
            if (i >= use.plan.items.len) Common.invariant("tuple demand outside source type");
            use.plan.useItem(i);
        } else {
            // An opaque use observes the complete tuple, including retained
            // locals and values crossing call or capture boundaries.
            for (use.plan.items) |*item| item.used = true;
            use.plan.used_count = use.plan.items.len;
        }
    }

    /// Record every loop-result binding in `id` and the tuple items its uses
    /// demand. Positions are visited in source order on an explicit stack, so
    /// a binding is always recorded before the uses in its scope.
    pub fn collect(self: *Inventory, id: Ast.ExprId) std.mem.Allocator.Error!void {
        var stack: std.ArrayList(Ast.ExprChild) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .expr = id });
        while (stack.pop()) |child| {
            // Children are appended in source order, then reversed.
            const start = stack.items.len;
            switch (child) {
                .stmt => |stmt_id| switch (self.program.getStmt(stmt_id)) {
                    .let_ => |let_| {
                        if (!let_.recursive) try self.binding(let_.pat, let_.value);
                        try stack.append(self.allocator, .{ .expr = let_.value });
                    },
                    .expr, .expect, .dbg => |expr| try stack.append(self.allocator, .{ .expr = expr }),
                    .return_ => |ret| try stack.append(self.allocator, .{ .expr = ret.value }),
                    .uninitialized, .crash => {},
                },
                .expr => |expr_id| {
                    if (@import("builtin").is_test) self.expr_visits += 1;
                    switch (self.program.getExpr(expr_id).data) {
                        .local => |local| self.useLocal(local, null),
                        .tuple_access => |access| {
                            const receiver = self.program.getExpr(access.tuple);
                            if (receiver.data == .local) {
                                self.useLocal(receiver.data.local, access.elem_index);
                            } else try stack.append(self.allocator, .{ .expr = access.tuple });
                        },
                        .let_ => |let_| {
                            try self.binding(let_.bind, let_.value);
                            try Ast.appendChildren(self.allocator, self.program, expr_id, &stack);
                        },
                        .join_point => |join| {
                            const retained = self.program.typedLocalSpan(join.retained);
                            for (0..retained.len) |i| self.useLocal(GuardedList.at(retained, i).local, null);
                            try Ast.appendChildren(self.allocator, self.program, expr_id, &stack);
                        },
                        .uninitialized_payload => |payload| self.useLocal(payload.condition, null),
                        .if_initialized_payload => |payload| {
                            self.useLocal(payload.payload, null);
                            try Ast.appendChildren(self.allocator, self.program, expr_id, &stack);
                        },
                        .lambda, .def_ref, .fn_def => Common.invariant("pre-lift expression in loop exit demand"),
                        .unit,
                        .@"unreachable",
                        .int_lit,
                        .dec_lit,
                        .frac_f32_lit,
                        .frac_f64_lit,
                        .str_lit,
                        .bytes_lit,
                        .crash,
                        .comptime_exhaustiveness_failed,
                        .uninitialized,
                        .block,
                        .loop_,
                        .list,
                        .tuple,
                        .record,
                        .record_update,
                        .tag,
                        .nominal,
                        .dbg,
                        .expect,
                        .static_data_candidate,
                        .comptime_value,
                        .typed_boundary,
                        .fn_ref,
                        .call_value,
                        .call_proc,
                        .low_level,
                        .field_access,
                        .structural_eq,
                        .structural_hash,
                        .if_,
                        .match_,
                        .jump,
                        .break_,
                        .continue_,
                        .return_,
                        .comptime_branch_taken,
                        .expect_err,
                        .literal_rejected,
                        .try_sequence,
                        .try_record_sequence,
                        => try Ast.appendChildren(self.allocator, self.program, expr_id, &stack),
                    }
                },
            }
            std.mem.reverse(Ast.ExprChild, stack.items[start..]);
        }
    }
};

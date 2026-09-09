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

    pub fn collect(self: *Inventory, id: Ast.ExprId) std.mem.Allocator.Error!void {
        var current = id;
        while (true) {
            if (@import("builtin").is_test) self.expr_visits += 1;
            switch (self.program.getExpr(current).data) {
                .local => |local| self.useLocal(local, null),
                .tuple_access => |access| {
                    const receiver = self.program.getExpr(access.tuple);
                    if (receiver.data == .local) {
                        self.useLocal(receiver.data.local, access.elem_index);
                    } else try self.collect(access.tuple);
                },
                .let_ => |let_| {
                    try self.binding(let_.bind, let_.value);
                    try self.collect(let_.value);
                    current = let_.rest;
                    continue;
                },
                .block => |block| {
                    try self.statements(block.statements);
                    current = block.final_expr;
                    continue;
                },
                .loop_ => |loop| {
                    try self.expressions(loop.initial_values);
                    try self.collect(loop.body);
                },
                .list, .tuple => |items| try self.expressions(items),
                .record => |record_fields| try self.fields(record_fields),
                .record_update => |update| {
                    try self.collect(update.base);
                    try self.fields(update.fields);
                },
                .tag => |tag| try self.expressions(tag.payloads),
                .nominal, .dbg, .expect => |child| try self.collect(child),
                .static_data_candidate => |candidate| try self.collect(candidate.runtime_expr),
                .typed_boundary => |boundary| try self.collect(boundary.value),
                .fn_ref => |ref| try self.captures(ref.captures),
                .call_value => |call| {
                    try self.collect(call.callee);
                    try self.expressions(call.args);
                },
                .call_proc => |call| {
                    try self.expressions(call.args);
                    try self.captures(call.captures);
                },
                .low_level => |call| try self.expressions(call.args),
                .field_access => |field| try self.collect(field.receiver),
                .structural_eq => |eq| {
                    try self.collect(eq.lhs);
                    try self.collect(eq.rhs);
                },
                .structural_hash => |hash| {
                    try self.collect(hash.value);
                    try self.collect(hash.hasher);
                },
                .if_ => |if_| {
                    const branches = self.program.ifBranchSpan(if_.branches);
                    for (0..branches.len) |i| {
                        const branch = GuardedList.at(branches, i);
                        try self.collect(branch.cond);
                        try self.collect(branch.body);
                    }
                    try self.collect(if_.final_else);
                },
                .match_ => |match| {
                    try self.collect(match.scrutinee);
                    const branches = self.program.branchSpan(match.branches);
                    for (0..branches.len) |i| {
                        const branch = GuardedList.at(branches, i);
                        try self.statements(branch.bindings);
                        if (branch.guard) |guard| try self.collect(guard);
                        try self.collect(branch.body);
                    }
                },
                .join_point => |join| {
                    const retained = self.program.typedLocalSpan(join.retained);
                    for (0..retained.len) |i| self.useLocal(GuardedList.at(retained, i).local, null);
                    try self.collect(join.body);
                    try self.collect(join.remainder);
                },
                .jump => |jump| {
                    try self.expressions(jump.args);
                    try self.expressions(jump.loop_values);
                },
                .break_ => |value| if (value) |expr| try self.collect(expr),
                .continue_ => |cont| try self.expressions(cont.values),
                .return_ => |ret| try self.collect(ret.value),
                .comptime_branch_taken => |taken| try self.collect(taken.body),
                .expect_err => |err| try self.collect(err.msg),
                .uninitialized_payload => |payload| self.useLocal(payload.condition, null),
                .if_initialized_payload => |payload| {
                    self.useLocal(payload.payload, null);
                    try self.collect(payload.cond);
                    try self.collect(payload.initialized);
                    try self.collect(payload.uninitialized);
                },
                .try_sequence => |seq| {
                    try self.collect(seq.try_expr);
                    try self.collect(seq.ok_body);
                },
                .try_record_sequence => |seq| {
                    try self.collect(seq.try_expr);
                    try self.collect(seq.ok_body);
                },
                .lambda, .def_ref, .fn_def => Common.invariant("pre-lift expression in loop exit demand"),
                .unit, .@"unreachable", .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .bytes_lit, .crash, .comptime_exhaustiveness_failed, .uninitialized => {},
            }
            return;
        }
    }

    fn statements(self: *Inventory, span: Ast.Span(Ast.StmtId)) std.mem.Allocator.Error!void {
        const stmts = self.program.stmtSpan(span);
        for (0..stmts.len) |i| switch (self.program.getStmt(GuardedList.at(stmts, i))) {
            .let_ => |let_| {
                if (!let_.recursive) try self.binding(let_.pat, let_.value);
                try self.collect(let_.value);
            },
            .expr, .expect, .dbg => |expr| try self.collect(expr),
            .return_ => |ret| try self.collect(ret.value),
            .uninitialized, .crash => {},
        };
    }

    fn expressions(self: *Inventory, span: Ast.Span(Ast.ExprId)) std.mem.Allocator.Error!void {
        const exprs = self.program.exprSpan(span);
        for (0..exprs.len) |i| try self.collect(GuardedList.at(exprs, i));
    }

    fn fields(self: *Inventory, span: Ast.Span(Ast.FieldExpr)) std.mem.Allocator.Error!void {
        const items = self.program.fieldExprSpan(span);
        for (0..items.len) |i| try self.collect(GuardedList.at(items, i).value);
    }

    fn captures(self: *Inventory, span: Ast.Span(Ast.CaptureOperand)) std.mem.Allocator.Error!void {
        const items = self.program.captureOperandSpan(span);
        for (0..items.len) |i| try self.collect(GuardedList.at(items, i).value);
    }
};

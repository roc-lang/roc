//! Sequence strict operands before their consumers. Monotype mutable versions
//! can be bound inside a block/let operand and used by the enclosing continuation.
//! The source expression graph is acyclic; function recursion travels through
//! explicit function references, which this walk does not follow.
//! Lift those bindings into that continuation, keeping every operand's evaluation
//! in source order. Branch and loop bodies retain their own control-flow scopes.
//! The walk runs from an explicit frame stack, so the depth of the source
//! never becomes native call depth.
const std = @import("std");
const collections = @import("collections");
const Ast = @import("ast.zig");
const Common = @import("../common.zig");
const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

const Statements = struct {
    list: std.ArrayList(Ast.StmtId) = .empty,
    terminated: bool = false,

    fn deinit(self: *Statements, allocator: Allocator) void {
        self.list.deinit(allocator);
    }
};

/// A statement list being filled, by its position on the sink stack. Frames
/// release their sinks in the reverse of the order they reserved them.
const SinkId = u32;

/// Sequence lifted operands while retaining lexical variable scope.
pub fn run(program: *Ast.Program) Allocator.Error!void {
    var normalizer = Normalizer{ .program = program, .symbols = .{ .next = program.next_symbol } };
    defer normalizer.deinit();
    const old_loc = program.current_loc;
    const old_region = program.current_region;
    const old_inline = program.current_inline_scope;
    defer {
        program.current_loc = old_loc;
        program.current_region = old_region;
        program.current_inline_scope = old_inline;
    }
    for (0..program.fnCount()) |index| {
        const id: Ast.FnId = @fromBackingInt(@intCast(index));
        var function = program.getFn(id);
        if (function.body != .roc) continue;
        const outer_shapes = program.beginFnShapes(id);
        function.body = .{ .roc = try normalizer.run(.{ .scope = function.body.roc }) };
        function.shapes = program.finishFnShapes(outer_shapes);
        program.setFn(id, function);
    }
    program.next_symbol = normalizer.symbols.next;
}

const IfExpr = @FieldType(Ast.ExprData, "if_");

/// The source position a frame lowers under, restored when it finishes.
const SavedLocation = struct {
    loc: @FieldType(Ast.Program, "current_loc"),
    region: @FieldType(Ast.Program, "current_region"),
    inline_scope: @FieldType(Ast.Program, "current_inline_scope"),
};

/// What a frame asks to have normalized before it continues.
const Request = union(enum) {
    /// An expression whose bindings join `sink`; delivers its result.
    expr: struct { source: Ast.ExprId, sink: SinkId },
    /// An expression in a fresh scope of its own; delivers the scoped result.
    scope: Ast.ExprId,
    /// Statements appended to `sink` in order; delivers nothing.
    stmts: struct { span: Ast.Span(Ast.StmtId), sink: SinkId },
};

const Step = union(enum) {
    request: Request,
    done: ?Ast.ExprId,
};

/// A consumer's strict operands, sequenced as one group. Each operand fills a
/// sink of its own so the group can see which later operands contribute
/// bindings before the consumer.
const OperandGroup = struct {
    values: std.ArrayList(Ast.ExprId) = .empty,
    /// Offsets splitting `values` into the consumer's spans.
    split: [2]usize = .{ 0, 0 },
    next: usize = 0,
    first_sink: SinkId = 0,

    fn deinit(group: *OperandGroup, allocator: Allocator) void {
        group.values.deinit(allocator);
    }

    fn appendExprs(group: *OperandGroup, allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        for (0..span.len) |index| try group.values.append(allocator, GuardedList.at(program.exprSpan(span), index));
    }

    fn appendFields(group: *OperandGroup, allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        for (0..span.len) |index| try group.values.append(allocator, GuardedList.at(program.fieldExprSpan(span), index).value);
    }

    fn appendCaptures(group: *OperandGroup, allocator: Allocator, program: *const Ast.Program, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        for (0..span.len) |index| try group.values.append(allocator, GuardedList.at(program.captureOperandSpan(span), index).value);
    }
};

const ExprFrame = struct {
    source: Ast.ExprId,
    ty: @FieldType(Ast.Expr, "ty"),
    data: Ast.ExprData,
    sink: SinkId,
    saved: SavedLocation,
    started: bool = false,
    /// Whether the expression sequences an operand group, and whether that
    /// group is complete.
    grouped: bool = false,
    group_done: bool = false,
    /// Progress through an expression without an operand group.
    cursor: u32 = 0,
    index: usize = 0,
    group: OperandGroup = .{},
    /// A match's rebuilt branches, or an `if` chain's conditions and bodies.
    branches: std.ArrayList(Ast.Branch) = .empty,
    if_branches: std.ArrayList(Ast.IfBranch) = .empty,
    /// Each `if` level's sink: the first is the frame's own, each later one
    /// the preceding false continuation.
    if_sinks: std.ArrayList(SinkId) = .empty,

    fn deinit(frame: *ExprFrame, allocator: Allocator) void {
        frame.group.deinit(allocator);
        frame.branches.deinit(allocator);
        frame.if_branches.deinit(allocator);
        frame.if_sinks.deinit(allocator);
    }
};

const StmtsFrame = struct {
    span: Ast.Span(Ast.StmtId),
    sink: SinkId,
    index: usize = 0,
    /// The statement suspended on its child, and the position it replaced.
    current: ?struct { stmt: Ast.Stmt, saved: SavedLocation } = null,
};

const ScopeFrame = struct {
    source: Ast.ExprId,
    sink: ?SinkId = null,
};

const Frame = struct {
    kind: union(enum) {
        expr: ExprFrame,
        stmts: StmtsFrame,
        scope: ScopeFrame,
    },
    /// The sink stack's height when the frame began; everything it reserved
    /// sits above.
    sink_base: SinkId,
};

const Normalizer = struct {
    program: *Ast.Program,
    symbols: Common.SymbolGen,
    sinks: std.ArrayList(Statements) = .empty,
    frames: std.ArrayList(Frame) = .empty,

    fn deinit(self: *Normalizer) void {
        const allocator = self.program.allocator;
        for (self.frames.items) |*frame| switch (frame.kind) {
            .expr => |*expr| expr.deinit(allocator),
            .stmts, .scope => {},
        };
        self.frames.deinit(allocator);
        for (self.sinks.items) |*statements| statements.deinit(allocator);
        self.sinks.deinit(allocator);
    }

    fn sink(self: *Normalizer, id: SinkId) *Statements {
        return &self.sinks.items[id];
    }

    fn reserveSink(self: *Normalizer) Allocator.Error!SinkId {
        const id: SinkId = @intCast(self.sinks.items.len);
        try self.sinks.append(self.program.allocator, .{});
        return id;
    }

    fn releaseSinksFrom(self: *Normalizer, base: SinkId) void {
        for (self.sinks.items[base..]) |*released| released.deinit(self.program.allocator);
        self.sinks.shrinkRetainingCapacity(base);
    }

    fn append(self: *Normalizer, id: SinkId, stmt: Ast.StmtId) Allocator.Error!void {
        try self.sink(id).list.append(self.program.allocator, stmt);
    }

    fn saveLocation(self: *Normalizer) SavedLocation {
        return .{
            .loc = self.program.current_loc,
            .region = self.program.current_region,
            .inline_scope = self.program.current_inline_scope,
        };
    }

    fn restoreLocation(self: *Normalizer, saved: SavedLocation) void {
        self.program.current_loc = saved.loc;
        self.program.current_region = saved.region;
        self.program.current_inline_scope = saved.inline_scope;
    }

    fn unreachableExpr(self: *Normalizer, ty: @FieldType(Ast.Expr, "ty")) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{ .ty = ty, .data = .@"unreachable" });
    }

    /// Normalize `root` to completion.
    fn run(self: *Normalizer, root: Request) Allocator.Error!Ast.ExprId {
        std.debug.assert(self.frames.items.len == 0);
        try self.push(root);
        var delivered: ?Ast.ExprId = null;
        while (true) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            const step = switch (frame.kind) {
                .expr => |*expr| try self.stepExpr(expr, delivered),
                .stmts => |*stmts| try self.stepStmts(stmts, delivered),
                .scope => |*scope| try self.stepScope(scope, delivered),
            };
            delivered = null;
            switch (step) {
                .request => |request| try self.push(request),
                .done => |result| {
                    var finished = self.frames.pop().?;
                    self.releaseSinksFrom(finished.sink_base);
                    switch (finished.kind) {
                        .expr => |*expr| {
                            self.restoreLocation(expr.saved);
                            expr.deinit(self.program.allocator);
                        },
                        .stmts, .scope => {},
                    }
                    if (self.frames.items.len == 0) return result.?;
                    delivered = result;
                },
            }
        }
    }

    fn push(self: *Normalizer, request: Request) Allocator.Error!void {
        const sink_base: SinkId = @intCast(self.sinks.items.len);
        switch (request) {
            .expr => |child| {
                const saved = self.saveLocation();
                self.program.current_loc = self.program.exprLoc(child.source);
                self.program.current_region = self.program.exprRegion(child.source);
                self.program.current_inline_scope = self.program.exprInlineScope(child.source);
                const expr = self.program.getExpr(child.source);
                try self.frames.append(self.program.allocator, .{ .sink_base = sink_base, .kind = .{ .expr = .{
                    .source = child.source,
                    .ty = expr.ty,
                    .data = expr.data,
                    .sink = child.sink,
                    .saved = saved,
                } } });
            },
            .scope => |source| try self.frames.append(self.program.allocator, .{ .sink_base = sink_base, .kind = .{ .scope = .{ .source = source } } }),
            .stmts => |stmts| try self.frames.append(self.program.allocator, .{ .sink_base = sink_base, .kind = .{ .stmts = .{ .span = stmts.span, .sink = stmts.sink } } }),
        }
    }

    fn stepScope(self: *Normalizer, frame: *ScopeFrame, delivered: ?Ast.ExprId) Allocator.Error!Step {
        const scope_sink = frame.sink orelse {
            const reserved = try self.reserveSink();
            frame.sink = reserved;
            return .{ .request = .{ .expr = .{ .source = frame.source, .sink = reserved } } };
        };
        const result = delivered.?;
        const statements = self.sink(scope_sink);
        if (statements.list.items.len == 0) return .{ .done = result };
        // The scope's block stands where its source expression did.
        const saved = self.saveLocation();
        defer self.restoreLocation(saved);
        self.program.current_loc = self.program.exprLoc(frame.source);
        self.program.current_region = self.program.exprRegion(frame.source);
        self.program.current_inline_scope = self.program.exprInlineScope(frame.source);
        return .{ .done = try self.program.addExpr(.{ .ty = self.program.getExpr(result).ty, .data = .{ .block = .{
            .statements = try self.program.addStmtSpan(statements.list.items),
            .final_expr = result,
        } } }) };
    }

    fn stepStmts(self: *Normalizer, frame: *StmtsFrame, delivered: ?Ast.ExprId) Allocator.Error!Step {
        if (frame.current) |*current| {
            const child = delivered.?;
            switch (current.stmt) {
                .let_ => |*binding| binding.value = child,
                .expr, .dbg, .expect => |*value| value.* = child,
                .return_ => |*ret| ret.value = child,
                .uninitialized, .crash, .checked_error => unreachable,
            }
            const saved = current.saved;
            const stmt = current.stmt;
            frame.current = null;
            try self.finishStatement(frame.sink, stmt);
            self.restoreLocation(saved);
        }
        while (frame.index < frame.span.len) {
            if (self.sink(frame.sink).terminated) return .{ .done = null };
            const source = GuardedList.at(self.program.stmtSpan(frame.span), frame.index);
            frame.index += 1;
            const saved = self.saveLocation();
            self.program.current_loc = self.program.stmtLoc(source);
            self.program.current_region = self.program.stmtRegion(source);
            self.program.current_inline_scope = self.program.stmtInlineScope(source);
            const stmt = self.program.getStmt(source);
            const request: ?Request = switch (stmt) {
                .let_ => |binding| if (binding.recursive) .{ .scope = binding.value } else .{ .expr = .{ .source = binding.value, .sink = frame.sink } },
                .expr, .dbg => |value| .{ .expr = .{ .source = value, .sink = frame.sink } },
                // Expect conditions retain their run/omit execution context.
                .expect => |value| .{ .scope = value },
                .return_ => |ret| .{ .expr = .{ .source = ret.value, .sink = frame.sink } },
                .uninitialized, .crash, .checked_error => null,
            };
            if (request) |child| {
                frame.current = .{ .stmt = stmt, .saved = saved };
                return .{ .request = child };
            }
            try self.finishStatement(frame.sink, stmt);
            self.restoreLocation(saved);
        }
        return .{ .done = null };
    }

    fn finishStatement(self: *Normalizer, sink_id: SinkId, normalized: Ast.Stmt) Allocator.Error!void {
        if (self.sink(sink_id).terminated) return;
        var stmt = normalized;
        if (stmt == .let_ and terminal(self.program.getExpr(stmt.let_.value).data)) {
            // Read the value before the assignment rewrites `stmt`'s tag.
            const value = stmt.let_.value;
            stmt = .{ .expr = value };
        }
        try self.append(sink_id, try self.program.addStmt(stmt));
        self.sink(sink_id).terminated = switch (stmt) {
            .return_, .crash, .checked_error => true,
            .expr => |value| terminal(self.program.getExpr(value).data),
            .uninitialized, .let_, .expect, .dbg => false,
        };
    }

    /// Finish a strict operand. An operand that transfers control ends the
    /// sequence at its position.
    fn finishOperand(self: *Normalizer, result: Ast.ExprId, sink_id: SinkId) Allocator.Error!Ast.ExprId {
        const expr = self.program.getExpr(result);
        if (!terminal(expr.data)) return result;
        if (expr.data != .@"unreachable") try self.append(sink_id, try self.program.addStmt(.{ .expr = result }));
        self.sink(sink_id).terminated = true;
        return try self.unreachableExpr(expr.ty);
    }

    /// Request the group's next operand, or null once the group is complete.
    fn nextOperand(self: *Normalizer, group: *OperandGroup) Allocator.Error!?Step {
        if (group.next == 0) group.first_sink = @intCast(self.sinks.items.len);
        if (group.next != 0 and self.sink(group.first_sink + @as(SinkId, @intCast(group.next - 1))).terminated) return null;
        if (group.next == group.values.items.len) return null;
        const operand_sink = try self.reserveSink();
        std.debug.assert(operand_sink == group.first_sink + group.next);
        return .{ .request = .{ .expr = .{ .source = group.values.items[group.next], .sink = operand_sink } } };
    }

    fn deliverOperand(self: *Normalizer, group: *OperandGroup, delivered: Ast.ExprId) Allocator.Error!void {
        const operand_sink = group.first_sink + @as(SinkId, @intCast(group.next));
        group.values.items[group.next] = try self.finishOperand(delivered, operand_sink);
        group.next += 1;
    }

    /// Join the group's operands into `sink_id`, left to right. An operand
    /// stays in place unless a later operand's bindings come before the
    /// consumer; then it is named at its own position first. This is
    /// sequencing, not code motion: even a pure opaque call keeps its place
    /// relative to every later operand's bindings.
    fn finishOperands(self: *Normalizer, group: *OperandGroup, sink_id: SinkId) Allocator.Error!void {
        const allocator = self.program.allocator;
        var last_with_bindings: ?usize = null;
        for (0..group.next) |index| {
            if (self.sink(group.first_sink + @as(SinkId, @intCast(index))).list.items.len != 0) last_with_bindings = index;
        }
        for (group.values.items[0..group.next], 0..) |*value, index| {
            const operand_sink = self.sink(group.first_sink + @as(SinkId, @intCast(index)));
            try self.sink(sink_id).list.appendSlice(allocator, operand_sink.list.items);
            if (operand_sink.terminated) {
                self.sink(sink_id).terminated = true;
                break;
            }
            if (last_with_bindings) |last| {
                if (index < last) value.* = try self.name(value.*, sink_id);
            }
        }
        self.releaseSinksFrom(group.first_sink);
    }

    /// Bind a sequenced operand to a fresh local at the current position.
    fn name(self: *Normalizer, result: Ast.ExprId, sink_id: SinkId) Allocator.Error!Ast.ExprId {
        const expr = self.program.getExpr(result);
        switch (expr.data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            => return result,
            .static_data_candidate,
            .comptime_value,
            .typed_boundary,
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .nominal,
            .let_,
            .fn_ref,
            .call_value,
            .call_proc,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .join_point,
            .comptime_branch_taken,
            .dbg,
            .expect,
            => {},
            .@"unreachable",
            .break_,
            .continue_,
            .jump,
            .return_,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            .expect_err,
            .literal_rejected,
            => unreachable,
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("unlifted function reached operand sequencing"),
        }
        const local = try self.program.addLocal(self.symbols.fresh(), expr.ty);
        const pattern = try self.program.addPat(.{ .ty = expr.ty, .data = .{ .bind = local } });
        try self.append(sink_id, try self.program.addStmt(.{ .let_ = .{
            .pat = pattern,
            .value = result,
        } }));
        return try self.program.addExpr(.{ .ty = expr.ty, .data = .{ .local = local } });
    }

    fn terminal(data: Ast.ExprData) bool {
        return switch (data) {
            .@"unreachable", .return_, .break_, .continue_, .jump, .crash, .checked_error, .expect_err, .literal_rejected, .comptime_exhaustiveness_failed => true,
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .static_data_candidate,
            .comptime_value,
            .typed_boundary,
            .list,
            .tuple,
            .record,
            .record_update,
            .tag,
            .nominal,
            .let_,
            .lambda,
            .def_ref,
            .fn_def,
            .fn_ref,
            .call_value,
            .call_proc,
            .low_level,
            .field_access,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .match_,
            .if_,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .try_sequence,
            .try_record_sequence,
            .block,
            .loop_,
            .join_point,
            .comptime_branch_taken,
            .dbg,
            .expect,
            => false,
        };
    }

    fn exprSpanFrom(self: *Normalizer, values: []const Ast.ExprId) Allocator.Error!Ast.Span(Ast.ExprId) {
        return try self.program.addExprSpan(values);
    }

    fn fieldSpanFrom(self: *Normalizer, span: Ast.Span(Ast.FieldExpr), values: []const Ast.ExprId) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const items = try self.program.allocator.alloc(Ast.FieldExpr, span.len);
        defer self.program.allocator.free(items);
        for (items, values, 0..) |*item, value, index| {
            item.* = GuardedList.at(self.program.fieldExprSpan(span), index);
            item.value = value;
        }
        return try self.program.addFieldExprSpan(items);
    }

    fn captureSpanFrom(self: *Normalizer, span: Ast.Span(Ast.CaptureOperand), values: []const Ast.ExprId) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        const items = try self.program.allocator.alloc(Ast.CaptureOperand, span.len);
        defer self.program.allocator.free(items);
        for (items, values, 0..) |*item, value, index| {
            item.* = GuardedList.at(self.program.captureOperandSpan(span), index);
            item.value = value;
        }
        return try self.program.addCaptureOperandSpan(items);
    }

    /// Gather the consumer's strict operands, in evaluation order, into the
    /// frame's group; false when the expression has no operand group.
    fn beginOperands(self: *Normalizer, frame: *ExprFrame) Allocator.Error!bool {
        const allocator = self.program.allocator;
        const group = &frame.group;
        switch (frame.data) {
            .list, .tuple => |items| try group.appendExprs(allocator, self.program, items),
            .tag => |tag| try group.appendExprs(allocator, self.program, tag.payloads),
            .low_level => |call| try group.appendExprs(allocator, self.program, call.args),
            .loop_ => |loop| try group.appendExprs(allocator, self.program, loop.initial_values),
            .continue_ => |transfer| try group.appendExprs(allocator, self.program, transfer.values),
            .record => |fields| try group.appendFields(allocator, self.program, fields),
            .fn_ref => |reference| try group.appendCaptures(allocator, self.program, reference.captures),
            .record_update => |update| {
                try group.values.append(allocator, update.base);
                try group.appendFields(allocator, self.program, update.fields);
            },
            .call_value => |call| {
                try group.values.append(allocator, call.callee);
                try group.appendExprs(allocator, self.program, call.args);
            },
            .call_proc => |call| {
                try group.appendExprs(allocator, self.program, call.args);
                group.split[0] = group.values.items.len;
                try group.appendCaptures(allocator, self.program, call.captures);
            },
            .jump => |transfer| {
                try group.appendExprs(allocator, self.program, transfer.loop_values);
                group.split[0] = group.values.items.len;
                try group.appendExprs(allocator, self.program, transfer.args);
            },
            .structural_eq => |equal| try group.values.appendSlice(allocator, &.{ equal.lhs, equal.rhs }),
            .structural_hash => |hash| try group.values.appendSlice(allocator, &.{ hash.value, hash.hasher }),
            .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .static_data_candidate, .comptime_value, .typed_boundary, .nominal, .let_, .field_access, .tuple_access, .match_, .if_, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .join_point, .comptime_branch_taken, .dbg, .expect, .@"unreachable", .break_, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .expect_err, .literal_rejected, .lambda, .def_ref, .fn_def => return false,
        }
        return true;
    }

    /// Write the group's sequenced operands back into the consumer.
    fn applyOperands(self: *Normalizer, frame: *ExprFrame) Allocator.Error!void {
        const values = frame.group.values.items;
        const split = frame.group.split[0];
        switch (frame.data) {
            .list, .tuple => |*items| items.* = try self.exprSpanFrom(values),
            .tag => |*tag| tag.payloads = try self.exprSpanFrom(values),
            .low_level => |*call| call.args = try self.exprSpanFrom(values),
            .loop_ => |*loop| loop.initial_values = try self.exprSpanFrom(values),
            .continue_ => |*transfer| transfer.values = try self.exprSpanFrom(values),
            .record => |*fields| fields.* = try self.fieldSpanFrom(fields.*, values),
            .fn_ref => |*reference| reference.captures = try self.captureSpanFrom(reference.captures, values),
            .record_update => |*update| {
                update.base = values[0];
                update.fields = try self.fieldSpanFrom(update.fields, values[1..]);
            },
            .call_value => |*call| {
                call.callee = values[0];
                call.args = try self.exprSpanFrom(values[1..]);
            },
            .call_proc => |*call| {
                call.args = try self.exprSpanFrom(values[0..split]);
                call.captures = try self.captureSpanFrom(call.captures, values[split..]);
            },
            .jump => |*transfer| {
                transfer.loop_values = try self.exprSpanFrom(values[0..split]);
                transfer.args = try self.exprSpanFrom(values[split..]);
            },
            .structural_eq => |*equal| {
                equal.lhs = values[0];
                equal.rhs = values[1];
            },
            .structural_hash => |*hash| {
                hash.value = values[0];
                hash.hasher = values[1];
            },
            .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .static_data_candidate, .comptime_value, .typed_boundary, .nominal, .let_, .field_access, .tuple_access, .match_, .if_, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .join_point, .comptime_branch_taken, .dbg, .expect, .@"unreachable", .break_, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .expect_err, .literal_rejected, .lambda, .def_ref, .fn_def => unreachable,
        }
    }

    /// The expression's rebuilt form, or `unreachable` once its sequence
    /// terminated.
    fn finishExpr(self: *Normalizer, frame: *ExprFrame) Allocator.Error!Step {
        if (self.sink(frame.sink).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
        return .{ .done = try self.program.addExpr(.{ .ty = frame.ty, .data = frame.data }) };
    }

    fn stepExpr(self: *Normalizer, frame: *ExprFrame, delivered: ?Ast.ExprId) Allocator.Error!Step {
        if (!frame.started) {
            frame.started = true;
            if (self.sink(frame.sink).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
            self.program.noteExprShapes(self.program.getExpr(frame.source));
            frame.grouped = try self.beginOperands(frame);
        }
        if (!frame.grouped) return try self.stepStructured(frame, delivered);
        if (!frame.group_done) {
            if (delivered) |result| try self.deliverOperand(&frame.group, result);
            if (try self.nextOperand(&frame.group)) |step| return step;
            try self.finishOperands(&frame.group, frame.sink);
            if (self.sink(frame.sink).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
            try self.applyOperands(frame);
            frame.group_done = true;
            return switch (frame.data) {
                // A loop's body keeps its own scope after its initial values.
                .loop_ => |loop| .{ .request = .{ .scope = loop.body } },
                .list, .tuple, .record, .record_update, .tag, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .continue_, .jump, .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .static_data_candidate, .comptime_value, .typed_boundary, .nominal, .let_, .field_access, .tuple_access, .match_, .if_, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .join_point, .comptime_branch_taken, .dbg, .expect, .@"unreachable", .break_, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .expect_err, .literal_rejected, .lambda, .def_ref, .fn_def => try self.finishExpr(frame),
            };
        }
        switch (frame.data) {
            .loop_ => |*loop| loop.body = delivered.?,
            .list, .tuple, .record, .record_update, .tag, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .continue_, .jump, .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .static_data_candidate, .comptime_value, .typed_boundary, .nominal, .let_, .field_access, .tuple_access, .match_, .if_, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .join_point, .comptime_branch_taken, .dbg, .expect, .@"unreachable", .break_, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .expect_err, .literal_rejected, .lambda, .def_ref, .fn_def => unreachable,
        }
        return try self.finishExpr(frame);
    }

    /// Expressions without an operand group, driven by `cursor` from 0 on.
    fn stepStructured(self: *Normalizer, frame: *ExprFrame, delivered: ?Ast.ExprId) Allocator.Error!Step {
        const cursor = frame.cursor;
        frame.cursor += 1;
        const sink_id = frame.sink;
        switch (frame.data) {
            .block => |block| switch (cursor) {
                0 => return .{ .request = .{ .stmts = .{ .span = block.statements, .sink = sink_id } } },
                1 => return .{ .request = .{ .expr = .{ .source = block.final_expr, .sink = sink_id } } },
                else => return .{ .done = delivered.? },
            },
            .let_ => |binding| switch (cursor) {
                0 => return .{ .request = .{ .expr = .{ .source = binding.value, .sink = sink_id } } },
                1 => {
                    const value = delivered.?;
                    if (!self.sink(sink_id).terminated and terminal(self.program.getExpr(value).data)) {
                        if (self.program.getExpr(value).data != .@"unreachable") try self.append(sink_id, try self.program.addStmt(.{ .expr = value }));
                        self.sink(sink_id).terminated = true;
                    }
                    if (self.sink(sink_id).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
                    try self.append(sink_id, try self.program.addStmt(.{ .let_ = .{
                        .pat = binding.bind,
                        .value = value,
                        .comptime_site = binding.comptime_site,
                    } }));
                    return .{ .request = .{ .expr = .{ .source = binding.rest, .sink = sink_id } } };
                },
                else => return .{ .done = delivered.? },
            },
            .match_ => |*match| return try self.stepMatch(frame, match, cursor, delivered),
            .if_ => |conditional| return try self.stepIf(frame, conditional, cursor, delivered),
            .if_initialized_payload => |*conditional| switch (cursor) {
                0 => return .{ .request = .{ .expr = .{ .source = conditional.cond, .sink = sink_id } } },
                1 => {
                    conditional.cond = try self.finishOperand(delivered.?, sink_id);
                    if (self.sink(sink_id).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
                    return .{ .request = .{ .scope = conditional.initialized } };
                },
                2 => {
                    conditional.initialized = delivered.?;
                    return .{ .request = .{ .scope = conditional.uninitialized } };
                },
                else => {
                    conditional.uninitialized = delivered.?;
                    return try self.finishExpr(frame);
                },
            },
            .join_point => |*join| switch (cursor) {
                0 => return .{ .request = .{ .scope = join.body } },
                1 => {
                    join.body = delivered.?;
                    return .{ .request = .{ .scope = join.remainder } };
                },
                else => {
                    join.remainder = delivered.?;
                    return try self.finishExpr(frame);
                },
            },
            inline .try_sequence, .try_record_sequence => |*sequence| switch (cursor) {
                0 => return .{ .request = .{ .expr = .{ .source = sequence.try_expr, .sink = sink_id } } },
                1 => {
                    sequence.try_expr = try self.finishOperand(delivered.?, sink_id);
                    if (self.sink(sink_id).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
                    return .{ .request = .{ .scope = sequence.ok_body } };
                },
                else => {
                    sequence.ok_body = delivered.?;
                    return try self.finishExpr(frame);
                },
            },
            // These children provide evidence or conditional execution, rather
            // than ordinary strict operands at the enclosing position.
            .static_data_candidate => |*candidate| return try self.scopedChild(frame, &candidate.runtime_expr, cursor, delivered),
            .comptime_value => |*candidate| return try self.scopedChild(frame, &candidate.initializer, cursor, delivered),
            .expect => |*child| return try self.scopedChild(frame, child, cursor, delivered),
            .comptime_branch_taken => |*taken| return try self.scopedChild(frame, &taken.body, cursor, delivered),
            .dbg => |*child| return try self.sinkChild(frame, child, cursor, delivered),
            .typed_boundary => |*boundary| return try self.sinkChild(frame, &boundary.value, cursor, delivered),
            .return_ => |*ret| return try self.sinkChild(frame, &ret.value, cursor, delivered),
            .nominal => |*child| return try self.operandChild(frame, child, cursor, delivered),
            .field_access => |*field| return try self.operandChild(frame, &field.receiver, cursor, delivered),
            .tuple_access => |*access| return try self.operandChild(frame, &access.tuple, cursor, delivered),
            inline .expect_err, .literal_rejected => |*failure| return try self.operandChild(frame, &failure.msg, cursor, delivered),
            .break_ => |*value| {
                if (value.*) |*child| return try self.operandChild(frame, child, cursor, delivered);
                return try self.finishExpr(frame);
            },
            .lambda, .def_ref, .fn_def => Common.invariant("unlifted function reached operand sequencing"),
            // The source marker carries the producer's explicit termination
            // relation, including divergent calls whose syntax is ordinary.
            .@"unreachable" => {
                self.sink(sink_id).terminated = true;
                return .{ .done = frame.source };
            },
            .local, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .uninitialized_payload, .crash, .checked_error, .comptime_exhaustiveness_failed => return .{ .done = frame.source },
            .list, .tuple, .record, .record_update, .tag, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .loop_, .continue_, .jump => unreachable,
        }
    }

    /// A child in its own scope.
    fn scopedChild(self: *Normalizer, frame: *ExprFrame, child: *Ast.ExprId, cursor: u32, delivered: ?Ast.ExprId) Allocator.Error!Step {
        if (cursor == 0) return .{ .request = .{ .scope = child.* } };
        child.* = delivered.?;
        return try self.finishExpr(frame);
    }

    /// A child whose bindings join the expression's own sequence.
    fn sinkChild(self: *Normalizer, frame: *ExprFrame, child: *Ast.ExprId, cursor: u32, delivered: ?Ast.ExprId) Allocator.Error!Step {
        if (cursor == 0) return .{ .request = .{ .expr = .{ .source = child.*, .sink = frame.sink } } };
        child.* = delivered.?;
        return try self.finishExpr(frame);
    }

    /// A consumer's single strict operand.
    fn operandChild(self: *Normalizer, frame: *ExprFrame, child: *Ast.ExprId, cursor: u32, delivered: ?Ast.ExprId) Allocator.Error!Step {
        if (cursor == 0) return .{ .request = .{ .expr = .{ .source = child.*, .sink = frame.sink } } };
        child.* = try self.finishOperand(delivered.?, frame.sink);
        return try self.finishExpr(frame);
    }

    /// The scrutinee, then each branch's bindings, guard, and scoped body.
    /// Cursor 0 requests the scrutinee; afterwards each branch takes three
    /// steps: its bindings, its guard, and its body.
    fn stepMatch(self: *Normalizer, frame: *ExprFrame, match: *@FieldType(Ast.ExprData, "match_"), cursor: u32, delivered: ?Ast.ExprId) Allocator.Error!Step {
        if (cursor == 0) return .{ .request = .{ .expr = .{ .source = match.scrutinee, .sink = frame.sink } } };
        if (cursor == 1) {
            match.scrutinee = try self.finishOperand(delivered.?, frame.sink);
            if (self.sink(frame.sink).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
            try frame.branches.ensureTotalCapacity(self.program.allocator, match.branches.len);
        }
        const phase = (cursor - 1) % 3;
        if (phase == 0) {
            // A branch finished its body, or none has started.
            if (cursor != 1) {
                frame.branches.items[frame.branches.items.len - 1].body = delivered.?;
            }
            if (frame.branches.items.len == match.branches.len) {
                match.branches = try self.program.addBranchSpan(frame.branches.items);
                return try self.finishExpr(frame);
            }
            const branch = GuardedList.at(self.program.branchSpan(match.branches), frame.branches.items.len);
            frame.branches.appendAssumeCapacity(branch);
            frame.index = try self.reserveSink();
            return .{ .request = .{ .stmts = .{ .span = branch.bindings, .sink = @intCast(frame.index) } } };
        }
        const branch = &frame.branches.items[frame.branches.items.len - 1];
        const bindings_sink: SinkId = @intCast(frame.index);
        if (phase == 1) {
            if (branch.guard) |guard| return .{ .request = .{ .expr = .{ .source = guard, .sink = bindings_sink } } };
            frame.cursor += 1;
        } else if (branch.guard != null) {
            // A guard whose evaluation transfers control ends the branch's
            // bindings there, so it produces no value to test: reaching the
            // branch runs its bindings up to that transfer.
            branch.guard = if (self.sink(bindings_sink).terminated) null else delivered.?;
        }
        branch.bindings = try self.program.addStmtSpan(self.sink(bindings_sink).list.items);
        self.releaseSinksFrom(bindings_sink);
        return .{ .request = .{ .scope = branch.body } };
    }

    /// Later conditions execute only when all earlier conditions are false.
    /// Keep their prefixes in that else continuation, never before the first if.
    /// Each level takes two steps: its condition, then its scoped body; the
    /// final else then fills the last level's continuation.
    fn stepIf(self: *Normalizer, frame: *ExprFrame, source: IfExpr, cursor: u32, delivered: ?Ast.ExprId) Allocator.Error!Step {
        const allocator = self.program.allocator;
        if (cursor == 0) try frame.if_sinks.append(allocator, frame.sink);
        const level = cursor / 2;
        const level_sink = frame.if_sinks.items[frame.if_sinks.items.len - 1];
        if (level < source.branches.len) {
            const branch = GuardedList.at(self.program.ifBranchSpan(source.branches), level);
            if (cursor % 2 == 0) {
                if (cursor != 0) {
                    // The preceding level's body.
                    frame.if_branches.items[frame.if_branches.items.len - 1].body = delivered.?;
                    const else_sink = try self.reserveSink();
                    try frame.if_sinks.append(allocator, else_sink);
                    return .{ .request = .{ .expr = .{ .source = branch.cond, .sink = else_sink } } };
                }
                return .{ .request = .{ .expr = .{ .source = branch.cond, .sink = level_sink } } };
            }
            const cond = try self.finishOperand(delivered.?, level_sink);
            if (self.sink(level_sink).terminated) return try self.assembleIf(frame, try self.unreachableExpr(frame.ty));
            try frame.if_branches.append(allocator, .{ .cond = cond, .body = branch.body });
            return .{ .request = .{ .scope = branch.body } };
        }
        if (cursor == source.branches.len * 2) {
            if (cursor != 0) {
                frame.if_branches.items[frame.if_branches.items.len - 1].body = delivered.?;
                const else_sink = try self.reserveSink();
                try frame.if_sinks.append(allocator, else_sink);
                return .{ .request = .{ .expr = .{ .source = source.final_else, .sink = else_sink } } };
            }
            return .{ .request = .{ .expr = .{ .source = source.final_else, .sink = level_sink } } };
        }
        return try self.assembleIf(frame, delivered.?);
    }

    /// Nest each completed level's `if` around its false continuation, from
    /// the innermost level out. `innermost` is the last level's result: the
    /// final else, or `unreachable` where a condition ended the sequence.
    fn assembleIf(self: *Normalizer, frame: *ExprFrame, innermost: Ast.ExprId) Allocator.Error!Step {
        var result = innermost;
        var level = frame.if_sinks.items.len - 1;
        while (level > 0) {
            level -= 1;
            const else_statements = self.sink(frame.if_sinks.items[level + 1]);
            if (else_statements.list.items.len != 0) result = try self.program.addExpr(.{ .ty = frame.ty, .data = .{ .block = .{
                .statements = try self.program.addStmtSpan(else_statements.list.items),
                .final_expr = result,
            } } });
            if (self.sink(frame.if_sinks.items[level]).terminated) {
                result = try self.unreachableExpr(frame.ty);
                continue;
            }
            const branch = frame.if_branches.items[level];
            result = try self.program.addExpr(.{ .ty = frame.ty, .data = .{ .if_ = .{
                .branches = try self.program.addIfBranchSpan(&.{branch}),
                .final_else = result,
            } } });
        }
        if (self.sink(frame.sink).terminated) return .{ .done = try self.unreachableExpr(frame.ty) };
        return .{ .done = result };
    }
};

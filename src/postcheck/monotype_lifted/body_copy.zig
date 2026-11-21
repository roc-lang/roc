//! Structural copies of lifted expressions.
//!
//! Monotype restores each compile-time constant once and names that one
//! expression wherever the constant is read, so two parents can hold the same
//! expression. An expression with expressions inside it has exactly one
//! parent in lifted output (design.md "Operand Sequencing"), so lifting gives
//! every parent after the first a copy of its own: each statement and each
//! expression with expressions inside it is added again, and locals, literals
//! and markers stay the ones the source names.
//! Copying runs from an explicit stack, so the depth of an expression never
//! becomes native call depth.
const std = @import("std");
const collections = @import("collections");
const Ast = @import("ast.zig");
const Common = @import("../common.zig");
const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

const Work = union(enum) {
    /// Copy this expression and deliver the copy.
    copy: Ast.ExprId,
    /// Every expression inside this one has been delivered: add it again
    /// around those copies.
    assemble: Ast.ExprId,
};

/// Copy the expression rooted at `root` and return the copy's root.
/// Expression shapes are recorded for whichever function's shapes the program
/// is collecting.
pub fn copy(program: *Ast.Program, root: Ast.ExprId) Allocator.Error!Ast.ExprId {
    var copier: Copier = .{ .program = program };
    defer copier.deinit();
    const position = copier.savePosition();
    defer copier.restorePosition(position);
    return try copier.copyOne(root);
}

/// Locals renamed by `copyRenamingReads`, each to its replacement.
pub const Renames = collections.DenseMapUnmanaged(Ast.LocalId, Ast.LocalId);

/// The expression rooted at `root`, with every read of a local in `renames`
/// reading its replacement instead. A read is a `local` expression, an
/// initialized-payload switch's payload, or a local a join point retains.
/// Only the expressions on a path from `root` to such a read are added
/// again; every other expression stays the one `root` names, so a body
/// that reads no renamed local is returned as it is.
pub fn copyRenamingReads(program: *Ast.Program, root: Ast.ExprId, renames: *const Renames) Allocator.Error!Ast.ExprId {
    var copier: Copier = .{ .program = program, .renames = renames };
    defer copier.deinit();
    try copier.markRenamedPaths(root);
    if (!copier.renamed_paths.contains(root)) return root;
    const position = copier.savePosition();
    defer copier.restorePosition(position);
    return try copier.copyOne(root);
}

/// A copy of the expression rooted at `root` that binds locals of its own,
/// for placing a closed expression in a body other than the one that holds
/// it. Every expression with expressions inside it is added again, and every
/// local a binder inside `root` binds (a pattern, a loop or join point
/// parameter, a `Try` sequence's payload) is replaced, at its binder and at
/// every read, by a fresh copy carrying its complete identity
/// (`Program.addLocalCopy`). Locals bound outside `root` stay the ones it
/// names. Lambda Solved gives each local one type, which only functions of
/// one binding group may name (design.md "Lambda Set Generalization").
pub fn copyBindingOwnLocals(program: *Ast.Program, root: Ast.ExprId, symbols: *Common.SymbolGen) Allocator.Error!Ast.ExprId {
    var own: OwnLocals = .{ .symbols = symbols };
    defer own.renames.deinit(program.allocator);
    var copier: Copier = .{ .program = program, .own_locals = &own };
    defer copier.deinit();
    const position = copier.savePosition();
    defer copier.restorePosition(position);
    return try copier.copyOne(root);
}

/// The locals a `copyBindingOwnLocals` copy binds in place of its source's.
const OwnLocals = struct {
    symbols: *Common.SymbolGen,
    renames: Renames = .empty,
};

/// Give `parent` a copy of its own of each expression in `shared`, which are
/// expressions directly inside it that another parent also holds. An
/// expression listed twice is copied at two of the places `parent` names it.
/// `parent` keeps its id.
pub fn copyInside(program: *Ast.Program, parent: Ast.ExprId, shared: []const Ast.ExprId) Allocator.Error!void {
    var copier: Copier = .{ .program = program };
    defer copier.deinit();
    const position = copier.savePosition();
    defer copier.restorePosition(position);

    const data = program.getExpr(parent).data;
    copier.children.clearRetainingCapacity();
    try copier.collectChildren(data);
    const inside = try program.allocator.dupe(Ast.ExprId, copier.children.items);
    defer program.allocator.free(inside);
    const copied = try program.allocator.alloc(bool, inside.len);
    defer program.allocator.free(copied);
    @memset(copied, false);
    var any = false;
    for (shared) |expr| {
        for (inside, copied) |*child, *done| {
            if (done.* or child.* != expr) continue;
            child.* = try copier.copyOne(expr);
            done.* = true;
            any = true;
            break;
        }
    }
    if (!any) return;
    program.setExprData(parent, try copier.assembledData(data, inside));
}

/// Adds an own local for each local a `copyBindingOwnLocals` copy binds.
const OwnBinder = struct {
    program: *Ast.Program,
    own: *OwnLocals,

    pub fn bindLocal(self: OwnBinder, local: Ast.LocalId) Allocator.Error!void {
        const entry = try self.own.renames.getOrPut(self.program.allocator, local);
        if (entry.found_existing) return;
        entry.value_ptr.* = try self.program.addLocalCopy(local, self.own.symbols.fresh());
    }
};

const Copier = struct {
    program: *Ast.Program,
    work: std.ArrayList(Work) = .empty,
    /// Copies delivered and not yet taken by the expression that holds them,
    /// in evaluation order.
    delivered: std.ArrayList(Ast.ExprId) = .empty,
    /// The expressions directly inside the expression being started.
    children: std.ArrayList(Ast.ExprId) = .empty,
    stmt_scratch: std.ArrayList(Ast.StmtId) = .empty,
    expr_scratch: std.ArrayList(Ast.ExprId) = .empty,
    field_scratch: std.ArrayList(Ast.FieldExpr) = .empty,
    capture_scratch: std.ArrayList(Ast.CaptureOperand) = .empty,
    branch_scratch: std.ArrayList(Ast.Branch) = .empty,
    /// For `copyRenamingReads`: the locals it renames, and the expressions
    /// on a path from its root to a read of one, which alone are copied.
    renames: ?*const Renames = null,
    renamed_paths: collections.DenseMapUnmanaged(Ast.ExprId, void) = .empty,
    typed_local_scratch: std.ArrayList(Ast.TypedLocal) = .empty,
    /// For `copyBindingOwnLocals`: the locals the copy binds in place of
    /// its source's, each added when the copy reaches its binder.
    own_locals: ?*OwnLocals = null,

    fn deinit(self: *Copier) void {
        const allocator = self.program.allocator;
        self.typed_local_scratch.deinit(allocator);
        self.renamed_paths.deinit(allocator);
        self.branch_scratch.deinit(allocator);
        self.capture_scratch.deinit(allocator);
        self.field_scratch.deinit(allocator);
        self.expr_scratch.deinit(allocator);
        self.stmt_scratch.deinit(allocator);
        self.children.deinit(allocator);
        self.delivered.deinit(allocator);
        self.work.deinit(allocator);
    }

    const Position = struct {
        loc: @FieldType(Ast.Program, "current_loc"),
        region: @FieldType(Ast.Program, "current_region"),
        inline_scope: @FieldType(Ast.Program, "current_inline_scope"),
    };

    fn savePosition(self: *Copier) Position {
        return .{
            .loc = self.program.current_loc,
            .region = self.program.current_region,
            .inline_scope = self.program.current_inline_scope,
        };
    }

    fn restorePosition(self: *Copier, position: Position) void {
        self.program.current_loc = position.loc;
        self.program.current_region = position.region;
        self.program.current_inline_scope = position.inline_scope;
    }

    fn copyOne(self: *Copier, root: Ast.ExprId) Allocator.Error!Ast.ExprId {
        const base = self.work.items.len;
        try self.work.append(self.program.allocator, .{ .copy = root });
        while (self.work.items.len > base) switch (self.work.pop().?) {
            .copy => |expr| try self.begin(expr),
            .assemble => |expr| try self.assemble(expr),
        };
        return self.delivered.pop().?;
    }

    fn child(self: *Copier, expr: Ast.ExprId) Allocator.Error!void {
        try self.children.append(self.program.allocator, expr);
    }

    fn childExprs(self: *Copier, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        for (0..span.len) |index| try self.child(GuardedList.at(self.program.exprSpan(span), index));
    }

    fn childFields(self: *Copier, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        for (0..span.len) |index| try self.child(GuardedList.at(self.program.fieldExprSpan(span), index).value);
    }

    fn childCaptures(self: *Copier, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        for (0..span.len) |index| try self.child(GuardedList.at(self.program.captureOperandSpan(span), index).value);
    }

    fn childStatements(self: *Copier, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        for (0..span.len) |index| {
            switch (self.program.getStmt(GuardedList.at(self.program.stmtSpan(span), index))) {
                .let_ => |binding| try self.child(binding.value),
                .expr, .expect, .dbg => |value| try self.child(value),
                .return_ => |ret| try self.child(ret.value),
                .uninitialized, .crash, .checked_error => {},
            }
        }
    }

    /// Whether `data` itself reads a local this copy renames.
    fn readsRenamed(self: *const Copier, data: Ast.ExprData) bool {
        const renames = self.renames.?;
        return switch (data) {
            .local => |local| renames.contains(local),
            .if_initialized_payload => |conditional| renames.contains(conditional.payload),
            .join_point => |join| for (0..join.retained.len) |index| {
                if (renames.contains(GuardedList.at(self.program.typedLocalSpan(join.retained), index).local)) break true;
            } else false,
            .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => false,
        };
    }

    /// Record every expression on a path from `root` to a read of a renamed
    /// local, visiting each expression after the expressions inside it.
    fn markRenamedPaths(self: *Copier, root: Ast.ExprId) Allocator.Error!void {
        const allocator = self.program.allocator;
        const Visit = struct { expr: Ast.ExprId, inside_done: bool };
        var stack: std.ArrayList(Visit) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, .{ .expr = root, .inside_done = false });
        while (stack.pop()) |visit| {
            const data = self.program.getExpr(visit.expr).data;
            self.children.clearRetainingCapacity();
            if (!data.isLeaf()) try self.collectChildren(data);
            if (!visit.inside_done) {
                try stack.append(allocator, .{ .expr = visit.expr, .inside_done = true });
                for (self.children.items) |inside| try stack.append(allocator, .{ .expr = inside, .inside_done = false });
                continue;
            }
            const on_path = self.readsRenamed(data) or for (self.children.items) |inside| {
                if (self.renamed_paths.contains(inside)) break true;
            } else false;
            if (on_path) try self.renamed_paths.put(allocator, visit.expr, {});
        }
    }

    /// Deliver a leaf as it is, or queue the expressions inside `expr` ahead
    /// of assembling its copy around theirs.
    fn begin(self: *Copier, expr: Ast.ExprId) Allocator.Error!void {
        const allocator = self.program.allocator;
        const source = self.program.getExpr(expr);
        const data = source.data;
        if (self.renames) |renames| {
            if (!self.renamed_paths.contains(expr)) {
                try self.delivered.append(allocator, expr);
                return;
            }
            if (data == .local) {
                self.program.current_loc = self.program.exprLoc(expr);
                self.program.current_region = self.program.exprRegion(expr);
                self.program.current_inline_scope = self.program.exprInlineScope(expr);
                try self.delivered.append(allocator, try self.program.addExpr(.{ .ty = source.ty, .data = .{ .local = renames.get(data.local).? } }));
                return;
            }
        }
        if (self.own_locals) |own| {
            const renamed: ?Ast.ExprData = switch (data) {
                .local => |local| if (own.renames.get(local)) |own_local| .{ .local = own_local } else null,
                .uninitialized_payload => |payload| if (own.renames.get(payload.condition)) |own_local| .{ .uninitialized_payload = .{
                    .condition = own_local,
                    .mask = payload.mask,
                } } else null,
                .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => null,
            };
            if (renamed) |renamed_data| {
                self.program.current_loc = self.program.exprLoc(expr);
                self.program.current_region = self.program.exprRegion(expr);
                self.program.current_inline_scope = self.program.exprInlineScope(expr);
                try self.delivered.append(allocator, try self.program.addExpr(.{ .ty = source.ty, .data = renamed_data }));
                return;
            }
            // A binder's scope lies inside the expression that holds it, so
            // its own local exists before any read of it is copied.
            if (!data.isLeaf()) try self.bindOwnLocals(own, data);
        }
        if (data.isLeaf()) {
            try self.delivered.append(allocator, expr);
            return;
        }
        self.children.clearRetainingCapacity();
        try self.collectChildren(data);
        try self.work.append(allocator, .{ .assemble = expr });
        // The stack hands the children back first to last.
        var index = self.children.items.len;
        while (index > 0) {
            index -= 1;
            try self.work.append(allocator, .{ .copy = self.children.items[index] });
        }
    }

    /// Add an own local for every local `data` binds directly, including in
    /// its statements and its match branches' patterns.
    fn bindOwnLocals(self: *Copier, own: *OwnLocals, data: Ast.ExprData) Allocator.Error!void {
        const program = self.program;
        const binder: OwnBinder = .{ .program = program, .own = own };
        switch (data) {
            .let_ => |binding| try Ast.forEachBoundLocal(program.allocator, program, binding.bind, binder),
            .match_ => |match| for (0..match.branches.len) |index| {
                const branch = GuardedList.at(program.branchSpan(match.branches), index);
                try Ast.forEachBoundLocal(program.allocator, program, branch.pat, binder);
                try self.bindStatementLocals(binder, branch.bindings);
            },
            .block => |block| try self.bindStatementLocals(binder, block.statements),
            .loop_ => |loop| try self.bindTypedLocals(binder, loop.params),
            .join_point => |join| try self.bindTypedLocals(binder, join.params),
            .try_sequence => |sequence| try binder.bindLocal(sequence.ok_local),
            .try_record_sequence => |sequence| {
                try binder.bindLocal(sequence.value_local);
                try binder.bindLocal(sequence.rest_local);
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
        }
    }

    fn bindStatementLocals(self: *Copier, binder: OwnBinder, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        const program = self.program;
        for (0..span.len) |index| {
            switch (program.getStmt(GuardedList.at(program.stmtSpan(span), index))) {
                .let_ => |binding| try Ast.forEachBoundLocal(program.allocator, program, binding.pat, binder),
                .uninitialized => |pat| try Ast.forEachBoundLocal(program.allocator, program, pat, binder),
                .expr, .expect, .dbg, .return_, .crash, .checked_error => {},
            }
        }
    }

    fn bindTypedLocals(self: *Copier, binder: OwnBinder, span: Ast.Span(Ast.TypedLocal)) Allocator.Error!void {
        for (0..span.len) |index| try binder.bindLocal(GuardedList.at(self.program.typedLocalSpan(span), index).local);
    }

    /// The own local of `local` when the copy binds one, or `local` itself
    /// when it is bound outside the copied expression.
    fn ownLocal(self: *const Copier, local: Ast.LocalId) Ast.LocalId {
        const own = self.own_locals orelse return local;
        return own.renames.get(local) orelse local;
    }

    fn ownTypedLocals(self: *Copier, span: Ast.Span(Ast.TypedLocal)) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        if (span.len == 0) return span;
        self.typed_local_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            var typed = GuardedList.at(self.program.typedLocalSpan(span), index);
            typed.local = self.ownLocal(typed.local);
            try self.typed_local_scratch.append(self.program.allocator, typed);
        }
        return try self.program.addTypedLocalSpan(self.typed_local_scratch.items);
    }

    /// Name the own locals in every local `data` holds as a local rather
    /// than as an expression: its binders and its embedded reads.
    fn nameOwnLocals(self: *Copier, data: *Ast.ExprData) Allocator.Error!void {
        switch (data.*) {
            .let_ => |*binding| binding.bind = try self.ownPat(binding.bind),
            .loop_ => |*loop| loop.params = try self.ownTypedLocals(loop.params),
            .join_point => |*join| {
                join.params = try self.ownTypedLocals(join.params);
                join.retained = try self.ownTypedLocals(join.retained);
            },
            .jump => |*transfer| transfer.loop_params = try self.ownTypedLocals(transfer.loop_params),
            .try_sequence => |*sequence| sequence.ok_local = self.ownLocal(sequence.ok_local),
            .try_record_sequence => |*sequence| {
                sequence.value_local = self.ownLocal(sequence.value_local);
                sequence.rest_local = self.ownLocal(sequence.rest_local);
            },
            .if_initialized_payload => |*conditional| conditional.payload = self.ownLocal(conditional.payload),
            // Match branch patterns are named as the branches are added again.
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .block, .break_, .continue_, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
        }
    }

    /// `root` itself, or for a `copyBindingOwnLocals` copy, `root` added
    /// again with every local it binds replaced by its own local. Each
    /// pattern is added after its subpatterns, from an explicit stack.
    fn ownPat(self: *Copier, root: Ast.PatId) Allocator.Error!Ast.PatId {
        if (self.own_locals == null) return root;
        const program = self.program;
        const allocator = program.allocator;
        const Visit = struct { pat: Ast.PatId, inside_done: bool };
        var stack: std.ArrayList(Visit) = .empty;
        defer stack.deinit(allocator);
        // The copies delivered and not yet taken by the pattern holding them.
        var copies: std.ArrayList(Ast.PatId) = .empty;
        defer copies.deinit(allocator);
        var inside: std.ArrayList(Ast.PatId) = .empty;
        defer inside.deinit(allocator);
        try stack.append(allocator, .{ .pat = root, .inside_done = false });
        while (stack.pop()) |visit| {
            const pat = program.getPat(visit.pat);
            inside.clearRetainingCapacity();
            try patChildren(program, pat.data, &inside);
            if (!visit.inside_done) {
                try stack.append(allocator, .{ .pat = visit.pat, .inside_done = true });
                // The stack hands the subpatterns back first to last.
                var index = inside.items.len;
                while (index > 0) {
                    index -= 1;
                    try stack.append(allocator, .{ .pat = inside.items[index], .inside_done = false });
                }
                continue;
            }
            const first = copies.items.len - inside.items.len;
            const taken = try allocator.dupe(Ast.PatId, copies.items[first..]);
            defer allocator.free(taken);
            copies.shrinkRetainingCapacity(first);
            const data: Ast.PatData = switch (pat.data) {
                .bind => |local| .{ .bind = self.ownLocal(local) },
                .as => |as| .{ .as = .{ .pattern = taken[0], .local = self.ownLocal(as.local) } },
                .record => |fields| blk: {
                    const destructs = try allocator.alloc(Ast.RecordDestruct, fields.len);
                    defer allocator.free(destructs);
                    for (destructs, taken, 0..) |*destruct, pattern, index| destruct.* = .{
                        .name = GuardedList.at(program.recordDestructSpan(fields), index).name,
                        .pattern = pattern,
                    };
                    break :blk .{ .record = try program.addRecordDestructSpan(destructs) };
                },
                .tuple => .{ .tuple = try program.addPatSpan(taken) },
                .tag => |tag| .{ .tag = .{ .name = tag.name, .payloads = try program.addPatSpan(taken) } },
                .nominal => .{ .nominal = taken[0] },
                .list => |list| .{ .list = .{
                    .patterns = try program.addPatSpan(taken[0..list.patterns.len]),
                    .rest = .of(if (list.rest.get()) |rest| .{
                        .index = rest.index,
                        .pattern = if (rest.pattern != .none) .some(taken[list.patterns.len]) else .none,
                    } else null),
                } },
                .str_pattern => |str| blk: {
                    const steps = try allocator.alloc(Ast.StrPatternStep, str.steps.len);
                    defer allocator.free(steps);
                    var captured: usize = 0;
                    for (steps, 0..) |*step, index| {
                        const source = GuardedList.at(program.strPatternStepSpan(str.steps), index);
                        step.* = .{ .capture = .none, .delimiter = source.delimiter };
                        if (source.capture != .none) {
                            step.capture = .some(taken[captured]);
                            captured += 1;
                        }
                    }
                    break :blk .{ .str_pattern = .{
                        .prefix = str.prefix,
                        .steps = try program.addStrPatternStepSpan(steps),
                        .end = str.end,
                    } };
                },
                .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => pat.data,
            };
            try copies.append(allocator, try program.addPat(.{ .ty = pat.ty, .data = data }));
        }
        return copies.pop().?;
    }

    /// Append every subpattern directly inside `data` to `out`, in order.
    fn patChildren(program: *const Ast.Program, data: Ast.PatData, out: *std.ArrayList(Ast.PatId)) Allocator.Error!void {
        const allocator = program.allocator;
        switch (data) {
            .as => |as| try out.append(allocator, as.pattern),
            .nominal => |backing| try out.append(allocator, backing),
            .record => |fields| for (0..fields.len) |index| {
                try out.append(allocator, GuardedList.at(program.recordDestructSpan(fields), index).pattern);
            },
            .tuple => |items| for (0..items.len) |index| try out.append(allocator, GuardedList.at(program.patSpan(items), index)),
            .tag => |tag| for (0..tag.payloads.len) |index| try out.append(allocator, GuardedList.at(program.patSpan(tag.payloads), index)),
            .list => |list| {
                for (0..list.patterns.len) |index| try out.append(allocator, GuardedList.at(program.patSpan(list.patterns), index));
                if (list.rest.get()) |rest| if (rest.pattern.get()) |pattern| try out.append(allocator, pattern);
            },
            .str_pattern => |str| for (0..str.steps.len) |index| {
                if (GuardedList.at(program.strPatternStepSpan(str.steps), index).capture.get()) |capture| try out.append(allocator, capture);
            },
            .bind, .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => {},
        }
    }

    /// Append every expression directly inside `data` to `children`, in
    /// evaluation order.
    fn collectChildren(self: *Copier, data: Ast.ExprData) Allocator.Error!void {
        switch (data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            => {},
            .let_ => |binding| {
                try self.child(binding.value);
                try self.child(binding.rest);
            },
            .lambda, .def_ref, .fn_def => Common.invariant("an unlifted function reached a lifted body copy"),
            .list, .tuple => |items| try self.childExprs(items),
            .tag => |tag| try self.childExprs(tag.payloads),
            .low_level => |call| try self.childExprs(call.args),
            .continue_ => |transfer| try self.childExprs(transfer.values),
            .jump => |transfer| {
                try self.childExprs(transfer.loop_values);
                try self.childExprs(transfer.args);
            },
            .record => |fields| try self.childFields(fields),
            .record_update => |update| {
                try self.child(update.base);
                try self.childFields(update.fields);
            },
            .fn_ref => |reference| try self.childCaptures(reference.captures),
            .call_value => |call| {
                try self.child(call.callee);
                try self.childExprs(call.args);
            },
            .call_proc => |call| {
                try self.childExprs(call.args);
                try self.childCaptures(call.captures);
            },
            .structural_eq => |equal| {
                try self.child(equal.lhs);
                try self.child(equal.rhs);
            },
            .structural_hash => |hash| {
                try self.child(hash.value);
                try self.child(hash.hasher);
            },
            .loop_ => |loop| {
                try self.childExprs(loop.initial_values);
                try self.child(loop.body);
            },
            .nominal, .dbg, .expect => |value| try self.child(value),
            .typed_boundary => |boundary| try self.child(boundary.value),
            .row_widen => |widen| try self.child(widen.value),
            .return_ => |ret| try self.child(ret.value),
            .field_access => |access| try self.child(access.receiver),
            .tuple_access => |access| try self.child(access.tuple),
            inline .expect_err, .literal_rejected => |failure| try self.child(failure.msg),
            .break_ => |maybe_value| if (maybe_value) |value| try self.child(value),
            .static_data_candidate => |candidate| if (candidate.value.subexpression()) |value| try self.child(value),
            .comptime_value => |candidate| try self.child(candidate.initializer),
            .join_point => |join| {
                try self.child(join.body);
                try self.child(join.remainder);
            },
            .if_initialized_payload => |conditional| {
                try self.child(conditional.cond);
                try self.child(conditional.initialized);
                try self.child(conditional.uninitialized);
            },
            inline .try_sequence, .try_record_sequence => |sequence| {
                try self.child(sequence.try_expr);
                try self.child(sequence.ok_body);
            },
            .match_ => |match| {
                try self.child(match.scrutinee);
                for (0..match.branches.len) |index| {
                    const branch = GuardedList.at(self.program.branchSpan(match.branches), index);
                    try self.childStatements(branch.bindings);
                    if (branch.guard.get()) |guard| try self.child(guard);
                    try self.child(branch.body);
                }
            },
            .if_ => |conditional| {
                for (0..conditional.branches.len) |index| {
                    const branch = GuardedList.at(self.program.ifBranchSpan(conditional.branches), index);
                    try self.child(branch.cond);
                    try self.child(branch.body);
                }
                try self.child(conditional.final_else);
            },
            .block => |block| {
                try self.childStatements(block.statements);
                try self.child(block.final_expr);
            },
        }
    }

    /// The copies an expression takes, first to last.
    const Taken = struct {
        copies: []const Ast.ExprId,
        next: usize = 0,

        fn one(taken: *Taken) Ast.ExprId {
            defer taken.next += 1;
            return taken.copies[taken.next];
        }
    };

    fn takeExprs(self: *Copier, taken: *Taken, span: Ast.Span(Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        self.expr_scratch.clearRetainingCapacity();
        for (0..span.len) |_| try self.expr_scratch.append(self.program.allocator, taken.one());
        return try self.program.addExprSpan(self.expr_scratch.items);
    }

    fn takeFields(self: *Copier, taken: *Taken, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        self.field_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            var field = GuardedList.at(self.program.fieldExprSpan(span), index);
            field.value = taken.one();
            try self.field_scratch.append(self.program.allocator, field);
        }
        return try self.program.addFieldExprSpan(self.field_scratch.items);
    }

    fn takeCaptures(self: *Copier, taken: *Taken, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        self.capture_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            var operand = GuardedList.at(self.program.captureOperandSpan(span), index);
            operand.value = taken.one();
            try self.capture_scratch.append(self.program.allocator, operand);
        }
        return try self.program.addCaptureOperandSpan(self.capture_scratch.items);
    }

    /// Add each statement of `span` again, at its own source position,
    /// around the copy of its value.
    fn takeStatements(self: *Copier, taken: *Taken, span: Ast.Span(Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const program = self.program;
        const loc = program.current_loc;
        const region = program.current_region;
        const inline_scope = program.current_inline_scope;
        defer {
            program.current_loc = loc;
            program.current_region = region;
            program.current_inline_scope = inline_scope;
        }
        self.stmt_scratch.clearRetainingCapacity();
        for (0..span.len) |index| {
            const source = GuardedList.at(program.stmtSpan(span), index);
            var stmt = program.getStmt(source);
            switch (stmt) {
                .let_ => |*binding| {
                    binding.value = taken.one();
                    binding.pat = try self.ownPat(binding.pat);
                },
                .uninitialized => |*pat| pat.* = try self.ownPat(pat.*),
                .expr, .expect, .dbg => |*value| value.* = taken.one(),
                .return_ => |*ret| ret.value = taken.one(),
                .crash, .checked_error => {},
            }
            program.current_loc = program.stmtLoc(source);
            program.current_region = program.stmtRegion(source);
            program.current_inline_scope = program.stmtInlineScope(source);
            try self.stmt_scratch.append(program.allocator, try program.addStmt(stmt));
        }
        return try program.addStmtSpan(self.stmt_scratch.items);
    }

    /// Add `expr` again around the copies of the expressions inside it, at
    /// its own source position, and deliver the copy.
    fn assemble(self: *Copier, expr: Ast.ExprId) Allocator.Error!void {
        const program = self.program;
        const source = program.getExpr(expr);
        const count = self.childCount(source.data);
        const first = self.delivered.items.len - count;
        // The copies are read from storage of their own: copying statements
        // below never moves them.
        const copies = try program.allocator.dupe(Ast.ExprId, self.delivered.items[first..]);
        defer program.allocator.free(copies);
        self.delivered.shrinkRetainingCapacity(first);
        var data = try self.assembledData(source.data, copies);
        if (self.renames) |renames| try self.renameEmbeddedReads(&data, renames);
        if (self.own_locals != null) try self.nameOwnLocals(&data);

        program.current_loc = program.exprLoc(expr);
        program.current_region = program.exprRegion(expr);
        program.current_inline_scope = program.exprInlineScope(expr);
        try self.delivered.append(program.allocator, try program.addExpr(.{ .ty = source.ty, .data = data }));
    }

    /// Rename the reads `data` holds as locals rather than as expressions.
    fn renameEmbeddedReads(self: *Copier, data: *Ast.ExprData, renames: *const Renames) Allocator.Error!void {
        switch (data.*) {
            .if_initialized_payload => |*conditional| if (renames.get(conditional.payload)) |renamed| {
                conditional.payload = renamed;
            },
            .join_point => |*join| {
                if (join.retained.len == 0) return;
                self.typed_local_scratch.clearRetainingCapacity();
                for (0..join.retained.len) |index| {
                    var retained = GuardedList.at(self.program.typedLocalSpan(join.retained), index);
                    if (renames.get(retained.local)) |renamed| retained.local = renamed;
                    try self.typed_local_scratch.append(self.program.allocator, retained);
                }
                join.retained = try self.program.addTypedLocalSpan(self.typed_local_scratch.items);
            },
            .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .comptime_value, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
        }
    }

    /// `source` around `inside`, which holds one expression for each
    /// expression directly inside it, in evaluation order. Its spans and
    /// statements are added again.
    fn assembledData(self: *Copier, source: Ast.ExprData, inside: []const Ast.ExprId) Allocator.Error!Ast.ExprData {
        const program = self.program;
        var data = source;
        var taken: Taken = .{ .copies = inside };

        switch (data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            => {},
            .let_ => |*binding| {
                binding.value = taken.one();
                binding.rest = taken.one();
            },
            .lambda, .def_ref, .fn_def => Common.invariant("an unlifted function reached a lifted body copy"),
            .list, .tuple => |*items| items.* = try self.takeExprs(&taken, items.*),
            .tag => |*tag| tag.payloads = try self.takeExprs(&taken, tag.payloads),
            .low_level => |*call| call.args = try self.takeExprs(&taken, call.args),
            .continue_ => |*transfer| transfer.values = try self.takeExprs(&taken, transfer.values),
            .jump => |*transfer| {
                transfer.loop_values = try self.takeExprs(&taken, transfer.loop_values);
                transfer.args = try self.takeExprs(&taken, transfer.args);
            },
            .record => |*fields| fields.* = try self.takeFields(&taken, fields.*),
            .record_update => |*update| {
                update.base = taken.one();
                update.fields = try self.takeFields(&taken, update.fields);
            },
            .fn_ref => |*reference| reference.captures = try self.takeCaptures(&taken, reference.captures),
            .call_value => |*call| {
                call.callee = taken.one();
                call.args = try self.takeExprs(&taken, call.args);
            },
            .call_proc => |*call| {
                call.args = try self.takeExprs(&taken, call.args);
                call.captures = try self.takeCaptures(&taken, call.captures);
            },
            .structural_eq => |*equal| {
                equal.lhs = taken.one();
                equal.rhs = taken.one();
            },
            .structural_hash => |*hash| {
                hash.value = taken.one();
                hash.hasher = taken.one();
            },
            .loop_ => |*loop| {
                loop.initial_values = try self.takeExprs(&taken, loop.initial_values);
                loop.body = taken.one();
            },
            .nominal, .dbg, .expect => |*value| value.* = taken.one(),
            .typed_boundary => |*boundary| boundary.value = taken.one(),
            .row_widen => |*widen| widen.value = taken.one(),
            .return_ => |*ret| ret.value = taken.one(),
            .field_access => |*access| access.receiver = taken.one(),
            .tuple_access => |*access| access.tuple = taken.one(),
            inline .expect_err, .literal_rejected => |*failure| failure.msg = taken.one(),
            .break_ => |*maybe_value| if (maybe_value.* != null) {
                maybe_value.* = taken.one();
            },
            .static_data_candidate => |*candidate| if (candidate.value.subexpressionPtr()) |value| {
                value.* = taken.one();
            },
            .comptime_value => |*candidate| candidate.initializer = taken.one(),
            .join_point => |*join| {
                join.body = taken.one();
                join.remainder = taken.one();
            },
            .if_initialized_payload => |*conditional| {
                conditional.cond = taken.one();
                conditional.initialized = taken.one();
                conditional.uninitialized = taken.one();
            },
            inline .try_sequence, .try_record_sequence => |*sequence| {
                sequence.try_expr = taken.one();
                sequence.ok_body = taken.one();
            },
            .match_ => |*match| {
                match.scrutinee = taken.one();
                const branches = try program.allocator.alloc(Ast.Branch, match.branches.len);
                defer program.allocator.free(branches);
                for (branches, 0..) |*branch, index| {
                    branch.* = GuardedList.at(program.branchSpan(match.branches), index);
                    branch.pat = try self.ownPat(branch.pat);
                    branch.bindings = try self.takeStatements(&taken, branch.bindings);
                    if (branch.guard != .none) branch.guard = .some(taken.one());
                    branch.body = taken.one();
                }
                match.branches = try program.addBranchSpan(branches);
            },
            .if_ => |*conditional| {
                const branches = try program.allocator.alloc(Ast.IfBranch, conditional.branches.len);
                defer program.allocator.free(branches);
                for (branches, 0..) |*branch, index| {
                    branch.* = GuardedList.at(program.ifBranchSpan(conditional.branches), index);
                    branch.cond = taken.one();
                    branch.body = taken.one();
                }
                conditional.branches = try program.addIfBranchSpan(branches);
                conditional.final_else = taken.one();
            },
            .block => |*block| {
                block.statements = try self.takeStatements(&taken, block.statements);
                block.final_expr = taken.one();
            },
        }
        std.debug.assert(taken.next == inside.len);
        return data;
    }

    /// How many expressions stand directly inside `data`.
    fn childCount(self: *Copier, data: Ast.ExprData) usize {
        const program = self.program;
        return switch (data) {
            .local,
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .crash,
            .checked_error,
            .comptime_exhaustiveness_failed,
            .lambda,
            .def_ref,
            .fn_def,
            => unreachable, // `begin` queues an assembly only for the forms below
            .let_ => 2,
            .list, .tuple => |items| items.len,
            .tag => |tag| tag.payloads.len,
            .low_level => |call| call.args.len,
            .continue_ => |transfer| transfer.values.len,
            .jump => |transfer| transfer.loop_values.len + transfer.args.len,
            .record => |fields| fields.len,
            .record_update => |update| 1 + update.fields.len,
            .fn_ref => |reference| reference.captures.len,
            .call_value => |call| 1 + call.args.len,
            .call_proc => |call| call.args.len + call.captures.len,
            .structural_eq, .structural_hash, .join_point => 2,
            .loop_ => |loop| loop.initial_values.len + 1,
            .nominal, .dbg, .expect, .typed_boundary, .row_widen, .return_, .field_access, .tuple_access, .expect_err, .literal_rejected, .comptime_value => 1,
            .break_ => |maybe_value| if (maybe_value == null) 0 else 1,
            .static_data_candidate => |candidate| if (candidate.value.subexpression() == null) 0 else 1,
            .if_initialized_payload => 3,
            .try_sequence, .try_record_sequence => 2,
            .match_ => |match| count: {
                var total: usize = 1;
                for (0..match.branches.len) |index| {
                    const branch = GuardedList.at(program.branchSpan(match.branches), index);
                    total += self.statementValueCount(branch.bindings) + 1;
                    if (branch.guard != .none) total += 1;
                }
                break :count total;
            },
            .if_ => |conditional| conditional.branches.len * 2 + 1,
            .block => |block| self.statementValueCount(block.statements) + 1,
        };
    }

    fn statementValueCount(self: *Copier, span: Ast.Span(Ast.StmtId)) usize {
        var total: usize = 0;
        for (0..span.len) |index| {
            switch (self.program.getStmt(GuardedList.at(self.program.stmtSpan(span), index))) {
                .let_, .expr, .expect, .dbg, .return_ => total += 1,
                .uninitialized, .crash, .checked_error => {},
            }
        }
        return total;
    }
};

const sequenced = @import("sequenced.zig");

test "a copied body names no statement or compound expression its source names" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    // One restored constant, read by two functions.
    const constant = try t.tuple(&.{ try t.tuple(&.{seven}), seven });
    const binding = try t.bind(constant);
    const body = try t.block(&.{binding}, seven);
    const first = try t.function(body);
    const second = try t.function(body);
    try std.testing.expectEqualStrings(
        "an expression with expressions inside it is used in two places",
        (try sequenced.check(&t.program)).?.message,
    );

    var function = t.program.getFn(second);
    function.body = .{ .roc = try copy(&t.program, body) };
    t.program.setFn(second, function);
    try std.testing.expectEqual(@as(?sequenced.Violation, null), try sequenced.check(&t.program));

    try std.testing.expectEqual(body, t.program.getFn(first).body.roc);
    const copied = t.program.getExpr(t.program.getFn(second).body.roc).data.block;
    try std.testing.expect(t.program.getFn(second).body.roc != body);
    // Leaves stay the ones the source names.
    try std.testing.expectEqual(seven, copied.final_expr);
    const copied_binding = GuardedList.at(t.program.stmtSpan(copied.statements), 0);
    try std.testing.expect(copied_binding != binding);
    const copied_constant = t.program.getStmt(copied_binding).let_.value;
    try std.testing.expect(copied_constant != constant);
    const items = t.program.getExpr(copied_constant).data.tuple;
    try std.testing.expect(GuardedList.at(t.program.exprSpan(items), 0) != GuardedList.at(t.program.exprSpan(t.program.getExpr(constant).data.tuple), 0));
    try std.testing.expectEqual(seven, GuardedList.at(t.program.exprSpan(items), 1));
}

test "a parent given its own copy of a shared expression keeps its id and everything else it names" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const seven = try t.int(7);
    // One restored constant, read once by one function and twice by another.
    const constant = try t.tuple(&.{ try t.tuple(&.{seven}), seven });
    const own = try t.tuple(&.{seven});
    _ = try t.function(try t.tuple(&.{constant}));
    const holder = try t.tuple(&.{ own, constant, constant });
    _ = try t.function(holder);
    try std.testing.expectEqualStrings(
        "an expression with expressions inside it is used in two places",
        (try sequenced.check(&t.program)).?.message,
    );

    try copyInside(&t.program, holder, &.{ constant, constant });
    try std.testing.expectEqual(@as(?sequenced.Violation, null), try sequenced.check(&t.program));

    const items = t.program.getExpr(holder).data.tuple;
    const second = GuardedList.at(t.program.exprSpan(items), 1);
    const third = GuardedList.at(t.program.exprSpan(items), 2);
    try std.testing.expectEqual(own, GuardedList.at(t.program.exprSpan(items), 0));
    try std.testing.expect(second != constant);
    try std.testing.expect(third != constant);
    try std.testing.expect(second != third);
    try std.testing.expectEqualDeep(t.program.getExpr(seven).data, t.program.getExpr(GuardedList.at(t.program.exprSpan(t.program.getExpr(second).data.tuple), 1)).data);
}

test "a renaming copy adds again only the expressions on a path to a renamed read" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const captured = try t.program.addLocal(@fromBackingInt(@intCast(1)), t.ty);
    const other = try t.program.addLocal(@fromBackingInt(@intCast(2)), t.ty);
    const own = try t.program.addLocal(@fromBackingInt(@intCast(3)), t.ty);
    const read_captured = try t.expr(.{ .local = captured });
    const untouched = try t.tuple(&.{ try t.expr(.{ .local = other }), try t.int(1) });
    const reading = try t.tuple(&.{read_captured});
    const root = try t.tuple(&.{ untouched, reading });

    var renames: Renames = .empty;
    defer renames.deinit(std.testing.allocator);
    try renames.put(std.testing.allocator, captured, own);

    const copied = try copyRenamingReads(&t.program, root, &renames);
    try std.testing.expect(copied != root);
    const items = t.program.exprSpan(t.program.getExpr(copied).data.tuple);
    try std.testing.expectEqual(untouched, GuardedList.at(items, 0));
    const copied_reading = GuardedList.at(items, 1);
    try std.testing.expect(copied_reading != reading);
    const read = GuardedList.at(t.program.exprSpan(t.program.getExpr(copied_reading).data.tuple), 0);
    try std.testing.expectEqual(own, t.program.getExpr(read).data.local);
    // The source keeps reading the captured local.
    try std.testing.expectEqual(captured, t.program.getExpr(read_captured).data.local);

    try std.testing.expectEqual(untouched, try copyRenamingReads(&t.program, untouched, &renames));
}

test "a renaming copy renames reads under a `let` that a SpecConstr body can hold" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const captured = try t.program.addLocal(@fromBackingInt(@intCast(1)), t.ty);
    const bound = try t.program.addLocal(@fromBackingInt(@intCast(2)), t.ty);
    const own = try t.program.addLocal(@fromBackingInt(@intCast(3)), t.ty);
    const bind = try t.program.addPat(.{ .ty = t.ty, .data = .{ .bind = bound } });
    const rest = try t.expr(.{ .local = bound });
    const root = try t.expr(.{ .let_ = .{ .bind = bind, .value = try t.expr(.{ .local = captured }), .rest = rest } });

    var renames: Renames = .empty;
    defer renames.deinit(std.testing.allocator);
    try renames.put(std.testing.allocator, captured, own);

    const copied = t.program.getExpr(try copyRenamingReads(&t.program, root, &renames)).data.let_;
    try std.testing.expectEqual(bind, copied.bind);
    try std.testing.expectEqual(rest, copied.rest);
    try std.testing.expectEqual(own, t.program.getExpr(copied.value).data.local);
}

test "a copy binding locals of its own names no local its source binds" {
    const allocator = std.testing.allocator;
    var t = try sequenced.TestProgram.init(allocator);
    defer t.deinit();
    // A closed initializer: a block binding a local, and a match whose
    // branch pattern binds another, both read inside.
    const binding = try t.bind(try t.int(7));
    const bound = t.program.getPat(t.program.getStmt(binding).let_.pat).data.bind;
    const read_bound = try t.expr(.{ .local = bound });
    const scrutinee = try t.tuple(&.{read_bound});
    const branch_local = try t.program.addLocal(@fromBackingInt(@intCast(50)), t.ty);
    const branch_pat = try t.program.addPat(.{ .ty = t.ty, .data = .{ .tuple = try t.program.addPatSpan(&.{
        try t.program.addPat(.{ .ty = t.ty, .data = .{ .bind = branch_local } }),
    }) } });
    const branch_body = try t.expr(.{ .local = branch_local });
    const match = try t.expr(.{ .match_ = .{
        .scrutinee = scrutinee,
        .branches = try t.program.addBranchSpan(&.{.{ .pat = branch_pat, .body = branch_body }}),
    } });
    const initializer = try t.block(&.{binding}, match);
    _ = try t.function(initializer);
    const second = try t.function(initializer);

    var groups = try t.program.bindingGroups(allocator);
    try std.testing.expect((try t.program.localSharedAcrossGroups(allocator, groups)) != null);
    groups.deinit(allocator);

    var symbols: Common.SymbolGen = .{ .next = t.program.next_symbol };
    var function = t.program.getFn(second);
    function.body = .{ .roc = try copyBindingOwnLocals(&t.program, initializer, &symbols) };
    t.program.setFn(second, function);
    t.program.next_symbol = symbols.next;

    try std.testing.expectEqual(@as(?sequenced.Violation, null), try sequenced.check(&t.program));
    groups = try t.program.bindingGroups(allocator);
    defer groups.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 2), groups.count());
    try std.testing.expectEqual(@as(?Ast.SharedLocal, null), try t.program.localSharedAcrossGroups(allocator, groups));

    // The copy reads the locals it binds, each carrying its source's identity.
    const copied = t.program.getExpr(function.body.roc).data.block;
    const copied_bound = t.program.getPat(t.program.getStmt(GuardedList.at(t.program.stmtSpan(copied.statements), 0)).let_.pat).data.bind;
    try std.testing.expect(copied_bound != bound);
    try std.testing.expectEqualDeep(t.program.getLocal(bound).identity, t.program.getLocal(copied_bound).identity);
    const copied_match = t.program.getExpr(copied.final_expr).data.match_;
    const copied_scrutinee = t.program.exprSpan(t.program.getExpr(copied_match.scrutinee).data.tuple);
    try std.testing.expectEqual(copied_bound, t.program.getExpr(GuardedList.at(copied_scrutinee, 0)).data.local);
    const copied_branch = GuardedList.at(t.program.branchSpan(copied_match.branches), 0);
    const copied_branch_local = t.program.getPat(GuardedList.at(t.program.patSpan(t.program.getPat(copied_branch.pat).data.tuple), 0)).data.bind;
    try std.testing.expect(copied_branch_local != branch_local);
    try std.testing.expectEqual(copied_branch_local, t.program.getExpr(copied_branch.body).data.local);
    // The source keeps its own locals.
    try std.testing.expectEqual(bound, t.program.getExpr(read_bound).data.local);
    try std.testing.expectEqual(branch_local, t.program.getExpr(branch_body).data.local);
}

test "a copy binding locals of its own keeps reading the locals bound around it" {
    var t = try sequenced.TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const outer = try t.program.addLocal(@fromBackingInt(@intCast(50)), t.ty);
    const read_outer = try t.expr(.{ .local = outer });
    const root = try t.tuple(&.{read_outer});
    var symbols: Common.SymbolGen = .{ .next = t.program.next_symbol };
    const copied = try copyBindingOwnLocals(&t.program, root, &symbols);
    try std.testing.expect(copied != root);
    try std.testing.expectEqual(read_outer, GuardedList.at(t.program.exprSpan(t.program.getExpr(copied).data.tuple), 0));
}

//! Closure lifting over Monotype IR.

const std = @import("std");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const MonoType = @import("../monotype/type.zig");
const Ast = @import("ast.zig");
const checked = @import("check").CheckedModule;

const Allocator = std.mem.Allocator;

/// Lift nested Monotype functions into explicit function bodies.
pub fn run(
    allocator: Allocator,
    mono: Mono.Program,
) Common.LowerError!Ast.Program {
    var owned = mono;
    errdefer owned.deinit();

    var name_store = owned.names;
    owned.names = @import("check").CheckedNames.NameStore.init(allocator);
    var types = owned.types;
    owned.types = @import("../monotype/type.zig").Store.init(allocator);
    var imported_fns = owned.imported_fns;
    owned.imported_fns = .empty;
    var exprs = owned.exprs;
    owned.exprs = .empty;
    var pats = owned.pats;
    owned.pats = .empty;
    var stmts = owned.stmts;
    owned.stmts = .empty;
    var locals = owned.locals;
    owned.locals = .empty;
    var expr_ids = owned.expr_ids;
    owned.expr_ids = .empty;
    var pat_ids = owned.pat_ids;
    owned.pat_ids = .empty;
    var typed_locals = owned.typed_locals;
    owned.typed_locals = .empty;
    var stmt_ids = owned.stmt_ids;
    owned.stmt_ids = .empty;
    var field_exprs = owned.field_exprs;
    owned.field_exprs = .empty;
    var fn_def_captures = owned.fn_def_captures;
    owned.fn_def_captures = .empty;
    var record_destructs = owned.record_destructs;
    owned.record_destructs = .empty;
    const str_pattern_steps = owned.str_pattern_steps;
    owned.str_pattern_steps = .empty;
    var branches = owned.branches;
    owned.branches = .empty;
    var if_branches = owned.if_branches;
    owned.if_branches = .empty;
    var string_literals = owned.string_literals;
    owned.string_literals = .empty;
    var proc_debug_names = owned.proc_debug_names;
    owned.proc_debug_names = Mono.ProcDebugNameMap.init(allocator);
    var runtime_schema_requests = owned.runtime_schema_requests;
    owned.runtime_schema_requests = .empty;
    var comptime_sites = owned.comptime_sites;
    owned.comptime_sites = .empty;
    var source_files = owned.source_files;
    owned.source_files = .empty;
    var expr_locs = owned.expr_locs;
    owned.expr_locs = .empty;
    var expr_regions = owned.expr_regions;
    owned.expr_regions = .empty;
    var stmt_locs = owned.stmt_locs;
    owned.stmt_locs = .empty;
    var stmt_regions = owned.stmt_regions;
    owned.stmt_regions = .empty;
    var local_names = owned.local_names;
    owned.local_names = .empty;

    var program = Ast.Program.init(
        allocator,
        name_store,
        types,
        imported_fns,
        exprs,
        pats,
        stmts,
        locals,
        expr_ids,
        pat_ids,
        typed_locals,
        stmt_ids,
        field_exprs,
        fn_def_captures,
        record_destructs,
        str_pattern_steps,
        branches,
        if_branches,
        string_literals,
        proc_debug_names,
        source_files,
        expr_locs,
        expr_regions,
        stmt_locs,
        stmt_regions,
        local_names,
        comptime_sites,
        owned.next_symbol,
    );
    name_store = undefined;
    types = undefined;
    imported_fns = undefined;
    exprs = undefined;
    pats = undefined;
    stmts = undefined;
    locals = undefined;
    expr_ids = undefined;
    pat_ids = undefined;
    typed_locals = undefined;
    stmt_ids = undefined;
    field_exprs = undefined;
    fn_def_captures = undefined;
    record_destructs = undefined;
    branches = undefined;
    if_branches = undefined;
    string_literals = undefined;
    proc_debug_names = undefined;
    source_files = undefined;
    expr_locs = undefined;
    expr_regions = undefined;
    stmt_locs = undefined;
    stmt_regions = undefined;
    local_names = undefined;
    comptime_sites = undefined;
    program.runtime_schema_requests = runtime_schema_requests;
    runtime_schema_requests = undefined;
    errdefer program.deinit();

    const source_view = movedMonoView(&owned, &program);
    var lifter = try Lifter.init(allocator, source_view, &program);
    defer lifter.deinit();

    try lifter.lowerDefsAndRoots();
    program.next_symbol = lifter.symbols.next;

    verifyCaptureInvariants(&program);

    owned.deinit();
    return program;
}

/// Build the read-only Monotype input view after side arrays have been moved
/// into the lifted output program. The source still owns definitions, roots,
/// and specialization metadata until `run` finishes.
fn movedMonoView(source: *const Mono.Program, moved: *const Ast.Program) Mono.ProgramView {
    return .{
        .names = &moved.names,
        .types = moved.types.view(),
        .specs = source.specs.items,
        .imported_fns = source.imported_fns.items,
        .fns = source.fns.items,
        .defs = source.defs.items,
        .nested_defs = source.nested_defs.items,
        .exprs = moved.exprs.items,
        .pats = moved.pats.items,
        .stmts = moved.stmts.items,
        .locals = moved.locals.items,
        .expr_ids = moved.expr_ids.items,
        .pat_ids = moved.pat_ids.items,
        .typed_locals = moved.typed_locals.items,
        .stmt_ids = moved.stmt_ids.items,
        .field_exprs = moved.field_exprs.items,
        .fn_def_captures = moved.fn_def_captures.items,
        .capture_operands = moved.capture_operands.items,
        .record_destructs = moved.record_destructs.items,
        .str_pattern_steps = moved.str_pattern_steps.items,
        .branches = moved.branches.items,
        .if_branches = moved.if_branches.items,
        .string_literals = moved.string_literals.items,
        .proc_debug_names = moved.proc_debug_names.items.items,
        .roots = source.roots.items,
        .layout_requests = source.layout_requests.items,
        .runtime_schema_requests = moved.runtime_schema_requests.items,
        .comptime_sites = moved.comptime_sites.items,
        .source_files = moved.source_files.items,
        .expr_locs = moved.expr_locs.items,
        .expr_regions = moved.expr_regions.items,
        .stmt_locs = moved.stmt_locs.items,
        .stmt_regions = moved.stmt_regions.items,
        .local_names = moved.local_names.items,
        .next_symbol = source.next_symbol,
    };
}

/// Recompute every lifted function's capture span from the current function
/// bodies, then rebase every function reference/direct-call capture operand
/// span to the recomputed capture slot order. Transformations that clone or
/// rewrite lifted bodies must call this after they finish mutating the program,
/// because substitutions can change the capture shape of the rewritten
/// functions.
pub fn recomputeCaptures(allocator: Allocator, program: *Ast.Program) Allocator.Error!void {
    const fn_captures = try allocateCaptureTable(allocator, program.fns.items.len);
    defer deinitCaptureTable(allocator, fn_captures);

    try solveCaptureFixpoint(allocator, program, fn_captures);
    try finalizeProgramFunctionReferenceCaptures(program, fn_captures);

    for (program.fns.items, 0..) |*fn_, index| {
        fn_.captures = try program.addTypedLocalSpan(fn_captures[index].items);
    }

    verifyCaptureInvariants(program);
}

/// Debug-only structural check that a Lifted program's capture representation is
/// internally consistent. It is run after every Lifted-IR mutation (the initial
/// lift and `recomputeCaptures`, which follows spec_constr / any body rewrite),
/// so a pass that forgets to maintain captures fails deterministically at its
/// own boundary instead of surfacing as a confusing crash five stages later.
/// Compiled out entirely in release builds — release cost is zero.
///
/// It checks, per function and per `fn_ref`/`call_proc` site:
///   - every capture slot's local carries a CaptureId, and the slot's type
///     agrees with that local's type;
///   - a function's capture slots are sorted by CaptureId with no duplicates;
///   - every binder-derived CaptureId names its local's live checked binder;
///   - an operand span carries exactly the target function's capture slots'
///     CaptureId sequence (same ids, same sorted order), and each operand's
///     value type equals its slot's type.
pub fn verifyCaptureInvariants(program: *const Ast.Program) void {
    if (@import("builtin").mode != .Debug) return;
    const violation = checkCaptureInvariants(program) catch |err| switch (err) {
        error.OutOfMemory => Common.invariant("verifyCaptureInvariants: out of memory during structural check"),
    };
    if (violation) |message| std.debug.panic("postcheck invariant violated: {s}", .{message});
}

/// The check itself, factored out of the panicking wrapper so it can be unit
/// tested: returns the first violated invariant's message, or null if the
/// program's capture representation is consistent.
pub fn checkCaptureInvariants(program: *const Ast.Program) Allocator.Error!?[]const u8 {
    for (program.fns.items) |fn_| {
        if (checkCaptureSlotSpan(program, program.typedLocalSpan(fn_.captures))) |message| return message;
    }

    for (program.exprs.items) |expr| {
        switch (expr.data) {
            .fn_ref => |fn_ref| if (try checkOperandSpan(program, fn_ref.fn_id, fn_ref.captures)) |message| return message,
            .call_proc => |call| switch (call.callee) {
                .lifted => |fn_id| if (try checkOperandSpan(program, fn_id, call.captures)) |message| return message,
                .func => {},
            },
            else => {},
        }
    }
    return null;
}

fn checkCaptureSlotSpan(program: *const Ast.Program, slots: []const Ast.TypedLocal) ?[]const u8 {
    var previous: ?checked.CaptureId = null;
    for (slots) |slot| {
        const local = program.locals.items[@intFromEnum(slot.local)];
        const id = local.capture_id orelse return "capture slot local had no CaptureId";
        if (slot.ty != local.ty) return "capture slot type disagreed with its local type";
        if (id.isCanonical()) {
            const binder = local.binder orelse return "binder-derived capture slot had no checked binder";
            if (id != checked.CaptureId.fromBinder(binder)) return "binder-derived CaptureId did not match its binder";
        }
        if (previous) |prev| {
            if (@intFromEnum(prev) > @intFromEnum(id)) return "capture slots not sorted by CaptureId";
            if (@intFromEnum(prev) == @intFromEnum(id)) return "duplicate CaptureId in a capture set";
        }
        previous = id;
    }
    return null;
}

fn checkOperandSpan(program: *const Ast.Program, fn_id: Ast.FnId, operand_span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!?[]const u8 {
    const slots = program.typedLocalSpan(program.fns.items[@intFromEnum(fn_id)].captures);
    const operands = program.captureOperandSpan(operand_span);
    if (slots.len != operands.len) return "operand count differed from target capture slot count";
    for (slots, operands) |slot, operand| {
        if (operand.id != slotCaptureId(program, slot)) return "operand CaptureId did not match its slot";
        // Types are compared structurally: monomorphization/specialization may
        // give the operand value and its slot distinct interned TypeIds for the
        // same type.
        const value_ty = program.exprs.items[@intFromEnum(operand.value)].ty;
        if (!try program.types.typeEql(&program.names, value_ty, slot.ty)) {
            return "operand value type differed from its capture slot type";
        }
    }
    return null;
}

const DefMap = []?Ast.FnId;
const NestedDefMap = []?Ast.FnId;
const FnMap = []?Ast.FnId;

const MonoFnBody = struct {
    args: Mono.Span(Mono.TypedLocal),
    body: Mono.FnBody,
};

const Lifter = struct {
    allocator: Allocator,
    source: Mono.ProgramView,
    output: *Ast.Program,
    expr_done: []bool,
    stmt_done: []bool,
    def_map: DefMap,
    nested_def_map: NestedDefMap,
    fn_map: FnMap,
    fn_bodies: std.ArrayList(?MonoFnBody),
    nested_fn_ids: std.AutoHashMap(Ast.FnId, void),
    initialized_fns: std.AutoHashMap(Ast.FnId, void),
    symbols: Common.SymbolGen,
    /// Solved capture set per lifted function, indexed by `Ast.FnId`. Computed
    /// as a least fixed point over the function-reference graph before any body
    /// is rewritten, so it never depends on lifting order or rewrite-collapsed
    /// nodes. Every later stage reads this rather than re-deriving captures by
    /// walking (possibly already-rewritten) bodies.
    fn_captures: []std.ArrayList(Ast.TypedLocal),

    fn init(allocator: Allocator, source: Mono.ProgramView, output: *Ast.Program) Allocator.Error!Lifter {
        const expr_done = try allocator.alloc(bool, output.exprCount());
        errdefer allocator.free(expr_done);
        @memset(expr_done, false);

        const stmt_done = try allocator.alloc(bool, output.stmtCount());
        errdefer allocator.free(stmt_done);
        @memset(stmt_done, false);

        return .{
            .allocator = allocator,
            .source = source,
            .output = output,
            .expr_done = expr_done,
            .stmt_done = stmt_done,
            .def_map = &.{},
            .nested_def_map = &.{},
            .fn_map = &.{},
            .fn_bodies = .empty,
            .nested_fn_ids = std.AutoHashMap(Ast.FnId, void).init(allocator),
            .initialized_fns = std.AutoHashMap(Ast.FnId, void).init(allocator),
            .symbols = .{ .next = source.next_symbol },
            .fn_captures = &.{},
        };
    }

    fn deinit(self: *Lifter) void {
        for (self.fn_captures) |*captures| captures.deinit(self.allocator);
        if (self.fn_captures.len > 0) self.allocator.free(self.fn_captures);
        self.initialized_fns.deinit();
        self.nested_fn_ids.deinit();
        self.fn_bodies.deinit(self.allocator);
        if (self.fn_map.len > 0) self.allocator.free(self.fn_map);
        if (self.nested_def_map.len > 0) self.allocator.free(self.nested_def_map);
        if (self.def_map.len > 0) self.allocator.free(self.def_map);
        self.allocator.free(self.stmt_done);
        self.allocator.free(self.expr_done);
    }

    fn lowerDefsAndRoots(self: *Lifter) Allocator.Error!void {
        self.fn_map = try self.allocator.alloc(?Ast.FnId, self.source.fns.len);
        @memset(self.fn_map, null);

        self.def_map = try self.allocator.alloc(?Ast.FnId, self.source.defs.len);
        @memset(self.def_map, null);

        for (self.source.defs, 0..) |def, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
            try self.output.fns.append(self.allocator, undefined);
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = def.body });
            self.def_map[index] = fn_id;
            if (def.fn_id) |source_fn_id| self.registerFn(source_fn_id, fn_id);
        }

        self.nested_def_map = try self.allocator.alloc(?Ast.FnId, self.source.nested_defs.len);
        @memset(self.nested_def_map, null);
        for (self.source.nested_defs, 0..) |def, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
            try self.output.fns.append(self.allocator, undefined);
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = .{ .roc = def.body } });
            self.nested_def_map[index] = fn_id;
            try self.nested_fn_ids.put(fn_id, {});
            self.registerFn(def.fn_id, fn_id);
        }

        try self.computeCaptureFixpoint();

        for (self.source.defs, 0..) |def, index| {
            try self.lowerTopLevelDef(self.def_map[index] orelse
                Common.invariant("Monotype definition was not reserved before lifting"), def);
        }

        for (self.source.nested_defs, 0..) |def, index| {
            try self.lowerNestedDef(self.nested_def_map[index] orelse
                Common.invariant("Monotype nested definition was not reserved before lifting"), def);
        }

        for (self.output.fns.items, 0..) |_, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            if (self.initialized_fns.contains(fn_id)) continue;
            Common.invariant("Monotype Lifted function was reserved but not initialized");
        }

        try self.completeFunctionReferenceCaptures();

        for (self.source.roots) |root| {
            const raw = @intFromEnum(root.def);
            if (raw >= self.def_map.len) Common.invariant("Monotype root references a missing definition");
            const fn_id = self.def_map[raw] orelse
                Common.invariant("Monotype root definition was not lifted");
            try self.output.roots.append(self.allocator, .{
                .fn_id = fn_id,
                .request = root.request,
            });
        }

        for (self.source.layout_requests) |request| {
            const fn_id = if (request.def) |def| blk: {
                const raw = @intFromEnum(def);
                if (raw >= self.def_map.len) Common.invariant("Monotype static data layout request references a missing definition");
                break :blk self.def_map[raw] orelse
                    Common.invariant("Monotype static data layout request definition was not lifted");
            } else null;
            try self.output.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = request.ty,
                .fn_id = fn_id,
            });
        }
    }

    fn completeFunctionReferenceCaptures(self: *Lifter) Allocator.Error!void {
        for (self.expr_done, 0..) |done, index| {
            if (!done) continue;
            const expr_id: Ast.ExprId = @enumFromInt(@as(u32, @intCast(index)));
            switch (self.output.exprs.items[index].data) {
                .fn_ref => |fn_ref| {
                    const slots = self.output.typedLocalSpan(self.output.fns.items[@intFromEnum(fn_ref.fn_id)].captures);
                    if (slots.len == 0 and fn_ref.captures.len == 0) continue;
                    const captures = try rebuildCaptureOperandSpan(self.output, fn_ref.captures, slots, expr_id);
                    self.output.exprs.items[index].data = .{ .fn_ref = .{
                        .fn_id = fn_ref.fn_id,
                        .captures = captures,
                    } };
                },
                .call_proc => |call| {
                    const fn_id = Ast.localDirectCallee(call) orelse continue;
                    const slots = self.output.typedLocalSpan(self.output.fns.items[@intFromEnum(fn_id)].captures);
                    if (slots.len == 0 and call.captures.len == 0) continue;
                    const captures = try rebuildCaptureOperandSpan(self.output, call.captures, slots, expr_id);
                    self.output.exprs.items[index].data = .{ .call_proc = .{
                        .callee = call.callee,
                        .args = call.args,
                        .captures = captures,
                        .is_cold = call.is_cold,
                    } };
                },
                else => {},
            }
        }
    }

    fn lowerTopLevelDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.Def) Allocator.Error!void {
        if (self.fn_captures[@intFromEnum(fn_id)].items.len != 0) {
            Common.invariant("top-level Monotype definition has free locals after checked closure collection");
        }

        const body: Ast.FnBody = switch (def.body) {
            .roc => |body| blk: {
                try self.rewriteExpr(body);
                break :blk .{ .roc = body };
            },
            .hosted => .hosted,
        };
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = def.symbol,
            .source = if (def.fn_id) |source_fn_id| self.defSource(source_fn_id, def.fn_def) else null,
            .args = def.args,
            .captures = .empty(),
            .body = body,
            .ret = def.ret,
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn lowerNestedDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.NestedDef) Allocator.Error!void {
        try self.rewriteExpr(def.body);
        const capture_span = try self.output.addTypedLocalSpan(self.fn_captures[@intFromEnum(fn_id)].items);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = def.symbol,
            .source = self.nestedSource(def.fn_id, def.fn_def),
            .args = def.args,
            .captures = capture_span,
            .body = .{ .roc = def.body },
            .ret = def.ret,
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn rewriteStmt(self: *Lifter, stmt_id: Mono.StmtId) Allocator.Error!void {
        const index = @intFromEnum(stmt_id);
        if (self.stmt_done[index]) return;
        self.stmt_done[index] = true;

        switch (self.output.stmts.items[index]) {
            .uninitialized => {},
            .let_ => |let_| try self.rewriteExpr(let_.value),
            .expr,
            .expect,
            .dbg,
            => |expr| try self.rewriteExpr(expr),
            .return_ => |ret| try self.rewriteExpr(ret.value),
            .crash => {},
        }
    }

    fn rewriteExpr(self: *Lifter, expr_id: Mono.ExprId) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (self.expr_done[index]) return;
        self.expr_done[index] = true;

        const expr = &self.output.exprs.items[index];
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
            .crash,
            .comptime_exhaustiveness_failed,
            => {},
            .fn_ref => |fn_ref| for (self.output.captureOperandSpan(fn_ref.captures)) |operand| try self.rewriteExpr(operand.value),
            .list,
            .tuple,
            => |items| for (self.output.exprSpan(items)) |child| try self.rewriteExpr(child),
            .record => |fields| for (self.output.fieldExprSpan(fields)) |field| try self.rewriteExpr(field.value),
            .tag => |tag| for (self.output.exprSpan(tag.payloads)) |child| try self.rewriteExpr(child),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.rewriteExpr(child),
            .return_ => |ret| try self.rewriteExpr(ret.value),
            .expect_err => |expect_err| try self.rewriteExpr(expect_err.msg),
            .comptime_branch_taken => |taken| try self.rewriteExpr(taken.body),
            .let_ => |let_| {
                try self.rewriteExpr(let_.value);
                try self.rewriteExpr(let_.rest);
            },
            .lambda => |lambda| try self.liftLambda(expr_id, expr.ty, lambda),
            .def_ref => |def_id| {
                const raw = @intFromEnum(def_id);
                if (raw >= self.def_map.len) Common.invariant("Monotype definition reference was outside the definition table");
                const fn_id = self.def_map[raw] orelse
                    Common.invariant("Monotype definition reference reached lifting before its function was registered");
                const captures = try self.captureExprSpanForFn(fn_id, expr_id);
                self.output.exprs.items[index].data = .{ .fn_ref = .{
                    .fn_id = fn_id,
                    .captures = captures,
                } };
            },
            .fn_def => |fn_def| {
                for (self.output.fnDefCaptureSpan(fn_def.captures)) |capture| try self.rewriteExpr(capture.value);
                const lifted = self.liftedFn(fn_def.fn_id);
                const captures = try self.fnRefCaptureExprSpanForFnDef(lifted, fn_def.captures, expr_id);
                self.output.exprs.items[index].data = .{ .fn_ref = .{
                    .fn_id = lifted,
                    .captures = captures,
                } };
            },
            .call_value => |call| {
                try self.rewriteExpr(call.callee);
                for (self.output.exprSpan(call.args)) |arg| try self.rewriteExpr(arg);
            },
            .call_proc => |call| {
                for (self.output.exprSpan(call.args)) |arg| try self.rewriteExpr(arg);
                for (self.output.captureOperandSpan(call.captures)) |operand| try self.rewriteExpr(operand.value);
                const RewrittenProcCall = struct {
                    callee: Mono.ProcCallee,
                    captures: Ast.Span(Ast.ExprId),
                };
                const rewritten: RewrittenProcCall = switch (call.callee) {
                    .func => |slot| switch (slot) {
                        .local => |mono_fn_id| blk: {
                            const fn_id = self.liftedFn(mono_fn_id);
                            break :blk .{
                                .callee = .{ .lifted = fn_id },
                                .captures = if (call.captures.len == 0)
                                    try self.captureExprSpanForFn(fn_id, expr_id)
                                else
                                    call.captures,
                            };
                        },
                        .imported => |imported| .{
                            .callee = .{ .func = .{ .imported = imported } },
                            .captures = call.captures,
                        },
                    },
                    .lifted => |fn_id| .{
                        .callee = .{ .lifted = fn_id },
                        .captures = if (call.captures.len == 0)
                            try self.captureExprSpanForFn(fn_id, expr_id)
                        else
                            call.captures,
                    },
                };
                self.output.exprs.items[index].data = .{ .call_proc = .{
                    .callee = rewritten.callee,
                    .args = call.args,
                    .captures = rewritten.captures,
                    .is_cold = call.is_cold,
                } };
            },
            .low_level => |call| for (self.output.exprSpan(call.args)) |arg| try self.rewriteExpr(arg),
            .field_access => |field| try self.rewriteExpr(field.receiver),
            .tuple_access => |access| try self.rewriteExpr(access.tuple),
            .structural_eq => |eq| {
                try self.rewriteExpr(eq.lhs);
                try self.rewriteExpr(eq.rhs);
            },
            .structural_hash => |h| {
                try self.rewriteExpr(h.value);
                try self.rewriteExpr(h.hasher);
            },
            .match_ => |match| {
                try self.rewriteExpr(match.scrutinee);
                for (self.output.branchSpan(match.branches)) |branch| {
                    if (branch.guard) |guard| try self.rewriteExpr(guard);
                    try self.rewriteExpr(branch.body);
                }
            },
            .if_ => |if_| {
                for (self.output.ifBranchSpan(if_.branches)) |branch| {
                    try self.rewriteExpr(branch.cond);
                    try self.rewriteExpr(branch.body);
                }
                try self.rewriteExpr(if_.final_else);
            },
            .if_initialized_payload => |payload_switch| {
                try self.rewriteExpr(payload_switch.cond);
                try self.rewriteExpr(payload_switch.initialized);
                try self.rewriteExpr(payload_switch.uninitialized);
            },
            .try_sequence => |sequence| {
                try self.rewriteExpr(sequence.try_expr);
                try self.rewriteExpr(sequence.ok_body);
            },
            .try_record_sequence => |sequence| {
                try self.rewriteExpr(sequence.try_expr);
                try self.rewriteExpr(sequence.ok_body);
            },
            .block => |block| {
                for (self.output.stmtSpan(block.statements)) |stmt| try self.rewriteStmt(stmt);
                try self.rewriteExpr(block.final_expr);
            },
            .loop_ => |loop| {
                for (self.output.exprSpan(loop.initial_values)) |initial| try self.rewriteExpr(initial);
                try self.rewriteExpr(loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.rewriteExpr(value),
            .continue_ => |continue_| for (self.output.exprSpan(continue_.values)) |value| try self.rewriteExpr(value),
        }
    }

    fn liftLambda(self: *Lifter, expr_id: Mono.ExprId, ty: @import("../monotype/type.zig").TypeId, lambda: Mono.LambdaExpr) Allocator.Error!void {
        const fn_id = try self.reserveFn(lambda.fn_id);
        if (self.nested_fn_ids.contains(fn_id) or self.initialized_fns.contains(fn_id)) {
            const captures = try self.captureExprSpanForFn(fn_id, expr_id);
            self.output.exprs.items[@intFromEnum(expr_id)].data = .{ .fn_ref = .{
                .fn_id = fn_id,
                .captures = captures,
            } };
            return;
        }

        try self.setFnBody(fn_id, .{ .args = lambda.args, .body = .{ .roc = lambda.body } });

        // Inline lambdas are never the target of a direct/devirtualized call
        // (those resolve to defs or nested defs), so they need no fixpoint
        // entry: their captures are computed here, reading the already-solved
        // capture sets of any defs they reference and descending inline into
        // their own nested lambdas.
        var captures = CaptureSet.init(self);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.output, &bound, self.output.typedLocalSpan(lambda.args));
        try captures.collectExpr(lambda.body, &bound);
        sortCaptureSlots(self.output, captures.items.items);

        const capture_exprs = try self.captureOperandSpanForSlots(captures.items.items, &.{}, expr_id);
        self.output.exprs.items[@intFromEnum(expr_id)].data = .{ .fn_ref = .{
            .fn_id = fn_id,
            .captures = capture_exprs,
        } };

        try self.rewriteExpr(lambda.body);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = self.symbols.fresh(),
            .source = self.source.fnSource(lambda.fn_id),
            .args = lambda.args,
            .captures = capture_span,
            .body = .{ .roc = lambda.body },
            .ret = functionRet(&self.output.types, ty),
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn reserveFn(self: *Lifter, mono_fn_id: Mono.FnId) Allocator.Error!Ast.FnId {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype lambda referenced a missing function specialization");
        if (self.fn_map[raw]) |existing| return existing;

        const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
        try self.output.fns.append(self.allocator, undefined);
        try self.fn_bodies.append(self.allocator, null);
        self.fn_map[raw] = fn_id;
        return fn_id;
    }

    fn setFnBody(self: *Lifter, fn_id: Ast.FnId, body: MonoFnBody) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.fn_bodies.items.len) Common.invariant("lifted function body id was outside body table");
        self.fn_bodies.items[raw] = body;
    }

    /// Solve every function's capture set as a fixed point over the
    /// function-reference graph. Each function's captures are the free locals
    /// of its body, where a reference to another function contributes that
    /// callee's current captures filtered by the locals bound at the reference
    /// site. The stored order is the body's discovery order under the final
    /// callee sets.
    fn computeCaptureFixpoint(self: *Lifter) Allocator.Error!void {
        // `count` covers top-level defs and nested defs only; inline lambdas are
        // reserved later, during `rewriteExpr`/`liftLambda`. Sizing here is what
        // keeps inline lambdas out of the fixpoint, which is sound because they
        // are never the target of a reference that reaches `collectFnCaptures`
        // (only defs and nested defs are) — an invariant that function enforces.
        const count = self.output.fns.items.len;
        self.fn_captures = try allocateCaptureTable(self.allocator, count);

        var scratch = CaptureSet.init(self);
        defer scratch.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();

        var changed = true;
        while (changed) {
            changed = false;
            for (0..count) |raw| {
                const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(raw)));
                try self.solveInto(fn_id, &scratch, &bound);
                if (!captureListEql(scratch.items.items, self.fn_captures[raw].items)) {
                    self.fn_captures[raw].clearRetainingCapacity();
                    try self.fn_captures[raw].appendSlice(self.allocator, scratch.items.items);
                    changed = true;
                }
            }
        }
    }

    /// Solve one function's captures into the reusable `scratch`/`bound`,
    /// reading the current solved sets of referenced functions. Both scratch
    /// buffers are cleared first.
    fn solveInto(self: *Lifter, fn_id: Ast.FnId, scratch: *CaptureSet, bound: *BoundSet) Allocator.Error!void {
        scratch.clear();
        bound.clear();
        const body = self.fn_bodies.items[@intFromEnum(fn_id)] orelse return;
        try bindTypedLocals(self.output, bound, self.output.typedLocalSpan(body.args));
        switch (body.body) {
            .roc => |expr| try scratch.collectExpr(expr, bound),
            .hosted => {},
        }
        sortCaptureSlots(self.output, scratch.items.items);
    }

    fn registerFn(self: *Lifter, mono_fn_id: Mono.FnId, fn_id: Ast.FnId) void {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype definition referenced a missing function specialization");
        if (self.fn_map[raw]) |existing| {
            if (existing != fn_id) Common.invariant("Monotype function specialization was assigned two lifted function ids");
            return;
        }
        self.fn_map[raw] = fn_id;
    }

    fn liftedFn(self: *Lifter, mono_fn_id: Mono.FnId) Ast.FnId {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype expression referenced a missing function specialization");
        return self.fn_map[raw] orelse
            Common.invariant("Monotype expression referenced a function specialization before lifting registered it");
    }

    fn captureExprSpanForFn(self: *Lifter, fn_id: Ast.FnId, call_expr: Mono.ExprId) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        return try self.captureOperandSpanForSlots(self.fn_captures[@intFromEnum(fn_id)].items, &.{}, call_expr);
    }

    /// Build the keyed capture operand span for a function reference. `slots`
    /// are the target function's canonically-sorted capture slots; the result
    /// is one operand per slot, in the same sorted order, so the join is an
    /// exact keyed walk. An operand's value comes from a matching explicit
    /// pre-lift capture operand (`explicit`, keyed by CaptureId) when present,
    /// otherwise an implicit read of the slot's local in the reference context.
    fn captureOperandSpanForSlots(
        self: *Lifter,
        slots: []const Ast.TypedLocal,
        explicit: []const Ast.FnDefCapture,
        call_expr: Mono.ExprId,
    ) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        if (slots.len == 0) return .empty();

        const saved_loc = self.output.current_loc;
        defer self.output.current_loc = saved_loc;
        const saved_region = self.output.current_region;
        defer self.output.current_region = saved_region;
        const call_loc = self.output.exprLoc(call_expr);
        if (call_loc.hasLocation()) self.output.current_loc = call_loc;
        const call_region = self.output.exprRegion(call_expr);
        if (!call_region.isEmpty()) self.output.current_region = call_region;

        const operands = try self.allocator.alloc(Ast.CaptureOperand, slots.len);
        defer self.allocator.free(operands);
        for (slots, 0..) |slot, index| {
            const id = slotCaptureId(self.output, slot);
            const value = explicitCaptureValueForId(self.output, explicit, id) orelse
                try self.output.addExpr(.{ .ty = slot.ty, .data = .{ .local = slot.local } });
            operands[index] = .{ .id = id, .value = value };
        }
        return try self.output.addCaptureOperandSpan(operands);
    }

    fn fnRefCaptureExprSpanForFnDef(
        self: *Lifter,
        fn_id: Ast.FnId,
        explicit_span: Ast.Span(Ast.FnDefCapture),
        call_expr: Mono.ExprId,
    ) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        const explicit = self.output.fnDefCaptureSpan(explicit_span);
        return try self.captureOperandSpanForSlots(self.fn_captures[@intFromEnum(fn_id)].items, explicit, call_expr);
    }

    fn defSource(self: *Lifter, mono_fn_id: Mono.FnId, expected: ?Mono.FnTemplate) ?Mono.FnTemplate {
        const source = self.source.fnSource(mono_fn_id);
        if (expected) |template| {
            if (!std.meta.eql(source, template)) {
                Common.invariant("Monotype definition source disagreed with its function specialization source");
            }
        }
        return source;
    }

    fn nestedSource(self: *Lifter, mono_fn_id: Mono.FnId, expected: Mono.FnTemplate) Mono.FnTemplate {
        const source = self.source.fnSource(mono_fn_id);
        if (!std.meta.eql(source, expected)) {
            Common.invariant("Monotype nested definition source disagreed with its function specialization source");
        }
        return source;
    }
};

fn allocateCaptureTable(allocator: Allocator, count: usize) Allocator.Error![]std.ArrayList(Ast.TypedLocal) {
    const captures = try allocator.alloc(std.ArrayList(Ast.TypedLocal), count);
    for (captures) |*capture| capture.* = .empty;
    return captures;
}

fn deinitCaptureTable(allocator: Allocator, captures: []std.ArrayList(Ast.TypedLocal)) void {
    for (captures) |*capture| capture.deinit(allocator);
    if (captures.len > 0) allocator.free(captures);
}

/// Find the existing operand value that supplies capture `id`.
///
/// For a plain local read whose value-local carries a CaptureId, that CaptureId
/// is authoritative — not the operand's stored id: spec_constr substitution can
/// replace an operand's value with a local of a different capture while leaving
/// the stored id stale, so the value's current identity wins.
///
/// When the value-local carries no CaptureId (e.g. a spec_constr-minted arg or
/// temp local produced by argument splitting/inlining), no value identity is
/// available, so the operand's stored id — set when the operand was built for a
/// specific slot — is the only identity and is honored when present. Genuinely
/// explicit (non-local) values likewise fall back to their stored id. A
/// value-id match always takes precedence over the stored id.
fn operandValueForSlotId(program: *const Ast.Program, existing: []const Ast.CaptureOperand, id: checked.CaptureId) ?Ast.ExprId {
    var fallback_by_id: ?Ast.ExprId = null;
    for (existing) |operand| {
        switch (program.exprs.items[@intFromEnum(operand.value)].data) {
            .local => |local| {
                if (program.locals.items[@intFromEnum(local)].capture_id) |value_id| {
                    if (value_id == id) return operand.value;
                } else if (operand.id == id and fallback_by_id == null) {
                    fallback_by_id = operand.value;
                }
            },
            else => if (operand.id == id and fallback_by_id == null) {
                fallback_by_id = operand.value;
            },
        }
    }
    return fallback_by_id;
}

/// Recompute a function reference / direct call's keyed capture operand span so it
/// matches `slots` (the target's canonically-sorted capture slots) exactly, in
/// the same order. Each operand's value is preserved from the node's existing
/// operands (keyed by CaptureId) when present — this keeps explicit non-local
/// values supplied at checked closure creation and const-fn restore — otherwise
/// it is an implicit read of the slot's local at the reference site.
fn rebuildCaptureOperandSpan(
    program: *Ast.Program,
    existing_span: Ast.Span(Ast.CaptureOperand),
    slots: []const Ast.TypedLocal,
    call_expr: Ast.ExprId,
) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
    if (slots.len == 0) return .empty();

    const existing = program.captureOperandSpan(existing_span);

    const saved_loc = program.current_loc;
    defer program.current_loc = saved_loc;
    const saved_region = program.current_region;
    defer program.current_region = saved_region;
    const call_loc = program.exprLoc(call_expr);
    if (call_loc.hasLocation()) program.current_loc = call_loc;
    const call_region = program.exprRegion(call_expr);
    if (!call_region.isEmpty()) program.current_region = call_region;

    const operands = try program.allocator.alloc(Ast.CaptureOperand, slots.len);
    defer program.allocator.free(operands);
    for (slots, 0..) |slot, index| {
        const id = slotCaptureId(program, slot);
        const value = operandValueForSlotId(program, existing, id) orelse
            try program.addExpr(.{ .ty = slot.ty, .data = .{ .local = slot.local } });
        operands[index] = .{ .id = id, .value = value };
    }
    return try program.addCaptureOperandSpan(operands);
}

/// Recompute every function reference / direct call's capture operand span to
/// match the recomputed capture slots in `fn_captures`. Used after a Lifted-IR
/// mutation (spec_constr, inlining) reshapes capture sets.
fn finalizeProgramFunctionReferenceCaptures(
    program: *Ast.Program,
    fn_captures: []std.ArrayList(Ast.TypedLocal),
) Allocator.Error!void {
    const expr_count = program.exprs.items.len;
    for (0..expr_count) |index| {
        const expr_id: Ast.ExprId = @enumFromInt(@as(u32, @intCast(index)));
        switch (program.exprs.items[index].data) {
            .fn_ref => |fn_ref| {
                const fn_index = @intFromEnum(fn_ref.fn_id);
                if (fn_index >= fn_captures.len) Common.invariant("function reference target missing recomputed captures");

                // Always recompute: even when the target's capture set is
                // unchanged, a caller-side operand value can have been rewritten
                // by spec_constr, so the operand span must be re-derived from the
                // current values, keyed to the target's recomputed slots.
                const new_captures = fn_captures[fn_index].items;
                const captures = try rebuildCaptureOperandSpan(program, fn_ref.captures, new_captures, expr_id);
                program.exprs.items[index].data = .{ .fn_ref = .{
                    .fn_id = fn_ref.fn_id,
                    .captures = captures,
                } };
            },
            .call_proc => |call| {
                const fn_id = switch (call.callee) {
                    .lifted => |fn_id| fn_id,
                    .func => continue,
                };
                const fn_index = @intFromEnum(fn_id);
                if (fn_index >= fn_captures.len) Common.invariant("direct call target missing recomputed captures");

                const new_captures = fn_captures[fn_index].items;
                const captures = try rebuildCaptureOperandSpan(program, call.captures, new_captures, expr_id);
                program.exprs.items[index].data = .{ .call_proc = .{
                    .callee = call.callee,
                    .args = call.args,
                    .captures = captures,
                    .is_cold = call.is_cold,
                } };
            },
            else => {},
        }
    }
}

fn solveCaptureFixpoint(
    allocator: Allocator,
    program: *Ast.Program,
    fn_captures: []std.ArrayList(Ast.TypedLocal),
) Allocator.Error!void {
    var scratch = CaptureSet.initForProgram(allocator, program, fn_captures);
    defer scratch.deinit();
    var bound = BoundSet.init(allocator);
    defer bound.deinit();

    var changed = true;
    while (changed) {
        changed = false;
        for (program.fns.items, 0..) |fn_, raw| {
            scratch.clear();
            bound.clear();

            try bindTypedLocals(program, &bound, program.typedLocalSpan(fn_.args));
            switch (fn_.body) {
                .roc => |expr| try scratch.collectExpr(expr, &bound),
                .hosted => {},
            }
            sortCaptureSlots(program, scratch.items.items);

            if (!captureListEql(scratch.items.items, fn_captures[raw].items)) {
                fn_captures[raw].clearRetainingCapacity();
                try fn_captures[raw].appendSlice(allocator, scratch.items.items);
                changed = true;
            }
        }
    }
}

fn captureListEql(lhs: []const Ast.TypedLocal, rhs: []const Ast.TypedLocal) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |a, b| {
        if (a.local != b.local or a.ty != b.ty) return false;
    }
    return true;
}

/// The CaptureId of a capture slot. Every capture slot's
/// local carries a CaptureId (assigned at creation for binder-backed and
/// compile-time locals, in `addIfFree` for lift-synthesized locals).
fn slotCaptureId(program: *const Ast.Program, slot: Ast.TypedLocal) checked.CaptureId {
    return program.locals.items[@intFromEnum(slot.local)].capture_id orelse
        Common.invariant("lifted capture slot local had no CaptureId");
}

fn captureSlotLessThan(program: *const Ast.Program, lhs: Ast.TypedLocal, rhs: Ast.TypedLocal) bool {
    return @intFromEnum(slotCaptureId(program, lhs)) < @intFromEnum(slotCaptureId(program, rhs));
}

/// Sort a capture set into ascending CaptureId order so operand↔slot joins are
/// an exact keyed walk and slot order is never load-bearing. Also asserts (in
/// debug) that no CaptureId appears twice.
fn sortCaptureSlots(program: *const Ast.Program, items: []Ast.TypedLocal) void {
    std.sort.pdq(Ast.TypedLocal, items, program, captureSlotLessThan);
    if (@import("builtin").mode == .Debug) {
        var index: usize = 1;
        while (index < items.len) : (index += 1) {
            if (slotCaptureId(program, items[index - 1]) == slotCaptureId(program, items[index])) {
                Common.invariant("lifted capture set contained two slots with the same CaptureId");
            }
        }
    }
}

/// Find the operand value supplied for `id` among explicit pre-lift capture
/// operands, keyed by the CaptureId of each operand's local.
fn explicitCaptureValueForId(program: *const Ast.Program, explicit: []const Ast.FnDefCapture, id: checked.CaptureId) ?Ast.ExprId {
    for (explicit) |capture| {
        const capture_id = program.locals.items[@intFromEnum(capture.local)].capture_id orelse
            Common.invariant("pre-lift capture operand local had no CaptureId");
        if (capture_id == id) return capture.value;
    }
    return null;
}

/// Whether an explicit pre-lift capture operand supplies the given CaptureId.
fn explicitProvidesCaptureId(program: *const Ast.Program, explicit: []const Ast.FnDefCapture, id: checked.CaptureId) bool {
    return explicitCaptureValueForId(program, explicit, id) != null;
}

const BoundSet = struct {
    locals: std.AutoHashMap(Mono.LocalId, void),
    binders: std.AutoHashMap(checked.PatternBinderId, u32),

    fn init(allocator: Allocator) BoundSet {
        return .{
            .locals = std.AutoHashMap(Mono.LocalId, void).init(allocator),
            .binders = std.AutoHashMap(checked.PatternBinderId, u32).init(allocator),
        };
    }

    fn deinit(self: *BoundSet) void {
        self.binders.deinit();
        self.locals.deinit();
    }

    fn contains(self: *const BoundSet, input: *const Ast.Program, local: Mono.LocalId) bool {
        if (self.locals.contains(local)) return true;
        const local_data = input.locals.items[@intFromEnum(local)];
        return if (local_data.binder) |binder| self.binders.contains(binder) else false;
    }

    fn put(self: *BoundSet, input: *const Ast.Program, local: Mono.LocalId) Allocator.Error!void {
        try self.locals.put(local, {});
        if (input.locals.items[@intFromEnum(local)].binder) |binder| try self.putBinder(binder);
    }

    fn remove(self: *BoundSet, input: *const Ast.Program, local: Mono.LocalId) void {
        _ = self.locals.remove(local);
        if (input.locals.items[@intFromEnum(local)].binder) |binder| self.removeBinder(binder);
    }

    fn putBinder(self: *BoundSet, binder: checked.PatternBinderId) Allocator.Error!void {
        const entry = try self.binders.getOrPut(binder);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    fn removeBinder(self: *BoundSet, binder: checked.PatternBinderId) void {
        const count = self.binders.getPtr(binder) orelse
            Common.invariant("capture collection removed an unbound checked binder");
        if (count.* > 1) {
            count.* -= 1;
        } else {
            _ = self.binders.remove(binder);
        }
    }

    /// Reset for reuse across fixpoint solves without freeing capacity.
    fn clear(self: *BoundSet) void {
        self.locals.clearRetainingCapacity();
        self.binders.clearRetainingCapacity();
    }
};

const CaptureSet = struct {
    allocator: Allocator,
    lifter: ?*Lifter,
    program: *Ast.Program,
    fn_captures: []std.ArrayList(Ast.TypedLocal),
    items: std.ArrayList(Ast.TypedLocal),
    seen: std.AutoHashMap(Mono.LocalId, void),

    fn init(lifter: *Lifter) CaptureSet {
        return .{
            .allocator = lifter.allocator,
            .lifter = lifter,
            .program = lifter.output,
            .fn_captures = lifter.fn_captures,
            .items = .empty,
            .seen = std.AutoHashMap(Mono.LocalId, void).init(lifter.allocator),
        };
    }

    fn initForProgram(
        allocator: Allocator,
        program: *Ast.Program,
        fn_captures: []std.ArrayList(Ast.TypedLocal),
    ) CaptureSet {
        return .{
            .allocator = allocator,
            .lifter = null,
            .program = program,
            .fn_captures = fn_captures,
            .items = .empty,
            .seen = std.AutoHashMap(Mono.LocalId, void).init(allocator),
        };
    }

    fn deinit(self: *CaptureSet) void {
        self.seen.deinit();
        self.items.deinit(self.allocator);
    }

    /// Reset for reuse across fixpoint solves without freeing capacity.
    fn clear(self: *CaptureSet) void {
        self.items.clearRetainingCapacity();
        self.seen.clearRetainingCapacity();
    }

    fn addIfFree(self: *CaptureSet, local: Mono.LocalId, bound: *const BoundSet) Allocator.Error!void {
        if (bound.contains(self.program, local) or self.seen.contains(local)) return;
        try self.seen.put(local, {});
        // Every capture slot needs a stable CaptureId. Binder-backed and
        // compile-time-synthesized locals already carry one; a local that is
        // free here without one is lift-synthesized (e.g. a spec_constr temp),
        // so mint a generated identity that travels with the local.
        if (self.program.locals.items[@intFromEnum(local)].capture_id == null) {
            self.program.locals.items[@intFromEnum(local)].capture_id = self.program.nextLiftCaptureId();
        }
        const local_data = self.program.locals.items[@intFromEnum(local)];
        try self.items.append(self.allocator, .{
            .local = local,
            .ty = local_data.ty,
        });
    }

    fn collectExpr(self: *CaptureSet, expr_id: Mono.ExprId, bound: *BoundSet) Allocator.Error!void {
        const input = self.program;
        const expr = input.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local => |local| try self.addIfFree(local, bound),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .uninitialized,
            .uninitialized_payload,
            .def_ref,
            .crash,
            .comptime_exhaustiveness_failed,
            => {},
            .fn_ref => |fn_ref| for (input.captureOperandSpan(fn_ref.captures)) |operand| try self.collectExpr(operand.value, bound),
            .fn_def => |fn_def| {
                const lifter = self.lifter orelse Common.invariant("post-lift capture recomputation saw a pre-lift function definition");
                const explicit = input.fnDefCaptureSpan(fn_def.captures);
                if (explicit.len == 0) {
                    try self.collectFnCaptures(lifter.liftedFn(fn_def.fn_id), bound);
                } else {
                    try self.collectFnCapturesExceptExplicit(lifter.liftedFn(fn_def.fn_id), explicit, bound);
                    for (explicit) |capture| try self.collectExpr(capture.value, bound);
                }
            },
            .list,
            .tuple,
            => |items| for (input.exprSpan(items)) |child| try self.collectExpr(child, bound),
            .record => |fields| for (input.fieldExprSpan(fields)) |field| try self.collectExpr(field.value, bound),
            .tag => |tag| for (input.exprSpan(tag.payloads)) |child| try self.collectExpr(child, bound),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.collectExpr(child, bound),
            .return_ => |ret| try self.collectExpr(ret.value, bound),
            .expect_err => |expect_err| try self.collectExpr(expect_err.msg, bound),
            .comptime_branch_taken => |taken| try self.collectExpr(taken.body, bound),
            .let_ => |let_| {
                try self.collectExpr(let_.value, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindPat(self.allocator, input, let_.bind, bound, &added);
                try self.collectExpr(let_.rest, bound);
                removeBound(input, bound, added.items);
            },
            .lambda => |lambda| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(lambda.args), &added);
                try self.collectExpr(lambda.body, bound);
                removeBound(input, bound, added.items);
            },
            .call_value => |call| {
                try self.collectExpr(call.callee, bound);
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound);
            },
            .call_proc => |call| {
                switch (call.callee) {
                    .func => |slot| switch (slot) {
                        .local => |mono_fn_id| {
                            const lifter = self.lifter orelse Common.invariant("post-lift capture recomputation saw a pre-lift function call");
                            try self.collectFnCaptures(lifter.liftedFn(mono_fn_id), bound);
                        },
                        .imported => {},
                    },
                    .lifted => |fn_id| try self.collectFnCaptures(fn_id, bound),
                }
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound);
                for (input.captureOperandSpan(call.captures)) |operand| try self.collectExpr(operand.value, bound);
            },
            .low_level => |call| for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound),
            .field_access => |field| try self.collectExpr(field.receiver, bound),
            .tuple_access => |access| try self.collectExpr(access.tuple, bound),
            .structural_eq => |eq| {
                try self.collectExpr(eq.lhs, bound);
                try self.collectExpr(eq.rhs, bound);
            },
            .structural_hash => |h| {
                try self.collectExpr(h.value, bound);
                try self.collectExpr(h.hasher, bound);
            },
            .match_ => |match| {
                try self.collectExpr(match.scrutinee, bound);
                for (input.branchSpan(match.branches)) |branch| {
                    var added = std.ArrayList(Mono.LocalId).empty;
                    defer added.deinit(self.allocator);
                    try bindPat(self.allocator, input, branch.pat, bound, &added);
                    if (branch.guard) |guard| try self.collectExpr(guard, bound);
                    try self.collectExpr(branch.body, bound);
                    removeBound(input, bound, added.items);
                }
            },
            .if_ => |if_| {
                for (input.ifBranchSpan(if_.branches)) |branch| {
                    try self.collectExpr(branch.cond, bound);
                    try self.collectExpr(branch.body, bound);
                }
                try self.collectExpr(if_.final_else, bound);
            },
            .if_initialized_payload => |payload_switch| {
                try self.collectExpr(payload_switch.cond, bound);
                try self.addIfFree(payload_switch.payload, bound);
                try self.collectExpr(payload_switch.initialized, bound);
                try self.collectExpr(payload_switch.uninitialized, bound);
            },
            .try_sequence => |sequence| {
                try self.collectExpr(sequence.try_expr, bound);
                try bound.put(input, sequence.ok_local);
                try self.collectExpr(sequence.ok_body, bound);
                _ = bound.remove(input, sequence.ok_local);
            },
            .try_record_sequence => |sequence| {
                try self.collectExpr(sequence.try_expr, bound);
                try bound.put(input, sequence.value_local);
                try bound.put(input, sequence.rest_local);
                try self.collectExpr(sequence.ok_body, bound);
                _ = bound.remove(input, sequence.rest_local);
                _ = bound.remove(input, sequence.value_local);
            },
            .block => |block| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                for (input.stmtSpan(block.statements)) |stmt| try self.collectStmt(input, stmt, bound, &added);
                try self.collectExpr(block.final_expr, bound);
                removeBound(input, bound, added.items);
            },
            .loop_ => |loop| {
                for (input.exprSpan(loop.initial_values)) |initial| try self.collectExpr(initial, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(loop.params), &added);
                try self.collectExpr(loop.body, bound);
                removeBound(input, bound, added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectExpr(value, bound),
            .continue_ => |continue_| for (input.exprSpan(continue_.values)) |value| try self.collectExpr(value, bound),
        }
    }

    /// Contribute a referenced function's solved captures to the current set,
    /// filtered by the locals bound at the reference site. Reads the solved
    /// set rather than re-walking the callee's body, so it is correct even
    /// after the callee's body has been rewritten and never under-approximates
    /// recursive references. During the fixpoint the read set is the previous
    /// round's value, which is exactly what makes recursion converge.
    fn collectFnCaptures(self: *CaptureSet, fn_id: Ast.FnId, caller_bound: *BoundSet) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        // Only defs and nested defs are reachable here (direct and
        // devirtualized calls and `fn_def` references never target an inline
        // lambda), and every one has a fixpoint entry. An out-of-range id
        // means an earlier stage produced a call target the fixpoint never saw.
        if (raw >= self.fn_captures.len) Common.invariant("capture collection referenced a function without a solved capture set");
        for (self.fn_captures[raw].items) |capture| {
            try self.addIfFree(capture.local, caller_bound);
        }
    }

    fn collectFnCapturesExceptExplicit(
        self: *CaptureSet,
        fn_id: Ast.FnId,
        explicit: []const Ast.FnDefCapture,
        caller_bound: *BoundSet,
    ) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.fn_captures.len) Common.invariant("capture collection referenced a function without a solved capture set");
        for (self.fn_captures[raw].items) |capture| {
            const id = slotCaptureId(self.program, capture);
            if (explicitProvidesCaptureId(self.program, explicit, id)) continue;
            try self.addIfFree(capture.local, caller_bound);
        }
    }

    fn collectStmt(self: *CaptureSet, input: *const Ast.Program, stmt_id: Mono.StmtId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
        switch (input.stmts.items[@intFromEnum(stmt_id)]) {
            .uninitialized => |pat| try bindPat(self.allocator, input, pat, bound, added),
            .let_ => |let_| {
                if (let_.recursive) {
                    try bindPat(self.allocator, input, let_.pat, bound, added);
                    try self.collectExpr(let_.value, bound);
                } else {
                    try self.collectExpr(let_.value, bound);
                    try bindPat(self.allocator, input, let_.pat, bound, added);
                }
            },
            .expr,
            .expect,
            .dbg,
            => |expr| try self.collectExpr(expr, bound),
            .return_ => |ret| try self.collectExpr(ret.value, bound),
            .crash => {},
        }
    }
};

fn bindTypedLocals(input: *const Ast.Program, bound: *BoundSet, locals: []const Mono.TypedLocal) Allocator.Error!void {
    for (locals) |local| try bound.put(input, local.local);
}

fn bindTypedLocalsTracked(allocator: Allocator, input: *const Ast.Program, bound: *BoundSet, locals: []const Mono.TypedLocal, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    for (locals) |local| {
        try bound.put(input, local.local);
        try added.append(allocator, local.local);
    }
}

fn bindPat(allocator: Allocator, input: *const Ast.Program, pat_id: Mono.PatId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    switch (input.pats.items[@intFromEnum(pat_id)].data) {
        .bind => |local| {
            try bound.put(input, local);
            try added.append(allocator, local);
        },
        .wildcard,
        .int_lit,
        .dec_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .str_lit,
        => {},
        .str_pattern => |str| {
            for (input.strPatternStepSpan(str.steps)) |step| {
                if (step.capture) |capture| {
                    try bindPat(allocator, input, capture, bound, added);
                }
            }
        },
        .as => |as| {
            try bindPat(allocator, input, as.pattern, bound, added);
            try bound.put(input, as.local);
            try added.append(allocator, as.local);
        },
        .record => |fields| for (input.recordDestructSpan(fields)) |field| try bindPat(allocator, input, field.pattern, bound, added),
        .tuple => |items| for (input.patSpan(items)) |child| try bindPat(allocator, input, child, bound, added),
        .list => |list| {
            for (input.patSpan(list.patterns)) |child| try bindPat(allocator, input, child, bound, added);
            if (list.rest) |rest| if (rest.pattern) |rest_pattern| try bindPat(allocator, input, rest_pattern, bound, added);
        },
        .tag => |tag| for (input.patSpan(tag.payloads)) |child| try bindPat(allocator, input, child, bound, added),
        .nominal => |backing| try bindPat(allocator, input, backing, bound, added),
    }
}

fn removeBound(input: *const Ast.Program, bound: *BoundSet, locals: []const Mono.LocalId) void {
    var index = locals.len;
    while (index > 0) {
        index -= 1;
        bound.remove(input, locals[index]);
    }
}

fn functionRet(types: *const MonoType.Store, ty: MonoType.TypeId) MonoType.TypeId {
    return switch (shapeContent(types, ty)) {
        .func => |fn_ty| fn_ty.ret,
        else => Common.invariant("lifted lambda expression did not have a function type"),
    };
}

fn shapeContent(types: *const MonoType.Store, ty: MonoType.TypeId) MonoType.Content {
    var current = ty;
    while (true) {
        switch (types.get(current)) {
            .named => |named| if (named.backing) |backing| {
                current = backing.ty;
                continue;
            } else {
                return types.get(current);
            },
            else => |content| return content,
        }
    }
}

test "monotype lifting preserves imported direct call slots" {
    const allocator = std.testing.allocator;
    var mono = Mono.Program.init(allocator);
    errdefer mono.deinit();

    const unit_ty = try mono.types.add(.zst);
    const imported = try mono.addImportedFn(.{
        .shard = @enumFromInt(1),
        .fn_id = @enumFromInt(1),
    });
    const body = try mono.addExpr(.{ .ty = unit_ty, .data = .{ .call_proc = .{
        .callee = Mono.importedProcCallee(imported),
        .args = Mono.Span(Mono.ExprId).empty(),
    } } });
    try mono.defs.append(allocator, .{
        .symbol = @enumFromInt(1),
        .args = Mono.Span(Mono.TypedLocal).empty(),
        .body = .{ .roc = body },
        .ret = unit_ty,
    });

    var lifted = try run(allocator, mono);
    defer lifted.deinit();

    try std.testing.expectEqual(@as(usize, 1), lifted.imported_fns.items.len);
    const call = switch (lifted.exprs.items[@intFromEnum(body)].data) {
        .call_proc => |call| call,
        else => return error.TestUnexpectedResult,
    };
    switch (call.callee) {
        .func => |slot| switch (slot) {
            .imported => |actual| try std.testing.expectEqual(imported, actual),
            .local => return error.TestUnexpectedResult,
        },
        .lifted => return error.TestUnexpectedResult,
    }
}

test "checkCaptureInvariants accepts a well-formed capture and catches a corrupted operand" {
    const allocator = std.testing.allocator;
    var program = Ast.Program.init(
        allocator,
        @import("check").CheckedNames.NameStore.init(allocator),
        MonoType.Store.init(allocator),
        .empty, // imported_fns
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // fn_def_captures
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        Ast.ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // local_names
        .empty, // comptime_sites
        0, // next_symbol
    );
    defer program.deinit();

    // One capturing function: a single binder-backed capture slot, and a
    // function reference that supplies it with a keyed operand.
    const ty = try program.types.add(.zst);
    const binder: checked.PatternBinderId = @enumFromInt(1);
    const cap_local = try program.addLocalWithBinder(@enumFromInt(1), ty, binder);
    const cap_span = try program.addTypedLocalSpan(&.{.{ .local = cap_local, .ty = ty }});
    const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(program.fns.items.len)));
    try program.fns.append(allocator, .{
        .symbol = @enumFromInt(1),
        .args = Ast.Span(Ast.TypedLocal).empty(),
        .captures = cap_span,
        .body = .hosted,
        .ret = ty,
    });
    const value = try program.addExpr(.{ .ty = ty, .data = .{ .local = cap_local } });
    const op_span = try program.addCaptureOperandSpan(&.{.{ .id = checked.CaptureId.fromBinder(binder), .value = value }});
    _ = try program.addExpr(.{ .ty = ty, .data = .{ .fn_ref = .{
        .fn_id = fn_id,
        .captures = op_span,
    } } });

    // A well-formed capture representation reports no violation.
    try std.testing.expectEqual(@as(?[]const u8, null), try checkCaptureInvariants(&program));

    // Intentionally skip capture maintenance: give the operand a CaptureId that
    // no longer matches its slot. The debug pass must catch it deterministically.
    program.capture_operands.items[0].id = checked.CaptureId.fromBinder(@enumFromInt(2));
    try std.testing.expectEqualStrings(
        "operand CaptureId did not match its slot",
        (try checkCaptureInvariants(&program)).?,
    );
}

test "monotype lifted lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}

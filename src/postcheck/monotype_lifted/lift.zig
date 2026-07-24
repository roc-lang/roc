//! Closure lifting over Monotype IR.

const std = @import("std");
const collections = @import("collections");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const MonoType = @import("../monotype/type.zig");
const Ast = @import("ast.zig");
const checked = @import("check").CheckedModule;

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

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
    var imported_fns = owned.imported_fns.takeArrayList();
    var exprs = owned.exprs.takeArrayList();
    var pats = owned.pats.takeArrayList();
    var stmts = owned.stmts.takeArrayList();
    var locals = owned.locals.takeArrayList();
    var expr_ids = owned.expr_ids.takeArrayList();
    var pat_ids = owned.pat_ids.takeArrayList();
    var typed_locals = owned.typed_locals.takeArrayList();
    var stmt_ids = owned.stmt_ids.takeArrayList();
    var field_exprs = owned.field_exprs.takeArrayList();
    var fn_def_captures = owned.fn_def_captures.takeArrayList();
    var const_evidence_pool = owned.const_evidence_pool.takeArrayList();
    var const_evidence_chain_pool = owned.const_evidence_chain_pool.takeArrayList();
    var record_destructs = owned.record_destructs.takeArrayList();
    var str_pattern_steps = owned.str_pattern_steps.takeArrayList();
    var branches = owned.branches.takeArrayList();
    var if_branches = owned.if_branches.takeArrayList();
    var string_literals = owned.string_literals.takeArrayList();
    var proc_debug_names = owned.proc_debug_names;
    owned.proc_debug_names = Mono.ProcDebugNameMap.init(allocator);
    var runtime_schema_requests = owned.runtime_schema_requests.takeArrayList();
    var comptime_sites = owned.comptime_sites.takeArrayList();
    var source_files = owned.source_files.takeArrayList();
    var expr_locs = owned.expr_locs.takeArrayList();
    var expr_regions = owned.expr_regions.takeArrayList();
    var stmt_locs = owned.stmt_locs.takeArrayList();
    var stmt_regions = owned.stmt_regions.takeArrayList();
    var local_names = owned.local_names.takeArrayList();
    var static_data_values = owned.static_data_values.takeArrayList();

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
        static_data_values,
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
    str_pattern_steps = undefined;
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
    static_data_values = undefined;
    comptime_sites = undefined;
    program.runtime_schema_requests = Ast.ProgramList(Ast.RuntimeSchemaRequest, "runtime_schema_requests").fromArrayList(runtime_schema_requests);
    runtime_schema_requests = undefined;
    program.const_evidence_pool = Ast.ProgramList(@import("check").ConstStore.ConstEvidence, "const_evidence_pool").fromArrayList(const_evidence_pool);
    const_evidence_pool = undefined;
    program.const_evidence_chain_pool = Ast.ProgramList(@import("check").ConstStore.ConstRange, "const_evidence_chain_pool").fromArrayList(const_evidence_chain_pool);
    const_evidence_chain_pool = undefined;
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
    const source_view = source.view();
    const moved_view = moved.view();
    return .{
        .names = &moved.names,
        .types = moved.types.view(),
        .specs = source_view.specs,
        .imported_fns = source_view.imported_fns,
        .fns = source_view.fns,
        .defs = source_view.defs,
        .nested_defs = source_view.nested_defs,
        .exprs = moved_view.exprs,
        .pats = moved_view.pats,
        .stmts = moved_view.stmts,
        .locals = moved_view.locals,
        .expr_ids = moved_view.expr_ids,
        .pat_ids = moved_view.pat_ids,
        .typed_locals = moved_view.typed_locals,
        .stmt_ids = moved_view.stmt_ids,
        .field_exprs = moved_view.field_exprs,
        .fn_def_captures = moved_view.fn_def_captures,
        .const_evidence_pool = moved_view.const_evidence_pool,
        .const_evidence_chain_pool = moved_view.const_evidence_chain_pool,
        .capture_operands = moved_view.capture_operands,
        .record_destructs = moved_view.record_destructs,
        .str_pattern_steps = moved_view.str_pattern_steps,
        .branches = moved_view.branches,
        .if_branches = moved_view.if_branches,
        .string_literals = moved_view.string_literals,
        .proc_debug_names = moved.proc_debug_names.view(),
        .roots = source_view.roots,
        .layout_requests = source_view.layout_requests,
        .runtime_schema_requests = moved_view.runtime_schema_requests,
        .static_data_values = moved_view.static_data_values,
        .comptime_sites = moved_view.comptime_sites,
        .source_files = moved_view.source_files,
        .expr_locs = moved_view.expr_locs,
        .expr_regions = moved_view.expr_regions,
        .stmt_locs = moved_view.stmt_locs,
        .stmt_regions = moved_view.stmt_regions,
        .local_names = moved_view.local_names,
        .next_symbol = source.next_symbol,
    };
}

/// Recompute every lifted function's capture span from the current function
/// bodies, then rebase every reachable function reference/direct-call capture
/// operand span to the recomputed capture slot order. Transformations that
/// clone or rewrite lifted bodies must call this after they finish mutating the
/// program, because substitutions can change the capture shape of the rewritten
/// functions. Replaced bodies remain in the append-only expression store but
/// are not part of the executable program and therefore need no capture
/// operands.
pub fn recomputeCaptures(allocator: Allocator, program: *Ast.Program) Allocator.Error!void {
    var graph = try CaptureDependencyGraph.init(allocator, program, null);
    defer graph.deinit();
    try graph.buildPostLift();
    try graph.solve();
    try graph.finalizePostLiftOperands();

    for (0..program.fnCount()) |index| {
        const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
        program.setFnCaptures(fn_id, try program.addTypedLocalSpan(graph.states[index].captures.items));
    }

    verifyActiveCaptureInvariants(program, &graph);
}

/// Verify the capture representation that is reachable from current function
/// bodies. Post-lift transforms use append-only stores, so expressions from
/// replaced bodies can retain capture operands for the old function shapes;
/// they are deliberately absent from the active capture graph and cannot reach
/// downstream compilation.
fn verifyActiveCaptureInvariants(program: *const Ast.Program, graph: *const CaptureDependencyGraph) void {
    if (@import("builtin").mode != .Debug) return;
    const violation = checkActiveCaptureInvariants(program, graph) catch |err| switch (err) {
        error.OutOfMemory => Common.invariant("verifyActiveCaptureInvariants: out of memory during structural check"),
    };
    if (violation) |message| std.debug.panic("postcheck invariant violated: {s}", .{message});
}

/// Debug-only structural check that a freshly lifted program's entire capture
/// backing store is internally consistent. Post-lift transforms use append-only
/// stores and can leave replaced expressions behind; `recomputeCaptures` checks
/// the active capture graph instead. Both checks fail at the mutation boundary
/// instead of surfacing as a confusing crash five stages later. Compiled out
/// entirely in release builds — release cost is zero.
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
    for (program.fnsView()) |fn_| {
        if (checkCaptureSlotSpan(program, program.typedLocalSpan(fn_.captures))) |message| return message;
    }

    for (program.exprsView()) |expr| {
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

fn checkActiveCaptureInvariants(program: *const Ast.Program, graph: *const CaptureDependencyGraph) Allocator.Error!?[]const u8 {
    for (program.fnsView()) |fn_| {
        if (checkCaptureSlotSpan(program, program.typedLocalSpan(fn_.captures))) |message| return message;
    }

    for (graph.edges.items) |edge| {
        if (!edge.active) continue;
        switch (edge.site) {
            .pre_lift => return "post-lift capture graph contained an active pre-lift edge",
            .fn_ref => |expr_id| {
                const fn_ref = switch (program.getExpr(expr_id).data) {
                    .fn_ref => |fn_ref| fn_ref,
                    else => return "active capture graph function-reference site changed expression kind",
                };
                if (fn_ref.fn_id != edge.target) return "active capture graph function-reference target changed";
                if (try checkOperandSpan(program, edge.target, fn_ref.captures)) |message| return message;
            },
            .call_proc => |expr_id| {
                const call = switch (program.getExpr(expr_id).data) {
                    .call_proc => |call| call,
                    else => return "active capture graph direct-call site changed expression kind",
                };
                const target = switch (call.callee) {
                    .lifted => |fn_id| fn_id,
                    .func => return "active capture graph direct-call target changed kind",
                };
                if (target != edge.target) return "active capture graph direct-call target changed";
                if (try checkOperandSpan(program, edge.target, call.captures)) |message| return message;
            },
        }
    }
    return null;
}

fn checkCaptureSlotSpan(program: *const Ast.Program, slots: anytype) ?[]const u8 {
    var previous: ?checked.CaptureId = null;
    for (0..slots.len) |index| {
        const slot = GuardedList.at(slots, index);
        const local = program.getLocal(slot.local);
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
    const slots = program.typedLocalSpan(program.getFn(fn_id).captures);
    const operands = program.captureOperandSpan(operand_span);
    if (slots.len != operands.len) return "operand count differed from target capture slot count";
    for (0..slots.len) |index| {
        const slot = GuardedList.at(slots, index);
        const operand = GuardedList.at(operands, index);
        if (operand.id != slotCaptureId(program, slot)) return "operand CaptureId did not match its slot";
        // Types are compared structurally: monomorphization/specialization may
        // give the operand value and its slot distinct interned TypeIds for the
        // same type.
        const value_ty = program.getExpr(operand.value).ty;
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
            const fn_id = try self.output.reserveFnSlot();
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = def.body });
            self.def_map[index] = fn_id;
            if (def.fn_id) |source_fn_id| self.registerFn(source_fn_id, fn_id);
        }

        self.nested_def_map = try self.allocator.alloc(?Ast.FnId, self.source.nested_defs.len);
        @memset(self.nested_def_map, null);
        for (self.source.nested_defs, 0..) |def, index| {
            const fn_id = try self.output.reserveFnSlot();
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

        for (0..self.output.fnCount()) |index| {
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
            try self.output.addRoot(.{
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
            try self.output.addLayoutRequest(.{
                .checked_type = request.checked_type,
                .ty = request.ty,
                .fn_id = fn_id,
                .const_locator = request.const_locator,
            });
        }
    }

    fn completeFunctionReferenceCaptures(self: *Lifter) Allocator.Error!void {
        try finalizeProgramFunctionReferenceCaptures(self.output, self.fn_captures);
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
        self.output.setFn(fn_id, .{
            .symbol = def.symbol,
            .source = if (def.fn_id) |source_fn_id| self.defSource(source_fn_id, def.fn_def) else null,
            .args = def.args,
            .captures = .empty(),
            .body = body,
            .ret = def.ret,
        });
        try self.initialized_fns.put(fn_id, {});
    }

    fn lowerNestedDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.NestedDef) Allocator.Error!void {
        try self.rewriteExpr(def.body);
        const capture_span = try self.output.addTypedLocalSpan(self.fn_captures[@intFromEnum(fn_id)].items);
        self.output.setFn(fn_id, .{
            .symbol = def.symbol,
            .source = self.nestedSource(def.fn_id, def.fn_def),
            .args = def.args,
            .captures = capture_span,
            .body = .{ .roc = def.body },
            .ret = def.ret,
        });
        try self.initialized_fns.put(fn_id, {});
    }

    fn rewriteStmt(self: *Lifter, stmt_id: Mono.StmtId) Allocator.Error!void {
        const index = @intFromEnum(stmt_id);
        if (self.stmt_done[index]) return;
        self.stmt_done[index] = true;

        switch (self.output.getStmt(stmt_id)) {
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

    fn rewriteExprSpan(self: *Lifter, span: Ast.Span(Ast.ExprId)) Allocator.Error!void {
        const exprs = self.output.exprSpan(span);
        for (0..exprs.len) |index| try self.rewriteExpr(GuardedList.at(exprs, index));
    }

    fn rewriteFieldExprSpan(self: *Lifter, span: Ast.Span(Ast.FieldExpr)) Allocator.Error!void {
        const fields = self.output.fieldExprSpan(span);
        for (0..fields.len) |index| try self.rewriteExpr(GuardedList.at(fields, index).value);
    }

    fn rewriteCaptureOperandSpan(self: *Lifter, span: Ast.Span(Ast.CaptureOperand)) Allocator.Error!void {
        const operands = self.output.captureOperandSpan(span);
        for (0..operands.len) |index| try self.rewriteExpr(GuardedList.at(operands, index).value);
    }

    fn rewriteFnDefCaptureSpan(self: *Lifter, span: Ast.Span(Ast.FnDefCapture)) Allocator.Error!void {
        const captures = self.output.fnDefCaptureSpan(span);
        for (0..captures.len) |index| try self.rewriteExpr(GuardedList.at(captures, index).value);
    }

    fn rewriteBranchSpan(self: *Lifter, span: Ast.Span(Ast.Branch)) Allocator.Error!void {
        const branches = self.output.branchSpan(span);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            if (branch.guard) |guard| try self.rewriteExpr(guard);
            try self.rewriteExpr(branch.body);
        }
    }

    fn rewriteIfBranchSpan(self: *Lifter, span: Ast.Span(Ast.IfBranch)) Allocator.Error!void {
        const branches = self.output.ifBranchSpan(span);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            try self.rewriteExpr(branch.cond);
            try self.rewriteExpr(branch.body);
        }
    }

    fn rewriteStmtSpan(self: *Lifter, span: Ast.Span(Ast.StmtId)) Allocator.Error!void {
        const statements = self.output.stmtSpan(span);
        for (0..statements.len) |index| try self.rewriteStmt(GuardedList.at(statements, index));
    }

    fn rewriteExpr(self: *Lifter, expr_id: Mono.ExprId) Allocator.Error!void {
        const index = @intFromEnum(expr_id);
        if (self.expr_done[index]) return;
        self.expr_done[index] = true;

        const expr = self.output.getExpr(expr_id);
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
            .fn_ref => |fn_ref| {
                const operands = self.output.captureOperandSpan(fn_ref.captures);
                for (0..operands.len) |operand_index| {
                    const operand = GuardedList.at(operands, operand_index);
                    try self.rewriteExpr(operand.value);
                }
            },
            .list,
            .tuple,
            => |items| try self.rewriteExprSpan(items),
            .record => |fields| try self.rewriteFieldExprSpan(fields),
            .tag => |tag| try self.rewriteExprSpan(tag.payloads),
            .static_data_candidate => |candidate| try self.rewriteExpr(candidate.runtime_expr),
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
                self.output.setExprData(expr_id, .{ .fn_ref = .{
                    .fn_id = fn_id,
                    .captures = captures,
                } });
            },
            .fn_def => |fn_def| {
                try self.rewriteFnDefCaptureSpan(fn_def.captures);
                const lifted = self.liftedFn(fn_def.fn_id);
                const captures = try self.fnRefCaptureExprSpanForFnDef(lifted, fn_def.captures, expr_id);
                self.output.setExprData(expr_id, .{ .fn_ref = .{
                    .fn_id = lifted,
                    .captures = captures,
                } });
            },
            .call_value => |call| {
                try self.rewriteExpr(call.callee);
                try self.rewriteExprSpan(call.args);
            },
            .call_proc => |call| {
                try self.rewriteExprSpan(call.args);
                try self.rewriteCaptureOperandSpan(call.captures);
                const RewrittenProcCall = struct {
                    callee: Mono.ProcCallee,
                    captures: Ast.Span(Ast.CaptureOperand),
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
                self.output.setExprData(expr_id, .{ .call_proc = .{
                    .callee = rewritten.callee,
                    .args = call.args,
                    .captures = rewritten.captures,
                    .is_cold = call.is_cold,
                } });
            },
            .low_level => |call| try self.rewriteExprSpan(call.args),
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
                try self.rewriteBranchSpan(match.branches);
            },
            .if_ => |if_| {
                try self.rewriteIfBranchSpan(if_.branches);
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
                try self.rewriteStmtSpan(block.statements);
                try self.rewriteExpr(block.final_expr);
            },
            .loop_ => |loop| {
                try self.rewriteExprSpan(loop.initial_values);
                try self.rewriteExpr(loop.body);
            },
            .break_ => |maybe| if (maybe) |value| try self.rewriteExpr(value),
            .continue_ => |continue_| try self.rewriteExprSpan(continue_.values),
            .join_point,
            .jump,
            => Common.invariant("lifted join-point control reached Monotype lifting"),
        }
    }

    fn liftLambda(self: *Lifter, expr_id: Mono.ExprId, ty: @import("../monotype/type.zig").TypeId, lambda: Mono.LambdaExpr) Allocator.Error!void {
        const fn_id = try self.reserveFn(lambda.fn_id);
        if (self.nested_fn_ids.contains(fn_id) or self.initialized_fns.contains(fn_id)) {
            const captures = try self.captureExprSpanForFn(fn_id, expr_id);
            self.output.setExprData(expr_id, .{ .fn_ref = .{
                .fn_id = fn_id,
                .captures = captures,
            } });
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
        self.output.setExprData(expr_id, .{ .fn_ref = .{
            .fn_id = fn_id,
            .captures = capture_exprs,
        } });

        try self.rewriteExpr(lambda.body);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        self.output.setFn(fn_id, .{
            .symbol = self.symbols.fresh(),
            .source = self.source.fnSource(lambda.fn_id),
            .args = lambda.args,
            .captures = capture_span,
            .body = .{ .roc = lambda.body },
            .ret = functionRet(&self.output.types, ty),
        });
        try self.initialized_fns.put(fn_id, {});
    }

    fn reserveFn(self: *Lifter, mono_fn_id: Mono.FnId) Allocator.Error!Ast.FnId {
        const raw = @intFromEnum(mono_fn_id);
        if (raw >= self.fn_map.len) Common.invariant("Monotype lambda referenced a missing function specialization");
        if (self.fn_map[raw]) |existing| return existing;

        const fn_id = try self.output.reserveFnSlot();
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
    /// function-reference graph. Each function body and conditional capture
    /// operand expression is summarized once; a worklist then propagates only
    /// newly discovered captures through affected reference edges.
    fn computeCaptureFixpoint(self: *Lifter) Allocator.Error!void {
        // `count` covers top-level defs and nested defs only; inline lambdas are
        // reserved later, during `rewriteExpr`/`liftLambda`. Sizing here is what
        // keeps inline lambdas out of the graph, which is sound because they are
        // never direct-call targets. Their captures are computed when lifting
        // reaches the lambda, from the already-solved def/nested-def states.
        var graph = try CaptureDependencyGraph.init(self.allocator, self.output, self);
        defer graph.deinit();
        try graph.buildPreLift(self.fn_bodies.items);
        try graph.solve();

        const count = self.output.fnCount();
        self.fn_captures = try allocateCaptureTable(self.allocator, count);
        for (0..count) |index| {
            try self.fn_captures[index].appendSlice(self.allocator, graph.states[index].captures.items);
        }
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
        explicit: anytype,
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
        for (0..slots.len) |index| {
            const slot = GuardedList.at(slots, index);
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

/// Find the existing operand value that supplies capture `id`.
///
/// An operand carries two identities: the value's own CaptureId (when its value
/// is a plain local that carries one) and the operand's declared `id` (set when
/// the operand was built to fill a specific target slot). They agree for a
/// pass-through capture, but diverge in two ways this join must both handle:
///
///   - A value-local whose CaptureId equals `id` is the exact supply for the
///     slot and wins outright. This also overrides a stale declared id: when
///     spec_constr substitutes an operand's value with a local of a different
///     capture but leaves the declared id unchanged, the value's current
///     identity is authoritative.
///
///   - Otherwise the operand's declared id names the slot it fills, even when
///     its value-local carries a different CaptureId. spec_constr routes a
///     value-local into a slot its own binding does not name — e.g. a
///     destructured successor `rest` passed as the next iterator's inner-state
///     capture — and only the declared id records which slot that is. A
///     value-local with no CaptureId, and a genuinely explicit (non-local)
///     value, are likewise keyed solely by the declared id.
///
/// An exact value-CaptureId match always takes precedence over a declared-id
/// match; a declared-id match is used only when no exact match exists.
fn operandValueForSlotId(program: *const Ast.Program, existing: anytype, id: checked.CaptureId) ?Ast.ExprId {
    var fallback_by_id: ?Ast.ExprId = null;
    for (0..existing.len) |index| {
        const operand = GuardedList.at(existing, index);
        switch (program.getExpr(operand.value).data) {
            .local => |local| {
                if (program.getLocal(local).capture_id) |value_id| {
                    if (value_id == id) return operand.value;
                    if (operand.id == id and fallback_by_id == null) fallback_by_id = operand.value;
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
    slots: anytype,
    call_expr: Ast.ExprId,
    bound: *const BoundSet,
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
    for (0..slots.len) |index| {
        const slot = GuardedList.at(slots, index);
        const id = slotCaptureId(program, slot);
        const existing_value = operandValueForSlotId(program, existing, id);
        const value = if (existing_value) |candidate| blk: {
            const candidate_local = switch (program.getExpr(candidate).data) {
                .local => |local| local,
                else => break :blk candidate,
            };
            if (program.getLocal(candidate_local).capture_id != id) break :blk candidate;
            const active = bound.bindingFor(program, candidate_local) orelse break :blk candidate;
            if (active == candidate_local) break :blk candidate;
            break :blk try program.addExpr(.{ .ty = slot.ty, .data = .{ .local = active } });
        } else blk: {
            const active = bound.bindingFor(program, slot.local) orelse slot.local;
            break :blk try program.addExpr(.{ .ty = slot.ty, .data = .{ .local = active } });
        };
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
    var walker = CaptureSet.initForProgram(program.allocator, program, fn_captures);
    defer walker.deinit();
    walker.finalize_operands = true;
    var bound = BoundSet.init(program.allocator);
    defer bound.deinit();

    for (0..program.fnCount()) |raw| {
        walker.clear();
        bound.clear();
        const fn_: Ast.FnId = @enumFromInt(@as(u32, @intCast(raw)));
        const body = program.getFn(fn_);
        try bindTypedLocals(program, &bound, program.typedLocalSpan(body.args));
        switch (body.body) {
            .roc => |expr| try walker.collectExpr(expr, &bound),
            .hosted => {},
        }
    }
}

/// The CaptureId of a capture slot. Every capture slot's
/// local carries a CaptureId (assigned at creation for binder-backed and
/// compile-time locals, in `addIfFree` for lift-synthesized locals).
fn slotCaptureId(program: *const Ast.Program, slot: Ast.TypedLocal) checked.CaptureId {
    return program.getLocal(slot.local).capture_id orelse
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
fn explicitCaptureValueForId(program: *const Ast.Program, explicit: anytype, id: checked.CaptureId) ?Ast.ExprId {
    for (0..explicit.len) |index| {
        const capture = GuardedList.at(explicit, index);
        const capture_id = program.getLocal(capture.local).capture_id orelse
            Common.invariant("pre-lift capture operand local had no CaptureId");
        if (capture_id == id) return capture.value;
    }
    return null;
}

/// Whether an explicit pre-lift capture operand supplies the given CaptureId.
fn explicitProvidesCaptureId(program: *const Ast.Program, explicit: anytype, id: checked.CaptureId) bool {
    return explicitCaptureValueForId(program, explicit, id) != null;
}

const BoundBinder = struct {
    local: Mono.LocalId,
    previous: ?u32,
};

const BoundSet = struct {
    locals: std.AutoHashMap(Mono.LocalId, void),
    binder_heads: std.AutoHashMap(checked.PatternBinderId, u32),
    binder_entries: std.ArrayList(BoundBinder),

    fn init(allocator: Allocator) BoundSet {
        return .{
            .locals = std.AutoHashMap(Mono.LocalId, void).init(allocator),
            .binder_heads = std.AutoHashMap(checked.PatternBinderId, u32).init(allocator),
            .binder_entries = .empty,
        };
    }

    fn deinit(self: *BoundSet) void {
        self.binder_entries.deinit(self.locals.allocator);
        self.binder_heads.deinit();
        self.locals.deinit();
    }

    fn contains(self: *const BoundSet, input: *const Ast.Program, local: Mono.LocalId) bool {
        return self.bindingFor(input, local) != null;
    }

    fn binderIdentity(input: *const Ast.Program, local: Mono.LocalId) ?checked.PatternBinderId {
        return input.getLocal(local).binder;
    }

    fn bindingFor(self: *const BoundSet, input: *const Ast.Program, local: Mono.LocalId) ?Mono.LocalId {
        if (self.locals.contains(local)) return local;
        const identity = binderIdentity(input, local) orelse return null;
        const entry = self.binder_heads.get(identity) orelse return null;
        return self.binder_entries.items[entry].local;
    }

    fn put(self: *BoundSet, input: *const Ast.Program, local: Mono.LocalId) Allocator.Error!void {
        try self.locals.put(local, {});
        if (binderIdentity(input, local)) |identity| {
            const entry: u32 = @intCast(self.binder_entries.items.len);
            try self.binder_entries.append(self.locals.allocator, .{
                .local = local,
                .previous = self.binder_heads.get(identity),
            });
            try self.binder_heads.put(identity, entry);
        }
    }

    fn remove(self: *BoundSet, input: *const Ast.Program, local: Mono.LocalId) void {
        _ = self.locals.remove(local);
        if (binderIdentity(input, local)) |identity| {
            const entry_index = self.binder_heads.get(identity) orelse
                Common.invariant("capture collection removed an unbound checked binder");
            const entry = self.binder_entries.items[entry_index];
            if (entry.local != local) {
                Common.invariant("capture collection removed a shadowed checked binder out of order");
            }
            if (entry.previous) |previous| {
                self.binder_heads.put(identity, previous) catch |err| switch (err) {
                    error.OutOfMemory => Common.invariant("restoring a bound checked binder cannot allocate"),
                };
            } else {
                _ = self.binder_heads.remove(identity);
            }
        }
    }

    /// Reset for reuse across fixpoint solves without freeing capacity.
    fn clear(self: *BoundSet) void {
        self.locals.clearRetainingCapacity();
        self.binder_heads.clearRetainingCapacity();
        self.binder_entries.clearRetainingCapacity();
    }
};

const CaptureSet = struct {
    allocator: Allocator,
    lifter: ?*Lifter,
    program: *Ast.Program,
    fn_captures: []std.ArrayList(Ast.TypedLocal),
    items: std.ArrayList(Ast.TypedLocal),
    seen: std.AutoHashMap(Mono.LocalId, void),
    finalize_operands: bool,

    fn init(lifter: *Lifter) CaptureSet {
        return .{
            .allocator = lifter.allocator,
            .lifter = lifter,
            .program = lifter.output,
            .fn_captures = lifter.fn_captures,
            .items = .empty,
            .seen = std.AutoHashMap(Mono.LocalId, void).init(lifter.allocator),
            .finalize_operands = false,
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
            .finalize_operands = false,
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
        _ = self.program.ensureLiftCaptureId(local);
        const local_data = self.program.getLocal(local);
        try self.items.append(self.allocator, .{
            .local = local,
            .ty = local_data.ty,
        });
    }

    fn collectExpr(self: *CaptureSet, expr_id: Mono.ExprId, bound: *BoundSet) Allocator.Error!void {
        const input = self.program;
        const expr = input.getExpr(expr_id);
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
            .fn_ref => |fn_ref| {
                const operands = try GuardedList.dupe(self.allocator, Ast.CaptureOperand, input.captureOperandSpan(fn_ref.captures));
                defer self.allocator.free(operands);
                for (operands) |operand| {
                    try self.collectExpr(operand.value, bound);
                }
                if (self.finalize_operands) {
                    const fn_index = @intFromEnum(fn_ref.fn_id);
                    // Inline lambdas are reserved while lifting expressions,
                    // after the initial def/nested-def fixed-point table was
                    // sized. Their function records already contain the exact
                    // capture span computed by `liftLambda`; later recompute
                    // passes size the table to include every function.
                    const captures = if (fn_index < self.fn_captures.len)
                        try rebuildCaptureOperandSpan(
                            self.program,
                            fn_ref.captures,
                            self.fn_captures[fn_index].items,
                            expr_id,
                            bound,
                        )
                    else
                        try rebuildCaptureOperandSpan(
                            self.program,
                            fn_ref.captures,
                            self.program.typedLocalSpan(self.program.getFn(fn_ref.fn_id).captures),
                            expr_id,
                            bound,
                        );
                    self.program.setExprData(expr_id, .{ .fn_ref = .{
                        .fn_id = fn_ref.fn_id,
                        .captures = captures,
                    } });
                }
            },
            .fn_def => |fn_def| {
                const lifter = self.lifter orelse Common.invariant("post-lift capture recomputation saw a pre-lift function definition");
                const explicit = input.fnDefCaptureSpan(fn_def.captures);
                if (explicit.len == 0) {
                    try self.collectFnCaptures(lifter.liftedFn(fn_def.fn_id), bound);
                } else {
                    try self.collectFnCapturesExceptExplicit(lifter.liftedFn(fn_def.fn_id), explicit, bound);
                    for (0..explicit.len) |capture_index| {
                        const capture = GuardedList.at(explicit, capture_index);
                        try self.collectExpr(capture.value, bound);
                    }
                }
            },
            .list,
            .tuple,
            => |items| {
                const children = input.exprSpan(items);
                for (0..children.len) |child_index| try self.collectExpr(GuardedList.at(children, child_index), bound);
            },
            .record => |fields| {
                const field_exprs = input.fieldExprSpan(fields);
                for (0..field_exprs.len) |field_index| try self.collectExpr(GuardedList.at(field_exprs, field_index).value, bound);
            },
            .tag => |tag| {
                const payloads = input.exprSpan(tag.payloads);
                for (0..payloads.len) |payload_index| try self.collectExpr(GuardedList.at(payloads, payload_index), bound);
            },
            .static_data_candidate => |candidate| try self.collectExpr(candidate.runtime_expr, bound),
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
                const args = input.exprSpan(call.args);
                for (0..args.len) |arg_index| try self.collectExpr(GuardedList.at(args, arg_index), bound);
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
                const args = input.exprSpan(call.args);
                for (0..args.len) |arg_index| try self.collectExpr(GuardedList.at(args, arg_index), bound);
                const captures = try GuardedList.dupe(self.allocator, Ast.CaptureOperand, input.captureOperandSpan(call.captures));
                defer self.allocator.free(captures);
                for (captures) |capture| try self.collectExpr(capture.value, bound);
                if (self.finalize_operands) {
                    const fn_id = switch (call.callee) {
                        .lifted => |fn_id| fn_id,
                        // Imported direct calls retain their imported function
                        // slot through lifting and have no local capture set.
                        .func => return,
                    };
                    const fn_index = @intFromEnum(fn_id);
                    if (fn_index >= self.fn_captures.len) Common.invariant("direct call target missing recomputed captures");
                    const finalized = try rebuildCaptureOperandSpan(
                        self.program,
                        call.captures,
                        self.fn_captures[fn_index].items,
                        expr_id,
                        bound,
                    );
                    self.program.setExprData(expr_id, .{ .call_proc = .{
                        .callee = call.callee,
                        .args = call.args,
                        .captures = finalized,
                        .is_cold = call.is_cold,
                    } });
                }
            },
            .low_level => |call| {
                const args = input.exprSpan(call.args);
                for (0..args.len) |arg_index| try self.collectExpr(GuardedList.at(args, arg_index), bound);
            },
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
                const branches = input.branchSpan(match.branches);
                for (0..branches.len) |branch_index| {
                    const branch = GuardedList.at(branches, branch_index);
                    var added = std.ArrayList(Mono.LocalId).empty;
                    defer added.deinit(self.allocator);
                    try bindPat(self.allocator, input, branch.pat, bound, &added);
                    if (branch.guard) |guard| try self.collectExpr(guard, bound);
                    try self.collectExpr(branch.body, bound);
                    removeBound(input, bound, added.items);
                }
            },
            .if_ => |if_| {
                const branches = input.ifBranchSpan(if_.branches);
                for (0..branches.len) |branch_index| {
                    const branch = GuardedList.at(branches, branch_index);
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
                const statements = input.stmtSpan(block.statements);
                for (0..statements.len) |stmt_index| try self.collectStmt(input, GuardedList.at(statements, stmt_index), bound, &added);
                try self.collectExpr(block.final_expr, bound);
                removeBound(input, bound, added.items);
            },
            .loop_ => |loop| {
                const initial_values = input.exprSpan(loop.initial_values);
                for (0..initial_values.len) |initial_index| try self.collectExpr(GuardedList.at(initial_values, initial_index), bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(loop.params), &added);
                try self.collectExpr(loop.body, bound);
                removeBound(input, bound, added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectExpr(value, bound),
            .continue_ => |continue_| {
                const values = input.exprSpan(continue_.values);
                for (0..values.len) |value_index| try self.collectExpr(GuardedList.at(values, value_index), bound);
            },
            .join_point => |join_point| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(join_point.params), &added);
                try self.collectExpr(join_point.body, bound);
                removeBound(input, bound, added.items);
                try self.collectExpr(join_point.remainder, bound);
            },
            .jump => |jump| {
                const args = input.exprSpan(jump.args);
                for (0..args.len) |arg_index| try self.collectExpr(GuardedList.at(args, arg_index), bound);
            },
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
        explicit: anytype,
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
        switch (input.getStmt(stmt_id)) {
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

fn bindTypedLocals(input: *const Ast.Program, bound: *BoundSet, locals: anytype) Allocator.Error!void {
    for (0..locals.len) |index| {
        const local = GuardedList.at(locals, index);
        try bound.put(input, local.local);
    }
}

fn bindTypedLocalsTracked(allocator: Allocator, input: *const Ast.Program, bound: *BoundSet, locals: anytype, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    for (0..locals.len) |index| {
        const local = GuardedList.at(locals, index);
        try bound.put(input, local.local);
        try added.append(allocator, local.local);
    }
}

fn bindPat(allocator: Allocator, input: *const Ast.Program, pat_id: Mono.PatId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    switch (input.getPat(pat_id).data) {
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
            const steps = input.strPatternStepSpan(str.steps);
            for (0..steps.len) |step_index| {
                const step = GuardedList.at(steps, step_index);
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
        .record => |fields| {
            const destructs = input.recordDestructSpan(fields);
            for (0..destructs.len) |field_index| try bindPat(allocator, input, GuardedList.at(destructs, field_index).pattern, bound, added);
        },
        .tuple => |items| {
            const children = input.patSpan(items);
            for (0..children.len) |child_index| try bindPat(allocator, input, GuardedList.at(children, child_index), bound, added);
        },
        .list => |list| {
            const children = input.patSpan(list.patterns);
            for (0..children.len) |child_index| try bindPat(allocator, input, GuardedList.at(children, child_index), bound, added);
            if (list.rest) |rest| if (rest.pattern) |rest_pattern| try bindPat(allocator, input, rest_pattern, bound, added);
        },
        .tag => |tag| {
            const payloads = input.patSpan(tag.payloads);
            for (0..payloads.len) |payload_index| try bindPat(allocator, input, GuardedList.at(payloads, payload_index), bound, added);
        },
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

const CaptureNodeId = enum(u32) { _ };
const CaptureEdgeId = enum(u32) { _ };
const CaptureScopeId = enum(u32) { _ };

const CaptureGraphNode = struct {
    owner: Ast.FnId,
    direct: std.ArrayList(Ast.TypedLocal) = .empty,
    edges: std.ArrayList(CaptureEdgeId) = .empty,
    active: bool = false,
};

const CaptureSupply = struct {
    id: checked.CaptureId,
    value: Ast.ExprId,
    node: CaptureNodeId,
};

const CaptureEdgeSite = union(enum) {
    pre_lift,
    fn_ref: Ast.ExprId,
    call_proc: Ast.ExprId,
};

const CaptureGraphEdge = struct {
    owner: Ast.FnId,
    target: Ast.FnId,
    scope: ?CaptureScopeId,
    site: CaptureEdgeSite,
    exact_supplies: std.ArrayList(CaptureSupply) = .empty,
    declared_supplies: std.ArrayList(CaptureSupply) = .empty,
    active: bool = false,
};

const CaptureScopeEntry = struct {
    parent: ?CaptureScopeId,
    local: Ast.LocalId,
};

const CaptureFnState = struct {
    captures: std.ArrayList(Ast.TypedLocal) = .empty,
    by_id: std.AutoHashMap(checked.CaptureId, Ast.TypedLocal),
    reverse_edges: std.ArrayList(CaptureEdgeId) = .empty,

    fn init(allocator: Allocator) CaptureFnState {
        return .{ .by_id = std.AutoHashMap(checked.CaptureId, Ast.TypedLocal).init(allocator) };
    }
};

const CaptureUpdate = struct {
    function: Ast.FnId,
    capture: Ast.TypedLocal,
};

/// Exact free-variable dataflow for lifted functions.
///
/// A root node represents the unconditionally evaluated part of one function
/// body. Each explicit capture operand is represented by a dormant child node:
/// it becomes active only if the target function actually has the CaptureId
/// that operand supplies. Active call/reference edges subscribe to their
/// target's capture set. A newly discovered capture therefore visits only the
/// affected edges, and each operand-expression node is activated at most once.
const CaptureDependencyGraph = struct {
    backing_allocator: Allocator,
    arena: *std.heap.ArenaAllocator,
    allocator: Allocator,
    program: *Ast.Program,
    lifter: ?*Lifter,
    states: []CaptureFnState,
    nodes: std.ArrayList(CaptureGraphNode),
    edges: std.ArrayList(CaptureGraphEdge),
    scopes: std.ArrayList(CaptureScopeEntry),
    roots: std.ArrayList(CaptureNodeId),
    pending_nodes: std.ArrayList(CaptureNodeId),
    updates: std.ArrayList(CaptureUpdate),
    next_node: usize,
    next_update: usize,

    fn init(allocator: Allocator, program: *Ast.Program, lifter: ?*Lifter) Allocator.Error!CaptureDependencyGraph {
        const arena = try allocator.create(std.heap.ArenaAllocator);
        errdefer allocator.destroy(arena);
        arena.* = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();
        const scratch = arena.allocator();
        const states = try scratch.alloc(CaptureFnState, program.fnCount());
        for (states) |*state| state.* = CaptureFnState.init(scratch);
        return .{
            .backing_allocator = allocator,
            .arena = arena,
            .allocator = scratch,
            .program = program,
            .lifter = lifter,
            .states = states,
            .nodes = .empty,
            .edges = .empty,
            .scopes = .empty,
            .roots = .empty,
            .pending_nodes = .empty,
            .updates = .empty,
            .next_node = 0,
            .next_update = 0,
        };
    }

    fn deinit(self: *CaptureDependencyGraph) void {
        self.arena.deinit();
        self.backing_allocator.destroy(self.arena);
    }

    fn addNode(self: *CaptureDependencyGraph, owner: Ast.FnId) Allocator.Error!CaptureNodeId {
        const id: CaptureNodeId = @enumFromInt(@as(u32, @intCast(self.nodes.items.len)));
        try self.nodes.append(self.allocator, .{ .owner = owner });
        return id;
    }

    fn addScopeEntry(self: *CaptureDependencyGraph, parent: ?CaptureScopeId, local: Ast.LocalId) Allocator.Error!CaptureScopeId {
        const id: CaptureScopeId = @enumFromInt(@as(u32, @intCast(self.scopes.items.len)));
        try self.scopes.append(self.allocator, .{ .parent = parent, .local = local });
        return id;
    }

    fn scopeBindingFor(self: *const CaptureDependencyGraph, scope: ?CaptureScopeId, local: Ast.LocalId) ?Ast.LocalId {
        const binder = self.program.getLocal(local).binder;
        var current = scope;
        while (current) |id| {
            const entry = self.scopes.items[@intFromEnum(id)];
            if (entry.local == local) return entry.local;
            if (binder) |identity| {
                if (self.program.getLocal(entry.local).binder == identity) return entry.local;
            }
            current = entry.parent;
        }
        return null;
    }

    fn buildPreLift(self: *CaptureDependencyGraph, bodies: []const ?MonoFnBody) Allocator.Error!void {
        if (self.lifter == null) Common.invariant("pre-lift capture graph had no lifter");
        if (bodies.len != self.states.len) Common.invariant("pre-lift capture body count differed from reserved function count");
        var builder = CaptureGraphBuilder.init(self);
        defer builder.deinit();
        for (bodies, 0..) |maybe_body, raw| {
            const body = maybe_body orelse continue;
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(raw)));
            const root = try self.addNode(fn_id);
            try self.roots.append(self.allocator, root);
            builder.reset();
            try builder.bindTypedLocals(self.program.typedLocalSpan(body.args), null);
            switch (body.body) {
                .roc => |expr| try builder.collectExpr(expr, root),
                .hosted => {},
            }
        }
    }

    fn buildPostLift(self: *CaptureDependencyGraph) Allocator.Error!void {
        if (self.lifter != null) Common.invariant("post-lift capture graph unexpectedly had a lifter");
        var builder = CaptureGraphBuilder.init(self);
        defer builder.deinit();
        for (0..self.program.fnCount()) |raw| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(raw)));
            const fn_ = self.program.getFn(fn_id);
            const root = try self.addNode(fn_id);
            try self.roots.append(self.allocator, root);
            builder.reset();
            try builder.bindTypedLocals(self.program.typedLocalSpan(fn_.args), null);
            switch (fn_.body) {
                .roc => |expr| try builder.collectExpr(expr, root),
                .hosted => {},
            }
        }
    }

    fn queueNode(self: *CaptureDependencyGraph, node_id: CaptureNodeId) Allocator.Error!void {
        const node = &self.nodes.items[@intFromEnum(node_id)];
        if (node.active) return;
        node.active = true;
        try self.pending_nodes.append(self.allocator, node_id);
    }

    fn addCaptureUpdate(self: *CaptureDependencyGraph, fn_id: Ast.FnId, capture: Ast.TypedLocal) Allocator.Error!void {
        const local_data = self.program.getLocal(capture.local);
        if (capture.ty != local_data.ty) Common.invariant("capture graph entry type differed from its local type");
        const id = self.program.ensureLiftCaptureId(capture.local);
        const state = &self.states[@intFromEnum(fn_id)];
        const result = try state.by_id.getOrPut(id);
        if (result.found_existing) {
            if (result.value_ptr.local != capture.local or result.value_ptr.ty != capture.ty) {
                Common.invariant("capture graph found two locals for one CaptureId in a function");
            }
            return;
        }
        result.value_ptr.* = capture;
        try state.captures.append(self.allocator, capture);
        try self.updates.append(self.allocator, .{ .function = fn_id, .capture = capture });
    }

    fn processNode(self: *CaptureDependencyGraph, node_id: CaptureNodeId) Allocator.Error!void {
        const node_index = @intFromEnum(node_id);
        const owner = self.nodes.items[node_index].owner;
        var direct_index: usize = 0;
        while (direct_index < self.nodes.items[node_index].direct.items.len) : (direct_index += 1) {
            try self.addCaptureUpdate(owner, self.nodes.items[node_index].direct.items[direct_index]);
        }
        var edge_index: usize = 0;
        while (edge_index < self.nodes.items[node_index].edges.items.len) : (edge_index += 1) {
            try self.activateEdge(self.nodes.items[node_index].edges.items[edge_index]);
        }
    }

    fn activateEdge(self: *CaptureDependencyGraph, edge_id: CaptureEdgeId) Allocator.Error!void {
        const edge_index = @intFromEnum(edge_id);
        if (self.edges.items[edge_index].active) return;
        self.edges.items[edge_index].active = true;
        const target = self.edges.items[edge_index].target;
        const state = &self.states[@intFromEnum(target)];
        try state.reverse_edges.append(self.allocator, edge_id);
        var capture_index: usize = 0;
        while (capture_index < state.captures.items.len) : (capture_index += 1) {
            try self.applyCaptureToEdge(edge_id, state.captures.items[capture_index]);
        }
    }

    fn supplyLessThan(_: void, lhs: CaptureSupply, rhs: CaptureSupply) bool {
        return @intFromEnum(lhs.id) < @intFromEnum(rhs.id);
    }

    fn findSupply(supplies: []const CaptureSupply, id: checked.CaptureId) ?CaptureSupply {
        var low: usize = 0;
        var high: usize = supplies.len;
        const wanted = @intFromEnum(id);
        while (low < high) {
            const mid = low + (high - low) / 2;
            const found = @intFromEnum(supplies[mid].id);
            if (found < wanted) {
                low = mid + 1;
            } else if (found > wanted) {
                high = mid;
            } else {
                return supplies[mid];
            }
        }
        return null;
    }

    fn edgeSupply(self: *const CaptureDependencyGraph, edge_id: CaptureEdgeId, id: checked.CaptureId) ?CaptureSupply {
        const edge = self.edges.items[@intFromEnum(edge_id)];
        return findSupply(edge.exact_supplies.items, id) orelse findSupply(edge.declared_supplies.items, id);
    }

    fn applyCaptureToEdge(self: *CaptureDependencyGraph, edge_id: CaptureEdgeId, capture: Ast.TypedLocal) Allocator.Error!void {
        const id = slotCaptureId(self.program, capture);
        if (self.edgeSupply(edge_id, id)) |supply| {
            try self.queueNode(supply.node);
            return;
        }
        const edge = self.edges.items[@intFromEnum(edge_id)];
        if (self.scopeBindingFor(edge.scope, capture.local) == null) {
            try self.addCaptureUpdate(edge.owner, capture);
        }
    }

    fn processUpdate(self: *CaptureDependencyGraph, update: CaptureUpdate) Allocator.Error!void {
        const state = &self.states[@intFromEnum(update.function)];
        var index: usize = 0;
        while (index < state.reverse_edges.items.len) : (index += 1) {
            try self.applyCaptureToEdge(state.reverse_edges.items[index], update.capture);
        }
    }

    fn solve(self: *CaptureDependencyGraph) Allocator.Error!void {
        for (self.roots.items) |root| try self.queueNode(root);
        while (self.next_node < self.pending_nodes.items.len or self.next_update < self.updates.items.len) {
            if (self.next_node < self.pending_nodes.items.len) {
                const node = self.pending_nodes.items[self.next_node];
                self.next_node += 1;
                try self.processNode(node);
            } else {
                const update = self.updates.items[self.next_update];
                self.next_update += 1;
                try self.processUpdate(update);
            }
        }
        for (self.states) |*state| sortCaptureSlots(self.program, state.captures.items);
    }

    fn resolvedOperandValue(self: *CaptureDependencyGraph, edge_id: CaptureEdgeId, slot: Ast.TypedLocal) Allocator.Error!Ast.ExprId {
        const edge = self.edges.items[@intFromEnum(edge_id)];
        const id = slotCaptureId(self.program, slot);
        if (self.edgeSupply(edge_id, id)) |supply| {
            const candidate_local = switch (self.program.getExpr(supply.value).data) {
                .local => |local| local,
                else => return supply.value,
            };
            if (self.program.getLocal(candidate_local).capture_id != id) return supply.value;
            const active = self.scopeBindingFor(edge.scope, candidate_local) orelse return supply.value;
            if (active == candidate_local) return supply.value;
            return try self.program.addExpr(.{ .ty = slot.ty, .data = .{ .local = active } });
        }
        const active = self.scopeBindingFor(edge.scope, slot.local) orelse slot.local;
        return try self.program.addExpr(.{ .ty = slot.ty, .data = .{ .local = active } });
    }

    fn finalizedSpan(self: *CaptureDependencyGraph, edge_id: CaptureEdgeId, call_expr: Ast.ExprId) Allocator.Error!Ast.Span(Ast.CaptureOperand) {
        const edge = self.edges.items[@intFromEnum(edge_id)];
        const slots = self.states[@intFromEnum(edge.target)].captures.items;
        if (slots.len == 0) return .empty();

        const saved_loc = self.program.current_loc;
        defer self.program.current_loc = saved_loc;
        const saved_region = self.program.current_region;
        defer self.program.current_region = saved_region;
        const call_loc = self.program.exprLoc(call_expr);
        if (call_loc.hasLocation()) self.program.current_loc = call_loc;
        const call_region = self.program.exprRegion(call_expr);
        if (!call_region.isEmpty()) self.program.current_region = call_region;

        const operands = try self.allocator.alloc(Ast.CaptureOperand, slots.len);
        defer self.allocator.free(operands);
        for (slots, 0..) |slot, index| {
            operands[index] = .{
                .id = slotCaptureId(self.program, slot),
                .value = try self.resolvedOperandValue(edge_id, slot),
            };
        }
        return try self.program.addCaptureOperandSpan(operands);
    }

    fn finalizePostLiftOperands(self: *CaptureDependencyGraph) Allocator.Error!void {
        for (self.edges.items, 0..) |edge, raw| {
            const edge_id: CaptureEdgeId = @enumFromInt(@as(u32, @intCast(raw)));
            switch (edge.site) {
                .pre_lift => {},
                .fn_ref => |expr_id| {
                    const fn_ref = switch (self.program.getExpr(expr_id).data) {
                        .fn_ref => |fn_ref| fn_ref,
                        else => Common.invariant("capture graph function-reference site changed expression kind"),
                    };
                    if (fn_ref.fn_id != edge.target) Common.invariant("capture graph function-reference target changed");
                    self.program.setExprData(expr_id, .{ .fn_ref = .{
                        .fn_id = fn_ref.fn_id,
                        .captures = try self.finalizedSpan(edge_id, expr_id),
                    } });
                },
                .call_proc => |expr_id| {
                    const call = switch (self.program.getExpr(expr_id).data) {
                        .call_proc => |call| call,
                        else => Common.invariant("capture graph direct-call site changed expression kind"),
                    };
                    const target = switch (call.callee) {
                        .lifted => |fn_id| fn_id,
                        .func => Common.invariant("capture graph direct-call site changed target kind"),
                    };
                    if (target != edge.target) Common.invariant("capture graph direct-call target changed");
                    self.program.setExprData(expr_id, .{ .call_proc = .{
                        .callee = call.callee,
                        .args = call.args,
                        .captures = try self.finalizedSpan(edge_id, expr_id),
                        .is_cold = call.is_cold,
                    } });
                },
            }
        }
    }
};

const CaptureGraphBuilder = struct {
    graph: *CaptureDependencyGraph,
    bound: BoundSet,
    current_scope: ?CaptureScopeId,

    fn init(graph: *CaptureDependencyGraph) CaptureGraphBuilder {
        return .{
            .graph = graph,
            .bound = BoundSet.init(graph.allocator),
            .current_scope = null,
        };
    }

    fn deinit(self: *CaptureGraphBuilder) void {
        self.bound.deinit();
    }

    fn reset(self: *CaptureGraphBuilder) void {
        self.bound.clear();
        self.current_scope = null;
    }

    fn bindLocal(self: *CaptureGraphBuilder, local: Ast.LocalId, added: ?*std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        try self.bound.put(self.graph.program, local);
        self.current_scope = try self.graph.addScopeEntry(self.current_scope, local);
        if (added) |list| try list.append(self.graph.allocator, local);
    }

    fn removeLocal(self: *CaptureGraphBuilder, local: Ast.LocalId) void {
        const scope_id = self.current_scope orelse Common.invariant("capture graph scope stack underflow");
        const entry = self.graph.scopes.items[@intFromEnum(scope_id)];
        if (entry.local != local) Common.invariant("capture graph removed a lexical binding out of order");
        self.current_scope = entry.parent;
        self.bound.remove(self.graph.program, local);
    }

    fn removeLocals(self: *CaptureGraphBuilder, locals: []const Ast.LocalId) void {
        var index = locals.len;
        while (index > 0) {
            index -= 1;
            self.removeLocal(locals[index]);
        }
    }

    fn bindTypedLocals(self: *CaptureGraphBuilder, locals: anytype, added: ?*std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        for (0..locals.len) |index| try self.bindLocal(GuardedList.at(locals, index).local, added);
    }

    fn bindPat(self: *CaptureGraphBuilder, pat_id: Ast.PatId, added: *std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        const input = self.graph.program;
        switch (input.getPat(pat_id).data) {
            .bind => |local| try self.bindLocal(local, added),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => {},
            .str_pattern => |str| {
                const steps = input.strPatternStepSpan(str.steps);
                for (0..steps.len) |index| {
                    if (GuardedList.at(steps, index).capture) |capture| try self.bindPat(capture, added);
                }
            },
            .as => |as| {
                try self.bindPat(as.pattern, added);
                try self.bindLocal(as.local, added);
            },
            .record => |fields| {
                const destructs = input.recordDestructSpan(fields);
                for (0..destructs.len) |index| try self.bindPat(GuardedList.at(destructs, index).pattern, added);
            },
            .tuple => |items| {
                const children = input.patSpan(items);
                for (0..children.len) |index| try self.bindPat(GuardedList.at(children, index), added);
            },
            .list => |list| {
                const children = input.patSpan(list.patterns);
                for (0..children.len) |index| try self.bindPat(GuardedList.at(children, index), added);
                if (list.rest) |rest| if (rest.pattern) |rest_pattern| try self.bindPat(rest_pattern, added);
            },
            .tag => |tag| {
                const payloads = input.patSpan(tag.payloads);
                for (0..payloads.len) |index| try self.bindPat(GuardedList.at(payloads, index), added);
            },
            .nominal => |backing| try self.bindPat(backing, added),
        }
    }

    fn addDirect(self: *CaptureGraphBuilder, node_id: CaptureNodeId, local: Ast.LocalId) Allocator.Error!void {
        if (self.bound.contains(self.graph.program, local)) return;
        _ = self.graph.program.ensureLiftCaptureId(local);
        const local_data = self.graph.program.getLocal(local);
        try self.graph.nodes.items[@intFromEnum(node_id)].direct.append(self.graph.allocator, .{
            .local = local,
            .ty = local_data.ty,
        });
    }

    fn hasSupplyId(supplies: []const CaptureSupply, id: checked.CaptureId) bool {
        for (supplies) |supply| if (supply.id == id) return true;
        return false;
    }

    fn finishEdge(
        self: *CaptureGraphBuilder,
        parent: CaptureNodeId,
        target: Ast.FnId,
        site: CaptureEdgeSite,
        exact_supplies: *std.ArrayList(CaptureSupply),
        declared_supplies: *std.ArrayList(CaptureSupply),
    ) Allocator.Error!void {
        std.sort.pdq(CaptureSupply, exact_supplies.items, {}, CaptureDependencyGraph.supplyLessThan);
        std.sort.pdq(CaptureSupply, declared_supplies.items, {}, CaptureDependencyGraph.supplyLessThan);
        if (declared_supplies.items.len > 1) {
            for (declared_supplies.items[1..], declared_supplies.items[0 .. declared_supplies.items.len - 1]) |current, previous| {
                if (current.id == previous.id) Common.invariant("capture edge declared one CaptureId more than once");
            }
        }
        const edge_id: CaptureEdgeId = @enumFromInt(@as(u32, @intCast(self.graph.edges.items.len)));
        try self.graph.edges.append(self.graph.allocator, .{
            .owner = self.graph.nodes.items[@intFromEnum(parent)].owner,
            .target = target,
            .scope = self.current_scope,
            .site = site,
            .exact_supplies = exact_supplies.*,
            .declared_supplies = declared_supplies.*,
        });
        exact_supplies.* = .empty;
        declared_supplies.* = .empty;
        try self.graph.nodes.items[@intFromEnum(parent)].edges.append(self.graph.allocator, edge_id);
    }

    fn addCaptureOperandEdge(
        self: *CaptureGraphBuilder,
        parent: CaptureNodeId,
        target: Ast.FnId,
        site: CaptureEdgeSite,
        span: Ast.Span(Ast.CaptureOperand),
    ) Allocator.Error!void {
        var exact: std.ArrayList(CaptureSupply) = .empty;
        errdefer exact.deinit(self.graph.allocator);
        var declared: std.ArrayList(CaptureSupply) = .empty;
        errdefer declared.deinit(self.graph.allocator);
        const operands = self.graph.program.captureOperandSpan(span);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            const child = try self.graph.addNode(self.graph.nodes.items[@intFromEnum(parent)].owner);
            try self.collectExpr(operand.value, child);
            const supply = CaptureSupply{ .id = operand.id, .value = operand.value, .node = child };
            try declared.append(self.graph.allocator, supply);
            switch (self.graph.program.getExpr(operand.value).data) {
                .local => |local| if (self.graph.program.getLocal(local).capture_id) |id| {
                    if (!hasSupplyId(exact.items, id)) {
                        try exact.append(self.graph.allocator, .{ .id = id, .value = operand.value, .node = child });
                    }
                },
                else => {},
            }
        }
        try self.finishEdge(parent, target, site, &exact, &declared);
    }

    fn addFnDefEdge(self: *CaptureGraphBuilder, parent: CaptureNodeId, target: Ast.FnId, span: Ast.Span(Ast.FnDefCapture)) Allocator.Error!void {
        var exact: std.ArrayList(CaptureSupply) = .empty;
        errdefer exact.deinit(self.graph.allocator);
        var declared: std.ArrayList(CaptureSupply) = .empty;
        errdefer declared.deinit(self.graph.allocator);
        const captures = self.graph.program.fnDefCaptureSpan(span);
        for (0..captures.len) |index| {
            const capture = GuardedList.at(captures, index);
            const id = self.graph.program.getLocal(capture.local).capture_id orelse
                Common.invariant("pre-lift explicit capture local had no CaptureId");
            const child = try self.graph.addNode(self.graph.nodes.items[@intFromEnum(parent)].owner);
            try self.collectExpr(capture.value, child);
            const supply = CaptureSupply{ .id = id, .value = capture.value, .node = child };
            try declared.append(self.graph.allocator, supply);
            if (!hasSupplyId(exact.items, id)) try exact.append(self.graph.allocator, supply);
        }
        try self.finishEdge(parent, target, .pre_lift, &exact, &declared);
    }

    fn collectExprSpan(self: *CaptureGraphBuilder, span: Ast.Span(Ast.ExprId), node: CaptureNodeId) Allocator.Error!void {
        const values = self.graph.program.exprSpan(span);
        for (0..values.len) |index| try self.collectExpr(GuardedList.at(values, index), node);
    }

    fn collectStmt(self: *CaptureGraphBuilder, stmt_id: Ast.StmtId, node: CaptureNodeId, added: *std.ArrayList(Ast.LocalId)) Allocator.Error!void {
        const input = self.graph.program;
        switch (input.getStmt(stmt_id)) {
            .uninitialized => |pat| try self.bindPat(pat, added),
            .let_ => |let_| {
                if (let_.recursive) {
                    try self.bindPat(let_.pat, added);
                    try self.collectExpr(let_.value, node);
                } else {
                    try self.collectExpr(let_.value, node);
                    try self.bindPat(let_.pat, added);
                }
            },
            .expr,
            .expect,
            .dbg,
            => |expr| try self.collectExpr(expr, node),
            .return_ => |ret| try self.collectExpr(ret.value, node),
            .crash => {},
        }
    }

    fn collectExpr(self: *CaptureGraphBuilder, expr_id: Ast.ExprId, node: CaptureNodeId) Allocator.Error!void {
        const input = self.graph.program;
        const expr = input.getExpr(expr_id);
        switch (expr.data) {
            .local => |local| try self.addDirect(node, local),
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
            .def_ref => if (self.graph.lifter == null) Common.invariant("post-lift capture graph saw a definition reference"),
            .fn_ref => |fn_ref| {
                if (self.graph.lifter == null) {
                    try self.addCaptureOperandEdge(node, fn_ref.fn_id, .{ .fn_ref = expr_id }, fn_ref.captures);
                } else {
                    // A pre-existing lifted reference already carries its exact
                    // capture payload. It is not part of the local Monotype
                    // definition graph, so its explicit values contribute
                    // directly and its lifted target is not subscribed here.
                    const operands = input.captureOperandSpan(fn_ref.captures);
                    for (0..operands.len) |index| try self.collectExpr(GuardedList.at(operands, index).value, node);
                }
            },
            .fn_def => |fn_def| {
                const lifter = self.graph.lifter orelse Common.invariant("post-lift capture graph saw a function definition");
                try self.addFnDefEdge(node, lifter.liftedFn(fn_def.fn_id), fn_def.captures);
            },
            .list,
            .tuple,
            => |items| try self.collectExprSpan(items, node),
            .record => |fields| {
                const field_exprs = input.fieldExprSpan(fields);
                for (0..field_exprs.len) |index| try self.collectExpr(GuardedList.at(field_exprs, index).value, node);
            },
            .tag => |tag| try self.collectExprSpan(tag.payloads, node),
            .static_data_candidate => |candidate| try self.collectExpr(candidate.runtime_expr, node),
            .nominal,
            .dbg,
            .expect,
            => |child| try self.collectExpr(child, node),
            .return_ => |ret| try self.collectExpr(ret.value, node),
            .expect_err => |expect_err| try self.collectExpr(expect_err.msg, node),
            .comptime_branch_taken => |taken| try self.collectExpr(taken.body, node),
            .let_ => |let_| {
                try self.collectExpr(let_.value, node);
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.graph.allocator);
                try self.bindPat(let_.bind, &added);
                try self.collectExpr(let_.rest, node);
                self.removeLocals(added.items);
            },
            .lambda => |lambda| {
                if (self.graph.lifter == null) Common.invariant("post-lift capture graph saw an inline lambda");
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.graph.allocator);
                try self.bindTypedLocals(input.typedLocalSpan(lambda.args), &added);
                try self.collectExpr(lambda.body, node);
                self.removeLocals(added.items);
            },
            .call_value => |call| {
                try self.collectExpr(call.callee, node);
                try self.collectExprSpan(call.args, node);
            },
            .call_proc => |call| {
                const maybe_target: ?Ast.FnId = switch (call.callee) {
                    .func => |slot| switch (slot) {
                        .local => |mono_fn_id| blk: {
                            const lifter = self.graph.lifter orelse Common.invariant("post-lift capture graph saw a pre-lift direct call");
                            break :blk lifter.liftedFn(mono_fn_id);
                        },
                        .imported => null,
                    },
                    .lifted => |fn_id| fn_id,
                };
                try self.collectExprSpan(call.args, node);
                if (maybe_target) |target| {
                    try self.addCaptureOperandEdge(
                        node,
                        target,
                        if (self.graph.lifter == null) .{ .call_proc = expr_id } else .pre_lift,
                        call.captures,
                    );
                }
            },
            .low_level => |call| try self.collectExprSpan(call.args, node),
            .field_access => |field| try self.collectExpr(field.receiver, node),
            .tuple_access => |access| try self.collectExpr(access.tuple, node),
            .structural_eq => |eq| {
                try self.collectExpr(eq.lhs, node);
                try self.collectExpr(eq.rhs, node);
            },
            .structural_hash => |hash| {
                try self.collectExpr(hash.value, node);
                try self.collectExpr(hash.hasher, node);
            },
            .match_ => |match| {
                try self.collectExpr(match.scrutinee, node);
                const branches = input.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    var added: std.ArrayList(Ast.LocalId) = .empty;
                    defer added.deinit(self.graph.allocator);
                    try self.bindPat(branch.pat, &added);
                    if (branch.guard) |guard| try self.collectExpr(guard, node);
                    try self.collectExpr(branch.body, node);
                    self.removeLocals(added.items);
                }
            },
            .if_ => |if_| {
                const branches = input.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    try self.collectExpr(branch.cond, node);
                    try self.collectExpr(branch.body, node);
                }
                try self.collectExpr(if_.final_else, node);
            },
            .if_initialized_payload => |payload_switch| {
                try self.collectExpr(payload_switch.cond, node);
                try self.addDirect(node, payload_switch.payload);
                try self.collectExpr(payload_switch.initialized, node);
                try self.collectExpr(payload_switch.uninitialized, node);
            },
            .try_sequence => |sequence| {
                try self.collectExpr(sequence.try_expr, node);
                try self.bindLocal(sequence.ok_local, null);
                try self.collectExpr(sequence.ok_body, node);
                self.removeLocal(sequence.ok_local);
            },
            .try_record_sequence => |sequence| {
                try self.collectExpr(sequence.try_expr, node);
                try self.bindLocal(sequence.value_local, null);
                try self.bindLocal(sequence.rest_local, null);
                try self.collectExpr(sequence.ok_body, node);
                self.removeLocal(sequence.rest_local);
                self.removeLocal(sequence.value_local);
            },
            .block => |block| {
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.graph.allocator);
                const statements = input.stmtSpan(block.statements);
                for (0..statements.len) |index| try self.collectStmt(GuardedList.at(statements, index), node, &added);
                try self.collectExpr(block.final_expr, node);
                self.removeLocals(added.items);
            },
            .loop_ => |loop| {
                try self.collectExprSpan(loop.initial_values, node);
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.graph.allocator);
                try self.bindTypedLocals(input.typedLocalSpan(loop.params), &added);
                try self.collectExpr(loop.body, node);
                self.removeLocals(added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectExpr(value, node),
            .continue_ => |continue_| try self.collectExprSpan(continue_.values, node),
            .join_point => |join_point| {
                var added: std.ArrayList(Ast.LocalId) = .empty;
                defer added.deinit(self.graph.allocator);
                try self.bindTypedLocals(input.typedLocalSpan(join_point.params), &added);
                try self.collectExpr(join_point.body, node);
                self.removeLocals(added.items);
                try self.collectExpr(join_point.remainder, node);
            },
            .jump => |jump| try self.collectExprSpan(jump.args, node),
        }
    }
};

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

fn initCaptureTestProgram(allocator: Allocator) Ast.Program {
    return Ast.Program.init(
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
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
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
    _ = try mono.addDef(.{
        .symbol = @enumFromInt(1),
        .args = Mono.Span(Mono.TypedLocal).empty(),
        .body = .{ .roc = body },
        .ret = unit_ty,
    });

    var lifted = try run(allocator, mono);
    defer lifted.deinit();

    try std.testing.expectEqual(@as(usize, 1), lifted.importedFnCount());
    const call = switch (lifted.getExpr(body).data) {
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
        .empty, // static_data_values
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
    const fn_id = try program.addFn(.{
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
    program.setCaptureOperandInSpan(op_span, 0, .{
        .id = checked.CaptureId.fromBinder(@enumFromInt(2)),
        .value = value,
    });
    try std.testing.expectEqualStrings(
        "operand CaptureId did not match its slot",
        (try checkCaptureInvariants(&program)).?,
    );
}

test "capture finalization supplies the caller's active binder local" {
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
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
    defer program.deinit();

    const ty = try program.types.add(.zst);
    const binder: checked.PatternBinderId = @enumFromInt(1);
    const captured_outer = try program.addLocalWithBinder(@enumFromInt(1), ty, binder);
    const active_arg = try program.addLocalWithBinder(@enumFromInt(2), ty, binder);

    const callee_body = try program.addExpr(.{ .ty = ty, .data = .{ .local = captured_outer } });
    const callee = try program.addFn(.{
        .symbol = @enumFromInt(1),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = callee_body },
        .ret = ty,
    });

    const call = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = callee },
        .args = .empty(),
        .captures = .empty(),
    } } });
    _ = try program.addFn(.{
        .symbol = @enumFromInt(2),
        .args = try program.addTypedLocalSpan(&.{.{ .local = active_arg, .ty = ty }}),
        .captures = .empty(),
        .body = .{ .roc = call },
        .ret = ty,
    });

    try recomputeCaptures(allocator, &program);

    const finalized = switch (program.getExpr(call).data) {
        .call_proc => |value| value,
        else => return error.TestUnexpectedResult,
    };
    const operands = program.captureOperandSpan(finalized.captures);
    try std.testing.expectEqual(@as(usize, 1), operands.len);
    const operand = GuardedList.at(operands, 0);
    try std.testing.expectEqual(checked.CaptureId.fromBinder(binder), operand.id);
    const supplied = switch (program.getExpr(operand.value).data) {
        .local => |local| local,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(active_arg, supplied);
}

test "capture graph does not activate an operand for a removed target slot" {
    const allocator = std.testing.allocator;
    var program = initCaptureTestProgram(allocator);
    defer program.deinit();

    const ty = try program.types.add(.zst);
    const binder: checked.PatternBinderId = @enumFromInt(1);
    const stale_capture = try program.addLocalWithBinder(@enumFromInt(1), ty, binder);
    const stale_slots = try program.addTypedLocalSpan(&.{.{ .local = stale_capture, .ty = ty }});
    const callee_body = try program.addExpr(.{ .ty = ty, .data = .unit });
    const callee = try program.addFn(.{
        .symbol = @enumFromInt(1),
        .args = .empty(),
        .captures = stale_slots,
        .body = .{ .roc = callee_body },
        .ret = ty,
    });

    const supplied_value = try program.addExpr(.{ .ty = ty, .data = .{ .local = stale_capture } });
    const supplied_span = try program.addCaptureOperandSpan(&.{.{
        .id = checked.CaptureId.fromBinder(binder),
        .value = supplied_value,
    }});
    const reference = try program.addExpr(.{ .ty = ty, .data = .{ .fn_ref = .{
        .fn_id = callee,
        .captures = supplied_span,
    } } });
    const caller = try program.addFn(.{
        .symbol = @enumFromInt(2),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = reference },
        .ret = ty,
    });

    try recomputeCaptures(allocator, &program);

    try std.testing.expectEqual(@as(usize, 0), program.typedLocalSpan(program.getFn(callee).captures).len);
    try std.testing.expectEqual(@as(usize, 0), program.typedLocalSpan(program.getFn(caller).captures).len);
    const finalized = switch (program.getExpr(reference).data) {
        .fn_ref => |fn_ref| fn_ref,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(@as(usize, 0), program.captureOperandSpan(finalized.captures).len);
}

test "capture recomputation excludes replaced bodies from the active invariant" {
    const allocator = std.testing.allocator;
    var program = initCaptureTestProgram(allocator);
    defer program.deinit();

    const ty = try program.types.add(.zst);
    const binder: checked.PatternBinderId = @enumFromInt(1);
    const stale_capture = try program.addLocalWithBinder(@enumFromInt(1), ty, binder);
    const stale_slots = try program.addTypedLocalSpan(&.{.{ .local = stale_capture, .ty = ty }});
    const callee_body = try program.addExpr(.{ .ty = ty, .data = .unit });
    const callee = try program.addFn(.{
        .symbol = @enumFromInt(1),
        .args = .empty(),
        .captures = stale_slots,
        .body = .{ .roc = callee_body },
        .ret = ty,
    });

    const supplied_value = try program.addExpr(.{ .ty = ty, .data = .{ .local = stale_capture } });
    const supplied_span = try program.addCaptureOperandSpan(&.{.{
        .id = checked.CaptureId.fromBinder(binder),
        .value = supplied_value,
    }});
    const replaced_reference = try program.addExpr(.{ .ty = ty, .data = .{ .fn_ref = .{
        .fn_id = callee,
        .captures = supplied_span,
    } } });

    try recomputeCaptures(allocator, &program);

    try std.testing.expectEqual(@as(usize, 0), program.typedLocalSpan(program.getFn(callee).captures).len);
    const stale_reference = switch (program.getExpr(replaced_reference).data) {
        .fn_ref => |fn_ref| fn_ref,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(@as(usize, 1), program.captureOperandSpan(stale_reference.captures).len);
    try std.testing.expectEqualStrings(
        "operand count differed from target capture slot count",
        (try checkCaptureInvariants(&program)).?,
    );
}

test "capture graph propagates recursive captures with a worklist" {
    const allocator = std.testing.allocator;
    var program = initCaptureTestProgram(allocator);
    defer program.deinit();

    const ty = try program.types.add(.zst);
    const captured = try program.addLocalWithBinder(@enumFromInt(1), ty, @enumFromInt(1));
    const first = try program.reserveFnSlot();
    const second = try program.reserveFnSlot();

    const captured_value = try program.addExpr(.{ .ty = ty, .data = .{ .local = captured } });
    const call_second = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = second },
        .args = .empty(),
        .captures = .empty(),
    } } });
    const first_body = try program.addExpr(.{ .ty = ty, .data = .{ .tuple = try program.addExprSpan(&.{ captured_value, call_second }) } });
    const call_first = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{
        .callee = .{ .lifted = first },
        .args = .empty(),
        .captures = .empty(),
    } } });
    program.setFn(first, .{
        .symbol = @enumFromInt(1),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = first_body },
        .ret = ty,
    });
    program.setFn(second, .{
        .symbol = @enumFromInt(2),
        .args = .empty(),
        .captures = .empty(),
        .body = .{ .roc = call_first },
        .ret = ty,
    });

    try recomputeCaptures(allocator, &program);

    try std.testing.expectEqual(@as(usize, 1), program.typedLocalSpan(program.getFn(first).captures).len);
    try std.testing.expectEqual(@as(usize, 1), program.typedLocalSpan(program.getFn(second).captures).len);
    const finalized_first = switch (program.getExpr(call_first).data) {
        .call_proc => |call| call,
        else => return error.TestUnexpectedResult,
    };
    const finalized_second = switch (program.getExpr(call_second).data) {
        .call_proc => |call| call,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(@as(usize, 1), program.captureOperandSpan(finalized_first.captures).len);
    try std.testing.expectEqual(@as(usize, 1), program.captureOperandSpan(finalized_second.captures).len);
}

test "monotype lifted lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}

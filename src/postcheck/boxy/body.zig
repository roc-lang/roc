//! Lowering one worker's checked body to LIR (design.md "`.boxy` Runtime
//! Lowering", "Dynamic Values In LIR").
//!
//! Statements are emitted in execution order. The builder always holds the
//! statement "hole" the next statement goes into: a placeholder statement
//! that emitting replaces in place, leaving a fresh hole as the new
//! statement's successor. Control flow keeps one hole per pending successor.
//!
//! Pending work lives on an explicit task stack: an expression task lowers a
//! checked expression and pushes the local holding its value onto the value
//! stack; continuation tasks pop the values their expression's operands
//! left. Source depth never becomes native call depth.
//!
//! Descriptors a body needs are emitted into the worker's entry chain, which
//! runs before the dynamic frame region is allocated and before the body.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const NumeralLiteral = @FieldType(checked.CheckedNumeralData, "literal");
const LIR = lir_core.LIR;
const LirStore = lir_core.LirStore;
const CheckedArithmetic = lir_core.CheckedArithmetic;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const TypeId = types.TypeId;
const ModuleIndex = types.ModuleIndex;
const Program = lower.Program;
const WorkerId = lower.WorkerId;
const invariant = types.invariant;
const invariantFmt = types.invariantFmt;

const CheckedExprId = checked.CheckedExprId;
const CheckedPatternId = checked.CheckedPatternId;
const CheckedStatementId = checked.CheckedStatementId;

/// Lower worker `id`'s body into its procedure.
pub fn lowerWorker(program: *Program, id: WorkerId) Allocator.Error!void {
    var builder = try Builder.init(program, id);
    defer builder.deinit();
    try builder.lowerBody();
}

const Task = union(enum) {
    expr: CheckedExprId,
    stmt: CheckedStatementId,
    /// Pop the value of a `decl` statement's expression and bind its pattern.
    bind: CheckedPatternId,
    /// Pop and drop one value.
    discard,
    /// Pop the arguments of a call to `callee` and call it.
    finish_call: struct { callee: WorkerId, hidden: []LocalId, arg_count: u32, result: TypeId },
    finish_erased_call: struct { fn_ty: TypeId, arg_count: u32, result: TypeId },
    finish_low_level: struct { op: LIR.LowLevel, arg_count: u32, result: TypeId, arg_types: []const TypeId = &.{} },
    finish_record: CheckedExprId,
    finish_tuple: CheckedExprId,
    finish_tag: CheckedExprId,
    /// `finish_tag` at the type of the nominal the tag constructs.
    finish_tag_at: struct { expr: CheckedExprId, ty: TypeId },
    finish_list: CheckedExprId,
    finish_str: struct { count: u32 },
    finish_eq: struct { negated: bool, operand: TypeId },
    /// Pop a Bool and check it as an `expect`.
    finish_expect: CheckedExprId,
    /// Pop the condition of if-branch `index` of `expr`.
    if_test: struct { expr: CheckedExprId, index: u32, join: LIR.JoinPointId, merge: ?MergeId },
    /// Pop a value and jump to `join` with it.
    jump: LIR.JoinPointId,
    /// Jump to a join that carries no value (a loop's head or exit).
    jump_without_value: LIR.JoinPointId,
    /// Pop a literal conversion's `Try` result and unwrap it.
    unwrap_literal: struct { expr: CheckedExprId, try_ty: TypeId, kind: LIR.LiteralRejectionKind },
    /// Push the `Numeral` argument of a `from_numeral` call.
    numeral_arg: struct { literal: NumeralLiteral, ty: TypeId },
    /// Pop a `for` loop's iterable and run the loop.
    for_start: struct { pattern: CheckedPatternId, body: CheckedExprId, plan: static_dispatch.IteratorForPlanId, mutations: ?checked.LoopMutationPlanId },
    /// Rebind a merge's binders to their values before its construct.
    restore_merge: MergeId,
    /// Rebind a merge's binders to its join's parameters.
    enter_merge: MergeId,
    /// Pop a loop condition: enter the body or leave the loop.
    loop_test: struct { body: CheckedExprId, head: LIR.JoinPointId, exit: LIR.JoinPointId },
    end_loop,
    /// Continue emitting at `hole`.
    resume_at: CFStmtId,
    finish_field_access: CheckedExprId,
    /// Pop a tag union value and compare its discriminant with `tag`'s.
    finish_discriminant_eq: struct { value_ty: TypeId, tag: check.CheckedNames.TagLabelId, negated: bool },
    finish_tuple_access: CheckedExprId,
    /// Pop a value of this type, report it through `dbg`, and push it back.
    finish_dbg: TypeId,
    restore: struct { module: ModuleIndex, node: checked.ConstNodeId, ty: TypeId },
    /// Push the function value a callable-eval binding stored.
    restore_callable_eval: struct { module: ModuleIndex, template: checked.CallableEvalTemplateId, ty: TypeId },
    /// Pop a restored closure's capture values and build it.
    finish_restore_closure: struct { worker: WorkerId, count: u32, descs: []const LocalId },
    finish_restore_struct: struct { ty: TypeId, count: u32 },
    finish_restore_tag: struct { ty: TypeId, name: types.NameId, count: u32 },
    finish_restore_list: struct { ty: TypeId, count: u32 },
    /// Push a value of `ty` for an expression whose evaluation diverged.
    unreachable_value: TypeId,
    /// Push the `{}` value of a construct whose value nothing reads.
    unreachable_or_unit,
    /// Push a string literal of the checked bodies as a `Str` value.
    push_value_str: checked.CheckedStringLiteralId,
    /// Push `local` as a value.
    push_value: LocalId,
    /// Pop a value; the enclosing procedure returns it.
    ret,
    /// Pop the left operand of a short-circuit operator.
    short_circuit: struct { is_and: bool, rhs: CheckedExprId, join: LIR.JoinPointId },
    /// Pop the scrutinee of a match and compile its branches.
    match_start: CheckedExprId,
    /// Test branch `index` of a match against the scrutinee.
    match_branch: struct { expr: CheckedExprId, index: u32, scrutinee: LocalId, join: LIR.JoinPointId },
    /// Pop a branch guard and continue into the branch's body or the next
    /// branch.
    match_guard: struct { expr: CheckedExprId, index: u32, scrutinee: LocalId, join: LIR.JoinPointId, miss: CFStmtId },
};

/// A control-flow construct's merge of the binders it reassigns (a checked
/// state merge plan): their values before it, and the join parameters that
/// carry their values after it.
const Merge = struct {
    binders: []const checked.PatternBinderId,
    pre: []LocalId,
    params: []LocalId,
};

const MergeId = u32;

/// What a join's parameters are: an optional value, then the binders of its
/// merge.
const JoinInfo = struct {
    value: ?LocalId,
    merge: ?MergeId,
};

const Builder = struct {
    program: *Program,
    allocator: Allocator,
    worker_id: WorkerId,
    module: ModuleIndex,
    view: *const types.ModuleView,
    env: types.Env,
    store: *LirStore,
    frame_locals: std.ArrayList(LocalId) = .empty,
    binders: std.AutoHashMapUnmanaged(checked.PatternBinderId, LocalId) = .empty,
    /// The descriptor local of each scope variable (its hidden parameter).
    var_descs: []LocalId = &.{},
    /// The dictionaries in scope (`Worker.scope_dictionary_indices` order).
    dict_locals: []LocalId = &.{},
    type_descs: std.AutoHashMapUnmanaged(TypeId, LocalId) = .empty,
    opaque_word_desc: ?LocalId = null,
    name_locals: std.AutoHashMapUnmanaged(types.NameId, LocalId) = .empty,
    entry_hole: CFStmtId = undefined,
    hole: ?CFStmtId = null,
    join_points: std.ArrayList(LIR.JoinPoint) = .empty,
    /// What each join of this body carries, by join id.
    join_infos: std.ArrayList(JoinInfo) = .empty,
    merges: std.ArrayList(Merge) = .empty,
    /// The exit joins of the loops being lowered, innermost last.
    loops: std.ArrayList(LIR.JoinPointId) = .empty,
    next_join: u32 = 0,
    values: std.ArrayList(LocalId) = .empty,
    tasks: std.ArrayList(Task) = .empty,
    region: base.Region = base.Region.zero(),
    has_dynamic_frame: bool = false,
    result_local: ?LocalId = null,

    fn init(program: *Program, id: WorkerId) Allocator.Error!Builder {
        const worker = program.worker(id);
        const bound = try program.arena.allocator().alloc(TypeId, worker.scheme_vars.len);
        for (bound, 0..) |*slot, index| slot.* = try program.store.variable(@intCast(index));
        return .{
            .program = program,
            .allocator = program.allocator,
            .worker_id = id,
            .module = worker.module,
            .view = program.module(worker.module),
            .env = .{ .module = worker.module, .vars = worker.scheme_vars, .bound = bound },
            .store = &program.result.store,
        };
    }

    fn deinit(self: *Builder) void {
        self.frame_locals.deinit(self.allocator);
        self.binders.deinit(self.allocator);
        self.type_descs.deinit(self.allocator);
        self.name_locals.deinit(self.allocator);
        self.join_points.deinit(self.allocator);
        self.join_infos.deinit(self.allocator);
        self.merges.deinit(self.allocator);
        self.loops.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.tasks.deinit(self.allocator);
    }

    fn currentWorker(self: *Builder) *lower.Worker {
        return self.program.worker(self.worker_id);
    }

    fn bodies(self: *const Builder) checked.CheckedBodyStoreView {
        return self.view.bodies();
    }

    // -- Locals --

    fn addLocal(self: *Builder, local: LIR.Local) Allocator.Error!LocalId {
        const id = try self.store.addLocal(local);
        try self.frame_locals.append(self.allocator, id);
        return id;
    }

    fn closedLocal(self: *Builder, layout_idx: layout.Idx) Allocator.Error!LocalId {
        return self.addLocal(.{ .layout_idx = layout_idx });
    }

    /// A local holding a value of `ty`: an ordinary local of its committed
    /// layout when `ty` is closed, otherwise a dynamic local of `storage`.
    fn localOf(self: *Builder, ty: TypeId, storage: LIR.DynStorage) Allocator.Error!LocalId {
        if (self.program.store.isClosed(ty)) return self.closedLocal(try self.program.layouts.layoutOf(ty));
        const desc = try self.descOf(ty);
        if (storage == .frame) self.has_dynamic_frame = true;
        return self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = storage } });
    }

    fn convert(self: *Builder, ty: checked.CheckedTypeId) Allocator.Error!TypeId {
        return self.program.converter().convert(self.module, ty, self.env);
    }

    fn exprType(self: *Builder, expr: CheckedExprId) Allocator.Error!TypeId {
        return self.convert(self.bodies().expr(expr).ty);
    }

    fn patternType(self: *Builder, pattern: CheckedPatternId) Allocator.Error!TypeId {
        return self.convert(self.bodies().pattern(pattern).ty);
    }

    // -- Statements --

    fn origin(self: *const Builder, kind: LIR.OriginKind) LIR.StmtOrigin {
        return .{
            .loc = self.program.sourceLoc(self.module, self.region),
            .region = self.region,
            .inline_scope = LIR.InlineScopeId.none,
            .kind = kind,
        };
    }

    fn newHole(self: *Builder) Allocator.Error!CFStmtId {
        return self.store.addCFStmt(.runtime_error, self.origin(.lowering_glue));
    }

    /// Replace `hole` with `stmt`.
    fn fill(self: *Builder, hole: CFStmtId, stmt: LIR.CFStmt, kind: LIR.OriginKind) Allocator.Error!void {
        try self.store.replaceCFStmt(hole, stmt, self.origin(kind));
        self.store.noteStmtShapes(stmt);
    }

    /// The hole the next statement goes into. Code after a diverging
    /// statement is unreachable; it is still emitted, into a hole nothing
    /// reaches.
    fn currentHole(self: *Builder) Allocator.Error!CFStmtId {
        if (self.hole) |hole| return hole;
        const hole = try self.newHole();
        self.hole = hole;
        return hole;
    }

    /// Emit a straight-line statement built by `make(next)`.
    fn emitLinear(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const hole = try self.currentHole();
        const next = try self.newHole();
        var linked = stmt;
        setNext(&linked, next);
        try self.fill(hole, linked, .source);
        self.hole = next;
    }

    /// Emit a terminal statement; nothing follows it.
    fn emitTerminal(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const hole = try self.currentHole();
        try self.fill(hole, stmt, .source);
        self.hole = null;
    }

    fn emitEntry(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const next = try self.newHole();
        var linked = stmt;
        setNext(&linked, next);
        try self.fill(self.entry_hole, linked, .scaffold);
        self.entry_hole = next;
    }

    fn setNext(stmt: *LIR.CFStmt, next: CFStmtId) void {
        switch (stmt.*) {
            inline .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .dyn,
            => |*s| s.next = next,
            .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => invariant("asked to link the successor of a non-linear statement"),
        }
    }

    fn emitLowLevel(self: *Builder, target: LocalId, op: LIR.LowLevel, args: []const LocalId) Allocator.Error!void {
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .args = try self.store.addLocalSpan(args),
            .next = undefined,
        } });
    }

    fn emitDyn(self: *Builder, target: ?LocalId, op: LIR.DynOp) Allocator.Error!void {
        try self.emitLinear(.{ .dyn = .{ .target = target, .data = try self.store.addStmtRow(LIR.DynOp, op), .next = undefined } });
    }

    fn freshJoin(self: *Builder) LIR.JoinPointId {
        const id: LIR.JoinPointId = @fromBackingInt(self.next_join);
        self.next_join += 1;
        return id;
    }

    /// Start a merge of `binders`, recording their current values.
    fn beginMerge(self: *Builder, binders: []const checked.PatternBinderId) Allocator.Error!?MergeId {
        if (binders.len == 0) return null;
        const arena = self.program.arena.allocator();
        const pre = try arena.alloc(LocalId, binders.len);
        const params = try arena.alloc(LocalId, binders.len);
        for (binders, pre, params) |binder, *before, *param| {
            before.* = self.binders.get(binder) orelse invariant("a state merge named a binder this body never bound");
            var local = self.store.getLocal(before.*);
            if (local.dyn) |*dyn| {
                dyn.storage = .frame;
                self.has_dynamic_frame = true;
            }
            param.* = try self.addLocal(local);
        }
        const id: MergeId = @intCast(self.merges.items.len);
        try self.merges.append(self.allocator, .{ .binders = binders, .pre = pre, .params = params });
        return id;
    }

    /// Declare a join carrying an optional value and a merge's binders; the
    /// builder continues in its remainder. Returns the join's body hole.
    fn declareMergeJoin(self: *Builder, id: LIR.JoinPointId, value: ?LocalId, merge: ?MergeId) Allocator.Error!CFStmtId {
        var params: std.ArrayList(LocalId) = .empty;
        defer params.deinit(self.allocator);
        if (value) |local| try params.append(self.allocator, local);
        if (merge) |m| try params.appendSlice(self.allocator, self.merges.items[m].params);
        const index = @backingInt(id);
        while (self.join_infos.items.len <= index) try self.join_infos.append(self.allocator, .{ .value = null, .merge = null });
        self.join_infos.items[index] = .{ .value = value, .merge = merge };
        return try self.declareJoin(id, params.items);
    }

    fn restoreMerge(self: *Builder, merge: MergeId) Allocator.Error!void {
        const m = self.merges.items[merge];
        for (m.binders, m.pre) |binder, value| try self.binders.put(self.allocator, binder, value);
    }

    fn enterMerge(self: *Builder, merge: MergeId) Allocator.Error!void {
        const m = self.merges.items[merge];
        for (m.binders, m.params) |binder, value| try self.binders.put(self.allocator, binder, value);
    }

    /// Declare join `id` with `params` here; continue in its remainder, and
    /// return the hole of its body.
    fn declareJoin(self: *Builder, id: LIR.JoinPointId, params: []const LocalId) Allocator.Error!CFStmtId {
        const hole = try self.currentHole();
        const body = try self.newHole();
        const remainder = try self.newHole();
        const span = try self.store.addLocalSpan(params);
        try self.fill(hole, .{ .join = .{ .id = id, .params = span, .body = body, .remainder = remainder } }, .lowering_glue);
        try self.join_points.append(self.allocator, .{ .id = id, .params = span, .body = body });
        self.hole = remainder;
        return body;
    }

    /// Jump to join `id`, assigning its parameters: `value` to its value
    /// parameter, and its merge's binders' current values to theirs.
    fn emitJump(self: *Builder, id: LIR.JoinPointId, value: ?LocalId) Allocator.Error!void {
        const index = @backingInt(id);
        const info: JoinInfo = if (index < self.join_infos.items.len) self.join_infos.items[index] else .{ .value = null, .merge = null };
        if (info.value) |param| {
            const source = value orelse invariant("a jump to a value join carried no value");
            try self.emitLinear(.{ .set_local = .{ .target = param, .value = source, .mode = .initialize_join_param, .next = undefined } });
        }
        if (info.merge) |merge| {
            const m = self.merges.items[merge];
            for (m.binders, m.params) |binder, param| {
                const current = self.binders.get(binder) orelse invariant("a merged binder lost its value");
                try self.emitLinear(.{ .set_local = .{ .target = param, .value = current, .mode = .initialize_join_param, .next = undefined } });
            }
        }
        try self.emitTerminal(.{ .jump = .{ .target = id } });
    }

    /// Emit `switch (cond) { 1 => then } else otherwise` and return the two
    /// holes.
    fn emitBoolSwitch(self: *Builder, cond: LocalId) Allocator.Error!struct { then: CFStmtId, otherwise: CFStmtId } {
        const then = try self.newHole();
        const otherwise = try self.newHole();
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = then }});
        try self.emitTerminal(.{ .switch_stmt = .{ .cond = cond, .branches = branches, .default_branch = otherwise } });
        return .{ .then = then, .otherwise = otherwise };
    }

    // -- Descriptors --

    /// The descriptor local of `ty`, emitted into the entry chain on first
    /// use: a static descriptor read for a closed type, a hidden parameter
    /// for a scope variable, and a composition of its components'
    /// descriptors otherwise (design.md "Type Descriptors"). Components are
    /// composed before the types that contain them, with an explicit stack.
    fn descOf(self: *Builder, root: TypeId) Allocator.Error!LocalId {
        if (self.type_descs.get(root)) |existing| return existing;
        var pending: std.ArrayList(struct { ty: TypeId, expanded: bool }) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .ty = root, .expanded = false });
        while (pending.items.len > 0) {
            const top = &pending.items[pending.items.len - 1];
            const ty = top.ty;
            if (self.type_descs.contains(ty)) {
                _ = pending.pop();
                continue;
            }
            if (try self.leafDesc(ty)) |local| {
                try self.type_descs.put(self.allocator, ty, local);
                _ = pending.pop();
                continue;
            }
            const children = try self.descComponents(ty);
            defer self.allocator.free(children);
            if (!top.expanded) {
                top.expanded = true;
                for (children) |child| {
                    if (!self.type_descs.contains(child)) {
                        if (child == ty) invariant("a type's descriptor composition reached itself");
                        try pending.append(self.allocator, .{ .ty = child, .expanded = false });
                    }
                }
                continue;
            }
            for (children) |child| {
                if (!self.type_descs.contains(child)) invariant("composing the descriptor of a recursive generic type is not yet lowered by Boxy");
            }
            const local = try self.composeDesc(ty);
            try self.type_descs.put(self.allocator, ty, local);
            _ = pending.pop();
        }
        return self.type_descs.get(root).?;
    }

    /// A descriptor needing no component descriptors: a static descriptor,
    /// or a scope variable's hidden parameter.
    fn leafDesc(self: *Builder, ty: TypeId) Allocator.Error!?LocalId {
        const store = &self.program.store;
        if (store.isClosed(ty)) return try self.staticDesc(ty);
        return switch (store.get(ty)) {
            .variable => |index| self.var_descs[index],
            else => null,
        };
    }

    fn staticDesc(self: *Builder, ty: TypeId) Allocator.Error!LocalId {
        if (self.type_descs.get(ty)) |existing| return existing;
        const slot = try self.program.static_descs.descriptorSlot(ty);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.type_descs.put(self.allocator, ty, local);
        return local;
    }

    fn nameDesc(self: *Builder, name: types.NameId) Allocator.Error!LocalId {
        if (self.name_locals.get(name)) |existing| return existing;
        const slot = try self.program.static_descs.nameSlot(name);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.name_locals.put(self.allocator, name, local);
        return local;
    }

    /// The types whose descriptors compose `ty`'s. Caller frees.
    fn descComponents(self: *Builder, ty: TypeId) Allocator.Error![]TypeId {
        const store = &self.program.store;
        var out: std.ArrayList(TypeId) = .empty;
        errdefer out.deinit(self.allocator);
        switch (store.get(ty)) {
            .list => |elem| try out.append(self.allocator, elem),
            .box => |payload| try out.append(self.allocator, payload),
            .tuple => |items| try out.appendSlice(self.allocator, store.span(items)),
            .record => |record| {
                if (record.ext) |ext| try out.append(self.allocator, ext);
                const fields = try self.allocator.dupe(types.Field, store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                for (fields) |field| try out.append(self.allocator, try self.program.layouts.fieldStorage(field));
            },
            .tag_union => |tag_union| {
                if (tag_union.ext) |ext| try out.append(self.allocator, ext);
                for (store.tagSpan(tag_union.tags)) |tag| try out.appendSlice(self.allocator, store.span(tag.payloads));
            },
            .nominal => try out.append(self.allocator, try self.program.converter().backing(ty)),
            .primitive, .empty_record, .function, .variable => {},
        }
        return try out.toOwnedSlice(self.allocator);
    }

    /// Emit the composition of `ty`'s descriptor from its components'.
    fn composeDesc(self: *Builder, ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const local = try self.closedLocal(.opaque_ptr);
        const op: LIR.DynOp = switch (store.get(ty)) {
            .list => |elem| .{ .desc_list = .{ .elem = self.type_descs.get(elem).? } },
            .box => |payload| .{ .desc_box = .{ .payload = self.type_descs.get(payload).? } },
            .tuple => |items_span| blk: {
                const items = store.span(items_span);
                const descs = try self.allocator.alloc(LocalId, items.len);
                defer self.allocator.free(descs);
                for (items, descs) |item, *desc| desc.* = self.type_descs.get(item).?;
                break :blk .{ .desc_tuple = .{ .elems = try self.store.addLocalSpan(descs) } };
            },
            .record => |record| blk: {
                const fields = try self.allocator.dupe(types.Field, store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                const names = try self.allocator.alloc(LocalId, fields.len);
                defer self.allocator.free(names);
                const descs = try self.allocator.alloc(LocalId, fields.len);
                defer self.allocator.free(descs);
                for (fields, names, descs) |field, *name, *desc| {
                    name.* = try self.nameDesc(field.name);
                    desc.* = self.type_descs.get(try self.program.layouts.fieldStorage(field)).?;
                }
                break :blk .{ .desc_record = .{
                    .names = try self.store.addLocalSpan(names),
                    .descs = try self.store.addLocalSpan(descs),
                    .ext = if (record.ext) |ext| self.type_descs.get(ext).? else null,
                } };
            },
            .tag_union => |tag_union| blk: {
                const tags = try self.allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
                defer self.allocator.free(tags);
                var names: std.ArrayList(LocalId) = .empty;
                defer names.deinit(self.allocator);
                var counts: std.ArrayList(u32) = .empty;
                defer counts.deinit(self.allocator);
                var payloads: std.ArrayList(LocalId) = .empty;
                defer payloads.deinit(self.allocator);
                for (tags) |tag| {
                    try names.append(self.allocator, try self.nameDesc(tag.name));
                    const tag_payloads = store.span(tag.payloads);
                    try counts.append(self.allocator, @intCast(tag_payloads.len));
                    for (tag_payloads) |payload| try payloads.append(self.allocator, self.type_descs.get(payload).?);
                }
                break :blk .{ .desc_tag_union = .{
                    .names = try self.store.addLocalSpan(names.items),
                    .payload_counts = try self.store.addU32Span(counts.items),
                    .payloads = try self.store.addLocalSpan(payloads.items),
                    .ext = if (tag_union.ext) |ext| self.type_descs.get(ext).? else null,
                } };
            },
            .nominal => |nominal| blk: {
                if (nominal.builtin == .bool) invariant("Bool is never an open type");
                break :blk .{ .desc_nominal = .{
                    .backing = self.type_descs.get(try self.program.converter().backing(ty)).?,
                    .name = try self.nameDesc(nominal.name),
                    .methods = null,
                    .inspect_opaque = nominal.is_opaque,
                } };
            },
            .primitive, .empty_record, .function, .variable => unreachable, // `leafDesc` answers these
        };
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, op), .next = undefined } });
        return local;
    }

    // -- Body --

    fn lowerBody(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const template = switch (worker.key) {
            .template => |key| self.view.view.checked_procedure_templates.templates.items[@backingInt(key.template)],
            .lambda => |key| return try self.lowerLambdaBody(key.expr),
            .method_entry => |target| return try self.lowerMethodEntry(target),
            .structural => |method| return try self.lowerStructuralMethod(method),
        };
        // Signature: explicit parameters, then hidden descriptors and
        // dictionaries.
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = try self.program.arena.allocator().alloc(LocalId, worker.scheme_vars.len);
        for (self.var_descs) |*slot| slot.* = try self.closedLocal(.opaque_ptr);
        const params = try self.allocator.alloc(LocalId, worker.params.len);
        defer self.allocator.free(params);
        for (params, worker.params) |*param, param_ty| param.* = try self.localOf(param_ty, .param);
        try args.appendSlice(self.allocator, params);
        try args.appendSlice(self.allocator, self.var_descs);
        self.dict_locals = try self.program.arena.allocator().alloc(LocalId, worker.dictionary_params.len);
        for (self.dict_locals) |*slot| slot.* = try self.closedLocal(try self.erasedLayout());
        try args.appendSlice(self.allocator, self.dict_locals);
        var ret_layout: layout.Idx = undefined;
        if (self.program.store.isClosed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }

        const body_start = try self.newHole();
        self.hole = body_start;
        switch (template.body) {
            .checked_body => |body_id| {
                const body = self.bodies().body(body_id);
                const root = self.bodies().expr(body.root_expr);
                switch (root.data) {
                    .lambda => |lambda| {
                        if (lambda.args.len != params.len) invariant("a template's lambda arity differs from its signature");
                        for (lambda.args, params) |pattern, param| try self.bindIrrefutable(pattern, param);
                        try self.tasks.append(self.allocator, .ret);
                        try self.tasks.append(self.allocator, .{ .expr = lambda.body });
                    },
                    .hosted_lambda => invariant("hosted procedures are not yet lowered by Boxy"),
                    else => {
                        if (params.len != 0) invariant("a non-lambda template body has parameters");
                        try self.tasks.append(self.allocator, .ret);
                        try self.tasks.append(self.allocator, .{ .expr = body.root_expr });
                    },
                }
            },
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                const message = try self.store.insertString(@import("../common.zig").unimplemented_declaration_crash);
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = message } } });
            },
            .intrinsic_wrapper => invariant("intrinsic wrappers are not yet lowered by Boxy"),
            .entry_wrapper => |wrapper_id| {
                const wrapper = self.view.view.entry_wrappers.get(wrapper_id);
                const root = self.view.view.compile_time_roots.root(wrapper.root);
                switch (root.kind) {
                    .expect, .repl_expr => {},
                    else => invariantFmt("{s} entry wrappers are not yet lowered by Boxy", .{@tagName(root.kind)}),
                }
                if (params.len != 0) invariant("an entry wrapper has parameters");
                try self.tasks.append(self.allocator, .ret);
                try self.tasks.append(self.allocator, .{ .expr = wrapper.body_expr });
            },
        }
        try self.run();

        try self.finishProc(entry, body_start, args.items, ret_layout);
    }

    /// Link the entry chain to the body and publish the procedure.
    fn finishProc(self: *Builder, entry: CFStmtId, body_start: CFStmtId, args: []const LocalId, ret_layout: layout.Idx) Allocator.Error!void {
        const worker = self.currentWorker().*;
        // Entry chain, then the dynamic frame region, then the body.
        if (self.has_dynamic_frame) {
            try self.emitEntry(.{ .dyn = .{ .target = null, .data = try self.store.addStmtRow(LIR.DynOp, .frame_alloc), .next = undefined } });
        }
        // The entry chain's last hole continues into the body: it becomes a
        // copy of the body's first statement.
        const proc_body: CFStmtId = if (entry == self.entry_hole) body_start else blk: {
            try self.store.replaceCFStmt(self.entry_hole, self.store.getCFStmt(body_start), self.store.stmtOrigin(body_start));
            break :blk entry;
        };

        std.mem.sort(LocalId, self.frame_locals.items, {}, struct {
            fn lessThan(_: void, a: LocalId, b: LocalId) bool {
                return @backingInt(a) < @backingInt(b);
            }
        }.lessThan);
        std.mem.sort(LIR.JoinPoint, self.join_points.items, {}, struct {
            fn lessThan(_: void, a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
                return @backingInt(a.id) < @backingInt(b.id);
            }
        }.lessThan);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.args = try self.store.addLocalSpan(args);
        proc.ret_layout = ret_layout;
        proc.dyn_result = self.result_local;
        proc.frame_locals = try self.store.addLocalSpan(self.frame_locals.items);
        proc.join_points = try self.store.addJoinPointSpan(self.join_points.items);
        proc.body = proc_body;
        proc.shapes = self.store.shapes;
        self.store.shapes = .{};
    }

    /// A lambda worker in the uniform erased-callable ABI: its one argument
    /// is the packed argument record, and its captures hold the captured
    /// values followed by the outer worker's descriptors.
    fn lowerLambdaBody(self: *Builder, lambda_expr: CheckedExprId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const lambda = switch (self.bodies().expr(lambda_expr).data) {
            .lambda => |lambda| lambda,
            else => invariant("a lambda worker's expression is not a lambda"),
        };
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;

        // Captures: values, then the scope's descriptors.
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        self.var_descs = try self.program.arena.allocator().alloc(LocalId, worker.scheme_vars.len);
        const capture_count = worker.capture_types.len + worker.scheme_vars.len + worker.captured_dictionaries;
        var capture_values: []LocalId = &.{};
        if (!self.allClosed(worker.capture_types)) {
            capture_values = try self.dynamicCaptures(worker, capture_ptr);
        } else if (capture_count != 0) {
            const capture_layout = try self.captureLayout(worker.capture_types, worker.scheme_vars.len, worker.captured_dictionaries);
            const record = try self.closedLocal(capture_layout);
            try self.emitEntry(.{ .assign_low_level = .{
                .target = record,
                .op = .erased_capture_load,
                .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
                .args = try self.store.addLocalSpan(&.{capture_ptr}),
                .next = undefined,
            } });
            capture_values = try self.allocator.alloc(LocalId, capture_count);
            for (capture_values, 0..) |*slot, index| {
                const field_layout = if (index < worker.capture_types.len)
                    try self.program.layouts.layoutOf(worker.capture_types[index])
                else if (index < worker.capture_types.len + worker.scheme_vars.len)
                    layout.Idx.opaque_ptr
                else
                    try self.erasedLayout();
                slot.* = try self.closedLocal(field_layout);
                try self.emitEntry(.{ .assign_ref = .{ .target = slot.*, .op = LIR.RefOp.field(record, @intCast(index)), .next = undefined } });
            }
            @memcpy(self.var_descs, capture_values[worker.capture_types.len..][0..worker.scheme_vars.len]);
            self.dict_locals = try self.program.arena.allocator().dupe(LocalId, capture_values[worker.capture_types.len + worker.scheme_vars.len ..]);
        }
        defer if (capture_count != 0) self.allocator.free(capture_values);
        for (worker.captures, 0..) |capture, index| {
            const binder = self.bodies().pattern_binder_by_pattern[@backingInt(capture.pattern)].get() orelse
                invariant("a closure capture named a pattern without a binder");
            try self.binders.put(self.allocator, binder, capture_values[index]);
        }

        // The argument record.
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        var record_param: ?LocalId = null;
        if (worker.params.len != 0) {
            record_param = try self.argsRecordLocal(worker.params, .param);
            try args.append(self.allocator, record_param.?);
        }
        var ret_layout: layout.Idx = undefined;
        if (self.program.store.isClosed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }

        const body_start = try self.newHole();
        self.hole = body_start;
        if (lambda.args.len != worker.params.len) invariant("a lambda's arity differs from its type");
        for (lambda.args, worker.params, 0..) |pattern, param_ty, index| {
            const value = try self.argsRecordField(worker.params, record_param.?, @intCast(index), param_ty);
            try self.bindIrrefutable(pattern, value);
        }
        try self.tasks.append(self.allocator, .ret);
        try self.tasks.append(self.allocator, .{ .expr = lambda.body });
        try self.run();
        try self.finishProc(entry, body_start, args.items, ret_layout);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = capture_ptr;
        proc.abi = .erased_record;
    }

    /// Read a dynamic capture record at `capture_ptr`: its descriptor word,
    /// the descriptor and dictionary words (which the captured values'
    /// descriptors compose from), then views of the captured values. Returns
    /// the capture values in the closed record's order: values, descriptors,
    /// dictionaries.
    fn dynamicCaptures(self: *Builder, worker: lower.Worker, capture_ptr: LocalId) Allocator.Error![]LocalId {
        const word_count = worker.scheme_vars.len + worker.captured_dictionaries;
        const record_desc = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record_desc,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = record_desc, .storage = .view } });
        try self.emitEntry(.{ .dyn = .{ .target = record, .data = try self.store.addStmtRow(LIR.DynOp, .{ .view = .{ .address = capture_ptr } }), .next = undefined } });
        const values = try self.allocator.alloc(LocalId, worker.capture_types.len + word_count);
        for (0..word_count) |index| {
            const word = try self.closedLocal(if (index < worker.scheme_vars.len) .opaque_ptr else try self.erasedLayout());
            try self.emitEntry(.{ .dyn = .{ .target = word, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = record_desc, .semantic_index = @intCast(1 + index) } }), .next = undefined } });
            values[worker.capture_types.len + index] = word;
        }
        @memcpy(self.var_descs, values[worker.capture_types.len..][0..worker.scheme_vars.len]);
        self.dict_locals = try self.program.arena.allocator().dupe(LocalId, values[worker.capture_types.len + worker.scheme_vars.len ..]);
        for (worker.capture_types, 0..) |ty, index| {
            const local = try self.localOf(ty, .view);
            try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = record_desc, .semantic_index = @intCast(1 + word_count + index) } }), .next = undefined } });
            values[index] = local;
        }
        return values;
    }

    fn erasedLayout(self: *Builder) Allocator.Error!layout.Idx {
        return self.program.result.layouts.insertErasedCallable();
    }

    /// Load an erased worker's capture record (closed fields of
    /// `field_layouts`) at the start of its entry chain, returning its fields.
    fn loadCaptures(self: *Builder, capture_ptr: LocalId, field_layouts: []const layout.Idx) Allocator.Error![]LocalId {
        const fields = try self.allocator.alloc(layout.StructField, field_layouts.len);
        defer self.allocator.free(fields);
        for (fields, field_layouts, 0..) |*field, field_layout, index| field.* = .{ .index = @intCast(index), .layout = field_layout };
        const record = try self.closedLocal(try self.program.result.layouts.putStructFields(fields));
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        const values = try self.allocator.alloc(LocalId, field_layouts.len);
        for (values, field_layouts, 0..) |*value, field_layout, index| {
            value.* = try self.closedLocal(field_layout);
            try self.emitEntry(.{ .assign_ref = .{ .target = value.*, .op = LIR.RefOp.field(record, @intCast(index)), .next = undefined } });
        }
        return values;
    }

    /// Begin an erased-ABI worker whose captures are `field_layouts`: entry
    /// chain, capture pointer, argument record, and result. Returns what
    /// `finishErasedWorker` needs.
    const ErasedStart = struct {
        entry: CFStmtId,
        body_start: CFStmtId,
        capture_ptr: LocalId,
        captures: []LocalId,
        args: []LocalId,
        record: ?LocalId,
        ret_layout: layout.Idx,
    };

    fn beginErasedWorker(self: *Builder, field_layouts: []const layout.Idx) Allocator.Error!ErasedStart {
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        const captures = if (field_layouts.len == 0) try self.allocator.alloc(LocalId, 0) else try self.loadCaptures(capture_ptr, field_layouts);
        return .{
            .entry = entry,
            .body_start = undefined,
            .capture_ptr = capture_ptr,
            .captures = captures,
            .args = &.{},
            .record = null,
            .ret_layout = undefined,
        };
    }

    /// The entry of a template worker reached through a dictionary.
    fn lowerMethodEntry(self: *Builder, target: WorkerId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const target_worker = self.program.worker(target).*;
        const field_layouts = try self.allocator.alloc(layout.Idx, target_worker.scheme_vars.len + target_worker.dictionary_params.len);
        defer self.allocator.free(field_layouts);
        for (field_layouts, 0..) |*field_layout, index| {
            field_layout.* = if (index < target_worker.scheme_vars.len) .opaque_ptr else try self.erasedLayout();
        }
        var start = try self.beginErasedWorker(field_layouts);
        defer self.allocator.free(start.captures);
        self.var_descs = try self.program.arena.allocator().dupe(LocalId, start.captures[0..target_worker.scheme_vars.len]);
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .call = .{ .target = target, .hidden = start.captures } });
    }

    /// A structural method over the one descriptor its dictionary captures.
    fn lowerStructuralMethod(self: *Builder, method: lower.StructuralMethod) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var start = try self.beginErasedWorker(&.{.opaque_ptr});
        defer self.allocator.free(start.captures);
        self.var_descs = try self.program.arena.allocator().dupe(LocalId, start.captures);
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .structural = method });
    }

    const ErasedBody = union(enum) {
        call: struct { target: WorkerId, hidden: []const LocalId },
        structural: lower.StructuralMethod,
    };

    fn runErasedBody(self: *Builder, start: *ErasedStart, params: []const TypeId, ret: TypeId, body: ErasedBody) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        var record: ?LocalId = null;
        if (params.len != 0) {
            record = try self.argsRecordLocal(params, .param);
            try args.append(self.allocator, record.?);
        }
        var ret_layout: layout.Idx = undefined;
        if (self.program.store.isClosed(ret)) {
            ret_layout = try self.program.layouts.layoutOf(ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        const values = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(values);
        for (values, params, 0..) |*value, param_ty, index| value.* = try self.argsRecordField(params, record.?, @intCast(index), param_ty);
        const result = switch (body) {
            .call => |call| blk: {
                const all = try std.mem.concat(self.allocator, LocalId, &.{ values, call.hidden });
                defer self.allocator.free(all);
                const target = try self.localOf(ret, .frame);
                try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(call.target).proc, .args = try self.store.addLocalSpan(all), .next = undefined } });
                break :blk target;
            },
            .structural => |method| blk: {
                const target = try self.localOf(ret, .frame);
                switch (method) {
                    .is_eq => try self.emitDyn(target, .{ .eq = .{ .lhs = values[0], .rhs = values[1], .desc = self.var_descs[0] } }),
                    .to_hash => try self.emitDyn(target, .{ .hash = .{ .hasher = values[1], .value = values[0], .desc = self.var_descs[0] } }),
                }
                break :blk target;
            },
        };
        try self.pushValue(result);
        try self.step(.ret);
        try self.finishProc(start.entry, body_start, args.items, ret_layout);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = start.capture_ptr;
        proc.abi = .erased_record;
    }

    /// The closed layout of a lambda's capture record: the captured values,
    /// then one descriptor pointer per scope variable, in that order of
    /// semantic indices.
    fn captureLayout(self: *Builder, capture_types: []const TypeId, descriptor_count: usize, dictionary_count: usize) Allocator.Error!layout.Idx {
        const fields = try self.allocator.alloc(layout.StructField, capture_types.len + descriptor_count + dictionary_count);
        defer self.allocator.free(fields);
        for (fields, 0..) |*field, index| {
            field.* = .{
                .index = @intCast(index),
                .layout = if (index < capture_types.len)
                    try self.program.layouts.layoutOf(capture_types[index])
                else if (index < capture_types.len + descriptor_count)
                    .opaque_ptr
                else
                    try self.erasedLayout(),
            };
        }
        return try self.program.result.layouts.putStructFields(fields);
    }

    /// The packed erased-call argument record of `params`: a declared-order
    /// struct when every parameter type is closed, a dynamic record composed
    /// with `desc_args` otherwise.
    fn argsRecordLocal(self: *Builder, params: []const TypeId, storage: LIR.DynStorage) Allocator.Error!LocalId {
        if (self.allClosed(params)) return self.closedLocal(try self.argsRecordLayout(params));
        const desc = try self.argsRecordDesc(params);
        if (storage == .frame) self.has_dynamic_frame = true;
        return self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = storage } });
    }

    fn allClosed(self: *Builder, tys: []const TypeId) bool {
        for (tys) |ty| {
            if (!self.program.store.isClosed(ty)) return false;
        }
        return true;
    }

    fn argsRecordLayout(self: *Builder, params: []const TypeId) Allocator.Error!layout.Idx {
        const fields = try self.allocator.alloc(layout.StructField, params.len);
        defer self.allocator.free(fields);
        for (fields, params, 0..) |*field, param, index| field.* = .{ .index = @intCast(index), .layout = try self.program.layouts.layoutOf(param) };
        return try self.program.result.layouts.putNominalStructFields(fields);
    }

    fn argsRecordDesc(self: *Builder, params: []const TypeId) Allocator.Error!LocalId {
        const descs = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(descs);
        for (descs, params) |*desc, param| desc.* = try self.descOf(param);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(descs) } }), .next = undefined } });
        return local;
    }

    /// Argument `index` of an argument record, as a borrowed read.
    fn argsRecordField(self: *Builder, params: []const TypeId, record: LocalId, index: u32, param_ty: TypeId) Allocator.Error!LocalId {
        if (self.store.localDyn(record)) |dyn| {
            const local = try self.localOf(param_ty, .view);
            try self.emitDyn(local, .{ .field_addr = .{ .base = record, .desc = dyn.desc, .semantic_index = index } });
            return local;
        }
        _ = params;
        const local = try self.localOf(param_ty, .frame);
        try self.emitLinear(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.field(record, index), .next = undefined } });
        return local;
    }

    fn run(self: *Builder) Allocator.Error!void {
        while (self.tasks.pop()) |task| try self.step(task);
        if (self.values.items.len != 0) invariant("body lowering left values on its value stack");
    }

    fn push(self: *Builder, task: Task) Allocator.Error!void {
        try self.tasks.append(self.allocator, task);
    }

    fn popValue(self: *Builder) LocalId {
        return self.values.pop() orelse invariant("body lowering popped an empty value stack");
    }

    fn pushValue(self: *Builder, local: LocalId) Allocator.Error!void {
        try self.values.append(self.allocator, local);
    }

    fn step(self: *Builder, task: Task) Allocator.Error!void {
        switch (task) {
            .expr => |expr| try self.stepExpr(expr),
            .stmt => |stmt| try self.stepStmt(stmt),
            .bind => |pattern| try self.bindIrrefutable(pattern, self.popValue()),
            .discard => _ = self.popValue(),
            .push_value => |local| try self.pushValue(local),
            .unreachable_value => |ty| try self.pushValue(try self.localOf(ty, .frame)),
            .unreachable_or_unit => try self.pushValue(try self.unitValue()),
            .push_value_str => |literal| try self.pushValue(try self.strLiteral(self.bodies().stringLiteral(literal))),

            .resume_at => |hole| self.hole = hole,
            .ret => {
                const value = self.popValue();
                if (self.result_local) |result| {
                    try self.emitDyn(result, .{ .copy = .{ .src = value, .desc = self.store.localDyn(result).?.desc } });
                    try self.emitTerminal(.{ .ret = .{ .value = result } });
                } else {
                    try self.emitTerminal(.{ .ret = .{ .value = value } });
                }
            },
            .jump => |join| try self.emitJump(join, self.popValue()),
            .jump_without_value => |join| try self.emitJump(join, null),
            .restore_merge => |merge| try self.restoreMerge(merge),
            .enter_merge => |merge| try self.enterMerge(merge),
            .loop_test => |loop| try self.loopTest(loop.body, loop.head, loop.exit),
            .end_loop => _ = self.loops.pop(),
            .for_start => |for_| try self.forStart(for_.pattern, for_.body, for_.plan, for_.mutations),
            .unwrap_literal => |unwrap| try self.unwrapLiteral(unwrap.expr, unwrap.try_ty, unwrap.kind),
            .numeral_arg => |arg| try self.pushValue(try self.numeralArgument(arg.literal, arg.ty)),
            .finish_call => |call| {
                defer self.allocator.free(call.hidden);
                try self.finishCall(call.callee, call.hidden, call.arg_count, call.result);
            },
            .finish_low_level => |ll| try self.finishLowLevel(ll.op, ll.arg_count, ll.result, ll.arg_types),
            .finish_record => |expr| try self.finishRecord(expr),
            .restore => |restore| try self.restoreStep(restore.module, restore.node, restore.ty),
            .restore_callable_eval => |eval| {
                const owner = self.program.module(eval.module);
                const template = owner.view.callable_eval_templates.templates[@backingInt(eval.template)];
                const root = owner.view.compile_time_roots.root(template.root);
                switch (root.payload) {
                    .fn_value => |fn_id| try self.restoreFn(eval.module, fn_id, eval.ty),
                    else => invariantFmt("a callable-eval binding whose root holds {s} is not yet lowered by Boxy", .{@tagName(root.payload)}),
                }
            },
            .finish_restore_closure => |closure| {
                const values = try self.popValues(closure.count);
                defer self.allocator.free(values);
                const worker = self.program.worker(closure.worker).*;
                if (self.allClosed(worker.capture_types)) {
                    try self.pushValue(try self.packClosure(closure.worker, values, worker.capture_types, closure.descs, &.{}));
                } else {
                    try self.pushValue(try self.packDynamicClosure(closure.worker, values, worker.capture_types, closure.descs, &.{}));
                }
            },
            .finish_restore_struct => |restore| {
                const values = try self.popValues(restore.count);
                defer self.allocator.free(values);
                try self.pushValue(try self.buildStruct(try self.structuralType(restore.ty), values));
            },
            .finish_restore_tag => |restore| {
                const values = try self.popValues(restore.count);
                defer self.allocator.free(values);
                try self.pushValue(try self.buildTag(restore.ty, restore.name, values));
            },
            .finish_restore_list => |restore| try self.finishRestoreList(restore.ty, restore.count),
            .finish_field_access => |expr| try self.finishFieldAccess(expr),
            .finish_discriminant_eq => |eq| try self.finishDiscriminantEq(eq.value_ty, eq.tag, eq.negated),
            .finish_tuple_access => |expr| {
                const access = self.bodies().expr(expr).data.tuple_access;
                const tuple = self.popValue();
                const tuple_ty = try self.structuralType(try self.exprType(access.tuple));
                const elems = switch (self.program.store.get(tuple_ty)) {
                    .tuple => |span| self.program.store.span(span),
                    else => invariant("a tuple access's receiver is not a tuple"),
                };
                try self.pushValue(try self.readField(tuple_ty, tuple, access.elem_index, elems[access.elem_index]));
            },
            .finish_dbg => |ty| {
                const value = self.popValue();
                const message = try self.closedLocal(.str);
                try self.emitDyn(message, .{ .inspect = .{ .value = value, .desc = try self.descOf(ty) } });
                try self.emitLinear(.{ .debug = .{ .message = message, .next = undefined } });
                try self.pushValue(value);
            },
            .finish_erased_call => |call| try self.finishErasedCall(call.fn_ty, call.arg_count, call.result),
            .finish_tuple => |expr| try self.finishTuple(expr),
            .finish_tag => |expr| try self.finishTag(expr, null),
            .finish_tag_at => |at| try self.finishTag(at.expr, at.ty),
            .finish_list => |expr| try self.finishList(expr),
            .finish_str => |str| try self.finishStr(str.count),
            .finish_eq => |eq| try self.finishEq(eq.negated, eq.operand),
            .finish_expect => |expr| {
                _ = expr;
                const cond = self.popValue();
                try self.emitLinear(.{ .expect = .{ .condition = cond, .site = try self.expectSite(), .next = undefined } });
                try self.pushValue(try self.unitValue());
            },
            .if_test => |test_| try self.ifTest(test_.expr, test_.index, test_.join, test_.merge),
            .match_start => |expr| try self.matchStart(expr),
            .short_circuit => |sc| try self.shortCircuit(sc.is_and, sc.rhs, sc.join),
            .match_branch => |branch| try self.matchBranch(branch.expr, branch.index, branch.scrutinee, branch.join),
            .match_guard => |guard| try self.matchGuard(guard.expr, guard.index, guard.scrutinee, guard.join, guard.miss),
        }
    }

    fn popValues(self: *Builder, count: u32) Allocator.Error![]LocalId {
        const result = try self.allocator.alloc(LocalId, count);
        var index = count;
        while (index > 0) {
            index -= 1;
            result[index] = self.popValue();
        }
        return result;
    }

    fn expectSite(self: *Builder) Allocator.Error!?LIR.ExpectSiteId {
        if (!self.program.observe_expects) return null;
        return try self.program.result.addExpectSite(self.program.sourceLoc(self.module, self.region), self.region);
    }

    /// The `{}` value.
    fn unitValue(self: *Builder) Allocator.Error!LocalId {
        const local = try self.closedLocal(.zst);
        try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
        return local;
    }

    fn boolValue(self: *Builder, value: bool) Allocator.Error!LocalId {
        const local = try self.closedLocal(.bool);
        const discriminant = @import("../common.zig").boolDiscriminant(value);
        try self.emitLinear(.{ .assign_tag = .{ .target = local, .variant_index = discriminant, .discriminant = discriminant, .payload = null, .next = undefined } });
        return local;
    }

    fn crashValue(self: *Builder, message: []const u8, ty: TypeId) Allocator.Error!void {
        const literal = try self.store.insertString(message);
        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal } } });
        try self.pushValue(try self.localOf(ty, .frame));
    }

    // -- Expressions --

    fn stepExpr(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        self.region = expr.source_region;
        switch (expr.data) {
            .numeral => |numeral| try self.lowerNumeral(expr_id, numeral),
            .str_segment => |literal| try self.pushValue(try self.strLiteral(self.bodies().stringLiteral(literal))),
            .str => |parts| {
                try self.push(.{ .finish_str = .{ .count = @intCast(parts.len) } });
                var index = parts.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .expr = parts[index] });
                }
            },
            .lookup_local => |lookup| try self.lowerLookupLocal(expr_id, lookup.pattern, lookup.resolved),
            .lookup_external => |resolved| try self.lowerLookupExternal(expr_id, resolved orelse invariant("an external lookup had no resolved reference")),
            .block => |block| {
                var index = block.statements.len;
                try self.push(.{ .expr = block.final_expr });
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .stmt = block.statements[index] });
                }
            },
            .call => |call| try self.lowerCall(expr_id, call),
            .dispatch_call => |plan| try self.lowerDispatch(expr_id, plan orelse invariant("a dispatch call had no checked plan")),
            .method_eq => |plan| try self.lowerDispatch(expr_id, plan orelse invariant("a method equality had no checked plan")),
            .type_dispatch_call => |plan| try self.lowerDispatch(expr_id, plan orelse invariant("a type dispatch call had no checked plan")),
            .run_low_level => |run_ll| {
                const arg_types = try self.program.arena.allocator().alloc(TypeId, run_ll.args.len);
                for (run_ll.args, arg_types) |arg, *arg_ty| arg_ty.* = try self.exprType(arg);
                try self.push(.{ .finish_low_level = .{ .op = run_ll.op, .arg_count = @intCast(run_ll.args.len), .result = try self.exprType(expr_id), .arg_types = arg_types } });
                try self.pushExprsReversed(run_ll.args);
            },
            .empty_record => try self.pushValue(try self.unitValue()),
            .record => |record| {
                if (record.ext != null) invariant("record update is not yet lowered by Boxy");
                if (record.unsets.len != 0) invariant("unset record fields are not yet lowered by Boxy");
                try self.push(.{ .finish_record = expr_id });
                var index = record.fields.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .expr = record.fields[index].value });
                }
            },
            .tuple => |items| {
                try self.push(.{ .finish_tuple = expr_id });
                try self.pushExprsReversed(items);
            },
            .tag => |tag| {
                try self.push(.{ .finish_tag = expr_id });
                try self.pushExprsReversed(tag.args);
            },
            .zero_argument_tag => try self.finishTag(expr_id, null),
            .list => |items| {
                try self.push(.{ .finish_list = expr_id });
                try self.pushExprsReversed(items);
            },
            .empty_list => try self.finishList(expr_id),
            .nominal => |nominal| {
                // A tag constructing a nominal is built at the nominal's
                // declared backing row (design.md "Nominal Constructor
                // Backing Relation"), not at the tag's own narrower row.
                const backing = self.bodies().expr(nominal.backing_expr);
                switch (backing.data) {
                    .tag => |tag| {
                        try self.push(.{ .finish_tag_at = .{ .expr = nominal.backing_expr, .ty = try self.exprType(expr_id) } });
                        try self.pushExprsReversed(tag.args);
                    },
                    .zero_argument_tag => try self.push(.{ .finish_tag_at = .{ .expr = nominal.backing_expr, .ty = try self.exprType(expr_id) } }),
                    else => try self.push(.{ .expr = nominal.backing_expr }),
                }
            },
            .lambda => try self.lowerClosure(expr_id, expr_id, &.{}),
            .closure => |closure| try self.lowerClosure(expr_id, closure.lambda, closure.captures),
            .row_coerce => |coerce| try self.push(.{ .expr = coerce.value }),
            .structural_eq => |eq| {
                if (eq.discriminant) |discriminant| {
                    try self.push(.{ .finish_discriminant_eq = .{ .value_ty = try self.exprType(discriminant.value), .tag = discriminant.tag, .negated = eq.negated } });
                    try self.push(.{ .expr = discriminant.value });
                    return;
                }
                try self.push(.{ .finish_eq = .{ .negated = eq.negated, .operand = try self.exprType(eq.lhs) } });
                try self.push(.{ .expr = eq.rhs });
                try self.push(.{ .expr = eq.lhs });
            },
            .expect => |cond| {
                try self.push(.{ .finish_expect = expr_id });
                try self.push(.{ .expr = cond });
            },
            .crash => |literal| try self.crashValue(self.bodies().stringLiteral(literal), try self.exprType(expr_id)),
            .runtime_error => |runtime_error| {
                if (runtime_error.evaluated.len != 0) invariant("runtime errors with evaluated operands are not yet lowered by Boxy");
                const literal = try self.store.insertString("runtime error");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                try self.pushValue(try self.localOf(try self.exprType(expr_id), .frame));
            },
            .if_ => |if_| {
                const result_ty = try self.exprType(expr_id);
                const result = try self.localOf(result_ty, .frame);
                const join = self.freshJoin();
                const merge = try self.beginMerge(if (if_.state_merge) |plan| self.bodies().stateMergeBinders(plan) else &.{});
                const after = try self.declareMergeJoin(join, result, merge);
                try self.push(.{ .push_value = result });
                if (merge) |m| try self.push(.{ .enter_merge = m });
                try self.push(.{ .resume_at = after });
                try self.push(.{ .if_test = .{ .expr = expr_id, .index = 0, .join = join, .merge = merge } });
                try self.push(.{ .expr = if_.branches[0].cond });
            },
            .match_ => |match| {
                try self.push(.{ .match_start = expr_id });
                try self.push(.{ .expr = match.cond });
            },
            .binop => |binop| switch (binop.op) {
                .@"and", .@"or" => try self.lowerShortCircuit(expr_id, binop.op == .@"and", binop.lhs, binop.rhs),
                else => invariantFmt("binary operator {s} reached Boxy lowering outside a dispatch", .{@tagName(binop.op)}),
            },
            .for_ => |for_| try self.pushForLoop(for_.pattern, for_.expr, for_.body, for_.plan, for_.mutations),
            .field_access => |access| {
                try self.push(.{ .finish_field_access = expr_id });
                try self.push(.{ .expr = access.receiver });
            },
            .tuple_access => |access| {
                try self.push(.{ .finish_tuple_access = expr_id });
                try self.push(.{ .expr = access.tuple });
            },
            .dbg => |inner| {
                try self.push(.{ .finish_dbg = try self.exprType(inner) });
                try self.push(.{ .expr = inner });
            },
            .str_from_quote => |quote| {
                if (quote.conversion_root) |root| return try self.restoreConversionRoot(root, try self.exprType(expr_id));
                if (quote.plan) |plan| return try self.lowerLiteralConversion(expr_id, plan, .quote);
                try self.pushValue(try self.strLiteral(self.bodies().stringLiteral(quote.literal)));
            },
            .return_ => |ret| {
                // The expression itself never produces a value; its successor
                // is unreachable.
                try self.push(.{ .unreachable_value = try self.exprType(expr_id) });
                try self.push(.ret);
                try self.push(.{ .expr = ret.expr });
            },
            .unary_not => |operand| {
                try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = try self.exprType(expr_id) } });
                try self.push(.{ .expr = operand });
            },
            else => invariantFmt("checked expression {s} is not yet lowered by Boxy", .{@tagName(expr.data)}),
        }
    }

    fn pushExprsReversed(self: *Builder, exprs: []const CheckedExprId) Allocator.Error!void {
        var index = exprs.len;
        while (index > 0) {
            index -= 1;
            try self.push(.{ .expr = exprs[index] });
        }
    }

    fn strLiteral(self: *Builder, text: []const u8) Allocator.Error!LocalId {
        const local = try self.closedLocal(.str);
        const backing = try self.store.insertString(text);
        try self.emitLinear(.{ .assign_literal = .{
            .target = local,
            .value = .{ .str_literal = .{ .backing = backing, .offset = 0, .len = @intCast(text.len) } },
            .next = undefined,
        } });
        return local;
    }

    fn finishStr(self: *Builder, count: u32) Allocator.Error!void {
        const parts = try self.popValues(count);
        defer self.allocator.free(parts);
        if (parts.len == 0) return try self.pushValue(try self.strLiteral(""));
        var acc = parts[0];
        for (parts[1..]) |part| {
            const next = try self.closedLocal(.str);
            try self.emitLowLevel(next, .str_concat, &.{ acc, part });
            acc = next;
        }
        try self.pushValue(acc);
    }

    fn lowerNumeral(self: *Builder, expr_id: CheckedExprId, numeral: checked.CheckedNumeralData) Allocator.Error!void {
        if (numeral.conversion_root) |root| return try self.restoreConversionRoot(root, try self.exprType(expr_id));
        if (numeral.plan) |plan| return try self.lowerLiteralConversion(expr_id, plan, .numeral);
        const ty = try self.exprType(expr_id);
        const primitive = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive,
            else => invariant("a builtin numeral's type is not a primitive"),
        };
        const exact = self.view.view.module_env.exactNumeral(numeral.literal);
        const exact_numeral = checked.exact_numeral;
        const local = try self.closedLocal(@import("layouts.zig").primitiveLayout(primitive));
        const value: LIR.LiteralValue = switch (primitive) {
            .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .u128, .i128 => blk: {
                const target: exact_numeral.Target = switch (primitive) {
                    inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| @field(exact_numeral.Target, @tagName(p)),
                    else => unreachable, // the enclosing prong admits only integers
                };
                const bits = exact_numeral.intBits(exact, target) orelse return try self.crashValue("invalid numeric literal", ty);
                const wide: i128 = switch (bits) {
                    .i128 => |v| v,
                    .u128 => |v| @bitCast(v),
                };
                break :blk switch (primitive) {
                    .u128, .i128 => .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = wide, .layout_idx = @import("layouts.zig").primitiveLayout(primitive) }) },
                    else => .{ .i64_literal = .{ .value = @truncate(wide), .layout_idx = @import("layouts.zig").primitiveLayout(primitive) } },
                };
            },
            .f32 => .{ .f32_literal = try exact_numeral.floatBits(f32, self.allocator, exact) },
            .f64 => .{ .f64_literal = try exact_numeral.floatBits(f64, self.allocator, exact) },
            .dec => .{ .dec_literal = (try exact_numeral.decBits(self.allocator, exact)) orelse return try self.crashValue("invalid numeric literal", ty) },
            else => return try self.crashValue("invalid numeric literal", ty),
        };
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = value, .next = undefined } });
        try self.pushValue(local);
    }

    /// A literal whose conversion a compile-time root evaluated: the root's
    /// stored value (design.md "Runtime Programs Read Stored Values").
    fn restoreConversionRoot(self: *Builder, root_id: checked.ComptimeRootId, ty: TypeId) Allocator.Error!void {
        switch (self.view.view.compile_time_roots.root(root_id).payload) {
            .const_node => |node| try self.push(.{ .restore = .{ .module = self.module, .node = node, .ty = ty } }),
            else => |payload| invariantFmt("a literal conversion root holding {s} reached Boxy lowering", .{@tagName(payload)}),
        }
    }

    /// A literal converted at runtime by its checked `from_numeral` or
    /// `from_quote` dispatch: the conversion's `Ok` payload, or a literal
    /// rejection carrying its error message.
    fn lowerLiteralConversion(self: *Builder, expr_id: CheckedExprId, plan_id: static_dispatch.StaticDispatchPlanId, kind: LIR.LiteralRejectionKind) Allocator.Error!void {
        const plan = self.dispatchPlan(plan_id);
        const try_ty = switch (self.program.store.get(try self.convert(plan.callable_ty))) {
            .function => |function| function.ret,
            else => invariant("a literal conversion's callable type is not a function type"),
        };
        try self.push(.{ .unwrap_literal = .{ .expr = expr_id, .try_ty = try_ty, .kind = kind } });
        try self.lowerDispatchAt(plan_id, try_ty);
    }

    fn unwrapLiteral(self: *Builder, expr_id: CheckedExprId, try_ty: TypeId, kind: LIR.LiteralRejectionKind) Allocator.Error!void {
        const value = self.popValue();
        const store = &self.program.store;
        const try_union = try self.structuralType(try_ty);
        const ok = try store.internName("Ok");
        const err = try store.internName("Err");
        const err_hole = try self.newHole();
        try self.branchOnTag(value, try_union, ok, err_hole);
        const payload = try self.tagPayloadValue(value, try_union, ok, 0);
        const ok_hole = self.hole;

        // Every tag of the conversion's error row carries its message.
        self.hole = err_hole;
        const errors = try self.tagPayloadValue(value, try_union, err, 0);
        const error_union = try self.structuralType(errors.ty);
        const tags = switch (store.get(error_union)) {
            .tag_union => |union_ty| if (union_ty.ext == null) store.tagSpan(union_ty.tags) else invariant("a literal conversion's error row is open"),
            else => invariant("a literal conversion's error is not a tag union"),
        };
        const names = try self.allocator.alloc(types.NameId, tags.len);
        defer self.allocator.free(names);
        for (tags, names) |tag, *name| {
            if (tag.payloads.len != 1) invariant("a literal conversion error tag does not carry one message");
            name.* = tag.name;
        }
        const site: LIR.LiteralRejectionSite = .{
            .owner = self.program.lowering_module_ids[@backingInt(self.module)],
            .checked_expr = @backingInt(expr_id),
            .kind = kind,
        };
        for (names, 0..) |name, index| {
            const miss = if (index + 1 < names.len) try self.newHole() else null;
            if (miss) |hole| try self.branchOnTag(errors.value, error_union, name, hole);
            const message = try self.tagPayloadValue(errors.value, error_union, name, 0);
            try self.emitTerminal(.{ .crash = .{
                .msg = .{ .local = message.value },
                .extra = try self.store.addStmtRow(LIR.CrashExtra, .{ .literal_rejection = site }),
            } });
            if (miss) |hole| self.hole = hole;
        }
        self.hole = ok_hole;
        try self.pushValue(payload.value);
    }

    /// The `Numeral` value `from_numeral` receives for `literal`, at the
    /// argument type `ty`: `Literal({ is_negative, digits_before_pt,
    /// digits_after_pt, digits_after_pt_count })`.
    fn numeralArgument(self: *Builder, literal: NumeralLiteral, ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const backing = try self.structuralType(ty);
        const name = try store.internName("Literal");
        const variant = self.closedVariant(backing, name);
        if (variant.payloads.len != 1) invariant("the Numeral Literal tag does not carry one record");
        const record_ty = try self.structuralType(variant.payloads[0]);
        const fields = switch (store.get(record_ty)) {
            .record => |record| try self.allocator.dupe(types.Field, store.fieldSpan(record.fields)),
            else => invariant("the Numeral Literal payload is not a record"),
        };
        defer self.allocator.free(fields);
        const env = self.view.view.module_env;
        const values = try self.allocator.alloc(LocalId, fields.len);
        defer self.allocator.free(values);
        for (fields, values) |field, *value| {
            const label = store.nameText(field.name);
            value.* = if (std.mem.eql(u8, label, "is_negative"))
                try self.boolValue(literal.isNegative())
            else if (std.mem.eql(u8, label, "digits_before_pt"))
                try self.byteList(env.numeralDigitsBefore(literal), field.ty)
            else if (std.mem.eql(u8, label, "digits_after_pt"))
                try self.byteList(env.numeralDigitsAfter(literal), field.ty)
            else if (std.mem.eql(u8, label, "digits_after_pt_count"))
                try self.intValue(literal.after_decimal_digit_count, field.ty)
            else
                invariant("the Numeral record has an unexpected field");
        }
        const record = try self.buildStruct(record_ty, values);
        return try self.buildTag(backing, name, &.{record});
    }

    /// A `List(U8)` (at `ty`) of `bytes`.
    fn byteList(self: *Builder, bytes: []const u8, ty: TypeId) Allocator.Error!LocalId {
        const elems = try self.allocator.alloc(LocalId, bytes.len);
        defer self.allocator.free(elems);
        for (bytes, elems) |byte, *elem| {
            elem.* = try self.closedLocal(.u8);
            try self.emitLinear(.{ .assign_literal = .{ .target = elem.*, .value = .{ .i64_literal = .{ .value = byte, .layout_idx = .u8 } }, .next = undefined } });
        }
        const list = try self.localOf(ty, .frame);
        try self.emitLinear(.{ .assign_list = .{ .target = list, .elems = try self.store.addLocalSpan(elems), .next = undefined } });
        return list;
    }

    /// An integer of the builtin integer type `ty`.
    fn intValue(self: *Builder, value: u64, ty: TypeId) Allocator.Error!LocalId {
        const primitive = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive,
            else => invariant("an integer field's type is not a primitive"),
        };
        const prim_layout = @import("layouts.zig").primitiveLayout(primitive);
        const local = try self.closedLocal(prim_layout);
        const literal: LIR.LiteralValue = switch (primitive) {
            .u128, .i128 => .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = value, .layout_idx = prim_layout }) },
            else => .{ .i64_literal = .{ .value = @bitCast(value), .layout_idx = prim_layout } },
        };
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = literal, .next = undefined } });
        return local;
    }

    fn resolvedRef(self: *const Builder, id: checked.ResolvedValueRefId) checked.ResolvedValueRefRecord {
        return self.view.view.resolved_value_refs.records[@backingInt(id)];
    }

    fn lowerLookupLocal(self: *Builder, expr_id: CheckedExprId, pattern: CheckedPatternId, resolved: ?checked.ResolvedValueRefId) Allocator.Error!void {
        if (resolved) |id| switch (self.resolvedRef(id).ref) {
            .local_param, .local_value, .pattern_binder, .local_mutable_version => |binding| {
                return try self.pushValue(self.binders.get(binding.binder) orelse invariant("a local lookup named a binder this body never bound"));
            },
            .top_level_const, .imported_const => |use| return try self.restoreConstUse(use, try self.exprType(expr_id)),
            else => |ref| invariantFmt("local lookup of a {s} is not yet lowered by Boxy", .{@tagName(ref)}),
        };
        const binder = self.bodies().pattern_binder_by_pattern[@backingInt(pattern)].get() orelse invariant("a local lookup named a pattern without a binder");
        try self.pushValue(self.binders.get(binder) orelse invariant("a local lookup named a binder this body never bound"));
    }

    fn lowerLookupExternal(self: *Builder, expr_id: CheckedExprId, resolved: checked.ResolvedValueRefId) Allocator.Error!void {
        switch (self.resolvedRef(resolved).ref) {
            .top_level_const, .imported_const => |use| return try self.restoreConstUse(use, try self.exprType(expr_id)),
            else => {},
        }
        invariantFmt("external value lookups ({s}) are not yet lowered by Boxy outside calls", .{@tagName(self.resolvedRef(resolved).ref)});
    }

    /// A read of a stored constant: its value's construction at `ty`
    /// (design.md "Compile-Time Evaluation Strategy").
    fn restoreConstUse(self: *Builder, use: checked.ConstUseTemplate, ty: TypeId) Allocator.Error!void {
        const owner = self.program.moduleByKey(use.const_ref.artifact);
        const template = owner.view.const_templates.get(use.const_ref);
        switch (template.state) {
            .stored_const => |stored| try self.push(.{ .restore = .{ .module = owner.index, .node = stored.node, .ty = ty } }),
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                try self.crashValue(@import("../common.zig").unimplemented_declaration_crash, ty);
            },
            .reserved, .eval_template => invariant("a constant read at runtime was not stored by compile-time evaluation"),
        }
    }

    /// Restore stored constant `node` of `module` at `ty`, pushing tasks for
    /// its components first.
    fn restoreStep(self: *Builder, module_index: ModuleIndex, node: checked.ConstNodeId, raw_ty: TypeId) Allocator.Error!void {
        const const_store = self.program.module(module_index).view.const_store;
        const store = &self.program.store;
        if (!store.isClosed(raw_ty)) invariant("restoring a constant at an open type is not yet lowered by Boxy");
        const ty = try self.structuralType(raw_ty);
        switch (const_store.get(node)) {
            .pending => invariant("a stored constant was never filled"),
            .zst => {
                const local = try self.localOf(raw_ty, .frame);
                try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                try self.pushValue(local);
            },
            .scalar => |scalar| {
                const local = try self.localOf(raw_ty, .frame);
                const value: LIR.LiteralValue = switch (scalar) {
                    inline .i8, .i16, .i32, .i64, .u8, .u16, .u32 => |v| .{ .i64_literal = .{ .value = v, .layout_idx = self.store.getLocal(local).layout_idx } },
                    .u64 => |v| .{ .i64_literal = .{ .value = @bitCast(v), .layout_idx = self.store.getLocal(local).layout_idx } },
                    .i128 => |v| .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = v, .layout_idx = self.store.getLocal(local).layout_idx }) },
                    .u128 => |v| .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = @bitCast(v), .layout_idx = self.store.getLocal(local).layout_idx }) },
                    .f32_bits => |bits| .{ .f32_literal = @bitCast(bits) },
                    .f64_bits => |bits| .{ .f64_literal = @bitCast(bits) },
                    .dec_bits => |bits| .{ .dec_literal = bits },
                };
                try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = value, .next = undefined } });
                try self.pushValue(local);
            },
            .str => |str| try self.pushValue(try self.strLiteral(const_store.strBytes(str))),
            .crash => |message| {
                try self.crashValue(const_store.strBytes(message), raw_ty);
            },
            .checked_error => |message| {
                const literal = try self.store.insertString(const_store.strBytes(message));
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                try self.pushValue(try self.localOf(raw_ty, .frame));
            },
            .nominal => |nominal| try self.push(.{ .restore = .{ .module = module_index, .node = nominal.backing, .ty = ty } }),
            .box => |child| {
                const payload = switch (store.get(ty)) {
                    .box => |payload| payload,
                    else => invariant("a stored box restored at a non-Box type"),
                };
                try self.push(.{ .finish_low_level = .{ .op = .box_box, .arg_count = 1, .result = raw_ty } });
                try self.push(.{ .restore = .{ .module = module_index, .node = child, .ty = payload } });
            },
            .tuple, .record => |children| {
                const component_types: []const TypeId = switch (store.get(ty)) {
                    .tuple => |items| store.span(items),
                    .record => |record| blk: {
                        const fields = store.fieldSpan(record.fields);
                        const storage = try self.program.arena.allocator().alloc(TypeId, fields.len);
                        for (fields, storage) |field, *slot| {
                            if (field.kind != .inline_slot) invariant("restoring optional record fields is not yet lowered by Boxy");
                            slot.* = field.ty;
                        }
                        break :blk storage;
                    },
                    .empty_record => &.{},
                    else => invariant("a stored product restored at a non-product type"),
                };
                if (component_types.len != children.len) invariant("a stored product's arity differs from its type");
                try self.push(.{ .finish_restore_struct = .{ .ty = raw_ty, .count = @intCast(children.len) } });
                var index = children.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .module = module_index, .node = children[index], .ty = component_types[index] } });
                }
            },
            .tag => |tag| {
                const union_ty = switch (store.get(self.program.joins.representative(ty))) {
                    .tag_union => |union_ty| union_ty,
                    else => invariant("a stored tag restored at a non-tag-union type"),
                };
                const name = try store.internName(tag.tag_name);
                const tags = store.tagSpan(union_ty.tags);
                const variant = for (tags, 0..) |candidate, index| {
                    if (candidate.name == name and candidate.payloads.len == tag.payloads.len) break index;
                } else invariant("a stored tag is absent from its type");
                const payload_types = store.span(tags[variant].payloads);
                try self.push(.{ .finish_restore_tag = .{ .ty = raw_ty, .name = name, .count = @intCast(tag.payloads.len) } });
                var index = tag.payloads.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .module = module_index, .node = tag.payloads[index], .ty = payload_types[index] } });
                }
            },
            .list => |list| {
                const elem = switch (store.get(ty)) {
                    .list => |elem| elem,
                    else => invariant("a stored list restored at a non-List type"),
                };
                switch (list) {
                    .nodes => |nodes| {
                        try self.push(.{ .finish_restore_list = .{ .ty = raw_ty, .count = @intCast(nodes.len) } });
                        var index = nodes.len;
                        while (index > 0) {
                            index -= 1;
                            try self.push(.{ .restore = .{ .module = module_index, .node = nodes[index], .ty = elem } });
                        }
                    },
                    .empty => try self.finishRestoreList(raw_ty, 0),
                    .packed_bytes => |packed_list| {
                        if (packed_list.element == null) invariant("restoring packed product lists is not yet lowered by Boxy");
                        const local = try self.localOf(raw_ty, .frame);
                        const bytes = const_store.blobBytes(packed_list.bytes);
                        const backing = try self.store.insertStringAligned(bytes, 16);
                        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .bytes_literal = .{
                            .bytes = .{ .backing = backing, .offset = 0, .len = @intCast(bytes.len) },
                            .len = packed_list.len,
                        } }, .next = undefined } });
                        try self.pushValue(local);
                    },
                }
            },
            .fn_value => |fn_id| try self.restoreFn(module_index, fn_id, raw_ty),
        }
    }

    /// Restore stored function value `fn_id` of `module`'s const store as
    /// an erased callable.
    /// Restore a function value compile-time evaluation stored, used at the
    /// closed function type `fn_ty`.
    fn restoreFn(self: *Builder, module_index: ModuleIndex, fn_id: check.ConstStore.ConstFnId, fn_ty: TypeId) Allocator.Error!void {
        const const_store = self.program.module(module_index).view.const_store;
        const stored = const_store.getFn(fn_id);
        switch (stored.fn_def) {
            .local_template, .imported_template => |ref| {
                const target = try self.program.templateRefWorker(ref);
                const target_worker = self.program.worker(target).*;
                if (target_worker.dictionary_params.len != 0) invariant("restoring stored procedure values that take dictionaries is not yet lowered by Boxy");
                const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                const bound = try self.relateVariables(formal, fn_ty, target_worker.scheme_vars.len);
                defer self.allocator.free(bound);
                const descs = try self.allocator.alloc(LocalId, bound.len);
                defer self.allocator.free(descs);
                for (bound, descs) |ty, *desc| desc.* = try self.descOf(ty);
                const entry = try self.program.methodEntryWorker(target);
                try self.pushValue(try self.composeDictionary(entry, descs));
            },
            .nested => |nested| {
                if (nested.default_root != null) invariant("restoring default-root closures is not yet lowered by Boxy");
                const owner_worker = try self.program.templateRefScope(nested.owner);
                const owner = self.program.worker(owner_worker).*;
                if (owner.dictionary_params.len != 0) invariant("restoring closures of procedures that take dictionaries is not yet lowered by Boxy");
                const owner_view = self.program.module(owner.module);
                const site = owner_view.view.nested_proc_sites.sites[@backingInt(nested.site)];
                const site_expr = site.checked_expr orelse invariant("a stored closure's site has no checked expression");
                const expr = owner_view.bodies().expr(site_expr);
                const lambda_expr, const captures: []const checked.CheckedCapture = switch (expr.data) {
                    .lambda => .{ site_expr, &.{} },
                    .closure => |closure| .{ closure.lambda, closure.captures },
                    else => invariant("a stored closure's site is not a lambda or closure"),
                };
                // The closure in its owner's scope, whose variables the
                // stored value's type instantiates.
                const variables = try self.allocator.alloc(TypeId, owner.scheme_vars.len);
                defer self.allocator.free(variables);
                for (variables, 0..) |*variable, index| variable.* = try self.program.store.variable(@intCast(index));
                const generic: types.Env = .{ .module = owner.module, .vars = owner.scheme_vars, .bound = variables };
                const generic_fn = try self.program.converter().convert(owner.module, expr.ty, generic);
                const bound = try self.relateVariables(generic_fn, fn_ty, owner.scheme_vars.len);
                defer self.allocator.free(bound);
                const concrete: types.Env = .{ .module = owner.module, .vars = owner.scheme_vars, .bound = bound };
                const capture_types = try self.allocator.alloc(TypeId, captures.len);
                defer self.allocator.free(capture_types);
                const concrete_capture_types = try self.allocator.alloc(TypeId, captures.len);
                defer self.allocator.free(concrete_capture_types);
                for (captures, capture_types, concrete_capture_types) |capture, *capture_ty, *concrete_ty| {
                    const checked_ty = owner_view.bodies().pattern(capture.pattern).ty;
                    capture_ty.* = try self.program.converter().convert(owner.module, checked_ty, generic);
                    concrete_ty.* = try self.program.converter().convert(owner.module, checked_ty, concrete);
                }
                const worker = try self.program.lambdaWorker(owner_worker, lambda_expr, captures, capture_types, generic_fn);
                const descs = try self.program.arena.allocator().alloc(LocalId, bound.len);
                for (bound, descs) |ty, *desc| desc.* = try self.descOf(ty);
                try self.push(.{ .finish_restore_closure = .{ .worker = worker, .count = @intCast(captures.len), .descs = descs } });
                var index = captures.len;
                while (index > 0) {
                    index -= 1;
                    const stored_capture = for (stored.captures) |candidate| {
                        if (std.meta.eql(candidate.id, captures[index].capture_id)) break candidate;
                    } else invariant("a stored closure lacks one of its site's captures");
                    try self.push(.{ .restore = .{ .module = module_index, .node = stored_capture.value, .ty = concrete_capture_types[index] } });
                }
            },
            else => invariantFmt("restoring stored {s} function values is not yet lowered by Boxy", .{@tagName(stored.fn_def)}),
        }
    }

    fn finishRestoreList(self: *Builder, ty: TypeId, count: u32) Allocator.Error!void {
        const values = try self.popValues(count);
        defer self.allocator.free(values);
        const target = try self.localOf(ty, .frame);
        try self.emitLinear(.{ .assign_list = .{ .target = target, .elems = try self.store.addLocalSpan(values), .next = undefined } });
        try self.pushValue(target);
    }

    /// The worker a procedure use names.
    /// What a procedure use calls: a template's worker, or a function value
    /// compile-time evaluation stored.
    const Callee = union(enum) {
        worker: WorkerId,
        callable_eval: struct { module: ModuleIndex, template: checked.CallableEvalTemplateId },
    };

    fn procedureUseCallee(self: *Builder, use: checked.ProcedureUseTemplate) Allocator.Error!Callee {
        var current = use;
        while (true) {
            switch (current.binding) {
                .top_level => |top| {
                    const owner = self.program.moduleByKey(top.artifact);
                    const binding = owner.view.top_level_procedure_bindings.bindings.items[@backingInt(top.binding)];
                    switch (binding.body) {
                        .callable_eval_template => |template_id| {
                            const template = owner.view.callable_eval_templates.templates[@backingInt(template_id)];
                            const forwarded = template.forwarded_lookup orelse
                                return .{ .callable_eval = .{ .module = owner.index, .template = template_id } };
                            // An exact procedure alias calls what it names.
                            const ref_id = owner.view.resolved_value_refs.by_checked_expr[@backingInt(forwarded)].get() orelse
                                invariant("a procedure alias's lookup has no resolved reference");
                            current = switch (owner.view.resolved_value_refs.records[@backingInt(ref_id)].ref) {
                                .top_level_proc, .imported_proc, .promoted_top_level_proc => |next| next,
                                else => |ref| invariantFmt("procedure aliases of a {s} are not yet lowered by Boxy", .{@tagName(ref)}),
                            };
                            continue;
                        },
                        else => {},
                    }
                },
                else => {},
            }
            return .{ .worker = try self.procedureUseWorker(current) };
        }
    }

    fn procedureUseWorker(self: *Builder, use: checked.ProcedureUseTemplate) Allocator.Error!WorkerId {
        return switch (use.binding) {
            .top_level => |top| blk: {
                const owner = self.program.moduleByKey(top.artifact);
                const binding = owner.view.top_level_procedure_bindings.bindings.items[@backingInt(top.binding)];
                break :blk switch (binding.body) {
                    .direct_template => |direct| switch (direct.template) {
                        .checked => |ref| try self.program.templateRefWorker(ref),
                        .lifted, .synthetic => invariant("lifted and synthetic procedure templates are not yet lowered by Boxy"),
                    },
                    .callable_eval_template => invariant("callable values evaluated at compile time are not yet lowered by Boxy"),
                    .checked_error => invariant("a call reached a rejected procedure binding"),
                };
            },
            .imported => |imported| blk: {
                const owner = self.program.moduleByKey(imported.artifact);
                const exported = owner.view.exported_procedure_bindings;
                _ = exported;
                const template = owner.view.checked_procedure_templates.lookupByDef(imported.def) orelse
                    invariant("an imported procedure binding has no template");
                break :blk try self.program.templateRefWorker(template);
            },
            .hosted => invariant("hosted calls are not yet lowered by Boxy"),
            .platform_required => invariant("platform-required procedures are not yet lowered by Boxy"),
        };
    }

    fn lowerCall(self: *Builder, expr_id: CheckedExprId, call: @FieldType(checked.CheckedExprData, "call")) Allocator.Error!void {
        const func = self.bodies().expr(call.func);
        const resolved: ?checked.ResolvedValueRefId = switch (func.data) {
            .lookup_external => |r| r orelse invariant("a call's external callee was unresolved"),
            .lookup_local => |lookup| lookup.resolved,
            else => null,
        };
        const callee: WorkerId = if (resolved) |id| switch (self.resolvedRef(id).ref) {
            .top_level_proc, .imported_proc, .promoted_top_level_proc => |use| switch (try self.procedureUseCallee(use)) {
                .worker => |worker_id| worker_id,
                .callable_eval => |eval| {
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = try self.exprType(call.func), .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
                    try self.pushExprsReversed(call.args);
                    try self.push(.{ .restore_callable_eval = .{ .module = eval.module, .template = eval.template, .ty = try self.exprType(call.func) } });
                    return;
                },
            },
            .local_param, .local_value, .pattern_binder, .local_mutable_version => return try self.lowerValueCall(expr_id, call),
            else => |ref| invariantFmt("calls through a {s} reference are not yet lowered by Boxy", .{@tagName(ref)}),
        } else return try self.lowerValueCall(expr_id, call);
        try self.push(.{ .finish_call = .{ .callee = callee, .hidden = try self.hiddenArgs(callee, self.siteSubstitution(call.func), self.view.view.static_dispatch_plans.siteEvidence(call.func) orelse &.{}), .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
        try self.pushExprsReversed(call.args);
    }

    /// A call of a function value: evaluate the function, then its
    /// arguments, then call through the erased-callable ABI.
    fn lowerValueCall(self: *Builder, expr_id: CheckedExprId, call: @FieldType(checked.CheckedExprData, "call")) Allocator.Error!void {
        try self.push(.{ .finish_erased_call = .{ .fn_ty = try self.exprType(call.func), .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
        try self.pushExprsReversed(call.args);
        try self.push(.{ .expr = call.func });
    }

    /// The hidden descriptor arguments of a call whose target's scheme
    /// variables `subst` instantiates (checked types of this body's module,
    /// in the target's `scheme_vars` order), followed by its dictionaries.
    fn hiddenArgs(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence) Allocator.Error![]LocalId {
        const target = self.program.worker(callee).*;
        if (subst.len != target.scheme_vars.len) invariant("a call's checked substitution differs from its target's scheme");
        const hidden = try self.allocator.alloc(LocalId, subst.len + target.dictionary_params.len);
        errdefer self.allocator.free(hidden);
        for (subst, hidden[0..subst.len]) |checked_ty, *desc| desc.* = try self.descOf(try self.convert(checked_ty));
        for (target.dictionary_params, hidden[subst.len..]) |param_index, *dict| {
            if (param_index >= evidence.len) invariant("a call's checked evidence does not cover its target's dictionaries");
            dict.* = try self.dictionaryOf(evidence[param_index]);
        }
        return hidden;
    }

    /// The dictionary `evidence` denotes (design.md "Dictionaries"),
    /// composed in the entry chain.
    fn dictionaryOf(self: *Builder, evidence: static_dispatch.CheckedEvidence) Allocator.Error!LocalId {
        const table = self.view.view.static_dispatch_plans;
        switch (evidence.resolution) {
            .constraint => |constraint| return self.scopeDictionary(constraint.index),
            .direct => |node_id| {
                const node = table.evidenceNode(node_id);
                if (node.instantiation == .alias) invariant("dictionaries of targets reached through a procedure alias are not yet lowered by Boxy");
                const target_module = self.program.moduleByIdx(node.target.module_idx);
                const template = target_module.view.checked_procedure_templates.lookupByDef(node.target.def_idx) orelse
                    invariant("a dictionary target has no procedure template");
                const target = try self.program.templateRefWorker(template);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                if (node.instantiation == .derived_from_callable) {
                    const hidden = try self.relatedHiddenArgs(target, node.instantiation.derived_from_callable);
                    defer self.allocator.free(hidden);
                    return try self.composeDictionary(try self.program.methodEntryWorker(target), hidden);
                }
                const nested: []const static_dispatch.CheckedEvidence = switch (node.nested) {
                    .resolved => table.nestedEvidence(node),
                    .from_callable => if (self.program.worker(target).dictionary_params.len == 0) &.{} else invariant("callable-derived dictionary evidence is not yet lowered by Boxy"),
                    .through_alias => invariant("alias-scope dictionary evidence is not yet lowered by Boxy"),
                };
                const hidden = try self.hiddenArgs(target, subst, nested);
                defer self.allocator.free(hidden);
                const entry = try self.program.methodEntryWorker(target);
                return try self.composeDictionary(entry, hidden);
            },
            .structural => |structural| {
                const method: lower.StructuralMethod = switch (structural.derivation) {
                    .equality => .is_eq,
                    .hash => .to_hash,
                    else => invariantFmt("structural {s} dictionaries are not yet lowered by Boxy", .{@tagName(structural.derivation)}),
                };
                const entry = try self.program.structuralWorker(self.module, method);
                const desc = try self.descOf(try self.convert(structural.dispatcher_ty));
                return try self.composeDictionary(entry, &.{desc});
            },
            else => invariantFmt("{s} dictionary evidence is not yet lowered by Boxy", .{@tagName(evidence.resolution)}),
        }
    }

    /// The hidden descriptors of a target the checker instantiated by
    /// relating its scheme to `callable` (an edge with no recorded
    /// substitution, `EvidenceTargetInstantiation.derived_from_callable`).
    fn relatedHiddenArgs(self: *Builder, callee: WorkerId, callable: checked.CheckedTypeId) Allocator.Error![]LocalId {
        const target = self.program.worker(callee).*;
        if (target.dictionary_params.len != 0) invariant("a callable-derived target with dictionaries is not yet lowered by Boxy");
        const target_fn = try self.program.store.intern(.{ .function = .{ .args = target.params, .ret = target.ret, .effectful = false } });
        const bound = try self.relateVariables(target_fn, try self.convert(callable), target.scheme_vars.len);
        defer self.allocator.free(bound);
        const hidden = try self.allocator.alloc(LocalId, bound.len);
        for (bound, hidden) |ty, *desc| desc.* = try self.descOf(ty);
        return hidden;
    }

    /// The types that variables `0..count` of `formal` take in `actual`: each
    /// variable takes the component of `actual` at the position the variable
    /// has in `formal`. An open row's remainder variable takes the actual
    /// row's entries the formal row does not name.
    fn relateVariables(self: *Builder, formal_root: TypeId, actual_root: TypeId, count: usize) Allocator.Error![]TypeId {
        const store = &self.program.store;
        const bound = try self.allocator.alloc(?TypeId, count);
        defer self.allocator.free(bound);
        @memset(bound, null);
        var pending: std.ArrayList([2]TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ formal_root, actual_root });
        while (pending.pop()) |pair| {
            const formal = store.get(pair[0]);
            if (formal == .variable) {
                bound[formal.variable] = pair[1];
                continue;
            }
            const other = store.get(pair[1]);
            if (std.meta.activeTag(formal) != std.meta.activeTag(other)) continue;
            switch (formal) {
                .list => |elem| try pending.append(self.allocator, .{ elem, other.list }),
                .box => |payload| try pending.append(self.allocator, .{ payload, other.box }),
                .tuple => |items| for (store.span(items), store.span(other.tuple)) |a, b| try pending.append(self.allocator, .{ a, b }),
                .function => |function| {
                    for (store.span(function.args), store.span(other.function.args)) |a, b| try pending.append(self.allocator, .{ a, b });
                    try pending.append(self.allocator, .{ function.ret, other.function.ret });
                },
                .nominal => |nominal| for (store.nominalArgs(nominal), store.nominalArgs(other.nominal)) |a, b| try pending.append(self.allocator, .{ a, b }),
                .record => |record| {
                    var residual: std.ArrayList(types.Field) = .empty;
                    defer residual.deinit(self.allocator);
                    for (store.fieldSpan(other.record.fields)) |candidate| {
                        var named = false;
                        for (store.fieldSpan(record.fields)) |field| {
                            if (candidate.name == field.name) {
                                named = true;
                                try pending.append(self.allocator, .{ field.ty, candidate.ty });
                            }
                        }
                        if (!named) try residual.append(self.allocator, candidate);
                    }
                    if (record.ext) |ext| {
                        const remainder = if (residual.items.len == 0 and other.record.ext == null)
                            try store.emptyRecord()
                        else
                            try store.intern(.{ .record = .{ .fields = residual.items, .ext = other.record.ext } });
                        try pending.append(self.allocator, .{ ext, remainder });
                    }
                },
                .tag_union => |tag_union| {
                    var residual: std.ArrayList(types.TagInput) = .empty;
                    defer residual.deinit(self.allocator);
                    for (store.tagSpan(other.tag_union.tags)) |candidate| {
                        var named = false;
                        for (store.tagSpan(tag_union.tags)) |tag| {
                            if (candidate.name == tag.name and candidate.payloads.len == tag.payloads.len) {
                                named = true;
                                for (store.span(tag.payloads), store.span(candidate.payloads)) |a, b| try pending.append(self.allocator, .{ a, b });
                            }
                        }
                        if (!named) try residual.append(self.allocator, .{ .name = candidate.name, .payloads = store.span(candidate.payloads) });
                    }
                    // An open formal row's remainder is the actual row's
                    // tags it does not name.
                    if (tag_union.ext) |ext| try pending.append(self.allocator, .{ ext, try store.intern(.{ .tag_union = .{ .tags = residual.items, .ext = other.tag_union.ext } }) });
                },
                .primitive, .empty_record, .variable => {},
            }
        }
        const result = try self.allocator.alloc(TypeId, count);
        for (bound, result) |ty, *slot| slot.* = ty orelse invariant("a related type leaves one of its variables unrelated");
        return result;
    }

    fn composeDictionary(self: *Builder, entry: WorkerId, captures: []const LocalId) Allocator.Error!LocalId {
        const local = try self.closedLocal(try self.erasedLayout());
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .dict_compose = .{
            .worker = self.program.worker(entry).proc,
            .captures = try self.store.addLocalSpan(captures),
        } }), .next = undefined } });
        return local;
    }

    /// The dictionary in scope for canonical evidence parameter `index`.
    fn scopeDictionary(self: *Builder, index: static_dispatch.EvidenceChainIndex) LocalId {
        if (index.depth != 0) invariant("evidence from enclosing generalized scopes is not yet lowered by Boxy");
        for (self.currentWorker().scope_dictionary_indices, 0..) |param_index, position| {
            if (param_index == index.index) return self.dict_locals[position];
        }
        invariant("evidence named a dictionary parameter its scope does not have");
    }

    fn siteSubstitution(self: *Builder, site: CheckedExprId) []const checked.CheckedTypeId {
        return self.view.view.static_dispatch_plans.siteSubstitution(site) orelse &.{};
    }

    fn finishCall(self: *Builder, callee: WorkerId, hidden: []const LocalId, arg_count: u32, result_ty: TypeId) Allocator.Error!void {
        const explicit = try self.popValues(arg_count);
        defer self.allocator.free(explicit);
        const args = try std.mem.concat(self.allocator, LocalId, &.{ explicit, hidden });
        defer self.allocator.free(args);
        const target = try self.localOf(result_ty, .frame);
        const proc = self.program.worker(callee).proc;
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = proc, .args = try self.store.addLocalSpan(args), .next = undefined } });
        try self.pushValue(target);
    }

    fn dispatchPlan(self: *const Builder, id: static_dispatch.StaticDispatchPlanId) static_dispatch.StaticDispatchCallPlan {
        return self.view.view.static_dispatch_plans.plans[@backingInt(id)];
    }

    fn lowerDispatch(self: *Builder, expr_id: CheckedExprId, plan_id: static_dispatch.StaticDispatchPlanId) Allocator.Error!void {
        try self.lowerDispatchAt(plan_id, try self.exprType(expr_id));
    }

    /// Push a dispatch operand: a checked expression, or a value the checker
    /// generated for a literal conversion.
    fn pushOperand(self: *Builder, operand: static_dispatch.StaticDispatchOperand, ty: TypeId) Allocator.Error!void {
        switch (operand) {
            .checked_expr => |expr| try self.push(.{ .expr = expr }),
            .generated_numeral => |literal| try self.push(.{ .numeral_arg = .{ .literal = literal, .ty = ty } }),
            .generated_quote => |literal| try self.push(.{ .push_value_str = literal }),
            .generated_interpolation_segments => invariant("interpolation segments are not yet lowered by Boxy"),
        }
    }

    fn operandTypes(self: *Builder, callable_ty: checked.CheckedTypeId) Allocator.Error![]const TypeId {
        return switch (self.program.store.get(try self.convert(callable_ty))) {
            .function => |function| self.program.store.span(function.args),
            else => invariant("a dispatch's callable type is not a function type"),
        };
    }

    fn lowerDispatchAt(self: *Builder, plan_id: static_dispatch.StaticDispatchPlanId, result_ty: TypeId) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const call_plan = self.dispatchPlan(plan_id);
        const operands = call_plan.argsSlice(table);
        const operand_types = try self.allocator.dupe(TypeId, try self.operandTypes(call_plan.callable_ty));
        defer self.allocator.free(operand_types);
        if (operand_types.len != operands.len) invariant("a dispatch's operand count differs from its callable type");
        switch (call_plan.resolution) {
            .direct_closed, .direct_parametric => |direct| {
                const node = table.evidenceNode(direct.evidence);
                const target_module = self.program.moduleByIdx(node.target.module_idx);
                const template = target_module.view.checked_procedure_templates.lookupByDef(node.target.def_idx) orelse
                    invariant("a dispatch target has no procedure template");
                const callee = try self.program.templateRefWorker(template);
                if (node.instantiation == .alias) invariant("dispatch targets reached through a procedure alias are not yet lowered by Boxy");
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                const nested: []const static_dispatch.CheckedEvidence = switch (node.nested) {
                    .resolved => table.nestedEvidence(node),
                    .from_callable => if (self.program.worker(callee).dictionary_params.len == 0) &.{} else invariant("callable-derived call evidence is not yet lowered by Boxy"),
                    .through_alias => invariant("alias-scope call evidence is not yet lowered by Boxy"),
                };
                const hidden = try self.hiddenArgs(callee, subst, nested);
                switch (call_plan.result_mode) {
                    .value => {},
                    .equality => |equality| if (equality.negated) {
                        try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                    },
                    else => invariantFmt("dispatch result mode {s} is not yet lowered by Boxy", .{@tagName(call_plan.result_mode)}),
                }
                try self.push(.{ .finish_call = .{ .callee = callee, .hidden = hidden, .arg_count = @intCast(operands.len), .result = if (call_plan.result_mode == .equality) try self.program.store.primitive(.bool) else result_ty } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.pushOperand(operands[index], operand_types[index]);
                }
            },
            .structural => |derivation| switch (derivation) {
                .equality => {
                    const negated = switch (call_plan.result_mode) {
                        .equality => |equality| equality.negated,
                        else => false,
                    };
                    if (operands.len != 2) invariant("structural equality has two operands");
                    const lhs = switch (operands[0]) {
                        .checked_expr => |operand| operand,
                        else => invariant("structural equality operand is not an expression"),
                    };
                    const rhs = switch (operands[1]) {
                        .checked_expr => |operand| operand,
                        else => invariant("structural equality operand is not an expression"),
                    };
                    try self.push(.{ .finish_eq = .{ .negated = negated, .operand = try self.exprType(lhs) } });
                    try self.push(.{ .expr = rhs });
                    try self.push(.{ .expr = lhs });
                },
                else => invariantFmt("structural {s} dispatch is not yet lowered by Boxy", .{@tagName(derivation)}),
            },
            .evidence_dependent => |dependent| {
                const dict = self.scopeDictionary(dependent.index);
                const fn_ty = try self.convert(call_plan.callable_ty);
                switch (call_plan.result_mode) {
                    .value => {},
                    .equality => |equality| if (equality.negated) {
                        try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                    },
                    else => invariantFmt("dispatch result mode {s} is not yet lowered by Boxy", .{@tagName(call_plan.result_mode)}),
                }
                const call_result = switch (self.program.store.get(fn_ty)) {
                    .function => |function| function.ret,
                    else => invariant("a dispatch's callable type is not a function type"),
                };
                try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = call_result } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.pushOperand(operands[index], operand_types[index]);
                }
                try self.pushValue(dict);
            },
            .checked_error => {
                const literal = try self.store.insertString("runtime error");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                try self.pushValue(try self.localOf(result_ty, .frame));
            },
            .@"unreachable" => try self.crashValue("unreachable dispatch", result_ty),
            else => invariantFmt("dispatch resolution {s} is not yet lowered by Boxy", .{@tagName(call_plan.resolution)}),
        }
    }

    /// Emit a low-level operation. An operation over values of open types
    /// is a list operation, which takes the element descriptor of its list
    /// as a trailing argument (`LowLevelExtra.dyn_elem_desc`), or a box
    /// operation, which lowers to the dynamic box statements.
    fn finishLowLevel(self: *Builder, op: LIR.LowLevel, arg_count: u32, result_ty: TypeId, arg_types: []const TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        const store = &self.program.store;
        var open = !store.isClosed(result_ty);
        for (arg_types) |arg_ty| open = open or !store.isClosed(arg_ty);
        if (!open) {
            const target = try self.localOf(result_ty, .frame);
            // Source arithmetic commits to its operand type's behavior.
            const committed = if (args.len != 0) CheckedArithmetic.lowerOp(op, self.store.getLocal(args[0]).layout_idx) else op;
            try self.emitLowLevel(target, committed, args);
            return try self.pushValue(target);
        }
        switch (op) {
            .box_box => {
                const target = try self.localOf(result_ty, .frame);
                try self.emitDyn(target, .{ .box = .{ .desc = try self.descOf(arg_types[0]), .payload = args[0] } });
                return try self.pushValue(target);
            },
            .box_unbox, .box_unbox_borrowed => {
                const view = try self.localOf(result_ty, .view);
                try self.emitDyn(view, .{ .box_payload_addr = .{ .box = args[0], .desc = try self.descOf(result_ty) } });
                const target = try self.localOf(result_ty, .frame);
                try self.emitDyn(target, .{ .copy = .{ .src = view, .desc = try self.descOf(result_ty) } });
                return try self.pushValue(target);
            },
            else => {},
        }
        if (!std.mem.startsWith(u8, @tagName(op), "list_")) {
            invariantFmt("low-level {s} over values of open types is not yet lowered by Boxy", .{@tagName(op)});
        }
        // The element type of the operation's list: its first List operand's,
        // else its List result's.
        const elem = for (arg_types) |arg_ty| {
            if (store.get(arg_ty) == .list) break store.get(arg_ty).list;
        } else switch (store.get(result_ty)) {
            .list => |elem| elem,
            else => invariantFmt("list low-level {s} has no List operand or result", .{@tagName(op)}),
        };
        const with_desc = try std.mem.concat(self.allocator, LocalId, &.{ args, &.{try self.descOf(elem)} });
        defer self.allocator.free(with_desc);
        const target = try self.localOf(result_ty, .frame);
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }),
            .args = try self.store.addLocalSpan(with_desc),
            .next = undefined,
        } });
        try self.pushValue(target);
    }

    /// A function value: an erased callable of the lambda's worker, whose
    /// captures hold the captured values and this worker's descriptors.
    fn lowerClosure(self: *Builder, expr_id: CheckedExprId, lambda_expr: CheckedExprId, captures: []const checked.CheckedCapture) Allocator.Error!void {
        const fn_ty = try self.exprType(expr_id);
        const capture_types = try self.allocator.alloc(TypeId, captures.len);
        defer self.allocator.free(capture_types);
        const values = try self.allocator.alloc(LocalId, captures.len + self.var_descs.len + self.dict_locals.len);
        defer self.allocator.free(values);
        for (captures, capture_types, 0..) |capture, *capture_ty, index| {
            capture_ty.* = try self.patternType(capture.pattern);
            const binder = self.bodies().pattern_binder_by_pattern[@backingInt(capture.pattern)].get() orelse
                invariant("a closure capture named a pattern without a binder");
            values[index] = self.binders.get(binder) orelse invariant("a closure captured a binder this body never bound");
        }
        const callee = try self.program.lambdaWorker(self.worker_id, lambda_expr, captures, capture_types, fn_ty);
        if (self.allClosed(capture_types)) {
            try self.pushValue(try self.packClosure(callee, values[0..captures.len], capture_types, self.var_descs, self.dict_locals));
        } else {
            try self.pushValue(try self.packDynamicClosure(callee, values[0..captures.len], capture_types, self.var_descs, self.dict_locals));
        }
    }

    /// The descriptor of a dynamic capture record (design.md "Function
    /// Values"): a declared-order record of its own descriptor word, the
    /// descriptor and dictionary words, then the captured values.
    fn dynamicCaptureDesc(self: *Builder, capture_types: []const TypeId, word_count: usize) Allocator.Error!LocalId {
        const word = try self.opaqueWordDesc();
        const descs = try self.allocator.alloc(LocalId, 1 + word_count + capture_types.len);
        defer self.allocator.free(descs);
        @memset(descs[0 .. 1 + word_count], word);
        for (descs[1 + word_count ..], capture_types) |*desc, ty| desc.* = try self.descOf(ty);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(descs) } }), .next = undefined } });
        return local;
    }

    fn opaqueWordDesc(self: *Builder) Allocator.Error!LocalId {
        if (self.opaque_word_desc) |local| return local;
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = try self.program.static_descs.opaqueWordSlot() } }, .next = undefined } });
        self.opaque_word_desc = local;
        return local;
    }

    /// A function value whose captures include values of open types: the
    /// capture record is built in the dynamic frame region and moved into
    /// the callable.
    fn packDynamicClosure(self: *Builder, callee: WorkerId, captured: []const LocalId, capture_types: []const TypeId, descs: []const LocalId, dicts: []const LocalId) Allocator.Error!LocalId {
        const record_desc = try self.dynamicCaptureDesc(capture_types, descs.len + dicts.len);
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = record_desc, .storage = .frame } });
        self.has_dynamic_frame = true;
        const fields = try std.mem.concat(self.allocator, LocalId, &.{ &.{record_desc}, descs, dicts, captured });
        defer self.allocator.free(fields);
        try self.emitDyn(record, .{ .record = .{ .desc = record_desc, .fields = try self.store.addLocalSpan(fields) } });
        const target = try self.closedLocal(try self.erasedLayout());
        try self.emitDyn(target, .{ .closure = .{ .proc = self.program.worker(callee).proc, .capture = record } });
        return target;
    }

    /// The erased callable of lambda worker `callee` capturing `values`
    /// (of `capture_types`), then descriptors and dictionaries.
    fn packClosure(self: *Builder, callee: WorkerId, captured: []const LocalId, capture_types: []const TypeId, descs: []const LocalId, dicts: []const LocalId) Allocator.Error!LocalId {
        const values = try std.mem.concat(self.allocator, LocalId, &.{ captured, descs, dicts });
        defer self.allocator.free(values);
        const target = try self.closedLocal(try self.erasedLayout());
        var capture: ?LocalId = null;
        var capture_layout: ?layout.Idx = null;
        var on_drop: LIR.ErasedCallableOnDrop = .none;
        if (values.len != 0) {
            const record_layout = try self.captureLayout(capture_types, descs.len, dicts.len);
            const record = try self.closedLocal(record_layout);
            try self.emitLinear(.{ .assign_struct = .{ .target = record, .fields = try self.store.addLocalSpan(values), .next = undefined } });
            capture = record;
            capture_layout = record_layout;
            const layouts = &self.program.result.layouts;
            if (layouts.layoutContainsRefcounted(layouts.getLayout(record_layout))) {
                on_drop = .{ .rc_helper = .{ .op = .host_drop, .layout_idx = record_layout } };
            }
        }
        try self.emitLinear(.{ .assign_packed_erased_fn = .{
            .target = target,
            .data = try self.store.addStmtRow(LIR.PackedErasedFnData, .{
                .proc = self.program.worker(callee).proc,
                .capture = capture,
                .capture_layout = capture_layout,
                .on_drop = on_drop,
            }),
            .next = undefined,
        } });
        return target;
    }

    /// Pop a function value's arguments and the function value, and call it
    /// through the uniform erased-callable ABI.
    fn finishErasedCall(self: *Builder, fn_ty: TypeId, arg_count: u32, result_ty: TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        const closure = self.popValue();
        try self.pushValue(try self.emitErasedCall(closure, fn_ty, args, result_ty));
    }

    /// Call the erased callable `closure` of type `fn_ty` with `args`.
    fn emitErasedCall(self: *Builder, closure: LocalId, fn_ty: TypeId, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const params = switch (self.program.store.get(fn_ty)) {
            .function => |function| self.program.store.span(function.args),
            else => invariant("a called value's type is not a function type"),
        };
        var record: ?LocalId = null;
        if (args.len != 0) {
            const params_copy = try self.allocator.dupe(TypeId, params);
            defer self.allocator.free(params_copy);
            const local = try self.argsRecordLocal(params_copy, .frame);
            if (self.store.localDyn(local)) |dyn| {
                try self.emitDyn(local, .{ .record = .{ .desc = dyn.desc, .fields = try self.store.addLocalSpan(args) } });
            } else {
                try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = try self.store.addLocalSpan(args), .next = undefined } });
            }
            record = local;
        }
        const target = try self.localOf(result_ty, .frame);
        try self.emitDyn(target, .{ .call_erased = .{ .closure = closure, .args = record } });
        return target;
    }

    fn finishEq(self: *Builder, negated: bool, operand_ty: TypeId) Allocator.Error!void {
        const rhs = self.popValue();
        const lhs = self.popValue();
        const result = try self.closedLocal(.bool);
        try self.emitDyn(result, .{ .eq = .{ .lhs = lhs, .rhs = rhs, .desc = try self.descOf(operand_ty) } });
        if (!negated) return try self.pushValue(result);
        const flipped = try self.closedLocal(.bool);
        try self.emitLowLevel(flipped, .bool_not, &.{result});
        try self.pushValue(flipped);
    }

    /// `a and b` evaluates `b` only when `a` is true; `a or b` only when
    /// `a` is false.
    fn lowerShortCircuit(self: *Builder, expr_id: CheckedExprId, is_and: bool, lhs: CheckedExprId, rhs: CheckedExprId) Allocator.Error!void {
        _ = expr_id;
        const result = try self.closedLocal(.bool);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        try self.push(.{ .push_value = result });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .short_circuit = .{ .is_and = is_and, .rhs = rhs, .join = join } });
        try self.push(.{ .expr = lhs });
    }

    fn shortCircuit(self: *Builder, is_and: bool, rhs: CheckedExprId, join: LIR.JoinPointId) Allocator.Error!void {
        const holes = try self.emitBoolSwitch(self.popValue());
        // The side that decides the result alone jumps with the constant.
        self.hole = if (is_and) holes.otherwise else holes.then;
        try self.emitJump(join, try self.boolValue(!is_and));
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = rhs });
        self.hole = if (is_and) holes.then else holes.otherwise;
    }

    // -- Aggregates --

    fn requireClosed(self: *Builder, ty: TypeId, comptime what: []const u8) void {
        if (!self.program.store.isClosed(ty)) invariant("building a dynamic " ++ what ++ " is not yet lowered by Boxy");
    }

    fn finishRecord(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const record = self.bodies().expr(expr_id).data.record;
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(record.fields.len));
        defer self.allocator.free(values);
        const record_ty = switch (self.program.store.get(try self.structuralType(ty))) {
            .record => |value| value,
            .empty_record => return try self.pushValue(try self.unitValue()),
            else => invariant("a record expression's type is not a record"),
        };
        const fields = self.program.store.fieldSpan(record_ty.fields);
        if (fields.len != values.len) invariant("record construction omitting fields is not yet lowered by Boxy");
        // Values arrive in source order; store them by semantic index.
        const ordered = try self.allocator.alloc(LocalId, fields.len);
        defer self.allocator.free(ordered);
        const name_store = self.view.nameStore();
        for (record.fields, values) |field, value| {
            const name = try self.program.store.internName(name_store.recordFieldLabelText(field.label));
            const semantic = for (fields, 0..) |candidate, index| {
                if (candidate.name == name) break index;
            } else invariant("a record expression's field is absent from its type");
            if (fields[semantic].kind != .inline_slot) invariant("constructing optional record fields is not yet lowered by Boxy");
            ordered[semantic] = value;
        }
        try self.pushValue(try self.buildStruct(ty, ordered));
    }

    /// A record or tuple of `ty` from its fields in semantic order.
    fn buildStruct(self: *Builder, ty: TypeId, fields: []const LocalId) Allocator.Error!LocalId {
        const target = try self.localOf(ty, .frame);
        if (self.program.store.isClosed(ty)) {
            try self.emitLinear(.{ .assign_struct = .{ .target = target, .fields = try self.store.addLocalSpan(fields), .next = undefined } });
        } else {
            try self.emitDyn(target, .{ .record = .{ .desc = try self.descOf(ty), .fields = try self.store.addLocalSpan(fields) } });
        }
        return target;
    }

    fn finishTuple(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const items = self.bodies().expr(expr_id).data.tuple;
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(items.len));
        defer self.allocator.free(values);
        try self.pushValue(try self.buildStruct(ty, values));
    }

    fn finishTag(self: *Builder, expr_id: CheckedExprId, at_ty: ?TypeId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const label, const arg_count: usize = switch (expr.data) {
            .tag => |tag| .{ tag.name, tag.args.len },
            .zero_argument_tag => |tag| .{ tag.name, 0 },
            else => unreachable, // only tag expressions finish as tags
        };
        const ty = at_ty orelse try self.exprType(expr_id);
        const values = try self.popValues(@intCast(arg_count));
        defer self.allocator.free(values);
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(label));
        try self.pushValue(try self.buildTag(ty, name, values));
    }

    /// The tag `name` of the tag union `ty` (through nominal wrappers) with
    /// `values` as its payloads.
    fn buildTag(self: *Builder, ty: TypeId, name: types.NameId, values: []const LocalId) Allocator.Error!LocalId {
        if (!self.program.store.isClosed(ty)) {
            const target = try self.localOf(ty, .frame);
            try self.emitDyn(target, .{ .tag = .{ .desc = try self.descOf(try self.structuralType(ty)), .name = try self.nameDesc(name), .payloads = try self.store.addLocalSpan(values) } });
            return target;
        }
        const rep = self.program.joins.representative(try self.structuralType(ty));
        const union_ty = switch (self.program.store.get(rep)) {
            .tag_union => |value| value,
            .primitive => |primitive| {
                if (primitive != .bool) invariant("a tag expression's type is a non-Bool primitive");
                return try self.boolValue(std.mem.eql(u8, self.program.store.nameText(name), "True"));
            },
            else => invariant("a tag expression's type is not a tag union"),
        };
        const tags = self.program.store.tagSpan(union_ty.tags);
        const variant = for (tags, 0..) |tag, index| {
            if (tag.name == name and tag.payloads.len == values.len) break index;
        } else invariant("a tag expression's tag is absent from its type");
        const target = try self.localOf(ty, .frame);
        const payload: ?LocalId = switch (values.len) {
            0 => null,
            1 => values[0],
            else => try self.buildStruct((try self.program.layouts.tagPayload(self.program.store.span(tags[variant].payloads))).?, values),
        };
        const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
        if (committed.tag != .tag_union) {
            // A single-variant union of zero-sized payloads has no tag storage.
            try self.emitLinear(.{ .assign_struct = .{ .target = target, .fields = LIR.LocalSpan.empty(), .next = undefined } });
        } else {
            try self.emitLinear(.{ .assign_tag = .{ .target = target, .variant_index = @intCast(variant), .discriminant = @intCast(variant), .payload = payload, .next = undefined } });
        }
        return target;
    }

    fn finishList(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const count: usize = switch (expr.data) {
            .list => |items| items.len,
            .empty_list => 0,
            else => unreachable, // only list expressions finish as lists
        };
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(count));
        defer self.allocator.free(values);
        const target = try self.localOf(ty, .frame);
        if (self.program.store.isClosed(ty)) {
            try self.emitLinear(.{ .assign_list = .{ .target = target, .elems = try self.store.addLocalSpan(values), .next = undefined } });
        } else {
            const elem = switch (self.program.store.get(ty)) {
                .list => |elem| elem,
                else => invariant("a list expression's type is not a List"),
            };
            try self.emitDyn(target, .{ .list = .{ .elem_desc = try self.descOf(elem), .elems = try self.store.addLocalSpan(values) } });
        }
        try self.pushValue(target);
    }

    // -- Control flow --

    fn ifTest(self: *Builder, expr_id: CheckedExprId, index: u32, join: LIR.JoinPointId, merge: ?MergeId) Allocator.Error!void {
        const if_ = self.bodies().expr(expr_id).data.if_;
        const cond = self.popValue();
        const holes = try self.emitBoolSwitch(cond);
        // Pushed in reverse execution order: the then-branch runs first, then
        // the else side continues at its own hole. Each side starts from the
        // binder values the construct started with.
        if (index + 1 < if_.branches.len) {
            try self.push(.{ .if_test = .{ .expr = expr_id, .index = index + 1, .join = join, .merge = merge } });
            try self.push(.{ .expr = if_.branches[index + 1].cond });
        } else {
            try self.push(.{ .jump = join });
            try self.push(.{ .expr = if_.final_else });
        }
        if (merge) |m| try self.push(.{ .restore_merge = m });
        try self.push(.{ .resume_at = holes.otherwise });
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = if_.branches[index].body });
        if (merge) |m| try self.restoreMerge(m);
        self.hole = holes.then;
    }

    fn matchStart(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const scrutinee = self.popValue();
        const match = self.bodies().expr(expr_id).data.match_;
        const result = try self.localOf(try self.exprType(expr_id), .frame);
        const join = self.freshJoin();
        const merge = try self.beginMerge(if (match.state_merge) |plan| self.bodies().stateMergeBinders(plan) else &.{});
        const after = try self.declareMergeJoin(join, result, merge);
        try self.push(.{ .push_value = result });
        if (merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .match_branch = .{ .expr = expr_id, .index = 0, .scrutinee = scrutinee, .join = join } });
    }

    fn matchBranch(self: *Builder, expr_id: CheckedExprId, index: u32, scrutinee: LocalId, join: LIR.JoinPointId) Allocator.Error!void {
        const match = self.bodies().expr(expr_id).data.match_;
        if (index >= match.branches.len) {
            // Exhaustive by checking: no value reaches past the last branch.
            try self.emitTerminal(.runtime_error);
            return;
        }
        const branch = match.branches[index];
        const join_index = @backingInt(join);
        if (self.join_infos.items[join_index].merge) |m| try self.restoreMerge(m);
        const patterns = branch.patternsSlice(self.bodies());
        if (patterns.len != 1) invariant("match branches with alternative patterns are not yet lowered by Boxy");
        const miss = try self.newHole();
        try self.testPattern(patterns[0].pattern, scrutinee, miss);
        if (branch.guard) |guard| {
            try self.push(.{ .match_guard = .{ .expr = expr_id, .index = index, .scrutinee = scrutinee, .join = join, .miss = miss } });
            try self.push(.{ .expr = guard });
            return;
        }
        try self.push(.{ .match_branch = .{ .expr = expr_id, .index = index + 1, .scrutinee = scrutinee, .join = join } });
        try self.push(.{ .resume_at = miss });
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = branch.value });
    }

    fn matchGuard(self: *Builder, expr_id: CheckedExprId, index: u32, scrutinee: LocalId, join: LIR.JoinPointId, miss: CFStmtId) Allocator.Error!void {
        const match = self.bodies().expr(expr_id).data.match_;
        try self.branchOrMiss(self.popValue(), miss);
        try self.push(.{ .match_branch = .{ .expr = expr_id, .index = index + 1, .scrutinee = scrutinee, .join = join } });
        try self.push(.{ .resume_at = miss });
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = match.branches[index].value });
    }

    /// Emit tests that `value` matches `pattern`, branching to `miss` when it
    /// does not, and bind the pattern's binders. Continues in the matched
    /// path.
    fn testPattern(self: *Builder, root_pattern: CheckedPatternId, root_value: LocalId, miss: CFStmtId) Allocator.Error!void {
        const Item = struct { pattern: CheckedPatternId, value: LocalId };
        var pending: std.ArrayList(Item) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .pattern = root_pattern, .value = root_value });
        while (pending.pop()) |item| {
            const pattern = self.bodies().pattern(item.pattern);
            switch (pattern.data) {
                .assign => |binder| try self.binders.put(self.allocator, binder, item.value),
                .underscore => {},
                .as => |as| {
                    try self.binders.put(self.allocator, as.binder, item.value);
                    try pending.append(self.allocator, .{ .pattern = as.pattern, .value = item.value });
                },
                .nominal => |nominal| try pending.append(self.allocator, .{ .pattern = nominal.backing_pattern, .value = item.value }),
                .applied_tag => |tag| {
                    const ty = try self.patternType(item.pattern);
                    const name = try self.program.store.internName(self.view.nameStore().tagLabelText(tag.name));
                    if (!self.program.store.isClosed(ty)) {
                        try self.testDynamicTag(ty, name, tag.args, item.value, miss, &pending);
                        continue;
                    }
                    const rep = self.program.joins.representative(try self.structuralType(ty));
                    switch (self.program.store.get(rep)) {
                        .primitive => |primitive| {
                            if (primitive != .bool) invariant("a tag pattern's type is a non-Bool primitive");
                            const want_true = std.mem.eql(u8, self.program.store.nameText(name), "True");
                            const cond = if (want_true) item.value else blk: {
                                const negated = try self.closedLocal(.bool);
                                try self.emitLowLevel(negated, .bool_not, &.{item.value});
                                break :blk negated;
                            };
                            try self.branchOrMiss(cond, miss);
                        },
                        .tag_union => |union_ty| {
                            const tags = self.program.store.tagSpan(union_ty.tags);
                            const variant = for (tags, 0..) |candidate, index| {
                                if (candidate.name == name and candidate.payloads.len == tag.args.len) break index;
                            } else invariant("a tag pattern's tag is absent from its type");
                            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
                            if (committed.tag == .tag_union and tags.len > 1) {
                                const discriminant = try self.closedLocal(.u32);
                                try self.emitLinear(.{ .assign_ref = .{ .target = discriminant, .op = LIR.RefOp.discriminant(item.value), .next = undefined } });
                                const expected = try self.closedLocal(.u32);
                                try self.emitLinear(.{ .assign_literal = .{ .target = expected, .value = .{ .i64_literal = .{ .value = @intCast(variant), .layout_idx = .u32 } }, .next = undefined } });
                                const matches = try self.closedLocal(.bool);
                                try self.emitLowLevel(matches, .num_is_eq, &.{ discriminant, expected });
                                try self.branchOrMiss(matches, miss);
                            }
                            const payloads = self.program.store.span(tags[variant].payloads);
                            for (tag.args, 0..) |arg_pattern, arg_index| {
                                const payload_local = try self.localOf(payloads[arg_index], .frame);
                                // A one-argument payload is the variant's whole payload.
                                const op: LIR.RefOp = if (committed.tag == .tag_union)
                                    LIR.RefOp.tagPayloadStruct(item.value, @intCast(variant), @intCast(variant))
                                else
                                    invariant("destructuring a tag of a storage-free union is not yet lowered by Boxy");
                                if (tag.args.len > 1) {
                                    // A multi-argument payload is a struct.
                                    const payload_struct = try self.localOf((try self.program.layouts.tagPayload(payloads)).?, .frame);
                                    try self.emitLinear(.{ .assign_ref = .{ .target = payload_struct, .op = LIR.RefOp.tagPayloadStruct(item.value, @intCast(variant), @intCast(variant)), .next = undefined } });
                                    try self.emitLinear(.{ .assign_ref = .{ .target = payload_local, .op = LIR.RefOp.field(payload_struct, @intCast(arg_index)), .next = undefined } });
                                } else {
                                    try self.emitLinear(.{ .assign_ref = .{ .target = payload_local, .op = op, .next = undefined } });
                                }
                                try pending.append(self.allocator, .{ .pattern = arg_pattern, .value = payload_local });
                            }
                        },
                        else => invariant("a tag pattern's type is not a tag union"),
                    }
                },
                .tuple => |items| {
                    const ty = try self.structuralType(try self.patternType(item.pattern));
                    const elems = switch (self.program.store.get(ty)) {
                        .tuple => |span| self.program.store.span(span),
                        else => invariant("a tuple pattern's type is not a tuple"),
                    };
                    for (items, 0..) |sub, index| {
                        const local = try self.readField(ty, item.value, @intCast(index), elems[index]);
                        try pending.append(self.allocator, .{ .pattern = sub, .value = local });
                    }
                },
                .record_destructure => |destructs| {
                    const ty = try self.structuralType(try self.patternType(item.pattern));
                    const fields = switch (self.program.store.get(ty)) {
                        .record => |record| self.program.store.fieldSpan(record.fields),
                        else => invariant("a record pattern's type is not a record"),
                    };
                    for (destructs) |destruct| {
                        const name = try self.program.store.internName(self.view.nameStore().recordFieldLabelText(destruct.label));
                        const semantic = for (fields, 0..) |field, index| {
                            if (field.name == name) break index;
                        } else invariant("a record pattern's field is absent from its type");
                        if (fields[semantic].kind != .inline_slot) invariant("destructuring optional record fields is not yet lowered by Boxy");
                        const local = try self.readField(ty, item.value, @intCast(semantic), fields[semantic].ty);
                        const sub = switch (destruct.kind) {
                            .required, .sub_pattern => |p| p,
                            .rest => invariant("record rest patterns are not yet lowered by Boxy"),
                        };
                        try pending.append(self.allocator, .{ .pattern = sub, .value = local });
                    }
                },
                else => invariantFmt("{s} patterns are not yet lowered by Boxy", .{@tagName(pattern.data)}),
            }
        }
    }

    /// Whether a tag union value holds the payload-free tag `label`: the
    /// checker's discriminant-only equality.
    fn finishDiscriminantEq(self: *Builder, value_ty: TypeId, label: check.CheckedNames.TagLabelId, negated: bool) Allocator.Error!void {
        const value = self.popValue();
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(label));
        const ty = try self.structuralType(value_ty);
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (self.program.store.isClosed(ty)) {
            const rep = self.program.joins.representative(ty);
            const tags = switch (self.program.store.get(rep)) {
                .tag_union => |union_ty| self.program.store.tagSpan(union_ty.tags),
                else => invariant("a discriminant equality's operand is not a tag union"),
            };
            const variant = for (tags, 0..) |tag, index| {
                if (tag.name == name and tag.payloads.len == 0) break index;
            } else invariant("a discriminant equality's tag is absent from its operand's type");
            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
            if (committed.tag == .tag_union) {
                try self.emitLinear(.{ .assign_ref = .{ .target = actual, .op = LIR.RefOp.discriminant(value), .next = undefined } });
            } else {
                try self.emitLinear(.{ .assign_literal = .{ .target = actual, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u32 } }, .next = undefined } });
            }
            try self.emitLinear(.{ .assign_literal = .{ .target = expected, .value = .{ .i64_literal = .{ .value = @intCast(variant), .layout_idx = .u32 } }, .next = undefined } });
        } else {
            const desc = try self.descOf(ty);
            try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = value, .desc = desc } });
            try self.emitDyn(expected, .{ .tag_discriminant_of = .{ .desc = desc, .name = try self.nameDesc(name) } });
        }
        const result = try self.closedLocal(.bool);
        try self.emitLowLevel(result, .num_is_eq, &.{ actual, expected });
        if (!negated) return try self.pushValue(result);
        const flipped = try self.closedLocal(.bool);
        try self.emitLowLevel(flipped, .bool_not, &.{result});
        try self.pushValue(flipped);
    }

    /// A record field access: each segment reads one field of the record
    /// the previous segment produced.
    fn finishFieldAccess(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const access = self.bodies().expr(expr_id).data.field_access;
        var value = self.popValue();
        var record_ty = try self.exprType(access.receiver);
        for (access.segments) |segment| {
            if (segment.mode != .required) invariant("optional field access is not yet lowered by Boxy");
            const name = try self.program.store.internName(self.view.nameStore().recordFieldLabelText(segment.field_name));
            const structural = try self.structuralType(record_ty);
            const record = switch (self.program.store.get(structural)) {
                .record => |record| record,
                else => invariant("a field access's receiver is not a record"),
            };
            const fields = self.program.store.fieldSpan(record.fields);
            const semantic = for (fields, 0..) |field, index| {
                if (field.name == name) break index;
            } else null;
            const field_ty = try self.convert(segment.success_ty);
            if (record.ext != null) {
                // An open row: the field's position is known only to the
                // record's descriptor.
                const local = try self.localOf(field_ty, .view);
                try self.emitDyn(local, .{ .field_addr_named = .{ .base = value, .desc = try self.descOf(structural), .name = try self.nameDesc(name) } });
                value = local;
            } else {
                const index = semantic orelse invariant("a field access's field is absent from its record type");
                if (fields[index].kind != .inline_slot) invariant("required access to an optional field reached Boxy");
                value = try self.readField(structural, value, @intCast(index), field_ty);
            }
            record_ty = field_ty;
        }
        try self.pushValue(value);
    }

    /// `ty` with its nominal wrappers peeled: a nominal type shares its
    /// backing's representation, so a tag or record of a nominal type is
    /// built and matched at its backing.
    fn structuralType(self: *Builder, ty: TypeId) Allocator.Error!TypeId {
        var current = ty;
        while (self.program.store.get(current) == .nominal) current = try self.program.converter().backing(current);
        return current;
    }

    /// Field `semantic_index` (of type `field_ty`) of the record or tuple
    /// `base` of type `ty`, as a borrowed read.
    fn readField(self: *Builder, ty: TypeId, base_local: LocalId, semantic_index: u32, field_ty: TypeId) Allocator.Error!LocalId {
        if (self.program.store.isClosed(ty)) {
            const local = try self.localOf(field_ty, .frame);
            try self.emitLinear(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.field(base_local, semantic_index), .next = undefined } });
            return local;
        }
        const local = try self.localOf(field_ty, .view);
        try self.emitDyn(local, .{ .field_addr = .{ .base = base_local, .desc = try self.descOf(ty), .semantic_index = semantic_index } });
        return local;
    }

    /// Test that the dynamic tag union `value` holds tag `name`, and queue
    /// its payloads against `args`.
    fn testDynamicTag(self: *Builder, nominal_ty: TypeId, name: types.NameId, args: []const CheckedPatternId, value: LocalId, miss: CFStmtId, pending: anytype) Allocator.Error!void {
        const store = &self.program.store;
        const ty = try self.structuralType(nominal_ty);
        const union_ty = switch (store.get(ty)) {
            .tag_union => |union_ty| union_ty,
            else => invariantFmt("tag patterns on a dynamic {s} are not yet lowered by Boxy", .{@tagName(store.get(ty))}),
        };
        const desc = try self.descOf(ty);
        const name_local = try self.nameDesc(name);
        const actual = try self.closedLocal(.u32);
        try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = value, .desc = desc } });
        const expected = try self.closedLocal(.u32);
        try self.emitDyn(expected, .{ .tag_discriminant_of = .{ .desc = desc, .name = name_local } });
        const matches = try self.closedLocal(.bool);
        try self.emitLowLevel(matches, .num_is_eq, &.{ actual, expected });
        try self.branchOrMiss(matches, miss);
        const payload_types = for (store.tagSpan(union_ty.tags)) |tag| {
            if (tag.name == name and tag.payloads.len == args.len) break store.span(tag.payloads);
        } else invariant("a tag pattern's tag is absent from its type");
        for (args, payload_types, 0..) |arg, payload_ty, index| {
            const local = try self.localOf(payload_ty, .view);
            try self.emitDyn(local, .{ .tag_payload_addr = .{ .base = value, .desc = desc, .name = name_local, .payload_index = @intCast(index) } });
            try pending.append(self.allocator, .{ .pattern = arg, .value = local });
        }
    }

    /// Continue when `cond` is true; otherwise continue at the enclosing
    /// match branch's shared `miss` continuation.
    fn branchOrMiss(self: *Builder, cond: LocalId, miss: CFStmtId) Allocator.Error!void {
        const then = try self.newHole();
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = then }});
        try self.emitTerminal(.{ .switch_stmt = .{ .cond = cond, .branches = branches, .default_branch = miss } });
        self.hole = then;
    }

    // -- Statements --

    fn stepStmt(self: *Builder, stmt_id: CheckedStatementId) Allocator.Error!void {
        const stmt = self.bodies().statement(stmt_id);
        self.region = stmt.source_region;
        switch (stmt.data) {
            .decl => |decl| {
                if (decl.recursive) invariant("recursive value declarations are not yet lowered by Boxy");
                try self.push(.{ .bind = decl.pattern });
                try self.push(.{ .expr = decl.expr });
            },
            .var_ => |var_| {
                try self.push(.{ .bind = var_.pattern });
                try self.push(.{ .expr = var_.expr });
            },
            .reassign => |reassign| {
                try self.push(.{ .bind = reassign.pattern });
                try self.push(.{ .expr = reassign.expr });
            },
            .var_uninitialized => |var_| {
                const local = try self.localOf(try self.patternType(var_.pattern), .frame);
                try self.emitLinear(.{ .init_uninitialized = .{ .target = local, .next = undefined } });
                try self.bindIrrefutable(var_.pattern, local);
            },
            .while_, .infinite_loop, .breakable_loop => |loop| try self.lowerLoop(loop),
            .for_ => |for_| {
                try self.push(.discard);
                try self.pushForLoop(for_.pattern, for_.expr, for_.body, for_.plan, for_.mutations);
            },
            .break_ => {
                const exit = self.loops.getLastOrNull() orelse invariant("a break outside every loop reached Boxy lowering");
                try self.emitJump(exit, null);
            },
            .expr => |expr| {
                try self.push(.discard);
                try self.push(.{ .expr = expr });
            },
            .expect => |cond| {
                try self.push(.discard);
                try self.push(.{ .finish_expect = cond });
                try self.push(.{ .expr = cond });
            },
            .crash => |literal| {
                const message = try self.store.insertString(self.bodies().stringLiteral(literal));
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = message } } });
            },
            .dbg => |expr| {
                try self.push(.discard);
                try self.push(.{ .expr = expr });
                invariant("dbg statements are not yet lowered by Boxy");
            },
            .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias => {},
            .promoted_proc => {},
            else => invariantFmt("checked statement {s} is not yet lowered by Boxy", .{@tagName(stmt.data)}),
        }
    }

    /// A condition-driven loop: a head join carrying the loop's mutated
    /// binders, entered first with their current values, and an exit join
    /// carrying them out.
    fn lowerLoop(self: *Builder, loop: checked.CheckedConditionLoop) Allocator.Error!void {
        const binders: []const checked.PatternBinderId = if (loop.mutations) |plan| blk: {
            const range = self.bodies().loopMutations(plan).binders;
            break :blk self.bodies().patternBinderIdPool()[range.start..][0..range.len];
        } else &.{};
        const exit_merge = try self.beginMerge(binders);
        const head_merge = try self.beginMerge(binders);
        const exit = self.freshJoin();
        const head = self.freshJoin();
        const after = try self.declareMergeJoin(exit, null, exit_merge);
        const head_body = try self.declareMergeJoin(head, null, head_merge);
        try self.emitJump(head, null);
        self.store.shapes.loop = true;
        // In the head: binders are the head's parameters.
        self.hole = head_body;
        if (head_merge) |m| try self.enterMerge(m);
        try self.loops.append(self.allocator, exit);
        try self.push(.end_loop);
        if (exit_merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .loop_test = .{ .body = loop.body, .head = head, .exit = exit } });
        try self.push(.{ .expr = loop.cond });
    }

    fn pushForLoop(self: *Builder, pattern: CheckedPatternId, iterable: CheckedExprId, body: CheckedExprId, plan: ?static_dispatch.IteratorForPlanId, mutations: ?checked.LoopMutationPlanId) Allocator.Error!void {
        const plan_id = plan orelse invariant("a for loop has no checked iterator plan");
        try self.push(.{ .unreachable_or_unit = {} });
        try self.push(.{ .for_start = .{ .pattern = pattern, .body = body, .plan = plan_id, .mutations = mutations } });
        try self.push(.{ .expr = iterable });
    }

    /// `for pattern in iterable { body }`: the iterable's iterator is the
    /// loop's state; each step calls `next` and matches its Done, Skip, or
    /// One tag (design.md "Iterator `for`").
    fn forStart(self: *Builder, pattern: CheckedPatternId, body: CheckedExprId, plan_id: static_dispatch.IteratorForPlanId, mutations: ?checked.LoopMutationPlanId) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const plan = table.iterator_for_plans[@backingInt(plan_id)];
        const iterable = self.popValue();
        const iterator_ty = try self.convert(plan.iterator_ty);
        const step_ty = try self.convert(plan.step_ty);
        const item_ty = try self.convert(plan.item_ty);
        const iterator = try self.emitDispatch(plan.iter.resolution, plan.iter.callable_ty, &.{iterable}, iterator_ty);

        const binders: []const checked.PatternBinderId = if (mutations) |id| blk: {
            const range = self.bodies().loopMutations(id).binders;
            break :blk self.bodies().patternBinderIdPool()[range.start..][0..range.len];
        } else &.{};
        const exit_merge = try self.beginMerge(binders);
        const head_merge = try self.beginMerge(binders);
        const exit = self.freshJoin();
        const head = self.freshJoin();
        const state = try self.localOf(iterator_ty, .frame);
        const after = try self.declareMergeJoin(exit, null, exit_merge);
        const head_body = try self.declareMergeJoin(head, state, head_merge);
        try self.emitJump(head, iterator);
        self.store.shapes.loop = true;

        self.hole = head_body;
        if (head_merge) |m| try self.enterMerge(m);
        const step_value = try self.emitDispatch(plan.next.resolution, plan.next.callable_ty, &.{state}, step_ty);
        const names = self.view.nameStore();
        const done = try self.program.store.internName(names.tagLabelText(plan.step_topology.done_tag));
        const skip = try self.program.store.internName(names.tagLabelText(plan.step_topology.skip_tag));
        const one = try self.program.store.internName(names.tagLabelText(plan.step_topology.one_tag));
        const item_name = try self.program.store.internName(names.recordFieldLabelText(plan.step_topology.item_field));
        const rest_name = try self.program.store.internName(names.recordFieldLabelText(plan.step_topology.rest_field));

        // Done: leave the loop.
        const not_done_hole = try self.newHole();
        try self.branchOnTag(step_value, step_ty, done, not_done_hole);
        try self.emitJump(exit, null);
        // Skip: continue with its rest.
        self.hole = not_done_hole;
        const one_hole = try self.newHole();
        try self.branchOnTag(step_value, step_ty, skip, one_hole);
        {
            const payload = try self.tagPayloadValue(step_value, step_ty, skip, 0);
            const rest = try self.recordFieldValue(payload.value, payload.ty, rest_name);
            try self.emitJump(head, rest.value);
        }
        // One: bind the item, run the body, continue with its rest.
        self.hole = one_hole;
        const payload = try self.tagPayloadValue(step_value, step_ty, one, 0);
        const item = try self.recordFieldValue(payload.value, payload.ty, item_name);
        const rest = try self.recordFieldValue(payload.value, payload.ty, rest_name);
        _ = item_ty;
        try self.bindIrrefutable(pattern, item.value);
        try self.loops.append(self.allocator, exit);
        try self.push(.end_loop);
        if (exit_merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .jump = head });
        try self.push(.{ .push_value = rest.value });
        try self.push(.discard);
        try self.push(.{ .expr = body });
    }

    /// Continue at the next statement when `value` (of `ty`) holds tag
    /// `name`; otherwise continue at `miss`.
    fn branchOnTag(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId, miss: CFStmtId) Allocator.Error!void {
        const ty = try self.structuralType(raw_ty);
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (self.program.store.isClosed(ty)) {
            const variant = self.closedVariant(ty, name);
            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
            if (committed.tag == .tag_union) {
                try self.emitLinear(.{ .assign_ref = .{ .target = actual, .op = LIR.RefOp.discriminant(value), .next = undefined } });
            } else {
                try self.emitLinear(.{ .assign_literal = .{ .target = actual, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u32 } }, .next = undefined } });
            }
            try self.emitLinear(.{ .assign_literal = .{ .target = expected, .value = .{ .i64_literal = .{ .value = @intCast(variant.index), .layout_idx = .u32 } }, .next = undefined } });
        } else {
            const desc = try self.descOf(ty);
            try self.emitDyn(actual, .{ .tag_discriminant = .{ .base = value, .desc = desc } });
            try self.emitDyn(expected, .{ .tag_discriminant_of = .{ .desc = desc, .name = try self.nameDesc(name) } });
        }
        const matches = try self.closedLocal(.bool);
        try self.emitLowLevel(matches, .num_is_eq, &.{ actual, expected });
        try self.branchOrMiss(matches, miss);
    }

    const ClosedVariant = struct { index: usize, payloads: []const TypeId };

    fn closedVariant(self: *Builder, ty: TypeId, name: types.NameId) ClosedVariant {
        const rep = self.program.joins.representative(ty);
        const tags = switch (self.program.store.get(rep)) {
            .tag_union => |union_ty| self.program.store.tagSpan(union_ty.tags),
            else => invariant("a tag test's value is not a tag union"),
        };
        for (tags, 0..) |tag, index| {
            if (tag.name == name) return .{ .index = index, .payloads = self.program.store.span(tag.payloads) };
        }
        invariant("a tag test's tag is absent from its type");
    }

    const TypedValue = struct { value: LocalId, ty: TypeId };

    /// Payload `index` of tag `name` in `value`, which holds that tag.
    fn tagPayloadValue(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId, index: u32) Allocator.Error!TypedValue {
        const ty = try self.structuralType(raw_ty);
        if (!self.program.store.isClosed(ty)) {
            const union_ty = self.program.store.get(ty).tag_union;
            const payload_ty = for (self.program.store.tagSpan(union_ty.tags)) |tag| {
                if (tag.name == name) break self.program.store.span(tag.payloads)[index];
            } else invariant("a tag payload's tag is absent from its type");
            const local = try self.localOf(payload_ty, .view);
            try self.emitDyn(local, .{ .tag_payload_addr = .{ .base = value, .desc = try self.descOf(ty), .name = try self.nameDesc(name), .payload_index = index } });
            return .{ .value = local, .ty = payload_ty };
        }
        const variant = self.closedVariant(ty, name);
        const payload_ty = variant.payloads[index];
        const local = try self.localOf(payload_ty, .frame);
        if (variant.payloads.len == 1) {
            try self.emitLinear(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.tagPayloadStruct(value, @intCast(variant.index), @intCast(variant.index)), .next = undefined } });
        } else {
            const payload_struct = try self.localOf((try self.program.layouts.tagPayload(variant.payloads)).?, .frame);
            try self.emitLinear(.{ .assign_ref = .{ .target = payload_struct, .op = LIR.RefOp.tagPayloadStruct(value, @intCast(variant.index), @intCast(variant.index)), .next = undefined } });
            try self.emitLinear(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.field(payload_struct, index), .next = undefined } });
        }
        return .{ .value = local, .ty = payload_ty };
    }

    /// Field `name` of the record `value`.
    fn recordFieldValue(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId) Allocator.Error!TypedValue {
        const ty = try self.structuralType(raw_ty);
        const record = switch (self.program.store.get(ty)) {
            .record => |record| record,
            else => invariant("a record field read's value is not a record"),
        };
        const fields = self.program.store.fieldSpan(record.fields);
        const index = for (fields, 0..) |field, i| {
            if (field.name == name) break i;
        } else invariant("a record field read's field is absent from its type");
        const field_ty = fields[index].ty;
        return .{ .value = try self.readField(ty, value, @intCast(index), field_ty), .ty = field_ty };
    }

    /// Call the target a dispatch resolution names with already-lowered
    /// arguments.
    fn emitDispatch(self: *Builder, resolution: static_dispatch.CheckedCallResolution, callable_ty: checked.CheckedTypeId, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const table = self.view.view.static_dispatch_plans;
        switch (resolution) {
            .direct_closed, .direct_parametric => |direct| {
                const node = table.evidenceNode(direct.evidence);
                if (node.instantiation == .alias) invariant("dispatch targets reached through a procedure alias are not yet lowered by Boxy");
                const target_module = self.program.moduleByIdx(node.target.module_idx);
                const template = target_module.view.checked_procedure_templates.lookupByDef(node.target.def_idx) orelse
                    invariant("a dispatch target has no procedure template");
                const callee = try self.program.templateRefWorker(template);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                const nested: []const static_dispatch.CheckedEvidence = switch (node.nested) {
                    .resolved => table.nestedEvidence(node),
                    .from_callable => if (self.program.worker(callee).dictionary_params.len == 0) &.{} else invariant("callable-derived call evidence is not yet lowered by Boxy"),
                    .through_alias => invariant("alias-scope call evidence is not yet lowered by Boxy"),
                };
                const hidden = try self.hiddenArgs(callee, subst, nested);
                defer self.allocator.free(hidden);
                const all = try std.mem.concat(self.allocator, LocalId, &.{ args, hidden });
                defer self.allocator.free(all);
                const target = try self.localOf(result_ty, .frame);
                try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(callee).proc, .args = try self.store.addLocalSpan(all), .next = undefined } });
                return target;
            },
            .evidence_dependent => |dependent| {
                return try self.emitErasedCall(self.scopeDictionary(dependent.index), try self.convert(callable_ty), args, result_ty);
            },
            else => invariantFmt("iterator dispatch resolution {s} is not yet lowered by Boxy", .{@tagName(resolution)}),
        }
    }

    fn loopTest(self: *Builder, body: CheckedExprId, head: LIR.JoinPointId, exit: LIR.JoinPointId) Allocator.Error!void {
        const holes = try self.emitBoolSwitch(self.popValue());
        self.hole = holes.otherwise;
        try self.emitJump(exit, null);
        self.hole = holes.then;
        try self.push(.{ .jump_without_value = head });
        try self.push(.discard);
        try self.push(.{ .expr = body });
    }

    /// Bind an irrefutable pattern (a parameter or a `decl`) to `value`.
    /// Destructuring binds through the same tests a match uses; an
    /// irrefutable pattern never takes their miss path.
    fn bindIrrefutable(self: *Builder, pattern_id: CheckedPatternId, value: LocalId) Allocator.Error!void {
        const miss = try self.newHole();
        try self.testPattern(pattern_id, value, miss);
    }
};

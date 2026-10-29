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
const collections = @import("collections");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");
const boxy_runtime = @import("boxy_runtime");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");
const Codec = @import("codec.zig");
const Match = @import("match.zig");

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
const Layouts = @import("layouts.zig").Layouts;
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
    // Statement emission records the procedure's calls and joins, from
    // which its finish proves the tail calls (design.md "Tail Return
    // Continuations").
    program.result.store.tail_call_builder = &builder.tail_calls;
    defer program.result.store.tail_call_builder = null;
    try builder.lowerBody();
}

/// One unit of pending lowering work on the builder's task stack.
pub const Task = union(enum) {
    expr: CheckedExprId,
    stmt: CheckedStatementId,
    /// Pop the value of a `decl` statement's expression and bind its pattern.
    bind: struct { pattern: CheckedPatternId, ty: TypeId },
    /// Pop and drop one value.
    discard,
    /// Re-tag the popped ground value of type `from` into the open-row type
    /// `to`: each reached tag row's discriminant by its tag's name, under
    /// `to`'s descriptor.
    retag: struct { from: TypeId, to: TypeId },
    /// After a divergent statement: no statement is reached, and the value
    /// of its block, of this type or of no type when the checker gave it
    /// one holding the erroneous type, is never produced.
    diverged: ?TypeId,
    /// Crash as a dispatch that cannot run does; its value is of type
    /// `result`.
    dispatch_crash: struct { kind: lower.CrashDictionary, result: TypeId },
    /// Crash with a checked runtime error, whose value is of this type, or
    /// of no type when the checker gave it the erroneous type.
    runtime_error: ?TypeId,
    /// Pop the arguments of a call to `callee` and call it.
    /// `adapt` is set for a callee storing record fields of its signature
    /// in presence slots: the types its scheme takes and the arguments'.
    finish_call: struct { callee: WorkerId, hidden: []LocalId, arg_count: u32, ret: TypeId, result: TypeId, adapt: ?Builder.CallAdapt = null },
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
    /// A null `join` is a construct in tail position: each branch returns.
    if_test: struct { expr: CheckedExprId, index: u32, join: ?LIR.JoinPointId, merge: ?MergeId },
    /// Pop a value and jump to `join` with it.
    jump: LIR.JoinPointId,
    /// Jump to a join that carries no value (a loop's head or exit).
    jump_without_value: LIR.JoinPointId,
    /// Pop a literal conversion's `Try` result and unwrap it.
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
    /// Leave the innermost scheme alias scope (`Builder.alias_scopes`).
    leave_alias_scope,
    /// A loop whose pattern checking rejected, its iterable evaluated:
    /// crash as the iterator dispatch that cannot run, or as rejected code.
    rejected_for: static_dispatch.IteratorForPlanId,
    /// Fill the slots of a recursive value declaration's binders, once the
    /// declaration's pattern bound them.
    fill_recursive_slots: CheckedPatternId,
    /// Continue emitting at `hole`.
    resume_at: CFStmtId,
    finish_field_access: CheckedExprId,
    /// Pop a tag union value and compare its discriminant with `tag`'s.
    finish_discriminant_eq: struct { value_ty: TypeId, tag: check.CheckedNames.TagLabelId, negated: bool },
    finish_tuple_access: CheckedExprId,
    /// Pop a value of this type, report it through `dbg`, and push it back.
    finish_dbg: TypeId,
    restore: struct { source: lower.ConstSource, node: checked.ConstNodeId, ty: TypeId },
    /// Push the function value a callable-eval binding stored.
    restore_callable_eval: struct { module: ModuleIndex, template: checked.CallableEvalTemplateId, ty: TypeId },
    /// Pop a restored closure's capture values and build it.
    /// `worker_ty` is the closure's type as its worker stores it, and `ty`
    /// the restored value's.
    finish_restore_closure: struct { worker: WorkerId, count: u32, descs: []const LocalId, dicts: []const LocalId, capture_types: []const TypeId, worker_ty: TypeId, ty: TypeId },
    finish_restore_struct: struct { ty: TypeId, count: u32 },
    finish_restore_tag: struct { ty: TypeId, name: types.NameId, count: u32 },
    finish_restore_list: struct { ty: TypeId, count: u32 },
    /// Push a value of `ty` for an expression whose evaluation diverged.
    unreachable_value: TypeId,
    /// Push the `{}` value of a construct whose value nothing reads.
    unreachable_or_unit,
    /// Crash with this message: a literal whose conversion failed at the
    /// instance a literal table selects.
    literal_failed: []const u8,
    /// Pop a generalized local procedure call's explicit arguments and call
    /// its function value with them after its own scheme's words.
    /// `params` and `ret` are the procedure's signature, `arg_types` the
    /// call's arguments' types and `result` its result's.
    finish_local_call: struct { closure: LocalId, words: []const LocalId, params: []const TypeId, ret: TypeId, arg_types: []const TypeId, result: TypeId },
    /// Push an interpolation's literal segments as the `List(Str)` of `ty`.
    segments_arg: struct { expr: CheckedExprId, ty: TypeId },
    /// Pop an interpolation's assembler and call it with the interpolated
    /// values.
    finish_interpolation: CheckedExprId,
    finish_interpolation_call: struct { assembler: LocalId, assembler_ty: TypeId, values_ty: TypeId, count: u32, result: TypeId },
    /// Pop a value and a hasher and hash the value structurally.
    finish_hash: struct { value: TypeId, result: TypeId },
    /// Pop an `expect`'s failing `?` value and fail the expect with it.
    finish_expect_err: struct { expr: CheckedExprId, value: TypeId },
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
    /// Continue a match's decision-tree emission with the statement that
    /// answers its outstanding request (null on its first step).
    match_advance: struct { state: *Match.State, input: ?CFStmtId },
    /// Pop a match guard's value into `cond`, continuing at `next`.
    match_guard_done: struct { cond: LocalId, next: CFStmtId },
};

/// A control-flow construct's merge of the binders it reassigns (a checked
/// state merge plan): their values before it, and the join parameters that
/// carry their values after it.
pub const Merge = struct {
    binders: []const checked.PatternBinderId,
    pre: []LocalId,
    params: []LocalId,
};

/// Names a `Merge` by its index among the builder's merges.
pub const MergeId = u32;

/// What a join's parameters are: an optional value, then the binders of its
/// merge.
pub const JoinInfo = struct {
    value: ?LocalId,
    merge: ?MergeId,
};

/// A loop being lowered: its exit join, and whether anything leaves through
/// it (a failed condition, or a break).
pub const LoopExit = struct { join: LIR.JoinPointId, reached: bool };

/// A value a pattern tests, with the value's type.
pub const PatternItem = struct { pattern: CheckedPatternId, value: LocalId, ty: TypeId };

/// The key and variable descriptors a recursive member's placeholder was
/// opened with, which its close repeats.
const OpenMember = struct { key: LocalId, vars: LIR.LocalSpan };

/// A scheme alias's scope as one use instantiates it: the checked evidence
/// parameters its dictionaries stand for, those dictionaries, and the
/// environment it extends.
const AliasScope = struct { params: []const u32, dicts: []const LocalId, saved_env: types.Env };

/// One exact procedure alias of a method's alias chain: its forwarded lookup
/// in `module`, and the quantified variables and requirements of the scope
/// the alias owns.
const AliasStep = struct {
    module: ModuleIndex,
    lookup: CheckedExprId,
    vars: []const checked.CheckedTypeId,
    params: []const static_dispatch.EvidenceParamRecord,
};

/// Lowers one worker's checked body to LIR.
pub const Builder = struct {
    program: *Program,
    allocator: Allocator,
    worker_id: WorkerId,
    module: ModuleIndex,
    view: *const types.ModuleView,
    env: types.Env,
    store: *LirStore,
    frame_locals: std.ArrayList(LocalId) = .empty,
    binders: collections.DenseMapUnmanaged(checked.PatternBinderId, LocalId) = .empty,
    /// The descriptor local of each scope variable (its hidden parameter).
    var_descs: []LocalId = &.{},
    /// The dictionaries in scope (`Worker.scope_dictionary_indices` order).
    dict_locals: []LocalId = &.{},
    /// The scheme alias scopes entered while lowering an alias's right-hand
    /// side at one of its uses, innermost last. Each is a generalized scope
    /// nested inside the worker's own scopes.
    alias_scopes: std.ArrayList(AliasScope) = .empty,
    /// The slot of each recursive value binder in scope (design.md
    /// "Recursive values"): a `Box` of a one-item tuple holding the value,
    /// which callables capture in place of the value.
    recursive_slots: collections.DenseMapUnmanaged(checked.PatternBinderId, LocalId) = .empty,
    /// What each lambda this body declares captured where it was declared:
    /// a `var`'s value then, whatever the `var` holds where the procedure is
    /// later used.
    declared_captures: collections.DenseMapUnmanaged(CheckedExprId, []const LocalId) = .empty,
    /// While lowering an exact procedure alias's forwarded lookup in the
    /// alias's own module (`aliasTargetHidden`): where that alias's scopes
    /// start in `alias_scopes`. Its lookup reaches no scope outside them.
    foreign_scopes: ?usize = null,
    type_descs: collections.DenseMapUnmanaged(TypeId, LocalId) = .empty,
    /// The locals the entry chain defines, which every statement of the
    /// body can read.
    entry_locals: collections.DenseMapUnmanaged(LocalId, void) = .empty,
    /// Whether straight-line statements currently go to the entry chain
    /// (`emitIntoEntry`), whose end is then the current hole.
    emitting_entry: bool = false,
    /// Whether function values are built as immortal compositions (a
    /// descriptor's local-procedure override, which the descriptor holds
    /// for the program's lifetime) instead of owned callables.
    immortal_closures: bool = false,
    /// The compile-time root whose entry wrapper this body is: its own
    /// hoisted expression computes its value here rather than reading it.
    own_root: ?checked.ComptimeRootId = null,
    /// The recursive components whose members' descriptors this body has
    /// begun composing.
    opened_components: std.AutoHashMapUnmanaged(u32, void) = .empty,
    /// The recursive members whose placeholder descriptor this body has
    /// opened and not yet closed.
    open_members: collections.DenseMapUnmanaged(TypeId, OpenMember) = .empty,
    /// The recursion box over each descriptor local that has one.
    recursive_boxes: collections.DenseMapUnmanaged(LocalId, LocalId) = .empty,
    opaque_word_desc: ?LocalId = null,
    /// Expressions whose value is built at a row coercion's destination
    /// type rather than their own (design.md "Row Coercion Primitive").
    destination_types: collections.DenseMapUnmanaged(CheckedExprId, TypeId) = .empty,
    /// The expressions whose value is the procedure's result.
    tail_exprs: collections.DenseMapUnmanaged(CheckedExprId, void) = .empty,
    /// The procedure's calls and joins as emitted, for its tail calls.
    tail_calls: lir_core.TailCallBuilder,
    /// The statement emitted last, while nothing follows it.
    last_stmt: ?CFStmtId = null,
    /// How many branching statements (terminals and joins) this body has
    /// emitted.
    branch_count: u32 = 0,
    name_locals: collections.DenseMapUnmanaged(types.NameId, LocalId) = .empty,
    entry_hole: CFStmtId = undefined,
    hole: ?CFStmtId = null,
    join_points: std.ArrayList(LIR.JoinPoint) = .empty,
    /// What each join of this body carries, by join id.
    join_infos: std.ArrayList(JoinInfo) = .empty,
    merges: std.ArrayList(Merge) = .empty,
    /// The exit joins of the loops being lowered, innermost last.
    loops: std.ArrayList(LoopExit) = .empty,
    next_join: u32 = 0,
    values: std.ArrayList(LocalId) = .empty,
    tasks: std.ArrayList(Task) = .empty,
    region: base.Region = base.Region.zero(),
    has_dynamic_frame: bool = false,
    result_local: ?LocalId = null,

    pub fn init(program: *Program, id: WorkerId) Allocator.Error!Builder {
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
            .tail_calls = lir_core.TailCallBuilder.init(program.allocator, worker.proc),
        };
    }

    pub fn deinit(self: *Builder) void {
        self.frame_locals.deinit(self.allocator);
        self.binders.deinit(self.allocator);
        self.alias_scopes.deinit(self.allocator);
        self.recursive_slots.deinit(self.allocator);
        self.declared_captures.deinit(self.allocator);
        self.type_descs.deinit(self.allocator);
        self.entry_locals.deinit(self.allocator);
        self.opened_components.deinit(self.allocator);
        self.open_members.deinit(self.allocator);
        self.recursive_boxes.deinit(self.allocator);
        self.name_locals.deinit(self.allocator);
        self.join_points.deinit(self.allocator);
        self.join_infos.deinit(self.allocator);
        self.destination_types.deinit(self.allocator);
        self.tail_exprs.deinit(self.allocator);
        self.tail_calls.deinit();
        self.merges.deinit(self.allocator);
        self.loops.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.tasks.deinit(self.allocator);
    }

    pub fn currentWorker(self: *Builder) *lower.Worker {
        return self.program.worker(self.worker_id);
    }

    pub fn bodies(self: *const Builder) checked.CheckedBodyStoreView {
        return self.view.bodies();
    }

    /// The inline-expect mode the program's divergence data is read in.
    pub fn inlineExpectMode(self: *const Builder) checked.InlineExpectMode {
        return switch (self.program.input.inline_expects) {
            .run => .run,
            .omit => .omit,
        };
    }

    // -- Locals --

    pub fn addLocal(self: *Builder, local: LIR.Local) Allocator.Error!LocalId {
        const id = try self.store.addLocal(local);
        try self.frame_locals.append(self.allocator, id);
        return id;
    }

    pub fn closedLocal(self: *Builder, layout_idx: layout.Idx) Allocator.Error!LocalId {
        return self.addLocal(.{ .layout_idx = layout_idx });
    }

    /// A local holding a value of `ty`: an ordinary local of its committed
    /// layout when `ty` is closed, otherwise a dynamic local of `storage`.
    pub fn localOf(self: *Builder, ty: TypeId, storage: LIR.DynStorage) Allocator.Error!LocalId {
        if (try self.closed(ty)) return self.closedLocal(try self.program.layouts.layoutOf(ty));
        const desc = try self.descOf(ty);
        if (storage == .frame) self.has_dynamic_frame = true;
        return self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = storage } });
    }

    /// Whether values of `ty` have one committed layout (see
    /// `Converter.representationClosed`).
    pub fn closed(self: *Builder, ty: TypeId) Allocator.Error!bool {
        return self.program.converter().representationClosed(ty);
    }

    pub fn convert(self: *Builder, ty: checked.CheckedTypeId) Allocator.Error!TypeId {
        return self.program.converter().convert(self.module, ty, self.env);
    }

    pub fn exprType(self: *Builder, expr: CheckedExprId) Allocator.Error!TypeId {
        const own = try self.convert(self.bodies().expr(expr).ty);
        // A value flowing to a destination of the same shape is built at the
        // destination's type; a diverging value's own type can differ in
        // shape, and it builds nothing. A value no construction builds
        // keeps its own type and is re-tagged into its destination
        // (`retagTarget`).
        if (self.destination_types.get(expr)) |destination| {
            if (try self.retagTarget(expr, own) != null) return own;
            if (try types.compatible(self.allocator, &self.program.store, own, destination)) return destination;
        }
        return own;
    }

    /// The destination a value of expression `expr` (of its own type `own`)
    /// is converted into: an expression that produces its value rather than
    /// building it (a read or a call) has the representation of its own
    /// type, which differs from its destination's where the destination
    /// widens a row of it (design.md "Row Coercion Primitive", "Sealed
    /// Result Rows").
    pub fn retagTarget(self: *Builder, expr: CheckedExprId, own: TypeId) Allocator.Error!?TypeId {
        const destination = self.destination_types.get(expr) orelse return null;
        if (destination == own) return null;
        switch (self.bodies().expr(expr).data) {
            .block, .if_, .match_, .nominal, .dbg, .tag, .zero_argument_tag, .record, .empty_record, .tuple, .list, .empty_list, .numeral, .str_from_quote, .str_segment, .str, .interpolation, .lambda, .closure, .runtime_error, .crash, .return_ => return null,
            .pending, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .call, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .expect_err, .expect, .ellipsis, .anno_only, .break_, .row_coerce, .for_, .hosted_lambda, .run_low_level => {},
        }
        // Two closed types committing one layout need no conversion.
        if (try self.closed(own) and try self.closed(destination) and try self.program.layouts.layoutOf(own) == try self.program.layouts.layoutOf(destination)) return null;
        return destination;
    }

    /// The type a read of a stored constant restores its value at: the
    /// read's own, closed type, converted into its destination where that
    /// widens it, or else the type `exprType` gives it.
    pub fn constReadType(self: *Builder, expr: CheckedExprId) Allocator.Error!TypeId {
        const own = try self.convert(self.bodies().expr(expr).ty);
        if (self.program.store.isClosed(own)) return own;
        return try self.exprType(expr);
    }

    pub fn patternType(self: *Builder, pattern: CheckedPatternId) Allocator.Error!TypeId {
        return self.convert(self.bodies().pattern(pattern).ty);
    }

    // -- Statements --

    pub fn origin(self: *const Builder, kind: LIR.OriginKind) LIR.StmtOrigin {
        return .{
            .loc = self.program.sourceLoc(self.module, self.region),
            .region = self.region,
            .inline_scope = LIR.InlineScopeId.none,
            .kind = kind,
        };
    }

    pub fn newHole(self: *Builder) Allocator.Error!CFStmtId {
        return self.store.addCFStmt(.runtime_error, self.origin(.lowering_glue));
    }

    /// Replace `hole` with `stmt`.
    pub fn fill(self: *Builder, hole: CFStmtId, stmt: LIR.CFStmt, kind: LIR.OriginKind) Allocator.Error!void {
        try self.store.replaceCFStmt(hole, stmt, self.origin(kind));
        self.store.noteStmtShapes(stmt);
    }

    /// The hole the next statement goes into. Code after a diverging
    /// statement is unreachable; it is still emitted, into a hole nothing
    /// reaches.
    pub fn currentHole(self: *Builder) Allocator.Error!CFStmtId {
        if (self.hole) |hole| return hole;
        const hole = try self.newHole();
        self.hole = hole;
        return hole;
    }

    /// Emit a straight-line statement built by `make(next)`.
    pub fn emitLinear(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const hole = try self.currentHole();
        const next = try self.newHole();
        var linked = stmt;
        setNext(&linked, next);
        try self.fill(hole, linked, .source);
        self.hole = next;
        self.last_stmt = hole;
    }

    /// Emit a terminal statement; nothing follows it.
    pub fn emitTerminal(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        const hole = try self.currentHole();
        try self.fill(hole, stmt, .source);
        self.hole = null;
        self.branch_count += 1;
    }

    pub fn emitEntry(self: *Builder, stmt: LIR.CFStmt) Allocator.Error!void {
        if (definedLocal(stmt)) |target| try self.entry_locals.put(self.allocator, target, {});
        if (self.emitting_entry) return try self.emitLinear(stmt);
        const next = try self.newHole();
        var linked = stmt;
        setNext(&linked, next);
        try self.fill(self.entry_hole, linked, .scaffold);
        self.entry_hole = next;
    }

    /// The local a straight-line statement defines, if any.
    fn definedLocal(stmt: LIR.CFStmt) ?LocalId {
        return switch (stmt) {
            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .expect_err, .dyn, .runtime_error, .comptime_exhaustiveness_failed, .incref, .decref, .decref_if_initialized, .free, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => |payload| blk: {
                const Payload = @TypeOf(payload);
                if (@typeInfo(Payload) != .@"struct" or !@hasField(Payload, "target")) break :blk null;
                if (@TypeOf(payload.target) != LocalId) break :blk null;
                break :blk payload.target;
            },
        };
    }

    /// Run `emit` (a function of `self` emitting straight-line statements)
    /// with its statements going to the entry chain, and record the locals
    /// they define as entry locals.
    pub fn emitIntoEntry(self: *Builder, comptime emit: anytype, args: anytype) Allocator.Error!LocalId {
        if (self.emitting_entry) {
            const nested = try @call(.auto, emit, .{self} ++ args);
            try self.entry_locals.put(self.allocator, nested, {});
            return nested;
        }
        const saved_hole = self.hole;
        const saved_last = self.last_stmt;
        self.hole = self.entry_hole;
        self.emitting_entry = true;
        const result = try @call(.auto, emit, .{self} ++ args);
        self.emitting_entry = false;
        self.entry_hole = self.hole orelse invariant("an entry-chain emission diverged");
        self.hole = saved_hole;
        self.last_stmt = saved_last;
        try self.entry_locals.put(self.allocator, result, {});
        return result;
    }

    pub fn setNext(stmt: *LIR.CFStmt, next: CFStmtId) void {
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

    pub fn emitLowLevel(self: *Builder, target: LocalId, op: LIR.LowLevel, args: []const LocalId) Allocator.Error!void {
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .args = try self.store.addLocalSpan(args),
            .next = undefined,
        } });
    }

    pub fn emitDyn(self: *Builder, target: ?LocalId, op: LIR.DynOp) Allocator.Error!void {
        try self.emitLinear(.{ .dyn = .{ .target = target, .data = try self.store.addStmtRow(LIR.DynOp, op), .next = undefined } });
    }

    pub fn freshJoin(self: *Builder) LIR.JoinPointId {
        const id: LIR.JoinPointId = @fromBackingInt(self.next_join);
        self.next_join += 1;
        return id;
    }

    /// Start a merge of `binders`, recording their current values.
    pub fn beginMerge(self: *Builder, plan_binders: []const checked.PatternBinderId) Allocator.Error!?MergeId {
        // A binder the construct reassigns but that is not bound before it
        // is declared inside it, and goes out of scope with it.
        const arena = self.program.arena.allocator();
        var in_scope: std.ArrayList(checked.PatternBinderId) = .empty;
        for (plan_binders) |binder| {
            if (self.binders.contains(binder)) try in_scope.append(arena, binder);
        }
        const binders = in_scope.items;
        if (binders.len == 0) return null;
        const pre = try arena.alloc(LocalId, binders.len);
        const params = try arena.alloc(LocalId, binders.len);
        for (binders, pre, params) |binder, *before, *param| {
            before.* = self.binders.get(binder).?;
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
    pub fn declareMergeJoin(self: *Builder, id: LIR.JoinPointId, value: ?LocalId, merge: ?MergeId) Allocator.Error!CFStmtId {
        var params: std.ArrayList(LocalId) = .empty;
        defer params.deinit(self.allocator);
        if (value) |local| try params.append(self.allocator, local);
        if (merge) |m| try params.appendSlice(self.allocator, self.merges.items[m].params);
        const index = @backingInt(id);
        while (self.join_infos.items.len <= index) try self.join_infos.append(self.allocator, .{ .value = null, .merge = null });
        self.join_infos.items[index] = .{ .value = value, .merge = merge };
        return try self.declareJoin(id, params.items);
    }

    pub fn restoreMerge(self: *Builder, merge: MergeId) Allocator.Error!void {
        const m = self.merges.items[merge];
        for (m.binders, m.pre) |binder, value| try self.binders.put(self.allocator, binder, value);
    }

    pub fn enterMerge(self: *Builder, merge: MergeId) Allocator.Error!void {
        const m = self.merges.items[merge];
        for (m.binders, m.params) |binder, value| try self.binders.put(self.allocator, binder, value);
    }

    /// Declare join `id` with `params` here; continue in its remainder, and
    /// return the hole of its body.
    pub fn declareJoin(self: *Builder, id: LIR.JoinPointId, params: []const LocalId) Allocator.Error!CFStmtId {
        self.branch_count += 1;
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
    pub fn emitJump(self: *Builder, id: LIR.JoinPointId, value: ?LocalId) Allocator.Error!void {
        const index = @backingInt(id);
        const info: JoinInfo = if (index < self.join_infos.items.len) self.join_infos.items[index] else .{ .value = null, .merge = null };
        if (info.value) |param| {
            const source = value orelse invariant("a jump to a value join carried no value");
            try self.emitLinear(.{ .set_local = .{ .target = param, .value = source, .mode = .initialize_join_param, .next = undefined } });
        }
        if (info.merge) |merge| {
            const m = self.merges.items[merge];
            // The parameters take their values at once: a value that is one
            // of the parameters (a loop's carried value moving to another
            // carried position) or a view, which may point into one, is
            // read into a local of its own before any parameter is written.
            const sources = try self.allocator.alloc(LocalId, m.params.len);
            defer self.allocator.free(sources);
            var params: collections.DenseMapUnmanaged(LocalId, void) = .empty;
            defer params.deinit(self.allocator);
            for (m.params) |param| try params.put(self.allocator, param, {});
            for (m.binders, m.params, sources) |binder, param, *source| {
                const current = self.binders.get(binder) orelse invariant("a merged binder lost its value");
                source.* = current;
                if (current == param) continue;
                const dyn = self.store.localDyn(current);
                if (!params.contains(current) and (dyn == null or dyn.?.storage != .view)) continue;
                if (dyn) |dyn_info| {
                    const staged = try self.addLocal(.{ .layout_idx = self.store.getLocal(current).layout_idx, .dyn = .{ .desc = dyn_info.desc, .storage = .frame } });
                    self.has_dynamic_frame = true;
                    try self.emitDyn(staged, .{ .copy = .{ .src = current, .desc = dyn_info.desc } });
                    source.* = staged;
                } else {
                    const staged = try self.closedLocal(self.store.getLocal(current).layout_idx);
                    try self.emitLinear(.{ .assign_ref = .{ .target = staged, .op = LIR.RefOp.local(current), .next = undefined } });
                    source.* = staged;
                }
            }
            for (m.params, sources) |param, source| {
                try self.emitLinear(.{ .set_local = .{ .target = param, .value = source, .mode = .initialize_join_param, .next = undefined } });
            }
        }
        try self.emitTerminal(.{ .jump = .{ .target = id } });
    }

    /// Emit `switch (cond) { 1 => then } else otherwise` and return the two
    /// holes.
    pub fn emitBoolSwitch(self: *Builder, cond: LocalId) Allocator.Error!struct { then: CFStmtId, otherwise: CFStmtId } {
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
    /// The members of a recursive type that its layout's boxed slots point
    /// at are opened as placeholders first, and each is closed once its own
    /// components are composed.
    pub fn descOf(self: *Builder, root: TypeId) Allocator.Error!LocalId {
        if (self.type_descs.get(root)) |existing| return existing;
        const Step = enum { fresh, expanded, close_fresh, close_expanded };
        var pending: std.ArrayList(struct { ty: TypeId, step: Step }) = .empty;
        defer pending.deinit(self.allocator);
        var expanding: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer expanding.deinit(self.allocator);
        try pending.append(self.allocator, .{ .ty = root, .step = .fresh });
        while (pending.items.len > 0) {
            const top = &pending.items[pending.items.len - 1];
            const ty = top.ty;
            switch (top.step) {
                .fresh, .close_fresh => {
                    if (top.step == .fresh) {
                        if (self.type_descs.contains(ty)) {
                            _ = pending.pop();
                            continue;
                        }
                        if (try self.leafDesc(ty)) |local| {
                            try self.type_descs.put(self.allocator, ty, local);
                            _ = pending.pop();
                            continue;
                        }
                        if (try self.program.layouts.recursiveComponent(ty)) |component| {
                            if (!self.opened_components.contains(component)) {
                                try self.opened_components.put(self.allocator, component, {});
                                const targets = self.program.layouts.componentTargets(component);
                                for (targets) |target| try self.openRecursiveMember(target);
                                for (targets) |target| try pending.append(self.allocator, .{ .ty = target, .step = .close_fresh });
                                continue;
                            }
                        }
                        // A nominal whose override instances mention the
                        // nominal (a method generic in its receiver) captures
                        // its own descriptor: it is composed as a recursive
                        // member, its placeholder standing for it meanwhile.
                        if (try self.methodsMention(ty)) {
                            try self.openRecursiveMember(ty);
                            top.step = .close_fresh;
                            continue;
                        }
                    }
                    top.step = if (top.step == .fresh) .expanded else .close_expanded;
                    try expanding.put(self.allocator, ty, {});
                    const children = try self.descComponents(ty);
                    defer self.allocator.free(children);
                    for (children) |child| {
                        if (self.type_descs.contains(child)) continue;
                        if (expanding.contains(child)) invariant("a descriptor composition reached a type it is composing outside its layout's boxed slots");
                        try pending.append(self.allocator, .{ .ty = child, .step = .fresh });
                    }
                },
                .expanded => {
                    _ = expanding.remove(ty);
                    const local = try self.composeDesc(ty);
                    try self.type_descs.put(self.allocator, ty, local);
                    _ = pending.pop();
                },
                .close_expanded => {
                    _ = expanding.remove(ty);
                    const desc = try self.composeDesc(ty);
                    const open = self.open_members.fetchRemove(ty).?.value;
                    const closed_local = try self.closedLocal(.opaque_ptr);
                    try self.emitEntry(.{ .dyn = .{ .target = closed_local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_recursive_close = .{
                        .shell = self.type_descs.get(ty).?,
                        .desc = desc,
                        .key = open.key,
                        .vars = open.vars,
                    } }), .next = undefined } });
                    _ = pending.pop();
                },
            }
        }
        return self.type_descs.get(root).?;
    }

    /// Whether the nominal `ty`'s overrides are instantiated at types that
    /// mention `ty`.
    fn methodsMention(self: *Builder, ty: TypeId) Allocator.Error!bool {
        const store = &self.program.store;
        if (store.get(ty) != .nominal) return false;
        const overrides = try self.program.nominalOverrides(ty);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        for (overrides) |maybe| switch (maybe orelse continue) {
            .procedure => |override| {
                try pending.appendSlice(self.allocator, override.bound);
                for (override.dicts) |requirement| try pending.appendSlice(self.allocator, &.{ requirement.dispatcher, requirement.callable });
            },
            .local => |override| try pending.append(self.allocator, override.interface),
        };
        while (pending.pop()) |current| {
            if (current == ty) return true;
            if ((try seen.getOrPut(self.allocator, current)).found_existing) continue;
            switch (store.get(current)) {
                .primitive, .empty_record, .variable => {},
                .record => |record| {
                    for (store.fieldSpan(record.fields)) |field| try pending.append(self.allocator, field.ty);
                    if (record.ext) |ext| try pending.append(self.allocator, ext);
                },
                .tuple => |items| try pending.appendSlice(self.allocator, store.span(items)),
                .tag_union => |tag_union| {
                    for (store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(self.allocator, store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(self.allocator, ext);
                },
                .list, .box => |child| try pending.append(self.allocator, child),
                .nominal => |nominal| try pending.appendSlice(self.allocator, store.nominalArgs(nominal)),
                .function => |function| {
                    try pending.appendSlice(self.allocator, store.span(function.args));
                    try pending.append(self.allocator, function.ret);
                },
            }
        }
        return false;
    }

    /// Open the placeholder descriptor of the recursive member `ty`, which
    /// stands for `ty`'s descriptor until `descOf` closes it.
    fn openRecursiveMember(self: *Builder, ty: TypeId) Allocator.Error!void {
        const key = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = key, .value = .{ .static_data = .{ .id = try self.program.static_descs.recursionKeySlot(ty) } }, .next = undefined } });
        const vars = try self.program.layouts.memberVars(ty);
        const var_descs = try self.allocator.alloc(LocalId, vars.len);
        defer self.allocator.free(var_descs);
        for (vars, var_descs) |var_ty, *desc| desc.* = try self.leafDesc(var_ty) orelse invariant("a recursive member's variable has no descriptor in scope");
        const vars_span = try self.store.addLocalSpan(var_descs);
        const shell = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = shell, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_recursive_open = .{ .key = key, .vars = vars_span } }), .next = undefined } });
        try self.type_descs.put(self.allocator, ty, shell);
        try self.open_members.put(self.allocator, ty, .{ .key = key, .vars = vars_span });
    }

    /// The descriptor a composition of `parent` stores for its inline
    /// component `edge`: the component's own, or the recursion box around it
    /// in a slot `parent`'s committed layout boxes.
    fn slotDesc(self: *Builder, parent: TypeId, edge: Layouts.InlineEdge) Allocator.Error!LocalId {
        const child = self.type_descs.get(edge.child).?;
        if (try self.program.layouts.boxedEdge(parent, edge)) {
            if (self.recursive_boxes.get(child)) |existing| return existing;
            const boxed = try self.closedLocal(.opaque_ptr);
            try self.emitEntry(.{ .dyn = .{ .target = boxed, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_recursive_box = .{ .payload = child } }), .next = undefined } });
            try self.recursive_boxes.put(self.allocator, child, boxed);
            return boxed;
        }
        if (self.open_members.contains(edge.child)) invariant("a descriptor composition stores an open recursive member outside a boxed slot");
        return child;
    }

    /// A descriptor needing no component descriptors: a static descriptor,
    /// or a scope variable's hidden parameter.
    pub fn leafDesc(self: *Builder, ty: TypeId) Allocator.Error!?LocalId {
        const store = &self.program.store;
        if (try self.closed(ty) and !try self.program.composedInBody(ty)) return try self.staticDesc(ty);
        return switch (store.get(ty)) {
            .variable => |index| self.var_descs[index],
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => null,
        };
    }

    pub fn staticDesc(self: *Builder, ty: TypeId) Allocator.Error!LocalId {
        if (self.type_descs.get(ty)) |existing| return existing;
        const slot = try self.program.static_descs.descriptorSlot(ty);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.type_descs.put(self.allocator, ty, local);
        return local;
    }

    pub fn nameDesc(self: *Builder, name: types.NameId) Allocator.Error!LocalId {
        if (self.name_locals.get(name)) |existing| return existing;
        const slot = try self.program.static_descs.nameSlot(name);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = slot } }, .next = undefined } });
        try self.name_locals.put(self.allocator, name, local);
        return local;
    }

    /// The types whose descriptors compose `ty`'s. Caller frees.
    pub fn descComponents(self: *Builder, ty: TypeId) Allocator.Error![]TypeId {
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
    pub fn composeDesc(self: *Builder, ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const local = try self.closedLocal(.opaque_ptr);
        if (self.program.layouts.presenceSlotPayload(ty)) |payload| {
            try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_presence_slot = .{
                .missing = try self.nameDesc(try store.internName(@import("layouts.zig").missing_label)),
                .present = try self.nameDesc(try store.internName(@import("layouts.zig").present_label)),
                .payload = try self.slotDesc(ty, .{ .child = payload, .slot = .payload }),
            } }), .next = undefined } });
            return local;
        }
        const op: LIR.DynOp = switch (store.get(ty)) {
            .list => |elem| .{ .desc_list = .{ .elem = self.type_descs.get(elem).? } },
            .box => |payload| .{ .desc_box = .{ .payload = self.type_descs.get(payload).? } },
            .tuple => |items_span| blk: {
                const items = try self.allocator.dupe(TypeId, store.span(items_span));
                defer self.allocator.free(items);
                const descs = try self.allocator.alloc(LocalId, items.len);
                defer self.allocator.free(descs);
                for (items, descs) |item, *desc| desc.* = try self.slotDesc(ty, .{ .child = item, .slot = .field });
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
                    desc.* = try self.slotDesc(ty, .{ .child = try self.program.layouts.fieldStorage(field), .slot = .field });
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
                    const tag_payloads = try self.allocator.dupe(TypeId, store.span(tag.payloads));
                    defer self.allocator.free(tag_payloads);
                    try counts.append(self.allocator, @intCast(tag_payloads.len));
                    for (tag_payloads) |payload| try payloads.append(self.allocator, try self.slotDesc(ty, .{
                        .child = payload,
                        .slot = if (tag_payloads.len == 1) .payload else .field,
                    }));
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
                    .backing = try self.slotDesc(ty, .{ .child = try self.program.converter().backing(ty), .slot = .backing }),
                    .name = try self.nameDesc(nominal.name),
                    .methods = try self.composedMethods(ty),
                    .inspect_opaque = nominal.is_opaque,
                } };
            },
            .primitive, .empty_record, .function, .variable => unreachable, // `leafDesc` answers these
        };
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, op), .next = undefined } });
        return local;
    }

    /// The `Methods` record of the nominal type `ty`'s overrides, composed
    /// at runtime from the descriptors of the types each method's scheme
    /// takes, or null when it has none.
    pub fn composedMethods(self: *Builder, ty: TypeId) Allocator.Error!?LocalId {
        const overrides = try self.program.nominalOverrides(ty);
        if (overrides[0] == null and overrides[1] == null and overrides[2] == null) return null;
        var callables: [3]LocalId = undefined;
        for (overrides, &callables) |maybe, *callable| {
            const override = maybe orelse {
                callable.* = try self.closedLocal(.opaque_ptr);
                try self.emitEntry(.{ .assign_literal = .{ .target = callable.*, .value = .{ .static_data = .{ .id = try self.program.static_descs.nullWordSlot() } }, .next = undefined } });
                continue;
            };
            switch (override) {
                .procedure => |procedure| {
                    const hidden = try self.allocator.alloc(LocalId, procedure.bound.len + procedure.dicts.len);
                    defer self.allocator.free(hidden);
                    for (procedure.bound, hidden[0..procedure.bound.len]) |bound_ty, *desc| desc.* = try self.descOf(bound_ty);
                    for (procedure.dicts, hidden[procedure.bound.len..]) |requirement, *local| local.* = try self.dictionaryFromType(requirement);
                    callable.* = try self.composeDictionary(procedure.entry, hidden);
                },
                // A local procedure's function value is built in the entry
                // chain, where the descriptor that holds it is composed.
                .local => |local| {
                    if (local.module != self.module) invariant("a local-procedure override's descriptor is composed outside its declaring module");
                    const saved = self.immortal_closures;
                    self.immortal_closures = true;
                    defer self.immortal_closures = saved;
                    callable.* = try self.emitIntoEntry(localProcInstanceValueAt, .{ local.binding, local.interface });
                },
            }
        }
        const methods = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = methods, .data = try self.store.addStmtRow(LIR.DynOp, .{ .dict_record = .{ .methods = try self.store.addLocalSpan(&callables) } }), .next = undefined } });
        return methods;
    }

    // -- Body --

    pub fn lowerBody(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const template = switch (worker.key) {
            .template => |key| self.view.view.checked_procedure_templates.templates.items[@backingInt(key.template)],
            .lambda => |key| return try self.lowerLambdaBody(key.expr),
            .adapter => |key| return try self.lowerAdapterBody(key.from),
            .method_entry => |target| return try self.lowerMethodEntry(target),
            .structural => |method| return try self.lowerStructuralMethod(method),
            .structural_map => |map| return try self.lowerStructuralMap(map),
            .root => return try self.lowerRootWrapper(),
            .static_data => return try self.lowerStaticDataInitializer(),
            .default => |key| return try self.lowerDefaultBody(key.expr),
            .codec => |piece| return try Codec.lowerPiece(self, piece),
            .crash_dictionary => |kind| return try self.lowerCrashDictionary(kind),
            .convert => |key| return try self.lowerConvertBody(key.from, key.to),
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
        if (try self.closed(worker.ret)) {
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
                        for (lambda.args, params, worker.params) |pattern, param, param_ty| try self.bindIrrefutable(pattern, param, param_ty);
                        try self.tasks.append(self.allocator, .ret);
                        // The body's value is built at the function's result
                        // type, which its rows join (design.md "Row Joins").
                        try self.destination_types.put(self.allocator, lambda.body, worker.ret);
                        try self.tail_exprs.put(self.allocator, lambda.body, {});
                        try self.tasks.append(self.allocator, .{ .expr = lambda.body });
                    },
                    .hosted_lambda => {
                        // A hosted procedure is an extern at its declaration's
                        // own type (design.md "Host ABI").
                        if (worker.dictionary_params.len != 0) invariant("a hosted declaration has no where requirements, which checking rejects");
                        const key = worker.key.template;
                        const hosted = self.program.hosted.get(.{ .module = key.module, .template = key.template }) orelse
                            invariant("a hosted procedure has no host binding");
                        if (hosted.dispatch_index) |dispatch_index| {
                            if (worker.scheme_vars.len == 0) {
                                const proc = self.store.getProcSpecPtr(worker.proc);
                                proc.args = try self.store.addLocalSpan(params);
                                proc.ret_layout = ret_layout;
                                proc.hosted = .{ .symbol = try self.store.insertString(hosted.symbol), .dispatch_index = dispatch_index };
                                return;
                            }
                            try self.callGenericExtern(hosted.symbol, dispatch_index, params, worker.params, worker.ret);
                        } else {
                            // The platform's hosted section omits it: it has
                            // no host slot, as checking reported.
                            const message = try self.store.insertString("a hosted function the platform does not provide was called");
                            try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = message }, .checked_error = true } });
                        }
                    },
                    .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .run_low_level => {
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
            .intrinsic_wrapper => |wrapper| try Codec.lowerIntrinsic(self, self.view.view.intrinsic_wrappers.get(wrapper).intrinsic, params),
            .entry_wrapper => |wrapper_id| {
                const wrapper = self.view.view.entry_wrappers.get(wrapper_id);
                const root = self.view.view.compile_time_roots.root(wrapper.root);
                switch (root.kind) {
                    .expect, .repl_expr, .callable_binding, .constant, .hoisted_constant => {},
                    .hoisted_validation => invariant("a validation root's entry wrapper produces no value for a runtime read to reach"),
                    .numeral_conversion, .quote_conversion => invariant("a literal conversion root's value is read from its literal table, never by calling its entry wrapper"),
                }
                if (params.len != 0) invariant("an entry wrapper has parameters");
                self.own_root = wrapper.root;
                try self.tasks.append(self.allocator, .ret);
                try self.tasks.append(self.allocator, .{ .expr = wrapper.body_expr });
            },
        }
        try self.run();

        try self.finishProc(entry, body_start, args.items, ret_layout);
    }

    /// The body of a generic hosted worker: a direct call of the extern at
    /// the declaration's own type. A variable slot sits inside a `Box`, a
    /// pointer whatever the variable is, so each value crosses as the bytes
    /// the calling Roc code holds, and the extern takes no descriptor
    /// (design.md "Host ABI").
    fn callGenericExtern(self: *Builder, symbol: []const u8, dispatch_index: u32, params: []const LocalId, param_types: []const TypeId, ret_ty: TypeId) Allocator.Error!void {
        const extern_args = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(extern_args);
        const extern_params = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(extern_params);
        for (params, param_types, extern_args, extern_params) |param, param_ty, *arg, *extern_param| {
            const abi = try self.hostAbiLayout(param_ty);
            extern_param.* = try self.closedLocal(abi);
            if (try self.closed(param_ty)) {
                arg.* = param;
                continue;
            }
            arg.* = try self.closedLocal(abi);
            try self.emitDyn(arg.*, .{ .copy = .{ .src = param, .desc = try self.descOf(param_ty) } });
        }
        const ret_abi = try self.hostAbiLayout(ret_ty);
        const extern_proc = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.hosted_extern", @backingInt(self.worker_id)),
            .emission_unit = .isolated,
            .args = try self.store.addLocalSpan(extern_params),
            .ret_layout = ret_abi,
            .hosted = .{ .symbol = try self.store.insertString(symbol), .dispatch_index = dispatch_index },
        }, base.SourceLoc.none);
        const result = try self.closedLocal(ret_abi);
        try self.emitLinear(.{ .assign_call = .{ .target = result, .proc = extern_proc, .args = try self.store.addLocalSpan(extern_args), .next = undefined } });
        if (try self.closed(ret_ty)) {
            try self.pushValue(result);
        } else {
            const value = try self.localOf(ret_ty, .frame);
            try self.emitDyn(value, .{ .copy = .{ .src = result, .desc = try self.descOf(ret_ty) } });
            try self.pushValue(value);
        }
        try self.step(.ret);
    }

    /// The layout a hosted declaration's type `root` has at the host: its
    /// committed layout where it is closed, and where a `Box` holds one of
    /// the declaration's variables, the opaque box `RocBox(RocUnknown)`
    /// (design.md "Host ABI"). The worker's own values of such a box are
    /// dynamic locals, and each crossing copies between the two, so a local
    /// of the opaque box is never the last owner of its allocation.
    fn hostAbiLayout(self: *Builder, root: TypeId) Allocator.Error!layout.Idx {
        const store = &self.program.store;
        const layouts = &self.program.result.layouts;
        var done: collections.DenseMapUnmanaged(TypeId, layout.Idx) = .empty;
        defer done.deinit(self.allocator);
        const Frame = struct { ty: TypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(self.allocator);
        var children: std.ArrayList(TypeId) = .empty;
        defer children.deinit(self.allocator);
        try stack.append(self.allocator, .{ .ty = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (done.contains(frame.ty)) continue;
            if (try self.closed(frame.ty)) {
                try done.put(self.allocator, frame.ty, try self.program.layouts.layoutOf(frame.ty));
                continue;
            }
            children.clearRetainingCapacity();
            switch (store.get(frame.ty)) {
                .box => |payload| if (!try self.closed(payload)) {
                    try done.put(self.allocator, frame.ty, try layouts.insertBox(.opaque_ptr));
                    continue;
                } else try children.append(self.allocator, payload),
                .nominal => try children.append(self.allocator, try self.program.converter().backing(frame.ty)),
                .tuple => |items| try children.appendSlice(self.allocator, store.span(items)),
                .record => |record| {
                    if (record.ext != null) invariant("a hosted declaration's record row is closed, as checking requires");
                    const fields = try self.allocator.dupe(types.Field, store.fieldSpan(record.fields));
                    defer self.allocator.free(fields);
                    for (fields) |field| try children.append(self.allocator, try self.program.layouts.fieldStorage(field));
                },
                .tag_union => |tag_union| {
                    if (tag_union.ext != null) invariant("a hosted declaration's tag row is closed, as checking requires");
                    const tags = try self.allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
                    defer self.allocator.free(tags);
                    for (tags) |tag| {
                        const payloads = try self.allocator.dupe(TypeId, store.span(tag.payloads));
                        defer self.allocator.free(payloads);
                        if (try self.program.layouts.tagPayload(payloads)) |payload| try children.append(self.allocator, payload);
                    }
                },
                .variable => invariant("a hosted declaration's variable appears only inside a Box, as checking requires"),
                .list => invariant("a hosted declaration's variable inside a List is outside a Box, which checking rejects"),
                .function, .primitive, .empty_record => unreachable, // closed
            }
            if (!frame.expanded) {
                try stack.append(self.allocator, .{ .ty = frame.ty, .expanded = true });
                for (children.items) |child| if (!done.contains(child)) try stack.append(self.allocator, .{ .ty = child, .expanded = false });
                continue;
            }
            const child_layouts = try self.allocator.alloc(layout.Idx, children.items.len);
            defer self.allocator.free(child_layouts);
            for (children.items, child_layouts) |child, *child_layout| child_layout.* = done.get(child).?;
            const built: layout.Idx = switch (store.get(frame.ty)) {
                .box => try layouts.insertBox(child_layouts[0]),
                .nominal => child_layouts[0],
                .tuple, .record => blk: {
                    const fields = try self.allocator.alloc(layout.StructField, child_layouts.len);
                    defer self.allocator.free(fields);
                    for (fields, child_layouts, 0..) |*field, child_layout, index| field.* = .{ .index = @intCast(index), .layout = child_layout };
                    break :blk try layouts.putStructFields(fields);
                },
                .tag_union => |tag_union| blk: {
                    const tags = try self.allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
                    defer self.allocator.free(tags);
                    const variants = try self.allocator.alloc(layout.Idx, tags.len);
                    defer self.allocator.free(variants);
                    var next: usize = 0;
                    for (tags, variants) |tag, *variant| {
                        if (tag.payloads.len == 0) {
                            variant.* = .zst;
                        } else {
                            variant.* = child_layouts[next];
                            next += 1;
                        }
                    }
                    break :blk try layouts.putTagUnion(variants);
                },
                .primitive, .empty_record, .list, .function, .variable => unreachable, // only the composite kinds above expand
            };
            try done.put(self.allocator, frame.ty, built);
        }
        return done.get(root).?;
    }

    /// Link the entry chain to the body and finish the procedure.
    pub fn finishProc(self: *Builder, entry: CFStmtId, body_start: CFStmtId, args: []const LocalId, ret_layout: layout.Idx) Allocator.Error!void {
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
        if (self.has_dynamic_frame) self.tail_calls.self_calls = .replace_frame;
        proc.tail_calls = try self.tail_calls.finish(self.store);
    }

    /// Crash with a checked runtime error; the value of type `ty` it stands
    /// for is never produced. A value whose type holds the erroneous type
    /// has no type of its own, and no reachable statement reads the
    /// zero-sized local standing for it.
    pub fn runtimeErrorValue(self: *Builder, ty: ?TypeId) Allocator.Error!void {
        const literal = try self.store.insertString("runtime error");
        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
        try self.pushValue(if (ty) |value_ty| try self.localOf(value_ty, .frame) else try self.closedLocal(.zst));
    }

    /// A lambda worker in the uniform erased-callable ABI: its one argument
    /// is the packed argument record, and its captures hold the captured
    /// values followed by the outer worker's descriptors.
    pub fn lowerLambdaBody(self: *Builder, lambda_expr: CheckedExprId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const lambda = switch (self.bodies().expr(lambda_expr).data) {
            .lambda => |lambda| lambda,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a lambda worker's expression is not a lambda"),
        };
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;

        // Captures: values, then the outer scope's descriptors and
        // dictionaries.
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        const outer_descs = worker.scheme_vars.len - worker.own_descs;
        const captured_dicts = worker.capturedDictionaryCount();
        self.var_descs = try self.program.arena.allocator().alloc(LocalId, worker.scheme_vars.len);
        self.dict_locals = try self.program.arena.allocator().alloc(LocalId, worker.own_dicts + captured_dicts);
        const capture_count = worker.capture_types.len + outer_descs + captured_dicts;
        var capture_values: []LocalId = &.{};
        if (!try self.allClosed(worker.capture_types)) {
            capture_values = try self.dynamicCaptures(worker, capture_ptr);
        } else if (capture_count != 0) {
            const capture_layout = try self.captureLayout(worker.capture_types, outer_descs, captured_dicts);
            // A record of zero-sized captures has no storage to read.
            const record: ?LocalId = if (capture_layout == .zst) null else try self.closedLocal(capture_layout);
            if (record) |loaded| try self.emitEntry(.{ .assign_low_level = .{
                .target = loaded,
                .op = .erased_capture_load,
                .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
                .args = try self.store.addLocalSpan(&.{capture_ptr}),
                .next = undefined,
            } });
            capture_values = try self.allocator.alloc(LocalId, capture_count);
            for (capture_values, 0..) |*slot, index| {
                const field_layout = if (index < worker.capture_types.len)
                    try self.program.layouts.layoutOf(worker.capture_types[index])
                else if (index < worker.capture_types.len + outer_descs)
                    layout.Idx.opaque_ptr
                else
                    try self.erasedLayout();
                slot.* = try self.closedLocal(field_layout);
                if (record) |loaded| {
                    try self.emitEntry(.{ .assign_ref = .{ .target = slot.*, .op = LIR.RefOp.field(loaded, @intCast(index)), .next = undefined } });
                } else {
                    if (field_layout != .zst) invariant("a zero-sized capture record holds a capture with storage");
                    try self.emitEntry(.{ .assign_struct = .{ .target = slot.*, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                }
            }
        }
        if (capture_count != 0) {
            @memcpy(self.var_descs[0..outer_descs], capture_values[worker.capture_types.len..][0..outer_descs]);
            @memcpy(self.dict_locals[worker.own_dicts..], capture_values[worker.capture_types.len + outer_descs ..]);
        }
        defer if (capture_count != 0) self.allocator.free(capture_values);
        for (worker.captures, worker.capture_types, 0..) |capture, capture_ty, index| {
            const binder = self.bodies().pattern_binder_by_pattern[@backingInt(capture.pattern)].get() orelse
                invariant("a closure capture named a pattern without a binder");
            if (self.bodies().patternBinder(binder).recursive_value) {
                // A recursive value's slot: the value is read from it on
                // entry, and the slot stays in scope for the callables this
                // body builds that capture the binder too.
                try self.recursive_slots.put(self.allocator, binder, capture_values[index]);
                try self.binders.put(self.allocator, binder, try self.readRecursiveSlot(capture_values[index], capture_ty));
            } else {
                try self.binders.put(self.allocator, binder, capture_values[index]);
            }
        }

        // The argument record.
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        var record_param: ?LocalId = null;
        const own_words = worker.own_descs + worker.own_dicts;
        if (own_words != 0) {
            record_param = try self.ownScopeArgs(worker, outer_descs);
            try args.append(self.allocator, record_param.?);
        } else if (worker.params.len != 0) {
            record_param = try self.argsRecordLocal(worker.params, .param);
            try args.append(self.allocator, record_param.?);
        }
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }

        const body_start = try self.newHole();
        self.hole = body_start;
        if (lambda.args.len != worker.params.len) invariant("a lambda's arity differs from its type");
        for (lambda.args, worker.params, 0..) |pattern, param_ty, index| {
            const value = if (own_words != 0) blk: {
                const local = try self.localOf(param_ty, .view);
                try self.emitDyn(local, .{ .field_addr = .{ .base = record_param.?, .desc = self.store.localDyn(record_param.?).?.desc, .field_index = @intCast(own_words + index) } });
                break :blk local;
            } else try self.argsRecordField(record_param.?, @intCast(index), param_ty);
            try self.bindIrrefutable(pattern, value, param_ty);
        }
        try self.tasks.append(self.allocator, .ret);
        try self.destination_types.put(self.allocator, lambda.body, worker.ret);
        try self.tail_exprs.put(self.allocator, lambda.body, {});
        try self.tasks.append(self.allocator, .{ .expr = lambda.body });
        try self.run();
        try args.append(self.allocator, capture_ptr);
        try self.finishProc(entry, body_start, args.items, ret_layout);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = capture_ptr;
        proc.abi = .erased_record;
    }

    /// An adapter's body (`WorkerKey.adapter`): each argument adapted to the
    /// called function's storage, the call, and its result adapted back.
    pub fn lowerAdapterBody(self: *Builder, from: TypeId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        const capture_ptr = try self.closedLocal(.opaque_ptr);
        const descs = worker.scheme_vars.len;
        self.var_descs = try self.program.arena.allocator().alloc(LocalId, descs);
        self.dict_locals = &.{};
        const capture_layout = try self.captureLayout(worker.capture_types, descs, 0);
        const record = try self.closedLocal(capture_layout);
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        const inner = try self.closedLocal(try self.erasedLayout());
        try self.emitEntry(.{ .assign_ref = .{ .target = inner, .op = LIR.RefOp.field(record, 0), .next = undefined } });
        for (self.var_descs, 0..) |*desc, index| {
            desc.* = try self.closedLocal(.opaque_ptr);
            try self.emitEntry(.{ .assign_ref = .{ .target = desc.*, .op = LIR.RefOp.field(record, @intCast(1 + index)), .next = undefined } });
        }
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        const record_param: ?LocalId = if (worker.params.len != 0) try self.argsRecordLocal(worker.params, .param) else null;
        if (record_param) |param| try args.append(self.allocator, param);
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(worker.ret)) {
            ret_layout = try self.program.layouts.layoutOf(worker.ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(worker.ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        const signature = switch (self.program.store.get(from)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an adapted function value's type is not a function type"),
        };
        const from_params = try self.allocator.dupe(TypeId, self.program.store.span(signature.args));
        defer self.allocator.free(from_params);
        const adapted = try self.allocator.alloc(LocalId, worker.params.len);
        defer self.allocator.free(adapted);
        for (worker.params, from_params, adapted, 0..) |param_ty, from_ty, *value, index| {
            const arg = try self.argsRecordField(record_param.?, @intCast(index), param_ty);
            value.* = try self.adaptValue(arg, param_ty, from_ty);
        }
        const result = try self.emitErasedCall(inner, from, adapted, signature.ret);
        try self.pushValue(try self.adaptValue(result, signature.ret, worker.ret));
        try self.tasks.append(self.allocator, .ret);
        try self.run();
        try args.append(self.allocator, capture_ptr);
        try self.finishProc(entry, body_start, args.items, ret_layout);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = capture_ptr;
        proc.abi = .erased_record;
    }

    /// Where two types of one value differ: nowhere; only in the tags of
    /// rows, one widening the other along the coercion's reach (design.md
    /// "Sealed Result Rows"); in the storage of record fields (design.md
    /// "Field Kinds"), with or without such rows; or inside the function
    /// type the value has.
    pub const StorageDifference = enum { none, rows, slots, function };

    /// A function value inside data is outside every row widening's reach
    /// (design.md "Row Coercion Primitive") and function types relate
    /// exactly, never by a construction relation that absorbs omitted
    /// optional fields (design.md "Field Kinds"), so both types of the value
    /// store such a function's signature alike.
    const nested_function_storage = "two types of one value store the signature of a function value inside data differently";

    pub fn storageDifference(self: *Builder, from_root: TypeId, to_root: TypeId) Allocator.Error!StorageDifference {
        if (from_root == to_root) return .none;
        const store = &self.program.store;
        // Where a pair sits: in data, in the signature of the root function
        // or of a function its signature passes (each adapted by its own
        // adapter), in data such a signature passes, or in a function type
        // inside data.
        const Place = enum { data, root_function, signature_data, nested_function };
        const Pair = struct { from: TypeId, to: TypeId, place: Place };
        var pending: std.ArrayList(Pair) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .from = from_root, .to = to_root, .place = .data });
        var result: StorageDifference = .none;
        while (pending.pop()) |pair| {
            if (pair.from == pair.to) continue;
            const from = store.get(pair.from);
            const to = store.get(pair.to);
            if ((from == .variable) != (to == .variable)) {
                // A value whose type a variable of the body leaves open, flowing
                // into a tag row or out of one (a `?` return's error into the
                // function's composed row): the variable's tags are known
                // only at run time, so the value is re-tagged by name under
                // the destination's descriptor. Elsewhere a variable has the
                // one representation every type has.
                if (pair.place == .data and try self.isTagRow(if (from == .variable) pair.to else pair.from)) {
                    if (result == .none) result = .rows;
                }
                continue;
            }
            if (from == .variable) continue;
            if (std.meta.activeTag(from) != std.meta.activeTag(to)) {
                // A nominal shares its backing's representation.
                if (from == .nominal and from.nominal.has_backing) {
                    try pending.append(self.allocator, .{ .from = try self.program.converter().backing(pair.from), .to = pair.to, .place = pair.place });
                } else if (to == .nominal and to.nominal.has_backing) {
                    try pending.append(self.allocator, .{ .from = pair.from, .to = try self.program.converter().backing(pair.to), .place = pair.place });
                }
                continue;
            }
            // The place of a component of a non-function type.
            const inner: Place = if (pair.place == .root_function) .signature_data else pair.place;
            switch (from) {
                .record => |record| {
                    for (store.fieldSpan(to.record.fields)) |to_field| {
                        // A closed record lacking an optional field the
                        // destination stores gets it missing.
                        const present = for (store.fieldSpan(record.fields)) |from_field| {
                            if (from_field.name == to_field.name) break true;
                        } else false;
                        if (!present and record.ext == null and to_field.kind == .presence_slot) switch (pair.place) {
                            .data => result = .slots,
                            .root_function, .signature_data => return .function,
                            .nested_function => invariant(nested_function_storage),
                        };
                        for (store.fieldSpan(record.fields)) |from_field| {
                            if (from_field.name != to_field.name) continue;
                            if (from_field.kind != to_field.kind) switch (pair.place) {
                                .data => result = .slots,
                                .root_function, .signature_data => return .function,
                                .nested_function => invariant(nested_function_storage),
                            };
                            try pending.append(self.allocator, .{ .from = from_field.ty, .to = to_field.ty, .place = inner });
                        }
                    }
                    if (record.ext != null and to.record.ext != null) try pending.append(self.allocator, .{ .from = record.ext.?, .to = to.record.ext.?, .place = inner });
                },
                .tuple => |items| for (store.span(items), store.span(to.tuple)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = inner }),
                .tag_union => |tag_union| {
                    // A row the destination widens holds its tags under
                    // other discriminants: the value is converted.
                    if (tag_union.tags.len != to.tag_union.tags.len or (tag_union.ext == null) != (to.tag_union.ext == null)) switch (pair.place) {
                        .data => if (result == .none) {
                            result = .rows;
                        },
                        .root_function, .signature_data => return .function,
                        .nested_function => invariant(nested_function_storage),
                    };
                    for (store.tagSpan(to.tag_union.tags)) |to_tag| {
                        for (store.tagSpan(tag_union.tags)) |from_tag| {
                            if (from_tag.name != to_tag.name or from_tag.payloads.len != to_tag.payloads.len) continue;
                            for (store.span(from_tag.payloads), store.span(to_tag.payloads)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = inner });
                        }
                    }
                    if (tag_union.ext != null and to.tag_union.ext != null) try pending.append(self.allocator, .{ .from = tag_union.ext.?, .to = to.tag_union.ext.?, .place = inner });
                },
                .list => |elem| try pending.append(self.allocator, .{ .from = elem, .to = to.list, .place = inner }),
                .box => |payload| try pending.append(self.allocator, .{ .from = payload, .to = to.box, .place = inner }),
                .nominal => |nominal| for (store.nominalArgs(nominal), store.nominalArgs(to.nominal)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = inner }),
                .function => |function| {
                    const place: Place = switch (pair.place) {
                        .data => if (pair.from == from_root) .root_function else .nested_function,
                        .root_function => .root_function,
                        .signature_data, .nested_function => .nested_function,
                    };
                    for (store.span(function.args), store.span(to.function.args)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = place });
                    try pending.append(self.allocator, .{ .from = function.ret, .to = to.function.ret, .place = place });
                },
                .primitive, .empty_record, .variable => {},
            }
        }
        return result;
    }

    /// Whether `ty` is a tag union, through the backing of a nominal.
    fn isTagRow(self: *Builder, ty: TypeId) Allocator.Error!bool {
        return switch (self.program.store.get(ty)) {
            .tag_union => true,
            .nominal => |nominal| nominal.has_backing and try self.isTagRow(try self.program.converter().backing(ty)),
            .primitive, .empty_record, .record, .tuple, .list, .box, .function, .variable => false,
        };
    }

    /// The value `value` of type `from` at type `to`, a type of the same
    /// value whose rows may be wider (design.md "Sealed Result Rows") or
    /// whose record fields may be stored differently (design.md "Field
    /// Kinds"): itself where the representations agree; converted between
    /// two closed types whose rows alone differ by the module's conversion
    /// worker; rebuilt by the descriptor-driven `adapt` where a row the
    /// body opens or a field's storage differs; and a function value
    /// adapting the calls where a function's signature does.
    pub fn adaptValue(self: *Builder, value: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        switch (try self.storageDifference(from, to)) {
            .none => return value,
            .rows => {
                if (try self.closed(from) and try self.closed(to)) return try self.convertRows(value, from, to);
                const target = try self.localOf(to, .frame);
                try self.emitDyn(target, .{ .adapt = .{ .src = value, .from = try self.descOf(from), .to = try self.descOf(to) } });
                return target;
            },
            .slots => {
                const target = try self.localOf(to, .frame);
                try self.emitDyn(target, .{ .adapt = .{ .src = value, .from = try self.descOf(from), .to = try self.descOf(to) } });
                return target;
            },
            .function => {
                const adapter = try self.program.adapterWorker(self.worker_id, from, to);
                return try self.packClosure(adapter, &.{value}, &.{from}, self.var_descs, &.{});
            },
        }
    }

    /// `value` of the closed type `from` converted to the closed type `to`,
    /// whose rows widen `from`'s (design.md "Sealed Result Rows"): itself
    /// when the two types commit one layout, otherwise the result of this
    /// module's conversion worker for the pair.
    pub fn convertRows(self: *Builder, value: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        if (try self.program.layouts.layoutOf(from) == try self.program.layouts.layoutOf(to)) return value;
        const worker = try self.program.convertWorker(self.module, from, to);
        const target = try self.localOf(to, .frame);
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(worker).proc, .args = try self.store.addLocalSpan(&.{value}), .next = undefined } });
        return target;
    }

    /// A conversion worker's body (`WorkerKey.convert`): its one parameter
    /// converted to its result type along the coercion's reach.
    pub fn lowerConvertBody(self: *Builder, from: TypeId, to: TypeId) Allocator.Error!void {
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const param = try self.localOf(from, .param);
        const ret_layout = try self.program.layouts.layoutOf(to);
        const body_start = try self.newHole();
        self.hole = body_start;
        const result = try self.convertValue(param, from, to);
        if (self.hole != null) try self.emitTerminal(.{ .ret = .{ .value = result } });
        try self.finishProc(entry, body_start, &.{param}, ret_layout);
    }

    /// `value` of `from` rebuilt at `to`, one reached row at a time: a
    /// switch on the source discriminant whose arm for each source variant
    /// builds the destination's variant of the same name, with each payload
    /// whose types differ converted in turn (`adaptValue`, which reaches
    /// the worker for that pair). Positions outside the reach are identical
    /// on both sides by the checker's rule.
    fn convertValue(self: *Builder, value: LocalId, from_raw: TypeId, to_raw: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const from = try self.structuralType(from_raw);
        const to = try self.structuralType(to_raw);
        const from_union = switch (store.get(from)) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a row conversion's source is not a tag union"),
        };
        switch (store.get(to)) {
            .tag_union => {},
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a row conversion's destination is not a tag union"),
        }
        // Building payloads interns types, which may grow the store under
        // its own spans.
        const from_tags = try self.allocator.dupe(types.Tag, store.tagSpan(from_union.tags));
        defer self.allocator.free(from_tags);
        if (from_tags.len == 0) {
            // No value has the empty row, so no conversion of one runs.
            try self.emitTerminal(.runtime_error);
            return try self.localOf(to_raw, .frame);
        }
        const result = try self.localOf(to_raw, .frame);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(from));
        const arms = try self.allocator.alloc(CFStmtId, from_tags.len);
        defer self.allocator.free(arms);
        if (from_tags.len > 1) {
            if (committed.tag != .tag_union) invariant("a tag union of several variants has no discriminant storage");
            const discriminant = try self.closedLocal(.u32);
            try self.emitLinear(.{ .assign_ref = .{ .target = discriminant, .op = LIR.RefOp.discriminant(value), .next = undefined } });
            for (arms) |*arm| arm.* = try self.newHole();
            const branches = try self.allocator.alloc(LIR.CFSwitchBranch, from_tags.len - 1);
            defer self.allocator.free(branches);
            for (branches, 0..) |*branch, index| branch.* = .{ .value = @intCast(index), .body = arms[index] };
            try self.emitTerminal(.{ .switch_stmt = .{ .cond = discriminant, .branches = try self.store.addCFSwitchBranches(branches), .default_branch = arms[from_tags.len - 1] } });
        } else {
            arms[0] = try self.currentHole();
        }
        for (from_tags, arms, 0..) |tag, arm, variant| {
            self.hole = arm;
            const payload_types = try self.allocator.dupe(TypeId, store.span(tag.payloads));
            defer self.allocator.free(payload_types);
            const destination = self.ownTag(to, tag.name, payload_types.len);
            const destination_payloads = try self.allocator.dupe(TypeId, store.span(destination.payloads));
            defer self.allocator.free(destination_payloads);
            const values = try self.allocator.alloc(LocalId, payload_types.len);
            defer self.allocator.free(values);
            for (values, payload_types, destination_payloads, 0..) |*slot, payload_ty, destination_ty, index| {
                const payload = if (committed.tag != .tag_union) blk: {
                    // A union without storage has only zero-sized payloads.
                    const payload_local = try self.localOf(payload_ty, .frame);
                    if (self.store.getLocal(payload_local).layout_idx != .zst) invariant("a storage-free union has a payload with storage");
                    try self.emitLinear(.{ .assign_struct = .{ .target = payload_local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                    break :blk payload_local;
                } else try self.readTagPayload(value, from, variant, payload_types, index);
                slot.* = try self.adaptValue(payload, payload_ty, destination_ty);
            }
            const built = try self.buildTag(to_raw, tag.name, values);
            try self.emitJump(join, built);
        }
        self.hole = after;
        return result;
    }

    /// Dictionary `dictionary` of type `from` adapted to `to`. A dictionary
    /// is composed in the entry chain and lives as long as the process, so
    /// its adapter is too.
    fn adaptDictionary(self: *Builder, dictionary: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        const saved = self.immortal_closures;
        self.immortal_closures = true;
        defer self.immortal_closures = saved;
        return try self.adaptValue(dictionary, from, to);
    }

    /// Read a dynamic capture record at `capture_ptr`: its descriptor word,
    /// the descriptor and dictionary words (which the captured values'
    /// descriptors compose from), then views of the captured values. Returns
    /// the capture values in the closed record's order: values, descriptors,
    /// dictionaries.
    /// The argument record of a generalized local procedure: its own
    /// scope's descriptors and dictionaries, which vary per call, then its
    /// explicit arguments. The leading words are read through a descriptor
    /// of just those words; the whole record's descriptor composes from them.
    pub fn ownScopeArgs(self: *Builder, worker: lower.Worker, outer_descs: usize) Allocator.Error!LocalId {
        const own_words = worker.own_descs + worker.own_dicts;
        const word = try self.opaqueWordDesc();
        const full_desc = try self.closedLocal(.opaque_ptr);
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = full_desc, .storage = .param } });
        const words = try self.allocator.alloc(LocalId, own_words);
        defer self.allocator.free(words);
        @memset(words, word);
        const prefix_desc = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = prefix_desc, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(words) } }), .next = undefined } });
        for (0..own_words) |index| {
            const local = try self.closedLocal(if (index < worker.own_descs) .opaque_ptr else try self.erasedLayout());
            try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = prefix_desc, .field_index = @intCast(index) } }), .next = undefined } });
            if (index < worker.own_descs) self.var_descs[outer_descs + index] = local else self.dict_locals[index - worker.own_descs] = local;
        }
        const elems = try self.allocator.alloc(LocalId, own_words + worker.params.len);
        defer self.allocator.free(elems);
        @memset(elems[0..own_words], word);
        for (elems[own_words..], worker.params) |*elem, param_ty| elem.* = try self.descOf(param_ty);
        try self.emitEntry(.{ .dyn = .{ .target = full_desc, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(elems) } }), .next = undefined } });
        return record;
    }

    pub fn dynamicCaptures(self: *Builder, worker: lower.Worker, capture_ptr: LocalId) Allocator.Error![]LocalId {
        const outer_descs = worker.scheme_vars.len - worker.own_descs;
        const word_count = outer_descs + worker.capturedDictionaryCount();
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
            const word = try self.closedLocal(if (index < outer_descs) .opaque_ptr else try self.erasedLayout());
            try self.emitEntry(.{ .dyn = .{ .target = word, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = record_desc, .field_index = @intCast(1 + index) } }), .next = undefined } });
            values[worker.capture_types.len + index] = word;
        }
        // The captured values' types mention only the outer scope.
        @memcpy(self.var_descs[0..outer_descs], values[worker.capture_types.len..][0..outer_descs]);
        for (worker.capture_types, 0..) |ty, index| {
            const local = try self.localOf(ty, .view);
            try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = record, .desc = record_desc, .field_index = @intCast(1 + word_count + index) } }), .next = undefined } });
            values[index] = local;
        }
        return values;
    }

    pub fn erasedLayout(self: *Builder) Allocator.Error!layout.Idx {
        return self.program.result.layouts.insertErasedCallable();
    }

    /// Load an erased worker's capture record (closed fields of
    /// `field_layouts`) at the start of its entry chain, returning its fields.
    pub fn loadCaptures(self: *Builder, capture_ptr: LocalId, field_layouts: []const layout.Idx) Allocator.Error![]LocalId {
        const fields = try self.allocator.alloc(layout.StructField, field_layouts.len);
        defer self.allocator.free(fields);
        for (fields, field_layouts, 0..) |*field, field_layout, index| field.* = .{ .index = @intCast(index), .layout = field_layout };
        const record_layout = try self.program.result.layouts.putStructFields(fields);
        const values = try self.allocator.alloc(LocalId, field_layouts.len);
        if (record_layout == .zst) {
            // Zero-sized captures are stored nowhere; each is built empty.
            for (values, field_layouts) |*value, field_layout| {
                if (field_layout != .zst) invariant("a zero-sized capture record holds a capture with storage");
                value.* = try self.closedLocal(.zst);
                try self.emitEntry(.{ .assign_struct = .{ .target = value.*, .fields = LIR.LocalSpan.empty(), .next = undefined } });
            }
            return values;
        }
        const record = try self.closedLocal(record_layout);
        try self.emitEntry(.{ .assign_low_level = .{
            .target = record,
            .op = .erased_capture_load,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.erased_capture_load.rcEffect()),
            .args = try self.store.addLocalSpan(&.{capture_ptr}),
            .next = undefined,
        } });
        for (values, field_layouts, 0..) |*value, field_layout, index| {
            value.* = try self.closedLocal(field_layout);
            try self.emitEntry(.{ .assign_ref = .{ .target = value.*, .op = LIR.RefOp.field(record, @intCast(index)), .next = undefined } });
        }
        return values;
    }

    /// Begin an erased-ABI worker whose captures are `field_layouts`: entry
    /// chain, capture pointer, argument record, and result. Returns what
    /// `finishErasedWorker` needs.
    pub const ErasedStart = struct {
        entry: CFStmtId,
        body_start: CFStmtId,
        capture_ptr: LocalId,
        captures: []LocalId,
        args: []LocalId,
        record: ?LocalId,
        ret_layout: layout.Idx,
    };

    pub fn beginErasedWorker(self: *Builder, field_layouts: []const layout.Idx) Allocator.Error!ErasedStart {
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
    /// A root's host-visible wrapper: its checked arguments, passed to the
    /// root's target with the hidden arguments the root's instantiation
    /// names, or to the function value compile-time evaluation stored.
    /// A static data initializer's body: the requested stored constant,
    /// restored at its type.
    pub fn lowerStaticDataInitializer(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const request = worker.static_data_request.?;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const ret_layout = try self.program.layouts.layoutOf(worker.ret);
        const body_start = try self.newHole();
        self.hole = body_start;
        const owner = self.program.moduleByKey(checked.constModuleId(request.const_locator));
        try self.tasks.append(self.allocator, .ret);
        switch (owner.view.const_templates.get(request.const_locator).state) {
            .stored_const => |stored| try self.tasks.append(self.allocator, .{ .restore = .{ .source = .{ .module = owner.index }, .node = request.node orelse stored.node, .ty = worker.ret } }),
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                try self.crashValue(@import("../common.zig").unimplemented_declaration_crash, worker.ret);
            },
            // A provided data export is a closed constant, which
            // compile-time evaluation stores before any runtime lowering.
            .eval_template, .reserved => invariant("a static data request names a constant compile-time evaluation did not store"),
        }
        try self.run();
        try self.finishProc(entry, body_start, &.{}, ret_layout);
    }

    pub fn lowerRootWrapper(self: *Builder) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const request = worker.root_request.?;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const params = try self.allocator.alloc(LocalId, worker.params.len);
        defer self.allocator.free(params);
        for (params, worker.params) |*param, param_ty| param.* = try self.localOf(param_ty, .param);
        const ret_layout = try self.program.layouts.layoutOf(worker.ret);
        const body_start = try self.newHole();
        self.hole = body_start;

        const fn_ty = try self.program.store.intern(.{ .function = .{ .args = worker.params, .ret = worker.ret, .effectful = false } });
        const callee: Callee = if (request.procedure_template) |template|
            .{ .worker = try self.program.templateRefWorker(template) }
        else if (request.procedure_binding) |binding| switch (self.view.view.top_level_procedure_bindings.bindings.items[@backingInt(binding)].body) {
            .direct_template => |direct| .{ .worker = try self.program.templateRefWorker(Program.directTemplate(direct.template)) },
            // An exact procedure alias calls what it names.
            .callable_eval_template => |template| if (self.view.view.callable_eval_templates.templates[@backingInt(template)].forwarded_lookup != null)
                try self.bindingCallee(self.forwardedBinding(self.module, template))
            else
                .{ .callable_eval = .{ .module = self.module, .template = template } },
            .checked_error => invariant("a root names a rejected procedure binding"),
        } else if (request.procedure_use) |use|
            try self.procedureUseCallee(use)
        else
            invariantFmt("a {s} root names no procedure", .{@tagName(request.kind)});

        try self.tasks.append(self.allocator, .ret);
        switch (callee) {
            .worker => |target| {
                const target_worker = self.program.worker(target).*;
                const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                const bound = try self.relateVariables(formal, fn_ty, target_worker.scheme_vars.len);
                defer self.allocator.free(bound);
                const hidden = try self.allocator.alloc(LocalId, bound.len + target_worker.dictionary_params.len);
                defer self.allocator.free(hidden);
                for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descOf(ty);
                if (target_worker.dictionary_params.len != 0) {
                    const span = request.root_evidence orelse invariant("a root whose target takes dictionaries has no root evidence");
                    const owner = self.program.moduleByKey(span.checked_module);
                    if (owner.index != self.module) invariant("a root's evidence was recorded by another module");
                    const evidence = owner.view.static_dispatch_plans.evidence_refs[span.span.start..][0..span.span.len];
                    const context = self.templateParamContext(target, bound);
                    for (target_worker.dictionary_params, hidden[bound.len..]) |param_index, *dict| {
                        if (param_index >= evidence.len) invariant("a root's evidence does not cover its target's dictionaries");
                        dict.* = try self.paramDictionary(evidence, param_index, context);
                    }
                }
                for (params) |param| try self.pushValue(param);
                try self.finishCall(target, hidden, @intCast(params.len), try self.program.substitute(self.allocator, target_worker.ret, bound), worker.ret);
            },
            .callable_eval => |eval| {
                try self.tasks.append(self.allocator, .{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(params.len), .result = worker.ret } });
                var index = params.len;
                while (index > 0) {
                    index -= 1;
                    try self.tasks.append(self.allocator, .{ .push_value = params[index] });
                }
                try self.tasks.append(self.allocator, .{ .restore_callable_eval = .{ .module = eval.module, .template = eval.template, .ty = fn_ty } });
            },
        }
        try self.run();
        try self.finishProc(entry, body_start, params, ret_layout);
    }

    /// A default expression's worker: the expression, in no scope.
    pub fn lowerDefaultBody(self: *Builder, expr: CheckedExprId) Allocator.Error!void {
        const worker = self.currentWorker().*;
        self.entry_hole = try self.newHole();
        const entry = self.entry_hole;
        self.var_descs = &.{};
        self.dict_locals = &.{};
        const ret_layout = try self.program.layouts.layoutOf(worker.ret);
        const body_start = try self.newHole();
        self.hole = body_start;
        try self.tasks.append(self.allocator, .ret);
        try self.tasks.append(self.allocator, .{ .expr = expr });
        try self.run();
        try self.finishProc(entry, body_start, &.{}, ret_layout);
    }

    /// The value of the `??` default `default`: its declaring module's
    /// default expression, evaluated now.
    pub fn defaultValue(self: *Builder, default: checked.CheckedFieldDefault) Allocator.Error!LocalId {
        const origin_module = default.origin() orelse invariant("a field default has no declaring module");
        const module_index = self.program.by_identity.get(self.view.nameStore().moduleIdentityBytes(origin_module).*) orelse
            invariant("a field default's declaring module is not in this lowering");
        const expr = self.program.module(module_index).bodies().defaultExpr(default.expr_node) orelse
            invariant("a field default's expression was not archived");
        const worker = try self.program.defaultWorker(module_index, expr);
        const target = try self.localOf(self.program.worker(worker).ret, .frame);
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(worker).proc, .args = LIR.LocalSpan.empty(), .next = undefined } });
        return target;
    }

    pub fn lowerMethodEntry(self: *Builder, target: WorkerId) Allocator.Error!void {
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
    pub fn lowerStructuralMethod(self: *Builder, method: lower.StructuralMethod) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var start = try self.beginErasedWorker(&.{.opaque_ptr});
        defer self.allocator.free(start.captures);
        self.var_descs = try self.program.arena.allocator().dupe(LocalId, start.captures);
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .structural = method });
    }

    /// A derived map over the tag union its first captured descriptor
    /// describes: the selected variant's payloads are read, its selected
    /// payload transformed, and the variant rebuilt under the output
    /// union's descriptor; any other variant is re-tagged by name, its
    /// payloads unchanged.
    pub fn lowerStructuralMap(self: *Builder, map: lower.StructuralMap) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const field_layouts = try self.allocator.alloc(layout.Idx, worker.structural_descs);
        defer self.allocator.free(field_layouts);
        @memset(field_layouts, .opaque_ptr);
        var start = try self.beginErasedWorker(field_layouts);
        defer self.allocator.free(start.captures);
        self.var_descs = try self.program.arena.allocator().dupe(LocalId, start.captures);
        try self.runErasedBody(&start, worker.params, worker.ret, .{ .structural_map = map });
    }

    /// The two branches of a derived map over `input` (of the structural
    /// type parameter `input_ty`) with `transform`: each returns the
    /// worker's result of type `output_ty`.
    fn structuralMapBranches(self: *Builder, map: lower.StructuralMap, input: LocalId, transform: LocalId, input_ty: TypeId, output_ty: TypeId) Allocator.Error!void {
        const store = &self.program.store;
        const input_desc = self.var_descs[0];
        const output_desc = self.var_descs[1];
        const name = try self.nameDesc(map.name);
        const discriminant = try self.closedLocal(.u32);
        try self.emitDyn(discriminant, .{ .tag_discriminant = .{ .base = input, .desc = input_desc } });
        const selected = try self.closedLocal(.u32);
        try self.emitDyn(selected, .{ .tag_discriminant_of = .{ .desc = input_desc, .name = name } });
        const is_selected = try self.closedLocal(.bool);
        try self.emitLowLevel(is_selected, .num_is_eq, &.{ discriminant, selected });
        const holes = try self.emitBoolSwitch(is_selected);

        self.hole = holes.then;
        const payloads = try self.allocator.alloc(LocalId, map.arity);
        defer self.allocator.free(payloads);
        for (payloads, 0..) |*payload, index| {
            const payload_ty = try store.variable(@intCast(2 + index));
            const view = try self.localOf(payload_ty, .view);
            try self.emitDyn(view, .{ .tag_payload_addr = .{ .base = input, .desc = input_desc, .name = name, .payload_index = @intCast(index) } });
            const copy = try self.localOf(payload_ty, .frame);
            try self.emitDyn(copy, .{ .copy = .{ .src = view, .desc = self.var_descs[2 + index] } });
            payload.* = copy;
        }
        const transform_ty = try store.intern(.{ .function = .{
            .args = &.{try store.variable(2 + map.payload_index)},
            .ret = try store.variable(2 + map.arity),
            .effectful = map.effectful,
        } });
        payloads[map.payload_index] = try self.emitErasedCall(transform, transform_ty, &.{payloads[map.payload_index]}, try store.variable(2 + map.arity));
        const mapped = try self.localOf(output_ty, .frame);
        try self.emitDyn(mapped, .{ .tag = .{ .desc = output_desc, .name = name, .payloads = try self.store.addLocalSpan(payloads) } });
        try self.pushValue(mapped);
        try self.step(.ret);

        self.hole = holes.otherwise;
        const unchanged = try self.localOf(output_ty, .frame);
        try self.emitDyn(unchanged, .{ .adapt = .{ .src = input, .from = try self.descOf(input_ty), .to = output_desc } });
        try self.pushValue(unchanged);
        try self.step(.ret);
    }

    /// A crashing dictionary method: it crashes as the dispatch it stands
    /// for would.
    pub fn lowerCrashDictionary(self: *Builder, kind: lower.CrashDictionary) Allocator.Error!void {
        const worker = self.currentWorker().*;
        const start = try self.beginErasedWorker(&.{});
        defer self.allocator.free(start.captures);
        self.var_descs = &.{};
        const body_start = try self.newHole();
        self.hole = body_start;
        try self.emitDispatchCrash(kind);
        try self.finishProc(start.entry, body_start, &.{start.capture_ptr}, .zst);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = start.capture_ptr;
        proc.abi = .erased_record;
    }

    /// The crash of a dispatch that cannot run: a rejected one is checked-
    /// error code, an unreachable one an ordinary crash.
    pub fn emitDispatchCrash(self: *Builder, kind: lower.CrashDictionary) Allocator.Error!void {
        switch (kind) {
            .checked_error => {
                const literal = try self.store.insertString("method dispatch failed to check");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
            },
            .unreachable_value => {
                const literal = try self.store.insertString("dispatch on a value that can never exist");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal } } });
            },
        }
    }

    pub const ErasedBody = union(enum) {
        call: struct { target: WorkerId, hidden: []const LocalId },
        structural: lower.StructuralMethod,
        structural_map: lower.StructuralMap,
    };

    pub fn runErasedBody(self: *Builder, start: *ErasedStart, params: []const TypeId, ret: TypeId, body: ErasedBody) Allocator.Error!void {
        const worker = self.currentWorker().*;
        var args: std.ArrayList(LocalId) = .empty;
        defer args.deinit(self.allocator);
        var record: ?LocalId = null;
        if (params.len != 0) {
            record = try self.argsRecordLocal(params, .param);
            try args.append(self.allocator, record.?);
        }
        var ret_layout: layout.Idx = undefined;
        if (try self.closed(ret)) {
            ret_layout = try self.program.layouts.layoutOf(ret);
        } else {
            ret_layout = try self.program.result.layouts.dynamicLayout();
            self.result_local = try self.localOf(ret, .result);
        }
        const body_start = try self.newHole();
        self.hole = body_start;
        const values = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(values);
        for (values, params, 0..) |*value, param_ty, index| value.* = try self.argsRecordField(record.?, @intCast(index), param_ty);
        const result: ?LocalId = switch (body) {
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
                    .to_inspect => try self.emitDyn(target, .{ .inspect = .{ .value = values[0], .desc = self.var_descs[0] } }),
                }
                break :blk target;
            },
            .structural_map => |map| blk: {
                try self.structuralMapBranches(map, values[0], values[1], params[0], ret);
                break :blk null;
            },
        };
        if (result) |value| {
            try self.pushValue(value);
            try self.step(.ret);
        }
        try args.append(self.allocator, start.capture_ptr);
        try self.finishProc(start.entry, body_start, args.items, ret_layout);
        const proc = self.store.getProcSpecPtr(worker.proc);
        proc.erased_capture_arg = start.capture_ptr;
        proc.abi = .erased_record;
    }

    /// The closed layout of a lambda's capture record: the captured values,
    /// then one descriptor pointer per scope variable, in that order of
    /// checked field indices.
    pub fn captureLayout(self: *Builder, capture_types: []const TypeId, descriptor_count: usize, dictionary_count: usize) Allocator.Error!layout.Idx {
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
    pub fn argsRecordLocal(self: *Builder, params: []const TypeId, storage: LIR.DynStorage) Allocator.Error!LocalId {
        if (try self.allClosed(params)) return self.closedLocal(try self.argsRecordLayout(params));
        const desc = try self.argsRecordDesc(params);
        if (storage == .frame) self.has_dynamic_frame = true;
        return self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = storage } });
    }

    pub fn allClosed(self: *Builder, tys: []const TypeId) Allocator.Error!bool {
        for (tys) |ty| {
            if (!try self.closed(ty)) return false;
        }
        return true;
    }

    pub fn argsRecordLayout(self: *Builder, params: []const TypeId) Allocator.Error!layout.Idx {
        const fields = try self.allocator.alloc(layout.StructField, params.len);
        defer self.allocator.free(fields);
        for (fields, params, 0..) |*field, param, index| field.* = .{ .index = @intCast(index), .layout = try self.program.layouts.layoutOf(param) };
        return try self.program.result.layouts.putNominalStructFields(fields);
    }

    pub fn argsRecordDesc(self: *Builder, params: []const TypeId) Allocator.Error!LocalId {
        const descs = try self.allocator.alloc(LocalId, params.len);
        defer self.allocator.free(descs);
        for (descs, params) |*desc, param| desc.* = try self.descOf(param);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(descs) } }), .next = undefined } });
        return local;
    }

    /// Argument `index` of an argument record, as a borrowed read.
    pub fn argsRecordField(self: *Builder, record: LocalId, index: u32, param_ty: TypeId) Allocator.Error!LocalId {
        if (self.store.localDyn(record)) |dyn| {
            const local = try self.localOf(param_ty, .view);
            try self.emitDyn(local, .{ .field_addr = .{ .base = record, .desc = dyn.desc, .field_index = index } });
            return local;
        }
        const local = try self.localOf(param_ty, .frame);
        if (self.store.getLocal(record).layout_idx == .zst) {
            // A record of zero-sized arguments has no storage to read.
            try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
            return local;
        }
        try self.emitLinear(.{ .assign_ref = .{ .target = local, .op = LIR.RefOp.field(record, index), .next = undefined } });
        return local;
    }

    pub fn run(self: *Builder) Allocator.Error!void {
        while (self.tasks.pop()) |task| try self.step(task);
        if (self.values.items.len != 0) invariant("body lowering left values on its value stack");
    }

    pub fn push(self: *Builder, task: Task) Allocator.Error!void {
        try self.tasks.append(self.allocator, task);
    }

    pub fn popValue(self: *Builder) LocalId {
        return self.values.pop() orelse invariant("body lowering popped an empty value stack");
    }

    pub fn pushValue(self: *Builder, local: LocalId) Allocator.Error!void {
        try self.values.append(self.allocator, local);
    }

    pub fn step(self: *Builder, task: Task) Allocator.Error!void {
        switch (task) {
            // Code after a terminal is unreachable: nothing of it is
            // lowered, and an expression there stands for a value no
            // execution produces.
            .expr => |expr| if (self.hole == null) {
                const checked_ty = self.bodies().expr(expr).ty;
                try self.pushValue(if (try self.program.converter().hasError(self.module, checked_ty)) try self.closedLocal(.zst) else try self.localOf(try self.exprType(expr), .frame));
            } else try self.stepExpr(expr),
            .stmt => |stmt| if (self.hole != null) try self.stepStmt(stmt),
            .bind => |bind| try self.bindIrrefutable(bind.pattern, self.popValue(), bind.ty),
            .discard => _ = self.popValue(),
            .runtime_error => |ty| try self.runtimeErrorValue(ty),
            .retag => |retag| {
                const value = self.popValue();
                try self.pushValue(try self.adaptValue(value, retag.from, retag.to));
            },
            .diverged => |ty| {
                if (self.hole != null) try self.emitTerminal(.runtime_error);
                try self.pushValue(if (ty) |value_ty| try self.localOf(value_ty, .frame) else try self.closedLocal(.zst));
            },
            .dispatch_crash => |crash| {
                try self.emitDispatchCrash(crash.kind);
                try self.pushValue(try self.localOf(crash.result, .frame));
            },
            .push_value => |local| try self.pushValue(local),
            .unreachable_value => |ty| try self.pushValue(try self.localOf(ty, .frame)),
            .unreachable_or_unit => try self.pushValue(try self.unitValue()),
            .segments_arg => |arg| {
                const interpolation = self.bodies().expr(arg.expr).data.interpolation;
                const elems = try self.allocator.alloc(LocalId, interpolation.segments.len);
                defer self.allocator.free(elems);
                for (interpolation.segments, elems) |segment, *elem| {
                    elem.* = switch (self.bodies().expr(segment).data) {
                        .str_segment => |literal| try self.strLiteral(self.bodies().stringLiteral(literal)),
                        .pending, .numeral, .str_from_quote, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("an interpolation segment is not a string segment"),
                    };
                }
                const list = try self.localOf(arg.ty, .frame);
                try self.emitLinear(.{ .assign_list = .{ .target = list, .elems = try self.store.addLocalSpan(elems), .next = undefined } });
                try self.pushValue(list);
            },
            .finish_interpolation => |expr| try self.finishInterpolation(expr),
            .finish_interpolation_call => |call| {
                const values = try self.popValues(call.count);
                defer self.allocator.free(values);
                const list = try self.buildList(call.values_ty, values);
                try self.pushValue(try self.emitErasedCall(call.assembler, call.assembler_ty, &.{list}, call.result));
            },
            .finish_local_call => |call| try self.finishLocalCall(call.closure, call.words, call.params, call.ret, call.arg_types, call.result),
            .finish_hash => |hash| {
                const hasher = self.popValue();
                const value = self.popValue();
                const target = try self.localOf(hash.result, .frame);
                try self.emitDyn(target, .{ .hash = .{ .hasher = hasher, .value = value, .desc = try self.descOf(hash.value) } });
                try self.pushValue(target);
            },
            .push_value_str => |literal| try self.pushValue(try self.strLiteral(self.bodies().stringLiteral(literal))),

            .resume_at => |hole| self.hole = hole,
            .ret => {
                const value = self.popValue();
                // Every branch of a construct in tail position returned.
                if (self.hole == null) return;
                if (self.result_local) |result| {
                    // A call's result is written straight into the result
                    // storage, so the call returns it directly.
                    if (!try self.retargetLastCall(value, result)) {
                        try self.emitDyn(result, .{ .copy = .{ .src = value, .desc = self.store.localDyn(result).?.desc } });
                    }
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
            .literal_failed => |message| {
                const literal = try self.store.insertString(message);
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal } } });
            },
            .fill_recursive_slots => |pattern| try self.fillRecursiveSlots(pattern),
            .rejected_for => |plan_id| {
                _ = self.popValue();
                const plan = self.view.view.static_dispatch_plans.iterator_for_plans[@backingInt(plan_id)];
                switch (plan.iter.resolution) {
                    .checked_error => try self.emitDispatchCrash(.checked_error),
                    .@"unreachable" => try self.emitDispatchCrash(.unreachable_value),
                    .direct_pending, .direct_closed, .direct_parametric, .evidence_dependent, .structural => {
                        const literal = try self.store.insertString("runtime error");
                        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                    },
                }
                try self.pushValue(try self.closedLocal(.zst));
            },
            .leave_alias_scope => {
                const scope = self.alias_scopes.pop().?;
                self.env = scope.saved_env;
            },
            .end_loop => {
                // A loop nothing leaves never reaches what follows it.
                const loop = self.loops.pop().?;
                if (!loop.reached) try self.emitTerminal(.runtime_error);
            },
            .for_start => |for_| try self.forStart(for_.pattern, for_.body, for_.plan, for_.mutations),
            .numeral_arg => |arg| try self.pushValue(try self.numeralArgument(arg.literal, arg.ty)),
            .finish_call => |call| {
                defer self.allocator.free(call.hidden);
                if (call.adapt) |adapt| return try self.finishAdaptedCall(call.callee, call.hidden, adapt, call.ret, call.result);
                try self.finishCall(call.callee, call.hidden, call.arg_count, call.ret, call.result);
            },
            .finish_low_level => |ll| try self.finishLowLevel(ll.op, ll.arg_count, ll.result, ll.arg_types),
            .finish_record => |expr| try self.finishRecord(expr),
            .restore => |restore| try self.restoreStep(restore.source, restore.node, restore.ty),
            .restore_callable_eval => |eval| {
                const owner = self.program.module(eval.module);
                const template = owner.view.callable_eval_templates.templates[@backingInt(eval.template)];
                const root = owner.view.compile_time_roots.root(template.root);
                switch (root.payload) {
                    .fn_value => |fn_id| try self.restoreFn(.{ .module = eval.module }, fn_id, eval.ty),
                    // Evaluation stored a value that is not a function: the
                    // crash a rejected binding is.
                    .const_node => |node| try self.push(.{ .restore = .{ .source = .{ .module = eval.module }, .node = node, .ty = eval.ty } }),
                    // No compile-time evaluation produced the value (a
                    // binding of the app a platform's roots name, or one
                    // whose evaluation failed where it was guarded): its
                    // entry wrapper computes it.
                    .pending, .unevaluated, .runtime => {
                        const wrapper = self.program.wrapper_by_root.get(.{ .module = eval.module, .root = template.root }) orelse
                            invariant("a callable-eval binding's root has no entry wrapper");
                        const callee = try self.program.templateRefWorker(wrapper);
                        const callee_worker = self.program.worker(callee).*;
                        if (callee_worker.params.len != 0 or callee_worker.dictionary_params.len != 0) invariant("a callable-eval entry wrapper takes arguments");
                        const bound = try self.relateVariables(callee_worker.ret, eval.ty, callee_worker.scheme_vars.len);
                        defer self.allocator.free(bound);
                        const hidden = try self.allocator.alloc(LocalId, bound.len);
                        defer self.allocator.free(hidden);
                        for (bound, hidden) |ty, *desc| desc.* = try self.descOf(ty);
                        try self.finishCall(callee, hidden, 0, try self.program.substitute(self.allocator, callee_worker.ret, bound), eval.ty);
                    },
                    .discarded, .expect => invariant("a callable-eval binding's root holds a value, which no expect or discarded root does"),
                }
            },
            .finish_restore_closure => |closure| {
                const values = try self.popValues(closure.count);
                defer self.allocator.free(values);
                const worker = self.program.worker(closure.worker).*;
                const packed_closure = if (try self.allClosed(worker.capture_types))
                    try self.packClosure(closure.worker, values, worker.capture_types, closure.descs, closure.dicts)
                else
                    try self.packDynamicClosure(closure.worker, values, closure.capture_types, closure.descs, closure.dicts);
                try self.pushValue(try self.adaptValue(packed_closure, closure.worker_ty, closure.ty));
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
                    .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a tuple access's receiver is not a tuple"),
                };
                try self.pushValue(try self.readField(tuple_ty, tuple, access.elem_index, elems[access.elem_index]));
            },
            .finish_expect_err => |fail| {
                // A `?` inside an `expect` that evaluated an `Err`: the
                // expect fails with the rendered value.
                const value = self.popValue();
                const expr = self.bodies().expr(fail.expr);
                const rendered = try self.closedLocal(.str);
                try self.emitDyn(rendered, .{ .inspect = .{ .value = value, .desc = try self.descOf(fail.value) } });
                const prefix_text = try std.fmt.allocPrint(self.allocator, "The `?` operator in `{s}` evaluated an `Err` inside an `expect`. The value was: Err(", .{self.bodies().stringLiteral(expr.data.expect_err.snippet)});
                defer self.allocator.free(prefix_text);
                const with_value = try self.closedLocal(.str);
                try self.emitLowLevel(with_value, .str_concat, &.{ try self.strLiteral(prefix_text), rendered });
                const message = try self.closedLocal(.str);
                try self.emitLowLevel(message, .str_concat, &.{ with_value, try self.strLiteral(")") });
                try self.emitTerminal(.{ .expect_err = .{ .message = message, .region = expr.source_region } });
                try self.pushValue(try self.localOf(try self.exprType(fail.expr), .frame));
            },
            .finish_dbg => |ty| {
                const value = self.popValue();
                const message = try self.closedLocal(.str);
                try self.emitDyn(message, .{ .inspect = .{ .value = value, .desc = try self.descOf(ty) } });
                try self.emitLinear(.{ .debug = .{ .message = message, .next = undefined } });
                // `dbg` evaluates to `{}`.
                try self.pushValue(try self.unitValue());
            },
            .finish_erased_call => |call| try self.finishErasedCall(call.fn_ty, call.arg_count, call.result),
            .finish_tuple => |expr| try self.finishTuple(expr),
            .finish_tag => |expr| try self.finishTag(expr, null),
            .finish_tag_at => |at| try self.finishTag(at.expr, at.ty),
            .finish_list => |expr| try self.finishList(expr),
            .finish_str => |str| try self.finishStr(str.count),
            .finish_eq => |eq| try self.finishEq(eq.negated, eq.operand),
            .finish_expect => {
                const cond = self.popValue();
                try self.emitLinear(.{ .expect = .{ .condition = cond, .site = try self.expectSite(), .next = undefined } });
                try self.pushValue(try self.unitValue());
            },
            .if_test => |test_| try self.ifTest(test_.expr, test_.index, test_.join, test_.merge),
            .match_start => |expr| try self.matchStart(expr),
            .short_circuit => |sc| try self.shortCircuit(sc.is_and, sc.rhs, sc.join),
            .match_advance => |advance| try self.matchAdvance(advance.state, advance.input),
            .match_guard_done => |done| {
                const value = self.popValue();
                try self.fill(try self.currentHole(), .{ .assign_ref = .{ .target = done.cond, .op = LIR.RefOp.local(value), .next = done.next } }, .source);
                self.hole = null;
            },
        }
    }

    pub fn popValues(self: *Builder, count: u32) Allocator.Error![]LocalId {
        const result = try self.allocator.alloc(LocalId, count);
        var index = count;
        while (index > 0) {
            index -= 1;
            result[index] = self.popValue();
        }
        return result;
    }

    pub fn expectSite(self: *Builder) Allocator.Error!?LIR.ExpectSiteId {
        if (!self.program.observe_expects) return null;
        return try self.program.result.addExpectSite(self.program.sourceLoc(self.module, self.region), self.region);
    }

    /// The `{}` value.
    pub fn unitValue(self: *Builder) Allocator.Error!LocalId {
        const local = try self.closedLocal(.zst);
        try self.emitLinear(.{ .assign_struct = .{ .target = local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
        return local;
    }

    pub fn boolValue(self: *Builder, value: bool) Allocator.Error!LocalId {
        const local = try self.closedLocal(.bool);
        const discriminant = @import("../common.zig").boolDiscriminant(value);
        try self.emitLinear(.{ .assign_tag = .{ .target = local, .variant_index = discriminant, .discriminant = discriminant, .payload = null, .next = undefined } });
        return local;
    }

    pub fn crashValue(self: *Builder, message: []const u8, ty: TypeId) Allocator.Error!void {
        try self.pushValue(try self.crashLocal(message, ty));
    }

    /// Crash with `message`; the returned local of `ty` stands for the value
    /// no execution reaches.
    pub fn crashLocal(self: *Builder, message: []const u8, ty: TypeId) Allocator.Error!LocalId {
        const literal = try self.store.insertString(message);
        try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal } } });
        return try self.localOf(ty, .frame);
    }

    /// Lower `expr` now, at the current statement position, and return its
    /// value: the tasks it pushes run before this returns.
    pub fn lowerExprNow(self: *Builder, expr: CheckedExprId) Allocator.Error!LocalId {
        const base_tasks = self.tasks.items.len;
        try self.push(.{ .expr = expr });
        while (self.tasks.items.len > base_tasks) try self.step(self.tasks.pop().?);
        return self.popValue();
    }

    // -- Expressions --

    /// The stored value of the hoisted root `expr_id` is, when this body
    /// reads it instead of computing it: not the root this body computes,
    /// not a root whose evaluation failed (it runs in place), and not a
    /// guarded binding's root, whose declaration keeps its right-hand side.
    fn storedHoisted(self: *Builder, expr_id: CheckedExprId) ?checked.StoredConstTemplate {
        const entry = self.view.view.hoisted_constants.lookupByExpr(expr_id) orelse return null;
        if (self.own_root) |own| if (own == entry.root) return null;
        const root = self.view.view.compile_time_roots.root(entry.root);
        if (root.payload == .runtime) return null;
        if (entry.pattern != null and root.guarded) return null;
        return switch (self.view.view.const_templates.get(entry.const_ref).state) {
            .stored_const => |stored| stored,
            // A polymorphic hoisted value computes at its instance, as its
            // entry procedure would.
            .eval_template => null,
            .reserved, .unimplemented => invariant("a hoisted root reached runtime lowering without a stored value or an entry procedure"),
        };
    }

    pub fn stepExpr(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        if (self.destination_types.get(expr_id)) |destination| try self.passDestination(expr.data, destination);
        self.region = expr.source_region;
        if (self.destination_types.contains(expr_id) and !try self.program.converter().hasError(self.module, expr.ty)) {
            const own = try self.convert(expr.ty);
            if (try self.retagTarget(expr_id, own)) |destination| try self.push(.{ .retag = .{ .from = own, .to = destination } });
        }
        // An expression compile-time evaluation hoisted and stored is read
        // (design.md "Runtime Programs Read Stored Values").
        if (self.storedHoisted(expr_id)) |stored| {
            return try self.pushStoredRestore(.{ .module = self.module }, stored, try self.exprType(expr_id));
        }
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
            .lookup_required => |resolved| try self.lowerLookupExternal(expr_id, resolved orelse invariant("a platform requirement lookup had no resolved reference")),
            .block => |block| {
                if (self.tail_exprs.contains(expr_id)) try self.tail_exprs.put(self.allocator, block.final_expr, {});
                // A divergent statement leaves the rest of the block
                // unreachable: nothing after it is lowered, and the block's
                // value is never produced.
                const mode = self.inlineExpectMode();
                const diverges_at: ?usize = for (block.statements, 0..) |stmt, position| {
                    if (self.bodies().statementDiverges(stmt, mode)) break position;
                    // A declaration whose value diverges binds nothing, so
                    // nothing after it is reached either.
                    switch (self.bodies().statement(stmt).data) {
                        .decl => |decl| if (self.bodies().exprDiverges(decl.expr, mode)) break position,
                        .var_ => |var_| if (self.bodies().exprDiverges(var_.expr, mode)) break position,
                        .pending, .promoted_proc, .var_uninitialized, .reassign, .crash, .dbg, .expr, .expect, .for_, .while_, .infinite_loop, .breakable_loop, .break_, .return_, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => {},
                    }
                } else null;
                var index = if (diverges_at) |position| position + 1 else block.statements.len;
                if (diverges_at == null) {
                    try self.push(.{ .expr = block.final_expr });
                } else {
                    const block_ty = self.bodies().expr(expr_id).ty;
                    try self.push(.{ .diverged = if (try self.program.converter().hasError(self.module, block_ty)) null else try self.exprType(expr_id) });
                }
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
            // An empty literal at a record type of optional fields builds
            // every field's slot as missing.
            .empty_record => try self.push(.{ .finish_record = expr_id }),
            .record => |record| {
                try self.push(.{ .finish_record = expr_id });
                var index = record.fields.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .expr = record.fields[index].value });
                }
                // An update evaluates its base first.
                if (record.ext) |base_expr| try self.push(.{ .expr = base_expr });
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
                        const nominal_ty = try self.exprType(expr_id);
                        try self.passDestination(self.bodies().expr(nominal.backing_expr).data, nominal_ty);
                        try self.push(.{ .finish_tag_at = .{ .expr = nominal.backing_expr, .ty = nominal_ty } });
                        try self.pushExprsReversed(tag.args);
                    },
                    .zero_argument_tag => try self.push(.{ .finish_tag_at = .{ .expr = nominal.backing_expr, .ty = try self.exprType(expr_id) } }),
                    .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .nominal, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => try self.push(.{ .expr = nominal.backing_expr }),
                }
            },
            .lambda => try self.lowerClosure(expr_id, expr_id, self.program.siteCaptures(self.module, expr_id)),
            .closure => |closure| try self.lowerClosure(expr_id, closure.lambda, self.program.siteCaptures(self.module, expr_id)),
            .row_coerce => |coerce| {
                // The coerced value is built straight at its destination's
                // row (design.md "Row Coercion Primitive").
                try self.destination_types.put(self.allocator, coerce.value, try self.exprType(expr_id));
                try self.push(.{ .expr = coerce.value });
            },
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
                // A retired expression evaluates the operands it still
                // evaluates, each for its effects, then crashes.
                const checked_ty = self.bodies().expr(expr_id).ty;
                try self.push(.{ .runtime_error = if (try self.program.converter().hasError(self.module, checked_ty)) null else try self.exprType(expr_id) });
                var index = runtime_error.evaluated.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.discard);
                    try self.push(.{ .expr = runtime_error.evaluated[index] });
                }
            },
            .if_ => |if_| {
                if (self.tail_exprs.contains(expr_id)) {
                    for (if_.branches) |branch| try self.tail_exprs.put(self.allocator, branch.body, {});
                    try self.tail_exprs.put(self.allocator, if_.final_else, {});
                    // Each branch returns; nothing reads the construct's
                    // own value.
                    try self.push(.unreachable_or_unit);
                    try self.push(.{ .if_test = .{ .expr = expr_id, .index = 0, .join = null, .merge = null } });
                    try self.push(.{ .expr = if_.branches[0].cond });
                    return;
                }
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
                // A match on a layout-resolved low-level whose answer is
                // known here is the branch that answer selects.
                if (try self.layoutResolvedBranch(match.cond, match.branches)) |value| {
                    try self.destination_types.put(self.allocator, value, try self.exprType(expr_id));
                    return try self.push(.{ .expr = value });
                }
                try self.push(.{ .match_start = expr_id });
                try self.push(.{ .expr = match.cond });
            },
            .binop => |binop| switch (binop.op) {
                .@"and", .@"or" => try self.lowerShortCircuit(binop.op == .@"and", binop.lhs, binop.rhs),
                .add, .sub, .mul, .div, .rem, .lt, .gt, .le, .ge, .eq, .ne, .div_trunc, .range_exclusive, .range_inclusive => invariantFmt("binary operator {s} reached Boxy lowering outside a dispatch", .{@tagName(binop.op)}),
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
            .interpolation => |interpolation| try self.lowerInterpolation(expr_id, interpolation),
            .expect_err => |expect_err| {
                try self.push(.{ .finish_expect_err = .{ .expr = expr_id, .value = try self.exprType(expect_err.expr) } });
                try self.push(.{ .expr = expect_err.expr });
            },
            .str_from_quote => |quote| {
                if (quote.conversion_root) |root| return try self.restoreConversionRoot(root, expr_id, .quote);
                if (quote.plan != null) return try self.lowerLiteralTable(expr_id, .quote);
                try self.pushValue(try self.strLiteral(self.bodies().stringLiteral(quote.literal)));
            },
            .return_ => |ret| {
                // The expression itself never produces a value; its successor
                // is unreachable.
                try self.push(.{ .unreachable_value = try self.exprType(expr_id) });
                try self.push(.ret);
                try self.destination_types.put(self.allocator, ret.expr, self.currentWorker().ret);
                try self.push(.{ .expr = ret.expr });
            },
            .unary_not => |operand| {
                try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = try self.exprType(expr_id) } });
                try self.push(.{ .expr = operand });
            },
            .structural_hash => |hash| {
                try self.push(.{ .finish_hash = .{ .value = try self.exprType(hash.value), .result = try self.exprType(expr_id) } });
                try self.push(.{ .expr = hash.hasher });
                try self.push(.{ .expr = hash.value });
            },
            .bytes_literal => |literal| {
                const bytes = self.bodies().stringLiteral(literal);
                const local = try self.localOf(try self.exprType(expr_id), .frame);
                const backing = try self.store.insertStringAligned(bytes, 16);
                try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .bytes_literal = .{
                    .bytes = .{ .backing = backing, .offset = 0, .len = @intCast(bytes.len) },
                    .len = @intCast(bytes.len),
                } }, .next = undefined } });
                try self.pushValue(local);
            },
            .ellipsis => try self.crashValue("not implemented", try self.exprType(expr_id)),
            .break_ => {
                if (self.loops.items.len == 0) invariant("a break outside every loop reached Boxy lowering");
                const loop = &self.loops.items[self.loops.items.len - 1];
                loop.reached = true;
                try self.emitJump(loop.join, null);
                try self.push(.{ .unreachable_value = try self.exprType(expr_id) });
            },
            .pending => invariant("a pending checked expression reached Boxy lowering, which reads only completed checking"),
            .unary_minus => invariant("a unary minus reached Boxy lowering without its checked dispatch"),
            .anno_only => invariant("an annotation-only declaration is read, which resolves to what it requests instead"),
            .hosted_lambda => invariant("a hosted lambda is a hosted template's body root, never a nested expression"),
        }
    }

    /// Hand an expression's destination type to the subexpressions whose
    /// value is its value.
    pub fn passDestination(self: *Builder, data: checked.CheckedExprData, destination: TypeId) Allocator.Error!void {
        switch (data) {
            .block => |block| try self.destination_types.put(self.allocator, block.final_expr, destination),
            .if_ => |if_| {
                for (if_.branches) |branch| try self.destination_types.put(self.allocator, branch.body, destination);
                try self.destination_types.put(self.allocator, if_.final_else, destination);
            },
            .match_ => |match| for (match.branches) |branch| try self.destination_types.put(self.allocator, branch.value, destination),
            .nominal => |nominal| try self.destination_types.put(self.allocator, nominal.backing_expr, try self.structuralType(destination)),
            // A tag's payloads are reached positions of its destination's
            // row (design.md "Row Coercion Primitive").
            .tag => |tag| {
                const store = &self.program.store;
                const tag_union = switch (store.get(try self.structuralType(destination))) {
                    .tag_union => |value| value,
                    .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => return,
                };
                const name = try store.internName(self.view.nameStore().tagLabelText(tag.name));
                for (store.tagSpan(tag_union.tags)) |candidate| {
                    if (candidate.name != name or candidate.payloads.len != tag.args.len) continue;
                    for (tag.args, store.span(candidate.payloads)) |arg, payload_ty| try self.destination_types.put(self.allocator, arg, payload_ty);
                    return;
                }
            },
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .call, .record, .empty_record, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => {},
        }
    }

    pub fn pushExprsReversed(self: *Builder, exprs: []const CheckedExprId) Allocator.Error!void {
        var index = exprs.len;
        while (index > 0) {
            index -= 1;
            try self.push(.{ .expr = exprs[index] });
        }
    }

    pub fn strLiteral(self: *Builder, text: []const u8) Allocator.Error!LocalId {
        const local = try self.closedLocal(.str);
        const backing = try self.store.insertString(text);
        try self.emitLinear(.{ .assign_literal = .{
            .target = local,
            .value = .{ .str_literal = .{ .backing = backing, .offset = 0, .len = @intCast(text.len) } },
            .next = undefined,
        } });
        return local;
    }

    pub fn finishStr(self: *Builder, count: u32) Allocator.Error!void {
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

    pub fn lowerNumeral(self: *Builder, expr_id: CheckedExprId, numeral: checked.CheckedNumeralData) Allocator.Error!void {
        if (numeral.conversion_root) |root| return try self.restoreConversionRoot(root, expr_id, .numeral);
        if (numeral.plan != null) return try self.lowerLiteralTable(expr_id, .numeral);
        try self.pushValue(try self.builtinNumeral(numeral.literal, try self.exprType(expr_id)));
    }

    /// A numeric literal at the type `ty` a generic body knows by its
    /// descriptor: the literal's value under every builtin number type,
    /// selected at runtime by the descriptor.
    pub fn descriptorNumeral(self: *Builder, literal: NumeralLiteral, ty: TypeId) Allocator.Error!LocalId {
        const Table = boxy_runtime.desc.NumeralTable;
        const Scalar = boxy_runtime.desc.Scalar;
        var table: [Table.size]u8 = @splat(0);
        var mask: u32 = 0;
        const exact = self.view.view.module_env.exactNumeral(literal);
        const exact_numeral = checked.exact_numeral;
        inline for (.{ "u8", "i8", "u16", "i16", "u32", "i32", "u64", "i64", "u128", "i128" }) |name| {
            if (exact_numeral.intBits(exact, @field(exact_numeral.Target, name))) |bits| {
                const scalar = @field(Scalar, name);
                const wide: u128 = switch (bits) {
                    .i128 => |v| @bitCast(v),
                    .u128 => |v| v,
                };
                std.mem.writeInt(u128, table[Table.slotOffset(scalar)..][0..16], wide, .little);
                mask |= @as(u32, 1) << @intCast(@backingInt(scalar));
            }
        }
        const f32_bits: u32 = @bitCast(try exact_numeral.floatBits(f32, self.allocator, exact));
        std.mem.writeInt(u32, table[Table.slotOffset(.f32)..][0..4], f32_bits, .little);
        mask |= @as(u32, 1) << @intCast(@backingInt(Scalar.f32));
        const f64_bits: u64 = @bitCast(try exact_numeral.floatBits(f64, self.allocator, exact));
        std.mem.writeInt(u64, table[Table.slotOffset(.f64)..][0..8], f64_bits, .little);
        mask |= @as(u32, 1) << @intCast(@backingInt(Scalar.f64));
        if (try exact_numeral.decBits(self.allocator, exact)) |dec| {
            std.mem.writeInt(i128, table[Table.slotOffset(.dec)..][0..16], dec, .little);
            mask |= @as(u32, 1) << @intCast(@backingInt(Scalar.dec));
        }
        std.mem.writeInt(u32, table[0..4], mask, .little);
        const table_local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = table_local, .value = .{ .static_data = .{ .id = try self.program.static_descs.numeralTableSlot(&table) } }, .next = undefined } });
        const target = try self.localOf(ty, .frame);
        try self.emitDyn(target, .{ .numeral = .{ .desc = try self.descOf(ty), .table = table_local } });
        return target;
    }

    /// A numeric literal at the builtin numeric type `ty`.
    pub fn builtinNumeral(self: *Builder, literal: NumeralLiteral, ty: TypeId) Allocator.Error!LocalId {
        const primitive = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a builtin numeral's type is not a primitive"),
        };
        const exact = self.view.view.module_env.exactNumeral(literal);
        const exact_numeral = checked.exact_numeral;
        const local = try self.closedLocal(@import("layouts.zig").primitiveLayout(primitive));
        const value: LIR.LiteralValue = switch (primitive) {
            .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64, .u128, .i128 => blk: {
                const target: exact_numeral.Target = switch (primitive) {
                    inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| @field(exact_numeral.Target, @tagName(p)),
                    .bool, .str, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => unreachable, // the enclosing prong admits only integers
                };
                const bits = exact_numeral.intBits(exact, target) orelse return try self.crashLocal("invalid numeric literal", ty);
                const wide: i128 = switch (bits) {
                    .i128 => |v| v,
                    .u128 => |v| @bitCast(v),
                };
                break :blk switch (primitive) {
                    .u128, .i128 => .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = wide, .layout_idx = @import("layouts.zig").primitiveLayout(primitive) }) },
                    .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .{ .i64_literal = .{ .value = @truncate(wide), .layout_idx = @import("layouts.zig").primitiveLayout(primitive) } },
                };
            },
            .f32 => .{ .f32_literal = try exact_numeral.floatBits(f32, self.allocator, exact) },
            .f64 => .{ .f64_literal = try exact_numeral.floatBits(f64, self.allocator, exact) },
            .dec => .{ .dec_literal = (try exact_numeral.decBits(self.allocator, exact)) orelse return try self.crashLocal("invalid numeric literal", ty) },
            .bool, .str, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => return try self.crashLocal("invalid numeric literal", ty),
        };
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = value, .next = undefined } });
        return local;
    }

    /// A literal whose conversion depends on its instance: the value
    /// compile-time evaluation converted it to at the instance it has here
    /// (design.md "Literal Tables"). Each instance the program's
    /// compile-time specialization converted the literal at is a candidate,
    /// selected by comparing the literal's descriptor with each candidate
    /// type's; the selected candidate's stored value is restored, or its
    /// failed conversion crashes. No user conversion runs here. A literal of
    /// a builtin type is converted by the builtin rule, which no compile-time
    /// root runs: a numeral by its number type, a quote as a Str.
    pub fn lowerLiteralTable(self: *Builder, expr_id: CheckedExprId, kind: LIR.LiteralRejectionKind) Allocator.Error!void {
        return try self.lowerLiteralTableAt(expr_id, kind, try self.exprType(expr_id));
    }

    /// `lowerLiteralTable` for a literal whose converted value has type
    /// `lit_ty`: an interpolation's conversion produces its assembler.
    pub fn lowerLiteralTableAt(self: *Builder, expr_id: CheckedExprId, kind: LIR.LiteralRejectionKind, lit_ty: TypeId) Allocator.Error!void {
        const values = self.program.input.literal_values;
        const indices: []const u32 = if (self.program.literal_entries.get(.{ .module = self.module, .expr = expr_id })) |list| list.items else &.{};
        // One candidate per converted type.
        var candidate_types: std.ArrayList(TypeId) = .empty;
        defer candidate_types.deinit(self.allocator);
        var candidates: std.ArrayList(u32) = .empty;
        defer candidates.deinit(self.allocator);
        for (indices) |index| {
            const ty = try self.constType(.literals, values.?.entries.items[index].ty);
            const seen = for (candidate_types.items) |existing| {
                if (existing == ty) break true;
            } else false;
            if (seen) continue;
            try candidate_types.append(self.allocator, ty);
            try candidates.append(self.allocator, index);
        }
        // A literal of a closed type converted at that type is its value.
        if (try self.closed(lit_ty)) {
            for (candidate_types.items, candidates.items) |ty, index| {
                if (ty != lit_ty) continue;
                return try self.pushLiteralOutcome(values.?.entries.items[index].outcome, ty, lit_ty);
            }
        }
        if (candidates.items.len == 0) return try self.pushValue(try self.builtinLiteral(expr_id, kind, lit_ty));
        // Compile-time evaluation converts every interpolation at each type
        // the program reaches it at, so one of its candidates describes the
        // literal's type: with one candidate, it is that one.
        if (kind == .interpolation and candidates.items.len == 1) {
            return try self.pushLiteralOutcome(values.?.entries.items[candidates.items[0]].outcome, candidate_types.items[0], lit_ty);
        }
        const selected = try self.closedLocal(.u32);
        try self.emitDyn(selected, .{ .desc_index = .{ .desc = try self.descOf(lit_ty), .candidates = try self.argsRecordDesc(candidate_types.items) } });
        const result = try self.localOf(lit_ty, .frame);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        const holes = try self.allocator.alloc(CFStmtId, candidates.items.len);
        defer self.allocator.free(holes);
        for (holes) |*hole| hole.* = try self.newHole();
        if (kind == .interpolation) {
            // Some candidate describes the literal's type: the last is
            // the one no earlier candidate is.
            const branches = try self.allocator.alloc(LIR.CFSwitchBranch, candidates.items.len - 1);
            defer self.allocator.free(branches);
            for (branches, holes[0..branches.len], 0..) |*branch, hole, position| branch.* = .{ .value = position, .body = hole };
            try self.emitTerminal(.{ .switch_stmt = .{ .cond = selected, .branches = try self.store.addCFSwitchBranches(branches), .default_branch = holes[holes.len - 1] } });
        } else {
            const branches = try self.allocator.alloc(LIR.CFSwitchBranch, candidates.items.len);
            defer self.allocator.free(branches);
            for (branches, holes, 0..) |*branch, hole, position| branch.* = .{ .value = position, .body = hole };
            const builtin_hole = try self.newHole();
            try self.emitTerminal(.{ .switch_stmt = .{ .cond = selected, .branches = try self.store.addCFSwitchBranches(branches), .default_branch = builtin_hole } });
            // No candidate describes the literal's type: it is a builtin type.
            self.hole = builtin_hole;
            try self.emitJump(join, try self.builtinLiteral(expr_id, kind, lit_ty));
        }
        // Pushed in reverse execution order: each candidate's branch, then
        // what follows the literal.
        try self.push(.{ .push_value = result });
        try self.push(.{ .resume_at = after });
        var position = candidates.items.len;
        while (position > 0) {
            position -= 1;
            switch (values.?.entries.items[candidates.items[position]].outcome) {
                .value => |node| {
                    try self.push(.{ .jump = join });
                    try self.push(.{ .retag = .{ .from = candidate_types.items[position], .to = lit_ty } });
                    try self.push(.{ .restore = .{ .source = .literals, .node = node, .ty = candidate_types.items[position] } });
                },
                .failed => |message| try self.push(.{ .literal_failed = message }),
            }
            try self.push(.{ .resume_at = holes[position] });
        }
    }

    /// The value of a literal table's entry at the literal's type `lit_ty`,
    /// the entry's own closed type being `ty`.
    fn pushLiteralOutcome(self: *Builder, outcome: @import("literal_values.zig").LiteralValues.Outcome, ty: TypeId, lit_ty: TypeId) Allocator.Error!void {
        switch (outcome) {
            .value => |node| {
                if (ty != lit_ty) try self.push(.{ .retag = .{ .from = ty, .to = lit_ty } });
                try self.push(.{ .restore = .{ .source = .literals, .node = node, .ty = ty } });
            },
            .failed => |message| try self.crashValue(message, lit_ty),
        }
    }

    /// A literal at the builtin type its descriptor names: a numeral by its
    /// number type, a quote as the Str it spells.
    fn builtinLiteral(self: *Builder, expr_id: CheckedExprId, kind: LIR.LiteralRejectionKind, lit_ty: TypeId) Allocator.Error!LocalId {
        return switch (kind) {
            .numeral => try self.descriptorNumeral(self.bodies().expr(expr_id).data.numeral.literal, lit_ty),
            .quote => blk: {
                const text = try self.strLiteral(self.bodies().stringLiteral(self.bodies().expr(expr_id).data.str_from_quote.literal));
                const str_ty = try self.program.store.primitive(.str);
                if (lit_ty == str_ty) break :blk text;
                const adapted = try self.localOf(lit_ty, .frame);
                try self.emitDyn(adapted, .{ .adapt = .{ .src = text, .from = try self.descOf(str_ty), .to = try self.descOf(lit_ty) } });
                break :blk adapted;
            },
            .interpolation => invariant("compile-time evaluation converted no instance of a reached interpolation"),
        };
    }

    /// A literal with a conversion root of its own: the root's stored value
    /// (design.md "Runtime Programs Read Stored Values"). A root whose type
    /// holds a variable no evaluation requests; the program converts the
    /// literal per instance instead, as a literal table.
    pub fn restoreConversionRoot(self: *Builder, root_id: checked.ComptimeRootId, expr_id: CheckedExprId, kind: LIR.LiteralRejectionKind) Allocator.Error!void {
        switch (self.view.view.compile_time_roots.root(root_id).payload) {
            .const_node => |node| try self.push(.{ .restore = .{ .source = .{ .module = self.module }, .node = node, .ty = try self.exprType(expr_id) } }),
            .pending, .unevaluated => try self.lowerLiteralTable(expr_id, kind),
            inline .fn_value, .discarded, .expect, .runtime => |_, tag| invariantFmt("a literal conversion root holding {s} reached runtime lowering", .{@tagName(tag)}),
        }
    }

    pub fn lowerInterpolation(self: *Builder, expr_id: CheckedExprId, interpolation: checked.CheckedInterpolation) Allocator.Error!void {
        const plan_id = interpolation.plan orelse invariant("an interpolation has no checked conversion plan");
        const plan = self.dispatchPlan(plan_id);
        const try_ty = switch (self.program.store.get(try self.convert(plan.callable_ty))) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an interpolation's conversion type is not a function type"),
        };
        // The conversion's assembler is read from the interpolation's
        // literal table, as compile-time evaluation produced it.
        const assembler_ty = try self.tryOkPayload(try_ty);
        try self.push(.{ .finish_interpolation = expr_id });
        try self.lowerLiteralTableAt(expr_id, .interpolation, assembler_ty);
    }

    /// The `Ok` payload type of the `Try` type `try_ty`.
    fn tryOkPayload(self: *Builder, try_ty: TypeId) Allocator.Error!TypeId {
        const store = &self.program.store;
        const ok = try store.internName("Ok");
        const union_ty = switch (store.get(try self.structuralType(try_ty))) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a conversion's Try type is not a tag union"),
        };
        for (store.tagSpan(union_ty.tags)) |tag| {
            if (tag.name == ok and tag.payloads.len == 1) return store.span(tag.payloads)[0];
        }
        invariant("a conversion's Try type has no Ok payload");
    }

    pub fn finishInterpolation(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const assembler = self.popValue();
        const interpolation = self.bodies().expr(expr_id).data.interpolation;
        const assembler_ty = try self.convert(interpolation.assembler_ty);
        const values_ty = switch (self.program.store.get(assembler_ty)) {
            .function => |function| blk: {
                const args = self.program.store.span(function.args);
                if (args.len != 1) invariant("an interpolation assembler does not take exactly the values list");
                break :blk args[0];
            },
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an interpolation assembler is not a function"),
        };
        // The values are lowered after the assembler, in order.
        try self.push(.{ .finish_interpolation_call = .{ .assembler = assembler, .assembler_ty = assembler_ty, .values_ty = values_ty, .count = @intCast(interpolation.values.len), .result = try self.exprType(expr_id) } });
        try self.pushExprsReversed(interpolation.values);
    }

    /// The `Numeral` value `from_numeral` receives for `literal`, at the
    /// argument type `ty`: `Literal({ is_negative, digits_before_pt,
    /// digits_after_pt, digits_after_pt_count })`.
    pub fn numeralArgument(self: *Builder, literal: NumeralLiteral, ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const backing = try self.structuralType(ty);
        const name = try store.internName("Literal");
        const variant = try self.closedVariant(backing, name);
        if (variant.payloads.len != 1) invariant("the Numeral Literal tag does not carry one record");
        const record_ty = try self.structuralType(variant.payloads[0]);
        const fields = switch (store.get(record_ty)) {
            .record => |record| try self.allocator.dupe(types.Field, store.fieldSpan(record.fields)),
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("the Numeral Literal payload is not a record"),
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
    pub fn byteList(self: *Builder, bytes: []const u8, ty: TypeId) Allocator.Error!LocalId {
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
    pub fn intValue(self: *Builder, value: u64, ty: TypeId) Allocator.Error!LocalId {
        const primitive = switch (self.program.store.get(ty)) {
            .primitive => |primitive| primitive,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("an integer field's type is not a primitive"),
        };
        const prim_layout = @import("layouts.zig").primitiveLayout(primitive);
        const local = try self.closedLocal(prim_layout);
        const literal: LIR.LiteralValue = switch (primitive) {
            .u128, .i128 => .{ .i128_literal = try self.store.addStmtRow(LIR.I128Literal, .{ .value = value, .layout_idx = prim_layout }) },
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .{ .i64_literal = .{ .value = @bitCast(value), .layout_idx = prim_layout } },
        };
        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = literal, .next = undefined } });
        return local;
    }

    pub fn resolvedRef(self: *const Builder, id: checked.ResolvedValueRefId) checked.ResolvedValueRefRecord {
        return self.view.view.resolved_value_refs.records[@backingInt(id)];
    }

    pub fn lowerLookupLocal(self: *Builder, expr_id: CheckedExprId, pattern: CheckedPatternId, resolved: ?checked.ResolvedValueRefId) Allocator.Error!void {
        if (resolved) |id| return try self.lowerResolvedValue(expr_id, self.resolvedRef(id).ref);
        const binder = self.bodies().pattern_binder_by_pattern[@backingInt(pattern)].get() orelse invariant("a local lookup named a pattern without a binder");
        try self.pushValue(self.binders.get(binder) orelse invariant("a local lookup named a binder this body never bound"));
    }

    pub fn lowerLookupExternal(self: *Builder, expr_id: CheckedExprId, resolved: checked.ResolvedValueRefId) Allocator.Error!void {
        try self.lowerResolvedValue(expr_id, self.resolvedRef(resolved).ref);
    }

    /// The value a lookup `expr_id` resolved to `ref` reads.
    fn lowerResolvedValue(self: *Builder, expr_id: CheckedExprId, ref: checked.ResolvedValueRef) Allocator.Error!void {
        switch (ref) {
            .local_param, .local_value, .pattern_binder, .local_mutable_version => |binding| {
                return try self.pushValue(self.binders.get(binding.binder) orelse invariant("a local lookup named a binder this body never bound"));
            },
            .top_level_const, .imported_const => |use| return try self.restoreConstUse(expr_id, use, try self.constReadType(expr_id)),
            // A local binding compile-time evaluation hoisted is read as the
            // constant it stored.
            .selected_hoisted_const => |selected| return try self.restoreConstUse(expr_id, selected.const_use, try self.constReadType(expr_id)),
            .local_proc => |local| {
                if (local.is_alias) return try self.lowerAliasUse(expr_id, local);
                return try self.pushValue(try self.localProcUseValue(expr_id, local));
            },
            .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => |use| return try self.procedureValue(expr_id, use),
            .platform_required_proc => |required| return try self.procedureValue(expr_id, required.procedure),
            .platform_required_const => |required| return try self.restoreConstUse(expr_id, required.const_use, try self.constReadType(expr_id)),
            .platform_required_checked_error => return try self.crashValue("runtime error", try self.exprType(expr_id)),
            .platform_required_declaration => invariant("a platform requirement reached lowering unpaired with its app binding"),
        }
    }

    /// A use `expr_id` of a scheme alias: the value its declaration bound,
    /// or, for an alias with a generalized scheme, its right-hand side
    /// lowered in the alias's scope as this use instantiates it (its
    /// variables bound to the use's types, its dictionaries to the use's
    /// evidence).
    fn lowerAliasUse(self: *Builder, expr_id: CheckedExprId, local: checked.LocalProcedureBinding) Allocator.Error!void {
        if (self.binders.get(local.binder)) |value| return try self.pushValue(value);
        const scope_id = local.dispatch_scope orelse invariant("an alias without a generalized scheme is bound where it is declared");
        const scheme = try self.localScheme(scope_id);
        const outer_count = self.env.vars.len;
        const vars = try std.mem.concat(self.program.arena.allocator(), checked.CheckedTypeId, &.{ scheme.scheme_vars, self.env.vars });
        const generic_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic_bound);
        for (generic_bound[0..scheme.scheme_vars.len], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(outer_count + index));
        for (generic_bound[scheme.scheme_vars.len..], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const alias_ty = self.bodies().expr(local.expr).ty;
        const generic_fn = try self.program.converter().convert(self.module, alias_ty, .{ .module = self.module, .vars = vars, .bound = generic_bound });
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, try self.exprType(expr_id) }}, outer_count + scheme.scheme_vars.len);
        defer self.allocator.free(related);
        const bound = try self.program.arena.allocator().alloc(TypeId, vars.len);
        // A variable the use's type does not relate takes the use's checked
        // instantiation of it, or, the checker having fixed none, seals as a
        // variable no use relates.
        const subst = self.siteSubstitution(expr_id);
        for (bound[0..scheme.scheme_vars.len], related[outer_count..], scheme.scheme_vars, 0..) |*slot, ty, variable, index| slot.* = ty orelse
            if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                try self.convert(subst[index])
            else
                try self.program.converter().sealedScheme(self.module, variable);
        @memcpy(bound[scheme.scheme_vars.len..], self.env.bound);
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{};
        const dicts = try self.program.arena.allocator().alloc(LocalId, scheme.dictionaries.len);
        const context: ParamContext = .{ .module = self.module, .params = self.scopeParams(scope_id), .env = .{ .module = self.module, .vars = vars, .bound = bound } };
        for (scheme.dictionaries, dicts) |param_index, *dict| {
            if (param_index >= evidence.len) invariant("an alias use's evidence does not cover its dictionaries");
            dict.* = try self.paramDictionary(evidence, param_index, context);
        }
        try self.alias_scopes.append(self.allocator, .{ .params = scheme.dictionaries, .dicts = dicts, .saved_env = self.env });
        self.env = .{ .module = self.module, .vars = vars, .bound = bound };
        try self.push(.leave_alias_scope);
        try self.push(.{ .expr = local.expr });
    }

    /// The function value a use `expr_id` of a local procedure denotes. A
    /// generalized local procedure whose calls pass its own scheme's
    /// instantiation is, as a value, instantiated where it is used: the
    /// value captures the use's descriptors and dictionaries of its scheme.
    pub fn localProcUseValue(self: *Builder, expr_id: CheckedExprId, local: checked.LocalProcedureBinding) Allocator.Error!LocalId {
        // A use checking recorded no evidence for (one inside a rejected
        // expression, evaluated only for its effects) resolves the
        // procedure's dictionaries from its types.
        return try self.localProcInstanceValue(local, try self.exprType(expr_id), self.view.view.static_dispatch_plans.siteEvidence(expr_id));
    }

    /// The function value of local procedure `local` used at the function
    /// type `use_ty`, whose own scheme's dictionaries `evidence` supplies
    /// (or, when null, each dispatcher's type resolves).
    /// `localProcInstanceValue` resolving the procedure's dictionaries from
    /// its types.
    fn localProcInstanceValueAt(self: *Builder, local: checked.LocalProcedureBinding, use_ty: TypeId) Allocator.Error!LocalId {
        return try self.localProcInstanceValue(local, use_ty, null);
    }

    pub fn localProcInstanceValue(self: *Builder, local: checked.LocalProcedureBinding, use_ty: TypeId, evidence: ?[]const static_dispatch.CheckedEvidence) Allocator.Error!LocalId {
        const lambda_expr = switch (self.bodies().expr(local.expr).data) {
            .lambda => local.expr,
            .closure => |closure| closure.lambda,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const scope_id = self.program.local_scopes.get(.{ .module = self.module, .expr = lambda_expr }) orelse
            return try self.localProcValue(local);
        var scheme = try self.localScheme(scope_id);
        if (!scheme.per_call or (scheme.scheme_vars.len == 0 and scheme.dictionaries.len == 0)) return try self.localProcValue(local);
        const scope_worker, const per_call = try self.localProcWorker(local);
        const declaring = self.program.worker(scope_worker).*;
        const per_call_worker = self.program.worker(per_call).*;
        const outer_count = declaring.scheme_vars.len;
        const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ declaring.scheme_vars, scheme.scheme_vars });
        defer self.allocator.free(vars);
        const generic_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic_bound);
        for (generic_bound, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const lambda_ty = self.bodies().expr(lambda_expr).ty;
        const generic_fn = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = generic_bound });
        // The procedure's own variables take the types the use's
        // instantiation gives them.
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, use_ty }}, vars.len);
        defer self.allocator.free(related);
        scheme.per_call = false;
        const callee = try self.program.lambdaWorker(scope_worker, lambda_expr, per_call_worker.captures, per_call_worker.capture_types, generic_fn, scheme);
        const values = try self.allocator.alloc(LocalId, per_call_worker.captures.len);
        defer self.allocator.free(values);
        const declared = self.declared_captures.get(lambda_expr);
        for (per_call_worker.captures, values, 0..) |capture, *value, index| {
            value.* = if (declared) |captured| captured[index] else self.capturedValue(capture.pattern) orelse invariant("a local procedure's capture is not visible where the procedure is used");
        }
        var declaring_dicts: usize = 0;
        for (declaring.dict_scopes) |dict_scope| declaring_dicts += dict_scope.len;
        const descs = try self.allocator.alloc(LocalId, vars.len);
        defer self.allocator.free(descs);
        @memcpy(descs[0..outer_count], self.var_descs[0..outer_count]);
        const own_types = try self.allocator.alloc(TypeId, scheme.scheme_vars.len);
        defer self.allocator.free(own_types);
        for (descs[outer_count..], own_types, related[outer_count..], scheme.scheme_vars) |*desc, *own_ty, ty, variable| {
            own_ty.* = ty orelse try self.program.converter().sealedScheme(self.module, variable);
            desc.* = try self.descOf(own_ty.*);
        }
        const dicts = try self.allocator.alloc(LocalId, scheme.dictionaries.len + declaring_dicts);
        defer self.allocator.free(dicts);
        const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
        for (scheme.dictionaries, dicts[0..scheme.dictionaries.len]) |param_index, *dict| {
            if (evidence) |entries| {
                if (param_index >= entries.len) invariant("a local procedure use's evidence does not cover its dictionaries");
                dict.* = try self.paramDictionary(entries, param_index, context);
            } else {
                const param = context.params[param_index];
                const converter = self.program.converter();
                dict.* = try self.dictionaryFromType(.{
                    .module = context.module,
                    .method = param.method,
                    .structural = param.structural,
                    .dispatcher = try converter.convert(context.module, param.dispatcher_ty, context.env),
                    .callable = try converter.convert(context.module, param.callable_ty, context.env),
                });
            }
        }
        @memcpy(dicts[scheme.dictionaries.len..], self.dict_locals[self.dict_locals.len - declaring_dicts ..]);
        const closure = if (try self.allClosed(per_call_worker.capture_types))
            try self.packClosure(callee, values, per_call_worker.capture_types, descs, dicts)
        else
            try self.packDynamicClosure(callee, values, per_call_worker.capture_types, descs, dicts);
        // The procedure's type at this use, in this worker's variables.
        const use_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(use_bound);
        for (use_bound[0..outer_count], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        for (use_bound[outer_count..], related[outer_count..], scheme.scheme_vars) |*slot, ty, variable| slot.* = ty orelse try self.program.converter().sealedScheme(self.module, variable);
        const procedure_ty = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = use_bound });
        return try self.adaptValue(closure, procedure_ty, use_ty);
    }

    /// The function value of the local procedure a dispatch edge `node`
    /// selected, at the function type `fn_ty`. A method of a type declared
    /// in a function body is a local procedure of that body, which this
    /// body is, or is nested in.
    pub fn localMethodValue(self: *Builder, node: static_dispatch.EvidenceNode, fn_ty: TypeId) Allocator.Error!LocalId {
        const target = switch (node.target.kind) {
            .local_proc => |target| target,
            .procedure, .structural => invariant("a local method value names a procedure that is not local"),
        };
        if (self.program.moduleByIdx(node.target.module_idx).index != self.module) invariant("a method declared in a function body is dispatched outside its module");
        const local: checked.LocalProcedureBinding = .{ .binder = target.binder, .expr = target.expr, .dispatch_scope = target.dispatch_scope };
        const table = self.view.view.static_dispatch_plans;
        const evidence: ?[]const static_dispatch.CheckedEvidence = switch (node.nested) {
            .resolved => table.nestedEvidence(node),
            .from_callable => null,
            .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
        };
        return try self.localProcInstanceValue(local, fn_ty, evidence);
    }

    /// The worker that declares local procedure `local` (this worker or one
    /// it is nested in) and the procedure's worker there. A procedure whose
    /// declaring body never ran here (it ran at compile time, and a closure
    /// it built was stored) gets its worker now.
    pub fn localProcWorker(self: *Builder, local: checked.LocalProcedureBinding) Allocator.Error!struct { WorkerId, WorkerId } {
        const closure_expr = self.bodies().expr(local.expr);
        const lambda_expr = switch (closure_expr.data) {
            .lambda => local.expr,
            .closure => |closure| closure.lambda,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const captures = self.program.siteCaptures(self.module, local.expr);
        var scope_worker = self.worker_id;
        while (true) {
            if (self.program.worker_ids.get(.{ .lambda = .{ .module = self.module, .expr = lambda_expr, .outer = scope_worker } })) |id| return .{ scope_worker, id };
            switch (self.program.worker(scope_worker).key) {
                .lambda => |key| scope_worker = key.outer,
                .template, .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert => break,
            }
        }
        // No body of the chain declared it: the chain is a stored closure
        // restored in its procedure's scope, and the procedure is declared
        // in that scope's body, which ran at compile time.
        const scope = self.program.worker(scope_worker).*;
        const own: ?lower.LocalScheme = if (self.program.local_scopes.get(.{ .module = self.module, .expr = lambda_expr })) |scope_id| blk: {
            const scheme = try self.localScheme(scope_id);
            break :blk if (scheme.scheme_vars.len == 0 and scheme.dictionaries.len == 0) null else scheme;
        } else null;
        const vars = if (own) |scheme| try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ scope.scheme_vars, scheme.scheme_vars }) else try self.allocator.dupe(checked.CheckedTypeId, scope.scheme_vars);
        defer self.allocator.free(vars);
        const bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(bound);
        for (bound, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const env: types.Env = .{ .module = self.module, .vars = vars, .bound = bound };
        const fn_ty = try self.program.converter().convert(self.module, closure_expr.ty, env);
        const capture_types = try self.allocator.alloc(TypeId, captures.len);
        defer self.allocator.free(capture_types);
        for (captures, capture_types) |capture, *capture_ty| capture_ty.* = try self.capturedType(capture.pattern, try self.program.converter().convert(self.module, self.bodies().pattern(capture.pattern).ty, env));
        return .{ scope_worker, try self.program.lambdaWorker(scope_worker, lambda_expr, captures, capture_types, fn_ty, own) };
    }

    /// The type a callable captures `pattern`'s binder at: its value's type
    /// `ty`, or for a recursive value, the slot's.
    fn capturedType(self: *Builder, pattern: CheckedPatternId, ty: TypeId) Allocator.Error!TypeId {
        const binder = self.bodies().pattern_binder_by_pattern[@backingInt(pattern)].get() orelse
            invariant("a closure capture named a pattern without a binder");
        if (!self.bodies().patternBinder(binder).recursive_value) return ty;
        return try self.recursiveSlotType(ty);
    }

    /// What a callable captures for `pattern`'s binder: its value, or for a
    /// recursive value, its slot.
    fn capturedValue(self: *Builder, pattern: CheckedPatternId) ?LocalId {
        const binder = self.bodies().pattern_binder_by_pattern[@backingInt(pattern)].get() orelse
            invariant("a closure capture named a pattern without a binder");
        if (self.bodies().patternBinder(binder).recursive_value) return self.recursive_slots.get(binder);
        return self.binders.get(binder);
    }

    /// The slot type of a recursive value of type `ty`: a `Box` of the
    /// one-item tuple holding it.
    fn recursiveSlotType(self: *Builder, ty: TypeId) Allocator.Error!TypeId {
        return try self.program.store.intern(.{ .box = try self.program.store.intern(.{ .tuple = &.{ty} }) });
    }

    /// The value the recursive value slot `slot` (of type `slot_ty`) holds,
    /// read in the entry chain.
    fn readRecursiveSlot(self: *Builder, slot: LocalId, slot_ty: TypeId) Allocator.Error!LocalId {
        const tuple_ty = switch (self.program.store.get(slot_ty)) {
            .box => |payload| payload,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .nominal, .function, .variable => invariant("a recursive value's slot is not a box"),
        };
        const value_ty = self.program.store.span(switch (self.program.store.get(tuple_ty)) {
            .tuple => |items| items,
            .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a recursive value's slot does not hold a tuple"),
        })[0];
        const payload = try self.localOf(tuple_ty, .view);
        try self.emitEntry(.{ .dyn = .{ .target = payload, .data = try self.store.addStmtRow(LIR.DynOp, .{ .box_payload_addr = .{ .box = slot, .desc = try self.descOf(tuple_ty) } }), .next = undefined } });
        const value = try self.localOf(value_ty, .view);
        try self.emitEntry(.{ .dyn = .{ .target = value, .data = try self.store.addStmtRow(LIR.DynOp, .{ .field_addr = .{ .base = payload, .desc = try self.descOf(tuple_ty), .field_index = 0 } }), .next = undefined } });
        return value;
    }

    /// The recursive value binders `pattern` binds.
    fn recursiveBinders(self: *Builder, pattern: CheckedPatternId, out: *std.ArrayList(checked.PatternBinderId)) Allocator.Error!void {
        var stack: std.ArrayList(CheckedPatternId) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, pattern);
        while (stack.pop()) |current| {
            switch (self.bodies().pattern(current).data) {
                .assign => |binder| if (self.bodies().patternBinder(binder).recursive_value) try out.append(self.allocator, binder),
                .as => |as| {
                    if (self.bodies().patternBinder(as.binder).recursive_value) try out.append(self.allocator, as.binder);
                    try stack.append(self.allocator, as.pattern);
                },
                .applied_tag => |tag| try stack.appendSlice(self.allocator, tag.args),
                .nominal => |nominal| try stack.append(self.allocator, nominal.backing_pattern),
                .record_destructure => |destructs| for (destructs) |destruct| switch (destruct.kind) {
                    .required, .sub_pattern, .rest => |sub| try stack.append(self.allocator, sub),
                },
                .list => |list| {
                    try stack.appendSlice(self.allocator, list.patterns);
                    if (list.rest) |rest| if (rest.pattern) |sub| try stack.append(self.allocator, sub);
                },
                .tuple => |items| try stack.appendSlice(self.allocator, items),
                .pending, .numeral_literal, .str_literal, .str_interpolation, .underscore, .runtime_error => {},
            }
        }
    }

    /// Allocate a zeroed slot for each recursive value binder `pattern`
    /// binds.
    fn allocRecursiveSlots(self: *Builder, pattern: CheckedPatternId) Allocator.Error!void {
        var binders: std.ArrayList(checked.PatternBinderId) = .empty;
        defer binders.deinit(self.allocator);
        try self.recursiveBinders(pattern, &binders);
        for (binders.items) |binder| {
            const value_ty = try self.patternType(self.bodies().patternBinder(binder).pattern);
            const slot_ty = try self.recursiveSlotType(value_ty);
            const tuple_ty = try self.program.store.intern(.{ .tuple = &.{value_ty} });
            const slot = try self.localOf(slot_ty, .frame);
            try self.emitDyn(slot, .{ .slot_alloc = .{ .desc = try self.descOf(tuple_ty) } });
            try self.recursive_slots.put(self.allocator, binder, slot);
        }
    }

    /// Store each recursive value binder's value, now bound, in its slot.
    fn fillRecursiveSlots(self: *Builder, pattern: CheckedPatternId) Allocator.Error!void {
        var binders: std.ArrayList(checked.PatternBinderId) = .empty;
        defer binders.deinit(self.allocator);
        try self.recursiveBinders(pattern, &binders);
        for (binders.items) |binder| {
            const value = self.binders.get(binder) orelse invariant("a recursive value's binder was not bound by its declaration");
            const value_ty = try self.patternType(self.bodies().patternBinder(binder).pattern);
            const tuple_ty = try self.program.store.intern(.{ .tuple = &.{value_ty} });
            const tuple = try self.buildStruct(tuple_ty, &.{value});
            const filled = try self.localOf(try self.recursiveSlotType(value_ty), .frame);
            try self.emitDyn(filled, .{ .slot_store = .{
                .slot = self.recursive_slots.get(binder).?,
                .value = tuple,
                .desc = try self.descOf(tuple_ty),
            } });
            try self.recursive_slots.put(self.allocator, binder, filled);
        }
    }

    /// The function value of a local procedure: the closure its declaration
    /// bound, or, inside the procedure's own body, the same closure rebuilt
    /// from the captures the body received.
    pub fn localProcValue(self: *Builder, local: checked.LocalProcedureBinding) Allocator.Error!LocalId {
        // The entry chain reads only values the entry chain defines.
        if (self.binders.get(local.binder)) |value| {
            if (!self.emitting_entry or self.entry_locals.contains(value)) return value;
        }
        if (local.is_alias) invariant("an alias is used only after its declaration binds it");
        // Referenced inside its own body or a nested function: build the
        // procedure's function value from its captures, which the checker
        // makes visible wherever the procedure is, and the scope it was
        // declared in, whose descriptors and dictionaries this body holds.
        switch (self.bodies().expr(local.expr).data) {
            .lambda, .closure => {},
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        }
        const scope_worker, const target = try self.localProcWorker(local);
        const declaring = self.program.worker(scope_worker).*;
        const target_worker = self.program.worker(target).*;
        const values = try self.allocator.alloc(LocalId, target_worker.captures.len);
        defer self.allocator.free(values);
        const declared = self.declared_captures.get(target_worker.key.lambda.expr);
        for (target_worker.captures, values, 0..) |capture, *value, index| {
            value.* = if (declared) |captured| captured[index] else self.capturedValue(capture.pattern) orelse invariant("a local procedure's capture is not visible where the procedure is referenced");
        }
        var declaring_dicts: usize = 0;
        for (declaring.dict_scopes) |scope| declaring_dicts += scope.len;
        const descs = self.var_descs[0..declaring.scheme_vars.len];
        const dicts = self.dict_locals[self.dict_locals.len - declaring_dicts ..];
        // A value built from entry locals only is built once, in the entry
        // chain, for every reference; one reading a value the body computes
        // is built where it is referenced.
        const at_entry = for (values) |value| {
            if (!self.entry_locals.contains(value)) break false;
        } else true;
        const all_closed = try self.allClosed(target_worker.capture_types);
        if (!at_entry) {
            return if (all_closed)
                try self.packClosure(target, values, target_worker.capture_types, descs, dicts)
            else
                try self.packDynamicClosure(target, values, target_worker.capture_types, descs, dicts);
        }
        const closure = if (all_closed)
            try self.emitIntoEntry(packClosure, .{ target, values, target_worker.capture_types, descs, dicts })
        else
            try self.emitIntoEntry(packDynamicClosure, .{ target, values, target_worker.capture_types, descs, dicts });
        if (!self.immortal_closures) try self.binders.put(self.allocator, local.binder, closure);
        return closure;
    }

    /// The own scheme of generalized-local scope `scope_id`: its quantified
    /// variables and its runtime evidence parameters.
    pub fn localScheme(self: *Builder, scope_id: checked.DispatchScopeId) Allocator.Error!lower.LocalScheme {
        return try self.localSchemeIn(self.view, scope_id);
    }

    pub fn localSchemeIn(self: *Builder, view: *const types.ModuleView, scope_id: checked.DispatchScopeId) Allocator.Error!lower.LocalScheme {
        const templates = view.view.checked_procedure_templates;
        const scope = templates.dispatch_scopes[@backingInt(scope_id)];
        const evidence_params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, scope.evidence_params);
        var dictionaries: std.ArrayList(u32) = .empty;
        for (evidence_params, 0..) |param, index| {
            if (param.runtime_dictionary) try dictionaries.append(self.program.arena.allocator(), @intCast(index));
        }
        return .{
            .scheme_vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, scope.scheme_vars),
            .dictionaries = dictionaries.items,
            .params = evidence_params,
            .per_call = self.program.local_procs.contains(.{ .module = view.index, .expr = scope.checked_expr }),
        };
    }

    /// A call of a generalized local procedure: its function value, called
    /// with an argument record of the call's instantiation of the
    /// procedure's own scheme (descriptors, then dictionaries) followed by
    /// the explicit arguments.
    pub fn lowerGeneralizedLocalCall(self: *Builder, expr_id: CheckedExprId, call: @FieldType(checked.CheckedExprData, "call"), local: checked.LocalProcedureBinding, scheme: lower.LocalScheme) Allocator.Error!void {
        const closure = try self.localProcValue(local);
        const evidence = self.view.view.static_dispatch_plans.siteEvidence(call.func) orelse &.{};
        // The procedure's own variables take the types the call's
        // instantiation of its type gives them; the enclosing scope's stay as
        // they are here.
        const lambda_expr = switch (self.bodies().expr(local.expr).data) {
            .lambda => local.expr,
            .closure => |closure_expr| closure_expr.lambda,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a local procedure's expression is not a lambda or closure"),
        };
        const outer_count = self.env.vars.len;
        const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ scheme.scheme_vars, self.env.vars });
        defer self.allocator.free(vars);
        const generic_bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(generic_bound);
        for (generic_bound[0..scheme.scheme_vars.len], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(outer_count + index));
        for (generic_bound[scheme.scheme_vars.len..], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const lambda_ty = self.bodies().expr(lambda_expr).ty;
        const generic_fn = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = generic_bound });
        const related = try self.relateVariablesPartial(&.{.{ generic_fn, try self.exprType(call.func) }}, outer_count + scheme.scheme_vars.len);
        defer self.allocator.free(related);
        const own_types = try self.allocator.alloc(TypeId, scheme.scheme_vars.len);
        defer self.allocator.free(own_types);
        // A variable the call's type does not relate takes the call's
        // checked instantiation of it, or, the checker having fixed none,
        // seals as a variable no use relates.
        const subst = self.siteSubstitution(call.func);
        for (own_types, related[outer_count..], scheme.scheme_vars, 0..) |*slot, ty, variable, index| slot.* = ty orelse
            if (index < subst.len and self.view.types().payload(subst[index]) != .err)
                try self.convert(subst[index])
            else
                try self.program.converter().sealedScheme(self.module, variable);
        const bound = try self.allocator.alloc(TypeId, vars.len);
        defer self.allocator.free(bound);
        @memcpy(bound[0..own_types.len], own_types);
        for (bound[own_types.len..], 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
        const fn_ty = try self.program.converter().convert(self.module, lambda_ty, .{ .module = self.module, .vars = vars, .bound = bound });
        const params, const ret = switch (self.program.store.get(fn_ty)) {
            .function => |function| .{ try self.allocator.dupe(TypeId, self.program.store.span(function.args)), function.ret },
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a local procedure's type is not a function type"),
        };
        defer self.allocator.free(params);
        if (params.len != call.args.len) invariant("a local procedure call's arity differs from its type");
        const arg_types = try self.program.arena.allocator().alloc(TypeId, call.args.len);
        for (call.args, arg_types) |arg, *arg_ty| arg_ty.* = try self.exprType(arg);
        const own_words = scheme.scheme_vars.len + scheme.dictionaries.len;
        const words = try self.allocator.alloc(LocalId, own_words);
        defer self.allocator.free(words);
        for (own_types, words[0..own_types.len]) |ty, *desc| desc.* = try self.descOf(ty);
        const scope_id = local.dispatch_scope orelse invariant("a generalized local procedure has no checked scheme scope");
        const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
        for (scheme.dictionaries, words[own_types.len..]) |param_index, *dict| {
            if (param_index >= evidence.len) invariant("a local procedure call's evidence does not cover its dictionaries");
            dict.* = try self.paramDictionary(evidence, param_index, context);
        }
        try self.push(.{ .finish_local_call = .{
            .closure = closure,
            .words = try self.program.arena.allocator().dupe(LocalId, words),
            .params = try self.program.arena.allocator().dupe(TypeId, params),
            .ret = ret,
            .arg_types = arg_types,
            .result = try self.exprType(expr_id),
        } });
        try self.pushExprsReversed(call.args);
    }

    pub fn finishLocalCall(self: *Builder, closure: LocalId, words: []const LocalId, params: []const TypeId, ret: TypeId, arg_types: []const TypeId, result_ty: TypeId) Allocator.Error!void {
        const args = try self.popValues(@intCast(params.len));
        defer self.allocator.free(args);
        // The procedure stores record fields its own way where its kinds
        // are its own scheme's (design.md "Field Kinds").
        for (args, arg_types, params) |*arg, arg_ty, param_ty| arg.* = try self.adaptValue(arg.*, arg_ty, param_ty);
        const word = try self.opaqueWordDesc();
        const elems = try self.allocator.alloc(LocalId, words.len + params.len);
        defer self.allocator.free(elems);
        @memset(elems[0..words.len], word);
        for (elems[words.len..], params) |*elem, param_ty| elem.* = try self.descOf(param_ty);
        const desc = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = desc, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(elems) } }), .next = undefined } });
        const record = try self.addLocal(.{ .layout_idx = try self.program.result.layouts.dynamicLayout(), .dyn = .{ .desc = desc, .storage = .frame } });
        self.has_dynamic_frame = true;
        const fields = try std.mem.concat(self.allocator, LocalId, &.{ words, args });
        defer self.allocator.free(fields);
        try self.emitDyn(record, .{ .record = .{ .desc = desc, .fields = try self.store.addLocalSpan(fields) } });
        const target = try self.callErased(closure, record, ret);
        try self.pushValue(try self.adaptValue(target, ret, result_ty));
    }

    /// A procedure used as a function value: an immortal callable of its
    /// dictionary entry, capturing the use site's hidden arguments.
    pub fn procedureValue(self: *Builder, expr_id: CheckedExprId, use: checked.ProcedureUseTemplate) Allocator.Error!void {
        switch (try self.procedureUseCallee(use)) {
            .worker => |callee| {
                const use_ty = try self.exprType(expr_id);
                const subst = self.siteSubstitution(expr_id);
                const hidden = try self.hiddenArgsAt(callee, subst, self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{}, use_ty);
                defer self.allocator.free(hidden);
                const value = try self.composeDictionary(try self.program.methodEntryWorker(callee), hidden);
                // The procedure stores record fields its own way where its
                // kinds are its own scheme's (design.md "Field Kinds"); the
                // value is adapted to the representation its use calls it
                // with.
                const target_worker = self.program.worker(callee).*;
                const bound = try self.boundTypesAt(callee, subst, use_ty);
                defer self.allocator.free(bound);
                const own_fn = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                try self.pushValue(try self.adaptValue(value, try self.program.substitute(self.allocator, own_fn, bound), use_ty));
            },
            .callable_eval => |eval| try self.push(.{ .restore_callable_eval = .{ .module = eval.module, .template = eval.template, .ty = try self.exprType(expr_id) } }),
        }
    }

    /// A read of a stored constant: its value's construction at `ty`
    /// (design.md "Compile-Time Evaluation Strategy").
    /// Restore stored constant `stored` read at `ty`: at `ty` when it is
    /// closed, and otherwise at the value's own stored type, re-tagged to
    /// `ty`.
    fn pushStoredRestore(self: *Builder, source: lower.ConstSource, stored: checked.StoredConstTemplate, ty: TypeId) Allocator.Error!void {
        if (self.program.store.isClosed(ty)) return try self.push(.{ .restore = .{ .source = source, .node = stored.node, .ty = ty } });
        const stored_ty = try self.constType(source, stored.root_type);
        try self.push(.{ .retag = .{ .from = stored_ty, .to = ty } });
        try self.push(.{ .restore = .{ .source = source, .node = stored.node, .ty = stored_ty } });
    }

    pub fn restoreConstUse(self: *Builder, expr_id: CheckedExprId, use: checked.ConstUseTemplate, ty: TypeId) Allocator.Error!void {
        const owner = self.program.moduleByKey(use.const_ref.artifact);
        const template = owner.view.const_templates.get(use.const_ref);
        switch (template.state) {
            .stored_const => |stored| try self.pushStoredRestore(.{ .module = owner.index }, stored, ty),
            // A polymorphic constant compile-time evaluation did not store
            // is its entry procedure's value at the read's instantiation.
            .eval_template => |eval| {
                const callee = try self.program.templateRefWorker(eval.entry_template);
                const read_ty = try self.exprType(expr_id);
                const fn_ty = try self.program.store.intern(.{ .function = .{ .args = &.{}, .ret = read_ty, .effectful = false } });
                const instance = try self.callInstanceAt(callee, self.siteSubstitution(expr_id), self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{}, fn_ty);
                defer self.allocator.free(instance.hidden);
                try self.finishCall(callee, instance.hidden, 0, instance.ret, read_ty);
            },
            .unimplemented => |unimplemented| {
                self.region = unimplemented.region;
                try self.crashValue(@import("../common.zig").unimplemented_declaration_crash, ty);
            },
            .reserved => invariant("a constant read at runtime was not stored by compile-time evaluation"),
        }
    }

    /// Restore stored constant `node` of `module` at `ty`, pushing tasks for
    /// its components first.
    pub fn restoreStep(self: *Builder, source: lower.ConstSource, node: checked.ConstNodeId, raw_ty: TypeId) Allocator.Error!void {
        const const_store = self.program.constStore(source);
        const store = &self.program.store;
        // Every restore starts at a closed type (`pushStoredRestore`), and a
        // closed type's components are closed.
        if (!store.isClosed(raw_ty)) invariant("a stored value is restored at an open type");
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
            .nominal => |nominal| try self.push(.{ .restore = .{ .source = source, .node = nominal.backing, .ty = ty } }),
            .box => |child| {
                const payload = switch (store.get(ty)) {
                    .box => |payload| payload,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .list, .nominal, .function, .variable => invariant("a stored box restored at a non-Box type"),
                };
                try self.push(.{ .finish_low_level = .{ .op = .box_box, .arg_count = 1, .result = raw_ty } });
                try self.push(.{ .restore = .{ .source = source, .node = child, .ty = payload } });
            },
            .tuple, .record => |children| {
                const component_types: []const TypeId = switch (store.get(ty)) {
                    .tuple => |items| store.span(items),
                    .record => |record| blk: {
                        const fields = store.fieldSpan(record.fields);
                        const storage = try self.program.arena.allocator().alloc(TypeId, fields.len);
                        // An optional field's stored value is its presence
                        // slot's tag (design.md "Field Kinds").
                        for (fields, storage) |field, *slot| slot.* = try self.program.layouts.fieldStorage(field);
                        break :blk storage;
                    },
                    .empty_record => &.{},
                    .primitive, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a stored product restored at a non-product type"),
                };
                if (component_types.len != children.len) invariant("a stored product's arity differs from its type");
                try self.push(.{ .finish_restore_struct = .{ .ty = raw_ty, .count = @intCast(children.len) } });
                var index = children.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .source = source, .node = children[index], .ty = component_types[index] } });
                }
            },
            .tag => |tag| {
                const union_ty = switch (store.get(ty)) {
                    .tag_union => |union_ty| union_ty,
                    .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a stored tag restored at a non-tag-union type"),
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
                    try self.push(.{ .restore = .{ .source = source, .node = tag.payloads[index], .ty = payload_types[index] } });
                }
            },
            .list => |list| {
                const elem = switch (store.get(ty)) {
                    .list => |elem| elem,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a stored list restored at a non-List type"),
                };
                switch (list) {
                    .nodes => |nodes| {
                        try self.push(.{ .finish_restore_list = .{ .ty = raw_ty, .count = @intCast(nodes.len) } });
                        var index = nodes.len;
                        while (index > 0) {
                            index -= 1;
                            try self.push(.{ .restore = .{ .source = source, .node = nodes[index], .ty = elem } });
                        }
                    },
                    .empty => try self.finishRestoreList(raw_ty, 0),
                    .packed_bytes => |packed_list| {
                        const local = try self.localOf(raw_ty, .frame);
                        var bytes = const_store.blobBytes(packed_list.bytes);
                        // A product's items are its scalar leaves in field
                        // order, laid out here as its layout stores them.
                        var decoded: ?[]u8 = null;
                        defer if (decoded) |owned| self.allocator.free(owned);
                        if (packed_list.element == null) {
                            var plan = try lir_core.PackedData.Plan.init(self.allocator, &self.program.result.layouts, try self.program.layouts.layoutOf(elem));
                            defer plan.deinit();
                            if (plan.packed_width != packed_list.product_width) invariant("a stored packed list's items differ from their type's fields");
                            const memory = try self.allocator.alloc(u8, @as(usize, packed_list.len) * plan.memory_width);
                            decoded = memory;
                            plan.decode(memory, bytes, packed_list.len);
                            bytes = memory;
                        }
                        const backing = try self.store.insertStringAligned(bytes, 16);
                        try self.emitLinear(.{ .assign_literal = .{ .target = local, .value = .{ .bytes_literal = .{
                            .bytes = .{ .backing = backing, .offset = 0, .len = @intCast(bytes.len) },
                            .len = packed_list.len,
                        } }, .next = undefined } });
                        try self.pushValue(local);
                    },
                }
            },
            .fn_value => |fn_id| try self.restoreFn(source, fn_id, raw_ty),
        }
    }

    /// Restore stored function value `fn_id` of `module`'s const store as
    /// an erased callable.
    /// Restore a function value compile-time evaluation stored, used at the
    /// closed function type `fn_ty`.
    pub fn restoreFn(self: *Builder, source: lower.ConstSource, fn_id: check.ConstStore.ConstFnId, fn_ty: TypeId) Allocator.Error!void {
        const const_store = self.program.constStore(source);
        const stored = const_store.getFn(fn_id);
        switch (stored.fn_def) {
            // A procedure template's value, whether a checked body, a hosted
            // procedure, or a checked-generated procedure.
            .local_template, .imported_template, .local_hosted, .imported_hosted, .checked_generated => |ref| {
                const target = try self.program.templateRefWorker(ref);
                const target_worker = self.program.worker(target).*;
                const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                // A scheme variable only the target's requirements mention is
                // fixed by relating those requirements' methods.
                const bound = (try self.program.instantiateTarget(target, formal, fn_ty)).bound;
                const dicts = try self.storedDictionaries(stored, target, bound);
                defer self.allocator.free(dicts);
                const hidden = try self.allocator.alloc(LocalId, bound.len + dicts.len);
                defer self.allocator.free(hidden);
                for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descOf(ty);
                @memcpy(hidden[bound.len..], dicts);
                const entry = try self.program.methodEntryWorker(target);
                try self.pushValue(try self.composeDictionary(entry, hidden));
            },
            .nested => |nested| {
                // A closure of a defaulted field's expression belongs to the
                // worker evaluating that default, which has no scheme and no
                // dictionaries of its own.
                const owner_worker = if (nested.default_root) |identity| blk: {
                    const declaring = self.program.by_identity.get(identity.bytes) orelse invariant("a stored default-root closure's module is not in this lowering");
                    const declaring_view = self.program.module(declaring);
                    const default_site = declaring_view.view.nested_proc_sites.sites[@backingInt(nested.site)];
                    break :blk switch (default_site.owner) {
                        .default_root => |default_expr| try self.program.defaultWorker(declaring, default_expr),
                        .template => invariant("a stored default-root closure's site is owned by a template"),
                    };
                } else try self.program.templateRefScope(nested.owner);
                const owner = self.program.worker(owner_worker).*;
                const owner_view = self.program.module(owner.module);
                const site = owner_view.view.nested_proc_sites.sites[@backingInt(nested.site)];
                const site_expr = site.checked_expr orelse invariant("a stored closure's site has no checked expression");
                const expr = owner_view.bodies().expr(site_expr);
                const lambda_expr = switch (expr.data) {
                    .lambda => site_expr,
                    .closure => |closure| closure.lambda,
                    .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a stored closure's site is not a lambda or closure"),
                };
                const captures = owner_view.view.nested_proc_sites.runtime_captures[site.runtime_captures.start..][0..site.runtime_captures.len];
                // The closure in its owner's scope (and, for a generalized
                // lambda, its own scheme's), whose variables the stored
                // value's type instantiates; the value captures them all.
                const own_scope = self.program.local_scopes.get(.{ .module = owner.module, .expr = lambda_expr });
                const own: ?lower.LocalScheme = if (own_scope) |scope_id| blk: {
                    var scheme = try self.localSchemeIn(owner_view, scope_id);
                    if (scheme.scheme_vars.len == 0 and scheme.dictionaries.len == 0) break :blk null;
                    scheme.per_call = false;
                    break :blk scheme;
                } else null;
                const scope_vars = if (own) |scheme| try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ owner.scheme_vars, scheme.scheme_vars }) else try self.allocator.dupe(checked.CheckedTypeId, owner.scheme_vars);
                defer self.allocator.free(scope_vars);
                const variables = try self.allocator.alloc(TypeId, scope_vars.len);
                defer self.allocator.free(variables);
                for (variables, 0..) |*variable, index| variable.* = try self.program.store.variable(@intCast(index));
                const generic: types.Env = .{ .module = owner.module, .vars = scope_vars, .bound = variables };
                const generic_fn = try self.program.converter().convert(owner.module, expr.ty, generic);
                const capture_types = try self.allocator.alloc(TypeId, captures.len);
                defer self.allocator.free(capture_types);
                const stored_captures = try self.allocator.alloc(check.ConstStore.ConstCapture, captures.len);
                defer self.allocator.free(stored_captures);
                // The variables relate through the function's type and
                // through the stored captures' types, which can mention
                // variables the function's type does not.
                var relations: std.ArrayList([2]TypeId) = .empty;
                defer relations.deinit(self.allocator);
                try relations.append(self.allocator, .{ generic_fn, fn_ty });
                for (captures, capture_types, stored_captures) |capture, *capture_ty, *stored_capture| {
                    capture_ty.* = try self.program.converter().convert(owner.module, owner_view.bodies().pattern(capture.pattern).ty, generic);
                    stored_capture.* = for (stored.captures) |candidate| {
                        if (std.meta.eql(candidate.id, capture.capture_id)) break candidate;
                    } else invariant("a stored closure lacks one of its site's captures");
                    // A site's runtime captures are binders of enclosing
                    // frames; a recursive-binding capture holds a top-level
                    // root, which the restored closure reaches through its
                    // own top-level reference and its site never lists.
                    if (stored_capture.kind != .lexical) invariant("a closure site's runtime capture matched a stored recursive-binding capture");
                    // A capture of erased function type names no signature
                    // to relate; its type is its site's, at the variables
                    // the rest relates.
                    if (!try self.constTypeHasErased(source, stored_capture.ty)) {
                        try relations.append(self.allocator, .{ capture_ty.*, try self.constType(source, stored_capture.ty) });
                    }
                }
                // A scheme variable neither the value's type nor its captures
                // mention reaches no value of the restored closure from
                // outside; it seals as every scope seals a variable no use
                // relates.
                const related = try self.relateVariablesPartial(relations.items, scope_vars.len);
                defer self.allocator.free(related);
                // A variable only the closure's requirements mention (its own
                // scheme's, then its owner's) is fixed by relating those
                // requirements' methods.
                {
                    var params: std.ArrayList(static_dispatch.EvidenceParamRecord) = .empty;
                    defer params.deinit(self.allocator);
                    if (own) |scheme| {
                        const templates = owner_view.view.checked_procedure_templates;
                        const scope_params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(own_scope.?)].evidence_params);
                        for (scheme.dictionaries) |param_index| try params.append(self.allocator, scope_params[param_index]);
                    }
                    if (owner.dictionary_params.len != 0) {
                        const owner_params = self.templateEvidenceParams(owner_worker);
                        for (owner.dictionary_params) |param_index| try params.append(self.allocator, owner_params[param_index]);
                    }
                    // A derived codec's or map's instantiation is the
                    // callable its stored evidence recorded: its own
                    // scheme's frame, then its owner's.
                    var recorded: std.ArrayList([2]TypeId) = .empty;
                    defer recorded.deinit(self.allocator);
                    if (own) |scheme| if (scheme.dictionaries.len != 0) {
                        const head = stored.evidence_frame_head orelse invariant("a stored generalized lambda with dictionaries has no evidence");
                        const templates = owner_view.view.checked_procedure_templates;
                        const scope_params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(own_scope.?)].evidence_params);
                        try self.recordedDerivedCallables(stored, head, scope_params, owner.module, generic, &recorded);
                    };
                    if (owner.dictionary_params.len != 0) {
                        var frame_index = stored.evidence_frame_head orelse invariant("a stored function value with dictionaries has no evidence");
                        while (stored.evidence_frames[frame_index].parent) |parent| frame_index = parent;
                        try self.recordedDerivedCallables(stored, frame_index, self.templateEvidenceParams(owner_worker), owner.module, generic, &recorded);
                    }
                    const from_edges = try self.relateVariablesPartial(recorded.items, scope_vars.len);
                    defer self.allocator.free(from_edges);
                    for (related, from_edges) |*slot, determined| {
                        if (slot.* == null) slot.* = determined;
                    }
                    try self.program.relateThroughRequirements(owner.module, scope_vars, params.items, related);
                }
                const bound = try self.allocator.alloc(TypeId, scope_vars.len);
                defer self.allocator.free(bound);
                for (related, bound, scope_vars) |ty, *slot, variable| slot.* = ty orelse try self.program.converter().sealedScheme(owner.module, variable);
                const concrete: types.Env = .{ .module = owner.module, .vars = scope_vars, .bound = bound };
                const concrete_capture_types = try self.allocator.alloc(TypeId, captures.len);
                defer self.allocator.free(concrete_capture_types);
                for (captures, concrete_capture_types) |capture, *concrete_ty| {
                    concrete_ty.* = try self.program.converter().convert(owner.module, owner_view.bodies().pattern(capture.pattern).ty, concrete);
                }
                const worker = try self.program.lambdaWorker(owner_worker, lambda_expr, captures, capture_types, generic_fn, own);
                const descs = try self.program.arena.allocator().alloc(LocalId, bound.len);
                for (bound, descs) |ty, *desc| desc.* = try self.descOf(ty);
                const owner_dicts = if (nested.default_root != null) try self.allocator.alloc(LocalId, 0) else try self.storedDictionaries(stored, owner_worker, bound[0..owner.scheme_vars.len]);
                defer self.allocator.free(owner_dicts);
                // A generalized lambda's own dictionaries are the stored
                // evidence's innermost frame, its own scope's, ahead of its
                // owner's.
                const own_dicts: []LocalId = if (own) |scheme| own: {
                    if (scheme.dictionaries.len == 0) break :own try self.allocator.alloc(LocalId, 0);
                    const head = stored.evidence_frame_head orelse invariant("a stored generalized lambda with dictionaries has no evidence");
                    const frame = stored.evidence_frames[head];
                    if (frame.scope() == .root) invariant("a stored generalized lambda's innermost evidence frame is its procedure's");
                    const templates = owner_view.view.checked_procedure_templates;
                    const params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(own_scope.?)].evidence_params);
                    const roots = try self.storedEvidenceTrees(stored.evidence, frame.roots_start, frame.roots_len, params, concrete);
                    defer self.allocator.free(roots);
                    const out = try self.allocator.alloc(LocalId, scheme.dictionaries.len);
                    for (scheme.dictionaries, out) |param_index, *dict| dict.* = roots[param_index] orelse invariant("a stored dictionary's evidence denotes no dictionary");
                    break :own out;
                } else try self.allocator.alloc(LocalId, 0);
                defer self.allocator.free(own_dicts);
                const dicts = try std.mem.concat(self.allocator, LocalId, &.{ own_dicts, owner_dicts });
                defer self.allocator.free(dicts);
                try self.push(.{ .finish_restore_closure = .{
                    .worker = worker,
                    .count = @intCast(captures.len),
                    .descs = descs,
                    .dicts = try self.program.arena.allocator().dupe(LocalId, dicts),
                    .capture_types = try self.program.arena.allocator().dupe(TypeId, concrete_capture_types),
                    .worker_ty = try self.program.converter().convert(owner.module, expr.ty, concrete),
                    .ty = fn_ty,
                } });
                var index = captures.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.{ .restore = .{ .source = source, .node = stored_captures[index].value, .ty = concrete_capture_types[index] } });
                }
            },
            inline .parser_runtime, .encoder_for_runtime => |runtime| try Codec.restoreRuntime(self, source, fn_id, runtime.owner, runtime.expr, fn_ty),
        }
    }

    /// The dictionaries a stored function value's procedure was
    /// instantiated with: for each of the worker `owner`'s dictionary
    /// parameters, the dictionary its stored evidence denotes, with the
    /// owner's scheme variables bound to `bound`.
    pub fn storedDictionaries(self: *Builder, stored: check.ConstStore.ConstFn, owner: WorkerId, bound: []const TypeId) Allocator.Error![]LocalId {
        const owner_worker = self.program.worker(owner).*;
        const dicts = try self.allocator.alloc(LocalId, owner_worker.dictionary_params.len);
        errdefer self.allocator.free(dicts);
        if (dicts.len == 0) return dicts;
        // The procedure's evidence is the outermost frame's roots.
        var frame_index = stored.evidence_frame_head orelse invariant("a stored function value with dictionaries has no evidence");
        while (stored.evidence_frames[frame_index].parent) |parent| frame_index = parent;
        const frame = stored.evidence_frames[frame_index];
        if (frame.scope() != .root) invariant("a stored function value's outermost evidence frame is not its procedure's");
        const owner_record = self.program.worker(owner).*;
        const roots = try self.storedEvidenceTrees(stored.evidence, frame.roots_start, frame.roots_len, self.templateEvidenceParams(owner), .{ .module = owner_record.module, .vars = owner_record.scheme_vars, .bound = bound });
        defer self.allocator.free(roots);
        for (owner_worker.dictionary_params, dicts) |param_index, *dict| {
            dict.* = roots[param_index] orelse invariant("a stored dictionary's evidence denotes no dictionary");
        }
        return dicts;
    }

    /// Append, for each derived codec or map root of stored evidence frame
    /// `frame_index` (whose roots are `params`, over `env`), the parameter's
    /// callable type paired with the instantiated callable its stored edge
    /// recorded.
    fn recordedDerivedCallables(self: *Builder, stored: check.ConstStore.ConstFn, frame_index: u32, params: []const static_dispatch.EvidenceParamRecord, module_index: types.ModuleIndex, env: types.Env, out: *std.ArrayList([2]TypeId)) Allocator.Error!void {
        const frame = stored.evidence_frames[frame_index];
        const roots = try self.storedEvidenceRoots(stored.evidence, frame.roots_start, frame.roots_len);
        defer self.allocator.free(roots);
        for (roots, 0..) |node_index, position| switch (stored.evidence.nodes[node_index]) {
            .structural => |index| if (stored.evidence.structural(index).checked) |edge| {
                if (position >= params.len) invariant("stored evidence has more entries than its scope has evidence parameters");
                const edge_view = self.program.moduleByKey(.{ .bytes = edge.view.bytes });
                const edge_callable = try self.program.converter().convert(edge_view.index, edge.callable_ty, .{ .module = edge_view.index, .vars = &.{}, .bound = &.{} });
                try out.append(self.allocator, .{ try self.program.converter().convert(module_index, params[position].callable_ty, env), edge_callable });
            },
            .target, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
        };
    }

    /// The node indices of `count` stored evidence trees starting at
    /// `start`: each tree's root, skipping its nested evidence and callable
    /// contracts.
    pub fn storedEvidenceRoots(self: *Builder, evidence: check.ConstStore.ConstFnEvidenceVector, start: u32, count: u32) Allocator.Error![]u32 {
        const roots = try self.allocator.alloc(u32, count);
        errdefer self.allocator.free(roots);
        // Entries left to read in each open vector; the outermost is the
        // roots.
        var open: std.ArrayList(u32) = .empty;
        defer open.deinit(self.allocator);
        try open.append(self.allocator, count);
        var cursor: u32 = start;
        var root_count: u32 = 0;
        while (open.items.len > 0) {
            const top = &open.items[open.items.len - 1];
            if (top.* == 0) {
                _ = open.pop();
                continue;
            }
            top.* -= 1;
            if (open.items.len == 1) {
                roots[root_count] = cursor;
                root_count += 1;
            }
            if (cursor >= evidence.nodes.len) invariant("stored evidence ended before its declared vector length");
            const node = evidence.nodes[cursor];
            cursor += 1;
            const contracts = evidence.callableContracts(node);
            if (contracts != 0) try open.append(self.allocator, contracts);
            switch (node) {
                .target => |target| switch (evidence.target(target).nested) {
                    .resolved => |nested| if (nested.count != 0) try open.append(self.allocator, nested.count),
                    .from_callable => {},
                },
                .structural, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
            }
        }
        return roots;
    }

    /// A template worker's evidence parameters.
    pub fn templateEvidenceParams(self: *Builder, worker_id: WorkerId) []const static_dispatch.EvidenceParamRecord {
        const key = switch (self.program.worker(worker_id).key) {
            .template => |key| key,
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert => invariant("evidence parameters of a worker that is not a procedure template's"),
        };
        const view = self.program.module(key.module).view;
        const template = view.checked_procedure_templates.templates.items[@backingInt(key.template)];
        return lower.poolSlice(static_dispatch.EvidenceParamRecord, view.checked_procedure_templates.evidence_params_pool, template.evidence_params);
    }

    /// The dictionaries of the stored evidence trees of `owner`'s evidence
    /// parameters, `count` pre-order trees starting at `start`: a target's
    /// nested vector is the evidence of the target's own parameters, and
    /// each entry's callable contracts, which no dictionary reads, follow
    /// it. A target takes the instantiation its parameter's callable type,
    /// bound by its owner's instantiation, relates it to.
    pub fn storedEvidenceTrees(self: *Builder, evidence: check.ConstStore.ConstFnEvidenceVector, start: u32, count: u32, root_params: []const static_dispatch.EvidenceParamRecord, root_env: types.Env) Allocator.Error![]?LocalId {
        const Parsed = struct {
            node: u32,
            /// The parsed target whose nested vector holds this entry and
            /// the entry's position there; null for a root.
            parent: ?u32,
            position: u32,
            /// Whether the entry is a callable contract.
            contract: bool = false,
            children: std.ArrayList(u32) = .empty,
            target: ?WorkerId = null,
            bound: []const TypeId = &.{},
        };
        var parsed: std.ArrayList(Parsed) = .empty;
        defer {
            for (parsed.items) |*entry| {
                entry.children.deinit(self.allocator);
                self.allocator.free(entry.bound);
            }
            parsed.deinit(self.allocator);
        }
        // An open vector: its entries left to read and read so far, and the
        // parsed target whose nested evidence it is; contracts and roots
        // have no owner.
        const Open = struct { remaining: u32, read: u32 = 0, owner: ?u32, contracts: bool };
        var open: std.ArrayList(Open) = .empty;
        defer open.deinit(self.allocator);
        var roots: std.ArrayList(u32) = .empty;
        defer roots.deinit(self.allocator);
        try open.append(self.allocator, .{ .remaining = count, .owner = null, .contracts = false });
        var cursor: u32 = start;
        while (open.items.len > 0) {
            const top = &open.items[open.items.len - 1];
            if (top.remaining == 0) {
                _ = open.pop();
                continue;
            }
            top.remaining -= 1;
            const position = top.read;
            top.read += 1;
            const contracts_vector = top.contracts;
            const parent = top.owner;
            const is_root = open.items.len == 1;
            if (cursor >= evidence.nodes.len) invariant("stored evidence ended before its declared vector length");
            const node = evidence.nodes[cursor];
            cursor += 1;
            const index: u32 = @intCast(parsed.items.len);
            if (contracts_vector) {
                try parsed.append(self.allocator, .{ .node = cursor - 1, .parent = null, .position = 0, .contract = true });
            } else {
                try parsed.append(self.allocator, .{ .node = cursor - 1, .parent = parent, .position = position });
                if (parent) |owner_index| try parsed.items[owner_index].children.append(self.allocator, index);
                if (is_root) try roots.append(self.allocator, index);
            }
            const contracts = evidence.callableContracts(node);
            if (contracts != 0) try open.append(self.allocator, .{ .remaining = contracts, .owner = null, .contracts = true });
            switch (node) {
                .target => |target| switch (evidence.target(target).nested) {
                    .resolved => |nested| if (nested.count != 0) try open.append(self.allocator, .{ .remaining = nested.count, .owner = index, .contracts = false }),
                    .from_callable => {},
                },
                .structural, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
            }
        }
        // Parents precede their children: bind each target's scheme from
        // its parameter's callable type at its parent's instantiation.
        const callables = try self.allocator.alloc(?TypeId, parsed.items.len);
        defer self.allocator.free(callables);
        const dispatchers = try self.allocator.alloc(?TypeId, parsed.items.len);
        defer self.allocator.free(dispatchers);
        for (parsed.items, callables, dispatchers) |*entry, *callable, *dispatcher| {
            callable.* = null;
            dispatcher.* = null;
            if (entry.contract) continue;
            const params, const env: types.Env = if (entry.parent) |p| blk: {
                const parent = parsed.items[p];
                const owner_record = self.program.worker(parent.target.?).*;
                break :blk .{ self.templateEvidenceParams(parent.target.?), .{ .module = owner_record.module, .vars = owner_record.scheme_vars, .bound = parent.bound } };
            } else .{ root_params, root_env };
            if (entry.position >= params.len) invariant("stored evidence has more entries than its owner has evidence parameters");
            const param = params[entry.position];
            callable.* = try self.program.converter().convert(env.module, param.callable_ty, env);
            dispatcher.* = try self.program.converter().convert(env.module, param.dispatcher_ty, env);
            switch (evidence.nodes[entry.node]) {
                .target => |target_index| {
                    const stored = evidence.target(target_index).*;
                    const target = try self.methodTargetWorker(stored.method);
                    const target_worker = self.program.worker(target).*;
                    const formal = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                    entry.target = target;
                    // A scheme variable only the target's own requirements
                    // mention is fixed by relating those requirements'
                    // methods (`Program.instantiateTarget`).
                    entry.bound = try self.allocator.dupe(TypeId, (try self.program.instantiateTarget(target, formal, callable.*.?)).bound);
                },
                .structural, .from_callable, .from_scheme, .unreachable_value, .checked_error => {},
            }
        }
        // Children follow their parents, so the reverse order builds every
        // nested dictionary before the dictionary that captures it.
        const dicts = try self.allocator.alloc(?LocalId, parsed.items.len);
        defer self.allocator.free(dicts);
        @memset(dicts, null);
        var index = parsed.items.len;
        while (index > 0) {
            index -= 1;
            const entry = parsed.items[index];
            if (callables[index] == null) continue;
            dicts[index] = switch (evidence.nodes[entry.node]) {
                .target => blk: {
                    const target_worker = self.program.worker(entry.target.?).*;
                    const hidden = try self.allocator.alloc(LocalId, entry.bound.len + target_worker.dictionary_params.len);
                    defer self.allocator.free(hidden);
                    for (entry.bound, hidden[0..entry.bound.len]) |ty, *desc| desc.* = try self.descOf(ty);
                    for (target_worker.dictionary_params, hidden[entry.bound.len..]) |param_index, *dict| {
                        if (param_index >= entry.children.items.len) invariant("stored method evidence does not cover its target's dictionaries");
                        dict.* = dicts[entry.children.items[param_index]] orelse invariant("a stored dictionary's evidence denotes no dictionary");
                    }
                    break :blk try self.composeDictionary(try self.program.methodEntryWorker(entry.target.?), hidden);
                },
                .structural => |structural_index| try self.storedStructuralDictionary(evidence.structural(structural_index).*, dispatchers[index].?, callables[index].?),
                // Evidence no dictionary reads restores to none.
                .from_callable, .from_scheme, .unreachable_value, .checked_error => null,
            };
        }
        const result = try self.allocator.alloc(?LocalId, roots.items.len);
        for (roots.items, result) |root, *dict| dict.* = dicts[root];
        return result;
    }

    pub fn storedStructuralDictionary(self: *Builder, stored: check.ConstStore.ConstFnStructuralEvidence, dispatcher: TypeId, callable: TypeId) Allocator.Error!?LocalId {
        const method: lower.StructuralMethod = switch (stored.derivation) {
            .equality => .is_eq,
            .hash => .to_hash,
            .parser, .encoder => {
                const recorded = stored.checked orelse invariant("stored structural codec evidence records no checked edge");
                const view = self.program.moduleByKey(.{ .bytes = recorded.view.bytes });
                const id = recorded.generated_codec_derivation orelse invariant("stored structural codec evidence names no generated derivation");
                return try Codec.dictionaryAt(self, view.index, id, callable);
            },
            .map, .map_effectful => |plan| return try self.structuralMapDictionary(plan, stored.derivation == .map_effectful, callable),
        };
        return try self.composeDictionary(try self.program.structuralWorker(self.module, method), &.{try self.descOf(dispatcher)});
    }

    pub fn finishRestoreList(self: *Builder, ty: TypeId, count: u32) Allocator.Error!void {
        const values = try self.popValues(count);
        defer self.allocator.free(values);
        const target = try self.localOf(ty, .frame);
        try self.emitLinear(.{ .assign_list = .{ .target = target, .elems = try self.store.addLocalSpan(values), .next = undefined } });
        try self.pushValue(target);
    }

    /// The worker a procedure use names.
    /// What a procedure use calls: a template's worker, or a function value
    /// compile-time evaluation stored.
    pub const Callee = union(enum) {
        worker: WorkerId,
        callable_eval: struct { module: ModuleIndex, template: checked.CallableEvalTemplateId },
    };

    pub fn procedureUseCallee(self: *Builder, use: checked.ProcedureUseTemplate) Allocator.Error!Callee {
        return try self.bindingCallee(use.binding);
    }

    /// What a procedure binding calls, through the exact procedure aliases
    /// it forwards.
    pub fn bindingCallee(self: *Builder, start: @FieldType(checked.ProcedureUseTemplate, "binding")) Allocator.Error!Callee {
        var binding = start;
        while (true) {
            const resolved = self.program.procedureBinding(binding);
            switch (resolved.body) {
                .direct_template => |direct| return .{ .worker = try self.program.templateRefWorker(Program.directTemplate(direct.template)) },
                .checked_error => invariant("a call reached a rejected procedure binding"),
                .callable_eval_template => |template_id| {
                    const owner = self.program.module(resolved.module);
                    const template = owner.view.callable_eval_templates.templates[@backingInt(template_id)];
                    if (template.forwarded_lookup == null) return .{ .callable_eval = .{ .module = resolved.module, .template = template_id } };
                    binding = self.forwardedBinding(resolved.module, template_id);
                },
            }
        }
    }

    /// The procedure binding that the exact procedure alias whose
    /// callable-eval template is `template_id` of `module` forwards to.
    fn forwardedBinding(self: *Builder, module: types.ModuleIndex, template_id: checked.CallableEvalTemplateId) @FieldType(checked.ProcedureUseTemplate, "binding") {
        const owner = self.program.module(module);
        const template = owner.view.callable_eval_templates.templates[@backingInt(template_id)];
        const forwarded = template.forwarded_lookup orelse invariant("a callable-eval template that is no alias forwards nothing");
        const ref_id = owner.view.resolved_value_refs.by_checked_expr[@backingInt(forwarded)].get() orelse
            invariant("a procedure alias's lookup has no resolved reference");
        var ref = owner.view.resolved_value_refs.records[@backingInt(ref_id)].ref;
        while (true) switch (ref) {
            .local_proc => |local| {
                if (!local.is_alias) invariant("a top-level procedure alias names a top-level procedure, as checking forwards only those");
                const final = local.alias_target orelse invariant("a callable alias has no recorded target");
                ref = owner.view.resolved_value_refs.records[@backingInt(final)].ref;
            },
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .selected_hoisted_const, .top_level_const, .imported_const, .top_level_proc, .imported_proc, .hosted_proc, .platform_required_declaration, .platform_required_checked_error, .platform_required_const, .platform_required_proc, .promoted_top_level_proc => break,
        };
        return switch (ref) {
            .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => |next| next.binding,
            .platform_required_proc => |required| required.procedure.binding,
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .local_proc, .selected_hoisted_const, .top_level_const, .imported_const, .platform_required_declaration, .platform_required_checked_error, .platform_required_const => invariant("a procedure alias forwards only to a procedure, as checking requires"),
        };
    }

    pub fn lowerCall(self: *Builder, expr_id: CheckedExprId, call: @FieldType(checked.CheckedExprData, "call")) Allocator.Error!void {
        const func = self.bodies().expr(call.func);
        const resolved: ?checked.ResolvedValueRefId = switch (func.data) {
            .lookup_external => |r| r orelse invariant("a call's external callee was unresolved"),
            .lookup_required => |r| r orelse invariant("a call's platform requirement callee was unresolved"),
            .lookup_local => |lookup| lookup.resolved,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => null,
        };
        const callee: WorkerId = if (resolved) |id| switch (self.resolvedRef(id).ref) {
            inline .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc, .platform_required_proc => |ref, tag| switch (try self.procedureUseCallee(if (tag == .platform_required_proc) ref.procedure else ref)) {
                .worker => |worker_id| worker_id,
                .callable_eval => |eval| {
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = try self.exprType(call.func), .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
                    try self.pushExprsReversed(call.args);
                    try self.push(.{ .restore_callable_eval = .{ .module = eval.module, .template = eval.template, .ty = try self.exprType(call.func) } });
                    return;
                },
            },
            .local_proc => |local| {
                if (local.is_alias) return try self.lowerValueCall(expr_id, call);
                if (local.dispatch_scope) |scope_id| {
                    const scheme = try self.localScheme(scope_id);
                    if (scheme.scheme_vars.len != 0 or scheme.dictionaries.len != 0) return try self.lowerGeneralizedLocalCall(expr_id, call, local, scheme);
                }
                return try self.lowerValueCall(expr_id, call);
            },
            .local_param, .local_value, .pattern_binder, .local_mutable_version, .platform_required_const, .top_level_const, .imported_const, .selected_hoisted_const, .platform_required_checked_error => return try self.lowerValueCall(expr_id, call),
            .platform_required_declaration => invariant("a platform requirement reached lowering unpaired with its app binding"),
        } else return try self.lowerValueCall(expr_id, call);
        const subst = self.siteSubstitution(call.func);
        const call_fn = try self.exprType(call.func);
        const instance = try self.callInstanceAt(callee, subst, self.view.view.static_dispatch_plans.siteEvidence(call.func) orelse &.{}, call_fn);
        var adapt: ?CallAdapt = null;
        if (try self.program.signatureHasSlots(callee)) {
            const arg_types = try self.program.arena.allocator().alloc(TypeId, call.args.len);
            for (call.args, arg_types) |arg, *arg_ty| arg_ty.* = try self.exprType(arg);
            adapt = .{ .bound = instance.bound, .arg_types = arg_types };
        }
        try self.push(.{ .finish_call = .{ .callee = callee, .hidden = instance.hidden, .arg_count = @intCast(call.args.len), .ret = instance.ret, .result = try self.exprType(expr_id), .adapt = adapt } });
        try self.pushExprsReversed(call.args);
    }

    pub const CallAdapt = struct { bound: []const TypeId, arg_types: []const TypeId };

    /// A call of a worker storing record fields of its signature in
    /// presence slots: each argument adapted to the worker's storage of its
    /// parameter at this call's instantiation, and the result adapted back
    /// (design.md "Field Kinds").
    pub fn finishAdaptedCall(self: *Builder, callee: WorkerId, hidden: []const LocalId, adapt: CallAdapt, ret: TypeId, result_ty: TypeId) Allocator.Error!void {
        const explicit = try self.popValues(@intCast(adapt.arg_types.len));
        defer self.allocator.free(explicit);
        const target_worker = self.program.worker(callee).*;
        for (explicit, adapt.arg_types, target_worker.params) |*arg, arg_ty, param_ty| {
            arg.* = try self.adaptValue(arg.*, arg_ty, try self.program.substitute(self.allocator, param_ty, adapt.bound));
        }
        const args = try std.mem.concat(self.allocator, LocalId, &.{ explicit, hidden });
        defer self.allocator.free(args);
        const target = try self.localOf(ret, .frame);
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = target_worker.proc, .args = try self.store.addLocalSpan(args), .next = undefined } });
        try self.pushValue(try self.adaptValue(target, ret, result_ty));
    }

    /// A call of a function value: evaluate the function, then its
    /// arguments, then call through the erased-callable ABI.
    pub fn lowerValueCall(self: *Builder, expr_id: CheckedExprId, call: @FieldType(checked.CheckedExprData, "call")) Allocator.Error!void {
        try self.push(.{ .finish_erased_call = .{ .fn_ty = try self.exprType(call.func), .arg_count = @intCast(call.args.len), .result = try self.exprType(expr_id) } });
        try self.pushExprsReversed(call.args);
        try self.push(.{ .expr = call.func });
    }

    /// The hidden descriptor arguments of a call whose target's scheme
    /// variables `subst` instantiates (checked types of this body's module,
    /// in the target's `scheme_vars` order), followed by its dictionaries.
    pub fn hiddenArgs(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence) Allocator.Error![]LocalId {
        return try self.hiddenArgsAt(callee, subst, evidence, null);
    }

    /// The hidden arguments of a call of `callee` whose function type at the
    /// call is `call_fn`. A substitution entry the checker recorded as
    /// erroneous is a scheme variable the call's own type does not fix: it
    /// takes what the call's function type relates it to, or, related to
    /// nothing, what a variable no use relates seals to.
    pub fn hiddenArgsAt(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence, call_fn: ?TypeId) Allocator.Error![]LocalId {
        return (try self.callInstanceAt(callee, subst, evidence, call_fn)).hidden;
    }

    /// One instantiation of a direct call of a template worker: its hidden
    /// arguments (owned by the caller), the types its scheme variables take
    /// (arena-owned), and the callee's result type at that instantiation.
    pub const CallInstance = struct { hidden: []LocalId, bound: []const TypeId, ret: TypeId };

    /// `hiddenArgsAt` with the instantiation it computed.
    pub fn callInstanceAt(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, evidence: []const static_dispatch.CheckedEvidence, call_fn: ?TypeId) Allocator.Error!CallInstance {
        const owned = try self.boundTypesAt(callee, subst, call_fn);
        defer self.allocator.free(owned);
        const bound = try self.program.arena.allocator().dupe(TypeId, owned);
        return try self.callInstanceBound(callee, bound, evidence);
    }

    /// The call instance of `callee` whose scheme variables take `bound`
    /// (arena-owned), its dictionaries from `evidence`.
    pub fn callInstanceBound(self: *Builder, callee: WorkerId, bound: []const TypeId, evidence: []const static_dispatch.CheckedEvidence) Allocator.Error!CallInstance {
        const target = self.program.worker(callee).*;
        const hidden = try self.allocator.alloc(LocalId, bound.len + target.dictionary_params.len);
        errdefer self.allocator.free(hidden);
        for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descOf(ty);
        if (target.dictionary_params.len != 0) {
            const context = self.templateParamContext(callee, bound);
            for (target.dictionary_params, hidden[bound.len..]) |param_index, *dict| {
                if (param_index >= evidence.len) invariant("a call's checked evidence does not cover its target's dictionaries");
                dict.* = try self.paramDictionary(evidence, param_index, context);
            }
        }
        return .{ .hidden = hidden, .bound = bound, .ret = try self.program.substitute(self.allocator, target.ret, bound) };
    }

    /// The function type an evidence edge instantiated its target at, when
    /// it recorded one.
    pub fn instantiationCallable(self: *Builder, instantiation: static_dispatch.EvidenceTargetInstantiation) Allocator.Error!?TypeId {
        return switch (instantiation) {
            .callable, .alias, .derived_from_callable => |callable| try self.convert(callable),
            .monomorphic => null,
        };
    }

    /// The types the scheme variables of `callee` take at a call: the
    /// checked substitution's, where an erroneous entry takes what the call's
    /// function type `call_fn` relates it to, or seals.
    pub fn boundTypesAt(self: *Builder, callee: WorkerId, checked_subst: []const checked.CheckedTypeId, call_fn: ?TypeId) Allocator.Error![]TypeId {
        const target = self.program.worker(callee).*;
        const subst = try self.program.keptSubstitution(callee, checked_subst);
        // A use the checker recorded no instantiation for (a platform's use
        // of a procedure the app provides) takes every variable from the
        // call's function type.
        const recorded = subst.len != 0;
        if (recorded and subst.len != target.scheme_vars.len) invariant("a call's checked substitution differs from its target's scheme");
        const bound = try self.allocator.alloc(TypeId, target.scheme_vars.len);
        errdefer self.allocator.free(bound);
        // A row variable takes its open row's remainder of the call's row
        // representative, whose layout the caller's values have (design.md
        // "Row Joins").
        const row_vars = try self.allocator.alloc(bool, target.scheme_vars.len);
        defer self.allocator.free(row_vars);
        @memset(row_vars, false);
        const related: []?TypeId = if (call_fn) |actual| blk: {
            const formal = try self.program.store.intern(.{ .function = .{ .args = target.params, .ret = target.ret, .effectful = false } });
            break :blk try self.program.relateVariablesRows(self.allocator, &.{.{ formal, actual }}, target.scheme_vars.len, row_vars);
        } else try self.allocator.alloc(?TypeId, 0);
        defer self.allocator.free(related);
        for (bound, 0..) |*slot, index| {
            const ty = if (index < related.len) related[index] else null;
            if (row_vars[index] and ty != null) {
                slot.* = ty.?;
                continue;
            }
            if (recorded and self.view.types().payload(subst[index]) != .err) {
                slot.* = try self.convert(subst[index]);
                continue;
            }
            slot.* = ty orelse try self.program.converter().sealedScheme(target.module, target.scheme_vars[index]);
        }
        return bound;
    }

    /// The worker of a method target: the procedure template the target
    /// names.
    pub fn methodTargetWorker(self: *Builder, target: static_dispatch.MethodTarget) Allocator.Error!WorkerId {
        return switch (target.kind) {
            .procedure => |procedure| try self.program.templateRefWorker(procedure.template),
            .local_proc => invariant("a method declared in a function body is a local procedure, called through its function value, never as a worker"),
            .structural => invariant("a structural method reached a procedure dispatch"),
        };
    }

    /// The dictionary `evidence` denotes (design.md "Dictionaries"),
    /// composed in the entry chain.
    pub fn dictionaryOf(self: *Builder, evidence: static_dispatch.CheckedEvidence) Allocator.Error!LocalId {
        return try self.dictionaryOfAt(evidence, null);
    }

    /// `dictionaryOf`, its method adapted to the callable type `interface`
    /// its consumer calls it at when one is known (the parameter it fills).
    pub fn dictionaryOfAt(self: *Builder, evidence: static_dispatch.CheckedEvidence, interface: ?TypeId) Allocator.Error!LocalId {
        const table = self.view.view.static_dispatch_plans;
        switch (evidence.resolution) {
            .constraint => |constraint| return self.scopeDictionary(constraint.index),
            .direct => |node_id| {
                const node = table.evidenceNode(node_id);
                if (node.target.kind == .local_proc) {
                    const fn_ty = (try self.instantiationCallable(node.instantiation)) orelse try self.convert(node.target.callable_ty);
                    return try self.localMethodValue(node, fn_ty);
                }
                const target = try self.methodTargetWorker(node.target);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                switch (node.instantiation) {
                    .derived_from_callable, .alias => |callable| {
                        const hidden = if (node.instantiation == .alias) try self.aliasTargetHidden(node, target) else try self.relatedHiddenArgs(target, callable);
                        defer self.allocator.free(hidden);
                        return try self.composeDictionary(try self.program.methodEntryWorker(target), hidden);
                    },
                    .monomorphic, .callable => {},
                }
                const callable = try self.instantiationCallable(node.instantiation);
                const hidden = switch (node.nested) {
                    .resolved => try self.hiddenArgsAt(target, subst, table.nestedEvidence(node), callable),
                    .from_callable => try self.callableDerivedHidden(target, subst, callable),
                    .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                };
                defer self.allocator.free(hidden);
                const entry = try self.program.methodEntryWorker(target);
                const dictionary = try self.composeDictionary(entry, hidden);
                // A method whose signature stores record fields in presence
                // slots, or holds rows its consumers' type widens, is adapted
                // to the representation they call it with (design.md "Field
                // Kinds", "Row Coercion Primitive").
                const expected = interface orelse callable orelse return dictionary;
                const target_worker = self.program.worker(target).*;
                const bound = try self.boundTypesAt(target, subst, callable);
                defer self.allocator.free(bound);
                const own_fn = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                return try self.adaptDictionary(dictionary, try self.program.substitute(self.allocator, own_fn, bound), expected);
            },
            .structural => |structural| {
                const method: lower.StructuralMethod = switch (structural.derivation) {
                    .equality => .is_eq,
                    .hash => .to_hash,
                    .parser, .encoder => return try Codec.dictionary(self, structural),
                    .map, .map_effectful => |plan| return try self.structuralMapDictionary(plan, structural.derivation == .map_effectful, try self.convert(structural.callable_ty)),
                };
                const entry = try self.program.structuralWorker(self.module, method);
                const desc = try self.descOf(try self.convert(structural.dispatcher_ty));
                return try self.composeDictionary(entry, &.{desc});
            },
            .checked_error => return try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, .checked_error), &.{}),
            .unreachable_value => return try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, .unreachable_value), &.{}),
            .from_callable => invariant("callable-derived evidence is resolved with the parameter it stands for (`paramDictionary`)"),
            .from_scheme => invariant("an abstract scheme requirement reached lowered code without its use's evidence"),
        }
    }

    /// The evidence parameters a dictionary vector stands for, and the types
    /// their scheme takes where the vector is built.
    pub const ParamContext = struct {
        module: ModuleIndex,
        params: []const static_dispatch.EvidenceParamRecord,
        env: types.Env,
    };

    /// The parameter context of template worker `callee` whose scheme takes
    /// `bound`.
    fn templateParamContext(self: *Builder, callee: WorkerId, bound: []const TypeId) ParamContext {
        const target = self.program.worker(callee).*;
        const key = switch (target.key) {
            .template => |key| key,
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert => invariant("a worker with dictionary parameters is not a template's"),
        };
        const templates = self.program.module(key.module).view.checked_procedure_templates;
        return .{
            .module = key.module,
            .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items[@backingInt(key.template)].evidence_params),
            .env = .{ .module = key.module, .vars = target.scheme_vars, .bound = bound },
        };
    }

    fn scopeParams(self: *Builder, scope_id: checked.DispatchScopeId) []const static_dispatch.EvidenceParamRecord {
        const templates = self.view.view.checked_procedure_templates;
        return lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.dispatch_scopes[@backingInt(scope_id)].evidence_params);
    }

    /// The parameter context of generalized-local scope `scope_id` whose own
    /// variables `vars` take `own_types`, inside this body's scope.
    fn scopeParamContext(self: *Builder, scope_id: checked.DispatchScopeId, vars: []const checked.CheckedTypeId, own_types: []const TypeId) Allocator.Error!ParamContext {
        const arena = self.program.arena.allocator();
        return .{
            .module = self.module,
            .params = self.scopeParams(scope_id),
            .env = .{
                .module = self.module,
                .vars = try std.mem.concat(arena, checked.CheckedTypeId, &.{ vars, self.env.vars }),
                .bound = try std.mem.concat(arena, TypeId, &.{ own_types, self.env.bound }),
            },
        };
    }

    /// The dictionary for evidence parameter `index` of `context`. An entry
    /// the checker left to the concrete callable (`from_callable`) is the
    /// dictionary of the parameter's dispatcher at the types the context
    /// binds.
    pub fn paramDictionary(self: *Builder, evidence: []const static_dispatch.CheckedEvidence, index: u32, context: ParamContext) Allocator.Error!LocalId {
        if (evidence[index].resolution != .from_callable) {
            const interface = if (index < context.params.len) try self.program.converter().convert(context.module, context.params[index].callable_ty, context.env) else null;
            return try self.dictionaryOfAt(evidence[index], interface);
        }
        if (index >= context.params.len) invariant("callable-derived evidence names a parameter its scheme does not have");
        const param = context.params[index];
        const converter = self.program.converter();
        return try self.dictionaryFromType(.{
            .module = context.module,
            .method = param.method,
            .structural = param.structural,
            .dispatcher = try converter.convert(context.module, param.dispatcher_ty, context.env),
            .callable = try converter.convert(context.module, param.callable_ty, context.env),
        });
    }

    /// The dictionary of `root`, resolved from its dispatcher's type
    /// (`Program.resolveRequirement`), with each method target's own
    /// requirements resolved the same way. Nested requirements are resolved
    /// with an explicit stack.
    pub fn dictionaryFromType(self: *Builder, root: lower.DictRequirement) Allocator.Error!LocalId {
        const Frame = struct {
            target: WorkerId,
            callable: TypeId,
            instance: lower.TargetInstance,
            hidden: std.ArrayList(LocalId),
            next: usize,
        };
        var frames: std.ArrayList(Frame) = .empty;
        defer {
            for (frames.items) |*frame| frame.hidden.deinit(self.allocator);
            frames.deinit(self.allocator);
        }
        var pending: ?lower.DictRequirement = root;
        var delivered: ?LocalId = null;
        while (true) {
            if (pending) |requirement| {
                pending = null;
                switch (try self.program.resolveRequirement(requirement)) {
                    .structural => |structural| delivered = try self.composeDictionary(try self.program.structuralWorker(self.module, structural.method), &.{try self.descOf(structural.over)}),
                    .crash => |kind| delivered = try self.composeDictionary(try self.program.crashDictionaryWorker(self.module, kind), &.{}),
                    .scope_variable => |index| delivered = self.variableDictionary(index, self.program.module(requirement.module).nameStore().methodNameText(requirement.method)),
                    .derived => invariant("a derived codec or map dictionary is resolved from a type alone, but checking records each with its plan at the dispatch it serves"),
                    .target => |target| {
                        const instance = try self.program.targetAt(target, requirement.callable);
                        var hidden: std.ArrayList(LocalId) = .empty;
                        for (instance.bound) |ty| try hidden.append(self.allocator, try self.descOf(ty));
                        try frames.append(self.allocator, .{ .target = target, .callable = requirement.callable, .instance = instance, .hidden = hidden, .next = 0 });
                    },
                }
            }
            if (delivered) |dictionary| {
                delivered = null;
                if (frames.items.len == 0) return dictionary;
                try frames.items[frames.items.len - 1].hidden.append(self.allocator, dictionary);
            }
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next < frame.instance.requirements.len) {
                pending = frame.instance.requirements[frame.next];
                frame.next += 1;
                continue;
            }
            var done = frames.pop().?;
            defer done.hidden.deinit(self.allocator);
            const target_worker = self.program.worker(done.target).*;
            const dictionary = try self.composeDictionary(try self.program.methodEntryWorker(done.target), done.hidden.items);
            const own_fn = try self.program.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
            delivered = try self.adaptDictionary(dictionary, try self.program.substitute(self.allocator, own_fn, done.instance.bound), done.callable);
        }
    }

    /// The dictionary this body's scopes hold for method `method_text` on
    /// scope variable `index`.
    fn variableDictionary(self: *Builder, index: u32, method_text: []const u8) LocalId {
        const worker = self.currentWorker().*;
        if (index >= worker.scheme_vars.len) invariant("a dispatcher variable is outside its worker's scope");
        const variable = worker.scheme_vars[index];
        const names = self.view.nameStore();
        var offset: usize = 0;
        for (worker.dict_scopes, worker.dict_scope_params) |scope, params| {
            for (scope, 0..) |param_index, position| {
                const param = params[param_index];
                if (param.dispatcher_ty == variable and std.mem.eql(u8, names.methodNameText(param.method), method_text)) return self.dict_locals[offset + position];
            }
            offset += scope.len;
        }
        invariant("a scope variable's method requirement has no dictionary in its scope, which checking requires");
    }

    /// The dictionary of a derived map with the checked selection `plan`,
    /// at the function type `fn_ty` (`input, transform -> output`): its
    /// worker captures the descriptors of the input and output unions, of
    /// the selected variant's payloads, and of the transformed payload.
    pub fn structuralMapDictionary(self: *Builder, plan: static_dispatch.DerivedMapPlan, effectful: bool, fn_ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const function = switch (store.get(fn_ty)) {
            .function => |value| value,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a derived map's callable type is not a function type"),
        };
        const args = try self.allocator.dupe(TypeId, store.span(function.args));
        defer self.allocator.free(args);
        if (args.len != 2) invariant("a derived map does not take a value and a transform");
        const transform = switch (store.get(args[1])) {
            .function => |value| value,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a derived map's transform is not a function"),
        };
        const mapped = transform.ret;
        const name = try store.internName(self.view.nameStore().tagLabelText(plan.tag));
        const input_union = switch (store.get(try self.structuralType(args[0]))) {
            .tag_union => |value| value,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a derived map's input is not a tag union"),
        };
        const payload_types: []const TypeId = for (store.tagSpan(input_union.tags)) |tag| {
            if (tag.name == name) break try self.allocator.dupe(TypeId, store.span(tag.payloads));
        } else invariant("a derived map's selected tag is not in its input row");
        defer self.allocator.free(payload_types);
        if (plan.payload_index >= payload_types.len) invariant("a derived map's selected payload is past its tag's payloads");
        const map: lower.StructuralMap = .{ .name = name, .payload_index = plan.payload_index, .arity = @intCast(payload_types.len), .effectful = effectful };
        const descs = try self.allocator.alloc(LocalId, 3 + payload_types.len);
        defer self.allocator.free(descs);
        descs[0] = try self.descOf(args[0]);
        descs[1] = try self.descOf(function.ret);
        for (payload_types, descs[2..][0..payload_types.len]) |ty, *desc| desc.* = try self.descOf(ty);
        descs[2 + payload_types.len] = try self.descOf(mapped);
        return try self.composeDictionary(try self.program.structuralMapWorker(self.module, map), descs);
    }

    /// The hidden descriptors of a target the checker instantiated by
    /// relating its scheme to `callable` (an edge with no recorded
    /// substitution, `EvidenceTargetInstantiation.derived_from_callable`).
    /// The hidden arguments of `callee`, the procedure a dispatch edge
    /// reached through the exact procedure alias the method is bound to
    /// (`EvidenceTargetInstantiation.alias`). The edge instantiated the
    /// alias's scope (`node.subst`) and resolved its requirements
    /// (`through_alias`); each alias's forwarded lookup recorded its own
    /// substitution and evidence over that scope, so lowering the lookup's
    /// in the scope so instantiated gives the next alias's, alias by alias,
    /// up to the procedure the chain reaches.
    pub fn aliasTargetHidden(self: *Builder, node: static_dispatch.EvidenceNode, callee: WorkerId) Allocator.Error![]LocalId {
        return (try self.aliasTargetInstance(node, callee)).hidden;
    }

    /// `aliasTargetHidden` with the instantiation the chain's last lookup
    /// gives the target.
    pub fn aliasTargetInstance(self: *Builder, node: static_dispatch.EvidenceNode, callee: WorkerId) Allocator.Error!CallInstance {
        const table = self.view.view.static_dispatch_plans;
        const alias = node.target.alias orelse invariant("an alias-instantiated target has no alias binding");
        const alias_module = self.program.moduleByIdx(node.target.module_idx).index;
        const arena = self.program.arena.allocator();
        var alias_step: AliasStep = switch (alias) {
            .top_level => |pattern| self.callableEvalAliasStep(alias_module, self.program.callable_eval_by_pattern.get(.{ .module = alias_module, .pattern = pattern }) orelse
                invariant("a method's alias binding has no callable-eval template")),
            .local => |local| blk: {
                const view = self.program.module(alias_module);
                const templates = view.view.checked_procedure_templates;
                const scope_id = local.dispatch_scope orelse break :blk AliasStep{ .module = alias_module, .lookup = local.lookup, .vars = &.{}, .params = &.{} };
                const scope = templates.dispatch_scopes[@backingInt(scope_id)];
                break :blk .{
                    .module = alias_module,
                    .lookup = local.lookup,
                    .vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, scope.scheme_vars),
                    .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, scope.evidence_params),
                };
            },
        };
        // The edge's instantiation of the first alias's scope, in this body.
        const node_subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
        if (node_subst.len != alias_step.vars.len) invariant("an alias edge's substitution differs from its alias's scope");
        var bound = try arena.alloc(TypeId, node_subst.len);
        for (node_subst, bound) |checked_ty, *ty| ty.* = try self.convert(checked_ty);
        var dicts = try self.scopeDictionaries(alias_step.params, table.aliasScopeEvidence(node));

        const saved_module = self.module;
        const saved_view = self.view;
        const saved_env = self.env;
        const saved_foreign = self.foreign_scopes;
        const saved_scopes = self.alias_scopes.items.len;
        defer {
            self.module = saved_module;
            self.view = saved_view;
            self.env = saved_env;
            self.foreign_scopes = saved_foreign;
            self.alias_scopes.shrinkRetainingCapacity(saved_scopes);
        }
        while (true) {
            self.module = alias_step.module;
            self.view = self.program.module(alias_step.module);
            self.env = .{ .module = alias_step.module, .vars = alias_step.vars, .bound = bound };
            self.alias_scopes.shrinkRetainingCapacity(saved_scopes);
            try self.alias_scopes.append(self.allocator, .{ .params = runtimeParams(arena, alias_step.params) catch return error.OutOfMemory, .dicts = dicts, .saved_env = self.env });
            self.foreign_scopes = saved_scopes;
            const lookup_subst = self.siteSubstitution(alias_step.lookup);
            const evidence = self.view.view.static_dispatch_plans.siteEvidence(alias_step.lookup) orelse &.{};
            const ref_id = self.view.view.resolved_value_refs.by_checked_expr[@backingInt(alias_step.lookup)].get() orelse
                invariant("a forwarded procedure alias lookup has no resolved reference");
            const next: AliasStep = switch (self.view.view.resolved_value_refs.records[@backingInt(ref_id)].ref) {
                .local_proc => |local| if (local.is_alias) blk: {
                    const templates = self.view.view.checked_procedure_templates;
                    const scope_id = local.dispatch_scope orelse break :blk AliasStep{ .module = alias_step.module, .lookup = local.expr, .vars = &.{}, .params = &.{} };
                    const scope = templates.dispatch_scopes[@backingInt(scope_id)];
                    break :blk .{
                        .module = alias_step.module,
                        .lookup = local.expr,
                        .vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, scope.scheme_vars),
                        .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, scope.evidence_params),
                    };
                } else invariant("a method's alias chain reaches a local procedure, which a method target never is"),
                inline .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc, .platform_required_proc => |ref, tag| blk: {
                    const use = if (tag == .platform_required_proc) ref.procedure else ref;
                    const resolved = self.program.procedureBinding(use.binding);
                    switch (resolved.body) {
                        .callable_eval_template => |template_id| {
                            const template = self.program.module(resolved.module).view.callable_eval_templates.templates[@backingInt(template_id)];
                            if (template.forwarded_lookup != null) break :blk self.callableEvalAliasStep(resolved.module, template_id);
                            invariant("a method's alias chain reaches a computed callable, which a method target never is");
                        },
                        .direct_template => {},
                        .checked_error => invariant("a method's alias chain reaches a rejected binding"),
                    }
                    // The procedure the chain reaches: its substitution and
                    // evidence are the lookup's.
                    const target = self.program.worker(callee).*;
                    const target_subst = try self.program.keptSubstitution(callee, lookup_subst);
                    if (target_subst.len != target.scheme_vars.len) invariant("an alias's forwarded lookup's substitution differs from its target's scheme");
                    const target_bound = try arena.alloc(TypeId, target_subst.len);
                    for (target_subst, target_bound) |checked_ty, *ty| ty.* = try self.convert(checked_ty);
                    const hidden = try self.allocator.alloc(LocalId, target_subst.len + target.dictionary_params.len);
                    errdefer self.allocator.free(hidden);
                    for (target_bound, hidden[0..target_subst.len]) |ty, *desc| desc.* = try self.descOf(ty);
                    for (target.dictionary_params, hidden[target_subst.len..]) |param_index, *dict| {
                        if (param_index >= evidence.len) invariant("an alias's forwarded lookup's evidence does not cover its target's dictionaries");
                        dict.* = try self.dictionaryOf(evidence[param_index]);
                    }
                    return .{ .hidden = hidden, .bound = target_bound, .ret = try self.program.substitute(self.allocator, target.ret, target_bound) };
                },
                .local_param, .local_value, .local_mutable_version, .pattern_binder, .selected_hoisted_const, .top_level_const, .imported_const, .platform_required_declaration, .platform_required_checked_error, .platform_required_const => invariant("a forwarded procedure alias lookup names no procedure"),
            };
            if (lookup_subst.len != next.vars.len) invariant("an alias's forwarded lookup's substitution differs from the next alias's scope");
            const next_bound = try arena.alloc(TypeId, lookup_subst.len);
            for (lookup_subst, next_bound) |checked_ty, *ty| ty.* = try self.convert(checked_ty);
            dicts = try self.scopeDictionaries(next.params, evidence);
            bound = next_bound;
            alias_step = next;
        }
    }

    /// The step of an alias chain through the exact procedure alias
    /// `template_id` of `module_index`: its forwarded lookup in its entry
    /// wrapper's scope.
    fn callableEvalAliasStep(self: *Builder, module_index: ModuleIndex, template_id: checked.CallableEvalTemplateId) AliasStep {
        const view = self.program.module(module_index);
        const template = view.view.callable_eval_templates.templates[@backingInt(template_id)];
        const wrapper = self.program.wrapper_by_root.get(.{ .module = module_index, .root = template.root }) orelse
            invariant("a callable-eval template's root has no entry wrapper");
        const owner = self.program.moduleByKey(.{ .bytes = wrapper.artifact.bytes });
        const templates = owner.view.checked_procedure_templates;
        const wrapper_template = templates.templates.items[@backingInt(wrapper.template)];
        return .{
            .module = module_index,
            .lookup = template.forwarded_lookup orelse invariant("a method's alias binding is not an exact procedure alias"),
            .vars = lower.poolSlice(checked.CheckedTypeId, templates.scheme_vars_pool, wrapper_template.scheme_vars),
            .params = lower.poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, wrapper_template.evidence_params),
        };
    }

    /// The dictionaries of the runtime parameters among `params`, from
    /// `evidence` (one entry per parameter).
    fn scopeDictionaries(self: *Builder, params: []const static_dispatch.EvidenceParamRecord, evidence: []const static_dispatch.CheckedEvidence) Allocator.Error![]const LocalId {
        if (evidence.len != params.len) invariant("an alias scope's evidence differs from its requirements");
        var dicts: std.ArrayList(LocalId) = .empty;
        for (params, evidence) |param, entry| {
            if (param.runtime_dictionary) try dicts.append(self.program.arena.allocator(), try self.dictionaryOf(entry));
        }
        return dicts.items;
    }

    fn runtimeParams(arena: Allocator, params: []const static_dispatch.EvidenceParamRecord) Allocator.Error![]const u32 {
        var indices: std.ArrayList(u32) = .empty;
        for (params, 0..) |param, index| {
            if (param.runtime_dictionary) try indices.append(arena, @intCast(index));
        }
        return indices.items;
    }

    pub fn relatedHiddenArgs(self: *Builder, callee: WorkerId, callable: checked.CheckedTypeId) Allocator.Error![]LocalId {
        return try self.callableDerivedHidden(callee, &.{}, try self.convert(callable));
    }

    /// The hidden arguments of `callee` instantiated at `subst` (or, with
    /// none recorded, at what `call_fn` relates its variables to), its
    /// dictionaries derived from those types: each requirement dispatches on
    /// its dispatcher's type at the instantiation, as checking selects
    /// targets.
    pub fn callableDerivedHidden(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, call_fn: ?TypeId) Allocator.Error![]LocalId {
        return (try self.callableDerivedInstance(callee, subst, call_fn)).hidden;
    }

    /// `callableDerivedHidden` with the instantiation it computed.
    pub fn callableDerivedInstance(self: *Builder, callee: WorkerId, subst: []const checked.CheckedTypeId, call_fn: ?TypeId) Allocator.Error!CallInstance {
        const target = self.program.worker(callee).*;
        const owned = try self.boundTypesAt(callee, subst, call_fn);
        defer self.allocator.free(owned);
        const bound = try self.program.arena.allocator().dupe(TypeId, owned);
        const hidden = try self.allocator.alloc(LocalId, bound.len + target.dictionary_params.len);
        errdefer self.allocator.free(hidden);
        for (bound, hidden[0..bound.len]) |ty, *desc| desc.* = try self.descOf(ty);
        const ret = try self.program.substitute(self.allocator, target.ret, bound);
        if (target.dictionary_params.len == 0) return .{ .hidden = hidden, .bound = bound, .ret = ret };
        const instance = try self.program.targetRequirements(callee, bound);
        for (instance.requirements, hidden[bound.len..]) |requirement, *dict| dict.* = try self.dictionaryFromType(requirement);
        return .{ .hidden = hidden, .bound = bound, .ret = ret };
    }

    /// Whether stored type evidence `root` of `source` holds an erased
    /// function type, which names no signature.
    fn constTypeHasErased(self: *Builder, source: lower.ConstSource, root: check.ConstStore.ConstTypeId) Allocator.Error!bool {
        const const_types = &self.program.constStore(source).type_store;
        var stack: std.ArrayList(check.ConstStore.ConstTypeId) = .empty;
        defer stack.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(check.ConstStore.ConstTypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        try stack.append(self.allocator, root);
        while (stack.pop()) |id| {
            if ((try seen.getOrPut(self.allocator, id)).found_existing) continue;
            switch (const_types.get(id)) {
                .erased => return true,
                .primitive, .zst => {},
                .list, .box => |child| try stack.append(self.allocator, child),
                .tuple => |range| try stack.appendSlice(self.allocator, const_types.typeSpan(range)),
                .record => |range| for (const_types.fieldSpan(range)) |field| {
                    try stack.append(self.allocator, field.ty);
                    if (field.value_ty) |value_ty| try stack.append(self.allocator, value_ty);
                },
                .tag_union => |range| for (const_types.tagSpan(range)) |tag| try stack.appendSlice(self.allocator, const_types.typeSpan(tag.payloads)),
                .func => |func| {
                    try stack.appendSlice(self.allocator, const_types.typeSpan(func.args));
                    try stack.append(self.allocator, func.ret);
                },
                .named => |named| try stack.appendSlice(self.allocator, const_types.typeSpan(named.args)),
            }
        }
        return false;
    }

    /// The Boxy type of a stored constant's type evidence `root` in the
    /// constant store of `source`.
    pub fn constType(self: *Builder, source: lower.ConstSource, root: check.ConstStore.ConstTypeId) Allocator.Error!TypeId {
        const const_types = &self.program.constStore(source).type_store;
        const names = self.program.constNames(source);
        const store = &self.program.store;
        var done: collections.DenseMapUnmanaged(check.ConstStore.ConstTypeId, TypeId) = .empty;
        defer done.deinit(self.allocator);
        const Frame = struct { id: check.ConstStore.ConstTypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .id = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (done.contains(frame.id)) continue;
            const ty = const_types.get(frame.id);
            if (!frame.expanded) {
                try stack.append(self.allocator, .{ .id = frame.id, .expanded = true });
                switch (ty) {
                    .list, .box => |child| try stack.append(self.allocator, .{ .id = child, .expanded = false }),
                    .tuple => |range| for (const_types.typeSpan(range)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false }),
                    .record => |range| for (const_types.fieldSpan(range)) |field| {
                        try stack.append(self.allocator, .{ .id = field.ty, .expanded = false });
                        if (field.value_ty) |value_ty| try stack.append(self.allocator, .{ .id = value_ty, .expanded = false });
                    },
                    .tag_union => |range| for (const_types.tagSpan(range)) |tag| {
                        for (const_types.typeSpan(tag.payloads)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false });
                    },
                    .func => |func| {
                        for (const_types.typeSpan(func.args)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false });
                        try stack.append(self.allocator, .{ .id = func.ret, .expanded = false });
                    },
                    .named => |named| {
                        for (const_types.typeSpan(named.args)) |child| try stack.append(self.allocator, .{ .id = child, .expanded = false });
                        // A stored alias is its backing.
                        if (named.kind == .alias) {
                            const backing = named.backing orelse invariant("a stored alias has no backing type");
                            try stack.append(self.allocator, .{ .id = backing.ty, .expanded = false });
                        }
                    },
                    .primitive, .erased, .zst => {},
                }
                continue;
            }
            const converted: TypeId = switch (ty) {
                .primitive => |primitive| try store.primitive(primitive),
                .zst => try store.emptyRecord(),
                .list => |child| try store.intern(.{ .list = done.get(child).? }),
                .box => |child| try store.intern(.{ .box = done.get(child).? }),
                .tuple => |range| blk: {
                    const items = try self.allocator.alloc(TypeId, range.len);
                    defer self.allocator.free(items);
                    for (const_types.typeSpan(range), items) |child, *item| item.* = done.get(child).?;
                    break :blk try store.intern(.{ .tuple = items });
                },
                .record => |range| blk: {
                    const fields = try self.allocator.alloc(types.Field, range.len);
                    defer self.allocator.free(fields);
                    for (const_types.fieldSpan(range), fields) |field, *out| {
                        // An optional field is stored as its presence slot
                        // over the value type; a defaulted field is stored
                        // inline like a required one.
                        out.* = if (field.value_ty) |value_ty|
                            .{ .name = try store.internName(names.recordFieldLabelText(field.name)), .ty = done.get(value_ty).?, .kind = .presence_slot }
                        else
                            .{ .name = try store.internName(names.recordFieldLabelText(field.name)), .ty = done.get(field.ty).?, .kind = .inline_slot };
                    }
                    std.mem.sort(types.Field, fields, store, struct {
                        fn lessThan(s: *types.Store, a: types.Field, b: types.Field) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk if (fields.len == 0) try store.emptyRecord() else try store.intern(.{ .record = .{ .fields = fields, .ext = null } });
                },
                .tag_union => |range| blk: {
                    const tags = const_types.tagSpan(range);
                    const inputs = try self.allocator.alloc(types.TagInput, tags.len);
                    defer self.allocator.free(inputs);
                    var payload_store: std.ArrayList(TypeId) = .empty;
                    defer payload_store.deinit(self.allocator);
                    for (tags) |tag| {
                        for (const_types.typeSpan(tag.payloads)) |child| try payload_store.append(self.allocator, done.get(child).?);
                    }
                    var at: usize = 0;
                    for (tags, inputs) |tag, *input| {
                        input.* = .{ .name = try store.internName(names.tagLabelText(tag.checked_name)), .payloads = payload_store.items[at..][0..tag.payloads.len] };
                        at += tag.payloads.len;
                    }
                    std.mem.sort(types.TagInput, inputs, store, struct {
                        fn lessThan(s: *types.Store, a: types.TagInput, b: types.TagInput) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk try store.intern(.{ .tag_union = .{ .tags = inputs, .ext = null } });
                },
                .func => |func| blk: {
                    const args = try self.allocator.alloc(TypeId, func.args.len);
                    defer self.allocator.free(args);
                    for (const_types.typeSpan(func.args), args) |child, *arg| arg.* = done.get(child).?;
                    break :blk try store.intern(.{ .function = .{ .args = args, .ret = done.get(func.ret).?, .effectful = false } });
                },
                // The stored definition names the nominal's declaration by
                // its declaring module's content identity and its source
                // statement, as Monotype reads it; the stored arguments are
                // what it is applied to. A stored type never names a type
                // store of another module: the program a value was evaluated
                // in need not be the one restoring it.
                .named => |named| blk: {
                    if (named.kind == .alias) break :blk done.get(named.backing.?.ty).?;
                    if (named.def.generated != null) invariant("a stored type names a generated nominal, which only specialization mints");
                    const identity = names.moduleIdentityBytes(named.def.module).*;
                    const declaring = self.program.by_identity.get(identity) orelse
                        invariant("a stored type's declaring module is not in this lowering");
                    const source_decl = named.def.source_decl orelse invariant("a stored nominal type names no declaring statement");
                    const view = self.program.module(declaring);
                    const declaration_id = try self.program.store.declarationBySourceDecl(view, source_decl) orelse
                        invariant("a stored type names a declaration its module does not make");
                    const declaration = view.types().nominalDeclarationById(declaration_id);
                    const args = try self.allocator.alloc(TypeId, named.args.len);
                    defer self.allocator.free(args);
                    for (const_types.typeSpan(named.args), args) |child, *arg| arg.* = done.get(child).?;
                    break :blk try self.program.converter().nominalAt(declaring, declaration.declaration_root, args, self.env);
                },
                // Only a closure's capture holds an erased function type, and
                // its restore relates no such capture (`constTypeHasErased`).
                .erased => invariant("a stored type converted for Boxy holds an erased function type, which only captures, which relate none, hold"),
            };
            try done.put(self.allocator, frame.id, converted);
        }
        return done.get(root).?;
    }

    /// The types that variables `0..count` of `formal` take in `actual`: each
    /// variable takes the component of `actual` at the position the variable
    /// has in `formal`. An open row's remainder variable takes the actual
    /// row's entries the formal row does not name.
    pub fn relateVariables(self: *Builder, formal_root: TypeId, actual_root: TypeId, count: usize) Allocator.Error![]TypeId {
        return try self.relateVariablesAll(&.{.{ formal_root, actual_root }}, count);
    }

    /// `relateVariables` over several formal and actual type pairs at once.
    pub fn relateVariablesAll(self: *Builder, roots: []const [2]TypeId, count: usize) Allocator.Error![]TypeId {
        const bound = try self.relateVariablesPartial(roots, count);
        defer self.allocator.free(bound);
        const result = try self.allocator.alloc(TypeId, count);
        for (bound, result) |ty, *slot| slot.* = ty orelse invariant("a related type leaves one of its variables unrelated");
        return result;
    }

    /// The relation of `relateVariablesAll`, leaving variables the types do
    /// not relate null.
    pub fn relateVariablesPartial(self: *Builder, roots: []const [2]TypeId, count: usize) Allocator.Error![]?TypeId {
        return try self.program.relateVariablesPartial(self.allocator, roots, count);
    }

    pub fn composeDictionary(self: *Builder, entry: WorkerId, captures: []const LocalId) Allocator.Error!LocalId {
        const local = try self.closedLocal(try self.erasedLayout());
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .dict_compose = .{
            .worker = self.program.worker(entry).proc,
            .captures = try self.store.addLocalSpan(captures),
        } }), .next = undefined } });
        return local;
    }

    /// The callable type the requirement of evidence parameter `index` of
    /// this worker's scopes names, at this body's types: what its
    /// dictionary's method is called at. Null for an alias scope's
    /// parameter.
    pub fn scopeInterface(self: *Builder, index: static_dispatch.EvidenceChainIndex) Allocator.Error!?TypeId {
        if (self.foreign_scopes != null) return null;
        const aliases = self.alias_scopes.items.len;
        if (index.depth < aliases) return null;
        const depth = index.depth - aliases;
        const worker = self.currentWorker().*;
        if (depth >= worker.dict_scope_params.len) invariant("evidence named a generalized scope outside its worker's scopes");
        const params = worker.dict_scope_params[depth];
        if (index.index >= params.len) invariant("evidence named a parameter its scope does not have");
        return try self.convert(params[index.index].callable_ty);
    }

    /// The dictionary in scope for checked evidence parameter `index`.
    /// Depth 0 is the innermost scope: an entered scheme alias's, then the
    /// worker's own.
    pub fn scopeDictionary(self: *Builder, index: static_dispatch.EvidenceChainIndex) LocalId {
        if (self.foreign_scopes) |first| {
            const scopes = self.alias_scopes.items[first..];
            if (index.depth >= scopes.len) invariant("an exact procedure alias's lookup names evidence outside the alias's scope");
            const scope = scopes[scopes.len - 1 - index.depth];
            for (scope.params, scope.dicts) |param_index, dict| {
                if (param_index == index.index) return dict;
            }
            invariant("evidence named a dictionary parameter its alias scope does not have");
        }
        const aliases = self.alias_scopes.items;
        if (index.depth < aliases.len) {
            const scope = aliases[aliases.len - 1 - index.depth];
            for (scope.params, scope.dicts) |param_index, dict| {
                if (param_index == index.index) return dict;
            }
            invariant("evidence named a dictionary parameter its alias scope does not have");
        }
        const depth = index.depth - aliases.len;
        const scopes = self.currentWorker().dict_scopes;
        if (depth >= scopes.len) invariant("evidence named a generalized scope outside its worker's scopes");
        var offset: usize = 0;
        for (scopes[0..depth]) |scope| offset += scope.len;
        for (scopes[depth], 0..) |param_index, position| {
            if (param_index == index.index) return self.dict_locals[offset + position];
        }
        invariant("evidence named a dictionary parameter its scope does not have");
    }

    pub fn siteSubstitution(self: *Builder, site: CheckedExprId) []const checked.CheckedTypeId {
        return self.view.view.static_dispatch_plans.siteSubstitution(site) orelse &.{};
    }

    /// When the statement emitted last is a call whose target is `value`,
    /// a dynamic frame local, make `result` its target instead.
    pub fn retargetLastCall(self: *Builder, value: LocalId, result: LocalId) Allocator.Error!bool {
        const last = self.last_stmt orelse return false;
        const dyn = self.store.localDyn(value) orelse return false;
        if (dyn.storage != .frame) return false;
        var stmt = self.store.getCFStmt(last);
        switch (stmt) {
            .assign_call => |*call| {
                if (call.target != value or call.next != self.hole.?) return false;
                call.target = result;
            },
            .dyn => |*op| {
                if (op.target != value or op.next != self.hole.?) return false;
                if (self.store.stmtRow(op.data) != .call_erased) return false;
                op.target = result;
            },
            .init_uninitialized, .assign_ref, .assign_literal, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .incref, .decref, .decref_if_initialized, .free, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret, .crash => return false,
        }
        try self.store.replaceCFStmt(last, stmt, self.store.stmtOrigin(last));
        return true;
    }

    /// A direct call of `callee` with the hidden arguments `hidden`, whose
    /// result the callee writes at its own result type at this
    /// instantiation, `ret`; the call's own type `result_ty` may widen a
    /// sealed row of it, which the result is converted to (design.md
    /// "Sealed Result Rows").
    pub fn finishCall(self: *Builder, callee: WorkerId, hidden: []const LocalId, arg_count: u32, ret: TypeId, result_ty: TypeId) Allocator.Error!void {
        const explicit = try self.popValues(arg_count);
        defer self.allocator.free(explicit);
        const args = try std.mem.concat(self.allocator, LocalId, &.{ explicit, hidden });
        defer self.allocator.free(args);
        const target = try self.localOf(ret, .frame);
        const proc = self.program.worker(callee).proc;
        try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = proc, .args = try self.store.addLocalSpan(args), .next = undefined } });
        try self.pushValue(try self.adaptValue(target, ret, result_ty));
    }

    pub fn dispatchPlan(self: *const Builder, id: static_dispatch.StaticDispatchPlanId) static_dispatch.StaticDispatchCallPlan {
        return self.view.view.static_dispatch_plans.plans[@backingInt(id)];
    }

    pub fn lowerDispatch(self: *Builder, expr_id: CheckedExprId, plan_id: static_dispatch.StaticDispatchPlanId) Allocator.Error!void {
        try self.lowerDispatchAt(plan_id, try self.exprType(expr_id));
    }

    /// Push a dispatch operand: a checked expression, or a value the checker
    /// generated for a literal conversion.
    pub fn pushOperand(self: *Builder, operand: static_dispatch.StaticDispatchOperand, ty: TypeId) Allocator.Error!void {
        switch (operand) {
            .checked_expr => |expr| try self.push(.{ .expr = expr }),
            .generated_numeral => |literal| try self.push(.{ .numeral_arg = .{ .literal = literal, .ty = ty } }),
            .generated_quote => |literal| try self.push(.{ .push_value_str = literal }),
            .generated_interpolation_segments => |expr| try self.push(.{ .segments_arg = .{ .expr = expr, .ty = ty } }),
        }
    }

    pub fn operandTypes(self: *Builder, callable_ty: checked.CheckedTypeId) Allocator.Error![]const TypeId {
        return switch (self.program.store.get(try self.convert(callable_ty))) {
            .function => |function| self.program.store.span(function.args),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a dispatch's callable type is not a function type"),
        };
    }

    pub fn lowerDispatchAt(self: *Builder, plan_id: static_dispatch.StaticDispatchPlanId, result_ty: TypeId) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const call_plan = self.dispatchPlan(plan_id);
        const operands = call_plan.argsSlice(table);
        const operand_types = try self.allocator.dupe(TypeId, try self.operandTypes(call_plan.callable_ty));
        defer self.allocator.free(operand_types);
        if (operand_types.len != operands.len) invariant("a dispatch's operand count differs from its callable type");
        switch (call_plan.resolution) {
            .direct_closed, .direct_parametric => |direct| {
                const node = table.evidenceNode(direct.evidence);
                if (node.target.kind == .local_proc) {
                    // A method declared in a function body is a call of
                    // its local procedure's function value.
                    const fn_ty = try self.convert(call_plan.callable_ty);
                    const value = try self.localMethodValue(node, fn_ty);
                    switch (call_plan.result_mode) {
                        .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                        .equality => |equality| if (equality.negated) {
                            try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                        },
                    }
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = if (call_plan.result_mode == .equality) try self.program.store.primitive(.bool) else result_ty } });
                    var index = operands.len;
                    while (index > 0) {
                        index -= 1;
                        try self.pushOperand(operands[index], operand_types[index]);
                    }
                    try self.pushValue(value);
                    return;
                }
                const callee = try self.methodTargetWorker(node.target);
                const instance = switch (node.instantiation) {
                    // Reached through a procedure alias, the target takes the
                    // instantiation the call's callable type gives it.
                    .alias => try self.aliasTargetInstance(node, callee),
                    .monomorphic, .callable, .derived_from_callable => blk: {
                        const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                        break :blk switch (node.nested) {
                            .resolved => try self.callInstanceAt(callee, subst, table.nestedEvidence(node), try self.convert(call_plan.callable_ty)),
                            .from_callable => try self.callableDerivedInstance(callee, subst, try self.convert(call_plan.callable_ty)),
                            .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                        };
                    },
                };
                switch (call_plan.result_mode) {
                    // A codec method is an ordinary call of its target.
                    .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                    .equality => |equality| if (equality.negated) {
                        try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                    },
                }
                try self.push(.{ .finish_call = .{ .callee = callee, .hidden = instance.hidden, .arg_count = @intCast(operands.len), .ret = instance.ret, .result = if (call_plan.result_mode == .equality) try self.program.store.primitive(.bool) else result_ty } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.pushOperand(operands[index], operand_types[index]);
                }
            },
            .structural => |derivation| switch (derivation) {
                .hash => {
                    if (operands.len != 2) invariant("structural hashing has a value and a hasher");
                    try self.push(.{ .finish_hash = .{ .value = operand_types[0], .result = result_ty } });
                    try self.pushOperand(operands[1], operand_types[1]);
                    try self.pushOperand(operands[0], operand_types[0]);
                },
                .equality => {
                    const negated = switch (call_plan.result_mode) {
                        .equality => |equality| equality.negated,
                        .value, .hash, .parser_for, .encoder_for, .map, .map_effectful => false,
                    };
                    if (operands.len != 2) invariant("structural equality has two operands");
                    const lhs = switch (operands[0]) {
                        .checked_expr => |operand| operand,
                        .generated_interpolation_segments, .generated_numeral, .generated_quote => invariant("structural equality operand is not an expression"),
                    };
                    const rhs = switch (operands[1]) {
                        .checked_expr => |operand| operand,
                        .generated_interpolation_segments, .generated_numeral, .generated_quote => invariant("structural equality operand is not an expression"),
                    };
                    try self.push(.{ .finish_eq = .{ .negated = negated, .operand = try self.exprType(lhs) } });
                    try self.push(.{ .expr = rhs });
                    try self.push(.{ .expr = lhs });
                },
                .parser, .encoder => try Codec.dispatch(self, call_plan, operands, operand_types, result_ty),
                .map, .map_effectful => |plan| {
                    // A derived map is a call of its dictionary.
                    const fn_ty = try self.convert(call_plan.callable_ty);
                    const dict = try self.structuralMapDictionary(plan, derivation == .map_effectful, fn_ty);
                    try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = result_ty } });
                    var index = operands.len;
                    while (index > 0) {
                        index -= 1;
                        try self.pushOperand(operands[index], operand_types[index]);
                    }
                    try self.pushValue(dict);
                },
            },
            .evidence_dependent => |dependent| {
                const dict = self.scopeDictionary(dependent.index);
                // The dictionary's method is called at the interface its
                // scope's requirement names; its result is read at the
                // dispatch's type.
                const fn_ty = (try self.scopeInterface(dependent.index)) orelse try self.convert(call_plan.callable_ty);
                switch (call_plan.result_mode) {
                    // A codec method is an ordinary call of its target.
                    .value, .parser_for, .encoder_for, .hash, .map, .map_effectful => {},
                    .equality => |equality| if (equality.negated) {
                        try self.push(.{ .finish_low_level = .{ .op = .bool_not, .arg_count = 1, .result = result_ty } });
                    },
                }
                const call_result = switch (self.program.store.get(try self.convert(call_plan.callable_ty))) {
                    .function => |function| function.ret,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a dispatch's callable type is not a function type"),
                };
                try self.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = @intCast(operands.len), .result = call_result } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.pushOperand(operands[index], operand_types[index]);
                }
                try self.pushValue(dict);
            },
            // A dispatch that cannot run evaluates its operands, each for its
            // effects, then crashes.
            .checked_error, .@"unreachable" => {
                try self.push(.{ .dispatch_crash = .{
                    .kind = if (call_plan.resolution == .checked_error) .checked_error else .unreachable_value,
                    .result = result_ty,
                } });
                var index = operands.len;
                while (index > 0) {
                    index -= 1;
                    try self.push(.discard);
                    try self.pushOperand(operands[index], operand_types[index]);
                }
            },
            .direct_pending => invariant("a checked module holds a pending dispatch resolution"),
        }
    }

    /// Emit a low-level operation. An operation over values of open types
    /// is a list operation, which takes the element descriptor of its list
    /// as a trailing argument (`LowLevelExtra.dyn_elem_desc`), or a box
    /// operation, which lowers to the dynamic box statements.
    pub fn finishLowLevel(self: *Builder, op: LIR.LowLevel, arg_count: u32, result_ty: TypeId, arg_types: []const TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        const store = &self.program.store;
        var open = !store.isClosed(result_ty);
        for (arg_types) |arg_ty| open = open or !store.isClosed(arg_ty);
        if (op == .list_map_can_reuse) return try self.pushValue(try self.listMapCanReuse(args, arg_types, result_ty));
        if (op == .list_prefetched) {
            // The result is the list itself, so it is the argument; the
            // hint is a separate statement that borrows it.
            if (args.len != 2) invariant("list_prefetched reached Boxy lowering with the wrong arity");
            const hint = try self.closedLocal(.zst);
            if (open) {
                const elem = switch (store.get(arg_types[0])) {
                    .list => |elem| elem,
                    .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("list_prefetched's first operand is not a List"),
                };
                try self.emitLinear(.{ .assign_low_level = .{
                    .target = hint,
                    .op = .list_prefetch,
                    .rc_effect = try self.store.addRcEffect(LIR.LowLevel.list_prefetch.rcEffect()),
                    .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }),
                    .args = try self.store.addLocalSpan(&.{ args[0], args[1], try self.descOf(elem) }),
                    .next = undefined,
                } });
            } else {
                try self.emitLowLevel(hint, .list_prefetch, args);
            }
            return try self.pushValue(args[0]);
        }
        if (op == .crash) {
            // A crash ends its block; the result stands for the value no
            // execution reaches.
            try self.emitTerminal(.{ .crash = .{ .msg = .{ .local = args[0] } } });
            return try self.pushValue(try self.localOf(result_ty, .frame));
        }
        // `Box(function)` is the function's erased callable: boxing or
        // unboxing one is the callable itself.
        switch (op) {
            .box_box => if (store.get(result_ty) == .box and store.get(store.get(result_ty).box) == .function) return try self.pushValue(args[0]),
            .box_unbox, .box_unbox_borrowed => if (store.get(result_ty) == .function) return try self.pushValue(args[0]),
            .str_is_eq, .str_is_eq_static_small, .str_static_small_word_eq, .str_static_small_word_caseless_eq, .str_concat, .str_contains, .str_trim, .str_trim_start, .str_trim_end, .str_caseless_ascii_equals, .str_with_ascii_lowercased, .str_with_ascii_uppercased, .str_starts_with, .str_ends_with, .str_repeat, .str_drop_prefix, .str_drop_prefix_caseless_ascii, .str_drop_suffix, .str_split_first, .str_split_last, .str_count_utf8_bytes, .str_get_utf8_byte_unsafe, .str_substring_unsafe, .str_with_capacity, .str_reserve, .str_release_excess_capacity, .str_to_utf8, .str_from_utf8_lossy, .str_from_utf8_validated, .str_from_utf16_le_short, .str_from_utf16_be_short, .str_from_utf32_le_short, .str_from_utf32_be_short, .str_from_utf8, .str_split_on, .str_join_with, .str_inspect, .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str, .dec_to_str, .f32_to_str, .f64_to_str, .list_len, .list_capacity, .list_get_unsafe, .list_prefetched, .list_prefetch, .list_append_unsafe, .list_concat, .list_append_range_within, .list_copy_range_within, .list_append_range_within_unsafe, .list_append_sublist, .list_append_le_bytes, .list_append_le_bytes_unsafe, .list_slack_unique, .list_owned_unique, .list_set_in_place_unsafe, .list_with_capacity, .list_drop_at, .list_sublist, .list_sublist_borrowed, .list_set, .list_replace_unsafe, .list_swap, .list_prepend, .list_take_first, .list_take_last, .list_reverse, .list_reserve, .list_reserve_for_append, .list_release_excess_capacity, .list_clear, .list_map_prepare_reuse, .list_map_can_reuse, .list_map_cast_unsafe, .list_map_extract_unsafe, .list_map_write_unsafe, .list_item_size_at_most, .bool_not, .bool_likely, .dict_pseudo_seed, .hasher_finish, .hasher_write_bool, .hasher_write_u8, .hasher_write_u16, .hasher_write_u32, .hasher_write_u64, .hasher_write_u128, .hasher_write_i8, .hasher_write_i16, .hasher_write_i32, .hasher_write_i64, .hasher_write_i128, .hasher_write_f32, .hasher_write_f64, .hasher_write_dec, .hasher_write_bytes, .hasher_write_str, .crypto_sha256_hash_bytes, .crypto_sha256_hasher_empty, .crypto_sha256_hasher_write, .crypto_sha256_hasher_finish, .crypto_blake3_hash_bytes, .crypto_blake3_hasher_empty, .crypto_blake3_hasher_write, .crypto_blake3_hasher_finish, .num_is_eq, .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte, .num_negate, .num_abs, .num_abs_diff, .num_plus, .num_minus, .num_times, .num_int_add_wrap, .num_int_add_crash_on_overflow, .num_int_add_overflows, .num_int_add_proven_cannot_overflow, .num_int_sub_wrap, .num_int_sub_crash_on_overflow, .num_int_sub_overflows, .num_int_sub_proven_cannot_overflow, .num_int_mul_wrap, .num_int_mul_crash_on_overflow, .num_int_mul_overflows, .num_int_mul_proven_cannot_overflow, .num_float_add, .num_float_sub, .num_float_mul, .dec_mul, .num_div_by, .num_div_by_checked, .num_div_trunc_by, .num_div_trunc_by_checked, .num_rem_by, .num_rem_by_checked, .num_mod_by, .num_mod_by_checked, .num_negate_checked, .num_abs_checked, .num_pow, .num_atan2, .num_sqrt, .num_sin, .num_cos, .num_tan, .num_asin, .num_acos, .num_atan, .num_floor, .num_ceiling, .f32_to_bits, .f32_from_bits, .f64_to_bits, .f64_from_bits, .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by, .num_bitwise_and, .num_bitwise_or, .num_bitwise_xor, .num_bitwise_not, .num_count_one_bits, .num_count_leading_zero_bits, .num_count_trailing_zero_bits, .num_from_le_bytes_unchecked, .simd_load_16_unchecked, .simd_store_16_unchecked, .simd_append_16, .simd_splat, .simd_get_lane_unchecked, .simd_with_lane_unchecked, .simd_to_u128_bits, .simd_from_u128_bits, .simd_add_wrap, .simd_sub_wrap, .simd_add_sat, .simd_sub_sat, .simd_neg_wrap, .simd_abs_wrap, .simd_min, .simd_max, .simd_abs_diff, .simd_avg_rounded, .simd_mul_wrap, .simd_mul_high, .simd_mul_q15_sat, .simd_mul_wide_lo, .simd_mul_wide_hi, .simd_dot_pairs, .simd_dot_pairs_sat, .simd_sad, .simd_and, .simd_or, .simd_xor, .simd_not, .simd_bit_select, .simd_eq_lanes, .simd_gt_lanes, .simd_gte_lanes, .simd_bitmask, .simd_shl_wrap, .simd_shr_wrap, .simd_shr_zf_wrap, .simd_shr_rounded, .simd_interleave_lo, .simd_interleave_hi, .simd_even_lanes, .simd_odd_lanes, .simd_reverse_lanes, .simd_table_lookup, .simd_concat_shift_bytes, .simd_widen_lo, .simd_widen_hi, .simd_pairwise_add_widen, .simd_narrow_wrap, .simd_narrow_sat, .simd_sum_lanes, .simd_sum_lanes_wrap, .simd_clmul_lo, .simd_clmul_hi, .u8_from_str, .i8_from_str, .u16_from_str, .i16_from_str, .u32_from_str, .i32_from_str, .u64_from_str, .i64_from_str, .u128_from_str, .i128_from_str, .dec_from_str, .dec_to_attos, .dec_from_attos, .f32_from_str, .f64_from_str, .u8_from_str_prefix, .u8_from_utf8_prefix, .i8_from_str_prefix, .i8_from_utf8_prefix, .u16_from_str_prefix, .u16_from_utf8_prefix, .i16_from_str_prefix, .i16_from_utf8_prefix, .u32_from_str_prefix, .u32_from_utf8_prefix, .i32_from_str_prefix, .i32_from_utf8_prefix, .u64_from_str_prefix, .u64_from_utf8_prefix, .i64_from_str_prefix, .i64_from_utf8_prefix, .u128_from_str_prefix, .u128_from_utf8_prefix, .i128_from_str_prefix, .i128_from_utf8_prefix, .dec_from_str_prefix, .dec_from_utf8_prefix, .f32_from_str_prefix, .f32_from_utf8_prefix, .f64_from_str_prefix, .f64_from_utf8_prefix, .u8_to_i8_wrap, .u8_to_i8_try, .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128, .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128, .u8_to_f32, .u8_to_f64, .u8_to_dec, .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128, .i8_to_u8_wrap, .i8_to_u8_try, .i8_to_u16_wrap, .i8_to_u16_try, .i8_to_u32_wrap, .i8_to_u32_try, .i8_to_u64_wrap, .i8_to_u64_try, .i8_to_u128_wrap, .i8_to_u128_try, .i8_to_f32, .i8_to_f64, .i8_to_dec, .u16_to_i8_wrap, .u16_to_i8_try, .u16_to_i16_wrap, .u16_to_i16_try, .u16_to_i32, .u16_to_i64, .u16_to_i128, .u16_to_u8_wrap, .u16_to_u8_try, .u16_to_u32, .u16_to_u64, .u16_to_u128, .u16_to_f32, .u16_to_f64, .u16_to_dec, .i16_to_i8_wrap, .i16_to_i8_try, .i16_to_i32, .i16_to_i64, .i16_to_i128, .i16_to_u8_wrap, .i16_to_u8_try, .i16_to_u16_wrap, .i16_to_u16_try, .i16_to_u32_wrap, .i16_to_u32_try, .i16_to_u64_wrap, .i16_to_u64_try, .i16_to_u128_wrap, .i16_to_u128_try, .i16_to_f32, .i16_to_f64, .i16_to_dec, .u32_to_i8_wrap, .u32_to_i8_try, .u32_to_i16_wrap, .u32_to_i16_try, .u32_to_i32_wrap, .u32_to_i32_try, .u32_to_i64, .u32_to_i128, .u32_to_u8_wrap, .u32_to_u8_try, .u32_to_u16_wrap, .u32_to_u16_try, .u32_to_u64, .u32_to_u128, .u32_to_f32, .u32_to_f64, .u32_to_dec, .i32_to_i8_wrap, .i32_to_i8_try, .i32_to_i16_wrap, .i32_to_i16_try, .i32_to_i64, .i32_to_i128, .i32_to_u8_wrap, .i32_to_u8_try, .i32_to_u16_wrap, .i32_to_u16_try, .i32_to_u32_wrap, .i32_to_u32_try, .i32_to_u64_wrap, .i32_to_u64_try, .i32_to_u128_wrap, .i32_to_u128_try, .i32_to_f32, .i32_to_f64, .i32_to_dec, .u64_to_i8_wrap, .u64_to_i8_try, .u64_to_i16_wrap, .u64_to_i16_try, .u64_to_i32_wrap, .u64_to_i32_try, .u64_to_i64_wrap, .u64_to_i64_try, .u64_to_i128, .u64_to_u8_wrap, .u64_to_u8_try, .u64_to_u16_wrap, .u64_to_u16_try, .u64_to_u32_wrap, .u64_to_u32_try, .u64_to_u128, .u64_to_f32, .u64_to_f64, .u64_to_dec, .i64_to_i8_wrap, .i64_to_i8_try, .i64_to_i16_wrap, .i64_to_i16_try, .i64_to_i32_wrap, .i64_to_i32_try, .i64_to_i128, .i64_to_u8_wrap, .i64_to_u8_try, .i64_to_u16_wrap, .i64_to_u16_try, .i64_to_u32_wrap, .i64_to_u32_try, .i64_to_u64_wrap, .i64_to_u64_try, .i64_to_u128_wrap, .i64_to_u128_try, .i64_to_f32, .i64_to_f64, .i64_to_dec, .u128_to_i8_wrap, .u128_to_i8_try, .u128_to_i16_wrap, .u128_to_i16_try, .u128_to_i32_wrap, .u128_to_i32_try, .u128_to_i64_wrap, .u128_to_i64_try, .u128_to_i128_wrap, .u128_to_i128_try, .u128_to_u8_wrap, .u128_to_u8_try, .u128_to_u16_wrap, .u128_to_u16_try, .u128_to_u32_wrap, .u128_to_u32_try, .u128_to_u64_wrap, .u128_to_u64_try, .u128_to_f32, .u128_to_f64, .u128_to_dec_try_unsafe, .i128_to_i8_wrap, .i128_to_i8_try, .i128_to_i16_wrap, .i128_to_i16_try, .i128_to_i32_wrap, .i128_to_i32_try, .i128_to_i64_wrap, .i128_to_i64_try, .i128_to_u8_wrap, .i128_to_u8_try, .i128_to_u16_wrap, .i128_to_u16_try, .i128_to_u32_wrap, .i128_to_u32_try, .i128_to_u64_wrap, .i128_to_u64_try, .i128_to_u128_wrap, .i128_to_u128_try, .i128_to_f32, .i128_to_f64, .i128_to_dec_try_unsafe, .f32_to_i8_trunc, .f32_to_i8_try_unsafe, .f32_to_i16_trunc, .f32_to_i16_try_unsafe, .f32_to_i32_trunc, .f32_to_i32_try_unsafe, .f32_to_i64_trunc, .f32_to_i64_try_unsafe, .f32_to_i128_trunc, .f32_to_i128_try_unsafe, .f32_to_u8_trunc, .f32_to_u8_try_unsafe, .f32_to_u16_trunc, .f32_to_u16_try_unsafe, .f32_to_u32_trunc, .f32_to_u32_try_unsafe, .f32_to_u64_trunc, .f32_to_u64_try_unsafe, .f32_to_u128_trunc, .f32_to_u128_try_unsafe, .f32_to_f64, .f64_to_i8_trunc, .f64_to_i8_try_unsafe, .f64_to_i16_trunc, .f64_to_i16_try_unsafe, .f64_to_i32_trunc, .f64_to_i32_try_unsafe, .f64_to_i64_trunc, .f64_to_i64_try_unsafe, .f64_to_i128_trunc, .f64_to_i128_try_unsafe, .f64_to_u8_trunc, .f64_to_u8_try_unsafe, .f64_to_u16_trunc, .f64_to_u16_try_unsafe, .f64_to_u32_trunc, .f64_to_u32_try_unsafe, .f64_to_u64_trunc, .f64_to_u64_try_unsafe, .f64_to_u128_trunc, .f64_to_u128_try_unsafe, .f64_to_f32_wrap, .f64_to_f32_try_unsafe, .dec_to_i8_trunc, .dec_to_i8_try_unsafe, .dec_to_i16_trunc, .dec_to_i16_try_unsafe, .dec_to_i32_trunc, .dec_to_i32_try_unsafe, .dec_to_i64_trunc, .dec_to_i64_try_unsafe, .dec_to_i128_trunc, .dec_to_u8_trunc, .dec_to_u8_try_unsafe, .dec_to_u16_trunc, .dec_to_u16_try_unsafe, .dec_to_u32_trunc, .dec_to_u32_try_unsafe, .dec_to_u64_trunc, .dec_to_u64_try_unsafe, .dec_to_u128_trunc, .dec_to_u128_try_unsafe, .dec_to_f32_wrap, .dec_to_f32_try_unsafe, .dec_to_f64, .box_prepare_update, .erased_capture_load, .ptr_alloca, .box_alloc_zeroed, .ptr_store, .ptr_load, .ptr_cast, .compare, .crash => {},
        }
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
            .str_is_eq, .str_is_eq_static_small, .str_static_small_word_eq, .str_static_small_word_caseless_eq, .str_concat, .str_contains, .str_trim, .str_trim_start, .str_trim_end, .str_caseless_ascii_equals, .str_with_ascii_lowercased, .str_with_ascii_uppercased, .str_starts_with, .str_ends_with, .str_repeat, .str_drop_prefix, .str_drop_prefix_caseless_ascii, .str_drop_suffix, .str_split_first, .str_split_last, .str_count_utf8_bytes, .str_get_utf8_byte_unsafe, .str_substring_unsafe, .str_with_capacity, .str_reserve, .str_release_excess_capacity, .str_to_utf8, .str_from_utf8_lossy, .str_from_utf8_validated, .str_from_utf16_le_short, .str_from_utf16_be_short, .str_from_utf32_le_short, .str_from_utf32_be_short, .str_from_utf8, .str_split_on, .str_join_with, .str_inspect, .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str, .dec_to_str, .f32_to_str, .f64_to_str, .list_len, .list_capacity, .list_get_unsafe, .list_prefetched, .list_prefetch, .list_append_unsafe, .list_concat, .list_append_range_within, .list_copy_range_within, .list_append_range_within_unsafe, .list_append_sublist, .list_append_le_bytes, .list_append_le_bytes_unsafe, .list_slack_unique, .list_owned_unique, .list_set_in_place_unsafe, .list_with_capacity, .list_drop_at, .list_sublist, .list_sublist_borrowed, .list_set, .list_replace_unsafe, .list_swap, .list_prepend, .list_take_first, .list_take_last, .list_reverse, .list_reserve, .list_reserve_for_append, .list_release_excess_capacity, .list_clear, .list_map_prepare_reuse, .list_map_can_reuse, .list_map_cast_unsafe, .list_map_extract_unsafe, .list_map_write_unsafe, .list_item_size_at_most, .bool_not, .bool_likely, .dict_pseudo_seed, .hasher_finish, .hasher_write_bool, .hasher_write_u8, .hasher_write_u16, .hasher_write_u32, .hasher_write_u64, .hasher_write_u128, .hasher_write_i8, .hasher_write_i16, .hasher_write_i32, .hasher_write_i64, .hasher_write_i128, .hasher_write_f32, .hasher_write_f64, .hasher_write_dec, .hasher_write_bytes, .hasher_write_str, .crypto_sha256_hash_bytes, .crypto_sha256_hasher_empty, .crypto_sha256_hasher_write, .crypto_sha256_hasher_finish, .crypto_blake3_hash_bytes, .crypto_blake3_hasher_empty, .crypto_blake3_hasher_write, .crypto_blake3_hasher_finish, .num_is_eq, .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte, .num_negate, .num_abs, .num_abs_diff, .num_plus, .num_minus, .num_times, .num_int_add_wrap, .num_int_add_crash_on_overflow, .num_int_add_overflows, .num_int_add_proven_cannot_overflow, .num_int_sub_wrap, .num_int_sub_crash_on_overflow, .num_int_sub_overflows, .num_int_sub_proven_cannot_overflow, .num_int_mul_wrap, .num_int_mul_crash_on_overflow, .num_int_mul_overflows, .num_int_mul_proven_cannot_overflow, .num_float_add, .num_float_sub, .num_float_mul, .dec_mul, .num_div_by, .num_div_by_checked, .num_div_trunc_by, .num_div_trunc_by_checked, .num_rem_by, .num_rem_by_checked, .num_mod_by, .num_mod_by_checked, .num_negate_checked, .num_abs_checked, .num_pow, .num_atan2, .num_sqrt, .num_sin, .num_cos, .num_tan, .num_asin, .num_acos, .num_atan, .num_floor, .num_ceiling, .f32_to_bits, .f32_from_bits, .f64_to_bits, .f64_from_bits, .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by, .num_bitwise_and, .num_bitwise_or, .num_bitwise_xor, .num_bitwise_not, .num_count_one_bits, .num_count_leading_zero_bits, .num_count_trailing_zero_bits, .num_from_le_bytes_unchecked, .simd_load_16_unchecked, .simd_store_16_unchecked, .simd_append_16, .simd_splat, .simd_get_lane_unchecked, .simd_with_lane_unchecked, .simd_to_u128_bits, .simd_from_u128_bits, .simd_add_wrap, .simd_sub_wrap, .simd_add_sat, .simd_sub_sat, .simd_neg_wrap, .simd_abs_wrap, .simd_min, .simd_max, .simd_abs_diff, .simd_avg_rounded, .simd_mul_wrap, .simd_mul_high, .simd_mul_q15_sat, .simd_mul_wide_lo, .simd_mul_wide_hi, .simd_dot_pairs, .simd_dot_pairs_sat, .simd_sad, .simd_and, .simd_or, .simd_xor, .simd_not, .simd_bit_select, .simd_eq_lanes, .simd_gt_lanes, .simd_gte_lanes, .simd_bitmask, .simd_shl_wrap, .simd_shr_wrap, .simd_shr_zf_wrap, .simd_shr_rounded, .simd_interleave_lo, .simd_interleave_hi, .simd_even_lanes, .simd_odd_lanes, .simd_reverse_lanes, .simd_table_lookup, .simd_concat_shift_bytes, .simd_widen_lo, .simd_widen_hi, .simd_pairwise_add_widen, .simd_narrow_wrap, .simd_narrow_sat, .simd_sum_lanes, .simd_sum_lanes_wrap, .simd_clmul_lo, .simd_clmul_hi, .u8_from_str, .i8_from_str, .u16_from_str, .i16_from_str, .u32_from_str, .i32_from_str, .u64_from_str, .i64_from_str, .u128_from_str, .i128_from_str, .dec_from_str, .dec_to_attos, .dec_from_attos, .f32_from_str, .f64_from_str, .u8_from_str_prefix, .u8_from_utf8_prefix, .i8_from_str_prefix, .i8_from_utf8_prefix, .u16_from_str_prefix, .u16_from_utf8_prefix, .i16_from_str_prefix, .i16_from_utf8_prefix, .u32_from_str_prefix, .u32_from_utf8_prefix, .i32_from_str_prefix, .i32_from_utf8_prefix, .u64_from_str_prefix, .u64_from_utf8_prefix, .i64_from_str_prefix, .i64_from_utf8_prefix, .u128_from_str_prefix, .u128_from_utf8_prefix, .i128_from_str_prefix, .i128_from_utf8_prefix, .dec_from_str_prefix, .dec_from_utf8_prefix, .f32_from_str_prefix, .f32_from_utf8_prefix, .f64_from_str_prefix, .f64_from_utf8_prefix, .u8_to_i8_wrap, .u8_to_i8_try, .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128, .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128, .u8_to_f32, .u8_to_f64, .u8_to_dec, .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128, .i8_to_u8_wrap, .i8_to_u8_try, .i8_to_u16_wrap, .i8_to_u16_try, .i8_to_u32_wrap, .i8_to_u32_try, .i8_to_u64_wrap, .i8_to_u64_try, .i8_to_u128_wrap, .i8_to_u128_try, .i8_to_f32, .i8_to_f64, .i8_to_dec, .u16_to_i8_wrap, .u16_to_i8_try, .u16_to_i16_wrap, .u16_to_i16_try, .u16_to_i32, .u16_to_i64, .u16_to_i128, .u16_to_u8_wrap, .u16_to_u8_try, .u16_to_u32, .u16_to_u64, .u16_to_u128, .u16_to_f32, .u16_to_f64, .u16_to_dec, .i16_to_i8_wrap, .i16_to_i8_try, .i16_to_i32, .i16_to_i64, .i16_to_i128, .i16_to_u8_wrap, .i16_to_u8_try, .i16_to_u16_wrap, .i16_to_u16_try, .i16_to_u32_wrap, .i16_to_u32_try, .i16_to_u64_wrap, .i16_to_u64_try, .i16_to_u128_wrap, .i16_to_u128_try, .i16_to_f32, .i16_to_f64, .i16_to_dec, .u32_to_i8_wrap, .u32_to_i8_try, .u32_to_i16_wrap, .u32_to_i16_try, .u32_to_i32_wrap, .u32_to_i32_try, .u32_to_i64, .u32_to_i128, .u32_to_u8_wrap, .u32_to_u8_try, .u32_to_u16_wrap, .u32_to_u16_try, .u32_to_u64, .u32_to_u128, .u32_to_f32, .u32_to_f64, .u32_to_dec, .i32_to_i8_wrap, .i32_to_i8_try, .i32_to_i16_wrap, .i32_to_i16_try, .i32_to_i64, .i32_to_i128, .i32_to_u8_wrap, .i32_to_u8_try, .i32_to_u16_wrap, .i32_to_u16_try, .i32_to_u32_wrap, .i32_to_u32_try, .i32_to_u64_wrap, .i32_to_u64_try, .i32_to_u128_wrap, .i32_to_u128_try, .i32_to_f32, .i32_to_f64, .i32_to_dec, .u64_to_i8_wrap, .u64_to_i8_try, .u64_to_i16_wrap, .u64_to_i16_try, .u64_to_i32_wrap, .u64_to_i32_try, .u64_to_i64_wrap, .u64_to_i64_try, .u64_to_i128, .u64_to_u8_wrap, .u64_to_u8_try, .u64_to_u16_wrap, .u64_to_u16_try, .u64_to_u32_wrap, .u64_to_u32_try, .u64_to_u128, .u64_to_f32, .u64_to_f64, .u64_to_dec, .i64_to_i8_wrap, .i64_to_i8_try, .i64_to_i16_wrap, .i64_to_i16_try, .i64_to_i32_wrap, .i64_to_i32_try, .i64_to_i128, .i64_to_u8_wrap, .i64_to_u8_try, .i64_to_u16_wrap, .i64_to_u16_try, .i64_to_u32_wrap, .i64_to_u32_try, .i64_to_u64_wrap, .i64_to_u64_try, .i64_to_u128_wrap, .i64_to_u128_try, .i64_to_f32, .i64_to_f64, .i64_to_dec, .u128_to_i8_wrap, .u128_to_i8_try, .u128_to_i16_wrap, .u128_to_i16_try, .u128_to_i32_wrap, .u128_to_i32_try, .u128_to_i64_wrap, .u128_to_i64_try, .u128_to_i128_wrap, .u128_to_i128_try, .u128_to_u8_wrap, .u128_to_u8_try, .u128_to_u16_wrap, .u128_to_u16_try, .u128_to_u32_wrap, .u128_to_u32_try, .u128_to_u64_wrap, .u128_to_u64_try, .u128_to_f32, .u128_to_f64, .u128_to_dec_try_unsafe, .i128_to_i8_wrap, .i128_to_i8_try, .i128_to_i16_wrap, .i128_to_i16_try, .i128_to_i32_wrap, .i128_to_i32_try, .i128_to_i64_wrap, .i128_to_i64_try, .i128_to_u8_wrap, .i128_to_u8_try, .i128_to_u16_wrap, .i128_to_u16_try, .i128_to_u32_wrap, .i128_to_u32_try, .i128_to_u64_wrap, .i128_to_u64_try, .i128_to_u128_wrap, .i128_to_u128_try, .i128_to_f32, .i128_to_f64, .i128_to_dec_try_unsafe, .f32_to_i8_trunc, .f32_to_i8_try_unsafe, .f32_to_i16_trunc, .f32_to_i16_try_unsafe, .f32_to_i32_trunc, .f32_to_i32_try_unsafe, .f32_to_i64_trunc, .f32_to_i64_try_unsafe, .f32_to_i128_trunc, .f32_to_i128_try_unsafe, .f32_to_u8_trunc, .f32_to_u8_try_unsafe, .f32_to_u16_trunc, .f32_to_u16_try_unsafe, .f32_to_u32_trunc, .f32_to_u32_try_unsafe, .f32_to_u64_trunc, .f32_to_u64_try_unsafe, .f32_to_u128_trunc, .f32_to_u128_try_unsafe, .f32_to_f64, .f64_to_i8_trunc, .f64_to_i8_try_unsafe, .f64_to_i16_trunc, .f64_to_i16_try_unsafe, .f64_to_i32_trunc, .f64_to_i32_try_unsafe, .f64_to_i64_trunc, .f64_to_i64_try_unsafe, .f64_to_i128_trunc, .f64_to_i128_try_unsafe, .f64_to_u8_trunc, .f64_to_u8_try_unsafe, .f64_to_u16_trunc, .f64_to_u16_try_unsafe, .f64_to_u32_trunc, .f64_to_u32_try_unsafe, .f64_to_u64_trunc, .f64_to_u64_try_unsafe, .f64_to_u128_trunc, .f64_to_u128_try_unsafe, .f64_to_f32_wrap, .f64_to_f32_try_unsafe, .dec_to_i8_trunc, .dec_to_i8_try_unsafe, .dec_to_i16_trunc, .dec_to_i16_try_unsafe, .dec_to_i32_trunc, .dec_to_i32_try_unsafe, .dec_to_i64_trunc, .dec_to_i64_try_unsafe, .dec_to_i128_trunc, .dec_to_u8_trunc, .dec_to_u8_try_unsafe, .dec_to_u16_trunc, .dec_to_u16_try_unsafe, .dec_to_u32_trunc, .dec_to_u32_try_unsafe, .dec_to_u64_trunc, .dec_to_u64_try_unsafe, .dec_to_u128_trunc, .dec_to_u128_try_unsafe, .dec_to_f32_wrap, .dec_to_f32_try_unsafe, .dec_to_f64, .box_prepare_update, .erased_capture_load, .ptr_alloca, .box_alloc_zeroed, .ptr_store, .ptr_load, .ptr_cast, .compare, .crash => {},
        }
        // Every Builtin low-level whose operands or result can mention a type
        // variable is a list or box operation; every other one is declared
        // at closed types (`BuiltinLowLevel`).
        if (!std.mem.startsWith(u8, @tagName(op), "list_")) {
            invariantFmt("low-level {s} over values of open types, which Builtin declares only at closed types", .{@tagName(op)});
        }
        // The element type of the operation's list: its first List operand's,
        // else its List result's.
        const elem = for (arg_types) |arg_ty| {
            if (store.get(arg_ty) == .list) break store.get(arg_ty).list;
        } else switch (store.get(result_ty)) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariantFmt("list low-level {s} has no List operand or result", .{@tagName(op)}),
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

    /// The branch of a match on `cond` that a layout-resolved low-level's
    /// statically known answer selects (as `.lss` folds it): `cond` calls a
    /// Builtin wrapper of `list_map_can_reuse` with local arguments, every
    /// branch tests an integer literal or nothing, and the answer is the
    /// same on every pointer width.
    fn layoutResolvedBranch(self: *Builder, cond: CheckedExprId, branches: []const checked.CheckedMatchBranch) Allocator.Error!?CheckedExprId {
        const call = switch (self.bodies().expr(cond).data) {
            .call => |call| call,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => return null,
        };
        const resolved = switch (self.bodies().expr(call.func).data) {
            .lookup_external => |r| r orelse return null,
            .lookup_local => |lookup| lookup.resolved orelse return null,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => return null,
        };
        const use = switch (self.resolvedRef(resolved).ref) {
            .top_level_proc, .imported_proc => |use| use,
            .local_param, .local_value, .local_mutable_version, .pattern_binder, .local_proc, .selected_hoisted_const, .top_level_const, .imported_const, .hosted_proc, .platform_required_declaration, .platform_required_checked_error, .platform_required_const, .platform_required_proc, .promoted_top_level_proc => return null,
        };
        const binding = self.program.procedureBinding(use.binding);
        const template_ref = switch (binding.body) {
            .direct_template => |direct| Program.directTemplate(direct.template),
            .callable_eval_template, .checked_error => return null,
        };
        const owner = self.program.moduleByKey(.{ .bytes = template_ref.artifact.bytes });
        const op = (owner.view.checked_procedure_templates.templates.items[@backingInt(template_ref.template)].provided_low_level orelse return null).layoutResolved() orelse return null;
        for (call.args) |arg| switch (self.bodies().expr(arg).data) {
            .lookup_local => {},
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => return null,
        };
        for (branches) |branch| {
            if (branch.guard != null) return null;
            for (branch.patternsSlice(self.bodies())) |pattern| switch (self.bodies().pattern(pattern.pattern).data) {
                .underscore => {},
                .numeral_literal => |numeral| if (numeral.guard != null) return null,
                .pending, .assign, .as, .applied_tag, .nominal, .record_destructure, .list, .tuple, .str_literal, .str_interpolation, .runtime_error => return null,
            };
        }
        const arg_types = try self.allocator.alloc(TypeId, call.args.len);
        defer self.allocator.free(arg_types);
        for (call.args, arg_types) |arg, *ty| ty.* = try self.exprType(arg);
        const answer: u8 = switch (op) {
            .list_map_can_reuse => blk: {
                const decision = try self.listMapInterchangeable(arg_types);
                if (decision.open) return null;
                break :blk if (!decision.widths.get(.u32) and !decision.widths.get(.u64)) 0 else return null;
            },
            // Its answer is computed where it runs.
            .list_item_size_at_most => return null,
        };
        for (branches) |branch| {
            for (branch.patternsSlice(self.bodies())) |pattern| switch (self.bodies().pattern(pattern.pattern).data) {
                .underscore => return branch.value,
                .numeral_literal => |numeral| {
                    const bits = checked.exact_numeral.intBits(self.view.view.module_env.exactNumeral(numeral.literal), .u8) orelse continue;
                    const value: i128 = switch (bits) {
                        .i128 => |value| value,
                        .u128 => |value| @bitCast(value),
                    };
                    if (value == answer) return branch.value;
                },
                .pending, .assign, .as, .applied_tag, .nominal, .record_destructure, .list, .tuple, .str_literal, .str_interpolation, .runtime_error => unreachable, // only literal and wildcard branches reach here
            };
        }
        return null;
    }

    /// Per pointer width, whether a `List.map` over a list of `arg_types[0]`
    /// with the transform `arg_types[1]` may reuse its input's allocation:
    /// never when the optimization is disabled or an element type is open
    /// (`open`: no committed layout to compare where this body is lowered),
    /// otherwise when the element layouts are interchangeable on that width.
    fn listMapInterchangeable(self: *Builder, arg_types: []const TypeId) Allocator.Error!struct { widths: layout.WidthValues(bool), output_elem: ?layout.Idx, open: bool = false } {
        const none = layout.WidthValues(bool).both(false, false);
        if (!self.program.list_in_place_map) return .{ .widths = none, .output_elem = null };
        const store = &self.program.store;
        const in_elem = switch (store.get(try self.structuralType(arg_types[0]))) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("list_map_can_reuse's first operand is not a List"),
        };
        const out_elem = switch (store.get(arg_types[1])) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("list_map_can_reuse's transform is not a function"),
        };
        if (!try self.closed(in_elem) or !try self.closed(out_elem)) return .{ .widths = none, .output_elem = null, .open = true };
        const layouts = &self.program.result.layouts;
        const in_idx = layouts.runtimeRepresentationLayoutIdx(try self.program.layouts.layoutOf(in_elem));
        const out_idx = layouts.runtimeRepresentationLayoutIdx(try self.program.layouts.layoutOf(out_elem));
        return .{ .widths = layout.WidthValues(bool).both(
            layouts.listItemsInterchangeableAt(in_idx, out_idx, .u32),
            layouts.listItemsInterchangeableAt(in_idx, out_idx, .u64),
        ), .output_elem = out_idx };
    }

    /// Whether `List.map` may reuse its input's allocation: the input and
    /// output element layouts must be interchangeable in one allocation (the
    /// same rule `.lss` applies), per pointer width, before the runtime
    /// uniqueness check decides. Elements of open types have no committed
    /// layout to compare, so their lists are never reused.
    pub fn listMapCanReuse(self: *Builder, args: []const LocalId, arg_types: []const TypeId, result_ty: TypeId) Allocator.Error!LocalId {
        if (args.len != 2) invariant("list_map_can_reuse takes a list and a transform");
        const target = try self.localOf(result_ty, .frame);
        const decision = try self.listMapInterchangeable(arg_types);
        if (!decision.widths.get(.u32) and !decision.widths.get(.u64)) {
            try self.emitLinear(.{ .assign_literal = .{ .target = target, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u8 } }, .next = undefined } });
            return target;
        }
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = .list_map_can_reuse,
            .rc_effect = try self.store.addRcEffect(LIR.LowLevel.list_map_can_reuse.rcEffect()),
            .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{
                .interchangeable = decision.widths,
                .map_output_elem = decision.output_elem.?,
            }),
            .args = try self.store.addLocalSpan(args),
            .next = undefined,
        } });
        return target;
    }

    /// A function value: an erased callable of the lambda's worker, whose
    /// captures hold the captured values and this worker's descriptors.
    pub fn lowerClosure(self: *Builder, expr_id: CheckedExprId, lambda_expr: CheckedExprId, captures: []const checked.CheckedCapture) Allocator.Error!void {
        // A generalized local procedure's type is converted with its own
        // scheme's variables in scope.
        const own: ?lower.LocalScheme = if (self.program.local_scopes.get(.{ .module = self.module, .expr = lambda_expr })) |scope_id|
            try self.localScheme(scope_id)
        else
            null;
        const fn_ty = if (own) |scheme| blk: {
            if (scheme.scheme_vars.len == 0 and scheme.dictionaries.len == 0) break :blk try self.exprType(expr_id);
            const vars = try std.mem.concat(self.allocator, checked.CheckedTypeId, &.{ self.env.vars, scheme.scheme_vars });
            defer self.allocator.free(vars);
            const bound = try self.allocator.alloc(TypeId, vars.len);
            defer self.allocator.free(bound);
            for (bound, 0..) |*slot, index| slot.* = try self.program.store.variable(@intCast(index));
            break :blk try self.program.converter().convert(self.module, self.bodies().expr(expr_id).ty, .{ .module = self.module, .vars = vars, .bound = bound });
        } else try self.exprType(expr_id);
        const generalized: ?lower.LocalScheme = if (own) |scheme| (if (scheme.scheme_vars.len == 0 and scheme.dictionaries.len == 0) null else scheme) else null;
        const capture_types = try self.allocator.alloc(TypeId, captures.len);
        defer self.allocator.free(capture_types);
        const values = try self.allocator.alloc(LocalId, captures.len + self.var_descs.len + self.dict_locals.len);
        defer self.allocator.free(values);
        for (captures, capture_types, 0..) |capture, *capture_ty, index| {
            capture_ty.* = try self.capturedType(capture.pattern, try self.patternType(capture.pattern));
            values[index] = self.capturedValue(capture.pattern) orelse invariant("a closure captured a binder this body never bound");
        }
        try self.declared_captures.put(self.allocator, lambda_expr, try self.program.arena.allocator().dupe(LocalId, values[0..captures.len]));
        const callee = try self.program.lambdaWorker(self.worker_id, lambda_expr, captures, capture_types, fn_ty, generalized);
        // A generalized lambda that is not a local procedure is instantiated
        // where it is constructed: its function value captures its own
        // scheme's descriptors and dictionaries after the enclosing scope's.
        var descs: []const LocalId = self.var_descs;
        var dicts: []const LocalId = self.dict_locals;
        var own_descs: std.ArrayList(LocalId) = .empty;
        defer own_descs.deinit(self.allocator);
        var own_dicts: std.ArrayList(LocalId) = .empty;
        defer own_dicts.deinit(self.allocator);
        if (generalized) |scheme| if (!scheme.per_call) {
            // A construction the checker recorded no instantiation for (no
            // use relates the value's variables) seals each of them as a
            // variable no use relates.
            const subst = self.siteSubstitution(expr_id);
            if (subst.len != 0 and subst.len != scheme.scheme_vars.len) invariant("a generalized lambda's construction substitution differs from its scheme");
            const evidence = self.view.view.static_dispatch_plans.siteEvidence(expr_id) orelse &.{};
            try own_descs.appendSlice(self.allocator, self.var_descs);
            const own_types = try self.allocator.alloc(TypeId, scheme.scheme_vars.len);
            defer self.allocator.free(own_types);
            for (own_types, scheme.scheme_vars, 0..) |*own_ty, variable, index| {
                own_ty.* = if (subst.len != 0) try self.convert(subst[index]) else try self.program.converter().sealedScheme(self.module, variable);
                try own_descs.append(self.allocator, try self.descOf(own_ty.*));
            }
            const scope_id = self.program.local_scopes.get(.{ .module = self.module, .expr = lambda_expr }).?;
            const context = try self.scopeParamContext(scope_id, scheme.scheme_vars, own_types);
            for (scheme.dictionaries) |param_index| {
                if (param_index >= evidence.len) invariant("a generalized lambda's construction evidence does not cover its dictionaries");
                try own_dicts.append(self.allocator, try self.paramDictionary(evidence, param_index, context));
            }
            try own_dicts.appendSlice(self.allocator, self.dict_locals);
            descs = own_descs.items;
            dicts = own_dicts.items;
        };
        if (try self.allClosed(capture_types)) {
            try self.pushValue(try self.packClosure(callee, values[0..captures.len], capture_types, descs, dicts));
        } else {
            try self.pushValue(try self.packDynamicClosure(callee, values[0..captures.len], capture_types, descs, dicts));
        }
    }

    /// The descriptor of a dynamic capture record (design.md "Function
    /// Values"): a declared-order record of its own descriptor word, the
    /// descriptor and dictionary words, then the captured values.
    pub fn dynamicCaptureDesc(self: *Builder, capture_types: []const TypeId, word_count: usize) Allocator.Error!LocalId {
        const word = try self.opaqueWordDesc();
        const descs = try self.allocator.alloc(LocalId, 1 + word_count + capture_types.len);
        defer self.allocator.free(descs);
        @memset(descs[0 .. 1 + word_count], word);
        for (descs[1 + word_count ..], capture_types) |*desc, ty| desc.* = try self.descOf(ty);
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .dyn = .{ .target = local, .data = try self.store.addStmtRow(LIR.DynOp, .{ .desc_args = .{ .elems = try self.store.addLocalSpan(descs) } }), .next = undefined } });
        return local;
    }

    pub fn opaqueWordDesc(self: *Builder) Allocator.Error!LocalId {
        if (self.opaque_word_desc) |local| return local;
        const local = try self.closedLocal(.opaque_ptr);
        try self.emitEntry(.{ .assign_literal = .{ .target = local, .value = .{ .static_data = .{ .id = try self.program.static_descs.opaqueWordSlot() } }, .next = undefined } });
        self.opaque_word_desc = local;
        return local;
    }

    /// A function value whose captures include values of open types: the
    /// capture record is built in the dynamic frame region and moved into
    /// the callable. `capture_types` are the captured values' types in this
    /// body's scope.
    pub fn packDynamicClosure(self: *Builder, callee: WorkerId, captured: []const LocalId, capture_types: []const TypeId, descs: []const LocalId, dicts: []const LocalId) Allocator.Error!LocalId {
        if (self.immortal_closures) invariant("a method override captures a value of an open type, which methods never capture");
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
    pub fn packClosure(self: *Builder, callee: WorkerId, captured: []const LocalId, capture_types: []const TypeId, descs: []const LocalId, dicts: []const LocalId) Allocator.Error!LocalId {
        const values = try std.mem.concat(self.allocator, LocalId, &.{ captured, descs, dicts });
        defer self.allocator.free(values);
        if (self.immortal_closures) {
            // Only function values (an adapted override's or dictionary's
            // own callable, itself immortal) are captured: methods capture
            // no values.
            for (capture_types) |capture_ty| {
                if (self.program.store.get(capture_ty) != .function) invariant("a method override captures a value, which methods never do");
            }
            return try self.composeDictionary(callee, values);
        }
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
    pub fn finishErasedCall(self: *Builder, fn_ty: TypeId, arg_count: u32, result_ty: TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        const closure = self.popValue();
        try self.pushValue(try self.emitErasedCall(closure, fn_ty, args, result_ty));
    }

    /// Call the erased callable `closure` of type `fn_ty` with `args`.
    pub fn emitErasedCall(self: *Builder, closure: LocalId, fn_ty: TypeId, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const params = switch (self.program.store.get(fn_ty)) {
            .function => |function| self.program.store.span(function.args),
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a called value's type is not a function type"),
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
        // The callee writes its result as the called type's result; a use
        // whose type rows it differently reads it re-tagged (design.md
        // "Row Joins").
        const ret = switch (self.program.store.get(fn_ty)) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => unreachable, // checked above
        };
        if (ret == result_ty) return try self.callErased(closure, record, result_ty);
        return try self.adaptValue(try self.callErased(closure, record, ret), ret, result_ty);
    }

    /// Call the erased callable `closure` with the packed argument record
    /// `record`, for a result of `result_ty`.
    pub fn callErased(self: *Builder, closure: LocalId, record: ?LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const target = try self.localOf(result_ty, .frame);
        try self.emitDyn(target, .{ .call_erased = .{ .closure = closure, .args = record } });
        return target;
    }

    pub fn finishEq(self: *Builder, negated: bool, operand_ty: TypeId) Allocator.Error!void {
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
    pub fn lowerShortCircuit(self: *Builder, is_and: bool, lhs: CheckedExprId, rhs: CheckedExprId) Allocator.Error!void {
        const result = try self.closedLocal(.bool);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        try self.push(.{ .push_value = result });
        try self.push(.{ .resume_at = after });
        try self.push(.{ .short_circuit = .{ .is_and = is_and, .rhs = rhs, .join = join } });
        try self.push(.{ .expr = lhs });
    }

    pub fn shortCircuit(self: *Builder, is_and: bool, rhs: CheckedExprId, join: LIR.JoinPointId) Allocator.Error!void {
        const holes = try self.emitBoolSwitch(self.popValue());
        // The side that decides the result alone jumps with the constant.
        self.hole = if (is_and) holes.otherwise else holes.then;
        try self.emitJump(join, try self.boolValue(!is_and));
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = rhs });
        self.hole = if (is_and) holes.then else holes.otherwise;
    }

    // -- Aggregates --

    pub fn finishRecord(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const record: @FieldType(checked.CheckedExprData, "record") = switch (self.bodies().expr(expr_id).data) {
            .record => |record| record,
            .empty_record => .{ .fields = &.{}, .unsets = &.{}, .ext = null },
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => invariant("a record construction is not a record expression"),
        };
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(record.fields.len));
        defer self.allocator.free(values);
        const update_base: ?LocalId = if (record.ext != null) self.popValue() else null;
        const record_ty = switch (self.program.store.get(try self.structuralType(ty))) {
            .record => |value| value,
            .empty_record => return try self.pushValue(try self.unitValue()),
            .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record expression's type is not a record"),
        };
        const fields = try self.allocator.dupe(types.Field, self.program.store.fieldSpan(record_ty.fields));
        defer self.allocator.free(fields);
        // Values arrive in source order; store them by checked field index. An
        // optional field's slot holds `#Present(value)`, or `#Missing` when
        // the literal omits it (design.md "Field Kinds").
        const ordered = try self.allocator.alloc(?LocalId, fields.len);
        defer self.allocator.free(ordered);
        @memset(ordered, null);
        const name_store = self.view.nameStore();
        for (record.fields, values) |field, value| {
            const name = try self.program.store.internName(name_store.recordFieldLabelText(field.label));
            const position = for (fields, 0..) |candidate, index| {
                if (candidate.name == name) break index;
            } else invariant("a record expression's field is absent from its type");
            ordered[position] = switch (fields[position].kind) {
                .inline_slot => value,
                .presence_slot => try self.buildTag(try self.program.layouts.presenceSlot(fields[position].ty), try self.program.store.internName(@import("layouts.zig").present_label), &.{value}),
            };
        }
        // Unset fields (`name: _`) are missing.
        for (record.unsets) |label| {
            const name = try self.program.store.internName(name_store.recordFieldLabelText(label));
            for (fields, ordered) |field, *slot| {
                if (field.name != name) continue;
                if (field.kind != .presence_slot) invariant("an unset field is not optional");
                slot.* = try self.buildTag(try self.program.layouts.presenceSlot(field.ty), try self.program.store.internName(@import("layouts.zig").missing_label), &.{});
            }
        }
        // An update copies the fields it does not mention from its base.
        if (update_base) |base_value| {
            // A record of an open row takes the fields its update names and
            // the rest of its base's, which only its descriptor lists.
            if (record_ty.ext != null) {
                var names: std.ArrayList(LocalId) = .empty;
                defer names.deinit(self.allocator);
                var updated: std.ArrayList(LocalId) = .empty;
                defer updated.deinit(self.allocator);
                for (fields, ordered) |field, value| {
                    const local = value orelse continue;
                    try names.append(self.allocator, try self.nameDesc(field.name));
                    try updated.append(self.allocator, local);
                }
                const result = try self.localOf(ty, .frame);
                try self.emitDyn(result, .{ .record_update = .{
                    .base = base_value,
                    .base_desc = try self.descOf(try self.exprType(record.ext.?)),
                    .desc = try self.descOf(ty),
                    .names = try self.store.addLocalSpan(names.items),
                    .values = try self.store.addLocalSpan(updated.items),
                } });
                return try self.pushValue(result);
            }
            const base_ty = try self.structuralType(try self.exprType(record.ext.?));
            const base_record = switch (self.program.store.get(base_ty)) {
                .record => |value| value,
                .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record update's base is not a record"),
            };
            const base_fields = try self.allocator.dupe(types.Field, self.program.store.fieldSpan(base_record.fields));
            defer self.allocator.free(base_fields);
            for (fields, ordered) |field, *slot| {
                if (slot.* != null) continue;
                const storage = try self.program.layouts.fieldStorage(field);
                const base_index = for (base_fields, 0..) |candidate, index| {
                    if (candidate.name == field.name) break index;
                } else null;
                if (base_index) |index| {
                    slot.* = try self.readField(base_ty, base_value, @intCast(index), storage);
                } else {
                    if (base_record.ext == null) invariant("a record update's field is absent from its base");
                    const local = try self.localOf(storage, .view);
                    try self.emitDyn(local, .{ .field_addr_named = .{ .base = base_value, .desc = try self.descOf(base_ty), .name = try self.nameDesc(field.name) } });
                    slot.* = local;
                }
            }
        }
        const slots = try self.allocator.alloc(LocalId, fields.len);
        defer self.allocator.free(slots);
        for (fields, ordered, slots) |field, value, *slot| {
            slot.* = value orelse switch (field.kind) {
                .presence_slot => try self.buildTag(try self.program.layouts.presenceSlot(field.ty), try self.program.store.internName(@import("layouts.zig").missing_label), &.{}),
                .inline_slot => try self.defaultValue(self.program.omitted_defaults.get(.{ .module = self.module, .expr = expr_id, .name = field.name }) orelse
                    invariant("a record construction omits a field it has no default for")),
            };
        }
        try self.pushValue(try self.buildStruct(ty, slots));
    }

    /// A record or tuple of `ty` from its fields in checked field order.
    pub fn buildStruct(self: *Builder, ty: TypeId, fields: []const LocalId) Allocator.Error!LocalId {
        const target = try self.localOf(ty, .frame);
        if (try self.closed(ty)) {
            const struct_layout = self.store.getLocal(target).layout_idx;
            const component_types = try self.structComponentTypes(ty);
            defer self.allocator.free(component_types);
            const slots = try self.allocator.alloc(LocalId, fields.len);
            defer self.allocator.free(slots);
            for (fields, slots, 0..) |field, *slot, index| {
                slot.* = if (component_types.len == fields.len and fields.len != 0)
                    try self.intoSlot(field, component_types[index], self.structFieldLayout(struct_layout, @intCast(index)))
                else
                    field;
            }
            try self.emitLinear(.{ .assign_struct = .{ .target = target, .fields = try self.store.addLocalSpan(slots), .next = undefined } });
        } else {
            try self.emitDyn(target, .{ .record = .{ .desc = try self.descOf(ty), .fields = try self.store.addLocalSpan(fields) } });
        }
        return target;
    }

    pub fn finishTuple(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const items = self.bodies().expr(expr_id).data.tuple;
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(items.len));
        defer self.allocator.free(values);
        try self.pushValue(try self.buildStruct(ty, values));
    }

    pub fn finishTag(self: *Builder, expr_id: CheckedExprId, at_ty: ?TypeId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const label, const arg_count: usize = switch (expr.data) {
            .tag => |tag| .{ tag.name, tag.args.len },
            .zero_argument_tag => |tag| .{ tag.name, 0 },
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .nominal, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => unreachable, // only tag expressions finish as tags
        };
        const ty = at_ty orelse try self.exprType(expr_id);
        const values = try self.popValues(@intCast(arg_count));
        defer self.allocator.free(values);
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(label));
        try self.pushValue(try self.buildTag(ty, name, values));
    }

    /// The tag `name` of the tag union `ty` (through nominal wrappers) with
    /// `values` as its payloads.
    pub fn buildTag(self: *Builder, ty: TypeId, name: types.NameId, values: []const LocalId) Allocator.Error!LocalId {
        if (!try self.closed(ty)) {
            const target = try self.localOf(ty, .frame);
            try self.emitDyn(target, .{ .tag = .{ .desc = try self.descOf(try self.structuralType(ty)), .name = try self.nameDesc(name), .payloads = try self.store.addLocalSpan(values) } });
            return target;
        }
        switch (self.program.store.get(try self.structuralType(ty))) {
            .tag_union => {},
            .primitive => |primitive| {
                if (primitive != .bool) invariant("a tag expression's type is a non-Bool primitive");
                return try self.boolValue(std.mem.eql(u8, self.program.store.nameText(name), "True"));
            },
            .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag expression's type is not a tag union"),
        }
        const selected = try self.selectVariant(try self.structuralType(ty), self.ownTag(try self.structuralType(ty), name, values.len));
        const variant = selected.index;
        const target = try self.localOf(ty, .frame);
        const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
        const payload: ?LocalId = if (values.len == 0 or committed.tag != .tag_union) null else blk: {
            const payload_layout = try self.variantPayloadLayout(try self.structuralType(ty), variant);
            if (values.len == 1) break :blk try self.intoSlot(values[0], selected.payloads[0], payload_layout);
            // A payload of several values is a struct at the variant's
            // committed payload layout.
            const slots = try self.allocator.alloc(LocalId, values.len);
            defer self.allocator.free(slots);
            for (values, selected.payloads, slots, 0..) |value, payload_ty, *slot, index| {
                slot.* = try self.intoSlot(value, payload_ty, self.structFieldLayout(payload_layout, @intCast(index)));
            }
            const payload_struct = try self.closedLocal(payload_layout);
            try self.emitLinear(.{ .assign_struct = .{ .target = payload_struct, .fields = try self.store.addLocalSpan(slots), .next = undefined } });
            break :blk payload_struct;
        };
        if (committed.tag != .tag_union) {
            // A single-variant union of zero-sized payloads has no tag storage.
            try self.emitLinear(.{ .assign_struct = .{ .target = target, .fields = LIR.LocalSpan.empty(), .next = undefined } });
        } else {
            try self.emitLinear(.{ .assign_tag = .{ .target = target, .variant_index = @intCast(variant), .discriminant = @intCast(variant), .payload = payload, .next = undefined } });
        }
        return target;
    }

    pub fn finishList(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const count: usize = switch (expr.data) {
            .list => |items| items.len,
            .empty_list => 0,
            .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => unreachable, // only list expressions finish as lists
        };
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(count));
        defer self.allocator.free(values);
        try self.pushValue(try self.buildList(ty, values));
    }

    /// A list of `ty` holding `values`.
    pub fn buildList(self: *Builder, ty: TypeId, values: []const LocalId) Allocator.Error!LocalId {
        const target = try self.localOf(ty, .frame);
        if (try self.closed(ty)) {
            try self.emitLinear(.{ .assign_list = .{ .target = target, .elems = try self.store.addLocalSpan(values), .next = undefined } });
        } else {
            const elem = switch (self.program.store.get(try self.structuralType(ty))) {
                .list => |elem| elem,
                .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a list's type is not a List"),
            };
            try self.emitDyn(target, .{ .list = .{ .elem_desc = try self.descOf(elem), .elems = try self.store.addLocalSpan(values) } });
        }
        return target;
    }

    // -- Control flow --

    pub fn ifTest(self: *Builder, expr_id: CheckedExprId, index: u32, join: ?LIR.JoinPointId, merge: ?MergeId) Allocator.Error!void {
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
            try self.pushBranchEnd(join);
            try self.push(.{ .expr = if_.final_else });
        }
        if (merge) |m| try self.push(.{ .restore_merge = m });
        try self.push(.{ .resume_at = holes.otherwise });
        try self.pushBranchEnd(join);
        try self.push(.{ .expr = if_.branches[index].body });
        if (merge) |m| try self.restoreMerge(m);
        self.hole = holes.then;
    }

    /// Where a branch of a construct ends: a jump to its join, or, in tail
    /// position, a return.
    pub fn pushBranchEnd(self: *Builder, join: ?LIR.JoinPointId) Allocator.Error!void {
        try self.push(if (join) |id| .{ .jump = id } else .ret);
    }

    pub fn matchStart(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const scrutinee = self.popValue();
        const match = self.bodies().expr(expr_id).data.match_;
        const scrutinee_ty = try self.exprType(match.cond);
        const tail = self.tail_exprs.contains(expr_id);
        const state = try self.program.arena.allocator().create(Match.State);
        state.* = .{
            .arena = std.heap.ArenaAllocator.init(self.allocator),
            .done = self.freshJoin(),
            .tail = tail,
            .merge = null,
            .remainder = undefined,
        };
        if (tail) {
            // Each branch returns; nothing reads the construct's own value.
            for (match.branches) |branch| try self.tail_exprs.put(self.allocator, branch.value, {});
            try self.push(.unreachable_or_unit);
        } else {
            const result = try self.localOf(try self.exprType(expr_id), .frame);
            state.merge = try self.beginMerge(if (match.state_merge) |plan| self.bodies().stateMergeBinders(plan) else &.{});
            const after = try self.declareMergeJoin(state.done, result, state.merge);
            try self.push(.{ .push_value = result });
            if (state.merge) |m| try self.push(.{ .enter_merge = m });
            try self.push(.{ .resume_at = after });
        }
        state.remainder = try self.currentHole();
        self.hole = null;

        // Every pattern of a branch is a row of the decision tree; rows of
        // one branch share its one lowered body.
        const arena = state.arena.allocator();
        const ctx: Match.Ctx = .{ .b = self, .state = state };
        var rows: std.ArrayList(Match.Compiler.Branch) = .empty;
        for (match.branches, 0..) |branch, index| {
            const patterns = branch.patternsSlice(self.bodies());
            try state.alternatives.put(arena, branch.value, @intCast(patterns.len));
            for (patterns) |alternative| {
                for (alternative.binderRemapsSlice(self.bodies())) |remap| {
                    try state.representatives.put(arena, remap.candidate_binder, remap.representative_binder);
                }
            }
            for (patterns) |alternative| try rows.append(arena, .{
                .pat = alternative.pattern,
                .guard = branch.guard,
                .body = branch.value,
                .branch_index = @intCast(index),
            });
        }
        try ctx.noteScrutinee(scrutinee, scrutinee_ty);
        const built = try Match.Compiler.build(arena, ctx, scrutinee_ty, rows.items);
        state.emitter = try Match.Compiler.Emitter.begin(arena, ctx, built, scrutinee, state.done);
        try self.push(.{ .match_advance = .{ .state = state, .input = null } });
    }

    /// Continue a match's decision-tree emission: lower the body, guard, or
    /// literal guard it asks for into a fresh hole, or link the finished
    /// tree into the match's place.
    pub fn matchAdvance(self: *Builder, state: *Match.State, input: ?CFStmtId) Allocator.Error!void {
        switch (try state.emitter.advance(input)) {
            .request => |request| switch (request) {
                .body => |requested| {
                    const shared = (state.alternatives.get(requested.body) orelse 1) > 1;
                    if (shared) {
                        if (state.shared_bodies.get(requested.body)) |join| {
                            const jump = try self.store.addCFStmt(.{ .jump = .{ .target = join } }, self.origin(.lowering_glue));
                            return try self.push(.{ .match_advance = .{ .state = state, .input = jump } });
                        }
                    }
                    const body_hole = try self.newHole();
                    var answer = body_hole;
                    if (shared) {
                        // Several patterns' rows share the one lowering:
                        // each jumps to it, its binders bound on every path.
                        const join = self.freshJoin();
                        try state.shared_bodies.put(state.arena.allocator(), requested.body, join);
                        try state.shared_joins.append(state.arena.allocator(), .{ .id = join, .body = body_hole });
                        answer = try self.store.addCFStmt(.{ .jump = .{ .target = join } }, self.origin(.lowering_glue));
                    }
                    try self.push(.{ .match_advance = .{ .state = state, .input = answer } });
                    try self.pushBranchEnd(if (state.tail) null else state.done);
                    try self.push(.{ .expr = requested.body });
                    if (state.merge) |m| try self.push(.{ .restore_merge = m });
                    try self.push(.{ .resume_at = body_hole });
                },
                .guard => |guard| {
                    const guard_hole = try self.newHole();
                    try self.push(.{ .match_advance = .{ .state = state, .input = guard_hole } });
                    try self.push(.{ .match_guard_done = .{ .cond = guard.cond, .next = guard.next } });
                    try self.push(.{ .expr = guard.guard });
                    if (state.merge) |m| try self.push(.{ .restore_merge = m });
                    try self.push(.{ .resume_at = guard_hole });
                },
                .literal_guard => |guard| {
                    // The literal's checked equality reads the matched value
                    // through the pattern's binder.
                    const binder = self.bodies().pattern_binder_by_pattern[@backingInt(guard.pat)].get() orelse
                        invariant("a converted literal pattern has no binder for its guard");
                    try self.binders.put(self.allocator, binder, guard.value);
                    const expr = switch (self.bodies().pattern(guard.pat).data) {
                        .numeral_literal => |numeral| numeral.guard.?,
                        .str_literal => |literal| literal.guard.?,
                        .pending, .assign, .as, .applied_tag, .nominal, .record_destructure, .list, .tuple, .str_interpolation, .underscore, .runtime_error => invariant("a literal guard was asked of a non-literal pattern"),
                    };
                    const guard_hole = try self.newHole();
                    try self.push(.{ .match_advance = .{ .state = state, .input = guard_hole } });
                    try self.push(.{ .match_guard_done = .{ .cond = guard.cond, .next = guard.next } });
                    try self.push(.{ .expr = expr });
                    try self.push(.{ .resume_at = guard_hole });
                },
                // Checked branches bind nothing beyond their patterns.
                .bindings => |bindings| try self.push(.{ .match_advance = .{ .state = state, .input = bindings.next } }),
            },
            .done => |tree| {
                var head = tree;
                for (state.shared_joins.items) |shared| head = try self.addJoinStmt(shared.id, &.{}, shared.body, head);
                try self.fill(state.remainder, self.store.getCFStmt(head), .lowering_glue);
                state.arena.deinit();
            },
        }
    }

    /// A join statement of this body, registered among its join points.
    pub fn addJoinStmt(self: *Builder, id: LIR.JoinPointId, params: []const LocalId, body: CFStmtId, remainder: CFStmtId) Allocator.Error!CFStmtId {
        const span = try self.store.addLocalSpan(params);
        try self.join_points.append(self.allocator, .{ .id = id, .params = span, .body = body });
        return try self.store.addCFStmt(.{ .join = .{ .id = id, .params = span, .body = body, .remainder = remainder } }, self.origin(.lowering_glue));
    }

    /// Start emitting statements front to back for a caller that gives
    /// their successor afterward (`endBackward`).
    pub fn beginBackward(self: *Builder) Allocator.Error!Backward {
        const saved: Backward = .{ .start = try self.newHole(), .hole = self.hole, .last_stmt = self.last_stmt, .branches = self.branch_count };
        self.hole = saved.start;
        self.last_stmt = null;
        return saved;
    }

    /// End statements begun by `beginBackward` at `next`: the first of them,
    /// or `next` itself when none was emitted. A straight-line run links its
    /// last statement to `next`; one that branched reaches `next` through a
    /// join.
    pub fn endBackward(self: *Builder, saved: Backward, next: CFStmtId) Allocator.Error!CFStmtId {
        defer {
            self.hole = saved.hole;
            self.last_stmt = saved.last_stmt;
        }
        const end_hole = self.hole orelse return saved.start;
        if (end_hole == saved.start) return next;
        if (self.branch_count == saved.branches) {
            const last = self.last_stmt orelse invariant("a straight-line statement run recorded no last statement");
            var stmt = self.store.getCFStmt(last);
            setNext(&stmt, next);
            try self.store.replaceCFStmt(last, stmt, self.store.stmtOrigin(last));
            return saved.start;
        }
        const join = self.freshJoin();
        try self.fill(end_hole, .{ .jump = .{ .target = join } }, .lowering_glue);
        return try self.addJoinStmt(join, &.{}, next, saved.start);
    }

    /// End statements begun by `beginBackward` that ended in a terminal:
    /// the first of them.
    pub fn endBackwardTerminal(self: *Builder, saved: Backward) CFStmtId {
        if (self.hole != null) invariant("a statement run expected to end in a terminal falls through");
        self.hole = saved.hole;
        self.last_stmt = saved.last_stmt;
        return saved.start;
    }

    pub const Backward = struct { start: CFStmtId, hole: ?CFStmtId, last_stmt: ?CFStmtId, branches: u32 };

    /// Emit tests that `value` matches `pattern`, branching to `miss` when it
    /// does not, and bind the pattern's binders: a destructuring pattern
    /// outside a match (a parameter, a declaration, a loop's pattern).
    /// Continues in the matched path.
    pub fn testPattern(self: *Builder, root_pattern: CheckedPatternId, root_value: LocalId, root_ty: TypeId, miss: CFStmtId) Allocator.Error!void {
        var pending: std.ArrayList(PatternItem) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .pattern = root_pattern, .value = root_value, .ty = root_ty });
        while (pending.pop()) |item| {
            const pattern = self.bodies().pattern(item.pattern);
            switch (pattern.data) {
                .assign => |binder| try self.binders.put(self.allocator, binder, item.value),
                .underscore => {},
                .as => |as| {
                    try self.binders.put(self.allocator, as.binder, item.value);
                    try pending.append(self.allocator, .{ .pattern = as.pattern, .value = item.value, .ty = item.ty });
                },
                .nominal => |nominal| try pending.append(self.allocator, .{ .pattern = nominal.backing_pattern, .value = item.value, .ty = try self.structuralType(item.ty) }),
                .applied_tag => |tag| {
                    const ty = item.ty;
                    const name = try self.program.store.internName(self.view.nameStore().tagLabelText(tag.name));
                    if (!try self.closed(ty)) {
                        try self.testDynamicTag(ty, name, tag.args, item.value, miss, &pending);
                        continue;
                    }
                    switch (self.program.store.get(try self.structuralType(ty))) {
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
                            const structural = try self.structuralType(ty);
                            const variant = (try self.selectVariant(structural, self.ownTag(structural, name, tag.args.len))).index;
                            const committed = self.program.result.layouts.getLayout(try self.program.layouts.layoutOf(ty));
                            if (committed.tag == .tag_union and tags.len > 1) {
                                // A switch on the discriminant, which proves
                                // the variant its arm reads.
                                const discriminant = try self.closedLocal(.u32);
                                try self.emitLinear(.{ .assign_ref = .{ .target = discriminant, .op = LIR.RefOp.discriminant(item.value), .next = undefined } });
                                const then = try self.newHole();
                                const branches = try self.store.addCFSwitchBranches(&.{.{ .value = @intCast(variant), .body = then }});
                                try self.emitTerminal(.{ .switch_stmt = .{ .cond = discriminant, .branches = branches, .default_branch = miss } });
                                self.hole = then;
                            }
                            const payloads = self.program.store.span(tags[variant].payloads);
                            for (tag.args, 0..) |arg_pattern, arg_index| {
                                if (committed.tag != .tag_union) {
                                    // A union without storage has only
                                    // zero-sized payloads.
                                    const payload_local = try self.localOf(payloads[arg_index], .frame);
                                    if (self.store.getLocal(payload_local).layout_idx != .zst) invariant("a storage-free union has a payload with storage");
                                    try self.emitLinear(.{ .assign_struct = .{ .target = payload_local, .fields = LIR.LocalSpan.empty(), .next = undefined } });
                                    try pending.append(self.allocator, .{ .pattern = arg_pattern, .value = payload_local, .ty = payloads[arg_index] });
                                    continue;
                                }
                                const payload_local = try self.readTagPayload(item.value, structural, variant, payloads, arg_index);
                                try pending.append(self.allocator, .{ .pattern = arg_pattern, .value = payload_local, .ty = payloads[arg_index] });
                            }
                        },
                        .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag pattern's type is not a tag union"),
                    }
                },
                .tuple => |items| {
                    const ty = try self.structuralType(item.ty);
                    const elems = switch (self.program.store.get(ty)) {
                        .tuple => |span| self.program.store.span(span),
                        .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a tuple pattern's type is not a tuple"),
                    };
                    for (items, 0..) |sub, index| {
                        const local = try self.readField(ty, item.value, @intCast(index), elems[index]);
                        try pending.append(self.allocator, .{ .pattern = sub, .value = local, .ty = elems[index] });
                    }
                },
                .record_destructure => |destructs| {
                    const ty = try self.structuralType(item.ty);
                    const record = switch (self.program.store.get(ty)) {
                        .record => |record| record,
                        .empty_record => if (destructs.len == 0) continue else invariant("a record pattern names a field of the empty record"),
                        .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record pattern's type is not a record"),
                    };
                    const fields = self.program.store.fieldSpan(record.fields);
                    for (destructs) |destruct| {
                        const name = try self.program.store.internName(self.view.nameStore().recordFieldLabelText(destruct.label));
                        const sub = switch (destruct.kind) {
                            .required, .sub_pattern => |p| p,
                            .rest => |rest| {
                                // `..` with nothing to bind reads nothing.
                                if (self.bodies().pattern(rest).data == .underscore) continue;
                                // The rest is the fields the pattern does
                                // not name, read by name.
                                const rest_ty = try self.patternType(rest);
                                const rest_value = try self.localOf(rest_ty, .frame);
                                try self.emitDyn(rest_value, .{ .adapt = .{ .src = item.value, .from = try self.descOf(item.ty), .to = try self.descOf(rest_ty) } });
                                try pending.append(self.allocator, .{ .pattern = rest, .value = rest_value, .ty = rest_ty });
                                continue;
                            },
                        };
                        const position = for (fields, 0..) |field, index| {
                            if (field.name == name) break index;
                        } else null;
                        if (position) |index| {
                            if (fields[index].kind == .presence_slot) {
                                // An optional field destructures to its
                                // presence as `Try(value, [MissingField])`.
                                const slot_ty = try self.program.layouts.presenceSlot(fields[index].ty);
                                const slot = try self.readField(ty, item.value, @intCast(index), slot_ty);
                                const try_ty = try self.patternType(sub);
                                const presence = try self.presenceTry(slot, slot_ty, try_ty);
                                try pending.append(self.allocator, .{ .pattern = sub, .value = presence, .ty = try_ty });
                                continue;
                            }
                            const local = try self.readField(ty, item.value, @intCast(index), fields[index].ty);
                            try pending.append(self.allocator, .{ .pattern = sub, .value = local, .ty = fields[index].ty });
                        } else {
                            // A field of an open row's remainder: only the
                            // record's descriptor knows where it is.
                            if (record.ext == null) invariant("a record pattern's field is absent from its type");
                            const field_ty = try self.patternType(sub);
                            const local = try self.localOf(field_ty, .view);
                            try self.emitDyn(local, .{ .field_addr_named = .{ .base = item.value, .desc = try self.descOf(ty), .name = try self.nameDesc(name) } });
                            try pending.append(self.allocator, .{ .pattern = sub, .value = local, .ty = field_ty });
                        }
                    }
                },
                .list => |list| try self.testListPattern(item, list.patterns, list.rest, miss, &pending),
                .numeral_literal => |numeral| {
                    if (numeral.guard) |guard| {
                        try self.literalPatternGuard(item, guard, miss);
                    } else {
                        const expected = try self.builtinNumeral(numeral.literal, item.ty);
                        const matches = try self.closedLocal(.bool);
                        try self.emitLowLevel(matches, .num_is_eq, &.{ item.value, expected });
                        try self.branchOrMiss(matches, miss);
                    }
                },
                .str_literal => |literal| {
                    if (literal.guard) |guard| {
                        try self.literalPatternGuard(item, guard, miss);
                    } else {
                        const expected = try self.strLiteral(self.bodies().stringLiteral(literal.literal));
                        const matches = try self.closedLocal(.bool);
                        try self.emitLowLevel(matches, .str_is_eq, &.{ item.value, expected });
                        try self.branchOrMiss(matches, miss);
                    }
                },
                .str_interpolation => |str| {
                    // The match binds each captured span as a `Str` view of
                    // the matched string.
                    const str_ty = try self.program.store.primitive(.str);
                    const steps = try self.allocator.alloc(LIR.StrMatchStep, str.steps.len);
                    defer self.allocator.free(steps);
                    for (steps, str.steps) |*out, str_step| {
                        const delimiter = self.bodies().stringLiteral(str_step.delimiter);
                        const capture: LIR.StrMatchCapture = if (str_step.capture) |capture_pattern| blk: {
                            const local = try self.closedLocal(.str);
                            try pending.append(self.allocator, .{ .pattern = capture_pattern, .value = local, .ty = str_ty });
                            break :blk .{ .view = local };
                        } else .discard;
                        out.* = .{ .capture = capture, .delimiter = .{ .backing = try self.store.insertString(delimiter), .offset = 0, .len = @intCast(delimiter.len) } };
                    }
                    const prefix = self.bodies().stringLiteral(str.prefix);
                    const then = try self.newHole();
                    try self.emitTerminal(.{ .str_match = .{
                        .source = item.value,
                        .data = try self.store.addStmtRow(LIR.StrMatchData, .{
                            .prefix = .{ .backing = try self.store.insertString(prefix), .offset = 0, .len = @intCast(prefix.len) },
                            .steps = try self.store.addStrMatchSteps(steps),
                            .end = switch (str.end) {
                                .exact => .exact,
                                .tail => .tail,
                            },
                        }),
                        .on_match = then,
                        .on_miss = miss,
                    } });
                    self.hole = then;
                },
                .runtime_error, .pending => invariant("an erroneous pattern reached Boxy pattern lowering, which code holding one never reaches"),
            }
        }
    }

    /// A list pattern: the length test (exact, or at least the items' count
    /// with a rest), then each item and the rest's sublist.
    pub fn testListPattern(self: *Builder, item: PatternItem, items: []const CheckedPatternId, rest: ?checked.CheckedListRestPattern, miss: CFStmtId, pending: *std.ArrayList(PatternItem)) Allocator.Error!void {
        const store = &self.program.store;
        const list_ty = try self.structuralType(item.ty);
        const elem_ty = switch (store.get(list_ty)) {
            .list => |elem| elem,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => invariant("a list pattern's value is not a List"),
        };
        const u64_ty = try store.primitive(.u64);
        const len = try self.closedLocal(.u64);
        try self.emitListLowLevel(len, .list_len, &.{item.value}, elem_ty, list_ty);
        const count = try self.intValue(items.len, u64_ty);
        const fits = try self.closedLocal(.bool);
        try self.emitLowLevel(fits, if (rest == null) .num_is_eq else .num_is_gte, &.{ len, count });
        try self.branchOrMiss(fits, miss);
        for (items, 0..) |sub, index| {
            const at = if (rest != null and index >= rest.?.index) blk: {
                // An item after the rest counts from the end.
                const from_end = try self.intValue(items.len - index, u64_ty);
                const position = try self.closedLocal(.u64);
                try self.emitLowLevel(position, .num_int_sub_wrap, &.{ len, from_end });
                break :blk position;
            } else try self.intValue(index, u64_ty);
            const elem = try self.localOf(elem_ty, .frame);
            try self.emitListLowLevel(elem, .list_get_unsafe, &.{ item.value, at }, elem_ty, list_ty);
            try pending.append(self.allocator, .{ .pattern = sub, .value = elem, .ty = elem_ty });
        }
        if (rest) |rest_info| if (rest_info.pattern) |rest_pattern| {
            if (self.bodies().pattern(rest_pattern).data == .underscore) return;
            const rest_len = try self.closedLocal(.u64);
            try self.emitLowLevel(rest_len, .num_int_sub_wrap, &.{ len, count });
            // `{ len, start }` in field-name order.
            const range_ty = try store.intern(.{ .record = .{ .fields = &.{
                .{ .name = try store.internName("len"), .ty = u64_ty, .kind = .inline_slot },
                .{ .name = try store.internName("start"), .ty = u64_ty, .kind = .inline_slot },
            }, .ext = null } });
            const range = try self.buildStruct(range_ty, &.{ rest_len, try self.intValue(rest_info.index, u64_ty) });
            const sublist = try self.localOf(list_ty, .frame);
            try self.emitListLowLevel(sublist, .list_sublist, &.{ item.value, range }, elem_ty, list_ty);
            try pending.append(self.allocator, .{ .pattern = rest_pattern, .value = sublist, .ty = item.ty });
        };
    }

    /// A list low-level over a list of `elem_ty`: an ordinary low-level for
    /// closed items, the descriptor-carrying form otherwise.
    pub fn emitListLowLevel(self: *Builder, target: LocalId, op: LIR.LowLevel, args: []const LocalId, elem_ty: TypeId, list_ty: TypeId) Allocator.Error!void {
        if (try self.closed(list_ty)) return try self.emitLowLevel(target, op, args);
        const with_desc = try std.mem.concat(self.allocator, LocalId, &.{ args, &.{try self.descOf(elem_ty)} });
        defer self.allocator.free(with_desc);
        try self.emitLinear(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .extra = try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }),
            .args = try self.store.addLocalSpan(with_desc),
            .next = undefined,
        } });
    }

    /// A literal pattern at a type whose literals convert through a method:
    /// the pattern's binder holds the tested value, and the checked equality
    /// against the converted literal decides the match.
    pub fn literalPatternGuard(self: *Builder, item: PatternItem, guard: CheckedExprId, miss: CFStmtId) Allocator.Error!void {
        const binder = self.bodies().pattern_binder_by_pattern[@backingInt(item.pattern)].get() orelse
            invariant("a converted literal pattern has no binder for its guard");
        try self.binders.put(self.allocator, binder, item.value);
        const matches = try self.lowerExprNow(guard);
        try self.branchOrMiss(matches, miss);
    }

    /// The `Try(value, [MissingField])` of type `try_ty` a presence slot
    /// holds: `Ok` of its payload, or `Err(MissingField)`.
    pub fn presenceTry(self: *Builder, slot: LocalId, slot_ty: TypeId, try_ty: TypeId) Allocator.Error!LocalId {
        const store = &self.program.store;
        const present = try store.internName(@import("layouts.zig").present_label);
        const try_union = try self.structuralType(try_ty);
        const result = try self.localOf(try_ty, .frame);
        const join = self.freshJoin();
        const after = try self.declareMergeJoin(join, result, null);
        const missing_hole = try self.newHole();
        try self.branchOnTag(slot, slot_ty, present, missing_hole);
        const payload = try self.tagPayloadValue(slot, slot_ty, present, 0);
        try self.emitJump(join, try self.buildTag(try_union, try store.internName("Ok"), &.{payload.value}));
        self.hole = missing_hole;
        const err_payload_ty = (try self.closedVariant(try_union, try store.internName("Err"))).payloads[0];
        const missing = try self.buildTag(try self.structuralType(err_payload_ty), try store.internName("MissingField"), &.{});
        try self.emitJump(join, try self.buildTag(try_union, try store.internName("Err"), &.{missing}));
        self.hole = after;
        return result;
    }

    /// Whether a tag union value holds the payload-free tag `label`: the
    /// checker's discriminant-only equality.
    pub fn finishDiscriminantEq(self: *Builder, value_ty: TypeId, label: check.CheckedNames.TagLabelId, negated: bool) Allocator.Error!void {
        const value = self.popValue();
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(label));
        const ty = try self.structuralType(value_ty);
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (try self.closed(ty)) {
            const tags = switch (self.program.store.get(ty)) {
                .tag_union => |union_ty| self.program.store.tagSpan(union_ty.tags),
                .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a discriminant equality's operand is not a tag union"),
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
    pub fn finishFieldAccess(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const access = self.bodies().expr(expr_id).data.field_access;
        var value = self.popValue();
        var record_ty = try self.exprType(access.receiver);
        var misses: std.ArrayList(CFStmtId) = .empty;
        defer misses.deinit(self.allocator);
        const present = try self.program.store.internName(@import("layouts.zig").present_label);
        // A path with a `.?` segment is one flat `Try(value, [MissingField])`,
        // which every exit of the path delivers to one join.
        const optional = for (access.segments) |segment| {
            if (segment.mode == .optional) break true;
        } else false;
        var result_join: ?struct { join: LIR.JoinPointId, after: CFStmtId, result: LocalId } = null;
        if (optional) {
            const result = try self.localOf(try self.exprType(expr_id), .frame);
            const join = self.freshJoin();
            const after = try self.declareMergeJoin(join, result, null);
            result_join = .{ .join = join, .after = after, .result = result };
        }
        for (access.segments) |segment| {
            const name = try self.program.store.internName(self.view.nameStore().recordFieldLabelText(segment.field_name));
            const structural = try self.structuralType(record_ty);
            const record = switch (self.program.store.get(structural)) {
                .record => |record| record,
                .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a field access's receiver is not a record"),
            };
            const fields = self.program.store.fieldSpan(record.fields);
            const position = for (fields, 0..) |field, index| {
                if (field.name == name) break index;
            } else null;
            const field_ty = try self.convert(segment.success_ty);
            // The stored type of the field: its value, or for an optional
            // segment its presence slot.
            const slot_ty = switch (segment.mode) {
                .required => field_ty,
                .optional => try self.program.layouts.presenceSlot(field_ty),
            };
            var stored: LocalId = undefined;
            if (record.ext != null and position == null) {
                // A field of an open row's remainder: only the record's
                // descriptor knows where it is.
                stored = try self.localOf(slot_ty, .view);
                try self.emitDyn(stored, .{ .field_addr_named = .{ .base = value, .desc = try self.descOf(structural), .name = try self.nameDesc(name) } });
            } else {
                const index = position orelse invariant("a field access's field is absent from its record type");
                const expected_kind: types.FieldKind = switch (segment.mode) {
                    .required => .inline_slot,
                    .optional => .presence_slot,
                };
                if (fields[index].kind != expected_kind) invariant("a field access's mode differs from its field's kind");
                stored = try self.readField(structural, value, @intCast(index), slot_ty);
            }
            switch (segment.mode) {
                .required => value = stored,
                .optional => {
                    // `.?`: a missing field ends the path with an error.
                    const miss = try self.newHole();
                    try self.branchOnTag(stored, slot_ty, present, miss);
                    try misses.append(self.allocator, miss);
                    value = (try self.tagPayloadValue(stored, slot_ty, present, 0)).value;
                },
            }
            record_ty = field_ty;
        }
        const exit = result_join orelse return try self.pushValue(value);
        const join = exit.join;
        const try_union = try self.structuralType(try self.exprType(expr_id));
        const store = &self.program.store;
        const ok = try self.buildTag(try_union, try store.internName("Ok"), &.{value});
        try self.emitJump(join, ok);
        const err_payload_ty = (try self.closedVariant(try_union, try store.internName("Err"))).payloads[0];
        for (misses.items) |miss| {
            self.hole = miss;
            const missing = try self.buildTag(try self.structuralType(err_payload_ty), try store.internName("MissingField"), &.{});
            const err = try self.buildTag(try_union, try store.internName("Err"), &.{missing});
            try self.emitJump(join, err);
        }
        self.hole = exit.after;
        try self.pushValue(exit.result);
    }

    /// The stored types of a record's or tuple's components, by checked field
    /// index.
    pub fn structComponentTypes(self: *Builder, raw_ty: TypeId) Allocator.Error![]TypeId {
        const ty = try self.structuralType(raw_ty);
        return switch (self.program.store.get(ty)) {
            .tuple => |items| try self.allocator.dupe(TypeId, self.program.store.span(items)),
            .record => |record| blk: {
                const fields = self.program.store.fieldSpan(record.fields);
                const out = try self.allocator.alloc(TypeId, fields.len);
                for (fields, out) |field, *slot| slot.* = try self.program.layouts.fieldStorage(field);
                break :blk out;
            },
            .primitive, .empty_record, .tag_union, .list, .box, .nominal, .function, .variable => try self.allocator.alloc(TypeId, 0),
        };
    }

    /// `ty` with its nominal wrappers peeled: a nominal type shares its
    /// backing's representation, so a tag or record of a nominal type is
    /// built and matched at its backing.
    pub fn structuralType(self: *Builder, ty: TypeId) Allocator.Error!TypeId {
        var current = ty;
        while (self.program.store.get(current) == .nominal) current = try self.program.converter().backing(current);
        return current;
    }

    /// Field `field_index` (of type `field_ty`) of the record or tuple
    /// `base` of type `ty`, as a borrowed read.
    pub fn readField(self: *Builder, ty: TypeId, base_local: LocalId, field_index: u32, field_ty: TypeId) Allocator.Error!LocalId {
        const local = try self.localOf(field_ty, if (try self.closed(ty)) .frame else .view);
        try self.readFieldInto(local, ty, base_local, field_index, field_ty);
        return local;
    }

    /// `readField` into `target`.
    pub fn readFieldInto(self: *Builder, target: LocalId, ty: TypeId, base_local: LocalId, field_index: u32, field_ty: TypeId) Allocator.Error!void {
        if (try self.closed(ty)) {
            const base_layout = self.store.getLocal(base_local).layout_idx;
            if (self.program.result.layouts.getLayout(base_layout).tag != .struct_) {
                try self.emitLinear(.{ .assign_ref = .{ .target = target, .op = LIR.RefOp.field(base_local, field_index), .next = undefined } });
                return;
            }
            return try self.readSlotInto(target, LIR.RefOp.field(base_local, field_index), field_ty, self.structFieldLayout(base_layout, field_index));
        }
        // A field of an open row ranks among the remainder's fields too, so
        // it is addressed by name.
        switch (self.program.store.get(try self.structuralType(ty))) {
            .record => |record| if (record.ext != null) {
                const name = self.program.store.fieldSpan(record.fields)[field_index].name;
                return try self.emitDyn(target, .{ .field_addr_named = .{ .base = base_local, .desc = try self.descOf(ty), .name = try self.nameDesc(name) } });
            },
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => {},
        }
        try self.emitDyn(target, .{ .field_addr = .{ .base = base_local, .desc = try self.descOf(ty), .field_index = field_index } });
    }

    /// Test that the dynamic tag union `value` holds tag `name`, and queue
    /// its payloads against `args`.
    pub fn testDynamicTag(self: *Builder, nominal_ty: TypeId, name: types.NameId, args: []const CheckedPatternId, value: LocalId, miss: CFStmtId, pending: anytype) Allocator.Error!void {
        const store = &self.program.store;
        const ty = try self.structuralType(nominal_ty);
        const union_ty = switch (store.get(ty)) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag pattern's type is not a tag union"),
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
            try pending.append(self.allocator, .{ .pattern = arg, .value = local, .ty = payload_ty });
        }
    }

    /// Continue when `cond` is true; otherwise continue at the enclosing
    /// match branch's shared `miss` continuation.
    pub fn branchOrMiss(self: *Builder, cond: LocalId, miss: CFStmtId) Allocator.Error!void {
        const then = try self.newHole();
        const branches = try self.store.addCFSwitchBranches(&.{.{ .value = 1, .body = then }});
        try self.emitTerminal(.{ .switch_stmt = .{ .cond = cond, .branches = branches, .default_branch = miss } });
        self.hole = then;
    }

    // -- Statements --

    pub fn stepStmt(self: *Builder, stmt_id: CheckedStatementId) Allocator.Error!void {
        const stmt = self.bodies().statement(stmt_id);
        self.region = stmt.source_region;
        switch (stmt.data) {
            .decl => |decl| {
                // An annotation-only declaration (a method requesting its
                // derivation) has no value, and every use of it resolves to
                // what it requests.
                if (self.bodies().expr(decl.expr).data == .anno_only) return;
                // A recursive value's binders get slots before its value is
                // built, which the callables in the value capture, and the
                // slots are filled once the value is bound.
                if (decl.recursive) {
                    try self.allocRecursiveSlots(decl.pattern);
                    try self.push(.{ .fill_recursive_slots = decl.pattern });
                }
                // A scheme alias with a generalized scheme binds nothing:
                // each use lowers its right-hand side at the use's
                // instantiation (`lowerAliasUse`).
                if (self.bodies().pattern_binder_by_pattern[@backingInt(decl.pattern)].get()) |binder| {
                    if (self.bodies().patternBinder(binder).is_scheme_alias) {
                        if (self.program.local_scopes.get(.{ .module = self.module, .expr = decl.expr })) |scope_id| {
                            const scheme = try self.localScheme(scope_id);
                            if (scheme.scheme_vars.len != 0 or scheme.dictionaries.len != 0) return;
                        }
                    }
                }
                // A value that diverges binds nothing.
                if (self.bodies().exprDiverges(decl.expr, self.inlineExpectMode())) {
                    try self.push(.discard);
                } else {
                    try self.push(.{ .bind = .{ .pattern = decl.pattern, .ty = try self.exprType(decl.expr) } });
                }
                try self.push(.{ .expr = decl.expr });
            },
            .var_ => |var_| {
                if (self.bodies().exprDiverges(var_.expr, self.inlineExpectMode())) {
                    try self.push(.discard);
                } else {
                    try self.push(.{ .bind = .{ .pattern = var_.pattern, .ty = try self.exprType(var_.expr) } });
                }
                try self.push(.{ .expr = var_.expr });
            },
            .reassign => |reassign| {
                try self.push(.{ .bind = .{ .pattern = reassign.pattern, .ty = try self.exprType(reassign.expr) } });
                try self.push(.{ .expr = reassign.expr });
            },
            .var_uninitialized => |var_| {
                const local = try self.localOf(try self.patternType(var_.pattern), .frame);
                try self.emitLinear(.{ .init_uninitialized = .{ .target = local, .next = undefined } });
                try self.bindIrrefutable(var_.pattern, local, try self.patternType(var_.pattern));
            },
            .while_ => |loop| try self.lowerLoop(loop, true),
            // `while True`: the body runs unconditionally, and only a break
            // (in a breakable loop) leaves it.
            .infinite_loop, .breakable_loop => |loop| try self.lowerLoop(loop, false),
            .for_ => |for_| {
                try self.push(.discard);
                try self.pushForLoop(for_.pattern, for_.expr, for_.body, for_.plan, for_.mutations);
            },
            .break_ => {
                if (self.loops.items.len == 0) invariant("a break outside every loop reached Boxy lowering");
                const loop = &self.loops.items[self.loops.items.len - 1];
                loop.reached = true;
                try self.emitJump(loop.join, null);
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
            .runtime_error => {
                const literal = try self.store.insertString("runtime error");
                try self.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
            },
            .dbg => |expr| {
                try self.push(.discard);
                try self.push(.{ .finish_dbg = try self.exprType(expr) });
                try self.push(.{ .expr = expr });
            },
            .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias => {},
            .promoted_proc => {},
            .return_ => |ret| {
                // The enclosing function returns the value; nothing after the
                // statement is reached.
                try self.push(.ret);
                try self.destination_types.put(self.allocator, ret.expr, self.currentWorker().ret);
                try self.push(.{ .expr = ret.expr });
            },
            .pending => invariant("a pending checked statement reached Boxy lowering, which reads only completed checking"),
        }
    }

    /// A condition-driven loop: a head join carrying the loop's mutated
    /// binders, entered first with their current values, and an exit join
    /// carrying them out.
    pub fn lowerLoop(self: *Builder, loop: checked.CheckedConditionLoop, tests_condition: bool) Allocator.Error!void {
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
        try self.loops.append(self.allocator, .{ .join = exit, .reached = tests_condition });
        try self.push(.end_loop);
        if (exit_merge) |m| try self.push(.{ .enter_merge = m });
        try self.push(.{ .resume_at = after });
        if (tests_condition) {
            try self.push(.{ .loop_test = .{ .body = loop.body, .head = head, .exit = exit } });
            try self.push(.{ .expr = loop.cond });
        } else {
            try self.push(.{ .jump_without_value = head });
            try self.push(.discard);
            try self.push(.{ .expr = loop.body });
        }
    }

    pub fn pushForLoop(self: *Builder, pattern: CheckedPatternId, iterable: CheckedExprId, body: CheckedExprId, plan: ?static_dispatch.IteratorForPlanId, mutations: ?checked.LoopMutationPlanId) Allocator.Error!void {
        const plan_id = plan orelse invariant("a for loop has no checked iterator plan");
        // A loop whose pattern checking rejected evaluates its iterable,
        // then crashes where it would begin: its pattern binds nothing.
        const item_ty = self.view.view.static_dispatch_plans.iterator_for_plans[@backingInt(plan_id)].item_ty;
        if (try self.program.converter().hasError(self.module, item_ty) or try self.program.converter().hasError(self.module, self.bodies().pattern(pattern).ty)) {
            try self.push(.{ .rejected_for = plan_id });
            try self.push(.{ .expr = iterable });
            return;
        }
        try self.push(.{ .unreachable_or_unit = {} });
        try self.push(.{ .for_start = .{ .pattern = pattern, .body = body, .plan = plan_id, .mutations = mutations } });
        try self.push(.{ .expr = iterable });
    }

    /// `for pattern in iterable { body }`: the iterable's iterator is the
    /// loop's state; each step calls `next` and matches its Done, Skip, or
    /// One tag (design.md "Iterator `for`").
    pub fn forStart(self: *Builder, pattern: CheckedPatternId, body: CheckedExprId, plan_id: static_dispatch.IteratorForPlanId, mutations: ?checked.LoopMutationPlanId) Allocator.Error!void {
        const table = self.view.view.static_dispatch_plans;
        const plan = table.iterator_for_plans[@backingInt(plan_id)];
        const iterable = self.popValue();
        const iterator_ty = try self.convert(plan.iterator_ty);
        const step_ty = try self.convert(plan.step_ty);
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
        try self.bindIrrefutable(pattern, item.value, item.ty);
        try self.loops.append(self.allocator, .{ .join = exit, .reached = true });
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
    pub fn branchOnTag(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId, miss: CFStmtId) Allocator.Error!void {
        const ty = try self.structuralType(raw_ty);
        const actual = try self.closedLocal(.u32);
        const expected = try self.closedLocal(.u32);
        if (try self.closed(ty)) {
            const variant = try self.closedVariant(ty, name);
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

    pub const ClosedVariant = struct { index: usize, payloads: []const TypeId };

    pub fn closedVariant(self: *Builder, ty: TypeId, name: types.NameId) Allocator.Error!ClosedVariant {
        const own = switch (self.program.store.get(ty)) {
            .tag_union => |union_ty| for (self.program.store.tagSpan(union_ty.tags)) |tag| {
                if (tag.name == name) break tag;
            } else invariant("a tag test's tag is absent from its type"),
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag test's value is not a tag union"),
        };
        return try self.selectVariant(ty, own);
    }

    /// The variant of the closed tag union `ty` that its tag `own` is: the
    /// one with its label and arity, which a checked row lists once.
    pub fn selectVariant(self: *Builder, ty: TypeId, own: types.Tag) Allocator.Error!ClosedVariant {
        const tags = switch (self.program.store.get(ty)) {
            .tag_union => |union_ty| self.program.store.tagSpan(union_ty.tags),
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag's type is not a tag union"),
        };
        for (tags, 0..) |tag, index| {
            if (tag.name != own.name or tag.payloads.len != own.payloads.len) continue;
            return .{ .index = index, .payloads = self.program.store.span(tag.payloads) };
        }
        invariant("a tag is absent from its row");
    }

    /// The tag of `ty` with label `name` and `arity` payloads.
    pub fn ownTag(self: *Builder, ty: TypeId, name: types.NameId, arity: usize) types.Tag {
        const union_ty = switch (self.program.store.get(ty)) {
            .tag_union => |union_ty| union_ty,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag's type is not a tag union"),
        };
        for (self.program.store.tagSpan(union_ty.tags)) |tag| {
            if (tag.name == name and tag.payloads.len == arity) return tag;
        }
        invariant("a tag is absent from its type");
    }

    pub const TypedValue = struct { value: LocalId, ty: TypeId };

    /// Payload `index` of tag `name` in `value`, which holds that tag.
    pub fn tagPayloadValue(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId, index: u32) Allocator.Error!TypedValue {
        const ty = try self.structuralType(raw_ty);
        if (!try self.closed(ty)) {
            const union_ty = self.program.store.get(ty).tag_union;
            const payload_ty = for (self.program.store.tagSpan(union_ty.tags)) |tag| {
                if (tag.name == name) break self.program.store.span(tag.payloads)[index];
            } else invariant("a tag payload's tag is absent from its type");
            const local = try self.localOf(payload_ty, .view);
            try self.emitDyn(local, .{ .tag_payload_addr = .{ .base = value, .desc = try self.descOf(ty), .name = try self.nameDesc(name), .payload_index = index } });
            return .{ .value = local, .ty = payload_ty };
        }
        const variant = try self.closedVariant(ty, name);
        const payload_ty = variant.payloads[index];
        return .{ .value = try self.readTagPayload(value, ty, variant.index, variant.payloads, index), .ty = payload_ty };
    }

    /// The committed layout a closed tag union stores variant `variant`'s
    /// payload in.
    pub fn variantPayloadLayout(self: *Builder, union_ty: TypeId, variant: usize) Allocator.Error!layout.Idx {
        const layouts = &self.program.result.layouts;
        const committed = layouts.getLayout(try self.program.layouts.layoutOf(union_ty));
        if (committed.tag != .tag_union) invariant("a stored tag payload's union has no storage");
        return layouts.getTagUnionInfo(committed).variants.get(variant).payload_layout;
    }

    /// Whether a component of type `ty` stored at `slot` is held in a box
    /// the committed layout inserted to break a recursive type.
    pub fn isRecursionBox(self: *Builder, slot: layout.Idx, ty: TypeId) Allocator.Error!bool {
        if (self.program.result.layouts.getLayout(slot).tag != .box) return false;
        return self.program.store.get(try self.structuralType(ty)) != .box;
    }

    /// `value` (of `ty`) as it is stored at `slot`: boxed when the slot is a
    /// recursion box.
    pub fn intoSlot(self: *Builder, value: LocalId, ty: TypeId, slot: layout.Idx) Allocator.Error!LocalId {
        if (!try self.isRecursionBox(slot, ty)) return value;
        const boxed = try self.closedLocal(slot);
        try self.emitLowLevel(boxed, .box_box, &.{value});
        return boxed;
    }

    /// Read the component at `op`, stored at `slot`, as a value of `ty`:
    /// unboxed when the slot is a recursion box.
    pub fn readSlot(self: *Builder, op: LIR.RefOp, ty: TypeId, slot: layout.Idx) Allocator.Error!LocalId {
        const local = try self.localOf(ty, .frame);
        try self.readSlotInto(local, op, ty, slot);
        return local;
    }

    /// `readSlot` into `target`.
    pub fn readSlotInto(self: *Builder, target: LocalId, op: LIR.RefOp, ty: TypeId, slot: layout.Idx) Allocator.Error!void {
        if (!try self.isRecursionBox(slot, ty)) {
            try self.emitLinear(.{ .assign_ref = .{ .target = target, .op = op, .next = undefined } });
            return;
        }
        const boxed = try self.closedLocal(slot);
        try self.emitLinear(.{ .assign_ref = .{ .target = boxed, .op = op, .next = undefined } });
        try self.emitLowLevel(target, .box_unbox, &.{boxed});
    }

    /// Payload `index` of variant `variant` (with payload types `payloads`)
    /// of the closed tag union `value` of type `union_ty`.
    pub fn readTagPayload(self: *Builder, value: LocalId, union_ty: TypeId, variant: usize, payloads: []const TypeId, index: usize) Allocator.Error!LocalId {
        const local = try self.localOf(payloads[index], .frame);
        try self.readTagPayloadInto(local, value, union_ty, variant, payloads, index);
        return local;
    }

    /// `readTagPayload` into `target`.
    pub fn readTagPayloadInto(self: *Builder, target: LocalId, value: LocalId, union_ty: TypeId, variant: usize, payloads: []const TypeId, index: usize) Allocator.Error!void {
        const payload_layout = try self.variantPayloadLayout(union_ty, variant);
        const whole = LIR.RefOp.tagPayloadStruct(value, @intCast(variant), @intCast(variant));
        if (payloads.len == 1) return try self.readSlotInto(target, whole, payloads[0], payload_layout);
        const payload_struct = try self.closedLocal(payload_layout);
        try self.emitLinear(.{ .assign_ref = .{ .target = payload_struct, .op = whole, .next = undefined } });
        const field_layout = self.structFieldLayout(payload_layout, @intCast(index));
        try self.readSlotInto(target, LIR.RefOp.field(payload_struct, @intCast(index)), payloads[index], field_layout);
    }

    /// The committed layout of field `index` (by checked field index) of the
    /// struct layout `struct_layout`. Every field of a zero-sized struct is
    /// zero-sized.
    pub fn structFieldLayout(self: *Builder, struct_layout: layout.Idx, index: u32) layout.Idx {
        const layouts = &self.program.result.layouts;
        const committed = layouts.getLayout(struct_layout);
        return switch (committed.tag) {
            .struct_ => layouts.getStructFieldLayoutByOriginalIndex(committed.getStruct().idx, index),
            .zst => .zst,
            .scalar, .box, .box_of_zst, .list, .list_of_zst, .closure, .erased_callable, .tag_union, .ptr, .hole, .dynamic => invariant("a component read names a field of a layout that is neither a struct nor zero-sized"),
        };
    }

    /// Field `name` of the record `value`.
    pub fn recordFieldValue(self: *Builder, value: LocalId, raw_ty: TypeId, name: types.NameId) Allocator.Error!TypedValue {
        const ty = try self.structuralType(raw_ty);
        const record = switch (self.program.store.get(ty)) {
            .record => |record| record,
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record field read's value is not a record"),
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
    pub fn emitDispatch(self: *Builder, resolution: static_dispatch.CheckedCallResolution, callable_ty: checked.CheckedTypeId, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const table = self.view.view.static_dispatch_plans;
        switch (resolution) {
            .direct_closed, .direct_parametric => |direct| {
                const node = table.evidenceNode(direct.evidence);
                if (node.target.kind == .local_proc) {
                    const fn_ty = try self.convert(callable_ty);
                    return try self.emitErasedCall(try self.localMethodValue(node, fn_ty), fn_ty, args, result_ty);
                }
                const callee = try self.methodTargetWorker(node.target);
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                const call_fn = try self.convert(callable_ty);
                // Reached through a procedure alias, the target takes the
                // instantiation the call's callable type gives it.
                const instance = if (node.instantiation == .alias) try self.aliasTargetInstance(node, callee) else switch (node.nested) {
                    .resolved => try self.callInstanceAt(callee, subst, table.nestedEvidence(node), call_fn),
                    .from_callable => try self.callableDerivedInstance(callee, subst, call_fn),
                    .through_alias => invariant("alias-scope evidence accompanies only an alias instantiation"),
                };
                defer self.allocator.free(instance.hidden);
                const all = try std.mem.concat(self.allocator, LocalId, &.{ args, instance.hidden });
                defer self.allocator.free(all);
                const target = try self.localOf(instance.ret, .frame);
                try self.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program.worker(callee).proc, .args = try self.store.addLocalSpan(all), .next = undefined } });
                return try self.adaptValue(target, instance.ret, result_ty);
            },
            .evidence_dependent => |dependent| {
                const fn_ty = (try self.scopeInterface(dependent.index)) orelse try self.convert(callable_ty);
                return try self.emitErasedCall(self.scopeDictionary(dependent.index), fn_ty, args, result_ty);
            },
            .checked_error, .@"unreachable" => {
                try self.emitDispatchCrash(if (resolution == .checked_error) .checked_error else .unreachable_value);
                return try self.localOf(result_ty, .frame);
            },
            .structural => invariant("an iterator dispatch resolved to a structural derivation, which no iterator method has"),
            .direct_pending => invariant("a checked module holds a pending dispatch resolution"),
        }
    }

    pub fn loopTest(self: *Builder, body: CheckedExprId, head: LIR.JoinPointId, exit: LIR.JoinPointId) Allocator.Error!void {
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
    pub fn bindIrrefutable(self: *Builder, pattern_id: CheckedPatternId, value: LocalId, ty: TypeId) Allocator.Error!void {
        const miss = try self.newHole();
        try self.testPattern(pattern_id, value, ty, miss);
    }
};

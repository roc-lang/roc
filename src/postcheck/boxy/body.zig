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
    finish_list: CheckedExprId,
    finish_str: struct { count: u32 },
    finish_eq: struct { negated: bool, operand: TypeId },
    /// Pop a Bool and check it as an `expect`.
    finish_expect: CheckedExprId,
    /// Pop the condition of if-branch `index` of `expr`.
    if_test: struct { expr: CheckedExprId, index: u32, join: LIR.JoinPointId },
    /// Pop a value and jump to `join` with it.
    jump: LIR.JoinPointId,
    /// Continue emitting at `hole`.
    resume_at: CFStmtId,
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
    name_locals: std.AutoHashMapUnmanaged(types.NameId, LocalId) = .empty,
    entry_hole: CFStmtId = undefined,
    hole: ?CFStmtId = null,
    join_points: std.ArrayList(LIR.JoinPoint) = .empty,
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

    fn emitJump(self: *Builder, id: LIR.JoinPointId, value: LocalId) Allocator.Error!void {
        const params = for (self.join_points.items) |join| {
            if (join.id == id) break self.store.getLocalSpan(join.params);
        } else invariant("a jump named a join this body never declared");
        if (params.len != 1) invariant("a value join has exactly one parameter");
        const param = LirStore.GuardedList.at(params, 0);
        // The jump assigns the join's parameter.
        try self.emitLinear(.{ .set_local = .{ .target = param, .value = value, .mode = .initialize_join_param, .next = undefined } });
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
                if (record.ext != null) invariant("composing the descriptor of an open record row is not yet lowered by Boxy");
                const fields = try self.allocator.dupe(types.Field, store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                for (fields) |field| try out.append(self.allocator, try self.program.layouts.fieldStorage(field));
            },
            .tag_union => |tag_union| {
                if (tag_union.ext != null) invariant("composing the descriptor of an open tag row is not yet lowered by Boxy");
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
                break :blk .{ .desc_record = .{ .names = try self.store.addLocalSpan(names), .descs = try self.store.addLocalSpan(descs) } };
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
        const worker = self.currentWorker();
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
        const worker = self.currentWorker();
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
        const worker = self.currentWorker();
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
        if (capture_count != 0) {
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
        const worker = self.currentWorker();
        const target_worker = self.program.worker(target);
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
        const worker = self.currentWorker();
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
        const worker = self.currentWorker();
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
            .finish_call => |call| {
                defer self.allocator.free(call.hidden);
                try self.finishCall(call.callee, call.hidden, call.arg_count, call.result);
            },
            .finish_low_level => |ll| try self.finishLowLevel(ll.op, ll.arg_count, ll.result, ll.arg_types),
            .finish_record => |expr| try self.finishRecord(expr),
            .finish_erased_call => |call| try self.finishErasedCall(call.fn_ty, call.arg_count, call.result),
            .finish_tuple => |expr| try self.finishTuple(expr),
            .finish_tag => |expr| try self.finishTag(expr),
            .finish_list => |expr| try self.finishList(expr),
            .finish_str => |str| try self.finishStr(str.count),
            .finish_eq => |eq| try self.finishEq(eq.negated, eq.operand),
            .finish_expect => |expr| {
                _ = expr;
                const cond = self.popValue();
                try self.emitLinear(.{ .expect = .{ .condition = cond, .site = try self.expectSite(), .next = undefined } });
                try self.pushValue(try self.unitValue());
            },
            .if_test => |test_| try self.ifTest(test_.expr, test_.index, test_.join),
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
            .zero_argument_tag => try self.finishTag(expr_id),
            .list => |items| {
                try self.push(.{ .finish_list = expr_id });
                try self.pushExprsReversed(items);
            },
            .empty_list => try self.finishList(expr_id),
            .nominal => |nominal| try self.push(.{ .expr = nominal.backing_expr }),
            .lambda => try self.lowerClosure(expr_id, expr_id, &.{}),
            .closure => |closure| try self.lowerClosure(expr_id, closure.lambda, closure.captures),
            .row_coerce => |coerce| try self.push(.{ .expr = coerce.value }),
            .structural_eq => |eq| {
                if (eq.discriminant != null) invariant("discriminant-only equality is not yet lowered by Boxy");
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
                const after = try self.declareJoin(join, &.{result});
                try self.push(.{ .push_value = result });
                try self.push(.{ .resume_at = after });
                try self.push(.{ .if_test = .{ .expr = expr_id, .index = 0, .join = join } });
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
        if (numeral.conversion_root != null) invariant("numerals converted by a compile-time root are not yet lowered by Boxy");
        if (numeral.plan != null) invariant("numerals converted by from_numeral are not yet lowered by Boxy");
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

    fn resolvedRef(self: *const Builder, id: checked.ResolvedValueRefId) checked.ResolvedValueRefRecord {
        return self.view.view.resolved_value_refs.records[@backingInt(id)];
    }

    fn lowerLookupLocal(self: *Builder, expr_id: CheckedExprId, pattern: CheckedPatternId, resolved: ?checked.ResolvedValueRefId) Allocator.Error!void {
        _ = expr_id;
        if (resolved) |id| switch (self.resolvedRef(id).ref) {
            .local_param, .local_value, .pattern_binder, .local_mutable_version => |binding| {
                return try self.pushValue(self.binders.get(binding.binder) orelse invariant("a local lookup named a binder this body never bound"));
            },
            else => |ref| invariantFmt("local lookup of a {s} is not yet lowered by Boxy", .{@tagName(ref)}),
        };
        const binder = self.bodies().pattern_binder_by_pattern[@backingInt(pattern)].get() orelse invariant("a local lookup named a pattern without a binder");
        try self.pushValue(self.binders.get(binder) orelse invariant("a local lookup named a binder this body never bound"));
    }

    fn lowerLookupExternal(self: *Builder, expr_id: CheckedExprId, resolved: checked.ResolvedValueRefId) Allocator.Error!void {
        _ = expr_id;
        invariantFmt("external value lookups ({s}) are not yet lowered by Boxy outside calls", .{@tagName(self.resolvedRef(resolved).ref)});
    }

    /// The worker a procedure use names.
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
            .top_level_proc, .imported_proc, .promoted_top_level_proc => |use| try self.procedureUseWorker(use),
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
        const target = self.program.worker(callee);
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
        const table = self.view.view.static_dispatch_plans;
        const call_plan = self.dispatchPlan(plan_id);
        const operands = call_plan.argsSlice(table);
        const result_ty = try self.exprType(expr_id);
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
                    switch (operands[index]) {
                        .checked_expr => |operand| try self.push(.{ .expr = operand }),
                        else => invariantFmt("dispatch operand {s} is not yet lowered by Boxy", .{@tagName(operands[index])}),
                    }
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
                    switch (operands[index]) {
                        .checked_expr => |operand| try self.push(.{ .expr = operand }),
                        else => invariantFmt("dispatch operand {s} is not yet lowered by Boxy", .{@tagName(operands[index])}),
                    }
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
            if (!self.program.store.isClosed(capture_ty.*)) invariant("closures capturing values of open types are not yet lowered by Boxy");
            const binder = self.bodies().pattern_binder_by_pattern[@backingInt(capture.pattern)].get() orelse
                invariant("a closure capture named a pattern without a binder");
            values[index] = self.binders.get(binder) orelse invariant("a closure captured a binder this body never bound");
        }
        @memcpy(values[captures.len..][0..self.var_descs.len], self.var_descs);
        @memcpy(values[captures.len + self.var_descs.len ..], self.dict_locals);
        const callee = try self.program.lambdaWorker(self.worker_id, lambda_expr, captures, capture_types, fn_ty);
        const target = try self.localOf(fn_ty, .frame);
        var capture: ?LocalId = null;
        var capture_layout: ?layout.Idx = null;
        var on_drop: LIR.ErasedCallableOnDrop = .none;
        if (values.len != 0) {
            const record_layout = try self.captureLayout(capture_types, self.var_descs.len, self.dict_locals.len);
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
        try self.pushValue(target);
    }

    /// Pop a function value's arguments and the function value, and call it
    /// through the uniform erased-callable ABI.
    fn finishErasedCall(self: *Builder, fn_ty: TypeId, arg_count: u32, result_ty: TypeId) Allocator.Error!void {
        const args = try self.popValues(arg_count);
        defer self.allocator.free(args);
        const closure = self.popValue();
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
        try self.pushValue(target);
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
        const after = try self.declareJoin(join, &.{result});
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
        const record_ty = switch (self.program.store.get(ty)) {
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

    fn finishTag(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const expr = self.bodies().expr(expr_id);
        const label, const arg_count: usize = switch (expr.data) {
            .tag => |tag| .{ tag.name, tag.args.len },
            .zero_argument_tag => |tag| .{ tag.name, 0 },
            else => unreachable, // only tag expressions finish as tags
        };
        const ty = try self.exprType(expr_id);
        const values = try self.popValues(@intCast(arg_count));
        defer self.allocator.free(values);
        const name = try self.program.store.internName(self.view.nameStore().tagLabelText(label));
        if (!self.program.store.isClosed(ty)) {
            const target = try self.localOf(ty, .frame);
            try self.emitDyn(target, .{ .tag = .{ .desc = try self.descOf(try self.structuralType(ty)), .name = try self.nameDesc(name), .payloads = try self.store.addLocalSpan(values) } });
            return try self.pushValue(target);
        }
        const rep = self.program.joins.representative(try self.structuralType(ty));
        const union_ty = switch (self.program.store.get(rep)) {
            .tag_union => |value| value,
            .primitive => |primitive| {
                if (primitive != .bool) invariant("a tag expression's type is a non-Bool primitive");
                const value = try self.boolValue(std.mem.eql(u8, self.program.store.nameText(name), "True"));
                return try self.pushValue(value);
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
        try self.pushValue(target);
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

    fn ifTest(self: *Builder, expr_id: CheckedExprId, index: u32, join: LIR.JoinPointId) Allocator.Error!void {
        const if_ = self.bodies().expr(expr_id).data.if_;
        const cond = self.popValue();
        const holes = try self.emitBoolSwitch(cond);
        // Pushed in reverse execution order: the then-branch runs first, then
        // the else side continues at its own hole.
        if (index + 1 < if_.branches.len) {
            try self.push(.{ .if_test = .{ .expr = expr_id, .index = index + 1, .join = join } });
            try self.push(.{ .expr = if_.branches[index + 1].cond });
        } else {
            try self.push(.{ .jump = join });
            try self.push(.{ .expr = if_.final_else });
        }
        try self.push(.{ .resume_at = holes.otherwise });
        try self.push(.{ .jump = join });
        try self.push(.{ .expr = if_.branches[index].body });
        self.hole = holes.then;
    }

    fn matchStart(self: *Builder, expr_id: CheckedExprId) Allocator.Error!void {
        const scrutinee = self.popValue();
        const result = try self.localOf(try self.exprType(expr_id), .frame);
        const join = self.freshJoin();
        const after = try self.declareJoin(join, &.{result});
        try self.push(.{ .push_value = result });
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
                                const op: LIR.RefOp = if (committed.tag == .tag_union)
                                    LIR.RefOp.tagPayload(item.value, @intCast(if (tag.args.len == 1) 0 else arg_index), @intCast(variant), @intCast(variant))
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
                    const ty = try self.patternType(item.pattern);
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
                    const ty = try self.patternType(item.pattern);
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
                try self.push(.{ .bind = decl.pattern });
                try self.push(.{ .expr = decl.expr });
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

    /// Bind an irrefutable pattern (a parameter or a `decl`) to `value`.
    /// Destructuring binds through the same tests a match uses; an
    /// irrefutable pattern never takes their miss path.
    fn bindIrrefutable(self: *Builder, pattern_id: CheckedPatternId, value: LocalId) Allocator.Error!void {
        const miss = try self.newHole();
        try self.testPattern(pattern_id, value, miss);
    }
};

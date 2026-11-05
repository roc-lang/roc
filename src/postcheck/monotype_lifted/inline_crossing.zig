//! Productivity of SpecConstr's direct-call inlining, decided by crossing
//! queries.
//!
//! The value-aware clone simplifies a known value in a few places: a `match`
//! on a known tag selects its arm, a call of a known callable inlines the
//! callable, a loop entered with a known constructor scalarizes it, and a
//! field or item read of a known record or tuple takes the part without
//! building the aggregate. Inlining a direct call pays for its copied body
//! only when a known value crosses the call's boundary and meets such a
//! consumer on the other side: a known part of an argument or capture meets
//! one inside the callee's extent (or, through the callee's result, in the
//! caller's continuation), or a part the callee builds on every path that
//! returns meets one in the caller's continuation.
//!
//! Nothing is summarized for the whole program. `Index` is built once per
//! SpecConstr run, in time linear in the program: the call graph's strongly
//! connected components, each source expression's owning function, and every
//! use of every expression and local. A `Session` answers queries at call
//! sites, each one a forward walk from the crossing value that follows only
//! parts the clone actually knows. See design.md "SpecConstr And Loop
//! Scalarization".

const std = @import("std");
const collections = @import("collections");
const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Mono = @import("../monotype/ast.zig");
const WrapperBody = @import("wrapper_body.zig");
const names = @import("check").CheckedNames;

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// A consumer that gains when it meets a known value.
pub const Consumer = enum {
    match,
    call,
    loop,
    /// A field or item read, or a record or tuple pattern. A call-pattern
    /// worker already takes a known record or tuple argument apart without
    /// inlining, so a read makes an inline productive only when it reads a
    /// part of the call's result.
    project,
};

/// A set of consumers.
pub const Consumers = std.EnumSet(Consumer);

/// The consumers a known argument or capture can meet.
pub const argument_consumers: Consumers = .initMany(&.{ .match, .call, .loop });

/// The consumers a part of a call's result can meet.
pub const result_consumers: Consumers = .initMany(&.{ .match, .call, .loop, .project });

/// One step from a value into a part of it. Field and tag labels are
/// interned, so equal labels have equal ids, and a read meets the part it
/// names by label whatever type a row coercion shows the part at.
pub const Step = struct {
    kind: Kind,
    label: u32,
    index: u32,

    /// What kind of part a step names.
    pub const Kind = enum(u8) { field, item, payload, capture };

    /// The record field `name`.
    pub fn field(name: names.RecordFieldNameId) Step {
        return .{ .kind = .field, .label = @backingInt(name), .index = 0 };
    }

    /// Item `index` of a tuple.
    pub fn item(index: u32) Step {
        return .{ .kind = .item, .label = 0, .index = index };
    }

    /// Payload `index` of tag `tag`.
    pub fn payload(tag: names.TagNameId, index: u32) Step {
        return .{ .kind = .payload, .label = @backingInt(tag), .index = index };
    }

    /// Capture slot `slot` of a value of function `fn_id`.
    pub fn capture(fn_id: Ast.FnId, slot: u32) Step {
        return .{ .kind = .capture, .label = @backingInt(fn_id), .index = slot };
    }

    /// Whether `a` and `b` name the same part.
    pub fn eql(a: Step, b: Step) bool {
        return a.kind == b.kind and a.label == b.label and a.index == b.index;
    }

    /// Whether this step names record field `name`.
    pub fn isField(self: Step, name: names.RecordFieldNameId) bool {
        return self.kind == .field and self.label == @backingInt(name);
    }

    /// Whether this step names tuple item `index`.
    pub fn isItem(self: Step, index: u32) bool {
        return self.kind == .item and self.index == index;
    }

    /// Whether this step names payload `index` of tag `tag`.
    pub fn isPayload(self: Step, tag: names.TagNameId, index: u32) bool {
        return self.kind == .payload and self.label == @backingInt(tag) and self.index == index;
    }
};

/// A parameter or capture of a function.
pub const Slot = struct {
    kind: Kind,
    index: u32,

    /// Whether a slot is a parameter or a capture.
    pub const Kind = enum(u8) { arg, capture };
};

/// An interned path of steps from a seed's root; `root` is the seed itself.
pub const PathId = enum(u32) { root = 0, _ };

const none: u32 = std.math.maxInt(u32);

/// How the code around an expression consumes its value. Lifted bodies share
/// expression nodes, within a body and across bodies, so a node can have
/// several parents; each use belongs to one body.
const Use = union(enum) {
    stop,
    /// The parent's value is this value.
    same: Ast.ExprId,
    /// The parent reads a part of this value: a field or tuple access.
    receiver: Ast.ExprId,
    /// The parent is a constructor holding this value at `step`. `loop` is
    /// the innermost loop enclosing the parent in this body.
    operand: struct { parent: Ast.ExprId, step: Step, loop: u32 },
    /// The parent is a record update of this value.
    update_base: struct { parent: Ast.ExprId, loop: u32 },
    /// A pattern binds this value.
    pattern: Ast.PatId,
    /// A recursive binding's pattern binds this value, which may read the
    /// binding: a back edge.
    rec_pattern: Ast.PatId,
    scrutinee: Ast.ExprId,
    callee: Ast.ExprId,
    proc_arg: struct { call: Ast.ExprId, callee: Ast.FnId, slot: Slot },
    loop_init: struct { loop: Ast.ExprId, local: Ast.LocalId },
    /// A jump or `continue` value. `loop` is the loop a back edge re-enters.
    into_local: struct { local: Ast.LocalId, loop: ?Ast.ExprId },
    result,
};

const UseRow = struct { body: u32, use: Use, next: u32 };

const LocalUseRow = struct { body: u32, expr: Ast.ExprId, next: u32 };

const LoopKey = struct { body: u32, loop: u32 };

/// The program-wide data every query reads. Built once, before any clone or
/// discovery task runs, and read-only afterwards, so tasks on any thread
/// share it.
pub const Index = struct {
    gpa: Allocator,
    /// Each original function's source body.
    bodies: []const ?Ast.ExprId,
    /// The first use of each source expression, by expression id, in a
    /// list of every body's uses of it.
    use_head: []u32,
    uses: std.ArrayList(UseRow) = .empty,
    local_use_head: collections.DenseMapUnmanaged(Ast.LocalId, u32) = .empty,
    local_use_rows: std.ArrayList(LocalUseRow) = .empty,
    /// The loop enclosing each loop of each body.
    enclosing_loop: std.AutoHashMapUnmanaged(LoopKey, u32) = .empty,
    /// Locals bound once by a plain binding, to their value; null for a
    /// local bound more than once or recursively.
    defs: collections.DenseMapUnmanaged(Ast.LocalId, ?Ast.ExprId) = .empty,
    slots: collections.DenseMapUnmanaged(Ast.LocalId, Slot) = .empty,
    /// The `return` values of each function.
    returns: []std.ArrayList(Ast.ExprId),
    /// Each function's strongly connected component of the call graph.
    scc: []u32,
    on_cycle: []bool,
    /// Whether each function always inlines as a wrapper (`WrapperBody`)
    /// that lies on no call cycle and passes none of its calls a value it
    /// builds where the callee consumes it.
    wrapper: []bool,

    /// Index the source bodies of `program`'s original functions.
    pub fn build(gpa: Allocator, program: *const Ast.Program, source_bodies: []const ?Ast.ExprId) Allocator.Error!*Index {
        const expr_count = program.exprCount();
        const self = try gpa.create(Index);
        errdefer gpa.destroy(self);
        self.* = .{
            .gpa = gpa,
            .bodies = try gpa.dupe(?Ast.ExprId, source_bodies),
            .use_head = try gpa.alloc(u32, expr_count),
            .returns = try gpa.alloc(std.ArrayList(Ast.ExprId), source_bodies.len),
            .scc = try gpa.alloc(u32, source_bodies.len),
            .on_cycle = try gpa.alloc(bool, source_bodies.len),
            .wrapper = try gpa.alloc(bool, source_bodies.len),
        };
        @memset(self.use_head, none);
        for (self.returns) |*list| list.* = .empty;
        @memset(self.wrapper, false);
        for (source_bodies, 0..) |maybe_body, index| {
            const body = maybe_body orelse continue;
            try self.indexFn(program, @fromBackingInt(@as(u32, @intCast(index))), body);
        }
        try self.computeSccs(program);
        return self;
    }

    /// Free the index.
    pub fn deinit(self: *Index) void {
        const gpa = self.gpa;
        gpa.free(self.bodies);
        gpa.free(self.use_head);
        self.uses.deinit(gpa);
        self.local_use_head.deinit(gpa);
        self.local_use_rows.deinit(gpa);
        self.enclosing_loop.deinit(gpa);
        self.defs.deinit(gpa);
        self.slots.deinit(gpa);
        for (self.returns) |*list| list.deinit(gpa);
        gpa.free(self.returns);
        gpa.free(self.scc);
        gpa.free(self.on_cycle);
        gpa.free(self.wrapper);
        gpa.destroy(self);
    }

    fn isOriginal(self: *const Index, fn_id: Ast.FnId) bool {
        return @backingInt(fn_id) < self.bodies.len;
    }

    /// Whether the source body of `fn_id` holds `expr`.
    pub fn bodyHolds(self: *const Index, fn_id: Ast.FnId, expr: Ast.ExprId) bool {
        const raw = @backingInt(expr);
        if (raw >= self.use_head.len) return false;
        var row = self.use_head[raw];
        while (row != none) : (row = self.uses.items[row].next) {
            if (self.uses.items[row].body == @backingInt(fn_id)) return true;
        }
        return false;
    }

    /// Whether `fn_id` always inlines as a wrapper.
    pub fn isWrapper(self: *const Index, fn_id: Ast.FnId) bool {
        const raw = @backingInt(fn_id);
        if (raw >= self.wrapper.len) Common.invariant("inline crossing was asked about a function created after it was indexed");
        return self.wrapper[raw];
    }

    /// Whether loop `inner` of `fn_id`'s body is `outer` or lies inside it.
    fn loopWithin(self: *const Index, fn_id: Ast.FnId, inner: u32, outer: Ast.ExprId) bool {
        var current = inner;
        while (current != none) : (current = self.enclosing_loop.get(.{ .body = @backingInt(fn_id), .loop = current }) orelse
            Common.invariant("inline crossing met a loop its body did not index"))
        {
            if (current == @backingInt(outer)) return true;
        }
        return false;
    }

    fn sameScc(self: *const Index, a: Ast.FnId, b: Ast.FnId) bool {
        if (!self.isOriginal(a) or !self.isOriginal(b)) return false;
        return self.scc[@backingInt(a)] == self.scc[@backingInt(b)];
    }

    const IndexTask = union(enum) {
        expr: struct { id: Ast.ExprId, use: Use },
        stmt: Ast.StmtId,
        pop_loop,
    };

    const LoopFrame = struct { loop: Ast.ExprId, params: Ast.Span(Ast.TypedLocal) };

    /// Record a use of `expr` in body `body`; whether it is the body's first.
    fn addUse(self: *Index, body: u32, expr: Ast.ExprId, use: Use) Allocator.Error!bool {
        const raw = @backingInt(expr);
        var fresh = true;
        var row = self.use_head[raw];
        while (row != none) : (row = self.uses.items[row].next) {
            const existing = self.uses.items[row];
            if (existing.body != body) continue;
            fresh = false;
            if (std.meta.eql(existing.use, use)) return false;
        }
        try self.uses.append(self.gpa, .{ .body = body, .use = use, .next = self.use_head[raw] });
        self.use_head[raw] = @intCast(self.uses.items.len - 1);
        return fresh;
    }

    fn noteDef(self: *Index, program: *const Ast.Program, pat: Ast.PatId, value: ?Ast.ExprId) Allocator.Error!void {
        switch (program.getPat(pat).data) {
            .bind => |local| {
                const gop = try self.defs.getOrPut(self.gpa, local);
                gop.value_ptr.* = if (gop.found_existing) null else value;
            },
            .wildcard, .as, .tag, .nominal, .list, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern, .record, .tuple => {},
        }
    }

    /// Record every use of every expression and local of one body.
    fn indexFn(self: *Index, program: *const Ast.Program, fn_id: Ast.FnId, body: Ast.ExprId) Allocator.Error!void {
        const gpa = self.gpa;
        const p = program;
        const raw_fn = @backingInt(fn_id);
        const f = p.getFn(fn_id);
        const args = p.typedLocalSpan(f.args);
        for (0..args.len) |i| try self.slots.put(gpa, GuardedList.at(args, i).local, .{ .kind = .arg, .index = @intCast(i) });
        const caps = p.typedLocalSpan(f.captures);
        for (0..caps.len) |i| try self.slots.put(gpa, GuardedList.at(caps, i).local, .{ .kind = .capture, .index = @intCast(i) });

        var tasks: std.ArrayList(IndexTask) = .empty;
        defer tasks.deinit(gpa);
        var loops: std.ArrayList(LoopFrame) = .empty;
        defer loops.deinit(gpa);
        var joins: collections.DenseMapUnmanaged(Mono.JoinPointId, Ast.Span(Ast.TypedLocal)) = .empty;
        defer joins.deinit(gpa);
        try tasks.append(gpa, .{ .expr = .{ .id = body, .use = .result } });
        while (tasks.pop()) |task| switch (task) {
            .pop_loop => _ = loops.pop() orelse Common.invariant("inline crossing loop stack underflow"),
            .stmt => |sid| switch (p.getStmt(sid)) {
                .let_ => |let_| {
                    try self.noteDef(p, let_.pat, if (let_.recursive) null else let_.value);
                    const use: Use = if (let_.recursive) .{ .rec_pattern = let_.pat } else .{ .pattern = let_.pat };
                    try tasks.append(gpa, .{ .expr = .{ .id = let_.value, .use = use } });
                },
                .expr, .expect, .dbg => |e| try tasks.append(gpa, .{ .expr = .{ .id = e, .use = .stop } }),
                .return_ => |ret| {
                    try self.returns[raw_fn].append(gpa, ret.value);
                    try tasks.append(gpa, .{ .expr = .{ .id = ret.value, .use = .result } });
                },
                .uninitialized => |pat| try self.noteDef(p, pat, null),
                .crash, .checked_error => {},
            },
            .expr => |item| {
                const e = item.id;
                const raw = @backingInt(e);
                if (raw >= self.use_head.len) Common.invariant("a source body held an expression created after indexing began");
                // A node this body already reached has its children indexed.
                if (!try self.addUse(raw_fn, e, item.use)) continue;
                const loop: u32 = if (loops.getLastOrNull()) |l| @backingInt(l.loop) else none;
                const push = struct {
                    fn push(t: *std.ArrayList(IndexTask), a: Allocator, id: Ast.ExprId, use: Use) Allocator.Error!void {
                        try t.append(a, .{ .expr = .{ .id = id, .use = use } });
                    }
                }.push;
                switch (p.getExpr(e).data) {
                    .local => |local| {
                        const gop = try self.local_use_head.getOrPut(gpa, local);
                        const next = if (gop.found_existing) gop.value_ptr.* else none;
                        try self.local_use_rows.append(gpa, .{ .body = raw_fn, .expr = e, .next = next });
                        gop.value_ptr.* = @intCast(self.local_use_rows.items.len - 1);
                    },
                    .typed_boundary => |b| try push(&tasks, gpa, b.value, .{ .same = e }),
                    .row_widen => |w| try push(&tasks, gpa, w.value, .{ .same = e }),
                    .nominal => |b| try push(&tasks, gpa, b, .{ .same = e }),
                    .static_data_candidate => |c| switch (c.value) {
                        .runtime => |r| try push(&tasks, gpa, r, .{ .same = e }),
                        .stored => {},
                    },
                    .tag => |tag| {
                        const ps = p.exprSpan(tag.payloads);
                        for (0..ps.len) |i| try push(&tasks, gpa, GuardedList.at(ps, i), .{ .operand = .{ .parent = e, .step = Step.payload(tag.name, @intCast(i)), .loop = loop } });
                    },
                    .record => |span| {
                        const fs = p.fieldExprSpan(span);
                        for (0..fs.len) |i| {
                            const fe = GuardedList.at(fs, i);
                            try push(&tasks, gpa, fe.value, .{ .operand = .{ .parent = e, .step = Step.field(fe.name), .loop = loop } });
                        }
                    },
                    .record_update => |u| {
                        const fs = p.fieldExprSpan(u.fields);
                        for (0..fs.len) |i| {
                            const fe = GuardedList.at(fs, i);
                            try push(&tasks, gpa, fe.value, .{ .operand = .{ .parent = e, .step = Step.field(fe.name), .loop = loop } });
                        }
                        try push(&tasks, gpa, u.base, .{ .update_base = .{ .parent = e, .loop = loop } });
                    },
                    .tuple => |span| {
                        const is = p.exprSpan(span);
                        for (0..is.len) |i| try push(&tasks, gpa, GuardedList.at(is, i), .{ .operand = .{ .parent = e, .step = Step.item(@intCast(i)), .loop = loop } });
                    },
                    .field_access => |a| try push(&tasks, gpa, a.receiver, .{ .receiver = e }),
                    .tuple_access => |a| try push(&tasks, gpa, a.tuple, .{ .receiver = e }),
                    .let_ => |let_| {
                        try self.noteDef(p, let_.bind, let_.value);
                        try push(&tasks, gpa, let_.value, .{ .pattern = let_.bind });
                        try push(&tasks, gpa, let_.rest, .{ .same = e });
                    },
                    .block => |block| {
                        const ss = p.stmtSpan(block.statements);
                        for (0..ss.len) |i| try tasks.append(gpa, .{ .stmt = GuardedList.at(ss, i) });
                        try push(&tasks, gpa, block.final_expr, .{ .same = e });
                    },
                    .if_ => |if_| {
                        const bs = p.ifBranchSpan(if_.branches);
                        for (0..bs.len) |i| {
                            const b = GuardedList.at(bs, i);
                            try push(&tasks, gpa, b.cond, .stop);
                            try push(&tasks, gpa, b.body, .{ .same = e });
                        }
                        try push(&tasks, gpa, if_.final_else, .{ .same = e });
                    },
                    .if_initialized_payload => |s| {
                        try push(&tasks, gpa, s.cond, .stop);
                        try push(&tasks, gpa, s.initialized, .{ .same = e });
                        try push(&tasks, gpa, s.uninitialized, .{ .same = e });
                    },
                    .match_ => |m| {
                        try push(&tasks, gpa, m.scrutinee, .{ .scrutinee = e });
                        const bs = p.branchSpan(m.branches);
                        for (0..bs.len) |i| {
                            const b = GuardedList.at(bs, i);
                            const bind = p.stmtSpan(b.bindings);
                            for (0..bind.len) |j| try tasks.append(gpa, .{ .stmt = GuardedList.at(bind, j) });
                            if (b.guard) |g| try push(&tasks, gpa, g, .stop);
                            try push(&tasks, gpa, b.body, .{ .same = e });
                        }
                    },
                    // The clone binds the `Ok` payload as a runtime value, so
                    // nothing read through it reaches the tried expression.
                    .try_sequence => |s| {
                        try push(&tasks, gpa, s.try_expr, .stop);
                        try push(&tasks, gpa, s.ok_body, .{ .same = e });
                    },
                    .try_record_sequence => |s| {
                        try push(&tasks, gpa, s.try_expr, .stop);
                        try push(&tasks, gpa, s.ok_body, .{ .same = e });
                    },
                    .loop_ => |loop_expr| {
                        const params = p.typedLocalSpan(loop_expr.params);
                        const inits = p.exprSpan(loop_expr.initial_values);
                        if (params.len != inits.len) Common.invariant("loop initial values differ in count from its parameters");
                        for (0..inits.len) |i| try push(&tasks, gpa, GuardedList.at(inits, i), .{ .loop_init = .{ .loop = e, .local = GuardedList.at(params, i).local } });
                        try self.enclosing_loop.put(gpa, .{ .body = raw_fn, .loop = raw }, loop);
                        try tasks.append(gpa, .pop_loop);
                        try push(&tasks, gpa, loop_expr.body, .stop);
                        try loops.append(gpa, .{ .loop = e, .params = loop_expr.params });
                    },
                    .break_ => |maybe| if (maybe) |v| {
                        const l = loops.getLastOrNull() orelse Common.invariant("break outside a loop reached inline crossing");
                        try push(&tasks, gpa, v, .{ .same = l.loop });
                    },
                    .continue_ => |c| {
                        const l = loops.getLastOrNull() orelse Common.invariant("continue outside a loop reached inline crossing");
                        const vs = p.exprSpan(c.values);
                        const params = p.typedLocalSpan(l.params);
                        if (params.len != vs.len) Common.invariant("continue values differ in count from the loop parameters");
                        for (0..vs.len) |i| try push(&tasks, gpa, GuardedList.at(vs, i), .{ .into_local = .{ .local = GuardedList.at(params, i).local, .loop = l.loop } });
                    },
                    .join_point => |j| {
                        try joins.put(gpa, j.id, j.params);
                        try push(&tasks, gpa, j.remainder, .{ .same = e });
                        try push(&tasks, gpa, j.body, .{ .same = e });
                    },
                    .jump => |j| {
                        const params = p.typedLocalSpan(joins.get(j.target) orelse Common.invariant("jump to an undeclared join point reached inline crossing"));
                        const as = p.exprSpan(j.args);
                        if (params.len != as.len) Common.invariant("jump values differ in count from their target's parameters");
                        for (0..as.len) |i| try push(&tasks, gpa, GuardedList.at(as, i), .{ .into_local = .{ .local = GuardedList.at(params, i).local, .loop = null } });
                        const lps = p.typedLocalSpan(j.loop_params);
                        const lvs = p.exprSpan(j.loop_values);
                        if (lps.len != lvs.len) Common.invariant("jump loop values differ in count from their parameters");
                        const l = loops.getLastOrNull();
                        for (0..lvs.len) |i| try push(&tasks, gpa, GuardedList.at(lvs, i), .{ .into_local = .{
                            .local = GuardedList.at(lps, i).local,
                            .loop = if (l) |frame| frame.loop else Common.invariant("a jump rebound loop parameters outside a loop"),
                        } });
                    },
                    .return_ => |ret| {
                        try self.returns[raw_fn].append(gpa, ret.value);
                        try push(&tasks, gpa, ret.value, .result);
                    },
                    .fn_ref => |r| {
                        const ops = p.captureOperandSpan(r.captures);
                        for (0..ops.len) |i| {
                            const op = GuardedList.at(ops, i);
                            try push(&tasks, gpa, op.value, .{ .operand = .{ .parent = e, .step = Step.capture(r.fn_id, captureSlot(p, r.fn_id, op.id)), .loop = loop } });
                        }
                    },
                    .call_value => |call| {
                        try push(&tasks, gpa, call.callee, .{ .callee = e });
                        const as = p.exprSpan(call.args);
                        for (0..as.len) |i| try push(&tasks, gpa, GuardedList.at(as, i), .stop);
                    },
                    .call_proc => |call| {
                        const as = p.exprSpan(call.args);
                        const ops = p.captureOperandSpan(call.captures);
                        if (Ast.localDirectCallee(call)) |callee| {
                            for (0..as.len) |i| try push(&tasks, gpa, GuardedList.at(as, i), .{ .proc_arg = .{ .call = e, .callee = callee, .slot = .{ .kind = .arg, .index = @intCast(i) } } });
                            for (0..ops.len) |i| {
                                const op = GuardedList.at(ops, i);
                                try push(&tasks, gpa, op.value, .{ .proc_arg = .{ .call = e, .callee = callee, .slot = .{ .kind = .capture, .index = captureSlot(p, callee, op.id) } } });
                            }
                        } else {
                            for (0..as.len) |i| try push(&tasks, gpa, GuardedList.at(as, i), .stop);
                            for (0..ops.len) |i| try push(&tasks, gpa, GuardedList.at(ops, i).value, .stop);
                        }
                    },
                    .dbg, .expect => |v| try push(&tasks, gpa, v, .stop),
                    .list => |span| {
                        const is = p.exprSpan(span);
                        for (0..is.len) |i| try push(&tasks, gpa, GuardedList.at(is, i), .stop);
                    },
                    .low_level, .structural_eq, .structural_hash, .expect_err, .literal_rejected => {
                        var children: std.ArrayList(Ast.ExprChild) = .empty;
                        defer children.deinit(gpa);
                        try Ast.appendChildren(gpa, p, e, &children);
                        for (children.items) |child| switch (child) {
                            .expr => |child_expr| try push(&tasks, gpa, child_expr, .stop),
                            .stmt => |s| try tasks.append(gpa, .{ .stmt = s }),
                        };
                    },
                    .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .uninitialized, .uninitialized_payload, .crash, .checked_error, .comptime_exhaustiveness_failed => {},
                    .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached inline crossing"),
                }
            },
        };
    }

    /// Tarjan's algorithm over direct calls and function values, on an
    /// explicit stack so call chains never become native call depth.
    fn computeSccs(self: *Index, program: *const Ast.Program) Allocator.Error!void {
        const gpa = self.gpa;
        const n = self.bodies.len;
        const edges = try gpa.alloc(std.ArrayList(u32), n);
        defer {
            for (edges) |*list| list.deinit(gpa);
            gpa.free(edges);
        }
        for (edges) |*list| list.* = .empty;
        var stack: std.ArrayList(Ast.ExprChild) = .empty;
        defer stack.deinit(gpa);
        for (self.bodies, 0..) |maybe_body, index| {
            const body = maybe_body orelse continue;
            stack.clearRetainingCapacity();
            try stack.append(gpa, .{ .expr = body });
            while (stack.pop()) |child| switch (child) {
                .stmt => |s| try Ast.appendStmtChildren(gpa, program, s, &stack),
                .expr => |e| {
                    switch (program.getExpr(e).data) {
                        .comptime_value => continue,
                        .call_proc => |call| if (Ast.localDirectCallee(call)) |callee| {
                            if (@backingInt(callee) < n) try edges[index].append(gpa, @backingInt(callee));
                        },
                        .fn_ref => |r| if (@backingInt(r.fn_id) < n) try edges[index].append(gpa, @backingInt(r.fn_id)),
                        .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
                    }
                    try Ast.appendChildren(gpa, program, e, &stack);
                },
            };
        }
        const order = try gpa.alloc(u32, n);
        defer gpa.free(order);
        const low = try gpa.alloc(u32, n);
        defer gpa.free(low);
        const on_stack = try gpa.alloc(bool, n);
        defer gpa.free(on_stack);
        @memset(order, none);
        @memset(on_stack, false);
        @memset(self.on_cycle, false);
        const DfsFrame = struct { node: u32, next: u32 };
        var frames: std.ArrayList(DfsFrame) = .empty;
        defer frames.deinit(gpa);
        var component: std.ArrayList(u32) = .empty;
        defer component.deinit(gpa);
        var next_order: u32 = 0;
        var next_scc: u32 = 0;
        for (0..n) |root_index| {
            const root: u32 = @intCast(root_index);
            if (order[root] != none) continue;
            order[root] = next_order;
            low[root] = next_order;
            next_order += 1;
            try component.append(gpa, root);
            on_stack[root] = true;
            try frames.append(gpa, .{ .node = root, .next = 0 });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                if (frame.next < edges[node].items.len) {
                    const target = edges[node].items[frame.next];
                    frame.next += 1;
                    if (order[target] == none) {
                        order[target] = next_order;
                        low[target] = next_order;
                        next_order += 1;
                        try component.append(gpa, target);
                        on_stack[target] = true;
                        try frames.append(gpa, .{ .node = target, .next = 0 });
                    } else if (on_stack[target]) {
                        low[node] = @min(low[node], order[target]);
                    }
                    continue;
                }
                _ = frames.pop();
                if (frames.items.len != 0) {
                    const parent = frames.items[frames.items.len - 1].node;
                    low[parent] = @min(low[parent], low[node]);
                }
                if (low[node] != order[node]) continue;
                const top = component.items.len;
                var start = top;
                while (true) {
                    start -= 1;
                    const member = component.items[start];
                    on_stack[member] = false;
                    self.scc[member] = next_scc;
                    if (member == node) break;
                }
                const members = component.items[start..top];
                if (members.len > 1) {
                    for (members) |member| self.on_cycle[member] = true;
                } else for (edges[node].items) |target| {
                    if (target == node) self.on_cycle[node] = true;
                }
                component.shrinkRetainingCapacity(start);
                next_scc += 1;
            }
        }
    }

    /// Decide which functions always inline as wrappers: a `WrapperBody`
    /// that lies on no call cycle, where inlining would only unroll the
    /// recursion, and that passes none of its direct calls a value it builds
    /// where the callee's extent consumes it, which would inline that callee
    /// into every copy of the body.
    pub fn classifyWrappers(self: *Index, session: *Session) Allocator.Error!void {
        const gpa = self.gpa;
        const program = session.program;
        var stack: std.ArrayList(Ast.ExprChild) = .empty;
        defer stack.deinit(gpa);
        for (self.bodies, 0..) |maybe_body, index| {
            const body = maybe_body orelse continue;
            const fn_id: Ast.FnId = @fromBackingInt(@as(u32, @intCast(index)));
            if (self.on_cycle[index]) continue;
            if (try WrapperBody.wrapperBody(gpa, program, fn_id) == null) continue;
            self.wrapper[index] = !try self.bodySpecializesCall(session, body, &stack);
        }
    }

    fn bodySpecializesCall(self: *const Index, session: *Session, body: Ast.ExprId, stack: *std.ArrayList(Ast.ExprChild)) Allocator.Error!bool {
        const gpa = self.gpa;
        const program = session.program;
        stack.clearRetainingCapacity();
        try stack.append(gpa, .{ .expr = body });
        while (stack.pop()) |child| switch (child) {
            .stmt => |s| try Ast.appendStmtChildren(gpa, program, s, stack),
            .expr => |e| {
                switch (program.getExpr(e).data) {
                    .comptime_value => continue,
                    .call_proc => |call| if (Ast.localDirectCallee(call)) |callee| {
                        const args = program.exprSpan(call.args);
                        for (0..args.len) |i| {
                            var built: BuiltOracle = .{ .session = session, .expr = GuardedList.at(args, i) };
                            if (try session.calleeConsumes(built.oracle(), callee, .{ .kind = .arg, .index = @intCast(i) })) return true;
                        }
                        const ops = program.captureOperandSpan(call.captures);
                        for (0..ops.len) |i| {
                            const op = GuardedList.at(ops, i);
                            var built: BuiltOracle = .{ .session = session, .expr = op.value };
                            if (try session.calleeConsumes(built.oracle(), callee, .{ .kind = .capture, .index = captureSlot(program, callee, op.id) })) return true;
                        }
                    },
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
                }
                try Ast.appendChildren(gpa, program, e, stack);
            },
        };
        return false;
    }
};

/// The capture slot of `fn_id` that receives capture operand `id`.
pub fn captureSlot(program: *const Ast.Program, fn_id: Ast.FnId, id: anytype) u32 {
    const slots = program.typedLocalSpan(program.getFn(fn_id).captures);
    for (0..slots.len) |slot| {
        if (program.captureIdOfLocal(GuardedList.at(slots, slot).local) == id) return @intCast(slot);
    }
    Common.invariant("capture operand named no capture slot of its target function");
}

/// What a function builds at one part of its result on every path that
/// returns normally: constructors serving `kinds`, or the part at `path` of
/// a parameter or capture it passes through.
pub const Built = struct {
    diverges: bool = false,
    kinds: Consumers = .{},
    passes: []const Pass = &.{},

    /// The part is the part at `path` of `slot`.
    pub const Pass = struct { slot: Slot, path: PathId };
};

/// Whether a query's seed is known at a path well enough for one of a set of
/// consumers.
pub const Oracle = struct {
    ctx: *anyopaque,
    known: *const fn (ctx: *anyopaque, path: []const Step, kinds: Consumers) Allocator.Error!bool,
};

/// A seed that knows exactly the parts an expression builds on every path:
/// the constructors of the body that holds it, with nothing known about the
/// body's own parameters.
const BuiltOracle = struct {
    session: *Session,
    expr: Ast.ExprId,

    fn oracle(self: *BuiltOracle) Oracle {
        return .{ .ctx = self, .known = known };
    }

    fn known(ctx: *anyopaque, path: []const Step, kinds: Consumers) Allocator.Error!bool {
        const self: *BuiltOracle = @ptrCast(@alignCast(ctx));
        const built = try self.session.builtExprAt(self.expr, try self.session.pathOf(path));
        return built.kinds.intersectWith(kinds).count() != 0;
    }
};

/// The inline frames a query's continuation climbs: for each, the function
/// whose body it inlines and the call it inlines, innermost last. A frame
/// with no call is a root context, such as a worker's own body: its result
/// leaves the query.
pub const RootFrame = struct { fn_id: Ast.FnId, call_expr: ?Ast.ExprId };

/// Query state owned by one task. Its memo tables live as long as the task;
/// a query's walk state lives for one query.
pub const Session = struct {
    gpa: Allocator,
    index: *const Index,
    program: *const Ast.Program,
    paths: SeqTable(Step),
    suffix_memo: collections.DenseMapUnmanaged(PathId, PathId) = .empty,
    built_memo: std.AutoHashMapUnmanaged(BuiltKey, BuiltEntry) = .empty,
    /// Built passes, kept for the session's life.
    built_arena: collections.SingleThreadArena,
    walk: Walk,

    /// Start a session over `index` reading `program`.
    pub fn init(gpa: Allocator, index: *const Index, program: *const Ast.Program) Allocator.Error!Session {
        return .{
            .gpa = gpa,
            .index = index,
            .program = program,
            .paths = try SeqTable(Step).init(gpa),
            .built_arena = collections.SingleThreadArena.init(gpa),
            .walk = .{ .arena = collections.SingleThreadArena.init(gpa) },
        };
    }

    /// Free the session.
    pub fn deinit(self: *Session) void {
        self.paths.deinit(self.gpa);
        self.suffix_memo.deinit(self.gpa);
        self.built_memo.deinit(self.gpa);
        self.built_arena.deinit();
        self.walk.arena.deinit();
    }

    /// The path of `steps`.
    pub fn pathOf(self: *Session, steps: []const Step) Allocator.Error!PathId {
        var id: PathId = .root;
        for (steps) |step| id = try self.paths.push(self.gpa, id, step);
        return id;
    }

    /// Replace `out` with the steps of `path`, root first.
    pub fn stepsOf(self: *const Session, allocator: Allocator, path: PathId, out: *std.ArrayList(Step)) Allocator.Error!void {
        try self.paths.items(allocator, path, out);
    }

    fn firstStep(self: *const Session, path: PathId) ?Step {
        if (path == .root) return null;
        var current = path;
        while (self.paths.parent(current) != .root) current = self.paths.parent(current);
        return self.paths.last(current);
    }

    /// `path` without its first step.
    fn suffix(self: *Session, path: PathId) Allocator.Error!PathId {
        if (path == .root) Common.invariant("the root path has no suffix");
        if (self.suffix_memo.get(path)) |cached| return cached;
        const parent = self.paths.parent(path);
        const result: PathId = if (parent == .root) .root else try self.paths.push(self.gpa, try self.suffix(parent), self.paths.last(path));
        try self.suffix_memo.put(self.gpa, path, result);
        return result;
    }

    const BuiltKey = struct { kind: enum(u8) { expr, result }, id: u32, path: PathId };
    const BuiltEntry = union(enum) { in_progress, done: Built };

    /// What `fn_id` builds at `path` of its result on every path that
    /// returns normally. A function whose result reaches itself at the same
    /// part builds nothing there: the least fixed point.
    pub fn builtAt(self: *Session, fn_id: Ast.FnId, path: PathId) Allocator.Error!Built {
        if (!self.index.isOriginal(fn_id)) return .{};
        return self.evalBuilt(.{ .kind = .result, .id = @backingInt(fn_id), .path = path });
    }

    /// What expression `expr` of a source body builds at `path` on every
    /// path that produces it.
    pub fn builtExprAt(self: *Session, expr: Ast.ExprId, path: PathId) Allocator.Error!Built {
        return self.evalBuilt(.{ .kind = .expr, .id = @backingInt(expr), .path = path });
    }

    /// Evaluate `root` and everything it needs on an explicit stack, so
    /// nesting and call chains never become native call depth. A node that
    /// needs an unfinished one is retried after it finishes; a node on the
    /// stack reads as building nothing.
    fn evalBuilt(self: *Session, root: BuiltKey) Allocator.Error!Built {
        if (self.built_memo.get(root)) |entry| return switch (entry) {
            .done => |built| built,
            .in_progress => .{},
        };
        var stack: std.ArrayList(BuiltKey) = .empty;
        defer stack.deinit(self.gpa);
        try stack.append(self.gpa, root);
        try self.built_memo.put(self.gpa, root, .in_progress);
        while (stack.getLastOrNull()) |key| {
            var missing: ?BuiltKey = null;
            const result = try self.computeBuilt(key, &missing);
            if (missing) |needed| {
                try self.built_memo.put(self.gpa, needed, .in_progress);
                try stack.append(self.gpa, needed);
                continue;
            }
            try self.built_memo.put(self.gpa, key, .{ .done = result });
            _ = stack.pop();
        }
        return self.built_memo.get(root).?.done;
    }

    fn child(self: *Session, key: BuiltKey, missing: *?BuiltKey) ?Built {
        if (self.built_memo.get(key)) |entry| return switch (entry) {
            .done => |built| built,
            .in_progress => Built{},
        };
        if (missing.* == null) missing.* = key;
        return null;
    }

    fn meet(self: *Session, a: Built, b: Built) Allocator.Error!Built {
        if (a.diverges) return b;
        if (b.diverges) return a;
        var passes: std.ArrayList(Built.Pass) = .empty;
        for (a.passes) |x| for (b.passes) |y| {
            if (x.slot.kind == y.slot.kind and x.slot.index == y.slot.index and x.path == y.path) try passes.append(self.built_arena.allocator(), x);
        };
        return .{ .kinds = a.kinds.intersectWith(b.kinds), .passes = passes.items };
    }

    fn exprKey(expr: Ast.ExprId, path: PathId) BuiltKey {
        return .{ .kind = .expr, .id = @backingInt(expr), .path = path };
    }

    /// Compute `key` from finished children, or name a missing child.
    fn computeBuilt(self: *Session, key: BuiltKey, missing: *?BuiltKey) Allocator.Error!Built {
        const p = self.program;
        const path = key.path;
        switch (key.kind) {
            .result => {
                const body = self.index.bodies[key.id] orelse return .{};
                var result = self.child(exprKey(body, path), missing) orelse return .{};
                for (self.index.returns[key.id].items) |ret| {
                    const value = self.child(exprKey(ret, path), missing) orelse return .{};
                    result = try self.meet(result, value);
                }
                // A function that never returns normally builds nothing a
                // caller can consume.
                return if (result.diverges) .{} else result;
            },
            .expr => {},
        }
        const e: Ast.ExprId = @fromBackingInt(key.id);
        const first = self.firstStep(path);
        switch (p.getExpr(e).data) {
            .typed_boundary => |b| return self.child(exprKey(b.value, path), missing) orelse .{},
            .row_widen => |w| return self.child(exprKey(w.value, path), missing) orelse .{},
            .nominal => |b| return self.child(exprKey(b, path), missing) orelse .{},
            .static_data_candidate => |c| switch (c.value) {
                .runtime => |r| return self.child(exprKey(r, path), missing) orelse .{},
                // A stored candidate is an opaque leaf to the clone.
                .stored => return .{},
            },
            .local => |local| {
                if (self.index.defs.get(local)) |maybe_def| {
                    const def = maybe_def orelse return .{};
                    return self.child(exprKey(def, path), missing) orelse .{};
                }
                const slot = self.index.slots.get(local) orelse return .{};
                const passes = try self.built_arena.allocator().alloc(Built.Pass, 1);
                passes[0] = .{ .slot = slot, .path = path };
                return .{ .passes = passes };
            },
            .let_ => |let_| return self.child(exprKey(let_.rest, path), missing) orelse .{},
            .block => |block| return self.child(exprKey(block.final_expr, path), missing) orelse .{},
            .if_ => |if_| {
                var result = self.child(exprKey(if_.final_else, path), missing) orelse return .{};
                const bs = p.ifBranchSpan(if_.branches);
                for (0..bs.len) |i| {
                    const value = self.child(exprKey(GuardedList.at(bs, i).body, path), missing) orelse return .{};
                    result = try self.meet(result, value);
                }
                return result;
            },
            .match_ => |m| {
                var result: Built = .{ .diverges = true };
                const bs = p.branchSpan(m.branches);
                for (0..bs.len) |i| {
                    const value = self.child(exprKey(GuardedList.at(bs, i).body, path), missing) orelse return .{};
                    result = try self.meet(result, value);
                }
                return result;
            },
            .join_point => |j| {
                const body = self.child(exprKey(j.body, path), missing) orelse return .{};
                const remainder = self.child(exprKey(j.remainder, path), missing) orelse return .{};
                return self.meet(body, remainder);
            },
            .tag => |tag| {
                const step = first orelse return .{ .kinds = .initMany(&.{ .match, .loop }) };
                const ps = p.exprSpan(tag.payloads);
                for (0..ps.len) |i| {
                    if (step.isPayload(tag.name, @intCast(i))) return self.child(exprKey(GuardedList.at(ps, i), try self.suffix(path)), missing) orelse .{};
                }
                return .{};
            },
            .record => |span| {
                const step = first orelse return .{ .kinds = .initMany(&.{ .loop, .project }) };
                const fs = p.fieldExprSpan(span);
                for (0..fs.len) |i| {
                    const fe = GuardedList.at(fs, i);
                    if (step.isField(fe.name)) return self.child(exprKey(fe.value, try self.suffix(path)), missing) orelse .{};
                }
                return .{};
            },
            .record_update => |u| {
                const step = first orelse return .{ .kinds = .initMany(&.{ .loop, .project }) };
                const fs = p.fieldExprSpan(u.fields);
                for (0..fs.len) |i| {
                    const fe = GuardedList.at(fs, i);
                    if (step.isField(fe.name)) return self.child(exprKey(fe.value, try self.suffix(path)), missing) orelse .{};
                }
                // The base's own parts are what the base builds, which this
                // summary does not see through.
                return .{};
            },
            .tuple => |span| {
                const step = first orelse return .{ .kinds = .initMany(&.{ .loop, .project }) };
                const is = p.exprSpan(span);
                for (0..is.len) |i| {
                    if (step.isItem(@intCast(i))) return self.child(exprKey(GuardedList.at(is, i), try self.suffix(path)), missing) orelse .{};
                }
                return .{};
            },
            .fn_ref => return if (first == null) .{ .kinds = .initOne(.call) } else .{},
            .call_proc => |call| {
                const callee = Ast.localDirectCallee(call) orelse return .{};
                if (!self.index.isOriginal(callee)) return .{};
                const inner = self.child(.{ .kind = .result, .id = @backingInt(callee), .path = path }, missing) orelse return .{};
                var result: Built = .{ .kinds = inner.kinds };
                var passes: std.ArrayList(Built.Pass) = .empty;
                const as = p.exprSpan(call.args);
                const ops = p.captureOperandSpan(call.captures);
                for (inner.passes) |pass| {
                    const source = switch (pass.slot.kind) {
                        .arg => GuardedList.at(as, pass.slot.index),
                        .capture => for (0..ops.len) |i| {
                            const op = GuardedList.at(ops, i);
                            if (captureSlot(p, callee, op.id) == pass.slot.index) break op.value;
                        } else Common.invariant("direct call supplied no operand for a capture its callee passes through"),
                    };
                    const value = self.child(exprKey(source, pass.path), missing) orelse return .{};
                    result.kinds.setUnion(value.kinds);
                    try passes.appendSlice(self.built_arena.allocator(), value.passes);
                }
                result.passes = passes.items;
                return result;
            },
            .@"unreachable", .return_, .jump, .break_, .continue_, .crash, .checked_error, .comptime_exhaustiveness_failed, .literal_rejected, .expect_err => return .{ .diverges = true },
            .field_access, .tuple_access, .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .list, .call_value, .low_level, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .dbg, .expect => return .{},
            .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached inline crossing"),
        }
    }

    /// Whether a known part of the value passed at `slot` meets an argument
    /// consumer inside `callee`'s extent.
    pub fn calleeConsumes(self: *Session, oracle: Oracle, callee: Ast.FnId, slot: Slot) Allocator.Error!bool {
        if (!try oracle.known(oracle.ctx, &.{}, argument_consumers)) return false;
        try self.walk.begin(self, oracle, argument_consumers, callee, &.{});
        defer self.walk.end();
        try self.walk.enterCall(null, callee, slot, .root, 0);
        return self.walk.run();
    }

    /// Whether inlining the direct call `call_expr` to `callee` lets a known
    /// part of the value passed at `slot` meet an argument consumer: inside
    /// the callee's extent, or through its result in the caller's
    /// continuation up through `frames`, the inline frames the call sits in,
    /// innermost last, above the body of `root_fn`.
    pub fn argumentCrosses(self: *Session, oracle: Oracle, root_fn: ?Ast.FnId, frames: []const RootFrame, call_expr: Ast.ExprId, callee: Ast.FnId, slot: Slot) Allocator.Error!bool {
        if (!try oracle.known(oracle.ctx, &.{}, argument_consumers)) return false;
        try self.walk.begin(self, oracle, argument_consumers, root_fn, frames);
        defer self.walk.end();
        const root = try self.walk.rootEntry(frames.len);
        if (std.debug.runtime_safety and !self.index.bodyHolds(self.walk.entries.items[root].fn_id, call_expr)) {
            Common.invariant("a crossing query's call lies outside the body its inline frames name");
        }
        try self.walk.enterCall(.{ .entry = root, .call = call_expr }, callee, slot, .root, 0);
        return self.walk.run();
    }

    /// Whether a part of `call_expr`'s result that the oracle knows meets a
    /// result consumer in the caller's continuation up through `frames`, as
    /// for `argumentCrosses`.
    pub fn resultCrosses(self: *Session, oracle: Oracle, root_fn: ?Ast.FnId, frames: []const RootFrame, call_expr: Ast.ExprId) Allocator.Error!bool {
        if (!try oracle.known(oracle.ctx, &.{}, .initMany(&.{ .match, .loop, .call }))) return false;
        try self.walk.begin(self, oracle, result_consumers, root_fn, frames);
        defer self.walk.end();
        const root = try self.walk.rootEntry(frames.len);
        if (std.debug.runtime_safety and !self.index.bodyHolds(self.walk.entries.items[root].fn_id, call_expr)) {
            Common.invariant("a crossing query's call lies outside the body its inline frames name");
        }
        try self.walk.addConfig(root, .{ .kind = .expr, .id = @backingInt(call_expr), .aux = 0 }, .root, 0);
        return self.walk.run();
    }
};

/// Interned sequences; id 0 is the empty sequence.
fn SeqTable(comptime T: type) type {
    return struct {
        const Self = @This();
        const Node = struct { parent: PathId, elem: T, len: u32 };
        const Key = struct { parent: PathId, elem: T };
        nodes: std.ArrayList(Node) = .empty,
        map: std.AutoHashMapUnmanaged(Key, PathId) = .empty,

        fn init(gpa: Allocator) Allocator.Error!Self {
            var self: Self = .{};
            // The empty sequence has no element; it is never read.
            try self.nodes.append(gpa, .{ .parent = .root, .elem = undefined, .len = 0 });
            return self;
        }

        fn deinit(self: *Self, gpa: Allocator) void {
            self.nodes.deinit(gpa);
            self.map.deinit(gpa);
        }

        fn push(self: *Self, gpa: Allocator, parent_id: PathId, elem: T) Allocator.Error!PathId {
            const gop = try self.map.getOrPut(gpa, .{ .parent = parent_id, .elem = elem });
            if (!gop.found_existing) {
                gop.value_ptr.* = @fromBackingInt(@as(u32, @intCast(self.nodes.items.len)));
                try self.nodes.append(gpa, .{ .parent = parent_id, .elem = elem, .len = self.nodes.items[@backingInt(parent_id)].len + 1 });
            }
            return gop.value_ptr.*;
        }

        fn last(self: *const Self, id: PathId) T {
            return self.nodes.items[@backingInt(id)].elem;
        }

        fn parent(self: *const Self, id: PathId) PathId {
            return self.nodes.items[@backingInt(id)].parent;
        }

        fn items(self: *const Self, allocator: Allocator, id: PathId, out: *std.ArrayList(T)) Allocator.Error!void {
            const n = self.nodes.items[@backingInt(id)].len;
            try out.resize(allocator, n);
            var current = id;
            var i = n;
            while (i > 0) {
                i -= 1;
                out.items[i] = self.nodes.items[@backingInt(current)].elem;
                current = self.nodes.items[@backingInt(current)].parent;
            }
        }
    };
}

/// A wrap frame: a constructor step the walk has put around the known part,
/// or a record update whose replaced fields no longer come from it.
const Frame = union(enum) {
    step: Step,
    update: Ast.ExprId,
};

/// Where a walk stands in a body.
const Item = struct {
    kind: Kind,
    id: u32,
    /// For `access`, the number of the field access's segments already read.
    aux: u32,

    const Kind = enum(u8) { expr, local, pat, access };

    fn expr(id: Ast.ExprId) Item {
        return .{ .kind = .expr, .id = @backingInt(id), .aux = 0 };
    }

    fn local(id: Ast.LocalId) Item {
        return .{ .kind = .local, .id = @backingInt(id), .aux = 0 };
    }

    fn pat(id: Ast.PatId) Item {
        return .{ .kind = .pat, .id = @backingInt(id), .aux = 0 };
    }
};

/// The forward walk of one query: a pushdown reachability problem whose
/// stack is the wrap frames around the known part, solved by saturation.
///
/// A stack is never stored. A frame pushed at a constructor is a node keyed
/// by the push site, the frame, and the seed path; node 0 is the empty
/// stack. Each node keeps the set of nodes that can lie beneath it, so a
/// configuration (body position, seed path, top node) stands for every stack
/// whose top is that node. A pop resumes every node beneath, now and as
/// beneath-sets grow. Calls are summarized per entry (callee, slot, seed
/// path, top node): every caller of an entry receives each of its exits at
/// its own call site, so a walk returns where it was called from.
const Walk = struct {
    arena: collections.SingleThreadArena,
    session: *Session = undefined,
    oracle: Oracle = undefined,
    counted: Consumers = .{},
    frames: []const RootFrame = &.{},
    /// The body beneath every inline frame, when the walk may climb that far.
    root_fn: ?Ast.FnId = null,
    hit: bool = false,

    nodes: std.ArrayList(Node) = .empty,
    node_map: std.AutoHashMapUnmanaged(NodeKey, u32) = .empty,
    below_set: std.AutoHashMapUnmanaged([2]u32, void) = .empty,
    escaped: std.AutoHashMapUnmanaged(u32, void) = .empty,
    watched: std.AutoHashMapUnmanaged(WatchKey, void) = .empty,
    entries: std.ArrayList(Entry) = .empty,
    entry_map: std.AutoHashMapUnmanaged(EntryKey, u32) = .empty,
    exit_set: std.AutoHashMapUnmanaged(ExitKey, void) = .empty,
    seen: std.AutoHashMapUnmanaged(Config, void) = .empty,
    work: std.ArrayList(Config) = .empty,
    applies: std.ArrayList(Apply) = .empty,

    const Node = struct {
        frame: Frame,
        site: Ast.ExprId,
        path: PathId,
        /// The body that pushed the frame, and the innermost loop around
        /// the push there.
        body: Ast.FnId,
        loop: u32,
        below: std.ArrayList(u32) = .empty,
        watchers: std.ArrayList(Watcher) = .empty,
    };

    const NodeKey = struct { body: Ast.FnId, site: Ast.ExprId, frame: Frame, path: PathId };

    const Caller = struct { entry: u32, call: Ast.ExprId };

    const Entry = struct {
        fn_id: Ast.FnId,
        /// For a root entry, the number of inline frames beneath its body.
        depth: u32,
        is_root: bool,
        callers: std.ArrayList(Caller) = .empty,
        exits: std.ArrayList(Exit) = .empty,
    };

    const EntryKey = struct { fn_id: Ast.FnId, slot_kind: Slot.Kind, slot: u32, path: PathId, node: u32, root_depth: u32 };

    const Exit = struct { path: PathId, node: u32 };

    const ExitKey = struct { entry: u32, path: PathId, node: u32 };

    const Config = struct { entry: u32, item: Item, path: PathId, node: u32 };

    const Watcher = union(enum) {
        /// The node's frame was popped: resume beneath it at `target`.
        pop: struct { entry: u32, target: Item },
        /// A field read passes through the node's record update frame: read
        /// the field of what lies beneath.
        descend: struct { entry: u32, target: Item, step: Step },
        /// The node's capture frame is called: enter the capture slot with
        /// what lies beneath, returning to `caller`.
        call_capture: struct { caller: ?Caller, target: Ast.FnId, slot: u32 },
        /// The node reached a function's result: any caller may call a
        /// function value wrapped around the known part.
        escape,
    };

    const Apply = struct { watcher: Watcher, node: u32, beneath: u32 };

    const WatchKey = struct { node: u32, watcher: Watcher };

    fn alloc(self: *Walk) Allocator {
        return self.arena.allocator();
    }

    fn begin(self: *Walk, session: *Session, oracle: Oracle, counted: Consumers, root_fn: ?Ast.FnId, frames: []const RootFrame) Allocator.Error!void {
        self.session = session;
        self.root_fn = root_fn;
        self.oracle = oracle;
        self.counted = counted;
        self.frames = frames;
        self.hit = false;
        self.nodes = .empty;
        self.node_map = .empty;
        self.below_set = .empty;
        self.escaped = .empty;
        self.watched = .empty;
        self.entries = .empty;
        self.entry_map = .empty;
        self.exit_set = .empty;
        self.seen = .empty;
        self.work = .empty;
        self.applies = .empty;
        // Node 0, the empty stack, has no frame; its fields are never read.
        try self.nodes.append(self.alloc(), .{ .frame = undefined, .site = undefined, .path = .root, .body = undefined, .loop = none });
    }

    fn end(self: *Walk) void {
        _ = self.arena.reset(.retain_capacity);
    }

    fn known(self: *Walk, path: PathId, kinds: Consumers) Allocator.Error!bool {
        var steps: std.ArrayList(Step) = .empty;
        try self.session.stepsOf(self.alloc(), path, &steps);
        return self.oracle.known(self.oracle.ctx, steps.items, kinds);
    }

    fn consume(self: *Walk, config: Config, kind: Consumer) Allocator.Error!void {
        if (config.node != 0) return;
        if (!self.counted.contains(kind)) return;
        if (try self.known(config.path, .initOne(kind))) self.hit = true;
    }

    fn addConfig(self: *Walk, entry: u32, item: Item, path: PathId, node: u32) Allocator.Error!void {
        const config: Config = .{ .entry = entry, .item = item, .path = path, .node = node };
        if ((try self.seen.getOrPut(self.alloc(), config)).found_existing) return;
        try self.work.append(self.alloc(), config);
    }

    /// The root entry for the body `depth` inline frames deep: the body
    /// beneath every frame is `root_fn`'s.
    fn rootEntry(self: *Walk, depth: usize) Allocator.Error!u32 {
        const fn_id = if (depth == 0)
            self.root_fn orelse Common.invariant("a crossing walk climbed beneath its inline frames without a root body")
        else
            self.frames[depth - 1].fn_id;
        const key: EntryKey = .{ .fn_id = fn_id, .slot_kind = .arg, .slot = none, .path = .root, .node = 0, .root_depth = @intCast(depth) };
        const gop = try self.entry_map.getOrPut(self.alloc(), key);
        if (!gop.found_existing) {
            gop.value_ptr.* = @intCast(self.entries.items.len);
            try self.entries.append(self.alloc(), .{ .fn_id = fn_id, .depth = @intCast(depth), .is_root = true });
        }
        return gop.value_ptr.*;
    }

    fn pushNode(self: *Walk, body: Ast.FnId, loop: u32, site: Ast.ExprId, frame: Frame, path: PathId, beneath: u32) Allocator.Error!u32 {
        const key: NodeKey = .{ .body = body, .site = site, .frame = frame, .path = path };
        const gop = try self.node_map.getOrPut(self.alloc(), key);
        if (!gop.found_existing) {
            gop.value_ptr.* = @intCast(self.nodes.items.len);
            try self.nodes.append(self.alloc(), .{ .frame = frame, .site = site, .path = path, .body = body, .loop = loop });
        }
        const node = gop.value_ptr.*;
        if (!(try self.below_set.getOrPut(self.alloc(), .{ node, beneath })).found_existing) {
            try self.nodes.items[node].below.append(self.alloc(), beneath);
            for (self.nodes.items[node].watchers.items) |watcher| {
                try self.applies.append(self.alloc(), .{ .watcher = watcher, .node = node, .beneath = beneath });
            }
        }
        return node;
    }

    fn watch(self: *Walk, node: u32, watcher: Watcher) Allocator.Error!void {
        if ((try self.watched.getOrPut(self.alloc(), .{ .node = node, .watcher = watcher })).found_existing) return;
        try self.nodes.items[node].watchers.append(self.alloc(), watcher);
        for (self.nodes.items[node].below.items) |beneath| {
            try self.applies.append(self.alloc(), .{ .watcher = watcher, .node = node, .beneath = beneath });
        }
    }

    /// Enter `callee` at `slot` with the known part at `path` under `node`.
    /// A caller, when there is one, receives the callee's exits.
    fn enterCall(self: *Walk, caller: ?Caller, callee: Ast.FnId, slot: Slot, path: PathId, node: u32) Allocator.Error!void {
        const index = self.session.index;
        if (!index.isOriginal(callee)) return;
        if (index.bodies[@backingInt(callee)] == null) return;
        if (caller) |c| {
            // Within a call cycle, a part wrapped in a frame the cycle
            // itself pushed grows on every round, and the clone never
            // inlines a recursive call whose argument grows.
            if (node != 0 and index.sameScc(self.entries.items[c.entry].fn_id, callee) and
                index.sameScc(self.nodes.items[node].body, callee)) return;
        }
        const key: EntryKey = .{ .fn_id = callee, .slot_kind = slot.kind, .slot = slot.index, .path = path, .node = node, .root_depth = none };
        const gop = try self.entry_map.getOrPut(self.alloc(), key);
        const entry: u32 = if (gop.found_existing) gop.value_ptr.* else blk: {
            const id: u32 = @intCast(self.entries.items.len);
            gop.value_ptr.* = id;
            try self.entries.append(self.alloc(), .{ .fn_id = callee, .depth = none, .is_root = false });
            const f = self.session.program.getFn(callee);
            const local = switch (slot.kind) {
                .arg => GuardedList.at(self.session.program.typedLocalSpan(f.args), slot.index).local,
                .capture => GuardedList.at(self.session.program.typedLocalSpan(f.captures), slot.index).local,
            };
            try self.addConfig(id, Item.local(local), path, node);
            break :blk id;
        };
        const c = caller orelse return;
        for (self.entries.items[entry].callers.items) |existing| {
            if (existing.entry == c.entry and existing.call == c.call) return;
        }
        try self.entries.items[entry].callers.append(self.alloc(), c);
        for (self.entries.items[entry].exits.items) |exit| {
            try self.addConfig(c.entry, Item.expr(c.call), exit.path, exit.node);
        }
    }

    fn addExit(self: *Walk, entry: u32, path: PathId, node: u32) Allocator.Error!void {
        if ((try self.exit_set.getOrPut(self.alloc(), .{ .entry = entry, .path = path, .node = node })).found_existing) return;
        const e = &self.entries.items[entry];
        try e.exits.append(self.alloc(), .{ .path = path, .node = node });
        if (e.is_root) {
            // The result of a root body is the result of the innermost
            // inline frame beneath it, read at that frame's call.
            if (e.depth == 0) return;
            const call = self.frames[e.depth - 1].call_expr orelse return;
            const next = try self.rootEntry(e.depth - 1);
            try self.addConfig(next, Item.expr(call), path, node);
            return;
        }
        for (e.callers.items) |caller| try self.addConfig(caller.entry, Item.expr(caller.call), path, node);
    }

    /// Read the part at `step` of the value at hand.
    fn descend(self: *Walk, entry: u32, target: Item, path: PathId, node: u32, part: Step) Allocator.Error!void {
        if (node == 0) {
            const next = try self.session.paths.push(self.session.gpa, path, part);
            // A known part has known ancestors, so a walk never follows a
            // part the seed does not know.
            if (!try self.known(next, .initMany(&.{ .match, .loop, .call }))) return;
            try self.addConfig(entry, target, next, 0);
            return;
        }
        switch (self.nodes.items[node].frame) {
            .step => |top| if (top.eql(part)) try self.watch(node, .{ .pop = .{ .entry = entry, .target = target } }),
            .update => |update| {
                if (part.kind == .field) {
                    const fs = self.session.program.fieldExprSpan(self.session.program.getExpr(update).data.record_update.fields);
                    for (0..fs.len) |i| {
                        if (part.label == @backingInt(GuardedList.at(fs, i).name)) return;
                    }
                }
                try self.watch(node, .{ .descend = .{ .entry = entry, .target = target, .step = part } });
            },
        }
    }

    fn escape(self: *Walk, node: u32) Allocator.Error!void {
        if (node == 0) return;
        if ((try self.escaped.getOrPut(self.alloc(), node)).found_existing) return;
        switch (self.nodes.items[node].frame) {
            .step => |top| if (top.kind == .capture) {
                return self.watch(node, .{ .call_capture = .{ .caller = null, .target = @fromBackingInt(top.label), .slot = top.index } });
            },
            .update => {},
        }
        try self.watch(node, .escape);
    }

    fn apply(self: *Walk, a: Apply) Allocator.Error!void {
        const path = self.nodes.items[a.node].path;
        switch (a.watcher) {
            .pop => |w| try self.addConfig(w.entry, w.target, path, a.beneath),
            .descend => |w| try self.descend(w.entry, w.target, path, a.beneath, w.step),
            .call_capture => |w| try self.enterCall(w.caller, w.target, .{ .kind = .capture, .index = w.slot }, path, a.beneath),
            .escape => try self.escape(a.beneath),
        }
    }

    fn run(self: *Walk) Allocator.Error!bool {
        while (!self.hit) {
            if (self.applies.pop()) |a| {
                try self.apply(a);
                continue;
            }
            const config = self.work.pop() orelse break;
            try self.step(config);
        }
        return self.hit;
    }

    fn step(self: *Walk, c: Config) Allocator.Error!void {
        const p = self.session.program;
        const index = self.session.index;
        switch (c.item.kind) {
            .local => {
                const body = @backingInt(self.entries.items[c.entry].fn_id);
                var row = index.local_use_head.get(@fromBackingInt(c.item.id)) orelse return;
                while (row != none) : (row = index.local_use_rows.items[row].next) {
                    const use = index.local_use_rows.items[row];
                    if (use.body == body) try self.addConfig(c.entry, Item.expr(use.expr), c.path, c.node);
                }
            },
            .pat => try self.stepPattern(c),
            .access => {
                const e: Ast.ExprId = @fromBackingInt(c.item.id);
                const access = p.getExpr(e).data.field_access;
                const segs = p.fieldAccessSegmentSpan(access.segments);
                if (c.item.aux == segs.len) return self.addConfig(c.entry, Item.expr(e), c.path, c.node);
                try self.consume(c, .project);
                const next: Item = .{ .kind = .access, .id = c.item.id, .aux = c.item.aux + 1 };
                try self.descend(c.entry, next, c.path, c.node, Step.field(GuardedList.at(segs, c.item.aux).field));
            },
            .expr => {
                const raw = c.item.id;
                if (raw >= index.use_head.len) Common.invariant("inline crossing walked an expression created after indexing");
                const body = @backingInt(self.entries.items[c.entry].fn_id);
                var row = index.use_head[raw];
                while (row != none) : (row = index.uses.items[row].next) {
                    const use = index.uses.items[row];
                    if (use.body == body) try self.stepUse(c, use.use);
                }
            },
        }
    }

    fn stepUse(self: *Walk, c: Config, use: Use) Allocator.Error!void {
        const p = self.session.program;
        switch (use) {
            .stop => {},
            .same => |parent| try self.addConfig(c.entry, Item.expr(parent), c.path, c.node),
            .receiver => |parent| switch (p.getExpr(parent).data) {
                .field_access => try self.addConfig(c.entry, .{ .kind = .access, .id = @backingInt(parent), .aux = 0 }, c.path, c.node),
                .tuple_access => |a| {
                    try self.consume(c, .project);
                    try self.descend(c.entry, Item.expr(parent), c.path, c.node, Step.item(a.elem_index));
                },
                .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected, .comptime_value => Common.invariant("a receiver use named a parent that reads no part"),
            },
            .operand => |o| {
                const node = try self.pushNode(self.entries.items[c.entry].fn_id, o.loop, o.parent, .{ .step = o.step }, c.path, c.node);
                try self.addConfig(c.entry, Item.expr(o.parent), c.path, node);
            },
            .update_base => |u| {
                const node = try self.pushNode(self.entries.items[c.entry].fn_id, u.loop, u.parent, .{ .update = u.parent }, c.path, c.node);
                try self.addConfig(c.entry, Item.expr(u.parent), c.path, node);
            },
            .pattern => |pat_id| try self.addConfig(c.entry, Item.pat(pat_id), c.path, c.node),
            // A recursive binding re-enters its own value: only an
            // unwrapped part can do so without growing.
            .rec_pattern => |pat_id| if (c.node == 0) try self.addConfig(c.entry, Item.pat(pat_id), c.path, c.node),
            .scrutinee => |m| {
                try self.consume(c, .match);
                const bs = p.branchSpan(p.getExpr(m).data.match_.branches);
                for (0..bs.len) |i| try self.addConfig(c.entry, Item.pat(GuardedList.at(bs, i).pat), c.path, c.node);
            },
            .callee => |call| {
                try self.consume(c, .call);
                // Calling a function value built around the known part
                // enters its body at the capture.
                if (c.node != 0) switch (self.nodes.items[c.node].frame) {
                    .step => |top| if (top.kind == .capture) try self.watch(c.node, .{ .call_capture = .{
                        .caller = .{ .entry = c.entry, .call = call },
                        .target = @fromBackingInt(top.label),
                        .slot = top.index,
                    } }),
                    .update => {},
                };
            },
            .proc_arg => |pa| try self.enterCall(.{ .entry = c.entry, .call = pa.call }, pa.callee, pa.slot, c.path, c.node),
            .loop_init => |li| {
                try self.consume(c, .loop);
                try self.addConfig(c.entry, Item.local(li.local), c.path, c.node);
            },
            .into_local => |il| {
                // A back edge stops a part wrapped in a frame pushed inside
                // the loop it re-enters: such a structure grows on every
                // pass and is no loop fixed point.
                if (il.loop) |loop| if (c.node != 0) {
                    const node = self.nodes.items[c.node];
                    const body = self.entries.items[c.entry].fn_id;
                    if (node.body == body and self.session.index.loopWithin(body, node.loop, loop)) return;
                };
                try self.addConfig(c.entry, Item.local(il.local), c.path, c.node);
            },
            .result => {
                try self.addExit(c.entry, c.path, c.node);
                try self.escape(c.node);
            },
        }
    }

    fn stepPattern(self: *Walk, c: Config) Allocator.Error!void {
        const p = self.session.program;
        switch (p.getPat(@fromBackingInt(c.item.id)).data) {
            .bind => |local| try self.addConfig(c.entry, Item.local(local), c.path, c.node),
            .as => |as_| {
                try self.addConfig(c.entry, Item.local(as_.local), c.path, c.node);
                try self.addConfig(c.entry, Item.pat(as_.pattern), c.path, c.node);
            },
            .nominal => |inner| try self.addConfig(c.entry, Item.pat(inner), c.path, c.node),
            .tag => |tag| {
                try self.consume(c, .match);
                const ps = p.patSpan(tag.payloads);
                for (0..ps.len) |i| try self.descend(c.entry, Item.pat(GuardedList.at(ps, i)), c.path, c.node, Step.payload(tag.name, @intCast(i)));
            },
            .record => |span| {
                try self.consume(c, .project);
                const ds = p.recordDestructSpan(span);
                for (0..ds.len) |i| {
                    const d = GuardedList.at(ds, i);
                    try self.descend(c.entry, Item.pat(d.pattern), c.path, c.node, Step.field(d.name));
                }
            },
            .tuple => |span| {
                try self.consume(c, .project);
                const ps = p.patSpan(span);
                for (0..ps.len) |i| try self.descend(c.entry, Item.pat(GuardedList.at(ps, i)), c.path, c.node, Step.item(@intCast(i)));
            },
            // A list, literal, or string pattern reads no part a walk can
            // know.
            .wildcard, .list, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => {},
        }
    }
};

const Type = @import("../monotype/type.zig");
const checked = @import("check").CheckedModule;

/// Builds small lifted programs whose every value has one zero-sized type:
/// the walk reads only expression shape and labels, never types.
const TestProgram = struct {
    program: Ast.Program,
    ty: Type.TypeId,
    next_symbol: u32 = 1,
    next_binder: u32 = 1,

    fn init(gpa: Allocator) Allocator.Error!TestProgram {
        var program = Ast.Program.init(
            gpa,
            names.NameStore.init(gpa),
            Type.Store.init(gpa),
            .empty, // const_fn_evidence
            .empty, // const_fn_evidence_targets
            .empty, // const_fn_evidence_structurals
            .empty, // const_fn_evidence_frames
            .empty, // exprs
            .empty, // pats
            .empty, // stmts
            .empty, // locals
            .empty, // expr_ids
            .empty, // pat_ids
            .empty, // typed_locals
            .empty, // stmt_ids
            .empty, // field_exprs
            .empty, // field_access_segments
            .empty, // fn_def_captures
            .empty, // capture_operands
            .empty, // record_destructs
            .empty, // str_pattern_steps
            .empty, // branches
            .empty, // if_branches
            .empty, // string_literals
            Mono.ProcDebugNameMap.init(gpa),
            .empty, // source_files
            .empty, // expr_locs
            .empty, // expr_regions
            .empty, // stmt_locs
            .empty, // stmt_regions
            .empty, // inline_scopes
            .empty, // expr_inline_scopes
            .empty, // stmt_inline_scopes
            .empty, // local_names
            .empty, // static_data_values
            .empty, // comptime_sites
            0, // next_symbol
        );
        const ty = try program.types.add(.zst);
        return .{ .program = program, .ty = ty };
    }

    fn deinit(self: *TestProgram) void {
        self.program.deinit();
    }

    fn symbol(self: *TestProgram) Common.Symbol {
        self.next_symbol += 1;
        return @fromBackingInt(self.next_symbol);
    }

    fn tagName(self: *TestProgram, text: []const u8) Allocator.Error!names.TagNameId {
        return self.program.names.internTagLabel(text);
    }

    fn fieldName(self: *TestProgram, text: []const u8) Allocator.Error!names.RecordFieldNameId {
        return self.program.names.internRecordFieldLabel(text);
    }

    fn local(self: *TestProgram) Allocator.Error!Ast.LocalId {
        return self.program.addLocal(self.symbol(), self.ty);
    }

    fn captureLocal(self: *TestProgram) Allocator.Error!Ast.LocalId {
        self.next_binder += 1;
        const binder: checked.PatternBinderId = @fromBackingInt(self.next_binder);
        const key = checked.CaptureId.fromBinder(binder);
        return self.program.addLocalWithCaptureIdentity(self.symbol(), self.ty, binder, key, key, .lexical);
    }

    fn expr(self: *TestProgram, data: Ast.ExprData) Allocator.Error!Ast.ExprId {
        return self.program.addExpr(.{ .ty = self.ty, .data = data });
    }

    fn ref(self: *TestProgram, l: Ast.LocalId) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .local = l });
    }

    fn unit(self: *TestProgram) Allocator.Error!Ast.ExprId {
        return self.expr(.unit);
    }

    fn tag(self: *TestProgram, name: []const u8, payloads: []const Ast.ExprId) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .tag = .{ .name = try self.tagName(name), .payloads = try self.program.addExprSpan(payloads) } });
    }

    fn record(self: *TestProgram, field: []const u8, value: Ast.ExprId) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .record = try self.program.addFieldExprSpan(&.{.{ .name = try self.fieldName(field), .value = value }}) });
    }

    fn update(self: *TestProgram, base: Ast.ExprId, field: []const u8, value: Ast.ExprId) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .record_update = .{
            .base = base,
            .fields = try self.program.addFieldExprSpan(&.{.{ .name = try self.fieldName(field), .value = value }}),
        } });
    }

    fn access(self: *TestProgram, receiver: Ast.ExprId, field: []const u8) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .field_access = .{
            .receiver = receiver,
            .segments = try self.program.addFieldAccessSegmentSpan(&.{.{ .field = try self.fieldName(field) }}),
        } });
    }

    fn call(self: *TestProgram, callee: Ast.FnId, args: []const Ast.ExprId) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .call_proc = .{ .callee = .{ .lifted = callee }, .args = try self.program.addExprSpan(args) } });
    }

    fn bind(self: *TestProgram, l: Ast.LocalId) Allocator.Error!Ast.PatId {
        return self.program.addPat(.{ .ty = self.ty, .data = .{ .bind = l } });
    }

    fn wildcard(self: *TestProgram) Allocator.Error!Ast.PatId {
        return self.program.addPat(.{ .ty = self.ty, .data = .wildcard });
    }

    fn tagPat(self: *TestProgram, name: []const u8, payloads: []const Ast.PatId) Allocator.Error!Ast.PatId {
        return self.program.addPat(.{ .ty = self.ty, .data = .{ .tag = .{ .name = try self.tagName(name), .payloads = try self.program.addPatSpan(payloads) } } });
    }

    /// `match scrutinee { pat => body, _ => unit }`.
    fn matchOne(self: *TestProgram, scrutinee: Ast.ExprId, pat: Ast.PatId, body: Ast.ExprId) Allocator.Error!Ast.ExprId {
        const fallback = try self.unit();
        return self.expr(.{ .match_ = .{
            .scrutinee = scrutinee,
            .branches = try self.program.addBranchSpan(&.{
                .{ .pat = pat, .body = body },
                .{ .pat = try self.wildcard(), .body = fallback },
            }),
        } });
    }

    fn let(self: *TestProgram, l: Ast.LocalId, value: Ast.ExprId, rest: Ast.ExprId) Allocator.Error!Ast.ExprId {
        return self.expr(.{ .let_ = .{ .bind = try self.bind(l), .value = value, .rest = rest } });
    }

    fn function(self: *TestProgram, args: []const Ast.LocalId, captures: []const Ast.LocalId, body: Ast.ExprId) Allocator.Error!Ast.FnId {
        const id = try self.program.reserveFnSlot();
        try self.define(id, args, captures, body);
        return id;
    }

    fn define(self: *TestProgram, id: Ast.FnId, args: []const Ast.LocalId, captures: []const Ast.LocalId, body: Ast.ExprId) Allocator.Error!void {
        var arg_locals: std.ArrayList(Ast.TypedLocal) = .empty;
        defer arg_locals.deinit(self.program.allocator);
        for (args) |a| try arg_locals.append(self.program.allocator, .{ .local = a, .ty = self.ty });
        var capture_locals: std.ArrayList(Ast.TypedLocal) = .empty;
        defer capture_locals.deinit(self.program.allocator);
        for (captures) |c| try capture_locals.append(self.program.allocator, .{ .local = c, .ty = self.ty });
        self.program.setFn(id, .{
            .shapes = self.program.finishFnShapes(.{}),
            .symbol = self.symbol(),
            .args = try self.program.addTypedLocalSpan(arg_locals.items),
            .captures = try self.program.addTypedLocalSpan(capture_locals.items),
            .body = .{ .roc = body },
            .ret = self.ty,
        });
    }

    fn bodies(self: *TestProgram, gpa: Allocator) Allocator.Error![]?Ast.ExprId {
        const out = try gpa.alloc(?Ast.ExprId, self.program.fnCount());
        for (out, 0..) |*body, index| body.* = switch (self.program.getFnAt(index).body) {
            .roc => |b| b,
            .hosted => null,
        };
        return out;
    }
};

/// A seed that knows a constructor of each listed kind at each listed path.
const TestSeed = struct {
    known_paths: []const KnownPath,

    const KnownPath = struct { steps: []const Step, kinds: Consumers };

    fn oracle(self: *TestSeed) Oracle {
        return .{ .ctx = self, .known = known };
    }

    fn known(ctx: *anyopaque, path: []const Step, kinds: Consumers) Allocator.Error!bool {
        const self: *TestSeed = @ptrCast(@alignCast(ctx));
        outer: for (self.known_paths) |entry| {
            if (entry.steps.len != path.len) continue;
            for (entry.steps, path) |a, b| if (!a.eql(b)) continue :outer;
            if (entry.kinds.intersectWith(kinds).count() != 0) return true;
        }
        return false;
    }
};

const tag_kinds: Consumers = .initMany(&.{ .match, .loop });
const record_kinds: Consumers = .initMany(&.{ .loop, .project });

/// Whether the seed meets an argument consumer in `callee`'s extent, asked
/// of a freshly indexed `t`.
fn testConsumes(t: *TestProgram, seed: *TestSeed, callee: Ast.FnId) Allocator.Error!bool {
    const gpa = std.testing.allocator;
    const bodies = try t.bodies(gpa);
    defer gpa.free(bodies);
    const index = try Index.build(gpa, &t.program, bodies);
    defer index.deinit();
    var session = try Session.init(gpa, index, &t.program);
    defer session.deinit();
    return session.calleeConsumes(seed.oracle(), callee, .{ .kind = .arg, .index = 0 });
}

test "a known part the callee only stores meets no consumer" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const x = try t.local();
    const callee = try t.function(&.{x}, &.{}, try t.record("held", try t.ref(x)));
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(!try testConsumes(&t, &seed, callee));
}

test "a known tag the callee matches meets the match" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const x = try t.local();
    const callee = try t.function(&.{x}, &.{}, try t.matchOne(try t.ref(x), try t.tagPat("A", &.{}), try t.unit()));
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(try testConsumes(&t, &seed, callee));
}

test "a node with two parents in one body is followed through both" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const x = try t.local();
    const held = try t.local();
    // One expression node is both the bound value and the scrutinee.
    const shared = try t.ref(x);
    const body = try t.let(held, shared, try t.matchOne(shared, try t.tagPat("A", &.{}), try t.record("held", try t.ref(held))));
    const callee = try t.function(&.{x}, &.{}, body);
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(try testConsumes(&t, &seed, callee));
}

test "a node two bodies share keeps each body's uses apart" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const x = try t.local();
    const shared = try t.tag("Wrap", &.{try t.ref(x)});
    // The callee only stores the node; another body matches the same node.
    const callee = try t.function(&.{x}, &.{}, try t.record("held", shared));
    const p = try t.local();
    _ = try t.function(&.{}, &.{}, try t.matchOne(shared, try t.tagPat("Wrap", &.{try t.bind(p)}), try t.matchOne(try t.ref(p), try t.tagPat("A", &.{}), try t.unit())));
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(!try testConsumes(&t, &seed, callee));
}

test "a part wrapped by the caller crosses two calls to the read that unwraps it" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    // h(r) = match r.mode { A => {} }
    const r_h = try t.local();
    const h = try t.function(&.{r_h}, &.{}, try t.matchOne(try t.access(try t.ref(r_h), "mode"), try t.tagPat("A", &.{}), try t.unit()));
    // g(r) = h(r)
    const r_g = try t.local();
    const g = try t.function(&.{r_g}, &.{}, try t.call(h, &.{try t.ref(r_g)}));
    // f(x) = g({ mode: x })
    const x = try t.local();
    const f = try t.function(&.{x}, &.{}, try t.call(g, &.{try t.record("mode", try t.ref(x))}));
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(try testConsumes(&t, &seed, f));
}

test "a record update hides the fields it replaces and passes the rest through" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const mode = Step.field(try t.fieldName("mode"));
    const kind = Step.field(try t.fieldName("kind"));
    var seed: TestSeed = .{ .known_paths = &.{
        .{ .steps = &.{}, .kinds = record_kinds },
        .{ .steps = &.{mode}, .kinds = tag_kinds },
        .{ .steps = &.{kind}, .kinds = tag_kinds },
    } };
    {
        // g(r) = match r.mode {..}; f(x) = g({ ..x, mode: B })
        const r = try t.local();
        const g = try t.function(&.{r}, &.{}, try t.matchOne(try t.access(try t.ref(r), "mode"), try t.tagPat("A", &.{}), try t.unit()));
        const x = try t.local();
        const f = try t.function(&.{x}, &.{}, try t.call(g, &.{try t.update(try t.ref(x), "mode", try t.tag("B", &.{}))}));
        try std.testing.expect(!try testConsumes(&t, &seed, f));
    }
    {
        // g(r) = match r.kind {..}; f(x) = g({ ..x, mode: B })
        const r = try t.local();
        const g = try t.function(&.{r}, &.{}, try t.matchOne(try t.access(try t.ref(r), "kind"), try t.tagPat("A", &.{}), try t.unit()));
        const x = try t.local();
        const f = try t.function(&.{x}, &.{}, try t.call(g, &.{try t.update(try t.ref(x), "mode", try t.tag("B", &.{}))}));
        try std.testing.expect(try testConsumes(&t, &seed, f));
    }
}

test "a loop that wraps the known part on every pass stops at the back edge" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    const x = try t.local();
    const acc = try t.local();
    const q = try t.local();
    // loop (acc = Hold(x)) {
    //     match acc { Wrap(Wrap(Hold(q))) => match q { A => {} }, _ => continue Wrap(acc) }
    // }
    const unwrapped = try t.tagPat("Wrap", &.{try t.tagPat("Wrap", &.{try t.tagPat("Hold", &.{try t.bind(q)})})});
    const inner = try t.matchOne(try t.ref(q), try t.tagPat("A", &.{}), try t.unit());
    const again = try t.expr(.{ .continue_ = .{ .values = try t.program.addExprSpan(&.{try t.tag("Wrap", &.{try t.ref(acc)})}) } });
    const loop_body = try t.expr(.{ .match_ = .{
        .scrutinee = try t.ref(acc),
        .branches = try t.program.addBranchSpan(&.{
            .{ .pat = unwrapped, .body = inner },
            .{ .pat = try t.wildcard(), .body = again },
        }),
    } });
    const loop = try t.expr(.{ .loop_ = .{
        .params = try t.program.addTypedLocalSpan(&.{.{ .local = acc, .ty = t.ty }}),
        .initial_values = try t.program.addExprSpan(&.{try t.tag("Hold", &.{try t.ref(x)})}),
        .body = loop_body,
    } });
    const f = try t.function(&.{x}, &.{}, loop);
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(!try testConsumes(&t, &seed, f));
}

test "a function value that escapes through the result consumes its captures" {
    var t = try TestProgram.init(std.testing.allocator);
    defer t.deinit();
    // The closure's body matches its capture.
    const captured = try t.captureLocal();
    const closure = try t.function(&.{}, &.{captured}, try t.matchOne(try t.ref(captured), try t.tagPat("A", &.{}), try t.unit()));
    // f(x) = the closure, capturing x
    const x = try t.local();
    const capture_id = t.program.captureIdOfLocal(captured);
    const value = try t.expr(.{ .fn_ref = .{
        .fn_id = closure,
        .captures = try t.program.addCaptureOperandSpan(&.{.{ .id = capture_id, .value = try t.ref(x) }}),
    } });
    const f = try t.function(&.{x}, &.{}, value);
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(try testConsumes(&t, &seed, f));
}

test "an argument the callee returns meets the caller's match" {
    const gpa = std.testing.allocator;
    var t = try TestProgram.init(gpa);
    defer t.deinit();
    const x = try t.local();
    const identity = try t.function(&.{x}, &.{}, try t.ref(x));
    const r = try t.local();
    const call = try t.call(identity, &.{try t.tag("A", &.{})});
    const caller = try t.function(&.{}, &.{}, try t.let(r, call, try t.matchOne(try t.ref(r), try t.tagPat("A", &.{}), try t.unit())));
    const bodies = try t.bodies(gpa);
    defer gpa.free(bodies);
    const index = try Index.build(gpa, &t.program, bodies);
    defer index.deinit();
    var session = try Session.init(gpa, index, &t.program);
    defer session.deinit();
    var seed: TestSeed = .{ .known_paths = &.{.{ .steps = &.{}, .kinds = tag_kinds }} };
    try std.testing.expect(!try session.calleeConsumes(seed.oracle(), identity, .{ .kind = .arg, .index = 0 }));
    try std.testing.expect(try session.argumentCrosses(seed.oracle(), caller, &.{}, call, identity, .{ .kind = .arg, .index = 0 }));
}

test "a deep recursive walk over a wide recursive type follows only known structure" {
    // A checker over a union of many variants, each holding a record of
    // further nodes, the shape of a parser's syntax tree: a type with
    // thousands of parts. A seed that knows two levels of one node is
    // followed through those levels only.
    const gpa = std.testing.allocator;
    var t = try TestProgram.init(gpa);
    defer t.deinit();
    const variants = 200;
    const fields = [_][]const u8{ "left", "right", "body", "args" };
    const check_fn = try t.program.reserveFnSlot();
    const node = try t.local();
    var branches: std.ArrayList(Ast.Branch) = .empty;
    defer branches.deinit(gpa);
    var name_buf: [32]u8 = undefined;
    for (0..variants) |v| {
        const payload = try t.local();
        var calls: std.ArrayList(Ast.ExprId) = .empty;
        defer calls.deinit(gpa);
        for (fields) |field| try calls.append(gpa, try t.call(check_fn, &.{try t.access(try t.ref(payload), field)}));
        const name = try std.fmt.bufPrint(&name_buf, "V{d}", .{v});
        try branches.append(gpa, .{
            .pat = try t.tagPat(name, &.{try t.bind(payload)}),
            .body = try t.expr(.{ .tuple = try t.program.addExprSpan(calls.items) }),
        });
    }
    try t.define(check_fn, &.{node}, &.{}, try t.expr(.{ .match_ = .{
        .scrutinee = try t.ref(node),
        .branches = try t.program.addBranchSpan(branches.items),
    } }));
    // entry(r) = (check(r.left), check(r.right), check(r.body), check(r.args))
    const r = try t.local();
    var entry_calls: std.ArrayList(Ast.ExprId) = .empty;
    defer entry_calls.deinit(gpa);
    for (fields) |field| try entry_calls.append(gpa, try t.call(check_fn, &.{try t.access(try t.ref(r), field)}));
    const entry = try t.function(&.{r}, &.{}, try t.expr(.{ .tuple = try t.program.addExprSpan(entry_calls.items) }));
    const v7 = Step.payload(try t.tagName("V7"), 0);
    const left = Step.field(try t.fieldName("left"));
    const inner = Step.field(try t.fieldName("inner"));
    // A record whose `inner` is known reaches no consumer: no part the
    // checker reads is known.
    var stored: TestSeed = .{ .known_paths = &.{
        .{ .steps = &.{}, .kinds = record_kinds },
        .{ .steps = &.{inner}, .kinds = tag_kinds },
    } };
    try std.testing.expect(!try testConsumes(&t, &stored, entry));
    // A record whose `left` is a known `V7` node meets the checker's match.
    var read: TestSeed = .{ .known_paths = &.{
        .{ .steps = &.{}, .kinds = record_kinds },
        .{ .steps = &.{left}, .kinds = tag_kinds },
        .{ .steps = &.{ left, v7 }, .kinds = record_kinds },
    } };
    try std.testing.expect(try testConsumes(&t, &read, entry));
}

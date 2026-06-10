//! Splits struct-typed join parameters into one parameter per field.
//!
//! Match-arm and loop lowering shuttle multi-value state between joins inside
//! by-value wrapper structs: each jump builds a fresh struct, the join body
//! immediately reads the fields back out, and nothing else ever touches the
//! whole value. The wrapper costs a build, per-field reads, and — once ARC
//! runs — a retain on each refcounted field read paired with a release of the
//! wrapper, because the field read's lender dies at the jump.
//!
//! This pass runs after direct LIR lowering and before ARC insertion. For a
//! join parameter that is only ever read field-by-field and only ever
//! initialized from single-use struct literals, it replaces the parameter
//! with one parameter per field, rewrites each jump to pass the literal's
//! operands directly (deleting the build), and rewrites each field read into
//! a local alias. Refcounted state then flows through pure alias chains that
//! borrow inference turns into moves, and the wrapper disappears entirely.
//!
//! The pass iterates to a fixpoint so nested wrappers dissolve layer by
//! layer. Parameters with any whole-value use, any non-literal initializer,
//! or a shared initializer keep their shape.

const std = @import("std");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

pub const ScalarizeError = std.mem.Allocator.Error;

/// Maximum scalarized field count per parameter; wider wrappers keep their
/// shape (parameter spans stay small and LLVM gains little beyond this).
const max_fields = 16;

/// Maximum nesting rounds; deeper than this means a degenerate wrapper tower.
const max_rounds = 16;

pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ScalarizeError!void {
    var pass = Pass{
        .store = store,
        .layouts = layouts,
        .allocator = store.allocator,
        .use_other = std.AutoHashMap(LIR.LocalId, void).init(store.allocator),
        .field_reads = std.AutoHashMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)).init(store.allocator),
        .init_writes = std.AutoHashMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)).init(store.allocator),
        .write_other = std.AutoHashMap(LIR.LocalId, void).init(store.allocator),
        .struct_builds = std.AutoHashMap(LIR.LocalId, StructBuild).init(store.allocator),
        .removed = std.AutoHashMap(LIR.CFStmtId, LIR.CFStmtId).init(store.allocator),
        .visited = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator),
        .stack = .empty,
    };
    defer pass.deinit();

    var rounds: usize = 0;
    while (rounds < max_rounds) : (rounds += 1) {
        var changed = false;
        for (store.proc_specs.items, 0..) |proc, proc_index| {
            const body = proc.body orelse continue;
            if (try pass.scalarizeProc(@enumFromInt(@as(u32, @intCast(proc_index))), body)) {
                changed = true;
            }
        }
        if (!changed) break;
    }
}

const StructBuild = struct {
    stmt: LIR.CFStmtId,
    fields: LIR.LocalSpan,
    /// Number of defining statements seen for the local; only exactly one
    /// struct literal qualifies.
    def_count: u32,
    /// Uses other than being an `initialize_join_param` value.
    uses: u32,
    /// Uses as an `initialize_join_param` value; exactly one qualifies.
    init_uses: u32,
};

const Pass = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    allocator: Allocator,
    /// Locals with any use other than a field read.
    use_other: std.AutoHashMap(LIR.LocalId, void),
    /// Field-read statements per source local.
    field_reads: std.AutoHashMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)),
    /// `initialize_join_param` writes per target local.
    init_writes: std.AutoHashMap(LIR.LocalId, std.ArrayList(LIR.CFStmtId)),
    /// Locals with any write other than an `initialize_join_param`.
    write_other: std.AutoHashMap(LIR.LocalId, void),
    /// Struct-literal defs per target local.
    struct_builds: std.AutoHashMap(LIR.LocalId, StructBuild),
    /// Deleted build statements mapped to their continuations, for edge
    /// patching.
    removed: std.AutoHashMap(LIR.CFStmtId, LIR.CFStmtId),
    visited: std.AutoHashMap(LIR.CFStmtId, void),
    stack: std.ArrayList(LIR.CFStmtId),

    fn deinit(self: *Pass) void {
        self.use_other.deinit();
        self.clearLists();
        self.field_reads.deinit();
        self.init_writes.deinit();
        self.write_other.deinit();
        self.struct_builds.deinit();
        self.removed.deinit();
        self.visited.deinit();
        self.stack.deinit(self.allocator);
    }

    fn clearLists(self: *Pass) void {
        var reads = self.field_reads.valueIterator();
        while (reads.next()) |list| list.deinit(self.allocator);
        var writes = self.init_writes.valueIterator();
        while (writes.next()) |list| list.deinit(self.allocator);
    }

    fn resetProc(self: *Pass) void {
        self.use_other.clearRetainingCapacity();
        self.clearLists();
        self.field_reads.clearRetainingCapacity();
        self.init_writes.clearRetainingCapacity();
        self.write_other.clearRetainingCapacity();
        self.struct_builds.clearRetainingCapacity();
        self.removed.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
    }

    fn noteUse(self: *Pass, local: LIR.LocalId) ScalarizeError!void {
        try self.use_other.put(local, {});
        if (self.struct_builds.getPtr(local)) |build| build.uses += 1;
    }

    fn noteFieldRead(self: *Pass, source: LIR.LocalId, stmt: LIR.CFStmtId) ScalarizeError!void {
        const entry = try self.field_reads.getOrPut(source);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.allocator, stmt);
        if (self.struct_builds.getPtr(source)) |build| build.uses += 1;
    }

    fn noteWrite(self: *Pass, target: LIR.LocalId) ScalarizeError!void {
        try self.write_other.put(target, {});
    }

    fn noteStructBuild(self: *Pass, target: LIR.LocalId, stmt: LIR.CFStmtId, fields: LIR.LocalSpan) ScalarizeError!void {
        const entry = try self.struct_builds.getOrPut(target);
        if (entry.found_existing) {
            entry.value_ptr.def_count += 1;
        } else {
            entry.value_ptr.* = .{ .stmt = stmt, .fields = fields, .def_count = 1, .uses = 0, .init_uses = 0 };
        }
    }

    fn scalarizeProc(self: *Pass, proc_id: LIR.LirProcSpecId, body: LIR.CFStmtId) ScalarizeError!bool {
        self.resetProc();
        try self.collect(body);

        // Find one scalarizable parameter; the fixpoint loop picks up the
        // rest on later rounds.
        var changed = false;

        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);
        outer: while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .join => |join_stmt| {
                    for (self.store.getLocalSpan(join_stmt.params), 0..) |param, position| {
                        if (try self.tryScalarize(current, join_stmt, param, position)) {
                            changed = true;
                            break :outer;
                        }
                    }
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                .switch_stmt => |s| {
                    for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                        try self.stack.append(self.allocator, branch.body);
                    }
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try self.stack.append(self.allocator, continuation);
                    }
                },
                inline .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free => |s| {
                    try self.stack.append(self.allocator, s.next);
                },
                .jump, .ret, .crash, .runtime_error, .loop_continue, .loop_break => {},
            }
        }

        if (changed) {
            try self.patchRemovedEdges(proc_id);
        }
        return changed;
    }

    fn tryScalarize(
        self: *Pass,
        join_id: LIR.CFStmtId,
        join_stmt: anytype,
        param: LIR.LocalId,
        position: usize,
    ) ScalarizeError!bool {
        const param_layout = self.layouts.getLayout(self.store.getLocal(param).layout_idx);
        if (param_layout.tag != .struct_) return false;
        const info = self.layouts.getStructInfo(param_layout);
        var field_count: usize = 0;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            field_count = @max(field_count, @as(usize, field.index) + 1);
        }
        if (field_count == 0 or field_count > max_fields) return false;

        // The parameter must be touched only by field reads and
        // `initialize_join_param` writes.
        if (self.use_other.contains(param)) return false;
        if (self.write_other.contains(param)) return false;
        const reads = self.field_reads.getPtr(param) orelse return false;
        const writes = self.init_writes.getPtr(param) orelse return false;

        // Every initializer must be a single-def, single-use struct literal
        // with one operand per field.
        for (writes.items) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).set_local;
            if (write.value == param) return false;
            const build = self.struct_builds.get(write.value) orelse return false;
            if (build.def_count != 1 or build.uses != 0 or build.init_uses != 1) return false;
            if (self.write_other.contains(write.value)) return false;
            if (self.store.getLocalSpan(build.fields).len != field_count) return false;
        }
        for (reads.items) |read_stmt| {
            const read = self.store.getCFStmt(read_stmt).assign_ref;
            if (read.op.field.field_idx >= field_count) return false;
        }

        // Create the per-field parameter locals.
        var field_locals_buffer: [max_fields]LIR.LocalId = undefined;
        for (0..field_count) |k| {
            const field_layout = self.layouts.getStructFieldLayoutByOriginalIndex(
                param_layout.getStruct().idx,
                @intCast(k),
            );
            field_locals_buffer[k] = try self.store.addLocal(.{ .layout_idx = field_layout });
        }
        const field_locals = field_locals_buffer[0..field_count];

        // The join's parameter span gets the fields in the parameter's
        // place.
        const old_params = self.store.getLocalSpan(join_stmt.params);
        var new_params = std.ArrayList(LIR.LocalId).empty;
        defer new_params.deinit(self.allocator);
        for (old_params, 0..) |old_param, old_position| {
            if (old_position == position) {
                try new_params.appendSlice(self.allocator, field_locals);
            } else {
                try new_params.append(self.allocator, old_param);
            }
        }
        const new_span = try self.store.addLocalSpan(new_params.items);
        self.store.getCFStmtPtr(join_id).join.params = new_span;

        // Field reads become aliases of the field parameters.
        for (reads.items) |read_stmt| {
            const read_ptr = self.store.getCFStmtPtr(read_stmt);
            const field_idx = read_ptr.assign_ref.op.field.field_idx;
            read_ptr.assign_ref.op = .{ .local = field_locals[field_idx] };
        }

        // Each jump-site write becomes one write per field, passing the
        // literal's operands directly; the literal's build is deleted.
        for (writes.items) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).set_local;
            const build = self.struct_builds.get(write.value).?;
            const operands = self.store.getLocalSpan(build.fields);

            var next = write.next;
            var k: usize = field_count;
            while (k > 1) {
                k -= 1;
                next = try self.store.addCFStmt(.{ .set_local = .{
                    .target = field_locals[k],
                    .value = operands[k],
                    .mode = .initialize_join_param,
                    .next = next,
                } });
            }
            self.store.getCFStmtPtr(write_stmt).set_local = .{
                .target = field_locals[0],
                .value = operands[0],
                .mode = .initialize_join_param,
                .next = next,
            };

            const build_next = switch (self.store.getCFStmt(build.stmt)) {
                .assign_struct => |b| b.next,
                else => unreachable,
            };
            try self.removed.put(build.stmt, build_next);
        }

        return true;
    }

    /// Redirects every edge that targets a deleted build statement to that
    /// statement's continuation.
    fn patchRemovedEdges(self: *Pass, proc_id: LIR.LirProcSpecId) ScalarizeError!void {
        if (self.removed.count() == 0) return;
        const proc = self.store.getProcSpecPtr(proc_id);
        if (proc.body) |body| {
            proc.body = self.resolveRemoved(body);
        }

        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, proc.body.?);
        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            const stmt = self.store.getCFStmtPtr(current);
            switch (stmt.*) {
                .switch_stmt => |*s| {
                    for (self.store.getCFSwitchBranchesMut(s.branches)) |*branch| {
                        branch.body = self.resolveRemoved(branch.body);
                        try self.stack.append(self.allocator, branch.body);
                    }
                    s.default_branch = self.resolveRemoved(s.default_branch);
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        s.continuation = self.resolveRemoved(continuation);
                        try self.stack.append(self.allocator, s.continuation.?);
                    }
                },
                .join => |*j| {
                    j.body = self.resolveRemoved(j.body);
                    j.remainder = self.resolveRemoved(j.remainder);
                    try self.stack.append(self.allocator, j.body);
                    try self.stack.append(self.allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free => |*s| {
                    s.next = self.resolveRemoved(s.next);
                    try self.stack.append(self.allocator, s.next);
                },
                .jump, .ret, .crash, .runtime_error, .loop_continue, .loop_break => {},
            }
        }
    }

    fn resolveRemoved(self: *const Pass, stmt: LIR.CFStmtId) LIR.CFStmtId {
        var cursor = stmt;
        var steps: usize = 0;
        while (self.removed.get(cursor)) |next| {
            cursor = next;
            steps += 1;
            if (steps > self.removed.count()) break;
        }
        return cursor;
    }

    fn collect(self: *Pass, body: LIR.CFStmtId) ScalarizeError!void {
        // First pass: struct-literal defs, so the use pass can attribute
        // uses to builds regardless of traversal order.
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);
        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .assign_struct => |assign| {
                    try self.noteStructBuild(assign.target, current, assign.fields);
                    try self.stack.append(self.allocator, assign.next);
                },
                .switch_stmt => |s| {
                    for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                        try self.stack.append(self.allocator, branch.body);
                    }
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try self.stack.append(self.allocator, continuation);
                    }
                },
                .join => |join_stmt| {
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                inline .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free => |a| {
                    try self.stack.append(self.allocator, a.next);
                },
                .jump, .ret, .crash, .runtime_error, .loop_continue, .loop_break => {},
            }
        }

        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);
        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .field => |op| try self.noteFieldRead(op.source, current),
                        .local => |source| try self.noteUse(source),
                        .discriminant => |op| try self.noteUse(op.source),
                        .tag_payload => |op| try self.noteUse(op.source),
                        .tag_payload_struct => |op| try self.noteUse(op.source),
                        .list_reinterpret => |op| try self.noteUse(op.backing_ref),
                        .nominal => |op| try self.noteUse(op.backing_ref),
                    }
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_call => |assign| {
                    for (self.store.getLocalSpan(assign.args)) |arg| try self.noteUse(arg);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    try self.noteUse(assign.closure);
                    for (self.store.getLocalSpan(assign.args)) |arg| try self.noteUse(arg);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| try self.noteUse(capture);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    for (self.store.getLocalSpan(assign.args)) |arg| try self.noteUse(arg);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_list => |assign| {
                    for (self.store.getLocalSpan(assign.elems)) |elem| try self.noteUse(elem);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    for (self.store.getLocalSpan(assign.fields)) |field| try self.noteUse(field);
                    try self.stack.append(self.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload) |payload| try self.noteUse(payload);
                    try self.noteWrite(assign.target);
                    try self.stack.append(self.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.mode == .initialize_join_param) {
                        const entry = try self.init_writes.getOrPut(assign.target);
                        if (!entry.found_existing) entry.value_ptr.* = .empty;
                        try entry.value_ptr.append(self.allocator, current);
                        if (self.struct_builds.getPtr(assign.value)) |build| {
                            // Counted separately: a qualifying build's only
                            // use must be this write.
                            build.init_uses += 1;
                        } else {
                            try self.noteUse(assign.value);
                        }
                    } else {
                        try self.noteUse(assign.value);
                        try self.noteWrite(assign.target);
                    }
                    try self.stack.append(self.allocator, assign.next);
                },
                .debug => |s| {
                    try self.noteUse(s.message);
                    try self.stack.append(self.allocator, s.next);
                },
                .expect => |s| {
                    try self.noteUse(s.condition);
                    try self.stack.append(self.allocator, s.next);
                },
                .switch_stmt => |s| {
                    try self.noteUse(s.cond);
                    for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                        try self.stack.append(self.allocator, branch.body);
                    }
                    try self.stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try self.stack.append(self.allocator, continuation);
                    }
                },
                .join => |join_stmt| {
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                .ret => |ret_stmt| try self.noteUse(ret_stmt.value),
                .incref => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .decref => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .free => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .jump, .crash, .runtime_error, .loop_continue, .loop_break => {},
            }
        }
    }
};

test "scalarize declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;

const ScalarizeTest = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    pair: layout_mod.Idx,

    fn init(allocator: Allocator) Allocator.Error!ScalarizeTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();
        const pair = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = .i64 },
            .{ .index = 1, .layout = .str },
        });
        return .{
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .pair = pair,
        };
    }

    fn deinit(self: *ScalarizeTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }
};

test "scalarize splits a literal-initialized struct join parameter" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // join J(state: {i64, str}):
    //   body: n = state.0; s = state.1; ret n
    //   remainder: num = 1; text = "x"; wrapper = {num, text};
    //              state := wrapper; jump J
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id: LIR.JoinPointId = @enumFromInt(0);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = n } });
    const read_s = try store.addCFStmt(.{ .assign_ref = .{
        .target = s,
        .op = .{ .field = .{ .source = state, .field_idx = 1 } },
        .next = ret,
    } });
    const read_n = try store.addCFStmt(.{ .assign_ref = .{
        .target = n,
        .op = .{ .field = .{ .source = state, .field_idx = 0 } },
        .next = read_s,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = read_n,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    // The join now carries two parameters, the field reads are aliases of
    // them, the jump site writes both fields directly, and the wrapper's
    // build is unreachable.
    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 2), params.len);

    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(params[0], new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(params[1], new_read_s.op.local);

    const new_set = store.getCFStmt(set_state).set_local;
    try testing.expectEqual(params[0], new_set.target);
    try testing.expectEqual(num, new_set.value);
    const second_set = store.getCFStmt(new_set.next).set_local;
    try testing.expectEqual(params[1], second_set.target);
    try testing.expectEqual(text, second_set.value);
    try testing.expectEqual(jump, second_set.next);

    // The text literal now flows straight to the first set_local.
    const new_text_assign = store.getCFStmt(text_assign).assign_literal;
    try testing.expectEqual(set_state, new_text_assign.next);
}

test "scalarize keeps parameters with whole-value uses" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const whole = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const wrapper = try store.addLocal(.{ .layout_idx = f.pair });
    const join_id: LIR.JoinPointId = @enumFromInt(0);

    // The body copies the whole parameter, which must block scalarization.
    const ret_local = try store.addLocal(.{ .layout_idx = .i64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = ret_local } });
    const ret_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = ret_local,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } },
        .next = ret,
    } });
    const copy_whole = try store.addCFStmt(.{ .assign_ref = .{
        .target = whole,
        .op = .{ .local = state },
        .next = ret_assign,
    } });

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_state = try store.addCFStmt(.{ .set_local = .{
        .target = state,
        .value = wrapper,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = wrapper,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = set_state,
    } });
    const text_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = text,
        .value = .{ .str_literal = try store.insertStringView("x", 0, 1) },
        .next = build,
    } });
    const num_assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = num,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
        .next = text_assign,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{state}),
        .body = copy_whole,
        .remainder = num_assign,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = join,
        .ret_layout = .i64,
    });

    try run(store, &f.layouts);

    const unchanged_join = store.getCFStmt(join).join;
    try testing.expectEqual(@as(usize, 1), store.getLocalSpan(unchanged_join.params).len);
    const unchanged_set = store.getCFStmt(set_state).set_local;
    try testing.expectEqual(state, unchanged_set.target);
    try testing.expectEqual(wrapper, unchanged_set.value);
}

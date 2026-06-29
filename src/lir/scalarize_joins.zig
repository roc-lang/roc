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
//! initialized from struct literals — either built directly into the
//! parameter local or built into a single-use temporary that a
//! `set_local initialize_join_param` copies in — it replaces the parameter
//! with one parameter per field, rewrites each jump to pass the literal's
//! operands directly (deleting or replacing the build), and rewrites each
//! field read into a local alias. Refcounted state then flows through pure
//! alias chains that borrow inference turns into moves, and the wrapper
//! disappears entirely.
//!
//! A join's remainder (its run-once entry path) may enter the join without an
//! `initialize_join_param` write for a parameter — a plain tail-call
//! elimination loop header does this, entering with a bare jump because the
//! parameters are the proc's own argument locals. Such a parameter is still
//! scalarizable: its per-field parameters are seeded once on the remainder by
//! reading the incoming struct's fields. That read is sound only because the
//! sole write-less entry shape supplies the value through a proc argument, an
//! invariant the pass enforces.
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

/// Scalarizes eligible struct-typed join parameters across every proc in the
/// store, repeating until no parameter qualifies.
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
    /// Every struct-literal statement defining the local. A wrapper
    /// temporary qualifies with exactly one; a parameter built directly may
    /// have one per jump.
    builds: std.ArrayList(BuildSite),
    /// Uses other than being an `initialize_join_param` value.
    uses: u32,
    /// Uses as an `initialize_join_param` value; a wrapper temporary
    /// qualifies with exactly one.
    init_uses: u32,
};

const BuildSite = struct {
    stmt: LIR.CFStmtId,
    fields: LIR.LocalSpan,
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
    /// Field-parameter locals created this round; they join the proc's
    /// frame locals so frame plans cover them.
    new_locals: std.ArrayList(LIR.LocalId) = .empty,

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
        self.new_locals.deinit(self.allocator);
    }

    fn clearLists(self: *Pass) void {
        var reads = self.field_reads.valueIterator();
        while (reads.next()) |list| list.deinit(self.allocator);
        var writes = self.init_writes.valueIterator();
        while (writes.next()) |list| list.deinit(self.allocator);
        var builds = self.struct_builds.valueIterator();
        while (builds.next()) |build| build.builds.deinit(self.allocator);
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
        self.new_locals.clearRetainingCapacity();
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
        if (!entry.found_existing) {
            entry.value_ptr.* = .{ .builds = .empty, .uses = 0, .init_uses = 0 };
        }
        try entry.value_ptr.builds.append(self.allocator, .{ .stmt = stmt, .fields = fields });
    }

    fn scalarizeProc(self: *Pass, proc_id: LIR.LirProcSpecId, body: LIR.CFStmtId) ScalarizeError!bool {
        self.resetProc();
        try self.collect(body);

        // Resolve proc-argument membership here, where the argument span is
        // freshly valid, and pass a plain bool into `tryScalarize`. The span is
        // a view into the store's local-id buffer, which `tryScalarize`
        // reallocates when it scalarizes, so it must not be read across that
        // call.
        const proc_args = self.store.getLocalSpan(self.store.getProcSpec(proc_id).args);

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
                        const param_is_proc_arg = std.mem.findScalar(LIR.LocalId, proc_args, param) != null;
                        if (try self.tryScalarize(param_is_proc_arg, current, join_stmt, param, position)) {
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
                .switch_initialized_payload => |s| {
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    for (self.store.getStrMatchArms(s.arms)) |arm| {
                        try self.stack.append(self.allocator, arm.on_match);
                    }
                    try self.stack.append(self.allocator, s.on_miss);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try self.stack.append(self.allocator, s.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }

        if (changed) {
            try self.patchRemovedEdges(proc_id);
            try self.extendFrameLocals(proc_id);
        }
        return changed;
    }

    /// Adds the new field-parameter locals to the proc's frame locals so
    /// frame plans cover them.
    fn extendFrameLocals(self: *Pass, proc_id: LIR.LirProcSpecId) ScalarizeError!void {
        if (self.new_locals.items.len == 0) return;
        const proc = self.store.getProcSpecPtr(proc_id);
        var combined = std.ArrayList(LIR.LocalId).empty;
        defer combined.deinit(self.allocator);
        try combined.appendSlice(self.allocator, self.store.getLocalSpan(proc.frame_locals));
        try combined.appendSlice(self.allocator, self.new_locals.items);
        proc.frame_locals = try self.store.addLocalSpan(combined.items);
        if (self.store.procNeedsStackProbe(self.layouts, proc.*)) {
            proc.stack_probe = .required;
        }
    }

    /// `param_is_proc_arg` records whether this join parameter is also one of
    /// the proc's argument locals. Every jump that targets the join carries
    /// `initialize_join_param` writes for its parameters, which the rewrite
    /// below turns into per-field writes. The one entry that carries no such
    /// write is a plain-TCE loop header: it reuses the proc's own argument
    /// locals as the join parameters and enters with a bare jump, so the
    /// parameter's initial value arrives through the argument. That is the only
    /// shape in which a join parameter is also a proc argument, and it is
    /// exactly the shape whose field parameters must be seeded from the
    /// argument struct on entry. (Any future shape that entered a parameter
    /// without a write and without a seed would surface as an unbound local in
    /// the ARC borrow certifier, never as a silent miscompile.)
    fn tryScalarize(
        self: *Pass,
        param_is_proc_arg: bool,
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

        // The parameter must be touched only by field reads, direct
        // struct-literal builds, and `initialize_join_param` writes.
        if (self.use_other.contains(param)) return false;
        if (self.write_other.contains(param)) return false;
        const reads = self.field_reads.getPtr(param) orelse return false;
        const empty_writes: []const LIR.CFStmtId = &.{};
        const writes: []const LIR.CFStmtId = if (self.init_writes.getPtr(param)) |list| list.items else empty_writes;
        const empty_builds: []const BuildSite = &.{};
        const direct_builds: []const BuildSite = if (self.struct_builds.getPtr(param)) |entry| entry.builds.items else empty_builds;
        if (writes.len == 0 and direct_builds.len == 0) return false;

        // A directly-built parameter's literals each become per-field
        // writes in place.
        for (direct_builds) |site| {
            if (self.store.getLocalSpan(site.fields).len != field_count) return false;
        }

        // A copied-in initializer must be a single-def, single-use struct
        // literal with one operand per field.
        for (writes) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).set_local;
            if (write.value == param) return false;
            const build = self.struct_builds.get(write.value) orelse return false;
            if (build.builds.items.len != 1 or build.uses != 0 or build.init_uses != 1) return false;
            if (self.write_other.contains(write.value)) return false;
            if (self.store.getLocalSpan(build.builds.items[0].fields).len != field_count) return false;
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
        try self.new_locals.appendSlice(self.allocator, field_locals);

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
        for (writes) |write_stmt| {
            const write = self.store.getCFStmt(write_stmt).set_local;
            const build = self.struct_builds.get(write.value).?;
            const site = build.builds.items[0];
            const operands = self.store.getLocalSpan(site.fields);
            try self.writeFields(write_stmt, write.next, field_locals, operands);

            const build_next = switch (self.store.getCFStmt(site.stmt)) {
                .assign_struct => |b| b.next,
                else => unreachable,
            };
            try self.removed.put(site.stmt, build_next);
        }

        // Each direct build becomes per-field writes in its place.
        for (direct_builds) |site| {
            const operands = self.store.getLocalSpan(site.fields);
            const build_next = switch (self.store.getCFStmt(site.stmt)) {
                .assign_struct => |b| b.next,
                else => unreachable,
            };
            try self.writeFields(site.stmt, build_next, field_locals, operands);
        }

        // Seed the field parameters on the write-less proc-entry path (see the
        // comment at param_is_proc_arg above): reading the argument struct's
        // fields in the join's remainder, which runs once before the loop body.
        if (param_is_proc_arg) try self.seedFieldsFromArgStruct(join_id, param, field_locals);

        return true;
    }

    /// Prepend, to the join's remainder, one `ref.field arg[k]` read plus an
    /// `initialize_join_param` write into the corresponding field parameter,
    /// so the field parameters carry the argument struct's fields on the
    /// proc-entry path. The read temporaries join the proc's frame locals.
    fn seedFieldsFromArgStruct(
        self: *Pass,
        join_id: LIR.CFStmtId,
        arg_struct: LIR.LocalId,
        field_locals: []const LIR.LocalId,
    ) ScalarizeError!void {
        var next = self.store.getCFStmtPtr(join_id).join.remainder;
        var k: usize = field_locals.len;
        while (k > 0) {
            k -= 1;
            const tmp = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(field_locals[k]).layout_idx });
            try self.new_locals.append(self.allocator, tmp);
            const set_stmt = try self.store.addCFStmt(.{ .set_local = .{
                .target = field_locals[k],
                .value = tmp,
                .mode = .initialize_join_param,
                .next = next,
            } });
            next = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = tmp,
                .op = .{ .field = .{ .source = arg_struct, .field_idx = @intCast(k) } },
                .next = set_stmt,
            } });
        }
        self.store.getCFStmtPtr(join_id).join.remainder = next;
    }

    /// Replaces `stmt` with an `initialize_join_param` write of field 0 and
    /// inserts writes for the remaining fields before `next_after`.
    fn writeFields(
        self: *Pass,
        stmt: LIR.CFStmtId,
        next_after: LIR.CFStmtId,
        field_locals: []const LIR.LocalId,
        operands: []const LIR.LocalId,
    ) ScalarizeError!void {
        var next = next_after;
        var k: usize = field_locals.len;
        while (k > 1) {
            k -= 1;
            next = try self.store.addCFStmt(.{ .set_local = .{
                .target = field_locals[k],
                .value = operands[k],
                .mode = .initialize_join_param,
                .next = next,
            } });
        }
        self.store.getCFStmtPtr(stmt).* = .{ .set_local = .{
            .target = field_locals[0],
            .value = operands[0],
            .mode = .initialize_join_param,
            .next = next,
        } };
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
                .switch_initialized_payload => |*s| {
                    s.initialized_branch = self.resolveRemoved(s.initialized_branch);
                    s.uninitialized_branch = self.resolveRemoved(s.uninitialized_branch);
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |*s| {
                    s.on_match = self.resolveRemoved(s.on_match);
                    s.on_miss = self.resolveRemoved(s.on_miss);
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |*s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    const rewritten_arms = try self.allocator.alloc(LIR.StrMatchArm, arms.len);
                    defer self.allocator.free(rewritten_arms);
                    for (arms, rewritten_arms) |arm, *rewritten| {
                        rewritten.* = arm;
                        rewritten.on_match = self.resolveRemoved(arm.on_match);
                        try self.stack.append(self.allocator, rewritten.on_match);
                    }
                    s.arms = try self.store.addStrMatchArms(rewritten_arms);
                    s.on_miss = self.resolveRemoved(s.on_miss);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .join => |*j| {
                    j.body = self.resolveRemoved(j.body);
                    j.remainder = self.resolveRemoved(j.remainder);
                    try self.stack.append(self.allocator, j.body);
                    try self.stack.append(self.allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |*s| {
                    s.next = self.resolveRemoved(s.next);
                    try self.stack.append(self.allocator, s.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
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
                .switch_initialized_payload => |s| {
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    for (self.store.getStrMatchArms(s.arms)) |arm| {
                        try self.stack.append(self.allocator, arm.on_match);
                    }
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .join => |join_stmt| {
                    try self.stack.append(self.allocator, join_stmt.body);
                    try self.stack.append(self.allocator, join_stmt.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |a| {
                    try self.stack.append(self.allocator, a.next);
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
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
                .init_uninitialized => |init| {
                    try self.noteWrite(init.target);
                    try self.stack.append(self.allocator, init.next);
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
                .expect_err => |s| try self.noteUse(s.message),
                .expect => |s| {
                    try self.noteUse(s.condition);
                    try self.stack.append(self.allocator, s.next);
                },
                .comptime_branch_taken => |s| try self.stack.append(self.allocator, s.next),
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
                .switch_initialized_payload => |s| {
                    try self.noteUse(s.cond);
                    try self.stack.append(self.allocator, s.initialized_branch);
                    try self.stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try self.noteUse(s.source);
                    for (self.store.getStrMatchSteps(s.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| try self.noteWrite(local),
                        }
                    }
                    try self.stack.append(self.allocator, s.on_match);
                    try self.stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    try self.noteUse(s.source);
                    for (self.store.getStrMatchArms(s.arms)) |arm| {
                        for (self.store.getStrMatchSteps(arm.steps)) |step| {
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| try self.noteWrite(local),
                            }
                        }
                        try self.stack.append(self.allocator, arm.on_match);
                    }
                    try self.stack.append(self.allocator, s.on_miss);
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
                .decref_if_initialized => |rc| {
                    try self.noteUse(rc.cond);
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .free => |rc| {
                    try self.noteUse(rc.value);
                    try self.stack.append(self.allocator, rc.next);
                },
                .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
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
    next_join_point: u32 = 0,

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

    fn freshJoinPointId(self: *ScalarizeTest) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
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
    const join_id = f.freshJoinPointId();

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
    const join_id = f.freshJoinPointId();

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

test "scalarize splits a parameter built directly by a struct literal" {
    var f = try ScalarizeTest.init(testing.allocator);
    defer f.deinit();
    const store = &f.store;

    // join J(state: {i64, str}):
    //   body: n = state.0; s = state.1; ret n
    //   remainder: num = 1; text = "x"; state = {num, text}; jump J
    const state = try store.addLocal(.{ .layout_idx = f.pair });
    const num = try store.addLocal(.{ .layout_idx = .i64 });
    const text = try store.addLocal(.{ .layout_idx = .str });
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const s = try store.addLocal(.{ .layout_idx = .str });
    const join_id = f.freshJoinPointId();

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
    const build = try store.addCFStmt(.{ .assign_struct = .{
        .target = state,
        .fields = try store.addLocalSpan(&.{ num, text }),
        .next = jump,
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
    // them, and the build became per-field writes feeding the jump.
    const new_join = store.getCFStmt(join).join;
    const params = store.getLocalSpan(new_join.params);
    try testing.expectEqual(@as(usize, 2), params.len);

    const new_read_n = store.getCFStmt(read_n).assign_ref;
    try testing.expectEqual(params[0], new_read_n.op.local);
    const new_read_s = store.getCFStmt(read_s).assign_ref;
    try testing.expectEqual(params[1], new_read_s.op.local);

    const first_set = store.getCFStmt(build).set_local;
    try testing.expectEqual(params[0], first_set.target);
    try testing.expectEqual(num, first_set.value);
    try testing.expectEqual(LIR.SetLocalWriteMode.initialize_join_param, first_set.mode);
    const second_set = store.getCFStmt(first_set.next).set_local;
    try testing.expectEqual(params[1], second_set.target);
    try testing.expectEqual(text, second_set.value);
    try testing.expectEqual(jump, second_set.next);
}

//! LIR-level tests for the TRMC compiler-internal pointer ops
//! (ptr_alloca, box_alloc_zeroed, ptr_store, ptr_load, ptr_cast), exercised
//! through the interpreter on hand-built LIR. The TRMC pass itself
//! (src/lir/trmc.zig) builds on these ops; its detection/transform tests
//! also live in this file.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const eval = @import("eval");
const layout = @import("layout");
const lir = @import("lir");
const RuntimeHostEnv = eval.RuntimeHostEnv;

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const LirStore = lir.LirStore;
const GuardedList = LirStore.GuardedList;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const LowLevel = lir.LowLevel;

const TrmcLirTestError = Allocator.Error || eval.Interpreter.Error || RuntimeHostEnv.LeakError || lir.DebugPrint.Error || error{
    TestExpectedEqual,
    TestUnexpectedResult,
};

/// Tracks created locals so the proc's frame_locals span (which the
/// interpreter binary-searches) can be built complete and sorted.
const ProcBuilder = struct {
    store: *LirStore,
    locals: std.ArrayList(LocalId),

    fn init(store: *LirStore) ProcBuilder {
        return .{ .store = store, .locals = .empty };
    }

    fn deinit(self: *ProcBuilder, allocator: Allocator) void {
        self.locals.deinit(allocator);
    }

    fn addLocal(self: *ProcBuilder, allocator: Allocator, layout_idx: layout.Idx) TrmcLirTestError!LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.locals.append(allocator, id);
        return id;
    }

    fn finishProc(self: *ProcBuilder, args: []const LocalId, body: CFStmtId, ret_layout: layout.Idx) TrmcLirTestError!LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(args),
            .frame_locals = try self.store.addLocalSpan(self.locals.items),
            .body = body,
            .ret_layout = ret_layout,
        });
    }
};

fn lowLevelStmt(store: *LirStore, target: LocalId, op: LowLevel, args: []const LocalId, next: CFStmtId) TrmcLirTestError!CFStmtId {
    return try lowLevelStmtWithUnique(store, target, op, args, 0, next);
}

fn lowLevelStmtWithUnique(store: *LirStore, target: LocalId, op: LowLevel, args: []const LocalId, unique_args: u64, next: CFStmtId) TrmcLirTestError!CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = try store.addLocalSpan(args),
        .unique_args = unique_args,
        .next = next,
    } });
}

/// Hand off a fresh join point id, mirroring how the real lowering allocates
/// them from a sequential counter (see arc.zig's next_join_point). Each proc
/// these tests build owns its own counter, so ids stay unique within a proc.
fn freshJoinPointId(next: *u32) LIR.JoinPointId {
    const id: LIR.JoinPointId = @enumFromInt(next.*);
    next.* += 1;
    return id;
}

fn runProcU64(allocator: Allocator, store: *const LirStore, layouts: *const layout.Store, proc: LIR.LirProcSpecId, runtime_env: *RuntimeHostEnv) TrmcLirTestError!u64 {
    var interp = try eval.Interpreter.init(allocator, store, layouts, runtime_env.get_ops(), .preserve);
    defer interp.deinit();
    const result = try interp.eval(.{ .proc_id = proc, .arg_layouts = &.{} });
    return result.value.read(u64);
}

test "ptr_alloca slot is zeroed and ptr_store/ptr_load round trip" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const ptr_u64 = try layouts.insertPtr(.u64);

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const slot = try b.addLocal(allocator, ptr_u64);
    const pre = try b.addLocal(allocator, .u64);
    const v = try b.addLocal(allocator, .u64);
    const st = try b.addLocal(allocator, .zst);
    const post = try b.addLocal(allocator, .u64);
    const sum = try b.addLocal(allocator, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    const add = try lowLevelStmt(&store, sum, .num_int_add_wrap, &.{ pre, post }, ret);
    const load_post = try lowLevelStmt(&store, post, .ptr_load, &.{slot}, add);
    const store_v = try lowLevelStmt(&store, st, .ptr_store, &.{ slot, v }, load_post);
    const v_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = v,
        .value = .{ .i64_literal = .{ .value = 41, .layout_idx = .u64 } },
        .next = store_v,
    } });
    // Loading before any store proves the alloca slot was zero-initialized.
    const load_pre = try lowLevelStmt(&store, pre, .ptr_load, &.{slot}, v_lit);
    const alloca = try lowLevelStmt(&store, slot, .ptr_alloca, &.{}, load_pre);
    const proc = try b.finishProc(&.{}, alloca, .u64);

    try std.testing.expectEqual(@as(u64, 41), try runProcU64(allocator, &store, &layouts, proc, &runtime_env));
    try runtime_env.checkForLeaks();
}

test "box_alloc_zeroed cell is zeroed, writable through ptr_cast, and freed by decref" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const box_u64 = try layouts.insertBox(.u64);
    const ptr_u64 = try layouts.insertPtr(.u64);

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const cell = try b.addLocal(allocator, box_u64);
    const p = try b.addLocal(allocator, ptr_u64);
    const pre = try b.addLocal(allocator, .u64);
    const v = try b.addLocal(allocator, .u64);
    const st = try b.addLocal(allocator, .zst);
    const post = try b.addLocal(allocator, .u64);
    const sum = try b.addLocal(allocator, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    const drop_cell = try store.addCFStmt(.{ .decref = .{
        .value = cell,
        .rc = .{ .concrete = .{ .op = .decref, .layout_idx = box_u64 } },
        .next = ret,
    } });
    const add = try lowLevelStmt(&store, sum, .num_int_add_wrap, &.{ pre, post }, drop_cell);
    const load_post = try lowLevelStmt(&store, post, .ptr_load, &.{p}, add);
    const store_v = try lowLevelStmt(&store, st, .ptr_store, &.{ p, v }, load_post);
    const v_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = v,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u64 } },
        .next = store_v,
    } });
    // Loading before any store proves the heap cell payload was zero-filled.
    const load_pre = try lowLevelStmt(&store, pre, .ptr_load, &.{p}, v_lit);
    const cast = try lowLevelStmt(&store, p, .ptr_cast, &.{cell}, load_pre);
    const alloc = try lowLevelStmt(&store, cell, .box_alloc_zeroed, &.{}, cast);
    const proc = try b.finishProc(&.{}, alloc, .u64);

    try std.testing.expectEqual(@as(u64, 7), try runProcU64(allocator, &store, &layouts, proc, &runtime_env));
    try std.testing.expectEqual(@as(u32, 1), runtime_env.allocationCallCount());
    try runtime_env.checkForLeaks();
}

test "box_prepare_update reuses a statically unique box" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const box_u64 = try layouts.insertBox(.u64);
    const ptr_u64 = try layouts.insertPtr(.u64);

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const initial = try b.addLocal(allocator, .u64);
    const replacement = try b.addLocal(allocator, .u64);
    const boxed = try b.addLocal(allocator, box_u64);
    const prepared = try b.addLocal(allocator, box_u64);
    const p = try b.addLocal(allocator, ptr_u64);
    const st = try b.addLocal(allocator, .zst);
    const loaded = try b.addLocal(allocator, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = loaded } });
    const drop_prepared = try store.addCFStmt(.{ .decref = .{
        .value = prepared,
        .rc = .{ .concrete = .{ .op = .decref, .layout_idx = box_u64 } },
        .next = ret,
    } });
    const load = try lowLevelStmt(&store, loaded, .ptr_load, &.{p}, drop_prepared);
    const store_replacement = try lowLevelStmt(&store, st, .ptr_store, &.{ p, replacement }, load);
    const replacement_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = replacement,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u64 } },
        .next = store_replacement,
    } });
    const cast = try lowLevelStmt(&store, p, .ptr_cast, &.{prepared}, replacement_lit);
    const prepare = try lowLevelStmtWithUnique(&store, prepared, .box_prepare_update, &.{boxed}, 1, cast);
    const box_initial = try lowLevelStmt(&store, boxed, .box_box, &.{initial}, prepare);
    const initial_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = initial,
        .value = .{ .i64_literal = .{ .value = 5, .layout_idx = .u64 } },
        .next = box_initial,
    } });
    const proc = try b.finishProc(&.{}, initial_lit, .u64);

    try std.testing.expectEqual(@as(u64, 7), try runProcU64(allocator, &store, &layouts, proc, &runtime_env));
    try std.testing.expectEqual(@as(u32, 1), runtime_env.allocationCallCount());
    try runtime_env.checkForLeaks();
}

test "box_prepare_update copies a shared box and leaves the original unchanged" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const box_u64 = try layouts.insertBox(.u64);
    const ptr_u64 = try layouts.insertPtr(.u64);

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const initial = try b.addLocal(allocator, .u64);
    const replacement = try b.addLocal(allocator, .u64);
    const boxed = try b.addLocal(allocator, box_u64);
    const prepared = try b.addLocal(allocator, box_u64);
    const old_p = try b.addLocal(allocator, ptr_u64);
    const new_p = try b.addLocal(allocator, ptr_u64);
    const st = try b.addLocal(allocator, .zst);
    const old_value = try b.addLocal(allocator, .u64);
    const new_value = try b.addLocal(allocator, .u64);
    const sum = try b.addLocal(allocator, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    const drop_boxed = try store.addCFStmt(.{ .decref = .{
        .value = boxed,
        .rc = .{ .concrete = .{ .op = .decref, .layout_idx = box_u64 } },
        .next = ret,
    } });
    const drop_prepared = try store.addCFStmt(.{ .decref = .{
        .value = prepared,
        .rc = .{ .concrete = .{ .op = .decref, .layout_idx = box_u64 } },
        .next = drop_boxed,
    } });
    const add = try lowLevelStmt(&store, sum, .num_int_add_wrap, &.{ old_value, new_value }, drop_prepared);
    const load_new = try lowLevelStmt(&store, new_value, .ptr_load, &.{new_p}, add);
    const load_old = try lowLevelStmt(&store, old_value, .ptr_load, &.{old_p}, load_new);
    const store_replacement = try lowLevelStmt(&store, st, .ptr_store, &.{ new_p, replacement }, load_old);
    const replacement_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = replacement,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u64 } },
        .next = store_replacement,
    } });
    const cast_new = try lowLevelStmt(&store, new_p, .ptr_cast, &.{prepared}, replacement_lit);
    const cast_old = try lowLevelStmt(&store, old_p, .ptr_cast, &.{boxed}, cast_new);
    const prepare = try lowLevelStmt(&store, prepared, .box_prepare_update, &.{boxed}, cast_old);
    const incref_boxed = try store.addCFStmt(.{ .incref = .{
        .value = boxed,
        .rc = .{ .concrete = .{ .op = .incref, .layout_idx = box_u64 } },
        .count = 1,
        .next = prepare,
    } });
    const box_initial = try lowLevelStmt(&store, boxed, .box_box, &.{initial}, incref_boxed);
    const initial_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = initial,
        .value = .{ .i64_literal = .{ .value = 5, .layout_idx = .u64 } },
        .next = box_initial,
    } });
    const proc = try b.finishProc(&.{}, initial_lit, .u64);

    try std.testing.expectEqual(@as(u64, 12), try runProcU64(allocator, &store, &layouts, proc, &runtime_env));
    try std.testing.expectEqual(@as(u32, 2), runtime_env.allocationCallCount());
    try runtime_env.checkForLeaks();
}

test "ptr ops round trip a multi-word payload through a heap cell" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    // A struct payload bigger than one word exercises the blob-copy paths
    // (TRMC stores whole union values through holes, not just words).
    const pair = try layouts.putRecord(&.{ layout.Layout.int(.u64), layout.Layout.int(.u64) });
    const box_pair = try layouts.insertBox(pair);
    const ptr_pair = try layouts.insertPtr(pair);

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const x = try b.addLocal(allocator, .u64);
    const y = try b.addLocal(allocator, .u64);
    const made = try b.addLocal(allocator, pair);
    const cell = try b.addLocal(allocator, box_pair);
    const p = try b.addLocal(allocator, ptr_pair);
    const st = try b.addLocal(allocator, .zst);
    const loaded = try b.addLocal(allocator, pair);
    const f0 = try b.addLocal(allocator, .u64);
    const f1 = try b.addLocal(allocator, .u64);
    const sum = try b.addLocal(allocator, .u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    const drop_cell = try store.addCFStmt(.{ .decref = .{
        .value = cell,
        .rc = .{ .concrete = .{ .op = .decref, .layout_idx = box_pair } },
        .next = ret,
    } });
    const add = try lowLevelStmt(&store, sum, .num_int_add_wrap, &.{ f0, f1 }, drop_cell);
    const get_f1 = try store.addCFStmt(.{ .assign_ref = .{
        .target = f1,
        .op = .{ .field = .{ .source = loaded, .field_idx = 1 } },
        .next = add,
    } });
    const get_f0 = try store.addCFStmt(.{ .assign_ref = .{
        .target = f0,
        .op = .{ .field = .{ .source = loaded, .field_idx = 0 } },
        .next = get_f1,
    } });
    const load_pair = try lowLevelStmt(&store, loaded, .ptr_load, &.{p}, get_f0);
    const store_pair = try lowLevelStmt(&store, st, .ptr_store, &.{ p, made }, load_pair);
    const cast = try lowLevelStmt(&store, p, .ptr_cast, &.{cell}, store_pair);
    const alloc = try lowLevelStmt(&store, cell, .box_alloc_zeroed, &.{}, cast);
    const make_pair = try store.addCFStmt(.{ .assign_struct = .{
        .target = made,
        .fields = try store.addLocalSpan(&.{ x, y }),
        .next = alloc,
    } });
    const y_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = y,
        .value = .{ .i64_literal = .{ .value = 1000, .layout_idx = .u64 } },
        .next = make_pair,
    } });
    const x_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = x,
        .value = .{ .i64_literal = .{ .value = 234, .layout_idx = .u64 } },
        .next = y_lit,
    } });
    const proc = try b.finishProc(&.{}, x_lit, .u64);

    try std.testing.expectEqual(@as(u64, 1234), try runProcU64(allocator, &store, &layouts, proc, &runtime_env));
    try runtime_env.checkForLeaks();
}

// ═══════════════════════════════════════════════════════════════════════════
// TRMC pass: detection + transform (hand-built LIR mirroring the verified
// solved_lir_lower shapes, run through Trmc.run + Arc.insert + interpreter)
// ═══════════════════════════════════════════════════════════════════════════

/// A "Peano list": U := [Nil, S(struct{ box(U) })]—the smallest recursive
/// tag union, built reserve-then-fill like the shared layout commit does.
const PeanoLayouts = struct {
    u: layout.Idx,
    box_u: layout.Idx,
    payload: layout.Idx,

    fn build(layouts: *layout.Store) TrmcLirTestError!PeanoLayouts {
        const u = try layouts.reserveLayout(layout.Layout.zst());
        const box_u = try layouts.insertBox(u);
        const payload = try layouts.putStructFields(&.{.{ .index = 0, .layout = box_u }});
        const tu = try layouts.putTagUnion(&.{ .zst, payload });
        layouts.updateLayout(u, layouts.getLayout(tu));
        return .{ .u = u, .box_u = box_u, .payload = payload };
    }

    /// Walk a U blob, counting S nodes until Nil. Variant/discriminant 0 is
    /// Nil, 1 is S; the box cell pointer is struct field 0 at payload offset 0.
    fn depth(self: PeanoLayouts, layouts: *const layout.Store, root: [*]const u8) usize {
        const u_layout = layouts.getLayout(self.u);
        const tu_data = layouts.getTagUnionData(u_layout.getTagUnion().idx);
        var count: usize = 0;
        var blob = root;
        while (true) {
            if (blob[tu_data.discriminant_offset.get(layouts.targetUsize())] == 0) return count;
            const cell = std.mem.readInt(usize, blob[0..@sizeOf(usize)], .little);
            if (cell == 0) @panic("null cell while walking a finished peano list");
            blob = @ptrFromInt(cell);
            count += 1;
        }
    }
};

/// The canonical lowered shape of
///   repeat = |n| match n { 0 => Nil, _ => S(repeat(n - 1)) }
/// - done-join with the result local as its param, branches lowering into it,
/// a box_box at the recursive field boundary, and a nominal alias hop.
fn buildRepeatProc(
    allocator: Allocator,
    b: *ProcBuilder,
    store: *LirStore,
    peano: PeanoLayouts,
) TrmcLirTestError!LIR.LirProcSpecId {
    const a_n = try b.addLocal(allocator, .u64);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{a_n}),
        .ret_layout = peano.u,
    });

    const res = try b.addLocal(allocator, peano.u);
    const zero = try b.addLocal(allocator, .u64);
    const cond = try b.addLocal(allocator, .bool);
    const one = try b.addLocal(allocator, .u64);
    const m = try b.addLocal(allocator, .u64);
    const r = try b.addLocal(allocator, peano.u);
    const cell = try b.addLocal(allocator, peano.box_u);
    const p = try b.addLocal(allocator, peano.payload);
    const t = try b.addLocal(allocator, peano.u);

    var next_join: u32 = 0;
    const join_id = freshJoinPointId(&next_join);
    const ret_res = try store.addCFStmt(.{ .ret = .{ .value = res } });

    // case n == 0: res = Nil; jump done
    const nil_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const nil_tag = try store.addCFStmt(.{ .assign_tag = .{
        .target = res,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = nil_jump,
    } });

    // default: res = S(repeat(n - 1)) via box_box + payload struct + alias hop
    const def_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = res,
        .op = .{ .local = t },
        .next = def_jump,
    } });
    const mk_tag = try store.addCFStmt(.{ .assign_tag = .{
        .target = t,
        .variant_index = 1,
        .discriminant = 1,
        .payload = p,
        .next = alias,
    } });
    const mk_payload = try store.addCFStmt(.{ .assign_struct = .{
        .target = p,
        .fields = try store.addLocalSpan(&.{cell}),
        .next = mk_tag,
    } });
    const mk_cell = try lowLevelStmt(store, cell, .box_box, &.{r}, mk_payload);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = r,
        .proc = proc,
        .args = try store.addLocalSpan(&.{m}),
        .next = mk_cell,
    } });
    const mk_m = try lowLevelStmt(store, m, .num_int_sub_wrap, &.{ a_n, one }, call);
    const mk_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = mk_m,
    } });

    const branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = nil_tag }});
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = branches,
        .default_branch = mk_one,
    } });
    const mk_cond = try lowLevelStmt(store, cond, .num_is_eq, &.{ a_n, zero }, switch_stmt);
    const mk_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = mk_cond,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{res}),
        .body = ret_res,
        .remainder = mk_zero,
    } });

    const proc_ptr = store.getProcSpecPtr(proc);
    proc_ptr.body = join;
    proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);
    return proc;
}

fn hasSelfCall(allocator: Allocator, store: *const LirStore, proc_id: LIR.LirProcSpecId) TrmcLirTestError!bool {
    var work = std.ArrayList(CFStmtId).empty;
    defer work.deinit(allocator);
    var visited = collections.DenseMap(CFStmtId, void).init(allocator);
    defer visited.deinit();

    const body = store.getProcSpec(proc_id).body orelse return false;
    try work.append(allocator, body);
    while (work.pop()) |stmt_id| {
        const entry = try visited.getOrPut(stmt_id);
        if (entry.found_existing) continue;
        switch (store.getCFStmt(stmt_id)) {
            .assign_call => |s| {
                if (s.proc == proc_id) return true;
                try work.append(allocator, s.next);
            },
            .join => |s| {
                try work.append(allocator, s.body);
                try work.append(allocator, s.remainder);
            },
            .switch_stmt => |s| {
                const branches = store.getCFSwitchBranches(s.branches);
                for (0..branches.len) |i| {
                    const branch = GuardedList.at(branches, i);
                    try work.append(allocator, branch.body);
                }
                try work.append(allocator, s.default_branch);
                if (s.continuation) |continuation| try work.append(allocator, continuation);
            },
            .switch_initialized_payload => |s| {
                try work.append(allocator, s.initialized_branch);
                try work.append(allocator, s.uninitialized_branch);
            },
            .str_match => |s| {
                try work.append(allocator, s.on_match);
                try work.append(allocator, s.on_miss);
            },
            .str_match_set => |s| {
                const arms = store.getStrMatchArms(s.arms);
                for (0..arms.len) |i| {
                    const arm = GuardedList.at(arms, i);
                    try work.append(allocator, arm.on_match);
                }
                try work.append(allocator, s.on_miss);
            },
            .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            .boxy_tag_match => |s| {
                try work.append(allocator, s.on_match);
                try work.append(allocator, s.on_miss);
            },
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict => |s| {
                try work.append(allocator, s.next);
            },
        }
    }
    return false;
}

fn runProcU64Args(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout.Store,
    proc: LIR.LirProcSpecId,
    runtime_env: *RuntimeHostEnv,
    args: []const u64,
) TrmcLirTestError!u64 {
    var interp = try eval.Interpreter.init(allocator, store, layouts, runtime_env.get_ops(), .preserve);
    defer interp.deinit();
    const arg_layouts = try allocator.alloc(layout.Idx, args.len);
    defer allocator.free(arg_layouts);
    @memset(arg_layouts, .u64);
    const packed_args = try allocator.dupe(u64, args);
    defer allocator.free(packed_args);
    const result = try interp.eval(.{
        .proc_id = proc,
        .arg_layouts = arg_layouts,
        .arg_ptr = @ptrCast(packed_args.ptr),
    });
    return result.value.read(u64);
}

test "trmc transforms the canonical repeat shape and builds correct structure" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const peano = try PeanoLayouts.build(&layouts);
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const repeat = try buildRepeatProc(allocator, &b, &store, peano);

    try lir.Trmc.run(&store, &layouts);

    try std.testing.expectEqual(LIR.TailTransform.trmc, store.getProcSpec(repeat).tail_transform);
    try std.testing.expect(!(try hasSelfCall(allocator, &store, repeat)));

    try lir.Arc.insert(&store, &layouts, .{});

    var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops(), .preserve);
    defer interp.deinit();
    for ([_]u64{ 0, 1, 3 }) |n| {
        var arg = n;
        const result = try interp.eval(.{
            .proc_id = repeat,
            .arg_layouts = &.{.u64},
            .arg_ptr = @ptrCast(&arg),
        });
        try std.testing.expectEqual(@as(usize, n), peano.depth(&layouts, result.value.ptr));
    }
}

test "trmc'd repeat escapes the interpreter call-depth cap" {
    const allocator = std.testing.allocator;

    // The interpreter's artificial call-depth cap is a Debug-only diagnostic.
    // In Debug, verify the untransformed control case exceeds that cap.
    if (comptime @import("builtin").mode == .Debug) {
        var store = LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
        defer layouts.deinit();
        var runtime_env = RuntimeHostEnv.init(allocator);
        defer runtime_env.deinit();

        const peano = try PeanoLayouts.build(&layouts);
        var b = ProcBuilder.init(&store);
        defer b.deinit(allocator);
        const repeat = try buildRepeatProc(allocator, &b, &store, peano);
        try lir.Arc.insert(&store, &layouts, .{});

        var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops(), .preserve);
        defer interp.deinit();
        var arg: u64 = 2000;
        try std.testing.expectError(error.Crash, interp.eval(.{
            .proc_id = repeat,
            .arg_layouts = &.{.u64},
            .arg_ptr = @ptrCast(&arg),
        }));
    }

    // With TRMC the same depth completes in one frame.
    {
        var store = LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
        defer layouts.deinit();
        var runtime_env = RuntimeHostEnv.init(allocator);
        defer runtime_env.deinit();

        const peano = try PeanoLayouts.build(&layouts);
        var b = ProcBuilder.init(&store);
        defer b.deinit(allocator);
        const repeat = try buildRepeatProc(allocator, &b, &store, peano);
        try lir.Trmc.run(&store, &layouts);
        try lir.Arc.insert(&store, &layouts, .{});

        var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops(), .preserve);
        defer interp.deinit();
        var arg: u64 = 2000;
        const result = try interp.eval(.{
            .proc_id = repeat,
            .arg_layouts = &.{.u64},
            .arg_ptr = @ptrCast(&arg),
        });
        try std.testing.expectEqual(@as(usize, 2000), peano.depth(&layouts, result.value.ptr));
    }
}

test "trmc'd repeat is leak-free and allocation-exact when consumed" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const peano = try PeanoLayouts.build(&layouts);
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const repeat = try buildRepeatProc(allocator, &b, &store, peano);

    // Root proc: build the list, return 0. ARC inserts the drop of the dead
    // list, so a leak or double-free here is a TRMC/ARC interaction bug.
    const root_n = try b.addLocal(allocator, .u64);
    const list = try b.addLocal(allocator, peano.u);
    const root_zero = try b.addLocal(allocator, .u64);
    const root_ret = try store.addCFStmt(.{ .ret = .{ .value = root_zero } });
    const mk_root_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = root_zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = root_ret,
    } });
    const call_repeat = try store.addCFStmt(.{ .assign_call = .{
        .target = list,
        .proc = repeat,
        .args = try store.addLocalSpan(&.{root_n}),
        .next = mk_root_zero,
    } });
    const root = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{root_n}),
        .body = call_repeat,
        .ret_layout = .u64,
    });
    store.getProcSpecPtr(root).frame_locals = try store.addLocalSpan(&.{ root_n, list, root_zero });

    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqual(LIR.TailTransform.trmc, store.getProcSpec(repeat).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(root).tail_transform);
    try lir.Arc.insert(&store, &layouts, .{});

    const n: u64 = 50;
    try std.testing.expectEqual(@as(u64, 0), try runProcU64Args(allocator, &store, &layouts, root, &runtime_env, &.{n}));
    // Exactly one heap cell per S node—TRMC must not change allocation counts.
    try std.testing.expectEqual(@as(u32, n), runtime_env.allocationCallCount());
    try runtime_env.checkForLeaks();
}

/// Direct-ret tail-recursive countdown:
///   count = |n, acc| if n == 0 { acc } else { count(n - 1, acc + 1) }
fn buildCountdownProc(allocator: Allocator, b: *ProcBuilder, store: *LirStore) TrmcLirTestError!LIR.LirProcSpecId {
    const a_n = try b.addLocal(allocator, .u64);
    const a_acc = try b.addLocal(allocator, .u64);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ a_n, a_acc }),
        .ret_layout = .u64,
    });
    var tail_builder = lir.TailCallBuilder.init(allocator, proc);
    defer tail_builder.deinit();
    store.tail_call_builder = &tail_builder;
    defer store.tail_call_builder = null;

    const zero = try b.addLocal(allocator, .u64);
    const cond = try b.addLocal(allocator, .bool);
    const one = try b.addLocal(allocator, .u64);
    const m = try b.addLocal(allocator, .u64);
    const acc2 = try b.addLocal(allocator, .u64);
    const r = try b.addLocal(allocator, .u64);

    const ret_acc = try store.addCFStmt(.{ .ret = .{ .value = a_acc } });
    const ret_r = try store.addCFStmt(.{ .ret = .{ .value = r } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = r,
        .proc = proc,
        .args = try store.addLocalSpan(&.{ m, acc2 }),
        .next = ret_r,
    } });
    const mk_acc2 = try lowLevelStmt(store, acc2, .num_int_add_wrap, &.{ a_acc, one }, call);
    const mk_m = try lowLevelStmt(store, m, .num_int_sub_wrap, &.{ a_n, one }, mk_acc2);
    const mk_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = mk_m,
    } });
    const branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = ret_acc }});
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = branches,
        .default_branch = mk_one,
    } });
    const mk_cond = try lowLevelStmt(store, cond, .num_is_eq, &.{ a_n, zero }, switch_stmt);
    const mk_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = mk_cond,
    } });

    const proc_ptr = store.getProcSpecPtr(proc);
    proc_ptr.body = mk_zero;
    proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);
    const tail_sites = try tail_builder.finish(store);
    store.getProcSpecPtr(proc).tail_calls = tail_sites;
    store.tail_call_builder = null;
    return proc;
}

test "tce transforms the countdown shape and escapes the call-depth cap" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const count = try buildCountdownProc(allocator, &b, &store);

    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqual(LIR.TailTransform.tce, store.getProcSpec(count).tail_transform);
    try std.testing.expect(!(try hasSelfCall(allocator, &store, count)));
    try lir.Arc.insert(&store, &layouts, .{});

    try std.testing.expectEqual(
        @as(u64, 100_000),
        try runProcU64Args(allocator, &store, &layouts, count, &runtime_env, &.{ 100_000, 0 }),
    );
    try runtime_env.checkForLeaks();
}

test "tce loop-back copies swapped params through temps" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);

    // swap = |a, b, n| if n == 0 { a } else { swap(b, a, n - 1) }
    const a_a = try b.addLocal(allocator, .u64);
    const a_b = try b.addLocal(allocator, .u64);
    const a_n = try b.addLocal(allocator, .u64);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ a_a, a_b, a_n }),
        .ret_layout = .u64,
    });
    var tail_builder = lir.TailCallBuilder.init(allocator, proc);
    defer tail_builder.deinit();
    store.tail_call_builder = &tail_builder;
    defer store.tail_call_builder = null;

    const zero = try b.addLocal(allocator, .u64);
    const cond = try b.addLocal(allocator, .bool);
    const one = try b.addLocal(allocator, .u64);
    const m = try b.addLocal(allocator, .u64);
    const r = try b.addLocal(allocator, .u64);

    const ret_a = try store.addCFStmt(.{ .ret = .{ .value = a_a } });
    const ret_r = try store.addCFStmt(.{ .ret = .{ .value = r } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = r,
        .proc = proc,
        .args = try store.addLocalSpan(&.{ a_b, a_a, m }),
        .next = ret_r,
    } });
    const mk_m = try lowLevelStmt(&store, m, .num_int_sub_wrap, &.{ a_n, one }, call);
    const mk_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = mk_m,
    } });
    const branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = ret_a }});
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = branches,
        .default_branch = mk_one,
    } });
    const mk_cond = try lowLevelStmt(&store, cond, .num_is_eq, &.{ a_n, zero }, switch_stmt);
    const mk_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = mk_cond,
    } });
    const proc_ptr = store.getProcSpecPtr(proc);
    proc_ptr.body = mk_zero;
    proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);

    const tail_sites = try tail_builder.finish(&store);
    store.getProcSpecPtr(proc).tail_calls = tail_sites;
    store.tail_call_builder = null;
    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqual(LIR.TailTransform.tce, store.getProcSpec(proc).tail_transform);
    try lir.Arc.insert(&store, &layouts, .{});

    // Odd swap count returns the second argument; even returns the first.
    // Sequential param writes without temp copies would return garbage.
    try std.testing.expectEqual(@as(u64, 2), try runProcU64Args(allocator, &store, &layouts, proc, &runtime_env, &.{ 1, 2, 101 }));
    try std.testing.expectEqual(@as(u64, 1), try runProcU64Args(allocator, &store, &layouts, proc, &runtime_env, &.{ 1, 2, 100 }));
}

test "mixed construct and plain-tail branches both become jumps" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const peano = try PeanoLayouts.build(&layouts);
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);

    // weird = |n| match n { 0 => Nil, odd => weird(n - 1), even => S(weird(n - 1)) }
    // (the filter shape: one TRMC branch plus one plain tail call)
    const a_n = try b.addLocal(allocator, .u64);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{a_n}),
        .ret_layout = peano.u,
    });
    var tail_builder = lir.TailCallBuilder.init(allocator, proc);
    defer tail_builder.deinit();
    store.tail_call_builder = &tail_builder;
    defer store.tail_call_builder = null;

    const res = try b.addLocal(allocator, peano.u);
    const zero = try b.addLocal(allocator, .u64);
    const is_zero = try b.addLocal(allocator, .bool);
    const one = try b.addLocal(allocator, .u64);
    const two = try b.addLocal(allocator, .u64);
    const rem = try b.addLocal(allocator, .u64);
    const m_tail = try b.addLocal(allocator, .u64);
    const m_cons = try b.addLocal(allocator, .u64);
    const r_cons = try b.addLocal(allocator, peano.u);
    const cell = try b.addLocal(allocator, peano.box_u);
    const p = try b.addLocal(allocator, peano.payload);
    const t = try b.addLocal(allocator, peano.u);

    var next_join: u32 = 0;
    const join_id = freshJoinPointId(&next_join);
    const ret_res = try store.addCFStmt(.{ .ret = .{ .value = res } });

    // n == 0: res = Nil
    const nil_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const nil_tag = try store.addCFStmt(.{ .assign_tag = .{
        .target = res,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = nil_jump,
    } });

    // odd: res = weird(n - 1)—plain tail call targeting the done-join param
    const tail_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const tail_call = try store.addCFStmt(.{ .assign_call = .{
        .target = res,
        .proc = proc,
        .args = try store.addLocalSpan(&.{m_tail}),
        .next = tail_jump,
    } });
    // Two edges enter the exact same plain-tail call inside a TRMC procedure.
    const shared_tail = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = is_zero,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = tail_call }}),
        .default_branch = tail_call,
    } });
    const mk_m_tail = try lowLevelStmt(&store, m_tail, .num_int_sub_wrap, &.{ a_n, one }, shared_tail);

    // even: res = S(weird(n - 1))—the construct branch
    const cons_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = res,
        .op = .{ .local = t },
        .next = cons_jump,
    } });
    const mk_tag = try store.addCFStmt(.{ .assign_tag = .{
        .target = t,
        .variant_index = 1,
        .discriminant = 1,
        .payload = p,
        .next = alias,
    } });
    const mk_payload = try store.addCFStmt(.{ .assign_struct = .{
        .target = p,
        .fields = try store.addLocalSpan(&.{cell}),
        .next = mk_tag,
    } });
    const mk_cell = try lowLevelStmt(&store, cell, .box_box, &.{r_cons}, mk_payload);
    const cons_call = try store.addCFStmt(.{ .assign_call = .{
        .target = r_cons,
        .proc = proc,
        .args = try store.addLocalSpan(&.{m_cons}),
        .next = mk_cell,
    } });
    const mk_m_cons = try lowLevelStmt(&store, m_cons, .num_int_sub_wrap, &.{ a_n, one }, cons_call);

    // odd/even dispatch
    const parity_branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = mk_m_tail }});
    const parity_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = rem,
        .branches = parity_branches,
        .default_branch = mk_m_cons,
    } });
    const mk_rem = try lowLevelStmt(&store, rem, .num_rem_by, &.{ a_n, two }, parity_switch);
    const mk_two = try store.addCFStmt(.{ .assign_literal = .{
        .target = two,
        .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u64 } },
        .next = mk_rem,
    } });
    const mk_one = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = mk_two,
    } });

    // n == 0 dispatch
    const zero_branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = nil_tag }});
    const zero_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = is_zero,
        .branches = zero_branches,
        .default_branch = mk_one,
    } });
    const mk_is_zero = try lowLevelStmt(&store, is_zero, .num_is_eq, &.{ a_n, zero }, zero_switch);
    const mk_zero = try store.addCFStmt(.{ .assign_literal = .{
        .target = zero,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = mk_is_zero,
    } });
    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{res}),
        .body = ret_res,
        .remainder = mk_zero,
    } });
    const proc_ptr = store.getProcSpecPtr(proc);
    proc_ptr.body = join;
    proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);

    const tail_sites = try tail_builder.finish(&store);
    store.getProcSpecPtr(proc).tail_calls = tail_sites;
    store.tail_call_builder = null;
    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqual(LIR.TailTransform.trmc, store.getProcSpec(proc).tail_transform);
    try std.testing.expect(!(try hasSelfCall(allocator, &store, proc)));
    try lir.Arc.insert(&store, &layouts, .{});

    // Depth 2000 makes 2000 recursive calls (1000 S nodes): both rewritten
    // branches must loop for this to survive the 1024-frame cap.
    var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops(), .preserve);
    defer interp.deinit();
    var arg: u64 = 2000;
    const result = try interp.eval(.{
        .proc_id = proc,
        .arg_layouts = &.{.u64},
        .arg_ptr = @ptrCast(&arg),
    });
    try std.testing.expectEqual(@as(usize, 1000), peano.depth(&layouts, result.value.ptr));
}

fn dumpProcText(allocator: Allocator, store: *const LirStore, layouts: *const layout.Store, proc: LIR.LirProcSpecId) TrmcLirTestError![]u8 {
    var buffer: std.Io.Writer.Allocating = .init(allocator);
    defer buffer.deinit();
    try lir.DebugPrint.writeProc(allocator, store, layouts, proc, &buffer.writer);
    return try allocator.dupe(u8, buffer.written());
}

test "golden: repeat IR before and after the trmc transform" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();

    const peano = try PeanoLayouts.build(&layouts);
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const repeat = try buildRepeatProc(allocator, &b, &store, peano);

    const before = try dumpProcText(allocator, &store, &layouts, repeat);
    defer allocator.free(before);
    try std.testing.expectEqualStrings(
        \\proc p0 args=[l0:u64] ret=tag_union#25
        \\  join j0 params=[l1]
        \\    remainder:
        \\      l2:u64 = literal 0
        \\      l3:bool = low_level num_is_eq(l0, l2)
        \\      switch l3 default_cold=false
        \\        case 1:
        \\          l1:tag_union#25 = tag v0 d0
        \\          jump j0
        \\        default:
        \\          l4:u64 = literal 1
        \\          l5:u64 = low_level num_int_sub_wrap(l0, l4)
        \\          l6:tag_union#25 = call p0(l5)
        \\          l7:box#26 = low_level box_box(l6)
        \\          l8:struct_#27 = struct(l7)
        \\          l9:tag_union#25 = tag v1 d1 (l8)
        \\          l1:tag_union#25 = ref.local l9
        \\          jump j0
        \\    body:
        \\      ret l1
        \\
    , before);

    try lir.Trmc.run(&store, &layouts);

    const after = try dumpProcText(allocator, &store, &layouts, repeat);
    defer allocator.free(after);
    try std.testing.expectEqualStrings(
        \\proc p0 args=[l17:u64] ret=tag_union#25 transform=trmc
        \\  join j1 params=[l0, l10, l11]
        \\    remainder:
        \\      l12:ptr#29 = low_level ptr_alloca()
        \\      set l0 := l17 (initialize_join_param)
        \\      set l10 := l12 (initialize_join_param)
        \\      set l11 := l12 (initialize_join_param)
        \\      jump j1
        \\    body:
        \\      join j0 params=[l1]
        \\        remainder:
        \\          l2:u64 = literal 0
        \\          l3:bool = low_level num_is_eq(l0, l2)
        \\          switch l3 default_cold=false
        \\            case 1:
        \\              l1:tag_union#25 = tag v0 d0
        \\              jump j0
        \\            default:
        \\              l4:u64 = literal 1
        \\              l5:u64 = low_level num_int_sub_wrap(l0, l4)
        \\              l7:box#26 = low_level box_alloc_zeroed()
        \\              l15:ptr#29 = low_level ptr_cast(l7)
        \\              l8:struct_#27 = struct(l7)
        \\              l9:tag_union#25 = tag v1 d1 (l8)
        \\              l16:zst = low_level ptr_store(l10, l9)
        \\              set l10 := l15 (initialize_join_param)
        \\              set l0 := l5 (initialize_join_param)
        \\              jump j1
        \\        body:
        \\          l14:zst = low_level ptr_store(l10, l1)
        \\          l13:tag_union#25 = low_level ptr_load(l11)
        \\          ret l13
        \\
    , after);
}

test "tail eligibility distinguishes result uses and mutual recursion from shared tails" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();

    const peano = try PeanoLayouts.build(&layouts);
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);

    // (1) Result boxed twice: the second box_box is a disqualifying use.
    const used_twice = blk: {
        const a_n = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{a_n}),
            .ret_layout = peano.u,
        });
        const r = try b.addLocal(allocator, peano.u);
        const cell = try b.addLocal(allocator, peano.box_u);
        const cell2 = try b.addLocal(allocator, peano.box_u);
        const p = try b.addLocal(allocator, peano.payload);
        const t = try b.addLocal(allocator, peano.u);
        const ret_t = try store.addCFStmt(.{ .ret = .{ .value = t } });
        const mk_tag = try store.addCFStmt(.{ .assign_tag = .{ .target = t, .variant_index = 1, .discriminant = 1, .payload = p, .next = ret_t } });
        const mk_p = try store.addCFStmt(.{ .assign_struct = .{ .target = p, .fields = try store.addLocalSpan(&.{cell}), .next = mk_tag } });
        const mk_cell2 = try lowLevelStmt(&store, cell2, .box_box, &.{r}, mk_p);
        const mk_cell = try lowLevelStmt(&store, cell, .box_box, &.{r}, mk_cell2);
        const call = try store.addCFStmt(.{ .assign_call = .{ .target = r, .proc = proc, .args = try store.addLocalSpan(&.{a_n}), .next = mk_cell } });
        const proc_ptr = store.getProcSpecPtr(proc);
        proc_ptr.body = call;
        proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);
        break :blk proc;
    };

    // (2) Constructor built but a different value returned.
    const not_returned = blk: {
        const a_n = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{a_n}),
            .ret_layout = peano.u,
        });
        const r = try b.addLocal(allocator, peano.u);
        const cell = try b.addLocal(allocator, peano.box_u);
        const p = try b.addLocal(allocator, peano.payload);
        const t = try b.addLocal(allocator, peano.u);
        const other = try b.addLocal(allocator, peano.u);
        const ret_other = try store.addCFStmt(.{ .ret = .{ .value = other } });
        const mk_other = try store.addCFStmt(.{ .assign_tag = .{ .target = other, .variant_index = 0, .discriminant = 0, .payload = null, .next = ret_other } });
        const mk_tag = try store.addCFStmt(.{ .assign_tag = .{ .target = t, .variant_index = 1, .discriminant = 1, .payload = p, .next = mk_other } });
        const mk_p = try store.addCFStmt(.{ .assign_struct = .{ .target = p, .fields = try store.addLocalSpan(&.{cell}), .next = mk_tag } });
        const mk_cell = try lowLevelStmt(&store, cell, .box_box, &.{r}, mk_p);
        const call = try store.addCFStmt(.{ .assign_call = .{ .target = r, .proc = proc, .args = try store.addLocalSpan(&.{a_n}), .next = mk_cell } });
        const proc_ptr = store.getProcSpecPtr(proc);
        proc_ptr.body = call;
        proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);
        break :blk proc;
    };

    // (3) Non-union return whose recursive result feeds arithmetic (fib shape).
    const fib_like = blk: {
        const a_n = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{a_n}),
            .ret_layout = .u64,
        });
        const one = try b.addLocal(allocator, .u64);
        const m = try b.addLocal(allocator, .u64);
        const r = try b.addLocal(allocator, .u64);
        const s = try b.addLocal(allocator, .u64);
        const ret_s = try store.addCFStmt(.{ .ret = .{ .value = s } });
        const mk_s = try lowLevelStmt(&store, s, .num_int_add_wrap, &.{ r, one }, ret_s);
        const call = try store.addCFStmt(.{ .assign_call = .{ .target = r, .proc = proc, .args = try store.addLocalSpan(&.{m}), .next = mk_s } });
        const mk_m = try lowLevelStmt(&store, m, .num_int_sub_wrap, &.{ a_n, one }, call);
        const mk_one = try store.addCFStmt(.{ .assign_literal = .{ .target = one, .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } }, .next = mk_m } });
        const proc_ptr = store.getProcSpecPtr(proc);
        proc_ptr.body = mk_one;
        proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);
        break :blk proc;
    };

    // (4) Mutual recursion: tail calls, but never to self.
    const mutual_a = try store.addProcSpec(.{ .name = store.freshSyntheticSymbol(), .args = LIR.LocalSpan.empty(), .ret_layout = .u64 });
    const mutual_b = try store.addProcSpec(.{ .name = store.freshSyntheticSymbol(), .args = LIR.LocalSpan.empty(), .ret_layout = .u64 });
    {
        const ra = try b.addLocal(allocator, .u64);
        const ret_ra = try store.addCFStmt(.{ .ret = .{ .value = ra } });
        const call_b = try store.addCFStmt(.{ .assign_call = .{ .target = ra, .proc = mutual_b, .args = try store.addLocalSpan(&.{}), .next = ret_ra } });
        const pa = store.getProcSpecPtr(mutual_a);
        pa.body = call_b;
        pa.frame_locals = try store.addLocalSpan(&.{ra});

        const rb = try b.addLocal(allocator, .u64);
        const ret_rb = try store.addCFStmt(.{ .ret = .{ .value = rb } });
        const call_a = try store.addCFStmt(.{ .assign_call = .{ .target = rb, .proc = mutual_a, .args = try store.addLocalSpan(&.{}), .next = ret_rb } });
        const pb = store.getProcSpecPtr(mutual_b);
        pb.body = call_a;
        pb.frame_locals = try store.addLocalSpan(&.{rb});
    }

    // (5) Two control-flow edges share the same recursive tail. TRMC/TCE
    // replaces the call node uniformly, so both edges must become back-edges.
    const shared_tail = blk: {
        const a_n = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{a_n}),
            .ret_layout = .u64,
        });
        var tail_builder = lir.TailCallBuilder.init(allocator, proc);
        defer tail_builder.deinit();
        store.tail_call_builder = &tail_builder;
        defer store.tail_call_builder = null;
        const cond = try b.addLocal(allocator, .bool);
        const r = try b.addLocal(allocator, .u64);
        const ret_r = try store.addCFStmt(.{ .ret = .{ .value = r } });
        const call = try store.addCFStmt(.{ .assign_call = .{
            .target = r,
            .proc = proc,
            .args = try store.addLocalSpan(&.{a_n}),
            .next = ret_r,
        } });
        const branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = call }});
        const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = branches,
            .default_branch = call,
        } });
        const proc_ptr = store.getProcSpecPtr(proc);
        proc_ptr.body = switch_stmt;
        proc_ptr.frame_locals = try store.addLocalSpan(b.locals.items);
        const sites = try tail_builder.finish(&store);
        store.getProcSpecPtr(proc).tail_calls = sites;
        break :blk proc;
    };

    try lir.Trmc.run(&store, &layouts);

    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(used_twice).tail_transform);
    try std.testing.expect(try hasSelfCall(allocator, &store, used_twice));
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(not_returned).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(fib_like).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(mutual_a).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(mutual_b).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.tce, store.getProcSpec(shared_tail).tail_transform);
    try std.testing.expect(!(try hasSelfCall(allocator, &store, shared_tail)));
}

test "tce has no site or forwarding-depth cap and preserves a shared base return" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const n = try b.addLocal(allocator, .u64);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{n}),
        .ret_layout = .u64,
    });
    var builder = lir.TailCallBuilder.init(allocator, proc);
    defer builder.deinit();
    store.tail_call_builder = &builder;
    defer store.tail_call_builder = null;

    const result = try b.addLocal(allocator, .u64);
    const one = try b.addLocal(allocator, .u64);
    const m = try b.addLocal(allocator, .u64);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    // A long forwarding join chain is shared by every call and the base case.
    var entry_jumps: [128]CFStmtId = undefined;
    for (&entry_jumps, 0..) |*entry, index| {
        entry.* = try store.addCFStmt(.{ .jump = .{ .target = @enumFromInt(index) } });
    }
    const shared = entry_jumps[entry_jumps.len - 1];
    const base_case = try store.addCFStmt(.{ .assign_literal = .{
        .target = result,
        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } },
        .next = shared,
    } });
    var branches: [96]LIR.CFSwitchBranch = undefined;
    for (&branches, 0..) |*branch, index| {
        branch.* = .{ .value = index + 1, .body = try store.addCFStmt(.{ .assign_call = .{
            .target = result,
            .proc = proc,
            .args = try store.addLocalSpan(&.{m}),
            .next = shared,
        } }) };
    }
    const dispatch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = n,
        .branches = try store.addCFSwitchBranches(&branches),
        .default_branch = base_case,
    } });
    // Put the dispatch under all forwarding joins' lexical scopes.
    var body = dispatch;
    for (0..entry_jumps.len) |offset| {
        const index = entry_jumps.len - 1 - offset;
        body = try store.addCFStmt(.{ .join = .{
            .id = @enumFromInt(index),
            .params = try store.addLocalSpan(&.{result}),
            .body = if (index == 0) ret else entry_jumps[index - 1],
            .remainder = body,
        } });
    }
    body = try lowLevelStmt(&store, m, .num_int_sub_wrap, &.{ n, one }, body);
    body = try store.addCFStmt(.{ .assign_literal = .{
        .target = one,
        .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
        .next = body,
    } });
    store.getProcSpecPtr(proc).body = body;
    store.getProcSpecPtr(proc).frame_locals = try store.addLocalSpan(b.locals.items);
    const sites = try builder.finish(&store);
    store.getProcSpecPtr(proc).tail_calls = sites;
    store.tail_call_builder = null;
    const frame_before = store.getProcSpec(proc).frame_locals;
    const stmts_before = store.cfStmtCount();
    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqualDeep(frame_before, store.getProcSpec(proc).frame_locals);
    // Each call row becomes a parameter write and needs only a fresh jump.
    // The loop entry adds one join and one jump, with no orphan heads.
    try std.testing.expectEqual(branches.len + 2, store.cfStmtCount() - stmts_before);
    try std.testing.expectEqual(LIR.TailTransform.tce, store.getProcSpec(proc).tail_transform);
    try std.testing.expect(!(try hasSelfCall(allocator, &store, proc)));
    try std.testing.expect(store.getCFStmt(ret) == .ret);
    try lir.Arc.insert(&store, &layouts, .{});
    try std.testing.expectEqual(@as(u64, 0), try runProcU64Args(allocator, &store, &layouts, proc, &runtime_env, &.{96}));
    try runtime_env.checkForLeaks();
}

test "tail-call proof rejects an intervening effect and a forwarding cycle" {
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |cycle| {
        var store = LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
        defer layouts.deinit();
        var b = ProcBuilder.init(&store);
        defer b.deinit(allocator);
        const cond = try b.addLocal(allocator, .bool);
        const result = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{cond}),
            .ret_layout = .u64,
        });
        var builder = lir.TailCallBuilder.init(allocator, proc);
        defer builder.deinit();
        store.tail_call_builder = &builder;
        defer store.tail_call_builder = null;
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
        var next_join: u32 = 0;
        const join_id = freshJoinPointId(&next_join);
        const suffix = if (cycle)
            try store.addCFStmt(.{ .jump = .{ .target = join_id } })
        else
            try store.addCFStmt(.{ .expect = .{ .condition = cond, .next = ret } });
        const call = try store.addCFStmt(.{ .assign_call = .{
            .target = result,
            .proc = proc,
            .args = try store.addLocalSpan(&.{cond}),
            .next = suffix,
        } });
        const body = if (cycle) try store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = LIR.LocalSpan.empty(),
            .body = suffix,
            .remainder = call,
        } }) else call;
        store.getProcSpecPtr(proc).body = body;
        store.getProcSpecPtr(proc).frame_locals = try store.addLocalSpan(b.locals.items);
        const sites = try builder.finish(&store);
        try std.testing.expect(sites == null);
        store.tail_call_builder = null;
        try lir.Trmc.run(&store, &layouts);
        try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(proc).tail_transform);
        try std.testing.expect(try hasSelfCall(allocator, &store, proc));
    }
}

test "tail-call proof consumes the explicit boxy adapter operation" {
    const allocator = std.testing.allocator;
    for ([_]lir.Program.BoxyAdapterOperation{ .relabel, .materialize }) |operation| {
        var store = LirStore.init(allocator);
        defer store.deinit();
        var b = ProcBuilder.init(&store);
        defer b.deinit(allocator);
        const arg = try b.addLocal(allocator, .u64);
        const result = try b.addLocal(allocator, .u64);
        const forwarded = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{arg}),
            .ret_layout = .u64,
        });
        var builder = lir.TailCallBuilder.init(allocator, proc);
        defer builder.deinit();
        store.tail_call_builder = &builder;
        defer store.tail_call_builder = null;
        var adapters: std.ArrayList(lir.Program.BoxyAdapter) = .empty;
        defer adapters.deinit(allocator);
        const adapter_id: LIR.BoxyAdapterId = @enumFromInt(adapters.items.len);
        try adapters.append(allocator, .{
            .kind = .boxy_to_boxy,
            .operation = operation,
            .source_layout = .u64,
            .target_layout = .u64,
            .consumes_source = true,
            .produces_owned_result = true,
        });
        builder.adapters = adapters.items;
        const ret = try store.addCFStmt(.{ .ret = .{ .value = forwarded } });
        const adapt = try store.addCFStmt(.{ .assign_boxy_adapt = .{
            .source = result,
            .target = forwarded,
            .source_desc = null,
            .target_desc = null,
            .source_mode = .move,
            .adapter = adapter_id,
            .next = ret,
        } });
        const call = try store.addCFStmt(.{ .assign_call = .{
            .target = result,
            .proc = proc,
            .args = try store.addLocalSpan(&.{arg}),
            .next = adapt,
        } });
        store.getProcSpecPtr(proc).body = call;
        const sites = try builder.finish(&store);
        try std.testing.expectEqual(operation == .relabel, sites != null);
    }
}

test "tce parallel transfers preserve every small source graph" {
    const allocator = std.testing.allocator;
    // Each of three destinations can read any old parameter or a fresh value.
    // Exhausting this space covers identity writes, chains, cycles, and fanout.
    for (0..64) |encoding| {
        var store = LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
        defer layouts.deinit();
        var runtime_env = RuntimeHostEnv.init(allocator);
        defer runtime_env.deinit();
        var b = ProcBuilder.init(&store);
        defer b.deinit(allocator);
        var args: [4]LocalId = undefined;
        for (&args) |*arg| arg.* = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&args),
            .ret_layout = .u64,
        });
        var builder = lir.TailCallBuilder.init(allocator, proc);
        defer builder.deinit();
        store.tail_call_builder = &builder;
        defer store.tail_call_builder = null;
        const result = try b.addLocal(allocator, .u64);
        const one = try b.addLocal(allocator, .u64);
        const fresh = try b.addLocal(allocator, .u64);
        const remaining = try b.addLocal(allocator, .u64);
        const radix = try b.addLocal(allocator, .u64);
        const scaled = try b.addLocal(allocator, .u64);
        const sum = try b.addLocal(allocator, .u64);
        const scaled_again = try b.addLocal(allocator, .u64);
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
        var base_case = try lowLevelStmt(&store, result, .num_int_add_wrap, &.{ args[0], scaled_again }, ret);
        base_case = try lowLevelStmt(&store, scaled_again, .num_int_mul_wrap, &.{ sum, radix }, base_case);
        base_case = try lowLevelStmt(&store, sum, .num_int_add_wrap, &.{ args[1], scaled }, base_case);
        base_case = try lowLevelStmt(&store, scaled, .num_int_mul_wrap, &.{ args[2], radix }, base_case);
        base_case = try store.addCFStmt(.{ .assign_literal = .{
            .target = radix,
            .value = .{ .i64_literal = .{ .value = 64, .layout_idx = .u64 } },
            .next = base_case,
        } });
        var sources: [3]usize = undefined;
        var call_args: [4]LocalId = undefined;
        var code = encoding;
        for (&sources, call_args[0..3]) |*source, *arg| {
            source.* = code % 4;
            code /= 4;
            arg.* = if (source.* == 3) fresh else args[source.*];
        }
        call_args[3] = remaining;
        const call = try store.addCFStmt(.{ .assign_call = .{
            .target = result,
            .proc = proc,
            .args = try store.addLocalSpan(&call_args),
            .next = ret,
        } });
        var recur = try lowLevelStmt(&store, remaining, .num_int_sub_wrap, &.{ args[3], one }, call);
        recur = try store.addCFStmt(.{ .assign_literal = .{
            .target = fresh,
            .value = .{ .i64_literal = .{ .value = 31, .layout_idx = .u64 } },
            .next = recur,
        } });
        recur = try store.addCFStmt(.{ .assign_literal = .{
            .target = one,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = recur,
        } });
        const body = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = args[3],
            .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = base_case }}),
            .default_branch = recur,
        } });
        store.getProcSpecPtr(proc).body = body;
        store.getProcSpecPtr(proc).frame_locals = try store.addLocalSpan(b.locals.items);
        const sites = try builder.finish(&store);
        store.getProcSpecPtr(proc).tail_calls = sites;
        store.tail_call_builder = null;
        const frame_before = store.getProcSpec(proc).frame_locals;
        const locals_before = store.localCount();
        try lir.Trmc.run(&store, &layouts);
        const has_cycle = (sources[0] == 1 and sources[1] == 0) or
            (sources[0] == 2 and sources[2] == 0) or
            (sources[1] == 2 and sources[2] == 1) or
            (sources[0] == 1 and sources[1] == 2 and sources[2] == 0) or
            (sources[0] == 2 and sources[2] == 1 and sources[1] == 0);
        try std.testing.expectEqual(@as(usize, @intFromBool(has_cycle)), store.localCount() - locals_before);
        const frame_after = store.getProcSpec(proc).frame_locals;
        if (has_cycle) {
            const frame = store.getLocalSpan(frame_after);
            try std.testing.expectEqual(b.locals.items.len + 1, frame.len);
            for (b.locals.items, 0..) |local, index| try std.testing.expectEqual(local, GuardedList.at(frame, index));
            try std.testing.expectEqual(@as(LocalId, @enumFromInt(locals_before)), GuardedList.at(frame, frame.len - 1));
        } else {
            try std.testing.expectEqualDeep(frame_before, frame_after);
        }
        try lir.Arc.insert(&store, &layouts, .{});
        for ([_]u64{ 1, 2, 7 }) |iterations| {
            var expected = [_]u64{ 11, 17, 23, 31 };
            for (0..iterations) |_| {
                const previous = expected;
                for (sources, 0..) |source, index| expected[index] = previous[source];
            }
            const actual = try runProcU64Args(allocator, &store, &layouts, proc, &runtime_env, &.{ 11, 17, 23, iterations });
            try std.testing.expectEqual(expected[0] + 64 * expected[1] + 4096 * expected[2], actual);
        }
        try runtime_env.checkForLeaks();
    }
}

test "tce reuses scratch across shrinking and growing procedure frames" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const widths = [_]usize{ 8, 0, 3, 12, 1 };
    var procs: [widths.len]LIR.LirProcSpecId = undefined;
    var frames: [widths.len]LIR.LocalSpan = undefined;
    for (widths, &procs, &frames) |width, *proc, *frame| {
        var b = ProcBuilder.init(&store);
        defer b.deinit(allocator);
        var arg_buffer: [13]LocalId = undefined;
        const args = arg_buffer[0 .. width + 1];
        for (args) |*arg| arg.* = try b.addLocal(allocator, .u64);
        proc.* = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(args),
            .ret_layout = .u64,
            // An explicit requirement must survive even when the pass adds
            // no locals. It is never downgraded based on the new locals alone.
            .stack_probe = .required,
        });
        var builder = lir.TailCallBuilder.init(allocator, proc.*);
        defer builder.deinit();
        store.tail_call_builder = &builder;
        defer store.tail_call_builder = null;
        const result = try b.addLocal(allocator, .u64);
        const one = try b.addLocal(allocator, .u64);
        const remaining = try b.addLocal(allocator, .u64);
        const base_case = try store.addCFStmt(.{ .ret = .{ .value = args[0] } });
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
        var call_buffer: [13]LocalId = undefined;
        const call_args = call_buffer[0 .. width + 1];
        for (call_args[0..width], 0..) |*arg, index| arg.* = args[(index + 1) % width];
        call_args[width] = remaining;
        const call = try store.addCFStmt(.{ .assign_call = .{
            .target = result,
            .proc = proc.*,
            .args = try store.addLocalSpan(call_args),
            .next = ret,
        } });
        var recur = try lowLevelStmt(&store, remaining, .num_int_sub_wrap, &.{ args[width], one }, call);
        recur = try store.addCFStmt(.{ .assign_literal = .{
            .target = one,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = recur,
        } });
        store.getProcSpecPtr(proc.*).body = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = args[width],
            .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = base_case }}),
            .default_branch = recur,
        } });
        frame.* = try store.addLocalSpan(b.locals.items);
        store.getProcSpecPtr(proc.*).frame_locals = frame.*;
        const sites = try builder.finish(&store);
        store.getProcSpecPtr(proc.*).tail_calls = sites;
    }
    try lir.Trmc.run(&store, &layouts);
    for (widths, procs, frames) |width, proc, old_frame| {
        const spec = store.getProcSpec(proc);
        try std.testing.expectEqual(LIR.TailTransform.tce, spec.tail_transform);
        try std.testing.expectEqual(LIR.StackProbe.required, spec.stack_probe);
        const frame = store.getLocalSpan(spec.frame_locals);
        const old = store.getLocalSpan(old_frame);
        try std.testing.expectEqual(old.len + @intFromBool(width > 1), frame.len);
        for (0..old.len) |index| try std.testing.expectEqual(GuardedList.at(old, index), GuardedList.at(frame, index));
        for (1..frame.len) |index| try std.testing.expect(@intFromEnum(GuardedList.at(frame, index - 1)) < @intFromEnum(GuardedList.at(frame, index)));
    }
    try lir.Arc.insert(&store, &layouts, .{});
    for (widths, procs) |width, proc| {
        var inputs: [13]u64 = undefined;
        for (inputs[0..width], 0..) |*value, index| value.* = 11 + index;
        for ([_]usize{ 0, 1, 2, width + 1 }) |iterations| {
            inputs[width] = iterations;
            const expected: u64 = if (width == 0) 0 else 11 + iterations % width;
            try std.testing.expectEqual(expected, try runProcU64Args(allocator, &store, &layouts, proc, &runtime_env, inputs[0 .. width + 1]));
        }
    }
    try runtime_env.checkForLeaks();
}

test "tce emits an identity loop directly into the original call" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var b = ProcBuilder.init(&store);
    defer b.deinit(allocator);
    const arg = try b.addLocal(allocator, .u64);
    const result = try b.addLocal(allocator, .u64);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{arg}),
        .frame_locals = try store.addLocalSpan(b.locals.items),
        .ret_layout = .u64,
    });
    var builder = lir.TailCallBuilder.init(allocator, proc);
    defer builder.deinit();
    store.tail_call_builder = &builder;
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const loc: base.SourceLoc = .{ .file = 0, .line = 7, .column = 3 };
    store.current_loc = loc;
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = proc,
        .args = try store.addLocalSpan(&.{arg}),
        .next = ret,
    } });
    store.current_loc = .none;
    store.getProcSpecPtr(proc).body = call;
    const sites = try builder.finish(&store);
    store.getProcSpecPtr(proc).tail_calls = sites;
    store.tail_call_builder = null;
    const stmts_before = store.cfStmtCount();
    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqual(stmts_before + 2, store.cfStmtCount());
    try std.testing.expectEqualDeep(loc, store.stmtLoc(call));
    const join = store.getCFStmt(store.getProcSpec(proc).body.?).join;
    try std.testing.expectEqual(call, join.body);
    try std.testing.expectEqual(join.id, store.getCFStmt(call).jump.target);
    try std.testing.expect(store.getCFStmt(ret) == .ret);
}

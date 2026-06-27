//! LIR-level tests for the TRMC compiler-internal pointer ops
//! (ptr_alloca, box_alloc_zeroed, ptr_store, ptr_load, ptr_cast), exercised
//! through the interpreter on hand-built LIR. The TRMC pass itself
//! (src/lir/trmc.zig) builds on these ops; its detection/transform tests
//! also live in this file.

const std = @import("std");
const base = @import("base");
const eval = @import("eval");
const layout = @import("layout");
const lir = @import("lir");
const RuntimeHostEnv = eval.RuntimeHostEnv;

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const LirStore = lir.LirStore;
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
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = try store.addLocalSpan(args),
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
    var interp = try eval.Interpreter.init(allocator, store, layouts, runtime_env.get_ops());
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
    const add = try lowLevelStmt(&store, sum, .num_plus, &.{ pre, post }, ret);
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
        .rc = .{ .op = .decref, .layout_idx = box_u64 },
        .next = ret,
    } });
    const add = try lowLevelStmt(&store, sum, .num_plus, &.{ pre, post }, drop_cell);
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
        .rc = .{ .op = .decref, .layout_idx = box_pair },
        .next = ret,
    } });
    const add = try lowLevelStmt(&store, sum, .num_plus, &.{ f0, f1 }, drop_cell);
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

/// A "Peano list": U := [Nil, S(struct{ box(U) })] — the smallest recursive
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
/// — done-join with the result local as its param, branches lowering into it,
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
    const mk_m = try lowLevelStmt(store, m, .num_minus, &.{ a_n, one }, call);
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
    var visited = std.AutoHashMap(CFStmtId, void).init(allocator);
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
                for (store.getCFSwitchBranches(s.branches)) |branch| try work.append(allocator, branch.body);
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
                for (store.getStrMatchArms(s.arms)) |arm| try work.append(allocator, arm.on_match);
                try work.append(allocator, s.on_miss);
            },
            .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
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
    var interp = try eval.Interpreter.init(allocator, store, layouts, runtime_env.get_ops());
    defer interp.deinit();
    const arg_layouts = [_]layout.Idx{.u64} ** 4;
    var packed_args: [4]u64 = undefined;
    @memcpy(packed_args[0..args.len], args);
    const result = try interp.eval(.{
        .proc_id = proc,
        .arg_layouts = arg_layouts[0..args.len],
        .arg_ptr = @ptrCast(&packed_args),
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

    var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops());
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

    // Control: without the transform, depth 2000 exceeds the interpreter's
    // 1024-frame cap and crashes.
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
        try lir.Arc.insert(&store, &layouts, .{});

        var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops());
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

        var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops());
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
    // Exactly one heap cell per S node — TRMC must not change allocation counts.
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
    const mk_acc2 = try lowLevelStmt(store, acc2, .num_plus, &.{ a_acc, one }, call);
    const mk_m = try lowLevelStmt(store, m, .num_minus, &.{ a_n, one }, mk_acc2);
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
    const mk_m = try lowLevelStmt(&store, m, .num_minus, &.{ a_n, one }, call);
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

    // odd: res = weird(n - 1) — plain tail call targeting the done-join param
    const tail_jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const tail_call = try store.addCFStmt(.{ .assign_call = .{
        .target = res,
        .proc = proc,
        .args = try store.addLocalSpan(&.{m_tail}),
        .next = tail_jump,
    } });
    const mk_m_tail = try lowLevelStmt(&store, m_tail, .num_minus, &.{ a_n, one }, tail_call);

    // even: res = S(weird(n - 1)) — the construct branch
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
    const mk_m_cons = try lowLevelStmt(&store, m_cons, .num_minus, &.{ a_n, one }, cons_call);

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

    try lir.Trmc.run(&store, &layouts);
    try std.testing.expectEqual(LIR.TailTransform.trmc, store.getProcSpec(proc).tail_transform);
    try std.testing.expect(!(try hasSelfCall(allocator, &store, proc)));
    try lir.Arc.insert(&store, &layouts, .{});

    // Depth 2000 makes 2000 recursive calls (1000 S nodes): both rewritten
    // branches must loop for this to survive the 1024-frame cap.
    var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops());
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
        \\proc p0 args=[l0:u64] ret=tag_union#17
        \\  join j0 params=[l1]
        \\    remainder:
        \\      l2:u64 = literal 0
        \\      l3:bool = low_level num_is_eq(l0, l2)
        \\      switch l3 default_cold=false
        \\        case 1:
        \\          l1:tag_union#17 = tag v0 d0
        \\          jump j0
        \\        default:
        \\          l4:u64 = literal 1
        \\          l5:u64 = low_level num_minus(l0, l4)
        \\          l6:tag_union#17 = call p0(l5)
        \\          l7:box#18 = low_level box_box(l6)
        \\          l8:struct_#19 = struct(l7)
        \\          l9:tag_union#17 = tag v1 d1 (l8)
        \\          l1:tag_union#17 = ref.local l9
        \\          jump j0
        \\    body:
        \\      ret l1
        \\
    , before);

    try lir.Trmc.run(&store, &layouts);

    const after = try dumpProcText(allocator, &store, &layouts, repeat);
    defer allocator.free(after);
    try std.testing.expectEqualStrings(
        \\proc p0 args=[l17:u64] ret=tag_union#17 transform=trmc
        \\  join j1 params=[l0, l10, l11]
        \\    remainder:
        \\      l12:ptr#21 = low_level ptr_alloca()
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
        \\              l1:tag_union#17 = tag v0 d0
        \\              jump j0
        \\            default:
        \\              l4:u64 = literal 1
        \\              l5:u64 = low_level num_minus(l0, l4)
        \\              l7:box#18 = low_level box_alloc_zeroed()
        \\              l15:ptr#21 = low_level ptr_cast(l7)
        \\              l8:struct_#19 = struct(l7)
        \\              l9:tag_union#17 = tag v1 d1 (l8)
        \\              l16:zst = low_level ptr_store(l10, l9)
        \\              set l0 := l5 (initialize_join_param)
        \\              set l10 := l15 (initialize_join_param)
        \\              jump j1
        \\        body:
        \\          l14:zst = low_level ptr_store(l10, l1)
        \\          l13:tag_union#17 = low_level ptr_load(l11)
        \\          ret l13
        \\
    , after);
}

test "must not transform: result used twice, constructor not returned, non-union return, mutual recursion, shared tail" {
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
        const mk_s = try lowLevelStmt(&store, s, .num_plus, &.{ r, one }, ret_s);
        const call = try store.addCFStmt(.{ .assign_call = .{ .target = r, .proc = proc, .args = try store.addLocalSpan(&.{m}), .next = mk_s } });
        const mk_m = try lowLevelStmt(&store, m, .num_minus, &.{ a_n, one }, call);
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
    // rewrites statements in place, so this shape is not eligible unless the
    // pass first splits the shared tail.
    const shared_tail = blk: {
        const a_n = try b.addLocal(allocator, .u64);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{a_n}),
            .ret_layout = .u64,
        });
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
        break :blk proc;
    };

    try lir.Trmc.run(&store, &layouts);

    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(used_twice).tail_transform);
    try std.testing.expect(try hasSelfCall(allocator, &store, used_twice));
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(not_returned).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(fib_like).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(mutual_a).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(mutual_b).tail_transform);
    try std.testing.expectEqual(LIR.TailTransform.none, store.getProcSpec(shared_tail).tail_transform);
    try std.testing.expect(try hasSelfCall(allocator, &store, shared_tail));
}

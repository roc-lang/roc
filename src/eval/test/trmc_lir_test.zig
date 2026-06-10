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

    fn addLocal(self: *ProcBuilder, allocator: Allocator, layout_idx: layout.Idx) !LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.locals.append(allocator, id);
        return id;
    }

    fn finishProc(self: *ProcBuilder, args: []const LocalId, body: CFStmtId, ret_layout: layout.Idx) !LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(args),
            .frame_locals = try self.store.addLocalSpan(self.locals.items),
            .body = body,
            .ret_layout = ret_layout,
        });
    }
};

fn lowLevelStmt(store: *LirStore, target: LocalId, op: LowLevel, args: []const LocalId, next: CFStmtId) !CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = try store.addLocalSpan(args),
        .next = next,
    } });
}

fn runProcU64(allocator: Allocator, store: *const LirStore, layouts: *const layout.Store, proc: LIR.LirProcSpecId, runtime_env: *RuntimeHostEnv) !u64 {
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

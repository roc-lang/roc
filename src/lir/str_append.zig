//! Creates internal append-into-string variants for direct string producers.
//!
//! This runs before ARC. It consumes this explicit caller shape:
//!
//! ```text
//! result = call(args...)
//! out    = join(acc, result)
//! ```
//!
//! where `join` is either string join, `str_concat` or `str_append`, and
//! rewrites it to:
//!
//! ```text
//! out = call_append(acc, args...)
//! ```
//!
//! The generated variant has the ordinary explicit signature:
//!
//! ```text
//! call_append(acc: Str, args...) -> Str
//! ```
//!
//! The variant joins onto the accumulator with the consumer's own join, so a
//! variant made for a spelled `Str.concat` keeps `str_concat` and one made
//! for a generated rendering keeps `str_append`; one variant exists per
//! source proc and join.
//!
//! If the source proc directly returns `Str.concat(left, right)`, the append
//! variant builds `Str.concat(join(acc, left), right)` so the source proc's
//! intermediate returned string is not materialized; the source's own join
//! stays the one it spelled. A source proc that starts its result from
//! `Str.with_capacity` keeps that allocation: the variant joins the finished
//! result onto the accumulator at the return.
//!
//! The fusion drops the call-result local (and any intervening `assign_ref
//! .local` aliases of it): after the rewrite nothing assigns them. It therefore
//! fires only when the concat consumes the alias chain's final value, only
//! `assign_ref .local` aliases of the chain value sit between the call and the
//! concat, and each chain local has exactly one read across the whole proc, so
//! the matched chain is that local's only use.

const std = @import("std");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LowLevelOp = LIR.LowLevel;

/// Allocation failure raised while rewriting string append statements.
pub const ResourceError = Allocator.Error;

/// Rewrite string append statements to direct helper procedure calls.
pub fn run(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    try body_clone.CallResultFusion(Spec).run(store, layouts);
}

/// The consumer this pass fuses: a string join whose right operand is the
/// call result.
const Spec = struct {
    pub const name = "string-append";
    pub const shape = "str_call";
    pub const origin_kind: LIR.OriginKind = .str_append_fuse;
    pub const State = *const layout_mod.Store;
    pub const VariantKey = struct {
        source: LIR.LirProcSpecId,
        /// The join the fused consumer performed.
        op: LowLevelOp,
    };

    pub fn resultEligible(_: State, result_layout: layout_mod.Idx) bool {
        return result_layout == .str;
    }

    pub fn match(
        _: State,
        store: *const LirStore,
        call: @FieldType(LIR.CFStmt, "assign_call"),
        _: layout_mod.Idx,
        forwarded: body_clone.ForwardedAlias,
    ) ?body_clone.CallResultFusion(Spec).Match {
        const concat_stmt_node = store.getCFStmtImmediate(forwarded.next);
        if (concat_stmt_node.* != .assign_low_level) return null;
        const concat_stmt = concat_stmt_node.assign_low_level;
        if (!concat_stmt.op.isStrJoin()) return null;
        const concat_args = store.getLocalSpan(concat_stmt.args);
        if (concat_args.len != 2) return null;
        if (GuardedList.at(concat_args, 1) != forwarded.value) return null;
        const accumulator = GuardedList.at(concat_args, 0);
        if (store.getLocal(concat_stmt.target).layout_idx != .str) return null;
        if (store.getLocal(accumulator).layout_idx != .str) return null;

        return .{
            .key = .{ .source = call.proc, .op = concat_stmt.op },
            .leading_arg = accumulator,
            .target = concat_stmt.target,
            .next = concat_stmt.next,
        };
    }

    pub fn createVariant(layouts: State, store: *LirStore, key: VariantKey) ResourceError!LIR.LirProcSpecId {
        const accumulator = try store.addLocal(.{ .layout_idx = .str });
        return try body_clone.cloneCallVariant(
            AppendRewriter,
            store,
            layouts,
            key.source,
            .{ .accumulator = accumulator, .op = key.op },
            .{ .leading_args = &.{accumulator}, .ret_layout = .str },
        );
    }
};

/// Origin of a statement this pass produces in place of `rewritten`.
fn fuseOrigin(rewritten: LIR.StmtOrigin) LIR.StmtOrigin {
    var origin = rewritten;
    origin.kind = .str_append_fuse;
    return origin;
}

/// Return rewriter that turns each source return into a join onto the
/// destination accumulator, folding a direct string-join return into two
/// joins so the source's intermediate string is never materialized.
const AppendRewriter = struct {
    accumulator: LocalId,
    /// The join the fused consumer performed, which every join onto the
    /// accumulator this rewriter adds uses.
    op: LowLevelOp,

    pub fn cloneRet(self: *AppendRewriter, cloner: anytype, value: LocalId, origin: LIR.StmtOrigin) ResourceError!CFStmtId {
        const fused = fuseOrigin(origin);
        const target = try cloner.addTemp(.str);
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = target } }, fused);
        return try joinInto(cloner, target, self.op, self.accumulator, try cloner.mapLocal(value), fused, ret_stmt);
    }

    pub fn interceptStmt(self: *AppendRewriter, cloner: anytype, _: CFStmtId, stmt: LIR.CFStmt, origin: LIR.StmtOrigin) ResourceError!body_clone.Intercept {
        if (stmt == .assign_low_level) {
            const s = stmt.assign_low_level;
            if (s.op.isStrJoin() and cloner.directReturnOf(s.next, s.target)) {
                return .{ .done = try self.cloneJoinReturn(cloner, s, origin) };
            }
        }
        return .none;
    }

    /// `ret join(left, right)` becomes `ret join(op(acc, left), right)`: the
    /// first join onto the accumulator is this rewriter's, the second is the
    /// source's own.
    fn cloneJoinReturn(self: *AppendRewriter, cloner: anytype, s: anytype, origin: LIR.StmtOrigin) ResourceError!CFStmtId {
        const args = cloner.store.getLocalSpan(s.args);
        if (args.len != 2) return try self.cloneRet(cloner, s.target, origin);

        const fused = fuseOrigin(origin);
        const first_append = try cloner.addTemp(.str);
        const final = try cloner.mapLocal(s.target);
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = final } }, fused);
        const second = try joinInto(cloner, final, s.op, first_append, try cloner.mapLocal(GuardedList.at(args, 1)), fused, ret_stmt);
        return try joinInto(cloner, first_append, self.op, self.accumulator, try cloner.mapLocal(GuardedList.at(args, 0)), fused, second);
    }

    fn joinInto(cloner: anytype, target: LocalId, op: LowLevelOp, left: LocalId, right: LocalId, origin: LIR.StmtOrigin, next: CFStmtId) ResourceError!CFStmtId {
        return try cloner.store.addLowLevelStmt(target, op, &.{ left, right }, next, origin);
    }
};

fn testStrLocal(store: *LirStore) ResourceError!LocalId {
    return try store.addLocal(.{ .layout_idx = .str });
}

fn testStrCallee(store: *LirStore) ResourceError!LIR.LirProcSpecId {
    const arg = try testStrLocal(store);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = arg } }, .test_fixture);
    return try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(1),
        .args = try store.addLocalSpan(&.{arg}),
        .frame_locals = try store.addLocalSpan(&.{arg}),
        .body = .some(ret),
        .ret_layout = .str,
    }, .none);
}

fn testJoin(store: *LirStore, op: LowLevelOp, target: LocalId, args: []const LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = try store.addRcEffect(op.rcEffect()),
        .args = try store.addLocalSpan(args),
        .next = next,
    } }, .test_fixture);
}

fn testConcat(store: *LirStore, target: LocalId, args: []const LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try testJoin(store, .str_concat, target, args, next);
}

/// The straight-line low-level ops of `body`, in order, up to its return;
/// any other statement fails the test.
fn testLowLevelOps(store: *const LirStore, body: CFStmtId, buffer: []LowLevelOp) error{TestUnexpectedResult}![]LowLevelOp {
    var count: usize = 0;
    var current = body;
    while (true) {
        const stmt = store.getCFStmtImmediate(current);
        if (stmt.* == .ret) return buffer[0..count];
        if (stmt.* != .assign_low_level) return error.TestUnexpectedResult;
        buffer[count] = stmt.assign_low_level.op;
        count += 1;
        current = stmt.assign_low_level.next;
    }
}

test "str append fuses a single-use call result into a direct append call" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const callee = try testStrCallee(&store);

    const acc = try testStrLocal(&store);
    const x = try testStrLocal(&store);
    const result = try testStrLocal(&store);
    const out = try testStrLocal(&store);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } }, .test_fixture);
    const concat = try testConcat(&store, out, &.{ acc, result }, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = concat,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(2),
        .args = try store.addLocalSpan(&.{ acc, x }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, result, out }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    const rewritten = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);
    try std.testing.expectEqual(out, rewritten.target);
    try std.testing.expectEqual(ret, rewritten.next);
    const rewritten_args = store.getLocalSpan(rewritten.args);
    try std.testing.expectEqual(@as(usize, 2), rewritten_args.len);
    try std.testing.expectEqual(acc, GuardedList.at(rewritten_args, 0));
    try std.testing.expectEqual(x, GuardedList.at(rewritten_args, 1));

    const variant = store.getProcSpec(rewritten.proc);
    try std.testing.expectEqual(layout_mod.Idx.str, variant.ret_layout);
    const variant_args = store.getLocalSpan(variant.args);
    try std.testing.expectEqual(@as(usize, 2), variant_args.len);
    try std.testing.expectEqual(layout_mod.Idx.str, store.getLocal(GuardedList.at(variant_args, 0)).layout_idx);
}

test "str append variant keeps a dynamic parameter's descriptor binding" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    const dynamic = try layouts.dynamicLayout();

    // `inspect(value: a) -> Str`, whose parameter `a` is described by the
    // descriptor parameter that follows it.
    const desc = try store.addLocal(.{ .layout_idx = .opaque_ptr });
    const value = try store.addLocal(.{ .layout_idx = dynamic, .dyn = .{ .desc = desc, .storage = .param } });
    const rendered = try testStrLocal(&store);
    const callee_ret = try store.addCFStmt(.{ .ret = .{ .value = rendered } }, .test_fixture);
    const inspect = try store.addCFStmt(.{ .dyn = .{
        .target = rendered,
        .data = try store.addStmtRow(LIR.DynOp, .{ .inspect = .{ .value = value, .desc = desc, .names = desc } }),
        .next = callee_ret,
    } }, .test_fixture);
    const callee = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(1),
        .args = try store.addLocalSpan(&.{ value, desc }),
        .frame_locals = try store.addLocalSpan(&.{ value, desc, rendered }),
        .body = .some(inspect),
        .ret_layout = .str,
    }, .none);

    // `out = Str.concat(acc, inspect(n))` with a closed `n`.
    const acc = try testStrLocal(&store);
    const n = try store.addLocal(.{ .layout_idx = .i64 });
    const n_desc = try store.addLocal(.{ .layout_idx = .opaque_ptr });
    const result = try testStrLocal(&store);
    const out = try testStrLocal(&store);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } }, .test_fixture);
    const concat = try testConcat(&store, out, &.{ acc, result }, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{ n, n_desc }),
        .next = concat,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(2),
        .args = try store.addLocalSpan(&.{ acc, n, n_desc }),
        .frame_locals = try store.addLocalSpan(&.{ acc, n, n_desc, result, out }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    try run(&store, &layouts);

    const rewritten = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);
    const variant_args = store.getLocalSpan(store.getProcSpec(rewritten.proc).args);
    try std.testing.expectEqual(@as(usize, 3), variant_args.len);
    const variant_value = store.getLocal(GuardedList.at(variant_args, 1));
    const variant_desc = GuardedList.at(variant_args, 2);
    try std.testing.expectEqual(dynamic, variant_value.layout_idx);
    const binding = variant_value.dyn orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(variant_desc, binding.desc);
    try std.testing.expectEqual(LIR.DynStorage.param, binding.storage);
    try std.testing.expect(store.getLocal(variant_desc).dyn == null);
}

test "str append fuses through a single-use alias of the call result" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const callee = try testStrCallee(&store);

    const acc = try testStrLocal(&store);
    const x = try testStrLocal(&store);
    const result = try testStrLocal(&store);
    const result_alias = try testStrLocal(&store);
    const out = try testStrLocal(&store);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } }, .test_fixture);
    const concat = try testConcat(&store, out, &.{ acc, result_alias }, ret);
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = result_alias,
        .op = .local(result),
        .next = concat,
    } }, .test_fixture);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = alias,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(3),
        .args = try store.addLocalSpan(&.{ acc, x }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, result, result_alias, out }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    const rewritten = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);
    try std.testing.expectEqual(out, rewritten.target);
    try std.testing.expectEqual(ret, rewritten.next);
}

test "str append does not fuse across an alias of an unrelated local" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const callee = try testStrCallee(&store);

    const acc = try testStrLocal(&store);
    const x = try testStrLocal(&store);
    const unrelated = try testStrLocal(&store);
    const unrelated_alias = try testStrLocal(&store);
    const result = try testStrLocal(&store);
    const out = try testStrLocal(&store);
    const extra = try testStrLocal(&store);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = extra } }, .test_fixture);
    const late_use = try testConcat(&store, extra, &.{ out, unrelated_alias }, ret);
    const concat = try testConcat(&store, out, &.{ acc, result }, late_use);
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = unrelated_alias,
        .op = .local(unrelated),
        .next = concat,
    } }, .test_fixture);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = alias,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(4),
        .args = try store.addLocalSpan(&.{ acc, x, unrelated }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, unrelated, unrelated_alias, result, out, extra }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    const before_proc_count = store.procSpecCount();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    const unchanged = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expectEqual(callee, unchanged.proc);
    try std.testing.expectEqual(result, unchanged.target);
    try std.testing.expectEqual(before_proc_count, store.procSpecCount());
}

test "str append does not fuse a multi-use call result" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const callee = try testStrCallee(&store);

    const acc = try testStrLocal(&store);
    const x = try testStrLocal(&store);
    const other = try testStrLocal(&store);
    const result = try testStrLocal(&store);
    const out = try testStrLocal(&store);
    const extra = try testStrLocal(&store);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = extra } }, .test_fixture);
    const concat_two = try testConcat(&store, extra, &.{ result, other }, ret);
    const concat_one = try testConcat(&store, out, &.{ acc, result }, concat_two);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = concat_one,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(5),
        .args = try store.addLocalSpan(&.{ acc, x, other }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, other, result, out, extra }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    const before_proc_count = store.procSpecCount();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    const unchanged = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expectEqual(callee, unchanged.proc);
    try std.testing.expectEqual(result, unchanged.target);
    try std.testing.expectEqual(before_proc_count, store.procSpecCount());
}

test "str append fuses a str_append consumer with a variant that appends with str_append" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const callee = try testStrCallee(&store);

    const acc = try testStrLocal(&store);
    const x = try testStrLocal(&store);
    const result = try testStrLocal(&store);
    const out = try testStrLocal(&store);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } }, .test_fixture);
    const append = try testJoin(&store, .str_append, out, &.{ acc, result }, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = append,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(6),
        .args = try store.addLocalSpan(&.{ acc, x }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, result, out }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    const rewritten = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);
    try std.testing.expectEqual(out, rewritten.target);
    try std.testing.expectEqual(ret, rewritten.next);

    var ops: [4]LowLevelOp = undefined;
    const variant_ops = try testLowLevelOps(&store, store.getProcSpec(rewritten.proc).body.get().?, &ops);
    try std.testing.expectEqualSlices(LowLevelOp, &.{.str_append}, variant_ops);
}

test "str append keeps the source's own join when rooting a direct join return" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    // `callee = |left, right| Str.concat(left, right)`
    const left = try testStrLocal(&store);
    const right = try testStrLocal(&store);
    const joined = try testStrLocal(&store);
    const callee_ret = try store.addCFStmt(.{ .ret = .{ .value = joined } }, .test_fixture);
    const callee_join = try testConcat(&store, joined, &.{ left, right }, callee_ret);
    const callee = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(7),
        .args = try store.addLocalSpan(&.{ left, right }),
        .frame_locals = try store.addLocalSpan(&.{ left, right, joined }),
        .body = .some(callee_join),
        .ret_layout = .str,
    }, .none);

    // `out = str_append(acc, callee(a, b))`, a generated rendering's join.
    const acc = try testStrLocal(&store);
    const a = try testStrLocal(&store);
    const b = try testStrLocal(&store);
    const result = try testStrLocal(&store);
    const out = try testStrLocal(&store);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } }, .test_fixture);
    const append = try testJoin(&store, .str_append, out, &.{ acc, result }, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{ a, b }),
        .next = append,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(8),
        .args = try store.addLocalSpan(&.{ acc, a, b }),
        .frame_locals = try store.addLocalSpan(&.{ acc, a, b, result, out }),
        .body = .some(call),
        .ret_layout = .str,
    }, .none);

    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    const rewritten = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);

    // `ret str_concat(str_append(acc, left), right)`: the join onto the
    // accumulator is the consumer's, the source's own join stays spelled.
    var ops: [4]LowLevelOp = undefined;
    const variant_ops = try testLowLevelOps(&store, store.getProcSpec(rewritten.proc).body.get().?, &ops);
    try std.testing.expectEqualSlices(LowLevelOp, &.{ .str_append, .str_concat }, variant_ops);
    const variant_body = store.getCFStmtImmediate(store.getProcSpec(rewritten.proc).body.get().?).assign_low_level;
    const variant_args = store.getLocalSpan(store.getProcSpec(rewritten.proc).args);
    try std.testing.expectEqual(GuardedList.at(variant_args, 0), GuardedList.at(store.getLocalSpan(variant_body.args), 0));
}

test "str append makes one variant per source and join" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const callee = try testStrCallee(&store);

    const acc = try testStrLocal(&store);
    const x = try testStrLocal(&store);
    const y = try testStrLocal(&store);
    const first_result = try testStrLocal(&store);
    const second_result = try testStrLocal(&store);
    const mid = try testStrLocal(&store);
    const out = try testStrLocal(&store);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } }, .test_fixture);
    const second_join = try testJoin(&store, .str_append, out, &.{ mid, second_result }, ret);
    const second_call = try store.addCFStmt(.{ .assign_call = .{
        .target = second_result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{y}),
        .next = second_join,
    } }, .test_fixture);
    const first_join = try testConcat(&store, mid, &.{ acc, first_result }, second_call);
    const first_call = try store.addCFStmt(.{ .assign_call = .{
        .target = first_result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = first_join,
    } }, .test_fixture);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(9),
        .args = try store.addLocalSpan(&.{ acc, x, y }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, y, first_result, second_result, mid, out }),
        .body = .some(first_call),
        .ret_layout = .str,
    }, .none);

    const before_proc_count = store.procSpecCount();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();
    try run(&store, &layouts);

    try std.testing.expectEqual(before_proc_count + 2, store.procSpecCount());
    const first = store.getCFStmtImmediate(first_call).assign_call;
    const second = store.getCFStmtImmediate(second_call).assign_call;
    try std.testing.expect(first.proc != second.proc);

    var ops: [4]LowLevelOp = undefined;
    try std.testing.expectEqualSlices(LowLevelOp, &.{.str_concat}, try testLowLevelOps(&store, store.getProcSpec(first.proc).body.get().?, &ops));
    try std.testing.expectEqualSlices(LowLevelOp, &.{.str_append}, try testLowLevelOps(&store, store.getProcSpec(second.proc).body.get().?, &ops));
}

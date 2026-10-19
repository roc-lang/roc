//! Creates internal append-into-string variants for direct string producers.
//!
//! This runs before ARC. It consumes this explicit caller shape:
//!
//! ```text
//! result = call(args...)
//! out    = str_concat(acc, result)
//! ```
//!
//! and rewrites it to:
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
//! If the source proc directly returns `Str.concat(left, right)`, the append
//! variant builds `Str.concat(Str.concat(acc, left), right)` so the source
//! proc's intermediate returned string is not materialized.
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
pub fn run(store: *LirStore) ResourceError!void {
    try body_clone.CallResultFusion(Spec).run(store, {});
}

/// `run`, linking variants the object cache offers instead of cloning them.
pub fn runWithCache(store: *LirStore, cache: body_clone.VariantCache) ResourceError!void {
    try body_clone.CallResultFusion(Spec).runWithCache(store, {}, cache);
}

/// The consumer this pass fuses: a `str_concat` whose right operand is the
/// call result.
const Spec = struct {
    pub const name = "string-append";
    pub const shape = "str_call";
    pub const origin_kind: LIR.OriginKind = .str_append_fuse;
    pub const State = void;
    pub const VariantKey = struct {
        source: LIR.LirProcSpecId,
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
        const concat_stmt_node = store.getCFStmt(forwarded.next);
        if (concat_stmt_node != .assign_low_level) return null;
        const concat_stmt = concat_stmt_node.assign_low_level;
        if (concat_stmt.op != .str_concat) return null;
        const concat_args = store.getLocalSpan(concat_stmt.args);
        if (concat_args.len != 2) return null;
        if (GuardedList.at(concat_args, 1) != forwarded.value) return null;
        const accumulator = GuardedList.at(concat_args, 0);
        if (store.getLocal(concat_stmt.target).layout_idx != .str) return null;
        if (store.getLocal(accumulator).layout_idx != .str) return null;

        return .{
            .key = .{ .source = call.proc },
            .leading_arg = accumulator,
            .target = concat_stmt.target,
            .next = concat_stmt.next,
        };
    }

    /// The variant takes the accumulator string ahead of the source's
    /// arguments and returns the appended string; the shape has no layout
    /// choice, so a fixed tag names it.
    pub fn variantShape(_: State, _: VariantKey) ResourceError!body_clone.CallVariantShape {
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash("roc.call-variant.str-append", &digest, .{});
        return .{ .leading_layouts = .{.str}, .leading_count = 1, .extra_frame_locals = 0, .ret_layout = .str, .shape_digest = digest };
    }

    pub fn createVariant(store: *LirStore, key: VariantKey, variant_shape: body_clone.CallVariantShape) ResourceError!LIR.LirProcSpecId {
        const accumulator = try store.addLocal(.{ .layout_idx = .str });
        return try body_clone.cloneCallVariant(
            AppendRewriter,
            store,
            key.source,
            .{ .accumulator = accumulator },
            .{ .leading_args = &.{accumulator}, .ret_layout = .str, .shape_digest = variant_shape.shape_digest },
        );
    }
};

/// Origin of a statement this pass produces in place of `rewritten`.
fn fuseOrigin(rewritten: LIR.StmtOrigin) LIR.StmtOrigin {
    var origin = rewritten;
    origin.kind = .str_append_fuse;
    return origin;
}

/// Return rewriter that turns each source return into an append onto the
/// destination accumulator, folding a direct `Str.concat` return into two
/// appends so the source's intermediate string is never materialized.
const AppendRewriter = struct {
    accumulator: LocalId,

    pub fn cloneRet(self: *AppendRewriter, cloner: anytype, value: LocalId, origin: LIR.StmtOrigin) ResourceError!CFStmtId {
        const fused = fuseOrigin(origin);
        const target = try cloner.addTemp(.str);
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = target } }, fused);
        return try self.concatInto(cloner, target, self.accumulator, try cloner.mapLocal(value), fused, ret_stmt);
    }

    pub fn interceptStmt(self: *AppendRewriter, cloner: anytype, _: CFStmtId, stmt: LIR.CFStmt, origin: LIR.StmtOrigin) ResourceError!body_clone.Intercept {
        if (stmt == .assign_low_level) {
            const s = stmt.assign_low_level;
            if (s.op == .str_concat and cloner.directReturnOf(s.next, s.target)) {
                return .{ .done = try self.cloneConcatReturn(cloner, s, origin) };
            }
        }
        return .none;
    }

    fn cloneConcatReturn(self: *AppendRewriter, cloner: anytype, s: anytype, origin: LIR.StmtOrigin) ResourceError!CFStmtId {
        const args = cloner.store.getLocalSpan(s.args);
        if (args.len != 2) return try self.cloneRet(cloner, s.target, origin);

        const fused = fuseOrigin(origin);
        const first_append = try cloner.addTemp(.str);
        const final = try cloner.mapLocal(s.target);
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = final } }, fused);
        const second = try self.concatInto(cloner, final, first_append, try cloner.mapLocal(GuardedList.at(args, 1)), fused, ret_stmt);
        return try self.concatInto(cloner, first_append, self.accumulator, try cloner.mapLocal(GuardedList.at(args, 0)), fused, second);
    }

    fn concatInto(_: *AppendRewriter, cloner: anytype, target: LocalId, left: LocalId, right: LocalId, origin: LIR.StmtOrigin, next: CFStmtId) ResourceError!CFStmtId {
        return try cloner.store.addLowLevelStmt(target, .str_concat, &.{ left, right }, next, origin);
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
        .body = ret,
        .ret_layout = .str,
    }, .none);
}

fn testConcat(store: *LirStore, target: LocalId, args: []const LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = .str_concat,
        .rc_effect = try store.addRcEffect(LowLevelOp.str_concat.rcEffect()),
        .args = try store.addLocalSpan(args),
        .next = next,
    } }, .test_fixture);
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
        .body = call,
        .ret_layout = .str,
    }, .none);

    try run(&store);

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
        .body = call,
        .ret_layout = .str,
    }, .none);

    try run(&store);

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
        .body = call,
        .ret_layout = .str,
    }, .none);

    const before_proc_count = store.procSpecCount();
    try run(&store);

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
        .body = call,
        .ret_layout = .str,
    }, .none);

    const before_proc_count = store.procSpecCount();
    try run(&store);

    const unchanged = store.getCFStmtImmediate(call).assign_call;
    try std.testing.expectEqual(callee, unchanged.proc);
    try std.testing.expectEqual(result, unchanged.target);
    try std.testing.expectEqual(before_proc_count, store.procSpecCount());
}

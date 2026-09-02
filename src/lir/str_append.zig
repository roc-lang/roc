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
    var pass = StrAppendPass{
        .store = store,
        .variants = std.AutoHashMap(VariantKey, LIR.LirProcSpecId).init(store.allocator),
    };
    defer pass.variants.deinit();

    const proc_count = store.procSpecCount();
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        try pass.transformProc(@enumFromInt(proc_index));
    }
}

const VariantKey = struct {
    source: LIR.LirProcSpecId,
};

const StrAppendPass = struct {
    store: *LirStore,
    variants: std.AutoHashMap(VariantKey, LIR.LirProcSpecId),

    fn transformProc(self: *StrAppendPass, proc_id: LIR.LirProcSpecId) ResourceError!void {
        const body = body_clone.rewritableProcBody(self.store, proc_id) orelse return;

        var stmts = try body_clone.ReachableStmts.init(self.store, body);
        defer stmts.deinit();
        while (try stmts.next()) |stmt_id| {
            _ = try self.rewriteAt(body, stmt_id);
        }
    }

    fn rewriteAt(self: *StrAppendPass, proc_body: CFStmtId, call_stmt_id: CFStmtId) ResourceError!bool {
        const call_stmt_node = self.store.getCFStmt(call_stmt_id);
        if (call_stmt_node != .assign_call) return false;
        const call_stmt = call_stmt_node.assign_call;

        if (!isStrLayout(self.store.getLocal(call_stmt.target).layout_idx)) return false;

        const callee = self.store.getProcSpec(call_stmt.proc);
        if (callee.body == null or callee.hosted != null or callee.abi != .roc) return false;
        if (callee.ret_layout != .str) return false;

        var chain = std.ArrayList(LocalId).empty;
        defer chain.deinit(self.store.allocator);
        const forwarded = try body_clone.forwardLocalAliasChainInto(self.store, self.store.allocator, call_stmt.target, call_stmt.next, &chain);
        const concat_stmt_id = forwarded.next;
        const concat_stmt_node = self.store.getCFStmt(concat_stmt_id);
        if (concat_stmt_node != .assign_low_level) return false;
        const concat_stmt = concat_stmt_node.assign_low_level;
        if (concat_stmt.op != .str_concat) return false;
        const concat_args = self.store.getLocalSpan(concat_stmt.args);
        if (concat_args.len != 2) return false;
        if (GuardedList.at(concat_args, 1) != forwarded.value) return false;
        const accumulator = GuardedList.at(concat_args, 0);
        if (!isStrLayout(self.store.getLocal(concat_stmt.target).layout_idx)) return false;

        if (!isStrLayout(self.store.getLocal(accumulator).layout_idx)) return false;

        if (!try body_clone.chainIsSingleUse(self.store, proc_body, chain.items)) return false;

        const variant = try self.appendVariant(call_stmt.proc);

        var args = std.ArrayList(LocalId).empty;
        defer args.deinit(self.store.allocator);
        try args.append(self.store.allocator, accumulator);
        const call_args = self.store.getLocalSpan(call_stmt.args);
        for (0..call_args.len) |index| {
            try args.append(self.store.allocator, GuardedList.at(call_args, index));
        }

        self.store.getCFStmtPtr(call_stmt_id).* = .{ .assign_call = .{
            .target = concat_stmt.target,
            .proc = variant,
            .args = try self.store.addLocalSpan(args.items),
            .is_cold = call_stmt.is_cold,
            .next = concat_stmt.next,
        } };

        return true;
    }

    fn appendVariant(self: *StrAppendPass, source: LIR.LirProcSpecId) ResourceError!LIR.LirProcSpecId {
        const key = VariantKey{ .source = source };
        if (self.variants.get(key)) |variant| return variant;

        const variant = try self.createAppendVariant(source);
        try self.variants.put(key, variant);
        return variant;
    }

    fn createAppendVariant(self: *StrAppendPass, source: LIR.LirProcSpecId) ResourceError!LIR.LirProcSpecId {
        const accumulator = try self.store.addLocal(.{ .layout_idx = .str });
        return try body_clone.cloneCallVariant(
            AppendRewriter,
            self.store,
            source,
            .{ .accumulator = accumulator },
            .{ .leading_args = &.{accumulator}, .ret_layout = .str },
        );
    }
};

fn isStrLayout(layout_idx: layout_mod.Idx) bool {
    return layout_idx == .str;
}

/// Return rewriter that turns each source return into an append onto the
/// destination accumulator, folding a direct `Str.concat` return into two
/// appends so the source's intermediate string is never materialized.
const AppendRewriter = struct {
    accumulator: LocalId,

    pub fn cloneRet(self: *AppendRewriter, cloner: anytype, value: LocalId) ResourceError!CFStmtId {
        const target = try cloner.addTemp(.str);
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = target } });
        return try self.concatInto(cloner, target, self.accumulator, try cloner.mapLocal(value), ret_stmt);
    }

    pub fn interceptStmt(self: *AppendRewriter, cloner: anytype, stmt: LIR.CFStmt) ResourceError!?CFStmtId {
        if (stmt == .assign_low_level) {
            const s = stmt.assign_low_level;
            if (s.op == .str_concat and cloner.directReturnOf(s.next, s.target)) {
                return try self.cloneConcatReturn(cloner, s);
            }
        }
        return null;
    }

    fn cloneConcatReturn(self: *AppendRewriter, cloner: anytype, s: anytype) ResourceError!CFStmtId {
        const args = cloner.store.getLocalSpan(s.args);
        if (args.len != 2) return try self.cloneRet(cloner, s.target);

        const first_append = try cloner.addTemp(.str);
        const final = try cloner.mapLocal(s.target);
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = final } });
        const second = try self.concatInto(cloner, final, first_append, try cloner.mapLocal(GuardedList.at(args, 1)), ret_stmt);
        return try self.concatInto(cloner, first_append, self.accumulator, try cloner.mapLocal(GuardedList.at(args, 0)), second);
    }

    fn concatInto(_: *AppendRewriter, cloner: anytype, target: LocalId, left: LocalId, right: LocalId, next: CFStmtId) ResourceError!CFStmtId {
        return try cloner.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .str_concat,
            .rc_effect = LowLevelOp.str_concat.rcEffect(),
            .args = try cloner.store.addLocalSpan(&.{ left, right }),
            .next = next,
        } });
    }
};

fn testStrLocal(store: *LirStore) ResourceError!LocalId {
    return try store.addLocal(.{ .layout_idx = .str });
}

fn testStrCallee(store: *LirStore) ResourceError!LIR.LirProcSpecId {
    const arg = try testStrLocal(store);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = arg } });
    return try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{arg}),
        .frame_locals = try store.addLocalSpan(&.{arg}),
        .body = ret,
        .ret_layout = .str,
    });
}

fn testConcat(store: *LirStore, target: LocalId, args: []const LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = .str_concat,
        .rc_effect = LowLevelOp.str_concat.rcEffect(),
        .args = try store.addLocalSpan(args),
        .next = next,
    } });
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

    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } });
    const concat = try testConcat(&store, out, &.{ acc, result }, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = concat,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ acc, x }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, result, out }),
        .body = call,
        .ret_layout = .str,
    });

    try run(&store);

    const rewritten = store.getCFStmt(call).assign_call;
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

    const ret = try store.addCFStmt(.{ .ret = .{ .value = out } });
    const concat = try testConcat(&store, out, &.{ acc, result_alias }, ret);
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = result_alias,
        .op = .{ .local = result },
        .next = concat,
    } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = alias,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ acc, x }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, result, result_alias, out }),
        .body = call,
        .ret_layout = .str,
    });

    try run(&store);

    const rewritten = store.getCFStmt(call).assign_call;
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

    const ret = try store.addCFStmt(.{ .ret = .{ .value = extra } });
    const late_use = try testConcat(&store, extra, &.{ out, unrelated_alias }, ret);
    const concat = try testConcat(&store, out, &.{ acc, result }, late_use);
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = unrelated_alias,
        .op = .{ .local = unrelated },
        .next = concat,
    } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = alias,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ acc, x, unrelated }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, unrelated, unrelated_alias, result, out, extra }),
        .body = call,
        .ret_layout = .str,
    });

    const before_proc_count = store.procSpecCount();
    try run(&store);

    const unchanged = store.getCFStmt(call).assign_call;
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

    const ret = try store.addCFStmt(.{ .ret = .{ .value = extra } });
    const concat_two = try testConcat(&store, extra, &.{ result, other }, ret);
    const concat_one = try testConcat(&store, out, &.{ acc, result }, concat_two);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try store.addLocalSpan(&.{x}),
        .next = concat_one,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ acc, x, other }),
        .frame_locals = try store.addLocalSpan(&.{ acc, x, other, result, out, extra }),
        .body = call,
        .ret_layout = .str,
    });

    const before_proc_count = store.procSpecCount();
    try run(&store);

    const unchanged = store.getCFStmt(call).assign_call;
    try std.testing.expectEqual(callee, unchanged.proc);
    try std.testing.expectEqual(result, unchanged.target);
    try std.testing.expectEqual(before_proc_count, store.procSpecCount());
}

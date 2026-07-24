//! Creates explicit internal return-slot proc variants for by-memory aggregate
//! results when the caller already has a concrete destination pointer.
//!
//! This runs after structural rewrites such as BoxReuse and before ARC. It
//! consumes this adjacent LIR shape, allowing only intervening `assign_ref
//! .local` aliases of the call result:
//!
//! ```text
//! result = call(args...)
//! _      = ptr_store(destination, result)
//! ```
//!
//! for aggregate layouts that are represented by memory. The generated variant
//! has the ordinary explicit signature:
//!
//! ```text
//! call_slot(out: ptr(T), args...) -> {}
//! ```
//!
//! Its body uses the base rule from design.md for general returns: materialize
//! the return value, then store it into `out`. When the original body ends in a
//! direct struct or tag construction, the variant writes that aggregate into
//! `out` with an explicit destination store instead of building a temporary.

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

/// Allocation failure raised while rewriting returned aggregate statements.
pub const ResourceError = Allocator.Error;

/// Rewrite eligible aggregate returns to destination-slot helper calls.
pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void {
    var pass = ReturnSlotPass{
        .store = store,
        .layouts = layouts,
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
    result_layout: layout_mod.Idx,
};

const ReturnSlotPass = struct {
    store: *LirStore,
    layouts: *layout_mod.Store,
    variants: std.AutoHashMap(VariantKey, LIR.LirProcSpecId),

    fn transformProc(self: *ReturnSlotPass, proc_id: LIR.LirProcSpecId) ResourceError!void {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body == null or proc.hosted != null or proc.abi != .roc) return;
        const body = proc.body.?;

        var work = std.ArrayList(CFStmtId).empty;
        defer work.deinit(self.store.allocator);
        var visited = std.AutoHashMap(CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();

        try work.append(self.store.allocator, body);
        while (work.pop()) |stmt_id| {
            const entry = try visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;

            _ = try self.rewriteAt(body, stmt_id);
            try body_clone.appendSuccessors(self.store, &work, stmt_id);
        }
    }

    fn rewriteAt(self: *ReturnSlotPass, proc_body: CFStmtId, call_stmt_id: CFStmtId) ResourceError!bool {
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };

        const result_layout = self.store.getLocal(call_stmt.target).layout_idx;
        if (!self.returnSlotEligible(result_layout)) return false;

        const callee = self.store.getProcSpec(call_stmt.proc);
        if (callee.body == null or callee.hosted != null or callee.abi != .roc) return false;
        if (callee.ret_layout != result_layout) return false;

        var chain = std.ArrayList(LocalId).empty;
        defer chain.deinit(self.store.allocator);
        const stored_alias = try body_clone.forwardLocalAliasChainInto(self.store, self.store.allocator, call_stmt.target, call_stmt.next, &chain);
        const stored_value = stored_alias.value;
        const store_stmt_id = stored_alias.next;
        const store_stmt = switch (self.store.getCFStmt(store_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (store_stmt.op != .ptr_store) return false;
        if (self.store.getLocal(store_stmt.target).layout_idx != .zst) return false;

        const store_args = self.store.getLocalSpan(store_stmt.args);
        if (store_args.len != 2) return false;
        const destination = GuardedList.at(store_args, 0);
        if (GuardedList.at(store_args, 1) != stored_value) return false;

        const destination_layout = self.layouts.getLayout(self.store.getLocal(destination).layout_idx);
        if (destination_layout.tag != .ptr) return false;
        if (destination_layout.getIdx() != result_layout) return false;

        if (!try self.chainIsSingleUse(proc_body, chain.items)) return false;

        const variant = try self.returnSlotVariant(call_stmt.proc, result_layout);

        var args = std.ArrayList(LocalId).empty;
        defer args.deinit(self.store.allocator);
        try args.append(self.store.allocator, destination);
        const call_args = self.store.getLocalSpan(call_stmt.args);
        for (0..call_args.len) |index| {
            try args.append(self.store.allocator, GuardedList.at(call_args, index));
        }

        self.store.getCFStmtPtr(call_stmt_id).* = .{ .assign_call = .{
            .target = store_stmt.target,
            .proc = variant,
            .args = try self.store.addLocalSpan(args.items),
            .is_cold = call_stmt.is_cold,
            .next = store_stmt.next,
        } };

        return true;
    }

    /// Every local in `chain` must have exactly one read across the proc: the
    /// call result is aliased or stored exactly once, each alias feeds the next
    /// link exactly once, and the final value is the matched store's only
    /// consumer. Any extra read means the fusion would orphan a still-live local.
    fn chainIsSingleUse(self: *ReturnSlotPass, proc_body: CFStmtId, chain: []const LocalId) ResourceError!bool {
        var reads = try body_clone.countReachableReads(self.store, proc_body);
        defer reads.deinit();
        for (chain) |local| {
            if (reads.get(local) != 1) return false;
        }
        return true;
    }

    fn returnSlotEligible(self: *const ReturnSlotPass, result_layout: layout_mod.Idx) bool {
        return switch (self.layouts.getLayout(result_layout).tag) {
            .struct_, .tag_union => true,
            .scalar,
            .box,
            .box_of_zst,
            .list,
            .list_of_zst,
            .closure,
            .erased_callable,
            .zst,
            .ptr,
            => false,
        };
    }

    fn returnSlotVariant(
        self: *ReturnSlotPass,
        source: LIR.LirProcSpecId,
        result_layout: layout_mod.Idx,
    ) ResourceError!LIR.LirProcSpecId {
        const key = VariantKey{ .source = source, .result_layout = result_layout };
        if (self.variants.get(key)) |variant| return variant;

        const variant = try self.createReturnSlotVariant(source, result_layout);
        try self.variants.put(key, variant);
        return variant;
    }

    fn createReturnSlotVariant(
        self: *ReturnSlotPass,
        source: LIR.LirProcSpecId,
        result_layout: layout_mod.Idx,
    ) ResourceError!LIR.LirProcSpecId {
        const source_spec = self.store.getProcSpec(source);
        const source_body = source_spec.body orelse unreachable;
        const source_args = self.store.getLocalSpan(source_spec.args);
        const out_ptr_layout = try self.layouts.insertPtr(result_layout);

        const out_ptr = try self.store.addLocal(.{ .layout_idx = out_ptr_layout });
        const store_unit = try self.store.addLocal(.{ .layout_idx = .zst });

        var variant_args = try std.ArrayList(LocalId).initCapacity(self.store.allocator, source_args.len + 1);
        defer variant_args.deinit(self.store.allocator);
        variant_args.appendAssumeCapacity(out_ptr);

        for (0..source_args.len) |index| {
            const source_arg = GuardedList.at(source_args, index);
            const arg = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(source_arg).layout_idx });
            variant_args.appendAssumeCapacity(arg);
        }

        var cloner = try Cloner.init(self.store, .{ .out_ptr = out_ptr, .store_unit = store_unit });
        defer cloner.deinit();

        for (0..source_args.len) |index| {
            const source_arg = GuardedList.at(source_args, index);
            const variant_arg = variant_args.items[index + 1];
            cloner.local_map[@intFromEnum(source_arg)] = variant_arg;
        }

        const source_frame = self.store.getLocalSpan(source_spec.frame_locals);
        try cloner.new_locals.appendSlice(self.store.allocator, variant_args.items);
        try cloner.new_locals.append(self.store.allocator, store_unit);
        for (0..source_frame.len) |index| {
            _ = try cloner.mapLocal(GuardedList.at(source_frame, index));
        }

        const body = try cloner.cloneStmt(source_body);

        var frame_locals = try std.ArrayList(LocalId).initCapacity(self.store.allocator, cloner.new_locals.items.len);
        defer frame_locals.deinit(self.store.allocator);
        frame_locals.appendSliceAssumeCapacity(cloner.new_locals.items);
        std.mem.sort(LocalId, frame_locals.items, {}, body_clone.localIdLessThan);
        const unique_len = body_clone.uniqueSortedLocals(frame_locals.items);

        const variant = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(variant_args.items),
            .frame_locals = try self.store.addLocalSpan(frame_locals.items[0..unique_len]),
            .body = body,
            .ret_layout = .zst,
            .abi = .roc,
        });
        try self.store.copyProcDebugInfo(variant, source);

        return variant;
    }
};

const Cloner = body_clone.BodyCloner(ReturnSlotRewriter);

/// Return rewriter that stores each source return value into the caller's
/// destination pointer, folding a direct struct or tag construction into a
/// destination store so the aggregate is never built into a temporary first.
const ReturnSlotRewriter = struct {
    out_ptr: LocalId,
    store_unit: LocalId,

    pub fn cloneRet(self: *ReturnSlotRewriter, cloner: anytype, value: LocalId) ResourceError!CFStmtId {
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = self.store_unit } });
        return try cloner.store.addCFStmt(.{ .assign_low_level = .{
            .target = self.store_unit,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = try cloner.store.addLocalSpan(&.{ self.out_ptr, try cloner.mapLocal(value) }),
            .next = ret_stmt,
        } });
    }

    pub fn interceptStmt(self: *ReturnSlotRewriter, cloner: anytype, stmt: LIR.CFStmt) ResourceError!?CFStmtId {
        switch (stmt) {
            .assign_struct => |s| {
                if (cloner.directReturnOf(s.next, s.target)) return try self.cloneStructReturn(cloner, s);
                return null;
            },
            .assign_tag => |s| {
                if (cloner.directReturnOf(s.next, s.target)) return try self.cloneTagReturn(cloner, s);
                return null;
            },
            else => return null,
        }
    }

    fn cloneStructReturn(self: *ReturnSlotRewriter, cloner: anytype, s: anytype) ResourceError!CFStmtId {
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = self.store_unit } });
        return try cloner.store.addCFStmt(.{ .store_struct = .{
            .dest = self.out_ptr,
            .struct_layout = cloner.store.getLocal(s.target).layout_idx,
            .fields = try cloner.mapLocalSpan(s.fields),
            .next = ret_stmt,
        } });
    }

    fn cloneTagReturn(self: *ReturnSlotRewriter, cloner: anytype, s: anytype) ResourceError!CFStmtId {
        const ret_stmt = try cloner.store.addCFStmt(.{ .ret = .{ .value = self.store_unit } });
        return try cloner.store.addCFStmt(.{ .store_tag = .{
            .dest = self.out_ptr,
            .tag_layout = cloner.store.getLocal(s.target).layout_idx,
            .variant_index = s.variant_index,
            .discriminant = s.discriminant,
            .payload = try cloner.mapMaybeLocal(s.payload),
            .next = ret_stmt,
        } });
    }
};

fn testLocal(store: *LirStore, layout_idx: layout_mod.Idx) ResourceError!LocalId {
    return try store.addLocal(.{ .layout_idx = layout_idx });
}

fn testLowLevel(store: *LirStore, target: LocalId, op: LowLevelOp, args: []const LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = try store.addLocalSpan(args),
        .next = next,
    } });
}

fn testStructLayout(layouts: *layout_mod.Store) ResourceError!layout_mod.Idx {
    return try layouts.putStructFields(&.{
        .{ .index = 0, .layout = .u64 },
        .{ .index = 1, .layout = .u64 },
    });
}

fn testAggregateCallee(store: *LirStore, result_layout: layout_mod.Idx) ResourceError!LIR.LirProcSpecId {
    const arg = try testLocal(store, .u64);
    const result = try testLocal(store, result_layout);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const assign = try store.addCFStmt(.{ .assign_struct = .{
        .target = result,
        .fields = try store.addLocalSpan(&.{ arg, arg }),
        .next = ret,
    } });
    return try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{arg}),
        .frame_locals = try store.addLocalSpan(&.{ arg, result }),
        .body = assign,
        .ret_layout = result_layout,
    });
}

fn testTagLayout(layouts: *layout_mod.Store) ResourceError!layout_mod.Idx {
    return try layouts.putTagUnion(&.{.u64});
}

fn testTagCallee(store: *LirStore, result_layout: layout_mod.Idx) ResourceError!LIR.LirProcSpecId {
    const arg = try testLocal(store, .u64);
    const result = try testLocal(store, result_layout);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const assign = try store.addCFStmt(.{ .assign_tag = .{
        .target = result,
        .variant_index = 0,
        .discriminant = 0,
        .payload = arg,
        .next = ret,
    } });
    return try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{arg}),
        .frame_locals = try store.addLocalSpan(&.{ arg, result }),
        .body = assign,
        .ret_layout = result_layout,
    });
}

test "return slot creates an explicit ptr-result variant for aggregate call stores" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const aggregate = try testStructLayout(&layouts);
    const aggregate_ptr = try layouts.insertPtr(aggregate);
    const callee = try testAggregateCallee(&store, aggregate);

    const destination = try testLocal(&store, aggregate_ptr);
    const arg = try testLocal(&store, .u64);
    const temporary = try testLocal(&store, aggregate);
    const temporary_alias = try testLocal(&store, aggregate);
    const store_unit = try testLocal(&store, .zst);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = store_unit } });
    const ptr_store = try testLowLevel(&store, store_unit, .ptr_store, &.{ destination, temporary_alias }, ret);
    const alias = try store.addCFStmt(.{ .assign_ref = .{
        .target = temporary_alias,
        .op = .{ .local = temporary },
        .next = ptr_store,
    } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = temporary,
        .proc = callee,
        .args = try store.addLocalSpan(&.{arg}),
        .next = alias,
    } });
    const caller = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ destination, arg }),
        .frame_locals = try store.addLocalSpan(&.{ destination, arg, temporary, temporary_alias, store_unit }),
        .body = call,
        .ret_layout = .zst,
    });

    try run(&store, &layouts);

    const rewritten = store.getCFStmt(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);
    try std.testing.expectEqual(store_unit, rewritten.target);
    try std.testing.expectEqual(ret, rewritten.next);
    const rewritten_args = store.getLocalSpan(rewritten.args);
    try std.testing.expectEqual(@as(usize, 2), rewritten_args.len);
    try std.testing.expectEqual(destination, GuardedList.at(rewritten_args, 0));
    try std.testing.expectEqual(arg, GuardedList.at(rewritten_args, 1));

    const variant = store.getProcSpec(rewritten.proc);
    try std.testing.expectEqual(layout_mod.Idx.zst, variant.ret_layout);
    const variant_args = store.getLocalSpan(variant.args);
    try std.testing.expectEqual(@as(usize, 2), variant_args.len);
    const variant_dest = GuardedList.at(variant_args, 0);
    const variant_value = GuardedList.at(variant_args, 1);
    try std.testing.expectEqual(aggregate_ptr, store.getLocal(variant_dest).layout_idx);
    try std.testing.expectEqual(layout_mod.Idx.u64, store.getLocal(variant_value).layout_idx);

    const variant_store = store.getCFStmt(variant.body.?).store_struct;
    try std.testing.expectEqual(variant_dest, variant_store.dest);
    try std.testing.expectEqual(aggregate, variant_store.struct_layout);
    const variant_store_fields = store.getLocalSpan(variant_store.fields);
    try std.testing.expectEqual(@as(usize, 2), variant_store_fields.len);
    try std.testing.expectEqual(variant_value, GuardedList.at(variant_store_fields, 0));
    try std.testing.expectEqual(variant_value, GuardedList.at(variant_store_fields, 1));
    try std.testing.expectEqual(layout_mod.Idx.zst, store.getLocal(store.getCFStmt(variant_store.next).ret.value).layout_idx);

    const caller_proc = store.getProcSpec(caller);
    try std.testing.expectEqual(call, caller_proc.body.?);
}

test "return slot lowers direct tag return into destination store" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const aggregate = try testTagLayout(&layouts);
    const aggregate_ptr = try layouts.insertPtr(aggregate);
    const callee = try testTagCallee(&store, aggregate);

    const destination = try testLocal(&store, aggregate_ptr);
    const arg = try testLocal(&store, .u64);
    const temporary = try testLocal(&store, aggregate);
    const store_unit = try testLocal(&store, .zst);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = store_unit } });
    const ptr_store = try testLowLevel(&store, store_unit, .ptr_store, &.{ destination, temporary }, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = temporary,
        .proc = callee,
        .args = try store.addLocalSpan(&.{arg}),
        .next = ptr_store,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ destination, arg }),
        .frame_locals = try store.addLocalSpan(&.{ destination, arg, temporary, store_unit }),
        .body = call,
        .ret_layout = .zst,
    });

    try run(&store, &layouts);

    const rewritten = store.getCFStmt(call).assign_call;
    try std.testing.expect(rewritten.proc != callee);
    const variant = store.getProcSpec(rewritten.proc);
    const variant_args = store.getLocalSpan(variant.args);

    const variant_store = store.getCFStmt(variant.body.?).store_tag;
    try std.testing.expectEqual(GuardedList.at(variant_args, 0), variant_store.dest);
    try std.testing.expectEqual(aggregate, variant_store.tag_layout);
    try std.testing.expectEqual(@as(u16, 0), variant_store.variant_index);
    try std.testing.expectEqual(@as(u16, 0), variant_store.discriminant);
    try std.testing.expectEqual(GuardedList.at(variant_args, 1), variant_store.payload.?);
    try std.testing.expectEqual(layout_mod.Idx.zst, store.getLocal(store.getCFStmt(variant_store.next).ret.value).layout_idx);
}

test "return slot shares one variant for identical proc and layout demands" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const aggregate = try testStructLayout(&layouts);
    const aggregate_ptr = try layouts.insertPtr(aggregate);
    const callee = try testAggregateCallee(&store, aggregate);

    const destination_a = try testLocal(&store, aggregate_ptr);
    const destination_b = try testLocal(&store, aggregate_ptr);
    const arg = try testLocal(&store, .u64);
    const temporary_a = try testLocal(&store, aggregate);
    const temporary_b = try testLocal(&store, aggregate);
    const store_unit_a = try testLocal(&store, .zst);
    const store_unit_b = try testLocal(&store, .zst);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = store_unit_b } });
    const ptr_store_b = try testLowLevel(&store, store_unit_b, .ptr_store, &.{ destination_b, temporary_b }, ret);
    const call_b = try store.addCFStmt(.{ .assign_call = .{
        .target = temporary_b,
        .proc = callee,
        .args = try store.addLocalSpan(&.{arg}),
        .next = ptr_store_b,
    } });
    const ptr_store_a = try testLowLevel(&store, store_unit_a, .ptr_store, &.{ destination_a, temporary_a }, call_b);
    const call_a = try store.addCFStmt(.{ .assign_call = .{
        .target = temporary_a,
        .proc = callee,
        .args = try store.addLocalSpan(&.{arg}),
        .next = ptr_store_a,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ destination_a, destination_b, arg }),
        .frame_locals = try store.addLocalSpan(&.{ destination_a, destination_b, arg, temporary_a, temporary_b, store_unit_a, store_unit_b }),
        .body = call_a,
        .ret_layout = .zst,
    });

    const before_proc_count = store.procSpecCount();
    try run(&store, &layouts);

    const rewritten_a = store.getCFStmt(call_a).assign_call;
    const rewritten_b = store.getCFStmt(call_b).assign_call;
    try std.testing.expectEqual(rewritten_a.proc, rewritten_b.proc);
    try std.testing.expectEqual(@as(usize, before_proc_count + 1), store.procSpecCount());
}

test "return slot does not fuse a multi-use stored call result" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const aggregate = try testStructLayout(&layouts);
    const aggregate_ptr = try layouts.insertPtr(aggregate);
    const callee = try testAggregateCallee(&store, aggregate);

    const destination_a = try testLocal(&store, aggregate_ptr);
    const destination_b = try testLocal(&store, aggregate_ptr);
    const arg = try testLocal(&store, .u64);
    const temporary = try testLocal(&store, aggregate);
    const store_unit_a = try testLocal(&store, .zst);
    const store_unit_b = try testLocal(&store, .zst);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = store_unit_b } });
    const ptr_store_b = try testLowLevel(&store, store_unit_b, .ptr_store, &.{ destination_b, temporary }, ret);
    const ptr_store_a = try testLowLevel(&store, store_unit_a, .ptr_store, &.{ destination_a, temporary }, ptr_store_b);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = temporary,
        .proc = callee,
        .args = try store.addLocalSpan(&.{arg}),
        .next = ptr_store_a,
    } });
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ destination_a, destination_b, arg }),
        .frame_locals = try store.addLocalSpan(&.{ destination_a, destination_b, arg, temporary, store_unit_a, store_unit_b }),
        .body = call,
        .ret_layout = .zst,
    });

    const before_proc_count = store.procSpecCount();
    try run(&store, &layouts);

    const unchanged = store.getCFStmt(call).assign_call;
    try std.testing.expectEqual(callee, unchanged.proc);
    try std.testing.expectEqual(temporary, unchanged.target);
    try std.testing.expectEqual(before_proc_count, store.procSpecCount());
}

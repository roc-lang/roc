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
//! Its initial body uses the base rule from design.md: call the original proc
//! normally, then store that temporary into `out`. Later destination-aware
//! expression lowering can replace the temporary inside the variant without
//! changing call sites or backend policy.

const std = @import("std");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LowLevelOp = LIR.LowLevel;

pub const ResourceError = Allocator.Error;

pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void {
    var pass = ReturnSlotPass{
        .store = store,
        .layouts = layouts,
        .variants = std.AutoHashMap(VariantKey, LIR.LirProcSpecId).init(store.allocator),
    };
    defer pass.variants.deinit();

    const proc_count = store.proc_specs.items.len;
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

        var work = std.ArrayList(CFStmtId).empty;
        defer work.deinit(self.store.allocator);
        var visited = std.AutoHashMap(CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();

        try work.append(self.store.allocator, proc.body.?);
        while (work.pop()) |stmt_id| {
            const entry = try visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;

            _ = try self.rewriteAt(stmt_id);
            try self.appendSuccessors(&work, stmt_id);
        }
    }

    fn rewriteAt(self: *ReturnSlotPass, call_stmt_id: CFStmtId) ResourceError!bool {
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };

        const result_layout = self.store.getLocal(call_stmt.target).layout_idx;
        if (!self.returnSlotEligible(result_layout)) return false;

        const callee = self.store.getProcSpec(call_stmt.proc);
        if (callee.body == null or callee.hosted != null or callee.abi != .roc) return false;
        if (callee.ret_layout != result_layout) return false;

        const stored_alias = self.forwardLocalAliasChain(call_stmt.target, call_stmt.next);
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
        const destination = store_args[0];
        if (store_args[1] != stored_value) return false;

        const destination_layout = self.layouts.getLayout(self.store.getLocal(destination).layout_idx);
        if (destination_layout.tag != .ptr) return false;
        if (destination_layout.getIdx() != result_layout) return false;

        const variant = try self.returnSlotVariant(call_stmt.proc, result_layout);

        var args = std.ArrayList(LocalId).empty;
        defer args.deinit(self.store.allocator);
        try args.append(self.store.allocator, destination);
        try args.appendSlice(self.store.allocator, self.store.getLocalSpan(call_stmt.args));

        self.store.getCFStmtPtr(call_stmt_id).* = .{ .assign_call = .{
            .target = store_stmt.target,
            .proc = variant,
            .args = try self.store.addLocalSpan(args.items),
            .is_cold = call_stmt.is_cold,
            .next = store_stmt.next,
        } };

        return true;
    }

    const ForwardedAlias = struct {
        value: LocalId,
        next: CFStmtId,
    };

    fn forwardLocalAliasChain(self: *const ReturnSlotPass, source: LocalId, first_stmt: CFStmtId) ForwardedAlias {
        var value = source;
        var current = first_stmt;
        while (true) {
            const stmt = switch (self.store.getCFStmt(current)) {
                .assign_ref => |s| s,
                else => return .{ .value = value, .next = current },
            };
            switch (stmt.op) {
                .local => |local| if (local == value and self.store.getLocal(stmt.target).layout_idx == self.store.getLocal(value).layout_idx) {
                    value = stmt.target;
                    current = stmt.next;
                    continue;
                },
                else => {},
            }
            return .{ .value = value, .next = current };
        }
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
        const source_args = self.store.getLocalSpan(source_spec.args);
        const out_ptr_layout = try self.layouts.insertPtr(result_layout);

        const out_ptr = try self.store.addLocal(.{ .layout_idx = out_ptr_layout });
        const temporary = try self.store.addLocal(.{ .layout_idx = result_layout });
        const store_unit = try self.store.addLocal(.{ .layout_idx = .zst });

        var variant_args = try std.ArrayList(LocalId).initCapacity(self.store.allocator, source_args.len + 1);
        defer variant_args.deinit(self.store.allocator);
        variant_args.appendAssumeCapacity(out_ptr);

        for (source_args) |source_arg| {
            const arg = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(source_arg).layout_idx });
            variant_args.appendAssumeCapacity(arg);
        }

        const call_args = try self.store.addLocalSpan(variant_args.items[1..]);
        const ret_stmt = try self.store.addCFStmt(.{ .ret = .{ .value = store_unit } });
        const store_stmt = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = store_unit,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ out_ptr, temporary }),
            .next = ret_stmt,
        } });
        const call_stmt = try self.store.addCFStmt(.{ .assign_call = .{
            .target = temporary,
            .proc = source,
            .args = call_args,
            .next = store_stmt,
        } });

        var frame_locals = try std.ArrayList(LocalId).initCapacity(self.store.allocator, variant_args.items.len + 2);
        defer frame_locals.deinit(self.store.allocator);
        frame_locals.appendSliceAssumeCapacity(variant_args.items);
        frame_locals.appendAssumeCapacity(temporary);
        frame_locals.appendAssumeCapacity(store_unit);
        std.mem.sort(LocalId, frame_locals.items, {}, localIdLessThan);

        const variant = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(variant_args.items),
            .frame_locals = try self.store.addLocalSpan(frame_locals.items),
            .body = call_stmt,
            .ret_layout = .zst,
            .abi = .roc,
        });
        try self.store.copyProcDebugInfo(variant, source);

        return variant;
    }

    fn appendSuccessors(
        self: *ReturnSlotPass,
        work: *std.ArrayList(CFStmtId),
        stmt_id: CFStmtId,
    ) ResourceError!void {
        switch (self.store.getCFStmt(stmt_id)) {
            inline .assign_ref,
            .assign_literal,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .set_local,
            .debug,
            .expect,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            => |s| try work.append(self.store.allocator, s.next),

            .switch_stmt => |s| {
                if (s.continuation) |continuation| try work.append(self.store.allocator, continuation);
                try work.append(self.store.allocator, s.default_branch);
                for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                    try work.append(self.store.allocator, branch.body);
                }
            },
            .switch_initialized_payload => |s| {
                try work.append(self.store.allocator, s.initialized_branch);
                try work.append(self.store.allocator, s.uninitialized_branch);
            },
            .str_match => |s| {
                try work.append(self.store.allocator, s.on_match);
                try work.append(self.store.allocator, s.on_miss);
            },
            .str_match_set => |s| {
                for (self.store.getStrMatchArms(s.arms)) |arm| {
                    try work.append(self.store.allocator, arm.on_match);
                }
                try work.append(self.store.allocator, s.on_miss);
            },
            .join => |s| {
                try work.append(self.store.allocator, s.body);
                try work.append(self.store.allocator, s.remainder);
            },
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .expect_err,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => {},
        }
    }

    fn localIdLessThan(_: void, a: LocalId, b: LocalId) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }
};

fn testLocal(store: *LirStore, layout_idx: layout_mod.Idx) !LocalId {
    return try store.addLocal(.{ .layout_idx = layout_idx });
}

fn testLowLevel(store: *LirStore, target: LocalId, op: LowLevelOp, args: []const LocalId, next: CFStmtId) !CFStmtId {
    return try store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = try store.addLocalSpan(args),
        .next = next,
    } });
}

fn testStructLayout(layouts: *layout_mod.Store) !layout_mod.Idx {
    return try layouts.putStructFields(&.{
        .{ .index = 0, .layout = .u64 },
        .{ .index = 1, .layout = .u64 },
    });
}

fn testAggregateCallee(store: *LirStore, result_layout: layout_mod.Idx) !LIR.LirProcSpecId {
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
    try std.testing.expectEqualSlices(LocalId, &.{ destination, arg }, store.getLocalSpan(rewritten.args));

    const variant = store.getProcSpec(rewritten.proc);
    try std.testing.expectEqual(layout_mod.Idx.zst, variant.ret_layout);
    const variant_args = store.getLocalSpan(variant.args);
    try std.testing.expectEqual(@as(usize, 2), variant_args.len);
    try std.testing.expectEqual(aggregate_ptr, store.getLocal(variant_args[0]).layout_idx);
    try std.testing.expectEqual(layout_mod.Idx.u64, store.getLocal(variant_args[1]).layout_idx);

    const variant_call = store.getCFStmt(variant.body.?).assign_call;
    try std.testing.expectEqual(callee, variant_call.proc);
    const variant_store = store.getCFStmt(variant_call.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_store, variant_store.op);
    try std.testing.expectEqualSlices(LocalId, &.{ variant_args[0], variant_call.target }, store.getLocalSpan(variant_store.args));

    const caller_proc = store.getProcSpec(caller);
    try std.testing.expectEqual(call, caller_proc.body.?);
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

    const before_proc_count = store.proc_specs.items.len;
    try run(&store, &layouts);

    const rewritten_a = store.getCFStmt(call_a).assign_call;
    const rewritten_b = store.getCFStmt(call_b).assign_call;
    try std.testing.expectEqual(rewritten_a.proc, rewritten_b.proc);
    try std.testing.expectEqual(@as(usize, before_proc_count + 1), store.proc_specs.items.len);
}

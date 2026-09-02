//! Inline exact single-use internal LIR procedures before structural passes.
//!
//! The admission rule is deliberately complete rather than budget-based: a
//! candidate is called from a producer-stamped iterator-fusion scope, has
//! exactly one reachable direct call, no root or first-class procedure
//! reference, an ordinary compiler-owned Roc ABI, and an ownership-neutral
//! frame. Removing its old unreachable body offsets the body cloned at the
//! call site, so the reachable program never grows. Outermost candidates are
//! processed together; a chain therefore needs one inventory scan per call
//! depth, not per call.

const std = @import("std");
const core = @import("lir_core");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirProgram = core.Program;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation failures produced while inventorying or cloning procedures.
pub const ResourceError = Allocator.Error;

const CallSite = struct {
    caller: LIR.LirProcSpecId,
    stmt: LIR.CFStmtId,
};

const Inventory = struct {
    allocator: Allocator,
    store: *LirStore,
    roots: []const LIR.LirProcSpecId,
    reachable: []bool,
    direct_calls: []u32,
    escapes: []bool,
    unique_site: []?CallSite,
    proc_queue: std.ArrayList(LIR.LirProcSpecId),

    fn init(result: *LirProgram.Result) ResourceError!Inventory {
        const allocator = result.store.allocator;
        const proc_count = result.store.procSpecCount();
        const reachable = try allocator.alloc(bool, proc_count);
        errdefer allocator.free(reachable);
        @memset(reachable, false);
        const direct_calls = try allocator.alloc(u32, proc_count);
        errdefer allocator.free(direct_calls);
        @memset(direct_calls, 0);
        const escapes = try allocator.alloc(bool, proc_count);
        errdefer allocator.free(escapes);
        @memset(escapes, false);
        const unique_site = try allocator.alloc(?CallSite, proc_count);
        errdefer allocator.free(unique_site);
        @memset(unique_site, null);

        return .{
            .allocator = allocator,
            .store = &result.store,
            .roots = result.root_procs.items,
            .reachable = reachable,
            .direct_calls = direct_calls,
            .escapes = escapes,
            .unique_site = unique_site,
            .proc_queue = .empty,
        };
    }

    fn deinit(self: *Inventory) void {
        self.proc_queue.deinit(self.allocator);
        self.allocator.free(self.unique_site);
        self.allocator.free(self.escapes);
        self.allocator.free(self.direct_calls);
        self.allocator.free(self.reachable);
    }

    fn collect(self: *Inventory) ResourceError!void {
        for (self.roots) |root| {
            self.escapes[@intFromEnum(root)] = true;
            try self.markReachable(root);
        }

        var queue_index: usize = 0;
        while (queue_index < self.proc_queue.items.len) : (queue_index += 1) {
            const caller = self.proc_queue.items[queue_index];
            const body = self.store.getProcSpec(caller).body orelse continue;
            var walk = try body_clone.ReachableStmts.init(self.store, body);
            defer walk.deinit();
            while (try walk.next()) |stmt_id| {
                const stmt = self.store.getCFStmt(stmt_id);
                if (stmt == .assign_call) {
                    const call = stmt.assign_call;
                    const callee_index = @intFromEnum(call.proc);
                    self.direct_calls[callee_index] += 1;
                    self.unique_site[callee_index] = .{ .caller = caller, .stmt = stmt_id };
                    try self.markReachable(call.proc);
                } else if (stmt == .assign_packed_erased_fn) {
                    const packed_fn = stmt.assign_packed_erased_fn;
                    self.escapes[@intFromEnum(packed_fn.proc)] = true;
                    try self.markReachable(packed_fn.proc);
                } else if (stmt == .assign_literal and stmt.assign_literal.value == .proc_ref) {
                    const proc = stmt.assign_literal.value.proc_ref;
                    self.escapes[@intFromEnum(proc)] = true;
                    try self.markReachable(proc);
                }
            }
        }
    }

    fn markReachable(self: *Inventory, proc: LIR.LirProcSpecId) ResourceError!void {
        const index = @intFromEnum(proc);
        if (self.reachable[index]) return;
        self.reachable[index] = true;
        try self.proc_queue.append(self.allocator, proc);
    }

    fn collectFrontier(self: *Inventory, layouts: *const layout_mod.Store, sites: *std.ArrayList(CallSite)) ResourceError!void {
        const candidate = try self.allocator.alloc(bool, self.store.procSpecCount());
        defer self.allocator.free(candidate);
        @memset(candidate, false);

        for (0..self.store.procSpecCount()) |callee_index| {
            if (self.direct_calls[callee_index] != 1 or self.escapes[callee_index]) continue;
            const site = self.unique_site[callee_index] orelse continue;
            if (!self.store.getProcSpec(site.caller).iterator_fusion_scope) continue;
            const stmt = self.store.getCFStmt(site.stmt);
            if (stmt != .assign_call or @intFromEnum(stmt.assign_call.proc) != callee_index) continue;
            if (!eligibleCall(self.store, layouts, stmt.assign_call)) continue;
            candidate[callee_index] = true;
        }

        // If a candidate's caller is itself a candidate, defer the nested site.
        // The outer clone migrates that call into its final owner first, so no
        // nested body is cloned and then cloned again on the same round.
        for (candidate, 0..) |is_candidate, callee_index| {
            if (!is_candidate) continue;
            const site = self.unique_site[callee_index].?;
            if (candidate[@intFromEnum(site.caller)]) continue;
            try sites.append(self.allocator, site);
        }
    }
};

fn eligibleCall(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    call: @FieldType(LIR.CFStmt, "assign_call"),
) bool {
    if (call.is_cold or call.result_desc != null or call.out_desc != null) return false;
    const callee = store.getProcSpec(call.proc);
    return callee.body != null and
        callee.hosted == null and
        callee.abi == .roc and
        callee.erased_reuse_arg == null and
        callee.erased_call_args == null and
        callee.ret_desc == null and
        callee.runtime_ret_desc == null and
        !callee.boxy_runtime_entry and
        !callee.is_static_initializer and
        ownershipNeutralFrame(store, layouts, callee.frame_locals);
}

fn ownershipNeutralFrame(store: *const LirStore, layouts: *const layout_mod.Store, frame_span: LIR.LocalSpan) bool {
    const frame = store.getLocalSpan(frame_span);
    for (0..frame.len) |index| {
        const local = GuardedList.at(frame, index);
        const layout = layouts.getLayout(store.getLocal(local).layout_idx);
        if (layouts.layoutContainsRefcounted(layout)) return false;
    }
    return true;
}

/// Inline every eligible, reachable, single-use procedure without code growth.
pub fn run(result: *LirProgram.Result) ResourceError!void {
    while (true) {
        var inventory = try Inventory.init(result);
        defer inventory.deinit();
        try inventory.collect();
        var sites = std.ArrayList(CallSite).empty;
        defer sites.deinit(result.store.allocator);
        try inventory.collectFrontier(&result.layouts, &sites);
        if (sites.items.len == 0) return;
        for (sites.items) |site| try inlineAt(&result.store, &result.layouts, site);
    }
}

const ReturnRewriter = struct {
    target: LIR.LocalId,
    next: LIR.CFStmtId,

    pub fn cloneRet(self: *ReturnRewriter, cloner: anytype, value: LIR.LocalId) ResourceError!LIR.CFStmtId {
        const source = try cloner.mapLocal(value);
        if (source == self.target) return self.next;
        return try cloner.store.addCFStmt(.{ .assign_ref = .{
            .target = self.target,
            .op = .{ .local = source },
            .next = self.next,
        } });
    }
};

fn inlineAt(store: *LirStore, layouts: *layout_mod.Store, site: CallSite) ResourceError!void {
    const call_node = store.getCFStmt(site.stmt);
    std.debug.assert(call_node == .assign_call);
    const call = call_node.assign_call;
    const callee = store.getProcSpec(call.proc);
    const source_body = callee.body.?;
    const source_args = try GuardedList.dupe(store.allocator, LIR.LocalId, store.getLocalSpan(callee.args));
    defer store.allocator.free(source_args);
    const call_args = try GuardedList.dupe(store.allocator, LIR.LocalId, store.getLocalSpan(call.args));
    defer store.allocator.free(call_args);
    const source_frame = try GuardedList.dupe(store.allocator, LIR.LocalId, store.getLocalSpan(callee.frame_locals));
    defer store.allocator.free(source_frame);
    if (source_args.len != call_args.len) @panic("single-use inline call arity differed from callee");

    const inline_scope = try store.addInlineScope(.{
        .source_symbol = callee.name,
        .source_name = store.procDebugNameString(call.proc),
        .source_loc = store.procLoc(call.proc),
        .call_site = store.stmtLoc(site.stmt),
        .parent = store.stmtInlineScope(site.stmt),
    });
    var cloner = try body_clone.BodyCloner(ReturnRewriter).initWithInlineScopeOuter(
        store,
        .{ .target = call.target, .next = call.next },
        inline_scope,
    );
    defer cloner.deinit();

    // Procedure arguments are distinct callee locals. Mapping them directly
    // onto caller operands would be wrong for a transformed tail-recursive
    // callee (or any future LIR producer that writes an argument local), and
    // would make two equal operands alias one mutable slot. Clone each argument
    // and materialize the call boundary as ordinary pre-ARC aliases instead.
    for (0..source_args.len) |index| {
        _ = try cloner.mapLocal(source_args[index]);
    }
    for (0..source_frame.len) |index| {
        _ = try cloner.mapLocal(source_frame[index]);
    }
    var cloned_body = try cloner.cloneStmt(source_body);
    const saved_loc = store.current_loc;
    const saved_region = store.current_region;
    const saved_inline_scope = store.current_inline_scope;
    defer {
        store.current_loc = saved_loc;
        store.current_region = saved_region;
        store.current_inline_scope = saved_inline_scope;
    }
    store.current_loc = store.stmtLoc(site.stmt);
    store.current_region = store.stmtRegion(site.stmt);
    store.current_inline_scope = inline_scope;
    var arg_index = source_args.len;
    while (arg_index > 0) {
        arg_index -= 1;
        cloned_body = try store.addCFStmt(.{ .assign_ref = .{
            .target = cloner.local_map[@intFromEnum(source_args[arg_index])].?,
            .op = .{ .local = call_args[arg_index] },
            .next = cloned_body,
        } });
    }
    store.getCFStmtPtr(site.stmt).* = store.getCFStmt(cloned_body);

    const caller = store.getProcSpecPtr(site.caller);
    const caller_frame = store.getLocalSpan(caller.frame_locals);
    var merged = try std.ArrayList(LIR.LocalId).initCapacity(store.allocator, caller_frame.len + cloner.new_locals.items.len);
    defer merged.deinit(store.allocator);
    for (0..caller_frame.len) |index| merged.appendAssumeCapacity(GuardedList.at(caller_frame, index));
    merged.appendSliceAssumeCapacity(cloner.new_locals.items);
    std.mem.sort(LIR.LocalId, merged.items, {}, body_clone.localIdLessThan);
    const unique_len = body_clone.uniqueSortedLocals(merged.items);
    caller.frame_locals = try store.addLocalSpan(merged.items[0..unique_len]);
    if (store.procNeedsStackProbe(layouts, caller.*)) caller.stack_probe = .required;
}

test "single-use inline declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "single-use inline keeps writable callee arguments distinct from caller operands" {
    const testing = std.testing;
    var result = try LirProgram.Result.init(testing.allocator, .u64);
    defer result.deinit();
    const store = &result.store;

    const callee_arg = try store.addLocal(.{ .layout_idx = .u64 });
    const two = try store.addLocal(.{ .layout_idx = .u64 });
    const callee_ret = try store.addCFStmt(.{ .ret = .{ .value = callee_arg } });
    const callee_set = try store.addCFStmt(.{ .set_local = .{
        .target = callee_arg,
        .value = two,
        .mode = .replace_existing,
        .next = callee_ret,
    } });
    const callee_body = try store.addCFStmt(.{ .assign_literal = .{
        .target = two,
        .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u64 } },
        .next = callee_set,
    } });
    const callee = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .body = callee_body,
        .frame_locals = try store.addLocalSpan(&.{ callee_arg, two }),
        .ret_layout = .u64,
    });

    const caller_arg = try store.addLocal(.{ .layout_idx = .u64 });
    const result_local = try store.addLocal(.{ .layout_idx = .u64 });
    const caller_ret = try store.addCFStmt(.{ .ret = .{ .value = result_local } });
    const caller_body = try store.addCFStmt(.{ .assign_call = .{
        .target = result_local,
        .proc = callee,
        .args = try store.addLocalSpan(&.{caller_arg}),
        .next = caller_ret,
    } });
    const caller = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(2),
        .args = try store.addLocalSpan(&.{caller_arg}),
        .iterator_fusion_scope = true,
        .body = caller_body,
        .frame_locals = try store.addLocalSpan(&.{ caller_arg, result_local }),
        .ret_layout = .u64,
    });
    try result.root_procs.append(testing.allocator, caller);

    try run(&result);

    var saw_argument_copy = false;
    var saw_distinct_write = false;
    var walk = try body_clone.ReachableStmts.init(store, store.getProcSpec(caller).body.?);
    defer walk.deinit();
    while (try walk.next()) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_call) return error.TestUnexpectedResult;
        if (stmt == .assign_ref and stmt.assign_ref.op == .local and stmt.assign_ref.op.local == caller_arg and stmt.assign_ref.target != caller_arg) {
            saw_argument_copy = true;
        }
        if (stmt == .set_local and stmt.set_local.value != caller_arg and stmt.set_local.target != caller_arg) {
            saw_distinct_write = true;
        }
    }
    try testing.expect(saw_argument_copy);
    try testing.expect(saw_distinct_write);
}

test "single-use inline preserves calls with refcounted callee frames" {
    const testing = std.testing;
    var result = try LirProgram.Result.init(testing.allocator, .u64);
    defer result.deinit();
    const store = &result.store;

    const callee_arg = try store.addLocal(.{ .layout_idx = .str });
    const callee_body = try store.addCFStmt(.{ .ret = .{ .value = callee_arg } });
    const callee = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .body = callee_body,
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .str,
    });

    const caller_arg = try store.addLocal(.{ .layout_idx = .str });
    const result_local = try store.addLocal(.{ .layout_idx = .str });
    const caller_ret = try store.addCFStmt(.{ .ret = .{ .value = result_local } });
    const caller_body = try store.addCFStmt(.{ .assign_call = .{
        .target = result_local,
        .proc = callee,
        .args = try store.addLocalSpan(&.{caller_arg}),
        .next = caller_ret,
    } });
    const caller = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(2),
        .args = try store.addLocalSpan(&.{caller_arg}),
        .iterator_fusion_scope = true,
        .body = caller_body,
        .frame_locals = try store.addLocalSpan(&.{ caller_arg, result_local }),
        .ret_layout = .str,
    });
    try result.root_procs.append(testing.allocator, caller);

    try run(&result);

    const preserved = store.getCFStmt(store.getProcSpec(caller).body.?);
    try testing.expect(preserved == .assign_call);
    try testing.expectEqual(callee, preserved.assign_call.proc);
}

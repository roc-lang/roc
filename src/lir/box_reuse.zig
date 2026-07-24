//! Rewrites direct allocation-replacement wrappers to reuse an existing
//! allocation when the LIR shape carries enough explicit information.
//!
//! This runs after SolvedLirLower/TRMC and before ARC insertion. It only
//! accepts an adjacent, straight-line shape:
//!
//! ```text
//! payload0 = box_unbox(boxed)
//! payload1 = call(payload0)
//! result   = box_box(payload1)
//! ret result
//! ```
//!
//! and rewrites it to:
//!
//! ```text
//! result   = box_prepare_update(boxed)
//! payloadp = ptr_cast(result)
//! payload0 = ptr_load(payloadp)
//! payload1 = call(payload0)
//! _        = ptr_store(payloadp, payload1)
//! ret result
//! ```
//!
//! It also accepts equivalent wrapper shapes when lowering routes the call
//! result through a one-parameter join before the final `box_box`, including the
//! platform-entrypoint form where the unbox and update call live in the join's
//! remainder. The join matchers only cross local aliases and zero-sized struct
//! statements, and validate the payload/box layouts before rewriting.

const std = @import("std");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const LowLevelOp = LIR.LowLevel;

/// A local reached by forwarding through `assign_ref .local` aliases, paired
/// with the first statement past the alias chain.
const ForwardedAlias = body_clone.ForwardedAlias;

/// Allocation failure raised while rewriting box update statements.
pub const ResourceError = Allocator.Error;

/// Rewrite eligible box unwrap/update pairs to direct box reuse helper calls.
pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void {
    const proc_count = store.procSpecCount();
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(proc_index);
        try transformProc(store, layouts, proc_id);
    }
}

fn transformProc(store: *LirStore, layouts: *layout_mod.Store, proc_id: LIR.LirProcSpecId) ResourceError!void {
    const proc = store.getProcSpec(proc_id);
    if (proc.body == null or proc.hosted != null or proc.abi != .roc) return;

    var transform = Transform{
        .store = store,
        .layouts = layouts,
        .proc_id = proc_id,
        .new_locals = .empty,
    };
    defer transform.new_locals.deinit(store.allocator);

    var current = proc.body.?;
    while (true) {
        _ = try transform.rewriteAt(current);
        const next = transform.nextOf(current) orelse break;
        current = next;
    }

    if (transform.new_locals.items.len != 0) {
        try transform.updateFrameLocals();
    }
}

const Transform = struct {
    store: *LirStore,
    layouts: *layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
    new_locals: std.ArrayList(LocalId),

    fn rewriteAt(self: *Transform, unbox_stmt_id: CFStmtId) ResourceError!bool {
        if (try self.rewritePackedErasedAt(unbox_stmt_id)) return true;
        if (try self.rewriteJoinBoxAt(unbox_stmt_id)) return true;
        return try self.rewriteBoxAt(unbox_stmt_id);
    }

    fn rewriteBoxAt(self: *Transform, unbox_stmt_id: CFStmtId) ResourceError!bool {
        const unbox_stmt = switch (self.store.getCFStmt(unbox_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (unbox_stmt.op != .box_unbox) return false;
        const unbox_args = self.store.getLocalSpan(unbox_stmt.args);
        if (unbox_args.len != 1) return false;
        const boxed = GuardedList.at(unbox_args, 0);

        if (try self.rewriteDirectBoxAt(unbox_stmt_id, unbox_stmt, boxed)) return true;
        return try self.rewriteJoinedBoxAt(unbox_stmt_id, unbox_stmt, boxed);
    }

    fn rewriteDirectBoxAt(
        self: *Transform,
        unbox_stmt_id: CFStmtId,
        unbox_stmt: @FieldType(LIR.CFStmt, "assign_low_level"),
        boxed: LocalId,
    ) ResourceError!bool {
        const call_stmt_id = unbox_stmt.next;
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };
        const call_args = self.store.getLocalSpan(call_stmt.args);
        if (call_args.len != 1 or GuardedList.at(call_args, 0) != unbox_stmt.target) return false;

        const payload_alias = body_clone.forwardLocalAliasChain(self.store, call_stmt.target, call_stmt.next);
        const payload_value = payload_alias.value;
        const box_stmt_id = payload_alias.next;
        const box_stmt = switch (self.store.getCFStmt(box_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (box_stmt.op != .box_box) return false;
        const box_args = self.store.getLocalSpan(box_stmt.args);
        if (box_args.len != 1 or GuardedList.at(box_args, 0) != payload_value) return false;

        const ret_stmt_id = box_stmt.next;
        const ret_stmt = switch (self.store.getCFStmt(ret_stmt_id)) {
            .ret => |s| s,
            else => return false,
        };
        if (ret_stmt.value != box_stmt.target) return false;

        const result_box = box_stmt.target;
        if (boxed == result_box) return false;

        const box_layout = self.store.getLocal(boxed).layout_idx;
        if (self.store.getLocal(result_box).layout_idx != box_layout) return false;
        if (self.store.getProcSpec(self.proc_id).ret_layout != box_layout) return false;

        const box_layout_value = self.layouts.getLayout(box_layout);
        if (box_layout_value.tag != .box) return false;
        const payload_layout = box_layout_value.getIdx();
        if (self.store.getLocal(unbox_stmt.target).layout_idx != payload_layout) return false;
        if (self.store.getLocal(payload_value).layout_idx != payload_layout) return false;

        const ptr_layout = try self.layouts.insertPtr(payload_layout);
        const payload_ptr = try self.addLocal(ptr_layout);
        const store_unit = try self.addLocal(.zst);

        const load_stmt_id = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = unbox_stmt.target,
            .op = .ptr_load,
            .rc_effect = LowLevelOp.ptr_load.rcEffect(),
            .args = try self.store.addLocalSpan(&.{payload_ptr}),
            .next = call_stmt_id,
        } });
        const cast_stmt_id = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = payload_ptr,
            .op = .ptr_cast,
            .rc_effect = LowLevelOp.ptr_cast.rcEffect(),
            .args = try self.store.addLocalSpan(&.{result_box}),
            .next = load_stmt_id,
        } });

        self.store.getCFStmtPtr(unbox_stmt_id).* = .{ .assign_low_level = .{
            .target = result_box,
            .op = .box_prepare_update,
            .rc_effect = LowLevelOp.box_prepare_update.rcEffect(),
            .args = try self.store.addLocalSpan(&.{boxed}),
            .next = cast_stmt_id,
        } };

        self.store.getCFStmtPtr(box_stmt_id).* = .{ .assign_low_level = .{
            .target = store_unit,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ payload_ptr, payload_value }),
            .next = ret_stmt_id,
        } };

        return true;
    }

    fn rewriteJoinedBoxAt(
        self: *Transform,
        unbox_stmt_id: CFStmtId,
        unbox_stmt: @FieldType(LIR.CFStmt, "assign_low_level"),
        boxed: LocalId,
    ) ResourceError!bool {
        const prelude = self.forwardThroughLocalAliasesAndZsts(unbox_stmt.target, unbox_stmt.next);
        const join_stmt_id = prelude.next;
        const join_stmt = switch (self.store.getCFStmt(join_stmt_id)) {
            .join => |s| s,
            else => return false,
        };

        const join_params = self.store.getLocalSpan(join_stmt.params);
        if (join_params.len != 1) return false;
        if (self.store.getLocalSpan(join_stmt.maybe_uninitialized_params).len != 0) return false;
        if (self.store.getLocalSpan(join_stmt.maybe_uninitialized_conditions).len != 0) return false;
        if (self.store.getU64Span(join_stmt.maybe_uninitialized_condition_masks).len != 0) return false;
        const join_payload = GuardedList.at(join_params, 0);

        const body_alias = body_clone.forwardLocalAliasChain(self.store, join_payload, join_stmt.body);
        const payload_value = body_alias.value;
        const box_stmt_id = body_alias.next;
        const box_stmt = switch (self.store.getCFStmt(box_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (box_stmt.op != .box_box) return false;
        const box_args = self.store.getLocalSpan(box_stmt.args);
        if (box_args.len != 1 or GuardedList.at(box_args, 0) != payload_value) return false;

        const ret_stmt_id = box_stmt.next;
        const ret_stmt = switch (self.store.getCFStmt(ret_stmt_id)) {
            .ret => |s| s,
            else => return false,
        };
        if (ret_stmt.value != box_stmt.target) return false;

        const call_prelude = self.forwardThroughLocalAliasesAndZsts(prelude.value, join_stmt.remainder);
        const call_stmt_id = call_prelude.next;
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };
        if (call_stmt.target != join_payload) return false;
        const call_args = self.store.getLocalSpan(call_stmt.args);
        if (!spanHasLocal(call_args, call_prelude.value)) return false;

        const jump_stmt = switch (self.store.getCFStmt(call_stmt.next)) {
            .jump => |s| s,
            else => return false,
        };
        if (jump_stmt.target != join_stmt.id) return false;

        const result_box = box_stmt.target;
        if (boxed == result_box) return false;

        const box_layout = self.store.getLocal(boxed).layout_idx;
        if (self.store.getLocal(result_box).layout_idx != box_layout) return false;
        if (self.store.getProcSpec(self.proc_id).ret_layout != box_layout) return false;

        const box_layout_value = self.layouts.getLayout(box_layout);
        if (box_layout_value.tag != .box) return false;
        const payload_layout = box_layout_value.getIdx();
        if (self.store.getLocal(unbox_stmt.target).layout_idx != payload_layout) return false;
        if (self.store.getLocal(prelude.value).layout_idx != payload_layout) return false;
        if (self.store.getLocal(call_prelude.value).layout_idx != payload_layout) return false;
        if (self.store.getLocal(join_payload).layout_idx != payload_layout) return false;
        if (self.store.getLocal(payload_value).layout_idx != payload_layout) return false;

        const ptr_layout = try self.layouts.insertPtr(payload_layout);
        const payload_ptr = try self.addLocal(ptr_layout);
        const store_unit = try self.addLocal(.zst);

        const load_stmt_id = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = unbox_stmt.target,
            .op = .ptr_load,
            .rc_effect = LowLevelOp.ptr_load.rcEffect(),
            .args = try self.store.addLocalSpan(&.{payload_ptr}),
            .next = unbox_stmt.next,
        } });
        const cast_stmt_id = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = payload_ptr,
            .op = .ptr_cast,
            .rc_effect = LowLevelOp.ptr_cast.rcEffect(),
            .args = try self.store.addLocalSpan(&.{result_box}),
            .next = load_stmt_id,
        } });

        self.store.getCFStmtPtr(unbox_stmt_id).* = .{ .assign_low_level = .{
            .target = result_box,
            .op = .box_prepare_update,
            .rc_effect = LowLevelOp.box_prepare_update.rcEffect(),
            .args = try self.store.addLocalSpan(&.{boxed}),
            .next = cast_stmt_id,
        } };

        self.store.getCFStmtPtr(box_stmt_id).* = .{ .assign_low_level = .{
            .target = store_unit,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ payload_ptr, payload_value }),
            .next = ret_stmt_id,
        } };

        return true;
    }

    fn rewriteJoinBoxAt(self: *Transform, join_stmt_id: CFStmtId) ResourceError!bool {
        const join_stmt = switch (self.store.getCFStmt(join_stmt_id)) {
            .join => |s| s,
            else => return false,
        };

        const join_params = self.store.getLocalSpan(join_stmt.params);
        if (join_params.len != 1) return false;
        if (self.store.getLocalSpan(join_stmt.maybe_uninitialized_params).len != 0) return false;
        if (self.store.getLocalSpan(join_stmt.maybe_uninitialized_conditions).len != 0) return false;
        if (self.store.getU64Span(join_stmt.maybe_uninitialized_condition_masks).len != 0) return false;
        const join_payload = GuardedList.at(join_params, 0);

        const body_alias = body_clone.forwardLocalAliasChain(self.store, join_payload, join_stmt.body);
        const payload_value = body_alias.value;
        const box_stmt_id = body_alias.next;
        const box_stmt = switch (self.store.getCFStmt(box_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (box_stmt.op != .box_box) return false;
        const box_args = self.store.getLocalSpan(box_stmt.args);
        if (box_args.len != 1 or GuardedList.at(box_args, 0) != payload_value) return false;

        const ret_stmt_id = box_stmt.next;
        const ret_stmt = switch (self.store.getCFStmt(ret_stmt_id)) {
            .ret => |s| s,
            else => return false,
        };
        if (ret_stmt.value != box_stmt.target) return false;

        const unbox_stmt_id = self.skipLocalAliasesAndZsts(join_stmt.remainder);
        const unbox_stmt = switch (self.store.getCFStmt(unbox_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (unbox_stmt.op != .box_unbox) return false;
        const unbox_args = self.store.getLocalSpan(unbox_stmt.args);
        if (unbox_args.len != 1) return false;
        const boxed = GuardedList.at(unbox_args, 0);

        const call_prelude = self.forwardThroughLocalAliasesAndZsts(unbox_stmt.target, unbox_stmt.next);
        const call_stmt_id = call_prelude.next;
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };
        if (call_stmt.target != join_payload) return false;
        const call_args = self.store.getLocalSpan(call_stmt.args);
        if (!spanHasLocal(call_args, call_prelude.value)) return false;

        const jump_stmt = switch (self.store.getCFStmt(call_stmt.next)) {
            .jump => |s| s,
            else => return false,
        };
        if (jump_stmt.target != join_stmt.id) return false;
        if (try self.jumpCountToJoin(join_stmt.id) != 1) return false;

        const result_box = box_stmt.target;
        if (boxed == result_box) return false;

        const box_layout = self.store.getLocal(boxed).layout_idx;
        if (self.store.getLocal(result_box).layout_idx != box_layout) return false;
        if (self.store.getProcSpec(self.proc_id).ret_layout != box_layout) return false;

        const box_layout_value = self.layouts.getLayout(box_layout);
        if (box_layout_value.tag != .box) return false;
        const payload_layout = box_layout_value.getIdx();
        if (self.store.getLocal(unbox_stmt.target).layout_idx != payload_layout) return false;
        if (self.store.getLocal(call_prelude.value).layout_idx != payload_layout) return false;
        if (self.store.getLocal(join_payload).layout_idx != payload_layout) return false;
        if (self.store.getLocal(payload_value).layout_idx != payload_layout) return false;

        const ptr_layout = try self.layouts.insertPtr(payload_layout);
        const payload_ptr = try self.addLocal(ptr_layout);
        const store_unit = try self.addLocal(.zst);

        const load_stmt_id = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = unbox_stmt.target,
            .op = .ptr_load,
            .rc_effect = LowLevelOp.ptr_load.rcEffect(),
            .args = try self.store.addLocalSpan(&.{payload_ptr}),
            .next = unbox_stmt.next,
        } });
        const cast_stmt_id = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = payload_ptr,
            .op = .ptr_cast,
            .rc_effect = LowLevelOp.ptr_cast.rcEffect(),
            .args = try self.store.addLocalSpan(&.{result_box}),
            .next = load_stmt_id,
        } });

        self.store.getCFStmtPtr(unbox_stmt_id).* = .{ .assign_low_level = .{
            .target = result_box,
            .op = .box_prepare_update,
            .rc_effect = LowLevelOp.box_prepare_update.rcEffect(),
            .args = try self.store.addLocalSpan(&.{boxed}),
            .next = cast_stmt_id,
        } };

        self.store.getCFStmtPtr(box_stmt_id).* = .{ .assign_low_level = .{
            .target = store_unit,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ payload_ptr, payload_value }),
            .next = ret_stmt_id,
        } };

        self.store.getCFStmtPtr(join_stmt_id).* = .{ .join = .{
            .id = join_stmt.id,
            .params = try self.store.addLocalSpan(&.{ join_payload, result_box, payload_ptr }),
            .maybe_uninitialized_params = join_stmt.maybe_uninitialized_params,
            .maybe_uninitialized_conditions = join_stmt.maybe_uninitialized_conditions,
            .maybe_uninitialized_condition_masks = join_stmt.maybe_uninitialized_condition_masks,
            .body = join_stmt.body,
            .remainder = join_stmt.remainder,
        } };

        return true;
    }

    /// Fuse a discarded erased-callable pack into the same-shape pack that
    /// replaces it, so the second pack reuses the first pack's allocation
    /// instead of allocating fresh.
    ///
    /// The matched shape is a proc that packs one erased callable, immediately
    /// packs another of the same payload size and alignment, and returns the
    /// second while never reading the first. Real lowering interposes
    /// `assign_ref .local` aliases (and zero-sized struct statements) between
    /// the two packs and between the second pack and the return, so the matcher
    /// crosses those the way the box variants do: it skips such statements to
    /// reach the second pack, then forwards the second pack's result through an
    /// alias chain to the `ret`.
    ///
    /// Setting `reuse = old.target` makes ARC treat the first pack's allocation
    /// as consumed by the second, so the fusion is only sound when the first
    /// pack's result has no other consumer and the crossed return-chain aliases
    /// are each single-use. Both preconditions are proven from proc-wide operand
    /// read counts rather than assumed from the local shape.
    fn rewritePackedErasedAt(self: *Transform, old_stmt_id: CFStmtId) ResourceError!bool {
        const old_stmt = switch (self.store.getCFStmt(old_stmt_id)) {
            .assign_packed_erased_fn => |s| s,
            else => return false,
        };
        if (old_stmt.reuse != null) return false;

        const new_stmt_id = self.skipLocalAliasesAndZsts(old_stmt.next);
        const new_stmt = switch (self.store.getCFStmt(new_stmt_id)) {
            .assign_packed_erased_fn => |s| s,
            else => return false,
        };
        if (new_stmt.reuse != null) return false;
        if (old_stmt.target == new_stmt.target) return false;
        if (new_stmt.capture != null and new_stmt.capture.? == old_stmt.target) return false;

        var return_chain = std.ArrayList(LocalId).empty;
        defer return_chain.deinit(self.store.allocator);
        const returned = try body_clone.forwardLocalAliasChainInto(
            self.store,
            self.store.allocator,
            new_stmt.target,
            new_stmt.next,
            &return_chain,
        );
        const ret_stmt = switch (self.store.getCFStmt(returned.next)) {
            .ret => |s| s,
            else => return false,
        };
        if (ret_stmt.value != returned.value) return false;

        const erased_layout = self.store.getLocal(old_stmt.target).layout_idx;
        if (self.store.getLocal(new_stmt.target).layout_idx != erased_layout) return false;
        if (self.store.getProcSpec(self.proc_id).ret_layout != erased_layout) return false;
        if (self.layouts.getLayout(erased_layout).tag != .erased_callable) return false;

        if (!self.samePackedErasedPayloadShape(old_stmt.capture_layout, new_stmt.capture_layout)) return false;

        const proc_body = self.store.getProcSpec(self.proc_id).body orelse return false;
        var reads = try body_clone.countReachableReads(self.store, proc_body);
        defer reads.deinit();

        // The first pack's allocation becomes the reuse target, so its result
        // must have no other consumer: any read would still be live after the
        // second pack overwrote the allocation. An alias of it read elsewhere
        // shows up here as a nonzero count and declines the rewrite.
        if (reads.get(old_stmt.target) != 0) return false;
        // Each crossed return-chain local (the second pack's result and its
        // aliases) must be read exactly once, so the matched chain is that
        // local's only use and no other statement observes it.
        for (return_chain.items) |local| {
            if (reads.get(local) != 1) return false;
        }

        self.store.getCFStmtPtr(new_stmt_id).* = .{ .assign_packed_erased_fn = .{
            .target = new_stmt.target,
            .proc = new_stmt.proc,
            .capture = new_stmt.capture,
            .capture_layout = new_stmt.capture_layout,
            .on_drop = new_stmt.on_drop,
            .reuse = old_stmt.target,
            .next = new_stmt.next,
        } };

        return true;
    }

    fn samePackedErasedPayloadShape(self: *const Transform, old_layout: ?layout_mod.Idx, new_layout: ?layout_mod.Idx) bool {
        if (old_layout == null or new_layout == null) return old_layout == null and new_layout == null;
        const old_size_align = self.layouts.layoutSizeAlign(self.layouts.getLayout(old_layout.?));
        const new_size_align = self.layouts.layoutSizeAlign(self.layouts.getLayout(new_layout.?));
        return old_size_align.size == new_size_align.size and
            old_size_align.alignment.toByteUnits() == new_size_align.alignment.toByteUnits();
    }

    fn forwardThroughLocalAliasesAndZsts(self: *const Transform, source: LocalId, first_stmt: CFStmtId) ForwardedAlias {
        var value = source;
        var current = first_stmt;
        while (true) {
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |stmt| {
                    switch (stmt.op) {
                        .local => |local| {
                            if (local == value and self.store.getLocal(stmt.target).layout_idx == self.store.getLocal(value).layout_idx) {
                                value = stmt.target;
                            }
                            current = stmt.next;
                            continue;
                        },
                        else => return .{ .value = value, .next = current },
                    }
                },
                .assign_struct => |stmt| {
                    if (self.store.getLocal(stmt.target).layout_idx != .zst) return .{ .value = value, .next = current };
                    if (self.store.getLocalSpan(stmt.fields).len != 0) return .{ .value = value, .next = current };
                    current = stmt.next;
                    continue;
                },
                else => return .{ .value = value, .next = current },
            }
        }
    }

    fn skipLocalAliasesAndZsts(self: *const Transform, first_stmt: CFStmtId) CFStmtId {
        var current = first_stmt;
        while (true) {
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |stmt| switch (stmt.op) {
                    .local => current = stmt.next,
                    else => return current,
                },
                .assign_struct => |stmt| {
                    if (self.store.getLocal(stmt.target).layout_idx != .zst) return current;
                    if (self.store.getLocalSpan(stmt.fields).len != 0) return current;
                    current = stmt.next;
                },
                else => return current,
            }
        }
    }

    fn jumpCountToJoin(self: *Transform, join_id: LIR.JoinPointId) ResourceError!usize {
        const proc = self.store.getProcSpec(self.proc_id);
        const body = proc.body orelse return 0;

        var work = std.ArrayList(CFStmtId).empty;
        defer work.deinit(self.store.allocator);
        var visited = std.AutoHashMap(CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();

        var count: usize = 0;
        try work.append(self.store.allocator, body);
        while (work.pop()) |stmt_id| {
            const entry = try visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;

            switch (self.store.getCFStmt(stmt_id)) {
                .jump => |stmt| {
                    if (stmt.target == join_id) count += 1;
                },
                else => try body_clone.appendSuccessors(self.store, &work, stmt_id),
            }
        }

        return count;
    }

    fn addLocal(self: *Transform, layout_idx: layout_mod.Idx) ResourceError!LocalId {
        const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.new_locals.append(self.store.allocator, local);
        return local;
    }

    fn updateFrameLocals(self: *Transform) ResourceError!void {
        const proc = self.store.getProcSpec(self.proc_id);
        const old = self.store.getLocalSpan(proc.frame_locals);
        var merged = try std.ArrayList(LocalId).initCapacity(self.store.allocator, old.len + self.new_locals.items.len);
        defer merged.deinit(self.store.allocator);
        for (0..old.len) |index| merged.appendAssumeCapacity(GuardedList.at(old, index));
        merged.appendSliceAssumeCapacity(self.new_locals.items);
        std.mem.sort(LocalId, merged.items, {}, body_clone.localIdLessThan);
        const unique_len = body_clone.uniqueSortedLocals(merged.items);

        const frame_locals = try self.store.addLocalSpan(merged.items[0..unique_len]);
        self.store.getProcSpecPtr(self.proc_id).frame_locals = frame_locals;
    }

    fn nextOf(self: *const Transform, stmt_id: CFStmtId) ?CFStmtId {
        return switch (self.store.getCFStmt(stmt_id)) {
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| s.next,
            else => null,
        };
    }
};

fn spanHasLocal(locals: anytype, needle: LocalId) bool {
    for (0..locals.len) |index| {
        if (GuardedList.at(locals, index) == needle) return true;
    }
    return false;
}

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

fn testLocalRef(store: *LirStore, target: LocalId, source: LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_ref = .{
        .target = target,
        .op = .{ .local = source },
        .next = next,
    } });
}

fn testZst(store: *LirStore, target: LocalId, next: CFStmtId) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_struct = .{
        .target = target,
        .fields = try store.addLocalSpan(&.{}),
        .next = next,
    } });
}

fn testFreshJoinPointId(next_join_point: *u32) LIR.JoinPointId {
    const id: LIR.JoinPointId = @enumFromInt(next_join_point.*);
    next_join_point.* += 1;
    return id;
}

fn testPackedErased(
    store: *LirStore,
    target: LocalId,
    proc: LIR.LirProcSpecId,
    capture: ?LocalId,
    capture_layout: ?layout_mod.Idx,
    next: CFStmtId,
) ResourceError!CFStmtId {
    return try store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = target,
        .proc = proc,
        .capture = capture,
        .capture_layout = capture_layout,
        .on_drop = .none,
        .next = next,
    } });
}

test "box reuse rewrites the direct unbox call rebox return chain" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const box_u64 = try layouts.insertBox(.u64);

    const callee_arg = try testLocal(&store, .u64);
    const callee = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });

    const boxed_arg = try testLocal(&store, box_u64);
    const old_payload = try testLocal(&store, .u64);
    const new_payload = try testLocal(&store, .u64);
    const result_box = try testLocal(&store, box_u64);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = result_box } });
    const rebox = try testLowLevel(&store, result_box, .box_box, &.{new_payload}, ret);
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = new_payload,
        .proc = callee,
        .args = try store.addLocalSpan(&.{old_payload}),
        .next = rebox,
    } });
    const unbox = try testLowLevel(&store, old_payload, .box_unbox, &.{boxed_arg}, call);
    const caller = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{boxed_arg}),
        .frame_locals = try store.addLocalSpan(&.{ boxed_arg, old_payload, new_payload, result_box }),
        .body = unbox,
        .ret_layout = box_u64,
    });

    try run(&store, &layouts);

    const prepare = store.getCFStmt(unbox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.box_prepare_update, prepare.op);
    try std.testing.expectEqual(result_box, prepare.target);
    try std.testing.expectEqual(boxed_arg, GuardedList.at(store.getLocalSpan(prepare.args), 0));

    const cast = store.getCFStmt(prepare.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_cast, cast.op);
    const payload_ptr = cast.target;
    try std.testing.expectEqual(result_box, GuardedList.at(store.getLocalSpan(cast.args), 0));

    const load = store.getCFStmt(cast.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_load, load.op);
    try std.testing.expectEqual(old_payload, load.target);
    try std.testing.expectEqual(payload_ptr, GuardedList.at(store.getLocalSpan(load.args), 0));
    try std.testing.expectEqual(call, load.next);

    const store_payload = store.getCFStmt(rebox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_store, store_payload.op);
    const store_args = store.getLocalSpan(store_payload.args);
    try std.testing.expectEqual(payload_ptr, GuardedList.at(store_args, 0));
    try std.testing.expectEqual(new_payload, GuardedList.at(store_args, 1));
    try std.testing.expectEqual(ret, store_payload.next);

    const frame_locals = store.getLocalSpan(store.getProcSpec(caller).frame_locals);
    try std.testing.expect(frame_locals.len >= 6);
}

test "box reuse rewrites joined update wrappers" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const box_u64 = try layouts.insertBox(.u64);

    const callee_old = try testLocal(&store, .u64);
    const callee_delta = try testLocal(&store, .u64);
    const callee = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ callee_old, callee_delta }),
        .frame_locals = try store.addLocalSpan(&.{ callee_old, callee_delta }),
        .ret_layout = .u64,
    });

    const boxed_arg = try testLocal(&store, box_u64);
    const delta_arg = try testLocal(&store, .u64);
    const old_payload = try testLocal(&store, .u64);
    const old_payload_alias = try testLocal(&store, .u64);
    const call_payload_alias = try testLocal(&store, .u64);
    const delta_alias = try testLocal(&store, .u64);
    const join_payload = try testLocal(&store, .u64);
    const body_payload_alias = try testLocal(&store, .u64);
    const result_box = try testLocal(&store, box_u64);
    const prelude_zst = try testLocal(&store, .zst);
    const remainder_zst = try testLocal(&store, .zst);

    var next_join_point: u32 = 0;
    const join_id = testFreshJoinPointId(&next_join_point);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = result_box } });
    const rebox = try testLowLevel(&store, result_box, .box_box, &.{body_payload_alias}, ret);
    const body_alias = try testLocalRef(&store, body_payload_alias, join_payload, rebox);

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = join_payload,
        .proc = callee,
        .args = try store.addLocalSpan(&.{ call_payload_alias, delta_alias }),
        .next = jump,
    } });
    const delta_ref = try testLocalRef(&store, delta_alias, delta_arg, call);
    const call_payload_ref = try testLocalRef(&store, call_payload_alias, old_payload_alias, delta_ref);
    const remainder_zst_stmt = try testZst(&store, remainder_zst, call_payload_ref);

    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{join_payload}),
        .body = body_alias,
        .remainder = remainder_zst_stmt,
    } });
    const prelude_zst_stmt = try testZst(&store, prelude_zst, join);
    const old_payload_ref = try testLocalRef(&store, old_payload_alias, old_payload, prelude_zst_stmt);
    const unbox = try testLowLevel(&store, old_payload, .box_unbox, &.{boxed_arg}, old_payload_ref);
    const caller = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{ boxed_arg, delta_arg }),
        .frame_locals = try store.addLocalSpan(&.{
            boxed_arg,
            delta_arg,
            old_payload,
            old_payload_alias,
            call_payload_alias,
            delta_alias,
            join_payload,
            body_payload_alias,
            result_box,
            prelude_zst,
            remainder_zst,
        }),
        .body = unbox,
        .ret_layout = box_u64,
    });

    try run(&store, &layouts);

    const prepare = store.getCFStmt(unbox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.box_prepare_update, prepare.op);
    try std.testing.expectEqual(result_box, prepare.target);
    try std.testing.expectEqual(boxed_arg, GuardedList.at(store.getLocalSpan(prepare.args), 0));

    const cast = store.getCFStmt(prepare.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_cast, cast.op);
    const payload_ptr = cast.target;
    try std.testing.expectEqual(result_box, GuardedList.at(store.getLocalSpan(cast.args), 0));

    const load = store.getCFStmt(cast.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_load, load.op);
    try std.testing.expectEqual(old_payload, load.target);
    try std.testing.expectEqual(payload_ptr, GuardedList.at(store.getLocalSpan(load.args), 0));
    try std.testing.expectEqual(old_payload_ref, load.next);

    const store_payload = store.getCFStmt(rebox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_store, store_payload.op);
    const store_args = store.getLocalSpan(store_payload.args);
    try std.testing.expectEqual(payload_ptr, GuardedList.at(store_args, 0));
    try std.testing.expectEqual(body_payload_alias, GuardedList.at(store_args, 1));
    try std.testing.expectEqual(ret, store_payload.next);

    const frame_locals = store.getLocalSpan(store.getProcSpec(caller).frame_locals);
    try std.testing.expect(frame_locals.len >= 13);
}

test "box reuse rewrites platform-style join remainder update wrappers" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const box_u64 = try layouts.insertBox(.u64);

    const callee_old = try testLocal(&store, .u64);
    const callee = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_old}),
        .frame_locals = try store.addLocalSpan(&.{callee_old}),
        .ret_layout = .u64,
    });

    const boxed_arg = try testLocal(&store, box_u64);
    const boxed_alias_a = try testLocal(&store, box_u64);
    const boxed_alias_b = try testLocal(&store, box_u64);
    const old_payload = try testLocal(&store, .u64);
    const join_payload = try testLocal(&store, .u64);
    const body_payload_alias = try testLocal(&store, .u64);
    const result_box = try testLocal(&store, box_u64);
    const proc_zst = try testLocal(&store, .zst);
    const remainder_zst = try testLocal(&store, .zst);

    var next_join_point: u32 = 0;
    const join_id = testFreshJoinPointId(&next_join_point);

    const ret = try store.addCFStmt(.{ .ret = .{ .value = result_box } });
    const rebox = try testLowLevel(&store, result_box, .box_box, &.{body_payload_alias}, ret);
    const body_alias = try testLocalRef(&store, body_payload_alias, join_payload, rebox);

    const jump = try store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const call = try store.addCFStmt(.{ .assign_call = .{
        .target = join_payload,
        .proc = callee,
        .args = try store.addLocalSpan(&.{old_payload}),
        .next = jump,
    } });
    const unbox = try testLowLevel(&store, old_payload, .box_unbox, &.{boxed_alias_b}, call);
    const boxed_ref_b = try testLocalRef(&store, boxed_alias_b, boxed_alias_a, unbox);
    const boxed_ref_a = try testLocalRef(&store, boxed_alias_a, boxed_arg, boxed_ref_b);
    const remainder_zst_stmt = try testZst(&store, remainder_zst, boxed_ref_a);

    const join = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try store.addLocalSpan(&.{join_payload}),
        .body = body_alias,
        .remainder = remainder_zst_stmt,
    } });
    const proc_zst_stmt = try testZst(&store, proc_zst, join);
    const caller = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{boxed_arg}),
        .frame_locals = try store.addLocalSpan(&.{
            boxed_arg,
            boxed_alias_a,
            boxed_alias_b,
            old_payload,
            join_payload,
            body_payload_alias,
            result_box,
            proc_zst,
            remainder_zst,
        }),
        .body = proc_zst_stmt,
        .ret_layout = box_u64,
    });

    try run(&store, &layouts);

    const prepare = store.getCFStmt(unbox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.box_prepare_update, prepare.op);
    try std.testing.expectEqual(result_box, prepare.target);
    try std.testing.expectEqual(boxed_alias_b, GuardedList.at(store.getLocalSpan(prepare.args), 0));

    const cast = store.getCFStmt(prepare.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_cast, cast.op);
    const payload_ptr = cast.target;
    try std.testing.expectEqual(result_box, GuardedList.at(store.getLocalSpan(cast.args), 0));

    const load = store.getCFStmt(cast.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_load, load.op);
    try std.testing.expectEqual(old_payload, load.target);
    try std.testing.expectEqual(payload_ptr, GuardedList.at(store.getLocalSpan(load.args), 0));
    try std.testing.expectEqual(call, load.next);

    const rewritten_join = store.getCFStmt(join).join;
    const rewritten_params = store.getLocalSpan(rewritten_join.params);
    try std.testing.expectEqual(@as(usize, 3), rewritten_params.len);
    try std.testing.expectEqual(join_payload, GuardedList.at(rewritten_params, 0));
    try std.testing.expectEqual(result_box, GuardedList.at(rewritten_params, 1));
    try std.testing.expectEqual(payload_ptr, GuardedList.at(rewritten_params, 2));

    const store_payload = store.getCFStmt(rebox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_store, store_payload.op);
    const store_args = store.getLocalSpan(store_payload.args);
    try std.testing.expectEqual(payload_ptr, GuardedList.at(store_args, 0));
    try std.testing.expectEqual(body_payload_alias, GuardedList.at(store_args, 1));
    try std.testing.expectEqual(ret, store_payload.next);

    const frame_locals = store.getLocalSpan(store.getProcSpec(caller).frame_locals);
    try std.testing.expect(frame_locals.len >= 11);
}

test "erased callable reuse rewrites adjacent same-shape repack" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const erased_callable = try layouts.insertErasedCallable();
    const old_capture = try testLocal(&store, .u64);
    const new_capture = try testLocal(&store, .u64);
    const old_callable = try testLocal(&store, erased_callable);
    const new_callable = try testLocal(&store, erased_callable);
    const callee_arg = try testLocal(&store, .u64);

    const old_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });
    const new_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = new_callable } });
    const new_pack = try store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = new_callable,
        .proc = new_proc,
        .capture = new_capture,
        .capture_layout = .u64,
        .on_drop = .none,
        .next = ret,
    } });
    const old_pack = try store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = old_callable,
        .proc = old_proc,
        .capture = old_capture,
        .capture_layout = .u64,
        .on_drop = .none,
        .next = new_pack,
    } });
    const caller = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{ old_capture, new_capture, old_callable, new_callable }),
        .body = old_pack,
        .ret_layout = erased_callable,
    });

    try run(&store, &layouts);

    const rewritten = store.getCFStmt(new_pack).assign_packed_erased_fn;
    try std.testing.expectEqual(old_callable, rewritten.reuse.?);
    try std.testing.expect(!rewritten.reuse_unique);

    const frame_locals = store.getLocalSpan(store.getProcSpec(caller).frame_locals);
    try std.testing.expectEqual(@as(usize, 4), frame_locals.len);
}

test "erased callable reuse forwards through aliases between the packs" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const erased_callable = try layouts.insertErasedCallable();
    const old_capture = try testLocal(&store, .u64);
    const new_capture = try testLocal(&store, .u64);
    const capture_alias_a = try testLocal(&store, .u64);
    const capture_alias_b = try testLocal(&store, .u64);
    const capture_alias_c = try testLocal(&store, .u64);
    const old_callable = try testLocal(&store, erased_callable);
    const new_callable = try testLocal(&store, erased_callable);
    const callee_arg = try testLocal(&store, .u64);

    const old_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });
    const new_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = new_callable } });
    const new_pack = try testPackedErased(&store, new_callable, new_proc, capture_alias_c, .u64, ret);
    // Three `assign_ref .local` aliases (of the second pack's capture, not of
    // the discarded first pack) sit between the two packs.
    const alias_c = try testLocalRef(&store, capture_alias_c, capture_alias_b, new_pack);
    const alias_b = try testLocalRef(&store, capture_alias_b, capture_alias_a, alias_c);
    const alias_a = try testLocalRef(&store, capture_alias_a, new_capture, alias_b);
    const old_pack = try testPackedErased(&store, old_callable, old_proc, old_capture, .u64, alias_a);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{
            old_capture,
            new_capture,
            capture_alias_a,
            capture_alias_b,
            capture_alias_c,
            old_callable,
            new_callable,
        }),
        .body = old_pack,
        .ret_layout = erased_callable,
    });

    try run(&store, &layouts);

    const rewritten = store.getCFStmt(new_pack).assign_packed_erased_fn;
    try std.testing.expectEqual(old_callable, rewritten.reuse.?);
    try std.testing.expect(!rewritten.reuse_unique);
}

test "erased callable reuse declines when an alias of the old pack is read elsewhere" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const erased_callable = try layouts.insertErasedCallable();
    const old_capture = try testLocal(&store, .u64);
    const new_capture = try testLocal(&store, .u64);
    const old_callable = try testLocal(&store, erased_callable);
    const old_callable_alias = try testLocal(&store, erased_callable);
    const old_callable_alias2 = try testLocal(&store, erased_callable);
    const new_callable = try testLocal(&store, erased_callable);
    const callee_arg = try testLocal(&store, .u64);

    const old_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });
    const new_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = new_callable } });
    const new_pack = try testPackedErased(&store, new_callable, new_proc, new_capture, .u64, ret);
    // The discarded first pack is aliased, and that alias is itself read again,
    // so the first pack's allocation still has a live consumer and reuse is
    // unsound.
    const alias2 = try testLocalRef(&store, old_callable_alias2, old_callable_alias, new_pack);
    const alias1 = try testLocalRef(&store, old_callable_alias, old_callable, alias2);
    const old_pack = try testPackedErased(&store, old_callable, old_proc, old_capture, .u64, alias1);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{
            old_capture,
            new_capture,
            old_callable,
            old_callable_alias,
            old_callable_alias2,
            new_callable,
        }),
        .body = old_pack,
        .ret_layout = erased_callable,
    });

    try run(&store, &layouts);

    const unchanged = store.getCFStmt(new_pack).assign_packed_erased_fn;
    try std.testing.expectEqual(@as(?LocalId, null), unchanged.reuse);
}

test "erased callable reuse forwards through the aliased return path" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(allocator, @import("base").target.TargetUsize.native);
    defer layouts.deinit();

    const erased_callable = try layouts.insertErasedCallable();
    const old_capture = try testLocal(&store, .u64);
    const new_capture = try testLocal(&store, .u64);
    const old_callable = try testLocal(&store, erased_callable);
    const new_callable = try testLocal(&store, erased_callable);
    const return_alias_a = try testLocal(&store, erased_callable);
    const return_alias_b = try testLocal(&store, erased_callable);
    const callee_arg = try testLocal(&store, .u64);

    const old_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });
    const new_proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{callee_arg}),
        .frame_locals = try store.addLocalSpan(&.{callee_arg}),
        .ret_layout = .u64,
    });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = return_alias_b } });
    const alias_b = try testLocalRef(&store, return_alias_b, return_alias_a, ret);
    const alias_a = try testLocalRef(&store, return_alias_a, new_callable, alias_b);
    const new_pack = try testPackedErased(&store, new_callable, new_proc, new_capture, .u64, alias_a);
    const old_pack = try testPackedErased(&store, old_callable, old_proc, old_capture, .u64, new_pack);
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{
            old_capture,
            new_capture,
            old_callable,
            new_callable,
            return_alias_a,
            return_alias_b,
        }),
        .body = old_pack,
        .ret_layout = erased_callable,
    });

    try run(&store, &layouts);

    const rewritten = store.getCFStmt(new_pack).assign_packed_erased_fn;
    try std.testing.expectEqual(old_callable, rewritten.reuse.?);
    try std.testing.expect(!rewritten.reuse_unique);
}

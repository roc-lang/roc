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
//! The pass deliberately does not chase aliases, branch tails, erased calls, or
//! non-adjacent statements. Broader destination-passing rewrites should consume
//! explicit data from earlier analysis rather than derive it here.

const std = @import("std");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const LowLevelOp = LIR.LowLevel;

/// Allocation failure raised while rewriting box update statements.
pub const ResourceError = Allocator.Error;

/// Rewrite eligible box unwrap/update pairs to direct box reuse helper calls.
pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void {
    const proc_count = store.proc_specs.items.len;
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

        const call_stmt_id = unbox_stmt.next;
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };
        const call_args = self.store.getLocalSpan(call_stmt.args);
        if (call_args.len != 1 or call_args[0] != unbox_stmt.target) return false;

        const payload_alias = self.forwardLocalAliasChain(call_stmt.target, call_stmt.next);
        const payload_value = payload_alias.value;
        const box_stmt_id = payload_alias.next;
        const box_stmt = switch (self.store.getCFStmt(box_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (box_stmt.op != .box_box) return false;
        const box_args = self.store.getLocalSpan(box_stmt.args);
        if (box_args.len != 1 or box_args[0] != payload_value) return false;

        const ret_stmt_id = box_stmt.next;
        const ret_stmt = switch (self.store.getCFStmt(ret_stmt_id)) {
            .ret => |s| s,
            else => return false,
        };
        if (ret_stmt.value != box_stmt.target) return false;

        const boxed = unbox_args[0];
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

    fn rewritePackedErasedAt(self: *Transform, old_stmt_id: CFStmtId) ResourceError!bool {
        const old_stmt = switch (self.store.getCFStmt(old_stmt_id)) {
            .assign_packed_erased_fn => |s| s,
            else => return false,
        };
        if (old_stmt.reuse != null) return false;

        const new_stmt_id = old_stmt.next;
        const new_stmt = switch (self.store.getCFStmt(new_stmt_id)) {
            .assign_packed_erased_fn => |s| s,
            else => return false,
        };
        if (new_stmt.reuse != null) return false;
        if (old_stmt.target == new_stmt.target) return false;
        if (new_stmt.capture != null and new_stmt.capture.? == old_stmt.target) return false;

        const ret_stmt = switch (self.store.getCFStmt(new_stmt.next)) {
            .ret => |s| s,
            else => return false,
        };
        if (ret_stmt.value != new_stmt.target) return false;

        const erased_layout = self.store.getLocal(old_stmt.target).layout_idx;
        if (self.store.getLocal(new_stmt.target).layout_idx != erased_layout) return false;
        if (self.store.getProcSpec(self.proc_id).ret_layout != erased_layout) return false;
        if (self.layouts.getLayout(erased_layout).tag != .erased_callable) return false;

        if (!self.samePackedErasedPayloadShape(old_stmt.capture_layout, new_stmt.capture_layout)) return false;

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

    const ForwardedAlias = struct {
        value: LocalId,
        next: CFStmtId,
    };

    fn forwardLocalAliasChain(self: *const Transform, source: LocalId, first_stmt: CFStmtId) ForwardedAlias {
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
        merged.appendSliceAssumeCapacity(old);
        merged.appendSliceAssumeCapacity(self.new_locals.items);
        std.mem.sort(LocalId, merged.items, {}, localIdLessThan);

        var unique_len: usize = 0;
        for (merged.items, 0..) |local, idx| {
            if (idx > 0 and merged.items[unique_len - 1] == local) continue;
            merged.items[unique_len] = local;
            unique_len += 1;
        }

        self.store.getProcSpecPtr(self.proc_id).frame_locals = try self.store.addLocalSpan(merged.items[0..unique_len]);
    }

    fn localIdLessThan(_: void, a: LocalId, b: LocalId) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }

    fn nextOf(self: *const Transform, stmt_id: CFStmtId) ?CFStmtId {
        return switch (self.store.getCFStmt(stmt_id)) {
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| s.next,
            else => null,
        };
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
    try std.testing.expectEqual(boxed_arg, store.getLocalSpan(prepare.args)[0]);

    const cast = store.getCFStmt(prepare.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_cast, cast.op);
    const payload_ptr = cast.target;
    try std.testing.expectEqual(result_box, store.getLocalSpan(cast.args)[0]);

    const load = store.getCFStmt(cast.next).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_load, load.op);
    try std.testing.expectEqual(old_payload, load.target);
    try std.testing.expectEqual(payload_ptr, store.getLocalSpan(load.args)[0]);
    try std.testing.expectEqual(call, load.next);

    const store_payload = store.getCFStmt(rebox).assign_low_level;
    try std.testing.expectEqual(LowLevelOp.ptr_store, store_payload.op);
    const store_args = store.getLocalSpan(store_payload.args);
    try std.testing.expectEqual(payload_ptr, store_args[0]);
    try std.testing.expectEqual(new_payload, store_args[1]);
    try std.testing.expectEqual(ret, store_payload.next);

    const frame_locals = store.getLocalSpan(store.getProcSpec(caller).frame_locals);
    try std.testing.expect(frame_locals.len >= 6);
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

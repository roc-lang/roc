//! Function values built in their caller's frame (design.md "Function
//! Values", "Frame Callables").
//!
//! A `.boxy` function value is an erased callable, a heap allocation. A
//! callable that is only called, retained, released, and passed to callee
//! parameters that never let it outlive their call needs no heap: it can live
//! in the frame of the procedure that builds it. This file decides where that
//! holds, over ARC-complete LIR, where every ownership transfer, deferred
//! erased call and frame-replacing call is explicit.
//!
//! `solveParams` computes each procedure's `nonescaping_fn_params`: bit `i`
//! is set when parameter `i` is a function value the procedure only calls
//! (never by a deferred call), retains, releases, aliases, and passes to such
//! parameters of its callees. It is a greatest fixpoint over the procedures
//! of one store: every candidate bit starts set and is cleared when any use
//! of the parameter could let it outlive the call. A body-less procedure
//! keeps the mask its defining object published (`bindImportedInterface`);
//! a procedure with no published mask has none. When in doubt a parameter
//! escapes. `capture_reuse_fn_params`, solved beside it, names the
//! parameters a call may hand over through the erased ABI's reuse channel.
//!
//! A function value stored into a capture record does not escape when the
//! record is only the capture of packings whose callables do not escape,
//! and whose procedures never let that capture field escape. The same
//! fixpoint solves, for each erased-callable procedure, which capture fields
//! its body keeps (`ProcUses.fields`). Such a body may call a capture field
//! by a deferred call: the erased-call runtime makes a pending call before
//! the erased call that invoked the body returns, and every call of a
//! callable that does not escape is made now, so that pending call runs
//! within the frame that called the callable.
//!
//! `markFrames` then rewrites every packing whose callable never outlives
//! its frame into a frame packing: the backend builds the callable in
//! storage the procedure reserves, its retains and releases are dropped,
//! and its capture record is released on every flow edge where the callable
//! dies. Like `immortal_locals`, this runs after ARC and the certifier and
//! changes no ownership decision; `verifyFrames` re-checks the result in a
//! Debug compiler.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const base = @import("base");
const BodyClone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const ProcId = LIR.LirProcSpecId;

/// Parameter positions a mask can name.
pub const mask_positions = 64;

/// A worker and the outcome-restitution variant its object publishes. An
/// importer reaches the variant with the worker's published mask, so the two
/// share one mask.
pub const PublishedPair = struct {
    worker: ProcId,
    variant: ProcId,
};

fn frameInvariant(comptime message: []const u8) noreturn {
    base.invariant("{s}", .{"frame callables invariant violated: " ++ message});
}

fn isErased(store: *const LirStore, layouts: *const layout_mod.Store, local: LocalId) bool {
    return layouts.getLayout(store.localLayout(local)).tag == .erased_callable;
}

/// A read of capture field `index` of an erased-callable procedure's own
/// capture, into the function-valued local `local`.
const FieldRead = struct { index: u32, local: LocalId };

/// A statement reading, or defining, a local.
const LocalStmt = struct {
    local: LocalId,
    stmt: CFStmtId,

    fn lessThan(_: void, a: LocalStmt, b: LocalStmt) bool {
        if (a.local != b.local) return @backingInt(a.local) < @backingInt(b.local);
        return @backingInt(a.stmt) < @backingInt(b.stmt);
    }
};

/// Pairs sorted by local, as parallel columns: the statements of one local
/// are a contiguous run.
const LocalStmts = struct {
    locals: []const LocalId = &.{},
    stmts: []const CFStmtId = &.{},

    /// Sort `pairs` and copy them into `arena`, dropping repeats when
    /// `dedup` says a statement counts once.
    fn build(arena: Allocator, pairs: []LocalStmt, dedup: bool) Allocator.Error!LocalStmts {
        std.mem.sort(LocalStmt, pairs, {}, LocalStmt.lessThan);
        var count: usize = 0;
        for (pairs, 0..) |pair, index| {
            if (dedup and index != 0 and pairs[index - 1].local == pair.local and pairs[index - 1].stmt == pair.stmt) continue;
            count += 1;
        }
        const locals = try arena.alloc(LocalId, count);
        const stmts = try arena.alloc(CFStmtId, count);
        var out: usize = 0;
        for (pairs, 0..) |pair, index| {
            if (dedup and index != 0 and pairs[index - 1].local == pair.local and pairs[index - 1].stmt == pair.stmt) continue;
            locals[out] = pair.local;
            stmts[out] = pair.stmt;
            out += 1;
        }
        return .{ .locals = locals, .stmts = stmts };
    }

    fn of(self: LocalStmts, local: LocalId) []const CFStmtId {
        const key = @backingInt(local);
        var low: usize = 0;
        var high: usize = self.locals.len;
        while (low < high) {
            const mid = (low + high) / 2;
            if (@backingInt(self.locals[mid]) < key) low = mid + 1 else high = mid;
        }
        var end = low;
        while (end < self.locals.len and self.locals[end] == local) end += 1;
        return self.stmts[low..end];
    }
};

/// The statements of one procedure that read each function-valued local,
/// each record it builds from a function value, and its capture.
const ProcUses = struct {
    reads: LocalStmts = .{},
    /// The statements defining each record built from a function value.
    record_defs: LocalStmts = .{},
    /// The function-valued reads of an erased-callable procedure's capture
    /// fields; null when the procedure has no capture or uses it other than
    /// by reading fields, so every field may escape.
    fields: ?[]const FieldRead = null,

    fn recordDefs(self: *const ProcUses, record: LocalId) usize {
        return self.record_defs.of(record).len;
    }

    fn of(self: *const ProcUses, local: LocalId) []const CFStmtId {
        return self.reads.of(local);
    }
};

/// The most sparse chunks a map keyed by store-wide ids keeps while it is
/// reused from one procedure to the next. Procedures' ids lie in different
/// ranges of the store, so a map that kept every chunk would come to span
/// the whole store; one that spans more is freed instead of cleared.
const retained_chunks = 64;

/// Storage the pass reuses for every procedure body it reads.
const Scratch = struct {
    visited: collections.DenseMapUnmanaged(CFStmtId, void) = .empty,
    work: std.ArrayList(CFStmtId) = .empty,
    /// The statements control reaches from the body last walked.
    stmts: std.ArrayList(CFStmtId) = .empty,
    extra: collections.DenseMapUnmanaged(LocalId, void) = .empty,
    containers: std.ArrayList(LocalId) = .empty,
    reads: std.ArrayList(LocalStmt) = .empty,
    defs: std.ArrayList(LocalStmt) = .empty,
    fields: std.ArrayList(FieldRead) = .empty,

    fn deinit(self: *Scratch, allocator: Allocator) void {
        self.visited.deinit(allocator);
        self.work.deinit(allocator);
        self.stmts.deinit(allocator);
        self.extra.deinit(allocator);
        self.containers.deinit(allocator);
        self.reads.deinit(allocator);
        self.defs.deinit(allocator);
        self.fields.deinit(allocator);
    }

    /// Fill `stmts` with the statements control reaches from `body`, each
    /// once, as `BodyClone.ReachableStmts` visits them.
    fn walk(self: *Scratch, allocator: Allocator, store: *const LirStore, body: CFStmtId) Allocator.Error!void {
        self.visited.clearRetainingAtMost(allocator, retained_chunks);
        self.work.clearRetainingCapacity();
        self.stmts.clearRetainingCapacity();
        try self.work.append(allocator, body);
        while (self.work.pop()) |stmt_id| {
            if ((try self.visited.getOrPut(allocator, stmt_id)).found_existing) continue;
            try BodyClone.appendSuccessors(store, &self.work, stmt_id, allocator);
            try self.stmts.append(allocator, stmt_id);
        }
    }
};

const UseCollector = struct {
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    allocator: Allocator,
    out: *std.ArrayList(LocalStmt),
    /// The locals other than function values whose uses are collected.
    extra: *const collections.DenseMapUnmanaged(LocalId, void),
    stmt: CFStmtId = undefined,
    failure: ?Allocator.Error = null,

    fn note(self: *UseCollector, local: LocalId) void {
        if (self.failure != null) return;
        if (!self.extra.contains(local) and !isErased(self.store, self.layouts, local)) return;
        self.out.append(self.allocator, .{ .local = local, .stmt = self.stmt }) catch |err| {
            self.failure = err;
        };
    }
};

/// The record a statement builds from operands that include a function
/// value, and those operands in field order.
fn builtRecord(store: *const LirStore, stmt: LIR.CFStmt) ?struct { record: LocalId, fields: LIR.LocalSpan } {
    if (stmt == .assign_struct) return .{ .record = stmt.assign_struct.target, .fields = stmt.assign_struct.fields };
    if (stmt != .dyn) return null;
    const op = store.stmtRow(stmt.dyn.data);
    if (op != .record) return null;
    return .{ .record = stmt.dyn.target orelse frameInvariant("a record with no target"), .fields = op.record.fields };
}

/// The capture container `stmt` reads out of `capture`, an erased-callable
/// procedure's capture argument: a borrowed load of the capture record, or a
/// view over it.
fn captureContainer(store: *const LirStore, stmt: LIR.CFStmt, capture: LocalId) ?LocalId {
    if (stmt == .assign_low_level) {
        const s = stmt.assign_low_level;
        if (s.op != .erased_capture_load_borrowed) return null;
        const args = store.getLocalSpan(s.args);
        if (GuardedList.borrowLen(args) == 0 or GuardedList.at(args, 0) != capture) return null;
        return s.target;
    }
    if (stmt != .dyn) return null;
    const op = store.stmtRow(stmt.dyn.data);
    if (op != .view or op.view.address != capture) return null;
    return stmt.dyn.target orelse frameInvariant("a view with no target");
}

/// Collects the definitions of the locals in `extra`, and of function
/// values when `erased` is set.
const DefCollector = struct {
    allocator: Allocator,
    extra: *const collections.DenseMapUnmanaged(LocalId, void),
    out: *std.ArrayList(LocalStmt),
    erased: ?struct { store: *const LirStore, layouts: *const layout_mod.Store } = null,
    stmt: CFStmtId = undefined,
    failure: ?Allocator.Error = null,

    fn note(self: *DefCollector, local: LocalId) void {
        if (self.failure != null) return;
        const tracked = self.extra.contains(local) or if (self.erased) |e| isErased(e.store, e.layouts, local) else false;
        if (!tracked) return;
        self.out.append(self.allocator, .{ .local = local, .stmt = self.stmt }) catch |err| {
            self.failure = err;
        };
    }
};

fn isRcStmt(stmt: LIR.CFStmt) bool {
    return stmt == .incref or stmt == .decref or stmt == .free;
}

/// The capture field `stmt` reads out of `container`, if it reads one by
/// its index.
fn fieldRead(store: *const LirStore, stmt: LIR.CFStmt, container: LocalId) ?FieldRead {
    if (stmt == .assign_ref) {
        const r = stmt.assign_ref;
        if (r.op.kind != .field or r.op.source != container) return null;
        return .{ .index = r.op.index, .local = r.target };
    }
    if (stmt != .dyn) return null;
    const op = store.stmtRow(stmt.dyn.data);
    if (op != .field_addr or op.field_addr.base != container or op.field_addr.field_index != .immediate) return null;
    return .{ .index = op.field_addr.field_index.immediate, .local = stmt.dyn.target orelse frameInvariant("a field read with no target") };
}

/// Record every statement of `spec`'s body that reads a function-valued
/// local, a record built from one, or the procedure's capture, and the
/// procedure's reads of its capture fields, into `arena`.
fn collectUses(
    allocator: Allocator,
    arena: Allocator,
    scratch: *Scratch,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    spec: *const LIR.LirProcSpec,
    body: CFStmtId,
) Allocator.Error!ProcUses {
    try scratch.walk(allocator, store, body);
    scratch.extra.clearRetainingAtMost(allocator, retained_chunks);
    scratch.containers.clearRetainingCapacity();
    scratch.reads.clearRetainingCapacity();
    scratch.defs.clearRetainingCapacity();
    const capture = spec.erased_capture_arg.get();
    if (capture) |local| try scratch.extra.put(allocator, local, {});
    // The records built from function values and the capture's containers,
    // whose uses are collected beside the function values'.
    for (scratch.stmts.items) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        if (builtRecord(store, stmt)) |built| {
            const fields = store.getLocalSpan(built.fields);
            for (0..fields.len) |index| {
                if (!isErased(store, layouts, GuardedList.at(fields, index))) continue;
                try scratch.extra.put(allocator, built.record, {});
                break;
            }
        }
        if (capture) |local| if (captureContainer(store, stmt, local)) |container| {
            try scratch.extra.put(allocator, container, {});
            try scratch.containers.append(allocator, container);
        };
    }
    var reads: UseCollector = .{ .store = store, .layouts = layouts, .allocator = allocator, .out = &scratch.reads, .extra = &scratch.extra };
    var defs: DefCollector = .{ .allocator = allocator, .extra = &scratch.extra, .out = &scratch.defs };
    const track_defs = scratch.extra.count() != 0;
    for (scratch.stmts.items) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        reads.stmt = stmt_id;
        BodyClone.forEachStmtRead(store, stmt, &reads, UseCollector.note);
        if (reads.failure) |err| return err;
        if (!track_defs) continue;
        defs.stmt = stmt_id;
        BodyClone.forEachStmtDef(store, stmt, &defs, DefCollector.note);
        // A store writes its destination in place.
        if (BodyClone.writtenLocal(stmt)) |written| if (BodyClone.definedTarget(stmt) == null) defs.note(written);
        if (defs.failure) |err| return err;
    }
    var uses: ProcUses = .{
        .reads = try LocalStmts.build(arena, scratch.reads.items, true),
        .record_defs = try LocalStmts.build(arena, scratch.defs.items, false),
    };

    const capture_local = capture orelse return uses;
    // The capture is only read into containers, and the containers only
    // have fields read out of them; otherwise any field may escape.
    for (uses.of(capture_local)) |stmt_id| {
        if (captureContainer(store, store.getCFStmt(stmt_id), capture_local) == null) return uses;
    }
    scratch.fields.clearRetainingCapacity();
    for (scratch.containers.items) |container| {
        // A dynamic record's load reads its descriptor, which holds no
        // field's value.
        if (store.localLayout(container) == .opaque_ptr and store.localDyn(container) == null) continue;
        for (uses.of(container)) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (isRcStmt(stmt)) continue;
            const field = fieldRead(store, stmt, container) orelse return uses;
            if (isErased(store, layouts, field.local)) try scratch.fields.append(allocator, field);
        }
    }
    uses.fields = try arena.dupe(FieldRead, scratch.fields.items);
    return uses;
}

/// How one statement uses a function value.
pub const Use = union(enum) {
    /// A retain or release, which does nothing to a frame callable.
    rc,
    /// An alias (`assign_ref` of the local or a transparent nominal, or a
    /// `set_local`) whose target holds the same callable.
    alias: LocalId,
    /// A call of the value. `deferred` calls are made after the procedure
    /// returns; `transfers` calls hand the callable to the callee through
    /// the erased ABI's reuse channel.
    call: struct { deferred: bool, transfers: bool },
    /// The value passed to a direct call, at every position `positions`
    /// names (positions at or past `mask_positions` escape).
    direct_arg: struct { callee: ProcId, positions: u64, replaces_frame: bool },
    /// The value stored as field `field` of the record `record`.
    captured: struct { record: LocalId, field: u32 },
    /// Anything that can let the value outlive the call: a return, a store
    /// into an aggregate other than as one field of a record, an argument
    /// to an erased call, a repack, a low-level operation, or a conditional
    /// release.
    escape,
};

/// Classify `stmt_id`'s use of `local`, which it reads.
pub fn classifyUse(store: *const LirStore, stmt_id: CFStmtId, local: LocalId) Use {
    const stmt = store.getCFStmt(stmt_id);
    return switch (stmt) {
        .incref => .rc,
        .decref => .rc,
        .free => .rc,
        .assign_ref => |s| switch (s.op.kind) {
            .local, .nominal => .{ .alias = s.target },
            .discriminant, .field, .tag_payload, .tag_payload_struct, .list_reinterpret => .escape,
        },
        .set_local => |s| if (s.value == local) .{ .alias = s.target } else .escape,
        .assign_call => |s| blk: {
            const args = store.getLocalSpan(s.args);
            var positions: u64 = 0;
            for (0..args.len) |position| {
                if (GuardedList.at(args, position) != local) continue;
                if (position >= mask_positions) break :blk .escape;
                positions |= @as(u64, 1) << @intCast(position);
            }
            if (positions == 0) frameInvariant("a direct call classified for a local it does not pass");
            break :blk .{ .direct_arg = .{
                .callee = s.proc,
                .positions = positions,
                .replaces_frame = store.stmtRowOrDefault(s.extra).replaces_frame,
            } };
        },
        .assign_call_erased => |s| blk: {
            const call = store.stmtRow(s.data);
            const args = store.getLocalSpan(call.args);
            for (0..args.len) |position| {
                if (GuardedList.at(args, position) == local) break :blk .escape;
            }
            break :blk .{ .call = .{ .deferred = call.deferred, .transfers = call.reuse_closure } };
        },
        .assign_struct => |s| capturedUse(store, s.target, s.fields, local),
        .dyn => |s| switch (store.stmtRow(s.data)) {
            .call_erased => |call| if (call.args != null and call.args.? == local)
                .escape
            else
                .{ .call = .{ .deferred = call.deferred, .transfers = call.reuse_closure } },
            .record => |r| if (r.desc == local)
                .escape
            else
                capturedUse(store, s.target orelse frameInvariant("a record with no target"), r.fields, local),
            .frame_alloc,
            .table_entry,
            .copy,
            .field_addr,
            .list_item_addr,
            .box_payload_addr,
            .tag_discriminant,
            .tag_payload_addr,
            .record_update,
            .tag,
            .list,
            .box,
            .slot_alloc,
            .slot_store,
            .eq,
            .hash,
            .inspect,
            .closure,
            .view,
            .adapt,
            => .escape,
        },
        .init_uninitialized,
        .assign_literal,
        .assign_packed_erased_fn,
        .assign_low_level,
        .assign_list,
        .assign_tag,
        .store_struct,
        .store_tag,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .decref_if_initialized,
        .switch_stmt,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .join,
        .jump,
        .ret,
        .crash,
        => .escape,
    };
}

/// A store of `local` into the record `record` built from `fields`: one
/// field, or an escape when it is several.
fn capturedUse(store: *const LirStore, record: LocalId, fields: LIR.LocalSpan, local: LocalId) Use {
    const operands = store.getLocalSpan(fields);
    var found: ?u32 = null;
    for (0..operands.len) |index| {
        if (GuardedList.at(operands, index) != local) continue;
        if (found != null) return .escape;
        found = @intCast(index);
    }
    return .{ .captured = .{ .record = record, .field = found orelse frameInvariant("a record classified for a local it does not hold") } };
}

/// The procedure a packing statement builds a callable of.
fn packingProc(store: *const LirStore, stmt_id: CFStmtId) ProcId {
    const stmt = store.getCFStmt(stmt_id);
    if (stmt == .assign_packed_erased_fn) return store.stmtRow(stmt.assign_packed_erased_fn.data).proc;
    if (stmt != .dyn) frameInvariant("a packing that is not a packing statement");
    return store.stmtRow(stmt.dyn.data).closure.proc;
}

/// The masks a call reads for `callee`: the solved masks of a procedure
/// with a body, or the ones its defining object published.
fn calleeMask(store: *const LirStore, masks: []const u64, callee: ProcId) u64 {
    const spec = store.getProcSpecImmediate(callee);
    if (spec.body != .none) return masks[@backingInt(callee)];
    return spec.nonescaping_fn_params;
}

fn calleeReuseMask(store: *const LirStore, reuse_masks: []const u64, callee: ProcId) u64 {
    const spec = store.getProcSpecImmediate(callee);
    if (spec.body != .none) return reuse_masks[@backingInt(callee)];
    return spec.capture_reuse_fn_params;
}

const ParamSolver = struct {
    allocator: Allocator,
    store: *const LirStore,
    masks: []u64,
    reuse_masks: []u64,
    /// Per erased-callable procedure, the capture fields its body keeps
    /// (bit `i` for field `i`), and those a call may hand over through the
    /// reuse channel.
    field_masks: []u64,
    field_reuse_masks: []u64,
    members: collections.DenseMapUnmanaged(LocalId, void) = .empty,
    work: std.ArrayList(LocalId) = .empty,

    fn fieldMask(self: *const ParamSolver, proc: ProcId) u64 {
        // A procedure another object defines publishes no capture fields.
        if (self.store.getProcSpecImmediate(proc).body == .none) return 0;
        return self.field_masks[@backingInt(proc)];
    }

    fn fieldReuseMask(self: *const ParamSolver, proc: ProcId) u64 {
        if (self.store.getProcSpecImmediate(proc).body == .none) return ~@as(u64, 0);
        return self.field_reuse_masks[@backingInt(proc)];
    }

    /// The packings of the record `record`, when every use of it is a
    /// retain, a release or the capture of a packing, and one statement
    /// defines it; null otherwise.
    fn recordPackings(self: *const ParamSolver, uses: *const ProcUses, record: LocalId) ?[]const CFStmtId {
        if (uses.recordDefs(record) != 1) return null;
        const record_uses = uses.of(record);
        for (record_uses) |stmt_id| {
            if (isRcStmt(self.store.getCFStmt(stmt_id))) continue;
            const packing = Packing.of(self.store, stmt_id) orelse return null;
            if (packing.capture != record) return null;
        }
        return record_uses;
    }

    fn deinit(self: *ParamSolver) void {
        self.members.deinit(self.allocator);
        self.work.deinit(self.allocator);
    }

    /// Whether the value `param` holds on entry can outlive the call under
    /// the current masks. `deferred_ok` says a deferred call of it is made
    /// before the procedure's own invocation completes: for a capture field
    /// of a procedure the erased-call runtime invokes, which makes a pending
    /// call its body leaves before that invocation returns, while the
    /// field's value is still the one the invoking frame keeps alive.
    fn escapes(self: *ParamSolver, uses: *const ProcUses, param: LocalId, deferred_ok: bool) Allocator.Error!bool {
        self.members.clearRetainingCapacity();
        self.work.clearRetainingCapacity();
        try self.members.put(self.allocator, param, {});
        try self.work.append(self.allocator, param);
        while (self.work.pop()) |member| {
            for (uses.of(member)) |stmt_id| {
                switch (classifyUse(self.store, stmt_id, member)) {
                    .rc => {},
                    .alias => |target| {
                        // Whatever else the target holds, the value only
                        // escapes through what the target's uses do.
                        if (!(try self.members.getOrPut(self.allocator, target)).found_existing) {
                            try self.work.append(self.allocator, target);
                        }
                    },
                    .call => |call| if (call.deferred and !deferred_ok) return true,
                    .direct_arg => |arg| {
                        if ((calleeMask(self.store, self.masks, arg.callee) & arg.positions) != arg.positions) return true;
                    },
                    .captured => |cap| {
                        // The value stays inside each callable packed from
                        // the record, which must not escape, and whose body
                        // must keep the field.
                        if (cap.field >= mask_positions) return true;
                        const record_uses = self.recordPackings(uses, cap.record) orelse return true;
                        for (record_uses) |use| {
                            const packing = Packing.of(self.store, use) orelse continue;
                            if ((self.fieldMask(packingProc(self.store, use)) >> @intCast(cap.field)) & 1 == 0) return true;
                            if (!(try self.members.getOrPut(self.allocator, packing.target)).found_existing) {
                                try self.work.append(self.allocator, packing.target);
                            }
                        }
                    },
                    .escape => return true,
                }
            }
        }
        return false;
    }

    /// Whether a call may hand the value `param` holds on entry over to its
    /// callee through the reuse channel, under the current reuse masks.
    fn reusesCapture(self: *ParamSolver, uses: *const ProcUses, param: LocalId) Allocator.Error!bool {
        self.members.clearRetainingCapacity();
        self.work.clearRetainingCapacity();
        try self.members.put(self.allocator, param, {});
        try self.work.append(self.allocator, param);
        while (self.work.pop()) |member| {
            for (uses.of(member)) |stmt_id| {
                switch (classifyUse(self.store, stmt_id, member)) {
                    .rc, .escape => {},
                    .alias => |target| {
                        if (!(try self.members.getOrPut(self.allocator, target)).found_existing) {
                            try self.work.append(self.allocator, target);
                        }
                    },
                    .call => |call| if (call.transfers) return true,
                    .direct_arg => |arg| {
                        if ((calleeReuseMask(self.store, self.reuse_masks, arg.callee) & arg.positions) != 0) return true;
                    },
                    .captured => |cap| {
                        // Handing over a callable packed from the record
                        // hands over the callable, not this value: only the
                        // packed body's own use of the field counts.
                        if (cap.field >= mask_positions) return true;
                        for (uses.of(cap.record)) |use| {
                            if (Packing.of(self.store, use) == null) continue;
                            if ((self.fieldReuseMask(packingProc(self.store, use)) >> @intCast(cap.field)) & 1 != 0) return true;
                        }
                    },
                }
            }
        }
        return false;
    }
};

/// The candidate positions of a procedure: its function-valued parameters
/// within the mask's reach, other than the erased ABI's reuse input, which
/// the body repacks or releases.
fn candidatePositions(store: *const LirStore, layouts: *const layout_mod.Store, spec: *const LIR.LirProcSpec) u64 {
    const args = store.getLocalSpan(spec.args);
    var mask: u64 = 0;
    for (0..@min(args.len, mask_positions)) |position| {
        const param = GuardedList.at(args, position);
        if (spec.erased_reuse_arg.get()) |reuse| if (reuse == param) continue;
        if (isErased(store, layouts, param)) mask |= @as(u64, 1) << @intCast(position);
    }
    return mask;
}

/// Solve `nonescaping_fn_params` and `capture_reuse_fn_params` for every
/// procedure of `store` with a body. The reuse mask is a least fixpoint:
/// every bit starts clear and is set by a transferring call of the
/// parameter or a pass to a callee position whose bit is set.
pub fn solveParams(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
    published: []const PublishedPair,
) Allocator.Error!void {
    const proc_count = store.procSpecCount();
    const masks = try allocator.alloc(u64, proc_count);
    defer allocator.free(masks);
    @memset(masks, 0);
    const reuse_masks = try allocator.alloc(u64, proc_count);
    defer allocator.free(reuse_masks);
    @memset(reuse_masks, 0);
    const candidate_masks = try allocator.alloc(u64, proc_count);
    defer allocator.free(candidate_masks);
    @memset(candidate_masks, 0);
    const field_masks = try allocator.alloc(u64, proc_count);
    defer allocator.free(field_masks);
    @memset(field_masks, 0);
    const field_reuse_masks = try allocator.alloc(u64, proc_count);
    defer allocator.free(field_reuse_masks);
    @memset(field_reuse_masks, 0);

    var candidates = std.ArrayList(u32).empty;
    defer candidates.deinit(allocator);
    var proc_uses = std.ArrayList(ProcUses).empty;
    defer proc_uses.deinit(allocator);
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    var scratch: Scratch = .{};
    defer scratch.deinit(allocator);

    // The candidates: every procedure with a function-valued parameter,
    // and every erased-callable procedure a candidate packs a record built
    // from a function value with, whose capture fields that record's
    // escape reads. No other procedure's capture fields are ever read.
    const queued = try allocator.alloc(bool, proc_count);
    defer allocator.free(queued);
    @memset(queued, false);
    var pending = std.ArrayList(u32).empty;
    defer pending.deinit(allocator);
    for (0..proc_count) |index| {
        const spec = store.getProcSpecImmediate(@fromBackingInt(@intCast(index)));
        if (spec.body == .none or candidatePositions(store, layouts, spec) == 0) continue;
        queued[index] = true;
        try pending.append(allocator, @intCast(index));
    }
    while (pending.pop()) |index| {
        const spec = store.getProcSpecImmediate(@fromBackingInt(index));
        const body = spec.body.get() orelse frameInvariant("a queued candidate with no body");
        const mask = candidatePositions(store, layouts, spec);
        masks[index] = mask;
        candidate_masks[index] = mask;
        try candidates.append(allocator, index);
        const uses = try collectUses(allocator, arena_state.allocator(), &scratch, store, layouts, spec, body);
        try proc_uses.append(allocator, uses);
        // Every field a body never reads into a function value it keeps.
        if (uses.fields != null) field_masks[index] = ~@as(u64, 0);
        for (uses.record_defs.locals) |record| {
            for (uses.of(record)) |stmt_id| {
                const packing = Packing.of(store, stmt_id) orelse continue;
                if (packing.capture != record) continue;
                const lambda = @backingInt(packingProc(store, stmt_id));
                if (queued[lambda] or store.getProcSpecImmediate(@fromBackingInt(lambda)).body == .none) continue;
                queued[lambda] = true;
                try pending.append(allocator, lambda);
            }
        }
    }

    var solver: ParamSolver = .{
        .allocator = allocator,
        .store = store,
        .masks = masks,
        .reuse_masks = reuse_masks,
        .field_masks = field_masks,
        .field_reuse_masks = field_reuse_masks,
    };
    defer solver.deinit();

    var changed = true;
    while (changed) {
        changed = false;
        for (candidates.items, proc_uses.items) |index, *uses| {
            const args = store.getLocalSpan(store.getProcSpecImmediate(@fromBackingInt(index)).args);
            var remaining = masks[index];
            while (remaining != 0) {
                const position: u6 = @intCast(@ctz(remaining));
                remaining &= remaining - 1;
                if (try solver.escapes(uses, GuardedList.at(args, position), false)) {
                    masks[index] &= ~(@as(u64, 1) << position);
                    changed = true;
                }
            }
            const deferred_ok = switch (store.getProcSpecImmediate(@fromBackingInt(index)).abi) {
                .erased_callable, .erased_record => true,
                .roc => false,
            };
            if (uses.fields) |fields| for (fields) |field| {
                if (field.index >= mask_positions) continue;
                const bit = @as(u64, 1) << @intCast(field.index);
                if (field_masks[index] & bit == 0) continue;
                if (try solver.escapes(uses, field.local, deferred_ok)) {
                    field_masks[index] &= ~bit;
                    changed = true;
                }
            };
        }
        for (published) |pair| {
            const worker = @backingInt(pair.worker);
            const variant = @backingInt(pair.variant);
            const shared = masks[worker] & masks[variant];
            if (masks[worker] != shared or masks[variant] != shared) {
                masks[worker] = shared;
                masks[variant] = shared;
                changed = true;
            }
        }
    }

    changed = true;
    while (changed) {
        changed = false;
        for (candidates.items, proc_uses.items) |index, *uses| {
            const args = store.getLocalSpan(store.getProcSpecImmediate(@fromBackingInt(index)).args);
            var remaining = candidate_masks[index] & ~reuse_masks[index];
            while (remaining != 0) {
                const position: u6 = @intCast(@ctz(remaining));
                remaining &= remaining - 1;
                if (try solver.reusesCapture(uses, GuardedList.at(args, position))) {
                    reuse_masks[index] |= @as(u64, 1) << position;
                    changed = true;
                }
            }
            if (uses.fields) |fields| for (fields) |field| {
                if (field.index >= mask_positions) continue;
                const bit = @as(u64, 1) << @intCast(field.index);
                if (field_reuse_masks[index] & bit != 0) continue;
                if (try solver.reusesCapture(uses, field.local)) {
                    field_reuse_masks[index] |= bit;
                    changed = true;
                }
            };
        }
        for (published) |pair| {
            const worker = @backingInt(pair.worker);
            const variant = @backingInt(pair.variant);
            const shared = reuse_masks[worker] | reuse_masks[variant];
            if (reuse_masks[worker] != shared or reuse_masks[variant] != shared) {
                reuse_masks[worker] = shared;
                reuse_masks[variant] = shared;
                changed = true;
            }
        }
    }

    for (0..proc_count) |index| {
        const spec = store.getProcSpecPtr(@fromBackingInt(@intCast(index)));
        if (spec.body == .none) continue;
        spec.nonescaping_fn_params = masks[index];
        spec.capture_reuse_fn_params = reuse_masks[index];
    }
}

/// One procedure's flow graph: its statements control reaches from the body,
/// their flow successors (a jump's is its join's body), and their
/// predecessors.
const FlowGraph = struct {
    stmts: std.ArrayList(CFStmtId) = .empty,
    index: collections.DenseMapUnmanaged(CFStmtId, u32) = .empty,
    succ_start: std.ArrayList(u32) = .empty,
    succs: std.ArrayList(u32) = .empty,
    pred_start: std.ArrayList(u32) = .empty,
    preds: std.ArrayList(u32) = .empty,
    join_bodies: collections.DenseMapUnmanaged(LIR.JoinPointId, CFStmtId) = .empty,
    scratch: std.ArrayList(CFStmtId) = .empty,
    walk: std.ArrayList(CFStmtId) = .empty,
    pred_counts: std.ArrayList(u32) = .empty,

    fn deinit(self: *FlowGraph, allocator: Allocator) void {
        self.join_bodies.deinit(allocator);
        self.scratch.deinit(allocator);
        self.walk.deinit(allocator);
        self.pred_counts.deinit(allocator);
        self.stmts.deinit(allocator);
        self.index.deinit(allocator);
        self.succ_start.deinit(allocator);
        self.succs.deinit(allocator);
        self.pred_start.deinit(allocator);
        self.preds.deinit(allocator);
    }

    fn successors(self: *const FlowGraph, node: u32) []const u32 {
        return self.succs.items[self.succ_start.items[node]..self.succ_start.items[node + 1]];
    }

    fn predecessors(self: *const FlowGraph, node: u32) []const u32 {
        return self.preds.items[self.pred_start.items[node]..self.pred_start.items[node + 1]];
    }

    /// Build the graph of `body`, whose reachable statements are
    /// `reachable`, reusing this graph's storage.
    fn build(self: *FlowGraph, allocator: Allocator, store: *const LirStore, body: CFStmtId, reachable: []const CFStmtId) Allocator.Error!void {
        self.stmts.clearRetainingCapacity();
        self.index.clearRetainingAtMost(allocator, retained_chunks);
        self.succ_start.clearRetainingCapacity();
        self.succs.clearRetainingCapacity();
        self.pred_start.clearRetainingCapacity();
        self.preds.clearRetainingCapacity();
        self.join_bodies.clearRetainingAtMost(allocator, retained_chunks);
        for (reachable) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .join) try self.join_bodies.put(allocator, stmt.join.id, stmt.join.body);
        }
        const join_bodies = &self.join_bodies;
        const scratch = &self.scratch;
        const work = &self.walk;
        work.clearRetainingCapacity();
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            const entry = try self.index.getOrPut(allocator, stmt_id);
            if (entry.found_existing) continue;
            entry.value_ptr.* = @intCast(self.stmts.items.len);
            try self.stmts.append(allocator, stmt_id);
            try flowSuccessors(allocator, store, join_bodies, stmt_id, scratch);
            try work.appendSlice(allocator, scratch.items);
        }
        const count = self.stmts.items.len;
        try self.pred_counts.resize(allocator, count);
        const pred_counts = self.pred_counts.items;
        @memset(pred_counts, 0);
        for (self.stmts.items) |stmt_id| {
            try self.succ_start.append(allocator, @intCast(self.succs.items.len));
            try flowSuccessors(allocator, store, join_bodies, stmt_id, scratch);
            for (scratch.items) |successor| {
                const node = self.index.get(successor) orelse frameInvariant("a flow successor outside its procedure's flow graph");
                try self.succs.append(allocator, node);
                pred_counts[node] += 1;
            }
        }
        try self.succ_start.append(allocator, @intCast(self.succs.items.len));
        try self.pred_start.ensureTotalCapacity(allocator, count + 1);
        var running: u32 = 0;
        for (pred_counts) |pred_count| {
            self.pred_start.appendAssumeCapacity(running);
            running += pred_count;
        }
        self.pred_start.appendAssumeCapacity(running);
        try self.preds.resize(allocator, running);
        @memset(pred_counts, 0);
        for (0..count) |node| {
            for (self.successors(@intCast(node))) |successor| {
                self.preds.items[self.pred_start.items[successor] + pred_counts[successor]] = @intCast(node);
                pred_counts[successor] += 1;
            }
        }
    }
};

const SuccessorCollector = struct {
    allocator: Allocator,
    out: *std.ArrayList(CFStmtId),

    fn visit(self: *SuccessorCollector, successor: CFStmtId) Allocator.Error!void {
        try self.out.append(self.allocator, successor);
    }
};

/// The statements control can move to directly from `stmt_id`: its
/// structural flow edges (`forEachFlowSuccessor`), and for a jump its join's
/// body.
fn flowSuccessors(
    allocator: Allocator,
    store: *const LirStore,
    join_bodies: *const collections.DenseMapUnmanaged(LIR.JoinPointId, CFStmtId),
    stmt_id: CFStmtId,
    out: *std.ArrayList(CFStmtId),
) Allocator.Error!void {
    out.clearRetainingCapacity();
    const stmt = store.getCFStmt(stmt_id);
    if (stmt == .jump) {
        try out.append(allocator, join_bodies.get(stmt.jump.target) orelse frameInvariant("a jump to a join its procedure does not define"));
        return;
    }
    var collector: SuccessorCollector = .{ .allocator = allocator, .out = out };
    try BodyClone.forEachFlowSuccessor(store, stmt, &collector, SuccessorCollector.visit);
}

/// A qualifying packing and what rewriting it into a frame packing changes.
const FrameSite = struct {
    pack: CFStmtId,
    /// The packing's target and its aliases, sorted.
    group: []LocalId,
    /// The capture record, when it holds reference-counted data that the
    /// callable's death must release.
    release: ?LocalId,
    /// Flow edges, as statement pairs, after which the callable is dead.
    deaths: [][2]CFStmtId,
    /// A `DynOp.closure`, whose frame storage is a new `frame_callable`
    /// local.
    dynamic: bool,
};

/// The facts of one procedure every candidate packing reads.
const ProcFacts = struct {
    /// The statements reading each function-valued local.
    uses: ProcUses = .{},
    /// Statements defining each function-valued or capture-record local.
    defs: LocalStmts = .{},
    packs: []const CFStmtId = &.{},

    fn defsOf(self: *const ProcFacts, local: LocalId) []const CFStmtId {
        return self.defs.of(local);
    }
};

/// What a packing statement builds: an `assign_packed_erased_fn`, or a
/// `DynOp.closure` whose capture is a dynamic record.
const Packing = struct {
    target: LocalId,
    capture: ?LocalId,
    reuse: ?LocalId,
    dynamic: bool,
    frame: bool,

    fn of(store: *const LirStore, stmt_id: CFStmtId) ?Packing {
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_packed_erased_fn) {
            const data = store.stmtRow(stmt.assign_packed_erased_fn.data);
            return .{ .target = stmt.assign_packed_erased_fn.target, .capture = data.capture.get(), .reuse = data.reuse.get(), .dynamic = false, .frame = data.frame };
        }
        if (stmt != .dyn) return null;
        const op = store.stmtRow(stmt.dyn.data);
        if (op != .closure) return null;
        return .{
            .target = stmt.dyn.target orelse frameInvariant("a closure with no target"),
            .capture = op.closure.capture,
            .reuse = op.closure.reuse,
            .dynamic = true,
            .frame = op.closure.frame != null,
        };
    }
};

/// Collect the uses, packings and relevant definitions of one body into
/// `arena`; `scratch.stmts` then holds the body's statements.
fn collectProcFacts(
    allocator: Allocator,
    arena: Allocator,
    scratch: *Scratch,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    body: CFStmtId,
) Allocator.Error!ProcFacts {
    try scratch.walk(allocator, store, body);
    scratch.extra.clearRetainingAtMost(allocator, retained_chunks);
    scratch.work.clearRetainingCapacity();
    for (scratch.stmts.items) |stmt_id| {
        const packing = Packing.of(store, stmt_id) orelse continue;
        try scratch.work.append(allocator, stmt_id);
        if (packing.capture) |capture| try scratch.extra.put(allocator, capture, {});
    }
    if (scratch.work.items.len == 0) return .{};
    const packs = try arena.dupe(CFStmtId, scratch.work.items);
    scratch.reads.clearRetainingCapacity();
    scratch.defs.clearRetainingCapacity();
    const no_extra = collections.DenseMapUnmanaged(LocalId, void).empty;
    var reads: UseCollector = .{ .store = store, .layouts = layouts, .allocator = allocator, .out = &scratch.reads, .extra = &no_extra };
    var defs: DefCollector = .{ .allocator = allocator, .extra = &scratch.extra, .out = &scratch.defs, .erased = .{ .store = store, .layouts = layouts } };
    for (scratch.stmts.items) |stmt_id| {
        const stmt = store.getCFStmt(stmt_id);
        reads.stmt = stmt_id;
        BodyClone.forEachStmtRead(store, stmt, &reads, UseCollector.note);
        if (reads.failure) |err| return err;
        defs.stmt = stmt_id;
        BodyClone.forEachStmtDef(store, stmt, &defs, DefCollector.note);
        // A store writes its destination in place.
        if (BodyClone.writtenLocal(stmt)) |written| if (BodyClone.definedTarget(stmt) == null) defs.note(written);
        if (defs.failure) |err| return err;
    }
    return .{
        .uses = .{ .reads = try LocalStmts.build(arena, scratch.reads.items, true) },
        .defs = try LocalStmts.build(arena, scratch.defs.items, false),
        .packs = packs,
    };
}

const SiteSolver = struct {
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    facts: *const ProcFacts,
    graph: *const FlowGraph,
    live: std.DynamicBitSetUnmanaged = .{},
    work: std.ArrayList(u32) = .empty,
    group: std.ArrayList(LocalId) = .empty,
    /// Set by `usesQualify` when some use may hand the heap callable to a
    /// callee that would then find its captures unshared.
    capture_reuse: bool = false,

    fn deinit(self: *SiteSolver) void {
        self.live.deinit(self.allocator);
        self.work.deinit(self.allocator);
        self.group.deinit(self.allocator);
    }

    fn inGroup(self: *const SiteSolver, local: LocalId) bool {
        return std.mem.findScalar(LocalId, self.group.items, local) != null;
    }

    /// Whether `pack` builds a callable whose every use keeps it within
    /// the procedure's own frame; fills `group` with its target and aliases.
    fn usesQualify(self: *SiteSolver, pack: CFStmtId) Allocator.Error!bool {
        const target = (Packing.of(self.store, pack) orelse frameInvariant("a frame candidate that is not a packing")).target;
        self.capture_reuse = false;
        self.group.clearRetainingCapacity();
        try self.group.append(self.allocator, target);
        var next: usize = 0;
        while (next < self.group.items.len) : (next += 1) {
            const member = self.group.items[next];
            // Every member holds this packing's callable and nothing else:
            // one definition, the packing or an alias of a member.
            const defs = self.facts.defsOf(member);
            if (defs.len != 1) return false;
            for (self.facts.uses.of(member)) |stmt_id| {
                switch (classifyUse(self.store, stmt_id, member)) {
                    .rc => {},
                    .alias => |alias| {
                        if (self.store.getCFStmt(stmt_id) != .assign_ref) return false;
                        if (!self.inGroup(alias)) try self.group.append(self.allocator, alias);
                    },
                    .call => |call| {
                        if (call.deferred) return false;
                        if (call.transfers) self.capture_reuse = true;
                    },
                    .direct_arg => |arg| {
                        if (arg.replaces_frame) return false;
                        const callee = self.store.getProcSpecImmediate(arg.callee);
                        if ((callee.nonescaping_fn_params & arg.positions) != arg.positions) return false;
                        if ((callee.capture_reuse_fn_params & arg.positions) != 0) self.capture_reuse = true;
                    },
                    // A callable captured by another one is live as long
                    // as that one, which this packing's liveness does not
                    // follow.
                    .captured, .escape => return false,
                }
            }
        }
        return true;
    }

    /// Solve where the callable `pack` builds is live: a statement is live
    /// when a path from it reaches a use of the group without passing the
    /// packing again.
    fn solveLiveness(self: *SiteSolver, pack_node: u32) Allocator.Error!void {
        const count = self.graph.stmts.items.len;
        try self.live.resize(self.allocator, count, false);
        self.live.unsetAll();
        self.work.clearRetainingCapacity();
        for (self.group.items) |member| {
            for (self.facts.uses.of(member)) |stmt_id| {
                const node = self.graph.index.get(stmt_id) orelse continue;
                if (self.live.isSet(node)) continue;
                self.live.set(node);
                try self.work.append(self.allocator, node);
            }
        }
        while (self.work.pop()) |node| {
            for (self.graph.predecessors(node)) |pred| {
                if (pred == pack_node or self.live.isSet(pred)) continue;
                self.live.set(pred);
                try self.work.append(self.allocator, pred);
            }
        }
    }

    fn liveAfter(self: *const SiteSolver, node: u32) bool {
        for (self.graph.successors(node)) |successor| if (self.live.isSet(successor)) return true;
        return false;
    }
};

/// Decide whether `pack`, whose uses qualify (`usesQualify`, which last
/// filled the solver's group), qualifies, and if so its site.
fn solveSite(solver: *SiteSolver, arena: Allocator, pack: CFStmtId) Allocator.Error!?FrameSite {
    const store = solver.store;
    const packing = Packing.of(store, pack) orelse frameInvariant("a frame candidate that is not a packing");
    const graph = solver.graph;
    // A packing control never reaches builds nothing.
    const pack_node = graph.index.get(pack) orelse return null;
    try solver.solveLiveness(pack_node);

    var release: ?LocalId = null;
    if (packing.capture) |capture| {
        // A dynamic record's frame storage is sized by its descriptor when
        // the frame region is placed, so the record must be a frame local,
        // whose descriptor is known by then and whose bytes stay put.
        if (packing.dynamic) {
            const dyn = store.localDyn(capture) orelse frameInvariant("a closure whose capture record is not dynamic");
            if (dyn.storage != .frame) return null;
        }
        if (solver.layouts.layoutContainsRefcounted(solver.layouts.getLayout(store.localLayout(capture)))) {
            // A callee handed the heap callable as it dies finds the
            // captures unshared and can update them in place; a frame
            // callable's captures stay shared with the record.
            if (solver.capture_reuse) return null;
            if (solver.inGroup(capture)) return null;
            // The callable's death releases its capture through the record,
            // whose bytes must still be the ones packed: no statement may
            // write the record while the callable is live.
            for (solver.facts.defsOf(capture)) |def| {
                const node = graph.index.get(def) orelse continue;
                if (solver.liveAfter(node)) return null;
            }
            release = capture;
        }
    }

    var deaths = std.ArrayList([2]CFStmtId).empty;
    for (graph.stmts.items, 0..) |stmt_id, node_index| {
        const node: u32 = @intCast(node_index);
        if (node != pack_node and !solver.live.isSet(node)) continue;
        const first = deaths.items.len;
        for (graph.successors(node)) |successor| {
            if (solver.live.isSet(successor)) continue;
            const edge: [2]CFStmtId = .{ stmt_id, graph.stmts.items[successor] };
            // Two branches of one switch can share a target: one edge.
            for (deaths.items[first..]) |seen| {
                if (seen[1] == edge[1]) break;
            } else try deaths.append(arena, edge);
        }
    }
    const group = try arena.dupe(LocalId, solver.group.items);
    std.mem.sort(LocalId, group, {}, BodyClone.localIdLessThan);
    return .{ .pack = pack, .group = group, .release = release, .deaths = try deaths.toOwnedSlice(arena), .dynamic = packing.dynamic };
}

/// Point the flow edge of `stmt_id` that leads to `from` at `to`. Only flow
/// edges move: a switch's continuation is release-placement metadata the
/// arms reach through their own chains.
fn redirectFlowEdge(store: *LirStore, stmt_id: CFStmtId, from: CFStmtId, to: CFStmtId) void {
    var moved = false;
    switch (store.getCFStmtPtr(stmt_id).*) {
        inline .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .dyn,
        => |*s| if (s.next == from) {
            s.next = to;
            moved = true;
        },
        .switch_stmt => |*s| {
            if (s.default_branch == from) {
                s.default_branch = to;
                moved = true;
            }
            const branches = store.getCFSwitchBranchesMut(s.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.atPtr(branches, index);
                if (branch.body == from) {
                    branch.body = to;
                    moved = true;
                }
            }
        },
        .switch_initialized_payload => |*s| {
            if (s.initialized_branch == from) {
                s.initialized_branch = to;
                moved = true;
            }
            if (s.uninitialized_branch == from) {
                s.uninitialized_branch = to;
                moved = true;
            }
        },
        .str_match => |*s| {
            if (s.on_match == from) {
                s.on_match = to;
                moved = true;
            }
            if (s.on_miss == from) {
                s.on_miss = to;
                moved = true;
            }
        },
        .str_match_set => |*s| {
            if (s.on_miss == from) {
                s.on_miss = to;
                moved = true;
            }
            const arms = store.getStrMatchArmsMut(s.arms);
            for (0..arms.len) |index| {
                const arm = GuardedList.atPtr(arms, index);
                if (arm.on_match == from) {
                    arm.on_match = to;
                    moved = true;
                }
            }
        },
        .join => |*s| if (s.remainder == from) {
            s.remainder = to;
            moved = true;
        },
        // A jump's successor is its join's body, where the callable is
        // live exactly when it is live at the jump, so no death lies on it.
        .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed => {},
    }
    if (!moved) frameInvariant("a death edge that is not a flow edge of its statement");
}

fn rcHelperRef(store: *LirStore, op: layout_mod.RcOp, layout_idx: layout_mod.Idx) Allocator.Error!LIR.RcHelperRef {
    return store.addRcHelperRef(.{ .concrete = .{ .op = op, .layout_idx = layout_idx } });
}

fn releaseOrigin(store: *const LirStore, cause: CFStmtId, subject: LocalId) LIR.StmtOrigin {
    const source = store.stmtOrigin(cause);
    return .{
        .loc = source.loc,
        .region = source.region,
        .inline_scope = source.inline_scope,
        .kind = .{ .arc_decref = .{ .subject_local = subject, .reason = .dead_after_stmt } },
    };
}

/// Rewrite `sites` of one procedure: each packing becomes a frame packing
/// that takes no reuse allocation (releasing it instead), the retains and
/// releases of its callable are dropped, calls of it transfer nothing, and
/// its capture record is released on each edge where the callable dies.
fn rewriteSites(allocator: Allocator, store: *LirStore, proc_id: ProcId, sites: []const FrameSite) Allocator.Error!void {
    // Releases first, while every statement is still where ARC put it.
    // Several callables can die on one edge; their releases form one chain
    // the edge is pointed at once.
    const Death = struct { edge: [2]CFStmtId, release: LocalId, subject: LocalId };
    var deaths = std.ArrayList(Death).empty;
    defer deaths.deinit(allocator);
    for (sites) |site| {
        const release = site.release orelse continue;
        const subject = (Packing.of(store, site.pack) orelse frameInvariant("a frame site that is not a packing")).target;
        for (site.deaths) |edge| try deaths.append(allocator, .{ .edge = edge, .release = release, .subject = subject });
    }
    std.mem.sort(Death, deaths.items, {}, struct {
        fn lessThan(_: void, a: Death, b: Death) bool {
            if (a.edge[0] != b.edge[0]) return @backingInt(a.edge[0]) < @backingInt(b.edge[0]);
            return @backingInt(a.edge[1]) < @backingInt(b.edge[1]);
        }
    }.lessThan);
    var start: usize = 0;
    while (start < deaths.items.len) {
        const edge = deaths.items[start].edge;
        var end = start;
        var head = edge[1];
        while (end < deaths.items.len and std.mem.eql(CFStmtId, &deaths.items[end].edge, &edge)) : (end += 1) {
            const death = deaths.items[end];
            head = try store.addCFStmt(.{ .decref = .{
                .value = death.release,
                .rc = try rcHelperRef(store, .decref, store.localLayout(death.release)),
                .next = head,
            } }, releaseOrigin(store, edge[0], death.subject));
        }
        redirectFlowEdge(store, edge[0], edge[1], head);
        start = end;
    }

    var dropped = collections.DenseMapUnmanaged(CFStmtId, CFStmtId).empty;
    defer dropped.deinit(allocator);
    const body = store.getProcSpecImmediate(proc_id).body.get() orelse frameInvariant("rewriting the frame sites of a procedure with no body");
    {
        var walk = try BodyClone.ReachableStmts.initWithAllocator(store, body, allocator);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            switch (stmt) {
                inline .incref, .decref, .free => |rc| if (siteOf(sites, rc.value) != null) try dropped.put(allocator, stmt_id, rc.next),
                .assign_call_erased => |call_stmt| {
                    var call = store.stmtRow(call_stmt.data);
                    if (siteOf(sites, call.closure) == null) continue;
                    if (!call.reuse_closure and !call.result_reuse) continue;
                    call.reuse_closure = false;
                    call.result_reuse = false;
                    const call_data = try store.addStmtRow(LIR.ErasedCallData, call);
                    store.getCFStmtPtr(stmt_id).assign_call_erased.data = call_data;
                },
                .dyn => |dyn_stmt| {
                    var op = store.stmtRow(dyn_stmt.data);
                    if (op != .call_erased or siteOf(sites, op.call_erased.closure) == null) continue;
                    if (!op.call_erased.reuse_closure and !op.call_erased.result_reuse) continue;
                    op.call_erased.reuse_closure = false;
                    op.call_erased.result_reuse = false;
                    const op_data = try store.addStmtRow(LIR.DynOp, op);
                    store.getCFStmtPtr(stmt_id).dyn.data = op_data;
                },
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_packed_erased_fn,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .decref_if_initialized,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .join,
                .jump,
                .ret,
                .crash,
                => {},
            }
        }
    }

    var frame_locals = std.ArrayList(LocalId).empty;
    defer frame_locals.deinit(allocator);
    for (sites) |site| {
        // The packing consumed one unit of its reuse allocation, which it no
        // longer repacks: release it.
        const packing = Packing.of(store, site.pack) orelse frameInvariant("a frame site that is not a packing");
        if (site.dynamic) {
            const dyn_stmt = store.getCFStmtImmediate(site.pack).dyn;
            var op = store.stmtRow(dyn_stmt.data);
            const capture_dyn = store.localDyn(op.closure.capture) orelse frameInvariant("a closure whose capture record is not dynamic");
            const storage = try store.addLocal(.{
                .layout_idx = store.localLayout(op.closure.capture),
                .dyn = .{ .desc = capture_dyn.desc, .storage = .frame_callable },
            });
            try frame_locals.append(allocator, storage);
            op.closure.frame = storage;
            op.closure.reuse = null;
            op.closure.reuse_unique = false;
            const next = try releaseReuse(store, site.pack, packing.reuse, dyn_stmt.next);
            store.getCFStmtPtr(site.pack).dyn = .{
                .target = dyn_stmt.target,
                .data = try store.addStmtRow(LIR.DynOp, op),
                .next = next,
            };
        } else {
            const pack = store.getCFStmtImmediate(site.pack).assign_packed_erased_fn;
            var data = store.stmtRow(pack.data);
            data.frame = true;
            data.reuse = .none;
            data.reuse_unique = false;
            const next = try releaseReuse(store, site.pack, packing.reuse, pack.next);
            store.getCFStmtPtr(site.pack).assign_packed_erased_fn = .{
                .target = pack.target,
                .data = try store.addStmtRow(LIR.PackedErasedFnData, data),
                .next = next,
            };
        }
    }
    if (frame_locals.items.len != 0) {
        // Fresh locals have the largest ids, so appending them keeps the
        // inventory sorted.
        const spec = store.getProcSpecImmediate(proc_id);
        const existing = store.getLocalSpan(spec.frame_locals);
        var all = try std.ArrayList(LocalId).initCapacity(allocator, existing.len + frame_locals.items.len);
        defer all.deinit(allocator);
        for (0..existing.len) |index| all.appendAssumeCapacity(GuardedList.at(existing, index));
        all.appendSliceAssumeCapacity(frame_locals.items);
        const all_span = try store.addLocalSpan(all.items);
        store.getProcSpecPtr(proc_id).frame_locals = all_span;
    }

    if (dropped.count() == 0) return;
    const Resolve = struct {
        dropped: *const collections.DenseMapUnmanaged(CFStmtId, CFStmtId),
        fn call(self: @This(), id: CFStmtId) CFStmtId {
            var target = id;
            while (self.dropped.get(target)) |next| target = next;
            return target;
        }
    };
    const resolve: Resolve = .{ .dropped = &dropped };
    var walk = try BodyClone.ReachableStmts.initWithAllocator(store, body, allocator);
    defer walk.deinit();
    var reachable = std.ArrayList(CFStmtId).empty;
    defer reachable.deinit(allocator);
    while (try walk.next()) |stmt_id| try reachable.append(allocator, stmt_id);
    for (reachable.items) |stmt_id| BodyClone.redirectSuccessors(store, stmt_id, resolve, Resolve.call);
    const spec = store.getProcSpecPtr(proc_id);
    spec.body = .some(resolve.call(body));
    const joins = store.getJoinPointSpanMut(spec.join_points);
    for (0..joins.len) |index| {
        const join = GuardedList.atPtr(joins, index);
        join.body = resolve.call(join.body);
    }
}

/// Rewrite into frame packings every packing of `store` whose callable
/// never outlives its procedure's frame, reading each callee's
/// `nonescaping_fn_params` (`solveParams` first). Returns how many
/// packings it rewrote.
pub fn markFrames(allocator: Allocator, store: *LirStore, layouts: *const layout_mod.Store) Allocator.Error!usize {
    var rewritten: usize = 0;
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    var scratch: Scratch = .{};
    defer scratch.deinit(allocator);
    var graph: FlowGraph = .{};
    defer graph.deinit(allocator);
    var solver: SiteSolver = .{ .allocator = allocator, .store = store, .layouts = layouts, .facts = undefined, .graph = &graph };
    defer solver.deinit();
    for (0..store.procSpecCount()) |index| {
        const proc_id: ProcId = @fromBackingInt(@intCast(index));
        const spec = store.getProcSpecImmediate(proc_id);
        const body = spec.arcBody() orelse continue;
        const has_packs = spec.shapes.erased_pack;
        if (!has_packs and !std.debug.runtime_safety) continue;

        _ = arena_state.reset(.retain_capacity);
        const arena = arena_state.allocator();
        const facts = try collectProcFacts(allocator, arena, &scratch, store, layouts, body);
        if (!has_packs) {
            if (facts.packs.len != 0) frameInvariant("a procedure whose shapes exclude packings contains one");
            continue;
        }
        if (facts.packs.len == 0) continue;

        solver.facts = &facts;
        // The flow graph and liveness serve only packings whose uses
        // qualify; a procedure with none needs neither.
        var graph_built = false;
        var sites = std.ArrayList(FrameSite).empty;
        for (facts.packs) |pack| {
            if (!(try solver.usesQualify(pack))) continue;
            if (!graph_built) {
                try graph.build(allocator, store, body, scratch.stmts.items);
                graph_built = true;
            }
            if (try solveSite(&solver, arena, pack)) |site| try sites.append(arena, site);
        }
        if (sites.items.len == 0) continue;
        try rewriteSites(allocator, store, proc_id, sites.items);
        rewritten += sites.items.len;
    }
    if (std.debug.runtime_safety) try verifyFrames(allocator, store, layouts);
    return rewritten;
}

/// Debug check of every frame packing in `store`: its callable and the
/// aliases of it are never retained or released, are called only by calls
/// that are made now and transfer nothing, and are passed only to callee
/// parameters whose published mask keeps them, by calls that do not replace
/// the frame.
fn verifyFrames(allocator: Allocator, store: *const LirStore, layouts: *const layout_mod.Store) Allocator.Error!void {
    var group = std.ArrayList(LocalId).empty;
    defer group.deinit(allocator);
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    var scratch: Scratch = .{};
    defer scratch.deinit(allocator);
    for (0..store.procSpecCount()) |index| {
        const spec = store.getProcSpecImmediate(@fromBackingInt(@intCast(index)));
        const body = spec.arcBody() orelse continue;
        if (!spec.shapes.erased_pack) continue;
        _ = arena_state.reset(.retain_capacity);
        const facts = try collectProcFacts(allocator, arena_state.allocator(), &scratch, store, layouts, body);
        for (facts.packs) |pack| {
            const packing = Packing.of(store, pack) orelse frameInvariant("a collected packing that is not a packing");
            if (!packing.frame) continue;
            if (packing.reuse != null) frameInvariant("a frame packing repacks an allocation");
            if (packing.dynamic) {
                const op = store.stmtRow(store.getCFStmtImmediate(pack).dyn.data).closure;
                const storage = store.localDyn(op.frame.?) orelse frameInvariant("a frame closure's storage is not a dynamic local");
                if (storage.storage != .frame_callable) frameInvariant("a frame closure's storage is not frame-callable storage");
                const capture = store.localDyn(op.capture) orelse frameInvariant("a frame closure's capture record is not dynamic");
                if (capture.storage != .frame or capture.desc != storage.desc) frameInvariant("a frame closure's storage is not sized by its capture record");
            }
            group.clearRetainingCapacity();
            try group.append(allocator, packing.target);
            var next: usize = 0;
            while (next < group.items.len) : (next += 1) {
                const member = group.items[next];
                for (facts.uses.of(member)) |stmt_id| {
                    const stmt = store.getCFStmt(stmt_id);
                    switch (classifyUse(store, stmt_id, member)) {
                        .rc => frameInvariant("a frame callable is retained or released"),
                        .alias => |alias| {
                            if (stmt != .assign_ref) frameInvariant("a frame callable flows into a variable");
                            if (std.mem.findScalar(LocalId, group.items, alias) == null) try group.append(allocator, alias);
                        },
                        .call => |call| {
                            if (call.deferred) frameInvariant("a frame callable is called after its frame returns");
                            const transfers = if (stmt == .assign_call_erased)
                                store.stmtRow(stmt.assign_call_erased.data).reuse_closure
                            else
                                store.stmtRow(stmt.dyn.data).call_erased.reuse_closure;
                            if (transfers) frameInvariant("a call transfers a frame callable");
                        },
                        .direct_arg => |arg| {
                            if (arg.replaces_frame) frameInvariant("a frame callable is passed to a call that replaces its frame");
                            if ((store.getProcSpecImmediate(arg.callee).nonescaping_fn_params & arg.positions) != arg.positions) {
                                frameInvariant("a frame callable is passed to a parameter that lets it escape");
                            }
                        },
                        .captured => frameInvariant("a frame callable is captured by another callable"),
                        .escape => frameInvariant("a frame callable escapes its frame"),
                    }
                }
            }
        }
    }
}

/// Release `reuse`, the allocation a packing consumed and no longer
/// repacks, after `pack`; returns the packing's new successor.
fn releaseReuse(store: *LirStore, pack: CFStmtId, reuse: ?LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
    const local = reuse orelse return next;
    return store.addCFStmt(.{ .decref = .{
        .value = local,
        .rc = try rcHelperRef(store, .decref, store.localLayout(local)),
        .next = next,
    } }, releaseOrigin(store, pack, local));
}

fn siteOf(sites: []const FrameSite, local: LocalId) ?*const FrameSite {
    for (sites) |*site| {
        if (std.sort.binarySearch(LocalId, site.group, local, struct {
            fn order(needle: LocalId, item: LocalId) std.math.Order {
                return std.math.order(@backingInt(needle), @backingInt(item));
            }
        }.order) != null) return site;
    }
    return null;
}

// Tests

const testing = std.testing;

const Fixture = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    erased: layout_mod.Idx,
    next_identity: u8 = 1,

    fn init() Allocator.Error!Fixture {
        var layouts = try layout_mod.Store.init(testing.allocator, .u64);
        errdefer layouts.deinit();
        const erased = try layouts.insertErasedCallable();
        return .{ .store = LirStore.init(testing.allocator), .layouts = layouts, .erased = erased };
    }

    fn deinit(self: *Fixture) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *Fixture, layout_idx: layout_mod.Idx) Allocator.Error!LocalId {
        return self.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn fnLocal(self: *Fixture) Allocator.Error!LocalId {
        return self.local(self.erased);
    }

    fn stmt(self: *Fixture, s: LIR.CFStmt) Allocator.Error!CFStmtId {
        return self.store.addCFStmt(s, .test_fixture);
    }

    fn proc(self: *Fixture, params: []const LocalId, body: ?CFStmtId) Allocator.Error!ProcId {
        const identity = LIR.ProcIdentity.forTest(self.next_identity);
        self.next_identity += 1;
        return self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .identity = identity,
            .args = try self.store.addLocalSpan(params),
            .body = if (body) |b| .some(b) else .none,
            .ret_layout = .u64,
        }, .none);
    }

    fn external(self: *Fixture, params: []const LocalId, published_mask: u64) Allocator.Error!ProcId {
        const id = try self.proc(params, null);
        const spec = self.store.getProcSpecPtr(id);
        spec.external = true;
        spec.linked = true;
        spec.nonescaping_fn_params = published_mask;
        return id;
    }

    /// `result = f(arg)` through the erased ABI, then `next`.
    fn erasedCall(self: *Fixture, closure: LocalId, args: []const LocalId, result: LocalId, deferred: bool, next: CFStmtId) Allocator.Error!CFStmtId {
        var arg_layouts: [4]layout_mod.Idx = undefined;
        for (args, 0..) |arg, index| arg_layouts[index] = self.store.localLayout(arg);
        const plan = try self.store.internErasedCallArgsPlan(&self.layouts, arg_layouts[0..args.len]);
        return self.stmt(.{ .assign_call_erased = .{
            .target = result,
            .data = try self.store.addStmtRow(LIR.ErasedCallData, .{
                .closure = closure,
                .args = try self.store.addLocalSpan(args),
                .arg_plan = plan,
                .deferred = deferred,
            }),
            .next = next,
        } });
    }

    fn call(self: *Fixture, callee: ProcId, args: []const LocalId, result: LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
        return self.stmt(.{ .assign_call = .{
            .target = result,
            .proc = callee,
            .args = try self.store.addLocalSpan(args),
            .next = next,
        } });
    }

    fn release(self: *Fixture, value: LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
        return self.stmt(.{ .decref = .{
            .value = value,
            .rc = try self.store.addRcHelperRef(.{ .concrete = .{ .op = .decref, .layout_idx = self.erased } }),
            .next = next,
        } });
    }

    fn ret(self: *Fixture, value: LocalId) Allocator.Error!CFStmtId {
        return self.stmt(.{ .ret = .{ .value = value } });
    }

    fn solve(self: *Fixture, published: []const PublishedPair) Allocator.Error!void {
        try solveParams(testing.allocator, &self.store, &self.layouts, published);
    }

    fn mask(self: *const Fixture, id: ProcId) u64 {
        return self.store.getProcSpecImmediate(id).nonescaping_fn_params;
    }
};

test "a parameter the procedure only calls and releases does not escape" {
    var f = try Fixture.init();
    defer f.deinit();
    const param = try f.fnLocal();
    const result = try f.local(.u64);
    const body = try f.erasedCall(param, &.{}, result, false, try f.release(param, try f.ret(result)));
    const proc = try f.proc(&.{param}, body);
    try f.solve(&.{});
    try testing.expectEqual(@as(u64, 1), f.mask(proc));
}

test "returning, storing, or passing a parameter to an erased call lets it escape" {
    const Case = enum { returned, stored, erased_arg, captured, conditional_release };
    for (std.enums.values(Case)) |case| {
        var f = try Fixture.init();
        defer f.deinit();
        const param = try f.fnLocal();
        const other = try f.fnLocal();
        const result = try f.local(.u64);
        const body = switch (case) {
            .returned => try f.ret(param),
            .stored => blk: {
                const record = try f.local(try f.layouts.putStructFields(&.{.{ .index = 0, .layout = f.erased }}));
                break :blk try f.stmt(.{ .assign_struct = .{
                    .target = record,
                    .fields = try f.store.addLocalSpan(&.{param}),
                    .next = try f.ret(record),
                } });
            },
            .erased_arg => try f.erasedCall(other, &.{param}, result, false, try f.ret(result)),
            .captured => blk: {
                const record_layout = try f.layouts.putStructFields(&.{.{ .index = 0, .layout = f.erased }});
                const record = try f.local(record_layout);
                const packed_fn = try f.fnLocal();
                const worker = try f.proc(&.{}, try f.ret(result));
                const pack = try f.stmt(.{ .assign_packed_erased_fn = .{
                    .target = packed_fn,
                    .data = try f.store.addStmtRow(LIR.PackedErasedFnData, .{
                        .proc = worker,
                        .capture = .some(record),
                        .capture_layout = .some(record_layout),
                        .on_drop = .none,
                    }),
                    .next = try f.ret(packed_fn),
                } });
                break :blk try f.stmt(.{ .assign_struct = .{
                    .target = record,
                    .fields = try f.store.addLocalSpan(&.{param}),
                    .next = pack,
                } });
            },
            .conditional_release => try f.stmt(.{ .decref_if_initialized = .{
                .cond = result,
                .value = param,
                .data = try f.store.addStmtRow(LIR.DecrefIfInitializedData, .{
                    .rc = .{ .concrete = .{ .op = .decref, .layout_idx = f.erased } },
                }),
                .next = try f.ret(result),
            } }),
        };
        const proc = try f.proc(&.{ param, other }, body);
        try f.solve(&.{});
        try testing.expectEqual(@as(u64, 0), f.mask(proc) & 1);
    }
}

test "a parameter called by a deferred erased call escapes" {
    for ([_]bool{ false, true }) |deferred| {
        var f = try Fixture.init();
        defer f.deinit();
        const param = try f.fnLocal();
        const result = try f.local(.u64);
        const body = try f.erasedCall(param, &.{}, result, deferred, try f.ret(result));
        const proc = try f.proc(&.{param}, body);
        try f.solve(&.{});
        try testing.expectEqual(@as(u64, if (deferred) 0 else 1), f.mask(proc));
    }
}

test "a parameter captured by a callable that keeps it does not escape, wherever its body calls the field" {
    const Case = enum { called, deferred_call, body_returns_field, callable_returned, record_returned, external_body, roc_body_deferred };
    for (std.enums.values(Case)) |case| {
        var f = try Fixture.init();
        defer f.deinit();
        const record_layout = try f.layouts.putStructFields(&.{.{ .index = 0, .layout = f.erased }});

        // The packed procedure: it loads its capture and reads field 0.
        const capture_ptr = try f.local(.opaque_ptr);
        const self_ref = try f.fnLocal();
        const loaded = try f.local(record_layout);
        const field = try f.fnLocal();
        const field_result = try f.local(.u64);
        const after_read = switch (case) {
            .body_returns_field => try f.ret(field),
            .deferred_call, .roc_body_deferred => try f.erasedCall(field, &.{}, field_result, true, try f.ret(field_result)),
            .called, .callable_returned, .record_returned, .external_body => try f.erasedCall(field, &.{}, field_result, false, try f.ret(field_result)),
        };
        const read = try f.stmt(.{ .assign_ref = .{ .target = field, .op = LIR.RefOp.field(loaded, 0), .next = after_read } });
        const load = try f.stmt(.{ .assign_low_level = .{
            .target = loaded,
            .op = .erased_capture_load_borrowed,
            .rc_effect = try f.store.addRcEffect(LIR.LowLevel.erased_capture_load_borrowed.rcEffect()),
            .args = try f.store.addLocalSpan(&.{ capture_ptr, self_ref }),
            .next = read,
        } });
        const lambda = try f.proc(&.{ capture_ptr, self_ref }, if (case == .external_body) null else load);
        {
            const spec = f.store.getProcSpecPtr(lambda);
            spec.erased_capture_arg = .some(capture_ptr);
            spec.erased_reuse_arg = .some(self_ref);
            spec.abi = if (case == .roc_body_deferred) .roc else .erased_callable;
        }

        // A callee that only calls its parameter.
        const keeps_param = try f.fnLocal();
        const keeps_result = try f.local(.u64);
        const keeps = try f.proc(&.{keeps_param}, try f.erasedCall(keeps_param, &.{}, keeps_result, false, try f.release(keeps_param, try f.ret(keeps_result))));

        // The packer: it stores its parameter into a record, packs the
        // procedure above with it, and passes the callable to `keeps`.
        const param = try f.fnLocal();
        const record = try f.local(record_layout);
        const callable = try f.fnLocal();
        const result = try f.local(.u64);
        const tail = switch (case) {
            .callable_returned => try f.ret(callable),
            .record_returned => try f.ret(record),
            .called, .deferred_call, .body_returns_field, .external_body, .roc_body_deferred => try f.ret(result),
        };
        const pass = try f.call(keeps, &.{callable}, result, tail);
        const pack = try f.stmt(.{ .assign_packed_erased_fn = .{
            .target = callable,
            .data = try f.store.addStmtRow(LIR.PackedErasedFnData, .{
                .proc = lambda,
                .capture = .some(record),
                .capture_layout = .some(record_layout),
                .on_drop = .{ .rc_helper = .{ .op = .decref, .layout_idx = record_layout } },
            }),
            .next = pass,
        } });
        const build = try f.stmt(.{ .assign_struct = .{ .target = record, .fields = try f.store.addLocalSpan(&.{param}), .next = pack } });
        const packer = try f.proc(&.{param}, build);
        try f.solve(&.{});
        const keeps_it = switch (case) {
            .called, .deferred_call => true,
            .body_returns_field, .callable_returned, .record_returned, .external_body, .roc_body_deferred => false,
        };
        try testing.expectEqual(@as(u64, @intFromBool(keeps_it)), f.mask(packer));
    }
}

test "forwarding follows the callee's mask, including a published one" {
    const Callee = enum { keeps, escapes, external_keeps, external_escapes, hosted };
    for (std.enums.values(Callee)) |kind| {
        var f = try Fixture.init();
        defer f.deinit();
        const callee_param = try f.fnLocal();
        const callee_result = try f.local(.u64);
        const callee = switch (kind) {
            .keeps => try f.proc(&.{callee_param}, try f.erasedCall(callee_param, &.{}, callee_result, false, try f.ret(callee_result))),
            .escapes => try f.proc(&.{callee_param}, try f.ret(callee_param)),
            .external_keeps => try f.external(&.{callee_param}, 1),
            .external_escapes => try f.external(&.{callee_param}, 0),
            .hosted => try f.proc(&.{callee_param}, null),
        };
        const param = try f.fnLocal();
        const alias = try f.fnLocal();
        const result = try f.local(.u64);
        const forward = try f.call(callee, &.{alias}, result, try f.ret(result));
        const body = try f.stmt(.{ .assign_ref = .{ .target = alias, .op = .local(param), .next = forward } });
        const proc = try f.proc(&.{param}, body);
        try f.solve(&.{});
        const keeps = kind == .keeps or kind == .external_keeps;
        try testing.expectEqual(@as(u64, @intFromBool(keeps)), f.mask(proc));
    }
}

test "recursion keeps a parameter unless some path lets it escape" {
    for ([_]bool{ false, true }) |escape_on_one_path| {
        var f = try Fixture.init();
        defer f.deinit();
        // a(p) = b(p); b(q) = a(q) or, on one variant, return q.
        const a_param = try f.fnLocal();
        const b_param = try f.fnLocal();
        const a_result = try f.local(.u64);
        const b_result = try f.local(.u64);
        const a = try f.proc(&.{a_param}, null);
        const b_body = if (escape_on_one_path)
            try f.ret(b_param)
        else
            try f.call(a, &.{b_param}, b_result, try f.ret(b_result));
        const b = try f.proc(&.{b_param}, b_body);
        f.store.setProcSpecBody(a, try f.call(b, &.{a_param}, a_result, try f.ret(a_result)));
        try f.solve(&.{});
        const expected: u64 = if (escape_on_one_path) 0 else 1;
        try testing.expectEqual(expected, f.mask(a));
        try testing.expectEqual(expected, f.mask(b));
    }
}

test "a published variant and its worker share one mask" {
    var f = try Fixture.init();
    defer f.deinit();
    const worker_param = try f.fnLocal();
    const variant_param = try f.fnLocal();
    const result = try f.local(.u64);
    const worker = try f.proc(&.{worker_param}, try f.erasedCall(worker_param, &.{}, result, false, try f.ret(result)));
    const variant = try f.proc(&.{variant_param}, try f.ret(variant_param));
    try f.solve(&.{.{ .worker = worker, .variant = variant }});
    try testing.expectEqual(@as(u64, 0), f.mask(worker));
    try testing.expectEqual(@as(u64, 0), f.mask(variant));
}

test "only function-valued parameters are candidates" {
    var f = try Fixture.init();
    defer f.deinit();
    const scalar = try f.local(.u64);
    const param = try f.fnLocal();
    const result = try f.local(.u64);
    const body = try f.erasedCall(param, &.{}, result, false, try f.ret(scalar));
    const proc = try f.proc(&.{ scalar, param }, body);
    try f.solve(&.{});
    try testing.expectEqual(@as(u64, 0b10), f.mask(proc));
}

/// A procedure that packs a callable capturing a string and hands it to
/// `use`, for the frame-packing tests.
const PackFixture = struct {
    f: Fixture,
    capture_layout: layout_mod.Idx,
    capture: LocalId,
    callable: LocalId,
    worker: ProcId,
    keeps: ProcId,
    escapes: ProcId,

    fn init() Allocator.Error!PackFixture {
        return initCapturing(.str);
    }

    fn initCapturing(captured: layout_mod.Idx) Allocator.Error!PackFixture {
        var f = try Fixture.init();
        errdefer f.deinit();
        const capture_layout = try f.layouts.putStructFields(&.{.{ .index = 0, .layout = captured }});
        const worker_result = try f.local(.u64);
        const worker = try f.proc(&.{}, try f.ret(worker_result));
        const keeps_param = try f.fnLocal();
        const keeps_result = try f.local(.u64);
        const keeps = try f.proc(&.{keeps_param}, try f.erasedCall(keeps_param, &.{}, keeps_result, false, try f.release(keeps_param, try f.ret(keeps_result))));
        const escapes_param = try f.fnLocal();
        const escapes = try f.proc(&.{escapes_param}, try f.ret(escapes_param));
        return .{
            .capture_layout = capture_layout,
            .capture = try f.local(capture_layout),
            .callable = try f.fnLocal(),
            .worker = worker,
            .keeps = keeps,
            .escapes = escapes,
            .f = f,
        };
    }

    fn deinit(self: *PackFixture) void {
        self.f.deinit();
    }

    fn pack(self: *PackFixture, reuse: ?LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
        return self.f.stmt(.{ .assign_packed_erased_fn = .{
            .target = self.callable,
            .data = try self.f.store.addStmtRow(LIR.PackedErasedFnData, .{
                .proc = self.worker,
                .capture = .some(self.capture),
                .capture_layout = .some(self.capture_layout),
                .on_drop = if (self.f.layouts.layoutContainsRefcounted(self.f.layouts.getLayout(self.capture_layout)))
                    .{ .rc_helper = .{ .op = .decref, .layout_idx = self.capture_layout } }
                else
                    .none,
                .reuse = .of(reuse),
            }),
            .next = next,
        } });
    }

    /// The procedure around `body`, with its packing shape recorded.
    fn packer(self: *PackFixture, params: []const LocalId, body: CFStmtId) Allocator.Error!ProcId {
        const id = try self.f.proc(params, body);
        self.f.store.getProcSpecPtr(id).shapes.erased_pack = true;
        return id;
    }

    fn run(self: *PackFixture) Allocator.Error!usize {
        try self.f.solve(&.{});
        return markFrames(testing.allocator, &self.f.store, &self.f.layouts);
    }

    fn packData(self: *const PackFixture, pack_stmt: CFStmtId) LIR.PackedErasedFnData {
        return self.f.store.stmtRow(self.f.store.getCFStmtImmediate(pack_stmt).assign_packed_erased_fn.data);
    }

    /// The statements of `proc`'s body in flow order along `next` edges,
    /// as tags, ending at its terminal.
    fn chain(self: *const PackFixture, proc_id: ProcId, out: []std.meta.Tag(LIR.CFStmt)) []std.meta.Tag(LIR.CFStmt) {
        var current = self.f.store.getProcSpecImmediate(proc_id).body.get().?;
        var len: usize = 0;
        while (true) {
            const stmt = self.f.store.getCFStmt(current);
            out[len] = std.meta.activeTag(stmt);
            len += 1;
            current = switch (stmt) {
                inline .assign_packed_erased_fn, .assign_call, .assign_call_erased, .incref, .decref, .free, .assign_ref => |s| s.next,
                .init_uninitialized,
                .assign_literal,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .expect_err,
                .dyn,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .decref_if_initialized,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .join,
                .jump,
                .ret,
                .crash,
                => return out[0..len],
            };
        }
    }
};

test "a callable passed only to a keeping parameter is built in the frame and its capture released after its last use" {
    var p = try PackFixture.init();
    defer p.deinit();
    const result = try p.f.local(.u64);
    const ret = try p.f.ret(result);
    const call = try p.f.call(p.keeps, &.{p.callable}, result, ret);
    const retain = try p.f.stmt(.{ .incref = .{
        .value = p.callable,
        .rc = try p.f.store.addRcHelperRef(.{ .concrete = .{ .op = .incref, .layout_idx = p.f.erased } }),
        .next = call,
    } });
    const second = try p.f.call(p.keeps, &.{p.callable}, result, retain);
    const pack = try p.pack(null, second);
    const proc = try p.packer(&.{}, pack);
    try testing.expectEqual(@as(usize, 1), try p.run());
    try testing.expect(p.packData(pack).frame);
    var tags: [8]std.meta.Tag(LIR.CFStmt) = undefined;
    try testing.expectEqualSlices(std.meta.Tag(LIR.CFStmt), &.{ .assign_packed_erased_fn, .assign_call, .assign_call, .decref, .ret }, p.chain(proc, &tags));
}

test "a callable that escapes, or is passed to a frame-replacing call, stays on the heap" {
    const Case = enum { escaping_param, returned, replaces_frame, deferred_call };
    for (std.enums.values(Case)) |case| {
        var p = try PackFixture.init();
        defer p.deinit();
        const result = try p.f.local(.u64);
        const after = switch (case) {
            .escaping_param => try p.f.call(p.escapes, &.{p.callable}, result, try p.f.ret(result)),
            .returned => try p.f.ret(p.callable),
            .replaces_frame => try p.f.stmt(.{ .assign_call = .{
                .target = result,
                .proc = p.keeps,
                .args = try p.f.store.addLocalSpan(&.{p.callable}),
                .extra = try p.f.store.addOptionalStmtRow(LIR.CallExtra, .{ .replaces_frame = true }),
                .next = try p.f.ret(result),
            } }),
            .deferred_call => try p.f.erasedCall(p.callable, &.{}, result, true, try p.f.ret(result)),
        };
        const pack = try p.pack(null, after);
        _ = try p.packer(&.{}, pack);
        try testing.expectEqual(@as(usize, 0), try p.run());
        try testing.expect(!p.packData(pack).frame);
    }
}

test "a frame packing releases the allocation it would have repacked, and calls of it transfer nothing" {
    var p = try PackFixture.initCapturing(.u64);
    defer p.deinit();
    const reuse = try p.f.fnLocal();
    const result = try p.f.local(.u64);
    const ret = try p.f.ret(result);
    const plan = try p.f.store.internErasedCallArgsPlan(&p.f.layouts, &.{});
    const call = try p.f.stmt(.{ .assign_call_erased = .{
        .target = result,
        .data = try p.f.store.addStmtRow(LIR.ErasedCallData, .{
            .closure = p.callable,
            .args = .empty(),
            .arg_plan = plan,
            .reuse_closure = true,
        }),
        .next = ret,
    } });
    const pack = try p.pack(reuse, call);
    const proc = try p.packer(&.{reuse}, pack);
    try testing.expectEqual(@as(usize, 1), try p.run());
    const data = p.packData(pack);
    try testing.expect(data.frame and data.reuse == .none);
    // The reuse allocation is released; a capture without reference-counted
    // data needs no release.
    var tags: [8]std.meta.Tag(LIR.CFStmt) = undefined;
    try testing.expectEqualSlices(std.meta.Tag(LIR.CFStmt), &.{ .assign_packed_erased_fn, .decref, .assign_call_erased, .ret }, p.chain(proc, &tags));
    const call_stmt = p.f.store.getCFStmt(p.f.store.getCFStmt(p.f.store.getCFStmt(pack).assign_packed_erased_fn.next).decref.next);
    try testing.expect(!p.f.store.stmtRow(call_stmt.assign_call_erased.data).reuse_closure);
}

test "a callable whose captures a callee could update in place stays on the heap" {
    const Case = enum { local_transfer, transferring_param };
    for (std.enums.values(Case)) |case| {
        for ([_]layout_mod.Idx{ .str, .u64 }) |captured| {
            var p = try PackFixture.initCapturing(captured);
            defer p.deinit();
            const result = try p.f.local(.u64);
            const plan = try p.f.store.internErasedCallArgsPlan(&p.f.layouts, &.{});
            const after = switch (case) {
                .local_transfer => try p.f.stmt(.{ .assign_call_erased = .{
                    .target = result,
                    .data = try p.f.store.addStmtRow(LIR.ErasedCallData, .{ .closure = p.callable, .args = .empty(), .arg_plan = plan, .reuse_closure = true }),
                    .next = try p.f.ret(result),
                } }),
                .transferring_param => blk: {
                    // once(f) = f() with f dying at the call.
                    const param = try p.f.fnLocal();
                    const once_result = try p.f.local(.u64);
                    const once_body = try p.f.stmt(.{ .assign_call_erased = .{
                        .target = once_result,
                        .data = try p.f.store.addStmtRow(LIR.ErasedCallData, .{ .closure = param, .args = .empty(), .arg_plan = plan, .reuse_closure = true }),
                        .next = try p.f.ret(once_result),
                    } });
                    const once = try p.f.proc(&.{param}, once_body);
                    break :blk try p.f.call(once, &.{p.callable}, result, try p.f.ret(result));
                },
            };
            const pack = try p.pack(null, after);
            _ = try p.packer(&.{}, pack);
            const rewritten = try p.run();
            try testing.expectEqual(@as(usize, if (captured == .str) 0 else 1), rewritten);
            if (case == .transferring_param) {
                const once = p.f.store.getCFStmt(after).assign_call.proc;
                try testing.expectEqual(@as(u64, 1), p.f.mask(once));
                try testing.expectEqual(@as(u64, 1), p.f.store.getProcSpecImmediate(once).capture_reuse_fn_params);
            }
        }
    }
}

test "a capture record rewritten while its callable is live keeps the callable on the heap" {
    var p = try PackFixture.init();
    defer p.deinit();
    const other = try p.f.local(p.capture_layout);
    const result = try p.f.local(.u64);
    const call = try p.f.call(p.keeps, &.{p.callable}, result, try p.f.ret(result));
    const rewrite = try p.f.stmt(.{ .set_local = .{ .target = p.capture, .value = other, .mode = .replace_existing, .next = call } });
    const pack = try p.pack(null, rewrite);
    _ = try p.packer(&.{other}, pack);
    try testing.expectEqual(@as(usize, 0), try p.run());
}

test "a branch that never uses the frame callable releases its capture on entry" {
    var p = try PackFixture.init();
    defer p.deinit();
    const cond = try p.f.local(.u8);
    const result = try p.f.local(.u64);
    const used = try p.f.call(p.keeps, &.{p.callable}, result, try p.f.ret(result));
    const unused = try p.f.ret(result);
    const branches = try p.f.store.addCFSwitchBranches(&.{.{ .value = 0, .body = used }});
    const branch = try p.f.stmt(.{ .switch_stmt = .{ .cond = cond, .branches = branches, .default_branch = unused } });
    const pack = try p.pack(null, branch);
    _ = try p.packer(&.{cond}, pack);
    try testing.expectEqual(@as(usize, 1), try p.run());
    const rewritten = p.f.store.getCFStmt(branch).switch_stmt;
    const default = p.f.store.getCFStmt(rewritten.default_branch);
    try testing.expect(default == .decref and default.decref.value == p.capture and default.decref.next == unused);
    const arm = GuardedList.at(p.f.store.getCFSwitchBranches(rewritten.branches), 0).body;
    try testing.expectEqual(used, arm);
    try testing.expect(p.f.store.getCFStmt(p.f.store.getCFStmt(used).assign_call.next) == .decref);
}

test "a dynamic closure passed only to a keeping parameter gets frame storage sized by its capture record" {
    for ([_]LIR.DynStorage{ .frame, .param }) |capture_storage| {
        var p = try PackFixture.initCapturing(.u64);
        defer p.deinit();
        const dynamic = try p.f.layouts.dynamicLayout();
        const desc = try p.f.local(.opaque_ptr);
        const record = try p.f.store.addLocal(.{ .layout_idx = dynamic, .dyn = .{ .desc = desc, .storage = capture_storage } });
        const result = try p.f.local(.u64);
        const call = try p.f.call(p.keeps, &.{p.callable}, result, try p.f.ret(result));
        const closure = try p.f.store.addDynStmt(p.callable, .{ .closure = .{ .proc = p.worker, .capture = record } }, call, .test_fixture);
        const proc = try p.packer(&.{ desc, record }, closure);
        const packer_frame_locals = try p.f.store.addLocalSpan(&.{ desc, record, p.callable, result });
        p.f.store.getProcSpecPtr(proc).frame_locals = packer_frame_locals;
        const frame = capture_storage == .frame;
        try testing.expectEqual(@as(usize, @intFromBool(frame)), try p.run());
        const op = p.f.store.stmtRow(p.f.store.getCFStmt(closure).dyn.data).closure;
        if (!frame) {
            try testing.expect(op.frame == null);
            continue;
        }
        const storage = op.frame.?;
        try testing.expectEqual(LIR.DynLocal{ .desc = desc, .storage = .frame_callable }, p.f.store.localDyn(storage).?);
        const frame_locals = p.f.store.getLocalSpan(p.f.store.getProcSpecImmediate(proc).frame_locals);
        try testing.expectEqual(storage, GuardedList.at(frame_locals, frame_locals.len - 1));
        // A dynamic record may hold reference-counted data, so it is
        // released after the call.
        const after = p.f.store.getCFStmt(p.f.store.getCFStmt(call).assign_call.next);
        try testing.expect(after == .decref and after.decref.value == record);
    }
}

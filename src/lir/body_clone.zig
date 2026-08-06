//! Shared machinery for the proc-specializing rewrite passes that clone a
//! source proc body into an internal variant with rewritten return handling.
//!
//! `StrAppend` and `ReturnSlot` both clone an existing proc body statement by
//! statement, remapping every local, while intercepting the body's returns to
//! emit their own destination-aware tail. This module owns the parts that do
//! not vary between them: the generic `BodyCloner(Rewriter)`, the reachable
//! successor walk, the alias-forwarding walk, the operand read counter used to
//! prove rewrite soundness, and the frame-local deduplication helpers.

const std = @import("std");
const collections = @import("collections");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;

/// A local reached by forwarding through `assign_ref .local` aliases, paired
/// with the first statement past the alias chain.
pub const ForwardedAlias = struct {
    value: LocalId,
    next: CFStmtId,
};

/// Follow a straight-line chain of `assign_ref .local` aliases of `source`,
/// returning the final aliased local and the first statement that is not such
/// an alias. Only aliases that copy the tracked value into a same-layout local
/// are crossed.
pub fn forwardLocalAliasChain(store: *const LirStore, source: LocalId, first_stmt: CFStmtId) ForwardedAlias {
    return forwardLocalAliasChainImpl(store, undefined, source, first_stmt, null) catch unreachable;
}

/// Like `forwardLocalAliasChain`, but also appends `source` and every crossed
/// alias local to `chain`, so a caller can require each consumed local is only
/// used by the chain before fusing the producer into the store.
pub fn forwardLocalAliasChainInto(
    store: *const LirStore,
    allocator: Allocator,
    source: LocalId,
    first_stmt: CFStmtId,
    chain: *std.ArrayList(LocalId),
) Allocator.Error!ForwardedAlias {
    return forwardLocalAliasChainImpl(store, allocator, source, first_stmt, chain);
}

fn forwardLocalAliasChainImpl(
    store: *const LirStore,
    allocator: Allocator,
    source: LocalId,
    first_stmt: CFStmtId,
    chain: ?*std.ArrayList(LocalId),
) Allocator.Error!ForwardedAlias {
    if (chain) |list| try list.append(allocator, source);
    var value = source;
    var current = first_stmt;
    while (true) {
        const stmt_node = store.getCFStmt(current);
        if (stmt_node != .assign_ref) return .{ .value = value, .next = current };
        const stmt = stmt_node.assign_ref;
        if (stmt.op == .local) {
            const local = stmt.op.local;
            if (local == value and store.getLocal(stmt.target).layout_idx == store.getLocal(value).layout_idx) {
                if (chain) |list| try list.append(allocator, stmt.target);
                value = stmt.target;
                current = stmt.next;
                continue;
            }
        }
        return .{ .value = value, .next = current };
    }
}

/// Push every control-flow successor of `stmt_id` onto `work`, covering
/// straight-line `next` edges, switch branches and continuations, initialized
/// payload arms, string-match arms, and join bodies. This is the reachability
/// step shared by the proc walkers.
pub fn appendSuccessors(
    store: *LirStore,
    work: *std.ArrayList(CFStmtId),
    stmt_id: CFStmtId,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        inline .assign_ref,
        .assign_literal,
        .init_uninitialized,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        => |s| try work.append(store.allocator, s.next),

        .switch_stmt => |s| {
            if (s.continuation) |continuation| try work.append(store.allocator, continuation);
            try work.append(store.allocator, s.default_branch);
            const branches = store.getCFSwitchBranches(s.branches);
            for (0..branches.len) |index| {
                try work.append(store.allocator, GuardedList.at(branches, index).body);
            }
        },
        .switch_initialized_payload => |s| {
            try work.append(store.allocator, s.initialized_branch);
            try work.append(store.allocator, s.uninitialized_branch);
        },
        .str_match => |s| {
            try work.append(store.allocator, s.on_match);
            try work.append(store.allocator, s.on_miss);
        },
        .boxy_tag_match => |s| {
            try work.append(store.allocator, s.on_match);
            try work.append(store.allocator, s.on_miss);
        },
        .str_match_set => |s| {
            const arms = store.getStrMatchArms(s.arms);
            for (0..arms.len) |index| {
                try work.append(store.allocator, GuardedList.at(arms, index).on_match);
            }
            try work.append(store.allocator, s.on_miss);
        },
        .join => |s| {
            try work.append(store.allocator, s.body);
            try work.append(store.allocator, s.remainder);
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

/// Per-local operand read counts over the statements reachable from one proc
/// body. A rewrite that fuses a producer into its consumer orphans the
/// producer's result local; these counts let a pass require that no other
/// statement still reads that local before it commits the fusion.
pub const ReadCounts = struct {
    allocator: Allocator,
    counts: []u32,

    /// Release the backing count storage.
    pub fn deinit(self: *ReadCounts) void {
        self.allocator.free(self.counts);
    }

    /// How many reachable statements read `local` as an operand.
    pub fn get(self: *const ReadCounts, local: LocalId) u32 {
        return self.counts[@intFromEnum(local)];
    }
};

/// Count operand reads of every local reachable from `body`, walking all
/// successor edges. Definitions (statement targets and join parameters) are not
/// reads; only operand positions count.
pub fn countReachableReads(store: *LirStore, body: CFStmtId) Allocator.Error!ReadCounts {
    const counts = try store.allocator.alloc(u32, store.localCount());
    errdefer store.allocator.free(counts);
    @memset(counts, 0);

    var work = std.ArrayList(CFStmtId).empty;
    defer work.deinit(store.allocator);
    var visited = collections.DenseMap(CFStmtId, void).init(store.allocator);
    defer visited.deinit();

    try work.append(store.allocator, body);
    while (work.pop()) |stmt_id| {
        const entry = try visited.getOrPut(stmt_id);
        if (entry.found_existing) continue;

        countStmtReads(store, counts, store.getCFStmt(stmt_id));
        try appendSuccessors(store, &work, stmt_id);
    }

    return .{ .allocator = store.allocator, .counts = counts };
}

fn countStmtReads(store: *LirStore, counts: []u32, stmt: LIR.CFStmt) void {
    switch (stmt) {
        .assign_ref => |s| switch (s.op) {
            .local => |source| noteRead(counts, source),
            .discriminant => |ref| noteRead(counts, ref.source),
            .field => |ref| noteRead(counts, ref.source),
            .tag_payload => |ref| noteRead(counts, ref.source),
            .tag_payload_struct => |ref| noteRead(counts, ref.source),
            .list_reinterpret => |ref| noteRead(counts, ref.backing_ref),
            .nominal => |ref| noteRead(counts, ref.backing_ref),
        },
        .assign_call => |s| {
            if (s.result_desc) |desc| noteDescRead(counts, desc);
            const args = store.getLocalSpan(s.args);
            for (0..args.len) |index| noteRead(counts, GuardedList.at(args, index));
        },
        .assign_call_erased => |s| {
            noteRead(counts, s.closure);
            noteSpanReads(store, counts, s.args);
            noteSpanReads(store, counts, s.arg_descs);
            if (s.result_desc) |desc| noteDescRead(counts, desc);
            if (s.reuse_source) |reuse_source| noteRead(counts, reuse_source);
        },
        .assign_packed_erased_fn => |s| {
            if (s.capture) |capture| noteRead(counts, capture);
            if (s.result_desc) |desc| noteDescRead(counts, desc);
            if (s.reuse) |reuse| noteRead(counts, reuse);
        },
        .assign_boxy_desc_ref => |s| {
            noteDescRead(counts, s.desc);
            if (s.tag_residual_for) |desc| noteDescRead(counts, desc);
            noteSpanReads(store, counts, s.captures);
        },
        .assign_boxy_dict_ref => |s| noteDictRead(counts, s.dict),
        .assign_boxy_box => |s| {
            noteRead(counts, s.payload);
            if (s.source_desc) |desc| noteDescRead(counts, desc);
            if (s.payload_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_reuse_box => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.desc);
        },
        .assign_boxy_unbox => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
            if (s.target_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_adapt => |s| {
            noteRead(counts, s.source);
            if (s.source_desc) |desc| noteDescRead(counts, desc);
            if (s.target_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_inspect => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
        },
        .assign_boxy_eq => |s| {
            noteRead(counts, s.lhs);
            noteRead(counts, s.rhs);
            noteDescRead(counts, s.source_desc);
        },
        .assign_boxy_tag => |s| {
            noteDescRead(counts, s.target_desc);
            if (s.payload) |payload| noteRead(counts, payload);
            if (s.payload_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_boxy_tag_payload => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
        },
        .boxy_tag_match => |s| {
            noteRead(counts, s.source);
            noteDescRead(counts, s.source_desc);
        },
        .assign_call_dict => |s| {
            noteDictRead(counts, s.dict);
            noteSpanReads(store, counts, s.args);
            noteSpanReads(store, counts, s.arg_descs);
            noteSpanReads(store, counts, s.hidden_args);
            if (s.result_desc) |desc| noteDescRead(counts, desc);
        },
        .assign_low_level => |s| {
            const args = store.getLocalSpan(s.args);
            for (0..args.len) |index| noteRead(counts, GuardedList.at(args, index));
        },
        .assign_list => |s| {
            const elems = store.getLocalSpan(s.elems);
            for (0..elems.len) |index| noteRead(counts, GuardedList.at(elems, index));
        },
        .assign_struct => |s| {
            const fields = store.getLocalSpan(s.fields);
            for (0..fields.len) |index| noteRead(counts, GuardedList.at(fields, index));
        },
        .assign_tag => |s| if (s.payload) |payload| noteRead(counts, payload),
        .store_struct => |s| {
            noteRead(counts, s.dest);
            const fields = store.getLocalSpan(s.fields);
            for (0..fields.len) |index| noteRead(counts, GuardedList.at(fields, index));
        },
        .store_tag => |s| {
            noteRead(counts, s.dest);
            if (s.payload) |payload| noteRead(counts, payload);
        },
        .set_local => |s| noteRead(counts, s.value),
        .debug => |s| noteRead(counts, s.message),
        .expect => |s| noteRead(counts, s.condition),
        .expect_err => |s| noteRead(counts, s.message),
        .switch_stmt => |s| noteRead(counts, s.cond),
        .switch_initialized_payload => |s| {
            noteRead(counts, s.cond);
            noteRead(counts, s.payload);
        },
        .str_match => |s| noteRead(counts, s.source),
        .str_match_set => |s| noteRead(counts, s.source),
        .ret => |s| noteRead(counts, s.value),
        .crash => |s| if (s.msg.localId()) |message| noteRead(counts, message),
        .incref => |s| noteRead(counts, s.value),
        .decref => |s| noteRead(counts, s.value),
        .decref_if_initialized => |s| {
            noteRead(counts, s.cond);
            noteRead(counts, s.value);
        },
        .free => |s| noteRead(counts, s.value),
        .init_uninitialized,
        .assign_literal,
        .comptime_branch_taken,
        .join,
        .jump,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .loop_continue,
        .loop_break,
        => {},
    }
}

fn noteRead(counts: []u32, local: LocalId) void {
    counts[@intFromEnum(local)] += 1;
}

fn noteSpanReads(store: *const LirStore, counts: []u32, span: LIR.LocalSpan) void {
    const locals = store.getLocalSpan(span);
    for (0..locals.len) |index| noteRead(counts, GuardedList.at(locals, index));
}

fn noteDescRead(counts: []u32, desc: LIR.BoxyDescRef) void {
    if (desc.localOrNull()) |local| noteRead(counts, local);
}

fn noteDictRead(counts: []u32, dict: LIR.BoxyDictRef) void {
    if (dict.localOrNull()) |local| noteRead(counts, local);
}

/// Compact a sorted slice of local ids in place, returning the length of the
/// deduplicated prefix. Callers sort with `localIdLessThan` first.
pub fn uniqueSortedLocals(items: []LocalId) usize {
    var unique_len: usize = 0;
    for (items, 0..) |local, idx| {
        if (idx > 0 and items[unique_len - 1] == local) continue;
        items[unique_len] = local;
        unique_len += 1;
    }
    return unique_len;
}

/// Order two local ids by their integer index, for `std.mem.sort`.
pub fn localIdLessThan(_: void, a: LocalId, b: LocalId) bool {
    return @intFromEnum(a) < @intFromEnum(b);
}

/// Clone a source proc body into fresh statements and locals, delegating return
/// handling to `Rewriter`.
///
/// `Rewriter` carries the pass-specific destination state and supplies the
/// hooks that diverge between passes:
///
///   * `cloneRet(self: *Rewriter, cloner: anytype, value: LocalId)`—required.
///     Produces the cloned tail for a source `ret value`.
///   * `interceptStmt(self: *Rewriter, cloner: anytype, stmt: LIR.CFStmt)`—
///     optional. Returns a cloned statement id to short-circuit the default
///     clone, letting the pass fuse a direct constructor/concat return into
///     the tail, or `null` to fall through to the ordinary clone.
///
/// Both hooks receive the cloner and use its `mapLocal`, `mapLocalSpan`,
/// `addTemp`, `directReturnOf`, and `store` surface to build their statements.
pub fn BodyCloner(comptime Rewriter: type) type {
    return struct {
        const Self = @This();

        /// The store the clone is written into.
        store: *LirStore,
        /// Pass-specific return rewriter and its destination state.
        rewriter: Rewriter,
        /// Old-local index to cloned-local, `null` until first mapped.
        local_map: []?LocalId,
        stmt_map: collections.DenseMap(CFStmtId, CFStmtId),
        /// Every local created by this clone, in creation order.
        new_locals: std.ArrayList(LocalId),

        /// Allocate the clone state sized for the store's current locals.
        pub fn init(store: *LirStore, rewriter: Rewriter) Allocator.Error!Self {
            const local_map = try store.allocator.alloc(?LocalId, store.localCount());
            @memset(local_map, null);
            return .{
                .store = store,
                .rewriter = rewriter,
                .local_map = local_map,
                .stmt_map = collections.DenseMap(CFStmtId, CFStmtId).init(store.allocator),
                .new_locals = .empty,
            };
        }

        /// Release the clone's scratch storage.
        pub fn deinit(self: *Self) void {
            self.new_locals.deinit(self.store.allocator);
            self.stmt_map.deinit();
            self.store.allocator.free(self.local_map);
        }

        /// Clone `old_id` and everything reachable from it, memoizing by
        /// original statement id so shared join targets are cloned once.
        pub fn cloneStmt(self: *Self, old_id: CFStmtId) Allocator.Error!CFStmtId {
            if (self.stmt_map.get(old_id)) |existing| return existing;

            const saved_loc = self.store.current_loc;
            defer self.store.current_loc = saved_loc;
            const saved_region = self.store.current_region;
            defer self.store.current_region = saved_region;
            const saved_inline_scope = self.store.current_inline_scope;
            defer self.store.current_inline_scope = saved_inline_scope;
            self.store.current_loc = self.store.stmtLoc(old_id);
            self.store.current_region = self.store.stmtRegion(old_id);
            self.store.current_inline_scope = self.store.stmtInlineScope(old_id);

            const stmt = self.store.getCFStmt(old_id);
            if (@hasDecl(Rewriter, "interceptStmt")) {
                if (try self.rewriter.interceptStmt(self, stmt)) |intercepted| {
                    try self.stmt_map.put(old_id, intercepted);
                    return intercepted;
                }
            }

            const cloned = switch (stmt) {
                .init_uninitialized => |s| try self.store.addCFStmt(.{ .init_uninitialized = .{
                    .target = try self.mapLocal(s.target),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_ref => |s| try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = try self.mapLocal(s.target),
                    .op = try self.mapRefOp(s.op),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_literal => |s| try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = try self.mapLocal(s.target),
                    .value = s.value,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_call => |s| try self.store.addCFStmt(.{ .assign_call = .{
                    .target = try self.mapLocal(s.target),
                    .proc = s.proc,
                    .args = try self.mapLocalSpan(s.args),
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .out_desc = try self.mapMaybeLocal(s.out_desc),
                    .is_cold = s.is_cold,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_call_erased => |s| try self.store.addCFStmt(.{ .assign_call_erased = .{
                    .target = try self.mapLocal(s.target),
                    .closure = try self.mapLocal(s.closure),
                    .args = try self.mapLocalSpan(s.args),
                    .arg_layouts = s.arg_layouts,
                    .arg_descs = try self.mapLocalSpan(s.arg_descs),
                    .arg_desc_keys = s.arg_desc_keys,
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .out_desc = try self.mapMaybeLocal(s.out_desc),
                    .arg_plan = s.arg_plan,
                    .reuse_closure = s.reuse_closure,
                    .reuse_source = try self.mapMaybeLocal(s.reuse_source),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_packed_erased_fn => |s| try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                    .target = try self.mapLocal(s.target),
                    .proc = s.proc,
                    .capture = try self.mapMaybeLocal(s.capture),
                    .capture_layout = s.capture_layout,
                    .on_drop = s.on_drop,
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .reuse = try self.mapMaybeLocal(s.reuse),
                    .reuse_unique = s.reuse_unique,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_desc_ref => |s| try self.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
                    .target = try self.mapLocal(s.target),
                    .desc = try self.mapBoxyDescRef(s.desc),
                    .nested_index = s.nested_index,
                    .box_payload_layout = s.box_payload_layout,
                    .tag_payload = s.tag_payload,
                    .tag_ext = s.tag_ext,
                    .tag_residual_for = try self.mapMaybeBoxyDescRef(s.tag_residual_for),
                    .captures = try self.mapLocalSpan(s.captures),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_dict_ref => |s| try self.store.addCFStmt(.{ .assign_boxy_dict_ref = .{
                    .target = try self.mapLocal(s.target),
                    .dict = try self.mapBoxyDictRef(s.dict),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_box => |s| try self.store.addCFStmt(.{ .assign_boxy_box = .{
                    .target = try self.mapLocal(s.target),
                    .payload = try self.mapLocal(s.payload),
                    .payload_layout = s.payload_layout,
                    .source_desc = try self.mapMaybeBoxyDescRef(s.source_desc),
                    .payload_desc = try self.mapMaybeBoxyDescRef(s.payload_desc),
                    .payload_mode = s.payload_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_reuse_box => |s| try self.store.addCFStmt(.{ .assign_boxy_reuse_box = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .desc = try self.mapBoxyDescRef(s.desc),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_unbox => |s| try self.store.addCFStmt(.{ .assign_boxy_unbox = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .target_desc = try self.mapMaybeBoxyDescRef(s.target_desc),
                    .target_layout = s.target_layout,
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_adapt => |s| try self.store.addCFStmt(.{ .assign_boxy_adapt = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .adapter = s.adapter,
                    .source_desc = try self.mapMaybeBoxyDescRef(s.source_desc),
                    .target_desc = try self.mapMaybeBoxyDescRef(s.target_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_inspect => |s| try self.store.addCFStmt(.{ .assign_boxy_inspect = .{
                    .target = try self.mapLocal(s.target),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_eq => |s| try self.store.addCFStmt(.{ .assign_boxy_eq = .{
                    .target = try self.mapLocal(s.target),
                    .lhs = try self.mapLocal(s.lhs),
                    .rhs = try self.mapLocal(s.rhs),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_tag => |s| try self.store.addCFStmt(.{ .assign_boxy_tag = .{
                    .target = try self.mapLocal(s.target),
                    .target_desc = try self.mapBoxyDescRef(s.target_desc),
                    .tag_name = s.tag_name,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .payload_layout = s.payload_layout,
                    .payload_desc = try self.mapMaybeBoxyDescRef(s.payload_desc),
                    .payload_mode = s.payload_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_boxy_tag_payload => |s| try self.store.addCFStmt(.{ .assign_boxy_tag_payload = .{
                    .target = try self.mapLocal(s.target),
                    .target_desc = try self.mapMaybeLocal(s.target_desc),
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .tag_name = s.tag_name,
                    .payload_index = s.payload_index,
                    .source_mode = s.source_mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .boxy_tag_match => |s| try self.store.addCFStmt(.{ .boxy_tag_match = .{
                    .source = try self.mapLocal(s.source),
                    .source_desc = try self.mapBoxyDescRef(s.source_desc),
                    .tag_name = s.tag_name,
                    .on_match = try self.cloneStmt(s.on_match),
                    .on_miss = try self.cloneStmt(s.on_miss),
                } }),
                .assign_call_dict => |s| try self.store.addCFStmt(.{ .assign_call_dict = .{
                    .target = try self.mapLocal(s.target),
                    .dict = try self.mapBoxyDictRef(s.dict),
                    .method = s.method,
                    .method_slot = s.method_slot,
                    .args = try self.mapLocalSpan(s.args),
                    .arg_descs = try self.mapLocalSpan(s.arg_descs),
                    .hidden_args = try self.mapLocalSpan(s.hidden_args),
                    .result_desc = try self.mapMaybeBoxyDescRef(s.result_desc),
                    .is_cold = s.is_cold,
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_low_level => |s| try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = try self.mapLocal(s.target),
                    .op = s.op,
                    .rc_effect = s.rc_effect,
                    .unique_args = s.unique_args,
                    .interchangeable = s.interchangeable,
                    .args = try self.mapLocalSpan(s.args),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_list => |s| try self.store.addCFStmt(.{ .assign_list = .{
                    .target = try self.mapLocal(s.target),
                    .elems = try self.mapLocalSpan(s.elems),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_struct => |s| try self.store.addCFStmt(.{ .assign_struct = .{
                    .target = try self.mapLocal(s.target),
                    .fields = try self.mapLocalSpan(s.fields),
                    .next = try self.cloneStmt(s.next),
                } }),
                .assign_tag => |s| try self.store.addCFStmt(.{ .assign_tag = .{
                    .target = try self.mapLocal(s.target),
                    .variant_index = s.variant_index,
                    .discriminant = s.discriminant,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .next = try self.cloneStmt(s.next),
                } }),
                .store_struct => |s| try self.store.addCFStmt(.{ .store_struct = .{
                    .dest = try self.mapLocal(s.dest),
                    .struct_layout = s.struct_layout,
                    .fields = try self.mapLocalSpan(s.fields),
                    .next = try self.cloneStmt(s.next),
                } }),
                .store_tag => |s| try self.store.addCFStmt(.{ .store_tag = .{
                    .dest = try self.mapLocal(s.dest),
                    .tag_layout = s.tag_layout,
                    .variant_index = s.variant_index,
                    .discriminant = s.discriminant,
                    .payload = try self.mapMaybeLocal(s.payload),
                    .next = try self.cloneStmt(s.next),
                } }),
                .set_local => |s| try self.store.addCFStmt(.{ .set_local = .{
                    .target = try self.mapLocal(s.target),
                    .value = try self.mapLocal(s.value),
                    .mode = s.mode,
                    .next = try self.cloneStmt(s.next),
                } }),
                .debug => |s| try self.store.addCFStmt(.{ .debug = .{
                    .message = try self.mapLocal(s.message),
                    .next = try self.cloneStmt(s.next),
                } }),
                .expect => |s| try self.store.addCFStmt(.{ .expect = .{
                    .condition = try self.mapLocal(s.condition),
                    .next = try self.cloneStmt(s.next),
                } }),
                .expect_err => |s| try self.store.addCFStmt(.{ .expect_err = .{
                    .message = try self.mapLocal(s.message),
                    .region = s.region,
                } }),
                .runtime_error => try self.store.addCFStmt(.runtime_error),
                .comptime_exhaustiveness_failed => |s| try self.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{
                    .site = s.site,
                } }),
                .comptime_branch_taken => |s| try self.store.addCFStmt(.{ .comptime_branch_taken = .{
                    .site = s.site,
                    .branch_index = s.branch_index,
                    .next = try self.cloneStmt(s.next),
                } }),
                .incref => |s| try self.store.addCFStmt(.{ .incref = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .count = s.count,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .decref => |s| try self.store.addCFStmt(.{ .decref = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .decref_if_initialized => |s| try self.store.addCFStmt(.{ .decref_if_initialized = .{
                    .cond = try self.mapLocal(s.cond),
                    .cond_mask = s.cond_mask,
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .free => |s| try self.store.addCFStmt(.{ .free = .{
                    .value = try self.mapLocal(s.value),
                    .rc = s.rc,
                    .atomicity = s.atomicity,
                    .next = try self.cloneStmt(s.next),
                } }),
                .switch_stmt => |s| try self.cloneSwitch(s),
                .switch_initialized_payload => |s| try self.store.addCFStmt(.{ .switch_initialized_payload = .{
                    .cond = try self.mapLocal(s.cond),
                    .cond_mask = s.cond_mask,
                    .payload = try self.mapLocal(s.payload),
                    .uninitialized_is_cold = s.uninitialized_is_cold,
                    .initialized_branch = try self.cloneStmt(s.initialized_branch),
                    .uninitialized_branch = try self.cloneStmt(s.uninitialized_branch),
                } }),
                .str_match => |s| try self.store.addCFStmt(.{ .str_match = .{
                    .source = try self.mapLocal(s.source),
                    .prefix = s.prefix,
                    .steps = try self.mapStrMatchSteps(s.steps),
                    .end = s.end,
                    .on_match = try self.cloneStmt(s.on_match),
                    .on_miss = try self.cloneStmt(s.on_miss),
                } }),
                .str_match_set => |s| try self.cloneStrMatchSet(s),
                .loop_continue => try self.store.addCFStmt(.loop_continue),
                .loop_break => try self.store.addCFStmt(.loop_break),
                .join => |s| try self.store.addCFStmt(.{ .join = .{
                    .id = s.id,
                    .params = try self.mapLocalSpan(s.params),
                    .maybe_uninitialized_params = try self.mapLocalSpan(s.maybe_uninitialized_params),
                    .maybe_uninitialized_conditions = try self.mapLocalSpan(s.maybe_uninitialized_conditions),
                    .maybe_uninitialized_condition_masks = s.maybe_uninitialized_condition_masks,
                    .body = try self.cloneStmt(s.body),
                    .remainder = try self.cloneStmt(s.remainder),
                } }),
                .jump => |s| try self.store.addCFStmt(.{ .jump = .{ .target = s.target } }),
                .ret => |s| try self.rewriter.cloneRet(self, s.value),
                .crash => |s| try self.store.addCFStmt(.{ .crash = .{ .msg = switch (s.msg) {
                    .literal => |literal| .{ .literal = literal },
                    .local => |local| .{ .local = try self.mapLocal(local) },
                } } }),
            };

            try self.stmt_map.put(old_id, cloned);
            return cloned;
        }

        /// True when `next` is a `ret` of exactly `value`, marking a direct
        /// constructor/concat return the rewriter may fuse into its tail.
        pub fn directReturnOf(self: *const Self, next: CFStmtId, value: LocalId) bool {
            const stmt = self.store.getCFStmt(next);
            return stmt == .ret and stmt.ret.value == value;
        }

        fn cloneSwitch(self: *Self, s: anytype) Allocator.Error!CFStmtId {
            const old_branches = self.store.getCFSwitchBranches(s.branches);
            const branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, old_branches.len);
            defer self.store.allocator.free(branches);
            for (0..old_branches.len) |index| {
                const old = GuardedList.at(old_branches, index);
                const new = &branches[index];
                new.* = .{
                    .value = old.value,
                    .body = try self.cloneStmt(old.body),
                };
            }
            return try self.store.addCFStmt(.{ .switch_stmt = .{
                .cond = try self.mapLocal(s.cond),
                .branches = try self.store.addCFSwitchBranches(branches),
                .default_branch = try self.cloneStmt(s.default_branch),
                .default_is_cold = s.default_is_cold,
                .continuation = if (s.continuation) |continuation| try self.cloneStmt(continuation) else null,
            } });
        }

        fn cloneStrMatchSet(self: *Self, s: anytype) Allocator.Error!CFStmtId {
            const old_arms = self.store.getStrMatchArms(s.arms);
            const arms = try self.store.allocator.alloc(LIR.StrMatchArm, old_arms.len);
            defer self.store.allocator.free(arms);
            for (0..old_arms.len) |index| {
                const old = GuardedList.at(old_arms, index);
                const new = &arms[index];
                new.* = .{
                    .prefix = old.prefix,
                    .steps = try self.mapStrMatchSteps(old.steps),
                    .end = old.end,
                    .on_match = try self.cloneStmt(old.on_match),
                };
            }
            return try self.store.addCFStmt(.{ .str_match_set = .{
                .source = try self.mapLocal(s.source),
                .arms = try self.store.addStrMatchArms(arms),
                .on_miss = try self.cloneStmt(s.on_miss),
            } });
        }

        fn mapStrMatchSteps(self: *Self, span: LIR.StrMatchStepSpan) Allocator.Error!LIR.StrMatchStepSpan {
            const old_steps = self.store.getStrMatchSteps(span);
            const steps = try self.store.allocator.alloc(LIR.StrMatchStep, old_steps.len);
            defer self.store.allocator.free(steps);
            for (0..old_steps.len) |index| {
                const old = GuardedList.at(old_steps, index);
                const new = &steps[index];
                new.* = old;
                new.capture = switch (old.capture) {
                    .discard => .discard,
                    .view => |local| .{ .view = try self.mapLocal(local) },
                };
            }
            return try self.store.addStrMatchSteps(steps);
        }

        fn mapRefOp(self: *Self, op: LIR.RefOp) Allocator.Error!LIR.RefOp {
            return switch (op) {
                .local => |local| .{ .local = try self.mapLocal(local) },
                .discriminant => |d| .{ .discriminant = .{ .source = try self.mapLocal(d.source) } },
                .field => |f| .{ .field = .{
                    .source = try self.mapLocal(f.source),
                    .field_idx = f.field_idx,
                } },
                .tag_payload => |t| .{ .tag_payload = .{
                    .source = try self.mapLocal(t.source),
                    .payload_idx = t.payload_idx,
                    .variant_index = t.variant_index,
                    .tag_discriminant = t.tag_discriminant,
                } },
                .tag_payload_struct => |t| .{ .tag_payload_struct = .{
                    .source = try self.mapLocal(t.source),
                    .variant_index = t.variant_index,
                    .tag_discriminant = t.tag_discriminant,
                } },
                .list_reinterpret => |l| .{ .list_reinterpret = .{ .backing_ref = try self.mapLocal(l.backing_ref) } },
                .nominal => |n| .{ .nominal = .{ .backing_ref = try self.mapLocal(n.backing_ref) } },
            };
        }

        /// Clone a local-id span, remapping each element.
        pub fn mapLocalSpan(self: *Self, span: LIR.LocalSpan) Allocator.Error!LIR.LocalSpan {
            const old_locals = self.store.getLocalSpan(span);
            const locals = try self.store.allocator.alloc(LocalId, old_locals.len);
            defer self.store.allocator.free(locals);
            for (0..old_locals.len) |index| {
                locals[index] = try self.mapLocal(GuardedList.at(old_locals, index));
            }
            return try self.store.addLocalSpan(locals);
        }

        /// Remap an optional local, preserving `null`.
        pub fn mapMaybeLocal(self: *Self, maybe: ?LocalId) Allocator.Error!?LocalId {
            return if (maybe) |local| try self.mapLocal(local) else null;
        }

        fn mapBoxyDescRef(self: *Self, desc: LIR.BoxyDescRef) Allocator.Error!LIR.BoxyDescRef {
            return switch (desc) {
                .static => |id| .{ .static = id },
                .local => |local| .{ .local = try self.mapLocal(local) },
                .runtime => |index| .{ .runtime = index },
                .dict_method_arg => |arg| .{ .dict_method_arg = arg },
                .dict_method_hidden => |hidden| .{ .dict_method_hidden = hidden },
            };
        }

        fn mapMaybeBoxyDescRef(self: *Self, maybe: ?LIR.BoxyDescRef) Allocator.Error!?LIR.BoxyDescRef {
            return if (maybe) |desc| try self.mapBoxyDescRef(desc) else null;
        }

        fn mapBoxyDictRef(self: *Self, dict: LIR.BoxyDictRef) Allocator.Error!LIR.BoxyDictRef {
            return switch (dict) {
                .static => |id| .{ .static = id },
                .local => |local| .{ .local = try self.mapLocal(local) },
            };
        }

        /// Map an old local to its clone, allocating a fresh same-layout local
        /// on first encounter.
        pub fn mapLocal(self: *Self, old: LocalId) Allocator.Error!LocalId {
            const index = @intFromEnum(old);
            if (index >= self.local_map.len) unreachable;
            if (self.local_map[index]) |existing| return existing;

            const old_local = self.store.getLocal(old);
            const fresh = try self.store.addLocal(.{ .layout_idx = old_local.layout_idx });
            self.local_map[index] = fresh;
            const boxy_desc = try self.mapMaybeBoxyDescRef(old_local.boxy_desc);
            if (boxy_desc) |desc| self.store.setLocalBoxyDesc(fresh, desc);
            try self.new_locals.append(self.store.allocator, fresh);
            return fresh;
        }

        /// Allocate a fresh local of `layout_idx` owned by the clone.
        pub fn addTemp(self: *Self, layout_idx: layout_mod.Idx) Allocator.Error!LocalId {
            const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
            try self.new_locals.append(self.store.allocator, local);
            return local;
        }
    };
}

//! Control-flow ordering of explicit LIR value uses and definitions.
//! Shared by value-flow rewrites and ARC; this module makes no ownership decisions.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const core = @import("lir_core");
const body_clone = @import("body_clone.zig");
const Allocator = std.mem.Allocator;
const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const no_local = std.math.maxInt(u32);

/// Exact topology construction count for reuse tests.
pub threadlocal var topology_builds: usize = 0;

/// Orders the uses of a local along control flow. A consuming use takes the
/// value's single ownership unit with it, so the value is unique at that use
/// exactly when no other use of the same local can execute afterwards
/// before the local is redefined: a read before the consume has finished
/// with the allocation, and two consumes on exclusive branches never both
/// run. The query walks the procedure's successor edges from the use,
/// following jumps through the procedure's joins and stopping at a
/// redefinition of the local (an initializing write to it, or entering a
/// join that declares it as a parameter), and reports whether it meets a
/// statement that reads the local. Statement inventories are per procedure;
/// variants sharing a body resolve its jumps to the same joins.
pub const UseOrder = struct {
    allocator: Allocator,
    topology: Topology,
    /// Component views borrow immutable CSR topology and own only compact marks.
    component: ?struct {
        stmt_to_dense: []const u32,
        stmt_owner: []const u32,
        id: u32,
        unresolved: []const u32,
    } = null,
    /// Generation stamps: a statement is marked for the current local when
    /// its stamp equals `generation`.
    mark_gen: []u32,
    generation: u32,
    /// The local the current marks describe, and whether they were made
    /// from its reads (rather than a caller's statement set).
    marked_local: u32,
    marked_uses: bool,
    work: std.ArrayList(u32),
    /// Only the fixed-point workspace enables this cache. Each key names
    /// an immutable ordered-use question, independent of signatures/takes.
    use_answers: ?*std.AutoHashMap(UseQuery, bool) = null,
    backward_visits: usize = 0,

    pub const UseQuery = struct { stmt: LIR.CFStmtId, local: LIR.LocalId };

    /// Explicit compact domains for one procedure. Whole-program ARC already
    /// owns dense store-wide domains and leaves this absent.
    const Domain = struct {
        stmts: collections.DenseMap(LIR.CFStmtId, u32),
        locals: collections.DenseMap(LIR.LocalId, u32),

        fn create(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId) !*Domain {
            const self = try allocator.create(Domain);
            self.* = .{ .stmts = .init(allocator), .locals = .init(allocator) };
            errdefer self.destroy(allocator);
            const Locals = struct {
                domain: *Domain,
                err: ?Allocator.Error = null,
                fn note(ctx: *@This(), local: LIR.LocalId) void {
                    if (ctx.err != null or ctx.domain.locals.contains(local)) return;
                    ctx.domain.locals.put(local, @intCast(ctx.domain.locals.count())) catch |err| {
                        ctx.err = err;
                    };
                }
            };
            var locals = Locals{ .domain = self };
            for (lists) |list| for (list) |id| {
                if (self.stmts.contains(id)) continue;
                try self.stmts.put(id, @intCast(self.stmts.count()));
                const stmt = store.getCFStmt(id);
                body_clone.forEachStmtRead(store, stmt, &locals, Locals.note);
                body_clone.forEachStmtDef(store, stmt, &locals, Locals.note);
                if (locals.err) |err| return err;
            };
            return self;
        }

        fn destroy(self: *Domain, allocator: Allocator) void {
            self.stmts.deinit();
            self.locals.deinit();
            allocator.destroy(self);
        }
    };

    /// Immutable store-wide CSR and control-flow topology. Component orders
    /// borrow these slices; all marking and query-cache state lives separately.
    pub const Topology = struct {
        store: *const LirStore,
        domain: ?*Domain = null,
        /// Statements reading and defining each local.
        reads_of: Rows,
        defs_of: Rows,
        /// The join statement each jump targets, or `no_local`.
        jump_join: []u32,
        /// Reverse edges of `forEachSuccessor`.
        preds: Rows,
        /// Unknown successors conservatively count as used.
        unresolved: std.bit_set.DynamicBitSetUnmanaged,

        pub fn deinit(self: *@This(), allocator: Allocator) void {
            self.reads_of.deinit(allocator);
            self.defs_of.deinit(allocator);
            self.preds.deinit(allocator);
            self.unresolved.deinit(allocator);
            allocator.free(self.jump_join);
            if (self.domain) |domain| domain.destroy(allocator);
        }
    };

    /// CSR rows of statement ids per key, each row sorted.
    pub const Rows = struct {
        offsets: []u32,
        stmts: []u32,
        domain: ?*const Domain = null,
        statement_keys: bool = false,

        pub fn row(self: *const Rows, local: LIR.LocalId) []const u32 {
            return self.rowAt(@intFromEnum(local));
        }

        pub fn rowAt(self: *const Rows, key: u32) []const u32 {
            const index = if (self.domain) |domain|
                (if (self.statement_keys) domain.stmts.get(@enumFromInt(key)) else domain.locals.get(@enumFromInt(key))) orelse return &.{}
            else
                key;
            if (index + 1 >= self.offsets.len) return &.{};
            return self.stmts[self.offsets[index]..self.offsets[index + 1]];
        }

        pub fn deinit(self: *Rows, allocator: Allocator) void {
            allocator.free(self.offsets);
            allocator.free(self.stmts);
        }
    };

    const RowKind = enum { reads, defs };

    pub fn init(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId) Allocator.Error!UseOrder {
        var topology = try initTopology(allocator, store, lists);
        errdefer topology.deinit(allocator);
        const mark_gen = try allocator.alloc(u32, store.cfStmtCount());
        @memset(mark_gen, 0);
        return .{
            .allocator = allocator,
            .topology = topology,
            .mark_gen = mark_gen,
            .generation = 0,
            .marked_local = no_local,
            .marked_uses = false,
            .work = .empty,
        };
    }

    pub fn initTopology(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId) Allocator.Error!Topology {
        return initTopologyInDomain(allocator, store, lists, null);
    }

    fn stmtIndex(domain: ?*const Domain, raw: u32) u32 {
        return if (domain) |d| d.stmts.get(@enumFromInt(raw)).? else raw;
    }

    fn localIndex(domain: ?*const Domain, local: LIR.LocalId) u32 {
        return if (domain) |d| d.locals.get(local).? else @intFromEnum(local);
    }

    fn initTopologyInDomain(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId, domain: ?*Domain) Allocator.Error!Topology {
        if (builtin.is_test) topology_builds += 1;
        const stmt_count = if (domain) |d| d.stmts.count() else store.cfStmtCount();
        const jump_join = try allocator.alloc(u32, stmt_count);
        errdefer allocator.free(jump_join);
        @memset(jump_join, no_local);
        var joins = collections.DenseMap(LIR.JoinPointId, u32).init(allocator);
        defer joins.deinit();
        for (lists) |list| {
            joins.clearRetainingCapacity();
            for (list) |stmt_id| {
                const stmt = store.getCFStmt(stmt_id);
                if (stmt == .join) try joins.put(stmt.join.id, @intFromEnum(stmt_id));
            }
            for (list) |stmt_id| {
                const stmt = store.getCFStmt(stmt_id);
                if (stmt != .jump) continue;
                if (joins.get(stmt.jump.target)) |join_stmt| {
                    jump_join[stmtIndex(domain, @intFromEnum(stmt_id))] = join_stmt;
                }
            }
        }

        var reads_of = try buildRows(allocator, store, lists, domain, .reads);
        errdefer reads_of.deinit(allocator);
        var defs_of = try buildRows(allocator, store, lists, domain, .defs);
        errdefer defs_of.deinit(allocator);

        var unresolved = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, stmt_count);
        errdefer unresolved.deinit(allocator);
        var preds = try buildPreds(allocator, store, lists, domain, jump_join, &unresolved);
        errdefer preds.deinit(allocator);
        return .{
            .store = store,
            .domain = domain,
            .reads_of = reads_of,
            .defs_of = defs_of,
            .jump_join = jump_join,
            .preds = preds,
            .unresolved = unresolved,
        };
    }

    /// Whether a statement contributes to the rows of `kind`.
    /// Reference-counting statements are ARC's bookkeeping, not uses of the
    /// value: the analysis runs before they exist, and the certifier
    /// re-derives it from the emitted procedure where they do. A join
    /// declares its parameters; the jumps entering its body define them.
    fn rowsInclude(stmt: LIR.CFStmt, comptime kind: RowKind) bool {
        return switch (stmt) {
            .incref, .decref, .decref_if_initialized, .free => false,
            .join => kind == .reads,
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
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
            .boxy_tag_match,
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
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => true,
        };
    }

    fn buildRows(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId, domain: ?*const Domain, comptime kind: RowKind) Allocator.Error!Rows {
        const stmt_count = if (domain) |d| d.stmts.count() else store.cfStmtCount();
        const local_count = if (domain) |d| d.locals.count() else store.localCount();
        var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, stmt_count);
        defer seen.deinit(allocator);
        const offsets = try allocator.alloc(u32, local_count + 1);
        errdefer allocator.free(offsets);
        @memset(offsets, 0);
        const Count = struct {
            offsets: []u32,
            domain: ?*const Domain,
            fn note(self: *@This(), local: LIR.LocalId) void {
                self.offsets[localIndex(self.domain, local) + 1] += 1;
            }
        };
        var count = Count{ .offsets = offsets, .domain = domain };
        for (lists) |list| {
            for (list) |stmt_id| {
                const index = stmtIndex(domain, @intFromEnum(stmt_id));
                if (seen.isSet(index)) continue;
                seen.set(index);
                const stmt = store.getCFStmt(stmt_id);
                if (!rowsInclude(stmt, kind)) continue;
                switch (kind) {
                    .reads => body_clone.forEachStmtRead(store, stmt, &count, Count.note),
                    .defs => body_clone.forEachStmtDef(store, stmt, &count, Count.note),
                }
            }
        }
        for (0..local_count) |local| offsets[local + 1] += offsets[local];
        const stmts = try allocator.alloc(u32, offsets[local_count]);
        errdefer allocator.free(stmts);
        const fill = try allocator.dupe(u32, offsets[0..local_count]);
        defer allocator.free(fill);
        const Fill = struct {
            fill: []u32,
            stmts: []u32,
            stmt: u32,
            domain: ?*const Domain,
            fn note(self: *@This(), local: LIR.LocalId) void {
                const raw = localIndex(self.domain, local);
                self.stmts[self.fill[raw]] = self.stmt;
                self.fill[raw] += 1;
            }
        };
        var filler = Fill{ .fill = fill, .stmts = stmts, .stmt = 0, .domain = domain };
        seen.setRangeValue(.{ .start = 0, .end = stmt_count }, false);
        for (lists) |list| {
            for (list) |stmt_id| {
                const index = stmtIndex(domain, @intFromEnum(stmt_id));
                if (seen.isSet(index)) continue;
                seen.set(index);
                filler.stmt = @intFromEnum(stmt_id);
                const stmt = store.getCFStmt(stmt_id);
                if (!rowsInclude(stmt, kind)) continue;
                switch (kind) {
                    .reads => body_clone.forEachStmtRead(store, stmt, &filler, Fill.note),
                    .defs => body_clone.forEachStmtDef(store, stmt, &filler, Fill.note),
                }
            }
        }
        for (0..local_count) |local| {
            std.mem.sort(u32, stmts[offsets[local]..offsets[local + 1]], {}, std.sort.asc(u32));
        }
        return .{ .offsets = offsets, .stmts = stmts, .domain = domain };
    }

    /// Predecessor rows over the successor edges of every statement in the
    /// inventories, and the set of statements whose successors are unknown.
    fn buildPreds(
        allocator: Allocator,
        store: *const LirStore,
        lists: []const []const LIR.CFStmtId,
        domain: ?*const Domain,
        jump_join: []const u32,
        unresolved: *std.bit_set.DynamicBitSetUnmanaged,
    ) Allocator.Error!Rows {
        const stmt_count = if (domain) |d| d.stmts.count() else store.cfStmtCount();
        var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, stmt_count);
        defer seen.deinit(allocator);
        const offsets = try allocator.alloc(u32, stmt_count + 1);
        errdefer allocator.free(offsets);
        @memset(offsets, 0);
        const Count = struct {
            offsets: []u32,
            domain: ?*const Domain,
            fn note(self: *@This(), succ: u32) void {
                self.offsets[stmtIndex(self.domain, succ) + 1] += 1;
            }
        };
        var count = Count{ .offsets = offsets, .domain = domain };
        for (lists) |list| {
            for (list) |stmt_id| {
                const index = stmtIndex(domain, @intFromEnum(stmt_id));
                if (seen.isSet(index)) continue;
                seen.set(index);
                if (forEachSuccessor(store, domain, jump_join, @intFromEnum(stmt_id), &count, Count.note)) unresolved.set(index);
            }
        }
        for (0..stmt_count) |index| offsets[index + 1] += offsets[index];
        const stmts = try allocator.alloc(u32, offsets[stmt_count]);
        errdefer allocator.free(stmts);
        const fill = try allocator.dupe(u32, offsets[0..stmt_count]);
        defer allocator.free(fill);
        const Fill = struct {
            fill: []u32,
            stmts: []u32,
            stmt: u32,
            domain: ?*const Domain,
            fn note(self: *@This(), succ: u32) void {
                const index = stmtIndex(self.domain, succ);
                self.stmts[self.fill[index]] = self.stmt;
                self.fill[index] += 1;
            }
        };
        var filler = Fill{ .fill = fill, .stmts = stmts, .stmt = 0, .domain = domain };
        seen.setRangeValue(.{ .start = 0, .end = stmt_count }, false);
        for (lists) |list| {
            for (list) |stmt_id| {
                const index = stmtIndex(domain, @intFromEnum(stmt_id));
                if (seen.isSet(index)) continue;
                seen.set(index);
                filler.stmt = @intFromEnum(stmt_id);
                _ = forEachSuccessor(store, domain, jump_join, @intFromEnum(stmt_id), &filler, Fill.note);
            }
        }
        return .{ .offsets = offsets, .stmts = stmts, .domain = domain, .statement_keys = true };
    }

    /// Statement inventories walked structurally from each procedure body,
    /// for callers that hold no per-procedure lists of their own.
    pub fn initFromStore(allocator: Allocator, store: *const LirStore, only_proc: ?LIR.LirProcSpecId) Allocator.Error!UseOrder {
        var lists = std.ArrayList([]const LIR.CFStmtId).empty;
        defer {
            for (lists.items) |list| allocator.free(list);
            lists.deinit(allocator);
        }
        var seen = collections.DenseMap(LIR.CFStmtId, void).init(allocator);
        defer seen.deinit();
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(allocator);
        const first: usize = if (only_proc) |id| @intFromEnum(id) else 0;
        const end = if (only_proc != null) first + 1 else store.procSpecCount();
        for (first..end) |proc_index| {
            const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
            const body = proc.body orelse continue;
            var list = std.ArrayList(LIR.CFStmtId).empty;
            errdefer list.deinit(allocator);
            seen.clearRetainingCapacity();
            stack.clearRetainingCapacity();
            try stack.append(allocator, body);
            while (stack.pop()) |current| {
                if (seen.contains(current)) continue;
                try seen.put(current, {});
                try list.append(allocator, current);
                try appendStructuralSuccessors(allocator, store, &stack, store.getCFStmt(current));
            }
            try lists.ensureUnusedCapacity(allocator, 1);
            lists.appendAssumeCapacity(try list.toOwnedSlice(allocator));
        }
        if (only_proc == null) return init(allocator, store, lists.items);
        const domain = try Domain.create(allocator, store, lists.items);
        errdefer domain.destroy(allocator);
        var topology = try initTopologyInDomain(allocator, store, lists.items, domain);
        errdefer {
            topology.domain = null;
            topology.deinit(allocator);
        }
        const marks = try allocator.alloc(u32, domain.stmts.count());
        @memset(marks, 0);
        return .{
            .allocator = allocator,
            .topology = topology,
            .mark_gen = marks,
            .generation = 0,
            .marked_local = no_local,
            .marked_uses = false,
            .work = .empty,
        };
    }

    pub fn deinit(self: *UseOrder) void {
        if (self.component == null) self.topology.deinit(self.allocator);
        self.allocator.free(self.mark_gen);
        self.work.deinit(self.allocator);
    }

    fn markIndex(self: *const UseOrder, stmt: u32) u32 {
        if (self.component) |component| {
            std.debug.assert(component.stmt_owner[stmt] == component.id);
            return component.stmt_to_dense[stmt];
        }
        return stmtIndex(self.topology.domain, stmt);
    }

    fn contains(row: []const u32, stmt: u32) bool {
        var lo: usize = 0;
        var hi: usize = row.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            if (row[mid] == stmt) return true;
            if (row[mid] < stmt) lo = mid + 1 else hi = mid;
        }
        return false;
    }

    fn reads(self: *const UseOrder, stmt: u32, local: LIR.LocalId) bool {
        return contains(self.topology.reads_of.row(local), stmt);
    }

    fn defines(self: *const UseOrder, stmt: u32, local: LIR.LocalId) bool {
        return contains(self.topology.defs_of.row(local), stmt);
    }

    /// Whether another use of `local` can execute after the use at `from`
    /// before `local` is redefined. Marks from the local's reads once and
    /// answers every later query about the same local from those marks.
    pub fn usesAfter(self: *UseOrder, from: u32, local: LIR.LocalId) Allocator.Error!bool {
        const key: UseQuery = .{ .stmt = @enumFromInt(from), .local = local };
        if (self.use_answers) |answers| if (answers.get(key)) |answer| return answer;
        if (self.marked_local != @intFromEnum(local) or !self.marked_uses) {
            try self.markFrom(local, self.topology.reads_of.row(local));
            self.marked_local = @intFromEnum(local);
            self.marked_uses = true;
        }
        const answer = self.after(from, local);
        if (self.use_answers) |answers| try answers.put(key, answer);
        return answer;
    }

    /// Marks the statements from which one of `among` can execute before
    /// `local` is redefined, for `after` queries about that local.
    pub fn markAmong(self: *UseOrder, local: LIR.LocalId, among: []const u32) Allocator.Error!void {
        try self.markFrom(local, among);
        self.marked_local = @intFromEnum(local);
        self.marked_uses = false;
    }

    /// Whether a marked statement can execute after `stmt`.
    pub fn after(self: *const UseOrder, stmt: u32, local: LIR.LocalId) bool {
        if (self.topology.unresolved.isSet(stmtIndex(self.topology.domain, stmt))) return true;
        const Probe = struct {
            order: *const UseOrder,
            stmt: u32,
            local: LIR.LocalId,
            hit: bool,
            fn note(self_probe: *@This(), succ: u32) void {
                if (self_probe.order.cutEdge(self_probe.stmt, succ, self_probe.local)) return;
                if (self_probe.order.mark_gen[self_probe.order.markIndex(succ)] == self_probe.order.generation) self_probe.hit = true;
            }
        };
        var probe = Probe{ .order = self, .stmt = stmt, .local = local, .hit = false };
        _ = forEachSuccessor(self.topology.store, self.topology.domain, self.topology.jump_join, stmt, &probe, Probe.note);
        return probe.hit;
    }

    /// Backward reachability from `initial` and the unresolved statements:
    /// a statement is marked when executing from it reaches one of them
    /// before `local` is redefined.
    fn markFrom(self: *UseOrder, local: LIR.LocalId, initial: []const u32) Allocator.Error!void {
        self.generation +%= 1;
        if (self.generation == 0) {
            @memset(self.mark_gen, 0);
            self.generation = 1;
        }
        self.work.clearRetainingCapacity();
        for (initial) |stmt| try self.mark(stmt);
        if (self.component) |component| {
            for (component.unresolved) |stmt| try self.mark(stmt);
        } else {
            var unresolved_iter = self.topology.unresolved.iterator(.{});
            if (self.topology.domain) |domain| {
                var it = domain.stmts.iterator();
                while (it.next()) |entry| if (self.topology.unresolved.isSet(entry.value_ptr.*)) {
                    try self.mark(@intFromEnum(entry.key_ptr.*));
                };
            } else {
                while (unresolved_iter.next()) |stmt| try self.mark(@intCast(stmt));
            }
        }
        while (self.work.pop()) |stmt| {
            if (builtin.is_test) self.backward_visits += 1;
            for (self.topology.preds.rowAt(stmt)) |pred| {
                if (self.mark_gen[self.markIndex(pred)] == self.generation) continue;
                if (self.cutEdge(pred, stmt, local)) continue;
                if (self.defines(pred, local)) continue;
                try self.mark(pred);
            }
        }
    }

    fn mark(self: *UseOrder, stmt: u32) Allocator.Error!void {
        const index = self.markIndex(stmt);
        if (self.mark_gen[index] == self.generation) return;
        self.mark_gen[index] = self.generation;
        try self.work.append(self.allocator, stmt);
    }

    /// A jump into a join that declares `local` as a parameter redefines
    /// it, so that edge carries no use of the previous value.
    fn cutEdge(self: *const UseOrder, from: u32, to: u32, local: LIR.LocalId) bool {
        const node = self.topology.store.getCFStmt(@enumFromInt(from));
        if (node != .jump) return false;
        const join_stmt = self.topology.jump_join[stmtIndex(self.topology.domain, from)];
        if (join_stmt == no_local) return false;
        const join = self.topology.store.getCFStmt(@enumFromInt(join_stmt)).join;
        if (@intFromEnum(join.body) != to) return false;
        const params = self.topology.store.getLocalSpan(join.params);
        for (0..GuardedList.borrowLen(params)) |index| {
            if (GuardedList.at(params, index) == local) return true;
        }
        return false;
    }

    /// Calls `note(ctx, successor)` for each control-flow successor of
    /// `stmt`; returns true when an edge cannot be resolved.
    fn forEachSuccessor(store: *const LirStore, domain: ?*const Domain, jump_join: []const u32, stmt: u32, ctx: anytype, comptime note: fn (@TypeOf(ctx), u32) void) bool {
        switch (store.getCFStmt(@enumFromInt(stmt))) {
            inline .init_uninitialized,
            .assign_ref,
            .assign_literal,
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
            => |node| note(ctx, @intFromEnum(node.next)),
            .switch_stmt => |node| {
                if (node.continuation) |continuation| note(ctx, @intFromEnum(continuation));
                note(ctx, @intFromEnum(node.default_branch));
                const branches = store.getCFSwitchBranches(node.branches);
                for (0..GuardedList.borrowLen(branches)) |index| {
                    note(ctx, @intFromEnum(GuardedList.at(branches, index).body));
                }
            },
            .switch_initialized_payload => |node| {
                note(ctx, @intFromEnum(node.initialized_branch));
                note(ctx, @intFromEnum(node.uninitialized_branch));
            },
            .str_match => |node| {
                note(ctx, @intFromEnum(node.on_match));
                note(ctx, @intFromEnum(node.on_miss));
            },
            .boxy_tag_match => |node| {
                note(ctx, @intFromEnum(node.on_match));
                note(ctx, @intFromEnum(node.on_miss));
            },
            .str_match_set => |node| {
                const arms = store.getStrMatchArms(node.arms);
                for (0..GuardedList.borrowLen(arms)) |index| {
                    note(ctx, @intFromEnum(GuardedList.at(arms, index).on_match));
                }
                note(ctx, @intFromEnum(node.on_miss));
            },
            // A join's body runs only when jumped to; declaring the join
            // continues with its remainder.
            .join => |node| note(ctx, @intFromEnum(node.remainder)),
            .jump => {
                const join_stmt = jump_join[stmtIndex(domain, stmt)];
                if (join_stmt == no_local) return true;
                note(ctx, @intFromEnum(store.getCFStmt(@enumFromInt(join_stmt)).join.body));
            },
            .loop_continue, .loop_break => return true,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .expect_err,
            .ret,
            .crash,
            => {},
        }
        return false;
    }
};

/// Append every structurally owned child, including join bodies and remainders.
pub fn appendStructuralSuccessors(
    allocator: Allocator,
    store: *const LirStore,
    stack: *std.ArrayList(LIR.CFStmtId),
    stmt: LIR.CFStmt,
) Allocator.Error!void {
    switch (stmt) {
        .switch_stmt => |switch_stmt| {
            const branches = store.getCFSwitchBranches(switch_stmt.branches);
            for (0..GuardedList.borrowLen(branches)) |branch_index| {
                try stack.append(allocator, GuardedList.at(branches, branch_index).body);
            }
            try stack.append(allocator, switch_stmt.default_branch);
            if (switch_stmt.continuation) |continuation| try stack.append(allocator, continuation);
        },
        .switch_initialized_payload => |switch_stmt| {
            try stack.append(allocator, switch_stmt.initialized_branch);
            try stack.append(allocator, switch_stmt.uninitialized_branch);
        },
        .str_match => |str_match| {
            try stack.append(allocator, str_match.on_match);
            try stack.append(allocator, str_match.on_miss);
        },
        .str_match_set => |str_match_set| {
            const arms = store.getStrMatchArms(str_match_set.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                try stack.append(allocator, GuardedList.at(arms, arm_index).on_match);
            }
            try stack.append(allocator, str_match_set.on_miss);
        },
        .boxy_tag_match => |tag_match| {
            try stack.append(allocator, tag_match.on_match);
            try stack.append(allocator, tag_match.on_miss);
        },
        .join => |join_stmt| {
            try stack.append(allocator, join_stmt.body);
            try stack.append(allocator, join_stmt.remainder);
        },
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
        => |linear| try stack.append(allocator, linear.next),
        .jump,
        .ret,
        .crash,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .loop_continue,
        .loop_break,
        => {},
    }
}

test "use order compact procedure domain matches dense queries and excludes unrelated rows" {
    const testing = std.testing;
    const allocator = testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    // A high store ID is not a large procedure domain.
    for (0..1024) |_| {
        const unused = try store.addLocal(.{ .layout_idx = .u64 });
        _ = try store.addCFStmt(.{ .ret = .{ .value = unused } });
    }
    const input = try store.addLocal(.{ .layout_idx = .u64 });
    const view = try store.addLocal(.{ .layout_idx = .u64 });
    const done = try store.addCFStmt(.{ .ret = .{ .value = view } });
    const alias = try store.addCFStmt(.{ .assign_ref = .{ .target = view, .op = .{ .local = input }, .next = done } });
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(11661),
        .args = try store.addLocalSpan(&.{input}),
        .body = alias,
        .ret_layout = .u64,
    });
    var compact = try UseOrder.initFromStore(allocator, &store, proc);
    defer compact.deinit();
    var dense = try UseOrder.init(allocator, &store, &.{&.{ alias, done }});
    defer dense.deinit();
    try testing.expectEqual(@as(usize, 2), compact.mark_gen.len);
    try testing.expectEqual(@as(usize, 3), compact.topology.reads_of.offsets.len);
    try testing.expectEqual(@as(usize, 3), compact.topology.preds.offsets.len);
    for ([_]LIR.CFStmtId{ alias, done }) |stmt| {
        for ([_]LIR.LocalId{ input, view }) |local| {
            try testing.expectEqual(try dense.usesAfter(@intFromEnum(stmt), local), try compact.usesAfter(@intFromEnum(stmt), local));
        }
    }
    try testing.expect(try compact.usesAfter(@intFromEnum(alias), view));
    try testing.expect(!try compact.usesAfter(@intFromEnum(alias), input));
}

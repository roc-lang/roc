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

        fn create(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId) Allocator.Error!*Domain {
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

    /// Immutable control-flow and ordered-use topology over a set of
    /// statement inventories. Component orders borrow it; all marking and
    /// query-cache state lives separately.
    pub const Topology = struct {
        store: *const LirStore,
        /// Store-indexed tables holding this topology's entries.
        tables: *Scratch,
        /// Whether `tables` belongs to this topology alone. Otherwise they are
        /// a caller's reusable scratch, and releasing the topology resets
        /// exactly the entries it wrote.
        owns_tables: bool,
        /// Each inventoried statement once, retained only to reset a
        /// caller's scratch on release.
        members: []u32,
        /// Statements reading and defining each local.
        reads_of: Rows,
        defs_of: Rows,
        /// The join statement each jump targets, or `no_local`.
        jump_join: []u32,
        /// Reverse edges of `forEachSuccessor`.
        preds: Rows,
        /// Unknown successors conservatively count as used.
        unresolved: std.bit_set.DynamicBitSetUnmanaged,
        unresolved_list: []u32,

        pub fn deinit(self: *@This(), allocator: Allocator) void {
            if (self.owns_tables) {
                if (self.tables.domain) |domain| domain.destroy(allocator);
                self.tables.deinit(allocator);
                allocator.destroy(self.tables);
            } else {
                for (self.members) |stmt| {
                    self.tables.jump_join[self.tables.stmtIndex(stmt)] = no_local;
                    self.tables.mark_gen[self.tables.stmtIndex(stmt)] = 0;
                    self.tables.unresolved.unset(self.tables.stmtIndex(stmt));
                }
                self.reads_of.clearIndex();
                self.defs_of.clearIndex();
                self.preds.clearIndex();
            }
            self.reads_of.deinit(allocator);
            self.defs_of.deinit(allocator);
            self.preds.deinit(allocator);
            allocator.free(self.unresolved_list);
            allocator.free(self.members);
        }
    };

    /// Store-indexed tables for building topologies. A caller that builds
    /// many small topologies over one store (one per procedure) allocates
    /// these once; each build writes and later resets only its own entries,
    /// so its cost is proportional to its inventories, not to the store.
    /// Between builds every row is empty, every jump is unresolved, and
    /// every mark is zero.
    pub const Scratch = struct {
        domain: ?*Domain = null,
        jump_join: []u32,
        reads_index: RowIndex,
        defs_index: RowIndex,
        preds_index: RowIndex,
        seen: std.bit_set.DynamicBitSetUnmanaged,
        unresolved: std.bit_set.DynamicBitSetUnmanaged,
        mark_gen: []u32,

        pub fn init(allocator: Allocator, store: *const LirStore) Allocator.Error!Scratch {
            return initTables(allocator, store, .with_marks);
        }

        /// Component orders keep their own compact marks, so a topology built
        /// only for them needs no store-indexed marks.
        fn initTables(allocator: Allocator, store: *const LirStore, marks: Marks) Allocator.Error!Scratch {
            return initTablesInDomain(allocator, store, marks, null);
        }

        fn stmtIndex(self: *const Scratch, raw: u32) u32 {
            return if (self.domain) |domain| domain.stmts.get(@enumFromInt(raw)).? else raw;
        }

        fn initTablesInDomain(allocator: Allocator, store: *const LirStore, marks: Marks, domain: ?*Domain) Allocator.Error!Scratch {
            const stmt_count = if (domain) |d| d.stmts.count() else store.cfStmtCount();
            const local_count = if (domain) |d| d.locals.count() else store.localCount();
            const jump_join = try allocator.alloc(u32, stmt_count);
            errdefer allocator.free(jump_join);
            @memset(jump_join, no_local);
            var reads_index = try RowIndex.init(allocator, local_count, domain, false);
            errdefer reads_index.deinit(allocator);
            var defs_index = try RowIndex.init(allocator, local_count, domain, false);
            errdefer defs_index.deinit(allocator);
            var preds_index = try RowIndex.init(allocator, stmt_count, domain, true);
            errdefer preds_index.deinit(allocator);
            var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, stmt_count);
            errdefer seen.deinit(allocator);
            var unresolved = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, stmt_count);
            errdefer unresolved.deinit(allocator);
            const mark_gen = try allocator.alloc(u32, if (marks == .with_marks) stmt_count else 0);
            @memset(mark_gen, 0);
            return .{
                .domain = domain,
                .jump_join = jump_join,
                .reads_index = reads_index,
                .defs_index = defs_index,
                .preds_index = preds_index,
                .seen = seen,
                .unresolved = unresolved,
                .mark_gen = mark_gen,
            };
        }

        pub fn deinit(self: *Scratch, allocator: Allocator) void {
            allocator.free(self.jump_join);
            self.reads_index.deinit(allocator);
            self.defs_index.deinit(allocator);
            self.preds_index.deinit(allocator);
            self.seen.deinit(allocator);
            self.unresolved.deinit(allocator);
            allocator.free(self.mark_gen);
        }
    };

    /// Key-indexed row placement; a key with no statements has length zero.
    pub const RowIndex = struct {
        start: []u32,
        len: []u32,
        domain: ?*const Domain,
        statement_keys: bool,

        fn keyIndex(self: *const RowIndex, raw: u32) ?u32 {
            return if (self.domain) |domain|
                if (self.statement_keys) domain.stmts.get(@enumFromInt(raw)) else domain.locals.get(@enumFromInt(raw))
            else
                raw;
        }

        pub fn init(allocator: Allocator, key_count: usize, domain: ?*const Domain, statement_keys: bool) Allocator.Error!RowIndex {
            const start = try allocator.alloc(u32, key_count);
            errdefer allocator.free(start);
            const len = try allocator.alloc(u32, key_count);
            @memset(start, 0);
            @memset(len, 0);
            return .{ .start = start, .len = len, .domain = domain, .statement_keys = statement_keys };
        }

        pub fn deinit(self: *RowIndex, allocator: Allocator) void {
            allocator.free(self.start);
            allocator.free(self.len);
        }
    };

    /// Sorted rows of statement ids per key, placed through a shared index.
    pub const Rows = struct {
        index: *RowIndex,
        /// The keys whose rows are nonempty.
        keys: []u32,
        stmts: []u32,

        pub fn row(self: *const Rows, local: LIR.LocalId) []const u32 {
            return self.rowAt(@intFromEnum(local));
        }

        pub fn rowAt(self: *const Rows, key: u32) []const u32 {
            const dense = self.index.keyIndex(key) orelse return &.{};
            if (dense >= self.index.len.len) return &.{};
            // An empty row's start is not maintained.
            const len = self.index.len[dense];
            if (len == 0) return &.{};
            return self.stmts[self.index.start[dense]..][0..len];
        }

        fn clearIndex(self: *Rows) void {
            for (self.keys) |key| self.index.len[key] = 0;
        }

        pub fn deinit(self: *Rows, allocator: Allocator) void {
            allocator.free(self.keys);
            allocator.free(self.stmts);
        }
    };

    const RowKind = enum { reads, defs };

    pub fn init(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId) Allocator.Error!UseOrder {
        return fromTopology(allocator, try initTopology(allocator, store, lists, .{ .owned = .with_marks }));
    }

    /// An order over `lists` whose store-indexed tables are the caller's
    /// reusable `scratch`, which must outlive the order.
    pub fn initInScratch(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId, scratch: *Scratch) Allocator.Error!UseOrder {
        return fromTopology(allocator, try initTopology(allocator, store, lists, .{ .scratch = scratch }));
    }

    fn fromTopology(allocator: Allocator, topology: Topology) UseOrder {
        return .{
            .allocator = allocator,
            .topology = topology,
            .mark_gen = topology.tables.mark_gen,
            .generation = 0,
            .marked_local = no_local,
            .marked_uses = false,
            .work = .empty,
        };
    }

    /// Whether a topology's tables include store-indexed marks for a whole order.
    pub const Marks = enum { with_marks, without_marks };

    /// Where a topology's store-indexed tables come from: fresh tables it
    /// owns, or a caller's reusable scratch.
    pub const Tables = union(enum) {
        owned: Marks,
        scratch: *Scratch,
        compact: *Domain,
    };

    pub fn initTopology(allocator: Allocator, store: *const LirStore, lists: []const []const LIR.CFStmtId, source: Tables) Allocator.Error!Topology {
        if (builtin.is_test) topology_builds += 1;
        const scratch: ?*Scratch = switch (source) {
            .owned, .compact => null,
            .scratch => |tables| tables,
        };
        const tables = scratch orelse owned: {
            const owned = try allocator.create(Scratch);
            errdefer allocator.destroy(owned);
            owned.* = switch (source) {
                .owned => |marks| try Scratch.initTables(allocator, store, marks),
                .compact => |domain| try Scratch.initTablesInDomain(allocator, store, .with_marks, domain),
                .scratch => unreachable,
            };
            break :owned owned;
        };
        errdefer if (scratch == null) {
            tables.deinit(allocator);
            allocator.destroy(tables);
        };

        var member_list = std.ArrayList(u32).empty;
        defer member_list.deinit(allocator);
        {
            errdefer for (member_list.items) |stmt| tables.seen.unset(tables.stmtIndex(stmt));
            for (lists) |list| {
                for (list) |stmt_id| {
                    const index = @intFromEnum(stmt_id);
                    if (tables.seen.isSet(tables.stmtIndex(index))) continue;
                    try member_list.append(allocator, index);
                    tables.seen.set(tables.stmtIndex(index));
                }
            }
        }
        for (member_list.items) |stmt| tables.seen.unset(tables.stmtIndex(stmt));
        const members = try allocator.dupe(u32, member_list.items);
        errdefer allocator.free(members);
        errdefer if (scratch != null) for (members) |stmt| {
            tables.jump_join[tables.stmtIndex(stmt)] = no_local;
        };

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
                    tables.jump_join[tables.stmtIndex(@intFromEnum(stmt_id))] = join_stmt;
                }
            }
        }

        var reads_of = try buildRows(allocator, store, members, &tables.reads_index, .reads);
        errdefer {
            reads_of.clearIndex();
            reads_of.deinit(allocator);
        }
        var defs_of = try buildRows(allocator, store, members, &tables.defs_index, .defs);
        errdefer {
            defs_of.clearIndex();
            defs_of.deinit(allocator);
        }
        var unresolved_list = std.ArrayList(u32).empty;
        errdefer unresolved_list.deinit(allocator);
        errdefer for (unresolved_list.items) |stmt| tables.unresolved.unset(tables.stmtIndex(stmt));
        var preds = try buildPreds(allocator, store, members, tables, &unresolved_list);
        errdefer {
            preds.clearIndex();
            preds.deinit(allocator);
        }
        const owned_unresolved = try allocator.dupe(u32, unresolved_list.items);
        unresolved_list.deinit(allocator);
        // Only a reusable scratch needs `seen` for a later build, or the
        // member inventory to reset its entries on release.
        if (scratch == null) {
            tables.seen.deinit(allocator);
            tables.seen = .{};
            allocator.free(members);
        }
        return .{
            .store = store,
            .tables = tables,
            .owns_tables = scratch == null,
            .members = if (scratch == null) &.{} else members,
            .reads_of = reads_of,
            .defs_of = defs_of,
            .jump_join = tables.jump_join,
            .preds = preds,
            .unresolved = tables.unresolved,
            .unresolved_list = owned_unresolved,
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

    /// Counts, places, and sorts one row per key reached from `members`.
    /// Only reached keys are written, so the index's other entries stay empty.
    fn RowBuilder(comptime Key: type, comptime keyIndex: fn (Key) u32) type {
        return struct {
            allocator: Allocator,
            index: *RowIndex,
            keys: std.ArrayList(u32) = .empty,
            stmts: []u32 = &.{},
            stmt: u32 = 0,
            failed: bool = false,

            fn count(self: *@This(), key: Key) void {
                const raw = self.index.keyIndex(keyIndex(key)).?;
                if (self.index.len[raw] == 0) {
                    self.keys.append(self.allocator, raw) catch {
                        self.failed = true;
                        return;
                    };
                }
                self.index.len[raw] += 1;
            }

            fn place(self: *@This()) Allocator.Error!void {
                if (self.failed) return error.OutOfMemory;
                var total: u32 = 0;
                for (self.keys.items) |raw| {
                    self.index.start[raw] = total;
                    total += self.index.len[raw];
                    self.index.len[raw] = 0;
                }
                self.stmts = try self.allocator.alloc(u32, total);
            }

            fn fill(self: *@This(), key: Key) void {
                const raw = self.index.keyIndex(keyIndex(key)).?;
                self.stmts[self.index.start[raw] + self.index.len[raw]] = self.stmt;
                self.index.len[raw] += 1;
            }

            fn finish(self: *@This()) Allocator.Error!Rows {
                for (self.keys.items) |raw| {
                    std.mem.sortUnstable(u32, self.stmts[self.index.start[raw]..][0..self.index.len[raw]], {}, std.sort.asc(u32));
                }
                return .{
                    .index = self.index,
                    .keys = try self.keys.toOwnedSlice(self.allocator),
                    .stmts = self.stmts,
                };
            }

            fn abandon(self: *@This()) void {
                for (self.keys.items) |raw| self.index.len[raw] = 0;
                self.keys.deinit(self.allocator);
                self.allocator.free(self.stmts);
            }
        };
    }

    fn localKey(local: LIR.LocalId) u32 {
        return @intFromEnum(local);
    }

    fn stmtKey(stmt: u32) u32 {
        return stmt;
    }

    fn buildRows(allocator: Allocator, store: *const LirStore, members: []const u32, index: *RowIndex, comptime kind: RowKind) Allocator.Error!Rows {
        const Builder = RowBuilder(LIR.LocalId, localKey);
        var builder = Builder{ .allocator = allocator, .index = index };
        errdefer builder.abandon();
        for (members) |raw| {
            const stmt = store.getCFStmt(@enumFromInt(raw));
            if (!rowsInclude(stmt, kind)) continue;
            switch (kind) {
                .reads => body_clone.forEachStmtRead(store, stmt, &builder, Builder.count),
                .defs => body_clone.forEachStmtDef(store, stmt, &builder, Builder.count),
            }
        }
        try builder.place();
        for (members) |raw| {
            const stmt = store.getCFStmt(@enumFromInt(raw));
            if (!rowsInclude(stmt, kind)) continue;
            builder.stmt = raw;
            switch (kind) {
                .reads => body_clone.forEachStmtRead(store, stmt, &builder, Builder.fill),
                .defs => body_clone.forEachStmtDef(store, stmt, &builder, Builder.fill),
            }
        }
        return try builder.finish();
    }

    /// Predecessor rows over the successor edges of every member, and the
    /// members whose successors are unknown.
    fn buildPreds(
        allocator: Allocator,
        store: *const LirStore,
        members: []const u32,
        tables: *Scratch,
        unresolved: *std.ArrayList(u32),
    ) Allocator.Error!Rows {
        const Builder = RowBuilder(u32, stmtKey);
        var builder = Builder{ .allocator = allocator, .index = &tables.preds_index };
        errdefer builder.abandon();
        for (members) |stmt| {
            if (forEachSuccessor(store, tables, stmt, &builder, Builder.count)) {
                try unresolved.append(allocator, stmt);
                tables.unresolved.set(tables.stmtIndex(stmt));
            }
        }
        try builder.place();
        for (members) |stmt| {
            builder.stmt = stmt;
            _ = forEachSuccessor(store, tables, stmt, &builder, Builder.fill);
        }
        return try builder.finish();
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
        return fromTopology(allocator, try initTopology(allocator, store, lists.items, .{ .compact = domain }));
    }

    pub fn deinit(self: *UseOrder) void {
        // A whole order's marks live in its topology's tables.
        if (self.component == null) self.topology.deinit(self.allocator) else self.allocator.free(self.mark_gen);
        self.work.deinit(self.allocator);
    }

    fn markIndex(self: *const UseOrder, stmt: u32) u32 {
        if (self.component) |component| {
            std.debug.assert(component.stmt_owner[stmt] == component.id);
            return component.stmt_to_dense[stmt];
        }
        return self.topology.tables.stmtIndex(stmt);
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

    pub fn reads(self: *const UseOrder, stmt: u32, local: LIR.LocalId) bool {
        return contains(self.topology.reads_of.row(local), stmt);
    }

    pub fn defines(self: *const UseOrder, stmt: u32, local: LIR.LocalId) bool {
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
        if (self.topology.unresolved.isSet(self.topology.tables.stmtIndex(stmt))) return true;
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
        _ = forEachSuccessor(self.topology.store, self.topology.tables, stmt, &probe, Probe.note);
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
            for (self.topology.unresolved_list) |stmt| try self.mark(stmt);
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
        const join_stmt = self.topology.jump_join[self.topology.tables.stmtIndex(from)];
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
    fn forEachSuccessor(store: *const LirStore, tables: *const Scratch, stmt: u32, ctx: anytype, comptime note: fn (@TypeOf(ctx), u32) void) bool {
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
                const join_stmt = tables.jump_join[tables.stmtIndex(stmt)];
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
    try testing.expectEqual(@as(usize, 2), compact.topology.reads_of.index.len.len);
    try testing.expectEqual(@as(usize, 2), compact.topology.preds.index.len.len);
    for ([_]LIR.CFStmtId{ alias, done }) |stmt| {
        for ([_]LIR.LocalId{ input, view }) |local| {
            try testing.expectEqual(try dense.usesAfter(@intFromEnum(stmt), local), try compact.usesAfter(@intFromEnum(stmt), local));
        }
    }
    try testing.expect(try compact.usesAfter(@intFromEnum(alias), view));
    try testing.expect(!try compact.usesAfter(@intFromEnum(alias), input));

    // Compact domains own both their remapping and topology storage. Every
    // partial construction and query must release both after allocation failure.
    const Probe = struct {
        fn run(probe_allocator: Allocator, source: *const LirStore, id: LIR.LirProcSpecId, stmt: LIR.CFStmtId, local: LIR.LocalId) Allocator.Error!void {
            var order = try UseOrder.initFromStore(probe_allocator, source, id);
            defer order.deinit();
            _ = try order.usesAfter(@intFromEnum(stmt), local);
        }
    };
    try testing.checkAllAllocationFailures(allocator, Probe.run, .{ &store, proc, alias, view });
}

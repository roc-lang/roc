//! Which locals hold data that lives in the program image.
//!
//! `static_data` and `bytes_literal` values are laid into the image with their
//! refcount word set to `REFCOUNT_STATIC_DATA` (zero), and both `increfRcPtr`
//! and `decrefRcPtr` return without touching a count when they read that
//! value. A retain or release on such a value is therefore a call, a load and
//! a compare that can only ever decide to do nothing. A word read from a
//! `.boxy` type table (`DynOp.table_entry`) is the same: a table is static
//! data, and every word it holds addresses image data too.
//!
//! This is deliberately not an ARC question. ARC still gives these values an
//! ownership unit, still places the retains and releases, and the debug
//! certifier still verifies that placement; `elide` runs after all of that and
//! drops the statements the placement made redundant. Keeping it out of the
//! ownership model is what makes it safe: no accounting changes, so no
//! placement changes.
//!
//! The elision happens here, in the LIR, and not in the backends: a backend
//! follows the `incref` and `decref` statements it is given and does not
//! reason about reference counting itself.
//!
//! A `str_literal` value is the same: one longer than a small string reads its
//! backing in the image, and a shorter one is held inline with no count at
//! all. Nearly every program holds one, so counting them means classifying the
//! locals of nearly every program. `Literals` lets a consumer choose: an
//! optimized build counts them, and the rest leave their retains and releases
//! in place and skip the analysis for a program with no other image data.
//!
//! A local qualifies only when a static-backed literal is the sole thing that
//! ever writes it. `writers` marks the writes that are not binders and
//! classifies literals; every binder comes from the shared definition walk,
//! whose exhaustive switch forces a decision for each new statement kind.

const std = @import("std");
const invariant = @import("base").invariant;
const collections = @import("collections");
const core = @import("lir_core");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;
const LocalId = LIR.LocalId;
const Body = @import("body_clone.zig");

/// Which literals count as image data.
pub const Literals = enum {
    /// `static_data` and `bytes_literal` literals, and type table reads.
    static_data,
    /// Those and `str_literal` literals.
    static_data_and_strings,
};

/// Locals whose every write is a static-backed literal.
pub const ImmortalLocals = struct {
    /// Written at least once by a literal `Literals` counts as image data.
    static_written: std.DynamicBitSetUnmanaged,
    /// Written by anything other than a static-backed literal. A local several
    /// static literals write on different paths still holds image data on every
    /// path, so only a non-static write disqualifies it.
    other_written: std.DynamicBitSetUnmanaged,

    pub const empty: ImmortalLocals = .{
        .static_written = .{},
        .other_written = .{},
    };

    pub fn deinit(self: *ImmortalLocals, allocator: Allocator) void {
        self.static_written.deinit(allocator);
        self.other_written.deinit(allocator);
        self.* = .empty;
    }

    /// Whether reference-count traffic on `local` can only ever be a no-op.
    pub fn contains(self: *const ImmortalLocals, local: LocalId) bool {
        const index = @backingInt(local);
        if (index >= self.static_written.bit_length) return false;
        return self.static_written.isSet(index) and !self.other_written.isSet(index);
    }
};

/// Classify every local the store's statements write.
pub fn compute(allocator: Allocator, store: *const LirStore, literals: Literals) Allocator.Error!ImmortalLocals {
    const count = store.localCount();
    var result: ImmortalLocals = .{
        .static_written = try std.DynamicBitSetUnmanaged.initEmpty(allocator, count),
        .other_written = try std.DynamicBitSetUnmanaged.initEmpty(allocator, count),
    };
    errdefer result.deinit(allocator);

    var pass: Pass = .{ .store = store, .result = &result, .literals = literals };
    for (0..store.cfStmtCount()) |index| pass.writers(@fromBackingInt(@intCast(@as(u32, @intCast(index)))));
    return result;
}

const Pass = struct {
    store: *const LirStore,
    result: *ImmortalLocals,
    literals: Literals,

    fn markStatic(self: *Pass, local: LocalId) void {
        const index = @backingInt(local);
        if (index >= self.result.static_written.bit_length) return;
        self.result.static_written.set(index);
    }

    fn markOther(self: *Pass, local: LocalId) void {
        const index = @backingInt(local);
        if (index >= self.result.other_written.bit_length) return;
        self.result.other_written.set(index);
    }

    /// Record every local this statement writes: a static-backed literal as a
    /// static write, everything else as another write. The shared binder
    /// walk covers every statement that binds a local; the arms here are the
    /// writes it does not count as binders, and the literal classification.
    fn writers(self: *Pass, stmt_id: LIR.CFStmtId) void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_literal => |s| switch (s.value) {
                // Image data, refcount word zero.
                .static_data, .bytes_literal => self.markStatic(s.target),
                // Image data, or held inline with no count.
                .str_literal => switch (self.literals) {
                    .static_data => self.markOther(s.target),
                    .static_data_and_strings => self.markStatic(s.target),
                },
                .i64_literal,
                .i128_literal,
                .f64_literal,
                .f32_literal,
                .dec_literal,
                .proc_ref,
                => self.markOther(s.target),
            },
            // A type table word is image data, as a static-data literal is.
            .dyn => |s| if (self.store.stmtRow(s.data) == .table_entry)
                self.markStatic(s.target.?)
            else
                Body.visitStmtDefinitions(self.store, self, stmt_id),
            .set_local => |s| self.markOther(s.target),
            inline .store_struct, .store_tag => |s| self.markOther(s.dest),
            .init_uninitialized,
            .assign_ref,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .join,
            .jump,
            .ret,
            .crash,
            => Body.visitStmtDefinitions(self.store, self, stmt_id),
        }
    }

    /// `visitStmtDefinitions` marker: every binder is another write.
    pub fn noteDefinition(self: *Pass, local: LocalId) void {
        self.markOther(local);
    }
};

/// Drops every `incref` and `decref` on a local that only image data writes.
///
/// Runs after ARC has placed reference counts and the debug certifier has
/// checked that placement, so the ledger it verified is the one ARC produced;
/// what is removed here is only the runtime no-ops that placement implies.
/// A `free` is left alone: releasing image data would be a placement bug
/// rather than a no-op worth eliding, and removing it would hide that.
///
/// Returns how many statements were dropped.
pub fn elide(gpa: Allocator, store: *LirStore, literals: Literals) Allocator.Error!usize {
    if (!anyStaticLiteral(store, literals)) {
        if (@import("builtin").mode == .debug) try verifyNothingToElide(gpa, store, literals);
        return 0;
    }
    var immortal = try compute(gpa, store, literals);
    defer immortal.deinit(gpa);

    const count = store.cfStmtCount();
    // `next` of each dropped statement, or itself when it is kept.
    const successor = try gpa.alloc(LIR.CFStmtId, count);
    defer gpa.free(successor);
    const dropped = try gpa.alloc(bool, count);
    defer gpa.free(dropped);
    @memset(dropped, false);

    var drop_count: usize = 0;
    for (store.getCFStmts(), 0..) |stmt, index| {
        const id: LIR.CFStmtId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
        successor[index] = id;
        const value, const next = referenceCount(stmt) orelse continue;
        if (!immortal.contains(value)) continue;
        dropped[index] = true;
        successor[index] = next;
        drop_count += 1;
    }
    if (drop_count == 0) return 0;

    // Collapse runs of dropped statements so every edge lands on a kept one.
    // Following `next` terminates because it only ever moves forward through
    // the run and the last statement of a run is kept.
    for (0..count) |index| {
        if (!dropped[index]) continue;
        var target = successor[index];
        var guard: usize = 0;
        while (dropped[@backingInt(target)]) {
            target = successor[@backingInt(target)];
            guard += 1;
            if (guard > count) immortalInvariant("dropped reference-count statements form a cycle");
        }
        successor[index] = target;
    }

    const resolve = struct {
        fn call(table: []const LIR.CFStmtId, id: LIR.CFStmtId) LIR.CFStmtId {
            return table[@backingInt(id)];
        }
    }.call;

    for (0..count) |index| {
        Body.redirectSuccessors(store, @fromBackingInt(@intCast(@as(u32, @intCast(index)))), successor, resolve);
    }

    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @fromBackingInt(@intCast(@as(u32, @intCast(proc_index))));
        const proc = store.getProcSpecPtr(proc_id);
        if (proc.body.get()) |body| proc.body = .some(resolve(successor, body));
        const joins = store.getJoinPointSpanMut(proc.join_points);
        for (0..joins.len) |join_index| {
            const join = GuardedList.atPtr(joins, join_index);
            join.body = resolve(successor, join.body);
        }
    }

    return drop_count;
}

/// Whether any procedure's shapes record a literal `literals` counts as image
/// data; without one no local can be immortal.
fn anyStaticLiteral(store: *const LirStore, literals: Literals) bool {
    for (0..store.procSpecCount()) |index| {
        const shapes = store.getProcSpecImmediate(@fromBackingInt(@intCast(@as(u32, @intCast(index))))).shapes;
        if (shapes.static_literal) return true;
        if (literals == .static_data_and_strings and shapes.str_literal) return true;
    }
    return false;
}

/// The local a reference-count statement retains or releases, and the
/// statement after it; null for every other statement.
fn referenceCount(stmt: LIR.CFStmt) ?struct { LocalId, LIR.CFStmtId } {
    return switch (stmt) {
        inline .incref, .decref, .decref_if_initialized => |s| .{ s.value, s.next },
        .init_uninitialized,
        .str_match,
        .str_match_set,
        .switch_stmt,
        .switch_initialized_payload,
        .join,
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
        .dyn,
        .debug,
        .expect,
        .free,
        .jump,
        .ret,
        .crash,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        => null,
    };
}

/// A store whose procedures record no static-backed literal must give elision
/// nothing to drop. Statements no procedure reaches are left behind by earlier
/// rewrites and are not part of the program, so only procedure bodies count.
fn verifyNothingToElide(gpa: Allocator, store: *LirStore, literals: Literals) Allocator.Error!void {
    var immortal = try compute(gpa, store, literals);
    defer immortal.deinit(gpa);
    for (0..store.procSpecCount()) |index| {
        const body = store.getProcSpec(@fromBackingInt(@intCast(@as(u32, @intCast(index))))).body.get() orelse continue;
        var walk = try Body.ReachableStmts.initWithAllocator(store, body, gpa);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            const value, _ = referenceCount(store.getCFStmt(stmt_id)) orelse continue;
            if (immortal.contains(value)) immortalInvariant("a program without static-literal shapes counted references on an immortal local");
        }
    }
}

fn immortalInvariant(comptime message: []const u8) noreturn {
    invariant("{s}", .{"immortal locals invariant violated: " ++ message});
}

test {
    std.testing.refAllDecls(@This());
}

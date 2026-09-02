//! Which locals hold data that lives in the program image.
//!
//! `static_data` and `bytes_literal` values are laid into the image with their
//! refcount word set to `REFCOUNT_STATIC_DATA` (zero), and both `increfRcPtr`
//! and `decrefRcPtr` return without touching a count when they read that
//! value. A retain or release on such a value is therefore a call, a load and
//! a compare that can only ever decide to do nothing.
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
//! `str_literal` is not included and must not be. A string literal longer than
//! a small string is materialized by `str_from_literal`, which heap-allocates
//! and writes a refcount of one, so its retains and releases are load-bearing.
//!
//! A local qualifies only when a static-backed literal is the sole thing that
//! ever writes it. `writers` classifies every statement kind exhaustively so
//! that adding a kind forces a decision here rather than silently widening the
//! set.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;
const LocalId = LIR.LocalId;

/// Locals whose every write is a static-backed literal.
pub const ImmortalLocals = struct {
    /// Written at least once by a `static_data` or `bytes_literal` literal.
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
        const index = @intFromEnum(local);
        if (index >= self.static_written.bit_length) return false;
        return self.static_written.isSet(index) and !self.other_written.isSet(index);
    }
};

/// Classify every local the store's statements write.
pub fn compute(allocator: Allocator, store: *const LirStore) Allocator.Error!ImmortalLocals {
    const count = store.localCount();
    var result: ImmortalLocals = .{
        .static_written = try std.DynamicBitSetUnmanaged.initEmpty(allocator, count),
        .other_written = try std.DynamicBitSetUnmanaged.initEmpty(allocator, count),
    };
    errdefer result.deinit(allocator);

    var pass: Pass = .{ .store = store, .result = &result };
    for (store.getCFStmts()) |stmt| pass.writers(stmt);
    return result;
}

const Pass = struct {
    store: *const LirStore,
    result: *ImmortalLocals,

    fn markStatic(self: *Pass, local: LocalId) void {
        const index = @intFromEnum(local);
        if (index >= self.result.static_written.bit_length) return;
        self.result.static_written.set(index);
    }

    fn markOther(self: *Pass, local: LocalId) void {
        const index = @intFromEnum(local);
        if (index >= self.result.other_written.bit_length) return;
        self.result.other_written.set(index);
    }

    fn markOtherOpt(self: *Pass, local: ?LocalId) void {
        if (local) |l| self.markOther(l);
    }

    fn markOtherSpan(self: *Pass, span: LIR.LocalSpan) void {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            self.markOther(GuardedList.at(locals, index));
        }
    }

    fn markOtherSteps(self: *Pass, span: LIR.StrMatchStepSpan) void {
        const steps = self.store.getStrMatchSteps(span);
        for (0..GuardedList.borrowLen(steps)) |index| {
            switch (GuardedList.at(steps, index).capture) {
                .discard => {},
                .view => |local| self.markOther(local),
            }
        }
    }

    /// Record every local this statement writes. Exhaustive by design: a new
    /// statement kind must be classified here before the analysis compiles.
    fn writers(self: *Pass, stmt: LIR.CFStmt) void {
        switch (stmt) {
            .assign_literal => |s| switch (s.value) {
                // Image data, refcount word zero.
                .static_data, .bytes_literal => self.markStatic(s.target),
                // A string literal longer than a small string is materialized
                // by `str_from_literal`, which heap-allocates and writes a
                // refcount of one, so its retains and releases are real.
                .str_literal,
                .i64_literal,
                .i128_literal,
                .f64_literal,
                .f32_literal,
                .dec_literal,
                .boxy_dynamic_num_literal,
                .boxy_dynamic_frac_literal,
                .null_ptr,
                .proc_ref,
                => self.markOther(s.target),
            },

            inline .init_uninitialized,
            .assign_ref,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .set_local,
            => |s| self.markOther(s.target),

            .assign_call => |s| {
                self.markOther(s.target);
                self.markOtherOpt(s.out_desc);
            },
            .assign_call_erased => |s| {
                self.markOther(s.target);
                self.markOtherOpt(s.out_desc);
            },
            .assign_call_dict => |s| self.markOther(s.target),
            .assign_packed_erased_fn => |s| self.markOther(s.target),
            .assign_boxy_tag_payload => |s| {
                self.markOther(s.target);
                self.markOtherOpt(s.target_desc);
            },

            .store_struct => |s| self.markOther(s.dest),
            .store_tag => |s| self.markOther(s.dest),

            .join => |s| {
                self.markOtherSpan(s.params);
                self.markOtherSpan(s.maybe_uninitialized_params);
            },

            .str_match => |s| self.markOtherSteps(s.steps),
            .str_match_set => |s| {
                const arms = self.store.getStrMatchArms(s.arms);
                for (0..GuardedList.borrowLen(arms)) |index| {
                    self.markOtherSteps(GuardedList.at(arms, index).steps);
                }
            },

            // Write no local.
            .boxy_tag_match,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => {},
        }
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
pub fn elide(gpa: Allocator, store: *LirStore) Allocator.Error!usize {
    var immortal = try compute(gpa, store);
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
        const id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(index)));
        successor[index] = id;
        const value: LIR.LocalId, const next: LIR.CFStmtId = switch (stmt) {
            .incref => |s| .{ s.value, s.next },
            .decref => |s| .{ s.value, s.next },
            .decref_if_initialized => |s| .{ s.value, s.next },
            .init_uninitialized,
            .boxy_tag_match,
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
            .free,
            .jump,
            .ret,
            .crash,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            => continue,
        };
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
        while (dropped[@intFromEnum(target)]) {
            target = successor[@intFromEnum(target)];
            guard += 1;
            if (guard > count) immortalInvariant("dropped reference-count statements form a cycle");
        }
        successor[index] = target;
    }

    const resolve = struct {
        fn call(table: []const LIR.CFStmtId, id: LIR.CFStmtId) LIR.CFStmtId {
            return table[@intFromEnum(id)];
        }
    }.call;

    for (0..count) |index| {
        const id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(index)));
        const stmt = store.getCFStmtPtr(id);
        switch (stmt.*) {
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
            => |*s| s.next = resolve(successor, s.next),
            .boxy_tag_match => |*s| {
                s.on_match = resolve(successor, s.on_match);
                s.on_miss = resolve(successor, s.on_miss);
            },
            .str_match => |*s| {
                s.on_match = resolve(successor, s.on_match);
                s.on_miss = resolve(successor, s.on_miss);
            },
            .str_match_set => |*s| {
                s.on_miss = resolve(successor, s.on_miss);
                const arms = store.getStrMatchArmsMut(s.arms);
                for (0..arms.len) |arm_index| {
                    const arm = GuardedList.atPtr(arms, arm_index);
                    arm.on_match = resolve(successor, arm.on_match);
                }
            },
            .switch_stmt => |*s| {
                s.default_branch = resolve(successor, s.default_branch);
                if (s.continuation) |continuation| s.continuation = resolve(successor, continuation);
                const branches = store.getCFSwitchBranchesMut(s.branches);
                for (0..branches.len) |branch_index| {
                    const branch = GuardedList.atPtr(branches, branch_index);
                    branch.body = resolve(successor, branch.body);
                }
            },
            .switch_initialized_payload => |*s| {
                s.initialized_branch = resolve(successor, s.initialized_branch);
                s.uninitialized_branch = resolve(successor, s.uninitialized_branch);
            },
            .join => |*s| {
                s.body = resolve(successor, s.body);
                s.remainder = resolve(successor, s.remainder);
            },
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

    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpecPtr(proc_id);
        if (proc.body) |body| proc.body = resolve(successor, body);
        const joins = store.getJoinPointSpanMut(proc.join_points);
        for (0..joins.len) |join_index| {
            const join = GuardedList.atPtr(joins, join_index);
            join.body = resolve(successor, join.body);
        }
    }

    return drop_count;
}

fn immortalInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        @panic("immortal locals invariant violated: " ++ message);
    }
    unreachable;
}

test {
    std.testing.refAllDecls(@This());
}

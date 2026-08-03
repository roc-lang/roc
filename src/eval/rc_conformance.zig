//! Debug-only conformance check between `LowLevel.rcEffect()` and what the
//! builtins actually do to refcounts.
//!
//! `base/rc_effect_rules.zig` rejects rows that contradict themselves. A row
//! can still be structurally valid and simply false — the case that shipped as
//! roc-lang/roc#10023, where a family of ops claimed a unique result while
//! returning a slice of an argument's allocation, and every call leaked one
//! reference. Nothing but running the builtin can tell those rows apart.
//!
//! So the interpreter reports, for every low-level statement it executes, what
//! the op did to the refcounts it could reach: which allocations were born,
//! which counts moved, and which of the result's allocations came from an
//! argument. This module judges that report against the row.
//!
//! ## Direction of the check
//!
//! Every rule reads "observed behavior the row does not account for", never
//! "declared behavior that did not happen on this path". A row is a claim
//! about what the op *may* do, and a single execution takes one path: a
//! copy-on-write op given a unique input never exercises its copy path. So an
//! unexercised claim is not a finding, while an unclaimed effect always is —
//! it is ownership traffic ARC did not plan for.
//!
//! The two effects that must therefore be driven in both regimes — unique
//! input and shared input — are `result_unique` and copy-on-write. The sweep
//! in `test/rc_conformance_tests.zig` is what supplies both.
//!
//! ## Who accounts for a count
//!
//! The rules below distinguish counts the *op* adds from counts *ARC* adds,
//! because the row decides which is which:
//!
//! - `retain_args` means ARC emits the retain (`lir/arc.zig`
//!   `retainMaskedArgs`), so the builtin must not also count the argument.
//! - `result_shares_args` without `retain_args` means the opposite: ARC emits
//!   nothing, so the builtin must count the argument itself for the handle it
//!   stores in the result.
//!
//! An op that counts an argument ARC already counted leaks; an op that counts
//! neither leaves a dangling handle.

const std = @import("std");
const builtin = @import("builtin");

const base = @import("base");
const builtins = @import("builtins");

const LowLevel = base.LowLevel;
const RcEffect = LowLevel.RcEffect;
const rc_effect_rules = base.rc_effect_rules;
const DebugRefcountTracker = builtins.utils.DebugRefcountTracker;

/// Observation is debug-only: release builds carry no observer, no branch on
/// one, and no hook in the interpreter's statement loop. Freestanding targets
/// have no refcount event log to read, so they stay out too.
pub const enabled = builtin.mode == .Debug and builtin.target.os.tag != .freestanding;

/// The most arguments one low-level statement is observed with. Every row in
/// the table names positions 0-2; positions past this are still checked for
/// existence by `mask_names_missing_argument`.
pub const max_observed_args = 8;

/// The most allocations collected from one value's refcounted interior.
/// Overflow only weakens the alias rules — they fire on allocations that were
/// found, never on ones that were not.
pub const max_collected_allocations = 96;

const max_findings = 32;

/// One allocation, identified by the address of its refcount.
pub const Allocation = struct {
    rc_addr: usize,
    count: isize,

    /// Static data and other constant-count allocations cannot be observed:
    /// incref and decref both leave them alone.
    pub fn isObservable(self: Allocation) bool {
        return !builtins.utils.rcConstant(self.count);
    }
};

/// The refcounted allocations reachable from one value.
pub const AllocationSet = struct {
    addrs: [max_collected_allocations]usize = undefined,
    len: usize = 0,
    overflowed: bool = false,

    pub fn add(self: *AllocationSet, rc_addr: usize) void {
        for (self.addrs[0..self.len]) |existing| {
            if (existing == rc_addr) return;
        }
        if (self.len == self.addrs.len) {
            self.overflowed = true;
            return;
        }
        self.addrs[self.len] = rc_addr;
        self.len += 1;
    }

    pub fn contains(self: AllocationSet, rc_addr: usize) bool {
        for (self.addrs[0..self.len]) |existing| {
            if (existing == rc_addr) return true;
        }
        return false;
    }
};

/// What one argument looked like before the op, and what the op did to it.
pub const ArgObservation = struct {
    /// The argument's own outermost allocation, before the op ran.
    outer: ?Allocation = null,
    /// That allocation's count after the op, or null when the op freed or
    /// moved it.
    count_after: ?isize = null,
};

/// One executed low-level statement, as seen from outside the op.
pub const Observation = struct {
    op: LowLevel,
    arg_count: usize,
    args: [max_observed_args]ArgObservation = [_]ArgObservation{.{}} ** max_observed_args,
    /// The result's outermost allocation, after the op ran.
    result_outer: ?Allocation = null,
    /// Everything refcounted the result can reach, after the op ran.
    result_reachable: AllocationSet = .{},
    /// The op called `allocateWithRefcount` at least once.
    allocated: bool = false,
    /// The op incremented or decremented at least one count.
    adjusted_counts: bool = false,
    /// The refcount event log dropped entries, so `allocated` and
    /// `adjusted_counts` may understate what happened.
    events_incomplete: bool = false,
};

/// A row that does not match its builtin.
pub const Rule = enum {
    allocated_without_may_allocate,
    adjusted_counts_without_may_retain_or_release,
    argument_counted_by_op_and_arc,
    argument_counted_without_claim,
    argument_released_without_consume,
    shared_argument_not_counted_by_op,
    result_outlives_uniqueness_claim,
    result_holds_unnamed_argument_allocation,
    mask_names_missing_argument,

    pub fn description(self: Rule) []const u8 {
        return switch (self) {
            .allocated_without_may_allocate => "the op allocated, but the row does not set may_allocate",
            .adjusted_counts_without_may_retain_or_release => "the op changed a refcount, but the row does not set may_retain_or_release",
            .argument_counted_by_op_and_arc => "the op counted an argument that retain_args already makes ARC count; the handle ends up one count too high",
            .argument_counted_without_claim => "the op counted an argument that neither retain_args nor result_shares_args names",
            .argument_released_without_consume => "the op released an argument the row does not name in consume_args; ARC releases it again",
            .shared_argument_not_counted_by_op => "the result holds this argument's allocation under result_shares_args, which ARC does not count, but the op did not count it either",
            .result_outlives_uniqueness_claim => "result_unique claims the result's outermost allocation has count 1, and it does not",
            .result_holds_unnamed_argument_allocation => "the result holds an allocation reachable from an argument the row does not link it to",
            .mask_names_missing_argument => "a mask names an argument position the op was not given",
        };
    }
};

/// A rule broken by one executed statement, with the numbers behind it.
pub const Finding = struct {
    op: LowLevel,
    rule: Rule,
    /// The argument position involved, when the rule is about one.
    arg: ?usize = null,
    /// Count before the op, for the allocation the rule is about.
    count_before: isize = 0,
    /// Count after the op, or null when the allocation was freed.
    count_after: ?isize = null,
    /// Whether this rule is about one allocation's count at all.
    counts_apply: bool = false,

    pub fn format(self: Finding, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("{s}: {s}", .{ @tagName(self.op), self.rule.description() });
        if (self.arg) |position| try writer.print(" (argument {d})", .{position});
        try writer.print(" [rule {s}", .{@tagName(self.rule)});
        if (self.counts_apply) {
            if (self.count_after) |after| {
                try writer.print(", count {d} -> {d}", .{ self.count_before, after });
            } else {
                try writer.print(", count {d} -> freed", .{self.count_before});
            }
        }
        try writer.print("]", .{});
    }
};

/// The set of ops the sweep has executed. `std.EnumSet` over `LowLevel` is one
/// bit per op.
pub const OpSet = std.EnumSet(LowLevel);

var active_flag: bool = false;
var findings_buf: [max_findings]Finding = undefined;
var findings_len: usize = 0;
var dropped_findings: usize = 0;
var covered_ops: OpSet = OpSet.initEmpty();
var row_overrides: std.EnumMap(LowLevel, RcEffect) = .{};
/// One observation at a time, reused by every statement. The interpreter is
/// single-threaded and low-level ops do not execute other low-level
/// statements, but `open_statements` keeps a surprise reentry from judging a
/// half-filled observation.
var scratch_observation: Observation = .{ .op = .crash, .arg_count = 0 };
var open_statements: usize = 0;

/// Start observing. Resets findings and coverage, and turns on the refcount
/// event log the observation reads.
pub fn begin() void {
    if (!enabled) return;
    findings_len = 0;
    dropped_findings = 0;
    covered_ops = OpSet.initEmpty();
    open_statements = 0;
    active_flag = true;
    DebugRefcountTracker.enable();
    // Only the operation log is read here — which allocations were born, freed,
    // or moved during one op — never the tracker's derived shadow counts.
    DebugRefcountTracker.setShadowDiagnostics(false);
}

/// Open an observation for one low-level statement, or null when nothing is
/// watching. The caller fills in the arguments, runs the op, fills in the
/// result, and closes it with `endStatement`.
pub fn beginStatement(op: LowLevel, arg_count: usize) ?*Observation {
    if (!enabled) return null;
    if (!active_flag) return null;
    if (open_statements != 0) return null;

    open_statements += 1;
    scratch_observation = .{ .op = op, .arg_count = arg_count };
    beginEventWindow();
    return &scratch_observation;
}

/// Judge a filled-in observation and close it.
pub fn endStatement(observation: *Observation) void {
    if (!enabled) return;
    open_statements -= 1;
    record(observation.*);
}

/// Stop observing, keeping findings and coverage for the caller to read.
pub fn end() void {
    if (!enabled) return;
    active_flag = false;
    DebugRefcountTracker.disable();
}

/// Whether observation is on right now.
pub fn isActive() bool {
    return enabled and active_flag;
}

/// Rows that did not match their builtin during this run, oldest first.
pub fn findings() []const Finding {
    return findings_buf[0..findings_len];
}

/// Findings beyond the fixed buffer. A run that drops findings still fails;
/// this only says the printed list is partial.
pub fn droppedFindings() usize {
    return dropped_findings;
}

/// The ops this run executed, whatever their row said.
pub fn covered() OpSet {
    return covered_ops;
}

/// Judge `op` against `effect` instead of against its own row, so a test can
/// point the harness at a row that is known to be wrong and watch it fail.
pub fn overrideRow(op: LowLevel, effect: RcEffect) void {
    if (!enabled) return;
    row_overrides.put(op, effect);
}

/// Judge every op against its own row again.
pub fn clearOverrides() void {
    if (!enabled) return;
    row_overrides = .{};
}

/// The row this op is judged against.
pub fn rowFor(op: LowLevel) RcEffect {
    if (enabled) {
        if (row_overrides.get(op)) |override| return override;
    }
    return op.rcEffect();
}

/// The refcount event log, cleared so the next op's events stand alone.
pub fn beginEventWindow() void {
    DebugRefcountTracker.clearLog();
}

/// What the refcount event log recorded since `beginEventWindow`.
pub const EventWindow = struct {
    allocated: bool = false,
    adjusted_counts: bool = false,
    incomplete: bool = false,
};

/// Whether the op that just ran left this refcount address unreadable, by
/// freeing the allocation or by moving it. Reading either one is a
/// use-after-free, so every count read after an op asks this first.
pub fn wasFreedInWindow(rc_addr: usize) bool {
    var gone = false;
    for (DebugRefcountTracker.recordedOps()) |event| {
        if (event.rc_addr != rc_addr) continue;
        switch (event.kind) {
            .free, .realloc => gone = true,
            // An address handed back out by a later allocation is live again.
            .alloc => gone = false,
            else => {},
        }
    }
    return gone;
}

/// Summarize the refcount events recorded for the op that just ran.
pub fn endEventWindow() EventWindow {
    var window = EventWindow{ .incomplete = DebugRefcountTracker.logOverflowed() or DebugRefcountTracker.isSaturated() };
    for (DebugRefcountTracker.recordedOps()) |event| {
        switch (event.kind) {
            .alloc, .realloc => window.allocated = true,
            .incref, .decref, .free => window.adjusted_counts = true,
        }
    }
    return window;
}

/// Read an allocation's current count. Returns null for the null pointer.
pub fn allocationAt(data_ptr: ?[*]u8) ?Allocation {
    const ptr = data_ptr orelse return null;
    const rc_addr = @intFromPtr(ptr) - @sizeOf(usize);
    if (rc_addr == 0) return null;
    const rc_ptr: *const isize = @ptrFromInt(rc_addr);
    return .{ .rc_addr = rc_addr, .count = rc_ptr.* };
}

/// Judge one executed statement and remember that its op has coverage.
pub fn record(observation: Observation) void {
    if (!enabled) return;
    covered_ops.insert(observation.op);

    const effect = rowFor(observation.op);
    if (rc_effect_rules.maskExceedsArgCount(effect, observation.arg_count)) |position| {
        addFinding(.{
            .op = observation.op,
            .rule = .mask_names_missing_argument,
            .arg = position,
        });
    }

    if (!observation.events_incomplete) {
        if (observation.allocated and !effect.may_allocate) {
            addFinding(.{ .op = observation.op, .rule = .allocated_without_may_allocate });
        }
        if (observation.adjusted_counts and !effect.may_retain_or_release) {
            addFinding(.{ .op = observation.op, .rule = .adjusted_counts_without_may_retain_or_release });
        }
    }

    const positions = @min(observation.arg_count, max_observed_args);
    for (observation.args[0..positions], 0..) |arg, position| {
        if (position >= 64) break;
        const bit = @as(u64, 1) << @intCast(position);
        const outer = arg.outer orelse continue;
        if (!outer.isObservable()) continue;

        const after = arg.count_after;
        const released = after == null or after.? < outer.count;
        const counted = after != null and after.? > outer.count;

        if (counted) {
            if ((effect.retain_args & bit) != 0) {
                addFinding(.{
                    .op = observation.op,
                    .rule = .argument_counted_by_op_and_arc,
                    .arg = position,
                    .count_before = outer.count,
                    .count_after = after,
                    .counts_apply = true,
                });
            } else if ((effect.result_shares_args & bit) == 0) {
                addFinding(.{
                    .op = observation.op,
                    .rule = .argument_counted_without_claim,
                    .arg = position,
                    .count_before = outer.count,
                    .count_after = after,
                    .counts_apply = true,
                });
            }
        }

        if (released and (effect.consume_args & bit) == 0) {
            addFinding(.{
                .op = observation.op,
                .rule = .argument_released_without_consume,
                .arg = position,
                .count_before = outer.count,
                .count_after = after,
                .counts_apply = true,
            });
        }

        const result_holds_it = observation.result_reachable.contains(outer.rc_addr);
        if (result_holds_it and
            (effect.result_shares_args & bit) != 0 and
            (effect.retain_args & bit) == 0 and
            !counted)
        {
            addFinding(.{
                .op = observation.op,
                .rule = .shared_argument_not_counted_by_op,
                .arg = position,
                .count_before = outer.count,
                .count_after = after,
                .counts_apply = true,
            });
        }
    }

    checkResultLinks(observation, effect, positions);

    if (effect.result_unique) {
        if (observation.result_outer) |result| {
            if (result.isObservable() and result.count != 1) {
                addFinding(.{
                    .op = observation.op,
                    .rule = .result_outlives_uniqueness_claim,
                    .count_before = result.count,
                    .count_after = result.count,
                    .counts_apply = true,
                });
            }
        }
    }
}

/// An argument's own allocation showing up anywhere inside the result — as the
/// result itself, or as one of its interior handles — has to be a link the row
/// names, because that is the link ARC follows to keep the lender alive.
///
/// Interior allocations *of* an argument (a list's elements, a box's payload)
/// are deliberately not checked: an op like `list_map_extract_unsafe` moves one
/// element's ownership out of a buffer, and no flag describes that move.
fn checkResultLinks(observation: Observation, effect: RcEffect, positions: usize) void {
    const linked = effect.result_shares_args |
        effect.result_borrows_args |
        effect.result_aliases_consumed_args |
        effect.retain_args;

    for (observation.args[0..positions], 0..) |arg, position| {
        if (position >= 64) break;
        const bit = @as(u64, 1) << @intCast(position);
        if ((linked & bit) != 0) continue;
        const outer = arg.outer orelse continue;
        if (!outer.isObservable()) continue;
        if (!observation.result_reachable.contains(outer.rc_addr)) continue;
        // Two arguments can be the same value; a link through any of them
        // accounts for the allocation.
        if (aliasOfLinkedArg(observation, linked, positions, outer.rc_addr)) continue;
        addFinding(.{
            .op = observation.op,
            .rule = .result_holds_unnamed_argument_allocation,
            .arg = position,
        });
    }
}

fn aliasOfLinkedArg(observation: Observation, linked: u64, positions: usize, rc_addr: usize) bool {
    for (observation.args[0..positions], 0..) |arg, position| {
        if (position >= 64) break;
        const bit = @as(u64, 1) << @intCast(position);
        if ((linked & bit) == 0) continue;
        const outer = arg.outer orelse continue;
        if (outer.rc_addr == rc_addr) return true;
    }
    return false;
}

fn addFinding(finding: Finding) void {
    if (findings_len == findings_buf.len) {
        dropped_findings += 1;
        return;
    }
    findings_buf[findings_len] = finding;
    findings_len += 1;
}

/// Ops with a nontrivial row that the sweep never executed.
///
/// A new builtin ships with a row; if no case drives it, the row is
/// unverified. Exempt ops are ones no interpreted program can reach, and they
/// must be listed with a reason at the exemption table.
pub fn coverageGaps(observed: OpSet, exempt: OpSet, gaps: *OpSet) void {
    gaps.* = OpSet.initEmpty();
    for (std.enums.values(LowLevel)) |op| {
        if (std.meta.eql(op.rcEffect(), RcEffect.none())) continue;
        if (observed.contains(op)) continue;
        if (exempt.contains(op)) continue;
        gaps.insert(op);
    }
}

/// Exemptions the sweep turned out to cover after all. A stale exemption hides
/// the op from the coverage requirement for no reason.
pub fn staleExemptions(observed: OpSet, exempt: OpSet, stale: *OpSet) void {
    stale.* = OpSet.initEmpty();
    var it = exempt.iterator();
    while (it.next()) |op| {
        if (observed.contains(op)) stale.insert(op);
    }
}

test "coverage gaps name every uncovered nontrivial op" {
    // Stand in for a newly added builtin: everything is covered except one op
    // with a nontrivial row.
    var observed = OpSet.initEmpty();
    for (std.enums.values(LowLevel)) |op| observed.insert(op);
    observed.remove(.str_concat);

    var gaps = OpSet.initEmpty();
    coverageGaps(observed, OpSet.initEmpty(), &gaps);

    try std.testing.expectEqual(@as(usize, 1), gaps.count());
    try std.testing.expect(gaps.contains(.str_concat));
}

test "an op with a trivial row needs no coverage" {
    var gaps = OpSet.initEmpty();
    coverageGaps(OpSet.initEmpty(), OpSet.initEmpty(), &gaps);

    try std.testing.expect(!gaps.contains(.num_plus));
    try std.testing.expect(gaps.contains(.str_concat));
}

test "an exemption suppresses a gap, and coverage makes it stale" {
    var exempt = OpSet.initEmpty();
    exempt.insert(.str_concat);

    var gaps = OpSet.initEmpty();
    coverageGaps(OpSet.initEmpty(), exempt, &gaps);
    try std.testing.expect(!gaps.contains(.str_concat));

    var observed = OpSet.initEmpty();
    observed.insert(.str_concat);
    var stale = OpSet.initEmpty();
    staleExemptions(observed, exempt, &stale);
    try std.testing.expect(stale.contains(.str_concat));
}

test "a uniqueness claim on a shared result is a finding" {
    if (!enabled) return;

    // The #10023 shape: the op returns a slice of its argument's allocation,
    // whose count is 3 because the string is also held elsewhere.
    var observation = Observation{ .op = .str_drop_prefix, .arg_count = 2 };
    observation.args[0] = .{
        .outer = .{ .rc_addr = 0x1000, .count = 3 },
        .count_after = 3,
    };
    observation.result_outer = .{ .rc_addr = 0x1000, .count = 3 };
    observation.result_reachable.add(0x1000);

    begin();
    defer end();

    // The row as it stands accounts for this: shared interior, no birth.
    record(observation);
    try std.testing.expectEqual(@as(usize, 0), findings().len);

    // The row as #10023 had it does not.
    var reintroduced = RcEffect.retainsSharingArgs(1);
    reintroduced.result_unique = true;
    overrideRow(.str_drop_prefix, reintroduced);
    defer clearOverrides();

    record(observation);
    try std.testing.expectEqual(@as(usize, 1), findings().len);
    try std.testing.expectEqual(Rule.result_outlives_uniqueness_claim, findings()[0].rule);
    try std.testing.expectEqual(LowLevel.str_drop_prefix, findings()[0].op);
}

test "a count the op adds on top of ARC's retain is a finding" {
    if (!enabled) return;

    var observation = Observation{ .op = .str_drop_prefix, .arg_count = 2 };
    observation.args[0] = .{
        .outer = .{ .rc_addr = 0x2000, .count = 1 },
        .count_after = 2,
    };

    begin();
    defer end();
    record(observation);

    try std.testing.expectEqual(@as(usize, 1), findings().len);
    try std.testing.expectEqual(Rule.argument_counted_by_op_and_arc, findings()[0].rule);
    try std.testing.expectEqual(@as(?usize, 0), findings()[0].arg);
}

test "releasing an argument the row does not consume is a finding" {
    if (!enabled) return;

    var observation = Observation{ .op = .str_drop_prefix, .arg_count = 2 };
    observation.args[0] = .{
        .outer = .{ .rc_addr = 0x3000, .count = 2 },
        .count_after = 1,
    };

    begin();
    defer end();
    record(observation);

    try std.testing.expectEqual(@as(usize, 1), findings().len);
    try std.testing.expectEqual(Rule.argument_released_without_consume, findings()[0].rule);
}

test "static data is not observable" {
    if (!enabled) return;

    var observation = Observation{ .op = .str_drop_prefix, .arg_count = 2 };
    observation.args[0] = .{
        .outer = .{ .rc_addr = 0x4000, .count = builtins.utils.REFCOUNT_STATIC_DATA },
        .count_after = builtins.utils.REFCOUNT_STATIC_DATA,
    };
    observation.result_outer = .{ .rc_addr = 0x4000, .count = builtins.utils.REFCOUNT_STATIC_DATA };

    begin();
    defer end();

    var reintroduced = RcEffect.retainsSharingArgs(1);
    reintroduced.result_unique = true;
    overrideRow(.str_drop_prefix, reintroduced);
    defer clearOverrides();

    record(observation);
    try std.testing.expectEqual(@as(usize, 0), findings().len);
}

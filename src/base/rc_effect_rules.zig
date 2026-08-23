//! Structural rules every `LowLevel.rcEffect()` row must satisfy.
//!
//! `RcEffect` is expressive enough to describe subtle ownership regimes, which
//! makes it expressive enough to describe a builtin wrongly. A wrong row is
//! not a type error and not a functional test failure—it is a silent
//! refcount imbalance in generated code. Two layers close that gap; this file
//! is the first one:
//!
//! 1. Structural rules (here): combinations no builtin can truthfully have.
//!    `assertTableConforms` runs the whole table through them at comptime, so
//!    a row that contradicts itself is a compile error naming the op and the
//!    rule it broke.
//! 2. Conformance (`eval/rc_conformance.zig`): structurally valid claims that
//!    do not match what the Zig builtin actually does at runtime.
//!
//! Rules are stated as implications between fields, one per field of
//! `RcEffect`, and are derived from the proof obligations documented on the
//! struct itself. They reject rows; they never repair or reinterpret them.
//!
//! Mask-versus-arity is deliberately *not* checked here: the number of
//! arguments an op takes is not recorded next to the table, and a second
//! hand-written arity table would be one more unchecked claim. Argument
//! positions are certified against the arguments an op is actually given, at
//! the seams where the two meet—`maskExceedsArgCount` is called by the ARC
//! borrow certifier for every low-level statement in every debug build, and by
//! the conformance observer for every op the interpreter executes.

const std = @import("std");

const LowLevel = @import("LowLevel.zig").LowLevel;

const RcEffect = LowLevel.RcEffect;

/// A structural contradiction between the fields of one `RcEffect` row.
pub const Rule = enum {
    retain_args_without_rc_flag,
    consume_args_without_rc_flag,
    retain_result_without_rc_flag,
    shares_args_without_effect,
    retain_and_consume_same_arg,
    alias_of_unconsumed_arg,
    runtime_check_of_unconsumed_arg,
    runtime_check_without_allocate,
    borrow_of_consumed_arg,
    borrow_and_share_same_arg,
    borrow_without_result_retain,
    unique_result_without_source,
    unique_result_that_is_retained,

    /// What the row claimed, and why those claims cannot both hold.
    pub fn description(self: Rule) []const u8 {
        return switch (self) {
            .retain_args_without_rc_flag => "retain_args names an argument, so the op adjusts a refcount; may_retain_or_release must be true",
            .consume_args_without_rc_flag => "consume_args names an argument, so the op releases or moves that argument's unit; may_retain_or_release must be true",
            .retain_result_without_rc_flag => "retain_result adds a count to the result, so may_retain_or_release must be true",
            .shares_args_without_effect => "result_shares_args means the result holds a handle into an argument's allocation, which requires either a fresh outer value (may_allocate) or a count adjustment (may_retain_or_release)",
            .retain_and_consume_same_arg => "one argument is both retained and consumed; the two cancel, so the row describes a borrow and must name neither",
            .alias_of_unconsumed_arg => "result_aliases_consumed_args names an argument the op does not consume; there is no unit to move into the result",
            .runtime_check_of_unconsumed_arg => "may_runtime_uniqueness_check_args names an argument the op does not consume; the in-place path would mutate a value the op does not own",
            .runtime_check_without_allocate => "a runtime uniqueness check exists to choose between mutating in place and copying, so the copy path allocates; may_allocate must be true",
            .borrow_of_consumed_arg => "result_borrows_args names a consumed argument; the lender's unit is gone by the time the result would be read through it",
            .borrow_and_share_same_arg => "one argument is named as both a borrow lender and an interior-sharing lender; the result either owns a handle into it or does not",
            .borrow_without_result_retain => "result_borrows_args names a lender, but without retain_result ARC never links the result to it and the borrow is silently dropped",
            .unique_result_without_source => "result_unique claims the result's outermost allocation has count 1, but the op neither allocates it (may_allocate) nor takes it out of a consumed argument (result_aliases_consumed_args)",
            .unique_result_that_is_retained => "retain_result means the result is read out of a structure that stays live, so its outermost allocation cannot also be uniquely owned",
        };
    }
};

/// The first rule this row breaks, or null when the row is structurally valid.
///
/// Structural validity says nothing about whether the row matches its builtin;
/// that is what the conformance harness measures.
pub fn violation(effect: RcEffect) ?Rule {
    if (effect.retain_args != 0 and !effect.may_retain_or_release) {
        return .retain_args_without_rc_flag;
    }
    if (effect.consume_args != 0 and !effect.may_retain_or_release) {
        return .consume_args_without_rc_flag;
    }
    if (effect.retain_result and !effect.may_retain_or_release) {
        return .retain_result_without_rc_flag;
    }
    if (effect.result_shares_args != 0 and !effect.may_allocate and !effect.may_retain_or_release) {
        return .shares_args_without_effect;
    }
    if ((effect.retain_args & effect.consume_args) != 0) {
        return .retain_and_consume_same_arg;
    }
    if ((effect.result_aliases_consumed_args & ~effect.consume_args) != 0) {
        return .alias_of_unconsumed_arg;
    }
    if ((effect.may_runtime_uniqueness_check_args & ~effect.consume_args) != 0) {
        return .runtime_check_of_unconsumed_arg;
    }
    if (effect.may_runtime_uniqueness_check_args != 0 and !effect.may_allocate) {
        return .runtime_check_without_allocate;
    }
    if ((effect.result_borrows_args & effect.consume_args) != 0) {
        return .borrow_of_consumed_arg;
    }
    if ((effect.result_borrows_args & effect.result_shares_args) != 0) {
        return .borrow_and_share_same_arg;
    }
    if (effect.result_borrows_args != 0 and !effect.retain_result) {
        return .borrow_without_result_retain;
    }
    if (effect.result_unique and !effect.may_allocate and effect.result_aliases_consumed_args == 0) {
        return .unique_result_without_source;
    }
    if (effect.result_unique and effect.retain_result) {
        return .unique_result_that_is_retained;
    }
    return null;
}

/// The lowest argument position a mask names that the op was not given, or
/// null when every named position exists.
///
/// A mask bit above the real argument count names an argument that does not
/// exist: ARC reads it as "no such position", so whatever ownership the row
/// meant to describe is silently dropped.
pub fn maskExceedsArgCount(effect: RcEffect, arg_count: usize) ?u6 {
    const named = effect.may_runtime_uniqueness_check_args |
        effect.consume_args |
        effect.result_aliases_consumed_args |
        effect.retain_args |
        effect.result_borrows_args |
        effect.result_shares_args;
    if (named == 0) return null;
    if (arg_count >= 64) return null;

    const in_range: u64 = if (arg_count == 0) 0 else (@as(u64, std.math.maxInt(u64)) >> @intCast(64 - arg_count));
    const out_of_range = named & ~in_range;
    if (out_of_range == 0) return null;
    return @intCast(@ctz(out_of_range));
}

/// Reject one row at compile time, naming the op and the rule it broke.
///
/// The rule tag comes last so a build step can require it verbatim without
/// restating the whole message; `run-test-rc-effect-rejected-row` does exactly
/// that.
pub fn assertRowConforms(comptime op_name: []const u8, comptime effect: RcEffect) void {
    comptime {
        if (violation(effect)) |rule| {
            @compileError("RcEffect row for low-level op '" ++ op_name ++ "' is invalid: " ++
                rule.description() ++ " [rule: " ++ @tagName(rule) ++ "]");
        }
    }
}

/// Reject every structurally invalid row in the table at compile time.
///
/// Called from `base/mod.zig`, so no build can link a compiler whose ownership
/// table contradicts itself.
pub fn assertTableConforms() void {
    @setEvalBranchQuota(2_000_000);
    comptime {
        for (std.enums.values(LowLevel)) |op| {
            assertRowConforms(@tagName(op), op.rcEffect());
        }
    }
}

test "every row in the table is structurally valid" {
    for (std.enums.values(LowLevel)) |op| {
        if (violation(op.rcEffect())) |rule| {
            std.debug.print(
                "op {s} violates {s}: {s}\n",
                .{ @tagName(op), @tagName(rule), rule.description() },
            );
            return error.StructurallyInvalidRcEffectRow;
        }
    }
}

test "the #10023 row is rejected: a shared-interior result is not a unique allocation" {
    // `str_drop_prefix` returns a substring of its argument's allocation. PR
    // roc-lang/roc#10023 (issue #9953) removed `result_unique` from this
    // constructor family after ARC counted both a fresh birth and a link to
    // the lender, leaking one reference per call.
    var reintroduced = RcEffect.retainsSharingArgs(1);
    reintroduced.result_unique = true;

    try std.testing.expectEqual(Rule.unique_result_without_source, violation(reintroduced).?);
    try std.testing.expectEqual(@as(?Rule, null), violation(RcEffect.retainsSharingArgs(1)));
}

test "each rule rejects the row it names, and every rule is reachable" {
    const cases = [_]struct { rule: Rule, effect: RcEffect }{
        .{ .rule = .retain_args_without_rc_flag, .effect = .{ .retain_args = 1 } },
        .{ .rule = .consume_args_without_rc_flag, .effect = .{ .consume_args = 1 } },
        .{ .rule = .retain_result_without_rc_flag, .effect = .{ .retain_result = true } },
        .{ .rule = .shares_args_without_effect, .effect = .{ .result_shares_args = 1 } },
        .{ .rule = .retain_and_consume_same_arg, .effect = .{
            .may_retain_or_release = true,
            .retain_args = 1,
            .consume_args = 1,
        } },
        .{ .rule = .alias_of_unconsumed_arg, .effect = .{
            .may_retain_or_release = true,
            .consume_args = 1,
            .result_aliases_consumed_args = 0b11,
        } },
        .{ .rule = .runtime_check_of_unconsumed_arg, .effect = .{
            .may_allocate = true,
            .may_retain_or_release = true,
            .may_runtime_uniqueness_check_args = 0b10,
            .consume_args = 1,
            .result_aliases_consumed_args = 1,
        } },
        .{ .rule = .runtime_check_without_allocate, .effect = .{
            .may_retain_or_release = true,
            .may_runtime_uniqueness_check_args = 1,
            .consume_args = 1,
            .result_aliases_consumed_args = 1,
        } },
        .{ .rule = .borrow_of_consumed_arg, .effect = .{
            .may_retain_or_release = true,
            .retain_result = true,
            .consume_args = 1,
            .result_aliases_consumed_args = 1,
            .result_borrows_args = 1,
        } },
        .{ .rule = .borrow_and_share_same_arg, .effect = .{
            .may_retain_or_release = true,
            .retain_result = true,
            .result_borrows_args = 1,
            .result_shares_args = 1,
        } },
        .{ .rule = .borrow_without_result_retain, .effect = .{
            .may_retain_or_release = true,
            .result_borrows_args = 1,
        } },
        .{ .rule = .unique_result_without_source, .effect = .{ .result_unique = true } },
        .{ .rule = .unique_result_that_is_retained, .effect = .{
            .may_allocate = true,
            .may_retain_or_release = true,
            .retain_result = true,
            .result_unique = true,
        } },
    };

    // A rule with no row that breaks it is a rule that is not doing anything.
    var seen = std.EnumSet(Rule).initEmpty();
    for (cases) |case| {
        try std.testing.expectEqual(case.rule, violation(case.effect).?);
        seen.insert(case.rule);
    }
    try std.testing.expectEqual(std.enums.values(Rule).len, seen.count());
}

test "argument positions above the real argument count are rejected" {
    const names_arg_two = RcEffect.consumesArgsRetainingArgs(0, 0b100);

    try std.testing.expectEqual(@as(?u6, 2), maskExceedsArgCount(names_arg_two, 0));
    try std.testing.expectEqual(@as(?u6, 2), maskExceedsArgCount(names_arg_two, 2));
    try std.testing.expectEqual(@as(?u6, null), maskExceedsArgCount(names_arg_two, 3));
    try std.testing.expectEqual(@as(?u6, null), maskExceedsArgCount(RcEffect.none(), 0));
}

//! A type declaration's marker pre-walk outcome is recorded at the first
//! instantiation at each position (`Check.polarizedMarkerChoices`) and read by
//! every later one. Each test references a declaration more than once at one
//! position, so a later reference reads the recorded outcome, which
//! `Instantiator.debugAssertKnownMarkerChoices` checks against the walk in
//! every build these tests run in. The where-method tests also instantiate the
//! signatures per body use, which always walks.
const std = @import("std");
const types = @import("types");
const TestEnv = @import("TestEnv.zig");

const PolarityVarBehavior = types.instantiate.Instantiator.PolarityVarBehavior;

/// The recorded outcomes at one behavior: how many were accepted with no
/// marker, accepted with markers (and how many of those markers were
/// chosen), and rejected.
const Summary = struct {
    markerless: usize = 0,
    with_markers: usize = 0,
    chosen_markers: usize = 0,
    rejected: usize = 0,
};

fn summarize(env: *const TestEnv, behavior: PolarityVarBehavior) Summary {
    var summary: Summary = .{};
    var entries = env.checker.polarized_marker_choices.iterator();
    while (entries.next()) |entry| {
        if (entry.key_ptr.polarity_behavior != behavior) continue;
        switch (entry.value_ptr.*) {
            .rejected => summary.rejected += 1,
            .chosen => |run| {
                const choices = env.checker.polarized_marker_choice_items.items[run.start..][0..run.len];
                if (choices.len == 0) {
                    summary.markerless += 1;
                } else {
                    summary.with_markers += 1;
                    for (choices) |choice| {
                        if (choice.chosen) summary.chosen_markers += 1;
                    }
                }
            },
        }
    }
    return summary;
}

test "marker choices - a declaration with no marker records an empty outcome" {
    var env = try TestEnv.init("Test",
        \\Pair(a) : (a, a)
        \\
        \\swap : Pair(Str) -> Pair(Str)
        \\swap = |(x, y)| (y, x)
        \\
        \\dup : Str -> Pair(Str)
        \\dup = |s| (s, s)
        \\
        \\main = swap(dup("x"))
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("main", "Pair(Str)");
    const summary = summarize(&env, .resolve_by_polarity);
    try std.testing.expect(summary.markerless >= 1);
    try std.testing.expectEqual(@as(usize, 0), summary.with_markers);
    try std.testing.expectEqual(@as(usize, 0), summary.rejected);
}

test "marker choices - an alias marker opens in every output reference and closes in every input one" {
    var env = try TestEnv.init("Test",
        \\Errs : [E1, E2]
        \\
        \\first : Str -> Errs
        \\first = |_| E1
        \\
        \\second : Str -> Errs
        \\second = |_| E2
        \\
        \\widened : Str -> [E1, E2, E3]
        \\widened = |s| if s == "" first(s) else second(s)
        \\
        \\consume : Errs -> Str
        \\consume = |_| "a"
        \\
        \\consume_again : Errs -> Str
        \\consume_again = |_| "b"
        \\
        \\main = consume_again(E3)
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
    const summary = summarize(&env, .resolve_by_polarity);
    // One outcome per position: the output one opens the marker, the input
    // one closes it.
    try std.testing.expectEqual(@as(usize, 2), summary.with_markers);
    try std.testing.expectEqual(@as(usize, 1), summary.chosen_markers);
}

test "marker choices - a where-method signature keeps a referenced alias's marker deferred" {
    var env = try TestEnv.init("Test",
        \\Errs : [E1, E2]
        \\
        \\parse_one : a -> Errs where [a.parse : a -> Errs]
        \\parse_one = |value| value.parse()
        \\
        \\parse_other : a -> [E1, E2, E3] where [a.parse : a -> Errs]
        \\parse_other = |value| value.parse()
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("parse_other", "a -> [E1, E2, E3] where [a.parse : a -> Errs]");
    const summary = summarize(&env, .defer_open);
    try std.testing.expectEqual(@as(usize, 1), summary.with_markers);
    try std.testing.expectEqual(@as(usize, 1), summary.chosen_markers);
}

test "marker choices - a where alias's signatures reach every referencing annotation" {
    var env = try TestEnv.init("Test",
        \\Errs : [E1, E2]
        \\
        \\a.Parses : where [a.parse : a -> Errs]
        \\
        \\a.Renders : where [a.render : a -> Errs]
        \\
        \\parse_one : a -> Errs where [a.Parses]
        \\parse_one = |value| value.parse()
        \\
        \\parse_other : a -> [E1, E2, E3] where [a.Parses]
        \\parse_other = |value| value.parse()
        \\
        \\render_wide : a -> [E1, E2, E3] where [a.Renders]
        \\render_wide = |value| value.render()
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("parse_other", "a -> [E1, E2, E3] where [a.parse : a -> Errs]");
    try env.assertDefType("render_wide", "a -> [E1, E2, E3] where [a.render : a -> Errs]");
    // Both where aliases' signatures reference `Errs` at the same result
    // position, so the second reads the first's outcome.
    const summary = summarize(&env, .defer_open);
    try std.testing.expectEqual(@as(usize, 1), summary.with_markers);
    try std.testing.expectEqual(@as(usize, 1), summary.chosen_markers);
}

test "marker choices - an imported alias records its outcome in the importing module" {
    var lib_env = try TestEnv.init("Lib",
        \\module [Errs]
        \\
        \\Errs : [E1, E2]
    );
    defer lib_env.deinit();

    var main_env = try TestEnv.initWithImport("Main",
        \\import Lib
        \\
        \\first : Str -> Lib.Errs
        \\first = |_| E1
        \\
        \\second : Str -> Lib.Errs
        \\second = |_| E2
        \\
        \\widened : Str -> [E1, E2, E3]
        \\widened = |s| if s == "" first(s) else second(s)
    , "Lib", &lib_env);
    defer main_env.deinit();
    try main_env.assertNoErrors();
    const summary = summarize(&main_env, .resolve_by_polarity);
    try std.testing.expectEqual(@as(usize, 1), summary.with_markers);
    try std.testing.expectEqual(@as(usize, 1), summary.chosen_markers);
}

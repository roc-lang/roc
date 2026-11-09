//! Regression tests for issue 12141.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12141
//
// A queued dispatch relation resolves under the scheme whose checking queued
// it. Resolving it while a later lambda's generalization boundary is being
// checked leaves the requirements of its method target's instantiation and of
// its component obligations with the relation's own owner, never in the
// lambda's scheme.

test "issue 12141: is_eq on a Try with a list-of-tags error beside a tagged literal closure" {
    const source =
        \\check : Str -> Try(Str, [Bad(List([Empty]))])
        \\check = |name| if name.is_empty() Err(Bad([Empty])) else Ok(name)
        \\
        \\with_fallback : Str, [Fallback(Str -> Str)] -> Str
        \\with_fallback = |name, _fallback| name
        \\
        \\expect check("") == Err(Bad([Empty]))
        \\
        \\expect with_fallback("note", Fallback(|_| "x")) == "note"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "issue 12141: a queued comparison resolved inside a later lambda stays with its expect" {
    const source =
        \\xs = []
        \\
        \\expect xs == []
        \\
        \\expect {
        \\    _y = |_| List.append(xs, [Empty])
        \\    Bool.True
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "issue 12141: a structural comparison's components are derived once" {
    const source =
        \\check : Str -> Try(Str, [Bad(List([Empty]))])
        \\check = |name| if name.is_empty() Err(Bad([Empty])) else Ok(name)
        \\
        \\expect check("") == Err(Bad([Empty]))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    // One derivation appends all of a comparison's component obligations
    // together, so a comparison derived twice shows up as a second run of
    // children for the same parent.
    const derivations = test_env.checker.component_derivations.items;
    for (derivations, 0..) |derivation, i| {
        if (i == 0 or derivations[i - 1].parent_fn_var == derivation.parent_fn_var) continue;
        for (derivations[0..i]) |earlier| {
            try std.testing.expect(earlier.parent_fn_var != derivation.parent_fn_var);
        }
    }
}

//! Declaration-level rejection of recursive nominal types applied at growing
//! type arguments.
//!
//! Monomorphization instantiates a declaration's backing at every distinct
//! argument tuple, so a declaration group is monomorphizable exactly when
//! those tuples form a finite set. A formal wrapped in more structure on a
//! formal-flow cycle grows without bound, and a recursive mention argument
//! carrying a variable that is no formal of its declaration is minted fresh
//! at every level; either way no application has a Monotype, and the checker
//! reports it once, at the declaration. Growth that never flows back into
//! its own declaration stays finite and is accepted.

const TestEnv = @import("./TestEnv.zig");

test "a recursive mention that wraps its own formal is rejected at the declaration" {
    const src =
        \\Nest(a) := [Done(a), More(Nest(List(a)))]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Invalid Recursive Type");
    try test_env.assertNominalDeclValidity("Nest", false);
}

test "a mutual recursion that wraps a formal is rejected at the wrapping declaration" {
    const src =
        \\Alpha(x) := [MkAlpha(Beta(List(x)))]
        \\Beta(y) := [MkBeta(Alpha(y)), EndBeta]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Invalid Recursive Type");
    try test_env.assertNominalDeclValidity("Alpha", false);
}

test "a recursive mention at a closed argument stays valid" {
    const src =
        \\Weird(a) := [End, Wrap(Weird(Str))]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("Weird", true);
}

test "a recursive mention that permutes its formals stays valid" {
    const src =
        \\Pair(a, b) := [Mk(Pair(b, a)), Stop]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("Pair", true);
}

test "wrapping a formal in a mention of a non-recursive declaration stays valid" {
    const src =
        \\Wrap(a) := [W(a)]
        \\Deep(x) := [T(Wrap(List(x))), DeepEnd]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("Wrap", true);
    try test_env.assertNominalDeclValidity("Deep", true);
}

test "regular recursion with the mention inside a container payload stays valid" {
    const src =
        \\Node(msg) := [Text(Str), Element(Str, List(Node(msg)))]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("Node", true);
}

test "growth that resets through a closed argument stays valid" {
    // Deep wraps its formal in the mention of Wrap, but the only way from
    // Wrap back into Deep passes the closed argument Str, so the growth
    // never returns to its origin: the reachable instantiations are just
    // Wrap at the original argument, Deep(Str), and Wrap(List(Str)).
    const src =
        \\Wrap(a) := [W(a), Back(Deep(Str))]
        \\Deep(x) := [T(Wrap(List(x))), DeepEnd]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("Wrap", true);
    try test_env.assertNominalDeclValidity("Deep", true);
}

test "a recursive mention that hides its formal behind an alias is rejected" {
    // A transparent alias of a formal is not the formal cell itself: every
    // expansion level instantiates its own alias node, so this recursion
    // grows exactly like a constructor-wrapped formal.
    const src =
        \\Same(a) : a
        \\Nest(a) := [Done, More(Nest(Same(a)))]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Invalid Recursive Type");
    try test_env.assertNominalDeclValidity("Nest", false);
}

test "mutual recursion at closed arguments with a formal-dependent payload stays valid" {
    const src =
        \\Alt(x) := [Load(List(x)), Wrap(Bare(Str))]
        \\Bare(y) := [Tie(Alt(Str)), End]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertNominalDeclValidity("Alt", true);
    try test_env.assertNominalDeclValidity("Bare", true);
}

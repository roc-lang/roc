//! Regression tests for https://github.com/roc-lang/roc/issues/13011.
//!
//! A top-level constant whose open tag row carries no requirement quantifies
//! that row (design.md "Polymorphic Constants"), so every use, including one
//! inside a generalized function that relates it to another function's
//! output row, widens its own copy, and the constant itself never gains the
//! tags a use adds. Each program here is therefore accepted, and `theme.v`
//! stays `[Light]`/`[Plain]` however its uses widen it.

const TestEnv = @import("./TestEnv.zig");

test "issue 13011 - a widening use inside a generalized function widens its own copy of a constant's row" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\theme = { v: Light }
        \\
        \\preference : Bool -> [Light, Dark]
        \\preference = |dark| if dark Dark else Light
        \\
        \\one = |value| [theme.v, preference(value)]
        \\
        \\seven = one(Bool.True).append(Other)
        \\
        \\four : [Light, Dark] -> Str
        \\four = |t| match t {
        \\    Light => "l"
        \\    Dark => "d"
        \\}
        \\
        \\five = four(theme.v)
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "issue 13011 - a row created between two young rows and joined to a constant's row is the use's own" {
    // `grow(theme.v)` is a young row whose tail is the value's row, and
    // `Other(value)` is a young row with a young tail. Relating them makes a
    // row for `Other(value)` that then joins the value's row, so `value`'s
    // type is the value's one type: `one` cannot be used at two types.
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\theme = { v: Plain }
        \\
        \\grow : [Plain, ..r] -> [Plain, Grown, ..r]
        \\grow = |_| crash "grow"
        \\
        \\one = |value| [grow(theme.v), Other(value)]
        \\
        \\first = one(1.U8)
        \\
        \\second = one("s")
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "issue 13011 - a parameter nested in a widened copy of a constant's row stays the function's own" {
    // `one` widens the value's row with `Wrapped(List(a))`, where `a` is the
    // type of `one`'s own parameter. The value shares that row, so `a` is the
    // value's one type: `one` cannot be used at two types.
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\theme = { v: Plain }
        \\
        \\wrap : a -> [Plain, Wrapped(List(a))]
        \\wrap = |x| Wrapped([x])
        \\
        \\one = |value| [theme.v, wrap(value)]
        \\
        \\first = one(1.U8)
        \\
        \\second = one("s")
    );
    defer env.deinit();

    try env.assertNoErrors();
}

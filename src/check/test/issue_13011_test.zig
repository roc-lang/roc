//! Regression tests for https://github.com/roc-lang/roc/issues/13011.
//!
//! A top-level value binding does not generalize, so every variable in its
//! type stays monomorphic however its uses inside generalized functions widen
//! it. Here `one` relates the value's open tag row to the instantiated output
//! row of `preference`, which extends the value's row by the tags only the
//! other side lists. The value's row keeps one shared extension, so a tag a use
//! of `one`'s result adds (`Other`) widens the value itself, and passing the
//! value to a function that accepts exactly `[Light, Dark]` is a type mismatch.

const TestEnv = @import("./TestEnv.zig");

test "issue 13011 - a widening use inside a generalized function keeps a value's row monomorphic" {
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

    try env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 18, .start_column = 13, .end_column = 20 });
}

test "issue 13011 - a row created between two young rows and joined to a value's row stays monomorphic" {
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

    try env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 12, .start_column = 14, .end_column = 17 });
}

test "issue 13011 - a parameter nested in a widened value's row stays monomorphic" {
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

    try env.assertOneTypeErrorHighlightsWithin("Type Mismatch", .{ .line = 12, .start_column = 14, .end_column = 17 });
}

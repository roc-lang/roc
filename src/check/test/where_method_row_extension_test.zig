//! A `..` on a `where` method's result row means an implementing type's
//! method may return more tags than the row lists (design.md "Polarity"). It
//! means that at the method's direct result row and in an argument of a named
//! type, and whether it is written bare or named.

const TestEnv = @import("./TestEnv.zig");

const types_source =
    \\Narrow := [N].{
    \\    parse : Narrow -> [BadNum, Fine]
    \\    parse = |_| BadNum
    \\}
    \\
    \\Wide := [W].{
    \\    parse : Wide -> [BadNum, Fine, TooBig]
    \\    parse = |_| TooBig
    \\}
    \\
;

/// Check `types_source ++ source` and require exactly the problems titled
/// `titles`, errors and warnings, in order.
fn expectProblems(comptime source: []const u8, titles: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", types_source ++ source);
    defer test_env.deinit();
    try test_env.assertTypeErrorTitles(titles);
}

/// `expectProblems` for a method row that `before` leaves unfinished, ended
/// with a bare `..` and with a named `..others`: both spellings must give the
/// same problems.
fn expectBothSpellings(comptime before: []const u8, comptime after: []const u8, titles: []const []const u8) TestEnv.TestEnvError!void {
    try expectProblems(before ++ ", .." ++ after, titles);
    try expectProblems(before ++ ", ..others" ++ after, titles);
}

test "a where-method's result row without `..` bounds an implementation by its tags" {
    // A type whose method returns the listed tags is accepted, and the body
    // may match its own copy of the row exhaustively.
    try expectProblems(
        \\describe : a -> Str where [a.parse : a -> [BadNum, Fine]]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\}
        \\
        \\narrow = describe(Narrow.N)
    , &.{});
    // A catch-all is accepted too: the body's copy of the row is its own to
    // leave open.
    try expectProblems(
        \\describe : a -> Str where [a.parse : a -> [BadNum, Fine]]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\    _ => "other"
        \\}
        \\
        \\narrow = describe(Narrow.N)
    , &.{});
    // The body may widen its own copy at a use.
    try expectProblems(
        \\run : a, Bool -> [BadNum, Fine, Empty] where [a.parse : a -> [BadNum, Fine]]
        \\run = |x, b| if b Empty else x.parse()
        \\
        \\narrow = run(Narrow.N, Bool.False)
    , &.{});
    // A type whose method returns another tag is rejected at the call.
    try expectProblems(
        \\describe : a -> Str where [a.parse : a -> [BadNum, Fine]]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\}
        \\
        \\wide = describe(Wide.W)
    , &.{"Type Mismatch"});
}

test "a where-method's result row with `..` accepts an implementation with more tags" {
    // Both types are accepted, with no redundant-`..` warning.
    try expectBothSpellings(
        \\describe : a -> Str where [a.parse : a -> [BadNum, Fine
    ,
        \\]]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\    _ => "other"
        \\}
        \\
        \\narrow = describe(Narrow.N)
        \\wide = describe(Wide.W)
    , &.{});
}

test "a where-method's result row with `..` needs a catch-all in the body" {
    try expectBothSpellings(
        \\describe : a -> Str where [a.parse : a -> [BadNum, Fine
    ,
        \\]]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\}
    , &.{"Non Exhaustive Match"});
}

test "a where-method's result row with `..` is not the body's to widen" {
    // The row's other tags are the implementing type's, so the body cannot
    // add one to its copy.
    try expectBothSpellings(
        \\run : a, Bool -> [BadNum, Fine, Empty] where [a.parse : a -> [BadNum, Fine
    ,
        \\]]
        \\run = |x, b| if b Empty else x.parse()
    , &.{"Type Mismatch"});
}

test "a where-method's result row with `..` behind an alias and in a named type's argument" {
    // An alias that is its formal's own row, and an argument of `Try`.
    try expectProblems(
        \\Outcome(e) : e
        \\
        \\describe : a -> Str where [a.parse : a -> Outcome([BadNum, Fine, ..])]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\    _ => "other"
        \\}
        \\
        \\wide = describe(Wide.W)
    , &.{});
    try expectProblems(
        \\Outcome(e) : e
        \\
        \\describe : a -> Str where [a.parse : a -> Outcome([BadNum, Fine, ..])]
        \\describe = |x| match x.parse() {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\}
    , &.{"Non Exhaustive Match"});
    try expectProblems(
        \\Checked := [C].{
        \\    check : Checked -> Try(U64, [BadNum, TooBig])
        \\    check = |_| Err(TooBig)
        \\}
        \\
        \\describe : a -> Str where [a.check : a -> Try(U64, [BadNum, ..])]
        \\describe = |x| match x.check() {
        \\    Ok(_) => "ok"
        \\    Err(BadNum) => "bad"
        \\    Err(_) => "other"
        \\}
        \\
        \\checked = describe(Checked.C)
    , &.{});
}

test "a function's own result row with `..` is redundant beside a where clause" {
    // The function produces its own result, so its `..` means what its
    // absence means; the method's does not.
    try expectProblems(
        \\run : a -> [Ran, Failed, ..] where [a.parse : a -> [BadNum, Fine, ..]]
        \\run = |x| match x.parse() {
        \\    Fine => Ran
        \\    _ => Failed
        \\}
    , &.{"Redundant Open Tag Union"});
}

test "a callback parameter's result row with `..` accepts a callback with more tags" {
    // A callback's result is an input, where `..` has always quantified the
    // row: a wider callback is accepted and the body needs a catch-all.
    try expectProblems(
        \\describe : (a -> [BadNum, Fine, ..]), a -> Str
        \\describe = |parse, x| match parse(x) {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\    _ => "other"
        \\}
        \\
        \\wide : Str -> [BadNum, Fine, TooBig]
        \\wide = |_| TooBig
        \\
        \\used = describe(wide, "")
    , &.{});
    try expectProblems(
        \\describe : (a -> [BadNum, Fine, ..]), a -> Str
        \\describe = |parse, x| match parse(x) {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\}
    , &.{"Non Exhaustive Match"});
    // Without `..` the callback's row is exactly the listed tags.
    try expectProblems(
        \\describe : (a -> [BadNum, Fine]), a -> Str
        \\describe = |parse, x| match parse(x) {
        \\    Fine => "ok"
        \\    BadNum => "bad"
        \\}
        \\
        \\wide : Str -> [BadNum, Fine, TooBig]
        \\wide = |_| TooBig
        \\
        \\used = describe(wide, "")
    , &.{"Type Mismatch"});
}

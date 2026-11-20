//! `Try` is an ordinary nominal type in checking: no relation descends into
//! its arguments except the `?` operator's own composition (design.md "Try
//! Return-Row Composition"). Each case here states a program over `Try` and
//! its twin over `Outcome(good, bad) := [Good(good), Bad(bad)]`, a user type
//! of the same shape, and requires the same verdict for both.

const TestEnv = @import("./TestEnv.zig");

const outcome_decl =
    \\Outcome(good, bad) := [Good(good), Bad(bad)]
    \\
;

const Verdict = enum { accepted, mismatch };

/// Check `source` and require exactly the problems titled `titles`, errors
/// and warnings, in order.
fn expectProblems(source: []const u8, titles: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertTypeErrorTitles(titles);
}

/// Check `try_source` and `outcome_source` (after the `Outcome` declaration)
/// and require `verdict` of both.
fn expectTwins(verdict: Verdict, try_source: []const u8, comptime outcome_source: []const u8) TestEnv.TestEnvError!void {
    const titles: []const []const u8 = switch (verdict) {
        .accepted => &.{},
        .mismatch => &.{"Type Mismatch"},
    };
    try expectProblems(try_source, titles);
    try expectProblems(outcome_decl ++ outcome_source, titles);
}

test "a function's quantified row in a named type's argument widens at each call" {
    try expectTwins(.accepted,
        \\parse : Str -> Try(U64, [BadNum])
        \\parse = |s| if s == "" Err(BadNum) else Ok(1)
        \\
        \\use : Str, Bool -> Try(U64, [BadNum, Empty])
        \\use = |s, b| if b Err(Empty) else parse(s)
    ,
        \\parse : Str -> Outcome(U64, [BadNum])
        \\parse = |s| if s == "" Outcome.Bad(BadNum) else Outcome.Good(1)
        \\
        \\use : Str, Bool -> Outcome(U64, [BadNum, Empty])
        \\use = |s, b| if b Outcome.Bad(Empty) else parse(s)
    );
}

test "a closed parameter does not flow into a wider error row" {
    try expectTwins(.mismatch,
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else t
    ,
        \\use : Outcome(U64, [BadNum]), Bool -> Outcome(U64, [BadNum, Empty])
        \\use = |t, b| if b Outcome.Bad(Empty) else t
    );
}

test "a closed parameter does not flow into a wider ok row" {
    try expectTwins(.mismatch,
        \\use : Try([A, B], Str), Bool -> Try([A, B, C], Str)
        \\use = |t, b| if b Ok(C) else t
    ,
        \\use : Outcome([A, B], Str), Bool -> Outcome([A, B, C], Str)
        \\use = |t, b| if b Outcome.Good(C) else t
    );
}

test "a callback's closed result does not flow into a wider error row" {
    try expectTwins(.mismatch,
        \\use : (Str -> Try(U64, [BadNum])), Bool -> Try(U64, [BadNum, Empty])
        \\use = |f, b| if b Err(Empty) else f("x")
    ,
        \\use : (Str -> Outcome(U64, [BadNum])), Bool -> Outcome(U64, [BadNum, Empty])
        \\use = |f, b| if b Outcome.Bad(Empty) else f("x")
    );
}

test "a where-method's result does not flow into a wider error row" {
    try expectTwins(.mismatch,
        \\run : a, Bool -> Try(U64, [BadNum, Empty]) where [a.parse : a -> Try(U64, [BadNum])]
        \\run = |x, c| if c x.parse() else Err(Empty)
    ,
        \\run : a, Bool -> Outcome(U64, [BadNum, Empty]) where [a.parse : a -> Outcome(U64, [BadNum])]
        \\run = |x, c| if c x.parse() else Outcome.Bad(Empty)
    );
}

test "a forwarding body closes its result row for its callers" {
    // Nothing is reported at `fwd`: its body binds the annotation's row to the
    // parameter's closed one, and the caller that needs a wider row reports.
    try expectTwins(.mismatch,
        \\fwd : Try(U64, [BadNum]) -> Try(U64, [BadNum])
        \\fwd = |t| t
        \\
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else fwd(t)
    ,
        \\fwd : Outcome(U64, [BadNum]) -> Outcome(U64, [BadNum])
        \\fwd = |t| t
        \\
        \\use : Outcome(U64, [BadNum]), Bool -> Outcome(U64, [BadNum, Empty])
        \\use = |t, b| if b Outcome.Bad(Empty) else fwd(t)
    );
}

test "a rebuilding body leaves its result row open for its callers" {
    try expectTwins(.accepted,
        \\build : Try(U64, [BadNum]) -> Try(U64, [BadNum])
        \\build = |t| match t {
        \\    Ok(n) => Ok(n)
        \\    Err(e) => Err(e)
        \\}
        \\
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else build(t)
    ,
        \\build : Outcome(U64, [BadNum]) -> Outcome(U64, [BadNum])
        \\build = |t| match t {
        \\    Good(n) => Good(n)
        \\    Bad(e) => Bad(e)
        \\}
        \\
        \\use : Outcome(U64, [BadNum]), Bool -> Outcome(U64, [BadNum, Empty])
        \\use = |t, b| if b Bad(Empty) else build(t)
    );
}

test "a body that rebuilds with qualified tags closes its result row for its callers" {
    // A qualified tag constructs the named type at its payload's own type,
    // so the closed payload row is the result's argument, as in a forwarding
    // body.
    try expectTwins(.mismatch,
        \\build : Try(U64, [BadNum]) -> Try(U64, [BadNum])
        \\build = |t| match t {
        \\    Ok(n) => Try.Ok(n)
        \\    Err(e) => Try.Err(e)
        \\}
        \\
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else build(t)
    ,
        \\build : Outcome(U64, [BadNum]) -> Outcome(U64, [BadNum])
        \\build = |t| match t {
        \\    Good(n) => Outcome.Good(n)
        \\    Bad(e) => Outcome.Bad(e)
        \\}
        \\
        \\use : Outcome(U64, [BadNum]), Bool -> Outcome(U64, [BadNum, Empty])
        \\use = |t, b| if b Outcome.Bad(Empty) else build(t)
    );
}

test "a closed value rebuilt tag by tag flows into a wider row" {
    try expectTwins(.accepted,
        \\use : Try([A, B], [BadNum]), Bool -> Try([A, B, C], [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else match t {
        \\    Ok(v) => Ok(v)
        \\    Err(e) => Err(e)
        \\}
    ,
        \\use : Outcome([A, B], [BadNum]), Bool -> Outcome([A, B, C], [BadNum, Empty])
        \\use = |t, b| if b Bad(Empty) else match t {
        \\    Good(v) => Good(v)
        \\    Bad(e) => Bad(e)
        \\}
    );
}

/// Check `source` and require no problem.
fn expectAccepted(source: []const u8) TestEnv.TestEnvError!void {
    try expectProblems(source, &.{});
}

test "`?` composes a closed error row into a wider one" {
    try expectAccepted(
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else Ok(t?)
    );
    try expectAccepted(
        \\use : (Str -> Try(U64, [BadNum])), Bool -> Try(U64, [BadNum, Empty])
        \\use = |f, b| if b Err(Empty) else Ok(f("x")?)
    );
    try expectAccepted(
        \\run : a, Bool -> Try(U64, [BadNum, Empty]) where [a.parse : a -> Try(U64, [BadNum])]
        \\run = |x, c| if c Ok(x.parse()?) else Err(Empty)
    );
    try expectAccepted(
        \\fwd : Try(U64, [BadNum]) -> Try(U64, [BadNum])
        \\fwd = |t| t
        \\
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else Ok(fwd(t)?)
    );
}

test "`map_err` and `map_ok` rebuild a closed row into a wider one" {
    try expectAccepted(
        \\use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
        \\use = |t, b| if b Err(Empty) else t.map_err(|BadNum| BadNum)
    );
    try expectAccepted(
        \\use : Try([A, B], Str), Bool -> Try([A, B, C], Str)
        \\use = |t, b| if b Ok(C) else t.map_ok(|v| match v {
        \\    A => A
        \\    B => B
        \\})
    );
    try expectAccepted(
        \\use : Try([A, B], Str), Bool -> Try([A, B, C], Str)
        \\use = |t, b| if b Ok(C) else Ok(match t? {
        \\    A => A
        \\    B => B
        \\})
    );
}

test "`?` composes a row its annotation bounds" {
    // An annotated local value and an annotated top-level value each have a
    // row that can gain no tag, so `?` includes it by its tags alone.
    try expectAccepted(
        \\closed_top : Try(U64, [BadNum])
        \\closed_top = Err(BadNum)
        \\
        \\use : Bool -> Try(U64, [BadNum, Empty])
        \\use = |b| {
        \\    local : Try(U64, [BadNum])
        \\    local = if b Ok(0) else Err(BadNum)
        \\    x = local?
        \\    y = if b x else closed_top?
        \\    if y == 0 Err(Empty) else Ok(y)
        \\}
    );
}

test "an unannotated local's `?` on a method result shares the method's error row" {
    // `widen` infers `b -> Try(ok, err) where [b.status : b -> Try(ok, err)]`:
    // its error row is the method's own, which `x`'s requirement states
    // exactly, so the use at a wider row does not fit.
    try expectProblems(
        \\show : Try(Str, [Bad(Str), Other]) -> Str
        \\show = |v| match v {
        \\    Ok(s) => s
        \\    Err(Bad(e)) => e
        \\    Err(Other) => "other"
        \\}
        \\
        \\describe : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
        \\describe = |x| {
        \\    widen = |y| Ok(y.status()?)
        \\    show(widen(x))
        \\}
    , &.{"Type Mismatch"});
    // An annotated local composes the method's closed row into its own
    // result, which each use instantiates.
    try expectAccepted(
        \\show : Try(Str, [Bad(Str), Other]) -> Str
        \\show = |v| match v {
        \\    Ok(s) => s
        \\    Err(Bad(e)) => e
        \\    Err(Other) => "other"
        \\}
        \\
        \\describe : a -> Str where [a.status : a -> Try(Str, [Bad(Str)])]
        \\describe = |x| {
        \\    widen : b -> Try(Str, [Bad(Str)]) where [b.status : b -> Try(Str, [Bad(Str)])]
        \\    widen = |y| Ok(y.status()?)
        \\    show(widen(x))
        \\}
    );
}

const where_types =
    \\Narrow := [N].{
    \\    parse : Narrow -> Try(U64, [BadNum])
    \\    parse = |_| Err(BadNum)
    \\}
    \\
    \\Wide := [W].{
    \\    parse : Wide -> Try(U64, [BadNum, TooBig])
    \\    parse = |_| Err(TooBig)
    \\}
    \\
;

/// Check `where_types ++ source` and require exactly the problems titled
/// `titles`.
fn expectWhereProblems(titles: []const []const u8, comptime source: []const u8) TestEnv.TestEnvError!void {
    try expectProblems(where_types ++ source, titles);
}

test "a where-method's Try error row without `..` lists exactly an implementation's tags" {
    // The `match` needs no catch-all, and a type whose method returns the
    // listed tags is accepted.
    try expectWhereProblems(&.{},
        \\describe : a -> Str where [a.parse : a -> Try(U64, [BadNum])]
        \\describe = |x| match x.parse() {
        \\    Ok(_) => "ok"
        \\    Err(BadNum) => "bad number"
        \\}
        \\
        \\narrow = describe(Narrow.N)
    );
    // A catch-all is redundant.
    try expectWhereProblems(&.{"Redundant Pattern"},
        \\describe : a -> Str where [a.parse : a -> Try(U64, [BadNum])]
        \\describe = |x| match x.parse() {
        \\    Ok(_) => "ok"
        \\    Err(BadNum) => "bad number"
        \\    Err(_) => "some other error"
        \\}
        \\
        \\narrow = describe(Narrow.N)
    );
    // A type whose method returns another tag is rejected at the call.
    try expectWhereProblems(&.{"Type Mismatch"},
        \\describe : a -> Str where [a.parse : a -> Try(U64, [BadNum])]
        \\describe = |x| match x.parse() {
        \\    Ok(_) => "ok"
        \\    Err(BadNum) => "bad number"
        \\}
        \\
        \\wide = describe(Wide.W)
    );
}

test "a where-method's Try error row with `..` accepts an implementation with more tags" {
    // Both types are accepted, the `..` is not reported as redundant, and
    // the `match` handles the tags the row does not list.
    try expectWhereProblems(&.{},
        \\describe : a -> Str where [a.parse : a -> Try(U64, [BadNum, ..])]
        \\describe = |x| match x.parse() {
        \\    Ok(_) => "ok"
        \\    Err(BadNum) => "bad number"
        \\    Err(_) => "some other error"
        \\}
        \\
        \\narrow = describe(Narrow.N)
        \\wide = describe(Wide.W)
    );
    // Without a catch-all the `match` does not cover the row.
    try expectWhereProblems(&.{"Non Exhaustive Match"},
        \\describe : a -> Str where [a.parse : a -> Try(U64, [BadNum, ..])]
        \\describe = |x| match x.parse() {
        \\    Ok(_) => "ok"
        \\    Err(BadNum) => "bad number"
        \\}
        \\
        \\narrow = describe(Narrow.N)
    );
    // The row is the enclosing function's own, not one a use may widen.
    try expectWhereProblems(&.{"Type Mismatch"},
        \\run : a, Bool -> Try(U64, [BadNum, Empty]) where [a.parse : a -> Try(U64, [BadNum, ..])]
        \\run = |x, c| if c x.parse() else Err(Empty)
    );
}

test "a where-method's direct result row still opens per use" {
    // The requirement's own row is the one position a body use may widen.
    try expectWhereProblems(&.{},
        \\Status := [S].{
        \\    status : Status -> [Up, Down]
        \\    status = |_| Up
        \\}
        \\
        \\report : a, Bool -> [Up, Down, Unknown] where [a.status : a -> [Up, Down]]
        \\report = |x, c| if c x.status() else Unknown
        \\
        \\reported = report(Status.S, Bool.True)
    );
}

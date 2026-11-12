//! Accepted and rejected sides of uncommitted record and tag tails
//! (design.md "Uncommitted Record Tails", "Uncommitted Tag Tails"): which
//! anonymous structure can still be lifted into a nominal type or widened
//! with optional fields, and where it commits.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

fn expectOk(comptime source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

fn expectDefType(comptime source: []const u8, def: []const u8, expected: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType(def, expected);
}

fn expectOneMismatch(comptime source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

const Refusal = @import("../problem.zig").RecordTailRefusal;

/// Expect exactly one type mismatch, refused because a use's record was asked
/// for more than its binding built or took apart.
fn expectRefusal(comptime source: []const u8, kind: Refusal.Kind, taken_apart: bool) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
    const refusal = test_env.checker.problems.problems.items[0].type_mismatch.record_tail_refusal orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(kind, refusal.kind);
    try std.testing.expectEqual(taken_apart, refusal.taken_apart);
}

const counter =
    \\Counter := { count : U64 }.{
    \\    describe : Counter -> Str
    \\    describe = |c| c.count.to_str()
    \\}
    \\
;

const light =
    \\Light := [Red, Green].{
    \\    describe : Light -> Str
    \\    describe = |_| "light"
    \\}
    \\
;

// Record literals

test "a record literal lifts into a nominal type" {
    try expectDefType(counter ++
        \\c : Counter
        \\c = { count: 1 }
    , "c", "Counter");
}

test "an empty record literal lifts into a nominal type with an empty backing" {
    try expectDefType(
        \\Token := {}
        \\t : Token
        \\t = {}
    , "t", "Token");
}

test "a record literal absorbs an omitted optional field" {
    try expectDefType(
        \\r : { a : U8, b ?: U8 }
        \\r = { a: 1 }
    , "r", "{ a: U8, b ?: U8 }");
}

test "a record literal commits closed: an extra required field is a mismatch" {
    try expectOneMismatch(
        \\r : { x : I64, y : Str }
        \\r = { x: 1 }
    );
}

test "a record literal renders as a closed record" {
    try expectDefType(
        \\make = |n| { count: n }
    , "make", "a -> { count: a }");
}

// Annotated records

test "an annotated record result never lifts" {
    try expectOneMismatch(counter ++
        \\make : U64 -> { count : U64 }
        \\make = |n| { count: n }
        \\c : Counter
        \\c = make(1)
    );
}

test "an annotated record result never widens" {
    try expectOneMismatch(
        \\make : U8 -> { a : U8 }
        \\make = |n| { a: n }
        \\r : { a : U8, b ?: U8 }
        \\r = make(1)
    );
}

test "an annotated record result rejects a nominal its body returns" {
    try expectOneMismatch(counter ++
        \\lift : U64 -> { count : U64 }
        \\lift = |n| {
        \\    c : Counter
        \\    c = { count: n }
        \\    c
        \\}
    );
}

// A record nobody wrote a type for stays open in its function's scheme. A use
// closes it or makes it a nominal type with exactly its fields stored the same
// way, wherever it sits in the function's type.

test "an unannotated function's result lifts at one call and stays a record at another" {
    try expectDefType(counter ++
        \\make = |n| { count: n }
        \\c : Counter
        \\c = make(1)
        \\p = make(2.U64)
    , "p", "{ count: U64 }");
}

test "two calls lift one function's result into two nominal types" {
    try expectOk(counter ++
        \\Other := { count : U64 }
        \\make = |n| { count: n }
        \\c : Counter
        \\c = make(1)
        \\o : Other
        \\o = make(2)
    );
}

test "a record nested in a returned list lifts at the call" {
    try expectOk(counter ++
        \\wrap = |n| [{ count: n }]
        \\xs : List(Counter)
        \\xs = wrap(1)
    );
}

test "a record passed to a callback argument lifts at the call" {
    try expectOk(counter ++
        \\apply = |f| f({ count: 1.U64 })
        \\s = apply(Counter.describe)
        \\n = apply(|r| r.count)
    );
}

test "a record a returned function returns lifts at the call" {
    try expectOk(counter ++
        \\reader = |n| |m| { count: n + m }
        \\c : Counter
        \\c = reader(1)(2)
        \\p = reader(3.U64)(4)
    );
}

test "a record pattern parameter of an unannotated function lifts at the call" {
    try expectOk(
        \\Point := { x : U64, y : U64 }
        \\get_x = |{ x, y }| x + y
        \\n = get_x(Point.{ x: 1, y: 2 })
        \\m = get_x({ x: 1, y: 2 })
    );
}

test "a record pattern still lifts inside an annotated function" {
    try expectOk(
        \\Rect := { w : U64, h : U64 }
        \\area : Rect -> U64
        \\area = |{ w, h }| w * h
    );
}

test "a tail on both sides of a function lifts at the call" {
    try expectDefType(counter ++
        \\both = |r| if r.count > 0 r else { count: 0 }
        \\c : Counter
        \\c = both({ count: 1 })
        \\p = both({ count: 2.U64 })
    , "p", "{ count: U64 }");
}

test "a tail both passed to a callback and taken as an argument lifts at the call" {
    try expectOk(counter ++
        \\pass = |f, r| f(if r.count > 0 r else { count: 0 })
        \\s = pass(Counter.describe, { count: 1 })
    );
}

test "a record pattern in a callback's own callback argument lifts at the call" {
    try expectOk(counter ++
        \\feed = |f| f(|{ count }| count)
        \\n = feed(|g| g(Counter.{ count: 1 }))
    );
}

test "a record only a static dispatch constraint holds lifts at the call" {
    try expectOk(counter ++
        \\Sink := {}.{
        \\    take : Sink, Counter -> U64
        \\    take = |_, c| c.count
        \\}
        \\make = |sink, n| sink.take({ count: n })
        \\sink : Sink
        \\sink = {}
        \\n = make(sink, 1)
    );
}

test "a record in a function stored in a list lifts at the call" {
    try expectOk(counter ++
        \\makers = |n| [|m| { count: n + m }]
        \\fs : List(U64 -> Counter)
        \\fs = makers(1)
    );
}

test "a record in a nominal type's argument lifts at the call" {
    try expectOk(counter ++
        \\Holder(a) := { item : a }
        \\hold = |n| Holder.{ item: { count: n } }
        \\h : Holder(Counter)
        \\h = hold(1)
    );
}

// A use's record never gains a field, and never becomes a nominal type stored
// differently from it

test "a use's record gains no optional field" {
    try expectRefusal(
        \\make = |value| { a: value }
        \\r : { a : U8, b ?: U8 }
        \\r = make(11)
    , .added_field, false);
}

test "a use's record gains no optional field through its nominal" {
    try expectRefusal(
        \\Wide := { a : U8, note ?: Str }
        \\make = |value| { a: value }
        \\w : Wide
        \\w = make(1)
    , .added_field, false);
}

test "a use's record gains no defaulted field through its nominal" {
    try expectRefusal(
        \\Cfg := { a : U8, b : U8 ?? 7 }
        \\keep = |r| if r.a > 0 r else { a: 1.U8 }
        \\c : Cfg
        \\c = keep({ a: 2 })
    , .added_field, false);
}

test "a record a function takes apart gains no field from the nominal passed to it" {
    try expectRefusal(
        \\Wide := { x : U8, note ?: Str }
        \\get_x = |{ x }| x
        \\w : Wide
        \\w = { x: 1 }
        \\n = get_x(w)
    , .added_field, true);
}

test "a nominal whose backing supplies every defaulted field takes the record" {
    try expectOk(
        \\Cfg := { a : U8, b : U8 ?? 7 }
        \\make = |n| { a: n, b: n }
        \\c : Cfg
        \\c = make(1)
    );
}

test "a missing required field stays an ordinary mismatch" {
    try expectOneMismatch(counter ++
        \\Pair := { count : U64, more : U64 }
        \\make = |n| { count: n }
        \\p : Pair
        \\p = make(1)
    );
}

// Rejecting a padded nominal is deliberate: turning a record into a nominal
// type with unnamed fields would need its values rebuilt at every use, an
// extremely rare want whose only gain is slightly shorter code, and the
// compiler is faster for not doing that work (design.md "Uncommitted Record
// Tails").

test "a use's record does not become a padded nominal" {
    try expectRefusal(
        \\Row := { columns : U64, _ : U32, values : U64 }
        \\make = |c, v| { columns: c, values: v }
        \\r : Row
        \\r = make(1, 2)
    , .padded_nominal, false);
}

test "a padded nominal is not passed to a function that takes its record apart" {
    try expectRefusal(
        \\Row := { columns : U64, _ : U32, values : U64 }
        \\product = |{ columns, values }| columns * values
        \\row : Row
        \\row = { columns: 3, values: 4 }
        \\n = product(row)
    , .padded_nominal, true);
}

test "a record literal checked at a padded nominal is built at it" {
    try expectOk(
        \\Row := { columns : U64, _ : U32, values : U64 }
        \\row : Row
        \\row = { columns: 3, values: 4 }
    );
}

test "a field-use row still lifts at a call" {
    try expectOk(counter ++
        \\get = |r| r.count
        \\n = get(Counter.{ count: 1 })
    );
}

// Value bindings share one type

test "a value binding's uses share its lift" {
    try expectOk(counter ++
        \\shared = { count: 1.U64 }
        \\c : Counter
        \\c = shared
        \\d = shared.describe()
    );
}

test "a value binding committed by one use rejects another nominal" {
    try expectOneMismatch(counter ++
        \\Other := { count : U64 }
        \\shared = { count: 1.U64 }
        \\c : Counter
        \\c = shared
        \\o : Other
        \\o = shared
    );
}

test "an unresolved tail ends as a plain closed record" {
    try expectDefType(
        \\plain = { a: 1.U8 }
    , "plain", "{ a: U8 }");
}

// Defaulted fields

test "a record literal absorbs a defaulted field through its nominal" {
    try expectOk(
        \\Cfg := { a : U8, b : U8 ?? 7 }
        \\c : Cfg
        \\c = { a: 1 }
    );
}

test "a call's result never absorbs a defaulted field" {
    try expectOneMismatch(
        \\Cfg := { a : U8, b : U8 ?? 7 }
        \\make = |n| { a: n }
        \\outer : { c : Cfg }
        \\outer = { c: make(1) }
    );
}

// Tag unions

test "an annotated tag union result never lifts" {
    try expectOneMismatch(light ++
        \\pick : U64 -> [Red, Green]
        \\pick = |n| if n > 0 Red else Green
        \\l : Light
        \\l = pick(1)
    );
}

test "an annotated tag union result still widens" {
    try expectOk(
        \\pick : U64 -> [Red, Green]
        \\pick = |n| if n > 0 Red else Green
        \\c : [Red, Green, Blue]
        \\c = pick(1)
    );
}

test "an annotated local tag value never lifts" {
    try expectOneMismatch(light ++
        \\l = {
        \\    e : [Red, Green]
        \\    e = Red
        \\    lifted : Light
        \\    lifted = e
        \\    lifted
        \\}
    );
}

test "an unannotated tag function's result lifts at one call and stays anonymous at another" {
    try expectDefType(light ++
        \\pick = |n| if n > 0 Red else Green
        \\l : Light
        \\l = pick(1)
        \\p = pick(0)
    , "p", "[Green, Red]");
}

test "a union an exhaustive match closed still lifts" {
    try expectOk(
        \\is_ok = |r| match r {
        \\    Ok(_) => Bool.True
        \\    Err(_) => Bool.False
        \\}
        \\t : Try(U8, Str)
        \\t = Ok(1)
        \\a = is_ok(t)
        \\b = is_ok(Ok(1))
    );
}

test "a union an exhaustive match closed gains no tag" {
    try expectOneMismatch(
        \\f = |r, c| {
        \\    _ = match r {
        \\        A => 1.U8
        \\        B => 2
        \\    }
        \\    x : [A, B, C]
        \\    x = C
        \\    if c r else x
        \\}
    );
}

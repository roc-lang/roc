//! Accepted and rejected sides of uncommitted record and tag tails
//! (design.md "Uncommitted Record Tails", "Uncommitted Tag Tails"): which
//! anonymous structure can still be lifted into a nominal type or widened
//! with optional fields, and where it commits.
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

// Unannotated functions: each call owns its result

test "an unannotated function's result lifts at one call and stays a record at another" {
    try expectDefType(counter ++
        \\make = |n| { count: n }
        \\c : Counter
        \\c = make(1)
        \\p = make(2.U64)
    , "p", "{ count: U64 }");
}

test "an unannotated function's result widens differently at two calls" {
    try expectOk(
        \\make = |value| { a: value }
        \\r1 : { a ?: U8, b ?: U8 }
        \\r1 = make(11)
        \\r2 : { a ?: U8, b ?: Str }
        \\r2 = make(5)
    );
}

test "a record nested in a returned list lifts at the call" {
    try expectOk(counter ++
        \\wrap = |n| [{ count: n }]
        \\xs : List(Counter)
        \\xs = wrap(1)
    );
}

// Argument-side and both-sides record tails commit at the boundary

test "a record pattern parameter of an unannotated function commits closed" {
    try expectOneMismatch(
        \\Point := { x : U64, y : U64 }
        \\get_x = |{ x, y }| x + y
        \\n = get_x(Point.{ x: 1, y: 2 })
    );
}

test "a record pattern still lifts inside an annotated function" {
    try expectOk(
        \\Rect := { w : U64, h : U64 }
        \\area : Rect -> U64
        \\area = |{ w, h }| w * h
    );
}

test "a tail on both sides of a function commits closed" {
    try expectOneMismatch(counter ++
        \\both = |r| if r.count > 0 r else { count: 0 }
        \\c : Counter
        \\c = both({ count: 1 })
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

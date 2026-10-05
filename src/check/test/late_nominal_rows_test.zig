//! Pin general bare-tag lifting and observable row complements independently
//! of constructor provenance and eager versus delayed nominal instantiation.

const TestEnv = @import("./TestEnv.zig");

test "late nominal bare helper is chosen at an unannotated call" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\make_some = |value| Some(value)
        \\consume : Choice(U64) -> U64
        \\consume = |choice| match choice {
        \\    Some(value) => value
        \\    None => 0
        \\    Message(_) => 0
        \\}
        \\answer = consume(make_some(42))
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal bare helper stays general across nominal and structural uses" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\Other(a) := [Some(a), OtherEnd]
        \\make_some = |value| Some(value)
        \\first : Choice(U64)
        \\first = make_some(42)
        \\second : Other(Str)
        \\second = make_some("text")
        \\third : [Some(Bool)]
        \\third = make_some(True)
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal multi-tag helper preserves every visible payload" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\make_choice = |flag, value| if flag Some(value) else None
        \\first : Choice(U64)
        \\first = make_choice(True, 42)
        \\second : [None, Some(Str)]
        \\second = make_choice(False, "text")
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal bare helper rejects a missing nominal variant" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\make_absent = |value| Absent(value)
        \\result : Choice(U64)
        \\result = make_absent(42)
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal repeated visible payload preserves shared equality" {
    const src =
        \\Choice(a) := [Pair(a, a), Other(a)]
        \\make_pair = |first, second| Pair(first, second)
        \\answer : Choice(U64)
        \\answer = make_pair(42, "different")
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal omitted payload retains equality with visible payload" {
    const src =
        \\Choice(a) := [Some(a), Other(a)]
        \\carry : [Some(U64), ..r], r -> { row: [Some(U64), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\pair = carry(Some(42), Other("different"))
        \\nominal : Choice(U64)
        \\nominal = pair.row
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal structural row rejects mismatched nominal arity" {
    const src =
        \\Choice(a) := [Some(a, a), None]
        \\make_some = |value| Some(value)
        \\answer : Choice(U64)
        \\answer = make_some(42)
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal structural row does not lift through a non-tag backing" {
    const src =
        \\Count := U64
        \\make_some = |value| Some(value)
        \\answer : Count
        \\answer = make_some(42)
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal open backing retains its declared residual extension" {
    const src =
        \\Open(r) := [Some(U64), ..r]
        \\carry : [Some(U64), ..r], r -> { row: [Some(U64), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\pair = carry(Some(42), Other("text"))
        \\nominal : Open([Other(Str)])
        \\nominal = pair.row
        \\complement : [Other(Str)]
        \\complement = {
        \\    _nominal = nominal
        \\    pair.tail
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal bare helper cannot open an imported opaque backing" {
    const source =
        \\Choice(a) :: [Some(a), None].{}
    ;
    var origin = try TestEnv.init("Choice", source);
    defer origin.deinit();
    try origin.assertNoErrors();
    const caller =
        \\import Choice
        \\make_some = |value| Some(value)
        \\answer : Choice(U64)
        \\answer = make_some(42)
    ;
    var env = try TestEnv.initWithImport("Caller", caller, "Choice", &origin);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal intermediary import reuses aliased schema with independent openings" {
    const source =
        \\Payload(a) : (a, a)
        \\Choice(a) := [Some(Payload(a)), Other(Payload(a))].{
        \\    forward : Choice(a) -> Choice(a)
        \\    forward = |value| value
        \\}
    ;
    var origin = try TestEnv.init("Choice", source);
    defer origin.deinit();
    try origin.assertNoErrors();
    const relay_source =
        \\import Choice
        \\Relay := [].{
        \\    Payload(a) : (a, a)
        \\    forward = |value| Choice.forward(value)
        \\    some : Payload(a) -> [Some(Payload(a))]
        \\    some = |value| Some(value)
        \\}
    ;
    var relay = try TestEnv.initWithImport("Relay", relay_source, "Choice", &origin);
    defer relay.deinit();
    try relay.assertNoErrors();
    const caller =
        \\import Relay
        \\first = Relay.forward(Relay.some((42, 42)))
        \\second = Relay.forward(Relay.some(("left", "right")))
        \\third = Relay.forward(Relay.some((True, False)))
    ;
    var env = try TestEnv.initWithImport("Caller", caller, "Relay", &relay);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal occurs rejects recursion through an omitted payload" {
    const src =
        \\Choice(a) := [Some(a), Other(a)]
        \\carry : [Some(r), ..r], r -> { row: [Some(r), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\close = |tail| {
        \\    pair = carry(Some(tail), tail)
        \\    [Choice.Some(tail), pair.row]
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertFirstTypeError("Anonymous Recursion");
}

test "late nominal generalized helper retains escaped monomorphic payload" {
    const src =
        \\Choice(a) := [Some(a), Other(a)]
        \\outer = |value| {
        \\    helper = || Some(value)
        \\    first : Choice(U64)
        \\    first = helper()
        \\    second : Choice(Str)
        \\    second = helper()
        \\    (first, second)
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal shared row tail retains the exact complement" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\carry : [Some(U64), ..r], r -> { row: [Some(U64), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\pair = carry(Some(42), None)
        \\nominal : Choice(U64)
        \\nominal = pair.row
        \\complement : [None, Message(Str)]
        \\complement = {
        \\    _nominal = nominal
        \\    pair.tail
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal complement narrowing control without nominal relation is accepted" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\carry : [Some(U64), ..r], r -> { row: [Some(U64), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\close = |tail| {
        \\    pair = carry(Some(42), tail)
        \\    complement : [None]
        \\    complement = pair.tail
        \\    complement
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertNoErrors();
}

test "late nominal complement cannot discard solely omitted Message" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\carry : [Some(U64), ..r], r -> { row: [Some(U64), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\close = |tail| {
        \\    pair = carry(Some(42), tail)
        \\    nominal : Choice(U64)
        \\    nominal = pair.row
        \\    complement : [None]
        \\    complement = {
        \\        _nominal = nominal
        \\        pair.tail
        \\    }
        \\    complement
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "late nominal shared row tail cannot be replaced by empty" {
    const src =
        \\Choice(a) := [None, Some(a), Message(Str)]
        \\carry : [Some(U64), ..r], r -> { row: [Some(U64), ..r], tail: r }
        \\carry = |row, tail| { row, tail }
        \\pair = carry(Some(42), None)
        \\nominal : Choice(U64)
        \\nominal = pair.row
        \\complement : []
        \\complement = {
        \\    _nominal = nominal
        \\    pair.tail
        \\}
    ;
    var env = try TestEnv.init("Test", src);
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

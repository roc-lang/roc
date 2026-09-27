//! Formatter tests for dropping anonymous `..` tag-union extensions that mean
//! exactly what their absence means (design.md "Polarity"). Every case that
//! keeps a `..` is one where dropping it would change the annotation, or where
//! the formatter cannot tell from the file alone that it would not.

const std = @import("std");
const fmt = @import("fmt.zig");

/// Format `input` and compare it with `expected`, whose lines are indented
/// with four spaces per level where the formatter writes a tab.
fn expectFormatsTo(input: []const u8, expected: []const u8) (fmt.FormatTestError || error{TestExpectedEqual})!void {
    const result = try fmt.moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    var tabbed = std.ArrayList(u8).empty;
    defer tabbed.deinit(std.testing.allocator);
    var lines = std.mem.splitScalar(u8, expected, '\n');
    var first = true;
    while (lines.next()) |line| {
        if (!first) try tabbed.append(std.testing.allocator, '\n');
        first = false;
        var rest = line;
        while (std.mem.startsWith(u8, rest, "    ")) : (rest = rest[4..]) {
            try tabbed.append(std.testing.allocator, '\t');
        }
        try tabbed.appendSlice(std.testing.allocator, rest);
    }
    try std.testing.expectEqualStrings(tabbed.items, result);
}

fn expectUnchanged(input: []const u8) (fmt.FormatTestError || error{TestExpectedEqual})!void {
    try expectFormatsTo(input, input);
}

test "open rows - function result drops its `..`" {
    try expectFormatsTo(
        \\parse : Str -> [Fail, Ok, ..]
        \\parse = |_| Ok
        \\
    ,
        \\parse : Str -> [Fail, Ok]
        \\parse = |_| Ok
        \\
    );
}

test "open rows - function argument keeps its `..`" {
    try expectUnchanged(
        \\handle : [Known, ..] -> Str
        \\handle = |_| "ok"
        \\
    );
}

test "open rows - value binding keeps its `..`" {
    // On a value, `..` is the opt-in to a quantified row.
    try expectUnchanged(
        \\boom : [Boom, ..]
        \\boom = Boom
        \\
    );
}

test "open rows - function annotation on a non-lambda body keeps its `..`" {
    try expectUnchanged(
        \\parse : Str -> [Fail, Ok, ..]
        \\parse = make_parser(1)
        \\
    );
}

test "open rows - value alias keeps its `..`" {
    try expectUnchanged(
        \\parse : Str -> [Fail, Ok, ..]
        \\parse = other_parse
        \\
    );
}

test "open rows - var annotation keeps its `..`" {
    try expectUnchanged(
        \\main = |_| {
        \\    var $x : [A, ..]
        \\    var $x = A
        \\    $x
        \\}
        \\
    );
}

test "open rows - empty union and named extension are kept" {
    try expectUnchanged(
        \\empty : Str -> [..]
        \\empty = |_| crash "x"
        \\
        \\named : Str -> [A, ..others]
        \\named = |_| A
        \\
    );
}

test "open rows - arguments of a function argument are outputs" {
    // `[A]` is the argument of a callback: the callback receives it from the
    // annotated function, so it is an output of that function.
    try expectFormatsTo(
        \\call : ([A, ..] -> Str) -> Str
        \\call = |f| f(A)
        \\
    ,
        \\call : ([A] -> Str) -> Str
        \\call = |f| f(A)
        \\
    );
}

test "open rows - result of a function argument is an input" {
    try expectUnchanged(
        \\run : (Str -> [A, ..]) -> Str
        \\run = |_| "x"
        \\
    );
}

test "open rows - result of a returned function is an output, its argument an input" {
    try expectFormatsTo(
        \\make : Str -> ([A, ..] -> [B, ..])
        \\make = |_| |_| B
        \\
    ,
        \\make : Str -> ([A, ..] -> [B])
        \\make = |_| |_| B
        \\
    );
}

test "open rows - nested output positions all drop" {
    try expectFormatsTo(
        \\f : Str -> Try([A([B, ..]), ..], [E, ..])
        \\f = |_| Ok(A(B))
        \\
        \\g : Str -> { x : List([C, ..]), y : ([D, ..], Str) }
        \\g = |_| { x: [C], y: (D, "") }
        \\
    ,
        \\f : Str -> Try([A([B])], [E])
        \\f = |_| Ok(A(B))
        \\
        \\g : Str -> { x : List([C]), y : ([D], Str) }
        \\g = |_| { x: [C], y: (D, "") }
        \\
    );
}

test "open rows - input positions keep nested `..`" {
    try expectUnchanged(
        \\f : Try([A([B, ..]), ..], [E, ..]) -> Str
        \\f = |_| "x"
        \\
        \\g : { x : List([C, ..]) } -> Str
        \\g = |_| "x"
        \\
    );
}

test "open rows - local alias composes its formal's variance" {
    // `Handler` holds `e` in an input position, so `Handler([A, ..])` written
    // as an output stands for `[A, ..] -> Str` and keeps its `..`; written as
    // an input, the two negations cancel and it drops.
    try expectFormatsTo(
        \\Handler(e) : e -> Str
        \\
        \\Producer(e) : Str -> e
        \\
        \\handler : Str -> Handler([A, ..])
        \\handler = |_| |_| "x"
        \\
        \\use_handler : Handler([A, ..]) -> Str
        \\use_handler = |h| h(A)
        \\
        \\producer : Str -> Producer([A, ..])
        \\producer = |_| |_| A
        \\
        \\use_producer : Producer([A, ..]) -> Str
        \\use_producer = |_| "x"
        \\
    ,
        \\Handler(e) : e -> Str
        \\
        \\Producer(e) : Str -> e
        \\
        \\handler : Str -> Handler([A, ..])
        \\handler = |_| |_| "x"
        \\
        \\use_handler : Handler([A]) -> Str
        \\use_handler = |h| h(A)
        \\
        \\producer : Str -> Producer([A])
        \\producer = |_| |_| A
        \\
        \\use_producer : Producer([A, ..]) -> Str
        \\use_producer = |_| "x"
        \\
    );
}

test "open rows - invariant formal is generated at the negative polarity" {
    // `Both` holds `e` on both sides, so its argument is generated closed; a
    // function argument inside that argument flips back to an output.
    try expectFormatsTo(
        \\Both(e) : e -> e
        \\
        \\both : Str -> Both([A, ..])
        \\both = |_| |x| x
        \\
        \\nested : Str -> Both([A, ..] -> Str)
        \\nested = |_| |x| x
        \\
    ,
        \\Both(e) : e -> e
        \\
        \\both : Str -> Both([A, ..])
        \\both = |_| |x| x
        \\
        \\nested : Str -> Both([A] -> Str)
        \\nested = |_| |x| x
        \\
    );
}

test "open rows - alias chains compose through every declaration" {
    try expectFormatsTo(
        \\Handler(e) : e -> Str
        \\
        \\Outer(e) : Handler(e)
        \\
        \\Twice(e) : Handler(Handler(e))
        \\
        \\outer : Str -> Outer([A, ..])
        \\outer = |_| |_| "x"
        \\
        \\twice : Str -> Twice([A, ..])
        \\twice = |_| |_| "x"
        \\
    ,
        \\Handler(e) : e -> Str
        \\
        \\Outer(e) : Handler(e)
        \\
        \\Twice(e) : Handler(Handler(e))
        \\
        \\outer : Str -> Outer([A, ..])
        \\outer = |_| |_| "x"
        \\
        \\twice : Str -> Twice([A])
        \\twice = |_| |_| "x"
        \\
    );
}

test "open rows - imported and qualified types keep every `..` beneath them" {
    // Another module's declaration may hold its formal in any position, so
    // nothing under it is opened, at any depth.
    try expectUnchanged(
        \\import Lib exposing [Producer]
        \\
        \\a : Str -> Producer([A, ..])
        \\a = |_| crash "x"
        \\
        \\b : Str -> Lib.Wrapper([A, ..])
        \\b = |_| crash "x"
        \\
        \\c : Str -> Lib.Wrapper([A, ..] -> Str)
        \\c = |_| crash "x"
        \\
    );
}

test "open rows - a name no visible declaration introduces keeps its `..`" {
    try expectUnchanged(
        \\a : Str -> Mystery([A, ..])
        \\a = |_| crash "x"
        \\
    );
}

test "open rows - a builtin name shadowed by a disagreeing declaration keeps its `..`" {
    // Either declaration could be the one `Try` names; they disagree about
    // the error row, so the `..` stays.
    try expectUnchanged(
        \\Try(ok, err) : err -> ok
        \\
        \\a : Str -> Try(Str, [E, ..])
        \\a = |_| |_| "x"
        \\
    );
}

test "open rows - same-named declarations that disagree keep their `..`" {
    try expectFormatsTo(
        \\Outer := [].{
        \\    Wrap(e) : e -> Str
        \\}
        \\
        \\Wrap(e) : Str -> e
        \\
        \\a : Str -> Wrap([A, ..])
        \\a = |_| |_| A
        \\
        \\b : Str -> List([A, ..])
        \\b = |_| []
        \\
    ,
        \\Outer := [].{
        \\    Wrap(e) : e -> Str
        \\}
        \\
        \\Wrap(e) : Str -> e
        \\
        \\a : Str -> Wrap([A, ..])
        \\a = |_| |_| A
        \\
        \\b : Str -> List([A])
        \\b = |_| []
        \\
    );
}

test "open rows - annotation-only definition keeps its `..` outside an app" {
    // A platform package's annotation-only definitions are hosted functions,
    // whose rows are a fixed ABI, and a headerless file may be one of its
    // modules.
    try expectUnchanged(
        \\line! : Str => Try({}, [Closed, ..])
        \\
    );
    try expectUnchanged(
        \\module [line!]
        \\
        \\line! : Str => Try({}, [Closed, ..])
        \\
    );
}

test "open rows - annotation-only definition drops its `..` in an app" {
    try expectFormatsTo(
        \\app [main!] { pf: platform "platform/main.roc" }
        \\
        \\todo : Str -> [A, ..]
        \\
        \\main! = |_| {}
        \\
    ,
        \\app [main!] { pf: platform "platform/main.roc" }
        \\
        \\todo : Str -> [A]
        \\
        \\main! = |_| {}
        \\
    );
}

test "open rows - platform provided definitions keep their `..`" {
    try expectFormatsTo(
        \\platform ""
        \\    requires {
        \\        main! : () => {}
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\
        \\main_for_host! : {} => Try({}, [Exit(I32), ..])
        \\main_for_host! = |_| Ok({})
        \\
        \\helper : Str -> [A, ..]
        \\helper = |_| A
        \\
    ,
        \\platform ""
        \\    requires {
        \\        main! : () => {}
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\
        \\main_for_host! : {} => Try({}, [Exit(I32), ..])
        \\main_for_host! = |_| Ok({})
        \\
        \\helper : Str -> [A]
        \\helper = |_| A
        \\
    );
}

test "open rows - associated and block-local function annotations drop their `..`" {
    try expectFormatsTo(
        \\Thing := [T].{
        \\    parse : Str -> [Bad, ..]
        \\    parse = |_| {
        \\        helper : Str -> [Worse, ..]
        \\        helper = |_| Worse
        \\
        \\        value : [Bad, ..]
        \\        value = Bad
        \\
        \\        value
        \\    }
        \\}
        \\
    ,
        \\Thing := [T].{
        \\    parse : Str -> [Bad]
        \\    parse = |_| {
        \\        helper : Str -> [Worse]
        \\        helper = |_| Worse
        \\
        \\        value : [Bad, ..]
        \\        value = Bad
        \\
        \\        value
        \\    }
        \\}
        \\
    );
}

test "open rows - where-method signatures open only what lowering can re-tag" {
    // A where-method's direct result and a `Try` result's error row open per
    // use; its arguments, a `Try`'s ok row, and anything nested deeper keep
    // their rows as written.
    try expectFormatsTo(
        \\f : a -> Str
        \\    where [
        \\        a.direct : a -> [X, ..],
        \\        a.error : a -> Try(Str, [X, ..]),
        \\        a.ok : a -> Try([X, ..], Str),
        \\        a.nested : a -> List([X, ..]),
        \\        a.arg : a, [X, ..] -> Str,
        \\    ]
        \\f = |_| "x"
        \\
    ,
        \\f : a -> Str
        \\    where [
        \\        a.direct : a -> [X],
        \\        a.error : a -> Try(Str, [X]),
        \\        a.ok : a -> Try([X, ..], Str),
        \\        a.nested : a -> List([X, ..]),
        \\        a.arg : a, [X, ..] -> Str,
        \\    ]
        \\f = |_| "x"
        \\
    );
}

test "open rows - where-method error row through a local alias" {
    try expectFormatsTo(
        \\Res(e) : Try(Str, e)
        \\
        \\load : a -> Res([IoErr, Other]) where [a.fetch : a -> Res([IoErr, ..])]
        \\load = |x| x.fetch()
        \\
    ,
        \\Res(e) : Try(Str, e)
        \\
        \\load : a -> Res([IoErr, Other]) where [a.fetch : a -> Res([IoErr])]
        \\load = |x| x.fetch()
        \\
    );
}

test "open rows - where clause on a receiver generated as written keeps its `..`" {
    try expectUnchanged(
        \\f : Lib.Wrapper(a) -> Str where [a.m : a -> [X, ..]]
        \\f = |_| "x"
        \\
    );
}

test "open rows - where alias arguments are inputs" {
    try expectUnchanged(
        \\f : Str -> Try(a, [Bad, ..errs]) where [a.Parseable([Bad, ..])]
        \\f = |_| crash "x"
        \\
    );
}

test "open rows - multiline union drops its `..` line and keeps its comments" {
    try expectFormatsTo(
        \\parse : Str -> [
        \\    Fail,
        \\    Ok,
        \\    # trailing
        \\    ..,
        \\]
        \\parse = |_| Ok
        \\
    ,
        \\parse : Str -> [
        \\    Fail,
        \\    Ok,
        \\    # trailing
        \\]
        \\parse = |_| Ok
        \\
    );
}

test "open rows - multiline union drops its `..` line with no comments" {
    try expectFormatsTo(
        \\parse : Str -> [
        \\    Fail,
        \\    Ok,
        \\
        \\    ..,
        \\]
        \\parse = |_| Ok
        \\
    ,
        \\parse : Str -> [
        \\    Fail,
        \\    Ok,
        \\]
        \\parse = |_| Ok
        \\
    );
}

test "open rows - multiline union keeps a comment after its dropped `..`" {
    try expectFormatsTo(
        \\parse : Str -> [
        \\    Fail,
        \\    Ok,
        \\    ..,
        \\    # after
        \\]
        \\parse = |_| Ok
        \\
    ,
        \\parse : Str -> [
        \\    Fail,
        \\    Ok,
        \\    # after
        \\]
        \\parse = |_| Ok
        \\
    );
}

//! `roc fmt` drops an anonymous `..` tag-union extension only where the type
//! checker generates it exactly as its absence (design.md "Polarity"), which is
//! where the checker reports it as a Redundant Open Tag Union. These tests run
//! both on the same source and compare the two sets of `..`.

const std = @import("std");
const fmt = @import("fmt");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

const Relation = enum {
    /// The formatter drops exactly the `..` the checker reports.
    exact,
    /// The formatter keeps some `..` the checker reports, because it cannot
    /// tell from the file alone that dropping them is safe; it drops no other.
    formatter_subset,
};

fn expectFormatterMatchesChecker(source: []const u8, relation: Relation) (TestEnv.TestEnvError || fmt.FormatAstError)!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertCanErrors(&.{});

    var reported = std.ArrayList(u32).empty;
    defer reported.deinit(testing.allocator);
    for (test_env.checker.problems.problems.items) |problem| {
        if (problem == .redundant_open_tag_union) {
            try reported.append(testing.allocator, problem.redundant_open_tag_union.region.start.offset);
        } else if (problem != .annotation_only_value) {
            // An annotation-only definition reports `annotation_only_value`;
            // every other problem means the source is not the valid program
            // the case intends.
            try testing.expectEqualStrings("redundant_open_tag_union", @tagName(problem));
        }
    }
    std.mem.sort(u32, reported.items, {}, std.sort.asc(u32));

    const dropped_tokens = try fmt.redundantOpenExtensions(testing.allocator, test_env.parse_ast.*);
    defer testing.allocator.free(dropped_tokens);
    var dropped = std.ArrayList(u32).empty;
    defer dropped.deinit(testing.allocator);
    for (dropped_tokens) |tok| {
        try dropped.append(testing.allocator, test_env.parse_ast.tokens.resolve(tok).start.offset);
    }

    switch (relation) {
        .exact => try testing.expectEqualSlices(u32, reported.items, dropped.items),
        .formatter_subset => {
            try testing.expect(dropped.items.len < reported.items.len);
            for (dropped.items) |offset| {
                var found = false;
                for (reported.items) |reported_offset| {
                    if (reported_offset == offset) found = true;
                }
                try testing.expect(found);
            }
        },
    }
}

test "redundant open rows - function results, arguments and callbacks" {
    try expectFormatterMatchesChecker(
        \\parse : Str -> [Fail, Ok, ..]
        \\parse = |_| Ok
        \\
        \\handle : [Known, ..] -> Str
        \\handle = |_| "ok"
        \\
        \\call : ([A, ..] -> Str) -> Str
        \\call = |f| f(A)
        \\
        \\run : (Str -> [A, ..]) -> Str
        \\run = |_| "x"
        \\
        \\make : Str -> ([A, ..] -> [B, ..])
        \\make = |_| |_| B
        \\
        \\drive : ((Str -> [A, ..]) -> Str) -> Str
        \\drive = |step| step(|_| A)
        \\
        \\feed : (([A, ..] -> Str) -> Str) -> Str
        \\feed = |_| "x"
        \\
        \\empty : Str -> [..]
        \\empty = |_| crash "x"
        \\
        \\named : Str -> [A, ..others]
        \\named = |_| A
    , .exact);
}

test "redundant open rows - retained phantom callback arguments" {
    try expectFormatterMatchesChecker(
        \\Phantom(a) : {}
        \\Outer(a) : Phantom(Str -> a)
        \\Chain(a) : Outer(a)
        \\Consumer(a) : Phantom(a -> Str)
        \\use : Outer([E, ..]) -> Str
        \\use = |_| "ok"
        \\chain : Chain([E, ..]) -> Str
        \\chain = |_| "ok"
        \\direct : Phantom(Str -> [E, ..]) -> Str
        \\direct = |_| "ok"
        \\consumer : Consumer([E, ..]) -> Str
        \\consumer = |_| "ok"
    , .exact);
}

test "redundant open rows - value alias bodies" {
    // A value alias generalizes, so the checker reports its `..`; the
    // formatter cannot tell a lookup body from a value and keeps it.
    try expectFormatterMatchesChecker(
        \\parse : Str -> [Fail, Ok, ..]
        \\parse = |_| Ok
        \\
        \\alias : Str -> [Fail, Ok, ..]
        \\alias = parse
    , .formatter_subset);
}

test "redundant open rows - nested output and input positions" {
    try expectFormatterMatchesChecker(
        \\f : Str -> Try([A([B, ..]), ..], [E, ..])
        \\f = |_| Ok(A(B))
        \\
        \\g : Str -> { x : List([C, ..]), y : ([D, ..], Str) }
        \\g = |_| { x: [C], y: (D, "") }
        \\
        \\h : Try([A([B, ..]), ..], [E, ..]) -> Str
        \\h = |_| "x"
        \\
        \\i : { x : List([C, ..]) } -> Str
        \\i = |_| "x"
    , .exact);
}

test "redundant open rows - local alias variance" {
    try expectFormatterMatchesChecker(
        \\Handler(e) : e -> Str
        \\
        \\Producer(e) : Str -> e
        \\
        \\Both(e) : e -> e
        \\
        \\Outer(e) : Handler(e)
        \\
        \\Twice(e) : Handler(Handler(e))
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
        \\both : Str -> Both([A, ..])
        \\both = |_| |x| x
        \\
        \\nested : Str -> Both([A, ..] -> Str)
        \\nested = |_| |x| x
        \\
        \\outer : Str -> Outer([A, ..])
        \\outer = |_| |_| "x"
        \\
        \\twice : Str -> Twice([A, ..])
        \\twice = |_| |_| "x"
    , .exact);
}

test "redundant open rows - same-named declarations that disagree" {
    // The checker resolves `Wrap` to the top-level declaration and reports
    // the `..`; the formatter cannot tell which declaration the name reaches
    // and keeps it.
    try expectFormatterMatchesChecker(
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
    , .formatter_subset);
}

test "redundant open rows - associated and block-local annotations" {
    try expectFormatterMatchesChecker(
        \\Thing := [T].{
        \\    parse : Str -> [Bad, ..]
        \\    parse = |s| {
        \\        helper : Str -> [Worse, ..]
        \\        helper = |_| Worse
        \\
        \\        value : [Bad]
        \\        value = Bad
        \\
        \\        match helper(s) {
        \\            Worse => value
        \\        }
        \\    }
        \\}
    , .exact);
}

test "redundant open rows - where-method signatures" {
    try expectFormatterMatchesChecker(
        \\f : a -> Str
        \\    where [
        \\        a.direct : a -> [X, ..],
        \\        a.error : a -> Try(Str, [X, ..]),
        \\        a.ok : a -> Try([X, ..], Str),
        \\        a.nested : a -> List([X, ..]),
        \\        a.arg : a, [X, ..] -> Str,
        \\    ]
        \\f = |_| "x"
    , .exact);
}

test "redundant open rows - where-method error row through a local alias" {
    try expectFormatterMatchesChecker(
        \\Res(e) : Try(Str, e)
        \\
        \\load : a -> Res([IoErr, Other]) where [a.fetch : a -> Res([IoErr, ..])]
        \\load = |x| x.fetch()
    , .exact);
}

test "redundant open rows - annotation-only definition outside an app" {
    // A headerless file may be a platform package's module, whose
    // annotation-only definitions are hosted functions.
    try expectFormatterMatchesChecker(
        \\todo : Str -> [A, ..]
        \\
        \\done : Str -> [A, ..]
        \\done = |_| A
    , .formatter_subset);
}

test "redundant open rows - destructured top-level literals in an app" {
    // Can attaches a top-level annotation to the def a destructured literal
    // splits off for that name. The formatter keeps a function's `..`, since
    // it does not decide which literal field a name binds.
    try expectFormatterMatchesChecker(
        \\app [main] { pf: platform "platform.roc" }
        \\
        \\e : [Boom]
        \\(e, n) = (Boom, 1)
        \\
        \\f : Str -> [A, ..]
        \\{ f, m } = { f: |_| A, m: 2 }
        \\
        \\main = (e, n, f, m)
    , .formatter_subset);
}

test "redundant open rows - destructuring that does not bind the name in an app" {
    // The destructured literal binds neither annotated name, so both
    // annotations are annotation-only definitions.
    try expectFormatterMatchesChecker(
        \\app [main] { pf: platform "platform.roc" }
        \\
        \\todo : Str -> [A, ..]
        \\(x, y) = (1, 2)
        \\
        \\later : Str -> [B, ..]
        \\{ a, b } = { a: 1, b: 2 }
        \\
        \\main = (x, y, a, b)
    , .exact);
}

test "redundant open rows - builtin function-shaped formals force output at input uses" {
    try expectFormatterMatchesChecker(
        \\iter : Iter([E, ..]) -> Str
        \\iter = |_| "ok"
        \\stream : Stream([E, ..]) -> Str
        \\stream = |_| "ok"
    , .exact);
}

test "redundant open rows - auto-imported Range shares builtin exposure knowledge" {
    try expectFormatterMatchesChecker(
        \\value : Str -> Range([E, ..])
        \\value = |_| crash "unused"
        \\consume : Range([E, ..]) -> Str
        \\consume = |_| "ok"
    , .exact);
}

test "redundant open rows - known declarations exceed old depth and arity limits" {
    try expectFormatterMatchesChecker(
        \\A0(a) : Str -> a
        \\A1(a) : A0(a)
        \\A2(a) : A1(a)
        \\A3(a) : A2(a)
        \\A4(a) : A3(a)
        \\A5(a) : A4(a)
        \\A6(a) : A5(a)
        \\A7(a) : A6(a)
        \\A8(a) : A7(a)
        \\A9(a) : A8(a)
        \\Many(a,b,c,d,e,f,g,h,i) : Str -> i
        \\deep : A9([E, ..]) -> Str
        \\deep = |_| "ok"
        \\wide : Many({},{},{},{},{},{},{},{},[E, ..]) -> Str
        \\wide = |_| "ok"
    , .exact);
}

test "redundant open rows - recursive nominal positions and transparent resets" {
    try expectFormatterMatchesChecker(
        \\Input(t) : t -> Str
        \\Out(t) : Str -> t
        \\Recursive(a) := [Next(Input(Out(Recursive(a))))]
        \\A(a,b) := [Next(B(a,b) -> Str)]
        \\B(a,b) := [Again(A(a,Str)), Value(Str -> b)]
        \\recursive : Recursive([E, ..]) -> Str
        \\recursive = |_| "ok"
        \\separate : A([E, ..], [F, ..]) -> Str
        \\separate = |_| "ok"
    , .exact);
}

test "redundant open rows - entire function aliases preserve adapter reach" {
    try expectFormatterMatchesChecker(
        \\Result(e) : Try(Str, e)
        \\Method(a,e) : a -> Result(e)
        \\load : a -> Try(Str, [E]) where [a.fetch : Method(a,[E, ..])]
        \\load = |_| crash "unused"
    , .exact);
}

test "redundant open rows - adapter reach crosses long alias chains" {
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(testing.allocator);
    try source.appendSlice(testing.allocator, "R0(e) : Try(Str,e)\n");
    for (1..70) |index| {
        const line = try std.fmt.allocPrint(testing.allocator, "R{d}(e) : R{d}(e)\n", .{ index, index - 1 });
        defer testing.allocator.free(line);
        try source.appendSlice(testing.allocator, line);
    }
    try source.appendSlice(testing.allocator,
        \\load : a -> Try(Str,[E]) where [a.fetch : a -> R69([E, ..])]
        \\load = |_| crash "unused"
    );
    try expectFormatterMatchesChecker(source.items, .exact);
}

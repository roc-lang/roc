//! The cross-strategy differential harness's own corpus.
//!
//! Most eval cases compute their values during checking finalization, which
//! always uses `.lss`, so neither strategy's runtime value semantics runs for
//! them. These cases are expressions, whose root runs at runtime, and they
//! build every value from a seed read after a `dbg` statement, which no
//! compile-time evaluation can perform. The harness requires each of them to
//! report that `dbg` at runtime, so a case whose values were computed during
//! checking fails instead of silently testing nothing. Generic helpers make
//! `.boxy` take its descriptor-driven paths.
//!
//! Each case states which value shapes it exercises for inspection,
//! equality, and hashing. Every `ShapeKind` must be covered by at least one
//! case for each, or carry a stated reason it cannot be, so a new shape fails
//! this file's comptime check until the corpus covers it.

const std = @import("std");
const value_semantics = @import("value_semantics");
const helpers = @import("eval").test_helpers;

const ShapeKind = value_semantics.ShapeKind;

pub const Case = struct {
    name: []const u8,
    source: []const u8,
    source_kind: helpers.SourceKind = .expr,
    imports: []const helpers.ModuleSource = &.{},
    expected: ?[]const u8 = null,
    inspects: []const ShapeKind = &.{},
    compares: []const ShapeKind = &.{},
    hashes: []const ShapeKind = &.{},
};

/// Shapes a value-semantics operation cannot reach, with the reason.
pub const Exemption = struct {
    kind: ShapeKind,
    reason: []const u8,
};

pub const equality_exemptions = [_]Exemption{
    .{ .kind = .function, .reason = "functions have no is_eq; the checker rejects comparing them" },
    .{ .kind = .box, .reason = "Box has no is_eq; the checker rejects comparing boxes" },
};

pub const hash_exemptions = [_]Exemption{
    .{ .kind = .function, .reason = "functions have no to_hash; the checker rejects hashing them" },
    .{ .kind = .box, .reason = "Box has no to_hash; the checker rejects hashing boxes" },
};

pub const cases = [_]Case{
    // `to_inspect` override selection, pinned under both strategies.
    .{
        .name = "override: a nominal's procedure to_inspect replaces the default rendering",
        .source =
        \\{
        \\    Color := [Red, Green].{
        \\        to_inspect : Color -> Str
        \\        to_inspect = |color| match color {
        \\            Red => "Color.Red"
        \\            Green => "Color.Green"
        \\        }
        \\    }
        \\    show = |value| Str.inspect(value)
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    color : Color
        \\    color = if k == 1 Red else Green
        \\    (Str.inspect(color), show(color), Str.inspect({ c: color }), show([color]))
        \\}
        ,
        .expected = "(\"Color.Red\", \"Color.Red\", \"{ c: Color.Red }\", \"[Color.Red]\")",
        .inspects = &.{ .nominal, .record, .list, .dynamic },
    },
    .{
        .name = "override: an unpromoted local procedure's to_inspect replaces the default rendering",
        .source =
        \\{
        \\    render : a -> Str
        \\    render = |x| {
        \\        Local := [Local(a)].{
        \\            to_inspect = |_value| "custom"
        \\        }
        \\        Str.inspect([Local.Local(x)])
        \\    }
        \\    k = {
        \\        dbg 5.U64
        \\        5.U64
        \\    }
        \\    render(k)
        \\}
        ,
        .expected = "\"[custom]\"",
        .inspects = &.{ .nominal, .list },
    },
    .{
        .name = "override: a rejected to_inspect declaration (not at T -> Str) renders the default form while explicit calls reach it",
        .source =
        \\{
        \\    Count := [Count(U64)].{
        \\        to_inspect : Count -> U64
        \\        to_inspect = |Count.Count(n)| n + 1
        \\    }
        \\    k = {
        \\        dbg 4.U64
        \\        4.U64
        \\    }
        \\    count : Count
        \\    count = Count(k)
        \\    (Str.inspect(count), count.to_inspect())
        \\}
        ,
        .expected = "(\"Count(4)\", 5)",
        .inspects = &.{ .nominal, .tag_union },
    },

    // Inspection of every shape, built at runtime.
    .{
        .name = "inspect shapes: primitives, records, tuples, and lists",
        .source =
        \\{
        \\    show = |value| Str.inspect(value)
        \\    k = {
        \\        dbg 2.U64
        \\        2.U64
        \\    }
        \\    record = { name: "n${k.to_str()}", count: k, ratio: 1.5.F64, flag: k == 2 }
        \\    pair = (k, "two")
        \\    items = [k, k + 1]
        \\    [Str.inspect(record), show(record), Str.inspect(pair), show(pair), Str.inspect(items), show(items), show({})]
        \\}
        ,
        .expected = "[\"{ count: 2, flag: True, name: \\\"n2\\\", ratio: 1.5 }\", \"{ count: 2, flag: True, name: \\\"n2\\\", ratio: 1.5 }\", \"(2, \\\"two\\\")\", \"(2, \\\"two\\\")\", \"[2, 3]\", \"[2, 3]\", \"{}\"]",
        .inspects = &.{ .primitive, .bool, .record, .tuple, .list, .empty_record, .dynamic },
    },
    .{
        .name = "inspect shapes: tag unions, boxes, functions, and aliases",
        .source =
        \\{
        \\    Shape : [Circle(U64), Rect(U64, U64), Empty]
        \\    show = |value| Str.inspect(value)
        \\    k = {
        \\        dbg 3.U64
        \\        3.U64
        \\    }
        \\    shapes : List(Shape)
        \\    shapes = [Circle(k), Rect(k, k + 1), Empty]
        \\    boxed = Box.box(k)
        \\    f : U64 -> U64
        \\    f = |x| x + k
        \\    [Str.inspect(shapes), show(shapes), Str.inspect(boxed), show(boxed), Str.inspect(f), show(Ok(k))]
        \\}
        ,
        .expected = "[\"[Circle(3), Rect(3, 4), Empty]\", \"[Circle(3), Rect(3, 4), Empty]\", \"Box(3)\", \"Box(3)\", \"<function>\", \"Ok(3)\"]",
        .inspects = &.{ .tag_union, .box, .function, .transparent, .dynamic },
    },
    .{
        .name = "inspect shapes: optional fields render <missing> or their value",
        .source =
        \\{
        \\    show = |value| Str.inspect(value)
        \\    pick : U64 -> { a ?: U64, b : U64 }
        \\    pick = |n| if n == 1 { b: n } else { a: n, b: n }
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    (Str.inspect(pick(k)), show(pick(k + 1)))
        \\}
        ,
        .expected = "(\"{ a: <missing>, b: 1 }\", \"{ a: 2, b: 2 }\")",
        .inspects = &.{.presence_slot},
    },
    .{
        .name = "inspect shapes: opaque nominals hide their backing",
        .source =
        \\{
        \\    Secret :: { key : Str }.{
        \\        new : Str -> Secret
        \\        new = |key| { key: key }
        \\    }
        \\    Plain := { key : Str }
        \\    show = |value| Str.inspect(value)
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    secret = Secret.new("s${k.to_str()}")
        \\    plain : Plain
        \\    plain = { key: "p${k.to_str()}" }
        \\    (Str.inspect(secret), show(secret), Str.inspect(plain), show(plain))
        \\}
        ,
        .expected = "(\"<opaque>\", \"<opaque>\", \"{ key: \\\"p1\\\" }\", \"{ key: \\\"p1\\\" }\")",
        .inspects = &.{.nominal},
    },

    // Structural equality of every shape, at runtime and through generic code.
    .{
        .name = "equality shapes: scalars, records, tuples, unions, and lists",
        .source =
        \\{
        \\    Shape : [Circle(U64), Rect(U64, U64), Empty]
        \\    same = |a, b| a == b
        \\    k = {
        \\        dbg 2.U64
        \\        2.U64
        \\    }
        \\    r1 = { a: k, b: "x", c: { d: k == 2 } }
        \\    r2 = { a: k, b: "x", c: { d: k == 3 } }
        \\    s1 : Shape
        \\    s1 = Rect(k, 1)
        \\    s2 : Shape
        \\    s2 = Rect(k, 2)
        \\    circle : Shape
        \\    circle = Circle(k)
        \\    [
        \\        same(k, 2), r1 == r1, same(r1, r2), (k, "t") == (k, "t"), same((k, 1.U64), (k, 2.U64)),
        \\        s1 == s1, same(s1, s2), same(circle, circle), [k] == [k], same([k], [k, k]),
        \\        same({}, {}), same(k == 2, True),
        \\    ]
        \\}
        ,
        .expected = "[True, True, False, True, False, True, False, True, True, False, True, True]",
        .compares = &.{ .primitive, .bool, .empty_record, .record, .tuple, .tag_union, .list, .transparent, .dynamic },
    },
    .{
        .name = "equality shapes: nominals with their own is_eq, and optional fields",
        .source =
        \\{
        \\    Loose := [L(U64)].{
        \\        is_eq = |_a, _b| True
        \\    }
        \\    same = |a, b| a == b
        \\    pick : U64 -> { a ?: U64, b : U64 }
        \\    pick = |n| if n == 1 { b: n } else { a: n, b: 1 }
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    loose_a : Loose
        \\    loose_a = L(k)
        \\    loose_b : Loose
        \\    loose_b = L(k + 1)
        \\    [loose_a == loose_b, same(loose_a, loose_b), pick(k) == pick(k), same(pick(k), pick(k + 1)), same({ s: pick(k) }, { s: pick(k) })]
        \\}
        ,
        .expected = "[True, True, True, False, True]",
        .compares = &.{ .nominal, .presence_slot },
    },

    // Structural hashing of every shape. Hashes are observable through Set
    // and Dict membership; a value inserted by generic code (Boxy's
    // descriptor-driven hash) is found by specialized code (its statically
    // lowered hash) only when both hash it identically.
    .{
        .name = "hash shapes: set membership across generic and specialized hashing",
        .source =
        \\{
        \\    Shape : [Circle(U64), Rect(U64, U64), Empty]
        \\    Tagged := [T(U64)].{
        \\        is_eq = |Tagged.T(a), Tagged.T(b)| a == b
        \\        to_hash = |Tagged.T(a), hasher| hasher.write_u64(a)
        \\    }
        \\    insert_all = |items| Set.from_list(items)
        \\    k = {
        \\        dbg 2.U64
        \\        2.U64
        \\    }
        \\    records = insert_all([{ a: k, b: "x" }, { a: k + 1, b: "y" }])
        \\    tuples = insert_all([(k, "a"), (k, "b")])
        \\    shapes : Set(Shape)
        \\    shapes = insert_all([Circle(k), Rect(k, 1), Empty])
        \\    lists = insert_all([[k], [k, k], []])
        \\    nominals = insert_all([Tagged.T(k), Tagged.T(k + 1)])
        \\    flags = insert_all([k == 2, k == 3])
        \\    units = insert_all([{}, {}])
        \\    [
        \\        records.contains({ a: k, b: "x" }), records.contains({ a: k, b: "y" }),
        \\        tuples.contains((k, "b")), shapes.contains(Rect(k, 1)), shapes.contains(Rect(k, 2)), shapes.contains(Empty),
        \\        lists.contains([k, k]), lists.contains([]),
        \\        nominals.contains(Tagged.T(k)), flags.contains(False), units.contains({}),
        \\    ]
        \\}
        ,
        .expected = "[True, False, True, True, False, True, True, True, True, True, True]",
        .hashes = &.{ .primitive, .bool, .empty_record, .record, .tuple, .tag_union, .list, .nominal, .transparent, .dynamic },
    },
    .{
        .name = "hash shapes: optional fields hash their presence",
        .source =
        \\{
        \\    pick : U64 -> { a ?: U64, b : U64 }
        \\    pick = |n| if n == 1 { b: n } else { a: n, b: 1 }
        \\    insert_all = |items| Set.from_list(items)
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    picks = insert_all([pick(k), pick(k + 1)])
        \\    (picks.len(), picks.contains(pick(k)), picks.contains(pick(k + 1)), picks.contains(pick(k + 2)))
        \\}
        ,
        .expected = "(2, True, True, False)",
        .hashes = &.{.presence_slot},
    },
    .{
        .name = "hash agreement: Boxy's static and descriptor-driven hashes land in the same bucket",
        .source =
        \\{
        \\    Key : { id : U64, tag : [A, B(Str)] }
        \\    insert_generic = |dict, key, value| dict.insert(key, value)
        \\    lookup_generic = |dict, key| dict.get(key)
        \\    k = {
        \\        dbg 7.U64
        \\        7.U64
        \\    }
        \\    key : Key
        \\    key = { id: k, tag: B("b${k.to_str()}") }
        \\    other : Key
        \\    other = { id: k, tag: A }
        \\    generic_then_static = insert_generic(Dict.empty(), key, "g")
        \\    static_then_generic = Dict.empty().insert(other, "s")
        \\    (generic_then_static.get(key), lookup_generic(static_then_generic, other), lookup_generic(generic_then_static, other))
        \\}
        ,
        .expected = "(Ok(\"g\"), Ok(\"s\"), Err(KeyNotFound))",
        .hashes = &.{ .record, .tag_union, .transparent, .dynamic },
        .compares = &.{.dynamic},
    },

    // Matching: one decision tree per match under both strategies.
    .{
        .name = "match: tags, literals, lists, records, guards, and alternatives",
        .source =
        \\{
        \\    Value : [Num(U64), Text(Str), Items(List(U64)), Pair({ x : U64, y : U64 })]
        \\    classify : Value -> Str
        \\    classify = |value| match value {
        \\        Num(0) => "zero"
        \\        Num(n) if n > 100 => "big"
        \\        Num(_) => "num"
        \\        Text("hi") | Text("hello") => "greeting"
        \\        Text(s) => "text ${s}"
        \\        Items([]) => "no items"
        \\        Items([first, .. as rest]) if rest.len() == 0 => "one item ${first.to_str()}"
        \\        Items([first, .., last]) => "items ${first.to_str()}..${last.to_str()}"
        \\        Items(_) => "other items"
        \\        Pair({ x: 0, y }) => "on axis ${y.to_str()}"
        \\        Pair({ x, y: 0 }) => "on other axis ${x.to_str()}"
        \\        Pair(_) => "pair"
        \\    }
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    [
        \\        classify(Num(k - 1)), classify(Num(k + 200)), classify(Num(k)),
        \\        classify(Text("hello")), classify(Text("x${k.to_str()}")),
        \\        classify(Items([])), classify(Items([k])), classify(Items([k, k + 1, k + 2])),
        \\        classify(Pair({ x: 0, y: k })), classify(Pair({ x: k, y: 0 })), classify(Pair({ x: k, y: k })),
        \\    ]
        \\}
        ,
        .expected = "[\"zero\", \"big\", \"num\", \"greeting\", \"text x1\", \"no items\", \"one item 1\", \"items 1..3\", \"on axis 1\", \"on other axis 1\", \"pair\"]",
    },
    .{
        .name = "match: string patterns, nested unions, and tuples",
        .source =
        \\{
        \\    describe : (Try(Try(U64, Str), Str), Str) -> Str
        \\    describe = |value| match value {
        \\        (Ok(Ok(n)), _) => "ok ok ${n.to_str()}"
        \\        (Ok(Err(_)), "x") => "ok err x"
        \\        (Ok(Err(e)), s) => "ok err ${e} ${s}"
        \\        (Err(_), "prefix ${rest}") => "err with ${rest}"
        \\        (Err(e), _) => "err ${e}"
        \\    }
        \\    k = {
        \\        dbg 4.U64
        \\        4.U64
        \\    }
        \\    [
        \\        describe((Ok(Ok(k)), "")), describe((Ok(Err("bad")), "x")), describe((Ok(Err("bad")), "y")),
        \\        describe((Err("e"), "prefix tail")), describe((Err("e${k.to_str()}"), "other")),
        \\    ]
        \\}
        ,
        .expected = "[\"ok ok 4\", \"ok err x\", \"ok err bad y\", \"err with tail\", \"err e4\"]",
    },
    .{
        .name = "match: generic helpers match values only their descriptors describe",
        .source =
        \\{
        \\    first_ok = |items, fallback| match items {
        \\        [Ok(x), ..] => x
        \\        [Err(_), Ok(x), ..] => x
        \\        _ => fallback
        \\    }
        \\    k = {
        \\        dbg 9.U64
        \\        9.U64
        \\    }
        \\    oks : List(Try(U64, Str))
        \\    oks = [Err("a"), Ok(k)]
        \\    words : List(Try(Str, U64))
        \\    words = [Ok("w${k.to_str()}")]
        \\    (first_ok(oks, 0), first_ok(words, "none"), first_ok([], k))
        \\}
        ,
        .expected = "(9, \"w9\", 9)",
    },
    .{
        .name = "match: a failed guard after custom-conversion literals retries the next row",
        .source =
        \\{
        \\    Word := { code : U64 }.{
        \\        from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
        \\        from_quote = |text| if text == "low" Ok({ code: 1 }) else if text == "high" Ok({ code: 2 }) else Ok({ code: 3 })
        \\        is_eq : Word, Word -> Bool
        \\        is_eq = |a, b| a.code == b.code
        \\    }
        \\    guarded : Word, Word, Bool -> U64
        \\    guarded = |a, b, flag| match (a, b) {
        \\        ("low", "high") if flag => 1
        \\        ("low", "high") => 2
        \\        _ => 3
        \\    }
        \\    k = {
        \\        dbg 1.U64
        \\        1.U64
        \\    }
        \\    low = "low".Word
        \\    high = "high".Word
        \\    (guarded(low, high, k == 1), guarded(low, high, k == 2), guarded(high, low, k == 1))
        \\}
        ,
        .expected = "(1, 2, 3)",
    },
    .{
        .name = "dbg and expect transcripts agree",
        .source =
        \\{
        \\    k = {
        \\        dbg 3.U64
        \\        3.U64
        \\    }
        \\    dbg { k: k, items: [k] }
        \\    expect k == 4
        \\    dbg Ok(k)
        \\    k
        \\}
        ,
        .expected = "3",
    },
};

fn covered(comptime kind: ShapeKind, comptime field: []const u8) bool {
    for (cases) |case| {
        for (@field(case, field)) |covered_kind| {
            if (covered_kind == kind) return true;
        }
    }
    return false;
}

fn exempt(comptime kind: ShapeKind, comptime exemptions: []const Exemption) bool {
    for (exemptions) |exemption| {
        if (exemption.kind == kind) return true;
    }
    return false;
}

comptime {
    @setEvalBranchQuota(100_000);
    for (std.enums.values(ShapeKind)) |kind| {
        if (!covered(kind, "inspects")) {
            @compileError("no strategy differential case inspects a value of shape `" ++ @tagName(kind) ++ "`");
        }
        if (!covered(kind, "compares") and !exempt(kind, &equality_exemptions)) {
            @compileError("no strategy differential case compares values of shape `" ++ @tagName(kind) ++ "`");
        }
        if (!covered(kind, "hashes") and !exempt(kind, &hash_exemptions)) {
            @compileError("no strategy differential case hashes a value of shape `" ++ @tagName(kind) ++ "`");
        }
    }
}

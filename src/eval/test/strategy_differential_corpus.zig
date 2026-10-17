//! The cross-strategy differential harness's own corpus.
//!
//! Most eval cases compute their values during checking finalization, which
//! always uses `.lss`, so neither strategy's runtime value semantics runs for
//! them. These cases read a mutable local (`var $k`), which is never a
//! compile-time dependency, so the values they render, compare, and hash are
//! built at runtime by the strategy under test, and generic helpers make
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
    source_kind: helpers.SourceKind = .module,
    imports: []const helpers.ModuleSource = &.{},
    expected: ?[]const u8 = null,
    inspects: []const ShapeKind = &.{},
    compares: []const ShapeKind = &.{},
    hashes: []const ShapeKind = &.{},
};

/// Shapes a value-semantics operation cannot reach, with the reason.
const Exemption = struct {
    kind: ShapeKind,
    reason: []const u8,
};

const equality_exemptions = [_]Exemption{
    .{ .kind = .function, .reason = "functions have no is_eq; the checker rejects comparing them" },
};

const hash_exemptions = [_]Exemption{
    .{ .kind = .function, .reason = "functions have no to_hash; the checker rejects hashing them" },
};

pub const cases = [_]Case{
    // `to_inspect` override selection, pinned under both strategies.
    .{
        .name = "override: a nominal's procedure to_inspect replaces the default rendering",
        .source =
        \\Color := [Red, Green].{
        \\    to_inspect : Color -> Str
        \\    to_inspect = |color| match color {
        \\        Red => "Color.Red"
        \\        Green => "Color.Green"
        \\    }
        \\}
        \\
        \\show = |value| Str.inspect(value)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    color : Color
        \\    color = if $k == 1 Red else Green
        \\    (Str.inspect(color), show(color), Str.inspect({ c: color }), show([color]))
        \\}
        ,
        .expected = "(\"Color.Red\", \"Color.Red\", \"{ c: Color.Red }\", \"[Color.Red]\")",
        .inspects = &.{ .nominal, .record, .list, .dynamic },
    },
    .{
        .name = "override: a local procedure's to_inspect is not an override",
        .source =
        \\make = |offset| {
        \\    Local := [Local(U64)].{
        \\        to_inspect : Local -> Str
        \\        to_inspect = |Local.Local(n)| "L${(n + offset).to_str()}"
        \\    }
        \\    value = Local.Local(offset)
        \\    (Str.inspect(value), value.to_inspect())
        \\}
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 5
        \\    make($k)
        \\}
        ,
        .expected = "(\"Local(5)\", \"L10\")",
        .inspects = &.{ .nominal, .tag_union },
    },
    .{
        .name = "override: a rejected to_inspect declaration renders the default form",
        .source =
        \\Color := [Red, Green].{
        \\    to_inspect : Color -> Str
        \\    to_inspect = |_color| not_defined_anywhere
        \\}
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    color : Color
        \\    color = if $k == 1 Red else Green
        \\    (Str.inspect(color), Str.inspect([color]))
        \\}
        ,
        .expected = "(\"Red\", \"[Red]\")",
        .inspects = &.{ .nominal, .list },
    },

    // Inspection of every shape, built at runtime.
    .{
        .name = "inspect shapes: primitives, records, tuples, and lists",
        .source =
        \\show = |value| Str.inspect(value)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 2
        \\    record = { name: "n${$k.to_str()}", count: $k, ratio: 1.5.F64, flag: $k == 2 }
        \\    pair = ($k, "two")
        \\    items = [$k, $k + 1]
        \\    [Str.inspect(record), show(record), Str.inspect(pair), show(pair), Str.inspect(items), show(items), show({}), show(())]
        \\}
        ,
        .expected = "[\"{ count: 2, flag: True, name: \\\"n2\\\", ratio: 1.5 }\", \"{ count: 2, flag: True, name: \\\"n2\\\", ratio: 1.5 }\", \"(2, \\\"two\\\")\", \"(2, \\\"two\\\")\", \"[2, 3]\", \"[2, 3]\", \"{}\", \"()\"]",
        .inspects = &.{ .primitive, .bool, .record, .tuple, .list, .empty_record, .dynamic },
    },
    .{
        .name = "inspect shapes: tag unions, boxes, functions, and aliases",
        .source =
        \\Shape : [Circle(U64), Rect(U64, U64), Empty]
        \\
        \\show = |value| Str.inspect(value)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 3
        \\    shapes : List(Shape)
        \\    shapes = [Circle($k), Rect($k, $k + 1), Empty]
        \\    boxed = Box.box($k)
        \\    f = |x| x + $k
        \\    [Str.inspect(shapes), show(shapes), Str.inspect(boxed), show(boxed), Str.inspect(f), show(Ok($k))]
        \\}
        ,
        .expected = "[\"[Circle(3), Rect(3, 4), Empty]\", \"[Circle(3), Rect(3, 4), Empty]\", \"Box(3)\", \"Box(3)\", \"<function>\", \"Ok(3)\"]",
        .inspects = &.{ .tag_union, .box, .function, .transparent, .dynamic },
    },
    .{
        .name = "inspect shapes: optional fields render <missing> or their value",
        .source =
        \\show = |value| Str.inspect(value)
        \\
        \\pick : U64 -> { a ?: U64, b : U64 }
        \\pick = |k| if k == 1 { b: k } else { a: k, b: k }
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    (Str.inspect(pick($k)), show(pick($k + 1)))
        \\}
        ,
        .expected = "(\"{ a: <missing>, b: 1 }\", \"{ a: 2, b: 2 }\")",
        .inspects = &.{.presence_slot},
    },
    .{
        .name = "inspect shapes: opaque nominals hide their backing",
        .source =
        \\Secret :: { key : Str }.{
        \\    new : Str -> Secret
        \\    new = |key| { key: key }
        \\}
        \\
        \\Plain := { key : Str }
        \\
        \\show = |value| Str.inspect(value)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    secret = Secret.new("s${$k.to_str()}")
        \\    plain : Plain
        \\    plain = { key: "p${$k.to_str()}" }
        \\    (Str.inspect(secret), show(secret), Str.inspect(plain), show(plain))
        \\}
        ,
        .expected = "(\"<opaque>\", \"<opaque>\", \"{ key: \\\"p1\\\" }\", \"{ key: \\\"p1\\\" }\")",
        .inspects = &.{.nominal},
    },

    // Structural equality of every shape, at runtime and through generic code.
    .{
        .name = "equality shapes: scalars, records, tuples, unions, lists, boxes",
        .source =
        \\Shape : [Circle(U64), Rect(U64, U64), Empty]
        \\
        \\same = |a, b| a == b
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 2
        \\    r1 = { a: $k, b: "x", c: { d: $k == 2 } }
        \\    r2 = { a: $k, b: "x", c: { d: $k == 3 } }
        \\    s1 : Shape
        \\    s1 = Rect($k, 1)
        \\    s2 : Shape
        \\    s2 = Rect($k, 2)
        \\    [
        \\        same($k, 2), r1 == r1, same(r1, r2), ($k, "t") == ($k, "t"), same(($k, 1), ($k, 2)),
        \\        s1 == s1, same(s1, s2), same(Circle($k), Circle($k)), [$k] == [$k], same([$k], [$k, $k]),
        \\        Box.box($k) == Box.box($k), same(Box.box($k), Box.box(0)), same({}, {}), same($k == 2, True),
        \\    ]
        \\}
        ,
        .expected = "[True, True, False, True, False, True, False, True, True, False, True, False, True, True]",
        .compares = &.{ .primitive, .bool, .empty_record, .record, .tuple, .tag_union, .list, .box, .transparent, .dynamic },
    },
    .{
        .name = "equality shapes: nominals with and without their own is_eq, and optional fields",
        .source =
        \\Loose := [L(U64)].{
        \\    is_eq = |_a, _b| True
        \\}
        \\
        \\Strict := [S(U64)]
        \\
        \\same = |a, b| a == b
        \\
        \\pick : U64 -> { a ?: U64, b : U64 }
        \\pick = |k| if k == 1 { b: k } else { a: k, b: 1 }
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    loose_a : Loose
        \\    loose_a = L($k)
        \\    loose_b : Loose
        \\    loose_b = L($k + 1)
        \\    strict_a : Strict
        \\    strict_a = S($k)
        \\    strict_b : Strict
        \\    strict_b = S($k + 1)
        \\    [loose_a == loose_b, same(loose_a, loose_b), strict_a == strict_b, same(strict_a, strict_a), pick($k) == pick($k), same(pick($k), pick($k + 1))]
        \\}
        ,
        .expected = "[True, True, False, True, True, False]",
        .compares = &.{ .nominal, .presence_slot },
    },

    // Structural hashing of every shape. Hashes are observable through Set
    // and Dict membership; a value inserted by generic code (Boxy's
    // descriptor-driven hash) is found by specialized code (its statically
    // lowered hash) only when both hash it identically.
    .{
        .name = "hash shapes: set membership across generic and specialized hashing",
        .source =
        \\Shape : [Circle(U64), Rect(U64, U64), Empty]
        \\
        \\Tagged := [T(U64)]
        \\
        \\insert_all = |items| Set.from_list(items)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 2
        \\    records = insert_all([{ a: $k, b: "x" }, { a: $k + 1, b: "y" }])
        \\    tuples = insert_all([($k, "a"), ($k, "b")])
        \\    shapes : Set(Shape)
        \\    shapes = insert_all([Circle($k), Rect($k, 1), Empty])
        \\    lists = insert_all([[$k], [$k, $k], []])
        \\    boxes = insert_all([Box.box($k), Box.box($k + 1)])
        \\    nominals = insert_all([Tagged.T($k), Tagged.T($k + 1)])
        \\    flags = insert_all([$k == 2, $k == 3])
        \\    units = insert_all([{}, {}])
        \\    [
        \\        records.contains({ a: $k, b: "x" }), records.contains({ a: $k, b: "y" }),
        \\        tuples.contains(($k, "b")), shapes.contains(Rect($k, 1)), shapes.contains(Rect($k, 2)), shapes.contains(Empty),
        \\        lists.contains([$k, $k]), lists.contains([]), boxes.contains(Box.box($k + 1)),
        \\        nominals.contains(Tagged.T($k)), flags.contains(False), units.contains({}),
        \\    ]
        \\}
        ,
        .expected = "[True, False, True, True, False, True, True, True, True, True, True, True]",
        .hashes = &.{ .primitive, .bool, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .transparent, .dynamic },
    },
    .{
        .name = "hash shapes: optional fields hash their presence",
        .source =
        \\pick : U64 -> { a ?: U64, b : U64 }
        \\pick = |k| if k == 1 { b: k } else { a: k, b: 1 }
        \\
        \\insert_all = |items| Set.from_list(items)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    picks = insert_all([pick($k), pick($k + 1)])
        \\    (picks.len(), picks.contains(pick($k)), picks.contains(pick($k + 1)), picks.contains(pick($k + 2)))
        \\}
        ,
        .expected = "(2, True, True, False)",
        .hashes = &.{.presence_slot},
    },
    .{
        .name = "hash agreement: Boxy's static and descriptor-driven hashes land in the same bucket",
        .source =
        \\Key := { id : U64, tag : [A, B(Str)] }
        \\
        \\insert_generic = |dict, key, value| dict.insert(key, value)
        \\
        \\lookup_generic = |dict, key| dict.get(key)
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 7
        \\    key : Key
        \\    key = { id: $k, tag: B("b${$k.to_str()}") }
        \\    other : Key
        \\    other = { id: $k, tag: A }
        \\    generic_then_static = insert_generic(Dict.empty(), key, "g")
        \\    static_then_generic = Dict.empty().insert(other, "s")
        \\    (generic_then_static.get(key), lookup_generic(static_then_generic, other), lookup_generic(generic_then_static, other))
        \\}
        ,
        .expected = "(Ok(\"g\"), Ok(\"s\"), Err(KeyNotFound))",
        .hashes = &.{ .nominal, .record, .tag_union, .dynamic },
        .compares = &.{.dynamic},
    },

    // Matching: one decision tree per match under both strategies.
    .{
        .name = "match: tags, literals, lists, records, guards, and alternatives",
        .source =
        \\classify = |value| match value {
        \\    Num(0) => "zero"
        \\    Num(n) if n > 100 => "big"
        \\    Num(_) => "num"
        \\    Text("hi") | Text("hello") => "greeting"
        \\    Text(s) => "text ${s}"
        \\    Items([]) => "no items"
        \\    Items([first, .. as rest]) if rest.len() == 0 => "one item ${first.to_str()}"
        \\    Items([first, .., last]) => "items ${first.to_str()}..${last.to_str()}"
        \\    Pair({ x: 0, y }) => "on axis ${y.to_str()}"
        \\    Pair({ x, y: 0 }) => "on other axis ${x.to_str()}"
        \\    Pair(_) => "pair"
        \\}
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 1
        \\    [
        \\        classify(Num($k - 1)), classify(Num($k + 200)), classify(Num($k)),
        \\        classify(Text("hello")), classify(Text("x${$k.to_str()}")),
        \\        classify(Items([])), classify(Items([$k])), classify(Items([$k, $k + 1, $k + 2])),
        \\        classify(Pair({ x: 0, y: $k })), classify(Pair({ x: $k, y: 0 })), classify(Pair({ x: $k, y: $k })),
        \\    ]
        \\}
        ,
        .expected = "[\"zero\", \"big\", \"num\", \"greeting\", \"text x1\", \"no items\", \"one item 1\", \"items 1..3\", \"on axis 1\", \"on other axis 1\", \"pair\"]",
    },
    .{
        .name = "match: string patterns, nested unions, and tuples",
        .source =
        \\describe = |value| match value {
        \\    (Ok(Ok(n)), _) => "ok ok ${n.to_str()}"
        \\    (Ok(Err(_)), "x") => "ok err x"
        \\    (Ok(Err(e)), s) => "ok err ${e} ${s}"
        \\    (Err(_), "prefix ${rest}") => "err with ${rest}"
        \\    (Err(e), _) => "err ${e}"
        \\}
        \\
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 4
        \\    [
        \\        describe((Ok(Ok($k)), "")), describe((Ok(Err("bad")), "x")), describe((Ok(Err("bad")), "y")),
        \\        describe((Err("e"), "prefix tail")), describe((Err("e${$k.to_str()}"), "other")),
        \\    ]
        \\}
        ,
        .expected = "[\"ok ok 4\", \"ok err x\", \"ok err bad y\", \"err with tail\", \"err e4\"]",
    },
    .{
        .name = "dbg and expect transcripts agree",
        .source =
        \\main = {
        \\    var $k = 0.U64
        \\    $k = $k + 3
        \\    dbg { k: $k, items: [$k] }
        \\    expect $k == 4
        \\    dbg Ok($k)
        \\    $k
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

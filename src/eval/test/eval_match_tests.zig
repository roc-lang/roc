//! Cross-engine conformance corpus for match lowering.
//!
//! These cases lock in the semantics of `match` compilation — branch order,
//! guard order, binding visibility, exhaustiveness defaults, and every pattern
//! kind — so the decision-tree match compiler (src/postcheck/match_tree.zig)
//! is verified against the same behavior on all executors.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Match-lowering conformance cases, consumed by the parallel runner.
pub const tests = [_]TestCase{
    // --- Tag dispatch ---
    .{
        .name = "match-dt: four-tag nominal union dispatches every arm",
        .source_kind = .module,
        .source =
        \\Color := [Red, Green, Blue, Yellow]
        \\
        \\rank : Color -> I64
        \\rank = |c| match c {
        \\    Red => 1
        \\    Green => 2
        \\    Blue => 3
        \\    Yellow => 4
        \\}
        \\
        \\main = {
        \\    colors : List(Color)
        \\    colors = [Red, Green, Blue, Yellow]
        \\    List.map(colors, rank)
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3, 4]" },
    },
    .{
        .name = "match-dt: tag payloads destructure through nested tags",
        .source_kind = .module,
        .source =
        \\describe : Try(Try(I64, Str), I64) -> I64
        \\describe = |v| match v {
        \\    Ok(Ok(n)) => n
        \\    Ok(Err(_)) => -1
        \\    Err(code) => code * 10
        \\}
        \\
        \\main = (describe(Ok(Ok(7))), describe(Ok(Err("x"))), describe(Err(4)))
        ,
        .expected = .{ .inspect_str = "(7, -1, 40)" },
    },
    .{
        .name = "match-dt: closed match with no wildcard uses checker exhaustiveness",
        .source_kind = .module,
        .source =
        \\pick : Try(I64, I64) -> I64
        \\pick = |v| match v {
        \\    Ok(a) => a + 1
        \\    Err(b) => b - 1
        \\}
        \\
        \\main = (pick(Ok(10)), pick(Err(10)))
        ,
        .expected = .{ .inspect_str = "(11, 9)" },
    },
    .{
        .name = "match-dt: bool match is exhaustive without wildcard",
        .source_kind = .module,
        .source =
        \\flip : Bool -> Bool
        \\flip = |b| match b {
        \\    True => False
        \\    False => True
        \\}
        \\
        \\main = (flip(True), flip(False))
        ,
        .expected = .{ .inspect_str = "(False, True)" },
    },
    .{
        .name = "match-dt: multi-payload tag binds all payloads in order",
        .source_kind = .module,
        .source =
        \\Shape := [Rect(I64, I64), Circle(I64), Point]
        \\
        \\area_ish : Shape -> I64
        \\area_ish = |s| match s {
        \\    Rect(w, h) => w * h
        \\    Circle(r) => r * r * 3
        \\    Point => 0
        \\}
        \\
        \\main = {
        \\    shapes : List(Shape)
        \\    shapes = [Rect(3, 4), Circle(2), Point]
        \\    List.map(shapes, area_ish)
        \\}
        ,
        .expected = .{ .inspect_str = "[12, 12, 0]" },
    },
    .{
        .name = "match-dt: zero-payload two-tag union dispatch",
        .source_kind = .module,
        .source =
        \\Toggle := [On, Off]
        \\
        \\to_num : Toggle -> I64
        \\to_num = |t| match t {
        \\    On => 1
        \\    Off => 0
        \\}
        \\
        \\main = (to_num(On), to_num(Off))
        ,
        .expected = .{ .inspect_str = "(1, 0)" },
    },

    // --- Guards ---
    .{
        .name = "match-dt: guards fire in source order",
        .source_kind = .module,
        .source =
        \\classify : I64 -> I64
        \\classify = |n| match n {
        \\    x if x > 10 => x * 100
        \\    x if x > 5 => x * 10
        \\    x => x
        \\}
        \\
        \\main = List.map([12, 7, 3], classify)
        ,
        .expected = .{ .inspect_str = "[1200, 70, 3]" },
    },
    .{
        .name = "match-dt: guard failure falls through to same-constructor later branch",
        .source_kind = .module,
        .source =
        \\pick : Try(I64, Str), Bool -> I64
        \\pick = |v, flag| match v {
        \\    Ok(n) if flag => n * 10
        \\    Ok(n) => n
        \\    Err(_) => -1
        \\}
        \\
        \\main = (pick(Ok(5), True), pick(Ok(5), False), pick(Err("e"), True))
        ,
        .expected = .{ .inspect_str = "(50, 5, -1)" },
    },
    .{
        .name = "match-dt: guard sees payload bindings",
        .source_kind = .module,
        .source =
        \\f : Try((I64, I64), {}) -> I64
        \\f = |v| match v {
        \\    Ok((a, b)) if a + b == 10 => a * b
        \\    Ok((a, _)) => a
        \\    Err({}) => -1
        \\}
        \\
        \\main = (f(Ok((4, 6))), f(Ok((4, 7))), f(Err({})))
        ,
        .expected = .{ .inspect_str = "(24, 4, -1)" },
    },
    .{
        .name = "match-dt: wildcard guard row between constructor rows",
        .source_kind = .module,
        .source =
        \\Color := [Red, Green, Blue]
        \\
        \\f : Color, Bool -> I64
        \\f = |c, flag| match c {
        \\    Red => 1
        \\    _ if flag => 0
        \\    Blue => 3
        \\    _ => 9
        \\}
        \\
        \\main = {
        \\    a = f(Red, False)
        \\    b = f(Blue, True)
        \\    c = f(Blue, False)
        \\    d = f(Green, False)
        \\    (a, b, c, d)
        \\}
        ,
        .expected = .{ .inspect_str = "(1, 0, 3, 9)" },
    },

    // --- Int / Dec / Frac literals ---
    .{
        .name = "match-dt: negative and boundary I8 literal patterns",
        .source_kind = .module,
        .source =
        \\f : I8 -> I64
        \\f = |n| match n {
        \\    -1 => 10
        \\    -128 => 20
        \\    127 => 30
        \\    _ => 0
        \\}
        \\
        \\main = {
        \\    ns : List(I8)
        \\    ns = [-1, -128, 127, 5]
        \\    List.map(ns, f)
        \\}
        ,
        .expected = .{ .inspect_str = "[10, 20, 30, 0]" },
    },
    .{
        .name = "match-dt: negative I32 and high-bit U64 literal patterns",
        .source_kind = .module,
        .source =
        \\g : I32 -> I64
        \\g = |n| match n {
        \\    -5 => 1
        \\    -2147483648 => 2
        \\    _ => 3
        \\}
        \\
        \\h : U64 -> I64
        \\h = |n| match n {
        \\    18446744073709551615 => 1
        \\    0 => 2
        \\    _ => 3
        \\}
        \\
        \\main = {
        \\    gs : List(I32)
        \\    gs = [-5, -2147483648, 7]
        \\    hs : List(U64)
        \\    hs = [18446744073709551615, 0, 42]
        \\    (List.map(gs, g), List.map(hs, h))
        \\}
        ,
        .expected = .{ .inspect_str = "([1, 2, 3], [1, 2, 3])" },
    },
    .{
        .name = "match-dt: I128 literal patterns fall back to wide equality",
        .source_kind = .module,
        .source =
        \\f : I128 -> I64
        \\f = |n| match n {
        \\    170141183460469231731687303715884105727 => 1
        \\    -170141183460469231731687303715884105728 => 2
        \\    0 => 3
        \\    _ => 4
        \\}
        \\
        \\main = {
        \\    ns : List(I128)
        \\    ns = [170141183460469231731687303715884105727, -170141183460469231731687303715884105728, 0, 9]
        \\    List.map(ns, f)
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3, 4]" },
    },
    // Fractional literal patterns: plain F32/F64 fractional literal patterns are
    // rejected by the checker, and small Dec fractional literal patterns hit a
    // pre-existing numeric-finalization panic ("small decimal pattern reached
    // Monotype after numeric finalization"). Long-form Dec literals (beyond the
    // small-dec form) are the reachable dec_lit path, so that is what we cover.
    .{
        .name = "match-dt: Dec long-form literal patterns use wide equality",
        .source_kind = .module,
        .source =
        \\f : Dec -> I64
        \\f = |n| match n {
        \\    1.000000000000000001 => 1
        \\    -2.000000000000000002 => 2
        \\    _ => 3
        \\}
        \\
        \\main = {
        \\    ns : List(Dec)
        \\    ns = [1.000000000000000001, -2.000000000000000002, 1.0]
        \\    List.map(ns, f)
        \\}
        ,
        .expected = .{ .inspect_str = "[1, 2, 3]" },
    },

    // --- Strings ---
    .{
        .name = "match-dt: adjacent string literal and interpolation arms dispatch in order",
        .source_kind = .module,
        .source =
        \\route : Str -> Str
        \\route = |s| match s {
        \\    "exact" => "lit"
        \\    "pre${rest}" => Str.concat("pre:", rest)
        \\    "${_}.txt" => "txt"
        \\    _ => "miss"
        \\}
        \\
        \\main = (route("exact"), route("prefix"), route("a.txt"), route("nope"))
        ,
        .expected = .{ .inspect_str = "(\"lit\", \"pre:fix\", \"txt\", \"miss\")" },
    },
    .{
        .name = "match-dt: string capture binding visible in guard",
        .source_kind = .module,
        .source =
        \\route : Str -> Str
        \\route = |s| match s {
        \\    "v${n}" if n == "1" => "one"
        \\    "v${n}" => Str.concat("v:", n)
        \\    _ => "miss"
        \\}
        \\
        \\main = (route("v1"), route("v2"), route("x"))
        ,
        .expected = .{ .inspect_str = "(\"one\", \"v:2\", \"miss\")" },
    },

    // --- Records and tuples ---
    .{
        .name = "match-dt: record fields with literal sub-patterns select by field values",
        .source_kind = .module,
        .source =
        \\f : { x : I64, y : I64 } -> I64
        \\f = |r| match r {
        \\    { x: 1, y } => y
        \\    { x, y: 2 } => x * 10
        \\    { x, y } => x + y
        \\}
        \\
        \\main = (f({ x: 1, y: 5 }), f({ x: 7, y: 2 }), f({ x: 3, y: 4 }))
        ,
        .expected = .{ .inspect_str = "(5, 70, 7)" },
    },
    .{
        .name = "match-dt: tuple literal positions tried in branch order",
        .source_kind = .module,
        .source =
        \\f : (I64, I64) -> I64
        \\f = |t| match t {
        \\    (1, a) => a
        \\    (a, 1) => a * 10
        \\    (a, b) => a + b
        \\}
        \\
        \\main = (f((1, 9)), f((8, 1)), f((2, 3)), f((1, 1)))
        ,
        .expected = .{ .inspect_str = "(9, 80, 5, 1)" },
    },
    .{
        .name = "match-dt: nested record inside tag inside tuple",
        .source_kind = .module,
        .source =
        \\f : (Try({ a : I64, b : I64 }, {}), I64) -> I64
        \\f = |t| match t {
        \\    (Ok({ a: 1, b }), n) => b + n
        \\    (Ok({ a, b: 2 }), _) => a
        \\    (Ok({ a, b }), n) => a + b + n
        \\    (Err({}), n) => n
        \\}
        \\
        \\main = {
        \\    r1 = f((Ok({ a: 1, b: 10 }), 100))
        \\    r2 = f((Ok({ a: 7, b: 2 }), 100))
        \\    r3 = f((Ok({ a: 3, b: 4 }), 100))
        \\    r4 = f((Err({}), 100))
        \\    (r1, r2, r3, r4)
        \\}
        ,
        .expected = .{ .inspect_str = "(110, 7, 107, 100)" },
    },

    // --- Lists ---
    .{
        .name = "match-dt: list exact-length buckets",
        .source_kind = .module,
        .source =
        \\f : List(I64) -> I64
        \\f = |xs| match xs {
        \\    [] => 0
        \\    [a] => a
        \\    [a, b] => a * b
        \\    _ => -1
        \\}
        \\
        \\main = List.map([[], [5], [3, 4], [1, 2, 3]], f)
        ,
        .expected = .{ .inspect_str = "[0, 5, 12, -1]" },
    },
    .{
        .name = "match-dt: rest patterns front middle and back",
        .source_kind = .module,
        .source =
        \\f : List(I64) -> I64
        \\f = |xs| match xs {
        \\    [first, .., last] if first == last => 100
        \\    [.., last] if last > 50 => last
        \\    [first, ..] => first
        \\    [] => 0
        \\}
        \\
        \\main = List.map([[7, 1, 7], [1, 99], [5, 6], []], f)
        ,
        .expected = .{ .inspect_str = "[100, 99, 5, 0]" },
    },
    .{
        .name = "match-dt: rest capture binds the middle slice",
        .source_kind = .module,
        .source =
        \\f : List(I64) -> List(I64)
        \\f = |xs| match xs {
        \\    [_, .. as mid, _] => mid
        \\    _ => []
        \\}
        \\
        \\main = (f([1, 2, 3, 4]), f([1, 2]), f([1]))
        ,
        .expected = .{ .inspect_str = "([2, 3], [], [])" },
    },
    .{
        .name = "match-dt: specific-length row wins over shorter rest row listed later",
        .source_kind = .module,
        .source =
        \\f : List(I64) -> I64
        \\f = |xs| match xs {
        \\    [x, y, ..] => x + y
        \\    [x, ..] => x * 10
        \\    _ => 0
        \\}
        \\
        \\main = List.map([[1, 2, 9], [5], []], f)
        ,
        .expected = .{ .inspect_str = "[3, 50, 0]" },
    },
    .{
        .name = "match-dt: list elements destructure nested tags",
        .source_kind = .module,
        .source =
        \\f : List(Try(I64, {})) -> I64
        \\f = |xs| match xs {
        \\    [Ok(a), Ok(b)] => a + b
        \\    [Ok(a), Err({})] => a
        \\    [Err({}), ..] => -1
        \\    _ => 0
        \\}
        \\
        \\main = {
        \\    r1 = f([Ok(3), Ok(4)])
        \\    r2 = f([Ok(3), Err({})])
        \\    r3 = f([Err({}), Ok(9)])
        \\    r4 = f([Ok(1)])
        \\    (r1, r2, r3, r4)
        \\}
        ,
        .expected = .{ .inspect_str = "(7, 3, -1, 0)" },
    },

    // --- as-patterns ---
    .{
        .name = "match-dt: as-pattern binds whole value alongside payload",
        .source_kind = .module,
        .source =
        \\f : Try(I64, {}) -> (I64, Try(I64, {}))
        \\f = |v| match v {
        \\    Ok(n) as whole if n > 5 => (n, whole)
        \\    Ok(n) as whole => (n * 10, whole)
        \\    Err({}) as whole => (0, whole)
        \\}
        \\
        \\main = (f(Ok(9)), f(Ok(2)), f(Err({})))
        ,
        .expected = .{ .inspect_str = "((9, Ok(9)), (20, Ok(2)), (0, Err({})))" },
    },

    // --- Nominal patterns (PR 9849 shapes) ---
    .{
        .name = "match-dt: nominal record with declared order differing from backing order",
        .source_kind = .module,
        .source =
        \\P := { y : U8, x : U64 }
        \\
        \\get : P -> U64
        \\get = |p| match p {
        \\    P.({ y: 1, x }) => x
        \\    P.({ y, x: 2 }) => U8.to_u64(y) * 100
        \\    P.({ y, x }) => x + U8.to_u64(y)
        \\}
        \\
        \\main = {
        \\    a : P
        \\    a = { y: 1, x: 77 }
        \\    b : P
        \\    b = { y: 9, x: 2 }
        \\    c : P
        \\    c = { y: 3, x: 40 }
        \\    (get(a), get(b), get(c))
        \\}
        ,
        .expected = .{ .inspect_str = "(77, 900, 43)" },
    },
    .{
        .name = "match-dt: nominal single-field wrapper unwraps in pattern",
        .source_kind = .module,
        .source =
        \\Distance := U64
        \\
        \\unwrap : Distance -> U64
        \\unwrap = |d| match d {
        \\    Distance.(n) => n
        \\}
        \\
        \\main = unwrap(Distance.(42))
        ,
        .expected = .{ .inspect_str = "42" },
    },

    // --- Match result feeding a larger expression (shared continuation) ---
    .{
        .name = "match-dt: match result flows into surrounding arithmetic",
        .source_kind = .module,
        .source =
        \\f : Try(I64, {}) -> I64
        \\f = |v| 1 + (match v {
        \\    Ok(n) => n * 2
        \\    Err({}) => -10
        \\}) * 10
        \\
        \\main = (f(Ok(3)), f(Err({})))
        ,
        .expected = .{ .inspect_str = "(61, -99)" },
    },

    // --- Exhaustiveness diagnostics ---
    .{
        .name = "match-dt: non-exhaustive tag match is a checker problem",
        .source_kind = .module,
        .source =
        \\f : Try(I64, Str) -> I64
        \\f = |v| match v {
        \\    Ok(n) => n
        \\}
        \\
        \\main = f(Ok(1))
        ,
        .expected = .{ .problem = {} },
    },

    // Regression coverage for the chain's string-group hazard: a guard failure
    // inside a string arm must retry later branches with the same string
    // pattern in source order (the deleted group special case jumped past the
    // whole group instead).
    .{
        .name = "match-dt: guard failure retries later branch with same string pattern",
        .source_kind = .module,
        .source =
        \\route : Str, Bool -> Str
        \\route = |s, flag| match s {
        \\    "${a}!" if flag => Str.concat("g:", a)
        \\    "${b}!" => Str.concat("n:", b)
        \\    _ => "miss"
        \\}
        \\
        \\main = (route("hi!", True), route("hi!", False), route("hi", True))
        ,
        .expected = .{ .inspect_str = "(\"g:hi\", \"n:hi\", \"miss\")" },
    },
};

//! Generated sweep corpus for the Lambda Mono differential harness.
//!
//! These programs need no expected-output strings: the harness compares two
//! independent executions of each program (LIR interpreter vs Lambda Mono
//! tree evaluator, plus dev-backend agreement). The sweep dimensions target
//! the direct solved-to-LIR lowering's body-level hazards:
//!
//!   - capture count (0, 1, 2, 8) and capture packing order
//!   - capture types (scalar, heap str, closure-in-closure, recursive
//!     closure, closure reached through a data structure)
//!   - lambda-set size (1, 2, 5) and function values in containers
//!   - match shapes (guards with retry, wildcards, string patterns with
//!     exact and tail ends, list patterns with and without rests, nested
//!     nominals, literal patterns)
//!   - argument/field/element evaluation order probed with dbg
//!   - `?` try sequencing, loops over vars, recursive box-backed nominals

/// One generated program: a name for reporting plus its source text.
pub const Case = struct {
    name: []const u8,
    source: []const u8,
    /// True for module-style sources (with a `main =` definition); false for
    /// bare expressions.
    module: bool = false,
    /// True for programs that currently panic the compiler (a pre-existing
    /// monotype-instantiation invariant, "instantiation unified two different
    /// primitive types", when calling a capture-carrying closure extracted
    /// from a container). Kept in the sweep so the harness reports them
    /// loudly; they stop counting as expected once the compiler bug is fixed.
    known_panic: bool = false,
};

/// All generated sweep cases.
pub const cases = [_]Case{
    // Capture-count sweep
    .{
        .name = "gen: capture count 0",
        .source =
        \\{
        \\    f = |x| x + 1.I64
        \\    f(41.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capture count 1 scalar",
        .source =
        \\{
        \\    c1 = 10.I64
        \\    f = |x| x + c1
        \\    f(1.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capture count 2 scalars used in declaration order",
        .source =
        \\{
        \\    c1 = 10.I64
        \\    c2 = 200.I64
        \\    f = |x| x + c1 + c2
        \\    f(1.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capture count 2 scalars used in reverse order",
        .source =
        \\{
        \\    c1 = 3.I64
        \\    c2 = 5.I64
        \\    f = |x| x * c2 * 100 + c1
        \\    f(1.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capture count 8 scalars mixed order",
        .source =
        \\{
        \\    c1 = 1.I64
        \\    c2 = 2.I64
        \\    c3 = 3.I64
        \\    c4 = 4.I64
        \\    c5 = 5.I64
        \\    c6 = 6.I64
        \\    c7 = 7.I64
        \\    c8 = 8.I64
        \\    f = |x| x + c8 * 10000000 + c1 * 1000000 + c7 * 100000 + c2 * 10000 + c6 * 1000 + c3 * 100 + c5 * 10 + c4
        \\    f(0.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capture count 8 distinct widths and kinds",
        .source =
        \\{
        \\    a = 1.U8
        \\    b = 2.I16
        \\    c = 3.U32
        \\    d = 4.I64
        \\    e = "five"
        \\    f = 6.5.F64
        \\    g = True
        \\    h = [7.I64, 8.I64]
        \\    mk = |x| (a, b, c, d, e, f, g, h, x)
        \\    mk(9.I64)
        \\}
        ,
    },

    // Capture-type sweep
    .{
        .name = "gen: heap str captures concat order",
        .source =
        \\{
        \\    prefix = "pre-"
        \\    suffix = "-post"
        \\    f = |x| Str.concat(prefix, Str.concat(x, suffix))
        \\    f("mid")
        \\}
        ,
    },
    .{
        .name = "gen: closure captured by closure",
        .source =
        \\{
        \\    base = 100.I64
        \\    outer = |a| {
        \\        inner = |b| a + b + base
        \\        inner
        \\    }
        \\    add = outer(20.I64)
        \\    add(3.I64)
        \\}
        ,
    },
    .{
        .name = "gen: recursive closure with capture",
        .source =
        \\{
        \\    step = 2.I64
        \\    count = |n| if n > 0 { step + count(n - 1) } else { 0.I64 }
        \\    count(5.I64)
        \\}
        ,
    },
    .{
        .name = "gen: closure reached through a tuple",
        .source =
        \\{
        \\    k = 7.I64
        \\    pair = (|x| x + k, |x| x * k)
        \\    match pair {
        \\        (add, mul) => add(1.I64) * 1000 + mul(2.I64)
        \\    }
        \\}
        ,
    },
    .{
        .name = "gen: closures stored in a list",
        .source =
        \\{
        \\    k = 3.I64
        \\    fs = [|x| x + k, |x| x * k, |x| x - k]
        \\    match fs {
        \\        [f, g, h] => f(10.I64) * 10000 + g(10.I64) * 100 + h(10.I64)
        \\        _ => -1.I64
        \\    }
        \\}
        ,
    },
    .{
        .name = "gen: closure stored in a record field",
        .source =
        \\{
        \\    k = 11.I64
        \\    r = { op: |x| x * k, tag: "times" }
        \\    (r.op)(4.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capturing closure called via List.first",
        .source =
        \\{
        \\    k = 3.I64
        \\    fs = [|x| x + k, |x| x * k]
        \\    match List.first(fs) {
        \\        Ok(f) => f(10.I64)
        \\        Err(_) => -1.I64
        \\    }
        \\}
        ,
    },
    .{
        .name = "gen: capturing closure rebound from record field",
        .source =
        \\{
        \\    k = 11.I64
        \\    r = { op: |x| x * k, tag: "times" }
        \\    op = r.op
        \\    op(4.I64)
        \\}
        ,
    },
    .{
        .name = "gen: capturing closures applied from tuple positions",
        .source =
        \\{
        \\    k = 7.I64
        \\    apply_both = |fns, x| match fns {
        \\        (f, g) => f(x) * 1000.I64 + g(x)
        \\    }
        \\    apply_both((|x| x + k, |x| x * k), 2.I64)
        \\}
        ,
    },

    // Lambda-set size sweep
    .{
        .name = "gen: lambda set size 2 dispatch both ways",
        .source =
        \\{
        \\    pick = |flag| if flag { |x| x + 1.I64 } else { |x| x * 2.I64 }
        \\    f = pick(True)
        \\    g = pick(False)
        \\    (f(10.I64), g(10.I64))
        \\}
        ,
    },
    .{
        .name = "gen: lambda set size 5 distinct captures",
        .source =
        \\{
        \\    a = 1.I64
        \\    b = 2.I64
        \\    c = 3.I64
        \\    d = 4.I64
        \\    pick = |n| match n {
        \\        1 => |x| x + a
        \\        2 => |x| x + b + b
        \\        3 => |x| x + c * 3
        \\        4 => |x| x + d * d
        \\        _ => |x| x
        \\    }
        \\    (pick(1.I64))(100.I64) + (pick(2.I64))(200.I64) + (pick(3.I64))(300.I64) + (pick(4.I64))(400.I64) + (pick(9.I64))(500.I64)
        \\}
        ,
    },
    .{
        .name = "gen: higher-order apply with two different closures",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 5.I64
        \\    b = 7.I64
        \\    (apply(|x| x + a, 1.I64), apply(|x| x * b, 2.I64))
        \\}
        ,
    },

    // Match shapes
    .{
        .name = "gen: guard failure falls through to later branch",
        .source =
        \\{
        \\    classify = |n, flag| match n {
        \\        x if flag and x > 10 => "big-flagged"
        \\        x if x > 10 => "big"
        \\        _ => "small"
        \\    }
        \\    (classify(20.I64, True), classify(20.I64, False), classify(2.I64, True))
        \\}
        ,
    },
    .{
        .name = "gen: branch order with overlapping int literals",
        .source =
        \\{
        \\    f = |n| match n {
        \\        1 => "one"
        \\        2 => "two"
        \\        _ => "many"
        \\    }
        \\    (f(1.I64), f(2.I64), f(3.I64))
        \\}
        ,
    },
    .{
        .name = "gen: list pattern exact lengths",
        .source =
        \\{
        \\    f = |l| match l {
        \\        [] => 0.I64
        \\        [a] => a
        \\        [a, b] => a * 10 + b
        \\        _ => -1.I64
        \\    }
        \\    (f([]), f([5.I64]), f([1.I64, 2.I64]), f([1.I64, 2.I64, 3.I64]))
        \\}
        ,
    },
    .{
        .name = "gen: list pattern front rest and back",
        .source =
        \\{
        \\    f = |l| match l {
        \\        [first, .. as mid, last] => (first, mid, last)
        \\        _ => (0.I64, [], 0.I64)
        \\    }
        \\    f([1.I64, 2.I64, 3.I64, 4.I64])
        \\}
        ,
    },
    .{
        .name = "gen: list pattern bare rest binds nothing",
        .source =
        \\{
        \\    f = |l| match l {
        \\        [a, ..] => a
        \\        [] => -1.I64
        \\    }
        \\    (f([9.I64, 8.I64, 7.I64]), f([]))
        \\}
        ,
    },
    .{
        .name = "gen: list rest capture whole list",
        .source =
        \\{
        \\    f = |l| match l {
        \\        [.. as whole] => whole
        \\    }
        \\    f([1.I64, 2.I64, 3.I64])
        \\}
        ,
    },
    .{
        .name = "gen: string pattern exact end",
        .source =
        \\{
        \\    f = |s| match s {
        \\        "${a}-${b}" => Str.concat(a, Str.concat("|", b))
        \\        _ => "miss"
        \\    }
        \\    (f("x-y"), f("xy"))
        \\}
        ,
    },
    .{
        .name = "gen: string pattern tail rest",
        .source =
        \\{
        \\    f = |s| match s {
        \\        "cmd:${rest}" => rest
        \\        _ => "miss"
        \\    }
        \\    (f("cmd:run"), f("nope"))
        \\}
        ,
    },
    .{
        .name = "gen: string pattern guard retry same pattern",
        .source =
        \\{
        \\    route = |s, flag| match s {
        \\        "${a}!" if flag => Str.concat("g:", a)
        \\        "${b}!" => Str.concat("n:", b)
        \\        _ => "miss"
        \\    }
        \\    (route("hi!", True), route("hi!", False), route("hi", True))
        \\}
        ,
    },
    .{
        .name = "gen: nested nominal tag payloads",
        .source =
        \\Wrap := [W(I64), D(Inner)]
        \\Inner := [In(I64, I64)]
        \\
        \\unwrap : Wrap -> I64
        \\unwrap = |w| match w {
        \\    W(n) => n
        \\    D(In(a, b)) => a * 100 + b
        \\}
        \\
        \\main = (unwrap(W(5)), unwrap(D(In(2, 3))))
        ,
        .module = true,
    },
    .{
        .name = "gen: record destructure with nested tag",
        .source =
        \\{
        \\    f = |r| match r {
        \\        { kind: Ok(v), count } => v + count
        \\        { kind: Err(_), count } => count
        \\    }
        \\    (f({ kind: Ok(10.I64), count: 1.I64 }), f({ kind: Err("x"), count: 2.I64 }))
        \\}
        ,
    },
    .{
        .name = "gen: as-pattern binds whole and part",
        .source =
        \\{
        \\    f = |l| match l {
        \\        [first, ..] as whole => (first, whole.len())
        \\        [] as empty => (0.I64, empty.len())
        \\    }
        \\    (f([4.I64, 5.I64]), f([]))
        \\}
        ,
    },

    // Evaluation-order probes (dbg transcript ordering)
    .{
        .name = "gen: argument evaluation order",
        .source =
        \\{
        \\    l = |name, v| {
        \\        dbg name
        \\        v
        \\    }
        \\    add3 = |a, b, c| a + b + c
        \\    add3(l("a", 1.I64), l("b", 2.I64), l("c", 3.I64))
        \\}
        ,
    },
    .{
        .name = "gen: record field evaluation order",
        .source =
        \\{
        \\    l = |name, v| {
        \\        dbg name
        \\        v
        \\    }
        \\    r = { zed: l("zed", 1.I64), alpha: l("alpha", 2.I64), mid: l("mid", 3.I64) }
        \\    r.alpha * 100 + r.mid * 10 + r.zed
        \\}
        ,
    },
    .{
        .name = "gen: list element evaluation order",
        .source =
        \\{
        \\    l = |name, v| {
        \\        dbg name
        \\        v
        \\    }
        \\    [l("first", 1.I64), l("second", 2.I64), l("third", 3.I64)]
        \\}
        ,
    },
    .{
        .name = "gen: tag payload evaluation order",
        .source =
        \\{
        \\    l = |name, v| {
        \\        dbg name
        \\        v
        \\    }
        \\    Pair(l("left", 1.I64), l("right", 2.I64))
        \\}
        ,
    },
    .{
        .name = "gen: nested call evaluation order",
        .source =
        \\{
        \\    l = |name, v| {
        \\        dbg name
        \\        v
        \\    }
        \\    sub = |a, b| a - b
        \\    sub(sub(l("x", 10.I64), l("y", 3.I64)), l("z", 2.I64))
        \\}
        ,
    },
    .{
        .name = "gen: dbg between statements",
        .source =
        \\{
        \\    a = 1.I64
        \\    dbg "after-a"
        \\    b = a + 1
        \\    dbg "after-b"
        \\    (a, b)
        \\}
        ,
    },

    // Try sequencing and loops
    .{
        .name = "gen: try sequence ok and err paths",
        .source =
        \\{
        \\    parse_pair = |a, b| {
        \\        x = I64.from_str(a)?
        \\        y = I64.from_str(b)?
        \\        Ok(x * 100 + y)
        \\    }
        \\    (parse_pair("1", "2"), parse_pair("1", "nope"), parse_pair("bad", "2"))
        \\}
        ,
    },
    .{
        .name = "gen: for loop accumulates through var",
        .source =
        \\{
        \\    var $sum = 0.I64
        \\    for item in [1.I64, 2.I64, 3.I64, 4.I64] {
        \\        $sum = $sum * 10 + item
        \\    }
        \\    $sum
        \\}
        ,
    },
    .{
        .name = "gen: nested for loops with capture",
        .source =
        \\{
        \\    base = 1.I64
        \\    var $acc = 0.I64
        \\    for i in [1.I64, 2.I64] {
        \\        for j in [10.I64, 20.I64] {
        \\            $acc = $acc + i * j + base
        \\        }
        \\    }
        \\    $acc
        \\}
        ,
    },
    .{
        .name = "gen: recursive box-backed nominal tree fold",
        .source =
        \\Tree := [Leaf(I64), Node(Tree, Tree)]
        \\
        \\sum : Tree -> I64
        \\sum = |t| match t {
        \\    Leaf(n) => n
        \\    Node(l, r) => sum(l) + sum(r)
        \\}
        \\
        \\main = sum(Node(Node(Leaf(1), Leaf(2)), Leaf(3)))
        ,
        .module = true,
    },
    .{
        .name = "gen: mixed capture closure inside match branch",
        .source =
        \\{
        \\    scale = 3.I64
        \\    f = |t| match t {
        \\        Ok(n) => {
        \\            g = |m| m * scale + n
        \\            g(10.I64)
        \\        }
        \\        Err(s) => if s == "four" { 4.I64 } else { 0.I64 }
        \\    }
        \\    (f(Ok(1.I64)), f(Err("four")))
        \\}
        ,
    },
    .{
        .name = "gen: crash on unreachable branch not taken",
        .source =
        \\{
        \\    f = |n| if n > 0 { n } else { crash "unreachable in test" }
        \\    f(5.I64)
        \\}
        ,
    },
    .{
        .name = "gen: crash message agreement",
        .source =
        \\{
        \\    f = |n| if n > 100 { crash "boom at ${n.to_str()}" } else { n }
        \\    f(101.I64)
        \\}
        ,
    },
    .{
        .name = "gen: integer overflow crash agreement",
        .source =
        \\{
        \\    f = |x| x + 1.I64
        \\    f(9223372036854775807.I64)
        \\}
        ,
    },
    .{
        .name = "gen: division by zero crash agreement",
        .source =
        \\{
        \\    f = |a, b| a // b
        \\    f(10.I64, 0.I64)
        \\}
        ,
    },
    .{
        .name = "gen: dec division by zero agreement",
        .source =
        \\{
        \\    f = |a, b| a / b
        \\    f(1.0.Dec, 0.0.Dec)
        \\}
        ,
    },
    .{
        .name = "gen: dec division result agreement",
        .source =
        \\{
        \\    f = |a, b| a / b
        \\    f(7.5.Dec, 2.5.Dec)
        \\}
        ,
    },
    .{
        .name = "gen: string interpolation with captured values",
        .source =
        \\{
        \\    name = "world"
        \\    count = 3.I64
        \\    greet = |prefix| "${prefix}, ${name} x${count.to_str()}"
        \\    greet("hello")
        \\}
        ,
    },
    .{
        .name = "gen: tuple access after closure call",
        .source =
        \\{
        \\    k = 2.I64
        \\    mk = |x| (x, x * k, x * k * k)
        \\    t = mk(3.I64)
        \\    match t {
        \\        (a, b, c) => a + b * 10 + c * 100
        \\    }
        \\}
        ,
    },
};

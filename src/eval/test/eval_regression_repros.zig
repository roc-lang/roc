//! Eval regression tests originally found during eval regression work.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Eval regression cases.
pub const tests = [_]TestCase{
    .{
        .name = "regression B004: polymorphic function inside tuple constant is concretely sealed",
        .source_kind = .module,
        .source =
        \\t = (|x| x, 0)
        \\
        \\main = (t.0)(1.I64)
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "regression B005: signed min-int division overflow crashes",
        .source = "(-128.I8) // -1.I8",
        .expected = .{ .crash = {} },
    },
    .{
        .name = "regression B006: signed min-int negate overflow crashes",
        .source = "I8.negate(-128)",
        .expected = .{ .crash = {} },
    },
    .{
        .name = "regression B007: Dec suffix preserves exact decimal source text",
        .source = "100000.0.Dec == 100000.Dec",
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "regression B008: out-of-range Dec suffix literal is rejected",
        .source = "170141183460469231732.0.Dec",
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B009: inferred out-of-range Dec literal is rejected",
        .source_kind = .module,
        .source =
        \\main : Dec
        \\main = 170141183460469231732.0
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B010: Dec multiplication overflow is a Roc crash",
        .source = "170141183460469231731.0.Dec * 2.0.Dec",
        .expected = .{ .crash = {} },
        .skip = .{ .llvm = true },
    },
    .{
        .name = "regression B012: F64.to_i8_try has a runtime body",
        .source = "F64.to_i8_try(42.0)",
        .expected = .{ .inspect_str = "Ok(42)" },
    },
    .{
        .name = "regression B013: F64.to_i8_wrap wraps out-of-range inputs",
        .source = "F64.to_i8_wrap(128.0)",
        .expected = .{ .inspect_str = "-128" },
        .skip = .{ .llvm = true },
    },
    .{
        .name = "regression B018: duplicate tag names in nominal declarations are rejected",
        .source_kind = .module,
        .source =
        \\Foo := [A, A]
        \\
        \\main = 1
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B019: duplicate record fields in nominal declarations are rejected",
        .source_kind = .module,
        .source =
        \\Foo := { x : U8, x : U16 }
        \\
        \\main = 1
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B021: duplicate type parameters are rejected",
        .source_kind = .module,
        .source =
        \\F(a, a) : a
        \\
        \\main : F(I64, Str)
        \\main = "x"
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B022: ellipsis expression crashes when reached",
        .source_kind = .module,
        .source =
        \\main = ...
        ,
        .expected = .{ .crash = {} },
    },
    .{
        .name = "regression B024: nested closure captures enclosing lambda parameter",
        .source_kind = .module,
        .source =
        \\main = (|x| {
        \\    g = || x
        \\    g()
        \\})(1.I64)
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "regression B026: local partial record destructuring preserves row fields",
        .source_kind = .module,
        .source =
        \\main = {
        \\    r = { a: 1.I64, b: 2.I64 }
        \\    f = |{ a, .. }| a
        \\    f(r)
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "regression B028: compile-time list-pattern failure becomes Roc crash",
        .source_kind = .module,
        .source =
        \\main : I64
        \\main = (|[a]| a)([])
        ,
        .expected = .{ .crash = {} },
    },
    .{
        .name = "regression B029: unannotated crash constant becomes Roc crash",
        .source_kind = .module,
        .source =
        \\main = {
        \\    crash "x"
        \\}
        ,
        .expected = .{ .crash = {} },
    },
    .{
        .name = "regression B030: out-of-range I8 literal is rejected",
        .source_kind = .module,
        .source =
        \\main : I8
        \\main = 128
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B031: very large decimal fraction literal is rejected",
        .source = "999999999999999999999999999999999999999.0 == 0.0",
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B035: out-of-bounds tuple projection is rejected",
        .source_kind = .module,
        .source =
        \\main = (1, 2).2
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B036: or-pattern alternatives bind the same names",
        .source_kind = .module,
        .source =
        \\main = match Err(2) {
        \\    Ok(x) | Err(y) => x
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B038: fractional decimal literal does not type-check as I64",
        .source_kind = .module,
        .source =
        \\main : I64
        \\main = 1.0
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B042: type error in match scrutinee is reported",
        .source_kind = .module,
        .source =
        \\f = |_| { b: match 0 - { a: 1, b: 2 } {
        \\    _ => 1
        \\} }
        \\
        \\main = f(0)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B045: custom from_numeral literal lowers through the nominal method",
        .source_kind = .module,
        .source =
        \\MyNum := [Blah].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Blah)
        \\}
        \\
        \\main : MyNum
        \\main = 42
        ,
        .expected = .{ .inspect_str = "Blah" },
    },
    .{
        .name = "regression B046: heterogeneous custom times dispatch lowers RHS as I64",
        .source_kind = .module,
        .source =
        \\Duration := { seconds: I64 }.{
        \\    times : Duration, I64 -> Duration
        \\    times = |d, n| { seconds: d.seconds * n }
        \\}
        \\
        \\my_duration : Duration
        \\my_duration = { seconds: 60 }
        \\
        \\result : Duration
        \\result = my_duration * 5
        \\
        \\main = result.seconds
        ,
        .expected = .{ .inspect_str = "300" },
    },
    .{
        .name = "regression B047: empty tag-union constant is not planned as runtime payload",
        .source_kind = .module,
        .source =
        \\x : Try(I64, [])
        \\x = Ok(42)
        \\
        \\main = x
        ,
        .expected = .{ .inspect_str = "Ok(42)" },
    },
    .{
        .name = "regression B049: unannotated numeric literal match is non-exhaustive",
        .source_kind = .module,
        .source =
        \\f = |n| match n {
        \\    30 => "thirty"
        \\}
        \\
        \\main = f(31)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B050: chained attached-method calls keep checked targets",
        .source_kind = .module,
        .source =
        \\Container(a) := [Value(a)].{
        \\    add2 = |container| container.add1().add1()
        \\
        \\    add1 = |container| container.map(|val| val + 1.I64)
        \\
        \\    map : Container(a), (a -> b) -> Container(b)
        \\    map = |Value(val), f| Value(f(val))
        \\}
        \\
        \\main = Container.Value(100.I64).add2()
        ,
        .expected = .{ .inspect_str = "Value(102)" },
    },
    .{
        .name = "regression B051: recursive attached-method call preserves nominal payload",
        .source_kind = .module,
        .source =
        \\Wrapper(a) := [Val(a)].{
        \\    apply_n = |Wrapper.Val(x), f, n| {
        \\        if n == 0.U64 {
        \\            Wrapper.Val(x)
        \\        } else {
        \\            Wrapper.Val(f(x)).apply_n(f, 0.U64)
        \\        }
        \\    }
        \\}
        \\
        \\main = Wrapper.Val(1.U64).apply_n(|x| x + 1.U64, 1.U64)
        ,
        .expected = .{ .inspect_str = "Val(2)" },
    },
    .{
        .name = "regression B052: stored returned closure preserves captured values",
        .source_kind = .module,
        .source =
        \\x : U8
        \\x = 10
        \\
        \\process = |y| |_| [x, y]
        \\
        \\main = {
        \\    f = process(1.U8)
        \\    f(0.U8)
        \\}
        ,
        .expected = .{ .inspect_str = "[10, 1]" },
    },
    .{
        .name = "regression B056: missing method inside function body is reported",
        .source_kind = .module,
        .source =
        \\f = |_| 1.nope()
        \\
        \\main = f(0)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B059: compile-time constant pattern failure becomes Roc crash",
        .source_kind = .module,
        .source =
        \\f = |0| 1
        \\
        \\main = f(1)
        ,
        .expected = .{ .crash = {} },
    },
    .{
        .name = "regression B060: record-rest declaration pattern preserves rest fields",
        .source_kind = .module,
        .source =
        \\main = {
        \\    { a, ..rest } = { a: 1.I64, b: 2.I64 }
        \\    rest.b
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "regression B063: extensible tag-union alias rejects non-union extension",
        .source_kind = .module,
        .source =
        \\A(x) : [Red, ..x]
        \\
        \\f : A(I64) -> I64
        \\f = |_| 1
        \\
        \\main = f(Red)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B065: huge integer Dec suffix literal is rejected",
        .source_kind = .module,
        .source =
        \\main = 99999999999999999999999999999999999999.Dec
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B068: Str.inspect handles values containing uninhabited tag unions",
        .source_kind = .module,
        .source =
        \\main = {
        \\    x : Try(I64, [])
        \\    x = Ok(1)
        \\    Str.inspect(x)
        \\}
        ,
        .expected = .{ .inspect_str = "\"Ok(1)\"" },
    },
    .{
        .name = "regression B076: F64.from_str rejects out-of-range finite inputs",
        .source = "F64.from_str(\"1e999\")",
        .expected = .{ .inspect_str = "Err(BadNumStr)" },
    },
    .{
        .name = "regression B077: signed right shift by type width preserves sign",
        .source = "I8.shift_right_by(-1, 8)",
        .expected = .{ .inspect_str = "-1" },
        .skip = .{ .llvm = true },
    },
    .{
        .name = "regression B078: guarded-only match is non-exhaustive",
        .source_kind = .module,
        .source =
        \\main = match 1.I64 {
        \\    x if x == 2.I64 => x
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B079: nested Box.box around boxed function preserves callable shape",
        .source_kind = .module,
        .source =
        \\main = {
        \\    f = Box.box(Box.box(|x| x + 1.I64))
        \\    Box.unbox(Box.unbox(f))(1.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "regression B080: bound immediately-invoked lambda return does not crash",
        .source_kind = .module,
        .source =
        \\main = {
        \\    _ = (|| { return {} })()
        \\    {}
        \\}
        ,
        .expected = .{ .inspect_str = "{}" },
    },
    .{
        .name = "regression B082: break inside nested lambda is rejected",
        .source_kind = .module,
        .source =
        \\main = {
        \\    var $n = 0.I64
        \\    while $n < 1 {
        \\        f = || { break }
        \\        f()
        \\        $n = $n + 1
        \\    }
        \\}
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B085: nominal plus operator uses attached method",
        .source_kind = .module,
        .source =
        \\Thing := [Thing(I64)].{
        \\    plus : Thing, Thing -> Thing
        \\    plus = |_, _| Thing(99)
        \\}
        \\
        \\main = {
        \\    a : Thing
        \\    a = Thing(1.I64)
        \\
        \\    b : Thing
        \\    b = Thing(2.I64)
        \\
        \\    (a.plus(b), a + b)
        \\}
        ,
        .expected = .{ .inspect_str = "(Thing(99), Thing(99))" },
    },
    .{
        .name = "regression B088: list rest-only pattern preserves list type",
        .source_kind = .module,
        .source =
        \\main = (|[.. as rest]| rest)([])
        ,
        .expected = .{ .inspect_str = "[]" },
    },
    .{
        .name = "regression B090: record extension alias rejects non-record extension",
        .source_kind = .module,
        .source =
        \\R(x) : { a : I64, ..x }
        \\
        \\f : R(List(I64)) -> I64
        \\f = |r| r.a
        \\
        \\main = 1
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B091: polymorphic top-level constant is rejected before runtime planning",
        .source_kind = .module,
        .source =
        \\x = []
        \\
        \\main = Str.inspect(x)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B092: ambiguous List.sum on empty list is rejected",
        .source = "List.sum([])",
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B095: record builder map2 must return B(c)",
        .source_kind = .module,
        .source =
        \\B(a) := { value : a }.{
        \\    map2 : B(a), B(b), (a, b -> c) -> B(a)
        \\    map2 = |ba, _bb, _f| ba
        \\
        \\    pure : a -> B(a)
        \\    pure = |x| { value: x }
        \\
        \\    run : B(a) -> a
        \\    run = |b| b.value
        \\}
        \\
        \\main = { a: B.pure(1.I64), b: B.pure(2.I64) }.B.run()
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B096: record extension aliases reject duplicate fields from extension args",
        .source_kind = .module,
        .source =
        \\Ext : { a : Str }
        \\R(x) : { a : I64, ..x }
        \\
        \\f : R(Ext) -> I64
        \\f = |r| r.a
        \\
        \\main = f({ a: 1.I64 })
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B097: tag-union extension aliases reject duplicate tags from extension args",
        .source_kind = .module,
        .source =
        \\Ext : [A]
        \\T(x) : [A, ..x]
        \\
        \\f : T(Ext) -> I64
        \\f = |t| match t {
        \\    A => 1
        \\}
        \\
        \\main = f(A)
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "regression B098: polymorphic record update preserves row fields",
        .source_kind = .module,
        .source =
        \\set = |r| { ..r, a: 2.I64 }
        \\
        \\main = set({ a: 1.I64, b: "x" }).b
        ,
        .expected = .{ .inspect_str = "\"x\"" },
    },
    .{
        .name = "regression B099: return inside aggregate-stored closure returns value",
        .source_kind = .module,
        .source =
        \\main = {
        \\    r = { f: |x| {
        \\        return x
        \\        x + 1
        \\    } }
        \\
        \\    (r.f)(1.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "regression B100: for loop list pattern materializes before IR lowering",
        .source_kind = .module,
        .source =
        \\main = {
        \\    var $out = 0.I64
        \\    for [a, b] in [[1.I64, 2.I64]] {
        \\        $out = a + b
        \\    }
        \\    $out
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "regression B101: monotype function template was assigned two lifted function ids",
        .source_kind = .module,
        .source =
        \\Foo(a) := [Bar(a), Baz]
        \\
        \\combine : Foo(a), Foo(b), (a, b -> c) -> Foo(c)
        \\combine = |ma, mb, f|
        \\    match (ma, mb) {
        \\        (Bar(a), Bar(b)) => Bar(f(a, b))
        \\        _ => Baz
        \\    }
        \\
        \\main = (combine(Baz, Baz, |a, b| a + b) == Baz, combine(Bar(1), Bar(2), |a, b| a + b) == Bar(3))
        ,
        .expected = .{ .inspect_str = "(True, True)" },
    },
    .{
        .name = "regression B102: recursive Monotype record lowering keeps field span stable",
        .source_kind = .module,
        .source =
        \\Leaf : {
        \\    v01 : I64, v02 : I64, v03 : I64, v04 : I64,
        \\    v05 : I64, v06 : I64, v07 : I64, v08 : I64,
        \\    v09 : I64, v10 : I64, v11 : I64, v12 : I64,
        \\    v13 : I64, v14 : I64, v15 : I64, v16 : I64,
        \\}
        \\
        \\Outer : {
        \\    f01 : Leaf, f02 : Leaf, f03 : Leaf, f04 : Leaf,
        \\    f05 : Leaf, f06 : Leaf, f07 : Leaf, f08 : Leaf,
        \\    f09 : Leaf, f10 : Leaf, f11 : Leaf, f12 : Leaf,
        \\    f13 : Leaf, f14 : Leaf, f15 : Leaf, f16 : Leaf,
        \\    f17 : Leaf, f18 : Leaf, f19 : Leaf, f20 : Leaf,
        \\    f21 : Leaf, f22 : Leaf, f23 : Leaf, f24 : Leaf,
        \\}
        \\
        \\leaf : I64 -> Leaf
        \\leaf = |n| {
        \\    v01: n, v02: n, v03: n, v04: n,
        \\    v05: n, v06: n, v07: n, v08: n,
        \\    v09: n, v10: n, v11: n, v12: n,
        \\    v13: n, v14: n, v15: n, v16: n,
        \\}
        \\
        \\main : I64
        \\main = {
        \\    r : Outer
        \\    r = {
        \\        f01: leaf(1.I64), f02: leaf(2.I64), f03: leaf(3.I64), f04: leaf(4.I64),
        \\        f05: leaf(5.I64), f06: leaf(6.I64), f07: leaf(7.I64), f08: leaf(8.I64),
        \\        f09: leaf(9.I64), f10: leaf(10.I64), f11: leaf(11.I64), f12: leaf(12.I64),
        \\        f13: leaf(13.I64), f14: leaf(14.I64), f15: leaf(15.I64), f16: leaf(16.I64),
        \\        f17: leaf(17.I64), f18: leaf(18.I64), f19: leaf(19.I64), f20: leaf(20.I64),
        \\        f21: leaf(21.I64), f22: leaf(22.I64), f23: leaf(23.I64), f24: leaf(24.I64),
        \\    }
        \\    r.f24.v16
        \\}
        ,
        .expected = .{ .inspect_str = "24" },
    },
    .{
        .name = "regression B103: dispatched imported-module method returning a multi-field record panics in monotype lowering (#9591)",
        .source_kind = .module,
        .imports = &.{.{
            .name = "Foo",
            .source =
            \\Foo :: { id : U64 }.{
            \\    bar : U64 -> Foo
            \\    bar = |id| { id: id }
            \\
            \\    baz : Foo, {} -> { a : I32, b : I32 }
            \\    baz = |_, {}| { a: 0, b: 0 }
            \\}
            ,
        }},
        .source =
        \\import Foo exposing [bar]
        \\
        \\main = {
        \\    out = bar(0).baz({})
        \\    out.a
        \\}
        ,
        .expected = .{ .inspect_str = "0" },
    },
    .{
        // #9700: a match with many fixed-length list patterns must lower to LIR
        // whose size is linear in the patterns, not exponential. Each list
        // branch shares one miss target, so this compiles quickly; before the
        // fix the shared fallback was re-materialized per element test, giving
        // ~(elements+1)^branches statements that exhausted memory at compile
        // time. A regression here reappears as an out-of-memory/timeout, not a
        // wrong value.
        .name = "issue 9700: long match of List(U8) does not blow up compilation",
        .source_kind = .module,
        .source =
        \\to_instruction : List(U8) -> Try(U8, [InvalidCodon])
        \\to_instruction = |codon| {
        \\    match codon {
        \\        ['A', 'U', 'G'] => Ok(1)
        \\        ['U', 'U', 'U'] => Ok(2)
        \\        ['U', 'U', 'C'] => Ok(3)
        \\        ['U', 'U', 'A'] => Ok(4)
        \\        ['U', 'U', 'G'] => Ok(5)
        \\        ['U', 'C', 'U'] => Ok(6)
        \\        ['U', 'C', 'C'] => Ok(7)
        \\        ['U', 'C', 'A'] => Ok(8)
        \\        ['U', 'C', 'G'] => Ok(9)
        \\        ['U', 'A', 'U'] => Ok(10)
        \\        ['U', 'A', 'C'] => Ok(11)
        \\        ['U', 'G', 'U'] => Ok(12)
        \\        ['U', 'G', 'C'] => Ok(13)
        \\        ['U', 'G', 'G'] => Ok(14)
        \\        ['U', 'A', 'A'] => Ok(15)
        \\        ['U', 'A', 'G'] => Ok(16)
        \\        ['U', 'G', 'A'] => Ok(17)
        \\        _ => Err(InvalidCodon)
        \\    }
        \\}
        \\
        \\main = to_instruction(Str.to_utf8("UUC"))
        ,
        .expected = .{ .inspect_str = "Ok(3)" },
    },
};

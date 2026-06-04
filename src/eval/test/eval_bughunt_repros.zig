//! Opt-in compiler-bug repros found during bug hunting.
//!
//! These are intentionally marked `known_bug = true` and excluded from ordinary
//! `zig build test-eval`. Run them explicitly with:
//!
//!   zig build test-eval -- known-bugs --filter bughunt

const TestCase = @import("parallel_runner.zig").TestCase;

/// Compiler bug repros found during bug hunting.
pub const tests = [_]TestCase{
    .{
        .name = "bughunt B004: polymorphic function inside tuple constant is concretely sealed",
        .source_kind = .module,
        .source =
        \\t = (|x| x, 0)
        \\
        \\main = (t.0)(1.I64)
        ,
        .expected = .{ .inspect_str = "1" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B005: signed min-int division overflow wraps",
        .source = "(-128.I8) // -1.I8",
        .expected = .{ .inspect_str = "-128" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B006: signed min-int negate wraps",
        .source = "I8.negate(-128)",
        .expected = .{ .inspect_str = "-128" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B007: Dec suffix preserves exact decimal source text",
        .source = "100000.0.Dec == 100000.Dec",
        .expected = .{ .inspect_str = "True" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B008: out-of-range Dec suffix literal is rejected",
        .source = "170141183460469231732.0.Dec",
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B009: inferred out-of-range Dec literal is rejected",
        .source_kind = .module,
        .source =
        \\main : Dec
        \\main = 170141183460469231732.0
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B010: Dec multiplication overflow is a Roc crash",
        .source = "170141183460469231731.0.Dec * 2.0.Dec",
        .expected = .{ .crash = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B012: F64.to_i8_try has a runtime body",
        .source = "F64.to_i8_try(42.0)",
        .expected = .{ .inspect_str = "Ok(42)" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B013: F64.to_i8_wrap wraps out-of-range inputs",
        .source = "F64.to_i8_wrap(128.0)",
        .expected = .{ .inspect_str = "-128" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B018: duplicate tag names in nominal declarations are rejected",
        .source_kind = .module,
        .source =
        \\Foo := [A, A]
        \\
        \\main = 1
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B019: duplicate record fields in nominal declarations are rejected",
        .source_kind = .module,
        .source =
        \\Foo := { x : U8, x : U16 }
        \\
        \\main = 1
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B021: duplicate type parameters are rejected",
        .source_kind = .module,
        .source =
        \\F(a, a) : a
        \\
        \\main : F(I64, Str)
        \\main = "x"
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B022: ellipsis expression crashes when reached",
        .source_kind = .module,
        .source =
        \\main = ...
        ,
        .expected = .{ .crash = {} },
    },
    .{
        .name = "bughunt B024: nested closure captures enclosing lambda parameter",
        .source_kind = .module,
        .source =
        \\main = (|x| {
        \\    g = || x
        \\    g()
        \\})(1.I64)
        ,
        .expected = .{ .inspect_str = "1" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B026: local partial record destructuring preserves row fields",
        .source_kind = .module,
        .source =
        \\main = {
        \\    r = { a: 1.I64, b: 2.I64 }
        \\    f = |{ a }| a
        \\    f(r)
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B028: compile-time list-pattern failure becomes Roc crash",
        .source_kind = .module,
        .source =
        \\main : I64
        \\main = (|[a]| a)([])
        ,
        .expected = .{ .crash = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B029: unannotated crash constant becomes Roc crash",
        .source_kind = .module,
        .source =
        \\main = {
        \\    crash "x"
        \\}
        ,
        .expected = .{ .crash = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B030: out-of-range I8 literal is rejected",
        .source_kind = .module,
        .source =
        \\main : I8
        \\main = 128
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B031: very large decimal fraction literal is not zero",
        .source = "999999999999999999999999999999999999999.0 == 0.0",
        .expected = .{ .inspect_str = "False" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B035: out-of-bounds tuple projection is rejected",
        .source_kind = .module,
        .source =
        \\main = (1, 2).2
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B036: or-pattern alternatives bind the same names",
        .source_kind = .module,
        .source =
        \\main = match Err(2) {
        \\    Ok(x) | Err(y) => x
        \\}
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B038: fractional decimal literal does not type-check as I64",
        .source_kind = .module,
        .source =
        \\main : I64
        \\main = 1.0
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B042: type error in match scrutinee is reported",
        .source_kind = .module,
        .source =
        \\f = |_| { b: match 0 - { a: 1, b: 2 } {
        \\    _ => 1
        \\} }
        \\
        \\main = f(0)
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B045: custom from_numeral literal lowers through the nominal method",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B046: heterogeneous custom times dispatch lowers RHS as I64",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B047: empty tag-union constant is not planned as runtime payload",
        .source_kind = .module,
        .source =
        \\x : Try(I64, [])
        \\x = Ok(42)
        \\
        \\main = x
        ,
        .expected = .{ .inspect_str = "Ok(42)" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B049: unannotated numeric literal match is non-exhaustive",
        .source_kind = .module,
        .source =
        \\f = |n| match n {
        \\    30 => "thirty"
        \\}
        \\
        \\main = f(31)
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B050: chained attached-method calls keep checked targets",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B051: recursive attached-method call preserves nominal payload",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B052: stored returned closure preserves captured values",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B056: missing method inside function body is reported",
        .source_kind = .module,
        .source =
        \\f = |_| 1.nope()
        \\
        \\main = f(0)
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B059: compile-time constant pattern failure becomes Roc crash",
        .source_kind = .module,
        .source =
        \\f = |0| 1
        \\
        \\main = f(1)
        ,
        .expected = .{ .crash = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B060: record-rest declaration pattern preserves rest fields",
        .source_kind = .module,
        .source =
        \\main = {
        \\    { a, ..rest } = { a: 1.I64, b: 2.I64 }
        \\    rest.b
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B063: extensible tag-union alias rejects non-union extension",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B065: huge integer Dec suffix literal is rejected",
        .source_kind = .module,
        .source =
        \\main = 99999999999999999999999999999999999999.Dec
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B068: Str.inspect handles values containing uninhabited tag unions",
        .source_kind = .module,
        .source =
        \\main = {
        \\    x : Try(I64, [])
        \\    x = Ok(1)
        \\    Str.inspect(x)
        \\}
        ,
        .expected = .{ .inspect_str = "\"Ok(1)\"" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B076: F64.from_str rejects out-of-range finite inputs",
        .source = "F64.from_str(\"1e999\")",
        .expected = .{ .inspect_str = "Err(BadNumStr)" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B077: signed right shift by type width preserves sign",
        .source = "I8.shift_right_by(-1, 8)",
        .expected = .{ .inspect_str = "-1" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B078: guarded-only match is non-exhaustive",
        .source_kind = .module,
        .source =
        \\main = match 1.I64 {
        \\    x if x == 2.I64 => x
        \\}
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B079: nested Box.box around boxed function preserves callable shape",
        .source_kind = .module,
        .source =
        \\main = {
        \\    f = Box.box(Box.box(|x| x + 1.I64))
        \\    Box.unbox(Box.unbox(f))(1.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B080: bound immediately-invoked lambda return does not crash",
        .source_kind = .module,
        .source =
        \\main = {
        \\    _ = (|| { return {} })()
        \\    {}
        \\}
        ,
        .expected = .{ .inspect_str = "{}" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B082: break inside nested lambda is rejected",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B085: nominal plus operator uses attached method",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B088: list rest-only pattern preserves list type",
        .source_kind = .module,
        .source =
        \\main = (|[.. as rest]| rest)([])
        ,
        .expected = .{ .inspect_str = "[]" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B090: record extension alias rejects non-record extension",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B091: polymorphic top-level constant is rejected before runtime planning",
        .source_kind = .module,
        .source =
        \\x = []
        \\
        \\main = Str.inspect(x)
        ,
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B092: ambiguous List.sum on empty list is rejected",
        .source = "List.sum([])",
        .expected = .{ .problem = {} },
        .known_bug = true,
    },
    .{
        .name = "bughunt B095: record builder map2 must return B(c)",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B096: record extension aliases reject duplicate fields from extension args",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B097: tag-union extension aliases reject duplicate tags from extension args",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B098: polymorphic record update preserves row fields",
        .source_kind = .module,
        .source =
        \\set = |r| { ..r, a: 2.I64 }
        \\
        \\main = set({ a: 1.I64, b: "x" }).b
        ,
        .expected = .{ .inspect_str = "\"x\"" },
        .known_bug = true,
    },
    .{
        .name = "bughunt B099: return inside aggregate-stored closure returns value",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B100: for loop list pattern materializes before IR lowering",
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
        .known_bug = true,
    },
    .{
        .name = "bughunt B101: monotype function template was assigned two lifted function ids",
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
        .known_bug = true,
    },
};

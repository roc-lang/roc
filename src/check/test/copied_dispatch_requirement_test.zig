//! A local definition that calls a method on an enclosing definition's
//! unresolved receiver keeps that requirement in its own scheme, and each use
//! of the local definition copies it. When the receiver is still a type
//! variable at the enclosing definition's generalization boundary, it has one
//! method of each name, so those copies, the requirement the local scheme
//! holds, and the enclosing definition's own calls are one requirement. When
//! the receiver resolves to a concrete type first, each copy is discharged
//! against that type's method on its own.

const TestEnv = @import("./TestEnv.zig");

const prelude =
    \\Repro := [].{}
    \\
    \\hole : {} -> a
    \\hole = |{}| crash "hole"
    \\
    \\same : a, a -> {}
    \\same = |_, _| {}
    \\
    \\W := [W].{
    \\    wrap : W, x -> List(x)
    \\    wrap = |_, x| [x]
    \\}
    \\
;

test "copied method requirements - a nested use on an unresolved receiver is a method conflict" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| outer.wrap(v)
        \\    List.len(helper(helper(1.U8)))
        \\}
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "two different, incompatible `wrap` methods");
}

test "copied method requirements - a receiver unified with the outer one is a method conflict" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| {
        \\        p = hole({})
        \\        _s = same(p, outer)
        \\        p.wrap(v)
        \\    }
        \\    List.len(helper(helper(1.U8)))
        \\}
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "two different, incompatible `wrap` methods");
}

test "copied method requirements - uses at two types on an unresolved receiver are a method conflict" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| outer.wrap(v)
        \\    x = helper(1.U8)
        \\    y = helper("s")
        \\    (List.len(x), List.len(y))
        \\}
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "two different, incompatible `wrap` methods");
}

test "copied method requirements - a use conflicting with a direct call is a method conflict" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| outer.wrap(v)
        \\    x = outer.wrap(1.U8)
        \\    y = helper("s")
        \\    List.len(x) + List.len(y)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "two different, incompatible `wrap` methods");
}

test "copied method requirements - agreeing uses and a direct call are one requirement" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| outer.wrap(v)
        \\    x = outer.wrap(1.U8)
        \\    y = helper(2.U8)
        \\    z = helper(3.U8)
        \\    List.len(x) + List.len(y) + List.len(z)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "b -> U64 where [b.wrap : b, U8 -> List(_item)]");
}

test "copied method requirements - a receiver resolved later serves each use at its own type" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| outer.wrap(v)
        \\    x = helper(1.U8)
        \\    y = helper("s")
        \\    _s = same(outer, W.W)
        \\    (x, y)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "W -> (List(U8), List(Str))");
}

test "copied method requirements - a receiver resolved later serves a nested use" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| {
        \\        p = hole({})
        \\        _s = same(p, outer)
        \\        p.wrap(v)
        \\    }
        \\    n = List.len(helper(helper(1.U8)))
        \\    _t = same(outer, W.W)
        \\    n
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "W -> U64");
}

test "copied method requirements - two uses of a local definition at one type check" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\poly = |s, t| {
        \\    helper = |x| s.read() + x
        \\    (helper(t), helper(t))
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "copied method requirements - two uses of a local definition with a ? check" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\Source := { n : U64 }.{
        \\    read : Source -> Try(U64, [Other])
        \\    read = |s| if s.n > 5 Err(Other) else Ok(s.n)
        \\}
        \\
        \\poly = |s, t| {
        \\    helper = |x| {
        \\        m = s.read()?
        \\        Ok(m + x)
        \\    }
        \\    (helper(t), helper(t))
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "copied method requirements - an unused local definition's call keeps the enclosing argument monomorphic" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\f = |x| {
        \\    g = |outer| {
        \\        _helper = |v| outer.wrap(v)
        \\        l = outer.wrap(x)
        \\        l
        \\    }
        \\    n : U64
        \\    n = x
        \\    b : Str
        \\    b = x
        \\    (g, n, b)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "Str");
}

test "copied method requirements - a local definition using another is not polymorphic recursion" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer, x| {
        \\    helper = |v| {
        \\        p = hole({})
        \\        _s = same(p, outer)
        \\        p.id(v)
        \\    }
        \\    other = |w| helper(w)
        \\    other(helper(x))
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "a, b -> b where [a.id : a, b -> b]");
}

test "copied method requirements - a nested call's template is not a second requirement" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\func = |a, b| {
        \\    add_ab = |x| a + b + x
        \\    add_ab(10)
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("func",
        \\c, d -> c
        \\  where [
        \\    c.plus : c, d -> c,
        \\    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
        \\  ]
    );
}

test "copied method requirements - an escaping local definition keeps its requirement on the receiver" {
    var test_env = try TestEnv.init("Repro", prelude ++
        \\a = |outer| {
        \\    helper = |v| outer.wrap(v)
        \\    _x = List.len(helper(1.U8))
        \\    helper
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("a", "b -> (U8 -> List(_item)) where [b.wrap : b, U8 -> List(_item)]");
}

const closure_record_prelude =
    \\Repro := [].{}
    \\
    \\task : { run : (() -> result), resolve : (result -> Str) } -> Str
    \\task = |config| (config.resolve)((config.run)())
    \\
    \\limit : U64
    \\limit = 96
    \\
    \\read_file = |directory, name| task({
    \\    run: || directory.read(),
    \\    resolve: |result| Str.concat(name, result.take_first(limit)),
    \\})
    \\
    \\Text := [Text].{
    \\    take_first : Text, U64 -> Str
    \\    take_first = |_, _| "text"
    \\}
    \\
    \\TextDir := [TextDir].{
    \\    read : TextDir -> Text
    \\    read = |_| Text.Text
    \\}
    \\
    \\BytesDir := [BytesDir].{
    \\    read : BytesDir -> List(U8)
    \\    read = |_| [1, 2]
    \\}
    \\
;

test "copied method requirements - a closure's call on another closure's method result is the enclosing definition's requirement" {
    var test_env = try TestEnv.init("Repro", closure_record_prelude);
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try test_env.assertDefType("read_file",
        \\a, Str -> Str
        \\  where [a.read : a -> result
        \\     , result.take_first : result, U64 -> Str]
    );
}

test "copied method requirements - a use satisfying a closure's requirement on a method result checks" {
    var test_env = try TestEnv.init("Repro", closure_record_prelude ++
        \\used : Str
        \\used = read_file(TextDir.TextDir, "name")
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "copied method requirements - a use violating a closure's requirement on a method result is rejected" {
    var test_env = try TestEnv.init("Repro", closure_record_prelude ++
        \\used : Str
        \\used = read_file(BytesDir.BytesDir, "name")
    );
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "`take_first` method on `List` has an incompatible type");
}

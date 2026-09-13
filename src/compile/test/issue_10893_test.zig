//! Regression test for issue #10893.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 10893: numeric literal pattern against a tag scrutinee does not panic monotype lowering" {
    try expectLowersToLirWithOptions(
        \\f : F64 -> I64
        \\f = |x| match Z {
        \\    1e-40 => 1
        \\    1e-40 => 2
        \\    _ => 0
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f(1.0).to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

test "issue 10893: quoted literal pattern against a tag scrutinee does not panic monotype lowering" {
    try expectLowersToLirWithOptions(
        \\f = match Z {
        \\    "a" => 1
        \\    _ => 0
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f.to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

test "issue 10893: deferred literal dispatch inside a compound pattern invalidates the match" {
    try expectLowersToLirWithOptions(
        \\f = match [Z] {
        \\    [1] => 1
        \\    _ => 0
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f.to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

test "issue 10893: rejected equality dispatch on a converted numeral pattern invalidates the match" {
    try expectLowersToLirWithOptions(
        \\N := [Val(I64)].{
        \\    from_numeral : Numeral -> Try(N, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(N.Val(0))
        \\}
        \\
        \\f = match N.Val(0) {
        \\    1 => 1
        \\    _ => 0
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f.to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

test "issue 10893: a shared rejected literal dispatch invalidates every owning match" {
    try expectLowersToLirWithOptions(
        \\f : [Z] -> I64
        \\f = |x| {
        \\    first = match x {
        \\        1 => 1
        \\        _ => 0
        \\    }
        \\    second = match x {
        \\        2 => 1
        \\        _ => 0
        \\    }
        \\    first + second
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f(Z).to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

test "issue 10893: merged rejected literal dispatch invalidates every owning match" {
    try expectLowersToLirWithOptions(
        \\f = |x| {
        \\    first = match x {
        \\        1 => 1
        \\        _ => 0
        \\    }
        \\    second = match x {
        \\        2 => 1
        \\        _ => 0
        \\    }
        \\    tag = match x {
        \\        Z => 1
        \\        _ => 0
        \\    }
        \\    first + second + tag
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f(Z).to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

test "issue 10893: merged rejected literal equality invalidates every owning match" {
    try expectLowersToLirWithOptions(
        \\N := [Val(I64)].{
        \\    from_numeral : Numeral -> Try(N, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(N.Val(0))
        \\}
        \\
        \\f : N -> I64
        \\f = |x| {
        \\    first = match x {
        \\        1 => 1
        \\        _ => 0
        \\    }
        \\    second = match x {
        \\        2 => 1
        \\        _ => 0
        \\    }
        \\    first + second
        \\}
        \\
        \\main! = |_args| {
        \\    echo!(f(N.Val(0)).to_str())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .monotype_only = true });
}

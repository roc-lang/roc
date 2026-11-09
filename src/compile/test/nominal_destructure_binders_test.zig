//! A nominal destructure names its type exactly as a nominal construction
//! does, wherever the type is declared; when that type does not resolve, the
//! names the destructure binds are erroneous, and every use of them is retired.

const harness = @import("lower_to_lir_harness.zig");

test "a top-level nominal destructure before its nested type's declaration binds its names" {
    try harness.expectLowersToLir(
        \\T.N.{ r0: a, r1: _ } = T.N.{ r0: True, r1: 2 }
        \\
        \\T := [A].{
        \\    N := { r0 : Bool, r1 : U64 }
        \\    f : U64 -> U64
        \\    f = |x| if (a) (x) else (x + 1)
        \\}
        \\
        \\main! = |_args| {
        \\    _ = T.f(1)
        \\    Ok({})
        \\}
    );
}

test "the names of a top-level nominal record destructure whose type is undeclared are erroneous" {
    try harness.expectLowersToLirWithOptions(
        \\Q.N.{ r0: a } = { r0: True }
        \\
        \\z = a
        \\
        \\main! = |_args| {
        \\    _ = z
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}

test "a method reading a name of a nominal destructure whose type is undeclared captures nothing" {
    try harness.expectLowersToLirWithOptions(
        \\Q.N.{ r0: a, r1: _ } = nope
        \\
        \\T := [A].{
        \\    f : U64 -> U64
        \\    f = |x| if (a) (x) else (x + 1)
        \\}
        \\
        \\main! = |_args| {
        \\    _ = T.f(1)
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}

test "the names of a top-level qualified tag destructure whose type is undeclared are erroneous" {
    try harness.expectLowersToLirWithOptions(
        \\Q.Tag(a) = Q.Tag(1)
        \\
        \\z = a
        \\
        \\main! = |_args| {
        \\    _ = z
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}

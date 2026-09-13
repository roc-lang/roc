//! Regression tests for issue #11098.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 11098: out-of-range literal in an unannotated lambda body lowers to a runtime error" {
    const source =
        \\get_n = |{}| 300
        \\
        \\main! = |_args| {
        \\    _a : U8
        \\    _a = get_n({})
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .specialization_strategy = .boxy });
}

test "issue 11098: rejecting one inferred literal specialization preserves another" {
    try expectLowersToLirWithOptions(
        \\get_n = |{}| 300
        \\
        \\choose : Bool -> [Small(U8), Large(I64)]
        \\choose = |small| if small {
        \\    Small(get_n({}))
        \\} else {
        \\    Large(get_n({}))
        \\}
        \\
        \\main! = |args| {
        \\    choose(args.is_empty())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}

test "issue 11098: every invalid use sharing one receiver lowers to a runtime error" {
    try expectLowersToLirWithOptions(
        \\get_n = |{}| 300
        \\
        \\choose : Bool -> U8
        \\choose = |first| if first {
        \\    get_n({})
        \\} else {
        \\    get_n({})
        \\}
        \\
        \\main! = |args| {
        \\    choose(args.is_empty())
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}

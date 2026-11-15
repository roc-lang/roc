//! Regression test for https://github.com/roc-lang/roc/issues/12999: a lambda
//! whose parameter type never resolves, passed to a function requiring a
//! curried `parser_for` of that parameter, is reported as a missing method on
//! a type that never resolved to a concrete type, and the rejected program
//! lowers to a checked runtime error under both lowering strategies.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 12999: an unresolved parser_for receiver behind a curried parser is reported" {
    const source =
        \\handle_as : (p -> Str) -> Str where [p.parser_for : {} -> (Str -> Try({ value : p, rest : Str }, [Bad]))]
        \\handle_as = |decide| {
        \\    P : p
        \\    parse = P.parser_for({})
        \\    match parse("41") {
        \\        Ok(read) => decide(read.value)
        \\        Err(Bad) => "bad"
        \\    }
        \\}
        \\
        \\on_count = handle_as(|n| (n + 1.U64).to_str())
        \\
        \\main! = |_args| {
        \\    echo!(on_count)
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Missing Method", .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Missing Method", .specialization_strategy = .boxy });
}

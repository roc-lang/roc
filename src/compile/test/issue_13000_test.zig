//! Regression tests for https://github.com/roc-lang/roc/issues/13000: a called
//! function whose `where` clause requires a derivable method that its
//! receiver cannot satisfy reports the diagnostic and lowers the rejected
//! call to a checked runtime error under both lowering strategies.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 13000: a called derivable-method where clause naming an internal type is reported" {
    const source =
        \\stamp : r -> Str where [r.encoder_for : Encoding.JsonEncoding -> {}]
        \\stamp = |_| "ok"
        \\
        \\main! = |_args| {
        \\    echo!(stamp({}))
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Internal Builtin Type", .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Internal Builtin Type", .specialization_strategy = .boxy });
}

test "issue 13000: an unresolved receiver of a derivable method through a top-level value is reported" {
    const source =
        \\handle_as : (p -> Str) -> Str where [p.parser_for : {} -> p]
        \\handle_as = |decide| {
        \\    P : p
        \\    decide(P.parser_for({}))
        \\}
        \\
        \\on_count : Str
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

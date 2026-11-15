//! A derivable-method requirement whose signature no derived method has is
//! reported, and the rejected call lowers to a checked runtime error under
//! both lowering strategies.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "a rejected derived encoder_for signature lowers under both strategies" {
    const source =
        \\stamp : r -> Str where [r.encoder_for : Str -> {}]
        \\stamp = |_| "ok"
        \\
        \\main! = |_args| {
        \\    echo!(stamp({}))
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Missing Method", .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Missing Method", .specialization_strategy = .boxy });
}

test "a rejected derived parser_for signature lowers under both strategies" {
    const source =
        \\make : {} -> p where [p.parser_for : {} -> p]
        \\make = |_| {
        \\    P : p
        \\    P.parser_for({})
        \\}
        \\
        \\main! = |_args| {
        \\    record : { a : U64 }
        \\    record = make({})
        \\    echo!(record.a.to_str())
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Missing Method", .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .expected_report_title = "Missing Method", .specialization_strategy = .boxy });
}

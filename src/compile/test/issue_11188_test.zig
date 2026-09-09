//! Regression tests for issue #11188.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11188: structural equality on records lowers under both specialization strategies" {
    const source =
        \\main! = |args| {
        \\    left = { x: args.len() }
        \\    right = { x: 1 }
        \\    if left == right {
        \\        Err(Mismatch)
        \\    } else {
        \\        Ok({})
        \\    }
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "issue 11188: structural equality on records holding SIMD vectors lowers under both specialization strategies" {
    const source =
        \\main! = |args| {
        \\    upper_salt = args.len().to_u8_wrap().to_u128()
        \\    upper_left = { vector: U8x16.from_u128_bits((18446744073709551616).bitwise_xor(upper_salt)) }
        \\    upper_right = { vector: U8x16.from_u128_bits((36893488147419103232).bitwise_xor(upper_salt)) }
        \\    if upper_left == upper_right {
        \\        Err(StructuralEqualityIgnoredUpperBits)
        \\    } else {
        \\        Ok({})
        \\    }
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "ARC representation: closure preserves captured list ownership under both strategies" {
    const source =
        \\main! = |args| {
        \\    reader = |offset| args.len() + offset
        \\    if reader(0) == args.len() {
        \\        Ok({})
        \\    } else {
        \\        Err(Mismatch)
        \\    }
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

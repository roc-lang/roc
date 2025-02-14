
fn infer_eq(expression_str: []const u8, expected: []const u8) void {
    _ = expression_str;
    _ = expected;
    // Implementation of infer_eq
}

test "string literal" {
    const expected = "Str";
    const expression_str =
        \\
        \\"type inference!"
        \\
    ;

    infer_eq(expression_str, expected);
}

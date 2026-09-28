//! Regression coverage for https://github.com/roc-lang/roc/issues/11698
//!
//! A left-associative chain of ~1500 `+` operators type-checks cleanly but
//! overflowed the compiler's stack during post-check lowering to LIR.
//! Expected behavior: lowering such a deep chain succeeds. Reaching the end of
//! this test without a stack-overflow crash (and without a checker error) is
//! the assertion.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 11698: lowering a ~1500-term + chain does not overflow the stack" {
    const term_count = 1500;

    const app_body = try std.testing.allocator.alloc(u8, 64 + term_count * 4);
    defer std.testing.allocator.free(app_body);

    var writer = std.Io.Writer.fixed(app_body);
    try writer.writeAll(
        \\main! = |args| {
        \\    x = List.len(args)
        \\    _y = x
    );
    var i: usize = 1;
    while (i < term_count) : (i += 1) {
        try writer.writeAll(" + x");
    }
    try writer.writeAll(
        \\
        \\    Ok({})
        \\}
    );

    try harness.expectLowersToLir(app_body);
}

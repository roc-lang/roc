//! Regression coverage for diagnostic rendering work on repeated method calls.
//! https://github.com/roc-lang/roc/issues/12881
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

fn diagnosticConstraintComparisons(rows: usize) TestEnv.TestEnvError!usize {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll(
        \\Db :: { name : Str }.{
        \\    get : Db -> Try(Str, [Missing])
        \\    get = |db| Ok(db.name)
        \\}
        \\
        \\route = |path, db|
        \\    match path {
        \\        "/x" => Ok(db.get())
        \\
    );
    for (0..rows) |i| {
        try source.writer.print("        \"/{d}\" => db.get()\n", .{i});
    }
    try source.writer.writeAll(
        \\        _ => Err(Missing)
        \\    }
        \\
        \\process_string : Str -> Str
        \\process_string = |s|
        \\    match route(s, Db.{ name: s }) {
        \\        Ok(text) => text
        \\        Err(_) => "error"
        \\    }
    );

    var env = try TestEnv.init("Test", source.written());
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
    return env.checker.type_writer.test_constraint_comparisons;
}

test "issue 12881: repeated method calls have bounded diagnostic rendering work" {
    // Measure work, not elapsed time. Doubling equivalent calls should scale
    // linearly; allow 3x for fixed overhead and naming differences.
    const small = try diagnosticConstraintComparisons(20);
    const large = try diagnosticConstraintComparisons(40);
    if (large > 3 * small) {
        std.debug.print("constraint comparisons: 20 calls = {d}, 40 calls = {d} (limit {d})\n", .{ small, large, 3 * small });
    }
    try std.testing.expect(large <= 3 * small);
}

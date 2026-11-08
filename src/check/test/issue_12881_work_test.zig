//! End-to-end diagnostic work bounds, including capture and rendering rather
//! than only the duplicate-constraint comparisons pinned by issue_12881_test.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const Work = struct {
    capture: usize,
    render: usize,
    comparisons: usize,

    fn total(self: Work) usize {
        return self.capture + self.render + self.comparisons;
    }
};

fn checkRepeatedMethodConflict(rows: usize) !Work {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll(
        \\Db :: { name : Str }.{
        \\    get : Db -> Try(Str, [Missing])
        \\    get = |db| Ok(db.name)
        \\}
        \\route = |path, db| match path {
        \\    "/bad" => Ok(db.get())
        \\
    );
    for (0..rows) |i| try source.writer.print("    \"/{d}\" => db.get()\n", .{i});
    try source.writer.writeAll(
        \\    _ => Err(Missing)
        \\}
        \\run : Str -> Str
        \\run = |s| match route(s, Db.{ name: s }) {
        \\    Ok(text) => text
        \\    Err(_) => "error"
        \\}
    );
    var env = try TestEnv.init("Test", source.written());
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
    return .{
        .capture = env.checker.snapshots.test_capture_steps,
        .render = env.checker.type_writer.test_work + env.checker.snapshots.test_format_work,
        .comparisons = env.checker.type_writer.test_constraint_comparisons,
    };
}

test "issue 12881: total diagnostic work scales with repeated calls" {
    const small = try checkRepeatedMethodConflict(20);
    const large = try checkRepeatedMethodConflict(40);
    if (large.total() > 3 * small.total()) {
        std.debug.print("diagnostic work: 20 calls {any}; 40 calls {any}\n", .{ small, large });
    }
    try std.testing.expect(large.total() <= 3 * small.total());
}

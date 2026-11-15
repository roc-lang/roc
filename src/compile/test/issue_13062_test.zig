//! Regression coverage for https://github.com/roc-lang/roc/issues/13062.
//! Nested function definitions must lower through Boxy without native
//! call-stack usage growing with lexical depth.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

fn lowerOnSmallStack(source: []const u8, result: *harness.LowerToLirHarnessError!void) void {
    result.* = harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

/// `depth` nested definitions, each calling the next one in and returning
/// its result through `wrap`, the innermost returning a number literal:
/// the literal's requirement is forwarded through every definition's scheme.
fn nestedSource(gpa: std.mem.Allocator, prelude: []const u8, depth: usize, wrap: []const u8) (std.mem.Allocator.Error || std.Io.Writer.Error)![]u8 {
    var source: std.Io.Writer.Allocating = .init(gpa);
    errdefer source.deinit();
    try source.writer.writeAll(prelude);
    for (0..depth) |i| try source.writer.print("f{d} = |_| {{\n", .{i});
    try source.writer.print("f{d} = |_| 1\n{s}(f{d}(0))\n", .{ depth, wrap, depth });
    var i: usize = depth;
    while (i > 0) {
        i -= 1;
        try source.writer.writeAll("}\n");
        if (i > 0) try source.writer.print("{s}(f{d}(0))\n", .{ wrap, i });
    }
    try source.writer.writeAll("\nmain! = |_args| {\n\techo!(\"${f0(0).to_str()}\")\n\tOk({})\n}\n");
    return try source.toOwnedSlice();
}

/// The whole pipeline runs on this stack, checking included, so it holds a
/// debug build's checking of any module on every target, while lowering
/// that recursed once per nested definition needed several times more at
/// these depths.
const small_stack_size = 8 * 1024 * 1024;

fn expectLowersOnSmallStack(source: []const u8) (std.Thread.SpawnError || harness.LowerToLirHarnessError)!void {
    var result: harness.LowerToLirHarnessError!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = small_stack_size }, lowerOnSmallStack, .{ source, &result });
    thread.join();
    try result;
}

test "issue 13062: nested function definitions lower through Boxy on a small stack" {
    const gpa = std.testing.allocator;
    const source = try nestedSource(gpa, "", 1_000, "");
    defer gpa.free(source);
    try expectLowersOnSmallStack(source);
}

test "issue 13062: a literal requirement wrapped by every nested definition lowers through Boxy on a small stack" {
    const gpa = std.testing.allocator;
    // Each definition calls the next at `W` of its own result type, so the
    // next one's numeral dictionary is `W`'s, built from the enclosing one's.
    const prelude =
        \\W(a) := [W(a)].{
        \\    from_numeral : Numeral -> Try(W(a), [InvalidNumeral(Str)]) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
        \\    from_numeral = |numeral| {
        \\        Item : a
        \\        match Item.from_numeral(numeral) {
        \\            Ok(inner) => Ok(W.W(inner))
        \\            Err(e) => Err(e)
        \\        }
        \\    }
        \\}
        \\
        \\unwrap : W(a) -> a
        \\unwrap = |W.W(inner)| inner
        \\
        \\
    ;
    const source = try nestedSource(gpa, prelude, 500, "unwrap");
    defer gpa.free(source);
    try expectLowersOnSmallStack(source);
}

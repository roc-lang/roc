//! Deep source nesting and long statement sequences compile on a small stack.
//!
//! Every compiler stage must keep its native call depth independent of how
//! deeply source expressions nest and how long statement sequences are. Each
//! case runs its whole compile-and-evaluate pipeline on a thread whose stack is
//! far smaller than the depth times any per-level recursion cost, so a stage
//! that recurses once per nesting level or statement fails here
//! deterministically.
//!
//! https://github.com/roc-lang/roc/issues/11698

const std = @import("std");
const TestCase = @import("parallel_runner.zig").TestCase;

const depth = 10000;
const depth_str = std.fmt.comptimePrint("{d}", .{depth});
const stack_bytes = 8 * 1024 * 1024;

fn repeat(comptime text: []const u8, comptime count: usize) []const u8 {
    return text ** count;
}

/// Deep-nesting eval cases, each run on a `stack_bytes` stack.
pub const tests = [_]TestCase{
    .{
        .name = "issue 11698: long left-associative + chain",
        .source_kind = .module,
        .source = "sum = |x| x" ++ repeat(" + x", depth - 1) ++ "\nmain = sum(1.U64)\n",
        .expected = .{ .inspect_str = depth_str },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: deeply nested calls",
        .source_kind = .module,
        .source = "inc = |x| x + 1\nmain = " ++ repeat("inc(", depth) ++ "0.U64" ++ repeat(")", depth) ++ "\n",
        .expected = .{ .inspect_str = depth_str },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: long straight-line block",
        .source_kind = .module,
        .source = "count = |start| {\n    var $x = start\n" ++ repeat("    $x = $x + 1\n", depth) ++ "    $x\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = depth_str },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: long string interpolation",
        .source_kind = .module,
        .source = "render = |s| \"" ++ repeat("${s}", depth) ++ "\"\nmain = Str.count_utf8_bytes(render(\"a\"))\n",
        .expected = .{ .inspect_str = depth_str },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: deeply nested lists",
        .source_kind = .module,
        .source = "main = List.len(" ++ repeat("[", depth) ++ "1.U64" ++ repeat("]", depth) ++ ")\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: deeply nested records",
        .source_kind = .module,
        .source = "main = " ++ repeat("{ a: ", depth) ++ "1.U64" ++ repeat(" }", depth) ++ repeat(".a", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
};

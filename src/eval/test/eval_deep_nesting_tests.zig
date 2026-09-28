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

const depth = 5000;
const depth_str = std.fmt.comptimePrint("{d}", .{depth});
const stack_bytes = 4 * 1024 * 1024;

fn repeat(comptime text: []const u8, comptime count: usize) []const u8 {
    return text ** count;
}

/// Deep-nesting eval cases, each run on a `stack_bytes` stack under both
/// specialization strategies.
pub const tests = cases ++ boxyVariants(&cases);

/// Each case again, lowered without specialization (`--specialize=no`).
/// The wasm evaluator runs a module without the Boxy runtime object that a
/// `roc build` links in, so these variants run on the native backends.
fn boxyVariants(comptime lss: []const TestCase) [lss.len]TestCase {
    var out: [lss.len]TestCase = undefined;
    for (lss, &out) |case, *variant| {
        variant.* = case;
        variant.name = case.name ++ " (specialize=no)";
        variant.specialization_strategy = .boxy;
        variant.skip.wasm = true;
    }
    return out;
}

const cases = [_]TestCase{
    .{
        .name = "TEMPDEEP else-if chain",
        .source_kind = .module,
        .source = "pick = |x| " ++ repeat("if x == 0 { 0.U64 } else ", depth) ++ "{ x }\nmain = pick(5.U64)\n",
        .expected = .{ .inspect_str = "5" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP nested parens",
        .source_kind = .module,
        .source = "main = " ++ repeat("(", depth) ++ "1.U64" ++ repeat(" + 1)", depth) ++ "\n",
        .expected = .{ .inspect_str = std.fmt.comptimePrint("{d}", .{depth + 1}) },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP nested tuples",
        .source_kind = .module,
        .source = "main = " ++ repeat("(", depth) ++ "1.U64" ++ repeat(", 2)", depth) ++ repeat(".0", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP nested tags",
        .source_kind = .module,
        // Rendering the value would recurse once per level at runtime, which
        // is the program's own depth; the untaken branch still compiles the
        // rendering of the deep type.
        .source = "nested = " ++ repeat("Ok(", depth) ++ "1.U64" ++ repeat(")", depth) ++
            "\nrender = |n| if n == 0 { 1.U64 } else { Str.count_utf8_bytes(Str.inspect(nested)) }\nmain = render(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP nested blocks",
        .source_kind = .module,
        .source = "main = " ++ repeat("{ ", depth) ++ "1.U64" ++ repeat(" }", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP wide match",
        .source_kind = .module,
        .source = "pick = |x| match x {\n" ++ blk: {
            @setEvalBranchQuota(10_000_000);
            var out: []const u8 = "";
            for (0..depth) |i| out = out ++ std.fmt.comptimePrint("    {d} => {d}.U64\n", .{ i, i });
            break :blk out;
        } ++ "    _ => 0.U64\n}\nmain = pick(7.U64)\n",
        .expected = .{ .inspect_str = "7" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP deep annotation",
        .source_kind = .module,
        .source = "f : " ++ repeat("List(", depth) ++ "U64" ++ repeat(")", depth) ++ " -> U64\nf = |_| 1\nmain = f([])\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP deep list pattern",
        .source_kind = .module,
        .source = "f = |l| match l { " ++ repeat("[", depth) ++ "x" ++ repeat("]", depth) ++ " => x, _ => 0.U64 }\nmain = f(" ++ repeat("[", depth) ++ "1.U64" ++ repeat("]", depth) ++ ")\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP curried lambdas",
        .source_kind = .module,
        .source = "f = " ++ repeat("|_| ", depth) ++ "1.U64\nmain = f" ++ repeat("(0)", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP deep equality",
        .source_kind = .module,
        // Comparing the values would recurse once per level at runtime, which
        // is the program's own depth; the untaken branch still compiles the
        // equality of the deep type.
        .source = "nested = " ++ repeat("Ok(", depth) ++ "1.U64" ++ repeat(")", depth) ++
            "\ncompare = |n| if n == 0 { 1.U64 } else if nested == nested { 2.U64 } else { 3.U64 }\nmain = compare(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP long call chain",
        .source_kind = .module,
        // Running the chain would recurse once per function at runtime, which
        // is the program's own depth; the untaken branch still compiles every
        // function in the chain.
        .source = blk: {
            @setEvalBranchQuota(10_000_000);
            var out: []const u8 = "";
            for (0..depth) |i| out = out ++ std.fmt.comptimePrint("f{d} : U64 -> U64\nf{d} = |x| f{d}(x + 1)\n", .{ i, i, i + 1 });
            break :blk out;
        } ++ "f" ++ depth_str ++ " : U64 -> U64\nf" ++ depth_str ++ " = |x| x\nrun = |n| if n == 0 { 1.U64 } else { f0(n) }\nmain = run(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP nested for loops",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("for _ in [1.U64] {\n", depth) ++ "$total = $total + 1\n" ++ repeat("}\n", depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP nested matches",
        .source_kind = .module,
        .source = "pick = |x| " ++ repeat("match x { 0 => 0.U64, _ => ", depth) ++ "x" ++ repeat(" }", depth) ++ "\nmain = pick(3.U64)\n",
        .expected = .{ .inspect_str = "3" },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
    .{
        .name = "TEMPDEEP long flat list",
        .source_kind = .module,
        .source = "main = List.len([" ++ repeat("1.U64, ", depth) ++ "])\n",
        .expected = .{ .inspect_str = depth_str },
        .stack_bytes = stack_bytes,
        .opt_in = true,
    },
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

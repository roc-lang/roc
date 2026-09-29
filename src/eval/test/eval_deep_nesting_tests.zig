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

/// Shapes whose compilation does work superlinear in their depth nest less
/// deeply, on a stack still far smaller than that depth times any per-level
/// recursion cost.
const shallow_depth = 1000;
const shallow_depth_str = std.fmt.comptimePrint("{d}", .{shallow_depth});
const shallow_stack_bytes = 2 * 1024 * 1024;

/// A deep equality's case fusion, a curried lambda chain's instantiated
/// types, and a method-dispatch chain's waiting dispatch constraints cost the
/// square of their depth.
const shallower_depth = 500;
const shallower_depth_str = std.fmt.comptimePrint("{d}", .{shallower_depth});

/// A loop nest's liveness facts grow with the product of its size and its
/// depth.
const loop_depth = 300;

fn repeat(comptime text: []const u8, comptime count: usize) []const u8 {
    return text ** count;
}

/// Deep-nesting eval cases, each run on a small stack under both
/// specialization strategies.
pub const tests = cases ++ boxyVariants(&cases) ++ probe_cases;

const probe_depth = 2000;
fn nestedRecord(comptime n: usize) []const u8 {
    return repeat("{ a: ", n) ++ "1.U64" ++ repeat(" }", n);
}
fn probe(comptime name: []const u8, comptime source: []const u8, comptime expected: []const u8) TestCase {
    return .{
        .name = "TEMPDEEP " ++ name,
        .source_kind = .module,
        .source = source,
        .expected = .{ .inspect_str = expected },
        .stack_bytes = 2 * 1024 * 1024,
        .specialization_strategy = .boxy,
        .opt_in = true,
        .skip = .{ .wasm = true },
    };
}
const probe_cases = [_]TestCase{
    probe("boxy generic identity", "id = |x| x\nmain = id(" ++ nestedRecord(probe_depth) ++ ")" ++ repeat(".a", probe_depth) ++ "\n", "1"),
    probe("boxy inspect", "main = Str.inspect(" ++ nestedRecord(probe_depth) ++ ")\n", "\"" ++ repeat("{ a: ", probe_depth) ++ "1" ++ repeat(" }", probe_depth) ++ "\""),
    probe("boxy equality", "main = " ++ nestedRecord(probe_depth) ++ " == " ++ nestedRecord(probe_depth) ++ "\n", "True"),
    probe("boxy eq shallow", "main = " ++ nestedRecord(1) ++ " == " ++ nestedRecord(1) ++ "\n", "True"),
    probe("boxy bool const", "main = Bool.True\n", "True"),
    probe("boxy generic eq", "eq = |x, y| x == y\nmain = eq(" ++ nestedRecord(probe_depth) ++ ", " ++ nestedRecord(probe_depth) ++ ")\n", "True"),
    probe("boxy generic inspect", "show = |x| Str.inspect(x)\nmain = show(" ++ nestedRecord(probe_depth) ++ ")\n", "\"" ++ repeat("{ a: ", probe_depth) ++ "1" ++ repeat(" }", probe_depth) ++ "\""),
    probe("boxy generic list", "wrap = |x| [x, x]\nmain = wrap(" ++ nestedRecord(probe_depth) ++ ").len()\n", "2"),
    probe("boxy generic tuple", "swap = |p| (p.1, p.0)\nmain = swap((" ++ nestedRecord(probe_depth) ++ ", 7.U64)).0\n", "7"),
    probe("boxy set", "main = Set.from_list([" ++ nestedRecord(probe_depth) ++ ", " ++ nestedRecord(probe_depth) ++ "]).len()\n", "1"),
    probe("lss generic identity", "id = |x| x\nmain = id(" ++ nestedRecord(probe_depth) ++ ")" ++ repeat(".a", probe_depth) ++ "\n", "1"),
};

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
        .name = "issue 11698: else-if chain",
        .source_kind = .module,
        .source = "pick = |x| " ++ repeat("if x == 0 { 0.U64 } else ", depth) ++ "{ x }\nmain = pick(5.U64)\n",
        .expected = .{ .inspect_str = "5" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: nested parens",
        .source_kind = .module,
        .source = "main = " ++ repeat("(", depth) ++ "1.U64" ++ repeat(" + 1)", depth) ++ "\n",
        .expected = .{ .inspect_str = std.fmt.comptimePrint("{d}", .{depth + 1}) },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: nested tuples",
        .source_kind = .module,
        .source = "main = " ++ repeat("(", shallow_depth) ++ "1.U64" ++ repeat(", 2)", shallow_depth) ++ repeat(".0", shallow_depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested tags",
        .source_kind = .module,
        // Rendering the value would recurse once per level at runtime, which
        // is the program's own depth; the untaken branch still compiles the
        // rendering of the deep type.
        .source = "nested = " ++ repeat("Ok(", shallow_depth) ++ "1.U64" ++ repeat(")", shallow_depth) ++
            "\nrender = |n| if n == 0 { 1.U64 } else { Str.count_utf8_bytes(Str.inspect(nested)) }\nmain = render(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested blocks",
        .source_kind = .module,
        .source = "main = " ++ repeat("{ ", depth) ++ "1.U64" ++ repeat(" }", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: wide match",
        .source_kind = .module,
        .source = "pick = |x| match x {\n" ++ blk: {
            @setEvalBranchQuota(10_000_000);
            var out: []const u8 = "";
            for (0..depth) |i| out = out ++ std.fmt.comptimePrint("    {d} => {d}.U64\n", .{ i, i });
            break :blk out;
        } ++ "    _ => 0.U64\n}\nmain = pick(7.U64)\n",
        .expected = .{ .inspect_str = "7" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: deep annotation",
        .source_kind = .module,
        .source = "f : " ++ repeat("List(", depth) ++ "U64" ++ repeat(")", depth) ++ " -> U64\nf = |_| 1\nmain = f([])\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: deep list pattern",
        .source_kind = .module,
        .source = "f = |l| match l { " ++ repeat("[", shallow_depth) ++ "x" ++ repeat("]", shallow_depth) ++ " => x, _ => 0.U64 }\nmain = f(" ++ repeat("[", shallow_depth) ++ "1.U64" ++ repeat("]", shallow_depth) ++ ")\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: curried lambdas",
        .source_kind = .module,
        .source = "f = " ++ repeat("|_| ", shallower_depth) ++ "1.U64\nmain = f" ++ repeat("(0)", shallower_depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: deep equality",
        .source_kind = .module,
        // Comparing the values would recurse once per level at runtime, which
        // is the program's own depth; the untaken branch still compiles the
        // equality of the deep type.
        .source = "nested = " ++ repeat("Ok(", shallower_depth) ++ "1.U64" ++ repeat(")", shallower_depth) ++
            "\ncompare = |n| if n == 0 { 1.U64 } else if nested == nested { 2.U64 } else { 3.U64 }\nmain = compare(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: long call chain",
        .source_kind = .module,
        // Running the chain would recurse once per function at runtime, which
        // is the program's own depth; the untaken branch still compiles every
        // function in the chain.
        .source = blk: {
            @setEvalBranchQuota(10_000_000);
            var out: []const u8 = "";
            for (0..shallow_depth) |i| out = out ++ std.fmt.comptimePrint("f{d} : U64 -> U64\nf{d} = |x| f{d}(x + 1)\n", .{ i, i, i + 1 });
            break :blk out;
        } ++ "f" ++ shallow_depth_str ++ " : U64 -> U64\nf" ++ shallow_depth_str ++ " = |x| x\nrun = |n| if n == 0 { 1.U64 } else { f0(n) }\nmain = run(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: method dispatch chain",
        .source_kind = .module,
        // Each method's body dispatches on the next type's value, whose
        // method checking resolves on demand.
        .source = blk: {
            @setEvalBranchQuota(10_000_000);
            var out: []const u8 = "";
            for (0..shallower_depth) |i| out = out ++ std.fmt.comptimePrint("T{d} := [V{d}(U64)].{{\n    step = |T{d}.V{d}(x)| T{d}.V{d}(x).step()\n}}\n", .{ i, i, i, i, i + 1, i + 1 });
            break :blk out;
        } ++ "T" ++ shallower_depth_str ++ " := [V" ++ shallower_depth_str ++ "(U64)].{\n    step = |T" ++ shallower_depth_str ++ ".V" ++ shallower_depth_str ++ "(x)| x\n}\nrun = |n| if n == 0 { 1.U64 } else { T0.V0(n).step() }\nmain = run(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested for loops",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("for _ in [1.U64] {\n", loop_depth) ++ "$total = $total + 1\n" ++ repeat("}\n", loop_depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested destructuring for loops",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("for [a] in [[1.U64]] {\n", loop_depth) ++ "$total = $total + a\n" ++ repeat("}\n", loop_depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested while loops",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("while $total < 1 {\n", loop_depth) ++ "$total = $total + 1\n" ++ repeat("}\n", loop_depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested stateful ifs",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("if $total == 0 {\n", shallow_depth) ++ "$total = $total + 1\n" ++ repeat("} else {}\n", shallow_depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested stateful matches",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("match $total {\n0 => {\n", shallow_depth) ++ "$total = $total + 1\n" ++ repeat("}\n_ => {}\n}\n", shallow_depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: nested divergent blocks",
        .source_kind = .module,
        .source = "pick = |x| if x == 0 { 1.U64 } else " ++ repeat("{\n", depth) ++ "crash \"deep\"" ++ repeat("\n}", depth) ++ "\nmain = pick(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: nested matches",
        .source_kind = .module,
        .source = "pick = |x| " ++ repeat("match x { 0 => 0.U64, _ => ", depth) ++ "x" ++ repeat(" }", depth) ++ "\nmain = pick(3.U64)\n",
        .expected = .{ .inspect_str = "3" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: long flat list",
        .source_kind = .module,
        .source = "main = List.len([" ++ repeat("1.U64, ", depth) ++ "])\n",
        .expected = .{ .inspect_str = depth_str },
        .stack_bytes = stack_bytes,
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

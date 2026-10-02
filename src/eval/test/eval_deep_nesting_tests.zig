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

/// Shapes whose compilation still does work superlinear in their depth nest
/// less deeply, on a stack still far smaller than that depth times any
/// per-level recursion cost.
const shallow_depth = 1000;
const shallow_depth_str = std.fmt.comptimePrint("{d}", .{shallow_depth});
const shallow_stack_bytes = 2 * 1024 * 1024;

/// A deep equality's canonical type keys, a curried lambda chain's
/// per-procedure function types, and a method-dispatch chain's waiting
/// dispatch obligations cost the square of their depth.
const shallower_depth = 500;
const shallower_depth_str = std.fmt.comptimePrint("{d}", .{shallower_depth});

/// A loop nest's liveness facts grow with the product of its size and its
/// depth.
const loop_depth = 300;

/// Lowering a custom-parser chain for compile-time evaluation relates each
/// level's codec contract type, which holds every level inside it.
const codec_chain_depth = 100;

fn repeat(comptime text: []const u8, comptime count: usize) []const u8 {
    return text ** count;
}

/// A custom parser whose where-clause asks for its argument's parser, applied
/// to a record holding the next level: settling each level's custom parser
/// validates the derived parser of the record inside it.
const codec_chain_prelude =
    \\Format := [Default].{
    \\    parse_u64 : Format, State -> Try({ value : U64, rest : State }, [FormatError])
    \\    parse_u64 = |_, state|
    \\        match state {
    \\            Present(value) => Ok({ value, rest: Done })
    \\            Done => Err(FormatError)
    \\        }
    \\
    \\    parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError])
    \\    parse_record_start = |_, state| Ok(Uncounted(state))
    \\
    \\    parse_record_field : Format,
    \\    Encoding.FieldName.FieldNames(_shape),
    \\    State -> Try(
    \\        [
    \\            Field({ field : Encoding.FieldName(_shape), rest : State }),
    \\            TryField({ name : Str, rest : State }),
    \\            TryFieldCaseless({ name : Str, rest : State }),
    \\            Continue(State),
    \\            Done(State),
    \\        ],
    \\        [FormatError],
    \\    )
    \\    parse_record_field = |_, _, state| Ok(Done(state))
    \\
    \\    parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError])
    \\    parse_record_after_field = |_, state| Ok(Continue(state))
    \\
    \\    rename_field : Format, Str -> Str
    \\    rename_field = |_, name| name
    \\
    \\    skip_record_field : Format, State -> Try(State, [FormatError])
    \\    skip_record_field = |_, _| Ok(Done)
    \\}
    \\
    \\State := [Present(U64), Done]
    \\
    \\Wrap(a) := [Wrap(a)].{
    \\    parser_for : Format -> (State -> Try({ value : Wrap(a), rest : State }, [FormatError, ..errs]))
    \\        where [a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs]))]
    \\    parser_for = |format| |state| {
    \\        Inner : a
    \\        parse_inner = Inner.parser_for(format)
    \\        parsed = parse_inner(state)?
    \\        Ok({ value: Wrap.Wrap(parsed.value), rest: parsed.rest })
    \\    }
    \\}
    \\
    \\parse : State -> Try(a, [FormatError, ..errs])
    \\    where [
    \\        a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
    \\    ]
    \\parse = |input| {
    \\    Shape : a
    \\    parse_shape = Shape.parser_for(Format.Default)
    \\    parsed = parse_shape(input)?
    \\    Ok(parsed.value)
    \\}
    \\
;

fn codecChain(comptime n: usize) []const u8 {
    return codec_chain_prelude ++
        "probe : State -> Try(" ++ repeat("Wrap({ x : ", n) ++ "U64" ++ repeat(" })", n) ++ ", [FormatError])\n" ++
        "probe = |input| parse(input)\n\n" ++
        "main = match probe(State.Done) {\n    Ok(_) => \"ok\"\n    Err(_) => \"err\"\n}\n";
}

/// Deep-nesting eval cases, each run on a small stack under both
/// specialization strategies.
pub const tests = cases ++ boxyVariants(&cases) ++ boxy_cases;

fn nestedRecord(comptime n: usize) []const u8 {
    return repeat("{ a: ", n) ++ "1.U64" ++ repeat(" }", n);
}

fn boxyCase(comptime name: []const u8, comptime source: []const u8, comptime expected: []const u8) TestCase {
    return .{
        .name = "issue 11698: " ++ name ++ " (specialize=no)",
        .source_kind = .module,
        .source = source,
        .expected = .{ .inspect_str = expected },
        .stack_bytes = stack_bytes,
        .specialization_strategy = .boxy,
        .skip = .{ .wasm = true },
    };
}

/// Deep types flowing through generic code, whose runtime descriptors and
/// dictionaries Boxy builds from the type's structure.
const boxy_cases = [_]TestCase{
    boxyCase("deep record through a generic identity", "id = |x| x\nmain = id(" ++ nestedRecord(depth) ++ ")" ++ repeat(".a", depth) ++ "\n", "1"),
    boxyCase("deep record inspected", "main = Str.inspect(" ++ nestedRecord(depth) ++ ")\n", "\"" ++ repeat("{ a: ", depth) ++ "1" ++ repeat(" }", depth) ++ "\""),
    boxyCase("deep record equality", "main = " ++ nestedRecord(depth) ++ " == " ++ nestedRecord(depth) ++ "\n", "True"),
    boxyCase("deep record through generic equality", "eq = |x, y| x == y\nmain = eq(" ++ nestedRecord(depth) ++ ", " ++ nestedRecord(depth) ++ ")\n", "True"),
    boxyCase("deep record through generic inspect", "show = |x| Str.inspect(x)\nmain = show(" ++ nestedRecord(depth) ++ ")\n", "\"" ++ repeat("{ a: ", depth) ++ "1" ++ repeat(" }", depth) ++ "\""),
    boxyCase("deep record in a generic list", "wrap = |x| [x, x]\nmain = wrap(" ++ nestedRecord(depth) ++ ").len()\n", "2"),
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
        .source = "main = " ++ repeat("(", depth) ++ "1.U64" ++ repeat(", 2)", depth) ++ repeat(".0", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
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
        .source = "f = |l| match l { " ++ repeat("[", depth) ++ "x" ++ repeat("]", depth) ++ " => x, _ => 0.U64 }\nmain = f(" ++ repeat("[", depth) ++ "1.U64" ++ repeat("]", depth) ++ ")\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
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
        // function in the chain. Building this source at comptime copies its
        // whole prefix once per function, so the chain is shallower.
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
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("if $total == 0 {\n", depth) ++ "$total = $total + 1\n" ++ repeat("} else {}\n", depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: nested stateful matches",
        .source_kind = .module,
        .source = "count = |n| {\n    var $total = n\n" ++ repeat("match $total {\n0 => {\n", depth) ++ "$total = $total + 1\n" ++ repeat("}\n_ => {}\n}\n", depth) ++ "    $total\n}\nmain = count(0.U64)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
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
        .name = "issue 11698: nested field access receivers",
        .source_kind = .module,
        .source = "main = " ++ repeat("{ a: ", depth) ++ "1.U64" ++ repeat(" }.a", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: deeply nested constant",
        .source_kind = .module,
        .source = "nested = " ++ repeat("[", depth) ++ "1.U64" ++ repeat("]", depth) ++ "\nmain = List.len(nested)\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: lambdas nested as call arguments",
        .source_kind = .module,
        .source = "apply = |f, x| f(x)\nmain = " ++ repeat("apply(|_| ", depth) ++ "1.U64" ++ repeat(", 0.U64)", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: closures nested as call arguments",
        .source_kind = .module,
        .source = "apply = |f, x| f(x)\nmain = {\n    a = 1.U64\n    " ++ repeat("apply(|_| ", depth) ++ "a" ++ repeat(", 0.U64)", depth) ++ "\n}\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
    .{
        .name = "issue 11698: lambdas nested as method arguments",
        .source_kind = .module,
        .source = "main = " ++ repeat("[1.U64].map(|_| ", shallow_depth) ++ "1.U64" ++ repeat(").len()", shallow_depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: custom parsers nesting derived record parsers",
        .source_kind = .module,
        .source = codecChain(codec_chain_depth),
        .expected = .{ .inspect_str = "\"err\"" },
        .stack_bytes = shallow_stack_bytes,
    },
    .{
        .name = "issue 11698: deeply nested records",
        .source_kind = .module,
        .source = "main = " ++ repeat("{ a: ", depth) ++ "1.U64" ++ repeat(" }", depth) ++ repeat(".a", depth) ++ "\n",
        .expected = .{ .inspect_str = "1" },
        .stack_bytes = stack_bytes,
    },
};

//! End-to-end coverage for the TRMC + TCE pass (src/lir/trmc.zig).
//!
//! These programs build recursive structures through non-tail recursive
//! constructors — the shape that stack-overflowed before TRMC. The deep
//! cases (n well past the interpreter's 1024-frame call cap) are the
//! correctness gates: they only pass because the pass rewrites the
//! recursion into a loop. Outputs must stay byte-identical across all
//! backends, like every case in this runner.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "trmc: linked list repeat and naive length",
        .source_kind = .module,
        .source =
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\repeat : I64, I64 -> LinkedList
        \\repeat = |value, n| if n <= 0.I64 LinkedList.Nil else LinkedList.Cons(value, repeat(value, n - 1))
        \\
        \\length : LinkedList -> I64
        \\length = |list| match list {
        \\    Nil => 0
        \\    Cons(_, rest) => 1 + length(rest)
        \\}
        \\
        \\main = length(repeat(7.I64, 5.I64))
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "trmc: repeat builds the full structure",
        .source_kind = .module,
        .source =
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\repeat : I64, I64 -> LinkedList
        \\repeat = |value, n| if n <= 0.I64 LinkedList.Nil else LinkedList.Cons(value, repeat(value, n - 1))
        \\
        \\main = repeat(7.I64, 3.I64)
        ,
        .expected = .{ .inspect_str = "Cons(7, Cons(7, Cons(7, Nil)))" },
    },
    .{
        .name = "trmc: repeat with Str elements (loop-carried refcounted param)",
        .source_kind = .module,
        .source =
        \\StrList := [Nil, Cons(Str, StrList)]
        \\
        \\repeat : Str, I64 -> StrList
        \\repeat = |value, n| if n <= 0.I64 StrList.Nil else StrList.Cons(value, repeat(value, n - 1))
        \\
        \\main = repeat("this string is long enough to be refcounted on the heap", 3.I64)
        ,
        .expected = .{ .inspect_str = "Cons(\"this string is long enough to be refcounted on the heap\", Cons(\"this string is long enough to be refcounted on the heap\", Cons(\"this string is long enough to be refcounted on the heap\", Nil)))" },
    },
    .{
        .name = "trmc: linked list map",
        .source_kind = .module,
        .source =
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\count_up : I64, I64 -> LinkedList
        \\count_up = |from, to| if from > to LinkedList.Nil else LinkedList.Cons(from, count_up(from + 1, to))
        \\
        \\map : LinkedList, (I64 -> I64) -> LinkedList
        \\map = |list, f| match list {
        \\    Nil => LinkedList.Nil
        \\    Cons(x, rest) => LinkedList.Cons(f(x), map(rest, f))
        \\}
        \\
        \\main = map(count_up(1.I64, 4.I64), |x| x * 10)
        ,
        .expected = .{ .inspect_str = "Cons(10, Cons(20, Cons(30, Cons(40, Nil))))" },
    },
    .{
        .name = "trmc: linked list filter mixes construct and plain tail branches",
        .source_kind = .module,
        .source =
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\count_up : I64, I64 -> LinkedList
        \\count_up = |from, to| if from > to LinkedList.Nil else LinkedList.Cons(from, count_up(from + 1, to))
        \\
        \\keep_even : LinkedList -> LinkedList
        \\keep_even = |list| match list {
        \\    Nil => LinkedList.Nil
        \\    Cons(x, rest) => if x % 2 == 0 LinkedList.Cons(x, keep_even(rest)) else keep_even(rest)
        \\}
        \\
        \\main = keep_even(count_up(1.I64, 8.I64))
        ,
        .expected = .{ .inspect_str = "Cons(2, Cons(4, Cons(6, Cons(8, Nil))))" },
    },
    .{
        .name = "trmc: binary tree build (one of two recursive calls loops) and sum",
        .source_kind = .module,
        .source =
        \\Tree := [Leaf, Node(Tree, I64, Tree)]
        \\
        \\build : I64 -> Tree
        \\build = |d| if d <= 0.I64 Tree.Leaf else Tree.Node(build(d - 1), d, build(d - 1))
        \\
        \\total : Tree -> I64
        \\total = |tree| match tree {
        \\    Leaf => 0
        \\    Node(left, v, right) => total(left) + v + total(right)
        \\}
        \\
        \\main = total(build(8.I64))
        ,
        // sum over a full tree of depth 8: sum(d) = 2*sum(d-1) + d for the
        // value at each node => 502
        .expected = .{ .inspect_str = "502" },
    },
    // NOTE: allocation-count parity (exactly one heap cell per node) is pinned
    // by the hand-built unit tests in trmc_lir_test.zig; the runner's
    // allocations_at_most path only supports expression sources, and recursive
    // nominal types require module sources.
    .{
        .name = "trmc: control - result used twice is not transformed and still correct",
        .source_kind = .module,
        .source =
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\length_acc : LinkedList, I64 -> I64
        \\length_acc = |list, acc| match list {
        \\    Nil => acc
        \\    Cons(_, rest) => length_acc(rest, acc + 1)
        \\}
        \\
        \\with_lengths : I64 -> LinkedList
        \\with_lengths = |n| if n <= 0.I64 LinkedList.Nil else {
        \\    rest = with_lengths(n - 1)
        \\    LinkedList.Cons(length_acc(rest, 0), rest)
        \\}
        \\
        \\main = with_lengths(4.I64)
        ,
        .expected = .{ .inspect_str = "Cons(3, Cons(2, Cons(1, Cons(0, Nil))))" },
    },
    .{
        .name = "trmc: stack gate - repeat far past the interpreter call cap",
        .source_kind = .module,
        .source =
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\repeat : I64, I64 -> LinkedList
        \\repeat = |value, n| if n <= 0.I64 LinkedList.Nil else LinkedList.Cons(value, repeat(value, n - 1))
        \\
        \\length_acc : LinkedList, I64 -> I64
        \\length_acc = |list, acc| match list {
        \\    Nil => acc
        \\    Cons(_, rest) => length_acc(rest, acc + 1)
        \\}
        \\
        \\main = length_acc(repeat(0.I64, 100000.I64), 0.I64)
        ,
        .expected = .{ .inspect_str = "100000" },
    },
    .{
        .name = "tce: stack gate - accumulator sum of one million",
        .source_kind = .module,
        .source =
        \\sum_to : I64, I64 -> I64
        \\sum_to = |n, acc| if n == 0.I64 acc else sum_to(n - 1, acc + n)
        \\
        \\main = sum_to(1000000.I64, 0.I64)
        ,
        .expected = .{ .inspect_str = "500000500000" },
    },
};

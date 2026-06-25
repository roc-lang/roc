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

    // ── Benchmark ports (acceptance criteria for the TRMC work) ──
    //
    // Ported from the old compiler's TRMC-era benchmarks at commit ae47cc5171
    // (crates/cli_testing_examples/benchmarks/), adapted to current syntax.
    // Sizes are chosen so the recursion shapes TRMC/TCE rewrites would
    // overflow the interpreter's 1024-frame call cap without the pass, while
    // keeping CI time reasonable.

    .{
        // mk_expr's second recursive call loops via TRMC (the first stays a
        // real call, bounded by tree depth); append_add/append_mul are pure
        // constructor tail recursion over reassociated spines. eval and
        // const_folding are inherently non-tail (the old benchmark needed an
        // unlimited stack even with TRMC), so n stays at 9 (512-node spines).
        .name = "trmc benchmark: CFold constant folding (n=9)",
        .source_kind = .module,
        .source =
        \\Expr := [Add(Expr, Expr), Mul(Expr, Expr), Val(I64), Var(I64)]
        \\
        \\max_i64 : I64, I64 -> I64
        \\max_i64 = |a, b| if a > b a else b
        \\
        \\mk_expr : I64, I64 -> Expr
        \\mk_expr = |n, v|
        \\    if n == 0 {
        \\        if v == 0 Expr.Var(1) else Expr.Val(v)
        \\    } else {
        \\        Expr.Add(mk_expr(n - 1, v + 1), mk_expr(n - 1, max_i64(v - 1, 0)))
        \\    }
        \\
        \\append_add : Expr, Expr -> Expr
        \\append_add = |e1, e2| match e1 {
        \\    Add(a1, a2) => Expr.Add(a1, append_add(a2, e2))
        \\    _ => Expr.Add(e1, e2)
        \\}
        \\
        \\append_mul : Expr, Expr -> Expr
        \\append_mul = |e1, e2| match e1 {
        \\    Mul(a1, a2) => Expr.Mul(a1, append_mul(a2, e2))
        \\    _ => Expr.Mul(e1, e2)
        \\}
        \\
        \\eval_expr : Expr -> I64
        \\eval_expr = |e| match e {
        \\    Var(_) => 0
        \\    Val(v) => v
        \\    Add(l, r) => eval_expr(l) + eval_expr(r)
        \\    Mul(l, r) => eval_expr(l) * eval_expr(r)
        \\}
        \\
        \\reassoc : Expr -> Expr
        \\reassoc = |e| match e {
        \\    Add(e1, e2) => append_add(reassoc(e1), reassoc(e2))
        \\    Mul(e1, e2) => append_mul(reassoc(e1), reassoc(e2))
        \\    _ => e
        \\}
        \\
        \\# The old benchmark's or-patterns (`Add (Val b) x | Add x (Val b)`)
        \\# are written out as nested matches.
        \\const_folding : Expr -> Expr
        \\const_folding = |e| match e {
        \\    Add(e1, e2) => {
        \\        x1 = const_folding(e1)
        \\        x2 = const_folding(e2)
        \\        match x1 {
        \\            Val(a) => match x2 {
        \\                Val(b) => Expr.Val(a + b)
        \\                Add(y1, y2) => match y1 {
        \\                    Val(b) => Expr.Add(Expr.Val(a + b), y2)
        \\                    _ => match y2 {
        \\                        Val(b) => Expr.Add(Expr.Val(a + b), y1)
        \\                        _ => Expr.Add(Expr.Val(a), Expr.Add(y1, y2))
        \\                    }
        \\                }
        \\                _ => Expr.Add(x1, x2)
        \\            }
        \\            _ => Expr.Add(x1, x2)
        \\        }
        \\    }
        \\    Mul(e1, e2) => {
        \\        x1 = const_folding(e1)
        \\        x2 = const_folding(e2)
        \\        match x1 {
        \\            Val(a) => match x2 {
        \\                Val(b) => Expr.Val(a * b)
        \\                Mul(y1, y2) => match y1 {
        \\                    Val(b) => Expr.Mul(Expr.Val(a * b), y2)
        \\                    _ => match y2 {
        \\                        Val(b) => Expr.Mul(Expr.Val(a * b), y1)
        \\                        _ => Expr.Mul(Expr.Val(a), Expr.Mul(y1, y2))
        \\                    }
        \\                }
        \\                _ => Expr.Mul(x1, x2)
        \\            }
        \\            _ => Expr.Mul(x1, x2)
        \\        }
        \\    }
        \\    _ => e
        \\}
        \\
        \\main = {
        \\    e = mk_expr(9.I64, 1.I64)
        \\    unoptimized = eval_expr(e)
        \\    optimized = eval_expr(const_folding(reassoc(e)))
        \\    (unoptimized, optimized)
        \\}
        ,
        .expected = .{ .inspect_str = "(1130, 1130)" },
    },
    .{
        // At n=9 the partial-solution lists reach 2828 elements, so extend /
        // append_safe / length_help all recurse far past the interpreter's
        // 1024-frame cap without TCE.
        .name = "trmc benchmark: NQueens (n=9)",
        .source_kind = .module,
        .source =
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\queens : I64 -> I64
        \\queens = |n| length_help(find_solutions(n, n), 0)
        \\
        \\find_solutions : I64, I64 -> ConsList(ConsList(I64))
        \\find_solutions = |n, k|
        \\    if k <= 0 {
        \\        ConsList.Cons(ConsList.Nil, ConsList.Nil)
        \\    } else {
        \\        extend(n, ConsList.Nil, find_solutions(n, k - 1))
        \\    }
        \\
        \\extend : I64, ConsList(ConsList(I64)), ConsList(ConsList(I64)) -> ConsList(ConsList(I64))
        \\extend = |n, acc, solutions| match solutions {
        \\    Nil => acc
        \\    Cons(soln, rest) => extend(n, append_safe(n, soln, acc), rest)
        \\}
        \\
        \\append_safe : I64, ConsList(I64), ConsList(ConsList(I64)) -> ConsList(ConsList(I64))
        \\append_safe = |k, soln, solns|
        \\    if k <= 0 {
        \\        solns
        \\    } else if safe(k, 1, soln) {
        \\        append_safe(k - 1, soln, ConsList.Cons(ConsList.Cons(k, soln), solns))
        \\    } else {
        \\        append_safe(k - 1, soln, solns)
        \\    }
        \\
        \\safe : I64, I64, ConsList(I64) -> Bool
        \\safe = |queen, diagonal, xs| match xs {
        \\    Nil => True
        \\    Cons(q, t) =>
        \\        if queen != q and queen != q + diagonal and queen != q - diagonal {
        \\            safe(queen, diagonal + 1, t)
        \\        } else {
        \\            False
        \\        }
        \\}
        \\
        \\length_help : ConsList(ConsList(I64)), I64 -> I64
        \\length_help = |xs, acc| match xs {
        \\    Cons(_, rest) => length_help(rest, 1 + acc)
        \\    Nil => acc
        \\}
        \\
        \\main = queens(9.I64)
        ,
        .expected = .{ .inspect_str = "352" },
    },
    .{
        // make_map_help is the TCE gate: 2000 sequential inserts would be
        // 2000 stack frames without the pass. The checkpoint list keeps every
        // fifth map alive, stressing shared persistent-tree refcounting.
        .name = "trmc benchmark: RBTreeCk red-black tree checkpoints (n=2000)",
        .source_kind = .module,
        .source =
        \\Tree := [Leaf, Node([Red, Black], Tree, I64, Bool, Tree)]
        \\
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\make_map : I64, I64 -> ConsList(Tree)
        \\make_map = |freq, n| make_map_help(freq, n, Tree.Leaf, ConsList.Nil)
        \\
        \\make_map_help : I64, I64, Tree, ConsList(Tree) -> ConsList(Tree)
        \\make_map_help = |freq, n, m, acc|
        \\    if n == 0 {
        \\        ConsList.Cons(m, acc)
        \\    } else {
        \\        power_of_10 = n % 10 == 0
        \\        m1 = insert(m, n, power_of_10)
        \\        is_frequency = n % freq == 0
        \\        x = if is_frequency ConsList.Cons(m1, acc) else acc
        \\        make_map_help(freq, n - 1, m1, x)
        \\    }
        \\
        \\fold_count : Tree, I64 -> I64
        \\fold_count = |tree, b| match tree {
        \\    Leaf => b
        \\    Node(_, l, _, v, r) => fold_count(r, (if v fold_count(l, b) + 1 else fold_count(l, b)))
        \\}
        \\
        \\insert : Tree, I64, Bool -> Tree
        \\insert = |t, k, v| if is_red(t) set_black(ins(t, k, v)) else ins(t, k, v)
        \\
        \\set_black : Tree -> Tree
        \\set_black = |tree| match tree {
        \\    Node(_, l, k, v, r) => Tree.Node(Black, l, k, v, r)
        \\    _ => tree
        \\}
        \\
        \\is_red : Tree -> Bool
        \\is_red = |tree| match tree {
        \\    Node(Red, _, _, _, _) => True
        \\    _ => False
        \\}
        \\
        \\ins : Tree, I64, Bool -> Tree
        \\ins = |tree, kx, vx| match tree {
        \\    Leaf => Tree.Node(Red, Tree.Leaf, kx, vx, Tree.Leaf)
        \\    Node(Red, a, ky, vy, b) =>
        \\        if kx < ky {
        \\            Tree.Node(Red, ins(a, kx, vx), ky, vy, b)
        \\        } else if ky < kx {
        \\            Tree.Node(Red, a, ky, vy, ins(b, kx, vx))
        \\        } else {
        \\            Tree.Node(Red, a, ky, vy, ins(b, kx, vx))
        \\        }
        \\    Node(Black, a, ky, vy, b) =>
        \\        if kx < ky {
        \\            if is_red(a) {
        \\                balance1(Tree.Node(Black, Tree.Leaf, ky, vy, b), ins(a, kx, vx))
        \\            } else {
        \\                Tree.Node(Black, ins(a, kx, vx), ky, vy, b)
        \\            }
        \\        } else if ky < kx {
        \\            if is_red(b) {
        \\                balance2(Tree.Node(Black, a, ky, vy, Tree.Leaf), ins(b, kx, vx))
        \\            } else {
        \\                Tree.Node(Black, a, ky, vy, ins(b, kx, vx))
        \\            }
        \\        } else {
        \\            Tree.Node(Black, a, kx, vx, b)
        \\        }
        \\}
        \\
        \\balance1 : Tree, Tree -> Tree
        \\balance1 = |tree1, tree2| match tree1 {
        \\    Leaf => Tree.Leaf
        \\    Node(_, _, kv, vv, t) => match tree2 {
        \\        Node(_, Node(Red, l, kx, vx, r1), ky, vy, r2) =>
        \\            Tree.Node(Red, Tree.Node(Black, l, kx, vx, r1), ky, vy, Tree.Node(Black, r2, kv, vv, t))
        \\        Node(_, l1, ky, vy, Node(Red, l2, kx, vx, r)) =>
        \\            Tree.Node(Red, Tree.Node(Black, l1, ky, vy, l2), kx, vx, Tree.Node(Black, r, kv, vv, t))
        \\        Node(_, l, ky, vy, r) =>
        \\            Tree.Node(Black, Tree.Node(Red, l, ky, vy, r), kv, vv, t)
        \\        Leaf => Tree.Leaf
        \\    }
        \\}
        \\
        \\balance2 : Tree, Tree -> Tree
        \\balance2 = |tree1, tree2| match tree1 {
        \\    Leaf => Tree.Leaf
        \\    Node(_, t, kv, vv, _) => match tree2 {
        \\        Node(_, Node(Red, l, kx1, vx1, r1), ky, vy, r2) =>
        \\            Tree.Node(Red, Tree.Node(Black, t, kv, vv, l), kx1, vx1, Tree.Node(Black, r1, ky, vy, r2))
        \\        Node(_, l1, ky, vy, Node(Red, l2, kx2, vx2, r2)) =>
        \\            Tree.Node(Red, Tree.Node(Black, t, kv, vv, l1), ky, vy, Tree.Node(Black, l2, kx2, vx2, r2))
        \\        Node(_, l, ky, vy, r) =>
        \\            Tree.Node(Black, t, kv, vv, Tree.Node(Red, l, ky, vy, r))
        \\        Leaf => Tree.Leaf
        \\    }
        \\}
        \\
        \\main = match make_map(5.I64, 2000.I64) {
        \\    Cons(head, _) => fold_count(head, 0)
        \\    Nil => -1
        \\}
        ,
        .expected = .{ .inspect_str = "200" },
    },
};

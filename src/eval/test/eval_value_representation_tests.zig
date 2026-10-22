//! Programs whose scalars the LLVM backend carries as SSA values: loop-carried
//! and branch-merged locals, simultaneous parameter updates, early exits,
//! recursion, every scalar width, and scalars flowing into and out of
//! aggregates. Every backend must agree on each result.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "value representation: loop swaps carried scalars",
        .source =
        \\{
        \\    swap_n = |start_a, start_b, n| {
        \\        var $a = start_a
        \\        var $b = start_b
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            tmp = $a
        \\            $a = $b
        \\            $b = tmp
        \\            $i = $i + 1
        \\        }
        \\        ($a, $b)
        \\    }
        \\    (swap_n(1.I64, 2.I64, 5), swap_n(1.I64, 2.I64, 6))
        \\}
        ,
        .expected = .{ .inspect_str = "((2, 1), (1, 2))" },
    },
    .{
        .name = "value representation: loop swaps carried strings",
        .source =
        \\{
        \\    swap_n = |start_a, start_b, n| {
        \\        var $a = start_a
        \\        var $b = start_b
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            tmp = $a
        \\            $a = $b
        \\            $b = tmp.concat("!")
        \\            $i = $i + 1
        \\        }
        \\        ($a, $b)
        \\    }
        \\    swap_n("left side of the swap", "right side of the swap", 3)
        \\}
        ,
        .expected = .{ .inspect_str = "(\"right side of the swap!\", \"left side of the swap!!\")" },
    },
    .{
        .name = "value representation: Fibonacci pair rotates through a loop",
        .source =
        \\{
        \\    fib = |n| {
        \\        var $prev = 0.U64
        \\        var $curr = 1.U64
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            next = $prev + $curr
        \\            $prev = $curr
        \\            $curr = next
        \\            $i = $i + 1
        \\        }
        \\        $prev
        \\    }
        \\    (fib(0), fib(1), fib(10), fib(50))
        \\}
        ,
        .expected = .{ .inspect_str = "(0, 1, 55, 12586269025)" },
    },
    .{
        .name = "value representation: nested loops with an early return",
        .source =
        \\{
        \\    find = |rows, cols, wanted| {
        \\        var $r = 0.U64
        \\        while $r < rows {
        \\            var $c = 0.U64
        \\            while $c < cols {
        \\                if $r * cols + $c == wanted {
        \\                    return ($r, $c)
        \\                }
        \\                $c = $c + 1
        \\            }
        \\            $r = $r + 1
        \\        }
        \\        (rows, cols)
        \\    }
        \\    (find(4, 5, 13), find(4, 5, 0), find(4, 5, 99))
        \\}
        ,
        .expected = .{ .inspect_str = "((2, 3), (0, 0), (4, 5))" },
    },
    .{
        .name = "value representation: branch merges of scalars across if arms",
        .source =
        \\{
        \\    classify = |n| {
        \\        var $score = 0.I32
        \\        var $flag = False
        \\        var $i = 0.I32
        \\        while $i < n {
        \\            r = $i % 3
        \\            if r == 0 {
        \\                $score = $score + 10
        \\            } else if r == 1 {
        \\                $score = $score - 3
        \\                $flag = !$flag
        \\            } else {
        \\                $flag = !$flag
        \\            }
        \\            $i = $i + 1
        \\        }
        \\        ($score, $flag)
        \\    }
        \\    (classify(0), classify(1), classify(2), classify(7))
        \\}
        ,
        .expected = .{ .inspect_str = "((0, False), (10, False), (7, True), (24, False))" },
    },
    .{
        .name = "value representation: recursion passes and returns scalars",
        .source_kind = .module,
        .source =
        \\ackermann : U64, U64 -> U64
        \\ackermann = |m, n|
        \\    if m == 0 n + 1
        \\    else if n == 0 ackermann(m - 1, 1)
        \\    else ackermann(m - 1, ackermann(m, n - 1))
        \\
        \\collatz : U64, U64 -> U64
        \\collatz = |n, steps| if n == 1 steps else if n % 2 == 0 collatz(n // 2, steps + 1) else collatz(3 * n + 1, steps + 1)
        \\
        \\main = (ackermann(2, 3), collatz(27, 0))
        ,
        .expected = .{ .inspect_str = "(9, 111)" },
    },
    .{
        .name = "value representation: every scalar width carried through a loop",
        .source =
        \\{
        \\    run = |n| {
        \\        var $small = 250.U8
        \\        var $signed = -100.I8
        \\        var $wide = 1.U128
        \\        var $huge = -1.I128
        \\        var $dec = 0.5.Dec
        \\        var $f32 = 1.5.F32
        \\        var $f64 = 0.25.F64
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            $small = $small.plus_wrap(3)
        \\            $signed = $signed.minus_wrap(50)
        \\            $wide = $wide * 1_000_000_007
        \\            $huge = $huge * 3
        \\            $dec = $dec * 1.5
        \\            $f32 = $f32 * 2.0
        \\            $f64 = $f64 + 0.5
        \\            $i = $i + 1
        \\        }
        \\        ($small, $signed, $wide, $huge, $dec, $f32, $f64)
        \\    }
        \\    run(3)
        \\}
        ,
        .expected = .{ .inspect_str = "(3, 6, 1000000021000000147000000343, -27, 1.6875, 12, 1.75)" },
    },
    .{
        .name = "value representation: loop scalars build records and tag payloads",
        .source =
        \\{
        \\    build = |n| {
        \\        var $acc = []
        \\        var $i = 0.U8
        \\        var $total = 0.U64
        \\        while $i < n {
        \\            item = if $i % 2 == 0 Even({ index: $i, square: $i.to_u64() * $i.to_u64() }) else Odd($i)
        \\            $acc = $acc.append(item)
        \\            $total = $total + $i.to_u64()
        \\            $i = $i + 1
        \\        }
        \\        ($acc, $total)
        \\    }
        \\    build(4)
        \\}
        ,
        .expected = .{ .inspect_str = "([Even({ index: 0, square: 0 }), Odd(1), Even({ index: 2, square: 4 }), Odd(3)], 6)" },
    },
    .{
        .name = "value representation: loop scalars read back out of records and lists",
        .source =
        \\{
        \\    sum_fields = |points| {
        \\        var $x = 0.I64
        \\        var $y = 0.I64
        \\        for point in points {
        \\            $x = $x + point.x
        \\            $y = $y - point.y
        \\        }
        \\        { x: $x, y: $y }
        \\    }
        \\    sum_fields([{ x: 1, y: 2 }, { x: 30, y: 40 }, { x: 500, y: 600 }])
        \\}
        ,
        .expected = .{ .inspect_str = "{ x: 531, y: -642 }" },
    },
    .{
        .name = "value representation: one helper called from many loop sites with an aggregate accumulator",
        .source =
        \\{
        \\    step = |acc, shape, value|
        \\        match shape {
        \\            0 => { ..acc, a: acc.a.append(value) }
        \\            1 => { ..acc, b: acc.b.append(value * 2) }
        \\            2 => { ..acc, c: acc.c.append(value + 1) }
        \\            _ => { ..acc, a: acc.a.append(value), c: acc.c.append(value) }
        \\        }
        \\    run = |n| {
        \\        var $acc = { a: [], b: [], c: [] }
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            var $j = 0.U64
        \\            while $j < 2 {
        \\                $acc = step($acc, ($i + $j) % 4, $i * 10 + $j)
        \\                $j = $j + 1
        \\            }
        \\            $acc = step($acc, 3, $i)
        \\            $acc = step($acc, $i % 3, 100 + $i)
        \\            $i = $i + 1
        \\        }
        \\        $acc
        \\    }
        \\    run(3)
        \\}
        ,
        .expected = .{ .inspect_str = "{ a: [0, 0, 100, 1, 21, 2], b: [2, 20, 202], c: [0, 12, 1, 21, 21, 2, 103] }" },
    },
    .{
        .name = "value representation: overflow in a loop crashes",
        .source =
        \\{
        \\    grow = |n| {
        \\        var $value = 1.U8
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            $value = $value * 2
        \\            $i = $i + 1
        \\        }
        \\        $value
        \\    }
        \\    grow(9)
        \\}
        ,
        .expected = .{ .crash = {} },
    },
    .{
        .name = "value representation: SIMD vector carried through a loop",
        .source =
        \\{
        \\    run = |n| {
        \\        var $v = U32x4.splat(1)
        \\        var $i = 0.U64
        \\        while $i < n {
        \\            $v = $v.plus_wrap(U32x4.splat(3))
        \\            $i = $i + 1
        \\        }
        \\        ($v.get_lane(0), $v.get_lane(3))
        \\    }
        \\    run(4)
        \\}
        ,
        .expected = .{ .inspect_str = "(13, 13)" },
    },
};

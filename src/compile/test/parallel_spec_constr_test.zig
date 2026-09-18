//! Real-source determinism at SpecConstr's immutable discovery and body shards.
//! Ordinary specialized bodies deliberately remain serial; their captures and
//! strict binding chains must survive the parallel stages without changing IDs.

const harness = @import("lower_to_lir_harness.zig");

test "SpecConstr staged workers discover value patterns and preserve strict captured callables" {
    // The constructor reaches `consume` through ordinary value flow, while
    // nested effectful prefixes and branch-owned captures forbid commuting or
    // duplicating the strict work when specialized bodies are cloned serially.
    try harness.expectSpecConstrParallelismDeterministicLir(
        \\consume : { value : U64, step : U64 -> U64 } -> U64
        \\consume = |item| {
        \\    step = item.step
        \\    step(item.value)
        \\}
        \\
        \\scenario! : U64 => U64
        \\scenario! = |n| {
        \\    base = {
        \\        echo!("outer")
        \\        inner = {
        \\            echo!("inner")
        \\            if n == 0 9.U64 else 19.U64
        \\        }
        \\        echo!("after-inner")
        \\        inner + n
        \\    }
        \\    add_captured = |remaining, value| {
        \\        if remaining == 0 value else add_captured(remaining - 1, value + base)
        \\    }
        \\    echo!("prefix")
        \\    transform = if n == 0 {
        \\        echo!("left")
        \\        |value| value + base
        \\    } else {
        \\        echo!("right")
        \\        |value| value * base
        \\    }
        \\    echo!("shared")
        \\    item = { value: n, step: transform }
        \\    add_captured(2, consume(item))
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    echo!(Str.inspect(scenario!(List.len(args))))
        \\    Ok({})
        \\}
    , .{ .inline_mode = .wrappers }, &.{.discovery});
}

test "SpecConstr staged workers project loop results deterministically" {
    // Adapt the real source regression for #11209: both source and generated
    // loop-state tuples remain live, but the continuation observes only `res`.
    try harness.expectSpecConstrParallelismDeterministicLir(
        \\main! = |args| {
        \\    for _ in args {
        \\        (res, _) = List.fold_until(
        \\            args,
        \\            (0, 0),
        \\            |(p, f), v| {
        \\                n = if v == "" 1 else 0
        \\                if n <= p (if f == 1 Break((0, 1)) else Continue((p, 1))) else Continue((n, f))
        \\            },
        \\        )
        \\        if res == 0 0 else 0
        \\    }
        \\    Ok({})
        \\}
    , .{ .inline_mode = .wrappers }, &.{.loop_projection});
}

test "SpecConstr staged workers fuse iterator-only bodies deterministically" {
    // No general pattern discovery or loop-result projection is enabled in
    // none mode. The runtime bound keeps checker-stamped iterator work live.
    try harness.expectSpecConstrParallelismDeterministicLir(
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    offset = List.len(args)
        \\    total = 0.U64.until(offset).map(|n| n + offset).fold(0, |sum, n| sum + n)
        \\    echo!(Str.inspect(total))
        \\    Ok({})
        \\}
    , .{ .inline_mode = .none }, &.{.iterator_fusion});
}

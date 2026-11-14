//! Public iterators whose item type holds generated-private evidence.
//!
//! An iterator over items that are themselves produced iterators, or records
//! or functions holding one, has a public view `Iter(item)` that contains
//! private evidence through `item` alone. An adapter's step matches the inner
//! iterator's step result against a pattern typed at that public view, so both
//! sides of the relation hold evidence: the scrutinee's `rest` is the
//! representation, and the pattern's `rest` is its public view. The opaque
//! interface relation exchanges its roles at such a pair (design.md
//! "Explicit Iterator Representation Tiers"). These programs pin
//! that each such adapter lowers through Lambda Solved and ARC certification
//! under both specialization strategies.

const harness = @import("lower_to_lir_harness.zig");

fn expectLowersUnderBothStrategies(comptime source: []const u8) harness.LowerToLirHarnessError!void {
    try harness.expectLowersToLirWithOptions(source, .{});
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

const digits =
    \\digits : Iter(U8) -> U64
    \\digits = |items| {
    \\    var $digits = 0
    \\    for item in items {
    \\        $digits = $digits * 10 + item.to_u64()
    \\    }
    \\    $digits
    \\}
    \\
;

test "iterator item evidence: an adapter over an iterator of produced iterators" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    sums = [[1.U8, 2].iter(), [3.U8].iter()].iter().map(|it| digits(it))
        \\    var $total = 0.U64
        \\    for sum in sums {
        \\        $total = $total * 100 + sum
        \\    }
        \\    echo!($total.to_str())
        \\    Ok({})
        \\}
    );
}

test "iterator item evidence: an adapter over an iterator of records holding produced iterators" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    sums = [{ it: [1.U8, 2].iter() }, { it: [3.U8].iter() }].iter().map(|record| digits(record.it))
        \\    var $total = 0.U64
        \\    for sum in sums {
        \\        $total = $total * 100 + sum
        \\    }
        \\    echo!($total.to_str())
        \\    Ok({})
        \\}
    );
}

test "iterator item evidence: adapters over an iterator of functions returning produced iterators" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    made = [|| [1.U8, 2].iter(), || [3.U8].iter()].iter().map(|make| make())
        \\    kept = [|| [4.U8, 5].iter(), || [6.U8].iter()].iter().keep_if(|make| digits(make()) > 10)
        \\    var $total = 0.U64
        \\    for it in made {
        \\        $total = $total * 100 + digits(it)
        \\    }
        \\    for make in kept {
        \\        $total = $total * 100 + digits(make())
        \\    }
        \\    echo!($total.to_str())
        \\    Ok({})
        \\}
    );
}

test "iterator item evidence: a fold over an adapter of functions returning produced iterators" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    total = [|| [1.U8, 2].iter(), || [3.U8].iter()].iter().map(|make| digits(make())).fold(0, |acc, sum| acc * 100 + sum)
        \\    echo!(total.to_str())
        \\    Ok({})
        \\}
    );
}

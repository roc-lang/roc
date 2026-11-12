//! Joins of a public iterator that cannot merge with a generated-private one.
//!
//! Generated-private selection never merges a class that reaches a finished
//! Monotype, and a `for` loop's iterable request reaches one. A branch or early
//! return that yields a public `Iter` there, next to one that yields a freshly
//! produced iterator, therefore joins a public view with a private
//! representation (design.md "Explicit Iterator Representation Tiers"). These
//! programs pin that the join keeps the private selection, that each lowers
//! through Lambda Solved and ARC certification, and that an early return names
//! the output witness its lambda finally selects.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

/// Value-producing `match` and `if` expressions typed at a minted iterator.
fn countMintedIteratorJoins(program: anytype) usize {
    var count: usize = 0;
    for (program.exprs) |expr| {
        if (expr.data != .match_ and expr.data != .if_) continue;
        switch (program.types.get(expr.ty)) {
            .named => |named| if (named.def.iterator_representation == .minted) {
                count += 1;
            },
            .primitive, .func, .record, .tuple, .tag_union, .list, .box, .erased, .zst, .hole => {},
        }
    }
    return count;
}

fn expectMintedIteratorJoin(
    prepared: *const lir.CheckedPipeline.PreparedMonotype,
) harness.LowerToLirHarnessError!void {
    try std.testing.expect(countMintedIteratorJoins(prepared.program.view()) >= 1);
}

test "control-flow join: a public pattern binder before a produced iterator keeps the private selection" {
    try harness.expectLowersToLirWithOptions(
        \\digits = |seed| {
        \\    its : List(Iter(U8))
        \\    its = []
        \\    var $digits = seed
        \\    for item in (match its { [first, ..] => first, _ => [9.U8].iter() }) {
        \\        $digits = $digits * 10 + item.to_u64()
        \\    }
        \\    $digits
        \\}
        \\main! = |args| {
        \\    echo!(digits(args.len()).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectMintedIteratorJoin });
}

test "control-flow join: a produced iterator before a public pattern binder keeps the private selection" {
    try harness.expectLowersToLirWithOptions(
        \\digits = |seed| {
        \\    its : List(Iter(U8))
        \\    its = []
        \\    var $digits = seed
        \\    for item in (match its { [] => [9.U8].iter().map(|item| item - 1), [first, ..] => first }) {
        \\        $digits = $digits * 10 + item.to_u64()
        \\    }
        \\    $digits
        \\}
        \\main! = |args| {
        \\    echo!(digits(args.len()).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectMintedIteratorJoin });
}

test "control-flow join: a public Try payload defaults to a produced iterator" {
    try harness.expectLowersToLirWithOptions(
        \\digits = |seed| {
        \\    held : Try(Iter(U8), Str)
        \\    held = Err("none")
        \\    var $digits = seed
        \\    for item in (held ?? [7.U8, 8].iter()) {
        \\        $digits = $digits * 10 + item.to_u64()
        \\    }
        \\    $digits
        \\}
        \\main! = |args| {
        \\    echo!(digits(args.len()).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectMintedIteratorJoin });
}

test "control-flow join: a nested join carries a public branch into the outer private selection" {
    try harness.expectLowersToLirWithOptions(
        \\digits = |seed| {
        \\    its : List(Iter(U8))
        \\    its = []
        \\    var $digits = seed
        \\    for item in (if seed > 100 { [1.U8].iter() } else { match its { [first, ..] => first, _ => [9.U8].iter().map(|item| item - 3) } }) {
        \\        $digits = $digits * 10 + item.to_u64()
        \\    }
        \\    $digits
        \\}
        \\main! = |args| {
        \\    echo!(digits(args.len()).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectMintedIteratorJoin });
}

test "control-flow join: an early return of a public iterator names the private output its lambda selects" {
    // Lambda Solved cross-checks every return's target against its function's
    // result type in debug builds, so lowering this program is the assertion.
    try harness.expectLowersToLir(
        \\digits = |seed| {
        \\    its : List(Iter(U8))
        \\    its = []
        \\    pick = |held| {
        \\        for it in held {
        \\            return it
        \\        }
        \\        [9.U8].iter()
        \\    }
        \\    var $digits = seed
        \\    for item in pick(its) {
        \\        $digits = $digits * 10 + item.to_u64()
        \\    }
        \\    $digits
        \\}
        \\main! = |args| {
        \\    echo!(digits(args.len()).to_str())
        \\    Ok({})
        \\}
    );
}

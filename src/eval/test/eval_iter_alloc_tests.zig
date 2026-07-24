//! Zero-allocation gate for Iter.
//!
//! Every iterator case asserts `max_allocations = 0`: a statically-known
//! iterator chain must perform ZERO heap allocations, regardless of how it is
//! consumed. Range sources are used deliberately — a range's state is two
//! integers, so there is no list-build allocation to confound the measurement;
//! any allocation is the iterator machinery itself.
//!
//! The observed output is a static string literal chosen by comparing the
//! iterator's computed value to its expected value ("ok" iff correct). String
//! literals are compile-time constants, so the observation itself allocates
//! nothing — the iterator's fold is the only possible allocator. This is
//! confirmed by the two canary cases below, which must be GREEN (0 allocations).
//!
//! The iterator cases are RED on the recursive-nominal representation (the
//! iterator boxes a successor state per step). They turn GREEN only when the
//! internal representation carries a statically-known chain by value.
//! `allocations_at_most` counts cumulative roc_alloc/roc_realloc, so a per-step
//! allocate/free loop is caught even though it leaves the live heap near zero.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Eval test cases that require statically-known Iter chains to run without
/// heap allocation.
pub const tests = [_]TestCase{
    // --- Canaries: prove the observation pattern (literal + arithmetic +
    // branch) allocates nothing, so a nonzero count is the iterator's fault. ---
    .{
        .name = "iter alloc canary: bare string literal is zero-alloc",
        .source = "\"ok\"",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc canary: arithmetic and branch to a literal is zero-alloc",
        .source = "if 2.U64 + 1 == 3 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },

    // --- Expression sources: base + single adapters over a range ---
    .{
        .name = "iter alloc: range fold is zero-alloc",
        .source = "if Iter.fold(0.U64..<5, 0.U64, |a, b| a + b) == 10 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range map fold is zero-alloc",
        .source = "if Iter.fold(Iter.map(0.U64..<5, |n| n + 1), 0.U64, |a, b| a + b) == 15 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range keep_if fold is zero-alloc",
        .source = "if Iter.fold(Iter.keep_if(0.U64..<6, |n| n > 2), 0.U64, |a, b| a + b) == 12 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range drop_if fold is zero-alloc",
        .source = "if Iter.fold(Iter.drop_if(0.U64..<6, |n| n <= 2), 0.U64, |a, b| a + b) == 12 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range take_first fold is zero-alloc",
        .source = "if Iter.fold(Iter.take_first(0.U64..<6, 3), 0.U64, |a, b| a + b) == 3 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range drop_first fold is zero-alloc",
        .source = "if Iter.fold(Iter.drop_first(0.U64..<6, 3), 0.U64, |a, b| a + b) == 12 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range concat fold is zero-alloc",
        .source = "if Iter.fold(Iter.concat(0.U64..<3, 3.U64..<6), 0.U64, |a, b| a + b) == 15 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range append fold is zero-alloc",
        .source = "if Iter.fold(Iter.append(0.U64..<5, 5), 0.U64, |a, b| a + b) == 15 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        // concat over a `single` combines two differently-typed minted iterators
        // by value; `single`'s successor and concat's exhausted first must each
        // keep one monomorphic type, else the chain is un-representable flat.
        .name = "iter alloc: range concat single fold is zero-alloc",
        .source = "if Iter.fold(Iter.concat(0.U64..<3, Iter.single(9)), 0.U64, |a, b| a + b) == 12 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: range single fold is zero-alloc",
        .source = "if Iter.fold(Iter.single(7.U64), 0.U64, |a, b| a + b) == 7 { \"ok\" } else { \"bad\" }",
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },

    // --- `for`-loop driver over a minted chain. A `for` sinks the consuming
    // loop into the chain and rebases the step's inline captures; the append
    // step re-feeds its own inner-iterator capture slot with the destructured
    // successor `rest`, so a driver that drops that operand freezes the inner
    // iterator at its head and the loop never terminates. `fold` does not
    // exercise this path, so these `for` cases are the gate for it. ---
    .{
        .name = "iter alloc: range append for-loop is zero-alloc",
        .source =
        \\{
        \\    var sum = 0.U64
        \\    for x in Iter.append(0.U64..<5, 5) {
        \\        sum = sum + x
        \\    }
        \\    if sum == 15 { "ok" } else { "bad" }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: list append append for-loop only allocates base list",
        .source =
        \\{
        \\    base_points = [
        \\        { x: 11.I64, y: 2.I64 }, { x: 13, y: 3 },
        \\        { x: 3, y: 5 }, { x: 11, y: 6 },
        \\        { x: 9, y: 8 }, { x: 5, y: 9 },
        \\        { x: 7, y: 10 }, { x: 5, y: 12 },
        \\    ].iter()
        \\    collision_points = base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
        \\    var sum = 0.I64
        \\    for { x, y } in collision_points {
        \\        sum = sum + x + y
        \\    }
        \\    if sum == 130 { "ok" } else { "bad" }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 1, .optimized = true } },
    },
    .{
        .name = "iter alloc: range map for-loop is zero-alloc",
        .source =
        \\{
        \\    var sum = 0.U64
        \\    for x in Iter.map(0.U64..<5, |n| n + 1) {
        \\        sum = sum + x
        \\    }
        \\    if sum == 15 { "ok" } else { "bad" }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: captured range map folds are zero-alloc",
        .source =
        \\{
        \\    offset = 1.U64
        \\    record = { big: 10.U64, small: 3.U64 }
        \\    small = Iter.fold(Iter.map(0.U64..<5, |n| n + offset), 0.U64, |a, b| a + b)
        \\    large = Iter.fold(Iter.map(0.U64..<5, |n| n + record.big + record.small), 0.U64, |a, b| a + b)
        \\    if small == 15 and large == 75 { "ok" } else { "bad" }
        \\}
        ,
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },
    .{
        .name = "iter alloc: finite deep static map chain is zero-alloc",
        .source =
        \\if Iter.fold(
        \\    Iter.map(
        \\        Iter.map(
        \\            Iter.map(
        \\                Iter.map(
        \\                    Iter.map(
        \\                        Iter.map(
        \\                            Iter.map(
        \\                                Iter.map(
        \\                                    Iter.map(
        \\                                        Iter.map(
        \\                                            0.U64..<5,
        \\                                            |n| n + 1,
        \\                                        ),
        \\                                        |n| n + 1,
        \\                                    ),
        \\                                    |n| n + 1,
        \\                                ),
        \\                                |n| n + 1,
        \\                            ),
        \\                            |n| n + 1,
        \\                        ),
        \\                        |n| n + 1,
        \\                    ),
        \\                    |n| n + 1,
        \\                ),
        \\                |n| n + 1,
        \\            ),
        \\            |n| n + 1,
        \\        ),
        \\        |n| n + 1,
        \\    ),
        \\    0.U64,
        \\    |a, b| a + b,
        \\) == 60 { "ok" } else { "bad" }
        ,
        .expected = .{ .allocations_at_most = .{ .output = "ok", .max_allocations = 0 } },
    },

    // Escaping cases (iterator returned / passed / branch-merged) require
    // module-level function definitions, which the runtime allocation path does
    // not support. They are gated statically by `box_box_count == 0` over
    // reachable procs in lir_inline_test.zig ("iter alloc static: …").

    // Behavioral aliasing guard — the allocation gate is INVERTED for this bug.
    // A list held by a live iterator must not be seen as unique: if in-place
    // mutation of the same list fired, it would corrupt the iterator's view AND
    // lower the allocation count (the gate would go greener on a miscompile).
    // Here `it` holds `xs`, so `List.map(xs, ...)` must not mutate `xs` in
    // place; the fold over `it` must observe the original elements. a = sum(xs)
    // = 15, b = sum(2*xs) = 30, result = a*1000 + b = 15030. A wrong `a` (e.g.
    // 30030) means the shared buffer was mutated under the live iterator. This
    // is a correctness assertion, not an allocation one.
    .{
        .name = "iter alloc guard: list held by a live iterator is not mutated in place",
        .source_kind = .module,
        .source =
        \\main : I64
        \\main = {
        \\    xs = [1.I64, 2, 3, 4, 5]
        \\    it = List.iter(xs)
        \\    doubled = List.map(xs, |n| n * 2)
        \\    a = Iter.fold(it, 0.I64, |s, n| s + n)
        \\    b = List.fold(doubled, 0.I64, |s, n| s + n)
        \\    a * 1000 + b
        \\}
        ,
        .expected = .{ .inspect_str = "15030" },
    },
};

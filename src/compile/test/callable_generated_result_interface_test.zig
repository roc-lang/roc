//! Functions whose results hold generated-private iterators, viewed through a
//! type variable.
//!
//! A function's type carries its result's representation, so a lambda that
//! returns a produced iterator has a function type holding generated-private
//! evidence. A generic procedure specialized at that function (`Box.box`, the
//! list a `for` loop walks, a record or tag passed through an identity) binds
//! a parameter whose checked type is a bare variable to it, and the opaque
//! interface relation gives that variable the function's public interface
//! (design.md "Explicit Iterator Representation Tiers"). These programs pin
//! that each such specialization lowers through Lambda Solved and ARC
//! certification under both specialization strategies; the fx app runs the same
//! shapes, through the host as well.

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

test "callable result interface: a boxed function returning a produced iterator is unboxed and called" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    make = Box.unbox(Box.box(|| [1.U8, 2].iter()))
        \\    echo!(digits(make()).to_str())
        \\    Ok({})
        \\}
    );
}

test "callable result interface: a boxed function returning an adapter chain is the loop iterable" {
    try expectLowersUnderBothStrategies(
        \\main! = |_args| {
        \\    make = Box.unbox(Box.box(|| [1.U8, 2].iter().map(|item| item + 1).concat([9.U8].iter())))
        \\    var $digits = 0.U64
        \\    for item in make() {
        \\        $digits = $digits * 10 + item.to_u64()
        \\    }
        \\    echo!($digits.to_str())
        \\    Ok({})
        \\}
    );
}

test "callable result interface: a list of functions returning produced iterators is walked and called" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    makers = [|| [1.U8, 2].iter(), || [3.U8].iter().map(|item| item + 1)]
        \\    var $total = 0.U64
        \\    for make in makers {
        \\        $total = $total * 100 + digits(make())
        \\    }
        \\    echo!($total.to_str())
        \\    Ok({})
        \\}
    );
}

test "callable result interface: a boxed function returns a record, a tag, and a function holding iterators" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |_args| {
        \\    record = Box.unbox(Box.box(|| { it: [1.U8, 2].iter(), count: 3.U64 }))()
        \\    tagged = Box.unbox(Box.box(|ok| if ok { Ok([2.U8, 3].iter()) } else { Err(7.U64) }))
        \\    nested = Box.unbox(Box.box(|| || [4.U8, 5].iter()))
        \\    from_tag = match tagged(Bool.True) {
        \\        Ok(it) => digits(it)
        \\        Err(code) => code
        \\    }
        \\    echo!((digits(record.it) + record.count + from_tag + digits(nested()())).to_str())
        \\    Ok({})
        \\}
    );
}

test "callable result interface: functions inside a record, a tuple, and a tag cross a generic procedure" {
    try expectLowersUnderBothStrategies(digits ++
        \\id : a -> a
        \\id = |x| x
        \\
        \\main! = |args| {
        \\    record = id({ make: || [1.U8, 2].iter(), count: 3.U64 })
        \\    pair = id((|| [2.U8, 3].iter(), 4.U64))
        \\    tagged = id(if args.len() > 0 { Ok(|| [5.U8, 6].iter()) } else { Err(6.U64) })
        \\    from_tag = match tagged {
        \\        Ok(make) => digits(make())
        \\        Err(code) => code
        \\    }
        \\    echo!((digits((record.make)()) + record.count + digits((pair.0)()) + pair.1 + from_tag).to_str())
        \\    Ok({})
        \\}
    );
}

test "callable result interface: an unboxed function joins a local one" {
    try expectLowersUnderBothStrategies(digits ++
        \\main! = |args| {
        \\    held = Box.unbox(Box.box(|| [1.U8, 2].iter()))
        \\    make = if args.len() > 1 { held } else { || [9.U8].iter().map(|item| item - 1) }
        \\    echo!(digits(make()).to_str())
        \\    Ok({})
        \\}
    );
}

test "callable result interface: functions returning iterators cross the host in every form" {
    try harness.expectAppPathLowersToLir("test/fx/callable_iterator_result_host_round_trip.roc");
    try harness.expectAppPathLowersToLirWithOptions(
        "test/fx/callable_iterator_result_host_round_trip.roc",
        .{ .specialization_strategy = .boxy },
    );
}

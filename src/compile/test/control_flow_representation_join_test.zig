//! Joins of iterators that reach one control-flow merge from different
//! producers: a branch or early return that yields an iterator held in a
//! value next to one freshly produced. Each program lowers through Lambda
//! Solved and ARC certification, and an early return names the result type
//! its lambda finally selects. The fx app adds values that exist at runtime,
//! including a nominal read back at one representation and constructed at
//! another.

const harness = @import("lower_to_lir_harness.zig");

test "control-flow join: a public pattern binder before a produced iterator lowers" {
    try harness.expectLowersToLir(
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
    );
}

test "control-flow join: a produced iterator before a public pattern binder lowers" {
    try harness.expectLowersToLir(
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
    );
}

test "control-flow join: a public Try payload defaults to a produced iterator" {
    try harness.expectLowersToLir(
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
    );
}

test "control-flow join: a nested join carries a public branch into the outer join" {
    try harness.expectLowersToLir(
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
    );
}

test "control-flow join: an early return of a public iterator names the output its lambda selects" {
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

test "control-flow join: host-held iterators, bare and inside a nominal, join produced ones in every form" {
    // The fx platform's hosted `Box(a)` round trip yields public iterators that
    // exist at runtime. Lowering the app runs every Monotype and Lambda Solved
    // debug assertion over each join form the fx suite then executes.
    try harness.expectAppPathLowersToLir("test/fx/control_flow_iterator_join_host_round_trip.roc");
}

//! Regression tests for https://github.com/roc-lang/roc/issues/12060.
//!
//! `Outer.is_ok`'s scheme has a variable its own signature does not reach: the
//! error row of `checked_send`'s result, which only the `send` requirement's
//! callable mentions. Every call to such a target needs its checked
//! substitution, including a second, independent call of the same method on
//! the same receiver and an iterator `for` plan's call.
const harness = @import("lower_to_lir_harness.zig");

const forwarding_methods =
    \\Outer :: { inner : Inner }.{
    \\    send = |outer| outer.inner.send()
    \\
    \\    is_ok = |outer| checked_send(outer).is_ok()
    \\
    \\    iter = |outer|
    \\        if checked_send(outer).is_ok() {
    \\            [1.U64, 2, 3].iter()
    \\        } else {
    \\            [10.U64].iter()
    \\        }
    \\}
    \\
    \\Inner :: [Inner].{
    \\    send : Inner -> Try({}, [Failed(Str)])
    \\    send = |_| Err(Failed("no"))
    \\}
    \\
    \\checked_send = |x| {
    \\    x.send()?
    \\    Ok({})
    \\}
    \\
;

const twice_app = forwarding_methods ++
    \\twice = |outer| (outer.is_ok(), outer.is_ok())
    \\
    \\main! = |_args| {
    \\    _ = twice(Outer.{ inner: Inner.Inner })
    \\    Ok({})
    \\}
;

const loops_app = forwarding_methods ++
    \\total = |xs| {
    \\    var $sum = 0
    \\    for x in xs {
    \\        $sum = $sum + x
    \\    }
    \\    $sum
    \\}
    \\
    \\both = |xs| {
    \\    var $sum = 0
    \\    for x in xs {
    \\        $sum = $sum + x
    \\    }
    \\    for y in xs {
    \\        $sum = $sum + y
    \\    }
    \\    $sum
    \\}
    \\
    \\main! = |_args| {
    \\    _ = total(Outer.{ inner: Inner.Inner })
    \\    _ = both(Outer.{ inner: Inner.Inner })
    \\    Ok({})
    \\}
;

test "issue 12060: a forwarding method dispatched twice on one receiver specializes" {
    try harness.expectLowersToLir(twice_app);
}

test "issue 12060: a forwarding method dispatched twice on one receiver specializes under boxy" {
    try harness.expectLowersToLirWithOptions(twice_app, .{ .specialization_strategy = .boxy });
}

test "issue 12060: iterator for plans keep their target substitution" {
    try harness.expectLowersToLir(loops_app);
}

test "issue 12060: iterator for plans keep their target substitution under boxy" {
    try harness.expectLowersToLirWithOptions(loops_app, .{ .specialization_strategy = .boxy });
}

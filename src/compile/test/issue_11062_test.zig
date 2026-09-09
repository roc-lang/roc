//! Regression test for issue #11062.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11062: unannotated generic higher-order helper called at three record types lowers to Monotype" {
    // Repro for https://github.com/roc-lang/roc/issues/11062.
    //
    // `section` is generic and unannotated, and every call site passes a
    // closure that interpolates fields of a different record type. The
    // interpolations dispatch through `section`'s parameter, so each call site
    // needs its own dispatch plan. Checking this program reports no errors and
    // Monotype lowering resolves every plan.
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| {
        \\    xs : List({ id : I32, name : Str })
        \\    xs = [{ id: 1, name: "n" }]
        \\    ys : List({ id : I32, size : I32 })
        \\    ys = [{ id: 2, size: 9 }]
        \\    zs : List({ id : I32, reason : Str })
        \\    zs = [{ id: 3, reason: "r" }]
        \\    echo!(render(xs, ys, zs))
        \\    Ok({})
        \\}
        \\
        \\render = |xs, ys, zs| {
        \\    a = section([], xs, "A", |r| "${r.id.to_str()} ${r.name}")
        \\    b = section(a, ys, "B", |r| "${r.id.to_str()} ${r.size.to_str()}")
        \\    c = section(b, zs, "C", |r| "${r.id.to_str()} ${r.reason}")
        \\    Str.join_with(c, "\n")
        \\}
        \\
        \\section = |acc, items, heading, f|
        \\    if items.is_empty() {
        \\        acc
        \\    } else {
        \\        acc.append(Str.join_with(items.map(f).prepend(heading), "\n"))
        \\    }
    , .{ .monotype_only = true });
}

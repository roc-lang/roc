app [main!] { pf: platform "./static-lib-platform/main.roc" }

# End-to-end cart gate for static-data hoisting of constant list literals
# consumed via `.iter()`. A constant list literal is materialized as static
# data (no runtime heap allocation), the minted adapters hold their predecessor
# by value, and the `for`-drive is scalarized — so the whole chain, base list
# included, allocates ZERO on the `--opt=size` cart path. The runner asserts
# `--max-allocs 0`, which the `--assert-alloc-balanced` iter_for gate cannot:
# a heap-built base list would allocate-and-free (balanced) yet still be caught
# here.
# Repro for https://github.com/roc-lang/roc/issues/10123: two constant
# iterator appends must preserve the bounded representation and allocate zero.
main! : U64 => Str
main! = |_seed| {
    collision_points =
        [
            { x: 11, y: 2 }, { x: 13, y: 3 }, { x: 3, y: 5 }, { x: 11, y: 6 },
            { x: 9, y: 8 }, { x: 5, y: 9 }, { x: 7, y: 10 }, { x: 5, y: 12 },
        ]
        .iter()
        .append({ x: 2, y: 1 })
        .append({ x: 7, y: 1 })
    var sum = 0.I64
    for { x, y } in collision_points {
        sum = sum + x + y
    }
    if sum == 130 { "ok" } else { "bad" }
}

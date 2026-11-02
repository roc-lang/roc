app [main!] { pf: platform "./reserved-low-memory-platform/main.roc" }

## Recursion within the stack, recursive data dropped by its refcount, and a
## closure called through a function value, all checking the stack bounds a
## platform that reserves low memory needs.
Chain := [Link(U64, Chain), End]

build : U64, Chain -> Chain
build = |n, acc| if n == 0 { acc } else { build(n - 1, Link(n, acc)) }

total : Chain -> U64
total = |chain|
    match chain {
        Link(n, rest) => n + total(rest)
        End => 0
    }

apply_twice : (U64 -> U64), U64 -> U64
apply_twice = |f, x| f(f(x))

main! = |seed| {
    chain = build(seed + 100, End)
    offset = seed + 1
    doubled = apply_twice(|n| n * 2 + offset, total(chain))
    "${doubled.to_str()}"
}

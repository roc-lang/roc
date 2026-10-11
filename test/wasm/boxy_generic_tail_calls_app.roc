app [main!] { pf: platform "./platform/main.roc" }

# Built without specialization, every function here is generic at run time:
# comparisons go through dictionary calls, the mutually recursive pair returns
# a type descriptor with its value, and the cycle through `apply` converts its
# result on the way back. Each cycle is made only of tail calls, so it runs in
# constant stack space.

apply = |f, x| f(x)

ping = |n, x| if n == 0 x else pong(n - 1, x)

pong = |n, x| if n == 0 x else ping(n - 1, x)

label_through_apply : U64, Str -> Str
label_through_apply = |n, label| if n == 0 Str.concat(label, "!") else apply(|m| label_through_apply(m - 1, label), n)

main! = || {
    first = ping(40_001, "a result string long enough to live on the heap")
    label_through_apply(20_000, first)
}

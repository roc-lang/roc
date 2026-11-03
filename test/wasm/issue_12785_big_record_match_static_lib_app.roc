app [main!] { pf: platform "./static-lib-platform/main.roc" }

# A 64 KiB record moves through ten match arms (#12785). Each move of the
# record and each zero fill of an arm's result is one bounded copy, so the
# function stays far below an engine's size limit however wide the record
# is, and a build run with ROC_WASM_SPLIT_BYTES set emits every procedure
# here as a family of functions. The input comes from the host so that no
# call is evaluated at compile time, and the deep field reads check the
# copies.

two = |x| { a: x, b: x }

step = |m, n| match n {
    0 => { model: m, tag: 0 }
    1 => { model: m, tag: 1 }
    2 => { model: m, tag: 2 }
    3 => { model: m, tag: 3 }
    4 => { model: m, tag: 4 }
    5 => { model: m, tag: 5 }
    6 => { model: m, tag: 6 }
    7 => { model: m, tag: 7 }
    8 => { model: m, tag: 8 }
    _ => { model: m, tag: 9 }
}

main! = |seed| {
    n = seed + 21
    big = two(two(two(two(two(two(two(two(two(two(two(two(two(n)))))))))))))
    first = step(big, n % 10)
    second = step(first.model, (n + 3) % 10)
    third = step(second.model, n + 100)
    deep = third.model.a.b.a.b.a.b.a.b.a.b.a.b.a
    "${first.tag.to_str()} ${second.tag.to_str()} ${third.tag.to_str()} ${deep.to_str()}"
}

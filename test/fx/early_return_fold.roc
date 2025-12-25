app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test with List.fold

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

main! = || {
    # Use fold to apply compute to each element
    result = List.fold(
        [Ok(1), Err({})],
        [],
        |acc, x| List.append(acc, compute(x))
    )

    count = List.len(result)
    Stdout.line!("Count: ${count.to_str()}")
}

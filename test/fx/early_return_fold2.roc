app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test with List.fold - just one Err element

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

main! = || {
    # Use fold with just one Err element
    result = List.fold(
        [Err({})],
        [],
        |acc, x| List.append(acc, compute(x))
    )

    count = List.len(result)
    Stdout.line!("Count: ${count.to_str()}")
}

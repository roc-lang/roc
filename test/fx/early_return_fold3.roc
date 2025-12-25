app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test with List.fold - no append, just compute

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

main! = || {
    # Use fold - just return the result of compute, ignore acc
    result = List.fold(
        [Err({})],
        Ok(0),
        |_acc, x| compute(x)
    )

    match result {
        Ok(v) => Stdout.line!("Ok: ${v.to_str()}")
        Err({}) => Stdout.line!("Err!")
    }
}

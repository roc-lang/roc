app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test with a simple higher-order function

apply : a, (a -> b) -> b
apply = |x, f| f(x)

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

main! = || {
    # Use apply to call compute
    result = apply(Err({}), compute)

    match result {
        Ok(x) => Stdout.line!("Ok: ${x.to_str()}")
        Err({}) => Stdout.line!("Err!")
    }
}

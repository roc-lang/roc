app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Simpler test - just call the function directly, no List.map

main! = || {
    # Call a function that uses ? on an Err value
    result = compute(Err({}))

    match result {
        Ok(x) => Stdout.line!("Ok: ${x.to_str()}")
        Err({}) => Stdout.line!("Err!")
    }
}

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

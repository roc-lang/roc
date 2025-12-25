app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test with a simple two-arg function where the second arg early-returns

my_func : a, b -> b
my_func = |_a, b| b

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

main! = || {
    # Call my_func with two args, where the second uses ?
    result = my_func(42, compute(Err({})))

    match result {
        Ok(x) => Stdout.line!("Ok: ${x.to_str()}")
        Err({}) => Stdout.line!("Err!")
    }
}

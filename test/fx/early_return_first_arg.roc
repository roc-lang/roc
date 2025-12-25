app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test with a simple two-arg function where the FIRST arg early-returns

my_func : a, b -> a
my_func = |a, _b| a

compute : Try(U64, {}) -> Try(U64, {})
compute = |x| Ok(x?)

main! = || {
    # Call my_func with two args, where the FIRST uses ?
    result = my_func(compute(Err({})), 42)

    match result {
        Ok(x) => Stdout.line!("Ok: ${x.to_str()}")
        Err({}) => Stdout.line!("Err!")
    }
}

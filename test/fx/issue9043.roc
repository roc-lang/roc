app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Simulates a function that returns (value, new_index)
next! : U64 => (U64, U64)
next! = |index| {
    (index, index + 1)
}

# Bug: $n is used in next!($n) but is only being declared
# in the pattern (var $n). This should be an "identifier not in scope" error,
# but instead causes a crash: "e_lookup_local: definition not found in current scope"
parse! : {} => U64
parse! = |{}| {
    # This is the problematic line: $n is used before it's declared
    (_value, var $n) = next!($n)
    $n
}

main! = || {
    result = parse!({})
    Stdout.line!("Result: ${Str.inspect(result)}")
}

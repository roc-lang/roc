app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: OR-pattern (pat1 | pat2 => body) with tag union.
# Multiple patterns share the same body in the lowered LIR.

describe : [Red, Green, Blue, Yellow] -> Str
describe = |color| match color {
    Red | Blue => "cool"
    Green | Yellow => "warm"
}

main! = || {
    Stdout.line!(describe(Red))
    Stdout.line!(describe(Green))
    Stdout.line!(describe(Blue))
    Stdout.line!(describe(Yellow))
}

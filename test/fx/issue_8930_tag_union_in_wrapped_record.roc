app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: wrapped tag union inside a record inside a wrapped type
# This reproduces issue #8930 where Inspect.to_str crashed with
# "increfDataPtrC: ptr=0x2 is not 8-byte aligned"

ValueCombinationMethod := [
    Divide,
    Modulo,
    Add,
    Subtract,
]

Value := [
    CombinedValue({
        combination_method: ValueCombinationMethod,
    }),
]

parse_value! : {} => Value
parse_value! = |_| {
    value = CombinedValue({combination_method: Modulo})
    value
}

main! = || {
    parsed = parse_value!({})
    Stdout.line!("parsed: ${Str.inspect(parsed)}")
}

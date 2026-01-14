app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Minimal reproduction of issue 9011

ValueCombinationMethod := [A, B, C, D, E, F, G, H, I, J, K, L, M, N]

Value := [
    UInt(U64),
    CombinedValue({
        combination_method: ValueCombinationMethod,
    }),
]

other! : {} => Try((Value, U64), Str)
other! = |{}| {
    Ok((UInt(3), 1))
}

helper! : {} => Try((Value, U64), Str)
helper! = |{}| {
    combination_method1 : ValueCombinationMethod
    combination_method1 = C

    (_value, index) = other!({})?

    value : Value
    value = CombinedValue({combination_method: combination_method1})
    Ok((value, index))
}

main! = || {
    match helper!({}) {
        Ok((value, index)) => Stdout.line!("value: ${Str.inspect(value)}, index: ${Str.inspect(index)}")
        Err(_) => {}
    }
}

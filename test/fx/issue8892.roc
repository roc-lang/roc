app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    parsed = parse_value()
    Stdout.line!("parsed: ${Str.inspect(parsed)}")
}

ValueCombinationMethod := [
    Modulo,
]

parse_value : () -> ValueCombinationMethod
parse_value = || {
    combination_method1 : ValueCombinationMethod
    combination_method1 = match ModuloToken {
        ModuloToken => Modulo
    }
    combination_method1
}

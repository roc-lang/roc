app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    value : Type
    value = Array((0, Name("String")))
    Stdout.line!("type: ${Str.inspect(value)}")
}

Type := [
    Name(Str),
    Array((
        U64, # Length, 0 means that the length is dynamic
        Type, # The type of the elements in the array
    )),
]

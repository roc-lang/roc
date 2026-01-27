app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Minimal reproduction of issue #9042

Value := [
    Leaf,
    Node(Value),
]

build! : U64 => Try(Value, Str)
build! = |n| {
    if n == 0 {
        Ok(Leaf)
    } else {
        child = build!(n - 1)?
        Ok(Node(child))
    }
}

main! : () => {}
main! = || {
    result = build!(2)
    Stdout.line!("result: ${Str.inspect(result)}")
}

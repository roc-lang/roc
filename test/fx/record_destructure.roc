app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test: record destructuring where wildcard fields must use
# actual field layouts (not .zst). Also covers alignment-reordering of
# destructured fields to match layout order.

main! = || {
    rec : { name : Str, age : U8, score : I64 }
    rec = { name: "Bob", age: 25, score: 99 }

    # Destructure all fields
    { name, age, score } = rec
    Stdout.line!("${name} ${age.to_str()} ${score.to_str()}")
}

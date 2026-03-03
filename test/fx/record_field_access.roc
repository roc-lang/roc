app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test: record field access where monotype order (alphabetical)
# differs from layout order (alignment-sorted). For { name: Str, age: U8, score: I64 }:
# - Monotype order: [age=0, name=1, score=2]
# - Layout order: [name=0, score=1, age=2] (24B, 8B, 1B)
# Catches bug where field_idx was computed from monotype order.

main! = || {
    rec : { name : Str, age : U8, score : I64 }
    rec = { name: "Alice", age: 30, score: 100 }
    Stdout.line!(rec.name)
    Stdout.line!(rec.age.to_str())
    Stdout.line!(rec.score.to_str())
}

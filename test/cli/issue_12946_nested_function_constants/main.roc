app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout
import Registry

# Top-level constants that hold functions only inside other values: a list of
# records and a record, here and in an imported module. The checked-module
# cache must keep their compile-time values ready for both specialization
# strategies.

make_adder : U64 -> (U64 -> U64)
make_adder = |k| |n| n + k

parsers : List({ name : Str, parse : Str -> U64 })
parsers = [
    { name: "length", parse: |s| Str.count_utf8_bytes(s) },
    { name: "plus five", parse: |s| make_adder(5)(Str.count_utf8_bytes(s)) },
]

ops : { label : Str, double : U64 -> U64, add_ten : U64 -> U64 }
ops = { label: "ops", double: |n| n * 2, add_ten: make_adder(10) }

main! = || {
    for parser in parsers {
        Stdout.line!("${parser.name}: ${(parser.parse)("abcd").to_str()}")
    }
    Stdout.line!("${ops.label}: ${(ops.double)((ops.add_ten)(11)).to_str()}")
    for entry in Registry.entries {
        Stdout.line!("${(entry.render)("x")} ${(entry.weight)(1).to_str()}")
    }
    Stdout.line!("${(Registry.table.lookup)("y")} ${(Registry.table.nested.inner)(5).to_str()}")
}

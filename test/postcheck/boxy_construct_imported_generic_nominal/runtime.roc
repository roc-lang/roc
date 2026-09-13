app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout
import Container
import Nested
import Optional

make : List(a) -> Container(a)
make = |items| { items: items }

main! = || {
    bytes : Container(U8)
    bytes = { items: Str.to_utf8("xyz") }
    strings : Container(Str)
    strings = { items: ["a string longer than the small string representation"] }
    empty : Container(Str)
    empty = { items: [] }
    nested : Nested(U8)
    nested = { first: bytes, second: strings, fixed: ["fixed"] }
    absent : Optional(U8)
    absent = {}
    present : Optional(Str)
    present = { item: "present" }
    made = make(nested.second.items)
    Stdout.line!(Str.inspect(nested.first.items))
    Stdout.line!(Str.inspect(made.items))
    Stdout.line!(Str.inspect(empty.items))
    Stdout.line!(Str.inspect(absent))
    Stdout.line!(Str.inspect(present))
}

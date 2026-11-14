# repro for https://github.com/roc-lang/roc/issues/12912
# A padded nominal whose row parameter supplies more fields lays out its
# declared entries first, then the extension's fields in label order. Every
# expect builds the value one way and reads it another, so a layout mismatch
# between the two shows up as a wrong field.
PaddedNominalNamedRowExtension := [].{}

Pad(r) := { z : U64, _ : U8, ..r }

read_z : Pad(r) -> U64
read_z = |item| item.z

expect {
    item : Pad({ a : U64 })
    item = Pad.{ z: 7, a: 11 }
    read_z(item) == 7 and item.a == 11
}

expect {
    item : Pad({ b : U64, a : U64 })
    item = Pad.{ z: 7, b: 13, a: 11 }
    read_z(item) == 7 and item.a == 11 and item.b == 13
}

expect {
    item : Pad({ name : Str })
    item = Pad.{ z: 7, name: "a string long enough to live on the heap" }
    read_z(item) == 7 and item.name == "a string long enough to live on the heap"
}

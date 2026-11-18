# Stored nominal types use structural layout unless an unnamed field opts in.
Plain := { z : U8, a : U64 }
Padded := { z : U8, _ : {}, a : U64 }
Generic(a) := { value : a }
Opaque :: { bits : U128 }
Recursive := { children : List(Recursive) }

plain : List(Plain)
plain = [{ z: 7, a: 9 }]

padded : List(Padded)
padded = [{ z: 7, a: 9 }]

generic : List(Generic(Str))
generic = [{ value: "hello" }]

opaque : List(Opaque)
opaque = []

recursive : List(Recursive)
recursive = [{ children: [] }]

expect plain.map(|value| value.a) == [9]
expect plain.map(|value| value.z) == [7]
expect padded.map(|value| value.a) == [9]
expect padded.map(|value| value.z) == [7]
expect generic.map(|item| item.value) == ["hello"]
expect opaque.len() == 0
expect recursive.map(|node| node.children.len()) == [0]

app [main!] { pf: platform "./platform/main.roc" }

# A padded nominal's generic bodies compose its descriptor at runtime, whose
# entries are 32-bit on this target, and its closed code uses committed offsets.
Slot(a) := { z : U32, _ : U32, a : a, y : U8 }.{
	new : U32, a -> Slot(a)
	new = |z, a| { z, a, y: 3 }

	set : Slot(a), b -> Slot(b)
	set = |s, b| { z: s.z, a: b, y: s.y }

	value : Slot(a) -> a
	value = |s| s.a
}

Padded := { z : U32, _ : U32, a : U64 }

make : U32, U64 -> Padded
make = |z, a| { z, a }

read_z = |r| r.z

main! = || {
	text = Slot.new(1, "a string that is too long to be stored inline")
	number : Slot(U64)
	number = Slot.set(text, 42)
	lifted : Padded
	lifted = make(5, 6)
	"${Slot.value(text)} ${number.a.to_str()} ${number.z.to_str()} ${number.y.to_str()} ${lifted.a.to_str()} ${read_z(lifted).to_str()}"
}

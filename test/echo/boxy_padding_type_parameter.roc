# A padded nominal whose padding is its type parameter is one record row with a
# different size at each argument, so under `--specialize=no` a body generic in
# the parameter takes its descriptor even though no value holds it.
Pad(p) := { x : U8, _ : p, w : U16 }.{
	make : U8, U16 -> Pad(p)
	make = |x, w| { x, w }

	sum : Pad(p) -> U16
	sum = |s| s.x.to_u16() + s.w
}

# A generic type recursive only through an unnamed field: its descriptor has to
# break that cycle as it does a named field's.
Spacer(a) := { x : a, _ : [Nil, Cons(Spacer(a))], y : U8 }.{
	new : a, U8 -> Spacer(a)
	new = |x, y| { x, y }

	top : Spacer(a) -> a
	top = |s| s.x
}

# A declaration in a body, whose padding is a variable of the enclosing function.
in_body : a, U8 -> U16
in_body = |_, x| {
	Local := { x : U8, _ : a, w : U16 }
	local : Local
	local = { x, w: 4 }
	local.x.to_u16() + local.w
}

main! = |_args| {
	small : Pad(U64)
	small = Pad.make(1, 2)
	large : Pad(Str)
	large = Pad.make(3, 4)
	spacer = Spacer.new("long enough to be stored outside the record", 2)
	echo!("${Str.inspect(Pad.sum(small))} ${Str.inspect(Pad.sum(large))} ${Str.inspect(large.w)} ${Str.inspect(in_body("a", 1))} ${Spacer.top(spacer)} ${Str.inspect(spacer.y)}\n")
	Ok({})
}

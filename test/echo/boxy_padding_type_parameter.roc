# A padded nominal whose padding is its type parameter is one record row with a
# different size at each argument, so under `--specialize=no` a body generic in
# the parameter takes its descriptor even though no value holds it.
Pad(p) := { x : U8, _ : p, w : U16 }.{
	make : U8, U16 -> Pad(p)
	make = |x, w| { x, w }

	sum : Pad(p) -> U16
	sum = |s| s.x.to_u16() + s.w
}

main! = |_args| {
	small : Pad(U64)
	small = Pad.make(1, 2)
	large : Pad(Str)
	large = Pad.make(3, 4)
	echo!("${Str.inspect(Pad.sum(small))} ${Str.inspect(Pad.sum(large))} ${Str.inspect(large.w)}\n")
	Ok({})
}

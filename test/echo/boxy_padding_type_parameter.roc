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

# A recursive type whose variable appears only in its padding. Its recursive
# descriptor is cached by its variables, so each instantiation has its own.
Linked(p) := { x : U8, _ : p, next : [End, Next(Box(Linked(p)))], w : U16 }.{
	new : U8 -> Linked(p)
	new = |x| { x, next: Next(Box.box({ x: x + 1, next: End, w: 2 })), w: 1 }
}

second : Linked(p) -> U8
second = |l| match l.next {
	End => 0
	Next(b) => Box.unbox(b).x + Box.unbox(b).w.to_u8()
}

# Closed readers use the offsets the generic `new` has to agree with.
second_small : Linked(U8) -> U8
second_small = |l| match l.next {
	End => 0
	Next(b) => Box.unbox(b).x + Box.unbox(b).w.to_u8()
}

second_large : Linked(Str) -> U8
second_large = |l| match l.next {
	End => 0
	Next(b) => Box.unbox(b).x + Box.unbox(b).w.to_u8()
}

stored_spacer : Spacer(Str)
stored_spacer = Spacer.new("a stored string too long to be inline", 4)

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
	short_chain : Linked(U8)
	short_chain = Linked.new(1)
	long_chain : Linked(Str)
	long_chain = Linked.new(5)
	echo!("${Str.inspect(second_small(short_chain))} ${Str.inspect(second_large(long_chain))} ${Str.inspect(second(short_chain))} ${Str.inspect(second(long_chain))} ${Str.inspect(short_chain.w)} ${Str.inspect(long_chain.w)}\n")
	echo!("${stored_spacer.x} ${Str.inspect(stored_spacer.y)}\n")
	Ok({})
}

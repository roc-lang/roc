# Types declared in a function body whose backings name the function's type
# variables, with the function called at several instantiations of them. Each
# instantiation gives the type its own instance: values reach methods
# (annotated or not, called directly, by dispatch, by `==`, and by inspection),
# other local types, and generic helpers at the right types.

get_v = |w| w.v

show = |x| Str.inspect(x)

direct : a -> a
direct = |x| {
	W := { v : a }.{
		get = |w| w.v
	}

	W.get(W.{ v: x })
}

annotated : a -> a
annotated = |x| {
	W := { v : a }.{
		get : W -> a
		get = |w| w.v
	}

	W.get(W.{ v: x })
}

dispatched : a -> a
dispatched = |x| {
	W := { v : a }.{
		get = |w| w.v
	}

	(W.{ v: x }).get()
}

through_helper : a -> a
through_helper = |x| {
	W := { v : a }

	get_v(W.{ v: x })
}

described : a -> Str where [a.to_str : a -> Str, a.is_eq : a, a -> Bool]
described = |x| {
	W := { v : a }.{
		render = |w| "W(${w.v.to_str()})"
		is_eq = |l, r| l.v == r.v
	}

	w = W.{ v: x }
	same = if w == w "same" else "different"
	"${w.render()} ${same}"
}

# A generic function reaching the local types at its own instantiations.
generic_caller : a -> Str where [a.to_str : a -> Str, a.is_eq : a, a -> Bool]
generic_caller = |x| "${described(x)} ${Str.inspect(direct([x]))}"

nested : a -> a
nested = |x| {
	Inner := { v : a }
	Outer := { inner : Inner }.{
		get = |o| o.inner.v
	}

	Outer.get(Outer.{ inner: Inner.{ v: x } })
}

pairs : a, b -> Str where [a.to_str : a -> Str, b.to_str : b -> Str]
pairs = |x, y| {
	P(c) := { v : a, c : c }.{
		to_inspect = |p| "P(${p.v.to_str()}, ${p.c.to_str()})"
	}

	"${show(P.{ v: x, c: y })} ${Str.inspect([P.{ v: x, c: {} }])}"
}

main! = |_args| {
	echo!("${Str.inspect(direct(1.I8))} ${direct("q")} ${Str.inspect(direct(2.5.F64))}\n")
	echo!("${Str.inspect(annotated(1.I8))} ${annotated("q")} ${Str.inspect(annotated(Bool.True))}\n")
	echo!("${Str.inspect(dispatched(1.I8))} ${dispatched("q")} ${Str.inspect(dispatched([3.U16]))}\n")
	echo!("${Str.inspect(through_helper(1.I8))} ${through_helper("q")} ${Str.inspect(through_helper({ z: 4.U32 }))}\n")
	echo!("${described(1.I8)} ${described("q")} ${described(5.U64)}\n")
	echo!("${generic_caller(8.U8)} ${generic_caller("t")}\n")
	echo!("${Str.inspect(nested(1.I8))} ${nested("q")} ${Str.inspect(nested(6.I64))}\n")
	echo!("${pairs(1.I8, 2.I16)} ${pairs("q", 2.I16)} ${pairs(7.U8, "r")}\n")
	Ok({})
}

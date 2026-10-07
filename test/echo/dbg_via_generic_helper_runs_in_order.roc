# A hoisted binding reaches a `dbg` only through a function it passes to a
# generic helper. The binding is not evaluated at compile time: in every
# lowering mode the `dbg` runs in source order, between "before" and "middle".
main! = |_args| {
	echo!("before\n")
	x = apply(noisy, 2)
	echo!("middle\n")
	echo!("${x.to_str()}\n")
	Ok({})
}

apply : (a -> b), a -> b
apply = |f, x| f(x)

noisy : I64 -> I64
noisy = |n| {
	dbg n
	n + 1
}

# A boxed function inspects as a box under every strategy, though its value is
# the function's erased callable.
f : I64 -> I64
f = |x| x + 1

main! = |_args| {
	echo!(Str.inspect(Box.box(f)))
	echo!(Str.inspect({ b: Box.box(f) }))
	echo!(Str.inspect(Box.box(Box.box(f))))
	Ok({})
}

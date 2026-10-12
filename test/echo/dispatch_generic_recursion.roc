# A method that recurses on itself through a top-level helper whose static
# dispatch resolves back to the method.
N := { n : U64 }.{
	pred = |x| N.{ n: x.n - 1 }
	is_zero = |x| x.n == 0
	count : N -> U64
	count = |x| if x.is_zero() 0 else 1 + step(x)
}

step = |x| x.pred().count()

main! = |_args| {
	echo!(Str.inspect(N.count(N.{ n: 5 })))
	echo!(Str.inspect(step(N.{ n: 3 })))
	Ok({})
}

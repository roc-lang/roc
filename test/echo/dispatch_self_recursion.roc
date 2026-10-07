# A method that recurses on itself through static dispatch on a value another
# method produced: `x.pred().f()` resolves back to `N.f`.
N := { n : U64 }.{
	pred = |x| N.{ n: x.n - 1 }
	f : N -> Bool
	f = |x| if x.n == 0 Bool.True else x.pred().f()
}

main! = |_args| {
	echo!(Str.inspect(N.f(N.{ n: 4 })))
	Ok({})
}

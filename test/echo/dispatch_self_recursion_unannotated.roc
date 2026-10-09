# An unannotated method that recurses on itself through static dispatch on a
# value a qualified method call produced: `N.pred(x).f()` resolves back to `N.f`.
N := { n : U64 }.{
	pred = |x| N.{ n: x.n - 1 }
	f = |x| if x.n == 0 Bool.True else N.pred(x).f()
}

main! = |_args| {
	echo!(Str.inspect(N.f(N.{ n: 4 })))
	Ok({})
}

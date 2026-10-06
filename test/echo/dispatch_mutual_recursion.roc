# Two methods that recurse on each other through static dispatch on a value
# another method produced.
N := { n : U64 }.{
	pred = |x| N.{ n: x.n - 1 }
	is_even : N -> Bool
	is_even = |x| if x.n == 0 Bool.True else x.pred().is_odd()
	is_odd : N -> Bool
	is_odd = |x| if x.n == 0 Bool.False else x.pred().is_even()
}

main! = |_args| {
	echo!(Str.inspect(N.is_even(N.{ n: 10 })))
	echo!(Str.inspect(N.is_odd(N.{ n: 7 })))
	echo!(Str.inspect(N.is_even(N.{ n: 3 })))
	Ok({})
}

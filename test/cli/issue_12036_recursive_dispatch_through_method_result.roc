# Repro for https://github.com/roc-lang/roc/issues/12036: mutual recursion that
# dispatches on another method's result closes at the repeated concrete state.
Num := { n : U64 }.{
	pred = |x| Num.{ n: x.n - 1 }
	is_zero = |x| x.n == 0
	is_even = |x| if x.is_zero() Bool.True else x.pred().is_odd()
	is_odd = |x| if x.is_zero() Bool.False else x.pred().is_even()
}

main! = |_args| {
	echo!(Str.inspect(Num.is_even(Num.{ n: 4 })))
	echo!(Str.inspect(Num.is_odd(Num.{ n: 4 })))
	Ok({})
}

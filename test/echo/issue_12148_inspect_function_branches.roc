# `Str.inspect` renders any function as `<function>` without calling it, so the
# lambdas below are never applied and nothing constrains their parameters. The
# argument is not a lambda itself, so it still runs for its effects.
main! = |args| {
	verbose = List.len(args) > 100
	echo!(Str.inspect(if verbose |x| x + 1 else |x| x * 2))
	echo!(Str.inspect(if verbose |x| x else |x| x))
	echo!(Str.inspect(match verbose {
		True => |x| x
		False => |_| {}
	}))
	echo!(Str.inspect({
		echo!(" effect ")
		|x| x
	}))
	Ok({})
}

# A block bound to a name, whose final value is a use of a name that binds
# nothing, runs its earlier statements before it crashes at that use.
main! = |args| {
	echo!("start\n")
	z = {
		echo!("before\n")
		y = undefined_fn(args)
		y
	}
	echo!("${Str.inspect(z)}")
	Ok({})
}

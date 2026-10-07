# A function whose body block ends in a use of a name that binds nothing runs
# the block's earlier statements, then crashes where the use is evaluated.
helper! = |x| {
	echo!("before\n")
	y = undefined_fn(x)
	y
}

main! = |args| {
	n = List.len(args)
	echo!("start\n")
	helper!(n)
	Ok({})
}

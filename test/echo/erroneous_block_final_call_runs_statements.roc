# A function whose body block ends in a call to an undefined name runs the
# block's earlier statements, then crashes where that call is evaluated.
helper! = |x| {
	echo!("before\n")
	undefined_fn(x)
}

main! = |args| {
	echo!("start\n")
	helper!(List.len(args))
	Ok({})
}

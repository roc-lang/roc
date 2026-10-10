# An annotated function whose body block ends in a use of a name that binds
# nothing runs the block's earlier statements, then crashes at that use.
helper! : U64 => {}
helper! = |x| {
	echo!("before\n")
	y = undefined_fn(x)
	y
}

main! = |args| {
	echo!("start\n")
	helper!(List.len(args))
	Ok({})
}

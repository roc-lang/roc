# A method call on the item of a `for` loop over an empty list whose item type
# is unresolved is rejected. The loop body never runs, so running the program
# anyway reaches the end.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	x = []
	for item in x {
		echo!(item.frob({
			dbg "arg"
			n
		}))
	}
	echo!("after\n")
	Ok({})
}

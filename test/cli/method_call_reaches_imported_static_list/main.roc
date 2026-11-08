import Page

main! = |args| {
	echo!(Str.join_with(Page.greet(args.len()), ","))
	Ok({})
}

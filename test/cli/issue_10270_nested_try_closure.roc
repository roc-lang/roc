mk = |f| {
	show = || f({}).map_err(|_| ShowFailed)
	show
}

main! = |_args| {
	f : {} -> Try({}, [Empty])
	f = |_| Ok({})
	mk(f)()?
	Ok({})
}

run! : List(Str) => Try({}, Str)
run! = |args| {
	r : Try(List(Str), [Failed(Str)])
	r = if (args.len() > 2) Ok(["a"]) else Err(Failed("x"))
	_ = r.map_err(|Failed(s)| s)?
	echo!(Str.inspect(r))
	Ok({})
}

main! = |args| {
	_ = run!(args)
	Ok({})
}

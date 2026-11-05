# Issues 12771 and 12762: a `?` in a lambda whose error row nothing closes,
# since every use matches the error with `Err(_)`.

Handlers : { on_open : U64 -> Try(U64, [A]) }

opened = |state, handlers| {
	match (handlers.on_open)(state) {
		Ok(_) => Ok({})
		Err(e) => Err(e)
	}
}

handlers : Handlers
handlers = { on_open: |n| Ok(n) }

wrap = |f| |x| match f(x) {
	Ok(_) => {}
	Err(_) => {}
}

main! = |_args| {
	run = wrap(
		|x| {
			opened(x, handlers)?
			Ok(1)
		},
	)
	run(0)
	echo!("ok")
	Ok({})
}

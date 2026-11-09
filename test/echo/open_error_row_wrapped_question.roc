# Issue 12762: `?` on a call returning a wrapped open error row, inside a
# lambda whose error only `Err(_)` reads.

Handlers(s, e) : { on_open : s -> Try(s, [Closed, ..e]) }

opened : s, Handlers(s, e) -> Try({}, [Handler([Closed, ..e])])
opened = |state, handlers| {
	_ = (handlers.on_open)(state).map_err(|e| Handler(e))?
	Ok({})
}

handlers : Handlers(U64, [])
handlers = { on_open: |n| Ok(n) }

run = |f| match f({}) {
	Ok(s) => s
	Err(_) => "err"
}

main! = |_| {
	echo!(run(|{}| {
		opened(0, handlers)?
		Ok("done")
	}))
	Ok({})
}

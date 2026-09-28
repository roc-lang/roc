# repro for https://github.com/roc-lang/roc/issues/11737
#
# `roc check` on this program must succeed without crashing. The bug: a nominal
# method called twice from a generic function, where the method body dispatches
# another method on a value coming from its own type parameter (`fetch({}).repeat(3)`),
# violates a postcheck invariant and crashes the compiler. It must type-check cleanly.

app [main!] {}

Db(deps) :: { deps : deps }.{
	run = |db, body| {
		fetch = db.deps.fetch
		_ = fetch({}).repeat(3)
		body({})
	}
}

demo = |db| {
	_ = db.run(|{}| "a")
	db.run(|{}| "b")
}

main! = |_args| {
	_ = demo(Db.{ deps: { fetch: |{}| "x" } })
	Ok({})
}

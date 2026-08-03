app [main!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10270

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

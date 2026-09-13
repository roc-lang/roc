# repro for https://github.com/roc-lang/roc/issues/11231
f = |t| t.0

main! = |_| {
	Ok(f(0))
}

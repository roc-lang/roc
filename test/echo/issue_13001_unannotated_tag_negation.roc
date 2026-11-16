# repro for https://github.com/roc-lang/roc/issues/13001
# Negating an unannotated tag binding resolves the tag union at Bool and calls
# Bool.not. The condition is known at compile time, which is reported as a
# warning.
go : U64 -> U64
go = |n| {
	done = False
	if !done { n } else { 0 }
}

main! = |args| {
	echo!(Str.inspect(go(args.len())))
	Ok({})
}

# repro for https://github.com/roc-lang/roc/issues/12495
# An optimized build of a program that fills a `Dict` from runtime values
# must compile and find every key it inserted.
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = |args| {
	var $seen = Dict.empty()
	for arg in args {
		$seen = $seen.insert(arg, {})
	}
	Stdout.line!(Str.join_with(args.map(|arg| if ($seen.contains(arg)) "T" else "F"), ""))
	Ok({})
}

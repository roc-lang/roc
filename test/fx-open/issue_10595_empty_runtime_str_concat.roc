app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |args| {
	empty = if args.len() > 99 "z" else ""
	Stdout.line!("aaaaaaaaaaaaaaaaaaaaaaaa${empty}b")
	Ok({})
}

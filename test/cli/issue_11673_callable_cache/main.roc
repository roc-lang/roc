app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Eq

boundary : Str, (I64, I64 -> Bool) -> Str
boundary = |label, eq| if eq(1, 1) label else "differs"

main! = |args| {
	Stdout.line!(boundary("b", |x, y| x > y))
	# The label depends on the arguments, so this call runs at runtime and
	# reaches the cached Eq.same.
	Stdout.line!(boundary(if args.is_empty() "a" else "a", Eq.same))
	Ok({})
}

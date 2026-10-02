app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Eq

boundary : Str, (I64, I64 -> Bool) -> Str
boundary = |label, eq| if eq(1, 1) label else "differs"

main! = |args| {
	Stdout.line!(boundary("b", |x, y| x > y))
	# The label depends on `args`, so this call, and `Eq.same`, run at runtime.
	label = List.first(List.concat(["a"], args)) ?? "a"
	Stdout.line!(boundary(label, Eq.same))
	Ok({})
}

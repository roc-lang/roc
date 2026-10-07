app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Eq

boundary : Str, (I64, I64 -> Bool) -> Str
boundary = |label, eq| if eq(1, 1) label else "differs"

main! = |_args| {
	Stdout.line!(boundary("a", Eq.same))
	Stdout.line!(boundary("b", |x, y| x > y))
	Ok({})
}

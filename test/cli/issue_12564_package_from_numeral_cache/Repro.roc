# Converts a literal of Unit through the from_numeral Primer's check cached.
app [main!] { pf: platform "../../fx-open/platform/main.roc", u: "pkg/main.roc" }

import pf.Stdout
import u.Unit

main! = |_args| {
	x : Unit
	x = 10
	Stdout.line!(x.raw().to_str())
	Ok({})
}

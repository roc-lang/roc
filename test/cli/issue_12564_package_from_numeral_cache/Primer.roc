# Imports Unit without converting a literal of it.
app [main!] { pf: platform "../../fx-open/platform/main.roc", u: "pkg/main.roc" }

import pf.Stdout
import u.Unit

main! = |_args| {
	Stdout.line!("primer")
	Ok({})
}

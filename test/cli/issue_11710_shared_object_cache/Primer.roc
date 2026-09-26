# Imports Lib without calling it. Building this app caches Lib's procedures.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

main! = |_args| {
	Stdout.line!("primer")
	Ok({})
}

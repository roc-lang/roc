# Passes Lib's own functions to Lib's higher-order function. Editing this
# module leaves those specializations' identities unchanged, so a rebuild
# after the edit is served them from the object cache.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

main! = |args| {
	start = List.len(args) + 1
	Stdout.line!(Lib.apply_steps(Lib.double, start, 5).to_str())
	Stdout.line!(Lib.apply_steps(Lib.increment, start, 5).to_str())
	Stdout.line!(Lib.apply_steps(Lib.double, start + 1, 3).to_str())
	Ok({})
}

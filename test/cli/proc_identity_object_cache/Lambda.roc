# Passes the app's own closures to Lib's higher-order function. Editing a
# closure's body changes that closure's code and nothing else: Lib's
# specializations that receive it are still served from the object cache,
# and they call the edited closure.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

main! = |args| {
	offset = List.len(args) + 1
	Stdout.line!(Lib.apply_steps(|n| n * 3 + offset, 1, 4).to_str())
	Stdout.line!(Lib.apply_steps(|n| n + offset, 2, 3).to_str())
	Ok({})
}

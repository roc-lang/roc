# Calls Zeros and Dash. Its output must not depend on what the object cache holds.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Zeros
import Dash

main! = |args| {
	n = List.len(args).to_u8_wrap()
	Stdout.line!(Str.inspect(List.concat(Zeros.frame(n), Dash.frame(n))))
	Ok({})
}

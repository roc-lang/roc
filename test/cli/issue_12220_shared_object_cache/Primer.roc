# Calls only Dash. Building this app fills the object cache Repro.roc's build reads.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Dash

main! = |args| {
	Stdout.line!(Str.inspect(Dash.frame(List.len(args).to_u8_wrap())))
	Ok({})
}

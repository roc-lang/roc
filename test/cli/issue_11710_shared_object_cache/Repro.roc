# Calls Lib.parse. Building this app after Primer.roc consumes Primer's pack.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

main! = |args| {
	message =
		if List.len(args) > 1 {
			match Lib.parse(Str.join_with(args, " ")) {
				Ok(_) => "parsed"
				Err(_) => "invalid"
			}
		} else {
			"no url"
		}
	Stdout.line!(message)
	Ok({})
}

# Calls Lib.parse and Lib.query_pairs on a path its run never takes. Building
# this app caches the Lib procedures its program compiled.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

main! = |args| {
	message =
		if List.len(args) > 1 {
			match Lib.parse(Str.join_with(args, " ")) {
				Ok(url) => Str.join_with(Lib.query_pairs(url).map(|(name, _)| name), "&")
				Err(_) => "invalid"
			}
		} else {
			"primer"
		}
	Stdout.line!(message)
	Ok({})
}

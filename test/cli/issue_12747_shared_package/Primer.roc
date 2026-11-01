# Calls the closed `Flow.score` at runtime without boxing any function.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "../issue_12747_check_then_build/pkg/main.roc" }

import pf.Stdout
import pkg.Flow

main! = |args| {
	Stdout.line!(Flow.done(args.len()).score().to_str())
	Ok({})
}

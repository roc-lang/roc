# Boxes a function and passes it to the closed `Flow.score` Primer also calls.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "../issue_12747_check_then_build/pkg/main.roc" }

import pf.Stdout
import pkg.Flow

main! = |args| {
	flow = Flow.later(|input| input + args.len())
	Stdout.line!("${flow.score().to_str()} ${flow.resume(1).to_str()}")
	Ok({})
}

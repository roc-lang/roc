# `baseline` runs `Flow.score` at compile time, where nothing boxes a function.
# `main!` boxes one and passes it to the same closed `Flow.score`.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "pkg/main.roc" }

import pf.Stdout
import pkg.Flow

baseline : U64
baseline = Flow.done(41).score()

main! = |args| {
	flow = Flow.later(|input| input + baseline + args.len())
	Stdout.line!("${baseline.to_str()} ${flow.score().to_str()} ${flow.resume(1).to_str()}")
	Ok({})
}

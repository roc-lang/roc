# Repro for https://github.com/roc-lang/roc/issues/10167: a three-field parser
# stored as a compile-time constant must materialize during `roc build`.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import CSV
import pf.Stdin

main! = || {
	match CSV.parse_str(movie_parser, Stdin.line!()) {
		Ok(_) => {}
		Err(_) => {}
	}
}

movie_parser =
	CSV.record(|title| |year| |actor| { title, year, actor })
		.keep(CSV.field(CSV.string))
		.keep(CSV.field(CSV.u64))
		.keep(CSV.field(CSV.string))

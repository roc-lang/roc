app [main!] {
	pf: platform "../../fx/platform/main.roc",
	repro: "./pkg/main.roc",
}

import pf.Stdout
import repro.Base exposing [ArgParserResult]
import repro.Cli
import repro.Param

report! : Str, ArgParserResult(List(Str)) => {}
report! = |label, outcome| {
	match outcome {
		SuccessfullyParsed(values) => {
			match values {
				["alpha", "beta"] => Stdout.line!(Str.concat(label, " ok"))
				_ => Stdout.line!(Str.concat(label, " wrong values"))
			}
		}
		IncorrectUsage(_) => Stdout.line!(Str.concat(label, " incorrect usage"))
	}
}

main! = || {
	unqualified = Cli.finish(Param.str_list({}))
	report!("unqualified", (unqualified.parser)(["prog", "alpha", "beta"]))

	qualified = Cli.finish(Param.str_list_qualified({}))
	report!("qualified", (qualified.parser)(["prog", "alpha", "beta"]))
}

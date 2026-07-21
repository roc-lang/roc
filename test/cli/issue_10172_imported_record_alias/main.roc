# Repro for https://github.com/roc-lang/roc/issues/10172
app [main!] {
	pf: platform "../../fx/platform/main.roc",
	repro: "./pkg/main.roc",
}

import pf.Stdout
import repro.Validate

main! = || {
	option = { short: "a" }
	result = Validate.validate([option])
	Stdout.line!(Str.inspect(result))
}

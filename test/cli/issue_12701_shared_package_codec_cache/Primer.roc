# Checking this app caches Other and Form. Both store a function that
# Api.parser specializes at one codec contract, so one specialization serves
# them, first requested from Other.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "pkg/main.roc" }

import pf.Stdout
import pkg.Other
import pkg.Form

main! = |_args| {
	Stdout.line!("primer")
	Ok({})
}

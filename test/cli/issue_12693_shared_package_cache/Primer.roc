# Reads Form.submit, whose stored value Repro's build reuses from the cache.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "pkg/main.roc" }

import pf.Stdout
import pkg.Form

main! = |_args| {
	_ = Form.submit
	Stdout.line!("primer")
	Ok({})
}

# The cache this check writes is reused by Repro's build.

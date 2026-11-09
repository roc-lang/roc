# Calls the cached Form.submit without importing Other.
app [main!] { pf: platform "../../fx-open/platform/main.roc", pkg: "pkg/main.roc" }

import pf.Stdout
import pkg.Form

main! = |args| {
	Stdout.line!(Str.inspect(Form.submit(Str.inspect(args.len()))))
	Ok({})
}

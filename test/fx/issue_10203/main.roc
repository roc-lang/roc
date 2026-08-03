# Repro for https://github.com/roc-lang/roc/issues/10203
app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import Bar

main! = || {
	_foo = Bar.build([])
	Stdout.line!("done")
}

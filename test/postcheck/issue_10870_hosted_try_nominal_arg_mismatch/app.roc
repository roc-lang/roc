app [main!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10870
#
# `Env.var!` produces an `OsStr`, so handing it to `Stdout.line!`, which needs
# a `Str`, is a type mismatch. Checking reports that mismatch; lowering the
# checked program has to reach the same conclusion instead of crashing while it
# builds the hosted-try adapter for these `?`s.

import pf.Env
import pf.Stdout

main! = |_args| {
	home = Env.var!("HOME")?
	Stdout.line!(home)?
	Ok({})
}

app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/9963
#
# The platform explicitly reconstructs the host's closed error before `?`
# combines it with Exit(I32). The host always returns Ok("ok"); preserving
# its declared ABI must keep that value from being misread as an Err.

import pf.Fallible
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32), HostErr(Str)])
main! = |_args| {
	match_value = Fallible.via_match!({})?
	Stdout.line!("match ok: ${match_value}")

	question_value = Fallible.via_question!({})?
	Stdout.line!("question ok: ${question_value}")

	Ok({})
}

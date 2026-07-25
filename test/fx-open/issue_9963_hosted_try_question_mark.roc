app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/9963
#
# FallibleHost.str_ok! is a hosted function returning Try(Str, [HostErr(Str)])
# whose host implementation always returns Ok("ok"). Unwrapping it with `?`
# inside Ok(...) re-raises the hosted function's closed error row at the use
# site (design.md "Try Question Error Re-raise"); the hosted callee itself
# must stay specialized at its declared ABI row — a spec at the widened
# layout misread Ok("ok") as Err(HostErr("ok")).

import pf.Fallible
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32), HostErr(Str), ..])
main! = |_args| {
	match_value = Fallible.via_match!({})?
	Stdout.line!("match ok: ${match_value}")

	question_value = Fallible.via_question!({})?
	Stdout.line!("question ok: ${question_value}")

	Ok({})
}

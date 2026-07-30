app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/9963
#
# FallibleHost.str_ok! is a hosted function returning Try(Str, [HostErr(Str)])
# whose host implementation always returns Ok("ok"). Unwrapping it with `?`
# inside Ok(...) widens the hosted function's closed error row at the use site
# (here with Exit(I32) from main!'s error union); the compiler must bridge that
# widened request with an adapter instead of specializing the host ABI at the
# widened layout, which misread Ok("ok") as Err(HostErr("ok")).

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

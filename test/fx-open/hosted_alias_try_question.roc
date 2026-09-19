app [main!] { pf: platform "./platform/fallible_alias_main.roc" }

# A hosted result declared through a transparent alias, widened with `?`:
# platform/FallibleHostAlias.roc declares its host symbol as IoResult(Str), an
# alias over Try(Str, [HostErr(Str)]), and platform/FallibleAlias.roc unwraps
# it into a closed row that also carries Widened(I32). Monotype lowering keeps
# the alias on the declared side, so building the widening adapter has to cross
# it to find the `Try` the checker published the capability for; reading the
# alias as written declines an adapter the relation already committed to.
#
# The host always returns Ok("ok"), so this must print "ok". An extern emitted
# at the widened row would read those same bytes as Err.

import pf.FallibleAlias
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	Stdout.line!("alias closed wider: ${wider_row(FallibleAlias.via_question_closed_wider!({}))}")

	Ok({})
}

wider_row : Try(Str, [HostErr(Str), Widened(I32)]) -> Str
wider_row = |result|
	match result {
		Ok(value) => value
		Err(HostErr(message)) => "misread as Err(HostErr(${message}))"
		Err(Widened(_)) => "misread as Err(Widened)"
	}

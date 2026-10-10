app [main!] { pf: platform "./platform/fallible_alias_main.roc" }

# An alias-wrapped hosted result explicitly reconstructed into a wider row.
# The host always returns Ok("ok"); a widened extern would misread it as Err.

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

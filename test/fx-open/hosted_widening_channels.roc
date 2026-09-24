app [main!] { pf: platform "./platform/fallible_widen_main.roc" }

# Every channel a hosted result reaches a caller through, each at an error row
# wider than the one FallibleHost.str_ok! declares (platform/FallibleWiden.roc):
# an annotated binding, an argument, a record field, the function carried as a
# value, passed to a higher-order function, boxed, `?` in an unannotated
# function, and the function named through an alias of its owner. Row
# subsumption re-opens the hosted `Try` error row at each use (design.md "Row
# Subsumption"), so all of them typecheck.
#
# The host always returns Ok("ok"), so every line must print "ok". An extern
# emitted at the wider row instead of the declared one would read those same
# bytes as Err (design.md "Host Symbol ABI"), which is why each line prints
# what it actually received. hosted_channels_declared.roc is the same set of
# channels at the declared row.

import pf.FallibleWiden
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	Stdout.line!("annotation: ${wider_row(FallibleWiden.via_annotation!({}))}")
	Stdout.line!("argument: ${wider_row(FallibleWiden.via_argument!({}))}")
	Stdout.line!("record field: ${wider_row(FallibleWiden.via_record_field!({}))}")
	Stdout.line!("value: ${wider_row(FallibleWiden.via_value!({}))}")
	Stdout.line!("higher order: ${wider_row(FallibleWiden.via_higher_order!({}))}")
	Stdout.line!("box: ${wider_row(FallibleWiden.via_box!({}))}")
	Stdout.line!("unannotated question: ${wider_row(FallibleWiden.via_unannotated_question!({}))}")
	Stdout.line!("alias owner: ${wider_row(FallibleWiden.via_alias_owner!({}))}")

	Ok({})
}

wider_row : Try(Str, [HostErr(Str), Widened(I32)]) -> Str
wider_row = |result|
	match result {
		Ok(value) => value
		Err(HostErr(message)) => "misread as Err(HostErr(${message}))"
		Err(Widened(_)) => "misread as Err(Widened)"
	}

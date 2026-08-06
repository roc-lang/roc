app [main!] { pf: platform "./platform/main.roc" }

# Accepted side of the non-`?` channels (platform/FallibleChannels.roc): a
# hosted result reaching an annotated binding, an argument, a record field, and
# a hand-written re-tag into a wider row. FallibleHost.str_ok!'s host always
# returns Ok("ok"), so each line must print "ok". An extern emitted at anything
# other than the declared row would read those same bytes as Err, which is why
# each line prints what it actually received (design.md "Host Symbol ABI").
# hosted_widening_channels.roc is the rejected counterpart.

import pf.FallibleChannels
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	Stdout.line!("annotation: ${declared_row(FallibleChannels.via_annotation!({}))}")
	Stdout.line!("argument: ${declared_row(FallibleChannels.via_argument!({}))}")
	Stdout.line!("record field: ${declared_row(FallibleChannels.via_record_field!({}))}")

	Stdout.line!("closed wider: ${wider_row(FallibleChannels.via_question_closed_wider!({}))}")
	Stdout.line!("retag: ${wider_row(FallibleChannels.via_retag!({}))}")

	Ok({})
}

declared_row : Try(Str, [HostErr(Str)]) -> Str
declared_row = |result|
	match result {
		Ok(value) => value
		Err(HostErr(message)) => "misread as Err(HostErr(${message}))"
	}

wider_row : Try(Str, [HostErr(Str), Widened(I32)]) -> Str
wider_row = |result|
	match result {
		Ok(value) => value
		Err(HostErr(message)) => "misread as Err(HostErr(${message}))"
		Err(Widened(_)) => "misread as Err(Widened)"
	}

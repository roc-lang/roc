app [main!] { pf: platform "./platform/main.roc" }

# Rejected side of the `?` re-raise rule (design.md "Try Question Error
# Re-raise"), paired with issue_9963_hosted_try_question_mark.roc (the
# accepted side): `?` re-raises the hosted callee's closed error row at an
# open row, but FallibleReject's annotated return row omits HostErr and its
# rigid extension cannot absorb it, so checking this app must fail with a
# type mismatch in that platform module.

import pf.FallibleReject
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
	value = FallibleReject.mismatched!({})?
	Stdout.line!("unreachable: ${value}")

	Ok({})
}

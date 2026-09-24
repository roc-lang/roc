app [main!] { pf: platform "./platform/fallible_reject_main.roc" }

# Rejected counterpart of issue_9963_hosted_try_question_mark.roc: `?` on a
# hosted call re-raises the hosted error row into the enclosing function's
# return row, whose annotation still bounds what that function may produce
# (design.md "Row Subsumption"). FallibleReject's annotation omits HostErr, so
# checking this app must fail with a type mismatch in that platform module.
# This app is well-typed on its own terms, so that mismatch is the only
# one. It handles the error with a catch-all: the rejected definition's row
# still carries the tag it leaked, and matching only the listed tag would
# report that leak again here.

import pf.FallibleReject

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args|
	match FallibleReject.mismatched!({}) {
		Ok(_) => Ok({})
		Err(_) => Err(Exit(1))
	}

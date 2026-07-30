app [main!] { pf: platform "./platform/main.roc" }

# Rejected side of the hosted-try-question-widening rule (design.md "Hosted
# Try Question Widening"), paired with issue_9963_hosted_try_question_mark.roc
# (the accepted side): a hosted callee's `?` widens only when its visible
# errors are included in the enclosing annotated return row. FallibleReject's
# annotation omits HostErr, so checking this app must fail with a type
# mismatch in that platform module.

import pf.FallibleReject
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
	value = FallibleReject.mismatched!({})?
	Stdout.line!("unreachable: ${value}")

	Ok({})
}

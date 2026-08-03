app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/10518
#
# A hosted function may return Try with an ordinary value error type. Calling
# this Ok-returning function must use its declared ABI and build successfully.

import pf.FallibleHostStrErr

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
	_ = FallibleHostStrErr.str_ok!({})
	Ok({})
}

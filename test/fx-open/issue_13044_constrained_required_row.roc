# https://github.com/roc-lang/roc/issues/13044
# Platform requirement checking must diagnose an unsatisfied row method.
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

describe : Try({}, [Exit(I32), ..e]) -> Str where [e.zap : e -> Str]
describe = |result| match result {
	Ok(_) => "ok"
	Err(_) => "err"
}

main! : List(Str) => Try({}, [Exit(I32), ..e]) where [e.zap : e -> Str]
main! = |_args| {
	result = Ok({})
	Stdout.line!(describe(result))
	result
}

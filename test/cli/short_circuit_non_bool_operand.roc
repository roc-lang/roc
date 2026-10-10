# An `and` whose right operand is not a `Bool` is reported as a bool operation.
# Checking retires the operator without touching the operand's callee, so
# `roc check` and `roc` report it instead of crashing while lowering it, and
# the independent work before it still runs.

describe : U64 -> [Verbose(U64)]
describe = |n| Verbose(n)

main! = |_| {
	echo!("before")
	if Bool.True and describe(1) { echo!("details") } else { {} }
	echo!(Str.inspect(describe(2)))
	Ok({})
}

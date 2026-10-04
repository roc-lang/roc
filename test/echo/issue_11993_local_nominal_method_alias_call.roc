# repro for https://github.com/roc-lang/roc/issues/11993
#
# `roc check` on this program must succeed without crashing. The bug: a method
# of a nominal type declared in a function body, bound to a local function that
# checking had promoted to a procedure of its own, dispatched to that function's
# local declaration instead, so lowering the call violated a postcheck invariant
# and crashed the compiler.

app [main!] {}

main! = |_args| {
	helper = |c| c.count + 1
	Counter := { count : U64 }.{
		value = helper
	}

	counter = Counter.{ count: 0 }
	count = counter.value()
	expect count == 1
	Ok({})
}

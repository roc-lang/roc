# repro for https://github.com/roc-lang/roc/issues/11993
#
# `roc check` on this program must succeed without crashing. The bug: a method
# call on a nominal type declared in a function body, whose method refers to
# nothing from the enclosing body, dispatched to the method's local declaration
# even though checking had promoted it to a procedure of its own, so lowering
# the call violated a postcheck invariant and crashed the compiler.

app [main!] {}

main! = |_args| {
	Counter := { count : U64 }.{
		value = |counter| counter.count
	}

	counter = Counter.{ count: 0 }
	count = counter.value()
	expect count == 0
	Ok({})
}

# repro for https://github.com/roc-lang/roc/issues/11993
#
# `roc check` on this program must succeed without crashing. The bug: a method
# call on a nominal type declared in a function body, whose method captures a
# local of the enclosing body, was selected as a compile-time root even though
# such a root has no declaration context in which to call that local method,
# so lowering the root violated a postcheck invariant and crashed the compiler.

app [main!] {}

main! = |_args| {
	offset = 1
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
	}

	counter = Counter.{ count: 0 }
	count = counter.value()
	expect count == 1
	Ok({})
}

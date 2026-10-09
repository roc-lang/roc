# repro for https://github.com/roc-lang/roc/issues/11993
#
# `roc check` on this program must report the method's capture without
# crashing. The bug: a call of a generic local helper on a constant value of a
# nominal type declared in a function body was selected as a compile-time
# root, although the helper's where-clause evidence is a method of that type
# that captures a local of the body, so lowering the root violated a postcheck
# invariant and crashed the compiler. Methods never capture, so the method is
# rejected at `offset`.

app [main!] {}

main! = |_args| {
	offset = 1
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
	}

	get = |c| c.value()
	count = get(Counter.{ count: 0 })
	expect count == 1
	Ok({})
}

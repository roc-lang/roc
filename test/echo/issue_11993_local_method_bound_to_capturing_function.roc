# repro for https://github.com/roc-lang/roc/issues/11993
#
# `roc check` on this program must succeed without crashing. The bug: a method
# of a nominal type declared in a function body, bound through a local alias to
# a local function that captures a local of the body, was not recognized as
# that local function, so a call of it on a constant was selected as a
# compile-time root and lowering the root violated a postcheck invariant and
# crashed the compiler.

app [main!] {}

main! = |_args| {
	offset = 1
	helper = |c| c.count + offset
	alias = helper
	Counter := { count : U64 }.{
		value = alias
	}

	count = Counter.{ count: 0 }.value()
	expect count == 1
	Ok({})
}

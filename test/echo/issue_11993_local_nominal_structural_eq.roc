# repro for https://github.com/roc-lang/roc/issues/11993
#
# `roc check` on this program must report the method's capture without
# crashing. The bug: a structural comparison of records whose field type is
# declared in a function body, with an `is_eq` method that captures a local of
# that body, was selected as a compile-time root even though the comparison
# dispatches that local method, so lowering the root violated a postcheck
# invariant and crashed the compiler. Methods never capture, so the method is
# rejected at `offset`.

app [main!] {}

main! = |_args| {
	offset = 1
	Counter := { count : U64 }.{
		is_eq = |a, b| a.count + offset == b.count
	}

	same = { c: Counter.{ count: 0 } } == { c: Counter.{ count: 1 } }
	expect same
	Ok({})
}

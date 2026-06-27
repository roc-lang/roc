# repro for https://github.com/roc-lang/roc/issues/9690
# A nested, self-recursive closure that captures a value ("radicand") from its
# enclosing function must compile and run through the LLVM (size/speed) backend.
# sqrt(625) == 25; the crash guard makes a wrong result observable without
# relying on optimized-build `dbg` output.
square_root : U64 -> U64
square_root = |radicand| {
	binary_search = |min, max| {
		val = (min + max) // 2
		square = val * val
		if square == radicand or min >= max {
			val
		} else if square > radicand {
			binary_search(min, (val - 1))
		} else {
			binary_search((val + 1), max)
		}
	}
	binary_search(0, radicand)
}

main! = |args| {
	arg_count = List.len(args)
	result = square_root((25 * 25) + arg_count - arg_count)
	if result != 25 {
		crash "square_root(625) should be 25"
	}
	Ok({})
}

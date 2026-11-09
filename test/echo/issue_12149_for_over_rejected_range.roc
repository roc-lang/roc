# The range's bounds are quote literals whose type is decided only once the
# loop body passes each item to `echo!`, which makes them `Str`. `Str` has no
# `range_inclusive_to`, so the `..=` operator is rejected after the loop was
# checked. The loop then binds nothing and never iterates: the program runs
# until the loop evaluates its iterable and crashes there.
main! = |_| {
	echo!("before")
	for letter in "a"..="z" {
		echo!(letter)
	}
	echo!("after")
	Ok({})
}

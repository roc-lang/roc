# A `for` loop over a range whose bounds have no range constructor. The
# string literals' type is only decided by `echo!(letter)` in the body, which
# is where the range's rejected `range_inclusive_to` gets resolved, but the
# range itself is retired as a runtime error: the loop crashes where it
# evaluates its iterable.
main! = |_| {
	echo!("before\n")
	for letter in "a"..="z" {
		echo!(letter)
	}
	Ok({})
}

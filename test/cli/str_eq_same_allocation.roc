# Fixture for the same-allocation string equality check.
#
# Comparing a string against itself is settled by the byte pointers and the
# lengths: equal pointers with equal lengths mean equal contents, so neither
# string's bytes have to be read at all. ci/check_str_eq_same_allocation.sh
# runs this twice with different string lengths and fails if the number of
# instructions executed grows with the length.
#
# Two details are load-bearing:
#
#   - the string length comes from the command line, so the string is built at
#     runtime and is not a literal the compiler can fold
#   - both sides of the `==` come back out of a list, so they are separate
#     bindings that happen to hold the same pointer; `s == s` would risk being
#     folded to True and would prove nothing
app [main!] { pf: platform "../fx-open/platform/main.roc" }

comparisons : U64
comparisons = 4096

main! = |args| {
	# args[0] is the program path; args[1] is the length control, and every
	# byte of it stands for 4096 bytes of string.
	control = List.get(args, 1) ?? ""
	s = Str.repeat("x", Str.count_utf8_bytes(control) * 4096)
	pair = [s, s]
	a = List.get(pair, 0) ?? ""
	b = List.get(pair, 1) ?? ""

	var $hits = 0
	var $i = 0
	while $i < comparisons {
		if a == b {
			$hits = $hits + 1
		}
		$i = $i + 1
	}

	# Consumed by the exit status so the comparisons cannot be optimized away.
	if $hits == comparisons {
		Ok({})
	} else {
		Err(Exit(1))
	}
}

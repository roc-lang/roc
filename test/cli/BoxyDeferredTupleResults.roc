# Programs from issue 12231 that crashed under Boxy: a deferred erased call
# whose callee defers again, returning a tuple built by a callback; a tail
# group whose branches return a tuple from a call and from a construction;
# and a closure whose result is destructured before its descriptor is known.
call_unit : ({} -> (a, a)) -> (a, a)
call_unit = |g| g({})

expect {
	g = |_| (0, 0)
	call_unit(g) == (0, 0)
}

scan_match : U64, List(U64) -> (Bool, List(U64))
scan_match = |j, used|
	scan_match_check(List.contains(used, j), j, used)

scan_match_check : Bool, U64, List(U64) -> (Bool, List(U64))
scan_match_check = |already, j, used|
	if !already {
		(Bool.True, List.concat(used, [j]))
	} else {
		scan_match(j + 1, used)
	}

loop : U64, List(U64) -> List(U64)
loop = |i, used|
	if i >= 2 {
		used
	} else {
		(found, new_used) = scan_match(0, used)
		if found {
			loop(i + 1, new_used)
		} else {
			loop(i + 1, used)
		}
	}

expect {
	res = loop(0, [])
	List.len(res) == 2
}

expect {
	step = |f| |x| {
		(val, s) = f(x)
		(
			match val {
				Node(a, b) => Node(a, b)
			},
			s,
		)
	}
	g = step(|s| (Node(10, 0), s))
	_ = g(0)
	Bool.True
}

main! = |_args| Ok({})

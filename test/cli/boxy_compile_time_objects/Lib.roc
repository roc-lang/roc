import Util

Lib := [].{
	compute : U64 -> U64
	compute = |n| List.repeat(2.U64, n).fold(0, |a, b| a + b)

	names : List(Str)
	names = ["a", "b"]

	explode : U64 -> U64
	explode = |n| {
		dbg n
		if n > 2 {
			crash "too big"
		} else {
			n
		}
	}

	through_util : U64 -> U64
	through_util = |n| Util.fail(n)

	narrow : U8 -> U8
	narrow = |n| n + 200

	forever : U64 -> U64
	forever = |n| 1 + Lib.forever(n + 1)

	checks : U64 -> U64
	checks = |n| {
		expect n == 0
		n
	}

	pick : U64 -> U64
	pick = |n| {
		x = n + 1
		match x {
			1 => 2
			_ => crash "no such pick"
		}
	}
}

# Regression test for #12191: `walk_a` and `walk_b` tail-call each other and
# take more arguments than fit in AArch64 registers. Values the loop reloaded
# right after calling them were read from the wrong stack slot.
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || {
	input = Stdin.line!()
	n = input.count_utf8_bytes()
	var $i = 0.U64
	var $v0 = n + 0
	var $v1 = n + 1
	var $v2 = n + 2
	var $v3 = n + 3
	var $v4 = n + 4
	var $v5 = n + 5
	var $v6 = n + 6
	var $v7 = n + 7
	var $v8 = n + 8
	var $v9 = n + 9
	var $v10 = n + 10
	var $v11 = n + 11
	while $i < n {
		if $i % 5 == 3 {
			$v3 = $v4 + walk_b($i % 4, $v5, $v6, $v7, $v8, $v9, $v1, $v2, $v3, $v0)
		} else if $i % 5 == 4 {
			$v5 = $v6 + walk_a($i % 6, $v9, $v8, $v7, $v6, $v5, $v4, $v3, $v2, $v1)
		}
		if $i % 3 == 0 {
			$v0 = $v1 + walk_a($i % 5, $v0, $v1, $v2, $v3, $v4, $v5, $v6, $v7, $v8)
		} else if $i % 3 == 1 {
			$v0 = $v2 + walk_b($i % 7, $v1, $v2, $v3, $v4, $v5, $v6, $v7, $v8, $v9)
		}
		$v1 = $v2.bitwise_xor($v1) + 1
		$v2 = $v3.bitwise_xor($v2) + 2
		$v3 = $v4.bitwise_xor($v3) + 3
		$v4 = $v5.bitwise_xor($v4) + 4
		$v5 = $v6.bitwise_xor($v5) + 5
		$v6 = $v7.bitwise_xor($v6) + 6
		$v7 = $v8.bitwise_xor($v7) + 7
		$v8 = $v9.bitwise_xor($v8) + 8
		$v9 = $v10.bitwise_xor($v9) + 9
		$v10 = $v11.bitwise_xor($v10) + 10
		$v11 = $v0.bitwise_xor($v11) + 11
		$i = $i + 1
	}
	Stdout.line!("${$v0.to_str()} ${$v1.to_str()} ${$v2.to_str()} ${$v3.to_str()} ${$v4.to_str()} ${$v5.to_str()} ${$v6.to_str()} ${$v7.to_str()} ${$v8.to_str()} ${$v9.to_str()} ${$v10.to_str()} ${$v11.to_str()}")
}

walk_a : U64, U64, U64, U64, U64, U64, U64, U64, U64, U64 -> U64
walk_a = |p0, p1, p2, p3, p4, p5, p6, p7, p8, p9| {
	if p0 == 0 {
		p1.bitwise_xor(p2).bitwise_xor(p3) + p4.bitwise_and(p5) + p6.bitwise_or(p7) + p8 + p9
	} else {
		if p1 % 2 == 0 {
			walk_b(p0 - 1, p2.shl_wrap(1), p3, p4.bitwise_xor(p1), p5, p6, p7 + 1, p8, p9, p1)
		} else {
			walk_b(p0 - 1, p3, p2.shl_wrap(1), p5, p4.bitwise_and(p1), p7, p6 + 3, p9, p8, p2)
		}
	}
}

walk_b : U64, U64, U64, U64, U64, U64, U64, U64, U64, U64 -> U64
walk_b = |p0, p1, p2, p3, p4, p5, p6, p7, p8, p9| {
	if p0 == 0 {
		p1.bitwise_xor(p2).bitwise_xor(p3) + p4.bitwise_and(p5) + p6.bitwise_or(p7) + p8 + p9
	} else {
		if p1 % 2 == 0 {
			walk_a(p0 - 1, p2.shr_wrap(1), p3, p4.bitwise_xor(p1), p5, p6, p7 + 1, p8, p9, p1)
		} else {
			walk_a(p0 - 1, p3, p2.shr_wrap(1), p5, p4.bitwise_and(p1), p7, p6 + 3, p9, p8, p2)
		}
	}
}

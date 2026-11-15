app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Regression test for https://github.com/roc-lang/roc/issues/13026
# A helper whose only use of a known tag is its final `match` computes the
# same results, and performs its effects in the same order, whether its loop
# sits inline before the `match` or is factored into a function of its own.

combined! : [A, B, C], U64 => U64
combined! = |mode, n| {
	Stdout.line!("combined start ${n.to_str()}")
	var $acc = n % 1000003
	var $i = 0
	while $i < n % 7 {
		$acc = ($acc * 17 + $i) % 1000003
		$acc = ($acc * 19 + $i) % 1000003
		$acc = ($acc * 23 + $i) % 1000003
		$i = $i + 1
	}
	Stdout.line!("combined loop ${$acc.to_str()}")
	match mode {
		A => $acc + 1
		B => $acc + 2
		C => $acc + 3
	}
}

common! : Str, U64 => U64
common! = |label, n| {
	Stdout.line!("${label} start ${n.to_str()}")
	var $acc = n % 1000003
	var $i = 0
	while $i < n % 7 {
		$acc = ($acc * 17 + $i) % 1000003
		$acc = ($acc * 19 + $i) % 1000003
		$acc = ($acc * 23 + $i) % 1000003
		$i = $i + 1
	}
	Stdout.line!("${label} loop ${$acc.to_str()}")
	$acc
}

factored! : [A, B, C], U64 => U64
factored! = |mode, n| {
	acc = common!("factored", n)
	match mode {
		A => acc + 1
		B => acc + 2
		C => acc + 3
	}
}

main! = || {
	n = Stdin.line!().to_utf8().len()
	c = combined!(A, n) + combined!(B, n + 1) + combined!(C, n + 2)
	f = factored!(A, n) + factored!(B, n + 1) + factored!(C, n + 2)
	Stdout.line!("${c.to_str()} ${f.to_str()}")
}

app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Iterator pipelines over a range (`sum`, `fold`, and `map` then `fold`) each
# compute their exact mathematical total. The CLI runner's timeout keeps them
# fused into loops that cost like a hand-written loop over the same range.

sum_of_pipelines = |big| {
	var $total = 0.U64
	var $pass = 0.U64
	while $pass < 10 {
		n = big + $pass
		$total = $total + (0..<n).iter().sum()
		$pass = $pass + 1
	}
	$total
}

fold_of_pipelines = |big| {
	var $total = 0.U64
	var $pass = 0.U64
	while $pass < 10 {
		n = big + $pass
		$total = $total + (0..<n).iter().fold(0, |acc, item| acc + item)
		$pass = $pass + 1
	}
	$total
}

map_of_pipelines = |big| {
	var $total = 0.U64
	var $pass = 0.U64
	while $pass < 10 {
		n = big + $pass
		$total = $total + (0..<n).iter().map(|x| x * 2).fold(0, |acc, item| acc + item)
		$pass = $pass + 1
	}
	$total
}

main! = |args| {
	# The bound comes from the runtime argument count (just argv0 when run
	# with no arguments), so the pipelines run in the built program rather
	# than being evaluated at compile time.
	big = 99_999_999.U64 + args.len()

	# The exact expected total of the ten summed ranges 0..<(big + p), p in 0..10:
	# sum over p of (big+p)*(big+p-1)/2, computed with the same while shape.
	var $expected_sum = 0.U64
	var $ep = 0.U64
	while $ep < 10 {
		n = big + $ep
		$expected_sum = $expected_sum + (n * (n - 1)) / 2
		$ep = $ep + 1
	}
	var $expected_doubles = 0.U64
	var $dp = 0.U64
	while $dp < 10 {
		n = big + $dp
		$expected_doubles = $expected_doubles + (n * (n - 1))
		$dp = $dp + 1
	}

	summed = sum_of_pipelines(big)
	folded = fold_of_pipelines(big)
	mapped = map_of_pipelines(big)

	if summed == $expected_sum {
		if folded == $expected_sum {
			if mapped == $expected_doubles {
				Stdout.line!("all sums correct")
			} else {
				Stdout.line!("WRONG map sum: ${mapped.to_str()} expected ${$expected_doubles.to_str()}")
			}
		} else {
			Stdout.line!("WRONG fold sum: ${folded.to_str()} expected ${$expected_sum.to_str()}")
		}
	} else {
		Stdout.line!("WRONG sum: ${summed.to_str()} expected ${$expected_sum.to_str()}")
	}
	Ok({})
}

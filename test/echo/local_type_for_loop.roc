# A `for` loop over a function-body type whose `iter` method captures a
# local: directly in the declaring body, in a closure created there, and in a
# generic helper that receives the type's `iter` as evidence.

sum_all = |xs| {
	var $s = 0
	for x in xs {
		$s = $s + x
	}
	$s
}

main! = |args| {
	extra = List.len(args) + 1
	Countdown := { n : U64 }.{
		iter = |c| Iter.custom(c.n, Known(c.n), |i| if i == 0 { Err(NoMore) } else { Ok((i + extra, i - 1)) })
	}

	var $direct = 0
	for x in Countdown.{ n: 3 } {
		$direct = $direct + x
	}
	in_closure = (|| {
		var $t = 0
		for x in Countdown.{ n: 2 } {
			$t = $t + x
		}
		$t
	})()
	generic : U64
	generic = sum_all(Countdown.{ n: 4 })
	echo!("${$direct.to_str()} ${in_closure.to_str()} ${generic.to_str()}\n")
	Ok({})
}

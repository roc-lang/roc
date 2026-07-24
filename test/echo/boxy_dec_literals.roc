# Untyped numeric literals default to Dec; their runtime encoding must be
# the scaled Dec bit pattern everywhere. Mixing raw-integer and scaled
# encodings made this loop compare a scaled count against an unscaled limit
# and exit after one iteration with sum 0.
main! = |_args| {
	var $count = 0
	var $sum = 0

	while $count < 5 {
		$sum = $sum + $count
		$count = $count + 1
	}

	echo!("${$sum.to_str()}\n")
	Ok({})
}

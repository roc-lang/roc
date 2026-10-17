# A higher-order function: a request for it mentions a function type, so no
# reservation-time key names its specializations. They are served by their
# procedure identity, which an edit of the requesting app module leaves alone
# when every function the request mentions is declared here.
Lib := [].{
	apply_steps : (U64 -> U64), U64, U64 -> U64
	apply_steps = |step, start, count| {
		var $value = start
		var $remaining = count
		while $remaining > 0 {
			$value = step($value)
			$remaining = $remaining - 1
		}
		$value
	}

	double : U64 -> U64
	double = |n| n * 2

	increment : U64 -> U64
	increment = |n| n + 1
}

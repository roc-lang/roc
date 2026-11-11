import Shared

Operators := [].{

	## Double hashtag for doc comment
	number_operators : I64, I64 -> _
	number_operators = |a, b| {
		a_f64 = I64.to_f64(a)
		b_f64 = I64.to_f64(b)

		{
			# binary operators
			sum: a + b,
			diff: a - b,
			prod: a * b,
			div: a_f64 / b_f64,
			div_trunc: a // b,
			rem: a % b,
			eq: a == b,
			neq: a != b,
			lt: a < b,
			lteq: a <= b,
			gt: a > b,
			gteq: a >= b,

			# `??` to provide a default value in case of `Err`.
			default: I64.from_str("not a number") ?? 0,

			# unary operators
			neg: -a,
			total: Shared.sum([a, b]),
		}
	}

	boolean_operators : Bool, Bool -> _
	boolean_operators = |a, b| {
		bool_and_keyword: a and b,
		bool_or_keyword: a or b,
		not_a: !a,
	}

	early_return = |arg| {
		first =
			if !arg {
				return 99
			} else {
				"continue"
			}

		Str.count_utf8_bytes(first)
	}

	# A function that works for any type with a `to_str` method.
	stringify : a -> Str where [a.to_str : a -> Str]
	stringify = |value| Shared.label("value", value.to_str())
}

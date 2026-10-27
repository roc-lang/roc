# The expect converts a literal of Unit when this module is checked, so the
# module's compile-time code is cached along with its from_numeral.
Unit :: I64.{
	from_numeral : Numeral -> Try(Unit, [InvalidNumeral(Str)])
	from_numeral = |numeral| {
		var $n = 0.I64
		for digit in numeral.digits_before_pt() {
			$n = $n * 256 + digit.to_i64()
		}
		Ok(Unit.($n))
	}

	raw : Unit -> I64
	raw = |Unit.(r)| r
}

expect Unit.raw(1) == 1

Layout :: [].{
	Unit :: I64.{
		from_numeral : Numeral -> Try(Unit, [InvalidNumeral(Str)])
		from_numeral = |numeral| match I64.from_numeral(numeral) {
			Ok(value) => Ok(Unit.(value * 1000))
			Err(error) => Err(error)
		}
		raw : Unit -> I64
		raw = |Unit.(r)| r
	}
}

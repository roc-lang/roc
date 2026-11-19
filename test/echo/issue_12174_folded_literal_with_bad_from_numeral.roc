# `Celsius.from_numeral` does not have the `Numeral -> Try(..)` shape a numeral
# conversion needs. The numerals are checked in `pairs` with no expected type
# and reach `Celsius` only through `Dict.from_list(pairs)`, so their
# conversions are folded into one relation, and rejecting that relation
# rejects both numerals. Reading `readings` crashes.
Celsius := F64.{
	from_numeral : F64 -> Celsius
	from_numeral = |n| Celsius.(n)
}

readings : Dict(Str, Celsius)
readings = {
	pairs = [("inside", 21.5), ("outside", 3.0)]
	Dict.from_list(pairs)
}

main! = |_args| {
	echo!("before")
	echo!(readings.len().to_str())
	Ok({})
}

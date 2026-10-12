# `Celsius.from_numeral` does not have the `Numeral -> Try(..)` shape a numeral
# conversion needs. Both numerals reach `Celsius` through `Dict.from_list`,
# so their conversions are folded into one relation, and rejecting that
# relation rejects both numerals. Reading `readings` crashes.
Celsius := F64.{
	from_numeral : F64 -> Celsius
	from_numeral = |n| Celsius.(n)
}

readings : Dict(Str, Celsius)
readings = Dict.from_list([("inside", 21.5), ("outside", 3.0)])

main! = |_args| {
	echo!("before")
	echo!(readings.len().to_str())
	Ok({})
}

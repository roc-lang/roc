# String and numeric literals converted through `from_quote` and
# `from_numeral` methods of function-body types: written in the declaring
# body, and inside generic helpers whose literal conversion the declaring body
# selects.

conv = |_u| "abc"

num = |_u| 42

main! = |_args| {
	Name := { s : Str, n : U64 }.{
		from_quote = |s| Ok(Name.{ s, n: Str.count_utf8_bytes(s) })
	}

	Amount := { v : U64 }.{
		from_numeral : Numeral -> Try(Amount, [InvalidNumeral(Str)])
		from_numeral = |_| Ok(Amount.{ v: 11 })
	}

	quoted : Name
	quoted = "hello"
	numeral : Amount
	numeral = 7
	via_helper : Name
	via_helper = conv({})
	numeral_via_helper : Amount
	numeral_via_helper = num({})
	echo!("${quoted.s} ${quoted.n.to_str()} ${numeral.v.to_str()} ${via_helper.s} ${via_helper.n.to_str()} ${numeral_via_helper.v.to_str()}\n")
	Ok({})
}

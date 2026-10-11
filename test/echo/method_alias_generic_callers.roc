# Methods bound to local functions (`value = helper`) that read the value's
# own fields, converting literals and interpolating other requirements'
# results, called directly, through one generic caller, and through two.

call_value = |x| x.value()

twice = |x| call_value(x) + call_value(x)

call_describe = |x| x.describe()

describe_twice = |x| "${call_describe(x)}+${call_describe(x)}"

main! = |args| {
	extra = List.len(args) + 10
	shifted = |c| c.count + c.extra
	describe_shifted = |c| "cap ${(c.count + c.extra).to_str()}"
	Shifted := { count : U64, extra : U64 }.{
		value = shifted
		describe = describe_shifted
	}

	literal = |c| c.count * 3 + c.offset + 1
	Lit := { count : U64, offset : U64 }.{
		value = literal
	}

	plain = |c| c.count + 100
	describe_plain = |c| "plain ${c.count.to_str()}"
	Plain := { count : U64 }.{
		value = plain
		describe = describe_plain
	}

	cap = Shifted.{ count: 5, extra }
	lit = Lit.{ count: 2, offset: extra * 2 }
	pl = Plain.{ count: 1 }
	echo!("${cap.value().to_str()} ${lit.value().to_str()} ${pl.value().to_str()} ${cap.describe()} ${pl.describe()}\n")
	echo!("${call_value(cap).to_str()} ${call_value(lit).to_str()} ${call_value(pl).to_str()} ${call_describe(cap)} ${call_describe(pl)}\n")
	echo!("${twice(cap).to_str()} ${twice(lit).to_str()} ${twice(pl).to_str()} ${describe_twice(cap)} ${describe_twice(pl)}\n")
	echo!("${Str.inspect(pl)}\n")
	Ok({})
}

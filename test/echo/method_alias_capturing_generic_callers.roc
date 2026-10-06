# Methods bound to local functions (`value = helper`), capturing and not,
# converting literals and interpolating other requirements' results, called
# directly, through one generic caller, and through two.

call_value = |x| x.value()

twice = |x| call_value(x) + call_value(x)

call_describe = |x| x.describe()

describe_twice = |x| "${call_describe(x)}+${call_describe(x)}"

main! = |args| {
	extra = List.len(args) + 10
	capturing = |c| c.count + extra
	describe_capturing = |c| "cap ${(c.count + extra).to_str()}"
	Captured := { count : U64 }.{
		value = capturing
		describe = describe_capturing
	}

	offset = extra * 2
	literal = |c| c.count * 3 + offset + 1
	Lit := { count : U64 }.{
		value = literal
	}

	plain = |c| c.count + 100
	describe_plain = |c| "plain ${c.count.to_str()}"
	Plain := { count : U64 }.{
		value = plain
		describe = describe_plain
	}

	cap = Captured.{ count: 5 }
	lit = Lit.{ count: 2 }
	pl = Plain.{ count: 1 }
	echo!("${cap.value().to_str()} ${lit.value().to_str()} ${pl.value().to_str()} ${cap.describe()} ${pl.describe()}\n")
	echo!("${call_value(cap).to_str()} ${call_value(lit).to_str()} ${call_value(pl).to_str()} ${call_describe(cap)} ${call_describe(pl)}\n")
	echo!("${twice(cap).to_str()} ${twice(lit).to_str()} ${twice(pl).to_str()} ${describe_twice(cap)} ${describe_twice(pl)}\n")
	echo!("${Str.inspect(cap)} ${Str.inspect(pl)}\n")
	Ok({})
}

# Methods bound to helpers whose bodies convert number literals and
# interpolate, top-level and local, capturing and not, called directly and
# through a generic caller.

render_top = |c| "Top(${(c.count + 1).to_str()})"

Top := { count : U64 }.{
	describe = render_top
}

call_describe = |x| x.describe()

call_value = |x| x.value()

main! = |args| {
	literal = |c| c.count * 3 + 1
	Lit := { count : U64 }.{
		value = literal
	}

	extra = List.len(args) + 10
	describe_capturing = |c| "cap ${(c.count + extra).to_str()}"
	Captured := { count : U64 }.{
		describe = describe_capturing
	}

	plain = |c| "plain ${c.count.to_str()}"
	Plain := { count : U64 }.{
		describe = plain
	}

	t = Top.{ count: 3 }
	l = Lit.{ count: 2 }
	c = Captured.{ count: 5 }
	p = Plain.{ count: 1 }
	echo!("${t.describe()} ${l.value().to_str()} ${c.describe()} ${p.describe()}\n")
	echo!("${call_describe(t)} ${call_value(l).to_str()} ${call_describe(c)} ${call_describe(p)}\n")
	Ok({})
}

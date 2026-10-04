# repro for https://github.com/roc-lang/roc/issues/11993
#
# A method of a nominal type declared in a function body that captures a local
# of that body is passed as where-clause evidence to generic procedures: a
# top-level one, a promoted local one, and a capturing local one, including
# through a method bound to a capturing local function and through a generic
# top-level `is_eq`. Each call must use the capture of the call that declared
# the type.

get = |c| c.value()

Wrap(a) := { inner : a }.{
	is_eq = |x, y| x.inner == y.inner
}

through_top = |offset, base| {
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
	}
	get(Counter.{ count: base })
}

through_promoted = |offset, base| {
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
	}
	read = |c| c.value()
	read(Counter.{ count: base })
}

through_capturing = |offset, base| {
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
	}
	read = |c| c.value() + offset
	read(Counter.{ count: base })
}

scaled = |scale, base| {
	Scaled := { n : U64 }.{
		value = |s| s.n * scale
	}
	get(Scaled.{ n: base })
}

same = |offset, base| {
	Counter := { count : U64 }.{
		is_eq = |a, b| a.count + offset == b.count
	}
	{ c: Counter.{ count: base } } == { c: Counter.{ count: base + 1 } }
}

through_alias = |offset, base| {
	helper = |c| c.count + offset
	Counter := { count : U64 }.{
		value = helper
	}
	Counter.{ count: base }.value()
}

wrapped = |offset, base| {
	Counter := { count : U64 }.{
		is_eq = |a, b| a.count + offset == b.count
	}
	Wrap.{ inner: Counter.{ count: base } } == Wrap.{ inner: Counter.{ count: base + 1 } }
}

main! = |args| {
	base = args.len()
	echo!("${through_top(1, base).to_str()} ${through_top(2, base).to_str()}\n")
	echo!("${through_promoted(1, base).to_str()} ${through_promoted(2, base).to_str()}\n")
	echo!("${through_capturing(1, base).to_str()} ${through_capturing(2, base).to_str()}\n")
	echo!("${scaled(10, base + 2).to_str()} ${through_top(5, base).to_str()}\n")
	echo!("${Str.inspect(same(1, base))} ${Str.inspect(same(2, base))}\n")
	echo!("${through_alias(1, base).to_str()} ${through_alias(2, base).to_str()}\n")
	echo!("${Str.inspect(wrapped(1, base))} ${Str.inspect(wrapped(2, base))}\n")
	Ok({})
}

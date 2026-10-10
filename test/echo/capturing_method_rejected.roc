# A method of a type declared in a function body may not capture a value of
# that body, directly or through a local function that does. `roc check`
# reports each such method at the capture. The program still runs: declaring
# the rejected methods does nothing, other methods work, and a call reaching a
# rejected method evaluates its receiver and arguments, then crashes.

make = |offset, n| {
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
		plain = |counter| counter.count * 2
	}
	Counter.{ count: n }
}

main! = |args| {
	extra = List.len(args) + 1
	shift = |x| x + extra
	Local := { n : U64 }.{
		via = |l| shift(l.n)
	}

	c = make(1, args.len() + 3)
	local = Local.{ n: 2 }
	echo!("plain ${c.plain().to_str()} shift ${shift(local.n).to_str()}\n")
	echo!("${Str.inspect({
		dbg "receiver"
		c
	}.value())}\n")
	echo!("unreachable ${local.via().to_str()}\n")
	Ok({})
}

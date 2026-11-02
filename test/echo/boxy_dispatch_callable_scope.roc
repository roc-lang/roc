# Generic bodies dispatching directly to methods. Every call's types mention
# the enclosing body's own type variables, so each must read them in that
# body's scope.

Cell(a) :: { value : a }.{
	# Open in the error row of `f`, plus `Empty`.
	attempt = |cell, full, f| if full f(cell.value) else Err(Empty)

	# Generic in the error row `attempt` leaves open, which only the caller
	# of `forward` closes.
	forward = |cell, full, f| cell.attempt(full, f)

	# Reads the row back through a second generic layer.
	forward_twice = |cell, full, f| match cell.forward(full, f) {
		Ok(value) => Ok(value)
		Err(e) => Err(Wrapped(e))
	}
}

describe : Try(Str, [Empty, Short(U64), Long(Str)]) -> Str
describe = |result| match result {
	Ok(text) => text
	Err(Empty) => "empty"
	Err(Short(n)) => "short ${n.to_str()}"
	Err(Long(text)) => "long ${text}"
}

wrapped = |cell, full| match cell.forward_twice(full, |_| Err(Other(1.U8))) {
	Err(Wrapped(Empty)) => "wrapped empty"
	Err(Wrapped(Other(n))) => "wrapped ${n.to_str()}"
	Ok(_) => "ok"
}

local_method = |value, other| {
	Pair(a) := { left : a, right : a }.{
		right = |pair| pair.right
	}
	Pair.{ left: value, right: other }.right()
}

generic_literal = |value| {
	Counted(a) := { item : a, count : U64 }.{
		bump = |counted, by| Counted.{ item: counted.item, count: counted.count + by }
		count = |counted| counted.count
	}
	Counted.{ item: value, count: 1 }.bump(41).count()
}

main! = |_args| {
	cell = Cell.{ value: "payload that does not fit in a small string, so it is on the heap" }
	echo!(describe(cell.forward(Bool.True, |text| Ok(text))))
	echo!(describe(cell.forward(Bool.True, |text| Err(Long(text)))))
	echo!(describe(cell.forward(Bool.True, |text| Err(Short(Str.count_utf8_bytes(text))))))
	echo!(describe(cell.forward(Bool.False, |text| Ok(text))))
	echo!(wrapped(cell, Bool.False))
	echo!(wrapped(cell, Bool.True))
	echo!(local_method("left", "right"))
	echo!(local_method(1.U64, 2.U64).to_str())
	echo!(generic_literal("anything").to_str())
	Ok({})
}

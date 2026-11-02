# Generic bodies dispatching directly to methods. Every call's types mention
# the enclosing body's own type variables, so each must read them in that
# body's scope: a row variable of the target takes the caller's row, not the
# empty row of a variable no scope binds.

Cell(a) :: { value : a }.{
	attempt = |cell, full, f| {
		if !full {
			return Err(Empty)
		}
		f(cell.value)
	}

	forward : Cell(_), Bool, _ -> Try(_, _)
	forward = |cell, full, f| cell.attempt(full, f)

	unannotated = |cell, full, f| cell.attempt(full, f)
}

local_row = |value, full, f| {
	Local(a) := { item : a }.{
		attempt = |l, ok, g| {
			if !ok {
				return Err(Empty)
			}
			g(l.item)
		}
	}
	Local.{ item: value }.attempt(full, f)
}

describe : Try(Str, [Empty, Short(U64), Long(Str)]) -> Str
describe = |result| match result {
	Ok(text) => text
	Err(Empty) => "empty"
	Err(Short(n)) => "short ${n.to_str()}"
	Err(Long(text)) => "long ${text}"
}

Bag(a) :: { items : List(a) }.{
	iter = |bag| bag.items.iter()
}

total = |bag, start| {
	var $sum = start
	for item in bag {
		$sum = $sum + item
	}
	$sum
}

first_or = |items, default| {
	var $found = Err(Missing(default))
	for item in Bag.{ items } {
		$found = Ok(item)
		break
	}
	$found
}

Mark(a) := { tag : a }.{
	label = |m, prefix| "${prefix}:${m.tag.to_str()}"
}

interpolate = |value, prefix| Mark.{ tag: value }.label(prefix)

main! = |args| {
	full = List.len(args) < 9
	payload = "payload that does not fit in a small string, so it is on the heap"
	cell = Cell.{ value: payload }
	echo!(describe(cell.forward(full, |text| Err(Long(text)))))
	echo!(describe(cell.forward(!full, |text| Err(Long(text)))))
	echo!(describe(cell.unannotated(full, |text| Err(Short(Str.count_utf8_bytes(text))))))
	echo!(describe(cell.unannotated(!full, |text| Err(Short(Str.count_utf8_bytes(text))))))
	echo!(describe(local_row(payload, full, |text| Err(Long(text)))))
	echo!(describe(local_row(payload, !full, |text| Err(Long(text)))))
	echo!(total(Bag.{ items: [1.U64, 2, 3] }, 10).to_str())
	match first_or(if full ["x", "y"] else [], "none") {
		Ok(item) => echo!(item)
		Err(Missing(item)) => echo!(item)
	}
	echo!(interpolate(7.U8, "n"))
	Ok({})
}

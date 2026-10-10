Helpers := [].{
	# Stores its borrowed list in a new list, so its code updates the
	# caller's reference count.
	helper : List(U8), U8 -> U64
	helper = |list, extra| {
		wrapped = [list, list]
		wrapped.len() + extra.to_u64() + list.len()
	}

	grow : List(U8), U8 -> List(U8)
	grow = |list, extra| list.append(extra).append(extra)

	# `inner` is called at two sites of this source. A program that calls
	# only `outer` calls it once, a program that also calls `other` twice.
	outer : U64 -> U64
	outer = |n| {
		var $value = inner(n)
		while $value > 100 {
			$value = $value - 3
		}
		$value
	}

	other : U64 -> U64
	other = |n| inner(n * 2)

	# Using List.len as a value gives it a procedure of its own wherever
	# this is lowered, including this module's pack program.
	widths : List(List(U8)) -> List(U64)
	widths = |lists| lists.map(List.len)
}

inner : U64 -> U64
inner = |n| {
	var $total = 0
	var $index = 0
	while $index < n {
		$total = $total + $index
		$index = $index + 1
	}
	$total
}

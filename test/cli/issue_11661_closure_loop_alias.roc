answer : U64 -> Str
answer = |n| {
	edges = List.map_with_index(List.repeat(0, n - 1), |_, i| { from: n - 1 - i, to: n - 2 - i, cost: 1 })
	relax = |d| List.fold(
		edges,
		d,
		|acc, e| {
			via = (List.get(acc, e.to) ?? 1000) + e.cost
			if via < (List.get(acc, e.from) ?? 1000) {
				List.set(acc, e.from, via) ?? crash ("relax: out of range")
			} else {
				acc
			}
		},
	)
	start = List.set(List.repeat(1000, n), 0, 0) ?? crash ("start")
	var $d = start
	var $next = relax(start)
	var $passes = 1
	while $next != $d {
		$d = $next
		$next = relax($d)
		$passes = $passes + 1
	}
	"passes ${U64.to_str($passes)}: ${Str.join_with(List.map($d, I64.to_str), " ")}"
}

expect answer(10) == "passes 10: 0 1 2 3 4 5 6 7 8 9"

expect answer(2) == "passes 2: 0 1"
expect answer(5) == "passes 5: 0 1 2 3 4"

# A view of an old iteration must survive rebinding the loop parameter.
expect {
	var $list = List.repeat(0.U64, 4)
	var $sum = 0.U64
	var $i = 0.U64
	while $i < 4 {
		previous = $list
		$list = List.set($list, $i, $i + 1) ?? crash ("set")
		$sum = $sum + (List.get(previous, $i) ?? crash ("get"))
		$i = $i + 1
	}
	$sum == 0 and $list == [1, 2, 3, 4]
}

# Cached spare capacity must not let sibling appends overwrite each other.
expect {
	var $list = List.with_capacity(8)
	var $i = 0.U64
	var $valid = True
	while $i < 4 {
		left = List.append($list, 1.U8)
		right = List.append($list, 2.U8)
		$valid = $valid and List.last(left) == Ok(1) and List.last(right) == Ok(2)
		$list = right
		$i = $i + 1
	}
	$valid
}

Issue11322BranchEnvironments := {}

choose : [First(U64), Second(U64), Neither] -> U64
choose = |choice| {
	var $total = 10
	callback = match choice {
		First(value) => {
			$total = $total + value
			increment = if value > 2 {
				value
			} else {
				2
			}
			|extra| extra + increment
		}
		Second(value) => {
			$total = $total + 2 * value
			increment = if value > 2 {
				2 * value
			} else {
				4
			}
			|extra| extra + increment
		}
		Neither => |extra| extra
	}
	callback($total)
}

expect choose(First(3)) == 16
expect choose(Second(3)) == 22
expect choose(First(1)) == 13
expect choose(Neither) == 10

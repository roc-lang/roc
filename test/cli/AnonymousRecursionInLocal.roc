f = |n| {
	x = if n < 4 { S(n) } else { n }
	n
}

expect f(0) == 0

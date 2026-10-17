# A URL package whose exports are closed: a dev build links its optimized
# object pack in place of compiling this code itself.
Numeric := [].{
	sum_squares : U64 -> U64
	sum_squares = |n| sum_squares_from(n, 0)

	describe : U64 -> Str
	describe = |n| "sum of squares to ${n.to_str()}: ${sum_squares(n).to_str()}"
}

sum_squares_from : U64, U64 -> U64
sum_squares_from = |n, acc| if n == 0 { acc } else { sum_squares_from(n - 1, acc + n * n) }

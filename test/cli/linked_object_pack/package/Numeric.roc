# A URL package whose exports are closed: a dev build links its optimized
# object pack in place of compiling this code itself.
Numeric := [].{
	sum_squares : U64 -> U64
	sum_squares = |n| sum_squares_from(n, 0)

	describe : U64 -> Str
	describe = |n| "sum of squares to ${n.to_str()}: ${sum_squares(n).to_str()}"

	double_lanes : U8x16, U64 -> U8x16
	double_lanes = |lanes, times| if times == 0 { lanes } else { double_lanes(lanes.plus_wrap(lanes), times - 1) }

	weighted : U64 -> U64
	weighted = |n| match List.get(steps, n % List.len(steps)) { Ok(step) => step.weight, Err(_) => 0 }
}

sum_squares_from : U64, U64 -> U64
sum_squares_from = |n, acc| if n == 0 { acc } else { sum_squares_from(n - 1, acc + n * n) }

# A constant the optimized object carries as weak data. Its boxed functions
# are code pointers to procedures the object defines, though no code here
# calls them.
steps : List({ weight : U64, apply : Box(U64 -> U64) })
steps = [1, 4, 9, 16].map(|x| { weight: x * 3 + 2, apply: Box.box(|y| y * x) })

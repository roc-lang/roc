expect [1.I64, 2, 3, 4].iter().keep_oks(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err({})
	},
).collect() == [2, 4]

expect [1.I64, 2, 3, 4].iter().keep_errs(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err(n)
	},
).collect() == [1, 3]

expect [1.I64, 3].iter().keep_oks(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err({})
	},
).collect() == []

expect [2.I64, 4].iter().keep_errs(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err(n)
	},
).collect() == []

expect [1.I64, 2, 3, 4, 5, 6].iter().keep_if(|n| n > 2).keep_oks(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err({})
	},
).collect() == [4, 6]

expect [1.I64, 2, 3, 4, 5, 6].iter().keep_if(|n| n > 2).keep_errs(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err(n)
	},
).collect() == [3, 5]

expect [1.I64, 2, 3, 4, 5, 6].iter().keep_oks(
	|n| if n.is_even() {
		Ok(n)
	} else {
		Err({})
	},
).keep_errs(
	|n| if n > 4 {
		Err(n)
	} else {
		Ok(n)
	},
).collect() == [6]

ExactTupleCallChildren :: [].{}

classify = |n| if n == 0 {
	Ok("zero")
} else {
	Err("nonzero")
}

expect {
	pair = (classify(0), classify(1))

	match pair {
		(Ok(first), Err(second)) => first == "zero" and second == "nonzero"
		_ => False
	}
}

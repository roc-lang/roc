# Passing a tag that lives only in the open extension of a parameter's tag
# union must reach the worker with a descriptor that describes the actual
# value. The unrelated open-error-union function perturbs representation
# allocation so the declared-shape descriptor and the call-site value
# descriptor diverge; matching then picked the wrong branch.
question_postfix : List(Str) -> Try(I64, _)
question_postfix = |strings| {
	first_str = strings.first()?
	first_num = I64.from_str(first_str)?

	Ok(first_num)
}

color_to_str : [Red, Green, ..] -> Str
color_to_str = |color| match color {
	Red => "red"
	Green => "green"
	_ => "other color"
}

main! = |_args| {
	_ = question_postfix(["1", "not a number", "100"])
	echo!("${color_to_str(Blue)}\n")
	echo!("${color_to_str(Red)}\n")
	echo!("${color_to_str(Green)}\n")
	Ok({})
}

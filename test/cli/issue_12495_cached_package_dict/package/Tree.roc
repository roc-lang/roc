Tree := [Array(List(Str)), Other].{
	items : Tree -> Try(List(Str), [NotArray])
	items = |tree|
		match tree {
			Array(list) => Ok(list)
			_ => Err(NotArray)
		}
}

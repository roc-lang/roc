Issue10698WhileVarCapture :: [].{
	validate : List(U64) -> Try(U64, [InvalidBlock({ block : U64 })])
	validate = |items| {
		var $index = 0
		var $total = 0
		while $index < items.len() {
			value = match items.get($index) {
				Err(_) => 0
				Ok(item) => item
			}
			leading = positive_raw(value) ? |_| InvalidBlock({ block: $index })
			$total = $total + leading
			$index = $index + 1
		}
		Ok($total)
	}
}

positive_raw : U64 -> Try(U64, [NotPositive])
positive_raw = |value| if value == 0 Err(NotPositive) else Ok(value)

expect Issue10698WhileVarCapture.validate([1, 2, 3]) == Ok(6)
expect Issue10698WhileVarCapture.validate([1, 0, 3]) == Err(InvalidBlock({ block: 1 }))

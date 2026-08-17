Color := [Red, Green].{
	is_red : Color -> Bool
	is_red = |color|
		match color {
			Red => True
			Green => False
		}
}

expect Color.is_red(Color.Red)

expect !Color.is_red(Color.Green)

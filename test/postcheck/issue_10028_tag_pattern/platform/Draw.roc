Draw := [].{
	HAlign : [Left, Center, Right]

	Align : {
		horizontal : HAlign,
	}

	align_left : Align
	align_left = { horizontal: Left }

	offset : Align -> F32
	offset = |align|
		match align.horizontal {
			Left => 0
			Center => 1
			Right => 2
		}
}

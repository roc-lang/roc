# Appends one `-` byte, then the length so far.
Dash :: {}.{
	frame : U8 -> List(U8)
	frame = |n| {
		out = List.concat([n], [45])
		List.append(out, List.len(out).to_u8_wrap())
	}
}

# Appends four zero bytes, then the length so far.
Zeros :: {}.{
	frame : U8 -> List(U8)
	frame = |n| {
		out = List.concat([n], [0, 0, 0, 0])
		List.append(out, List.len(out).to_u8_wrap())
	}
}

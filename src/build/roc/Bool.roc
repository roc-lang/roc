Bool := [True, False].{
	not : Bool -> Bool
	not = |bool| match bool {
		Bool.True => Bool.False
		Bool.False => Bool.True
	}

	eq : Bool, Bool -> Bool
	eq = |a, b| match a {
		Bool.True => b
		Bool.False => Bool.not(b)
	}

	ne : Bool, Bool -> Bool
	ne = |a, b| match a {
		Bool.True => Bool.not(b)
		Bool.False => b
	}

	#encoder : Bool -> Encoder(fmt, [])
	#	where [fmt implements EncoderFormatting]
	#encoder =

	#Encoder fmt := List U8, fmt -> List U8 where fmt implements EncoderFormatting
}

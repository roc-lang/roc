Key :: { raw : Str }.{
	new : Str -> Key
	new = |raw| Key.{ raw }

	is_eq : Key, Key -> Bool
	is_eq = |left, right| ascii_lower(left.raw) == ascii_lower(right.raw)
}

ascii_lower : Str -> Str
ascii_lower = |input|
	Str.from_utf8_lossy(
		Str.to_utf8(input).map(|byte|
			if byte >= 65 and byte <= 90 { byte + 32 } else { byte }),
	)

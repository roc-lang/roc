Decoder(a) := [
	Record2(Str, Str, (Str, Str -> a)),
].{
	derive : {} -> Decoder(a) where [a.decoder : () -> Decoder(a)]
	derive = |_| {
		A : a
		A.decoder()
	}
}

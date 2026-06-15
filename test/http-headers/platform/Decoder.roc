Decoder(a) := [
	Record4(Str, Str, Str, Str, (FieldValue, FieldValue, FieldValue, FieldValue -> a)),
].{
	FieldValue := [Present(Str), Missing].{}

	derive : {} -> Decoder(a) where [a.decoder : () -> Decoder(a)]
	derive = |_| {
		A : a
		A.decoder()
	}
}

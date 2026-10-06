DerivedEncoderChart :: { data : DerivedEncoderData }.{
	encoder_for : _

	make : Str -> DerivedEncoderChart
	make = |url| { data: DerivedEncoderData.Url(url) }
}

DerivedEncoderData := [Url(Str)]

KeywordTags := [].{
	send! : U8 => Try({}, [TooLarge, Unreachable])
	resolve! : U8 => Try({}, [Defer, Other(Str)])
	payload! : U8 => [Defer, Export(U64), Struct(U64, U32)]
}

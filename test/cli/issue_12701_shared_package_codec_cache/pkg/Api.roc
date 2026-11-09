Api := [].{
	parser : {} -> (Str -> Try(a, [InvalidJson(Str)])) where [a.Json.Parseable([InvalidJson(Str)])]
	parser = |{}| |text| Json.parse(text)
}

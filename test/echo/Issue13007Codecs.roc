Issue13007Codecs :: [].{
	in_record : a -> Str where [a.Json.Encodable([])]
	in_record = |x| Json.to_str({ value: x })

	in_pair : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
	in_pair = |x, y| Json.to_str({ left: [x], right: (y, x) })

	value_of : Str -> Try(a, [InvalidJson(Str)]) where [a.Json.Parseable([InvalidJson(Str)])]
	value_of = |text| {
		parsed : Try({ value : a }, [InvalidJson(Str)])
		parsed = Json.parse(text)
		match parsed {
			Ok(record) => Ok(record.value)
			Err(e) => Err(e)
		}
	}
}

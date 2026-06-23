EncodeToMissingStrMethod :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	begin_record : U64 -> Try(U64, [])
	begin_record = |state| Ok(state)

	encode_record_field : Str, U64 -> Try(U64, [])
	encode_record_field = |_, state| Ok(state)

	end_record : U64 -> Try(U64, [])
	end_record = |state| Ok(state)
}

encode : value -> Try(U64, [])
	where [
		value.encode_to : value, Format -> (U64 -> Try(U64, [])),
	]
encode = |value| {
	encode_value = value.encode_to(Format.Default)
	encode_value(0)
}

main : Try(U64, [])
main = {
	value : { name : Str }
	value = { name: "Sam" }

	encode(value)
}

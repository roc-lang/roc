EncodeToEmptyRecordNoFieldMethods :: [].{}

Format := [Default].{
	begin_record : U64 -> Try(U64, [])
	begin_record = |state| Ok(state + 1)

	end_record : U64 -> Try(U64, [])
	end_record = |state| Ok(state + 2)
}

encode : value -> Try(U64, [])
	where [
		value.encode_to : value, Format -> (U64 -> Try(U64, [])),
	]
encode = |value| {
	encode_value = value.encode_to(Format.Default)
	encode_value(0)
}

expect encode({}) == Ok(3)

OptionalFieldsPerUse :: [].{}

# Each use of `make` owns its copy of the record `make` returns, so two uses
# can each widen it by an optional field of a different type.

make = |value| { a: value }

expect {
	record : { a ?: U8, b ?: U8 }
	record = make(11)
	(record.?a ?? 0) + (record.?b ?? 20) == 31
}

expect {
	record : { a ?: U8, b ?: Str }
	record = make(5)
	record.?b == Err(MissingField)
}

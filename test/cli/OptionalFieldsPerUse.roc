OptionalFieldsPerUse :: [].{}

# A use of `make` cannot give the record `make` built an optional field it was
# built without, whatever the field's type: each use here is a type mismatch.

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

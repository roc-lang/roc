Issue12978CallbackRecordDefaults := [].{
	Value :: U64.{
		make : U64 -> Value
		make = |n| Value.(n)

		read : Value, ReadProps -> U64
		read = |Value.(n), ReadProps.({ max_bytes })| n + max_bytes
	}

	ReadProps := { max_bytes : U64 ?? 8 }

	Ask : [Bytes(Value), Large(Value)]

	apply : a, { plan : a -> b } -> b
	apply = |value, { plan }| plan(value)

	# `ask` fixes the callback's parameter before the record is checked, so
	# `f.read({})` knows `ReadProps` when it reaches the `{}`.
	reader : Ask -> List(U64)
	reader = |ask| apply(
		ask,
		{
			plan: |a| match a {
				Bytes(f) => [f.read({})]
				Large(f) => [f.read({ max_bytes: 64 })]
			},
		},
	)
}

expect Issue12978CallbackRecordDefaults.reader(Bytes(Issue12978CallbackRecordDefaults.Value.make(1))) == [9]
expect Issue12978CallbackRecordDefaults.reader(Large(Issue12978CallbackRecordDefaults.Value.make(1))) == [65]

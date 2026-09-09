Shapes := [].{
	Zero := [Only({})]
	Nested := [None, Some(Zero)]
	TupleZero := [None, Pair({}, [Only])]
	Mixed := [Empty(Zero, [Only]), Byte(U8), Text(Str), Pair({}, Str, U8)]
	Scalar := [Empty({}), Byte(U8)]

	first_scope! : U8 => Try({}, [ScopeLimit])
	second_scope! : U8 => Try({}, [ScopeLimit])
	nested! : U8 => Nested
	tuple! : U8 => TupleZero
	mixed! : U8 => Mixed
	scalar! : U8 => Scalar

	# These aliases reuse names of payload types that must never be reserved.
	nestedPayload! : U8 => { value : U16 }
	mixedEmptyPayload! : U8 => { value : U8 }
}

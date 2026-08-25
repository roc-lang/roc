# Regression for https://github.com/roc-lang/roc/issues/10576: a generalized
# record constructor must use the field kind selected by each instantiation.
Issue10576GeneralizedOptionalFieldKind :: [].{}

make = |value| { a: value }

choose = |take_input, input| if take_input input else { a: 3 }

makeCompared = |value| { a: if value == value value else value }

expect {
	record : { a ?: U8 }
	record = make(5)

	record.?a == Ok(5)
}

expect {
	record : { a : U8 }
	record = make(7)

	record.a == 7
}

expect {
	record : { a ?: U8, b ?: U8 }
	record = make(11)

	(record.?a ?? 0) + (record.?b ?? 20) == 31
}

expect {
	input : { a : U8 }
	input = { a: 13 }
	record : { a : U8 }
	record = choose(True, input)

	record.a == 13
}

expect {
	input : { a ?: U8 }
	input = {}
	record : { a ?: U8 }
	record = choose(True, input)

	record.?a == Err(MissingField)
}

expect {
	record : { a ?: Str }
	record = make("kept")

	record.?a == Ok("kept")
}

expect {
	record : { a : Str }
	record = make("inline")

	record.a == "inline"
}

expect {
	record : { a ?: Str }
	record = makeCompared("dictionary")

	record.?a == Ok("dictionary")
}

expect {
	record : { a ?: List(U8) }
	record = make([1, 2, 3])

	record.?a == Ok([1, 2, 3])
}

expect {
	record : { a : List(U8) }
	record = make([4, 5, 6])

	record.a == [4, 5, 6]
}

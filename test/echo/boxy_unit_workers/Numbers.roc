## The same shape as `Letters`. `Empty` comes second in `widened`'s union where
## `Letters` has it first, `encoded` derives an encoder for a different record,
## and `matching` takes its equality from a generated dictionary as `Letters`'s
## does.
Numbers := {}.{
	first : List(U16) -> Try({ value : U16, rest : List(U16) }, [Empty])
	first = |items|
		match items.first() {
			Ok(value) => Ok({ value, rest: items.drop_first(1) })
			Err(_) => Err(Empty)
		}

	widened : List(U16) -> Try(U16, [Aborted(U64), Empty])
	widened = |items| {
		{ value, rest } = Numbers.first(items)?
		if rest.len() > 1 {
			Err(Aborted(rest.len()))
		} else {
			Ok(value)
		}
	}

	matching = |a, b| [a] == [b]

	same : U16, U16 -> Bool
	same = |a, b| Numbers.matching({ count: a }, { count: b })

	encoded : U16 -> Str
	encoded = |count| Json.to_str([{ count: count }])
}

## The same shape as `Numbers`. `Empty` comes first in `widened`'s union where
## `Numbers` has it second, `encoded` derives an encoder for a different record,
## and `matching` takes its equality from a generated dictionary as `Numbers`'s
## does.
Letters := {}.{
	first : List(Str) -> Try({ value : Str, rest : List(Str) }, [Empty])
	first = |items|
		match items.first() {
			Ok(value) => Ok({ value, rest: items.drop_first(1) })
			Err(_) => Err(Empty)
		}

	widened : List(Str) -> Try(Str, [Empty, TooMany(U64)])
	widened = |items| {
		{ value, rest } = Letters.first(items)?
		if rest.len() > 1 {
			Err(TooMany(rest.len()))
		} else {
			Ok(value)
		}
	}

	matching = |a, b| [a] == [b]

	same : Str, Str -> Bool
	same = |a, b| Letters.matching({ name: a }, { name: b })

	encoded : Str -> Str
	encoded = |name| Json.to_str([{ name: name }])
}

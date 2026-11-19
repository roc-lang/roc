# A `Box` of a type without values has no values either, so a `match` on a
# `Try` whose error is such a box needs no `Err` branch, at the top level and
# inside a record and a list. The values come from the argument list so no
# stage can fold them away.
describe : Try(Str, Box([])) -> Str
describe = |t| match t {
    Ok(s) => s
}

describe_field : { result : Try(Str, Box({ a : [], b : Str })) } -> Str
describe_field = |r| match r.result {
    Ok(s) => s
}

main! = |args| {
	word = if List.len(args) > 100 "never" else "boxed"
	results : List(Try(Str, Box([])))
	results = [Ok(word), Ok("${word}!")]
	joined = Str.join_with(results.map(describe), " ")
	field = describe_field({ result: Ok(word) })
	echo!("${joined} ${field}\n")
	Ok({})
}

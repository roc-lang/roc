# One interface summary serves every instantiation of a variable its template
# relates only by unification, while a variable the template dispatches on
# keeps a summary per instantiation.
app [main!] {}

Celsius := { degrees : I64 }.{
	to_text : Celsius -> Str
	to_text = |celsius| "${celsius.degrees.to_str()}C"
}

Label := { name : Str }.{
	to_text : Label -> Str
	to_text = |label| "<${label.name}>"
}

count_items : List(a) -> U64
count_items = |items| items.len()

first_or : List(a), a -> a
first_or = |items, fallback| match List.first(items) {
	Ok(item) => item
	Err(_) => fallback
}

describe_first : List(a), a -> Str where [a.to_text : a -> Str]
describe_first = |items, fallback| first_or(items, fallback).to_text()

main! = |_| {
	counts = [count_items([1.U8, 2.U8]), count_items(["a", "b", "c"]), count_items([{ x: 1.I64 }])]
	echo!("${Str.inspect(counts)}\n")
	echo!("${first_or(["first", "second"], "none")} ${first_or([], 7.I64).to_str()}\n")
	echo!("${describe_first([Celsius.{ degrees: 21 }], Celsius.{ degrees: 0 })} ${describe_first([], Label.{ name: "empty" })}\n")
	Ok({})
}

# A top-level value keeps the part of its type that nothing pins, and
# `Str.inspect`, string interpolation and `dbg` render it.
Wrap(a) := [W(List(a))].{
	to_inspect : Wrap(a) -> Str
	to_inspect = |Wrap.(W(items))| "Wrap<${Str.inspect(items)}>"
}

Phantom(a) := [P(U64)]

empty = []

ok_empty = Ok([])

pair = ([], Ok([]))

rec = { items: [] }

first = List.first([])

dict = Dict.empty()

set = Set.empty()

wrapped = Wrap.W([])

phantom = Phantom.P(1)

shown = Str.inspect(pair)

main! = |_args| {
	echo!(Str.inspect(empty))
	echo!(Str.inspect(ok_empty))
	echo!(Str.inspect(pair))
	echo!(Str.inspect(rec))
	echo!(Str.inspect(first))
	echo!(Str.inspect(dict))
	echo!(Str.inspect(set))
	echo!(Str.inspect(wrapped))
	echo!(Str.inspect(phantom))
	echo!("${Str.inspect(empty)} and ${Str.inspect(wrapped)}")
	echo!(shown)
	dbg empty
	dbg pair
	dbg wrapped
	Ok({})
}

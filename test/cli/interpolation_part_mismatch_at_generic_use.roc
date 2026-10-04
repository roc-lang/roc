# A generic body's interpolation is valid for a result type whose
# `from_interpolation` accepts its parts. Its `Str` instantiation rejects the
# `U8` part at that use alone, so the `Bytes` use still runs.
Bytes := [Bytes(List(U8))].{
	from_interpolation : Str, Iter((U8, Str)) -> Bytes
	from_interpolation = |_first, rest| Bytes.Bytes(rest.fold([], |acc, (b, _segment)| acc.append(b)))
	count : Bytes -> U64
	count = |Bytes.Bytes(list)| list.len()
}

render = |{}| {
	c = 7.U8
	"${c}"
}

main! = |_args| {
	b : Bytes
	b = render({})
	echo!(b.count().to_str())
	s : Str
	s = render({})
	echo!(s)
	Ok({})
}

# A generic body's interpolation is valid for a result type whose
# `from_interpolation` accepts its parts. Its `Str` instantiation rejects the
# `U8` part at that use alone, so the `Bytes` use still runs.
Bytes := [Bytes(List(U8))].{
	from_interpolation : List(Str) -> Try((List(U8) -> Bytes), [InvalidInterpolation(Str)])
	from_interpolation = |_segments| Ok(|values| Bytes.Bytes(values))
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

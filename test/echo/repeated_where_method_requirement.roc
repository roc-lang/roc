# A where clause listing the same method twice declares one requirement, so
# `get` is as polymorphic as with a single `a.info`.
Item := [Item(U64)].{
	info : Item -> U64
	info = |Item(n)| n
}

Pair := [Pair(U64, U64)].{
	info : Pair -> U64
	info = |Pair(a, b)| a + b
}

get : a -> U64 where [a.info : a -> U64, a.info : a -> U64]
get = |item| item.info()

main! = |_args| {
	echo!((get(Item.Item(3)) + get(Pair.Pair(1, 2))).to_str())
	Ok({})
}

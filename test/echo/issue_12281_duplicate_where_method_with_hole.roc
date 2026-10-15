# `check_item` and `inspect_item` call each other, so `check_item` uses
# `inspect_item`'s annotation before its body is checked. The two `info`
# requirements relate, which decides the `_` from the annotation itself.
Item := [Item(U64)].{
	info : Item -> { count : U64 }
	info = |Item(n)| { count: n }

	next : Item -> Item
	next = |Item(n)| Item(n - 1)
}

check_item = |item| inspect_item(item)

inspect_item : a -> U64 where [a.info : a -> _, a.info : a -> { count : U64 }, a.next : a -> a]
inspect_item = |item| {
	count = item.info().count
	if count == 0 0 else count + check_item(item.next())
}

main! = |_args| {
	echo!(inspect_item(Item.Item(3)).to_str())
	Ok({})
}

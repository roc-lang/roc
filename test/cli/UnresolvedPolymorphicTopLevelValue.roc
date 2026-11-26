UnresolvedPolymorphicTopLevelValue :: [].{}

# The item type of `empty` carries a `to_str` requirement, so `empty` is a
# weak value, and nothing fixes that type. Checking reports this value's
# unresolved item type, and the tests still run with the item type at its
# default.
empty = {
	items = []
	_ = List.map(items, |item| item.to_str())
	items
}

expect empty == []

expect List.len(empty) == 0

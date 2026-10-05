UnresolvedPolymorphicTopLevelValue :: [].{}

# Checking reports this value's unresolved item type, and the tests still run
# with the item type at its default.
empty = []

expect empty == []

expect List.len(empty) == 0

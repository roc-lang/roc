# Comparing a zero-payload tag only needs the discriminant. The other variant
# contains Box(U64), which deliberately has no is_eq method.

is_none : [None, Some(Box(U64))] -> Bool
is_none = |value| value == None

is_some : [None, Some(Box(U64))] -> Bool
is_some = |value| value != None

expect is_none(None)
expect !is_none(Some(Box.box(7)))
expect !is_some(None)
expect is_some(Some(Box.box(7)))

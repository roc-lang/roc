# A full union comparison must still require equality for its Box payload.

equal : [None, Some(Box(U64))], [None, Some(Box(U64))] -> Bool
equal = |lhs, rhs| lhs == rhs

main! = |_args| Ok({})

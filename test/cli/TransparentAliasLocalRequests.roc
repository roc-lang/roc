TransparentAliasLocalRequests :: [].{
	Pair : { left : U64, right : U64 }

	structural_total : { left : U64, right : U64 } -> U64
	structural_total = |pair| pair.left + pair.right

	total : Pair -> U64
	total = |pair| pair.left + structural_total(pair)
}

expect TransparentAliasLocalRequests.total({ left: 1, right: 2 }) == 4

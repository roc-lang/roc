Abi :: [].{
	Machine := [Machine(Box(U64 => Step))]

	Step := [End, Emit({ machine : Machine, observed : U64 })]
}

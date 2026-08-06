Abi :: [].{
	SourceMachine := [SourceMachine(Box(U64 => SourceStep))]

	SourceStep := [End, Emit({ machine : SourceMachine, sequence : U64 })]

	SourceOutcome := [Response(U16), Stream(SourceMachine)]
}

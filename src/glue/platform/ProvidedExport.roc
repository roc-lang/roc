import FunctionSignature exposing [FunctionSignature]

## What a provided symbol exports. A procedure is exported as a function with
## the natural C ABI of its signature; data is exported as a value of the type
## with this id.
ProvidedExport := [
	ProvidedData(U64),
	ProvidedProcedure(FunctionSignature),
]

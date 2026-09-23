import FunctionSignature exposing [FunctionSignature]

## What glue knows about a stored function value's signature. `Known` names
## the argument and result type ids a host uses to fill the callable's
## argument buffer and read its result buffer. `Opaque` means some argument
## or result has no standalone committed layout, so the host can only store
## the callable and hand it back to Roc.
CallableSignature := [
	Known(FunctionSignature),
	Opaque,
]

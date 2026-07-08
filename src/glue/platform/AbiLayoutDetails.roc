import AbiRecordLayout exposing [AbiRecordLayout]
import AbiTagUnionLayout exposing [AbiTagUnionLayout]

## Shape-specific ABI metadata emitted by src/glue/glue.zig from LIR layouts.
AbiLayoutDetails := [
	AbiBuiltin,
	AbiRecord(AbiRecordLayout),
	AbiTagUnion(AbiTagUnionLayout),
]

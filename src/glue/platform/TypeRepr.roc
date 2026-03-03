import FunctionRepr exposing [FunctionRepr]
import RecordRepr exposing [RecordRepr]
import TagUnionRepr exposing [TagUnionRepr]

## Structured, language-agnostic representation of a Roc type for glue code generation.
## All naming uses Roc conventions with "Roc" prefix to avoid conflicts with builtin type names.
## Uses U64 indices into Types.type_table for type references.
TypeRepr := [
    RocBool,
    RocDec,
    RocF32,
    RocF64,
    RocFunction(FunctionRepr),
    RocI128,
    RocI16,
    RocI32,
    RocI64,
    RocI8,
    RocList(U64),
    RocRecord(RecordRepr),
    RocStr,
    RocTagUnion(TagUnionRepr),
    RocU128,
    RocU16,
    RocU32,
    RocU64,
    RocU8,
    RocUnit,
    RocUnknown(Str),
]

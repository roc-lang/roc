import AbiFieldLayout exposing [AbiFieldLayout]

## Exact committed layout for one tag-union variant payload.
##
## Source of truth: src/layout/store.zig `TagUnionVariant`/`TagUnionData`,
## reached through src/lir/program.zig `RequestedLayout`.
AbiTagLayout := {
	discriminant : U64,
	name : Str,
	payload : List(U64),
	payload_fields : List(AbiFieldLayout),
	payload_alignment32 : U64,
	payload_alignment64 : U64,
	payload_size32 : U64,
	payload_size64 : U64,
}

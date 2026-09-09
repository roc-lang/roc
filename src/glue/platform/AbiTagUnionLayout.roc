import AbiTagLayout exposing [AbiTagLayout]

## Exact committed tag-union layout details from src/layout/store.zig.
AbiTagUnionLayout := {
	discriminant_offset32 : U64,
	discriminant_offset64 : U64,
	discriminant_size : U64,

	## Whether any variant occupies payload storage on either pointer width.
	has_payload : Bool,
	tags : List(AbiTagLayout),
}

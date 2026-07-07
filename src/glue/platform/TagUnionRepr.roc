import TagVariant exposing [TagVariant]

TagUnionRepr := {
	alignment : U64,
	alignment_32 : U64,
	alignment_64 : U64,
	discriminant_offset_32 : U64,
	discriminant_offset_64 : U64,
	discriminant_size : U64,
	name : Str,
	size : U64,
	size_32 : U64,
	size_64 : U64,
	tags : List(TagVariant),
}

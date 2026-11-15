# An opaque type for `JoinedRowRepeatedLabelOpaque.roc`: outside this module
# its backing fixes only its runtime layout.
JoinedRowRepeatedLabelOpaqueId :: U64.{
	from_u64 : U64 -> JoinedRowRepeatedLabelOpaqueId
	from_u64 = |n| JoinedRowRepeatedLabelOpaqueId.(n)

	to_u64 : JoinedRowRepeatedLabelOpaqueId -> U64
	to_u64 = |JoinedRowRepeatedLabelOpaqueId.(n)| n
}

## Exact committed layout for one ABI-visible record field.
##
## Source of truth: src/layout/store.zig `StructField`/`StructData`, reached
## through src/lir/program.zig `RequestedLayout` for this glue type id.
AbiFieldLayout := {
	alignment32 : U64,
	alignment64 : U64,
	is_padding : Bool,
	name : Str,
	offset32 : U64,
	offset64 : U64,
	original_index : U64,
	size32 : U64,
	size64 : U64,
	type_id : U64,
}.{
	target_offset : AbiFieldLayout, Bool -> U64
	target_offset = |field, is_wasm32| if is_wasm32 { field.offset32 } else { field.offset64 }

	compare_u64 : U64, U64 -> [LT, EQ, GT]
	compare_u64 = |left, right| if left < right { LT } else if left > right { GT } else { EQ }

	## Sort fields into committed target layout order.
	##
	## Source of truth: src/layout/store.zig `getStructFieldOffsetByOriginalIndexAt`
	## exposes the offset selected for the concrete target width. Glue emitters
	## must use these offsets directly; source/declaration order is not an ABI fact.
	sort_by_target_offset : List(AbiFieldLayout), Bool -> List(AbiFieldLayout)
	sort_by_target_offset = |fields, is_wasm32|
		List.sort_with(
			fields,
			|left, right| {
				offset_order = compare_u64(target_offset(left, is_wasm32), target_offset(right, is_wasm32))
				match offset_order {
					EQ => compare_u64(left.original_index, right.original_index)
					_ => offset_order
				}
			},
		)
}

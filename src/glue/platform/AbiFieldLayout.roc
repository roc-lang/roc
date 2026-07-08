import AbiWidth exposing [AbiWidth]

## Exact committed layout for one ABI-visible record field.
##
## Field lists (`AbiRecordLayout.fields`, `AbiTagLayout.payload_fields`) are
## emitted in committed layout order, which is identical at both pointer
## widths with non-decreasing offsets at both widths (asserted by
## src/glue/glue.zig). Emitters must iterate fields in the given order and
## must not re-sort them. Padding fields have nonzero size at both widths.
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
	offset : AbiFieldLayout, AbiWidth -> U64
	offset = |field, width|
		match width {
			Pointer32 => field.offset32
			Pointer64 => field.offset64
		}

	size : AbiFieldLayout, AbiWidth -> U64
	size = |field, width|
		match width {
			Pointer32 => field.size32
			Pointer64 => field.size64
		}

	alignment : AbiFieldLayout, AbiWidth -> U64
	alignment = |field, width|
		match width {
			Pointer32 => field.alignment32
			Pointer64 => field.alignment64
		}
}

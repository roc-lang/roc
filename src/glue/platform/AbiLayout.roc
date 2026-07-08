import AbiLayoutDetails exposing [AbiLayoutDetails]
import AbiFieldLayout exposing [AbiFieldLayout]
import AbiTagLayout exposing [AbiTagLayout]
import AbiWidth exposing [AbiWidth]

## Exact compiler-emitted ABI layout metadata for one public glue type id.
##
## Every fact is carried for both pointer widths ([AbiWidth]); nothing here
## depends on OS, architecture, or a concrete Roc build target.
##
## Authoritative compiler sources:
## - src/lir/program.zig `RequestedLayout` maps checked type ids to committed
##   layout ids after post-check lowering.
## - src/layout/store.zig owns size, alignment, field order, offsets, tag
##   payload layouts, discriminant offsets, and refcountedness.
## - design.md Layout Selection requires glue to consume these emitted facts
##   instead of reconstructing target layout from reflected type shape.
AbiLayout := {
	alignment32 : U64,
	alignment64 : U64,
	contains_refcounted : Bool,
	details : AbiLayoutDetails,
	size32 : U64,
	size64 : U64,
}.{
	size : AbiLayout, AbiWidth -> U64
	size = |layout, width|
		match width {
			Pointer32 => layout.size32
			Pointer64 => layout.size64
		}

	alignment : AbiLayout, AbiWidth -> U64
	alignment = |layout, width|
		match width {
			Pointer32 => layout.alignment32
			Pointer64 => layout.alignment64
		}

	record_fields : AbiLayout -> List(AbiFieldLayout)
	record_fields = |layout|
		match layout.details {
			AbiRecord(record) => record.fields
			_ => {
				crash "glue invariant violated: expected record ABI layout"
			}
		}

	tag_layouts : AbiLayout -> List(AbiTagLayout)
	tag_layouts = |layout|
		match layout.details {
			AbiTagUnion(tag_union) => tag_union.tags
			_ => {
				crash "glue invariant violated: expected tag union ABI layout"
			}
		}

	discriminant_offset : AbiLayout, AbiWidth -> U64
	discriminant_offset = |layout, width|
		match layout.details {
			AbiTagUnion(tag_union) =>
				match width {
					Pointer32 => tag_union.discriminant_offset32
					Pointer64 => tag_union.discriminant_offset64
				}
			_ => {
				crash "glue invariant violated: expected tag union ABI layout"
			}
		}

	discriminant_size : AbiLayout -> U64
	discriminant_size = |layout|
		match layout.details {
			AbiTagUnion(tag_union) => tag_union.discriminant_size
			_ => {
				crash "glue invariant violated: expected tag union ABI layout"
			}
		}
}

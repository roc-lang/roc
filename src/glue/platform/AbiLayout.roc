import AbiLayoutDetails exposing [AbiLayoutDetails]
import AbiFieldLayout exposing [AbiFieldLayout]
import AbiTagLayout exposing [AbiTagLayout]

## Exact compiler-emitted ABI layout metadata for one public glue type id.
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
	Layout := { alignment : U64, size : U64 }

	native64 : AbiLayout -> Layout
	native64 = |layout| { alignment: layout.alignment64, size: layout.size64 }

	wasm32 : AbiLayout -> Layout
	wasm32 = |layout| { alignment: layout.alignment32, size: layout.size32 }

	record_fields : AbiLayout -> List(AbiFieldLayout)
	record_fields = |layout|
		match layout.details {
			AbiRecord(record) => record.fields
			_ => []
		}

	tag_layouts : AbiLayout -> List(AbiTagLayout)
	tag_layouts = |layout|
		match layout.details {
			AbiTagUnion(tag_union) => tag_union.tags
			_ => []
		}

	discriminant_offset64 : AbiLayout -> U64
	discriminant_offset64 = |layout|
		match layout.details {
			AbiTagUnion(tag_union) => tag_union.discriminant_offset64
			_ => 0
		}

	discriminant_offset32 : AbiLayout -> U64
	discriminant_offset32 = |layout|
		match layout.details {
			AbiTagUnion(tag_union) => tag_union.discriminant_offset32
			_ => 0
		}
}

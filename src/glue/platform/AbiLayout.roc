import RecordField exposing [RecordField]
import TagUnionRepr exposing [TagUnionRepr]
import TypeRepr exposing [TypeRepr]
import TypeTable exposing [TypeTable]

## Target ABI layout calculations shared by glue generators.
##
## This module returns plain layout data. Target generators remain responsible
## for rendering language-specific declarations and assertions.
##
## Authoritative compiler sources:
## - src/layout/field_order.zig owns target-independent ABI field order.
## - src/layout/store.zig commits runtime offsets and layouts.
## - src/glue/glue.zig reflects RecordField, TagUnionRepr, and TypeRepr data for
##   glue scripts to consume.
##
## Fields passed to these helpers are already in ABI memory order. This module
## only derives pointer-width-dependent sizes, alignments, and tag discriminant
## offsets needed by generated glue assertions. The long-term replacement is
## compiler-emitted target layout metadata queried directly by glue.
AbiLayout := {}.{
	Layout := { alignment : U64, size : U64 }

	TagUnionLayout := { alignment : U64, discriminant_offset : U64, size : U64 }

	align_forward_u64 : U64, U64 -> U64
	align_forward_u64 = |offset, alignment| {
		if alignment == 0 {
			offset
		} else {
			remainder = U64.rem_by(offset, alignment)
			if remainder == 0 {
				offset
			} else {
				offset + alignment - remainder
			}
		}
	}

	type_layout_64 : List(TypeRepr), U64 -> Layout
	type_layout_64 = |type_table, type_id| {
		type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
		repr_layout_64(type_table, type_repr)
	}

	repr_layout_64 : List(TypeRepr), TypeRepr -> Layout
	repr_layout_64 = |type_table, type_repr| {
		match type_repr {
			RocBool => { size: 1, alignment: 1 }
			RocBox(_) => { size: 8, alignment: 8 }
			RocDec => { size: 8, alignment: 8 }
			RocF32 => { size: 4, alignment: 4 }
			RocF64 => { size: 8, alignment: 8 }
			RocFunction(_) => { size: 8, alignment: 8 }
			RocI128 => { size: 16, alignment: 16 }
			RocI16 => { size: 2, alignment: 2 }
			RocI32 => { size: 4, alignment: 4 }
			RocI64 => { size: 8, alignment: 8 }
			RocI8 => { size: 1, alignment: 1 }
			RocList(_) => { size: 24, alignment: 8 }
			RocRecord(rec) => record_layout_from_fields(type_table, rec.fields)
			RocStr => { size: 24, alignment: 8 }
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) => type_layout_64(type_table, payload_id)
					SingleNoPayload => { size: 0, alignment: 1 }
					NotSingleVariant => {
						layout = tag_union_layout_64(type_table, tu)
						{ size: layout.size, alignment: layout.alignment }
					}
				}
			RocU128 => { size: 16, alignment: 16 }
			RocU16 => { size: 2, alignment: 2 }
			RocU32 => { size: 4, alignment: 4 }
			RocU64 => { size: 8, alignment: 8 }
			RocU8 => { size: 1, alignment: 1 }
			RocUnit => { size: 0, alignment: 1 }
			RocUnknown(_) => { size: 8, alignment: 8 }
		}
	}

	type_layout_32 : List(TypeRepr), U64 -> Layout
	type_layout_32 = |type_table, type_id| {
		type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
		repr_layout_32(type_table, type_repr)
	}

	repr_layout_32 : List(TypeRepr), TypeRepr -> Layout
	repr_layout_32 = |type_table, type_repr| {
		match type_repr {
			RocBool => { size: 1, alignment: 1 }
			RocBox(_) => { size: 4, alignment: 4 }
			RocDec => { size: 8, alignment: 8 }
			RocF32 => { size: 4, alignment: 4 }
			RocF64 => { size: 8, alignment: 8 }
			RocFunction(_) => { size: 4, alignment: 4 }
			RocI128 => { size: 16, alignment: 16 }
			RocI16 => { size: 2, alignment: 2 }
			RocI32 => { size: 4, alignment: 4 }
			RocI64 => { size: 8, alignment: 8 }
			RocI8 => { size: 1, alignment: 1 }
			RocList(_) => { size: 12, alignment: 4 }
			RocRecord(rec) => record_layout_from_fields_32(type_table, rec.fields)
			RocStr => { size: 12, alignment: 4 }
			RocTagUnion(tu) => {
				layout = tag_union_layout_32(type_table, tu)
				{ size: layout.size, alignment: layout.alignment }
			}
			RocU128 => { size: 16, alignment: 16 }
			RocU16 => { size: 2, alignment: 2 }
			RocU32 => { size: 4, alignment: 4 }
			RocU64 => { size: 8, alignment: 8 }
			RocU8 => { size: 1, alignment: 1 }
			RocUnit => { size: 0, alignment: 1 }
			RocUnknown(_) => { size: 4, alignment: 4 }
		}
	}

	record_layout_from_fields : List(TypeRepr), List(RecordField) -> Layout
	record_layout_from_fields = |type_table, fields| {
		var $offset = 0
		var $alignment = 1

		for field in fields {
			field_layout = record_field_layout_64(type_table, field)
			if field_layout.alignment > $alignment {
				$alignment = field_layout.alignment
			}
			$offset = align_forward_u64($offset, field_layout.alignment)
			$offset = $offset + field_layout.size
		}

		{ size: align_forward_u64($offset, $alignment), alignment: $alignment }
	}

	record_field_layout_64 : List(TypeRepr), RecordField -> Layout
	record_field_layout_64 = |type_table, field| {
		if field.is_padding {
			{ size: field.size, alignment: 1 }
		} else {
			type_layout_64(type_table, field.type_id)
		}
	}

	record_layout_from_fields_32 : List(TypeRepr), List(RecordField) -> Layout
	record_layout_from_fields_32 = |type_table, fields| {
		var $offset = 0
		var $alignment = 1

		for field in fields {
			field_layout = record_field_layout_32(type_table, field)
			if field_layout.alignment > $alignment {
				$alignment = field_layout.alignment
			}
			$offset = align_forward_u64($offset, field_layout.alignment)
			$offset = $offset + field_layout.size
		}

		{ size: align_forward_u64($offset, $alignment), alignment: $alignment }
	}

	record_field_layout_32 : List(TypeRepr), RecordField -> Layout
	record_field_layout_32 = |type_table, field| {
		if field.is_padding {
			{ size: field.size, alignment: 1 }
		} else {
			type_layout_32(type_table, field.type_id)
		}
	}

	tag_union_layout_64 : List(TypeRepr), TagUnionRepr -> TagUnionLayout
	tag_union_layout_64 = |type_table, tu| {
		match TypeTable.single_variant_payload(tu) {
			SinglePayload(payload_id) => {
				payload_layout = type_layout_64(type_table, payload_id)
				{ size: payload_layout.size, alignment: payload_layout.alignment, discriminant_offset: payload_layout.size }
			}
			SingleNoPayload => { size: 0, alignment: 1, discriminant_offset: 0 }
			NotSingleVariant => tag_union_layout_from_payloads(type_table, tu, Bool.False)
		}
	}

	tag_union_layout_32 : List(TypeRepr), TagUnionRepr -> TagUnionLayout
	tag_union_layout_32 = |type_table, tu| {
		match TypeTable.single_variant_payload(tu) {
			SinglePayload(payload_id) => {
				payload_layout = type_layout_32(type_table, payload_id)
				{ size: payload_layout.size, alignment: payload_layout.alignment, discriminant_offset: payload_layout.size }
			}
			SingleNoPayload => { size: 0, alignment: 1, discriminant_offset: 0 }
			NotSingleVariant => tag_union_layout_from_payloads(type_table, tu, Bool.True)
		}
	}

	tag_union_layout_from_payloads : List(TypeRepr), TagUnionRepr, Bool -> TagUnionLayout
	tag_union_layout_from_payloads = |type_table, tu, wasm32| {
		var $max_payload_size = 0
		var $max_payload_alignment = 1

		for tag in tu.tags {
			payload_layout = if tag.payload_size == 0 {
				{ size: 0, alignment: 1 }
			} else if wasm32 {
				tag_payload_layout_32(type_table, tag.payload)
			} else {
				{ size: tag.payload_size, alignment: tag.payload_alignment }
			}
			if payload_layout.size > $max_payload_size {
				$max_payload_size = payload_layout.size
			}
			if payload_layout.alignment > $max_payload_alignment {
				$max_payload_alignment = payload_layout.alignment
			}
		}

		disc = disc_layout_for_count(List.len(tu.tags))
		disc_offset = align_forward_u64($max_payload_size, disc.alignment)
		total_alignment = if $max_payload_alignment > disc.alignment {
			$max_payload_alignment
		} else {
			disc.alignment
		}
		total_size = align_forward_u64(disc_offset + disc.size, total_alignment)
		{ size: total_size, alignment: total_alignment, discriminant_offset: disc_offset }
	}

	tag_payload_layout_64 : List(TypeRepr), List(U64) -> Layout
	tag_payload_layout_64 = |type_table, payload| {
		var $offset = 0
		var $alignment = 1

		for payload_id in payload {
			payload_layout = type_layout_64(type_table, payload_id)
			if payload_layout.alignment > $alignment {
				$alignment = payload_layout.alignment
			}
			$offset = align_forward_u64($offset, payload_layout.alignment)
			$offset = $offset + payload_layout.size
		}

		{ size: align_forward_u64($offset, $alignment), alignment: $alignment }
	}

	tag_payload_layout_32 : List(TypeRepr), List(U64) -> Layout
	tag_payload_layout_32 = |type_table, payload| {
		var $offset = 0
		var $alignment = 1

		for payload_id in payload {
			payload_layout = type_layout_32(type_table, payload_id)
			if payload_layout.alignment > $alignment {
				$alignment = payload_layout.alignment
			}
			$offset = align_forward_u64($offset, payload_layout.alignment)
			$offset = $offset + payload_layout.size
		}

		{ size: align_forward_u64($offset, $alignment), alignment: $alignment }
	}

	disc_layout_for_count : U64 -> Layout
	disc_layout_for_count = |count| {
		if count <= 1 {
			{ size: 0, alignment: 1 }
		} else if count <= 256 {
			{ size: 1, alignment: 1 }
		} else if count <= 65536 {
			{ size: 2, alignment: 2 }
		} else {
			{ size: 4, alignment: 4 }
		}
	}
}

expect AbiLayout.align_forward_u64(9, 8) == 16
expect AbiLayout.disc_layout_for_count(256) == { size: 1, alignment: 1 }
expect AbiLayout.disc_layout_for_count(257) == { size: 2, alignment: 2 }

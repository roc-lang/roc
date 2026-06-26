import RecordField exposing [RecordField]
import TagUnionRepr exposing [TagUnionRepr]
import TypeRepr exposing [TypeRepr]

## Typed view over the reflected glue type table.
##
## Glue scripts should ask ABI-shape questions through this module instead of
## re-deriving them independently per target language.
TypeTable := { entries : List(TypeRepr) }.{
	RecordLookup := [
		RecordFound({ fields : List(RecordField), size : U64, alignment : U64 }),
		NotRecord,
	]

	SingleVariantPayload := [
		SinglePayload(U64),
		SingleNoPayload,
		NotSingleVariant,
	]

	from_list : List(TypeRepr) -> TypeTable
	from_list = |entries| TypeTable.{ entries }

	entries : TypeTable -> List(TypeRepr)
	entries = |{ entries }| entries

	get : TypeTable, U64 -> TypeRepr
	get = |table, type_id|
		match List.get(entries(table), type_id) {
			Ok(type_repr) => type_repr
			Err(_) => {
				crash "glue invariant violated: missing type table entry ${U64.to_str(type_id)}"
			}
		}

	is_unit : TypeTable, U64 -> Bool
	is_unit = |table, type_id|
		match get(table, type_id) {
			RocUnit => Bool.True
			_ => Bool.False
		}

	is_refcounted : TypeTable, U64 -> Bool
	is_refcounted = |table, type_id| repr_is_refcounted(table, get(table, type_id))

	repr_is_refcounted : TypeTable, TypeRepr -> Bool
	repr_is_refcounted = |table, type_repr|
		match type_repr {
			RocStr => Bool.True
			RocBox(_) => Bool.True
			RocList(_) => Bool.True
			RocFunction(_) => Bool.True
			RocRecord(rec) => List.any(rec.fields, |field| !field.is_padding and is_refcounted(table, field.type_id))
			RocTagUnion(tu) => List.any(tu.tags, |tag| List.any(tag.payload, |payload_id| is_refcounted(table, payload_id)))
			_ => Bool.False
		}

	single_variant_payload : TagUnionRepr -> SingleVariantPayload
	single_variant_payload = |tu|
		if List.len(tu.tags) == 1 {
			match List.first(tu.tags) {
				Ok(tag) =>
					match List.first(tag.payload) {
						Ok(payload_id) => SinglePayload(payload_id)
						Err(_) => SingleNoPayload
					}
				Err(_) => SingleNoPayload
			}
		} else {
			NotSingleVariant
		}

	record_layout : TypeTable, U64 -> RecordLookup
	record_layout = |table, type_id| record_layout_from_repr(table, get(table, type_id))

	record_layout_from_repr : TypeTable, TypeRepr -> RecordLookup
	record_layout_from_repr = |table, type_repr|
		match type_repr {
			RocRecord(rec) =>
				if List.len(rec.fields) > 0 {
					RecordFound({ fields: rec.fields, size: rec.size, alignment: rec.alignment })
				} else {
					NotRecord
				}
			RocTagUnion(tu) =>
				match single_variant_payload(tu) {
					SinglePayload(payload_id) => record_layout(table, payload_id)
					_ => NotRecord
				}
			_ => NotRecord
		}

	is_anonymous_record : TypeTable, U64 -> Bool
	is_anonymous_record = |table, type_id| is_anonymous_record_repr(table, get(table, type_id))

	is_anonymous_record_repr : TypeTable, TypeRepr -> Bool
	is_anonymous_record_repr = |table, type_repr|
		match type_repr {
			RocRecord(rec) => rec.anonymous
			RocTagUnion(tu) =>
				match single_variant_payload(tu) {
					SinglePayload(payload_id) => is_anonymous_record(table, payload_id)
					_ => Bool.False
				}
			_ => Bool.False
		}

	is_named_multi_tag_union : TypeRepr -> Bool
	is_named_multi_tag_union = |type_repr|
		match type_repr {
			RocTagUnion(tu) => List.len(tu.tags) >= 2 and tu.name != ""
			_ => Bool.False
		}

	tag_union_has_payload : TagUnionRepr -> Bool
	tag_union_has_payload = |tu| !(List.all(tu.tags, |tag| List.is_empty(tag.payload)))

	duplicate_tag_union_names : TypeTable -> List(Str)
	duplicate_tag_union_names = |table| {
		var $seen_names = []
		var $duplicates = []

		for type_repr in entries(table) {
			match type_repr {
				RocTagUnion(tu) =>
					if List.len(tu.tags) >= 2 and tu.name != "" {
						if List.contains($seen_names, tu.name) {
							if !(List.contains($duplicates, tu.name)) {
								$duplicates = $duplicates.append(tu.name)
							}
						} else {
							$seen_names = $seen_names.append(tu.name)
						}
					}
				_ => {}
			}
		}

		$duplicates
	}
}

sample_table : TypeTable
sample_table = TypeTable.from_list([
	RocU8,
	RocStr,
	RocList(0),
	RocRecord({ name: "Pair", anonymous: Bool.False, fields: [{ name: "left", type_id: 1, size: 24, alignment: 8, is_padding: Bool.False }], size: 24, alignment: 8 }),
	RocTagUnion({ name: "Wrapped", tags: [{ name: "Wrapped", payload: [3], payload_size: 24, payload_alignment: 8 }], size: 24, alignment: 8 }),
	RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [1], payload_size: 24, payload_alignment: 8 }, { name: "Ok", payload: [0], payload_size: 1, payload_alignment: 1 }], size: 32, alignment: 8 }),
	RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [2], payload_size: 24, payload_alignment: 8 }, { name: "Ok", payload: [0], payload_size: 1, payload_alignment: 1 }], size: 32, alignment: 8 }),
])

expect TypeTable.is_unit(sample_table, 0) == Bool.False
expect TypeTable.is_refcounted(sample_table, 1)
expect TypeTable.is_refcounted(sample_table, 2)
expect TypeTable.is_refcounted(sample_table, 3)
expect TypeTable.is_anonymous_record(sample_table, 4) == Bool.False
expect TypeTable.duplicate_tag_union_names(sample_table) == ["Try"]

expect
	match TypeTable.record_layout(sample_table, 4) {
		RecordFound(layout) => layout.size == 24 and List.len(layout.fields) == 1
		NotRecord => Bool.False
	}

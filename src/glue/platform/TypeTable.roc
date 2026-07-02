import AbiLayout exposing [AbiLayout]
import HostRcPlan exposing [HostRcPlan]
import RecordField exposing [RecordField]
import TagUnionRepr exposing [TagUnionRepr]
import TypeInfo exposing [TypeInfo]
import TypeRepr exposing [TypeRepr]

## Typed view over compiler-emitted glue type metadata.
##
## Authoritative compiler sources:
## - src/glue/glue.zig builds each TypeInfo row from checked type shape plus the
##   requested LIR layout metadata for the same checked type id.
## - src/layout/store.zig owns layout facts; TypeTable only exposes them.
##
## Glue scripts should ask semantic and ABI questions through this receiver
## module instead of rebuilding lookup tables or deriving layout facts locally.
TypeTable := { entries : List(TypeInfo) }.{
	RecordLookup := [
		RecordFound({ fields : List(RecordField), type_id : U64 }),
		NotRecord,
	]

	SingleVariantPayload := [
		SinglePayload(U64),
		SingleNoPayload,
		NotSingleVariant,
	]

	from_list : List(TypeInfo) -> TypeTable
	from_list = |entries| TypeTable.{ entries }

	entries : TypeTable -> List(TypeInfo)
	entries = |{ entries }| entries

	type_info : TypeTable, U64 -> TypeInfo
	type_info = |table, type_id|
		match List.get(table.entries(), type_id) {
			Ok(info) => info
			Err(_) => {
				crash "glue invariant violated: missing type table entry ${U64.to_str(type_id)}"
			}
		}

	get : TypeTable, U64 -> TypeRepr
	get = |table, type_id| (table.type_info(type_id)).repr

	layout : TypeTable, U64 -> AbiLayout
	layout = |table, type_id| (table.type_info(type_id)).layout

	rc_plan : TypeTable, U64 -> HostRcPlan
	rc_plan = |table, type_id| (table.type_info(type_id)).rc

	is_unit : TypeTable, U64 -> Bool
	is_unit = |table, type_id|
		match table.get(type_id) {
			RocUnit => Bool.True
			_ => Bool.False
		}

	is_refcounted : TypeTable, U64 -> Bool
	is_refcounted = |table, type_id| (table.layout(type_id)).contains_refcounted

	repr_is_refcounted : TypeTable, TypeRepr -> Bool
	repr_is_refcounted = |_table, type_repr|
		match type_repr {
			RocStr => Bool.True
			RocBox(_) => Bool.True
			RocList(_) => Bool.True
			RocFunction(_) => Bool.True
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
	record_layout = |table, type_id| table.record_layout_from_repr(type_id, table.get(type_id))

	record_layout_from_repr : TypeTable, U64, TypeRepr -> RecordLookup
	record_layout_from_repr = |table, type_id, type_repr|
		match type_repr {
			RocRecord(rec) =>
				if List.len(rec.fields) > 0 {
					RecordFound({ fields: rec.fields, type_id })
				} else {
					NotRecord
				}
			RocTagUnion(tu) =>
				match single_variant_payload(tu) {
					SinglePayload(payload_id) => table.record_layout(payload_id)
					_ => NotRecord
				}
			_ => NotRecord
		}

	is_anonymous_record : TypeTable, U64 -> Bool
	is_anonymous_record = |table, type_id| table.is_anonymous_record_repr(table.get(type_id))

	is_anonymous_record_repr : TypeTable, TypeRepr -> Bool
	is_anonymous_record_repr = |table, type_repr|
		match type_repr {
			RocRecord(rec) => rec.anonymous
			RocTagUnion(tu) =>
				match single_variant_payload(tu) {
					SinglePayload(payload_id) => table.is_anonymous_record(payload_id)
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

		for entry in table.entries() {
			match entry.repr {
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

sample_layout = {
	size32: 1,
	alignment32: 1,
	size64: 1,
	alignment64: 1,
	contains_refcounted: Bool.False,
	details: AbiBuiltin,
}

ref_layout = {
	size32: 12,
	alignment32: 4,
	size64: 24,
	alignment64: 8,
	contains_refcounted: Bool.True,
	details: AbiBuiltin,
}

sample_info = |repr| { repr, layout: sample_layout, rc: RcNoop }
ref_info = |repr| { repr, layout: ref_layout, rc: RcRefcounted }

sample_table : TypeTable
sample_table = TypeTable.from_list([
	sample_info(RocU8),
	ref_info(RocStr),
	ref_info(RocList(0)),
	ref_info(RocRecord({ name: "Pair", anonymous: Bool.False, fields: [{ name: "left", type_id: 1, is_padding: Bool.False }] })),
	ref_info(RocTagUnion({ name: "Wrapped", tags: [{ name: "Wrapped", payload: [3] }] })),
	ref_info(RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [1] }, { name: "Ok", payload: [0] }] })),
	ref_info(RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [2] }, { name: "Ok", payload: [0] }] })),
])

expect sample_table.is_unit(0) == Bool.False
expect sample_table.is_refcounted(1)
expect sample_table.is_refcounted(2)
expect sample_table.is_refcounted(3)
expect sample_table.is_anonymous_record(4) == Bool.False
expect sample_table.duplicate_tag_union_names() == ["Try"]

expect TypeTable.is_named_multi_tag_union(sample_table.get(5))

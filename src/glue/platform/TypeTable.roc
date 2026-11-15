import AbiLayout
import HostRcPlan
import RecordField
import RecordRepr
import TagUnionRepr
import TypeInfo
import TypeRepr

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
	from_list = |infos| TypeTable.{ entries: infos }

	entries : TypeTable -> List(TypeInfo)
	entries = |table| table.entries

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
	is_refcounted = |table, type_id|
		match table.rc_plan(type_id) {
			RcNoop => Bool.False
			RcRefcounted => Bool.True
			RcRocReleased => Bool.True
		}

	## True when a host that owns a value of this type cannot release it: the
	## value holds a reference only Roc can release. See `HostRcPlan`.
	is_roc_released : TypeTable, U64 -> Bool
	is_roc_released = |table, type_id|
		match table.rc_plan(type_id) {
			RcNoop => Bool.False
			RcRefcounted => Bool.False
			RcRocReleased => Bool.True
		}

	repr_is_refcounted : TypeTable, TypeRepr -> Bool
	repr_is_refcounted = |_table, type_repr|
		match type_repr {
			RocStr => Bool.True
			RocBox(_) => Bool.True
			RocList(_) => Bool.True
			RocErasedCallable(_) => Bool.True
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
				Err(_) => {
					crash "glue invariant violated: single-tag union had no tag"
				}
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

	## Id of the first row describing the same host type as `type_id`; see
	## `TypeInfo`.
	shape_id : TypeTable, U64 -> U64
	shape_id = |table, type_id| (table.type_info(type_id)).shape_id

	## The suffix that tells row `type_id` apart from other rows emitted under
	## `name`, or "" when `name` is not in `duplicate_names`. It is built from
	## the row's shape id, so every row of one shape gets the same suffix.
	shape_suffix : TypeTable, List(Str), Str, U64 -> Str
	shape_suffix = |table, duplicate_names, name, type_id|
		if List.contains(duplicate_names, name) {
			"Type${U64.to_str(table.shape_id(type_id))}"
		} else {
			""
		}

	## Names of tag unions that need per-shape disambiguation: a name qualifies
	## when two or more rows share it with different shape ids. Rows that
	## repeat one shape are the same emitted type and keep the bare name.
	duplicate_tag_union_names : TypeTable -> List(Str)
	duplicate_tag_union_names = |table| {
		var $seen = []
		var $duplicates = []

		for entry in table.entries() {
			match entry.repr {
				RocTagUnion(tu) =>
					if List.len(tu.tags) >= 2 and tu.name != "" {
						conflicts = List.any($seen, |e| e.name == tu.name and e.shape_id != entry.shape_id)
						if conflicts {
							if !(List.contains($duplicates, tu.name)) {
								$duplicates = $duplicates.append(tu.name)
							}
						}
						already = List.any($seen, |e| e.name == tu.name and e.shape_id == entry.shape_id)
						if !already {
							$seen = $seen.append({ name: tu.name, shape_id: entry.shape_id })
						}
					}
				_ => {}
			}
		}

		$duplicates
	}

	## Names of records that need per-shape disambiguation: a name qualifies
	## when two or more rows share it with different shape ids, as two
	## instantiations of one generic nominal do. Rows that repeat one shape are
	## the same emitted type and keep the bare name.
	duplicate_record_names : TypeTable -> List(Str)
	duplicate_record_names = |table| {
		var $seen = []
		var $duplicates = []

		for entry in table.entries() {
			match entry.repr {
				RocRecord(rec) =>
					if rec.name != "" {
						conflicts = List.any($seen, |e| e.name == rec.name and e.shape_id != entry.shape_id)
						if conflicts {
							if !(List.contains($duplicates, rec.name)) {
								$duplicates = $duplicates.append(rec.name)
							}
						}
						already = List.any($seen, |e| e.name == rec.name and e.shape_id == entry.shape_id)
						if !already {
							$seen = $seen.append({ name: rec.name, shape_id: entry.shape_id })
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

sample_info : TypeRepr, U64 -> TypeInfo
sample_info = |repr, shape_id| { repr, layout: sample_layout, rc: RcNoop, shape_id }

ref_info : TypeRepr, U64 -> TypeInfo
ref_info = |repr, shape_id| { repr, layout: ref_layout, rc: RcRefcounted, shape_id }

sample_table : TypeTable
sample_table = TypeTable.from_list([
	sample_info(RocU8, 0),
	ref_info(RocStr, 1),
	ref_info(RocList(0), 2),
	ref_info(RocRecord({ name: "Pair", anonymous: Bool.False, fields: [{ name: "left", type_id: 1, is_padding: Bool.False }] }), 3),
	ref_info(RocTagUnion({ name: "Wrapped", tags: [{ name: "Wrapped", payload: [3] }] }), 4),
	ref_info(RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [1] }, { name: "Ok", payload: [0] }] }), 5),
	ref_info(RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [2] }, { name: "Ok", payload: [0] }] }), 6),
])

## Checks the sample `TypeTable.is_unit` behavior.
expect sample_table.is_unit(0) == Bool.False

## Checks the sample `TypeTable.is_refcounted` behavior.
expect sample_table.is_refcounted(1)

## Checks the sample `TypeTable.is_refcounted` behavior.
expect sample_table.is_refcounted(2)

## Checks the sample `TypeTable.is_refcounted` behavior.
expect sample_table.is_refcounted(3)

## Checks the sample `TypeTable.is_anonymous_record` behavior.
expect sample_table.is_anonymous_record(4) == Bool.False

## Checks the sample `TypeTable.duplicate_tag_union_names` behavior.
expect sample_table.duplicate_tag_union_names() == ["Try"]

## Checks `TypeTable.is_named_multi_tag_union` for this representative case.
expect TypeTable.is_named_multi_tag_union(sample_table.get(5))

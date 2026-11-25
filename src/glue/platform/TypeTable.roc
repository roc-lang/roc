import AbiLayout
import AbiTagLayout
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

	## What the host sees in place of a tag union that has one variant. Such a
	## union stores no discriminant, so its value is exactly its payload.
	SingleVariantPayload := [
		## One payload: the union is that payload's type.
		SinglePayload(U64),

		## Several payloads that occupy storage: the union is a struct of the
		## variant's committed payload fields.
		SinglePayloadStruct(AbiTagLayout),

		## No payloads, or several that occupy no storage.
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

	## True when a value of this type occupies no storage: its committed size
	## is zero at both pointer widths. Such a value has no host representation.
	## It is not passed as an argument, is not returned, and is not a member of
	## a C struct. An opaque heap target is never zero-sized: its row carries
	## no size because the host cannot know one.
	is_zero_sized : TypeTable, U64 -> Bool
	is_zero_sized = |table, type_id| {
		info = table.type_info(type_id)
		match info.repr {
			RocUnknown(_) => Bool.False
			_ => info.layout.size32 == 0 and info.layout.size64 == 0
		}
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

	## Classify the tag union at row `type_id`, whose repr is `tu`.
	single_variant_payload : TypeTable, U64, TagUnionRepr -> SingleVariantPayload
	single_variant_payload = |table, type_id, tu|
		if List.len(tu.tags) == 1 {
			match List.first(tu.tags) {
				Ok(tag) =>
					if List.len(tag.payload) <= 1 {
						match List.first(tag.payload) {
							Ok(payload_id) => SinglePayload(payload_id)
							Err(_) => SingleNoPayload
						}
					} else {
						match List.first((table.layout(type_id)).tag_layouts()) {
							Ok(tag_layout) =>
								if tag_layout.payload_size32 > 0 or tag_layout.payload_size64 > 0 {
									SinglePayloadStruct(tag_layout)
								} else {
									SingleNoPayload
								}
							Err(_) => {
								crash "glue invariant violated: single-tag union had no ABI tag layout"
							}
						}
					}
				Err(_) => {
					crash "glue invariant violated: single-tag union had no tag"
				}
			}
		} else {
			NotSingleVariant
		}

	## Whether generators declare a host type for the tag union itself: it has
	## several variants, or its one variant holds several payloads in storage.
	## Every other tag union is its payload's type, or nothing.
	declares_tag_union : TypeTable, U64, TagUnionRepr -> Bool
	declares_tag_union = |table, type_id, tu|
		if tu.name == "" {
			Bool.False
		} else {
			match table.single_variant_payload(type_id, tu) {
				SinglePayloadStruct(_) => Bool.True
				NotSingleVariant => Bool.True
				SinglePayload(_) => Bool.False
				SingleNoPayload => Bool.False
			}
		}

	record_layout : TypeTable, U64 -> RecordLookup
	record_layout = |table, type_id|
		if table.is_zero_sized(type_id) {
			NotRecord
		} else {
			table.record_layout_from_repr(type_id, table.get(type_id))
		}

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
				match table.single_variant_payload(type_id, tu) {
					SinglePayload(payload_id) => table.record_layout(payload_id)
					_ => NotRecord
				}
			_ => NotRecord
		}

	is_anonymous_record : TypeTable, U64 -> Bool
	is_anonymous_record = |table, type_id|
		match table.get(type_id) {
			RocRecord(rec) => rec.anonymous
			RocTagUnion(tu) =>
				match table.single_variant_payload(type_id, tu) {
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

	## Names of declared tag unions that need per-shape disambiguation: a name
	## qualifies when two or more rows share it with different shape ids. Rows
	## that repeat one shape are the same emitted type and keep the bare name.
	duplicate_tag_union_names : TypeTable -> List(Str)
	duplicate_tag_union_names = |table| {
		var $seen = []
		var $duplicates = []
		var $type_id = 0

		for entry in table.entries() {
			match entry.repr {
				RocTagUnion(tu) =>
					if table.declares_tag_union($type_id, tu) {
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

			$type_id = $type_id + 1
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

## A one-variant union named `name` whose payloads are the record at row 3 and
## `second_payload_id`, together occupying `payload_size` bytes.
multi_payload_info : Str, U64, U64, U64 -> TypeInfo
multi_payload_info = |name, second_payload_id, payload_size, shape_id| {
	repr: RocTagUnion({ name, tags: [{ name, payload: [3, second_payload_id] }] }),
	layout: {
		size32: payload_size,
		alignment32: 4,
		size64: payload_size,
		alignment64: 8,
		contains_refcounted: Bool.True,
		details: AbiTagUnion({
			discriminant_offset32: 0,
			discriminant_offset64: 0,
			discriminant_size: 0,
			has_payload: payload_size > 0,
			tags: [
				{
					discriminant: 0,
					name,
					payload: [3, second_payload_id],
					payload_fields: [],
					payload_alignment32: 4,
					payload_alignment64: 8,
					payload_size32: payload_size,
					payload_size64: payload_size,
				},
			],
		}),
	},
	rc: RcRefcounted,
	shape_id,
}

sample_single_variant : U64 -> TypeTable.SingleVariantPayload
sample_single_variant = |type_id|
	match sample_table.get(type_id) {
		RocTagUnion(tu) => sample_table.single_variant_payload(type_id, tu)
		_ => NotSingleVariant
	}

sample_table : TypeTable
sample_table = TypeTable.from_list([
	sample_info(RocU8, 0),
	ref_info(RocStr, 1),
	ref_info(RocList(0), 2),
	ref_info(RocRecord({ name: "Pair", anonymous: Bool.False, fields: [{ name: "left", type_id: 1, is_padding: Bool.False }] }), 3),
	ref_info(RocTagUnion({ name: "Wrapped", tags: [{ name: "Wrapped", payload: [3] }] }), 4),
	ref_info(RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [1] }, { name: "Ok", payload: [0] }] }), 5),
	ref_info(RocTagUnion({ name: "Try", tags: [{ name: "Err", payload: [2] }, { name: "Ok", payload: [0] }] }), 6),
	multi_payload_info("Stepped", 0, 32, 7),
	multi_payload_info("Stepped", 1, 48, 8),
	multi_payload_info("Hollow", 0, 0, 9),
	{ repr: RocUnknown("model"), layout: { size32: 0, alignment32: 1, size64: 0, alignment64: 1, contains_refcounted: Bool.False, details: AbiBuiltin }, rc: RcNoop, shape_id: 10 },
])

## A sized value is not zero-sized.
expect sample_table.is_zero_sized(0) == Bool.False

## A union whose payloads occupy no storage is zero-sized.
expect sample_table.is_zero_sized(9)

## An opaque heap target carries no size and is not zero-sized.
expect sample_table.is_zero_sized(10) == Bool.False

## Checks the sample `TypeTable.is_refcounted` behavior.
expect sample_table.is_refcounted(1)

## Checks the sample `TypeTable.is_refcounted` behavior.
expect sample_table.is_refcounted(2)

## Checks the sample `TypeTable.is_refcounted` behavior.
expect sample_table.is_refcounted(3)

## Checks the sample `TypeTable.is_anonymous_record` behavior.
expect sample_table.is_anonymous_record(4) == Bool.False

## Checks the sample `TypeTable.duplicate_tag_union_names` behavior.
expect sample_table.duplicate_tag_union_names() == ["Try", "Stepped"]

## A one-variant union with one payload is that payload.
expect
	match sample_single_variant(4) {
		SinglePayload(payload_id) => payload_id == 3
		_ => Bool.False
	}

## A one-variant union with several payloads is a struct of all of them.
expect
	match sample_single_variant(7) {
		SinglePayloadStruct(tag_layout) => tag_layout.payload == [3, 0]
		_ => Bool.False
	}

## Several payloads that occupy no storage leave nothing for the host to hold.
expect
	match sample_single_variant(9) {
		SingleNoPayload => Bool.True
		_ => Bool.False
	}

## A one-variant union with several payloads is not its first payload's record.
expect
	match sample_table.record_layout(7) {
		NotRecord => Bool.True
		RecordFound(_) => Bool.False
	}

## Checks `TypeTable.is_named_multi_tag_union` for this representative case.
expect TypeTable.is_named_multi_tag_union(sample_table.get(5))

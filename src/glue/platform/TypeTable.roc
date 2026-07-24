import AbiLayout exposing [AbiLayout]
import HostRcPlan exposing [HostRcPlan]
import RecordField exposing [RecordField]
import RecordRepr exposing [RecordRepr]
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

	tag_union_has_payload : TagUnionRepr -> Bool
	tag_union_has_payload = |tu| !(List.all(tu.tags, |tag| List.is_empty(tag.payload)))

	## Stable structural token for a type, independent of type-table entry
	## order. Named types (record / tag union / unknown) contribute their name
	## and terminate the recursion (a recursive shape routes through a named
	## nominal, and anonymous records already carry a structural `__AnonStruct_`
	## name assigned by the compiler); box and list recurse into their element;
	## primitives contribute a fixed token. This mirrors the compiler-side
	## `hashStructuralId` used to name anonymous structs.
	structural_token : TypeTable, U64 -> Str
	structural_token = |table, type_id|
		match table.get(type_id) {
			RocBool => "bool"
			RocBox(inner) => Str.concat("box:", table.structural_token(inner))
			RocDec => "dec"
			RocF32 => "f32"
			RocF64 => "f64"
			RocFunction(_) => "fn"
			RocI128 => "i128"
			RocI16 => "i16"
			RocI32 => "i32"
			RocI64 => "i64"
			RocI8 => "i8"
			RocList(elem) => Str.concat("list:", table.structural_token(elem))
			RocRecord(rec) => Str.concat("rec:", rec.name)
			RocStr => "str"
			RocTagUnion(tu) => Str.concat("tu:", tu.name)
			RocU128 => "u128"
			RocU16 => "u16"
			RocU32 => "u32"
			RocU64 => "u64"
			RocU8 => "u8"
			RocUnit => "unit"
			RocUnknown(s) => Str.concat("unk:", s)
		}

	## Stable structural signature of a tag union: its ABI size and alignment
	## plus each tag's name and payload shapes. Two same-named tag-union entries
	## with equal signatures are the same emitted type (glue renders them
	## identically, and the emitter deduplicates them by name); unequal
	## signatures are genuinely distinct shapes that need disambiguation.
	tag_union_signature : TypeTable, AbiLayout, TagUnionRepr -> Str
	tag_union_signature = |table, abi_layout, tu| {
		var $sig = "${U64.to_str(abi_layout.size64)}/${U64.to_str(abi_layout.alignment64)}/${U64.to_str(abi_layout.size32)}/${U64.to_str(abi_layout.alignment32)}"

		for tag in tu.tags {
			$sig = Str.concat($sig, "|${tag.name}(")
			for payload_id in tag.payload {
				$sig = Str.concat($sig, Str.concat(table.structural_token(payload_id), ","))
			}
			$sig = Str.concat($sig, ")")
		}

		$sig
	}

	## Names of tag unions that need per-entry disambiguation: a name qualifies
	## only when two or more entries share it with genuinely distinct structural
	## signatures. Entries that merely repeat an identical shape are the same
	## emitted type and collapse to the bare name, so they are not reported.
	duplicate_tag_union_names : TypeTable -> List(Str)
	duplicate_tag_union_names = |table| {
		var $seen = []
		var $duplicates = []

		for entry in table.entries() {
			match entry.repr {
				RocTagUnion(tu) =>
					if List.len(tu.tags) >= 2 and tu.name != "" {
						sig = table.tag_union_signature(entry.layout, tu)
						conflicts = List.any($seen, |e| e.name == tu.name and e.sig != sig)
						if conflicts {
							if !(List.contains($duplicates, tu.name)) {
								$duplicates = $duplicates.append(tu.name)
							}
						}
						already = List.any($seen, |e| e.name == tu.name and e.sig == sig)
						if !already {
							$seen = $seen.append({ name: tu.name, sig: sig })
						}
					}
				_ => {}
			}
		}

		$duplicates
	}

	## Stable structural signature of a record: its ABI size and alignment plus
	## each field's name and shape. Same-named record entries with equal
	## signatures are the same emitted type (the emitter deduplicates them by
	## name); unequal signatures are genuinely distinct shapes needing
	## disambiguation. Mirrors `tag_union_signature`.
	record_signature : TypeTable, AbiLayout, RecordRepr -> Str
	record_signature = |table, abi_layout, rec| {
		var $sig = "${U64.to_str(abi_layout.size64)}/${U64.to_str(abi_layout.alignment64)}/${U64.to_str(abi_layout.size32)}/${U64.to_str(abi_layout.alignment32)}"

		for field in rec.fields {
			$sig = Str.concat($sig, "|${field.name}:${table.structural_token(field.type_id)}")
		}

		$sig
	}

	## Names of records that need per-entry disambiguation: a name qualifies
	## only when two or more entries share it with genuinely distinct structural
	## signatures. Entries that merely repeat an identical shape are the same
	## emitted type and collapse to the bare name, so they are not reported.
	## Mirrors `duplicate_tag_union_names`.
	duplicate_record_names : TypeTable -> List(Str)
	duplicate_record_names = |table| {
		var $seen = []
		var $duplicates = []

		for entry in table.entries() {
			match entry.repr {
				RocRecord(rec) =>
					if rec.name != "" {
						sig = table.record_signature(entry.layout, rec)
						conflicts = List.any($seen, |e| e.name == rec.name and e.sig != sig)
						if conflicts {
							if !(List.contains($duplicates, rec.name)) {
								$duplicates = $duplicates.append(rec.name)
							}
						}
						already = List.any($seen, |e| e.name == rec.name and e.sig == sig)
						if !already {
							$seen = $seen.append({ name: rec.name, sig: sig })
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

sample_info : TypeRepr -> TypeInfo
sample_info = |repr| { repr, layout: sample_layout, rc: RcNoop }

ref_info : TypeRepr -> TypeInfo
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

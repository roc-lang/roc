import HostedFunctionInfo exposing [HostedFunctionInfo]
import RecordField exposing [RecordField]
import TypeRepr exposing [TypeRepr]
import TypeTable exposing [TypeTable]

## Shared ABI argument-shape classification for glue generators.
##
## This module decides which reflected type IDs are ABI-visible. Target
## generators still render those shapes in their own syntax.
##
## Authoritative compiler sources:
## - src/glue/glue.zig emits hosted function arg_type_ids, ret_type_id, and
##   reflected TypeRepr rows.
## - src/layout/store.zig commits record layouts; src/layout/field_order.zig
##   owns field order for structural records.
## - design.md documents that checked row order is for lookup, not runtime slot
##   order; layout is committed later.
##
## This helper consumes those explicit IDs and reflected layouts. It does not
## reconstruct argument shape from source syntax.
ArgShape := {}.{
	RecordInfo := { alignment : U64, anonymous : Bool, fields : List(RecordField), size : U64 }

	RecordLookup := [
		ArgRecordFound(RecordInfo),
		ArgNotRecord,
	]

	HostedArgs := [
		NoMeaningfulArgs,
		SingleRecordArg(RecordInfo),
		PositionalArgs(List(U64)),
	]

	## Look up a type id as an ABI record, following the TypeTable rules for
	## single-payload tag unions. The returned fields come from reflected compiler
	## layout data and are not reordered here.
	record_lookup : List(TypeRepr), U64 -> RecordLookup
	record_lookup = |type_table, type_id| {
		table = TypeTable.from_list(type_table)
		match TypeTable.record_layout(table, type_id) {
			RecordFound(layout) =>
				ArgRecordFound({
					alignment: layout.alignment,
					anonymous: TypeTable.is_anonymous_record(table, type_id),
					fields: layout.fields,
					size: layout.size,
				})
			NotRecord => ArgNotRecord
		}
	}

	## Classify hosted-function arguments into the data shape each target renders:
	## no ABI-visible args, one record wrapper, or positional non-unit args.
	hosted_args : List(TypeRepr), HostedFunctionInfo -> HostedArgs
	hosted_args = |type_table, func| args_for_type_ids(type_table, func.arg_type_ids)

	## Classify a raw compiler-emitted argument id list. Unit args are not
	## ABI-visible and are excluded from positional output.
	args_for_type_ids : List(TypeRepr), List(U64) -> HostedArgs
	args_for_type_ids = |type_table, arg_type_ids| {
		if List.is_empty(arg_type_ids) {
			return NoMeaningfulArgs
		}

		if List.len(arg_type_ids) == 1 {
			match List.first(arg_type_ids) {
				Ok(type_id) =>
					if TypeTable.is_unit(TypeTable.from_list(type_table), type_id) {
						NoMeaningfulArgs
					} else {
						match record_lookup(type_table, type_id) {
							ArgRecordFound(record) => SingleRecordArg(record)
							ArgNotRecord => PositionalArgs([type_id])
						}
					}
				Err(_) => NoMeaningfulArgs
			}
		} else {
			non_unit_ids = positional_non_unit_type_ids(type_table, arg_type_ids)
			if List.is_empty(non_unit_ids) {
				NoMeaningfulArgs
			} else {
				PositionalArgs(non_unit_ids)
			}
		}
	}

	## Return only ABI-visible positional argument ids, preserving compiler order.
	positional_non_unit_type_ids : List(TypeRepr), List(U64) -> List(U64)
	positional_non_unit_type_ids = |type_table, arg_type_ids| {
		table = TypeTable.from_list(type_table)
		var $non_unit_ids = []

		for type_id in arg_type_ids {
			if !(TypeTable.is_unit(table, type_id)) {
				$non_unit_ids = $non_unit_ids.append(type_id)
			}
		}

		$non_unit_ids
	}

	## Anonymous single-record args are rendered through the generated Args wrapper
	## in direct hosted symbol declarations.
	single_arg_is_anonymous_record : List(TypeRepr), List(U64) -> Bool
	single_arg_is_anonymous_record = |type_table, arg_type_ids| {
		if List.len(arg_type_ids) == 1 {
			match List.first(arg_type_ids) {
				Ok(type_id) => TypeTable.is_anonymous_record(TypeTable.from_list(type_table), type_id)
				Err(_) => Bool.False
			}
		} else {
			Bool.False
		}
	}
}

import AbiLayout exposing [AbiLayout]
import AbiFieldLayout exposing [AbiFieldLayout]
import HostedFunctionInfo exposing [HostedFunctionInfo]
import TypeTable exposing [TypeTable]

## Shared ABI argument-shape classification for glue generators.
##
## This module decides which reflected type IDs are ABI-visible. Target
## generators still render those shapes in their own syntax.
##
## Authoritative compiler sources:
## - src/glue/glue.zig emits hosted function arg_type_ids, ret_type_id, and
##   reflected TypeRepr rows.
## - src/lir/program.zig `RequestedLayout` and src/layout/store.zig provide the
##   exact ABI layout rows returned here.
## - design.md documents that checked row order is for lookup, not runtime slot
##   order; layout is committed later.
##
## This helper consumes those explicit IDs and compiler-emitted layouts. It does not
## reconstruct argument shape from source syntax.
ArgShape := { table : TypeTable }.{
	RecordInfo := { anonymous : Bool, fields : List(AbiFieldLayout), layout : AbiLayout, type_id : U64 }

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
	from_table : TypeTable -> ArgShape
	from_table = |table| ArgShape.{ table }

	record_lookup : ArgShape, U64 -> RecordLookup
	record_lookup = |arg_shape, type_id| {
		match arg_shape.table.record_layout(type_id) {
			RecordFound(record) => {
				abi_layout = arg_shape.table.layout(record.type_id)
				ArgRecordFound({
					anonymous: arg_shape.table.is_anonymous_record(record.type_id),
					fields: abi_layout.record_fields(),
					layout: abi_layout,
					type_id: record.type_id,
				})
			}
			NotRecord => ArgNotRecord
		}
	}

	## Classify hosted-function arguments into the data shape each target renders:
	## no ABI-visible args, one record wrapper, or positional non-unit args.
	hosted_args : ArgShape, HostedFunctionInfo -> HostedArgs
	hosted_args = |arg_shape, func| args_for_type_ids(arg_shape, func.arg_type_ids)

	## Classify a raw compiler-emitted argument id list. Unit args are not
	## ABI-visible and are excluded from positional output.
	args_for_type_ids : ArgShape, List(U64) -> HostedArgs
	args_for_type_ids = |arg_shape, arg_type_ids| {
		if List.is_empty(arg_type_ids) {
			return NoMeaningfulArgs
		}

		if List.len(arg_type_ids) == 1 {
			match List.first(arg_type_ids) {
				Ok(type_id) =>
					if arg_shape.table.is_unit(type_id) {
						NoMeaningfulArgs
					} else {
						match arg_shape.record_lookup(type_id) {
							ArgRecordFound(record) => SingleRecordArg(record)
							ArgNotRecord => PositionalArgs([type_id])
						}
					}
				Err(_) => NoMeaningfulArgs
			}
		} else {
			non_unit_ids = positional_non_unit_type_ids(arg_shape, arg_type_ids)
			if List.is_empty(non_unit_ids) {
				NoMeaningfulArgs
			} else {
				PositionalArgs(non_unit_ids)
			}
		}
	}

	## Return only ABI-visible positional argument ids, preserving compiler order.
	positional_non_unit_type_ids : ArgShape, List(U64) -> List(U64)
	positional_non_unit_type_ids = |arg_shape, arg_type_ids| {
		var $non_unit_ids = []

		for type_id in arg_type_ids {
			if !(arg_shape.table.is_unit(type_id)) {
				$non_unit_ids = $non_unit_ids.append(type_id)
			}
		}

		$non_unit_ids
	}

	## Anonymous single-record args are rendered through the generated Args wrapper
	## in direct hosted symbol declarations.
	single_arg_is_anonymous_record : ArgShape, List(U64) -> Bool
	single_arg_is_anonymous_record = |arg_shape, arg_type_ids| {
		if List.len(arg_type_ids) == 1 {
			match List.first(arg_type_ids) {
				Ok(type_id) => arg_shape.table.is_anonymous_record(type_id)
				Err(_) => Bool.False
			}
		} else {
			Bool.False
		}
	}
}

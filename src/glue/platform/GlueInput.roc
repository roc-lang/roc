import HostedFunctionInfo exposing [HostedFunctionInfo]
import ProvidesEntry exposing [ProvidesEntry]
import TypeRepr exposing [TypeRepr]
import Types exposing [Types]

## Normalized input shared by glue generators.
##
## This keeps target entrypoints data-oriented: generators receive sorted
## hosted functions, the reflected type table, and provides metadata, then render
## their language-specific files locally.
##
## Authoritative compiler sources:
## - src/glue/glue.zig produces the reflected Types payload.
## - src/glue/platform/host.zig marshals that payload into these Roc records.
##
## This helper only qualifies hosted function names with their module names and
## sorts by HostedFunctionInfo.index, matching the compiler-emitted index order.
## It does not derive ABI shape or inspect source syntax.
GlueInput := {}.{
	Input := {
		hosted_functions : List(HostedFunctionInfo),
		provides_entries : List(ProvidesEntry),
		type_table : List(TypeRepr),
	}

	from_types : List(Types) -> Input
	from_types = |types_list| {
		var $hosted_functions = []
		var $type_table = []
		var $provides_entries = []

		for types in types_list {
			$type_table = types.type_table
			$provides_entries = types.provides_entries

			for mod in types.modules {
				for func in mod.hosted_functions {
					qualified_func = {
						arg_fields: func.arg_fields,
						arg_type_ids: func.arg_type_ids,
						ffi_symbol: func.ffi_symbol,
						index: func.index,
						name: "${mod.name}.${func.name}",
						ret_fields: func.ret_fields,
						ret_type_id: func.ret_type_id,
						type_str: func.type_str,
					}

					$hosted_functions = $hosted_functions.append(qualified_func)
				}
			}
		}

		{
			hosted_functions: List.sort_with($hosted_functions, compare_by_index),
			provides_entries: $provides_entries,
			type_table: $type_table,
		}
	}

	compare_by_index : HostedFunctionInfo, HostedFunctionInfo -> [LT, EQ, GT]
	compare_by_index = |a, b| {
		if a.index < b.index {
			return LT
		}
		if a.index > b.index {
			return GT
		}
		EQ
	}
}

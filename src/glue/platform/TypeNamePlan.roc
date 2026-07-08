import ProvidesEntry exposing [ProvidesEntry]
import RocName exposing [RocName]
import TypeTable exposing [TypeTable]

## Shared traversal for generated type names and platform aliases.
##
## Authoritative compiler sources:
## - src/glue/glue.zig emits the TypeRepr graph and hosted/provided root ids.
## - src/glue/platform/host.zig marshals those ids into Roc glue records.
##
## Target generators provide already-sanitized alias_base and module_base strings
## because language-specific escaping and reserved words stay local. This module
## only walks the reflected type graph and returns data plans.
TypeNamePlan := { table : TypeTable }.{
	Root := { alias_base : Str, module_base : Str, type_id : U64 }

	PreferredName := { name : Str, type_id : U64 }

	PreferredNames := { entries : List(PreferredName) }.{
		from_list : List(PreferredName) -> PreferredNames
		from_list = |entries| PreferredNames.{ entries }

		lookup : PreferredNames, U64 -> { found : Bool, name : Str }
		lookup = |preferred_names, type_id| {
			var $found = Bool.False
			var $name = ""

			for entry in preferred_names.entries {
				if !$found and entry.type_id == type_id {
					$found = Bool.True
					$name = entry.name
				}
			}

			{ found: $found, name: $name }
		}
	}

	AliasKind := [PlainAlias, TagUnionAlias]

	AliasPlan := { alias : Str, kind : AliasKind, type_id : U64 }

	from_table : TypeTable -> TypeNamePlan
	from_table = |table| TypeNamePlan.{ table }

	preferred_names : TypeNamePlan, List(Str), List(Root) -> PreferredNames
	preferred_names = |planner, existing_names, roots| {
		var $state = { entries: [], seen_names: existing_names, seen_type_ids: [] }

		for root in roots {
			$state = collect_preferred_for_type_id(
				$state,
				planner.table,
				root.type_id,
				root.alias_base,
				root.module_base,
				[],
			)
		}

		PreferredNames.from_list($state.entries)
	}

	alias_plan : TypeNamePlan, List(Root) -> List(AliasPlan)
	alias_plan = |planner, roots| {
		var $state = { entries: [], seen_aliases: [] }

		for root in roots {
			$state = collect_aliases_for_type_id(
				$state,
				planner.table,
				root.type_id,
				root.alias_base,
				root.module_base,
				[],
			)
		}

		$state.entries
	}

	provided_entry_root_type_id : TypeNamePlan, ProvidesEntry -> U64
	provided_entry_root_type_id = |planner, entry| {
		match planner.table.get(entry.type_id) {
			RocFunction(func) => func.ret
			_ => entry.type_id
		}
	}

	collect_preferred_for_type_id : { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }, TypeTable, U64, Str, Str, List(U64) -> { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }
	collect_preferred_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids| {
		if List.contains(visited_type_ids, type_id) {
			return state
		}

		next_visited = visited_type_ids.append(type_id)

		type_repr = type_table.get(type_id)
		match type_repr {
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) =>
						collect_preferred_for_type_id(
							state,
							type_table,
							payload_id,
							alias_base,
							module_base,
							next_visited,
						)
					SingleNoPayload => state
					NotSingleVariant =>
						if tu.name != "" {
							with_union_name =
								if tu.name == "Try" {
									add_preferred_name(state, type_id, "${alias_base}Result")
								} else if tu.name == "IOErr" {
									add_preferred_name(state, type_id, "${module_base}IOErr")
								} else {
									state
								}

							var $next = with_union_name
							for tag in tu.tags {
								child_base = "${alias_base}${RocName.capitalize_first(tag.name)}"
								for payload_id in tag.payload {
									$next = collect_preferred_for_type_id(
										$next,
										type_table,
										payload_id,
										child_base,
										module_base,
										next_visited,
									)
								}
							}
							$next
						} else {
							state
						}
				}
			RocList(elem_id) => collect_preferred_for_type_id(state, type_table, elem_id, alias_base, module_base, next_visited)
			RocBox(inner_id) => collect_preferred_for_type_id(state, type_table, inner_id, alias_base, module_base, next_visited)
			_ => state
		}
	}

	add_preferred_name : { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }, U64, Str -> { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }
	add_preferred_name = |state, type_id, name| {
		if name == "" or List.contains(state.seen_type_ids, type_id) or List.contains(state.seen_names, name) {
			state
		} else {
			{
				entries: state.entries.append({ type_id, name }),
				seen_names: state.seen_names.append(name),
				seen_type_ids: state.seen_type_ids.append(type_id),
			}
		}
	}

	collect_aliases_for_type_id : { entries : List(AliasPlan), seen_aliases : List(Str) }, TypeTable, U64, Str, Str, List(U64) -> { entries : List(AliasPlan), seen_aliases : List(Str) }
	collect_aliases_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids| {
		if List.contains(visited_type_ids, type_id) {
			return state
		}

		next_visited = visited_type_ids.append(type_id)

		type_repr = type_table.get(type_id)
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" {
					add_alias(state, alias_base, type_id, PlainAlias)
				} else {
					state
				}
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) =>
						collect_aliases_for_type_id(
							state,
							type_table,
							payload_id,
							alias_base,
							module_base,
							next_visited,
						)
					SingleNoPayload => state
					NotSingleVariant =>
						if tu.name != "" {
							with_union_alias =
								if tu.name == "Try" {
									add_alias(state, "${alias_base}Result", type_id, TagUnionAlias)
								} else if tu.name == "IOErr" {
									add_alias(state, "${module_base}IOErr", type_id, TagUnionAlias)
								} else {
									add_alias(state, alias_base, type_id, TagUnionAlias)
								}

							var $next = with_union_alias
							for tag in tu.tags {
								child_base = "${alias_base}${RocName.capitalize_first(tag.name)}"
								for payload_id in tag.payload {
									$next = collect_aliases_for_type_id(
										$next,
										type_table,
										payload_id,
										child_base,
										module_base,
										next_visited,
									)
								}
							}
							$next
						} else {
							state
						}
				}
			RocList(elem_id) => collect_aliases_for_type_id(state, type_table, elem_id, alias_base, module_base, next_visited)
			RocBox(inner_id) => collect_aliases_for_type_id(state, type_table, inner_id, alias_base, module_base, next_visited)
			_ => state
		}
	}

	add_alias : { entries : List(AliasPlan), seen_aliases : List(Str) }, Str, U64, AliasKind -> { entries : List(AliasPlan), seen_aliases : List(Str) }
	add_alias = |state, alias, type_id, kind| {
		if alias == "" or List.contains(state.seen_aliases, alias) {
			state
		} else {
			{
				entries: state.entries.append({ alias, kind, type_id }),
				seen_aliases: state.seen_aliases.append(alias),
			}
		}
	}
}

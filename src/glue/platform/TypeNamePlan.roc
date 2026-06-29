import ProvidesEntry exposing [ProvidesEntry]
import RocName exposing [RocName]
import TypeRepr exposing [TypeRepr]
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
TypeNamePlan := {}.{
	Root := { alias_base : Str, module_base : Str, type_id : U64 }

	PreferredName := { name : Str, type_id : U64 }

	AliasKind := [PlainAlias, TagUnionAlias]

	AliasPlan := { alias : Str, kind : AliasKind, type_id : U64 }

	preferred_names : List(TypeRepr), List(Str), List(Root) -> List(PreferredName)
	preferred_names = |type_table, existing_names, roots| {
		var $state = { entries: [], seen_names: existing_names, seen_type_ids: [] }

		for root in roots {
			$state = collect_preferred_for_type_id(
				$state,
				type_table,
				root.type_id,
				root.alias_base,
				root.module_base,
				[],
			)
		}

		$state.entries
	}

	lookup_preferred : List(PreferredName), U64 -> { found : Bool, name : Str }
	lookup_preferred = |preferred_names_list, type_id| {
		var $found = Bool.False
		var $name = ""

		for entry in preferred_names_list {
			if !$found and entry.type_id == type_id {
				$found = Bool.True
				$name = entry.name
			}
		}

		{ found: $found, name: $name }
	}

	alias_plan : List(TypeRepr), List(Root) -> List(AliasPlan)
	alias_plan = |type_table, roots| {
		var $state = { entries: [], seen_aliases: [] }

		for root in roots {
			$state = collect_aliases_for_type_id(
				$state,
				type_table,
				root.type_id,
				root.alias_base,
				root.module_base,
				[],
			)
		}

		$state.entries
	}

	provided_entry_root_type_id : List(TypeRepr), ProvidesEntry -> U64
	provided_entry_root_type_id = |type_table, entry| {
		match TypeTable.get(TypeTable.from_list(type_table), entry.type_id) {
			RocFunction(func) => func.ret
			_ => entry.type_id
		}
	}

	collect_preferred_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids| {
		if List.contains(visited_type_ids, type_id) {
			return state
		}

		next_visited = visited_type_ids.append(type_id)

		type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
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

	collect_aliases_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids| {
		if List.contains(visited_type_ids, type_id) {
			return state
		}

		next_visited = visited_type_ids.append(type_id)

		type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" and rec.anonymous {
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
									state
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

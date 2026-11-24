import HostedFunctionInfo
import ProvidesEntry
import RocName
import TypeTable

## Shared traversal for generated type names and platform aliases.
##
## Authoritative compiler sources:
## - src/glue/glue.zig emits the TypeRepr graph and hosted/provided root ids,
##   and marshals those ids into Roc glue records.
##
## Target generators provide already-sanitized alias_base and module_base strings
## because language-specific escaping and reserved words stay local. This module
## only walks the reflected type graph and returns data plans.
##
## A generated file has one namespace for its top-level names. Five kinds of
## name land in it, and each kind yields to the ones before it:
##
## 1. The platform's linker symbols, which the file declares under their own
##    names, and the names of the generated runtime support, which host code
##    is written against. Neither can change, so a symbol that is one of the
##    runtime support's names is an error (`undeclarable_symbol`).
## 2. The names derived from each hosted function, which a generator reports
##    as one `Declared`. `disambiguated` settles which functions need names
##    derived from their hosted index for these to be distinct.
## 3. Each host type's own name and the names derived from it, also one
##    `Declared` each. `disambiguated` settles which Roc type names need a
##    shape-derived name.
## 4. Preferred names, which replace a host type's own name and so claim the
##    names derived from them too. `preferred_names` grants one only when
##    every name it claims is free.
## 5. Aliases, which a generator emits only under names that
##    `PreferredNames.declared_names` leaves free.
TypeNamePlan := { table : TypeTable }.{
	Root := { alias_base : Str, module_base : Str, type_id : U64 }

	## The names a generator declares for one host type or one hosted function
	## when it emits it under one name: the name itself and every name derived
	## from it. `id` tells the declarations of one kind apart: a host type's
	## shape id (see `TypeInfo`) or a hosted function's index.
	##
	## A name that lives in a namespace of its own, such as a struct member,
	## is reported with a prefix no identifier can have, so that it is compared
	## only with the other names of that namespace.
	Declared := { roc_name : Str, id : U64, names : List(Str) }

	PreferredName := { name : Str, type_id : U64 }

	PreferredNames := { entries : List(PreferredName), declared : Set(Str) }.{

		## Every top-level type name the generated file declares apart from
		## its aliases: the reserved names, each host type's declared names,
		## and the names each preferred name claims.
		declared_names : PreferredNames -> Set(Str)
		declared_names = |names| names.declared

		lookup : PreferredNames, U64 -> { found : Bool, name : Str }
		lookup = |names, type_id| {
			var $found = Bool.False
			var $name = ""

			for entry in names.entries {
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

	## Every linker symbol the platform header names: the hosted functions'
	## symbols, then the provided ones.
	platform_symbols : List(HostedFunctionInfo), List(ProvidesEntry) -> List(Str)
	platform_symbols = |hosted_functions, provides_list| {
		var $symbols = []

		for func in hosted_functions {
			$symbols = $symbols.append(func.ffi_symbol)
		}
		for entry in provides_list {
			$symbols = $symbols.append(entry.ffi_symbol)
		}

		$symbols
	}

	## The first of `symbols` that is one of `fixed_names`, as an error: the
	## generated file declares each platform symbol under the symbol's own
	## name, and `fixed_names` are names it declares that host code depends on.
	undeclarable_symbol : List(Str), List(Str) -> Try({}, Str)
	undeclarable_symbol = |symbols, fixed_names| {
		fixed = Set.from_list(fixed_names)

		for symbol in symbols {
			if fixed.contains(symbol) {
				return Err(symbol_error(symbol))
			}
		}

		Ok({})
	}

	## The error for a platform symbol the generated file cannot declare.
	symbol_error : Str -> Str
	symbol_error = |symbol|
		"Glue cannot declare the platform symbol `${symbol}`: the generated bindings, or the language they are written in, already use that name. Give the symbol another name in the platform header."

	## The suffix a name derived from the hosted function or provided entry
	## `name` takes when `renamed` holds that name: `marker` followed by
	## `index`, which nothing else of its kind has.
	index_suffix : List(Str), Str, U64, Str -> Str
	index_suffix = |renamed, name, index, marker|
		if List.contains(renamed, name) {
			"${marker}${U64.to_str(index)}"
		} else {
			""
		}

	## The Roc names emitted under names derived from their ids (a shape id,
	## see `TypeTable.shape_suffix`, or a hosted index, see
	## `index_suffix`): `shared_names`, each of which names several
	## declarations, and every Roc name with a declared name that is also
	## reserved or declared under another id. A name declared twice denotes
	## neither declaration, so both take id-derived names.
	##
	## `declared_for` gives the declarations that result from emitting a given
	## list of Roc names under id-derived names. Renaming one declaration can
	## uncover a collision with another, so the list grows until every
	## declaration is distinct. Two declarations that still collide under
	## their id-derived names have no distinct names to take, which is an
	## error.
	disambiguated : List(Str), List(Str), (List(Str) -> List(Declared)) -> Try(List(Str), Str)
	disambiguated = |reserved, shared_names, declared_for|
		disambiguated_from(Set.from_list(reserved), shared_names, declared_for)

	disambiguated_from : Set(Str), List(Str), (List(Str) -> List(Declared)) -> Try(List(Str), Str)
	disambiguated_from = |reserved, renamed_so_far, declared_for| {
		var $owners = Dict.empty()
		var $colliding = []
		var $unresolved = ""

		for declared in declared_for(renamed_so_far) {
			declared_renamed = List.contains(renamed_so_far, declared.roc_name)

			for name in declared.names {
				if reserved.contains(name) {
					if declared_renamed {
						$unresolved = "Glue cannot name `${declared.roc_name}`: it would be declared as `${name}`, which the generated bindings already declare."
					} else if !List.contains($colliding, declared.roc_name) {
						$colliding = $colliding.append(declared.roc_name)
					}
				} else {
					match $owners.get(name) {
						Ok(owner) =>
							if owner.id != declared.id {
								owner_renamed = List.contains(renamed_so_far, owner.roc_name)

								if owner_renamed and declared_renamed {
									$unresolved = "Glue cannot give `${owner.roc_name}` and `${declared.roc_name}` distinct names: both would be declared as `${name}`."
								}
								if !owner_renamed and !List.contains($colliding, owner.roc_name) {
									$colliding = $colliding.append(owner.roc_name)
								}
								if !declared_renamed and !List.contains($colliding, declared.roc_name) {
									$colliding = $colliding.append(declared.roc_name)
								}
							}
						Err(_) => {
							$owners = $owners.insert(name, { roc_name: declared.roc_name, id: declared.id })
						}
					}
				}
			}
		}

		if !List.is_empty($colliding) {
			disambiguated_from(reserved, List.concat(renamed_so_far, $colliding), declared_for)
		} else if $unresolved != "" {
			Err($unresolved)
		} else {
			Ok(renamed_so_far)
		}
	}

	## `existing_names` are the names already declared: the reserved names and
	## each host type's declared names. `names_declared_as` gives every name a
	## generator declares for a type id when it emits that type under a given
	## name, which is what a preferred name claims.
	preferred_names : TypeNamePlan, List(Str), List(Root), (U64, Str -> List(Str)) -> PreferredNames
	preferred_names = |planner, existing_names, roots, names_declared_as| {
		var $state = { entries: [], seen_names: Set.from_list(existing_names), seen_type_ids: [] }

		for root in roots {
			$state = collect_preferred_for_type_id(
				$state,
				planner.table,
				root.type_id,
				root.alias_base,
				root.module_base,
				[],
				names_declared_as,
			)
		}

		PreferredNames.{ entries: $state.entries, declared: $state.seen_names }
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

	collect_preferred_for_type_id : { entries : List(PreferredName), seen_names : Set(Str), seen_type_ids : List(U64) }, TypeTable, U64, Str, Str, List(U64), (U64, Str -> List(Str)) -> { entries : List(PreferredName), seen_names : Set(Str), seen_type_ids : List(U64) }
	collect_preferred_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids, names_declared_as| {
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
							names_declared_as,
						)
					SingleNoPayload => state
					NotSingleVariant =>
						if tu.name != "" {
							with_union_name =
								if tu.name == "Try" {
									add_preferred_name(state, type_id, "${alias_base}Result", names_declared_as)
								} else if tu.name == "IOErr" {
									add_preferred_name(state, type_id, "${module_base}IOErr", names_declared_as)
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
										names_declared_as,
									)
								}
							}
							$next
						} else {
							state
						}
				}
			RocList(elem_id) => collect_preferred_for_type_id(state, type_table, elem_id, alias_base, module_base, next_visited, names_declared_as)
			RocBox(inner_id) => collect_preferred_for_type_id(state, type_table, inner_id, alias_base, module_base, next_visited, names_declared_as)
			_ => state
		}
	}

	add_preferred_name : { entries : List(PreferredName), seen_names : Set(Str), seen_type_ids : List(U64) }, U64, Str, (U64, Str -> List(Str)) -> { entries : List(PreferredName), seen_names : Set(Str), seen_type_ids : List(U64) }
	add_preferred_name = |state, type_id, name, names_declared_as| {
		if name == "" or List.contains(state.seen_type_ids, type_id) {
			return state
		}

		claimed = names_declared_as(type_id, name)
		if List.any(claimed, |claimed_name| state.seen_names.contains(claimed_name)) {
			return state
		}

		var $seen_names = state.seen_names
		for claimed_name in claimed {
			$seen_names = $seen_names.insert(claimed_name)
		}

		{
			entries: state.entries.append({ type_id, name }),
			seen_names: $seen_names,
			seen_type_ids: state.seen_type_ids.append(type_id),
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

## Declares each sample type as a generator would: under its Roc name, or under
## its shape-derived name when `renamed` holds that Roc name, together with
## one helper named after it.
sample_declared : List({ roc_name : Str, id : U64 }), List(Str) -> List(TypeNamePlan.Declared)
sample_declared = |types, renamed|
	List.map(
		types,
		|sample| {
			name =
				if List.contains(renamed, sample.roc_name) {
					"${sample.roc_name}Type${U64.to_str(sample.id)}"
				} else {
					sample.roc_name
				}

			{ roc_name: sample.roc_name, id: sample.id, names: [name, "${name}Release"] }
		},
	)

## Distinct declarations leave every type under its own name.
expect TypeNamePlan.disambiguated(
	["RocStr"],
	[],
	|renamed| sample_declared([{ roc_name: "Event", id: 0 }, { roc_name: "Key", id: 1 }], renamed),
) == Ok([])

## A type named like another type's helper, and a type named like a reserved
## name, take shape-derived names; so does the type whose helper collided.
expect TypeNamePlan.disambiguated(
	["RocStr"],
	[],
	|renamed| sample_declared(
		[
			{ roc_name: "Event", id: 0 },
			{ roc_name: "EventRelease", id: 1 },
			{ roc_name: "RocStr", id: 2 },
			{ roc_name: "Key", id: 3 },
		],
		renamed,
	),
) == Ok(["Event", "EventRelease", "RocStr"])

## Rows of one shape declare one host type, which does not collide with itself.
expect TypeNamePlan.disambiguated(
	[],
	[],
	|renamed| sample_declared([{ roc_name: "Event", id: 0 }, { roc_name: "Event", id: 0 }], renamed),
) == Ok([])

## Renaming a type can make it collide with a type that had a distinct name.
expect TypeNamePlan.disambiguated(
	[],
	["Wrap"],
	|renamed| sample_declared(
		[
			{ roc_name: "Wrap", id: 0 },
			{ roc_name: "Wrap", id: 1 },
			{ roc_name: "WrapType1", id: 2 },
		],
		renamed,
	),
) == Ok(["Wrap", "WrapType1"])

## Two host types that collide under their shape-derived names are an error.
expect TypeNamePlan.disambiguated(
	[],
	[],
	|_renamed| [
		{ roc_name: "First", id: 0, names: ["Same"] },
		{ roc_name: "Second", id: 1, names: ["Same"] },
	],
) == Err("Glue cannot give `First` and `Second` distinct names: both would be declared as `Same`.")

## A platform symbol named like a fixed name of the generated file is an error.
expect TypeNamePlan.undeclarable_symbol(["roc_main", "decref_box"], ["decref_box", "RocStr"]) == Err("Glue cannot declare the platform symbol `decref_box`: the generated bindings, or the language they are written in, already use that name. Give the symbol another name in the platform header.")

## Symbols that leave the fixed names alone can all be declared.
expect TypeNamePlan.undeclarable_symbol(["roc_main", "roc_host_send"], ["decref_box", "RocStr"]) == Ok({})

## Only a renamed function takes its index-derived suffix.
expect TypeNamePlan.index_suffix(["Host.send_it!"], "Host.send_it!", 3, "Hosted") == "Hosted3"

## Only a renamed function takes its index-derived suffix.
expect TypeNamePlan.index_suffix(["Host.send_it!"], "Host.load!", 1, "Hosted") == ""

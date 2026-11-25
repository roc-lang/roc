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

	PreferredNames := { entries : List(PreferredName), declared : List(Str) }.{

		## Every top-level type name the generated file declares apart from
		## its aliases: the reserved names, each host type's declared names,
		## and the names each preferred name claims.
		declared_names : PreferredNames -> List(Str)
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
		for symbol in symbols {
			if List.contains(fixed_names, symbol) {
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

	## The identifiers of the members of one record or tag union, in the order
	## the members were given. A declaration's members have a namespace of
	## their own, and `to_ident` can give two of them one identifier (`fooBar`
	## and `foo_bar`, or a keyword's escaped spelling and a member spelled that
	## way). An identifier two members share denotes neither, so each takes it
	## followed by `marker` and its rank among the members that share it,
	## ordered by Roc name, and by `marker` again for as long as that is
	## another member's identifier.
	##
	## A member's identifier depends on the Roc names alone, so every place
	## that names a member agrees on it whatever order it lists the members
	## in. `marker` is not empty.
	MemberIdents := { idents : List(Str), ranked : Bool }.{

		## The identifier of the member at `index` of the names given.
		at : MemberIdents, U64 -> Str
		at = |members, index|
			match List.get(members.idents, index) {
				Ok(ident) => ident
				Err(_) => {
					crash "glue invariant violated: a declaration has no member ${U64.to_str(index)}"
				}
			}

		## True when two members shared an identifier, so that some member's
		## identifier holds `marker`.
		any_ranked : MemberIdents -> Bool
		any_ranked = |members| members.ranked
	}

	member_idents : List(Str), (Str -> Str), Str -> MemberIdents
	member_idents = |member_names, to_ident, marker| {
		var $members = []
		var $idents = []
		for name in member_names {
			ident = to_ident(name)
			$idents = $idents.append(ident)
			$members = $members.append({ name, ident, ident_bytes: Str.to_utf8(ident) })
		}

		# Sorted by identifier, the members that share one are neighbors.
		var $sharing = []
		var $previous = { name: "", ident: "", ident_bytes: [] }
		var $previous_shares = Bool.False
		var $first = Bool.True
		for member in List.sort_with($members, |left, right| compare_bytes(left.ident_bytes, right.ident_bytes)) {
			shares = !$first and member.ident == $previous.ident and member.name != $previous.name
			if shares {
				if !$previous_shares {
					$sharing = $sharing.append({ name: $previous.name, ident: $previous.ident, name_bytes: Str.to_utf8($previous.name) })
				}
				$sharing = $sharing.append({ name: member.name, ident: member.ident, name_bytes: Str.to_utf8(member.name) })
			}
			$previous_shares = shares or ($previous_shares and !$first and member.ident == $previous.ident)
			$previous = member
			$first = Bool.False
		}

		if List.is_empty($sharing) {
			return MemberIdents.{ idents: $idents, ranked: Bool.False }
		}

		var $taken = $idents
		var $ranked = []
		for member in List.sort_with($sharing, |left, right| compare_bytes(left.name_bytes, right.name_bytes)) {
			if !List.any($ranked, |settled| settled.name == member.name) {
				rank = List.count_if($ranked, |settled| settled.plain == member.ident)
				ident = first_free($taken, "${member.ident}${marker}${U64.to_str(rank)}", marker)
				$taken = $taken.append(ident)
				$ranked = $ranked.append({ name: member.name, plain: member.ident, ident })
			}
		}

		var $settled = []
		for member in $members {
			$settled = $settled.append(ranked_ident($ranked, member.name, member.ident))
		}

		MemberIdents.{ idents: $settled, ranked: Bool.True }
	}

	## The identifier `ranked` settled for the member `name`, or `plain` when
	## the member shares its identifier with no other.
	ranked_ident : List({ name : Str, plain : Str, ident : Str }), Str, Str -> Str
	ranked_ident = |ranked, name, plain| {
		for settled in ranked {
			if settled.name == name {
				return settled.ident
			}
		}

		plain
	}

	## `candidate`, followed by `marker` for as long as it is in `taken`.
	first_free : List(Str), Str, Str -> Str
	first_free = |taken, candidate, marker|
		if List.contains(taken, candidate) {
			first_free(taken, Str.concat(candidate, marker), marker)
		} else {
			candidate
		}

	## The order of two names by their bytes.
	compare_bytes : List(U8), List(U8) -> [Before, Same, After]
	compare_bytes = |left, right| {
		var $index = 0

		for byte in left {
			match List.get(right, $index) {
				Ok(other) => {
					if byte < other {
						return Before
					}
					if byte > other {
						return After
					}
				}
				Err(_) => {
					return After
				}
			}
			$index = $index + 1
		}

		if List.len(left) < List.len(right) {
			Before
		} else {
			Same
		}
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
	disambiguated = |reserved, shared_names, declared_for| {
		var $reserved_claims = []
		for name in reserved {
			$reserved_claims = $reserved_claims.append({ name, bytes: Str.to_utf8(name), roc_name: "", id: 0, reserved: Bool.True })
		}

		disambiguated_from($reserved_claims, shared_names, declared_for)
	}

	## One declaration of one name: reserved, or made for the Roc name
	## `roc_name` under `id`.
	Claim : { name : Str, bytes : List(U8), roc_name : Str, id : U64, reserved : Bool }

	disambiguated_from : List(Claim), List(Str), (List(Str) -> List(Declared)) -> Try(List(Str), Str)
	disambiguated_from = |reserved_claims, renamed_so_far, declared_for| {
		var $claims = reserved_claims
		for declared in declared_for(renamed_so_far) {
			for name in declared.names {
				$claims = $claims.append({ name, bytes: Str.to_utf8(name), roc_name: declared.roc_name, id: declared.id, reserved: Bool.False })
			}
		}

		var $colliding = []
		var $unresolved = ""
		var $previous = { name: "", bytes: [], roc_name: "", id: 0, reserved: Bool.True }
		var $first = Bool.True

		# Sorted by name, the claims of one name are neighbors, and every claim
		# of a name with two claimants has a neighbor from another claimant.
		for claim in List.sort_with($claims, |left, right| compare_bytes(left.bytes, right.bytes)) {
			if !$first and claim.name == $previous.name {
				if claim.reserved != $previous.reserved {
					declared = if (claim.reserved) $previous else claim
					if List.contains(renamed_so_far, declared.roc_name) {
						$unresolved = "Glue cannot name `${declared.roc_name}`: it would be declared as `${declared.name}`, which the generated bindings already declare."
					} else if !List.contains($colliding, declared.roc_name) {
						$colliding = $colliding.append(declared.roc_name)
					}
				} else if !claim.reserved and (claim.id != $previous.id or claim.roc_name != $previous.roc_name) {
					previous_renamed = List.contains(renamed_so_far, $previous.roc_name)
					claim_renamed = List.contains(renamed_so_far, claim.roc_name)

					if previous_renamed and claim_renamed {
						$unresolved = "Glue cannot give `${$previous.roc_name}` and `${claim.roc_name}` distinct names: both would be declared as `${claim.name}`."
					}
					if !previous_renamed and !List.contains($colliding, $previous.roc_name) {
						$colliding = $colliding.append($previous.roc_name)
					}
					if !claim_renamed and !List.contains($colliding, claim.roc_name) {
						$colliding = $colliding.append(claim.roc_name)
					}
				}
			}
			$previous = claim
			$first = Bool.False
		}

		if !List.is_empty($colliding) {
			disambiguated_from(reserved_claims, List.concat(renamed_so_far, $colliding), declared_for)
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
		var $state = { entries: [], seen_names: existing_names, seen_type_ids: [] }

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

	collect_preferred_for_type_id : { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }, TypeTable, U64, Str, Str, List(U64), (U64, Str -> List(Str)) -> { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }
	collect_preferred_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids, names_declared_as| {
		if List.contains(visited_type_ids, type_id) {
			return state
		}

		# A zero-sized type has no host declaration to name.
		if type_table.is_zero_sized(type_id) {
			return state
		}

		next_visited = visited_type_ids.append(type_id)

		type_repr = type_table.get(type_id)
		match type_repr {
			RocTagUnion(tu) =>
				match type_table.single_variant_payload(type_id, tu) {
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
					SinglePayloadStruct(tag_layout) => {
						child_base = "${alias_base}${RocName.capitalize_first(tag_layout.name)}"
						var $next = state
						for payload_id in tag_layout.payload {
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
						$next
					}
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

	add_preferred_name : { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }, U64, Str, (U64, Str -> List(Str)) -> { entries : List(PreferredName), seen_names : List(Str), seen_type_ids : List(U64) }
	add_preferred_name = |state, type_id, name, names_declared_as| {
		if name == "" or List.contains(state.seen_type_ids, type_id) {
			return state
		}

		claimed = names_declared_as(type_id, name)
		if List.any(claimed, |claimed_name| List.contains(state.seen_names, claimed_name)) {
			return state
		}

		{
			entries: state.entries.append({ type_id, name }),
			seen_names: List.concat(state.seen_names, claimed),
			seen_type_ids: state.seen_type_ids.append(type_id),
		}
	}

	collect_aliases_for_type_id : { entries : List(AliasPlan), seen_aliases : List(Str) }, TypeTable, U64, Str, Str, List(U64) -> { entries : List(AliasPlan), seen_aliases : List(Str) }
	collect_aliases_for_type_id = |state, type_table, type_id, alias_base, module_base, visited_type_ids| {
		if List.contains(visited_type_ids, type_id) {
			return state
		}

		# A zero-sized type has no host declaration to name.
		if type_table.is_zero_sized(type_id) {
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
				match type_table.single_variant_payload(type_id, tu) {
					SinglePayload(payload_id) =>
						collect_aliases_for_type_id(
							state,
							type_table,
							payload_id,
							alias_base,
							module_base,
							next_visited,
						)
					SinglePayloadStruct(tag_layout) => {
						child_base = "${alias_base}${RocName.capitalize_first(tag_layout.name)}"
						var $next = add_alias(state, alias_base, type_id, PlainAlias)
						for payload_id in tag_layout.payload {
							$next = collect_aliases_for_type_id(
								$next,
								type_table,
								payload_id,
								child_base,
								module_base,
								next_visited,
							)
						}
						$next
					}
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

## A member whose identifier no sibling shares keeps it.
expect TypeNamePlan.member_idents(["fooBar", "other"], RocName.lower_snake_ascii, "_Member").at(0) == "foo_bar"

## Members that share an identifier are ranked by Roc name, in any order.
expect TypeNamePlan.member_idents(["foo_bar", "other", "fooBar"], RocName.lower_snake_ascii, "_Member").at(2) == "foo_bar_Member0"

## Members that share an identifier are ranked by Roc name, in any order.
expect TypeNamePlan.member_idents(["fooBar", "foo_bar"], RocName.lower_snake_ascii, "_Member").at(1) == "foo_bar_Member1"

## A name that another name extends sorts first.
expect TypeNamePlan.compare_bytes(Str.to_utf8("foo"), Str.to_utf8("foo_bar")) == Before

## A name that another name extends sorts first.
expect TypeNamePlan.compare_bytes(Str.to_utf8("foo_bar"), Str.to_utf8("foo")) == After

## A name sorts with itself.
expect TypeNamePlan.compare_bytes(Str.to_utf8("foo"), Str.to_utf8("foo")) == Same

## A ranked identifier that is another member's own takes the marker again.
expect TypeNamePlan.member_idents(["Self", "Self_", "Self__0"], |tag| if (tag == "Self") "Self_" else tag, "_").at(0) == "Self__0_"

## A ranked identifier that is another member's own takes the marker again.
expect TypeNamePlan.member_idents(["Self", "Self_", "Self__0"], |tag| if (tag == "Self") "Self_" else tag, "_").at(1) == "Self__1"

## A member whose identifier is its own keeps it beside ranked ones.
expect TypeNamePlan.member_idents(["Self", "Self_", "Self__0"], |tag| if (tag == "Self") "Self_" else tag, "_").at(2) == "Self__0"

## Members that share no identifier are not ranked.
expect !TypeNamePlan.member_idents(["fooBar", "other"], RocName.lower_snake_ascii, "_Member").any_ranked()

## Members that share an identifier are ranked.
expect TypeNamePlan.member_idents(["fooBar", "foo_bar"], RocName.lower_snake_ascii, "_Member").any_ranked()

## A glue script for generating a Rust source file with hosted function bindings.
app [make_glue] { pf: platform glue }

import pf.Types exposing [Types]
import pf.File exposing [File]
import pf.RecordFieldInfo exposing [RecordFieldInfo]
import pf.TypeRepr exposing [TypeRepr]
import pf.AbiLayout exposing [AbiLayout]
import pf.AbiFieldLayout exposing [AbiFieldLayout]
import pf.AbiTagLayout exposing [AbiTagLayout]
import pf.AbiWidth exposing [AbiWidth]
import pf.ArgShape exposing [ArgShape]
import pf.GlueInput exposing [GlueInput]
import pf.HostedFunctionInfo exposing [HostedFunctionInfo]
import pf.TypeNamePlan exposing [TypeNamePlan]
import pf.FunctionRepr exposing [FunctionRepr]
import pf.RecordRepr exposing [RecordRepr]
import pf.TagUnionRepr exposing [TagUnionRepr]
import pf.RecordField exposing [RecordField]
import pf.TagVariant exposing [TagVariant]
import pf.ProvidesEntry exposing [ProvidesEntry]
import pf.TypeInfo exposing [TypeInfo]
import pf.TypeTable exposing [TypeTable]
import pf.RocName exposing [RocName]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	input = GlueInput.from_types(types_list)
	type_table = TypeTable.from_list(input.types)
	rust_content = generate_rust_file(input.hosted_functions, type_table, input.provides_entries)

	Ok([{ name: "roc_platform_abi.rs", content: rust_content }])
}

# =============================================================================
# TypeRepr-based Type Mapping
# =============================================================================

## Map a type table entry to its Rust type string using structured TypeRepr
type_id_to_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64 -> Str
type_id_to_rust = |type_table, duplicate_names, preferred_names, type_id| {
	type_repr = type_table.get(type_id)
	match type_repr {
		RocRecord(rec) =>
			if rec.name == "" {
				"*mut c_void"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type_rust(type_table, duplicate_names, preferred_names, type_id, tu)
		_ => type_repr_to_rust(type_table, duplicate_names, preferred_names, type_id, type_repr)
	}
}

## Render one struct field declaration for a record field. Unnamed
## nominal-record padding fields become fixed-size byte arrays (`[u8; size]`);
## named fields use their resolved Rust type.
rust_record_field_decl : TypeTable, List(Str), TypeNamePlan.PreferredNames, AbiFieldLayout, AbiWidth -> Str
rust_record_field_decl = |type_table, duplicate_names, preferred_names, field, width| {
	field_name = name_to_rust_field_ident(field.name)
	rust_type = if field.is_padding {
		"[u8; ${U64.to_str(AbiFieldLayout.size(field, width))}]"
	} else {
		type_id_to_rust(type_table, duplicate_names, preferred_names, field.type_id)
	}
	"    pub ${field_name}: ${rust_type},\n"
}

## Fields arrive in committed layout order (valid at both pointer widths);
## only per-width padding byte counts differ between the two renderings.
rust_record_fields_decl : TypeTable, List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout), AbiWidth -> Str
rust_record_fields_decl = |type_table, duplicate_names, preferred_names, fields, width| {
	var $field_strs = ""
	for field in fields {
		$field_strs = Str.concat($field_strs, rust_record_field_decl(type_table, duplicate_names, preferred_names, field, width))
	}
	$field_strs
}

rust_size_align_assertions : Str, U64, U64, U64, U64 -> Str
rust_size_align_assertions = |type_name, size_32, alignment_32, size_64, alignment_64| {
	if size_32 > 0 or size_64 > 0 {
		"#[cfg(target_pointer_width = \"32\")]\nconst _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(size_32)}, \"${type_name} size mismatch\");\n#[cfg(target_pointer_width = \"32\")]\nconst _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(alignment_32)}, \"${type_name} alignment mismatch\");\n#[cfg(target_pointer_width = \"64\")]\nconst _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(size_64)}, \"${type_name} size mismatch\");\n#[cfg(target_pointer_width = \"64\")]\nconst _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(alignment_64)}, \"${type_name} alignment mismatch\");\n\n"
	} else {
		""
	}
}

## Convert a TypeRepr to its Rust type string
type_repr_to_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr -> Str
type_repr_to_rust = |type_table, duplicate_names, preferred_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "RocErasedCallable"
				RocUnknown(_) => "RocBox"
				_ => {
					inner_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, inner_id)
					if inner_rust == "*mut c_void" {
						"RocBox"
					} else {
						"*mut ${inner_rust}"
					}
				}
			}
		RocStr => "RocStr"
		RocUnit => "()"
		RocU8 => "u8"
		RocU16 => "u16"
		RocU32 => "u32"
		RocU64 => "u64"
		RocU128 => "u128"
		RocI8 => "i8"
		RocI16 => "i16"
		RocI32 => "i32"
		RocI64 => "i64"
		RocI128 => "i128"
		RocF32 => "f32"
		RocF64 => "f64"
		RocDec => "RocDec"
		RocList(elem_id) =>
			if is_type_refcounted(type_table, elem_id) {
				"RocList<${type_id_to_rust(type_table, duplicate_names, preferred_names, elem_id)}>"
			} else {
				"RocListWith<${type_id_to_rust(type_table, duplicate_names, preferred_names, elem_id)}, false>"
			}
		RocRecord(rec) =>
			if rec.name == "" {
				"*mut c_void"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type_rust(type_table, duplicate_names, preferred_names, type_id, tu)
		RocFunction(_) => "*mut c_void"
		RocUnknown(_) => "*mut c_void"
	}
}

## Resolve a tag union to a Rust type. Single-variant unions are unwrapped to their payload.
## Multi-variant unions with a name return a generated struct name.
resolve_tag_union_type_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr -> Str
resolve_tag_union_type_rust = |type_table, duplicate_names, preferred_names, type_id, tu|
	match TypeTable.single_variant_payload(tu) {
		SinglePayload(payload_id) => type_id_to_rust(type_table, duplicate_names, preferred_names, payload_id)
		SingleNoPayload => "*mut c_void"
		NotSingleVariant =>
			if tu.name != "" {
				tag_union_struct_name(preferred_names, duplicate_names, type_id, tu)
			} else {
				"*mut c_void"
			}
	}

## Determine whether a type is refcounted (heap-allocated).
## Refcounted types need 2*ptr_width header space in list allocations.
is_type_refcounted : TypeTable, U64 -> Bool
is_type_refcounted = |type_table, type_id| type_table.is_refcounted(type_id)

is_repr_refcounted : TypeTable, TypeRepr -> Bool
is_repr_refcounted = |type_table, type_repr| type_table.repr_is_refcounted(type_repr)

type_table_entries : TypeTable -> List(TypeInfo)
type_table_entries = |type_table| type_table.entries()

abi_record_fields : AbiLayout -> List(AbiFieldLayout)
abi_record_fields = |abi_layout| abi_layout.record_fields()

abi_tag_layouts : AbiLayout -> List(AbiTagLayout)
abi_tag_layouts = |abi_layout| abi_layout.tag_layouts()

abi_discriminant_offset : AbiLayout, AbiWidth -> U64
abi_discriminant_offset = |abi_layout, width| abi_layout.discriminant_offset(width)

abi_discriminant_size : AbiLayout -> U64
abi_discriminant_size = |abi_layout| abi_layout.discriminant_size()

# =============================================================================
# String Utilities
# =============================================================================

## Replace all occurrences of a substring
str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| RocName.replace_all(s, from, to)

## Checks `str_replace_all` for this representative case.
expect str_replace_all("a.b.c", ".", "_") == "a_b_c"
## Checks `str_replace_all` for this representative case.
expect str_replace_all("hello!", "!", "") == "hello"

## Convert a string to lower_snake_case
to_lower_snake_case : Str -> Str
to_lower_snake_case = |s| RocName.lower_snake_ascii(s)

## Checks `to_lower_snake_case` for this representative case.
expect to_lower_snake_case("FooBar") == "foo_bar"
## Checks `to_lower_snake_case` for this representative case.
expect to_lower_snake_case("fooBar") == "foo_bar"
## Checks `to_lower_snake_case` for this representative case.
expect to_lower_snake_case("foo") == "foo"
## Checks `to_lower_snake_case` for this representative case.
expect to_lower_snake_case("FOO") == "foo"
## Checks `to_lower_snake_case` for this representative case.
expect to_lower_snake_case("Stdout_line") == "stdout_line"

## Convert a string to SCREAMING_SNAKE_CASE (for Rust constants)
to_screaming_snake_case : Str -> Str
to_screaming_snake_case = |s| RocName.screaming_snake_ascii(s)

## Checks `to_screaming_snake_case` for this representative case.
expect to_screaming_snake_case("FooBar") == "FOO_BAR"
## Checks `to_screaming_snake_case` for this representative case.
expect to_screaming_snake_case("fooBar") == "FOO_BAR"
## Checks `to_screaming_snake_case` for this representative case.
expect to_screaming_snake_case("foo") == "FOO"
## Checks `to_screaming_snake_case` for this representative case.
expect to_screaming_snake_case("FOO") == "FOO"
## Checks `to_screaming_snake_case` for this representative case.
expect to_screaming_snake_case("Stdout_line") == "STDOUT_LINE"

## Capitalize the first character of a string
capitalize_first : Str -> Str
capitalize_first = |s| RocName.capitalize_first(s)

## Checks `capitalize_first` for this representative case.
expect capitalize_first("hello") == "Hello"
## Checks `capitalize_first` for this representative case.
expect capitalize_first("Hello") == "Hello"
## Checks `capitalize_first` for this representative case.
expect capitalize_first("") == ""

## Lowercase the first character of a string
lowercase_first : Str -> Str
lowercase_first = |s| RocName.lowercase_first(s)

## Checks `lowercase_first` for this representative case.
expect lowercase_first("Hello") == "hello"
## Checks `lowercase_first` for this representative case.
expect lowercase_first("hello") == "hello"
## Checks `lowercase_first` for this representative case.
expect lowercase_first("") == ""

# =============================================================================
# Name Conversion
# =============================================================================

## Convert function name to PascalCase struct name (e.g., "Stdout.line!" -> "StdoutLine")
name_to_struct_name : Str -> Str
name_to_struct_name = |name| RocName.from_str(name).to_pascal_clean()

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Stdout.line!") == "StdoutLine"
## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("line!") == "Line"
## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"
## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Builder.print_value!") == "BuilderPrintValue"
## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("__AnonStruct10") == "AnonStruct10"

## Find named multi-variant tag unions whose Roc name appears more than once in
## the type table. The result is computed once per glue run and reused while
## rendering type names.
duplicate_tag_union_names : TypeTable -> List(Str)
duplicate_tag_union_names = |type_table| type_table.duplicate_tag_union_names()

## Return the default Rust struct name for a multi-variant tag union.
##
## Generic tag unions used heavily by hosted functions can appear many times in
## the type table with different payload layouts. Rust needs a distinct concrete
## type for each layout.
default_tag_union_struct_name : List(Str), U64, TagUnionRepr -> Str
default_tag_union_struct_name = |duplicate_names, type_id, tu| {
	base = name_to_struct_name(tu.name)

	if List.contains(duplicate_names, tu.name) {
		"${base}Type${U64.to_str(type_id)}"
	} else {
		base
	}
}

## Return the emitted Rust struct name for a multi-variant tag union.
tag_union_struct_name : TypeNamePlan.PreferredNames, List(Str), U64, TagUnionRepr -> Str
tag_union_struct_name = |preferred_names, duplicate_names, type_id, tu| {
	preferred = preferred_names.lookup(type_id)
	if preferred.found {
		preferred.name
	} else {
		default_tag_union_struct_name(duplicate_names, type_id, tu)
	}
}

hosted_module_name_to_struct_name : Str -> Str
hosted_module_name_to_struct_name = |name|
	match List.first(Str.split_on(name, ".")) {
		Ok(module_name) => name_to_struct_name(module_name)
		Err(_) => {
			crash "glue invariant violated: module name split produced no segments"
		}
	}

generated_type_names_rust : TypeTable, List(Str) -> List(Str)
generated_type_names_rust = |type_table, duplicate_names| {
	var $names = []
	var $type_id = 0

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					$names = $names.append(name_to_struct_name(rec.name))
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					struct_name = default_tag_union_struct_name(duplicate_names, $type_id, tu)
					$names = $names.append(struct_name)
					if TypeTable.tag_union_has_payload(tu) {
						$names = $names.append("${struct_name}Tag")
						$names = $names.append("${struct_name}Payload")
						for tag in tu.tags {
							if List.len(tag.payload) > 1 {
								$names = $names.append("${struct_name}${capitalize_first(tag.name)}Payload")
							}
						}
					}
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	$names
}

type_name_root_alias_base_rust : TypeTable, Str, U64 -> Str
type_name_root_alias_base_rust = |type_table, fallback, type_id|
	match type_table.get(type_id) {
		RocRecord(rec) =>
			if rec.name != "" and !rec.anonymous {
				name_to_struct_name(rec.name)
			} else {
				fallback
			}
		RocTagUnion(tu) =>
			if tu.name != "" and tu.name != "Try" and tu.name != "IOErr" {
				name_to_struct_name(tu.name)
			} else {
				fallback
			}
		_ => fallback
	}

record_alias_fields_rust : TypeTable, RecordRepr -> List(RecordField)
record_alias_fields_rust = |type_table, rec| {
	var $fields = rec.fields
	var $found = Bool.False

	if rec.name != "" and !rec.anonymous {
		for type_info in type_table.entries() {
			match type_info.repr {
				RocRecord(candidate) =>
					if !$found and candidate.name == rec.name {
						$fields = candidate.fields
						$found = Bool.True
					}
				_ => {}
			}
		}
	}

	$fields
}

type_name_roots_rust : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable -> List(TypeNamePlan.Root)
type_name_roots_rust = |hosted_functions, provides_list, type_table| {
	var $roots = []

	for func in hosted_functions {
		base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = $roots.append(
				{
					alias_base: type_name_root_alias_base_rust(type_table, arg_fallback, arg_type_id),
					module_base,
					type_id: arg_type_id,
				},
			)
			$arg_idx = $arg_idx + 1
		}

		$roots = $roots.append(
			{
				alias_base: base,
				module_base,
				type_id: func.ret_type_id,
			},
		)
	}

	for entry in provides_list {
		base = name_to_struct_name(entry.name)
		module_base = hosted_module_name_to_struct_name(entry.name)

		match type_table.get(entry.type_id) {
			RocFunction(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = $roots.append(
						{
							alias_base: type_name_root_alias_base_rust(type_table, arg_fallback, arg_type_id),
							module_base,
							type_id: arg_type_id,
						},
					)
					$arg_idx = $arg_idx + 1
				}

				$roots = $roots.append(
					{
						alias_base: base,
						module_base,
						type_id: func.ret,
					},
				)
			}
			_ => {
				$roots = $roots.append(
					{
						alias_base: type_name_root_alias_base_rust(type_table, base, entry.type_id),
						module_base,
						type_id: entry.type_id,
					},
				)
			}
		}
	}

	$roots
}

append_type_alias_roots_rust : List(TypeNamePlan.Root), TypeTable, Str, Str, U64, List(U64) -> List(TypeNamePlan.Root)
append_type_alias_roots_rust = |roots, type_table, alias_base, module_base, type_id, visited_type_ids| {
	if List.contains(visited_type_ids, type_id) {
		return roots
	}

	next_visited = visited_type_ids.append(type_id)
	root_alias_base = type_name_root_alias_base_rust(type_table, alias_base, type_id)

	var $roots = roots.append({ alias_base: root_alias_base, module_base, type_id })

	match type_table.get(type_id) {
		RocRecord(rec) => {
			for field in record_alias_fields_rust(type_table, rec) {
				field_base = "${root_alias_base}${RocName.from_str(field.name).to_pascal_clean()}"
				$roots = append_type_alias_roots_rust($roots, type_table, field_base, module_base, field.type_id, next_visited)
			}
			$roots
		}
		RocList(elem_id) => append_type_alias_roots_rust($roots, type_table, root_alias_base, module_base, elem_id, next_visited)
		RocBox(inner_id) => append_type_alias_roots_rust($roots, type_table, root_alias_base, module_base, inner_id, next_visited)
		RocTagUnion(tu) => {
			var $next = $roots
			for tag in tu.tags {
				child_base = "${root_alias_base}${RocName.capitalize_first(tag.name)}"
				for payload_id in tag.payload {
					$next = append_type_alias_roots_rust($next, type_table, child_base, module_base, payload_id, next_visited)
				}
			}
			$next
		}
		_ => $roots
	}
}

type_alias_roots_rust : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable -> List(TypeNamePlan.Root)
type_alias_roots_rust = |hosted_functions, provides_list, type_table| {
	var $roots = []

	for func in hosted_functions {
		base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = append_type_alias_roots_rust($roots, type_table, arg_fallback, module_base, arg_type_id, [])
			$arg_idx = $arg_idx + 1
		}

		$roots = append_type_alias_roots_rust($roots, type_table, base, module_base, func.ret_type_id, [])
	}

	for entry in provides_list {
		base = name_to_struct_name(entry.name)
		module_base = hosted_module_name_to_struct_name(entry.name)

		match type_table.get(entry.type_id) {
			RocFunction(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = append_type_alias_roots_rust($roots, type_table, arg_fallback, module_base, arg_type_id, [])
					$arg_idx = $arg_idx + 1
				}

				$roots = append_type_alias_roots_rust($roots, type_table, base, module_base, func.ret, [])
			}
			_ => {
				$roots = append_type_alias_roots_rust($roots, type_table, base, module_base, entry.type_id, [])
			}
		}
	}

	$roots
}

preferred_type_names_rust : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> TypeNamePlan.PreferredNames
preferred_type_names_rust = |hosted_functions, provides_list, type_table, duplicate_names|
	TypeNamePlan.from_table(type_table).preferred_names(
		generated_type_names_rust(type_table, duplicate_names),
		type_name_roots_rust(hosted_functions, provides_list, type_table),
	)

## RustGlue must keep distinct concrete structs for generic Roc types such as
## `Try` because different hosted functions can use different payload layouts.
## These aliases give platform authors stable API names for secondary names that
## share a concrete layout.
add_type_alias_rust : { content : Str, seen : List(Str) }, Str, Str -> { content : Str, seen : List(Str) }
add_type_alias_rust = |state, alias, target| {
	if alias == target or List.contains(state.seen, alias) {
		state
	} else {
		{
			content: Str.concat(state.content, "pub type ${alias} = ${target};\n"),
			seen: state.seen.append(alias),
		}
	}
}

tag_union_has_payload_rust : TagUnionRepr -> Bool
tag_union_has_payload_rust = |tu| TypeTable.tag_union_has_payload(tu)

add_tag_union_aliases_rust : { content : Str, seen : List(Str) }, Str, Str, TagUnionRepr -> { content : Str, seen : List(Str) }
add_tag_union_aliases_rust = |state, alias, target, tu| {
	with_main_alias = add_type_alias_rust(state, alias, target)

	if tag_union_has_payload_rust(tu) {
		with_payload_alias = add_type_alias_rust(with_main_alias, "${alias}Payload", "${target}Payload")
		add_type_alias_rust(with_payload_alias, "${alias}Tag", "${target}Tag")
	} else {
		with_main_alias
	}
}

generate_platform_type_aliases_rust : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_platform_type_aliases_rust = |hosted_functions, provides_list, type_table, duplicate_names, preferred_names| {
	var $state = { content: "", seen: generated_type_names_rust(type_table, duplicate_names) }
	name_plan = TypeNamePlan.from_table(type_table)

	for plan in name_plan.alias_plan(type_alias_roots_rust(hosted_functions, provides_list, type_table)) {
		target = type_id_to_rust(type_table, duplicate_names, preferred_names, plan.type_id)
		$state = match plan.kind {
			PlainAlias => add_type_alias_rust($state, plan.alias, target)
			TagUnionAlias =>
				match type_table.get(plan.type_id) {
					RocTagUnion(tu) => add_tag_union_aliases_rust($state, plan.alias, target, tu)
					_ => add_type_alias_rust($state, plan.alias, target)
				}
			}
	}

	if $state.content == "" {
		""
	} else {
		"// Platform Type Aliases\n\n${$state.content}\n"
	}
}

## Convert function name to snake_case (e.g., "Stdout.line!" -> "stdout_line")
name_to_snake : Str -> Str
name_to_snake = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_lower_snake_case()
}

## Checks `name_to_snake` for this representative case.
expect name_to_snake("Stdout.line!") == "stdout_line"
## Checks `name_to_snake` for this representative case.
expect name_to_snake("line!") == "line"
## Checks `name_to_snake` for this representative case.
expect name_to_snake("Foo.barBaz!") == "foo_bar_baz"
## Checks `name_to_snake` for this representative case.
expect name_to_snake("PartDef.Idx.get!") == "part_def_idx_get"

## Convert a Roc name to a Rust snake_case function suffix.
name_to_rust_fn_suffix : Str -> Str
name_to_rust_fn_suffix = |name| {
	suffix =
		RocName.from_str(name).to_lower_snake_identifier()
			->RocName.strip_leading_underscores()

	if suffix == "" or suffix == "_" {
		"anon"
	} else {
		suffix
	}
}

## Checks `name_to_rust_fn_suffix` for this representative case.
expect name_to_rust_fn_suffix("Host.Tree") == "host_tree"
## Checks `name_to_rust_fn_suffix` for this representative case.
expect name_to_rust_fn_suffix("TryType17") == "try_type17"
## Checks `name_to_rust_fn_suffix` for this representative case.
expect name_to_rust_fn_suffix("__AnonStruct10") == "anon_struct10"

## Convert function name to SCREAMING_SNAKE_CASE (e.g., "Stdout.line!" -> "STDOUT_LINE")
name_to_screaming_snake : Str -> Str
name_to_screaming_snake = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_screaming_snake_case()
}

## Checks `name_to_screaming_snake` for this representative case.
expect name_to_screaming_snake("Stdout.line!") == "STDOUT_LINE"
## Checks `name_to_screaming_snake` for this representative case.
expect name_to_screaming_snake("line!") == "LINE"
## Checks `name_to_screaming_snake` for this representative case.
expect name_to_screaming_snake("Foo.barBaz!") == "FOO_BAR_BAZ"
## Checks `name_to_screaming_snake` for this representative case.
expect name_to_screaming_snake("PartDef.Idx.get!") == "PART_DEF_IDX_GET"

## Convert a Roc record field name to a valid Rust field identifier.
## Rust cannot quote arbitrary punctuation in identifiers, so ABI structs use
## sanitized field names while preserving Roc's field order and repr(C) layout.
name_to_rust_field_ident : Str -> Str
name_to_rust_field_ident = |name| {
	sanitized =
		RocName.from_str(name).to_bang_snake_identifier()

	match sanitized {
		"" => "_field"
		"_" => "_field"
		"as" => "r#as"
		"break" => "r#break"
		"const" => "r#const"
		"continue" => "r#continue"
		"crate" => "r#crate"
		"else" => "r#else"
		"enum" => "r#enum"
		"extern" => "r#extern"
		"false" => "r#false"
		"fn" => "r#fn"
		"for" => "r#for"
		"if" => "r#if"
		"impl" => "r#impl"
		"in" => "r#in"
		"let" => "r#let"
		"loop" => "r#loop"
		"match" => "r#match"
		"mod" => "r#mod"
		"move" => "r#move"
		"mut" => "r#mut"
		"pub" => "r#pub"
		"ref" => "r#ref"
		"return" => "r#return"
		"self" => "self_"
		"Self" => "self_type"
		"static" => "r#static"
		"struct" => "r#struct"
		"super" => "super_"
		"trait" => "r#trait"
		"true" => "r#true"
		"type" => "r#type"
		"unsafe" => "r#unsafe"
		"use" => "r#use"
		"where" => "r#where"
		"while" => "r#while"
		_ => sanitized
	}
}

## Checks `name_to_rust_field_ident` for this representative case.
expect name_to_rust_field_ident("init!") == "init_bang"
## Checks `name_to_rust_field_ident` for this representative case.
expect name_to_rust_field_ident("render!") == "render_bang"
## Checks `name_to_rust_field_ident` for this representative case.
expect name_to_rust_field_ident("type") == "r#type"
## Checks `name_to_rust_field_ident` for this representative case.
expect name_to_rust_field_ident("_") == "_field"

## Return the Rust discriminant type for a committed discriminant size.
disc_type_for_size : U64 -> Str
disc_type_for_size = |size|
	if size <= 1 {
		"u8"
	} else if size == 2 {
		"u16"
	} else if size == 4 {
		"u32"
	} else {
		"u64"
	}

# =============================================================================
# Type Table Helpers
# =============================================================================

abi_tag_at : List(AbiTagLayout), U64 -> AbiTagLayout
abi_tag_at = |abi_tags, index| {
	match List.get(abi_tags, index) {
		Ok(tag) => tag
		Err(_) => {
			crash "glue invariant violated: missing ABI tag layout at index ${U64.to_str(index)}"
		}
	}
}

abi_tag_has_payload : AbiTagLayout -> Bool
abi_tag_has_payload = |tag| tag.payload_size32 > 0 or tag.payload_size64 > 0

rust_layout_assertions : Str, AbiLayout -> Str
rust_layout_assertions = |type_name, abi_layout| {
	if abi_layout.size64 > 0 or abi_layout.size32 > 0 {
		block =
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(abi_layout.size64)}, "${type_name} size mismatch");
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(abi_layout.alignment64)}, "${type_name} alignment mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(abi_layout.size32)}, "${type_name} size mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(abi_layout.alignment32)}, "${type_name} alignment mismatch");
		"${block}\n\n"
	} else {
		""
	}
}

rust_tag_union_layout_assertions : Str, AbiLayout -> Str
rust_tag_union_layout_assertions = |type_name, abi_layout| {
	if abi_layout.size64 > 0 or abi_layout.size32 > 0 {
		block =
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(abi_layout.size64)}, "${type_name} size mismatch");
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(abi_layout.alignment64)}, "${type_name} alignment mismatch");
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::offset_of!(${type_name}, tag) == ${U64.to_str(abi_discriminant_offset(abi_layout, Pointer64))}, "${type_name} tag offset mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(abi_layout.size32)}, "${type_name} size mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(abi_layout.alignment32)}, "${type_name} alignment mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::offset_of!(${type_name}, tag) == ${U64.to_str(abi_discriminant_offset(abi_layout, Pointer32))}, "${type_name} tag offset mismatch");
		"${block}\n\n"
	} else {
		""
	}
}

rust_payload_layout_assertions : Str, AbiTagLayout -> Str
rust_payload_layout_assertions = |type_name, tag_layout| {
	if tag_layout.payload_size64 > 0 or tag_layout.payload_size32 > 0 {
		block =
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(tag_layout.payload_size64)}, "${type_name} size mismatch");
			\\#[cfg(target_pointer_width = "64")]
			\\const _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(tag_layout.payload_alignment64)}, "${type_name} alignment mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::size_of::<${type_name}>() == ${U64.to_str(tag_layout.payload_size32)}, "${type_name} size mismatch");
			\\#[cfg(target_pointer_width = "32")]
			\\const _: () = assert!(core::mem::align_of::<${type_name}>() == ${U64.to_str(tag_layout.payload_alignment32)}, "${type_name} alignment mismatch");
		"${block}\n\n"
	} else {
		""
	}
}

generate_record_struct_decl_rust : Str, Str, TypeTable, List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout), Bool, AbiLayout -> Str
generate_record_struct_decl_rust = |doc, struct_name, type_table, duplicate_names, preferred_names, fields, _anonymous, abi_layout| {
	field_strs64 = rust_record_fields_decl(type_table, duplicate_names, preferred_names, fields, Pointer64)
	field_strs32 = rust_record_fields_decl(type_table, duplicate_names, preferred_names, fields, Pointer32)
	assertions = rust_layout_assertions(struct_name, abi_layout)

	struct_decl =
		\\${doc}#[cfg(target_pointer_width = "32")]
		\\#[repr(C)]
		\\#[derive(Clone, Copy)]
		\\pub struct ${struct_name} {
		\\${field_strs32}}
		\\
		\\${doc}#[cfg(not(target_pointer_width = "32"))]
		\\#[repr(C)]
		\\#[derive(Clone, Copy)]
		\\pub struct ${struct_name} {
		\\${field_strs64}}

	"${struct_decl}\n\n${assertions}"
}

generate_payload_struct_decl_rust : Str, Str, TypeTable, List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout), AbiTagLayout -> Str
generate_payload_struct_decl_rust = |doc, struct_name, type_table, duplicate_names, preferred_names, fields, tag_layout| {
	field_strs64 = rust_record_fields_decl(type_table, duplicate_names, preferred_names, fields, Pointer64)
	field_strs32 = rust_record_fields_decl(type_table, duplicate_names, preferred_names, fields, Pointer32)
	assertions = rust_payload_layout_assertions(struct_name, tag_layout)

	struct_decl =
		\\${doc}#[cfg(target_pointer_width = "32")]
		\\#[repr(C)]
		\\#[derive(Clone, Copy)]
		\\pub struct ${struct_name} {
		\\${field_strs32}}
		\\
		\\${doc}#[cfg(not(target_pointer_width = "32"))]
		\\#[repr(C)]
		\\#[derive(Clone, Copy)]
		\\pub struct ${struct_name} {
		\\${field_strs64}}

	"${struct_decl}\n\n${assertions}"
}

# =============================================================================
# Rust Code Generation
# =============================================================================

## Generate the complete Rust source file
generate_rust_file : List(HostedFunctionInfo), TypeTable, List(ProvidesEntry) -> Str
generate_rust_file = |hosted_functions, type_table, provides_list| {
	duplicate_names = duplicate_tag_union_names(type_table)
	preferred_names = preferred_type_names_rust(hosted_functions, provides_list, type_table, duplicate_names)

	generate_rust_file_header
		.concat(generate_rust_imports)
		.concat("\n")
		.concat(generate_host_abi_types_rust)
		.concat("\n")
		.concat(generate_roc_box_helpers_rust)
		.concat("\n")
		.concat(generate_rust_roc_str)
		.concat("\n")
		.concat(generate_rust_roc_list)
		.concat("\n")
		.concat(generate_element_type_structs_rust(type_table, duplicate_names, preferred_names))
		.concat(generate_tag_union_structs_rust(type_table, duplicate_names, preferred_names))
		.concat(generate_all_record_structs_rust(hosted_functions, type_table, duplicate_names, preferred_names))
		.concat(generate_all_args_structs_rust(hosted_functions, type_table, duplicate_names, preferred_names))
		.concat(generate_platform_type_aliases_rust(hosted_functions, provides_list, type_table, duplicate_names, preferred_names))
		.concat(generate_refcount_helpers_rust(type_table, duplicate_names, preferred_names))
		.concat("\n")
		.concat(generate_runtime_symbol_externs_rust)
		.concat("\n")
		.concat(generate_hosted_symbol_externs_rust(hosted_functions, type_table, duplicate_names, preferred_names))
		.concat("\n")
		.concat(generate_host_helpers_rust)
		.concat("\n")
		.concat(generate_entrypoint_externs_rust(provides_list, type_table, duplicate_names, preferred_names))
}

## File header comment
generate_rust_file_header : Str
generate_rust_file_header =
	\\//! Roc Platform ABI
	\\//!
	\\//! This file defines the Rust interface for hosted functions in a Roc platform.
	\\//! It is automatically generated by the Roc glue generator.
	\\//!
	\\//! Hosted argument ownership:
	\\//! - Roc transfers ownership of refcounted arguments to the hosted function.
	\\//! - The hosted function must decref owned refcounted arguments when done.
	\\//! - If the host stores or returns an argument, it must retain or transfer ownership explicitly.
	\\//!
	\\//! Import this module from the platform host and implement the listed hosted symbols
	\\//! with the exact natural C ABI signatures shown below.
	\\
	\\#![cfg_attr(rustfmt, rustfmt_skip)]
	\\#![allow(dead_code)]
	\\
	\\

## Import section
generate_rust_imports : Str
generate_rust_imports =
	\\use core::ffi::c_void;
	\\use core::sync::atomic::{fence, AtomicIsize, Ordering};
	\\#[cfg(not(no_roc_std_helpers))]
	\\use std::alloc::Layout;
	\\

## Generate self-contained host ABI type definitions (matching roc_ops.rs)
generate_host_abi_types_rust : Str
generate_host_abi_types_rust =
	\\/// Runtime representation of Roc's fixed-point `Dec` value.
	\\///
	\\/// `num` stores the decimal value scaled by 10^18.
	\\#[repr(C)]
	\\#[derive(Clone, Copy)]
	\\pub struct RocDec {
	\\    pub num: i128,
	\\}
	\\
	\\const _: [(); 16] = [(); core::mem::size_of::<RocDec>()];
	\\const _: [(); 16] = [(); core::mem::align_of::<RocDec>()];
	\\
	\\/// Runtime representation of an opaque `Box(T)` value.
	\\pub type RocBox = *mut c_void;
	\\
	\\/// Host-internal allocator and diagnostic context used by helper functions in this file.
	\\///
	\\/// Compiled Roc code does not receive this value. The real host ABI is the set of direct
	\\/// linker symbols declared below (`roc_alloc`, hosted symbols, and provided entrypoints).
	\\#[repr(C)]
	\\pub struct RocHost {
	\\    pub env: *mut c_void,
	\\    pub roc_alloc: extern "C" fn(*mut RocHost, usize, usize) -> *mut c_void,
	\\    pub roc_dealloc: extern "C" fn(*mut RocHost, *mut c_void, usize),
	\\    pub roc_realloc: extern "C" fn(*mut RocHost, *mut c_void, usize, usize) -> *mut c_void,
	\\    pub roc_dbg: extern "C" fn(*mut RocHost, *const u8, usize),
	\\    pub roc_expect_failed: extern "C" fn(*mut RocHost, *const u8, usize),
	\\    pub roc_crashed: extern "C" fn(*mut RocHost, *const u8, usize),
	\\}
	\\
	\\impl RocHost {
	\\    /// Allocate memory with the given alignment and length.
	\\    ///
	\\    /// # Safety
	\\    /// The returned pointer must be used only according to Roc allocation layout
	\\    /// rules and later released through the matching host deallocator.
	\\    #[inline]
	\\    pub unsafe fn alloc(&self, alignment: usize, length: usize) -> *mut c_void {
	\\        let host = self as *const RocHost as *mut RocHost;
	\\        (self.roc_alloc)(host, length, alignment)
	\\    }
	\\
	\\    /// Deallocate memory previously allocated with `alloc`.
	\\    ///
	\\    /// # Safety
	\\    /// `ptr` must have been allocated by this host with the same alignment and must
	\\    /// not be used after this call.
	\\    #[inline]
	\\    pub unsafe fn dealloc(&self, ptr: *mut c_void, alignment: usize) {
	\\        let host = self as *const RocHost as *mut RocHost;
	\\        (self.roc_dealloc)(host, ptr, alignment);
	\\    }
	\\
	\\    /// Reallocate memory to a new size.
	\\    ///
	\\    /// # Safety
	\\    /// `old_ptr` must have been allocated by this host with the same alignment.
	\\    /// The returned pointer replaces `old_ptr`; the old pointer must not be used.
	\\    #[inline]
	\\    pub unsafe fn realloc(
	\\        &self,
	\\        old_ptr: *mut c_void,
	\\        alignment: usize,
	\\        new_length: usize,
	\\    ) -> *mut c_void {
	\\        let host = self as *const RocHost as *mut RocHost;
	\\        (self.roc_realloc)(host, old_ptr, new_length, alignment)
	\\    }
	\\}
	\\
	\\#[inline]
	\\fn checked_add_usize(left: usize, right: usize, context: &str) -> usize {
	\\    left.checked_add(right).expect(context)
	\\}
	\\
	\\#[inline]
	\\fn checked_mul_usize(left: usize, right: usize, context: &str) -> usize {
	\\    left.checked_mul(right).expect(context)
	\\}
	\\
	\\#[inline]
	\\fn shifted_capacity(capacity: usize) -> usize {
	\\    capacity.checked_shl(1).expect("Roc capacity does not fit shifted representation")
	\\}
	\\
	\\/// Uniform ABI function pointer stored in `RocErasedCallablePayload`.
	\\pub type RocErasedCallableFn = extern "C" fn(*mut RocHost, *mut u8, *const u8, *mut u8);
	\\
	\\/// Final-drop callback for inline erased-callable captures.
	\\pub type RocErasedCallableOnDrop = extern "C" fn(*mut u8, *mut RocHost);
	\\
	\\/// Payload header for `Box(function)`.
	\\#[repr(C)]
	\\#[derive(Debug, Clone, Copy)]
	\\pub struct RocErasedCallablePayload {
	\\    pub callable_fn_ptr: RocErasedCallableFn,
	\\    pub on_drop: Option<RocErasedCallableOnDrop>,
	\\}
	\\
	\\const _: () = assert!(core::mem::size_of::<RocErasedCallablePayload>() == 2 * core::mem::size_of::<usize>(), "RocErasedCallablePayload size mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocErasedCallablePayload, callable_fn_ptr) == 0, "RocErasedCallablePayload.callable_fn_ptr offset mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocErasedCallablePayload, on_drop) == core::mem::size_of::<usize>(), "RocErasedCallablePayload.on_drop offset mismatch");
	\\
	\\/// Runtime representation of `Box(function)`.
	\\pub type RocErasedCallable = *mut u8;
	\\
	\\pub const ROC_ERASED_CALLABLE_CAPTURE_ALIGNMENT: usize = 16;
	\\pub const ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT: usize = 16;
	\\pub const ROC_ERASED_CALLABLE_CAPTURE_OFFSET: usize =
	\\    (core::mem::size_of::<RocErasedCallablePayload>() + 15) & !15;
	\\
	\\#[inline]
	\\pub fn roc_erased_callable_payload_size(capture_size: usize) -> usize {
	\\    checked_add_usize(
	\\        ROC_ERASED_CALLABLE_CAPTURE_OFFSET,
	\\        capture_size,
	\\        "Roc erased-callable payload size overflow",
	\\    )
	\\}
	\\
	\\#[inline]
	\\/// # Safety
	\\/// `callable` must be a non-null Roc erased-callable data pointer.
	\\pub unsafe fn roc_erased_callable_payload_ptr(callable: RocErasedCallable) -> *mut RocErasedCallablePayload {
	\\    callable as *mut RocErasedCallablePayload
	\\}
	\\
	\\#[inline]
	\\/// # Safety
	\\/// `callable` must be a non-null Roc erased-callable data pointer.
	\\pub unsafe fn roc_erased_callable_capture_ptr(callable: RocErasedCallable) -> *mut u8 {
	\\    callable.add(ROC_ERASED_CALLABLE_CAPTURE_OFFSET)
	\\}
	\\
	\\/// Allocate a Roc erased callable payload.
	\\///
	\\/// # Safety
	\\/// The caller must initialize and use the returned callable according to Roc's
	\\/// erased-callable ABI. `callable_fn_ptr` and `on_drop` must have matching ABI
	\\/// signatures for the captured payload.
	\\pub unsafe fn roc_erased_callable_allocate(
	\\    roc_host: &RocHost,
	\\    callable_fn_ptr: RocErasedCallableFn,
	\\    on_drop: Option<RocErasedCallableOnDrop>,
	\\    capture_size: usize,
	\\) -> RocErasedCallable {
	\\    let ptr_width = core::mem::size_of::<usize>();
	\\    let alignment = core::cmp::max(ptr_width, ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT);
	\\    let extra_bytes = core::cmp::max(ptr_width, ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT);
	\\    let total = checked_add_usize(
	\\        extra_bytes,
	\\        roc_erased_callable_payload_size(capture_size),
	\\        "Roc erased-callable allocation size overflow",
	\\    );
	\\    let base = roc_host.alloc(alignment, total) as *mut u8;
	\\    let data = base.add(extra_bytes);
	\\    let rc = data.sub(core::mem::size_of::<isize>()) as *mut isize;
	\\    *rc = 1;
	\\    let payload = roc_erased_callable_payload_ptr(data);
	\\    *payload = RocErasedCallablePayload { callable_fn_ptr, on_drop };
	\\    data
	\\}
	\\

## Generate self-contained RocBox refcount helpers.
generate_roc_box_helpers_rust : Str
generate_roc_box_helpers_rust =
	\\/// Payload drop callback for a boxed value.
	\\///
	\\/// The callback receives the boxed payload data pointer and must recursively
	\\/// decref any Roc refcounted values inside the payload. It must not free the
	\\/// box allocation; `decref_box_with` and `free_box_with` free it after the callback.
	\\pub type RocBoxPayloadDecref = extern "C" fn(*mut c_void, *mut RocHost);
	\\
	\\/// Increment the refcount of a boxed payload data pointer.
	\\///
	\\/// # Safety
	\\/// `data_ptr` must be a valid Roc box payload pointer. The caller must ensure
	\\/// that the extra retained references are balanced by later decrefs.
	\\pub unsafe fn incref_box(data_ptr: RocBox, amount: isize) {
	\\    let data = match box_data_ptr(data_ptr) {
	\\        Some(ptr) => ptr,
	\\        None => return,
	\\    };
	\\    let rc = box_refcount_ptr(data);
	\\    if unsafe { (*rc).load(Ordering::Relaxed) } == 0 {
	\\        return; // REFCOUNT_STATIC_DATA
	\\    }
	\\    unsafe {
	\\        (*rc).fetch_add(amount, Ordering::Relaxed);
	\\    }
	\\}
	\\
	\\/// Allocate a Roc box and return a pointer to its payload data.
	\\///
	\\/// # Safety
	\\/// The returned payload memory is uninitialized. The caller must initialize it
	\\/// according to the Roc type before exposing it to safe APIs or Roc code.
	\\pub unsafe fn allocate_box(
	\\    payload_size: usize,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    roc_host: &RocHost,
	\\) -> RocBox {
	\\    let ptr_width = core::mem::size_of::<usize>();
	\\    let required_space = if payload_contains_refcounted { checked_mul_usize(2, ptr_width, "Roc box header size overflow") } else { ptr_width };
	\\    let header_bytes = required_space.max(payload_alignment);
	\\    let alloc_alignment = ptr_width.max(payload_alignment);
	\\    let total = checked_add_usize(header_bytes, payload_size, "Roc box allocation size overflow");
	\\    let base = roc_host.alloc(alloc_alignment, total) as *mut u8;
	\\    let data = base.add(header_bytes);
	\\    unsafe {
	\\        let rc = data.sub(core::mem::size_of::<isize>()) as *mut isize;
	\\        *rc = 1;
	\\    }
	\\    data as RocBox
	\\}
	\\
	\\/// Decrement a pointer-aligned boxed payload with no Roc refcounted values.
	\\///
	\\/// # Safety
	\\/// `data_ptr` must be a valid Roc box payload pointer owned by this reference.
	\\pub unsafe fn decref_box(data_ptr: RocBox, roc_host: &RocHost) {
	\\    unsafe {
	\\        decref_box_with(data_ptr, core::mem::align_of::<usize>(), false, None, roc_host);
	\\    }
	\\}
	\\
	\\/// Increment a boxed function closure.
	\\///
	\\/// # Safety
	\\/// `callable` must be a valid Roc erased-callable payload pointer.
	\\pub unsafe fn incref_erased_callable(callable: RocErasedCallable, amount: isize) {
	\\    unsafe {
	\\        incref_box(callable as RocBox, amount);
	\\    }
	\\}
	\\
	\\/// Decrement a boxed function closure and run its capture drop callback on final release.
	\\///
	\\/// # Safety
	\\/// `callable` must be a valid Roc erased-callable payload pointer owned by this reference.
	\\pub unsafe fn decref_erased_callable(callable: RocErasedCallable, roc_host: &RocHost) {
	\\    unsafe {
	\\        decref_box_with(
	\\            callable as RocBox,
	\\            ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT,
	\\            false,
	\\            Some(drop_erased_callable_payload),
	\\            roc_host,
	\\        );
	\\    }
	\\}
	\\
	\\extern "C" fn drop_erased_callable_payload(data_ptr: *mut c_void, roc_host: *mut RocHost) {
	\\    if data_ptr.is_null() || roc_host.is_null() {
	\\        return;
	\\    }
	\\    unsafe {
	\\        let callable = data_ptr as RocErasedCallable;
	\\        let payload = roc_erased_callable_payload_ptr(callable);
	\\        if let Some(on_drop) = (*payload).on_drop {
	\\            on_drop(roc_erased_callable_capture_ptr(callable), roc_host);
	\\        }
	\\    }
	\\}
	\\
	\\/// Decrement a boxed payload and run payload teardown when this is the final ref.
	\\///
	\\/// `payload_contains_refcounted` must match the value passed to `allocate_box`:
	\\/// it determines the box header size, and is independent of whether a
	\\/// `payload_decref` teardown callback is supplied. A host resource handle such
	\\/// as `Box(U64)` holding a raw pointer has `payload_contains_refcounted: false`
	\\/// even when it provides a teardown callback to free the underlying resource.
	\\///
	\\/// # Safety
	\\/// `data_ptr` must be a valid Roc box payload pointer owned by this reference,
	\\/// and `payload_alignment`/`payload_contains_refcounted` must match allocation.
	\\pub unsafe fn decref_box_with(
	\\    data_ptr: RocBox,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    payload_decref: Option<RocBoxPayloadDecref>,
	\\    roc_host: &RocHost,
	\\) {
	\\    let data = match box_data_ptr(data_ptr) {
	\\        Some(ptr) => ptr,
	\\        None => return,
	\\    };
	\\    let rc = box_refcount_ptr(data);
	\\    if unsafe { (*rc).load(Ordering::Relaxed) } == 0 {
	\\        return; // REFCOUNT_STATIC_DATA
	\\    }
	\\    let prev = unsafe { (*rc).fetch_sub(1, Ordering::Release) };
	\\    if prev == 1 {
	\\        fence(Ordering::Acquire);
	\\        if let Some(callback) = payload_decref {
	\\            callback(data_ptr, roc_host as *const RocHost as *mut RocHost);
	\\        }
	\\        free_box_allocation(data, payload_alignment, payload_contains_refcounted, roc_host);
	\\    }
	\\}
	\\
	\\/// Free a boxed payload allocation immediately after running payload teardown.
	\\///
	\\/// See `decref_box_with` for the meaning of `payload_contains_refcounted`.
	\\///
	\\/// # Safety
	\\/// `data_ptr` must be a valid Roc box payload pointer that will not be used after this call.
	\\pub unsafe fn free_box_with(
	\\    data_ptr: RocBox,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    payload_decref: Option<RocBoxPayloadDecref>,
	\\    roc_host: &RocHost,
	\\) {
	\\    let data = match box_data_ptr(data_ptr) {
	\\        Some(ptr) => ptr,
	\\        None => return,
	\\    };
	\\    if let Some(callback) = payload_decref {
	\\        callback(data_ptr, roc_host as *const RocHost as *mut RocHost);
	\\    }
	\\    free_box_allocation(data, payload_alignment, payload_contains_refcounted, roc_host);
	\\}
	\\
	\\/// Return true when a boxed payload data pointer has exactly one live ref.
	\\pub fn is_unique_box(data_ptr: RocBox) -> bool {
	\\    let data = match box_data_ptr(data_ptr) {
	\\        Some(ptr) => ptr,
	\\        None => return true,
	\\    };
	\\    let rc = box_refcount_ptr(data);
	\\    unsafe { (*rc).load(Ordering::Acquire) == 1 }
	\\}
	\\
	\\fn box_data_ptr(data_ptr: RocBox) -> Option<*mut u8> {
	\\    if data_ptr.is_null() {
	\\        None
	\\    } else {
	\\        Some(data_ptr as *mut u8)
	\\    }
	\\}
	\\
	\\fn box_refcount_ptr(data: *mut u8) -> *mut AtomicIsize {
	\\    unsafe { data.sub(core::mem::size_of::<isize>()) as *mut AtomicIsize }
	\\}
	\\
	\\fn free_box_allocation(
	\\    data: *mut u8,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    roc_host: &RocHost,
	\\) {
	\\    let ptr_width = core::mem::size_of::<usize>();
	\\    let required_space = if payload_contains_refcounted { checked_mul_usize(2, ptr_width, "Roc box header size overflow") } else { ptr_width };
	\\    let header_bytes = required_space.max(payload_alignment);
	\\    let alloc_alignment = ptr_width.max(payload_alignment);
	\\    let base = unsafe { data.sub(header_bytes) } as *mut c_void;
	\\    unsafe {
	\\        roc_host.dealloc(base, alloc_alignment);
	\\    }
	\\}
	\\

## Generate self-contained RocStr type (simplified, raw pointer approach)
generate_rust_roc_str : Str
generate_rust_roc_str =
	\\/// A Roc string value. Small strings (up to 23 bytes on 64-bit) are stored inline;
	\\/// larger strings are heap-allocated with a reference count.
	\\///
	\\/// `bytes` is never tagged. Operations, host code, glue code, and object-file
	\\/// relocations can use it directly as the UTF-8 byte pointer for non-small
	\\/// strings. Seamless-slice tagging lives in `capacity_or_alloc_ptr` instead.
	\\/// Big-string capacity is stored shifted left by one bit, so max capacity is
	\\/// essentially `isize::MAX` bytes: about 2 GiB on 32-bit targets and 8 EiB on
	\\/// 64-bit targets.
	\\///
	\\/// This type is ABI-compatible with the Zig RocStr (24 bytes, `#[repr(C)]`).
	\\#[repr(C)]
	\\#[derive(Clone, Copy)]
	\\pub struct RocStr {
	\\    pub bytes: *mut u8,
	\\    pub capacity_or_alloc_ptr: usize,
	\\    pub length: usize,
	\\}
	\\
	\\const _: () = assert!(core::mem::size_of::<RocStr>() == 3 * core::mem::size_of::<usize>(), "RocStr size mismatch");
	\\const _: () = assert!(core::mem::align_of::<RocStr>() == core::mem::align_of::<usize>(), "RocStr alignment mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocStr, bytes) == 0, "RocStr.bytes offset mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocStr, capacity_or_alloc_ptr) == core::mem::size_of::<usize>(), "RocStr.capacity_or_alloc_ptr offset mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocStr, length) == 2 * core::mem::size_of::<usize>(), "RocStr.length offset mismatch");
	\\
	\\const ROC_STR_SIZE: usize = core::mem::size_of::<RocStr>();
	\\const ROC_SMALL_STR_MAX_LEN: usize = ROC_STR_SIZE - 1;
	\\const ROC_SMALL_STR_BIT: usize = isize::MIN as usize;
	\\const ROC_SEAMLESS_SLICE_TAG: usize = 1;
	\\
	\\impl RocStr {
	\\    /// Return an empty RocStr (small string with zero length).
	\\    pub fn empty() -> Self {
	\\        Self {
	\\            bytes: core::ptr::null_mut(),
	\\            capacity_or_alloc_ptr: 0,
	\\            length: ROC_SMALL_STR_BIT,
	\\        }
	\\    }
	\\
	\\    /// Return true if this string is stored inline (small string optimization).
	\\    #[inline]
	\\    pub fn is_small_str(&self) -> bool {
	\\        (self.length as isize) < 0
	\\    }
	\\
	\\    /// Return true if this string is a seamless slice into another allocation.
	\\    #[inline]
	\\    pub fn is_seamless_slice(&self) -> bool {
	\\        !self.is_small_str() && (self.capacity_or_alloc_ptr & ROC_SEAMLESS_SLICE_TAG) != 0
	\\    }
	\\
	\\    /// Return the length of the string in bytes.
	\\    #[inline]
	\\    pub fn len(&self) -> usize {
	\\        if self.is_small_str() {
	\\            let bytes_ptr = self as *const Self as *const u8;
	\\            let last_byte = unsafe { *bytes_ptr.add(ROC_STR_SIZE - 1) };
	\\            (last_byte ^ 0b1000_0000) as usize
	\\        } else {
	\\            self.length
	\\        }
	\\    }
	\\
	\\    /// Return true if the string has zero length.
	\\    #[inline]
	\\    pub fn is_empty(&self) -> bool {
	\\        self.len() == 0
	\\    }
	\\
	\\    /// Return the string contents as a byte slice.
	\\    pub fn as_slice(&self) -> &[u8] {
	\\        let ptr = self.as_u8_ptr();
	\\        unsafe { core::slice::from_raw_parts(ptr, self.len()) }
	\\    }
	\\
	\\    /// Return a pointer to the raw UTF-8 bytes.
	\\    #[inline]
	\\    pub fn as_u8_ptr(&self) -> *const u8 {
	\\        if self.is_small_str() {
	\\            self as *const Self as *const u8
	\\        } else {
	\\            self.bytes as *const u8
	\\        }
	\\    }
	\\
	\\    /// Return the string contents as a `&str`.
	\\    pub fn as_str(&self) -> &str {
	\\        core::str::from_utf8(self.as_slice()).expect("RocStr contained invalid UTF-8")
	\\    }
	\\
	\\    /// Create a RocStr from a UTF-8 byte slice, using `roc_host` for heap allocation if needed.
	\\    pub fn from_slice(slice: &[u8], roc_host: &RocHost) -> Self {
	\\        core::str::from_utf8(slice).expect("RocStr::from_slice requires valid UTF-8");
	\\        if slice.len() < ROC_STR_SIZE {
	\\            let mut result = Self::empty();
	\\            let ptr = &mut result as *mut Self as *mut u8;
	\\            unsafe {
	\\                core::ptr::copy_nonoverlapping(slice.as_ptr(), ptr, slice.len());
	\\                *ptr.add(ROC_STR_SIZE - 1) = (slice.len() as u8) | 0b1000_0000;
	\\            }
	\\            result
	\\        } else {
	\\            let ptr_width = core::mem::size_of::<usize>();
	\\            let total = checked_add_usize(ptr_width, slice.len(), "RocStr allocation size overflow");
	\\            let base = unsafe { roc_host.alloc(core::mem::align_of::<usize>(), total) };
	\\            let data_ptr = unsafe { (base as *mut u8).add(ptr_width) };
	\\            // Write refcount = 1
	\\            unsafe {
	\\                let rc = (data_ptr as *mut isize).sub(1);
	\\                *rc = 1;
	\\                core::ptr::copy_nonoverlapping(slice.as_ptr(), data_ptr, slice.len());
	\\            }
	\\            Self {
	\\                bytes: data_ptr,
	\\                capacity_or_alloc_ptr: shifted_capacity(slice.len()),
	\\                length: slice.len(),
	\\            }
	\\        }
	\\    }
	\\
	\\    /// Create a RocStr from a `&str`.
	\\    pub fn from_str(s: &str, roc_host: &RocHost) -> Self {
	\\        Self::from_slice(s.as_bytes(), roc_host)
	\\    }
	\\
	\\    /// Decrement the reference count; frees the allocation when it reaches zero.
	\\    ///
	\\    /// # Safety
	\\    /// `self` must own one live Roc reference. Calling this more than once for the
	\\    /// same ownership reference may double-free.
	\\    pub unsafe fn decref(self, roc_host: &RocHost) {
	\\        if self.is_small_str() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        let rc = unsafe { (alloc_ptr as *mut AtomicIsize).sub(1) };
	\\        if unsafe { (*rc).load(Ordering::Relaxed) } == 0 {
	\\            return; // REFCOUNT_STATIC_DATA — bytes are in read-only memory
	\\        }
	\\        let prev = unsafe { (*rc).fetch_sub(1, Ordering::Release) };
	\\        if prev == 1 {
	\\            fence(Ordering::Acquire);
	\\            let ptr_width = core::mem::size_of::<usize>();
	\\            let base = unsafe { alloc_ptr.sub(ptr_width) } as *mut c_void;
	\\            unsafe {
	\\                roc_host.dealloc(base, core::mem::align_of::<usize>());
	\\            }
	\\        }
	\\    }
	\\
	\\    /// Increment the reference count by `amount`.
	\\    ///
	\\    /// # Safety
	\\    /// `self` must point at a live Roc allocation. The retained references must
	\\    /// be balanced by later decrefs.
	\\    pub unsafe fn incref(self, amount: isize) {
	\\        if self.is_small_str() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        let rc = unsafe { (alloc_ptr as *mut AtomicIsize).sub(1) };
	\\        if unsafe { (*rc).load(Ordering::Relaxed) } == 0 {
	\\            return; // REFCOUNT_STATIC_DATA
	\\        }
	\\        unsafe {
	\\            (*rc).fetch_add(amount, Ordering::Relaxed);
	\\        }
	\\    }
	\\
	\\    /// Return true if this string has a reference count of exactly one.
	\\    pub fn is_unique(&self) -> bool {
	\\        if self.is_small_str() {
	\\            return true;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return true;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *const AtomicIsize).sub(1);
	\\            let count = (*rc).load(Ordering::Acquire);
	\\            count == 0 || count == 1
	\\        }
	\\    }
	\\
	\\    fn get_allocation_ptr(&self) -> *mut u8 {
	\\        if self.is_seamless_slice() {
	\\            (self.capacity_or_alloc_ptr & !ROC_SEAMLESS_SLICE_TAG) as *mut u8
	\\        } else {
	\\            self.bytes
	\\        }
	\\    }
	\\}
	\\
	\\impl core::fmt::Debug for RocStr {
	\\    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
	\\        f.debug_struct("RocStr")
	\\            .field("len", &self.len())
	\\            .field("is_small", &self.is_small_str())
	\\            .finish()
	\\    }
	\\}
	\\

## Generate self-contained RocList<T> type (simplified, raw pointer approach)
generate_rust_roc_list : Str
generate_rust_roc_list =
	\\/// A generic Roc list. Elements are reference-counted and heap-allocated.
	\\///
	\\/// When `ELEMENTS_REFCOUNTED` is true (the default via `RocList<T>`), an extra
	\\/// `ptr_width` bytes are reserved in the allocation header for the element count,
	\\/// matching the Roc runtime's `allocateWithRefcount` layout.
	\\pub type RocList<T> = RocListWith<T, true>;
	\\
	\\/// Parameterized list constructor; use `RocList<T>` for refcounted elements.
	\\#[repr(C)]
	\\#[derive(Clone, Copy)]
	\\pub struct RocListWith<T, const ELEMENTS_REFCOUNTED: bool> {
	\\    pub elements: *mut T,
	\\    pub length: usize,
	\\    pub capacity_or_alloc_ptr: usize,
	\\}
	\\
	\\const _: () = assert!(core::mem::size_of::<RocList<u8>>() == 3 * core::mem::size_of::<usize>(), "RocList size mismatch");
	\\const _: () = assert!(core::mem::align_of::<RocList<u8>>() == core::mem::align_of::<usize>(), "RocList alignment mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocList<u8>, elements) == 0, "RocList.elements offset mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocList<u8>, length) == core::mem::size_of::<usize>(), "RocList.length offset mismatch");
	\\const _: () = assert!(core::mem::offset_of!(RocList<u8>, capacity_or_alloc_ptr) == 2 * core::mem::size_of::<usize>(), "RocList.capacity_or_alloc_ptr offset mismatch");
	\\
	\\impl<T, const ELEMENTS_REFCOUNTED: bool> RocListWith<T, ELEMENTS_REFCOUNTED> {
	\\    #[inline]
	\\    fn header_bytes() -> usize {
	\\        let ptr_width = core::mem::size_of::<usize>();
	\\        let required_space = if ELEMENTS_REFCOUNTED { checked_mul_usize(2, ptr_width, "RocList header size overflow") } else { ptr_width };
	\\        required_space.max(core::mem::align_of::<T>())
	\\    }
	\\
	\\    /// Return an empty RocList.
	\\    pub fn empty() -> Self {
	\\        Self {
	\\            elements: core::ptr::null_mut(),
	\\            length: 0,
	\\            capacity_or_alloc_ptr: 0,
	\\        }
	\\    }
	\\
	\\    /// Return the number of elements in the list.
	\\    #[inline]
	\\    pub fn len(&self) -> usize {
	\\        self.length
	\\    }
	\\
	\\    /// Return true if the list has zero elements.
	\\    #[inline]
	\\    pub fn is_empty(&self) -> bool {
	\\        self.length == 0
	\\    }
	\\
	\\    /// Return true if this list is a seamless slice into another allocation.
	\\    /// Slices share the rc slot with their backing allocation; the alloc ptr is
	\\    /// encoded in `capacity_or_alloc_ptr` with the low bit set.
	\\    #[inline]
	\\    pub fn is_seamless_slice(&self) -> bool {
	\\        (self.capacity_or_alloc_ptr & 1) != 0
	\\    }
	\\
	\\    /// Resolve `self` to the start of its backing allocation (the element block
	\\    /// just after the rc slot). Returns `null` for empty lists. Handles both
	\\    /// whole-backing and seamless-slice forms.
	\\    fn get_allocation_ptr(&self) -> *mut u8 {
	\\        if self.is_seamless_slice() {
	\\            (self.capacity_or_alloc_ptr & !1) as *mut u8
	\\        } else {
	\\            self.elements as *mut u8
	\\        }
	\\    }
	\\
	\\    fn allocation_element_count(&self) -> usize {
	\\        if self.is_seamless_slice() && ELEMENTS_REFCOUNTED {
	\\            let alloc_ptr = self.get_allocation_ptr();
	\\            if alloc_ptr.is_null() {
	\\                return 0;
	\\            }
	\\            unsafe {
	\\                let ptr = alloc_ptr as *const usize;
	\\                *ptr.sub(2)
	\\            }
	\\        } else {
	\\            self.length
	\\        }
	\\    }
	\\
	\\    /// Return the list elements as a slice.
	\\    pub fn as_slice(&self) -> &[T] {
	\\        if self.elements.is_null() {
	\\            &[]
	\\        } else {
	\\            unsafe { core::slice::from_raw_parts(self.elements, self.length) }
	\\        }
	\\    }
	\\
	\\    /// Return all items in the backing allocation, not just this slice.
	\\    pub fn allocation_items(&self) -> &[T] {
	\\        if self.elements.is_null() {
	\\            &[]
	\\        } else {
	\\            unsafe { core::slice::from_raw_parts(self.get_allocation_ptr() as *const T, self.allocation_element_count()) }
	\\        }
	\\    }
	\\
	\\    /// Allocate a new list with space for `length` elements.
	\\    ///
	\\    /// # Safety
	\\    /// The returned element memory is uninitialized while `length` is already
	\\    /// set. The caller must initialize every element before exposing the list
	\\    /// to safe APIs, Roc code, or generated refcount helpers.
	\\    pub unsafe fn allocate(length: usize, roc_host: &RocHost) -> Self {
	\\        if length == 0 {
	\\            return Self::empty();
	\\        }
	\\        let align = core::mem::align_of::<T>().max(core::mem::align_of::<usize>());
	\\        let header_bytes = Self::header_bytes();
	\\        let data_bytes = checked_mul_usize(length, core::mem::size_of::<T>(), "RocList allocation element bytes overflow");
	\\        let total = checked_add_usize(data_bytes, header_bytes, "RocList allocation size overflow");
	\\        let base = roc_host.alloc(align, total);
	\\        let data_ptr = (base as *mut u8).add(header_bytes);
	\\        unsafe {
	\\            let rc = (data_ptr as *mut isize).sub(1);
	\\            *rc = 1;
	\\            if ELEMENTS_REFCOUNTED {
	\\                let count = (data_ptr as *mut usize).sub(2);
	\\                *count = length;
	\\            }
	\\        }
	\\        Self {
	\\            elements: data_ptr as *mut T,
	\\            length,
	\\            capacity_or_alloc_ptr: shifted_capacity(length),
	\\        }
	\\    }
	\\
	\\    /// Create a RocList from a slice, copying elements into a new allocation.
	\\    ///
	\\    /// # Safety
	\\    /// This is a shallow copy. For element types that own Roc references, the
	\\    /// caller must ensure each copied element is already retained for the new
	\\    /// list or will not be decref'd through another owner.
	\\    pub unsafe fn from_slice(slice: &[T], roc_host: &RocHost) -> Self where T: Copy {
	\\        if slice.is_empty() {
	\\            return Self::empty();
	\\        }
	\\        let list = unsafe { Self::allocate(slice.len(), roc_host) };
	\\        unsafe {
	\\            core::ptr::copy_nonoverlapping(
	\\                slice.as_ptr(),
	\\                list.elements,
	\\                slice.len(),
	\\            );
	\\        }
	\\        list
	\\    }
	\\
	\\    /// Decrement the reference count; frees the allocation when it reaches zero.
	\\    ///
	\\    /// # Safety
	\\    /// `self` must own one live Roc list reference. Calling this more than once
	\\    /// for the same ownership reference may double-free.
	\\    pub unsafe fn decref(self, roc_host: &RocHost) {
	\\        if self.elements.is_null() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        let align = core::mem::align_of::<T>().max(core::mem::align_of::<usize>());
	\\        let header_bytes = Self::header_bytes();
	\\        let rc = unsafe { (alloc_ptr as *mut AtomicIsize).sub(1) };
	\\        if unsafe { (*rc).load(Ordering::Relaxed) } == 0 {
	\\            return; // REFCOUNT_STATIC_DATA — elements are in read-only memory
	\\        }
	\\        let prev = unsafe { (*rc).fetch_sub(1, Ordering::Release) };
	\\        if prev == 1 {
	\\            fence(Ordering::Acquire);
	\\            let base = unsafe { alloc_ptr.sub(header_bytes) } as *mut c_void;
	\\            unsafe {
	\\                roc_host.dealloc(base, align);
	\\            }
	\\        }
	\\    }
	\\
	\\    /// Increment the reference count by `amount`.
	\\    ///
	\\    /// # Safety
	\\    /// `self` must point at a live Roc list allocation. The retained references
	\\    /// must be balanced by later decrefs.
	\\    pub unsafe fn incref(self, amount: isize) {
	\\        if self.elements.is_null() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        let rc = unsafe { (alloc_ptr as *mut AtomicIsize).sub(1) };
	\\        if unsafe { (*rc).load(Ordering::Relaxed) } == 0 {
	\\            return; // REFCOUNT_STATIC_DATA
	\\        }
	\\        unsafe {
	\\            (*rc).fetch_add(amount, Ordering::Relaxed);
	\\        }
	\\    }
	\\
	\\    /// Return true if this list has a reference count of exactly one.
	\\    pub fn is_unique(&self) -> bool {
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return true;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *const AtomicIsize).sub(1);
	\\            let count = (*rc).load(Ordering::Acquire);
	\\            count == 0 || count == 1
	\\        }
	\\    }
	\\
	\\    /// Return true if this list's allocation has exactly one counted ref.
	\\    pub fn has_one_ref(&self) -> bool {
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return false;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *const AtomicIsize).sub(1);
	\\            (*rc).load(Ordering::Acquire) == 1
	\\        }
	\\    }
	\\}
	\\
	\\impl<T: core::fmt::Debug, const ELEMENTS_REFCOUNTED: bool> core::fmt::Debug for RocListWith<T, ELEMENTS_REFCOUNTED> {
	\\    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
	\\        f.debug_list().entries(self.as_slice().iter()).finish()
	\\    }
	\\}
	\\

## Generate index constants with SCREAMING_SNAKE_CASE
generate_index_constants_rust : List(HostedFunctionInfo), U64 -> Str
generate_index_constants_rust = |hosted_functions, count| {
	var $constants = "/// Total number of hosted functions in this platform.\npub const HOSTED_FUNCTION_COUNT: u32 = ${U64.to_str(count)};\n\n"

	for func in hosted_functions {
		screaming = name_to_screaming_snake(func.name)
		$constants = Str.concat(
			$constants,
			"/// Dispatch index for ${func.name}.\npub const HOSTED_IDX_${screaming}: u32 = ${U64.to_str(func.index)};\n",
		)
	}

	$constants
}

## Generate #[repr(C)] structs for element types found in the type table.
generate_element_type_structs_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_element_type_structs_rust = |type_table, duplicate_names, preferred_names| {
	var $structs = ""
	var $seen_names = []

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					struct_name = name_to_struct_name(rec.name)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						doc = "/// Element type for ${rec.name}\n"
						$structs = Str.concat(
							$structs,
							generate_record_struct_decl_rust(doc, struct_name, type_table, duplicate_names, preferred_names, abi_record_fields(type_info.layout), rec.anonymous, type_info.layout),
						)
					}
				}
			_ => {}
		}
	}

	$structs
}

## Generate #[repr(C)] structs for tag union types found in the type table.
generate_tag_union_structs_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_tag_union_structs_rust = |type_table, duplicate_names, preferred_names| {
	var $structs = ""
	var $seen_names = []
	var $type_id = 0

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					struct_name = tag_union_struct_name(preferred_names, duplicate_names, $type_id, tu)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$structs = Str.concat($structs, generate_single_tag_union_rust(type_table, duplicate_names, preferred_names, $type_id, tu, type_info.layout))
					}
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	$structs
}

## Generate Rust code for a single multi-variant tag union.
generate_single_tag_union_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr, AbiLayout -> Str
generate_single_tag_union_rust = |type_table, duplicate_names, preferred_names, type_id, tu, abi_layout| {
	struct_name = tag_union_struct_name(preferred_names, duplicate_names, type_id, tu)
	disc_type = disc_type_for_size(abi_discriminant_size(abi_layout))
	abi_tags = abi_tag_layouts(abi_layout)

	# Check if this is a pure enum (all variants have no payload)
	is_pure_enum = List.all(abi_tags, |tag| !(abi_tag_has_payload(tag)))

	if is_pure_enum {
		# Pure enum: just emit the enum type
		var $variants = ""
		var $idx = 0
		for tag in tu.tags {
			$variants = Str.concat($variants, "    ${capitalize_first(tag.name)} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		assertions = rust_layout_assertions(struct_name, abi_layout)
		"/// Tag union: ${tu.name}\n#[repr(${disc_type})]\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum ${struct_name} {\n${$variants}}\n\n${assertions}"
	} else {
		# Generate tuple structs for any variant with >1 payload
		var $tuple_structs = ""
		var $tag_idx = 0
		for tag in tu.tags {
			tag_layout = abi_tag_at(abi_tags, $tag_idx)
			if abi_tag_has_payload(tag_layout) and List.len(tag.payload) > 1 {
				tuple_name = "${struct_name}${capitalize_first(tag.name)}Payload"
				tuple_doc = "/// Payload struct for ${tag.name} variant.\n"
				$tuple_structs = Str.concat($tuple_structs, generate_payload_struct_decl_rust(tuple_doc, tuple_name, type_table, duplicate_names, preferred_names, tag_layout.payload_fields, tag_layout))
			}
			$tag_idx = $tag_idx + 1
		}

		# Tag enum
		var $enum_variants = ""
		var $idx = 0
		for enum_tag in tu.tags {
			$enum_variants = Str.concat($enum_variants, "    ${capitalize_first(enum_tag.name)} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		# Payload union - Rust requires ManuallyDrop for non-Copy fields in unions
		var $union_fields = ""
		var $payload_accessors = ""
		var $union_tag_idx = 0
		for union_tag in tu.tags {
			tag_layout = abi_tag_at(abi_tags, $union_tag_idx)
			snake = to_lower_snake_case(union_tag.name)
			if !(abi_tag_has_payload(tag_layout)) {
				# No-payload variant: use [u8; 0]
				$union_fields = Str.concat($union_fields, "    pub ${snake}: [u8; 0],\n")
				} else if List.len(union_tag.payload) == 1 {
					rust_type = match List.first(union_tag.payload) {
						Ok(pid) => type_id_to_rust(type_table, duplicate_names, preferred_names, pid)
						Err(_) => {
							crash "glue invariant violated: single-payload tag had no payload"
						}
					}
				# Wrap in ManuallyDrop since union fields must be Copy or ManuallyDrop
				$union_fields = Str.concat($union_fields, "    pub ${snake}: core::mem::ManuallyDrop<${rust_type}>,\n")
				$payload_accessors = Str.concat(
					$payload_accessors,
					"    #[cfg(target_pointer_width = \"32\")]\n    pub fn payload_${snake}(&self) -> ${rust_type} {\n        unsafe { core::ptr::read(self.payload.as_ptr() as *const ${rust_type}) }\n    }\n\n    #[cfg(not(target_pointer_width = \"32\"))]\n    pub fn payload_${snake}(&self) -> ${rust_type} {\n        unsafe { core::mem::ManuallyDrop::into_inner(self.payload.${snake}) }\n    }\n\n",
				)
			} else {
				tuple_name = "${struct_name}${capitalize_first(union_tag.name)}Payload"
				$union_fields = Str.concat($union_fields, "    pub ${snake}: core::mem::ManuallyDrop<${tuple_name}>,\n")
				$payload_accessors = Str.concat(
					$payload_accessors,
					"    #[cfg(target_pointer_width = \"32\")]\n    pub fn payload_${snake}(&self) -> ${tuple_name} {\n        unsafe { core::ptr::read(self.payload.as_ptr() as *const ${tuple_name}) }\n    }\n\n    #[cfg(not(target_pointer_width = \"32\"))]\n    pub fn payload_${snake}(&self) -> ${tuple_name} {\n        unsafe { core::mem::ManuallyDrop::into_inner(self.payload.${snake}) }\n    }\n\n",
				)
			}
			$union_tag_idx = $union_tag_idx + 1
		}

		# Size/alignment assertions
		assertions = rust_tag_union_layout_assertions(struct_name, abi_layout)
		alignment_marker = "${struct_name}PayloadAlignment"

		decl =
			\\/// Tag discriminant for ${tu.name}.
			\\#[repr(${disc_type})]
			\\#[derive(Debug, Clone, Copy, PartialEq, Eq)]
			\\pub enum ${struct_name}Tag {
			\\${$enum_variants}}
			\\
			\\#[repr(C)]
			\\#[derive(Clone, Copy)]
			\\pub union ${struct_name}Payload {
			\\${$union_fields}}
			\\
			\\#[cfg(target_pointer_width = "32")]
			\\#[repr(align(${U64.to_str(abi_layout.alignment32)}))]
			\\#[derive(Clone, Copy)]
			\\pub struct ${alignment_marker};
			\\
			\\/// Tag union: ${tu.name}
			\\#[cfg(target_pointer_width = "32")]
			\\#[repr(C)]
			\\#[derive(Clone, Copy)]
			\\pub struct ${struct_name} {
			\\    pub _payload_alignment: [${alignment_marker}; 0],
			\\    pub payload: [u8; ${U64.to_str(abi_discriminant_offset(abi_layout, Pointer32))}],
			\\    pub tag: ${struct_name}Tag,
			\\}
			\\
			\\/// Tag union: ${tu.name}
			\\#[cfg(not(target_pointer_width = "32"))]
			\\#[repr(C)]
			\\#[derive(Clone, Copy)]
			\\pub struct ${struct_name} {
			\\    pub payload: ${struct_name}Payload,
			\\    pub tag: ${struct_name}Tag,
			\\}
			\\
			\\impl ${struct_name} {
			\\${$payload_accessors}}
		"${$tuple_structs}${decl}\n\n${assertions}"
	}
}

## Generate #[repr(C)] structs for record return types using type table.
generate_all_record_structs_rust : List(HostedFunctionInfo), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_all_record_structs_rust = |hosted_functions, type_table, duplicate_names, preferred_names| {
	var $structs = ""
	arg_shape = ArgShape.from_table(type_table)
	for func in hosted_functions {
		match arg_shape.record_lookup(func.ret_type_id) {
			ArgRecordFound(record) => {
				struct_name = name_to_struct_name(func.name)
				doc = "/// Return type record for ${func.name}\n/// Fields ordered by compiler-emitted ABI offsets.\n"
				$structs = Str.concat(
					$structs,
					generate_record_struct_decl_rust(doc, "${struct_name}RetRecord", type_table, duplicate_names, preferred_names, record.fields, record.anonymous, record.layout),
				)
			}
			ArgNotRecord => {}
		}
	}
	$structs
}

## Generate all argument #[repr(C)] structs
generate_all_args_structs_rust : List(HostedFunctionInfo), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_all_args_structs_rust = |hosted_functions, type_table, duplicate_names, preferred_names| {
	var $structs = ""
	for func in hosted_functions {
		$structs = Str.concat($structs, generate_args_struct_rust(func, type_table, duplicate_names, preferred_names))
	}
	$structs
}

## Generate a single argument struct (empty string if no args).
generate_args_struct_rust : HostedFunctionInfo, TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_args_struct_rust = |func, type_table, duplicate_names, preferred_names| {
	struct_name = name_to_struct_name(func.name)
	arg_shape = ArgShape.from_table(type_table)

	match arg_shape.hosted_args(func) {
		NoMeaningfulArgs => ""
		SingleRecordArg(record) => {
			doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"
			generate_record_struct_decl_rust(doc, "${struct_name}Args", type_table, duplicate_names, preferred_names, record.fields, record.anonymous, record.layout)
		}
		PositionalArgs(arg_type_ids) => {
			var $positional_fields = ""
			var $idx = 0
			for arg_type_id in arg_type_ids {
				rust_type = type_id_to_rust(type_table, duplicate_names, preferred_names, arg_type_id)
				$positional_fields = Str.concat(
					$positional_fields,
					"    pub arg${U64.to_str($idx)}: ${rust_type},\n",
				)
				$idx = $idx + 1
			}

			doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"

			"${doc}#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${struct_name}Args {\n${$positional_fields}}\n\n"
		}
	}
}

# =============================================================================
# Generated Refcount Helpers
# =============================================================================

indent_lines : Str, Str -> Str
indent_lines = |text, prefix| {
	if text == "" {
		return ""
	}

	lines = Str.split_on(text, "\n")
	var $result = ""
	for line in lines {
		if line != "" {
			$result = Str.concat($result, "${prefix}${line}\n")
		}
	}

	$result
}

box_payload_decref_name_rust : U64 -> Str
box_payload_decref_name_rust = |inner_id| "decref_box_payload_type${U64.to_str(inner_id)}"

decref_stmt_for_type_id_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, Str -> Str
decref_stmt_for_type_id_rust = |type_table, duplicate_names, preferred_names, type_id, expr| {
	type_repr = type_table.get(type_id)
	decref_stmt_for_repr_rust(type_table, duplicate_names, preferred_names, type_id, type_repr, expr)
}

decref_stmt_for_repr_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr, Str -> Str
decref_stmt_for_repr_rust = |type_table, duplicate_names, preferred_names, _type_id, type_repr, expr| {
	match type_repr {
		RocStr => "    unsafe { ${expr}.decref(roc_host); }\n"
		RocList(elem_id) => {
			if is_type_refcounted(type_table, elem_id) {
				elem_stmt = decref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, elem_id, "item")
				if elem_stmt == "" {
					"    compile_error!(\"missing decref helper for refcounted list element type id ${U64.to_str(elem_id)}\");\n"
				} else {
					"    {\n        let list = ${expr};\n        if list.has_one_ref() {\n            for item_ref in list.allocation_items() {\n                let item = *item_ref;\n${indent_lines(elem_stmt, "                ")}            }\n        }\n        unsafe { list.decref(roc_host); }\n    }\n"
				}
			} else {
				"    unsafe { ${expr}.decref(roc_host); }\n"
			}
		}
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "    unsafe { decref_erased_callable(${expr}, roc_host); }\n"
				_ => {
					inner_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, inner_id)
					if inner_rust == "RocBox" or inner_rust == "*mut c_void" {
						"    unsafe { decref_box(${expr} as RocBox, roc_host); }\n"
					} else if is_type_refcounted(type_table, inner_id) {
						"    unsafe { decref_box_with(${expr} as RocBox, core::mem::align_of::<${inner_rust}>(), true, Some(${box_payload_decref_name_rust(inner_id)}), roc_host); }\n"
					} else {
						"    unsafe { decref_box_with(${expr} as RocBox, core::mem::align_of::<${inner_rust}>(), false, None, roc_host); }\n"
					}
				}
			}
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				"    unsafe { ${expr}.decref(roc_host); }\n"
			}
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) => decref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, payload_id, expr)
					SingleNoPayload => ""
					NotSingleVariant =>
						if tu.name != "" {
							"    unsafe { ${expr}.decref(roc_host); }\n"
						} else {
							""
						}
				}
			_ => ""
		}
	}

incref_stmt_for_type_id_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, Str -> Str
incref_stmt_for_type_id_rust = |type_table, duplicate_names, preferred_names, type_id, expr| {
	type_repr = type_table.get(type_id)
	incref_stmt_for_repr_rust(type_table, duplicate_names, preferred_names, type_id, type_repr, expr)
}

incref_stmt_for_repr_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr, Str -> Str
incref_stmt_for_repr_rust = |type_table, duplicate_names, preferred_names, _type_id, type_repr, expr| {
	match type_repr {
		RocStr => "    unsafe { ${expr}.incref(amount); }\n"
		RocList(_) => "    unsafe { ${expr}.incref(amount); }\n"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "    unsafe { incref_erased_callable(${expr}, amount); }\n"
				_ => "    unsafe { incref_box(${expr} as RocBox, amount); }\n"
			}
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				"    unsafe { ${expr}.incref(amount); }\n"
			}
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) => incref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, payload_id, expr)
					SingleNoPayload => ""
					NotSingleVariant =>
						if tu.name != "" {
							"    unsafe { ${expr}.incref(amount); }\n"
						} else {
							""
						}
				}
			_ => ""
		}
	}

generate_record_refcount_helpers_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, RecordRepr -> Str
generate_record_refcount_helpers_rust = |type_table, duplicate_names, preferred_names, rec| {
	struct_name = name_to_struct_name(rec.name)
	var $decref_body = ""
	var $incref_body = ""

	for field in rec.fields {
		if !field.is_padding {
			field_expr = "value.${name_to_rust_field_ident(field.name)}"
			$decref_body = Str.concat($decref_body, decref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, field.type_id, field_expr))
			$incref_body = Str.concat($incref_body, incref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, field.type_id, field_expr))
		}
	}

	if $decref_body == "" {
		$decref_body = "        let _ = value;\n        let _ = roc_host;\n"
	} else {
		$decref_body = indent_lines($decref_body, "    ")
	}
	if $incref_body == "" {
		$incref_body = "        let _ = value;\n        let _ = amount;\n"
	} else {
		$incref_body = indent_lines($incref_body, "    ")
	}

	"impl ${struct_name} {\n    /// Recursively decrement Roc-owned fields.\n    ///\n    /// # Safety\n    /// `self` must own one live Roc reference for each refcounted field.\n    pub unsafe fn decref(self, roc_host: &RocHost) {\n        let value = self;\n${$decref_body}    }\n\n    /// Increment Roc-owned fields.\n    ///\n    /// # Safety\n    /// `self` must point at live Roc allocations. The retained references must\n    /// be balanced by later decrefs.\n    pub unsafe fn incref(self, amount: isize) {\n        let value = self;\n${$incref_body}    }\n}\n\n"
}

generate_tag_payload_refcount_branch_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, Str, TagVariant, Str -> Str
generate_tag_payload_refcount_branch_rust = |type_table, duplicate_names, preferred_names, struct_name, tag, mode| {
	snake = to_lower_snake_case(tag.name)
	variant = capitalize_first(tag.name)
	if List.is_empty(tag.payload) {
		return "        ${struct_name}Tag::${variant} => {},\n"
	}

	if List.len(tag.payload) == 1 {
		body =
			match List.first(tag.payload) {
				Ok(payload_id) => {
					payload_expr = "payload"
					if mode == "decref" {
						decref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, payload_id, payload_expr)
					} else {
						incref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, payload_id, payload_expr)
					}
					}
					Err(_) => {
						crash "glue invariant violated: single-payload tag had no payload"
					}
				}

		if body == "" {
			"        ${struct_name}Tag::${variant} => {},\n"
		} else {
			"        ${struct_name}Tag::${variant} => {\n            let payload = value.payload_${snake}();\n${indent_lines(body, "        ")}        },\n"
		}
	} else {
		var $body = "            let payload = value.payload_${snake}();\n"
		var $idx = 0
		for payload_id in tag.payload {
			field_expr = "payload._${U64.to_str($idx)}"
			stmt = if mode == "decref" {
				decref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, payload_id, field_expr)
			} else {
				incref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, payload_id, field_expr)
			}
			$body = Str.concat($body, indent_lines(stmt, "        "))
			$idx = $idx + 1
		}

		"        ${struct_name}Tag::${variant} => {\n${$body}        },\n"
	}
}

generate_tag_union_refcount_helpers_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr -> Str
generate_tag_union_refcount_helpers_rust = |type_table, duplicate_names, preferred_names, type_id, tu| {
	struct_name = tag_union_struct_name(preferred_names, duplicate_names, type_id, tu)
	is_pure_enum = List.all(tu.tags, |tag| List.is_empty(tag.payload))

	if is_pure_enum {
		return "impl ${struct_name} {\n    /// Recursively decrement Roc-owned payloads.\n    ///\n    /// # Safety\n    /// `self` must own one live Roc reference for each refcounted payload.\n    pub unsafe fn decref(self, roc_host: &RocHost) {\n        let _ = self;\n        let _ = roc_host;\n    }\n\n    /// Increment Roc-owned payloads.\n    ///\n    /// # Safety\n    /// `self` must point at live Roc allocations. The retained references must\n    /// be balanced by later decrefs.\n    pub unsafe fn incref(self, amount: isize) {\n        let _ = self;\n        let _ = amount;\n    }\n}\n\n"
	}

	var $decref_branches = ""
	var $incref_branches = ""
	for tag in tu.tags {
		$decref_branches = Str.concat($decref_branches, generate_tag_payload_refcount_branch_rust(type_table, duplicate_names, preferred_names, struct_name, tag, "decref"))
		$incref_branches = Str.concat($incref_branches, generate_tag_payload_refcount_branch_rust(type_table, duplicate_names, preferred_names, struct_name, tag, "incref"))
	}

	"impl ${struct_name} {\n    /// Recursively decrement Roc-owned payloads.\n    ///\n    /// # Safety\n    /// `self` must own one live Roc reference for each refcounted payload.\n    pub unsafe fn decref(self, roc_host: &RocHost) {\n        let value = self;\n        let _ = roc_host;\n        match value.tag {\n${indent_lines($decref_branches, "    ")}        }\n    }\n\n    /// Increment Roc-owned payloads.\n    ///\n    /// # Safety\n    /// `self` must point at live Roc allocations. The retained references must\n    /// be balanced by later decrefs.\n    pub unsafe fn incref(self, amount: isize) {\n        let value = self;\n        let _ = amount;\n        match value.tag {\n${indent_lines($incref_branches, "    ")}        }\n    }\n}\n\n"
}

generate_box_payload_decref_helpers_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_box_payload_decref_helpers_rust = |type_table, duplicate_names, preferred_names| {
	var $helpers = ""
	var $seen_inner_ids = []

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocBox(inner_id) => {
				if !(List.contains($seen_inner_ids, inner_id)) {
					$seen_inner_ids = $seen_inner_ids.append(inner_id)
					match type_table.get(inner_id) {
						RocFunction(_) => {}
						_ => {
							inner_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, inner_id)
							if inner_rust != "RocBox" and inner_rust != "*mut c_void" and is_type_refcounted(type_table, inner_id) {
								stmt = decref_stmt_for_type_id_rust(type_table, duplicate_names, preferred_names, inner_id, "payload")
								$helpers = Str.concat(
									$helpers,
									"extern \"C\" fn ${box_payload_decref_name_rust(inner_id)}(data_ptr: *mut c_void, roc_host: *mut RocHost) {\n    if data_ptr.is_null() || roc_host.is_null() {\n        return;\n    }\n    let payload = unsafe { *(data_ptr as *const ${inner_rust}) };\n    let roc_host = unsafe { &*roc_host };\n${stmt}}\n\n",
								)
							}
						}
					}
				}
			}
			_ => {}
		}
	}

	$helpers
}

generate_refcount_helpers_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_refcount_helpers_rust = |type_table, duplicate_names, preferred_names| {
	var $helpers = "// Generated Refcount Helpers\n\n"
	var $seen_names = []
	var $type_id = 0

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					struct_name = name_to_struct_name(rec.name)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$helpers = Str.concat($helpers, generate_record_refcount_helpers_rust(type_table, duplicate_names, preferred_names, rec))
					}
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					struct_name = tag_union_struct_name(preferred_names, duplicate_names, $type_id, tu)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$helpers = Str.concat($helpers, generate_tag_union_refcount_helpers_rust(type_table, duplicate_names, preferred_names, $type_id, tu))
					}
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	$helpers.concat(generate_box_payload_decref_helpers_rust(type_table, duplicate_names, preferred_names))
}

## Build a natural C ABI parameter list from Roc function argument type IDs.
direct_param_list_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, List(U64) -> Str
direct_param_list_rust = |type_table, duplicate_names, preferred_names, arg_type_ids| {
	var $params = ""
	var $idx = 0
	arg_shape = ArgShape.from_table(type_table)

	for arg_type_id in arg_shape.positional_non_unit_type_ids(arg_type_ids) {
		arg_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, arg_type_id)
		sep = if $params == "" {
			""
		} else {
			", "
		}
		$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_rust}"
		$idx = $idx + 1
	}

	$params
}

## Build a hosted symbol parameter list, using the generated Args wrapper for
## anonymous single-record arguments so direct-symbol glue stays readable.
direct_hosted_param_list_rust : TypeTable, List(Str), TypeNamePlan.PreferredNames, HostedFunctionInfo -> Str
direct_hosted_param_list_rust = |type_table, duplicate_names, preferred_names, func| {
	arg_shape = ArgShape.from_table(type_table)
	use_args_wrapper = arg_shape.single_arg_is_anonymous_record(func.arg_type_ids)

	var $params = ""
	var $idx = 0

	for arg_type_id in arg_shape.positional_non_unit_type_ids(func.arg_type_ids) {
		arg_rust = if use_args_wrapper {
			"${name_to_struct_name(func.name)}Args"
		} else {
			type_id_to_rust(type_table, duplicate_names, preferred_names, arg_type_id)
		}
		sep = if $params == "" {
			""
		} else {
			", "
		}
		$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_rust}"
		$idx = $idx + 1
	}

	$params
}

# =============================================================================
# Direct ABI Declarations
# =============================================================================

## Generate direct extern declarations for the fixed runtime symbols every host defines.
generate_runtime_symbol_externs_rust : Str
generate_runtime_symbol_externs_rust =
	\\// Runtime Symbols
	\\//
	\\// The host defines these linker symbols. Compiled Roc code calls them directly.
	\\
	\\#[allow(improper_ctypes)]
	\\unsafe extern "C" {
	\\    pub fn roc_alloc(length: usize, alignment: usize) -> *mut c_void;
	\\    pub fn roc_dealloc(ptr: *mut c_void, alignment: usize);
	\\    pub fn roc_realloc(ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void;
	\\    pub fn roc_dbg(bytes: *const u8, len: usize);
	\\    pub fn roc_expect_failed(bytes: *const u8, len: usize);
	\\    pub fn roc_crashed(bytes: *const u8, len: usize);
	\\}
	\\

## Generate direct extern declarations for hosted symbols.
generate_hosted_symbol_externs_rust : List(HostedFunctionInfo), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_hosted_symbol_externs_rust = |hosted_functions, type_table, duplicate_names, preferred_names| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $result = "// Hosted Symbols\n//\n// The platform host must export these symbols with the exact direct C ABI signatures.\n// Refcounted arguments are owned by the hosted function.\n\n#[allow(improper_ctypes)]\nunsafe extern \"C\" {\n"

	for func in hosted_functions {
		params = direct_hosted_param_list_rust(type_table, duplicate_names, preferred_names, func)
		ret_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, func.ret_type_id)
		ret_suffix = if ret_rust == "()" {
			""
		} else {
			" -> ${ret_rust}"
		}

		$result = Str.concat(
			$result,
			"    /// Hosted symbol for ${func.name}\n    /// Roc signature: ${func.type_str}\n    pub fn ${func.ffi_symbol}(${params})${ret_suffix};\n\n",
		)
	}

	Str.concat($result, "}\n")
}

generate_provided_decl_rust : ProvidesEntry, TypeTable, List(Str), TypeNamePlan.PreferredNames, TypeRepr -> Str
generate_provided_decl_rust = |entry, type_table, duplicate_names, preferred_names, type_repr| {
	match type_repr {
		RocFunction(func) => {
			params = direct_param_list_rust(type_table, duplicate_names, preferred_names, func.args)
			ret_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, func.ret)
			ret_suffix = if ret_rust == "()" {
				""
			} else {
				" -> ${ret_rust}"
			}
			"    /// Entrypoint: ${entry.name}\n    pub fn ${entry.ffi_symbol}(${params})${ret_suffix};\n\n"
		}
		_ => {
			value_rust = type_id_to_rust(type_table, duplicate_names, preferred_names, entry.type_id)
			"    /// Static provided value: ${entry.name}\n    pub static ${entry.ffi_symbol}: ${value_rust};\n\n"
		}
	}
}

## Generate extern declarations for entrypoints from the provides clause.
generate_entrypoint_externs_rust : List(ProvidesEntry), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_entrypoint_externs_rust = |provides_list, type_table, duplicate_names, preferred_names| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $result = "// Provided Symbols\n//\n// Roc exports these symbols from the app with their natural C ABI signatures.\n\n#[allow(improper_ctypes)]\nunsafe extern \"C\" {\n"

	for entry in provides_list {
		type_repr = type_table.get(entry.type_id)
		$result = Str.concat($result, generate_provided_decl_rust(entry, type_table, duplicate_names, preferred_names, type_repr))
	}

	Str.concat($result, "}\n")
}

# =============================================================================
# Host Helper Utilities
# =============================================================================

## Generate DefaultAllocators, DefaultHandlers, and make_roc_host helpers.
generate_host_helpers_rust : Str
generate_host_helpers_rust =
	generate_default_allocators_direct_rust
		.concat("\n")
		.concat(generate_default_handlers_direct_rust)
		.concat("\n")
		.concat(generate_make_roc_host_rust)

generate_default_allocators_direct_rust : Str
generate_default_allocators_direct_rust =
	\\/// Default memory management functions for Roc platform helpers.
	\\///
	\\/// Memory layout: each allocation prepends size metadata so that dealloc/realloc
	\\/// can recover the original allocation size because `roc_dealloc` receives no length.
	\\#[cfg(not(no_roc_std_helpers))]
	\\pub struct DefaultAllocators;
	\\
	\\#[cfg(not(no_roc_std_helpers))]
	\\impl DefaultAllocators {
	\\    /// Allocate memory using the Rust global allocator.
	\\    pub extern "C" fn roc_alloc(_roc_host: *mut RocHost, length: usize, alignment: usize) -> *mut c_void {
	\\        unsafe {
	\\            let min_alignment = alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\            let total_size = checked_add_usize(length, size_storage_bytes, "roc_alloc allocation size overflow");
	\\
	\\            debug_assert!(min_alignment.is_power_of_two(), "alignment must be a power of two");
	\\            let layout = Layout::from_size_align_unchecked(total_size, min_alignment);
	\\            let base_ptr = std::alloc::alloc(layout);
	\\            if base_ptr.is_null() {
	\\                eprintln!("roc_alloc: out of memory");
	\\                std::process::exit(1);
	\\            }
	\\
	\\            let size_ptr = base_ptr.add(size_storage_bytes).sub(core::mem::size_of::<usize>()) as *mut usize;
	\\            *size_ptr = total_size;
	\\
	\\            base_ptr.add(size_storage_bytes) as *mut c_void
	\\        }
	\\    }
	\\
	\\    /// Free memory previously allocated by `roc_alloc`.
	\\    pub extern "C" fn roc_dealloc(_roc_host: *mut RocHost, ptr: *mut c_void, alignment: usize) {
	\\        unsafe {
	\\            let min_alignment = alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\
	\\            let size_ptr = (ptr as *const u8).sub(core::mem::size_of::<usize>()) as *const usize;
	\\            let total_size = *size_ptr;
	\\
	\\            let base_ptr = (ptr as *mut u8).sub(size_storage_bytes);
	\\            debug_assert!(min_alignment.is_power_of_two(), "alignment must be a power of two");
	\\            let layout = Layout::from_size_align_unchecked(total_size, min_alignment);
	\\            std::alloc::dealloc(base_ptr, layout);
	\\        }
	\\    }
	\\
	\\    /// Reallocate memory, preserving existing user data.
	\\    pub extern "C" fn roc_realloc(_roc_host: *mut RocHost, ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void {
	\\        unsafe {
	\\            let min_alignment = alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\
	\\            let old_size_ptr = (ptr as *const u8).sub(core::mem::size_of::<usize>()) as *const usize;
	\\            let old_total_size = *old_size_ptr;
	\\            let old_base_ptr = (ptr as *mut u8).sub(size_storage_bytes);
	\\
	\\            let new_total_size = checked_add_usize(new_length, size_storage_bytes, "roc_realloc allocation size overflow");
	\\            debug_assert!(min_alignment.is_power_of_two(), "alignment must be a power of two");
	\\            let old_layout = Layout::from_size_align_unchecked(old_total_size, min_alignment);
	\\            let new_base_ptr = std::alloc::realloc(old_base_ptr, old_layout, new_total_size);
	\\            if new_base_ptr.is_null() {
	\\                eprintln!("roc_realloc: out of memory");
	\\                std::process::exit(1);
	\\            }
	\\
	\\            let new_user_ptr = new_base_ptr.add(size_storage_bytes);
	\\            let new_size_ptr = new_user_ptr.sub(core::mem::size_of::<usize>()) as *mut usize;
	\\            *new_size_ptr = new_total_size;
	\\            new_user_ptr as *mut c_void
	\\        }
	\\    }
	\\}
	\\

generate_default_handlers_direct_rust : Str
generate_default_handlers_direct_rust =
	\\/// Default handlers for dbg, expect-failed, and crash.
	\\#[cfg(not(no_roc_std_helpers))]
	\\pub struct DefaultHandlers;
	\\
	\\#[cfg(not(no_roc_std_helpers))]
	\\impl DefaultHandlers {
	\\    /// Print a `dbg` expression to stderr.
	\\    pub extern "C" fn roc_dbg(_roc_host: *mut RocHost, bytes: *const u8, len: usize) {
	\\        unsafe {
	\\            let msg = core::slice::from_raw_parts(bytes, len);
	\\            let msg = core::str::from_utf8_unchecked(msg);
	\\            eprintln!("[ROC DBG] {}", msg);
	\\        }
	\\    }
	\\
	\\    /// Print a failed `expect` to stderr.
	\\    pub extern "C" fn roc_expect_failed(_roc_host: *mut RocHost, bytes: *const u8, len: usize) {
	\\        unsafe {
	\\            let msg = core::slice::from_raw_parts(bytes, len);
	\\            let msg = core::str::from_utf8_unchecked(msg);
	\\            eprintln!("[ROC EXPECT] {}", msg);
	\\        }
	\\    }
	\\
	\\    /// Print a `crash` message to stderr and exit.
	\\    pub extern "C" fn roc_crashed(_roc_host: *mut RocHost, bytes: *const u8, len: usize) {
	\\        unsafe {
	\\            let msg = core::slice::from_raw_parts(bytes, len);
	\\            let msg = core::str::from_utf8_unchecked(msg);
	\\            eprintln!("[ROC CRASHED] {}", msg);
	\\            std::process::exit(1);
	\\        }
	\\    }
	\\}
	\\

generate_make_roc_host_rust : Str
generate_make_roc_host_rust =
	\\/// Create a host-internal helper context with default memory management and handlers.
	\\///
	\\/// This is only for helper functions in this generated file. It is not passed to
	\\/// compiled Roc code, which uses the direct symbol ABI declared above.
	\\#[cfg(not(no_roc_std_helpers))]
	\\pub fn make_roc_host(env: *mut c_void) -> RocHost {
	\\    RocHost {
	\\        env,
	\\        roc_alloc: DefaultAllocators::roc_alloc,
	\\        roc_dealloc: DefaultAllocators::roc_dealloc,
	\\        roc_realloc: DefaultAllocators::roc_realloc,
	\\        roc_dbg: DefaultHandlers::roc_dbg,
	\\        roc_expect_failed: DefaultHandlers::roc_expect_failed,
	\\        roc_crashed: DefaultHandlers::roc_crashed,
	\\    }
	\\}
	\\

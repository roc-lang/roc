## A glue script for generating a C header file.
app [make_glue] { pf: platform glue }

import pf.Types exposing [Types]
import pf.File exposing [File]
import pf.TypeRepr exposing [TypeRepr]
import pf.AbiLayout exposing [AbiLayout]
import pf.AbiFieldLayout exposing [AbiFieldLayout]
import pf.AbiTagLayout exposing [AbiTagLayout]
import pf.AbiWidth exposing [AbiWidth]
import pf.ArgShape exposing [ArgShape]
import pf.GlueInput exposing [GlueInput]
import pf.HostedFunctionInfo exposing [HostedFunctionInfo]
import pf.TypeNamePlan exposing [TypeNamePlan]
import pf.RecordField exposing [RecordField]
import pf.RecordRepr exposing [RecordRepr]
import pf.TagUnionRepr exposing [TagUnionRepr]
import pf.ProvidesEntry exposing [ProvidesEntry]
import pf.TypeTable exposing [TypeTable]
import pf.RocName exposing [RocName]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	input = GlueInput.from_types(types_list)
	type_table = TypeTable.from_list(input.types)
	header_content = generate_c_header(input.hosted_functions, type_table, input.provides_entries)

	Ok([{ name: "roc_platform_abi.h", content: header_content }])
}

# =============================================================================
# TypeRepr-based C Type Mapping
# =============================================================================

type_id_to_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64 -> Str
type_id_to_c = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id| {
	type_repr = type_table.get(type_id)
	type_repr_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, type_repr)
}

type_repr_to_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr -> Str
type_repr_to_c = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "RocErasedCallable"
				RocUnknown(_) => "RocBox"
				_ => {
					inner_c = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, inner_id)
					if inner_c == "void*" or inner_c == "RocBox" {
						"RocBox"
					} else {
						"${inner_c}*"
					}
				}
			}
		RocStr => "RocStr"
		RocUnit => "void"
		RocU8 => "uint8_t"
		RocU8x16 => "RocU8x16"
		RocI8x16 => "RocI8x16"
		RocU16x8 => "RocU16x8"
		RocI16x8 => "RocI16x8"
		RocU32x4 => "RocU32x4"
		RocI32x4 => "RocI32x4"
		RocU64x2 => "RocU64x2"
		RocI64x2 => "RocI64x2"
		RocU16 => "uint16_t"
		RocU32 => "uint32_t"
		RocU64 => "uint64_t"
		RocU128 => "unsigned __int128"
		RocI8 => "int8_t"
		RocI16 => "int16_t"
		RocI32 => "int32_t"
		RocI64 => "int64_t"
		RocI128 => "__int128"
		RocF32 => "float"
		RocF64 => "double"
		RocDec => "RocDec"
		RocList(_) => "RocList"
		RocRecord(rec) =>
			if rec.name == "" {
				"void*"
			} else {
				record_struct_name(duplicate_record_names, type_id, rec)
			}
		RocTagUnion(tu) => resolve_tag_union_type_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, tu)
		RocFunction(_) => "void*"
		RocUnknown(_) => "void*"
	}
}

resolve_tag_union_type_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr -> Str
resolve_tag_union_type_c = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, tu|
	match TypeTable.single_variant_payload(tu) {
		SinglePayload(payload_id) => type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, payload_id)
		SingleNoPayload => "void"
		NotSingleVariant =>
			if tu.name != "" {
				tag_union_struct_name(preferred_names, duplicate_tag_names, type_id, tu)
			} else {
				"void*"
			}
		}

## Transitively flattened vector-member count of the type when its leaves are
## exclusively 128-bit vectors (counting through record fields and transparent
## single-variant tag wrappers); 0 for any other type. Counts saturate at 5:
## callers only distinguish the 2..4 range, which is the memory-class shape
## that needs the vector/byte union spelling.
homogeneous_vector_member_count : TypeTable, U64 -> U64
homogeneous_vector_member_count = |type_table, type_id|
	match type_table.get(type_id) {
		RocU8x16 => 1
		RocI8x16 => 1
		RocU16x8 => 1
		RocI16x8 => 1
		RocU32x4 => 1
		RocI32x4 => 1
		RocU64x2 => 1
		RocI64x2 => 1
		RocRecord(rec) => {
			var $total = 0
			var $homogeneous = !(List.is_empty(rec.fields))
			for field in rec.fields {
				member = if field.is_padding {
					0
				} else {
					homogeneous_vector_member_count(type_table, field.type_id)
				}
				if member == 0 {
					$homogeneous = Bool.False
				} else if $total + member > 4 {
					$total = 5
				} else {
					$total = $total + member
				}
			}
			if $homogeneous {
				$total
			} else {
				0
			}
		}
		RocTagUnion(tu) =>
			match TypeTable.single_variant_payload(tu) {
				SinglePayload(payload_id) => homogeneous_vector_member_count(type_table, payload_id)
				SingleNoPayload => 0
				NotSingleVariant => 0
			}
		_ => 0
	}

## Whether a record with these committed fields is a memory-class vector
## aggregate: every member is (an aggregate of) 128-bit vectors and there are
## two to four of them in total. C classifies such an aggregate as homogeneous
## and would pass it in registers, but the Roc host ABI passes it in memory —
## the convention Zig and Rust hosts compile to — so C glue spells its members
## as vector/byte unions to pin the memory-class classification.
c_needs_vector_union_spelling : TypeTable, List(AbiFieldLayout) -> Bool
c_needs_vector_union_spelling = |type_table, fields| {
	var $total = 0
	var $homogeneous = !(List.is_empty(fields))
	for field in fields {
		member = if field.is_padding {
			0
		} else {
			homogeneous_vector_member_count(type_table, field.type_id)
		}
		if member == 0 {
			$homogeneous = Bool.False
		} else if $total + member > 4 {
			$total = 5
		} else {
			$total = $total + member
		}
	}
	$homogeneous and $total >= 2 and $total <= 4
}

c_vector_union_comment : Str
c_vector_union_comment =
	\\/* Every member of this aggregate is a 128-bit vector, so members are spelled
	\\   as vector/byte unions (read the vector via `.value`). The union keeps C
	\\   from classifying the aggregate as homogeneous: the Roc host ABI passes
	\\   multi-vector aggregates in memory, matching Zig and Rust hosts. */

c_record_field_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, AbiFieldLayout, AbiWidth, Bool -> Str
c_record_field_decl = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, field, width, wrap_vector_members| {
	field_name = name_to_c_field_ident(field.name)
	if field.is_padding {
		# Padding fields are nonzero at both widths (asserted by the compiler).
		"    uint8_t ${field_name}[${U64.to_str(AbiFieldLayout.size(field, width))}];\n"
	} else {
		c_type = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, field.type_id)
		if wrap_vector_members {
			"    union { ${c_type} value; uint8_t bytes[${U64.to_str(AbiFieldLayout.size(field, width))}]; } ${field_name};\n"
		} else {
			"    ${c_type} ${field_name};\n"
		}
	}
}

## Fields arrive in committed layout order (valid at both pointer widths);
## only per-width padding byte counts differ between the two renderings.
c_record_fields_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout), AbiWidth, Bool -> Str
c_record_fields_decl = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, fields, width, wrap_vector_members| {
	var $field_strs = ""
	for field in fields {
		$field_strs = Str.concat($field_strs, c_record_field_decl(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, field, width, wrap_vector_members))
	}
	$field_strs
}

duplicate_record_names : TypeTable -> List(Str)
duplicate_record_names = |type_table| type_table.duplicate_record_names()

record_struct_name : List(Str), U64, RecordRepr -> Str
record_struct_name = |duplicate_names, type_id, rec| {
	base = name_to_struct_name(rec.name)
	if List.contains(duplicate_names, rec.name) {
		"${base}Type${U64.to_str(type_id)}"
	} else {
		base
	}
}

duplicate_tag_union_names : TypeTable -> List(Str)
duplicate_tag_union_names = |type_table| type_table.duplicate_tag_union_names()

default_tag_union_struct_name : List(Str), U64, TagUnionRepr -> Str
default_tag_union_struct_name = |duplicate_names, type_id, tu| {
	base = name_to_struct_name(tu.name)
	if List.contains(duplicate_names, tu.name) {
		"${base}Type${U64.to_str(type_id)}"
	} else {
		base
	}
}

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

generated_type_names_c : TypeTable, List(Str), List(Str) -> List(Str)
generated_type_names_c = |type_table, duplicate_records, duplicate_tags| {
	var $names = []
	var $type_id = 0

	for type_info in type_table.entries() {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					$names = $names.append(record_struct_name(duplicate_records, $type_id, rec))
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					$names = $names.append(default_tag_union_struct_name(duplicate_tags, $type_id, tu))
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	$names
}

type_name_root_alias_base_c : TypeTable, Str, U64 -> Str
type_name_root_alias_base_c = |type_table, fallback, type_id|
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

record_alias_fields_c : TypeTable, RecordRepr -> List(RecordField)
record_alias_fields_c = |type_table, rec| {
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

type_name_roots_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable -> List(TypeNamePlan.Root)
type_name_roots_c = |hosted_functions, provides_list, type_table| {
	var $roots = []

	for func in hosted_functions {
		base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = $roots.append({
				alias_base: type_name_root_alias_base_c(type_table, arg_fallback, arg_type_id),
				module_base,
				type_id: arg_type_id,
			})
			$arg_idx = $arg_idx + 1
		}

		$roots = $roots.append({
			alias_base: base,
			module_base,
			type_id: func.ret_type_id,
		})
	}

	for entry in provides_list {
		base = name_to_struct_name(entry.name)
		module_base = hosted_module_name_to_struct_name(entry.name)

		match type_table.get(entry.type_id) {
			RocFunction(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = $roots.append({
						alias_base: type_name_root_alias_base_c(type_table, arg_fallback, arg_type_id),
						module_base,
						type_id: arg_type_id,
					})
					$arg_idx = $arg_idx + 1
				}

				$roots = $roots.append({
					alias_base: base,
					module_base,
					type_id: func.ret,
				})
			}
			_ => {
				$roots = $roots.append({
					alias_base: type_name_root_alias_base_c(type_table, base, entry.type_id),
					module_base,
					type_id: entry.type_id,
				})
			}
		}
	}

	$roots
}

append_type_alias_roots_c : List(TypeNamePlan.Root), TypeTable, Str, Str, U64, List(U64) -> List(TypeNamePlan.Root)
append_type_alias_roots_c = |roots, type_table, alias_base, module_base, type_id, visited_type_ids| {
	if List.contains(visited_type_ids, type_id) {
		return roots
	}

	next_visited = visited_type_ids.append(type_id)
	root_alias_base = type_name_root_alias_base_c(type_table, alias_base, type_id)

	var $roots = roots.append({ alias_base: root_alias_base, module_base, type_id })

	match type_table.get(type_id) {
		RocRecord(rec) => {
			for field in record_alias_fields_c(type_table, rec) {
				field_base = "${root_alias_base}${RocName.from_str(field.name).to_pascal_clean()}"
				$roots = append_type_alias_roots_c($roots, type_table, field_base, module_base, field.type_id, next_visited)
			}
			$roots
		}
		RocList(elem_id) => append_type_alias_roots_c($roots, type_table, root_alias_base, module_base, elem_id, next_visited)
		RocBox(inner_id) => append_type_alias_roots_c($roots, type_table, root_alias_base, module_base, inner_id, next_visited)
		RocTagUnion(tu) => {
			var $next = $roots
			for tag in tu.tags {
				child_base = "${root_alias_base}${RocName.capitalize_first(tag.name)}"
				for payload_id in tag.payload {
					$next = append_type_alias_roots_c($next, type_table, child_base, module_base, payload_id, next_visited)
				}
			}
			$next
		}
		_ => $roots
	}
}

type_alias_roots_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable -> List(TypeNamePlan.Root)
type_alias_roots_c = |hosted_functions, provides_list, type_table| {
	var $roots = []

	for func in hosted_functions {
		base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = append_type_alias_roots_c($roots, type_table, arg_fallback, module_base, arg_type_id, [])
			$arg_idx = $arg_idx + 1
		}

		$roots = append_type_alias_roots_c($roots, type_table, base, module_base, func.ret_type_id, [])
	}

	for entry in provides_list {
		base = name_to_struct_name(entry.name)
		module_base = hosted_module_name_to_struct_name(entry.name)

		match type_table.get(entry.type_id) {
			RocFunction(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = append_type_alias_roots_c($roots, type_table, arg_fallback, module_base, arg_type_id, [])
					$arg_idx = $arg_idx + 1
				}

				$roots = append_type_alias_roots_c($roots, type_table, base, module_base, func.ret, [])
			}
			_ => {
				$roots = append_type_alias_roots_c($roots, type_table, base, module_base, entry.type_id, [])
			}
		}
	}

	$roots
}

preferred_type_names_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str), List(Str) -> TypeNamePlan.PreferredNames
preferred_type_names_c = |hosted_functions, provides_list, type_table, duplicate_records, duplicate_tags|
	TypeNamePlan.from_table(type_table).preferred_names(
		generated_type_names_c(type_table, duplicate_records, duplicate_tags),
		type_name_roots_c(hosted_functions, provides_list, type_table),
	)

# =============================================================================
# Name Conversion
# =============================================================================

str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| RocName.replace_all(s, from, to)

to_lower_snake_case : Str -> Str
to_lower_snake_case = |s| RocName.lower_snake_ascii(s)

to_screaming_snake_case : Str -> Str
to_screaming_snake_case = |s| RocName.screaming_snake_ascii(s)

capitalize_first : Str -> Str
capitalize_first = |s| RocName.capitalize_first(s)

name_to_struct_name : Str -> Str
name_to_struct_name = |name| RocName.from_str(name).to_pascal_clean()

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Stdout.line!") == "StdoutLine"

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("__AnonStruct10") == "AnonStruct10"

name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| RocName.from_str(name).to_screaming_snake_identifier()

## Checks `name_to_upper_ident` for this representative case.
expect name_to_upper_ident("Stdout.line!") == "STDOUT_LINE"

## Checks `name_to_upper_ident` for this representative case.
expect name_to_upper_ident("Foo.barBaz!") == "FOO_BAR_BAZ"

name_to_c_func_name : Str -> Str
name_to_c_func_name = |name| RocName.from_str(name).to_lower_snake_identifier()

## Checks `name_to_c_func_name` for this representative case.
expect name_to_c_func_name("Stdout.line!") == "stdout_line"

## Checks `name_to_c_func_name` for this representative case.
expect name_to_c_func_name("Foo.barBaz!") == "foo_bar_baz"

name_to_c_field_ident : Str -> Str
name_to_c_field_ident = |name| {
	sanitized =
		RocName.from_str(name).to_bang_snake_identifier()

	match sanitized {
		"" => "field"
		"_" => "field"
		"auto" => "auto_field"
		"break" => "break_field"
		"case" => "case_field"
		"char" => "char_field"
		"const" => "const_field"
		"continue" => "continue_field"
		"default" => "default_field"
		"do" => "do_field"
		"double" => "double_field"
		"else" => "else_field"
		"enum" => "enum_field"
		"extern" => "extern_field"
		"float" => "float_field"
		"for" => "for_field"
		"goto" => "goto_field"
		"if" => "if_field"
		"inline" => "inline_field"
		"int" => "int_field"
		"long" => "long_field"
		"register" => "register_field"
		"restrict" => "restrict_field"
		"return" => "return_field"
		"short" => "short_field"
		"signed" => "signed_field"
		"sizeof" => "sizeof_field"
		"static" => "static_field"
		"struct" => "struct_field"
		"switch" => "switch_field"
		"typedef" => "typedef_field"
		"union" => "union_field"
		"unsigned" => "unsigned_field"
		"void" => "void_field"
		"volatile" => "volatile_field"
		"while" => "while_field"
		_ => sanitized
	}
}

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("init!") == "init_bang"

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("type") == "type"

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("struct") == "struct_field"

# =============================================================================
# Header Generation
# =============================================================================

generate_c_header : List(HostedFunctionInfo), TypeTable, List(ProvidesEntry) -> Str
generate_c_header = |hosted_functions, type_table, provides_list| {
	duplicate_records = duplicate_record_names(type_table)
	duplicate_tags = duplicate_tag_union_names(type_table)
	preferred_names = preferred_type_names_c(hosted_functions, provides_list, type_table, duplicate_records, duplicate_tags)

	defines = generate_defines(hosted_functions)
	count = List.len(hosted_functions)
	type_decls = generate_type_decls(type_table, duplicate_records, duplicate_tags, preferred_names)
	type_aliases = generate_platform_type_aliases_c(hosted_functions, provides_list, type_table, duplicate_records, duplicate_tags, preferred_names)
	args_structs = generate_all_args_structs(hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names)
	hosted_fn_fields = generate_hosted_fn_fields(hosted_functions)
	hosted_symbol_decls = generate_hosted_symbol_decls(hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names)
	provided_symbol_decls = generate_provided_symbol_decls(provides_list, type_table, duplicate_records, duplicate_tags, preferred_names)

	header_guard_top
		.concat(includes_section)
		.concat(extern_c_start)
		.concat(core_types_section)
		.concat(type_decls)
		.concat(type_aliases)
		.concat(hosted_fn_infrastructure)
		.concat(function_count_section(count))
		.concat(defines)
		.concat("\n\n")
		.concat(args_structs_header)
		.concat(args_structs)
		.concat("\n")
		.concat(hosted_symbol_decls)
		.concat(provided_symbol_decls)
		.concat(hosted_functions_registry(hosted_fn_fields))
		.concat(extern_c_end)
		.concat(header_guard_bottom)
}

generate_defines : List(HostedFunctionInfo) -> Str
generate_defines = |hosted_functions| {
	var $defines = ""
	var $first = Bool.True
	for func in hosted_functions {
		upper_name = name_to_upper_ident(func.name)
		if !$first {
			$defines = Str.concat($defines, "\n")
		}
		$defines = Str.concat($defines, "#define HOSTED_IDX_${upper_name} ${U64.to_str(func.index)}")
		$first = Bool.False
	}

	$defines
}

generate_type_decls : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_type_decls = |type_table, duplicate_records, duplicate_tags, preferred_names| {
	type_definitions = generate_opaque_type_decls(type_table, duplicate_records, duplicate_tags, preferred_names)

	if type_definitions == "" {
		""
	} else {
		section("Reflected Roc Types", type_definitions)
	}
}

generate_opaque_type_decls : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_opaque_type_decls = |type_table, duplicate_records, duplicate_tags, preferred_names| {
	var $forwards = ""
	var $seen_names = []
	var $type_id = 0

	for type_info in type_table.entries() {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					type_name = record_struct_name(duplicate_records, $type_id, rec)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$forwards = Str.concat($forwards, "typedef struct ${type_name} ${type_name};\n")
					}
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					type_name = tag_union_struct_name(preferred_names, duplicate_tags, $type_id, tu)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$forwards = Str.concat($forwards, "typedef struct ${type_name} ${type_name};\n")
					}
				}
			_ => {}
		}
		$type_id = $type_id + 1
	}

	var $state = { content: "", seen_names: [] }
	$type_id = 0
	for _type_info in type_table.entries() {
		$state = generate_type_decl_c($state, type_table, duplicate_records, duplicate_tags, preferred_names, $type_id)
		$type_id = $type_id + 1
	}

	if $forwards == "" {
		$state.content
	} else {
		Str.concat($forwards, "\n${$state.content}")
	}
}

generate_type_decl_c : { content : Str, seen_names : List(Str) }, TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64 -> { content : Str, seen_names : List(Str) }
generate_type_decl_c = |state, type_table, duplicate_records, duplicate_tags, preferred_names, type_id| {
	type_info = type_table.type_info(type_id)
	match type_info.repr {
		RocRecord(rec) =>
			if rec.name == "" {
				state
			} else {
				type_name = record_struct_name(duplicate_records, type_id, rec)
				if List.contains(state.seen_names, type_name) {
					state
				} else {
					var $next = { content: state.content, seen_names: state.seen_names.append(type_name) }
					for field in rec.fields {
						if !field.is_padding {
							$next = generate_type_decl_c($next, type_table, duplicate_records, duplicate_tags, preferred_names, field.type_id)
						}
					}
					{
						content: Str.concat($next.content, generate_record_type_decl(type_table, duplicate_records, duplicate_tags, preferred_names, type_name, type_info.layout)),
						seen_names: $next.seen_names,
					}
				}
			}
		RocTagUnion(tu) =>
			match TypeTable.single_variant_payload(tu) {
				SinglePayload(payload_id) => generate_type_decl_c(state, type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
				SingleNoPayload => state
				NotSingleVariant =>
					if tu.name == "" {
						state
					} else {
						type_name = tag_union_struct_name(preferred_names, duplicate_tags, type_id, tu)
						if List.contains(state.seen_names, type_name) {
							state
						} else {
							var $next = { content: state.content, seen_names: state.seen_names.append(type_name) }
							for tag in tu.tags {
								for payload_id in tag.payload {
									$next = generate_type_decl_c($next, type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
								}
							}
							{
								content: Str.concat($next.content, generate_tag_union_type_decl(type_table, duplicate_records, duplicate_tags, preferred_names, type_id, tu, type_info.layout)),
								seen_names: $next.seen_names,
							}
						}
					}
				}
		_ => state
	}
}

abi_tag_layouts_c : AbiLayout -> List(AbiTagLayout)
abi_tag_layouts_c = |abi_layout| abi_layout.tag_layouts()

abi_tag_at_c : List(AbiTagLayout), U64 -> AbiTagLayout
abi_tag_at_c = |abi_tags, index|
	match List.get(abi_tags, index) {
		Ok(tag) => tag
		Err(_) => {
			crash "glue invariant violated: missing C ABI tag layout at index ${U64.to_str(index)}"
		}
	}

abi_tag_has_payload_c : AbiTagLayout -> Bool
abi_tag_has_payload_c = |tag| tag.payload_size32 > 0 or tag.payload_size64 > 0

c_discriminant_type : U64 -> Str
c_discriminant_type = |size|
	if size <= 1 {
		"uint8_t"
	} else if size == 2 {
		"uint16_t"
	} else if size == 4 {
		"uint32_t"
	} else {
		"uint64_t"
	}

c_payload_layout_assertions : Str, AbiTagLayout, AbiWidth -> Str
c_payload_layout_assertions = |type_name, tag_layout, width| {
	size = match width {
		Pointer32 => tag_layout.payload_size32
		Pointer64 => tag_layout.payload_size64
	}
	alignment = match width {
		Pointer32 => tag_layout.payload_alignment32
		Pointer64 => tag_layout.payload_alignment64
	}
	fields = tag_layout.payload_fields
	Str.concat(static_asserts(type_name, size, alignment), c_record_offset_assertions(type_name, fields, width))
}

generate_payload_type_decl_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, Str, AbiTagLayout -> Str
generate_payload_type_decl_c = |type_table, duplicate_records, duplicate_tags, preferred_names, type_name, tag_layout| {
	wrap_vector_members = c_needs_vector_union_spelling(type_table, tag_layout.payload_fields)
	fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, tag_layout.payload_fields, Pointer64, wrap_vector_members)
	fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, tag_layout.payload_fields, Pointer32, wrap_vector_members)
	assertions64 = c_payload_layout_assertions(type_name, tag_layout, Pointer64)
	assertions32 = c_payload_layout_assertions(type_name, tag_layout, Pointer32)
	comment = if wrap_vector_members {
		"${c_vector_union_comment}\n"
	} else {
		""
	}
	decl =
		\\#if UINTPTR_MAX == UINT64_MAX
		\\typedef struct {
		\\${fields64}} ${type_name};
		\\${assertions64}#else
		\\typedef struct {
		\\${fields32}} ${type_name};
		\\${assertions32}#endif
	"${comment}${decl}\n\n"
}

c_tag_union_layout_assertions : Str, AbiLayout, AbiWidth -> Str
c_tag_union_layout_assertions = |type_name, abi_layout, width| {
	size = AbiLayout.size(abi_layout, width)
	alignment = AbiLayout.alignment(abi_layout, width)
	tag_offset = abi_layout.discriminant_offset(width)
	Str.concat(static_asserts(type_name, size, alignment), "ROC_STATIC_ASSERT(offsetof(${type_name}, tag) == ${U64.to_str(tag_offset)}, \"${type_name}.tag offset mismatch\");\n")
}

generate_tag_union_type_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr, AbiLayout -> Str
generate_tag_union_type_decl = |type_table, duplicate_records, duplicate_tags, preferred_names, type_id, tu, abi_layout| {
	struct_name = tag_union_struct_name(preferred_names, duplicate_tags, type_id, tu)
	disc_type = c_discriminant_type(abi_layout.discriminant_size())
	abi_tags = abi_tag_layouts_c(abi_layout)
	is_pure_enum = List.all(abi_tags, |tag| !(abi_tag_has_payload_c(tag)))

	var $tag_constants = ""
	var $tag_index = 0
	for tag in tu.tags {
		separator = if $tag_index == 0 {
			""
		} else {
			","
		}
		constant_name = name_to_struct_name(tag.name)
		$tag_constants = Str.concat($tag_constants, "${separator}\n    ${struct_name}Tag_${constant_name} = ${U64.to_str($tag_index)}")
		$tag_index = $tag_index + 1
	}
	$tag_constants = Str.concat($tag_constants, "\n")

	if is_pure_enum {
		decl =
			\\typedef ${disc_type} ${struct_name};
			\\enum {${$tag_constants}};
			\\${static_asserts(struct_name, abi_layout.size64, abi_layout.alignment64)}
		"${decl}\n"
	} else {
		var $payload_structs = ""
		var $union_fields = ""
		var $constructors = ""
		var $accessors = ""
		var $variant_index = 0
		for tag in tu.tags {
			tag_layout = abi_tag_at_c(abi_tags, $variant_index)
			field_name = name_to_c_field_ident(tag.name)
			constant_name = name_to_struct_name(tag.name)
			has_payload = abi_tag_has_payload_c(tag_layout)
			if !has_payload {
				$union_fields = Str.concat($union_fields, "    uint8_t ${field_name};\n")
				$constructors = Str.concat($constructors, "static inline ${struct_name} ${struct_name}_make_${field_name}(void) {\n    ${struct_name} out = {0};\n    out.tag = ${struct_name}Tag_${constant_name};\n    return out;\n}\n\n")
			} else {
				payload_type = if List.len(tag.payload) == 1 {
					match List.first(tag.payload) {
						Ok(payload_id) => type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
						Err(_) => {
							crash "glue invariant violated: C single-payload tag had no payload"
						}
					}
				} else {
					tuple_name = "${struct_name}${constant_name}Payload"
					$payload_structs = Str.concat($payload_structs, generate_payload_type_decl_c(type_table, duplicate_records, duplicate_tags, preferred_names, tuple_name, tag_layout))
					tuple_name
				}
				$union_fields = Str.concat($union_fields, "    ${payload_type} ${field_name};\n")
				constructor =
					\\static inline ${struct_name} ${struct_name}_make_${field_name}(${payload_type} payload) {
					\\    ${struct_name} out = {0};
					\\    out.tag = ${struct_name}Tag_${constant_name};
					\\#if UINTPTR_MAX == UINT64_MAX
					\\    out.payload.${field_name} = payload;
					\\#else
					\\    roc_abi_copy_bytes(out.payload, &payload, sizeof(payload));
					\\#endif
					\\    return out;
					\\}
				accessor =
					\\static inline ${payload_type} ${struct_name}_payload_${field_name}(const ${struct_name}* value) {
					\\#if UINTPTR_MAX == UINT64_MAX
					\\    return value->payload.${field_name};
					\\#else
					\\    ${payload_type} payload;
					\\    roc_abi_copy_bytes(&payload, value->payload, sizeof(payload));
					\\    return payload;
					\\#endif
					\\}
				$constructors = Str.concat($constructors, "${constructor}\n\n")
				$accessors = Str.concat($accessors, "${accessor}\n\n")
			}
			$variant_index = $variant_index + 1
		}

		assertions64 = c_tag_union_layout_assertions(struct_name, abi_layout, Pointer64)
		assertions32 = c_tag_union_layout_assertions(struct_name, abi_layout, Pointer32)
		payload_union_name = "${struct_name}TagPayloadStorage"
		decl =
			\\${$payload_structs}typedef ${disc_type} ${struct_name}Tag;
			\\enum {${$tag_constants}};
			\\typedef union {
			\\${$union_fields}} ${payload_union_name};
			\\#if UINTPTR_MAX == UINT64_MAX
			\\struct ${struct_name} {
			\\    ${payload_union_name} payload;
			\\    ${struct_name}Tag tag;
			\\};
			\\${assertions64}#else
			\\struct ${struct_name} {
			\\    ROC_ALIGNAS(${U64.to_str(abi_layout.alignment32)}) uint8_t payload[${U64.to_str(abi_layout.discriminant_offset(Pointer32))}];
			\\    ${struct_name}Tag tag;
			\\};
			\\${assertions32}#endif
		"${decl}\n${$constructors}${$accessors}"
	}
}

abi_record_fields_c : AbiLayout -> List(AbiFieldLayout)
abi_record_fields_c = |abi_layout| abi_layout.record_fields()

c_record_offset_assertions : Str, List(AbiFieldLayout), AbiWidth -> Str
c_record_offset_assertions = |type_name, fields, width| {
	var $assertions = ""
	for field in fields {
		if !field.is_padding {
			field_name = name_to_c_field_ident(field.name)
			offset = AbiFieldLayout.offset(field, width)
			$assertions = Str.concat($assertions, "ROC_STATIC_ASSERT(offsetof(${type_name}, ${field_name}) == ${U64.to_str(offset)}, \"${type_name}.${field_name} offset mismatch\");\n")
		}
	}
	$assertions
}

generate_record_type_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, Str, AbiLayout -> Str
generate_record_type_decl = |type_table, duplicate_records, duplicate_tags, preferred_names, type_name, abi_layout| {
	fields = abi_record_fields_c(abi_layout)
	if List.is_empty(fields) {
		return generate_opaque_type_decl(type_name, abi_layout.size64, abi_layout.alignment64, abi_layout.size32, abi_layout.alignment32)
	}

	wrap_vector_members = c_needs_vector_union_spelling(type_table, fields)
	fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, fields, Pointer64, wrap_vector_members)
	fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, fields, Pointer32, wrap_vector_members)
	assertions64 = Str.concat(static_asserts(type_name, abi_layout.size64, abi_layout.alignment64), c_record_offset_assertions(type_name, fields, Pointer64))
	assertions32 = Str.concat(static_asserts(type_name, abi_layout.size32, abi_layout.alignment32), c_record_offset_assertions(type_name, fields, Pointer32))
	comment = if wrap_vector_members {
		"${c_vector_union_comment}\n"
	} else {
		""
	}
	decl =
		\\#if UINTPTR_MAX == UINT64_MAX
		\\struct ${type_name} {
		\\${fields64}};
		\\${assertions64}#else
		\\struct ${type_name} {
		\\${fields32}};
		\\${assertions32}#endif
	"${comment}${decl}\n\n"
}

generate_opaque_type_decl : Str, U64, U64, U64, U64 -> Str
generate_opaque_type_decl = |type_name, size64, alignment64, size32, alignment32| {
	byte_count64 = if size64 == 0 {
		1
	} else {
		size64
	}

	type_alignment64 = if alignment64 == 0 {
		1
	} else {
		alignment64
	}

	byte_count32 = if size32 == 0 {
		1
	} else {
		size32
	}

	type_alignment32 = if alignment32 == 0 {
		1
	} else {
		alignment32
	}

	decl =
		\\#if UINTPTR_MAX == UINT64_MAX
		\\struct ${type_name} {
		\\    ROC_ALIGNAS(${U64.to_str(type_alignment64)}) uint8_t bytes[${U64.to_str(byte_count64)}];
		\\};
		\\${static_asserts(type_name, size64, alignment64)}#else
		\\struct ${type_name} {
		\\    ROC_ALIGNAS(${U64.to_str(type_alignment32)}) uint8_t bytes[${U64.to_str(byte_count32)}];
		\\};
		\\${static_asserts(type_name, size32, alignment32)}#endif
	"${decl}\n\n"
}

static_asserts : Str, U64, U64 -> Str
static_asserts = |type_name, size, alignment|
	if size > 0 {
		"ROC_STATIC_ASSERT(sizeof(${type_name}) == ${U64.to_str(size)}, \"${type_name} size mismatch\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(${type_name}) == ${U64.to_str(alignment)}, \"${type_name} alignment mismatch\");\n"
	} else {
		""
	}

add_type_alias_c : { content : Str, seen : List(Str) }, Str, Str -> { content : Str, seen : List(Str) }
add_type_alias_c = |state, alias, target| {
	if alias == target or List.contains(state.seen, alias) {
		state
	} else {
		{
			content: Str.concat(state.content, "typedef ${target} ${alias};\n"),
			seen: state.seen.append(alias),
		}
	}
}

generate_platform_type_aliases_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_platform_type_aliases_c = |hosted_functions, provides_list, type_table, duplicate_records, duplicate_tags, preferred_names| {
	var $state = { content: "", seen: generated_type_names_c(type_table, duplicate_records, duplicate_tags) }
	name_plan = TypeNamePlan.from_table(type_table)

	for plan in name_plan.alias_plan(type_alias_roots_c(hosted_functions, provides_list, type_table)) {
		target = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, plan.type_id)
		$state = add_type_alias_c($state, plan.alias, target)
	}

	if $state.content == "" {
		""
	} else {
		section("Platform Type Aliases", $state.content)
	}
}

generate_all_args_structs : List(HostedFunctionInfo), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_all_args_structs = |hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names| {
	var $args_structs = ""
	for func in hosted_functions {
		$args_structs = Str.concat($args_structs, generate_args_struct(func, type_table, duplicate_records, duplicate_tags, preferred_names))
	}
	$args_structs
}

generate_args_struct : HostedFunctionInfo, TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_args_struct = |func, type_table, duplicate_records, duplicate_tags, preferred_names| {
	struct_name = name_to_struct_name(func.name)
	arg_shape = ArgShape.from_table(type_table)

	match arg_shape.hosted_args(func) {
		NoMeaningfulArgs => ""
		SingleRecordArg(record) => {
			wrap_vector_members = c_needs_vector_union_spelling(type_table, record.fields)
			fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, record.fields, Pointer64, wrap_vector_members)
			fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, record.fields, Pointer32, wrap_vector_members)

			doc = doc_comment([
				"Arguments for ${func.name}",
				"Roc signature: ${func.type_str}",
				"Refcounted fields are owned by the hosted function.",
			])

			args_name = "${struct_name}Args"
			assertions64 = static_asserts(args_name, record.layout.size64, record.layout.alignment64)
			assertions32 = static_asserts(args_name, record.layout.size32, record.layout.alignment32)
			decl =
				\\${doc}#if UINTPTR_MAX == UINT64_MAX
				\\typedef struct {
				\\${fields64}} ${args_name};
				\\${assertions64}#else
				\\typedef struct {
				\\${fields32}} ${args_name};
				\\${assertions32}#endif
			"${decl}\n\n"
		}
		PositionalArgs(arg_type_ids) => {
			var $positional_fields = ""
			var $idx = 0
			for arg_type_id in arg_type_ids {
				c_type = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_id)
				$positional_fields = Str.concat($positional_fields, "    ${c_type} arg${U64.to_str($idx)};\n")
				$idx = $idx + 1
			}

			doc = doc_comment([
				"Arguments for ${func.name}",
				"Roc signature: ${func.type_str}",
				"Refcounted fields are owned by the hosted function.",
			])

			"${doc}typedef struct {\n${$positional_fields}} ${struct_name}Args;\n\n"
		}
	}
}

direct_param_list : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, List(U64) -> Str
direct_param_list = |type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_ids| {
	var $params = ""
	var $idx = 0
	arg_shape = ArgShape.from_table(type_table)

	for arg_type_id in arg_shape.positional_non_unit_type_ids(arg_type_ids) {
		arg_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_id)
		sep = if $params == "" {
			""
		} else {
			", "
		}
		$params = "${$params}${sep}${arg_c} arg${U64.to_str($idx)}"
		$idx = $idx + 1
	}

	if $params == "" {
		"void"
	} else {
		$params
	}
}

direct_hosted_param_list : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, HostedFunctionInfo -> Str
direct_hosted_param_list = |type_table, duplicate_records, duplicate_tags, preferred_names, func| {
	arg_shape = ArgShape.from_table(type_table)
	use_args_wrapper = arg_shape.single_arg_is_anonymous_record(func.arg_type_ids)

	var $params = ""
	var $idx = 0

	for arg_type_id in arg_shape.positional_non_unit_type_ids(func.arg_type_ids) {
		arg_c = if use_args_wrapper {
			"${name_to_struct_name(func.name)}Args"
		} else {
			type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_id)
		}
		sep = if $params == "" {
			""
		} else {
			", "
		}
		$params = "${$params}${sep}${arg_c} arg${U64.to_str($idx)}"
		$idx = $idx + 1
	}

	if $params == "" {
		"void"
	} else {
		$params
	}
}

generate_hosted_symbol_decls : List(HostedFunctionInfo), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_hosted_symbol_decls = |hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $decls = ""
	for func in hosted_functions {
		params = direct_hosted_param_list(type_table, duplicate_records, duplicate_tags, preferred_names, func)
		ret_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, func.ret_type_id)
		$decls = Str.concat($decls, "/* ${func.name}: ${func.type_str} */\nextern ${ret_c} ${func.ffi_symbol}(${params});\n\n")
	}

	section("Hosted Symbols", $decls)
}

generate_provided_symbol_decls : List(ProvidesEntry), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_provided_symbol_decls = |provides_list, type_table, duplicate_records, duplicate_tags, preferred_names| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $decls = ""
	for entry in provides_list {
		type_repr = type_table.get(entry.type_id)
		match type_repr {
			RocFunction(func) => {
				params = direct_param_list(type_table, duplicate_records, duplicate_tags, preferred_names, func.args)
				ret_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, func.ret)
				$decls = Str.concat($decls, "/* Entrypoint: ${entry.name} */\nextern ${ret_c} ${entry.ffi_symbol}(${params});\n\n")
			}
			_ => {
				value_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, entry.type_id)
				$decls = Str.concat($decls, "/* Static provided value: ${entry.name} */\nextern const ${value_c} ${entry.ffi_symbol};\n\n")
			}
		}
	}

	section("Provided Symbols", $decls)
}

generate_hosted_fn_fields : List(HostedFunctionInfo) -> Str
generate_hosted_fn_fields = |hosted_functions| {
	var $fields = ""
	var $first = Bool.True

	for f in hosted_functions {
		field_name = name_to_c_field_ident(f.name)
		c_func_name = name_to_c_func_name(f.name)
		if !$first {
			$fields = Str.concat($fields, "\n")
		}
		$fields = Str.concat($fields, "    HostedFn ${field_name};  /* index ${U64.to_str(f.index)}, C name: ${c_func_name} */")
		$first = Bool.False
	}

	$fields
}

# =============================================================================
# Header Sections
# =============================================================================

section : Str, Str -> Str
section = |title, body|
	"// ${title}\n\n${body}\n"

doc_comment : List(Str) -> Str
doc_comment = |lines| {
	var $result = "/**\n"
	for line in lines {
		if line == "" {
			$result = Str.concat($result, " *\n")
		} else {
			$result = Str.concat($result, " * ${line}\n")
		}
	}

	Str.concat($result, " */\n")
}

header_guard_top : Str
header_guard_top = {
	header_doc = doc_comment([
		"Roc Platform ABI Header",
		"",
		"This file defines C declarations for a Roc platform's direct symbol ABI.",
		"It is automatically generated by the Roc glue generator.",
		"",
		"Hosted argument ownership:",
		"Roc transfers ownership of refcounted arguments to the hosted function.",
		"The hosted function must decref owned refcounted arguments when done,",
		"or retain/transfer ownership explicitly when storing or returning them.",
	])

	"${header_doc}\n#ifndef ROC_PLATFORM_ABI_H\n#define ROC_PLATFORM_ABI_H\n\n"
}

includes_section : Str
includes_section =
	"#include <stdbool.h>\n#include <stddef.h>\n#include <stdint.h>\n\n#if defined(__cplusplus)\n#define ROC_ALIGNAS(n) alignas(n)\n#define ROC_ALIGNOF(T) alignof(T)\n#define ROC_STATIC_ASSERT(cond, message) static_assert(cond, message)\n#else\n#define ROC_ALIGNAS(n) _Alignas(n)\n#define ROC_ALIGNOF(T) _Alignof(T)\n#define ROC_STATIC_ASSERT(cond, message) _Static_assert(cond, message)\n#endif\n\nstatic inline void roc_abi_copy_bytes(void* dst, const void* src, size_t count) {\n    uint8_t* dst_bytes = (uint8_t*)dst;\n    const uint8_t* src_bytes = (const uint8_t*)src;\n    for (size_t index = 0; index < count; index++) {\n        dst_bytes[index] = src_bytes[index];\n    }\n}\n\n"

extern_c_start : Str
extern_c_start =
	"#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"

extern_c_end : Str
extern_c_end =
	"\n#ifdef __cplusplus\n}\n#endif\n\n"

header_guard_bottom : Str
header_guard_bottom =
	"#endif /* ROC_PLATFORM_ABI_H */\n"

core_types_section : Str
core_types_section = {
	roc_dec_def = "typedef struct {\n    __int128 num;\n} RocDec;\n\nROC_STATIC_ASSERT(sizeof(RocDec) == 16, \"RocDec must be sixteen bytes\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocDec) == 16, \"RocDec must be 16-byte aligned\");\n\n"
	roc_vector_defs = "#if defined(_MSC_VER) && defined(_M_X64)\n#include <intrin.h>\ntypedef __m128i RocU8x16;\ntypedef __m128i RocI8x16;\ntypedef __m128i RocU16x8;\ntypedef __m128i RocI16x8;\ntypedef __m128i RocU32x4;\ntypedef __m128i RocI32x4;\ntypedef __m128i RocU64x2;\ntypedef __m128i RocI64x2;\n#elif defined(_M_ARM64)\n#include <arm64_neon.h>\ntypedef uint8x16_t RocU8x16;\ntypedef int8x16_t RocI8x16;\ntypedef uint16x8_t RocU16x8;\ntypedef int16x8_t RocI16x8;\ntypedef uint32x4_t RocU32x4;\ntypedef int32x4_t RocI32x4;\ntypedef uint64x2_t RocU64x2;\ntypedef int64x2_t RocI64x2;\n#elif defined(__aarch64__)\n#include <arm_neon.h>\ntypedef uint8x16_t RocU8x16;\ntypedef int8x16_t RocI8x16;\ntypedef uint16x8_t RocU16x8;\ntypedef int16x8_t RocI16x8;\ntypedef uint32x4_t RocU32x4;\ntypedef int32x4_t RocI32x4;\ntypedef uint64x2_t RocU64x2;\ntypedef int64x2_t RocI64x2;\n#elif defined(__wasm_simd128__)\n#include <wasm_simd128.h>\ntypedef v128_t RocU8x16;\ntypedef v128_t RocI8x16;\ntypedef v128_t RocU16x8;\ntypedef v128_t RocI16x8;\ntypedef v128_t RocU32x4;\ntypedef v128_t RocI32x4;\ntypedef v128_t RocU64x2;\ntypedef v128_t RocI64x2;\n#else\ntypedef uint8_t RocU8x16 __attribute__((vector_size(16)));\ntypedef int8_t RocI8x16 __attribute__((vector_size(16)));\ntypedef uint16_t RocU16x8 __attribute__((vector_size(16)));\ntypedef int16_t RocI16x8 __attribute__((vector_size(16)));\ntypedef uint32_t RocU32x4 __attribute__((vector_size(16)));\ntypedef int32_t RocI32x4 __attribute__((vector_size(16)));\ntypedef uint64_t RocU64x2 __attribute__((vector_size(16)));\ntypedef int64_t RocI64x2 __attribute__((vector_size(16)));\n#endif\n\n#define ROC_ASSERT_VECTOR(T) ROC_STATIC_ASSERT(sizeof(T) == 16, #T \" size mismatch\"); ROC_STATIC_ASSERT(ROC_ALIGNOF(T) == 16, #T \" alignment mismatch\")\nROC_ASSERT_VECTOR(RocU8x16);\nROC_ASSERT_VECTOR(RocI8x16);\nROC_ASSERT_VECTOR(RocU16x8);\nROC_ASSERT_VECTOR(RocI16x8);\nROC_ASSERT_VECTOR(RocU32x4);\nROC_ASSERT_VECTOR(RocI32x4);\nROC_ASSERT_VECTOR(RocU64x2);\nROC_ASSERT_VECTOR(RocI64x2);\n#undef ROC_ASSERT_VECTOR\n\n"
	roc_vector_defs_clang_x64 = RocName.replace_all(roc_vector_defs, "#if defined(_MSC_VER) && defined(_M_X64)", "#if defined(_MSC_VER) && !defined(__clang__) && defined(_M_X64)")
	roc_vector_defs_portable = RocName.replace_all(roc_vector_defs_clang_x64, "#elif defined(_M_ARM64)", "#elif defined(_MSC_VER) && !defined(__clang__) && defined(_M_ARM64)")

	roc_str_def = "typedef struct {\n    uint8_t* bytes;\n    size_t capacity_or_alloc_ptr;\n    size_t length;\n} RocStr;\n\nROC_STATIC_ASSERT(sizeof(RocStr) == 3 * sizeof(size_t), \"RocStr must be three pointer-sized words\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocStr) == ROC_ALIGNOF(size_t), \"RocStr must be pointer-word aligned\");\nROC_STATIC_ASSERT(offsetof(RocStr, bytes) == 0, \"RocStr.bytes offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocStr, capacity_or_alloc_ptr) == sizeof(size_t), \"RocStr.capacity_or_alloc_ptr offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocStr, length) == 2 * sizeof(size_t), \"RocStr.length offset mismatch\");\n\n"

	roc_list_def = "typedef struct {\n    void* elements;\n    size_t length;\n    size_t capacity_or_alloc_ptr;\n} RocList;\n\nROC_STATIC_ASSERT(sizeof(RocList) == 3 * sizeof(size_t), \"RocList must be three pointer-sized words\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocList) == ROC_ALIGNOF(size_t), \"RocList must be pointer-word aligned\");\nROC_STATIC_ASSERT(offsetof(RocList, elements) == 0, \"RocList.elements offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocList, length) == sizeof(size_t), \"RocList.length offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocList, capacity_or_alloc_ptr) == 2 * sizeof(size_t), \"RocList.capacity_or_alloc_ptr offset mismatch\");\n\n"

	roc_box_def = "typedef void* RocBox;\n\n"

	erased_callable_def =
		"struct RocOps;\n\n"
			.concat("typedef void (*RocErasedCallableFn)(struct RocOps* ops, uint8_t* ret, const uint8_t* args, uint8_t* capture);\n")
			.concat("typedef void (*RocErasedCallableOnDrop)(uint8_t* capture, struct RocOps* ops);\n")
			.concat("typedef struct {\n    RocErasedCallableFn callable_fn_ptr;\n    RocErasedCallableOnDrop on_drop;\n} RocErasedCallablePayload;\n")
			.concat("ROC_STATIC_ASSERT(offsetof(RocErasedCallablePayload, callable_fn_ptr) == 0, \"RocErasedCallablePayload.callable_fn_ptr offset mismatch\");\n")
			.concat("ROC_STATIC_ASSERT(offsetof(RocErasedCallablePayload, on_drop) == sizeof(RocErasedCallableFn), \"RocErasedCallablePayload.on_drop offset mismatch\");\n")
			.concat("typedef uint8_t* RocErasedCallable;\n")
			.concat("#define ROC_ERASED_CALLABLE_CAPTURE_ALIGNMENT 16u\n")
			.concat("#define ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT 16u\n")
			.concat("#define ROC_ERASED_CALLABLE_CAPTURE_OFFSET ((sizeof(RocErasedCallablePayload) + 15u) & ~15u)\n")
			.concat("#define ROC_ERASED_CALLABLE_PAYLOAD_SIZE(capture_size) (ROC_ERASED_CALLABLE_CAPTURE_OFFSET + (capture_size))\n")
			.concat("static inline RocErasedCallablePayload* roc_erased_callable_payload_ptr(RocErasedCallable callable) {\n    return (RocErasedCallablePayload*)callable;\n}\n")
			.concat("static inline uint8_t* roc_erased_callable_capture_ptr(RocErasedCallable callable) {\n    return callable == 0 ? 0 : callable + ROC_ERASED_CALLABLE_CAPTURE_OFFSET;\n}\n\n")

	section("Core Roc Types", "${roc_dec_def}${roc_vector_defs_portable}${roc_str_def}${roc_list_def}${roc_box_def}${erased_callable_def}")
}

hosted_fn_infrastructure : Str
hosted_fn_infrastructure = {
	roc_ops_decl = "struct RocOps;\n\n"
	hosted_fn_typedef = "typedef void (*HostedFn)(void);\n\n"

	section("Hosted Function Infrastructure", "${roc_ops_decl}${hosted_fn_typedef}")
}

function_count_section : U64 -> Str
function_count_section = |count| {
	count_define = "#define HOSTED_FUNCTION_COUNT ${U64.to_str(count)}\n\n"
	section("Hosted Function Count", count_define)
}

args_structs_header : Str
args_structs_header =
	section("Argument Structures", "")

hosted_functions_registry : Str -> Str
hosted_functions_registry = |fields| {
	registry_doc = doc_comment([
		"Registry of all hosted function implementations.",
		"Store each implementation cast to HostedFn.",
	])
	registry_typedef = "typedef struct {\n${fields}\n} HostedFunctions;\n"

	section("HostedFunctions Registry", "${registry_doc}${registry_typedef}")
}

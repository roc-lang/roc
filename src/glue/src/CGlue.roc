## A glue script for generating a C header file.
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]
import pf.TypeRepr exposing [TypeRepr]
import pf.RecordField exposing [RecordField]
import pf.TagUnionRepr exposing [TagUnionRepr]
import pf.ProvidesEntry exposing [ProvidesEntry]
import pf.TypeTable exposing [TypeTable]
import pf.RocName exposing [RocName]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	var $hosted_functions = []
	var $type_table = []
	var $provides_entries = []

	for types in types_list {
		$type_table = types.type_table
		$provides_entries = types.provides_entries

		for mod in types.modules {
			for func in mod.hosted_functions {
				full_qualified_name = "${mod.name}.${func.name}"

				hosted_func = {
					arg_type_ids: func.arg_type_ids,
					ffi_symbol: func.ffi_symbol,
					index: func.index,
					name: full_qualified_name,
					ret_type_id: func.ret_type_id,
					type_str: func.type_str,
				}

				$hosted_functions = $hosted_functions.append(hosted_func)
			}
		}
	}

	sorted = List.sort_with($hosted_functions, compare_by_index)
	header_content = generate_c_header(sorted, $type_table, $provides_entries)

	Ok([{ name: "roc_platform_abi.h", content: header_content }])
}

compare_by_index = |a, b| {
	if a.index < b.index {
		return LT
	}
	if a.index > b.index {
		return GT
	}
	EQ
}

# =============================================================================
# TypeRepr-based C Type Mapping
# =============================================================================

type_id_to_c : List(TypeRepr), List(Str), List(Str), U64 -> Str
type_id_to_c = |type_table, duplicate_record_names, duplicate_tag_names, type_id| {
	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	type_repr_to_c(type_table, duplicate_record_names, duplicate_tag_names, type_id, type_repr)
}

type_repr_to_c : List(TypeRepr), List(Str), List(Str), U64, TypeRepr -> Str
type_repr_to_c = |type_table, duplicate_record_names, duplicate_tag_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match TypeTable.get(TypeTable.from_list(type_table), inner_id) {
				RocFunction(_) => "RocErasedCallable"
				RocUnknown(_) => "RocBox"
				_ => {
					inner_c = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, inner_id)
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
		RocDec => "double"
		RocList(_) => "RocList"
		RocRecord(rec) =>
			if rec.name == "" {
				"void*"
			} else {
				record_struct_name(duplicate_record_names, type_id, rec)
			}
		RocTagUnion(tu) => resolve_tag_union_type_c(type_table, duplicate_record_names, duplicate_tag_names, type_id, tu)
		RocFunction(_) => "void*"
		RocUnknown(_) => "void*"
	}
}

resolve_tag_union_type_c = |type_table, duplicate_record_names, duplicate_tag_names, type_id, tu| {
	match TypeTable.single_variant_payload(tu) {
		SinglePayload(payload_id) => type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, payload_id)
		SingleNoPayload => "void"
		NotSingleVariant =>
			if tu.name != "" {
				tag_union_struct_name(duplicate_tag_names, type_id, tu)
			} else {
				"void*"
			}
	}
}

c_record_field_decl : List(TypeRepr), List(Str), List(Str), RecordField -> Str
c_record_field_decl = |type_table, duplicate_record_names, duplicate_tag_names, field| {
	field_name = name_to_c_field_ident(field.name)
	if field.is_padding {
		"    uint8_t ${field_name}[${U64.to_str(field.size)}];\n"
	} else {
		c_type = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, field.type_id)
		"    ${c_type} ${field_name};\n"
	}
}

duplicate_record_names : List(TypeRepr) -> List(Str)
duplicate_record_names = |type_table| {
	var $seen_names = []
	var $duplicates = []

	for type_repr in type_table {
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" {
					if List.contains($seen_names, rec.name) {
						if !(List.contains($duplicates, rec.name)) {
							$duplicates = $duplicates.append(rec.name)
						}
					} else {
						$seen_names = $seen_names.append(rec.name)
					}
				}
			_ => {}
		}
	}

	$duplicates
}

record_struct_name = |duplicate_names, type_id, rec| {
	base = name_to_struct_name(rec.name)
	if List.contains(duplicate_names, rec.name) {
		"${base}Type${U64.to_str(type_id)}"
	} else {
		base
	}
}

duplicate_tag_union_names : List(TypeRepr) -> List(Str)
duplicate_tag_union_names = |type_table| TypeTable.duplicate_tag_union_names(TypeTable.from_list(type_table))

tag_union_struct_name : List(Str), U64, TagUnionRepr -> Str
tag_union_struct_name = |duplicate_names, type_id, tu| {
	base = name_to_struct_name(tu.name)
	if List.contains(duplicate_names, tu.name) {
		"${base}Type${U64.to_str(type_id)}"
	} else {
		base
	}
}

disc_type_for_count = |count| {
	if count <= 256 {
		"uint8_t"
	} else if count <= 65536 {
		"uint16_t"
	} else {
		"uint32_t"
	}
}

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
name_to_struct_name = |name| {
	parts = Str.split_on(name, ".")

	var $result = ""
	for part in parts {
		for subpart in Str.split_on(part, "_") {
			cleaned = subpart
				->str_replace_all("!", "")
				->str_replace_all("-", "")
				->str_replace_all(" ", "")

			if cleaned != "" {
				$result = Str.concat($result, capitalize_first(cleaned))
			}
		}
	}

	if $result == "" {
		"Anon"
	} else {
		$result
	}
}

expect name_to_struct_name("Stdout.line!") == "StdoutLine"
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"
expect name_to_struct_name("__AnonStruct10") == "AnonStruct10"

name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->str_replace_all("-", "_")
		->str_replace_all(" ", "_")
		->to_screaming_snake_case()
}

expect name_to_upper_ident("Stdout.line!") == "STDOUT_LINE"
expect name_to_upper_ident("Foo.barBaz!") == "FOO_BAR_BAZ"

name_to_c_func_name : Str -> Str
name_to_c_func_name = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->str_replace_all("-", "_")
		->str_replace_all(" ", "_")
		->to_lower_snake_case()
}

expect name_to_c_func_name("Stdout.line!") == "stdout_line"
expect name_to_c_func_name("Foo.barBaz!") == "foo_bar_baz"

name_to_c_field_ident : Str -> Str
name_to_c_field_ident = |name| {
	sanitized =
		name
			->str_replace_all("!", "_bang")
			->str_replace_all("-", "_")
			->str_replace_all(".", "_")
			->str_replace_all(" ", "_")
			->to_lower_snake_case()

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

expect name_to_c_field_ident("init!") == "init_bang"
expect name_to_c_field_ident("type") == "type"
expect name_to_c_field_ident("struct") == "struct_field"

# =============================================================================
# Header Generation
# =============================================================================

generate_c_header : List({ arg_type_ids : List(U64), ffi_symbol : Str, index : U64, name : Str, ret_type_id : U64, type_str : Str }), List(TypeRepr), List(ProvidesEntry) -> Str
generate_c_header = |hosted_functions, type_table, provides_list| {
	duplicate_records = duplicate_record_names(type_table)
	duplicate_tags = duplicate_tag_union_names(type_table)

	defines = generate_defines(hosted_functions)
	count = List.len(hosted_functions)
	type_decls = generate_type_decls(type_table, duplicate_records, duplicate_tags)
	args_structs = generate_all_args_structs(hosted_functions, type_table, duplicate_records, duplicate_tags)
	hosted_fn_fields = generate_hosted_fn_fields(hosted_functions)
	hosted_symbol_decls = generate_hosted_symbol_decls(hosted_functions, type_table, duplicate_records, duplicate_tags)
	provided_symbol_decls = generate_provided_symbol_decls(provides_list, type_table, duplicate_records, duplicate_tags)

	header_guard_top
		.concat(includes_section)
		.concat(extern_c_start)
		.concat(core_types_section)
		.concat(type_decls)
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

generate_type_decls = |type_table, duplicate_records, duplicate_tags| {
	type_definitions = generate_opaque_type_decls(type_table, duplicate_records, duplicate_tags)

	if type_definitions == "" {
		""
	} else {
		section("Reflected Roc Types", type_definitions)
	}
}

generate_opaque_type_decls = |type_table, duplicate_records, duplicate_tags| {
	var $decls = ""
	var $seen_names = []
	var $type_id = 0

	for type_repr in type_table {
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" {
					type_name = record_struct_name(duplicate_records, $type_id, rec)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$decls = Str.concat($decls, generate_opaque_type_decl(type_name, rec.size, rec.alignment))
					}
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					type_name = tag_union_struct_name(duplicate_tags, $type_id, tu)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$decls = Str.concat($decls, generate_opaque_type_decl(type_name, tu.size, tu.alignment))
					}
				}
			_ => {}
		}
		$type_id = $type_id + 1
	}

	$decls
}

generate_opaque_type_decl = |type_name, size, alignment| {
	byte_count = if size == 0 {
		1
	} else {
		size
	}

	type_alignment = if alignment == 0 {
		1
	} else {
		alignment
	}

	"typedef struct {\n    ROC_ALIGNAS(${U64.to_str(type_alignment)}) uint8_t bytes[${U64.to_str(byte_count)}];\n} ${type_name};\n${native_64_static_asserts(type_name, size, alignment)}"
}

native_64_static_asserts = |type_name, size, alignment| {
	if size > 0 {
		"#if UINTPTR_MAX == UINT64_MAX\nROC_STATIC_ASSERT(sizeof(${type_name}) == ${U64.to_str(size)}, \"${type_name} size mismatch\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(${type_name}) == ${U64.to_str(alignment)}, \"${type_name} alignment mismatch\");\n#endif\n\n"
	} else {
		""
	}
}

generate_all_args_structs = |hosted_functions, type_table, duplicate_records, duplicate_tags| {
	var $args_structs = ""
	for func in hosted_functions {
		$args_structs = Str.concat($args_structs, generate_args_struct(func, type_table, duplicate_records, duplicate_tags))
	}
	$args_structs
}

generate_args_struct = |func, type_table, duplicate_records, duplicate_tags| {
	if !(has_meaningful_args(func, type_table)) {
		return ""
	}

	struct_name = name_to_struct_name(func.name)

	type_table_result = if List.len(func.arg_type_ids) == 1 {
		match List.first(func.arg_type_ids) {
			Ok(arg_id) => lookup_record_in_type_table(type_table, arg_id)
			Err(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		}
	} else {
		{ found: Bool.False, fields: [], size: 0, alignment: 0 }
	}

	if type_table_result.found {
		var $fields = ""
		for field in type_table_result.fields {
			$fields = Str.concat($fields, c_record_field_decl(type_table, duplicate_records, duplicate_tags, field))
		}

		doc = doc_comment([
			"Arguments for ${func.name}",
			"Roc signature: ${func.type_str}",
			"Refcounted fields are owned by the hosted function.",
		])

		args_name = "${struct_name}Args"
		args_assertions = native_64_static_asserts(args_name, type_table_result.size, type_table_result.alignment)
		return "${doc}typedef struct {\n${$fields}} ${args_name};\n${args_assertions}"
	}

	var $positional_fields = ""
	var $idx = 0
	for arg_type_id in func.arg_type_ids {
		if !TypeTable.is_unit(TypeTable.from_list(type_table), arg_type_id) {
			c_type = type_id_to_c(type_table, duplicate_records, duplicate_tags, arg_type_id)
			$positional_fields = Str.concat($positional_fields, "    ${c_type} arg${U64.to_str($idx)};\n")
			$idx = $idx + 1
		}
	}

	doc = doc_comment([
		"Arguments for ${func.name}",
		"Roc signature: ${func.type_str}",
		"Refcounted fields are owned by the hosted function.",
	])

	"${doc}typedef struct {\n${$positional_fields}} ${struct_name}Args;\n\n"
}

lookup_record_in_type_table = |type_table, type_id| {
	match TypeTable.record_layout(TypeTable.from_list(type_table), type_id) {
		RecordFound(layout) => { found: Bool.True, fields: layout.fields, size: layout.size, alignment: layout.alignment }
		NotRecord => { found: Bool.False, fields: [], size: 0, alignment: 0 }
	}
}

has_meaningful_args = |func, type_table| {
	if List.is_empty(func.arg_type_ids) {
		Bool.False
	} else if List.len(func.arg_type_ids) == 1 {
		match List.first(func.arg_type_ids) {
			Ok(id) => !(TypeTable.is_unit(TypeTable.from_list(type_table), id))
			_ => Bool.False
		}
	} else {
		Bool.True
	}
}

direct_param_list = |type_table, duplicate_records, duplicate_tags, arg_type_ids| {
	var $params = ""
	var $idx = 0

	for arg_type_id in arg_type_ids {
		if !TypeTable.is_unit(TypeTable.from_list(type_table), arg_type_id) {
			arg_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, arg_type_id)
			sep = if $params == "" {
				""
			} else {
				", "
			}
			$params = "${$params}${sep}${arg_c} arg${U64.to_str($idx)}"
			$idx = $idx + 1
		}
	}

	if $params == "" {
		"void"
	} else {
		$params
	}
}

direct_hosted_param_list = |type_table, duplicate_records, duplicate_tags, func| {
	use_args_wrapper =
		if List.len(func.arg_type_ids) == 1 {
			match List.first(func.arg_type_ids) {
				Ok(arg_id) => TypeTable.is_anonymous_record(TypeTable.from_list(type_table), arg_id)
				Err(_) => Bool.False
			}
		} else {
			Bool.False
		}

	var $params = ""
	var $idx = 0

	for arg_type_id in func.arg_type_ids {
		if !TypeTable.is_unit(TypeTable.from_list(type_table), arg_type_id) {
			arg_c = if use_args_wrapper {
				"${name_to_struct_name(func.name)}Args"
			} else {
				type_id_to_c(type_table, duplicate_records, duplicate_tags, arg_type_id)
			}
			sep = if $params == "" {
				""
			} else {
				", "
			}
			$params = "${$params}${sep}${arg_c} arg${U64.to_str($idx)}"
			$idx = $idx + 1
		}
	}

	if $params == "" {
		"void"
	} else {
		$params
	}
}

generate_hosted_symbol_decls = |hosted_functions, type_table, duplicate_records, duplicate_tags| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $decls = ""
	for func in hosted_functions {
		params = direct_hosted_param_list(type_table, duplicate_records, duplicate_tags, func)
		ret_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, func.ret_type_id)
		$decls = Str.concat($decls, "/* ${func.name}: ${func.type_str} */\nextern ${ret_c} ${func.ffi_symbol}(${params});\n\n")
	}

	section("Hosted Symbols", $decls)
}

generate_provided_symbol_decls = |provides_list, type_table, duplicate_records, duplicate_tags| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $decls = ""
	for entry in provides_list {
		type_repr = TypeTable.get(TypeTable.from_list(type_table), entry.type_id)
		match type_repr {
			RocFunction(func) => {
				params = direct_param_list(type_table, duplicate_records, duplicate_tags, func.args)
				ret_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, func.ret)
				$decls = Str.concat($decls, "/* Entrypoint: ${entry.name} */\nextern ${ret_c} ${entry.ffi_symbol}(${params});\n\n")
			}
			_ => {
				value_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, entry.type_id)
				$decls = Str.concat($decls, "/* Static provided value: ${entry.name} */\nextern const ${value_c} ${entry.ffi_symbol};\n\n")
			}
		}
	}

	section("Provided Symbols", $decls)
}

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
	"// =============================================================================\n// ${title}\n// =============================================================================\n\n${body}\n"

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
	"#include <stdbool.h>\n#include <stddef.h>\n#include <stdint.h>\n\n#if defined(__cplusplus)\n#define ROC_ALIGNAS(n) alignas(n)\n#define ROC_ALIGNOF(T) alignof(T)\n#define ROC_STATIC_ASSERT(cond, message) static_assert(cond, message)\n#else\n#define ROC_ALIGNAS(n) _Alignas(n)\n#define ROC_ALIGNOF(T) _Alignof(T)\n#define ROC_STATIC_ASSERT(cond, message) _Static_assert(cond, message)\n#endif\n\n"

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
	roc_str_def = "typedef struct {\n    uint8_t* bytes;\n    size_t capacity_or_alloc_ptr;\n    size_t length;\n} RocStr;\n\nROC_STATIC_ASSERT(sizeof(RocStr) == 3 * sizeof(size_t), \"RocStr must be three pointer-sized words\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocStr) == ROC_ALIGNOF(size_t), \"RocStr must be pointer-word aligned\");\n\n"

	roc_list_def = "typedef struct {\n    void* elements;\n    size_t length;\n    size_t capacity_or_alloc_ptr;\n} RocList;\n\nROC_STATIC_ASSERT(sizeof(RocList) == 3 * sizeof(size_t), \"RocList must be three pointer-sized words\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocList) == ROC_ALIGNOF(size_t), \"RocList must be pointer-word aligned\");\n\n"

	roc_box_def = "typedef void* RocBox;\n\n"

	erased_callable_def =
		"struct RocOps;\n\n"
			.concat("typedef void (*RocErasedCallableFn)(struct RocOps* ops, uint8_t* ret, const uint8_t* args, uint8_t* capture);\n")
			.concat("typedef void (*RocErasedCallableOnDrop)(uint8_t* capture, struct RocOps* ops);\n")
			.concat("typedef struct {\n    RocErasedCallableFn callable_fn_ptr;\n    RocErasedCallableOnDrop on_drop;\n} RocErasedCallablePayload;\n")
			.concat("typedef uint8_t* RocErasedCallable;\n")
			.concat("#define ROC_ERASED_CALLABLE_CAPTURE_ALIGNMENT 16u\n")
			.concat("#define ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT 16u\n")
			.concat("#define ROC_ERASED_CALLABLE_CAPTURE_OFFSET ((sizeof(RocErasedCallablePayload) + 15u) & ~15u)\n")
			.concat("#define ROC_ERASED_CALLABLE_PAYLOAD_SIZE(capture_size) (ROC_ERASED_CALLABLE_CAPTURE_OFFSET + (capture_size))\n")
			.concat("static inline RocErasedCallablePayload* roc_erased_callable_payload_ptr(RocErasedCallable callable) {\n    return (RocErasedCallablePayload*)callable;\n}\n")
			.concat("static inline uint8_t* roc_erased_callable_capture_ptr(RocErasedCallable callable) {\n    return callable == 0 ? 0 : callable + ROC_ERASED_CALLABLE_CAPTURE_OFFSET;\n}\n\n")

	section("Core Roc Types", "${roc_str_def}${roc_list_def}${roc_box_def}${erased_callable_def}")
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

## A glue script for generating a C header file.
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]
import pf.TypeRepr exposing [TypeRepr]
import pf.AbiFieldLayout exposing [AbiFieldLayout]
import pf.AbiWidth exposing [AbiWidth]
import pf.ArgShape exposing [ArgShape]
import pf.GlueInput exposing [GlueInput]
import pf.HostedFunctionInfo exposing [HostedFunctionInfo]
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

type_id_to_c : TypeTable, List(Str), List(Str), U64 -> Str
type_id_to_c = |type_table, duplicate_record_names, duplicate_tag_names, type_id| {
	type_repr = type_table.get(type_id)
	type_repr_to_c(type_table, duplicate_record_names, duplicate_tag_names, type_id, type_repr)
}

type_repr_to_c : TypeTable, List(Str), List(Str), U64, TypeRepr -> Str
type_repr_to_c = |type_table, duplicate_record_names, duplicate_tag_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
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
		RocDec => "RocDec"
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

c_record_field_decl : TypeTable, List(Str), List(Str), AbiFieldLayout, AbiWidth -> Str
c_record_field_decl = |type_table, duplicate_record_names, duplicate_tag_names, field, width| {
	field_name = name_to_c_field_ident(field.name)
	if field.is_padding {
		# Padding fields are nonzero at both widths (asserted by the compiler).
		"    uint8_t ${field_name}[${U64.to_str(AbiFieldLayout.size(field, width))}];\n"
	} else {
		c_type = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, field.type_id)
		"    ${c_type} ${field_name};\n"
	}
}

## Fields arrive in committed layout order (valid at both pointer widths);
## only per-width padding byte counts differ between the two renderings.
c_record_fields_decl = |type_table, duplicate_record_names, duplicate_tag_names, fields, width| {
	var $field_strs = ""
	for field in fields {
		$field_strs = Str.concat($field_strs, c_record_field_decl(type_table, duplicate_record_names, duplicate_tag_names, field, width))
	}
	$field_strs
}

duplicate_record_names : TypeTable -> List(Str)
duplicate_record_names = |type_table| {
	var $seen_names = []
	var $duplicates = []

	for type_info in type_table.entries() {
		match type_info.repr {
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

duplicate_tag_union_names : TypeTable -> List(Str)
duplicate_tag_union_names = |type_table| type_table.duplicate_tag_union_names()

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
name_to_struct_name = |name| RocName.from_str(name).to_pascal_clean()

expect name_to_struct_name("Stdout.line!") == "StdoutLine"
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"
expect name_to_struct_name("__AnonStruct10") == "AnonStruct10"

name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| RocName.from_str(name).to_screaming_snake_identifier()

expect name_to_upper_ident("Stdout.line!") == "STDOUT_LINE"
expect name_to_upper_ident("Foo.barBaz!") == "FOO_BAR_BAZ"

name_to_c_func_name : Str -> Str
name_to_c_func_name = |name| RocName.from_str(name).to_lower_snake_identifier()

expect name_to_c_func_name("Stdout.line!") == "stdout_line"
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

expect name_to_c_field_ident("init!") == "init_bang"
expect name_to_c_field_ident("type") == "type"
expect name_to_c_field_ident("struct") == "struct_field"

# =============================================================================
# Header Generation
# =============================================================================

generate_c_header : List(HostedFunctionInfo), TypeTable, List(ProvidesEntry) -> Str
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

	for type_info in type_table.entries() {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					type_name = record_struct_name(duplicate_records, $type_id, rec)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$decls = Str.concat($decls, generate_opaque_type_decl(type_name, type_info.layout.size64, type_info.layout.alignment64, type_info.layout.size32, type_info.layout.alignment32))
					}
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					type_name = tag_union_struct_name(duplicate_tags, $type_id, tu)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$decls = Str.concat($decls, generate_opaque_type_decl(type_name, type_info.layout.size64, type_info.layout.alignment64, type_info.layout.size32, type_info.layout.alignment32))
					}
				}
			_ => {}
		}
		$type_id = $type_id + 1
	}

	$decls
}

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
		\\typedef struct {
		\\    ROC_ALIGNAS(${U64.to_str(type_alignment64)}) uint8_t bytes[${U64.to_str(byte_count64)}];
		\\} ${type_name};
		\\${static_asserts(type_name, size64, alignment64)}#else
		\\typedef struct {
		\\    ROC_ALIGNAS(${U64.to_str(type_alignment32)}) uint8_t bytes[${U64.to_str(byte_count32)}];
		\\} ${type_name};
		\\${static_asserts(type_name, size32, alignment32)}#endif
	"${decl}\n\n"
}

static_asserts = |type_name, size, alignment| {
	if size > 0 {
		"ROC_STATIC_ASSERT(sizeof(${type_name}) == ${U64.to_str(size)}, \"${type_name} size mismatch\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(${type_name}) == ${U64.to_str(alignment)}, \"${type_name} alignment mismatch\");\n"
	} else {
		""
	}
}

generate_all_args_structs : List(HostedFunctionInfo), TypeTable, List(Str), List(Str) -> Str
generate_all_args_structs = |hosted_functions, type_table, duplicate_records, duplicate_tags| {
	var $args_structs = ""
	for func in hosted_functions {
		$args_structs = Str.concat($args_structs, generate_args_struct(func, type_table, duplicate_records, duplicate_tags))
	}
	$args_structs
}

generate_args_struct : HostedFunctionInfo, TypeTable, List(Str), List(Str) -> Str
generate_args_struct = |func, type_table, duplicate_records, duplicate_tags| {
	struct_name = name_to_struct_name(func.name)
	arg_shape = ArgShape.from_table(type_table)

	match arg_shape.hosted_args(func) {
		NoMeaningfulArgs => ""
		SingleRecordArg(record) => {
			fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, record.fields, Pointer64)
			fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, record.fields, Pointer32)

			doc = doc_comment(
				[
					"Arguments for ${func.name}",
					"Roc signature: ${func.type_str}",
					"Refcounted fields are owned by the hosted function.",
				],
			)

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
				c_type = type_id_to_c(type_table, duplicate_records, duplicate_tags, arg_type_id)
				$positional_fields = Str.concat($positional_fields, "    ${c_type} arg${U64.to_str($idx)};\n")
				$idx = $idx + 1
			}

			doc = doc_comment(
				[
					"Arguments for ${func.name}",
					"Roc signature: ${func.type_str}",
					"Refcounted fields are owned by the hosted function.",
				],
			)

			"${doc}typedef struct {\n${$positional_fields}} ${struct_name}Args;\n\n"
		}
	}
}

direct_param_list = |type_table, duplicate_records, duplicate_tags, arg_type_ids| {
	var $params = ""
	var $idx = 0
	arg_shape = ArgShape.from_table(type_table)

	for arg_type_id in arg_shape.positional_non_unit_type_ids(arg_type_ids) {
		arg_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, arg_type_id)
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

direct_hosted_param_list : TypeTable, List(Str), List(Str), HostedFunctionInfo -> Str
direct_hosted_param_list = |type_table, duplicate_records, duplicate_tags, func| {
	arg_shape = ArgShape.from_table(type_table)
	use_args_wrapper = arg_shape.single_arg_is_anonymous_record(func.arg_type_ids)

	var $params = ""
	var $idx = 0

	for arg_type_id in arg_shape.positional_non_unit_type_ids(func.arg_type_ids) {
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

	if $params == "" {
		"void"
	} else {
		$params
	}
}

generate_hosted_symbol_decls : List(HostedFunctionInfo), TypeTable, List(Str), List(Str) -> Str
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
		type_repr = type_table.get(entry.type_id)
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
	header_doc = doc_comment(
		[
			"Roc Platform ABI Header",
			"",
			"This file defines C declarations for a Roc platform's direct symbol ABI.",
			"It is automatically generated by the Roc glue generator.",
			"",
			"Hosted argument ownership:",
			"Roc transfers ownership of refcounted arguments to the hosted function.",
			"The hosted function must decref owned refcounted arguments when done,",
			"or retain/transfer ownership explicitly when storing or returning them.",
		],
	)

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
	roc_dec_def = "typedef struct {\n    __int128 num;\n} RocDec;\n\nROC_STATIC_ASSERT(sizeof(RocDec) == 16, \"RocDec must be sixteen bytes\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocDec) == 16, \"RocDec must be 16-byte aligned\");\n\n"

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

	section("Core Roc Types", "${roc_dec_def}${roc_str_def}${roc_list_def}${roc_box_def}${erased_callable_def}")
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
	registry_doc = doc_comment(
		[
			"Registry of all hosted function implementations.",
			"Store each implementation cast to HostedFn.",
		],
	)
	registry_typedef = "typedef struct {\n${fields}\n} HostedFunctions;\n"

	section("HostedFunctions Registry", "${registry_doc}${registry_typedef}")
}

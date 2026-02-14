## A glue script for generating a C header file.
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	# Collect all hosted functions from all modules, with module name prefix
	var $hosted_functions = []

	for types in types_list {
		for mod in types.modules {
			for func in mod.hosted_functions {
				full_qualified_name = "${mod.name}.${func.name}"

				hosted_func = {
					index: func.index,
					name: full_qualified_name,
					type_str: func.type_str,
				}

				$hosted_functions = $hosted_functions.append(hosted_func)
			}
		}
	}

	header_content = generate_c_header($hosted_functions)

	Ok([{ name: "roc_platform_abi.h", content: header_content }])
}

# TODO This is simplistic and broken.
# But also, this kind of thing will always break for more complex signatures
# we need to use the actual parser
## Parse a type string like "Str => {}"
parse_type_str : Str -> { args : List(Str), ret : Str }
parse_type_str = |type_str| {
	parts = Str.split_on(type_str, " => ")

	match parts {
		[args_part, ret_part] => {
			args = parse_args(args_part)
			{ args, ret: ret_part }
		}

		_ => {
			thin_arrow_parts = Str.split_on(type_str, " -> ")
			match thin_arrow_parts {
				[args_part2, ret_part2] => {
					args = parse_args(args_part2)
					{ args, ret: ret_part2 }
				}

				_ => { args: [], ret: type_str }
			}
		}
	}
}

expect parse_type_str("Str => {}") == { args: ["Str"], ret: "{}" }
expect parse_type_str("{}") == { args: [], ret: "{}" }
expect parse_type_str("Str -> {}") == { args: ["Str"], ret: "{}" }
expect parse_type_str("Str, Str -> {}") == { args: ["Str", "Str"], ret: "{}" }

# Parse args portion of type string
parse_args : Str -> List(Str)
parse_args = |args_part| {
	trimmed = Str.trim(args_part)
	if trimmed == "()" or trimmed == "" or trimmed == "({})" or trimmed == "{}" {
		return []
	}

	is_parenthesized = Str.starts_with(trimmed, "(") and Str.ends_with(trimmed, ")")
	stripped = if is_parenthesized drop_first_last(trimmed) else trimmed

	# Handle comma-separated args like "Str, U64"
	var $result = []
	for s in Str.split_on(stripped, ", ") {
		t = Str.trim(s)
		if t != "" and t != "{}" {
			$result = $result.append(t)
		}
	}

	$result
}

expect parse_args("()") == []
expect parse_args("") == []
expect parse_args("({})") == []
expect parse_args("{}") == []
expect parse_args("Str") == ["Str"]
expect parse_args("(Str)") == ["Str"]
expect parse_args("Str, U64") == ["Str", "U64"]
expect parse_args("(Str, U64)") == ["Str", "U64"]
expect parse_args("Str, U64, Bool") == ["Str", "U64", "Bool"]

# Drop first and last character from string
drop_first_last : Str -> Str
drop_first_last = |s| {
	bytes = Str.to_utf8(s)
	len = List.len(bytes)
	if len <= 2 {
		return ""
	}

	# Drop first and last byte using sublist
	new_bytes = List.sublist(bytes, { start: 1, len: len - 2 })
	match Str.from_utf8(new_bytes) {
		Ok(str) => str
		# TODO is Roc source code guaranteed to be ascii?
		#   ie should this case crash? or handle ascii in a more structured way
		Err(_) => s
	}
}

expect drop_first_last("(Str)") == "Str"
expect drop_first_last("ab") == ""
expect drop_first_last("a") == ""
expect drop_first_last("") == ""
expect drop_first_last("(hello)") == "hello"

# Map a Roc type to its C equivalent
roc_type_to_c : Str -> Str
roc_type_to_c = |roc_type| {
	trimmed = Str.trim(roc_type)

	if trimmed == "{}" or trimmed == "()" or trimmed == "{  }" {
		return "void"
	}

	if Str.starts_with(trimmed, "List") {
		return "RocList"
	}

	match trimmed {
		"Str" => "RocStr"
		"Bool" => "bool"
		"U8" => "uint8_t"
		"U16" => "uint16_t"
		"U32" => "uint32_t"
		"U64" => "uint64_t"
		"U128" => "unsigned __int128"
		"I8" => "int8_t"
		"I16" => "int16_t"
		"I32" => "int32_t"
		"I64" => "int64_t"
		"I128" => "__int128"
		"F32" => "float"
		"F64" => "double"
		_ => "void*"
	}
}

expect roc_type_to_c("Str") == "RocStr"
expect roc_type_to_c("Bool") == "bool"
expect roc_type_to_c("U8") == "uint8_t"
expect roc_type_to_c("U16") == "uint16_t"
expect roc_type_to_c("U32") == "uint32_t"
expect roc_type_to_c("U64") == "uint64_t"
expect roc_type_to_c("I8") == "int8_t"
expect roc_type_to_c("I16") == "int16_t"
expect roc_type_to_c("I32") == "int32_t"
expect roc_type_to_c("I64") == "int64_t"
expect roc_type_to_c("F32") == "float"
expect roc_type_to_c("F64") == "double"
expect roc_type_to_c("{}") == "void"
expect roc_type_to_c("()") == "void"
expect roc_type_to_c("List(U8)") == "RocList"
expect roc_type_to_c("{ foo : Str }") == "void*"

# Convert function name to uppercase identifier (e.g., "Stdout.line!" -> "STDOUT_LINE")
name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_screaming_snake_case()
}

expect name_to_upper_ident("Stdout.line!") == "STDOUT_LINE"
expect name_to_upper_ident("line!") == "LINE"
expect name_to_upper_ident("Foo.barBaz!") == "FOO_BAR_BAZ"

# Replace all occurrences of a substring
str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| {
	parts = Str.split_on(s, from)
	Str.join_with(parts, to)
}

expect str_replace_all("a.b.c", ".", "_") == "a_b_c"
expect str_replace_all("hello!", "!", "") == "hello"
expect str_replace_all("no match", "x", "y") == "no match"

# Convert a string to SCREAMING_SNAKE_CASE
to_screaming_snake_case : Str -> Str
to_screaming_snake_case = |s| {
	bytes = Str.to_utf8(s)
	var $output = []
	var $prev_was_lower = Bool.False

	for byte in bytes {
		is_upper = byte >= 'A' and byte <= 'Z'
		is_lower = byte >= 'a' and byte <= 'z'

		if is_upper and $prev_was_lower {
			$output = $output.append('_')
		}

		new_byte = if is_lower to_uppercase(byte) else byte
		$output = $output.append(new_byte)

		$prev_was_lower = is_lower
	}

	match Str.from_utf8($output) {
		Ok(str) => str
		# TODO as above; figure out if this should crash
		Err(_) => s
	}
}

to_uppercase : U8 -> U8
to_uppercase = |ch| ch - 32

to_lowercase : U8 -> U8
to_lowercase = |ch| ch + 32

expect to_screaming_snake_case("fooBar") == "FOO_BAR"
expect to_screaming_snake_case("FooBar") == "FOO_BAR"
expect to_screaming_snake_case("foo") == "FOO"
expect to_screaming_snake_case("FOO") == "FOO"
expect to_screaming_snake_case("Stdout_line") == "STDOUT_LINE"

## Convert function name to a C-friendly struct name (e.g., "Stdout.line!" -> "StdoutLine")
## Keep module prefix to avoid conflicts between modules with same function names
name_to_struct_name : Str -> Str
name_to_struct_name = |name| {
	parts = Str.split_on(name, ".")

	var $result = ""
	for part in parts {
		capitalized = part
			->str_replace_all("!", "")
			->capitalize_first()
		$result = Str.concat($result, capitalized)
	}

	$result
}

expect name_to_struct_name("Stdout.line!") == "StdoutLine"
expect name_to_struct_name("line!") == "Line"
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"

capitalize_first : Str -> Str
capitalize_first = |s| {
	bytes = Str.to_utf8(s)
	if List.is_empty(bytes) {
		return ""
	}

	first = match List.first(bytes) {
		Ok(b) => b
		Err(_) => 0
	}
	first_is_lower = first >= 'a' and first <= 'z'
	new_first = if first_is_lower to_uppercase(first) else first
	rest = List.drop_first(bytes, 1)
	new_bytes = List.concat([new_first], rest)
	match Str.from_utf8(new_bytes) {
		Ok(str) => str
		Err(_) => s
	}
}

expect capitalize_first("hello") == "Hello"
expect capitalize_first("Hello") == "Hello"
expect capitalize_first("") == ""
expect capitalize_first("a") == "A"

# Convert function name to C field name with module prefix (e.g., "Stdout.line!" -> "Stdout_line")
name_to_field_name : Str -> Str
name_to_field_name = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
}

expect name_to_field_name("Stdout.line!") == "Stdout_line"
expect name_to_field_name("line!") == "line"
expect name_to_field_name("Foo.bar!") == "Foo_bar"

# Generate args struct for a function if it has arguments
# Enhanced with Roc type signature, field comments, and alignment assertions
generate_args_struct : { index : U64, name : Str, type_str : Str } -> Str
generate_args_struct = |func| {
	parsed = parse_type_str(func.type_str)

	if List.is_empty(parsed.args) {
		return ""
	}

	struct_name = name_to_struct_name(func.name)
	c_func_name = name_to_c_func_name(func.name)
	ret_c_type = roc_type_to_c(parsed.ret)

	var $fields = ""
	var $idx = 0
	for arg in parsed.args {
		c_type = roc_type_to_c(arg)
		if $idx > 0 {
			$fields = Str.concat($fields, "\n")
		}
		$fields = Str.concat(
			$fields,
			"    ${c_type} arg${U64.to_str($idx)};  // ${arg}",
		)
		$idx = $idx + 1
	}

	struct_doc = doc_comment(
		[
			"Arguments for ${func.name}",
			"Roc signature: ${func.type_str}",
			"C function name: ${c_func_name}",
			"Return type: ${ret_c_type}",
		],
	)
	struct_def = "typedef struct {\n${$fields}\n} ${struct_name}Args;\n\n"
	size_assert = "_Static_assert(sizeof(${struct_name}Args) > 0, \"${struct_name}Args must have non-zero size\");\n"
	align_assert = "_Static_assert(_Alignof(${struct_name}Args) >= 1, \"${struct_name}Args must be aligned\");\n\n"
	example = generate_example_impl(func.name, struct_name, parsed.args, ret_c_type)

	"${struct_doc}${struct_def}${size_assert}${align_assert}${example}"
}

## Convert function name to lowercase C function name
## (e.g., "Stdout.line!" -> "stdout_line")
name_to_c_func_name : Str -> Str
name_to_c_func_name = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_lower_snake_case()
}

expect name_to_c_func_name("Stdout.line!") == "stdout_line"
expect name_to_c_func_name("line!") == "line"
expect name_to_c_func_name("Foo.barBaz!") == "foo_bar_baz"

## Convert a string to lower_snake_case
to_lower_snake_case : Str -> Str
to_lower_snake_case = |s| {
	bytes = Str.to_utf8(s)
	var $output = []
	var $prev_was_lower = Bool.False

	for byte in bytes {
		is_upper = byte >= 'A' and byte <= 'Z'
		is_lower = byte >= 'a' and byte <= 'z'

		new_byte = if is_upper to_lowercase(byte) else byte

		if is_upper and $prev_was_lower {
			$output = $output.append('_')
		}
		$output = $output.append(new_byte)
		$prev_was_lower = is_lower
	}

	match Str.from_utf8($output) {
		Ok(str) => str
		Err(_) => s
	}
}

expect to_lower_snake_case("FooBar") == "foo_bar"
expect to_lower_snake_case("fooBar") == "foo_bar"
expect to_lower_snake_case("foo") == "foo"
expect to_lower_snake_case("FOO") == "foo"
expect to_lower_snake_case("Stdout_line") == "stdout_line"

## Generate example implementation comment for a hosted function
generate_example_impl : Str, Str, List(Str), Str -> Str
generate_example_impl = |func_name, struct_name, args, ret_c_type| {
	c_func_name = name_to_c_func_name(func_name)

	args_comment = if List.is_empty(args) {
		" *     // No arguments"
	} else {
		var $arg_lines = ""
		var $idx = 0
		for arg in args {
			c_type = roc_type_to_c(arg)
			$arg_lines = Str.concat(
				$arg_lines,
				" *     // args->arg${U64.to_str($idx)} is ${c_type} (${arg})\n",
			)
			$idx = $idx + 1
		}

		Str.trim_end($arg_lines)
	}

	ret_comment = if ret_c_type == "void" {
		" *     // No return value (void)"
	} else {
		" *     // Set return value: *((${ret_c_type}*)ret) = result;"
	}

	args_param = if List.is_empty(args) {
		"void* args"
	} else {
		"${struct_name}Args* args"
	}

	"/*\n * Example implementation:\n * void hosted_${c_func_name}(struct RocOps* ops, ${args_param}, void* ret) {\n${args_comment}\n${ret_comment}\n * }\n */\n\n"
}

## Generate the complete C header file
generate_c_header : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_c_header = |hosted_functions| {
	# Generate defines for function indices
	defines = generate_defines(hosted_functions)

	count = List.len(hosted_functions)

	# Generate args structs for functions with arguments
	args_structs = generate_all_args_structs(hosted_functions)

	# Generate HostedFunctions struct fields
	hosted_fn_fields = generate_hosted_fn_fields(hosted_functions)

	header_guard_top
		.concat(includes_section)
		.concat(extern_c_start)
		.concat(core_types_section)
		.concat(hosted_fn_infrastructure)
		.concat(function_count_section(count))
		.concat(defines)
		.concat("\n\n")
		.concat(args_structs_header)
		.concat(args_structs)
		.concat("\n")
		.concat(hosted_functions_registry(hosted_fn_fields))
		.concat(extern_c_end)
		.concat(header_guard_bottom)
}

## Generate defines for function indices
generate_defines : List({ index : U64, name : Str, type_str : Str }) -> Str
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

## Generate all args structs
generate_all_args_structs : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_all_args_structs = |hosted_functions| {
	var $args_structs = ""
	for f in hosted_functions {
		$args_structs = Str.concat($args_structs, generate_args_struct(f))
	}
	$args_structs
}

## Generate HostedFunctions struct fields
generate_hosted_fn_fields : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_hosted_fn_fields = |hosted_functions| {
	var $fields = ""
	var $first = Bool.True

	for f in hosted_functions {
		field_name = name_to_field_name(f.name)
		c_func_name = name_to_c_func_name(f.name)
		if !$first {
			$fields = Str.concat($fields, "\n")
		}
		$fields = Str.concat($fields, "    HostedFn ${field_name};  // index ${U64.to_str(f.index)}, C name: ${c_func_name}")
		$first = Bool.False
	}

	$fields
}

# Header sections as separate functions to avoid multi-line string issues

## Helper to generate a section with a banner comment
section : Str, Str -> Str
section = |title, body|
	"// =============================================================================\n// ${title}\n// =============================================================================\n\n${body}"

## Helper to generate a doc comment from a list of lines
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
			"This file defines the C interface for hosted functions in a Roc platform.",
			"It is automatically generated by the Roc glue generator.",
			"",
			"USAGE:",
			"1. Include this header in your platform host implementation",
			"2. Implement each hosted function according to its signature",
			"3. Register your implementations with the Roc runtime",
			"",
		],
	)

	"${header_doc}\n#ifndef ROC_PLATFORM_ABI_H\n#define ROC_PLATFORM_ABI_H\n\n"
}

includes_section : Str
includes_section = 
	"#include <stdbool.h>\n#include <stdint.h>\n#include <stddef.h>\n\n"

extern_c_start : Str
extern_c_start = 
	"#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"

extern_c_end : Str
extern_c_end = 
	"\n#ifdef __cplusplus\n}\n#endif\n\n"

header_guard_bottom : Str
header_guard_bottom = 
	"#endif // ROC_PLATFORM_ABI_H\n"

core_types_section : Str
core_types_section = {
	roc_str_doc = doc_comment(
		[
			"RocStr - Roc string type",
			"",
			"A 24-byte structure representing a string. Small strings (up to 23 bytes",
			"on 64-bit systems) are stored inline. Larger strings store a pointer to",
			"heap-allocated data.",
		],
	)
	roc_str_def = "typedef struct {\n    uint8_t* bytes;\n    size_t len;\n    size_t capacity;\n} RocStr;\n\n_Static_assert(sizeof(RocStr) == 24, \"RocStr must be 24 bytes\");\n_Static_assert(_Alignof(RocStr) == 8, \"RocStr must be 8-byte aligned\");\n\n"

	roc_list_doc = doc_comment(
		[
			"RocList - Roc list type",
			"",
			"A 24-byte structure representing a list. Similar to RocStr, but for",
			"arbitrary element types.",
		],
	)
	roc_list_def = 
		"typedef struct {\n    void* elements;\n    size_t len;\n    size_t capacity;\n} RocList;\n\n_Static_assert(sizeof(RocList) == 24, \"RocList must be 24 bytes\");\n_Static_assert(_Alignof(RocList) == 8, \"RocList must be 8-byte aligned\");\n\n"

	section("Core Roc Types", "${roc_str_doc}${roc_str_def}${roc_list_doc}${roc_list_def}")
}

hosted_fn_infrastructure : Str
hosted_fn_infrastructure = {
	roc_ops_doc = doc_comment(
		[
			"Forward declaration for RocOps",
			"This structure contains function pointers for Roc runtime operations",
			"(allocation, deallocation, etc.)",
		],
	)
	roc_ops_decl = "struct RocOps;\n\n"

	hosted_fn_doc = doc_comment(
		[
			"HostedFn - Function pointer type for hosted functions",
			"",
			"All hosted functions follow this signature:",
			"  - ops: pointer to Roc runtime operations",
			"  - args: pointer to function-specific arguments struct (or NULL if no args)",
			"  - ret: pointer to return value storage (or NULL if void return)",
		],
	)
	hosted_fn_typedef = 
		"typedef void (*HostedFn)(struct RocOps* ops, void* args, void* ret);\n\n"

	section(
		"Hosted Function Infrastructure",
		"${roc_ops_doc}${roc_ops_decl}${hosted_fn_doc}${hosted_fn_typedef}",
	)
}

function_count_section : U64 -> Str
function_count_section = |count| {
	count_doc = doc_comment(["Total number of hosted functions in this platform"])
	count_define = "#define HOSTED_FUNCTION_COUNT ${U64.to_str(count)}\n\n"

	indices_doc = doc_comment(
		[
			"Index constants for each hosted function",
			"Use these with the HostedFunctions struct to access specific functions",
		],
	)

	section(
		"Hosted Function Count and Indices",
		"${count_doc}${count_define}${indices_doc}",
	)
}

args_structs_header : Str
args_structs_header = 
	section("Argument Structures", "")

hosted_functions_registry : Str -> Str
hosted_functions_registry = |fields| {
	registry_doc = doc_comment(
		[
			"HostedFunctions - Registry of all hosted function implementations",
			"",
			"Platforms should create an instance of this struct and populate it with",
			"function pointers for each hosted function they implement.",
		],
	)
	registry_typedef = "typedef struct {\n${fields}\n} HostedFunctions;\n"

	section("HostedFunctions Registry", "${registry_doc}${registry_typedef}")
}

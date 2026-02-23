## A glue script for generating a Zig interface file.
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

	zig_content = generate_zig_file($hosted_functions)

	Ok([{ name: "roc_platform_abi.zig", content: zig_content }])
}

# =============================================================================
# Language-independent helpers (copied from CGlue.roc)
# =============================================================================

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

	new_bytes = List.sublist(bytes, { start: 1, len: len - 2 })
	match Str.from_utf8(new_bytes) {
		Ok(str) => str
		Err(_) => s
	}
}

expect drop_first_last("(Str)") == "Str"
expect drop_first_last("ab") == ""
expect drop_first_last("a") == ""
expect drop_first_last("") == ""
expect drop_first_last("(hello)") == "hello"

# Replace all occurrences of a substring
str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| {
	parts = Str.split_on(s, from)
	Str.join_with(parts, to)
}

expect str_replace_all("a.b.c", ".", "_") == "a_b_c"
expect str_replace_all("hello!", "!", "") == "hello"
expect str_replace_all("no match", "x", "y") == "no match"

## Convert first character to uppercase
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

## Convert first character to lowercase
lowercase_first : Str -> Str
lowercase_first = |s| {
	bytes = Str.to_utf8(s)
	if List.is_empty(bytes) {
		return ""
	}

	first = match List.first(bytes) {
		Ok(b) => b
		Err(_) => 0
	}
	first_is_upper = first >= 'A' and first <= 'Z'
	new_first = if first_is_upper to_lowercase(first) else first
	rest = List.drop_first(bytes, 1)
	new_bytes = List.concat([new_first], rest)
	match Str.from_utf8(new_bytes) {
		Ok(str) => str
		Err(_) => s
	}
}

expect lowercase_first("Hello") == "hello"
expect lowercase_first("hello") == "hello"
expect lowercase_first("") == ""
expect lowercase_first("A") == "a"

to_uppercase : U8 -> U8
to_uppercase = |ch| ch - 32

to_lowercase : U8 -> U8
to_lowercase = |ch| ch + 32

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

# =============================================================================
# Zig-specific helpers
# =============================================================================

## Map a Roc type to its Zig equivalent
roc_type_to_zig : Str -> Str
roc_type_to_zig = |roc_type| {
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
		"U8" => "u8"
		"U16" => "u16"
		"U32" => "u32"
		"U64" => "u64"
		"U128" => "u128"
		"I8" => "i8"
		"I16" => "i16"
		"I32" => "i32"
		"I64" => "i64"
		"I128" => "i128"
		"F32" => "f32"
		"F64" => "f64"
		_ => "?*anyopaque"
	}
}

expect roc_type_to_zig("Str") == "RocStr"
expect roc_type_to_zig("Bool") == "bool"
expect roc_type_to_zig("U8") == "u8"
expect roc_type_to_zig("U16") == "u16"
expect roc_type_to_zig("U32") == "u32"
expect roc_type_to_zig("U64") == "u64"
expect roc_type_to_zig("U128") == "u128"
expect roc_type_to_zig("I8") == "i8"
expect roc_type_to_zig("I16") == "i16"
expect roc_type_to_zig("I32") == "i32"
expect roc_type_to_zig("I64") == "i64"
expect roc_type_to_zig("I128") == "i128"
expect roc_type_to_zig("F32") == "f32"
expect roc_type_to_zig("F64") == "f64"
expect roc_type_to_zig("{}") == "void"
expect roc_type_to_zig("()") == "void"
expect roc_type_to_zig("List(U8)") == "RocList"
expect roc_type_to_zig("{ foo : Str }") == "?*anyopaque"

## Generate a Zig doc comment line (/// prefix)
zig_doc_comment : List(Str) -> Str
zig_doc_comment = |lines| {
	var $result = ""
	for line in lines {
		if line == "" {
			$result = Str.concat($result, "///\n")
		} else {
			$result = Str.concat($result, "/// ${line}\n")
		}
	}

	$result
}

expect zig_doc_comment(["Hello"]) == "/// Hello\n"
expect zig_doc_comment(["Line 1", "", "Line 2"]) == "/// Line 1\n///\n/// Line 2\n"

## Convert function name to PascalCase struct name
## "Stdout.line!" -> "StdoutLine"
## Splits on both "." and "_", capitalizing each segment
name_to_pascal_case : Str -> Str
name_to_pascal_case = |name| {
	cleaned = name
		->str_replace_all("!", "")

	# Split on dots first
	dot_parts = Str.split_on(cleaned, ".")

	var $result = ""
	for dot_part in dot_parts {
		# Then split each dot-segment on underscores
		underscore_parts = Str.split_on(dot_part, "_")
		for part in underscore_parts {
			$result = Str.concat($result, capitalize_first(part))
		}
	}

	$result
}

expect name_to_pascal_case("Stdout.line!") == "StdoutLine"
expect name_to_pascal_case("line!") == "Line"
expect name_to_pascal_case("Foo.bar_baz!") == "FooBarBaz"
expect name_to_pascal_case("Builder.print_value!") == "BuilderPrintValue"
expect name_to_pascal_case("Host.get_greeting!") == "HostGetGreeting"

## Convert function name to camelCase for convenience methods
## "Stdout.line!" -> "stdoutLine"
name_to_camel_case : Str -> Str
name_to_camel_case = |name| {
	pascal = name_to_pascal_case(name)
	lowercase_first(pascal)
}

expect name_to_camel_case("Stdout.line!") == "stdoutLine"
expect name_to_camel_case("line!") == "line"
expect name_to_camel_case("Builder.print_value!") == "builderPrintValue"
expect name_to_camel_case("Host.get_greeting!") == "hostGetGreeting"
expect name_to_camel_case("Stdin.line!") == "stdinLine"

## Convert function name to lower_snake_case for Zig field names
## "Stdout.line!" -> "stdout_line"
name_to_zig_field_name : Str -> Str
name_to_zig_field_name = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_lower_snake_case()
}

expect name_to_zig_field_name("Stdout.line!") == "stdout_line"
expect name_to_zig_field_name("line!") == "line"
expect name_to_zig_field_name("Foo.barBaz!") == "foo_bar_baz"
expect name_to_zig_field_name("Builder.print_value!") == "builder_print_value"

## Convert function name to UPPER_SNAKE_CASE for index constants
name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_screaming_snake_case()
}

## Convert a string to SCREAMING_SNAKE_CASE
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
		Err(_) => s
	}
}

expect name_to_upper_ident("Stdout.line!") == "STDOUT_LINE"
expect name_to_upper_ident("line!") == "LINE"
expect name_to_upper_ident("Foo.barBaz!") == "FOO_BAR_BAZ"

# =============================================================================
# Code generation
# =============================================================================

## Generate the complete Zig file
generate_zig_file : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_zig_file = |hosted_functions| {
	index_constants = generate_index_constants(hosted_functions)
	args_structs = generate_all_args_structs(hosted_functions)
	platform_struct = generate_platform_struct(hosted_functions)

	file_header
		.concat(core_types_section)
		.concat(ops_and_fn_type_section)
		.concat(index_constants)
		.concat("\n")
		.concat(args_structs)
		.concat(platform_struct)
}

## File header with module doc comments
file_header : Str
file_header =
	"//! Roc Platform ABI for Zig\n//!\n//! This file defines the Zig interface for hosted functions in a Roc platform.\n//! It is automatically generated by the Roc glue generator.\n\n"

## Core Roc types section
core_types_section : Str
core_types_section = {
	roc_str = Str.concat(
		zig_doc_comment(
			[
				"RocStr - Roc string type",
				"",
				"A 24-byte structure representing a string. Small strings (up to 23 bytes",
				"on 64-bit systems) are stored inline. Larger strings store a pointer to",
				"heap-allocated data.",
			],
		),
		"pub const RocStr = extern struct {\n    bytes: ?[*]u8,\n    len: usize,\n    capacity: usize,\n\n    comptime {\n        if (@sizeOf(RocStr) != 24) @compileError(\"RocStr must be 24 bytes\");\n        if (@alignOf(RocStr) != 8) @compileError(\"RocStr must be 8-byte aligned\");\n    }\n};\n\n",
	)

	roc_list = Str.concat(
		zig_doc_comment(
			[
				"RocList - Roc list type",
				"",
				"A 24-byte structure representing a list. Similar to RocStr, but for",
				"arbitrary element types.",
			],
		),
		"pub const RocList = extern struct {\n    elements: ?*anyopaque,\n    len: usize,\n    capacity: usize,\n\n    comptime {\n        if (@sizeOf(RocList) != 24) @compileError(\"RocList must be 24 bytes\");\n        if (@alignOf(RocList) != 8) @compileError(\"RocList must be 8-byte aligned\");\n    }\n};\n\n",
	)

	Str.concat(roc_str, roc_list)
}

## RocOps and HostedFn type section
ops_and_fn_type_section : Str
ops_and_fn_type_section = {
	ops = Str.concat(
		zig_doc_comment(
			[
				"RocOps - opaque type for Roc runtime operations",
				"Contains function pointers for allocation, deallocation, etc.",
			],
		),
		"pub const RocOps = opaque {};\n\n",
	)

	fn_type = Str.concat(
		zig_doc_comment(
			[
				"HostedFn - Function pointer type for hosted functions",
				"",
				"All hosted functions follow this signature:",
				"  - ops: pointer to Roc runtime operations",
				"  - args: pointer to function-specific arguments struct (or null if no args)",
				"  - ret: pointer to return value storage (or null if void return)",
			],
		),
		"pub const HostedFn = *const fn (ops: *RocOps, args: ?*anyopaque, ret: ?*anyopaque) void;\n\n",
	)

	Str.concat(ops, fn_type)
}

## Generate index constants for each hosted function
generate_index_constants : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_index_constants = |hosted_functions| {
	var $result = ""
	for func in hosted_functions {
		field_name = name_to_zig_field_name(func.name)
		$result = Str.concat($result, "pub const hosted_idx_${field_name}: usize = ${U64.to_str(func.index)};\n")
	}

	$result
}

## Generate args struct for a function if it has arguments
generate_args_struct : { index : U64, name : Str, type_str : Str } -> Str
generate_args_struct = |func| {
	parsed = parse_type_str(func.type_str)

	if List.is_empty(parsed.args) {
		return ""
	}

	struct_name = name_to_pascal_case(func.name)

	var $fields = ""
	var $idx = 0
	for arg in parsed.args {
		zig_type = roc_type_to_zig(arg)
		$fields = Str.concat($fields, "    arg${U64.to_str($idx)}: ${zig_type},\n")
		$idx = $idx + 1
	}

	doc = zig_doc_comment(["Arguments for ${func.name}"])
	"${doc}pub const ${struct_name}Args = extern struct {\n${$fields}};\n\n"
}

## Generate all args structs
generate_all_args_structs : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_all_args_structs = |hosted_functions| {
	var $result = ""
	for f in hosted_functions {
		$result = Str.concat($result, generate_args_struct(f))
	}
	$result
}

## Generate a typed convenience method for a hosted function
generate_convenience_method : { index : U64, name : Str, type_str : Str } -> Str
generate_convenience_method = |func| {
	parsed = parse_type_str(func.type_str)
	camel_name = name_to_camel_case(func.name)
	field_name = name_to_zig_field_name(func.name)
	struct_name = name_to_pascal_case(func.name)
	ret_zig_type = roc_type_to_zig(parsed.ret)
	has_args = !(List.is_empty(parsed.args))
	has_return = ret_zig_type != "void"

	# Build parameter list
	var $params = "self: Platform, ops: *RocOps"
	if has_args {
		var $idx = 0
		for arg in parsed.args {
			zig_type = roc_type_to_zig(arg)
			$params = Str.concat($params, ", arg${U64.to_str($idx)}: ${zig_type}")
			$idx = $idx + 1
		}
	}

	# Return type
	ret_type = if has_return ret_zig_type else "void"

	# Function body
	if has_args and has_return {
		# Has args, has return
		var $arg_fields = ""
		var $idx2 = 0
		for _arg in parsed.args {
			$arg_fields = Str.concat($arg_fields, " .arg${U64.to_str($idx2)} = arg${U64.to_str($idx2)},")
			$idx2 = $idx2 + 1
		}
		"    pub fn ${camel_name}(${$params}) ${ret_type} {\n        var args = ${struct_name}Args{${$arg_fields} };\n        var ret: ${ret_type} = undefined;\n        self.${field_name}(ops, @ptrCast(&args), @ptrCast(&ret));\n        return ret;\n    }\n"
	} else if has_args {
		# Has args, void return
		var $arg_fields2 = ""
		var $idx3 = 0
		for _arg in parsed.args {
			$arg_fields2 = Str.concat($arg_fields2, " .arg${U64.to_str($idx3)} = arg${U64.to_str($idx3)},")
			$idx3 = $idx3 + 1
		}
		"    pub fn ${camel_name}(${$params}) void {\n        var args = ${struct_name}Args{${$arg_fields2} };\n        self.${field_name}(ops, @ptrCast(&args), null);\n    }\n"
	} else if has_return {
		# No args, has return
		"    pub fn ${camel_name}(${$params}) ${ret_type} {\n        var ret: ${ret_type} = undefined;\n        self.${field_name}(ops, null, @ptrCast(&ret));\n        return ret;\n    }\n"
	} else {
		# No args, void return
		"    pub fn ${camel_name}(${$params}) void {\n        self.${field_name}(ops, null, null);\n    }\n"
	}
}

## Generate the Platform struct with HostedFn fields and convenience methods
generate_platform_struct : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_platform_struct = |hosted_functions| {
	doc = zig_doc_comment(
		[
			"Platform - typed interface to Roc hosted functions",
			"",
			"Contains function pointers for each hosted function, plus typed",
			"convenience methods that handle argument packing and return value unpacking.",
		],
	)

	# Generate fields
	var $fields = ""
	for func in hosted_functions {
		field_name = name_to_zig_field_name(func.name)
		$fields = Str.concat($fields, "    ${field_name}: HostedFn,\n")
	}

	# Generate convenience methods
	var $methods = ""
	var $first = Bool.True
	for f in hosted_functions {
		if !$first {
			$methods = Str.concat($methods, "\n")
		}
		$methods = Str.concat($methods, generate_convenience_method(f))
		$first = Bool.False
	}

	"${doc}pub const Platform = struct {\n${$fields}\n${$methods}};\n"
}

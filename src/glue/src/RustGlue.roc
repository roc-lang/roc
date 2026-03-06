## A glue script for generating a Rust source file with hosted function bindings.
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]
import pf.RecordFieldInfo exposing [RecordFieldInfo]
import pf.TypeRepr exposing [TypeRepr]
import pf.FunctionRepr exposing [FunctionRepr]
import pf.RecordRepr exposing [RecordRepr]
import pf.TagUnionRepr exposing [TagUnionRepr]
import pf.RecordField exposing [RecordField]
import pf.TagVariant exposing [TagVariant]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	# Collect all hosted functions from all modules, with module name prefix
	var $hosted_functions = []
	var $type_table = []

	for types in types_list {
		$type_table = types.type_table

		for mod in types.modules {
			for func in mod.hosted_functions {
				full_qualified_name = "${mod.name}.${func.name}"

				hosted_func = {
					arg_fields: func.arg_fields,
					arg_type_ids: func.arg_type_ids,
					index: func.index,
					name: full_qualified_name,
					ret_fields: func.ret_fields,
					ret_type_id: func.ret_type_id,
					type_str: func.type_str,
				}

				$hosted_functions = $hosted_functions.append(hosted_func)
			}
		}
	}

	# Sort by index so array entries are in the correct order
	sorted = List.sort_with($hosted_functions, compare_by_index)

	rust_content = generate_rust_file(sorted, $type_table)

	Ok([{ name: "roc_platform_abi.rs", content: rust_content }])
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
# Type String Parsing
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

## Parse args portion of type string
parse_args : Str -> List(Str)
parse_args = |args_part| {
	trimmed = Str.trim(args_part)
	if trimmed == "()" or trimmed == "" or trimmed == "({})" or trimmed == "{}" {
		return []
	}

	is_parenthesized = Str.starts_with(trimmed, "(") and Str.ends_with(trimmed, ")")
	stripped = if is_parenthesized drop_first_last(trimmed) else trimmed

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

## Drop first and last character from string
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

# =============================================================================
# Roc Type to Rust Type Mapping
# =============================================================================

## Map a Roc type to its Rust equivalent
roc_type_to_rust : Str -> Str
roc_type_to_rust = |roc_type| {
	trimmed = Str.trim(roc_type)

	if trimmed == "{}" or trimmed == "()" or trimmed == "{  }" {
		return "()"
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
		_ => "*mut c_void"
	}
}

expect roc_type_to_rust("Str") == "RocStr"
expect roc_type_to_rust("Bool") == "bool"
expect roc_type_to_rust("U8") == "u8"
expect roc_type_to_rust("U64") == "u64"
expect roc_type_to_rust("F64") == "f64"
expect roc_type_to_rust("{}") == "()"
expect roc_type_to_rust("()") == "()"
expect roc_type_to_rust("List(U8)") == "RocList"
expect roc_type_to_rust("{ foo : Str }") == "*mut c_void"

# =============================================================================
# TypeRepr-based Type Mapping
# =============================================================================

## Map a type table entry to its Rust type string using structured TypeRepr
type_id_to_rust : List(TypeRepr), U64 -> Str
type_id_to_rust = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => type_repr_to_rust(type_table, type_repr)
		Err(_) => "*mut c_void"
	}
}

## Convert a TypeRepr to its Rust type string
type_repr_to_rust : List(TypeRepr), TypeRepr -> Str
type_repr_to_rust = |type_table, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) => "*mut ${type_id_to_rust(type_table, inner_id)}"
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
		RocDec => "f64"
		RocList(elem_id) =>
			if is_type_refcounted(type_table, elem_id) {
				"RocList<${type_id_to_rust(type_table, elem_id)}>"
			} else {
				"RocListWith<${type_id_to_rust(type_table, elem_id)}, false>"
			}
		RocRecord(rec) =>
			if rec.name == "" {
				"*mut c_void"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type_rust(type_table, tu)
		RocFunction(_) => "*mut c_void"
		RocUnknown(_) => "*mut c_void"
	}
}

## Resolve a tag union to a Rust type. Single-variant unions are unwrapped to their payload.
## Multi-variant unions with a name return a generated struct name.
resolve_tag_union_type_rust = |type_table, tu| {
	if List.len(tu.tags) == 1 {
		match List.first(tu.tags) {
			Ok(tag) =>
				match List.first(tag.payload) {
					Ok(payload_id) => type_id_to_rust(type_table, payload_id)
					_ => "*mut c_void"
				}
			_ => "*mut c_void"
		}
	} else if tu.name != "" {
		name_to_struct_name(tu.name)
	} else {
		"*mut c_void"
	}
}

## Determine whether a type is refcounted (heap-allocated).
## Refcounted types need 2*ptr_width header space in list allocations.
is_type_refcounted : List(TypeRepr), U64 -> Bool
is_type_refcounted = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => is_repr_refcounted(type_table, type_repr)
		Err(_) => Bool.False
	}
}

is_repr_refcounted : List(TypeRepr), TypeRepr -> Bool
is_repr_refcounted = |type_table, type_repr| {
	match type_repr {
		RocStr => Bool.True
		RocList(_) => Bool.True
		RocFunction(_) => Bool.True
		RocRecord(rec) => List.any(rec.fields, |field| is_type_refcounted(type_table, field.type_id))
		RocTagUnion(tu) => List.any(tu.tags, |tag| List.any(tag.payload, |pid| is_type_refcounted(type_table, pid)))
		_ => Bool.False
	}
}

# =============================================================================
# String Utilities
# =============================================================================

## Replace all occurrences of a substring
str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| {
	parts = Str.split_on(s, from)
	Str.join_with(parts, to)
}

expect str_replace_all("a.b.c", ".", "_") == "a_b_c"
expect str_replace_all("hello!", "!", "") == "hello"

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

## Convert a string to SCREAMING_SNAKE_CASE (for Rust constants)
to_screaming_snake_case : Str -> Str
to_screaming_snake_case = |s| {
	bytes = Str.to_utf8(s)
	var $output = []
	var $prev_was_lower = Bool.False

	for byte in bytes {
		is_upper = byte >= 'A' and byte <= 'Z'
		is_lower = byte >= 'a' and byte <= 'z'

		new_byte = if is_lower to_uppercase(byte) else byte

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

expect to_screaming_snake_case("FooBar") == "FOO_BAR"
expect to_screaming_snake_case("fooBar") == "FOO_BAR"
expect to_screaming_snake_case("foo") == "FOO"
expect to_screaming_snake_case("FOO") == "FOO"
expect to_screaming_snake_case("Stdout_line") == "STDOUT_LINE"

## Capitalize the first character of a string
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

## Lowercase the first character of a string
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

# =============================================================================
# Name Conversion
# =============================================================================

## Convert function name to PascalCase struct name (e.g., "Stdout.line!" -> "StdoutLine")
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

## Convert function name to snake_case (e.g., "Stdout.line!" -> "stdout_line")
name_to_snake : Str -> Str
name_to_snake = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_lower_snake_case()
}

expect name_to_snake("Stdout.line!") == "stdout_line"
expect name_to_snake("line!") == "line"
expect name_to_snake("Foo.barBaz!") == "foo_bar_baz"
expect name_to_snake("PartDef.Idx.get!") == "part_def_idx_get"

## Convert function name to SCREAMING_SNAKE_CASE (e.g., "Stdout.line!" -> "STDOUT_LINE")
name_to_screaming_snake : Str -> Str
name_to_screaming_snake = |name| {
	name
		->str_replace_all(".", "_")
		->str_replace_all("!", "")
		->to_screaming_snake_case()
}

expect name_to_screaming_snake("Stdout.line!") == "STDOUT_LINE"
expect name_to_screaming_snake("line!") == "LINE"
expect name_to_screaming_snake("Foo.barBaz!") == "FOO_BAR_BAZ"
expect name_to_screaming_snake("PartDef.Idx.get!") == "PART_DEF_IDX_GET"

## Return the Rust discriminant type for a given tag count.
disc_type_for_count = |count| {
	if count <= 256 {
		"u8"
	} else if count <= 65536 {
		"u16"
	} else {
		"u32"
	}
}

# =============================================================================
# Type Table Helpers
# =============================================================================

## Look up a type_id in the type table and return record fields if it's a record.
## Follows single-variant tag unions (unwrapping to their payload).
lookup_record_in_type_table = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) =>
			match type_repr {
				RocRecord(rec) =>
					if List.len(rec.fields) > 0 {
						{ found: Bool.True, fields: rec.fields, size: rec.size, alignment: rec.alignment }
					} else {
						{ found: Bool.False, fields: [], size: 0, alignment: 0 }
					}
				RocTagUnion(tu) =>
					# Follow single-variant tag unions to their payload
					if List.len(tu.tags) == 1 {
						match List.first(tu.tags) {
							Ok(tag) =>
								match List.first(tag.payload) {
									Ok(payload_id) => lookup_record_in_type_table(type_table, payload_id)
									_ => { found: Bool.False, fields: [], size: 0, alignment: 0 }
								}
							_ => { found: Bool.False, fields: [], size: 0, alignment: 0 }
						}
					} else {
						{ found: Bool.False, fields: [], size: 0, alignment: 0 }
					}
				_ => { found: Bool.False, fields: [], size: 0, alignment: 0 }
			}
		Err(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
	}
}

# =============================================================================
# Rust Code Generation
# =============================================================================

## Generate the complete Rust source file
generate_rust_file = |hosted_functions, type_table| {
	count = List.len(hosted_functions)

	generate_rust_file_header
		.concat(generate_rust_imports)
		.concat("\n")
		.concat(generate_host_abi_types_rust)
		.concat("\n")
		.concat(generate_rust_roc_str)
		.concat("\n")
		.concat(generate_rust_roc_list)
		.concat("\n")
		.concat(generate_index_constants_rust(hosted_functions, count))
		.concat("\n")
		.concat(generate_element_type_structs_rust(type_table))
		.concat(generate_tag_union_structs_rust(type_table))
		.concat(generate_all_record_structs_rust(hosted_functions, type_table))
		.concat(generate_all_args_structs_rust(hosted_functions, type_table))
		.concat(generate_platform_fns_struct_rust(hosted_functions, type_table))
		.concat("\n")
		.concat(generate_hosted_functions_helper_rust(hosted_functions))
		.concat("\n")
		.concat(generate_default_allocators_rust)
		.concat("\n")
		.concat(generate_default_handlers_rust)
		.concat("\n")
		.concat(generate_make_roc_ops_rust)
}

## File header comment
generate_rust_file_header : Str
generate_rust_file_header =
	\\//! Roc Platform ABI
	\\//!
	\\//! This file defines the Rust interface for hosted functions in a Roc platform.
	\\//! It is automatically generated by the Roc glue generator.
	\\//!
	\\//! Usage:
	\\//! 1. Import this module in your platform host
	\\//! 2. Implement each hosted function according to its signature
	\\//! 3. Call `hosted_functions()` to create the dispatch table for RocOps
	\\
	\\#![allow(non_camel_case_types, dead_code, unused_imports, clippy::missing_safety_doc)]
	\\
	\\

## Import section
generate_rust_imports : Str
generate_rust_imports =
	\\use core::ffi::c_void;
	\\use std::alloc::Layout;
	\\

## Generate self-contained host ABI type definitions (matching roc_ops.rs)
generate_host_abi_types_rust : Str
generate_host_abi_types_rust =
	\\/// Type-erased function pointer for hosted function dispatch.
	\\pub type HostedFn = extern "C" fn(*const RocOps, *mut c_void, *mut c_void);
	\\
	\\/// Table of hosted function pointers passed to the Roc runtime.
	\\#[repr(C)]
	\\#[derive(Debug, Clone, Copy)]
	\\pub struct HostedFunctions {
	\\    pub count: u32,
	\\    pub fns: *const HostedFn,
	\\}
	\\
	\\/// Arguments for a Roc allocation request.
	\\#[repr(C)]
	\\#[derive(Debug)]
	\\pub struct RocAlloc {
	\\    pub alignment: usize,
	\\    pub length: usize,
	\\    pub answer: *mut c_void,
	\\}
	\\
	\\/// Arguments for a Roc deallocation request.
	\\#[repr(C)]
	\\#[derive(Debug)]
	\\pub struct RocDealloc {
	\\    pub alignment: usize,
	\\    pub ptr: *mut c_void,
	\\}
	\\
	\\/// Arguments for a Roc reallocation request.
	\\#[repr(C)]
	\\#[derive(Debug)]
	\\pub struct RocRealloc {
	\\    pub alignment: usize,
	\\    pub new_length: usize,
	\\    pub answer: *mut c_void,
	\\}
	\\
	\\/// Arguments for a Roc `dbg` expression.
	\\#[repr(C)]
	\\#[derive(Debug)]
	\\pub struct RocDbg {
	\\    pub utf8_bytes: *mut u8,
	\\    pub len: usize,
	\\}
	\\
	\\/// Arguments for a failed Roc `expect`.
	\\#[repr(C)]
	\\#[derive(Debug)]
	\\pub struct RocExpectFailed {
	\\    pub utf8_bytes: *mut u8,
	\\    pub len: usize,
	\\}
	\\
	\\/// Arguments for a Roc `crash`.
	\\#[repr(C)]
	\\#[derive(Debug)]
	\\pub struct RocCrashed {
	\\    pub utf8_bytes: *mut u8,
	\\    pub len: usize,
	\\}
	\\
	\\/// The operations table passed from the host to the Roc runtime.
	\\#[repr(C)]
	\\pub struct RocOps {
	\\    pub env: *mut c_void,
	\\    pub roc_alloc: extern "C" fn(*mut RocAlloc, *mut c_void),
	\\    pub roc_dealloc: extern "C" fn(*mut RocDealloc, *mut c_void),
	\\    pub roc_realloc: extern "C" fn(*mut RocRealloc, *mut c_void),
	\\    pub roc_dbg: extern "C" fn(*const RocDbg, *mut c_void),
	\\    pub roc_expect_failed: extern "C" fn(*const RocExpectFailed, *mut c_void),
	\\    pub roc_crashed: extern "C" fn(*const RocCrashed, *mut c_void),
	\\    pub hosted_fns: HostedFunctions,
	\\}
	\\
	\\impl RocOps {
	\\    /// Allocate memory with the given alignment and length.
	\\    #[inline]
	\\    pub unsafe fn alloc(&self, alignment: usize, length: usize) -> *mut c_void {
	\\        let mut args = RocAlloc {
	\\            alignment,
	\\            length,
	\\            answer: core::ptr::null_mut(),
	\\        };
	\\        (self.roc_alloc)(&mut args, self.env);
	\\        args.answer
	\\    }
	\\
	\\    /// Deallocate memory previously allocated with `alloc`.
	\\    #[inline]
	\\    pub unsafe fn dealloc(&self, ptr: *mut c_void, alignment: usize) {
	\\        let mut args = RocDealloc { alignment, ptr };
	\\        (self.roc_dealloc)(&mut args, self.env);
	\\    }
	\\
	\\    /// Reallocate memory to a new size.
	\\    #[inline]
	\\    pub unsafe fn realloc(
	\\        &self,
	\\        old_ptr: *mut c_void,
	\\        alignment: usize,
	\\        new_length: usize,
	\\    ) -> *mut c_void {
	\\        let mut args = RocRealloc {
	\\            alignment,
	\\            new_length,
	\\            answer: old_ptr,
	\\        };
	\\        (self.roc_realloc)(&mut args, self.env);
	\\        args.answer
	\\    }
	\\}
	\\

## Generate self-contained RocStr type (simplified, raw pointer approach)
generate_rust_roc_str : Str
generate_rust_roc_str =
	\\/// A Roc string value. Small strings (up to 23 bytes on 64-bit) are stored inline;
	\\/// larger strings are heap-allocated with a reference count.
	\\///
	\\/// This type is ABI-compatible with the Zig RocStr (24 bytes, `#[repr(C)]`).
	\\#[repr(C)]
	\\pub struct RocStr {
	\\    pub bytes: *mut u8,
	\\    pub length: usize,
	\\    pub capacity_or_alloc_ptr: usize,
	\\}
	\\
	\\const ROC_STR_SIZE: usize = core::mem::size_of::<RocStr>();
	\\const ROC_SMALL_STR_MAX_LEN: usize = ROC_STR_SIZE - 1;
	\\const ROC_SEAMLESS_SLICE_BIT: usize = isize::MIN as usize;
	\\
	\\impl RocStr {
	\\    /// Return an empty RocStr (small string with zero length).
	\\    pub fn empty() -> Self {
	\\        Self {
	\\            bytes: core::ptr::null_mut(),
	\\            length: 0,
	\\            capacity_or_alloc_ptr: ROC_SEAMLESS_SLICE_BIT,
	\\        }
	\\    }
	\\
	\\    /// Return true if this string is stored inline (small string optimization).
	\\    #[inline]
	\\    pub fn is_small_str(&self) -> bool {
	\\        (self.capacity_or_alloc_ptr as isize) < 0
	\\    }
	\\
	\\    /// Return true if this string is a seamless slice into another allocation.
	\\    #[inline]
	\\    pub fn is_seamless_slice(&self) -> bool {
	\\        !self.is_small_str() && (self.length as isize) < 0
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
	\\            self.length & !ROC_SEAMLESS_SLICE_BIT
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
	\\    /// Return the string contents as a `&str`, assuming valid UTF-8.
	\\    pub fn as_str(&self) -> &str {
	\\        // SAFETY: Roc guarantees all strings are valid UTF-8.
	\\        unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
	\\    }
	\\
	\\    /// Create a RocStr from a byte slice, using `roc_ops` for heap allocation if needed.
	\\    pub fn from_slice(slice: &[u8], roc_ops: &RocOps) -> Self {
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
	\\            let total = ptr_width + slice.len();
	\\            let base = unsafe { roc_ops.alloc(core::mem::align_of::<usize>(), total) };
	\\            let data_ptr = unsafe { (base as *mut u8).add(ptr_width) };
	\\            // Write refcount = 1
	\\            unsafe {
	\\                let rc = (data_ptr as *mut isize).sub(1);
	\\                *rc = 1;
	\\                core::ptr::copy_nonoverlapping(slice.as_ptr(), data_ptr, slice.len());
	\\            }
	\\            Self {
	\\                bytes: data_ptr,
	\\                length: slice.len(),
	\\                capacity_or_alloc_ptr: slice.len(),
	\\            }
	\\        }
	\\    }
	\\
	\\    /// Create a RocStr from a `&str`.
	\\    pub fn from_str(s: &str, roc_ops: &RocOps) -> Self {
	\\        Self::from_slice(s.as_bytes(), roc_ops)
	\\    }
	\\
	\\    /// Decrement the reference count; frees the allocation when it reaches zero.
	\\    pub fn decref(&self, roc_ops: &RocOps) {
	\\        if self.is_small_str() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *mut isize).sub(1);
	\\            let prev = *rc;
	\\            *rc = prev - 1;
	\\            if prev == 1 {
	\\                let ptr_width = core::mem::size_of::<usize>();
	\\                let base = (alloc_ptr as *mut u8).sub(ptr_width) as *mut c_void;
	\\                roc_ops.dealloc(base, core::mem::align_of::<usize>());
	\\            }
	\\        }
	\\    }
	\\
	\\    fn get_allocation_ptr(&self) -> *mut u8 {
	\\        if self.is_seamless_slice() {
	\\            (self.capacity_or_alloc_ptr << 1) as *mut u8
	\\        } else {
	\\            self.bytes
	\\        }
	\\    }
	\\}
	\\
	\\impl core::fmt::Debug for RocStr {
	\\    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
	\\        f.debug_struct("RocStr")
	\\            .field("value", &self.as_str())
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
	\\pub struct RocListWith<T, const ELEMENTS_REFCOUNTED: bool> {
	\\    pub elements: *mut T,
	\\    pub length: usize,
	\\    pub capacity_or_alloc_ptr: usize,
	\\}
	\\
	\\impl<T, const ELEMENTS_REFCOUNTED: bool> RocListWith<T, ELEMENTS_REFCOUNTED> {
	\\    #[inline]
	\\    fn header_bytes() -> usize {
	\\        let ptr_width = core::mem::size_of::<usize>();
	\\        let required_space = if ELEMENTS_REFCOUNTED { 2 * ptr_width } else { ptr_width };
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
	\\    /// Return the list elements as a slice.
	\\    pub fn as_slice(&self) -> &[T] {
	\\        if self.elements.is_null() {
	\\            &[]
	\\        } else {
	\\            unsafe { core::slice::from_raw_parts(self.elements, self.length) }
	\\        }
	\\    }
	\\
	\\    /// Allocate a new list with space for `length` elements.
	\\    pub fn allocate(length: usize, roc_ops: &RocOps) -> Self {
	\\        if length == 0 {
	\\            return Self::empty();
	\\        }
	\\        let align = core::mem::align_of::<T>().max(core::mem::align_of::<usize>());
	\\        let header_bytes = Self::header_bytes();
	\\        let data_bytes = length * core::mem::size_of::<T>();
	\\        let total = data_bytes + header_bytes;
	\\        let base = unsafe { roc_ops.alloc(align, total) };
	\\        let data_ptr = unsafe { (base as *mut u8).add(header_bytes) };
	\\        // Write refcount = 1
	\\        unsafe {
	\\            let rc = (data_ptr as *mut isize).sub(1);
	\\            *rc = 1;
	\\        }
	\\        Self {
	\\            elements: data_ptr as *mut T,
	\\            length,
	\\            capacity_or_alloc_ptr: length,
	\\        }
	\\    }
	\\
	\\    /// Create a RocList from a slice, copying elements into a new allocation.
	\\    pub fn from_slice(slice: &[T], roc_ops: &RocOps) -> Self where T: Copy {
	\\        if slice.is_empty() {
	\\            return Self::empty();
	\\        }
	\\        let list = Self::allocate(slice.len(), roc_ops);
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
	\\    pub fn decref(&self, roc_ops: &RocOps) {
	\\        if self.elements.is_null() {
	\\            return;
	\\        }
	\\        let align = core::mem::align_of::<T>().max(core::mem::align_of::<usize>());
	\\        let header_bytes = Self::header_bytes();
	\\        unsafe {
	\\            let rc = (self.elements as *mut isize).sub(1);
	\\            let prev = *rc;
	\\            *rc = prev - 1;
	\\            if prev == 1 {
	\\                let base = (self.elements as *mut u8).sub(header_bytes) as *mut c_void;
	\\                roc_ops.dealloc(base, align);
	\\            }
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
generate_element_type_structs_rust : List(TypeRepr) -> Str
generate_element_type_structs_rust = |type_table| {
	var $structs = ""
	var $seen_names = []

	for type_repr in type_table {
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" and !(List.contains($seen_names, rec.name)) {
					$seen_names = $seen_names.append(rec.name)

					struct_name = name_to_struct_name(rec.name)
					var $field_strs = ""
					for field in rec.fields {
						rust_type = type_id_to_rust(type_table, field.type_id)
						$field_strs = Str.concat(
							$field_strs,
							"    pub ${field.name}: ${rust_type},\n",
						)
					}

					# Size/alignment assertions (guarded by pointer width)
					assertions = if rec.size > 0 {
						"const _: () = assert!(core::mem::size_of::<${struct_name}>() == ${U64.to_str(rec.size)}, \"${struct_name} size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}>() == ${U64.to_str(rec.alignment)}, \"${struct_name} alignment mismatch\");\n\n"
					} else {
						""
					}

					$structs = Str.concat(
						$structs,
						"/// Element type for ${rec.name}\n#[repr(C)]\npub struct ${struct_name} {\n${$field_strs}}\n\n${assertions}",
					)
				}
			_ => {}
		}
	}

	$structs
}

## Generate #[repr(C)] structs for tag union types found in the type table.
generate_tag_union_structs_rust : List(TypeRepr) -> Str
generate_tag_union_structs_rust = |type_table| {
	var $structs = ""
	var $seen_names = []

	for type_repr in type_table {
		match type_repr {
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" and !(List.contains($seen_names, tu.name)) {
					$seen_names = $seen_names.append(tu.name)
					$structs = Str.concat($structs, generate_single_tag_union_rust(type_table, tu))
				}
			_ => {}
		}
	}

	$structs
}

## Generate Rust code for a single multi-variant tag union.
generate_single_tag_union_rust = |type_table, tu| {
	struct_name = name_to_struct_name(tu.name)
	tag_count = List.len(tu.tags)
	disc_type = disc_type_for_count(tag_count)

	# Check if this is a pure enum (all variants have no payload)
	is_pure_enum = List.all(tu.tags, |tag| List.is_empty(tag.payload))

	if is_pure_enum {
		# Pure enum: just emit the enum type
		var $variants = ""
		var $idx = 0
		for tag in tu.tags {
			$variants = Str.concat($variants, "    ${capitalize_first(tag.name)} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		"/// Tag union: ${tu.name}\n#[repr(${disc_type})]\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum ${struct_name} {\n${$variants}}\n\n"
	} else {
		# Generate tuple structs for any variant with >1 payload
		var $tuple_structs = ""
		for tag in tu.tags {
			if List.len(tag.payload) > 1 {
				tuple_name = "${struct_name}${capitalize_first(tag.name)}Payload"
				var $tuple_fields = ""
				var $ti = 0
				for pid in tag.payload {
					rust_type = type_id_to_rust(type_table, pid)
					$tuple_fields = Str.concat($tuple_fields, "    pub _${U64.to_str($ti)}: ${rust_type},\n")
					$ti = $ti + 1
				}
				$tuple_structs = Str.concat($tuple_structs, "/// Payload struct for ${tag.name} variant.\n#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${tuple_name} {\n${$tuple_fields}}\n\n")
			}
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
		for union_tag in tu.tags {
			snake = to_lower_snake_case(union_tag.name)
			if List.is_empty(union_tag.payload) {
				# No-payload variant: use [u8; 0]
				$union_fields = Str.concat($union_fields, "    pub ${snake}: [u8; 0],\n")
			} else if List.len(union_tag.payload) == 1 {
				rust_type = match List.first(union_tag.payload) {
					Ok(pid) => type_id_to_rust(type_table, pid)
					Err(_) => "*mut c_void"
				}
				# Wrap in ManuallyDrop since union fields must be Copy or ManuallyDrop
				$union_fields = Str.concat($union_fields, "    pub ${snake}: core::mem::ManuallyDrop<${rust_type}>,\n")
			} else {
				tuple_name = "${struct_name}${capitalize_first(union_tag.name)}Payload"
				$union_fields = Str.concat($union_fields, "    pub ${snake}: core::mem::ManuallyDrop<${tuple_name}>,\n")
			}
		}

		# Size/alignment assertions
		assertions = if tu.size > 0 {
			"const _: () = assert!(core::mem::size_of::<${struct_name}>() == ${U64.to_str(tu.size)}, \"${struct_name} size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}>() == ${U64.to_str(tu.alignment)}, \"${struct_name} alignment mismatch\");\n\n"
		} else {
			""
		}

		"${$tuple_structs}/// Tag discriminant for ${tu.name}.\n#[repr(${disc_type})]\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum ${struct_name}Tag {\n${$enum_variants}}\n\n/// Tag union: ${tu.name}\n#[repr(C)]\npub struct ${struct_name} {\n    pub payload: ${struct_name}Payload,\n    pub tag: ${struct_name}Tag,\n}\n\n#[repr(C)]\npub union ${struct_name}Payload {\n${$union_fields}}\n\n${assertions}"
	}
}

## Generate #[repr(C)] structs for record return types using type table.
generate_all_record_structs_rust = |hosted_functions, type_table| {
	var $structs = ""
	for func in hosted_functions {
		type_table_result = lookup_record_in_type_table(type_table, func.ret_type_id)

		if type_table_result.found {
			struct_name = name_to_struct_name(func.name)

			var $fields = ""
			for field in type_table_result.fields {
				rust_type = type_id_to_rust(type_table, field.type_id)
				$fields = Str.concat(
					$fields,
					"    pub ${field.name}: ${rust_type},\n",
				)
			}

			assertions = if type_table_result.size > 0 {
				"const _: () = assert!(core::mem::size_of::<${struct_name}RetRecord>() == ${U64.to_str(type_table_result.size)}, \"${struct_name}RetRecord size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}RetRecord>() == ${U64.to_str(type_table_result.alignment)}, \"${struct_name}RetRecord alignment mismatch\");\n\n"
			} else {
				""
			}

			doc = "/// Return type record for ${func.name}\n/// Fields ordered by alignment descending (Roc ABI)\n"
			$structs = Str.concat(
				$structs,
				"${doc}#[repr(C)]\npub struct ${struct_name}RetRecord {\n${$fields}}\n\n${assertions}",
			)
		}
	}
	$structs
}

## Generate all argument #[repr(C)] structs
generate_all_args_structs_rust = |hosted_functions, type_table| {
	var $structs = ""
	for func in hosted_functions {
		$structs = Str.concat($structs, generate_args_struct_rust(func, type_table))
	}
	$structs
}

## Generate a single argument struct (empty string if no args).
generate_args_struct_rust = |func, type_table| {
	parsed = parse_type_str(func.type_str)

	if List.is_empty(parsed.args) {
		return ""
	}

	struct_name = name_to_struct_name(func.name)

	# Try type table lookup for single-record arg
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
			rust_type = type_id_to_rust(type_table, field.type_id)
			$fields = Str.concat(
				$fields,
				"    pub ${field.name}: ${rust_type},\n",
			)
		}

		assertions = if type_table_result.size > 0 {
			"const _: () = assert!(core::mem::size_of::<${struct_name}Args>() == ${U64.to_str(type_table_result.size)}, \"${struct_name}Args size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}Args>() == ${U64.to_str(type_table_result.alignment)}, \"${struct_name}Args alignment mismatch\");\n\n"
		} else {
			""
		}

		doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n"
		return "${doc}#[repr(C)]\npub struct ${struct_name}Args {\n${$fields}}\n\n${assertions}"
	}

	# Multi-arg or primitive args: use positional fields from type table
	var $positional_fields = ""
	var $idx = 0
	for arg_type_id in func.arg_type_ids {
		rust_type = type_id_to_rust(type_table, arg_type_id)
		$positional_fields = Str.concat(
			$positional_fields,
			"    pub arg${U64.to_str($idx)}: ${rust_type},\n",
		)
		$idx = $idx + 1
	}

	doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n"

	"${doc}#[repr(C)]\npub struct ${struct_name}Args {\n${$positional_fields}}\n\n"
}

## Generate the PlatformHostedFns struct type
generate_platform_fns_struct_rust = |hosted_functions, type_table| {
	var $fields = ""

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		fn_type = hosted_fn_type_rust(func, type_table)

		$fields = Str.concat(
			$fields,
			"    pub ${snake}: ${fn_type}, // ${func.name}\n",
		)
	}

	"/// Implement this struct with your hosted function implementations.\n/// Pass it to `hosted_functions()` to create the dispatch table.\npub struct PlatformHostedFns {\n${$fields}}\n"
}

## Get the Rust function pointer type for a hosted function
hosted_fn_type_rust = |func, type_table| {
	parsed = parse_type_str(func.type_str)
	struct_name = name_to_struct_name(func.name)

	# Use type table to detect record return types
	ret_record = lookup_record_in_type_table(type_table, func.ret_type_id)
	ret_param = if ret_record.found {
		"*mut ${struct_name}RetRecord"
	} else {
		rust_ret = type_id_to_rust(type_table, func.ret_type_id)
		if rust_ret == "()" or rust_ret == "*mut c_void" {
			"*mut c_void"
		} else {
			"*mut ${rust_ret}"
		}
	}

	args_param = if List.is_empty(parsed.args) {
		"*mut c_void"
	} else {
		"*const ${struct_name}Args"
	}

	"extern \"C\" fn(*const RocOps, ${ret_param}, ${args_param})"
}

## Generate the hosted_functions() helper that builds the dispatch table
generate_hosted_functions_helper_rust = |hosted_functions| {
	var $entries = ""

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		$entries = Str.concat(
			$entries,
			"        unsafe { core::mem::transmute(fns.${snake} as *const ()) }, // ${func.name} (index ${U64.to_str(func.index)})\n",
		)
	}

	"/// Create a HostedFunctions dispatch table from your implementations.\n///\n/// The returned `HostedFunctions` contains a pointer to a leaked heap allocation\n/// that lives for the remainder of the program. This is intentional — the dispatch\n/// table is typically created once at startup.\n///\n/// # Safety\n/// The function pointers in `fns` must have the correct ABI signatures.\npub fn hosted_functions(fns: &PlatformHostedFns) -> HostedFunctions {\n    let ptrs: &'static [HostedFn] = Box::leak(Box::new([\n${$entries}    ]));\n\n    HostedFunctions {\n        count: ptrs.len() as u32,\n        fns: ptrs.as_ptr(),\n    }\n}\n"
}

## Generate the DefaultAllocators using std::alloc::Layout
generate_default_allocators_rust : Str
generate_default_allocators_rust =
	\\/// Default memory management functions for Roc platforms.
	\\///
	\\/// Memory layout: each allocation prepends size metadata so that dealloc/realloc
	\\/// can recover the original allocation size (required because `roc_dealloc` does
	\\/// not receive a length parameter).
	\\pub struct DefaultAllocators;
	\\
	\\impl DefaultAllocators {
	\\    /// Allocate memory for the Roc runtime using the global allocator.
	\\    pub extern "C" fn roc_alloc(alloc_args: *mut RocAlloc, _env: *mut c_void) {
	\\        unsafe {
	\\            let args = &mut *alloc_args;
	\\            let min_alignment = args.alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\            let total_size = args.length + size_storage_bytes;
	\\
	\\            debug_assert!(min_alignment.is_power_of_two(), "alignment must be a power of two");
	\\            let layout = Layout::from_size_align_unchecked(total_size, min_alignment);
	\\            let base_ptr = std::alloc::alloc(layout);
	\\            if base_ptr.is_null() {
	\\                eprintln!("roc_alloc: out of memory");
	\\                std::process::exit(1);
	\\            }
	\\
	\\            // Store total size immediately before the user data pointer
	\\            let size_ptr = base_ptr.add(size_storage_bytes).sub(core::mem::size_of::<usize>()) as *mut usize;
	\\            *size_ptr = total_size;
	\\
	\\            args.answer = base_ptr.add(size_storage_bytes) as *mut c_void;
	\\        }
	\\    }
	\\
	\\    /// Free memory previously allocated by `roc_alloc`.
	\\    pub extern "C" fn roc_dealloc(dealloc_args: *mut RocDealloc, _env: *mut c_void) {
	\\        unsafe {
	\\            let args = &*dealloc_args;
	\\            let min_alignment = args.alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\
	\\            let size_ptr = (args.ptr as *const u8).sub(core::mem::size_of::<usize>()) as *const usize;
	\\            let total_size = *size_ptr;
	\\
	\\            let base_ptr = (args.ptr as *mut u8).sub(size_storage_bytes);
	\\            debug_assert!(min_alignment.is_power_of_two(), "alignment must be a power of two");
	\\            let layout = Layout::from_size_align_unchecked(total_size, min_alignment);
	\\            std::alloc::dealloc(base_ptr, layout);
	\\        }
	\\    }
	\\
	\\    /// Reallocate memory, potentially extending the existing allocation in-place.
	\\    pub extern "C" fn roc_realloc(realloc_args: *mut RocRealloc, _env: *mut c_void) {
	\\        unsafe {
	\\            let args = &mut *realloc_args;
	\\            let min_alignment = args.alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\
	\\            // Read old size from metadata
	\\            let old_size_ptr = (args.answer as *const u8).sub(core::mem::size_of::<usize>()) as *const usize;
	\\            let old_total_size = *old_size_ptr;
	\\            let old_base_ptr = (args.answer as *mut u8).sub(size_storage_bytes);
	\\
	\\            // Realloc (may extend in-place without copying)
	\\            let new_total_size = args.new_length + size_storage_bytes;
	\\            debug_assert!(min_alignment.is_power_of_two(), "alignment must be a power of two");
	\\            let old_layout = Layout::from_size_align_unchecked(old_total_size, min_alignment);
	\\            let new_base_ptr = std::alloc::realloc(old_base_ptr, old_layout, new_total_size);
	\\            if new_base_ptr.is_null() {
	\\                eprintln!("roc_realloc: out of memory");
	\\                std::process::exit(1);
	\\            }
	\\
	\\            // Store new size and return user pointer
	\\            let new_user_ptr = new_base_ptr.add(size_storage_bytes);
	\\            let new_size_ptr = new_user_ptr.sub(core::mem::size_of::<usize>()) as *mut usize;
	\\            *new_size_ptr = new_total_size;
	\\            args.answer = new_user_ptr as *mut c_void;
	\\        }
	\\    }
	\\}
	\\

## Generate the DefaultHandlers
generate_default_handlers_rust : Str
generate_default_handlers_rust =
	\\/// Default handlers for dbg, expect-failed, and crash.
	\\///
	\\/// These print to stderr. Suitable for most platform hosts.
	\\pub struct DefaultHandlers;
	\\
	\\impl DefaultHandlers {
	\\    /// Print a `dbg` expression to stderr.
	\\    pub extern "C" fn roc_dbg(dbg_args: *const RocDbg, _env: *mut c_void) {
	\\        unsafe {
	\\            let args = &*dbg_args;
	\\            let msg = core::slice::from_raw_parts(args.utf8_bytes, args.len);
	\\            // SAFETY: Roc guarantees all strings are valid UTF-8.
	\\            let msg = core::str::from_utf8_unchecked(msg);
	\\            eprintln!("\\x1b[36m[ROC DBG]\\x1b[0m {}", msg);
	\\        }
	\\    }
	\\
	\\    /// Print a failed `expect` to stderr.
	\\    pub extern "C" fn roc_expect_failed(expect_args: *const RocExpectFailed, _env: *mut c_void) {
	\\        unsafe {
	\\            let args = &*expect_args;
	\\            let msg = core::slice::from_raw_parts(args.utf8_bytes, args.len);
	\\            // SAFETY: Roc guarantees all strings are valid UTF-8.
	\\            let msg = core::str::from_utf8_unchecked(msg);
	\\            eprintln!("\\x1b[33m[ROC EXPECT]\\x1b[0m {}", msg);
	\\        }
	\\    }
	\\
	\\    /// Print a `crash` message to stderr and exit.
	\\    pub extern "C" fn roc_crashed(crash_args: *const RocCrashed, _env: *mut c_void) {
	\\        unsafe {
	\\            let args = &*crash_args;
	\\            let msg = core::slice::from_raw_parts(args.utf8_bytes, args.len);
	\\            // SAFETY: Roc guarantees all strings are valid UTF-8.
	\\            let msg = core::str::from_utf8_unchecked(msg);
	\\            eprintln!("\\x1b[31m[ROC CRASHED]\\x1b[0m {}", msg);
	\\            std::process::exit(1);
	\\        }
	\\    }
	\\}
	\\

## Generate the make_roc_ops convenience function
generate_make_roc_ops_rust : Str
generate_make_roc_ops_rust =
	\\/// Create a RocOps struct with default memory management and error handlers.
	\\///
	\\/// This is a convenience function that wires together `DefaultAllocators`,
	\\/// `DefaultHandlers`, and the provided hosted functions into a ready-to-use `RocOps`.
	\\///
	\\/// # Arguments
	\\///
	\\/// * `env` - A pointer to your host environment (or null if unused)
	\\/// * `hosted_fns` - The hosted function dispatch table from `hosted_functions()`
	\\///
	\\/// # Example
	\\///
	\\/// ```ignore
	\\/// let roc_ops = make_roc_ops(core::ptr::null_mut(), hosted_functions(&fns));
	\\/// ```
	\\pub fn make_roc_ops(env: *mut c_void, hosted_fns: HostedFunctions) -> RocOps {
	\\    RocOps {
	\\        env,
	\\        roc_alloc: DefaultAllocators::roc_alloc,
	\\        roc_dealloc: DefaultAllocators::roc_dealloc,
	\\        roc_realloc: DefaultAllocators::roc_realloc,
	\\        roc_dbg: DefaultHandlers::roc_dbg,
	\\        roc_expect_failed: DefaultHandlers::roc_expect_failed,
	\\        roc_crashed: DefaultHandlers::roc_crashed,
	\\        hosted_fns,
	\\    }
	\\}
	\\

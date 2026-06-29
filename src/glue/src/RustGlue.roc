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
import pf.ProvidesEntry exposing [ProvidesEntry]
import pf.TypeTable exposing [TypeTable]
import pf.RocName exposing [RocName]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	# Collect all hosted functions from all modules, with module name prefix
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
					arg_fields: func.arg_fields,
					arg_type_ids: func.arg_type_ids,
					ffi_symbol: func.ffi_symbol,
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

	rust_content = generate_rust_file(sorted, $type_table, $provides_entries)

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
# TypeRepr-based Type Mapping
# =============================================================================

## Map a type table entry to its Rust type string using structured TypeRepr
type_id_to_rust = |type_table, duplicate_names, type_id| {
	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	match type_repr {
		RocRecord(rec) =>
			if rec.name == "" {
				"*mut c_void"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type_rust(type_table, duplicate_names, type_id, tu)
		_ => type_repr_to_rust(type_table, duplicate_names, type_id, type_repr)
	}
}

## Render one struct field declaration for a record field. Unnamed
## nominal-record padding fields become fixed-size byte arrays (`[u8; size]`);
## named fields use their resolved Rust type.
rust_record_field_decl = |type_table, duplicate_names, field| {
	field_name = name_to_rust_field_ident(field.name)
	rust_type = if field.is_padding {
		"[u8; ${U64.to_str(field.size)}]"
	} else {
		type_id_to_rust(type_table, duplicate_names, field.type_id)
	}
	"    pub ${field_name}: ${rust_type},\n"
}

## Convert a TypeRepr to its Rust type string
type_repr_to_rust = |type_table, duplicate_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match TypeTable.get(TypeTable.from_list(type_table), inner_id) {
				RocFunction(_) => "RocErasedCallable"
				RocUnknown(_) => "RocBox"
				_ => {
					inner_rust = type_id_to_rust(type_table, duplicate_names, inner_id)
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
		RocDec => "f64"
		RocList(elem_id) =>
			if is_type_refcounted(type_table, elem_id) {
				"RocList<${type_id_to_rust(type_table, duplicate_names, elem_id)}>"
			} else {
				"RocListWith<${type_id_to_rust(type_table, duplicate_names, elem_id)}, false>"
			}
		RocRecord(rec) =>
			if rec.name == "" {
				"*mut c_void"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type_rust(type_table, duplicate_names, type_id, tu)
		RocFunction(_) => "*mut c_void"
		RocUnknown(_) => "*mut c_void"
	}
}

## Resolve a tag union to a Rust type. Single-variant unions are unwrapped to their payload.
## Multi-variant unions with a name return a generated struct name.
resolve_tag_union_type_rust = |type_table, duplicate_names, type_id, tu| {
	match TypeTable.single_variant_payload(tu) {
		SinglePayload(payload_id) => type_id_to_rust(type_table, duplicate_names, payload_id)
		SingleNoPayload => "*mut c_void"
		NotSingleVariant =>
			if tu.name != "" {
				tag_union_struct_name(duplicate_names, type_id, tu)
			} else {
				"*mut c_void"
			}
	}
}

## Determine whether a type is refcounted (heap-allocated).
## Refcounted types need 2*ptr_width header space in list allocations.
is_type_refcounted : List(TypeRepr), U64 -> Bool
is_type_refcounted = |type_table, type_id| TypeTable.is_refcounted(TypeTable.from_list(type_table), type_id)

is_repr_refcounted : List(TypeRepr), TypeRepr -> Bool
is_repr_refcounted = |type_table, type_repr| TypeTable.repr_is_refcounted(TypeTable.from_list(type_table), type_repr)

# =============================================================================
# String Utilities
# =============================================================================

## Replace all occurrences of a substring
str_replace_all : Str, Str, Str -> Str
str_replace_all = |s, from, to| RocName.replace_all(s, from, to)

expect str_replace_all("a.b.c", ".", "_") == "a_b_c"
expect str_replace_all("hello!", "!", "") == "hello"

## Convert a string to lower_snake_case
to_lower_snake_case : Str -> Str
to_lower_snake_case = |s| RocName.lower_snake_ascii(s)

expect to_lower_snake_case("FooBar") == "foo_bar"
expect to_lower_snake_case("fooBar") == "foo_bar"
expect to_lower_snake_case("foo") == "foo"
expect to_lower_snake_case("FOO") == "foo"
expect to_lower_snake_case("Stdout_line") == "stdout_line"

## Convert a string to SCREAMING_SNAKE_CASE (for Rust constants)
to_screaming_snake_case : Str -> Str
to_screaming_snake_case = |s| RocName.screaming_snake_ascii(s)

expect to_screaming_snake_case("FooBar") == "FOO_BAR"
expect to_screaming_snake_case("fooBar") == "FOO_BAR"
expect to_screaming_snake_case("foo") == "FOO"
expect to_screaming_snake_case("FOO") == "FOO"
expect to_screaming_snake_case("Stdout_line") == "STDOUT_LINE"

## Capitalize the first character of a string
capitalize_first : Str -> Str
capitalize_first = |s| RocName.capitalize_first(s)

expect capitalize_first("hello") == "Hello"
expect capitalize_first("Hello") == "Hello"
expect capitalize_first("") == ""

## Lowercase the first character of a string
lowercase_first : Str -> Str
lowercase_first = |s| RocName.lowercase_first(s)

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
expect name_to_struct_name("line!") == "Line"
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"
expect name_to_struct_name("Builder.print_value!") == "BuilderPrintValue"
expect name_to_struct_name("__AnonStruct10") == "AnonStruct10"

## Find named multi-variant tag unions whose Roc name appears more than once in
## the type table. The result is computed once per glue run and reused while
## rendering type names.
duplicate_tag_union_names : List(TypeRepr) -> List(Str)
duplicate_tag_union_names = |type_table| TypeTable.duplicate_tag_union_names(TypeTable.from_list(type_table))

## Return the emitted Rust struct name for a multi-variant tag union.
##
## Generic tag unions used heavily by hosted functions can appear many times in
## the type table with different payload layouts. Rust needs a distinct concrete
## type for each layout.
tag_union_struct_name : List(Str), U64, TagUnionRepr -> Str
tag_union_struct_name = |duplicate_names, type_id, tu| {
	base = name_to_struct_name(tu.name)

	if List.contains(duplicate_names, tu.name) {
		"${base}Type${U64.to_str(type_id)}"
	} else {
		base
	}
}

hosted_module_name_to_struct_name = |name| {
	match List.first(Str.split_on(name, ".")) {
		Ok(module_name) => name_to_struct_name(module_name)
		Err(_) => name_to_struct_name(name)
	}
}

## RustGlue must keep distinct concrete structs for generic Roc types such as
## `Try` because different hosted functions can use different payload layouts.
## These aliases give platform authors stable API-level names while leaving the
## concrete layout names available for low-level generated code.
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

tag_union_has_payload_rust = |tu| TypeTable.tag_union_has_payload(tu)

add_tag_union_aliases_rust = |state, alias, target, tu| {
	with_main_alias = add_type_alias_rust(state, alias, target)

	if tag_union_has_payload_rust(tu) {
		with_payload_alias = add_type_alias_rust(with_main_alias, "${alias}Payload", "${target}Payload")
		add_type_alias_rust(with_payload_alias, "${alias}Tag", "${target}Tag")
	} else {
		with_main_alias
	}
}

collect_semantic_type_aliases_for_type_id_rust = |state, type_table, duplicate_names, type_id, alias_base, module_base, visited_type_ids| {
	if List.contains(visited_type_ids, type_id) {
		return state
	}

	next_visited = visited_type_ids.append(type_id)

	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	match type_repr {
		RocRecord(rec) =>
			if rec.name != "" and rec.anonymous {
				add_type_alias_rust(state, alias_base, type_id_to_rust(type_table, duplicate_names, type_id))
			} else {
				state
			}
		RocTagUnion(tu) =>
			match TypeTable.single_variant_payload(tu) {
				SinglePayload(payload_id) =>
					collect_semantic_type_aliases_for_type_id_rust(
						state,
						type_table,
						duplicate_names,
						payload_id,
						alias_base,
						module_base,
						next_visited,
					)
				SingleNoPayload => state
				NotSingleVariant =>
					if tu.name != "" {
						target = type_id_to_rust(type_table, duplicate_names, type_id)
						with_union_alias =
							if tu.name == "Try" {
								add_tag_union_aliases_rust(state, "${alias_base}Result", target, tu)
							} else if tu.name == "IOErr" {
								add_tag_union_aliases_rust(state, "${module_base}IOErr", target, tu)
							} else {
								state
							}

						var $next = with_union_alias
						for tag in tu.tags {
							child_base = "${alias_base}${capitalize_first(tag.name)}"
							for payload_id in tag.payload {
								$next = collect_semantic_type_aliases_for_type_id_rust(
									$next,
									type_table,
									duplicate_names,
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
		RocList(elem_id) => collect_semantic_type_aliases_for_type_id_rust(state, type_table, duplicate_names, elem_id, alias_base, module_base, next_visited)
		RocBox(inner_id) => collect_semantic_type_aliases_for_type_id_rust(state, type_table, duplicate_names, inner_id, alias_base, module_base, next_visited)
		_ => state
	}
}

generate_semantic_type_aliases_rust = |hosted_functions, type_table, duplicate_names| {
	var $state = { content: "", seen: [] }

	for func in hosted_functions {
		alias_base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)
		$state = collect_semantic_type_aliases_for_type_id_rust(
			$state,
			type_table,
			duplicate_names,
			func.ret_type_id,
			alias_base,
			module_base,
			[],
		)
	}

	if $state.content == "" {
		""
	} else {
		"// =============================================================================\n// Semantic Type Aliases\n// =============================================================================\n\n${$state.content}\n"
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

expect name_to_snake("Stdout.line!") == "stdout_line"
expect name_to_snake("line!") == "line"
expect name_to_snake("Foo.barBaz!") == "foo_bar_baz"
expect name_to_snake("PartDef.Idx.get!") == "part_def_idx_get"

## Convert a Roc name to a Rust snake_case function suffix.
name_to_rust_fn_suffix : Str -> Str
name_to_rust_fn_suffix = |name| {
	suffix =
		name
			->str_replace_all(".", "_")
			->str_replace_all("!", "")
			->str_replace_all("-", "_")
			->str_replace_all(" ", "_")
			->to_lower_snake_case()
			->strip_leading_underscores()

	if suffix == "" or suffix == "_" {
		"anon"
	} else {
		suffix
	}
}

expect name_to_rust_fn_suffix("Host.Tree") == "host_tree"
expect name_to_rust_fn_suffix("TryType17") == "try_type17"
expect name_to_rust_fn_suffix("__AnonStruct10") == "anon_struct10"

strip_leading_underscores : Str -> Str
strip_leading_underscores = |s| {
	bytes = Str.to_utf8(s)
	var $drop_count = 0
	var $done = Bool.False

	for byte in bytes {
		if !$done {
			if byte == '_' {
				$drop_count = $drop_count + 1
			} else {
				$done = Bool.True
			}
		}
	}

	new_bytes = List.drop_first(bytes, $drop_count)
	match Str.from_utf8(new_bytes) {
		Ok(str) => str
		Err(_) => s
	}
}

expect strip_leading_underscores("__anon") == "anon"
expect strip_leading_underscores("anon") == "anon"

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

## Convert a Roc record field name to a valid Rust field identifier.
## Rust cannot quote arbitrary punctuation in identifiers, so ABI structs use
## sanitized field names while preserving Roc's field order and repr(C) layout.
name_to_rust_field_ident : Str -> Str
name_to_rust_field_ident = |name| {
	sanitized =
		name
			->str_replace_all("!", "_bang")
			->str_replace_all("-", "_")
			->str_replace_all(".", "_")
			->str_replace_all(" ", "_")
			->to_lower_snake_case()

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

expect name_to_rust_field_ident("init!") == "init_bang"
expect name_to_rust_field_ident("render!") == "render_bang"
expect name_to_rust_field_ident("type") == "r#type"
expect name_to_rust_field_ident("_") == "_field"

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
	match TypeTable.record_layout(TypeTable.from_list(type_table), type_id) {
		RecordFound(layout) => { found: Bool.True, fields: layout.fields, size: layout.size, alignment: layout.alignment }
		NotRecord => { found: Bool.False, fields: [], size: 0, alignment: 0 }
	}
}

# =============================================================================
# Rust Code Generation
# =============================================================================

## Generate the complete Rust source file
generate_rust_file = |hosted_functions, type_table, provides_list| {
	duplicate_names = duplicate_tag_union_names(type_table)

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
		.concat(generate_element_type_structs_rust(type_table, duplicate_names))
		.concat(generate_tag_union_structs_rust(type_table, duplicate_names))
		.concat(generate_all_record_structs_rust(hosted_functions, type_table, duplicate_names))
		.concat(generate_all_args_structs_rust(hosted_functions, type_table, duplicate_names))
		.concat(generate_semantic_type_aliases_rust(hosted_functions, type_table, duplicate_names))
		.concat(generate_refcount_helpers_rust(type_table, duplicate_names))
		.concat("\n")
		.concat(generate_runtime_symbol_externs_rust)
		.concat("\n")
		.concat(generate_hosted_symbol_externs_rust(hosted_functions, type_table, duplicate_names))
		.concat("\n")
		.concat(generate_host_helpers_rust)
		.concat("\n")
		.concat(generate_entrypoint_externs_rust(provides_list, type_table, duplicate_names))
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
	\\use core::sync::atomic::{AtomicIsize, Ordering};
	\\use std::alloc::Layout;
	\\

## Generate self-contained host ABI type definitions (matching roc_ops.rs)
generate_host_abi_types_rust : Str
generate_host_abi_types_rust =
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
	\\/// Runtime representation of `Box(function)`.
	\\pub type RocErasedCallable = *mut u8;
	\\
	\\pub const ROC_ERASED_CALLABLE_CAPTURE_ALIGNMENT: usize = 16;
	\\pub const ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT: usize = 16;
	\\pub const ROC_ERASED_CALLABLE_CAPTURE_OFFSET: usize =
	\\    (core::mem::size_of::<RocErasedCallablePayload>() + 15) & !15;
	\\
	\\#[inline]
	\\pub const fn roc_erased_callable_payload_size(capture_size: usize) -> usize {
	\\    ROC_ERASED_CALLABLE_CAPTURE_OFFSET + capture_size
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
	\\    let base = roc_host.alloc(alignment, extra_bytes + roc_erased_callable_payload_size(capture_size)) as *mut u8;
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
	\\pub fn incref_box(data_ptr: RocBox, amount: isize) {
	\\    let data = match box_data_ptr(data_ptr) {
	\\        Some(ptr) => ptr,
	\\        None => return,
	\\    };
	\\    let rc = box_refcount_ptr(data);
	\\    unsafe {
	\\        if (*rc).load(Ordering::Relaxed) == 0 {
	\\            return; // REFCOUNT_STATIC_DATA
	\\        }
	\\        (*rc).fetch_add(amount, Ordering::Relaxed);
	\\    }
	\\}
	\\
	\\/// Allocate a Roc box and return a pointer to its payload data.
	\\pub fn allocate_box(
	\\    payload_size: usize,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    roc_host: &RocHost,
	\\) -> RocBox {
	\\    let ptr_width = core::mem::size_of::<usize>();
	\\    let required_space = if payload_contains_refcounted { 2 * ptr_width } else { ptr_width };
	\\    let header_bytes = required_space.max(payload_alignment);
	\\    let alloc_alignment = ptr_width.max(payload_alignment);
	\\    let base = unsafe { roc_host.alloc(alloc_alignment, header_bytes + payload_size) } as *mut u8;
	\\    let data = unsafe { base.add(header_bytes) };
	\\    unsafe {
	\\        let rc = data.sub(core::mem::size_of::<isize>()) as *mut isize;
	\\        *rc = 1;
	\\    }
	\\    data as RocBox
	\\}
	\\
	\\/// Decrement a pointer-aligned boxed payload with no Roc refcounted values.
	\\pub fn decref_box(data_ptr: RocBox, roc_host: &RocHost) {
	\\    decref_box_with(data_ptr, core::mem::align_of::<usize>(), false, None, roc_host);
	\\}
	\\
	\\/// Increment a boxed function closure.
	\\pub fn incref_erased_callable(callable: RocErasedCallable, amount: isize) {
	\\    incref_box(callable as RocBox, amount);
	\\}
	\\
	\\/// Decrement a boxed function closure and run its capture drop callback on final release.
	\\pub fn decref_erased_callable(callable: RocErasedCallable, roc_host: &RocHost) {
	\\    decref_box_with(
	\\        callable as RocBox,
	\\        ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT,
	\\        false,
	\\        Some(drop_erased_callable_payload),
	\\        roc_host,
	\\    );
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
	\\pub fn decref_box_with(
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
	\\    unsafe {
	\\        if (*rc).load(Ordering::Relaxed) == 0 {
	\\            return; // REFCOUNT_STATIC_DATA
	\\        }
	\\        let prev = (*rc).fetch_sub(1, Ordering::Relaxed);
	\\        if prev == 1 {
	\\            if let Some(callback) = payload_decref {
	\\                callback(data_ptr, roc_host as *const RocHost as *mut RocHost);
	\\            }
	\\            free_box_allocation(data, payload_alignment, payload_contains_refcounted, roc_host);
	\\        }
	\\    }
	\\}
	\\
	\\/// Free a boxed payload allocation immediately after running payload teardown.
	\\///
	\\/// See `decref_box_with` for the meaning of `payload_contains_refcounted`.
	\\pub fn free_box_with(
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
	\\    unsafe { (*rc).load(Ordering::Relaxed) == 1 }
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
	\\    let required_space = if payload_contains_refcounted { 2 * ptr_width } else { ptr_width };
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
	\\    /// Return the string contents as a `&str`, assuming valid UTF-8.
	\\    pub fn as_str(&self) -> &str {
	\\        // SAFETY: Roc guarantees all strings are valid UTF-8.
	\\        unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
	\\    }
	\\
	\\    /// Create a RocStr from a byte slice, using `roc_host` for heap allocation if needed.
	\\    pub fn from_slice(slice: &[u8], roc_host: &RocHost) -> Self {
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
	\\                capacity_or_alloc_ptr: slice.len() << 1,
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
	\\    pub fn decref(&self, roc_host: &RocHost) {
	\\        if self.is_small_str() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *mut AtomicIsize).sub(1);
	\\            if (*rc).load(Ordering::Relaxed) == 0 {
	\\                return; // REFCOUNT_STATIC_DATA — bytes are in read-only memory
	\\            }
	\\            let prev = (*rc).fetch_sub(1, Ordering::Relaxed);
	\\            if prev == 1 {
	\\                let ptr_width = core::mem::size_of::<usize>();
	\\                let base = alloc_ptr.sub(ptr_width) as *mut c_void;
	\\                roc_host.dealloc(base, core::mem::align_of::<usize>());
	\\            }
	\\        }
	\\    }
	\\
	\\    /// Increment the reference count by `amount`.
	\\    pub fn incref(&self, amount: isize) {
	\\        if self.is_small_str() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *mut AtomicIsize).sub(1);
	\\            if (*rc).load(Ordering::Relaxed) == 0 {
	\\                return; // REFCOUNT_STATIC_DATA
	\\            }
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
	\\            let count = (*rc).load(Ordering::Relaxed);
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
	\\#[derive(Clone, Copy)]
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
	\\    pub fn allocate(length: usize, roc_host: &RocHost) -> Self {
	\\        if length == 0 {
	\\            return Self::empty();
	\\        }
	\\        let align = core::mem::align_of::<T>().max(core::mem::align_of::<usize>());
	\\        let header_bytes = Self::header_bytes();
	\\        let data_bytes = length * core::mem::size_of::<T>();
	\\        let total = data_bytes + header_bytes;
	\\        let base = unsafe { roc_host.alloc(align, total) };
	\\        let data_ptr = unsafe { (base as *mut u8).add(header_bytes) };
	\\        // Write refcount = 1
	\\        unsafe {
	\\            let rc = (data_ptr as *mut isize).sub(1);
	\\            *rc = 1;
	\\        }
	\\        Self {
	\\            elements: data_ptr as *mut T,
	\\            length,
	\\            capacity_or_alloc_ptr: length << 1,
	\\        }
	\\    }
	\\
	\\    /// Create a RocList from a slice, copying elements into a new allocation.
	\\    pub fn from_slice(slice: &[T], roc_host: &RocHost) -> Self where T: Copy {
	\\        if slice.is_empty() {
	\\            return Self::empty();
	\\        }
	\\        let list = Self::allocate(slice.len(), roc_host);
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
	\\    pub fn decref(&self, roc_host: &RocHost) {
	\\        if self.elements.is_null() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        let align = core::mem::align_of::<T>().max(core::mem::align_of::<usize>());
	\\        let header_bytes = Self::header_bytes();
	\\        unsafe {
	\\            let rc = (alloc_ptr as *mut AtomicIsize).sub(1);
	\\            if (*rc).load(Ordering::Relaxed) == 0 {
	\\                return; // REFCOUNT_STATIC_DATA — elements are in read-only memory
	\\            }
	\\            let prev = (*rc).fetch_sub(1, Ordering::Relaxed);
	\\            if prev == 1 {
	\\                let base = alloc_ptr.sub(header_bytes) as *mut c_void;
	\\                roc_host.dealloc(base, align);
	\\            }
	\\        }
	\\    }
	\\
	\\    /// Increment the reference count by `amount`.
	\\    pub fn incref(&self, amount: isize) {
	\\        if self.elements.is_null() {
	\\            return;
	\\        }
	\\        let alloc_ptr = self.get_allocation_ptr();
	\\        if alloc_ptr.is_null() {
	\\            return;
	\\        }
	\\        unsafe {
	\\            let rc = (alloc_ptr as *mut AtomicIsize).sub(1);
	\\            if (*rc).load(Ordering::Relaxed) == 0 {
	\\                return; // REFCOUNT_STATIC_DATA
	\\            }
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
	\\            let count = (*rc).load(Ordering::Relaxed);
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
	\\            (*rc).load(Ordering::Relaxed) == 1
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
generate_element_type_structs_rust = |type_table, duplicate_names| {
	var $structs = ""
	var $seen_names = []

	for type_repr in type_table {
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" {
					struct_name = name_to_struct_name(rec.name)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)

						var $field_strs = ""
						for field in rec.fields {
							$field_strs = Str.concat($field_strs, rust_record_field_decl(type_table, duplicate_names, field))
						}

						# Size/alignment assertions (guarded by pointer width)
						assertions = if rec.size > 0 {
							"const _: () = assert!(core::mem::size_of::<${struct_name}>() == ${U64.to_str(rec.size)}, \"${struct_name} size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}>() == ${U64.to_str(rec.alignment)}, \"${struct_name} alignment mismatch\");\n\n"
						} else {
							""
						}

						$structs = Str.concat(
							$structs,
							"/// Element type for ${rec.name}\n#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${struct_name} {\n${$field_strs}}\n\n${assertions}",
						)
					}
				}
			_ => {}
		}
	}

	$structs
}

## Generate #[repr(C)] structs for tag union types found in the type table.
generate_tag_union_structs_rust = |type_table, duplicate_names| {
	var $structs = ""
	var $seen_names = []
	var $type_id = 0

	for type_repr in type_table {
		match type_repr {
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					struct_name = tag_union_struct_name(duplicate_names, $type_id, tu)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$structs = Str.concat($structs, generate_single_tag_union_rust(type_table, duplicate_names, $type_id, tu))
					}
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	$structs
}

## Generate Rust code for a single multi-variant tag union.
generate_single_tag_union_rust = |type_table, duplicate_names, type_id, tu| {
	struct_name = tag_union_struct_name(duplicate_names, type_id, tu)
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
					rust_type = type_id_to_rust(type_table, duplicate_names, pid)
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
					Ok(pid) => type_id_to_rust(type_table, duplicate_names, pid)
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

		"${$tuple_structs}/// Tag discriminant for ${tu.name}.\n#[repr(${disc_type})]\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum ${struct_name}Tag {\n${$enum_variants}}\n\n/// Tag union: ${tu.name}\n#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${struct_name} {\n    pub payload: ${struct_name}Payload,\n    pub tag: ${struct_name}Tag,\n}\n\n#[repr(C)]\n#[derive(Clone, Copy)]\npub union ${struct_name}Payload {\n${$union_fields}}\n\n${assertions}"
	}
}

## Generate #[repr(C)] structs for record return types using type table.
generate_all_record_structs_rust = |hosted_functions, type_table, duplicate_names| {
	var $structs = ""
	for func in hosted_functions {
		type_table_result = lookup_record_in_type_table(type_table, func.ret_type_id)

		if type_table_result.found {
			struct_name = name_to_struct_name(func.name)

			var $fields = ""
			for field in type_table_result.fields {
				$fields = Str.concat($fields, rust_record_field_decl(type_table, duplicate_names, field))
			}

			assertions = if type_table_result.size > 0 {
				"const _: () = assert!(core::mem::size_of::<${struct_name}RetRecord>() == ${U64.to_str(type_table_result.size)}, \"${struct_name}RetRecord size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}RetRecord>() == ${U64.to_str(type_table_result.alignment)}, \"${struct_name}RetRecord alignment mismatch\");\n\n"
			} else {
				""
			}

			doc = "/// Return type record for ${func.name}\n/// Fields ordered by alignment descending (Roc ABI)\n"
			$structs = Str.concat(
				$structs,
				"${doc}#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${struct_name}RetRecord {\n${$fields}}\n\n${assertions}",
			)
		}
	}
	$structs
}

## Generate all argument #[repr(C)] structs
generate_all_args_structs_rust = |hosted_functions, type_table, duplicate_names| {
	var $structs = ""
	for func in hosted_functions {
		$structs = Str.concat($structs, generate_args_struct_rust(func, type_table, duplicate_names))
	}
	$structs
}

## Generate a single argument struct (empty string if no args).
generate_args_struct_rust = |func, type_table, duplicate_names| {
	if !(has_meaningful_args_rust(func, type_table)) {
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
			$fields = Str.concat($fields, rust_record_field_decl(type_table, duplicate_names, field))
		}

		assertions = if type_table_result.size > 0 {
			"const _: () = assert!(core::mem::size_of::<${struct_name}Args>() == ${U64.to_str(type_table_result.size)}, \"${struct_name}Args size mismatch\");\nconst _: () = assert!(core::mem::align_of::<${struct_name}Args>() == ${U64.to_str(type_table_result.alignment)}, \"${struct_name}Args alignment mismatch\");\n\n"
		} else {
			""
		}

		doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"
		return "${doc}#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${struct_name}Args {\n${$fields}}\n\n${assertions}"
	}

	# Multi-arg or primitive args: use positional fields from type table
	var $positional_fields = ""
	var $idx = 0
	for arg_type_id in func.arg_type_ids {
		rust_type = type_id_to_rust(type_table, duplicate_names, arg_type_id)
		$positional_fields = Str.concat(
			$positional_fields,
			"    pub arg${U64.to_str($idx)}: ${rust_type},\n",
		)
		$idx = $idx + 1
	}

	doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"

	"${doc}#[repr(C)]\n#[derive(Clone, Copy)]\npub struct ${struct_name}Args {\n${$positional_fields}}\n\n"
}

has_meaningful_args_rust = |func, type_table| {
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

# =============================================================================
# Generated Refcount Helpers
# =============================================================================

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

box_payload_decref_name_rust = |inner_id| "decref_box_payload_type${U64.to_str(inner_id)}"

decref_helper_name_for_type_id_rust = |type_table, duplicate_names, type_id| {
	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	match type_repr {
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				"decref_${name_to_rust_fn_suffix(rec.name)}"
			}
		RocTagUnion(tu) =>
			if List.len(tu.tags) >= 2 and tu.name != "" {
				"decref_${name_to_rust_fn_suffix(tag_union_struct_name(duplicate_names, type_id, tu))}"
			} else {
				""
			}
		_ => ""
	}
}

incref_helper_name_for_type_id_rust = |type_table, duplicate_names, type_id| {
	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	match type_repr {
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				"incref_${name_to_rust_fn_suffix(rec.name)}"
			}
		RocTagUnion(tu) =>
			if List.len(tu.tags) >= 2 and tu.name != "" {
				"incref_${name_to_rust_fn_suffix(tag_union_struct_name(duplicate_names, type_id, tu))}"
			} else {
				""
			}
		_ => ""
	}
}

decref_stmt_for_type_id_rust = |type_table, duplicate_names, type_id, expr| {
	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	decref_stmt_for_repr_rust(type_table, duplicate_names, type_id, type_repr, expr)
}

decref_stmt_for_repr_rust = |type_table, duplicate_names, type_id, type_repr, expr| {
	match type_repr {
		RocStr => "    ${expr}.decref(roc_host);\n"
		RocList(elem_id) => {
			if is_type_refcounted(type_table, elem_id) {
				elem_stmt = decref_stmt_for_type_id_rust(type_table, duplicate_names, elem_id, "item")
				if elem_stmt == "" {
					"    compile_error!(\"missing decref helper for refcounted list element type id ${U64.to_str(elem_id)}\");\n"
				} else {
					"    {\n        let list = ${expr};\n        if list.has_one_ref() {\n            for item_ref in list.allocation_items() {\n                let item = *item_ref;\n${indent_lines(elem_stmt, "                ")}            }\n        }\n        list.decref(roc_host);\n    }\n"
				}
			} else {
				"    ${expr}.decref(roc_host);\n"
			}
		}
		RocBox(inner_id) =>
			match TypeTable.get(TypeTable.from_list(type_table), inner_id) {
				RocFunction(_) => "    decref_erased_callable(${expr}, roc_host);\n"
				_ => {
					inner_rust = type_id_to_rust(type_table, duplicate_names, inner_id)
					if inner_rust == "RocBox" or inner_rust == "*mut c_void" {
						"    decref_box(${expr} as RocBox, roc_host);\n"
					} else if is_type_refcounted(type_table, inner_id) {
						"    decref_box_with(${expr} as RocBox, core::mem::align_of::<${inner_rust}>(), true, Some(${box_payload_decref_name_rust(inner_id)}), roc_host);\n"
					} else {
						"    decref_box_with(${expr} as RocBox, core::mem::align_of::<${inner_rust}>(), false, None, roc_host);\n"
					}
				}
			}
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				helper = decref_helper_name_for_type_id_rust(type_table, duplicate_names, type_id)
				if helper == "" {
					""
				} else {
					"    ${helper}(${expr}, roc_host);\n"
				}
			}
		RocTagUnion(tu) =>
			if List.len(tu.tags) == 1 {
				match List.first(tu.tags) {
					Ok(tag) =>
						match List.first(tag.payload) {
							Ok(payload_id) => decref_stmt_for_type_id_rust(type_table, duplicate_names, payload_id, expr)
							_ => ""
						}
					_ => ""
				}
			} else {
				helper = decref_helper_name_for_type_id_rust(type_table, duplicate_names, type_id)
				if helper == "" {
					""
				} else {
					"    ${helper}(${expr}, roc_host);\n"
				}
			}
		_ => ""
	}
}

incref_stmt_for_type_id_rust = |type_table, duplicate_names, type_id, expr| {
	type_repr = TypeTable.get(TypeTable.from_list(type_table), type_id)
	incref_stmt_for_repr_rust(type_table, duplicate_names, type_id, type_repr, expr)
}

incref_stmt_for_repr_rust = |type_table, duplicate_names, type_id, type_repr, expr| {
	match type_repr {
		RocStr => "    ${expr}.incref(amount);\n"
		RocList(_) => "    ${expr}.incref(amount);\n"
		RocBox(inner_id) =>
			match TypeTable.get(TypeTable.from_list(type_table), inner_id) {
				RocFunction(_) => "    incref_erased_callable(${expr}, amount);\n"
				_ => "    incref_box(${expr} as RocBox, amount);\n"
			}
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				helper = incref_helper_name_for_type_id_rust(type_table, duplicate_names, type_id)
				if helper == "" {
					""
				} else {
					"    ${helper}(${expr}, amount);\n"
				}
			}
		RocTagUnion(tu) =>
			if List.len(tu.tags) == 1 {
				match List.first(tu.tags) {
					Ok(tag) =>
						match List.first(tag.payload) {
							Ok(payload_id) => incref_stmt_for_type_id_rust(type_table, duplicate_names, payload_id, expr)
							_ => ""
						}
					_ => ""
				}
			} else {
				helper = incref_helper_name_for_type_id_rust(type_table, duplicate_names, type_id)
				if helper == "" {
					""
				} else {
					"    ${helper}(${expr}, amount);\n"
				}
			}
		_ => ""
	}
}

generate_record_refcount_helpers_rust = |type_table, duplicate_names, rec| {
	struct_name = name_to_struct_name(rec.name)
	decref_name = "decref_${name_to_rust_fn_suffix(struct_name)}"
	incref_name = "incref_${name_to_rust_fn_suffix(struct_name)}"
	var $decref_body = ""
	var $incref_body = ""

	for field in rec.fields {
		if !field.is_padding {
			field_expr = "value.${name_to_rust_field_ident(field.name)}"
			$decref_body = Str.concat($decref_body, decref_stmt_for_type_id_rust(type_table, duplicate_names, field.type_id, field_expr))
			$incref_body = Str.concat($incref_body, incref_stmt_for_type_id_rust(type_table, duplicate_names, field.type_id, field_expr))
		}
	}

	if $decref_body == "" {
		$decref_body = "    let _ = value;\n    let _ = roc_host;\n"
	}
	if $incref_body == "" {
		$incref_body = "    let _ = value;\n    let _ = amount;\n"
	}

	"/// Recursively decrement Roc-owned fields in ${struct_name}.\npub fn ${decref_name}(value: ${struct_name}, roc_host: &RocHost) {\n${$decref_body}}\n\n/// Increment Roc-owned fields in ${struct_name}.\npub fn ${incref_name}(value: ${struct_name}, amount: isize) {\n${$incref_body}}\n\n"
}

generate_tag_payload_refcount_branch_rust = |type_table, duplicate_names, struct_name, tag, mode| {
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
						decref_stmt_for_type_id_rust(type_table, duplicate_names, payload_id, payload_expr)
					} else {
						incref_stmt_for_type_id_rust(type_table, duplicate_names, payload_id, payload_expr)
					}
				}
				_ => ""
			}

		if body == "" {
			"        ${struct_name}Tag::${variant} => {},\n"
		} else {
			"        ${struct_name}Tag::${variant} => unsafe {\n            let payload = core::mem::ManuallyDrop::into_inner(value.payload.${snake});\n${indent_lines(body, "        ")}        },\n"
		}
	} else {
		var $body = "            let payload = core::mem::ManuallyDrop::into_inner(value.payload.${snake});\n"
		var $idx = 0
		for payload_id in tag.payload {
			field_expr = "payload._${U64.to_str($idx)}"
			stmt = if mode == "decref" {
				decref_stmt_for_type_id_rust(type_table, duplicate_names, payload_id, field_expr)
			} else {
				incref_stmt_for_type_id_rust(type_table, duplicate_names, payload_id, field_expr)
			}
			$body = Str.concat($body, indent_lines(stmt, "        "))
			$idx = $idx + 1
		}

		"        ${struct_name}Tag::${variant} => unsafe {\n${$body}        },\n"
	}
}

generate_tag_union_refcount_helpers_rust = |type_table, duplicate_names, type_id, tu| {
	struct_name = tag_union_struct_name(duplicate_names, type_id, tu)
	decref_name = "decref_${name_to_rust_fn_suffix(struct_name)}"
	incref_name = "incref_${name_to_rust_fn_suffix(struct_name)}"
	var $decref_branches = ""
	var $incref_branches = ""
	for tag in tu.tags {
		$decref_branches = Str.concat($decref_branches, generate_tag_payload_refcount_branch_rust(type_table, duplicate_names, struct_name, tag, "decref"))
		$incref_branches = Str.concat($incref_branches, generate_tag_payload_refcount_branch_rust(type_table, duplicate_names, struct_name, tag, "incref"))
	}

	"/// Recursively decrement Roc-owned payloads in ${struct_name}.\npub fn ${decref_name}(value: ${struct_name}, roc_host: &RocHost) {\n    let _ = roc_host;\n    match value.tag {\n${$decref_branches}    }\n}\n\n/// Increment Roc-owned payloads in ${struct_name}.\npub fn ${incref_name}(value: ${struct_name}, amount: isize) {\n    let _ = amount;\n    match value.tag {\n${$incref_branches}    }\n}\n\n"
}

generate_box_payload_decref_helpers_rust = |type_table, duplicate_names| {
	var $helpers = ""
	var $seen_inner_ids = []

	for type_repr in type_table {
		match type_repr {
			RocBox(inner_id) => {
				if !(List.contains($seen_inner_ids, inner_id)) {
					$seen_inner_ids = $seen_inner_ids.append(inner_id)
					match TypeTable.get(TypeTable.from_list(type_table), inner_id) {
						RocFunction(_) => {}
						_ => {
							inner_rust = type_id_to_rust(type_table, duplicate_names, inner_id)
							if inner_rust != "RocBox" and inner_rust != "*mut c_void" and is_type_refcounted(type_table, inner_id) {
								stmt = decref_stmt_for_type_id_rust(type_table, duplicate_names, inner_id, "payload")
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

generate_refcount_helpers_rust = |type_table, duplicate_names| {
	var $helpers = "// =============================================================================\n// Generated Refcount Helpers\n// =============================================================================\n\n"
	var $seen_names = []
	var $type_id = 0

	for type_repr in type_table {
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" {
					struct_name = name_to_struct_name(rec.name)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$helpers = Str.concat($helpers, generate_record_refcount_helpers_rust(type_table, duplicate_names, rec))
					}
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					struct_name = tag_union_struct_name(duplicate_names, $type_id, tu)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$helpers = Str.concat($helpers, generate_tag_union_refcount_helpers_rust(type_table, duplicate_names, $type_id, tu))
					}
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	$helpers.concat(generate_box_payload_decref_helpers_rust(type_table, duplicate_names))
}

## Check whether a type is the zero-sized Roc unit type.
is_unit_type_id_rust = |type_table, type_id| TypeTable.is_unit(TypeTable.from_list(type_table), type_id)

## Check whether a type is an explicitly anonymous record shape.
is_anonymous_record_type_id_rust = |type_table, type_id| TypeTable.is_anonymous_record(TypeTable.from_list(type_table), type_id)

## Build a natural C ABI parameter list from Roc function argument type IDs.
direct_param_list_rust = |type_table, duplicate_names, arg_type_ids| {
	var $params = ""
	var $idx = 0

	for arg_type_id in arg_type_ids {
		if !is_unit_type_id_rust(type_table, arg_type_id) {
			arg_rust = type_id_to_rust(type_table, duplicate_names, arg_type_id)
			sep = if $params == "" {
				""
			} else {
				", "
			}
			$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_rust}"
			$idx = $idx + 1
		}
	}

	$params
}

## Build a hosted symbol parameter list, using the generated Args wrapper for
## anonymous single-record arguments so direct-symbol glue stays readable.
direct_hosted_param_list_rust = |type_table, duplicate_names, func| {
	use_args_wrapper =
		if List.len(func.arg_type_ids) == 1 {
			match List.first(func.arg_type_ids) {
				Ok(arg_id) => is_anonymous_record_type_id_rust(type_table, arg_id)
				Err(_) => Bool.False
			}
		} else {
			Bool.False
		}

	var $params = ""
	var $idx = 0

	for arg_type_id in func.arg_type_ids {
		if !is_unit_type_id_rust(type_table, arg_type_id) {
			arg_rust = if use_args_wrapper {
				"${name_to_struct_name(func.name)}Args"
			} else {
				type_id_to_rust(type_table, duplicate_names, arg_type_id)
			}
			sep = if $params == "" {
				""
			} else {
				", "
			}
			$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_rust}"
			$idx = $idx + 1
		}
	}

	$params
}

# =============================================================================
# Direct ABI Declarations
# =============================================================================

## Generate direct extern declarations for the fixed runtime symbols every host defines.
generate_runtime_symbol_externs_rust : Str
generate_runtime_symbol_externs_rust =
	\\// =============================================================================
	\\// Runtime Symbols
	\\//
	\\// The host defines these linker symbols. Compiled Roc code calls them directly.
	\\// =============================================================================
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
generate_hosted_symbol_externs_rust = |hosted_functions, type_table, duplicate_names| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $result = "// =============================================================================\n// Hosted Symbols\n//\n// The platform host must export these symbols with the exact direct C ABI signatures.\n// Refcounted arguments are owned by the hosted function.\n// =============================================================================\n\n#[allow(improper_ctypes)]\nunsafe extern \"C\" {\n"

	for func in hosted_functions {
		params = direct_hosted_param_list_rust(type_table, duplicate_names, func)
		ret_rust = type_id_to_rust(type_table, duplicate_names, func.ret_type_id)
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

generate_provided_decl_rust = |entry, type_table, duplicate_names, type_repr| {
	match type_repr {
		RocFunction(func) => {
			params = direct_param_list_rust(type_table, duplicate_names, func.args)
			ret_rust = type_id_to_rust(type_table, duplicate_names, func.ret)
			ret_suffix = if ret_rust == "()" {
				""
			} else {
				" -> ${ret_rust}"
			}
			"    /// Entrypoint: ${entry.name}\n    pub fn ${entry.ffi_symbol}(${params})${ret_suffix};\n\n"
		}
		_ => {
			value_rust = type_id_to_rust(type_table, duplicate_names, entry.type_id)
			"    /// Static provided value: ${entry.name}\n    pub static ${entry.ffi_symbol}: ${value_rust};\n\n"
		}
	}
}

## Generate extern declarations for entrypoints from the provides clause.
generate_entrypoint_externs_rust = |provides_list, type_table, duplicate_names| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $result = "// =============================================================================\n// Provided Symbols\n//\n// Roc exports these symbols from the app with their natural C ABI signatures.\n// =============================================================================\n\n#[allow(improper_ctypes)]\nunsafe extern \"C\" {\n"

	for entry in provides_list {
		type_repr = TypeTable.get(TypeTable.from_list(type_table), entry.type_id)
		$result = Str.concat($result, generate_provided_decl_rust(entry, type_table, duplicate_names, type_repr))
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
	\\pub struct DefaultAllocators;
	\\
	\\impl DefaultAllocators {
	\\    /// Allocate memory using the Rust global allocator.
	\\    pub extern "C" fn roc_alloc(_roc_host: *mut RocHost, length: usize, alignment: usize) -> *mut c_void {
	\\        unsafe {
	\\            let min_alignment = alignment.max(core::mem::align_of::<usize>());
	\\            let size_storage_bytes = min_alignment;
	\\            let total_size = length + size_storage_bytes;
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
	\\            let new_total_size = new_length + size_storage_bytes;
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
	\\pub struct DefaultHandlers;
	\\
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

## A glue script for generating a Zig source file with hosted function bindings.
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

	zig_content = generate_zig_file(sorted, $type_table)

	Ok([{ name: "roc_platform_abi.zig", content: zig_content }])
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
# Type String Parsing (reused from CGlue.roc)
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
# Roc Type to Zig Type Mapping
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
		_ => "*anyopaque"
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
expect roc_type_to_zig("{ foo : Str }") == "*anyopaque"

# =============================================================================
# TypeRepr-based Type Mapping
# =============================================================================

## Map a type table entry to its Zig type string using structured TypeRepr
type_id_to_zig : List(TypeRepr), U64 -> Str
type_id_to_zig = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => type_repr_to_zig(type_table, type_repr)
		Err(_) => "*anyopaque"
	}
}

## Convert a TypeRepr to its Zig type string
type_repr_to_zig : List(TypeRepr), TypeRepr -> Str
type_repr_to_zig = |type_table, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocStr => "RocStr"
		RocUnit => "void"
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
		RocList(elem_id) => "RocList(${type_id_to_zig(type_table, elem_id)})"
		RocRecord(rec) =>
			if rec.name == "" {
				"*anyopaque"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type(type_table, tu)
		RocFunction(_) => "*anyopaque"
		RocUnknown(_) => "*anyopaque"
	}
}

## Resolve a tag union to a Zig type. Single-variant unions are unwrapped to their payload.
## Multi-variant unions with a name return a generated struct name.
resolve_tag_union_type = |type_table, tu| {
	if List.len(tu.tags) == 1 {
		match List.first(tu.tags) {
			Ok(tag) =>
				match List.first(tag.payload) {
					Ok(payload_id) => type_id_to_zig(type_table, payload_id)
					_ => "*anyopaque"
				}
			_ => "*anyopaque"
		}
	} else if tu.name != "" {
		name_to_struct_name(tu.name)
	} else {
		"*anyopaque"
	}
}

## Generate the RocList(T) generic type function (static Zig code)
generate_roc_list_generic : Str
generate_roc_list_generic =
	\\/// A generic Roc list. Elements are reference-counted and heap-allocated.
	\\///
	\\/// When `elements_refcounted` is true (the default), an extra `ptr_width` bytes
	\\/// are reserved in the allocation header for the element count, matching the Roc
	\\/// runtime's `allocateWithRefcount` layout. Set to `false` for lists of
	\\/// non-refcounted primitives (e.g. `U8`, `I32`).
	\\pub fn RocList(comptime T: type) type {
	\\    return RocListWith(T, true);
	\\}
	\\
	\\pub fn RocListWith(comptime T: type, comptime elements_refcounted: bool) type {
	\\    return extern struct {
	\\        elements_ptr: ?[*]T,
	\\        length: usize,
	\\        capacity_or_alloc_ptr: usize,
	\\
	\\        const Self = @This();
	\\        const ptr_width = @sizeOf(usize);
	\\        const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
	\\        const header_bytes = @max(required_space, @alignOf(T));
	\\        const alloc_align = @max(ptr_width, @alignOf(T));
	\\
	\\        /// Return the list elements as a `[]const T` slice.
	\\        pub fn items(self: Self) []const T {
	\\            if (self.elements_ptr) |ptr| return ptr[0..self.length];
	\\            return &[_]T{};
	\\        }
	\\
	\\        /// Return the number of elements in the list.
	\\        pub fn len(self: Self) usize {
	\\            return self.length;
	\\        }
	\\
	\\        /// Return true if the list has zero elements.
	\\        pub fn isEmpty(self: Self) bool {
	\\            return self.length == 0;
	\\        }
	\\
	\\        /// Return an empty RocList.
	\\        pub fn empty() Self {
	\\            return .{ .elements_ptr = null, .length = 0, .capacity_or_alloc_ptr = 0 };
	\\        }
	\\
	\\        /// Allocate a new list with space for `length` elements.
	\\        pub fn allocate(length: usize, roc_ops: *RocOps) Self {
	\\            if (length == 0) return empty();
	\\            const data_bytes = length * @sizeOf(T);
	\\            const total = data_bytes + header_bytes;
	\\            var alloc_args: RocAlloc = .{
	\\                .alignment = alloc_align,
	\\                .length = total,
	\\                .answer = undefined,
	\\            };
	\\            roc_ops.roc_alloc(&alloc_args, roc_ops.env);
	\\            const base: [*]u8 = @ptrCast(alloc_args.answer);
	\\            const data_ptr = base + header_bytes;
	\\            const rc: *isize = @ptrFromInt(@intFromPtr(data_ptr) - @sizeOf(isize));
	\\            rc.* = 1;
	\\            return .{
	\\                .elements_ptr = @ptrCast(@alignCast(data_ptr)),
	\\                .length = length,
	\\                .capacity_or_alloc_ptr = length,
	\\            };
	\\        }
	\\
	\\        /// Create a RocList from a slice, copying elements into a new allocation.
	\\        pub fn fromSlice(slice: []const T, roc_ops: *RocOps) Self {
	\\            if (slice.len == 0) return empty();
	\\            const list = allocate(slice.len, roc_ops);
	\\            const dest: [*]u8 = @ptrCast(list.elements_ptr.?);
	\\            const src: [*]const u8 = @ptrCast(slice.ptr);
	\\            @memcpy(dest[0 .. slice.len * @sizeOf(T)], src[0 .. slice.len * @sizeOf(T)]);
	\\            return list;
	\\        }
	\\
	\\        /// Decrement the reference count; frees the allocation when it reaches zero.
	\\        pub fn decref(self: Self, roc_ops: *RocOps) void {
	\\            const ptr = self.elements_ptr orelse return;
	\\            const data_addr = @intFromPtr(ptr);
	\\            const rc: *isize = @ptrFromInt(data_addr - @sizeOf(isize));
	\\            const prev = @atomicRmw(isize, rc, .Sub, 1, .monotonic);
	\\            if (prev == 1) {
	\\                const base: *anyopaque = @ptrFromInt(data_addr - header_bytes);
	\\                var dealloc_args: RocDealloc = .{
	\\                    .alignment = alloc_align,
	\\                    .ptr = base,
	\\                };
	\\                roc_ops.roc_dealloc(&dealloc_args, roc_ops.env);
	\\            }
	\\        }
	\\
	\\        /// Increment the reference count by `amount`.
	\\        pub fn incref(self: Self, amount: isize) void {
	\\            const ptr = self.elements_ptr orelse return;
	\\            const rc: *isize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(isize));
	\\            _ = @atomicRmw(isize, rc, .Add, amount, .monotonic);
	\\        }
	\\
	\\        /// Return true if this list has a reference count of exactly one.
	\\        pub fn isUnique(self: Self) bool {
	\\            const ptr = self.elements_ptr orelse return true;
	\\            if (self.capacity_or_alloc_ptr == 0) return true;
	\\            const rc: *const isize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(isize));
	\\            return rc.* == 1;
	\\        }
	\\    };
	\\}
	\\

## Generate extern structs for element types found in the type table.
## Scans for Record types and generates Zig extern structs for them.
## Fields arrive pre-sorted by alignment descending from the compiler.
generate_element_type_structs : List(TypeRepr) -> Str
generate_element_type_structs = |type_table| {
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
						zig_type = type_id_to_zig(type_table, field.type_id)
						$field_strs = Str.concat(
							$field_strs,
							"    ${field.name}: ${zig_type},\n",
						)
					}

					# Comptime size/alignment assertions (guarded by pointer width)
					assertions = if rec.size > 0 {
						"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}) != ${U64.to_str(rec.size)}) @compileError(\"${struct_name} size mismatch\");\n        if (@alignOf(${struct_name}) != ${U64.to_str(rec.alignment)}) @compileError(\"${struct_name} alignment mismatch\");\n    }\n}\n\n"
					} else {
						""
					}

					$structs = Str.concat(
						$structs,
						"/// Element type for ${rec.name}\npub const ${struct_name} = extern struct {\n${$field_strs}};\n\n${assertions}",
					)
				}
			_ => {}
		}
	}

	$structs
}

## Generate extern structs for tag union types found in the type table.
## Multi-variant tag unions get a tag enum, payload extern union, and wrapping extern struct.
## Pure enums (all variants have no payload) get just an enum.
generate_tag_union_structs : List(TypeRepr) -> Str
generate_tag_union_structs = |type_table| {
	var $structs = ""
	var $seen_names = []

	for type_repr in type_table {
		match type_repr {
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" and !(List.contains($seen_names, tu.name)) {
					$seen_names = $seen_names.append(tu.name)
					$structs = Str.concat($structs, generate_single_tag_union(type_table, tu))
				}
			_ => {}
		}
	}

	$structs
}

## Generate Zig code for a single multi-variant tag union.
generate_single_tag_union = |type_table, tu| {
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
			snake = to_lower_snake_case(tag.name)
			$variants = Str.concat($variants, "    ${snake} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		"/// Tag union: ${tu.name}\npub const ${struct_name} = enum(${disc_type}) {\n${$variants}};\n\n"
	} else {
		# Generate tuple structs for any variant with >1 payload
		var $tuple_structs = ""
		for tag in tu.tags {
			if List.len(tag.payload) > 1 {
				tuple_name = "${struct_name}${capitalize_first(tag.name)}Payload"
				var $tuple_fields = ""
				var $ti = 0
				for pid in tag.payload {
					zig_type = type_id_to_zig(type_table, pid)
					$tuple_fields = Str.concat($tuple_fields, "    _${U64.to_str($ti)}: ${zig_type},\n")
					$ti = $ti + 1
				}
				$tuple_structs = Str.concat($tuple_structs, "/// Payload struct for ${tag.name} variant.\npub const ${tuple_name} = extern struct {\n${$tuple_fields}};\n\n")
			}
		}

		# Tag enum
		var $enum_variants = ""
		var $idx = 0
		for enum_tag in tu.tags {
			$enum_variants = Str.concat($enum_variants, "    ${enum_tag.name} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		# Payload extern union
		var $union_fields = ""
		for union_tag in tu.tags {
			snake = to_lower_snake_case(union_tag.name)
			if List.is_empty(union_tag.payload) {
				# No-payload variant: use [0]u8 (Zig extern unions can't have void)
				$union_fields = Str.concat($union_fields, "        ${snake}: [0]u8,\n")
			} else if List.len(union_tag.payload) == 1 {
				zig_type = match List.first(union_tag.payload) {
					Ok(pid) => type_id_to_zig(type_table, pid)
					Err(_) => "*anyopaque"
				}
				$union_fields = Str.concat($union_fields, "        ${snake}: ${zig_type},\n")
			} else {
				tuple_name = "${struct_name}${capitalize_first(union_tag.name)}Payload"
				$union_fields = Str.concat($union_fields, "        ${snake}: ${tuple_name},\n")
			}
		}

		# Comptime assertions
		assertions = if tu.size > 0 {
			"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}) != ${U64.to_str(tu.size)}) @compileError(\"${struct_name} size mismatch\");\n        if (@alignOf(${struct_name}) != ${U64.to_str(tu.alignment)}) @compileError(\"${struct_name} alignment mismatch\");\n    }\n}\n\n"
		} else {
			""
		}

		"${$tuple_structs}/// Tag discriminant for ${tu.name}.\npub const ${struct_name}Tag = enum(${disc_type}) {\n${$enum_variants}};\n\n/// Tag union: ${tu.name}\npub const ${struct_name} = extern struct {\n    payload: extern union {\n${$union_fields}    },\n    tag: ${struct_name}Tag,\n};\n\n${assertions}"
	}
}

## Return the Zig discriminant type for a given tag count.
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

## Convert function name to camelCase for Zig function names (e.g., "Stdout.line!" -> "hostedStdoutLine")
name_to_camel : Str -> Str
name_to_camel = |name| {
	parts = Str.split_on(name, ".")

	var $result = ""
	var $first = Bool.True
	for part in parts {
		cleaned = str_replace_all(part, "!", "")
		if $first {
			$result = Str.concat($result, lowercase_first(cleaned))
			$first = Bool.False
		} else {
			$result = Str.concat($result, capitalize_first(cleaned))
		}
	}

	$result
}

expect name_to_camel("Stdout.line!") == "stdoutLine"
expect name_to_camel("Echo.line!") == "echoLine"
expect name_to_camel("PartDef.Idx.get!") == "partDefIdxGet"

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
# Zig Code Generation
# =============================================================================

## Generate the complete Zig source file
generate_zig_file = |hosted_functions, type_table| {
	count = List.len(hosted_functions)

	file_header
		.concat(generate_imports)
		.concat("\n")
		.concat(generate_host_abi_types)
		.concat("\n")
		.concat(generate_roc_str)
		.concat("\n")
		.concat(generate_roc_list_generic)
		.concat("\n")
		.concat(generate_index_constants(hosted_functions, count))
		.concat("\n")
		.concat(generate_element_type_structs(type_table))
		.concat(generate_tag_union_structs(type_table))
		.concat(generate_all_record_structs(hosted_functions, type_table))
		.concat(generate_all_args_structs(hosted_functions, type_table))
		.concat(generate_platform_fns_struct(hosted_functions, type_table))
		.concat("\n")
		.concat(generate_hosted_functions_helper(hosted_functions))
		.concat("\n")
		.concat(generate_host_helpers)
}

## File header comment
file_header : Str
file_header =
	"//! Roc Platform ABI\n//!\n//! This file defines the Zig interface for hosted functions in a Roc platform.\n//! It is automatically generated by the Roc glue generator.\n//!\n//! Usage:\n//! 1. Import this file in your platform host\n//! 2. Implement each hosted function according to its signature\n//! 3. Call hostedFunctions() to create the dispatch table for RocOps\n\n"

## Import section
generate_imports : Str
generate_imports =
	"const std = @import(\"std\");\n"

## Generate self-contained host ABI type definitions
generate_host_abi_types : Str
generate_host_abi_types =
	\\/// Type-erased function pointer for hosted function dispatch.
	\\pub const HostedFn = *const fn (*anyopaque, *anyopaque, *anyopaque) callconv(.c) void;
	\\
	\\/// Table of hosted function pointers passed to the Roc runtime.
	\\pub const HostedFunctions = extern struct {
	\\    count: u32,
	\\    fns: [*]HostedFn,
	\\};
	\\
	\\/// Arguments for a Roc allocation request.
	\\pub const RocAlloc = extern struct { alignment: usize, length: usize, answer: *anyopaque };
	\\/// Arguments for a Roc deallocation request.
	\\pub const RocDealloc = extern struct { alignment: usize, ptr: *anyopaque };
	\\/// Arguments for a Roc reallocation request.
	\\pub const RocRealloc = extern struct { alignment: usize, new_length: usize, answer: *anyopaque };
	\\/// Arguments for a Roc `dbg` expression.
	\\pub const RocDbg = extern struct { utf8_bytes: [*]u8, len: usize };
	\\/// Arguments for a failed Roc `expect`.
	\\pub const RocExpectFailed = extern struct { utf8_bytes: [*]u8, len: usize };
	\\/// Arguments for a Roc `crash`.
	\\pub const RocCrashed = extern struct { utf8_bytes: [*]u8, len: usize };
	\\
	\\/// The operations table passed from the host to the Roc runtime.
	\\pub const RocOps = extern struct {
	\\    env: *anyopaque,
	\\    roc_alloc: *const fn (*RocAlloc, *anyopaque) callconv(.c) void,
	\\    roc_dealloc: *const fn (*RocDealloc, *anyopaque) callconv(.c) void,
	\\    roc_realloc: *const fn (*RocRealloc, *anyopaque) callconv(.c) void,
	\\    roc_dbg: *const fn (*const RocDbg, *anyopaque) callconv(.c) void,
	\\    roc_expect_failed: *const fn (*const RocExpectFailed, *anyopaque) callconv(.c) void,
	\\    roc_crashed: *const fn (*const RocCrashed, *anyopaque) callconv(.c) void,
	\\    hosted_fns: HostedFunctions,
	\\};
	\\
	\\/// Type-erase a hosted function pointer to `HostedFn`.
	\\///
	\\/// Hosted functions are typically written with concrete parameter types for clarity
	\\/// (e.g. `*RocOps`, `*RocStr`, `[*]u8`), but must be stored as `HostedFn` which
	\\/// uses `*anyopaque` for all parameters. This helper performs that cast.
	\\pub fn hostedFn(func: anytype) HostedFn {
	\\    const T = @TypeOf(func);
	\\    const info = @typeInfo(T);
	\\    if (info == .pointer) {
	\\        const child = @typeInfo(info.pointer.child);
	\\        if (child == .@"fn") {
	\\            const f = child.@"fn";
	\\            if (f.params.len != 3)
	\\                @compileError("hostedFn: function must take exactly 3 parameters (ops, ret_ptr, args_ptr)");
	\\            if (f.return_type != void)
	\\                @compileError("hostedFn: function must return void");
	\\        }
	\\    }
	\\    return @ptrCast(func);
	\\}
	\\

## Generate self-contained RocStr type definition
generate_roc_str : Str
generate_roc_str =
	\\/// A Roc string value. Small strings (up to 23 bytes) are stored inline;
	\\/// larger strings are heap-allocated with a reference count.
	\\pub const RocStr = extern struct {
	\\    bytes: ?[*]u8,
	\\    length: usize,
	\\    capacity_or_alloc_ptr: usize,
	\\
	\\    const Self = @This();
	\\    const small_string_size = @sizeOf(RocStr);
	\\    const small_str_max_length = small_string_size - 1;
	\\    const seamless_slice_bit: usize = @as(usize, @bitCast(@as(isize, std.math.minInt(isize))));
	\\
	\\    /// Return the string contents as a `[]const u8` slice.
	\\    pub fn asSlice(self: *const Self) []const u8 {
	\\        return self.asU8ptr()[0..self.len()];
	\\    }
	\\
	\\    /// Return the length of the string in bytes.
	\\    pub fn len(self: Self) usize {
	\\        if (self.isSmallStr()) {
	\\            return @as([*]const u8, @ptrCast(&self))[@sizeOf(Self) - 1] ^ 0b1000_0000;
	\\        } else {
	\\            return self.length & ~seamless_slice_bit;
	\\        }
	\\    }
	\\
	\\    /// Return true if the string has zero length.
	\\    pub fn isEmpty(self: Self) bool {
	\\        return self.len() == 0;
	\\    }
	\\
	\\    /// Return true if this string is stored inline (small string optimization).
	\\    pub fn isSmallStr(self: Self) bool {
	\\        return @as(isize, @bitCast(self.capacity_or_alloc_ptr)) < 0;
	\\    }
	\\
	\\    /// Return true if this string is a seamless slice into another allocation.
	\\    pub fn isSeamlessSlice(self: Self) bool {
	\\        return !self.isSmallStr() and @as(isize, @bitCast(self.length)) < 0;
	\\    }
	\\
	\\    /// Return a pointer to the raw UTF-8 bytes.
	\\    pub fn asU8ptr(self: *const Self) [*]const u8 {
	\\        if (self.isSmallStr()) {
	\\            return @as([*]const u8, @ptrCast(self));
	\\        } else {
	\\            return @as([*]const u8, @ptrCast(self.bytes));
	\\        }
	\\    }
	\\
	\\    /// Return an empty RocStr.
	\\    pub fn empty() Self {
	\\        return .{ .bytes = null, .length = 0, .capacity_or_alloc_ptr = seamless_slice_bit };
	\\    }
	\\
	\\    /// Create a RocStr from a byte slice, using `roc_ops` for heap allocation if needed.
	\\    pub fn fromSlice(slice: []const u8, roc_ops: *RocOps) Self {
	\\        if (slice.len < small_string_size) {
	\\            var result = Self.empty();
	\\            const ptr: [*]u8 = @ptrCast(&result);
	\\            @memcpy(ptr[0..slice.len], slice);
	\\            ptr[@sizeOf(Self) - 1] = @as(u8, @intCast(slice.len)) | 0b1000_0000;
	\\            return result;
	\\        } else {
	\\            const ptr_width = @sizeOf(usize);
	\\            const extra_bytes = ptr_width;
	\\            const total = extra_bytes + slice.len;
	\\            var alloc_args: RocAlloc = .{
	\\                .alignment = @alignOf(usize),
	\\                .length = total,
	\\                .answer = undefined,
	\\            };
	\\            roc_ops.roc_alloc(&alloc_args, roc_ops.env);
	\\            const base: [*]u8 = @ptrCast(alloc_args.answer);
	\\            const data_ptr = base + extra_bytes;
	\\            const rc: *isize = @ptrFromInt(@intFromPtr(data_ptr) - @sizeOf(isize));
	\\            rc.* = 1;
	\\            @memcpy(data_ptr[0..slice.len], slice.ptr[0..slice.len]);
	\\            return .{
	\\                .bytes = data_ptr,
	\\                .length = slice.len,
	\\                .capacity_or_alloc_ptr = slice.len,
	\\            };
	\\        }
	\\    }
	\\
	\\    /// Decrement the reference count; frees the allocation when it reaches zero.
	\\    pub fn decref(self: Self, roc_ops: *RocOps) void {
	\\        if (self.isSmallStr()) return;
	\\        const alloc_ptr = self.getAllocationPtr() orelse return;
	\\        const data_addr = @intFromPtr(alloc_ptr);
	\\        const rc: *isize = @ptrFromInt(data_addr - @sizeOf(isize));
	\\        const prev = @atomicRmw(isize, rc, .Sub, 1, .monotonic);
	\\        if (prev == 1) {
	\\            const ptr_width = @sizeOf(usize);
	\\            const base: *anyopaque = @ptrFromInt(data_addr - ptr_width);
	\\            var dealloc_args: RocDealloc = .{ .alignment = @alignOf(usize), .ptr = base };
	\\            roc_ops.roc_dealloc(&dealloc_args, roc_ops.env);
	\\        }
	\\    }
	\\
	\\    /// Increment the reference count by `amount`.
	\\    pub fn incref(self: Self, amount: isize) void {
	\\        if (self.isSmallStr()) return;
	\\        const alloc_ptr = self.getAllocationPtr() orelse return;
	\\        const rc: *isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\        _ = @atomicRmw(isize, rc, .Add, amount, .monotonic);
	\\    }
	\\
	\\    /// Return true if this string has a reference count of exactly one.
	\\    pub fn isUnique(self: Self) bool {
	\\        if (self.isSmallStr()) return true;
	\\        if (self.capacity_or_alloc_ptr == 0) return true;
	\\        const alloc_ptr = self.getAllocationPtr() orelse return true;
	\\        const rc: *const isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\        return rc.* == 1;
	\\    }
	\\
	\\    fn getAllocationPtr(self: Self) ?[*]u8 {
	\\        if (self.isSeamlessSlice()) {
	\\            return @as(?[*]u8, @ptrFromInt(self.capacity_or_alloc_ptr << 1));
	\\        } else {
	\\            return self.bytes;
	\\        }
	\\    }
	\\};
	\\

## Generate index constants and count
generate_index_constants = |hosted_functions, count| {
	var $constants = "/// Total number of hosted functions in this platform.\npub const hosted_function_count: u32 = ${U64.to_str(count)};\n\n"

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		$constants = Str.concat(
			$constants,
			"/// Dispatch index for ${func.name}.\npub const hosted_idx_${snake}: u32 = ${U64.to_str(func.index)};\n",
		)
	}

	$constants
}

## Generate extern structs for record return types using type table (correctly sorted by alignment).
## Only generates RetRecord structs when ret_type_id resolves to a record in the type table.
## Tag union return types (e.g., Try(Record, Str)) are not yet supported and are skipped.
generate_all_record_structs = |hosted_functions, type_table| {
	var $structs = ""
	for func in hosted_functions {
		# Only generate RetRecord if the return type is actually a record
		type_table_result = lookup_record_in_type_table(type_table, func.ret_type_id)

		if type_table_result.found {
			struct_name = name_to_struct_name(func.name)

			var $fields = ""
			for field in type_table_result.fields {
				zig_type = type_id_to_zig(type_table, field.type_id)
				$fields = Str.concat(
					$fields,
					"    ${field.name}: ${zig_type},\n",
				)
			}

			assertions = if type_table_result.size > 0 {
				"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}RetRecord) != ${U64.to_str(type_table_result.size)}) @compileError(\"${struct_name}RetRecord size mismatch\");\n        if (@alignOf(${struct_name}RetRecord) != ${U64.to_str(type_table_result.alignment)}) @compileError(\"${struct_name}RetRecord alignment mismatch\");\n    }\n}\n\n"
			} else {
				""
			}

			doc = "/// Return type record for ${func.name}\n/// Fields ordered by alignment descending (Roc ABI)\n"
			$structs = Str.concat(
				$structs,
				"${doc}pub const ${struct_name}RetRecord = extern struct {\n${$fields}};\n\n${assertions}",
			)
		}
		# else: return type is not a record (tag union, primitive, etc.) — skip RetRecord generation
	}
	$structs
}

## Generate all argument extern structs
generate_all_args_structs = |hosted_functions, type_table| {
	var $structs = ""
	for func in hosted_functions {
		$structs = Str.concat($structs, generate_args_struct(func, type_table))
	}
	$structs
}

## Generate a single argument extern struct (empty string if no args).
## Uses type table for single-record args; positional for multi-arg or primitive args.
generate_args_struct = |func, type_table| {
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
			zig_type = type_id_to_zig(type_table, field.type_id)
			$fields = Str.concat(
				$fields,
				"    ${field.name}: ${zig_type},\n",
			)
		}

		assertions = if type_table_result.size > 0 {
			"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}Args) != ${U64.to_str(type_table_result.size)}) @compileError(\"${struct_name}Args size mismatch\");\n        if (@alignOf(${struct_name}Args) != ${U64.to_str(type_table_result.alignment)}) @compileError(\"${struct_name}Args alignment mismatch\");\n    }\n}\n\n"
		} else {
			""
		}

		doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n"
		return "${doc}pub const ${struct_name}Args = extern struct {\n${$fields}};\n\n${assertions}"
	}

	# Multi-arg or primitive args: use positional fields from type table
	var $positional_fields = ""
	var $idx = 0
	for arg_type_id in func.arg_type_ids {
		zig_type = type_id_to_zig(type_table, arg_type_id)
		$positional_fields = Str.concat(
			$positional_fields,
			"    arg${U64.to_str($idx)}: ${zig_type},\n",
		)
		$idx = $idx + 1
	}

	doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n"

	"${doc}pub const ${struct_name}Args = extern struct {\n${$positional_fields}};\n\n"
}

## Generate the PlatformHostedFns struct type
generate_platform_fns_struct = |hosted_functions, type_table| {
	var $fields = ""

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		fn_type = hosted_fn_type(func, type_table)

		$fields = Str.concat(
			$fields,
			"    ${snake}: ${fn_type}, // ${func.name}\n",
		)
	}

	"/// Implement this struct with your hosted function implementations.\n/// Pass it to hostedFunctions() to create the dispatch table.\npub const PlatformHostedFns = struct {\n${$fields}};\n"
}

## Get the Zig function pointer type for a hosted function
hosted_fn_type = |func, type_table| {
	parsed = parse_type_str(func.type_str)
	struct_name = name_to_struct_name(func.name)

	# Use type table to detect record return types
	ret_record = lookup_record_in_type_table(type_table, func.ret_type_id)
	ret_param = if ret_record.found {
		"*${struct_name}RetRecord"
	} else {
		zig_ret = type_id_to_zig(type_table, func.ret_type_id)
		if zig_ret == "void" or zig_ret == "*anyopaque" {
			"*anyopaque"
		} else {
			"*${zig_ret}"
		}
	}

	args_param = if List.is_empty(parsed.args) {
		"*anyopaque"
	} else {
		"*const ${struct_name}Args"
	}

	"*const fn (*RocOps, ${ret_param}, ${args_param}) callconv(.c) void"
}

## Generate the hostedFunctions() helper that builds the dispatch table
generate_hosted_functions_helper = |hosted_functions| {
	var $entries = ""

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		$entries = Str.concat(
			$entries,
			"            hostedFn(fns.${snake}), // ${func.name} (index ${U64.to_str(func.index)})\n",
		)
	}

	"/// Create a HostedFunctions dispatch table from your implementations.\n/// The comptime parameter + nested struct ensures the function pointer array\n/// lives in static memory, not on the stack.\npub fn hostedFunctions(comptime fns: PlatformHostedFns) HostedFunctions {\n    const Static = struct {\n        const ptrs = [_]HostedFn{\n${$entries}        };\n    };\n    return .{\n        .count = Static.ptrs.len,\n        .fns = @constCast(&Static.ptrs),\n    };\n}\n"
}

# =============================================================================
# Host Helper Utilities
# =============================================================================

## Generate DefaultAllocators, DefaultHandlers, and makeRocOps helpers.
## These are static Zig code blocks that don't depend on specific hosted functions.
generate_host_helpers : Str
generate_host_helpers =
	generate_default_allocators
		.concat("\n")
		.concat(generate_default_handlers)
		.concat("\n")
		.concat(generate_make_roc_ops)

## Generate the DefaultAllocators generic function
generate_default_allocators : Str
generate_default_allocators =
	\\/// Default memory management functions for Roc platforms.
	\\///
	\\/// Returns a struct with `rocAlloc`, `rocDealloc`, and `rocRealloc` functions
	\\/// that follow the Roc ABI and use `EnvType.allocator()` for allocation.
	\\///
	\\/// Memory layout: each allocation prepends size metadata so that dealloc/realloc
	\\/// can recover the original allocation size (required because `roc_dealloc` does
	\\/// not receive a length parameter).
	\\///
	\\/// `EnvType` must have an `.allocator()` method that returns `std.mem.Allocator`.
	\\pub fn DefaultAllocators(comptime EnvType: type) type {
	\\    return struct {
	\\        /// Allocate memory for the Roc runtime using the host environment's allocator.
	\\        pub fn rocAlloc(alloc_args: *RocAlloc, env_ptr: *anyopaque) callconv(.c) void {
	\\            const host_env: *EnvType = @ptrCast(@alignCast(env_ptr));
	\\            const allocator = host_env.allocator();
	\\
	\\            const min_alignment: usize = @max(alloc_args.alignment, @alignOf(usize));
	\\            const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
	\\            const size_storage_bytes = min_alignment;
	\\            const total_size = alloc_args.length + size_storage_bytes;
	\\
	\\            const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
	\\                const stderr_file: std.fs.File = .stderr();
	\\                stderr_file.writeAll("roc_alloc: out of memory\\n") catch {};
	\\                std.process.exit(1);
	\\            };
	\\
	\\            // Store total size immediately before the user data pointer
	\\            const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
	\\            size_ptr.* = total_size;
	\\
	\\            alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
	\\        }
	\\
	\\        /// Free memory previously allocated by `rocAlloc`.
	\\        pub fn rocDealloc(dealloc_args: *RocDealloc, env_ptr: *anyopaque) callconv(.c) void {
	\\            const host_env: *EnvType = @ptrCast(@alignCast(env_ptr));
	\\            const allocator = host_env.allocator();
	\\
	\\            const min_alignment: usize = @max(dealloc_args.alignment, @alignOf(usize));
	\\            const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
	\\            const size_storage_bytes = min_alignment;
	\\
	\\            const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
	\\            const total_size = size_ptr.*;
	\\
	\\            const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
	\\            const slice = base_ptr[0..total_size];
	\\            allocator.rawFree(slice, align_enum, @returnAddress());
	\\        }
	\\
	\\        /// Reallocate memory, copying existing data to the new allocation.
	\\        pub fn rocRealloc(realloc_args: *RocRealloc, env_ptr: *anyopaque) callconv(.c) void {
	\\            const host_env: *EnvType = @ptrCast(@alignCast(env_ptr));
	\\            const allocator = host_env.allocator();
	\\
	\\            const min_alignment: usize = @max(realloc_args.alignment, @alignOf(usize));
	\\            const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
	\\            const size_storage_bytes = min_alignment;
	\\
	\\            // Read old size from metadata
	\\            const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
	\\            const old_total_size = old_size_ptr.*;
	\\            const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
	\\
	\\            // Allocate new block
	\\            const new_total_size = realloc_args.new_length + size_storage_bytes;
	\\            const new_base_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
	\\                const stderr_file: std.fs.File = .stderr();
	\\                stderr_file.writeAll("roc_realloc: out of memory\\n") catch {};
	\\                std.process.exit(1);
	\\            };
	\\
	\\            // Copy old user data to new location
	\\            const old_user_data_size = old_total_size - size_storage_bytes;
	\\            const copy_size = @min(old_user_data_size, realloc_args.new_length);
	\\            const new_user_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes);
	\\            const old_user_ptr: [*]const u8 = @ptrCast(realloc_args.answer);
	\\            @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);
	\\
	\\            // Free old allocation
	\\            allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());
	\\
	\\            // Store new size and return user pointer
	\\            const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes - @sizeOf(usize));
	\\            new_size_ptr.* = new_total_size;
	\\            realloc_args.answer = new_user_ptr;
	\\        }
	\\    };
	\\}
	\\

## Generate the DefaultHandlers namespace
generate_default_handlers : Str
generate_default_handlers =
	\\/// Default handlers for dbg, expect-failed, and crash.
	\\///
	\\/// These print to stderr and don't use the env pointer. Suitable for most
	\\/// platform hosts that don't need custom handling of these events.
	\\pub const DefaultHandlers = struct {
	\\    /// Print a `dbg` expression to stderr.
	\\    pub fn rocDbg(dbg_args: *const RocDbg, _: *anyopaque) callconv(.c) void {
	\\        const msg = dbg_args.utf8_bytes[0..dbg_args.len];
	\\        const stderr_file: std.fs.File = .stderr();
	\\        stderr_file.writeAll("\\x1b[36m[ROC DBG]\\x1b[0m ") catch {};
	\\        stderr_file.writeAll(msg) catch {};
	\\        stderr_file.writeAll("\\n") catch {};
	\\    }
	\\
	\\    /// Print a failed `expect` to stderr.
	\\    pub fn rocExpectFailed(expect_args: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
	\\        const msg = expect_args.utf8_bytes[0..expect_args.len];
	\\        const stderr_file: std.fs.File = .stderr();
	\\        stderr_file.writeAll("\\x1b[33m[ROC EXPECT]\\x1b[0m ") catch {};
	\\        stderr_file.writeAll(msg) catch {};
	\\        stderr_file.writeAll("\\n") catch {};
	\\    }
	\\
	\\    /// Print a `crash` message to stderr and exit.
	\\    pub fn rocCrashed(crash_args: *const RocCrashed, _: *anyopaque) callconv(.c) void {
	\\        const msg = crash_args.utf8_bytes[0..crash_args.len];
	\\        const stderr_file: std.fs.File = .stderr();
	\\        stderr_file.writeAll("\\x1b[31m[ROC CRASHED]\\x1b[0m ") catch {};
	\\        stderr_file.writeAll(msg) catch {};
	\\        stderr_file.writeAll("\\n") catch {};
	\\        std.process.exit(1);
	\\    }
	\\};
	\\

## Generate the makeRocOps convenience function
generate_make_roc_ops : Str
generate_make_roc_ops =
	\\/// Create a RocOps struct with default memory management and error handlers.
	\\///
	\\/// This is a convenience function that wires together `DefaultAllocators(EnvType)`,
	\\/// `DefaultHandlers`, and the provided hosted functions into a ready-to-use `RocOps`.
	\\///
	\\/// `EnvType` must have an `.allocator()` method that returns `std.mem.Allocator`.
	\\///
	\\/// Example usage:
	\\/// ```
	\\/// var roc_ops = makeRocOps(HostEnv, &host_env, hostedFunctions(.{ ... }));
	\\/// ```
	\\pub fn makeRocOps(comptime EnvType: type, env: *EnvType, hosted_fns: HostedFunctions) RocOps {
	\\    const Allocs = DefaultAllocators(EnvType);
	\\    return .{
	\\        .env = @ptrCast(env),
	\\        .roc_alloc = &Allocs.rocAlloc,
	\\        .roc_dealloc = &Allocs.rocDealloc,
	\\        .roc_realloc = &Allocs.rocRealloc,
	\\        .roc_dbg = &DefaultHandlers.rocDbg,
	\\        .roc_expect_failed = &DefaultHandlers.rocExpectFailed,
	\\        .roc_crashed = &DefaultHandlers.rocCrashed,
	\\        .hosted_fns = hosted_fns,
	\\    };
	\\}
	\\

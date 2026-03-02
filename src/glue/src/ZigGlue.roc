## A glue script for generating a Zig source file with hosted function bindings.
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

	# Sort by index so array entries are in the correct order
	sorted = List.sort_with($hosted_functions, compare_by_index)

	zig_content = generate_zig_file(sorted)

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
# Zig Code Generation
# =============================================================================

## Generate the complete Zig source file
generate_zig_file : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_zig_file = |hosted_functions| {
	count = List.len(hosted_functions)

	file_header
		.concat(generate_imports)
		.concat("\n")
		.concat(generate_index_constants(hosted_functions, count))
		.concat("\n")
		.concat(generate_all_args_structs(hosted_functions))
		.concat(generate_platform_fns_struct(hosted_functions))
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
	"const std = @import(\"std\");\nconst builtins = @import(\"builtins\");\nconst RocStr = builtins.str.RocStr;\nconst RocList = builtins.list.RocList;\nconst RocOps = builtins.host_abi.RocOps;\nconst HostedFn = builtins.host_abi.HostedFn;\n"

## Generate index constants and count
generate_index_constants : List({ index : U64, name : Str, type_str : Str }), U64 -> Str
generate_index_constants = |hosted_functions, count| {
	var $constants = "pub const hosted_function_count: u32 = ${U64.to_str(count)};\n\n"

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		$constants = Str.concat(
			$constants,
			"pub const hosted_idx_${snake}: u32 = ${U64.to_str(func.index)};\n",
		)
	}

	$constants
}

## Generate all argument extern structs
generate_all_args_structs : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_all_args_structs = |hosted_functions| {
	var $structs = ""
	for func in hosted_functions {
		$structs = Str.concat($structs, generate_args_struct(func))
	}
	$structs
}

## Generate a single argument extern struct (empty string if no args)
generate_args_struct : { index : U64, name : Str, type_str : Str } -> Str
generate_args_struct = |func| {
	parsed = parse_type_str(func.type_str)

	if List.is_empty(parsed.args) {
		return ""
	}

	struct_name = name_to_struct_name(func.name)

	var $fields = ""
	var $idx = 0
	for arg in parsed.args {
		zig_type = roc_type_to_zig(arg)
		$fields = Str.concat(
			$fields,
			"    arg${U64.to_str($idx)}: ${zig_type}, // ${arg}\n",
		)
		$idx = $idx + 1
	}

	doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n"

	"${doc}pub const ${struct_name}Args = extern struct {\n${$fields}};\n\n"
}

## Generate the PlatformHostedFns struct type
generate_platform_fns_struct : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_platform_fns_struct = |hosted_functions| {
	var $fields = ""

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		fn_type = hosted_fn_type(func)

		$fields = Str.concat(
			$fields,
			"    ${snake}: ${fn_type}, // ${func.name}\n",
		)
	}

	"/// Implement this struct with your hosted function implementations.\n/// Pass it to hostedFunctions() to create the dispatch table.\npub const PlatformHostedFns = struct {\n${$fields}};\n"
}

## Get the Zig function pointer type for a hosted function
hosted_fn_type : { index : U64, name : Str, type_str : Str } -> Str
hosted_fn_type = |func| {
	parsed = parse_type_str(func.type_str)
	struct_name = name_to_struct_name(func.name)

	args_param = if List.is_empty(parsed.args) {
		"*anyopaque"
	} else {
		"*const ${struct_name}Args"
	}

	"*const fn (*RocOps, *anyopaque, ${args_param}) callconv(.c) void"
}

## Generate the hostedFunctions() helper that builds the dispatch table
generate_hosted_functions_helper : List({ index : U64, name : Str, type_str : Str }) -> Str
generate_hosted_functions_helper = |hosted_functions| {
	var $entries = ""

	for func in hosted_functions {
		snake = name_to_snake(func.name)
		$entries = Str.concat(
			$entries,
			"            builtins.host_abi.hostedFn(fns.${snake}), // ${func.name} (index ${U64.to_str(func.index)})\n",
		)
	}

	"/// Create a HostedFunctions dispatch table from your implementations.\n/// The comptime parameter + nested struct ensures the function pointer array\n/// lives in static memory, not on the stack.\npub fn hostedFunctions(comptime fns: PlatformHostedFns) builtins.host_abi.HostedFunctions {\n    const Static = struct {\n        const ptrs = [_]HostedFn{\n${$entries}        };\n    };\n    return .{\n        .count = Static.ptrs.len,\n        .fns = @constCast(&Static.ptrs),\n    };\n}\n"
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
	\\        pub fn rocAlloc(alloc_args: *builtins.host_abi.RocAlloc, env_ptr: *anyopaque) callconv(.c) void {
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
	\\        pub fn rocDealloc(dealloc_args: *builtins.host_abi.RocDealloc, env_ptr: *anyopaque) callconv(.c) void {
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
	\\        pub fn rocRealloc(realloc_args: *builtins.host_abi.RocRealloc, env_ptr: *anyopaque) callconv(.c) void {
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
	\\    pub fn rocDbg(dbg_args: *const builtins.host_abi.RocDbg, _: *anyopaque) callconv(.c) void {
	\\        const msg = dbg_args.utf8_bytes[0..dbg_args.len];
	\\        const stderr_file: std.fs.File = .stderr();
	\\        stderr_file.writeAll("\\x1b[36m[ROC DBG]\\x1b[0m ") catch {};
	\\        stderr_file.writeAll(msg) catch {};
	\\        stderr_file.writeAll("\\n") catch {};
	\\    }
	\\
	\\    pub fn rocExpectFailed(expect_args: *const builtins.host_abi.RocExpectFailed, _: *anyopaque) callconv(.c) void {
	\\        const msg = expect_args.utf8_bytes[0..expect_args.len];
	\\        const stderr_file: std.fs.File = .stderr();
	\\        stderr_file.writeAll("\\x1b[33m[ROC EXPECT]\\x1b[0m ") catch {};
	\\        stderr_file.writeAll(msg) catch {};
	\\        stderr_file.writeAll("\\n") catch {};
	\\    }
	\\
	\\    pub fn rocCrashed(crash_args: *const builtins.host_abi.RocCrashed, _: *anyopaque) callconv(.c) void {
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
	\\pub fn makeRocOps(comptime EnvType: type, env: *EnvType, hosted_fns: builtins.host_abi.HostedFunctions) RocOps {
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

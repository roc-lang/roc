## A glue script for generating a Zig source file with hosted function bindings.
app [make_glue] { pf: platform "../platform/main.roc" }

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
	zig_content = generate_zig_file(input.hosted_functions, type_table, input.provides_entries)

	Ok([{ name: "roc_platform_abi.zig", content: zig_content }])
}

# =============================================================================
# TypeRepr-based Type Mapping
# =============================================================================

## Find named multi-variant tag unions whose Roc name appears more than once in
## the type table. The result is computed once per glue run and reused while
## rendering type names.
duplicate_tag_union_names : TypeTable -> List(Str)
duplicate_tag_union_names = |type_table| type_table.duplicate_tag_union_names()

## Return the default Zig type name for a multi-variant tag union.
##
## Generic tag unions used heavily by hosted functions can appear many times in
## the type table with different payload layouts. Zig needs a distinct concrete
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

## Return the emitted Zig type name for a multi-variant tag union.
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

generated_type_names_zig : TypeTable, List(Str) -> List(Str)
generated_type_names_zig = |type_table, duplicate_names| {
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

type_name_root_alias_base_zig : TypeTable, Str, U64 -> Str
type_name_root_alias_base_zig = |type_table, fallback, type_id|
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

record_alias_fields_zig : TypeTable, RecordRepr -> List(RecordField)
record_alias_fields_zig = |type_table, rec| {
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

type_name_roots_zig : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable -> List(TypeNamePlan.Root)
type_name_roots_zig = |hosted_functions, provides_list, type_table| {
	var $roots = []

	for func in hosted_functions {
		base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = $roots.append(
				{
					alias_base: type_name_root_alias_base_zig(type_table, arg_fallback, arg_type_id),
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
							alias_base: type_name_root_alias_base_zig(type_table, arg_fallback, arg_type_id),
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
						alias_base: type_name_root_alias_base_zig(type_table, base, entry.type_id),
						module_base,
						type_id: entry.type_id,
					},
				)
			}
		}
	}

	$roots
}

append_type_alias_roots_zig : List(TypeNamePlan.Root), TypeTable, Str, Str, U64, List(U64) -> List(TypeNamePlan.Root)
append_type_alias_roots_zig = |roots, type_table, alias_base, module_base, type_id, visited_type_ids| {
	if List.contains(visited_type_ids, type_id) {
		return roots
	}

	next_visited = visited_type_ids.append(type_id)
	root_alias_base = type_name_root_alias_base_zig(type_table, alias_base, type_id)

	var $roots = roots.append({ alias_base: root_alias_base, module_base, type_id })

	match type_table.get(type_id) {
		RocRecord(rec) => {
			for field in record_alias_fields_zig(type_table, rec) {
				field_base = "${root_alias_base}${RocName.from_str(field.name).to_pascal_clean()}"
				$roots = append_type_alias_roots_zig($roots, type_table, field_base, module_base, field.type_id, next_visited)
			}
			$roots
		}
		RocList(elem_id) => append_type_alias_roots_zig($roots, type_table, root_alias_base, module_base, elem_id, next_visited)
		RocBox(inner_id) => append_type_alias_roots_zig($roots, type_table, root_alias_base, module_base, inner_id, next_visited)
		RocTagUnion(tu) => {
			var $next = $roots
			for tag in tu.tags {
				child_base = "${root_alias_base}${RocName.capitalize_first(tag.name)}"
				for payload_id in tag.payload {
					$next = append_type_alias_roots_zig($next, type_table, child_base, module_base, payload_id, next_visited)
				}
			}
			$next
		}
		_ => $roots
	}
}

type_alias_roots_zig : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable -> List(TypeNamePlan.Root)
type_alias_roots_zig = |hosted_functions, provides_list, type_table| {
	var $roots = []

	for func in hosted_functions {
		base = name_to_struct_name(func.name)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = append_type_alias_roots_zig($roots, type_table, arg_fallback, module_base, arg_type_id, [])
			$arg_idx = $arg_idx + 1
		}

		$roots = append_type_alias_roots_zig($roots, type_table, base, module_base, func.ret_type_id, [])
	}

	for entry in provides_list {
		base = name_to_struct_name(entry.name)
		module_base = hosted_module_name_to_struct_name(entry.name)

		match type_table.get(entry.type_id) {
			RocFunction(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = append_type_alias_roots_zig($roots, type_table, arg_fallback, module_base, arg_type_id, [])
					$arg_idx = $arg_idx + 1
				}

				$roots = append_type_alias_roots_zig($roots, type_table, base, module_base, func.ret, [])
			}
			_ => {
				$roots = append_type_alias_roots_zig($roots, type_table, base, module_base, entry.type_id, [])
			}
		}
	}

	$roots
}

preferred_type_names_zig : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> TypeNamePlan.PreferredNames
preferred_type_names_zig = |hosted_functions, provides_list, type_table, duplicate_names|
	TypeNamePlan.from_table(type_table).preferred_names(
		generated_type_names_zig(type_table, duplicate_names),
		type_name_roots_zig(hosted_functions, provides_list, type_table),
	)

## Map a type table entry to its Zig type string using structured TypeRepr
type_id_to_zig : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64 -> Str
type_id_to_zig = |type_table, duplicate_tag_names, preferred_names, type_id| {
	type_repr_to_zig(type_table, duplicate_tag_names, preferred_names, type_id, type_table.get(type_id))
}

## Render one `extern struct` field declaration for a record field. Unnamed
## nominal-record padding fields become fixed-size byte arrays (`[size]u8`);
## named fields use their resolved Zig type.
zig_record_field_decl : TypeTable, List(Str), TypeNamePlan.PreferredNames, AbiFieldLayout, AbiWidth -> Str
zig_record_field_decl = |type_table, duplicate_tag_names, preferred_names, field, width| {
	field_name = name_to_zig_quoted_ident(field.name)
	zig_type = if field.is_padding {
		"[${U64.to_str(AbiFieldLayout.size(field, width))}]u8"
	} else {
		type_id_to_zig(type_table, duplicate_tag_names, preferred_names, field.type_id)
	}
	"    ${field_name}: ${zig_type},\n"
}

## Fields arrive in committed layout order (valid at both pointer widths);
## only per-width padding byte counts differ between the two renderings.
zig_record_fields_decl : TypeTable, List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout), AbiWidth -> Str
zig_record_fields_decl = |type_table, duplicate_tag_names, preferred_names, fields, width| {
	var $field_strs = ""
	for field in fields {
		$field_strs = Str.concat($field_strs, zig_record_field_decl(type_table, duplicate_tag_names, preferred_names, field, width))
	}
	$field_strs
}

zig_record_layout_assertions : Str, AbiLayout -> Str
zig_record_layout_assertions = |type_name, abi_layout| {
	if abi_layout.size64 > 0 or abi_layout.size32 > 0 {
		block =
			\\comptime {
			\\    if (@sizeOf(usize) == 8) {
			\\        if (@sizeOf(${type_name}) != ${U64.to_str(abi_layout.size64)}) @compileError("${type_name} size mismatch");
			\\        if (@alignOf(${type_name}) != ${U64.to_str(abi_layout.alignment64)}) @compileError("${type_name} alignment mismatch");
			\\    }
			\\    if (@sizeOf(usize) == 4) {
			\\        if (@sizeOf(${type_name}) != ${U64.to_str(abi_layout.size32)}) @compileError("${type_name} size mismatch");
			\\        if (@alignOf(${type_name}) != ${U64.to_str(abi_layout.alignment32)}) @compileError("${type_name} alignment mismatch");
			\\    }
			\\}
		"${block}\n\n"
	} else {
		""
	}
}

zig_payload_layout_assertions : Str, AbiTagLayout -> Str
zig_payload_layout_assertions = |type_name, tag_layout| {
	if tag_layout.payload_size64 > 0 or tag_layout.payload_size32 > 0 {
		block =
			\\comptime {
			\\    if (@sizeOf(usize) == 8) {
			\\        if (@sizeOf(${type_name}) != ${U64.to_str(tag_layout.payload_size64)}) @compileError("${type_name} size mismatch");
			\\        if (@alignOf(${type_name}) != ${U64.to_str(tag_layout.payload_alignment64)}) @compileError("${type_name} alignment mismatch");
			\\    }
			\\    if (@sizeOf(usize) == 4) {
			\\        if (@sizeOf(${type_name}) != ${U64.to_str(tag_layout.payload_size32)}) @compileError("${type_name} size mismatch");
			\\        if (@alignOf(${type_name}) != ${U64.to_str(tag_layout.payload_alignment32)}) @compileError("${type_name} alignment mismatch");
			\\    }
			\\}
		"${block}\n\n"
	} else {
		""
	}
}

zig_record_struct_decl : Str, Str, Str, Str, Str, Str, AbiLayout -> Str
zig_record_struct_decl = |doc, struct_name, field_strs64, field_strs32, method_decls64, method_decls32, abi_layout| {
	assertions = zig_record_layout_assertions(struct_name, abi_layout)
	decl =
		\\pub const ${struct_name} = if (@sizeOf(usize) == 4) extern struct {
		\\${field_strs32}${method_decls32}} else extern struct {
		\\${field_strs64}${method_decls64}};
	"${doc}${decl}\n\n${assertions}"
}

zig_payload_struct_decl : Str, Str, TypeTable, List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout), AbiTagLayout -> Str
zig_payload_struct_decl = |doc, struct_name, type_table, duplicate_tag_names, preferred_names, fields, tag_layout| {
	field_strs64 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, fields, Pointer64)
	field_strs32 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, fields, Pointer32)
	assertions = zig_payload_layout_assertions(struct_name, tag_layout)
	decl =
		\\pub const ${struct_name} = if (@sizeOf(usize) == 4) extern struct {
		\\${field_strs32}} else extern struct {
		\\${field_strs64}};
	"${doc}${decl}\n\n${assertions}"
}

## Convert a TypeRepr to its Zig type string
type_repr_to_zig : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr -> Str
type_repr_to_zig = |type_table, duplicate_tag_names, preferred_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "RocErasedCallable"
				RocUnknown(_) => "RocBox"
				_ => {
					inner_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, inner_id)
					if inner_zig == "*anyopaque" {
						"*anyopaque"
					} else {
						"*${inner_zig}"
					}
				}
			}
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
		RocDec => "RocDec"
		RocList(elem_id) =>
			if is_type_refcounted(type_table, elem_id) {
				"RocList(${type_id_to_zig(type_table, duplicate_tag_names, preferred_names, elem_id)})"
			} else {
				"RocListWith(${type_id_to_zig(type_table, duplicate_tag_names, preferred_names, elem_id)}, false)"
			}
		RocRecord(rec) =>
			if rec.name == "" {
				"*anyopaque"
			} else {
				name_to_struct_name(rec.name)
			}
		RocTagUnion(tu) => resolve_tag_union_type(type_table, duplicate_tag_names, preferred_names, type_id, tu)
		RocFunction(_) => "*anyopaque"
		RocUnknown(_) => "*anyopaque"
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

## Resolve a tag union to a Zig type. Single-variant unions are unwrapped to their payload.
## Multi-variant unions with a name return a generated struct name.
resolve_tag_union_type : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr -> Str
resolve_tag_union_type = |type_table, duplicate_tag_names, preferred_names, type_id, tu|
	match TypeTable.single_variant_payload(tu) {
		SinglePayload(payload_id) => type_id_to_zig(type_table, duplicate_tag_names, preferred_names, payload_id)
		SingleNoPayload => "*anyopaque"
		NotSingleVariant =>
			if tu.name != "" {
				tag_union_struct_name(preferred_names, duplicate_tag_names, type_id, tu)
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
	\\/// Parameterized list constructor; use `RocList(T)` for refcounted elements.
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
	\\        /// Return every initialized element in the backing allocation.
	\\        pub fn allocationItems(self: Self) []const T {
	\\            const alloc_ptr = self.getAllocationPtr() orelse return &[_]T{};
	\\            const count = self.allocationElementCount();
	\\            const ptr: [*]const T = @ptrCast(@alignCast(alloc_ptr));
	\\            return ptr[0..count];
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
	\\        /// Return true if this list is a seamless slice into another allocation.
	\\        /// Slices share the rc slot with their backing allocation; the alloc ptr is
	\\        /// encoded in `capacity_or_alloc_ptr` with the low bit set.
	\\        pub fn isSeamlessSlice(self: Self) bool {
	\\            return (self.capacity_or_alloc_ptr & 1) != 0;
	\\        }
	\\
	\\        /// Return an empty RocList.
	\\        pub fn empty() Self {
	\\            return .{ .elements_ptr = null, .length = 0, .capacity_or_alloc_ptr = 0 };
	\\        }
	\\
	\\        /// Resolve `self` to the start of its backing allocation (the element block
	\\        /// just after the rc slot). Returns `null` for empty lists. Handles both
	\\        /// whole-backing and seamless-slice forms.
	\\        fn getAllocationPtr(self: Self) ?[*]u8 {
	\\            if (self.isSeamlessSlice()) {
	\\                return @as(?[*]u8, @ptrFromInt(self.capacity_or_alloc_ptr & ~@as(usize, 1)));
	\\            }
	\\            const ptr = self.elements_ptr orelse return null;
	\\            return @ptrCast(ptr);
	\\        }
	\\
	\\        fn allocationElementCount(self: Self) usize {
	\\            if (self.isSeamlessSlice() and elements_refcounted) {
	\\                const alloc_ptr = self.getAllocationPtr() orelse return 0;
	\\                const ptr: [*]const usize = @ptrCast(@alignCast(alloc_ptr));
	\\                return (ptr - 2)[0];
	\\            }
	\\            return self.length;
	\\        }
	\\
	\\        /// Allocate a new list with space for `length` elements.
	\\        pub fn allocate(length: usize, roc_host: *RocHost) Self {
	\\            if (length == 0) return empty();
	\\            const data_bytes = length * @sizeOf(T);
	\\            const total = data_bytes + header_bytes;
	\\            const base: [*]u8 = @ptrCast(roc_host.roc_alloc(roc_host, total, alloc_align));
	\\            const data_ptr = base + header_bytes;
	\\            const rc: *isize = @ptrFromInt(@intFromPtr(data_ptr) - @sizeOf(isize));
	\\            rc.* = 1;
	\\            if (elements_refcounted) {
	\\                const count: *usize = @ptrFromInt(@intFromPtr(data_ptr) - (2 * @sizeOf(usize)));
	\\                count.* = length;
	\\            }
	\\            return .{
	\\                .elements_ptr = @ptrCast(@alignCast(data_ptr)),
	\\                .length = length,
	\\                .capacity_or_alloc_ptr = length << 1,
	\\            };
	\\        }
	\\
	\\        /// Create a RocList from a slice, copying elements into a new allocation.
	\\        pub fn fromSlice(slice: []const T, roc_host: *RocHost) Self {
	\\            if (slice.len == 0) return empty();
	\\            const list = allocate(slice.len, roc_host);
	\\            const dest: [*]u8 = @ptrCast(list.elements_ptr.?);
	\\            const src: [*]const u8 = @ptrCast(slice.ptr);
	\\            @memcpy(dest[0 .. slice.len * @sizeOf(T)], src[0 .. slice.len * @sizeOf(T)]);
	\\            return list;
	\\        }
	\\
	\\        /// Decrement the reference count; frees the allocation when it reaches zero.
	\\        pub fn decref(self: Self, roc_host: *RocHost) void {
	\\            const alloc_ptr = self.getAllocationPtr() orelse return;
	\\            const data_addr = @intFromPtr(alloc_ptr);
	\\            const rc: *isize = @ptrFromInt(data_addr - @sizeOf(isize));
	\\            if (rc.* == 0) return; // REFCOUNT_STATIC_DATA — bytes are in read-only memory
	\\            const prev = @atomicRmw(isize, rc, .Sub, 1, .monotonic);
	\\            if (prev == 1) {
	\\                const base: *anyopaque = @ptrFromInt(data_addr - header_bytes);
	\\                roc_host.roc_dealloc(roc_host, base, alloc_align);
	\\            }
	\\        }
	\\
	\\        /// Increment the reference count by `amount`.
	\\        pub fn incref(self: Self, amount: isize) void {
	\\            const alloc_ptr = self.getAllocationPtr() orelse return;
	\\            const rc: *isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\            if (rc.* == 0) return; // REFCOUNT_STATIC_DATA
	\\            _ = @atomicRmw(isize, rc, .Add, amount, .monotonic);
	\\        }
	\\
	\\        /// Return true if this list has a reference count of exactly one.
	\\        pub fn isUnique(self: Self) bool {
	\\            const alloc_ptr = self.getAllocationPtr() orelse return true;
	\\            const rc: *const isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\            if (rc.* == 0) return true; // REFCOUNT_STATIC_DATA — treated as unique
	\\            return rc.* == 1;
	\\        }
	\\
	\\        /// Return true if this list's allocation has exactly one counted ref.
	\\        pub fn hasOneRef(self: Self) bool {
	\\            const alloc_ptr = self.getAllocationPtr() orelse return false;
	\\            const rc: *const isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\            return rc.* == 1;
	\\        }
	\\    };
	\\}
	\\

## Generate the RocIo vtable-based I/O abstraction
generate_roc_io : Str
generate_roc_io =
	\\/// Minimal I/O abstraction for Roc platform hosts.
	\\///
	\\/// Provides stderr output and fatal-error handling that can be routed
	\\/// to different backends (POSIX, freestanding/WASM, custom).
	\\///
	\\/// Use `RocIo.default()` for most platforms; inject a custom vtable for
	\\/// environments without POSIX stdio (e.g. WASM freestanding).
	\\pub const RocIo = struct {
	\\    ctx: ?*anyopaque,
	\\    vtable: *const VTable,
	\\
	\\    pub const VTable = struct {
	\\        /// Write to stderr. Best-effort: errors are silently ignored.
	\\        writeStderr: *const fn (?*anyopaque, []const u8) void,
	\\        /// Terminate the process or trap. Must not return.
	\\        onFatal: *const fn (?*anyopaque) noreturn,
	\\    };
	\\
	\\    /// Write `data` to stderr (best-effort).
	\\    pub fn writeStderr(self: RocIo, data: []const u8) void {
	\\        self.vtable.writeStderr(self.ctx, data);
	\\    }
	\\
	\\    /// Terminate the process or trap. Does not return.
	\\    pub fn onFatal(self: RocIo) noreturn {
	\\        self.vtable.onFatal(self.ctx);
	\\    }
	\\
	\\    /// Native (POSIX) implementation: writes to stderr, exits with code 1.
	\\    pub fn native() RocIo {
	\\        return .{ .ctx = null, .vtable = &native_vtable };
	\\    }
	\\
	\\    /// Freestanding implementation: no-op stderr, @trap() on fatal.
	\\    pub fn freestanding() RocIo {
	\\        return .{ .ctx = null, .vtable = &freestanding_vtable };
	\\    }
	\\
	\\    /// Select the appropriate implementation for the current build target.
	\\    pub fn default() RocIo {
	\\        if (comptime builtin.os.tag == .freestanding) {
	\\            return freestanding();
	\\        }
	\\        return native();
	\\    }
	\\
	\\    fn nativeWriteStderr(_: ?*anyopaque, data: []const u8) void {
	\\        std.Io.File.stderr().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), data) catch {};
	\\    }
	\\
	\\    fn nativeOnFatal(_: ?*anyopaque) noreturn {
	\\        std.process.exit(1);
	\\    }
	\\
	\\    fn freestandingWriteStderr(_: ?*anyopaque, _: []const u8) void {}
	\\
	\\    fn freestandingOnFatal(_: ?*anyopaque) noreturn {
	\\        @trap();
	\\    }
	\\
	\\    const native_vtable: VTable = .{
	\\        .writeStderr = &nativeWriteStderr,
	\\        .onFatal = &nativeOnFatal,
	\\    };
	\\
	\\    const freestanding_vtable: VTable = .{
	\\        .writeStderr = &freestandingWriteStderr,
	\\        .onFatal = &freestandingOnFatal,
	\\    };
	\\};
	\\

## Generate the RocEnv host environment struct
generate_roc_env : Str
generate_roc_env =
	\\/// Host environment passed to the Roc runtime.
	\\///
	\\/// Bundles the allocator and I/O backend used by DefaultAllocators
	\\/// and DefaultHandlers.
	\\pub const RocEnv = struct {
	\\    allocator: std.mem.Allocator,
	\\    roc_io: RocIo,
	\\};
	\\

## Generate extern structs for element types found in the type table.
## Scans for Record types and generates Zig extern structs for them.
generate_element_type_structs : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_element_type_structs = |type_table, duplicate_tag_names, preferred_names| {
	var $structs = ""
	var $seen_names = []

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" {
					struct_name = name_to_struct_name(rec.name)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)

						abi_fields = abi_record_fields(type_info.layout)
						field_strs64 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, abi_fields, Pointer64)
						field_strs32 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, abi_fields, Pointer32)
						method_decls32 = generate_record_refcount_methods(type_table, duplicate_tag_names, preferred_names, abi_fields)
						method_decls64 = generate_record_refcount_methods(type_table, duplicate_tag_names, preferred_names, abi_fields)

						$structs = Str.concat(
							$structs,
							zig_record_struct_decl("/// Element type for ${rec.name}\n", struct_name, field_strs64, field_strs32, method_decls64, method_decls32, type_info.layout),
						)
					}
				}
			RocBox(_) => {}
			RocTagUnion(_) => {}
			RocBool => {}
			RocDec => {}
			RocF32 => {}
			RocF64 => {}
			RocFunction(_) => {}
			RocI128 => {}
			RocI16 => {}
			RocI32 => {}
			RocI64 => {}
			RocI8 => {}
			RocList(_) => {}
			RocStr => {}
			RocU128 => {}
			RocU16 => {}
			RocU32 => {}
			RocU64 => {}
			RocU8 => {}
			RocUnit => {}
			RocUnknown(_) => {}
		}

	}

	$structs
}

## Generate extern structs for tag union types found in the type table.
## Multi-variant tag unions get a tag enum, payload extern union, and wrapping extern struct.
## Pure enums (all variants have no payload) get just an enum.
generate_tag_union_structs : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_tag_union_structs = |type_table, duplicate_tag_names, preferred_names| {
	var $structs = ""
	var $seen_names = []
	var $type_id = 0

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" {
					struct_name = tag_union_struct_name(preferred_names, duplicate_tag_names, $type_id, tu)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$structs = Str.concat($structs, generate_single_tag_union(type_table, duplicate_tag_names, preferred_names, $type_id, tu, type_info.layout))
					}
				}
			RocBox(_) => {}
			RocRecord(_) => {}
			RocBool => {}
			RocDec => {}
			RocF32 => {}
			RocF64 => {}
			RocFunction(_) => {}
			RocI128 => {}
			RocI16 => {}
			RocI32 => {}
			RocI64 => {}
			RocI8 => {}
			RocList(_) => {}
			RocStr => {}
			RocU128 => {}
			RocU16 => {}
			RocU32 => {}
			RocU64 => {}
			RocU8 => {}
			RocUnit => {}
			RocUnknown(_) => {}
		}

		$type_id = $type_id + 1
	}

	$structs
}

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

## Generate Zig code for a single multi-variant tag union.
generate_single_tag_union : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr, AbiLayout -> Str
generate_single_tag_union = |type_table, duplicate_tag_names, preferred_names, type_id, tu, abi_layout| {
	struct_name = tag_union_struct_name(preferred_names, duplicate_tag_names, type_id, tu)
	disc_type = disc_type_for_size(abi_discriminant_size(abi_layout))
	abi_tags = abi_tag_layouts(abi_layout)

	# Check if this is a pure enum (all variants have no payload)
	is_pure_enum = List.all(abi_tags, |tag| !(abi_tag_has_payload(tag)))

	if is_pure_enum {
		# Pure enum: just emit the enum type
		method_decls = "    /// Recursively decrement Roc-owned payloads.\n    pub fn decref(self: @This(), roc_host: *RocHost) void {\n        _ = self;\n        _ = roc_host;\n    }\n\n    /// Increment Roc-owned payloads.\n    pub fn incref(self: @This(), amount: isize) void {\n        _ = self;\n        _ = amount;\n    }\n"
		var $variants = ""
		var $idx = 0
		for tag in tu.tags {
			snake = to_lower_snake_case(tag.name)
			$variants = Str.concat($variants, "    ${snake} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		assertions = zig_record_layout_assertions(struct_name, abi_layout)
		"/// Tag union: ${tu.name}\npub const ${struct_name} = enum(${disc_type}) {\n${$variants}${method_decls}};\n\n${assertions}"
	} else {
		# Generate tuple structs for any variant with >1 payload
		var $tuple_structs = ""
		var $tag_idx = 0
		for tag in tu.tags {
			tag_layout = abi_tag_at(abi_tags, $tag_idx)
			if abi_tag_has_payload(tag_layout) and List.len(tag.payload) > 1 {
				tuple_name = "${struct_name}${capitalize_first(tag.name)}Payload"
				tuple_doc = "/// Payload struct for ${tag.name} variant.\n"
				$tuple_structs = Str.concat($tuple_structs, zig_payload_struct_decl(tuple_doc, tuple_name, type_table, duplicate_tag_names, preferred_names, tag_layout.payload_fields, tag_layout))
			}
			$tag_idx = $tag_idx + 1
		}

		# Tag enum
		var $enum_variants = ""
		var $idx = 0
		for enum_tag in tu.tags {
			$enum_variants = Str.concat($enum_variants, "    ${enum_tag.name} = ${U64.to_str($idx)},\n")
			$idx = $idx + 1
		}

		payload_union_name = "${struct_name}Payload"

		# Payload extern union
		var $union_fields = ""
		var $accessors64 = ""
		var $accessors32 = ""
		var $union_tag_idx = 0
		for union_tag in tu.tags {
			tag_layout = abi_tag_at(abi_tags, $union_tag_idx)
			snake = to_lower_snake_case(union_tag.name)
			if !(abi_tag_has_payload(tag_layout)) {
				# No-payload variant: use [0]u8 (Zig extern unions can't have void)
				$union_fields = Str.concat($union_fields, "        ${snake}: [0]u8,\n")
				} else if List.len(union_tag.payload) == 1 {
					zig_type = match List.first(union_tag.payload) {
						Ok(pid) => type_id_to_zig(type_table, duplicate_tag_names, preferred_names, pid)
						Err(_) => {
							crash "glue invariant violated: single-payload tag had no payload"
						}
					}
				$union_fields = Str.concat($union_fields, "        ${snake}: ${zig_type},\n")
				$accessors64 = Str.concat($accessors64, "    pub fn payload_${snake}(self: *const @This()) ${zig_type} {\n        return self.payload.${snake};\n    }\n")
				$accessors32 = Str.concat($accessors32, "    pub fn payload_${snake}(self: *const @This()) ${zig_type} {\n        const ptr: *const ${zig_type} = @ptrCast(@alignCast(&self.payload));\n        return ptr.*;\n    }\n")
			} else {
				tuple_name = "${struct_name}${capitalize_first(union_tag.name)}Payload"
				$union_fields = Str.concat($union_fields, "        ${snake}: ${tuple_name},\n")
				$accessors64 = Str.concat($accessors64, "    pub fn payload_${snake}(self: *const @This()) ${tuple_name} {\n        return self.payload.${snake};\n    }\n")
				$accessors32 = Str.concat($accessors32, "    pub fn payload_${snake}(self: *const @This()) ${tuple_name} {\n        const ptr: *const ${tuple_name} = @ptrCast(@alignCast(&self.payload));\n        return ptr.*;\n    }\n")
			}
			$union_tag_idx = $union_tag_idx + 1
		}

		# Comptime assertions
		method_decls32 = generate_tag_union_refcount_method_delegates(struct_name)
		method_decls64 = generate_tag_union_refcount_method_delegates(struct_name)
		assertions = if abi_layout.size64 > 0 or abi_layout.size32 > 0 {
			block =
				\\comptime {
				\\    if (@sizeOf(usize) == 8) {
				\\        if (@sizeOf(${struct_name}) != ${U64.to_str(abi_layout.size64)}) @compileError("${struct_name} size mismatch");
				\\        if (@alignOf(${struct_name}) != ${U64.to_str(abi_layout.alignment64)}) @compileError("${struct_name} alignment mismatch");
				\\        if (@offsetOf(${struct_name}, "tag") != ${U64.to_str(abi_discriminant_offset(abi_layout, Pointer64))}) @compileError("${struct_name} tag offset mismatch");
				\\    }
				\\    if (@sizeOf(usize) == 4) {
				\\        if (@sizeOf(${struct_name}) != ${U64.to_str(abi_layout.size32)}) @compileError("${struct_name} size mismatch");
				\\        if (@alignOf(${struct_name}) != ${U64.to_str(abi_layout.alignment32)}) @compileError("${struct_name} alignment mismatch");
				\\        if (@offsetOf(${struct_name}, "tag") != ${U64.to_str(abi_discriminant_offset(abi_layout, Pointer32))}) @compileError("${struct_name} tag offset mismatch");
				\\    }
				\\}
			"${block}\n\n"
		} else {
			""
		}

		decl =
			\\/// Tag discriminant for ${tu.name}.
			\\pub const ${struct_name}Tag = enum(${disc_type}) {
			\\${$enum_variants}};
			\\
			\\/// Payload union for ${tu.name}.
			\\pub const ${payload_union_name} = extern union {
			\\${$union_fields}};
			\\
			\\/// Tag union: ${tu.name}
			\\pub const ${struct_name} = if (@sizeOf(usize) == 4) extern struct {
			\\    payload: [${U64.to_str(abi_discriminant_offset(abi_layout, Pointer32))}]u8 align(${U64.to_str(abi_layout.alignment32)}),
			\\    tag: ${struct_name}Tag,
			\\${$accessors32}${method_decls32}} else extern struct {
			\\    payload: ${payload_union_name},
			\\    tag: ${struct_name}Tag,
			\\${$accessors64}${method_decls64}};
		"${$tuple_structs}${decl}\n\n${assertions}"
	}
}

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

box_payload_decref_name : U64 -> Str
box_payload_decref_name = |inner_id| "decrefBoxPayloadType${U64.to_str(inner_id)}"

decref_stmt_for_type_id : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, Str -> Str
decref_stmt_for_type_id = |type_table, duplicate_tag_names, preferred_names, type_id, expr| {
	type_repr = type_table.get(type_id)
	decref_stmt_for_repr(type_table, duplicate_tag_names, preferred_names, type_id, type_repr, expr)
}

decref_stmt_for_repr : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr, Str -> Str
decref_stmt_for_repr = |type_table, duplicate_tag_names, preferred_names, _type_id, type_repr, expr| {
	match type_repr {
		RocStr => "    ${expr}.decref(roc_host);\n"
		RocList(elem_id) => {
			if is_type_refcounted(type_table, elem_id) {
				elem_stmt = decref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, elem_id, "item")
				if elem_stmt == "" {
					"    comptime { @compileError(\"missing decref helper for refcounted list element type id ${U64.to_str(elem_id)}\"); }\n"
				} else {
					"    {\n        const list = ${expr};\n        if (list.hasOneRef()) {\n            for (list.allocationItems()) |item| {\n${indent_lines(elem_stmt, "                ")}            }\n        }\n        list.decref(roc_host);\n    }\n"
				}
			} else {
				"    ${expr}.decref(roc_host);\n"
			}
		}
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "    decrefErasedCallable(${expr}, roc_host);\n"
				_ => {
					inner_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, inner_id)
					if inner_zig == "*anyopaque" {
						"    decrefBox(@ptrCast(${expr}), roc_host);\n"
					} else if is_type_refcounted(type_table, inner_id) {
						"    decrefBoxWith(@ptrCast(${expr}), @alignOf(${inner_zig}), true, &${box_payload_decref_name(inner_id)}, roc_host);\n"
					} else {
						"    decrefBoxWith(@ptrCast(${expr}), @alignOf(${inner_zig}), false, null, roc_host);\n"
					}
				}
			}
		RocRecord(rec) => {
			if rec.name == "" {
				""
			} else {
				"    ${expr}.decref(roc_host);\n"
			}
		}
		RocTagUnion(tu) =>
			match TypeTable.single_variant_payload(tu) {
				SinglePayload(payload_id) => decref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, payload_id, expr)
				SingleNoPayload => ""
				NotSingleVariant => {
					if tu.name != "" {
						"    ${expr}.decref(roc_host);\n"
					} else {
						""
					}
				}
			}
		_ => ""
	}
}

incref_stmt_for_type_id : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, Str -> Str
incref_stmt_for_type_id = |type_table, duplicate_tag_names, preferred_names, type_id, expr| {
	type_repr = type_table.get(type_id)
	incref_stmt_for_repr(type_table, duplicate_tag_names, preferred_names, type_id, type_repr, expr)
}

incref_stmt_for_repr : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr, Str -> Str
incref_stmt_for_repr = |type_table, duplicate_tag_names, preferred_names, _type_id, type_repr, expr| {
	match type_repr {
		RocStr => "    ${expr}.incref(amount);\n"
		RocList(_) => "    ${expr}.incref(amount);\n"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocFunction(_) => "    increfErasedCallable(${expr}, amount);\n"
				_ => "    increfBox(@ptrCast(${expr}), amount);\n"
			}
		RocRecord(rec) => {
			if rec.name == "" {
				""
			} else {
				"    ${expr}.incref(amount);\n"
			}
		}
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) => incref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, payload_id, expr)
					SingleNoPayload => ""
					NotSingleVariant =>
						if tu.name != "" {
							"    ${expr}.incref(amount);\n"
						} else {
							""
						}
				}
			_ => ""
		}
	}

generate_record_refcount_methods : TypeTable, List(Str), TypeNamePlan.PreferredNames, List(AbiFieldLayout) -> Str
generate_record_refcount_methods = |type_table, duplicate_tag_names, preferred_names, fields| {
	var $decref_body = ""
	var $incref_body = ""

	for field in fields {
		# Padding fields are raw bytes with no Roc type, so they are never
		# refcounted and contribute no incref/decref statements.
		if !field.is_padding {
			field_expr = "value.${name_to_zig_quoted_ident(field.name)}"
			$decref_body = Str.concat($decref_body, decref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, field.type_id, field_expr))
			$incref_body = Str.concat($incref_body, incref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, field.type_id, field_expr))
		}
	}

	if $decref_body == "" {
		$decref_body = "        _ = value;\n        _ = roc_host;\n"
	} else {
		$decref_body = indent_lines($decref_body, "    ")
	}
	if $incref_body == "" {
		$incref_body = "        _ = value;\n        _ = amount;\n"
	} else {
		$incref_body = indent_lines($incref_body, "    ")
	}

	"    /// Recursively decrement Roc-owned fields.\n    pub fn decref(self: @This(), roc_host: *RocHost) void {\n        const value = self;\n${$decref_body}    }\n\n    /// Increment Roc-owned fields.\n    pub fn incref(self: @This(), amount: isize) void {\n        const value = self;\n${$incref_body}    }\n"
}

generate_tag_payload_refcount_branch : TypeTable, List(Str), TypeNamePlan.PreferredNames, TagVariant, Str -> Str
generate_tag_payload_refcount_branch = |type_table, duplicate_tag_names, preferred_names, tag, mode| {
	snake = to_lower_snake_case(tag.name)

	if List.is_empty(tag.payload) {
		return "        .${tag.name} => {},\n"
	}

	if List.len(tag.payload) == 1 {
		body =
			match List.first(tag.payload) {
				Ok(payload_id) =>
					if mode == "decref" {
						decref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, payload_id, "value.payload_${snake}()")
						} else {
							incref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, payload_id, "value.payload_${snake}()")
						}
					Err(_) => {
						crash "glue invariant violated: single-payload tag had no payload"
					}
				}

		if body == "" {
			"        .${tag.name} => {},\n"
		} else {
			"        .${tag.name} => {\n${indent_lines(body, "    ")}        },\n"
		}
	} else {
		var $body = "        const payload = value.payload_${snake}();\n"
		var $idx = 0
		for payload_id in tag.payload {
			field_expr = "payload._${U64.to_str($idx)}"
			stmt = if mode == "decref" {
				decref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, payload_id, field_expr)
			} else {
				incref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, payload_id, field_expr)
			}
			$body = Str.concat($body, indent_lines(stmt, "    "))
			$idx = $idx + 1
		}

		"        .${tag.name} => {\n${$body}        },\n"
	}
}

decref_stmt_uses_roc_host_for_type_id : TypeTable, U64 -> Bool
decref_stmt_uses_roc_host_for_type_id = |type_table, type_id| {
	type_repr = type_table.get(type_id)
	match type_repr {
		RocStr => Bool.True
		RocList(_) => Bool.True
		RocBox(_) => Bool.True
		RocRecord(rec) => rec.name != ""
		RocTagUnion(tu) =>
			match TypeTable.single_variant_payload(tu) {
				SinglePayload(payload_id) => decref_stmt_uses_roc_host_for_type_id(type_table, payload_id)
				SingleNoPayload => Bool.False
				NotSingleVariant => tu.name != ""
			}
		_ => Bool.False
	}
}

incref_stmt_uses_amount_for_type_id : TypeTable, U64 -> Bool
incref_stmt_uses_amount_for_type_id = |type_table, type_id| {
	type_repr = type_table.get(type_id)
	match type_repr {
		RocStr => Bool.True
		RocList(_) => Bool.True
		RocBox(_) => Bool.True
			RocRecord(rec) => rec.name != ""
			RocTagUnion(tu) =>
				match TypeTable.single_variant_payload(tu) {
					SinglePayload(payload_id) => incref_stmt_uses_amount_for_type_id(type_table, payload_id)
					SingleNoPayload => Bool.False
					NotSingleVariant => tu.name != ""
				}
			_ => Bool.False
		}
	}

tag_payload_refcount_uses_param : TypeTable, TagVariant, Str -> Bool
tag_payload_refcount_uses_param = |type_table, tag, mode| {
	if List.is_empty(tag.payload) {
		return Bool.False
	}

	List.any(
		tag.payload,
		|payload_id| {
			if mode == "decref" {
				decref_stmt_uses_roc_host_for_type_id(type_table, payload_id)
			} else {
				incref_stmt_uses_amount_for_type_id(type_table, payload_id)
			}
		},
	)
}

tag_union_refcount_uses_param : TypeTable, TagUnionRepr, Str -> Bool
tag_union_refcount_uses_param = |type_table, tu, mode| {
	List.any(tu.tags, |tag| tag_payload_refcount_uses_param(type_table, tag, mode))
}

generate_tag_union_refcount_method_delegates : Str -> Str
generate_tag_union_refcount_method_delegates = |struct_name| {
	"    /// Recursively decrement Roc-owned payloads.\n    pub fn decref(self: @This(), roc_host: *RocHost) void {\n        decref${struct_name}(self, roc_host);\n    }\n\n    /// Increment Roc-owned payloads.\n    pub fn incref(self: @This(), amount: isize) void {\n        incref${struct_name}(self, amount);\n    }\n"
}

generate_tag_union_refcount_helpers : TypeTable, List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr -> Str
generate_tag_union_refcount_helpers = |type_table, duplicate_tag_names, preferred_names, type_id, tu| {
	struct_name = tag_union_struct_name(preferred_names, duplicate_tag_names, type_id, tu)
	var $decref_branches = ""
	var $incref_branches = ""
	for tag in tu.tags {
		$decref_branches = Str.concat($decref_branches, generate_tag_payload_refcount_branch(type_table, duplicate_tag_names, preferred_names, tag, "decref"))
		$incref_branches = Str.concat($incref_branches, generate_tag_payload_refcount_branch(type_table, duplicate_tag_names, preferred_names, tag, "incref"))
	}

	decref_uses_param = tag_union_refcount_uses_param(type_table, tu, "decref")
	incref_uses_param = tag_union_refcount_uses_param(type_table, tu, "incref")
	decref_unused = if decref_uses_param {
		""
	} else {
		"    _ = roc_host;\n"
	}
	incref_unused = if incref_uses_param {
		""
	} else {
		"    _ = amount;\n"
	}

	"fn decref${struct_name}(value: ${struct_name}, roc_host: *RocHost) void {\n${decref_unused}    switch (value.tag) {\n${$decref_branches}    }\n}\n\nfn incref${struct_name}(value: ${struct_name}, amount: isize) void {\n${incref_unused}    switch (value.tag) {\n${$incref_branches}    }\n}\n\n"
}

generate_box_payload_decref_helpers : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_box_payload_decref_helpers = |type_table, duplicate_tag_names, preferred_names| {
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
							inner_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, inner_id)
							if inner_zig != "*anyopaque" and is_type_refcounted(type_table, inner_id) {
								stmt = decref_stmt_for_type_id(type_table, duplicate_tag_names, preferred_names, inner_id, "payload.*")
								$helpers = Str.concat(
									$helpers,
									"fn ${box_payload_decref_name(inner_id)}(data_ptr: ?*anyopaque, roc_host: *RocHost) callconv(.c) void {\n    const payload: *${inner_zig} = @ptrCast(@alignCast(data_ptr orelse return));\n${stmt}}\n\n",
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

generate_refcount_helpers : TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_refcount_helpers = |type_table, duplicate_tag_names, preferred_names| {
	var $helpers = ""
	var $seen_names = []
	var $type_id = 0

	for type_info in type_table_entries(type_table) {
		match type_info.repr {
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" and !List.all(tu.tags, |tag| List.is_empty(tag.payload)) {
					struct_name = tag_union_struct_name(preferred_names, duplicate_tag_names, $type_id, tu)
					if !(List.contains($seen_names, struct_name)) {
						$seen_names = $seen_names.append(struct_name)
						$helpers = Str.concat($helpers, generate_tag_union_refcount_helpers(type_table, duplicate_tag_names, preferred_names, $type_id, tu))
					}
				}
			_ => {}
		}

		$type_id = $type_id + 1
	}

	box_helpers = generate_box_payload_decref_helpers(type_table, duplicate_tag_names, preferred_names)
	all_helpers = Str.concat($helpers, box_helpers)
	if all_helpers == "" {
		""
	} else {
		"// Generated Refcount Helpers\n\n${all_helpers}"
	}
}

## ZigGlue must keep distinct concrete types for generic Roc types such as
## `Try` because different hosted functions can use different payload layouts.
## These aliases give platform authors stable API names for secondary names that
## share a concrete layout.
add_type_alias_zig : { content : Str, seen : List(Str) }, Str, Str -> { content : Str, seen : List(Str) }
add_type_alias_zig = |state, alias, target| {
	if alias == target or List.contains(state.seen, alias) {
		state
	} else {
		{
			content: Str.concat(state.content, "pub const ${alias} = ${target};\n"),
			seen: state.seen.append(alias),
		}
	}
}

tag_union_has_payload_zig : TagUnionRepr -> Bool
tag_union_has_payload_zig = |tu| TypeTable.tag_union_has_payload(tu)

add_tag_union_aliases_zig : { content : Str, seen : List(Str) }, Str, Str, TagUnionRepr -> { content : Str, seen : List(Str) }
add_tag_union_aliases_zig = |state, alias, target, tu| {
	with_main_alias = add_type_alias_zig(state, alias, target)

	if tag_union_has_payload_zig(tu) {
		with_payload_alias = add_type_alias_zig(with_main_alias, "${alias}Payload", "${target}Payload")
		add_type_alias_zig(with_payload_alias, "${alias}Tag", "${target}Tag")
	} else {
		with_main_alias
	}
}

generate_platform_type_aliases_zig : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_platform_type_aliases_zig = |hosted_functions, provides_list, type_table, duplicate_tag_names, preferred_names| {
	var $state = { content: "", seen: generated_type_names_zig(type_table, duplicate_tag_names) }
	name_plan = TypeNamePlan.from_table(type_table)

	for plan in name_plan.alias_plan(type_alias_roots_zig(hosted_functions, provides_list, type_table)) {
		target = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, plan.type_id)
		$state = match plan.kind {
			PlainAlias => add_type_alias_zig($state, plan.alias, target)
			TagUnionAlias =>
				match type_table.get(plan.type_id) {
					RocTagUnion(tu) => add_tag_union_aliases_zig($state, plan.alias, target, tu)
					_ => add_type_alias_zig($state, plan.alias, target)
				}
			}
	}

	if $state.content == "" {
		""
	} else {
		"// Platform Type Aliases\n\n${$state.content}\n"
	}
}

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

to_uppercase : U8 -> U8
to_uppercase = |ch| ch - 32

to_lowercase : U8 -> U8
to_lowercase = |ch| ch + 32

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

## Capitalize the first character of a string
capitalize_first : Str -> Str
capitalize_first = |s| RocName.capitalize_first(s)

## Checks `capitalize_first` for this representative case.
expect capitalize_first("hello") == "Hello"
## Checks `capitalize_first` for this representative case.
expect capitalize_first("Hello") == "Hello"
## Checks `capitalize_first` for this representative case.
expect capitalize_first("") == ""

# =============================================================================
# Name Conversion
# =============================================================================

## Convert function name to PascalCase struct name (e.g., "Stdout.line!" -> "StdoutLine")
name_to_struct_name : Str -> Str
name_to_struct_name = |name| RocName.from_str(name).to_pascal()

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Stdout.line!") == "StdoutLine"
## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("line!") == "Line"
## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"

## Convert function name to snake_case (e.g., "Stdout.line!" -> "stdout_line")
name_to_snake : Str -> Str
name_to_snake = |name| RocName.from_str(name).to_lower_snake()

## Checks `name_to_snake` for this representative case.
expect name_to_snake("Stdout.line!") == "stdout_line"
## Checks `name_to_snake` for this representative case.
expect name_to_snake("line!") == "line"
## Checks `name_to_snake` for this representative case.
expect name_to_snake("Foo.barBaz!") == "foo_bar_baz"
## Checks `name_to_snake` for this representative case.
expect name_to_snake("PartDef.Idx.get!") == "part_def_idx_get"

## Convert function name to camelCase for Zig function names (e.g., "Stdout.line!" -> "hostedStdoutLine")
name_to_camel : Str -> Str
name_to_camel = |name| RocName.from_str(name).to_camel()

## Checks `name_to_camel` for this representative case.
expect name_to_camel("Stdout.line!") == "stdoutLine"
## Checks `name_to_camel` for this representative case.
expect name_to_camel("Echo.line!") == "echoLine"
## Checks `name_to_camel` for this representative case.
expect name_to_camel("PartDef.Idx.get!") == "partDefIdxGet"

## Quote a Roc record field as a Zig identifier without changing its name.
name_to_zig_quoted_ident : Str -> Str
name_to_zig_quoted_ident = |name| "@\"${name}\""

## Checks `name_to_zig_quoted_ident` for this representative case.
expect name_to_zig_quoted_ident("init!") == "@\"init!\""
## Checks `name_to_zig_quoted_ident` for this representative case.
expect name_to_zig_quoted_ident("render!") == "@\"render!\""
## Checks `name_to_zig_quoted_ident` for this representative case.
expect name_to_zig_quoted_ident("answer") == "@\"answer\""

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
# Zig Code Generation
# =============================================================================

## Generate the complete Zig source file
generate_zig_file : List(HostedFunctionInfo), TypeTable, List(ProvidesEntry) -> Str
generate_zig_file = |hosted_functions, type_table, provides_list| {
	duplicate_tag_names = duplicate_tag_union_names(type_table)
	preferred_names = preferred_type_names_zig(hosted_functions, provides_list, type_table, duplicate_tag_names)

	file_header
		.concat(generate_imports)
		.concat("\n")
		.concat(generate_host_abi_types)
		.concat("\n")
		.concat(generate_roc_box_helpers)
		.concat("\n")
		.concat(generate_roc_str)
		.concat("\n")
		.concat(generate_roc_list_generic)
		.concat("\n")
		.concat(generate_roc_io)
		.concat("\n")
		.concat(generate_roc_env)
		.concat("\n")
		.concat(generate_element_type_structs(type_table, duplicate_tag_names, preferred_names))
		.concat(generate_tag_union_structs(type_table, duplicate_tag_names, preferred_names))
		.concat(generate_all_record_structs(hosted_functions, type_table, duplicate_tag_names, preferred_names))
		.concat(generate_all_args_structs(hosted_functions, type_table, duplicate_tag_names, preferred_names))
		.concat(generate_platform_type_aliases_zig(hosted_functions, provides_list, type_table, duplicate_tag_names, preferred_names))
		.concat(generate_refcount_helpers(type_table, duplicate_tag_names, preferred_names))
		.concat("\n")
		.concat(generate_runtime_symbol_externs)
		.concat("\n")
		.concat(generate_hosted_symbol_externs(hosted_functions, type_table, duplicate_tag_names, preferred_names))
		.concat("\n")
		.concat(generate_host_helpers)
		.concat("\n")
		.concat(generate_entrypoint_externs(provides_list, type_table, duplicate_tag_names, preferred_names))
}

## File header comment
file_header : Str
file_header =
	"//! Roc Platform ABI\n//!\n//! This file defines Zig declarations for the direct symbol ABI used by a Roc platform.\n//! It is automatically generated by the Roc glue generator.\n//!\n//! Hosted argument ownership:\n//! - Roc transfers ownership of refcounted arguments to the hosted function.\n//! - The hosted function must decref owned refcounted arguments when done.\n//! - If the host stores or returns an argument, it must retain or transfer ownership explicitly.\n//!\n//! Import this file from the platform host and implement the listed hosted symbols\n//! with the exact natural C ABI signatures shown below.\n\n"

## Import section
generate_imports : Str
generate_imports =
	"const std = @import(\"std\");\nconst builtin = @import(\"builtin\");\n"

## Generate self-contained host ABI type definitions
generate_host_abi_types : Str
generate_host_abi_types =
	\\/// Runtime representation of Roc's fixed-point `Dec` value.
	\\///
	\\/// `num` stores the decimal value scaled by 10^18.
	\\pub const RocDec = extern struct {
	\\    num: i128,
	\\};
	\\
	\\comptime {
	\\    if (@sizeOf(RocDec) != 16) @compileError("RocDec size mismatch");
	\\    if (@alignOf(RocDec) != 16) @compileError("RocDec alignment mismatch");
	\\}
	\\
	\\/// Runtime representation of an opaque `Box(T)` value.
	\\pub const RocBox = ?*anyopaque;
	\\
	\\/// Host-internal allocator and diagnostic context used by helper functions in this file.
	\\///
	\\/// Compiled Roc code does not receive this value. The real host ABI is the set of direct
	\\/// linker symbols declared below (`roc_alloc`, hosted symbols, and provided entrypoints).
	\\pub const RocHost = extern struct {
	\\    env: *anyopaque,
	\\    roc_alloc: *const fn (*RocHost, usize, usize) callconv(.c) ?*anyopaque,
	\\    roc_dealloc: *const fn (*RocHost, *anyopaque, usize) callconv(.c) void,
	\\    roc_realloc: *const fn (*RocHost, *anyopaque, usize, usize) callconv(.c) ?*anyopaque,
	\\    roc_dbg: *const fn (*RocHost, [*]const u8, usize) callconv(.c) void,
	\\    roc_expect_failed: *const fn (*RocHost, [*]const u8, usize) callconv(.c) void,
	\\    roc_crashed: *const fn (*RocHost, [*]const u8, usize) callconv(.c) void,
	\\};
	\\
	\\/// Private erased-callable function pointer stored in `RocErasedCallablePayload`.
	\\pub const RocErasedCallableFn = *const fn (*RocHost, ?[*]u8, ?[*]const u8, ?[*]u8) callconv(.c) void;
	\\
	\\/// Final-drop callback for inline erased-callable captures.
	\\pub const RocErasedCallableOnDrop = *const fn (?[*]u8, *RocHost) callconv(.c) void;
	\\
	\\/// Payload header for `Box(function)`.
	\\pub const RocErasedCallablePayload = extern struct {
	\\    callable_fn_ptr: RocErasedCallableFn,
	\\    on_drop: ?RocErasedCallableOnDrop,
	\\};
	\\
	\\/// Runtime representation of `Box(function)`.
	\\pub const RocErasedCallable = ?[*]u8;
	\\
	\\pub const roc_erased_callable_capture_alignment: usize = 16;
	\\pub const roc_erased_callable_payload_alignment: usize = 16;
	\\pub const roc_erased_callable_capture_offset: usize = std.mem.alignForward(usize, @sizeOf(RocErasedCallablePayload), roc_erased_callable_capture_alignment);
	\\
	\\pub fn rocErasedCallablePayloadSize(capture_size: usize) usize {
	\\    return roc_erased_callable_capture_offset + capture_size;
	\\}
	\\
	\\pub fn rocErasedCallablePayloadPtr(callable: RocErasedCallable) *RocErasedCallablePayload {
	\\    return @ptrCast(@alignCast(callable orelse unreachable));
	\\}
	\\
	\\pub fn rocErasedCallableCapturePtr(callable: RocErasedCallable) ?[*]u8 {
	\\    const data = callable orelse return null;
	\\    return data + roc_erased_callable_capture_offset;
	\\}
	\\
	\\pub fn rocErasedCallableAllocate(
	\\    roc_host: *RocHost,
	\\    callable_fn_ptr: RocErasedCallableFn,
	\\    on_drop: ?RocErasedCallableOnDrop,
	\\    capture_size: usize,
	\\) RocErasedCallable {
	\\    const ptr_width = @sizeOf(usize);
	\\    const alignment = @max(ptr_width, roc_erased_callable_payload_alignment);
	\\    const extra_bytes = @max(ptr_width, roc_erased_callable_payload_alignment);
	\\    const length = extra_bytes + rocErasedCallablePayloadSize(capture_size);
	\\    const base: [*]u8 = @ptrCast(roc_host.roc_alloc(roc_host, length, alignment));
	\\    const data = base + extra_bytes;
	\\    const rc: *isize = @ptrFromInt(@intFromPtr(data) - @sizeOf(isize));
	\\    rc.* = 1;
	\\    const payload: *RocErasedCallablePayload = @ptrCast(@alignCast(data));
	\\    payload.* = .{ .callable_fn_ptr = callable_fn_ptr, .on_drop = on_drop };
	\\    return data;
	\\}
	\\

## Generate self-contained RocBox refcount helpers
generate_roc_box_helpers : Str
generate_roc_box_helpers =
	\\/// Payload drop callback for a boxed value.
	\\///
	\\/// The callback receives the boxed payload data pointer and must recursively
	\\/// decref any Roc refcounted values inside the payload. It must not free the
	\\/// box allocation; `decrefBoxWith` and `freeBoxWith` free it after the callback.
	\\pub const RocBoxPayloadDecref = *const fn (?*anyopaque, *RocHost) callconv(.c) void;
	\\
	\\/// Increment the refcount of a boxed payload data pointer.
	\\pub fn increfBox(data_ptr: ?*anyopaque, amount: isize) void {
	\\    const data = boxDataPtr(data_ptr) orelse return;
	\\    const rc = boxRefcountPtr(data);
	\\    if (rc.* == 0) return; // REFCOUNT_STATIC_DATA
	\\    _ = @atomicRmw(isize, rc, .Add, amount, .monotonic);
	\\}
	\\
	\\/// Allocate a Roc box and return a pointer to its payload data.
	\\pub fn allocateBox(
	\\    payload_size: usize,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    roc_host: *RocHost,
	\\) *anyopaque {
	\\    const ptr_width = @sizeOf(usize);
	\\    const required_space: usize = if (payload_contains_refcounted) (2 * ptr_width) else ptr_width;
	\\    const header_bytes = @max(required_space, payload_alignment);
	\\    const alloc_alignment = @max(ptr_width, payload_alignment);
	\\    const base: [*]u8 = @ptrCast(roc_host.roc_alloc(roc_host, header_bytes + payload_size, alloc_alignment));
	\\    const data = base + header_bytes;
	\\    const rc: *isize = @ptrFromInt(@intFromPtr(data) - @sizeOf(isize));
	\\    rc.* = 1;
	\\    return @ptrCast(data);
	\\}
	\\
	\\/// Decrement a pointer-aligned boxed payload with no Roc refcounted values.
	\\pub fn decrefBox(data_ptr: ?*anyopaque, roc_host: *RocHost) void {
	\\    decrefBoxWith(data_ptr, @alignOf(usize), false, null, roc_host);
	\\}
	\\
	\\/// Increment a boxed function closure.
	\\pub fn increfErasedCallable(callable: RocErasedCallable, amount: isize) void {
	\\    const data = callable orelse return;
	\\    increfBox(@ptrCast(data), amount);
	\\}
	\\
	\\/// Decrement a boxed function closure and run its capture drop callback on final release.
	\\pub fn decrefErasedCallable(callable: RocErasedCallable, roc_host: *RocHost) void {
	\\    const data = callable orelse return;
	\\    decrefBoxWith(@ptrCast(data), roc_erased_callable_payload_alignment, false, &dropErasedCallablePayload, roc_host);
	\\}
	\\
	\\fn dropErasedCallablePayload(data_ptr: ?*anyopaque, roc_host: *RocHost) callconv(.c) void {
	\\    const data = data_ptr orelse return;
	\\    const callable: RocErasedCallable = @ptrCast(data);
	\\    const payload = rocErasedCallablePayloadPtr(callable);
	\\    if (payload.on_drop) |on_drop| {
	\\        on_drop(rocErasedCallableCapturePtr(callable), roc_host);
	\\    }
	\\}
	\\
	\\/// Decrement a boxed payload and run payload teardown when this is the final ref.
	\\///
	\\/// `payload_contains_refcounted` must match the value passed to `allocateBox`:
	\\/// it determines the box header size, and is independent of whether a
	\\/// `payload_decref` teardown callback is supplied. A host resource handle such
	\\/// as `Box(U64)` holding a raw pointer has `payload_contains_refcounted = false`
	\\/// even when it provides a teardown callback to free the underlying resource.
	\\pub fn decrefBoxWith(
	\\    data_ptr: ?*anyopaque,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    payload_decref: ?RocBoxPayloadDecref,
	\\    roc_host: *RocHost,
	\\) void {
	\\    const data = boxDataPtr(data_ptr) orelse return;
	\\    const rc = boxRefcountPtr(data);
	\\    if (rc.* == 0) return; // REFCOUNT_STATIC_DATA
	\\
	\\    const prev = @atomicRmw(isize, rc, .Sub, 1, .monotonic);
	\\    if (prev == 1) {
	\\        if (payload_decref) |callback| callback(data_ptr, roc_host);
	\\        freeBoxAllocation(data, payload_alignment, payload_contains_refcounted, roc_host);
	\\    }
	\\}
	\\
	\\/// Free a boxed payload allocation immediately after running payload teardown.
	\\///
	\\/// See `decrefBoxWith` for the meaning of `payload_contains_refcounted`.
	\\pub fn freeBoxWith(
	\\    data_ptr: ?*anyopaque,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    payload_decref: ?RocBoxPayloadDecref,
	\\    roc_host: *RocHost,
	\\) void {
	\\    const data = boxDataPtr(data_ptr) orelse return;
	\\    if (payload_decref) |callback| callback(data_ptr, roc_host);
	\\    freeBoxAllocation(data, payload_alignment, payload_contains_refcounted, roc_host);
	\\}
	\\
	\\/// Return true when a boxed payload data pointer has exactly one live ref.
	\\pub fn isUniqueBox(data_ptr: ?*anyopaque) bool {
	\\    const data = boxDataPtr(data_ptr) orelse return true;
	\\    const rc = boxRefcountPtr(data);
	\\    return rc.* == 1;
	\\}
	\\
	\\fn boxDataPtr(data_ptr: ?*anyopaque) ?[*]u8 {
	\\    const ptr = data_ptr orelse return null;
	\\    return @ptrCast(ptr);
	\\}
	\\
	\\fn boxRefcountPtr(data: [*]u8) *isize {
	\\    return @ptrFromInt(@intFromPtr(data) - @sizeOf(isize));
	\\}
	\\
	\\fn freeBoxAllocation(
	\\    data: [*]u8,
	\\    payload_alignment: usize,
	\\    payload_contains_refcounted: bool,
	\\    roc_host: *RocHost,
	\\) void {
	\\    const ptr_width = @sizeOf(usize);
	\\    const required_space: usize = if (payload_contains_refcounted) (2 * ptr_width) else ptr_width;
	\\    const header_bytes = @max(required_space, payload_alignment);
	\\    const alloc_alignment = @max(ptr_width, payload_alignment);
	\\    const base: *anyopaque = @ptrFromInt(@intFromPtr(data) - header_bytes);
	\\    roc_host.roc_dealloc(roc_host, base, alloc_alignment);
	\\}
	\\

## Generate self-contained RocStr type definition
generate_roc_str : Str
generate_roc_str =
	\\/// A Roc string value. Small strings (up to 23 bytes) are stored inline;
	\\/// larger strings are heap-allocated with a reference count.
	\\///
	\\/// `bytes` is never tagged. Operations, host code, glue code, and object-file
	\\/// relocations can use it directly as the UTF-8 byte pointer for non-small
	\\/// strings. Seamless-slice tagging lives in `capacity_or_alloc_ptr` instead.
	\\/// Big-string capacity is stored shifted left by one bit, so max capacity is
	\\/// essentially `std.math.maxInt(isize)` bytes: about 2 GiB on 32-bit targets
	\\/// and 8 EiB on 64-bit targets.
	\\pub const RocStr = extern struct {
	\\    bytes: ?[*]u8,
	\\    capacity_or_alloc_ptr: usize,
	\\    length: usize,
	\\
	\\    const Self = @This();
	\\    const small_string_size = @sizeOf(RocStr);
	\\    const small_str_max_length = small_string_size - 1;
	\\    const small_str_bit: usize = @as(usize, @bitCast(@as(isize, std.math.minInt(isize))));
	\\    const seamless_slice_tag: usize = 1;
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
	\\            return self.length;
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
	\\        return @as(isize, @bitCast(self.length)) < 0;
	\\    }
	\\
	\\    /// Return true if this string is a seamless slice into another allocation.
	\\    pub fn isSeamlessSlice(self: Self) bool {
	\\        return !self.isSmallStr() and (self.capacity_or_alloc_ptr & seamless_slice_tag) != 0;
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
	\\        return .{ .bytes = null, .capacity_or_alloc_ptr = 0, .length = small_str_bit };
	\\    }
	\\
	\\    /// Create a RocStr from a byte slice, using `roc_host` for heap allocation if needed.
	\\    pub fn fromSlice(slice: []const u8, roc_host: *RocHost) Self {
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
	\\            const base: [*]u8 = @ptrCast(roc_host.roc_alloc(roc_host, total, @alignOf(usize)));
	\\            const data_ptr = base + extra_bytes;
	\\            const rc: *isize = @ptrFromInt(@intFromPtr(data_ptr) - @sizeOf(isize));
	\\            rc.* = 1;
	\\            @memcpy(data_ptr[0..slice.len], slice.ptr[0..slice.len]);
	\\            return .{
	\\                .bytes = data_ptr,
	\\                .capacity_or_alloc_ptr = slice.len << 1,
	\\                .length = slice.len,
	\\            };
	\\        }
	\\    }
	\\
	\\    /// Decrement the reference count; frees the allocation when it reaches zero.
	\\    pub fn decref(self: Self, roc_host: *RocHost) void {
	\\        if (self.isSmallStr()) return;
	\\        const alloc_ptr = self.getAllocationPtr() orelse return;
	\\        const data_addr = @intFromPtr(alloc_ptr);
	\\        const rc: *isize = @ptrFromInt(data_addr - @sizeOf(isize));
	\\        if (rc.* == 0) return; // REFCOUNT_STATIC_DATA — bytes are in read-only memory
	\\        const prev = @atomicRmw(isize, rc, .Sub, 1, .monotonic);
	\\        if (prev == 1) {
	\\            const ptr_width = @sizeOf(usize);
	\\            const base: *anyopaque = @ptrFromInt(data_addr - ptr_width);
	\\            roc_host.roc_dealloc(roc_host, base, @alignOf(usize));
	\\        }
	\\    }
	\\
	\\    /// Increment the reference count by `amount`.
	\\    pub fn incref(self: Self, amount: isize) void {
	\\        if (self.isSmallStr()) return;
	\\        const alloc_ptr = self.getAllocationPtr() orelse return;
	\\        const rc: *isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\        if (rc.* == 0) return; // REFCOUNT_STATIC_DATA
	\\        _ = @atomicRmw(isize, rc, .Add, amount, .monotonic);
	\\    }
	\\
	\\    /// Return true if this string has a reference count of exactly one.
	\\    pub fn isUnique(self: Self) bool {
	\\        if (self.isSmallStr()) return true;
	\\        const alloc_ptr = self.getAllocationPtr() orelse return true;
	\\        const rc: *const isize = @ptrFromInt(@intFromPtr(alloc_ptr) - @sizeOf(isize));
	\\        if (rc.* == 0) return true; // REFCOUNT_STATIC_DATA — treated as unique
	\\        return rc.* == 1;
	\\    }
	\\
	\\    fn getAllocationPtr(self: Self) ?[*]u8 {
	\\        if (self.isSeamlessSlice()) {
	\\            return @as(?[*]u8, @ptrFromInt(self.capacity_or_alloc_ptr & ~seamless_slice_tag));
	\\        } else {
	\\            return self.bytes;
	\\        }
	\\    }
	\\};
	\\

## Generate extern structs for record return types using compiler-emitted ABI field offsets.
## Only generates RetRecord structs when ret_type_id resolves to a record in the type table.
## Tag union return types (e.g., Try(Record, Str)) are not yet supported and are skipped.
generate_all_record_structs : List(HostedFunctionInfo), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_all_record_structs = |hosted_functions, type_table, duplicate_tag_names, preferred_names| {
	var $structs = ""
	arg_shape = ArgShape.from_table(type_table)
	for func in hosted_functions {
		match arg_shape.record_lookup(func.ret_type_id) {
			ArgRecordFound(record) => {
				struct_name = name_to_struct_name(func.name)
				fields64 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, record.fields, Pointer64)
				fields32 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, record.fields, Pointer32)

				doc = "/// Return type record for ${func.name}\n/// Fields ordered by compiler-emitted ABI offsets.\n"
				$structs = Str.concat(
					$structs,
					zig_record_struct_decl(doc, "${struct_name}RetRecord", fields64, fields32, "", "", record.layout),
				)
			}
			ArgNotRecord => {}
		}
	}
	$structs
}

## Generate all argument extern structs
generate_all_args_structs : List(HostedFunctionInfo), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_all_args_structs = |hosted_functions, type_table, duplicate_tag_names, preferred_names| {
	var $structs = ""
	for func in hosted_functions {
		$structs = Str.concat($structs, generate_args_struct(func, type_table, duplicate_tag_names, preferred_names))
	}
	$structs
}

## Generate a single argument extern struct (empty string if no args).
## Uses type table for single-record args; positional for multi-arg or primitive args.
generate_args_struct : HostedFunctionInfo, TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_args_struct = |func, type_table, duplicate_tag_names, preferred_names| {
	struct_name = name_to_struct_name(func.name)
	arg_shape = ArgShape.from_table(type_table)

	match arg_shape.hosted_args(func) {
		NoMeaningfulArgs => ""
		SingleRecordArg(record) => {
			fields64 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, record.fields, Pointer64)
			fields32 = zig_record_fields_decl(type_table, duplicate_tag_names, preferred_names, record.fields, Pointer32)

			doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"
			zig_record_struct_decl(doc, "${struct_name}Args", fields64, fields32, "", "", record.layout)
		}
		PositionalArgs(arg_type_ids) => {
			var $positional_fields = ""
			var $idx = 0
			for arg_type_id in arg_type_ids {
				zig_type = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, arg_type_id)
				$positional_fields = Str.concat(
					$positional_fields,
					"    arg${U64.to_str($idx)}: ${zig_type},\n",
				)
				$idx = $idx + 1
			}

			doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"

			"${doc}pub const ${struct_name}Args = extern struct {\n${$positional_fields}};\n\n"
		}
	}
}

## Build a natural C ABI parameter list from Roc function argument type IDs.
direct_param_list : TypeTable, List(Str), TypeNamePlan.PreferredNames, List(U64) -> Str
direct_param_list = |type_table, duplicate_tag_names, preferred_names, arg_type_ids| {
	var $params = ""
	var $idx = 0
	arg_shape = ArgShape.from_table(type_table)

	for arg_type_id in arg_shape.positional_non_unit_type_ids(arg_type_ids) {
		arg_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, arg_type_id)
		sep = if $params == "" {
			""
		} else {
			", "
		}
		$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_zig}"
		$idx = $idx + 1
	}

	$params
}

## Build a hosted symbol parameter list, using the generated Args wrapper for
## anonymous single-record arguments so direct-symbol glue stays readable.
direct_hosted_param_list : TypeTable, List(Str), TypeNamePlan.PreferredNames, HostedFunctionInfo -> Str
direct_hosted_param_list = |type_table, duplicate_tag_names, preferred_names, func| {
	arg_shape = ArgShape.from_table(type_table)
	use_args_wrapper = arg_shape.single_arg_is_anonymous_record(func.arg_type_ids)

	var $params = ""
	var $idx = 0

	for arg_type_id in arg_shape.positional_non_unit_type_ids(func.arg_type_ids) {
		arg_zig = if use_args_wrapper {
			"${name_to_struct_name(func.name)}Args"
		} else {
			type_id_to_zig(type_table, duplicate_tag_names, preferred_names, arg_type_id)
		}
		sep = if $params == "" {
			""
		} else {
			", "
		}
		$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_zig}"
		$idx = $idx + 1
	}

	$params
}

## Generate direct extern declarations for the fixed runtime symbols every host defines.
generate_runtime_symbol_externs : Str
generate_runtime_symbol_externs =
	\\// Runtime Symbols
	\\//
	\\// The host defines these linker symbols. Compiled Roc code calls them directly.
	\\
	\\pub extern fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque;
	\\pub extern fn roc_dealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void;
	\\pub extern fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque;
	\\pub extern fn roc_dbg(bytes: [*]const u8, len: usize) callconv(.c) void;
	\\pub extern fn roc_expect_failed(bytes: [*]const u8, len: usize) callconv(.c) void;
	\\pub extern fn roc_crashed(bytes: [*]const u8, len: usize) callconv(.c) void;
	\\

## Generate direct extern declarations for hosted symbols.
generate_hosted_symbol_externs : List(HostedFunctionInfo), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_hosted_symbol_externs = |hosted_functions, type_table, duplicate_tag_names, preferred_names| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $result = "// Hosted Symbols\n//\n// The platform host must export these symbols with the exact direct C ABI signatures.\n// Refcounted arguments are owned by the hosted function.\n\n"

	for func in hosted_functions {
		params = direct_hosted_param_list(type_table, duplicate_tag_names, preferred_names, func)
		ret_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, func.ret_type_id)

		$result = Str.concat(
			$result,
			"/// Hosted symbol for ${func.name}\n/// Roc signature: ${func.type_str}\npub extern fn ${func.ffi_symbol}(${params}) callconv(.c) ${ret_zig};\n\n",
		)
	}

	$result
}

# =============================================================================
# Host Helper Utilities
# =============================================================================

## Generate DefaultAllocators, DefaultHandlers, and makeRocHost helpers.
## These are static Zig code blocks that don't depend on specific hosted functions.
generate_host_helpers : Str
generate_host_helpers =
	generate_default_allocators
		.concat("\n")
		.concat(generate_default_handlers)
		.concat("\n")
		.concat(generate_make_roc_host)

## Generate the DefaultAllocators struct
generate_default_allocators : Str
generate_default_allocators =
	\\/// Default memory management functions for Roc platforms.
	\\///
	\\/// Uses `RocEnv.allocator` for allocation. Each allocation prepends
	\\/// size metadata so that dealloc/realloc can recover the original
	\\/// allocation size (required because `roc_dealloc` receives no length).
	\\pub const DefaultAllocators = struct {
	\\    /// Allocate memory for the Roc runtime.
	\\    pub fn rocAlloc(roc_host: *RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
	\\        const env: *RocEnv = @ptrCast(@alignCast(roc_host.env));
	\\        const allocator = env.allocator;
	\\
	\\        const min_alignment: usize = @max(alignment, @alignOf(usize));
	\\        const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
	\\        const size_storage_bytes = min_alignment;
	\\        const total_size = length + size_storage_bytes;
	\\
	\\        const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
	\\            env.roc_io.writeStderr("roc_alloc: out of memory\\n");
	\\            env.roc_io.onFatal();
	\\        };
	\\
	\\        // Store total size immediately before the user data pointer
	\\        const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
	\\        size_ptr.* = total_size;
	\\
	\\        return @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
	\\    }
	\\
	\\    /// Free memory previously allocated by `rocAlloc`.
	\\    pub fn rocDealloc(roc_host: *RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
	\\        const env: *RocEnv = @ptrCast(@alignCast(roc_host.env));
	\\        const allocator = env.allocator;
	\\
	\\        const min_alignment: usize = @max(alignment, @alignOf(usize));
	\\        const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
	\\        const size_storage_bytes = min_alignment;
	\\
	\\        const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
	\\        const total_size = size_ptr.*;
	\\
	\\        const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);
	\\        const slice = base_ptr[0..total_size];
	\\        allocator.rawFree(slice, align_enum, @returnAddress());
	\\    }
	\\
	\\    /// Reallocate memory, copying existing data to the new allocation.
	\\    pub fn rocRealloc(roc_host: *RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
	\\        const env: *RocEnv = @ptrCast(@alignCast(roc_host.env));
	\\        const allocator = env.allocator;
	\\
	\\        const min_alignment: usize = @max(alignment, @alignOf(usize));
	\\        const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
	\\        const size_storage_bytes = min_alignment;
	\\
	\\        // Read old size from metadata
	\\        const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
	\\        const old_total_size = old_size_ptr.*;
	\\        const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);
	\\
	\\        // Allocate new block
	\\        const new_total_size = new_length + size_storage_bytes;
	\\        const new_base_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
	\\            env.roc_io.writeStderr("roc_realloc: out of memory\\n");
	\\            env.roc_io.onFatal();
	\\        };
	\\
	\\        // Copy old user data to new location
	\\        const old_user_data_size = old_total_size - size_storage_bytes;
	\\        const copy_size = @min(old_user_data_size, new_length);
	\\        const new_user_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes);
	\\        const old_user_ptr: [*]const u8 = @ptrCast(ptr);
	\\        @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);
	\\
	\\        // Free old allocation
	\\        allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());
	\\
	\\        // Store new size and return user pointer
	\\        const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes - @sizeOf(usize));
	\\        new_size_ptr.* = new_total_size;
	\\        return new_user_ptr;
	\\    }
	\\};
	\\

## Generate the DefaultHandlers namespace
generate_default_handlers : Str
generate_default_handlers =
	\\/// Default handlers for dbg, expect-failed, and crash.
	\\///
	\\/// Routes output through `RocEnv.roc_io` for platform portability.
	\\pub const DefaultHandlers = struct {
	\\    /// Print a `dbg` expression to stderr.
	\\    pub fn rocDbg(roc_host: *RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
	\\        const env: *RocEnv = @ptrCast(@alignCast(roc_host.env));
	\\        env.roc_io.writeStderr("[ROC DBG] ");
	\\        env.roc_io.writeStderr(bytes[0..len]);
	\\        env.roc_io.writeStderr("\\n");
	\\    }
	\\
	\\    /// Print a failed `expect` to stderr.
	\\    pub fn rocExpectFailed(roc_host: *RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
	\\        const env: *RocEnv = @ptrCast(@alignCast(roc_host.env));
	\\        env.roc_io.writeStderr("[ROC EXPECT] ");
	\\        env.roc_io.writeStderr(bytes[0..len]);
	\\        env.roc_io.writeStderr("\\n");
	\\    }
	\\
	\\    /// Print a `crash` message to stderr and terminate.
	\\    pub fn rocCrashed(roc_host: *RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
	\\        const env: *RocEnv = @ptrCast(@alignCast(roc_host.env));
	\\        env.roc_io.writeStderr("[ROC CRASHED] ");
	\\        env.roc_io.writeStderr(bytes[0..len]);
	\\        env.roc_io.writeStderr("\\n");
	\\        env.roc_io.onFatal();
	\\    }
	\\};
	\\

## Generate the makeRocHost convenience function
generate_make_roc_host : Str
generate_make_roc_host =
	\\/// Create a host-internal helper context with default memory management and handlers.
	\\///
	\\/// This is only for helper functions in this generated file. It is not passed to
	\\/// compiled Roc code, which uses the direct symbol ABI declared above.
	\\///
	\\/// Example:
	\\/// ```zig
	\\/// var env = RocEnv{ .allocator = gpa.allocator(), .roc_io = RocIo.default() };
	\\/// var roc_host = makeRocHost(&env);
	\\/// ```
	\\pub fn makeRocHost(env: *RocEnv) RocHost {
	\\    return .{
	\\        .env = @ptrCast(env),
	\\        .roc_alloc = &DefaultAllocators.rocAlloc,
	\\        .roc_dealloc = &DefaultAllocators.rocDealloc,
	\\        .roc_realloc = &DefaultAllocators.rocRealloc,
	\\        .roc_dbg = &DefaultHandlers.rocDbg,
	\\        .roc_expect_failed = &DefaultHandlers.rocExpectFailed,
	\\        .roc_crashed = &DefaultHandlers.rocCrashed,
	\\    };
	\\}
	\\

# =============================================================================
# Entrypoint Declarations
# =============================================================================

generate_provided_decl : ProvidesEntry, TypeTable, List(Str), TypeNamePlan.PreferredNames, TypeRepr -> Str
generate_provided_decl = |entry, type_table, duplicate_tag_names, preferred_names, type_repr| {
	match type_repr {
		RocFunction(func) => {
			params = direct_param_list(type_table, duplicate_tag_names, preferred_names, func.args)
			ret_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, func.ret)
			"/// Entrypoint: ${entry.name}\npub extern fn ${entry.ffi_symbol}(${params}) callconv(.c) ${ret_zig};\n\n"
		}
		_ => {
			value_zig = type_id_to_zig(type_table, duplicate_tag_names, preferred_names, entry.type_id)
			"/// Static provided value: ${entry.name}\npub extern const ${entry.ffi_symbol}: ${value_zig};\n\n"
		}
	}
}

## Generate extern declarations for entrypoints from the provides clause.
generate_entrypoint_externs : List(ProvidesEntry), TypeTable, List(Str), TypeNamePlan.PreferredNames -> Str
generate_entrypoint_externs = |provides_list, type_table, duplicate_tag_names, preferred_names| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $result = "// Provided Symbols\n//\n// Roc exports these symbols from the app with their natural C ABI signatures.\n\n"

	for entry in provides_list {
		type_repr = type_table.get(entry.type_id)
		$result = Str.concat($result, generate_provided_decl(entry, type_table, duplicate_tag_names, preferred_names, type_repr))
	}

	$result
}

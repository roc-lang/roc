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
import pf.ProvidesEntry exposing [ProvidesEntry]

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

	zig_content = generate_zig_file(sorted, $type_table, $provides_entries)

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
		RocBox(inner_id) =>
			match List.get(type_table, inner_id) {
				Ok(RocFunction(_)) => "RocErasedCallable"
				Ok(RocUnknown(_)) => "RocBox"
				_ => {
					inner_zig = type_id_to_zig(type_table, inner_id)
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
		RocDec => "f64"
		RocList(elem_id) =>
			if is_type_refcounted(type_table, elem_id) {
				"RocList(${type_id_to_zig(type_table, elem_id)})"
			} else {
				"RocListWith(${type_id_to_zig(type_table, elem_id)}, false)"
			}
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
		RocBox(_) => Bool.True
		RocList(_) => Bool.True
		RocFunction(_) => Bool.True
		RocRecord(rec) => List.any(rec.fields, |field| is_type_refcounted(type_table, field.type_id))
		RocTagUnion(tu) => List.any(tu.tags, |tag| List.any(tag.payload, |pid| is_type_refcounted(type_table, pid)))
		RocBool => Bool.False
		RocDec => Bool.False
		RocF32 => Bool.False
		RocF64 => Bool.False
		RocI128 => Bool.False
		RocI16 => Bool.False
		RocI32 => Bool.False
		RocI64 => Bool.False
		RocI8 => Bool.False
		RocU128 => Bool.False
		RocU16 => Bool.False
		RocU32 => Bool.False
		RocU64 => Bool.False
		RocU8 => Bool.False
		RocUnit => Bool.False
		RocUnknown(_) => Bool.False
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

align_forward_u64 : U64, U64 -> U64
align_forward_u64 = |offset, alignment| {
	if alignment == 0 {
		offset
	} else {
		remainder = U64.rem_by(offset, alignment)
		if remainder == 0 {
			offset
		} else {
			offset + alignment - remainder
		}
	}
}

type_layout_64 : List(TypeRepr), U64 -> { size : U64, alignment : U64 }
type_layout_64 = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => repr_layout_64(type_table, type_repr)
		Err(_) => { size: 8, alignment: 8 }
	}
}

repr_layout_64 : List(TypeRepr), TypeRepr -> { size : U64, alignment : U64 }
repr_layout_64 = |type_table, type_repr| {
	match type_repr {
		RocBool => { size: 1, alignment: 1 }
		RocBox(_) => { size: 8, alignment: 8 }
		RocDec => { size: 8, alignment: 8 }
		RocF32 => { size: 4, alignment: 4 }
		RocF64 => { size: 8, alignment: 8 }
		RocFunction(_) => { size: 8, alignment: 8 }
		RocI128 => { size: 16, alignment: 16 }
		RocI16 => { size: 2, alignment: 2 }
		RocI32 => { size: 4, alignment: 4 }
		RocI64 => { size: 8, alignment: 8 }
		RocI8 => { size: 1, alignment: 1 }
		RocList(_) => { size: 24, alignment: 8 }
		RocRecord(rec) => record_layout_from_fields(type_table, rec.fields)
		RocStr => { size: 24, alignment: 8 }
		RocTagUnion(tu) =>
			if List.len(tu.tags) == 1 {
				match List.first(tu.tags) {
					Ok(tag) =>
						match List.first(tag.payload) {
							Ok(payload_id) => type_layout_64(type_table, payload_id)
							_ => { size: 0, alignment: 1 }
						}
					_ => { size: 0, alignment: 1 }
				}
			} else {
				{ size: tu.size, alignment: tu.alignment }
			}
		RocU128 => { size: 16, alignment: 16 }
		RocU16 => { size: 2, alignment: 2 }
		RocU32 => { size: 4, alignment: 4 }
		RocU64 => { size: 8, alignment: 8 }
		RocU8 => { size: 1, alignment: 1 }
		RocUnit => { size: 0, alignment: 1 }
		RocUnknown(_) => { size: 8, alignment: 8 }
	}
}

record_layout_from_fields : List(TypeRepr), List(RecordField) -> { size : U64, alignment : U64 }
record_layout_from_fields = |type_table, fields| {
	var $offset = 0
	var $alignment = 1

	for field in fields {
		field_layout = type_layout_64(type_table, field.type_id)
		if field_layout.alignment > $alignment {
			$alignment = field_layout.alignment
		}
		$offset = align_forward_u64($offset, field_layout.alignment)
		$offset = $offset + field_layout.size
	}

	{ size: align_forward_u64($offset, $alignment), alignment: $alignment }
}

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
						field_name = name_to_zig_quoted_ident(field.name)
						$field_strs = Str.concat(
							$field_strs,
							"    ${field_name}: ${zig_type},\n",
						)
					}

					# Comptime size/alignment assertions (guarded by pointer width)
					layout = record_layout_from_fields(type_table, rec.fields)
					assertions = if layout.size > 0 {
						"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}) != ${U64.to_str(layout.size)}) @compileError(\"${struct_name} size mismatch\");\n        if (@alignOf(${struct_name}) != ${U64.to_str(layout.alignment)}) @compileError(\"${struct_name} alignment mismatch\");\n    }\n}\n\n"
					} else {
						""
					}

					$structs = Str.concat(
						$structs,
						"/// Element type for ${rec.name}\npub const ${struct_name} = extern struct {\n${$field_strs}};\n\n${assertions}",
					)
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

box_payload_decref_name = |inner_id| "decrefBoxPayloadType${U64.to_str(inner_id)}"

missing_type_compile_error = |type_id, mode| {
	"    comptime { @compileError(\"missing glue type information for recursive ${mode} of type id ${U64.to_str(type_id)}\"); }\n"
}

decref_helper_name_from_repr = |type_repr| {
	match type_repr {
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				"decref${name_to_struct_name(rec.name)}"
			}
		RocTagUnion(tu) =>
			if List.len(tu.tags) >= 2 and tu.name != "" {
				"decref${name_to_struct_name(tu.name)}"
			} else {
				""
			}
		_ => ""
	}
}

incref_helper_name_from_repr = |type_repr| {
	match type_repr {
		RocRecord(rec) =>
			if rec.name == "" {
				""
			} else {
				"incref${name_to_struct_name(rec.name)}"
			}
		RocTagUnion(tu) =>
			if List.len(tu.tags) >= 2 and tu.name != "" {
				"incref${name_to_struct_name(tu.name)}"
			} else {
				""
			}
		_ => ""
	}
}

decref_stmt_for_type_id = |type_table, type_id, expr| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => decref_stmt_for_repr(type_table, type_repr, expr)
		Err(_) => missing_type_compile_error(type_id, "decref")
	}
}

decref_stmt_for_repr = |type_table, type_repr, expr| {
	match type_repr {
		RocStr => "    ${expr}.decref(roc_host);\n"
		RocList(elem_id) => {
			elem_stmt = decref_stmt_for_type_id(type_table, elem_id, "item")
			if elem_stmt == "" {
				"    ${expr}.decref(roc_host);\n"
			} else {
				"    {\n        const list = ${expr};\n        if (list.hasOneRef()) {\n            for (list.allocationItems()) |item| {\n${indent_lines(elem_stmt, "                ")}            }\n        }\n        list.decref(roc_host);\n    }\n"
			}
		}
		RocBox(inner_id) =>
			match List.get(type_table, inner_id) {
				Ok(RocFunction(_)) => "    decrefErasedCallable(${expr}, roc_host);\n"
				Ok(_) => {
					inner_zig = type_id_to_zig(type_table, inner_id)
					if inner_zig == "*anyopaque" {
						"    decrefBox(@ptrCast(${expr}), roc_host);\n"
					} else if is_type_refcounted(type_table, inner_id) {
						"    decrefBoxWith(@ptrCast(${expr}), @alignOf(${inner_zig}), &${box_payload_decref_name(inner_id)}, roc_host);\n"
					} else {
						"    decrefBox(@ptrCast(${expr}), roc_host);\n"
					}
				}
				Err(_) => missing_type_compile_error(inner_id, "boxed-payload decref")
			}
		RocRecord(_) => {
			helper = decref_helper_name_from_repr(type_repr)
			if helper == "" or !(is_repr_refcounted(type_table, type_repr)) {
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
							Ok(payload_id) => decref_stmt_for_type_id(type_table, payload_id, expr)
							_ => ""
						}
					_ => ""
				}
			} else {
				helper = decref_helper_name_from_repr(type_repr)
				if helper == "" or !(is_repr_refcounted(type_table, type_repr)) {
					""
				} else {
					"    ${helper}(${expr}, roc_host);\n"
				}
			}
		_ => ""
	}
}

incref_stmt_for_type_id = |type_table, type_id, expr| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => incref_stmt_for_repr(type_table, type_repr, expr)
		Err(_) => missing_type_compile_error(type_id, "incref")
	}
}

incref_stmt_for_repr = |type_table, type_repr, expr| {
	match type_repr {
		RocStr => "    ${expr}.incref(amount);\n"
		RocList(_) => "    ${expr}.incref(amount);\n"
		RocBox(inner_id) =>
			match List.get(type_table, inner_id) {
				Ok(RocFunction(_)) => "    increfErasedCallable(${expr}, amount);\n"
				Ok(_) => "    increfBox(@ptrCast(${expr}), amount);\n"
				Err(_) => missing_type_compile_error(inner_id, "boxed-payload incref")
			}
		RocRecord(_) => {
			helper = incref_helper_name_from_repr(type_repr)
			if helper == "" or !(is_repr_refcounted(type_table, type_repr)) {
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
							Ok(payload_id) => incref_stmt_for_type_id(type_table, payload_id, expr)
							_ => ""
						}
					_ => ""
				}
			} else {
				helper = incref_helper_name_from_repr(type_repr)
				if helper == "" or !(is_repr_refcounted(type_table, type_repr)) {
					""
				} else {
					"    ${helper}(${expr}, amount);\n"
				}
			}
		_ => ""
	}
}

generate_record_refcount_helpers = |type_table, rec| {
	struct_name = name_to_struct_name(rec.name)
	var $decref_body = ""
	var $incref_body = ""

	for field in rec.fields {
		field_expr = "value.${name_to_zig_quoted_ident(field.name)}"
		$decref_body = Str.concat($decref_body, decref_stmt_for_type_id(type_table, field.type_id, field_expr))
		$incref_body = Str.concat($incref_body, incref_stmt_for_type_id(type_table, field.type_id, field_expr))
	}

	if $decref_body == "" {
		$decref_body = "    _ = value;\n    _ = roc_host;\n"
	}
	if $incref_body == "" {
		$incref_body = "    _ = value;\n    _ = amount;\n"
	}

	"/// Recursively decrement Roc-owned fields in ${struct_name}.\npub fn decref${struct_name}(value: ${struct_name}, roc_host: *RocHost) void {\n${$decref_body}}\n\n/// Increment Roc-owned fields in ${struct_name}.\npub fn incref${struct_name}(value: ${struct_name}, amount: isize) void {\n${$incref_body}}\n\n"
}

generate_tag_payload_refcount_branch = |type_table, tag, mode| {
	snake = to_lower_snake_case(tag.name)
	if List.is_empty(tag.payload) {
		return "        .${tag.name} => {},\n"
	}

	if List.len(tag.payload) == 1 {
		body =
			match List.first(tag.payload) {
				Ok(payload_id) =>
					if mode == "decref" {
						decref_stmt_for_type_id(type_table, payload_id, "value.payload.${snake}")
					} else {
						incref_stmt_for_type_id(type_table, payload_id, "value.payload.${snake}")
					}
				_ => ""
			}

		if body == "" {
			"        .${tag.name} => {},\n"
		} else {
			"        .${tag.name} => {\n${indent_lines(body, "    ")}        },\n"
		}
	} else {
		var $body = "        const payload = value.payload.${snake};\n"
		var $idx = 0
		for payload_id in tag.payload {
			field_expr = "payload._${U64.to_str($idx)}"
			stmt = if mode == "decref" {
				decref_stmt_for_type_id(type_table, payload_id, field_expr)
			} else {
				incref_stmt_for_type_id(type_table, payload_id, field_expr)
			}
			$body = Str.concat($body, indent_lines(stmt, "    "))
			$idx = $idx + 1
		}

		"        .${tag.name} => {\n${$body}        },\n"
	}
}

generate_tag_union_refcount_helpers = |type_table, tu| {
	struct_name = name_to_struct_name(tu.name)
	var $decref_branches = ""
	var $incref_branches = ""
	for tag in tu.tags {
		$decref_branches = Str.concat($decref_branches, generate_tag_payload_refcount_branch(type_table, tag, "decref"))
		$incref_branches = Str.concat($incref_branches, generate_tag_payload_refcount_branch(type_table, tag, "incref"))
	}

	"/// Recursively decrement Roc-owned payloads in ${struct_name}.\npub fn decref${struct_name}(value: ${struct_name}, roc_host: *RocHost) void {\n    switch (value.tag) {\n${$decref_branches}    }\n}\n\n/// Increment Roc-owned payloads in ${struct_name}.\npub fn incref${struct_name}(value: ${struct_name}, amount: isize) void {\n    switch (value.tag) {\n${$incref_branches}    }\n}\n\n"
}

generate_box_payload_decref_helpers = |type_table| {
	var $helpers = ""
	var $seen_inner_ids = []

	for type_repr in type_table {
		match type_repr {
			RocBox(inner_id) => {
				if !(List.contains($seen_inner_ids, inner_id)) {
					$seen_inner_ids = $seen_inner_ids.append(inner_id)
					match List.get(type_table, inner_id) {
						Ok(RocFunction(_)) => {}
						Ok(_) => {
							inner_zig = type_id_to_zig(type_table, inner_id)
							if inner_zig != "*anyopaque" and is_type_refcounted(type_table, inner_id) {
								stmt = decref_stmt_for_type_id(type_table, inner_id, "payload.*")
								$helpers = Str.concat(
									$helpers,
									"fn ${box_payload_decref_name(inner_id)}(data_ptr: ?*anyopaque, roc_host: *RocHost) callconv(.c) void {\n    const payload: *${inner_zig} = @ptrCast(@alignCast(data_ptr orelse return));\n${stmt}}\n\n",
								)
							}
						}
						Err(_) => {
							$helpers = Str.concat(
								$helpers,
								"fn ${box_payload_decref_name(inner_id)}(_: ?*anyopaque, _: *RocHost) callconv(.c) void {\n    comptime { @compileError(\"missing glue type information for boxed payload type id ${U64.to_str(inner_id)}\"); }\n}\n\n",
							)
						}
					}
				}
			}
			_ => {}
		}
	}

	$helpers
}

generate_refcount_helpers = |type_table| {
	var $helpers = "// =============================================================================\n// Generated Refcount Helpers\n//\n// These helpers recursively retain or release Roc-owned fields using the explicit\n// TypeRepr layout supplied to glue generation. For RocList(T), element release\n// only runs when the list allocation is uniquely owned, immediately before the\n// outer list allocation is decremented and potentially freed.\n// =============================================================================\n\n"
	var $seen_names = []

	for type_repr in type_table {
		match type_repr {
			RocRecord(rec) =>
				if rec.name != "" and is_repr_refcounted(type_table, type_repr) and !(List.contains($seen_names, rec.name)) {
					$seen_names = $seen_names.append(rec.name)
					$helpers = Str.concat($helpers, generate_record_refcount_helpers(type_table, rec))
				}
			RocTagUnion(tu) =>
				if List.len(tu.tags) >= 2 and tu.name != "" and is_repr_refcounted(type_table, type_repr) and !(List.contains($seen_names, tu.name)) {
					$seen_names = $seen_names.append(tu.name)
					$helpers = Str.concat($helpers, generate_tag_union_refcount_helpers(type_table, tu))
				}
			_ => {}
		}
	}

	$helpers.concat(generate_box_payload_decref_helpers(type_table))
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

## Quote a Roc record field as a Zig identifier without changing its name.
name_to_zig_quoted_ident : Str -> Str
name_to_zig_quoted_ident = |name| "@\"${name}\""

expect name_to_zig_quoted_ident("init!") == "@\"init!\""
expect name_to_zig_quoted_ident("render!") == "@\"render!\""
expect name_to_zig_quoted_ident("answer") == "@\"answer\""

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
## Type annotation ensures the interpreter uses the full TypeRepr layout.
lookup_record_in_type_table : List(TypeRepr), U64 -> { found: Bool, fields: List(RecordField), size: U64, alignment: U64 }
lookup_record_in_type_table = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => lookup_record_from_repr(type_table, type_repr)
		Err(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
	}
}

## Match a TypeRepr and return record fields if it's a record.
## Type annotation ensures the interpreter uses the full 21-variant TypeRepr layout.
lookup_record_from_repr : List(TypeRepr), TypeRepr -> { found: Bool, fields: List(RecordField), size: U64, alignment: U64 }
lookup_record_from_repr = |type_table, type_repr| {
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
		RocBox(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocBool => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocDec => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocF32 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocF64 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocFunction(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocI128 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocI16 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocI32 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocI64 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocI8 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocList(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocStr => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocU128 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocU16 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocU32 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocU64 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocU8 => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocUnit => { found: Bool.False, fields: [], size: 0, alignment: 0 }
		RocUnknown(_) => { found: Bool.False, fields: [], size: 0, alignment: 0 }
	}
}

# =============================================================================
# Zig Code Generation
# =============================================================================

## Generate the complete Zig source file
generate_zig_file = |hosted_functions, type_table, provides_list| {
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
		.concat(generate_element_type_structs(type_table))
		.concat(generate_tag_union_structs(type_table))
		.concat(generate_all_record_structs(hosted_functions, type_table))
		.concat(generate_all_args_structs(hosted_functions, type_table))
		.concat(generate_refcount_helpers(type_table))
		.concat("\n")
		.concat(generate_runtime_symbol_externs)
		.concat("\n")
		.concat(generate_hosted_symbol_externs(hosted_functions, type_table))
		.concat("\n")
		.concat(generate_host_helpers)
		.concat("\n")
		.concat(generate_entrypoint_externs(provides_list, type_table))
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
	\\    decrefBoxWith(data_ptr, @alignOf(usize), null, roc_host);
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
	\\    decrefBoxWith(@ptrCast(data), roc_erased_callable_payload_alignment, &dropErasedCallablePayload, roc_host);
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
	\\pub fn decrefBoxWith(
	\\    data_ptr: ?*anyopaque,
	\\    payload_alignment: usize,
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
	\\        freeBoxAllocation(data, payload_alignment, payload_decref != null, roc_host);
	\\    }
	\\}
	\\
	\\/// Free a boxed payload allocation immediately after running payload teardown.
	\\pub fn freeBoxWith(
	\\    data_ptr: ?*anyopaque,
	\\    payload_alignment: usize,
	\\    payload_decref: ?RocBoxPayloadDecref,
	\\    roc_host: *RocHost,
	\\) void {
	\\    const data = boxDataPtr(data_ptr) orelse return;
	\\    if (payload_decref) |callback| callback(data_ptr, roc_host);
	\\    freeBoxAllocation(data, payload_alignment, payload_decref != null, roc_host);
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
				field_name = name_to_zig_quoted_ident(field.name)
				$fields = Str.concat(
					$fields,
					"    ${field_name}: ${zig_type},\n",
				)
			}

			layout = record_layout_from_fields(type_table, type_table_result.fields)
			assertions = if layout.size > 0 {
				"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}RetRecord) != ${U64.to_str(layout.size)}) @compileError(\"${struct_name}RetRecord size mismatch\");\n        if (@alignOf(${struct_name}RetRecord) != ${U64.to_str(layout.alignment)}) @compileError(\"${struct_name}RetRecord alignment mismatch\");\n    }\n}\n\n"
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
	if !(has_meaningful_args(func, type_table)) {
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
			field_name = name_to_zig_quoted_ident(field.name)
			$fields = Str.concat(
				$fields,
				"    ${field_name}: ${zig_type},\n",
			)
		}

	layout = record_layout_from_fields(type_table, type_table_result.fields)
	assertions = if layout.size > 0 {
		"comptime {\n    if (@sizeOf(usize) == 8) {\n        if (@sizeOf(${struct_name}Args) != ${U64.to_str(layout.size)}) @compileError(\"${struct_name}Args size mismatch\");\n        if (@alignOf(${struct_name}Args) != ${U64.to_str(layout.alignment)}) @compileError(\"${struct_name}Args alignment mismatch\");\n    }\n}\n\n"
	} else {
		""
	}

		doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"
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

	doc = "/// Arguments for ${func.name}\n/// Roc signature: ${func.type_str}\n/// Refcounted fields are owned by the hosted function.\n"

	"${doc}pub const ${struct_name}Args = extern struct {\n${$positional_fields}};\n\n"
}

## Check whether a type is the zero-sized Roc unit type.
is_unit_type_id = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(RocUnit) => Bool.True
		_ => Bool.False
	}
}

## Check whether a type is an explicitly anonymous record shape.
is_anonymous_record_type_id = |type_table, type_id| {
	match List.get(type_table, type_id) {
		Ok(type_repr) => is_anonymous_record_repr(type_table, type_repr)
		Err(_) => Bool.False
	}
}

is_anonymous_record_repr = |type_table, type_repr| {
	match type_repr {
		RocRecord(rec) => rec.anonymous
		RocTagUnion(tu) =>
			if List.len(tu.tags) == 1 {
				match List.first(tu.tags) {
					Ok(tag) =>
						match List.first(tag.payload) {
							Ok(payload_id) => is_anonymous_record_type_id(type_table, payload_id)
							_ => Bool.False
						}
					_ => Bool.False
				}
			} else {
				Bool.False
			}
		_ => Bool.False
	}
}

## Build a natural C ABI parameter list from Roc function argument type IDs.
direct_param_list = |type_table, arg_type_ids| {
	var $params = ""
	var $idx = 0

	for arg_type_id in arg_type_ids {
		if !is_unit_type_id(type_table, arg_type_id) {
			arg_zig = type_id_to_zig(type_table, arg_type_id)
			sep = if $params == "" {
				""
			} else {
				", "
			}
			$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_zig}"
			$idx = $idx + 1
		}
	}

	$params
}

## Build a hosted symbol parameter list, using the generated Args wrapper for
## anonymous single-record arguments so direct-symbol glue stays readable.
direct_hosted_param_list = |type_table, func| {
	use_args_wrapper =
		if List.len(func.arg_type_ids) == 1 {
			match List.first(func.arg_type_ids) {
				Ok(arg_id) => is_anonymous_record_type_id(type_table, arg_id)
				Err(_) => Bool.False
			}
		} else {
			Bool.False
		}

	var $params = ""
	var $idx = 0

	for arg_type_id in func.arg_type_ids {
		if !is_unit_type_id(type_table, arg_type_id) {
			arg_zig = if use_args_wrapper {
				"${name_to_struct_name(func.name)}Args"
			} else {
				type_id_to_zig(type_table, arg_type_id)
			}
			sep = if $params == "" {
				""
			} else {
				", "
			}
			$params = "${$params}${sep}arg${U64.to_str($idx)}: ${arg_zig}"
			$idx = $idx + 1
		}
	}

	$params
}

## Generate direct extern declarations for the fixed runtime symbols every host defines.
generate_runtime_symbol_externs : Str
generate_runtime_symbol_externs =
	\\// =============================================================================
	\\// Runtime Symbols
	\\//
	\\// The host defines these linker symbols. Compiled Roc code calls them directly.
	\\// =============================================================================
	\\
	\\pub extern fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque;
	\\pub extern fn roc_dealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void;
	\\pub extern fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque;
	\\pub extern fn roc_dbg(bytes: [*]const u8, len: usize) callconv(.c) void;
	\\pub extern fn roc_expect_failed(bytes: [*]const u8, len: usize) callconv(.c) void;
	\\pub extern fn roc_crashed(bytes: [*]const u8, len: usize) callconv(.c) void;
	\\

## Generate direct extern declarations for hosted symbols.
generate_hosted_symbol_externs = |hosted_functions, type_table| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $result = "// =============================================================================\n// Hosted Symbols\n//\n// The platform host must export these symbols with the exact direct C ABI signatures.\n// Refcounted arguments are owned by the hosted function.\n// =============================================================================\n\n"

	for func in hosted_functions {
		params = direct_hosted_param_list(type_table, func)
		ret_zig = type_id_to_zig(type_table, func.ret_type_id)

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
# Argument Helpers
# =============================================================================

## Check if a hosted function has meaningful (non-unit) arguments using the type table.
has_meaningful_args = |func, type_table| {
	if List.is_empty(func.arg_type_ids) {
		Bool.False
	} else if List.len(func.arg_type_ids) == 1 {
		match List.first(func.arg_type_ids) {
			Ok(id) =>
				match List.get(type_table, id) {
					Ok(RocUnit) => Bool.False
					_ => Bool.True
				}
			_ => Bool.False
		}
	} else {
		Bool.True
	}
}

# =============================================================================
# Entrypoint Declarations
# =============================================================================

generate_provided_decl = |entry, type_table, type_repr| {
	match type_repr {
		RocFunction(func) => {
			params = direct_param_list(type_table, func.args)
			ret_zig = type_id_to_zig(type_table, func.ret)
			"/// Entrypoint: ${entry.name}\npub extern fn ${entry.ffi_symbol}(${params}) callconv(.c) ${ret_zig};\n\n"
		}
		_ => {
			value_zig = type_repr_to_zig(type_table, type_repr)
			"/// Static provided value: ${entry.name}\npub extern const ${entry.ffi_symbol}: ${value_zig};\n\n"
		}
	}
}

## Generate extern declarations for entrypoints from the provides clause.
generate_entrypoint_externs = |provides_list, type_table| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $result = "// =============================================================================\n// Provided Symbols\n//\n// Roc exports these symbols from the app with their natural C ABI signatures.\n// =============================================================================\n\n"

	for entry in provides_list {
		match List.get(type_table, entry.type_id) {
			Ok(type_repr) => {
				$result = Str.concat($result, generate_provided_decl(entry, type_table, type_repr))
			}
			Err(_) => {
				$result = Str.concat($result, "comptime { @compileError(\"missing glue type information for provided symbol ${entry.ffi_symbol}\"); }\n\n")
			}
		}
	}

	$result
}

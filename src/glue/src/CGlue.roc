## A glue script for generating a C header file.
app [make_glue] { pf: platform glue }

import pf.Types
import pf.File
import pf.TypeRepr
import pf.AbiLayout
import pf.AbiFieldLayout
import pf.AbiTagLayout
import pf.AbiWidth
import pf.ArgShape
import pf.GlueInput
import pf.HostedFunctionInfo
import pf.TypeNamePlan
import pf.RecordField
import pf.RecordRepr
import pf.TagUnionRepr
import pf.ProvidesEntry
import pf.TypeTable
import pf.RocName

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	input = GlueInput.from_types(types_list)
	type_table = TypeTable.from_list(input.types)
	symbols = TypeNamePlan.platform_symbols(input.hosted_functions, input.provides_entries)
	undeclarable_symbol_c(symbols)?
	renamed_roots = renamed_roots_c(input.hosted_functions, input.provides_entries, type_table, symbols)?
	duplicate_names = duplicate_type_names(input.hosted_functions, type_table, symbols, renamed_roots)?
	header_content = generate_c_header(input.hosted_functions, type_table, input.provides_entries, duplicate_names)

	Ok([{ name: "roc_platform_abi.h", content: header_content }])
}

# =============================================================================
# TypeRepr-based C Type Mapping
# =============================================================================

type_id_to_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64 -> Str
type_id_to_c = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id| {
	# A zero-sized value has no host representation.
	if type_table.is_zero_sized(type_id) {
		return "void"
	}

	type_repr = type_table.get(type_id)
	type_repr_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, type_repr)
}

type_repr_to_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64, TypeRepr -> Str
type_repr_to_c = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, type_repr| {
	match type_repr {
		RocBool => "bool"
		RocBox(inner_id) =>
			match type_table.get(inner_id) {
				RocUnknown(_) => "RocBox"
				_ => {
					inner_c = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, inner_id)
					if inner_c == "void*" or inner_c == "RocBox" or type_table.is_zero_sized(inner_id) {
						"RocBox"
					} else {
						"${inner_c}*"
					}
				}
			}
		RocStr => "RocStr"
		RocUnit => "void"
		RocU8 => "uint8_t"
		RocU8x16 => "RocU8x16"
		RocI8x16 => "RocI8x16"
		RocU16x8 => "RocU16x8"
		RocI16x8 => "RocI16x8"
		RocU32x4 => "RocU32x4"
		RocI32x4 => "RocI32x4"
		RocU64x2 => "RocU64x2"
		RocI64x2 => "RocI64x2"
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
		RocList(elem_id) =>
			match type_table.get(elem_id) {
				RocUnknown(_) => "RocOpaqueList"
				_ =>
					if type_table.is_roc_released(elem_id) {
						"RocOpaqueList"
					} else {
						"RocList"
					}
				}
		RocRecord(rec) =>
			if rec.name == "" {
				"void*"
			} else {
				record_struct_name(type_table, duplicate_record_names, type_id, rec)
			}
		RocTagUnion(tu) => resolve_tag_union_type_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, tu)
		# A function stored inside a value is one erased-callable allocation,
		# exactly like `Box(fn)`.
		RocErasedCallable(_) => "RocErasedCallable"
		RocUnknown(_) => "void*"
	}
}

resolve_tag_union_type_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr -> Str
resolve_tag_union_type_c = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, type_id, tu|
	match type_table.single_variant_payload(type_id, tu) {
		SinglePayload(payload_id) => type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, payload_id)
		SinglePayloadStruct(_) => tag_union_struct_name(type_table, preferred_names, duplicate_tag_names, type_id, tu)
		SingleNoPayload => "void"
		NotSingleVariant =>
			if tu.name != "" {
				tag_union_struct_name(type_table, preferred_names, duplicate_tag_names, type_id, tu)
			} else {
				"void*"
			}
		}

## Transitively flattened vector-member count of the type when its leaves are
## exclusively 128-bit vectors (counting through record fields and
## single-variant tag unions, which are their payloads); 0 for any other type.
## Counts saturate at 5: callers only distinguish the 2..4 range, which is the
## memory-class shape that needs the vector/byte union spelling.
homogeneous_vector_member_count : TypeTable, U64 -> U64
homogeneous_vector_member_count = |type_table, type_id|
	match type_table.get(type_id) {
		RocU8x16 => 1
		RocI8x16 => 1
		RocU16x8 => 1
		RocI16x8 => 1
		RocU32x4 => 1
		RocI32x4 => 1
		RocU64x2 => 1
		RocI64x2 => 1
		RocRecord(rec) => {
			var $total = 0
			var $homogeneous = !(List.is_empty(rec.fields))
			for field in rec.fields {
				if field.is_padding or !type_table.is_zero_sized(field.type_id) {
					member = if field.is_padding {
						0
					} else {
						homogeneous_vector_member_count(type_table, field.type_id)
					}
					if member == 0 {
						$homogeneous = Bool.False
					} else if $total + member > 4 {
						$total = 5
					} else {
						$total = $total + member
					}
				}
			}
			if $homogeneous {
				$total
			} else {
				0
			}
		}
		RocTagUnion(tu) =>
			match type_table.single_variant_payload(type_id, tu) {
				SinglePayload(payload_id) => homogeneous_vector_member_count(type_table, payload_id)
				SinglePayloadStruct(tag_layout) => homogeneous_vector_field_count(type_table, tag_layout.payload_fields)
				SingleNoPayload => 0
				NotSingleVariant => 0
			}
		_ => 0
	}

## `homogeneous_vector_member_count` of a struct with these committed fields.
homogeneous_vector_field_count : TypeTable, List(AbiFieldLayout) -> U64
homogeneous_vector_field_count = |type_table, fields| {
	var $total = 0
	var $homogeneous = !(List.is_empty(fields))
	for field in fields {
		if !field.is_zero_sized() {
			member = if field.is_padding {
				0
			} else {
				homogeneous_vector_member_count(type_table, field.type_id)
			}
			if member == 0 {
				$homogeneous = Bool.False
			} else if $total + member > 4 {
				$total = 5
			} else {
				$total = $total + member
			}
		}
	}
	if $homogeneous {
		$total
	} else {
		0
	}
}

## Whether a struct with these committed fields is a memory-class vector
## aggregate: every member is (an aggregate of) 128-bit vectors and there are
## two to four of them in total. C classifies such an aggregate as homogeneous
## and would pass it in registers, but the Roc host ABI passes it in memory—
## the convention Zig and Rust hosts compile to—so C glue spells its members
## as vector/byte unions to pin the memory-class classification.
c_needs_vector_union_spelling : TypeTable, List(AbiFieldLayout) -> Bool
c_needs_vector_union_spelling = |type_table, fields| {
	total = homogeneous_vector_field_count(type_table, fields)
	total >= 2 and total <= 4
}

c_vector_union_comment : Str
c_vector_union_comment =
	\\/* Every member of this aggregate is a 128-bit vector, so members are spelled
	\\   as vector/byte unions (read the vector via `.value`). The union keeps C
	\\   from classifying the aggregate as homogeneous: the Roc host ABI passes
	\\   multi-vector aggregates in memory, matching Zig and Rust hosts. */

## The named fields of a record, whose names settle each one's member name.
field_names_c : List(AbiFieldLayout) -> List(Str)
field_names_c = |fields| {
	var $names = []

	for field in fields {
		if !field.is_padding {
			$names = $names.append(field.name)
		}
	}

	$names
}

## The tags of a union, whose names settle each one's member names.
tag_names_c : TagUnionRepr -> List(Str)
tag_names_c = |tu| List.map(tu.tags, |tag| tag.name)

## The member names of the fields or tags named `member_names`. Every
## identifier `name_to_c_field_ident` gives is lowercase, so a ranked one is no
## other member's own.
field_idents_c : List(Str) -> TypeNamePlan.MemberIdents
field_idents_c = |member_names|
	TypeNamePlan.member_idents(member_names, name_to_c_field_ident, "_Member")

## For each tag, the part of its constant and payload struct names that comes
## from the tag. No name `name_to_struct_name` gives holds an underscore, so a
## ranked one is no other tag's own.
tag_constants_c : List(Str) -> TypeNamePlan.MemberIdents
tag_constants_c = |tag_names|
	TypeNamePlan.member_idents(tag_names, name_to_struct_name, "_")

c_record_field_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, Str, AbiFieldLayout, AbiWidth, Bool -> Str
c_record_field_decl = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, member_name, field, width, wrap_vector_members| {
	if field.is_padding {
		field_name = name_to_c_field_ident(field.name)

		# Padding fields are nonzero at both widths (asserted by the compiler).
		"    uint8_t ${field_name}[${U64.to_str(AbiFieldLayout.size(field, width))}];\n"
	} else {
		field_name = member_name
		c_type = type_id_to_c(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, field.type_id)
		if wrap_vector_members {
			"    union { ${c_type} value; uint8_t bytes[${U64.to_str(AbiFieldLayout.size(field, width))}]; } ${field_name};\n"
		} else {
			"    ${c_type} ${field_name};\n"
		}
	}
}

## Fields arrive in committed layout order (valid at both pointer widths);
## only per-width padding byte counts differ between the two renderings.
c_record_fields_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, TypeNamePlan.MemberIdents, List(AbiFieldLayout), AbiWidth, Bool -> Str
c_record_fields_decl = |type_table, duplicate_record_names, duplicate_tag_names, preferred_names, field_idents, fields, width, wrap_vector_members| {
	var $field_strs = ""
	var $member_index = 0
	for field in fields {
		member_name = if field.is_padding {
			""
		} else {
			field_idents.at($member_index)
		}
		if !field.is_padding {
			$member_index = $member_index + 1
		}

		# A zero-sized field occupies no storage, and C has no zero-sized member.
		if !field.is_zero_sized() {
			$field_strs = Str.concat($field_strs, c_record_field_decl(type_table, duplicate_record_names, duplicate_tag_names, preferred_names, member_name, field, width, wrap_vector_members))
		}
	}
	$field_strs
}

duplicate_record_names : TypeTable -> List(Str)
duplicate_record_names = |type_table| type_table.duplicate_record_names()

record_struct_name : TypeTable, List(Str), U64, RecordRepr -> Str
record_struct_name = |type_table, duplicate_names, type_id, rec| {
	Str.concat(name_to_struct_name(rec.name), type_table.shape_suffix(duplicate_names, rec.name, type_id))
}

duplicate_tag_union_names : TypeTable -> List(Str)
duplicate_tag_union_names = |type_table| type_table.duplicate_tag_union_names()

default_tag_union_struct_name : TypeTable, List(Str), U64, TagUnionRepr -> Str
default_tag_union_struct_name = |type_table, duplicate_names, type_id, tu| {
	Str.concat(name_to_struct_name(tu.name), type_table.shape_suffix(duplicate_names, tu.name, type_id))
}

tag_union_struct_name : TypeTable, TypeNamePlan.PreferredNames, List(Str), U64, TagUnionRepr -> Str
tag_union_struct_name = |type_table, preferred_names, duplicate_names, type_id, tu| {
	preferred = preferred_names.lookup(type_id)
	if preferred.found {
		preferred.name
	} else {
		default_tag_union_struct_name(type_table, duplicate_names, type_id, tu)
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

## Roc names emitted under id-derived names. For host types these are the
## named tag unions and named records whose name appears in the type table
## with different shapes, and those whose declarations would collide with
## another declaration in the generated header; they take shape-derived names.
## `renamed_roots` are the hosted functions and provided entries that take
## index-derived names. The result is computed once per glue run and reused
## while rendering names.
duplicate_type_names : List(HostedFunctionInfo), TypeTable, List(Str), List(Str) -> Try(List(Str), Str)
duplicate_type_names = |hosted_functions, type_table, symbols, renamed_roots|
	TypeNamePlan.disambiguated(
		List.concat(reserved_type_names_c(hosted_functions, type_table, renamed_roots), symbols),
		List.concat(renamed_roots, List.concat(duplicate_tag_union_names(type_table), duplicate_record_names(type_table))),
		|renamed| declared_types_c(type_table, renamed),
	)

## Type names the runtime support in every generated header declares or takes
## from the standard headers it includes. A host type declared under one of
## them would redefine it.
runtime_type_names_c : List(Str)
runtime_type_names_c = [
	"HostedFn",
	"HostedFunctions",
	"NULL",
	"RocBox",
	"RocDec",
	"RocErasedCallable",
	"RocErasedCallableFn",
	"RocErasedCallableOnDrop",
	"RocErasedCallablePayload",
	"RocI16x8",
	"RocI32x4",
	"RocI64x2",
	"RocI8x16",
	"RocList",
	"RocOpaqueList",
	"RocOps",
	"RocStr",
	"RocU16x8",
	"RocU32x4",
	"RocU64x2",
	"RocU8x16",
]

## Type names the generated header declares whatever the type table's own
## types are named: the runtime support and each hosted function's argument
## type.
reserved_type_names_c : List(HostedFunctionInfo), TypeTable, List(Str) -> List(Str)
reserved_type_names_c = |hosted_functions, type_table, renamed| {
	arg_shape = ArgShape.from_table(type_table)
	var $names = runtime_type_names_c

	for func in hosted_functions {
		$names = List.concat($names, hosted_top_level_names_c(arg_shape, renamed, func))
	}

	$names
}

## Names other than type names that the generated header declares or uses
## whatever the platform holds: its macros and functions, and the names it
## takes from the standard headers it includes. The header declares each
## platform symbol under the symbol's own name, which would redefine one of
## these or be rewritten by it.
runtime_value_names_c : List(Str)
runtime_value_names_c = [
	"HOSTED_FUNCTION_COUNT",
	"ROC_ALIGNAS",
	"ROC_ALIGNOF",
	"ROC_ASSERT_VECTOR",
	"ROC_ERASED_CALLABLE_CAPTURE_ALIGNMENT",
	"ROC_ERASED_CALLABLE_CAPTURE_OFFSET",
	"ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT",
	"ROC_ERASED_CALLABLE_PAYLOAD_SIZE",
	"ROC_PLATFORM_ABI_H",
	"ROC_RETURNS_NONNULL",
	"ROC_STATIC_ASSERT",
	"UINT64_MAX",
	"UINTPTR_MAX",
	"bool",
	"false",
	"int16_t",
	"int16x8_t",
	"int32_t",
	"int32x4_t",
	"int64_t",
	"int64x2_t",
	"int8_t",
	"int8x16_t",
	"intptr_t",
	"offsetof",
	"ptrdiff_t",
	"roc_abi_copy_bytes",
	"roc_erased_callable_capture_ptr",
	"roc_erased_callable_payload_ptr",
	"size_t",
	"true",
	"uint16_t",
	"uint16x8_t",
	"uint32_t",
	"uint32x4_t",
	"uint64_t",
	"uint64x2_t",
	"uint8_t",
	"uint8x16_t",
	"uintptr_t",
	"v128_t",
]

## Words C keeps for itself, which cannot name a declaration.
keywords_c : List(Str)
keywords_c = [
	"alignas",
	"alignof",
	"auto",
	"break",
	"case",
	"char",
	"const",
	"continue",
	"default",
	"do",
	"double",
	"else",
	"enum",
	"extern",
	"float",
	"for",
	"goto",
	"if",
	"inline",
	"int",
	"long",
	"register",
	"restrict",
	"return",
	"short",
	"signed",
	"sizeof",
	"static",
	"static_assert",
	"struct",
	"switch",
	"typedef",
	"union",
	"unsigned",
	"void",
	"volatile",
	"while",
]

## True for the identifiers C reserves for the implementation, which the
## compiler and its headers may define as anything: those that begin with two
## underscores, or with an underscore and an uppercase letter.
is_implementation_name_c : Str -> Bool
is_implementation_name_c = |name| {
		bytes = Str.to_utf8(name)
		match List.get(bytes, 1) {
			Ok(second) => List.first(bytes) == Ok('_') and (second == '_' or (second >= 'A' and second <= 'Z'))
			Err(_) => Bool.False
		}
	}

## An error when the generated header cannot declare one of the platform's
## symbols under its own name: the name is a keyword, is reserved for the C
## implementation, or belongs to the runtime support.
undeclarable_symbol_c : List(Str) -> Try({}, Str)
undeclarable_symbol_c = |symbols| {
	fixed_names = List.concat(runtime_type_names_c, List.concat(runtime_value_names_c, keywords_c))
	TypeNamePlan.undeclarable_symbol(symbols, fixed_names)?

	for symbol in symbols {
		if is_implementation_name_c(symbol) {
			return Err(TypeNamePlan.symbol_error(symbol))
		}
	}

	Ok({})
}

## The name a hosted function's argument type, and the aliases of the types
## it reaches, are derived from.
hosted_struct_name_c : List(Str), HostedFunctionInfo -> Str
hosted_struct_name_c = |renamed, func|
	Str.concat(name_to_struct_name(func.name), TypeNamePlan.index_suffix(renamed, func.name, func.index, "Hosted"))

## The name of a hosted function's index macro, after `HOSTED_IDX_`.
hosted_upper_ident_c : List(Str), HostedFunctionInfo -> Str
hosted_upper_ident_c = |renamed, func|
	Str.concat(name_to_upper_ident(func.name), TypeNamePlan.index_suffix(renamed, func.name, func.index, "_HOSTED"))

## The name of a hosted function's member of `HostedFunctions`.
hosted_field_ident_c : List(Str), HostedFunctionInfo -> Str
hosted_field_ident_c = |renamed, func|
	Str.concat(name_to_c_field_ident(func.name), TypeNamePlan.index_suffix(renamed, func.name, func.index, "_hosted"))

## The name the aliases of the types a provided entry reaches are derived
## from. `position` is the entry's place in the platform's provides list.
provided_struct_name_c : List(Str), ProvidesEntry, U64 -> Str
provided_struct_name_c = |renamed, entry, position|
	Str.concat(name_to_struct_name(entry.name), TypeNamePlan.index_suffix(renamed, entry.name, position, "Provided"))

## The top-level names the generated header declares for a hosted function:
## its index macro and its argument type.
hosted_top_level_names_c : ArgShape, List(Str), HostedFunctionInfo -> List(Str)
hosted_top_level_names_c = |arg_shape, renamed, func| {
	index_macro = "HOSTED_IDX_${hosted_upper_ident_c(renamed, func)}"

	match arg_shape.hosted_args(func) {
		NoMeaningfulArgs => [index_macro]
		SingleRecordArg(_) => [index_macro, "${hosted_struct_name_c(renamed, func)}Args"]
		PositionalArgs(_) => [index_macro, "${hosted_struct_name_c(renamed, func)}Args"]
	}
}

## What the generated header derives from the name of each hosted function and
## each provided entry when the Roc names in `renamed` take index-derived
## names: a hosted function's top-level names; its member of
## `HostedFunctions`, which is compared only with other members; and the base
## its aliases are named from, which is not itself declared and so is compared
## only with other bases.
declared_roots_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> List(TypeNamePlan.Declared)
declared_roots_c = |hosted_functions, provides_list, type_table, renamed| {
	arg_shape = ArgShape.from_table(type_table)
	var $declared = []

	for func in hosted_functions {
		$declared = $declared.append({
			roc_name: func.name,
			id: func.index * 2,
			names: List.concat(
				hosted_top_level_names_c(arg_shape, renamed, func),
				[
					"member ${hosted_field_ident_c(renamed, func)}",
					"base ${hosted_struct_name_c(renamed, func)}",
				],
			),
		})
	}

	var $position = 0
	for entry in provides_list {
		$declared = $declared.append({
			roc_name: entry.name,
			id: $position * 2 + 1,
			names: ["base ${provided_struct_name_c(renamed, entry, $position)}"],
		})
		$position = $position + 1
	}

	$declared
}

## The hosted functions and provided entries whose derived names take
## index-derived names, because they would collide with another declaration.
renamed_roots_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> Try(List(Str), Str)
renamed_roots_c = |hosted_functions, provides_list, type_table, symbols|
	TypeNamePlan.disambiguated(
		List.concat(runtime_type_names_c, List.concat(runtime_value_names_c, symbols)),
		[],
		|renamed| declared_roots_c(hosted_functions, provides_list, type_table, renamed),
	)

## Every top-level name the generated header declares for the host type at
## `type_id` when it emits that type as `struct_name`: the type itself and each
## helper type, tag constant, and function named after it.
type_names_declared_as_c : TypeTable, U64, Str -> List(Str)
type_names_declared_as_c = |type_table, type_id, struct_name|
	match type_table.get(type_id) {
		RocRecord(rec) =>
			if rec.name != "" and !type_table.is_zero_sized(type_id) {
				[struct_name]
			} else {
				[]
			}
		RocTagUnion(tu) =>
			match type_table.single_variant_payload(type_id, tu) {
				SinglePayloadStruct(_) => [struct_name]
				NotSingleVariant => type_names_declared_as_union_c(type_table, type_id, tu, struct_name)
				SinglePayload(_) => []
				SingleNoPayload => []
			}
		_ => []
	}

## `type_names_declared_as_c` of the tag union `tu` with several variants.
type_names_declared_as_union_c : TypeTable, U64, TagUnionRepr, Str -> List(Str)
type_names_declared_as_union_c = |type_table, type_id, tu, struct_name|
	if tu.name != "" {
		abi_layout = type_table.layout(type_id)
		has_payload = abi_layout.tag_union_has_payload()
		abi_tags = abi_tag_layouts_c(abi_layout)
		var $names = if has_payload {
			[struct_name, "${struct_name}Tag", "${struct_name}TagPayloadStorage"]
		} else {
			[struct_name]
		}
		var $tag_idx = 0
		tag_names = tag_names_c(tu)
		tag_constants = tag_constants_c(tag_names)
		tag_fields = field_idents_c(tag_names)
		for tag in tu.tags {
			constant_name = tag_constants.at($tag_idx)
			$names = $names.append("${struct_name}Tag_${constant_name}")
			if has_payload {
				field_name = tag_fields.at($tag_idx)
				$names = $names.append("${struct_name}_make_${field_name}")
				if abi_tag_has_payload_c(abi_tag_at_c(abi_tags, $tag_idx)) {
					$names = $names.append("${struct_name}_payload_${field_name}")
					if List.len(tag.payload) != 1 {
						$names = $names.append("${struct_name}${constant_name}Payload")
					}
				}
			}
			$tag_idx = $tag_idx + 1
		}
		$names
	} else {
		[]
	}

## Whether the generated header declares the tag union at `type_id` as a
## struct. A union without payloads is declared as its discriminant's integer
## type instead.
declares_tag_union_struct_c : TypeTable, U64, TagUnionRepr -> Bool
declares_tag_union_struct_c = |type_table, type_id, tu|
	match type_table.single_variant_payload(type_id, tu) {
		SinglePayloadStruct(_) => tu.name != ""
		NotSingleVariant => tu.name != "" and type_table.layout(type_id).tag_union_has_payload()
		SinglePayload(_) => Bool.False
		SingleNoPayload => Bool.False
	}

## What the generated header declares for each named host type when the Roc
## names in `duplicate_names` are emitted under shape-derived names.
declared_types_c : TypeTable, List(Str) -> List(TypeNamePlan.Declared)
declared_types_c = |type_table, duplicate_names| {
	var $declared = []
	var $type_id = 0

	for type_info in type_table.entries() {
		if type_info.shape_id == $type_id {
			match type_info.repr {
				RocRecord(rec) =>
					if rec.name != "" and !type_table.is_zero_sized($type_id) {
						struct_name = record_struct_name(type_table, duplicate_names, $type_id, rec)
						$declared = $declared.append({
							roc_name: rec.name,
							id: $type_id,
							names: type_names_declared_as_c(type_table, $type_id, struct_name),
						})
					}
				RocTagUnion(tu) =>
					if type_table.declares_tag_union($type_id, tu) {
						struct_name = default_tag_union_struct_name(type_table, duplicate_names, $type_id, tu)
						$declared = $declared.append({
							roc_name: tu.name,
							id: $type_id,
							names: type_names_declared_as_c(type_table, $type_id, struct_name),
						})
					}
				_ => {}
			}
		}

		$type_id = $type_id + 1
	}

	$declared
}

## Every name the generated header declares before any preferred name or alias
## is chosen, which neither of those may take.
declared_type_names_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> List(Str)
declared_type_names_c = |hosted_functions, provides_list, type_table, duplicate_names| {
	var $names = List.concat(
		reserved_type_names_c(hosted_functions, type_table, duplicate_names),
		TypeNamePlan.platform_symbols(hosted_functions, provides_list),
	)

	for declared in declared_types_c(type_table, duplicate_names) {
		$names = List.concat($names, declared.names)
	}

	$names
}

## A named type lends its name to the aliases of the root it is reached
## from, unless that name is ambiguous: a name several host types are emitted
## under denotes none of them, so the root keeps its own name.
type_name_root_alias_base_c : TypeTable, List(Str), Str, U64 -> Str
type_name_root_alias_base_c = |type_table, ambiguous_names, fallback, type_id|
	match type_table.get(type_id) {
		RocRecord(rec) =>
			if rec.name != "" and !rec.anonymous and !List.contains(ambiguous_names, rec.name) {
				name_to_struct_name(rec.name)
			} else {
				fallback
			}
		RocTagUnion(tu) =>
			if tu.name != "" and tu.name != "Try" and tu.name != "IOErr" and !List.contains(ambiguous_names, tu.name) {
				name_to_struct_name(tu.name)
			} else {
				fallback
			}
		_ => fallback
	}

## The fields a record's aliases are named from: those of the row its
## declaration is emitted from, which is the first row with its shape.
record_alias_fields_c : TypeTable, U64 -> List(RecordField)
record_alias_fields_c = |type_table, type_id|
	match type_table.get(type_table.shape_id(type_id)) {
		RocRecord(declared) => declared.fields
		_ => {
			crash "glue invariant violated: a record's shape id named a row that is not a record"
		}
	}

type_name_roots_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> List(TypeNamePlan.Root)
type_name_roots_c = |hosted_functions, provides_list, type_table, ambiguous_names| {
	var $roots = []

	for func in hosted_functions {
		base = hosted_struct_name_c(ambiguous_names, func)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = $roots.append({
				alias_base: type_name_root_alias_base_c(type_table, ambiguous_names, arg_fallback, arg_type_id),
				module_base,
				type_id: arg_type_id,
			})
			$arg_idx = $arg_idx + 1
		}

		$roots = $roots.append({
			alias_base: base,
			module_base,
			type_id: func.ret_type_id,
		})
	}

	var $position = 0
	for entry in provides_list {
		base = provided_struct_name_c(ambiguous_names, entry, $position)
		module_base = hosted_module_name_to_struct_name(entry.name)
		$position = $position + 1

		match entry.exported {
			ProvidedProcedure(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = $roots.append({
						alias_base: type_name_root_alias_base_c(type_table, ambiguous_names, arg_fallback, arg_type_id),
						module_base,
						type_id: arg_type_id,
					})
					$arg_idx = $arg_idx + 1
				}

				$roots = $roots.append({
					alias_base: base,
					module_base,
					type_id: func.ret,
				})
			}
			ProvidedData(type_id) => {
				$roots = $roots.append({
					alias_base: type_name_root_alias_base_c(type_table, ambiguous_names, base, type_id),
					module_base,
					type_id,
				})
			}
		}
	}

	$roots
}

append_type_alias_roots_c : List(TypeNamePlan.Root), TypeTable, List(Str), Str, Str, U64, List(U64) -> List(TypeNamePlan.Root)
append_type_alias_roots_c = |roots, type_table, ambiguous_names, alias_base, module_base, type_id, visited_type_ids| {
	if List.contains(visited_type_ids, type_id) {
		return roots
	}

	next_visited = visited_type_ids.append(type_id)
	root_alias_base = type_name_root_alias_base_c(type_table, ambiguous_names, alias_base, type_id)

	var $roots = roots.append({ alias_base: root_alias_base, module_base, type_id })

	match type_table.get(type_id) {
		RocRecord(_) => {
			for field in record_alias_fields_c(type_table, type_id) {
				field_base = "${root_alias_base}${RocName.from_str(field.name).to_pascal_clean()}"
				$roots = append_type_alias_roots_c($roots, type_table, ambiguous_names, field_base, module_base, field.type_id, next_visited)
			}
			$roots
		}
		RocList(elem_id) => append_type_alias_roots_c($roots, type_table, ambiguous_names, root_alias_base, module_base, elem_id, next_visited)
		RocBox(inner_id) => append_type_alias_roots_c($roots, type_table, ambiguous_names, root_alias_base, module_base, inner_id, next_visited)
		RocTagUnion(tu) => {
			var $next = $roots
			for tag in tu.tags {
				child_base = "${root_alias_base}${RocName.capitalize_first(tag.name)}"
				for payload_id in tag.payload {
					$next = append_type_alias_roots_c($next, type_table, ambiguous_names, child_base, module_base, payload_id, next_visited)
				}
			}
			$next
		}
		_ => $roots
	}
}

type_alias_roots_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> List(TypeNamePlan.Root)
type_alias_roots_c = |hosted_functions, provides_list, type_table, ambiguous_names| {
	var $roots = []

	for func in hosted_functions {
		base = hosted_struct_name_c(ambiguous_names, func)
		module_base = hosted_module_name_to_struct_name(func.name)

		var $arg_idx = 0
		for arg_type_id in func.arg_type_ids {
			arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
			$roots = append_type_alias_roots_c($roots, type_table, ambiguous_names, arg_fallback, module_base, arg_type_id, [])
			$arg_idx = $arg_idx + 1
		}

		$roots = append_type_alias_roots_c($roots, type_table, ambiguous_names, base, module_base, func.ret_type_id, [])
	}

	var $position = 0
	for entry in provides_list {
		base = provided_struct_name_c(ambiguous_names, entry, $position)
		module_base = hosted_module_name_to_struct_name(entry.name)
		$position = $position + 1

		match entry.exported {
			ProvidedProcedure(func) => {
				var $arg_idx = 0
				for arg_type_id in func.args {
					arg_fallback = "${base}Arg${U64.to_str($arg_idx)}"
					$roots = append_type_alias_roots_c($roots, type_table, ambiguous_names, arg_fallback, module_base, arg_type_id, [])
					$arg_idx = $arg_idx + 1
				}

				$roots = append_type_alias_roots_c($roots, type_table, ambiguous_names, base, module_base, func.ret, [])
			}
			ProvidedData(type_id) => {
				$roots = append_type_alias_roots_c($roots, type_table, ambiguous_names, base, module_base, type_id, [])
			}
		}
	}

	$roots
}

preferred_type_names_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str) -> TypeNamePlan.PreferredNames
preferred_type_names_c = |hosted_functions, provides_list, type_table, duplicate_names|
	TypeNamePlan.from_table(type_table).preferred_names(
		declared_type_names_c(hosted_functions, provides_list, type_table, duplicate_names),
		type_name_roots_c(hosted_functions, provides_list, type_table, duplicate_names),
		|type_id, struct_name| type_names_declared_as_c(type_table, type_id, struct_name),
	)

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

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Stdout.line!") == "StdoutLine"

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("Foo.bar.baz!") == "FooBarBaz"

## Checks `name_to_struct_name` for this representative case.
expect name_to_struct_name("__AnonStruct10") == "AnonStruct10"

name_to_upper_ident : Str -> Str
name_to_upper_ident = |name| RocName.from_str(name).to_screaming_snake_identifier()

## Checks `name_to_upper_ident` for this representative case.
expect name_to_upper_ident("Stdout.line!") == "STDOUT_LINE"

## Checks `name_to_upper_ident` for this representative case.
expect name_to_upper_ident("Foo.barBaz!") == "FOO_BAR_BAZ"

name_to_c_func_name : Str -> Str
name_to_c_func_name = |name| RocName.from_str(name).to_lower_snake_identifier()

## Checks `name_to_c_func_name` for this representative case.
expect name_to_c_func_name("Stdout.line!") == "stdout_line"

## Checks `name_to_c_func_name` for this representative case.
expect name_to_c_func_name("Foo.barBaz!") == "foo_bar_baz"

name_to_c_field_ident : Str -> Str
name_to_c_field_ident = |name| {
	sanitized =
		RocName.from_str(name).to_bang_snake_identifier()

	match sanitized {
		"" => "field"
		"_" => "field"
		"alignas" => "alignas_field"
		"alignof" => "alignof_field"
		"auto" => "auto_field"
		"bool" => "bool_field"
		"break" => "break_field"
		"case" => "case_field"
		"char" => "char_field"
		"const" => "const_field"
		"constexpr" => "constexpr_field"
		"continue" => "continue_field"
		"default" => "default_field"
		"do" => "do_field"
		"double" => "double_field"
		"else" => "else_field"
		"enum" => "enum_field"
		"extern" => "extern_field"
		"false" => "false_field"
		"float" => "float_field"
		"for" => "for_field"
		"goto" => "goto_field"
		"if" => "if_field"
		"inline" => "inline_field"
		"int" => "int_field"
		"long" => "long_field"
		"nullptr" => "nullptr_field"
		"register" => "register_field"
		"restrict" => "restrict_field"
		"return" => "return_field"
		"short" => "short_field"
		"signed" => "signed_field"
		"sizeof" => "sizeof_field"
		"static" => "static_field"
		"static_assert" => "static_assert_field"
		"struct" => "struct_field"
		"switch" => "switch_field"
		"thread_local" => "thread_local_field"
		"true" => "true_field"
		"typedef" => "typedef_field"
		"typeof" => "typeof_field"
		"typeof_unqual" => "typeof_unqual_field"
		"union" => "union_field"
		"unsigned" => "unsigned_field"
		"void" => "void_field"
		"volatile" => "volatile_field"
		"while" => "while_field"
		_ => sanitized
	}
}

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("init!") == "init_bang"

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("type") == "type"

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("struct") == "struct_field"

## Checks `name_to_c_field_ident` for this representative case.
expect name_to_c_field_ident("true") == "true_field"

# =============================================================================
# Header Generation
# =============================================================================

generate_c_header : List(HostedFunctionInfo), TypeTable, List(ProvidesEntry), List(Str) -> Str
generate_c_header = |hosted_functions, type_table, provides_list, duplicate_names| {
	# Records and tag unions share one namespace, so a Roc name emitted under
	# shape-derived names is emitted that way for both kinds.
	duplicate_records = duplicate_names
	duplicate_tags = duplicate_names
	preferred_names = preferred_type_names_c(hosted_functions, provides_list, type_table, duplicate_names)

	defines = generate_defines(hosted_functions, duplicate_names)
	count = List.len(hosted_functions)
	type_decls = generate_type_decls(type_table, duplicate_records, duplicate_tags, preferred_names)
	type_aliases = generate_platform_type_aliases_c(hosted_functions, provides_list, type_table, duplicate_records, duplicate_tags, preferred_names)
	args_structs = generate_all_args_structs(hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names)
	hosted_fn_fields = generate_hosted_fn_fields(hosted_functions, duplicate_names)
	hosted_symbol_decls = generate_hosted_symbol_decls(hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names)
	provided_symbol_decls = generate_provided_symbol_decls(provides_list, type_table, duplicate_records, duplicate_tags, preferred_names)

	header_guard_top
		.concat(includes_section)
		.concat(extern_c_start)
		.concat(core_types_section)
		.concat(type_decls)
		.concat(type_aliases)
		.concat(hosted_fn_infrastructure)
		.concat(function_count_section(count))
		.concat(defines)
		.concat("\n\n")
		.concat(args_structs_header)
		.concat(args_structs)
		.concat("\n")
		.concat(runtime_symbol_decls)
		.concat(hosted_symbol_decls)
		.concat(provided_symbol_decls)
		.concat(hosted_functions_registry(hosted_fn_fields))
		.concat(extern_c_end)
		.concat(header_guard_bottom)
}

generate_defines : List(HostedFunctionInfo), List(Str) -> Str
generate_defines = |hosted_functions, renamed| {
	var $defines = ""
	var $first = Bool.True
	for func in hosted_functions {
		upper_name = hosted_upper_ident_c(renamed, func)
		if !$first {
			$defines = Str.concat($defines, "\n")
		}
		$defines = Str.concat($defines, "#define HOSTED_IDX_${upper_name} ${U64.to_str(func.index)}")
		$first = Bool.False
	}

	$defines
}

generate_type_decls : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_type_decls = |type_table, duplicate_records, duplicate_tags, preferred_names| {
	type_definitions = generate_opaque_type_decls(type_table, duplicate_records, duplicate_tags, preferred_names)

	if type_definitions == "" {
		""
	} else {
		section("Reflected Roc Types", type_definitions)
	}
}

generate_opaque_type_decls : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_opaque_type_decls = |type_table, duplicate_records, duplicate_tags, preferred_names| {
	var $forwards = ""
	var $seen_names = []
	var $type_id = 0

	for type_info in type_table.entries() {
		match type_info.repr {
			RocRecord(rec) =>
				if rec.name != "" and !type_table.is_zero_sized($type_id) {
					type_name = record_struct_name(type_table, duplicate_records, $type_id, rec)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$forwards = Str.concat($forwards, "typedef struct ${type_name} ${type_name};\n")
					}
				}
			RocTagUnion(tu) =>
				if declares_tag_union_struct_c(type_table, $type_id, tu) {
					type_name = tag_union_struct_name(type_table, preferred_names, duplicate_tags, $type_id, tu)
					if !(List.contains($seen_names, type_name)) {
						$seen_names = $seen_names.append(type_name)
						$forwards = Str.concat($forwards, "typedef struct ${type_name} ${type_name};\n")
					}
				}
			_ => {}
		}
		$type_id = $type_id + 1
	}

	var $state = { content: "", seen_names: [] }
	$type_id = 0
	for _type_info in type_table.entries() {
		$state = generate_type_decl_c($state, type_table, duplicate_records, duplicate_tags, preferred_names, $type_id)
		$type_id = $type_id + 1
	}

	if $forwards == "" {
		$state.content
	} else {
		Str.concat($forwards, "\n${$state.content}")
	}
}

generate_type_decl_c : { content : Str, seen_names : List(Str) }, TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64 -> { content : Str, seen_names : List(Str) }
generate_type_decl_c = |state, type_table, duplicate_records, duplicate_tags, preferred_names, type_id| {
	type_info = type_table.type_info(type_id)
	match type_info.repr {
		RocRecord(rec) =>
			if rec.name == "" or type_table.is_zero_sized(type_id) {
				state
			} else {
				type_name = record_struct_name(type_table, duplicate_records, type_id, rec)
				if List.contains(state.seen_names, type_name) {
					state
				} else {
					var $next = { content: state.content, seen_names: state.seen_names.append(type_name) }
					for field in rec.fields {
						if !field.is_padding {
							$next = generate_type_decl_c($next, type_table, duplicate_records, duplicate_tags, preferred_names, field.type_id)
						}
					}
					{
						content: Str.concat($next.content, generate_record_type_decl(type_table, duplicate_records, duplicate_tags, preferred_names, type_name, type_info.layout)),
						seen_names: $next.seen_names,
					}
				}
			}
		RocTagUnion(tu) =>
			match type_table.single_variant_payload(type_id, tu) {
				SinglePayload(payload_id) => generate_type_decl_c(state, type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
				SinglePayloadStruct(tag_layout) => {
					type_name = tag_union_struct_name(type_table, preferred_names, duplicate_tags, type_id, tu)
					if List.contains(state.seen_names, type_name) {
						state
					} else {
						var $next = { content: state.content, seen_names: state.seen_names.append(type_name) }
						for payload_id in tag_layout.payload {
							$next = generate_type_decl_c($next, type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
						}
						{
							content: Str.concat($next.content, generate_struct_type_decl(type_table, duplicate_records, duplicate_tags, preferred_names, type_name, tag_layout.payload_fields, type_info.layout)),
							seen_names: $next.seen_names,
						}
					}
				}
				SingleNoPayload => state
				NotSingleVariant =>
					if tu.name == "" {
						state
					} else {
						type_name = tag_union_struct_name(type_table, preferred_names, duplicate_tags, type_id, tu)
						if List.contains(state.seen_names, type_name) {
							state
						} else {
							var $next = { content: state.content, seen_names: state.seen_names.append(type_name) }
							for tag in tu.tags {
								for payload_id in tag.payload {
									$next = generate_type_decl_c($next, type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
								}
							}
							{
								content: Str.concat($next.content, generate_tag_union_type_decl(type_table, duplicate_records, duplicate_tags, preferred_names, type_id, tu, type_info.layout)),
								seen_names: $next.seen_names,
							}
						}
					}
				}
		_ => state
	}
}

abi_tag_layouts_c : AbiLayout -> List(AbiTagLayout)
abi_tag_layouts_c = |abi_layout| abi_layout.tag_layouts()

abi_tag_at_c : List(AbiTagLayout), U64 -> AbiTagLayout
abi_tag_at_c = |abi_tags, index|
	match List.get(abi_tags, index) {
		Ok(tag) => tag
		Err(_) => {
			crash "glue invariant violated: missing C ABI tag layout at index ${U64.to_str(index)}"
		}
	}

abi_tag_has_payload_c : AbiTagLayout -> Bool
abi_tag_has_payload_c = |tag| tag.payload_size32 > 0 or tag.payload_size64 > 0

c_discriminant_type : U64 -> Str
c_discriminant_type = |size|
	if size <= 1 {
		"uint8_t"
	} else if size == 2 {
		"uint16_t"
	} else if size == 4 {
		"uint32_t"
	} else {
		"uint64_t"
	}

c_payload_layout_assertions : Str, AbiTagLayout, AbiWidth -> Str
c_payload_layout_assertions = |type_name, tag_layout, width| {
	size = match width {
		Pointer32 => tag_layout.payload_size32
		Pointer64 => tag_layout.payload_size64
	}
	alignment = match width {
		Pointer32 => tag_layout.payload_alignment32
		Pointer64 => tag_layout.payload_alignment64
	}
	fields = tag_layout.payload_fields
	Str.concat(static_asserts(type_name, size, alignment), c_record_offset_assertions(type_name, field_idents_c(field_names_c(fields)), fields, width))
}

generate_payload_type_decl_c : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, Str, AbiTagLayout -> Str
generate_payload_type_decl_c = |type_table, duplicate_records, duplicate_tags, preferred_names, type_name, tag_layout| {
	wrap_vector_members = c_needs_vector_union_spelling(type_table, tag_layout.payload_fields)
	field_idents = field_idents_c(field_names_c(tag_layout.payload_fields))
	fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, field_idents, tag_layout.payload_fields, Pointer64, wrap_vector_members)
	fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, field_idents, tag_layout.payload_fields, Pointer32, wrap_vector_members)
	assertions64 = c_payload_layout_assertions(type_name, tag_layout, Pointer64)
	assertions32 = c_payload_layout_assertions(type_name, tag_layout, Pointer32)
	comment = if wrap_vector_members {
		"${c_vector_union_comment}\n"
	} else {
		""
	}
	decl =
		\\#if UINTPTR_MAX == UINT64_MAX
		\\typedef struct {
		\\${fields64}} ${type_name};
		\\${assertions64}#else
		\\typedef struct {
		\\${fields32}} ${type_name};
		\\${assertions32}#endif
	"${comment}${decl}\n\n"
}

c_tag_union_layout_assertions : Str, AbiLayout, AbiWidth -> Str
c_tag_union_layout_assertions = |type_name, abi_layout, width| {
	size = AbiLayout.size(abi_layout, width)
	alignment = AbiLayout.alignment(abi_layout, width)
	tag_offset = abi_layout.discriminant_offset(width)
	Str.concat(static_asserts(type_name, size, alignment), "ROC_STATIC_ASSERT(offsetof(${type_name}, tag) == ${U64.to_str(tag_offset)}, \"${type_name}.tag offset mismatch\");\n")
}

generate_tag_union_type_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, U64, TagUnionRepr, AbiLayout -> Str
generate_tag_union_type_decl = |type_table, duplicate_records, duplicate_tags, preferred_names, type_id, tu, abi_layout| {
	tag_names = tag_names_c(tu)
	tag_constants = tag_constants_c(tag_names)
	tag_fields = field_idents_c(tag_names)
	struct_name = tag_union_struct_name(type_table, preferred_names, duplicate_tags, type_id, tu)
	disc_type = c_discriminant_type(abi_layout.discriminant_size())
	abi_tags = abi_tag_layouts_c(abi_layout)
	is_pure_enum = !abi_layout.tag_union_has_payload()

	var $tag_constants = ""
	var $tag_index = 0
	for _tag in tu.tags {
		separator = if $tag_index == 0 {
			""
		} else {
			","
		}
		constant_name = tag_constants.at($tag_index)
		$tag_constants = Str.concat($tag_constants, "${separator}\n    ${struct_name}Tag_${constant_name} = ${U64.to_str($tag_index)}")
		$tag_index = $tag_index + 1
	}
	$tag_constants = Str.concat($tag_constants, "\n")

	if is_pure_enum {
		decl =
			\\typedef ${disc_type} ${struct_name};
			\\enum {${$tag_constants}};
			\\${static_asserts(struct_name, abi_layout.size64, abi_layout.alignment64)}
		"${decl}\n"
	} else {
		var $payload_structs = ""
		var $union_fields = ""
		var $constructors = ""
		var $accessors = ""
		var $variant_index = 0
		for tag in tu.tags {
			tag_layout = abi_tag_at_c(abi_tags, $variant_index)
			field_name = tag_fields.at($variant_index)
			constant_name = tag_constants.at($variant_index)
			has_payload = abi_tag_has_payload_c(tag_layout)
			if !has_payload {
				$union_fields = Str.concat($union_fields, "    uint8_t ${field_name};\n")
				$constructors = Str.concat($constructors, "static inline ${struct_name} ${struct_name}_make_${field_name}(void) {\n    ${struct_name} out = {0};\n    out.tag = ${struct_name}Tag_${constant_name};\n    return out;\n}\n\n")
			} else {
				payload_type = if List.len(tag.payload) == 1 {
					match List.first(tag.payload) {
						Ok(payload_id) => type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, payload_id)
						Err(_) => {
							crash "glue invariant violated: C single-payload tag had no payload"
						}
					}
				} else {
					tuple_name = "${struct_name}${constant_name}Payload"
					$payload_structs = Str.concat($payload_structs, generate_payload_type_decl_c(type_table, duplicate_records, duplicate_tags, preferred_names, tuple_name, tag_layout))
					tuple_name
				}
				$union_fields = Str.concat($union_fields, "    ${payload_type} ${field_name};\n")
				constructor =
					\\static inline ${struct_name} ${struct_name}_make_${field_name}(${payload_type} payload) {
					\\    ${struct_name} out = {0};
					\\    out.tag = ${struct_name}Tag_${constant_name};
					\\#if UINTPTR_MAX == UINT64_MAX
					\\    out.payload.${field_name} = payload;
					\\#else
					\\    roc_abi_copy_bytes(out.payload, &payload, sizeof(payload));
					\\#endif
					\\    return out;
					\\}
				accessor =
					\\static inline ${payload_type} ${struct_name}_payload_${field_name}(const ${struct_name}* value) {
					\\#if UINTPTR_MAX == UINT64_MAX
					\\    return value->payload.${field_name};
					\\#else
					\\    ${payload_type} payload;
					\\    roc_abi_copy_bytes(&payload, value->payload, sizeof(payload));
					\\    return payload;
					\\#endif
					\\}
				$constructors = Str.concat($constructors, "${constructor}\n\n")
				$accessors = Str.concat($accessors, "${accessor}\n\n")
			}
			$variant_index = $variant_index + 1
		}

		assertions64 = c_tag_union_layout_assertions(struct_name, abi_layout, Pointer64)
		assertions32 = c_tag_union_layout_assertions(struct_name, abi_layout, Pointer32)
		payload_union_name = "${struct_name}TagPayloadStorage"
		decl =
			\\${$payload_structs}typedef ${disc_type} ${struct_name}Tag;
			\\enum {${$tag_constants}};
			\\typedef union {
			\\${$union_fields}} ${payload_union_name};
			\\#if UINTPTR_MAX == UINT64_MAX
			\\struct ${struct_name} {
			\\    ${payload_union_name} payload;
			\\    ${struct_name}Tag tag;
			\\};
			\\${assertions64}#else
			\\struct ${struct_name} {
			\\    ROC_ALIGNAS(${U64.to_str(abi_layout.alignment32)}) uint8_t payload[${U64.to_str(abi_layout.discriminant_offset(Pointer32))}];
			\\    ${struct_name}Tag tag;
			\\};
			\\${assertions32}#endif
		"${decl}\n${$constructors}${$accessors}"
	}
}

abi_record_fields_c : AbiLayout -> List(AbiFieldLayout)
abi_record_fields_c = |abi_layout| abi_layout.record_fields()

c_record_offset_assertions : Str, TypeNamePlan.MemberIdents, List(AbiFieldLayout), AbiWidth -> Str
c_record_offset_assertions = |type_name, field_idents, fields, width| {
	var $assertions = ""
	var $member_index = 0
	for field in fields {
		member_name = if field.is_padding {
			""
		} else {
			field_idents.at($member_index)
		}
		if !field.is_padding {
			$member_index = $member_index + 1
		}

		if !field.is_padding and !field.is_zero_sized() {
			field_name = member_name
			offset = AbiFieldLayout.offset(field, width)
			$assertions = Str.concat($assertions, "ROC_STATIC_ASSERT(offsetof(${type_name}, ${field_name}) == ${U64.to_str(offset)}, \"${type_name}.${field_name} offset mismatch\");\n")
		}
	}
	$assertions
}

generate_record_type_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, Str, AbiLayout -> Str
generate_record_type_decl = |type_table, duplicate_records, duplicate_tags, preferred_names, type_name, abi_layout| {
	fields = abi_record_fields_c(abi_layout)
	if List.is_empty(fields) {
		return generate_opaque_type_decl(type_name, abi_layout.size64, abi_layout.alignment64, abi_layout.size32, abi_layout.alignment32)
	}

	generate_struct_type_decl(type_table, duplicate_records, duplicate_tags, preferred_names, type_name, fields, abi_layout)
}

## Define `struct type_name` from its committed fields, which are a record's
## fields or the payloads of a single-variant tag union.
generate_struct_type_decl : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, Str, List(AbiFieldLayout), AbiLayout -> Str
generate_struct_type_decl = |type_table, duplicate_records, duplicate_tags, preferred_names, type_name, fields, abi_layout| {
	wrap_vector_members = c_needs_vector_union_spelling(type_table, fields)
	field_idents = field_idents_c(field_names_c(fields))
	fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, field_idents, fields, Pointer64, wrap_vector_members)
	fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, field_idents, fields, Pointer32, wrap_vector_members)
	assertions64 = Str.concat(static_asserts(type_name, abi_layout.size64, abi_layout.alignment64), c_record_offset_assertions(type_name, field_idents, fields, Pointer64))
	assertions32 = Str.concat(static_asserts(type_name, abi_layout.size32, abi_layout.alignment32), c_record_offset_assertions(type_name, field_idents, fields, Pointer32))
	comment = if wrap_vector_members {
		"${c_vector_union_comment}\n"
	} else {
		""
	}
	decl =
		\\#if UINTPTR_MAX == UINT64_MAX
		\\struct ${type_name} {
		\\${fields64}};
		\\${assertions64}#else
		\\struct ${type_name} {
		\\${fields32}};
		\\${assertions32}#endif
	"${comment}${decl}\n\n"
}

generate_opaque_type_decl : Str, U64, U64, U64, U64 -> Str
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
		\\struct ${type_name} {
		\\    ROC_ALIGNAS(${U64.to_str(type_alignment64)}) uint8_t bytes[${U64.to_str(byte_count64)}];
		\\};
		\\${static_asserts(type_name, size64, alignment64)}#else
		\\struct ${type_name} {
		\\    ROC_ALIGNAS(${U64.to_str(type_alignment32)}) uint8_t bytes[${U64.to_str(byte_count32)}];
		\\};
		\\${static_asserts(type_name, size32, alignment32)}#endif
	"${decl}\n\n"
}

static_asserts : Str, U64, U64 -> Str
static_asserts = |type_name, size, alignment|
	if size > 0 {
		"ROC_STATIC_ASSERT(sizeof(${type_name}) == ${U64.to_str(size)}, \"${type_name} size mismatch\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(${type_name}) == ${U64.to_str(alignment)}, \"${type_name} alignment mismatch\");\n"
	} else {
		""
	}

add_type_alias_c : { content : Str, seen : List(Str) }, Str, Str -> { content : Str, seen : List(Str) }
add_type_alias_c = |state, alias, target| {
	if alias == target or List.contains(state.seen, alias) {
		state
	} else {
		{
			content: Str.concat(state.content, "typedef ${target} ${alias};\n"),
			seen: state.seen.append(alias),
		}
	}
}

generate_platform_type_aliases_c : List(HostedFunctionInfo), List(ProvidesEntry), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_platform_type_aliases_c = |hosted_functions, provides_list, type_table, duplicate_records, duplicate_tags, preferred_names| {
	var $state = { content: "", seen: preferred_names.declared_names() }
	name_plan = TypeNamePlan.from_table(type_table)

	for plan in name_plan.alias_plan(type_alias_roots_c(hosted_functions, provides_list, type_table, List.concat(duplicate_records, duplicate_tags))) {
		target = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, plan.type_id)
		$state = add_type_alias_c($state, plan.alias, target)
	}

	if $state.content == "" {
		""
	} else {
		section("Platform Type Aliases", $state.content)
	}
}

generate_all_args_structs : List(HostedFunctionInfo), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_all_args_structs = |hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names| {
	var $args_structs = ""
	for func in hosted_functions {
		$args_structs = Str.concat($args_structs, generate_args_struct(func, type_table, duplicate_records, duplicate_tags, preferred_names))
	}
	$args_structs
}

generate_args_struct : HostedFunctionInfo, TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_args_struct = |func, type_table, duplicate_records, duplicate_tags, preferred_names| {
	struct_name = hosted_struct_name_c(duplicate_records, func)
	arg_shape = ArgShape.from_table(type_table)

	match arg_shape.hosted_args(func) {
		NoMeaningfulArgs => ""
		SingleRecordArg(record) => {
			wrap_vector_members = c_needs_vector_union_spelling(type_table, record.fields)
			field_idents = field_idents_c(field_names_c(record.fields))
			fields64 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, field_idents, record.fields, Pointer64, wrap_vector_members)
			fields32 = c_record_fields_decl(type_table, duplicate_records, duplicate_tags, preferred_names, field_idents, record.fields, Pointer32, wrap_vector_members)

			doc = doc_comment([
				"Arguments for ${func.name}",
				"Roc signature: ${func.type_str}",
				"Refcounted fields are owned by the hosted function.",
			])

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
				c_type = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_id)
				$positional_fields = Str.concat($positional_fields, "    ${c_type} arg${U64.to_str($idx)};\n")
				$idx = $idx + 1
			}

			doc = doc_comment([
				"Arguments for ${func.name}",
				"Roc signature: ${func.type_str}",
				"Refcounted fields are owned by the hosted function.",
			])

			"${doc}typedef struct {\n${$positional_fields}} ${struct_name}Args;\n\n"
		}
	}
}

direct_param_list : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, List(U64) -> Str
direct_param_list = |type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_ids| {
	var $params = ""
	var $idx = 0
	arg_shape = ArgShape.from_table(type_table)

	for arg_type_id in arg_shape.abi_visible_type_ids(arg_type_ids) {
		arg_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_id)
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

direct_hosted_param_list : TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames, HostedFunctionInfo -> Str
direct_hosted_param_list = |type_table, duplicate_records, duplicate_tags, preferred_names, func| {
	arg_shape = ArgShape.from_table(type_table)
	use_args_wrapper = arg_shape.single_arg_is_anonymous_record(func.arg_type_ids)

	var $params = ""
	var $idx = 0

	for arg_type_id in arg_shape.abi_visible_type_ids(func.arg_type_ids) {
		arg_c = if use_args_wrapper {
			"${hosted_struct_name_c(duplicate_records, func)}Args"
		} else {
			type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, arg_type_id)
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

## Declarations for the fixed runtime symbols every host defines.
runtime_symbol_decls : Str
runtime_symbol_decls =
	section(
		"Runtime Symbols",
		"/* The host defines these linker symbols. Compiled Roc code calls them directly. */\n\n#if defined(__GNUC__) || defined(__clang__)\n#define ROC_RETURNS_NONNULL __attribute__((returns_nonnull))\n#else\n#define ROC_RETURNS_NONNULL\n#endif\n\n/* Returns `length` bytes aligned to `alignment`. Never returns NULL: Roc writes through\n   the result without checking it, so a host that cannot allocate must stop the Roc\n   program and not return, exactly as roc_crashed does. */\nROC_RETURNS_NONNULL void* roc_alloc(size_t length, size_t alignment);\nvoid roc_dealloc(void* ptr, size_t alignment);\n/* Never returns NULL, under the same rule as roc_alloc. */\nROC_RETURNS_NONNULL void* roc_realloc(void* ptr, size_t new_length, size_t alignment);\nvoid roc_dbg(const uint8_t* bytes, size_t len);\nvoid roc_expect_failed(const uint8_t* bytes, size_t len);\nvoid roc_crashed(const uint8_t* bytes, size_t len);\n",
	)

generate_hosted_symbol_decls : List(HostedFunctionInfo), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_hosted_symbol_decls = |hosted_functions, type_table, duplicate_records, duplicate_tags, preferred_names| {
	if List.is_empty(hosted_functions) {
		return ""
	}

	var $decls = ""
	for func in hosted_functions {
		params = direct_hosted_param_list(type_table, duplicate_records, duplicate_tags, preferred_names, func)
		ret_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, func.ret_type_id)
		$decls = Str.concat($decls, "/* ${func.name}: ${func.type_str} */\nextern ${ret_c} ${func.ffi_symbol}(${params});\n\n")
	}

	section("Hosted Symbols", $decls)
}

generate_provided_symbol_decls : List(ProvidesEntry), TypeTable, List(Str), List(Str), TypeNamePlan.PreferredNames -> Str
generate_provided_symbol_decls = |provides_list, type_table, duplicate_records, duplicate_tags, preferred_names| {
	if List.is_empty(provides_list) {
		return ""
	}

	var $decls = ""
	for entry in provides_list {
		match entry.exported {
			ProvidedProcedure(func) => {
				params = direct_param_list(type_table, duplicate_records, duplicate_tags, preferred_names, func.args)
				ret_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, func.ret)
				$decls = Str.concat($decls, "/* Entrypoint: ${entry.name} */\nextern ${ret_c} ${entry.ffi_symbol}(${params});\n\n")
			}
			ProvidedData(type_id) => {
				value_c = type_id_to_c(type_table, duplicate_records, duplicate_tags, preferred_names, type_id)
				$decls = Str.concat($decls, "/* Static provided value: ${entry.name} */\nextern const ${value_c} ${entry.ffi_symbol};\n\n")
			}
		}
	}

	section("Provided Symbols", $decls)
}

generate_hosted_fn_fields : List(HostedFunctionInfo), List(Str) -> Str
generate_hosted_fn_fields = |hosted_functions, renamed| {
	var $fields = ""
	var $first = Bool.True

	for f in hosted_functions {
		field_name = hosted_field_ident_c(renamed, f)
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
	header_doc = doc_comment([
		"Roc Platform ABI Header",
		"",
		"This file defines C declarations for a Roc platform's direct symbol ABI.",
		"It is automatically generated by the Roc glue generator.",
		"",
		"Hosted argument ownership:",
		"Roc transfers ownership of refcounted arguments to the hosted function.",
		"The hosted function must release owned refcounted arguments when done,",
		"or retain/transfer ownership explicitly when storing or returning them.",
		"Releasing a container releases its elements only when the container's own",
		"count reaches zero, which is what compiled Roc code does when it drops one",
		"it owns. Releasing the elements unconditionally double-frees them whenever",
		"the Roc caller still holds the container.",
		"",
		"Opaque values:",
		"A RocBox whose payload type the application chooses, and a RocOpaqueList,",
		"refer to values whose layout this header cannot describe. The host may",
		"store them, retain them, and pass them back to Roc. It cannot release",
		"them, and neither can it release a record, tag union, box, or list that",
		"holds one: the allocation header and the payload teardown both depend on",
		"the missing layout. Pass the value to a Roc function that takes ownership",
		"of it instead.",
	])

	"${header_doc}\n#ifndef ROC_PLATFORM_ABI_H\n#define ROC_PLATFORM_ABI_H\n\n"
}

includes_section : Str
includes_section =
	"#include <stdbool.h>\n#include <stddef.h>\n#include <stdint.h>\n\n#if defined(__cplusplus)\n#define ROC_ALIGNAS(n) alignas(n)\n#define ROC_ALIGNOF(T) alignof(T)\n#define ROC_STATIC_ASSERT(cond, message) static_assert(cond, message)\n#else\n#define ROC_ALIGNAS(n) _Alignas(n)\n#define ROC_ALIGNOF(T) _Alignof(T)\n#define ROC_STATIC_ASSERT(cond, message) _Static_assert(cond, message)\n#endif\n\nstatic inline void roc_abi_copy_bytes(void* dst, const void* src, size_t count) {\n    uint8_t* dst_bytes = (uint8_t*)dst;\n    const uint8_t* src_bytes = (const uint8_t*)src;\n    for (size_t index = 0; index < count; index++) {\n        dst_bytes[index] = src_bytes[index];\n    }\n}\n\n"

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
	roc_vector_defs = "#if defined(_MSC_VER) && defined(_M_X64)\n#include <intrin.h>\ntypedef __m128i RocU8x16;\ntypedef __m128i RocI8x16;\ntypedef __m128i RocU16x8;\ntypedef __m128i RocI16x8;\ntypedef __m128i RocU32x4;\ntypedef __m128i RocI32x4;\ntypedef __m128i RocU64x2;\ntypedef __m128i RocI64x2;\n#elif defined(_M_ARM64)\n#include <arm64_neon.h>\ntypedef uint8x16_t RocU8x16;\ntypedef int8x16_t RocI8x16;\ntypedef uint16x8_t RocU16x8;\ntypedef int16x8_t RocI16x8;\ntypedef uint32x4_t RocU32x4;\ntypedef int32x4_t RocI32x4;\ntypedef uint64x2_t RocU64x2;\ntypedef int64x2_t RocI64x2;\n#elif defined(__aarch64__)\n#include <arm_neon.h>\ntypedef uint8x16_t RocU8x16;\ntypedef int8x16_t RocI8x16;\ntypedef uint16x8_t RocU16x8;\ntypedef int16x8_t RocI16x8;\ntypedef uint32x4_t RocU32x4;\ntypedef int32x4_t RocI32x4;\ntypedef uint64x2_t RocU64x2;\ntypedef int64x2_t RocI64x2;\n#elif defined(__wasm_simd128__)\n#include <wasm_simd128.h>\ntypedef v128_t RocU8x16;\ntypedef v128_t RocI8x16;\ntypedef v128_t RocU16x8;\ntypedef v128_t RocI16x8;\ntypedef v128_t RocU32x4;\ntypedef v128_t RocI32x4;\ntypedef v128_t RocU64x2;\ntypedef v128_t RocI64x2;\n#else\ntypedef uint8_t RocU8x16 __attribute__((vector_size(16)));\ntypedef int8_t RocI8x16 __attribute__((vector_size(16)));\ntypedef uint16_t RocU16x8 __attribute__((vector_size(16)));\ntypedef int16_t RocI16x8 __attribute__((vector_size(16)));\ntypedef uint32_t RocU32x4 __attribute__((vector_size(16)));\ntypedef int32_t RocI32x4 __attribute__((vector_size(16)));\ntypedef uint64_t RocU64x2 __attribute__((vector_size(16)));\ntypedef int64_t RocI64x2 __attribute__((vector_size(16)));\n#endif\n\n#define ROC_ASSERT_VECTOR(T) ROC_STATIC_ASSERT(sizeof(T) == 16, #T \" size mismatch\"); ROC_STATIC_ASSERT(ROC_ALIGNOF(T) == 16, #T \" alignment mismatch\")\nROC_ASSERT_VECTOR(RocU8x16);\nROC_ASSERT_VECTOR(RocI8x16);\nROC_ASSERT_VECTOR(RocU16x8);\nROC_ASSERT_VECTOR(RocI16x8);\nROC_ASSERT_VECTOR(RocU32x4);\nROC_ASSERT_VECTOR(RocI32x4);\nROC_ASSERT_VECTOR(RocU64x2);\nROC_ASSERT_VECTOR(RocI64x2);\n#undef ROC_ASSERT_VECTOR\n\n"
	roc_vector_defs_clang_x64 = RocName.replace_all(roc_vector_defs, "#if defined(_MSC_VER) && defined(_M_X64)", "#if defined(_MSC_VER) && !defined(__clang__) && defined(_M_X64)")
	roc_vector_defs_portable = RocName.replace_all(roc_vector_defs_clang_x64, "#elif defined(_M_ARM64)", "#elif defined(_MSC_VER) && !defined(__clang__) && defined(_M_ARM64)")

	roc_str_def = "typedef struct {\n    uint8_t* bytes;\n    size_t capacity_or_alloc_ptr;\n    size_t length;\n} RocStr;\n\nROC_STATIC_ASSERT(sizeof(RocStr) == 3 * sizeof(size_t), \"RocStr must be three pointer-sized words\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocStr) == ROC_ALIGNOF(size_t), \"RocStr must be pointer-word aligned\");\nROC_STATIC_ASSERT(offsetof(RocStr, bytes) == 0, \"RocStr.bytes offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocStr, capacity_or_alloc_ptr) == sizeof(size_t), \"RocStr.capacity_or_alloc_ptr offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocStr, length) == 2 * sizeof(size_t), \"RocStr.length offset mismatch\");\n\n"

	roc_list_def = "typedef struct {\n    void* elements;\n    size_t length;\n    size_t capacity_or_alloc_ptr;\n} RocList;\n\nROC_STATIC_ASSERT(sizeof(RocList) == 3 * sizeof(size_t), \"RocList must be three pointer-sized words\");\nROC_STATIC_ASSERT(ROC_ALIGNOF(RocList) == ROC_ALIGNOF(size_t), \"RocList must be pointer-word aligned\");\nROC_STATIC_ASSERT(offsetof(RocList, elements) == 0, \"RocList.elements offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocList, length) == sizeof(size_t), \"RocList.length offset mismatch\");\nROC_STATIC_ASSERT(offsetof(RocList, capacity_or_alloc_ptr) == 2 * sizeof(size_t), \"RocList.capacity_or_alloc_ptr offset mismatch\");\n\n/* A list whose items are, or hold, values of a type the application chooses.\n * The host cannot allocate or release one; see \"Opaque values\" above. */\ntypedef RocList RocOpaqueList;\n\n"

	roc_box_def = "typedef void* RocBox;\n\n"

	erased_callable_def =
		"struct RocOps;\n\n"
			.concat("/* `reuse` is nullable. Non-null must be the callable data pointer whose inline capture begins at `capture`; it transfers one owned reference to the callee. The caller must not use or decref that ownership unit after the call. The callee consumes it exactly once, whether or not the result can reuse the allocation. */\n")
			.concat("typedef void (*RocErasedCallableFn)(struct RocOps* ops, uint8_t* ret, const uint8_t* args, uint8_t* capture, uint8_t* reuse, const void** ret_desc);\n")
			.concat("typedef void (*RocErasedCallableOnDrop)(uint8_t* capture, struct RocOps* ops);\n")
			.concat("typedef struct {\n    RocErasedCallableFn callable_fn_ptr;\n    RocErasedCallableOnDrop on_drop;\n    void (*on_copy)(uint8_t* capture, intptr_t amount);\n    size_t capture_size;\n} RocErasedCallablePayload;\n")
			.concat("ROC_STATIC_ASSERT(offsetof(RocErasedCallablePayload, callable_fn_ptr) == 0, \"RocErasedCallablePayload.callable_fn_ptr offset mismatch\");\n")
			.concat("ROC_STATIC_ASSERT(offsetof(RocErasedCallablePayload, on_drop) == sizeof(RocErasedCallableFn), \"RocErasedCallablePayload.on_drop offset mismatch\");\n")
			.concat("typedef uint8_t* RocErasedCallable;\n")
			.concat("#define ROC_ERASED_CALLABLE_CAPTURE_ALIGNMENT 16u\n")
			.concat("#define ROC_ERASED_CALLABLE_PAYLOAD_ALIGNMENT 16u\n")
			.concat("#define ROC_ERASED_CALLABLE_CAPTURE_OFFSET ((sizeof(RocErasedCallablePayload) + 15u) & ~15u)\n")
			.concat("#define ROC_ERASED_CALLABLE_PAYLOAD_SIZE(capture_size) (ROC_ERASED_CALLABLE_CAPTURE_OFFSET + (capture_size))\n")
			.concat("static inline RocErasedCallablePayload* roc_erased_callable_payload_ptr(RocErasedCallable callable) {\n    return (RocErasedCallablePayload*)callable;\n}\n")
			.concat("static inline uint8_t* roc_erased_callable_capture_ptr(RocErasedCallable callable) {\n    return callable == 0 ? 0 : callable + ROC_ERASED_CALLABLE_CAPTURE_OFFSET;\n}\n\n")

	section("Core Roc Types", "${roc_dec_def}${roc_vector_defs_portable}${roc_str_def}${roc_list_def}${roc_box_def}${erased_callable_def}")
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

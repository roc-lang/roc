app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.Shape exposing [Shape, RocFn]
import pf.File exposing [File]
import pf.TypeId exposing [TypeId]
import "../static/Cargo.toml" as roc_app_cargo_toml : Str
import "../../roc_std/Cargo.toml" as roc_std_cargo_toml : Str
import "../../roc_std/src/lib.rs" as roc_std_lib : Str
import "../../roc_std/src/roc_box.rs" as roc_std_box : Str
import "../../roc_std/src/roc_list.rs" as roc_std_list : Str
import "../../roc_std/src/roc_str.rs" as roc_std_str : Str
import "../../roc_std/src/storage.rs" as roc_std_storage : Str

make_glue : List Types -> Result (List File) Str
make_glue = \types_by_arch ->
    mod_file_content =
        List.walk(types_by_arch, file_header, \content, types ->
            arch = (Types.target(types)).architecture
            arch_str = arch_name(arch)

            Str.concat(
                content,
                """
                #[cfg(target_arch = "${arch_str}")]
                mod ${arch_str};
                #[cfg(target_arch = "${arch_str}")]
                pub use ${arch_str}::*;

                """,
            ))

    types_by_arch
    |> List.map(convert_types_to_file)
    |> List.append({ name: "roc_app/src/lib.rs", content: mod_file_content })
    |> List.concat(static_files)
    |> Ok

## These are always included, and don't depend on the specifics of the app.
static_files : List File
static_files = [
    { name: "roc_app/Cargo.toml", content: roc_app_cargo_toml },
    { name: "roc_std/Cargo.toml", content: roc_std_cargo_toml },
    { name: "roc_std/src/lib.rs", content: roc_std_lib },
    { name: "roc_std/src/roc_box.rs", content: roc_std_box },
    { name: "roc_std/src/roc_list.rs", content: roc_std_list },
    { name: "roc_std/src/roc_str.rs", content: roc_std_str },
    { name: "roc_std/src/storage.rs", content: roc_std_storage },
]

convert_types_to_file : Types -> File
convert_types_to_file = \types ->
    content =
        Types.walk_shapes(types, file_header, \buf, type, id ->
            when type is
                Struct({ name, fields }) ->
                    generate_struct(buf, types, id, name, fields, Public)

                TagUnionPayload({ name, fields }) ->
                    generate_struct(buf, types, id, name, name_tag_union_payload_fields(fields), Public)

                TagUnion(Enumeration({ name, tags, size })) ->
                    generate_enumeration(buf, types, type, name, tags, size)

                TagUnion(NonRecursive({ name, tags, discriminant_size, discriminant_offset })) ->
                    if !(List.is_empty(tags)) then
                        generate_non_recursive_tag_union(buf, types, id, name, tags, discriminant_size, discriminant_offset)
                    else
                        buf

                TagUnion(Recursive({ name, tags, discriminant_size, discriminant_offset })) ->
                    if !(List.is_empty(tags)) then
                        generate_recursive_tag_union(buf, types, id, name, tags, discriminant_size, discriminant_offset, None)
                    else
                        buf

                TagUnion(NullableWrapped({ name, index_of_null_tag, tags, discriminant_size, discriminant_offset })) ->
                    # TODO: generate this as `TypeName(*mut u8)` if the payload contains functions / unsized types
                    generate_recursive_tag_union(buf, types, id, name, tags, discriminant_size, discriminant_offset, Some(index_of_null_tag))

                TagUnion(NullableUnwrapped({ name, null_tag, non_null_tag, non_null_payload, which_tag_is_null })) ->
                    generate_nullable_unwrapped(buf, types, id, name, null_tag, non_null_tag, non_null_payload, which_tag_is_null)

                TagUnion(SingleTagStruct({ name, tag_name, payload })) ->
                    generate_single_tag_struct(buf, types, name, tag_name, payload)

                TagUnion(NonNullableUnwrapped({ name, tag_name, payload })) ->
                    generate_non_nullable_unwrapped(buf, types, name, tag_name, payload, 0, 0, None)

                Function(roc_fn) ->
                    if roc_fn.is_toplevel then
                        buf
                    else
                        generate_function(buf, types, roc_fn)

                RecursivePointer(_) ->
                    # This is recursively pointing to a type that should already have been added,
                    # so no extra work needs to happen.
                    buf

                Unit
                | Unsized
                | EmptyTagUnion
                | Num(_)
                | Bool
                | RocResult(_, _)
                | RocStr
                | RocDict(_, _)
                | RocSet(_)
                | RocList(_)
                | RocBox(_) ->
                    # These types don't need to be declared in Rust.
                    # TODO: Eventually we want to generate roc_std. So these types will need to be emitted.
                    buf)

    arch = (Types.target(types)).architecture
    arch_str = arch_name(arch)

    {
        name: "roc_app/src/${arch_str}.rs",
        content: content |> generate_entry_points(types),
    }

generate_entry_points : Str, Types -> Str
generate_entry_points = \buf, types ->
    List.walk(Types.entry_points(types), buf, \accum, T(name, id) -> generate_entry_point(accum, types, name, id))

generate_entry_point : Str, Types, Str, TypeId -> Str
generate_entry_point = \buf, types, name, id ->
    public_signature =
        when Types.shape(types, id) is
            Function(roc_fn) ->
                arguments =
                    to_arg_str(roc_fn.args, types, \arg_id, _shape, index ->
                        type = type_name(types, arg_id)
                        index_str = Num.to_str(index)

                        "arg${index_str}: ${type}")

                ret = type_name(types, roc_fn.ret)

                "(${arguments}) -> ${ret}"

            _ ->
                ret = type_name(types, id)
                "() -> ${ret}"

    (extern_signature, return_type_name, returns_fn) =
        when Types.shape(types, id) is
            Function(roc_fn) ->
                arguments =
                    to_arg_str(roc_fn.args, types, \arg_id, shape, _index ->
                        type = type_name(types, arg_id)

                        if can_derive_copy(types, shape) then
                            "_: ${type}"
                        else
                            "_: &mut core::mem::ManuallyDrop<${type}>")

                ret = type_name(types, roc_fn.ret)
                when Types.shape(types, roc_fn.ret) is
                    Function(_) ->
                        ("(_: *mut u8, ${arguments})", ret, Bool.true)

                    _ ->
                        ("(_: *mut ${ret}, ${arguments})", ret, Bool.false)

            _ ->
                ret = type_name(types, id)
                ("(_: *mut ${ret})", ret, Bool.false)

    extern_arguments =
        when Types.shape(types, id) is
            Function(roc_fn) ->
                to_arg_str(roc_fn.args, types, \_argId, shape, index ->
                    index_str = Num.to_str(index)

                    if can_derive_copy(types, shape) then
                        "arg${index_str}"
                    else
                        "&mut core::mem::ManuallyDrop::new(arg${index_str})")

            _ ->
                ""

    if returns_fn then
        """
        ${buf}

        pub fn ${name}${public_signature} {
            extern "C" {
                fn roc__${name}_1_exposed_generic${extern_signature};
                fn roc__${name}_1_exposed_size() -> i64;
            }

            unsafe {
                let capacity = roc__${name}_1_exposed_size() as usize;

                let mut ret = ${return_type_name} {
                    closure_data: Vec::with_capacity(capacity),
                };
                ret.closure_data.resize(capacity, 0);

                roc__${name}_1_exposed_generic(ret.closure_data.as_mut_ptr(), ${extern_arguments});

                ret
            }
        }
        """
    else
        """
        ${buf}

        pub fn ${name}${public_signature} {
            extern "C" {
                fn roc__${name}_1_exposed_generic${extern_signature};
            }

            let mut ret = core::mem::MaybeUninit::uninit();

            unsafe {
                roc__${name}_1_exposed_generic(ret.as_mut_ptr(), ${extern_arguments});

                ret.assume_init()
            }
        }
        """

generate_function : Str, Types, RocFn -> Str
generate_function = \buf, types, roc_fn ->
    name = roc_fn.function_name
    extern_name = roc_fn.extern_name

    public_arguments =
        to_arg_str(roc_fn.args, types, \arg_id, _shape, index ->
            type = type_name(types, arg_id)
            index_str = Num.to_str(index)

            "arg${index_str}: ${type}")

    extern_def_arguments =
        without_unit =
            to_arg_str(roc_fn.args, types, \arg_id, _shape, index ->
                type = type_name(types, arg_id)
                index_str = Num.to_str(index)

                "arg${index_str}: *const ${type}")

        if Str.is_empty(without_unit) then
            # These always have a first argument that's a pointer, even if it's to nothing.
            "arg0: *const ()"
        else
            without_unit

    extern_call_arguments =
        without_unit =
            to_arg_str(roc_fn.args, types, \_argId, _shape, index ->
                index_str = Num.to_str(index)

                "&arg${index_str}")

        if Str.is_empty(without_unit) then
            # These always have a first argument that's a pointer, even if it's to nothing.
            "&()"
        else
            without_unit

    public_comma = if Str.is_empty(public_arguments) then "" else ", "

    ret = type_name(types, roc_fn.ret)

    """
    ${buf}

    #[repr(C)]
    #[derive(Debug)]
    pub struct ${name} {
        closure_data: Vec<u8>,
    }

    impl ${name} {
        pub fn force_thunk(mut self${public_comma}${public_arguments}) -> ${ret} {
            extern "C" {
                fn ${extern_name}(${extern_def_arguments}, closure_data: *mut u8, output: *mut ${ret});
            }

            let mut output = core::mem::MaybeUninit::uninit();

            unsafe {
                ${extern_name}(${extern_call_arguments}, self.closure_data.as_mut_ptr(), output.as_mut_ptr());

                output.assume_init()
            }
        }
    }
    """
    |> generate_roc_refcounted(types, Function(roc_fn), name)

generate_struct : Str, Types, TypeId, _, _, _ -> Str
generate_struct = \buf, types, id, name, struct_fields, visibility ->
    escaped_name = escape_kw(name)
    repr =
        length =
            when struct_fields is
                HasClosure(fields) -> List.len(fields)
                HasNoClosure(fields) -> List.len(fields)
        if length <= 1 then
            "transparent"
        else
            "C"

    pub =
        when visibility is
            Public -> "pub "
            Private -> ""

    struct_type = Types.shape(types, id)

    buf
    |> generate_derive_str(types, struct_type, IncludeDebug)
    |> Str.concat("#[repr(${repr})]\n${pub}struct ${escaped_name} {\n")
    |> generate_struct_fields(types, Public, struct_fields)
    |> Str.concat("}\n\n")
    |> generate_roc_refcounted(types, struct_type, escaped_name)

generate_struct_fields = \buf, types, visibility, struct_fields ->
    when struct_fields is
        HasNoClosure(fields) ->
            List.walk(fields, buf, generate_struct_field_without_closure(types, visibility))

        HasClosure(fields) ->
            List.walk(fields, buf, generate_struct_field_without_closure(types, visibility))

generate_struct_field_without_closure = \types, visibility ->
    \accum, { name: field_name, id } ->
        type_str = type_name(types, id)
        escaped_field_name = escape_kw(field_name)

        pub =
            when visibility is
                Public -> "pub"
                Private -> ""

        Str.concat(accum, "${indent}${pub} ${escaped_field_name}: ${type_str},\n")

name_tag_union_payload_fields = \payload_fields ->
    # Tag union payloads have numbered fields, so we prefix them
    # with an "f" because Rust doesn't allow struct fields to be numbers.
    when payload_fields is
        HasNoClosure(fields) ->
            renamed_fields = List.map(fields, \{ name, id } -> { name: "f${name}", id })
            HasNoClosure(renamed_fields)

        HasClosure(fields) ->
            renamed_fields = List.map(fields, \{ name, id, accessors } -> { name: "f${name}", id, accessors })
            HasClosure(renamed_fields)

generate_enumeration = \buf, types, enum_type, name, tags, tag_bytes ->
    escaped_name = escape_kw(name)

    repr_bits = tag_bytes * 8 |> Num.to_str

    buf
    |> generate_derive_str(types, enum_type, ExcludeDebug)
    |> Str.concat("#[repr(u${repr_bits})]\npub enum ${escaped_name} {\n")
    |> \b -> List.walk_with_index(tags, b, generate_enum_tags)
    |>
    # Enums require a custom debug impl to ensure naming is identical on all platforms.
    Str.concat(
        """
        }

        impl core::fmt::Debug for ${escaped_name} {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {

        """,
    )
    |> \b -> List.walk(tags, b, generate_enum_tags_debug(name))
    |> Str.concat("${indent}${indent}}\n${indent}}\n}\n\n")
    |> generate_roc_refcounted(types, enum_type, escaped_name)

generate_enum_tags = \accum, name, index ->
    index_str = Num.to_str(index)

    Str.concat(accum, "${indent}${name} = ${index_str},\n")

generate_enum_tags_debug = \name ->
    \accum, tag_name ->
        Str.concat(accum, "${indent}${indent}${indent}Self::${tag_name} => f.write_str(\"${name}::${tag_name}\"),\n")

derive_clone_tag_union : Str, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derive_clone_tag_union = \buf, tag_union_type, tags ->
    clones =
        List.walk(tags, "", \accum, { name: tag_name } ->
            """
            ${accum}
                            ${tag_name} => union_${tag_union_type} {
                                ${tag_name}: self.payload.${tag_name}.clone(),
                            },
            """)

    """
    ${buf}

    impl Clone for ${tag_union_type} {
        fn clone(&self) -> Self {
            use discriminant_${tag_union_type}::*;

            let payload = unsafe {
                match self.discriminant {${clones}
                }
            };

            Self {
                discriminant: self.discriminant,
                payload,
            }
        }
    }
    """

derive_debug_tag_union : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derive_debug_tag_union = \buf, types, tag_union_type, tags ->
    checks =
        List.walk(tags, "", \accum, { name: tag_name, payload } ->
            type =
                when payload is
                    Some(id) -> type_name(types, id)
                    None -> "()"

            """
            ${accum}
                            ${tag_name} => {
                                let field: &${type} = &self.payload.${tag_name};
                                f.debug_tuple("${tag_union_type}::${tag_name}").field(field).finish()
                            },
            """)

    """
    ${buf}

    impl core::fmt::Debug for ${tag_union_type} {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            use discriminant_${tag_union_type}::*;

            unsafe {
                match self.discriminant {${checks}
                }
            }
        }
    }
    """

derive_eq_tag_union : Str, Types, Shape, Str -> Str
derive_eq_tag_union = \buf, types, shape, tag_union_type ->
    if can_support_eq_hash_ord(types, shape) then
        """
        ${buf}

        impl Eq for ${tag_union_type} {}
        """
    else
        buf

derive_partial_eq_tag_union : Str, Types, Shape, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derive_partial_eq_tag_union = \buf, types, shape, tag_union_type, tags ->
    if can_support_partial_eq_ord(types, shape) then
        checks =
            List.walk(tags, "", \accum, { name: tag_name } ->
                """
                ${accum}
                                ${tag_name} => self.payload.${tag_name} == other.payload.${tag_name},
                """)

        """
        ${buf}

        impl PartialEq for ${tag_union_type} {
            fn eq(&self, other: &Self) -> bool {
                use discriminant_${tag_union_type}::*;

                if self.discriminant != other.discriminant {
                    return false;
                }

                unsafe {
                    match self.discriminant {${checks}
                    }
                }
            }
        }
        """
    else
        buf

derive_ord_tag_union : Str, Types, Shape, Str -> Str
derive_ord_tag_union = \buf, types, shape, tag_union_type ->
    if can_support_eq_hash_ord(types, shape) then
        """
        ${buf}

        impl Ord for ${tag_union_type} {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.partial_cmp(other).unwrap()
            }
        }
        """
    else
        buf

derive_partial_ord_tag_union : Str, Types, Shape, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derive_partial_ord_tag_union = \buf, types, shape, tag_union_type, tags ->
    if can_support_partial_eq_ord(types, shape) then
        checks =
            List.walk(tags, "", \accum, { name: tag_name } ->
                """
                ${accum}
                                    ${tag_name} => self.payload.${tag_name}.partial_cmp(&other.payload.${tag_name}),
                """)

        """
        ${buf}

        impl PartialOrd for ${tag_union_type} {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                use discriminant_${tag_union_type}::*;

                use std::cmp::Ordering::*;

                match self.discriminant.cmp(&other.discriminant) {
                    Less => Option::Some(Less),
                    Greater => Option::Some(Greater),
                    Equal => unsafe {
                        match self.discriminant {${checks}
                        }
                    },
                }
            }
        }
        """
    else
        buf

derive_hash_tag_union : Str, Types, Shape, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
derive_hash_tag_union = \buf, types, shape, tag_union_type, tags ->
    if can_support_eq_hash_ord(types, shape) then
        checks =
            List.walk(tags, "", \accum, { name: tag_name } ->
                """
                ${accum}
                                ${tag_name} => self.payload.${tag_name}.hash(state),
                """)

        """
        ${buf}

        impl core::hash::Hash for ${tag_union_type} {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                use discriminant_${tag_union_type}::*;

                unsafe {
                    match self.discriminant {${checks}
                    }
                }
            }
        }
        """
    else
        buf

generate_constructor_functions : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
generate_constructor_functions = \buf, types, tag_union_type, tags ->
    buf
    |> Str.concat("\n\nimpl ${tag_union_type} {")
    |> \b -> List.walk(tags, b, \accum, r -> generate_constructor_function(accum, types, tag_union_type, r.name, r.payload))
    |> Str.concat("\n}\n\n")

generate_constructor_function : Str, Types, Str, Str, [Some TypeId, None] -> Str
generate_constructor_function = \buf, types, tag_union_type, name, opt_payload ->
    when opt_payload is
        None ->
            """
            ${buf}

                pub fn ${name}() -> Self {
                    Self {
                        discriminant: discriminant_${tag_union_type}::${name},
                        payload: union_${tag_union_type} {
                            ${name}: (),
                        }
                    }
                }
            """

        Some(payload_id) ->
            payload_type = type_name(types, payload_id)
            shape = Types.shape(types, payload_id)

            new =
                if can_derive_copy(types, shape) then
                    "payload"
                else
                    "core::mem::ManuallyDrop::new(payload)"

            """
            ${buf}

                pub fn ${name}(payload: ${payload_type}) -> Self {
                    Self {
                        discriminant: discriminant_${tag_union_type}::${name},
                        payload: union_${tag_union_type} {
                            ${name}: ${new},
                        }
                    }
                }
            """

generate_destructor_functions : Str, Types, Str, List { name : Str, payload : [Some TypeId, None] } -> Str
generate_destructor_functions = \buf, types, tag_union_type, tags ->
    buf
    |> Str.concat("\n\nimpl ${tag_union_type} {")
    |> \b -> List.walk(tags, b, \accum, r -> generate_destructor_function(accum, types, tag_union_type, r.name, r.payload))
    |> Str.concat("\n}\n\n")

generate_destructor_function : Str, Types, Str, Str, [Some TypeId, None] -> Str
generate_destructor_function = \buf, types, tag_union_type, name, opt_payload ->
    when opt_payload is
        None ->
            """
            ${buf}

                pub fn is_${name}(&self) -> bool {
                    matches!(self.discriminant, discriminant_${tag_union_type}::${name})
                }
            """

        Some(payload_id) ->
            payload_type = type_name(types, payload_id)
            shape = Types.shape(types, payload_id)

            take =
                if can_derive_copy(types, shape) then
                    "unsafe { self.payload.${name} }"
                else
                    "unsafe { core::mem::ManuallyDrop::take(&mut self.payload.${name}) }"

            (borrow, borrow_type) =
                if can_derive_copy(types, shape) then
                    ("unsafe { self.payload.${name} }", payload_type)
                else
                    (
                        """
                        use core::borrow::Borrow;
                        unsafe { self.payload.${name}.borrow() }
                        """,
                        "&${payload_type}",
                    )

            (borrow_mut, borrow_mut_type) =
                if can_derive_copy(types, shape) then
                    ("unsafe { &mut self.payload.${name} }", "&mut ${payload_type}")
                else
                    (
                        """
                        use core::borrow::BorrowMut;
                        unsafe { self.payload.${name}.borrow_mut() }
                        """,
                        "&mut ${payload_type}",
                    )

            """
            ${buf}

                pub fn unwrap_${name}(mut self) -> ${payload_type} {
                    debug_assert_eq!(self.discriminant, discriminant_${tag_union_type}::${name});
                    ${take}
                }

                pub fn borrow_${name}(&self) -> ${borrow_type} {
                    debug_assert_eq!(self.discriminant, discriminant_${tag_union_type}::${name});
                    ${borrow}
                }

                pub fn borrow_mut_${name}(&mut self) -> ${borrow_mut_type} {
                    debug_assert_eq!(self.discriminant, discriminant_${tag_union_type}::${name});
                    ${borrow_mut}
                }

                pub fn is_${name}(&self) -> bool {
                    matches!(self.discriminant, discriminant_${tag_union_type}::${name})
                }
            """

generate_non_recursive_tag_union : Str, Types, TypeId, Str, List { name : Str, payload : [Some TypeId, None] }, U32, U32 -> Str
generate_non_recursive_tag_union = \buf, types, id, name, tags, discriminant_size, discriminant_offset ->
    escaped_name = escape_kw(name)
    discriminant_name = "discriminant_${escaped_name}"
    union_name = "union_${escaped_name}"
    discriminant_offset_str = Num.to_str(discriminant_offset)
    tag_names = List.map(tags, \{ name: n } -> n)
    self_mut = "self"

    max = \a, b -> if a >= b then a else b

    align_of_union =
        List.walk(tags, 1, \accum, { payload } ->
            when payload is
                Some(payload_id) -> max(accum, Types.alignment(types, payload_id))
                None -> accum)

    align_of_union_str = Num.to_str(align_of_union)

    size_of_union_str =
        List.walk(tags, 1, \accum, { payload } ->
            when payload is
                Some(payload_id) -> max(accum, Types.size(types, payload_id))
                None -> accum)
        |> next_multiple_of(align_of_union)
        |> Num.to_str

    size_of_self = Num.to_str(Types.size(types, id))
    align_of_self = Num.to_str(Types.alignment(types, id))
    shape = Types.shape(types, id)

    # TODO: this value can be different than the alignment of `id`
    align =
        List.walk(tags, 1, \accum, { payload } ->
            when payload is
                Some(payload_id) -> max(accum, Types.alignment(types, payload_id))
                None -> accum)
        |> Num.to_str

    union_type = Types.shape(types, id)

    buf
    |> generate_discriminant(types, discriminant_name, tag_names, discriminant_size)
    |> Str.concat("#[repr(C, align(${align}))]\npub union ${union_name} {\n")
    |> \b -> List.walk(tags, b, generate_union_field(types))
    |> Str.concat(
        """
        }

        // TODO(@roc-lang): See https://github.com/roc-lang/roc/issues/6012
        // const _SIZE_CHECK_${union_name}: () = assert!(core::mem::size_of::<${union_name}>() == ${size_of_union_str});
        const _ALIGN_CHECK_${union_name}: () = assert!(core::mem::align_of::<${union_name}>() == ${align_of_union_str});

        const _SIZE_CHECK_${escaped_name}: () = assert!(core::mem::size_of::<${escaped_name}>() == ${size_of_self});
        const _ALIGN_CHECK_${escaped_name}: () = assert!(core::mem::align_of::<${escaped_name}>() == ${align_of_self});

        impl ${escaped_name} {
            ${discriminant_doc_comment}
            pub fn discriminant(&self) -> ${discriminant_name} {
                unsafe {
                    let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

                    core::mem::transmute::<u8, ${discriminant_name}>(*bytes.as_ptr().add(${discriminant_offset_str}))
                }
            }

            /// Internal helper
            fn set_discriminant(&mut self, discriminant: ${discriminant_name}) {
                let discriminant_ptr: *mut ${discriminant_name} = (self as *mut ${escaped_name}).cast();

                unsafe {
                    *(discriminant_ptr.add(${discriminant_offset_str})) = discriminant;
                }
            }
        }


        """,
    )
    |> Str.concat(
        """
        #[repr(C)]
        pub struct ${escaped_name} {
            payload: union_${escaped_name},
            discriminant: discriminant_${escaped_name},
        }
        """,
    )
    |> derive_clone_tag_union(escaped_name, tags)
    |> derive_debug_tag_union(types, escaped_name, tags)
    |> derive_eq_tag_union(types, shape, escaped_name)
    |> derive_partial_eq_tag_union(types, shape, escaped_name, tags)
    |> derive_ord_tag_union(types, shape, escaped_name)
    |> derive_partial_ord_tag_union(types, shape, escaped_name, tags)
    |> derive_hash_tag_union(types, shape, escaped_name, tags)
    |> generate_destructor_functions(types, escaped_name, tags)
    |> generate_constructor_functions(types, escaped_name, tags)
    |> \b ->
        if cannot_support_copy(types, union_type) then
            # A custom drop impl is only needed when we can't derive copy.
            b
            |> Str.concat(
                """
                impl Drop for ${escaped_name} {
                    fn drop(&mut self) {
                        // Drop the payloads

                """,
            )
            |> generate_tag_union_drop_payload(types, self_mut, tags, discriminant_name, discriminant_size, 2)
            |> Str.concat(
                """
                    }
                }


                """,
            )
        else
            b
    |> generate_roc_refcounted(types, union_type, escaped_name)

generate_non_nullable_unwrapped = \buf, types, name, tag_name, payload, discriminant_size, _discriminant_offset, _null_tagIndex ->
    escaped_name = escape_kw(name)
    discriminant_name = "discriminant_${escaped_name}"

    payload_fields =
        when Types.shape(types, payload) is
            TagUnionPayload({ fields }) ->
                when fields is
                    HasNoClosure(xs) -> List.map(xs, .id)
                    HasClosure(xs) -> List.map(xs, .id)

            _ ->
                []

    payload_field_names =
        comma_separated("", payload_fields, \_, i ->
            n = Num.to_str(i)
            "f${n}")

    constructor_arguments =
        comma_separated("", payload_fields, \id, i ->
            n = Num.to_str(i)
            type = type_name(types, id)
            "f${n}: ${type}")

    debug_fields =
        payload_fields
        |> List.map_with_index(\_, i ->
            n = Num.to_str(i)
            ".field(&node.f${n})")
        |> Str.join_with("")

    buf1 = buf |> generate_discriminant(types, discriminant_name, [tag_name], discriminant_size)

    union_type = TagUnion(NonNullableUnwrapped({ name, tag_name, payload }))

    """
    ${buf1}

    #[repr(transparent)]
    #[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
    pub struct ${escaped_name}(roc_std::RocBox<${name}_${tag_name}>);

    impl ${escaped_name} {
        pub fn ${tag_name)(${constructor_arguments}) -> Self {
            let payload = ${name}_${tag_name} { ${payload_field_names} };

            Self(roc_std::RocBox::new(payload))
        }
    }

    impl core::fmt::Debug for ${escaped_name} {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            let node = &self.0;
            f.debug_tuple("${escaped_name}::${tag_name}")${debug_fields}.finish()
        }
    }
    """
    |> generate_roc_refcounted(types, union_type, escaped_name)

generate_recursive_tag_union = \buf, types, id, tag_union_name, tags, discriminant_size, _discriminant_offset, null_tag_index ->
    escaped_name = escape_kw(tag_union_name)
    discriminant_name = "discriminant_${escaped_name}"
    tag_names = List.map(tags, \{ name: n } -> n)
    # self = "(&*self.union_pointer())"
    # selfMut = "(&mut *self.union_pointer())"
    # other = "(&*other.union_pointer())"
    union_name = "union_${escaped_name}"

    discriminants =
        tag_names
        |> Str.join_with(", ")
        |> \b -> "[ ${b} ]"

    null_tag_id =
        when null_tag_index is
            Some(index) ->
                n = Num.to_str(index)
                "discriminants[${n}]"

            None ->
                """
                unreachable!("this pointer cannot be NULL")
                """

    is_function = \{ name: tag_name, payload: opt_payload }, index ->
        payload_fields =
            when opt_payload is
                Some(payload) ->
                    when Types.shape(types, payload) is
                        TagUnionPayload({ fields }) ->
                            when fields is
                                HasNoClosure(xs) -> List.map(xs, .id)
                                HasClosure(xs) -> List.map(xs, .id)

                        _ ->
                            []

                None ->
                    []

        field_getters =
            List.walk(payload_fields, { i: 0, accum: "" }, \{ i, accum }, field_type_id ->
                field_type_name = type_name(types, field_type_id)
                field_index = Num.to_str(i)

                {
                    i: i + 1,
                    accum:
                    """
                    ${accum}
                        pub fn get_${tag_name}_f${field_index}(&self) -> &${field_type_name} {
                            debug_assert!(self.is_${tag_name}());

                            // extern "C" {
                            //     fn foobar(tag_id: u16, field_index: usize) -> usize;
                            // }

                            // let offset = unsafe { foobar(${field_index}) };
                            let offset = 0;
                            unsafe { &*self.unmasked_pointer().add(offset).cast() }
                        }

                    """,
                })
            |> .accum

        payload_field_names =
            comma_separated("", payload_fields, \_, i ->
                n = Num.to_str(i)
                "f${n}")

        constructor_arguments =
            comma_separated("", payload_fields, \payload_id, i ->
                n = Num.to_str(i)
                type = type_name(types, payload_id)
                "f${n}: ${type}")

        fix_manually_drop =
            when opt_payload is
                Some(payload) ->
                    shape = Types.shape(types, payload)

                    if can_derive_copy(types, shape) then
                        "payload"
                    else
                        "core::mem::ManuallyDrop::new(payload)"

                None ->
                    "payload"

        if Some(Num.int_cast(index)) == null_tag_index then
            """
                pub fn is_${tag_name}(&self) -> bool {
                    matches!(self.discriminant(), discriminant_${escaped_name}::${tag_name})
                }

                pub fn ${tag_name}(${constructor_arguments}) -> Self {
                    Self(std::ptr::null_mut())
                }
            """
        else
            """
                pub fn is_${tag_name}(&self) -> bool {
                    matches!(self.discriminant(), discriminant_${escaped_name}::${tag_name})
                }

                pub fn ${tag_name}(${constructor_arguments}) -> Self {
                    let tag_id = discriminant_${escaped_name}::${tag_name};

                    let payload = ${escaped_name}_${tag_name} { ${payload_field_names} } ;

                    let union_payload = union_${escaped_name} { ${tag_name}: ${fix_manually_drop} };

                    let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(union_payload)) };

                    Self((ptr as usize | tag_id as usize) as *mut _)
                }
            ${field_getters}

                pub fn get_${tag_name}(mut self) -> ${escaped_name}_${tag_name} {
                    debug_assert!(self.is_${tag_name}());

                    unsafe { core::mem::ManuallyDrop::take(&mut self.ptr_read_union().{dtag_name}) }
                }
            """

    constructors =
        tags
        |> List.map_with_index(is_function)
        |> Str.join_with("\n\n")

    clone_case = \{ name: tag_name }, index ->
        if Some(Num.int_cast(index)) == null_tag_index then
            """
                        ${tag_name} => Self::${tag_name}(),
            """
        else
            """
                        ${tag_name} => {
                            let tag_id = discriminant_${escaped_name}::${tag_name};

                            let payload_union = unsafe { self.ptr_read_union() };
                            let payload = union_${escaped_name} {
                                ${tag_name}: unsafe { payload_union.${tag_name}.clone() },
                            };

                            let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(payload)) };

                            Self((ptr as usize | tag_id as usize) as *mut _)
                        },
            """

    clone_cases =
        tags
        |> List.map_with_index(clone_case)
        |> Str.join_with("\n")

    partial_eq_case = \{ name: tag_name }, index ->
        if Some(Num.int_cast(index)) == null_tag_index then
            """
                        ${tag_name} => true,
            """
        else
            """
                        ${tag_name} => {
                            let payload_union1 = unsafe { self.ptr_read_union() };
                            let payload_union2 = unsafe { other.ptr_read_union() };

                            unsafe {
                                payload_union1.${tag_name} == payload_union2.${tag_name}
                            }
                        },
            """

    partial_eq_cases =
        tags
        |> List.map_with_index(partial_eq_case)
        |> Str.join_with("\n")

    partial_eq_impl =
        if can_support_partial_eq_ord(types, Types.shape(types, id)) then
            """
            impl PartialEq for ${escaped_name} {
                fn eq(&self, other: &Self) -> bool {
                    use discriminant_${escaped_name}::*;

                    if self.discriminant() != other.discriminant() {
                        return false;
                    }

                    match self.discriminant() {
                        ${partial_eq_cases}
                    }
                }
            }

            impl Eq for ${escaped_name} {}
            """
        else
            ""

    debug_case = \{ name: tag_name, payload: opt_payload }, index ->
        if Some(Num.int_cast(index)) == null_tag_index then
            """
                        ${tag_name} => f.debug_tuple("${escaped_name}::${tag_name}").finish(),
            """
        else
            payload_fields =
                when opt_payload is
                    Some(payload) ->
                        when Types.shape(types, payload) is
                            TagUnionPayload({ fields }) ->
                                when fields is
                                    HasNoClosure(xs) -> List.map(xs, .id)
                                    HasClosure(xs) -> List.map(xs, .id)

                            _ ->
                                []

                    None ->
                        []

            debug_fields =
                payload_fields
                |> List.map_with_index(\_, i ->
                    n = Num.to_str(i)
                    ".field(&payload_union.${tag_name}.f${n})")
                |> Str.join_with("")

            """
                        ${tag_name} => {
                            let payload_union = unsafe { self.ptr_read_union() };

                            unsafe {
                                f.debug_tuple("${escaped_name}::${tag_name}")${debug_fields}.finish()
                            }
                        },
            """

    debug_cases =
        tags
        |> List.map_with_index(debug_case)
        |> Str.join_with("\n")

    hash_case = \{ name: tag_name }, index ->
        if Some(Num.int_cast(index)) == null_tag_index then
            """
                        ${tag_name} => {}
            """
        else
            """
                        ${tag_name} => {
                            let payload_union = unsafe { self.ptr_read_union() };
                            unsafe { payload_union.${tag_name}.hash(state) };
                        },
            """

    hash_cases =
        tags
        |> List.map_with_index(hash_case)
        |> Str.join_with("\n")

    union_type = Types.shape(types, id)
    hash_impl =
        if can_support_partial_eq_ord(types, union_type) then
            """
            impl core::hash::Hash for ${escaped_name} {
                fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                    use discriminant_${escaped_name}::*;

                    self.discriminant().hash(state);

                    match self.discriminant() {
                        ${hash_cases}
                    }
                }
            }
            """
        else
            ""

    partial_ord_case = \{ name: tag_name }, index ->
        if Some(Num.int_cast(index)) == null_tag_index then
            """
                        ${tag_name} => std::cmp::Ordering::Equal,
            """
        else
            """
                        ${tag_name} => {
                            let payload_union1 = unsafe { self.ptr_read_union() };
                            let payload_union2 = unsafe { other.ptr_read_union() };

                            unsafe {
                                payload_union1.${tag_name}.cmp(&payload_union2.${tag_name})
                            }
                        },
            """

    partial_ord_cases =
        tags
        |> List.map_with_index(partial_ord_case)
        |> Str.join_with("\n")

    partial_ord_impl =
        if can_support_partial_eq_ord(types, Types.shape(types, id)) then
            """
            impl PartialOrd for ${escaped_name} {
                fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                    Some(<Self as Ord>::cmp(self, other))
                }
            }

            impl Ord for ${escaped_name} {
                fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                    use discriminant_${escaped_name}::*;

                    use std::cmp::Ordering::*;

                    match self.discriminant().cmp(&other.discriminant()) {
                        Less => Less,
                        Greater => Greater,
                        Equal => unsafe {
                            match self.discriminant() {
                                ${partial_ord_cases}
                            }
                        },
                    }
                }
            }
            """
        else
            ""

    size_of_self = Num.to_str(Types.size(types, id))
    align_of_self = Num.to_str(Types.alignment(types, id))

    buf
    |> generate_discriminant(types, discriminant_name, tag_names, discriminant_size)
    |> Str.concat(
        """
        #[repr(transparent)]
        pub struct ${escaped_name}(*mut ${union_name});

        const _SIZE_CHECK_${escaped_name}: () = assert!(core::mem::size_of::<${escaped_name}>() == ${size_of_self});
        const _ALIGN_CHECK_${escaped_name}: () = assert!(core::mem::align_of::<${escaped_name}>() == ${align_of_self});

        impl ${escaped_name} {
            pub fn discriminant(&self) -> discriminant_${escaped_name} {
                let discriminants = {
                    use ${discriminant_name}::*;

                    ${discriminants}
                };

                if self.0.is_null() {
                    ${null_tag_id}
                } else  {
                    match std::mem::size_of::<usize>() {
                        4 => discriminants[self.0 as usize & 0b011],
                        8 => discriminants[self.0 as usize & 0b111],
                        _ => unreachable!(),
                    }
                }
            }

            fn unmasked_pointer(&self) -> *mut union_${escaped_name} {
                debug_assert!(!self.0.is_null());

                let mask = match std::mem::size_of::<usize>() {
                    4 => !0b011usize,
                    8 => !0b111usize,
                    _ => unreachable!(),
                };

                ((self.0 as usize) & mask) as *mut union_${escaped_name}
            }

            unsafe fn ptr_read_union(&self) -> core::mem::ManuallyDrop<union_${escaped_name}> {
                let ptr = self.unmasked_pointer();

                core::mem::ManuallyDrop::new(unsafe { std::ptr::read(ptr) })
            }

            ${constructors}
        }

        impl Clone for ${escaped_name} {
            fn clone(&self) -> Self {
                use discriminant_${escaped_name}::*;

                let discriminant = self.discriminant();

                match discriminant {
                ${clone_cases}
                }
            }
        }

        ${partial_eq_impl}

        ${hash_impl}

        ${partial_ord_impl}


        impl core::fmt::Debug for ${escaped_name} {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                use discriminant_${escaped_name}::*;

                match self.discriminant() {
                    ${debug_cases}
                }
            }
        }


        #[repr(C)]
        union ${union_name} {
        """,
    )
    |> \b -> List.walk(tags, b, generate_union_field(types))
    |> Str.concat("}\n\n")
    |> generate_roc_refcounted(types, union_type, escaped_name)

generate_tag_union_drop_payload = \buf, types, self_mut, tags, discriminant_name, discriminant_size, indents ->
    if discriminant_size == 0 then
        when List.first(tags) is
            Ok({ name }) ->
                # There's only one tag, so there's no discriminant and no need to match;
                # just drop the pointer.
                buf
                |> write_indents(indents)
                |> Str.concat("unsafe { core::mem::ManuallyDrop::drop(&mut core::ptr::read(self.pointer).${name}); }")

            Err(ListWasEmpty) ->
                crash("unreachable")
    else
        buf
        |> write_tag_impls(tags, discriminant_name, indents, \name, payload ->
            when payload is
                Some(id) if cannot_support_copy(types, Types.shape(types, id)) ->
                    "unsafe { core::mem::ManuallyDrop::drop(&mut ${self_mut}.payload.${name}) },"

                _ ->
                    # If it had no payload, or if the payload had no pointers,
                    # there's nothing to clean up, so do `=> {}` for the branch.
                    "{}")

write_indents = \buf, indents ->
    if indents <= 0 then
        buf
    else
        buf
        |> Str.concat(indent)
        |> write_indents((indents - 1))

write_tag_impls : Str, List { name : Str, payload : [Some TypeId, None] }, Str, U64, (Str, [Some TypeId, None] -> Str) -> Str
write_tag_impls = \buf, tags, discriminant_name, indents, f ->
    buf
    |> write_indents(indents)
    |> Str.concat("match self.discriminant() {\n")
    |> \b ->
        List.walk(tags, b, \accum, { name, payload } ->
            branch_str = f(name, payload)
            accum
            |> write_indents((indents + 1))
            |> Str.concat("${discriminant_name}::${name} => ${branch_str}\n"))
    |> write_indents(indents)
    |> Str.concat("}\n")

generate_discriminant = \buf, types, name, tags, size ->
    if size > 0 then
        enum_type =
            TagUnion(
                Enumeration({
                    name,
                    tags,
                    size,
                }),
            )

        buf
        |> generate_enumeration(types, enum_type, name, tags, size)
    else
        buf

generate_union_field = \types ->
    \accum, { name: field_name, payload } ->
        escaped_field_name = escape_kw(field_name)

        when payload is
            Some(id) ->
                type_str = type_name(types, id)

                type = Types.shape(types, id)
                full_type_str =
                    if cannot_support_copy(types, type) then
                        # types with pointers need ManuallyDrop
                        # because rust unions don't (and can't)
                        # know how to drop them automatically!
                        "core::mem::ManuallyDrop<${type_str}>"
                    else
                        type_str

                Str.concat(accum, "${indent}${escaped_field_name}: ${full_type_str},\n")

            None ->
                # use unit as the payload
                Str.concat(accum, "${indent}${escaped_field_name}: (),\n")

comma_separated : Str, List a, (a, U64 -> Str) -> Str
comma_separated = \buf, items, step ->

    length = List.len(items) |> Num.int_cast

    help : { buf : Str, count : U64 }, a -> { buf : Str, count : U64 }
    help = \accum, item ->
        if accum.count + 1 == length then
            { buf: Str.concat(accum.buf, step(item, accum.count)), count: length }
        else
            { buf: Str.concat(accum.buf, step(item, accum.count)) |> Str.concat(", "), count: accum.count + 1 }

    items
    |> List.walk({ buf, count: 0 }, help)
    |> .buf

generate_nullable_unwrapped : Str, Types, TypeId, Str, Str, Str, TypeId, [FirstTagIsNull, SecondTagIsNull] -> Str
generate_nullable_unwrapped = \buf, types, tag_unionid, name, null_tag, non_null_tag, non_null_payload, which_tag_is_null ->
    payload_fields =
        when Types.shape(types, non_null_payload) is
            TagUnionPayload({ fields }) ->
                when fields is
                    HasNoClosure(xs) -> List.map(xs, .id)
                    HasClosure(xs) -> List.map(xs, .id)

            _ ->
                []

    payload_field_names =
        comma_separated("", payload_fields, \_, i ->
            n = Num.to_str(i)
            "f${n}")

    constructor_arguments =
        comma_separated("", payload_fields, \id, i ->
            n = Num.to_str(i)
            type = type_name(types, id)
            "f${n}: ${type}")

    debug_fields =
        payload_fields
        |> List.map_with_index(\_, i ->
            n = Num.to_str(i)
            ".field(&node.f${n})")
        |> Str.join_with("")

    union_type = TagUnion(NullableUnwrapped({ name, null_tag, non_null_tag, non_null_payload, which_tag_is_null }))
    discriminant =
        when which_tag_is_null is
            FirstTagIsNull ->
                """
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                pub enum discriminant_${name} {
                    ${null_tag} = 0,
                    ${non_null_tag} = 1,
                }
                """

            SecondTagIsNull ->
                """
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                pub enum discriminant_${name} {
                    ${non_null_tag} = 0,
                    ${null_tag} = 1,
                }
                """

    size_of_self = Num.to_str(Types.size(types, tag_unionid))
    align_of_self = Num.to_str(Types.alignment(types, tag_unionid))

    """
    ${buf}

    #[derive(PartialOrd, Ord)]
    #[repr(C)]
    pub struct ${name}(*mut ${name}_${non_null_tag});

    ${discriminant}

    const _SIZE_CHECK_${name}: () = assert!(core::mem::size_of::<${name}>() == ${size_of_self});
    const _ALIGN_CHECK_${name}: () = assert!(core::mem::align_of::<${name}>() == ${align_of_self});

    impl ${name} {
        pub fn ${null_tag}() -> Self {
            Self(core::ptr::null_mut())
        }

        pub fn ${non_null_tag}(${constructor_arguments}) -> Self {
            let payload = ${name}_${non_null_tag} { ${payload_field_names} };

            let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(payload)) };

            Self(ptr)
        }

        pub fn discriminant(&self) -> discriminant_${name} {
            if self.is_${null_tag}() {
                discriminant_${name}::${null_tag}
            } else {
                discriminant_${name}::${non_null_tag}
            }
        }

        pub fn is_${null_tag}(&self) -> bool {
            self.0.is_null()
        }

        pub fn is_${non_null_tag}(&self) -> bool {
            !self.0.is_null()
        }
    }

    impl core::fmt::Debug for ${name} {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            if self.is_${null_tag}() {
                f.debug_tuple("${name}::${null_tag}").finish()
            } else {
                let node = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
                f.debug_tuple("${name}::${non_null_tag}")${debug_fields}.finish()
            }
        }
    }

    impl Clone for ${name} {
        fn clone(&self) -> Self {
            if self.is_${null_tag}() {
                Self::${null_tag}()
            } else {
                use std::ops::Deref;

                let node_ref = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
                let payload : ${name}_${non_null_tag} = (node_ref.deref()).clone();

                let ptr = unsafe { roc_std::RocBox::leak(roc_std::RocBox::new(payload)) };

                Self(ptr)
            }
        }
    }

    impl PartialEq for ${name} {
        fn eq(&self, other: &Self) -> bool {
            if self.discriminant() != other.discriminant() {
                return false;
            }

            if self.is_${null_tag}() {
                return true;
            }

            let payload1 = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
            let payload2 = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(other.0) });

            payload1 == payload2
        }
    }

    impl Eq for ${name} {}

    impl core::hash::Hash for ${name} {
        fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
            self.discriminant().hash(state);

            if self.is_${non_null_tag}() {
                let payload = core::mem::ManuallyDrop::new(unsafe { std::ptr::read(self.0) });
                payload.hash(state);
            }
        }
    }
    """
    |> generate_roc_refcounted(types, union_type, name)

generate_single_tag_struct = \buf, types, name, tag_name, payload ->
    # Store single-tag unions as structs rather than enums,
    # because they have only one alternative. However, still
    # offer the usual tag union APIs.
    escaped_name = escape_kw(name)
    repr =
        length =
            when payload is
                HasClosure(fields) -> List.len(fields)
                HasNoClosure(fields) -> List.len(fields)
        if length <= 1 then
            "transparent"
        else
            "C"

    when payload is
        HasNoClosure(fields) ->
            as_struct_fields =
                List.map_with_index(fields, \{ id }, index ->
                    index_str = Num.to_str(index)

                    { name: "f${index_str}", id })
                |> HasNoClosure
            as_struct_type =
                Struct({
                    name,
                    fields: as_struct_fields,
                })

            buf
            |> generate_derive_str(types, as_struct_type, ExcludeDebug)
            |> Str.concat("#[repr(${repr})]\npub struct ${escaped_name} ")
            |> \b ->
                if List.is_empty(fields) then
                    generate_zero_element_single_tag_struct(b, escaped_name, tag_name)
                else
                    generate_multi_element_single_tag_struct(b, types, escaped_name, tag_name, fields, as_struct_fields)
            |> generate_roc_refcounted(types, TagUnion(SingleTagStruct({ name, tag_name, payload })), escaped_name)

        HasClosure(_) ->
            Str.concat(buf, "\\TODO: SingleTagStruct with closures")

generate_multi_element_single_tag_struct = \buf, types, name, tag_name, payload_fields, as_struct_fields ->
    buf
    |> Str.concat("{\n")
    |> generate_struct_fields(types, Private, as_struct_fields)
    |> Str.concat("}\n\n")
    |> Str.concat(
        """
        impl ${name} {

        """,
    )
    |> \b ->
        field_types =
            payload_fields
            |> List.map(\{ id } ->
                type_name(types, id))
        args =
            field_types
            |> List.map_with_index(\field_type_name, index ->
                index_str = Num.to_str(index)

                "f${index_str}: ${field_type_name}")
        fields =
            payload_fields
            |> List.map_with_index(\_, index ->
                index_str = Num.to_str(index)

                "f${index_str}")

        field_accesses =
            fields
            |> List.map(\field ->
                "self.${field}")

        {
            b,
            args,
            fields,
            field_types,
            field_accesses,
        }
    |> \{ b, args, fields, field_types, field_accesses } ->
        args_str = Str.join_with(args, ", ")
        fields_str = Str.join_with(fields, ",\n${indent}${indent}${indent}")

        {
            b: Str.concat(
                b,
                """
                ${indent}/// A tag named ``${tag_name}``, with the given payload.
                ${indent}pub fn ${tag_name}(${args_str}) -> Self {
                ${indent}    Self {
                ${indent}        ${fields_str}
                ${indent}    }
                ${indent}}


                """,
            ),
            field_types,
            field_accesses,
        }
    |> \{ b, field_types, field_accesses } ->
        ret_type = as_rust_tuple(field_types)
        ret_expr = as_rust_tuple(field_accesses)

        {
            b: Str.concat(
                b,
                """
                ${indent}/// Since `${name}` only has one tag (namely, `${tag_name}`),
                ${indent}/// convert it to `${tag_name}`'s payload.
                ${indent}pub fn into_${tag_name}(self) -> ${ret_type} {
                ${indent}    ${ret_expr}
                ${indent}}


                """,
            ),
            field_types,
            field_accesses,
        }
    |> \{ b, field_types, field_accesses } ->
        ret_type =
            field_types
            |> List.map(\ft -> "&${ft}")
            |> as_rust_tuple
        ret_expr =
            field_accesses
            |> List.map(\fa -> "&${fa}")
            |> as_rust_tuple

        Str.concat(
            b,
            """
            ${indent}/// Since `${name}` only has one tag (namely, `${tag_name}`),
            ${indent}/// convert it to `${tag_name}`'s payload.
            ${indent}pub fn as_${tag_name}(&self) -> ${ret_type} {
            ${indent}    ${ret_expr}
            ${indent}}

            """,
        )
    |> Str.concat(
        """
        }


        impl core::fmt::Debug for ${name} {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.debug_tuple("${name}::${tag_name}")

        """,
    )
    |> \b ->
        payload_fields
        |> List.map_with_index(\_, index ->
            index_str = Num.to_str(index)

            "${indent}${indent}${indent}${indent}.field(&self.f${index_str})\n")
        |> List.walk(b, Str.concat)
    |> Str.concat(
        """
                        .finish()
            }
        }


        """,
    )

as_rust_tuple = \list ->
    # If there is 1 element in the list we just return it
    # Otherwise, we make a proper tuple string.
    joined = Str.join_with(list, ", ")

    if List.len(list) == 1 then
        joined
    else
        "(${joined})"

generate_zero_element_single_tag_struct = \buf, name, tag_name ->
    # A single tag with no payload is a zero-sized unit type, so
    # represent it as a zero-sized struct (e.g. "struct Foo()").
    buf
    |> Str.concat("();\n\n")
    |> Str.concat(
        """
        impl ${name} {
            /// A tag named ${tag_name}, which has no payload.
            pub const ${tag_name}: Self = Self();

            /// Other `into_` methods return a payload, but since ${tag_name} tag
            /// has no payload, this does nothing and is only here for completeness.
            pub fn into_${tag_name}(self) {
                ()
            }

            /// Other `as_` methods return a payload, but since ${tag_name} tag
            /// has no payload, this does nothing and is only here for completeness.
            pub fn as_${tag_name}(&self) {
                ()
            }
        }

        impl core::fmt::Debug for ${name} {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.write_str("${name}::${tag_name}")
            }
        }


        """,
    )

generate_derive_str = \buf, types, type, include_debug ->
    cond_write = \b, cond, str ->
        if cond then
            Str.concat(b, str)
        else
            b

    derive_debug =
        when include_debug is
            IncludeDebug -> Bool.true
            ExcludeDebug -> Bool.false

    buf
    |> Str.concat("#[derive(Clone, ")
    |> cond_write(!(cannot_support_copy(types, type)), "Copy, ")
    |> cond_write(!(cannot_support_default(types, type)), "Default, ")
    |> cond_write(derive_debug, "Debug, ")
    |> cond_write(can_support_partial_eq_ord(types, type), "PartialEq, PartialOrd, ")
    |> cond_write(can_support_eq_hash_ord(types, type), "Eq, Ord, Hash, ")
    |> Str.concat(")]\n")

can_support_eq_hash_ord : Types, Shape -> Bool
can_support_eq_hash_ord = \types, type ->
    !(has_float(types, type)) && (can_support_partial_eq_ord(types, type))

can_support_partial_eq_ord : Types, Shape -> Bool
can_support_partial_eq_ord = \types, type ->
    when type is
        Function(roc_fn) ->
            runtime_representation = Types.shape(types, roc_fn.lambda_set)
            can_support_partial_eq_ord(types, runtime_representation)

        Unsized -> Bool.false
        Unit | EmptyTagUnion | Bool | Num(_) | TagUnion(Enumeration(_)) -> Bool.true
        RocStr -> Bool.true
        RocList(inner) | RocSet(inner) | RocBox(inner) ->
            inner_type = Types.shape(types, inner)
            can_support_partial_eq_ord(types, inner_type)

        RocDict(k, v) ->
            k_type = Types.shape(types, k)
            v_type = Types.shape(types, v)

            can_support_partial_eq_ord(types, k_type) && can_support_partial_eq_ord(types, v_type)

        TagUnion(Recursive({ tags })) ->
            List.all(tags, \{ payload } ->
                when payload is
                    None -> Bool.true
                    Some(id) -> can_support_partial_eq_ord(types, Types.shape(types, id)))

        TagUnion(NullableWrapped({ tags })) ->
            List.all(tags, \{ payload } ->
                when payload is
                    None -> Bool.true
                    Some(id) -> can_support_partial_eq_ord(types, Types.shape(types, id)))

        TagUnion(NonNullableUnwrapped({ payload })) ->
            can_support_partial_eq_ord(types, Types.shape(types, payload))

        TagUnion(NullableUnwrapped({ non_null_payload })) ->
            can_support_partial_eq_ord(types, Types.shape(types, non_null_payload))

        RecursivePointer(_) -> Bool.true
        TagUnion(SingleTagStruct({ payload: HasNoClosure(fields) })) ->
            List.all(fields, \{ id } -> can_support_partial_eq_ord(types, Types.shape(types, id)))

        TagUnion(SingleTagStruct({ payload: HasClosure(_) })) ->
            Bool.false

        TagUnion(NonRecursive({ tags })) ->
            List.all(tags, \{ payload } ->
                when payload is
                    Some(id) -> can_support_partial_eq_ord(types, Types.shape(types, id))
                    None -> Bool.true)

        RocResult(ok_id, err_id) ->
            ok_shape = Types.shape(types, ok_id)
            err_shape = Types.shape(types, err_id)

            can_support_partial_eq_ord(types, ok_shape) && can_support_partial_eq_ord(types, err_shape)

        Struct({ fields: HasNoClosure(fields) }) | TagUnionPayload({ fields: HasNoClosure(fields) }) ->
            List.all(fields, \{ id } -> can_support_partial_eq_ord(types, Types.shape(types, id)))

        Struct({ fields: HasClosure(fields) }) | TagUnionPayload({ fields: HasClosure(fields) }) ->
            List.all(fields, \{ id } -> can_support_partial_eq_ord(types, Types.shape(types, id)))

cannot_support_copy : Types, Shape -> Bool
cannot_support_copy = \types, type ->
    !(can_derive_copy(types, type))

can_derive_copy : Types, Shape -> Bool
can_derive_copy = \types, type ->
    when type is
        Function(roc_fn) ->
            runtime_representation = Types.shape(types, roc_fn.lambda_set)
            can_derive_copy(types, runtime_representation)

        # unsized values are heap-allocated
        Unsized -> Bool.false
        Unit | EmptyTagUnion | Bool | Num(_) | TagUnion(Enumeration(_)) -> Bool.true
        RocStr | RocList(_) | RocDict(_, _) | RocSet(_) | RocBox(_) | TagUnion(NullableUnwrapped(_)) | TagUnion(NullableWrapped(_)) | TagUnion(Recursive(_)) | TagUnion(NonNullableUnwrapped(_)) | RecursivePointer(_) -> Bool.false
        TagUnion(SingleTagStruct({ payload: HasNoClosure(fields) })) ->
            List.all(fields, \{ id } -> can_derive_copy(types, Types.shape(types, id)))

        TagUnion(SingleTagStruct({ payload: HasClosure(fields) })) ->
            List.all(fields, \{ id } -> can_derive_copy(types, Types.shape(types, id)))

        TagUnion(NonRecursive({ tags })) ->
            List.all(tags, \{ payload } ->
                when payload is
                    Some(id) -> can_derive_copy(types, Types.shape(types, id))
                    None -> Bool.true)

        RocResult(ok_id, err_id) ->
            can_derive_copy(types, Types.shape(types, ok_id))
            && can_derive_copy(types, Types.shape(types, err_id))

        Struct({ fields: HasNoClosure(fields) }) | TagUnionPayload({ fields: HasNoClosure(fields) }) ->
            List.all(fields, \{ id } -> can_derive_copy(types, Types.shape(types, id)))

        Struct({ fields: HasClosure(fields) }) | TagUnionPayload({ fields: HasClosure(fields) }) ->
            List.all(fields, \{ id } -> can_derive_copy(types, Types.shape(types, id)))

cannot_support_default = \types, type ->
    when type is
        Unit | Unsized | EmptyTagUnion | TagUnion(_) | RocResult(_, _) | RecursivePointer(_) | Function(_) -> Bool.true
        RocStr | Bool | Num(_) -> Bool.false
        RocList(id) | RocSet(id) | RocBox(id) ->
            cannot_support_default(types, Types.shape(types, id))

        TagUnionPayload({ fields: HasClosure(_) }) -> Bool.true
        RocDict(key_id, val_id) ->
            cannot_support_copy(types, Types.shape(types, key_id))
            || cannot_support_copy(types, Types.shape(types, val_id))

        Struct({ fields: HasClosure(_) }) -> Bool.true
        Struct({ fields: HasNoClosure(fields) }) | TagUnionPayload({ fields: HasNoClosure(fields) }) ->
            List.any(fields, \{ id } -> cannot_support_default(types, Types.shape(types, id)))

has_float = \types, type ->
    has_float_help(types, type, Set.empty({}))

has_float_help = \types, type, do_not_recurse ->
    # TODO: is doNotRecurse problematic? Do we need an updated doNotRecurse for calls up the tree?
    # I think there is a change it really only matters for RecursivePointer, so it may be fine.
    # Otherwise we need to deal with threading through updates to doNotRecurse
    when type is
        Num(kind) ->
            when kind is
                F32 | F64 -> Bool.true
                _ -> Bool.false

        Unit | Unsized | EmptyTagUnion | RocStr | Bool | TagUnion(Enumeration(_)) | Function(_) -> Bool.false
        RocList(id) | RocSet(id) | RocBox(id) ->
            has_float_help(types, Types.shape(types, id), do_not_recurse)

        RocDict(id0, id1) | RocResult(id0, id1) ->
            has_float_help(types, Types.shape(types, id0), do_not_recurse)
            || has_float_help(types, Types.shape(types, id1), do_not_recurse)

        Struct({ fields: HasNoClosure(fields) }) | TagUnionPayload({ fields: HasNoClosure(fields) }) ->
            List.any(fields, \{ id } -> has_float_help(types, Types.shape(types, id), do_not_recurse))

        Struct({ fields: HasClosure(fields) }) | TagUnionPayload({ fields: HasClosure(fields) }) ->
            List.any(fields, \{ id } -> has_float_help(types, Types.shape(types, id), do_not_recurse))

        TagUnion(SingleTagStruct({ payload: HasNoClosure(fields) })) ->
            List.any(fields, \{ id } -> has_float_help(types, Types.shape(types, id), do_not_recurse))

        TagUnion(SingleTagStruct({ payload: HasClosure(fields) })) ->
            List.any(fields, \{ id } -> has_float_help(types, Types.shape(types, id), do_not_recurse))

        TagUnion(Recursive({ tags })) ->
            List.any(tags, \{ payload } ->
                when payload is
                    Some(id) -> has_float_help(types, Types.shape(types, id), do_not_recurse)
                    None -> Bool.false)

        TagUnion(NonRecursive({ tags })) ->
            List.any(tags, \{ payload } ->
                when payload is
                    Some(id) -> has_float_help(types, Types.shape(types, id), do_not_recurse)
                    None -> Bool.false)

        TagUnion(NullableWrapped({ tags })) ->
            List.any(tags, \{ payload } ->
                when payload is
                    Some(id) -> has_float_help(types, Types.shape(types, id), do_not_recurse)
                    None -> Bool.false)

        TagUnion(NonNullableUnwrapped({ payload })) ->
            if Set.contains(do_not_recurse, payload) then
                Bool.false
            else
                next_do_not_recurse = Set.insert(do_not_recurse, payload)

                has_float_help(types, Types.shape(types, payload), next_do_not_recurse)

        TagUnion(NullableUnwrapped({ non_null_payload })) ->
            if Set.contains(do_not_recurse, non_null_payload) then
                Bool.false
            else
                next_do_not_recurse = Set.insert(do_not_recurse, non_null_payload)

                has_float_help(types, Types.shape(types, non_null_payload), next_do_not_recurse)

        RecursivePointer(payload) ->
            if Set.contains(do_not_recurse, payload) then
                Bool.false
            else
                next_do_not_recurse = Set.insert(do_not_recurse, payload)

                has_float_help(types, Types.shape(types, payload), next_do_not_recurse)

generate_roc_refcounted = \buf, types, type, escaped_name ->
    if contains_refcounted(types, type) then
        impl =
            when type is
                TagUnion(NonNullableUnwrapped(_)) ->
                    """
                    impl roc_std::RocRefcounted for ${escaped_name} {
                        fn inc(&mut self) {
                            self.0.inc();
                        }
                        fn dec(&mut self) {
                            self.0.dec();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                TagUnion(Recursive(_)) ->
                    """
                    impl roc_std::RocRefcounted for ${escaped_name} {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    impl roc_std::RocRefcounted for union_${escaped_name} {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                TagUnion(NullableWrapped(_)) ->
                    """
                    impl roc_std::RocRefcounted for ${escaped_name} {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    impl roc_std::RocRefcounted for union_${escaped_name} {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            unimplemented!();
                        }
                    }\n\n
                    """

                Struct({ fields: HasNoClosure(fields) }) ->
                    inc_fields = generate_roc_refcounted_named_fields(types, fields, Inc, Struct)
                    dec_fields = generate_roc_refcounted_named_fields(types, fields, Dec, Struct)
                    """
                    impl roc_std::RocRefcounted for ${escaped_name} {
                        fn inc(&mut self) {
                        ${inc_fields}
                        }
                        fn dec(&mut self) {
                        ${dec_fields}
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                TagUnionPayload({ fields: HasNoClosure(fields) }) ->
                    inc_fields = generate_roc_refcounted_named_fields(types, fields, Inc, Tag)
                    dec_fields = generate_roc_refcounted_named_fields(types, fields, Dec, Tag)
                    """
                    impl roc_std::RocRefcounted for ${escaped_name} {
                        fn inc(&mut self) {
                        ${inc_fields}
                        }
                        fn dec(&mut self) {
                        ${dec_fields}
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """

                _ ->
                    """
                    impl roc_std::RocRefcounted for ${escaped_name} {
                        fn inc(&mut self) {
                            unimplemented!();
                        }
                        fn dec(&mut self) {
                            unimplemented!();
                        }
                        fn is_refcounted() -> bool {
                            true
                        }
                    }\n\n
                    """
        Str.concat(
            buf,
            impl,
        )
    else
        Str.concat(buf, "roc_refcounted_noop_impl!(${escaped_name});\n\n")

generate_roc_refcounted_named_fields = \types, fields, mode, wrapper ->
    field_name = \name ->
        escaped_name = escape_kw(name)
        when wrapper is
            Struct -> escaped_name
            Tag -> "f${escaped_name}"

    method_name =
        when mode is
            Inc -> "inc"
            Dec -> "dec"

    walker =
        \accum, { name, id } ->
            if contains_refcounted(types, Types.shape(types, id)) then
                Str.concat(
                    accum,
                    "${indent} self.${field_name(name)}.${method_name}();\n",
                )
            else
                accum

    List.walk(fields, "", walker)

# If a value or any data in it must be refcounted.
contains_refcounted = \types, type ->
    contains_refcounted_help(types, type, Set.empty({}))

contains_refcounted_help = \types, type, do_not_recurse ->
    # TODO: is doNotRecurse problematic? Do we need an updated doNotRecurse for calls up the tree?
    # I think there is a change it really only matters for RecursivePointer, so it may be fine.
    # Otherwise we need to deal with threading through updates to doNotRecurse
    when type is
        RocStr | RocList(_) | RocSet(_) | RocDict(_, _) | RocBox(_) | RecursivePointer(_) ->
            Bool.true

        Unit | Unsized | EmptyTagUnion | Num(_) | Bool | TagUnion(Enumeration(_)) ->
            Bool.false

        Function({ lambda_set: id }) ->
            contains_refcounted_help(types, Types.shape(types, id), do_not_recurse)

        RocResult(id0, id1) ->
            contains_refcounted_help(types, Types.shape(types, id0), do_not_recurse)
            || contains_refcounted_help(types, Types.shape(types, id1), do_not_recurse)

        Struct({ fields: HasNoClosure(fields) }) | TagUnionPayload({ fields: HasNoClosure(fields) }) ->
            List.any(fields, \{ id } -> contains_refcounted_help(types, Types.shape(types, id), do_not_recurse))

        Struct({ fields: HasClosure(fields) }) | TagUnionPayload({ fields: HasClosure(fields) }) ->
            List.any(fields, \{ id } -> contains_refcounted_help(types, Types.shape(types, id), do_not_recurse))

        TagUnion(SingleTagStruct({ payload: HasNoClosure(fields) })) ->
            List.any(fields, \{ id } -> contains_refcounted_help(types, Types.shape(types, id), do_not_recurse))

        TagUnion(SingleTagStruct({ payload: HasClosure(fields) })) ->
            List.any(fields, \{ id } -> contains_refcounted_help(types, Types.shape(types, id), do_not_recurse))

        TagUnion(Recursive(_)) -> Bool.true
        TagUnion(NullableWrapped(_)) -> Bool.true
        TagUnion(NonNullableUnwrapped(_)) -> Bool.true
        TagUnion(NullableUnwrapped(_)) -> Bool.true
        TagUnion(NonRecursive({ tags })) ->
            List.any(tags, \{ payload } ->
                when payload is
                    Some(id) -> contains_refcounted_help(types, Types.shape(types, id), do_not_recurse)
                    None -> Bool.false)

type_name = \types, id ->
    when Types.shape(types, id) is
        Unit -> "()"
        Unsized -> "roc_std::RocList<u8>"
        EmptyTagUnion -> "std::convert::Infallible"
        RocStr -> "roc_std::RocStr"
        Bool -> "bool"
        Num(U8) -> "u8"
        Num(U16) -> "u16"
        Num(U32) -> "u32"
        Num(U64) -> "u64"
        Num(U128) -> "u128"
        Num(I8) -> "i8"
        Num(I16) -> "i16"
        Num(I32) -> "i32"
        Num(I64) -> "i64"
        Num(I128) -> "i128"
        Num(F32) -> "f32"
        Num(F64) -> "f64"
        Num(Dec) -> "roc_std:RocDec"
        RocDict(_key, _value) ->
            # key_name = type_name(types, key)
            # value_name = type_name(types, value)
            # "roc_std::RocDict<${key_name}, ${value_name}>"
            crash("RocDict is not yet supported in rust")

        RocSet(_elem) ->
            # elem_name = type_name(types, elem)
            # "roc_std::RocSet<${elem_name}>"
            crash("RocSet is not yet supported in rust")

        RocList(elem) ->
            elem_name = type_name(types, elem)

            "roc_std::RocList<{delem_name}>"

        RocBox(elem) ->
            elem_name = type_name(types, elem)

            "roc_std::RocBox<${elem_name}>"

        RocResult(ok, err) ->
            ok_name = type_name(types, ok)
            err_name = type_name(types, err)

            "roc_std::RocResult<${ok_name}, ${err_name}>"

        RecursivePointer(content) ->
            type_name(types, content)

        Struct({ name }) -> escape_kw(name)
        TagUnionPayload({ name }) -> escape_kw(name)
        TagUnion(NonRecursive({ name })) -> escape_kw(name)
        TagUnion(Recursive({ name })) -> escape_kw(name)
        TagUnion(Enumeration({ name })) -> escape_kw(name)
        TagUnion(NullableWrapped({ name })) -> escape_kw(name)
        TagUnion(NullableUnwrapped({ name })) -> escape_kw(name)
        TagUnion(NonNullableUnwrapped({ name })) -> escape_kw(name)
        TagUnion(SingleTagStruct({ name })) -> escape_kw(name)
        Function({ function_name }) -> escape_kw(function_name)

arch_name = \arch ->
    when arch is
        Aarch32 ->
            "arm"

        Aarch64 ->
            "aarch64"

        Wasm32 ->
            "wasm32"

        X86x32 ->
            "x86"

        X86x64 ->
            "x86_64"

file_header =
    """
    // ⚠️ GENERATED CODE ⚠️ - this entire file was generated by the `roc glue` CLI command

    #![allow(unused_unsafe)]
    #![allow(dead_code)]
    #![allow(unused_mut)]
    #![allow(non_snake_case)]
    #![allow(non_camel_case_types)]
    #![allow(non_upper_case_globals)]
    #![allow(clippy::undocumented_unsafe_blocks)]
    #![allow(clippy::redundant_static_lifetimes)]
    #![allow(clippy::unused_unit)]
    #![allow(clippy::missing_safety_doc)]
    #![allow(clippy::let_and_return)]
    #![allow(clippy::missing_safety_doc)]
    #![allow(clippy::needless_borrow)]
    #![allow(clippy::clone_on_copy)]
    #![allow(clippy::non_canonical_partial_ord_impl)]


    use roc_std::RocRefcounted;
    use roc_std::roc_refcounted_noop_impl;


    """

indent = "    "
discriminant_doc_comment = "/// Returns which variant this tag union holds. Note that this never includes a payload!"

reserved_keywords = Set.from_list([
    "try",
    "abstract",
    "become",
    "box",
    "do",
    "final",
    "macro",
    "override",
    "priv",
    "typeof",
    "unsized",
    "virtual",
    "yield",
    "async",
    "await",
    "dyn",
    "as",
    "break",
    "const",
    "continue",
    "crate",
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
])

escape_kw = \input ->
    # use a raw identifier for this, to prevent a syntax error due to using a reserved keyword.
    # https://doc.rust-lang.org/rust-by-example/compatibility/raw_identifiers.html
    # another design would be to add an underscore after it; this is an experiment!
    if Set.contains(reserved_keywords, input) then
        "r#${input}"
    else
        input

next_multiple_of = \lhs, rhs ->
    when lhs % rhs is
        0 -> lhs
        r -> lhs + (rhs - r)

is_unit : Shape -> Bool
is_unit = \shape ->
    when shape is
        Unit -> Bool.true
        _ -> Bool.false

to_arg_str : List TypeId, Types, (TypeId, Shape, U64 -> Str) -> Str
to_arg_str = \args, types, fmt ->
    List.walk_with_index(args, "", \state, arg_id, index ->
        shape = Types.shape(types, arg_id)

        # Drop `()` args; they aren't FFI-safe, and nothing will get passed anyway.
        if is_unit(shape) then
            state
        else
            arg_str = fmt(arg_id, shape, index)

            if Str.is_empty(state) then
                arg_str # Don't prepend a comma if this is the first one
            else
                state
                |> Str.concat(", ")
                |> Str.concat(arg_str))

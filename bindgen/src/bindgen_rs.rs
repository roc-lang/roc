use roc_mono::layout::UnionLayout;
use roc_target::{Architecture, TargetInfo};

use crate::types::{RocTagUnion, RocType, TypeId, Types};
use std::{
    convert::TryInto,
    fmt::{self, Write},
};

pub static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
pub static HEADER: &[u8] = include_bytes!("../templates/header.rs");
const INDENT: &str = "    ";

pub fn write_types(architecture: Architecture, types: &Types, buf: &mut String) -> fmt::Result {
    for id in types.sorted_ids() {
        write_type(architecture, id, types, buf)?;
    }

    Ok(())
}

fn write_type(
    architecture: Architecture,
    id: TypeId,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    match types.get(id) {
        RocType::Struct { name, fields } => {
            write_struct(name, architecture, fields, id, types, buf)
        }
        RocType::TagUnion(tag_union) => {
            match tag_union {
                RocTagUnion::Enumeration { tags, name } => {
                    if tags.len() == 1 {
                        // An enumeration with one tag is a zero-sized unit type, so
                        // represent it as a zero-sized struct (e.g. "struct Foo()").
                        write_derive(types.get(id), types, buf)?;
                        writeln!(buf, "\nstruct {}();", type_name(id, types))
                    } else {
                        write_enumeration(
                            name,
                            architecture,
                            types.get(id),
                            tags.iter(),
                            types,
                            buf,
                        )
                    }
                }
                RocTagUnion::NonRecursive { tags, name } => {
                    // Empty tag unions can never come up at runtime,
                    // and so don't need declared types.
                    if !tags.is_empty() {
                        write_tag_union(name, architecture, id, tags, types, buf)
                    } else {
                        Ok(())
                    }
                }
                RocTagUnion::Recursive { .. } => {
                    todo!();
                }
                RocTagUnion::NullableWrapped { .. } => {
                    todo!();
                }
                RocTagUnion::NullableUnwrapped {
                    name,
                    null_tag,
                    non_null_tag,
                    non_null_payload,
                } => write_nullable_unwrapped(
                    name,
                    architecture,
                    id,
                    null_tag,
                    non_null_tag,
                    *non_null_payload,
                    types,
                    buf,
                ),
                RocTagUnion::NonNullableUnwrapped { .. } => {
                    todo!();
                }
            }
        }
        // These types don't need to be declared in Rust.
        RocType::U8
        | RocType::U16
        | RocType::U32
        | RocType::U64
        | RocType::U128
        | RocType::I8
        | RocType::I16
        | RocType::I32
        | RocType::I64
        | RocType::I128
        | RocType::F32
        | RocType::F64
        | RocType::F128
        | RocType::Bool
        | RocType::RocDec
        | RocType::RocStr
        | RocType::RocDict(_, _)
        | RocType::RocSet(_)
        | RocType::RocList(_)
        | RocType::RocBox(_) => Ok(()),
        RocType::TransparentWrapper { name, content } => {
            write_derive(types.get(id), types, buf)?;
            writeln!(
                buf,
                "#[repr(transparent)]\npub struct {name}({});",
                type_name(*content, types)
            )
        }
    }
}

fn write_discriminant(
    name: &str,
    architecture: Architecture,
    tag_names: Vec<String>,
    types: &Types,
    buf: &mut String,
) -> Result<String, fmt::Error> {
    // The tag union's discriminant, e.g.
    //
    // #[repr(u8)]
    // pub enum tag_MyTagUnion {
    //     Bar,
    //     Foo,
    // }
    let discriminant_name = format!("tag_{name}");
    let discriminant_type = RocType::TagUnion(RocTagUnion::Enumeration {
        name: discriminant_name.clone(),
        tags: tag_names.clone(),
    });

    write_enumeration(
        &discriminant_name,
        architecture,
        &discriminant_type,
        tag_names.into_iter(),
        types,
        buf,
    )?;

    Ok(discriminant_name)
}

fn write_tag_union(
    name: &str,
    architecture: Architecture,
    type_id: TypeId,
    tags: &[(String, Option<TypeId>)],
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    let tag_names = tags.iter().map(|(name, _)| name).cloned().collect();
    let discriminant_name = write_discriminant(name, architecture, tag_names, types, buf)?;
    let typ = types.get(type_id);
    // TODO also do this for other targets. Remember, these can change based on more
    // than just pointer width; e.g. on wasm, the alignments of U16 and U8 are both 4!
    let target_info = TargetInfo::from(&target_lexicon::Triple::host());
    let discriminant_offset = RocTagUnion::discriminant_offset(tags, types, target_info);
    let size = typ.size(types, target_info);

    {
        // No deriving for unions; we have to add the impls ourselves!

        writeln!(
            buf,
            r#"
#[repr(C)]
pub union {name} {{"#
        )?;

        for (tag_name, opt_payload_id) in tags {
            // If there's no payload, we don't need a variant for it.
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get(*payload_id);

                write!(buf, "{INDENT}{tag_name}: ")?;

                if payload_type.has_pointer(types) {
                    // types with pointers need ManuallyDrop
                    // because rust unions don't (and can't)
                    // know how to drop them automatically!
                    writeln!(
                        buf,
                        "core::mem::ManuallyDrop<{}>,",
                        type_name(*payload_id, types)
                    )?;
                } else {
                    writeln!(buf, "{},", type_name(*payload_id, types))?;
                }
            }
        }

        // When there's no alignment padding after the largest variant,
        // the compiler will make extra room for the discriminant.
        // We need that to be reflected in the overall size of the enum,
        // so add an extra variant with the appropriate size.
        //
        // (Do this even if theoretically shouldn't be necessary, since
        // there's no runtime cost and it more explicitly syncs the
        // union's size with what we think it should be.)
        writeln!(buf, "    _size_with_discriminant: [u8; {size}],")?;

        buf.write_str("}\n")?;
    }

    // The impl for the tag union
    {
        // An old design, which ended up not working out, was that the tag union
        // was a struct containing two fields: one for the `union`, and another
        // for the discriminant.
        //
        // The problem with this was alignment; e.g. if you have one variant with a
        // RocStr in it and another with an I128, then the `union` has a size of 32B
        // and the discriminant is right after it - making the size of the whole struct
        // round up to 48B total, since it has an alignment of 16 from the I128.
        //
        // However, Roc will generate the more efficient thing here: the whole thing will
        // be 32B, and the discriminant will appear at offset 24 - right after the end of
        // the RocStr. The current design recognizes this and works with it, by representing
        // the entire structure as a union and manually setting the tag at the appropriate offset.
        write!(
            buf,
            r#"
impl {name} {{
    pub fn tag(&self) -> {discriminant_name} {{
        unsafe {{
            let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

            core::mem::transmute::<u8, {discriminant_name}>(*bytes.as_ptr().add({discriminant_offset}))
        }}
    }}

    /// Internal helper
    fn set_discriminant(&mut self, tag: {discriminant_name}) {{
        let discriminant_ptr: *mut {discriminant_name} = (self as *mut {name}).cast();

        unsafe {{
            *(discriminant_ptr.add({discriminant_offset})) = tag;
        }}
    }}
"#
        )?;

        for (tag_name, opt_payload_id) in tags {
            // Add a convenience constructor function to the impl, e.g.
            //
            // /// Construct a tag named Foo, with the appropriate payload
            // pub fn Foo(payload: roc_std::RocStr) -> Self {
            //     Self {
            //         tag: tag_MyTagUnion::Foo,
            //         variant: variant_MyTagUnion {
            //             Foo: core::mem::ManuallyDrop::new(payload),
            //         },
            //     }
            // }
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get(*payload_id);
                let payload_type_name = type_name(*payload_id, types);

                let (init_payload, get_payload, ref_if_needed, self_for_into) =
                    if payload_type.has_pointer(types) {
                        (
                            "core::mem::ManuallyDrop::new(payload)",
                            format!("core::mem::ManuallyDrop::take(&mut self.{tag_name})",),
                            // Since this is a ManuallyDrop, our `as_` method will need
                            // to dereference the variant (e.g. `&self.Foo`)
                            "&",
                            // we need `mut self` for the argument because of ManuallyDrop
                            "mut self",
                        )
                    } else {
                        (
                            "payload",
                            format!("self.{tag_name}"),
                            // Since this is not a ManuallyDrop, our `as_` method will not
                            // want to dereference the variant (e.g. `self.Foo` with no '&')
                            "",
                            // we don't need `mut self` unless we need ManuallyDrop
                            "self",
                        )
                    };

                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// Construct a tag named {tag_name}, with the appropriate payload
    pub fn {tag_name}(payload: {payload_type_name}) -> Self {{
        let mut answer = Self {{
            {tag_name}: {init_payload}
        }};

        answer.set_discriminant({discriminant_name}::{tag_name});

        answer
    }}"#,
                )?;

                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// Unsafely assume the given {name} has a .tag() of {tag_name} and convert it to {tag_name}'s payload.
    /// (Always examine .tag() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .tag() doesn't return {tag_name}.
    pub unsafe fn into_{tag_name}({self_for_into}) -> {payload_type_name} {{
        debug_assert_eq!(self.tag(), {discriminant_name}::{tag_name});
        {get_payload}
    }}"#,
                )?;

                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// Unsafely assume the given {name} has a .tag() of {tag_name} and return its payload.
    /// (Always examine .tag() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .tag() doesn't return {tag_name}.
    pub unsafe fn as_{tag_name}(&self) -> {ref_if_needed}{payload_type_name} {{
        debug_assert_eq!(self.tag(), {discriminant_name}::{tag_name});
        {ref_if_needed}self.{tag_name}
    }}"#,
                )?;
            } else {
                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// A tag named {tag_name}, which has no payload.
    pub const {tag_name}: Self = unsafe {{
        let mut bytes = [0; core::mem::size_of::<{name}>()];

        bytes[{discriminant_offset}] = {discriminant_name}::{tag_name} as u8;

        core::mem::transmute::<[u8; core::mem::size_of::<{name}>()], {name}>(bytes)
    }};"#,
                )?;

                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// Other `into_` methods return a payload, but since the {tag_name} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub fn into_{tag_name}(self) {{
        ()
    }}"#,
                )?;

                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// Other `as` methods return a payload, but since the {tag_name} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub unsafe fn as_{tag_name}(&self) {{
        ()
    }}"#,
                )?;
            }
        }

        buf.write_str("}\n")?;
    }

    // The Drop impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl Drop for {name} {{
    fn drop(&mut self) {{"#
        )?;

        write_impl_tags(
            2,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                match opt_payload_id {
                    Some(payload_id) if types.get(payload_id).has_pointer(types) => {
                        format!("unsafe {{ core::mem::ManuallyDrop::drop(&mut self.{tag_name}) }},",)
                    }
                    _ => {
                        // If it had no payload, or if the payload had no pointers,
                        // there's nothing to clean up, so do `=> {}` for the branch.
                        "{}".to_string()
                    }
                }
            },
        )?;

        writeln!(
            buf,
            r#"    }}
}}"#
        )?;
    }

    // The PartialEq impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl PartialEq for {name} {{
    fn eq(&self, other: &Self) -> bool {{
        if self.tag() != other.tag() {{
            return false;
        }}

        unsafe {{"#
        )?;

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("self.{tag_name} == other.{tag_name},")
                } else {
                    // if the tags themselves had been unequal, we already would have
                    // early-returned with false, so this means the tags were equal
                    // and there's no payload; return true!
                    "true,".to_string()
                }
            },
        )?;

        writeln!(
            buf,
            r#"        }}
    }}
}}"#
        )?;
    }

    if !typ.has_float(types) {
        writeln!(buf, "\nimpl Eq for {name} {{}}")?;
    }

    // The PartialOrd impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl PartialOrd for {name} {{
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {{
        match self.tag().partial_cmp(&other.tag()) {{
            Some(core::cmp::Ordering::Equal) => {{}}
            not_eq => return not_eq,
        }}

        unsafe {{"#
        )?;

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("self.{tag_name}.partial_cmp(&other.{tag_name}),",)
                } else {
                    // if the tags themselves had been unequal, we already would have
                    // early-returned, so this means the tags were equal and there's
                    // no payload; return Equal!
                    "Some(core::cmp::Ordering::Equal),".to_string()
                }
            },
        )?;

        writeln!(
            buf,
            r#"        }}
    }}
}}"#
        )?;
    }

    // The Ord impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl Ord for {name} {{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {{
        match self.tag().cmp(&other.tag()) {{
            core::cmp::Ordering::Equal => {{}}
            not_eq => return not_eq,
        }}

        unsafe {{"#
        )?;

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("self.{tag_name}.cmp(&other.{tag_name}),",)
                } else {
                    // if the tags themselves had been unequal, we already would have
                    // early-returned, so this means the tags were equal and there's
                    // no payload; return Equal!
                    "core::cmp::Ordering::Equal,".to_string()
                }
            },
        )?;

        writeln!(
            buf,
            r#"        }}
    }}
}}"#
        )?;
    }

    // The Clone impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl Clone for {name} {{
    fn clone(&self) -> Self {{
        let mut answer = unsafe {{"#
        )?;

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!(
                        r#"Self {{
                        {tag_name}: self.{tag_name}.clone(),
                    }},"#,
                    )
                } else {
                    // when there's no payload, initialize to garbage memory.
                    format!(
                        r#"core::mem::transmute::<
                        core::mem::MaybeUninit<{name}>,
                        {name},
                    >(core::mem::MaybeUninit::uninit()),"#,
                    )
                }
            },
        )?;

        writeln!(
            buf,
            r#"
        }};

        answer.set_discriminant(self.tag());

        answer
    }}
}}"#
        )?;
    }

    if !typ.has_pointer(types) {
        writeln!(buf, "impl Copy for {name} {{}}\n")?;
    }

    // The Hash impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl core::hash::Hash for {name} {{
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {{"#
        )?;

        write_impl_tags(
            2,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                let hash_tag = format!("{discriminant_name}::{tag_name}.hash(state)");

                if opt_payload_id.is_some() {
                    format!(
                        r#"unsafe {{
                {hash_tag};
                self.{tag_name}.hash(state);
            }},"#
                    )
                } else {
                    format!("{},", hash_tag)
                }
            },
        )?;

        writeln!(
            buf,
            r#"    }}
}}"#
        )?;
    }

    // The Debug impl for the tag union
    {
        writeln!(
            buf,
            r#"
impl core::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        f.write_str("{name}::")?;

        unsafe {{"#
        )?;

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| match opt_payload_id {
                Some(payload_id) => {
                    // If it's a ManuallyDrop, we need a `*` prefix to dereference it
                    // (because otherwise we're using ManuallyDrop's Debug instance
                    // rather than the Debug instance of the value it wraps).
                    let deref_str = if types.get(payload_id).has_pointer(types) {
                        "&*"
                    } else {
                        "&"
                    };

                    format!(
                        r#"f.debug_tuple("{tag_name}").field({deref_str}self.{tag_name}).finish(),"#,
                    )
                }
                None => format!(r#"f.write_str("{tag_name}"),"#),
            },
        )?;

        writeln!(
            buf,
            r#"        }}
    }}
}}
"#
        )?;
    }

    Ok(())
}

fn write_impl_tags<
    'a,
    I: IntoIterator<Item = &'a (String, Option<TypeId>)>,
    F: Fn(&str, Option<TypeId>) -> String,
>(
    indentations: usize,
    tags: I,
    discriminant_name: &str,
    buf: &mut String,
    to_branch_str: F,
) -> fmt::Result {
    for _ in 0..indentations {
        buf.write_str(INDENT)?;
    }

    buf.write_str("match self.tag() {\n")?;

    for (tag_name, opt_payload_id) in tags {
        let branch_str = to_branch_str(tag_name, *opt_payload_id);

        for _ in 0..(indentations + 1) {
            buf.write_str(INDENT)?;
        }

        writeln!(buf, "{discriminant_name}::{tag_name} => {branch_str}")?;
    }

    for _ in 0..indentations {
        buf.write_str(INDENT)?;
    }

    buf.write_str("}\n")?;

    Ok(())
}

fn write_enumeration<I: ExactSizeIterator<Item = S>, S: AsRef<str> + fmt::Display>(
    name: &str,
    architecture: Architecture,
    typ: &RocType,
    tags: I,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    let tag_bytes: usize = UnionLayout::discriminant_size(tags.len())
        .stack_size()
        .try_into()
        .unwrap();

    write_arch_cfg(architecture, 0, buf)?;
    write_derive(typ, types, buf)?;

    // e.g. "#[repr(u8)]\npub enum Foo {\n"
    writeln!(buf, "#[repr(u{})]\npub enum {name} {{", tag_bytes * 8)?;

    let mut debug_buf = String::new();

    for (index, tag_name) in tags.enumerate() {
        writeln!(buf, "{INDENT}{tag_name} = {index},")?;

        debug_buf.push_str(&format!(
            r#"{INDENT}{INDENT}{INDENT}Self::{tag_name} => f.write_str("{name}::{tag_name}"),
"#
        ));
    }

    writeln!(
        buf,
        r#"}}

impl core::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        match self {{
{debug_buf}        }}
    }}
}}"#
    )
}

fn write_struct(
    name: &str,
    architecture: Architecture,
    fields: &[(String, TypeId)],
    struct_id: TypeId,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    match fields.len() {
        0 => {
            // An empty record is zero-sized and won't end up being passed to/from the host.
            Ok(())
        }
        1 => {
            // Unwrap single-field records
            write_type(architecture, fields.first().unwrap().1, types, buf)
        }
        _ => {
            write_arch_cfg(architecture, 0, buf)?;
            write_derive(types.get(struct_id), types, buf)?;

            writeln!(buf, "#[repr(C)]\npub struct {name} {{")?;

            for (label, field_id) in fields {
                writeln!(
                    buf,
                    "{INDENT}pub {}: {},",
                    label.as_str(),
                    type_name(*field_id, types)
                )?;
            }

            buf.write_str("}\n")
        }
    }
}

fn type_name(id: TypeId, types: &Types) -> String {
    match types.get(id) {
        RocType::U8 => "u8".to_string(),
        RocType::U16 => "u16".to_string(),
        RocType::U32 => "u32".to_string(),
        RocType::U64 => "u64".to_string(),
        RocType::U128 => "roc_std::U128".to_string(),
        RocType::I8 => "i8".to_string(),
        RocType::I16 => "i16".to_string(),
        RocType::I32 => "i32".to_string(),
        RocType::I64 => "i64".to_string(),
        RocType::I128 => "roc_std::I128".to_string(),
        RocType::F32 => "f32".to_string(),
        RocType::F64 => "f64".to_string(),
        RocType::F128 => "roc_std::F128".to_string(),
        RocType::Bool => "bool".to_string(),
        RocType::RocDec => "roc_std::RocDec".to_string(),
        RocType::RocStr => "roc_std::RocStr".to_string(),
        RocType::RocDict(key_id, val_id) => format!(
            "roc_std::RocDict<{}, {}>",
            type_name(*key_id, types),
            type_name(*val_id, types)
        ),
        RocType::RocSet(elem_id) => format!("roc_std::RocSet<{}>", type_name(*elem_id, types)),
        RocType::RocList(elem_id) => format!("roc_std::RocList<{}>", type_name(*elem_id, types)),
        RocType::RocBox(elem_id) => format!("roc_std::RocBox<{}>", type_name(*elem_id, types)),
        RocType::Struct { name, .. }
        | RocType::TransparentWrapper { name, .. }
        | RocType::TagUnion(RocTagUnion::NonRecursive { name, .. })
        | RocType::TagUnion(RocTagUnion::Recursive { name, .. })
        | RocType::TagUnion(RocTagUnion::Enumeration { name, .. })
        | RocType::TagUnion(RocTagUnion::NullableWrapped { name, .. })
        | RocType::TagUnion(RocTagUnion::NullableUnwrapped { name, .. })
        | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { name, .. }) => name.clone(),
    }
}

fn write_derive(typ: &RocType, types: &Types, buf: &mut String) -> fmt::Result {
    buf.write_str("\n#[derive(Clone, ")?;

    if !typ.has_pointer(types) {
        buf.write_str("Copy, ")?;
    }

    if !typ.has_enumeration(types) {
        buf.write_str("Debug, Default, ")?;
    }

    if !typ.has_float(types) {
        buf.write_str("Eq, Ord, Hash, ")?;
    }

    buf.write_str("PartialEq, PartialOrd)]\n")
}

fn write_nullable_unwrapped(
    name: &str,
    architecture: Architecture,
    id: TypeId,
    null_tag: &str,
    non_null_tag: &str,
    non_null_payload: TypeId,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    let mut tag_names = vec![null_tag.to_string(), non_null_tag.to_string()];

    tag_names.sort();

    let discriminant_name = write_discriminant(name, architecture, tag_names, types, buf)?;
    let payload_type = types.get(non_null_payload);
    let payload_type_name = type_name(non_null_payload, types);
    let has_pointer = payload_type.has_pointer(types);
    let (wrapped_payload_type_name, init_payload, ref_if_needed) = if has_pointer {
        (
            format!("core::mem::ManuallyDrop<{payload_type_name}>"),
            "core::mem::ManuallyDrop::new(payload)",
            "&",
        )
    } else {
        (payload_type_name.clone(), "payload", "")
    };

    write_derive(types.get(id), types, buf)?;

    write!(
        buf,
        r#"#[repr(C)]
pub struct {name} {{
    pointer: *mut {wrapped_payload_type_name},
}}
"#
    )?;

    // The impl for the tag union
    {
        write!(
            buf,
            r#"
impl {name} {{
    pub fn tag(&self) -> {discriminant_name} {{
        if self.pointer.is_null() {{
            {discriminant_name}::{null_tag}
        }} else {{
            {discriminant_name}::{non_null_tag}
        }}
    }}
"#
        )?;
    }

    {
        // Add a convenience constructor function for the tag with the payload, e.g.
        //
        // /// Construct a tag named Cons, with the appropriate payload
        // pub fn Cons(payload: roc_std::RocStr) -> Self {
        //     let size = core::mem::size_of::<roc_std::RocStr>();
        //     let align = core::mem::align_of::<roc_std::RocStr>();
        //
        //     unsafe {
        //         let pointer =
        //             roc_alloc(size, align as u32) as *mut core::mem::ManuallyDrop<roc_std::RocStr>;
        //
        //         *pointer = core::mem::ManuallyDrop::new(payload);
        //
        //         Self { pointer }
        //     }
        // }
        writeln!(
            buf,
            // Don't use indoc because this must be indented once!
            r#"
    /// Construct a tag named {non_null_tag}, with the appropriate payload
    pub fn {non_null_tag}(payload: {payload_type_name}) -> Self {{
        let size = core::mem::size_of::<{payload_type_name}>();
        let align = core::mem::align_of::<{payload_type_name}>();

        unsafe {{
            let pointer = crate::roc_alloc(size, align as u32) as *mut {wrapped_payload_type_name};

            *pointer = {init_payload};

            Self {{ pointer }}
        }}
    }}"#,
        )?;

        let assign_payload = if has_pointer {
            "core::mem::ManuallyDrop::take(&mut *self.pointer)"
        } else {
            "*self.pointer"
        };

        writeln!(
            buf,
            // Don't use indoc because this must be indented once!
            r#"
    /// Unsafely assume the given {name} has a .tag() of {non_null_tag} and convert it to {non_null_tag}'s payload.
    /// (Always examine .tag() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .tag() doesn't return {non_null_tag}.
    pub unsafe fn into_{non_null_tag}(self) -> {payload_type_name} {{
        debug_assert_eq!(self.tag(), {discriminant_name}::{non_null_tag});

        let payload = {assign_payload};
        let align = core::mem::align_of::<{payload_type_name}>() as u32;

        crate::roc_dealloc(self.pointer as *mut core::ffi::c_void, align);

        payload
    }}"#,
        )?;

        writeln!(
            buf,
            // Don't use indoc because this must be indented once!
            r#"
    /// Unsafely assume the given {name} has a .tag() of {non_null_tag} and return its payload.
    /// (Always examine .tag() first to make sure this is the correct variant!)
    /// Panics in debug builds if the .tag() doesn't return {non_null_tag}.
    pub unsafe fn as_{non_null_tag}(&self) -> {ref_if_needed}{payload_type_name} {{
        debug_assert_eq!(self.tag(), {discriminant_name}::{non_null_tag});
        {ref_if_needed}*self.pointer
    }}"#,
        )?;
    }

    {
        // Add a convenience constructor function for the nullable tag, e.g.
        //
        // /// Construct a tag named Nil
        // pub fn Nil() -> Self {
        //     Self {
        //         pointer: core::ptr::null_mut(),
        //     }
        // }
        writeln!(
            buf,
            // Don't use indoc because this must be indented once!
            r#"
    /// Construct a tag named {null_tag}
    pub fn {null_tag}() -> Self {{
        Self {{
            pointer: core::ptr::null_mut(),
        }}
    }}"#,
        )?;

        writeln!(
            buf,
            // Don't use indoc because this must be indented once!
            r#"
    /// Other `into_` methods return a payload, but since the {null_tag} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub fn into_{null_tag}(self) {{
        ()
    }}"#,
        )?;

        writeln!(
            buf,
            // Don't use indoc because this must be indented once!
            r#"
    /// Other `as` methods return a payload, but since the {null_tag} tag
    /// has no payload, this does nothing and is only here for completeness.
    pub unsafe fn as_{null_tag}(&self) {{
        ()
    }}"#,
        )?;
    }

    buf.write_str("}\n")?;

    // The Drop impl for the tag union
    {
        write!(
            buf,
            r#"
impl Drop for {name} {{
    fn drop(&mut self) {{
        if !self.pointer.is_null() {{
            let payload = unsafe {{ &*self.pointer }};
            let align = core::mem::align_of::<{payload_type_name}>() as u32;

            unsafe {{
                crate::roc_dealloc(self.pointer as *mut core::ffi::c_void, align);
            }}

            drop(payload);
        }}
    }}
}}
"#
        )?;
    }

    // The Debug impl for the tag union
    {
        let extra_deref = if has_pointer { "*" } else { "" };

        write!(
            buf,
            r#"
impl core::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
        if self.pointer.is_null() {{
            f.write_str("{name}::{null_tag}")
        }} else {{
            f.write_str("{name}::")?;

            unsafe {{ f.debug_tuple("{non_null_tag}").field(&*{extra_deref}self.pointer).finish() }}
        }}
    }}
}}

"#
        )?;
    }

    Ok(())
}

fn write_arch_cfg(
    architecture: Architecture,
    indentations: usize,
    buf: &mut String,
) -> fmt::Result {
    let arch_cfg_str = match architecture {
        Architecture::X86_64 => "x86_64",
        Architecture::X86_32 => "x86",
        Architecture::Aarch64 => "aarch64",
        Architecture::Arm => "arm",
        Architecture::Wasm32 => "wasm32",
    };

    for _ in 0..indentations {
        buf.write_str(INDENT)?;
    }

    write!(buf, "\n#[cfg(target_arch = \"{arch_cfg_str}\")]")
}

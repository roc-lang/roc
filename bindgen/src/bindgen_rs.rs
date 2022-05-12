use roc_mono::layout::UnionLayout;

use indoc::indoc;

use crate::types::{RocType, TypeId, Types};
use std::{
    convert::TryInto,
    fmt::{self, Write},
};

pub static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
pub static HEADER: &[u8] = include_bytes!("../templates/header.rs");
const INDENT: &str = "    ";

pub fn write_types(types: &Types, buf: &mut String) -> fmt::Result {
    for id in types.sorted_ids() {
        match types.get(id) {
            RocType::Struct { name, fields } => write_struct(name, fields, id, types, buf)?,
            RocType::Enumeration { tags, name } => {
                if tags.len() == 1 {
                    // An enumeration with one tag is a zero-sized unit type, so
                    // represent it as a zero-sized struct (e.g. "struct Foo()").
                    write_deriving(types.get(id), types, buf)?;
                    writeln!(buf, "\nstruct {}();", type_name(id, types))?;
                } else {
                    write_enumeration(name, types.get(id), tags.iter(), types, buf)?;
                }
            }
            RocType::TagUnion { tags, name } => {
                // Empty tag unions can never come up at runtime,
                // and so don't need declared types.
                if !tags.is_empty() {
                    write_tag_union(name, id, tags, types, buf)?;
                }
            }
            RocType::RecursiveTagUnion { .. } => {
                todo!();
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
            | RocType::RocBox(_) => {}
            RocType::TransparentWrapper { name, content } => {
                write_deriving(types.get(id), types, buf)?;
                writeln!(
                    buf,
                    "#[repr(transparent)]\npub struct {}({});",
                    name,
                    type_name(*content, types)
                )?;
            }
        }
    }

    Ok(())
}

fn write_tag_union(
    name: &str,
    type_id: TypeId,
    tags: &[(String, Option<TypeId>)],
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    // The tag union's discriminant, e.g.
    //
    // #[repr(u8)]
    // pub enum tag_MyTagUnion {
    //     Bar,
    //     Foo,
    // }
    let discriminant_name = format!("tag_{}", name);
    let tag_names = tags.iter().map(|(name, _)| name);
    let discriminant_type = RocType::Enumeration {
        name: discriminant_name.clone(),
        tags: tag_names.clone().cloned().collect(),
    };
    let typ = types.get(type_id);

    write_enumeration(
        &discriminant_name,
        &discriminant_type,
        tag_names,
        types,
        buf,
    )?;

    // The tag union's variant union, e.g.
    //
    // #[repr(C)]
    // union union_MyTagUnion {
    //     Bar: u128,
    //     Foo: core::mem::ManuallyDrop<roc_std::RocStr>,
    // }
    let variant_name = format!("union_{}", name);

    {
        // No deriving for unions; we have to add the impls ourselves!

        writeln!(buf, "\n#[repr(C)]\npub union {} {{", variant_name)?;

        for (tag_name, opt_payload_id) in tags {
            // If there's no payload, we don't need a variant for it.
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get(*payload_id);

                write!(buf, "{}{}: ", INDENT, tag_name)?;

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

        buf.write_str("}\n")?;
    }

    // The tag union struct itself, e.g.
    //
    // #[repr(C)]
    // pub struct MyTagUnion {
    //     variant: variant_MyTagUnion,
    //     tag: tag_MyTagUnion,
    // }
    {
        // no deriving because it contains a union; we have to
        // generate the impls explicitly!

        write!(
            buf,
            "\n#[repr(C)]\npub struct {} {{\n{}variant: {},\n{}tag: {},\n}}\n",
            name, INDENT, variant_name, INDENT, discriminant_name
        )?;
    }

    // The impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"

                    impl MyTagUnion {{
                        pub fn tag(&self) -> {} {{
                            self.tag
                        }}

                        pub fn variant(&self) -> &{} {{
                            &self.variant
                        }}
                "#
            ),
            discriminant_name, variant_name
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

                let payload_name = if payload_type.has_pointer(types) {
                    "core::mem::ManuallyDrop::new(payload)"
                } else {
                    "payload"
                };

                let payload_type_name = type_name(*payload_id, types);

                writeln!(
                    buf,
                    // Don't use indoc because this must be indented once!
                    r#"
    /// Construct a tag named {}, with the appropriate payload
    pub fn {}(payload: {}) -> Self {{
        Self {{
            tag: {}::{},
            variant: {} {{
                {}: {}
            }},
        }}
    }}"#,
                    tag_name,
                    tag_name,
                    payload_type_name,
                    discriminant_name,
                    tag_name,
                    variant_name,
                    tag_name,
                    payload_name
                )?;
            }
        }

        buf.write_str("}\n")?;
    }

    // The Drop impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"

                    impl Drop for {} {{
                        fn drop(&mut self) {{
                            match self.tag {{
                "#
            ),
            name
        )?;

        write_impl_tags(
            3,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                match opt_payload_id {
                    Some(payload_id) if types.get(payload_id).has_pointer(types) => {
                        format!(
                            "unsafe {{ core::mem::ManuallyDrop::drop(&mut self.variant.{}) }},",
                            tag_name
                        )
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
            indoc!(
                r#"
                            }}
                        }}
                    }}
                "#
            ),
        )?;
    }

    // The PartialEq impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"
                    impl PartialEq for {} {{
                        fn eq(&self, other: &Self) -> bool {{
                            if self.tag != other.tag {{
                                return false;
                            }}

                            unsafe {{
                                match self.tag {{
                "#
            ),
            name
        )?;

        write_impl_tags(
            4,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!("self.variant.{} == other.variant.{},", tag_name, tag_name)
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
            indoc!(
                r#"
                                }}
                            }}
                        }}
                    }}
                "#
            ),
        )?;
    }

    if !typ.has_float(types) {
        writeln!(buf, "impl Eq for {} {{}}\n", name)?;
    }

    // The PartialOrd impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"
                    impl PartialOrd for {} {{
                        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {{
                            match self.tag.partial_cmp(&other.tag) {{
                                Some(core::cmp::Ordering::Equal) => {{}}
                                not_eq => return not_eq,
                            }}

                            unsafe {{
                                match self.tag {{
                "#
            ),
            name
        )?;

        write_impl_tags(
            4,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!(
                        "self.variant.{}.partial_cmp(&other.variant.{}),",
                        tag_name, tag_name
                    )
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
            indoc!(
                r#"
                                }}
                            }}
                        }}
                    }}
                "#
            ),
        )?;
    }

    // The Ord impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"
                    impl Ord for {} {{
                        fn cmp(&self, other: &Self) -> core::cmp::Ordering {{
                            match self.tag.cmp(&other.tag) {{
                                core::cmp::Ordering::Equal => {{}}
                                not_eq => return not_eq,
                            }}

                            unsafe {{
                                match self.tag {{
                "#
            ),
            name
        )?;

        write_impl_tags(
            4,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!(
                        "self.variant.{}.cmp(&other.variant.{}),",
                        tag_name, tag_name
                    )
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
            indoc!(
                r#"
                                }}
                            }}
                        }}
                    }}
                "#
            ),
        )?;
    }

    // The Clone impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"
                    impl Clone for {} {{
                        fn clone(&self) -> Self {{
                            match self.tag {{
                "#
            ),
            name
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
                variant: {} {{
                    {}: unsafe {{ self.variant.{}.clone() }},
                }},
                tag: {}::{},
            }},"#,
                        variant_name, tag_name, tag_name, discriminant_name, tag_name
                    )
                } else {
                    // when there's no payload, we set the clone's `variant` field to
                    // garbage memory
                    format!(
                        r#"Self {{
                variant: unsafe {{
                    core::mem::transmute::<
                        core::mem::MaybeUninit<{}>,
                        {},
                    >(core::mem::MaybeUninit::uninit())
                }},
                tag: {}::{},
            }},"#,
                        variant_name, variant_name, discriminant_name, tag_name
                    )
                }
            },
        )?;

        writeln!(
            buf,
            indoc!(
                r#"
                            }}
                        }}
                    }}
                "#
            ),
        )?;
    }

    if !typ.has_pointer(types) {
        writeln!(buf, "impl Copy for {} {{}}\n", name)?;
    }

    // The Debug impl for the tag union
    {
        write!(
            buf,
            indoc!(
                r#"
                    impl core::fmt::Debug for {} {{
                        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
                            f.write_str("{}::")?;

                            unsafe {{
                                match self.tag {{
                "#
            ),
            name, name
        )?;

        write_impl_tags(
            4,
            tags.iter(),
            &discriminant_name,
            buf,
            |tag_name, opt_payload_id| {
                if opt_payload_id.is_some() {
                    format!(
                        r#"f.debug_tuple("{}").field(&self.variant.{}).finish(),"#,
                        tag_name, tag_name
                    )
                } else {
                    format!(r#"f.write_str("{}"),"#, tag_name)
                }
            },
        )?;

        writeln!(
            buf,
            indoc!(
                r#"
                                }}
                            }}
                        }}
                    }}
                "#
            ),
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
    for (tag_name, opt_payload_id) in tags {
        let branch_str = to_branch_str(tag_name, *opt_payload_id);

        for _ in 0..indentations {
            buf.write_str(INDENT)?;
        }

        writeln!(buf, "{}::{} => {}", discriminant_name, tag_name, branch_str)?;
    }

    Ok(())
}

fn write_enumeration<I: ExactSizeIterator<Item = S>, S: AsRef<str>>(
    name: &str,
    typ: &RocType,
    tags: I,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    let tag_bytes: usize = UnionLayout::discriminant_size(tags.len())
        .stack_size()
        .try_into()
        .unwrap();

    write_deriving(typ, types, buf)?;

    // e.g. "#[repr(u8)]\npub enum Foo {\n"
    writeln!(buf, "#[repr(u{})]\npub enum {} {{", tag_bytes * 8, name)?;

    for (index, name) in tags.enumerate() {
        writeln!(buf, "{}{} = {},", INDENT, name.as_ref(), index)?;
    }

    buf.write_str("}\n")
}

fn write_struct(
    name: &str,
    fields: &[(String, TypeId)],
    struct_id: TypeId,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    write_deriving(types.get(struct_id), types, buf)?;

    writeln!(buf, "#[repr(C)]\npub struct {} {{", name)?;

    for (label, field_id) in fields {
        writeln!(
            buf,
            "{}{}: {},",
            INDENT,
            label.as_str(),
            type_name(*field_id, types)
        )?;
    }

    buf.write_str("}\n")
}

fn type_name(id: TypeId, types: &Types) -> String {
    match types.get(id) {
        RocType::U8 => "u8".to_string(),
        RocType::U16 => "u16".to_string(),
        RocType::U32 => "u32".to_string(),
        RocType::U64 => "u64".to_string(),
        RocType::U128 => "u128".to_string(),
        RocType::I8 => "i8".to_string(),
        RocType::I16 => "i16".to_string(),
        RocType::I32 => "i32".to_string(),
        RocType::I64 => "i64".to_string(),
        RocType::I128 => "i128".to_string(),
        RocType::F32 => "f32".to_string(),
        RocType::F64 => "f64".to_string(),
        RocType::F128 => "f128".to_string(),
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
        | RocType::TagUnion { name, .. }
        | RocType::TransparentWrapper { name, .. }
        | RocType::Enumeration { name, .. }
        | RocType::RecursiveTagUnion { name, .. } => name.clone(),
    }
}

fn write_deriving(typ: &RocType, types: &Types, buf: &mut String) -> fmt::Result {
    buf.write_str("\n#[derive(Clone, PartialEq, PartialOrd, ")?;

    if !typ.has_pointer(types) {
        buf.write_str("Copy, ")?;
    }

    if !typ.has_enumeration(types) {
        buf.write_str("Default, ")?;
    }

    if !typ.has_float(types) {
        buf.write_str("Eq, Ord, Hash, ")?;
    }

    buf.write_str("Debug)]\n")
}

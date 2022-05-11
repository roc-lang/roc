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
                    buf.write_str("\nstruct ")?;
                    write_type_name(id, types, buf)?;
                    buf.write_str("();\n")?;
                } else {
                    write_enumeration(name, types.get(id), tags.into_iter(), types, buf)?;
                }
            }
            RocType::TagUnion { tags, name } => {
                // Empty tag unions can never come up at runtime,
                // and so don't need declared types.
                if tags.len() > 0 {
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
                write!(buf, "#[repr(transparent)]\npub struct {}(", name)?;
                write_type_name(*content, types, buf)?;
                buf.write_str(");\n")?;
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
    // union variant_MyTagUnion {
    //     Bar: u128,
    //     Foo: std::mem::ManuallyDrop<roc_std::RocStr>,
    // }
    let variant_name = format!("variant_{}", name);

    {
        // No deriving for unions; we have to add the impls ourselves!

        writeln!(
            buf,
            "\n#[repr(C)]\n#[allow(clippy::non_snake_case)]\npub union {} {{",
            variant_name
        )?;

        for (tag_name, opt_payload_id) in tags {
            // If there's no payload, we don't need a variant for it.
            if let Some(payload_id) = opt_payload_id {
                let payload_type = types.get(*payload_id);

                write!(buf, "{}{}: ", INDENT, tag_name)?;

                if payload_type.has_pointer(types) {
                    // types with pointers need ManuallyDrop
                    // because rust unions don't (and can't)
                    // know how to drop them automatically!
                    buf.write_str("std::mem::ManuallyDrop<")?;
                    write_type_name(*payload_id, types, buf)?;
                    buf.write_char('>')?;
                } else {
                    write_type_name(*payload_id, types, buf)?;
                };

                buf.write_str(",\n")?;
            }
        }

        buf.write_str("}\n")?;
    }

    // The tag union struct itself, e.g.
    //
    // #[repr(C)]
    // pub struct MyTagUnion {
    //     tag: tag_MyTagUnion,
    //     variant: variant_MyTagUnion,
    // }
    {
        write_deriving(types.get(type_id), types, buf)?;

        write!(
            buf,
            "#[repr(C)]\npub struct {} {{\n{}tag: {},\n{}variant: {}\n}}\n",
            name, INDENT, discriminant_name, INDENT, variant_name
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
                            self.variant
                        }}

                        pub fn into_variant(self) -> {} {{
                            self.variant
                        }}
                "#
            ),
            discriminant_name, variant_name, variant_name
        )?;

        buf.write_str("}\n")?;
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
    let tags = tags.into_iter();
    let tag_bytes: usize = UnionLayout::discriminant_size(tags.len())
        .stack_size()
        .try_into()
        .unwrap();

    write_deriving(typ, types, buf)?;

    // e.g. "#[repr(u8)]\npub enum Foo {\n"
    writeln!(buf, "#[repr(u{})]\npub enum {} {{", tag_bytes * 8, name)?;

    for name in tags {
        writeln!(buf, "{}{},", INDENT, name.as_ref())?;
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
        write!(buf, "{}{}: ", INDENT, label.as_str())?;
        write_type_name(*field_id, types, buf)?;
        buf.write_str(",\n")?;
    }

    buf.write_str("}\n")
}

fn write_type_name(id: TypeId, types: &Types, buf: &mut String) -> fmt::Result {
    match types.get(id) {
        RocType::U8 => buf.write_str("u8"),
        RocType::U16 => buf.write_str("u16"),
        RocType::U32 => buf.write_str("u32"),
        RocType::U64 => buf.write_str("u64"),
        RocType::U128 => buf.write_str("u128"),
        RocType::I8 => buf.write_str("i8"),
        RocType::I16 => buf.write_str("i16"),
        RocType::I32 => buf.write_str("i32"),
        RocType::I64 => buf.write_str("i64"),
        RocType::I128 => buf.write_str("i128"),
        RocType::F32 => buf.write_str("f32"),
        RocType::F64 => buf.write_str("f64"),
        RocType::F128 => buf.write_str("f128"),
        RocType::Bool => buf.write_str("bool"),
        RocType::RocDec => buf.write_str("roc_std::RocDec"),
        RocType::RocStr => buf.write_str("roc_std::RocStr"),
        RocType::RocDict(key_id, val_id) => {
            buf.write_str("roc_std::RocDict<")?;
            write_type_name(*key_id, types, buf)?;
            buf.write_str(", ")?;
            write_type_name(*val_id, types, buf)?;
            buf.write_char('>')
        }
        RocType::RocSet(elem_id) => {
            buf.write_str("roc_std::RocSet<")?;
            write_type_name(*elem_id, types, buf)?;
            buf.write_char('>')
        }
        RocType::RocList(elem_id) => {
            buf.write_str("roc_std::RocList<")?;
            write_type_name(*elem_id, types, buf)?;
            buf.write_char('>')
        }
        RocType::RocBox(elem_id) => {
            buf.write_str("roc_std::RocBox<")?;
            write_type_name(*elem_id, types, buf)?;
            buf.write_char('>')
        }
        RocType::Struct { name, .. }
        | RocType::TagUnion { name, .. }
        | RocType::TransparentWrapper { name, .. }
        | RocType::Enumeration { name, .. }
        | RocType::RecursiveTagUnion { name, .. } => buf.write_str(name),
    }
}

fn write_deriving(typ: &RocType, types: &Types, buf: &mut String) -> fmt::Result {
    buf.write_str("\n#[derive(Clone, PartialEq, PartialOrd, ")?;

    if !typ.has_pointer(types) {
        buf.write_str("Copy, ")?;
    }

    if !typ.has_tag_union(types) {
        buf.write_str("Default, ")?;
    }

    if !typ.has_float(types) {
        buf.write_str("Eq, Ord, Hash, ")?;
    }

    buf.write_str("Debug)]\n")
}

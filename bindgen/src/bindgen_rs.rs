use roc_mono::layout::UnionLayout;

use crate::types::{RocType, TypeId, Types};
use std::{
    convert::TryInto,
    fmt::{self, Write},
};

pub static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
pub static HEADER: &[u8] = include_bytes!("../templates/header.rs");
static INDENT: &str = "    ";

pub fn write_types(types: &Types, buf: &mut String) -> fmt::Result {
    for id in types.sorted_ids() {
        match types.get(id) {
            RocType::Struct { name, fields } => write_struct(name, fields, id, types, buf)?,
            RocType::Enumeration { tags, name } => {
                if tags.len() == 1 {
                    // An enumeration with one tag is a zero-sized unit type, so
                    // represent it as a zero-sized struct (e.g. "struct Foo()").
                    write_deriving(id, types, buf)?;
                    buf.write_str("\nstruct ")?;
                    write_type_name(id, types, buf)?;
                    buf.write_str("();\n")?;
                } else {
                    write_enum(name, id, tags.into_iter(), types, buf)?;
                }
            }
            RocType::TagUnion {
                discriminant,
                tags,
                name,
            } => {
                // Empty tag unions can never come up at runtime,
                // and so don't need declared types.
                if tags.len() > 0 {
                    write_tag_union(name, tags, *discriminant, types, buf)?;
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
                write_deriving(id, types, buf)?;
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
    tags: &[(String, Vec<TypeId>)],
    discriminant: TypeId,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    // the tag union's discriminant, e.g.
    //
    // #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
    // #[repr(u8)]
    // pub enum tag_MyTagUnion {
    //     Bar,
    //     Foo,
    // }
    write_enum(
        &format!("tag_{}", name),
        discriminant,
        tags.iter().map(|(name, _)| name),
        types,
        buf,
    )?;

    Ok(())
}

fn write_enum<I: ExactSizeIterator<Item = S>, S: AsRef<str>>(
    name: &str,
    type_id: TypeId,
    tags: I,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    write_deriving(type_id, types, buf)?;

    let tags = tags.into_iter();
    let tag_bytes: usize = UnionLayout::discriminant_size(tags.len())
        .stack_size()
        .try_into()
        .unwrap();

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
    write_deriving(struct_id, types, buf)?;

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

fn write_deriving(id: TypeId, types: &Types, buf: &mut String) -> fmt::Result {
    let typ = types.get(id);

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

use crate::types::{RocType, TypeId, Types};
use std::fmt::{self, Write};

static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
static HEADER: &[u8] = include_bytes!("../templates/header.rs");
static INDENT: &str = "    ";

pub fn write_types<'a>(types: &Types) -> Result<String, fmt::Error> {
    let mut buf = std::str::from_utf8(HEADER).unwrap().to_string();

    for id in types.sorted_ids() {
        match types.get(id) {
            RocType::Struct { name, fields } => write_struct(name, fields, id, types, &mut buf)?,
            RocType::TagUnion { name, tags } => {
                todo!();
            }
            RocType::RecursiveTagUnion { name, tags } => {
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
        }
    }

    Ok(buf)
}

fn write_struct(
    name: &str,
    fields: &[(String, TypeId)],
    struct_id: TypeId,
    types: &Types,
    buf: &mut String,
) -> fmt::Result {
    write_deriving(struct_id, types, buf)?;

    buf.write_str("\n#[repr(C)]\npub struct ")?;
    buf.write_str(name);
    buf.write_str(" {\n")?;

    for (label, field_id) in fields {
        buf.write_str(INDENT)?;
        buf.write_str(label.as_str())?;
        buf.write_str(": ")?;
        write_type_name(*field_id, types, buf)?;
        buf.write_str(",\n")?;
    }

    buf.write_str("}\n")
}

fn write_type_name<'a>(id: TypeId, types: &Types, buf: &mut String) -> fmt::Result {
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
        RocType::RocList(elem_type) => {
            buf.write_str("roc_std::RocList<")?;
            write_type_name(id, types, buf)?;
            buf.write_char('>')
        }
        RocType::RocBox(elem_type) => {
            buf.write_str("roc_std::RocBox<")?;
            write_type_name(id, types, buf)?;
            buf.write_char('>')
        }
        RocType::Struct { name, .. }
        | RocType::TagUnion { name, .. }
        | RocType::RecursiveTagUnion { name, .. } => buf.write_str(name),
    }
}

fn write_deriving<'a>(id: TypeId, types: &Types, buf: &mut String) -> fmt::Result {
    let typ = types.get(id);

    buf.write_str("#[derive(Clone, PartialEq, PartialOrd, ")?;

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

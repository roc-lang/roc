use crate::enums::Enums;
use crate::structs::Structs;
use crate::types::RocType;
use roc_module::symbol::Symbol;
use roc_mono::layout::Layout;
use roc_types::subs::{Subs, Variable};

use std::{
    fmt::{self, Write},
    io,
};

static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
static INDENT: &str = "    ";

pub fn write_template(writer: &mut impl io::Write) -> io::Result<()> {
    writer.write(TEMPLATE)?;

    Ok(())
}

struct RocRecord {
    name: String,
}

pub struct Required {
    name: String,
    args: Vec<RocType>,
    ret: RocType,
}

pub fn write_bindings(_writer: &mut impl io::Write) -> io::Result<()> {
    // extern "C" {
    //     #[link_name = "roc__mainForHost_1_exposed"]
    //     fn roc_main() -> RocStr;
    // }

    Ok(())
}

// pub fn declare_roc_type(roc_type: RocType, uid: &mut u32, buf: &mut String) {
// }

pub fn write_roc_type(
    roc_type: RocType,
    structs: &mut Structs,
    enums: &mut Enums,
    buf: &mut String,
) -> fmt::Result {
    match roc_type {
        RocType::Bool => buf.write_str("bool"),
        RocType::I8 => buf.write_str("i8"),
        RocType::U8 => buf.write_str("u8"),
        RocType::I16 => buf.write_str("i16"),
        RocType::U16 => buf.write_str("u16"),
        RocType::I32 => buf.write_str("i32"),
        RocType::U32 => buf.write_str("u32"),
        RocType::I64 => buf.write_str("i64"),
        RocType::U64 => buf.write_str("u64"),
        RocType::I128 => buf.write_str("i128"),
        RocType::U128 => buf.write_str("u128"),
        RocType::F32 => buf.write_str("f32"),
        RocType::F64 => buf.write_str("f64"),
        RocType::Dec => buf.write_str("RocDec"),
        RocType::Str => buf.write_str("RocStr"),
        RocType::Record(record) => buf.write_str(&structs.get_name(&record)),
        RocType::RocBox(elem_type) => {
            buf.write_str("RocBox<")?;
            write_roc_type(*elem_type, structs, enums, buf)?;
            buf.write_char('>')
        }
        RocType::List(elem_type) => {
            buf.write_str("RocList<")?;
            write_roc_type(*elem_type, structs, enums, buf)?;
            buf.write_char('>')
        }
        RocType::TagUnion(tag_union) => buf.write_str(&enums.get_name(&tag_union)),
    }
}

pub fn write_layout_type(
    layout: Layout<'_>,
    var: Variable,
    subs: &Subs,
    buf: &mut String,
) -> fmt::Result {
    use roc_builtins::bitcode::FloatWidth::*;
    use roc_builtins::bitcode::IntWidth::*;
    use roc_mono::layout::Builtin;

    match layout {
        Layout::Builtin(builtin) => match builtin {
            Builtin::Int(width) => match width {
                U8 => buf.write_str("u8"),
                U16 => buf.write_str("u16"),
                U32 => buf.write_str("u32"),
                U64 => buf.write_str("u64"),
                U128 => buf.write_str("u128"),
                I8 => buf.write_str("i8"),
                I16 => buf.write_str("i16"),
                I32 => buf.write_str("i32"),
                I64 => buf.write_str("i64"),
                I128 => buf.write_str("i128"),
            },
            Builtin::Float(width) => match width {
                F32 => buf.write_str("f32"),
                F64 => buf.write_str("f64"),
                F128 => buf.write_str("f128"),
            },
            Builtin::Bool => buf.write_str("bool"),
            Builtin::Decimal => buf.write_str("RocDec"),
            Builtin::Str => buf.write_str("RocStr"),
            Builtin::Dict(key_layout, val_layout) => {
                buf.write_str("RocDict<")?;
                write_layout_type(*key_layout, var, subs, buf)?;
                buf.write_str(", ")?;
                write_layout_type(*val_layout, var, subs, buf)?;
                buf.write_char('>')
            }
            Builtin::Set(elem_type) => {
                buf.write_str("RocSet<")?;
                write_layout_type(*elem_type, var, subs, buf)?;
                buf.write_char('>')
            }
            Builtin::List(elem_type) => {
                buf.write_str("RocList<")?;
                write_layout_type(*elem_type, var, subs, buf)?;
                buf.write_char('>')
            }
        },
        Layout::Struct {
            field_order_hash,
            field_layouts,
        } => {
            todo!("support records in host bindgen")
        }
        Layout::Boxed(_) => todo!("support Box in host bindgen"),
        Layout::Union(_) => todo!("support tag unions in host bindgen"),
        Layout::LambdaSet(_) => todo!("support functions in host bindgen"),
        Layout::RecursivePointer => todo!("support recursive pointers in host bindgen"),
    }
}

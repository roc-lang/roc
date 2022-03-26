use crate::types::RocType;
use std::{fmt::Write, io};

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

pub fn roc_type(roc_type: RocType, buf: &mut String) {
    match roc_type {
        RocType::Str => {
            // TODO
        }
        RocType::Bool => {
            // TODO
        }
        RocType::List(/*Box<RocType>*/) => {
            // TODO
        }
        RocType::TagUnion(/*Vec<(String, Vec<RocType>)>*/) => {
            // TODO
        }
        // RocType::Record(Vec<(String, Box<RocType>)>) => {
        //     *uid += 1;
        //     buf.write_str(&format!("struct S{} {", *uid));

        //     for (field_name, field_type) in fields.into_iter() {
        //         buf.write_str(INDENT);
        //         declare_roc_type
        //     }

        //     buf.write('}');
        // }
        RocType::I8 => {
            buf.write_str("i8");
        }
        RocType::U8 => {
            buf.write_str("u8");
        }
        RocType::I16 => {
            buf.write_str("i16");
        }
        RocType::U16 => {
            buf.write_str("u16");
        }
        RocType::I32 => {
            buf.write_str("i32");
        }
        RocType::U32 => {
            buf.write_str("u32");
        }
        RocType::I64 => {
            buf.write_str("i64");
        }
        RocType::U64 => {
            buf.write_str("u64");
        }
        RocType::I128 => {
            buf.write_str("i128");
        }
        RocType::U128 => {
            buf.write_str("u128");
        }
        RocType::F32 => {
            buf.write_str("f32");
        }
        RocType::F64 => {
            buf.write_str("f64");
        }
        RocType::Dec => {
            buf.write_str("RocDec");
        }
    }
}

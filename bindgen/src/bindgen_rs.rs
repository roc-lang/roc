use crate::enums::Enums;
use crate::structs::Structs;
use bumpalo::Bump;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::{Layout, LayoutCache};
use roc_types::{
    subs::{Content, FlatType, RecordFields, Subs, Variable},
    types::RecordField,
};

use std::{
    fmt::{self, Write},
    io,
};

static TEMPLATE: &[u8] = include_bytes!("../templates/template.rs");
static INDENT: &str = "    ";

pub fn write_template(writer: &mut impl io::Write) -> io::Result<()> {
    writer.write_all(TEMPLATE)?;

    Ok(())
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

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub subs: &'a Subs,
    pub layout_cache: &'a mut LayoutCache<'a>,
    pub interns: &'a Interns,
    pub struct_names: Structs,
    pub enum_names: Enums,
}

pub fn write_layout_type<'a>(
    env: &mut Env<'a>,
    layout: Layout<'a>,
    var: Variable,
    buf: &mut String,
) -> fmt::Result {
    use roc_builtins::bitcode::FloatWidth::*;
    use roc_builtins::bitcode::IntWidth::*;
    use roc_mono::layout::Builtin;

    let subs = env.subs;
    let content = subs.get_content_without_compacting(var);

    let (opt_name, content) = match content {
        Content::Alias(name, _variable, real_var, _kind) => {
            // todo handle type variables
            (Some(*name), subs.get_content_without_compacting(*real_var))
        }
        _ => (None, content),
    };

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
                write_layout_type(env, *key_layout, var, buf)?;
                buf.write_str(", ")?;
                write_layout_type(env, *val_layout, var, buf)?;
                buf.write_char('>')
            }
            Builtin::Set(elem_type) => {
                buf.write_str("RocSet<")?;
                write_layout_type(env, *elem_type, var, buf)?;
                buf.write_char('>')
            }
            Builtin::List(elem_type) => {
                buf.write_str("RocList<")?;
                write_layout_type(env, *elem_type, var, buf)?;
                buf.write_char('>')
            }
        },
        Layout::Struct { .. } => {
            match content {
                Content::FlexVar(_) => todo!(),
                Content::RigidVar(_) => todo!(),
                Content::FlexAbleVar(_, _) => todo!(),
                Content::RigidAbleVar(_, _) => todo!(),
                Content::RecursionVar { .. } => todo!(),
                Content::Structure(FlatType::Record(fields, ext)) => {
                    write_struct(env, opt_name, fields, var, *ext, subs, buf)
                }
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    debug_assert_eq!(tags.len(), 1);
                    todo!()

                    // let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);
                    // single_tag_union_to_ast(env, mem, addr, field_layouts, tag_name, payload_vars)
                }
                Content::Alias(_, _, _, _) => todo!(),
                Content::RangedNumber(_, _) => todo!(),
                Content::Error => todo!(),
                _ => {
                    todo!()
                }
            }

            // (_, Layout::Struct{field_layouts, ..}) => match raw_content {
            //     Content::Structure(FlatType::Record(fields, _)) => {
            //         struct_to_ast(env, mem, addr, *fields)
            //     }
            //     Content::Structure(FlatType::TagUnion(tags, _)) => {
            //         debug_assert_eq!(tags.len(), 1);

            //         let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);
            //         single_tag_union_to_ast(env, mem, addr, field_layouts, tag_name, payload_vars)
            //     }
            //     Content::Structure(FlatType::FunctionOrTagUnion(tag_name, _, _)) => {
            //         let tag_name = &env.subs[*tag_name];
            //         single_tag_union_to_ast(env, mem, addr, field_layouts, tag_name, &[])
            //     }
            //     Content::Structure(FlatType::EmptyRecord) => {
            //         struct_to_ast(env, mem, addr, RecordFields::empty())
            //     }
            //     other => {
            //         unreachable!(
            //             "Something had a Struct layout, but instead of a Record type, it had: {:?}",
            //             other
            //         );
            //     }
            // },
        }
        Layout::Boxed(_) => todo!("support Box in host bindgen"),
        Layout::Union(_) => todo!("support tag unions in host bindgen"),
        Layout::LambdaSet(_) => todo!("support functions in host bindgen"),
        Layout::RecursivePointer => todo!("support recursive pointers in host bindgen"),
    }
}

fn write_struct<'a>(
    env: &mut Env<'a>,
    opt_name: Option<Symbol>,
    record_fields: &RecordFields,
    var: Variable,
    ext: Variable,
    subs: &Subs,
    buf: &mut String,
) -> fmt::Result {
    let mut pairs = bumpalo::collections::Vec::with_capacity_in(record_fields.len(), env.arena);
    let it = record_fields
        .unsorted_iterator(subs, ext)
        .expect("something weird in content");
    for (label, field) in it {
        // drop optional fields
        let var = match field {
            RecordField::Optional(_) => continue,
            RecordField::Required(var) => var,
            RecordField::Demanded(var) => var,
        };

        pairs.push((
            label,
            var,
            env.layout_cache.from_var(env.arena, var, subs).unwrap(),
        ));
    }

    pairs.sort_by(|(label1, _, layout1), (label2, _, layout2)| {
        let size1 = layout1.alignment_bytes(env.layout_cache.target_info);
        let size2 = layout2.alignment_bytes(env.layout_cache.target_info);

        size2.cmp(&size1).then(label1.cmp(label2))
    });

    buf.write_str("struct ")?;

    match opt_name {
        Some(sym) => buf.write_str(sym.as_str(env.interns))?,
        None => buf.write_str(&env.struct_names.get_name(var))?,
    }

    buf.write_str(" {\n")?;

    for (label, field_var, field_layout) in pairs.into_iter() {
        buf.write_str(INDENT)?;
        buf.write_str(label.as_str())?;
        buf.write_str(": ")?;
        write_layout_type(env, field_layout, field_var, buf)?;
        buf.write_str(",\n")?;
    }

    buf.write_str("}\n")
}

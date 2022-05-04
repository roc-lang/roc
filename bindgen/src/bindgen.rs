use crate::structs::Structs;
use crate::types::{TypeId, Types};
use crate::{enums::Enums, types::RocType};
use bumpalo::Bump;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::{Layout, LayoutCache};
use roc_types::{
    subs::{Content, FlatType, RecordFields, Subs, Variable},
    types::RecordField,
};

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub subs: &'a Subs,
    pub layout_cache: &'a mut LayoutCache<'a>,
    pub interns: &'a Interns,
    pub struct_names: Structs,
    pub enum_names: Enums,
}

pub fn add_type<'a>(
    env: &mut Env<'a>,
    layout: Layout<'a>,
    var: Variable,
    types: &mut Types,
) -> TypeId {
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
                U8 => types.add(RocType::U8),
                U16 => types.add(RocType::U16),
                U32 => types.add(RocType::U32),
                U64 => types.add(RocType::U64),
                U128 => types.add(RocType::U128),
                I8 => types.add(RocType::I8),
                I16 => types.add(RocType::I16),
                I32 => types.add(RocType::I32),
                I64 => types.add(RocType::I64),
                I128 => types.add(RocType::I128),
            },
            Builtin::Float(width) => match width {
                F32 => types.add(RocType::F32),
                F64 => types.add(RocType::F64),
                F128 => types.add(RocType::F128),
            },
            Builtin::Bool => types.add(RocType::Bool),
            Builtin::Decimal => types.add(RocType::RocDec),
            Builtin::Str => types.add(RocType::RocStr),
            Builtin::Dict(key_layout, val_layout) => {
                // TODO FIXME this `var` is wrong - should have a different `var` for key and for val
                let key_id = add_type(env, *key_layout, var, types);
                let val_id = add_type(env, *val_layout, var, types);
                let dict_id = types.add(RocType::RocDict(key_id, val_id));

                types.depends(dict_id, key_id);
                types.depends(dict_id, val_id);

                dict_id
            }
            Builtin::Set(elem_layout) => {
                let elem_id = add_type(env, *elem_layout, var, types);
                let set_id = types.add(RocType::RocSet(elem_id));

                types.depends(set_id, elem_id);

                set_id
            }
            Builtin::List(elem_layout) => {
                let elem_id = add_type(env, *elem_layout, var, types);
                let list_id = types.add(RocType::RocList(elem_id));

                types.depends(list_id, elem_id);

                list_id
            }
        },
        Layout::Struct { .. } => {
            match content {
                Content::FlexVar(_)
                | Content::RigidVar(_)
                | Content::FlexAbleVar(_, _)
                | Content::RigidAbleVar(_, _)
                | Content::RecursionVar { .. } => {
                    todo!("TODO give a nice error message for a non-concrete type being passed to the host")
                }
                Content::Structure(FlatType::Record(fields, ext)) => {
                    add_struct(env, opt_name, fields, var, *ext, types)
                }
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    debug_assert_eq!(tags.len(), 1);
                    todo!()

                    // let (tag_name, payload_vars) = unpack_single_element_tag_union(env.subs, *tags);
                    // single_tag_union_to_ast(env, mem, addr, field_layouts, tag_name, payload_vars)
                }

                Content::Structure(FlatType::Apply(_, _)) => {
                    todo!()
                }
                Content::Structure(FlatType::Func(_, _, _)) => {
                    todo!()
                }
                Content::Structure(FlatType::FunctionOrTagUnion(_, _, _)) => {
                    todo!()
                }
                Content::Structure(FlatType::RecursiveTagUnion(_, _, _)) => {
                    todo!()
                }
                Content::Structure(FlatType::Erroneous(_)) => todo!(),
                Content::Structure(FlatType::EmptyRecord) => todo!(),
                Content::Structure(FlatType::EmptyTagUnion) => todo!(),
                Content::Alias(_, _, _, _) => todo!(),
                Content::RangedNumber(_, _) => todo!(),
                Content::Error => todo!(),
            }
        }
        Layout::Boxed(_) => todo!("support Box in host bindgen"),
        Layout::Union(_) => todo!("support tag unions in host bindgen"),
        Layout::LambdaSet(_) => todo!("support functions in host bindgen"),
        Layout::RecursivePointer => todo!("support recursive pointers in host bindgen"),
    }
}

fn add_struct(
    env: &mut Env<'_>,
    opt_name: Option<Symbol>,
    record_fields: &RecordFields,
    var: Variable,
    ext: Variable,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;
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

    let fields = pairs
        .into_iter()
        .map(|(label, field_var, field_layout)| {
            (
                label.to_string(),
                add_type(env, field_layout, field_var, types),
            )
        })
        .collect();

    let name = match opt_name {
        Some(sym) => sym.as_str(env.interns).to_string(),
        None => env.struct_names.get_name(var),
    };

    types.add(RocType::Struct { name, fields })
}

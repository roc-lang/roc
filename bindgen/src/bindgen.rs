use std::convert::TryInto;

use crate::structs::Structs;
use crate::types::{TypeId, Types};
use crate::{enums::Enums, types::RocType};
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth::*, IntWidth::*};
use roc_module::ident::TagName;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::{cmp_fields, ext_var_is_empty_tag_union, Builtin, Layout, LayoutCache};
use roc_types::subs::UnionTags;
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

pub fn add_type<'a>(env: &mut Env<'a>, var: Variable, types: &mut Types) -> TypeId {
    let layout = env
        .layout_cache
        .from_var(env.arena, var, env.subs)
        .expect("Something weird ended up in the content");

    add_type_help(env, layout, var, None, types)
}

pub fn add_type_help<'a>(
    env: &mut Env<'a>,
    layout: Layout<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;

    match subs.get_content_without_compacting(var) {
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
        Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, *ext_var));

            add_tag_union(env, opt_name, tags, var, types)
        }
        Content::Structure(FlatType::Apply(symbol, _)) => {
            if symbol.is_builtin() {
                match layout {
                    Layout::Builtin(builtin) => {
                        add_builtin_type(env, builtin, var, opt_name, types)
                    }
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                todo!("Handle non-builtin Apply")
            }
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
        Content::Structure(FlatType::EmptyTagUnion) => {
            // This can happen when unwrapping a tag union; don't do anything.
            todo!()
        }
        Content::Alias(name, _, real_var, _) => {
            if name.is_builtin() {
                match layout {
                    Layout::Builtin(builtin) => {
                        add_builtin_type(env, builtin, var, opt_name, types)
                    }
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                // If this was a non-builtin type alias, we can use that alias name
                // in the generated bindings.
                add_type_help(env, layout, *real_var, Some(*name), types)
            }
        }
        Content::RangedNumber(_, _) => todo!(),
        Content::Error => todo!(),
    }
}

pub fn add_builtin_type<'a>(
    env: &mut Env<'a>,
    builtin: Builtin<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
) -> TypeId {
    match builtin {
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
            let key_id = add_type_help(env, *key_layout, var, opt_name, types);
            let val_id = add_type_help(env, *val_layout, var, opt_name, types);
            let dict_id = types.add(RocType::RocDict(key_id, val_id));

            types.depends(dict_id, key_id);
            types.depends(dict_id, val_id);

            dict_id
        }
        Builtin::Set(elem_layout) => {
            let elem_id = add_type_help(env, *elem_layout, var, opt_name, types);
            let set_id = types.add(RocType::RocSet(elem_id));

            types.depends(set_id, elem_id);

            set_id
        }
        Builtin::List(elem_layout) => {
            let elem_id = add_type_help(env, *elem_layout, var, opt_name, types);
            let list_id = types.add(RocType::RocList(elem_id));

            types.depends(list_id, elem_id);

            list_id
        }
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
    let mut sortables = bumpalo::collections::Vec::with_capacity_in(record_fields.len(), env.arena);
    let it = record_fields
        .unsorted_iterator(subs, ext)
        .expect("something weird in content");

    for (label, field) in it {
        match field {
            RecordField::Required(field_var) | RecordField::Demanded(field_var) => {
                sortables.push((
                    label,
                    field_var,
                    env.layout_cache
                        .from_var(env.arena, field_var, subs)
                        .unwrap(),
                ));
            }
            RecordField::Optional(_) => {
                // drop optional fields
            }
        };
    }

    sortables.sort_by(|(label1, _, layout1), (label2, _, layout2)| {
        cmp_fields(
            label1,
            layout1,
            label2,
            layout2,
            env.layout_cache.target_info,
        )
    });

    let fields = sortables
        .into_iter()
        .map(|(label, field_var, field_layout)| {
            (
                label.to_string(),
                add_type_help(env, field_layout, field_var, opt_name, types),
            )
        })
        .collect();

    let name = match opt_name {
        Some(sym) => sym.as_str(env.interns).to_string(),
        None => env.struct_names.get_name(var),
    };

    types.add(RocType::Struct { name, fields })
}

fn add_tag_union(
    env: &mut Env<'_>,
    opt_name: Option<Symbol>,
    tags: &UnionTags,
    var: Variable,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;

    let typ = match env.layout_cache.from_var(env.arena, var, subs).unwrap() {
        Layout::Struct { .. } => {
            // a single-tag union with a payload
            todo!();
        }
        Layout::Union(_) => todo!(),
        Layout::Builtin(builtin) => match builtin {
            Builtin::Int(int_width) => {
                let tag_pairs = subs.tag_names[tags.tag_names().indices()]
                    .iter()
                    .map(|tag_name| {
                        let name_str = match tag_name {
                            TagName::Tag(uppercase) => uppercase.as_str().to_string(),
                            TagName::Closure(_) => unreachable!(),
                        };

                        // This is an enum, so there's no payload.
                        (name_str, Vec::new())
                    })
                    .collect();

                let tag_bytes = int_width.stack_size().try_into().unwrap();
                let name = match opt_name {
                    Some(sym) => sym.as_str(env.interns).to_string(),
                    None => env.enum_names.get_name(var),
                };

                RocType::TagUnion {
                    tag_bytes,
                    name,
                    tags: tag_pairs,
                }
            }
            Builtin::Bool => RocType::Bool,
            Builtin::Float(_)
            | Builtin::Decimal
            | Builtin::Str
            | Builtin::Dict(_, _)
            | Builtin::Set(_)
            | Builtin::List(_) => unreachable!(),
        },
        Layout::Boxed(_) | Layout::LambdaSet(_) | Layout::RecursivePointer => {
            unreachable!()
        }
    };

    types.add(typ)
}

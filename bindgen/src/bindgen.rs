use std::convert::TryInto;

use crate::structs::Structs;
use crate::types::{TypeId, Types};
use crate::{enums::Enums, types::RocType};
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth::*, IntWidth::*};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::{cmp_fields, ext_var_is_empty_tag_union, Builtin, Layout, LayoutCache};
use roc_types::subs::UnionTags;
use roc_types::{
    subs::{Content, FlatType, Subs, Variable},
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
            let it = fields
                .unsorted_iterator(subs, *ext)
                .expect("something weird in content")
                .flat_map(|(label, field)| {
                    match field {
                        RecordField::Required(field_var) | RecordField::Demanded(field_var) => {
                            Some((label.clone(), field_var))
                        }
                        RecordField::Optional(_) => {
                            // drop optional fields
                            None
                        }
                    }
                });

            let name = match opt_name {
                Some(sym) => sym.as_str(env.interns).to_string(),
                None => env.struct_names.get_name(var),
            };

            add_struct(env, name, it, types)
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

fn add_struct<I: IntoIterator<Item = (Lowercase, Variable)>>(
    env: &mut Env<'_>,
    name: String,
    fields: I,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;
    let fields_iter = fields.into_iter();
    let mut sortables = bumpalo::collections::Vec::with_capacity_in(
        fields_iter.size_hint().1.unwrap_or_default(),
        env.arena,
    );

    for (label, field_var) in fields_iter {
        sortables.push((
            label,
            field_var,
            env.layout_cache
                .from_var(env.arena, field_var, subs)
                .unwrap(),
        ));
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
                add_type_help(env, field_layout, field_var, None, types),
            )
        })
        .collect();

    types.add(RocType::Struct { name, fields })
}

fn add_tag_union(
    env: &mut Env<'_>,
    opt_name: Option<Symbol>,
    union_tags: &UnionTags,
    var: Variable,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;
    let mut tags: Vec<(String, Vec<Variable>)> = union_tags
        .iter_from_subs(subs)
        .map(|(tag_name, payload_vars)| {
            let name_str = match tag_name {
                TagName::Tag(uppercase) => uppercase.as_str().to_string(),
                TagName::Closure(_) => unreachable!(),
            };

            (name_str, payload_vars.to_vec())
        })
        .collect();

    if tags.len() == 1 {
        let (tag_name, payload_vars) = tags.pop().unwrap();

        // If there was a type alias name, use that. Otherwise use the tag name.
        let name = match opt_name {
            Some(sym) => sym.as_str(env.interns).to_string(),
            None => tag_name,
        };

        return match payload_vars.len() {
            0 => {
                // This is a single-tag union with no payload, e.g. `[ Foo ]`
                // so just generate an empty record
                types.add(RocType::Struct {
                    name,
                    fields: Vec::new(),
                })
            }
            1 => {
                // This is a single-tag union with 1 payload field, e.g.`[ Foo Str ]`.
                // We'll just wrap that.
                let var = *payload_vars.get(0).unwrap();
                let content = add_type(env, var, types);

                types.add(RocType::TransparentWrapper { name, content })
            }
            _ => {
                // This is a single-tag union with multiple payload field, e.g.`[ Foo Str U32 ]`.
                // Generate a record.
                let fields = payload_vars.iter().enumerate().map(|(index, payload_var)| {
                    let field_name = format!("f{}", index).into();

                    (field_name, *payload_var)
                });

                add_struct(env, name, fields, types)
            }
        };
    }

    let name = match opt_name {
        Some(sym) => sym.as_str(env.interns).to_string(),
        None => env.enum_names.get_name(var),
    };

    // Sort tags alphabetically by tag name
    tags.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

    let tags = tags
        .into_iter()
        .map(|(tag_name, payload_vars)| {
            let payloads = payload_vars
                .iter()
                .map(|payload_var| add_type(env, *payload_var, types))
                .collect::<Vec<TypeId>>();

            (tag_name, payloads)
        })
        .collect();

    let typ = match env.layout_cache.from_var(env.arena, var, subs).unwrap() {
        Layout::Struct { .. } => {
            // a single-tag union with multiple payload values, e.g. [ Foo Str Str ]
            unreachable!()
        }
        Layout::Union(_) => todo!(),
        Layout::Builtin(builtin) => match builtin {
            Builtin::Int(int_width) => RocType::TagUnion {
                tag_bytes: int_width.stack_size().try_into().unwrap(),
                name,
                tags,
            },
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

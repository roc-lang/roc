use crate::structs::Structs;
use crate::types::{RocTagUnion, TypeId, Types};
use crate::{
    enums::Enums,
    types::{RocNum, RocType},
};
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth::*, IntWidth::*};
use roc_collections::VecMap;
use roc_module::ident::TagName;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::{cmp_fields, ext_var_is_empty_tag_union, Builtin, Layout, LayoutCache};
use roc_types::subs::UnionTags;
use roc_types::{
    subs::{Content, FlatType, Subs, Variable},
    types::RecordField,
};
use std::fmt::Display;

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub subs: &'a Subs,
    pub layout_cache: &'a mut LayoutCache<'a>,
    pub interns: &'a Interns,
    pub struct_names: Structs,
    pub enum_names: Enums,
    pub pending_recursive_types: VecMap<TypeId, Variable>,
    pub known_recursive_types: VecMap<Variable, TypeId>,
}

impl<'a> Env<'a> {
    pub fn vars_to_types<I>(&mut self, variables: I) -> Types
    where
        I: IntoIterator<Item = Variable>,
    {
        let mut types = Types::default();

        for var in variables {
            self.add_type(var, &mut types);
        }

        self.resolve_pending_recursive_types(&mut types);

        types
    }

    fn add_type(&mut self, var: Variable, types: &mut Types) -> TypeId {
        let layout = self
            .layout_cache
            .from_var(self.arena, var, self.subs)
            .expect("Something weird ended up in the content");

        add_type_help(self, layout, var, None, types)
    }

    fn resolve_pending_recursive_types(&mut self, types: &mut Types) {
        // TODO if VecMap gets a drain() method, use that instead of doing take() and into_iter
        let pending = core::mem::take(&mut self.pending_recursive_types);

        for (type_id, var) in pending.into_iter() {
            let actual_type_id = self.known_recursive_types.get(&var).unwrap_or_else(|| {
                unreachable!(
                    "There was no known recursive TypeId for the pending recursive variable {:?}",
                    var
                );
            });

            debug_assert!(
                matches!(types.get(type_id), RocType::RecursivePointer(TypeId::PENDING)),
                "The TypeId {:?} was registered as a pending recursive pointer, but was not stored in Types as one.",
                type_id
            );

            types.replace(type_id, RocType::RecursivePointer(*actual_type_id));
        }
    }
}

fn add_type_help<'a>(
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
        | Content::RigidAbleVar(_, _) => {
            todo!("TODO give a nice error message for a non-concrete type being passed to the host")
        }
        Content::Structure(FlatType::Record(fields, ext)) => {
            let it = fields
                .unsorted_iterator(subs, *ext)
                .expect("something weird in content")
                .flat_map(|(label, field)| {
                    match field {
                        RecordField::Required(field_var) | RecordField::Demanded(field_var) => {
                            Some((label.to_string(), field_var))
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

            add_struct(env, name, it, types, |name, fields| RocType::Struct {
                name,
                fields,
            })
        }
        Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, *ext_var));

            add_tag_union(env, opt_name, tags, var, types)
        }
        Content::Structure(FlatType::RecursiveTagUnion(_rec_var, tag_vars, ext_var)) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, *ext_var));

            add_tag_union(env, opt_name, tag_vars, var, types)
        }
        Content::Structure(FlatType::Apply(symbol, _)) => match layout {
            Layout::Builtin(builtin) => add_builtin_type(env, builtin, var, opt_name, types),
            _ => {
                if symbol.is_builtin() {
                    todo!(
                        "Handle Apply for builtin symbol {:?} and layout {:?}",
                        symbol,
                        layout
                    )
                } else {
                    todo!(
                        "Handle non-builtin Apply for symbol {:?} and layout {:?}",
                        symbol,
                        layout
                    )
                }
            }
        },
        Content::Structure(FlatType::Func(_, _, _)) => {
            todo!()
        }
        Content::Structure(FlatType::FunctionOrTagUnion(_, _, _)) => {
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
        Content::RecursionVar { structure, .. } => {
            let type_id = types.add(RocType::RecursivePointer(TypeId::PENDING));

            env.pending_recursive_types.insert(type_id, *structure);

            type_id
        }
    }
}

fn add_builtin_type<'a>(
    env: &mut Env<'a>,
    builtin: Builtin<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
) -> TypeId {
    match builtin {
        Builtin::Int(width) => match width {
            U8 => types.add(RocType::Num(RocNum::U8)),
            U16 => types.add(RocType::Num(RocNum::U16)),
            U32 => types.add(RocType::Num(RocNum::U32)),
            U64 => types.add(RocType::Num(RocNum::U64)),
            U128 => types.add(RocType::Num(RocNum::U128)),
            I8 => types.add(RocType::Num(RocNum::I8)),
            I16 => types.add(RocType::Num(RocNum::I16)),
            I32 => types.add(RocType::Num(RocNum::I32)),
            I64 => types.add(RocType::Num(RocNum::I64)),
            I128 => types.add(RocType::Num(RocNum::I128)),
        },
        Builtin::Float(width) => match width {
            F32 => types.add(RocType::Num(RocNum::F32)),
            F64 => types.add(RocType::Num(RocNum::F64)),
            F128 => types.add(RocType::Num(RocNum::F128)),
        },
        Builtin::Decimal => types.add(RocType::Num(RocNum::Dec)),
        Builtin::Bool => types.add(RocType::Bool),
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

fn add_struct<I, L, F>(
    env: &mut Env<'_>,
    name: String,
    fields: I,
    types: &mut Types,
    to_type: F,
) -> TypeId
where
    I: IntoIterator<Item = (L, Variable)>,
    L: Display + Ord,
    F: FnOnce(String, Vec<(L, TypeId)>) -> RocType,
{
    let subs = env.subs;
    let fields_iter = &mut fields.into_iter();
    let mut sortables =
        bumpalo::collections::Vec::with_capacity_in(fields_iter.size_hint().0, env.arena);

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
            let type_id = add_type_help(env, field_layout, field_var, None, types);

            (label, type_id)
        })
        .collect::<Vec<(L, TypeId)>>();

    types.add(to_type(name, fields))
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
        // This is a single-tag union.
        let (tag_name, payload_vars) = tags.pop().unwrap();

        // If there was a type alias name, use that. Otherwise use the tag name.
        let name = match opt_name {
            Some(sym) => sym.as_str(env.interns).to_string(),
            None => tag_name,
        };

        let fields = payload_vars
            .iter()
            .enumerate()
            .map(|(index, payload_var)| (index, *payload_var));

        add_struct(env, name, fields, types, |name, fields| {
            RocType::TagUnionPayload { name, fields }
        })
    } else {
        // This is a multi-tag union.

        // This is a placeholder so that we can get a TypeId for future recursion IDs.
        // At the end, we will replace this with the real tag union type.
        let type_id = types.add(RocType::Struct {
            name: "[THIS SHOULD BE REMOVED]".to_string(),
            fields: Vec::new(),
        });
        let layout = env.layout_cache.from_var(env.arena, var, subs).unwrap();
        let name = match opt_name {
            Some(sym) => sym.as_str(env.interns).to_string(),
            None => env.enum_names.get_name(var),
        };

        // Sort tags alphabetically by tag name
        tags.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

        let is_recursive = is_recursive_tag_union(&layout);

        let mut tags: Vec<_> = tags
            .into_iter()
            .map(|(tag_name, payload_vars)| {
                match struct_fields_needed(env, payload_vars.iter().copied()) {
                    0 => {
                        // no payload
                        (tag_name, None)
                    }
                    1 if !is_recursive => {
                        // this isn't recursive and there's 1 payload item, so it doesn't
                        // need its own struct - e.g. for `[Foo Str, Bar Str]` both of them
                        // can have payloads of plain old Str, no struct wrapper needed.
                        let payload_var = payload_vars.get(0).unwrap();
                        let layout = env
                            .layout_cache
                            .from_var(env.arena, *payload_var, env.subs)
                            .expect("Something weird ended up in the content");
                        let payload_id = add_type_help(env, layout, *payload_var, None, types);

                        (tag_name, Some(payload_id))
                    }
                    _ => {
                        // create a RocType for the payload and save it
                        let struct_name = format!("{}_{}", name, tag_name); // e.g. "MyUnion_MyVariant"
                        let fields = payload_vars.iter().copied().enumerate();
                        let struct_id =
                            add_struct(env, struct_name, fields, types, |name, fields| {
                                RocType::TagUnionPayload { name, fields }
                            });

                        (tag_name, Some(struct_id))
                    }
                }
            })
            .collect();

        let typ = match layout {
            Layout::Union(union_layout) => {
                use roc_mono::layout::UnionLayout::*;

                match union_layout {
                    // A non-recursive tag union
                    // e.g. `Result ok err : [Ok ok, Err err]`
                    NonRecursive(_) => RocType::TagUnion(RocTagUnion::NonRecursive { name, tags }),
                    // A recursive tag union (general case)
                    // e.g. `Expr : [Sym Str, Add Expr Expr]`
                    Recursive(_) => RocType::TagUnion(RocTagUnion::Recursive { name, tags }),
                    // A recursive tag union with just one constructor
                    // Optimization: No need to store a tag ID (the payload is "unwrapped")
                    // e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
                    NonNullableUnwrapped(_) => {
                        todo!()
                    }
                    // A recursive tag union that has an empty variant
                    // Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
                    // It has more than one other variant, so they need tag IDs (payloads are "wrapped")
                    // e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
                    // see also: https://youtu.be/ip92VMpf_-A?t=164
                    NullableWrapped { .. } => {
                        todo!()
                    }
                    // A recursive tag union with only two variants, where one is empty.
                    // Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
                    // e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
                    NullableUnwrapped {
                        nullable_id: null_represents_first_tag,
                        other_fields: _, // TODO use this!
                    } => {
                        // NullableUnwrapped tag unions should always have exactly 2 tags.
                        debug_assert_eq!(tags.len(), 2);

                        let null_tag;
                        let non_null;

                        if null_represents_first_tag {
                            // If nullable_id is true, then the null tag is second, which means
                            // pop() will return it because it's at the end of the vec.
                            null_tag = tags.pop().unwrap().0;
                            non_null = tags.pop().unwrap();
                        } else {
                            // The null tag is first, which means the tag with the payload is second.
                            non_null = tags.pop().unwrap();
                            null_tag = tags.pop().unwrap().0;
                        }

                        let (non_null_tag, non_null_payload) = non_null;

                        RocType::TagUnion(RocTagUnion::NullableUnwrapped {
                            name,
                            null_tag,
                            non_null_tag,
                            non_null_payload: non_null_payload.unwrap(),
                            null_represents_first_tag,
                        })
                    }
                }
            }
            Layout::Builtin(builtin) => match builtin {
                Builtin::Int(_) => RocType::TagUnion(RocTagUnion::Enumeration {
                    name,
                    tags: tags.into_iter().map(|(tag_name, _)| tag_name).collect(),
                }),
                Builtin::Bool => RocType::Bool,
                Builtin::Float(_)
                | Builtin::Decimal
                | Builtin::Str
                | Builtin::Dict(_, _)
                | Builtin::Set(_)
                | Builtin::List(_) => unreachable!(),
            },
            Layout::Struct { .. }
            | Layout::Boxed(_)
            | Layout::LambdaSet(_)
            | Layout::RecursivePointer => {
                unreachable!()
            }
        };

        types.replace(type_id, typ);

        if is_recursive {
            env.known_recursive_types.insert(var, type_id);
        }

        type_id
    }
}

fn is_recursive_tag_union(layout: &Layout) -> bool {
    use roc_mono::layout::UnionLayout::*;

    match layout {
        Layout::Union(tag_union) => match tag_union {
            NonRecursive(_) => false,
            Recursive(_)
            | NonNullableUnwrapped(_)
            | NullableWrapped { .. }
            | NullableUnwrapped { .. } => true,
        },
        _ => false,
    }
}

fn struct_fields_needed<I: IntoIterator<Item = Variable>>(env: &mut Env<'_>, vars: I) -> usize {
    let subs = env.subs;
    let arena = env.arena;

    vars.into_iter().fold(0, |count, var| {
        let layout = env.layout_cache.from_var(arena, var, subs).unwrap();

        if layout.is_dropped_because_empty() {
            count
        } else {
            count + 1
        }
    })
}

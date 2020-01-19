use crate::can::env::Env;
use crate::can::ident::Lowercase;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::parse::ast::{AssignedField, Tag, TypeAnnotation};
use crate::subs::{VarStore, Variable};
use crate::types::RecordFieldLabel;
use crate::types::Type;

pub fn canonicalize_annotation(
    env: &Env,
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
) -> (SendMap<Variable, Lowercase>, crate::types::Type) {
    // NOTE on rigids
    //
    // Rigids must be unique within a type annoation.
    // E.g. in `identity : a -> a`, there should only be one
    // variable (a rigid one, with name "a").
    // Hence `rigids : ImMap<Lowercase, Variable>`
    //
    // But then between annotations, the same name can occur multiple times,
    // but a variable can only have one name. Therefore
    // `ftv : SendMap<Variable, Lowercase>`.
    let mut rigids = ImMap::default();
    let result = can_annotation_help(env, annotation, var_store, &mut rigids);

    let mut ftv = SendMap::default();

    for (k, v) in rigids {
        ftv.insert(v, k);
    }

    (ftv, result)
}

fn can_annotation_help(
    env: &Env,
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
) -> crate::types::Type {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                args.push(can_annotation_help(env, &arg.value, var_store, rigids));
            }

            let ret = can_annotation_help(env, &return_type.value, var_store, rigids);
            Type::Function(args, Box::new(ret))
        }
        Apply(module_name, name, type_arguments) => {
            let mut args = Vec::new();

            for arg in *type_arguments {
                args.push(can_annotation_help(env, &arg.value, var_store, rigids));
            }

            Type::Apply {
                module_name: module_name.join(".").into(),
                name: (*name).into(),
                args,
            }
        }
        BoundVariable(v) => {
            let name = Lowercase::from(*v);

            if let Some(var) = rigids.get(&name) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                rigids.insert(name, var);
                Type::Variable(var)
            }
        }
        As(loc_inner, _spaces, loc_as) => {
            let inner_type = can_annotation_help(env, &loc_inner.value, var_store, rigids);
            let as_type = can_annotation_help(env, &loc_as.value, var_store, rigids);

            panic!("TODO convert parsed `As` noode to Type::Alias");
            // Alias(
            //     ModuleName,
            //     Uppercase,
            //     Vec<(Lowercase, ErrorType)>,
            //     Box<ErrorType>,
            // ),
        }

        Record { fields, ext } => {
            let mut field_types = SendMap::default();

            for field in fields.iter() {
                can_assigned_field(env, &field.value, var_store, rigids, &mut field_types);
            }

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(env, &loc_ann.value, var_store, rigids),
                None => Type::EmptyRec,
            };

            Type::Record(field_types, Box::new(ext_type))
        }
        TagUnion { tags, ext } => {
            let mut tag_types = Vec::with_capacity(tags.len());

            for tag in tags.iter() {
                can_tag(env, &tag.value, var_store, rigids, &mut tag_types);
            }

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(env, &loc_ann.value, var_store, rigids),
                None => Type::EmptyTagUnion,
            };

            Type::TagUnion(tag_types, Box::new(ext_type))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(env, nested, var_store, rigids)
        }
        Wildcard | Malformed(_) => {
            let var = var_store.fresh();
            Type::Variable(var)
        }
    }
}

fn can_assigned_field<'a>(
    env: &Env,
    field: &AssignedField<'a, TypeAnnotation<'a>>,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    field_types: &mut SendMap<RecordFieldLabel, Type>,
) {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type = can_annotation_help(env, &annotation.value, var_store, rigids);
            let label = Lowercase::from(field_name.value);
            field_types.insert(label, field_type);
        }
        LabelOnly(loc_field_name) => {
            // Interpret { a, b } as { a : a, b : b }
            let field_name = Lowercase::from(loc_field_name.value);
            let field_type = {
                if let Some(var) = rigids.get(&field_name) {
                    Type::Variable(*var)
                } else {
                    let field_var = var_store.fresh();
                    rigids.insert(field_name.clone(), field_var);
                    Type::Variable(field_var)
                }
            };
            field_types.insert(field_name, field_type);
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_assigned_field(env, nested, var_store, rigids, field_types)
        }
        Malformed(_) => {}
    }
}

fn can_tag<'a>(
    env: &Env,
    tag: &Tag<'a>,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    tag_types: &mut Vec<(Symbol, Vec<Type>)>,
) {
    match tag {
        Tag::Global { name, args } => {
            let symbol = Symbol::from_global_tag(name.value);

            let arg_types = args
                .iter()
                .map(|arg| can_annotation_help(env, &arg.value, var_store, rigids))
                .collect();

            tag_types.push((symbol, arg_types));
        }
        Tag::Private { name, args } => {
            let symbol = Symbol::from_private_tag(env.home.as_str(), name.value);

            let arg_types = args
                .iter()
                .map(|arg| can_annotation_help(env, &arg.value, var_store, rigids))
                .collect();

            tag_types.push((symbol, arg_types));
        }
        Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => {
            can_tag(env, nested, var_store, rigids, tag_types)
        }
        Tag::Malformed(_) => {}
    }
}

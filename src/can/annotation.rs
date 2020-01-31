use crate::can::env::Env;
use crate::can::ident::{Lowercase, ModuleName, TagName, Uppercase};
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::parse::ast::{AssignedField, Tag, TypeAnnotation};
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::RecordFieldLabel;
use crate::types::{Problem, Type};

pub fn canonicalize_annotation(
    env: &Env,
    scope: &mut Scope,
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
    let mut local_aliases = Vec::new();
    let mut result =
        can_annotation_help(env, annotation, var_store, &mut rigids, &mut local_aliases);

    let mut aliases = scope.aliases.clone();

    for (_, name, loc_args, tipe) in local_aliases {
        aliases.insert(name, (Region::zero(), loc_args, tipe));
    }

    substitute_alias(&mut result, &aliases);

    let mut ftv = SendMap::default();

    for (k, v) in rigids {
        ftv.insert(v, k);
    }

    (ftv, result)
}

// swap Apply with Alias if their module and tag match
pub fn substitute_alias(
    tipe: &mut Type,
    aliases: &ImMap<Uppercase, (Region, Vec<Located<Lowercase>>, Type)>,
) {
    use Type::*;

    match tipe {
        Function(args, ret) => {
            for arg in args {
                substitute_alias(arg, aliases);
            }
            substitute_alias(ret, aliases);
        }
        TagUnion(tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    substitute_alias(x, aliases);
                }
            }
            substitute_alias(ext, aliases);
        }
        Record(fields, ext) => {
            for x in fields.iter_mut() {
                substitute_alias(x, aliases);
            }
            substitute_alias(ext, aliases);
        }
        Alias(_, _, _, actual_type) => {
            substitute_alias(actual_type, aliases);
        }
        Apply {
            module_name: _,
            name,
            args,
        } => {
            if let Some((_region, type_variables, actual)) = aliases.get(name) {
                let mut zipped_args: Vec<(Lowercase, Type)> =
                    Vec::with_capacity(type_variables.len());

                for (tvar, arg) in type_variables.iter().zip(args.iter()) {
                    zipped_args.push((tvar.value.clone(), arg.clone()));
                }

                *tipe = Alias(
                    // module_name.clone(),
                    "Test".into(),
                    name.clone(),
                    zipped_args,
                    Box::new(actual.clone()),
                );
            } else {
                for arg in args {
                    substitute_alias(arg, aliases);
                }
            }
        }
        EmptyRec | EmptyTagUnion | Erroneous(_) | Variable(_) | Boolean(_) => {}

        As(_, _) => unreachable!("As should be canonicalized away at this point"),
    }
}

fn can_annotation_help(
    env: &Env,
    annotation: &crate::parse::ast::TypeAnnotation,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    local_aliases: &mut Vec<(
        ModuleName,
        Uppercase,
        Vec<Located<Lowercase>>,
        crate::types::Type,
    )>,
) -> crate::types::Type {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                args.push(can_annotation_help(
                    env,
                    &arg.value,
                    var_store,
                    rigids,
                    local_aliases,
                ));
            }

            let ret =
                can_annotation_help(env, &return_type.value, var_store, rigids, local_aliases);
            Type::Function(args, Box::new(ret))
        }
        Apply(module_name, name, type_arguments) => {
            let mut args = Vec::new();

            for arg in *type_arguments {
                args.push(can_annotation_help(
                    env,
                    &arg.value,
                    var_store,
                    rigids,
                    local_aliases,
                ));
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
        As(loc_inner, _spaces, loc_as) => match loc_as.value {
            TypeAnnotation::Apply(module_name, name, loc_vars) if module_name.is_empty() => {
                let inner_type =
                    can_annotation_help(env, &loc_inner.value, var_store, rigids, local_aliases);
                let old_name = name.clone();
                let name = Uppercase::from(name);
                let mut loc_type_vars: Vec<Located<Lowercase>> = Vec::with_capacity(loc_vars.len());

                for loc_var in loc_vars {
                    match loc_var.value {
                        BoundVariable(ident) => {
                            let var_name = Lowercase::from(ident);
                            let tvar = Located::at(loc_var.region, var_name);
                            loc_type_vars.push(tvar);
                        }
                        _ => {
                            // If anything other than a lowercase identifier
                            // appears here, the whole annotation is invalid.
                            return Type::Erroneous(Problem::CanonicalizationProblem);
                        }
                    }
                }

                local_aliases.push((env.home.clone(), name, loc_type_vars, inner_type));

                // this Apply will later be resolved to the alias we just declared
                let my_module_name: &[&str] = &(["Test"]);
                let my_apply = TypeAnnotation::Apply(my_module_name, old_name, loc_vars);
                can_annotation_help(env, &my_apply, var_store, rigids, local_aliases)
            }
            _ => {
                // This is a syntactically invalid type alias.
                Type::Erroneous(Problem::CanonicalizationProblem)
            }
        },
        Record { fields, ext } => {
            let mut field_types = SendMap::default();

            for field in fields.iter() {
                can_assigned_field(
                    env,
                    &field.value,
                    var_store,
                    rigids,
                    local_aliases,
                    &mut field_types,
                );
            }

            let ext_type = match ext {
                Some(loc_ann) => {
                    can_annotation_help(env, &loc_ann.value, var_store, rigids, local_aliases)
                }
                None => Type::EmptyRec,
            };

            Type::Record(field_types, Box::new(ext_type))
        }
        TagUnion { tags, ext } => {
            let mut tag_types = Vec::with_capacity(tags.len());

            for tag in tags.iter() {
                can_tag(
                    env,
                    &tag.value,
                    var_store,
                    rigids,
                    local_aliases,
                    &mut tag_types,
                );
            }

            let ext_type = match ext {
                Some(loc_ann) => {
                    can_annotation_help(env, &loc_ann.value, var_store, rigids, local_aliases)
                }
                None => Type::EmptyTagUnion,
            };

            Type::TagUnion(tag_types, Box::new(ext_type))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(env, nested, var_store, rigids, local_aliases)
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
    local_aliases: &mut Vec<(
        ModuleName,
        Uppercase,
        Vec<Located<Lowercase>>,
        crate::types::Type,
    )>,
    field_types: &mut SendMap<RecordFieldLabel, Type>,
) {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type =
                can_annotation_help(env, &annotation.value, var_store, rigids, local_aliases);
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
            can_assigned_field(env, nested, var_store, rigids, local_aliases, field_types)
        }
        Malformed(_) => {}
    }
}

fn can_tag<'a>(
    env: &Env,
    tag: &Tag<'a>,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    local_aliases: &mut Vec<(
        ModuleName,
        Uppercase,
        Vec<Located<Lowercase>>,
        crate::types::Type,
    )>,
    tag_types: &mut Vec<(TagName, Vec<Type>)>,
) {
    match tag {
        Tag::Global { name, args } => {
            let name = name.value.into();

            let arg_types = args
                .iter()
                .map(|arg| can_annotation_help(env, &arg.value, var_store, rigids, local_aliases))
                .collect();

            tag_types.push((TagName::Global(name), arg_types));
        }
        Tag::Private { name, args } => {
            let symbol = Symbol::from_private_tag(env.home.as_str(), name.value);

            let arg_types = args
                .iter()
                .map(|arg| can_annotation_help(env, &arg.value, var_store, rigids, local_aliases))
                .collect();

            tag_types.push((TagName::Private(symbol), arg_types));
        }
        Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => {
            can_tag(env, nested, var_store, rigids, local_aliases, tag_types)
        }
        Tag::Malformed(_) => {}
    }
}

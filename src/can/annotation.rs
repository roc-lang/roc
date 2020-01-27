use crate::can::env::Env;
use crate::can::ident::Ident;
use crate::can::ident::{Lowercase, TagName};
use crate::can::scope::Scope;
use crate::collections::{ImMap, SendMap};
use crate::module::symbol::Symbol;
use crate::parse::ast::{AssignedField, Tag, TypeAnnotation};
use crate::region::Region;
use crate::subs::{VarStore, Variable};
use crate::types::{Problem, RecordFieldLabel, Type};

pub type Annotation = (Type, SendMap<Variable, Lowercase>);

pub fn canonicalize_annotation(
    env: &mut Env,
    scope: &mut Scope,
    annotation: &crate::parse::ast::TypeAnnotation,
    region: Region,
    var_store: &VarStore,
) -> Annotation {
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
    let mut typ = can_annotation_help(
        env,
        annotation,
        region,
        scope,
        var_store,
        &mut rigids,
        &mut local_aliases,
    );

    for (symbol, tipe) in local_aliases {
        typ.substitute_alias(symbol, &tipe);
    }

    let mut ftv = SendMap::default();

    for (k, v) in rigids {
        ftv.insert(v, k);
    }

    (typ, ftv)
}

fn can_annotation_help(
    env: &mut Env,
    annotation: &crate::parse::ast::TypeAnnotation,
    region: Region,
    scope: &mut Scope,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    local_aliases: &mut Vec<(Symbol, crate::types::Type)>,
) -> crate::types::Type {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                args.push(can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                ));
            }

            let ret = can_annotation_help(
                env,
                &return_type.value,
                region,
                scope,
                var_store,
                rigids,
                local_aliases,
            );
            Type::Function(args, Box::new(ret))
        }
        Apply(module_name, ident, type_arguments) => {
            let symbol = if module_name.is_empty() {
                // Since module_name was empty, this is an unqualified type.
                // Look it up in scope!
                let ident: Ident = (*ident).into();

                match scope.lookup(&ident, region) {
                    Ok(symbol) => symbol,
                    Err(problem) => {
                        env.problem(crate::can::problem::Problem::RuntimeError(problem));

                        return Type::Erroneous(Problem::UnrecognizedIdent(ident.into()));
                    }
                }
            } else {
                match env.qualified_lookup(module_name, ident, region) {
                    Ok(symbol) => symbol,
                    Err(problem) => {
                        // Either the module wasn't imported, or
                        // it was imported but it doesn't expose this ident.
                        env.problem(crate::can::problem::Problem::RuntimeError(problem));

                        return Type::Erroneous(Problem::UnrecognizedIdent((*ident).into()));
                    }
                }
            };

            let mut args = Vec::new();

            for arg in *type_arguments {
                args.push(can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                ));
            }

            Type::Apply(symbol, args)
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
            TypeAnnotation::Apply(module_name, ident, loc_vars) if module_name.is_empty() => {
                let symbol = if module_name.is_empty() {
                    // Since module_name was empty, this is an unqualified type.
                    // Look it up in scope!
                    let ident: Ident = (*ident).into();

                    match scope.lookup(&ident, region) {
                        Ok(symbol) => symbol,
                        Err(problem) => {
                            env.problem(crate::can::problem::Problem::RuntimeError(problem));

                            return Type::Erroneous(Problem::UnrecognizedIdent(ident.into()));
                        }
                    }
                } else {
                    match env.qualified_lookup(module_name, ident, region) {
                        Ok(symbol) => symbol,
                        Err(problem) => {
                            // Either the module wasn't imported, or
                            // it was imported but it doesn't expose this ident.
                            env.problem(crate::can::problem::Problem::RuntimeError(problem));

                            return Type::Erroneous(Problem::UnrecognizedIdent((*ident).into()));
                        }
                    }
                };

                let inner_type = can_annotation_help(
                    env,
                    &loc_inner.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                );
                let mut vars = Vec::with_capacity(loc_vars.len());

                for loc_var in loc_vars {
                    match loc_var.value {
                        BoundVariable(ident) => {
                            let var_name = Lowercase::from(ident);

                            if let Some(var) = rigids.get(&var_name) {
                                vars.push((var_name, *var));
                            } else {
                                let var = var_store.fresh();
                                rigids.insert(var_name.clone(), var);
                                vars.push((var_name, var));
                            }
                        }
                        _ => {
                            // If anything other than a lowercase identifier
                            // appears here, the whole annotation is invalid.
                            return Type::Erroneous(Problem::CanonicalizationProblem);
                        }
                    }
                }

                let alias = Type::Alias(symbol, vars, Box::new(inner_type));

                local_aliases.push((symbol, alias.clone()));

                alias
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
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                    &mut field_types,
                );
            }

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                ),
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
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                    &mut tag_types,
                );
            }

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                ),
                None => Type::EmptyTagUnion,
            };

            Type::TagUnion(tag_types, Box::new(ext_type))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(env, nested, region, scope, var_store, rigids, local_aliases)
        }
        Wildcard | Malformed(_) => {
            let var = var_store.fresh();
            Type::Variable(var)
        }
    }
}

fn can_assigned_field<'a>(
    env: &mut Env,
    field: &AssignedField<'a, TypeAnnotation<'a>>,
    region: Region,
    scope: &mut Scope,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    local_aliases: &mut Vec<(Symbol, Type)>,
    field_types: &mut SendMap<RecordFieldLabel, Type>,
) {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type = can_annotation_help(
                env,
                &annotation.value,
                region,
                scope,
                var_store,
                rigids,
                local_aliases,
            );
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
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => can_assigned_field(
            env,
            nested,
            region,
            scope,
            var_store,
            rigids,
            local_aliases,
            field_types,
        ),
        Malformed(_) => {}
    }
}

fn can_tag<'a>(
    env: &mut Env,
    tag: &Tag<'a>,
    region: Region,
    scope: &mut Scope,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    local_aliases: &mut Vec<(Symbol, Type)>,
    tag_types: &mut Vec<(TagName, Vec<Type>)>,
) {
    match tag {
        Tag::Global { name, args } => {
            let name = name.value.into();

            let arg_types = args
                .iter()
                .map(|arg| {
                    can_annotation_help(
                        env,
                        &arg.value,
                        region,
                        scope,
                        var_store,
                        rigids,
                        local_aliases,
                    )
                })
                .collect();

            tag_types.push((TagName::Global(name), arg_types));
        }
        Tag::Private { name, args } => {
            let ident_id = env.ident_ids.private_tag(&name.value.into());
            let symbol = Symbol::new(env.home, ident_id);

            let arg_types = args
                .iter()
                .map(|arg| {
                    can_annotation_help(
                        env,
                        &arg.value,
                        region,
                        scope,
                        var_store,
                        rigids,
                        local_aliases,
                    )
                })
                .collect();

            tag_types.push((TagName::Private(symbol), arg_types));
        }
        Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => can_tag(
            env,
            nested,
            region,
            scope,
            var_store,
            rigids,
            local_aliases,
            tag_types,
        ),
        Tag::Malformed(_) => {}
    }
}

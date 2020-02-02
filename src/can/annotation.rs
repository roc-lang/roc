use crate::can;
use crate::can::env::Env;
use crate::can::ident::Ident;
use crate::can::ident::{Lowercase, TagName};
use crate::can::scope::Scope;
use crate::collections::{default_hasher, ImMap, MutMap, MutSet, SendMap};
use crate::module::symbol::Symbol;
use crate::parse::ast::{AssignedField, Tag, TypeAnnotation};
use crate::region::Region;
use crate::subs::{VarStore, Variable};
use crate::types::{Problem, RecordFieldLabel, Type};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    pub typ: Type,
    pub ftv: MutMap<Variable, Lowercase>,
    pub rigids: ImMap<Lowercase, Variable>,
    pub references: MutSet<Symbol>,
}

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
    let (mut typ, references) = can_annotation_help(
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

    let mut ftv = MutMap::default();

    for (k, v) in rigids.clone() {
        ftv.insert(v, k);
    }

    Annotation {
        typ,
        ftv,
        references,
        rigids,
    }
}

fn can_annotation_help(
    env: &mut Env,
    annotation: &crate::parse::ast::TypeAnnotation,
    region: Region,
    scope: &mut Scope,
    var_store: &VarStore,
    rigids: &mut ImMap<Lowercase, Variable>,
    local_aliases: &mut Vec<(Symbol, crate::types::Type)>,
) -> (crate::types::Type, MutSet<Symbol>) {
    use crate::parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();
            let mut references = MutSet::default();

            for arg in *argument_types {
                let (arg_ann, arg_refs) = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                );

                references.extend(arg_refs);

                args.push(arg_ann);
            }

            let (ret, ret_refs) = can_annotation_help(
                env,
                &return_type.value,
                region,
                scope,
                var_store,
                rigids,
                local_aliases,
            );

            references.extend(ret_refs);

            (Type::Function(args, Box::new(ret)), references)
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

                        return (
                            Type::Erroneous(Problem::UnrecognizedIdent(ident.into())),
                            MutSet::default(),
                        );
                    }
                }
            } else {
                match env.qualified_lookup(module_name, ident, region) {
                    Ok(symbol) => symbol,
                    Err(problem) => {
                        // Either the module wasn't imported, or
                        // it was imported but it doesn't expose this ident.
                        env.problem(crate::can::problem::Problem::RuntimeError(problem));

                        return (
                            Type::Erroneous(Problem::UnrecognizedIdent((*ident).into())),
                            MutSet::default(),
                        );
                    }
                }
            };

            let mut args = Vec::new();
            let mut references = HashSet::with_capacity_and_hasher(1, default_hasher());

            references.insert(symbol);

            for arg in *type_arguments {
                let (arg_ann, arg_refs) = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                );

                references.extend(arg_refs);

                args.push(arg_ann);
            }

            if let Some((_region, ftv, actual)) = scope.lookup_alias(symbol) {
                if args.len() != ftv.len() {
                    panic!("TODO alias applied to incorrect number of type arguments");
                }
                let mut zipped_args: Vec<(Lowercase, Variable)> = Vec::with_capacity(args.len());
                let mut substitution = ImMap::default();

                for (loc_var, arg) in ftv.iter().zip(args.iter()) {
                    substitution.insert(loc_var.value.1, arg.clone());
                    zipped_args.push(loc_var.value.clone());
                }

                (
                    Type::Alias(symbol, zipped_args, Box::new(actual.clone())),
                    references,
                )
            } else {
                (Type::Apply(symbol, args), references)
            }
        }
        BoundVariable(v) => {
            let name = Lowercase::from(*v);
            let typ = match rigids.get(&name) {
                Some(var) => Type::Variable(*var),
                None => {
                    let var = var_store.fresh();

                    rigids.insert(name, var);

                    Type::Variable(var)
                }
            };

            (typ, MutSet::default())
        }
        As(loc_inner, _spaces, loc_as) => match loc_as.value {
            TypeAnnotation::Apply(module_name, ident, loc_vars) if module_name.is_empty() => {
                let symbol = match scope.introduce(ident.into(), &mut env.ident_ids, region) {
                    Ok(symbol) => symbol,

                    Err((original_region, shadow)) => {
                        let problem = Problem::Shadowed(original_region, shadow);
                        env.problem(can::problem::Problem::ErroneousAnnotation(problem.clone()));

                        return (Type::Erroneous(problem), MutSet::default());
                    }
                };

                let (inner_type, mut references) = can_annotation_help(
                    env,
                    &loc_inner.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                );
                let mut vars = Vec::with_capacity(loc_vars.len());

                references.insert(symbol);

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
                            return (
                                Type::Erroneous(Problem::CanonicalizationProblem),
                                references,
                            );
                        }
                    }
                }

                let alias = Type::Alias(symbol, vars, Box::new(inner_type));

                local_aliases.push((symbol, alias.clone()));

                (alias, references)
            }
            _ => {
                // This is a syntactically invalid type alias.
                (
                    Type::Erroneous(Problem::CanonicalizationProblem),
                    MutSet::default(),
                )
            }
        },

        Record { fields, ext } => {
            let mut field_types = SendMap::default();
            let mut references = MutSet::default();

            for field in fields.iter() {
                let field_refs = can_assigned_field(
                    env,
                    &field.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                    &mut field_types,
                );

                references.extend(field_refs);
            }

            let (ext_type, ext_refs) = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                ),
                None => (Type::EmptyRec, MutSet::default()),
            };

            references.extend(ext_refs);

            (Type::Record(field_types, Box::new(ext_type)), references)
        }
        TagUnion { tags, ext } => {
            let mut tag_types = Vec::with_capacity(tags.len());
            let mut references = MutSet::default();

            for tag in tags.iter() {
                let tag_refs = can_tag(
                    env,
                    &tag.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                    &mut tag_types,
                );

                references.extend(tag_refs);
            }

            let (ext_type, ext_refs) = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                ),
                None => (Type::EmptyTagUnion, MutSet::default()),
            };

            references.extend(ext_refs);

            (Type::TagUnion(tag_types, Box::new(ext_type)), references)
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
            can_annotation_help(env, nested, region, scope, var_store, rigids, local_aliases)
        }
        Wildcard | Malformed(_) => {
            let var = var_store.fresh();

            (Type::Variable(var), MutSet::default())
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
) -> MutSet<Symbol> {
    use crate::parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let (field_type, refs) = can_annotation_help(
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

            refs
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

            MutSet::default()
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
        Malformed(_) => MutSet::default(),
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
) -> MutSet<Symbol> {
    match tag {
        Tag::Global { name, args } => {
            let name = name.value.into();
            let mut references = MutSet::default();
            let mut arg_types = Vec::with_capacity(args.len());

            for arg in args.iter() {
                let (ann, refs) = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                );

                arg_types.push(ann);
                references.extend(refs);
            }

            tag_types.push((TagName::Global(name), arg_types));

            references
        }
        Tag::Private { name, args } => {
            let ident_id = env.ident_ids.get_or_insert(&name.value.into());
            let symbol = Symbol::new(env.home, ident_id);
            let mut references = MutSet::default();
            let mut arg_types = Vec::with_capacity(args.len());

            for arg in args.iter() {
                let (ann, refs) = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    rigids,
                    local_aliases,
                );

                arg_types.push(ann);
                references.extend(refs);
            }

            tag_types.push((TagName::Private(symbol), arg_types));

            references
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
        Tag::Malformed(_) => MutSet::default(),
    }
}

use crate::env::Env;
use crate::scope::Scope;
use roc_collections::all::{MutMap, MutSet, SendMap};
use roc_module::ident::Ident;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_parse::ast::{AssignedField, Tag, TypeAnnotation};
use roc_region::all::Located;
use roc_region::all::Region;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, Problem, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    pub typ: Type,
    pub introduced_variables: IntroducedVariables,
    pub references: MutSet<Symbol>,
    pub aliases: SendMap<Symbol, Alias>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct IntroducedVariables {
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
    pub wildcards: Vec<Variable>,
    pub var_by_name: SendMap<Lowercase, Variable>,
    pub name_by_var: SendMap<Variable, Lowercase>,
}

impl IntroducedVariables {
    pub fn insert_named(&mut self, name: Lowercase, var: Variable) {
        self.var_by_name.insert(name.clone(), var);
        self.name_by_var.insert(var, name);
    }

    pub fn insert_wildcard(&mut self, var: Variable) {
        self.wildcards.push(var);
    }

    pub fn union(&mut self, other: &Self) {
        self.wildcards.extend(other.wildcards.iter().cloned());
        self.var_by_name.extend(other.var_by_name.clone());
        self.name_by_var.extend(other.name_by_var.clone());
    }

    pub fn var_by_name(&self, name: &Lowercase) -> Option<&Variable> {
        self.var_by_name.get(name)
    }

    pub fn name_by_var(&self, var: Variable) -> Option<&Lowercase> {
        self.name_by_var.get(&var)
    }
}

pub fn canonicalize_annotation(
    env: &mut Env,
    scope: &mut Scope,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
    var_store: &mut VarStore,
) -> Annotation {
    let mut introduced_variables = IntroducedVariables::default();
    let mut aliases = SendMap::default();
    let mut references = MutSet::default();
    let typ = can_annotation_help(
        env,
        annotation,
        region,
        scope,
        var_store,
        &mut introduced_variables,
        &mut aliases,
        &mut references,
    );

    Annotation {
        typ,
        introduced_variables,
        references,
        aliases,
    }
}

#[allow(clippy::too_many_arguments)]
fn can_annotation_help(
    env: &mut Env,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut SendMap<Symbol, Alias>,
    references: &mut MutSet<Symbol>,
) -> Type {
    use roc_parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                let arg_ann = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                args.push(arg_ann);
            }

            let ret = can_annotation_help(
                env,
                &return_type.value,
                region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
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
                        env.problem(roc_problem::can::Problem::RuntimeError(problem));

                        return Type::Erroneous(Problem::UnrecognizedIdent(ident.into()));
                    }
                }
            } else {
                match env.qualified_lookup(module_name, ident, region) {
                    Ok(symbol) => symbol,
                    Err(problem) => {
                        // Either the module wasn't imported, or
                        // it was imported but it doesn't expose this ident.
                        env.problem(roc_problem::can::Problem::RuntimeError(problem));

                        return Type::Erroneous(Problem::UnrecognizedIdent((*ident).into()));
                    }
                }
            };

            let mut args = Vec::new();

            references.insert(symbol);

            for arg in *type_arguments {
                let arg_ann = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                args.push(arg_ann);
            }

            Type::Apply(symbol, args)
        }
        BoundVariable(v) => {
            let name = Lowercase::from(*v);

            match introduced_variables.var_by_name(&name) {
                Some(var) => Type::Variable(*var),
                None => {
                    let var = var_store.fresh();

                    introduced_variables.insert_named(name, var);

                    Type::Variable(var)
                }
            }
        }
        As(loc_inner, _spaces, loc_as) => match loc_as.value {
            TypeAnnotation::Apply(module_name, ident, loc_vars) if module_name.is_empty() => {
                let symbol = match scope.introduce(
                    ident.into(),
                    &env.exposed_ident_ids,
                    &mut env.ident_ids,
                    region,
                ) {
                    Ok(symbol) => symbol,

                    Err((original_region, shadow)) => {
                        let problem = Problem::Shadowed(original_region, shadow.clone());

                        env.problem(roc_problem::can::Problem::ShadowingInAnnotation {
                            original_region,
                            shadow,
                        });

                        return Type::Erroneous(problem);
                    }
                };

                let inner_type = can_annotation_help(
                    env,
                    &loc_inner.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );
                let mut vars = Vec::with_capacity(loc_vars.len());
                let mut lowercase_vars = Vec::with_capacity(loc_vars.len());

                references.insert(symbol);

                for loc_var in loc_vars {
                    match loc_var.value {
                        BoundVariable(ident) => {
                            let var_name = Lowercase::from(ident);

                            if let Some(var) = introduced_variables.var_by_name(&var_name) {
                                vars.push((var_name.clone(), Type::Variable(*var)));
                                lowercase_vars.push(Located::at(loc_var.region, (var_name, *var)));
                            } else {
                                let var = var_store.fresh();

                                introduced_variables.insert_named(var_name.clone(), var);
                                vars.push((var_name.clone(), Type::Variable(var)));

                                lowercase_vars.push(Located::at(loc_var.region, (var_name, var)));
                            }
                        }
                        _ => {
                            // If anything other than a lowercase identifier
                            // appears here, the whole annotation is invalid.
                            return Type::Erroneous(Problem::CanonicalizationProblem);
                        }
                    }
                }

                let alias_actual = if let Type::TagUnion(tags, ext) = inner_type {
                    let rec_var = var_store.fresh();

                    let mut new_tags = Vec::with_capacity(tags.len());
                    for (tag_name, args) in tags {
                        let mut new_args = Vec::with_capacity(args.len());
                        for arg in args {
                            let mut new_arg = arg.clone();
                            new_arg.substitute_alias(symbol, &Type::Variable(rec_var));
                            new_args.push(new_arg);
                        }
                        new_tags.push((tag_name.clone(), new_args));
                    }
                    Type::RecursiveTagUnion(rec_var, new_tags, ext)
                } else {
                    inner_type
                };

                let alias = Alias {
                    region,
                    vars: lowercase_vars,
                    uniqueness: None,
                    typ: alias_actual,
                };
                local_aliases.insert(symbol, alias);

                // We turn this 'inline' alias into an Apply. This will later get de-aliased again,
                // but this approach is easier wrt. instantiation of uniqueness variables.
                let args = vars.into_iter().map(|(_, b)| b).collect();
                Type::Apply(symbol, args)
            }
            _ => {
                // This is a syntactically invalid type alias.
                Type::Erroneous(Problem::CanonicalizationProblem)
            }
        },

        Record { fields, ext } => {
            let mut field_types = SendMap::default();
            let mut seen = MutMap::default();

            for field in fields.iter() {
                let opt_field_name = can_assigned_field(
                    env,
                    &field.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    &mut field_types,
                    references,
                );

                if let Some(added) = opt_field_name {
                    if let Some(replaced_region) = seen.insert(added.clone(), field.region) {
                        env.problem(roc_problem::can::Problem::DuplicateRecordFieldType {
                            field_name: added.clone(),
                            field_region: field.region,
                            record_region: region,
                            replaced_region,
                        });
                    }
                }
            }

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                ),
                None => Type::EmptyRec,
            };

            Type::Record(field_types, Box::new(ext_type))
        }
        TagUnion { tags, ext } => {
            let mut tag_types = Vec::with_capacity(tags.len());
            let mut seen = MutMap::default();

            for tag in tags.iter() {
                let opt_tag_name = can_tag(
                    env,
                    &tag.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    &mut tag_types,
                    references,
                );

                if let Some(added) = opt_tag_name {
                    if let Some(replaced_region) = seen.insert(added.clone(), tag.region) {
                        env.problem(roc_problem::can::Problem::DuplicateTag {
                            tag_name: added.clone(),
                            tag_region: tag.region,
                            tag_union_region: region,
                            replaced_region,
                        });
                    }
                }
            }

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                ),
                None => Type::EmptyTagUnion,
            };

            Type::TagUnion(tag_types, Box::new(ext_type))
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => can_annotation_help(
            env,
            nested,
            region,
            scope,
            var_store,
            introduced_variables,
            local_aliases,
            references,
        ),
        Wildcard | Malformed(_) => {
            let var = var_store.fresh();

            introduced_variables.insert_wildcard(var);

            Type::Variable(var)
        }
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_assigned_field<'a>(
    env: &mut Env,
    field: &AssignedField<'a, TypeAnnotation<'a>>,
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut SendMap<Symbol, Alias>,
    field_types: &mut SendMap<Lowercase, Type>,
    references: &mut MutSet<Symbol>,
) -> Option<Lowercase> {
    use roc_parse::ast::AssignedField::*;

    match field {
        LabeledValue(field_name, _, annotation) => {
            let field_type = can_annotation_help(
                env,
                &annotation.value,
                annotation.region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );

            let label = Lowercase::from(field_name.value);
            field_types.insert(label.clone(), field_type);

            Some(label)
        }
        LabelOnly(loc_field_name) => {
            // Interpret { a, b } as { a : a, b : b }
            let field_name = Lowercase::from(loc_field_name.value);
            let field_type = {
                if let Some(var) = introduced_variables.var_by_name(&field_name) {
                    Type::Variable(*var)
                } else {
                    let field_var = var_store.fresh();
                    introduced_variables.insert_named(field_name.clone(), field_var);
                    Type::Variable(field_var)
                }
            };

            field_types.insert(field_name.clone(), field_type);

            Some(field_name)
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => can_assigned_field(
            env,
            nested,
            region,
            scope,
            var_store,
            introduced_variables,
            local_aliases,
            field_types,
            references,
        ),
        Malformed(_) => None,
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_tag<'a>(
    env: &mut Env,
    tag: &Tag<'a>,
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut SendMap<Symbol, Alias>,
    tag_types: &mut Vec<(TagName, Vec<Type>)>,
    references: &mut MutSet<Symbol>,
) -> Option<TagName> {
    match tag {
        Tag::Global { name, args } => {
            let name = name.value.into();
            let mut arg_types = Vec::with_capacity(args.len());

            for arg in args.iter() {
                let ann = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                arg_types.push(ann);
            }

            let tag_name = TagName::Global(name);
            tag_types.push((tag_name.clone(), arg_types));

            Some(tag_name)
        }
        Tag::Private { name, args } => {
            let ident_id = env.ident_ids.get_or_insert(&name.value.into());
            let symbol = Symbol::new(env.home, ident_id);
            let mut arg_types = Vec::with_capacity(args.len());

            for arg in args.iter() {
                let ann = can_annotation_help(
                    env,
                    &arg.value,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                arg_types.push(ann);
            }

            let tag_name = TagName::Private(symbol);
            tag_types.push((tag_name.clone(), arg_types));

            Some(tag_name)
        }
        Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => can_tag(
            env,
            nested,
            region,
            scope,
            var_store,
            introduced_variables,
            local_aliases,
            tag_types,
            references,
        ),
        Tag::Malformed(_) => None,
    }
}

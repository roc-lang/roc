use crate::env::Env;
use crate::scope::Scope;
use roc_collections::all::{ImMap, MutMap, MutSet, SendMap};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_parse::ast::{AssignedField, Tag, TypeAnnotation};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, LambdaSet, Problem, RecordField, Type};

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
    pub host_exposed_aliases: MutMap<Symbol, Variable>,
}

impl IntroducedVariables {
    pub fn insert_named(&mut self, name: Lowercase, var: Variable) {
        self.var_by_name.insert(name.clone(), var);
        self.name_by_var.insert(var, name);
    }

    pub fn insert_wildcard(&mut self, var: Variable) {
        self.wildcards.push(var);
    }

    pub fn insert_host_exposed_alias(&mut self, symbol: Symbol, var: Variable) {
        self.host_exposed_aliases.insert(symbol, var);
    }

    pub fn union(&mut self, other: &Self) {
        self.wildcards.extend(other.wildcards.iter().cloned());
        self.var_by_name.extend(other.var_by_name.clone());
        self.name_by_var.extend(other.name_by_var.clone());
        self.host_exposed_aliases
            .extend(other.host_exposed_aliases.clone());
    }

    pub fn var_by_name(&self, name: &Lowercase) -> Option<&Variable> {
        self.var_by_name.get(name)
    }

    pub fn name_by_var(&self, var: Variable) -> Option<&Lowercase> {
        self.name_by_var.get(&var)
    }
}

fn malformed(env: &mut Env, region: Region, name: &str) {
    use roc_problem::can::RuntimeError::*;

    let problem = MalformedTypeName((*name).into(), region);
    env.problem(roc_problem::can::Problem::RuntimeError(problem));
}

pub fn canonicalize_annotation(
    env: &mut Env,
    scope: &mut Scope,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
    var_store: &mut VarStore,
) -> Annotation {
    let mut introduced_variables = IntroducedVariables::default();
    let mut references = MutSet::default();
    let mut aliases = SendMap::default();

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

            let closure = Type::Variable(var_store.fresh());

            Type::Function(args, Box::new(closure), Box::new(ret))
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

                        return Type::Erroneous(Problem::UnrecognizedIdent(ident));
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

            match scope.lookup_alias(symbol) {
                Some(alias) => {
                    // use a known alias
                    let mut actual = alias.typ.clone();
                    let mut substitutions = ImMap::default();
                    let mut vars = Vec::new();

                    if alias.type_variables.len() != args.len() {
                        let error = Type::Erroneous(Problem::BadTypeArguments {
                            symbol,
                            region,
                            alias_needs: alias.type_variables.len() as u8,
                            type_got: args.len() as u8,
                        });
                        return error;
                    }

                    for (loc_var, arg_ann) in alias.type_variables.iter().zip(args.into_iter()) {
                        let name = loc_var.value.0.clone();
                        let var = loc_var.value.1;

                        substitutions.insert(var, arg_ann.clone());
                        vars.push((name.clone(), arg_ann));
                    }

                    // make sure the recursion variable is freshly instantiated
                    if let Type::RecursiveTagUnion(rvar, _, _) = &mut actual {
                        let new = var_store.fresh();
                        substitutions.insert(*rvar, Type::Variable(new));
                        *rvar = new;
                    }

                    // make sure hidden variables are freshly instantiated
                    let mut lambda_set_variables =
                        Vec::with_capacity(alias.lambda_set_variables.len());
                    for typ in alias.lambda_set_variables.iter() {
                        if let Type::Variable(var) = typ.0 {
                            let fresh = var_store.fresh();
                            substitutions.insert(var, Type::Variable(fresh));
                            lambda_set_variables.push(LambdaSet(Type::Variable(fresh)));
                        } else {
                            unreachable!("at this point there should be only vars in there");
                        }
                    }

                    // instantiate variables
                    actual.substitute(&substitutions);

                    Type::Alias {
                        symbol,
                        type_arguments: vars,
                        lambda_set_variables,
                        actual: Box::new(actual),
                    }
                }
                None => Type::Apply(symbol, args),
            }
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

                let mut hidden_variables = MutSet::default();
                hidden_variables.extend(alias_actual.variables());

                for loc_var in lowercase_vars.iter() {
                    hidden_variables.remove(&loc_var.value.1);
                }

                scope.add_alias(symbol, region, lowercase_vars, alias_actual);

                let alias = scope.lookup_alias(symbol).unwrap();
                local_aliases.insert(symbol, alias.clone());

                // Type::Alias(symbol, vars, Box::new(alias.typ.clone()))

                if vars.is_empty() && env.home == symbol.module_id() {
                    let actual_var = var_store.fresh();
                    introduced_variables.insert_host_exposed_alias(symbol, actual_var);
                    Type::HostExposedAlias {
                        name: symbol,
                        type_arguments: vars,
                        lambda_set_variables: alias.lambda_set_variables.clone(),
                        actual: Box::new(alias.typ.clone()),
                        actual_var,
                    }
                } else {
                    Type::Alias {
                        symbol,
                        type_arguments: vars,
                        lambda_set_variables: alias.lambda_set_variables.clone(),
                        actual: Box::new(alias.typ.clone()),
                    }
                }
            }
            _ => {
                // This is a syntactically invalid type alias.
                Type::Erroneous(Problem::CanonicalizationProblem)
            }
        },

        Record { fields, ext, .. } => {
            let field_types = can_assigned_fields(
                env,
                fields,
                region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );

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
        TagUnion { tags, ext, .. } => {
            let tag_types = can_tags(
                env,
                tags,
                region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );

            let ext_type = match ext {
                Some(loc_ann) => can_annotation_help(
                    env,
                    &loc_ann.value,
                    loc_ann.region,
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
        Wildcard => {
            let var = var_store.fresh();

            introduced_variables.insert_wildcard(var);

            Type::Variable(var)
        }
        Malformed(string) => {
            malformed(env, region, string);

            let var = var_store.fresh();

            introduced_variables.insert_wildcard(var);

            Type::Variable(var)
        }
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_assigned_fields<'a>(
    env: &mut Env,
    fields: &&[Located<AssignedField<'a, TypeAnnotation<'a>>>],
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut SendMap<Symbol, Alias>,
    references: &mut MutSet<Symbol>,
) -> SendMap<Lowercase, RecordField<Type>> {
    use roc_parse::ast::AssignedField::*;
    use roc_types::types::RecordField::*;

    // SendMap doesn't have a `with_capacity`
    let mut field_types = SendMap::default();

    // field names we've seen so far in this record
    let mut seen = std::collections::HashMap::with_capacity(fields.len());

    'outer: for loc_field in fields.iter() {
        let mut field = &loc_field.value;

        // use this inner loop to unwrap the SpaceAfter/SpaceBefore
        // when we find the name of this field, break out of the loop
        // with that value, so we can check whether the field name is
        // a duplicate
        let new_name = 'inner: loop {
            match field {
                RequiredValue(field_name, _, annotation) => {
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
                    field_types.insert(label.clone(), Required(field_type));

                    break 'inner label;
                }
                OptionalValue(field_name, _, annotation) => {
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
                    field_types.insert(label.clone(), Optional(field_type));

                    break 'inner label;
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

                    field_types.insert(field_name.clone(), Required(field_type));

                    break 'inner field_name;
                }
                SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
                    // check the nested field instead
                    field = nested;
                    continue 'inner;
                }
                Malformed(string) => {
                    malformed(env, region, string);

                    // completely skip this element, advance to the next tag
                    continue 'outer;
                }
            }
        };

        // ensure that the new name is not already in this record:
        // note that the right-most tag wins when there are two with the same name
        if let Some(replaced_region) = seen.insert(new_name.clone(), loc_field.region) {
            env.problem(roc_problem::can::Problem::DuplicateRecordFieldType {
                field_name: new_name,
                record_region: region,
                field_region: loc_field.region,
                replaced_region,
            });
        }
    }

    field_types
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_tags<'a>(
    env: &mut Env,
    tags: &'a [Located<Tag<'a>>],
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut SendMap<Symbol, Alias>,
    references: &mut MutSet<Symbol>,
) -> Vec<(TagName, Vec<Type>)> {
    let mut tag_types = Vec::with_capacity(tags.len());

    // tag names we've seen so far in this tag union
    let mut seen = std::collections::HashMap::with_capacity(tags.len());

    'outer: for loc_tag in tags.iter() {
        let mut tag = &loc_tag.value;

        // use this inner loop to unwrap the SpaceAfter/SpaceBefore
        // when we find the name of this tag, break out of the loop
        // with that value, so we can check whether the tag name is
        // a duplicate
        let new_name = 'inner: loop {
            match tag {
                Tag::Global { name, args } => {
                    let name = name.value.into();
                    let mut arg_types = Vec::with_capacity(args.len());

                    for arg in args.iter() {
                        let ann = can_annotation_help(
                            env,
                            &arg.value,
                            arg.region,
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

                    break 'inner tag_name;
                }
                Tag::Private { name, args } => {
                    let ident_id = env.ident_ids.get_or_insert(&name.value.into());
                    let symbol = Symbol::new(env.home, ident_id);
                    let mut arg_types = Vec::with_capacity(args.len());

                    for arg in args.iter() {
                        let ann = can_annotation_help(
                            env,
                            &arg.value,
                            arg.region,
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

                    break 'inner tag_name;
                }
                Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => {
                    // check the nested tag instead
                    tag = nested;
                    continue 'inner;
                }
                Tag::Malformed(string) => {
                    malformed(env, region, string);

                    // completely skip this element, advance to the next tag
                    continue 'outer;
                }
            }
        };

        // ensure that the new name is not already in this tag union:
        // note that the right-most tag wins when there are two with the same name
        if let Some(replaced_region) = seen.insert(new_name.clone(), loc_tag.region) {
            env.problem(roc_problem::can::Problem::DuplicateTag {
                tag_name: new_name,
                tag_region: loc_tag.region,
                tag_union_region: region,
                replaced_region,
            });
        }
    }

    tag_types
}

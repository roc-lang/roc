use crate::env::Env;
use crate::scope::Scope;
use roc_collections::all::{ImMap, MutMap, MutSet, SendMap};
use roc_module::ident::{Ident, Lowercase, TagName};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_parse::ast::{AssignedField, Pattern, Tag, TypeAnnotation, TypeHeader};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, AliasKind, LambdaSet, Problem, RecordField, Type};

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
    // Rigids must be unique within a type annotation.
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

fn make_apply_symbol(
    env: &mut Env,
    region: Region,
    scope: &mut Scope,
    module_name: &str,
    ident: &str,
) -> Result<Symbol, Type> {
    if module_name.is_empty() {
        // Since module_name was empty, this is an unqualified type.
        // Look it up in scope!
        let ident: Ident = (*ident).into();

        match scope.lookup(&ident, region) {
            Ok(symbol) => Ok(symbol),
            Err(problem) => {
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                Err(Type::Erroneous(Problem::UnrecognizedIdent(ident)))
            }
        }
    } else {
        match env.qualified_lookup(module_name, ident, region) {
            Ok(symbol) => Ok(symbol),
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                Err(Type::Erroneous(Problem::UnrecognizedIdent((*ident).into())))
            }
        }
    }
}

/// Retrieves all symbols in an annotations that reference a type definition, that is either an
/// alias or an opaque type.
///
/// For example, in `[ A Age U8, B Str {} ]`, there are three type definition references - `Age`,
/// `U8`, and `Str`.
pub fn find_type_def_symbols(
    module_id: ModuleId,
    ident_ids: &mut IdentIds,
    initial_annotation: &roc_parse::ast::TypeAnnotation,
) -> Vec<Symbol> {
    use roc_parse::ast::TypeAnnotation::*;

    let mut result = Vec::new();

    let mut stack = vec![initial_annotation];

    while let Some(annotation) = stack.pop() {
        match annotation {
            Apply(_module_name, ident, arguments) => {
                let ident: Ident = (*ident).into();
                let ident_id = ident_ids.get_or_insert(&ident);

                let symbol = Symbol::new(module_id, ident_id);
                result.push(symbol);

                for t in arguments.iter() {
                    stack.push(&t.value);
                }
            }
            Function(arguments, result) => {
                for t in arguments.iter() {
                    stack.push(&t.value);
                }

                stack.push(&result.value);
            }
            BoundVariable(_) => {}
            As(actual, _, _) => {
                stack.push(&actual.value);
            }
            Record { fields, ext } => {
                let mut inner_stack = Vec::with_capacity(fields.items.len());

                for field in fields.items.iter() {
                    inner_stack.push(&field.value)
                }

                while let Some(assigned_field) = inner_stack.pop() {
                    match assigned_field {
                        AssignedField::RequiredValue(_, _, t)
                        | AssignedField::OptionalValue(_, _, t) => {
                            stack.push(&t.value);
                        }
                        AssignedField::LabelOnly(_) => {}
                        AssignedField::SpaceBefore(inner, _)
                        | AssignedField::SpaceAfter(inner, _) => inner_stack.push(inner),
                        AssignedField::Malformed(_) => {}
                    }
                }

                for t in ext.iter() {
                    stack.push(&t.value);
                }
            }
            TagUnion { ext, tags } => {
                let mut inner_stack = Vec::with_capacity(tags.items.len());

                for tag in tags.items.iter() {
                    inner_stack.push(&tag.value)
                }

                while let Some(tag) = inner_stack.pop() {
                    match tag {
                        Tag::Global { args, .. } | Tag::Private { args, .. } => {
                            for t in args.iter() {
                                stack.push(&t.value);
                            }
                        }
                        Tag::SpaceBefore(inner, _) | Tag::SpaceAfter(inner, _) => {
                            inner_stack.push(inner)
                        }
                        Tag::Malformed(_) => {}
                    }
                }

                for t in ext.iter() {
                    stack.push(&t.value);
                }
            }
            SpaceBefore(inner, _) | SpaceAfter(inner, _) => {
                stack.push(inner);
            }
            Inferred | Wildcard | Malformed(_) => {}
        }
    }

    result
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
            let symbol = match make_apply_symbol(env, region, scope, module_name, ident) {
                Err(problem) => return problem,
                Ok(symbol) => symbol,
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
                        kind: alias.kind,
                    }
                }
                None => Type::Apply(symbol, args, region),
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
        As(
            loc_inner,
            _spaces,
            alias_header
            @ TypeHeader {
                name,
                vars: loc_vars,
            },
        ) => {
            let symbol = match scope.introduce(
                name.value.into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                region,
            ) {
                Ok(symbol) => symbol,

                Err((original_region, shadow, _new_symbol)) => {
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

            for loc_var in *loc_vars {
                let var = match loc_var.value {
                    Pattern::Identifier(name) if name.chars().next().unwrap().is_lowercase() => {
                        name
                    }
                    _ => unreachable!("I thought this was validated during parsing"),
                };
                let var_name = Lowercase::from(var);

                if let Some(var) = introduced_variables.var_by_name(&var_name) {
                    vars.push((var_name.clone(), Type::Variable(*var)));
                    lowercase_vars.push(Loc::at(loc_var.region, (var_name, *var)));
                } else {
                    let var = var_store.fresh();

                    introduced_variables.insert_named(var_name.clone(), var);
                    vars.push((var_name.clone(), Type::Variable(var)));

                    lowercase_vars.push(Loc::at(loc_var.region, (var_name, var)));
                }
            }

            let alias_args = vars.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>();

            let alias_actual = if let Type::TagUnion(tags, ext) = inner_type {
                let rec_var = var_store.fresh();

                let mut new_tags = Vec::with_capacity(tags.len());
                let mut is_nested_datatype = false;
                for (tag_name, args) in tags {
                    let mut new_args = Vec::with_capacity(args.len());
                    for arg in args {
                        let mut new_arg = arg.clone();
                        let substitution_result =
                            new_arg.substitute_alias(symbol, &alias_args, &Type::Variable(rec_var));

                        if let Err(differing_recursion_region) = substitution_result {
                            env.problems
                                .push(roc_problem::can::Problem::NestedDatatype {
                                    alias: symbol,
                                    def_region: alias_header.region(),
                                    differing_recursion_region,
                                });
                            is_nested_datatype = true;
                        }

                        // Either way, add the argument; not doing so would only result in more
                        // confusing error messages later on.
                        new_args.push(new_arg);
                    }
                    new_tags.push((tag_name.clone(), new_args));
                }
                if is_nested_datatype {
                    // We don't have a way to represent nested data types; hence, we don't actually
                    // use the recursion var in them, and should avoid marking them as such.
                    Type::TagUnion(new_tags, ext)
                } else {
                    Type::RecursiveTagUnion(rec_var, new_tags, ext)
                }
            } else {
                inner_type
            };

            let mut hidden_variables = MutSet::default();
            hidden_variables.extend(alias_actual.variables());

            for loc_var in lowercase_vars.iter() {
                hidden_variables.remove(&loc_var.value.1);
            }

            scope.add_alias(
                symbol,
                region,
                lowercase_vars,
                alias_actual,
                AliasKind::Structural, // aliases in "as" are never opaque
            );

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
                    kind: alias.kind,
                }
            }
        }

        Record { fields, ext } => {
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

            if fields.is_empty() {
                match ext {
                    Some(_) => {
                        // just `a` does not mean the same as `{}a`, so even
                        // if there are no fields, still make this a `Record`,
                        // not an EmptyRec
                        Type::Record(Default::default(), Box::new(ext_type))
                    }

                    None => Type::EmptyRec,
                }
            } else {
                let field_types = can_assigned_fields(
                    env,
                    &fields.items,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                Type::Record(field_types, Box::new(ext_type))
            }
        }
        TagUnion { tags, ext, .. } => {
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

            if tags.is_empty() {
                match ext {
                    Some(_) => {
                        // just `a` does not mean the same as `{}a`, so even
                        // if there are no fields, still make this a `Record`,
                        // not an EmptyRec
                        Type::TagUnion(Default::default(), Box::new(ext_type))
                    }

                    None => Type::EmptyTagUnion,
                }
            } else {
                let mut tag_types = can_tags(
                    env,
                    tags.items,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                // sort here; we later instantiate type aliases, so this type might get duplicated
                // many times. Then, when inserting into the subs, the tags are sorted.
                // in theory we save a lot of time by sorting once here
                insertion_sort_by(&mut tag_types, |a, b| a.0.cmp(&b.0));

                Type::TagUnion(tag_types, Box::new(ext_type))
            }
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
        Inferred => {
            // Inference variables aren't bound to a rigid or a wildcard, so all we have to do is
            // make a fresh unconstrained variable, and let the type solver fill it in for us 🤠
            let var = var_store.fresh();
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

fn insertion_sort_by<T, F>(arr: &mut [T], mut compare: F)
where
    F: FnMut(&T, &T) -> std::cmp::Ordering,
{
    for i in 1..arr.len() {
        let val = &arr[i];
        let mut j = i;
        let pos = arr[..i]
            .binary_search_by(|x| compare(x, val))
            .unwrap_or_else(|pos| pos);
        // Swap all elements until specific position.
        while j > pos {
            arr.swap(j - 1, j);
            j -= 1;
        }
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_assigned_fields<'a>(
    env: &mut Env,
    fields: &&[Loc<AssignedField<'a, TypeAnnotation<'a>>>],
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
    tags: &'a [Loc<Tag<'a>>],
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

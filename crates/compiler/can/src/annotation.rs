use crate::env::Env;
use crate::procedure::{QualifiedReference, References};
use crate::scope::{PendingAbilitiesInScope, Scope, SymbolLookup};
use roc_collections::{ImMap, MutSet, SendMap, VecMap, VecSet};
use roc_module::ident::{Ident, IdentSuffix, Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_parse::ast::{
    AssignedField, ExtractSpaces, FunctionArrow, Tag, TypeAnnotation, TypeHeader, TypeVar,
};
use roc_problem::can::{Problem, ShadowKind};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{
    name_type_var, AbilitySet, Alias, AliasCommon, AliasKind, AliasVar, ExtImplicitOpenness,
    LambdaSet, OptAbleType, OptAbleVar, RecordField, Type, TypeExtension,
};

#[derive(Clone, Debug)]
pub struct Annotation {
    pub typ: Type,
    pub introduced_variables: IntroducedVariables,
    pub references: References,
    pub aliases: VecMap<Symbol, Alias>,
}

impl Annotation {
    pub fn add_to(
        &self,
        aliases: &mut VecMap<Symbol, Alias>,
        references: &mut References,
        introduced_variables: &mut IntroducedVariables,
    ) {
        references.union_mut(&self.references);

        introduced_variables.union(&self.introduced_variables);

        for (name, alias) in self.aliases.iter() {
            if !aliases.contains_key(name) {
                aliases.insert(*name, alias.clone());
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NamedOrAbleVariable<'a> {
    Named(&'a NamedVariable),
    Able(&'a AbleVariable),
}

impl<'a> NamedOrAbleVariable<'a> {
    pub fn first_seen(&self) -> Region {
        match self {
            NamedOrAbleVariable::Named(nv) => nv.first_seen,
            NamedOrAbleVariable::Able(av) => av.first_seen,
        }
    }

    pub fn name(&self) -> &Lowercase {
        match self {
            NamedOrAbleVariable::Named(nv) => &nv.name,
            NamedOrAbleVariable::Able(av) => &av.name,
        }
    }

    pub fn variable(&self) -> Variable {
        match self {
            NamedOrAbleVariable::Named(nv) => nv.variable,
            NamedOrAbleVariable::Able(av) => av.variable,
        }
    }
}

pub enum OwnedNamedOrAble {
    Named(NamedVariable),
    Able(AbleVariable),
}

impl OwnedNamedOrAble {
    pub fn first_seen(&self) -> Region {
        match self {
            OwnedNamedOrAble::Named(nv) => nv.first_seen,
            OwnedNamedOrAble::Able(av) => av.first_seen,
        }
    }

    pub fn ref_name(&self) -> &Lowercase {
        match self {
            OwnedNamedOrAble::Named(nv) => &nv.name,
            OwnedNamedOrAble::Able(av) => &av.name,
        }
    }

    pub fn name(self) -> Lowercase {
        match self {
            OwnedNamedOrAble::Named(nv) => nv.name,
            OwnedNamedOrAble::Able(av) => av.name,
        }
    }

    pub fn variable(&self) -> Variable {
        match self {
            OwnedNamedOrAble::Named(nv) => nv.variable,
            OwnedNamedOrAble::Able(av) => av.variable,
        }
    }

    pub fn opt_abilities(&self) -> Option<&AbilitySet> {
        match self {
            OwnedNamedOrAble::Named(_) => None,
            OwnedNamedOrAble::Able(av) => Some(&av.abilities),
        }
    }
}

/// A named type variable, not bound to an ability.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamedVariable {
    pub variable: Variable,
    pub name: Lowercase,
    // NB: there may be multiple occurrences of a variable
    pub first_seen: Region,
}

/// A type variable bound to an ability, like "a implements Hash".
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbleVariable {
    pub variable: Variable,
    pub name: Lowercase,
    pub abilities: AbilitySet,
    // NB: there may be multiple occurrences of a variable
    pub first_seen: Region,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct IntroducedVariables {
    pub wildcards: Vec<Loc<Variable>>,
    pub lambda_sets: Vec<Variable>,
    /// Explicit inference variables, i.e. `_`
    pub inferred: Vec<Loc<Variable>>,
    /// Named type variables
    pub named: VecSet<NamedVariable>,
    /// Named type variables bound to an ability
    pub able: VecSet<AbleVariable>,
    /// Extension variables which should be inferred in output position.
    pub infer_ext_in_output: Vec<Variable>,
}

impl IntroducedVariables {
    #[inline(always)]
    fn debug_assert_not_already_present(&self, var: Variable) {
        debug_assert!((self.wildcards.iter().map(|v| &v.value))
            .chain(self.lambda_sets.iter())
            .chain(self.inferred.iter().map(|v| &v.value))
            .chain(self.named.iter().map(|nv| &nv.variable))
            .chain(self.able.iter().map(|av| &av.variable))
            .chain(self.infer_ext_in_output.iter())
            .all(|&v| v != var));
    }

    pub fn insert_named(&mut self, name: Lowercase, var: Loc<Variable>) {
        self.debug_assert_not_already_present(var.value);

        let named_variable = NamedVariable {
            name,
            variable: var.value,
            first_seen: var.region,
        };

        self.named.insert(named_variable);
    }

    pub fn insert_able(&mut self, name: Lowercase, var: Loc<Variable>, abilities: AbilitySet) {
        self.debug_assert_not_already_present(var.value);

        let able_variable = AbleVariable {
            name,
            abilities,
            variable: var.value,
            first_seen: var.region,
        };

        self.able.insert(able_variable);
    }

    pub fn insert_wildcard(&mut self, var: Loc<Variable>) {
        self.debug_assert_not_already_present(var.value);
        self.wildcards.push(var);
    }

    pub fn insert_inferred(&mut self, var: Loc<Variable>) {
        self.debug_assert_not_already_present(var.value);
        self.inferred.push(var);
    }

    pub fn insert_infer_ext_in_output(&mut self, var: Variable) {
        self.debug_assert_not_already_present(var);
        self.infer_ext_in_output.push(var);
    }

    pub fn insert_lambda_set(&mut self, var: Variable) {
        self.debug_assert_not_already_present(var);
        self.lambda_sets.push(var);
    }

    pub fn union(&mut self, other: &Self) {
        self.wildcards.extend(other.wildcards.iter().copied());
        self.lambda_sets.extend(other.lambda_sets.iter().copied());
        self.inferred.extend(other.inferred.iter().copied());

        self.named.extend(other.named.iter().cloned());
        self.able.extend(other.able.iter().cloned());
        self.infer_ext_in_output
            .extend(other.infer_ext_in_output.iter().cloned());
    }

    pub fn union_owned(&mut self, other: Self) {
        self.wildcards.extend(other.wildcards);
        self.lambda_sets.extend(other.lambda_sets);
        self.inferred.extend(other.inferred);

        self.named.extend(other.named);
        self.able.extend(other.able);
        self.infer_ext_in_output.extend(other.infer_ext_in_output);
    }

    pub fn var_by_name(&self, name: &Lowercase) -> Option<Variable> {
        (self.named.iter().map(|nv| (&nv.name, nv.variable)))
            .chain(self.able.iter().map(|av| (&av.name, av.variable)))
            .find(|(cand, _)| cand == &name)
            .map(|(_, var)| var)
    }

    pub fn iter_named(&self) -> impl Iterator<Item = NamedOrAbleVariable> {
        (self.named.iter().map(NamedOrAbleVariable::Named))
            .chain(self.able.iter().map(NamedOrAbleVariable::Able))
    }

    pub fn named_var_by_name(&self, name: &Lowercase) -> Option<NamedOrAbleVariable> {
        self.iter_named().find(|v| v.name() == name)
    }

    pub fn collect_able(&self) -> Vec<Variable> {
        self.able.iter().map(|av| av.variable).collect()
    }

    pub fn collect_rigid(&self) -> Vec<Variable> {
        (self.named.iter().map(|nv| nv.variable))
            .chain(self.wildcards.iter().map(|wc| wc.value))
            // For our purposes, lambda set vars are treated like rigids
            .chain(self.lambda_sets.iter().copied())
            .collect()
    }

    pub fn collect_flex(&self) -> Vec<Variable> {
        self.inferred.iter().map(|iv| iv.value).collect()
    }
}

fn malformed(env: &mut Env, region: Region, name: &str) {
    use roc_problem::can::RuntimeError::*;

    let problem = MalformedTypeName((*name).into(), region);
    env.problem(roc_problem::can::Problem::RuntimeError(problem));
}

pub(crate) enum AnnotationFor {
    Value,
    Alias,
    Opaque,
}

/// Canonicalizes a top-level type annotation.
pub(crate) fn canonicalize_annotation(
    env: &mut Env,
    scope: &mut Scope,
    annotation: &TypeAnnotation,
    region: Region,
    var_store: &mut VarStore,
    pending_abilities_in_scope: &PendingAbilitiesInScope,
    annotation_for: AnnotationFor,
) -> Annotation {
    let mut introduced_variables = IntroducedVariables::default();
    let mut references = References::new();
    let mut aliases = VecMap::default();

    let (annotation, region) = match annotation {
        TypeAnnotation::Where(annotation, clauses) => {
            // Add each "implements" clause. The association of a variable to an ability will be saved on
            // `introduced_variables`, which we'll process later.
            for clause in clauses.iter() {
                let opt_err = canonicalize_has_clause(
                    env,
                    scope,
                    var_store,
                    &mut introduced_variables,
                    clause,
                    pending_abilities_in_scope,
                    &mut references,
                );
                if let Err(err_type) = opt_err {
                    return Annotation {
                        typ: err_type,
                        introduced_variables,
                        references,
                        aliases,
                    };
                }
            }
            (&annotation.value, annotation.region)
        }
        annot => (annot, region),
    };

    let pol = match annotation_for {
        // Values always have positive polarity.
        AnnotationFor::Value => CanPolarity::Pos,
        AnnotationFor::Alias => CanPolarity::InAlias,
        AnnotationFor::Opaque => CanPolarity::InOpaque,
    };

    let typ = can_annotation_help(
        env,
        pol,
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum CanPolarity {
    /// In an alias; polarity should be disregarded for now.
    InAlias,
    /// In an opaque type; polarity should be disregarded for now.
    InOpaque,
    Neg,
    Pos,
}

impl CanPolarity {
    fn set_neg(self) -> Self {
        match self {
            CanPolarity::InAlias | CanPolarity::InOpaque => self,
            CanPolarity::Neg | CanPolarity::Pos => CanPolarity::Neg,
        }
    }

    fn set_pos(self) -> Self {
        match self {
            CanPolarity::InAlias | CanPolarity::InOpaque => self,
            CanPolarity::Neg | CanPolarity::Pos => CanPolarity::Pos,
        }
    }
}

pub(crate) fn make_apply_symbol(
    env: &mut Env,
    region: Region,
    scope: &mut Scope,
    module_name: &str,
    ident: &str,
    references: &mut References,
) -> Result<Symbol, Type> {
    if module_name.is_empty() {
        // Since module_name was empty, this is an unqualified type.
        // Look it up in scope!

        match scope.lookup_str(ident, region) {
            Ok(SymbolLookup {
                symbol,
                module_params: _,
            }) => {
                references.insert_type_lookup(symbol, QualifiedReference::Unqualified);
                Ok(symbol)
            }
            Err(problem) => {
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                Err(Type::Error)
            }
        }
    } else {
        match env.qualified_lookup(scope, module_name, ident, region) {
            Ok(SymbolLookup {
                symbol,
                module_params: _,
            }) => {
                references.insert_type_lookup(symbol, QualifiedReference::Qualified);
                Ok(symbol)
            }
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(roc_problem::can::Problem::RuntimeError(problem));

                // A failed import should have already been reported through
                // roc_can::env::Env::qualified_lookup's checks
                Err(Type::Error)
            }
        }
    }
}

/// Retrieves all symbols in an annotations that reference a type definition, that is either an
/// alias or an opaque type.
///
/// For example, in `[A Age U8, B Str {}]`, there are three type definition references - `Age`,
/// `U8`, and `Str`.
pub fn find_type_def_symbols(
    scope: &mut Scope,
    initial_annotation: &roc_parse::ast::TypeAnnotation,
) -> Vec<Symbol> {
    use roc_parse::ast::TypeAnnotation::*;

    let mut result = Vec::new();

    let mut stack = vec![initial_annotation];

    while let Some(annotation) = stack.pop() {
        match annotation {
            Apply(_module_name, ident, arguments) => {
                let ident: Ident = (*ident).into();
                let symbol = scope.scopeless_symbol(&ident, Region::zero());

                result.push(symbol);

                for t in arguments.iter() {
                    stack.push(&t.value);
                }
            }
            Function(arguments, _arrow, result) => {
                for t in arguments.iter() {
                    stack.push(&t.value);
                }

                stack.push(&result.value);
            }
            BoundVariable(_) => {}
            As(actual, _, _) => {
                stack.push(&actual.value);
            }
            Tuple { elems, ext } => {
                stack.extend(elems.iter().map(|t| &t.value));
                stack.extend(ext.iter().map(|t| &t.value));
            }
            Record { fields, ext } => {
                let mut inner_stack = Vec::with_capacity(fields.items.len());

                for field in fields.items.iter() {
                    inner_stack.push(&field.value)
                }

                while let Some(assigned_field) = inner_stack.pop() {
                    match assigned_field {
                        AssignedField::RequiredValue(_, _, t)
                        | AssignedField::OptionalValue(_, _, t)
                        | AssignedField::IgnoredValue(_, _, t) => {
                            stack.push(&t.value);
                        }
                        AssignedField::LabelOnly(_) => {}
                        AssignedField::SpaceBefore(inner, _)
                        | AssignedField::SpaceAfter(inner, _) => inner_stack.push(inner),
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
                        Tag::Apply { args, .. } => {
                            for t in args.iter() {
                                stack.push(&t.value);
                            }
                        }
                        Tag::SpaceBefore(inner, _) | Tag::SpaceAfter(inner, _) => {
                            inner_stack.push(inner)
                        }
                    }
                }

                for t in ext.iter() {
                    stack.push(&t.value);
                }
            }
            SpaceBefore(inner, _) | SpaceAfter(inner, _) => {
                stack.push(inner);
            }
            Where(annotation, clauses) => {
                stack.push(&annotation.value);

                for has_clause in clauses.iter() {
                    for ab in has_clause.value.abilities {
                        stack.push(&ab.value);
                    }
                }
            }
            Inferred | Wildcard | Malformed(_) => {}
        }
    }

    result
}

fn find_fresh_var_name(introduced_variables: &IntroducedVariables) -> Lowercase {
    name_type_var("", 0, &mut introduced_variables.iter_named(), |var, str| {
        var.name().as_str() == str
    })
    .0
}

#[allow(clippy::too_many_arguments)]
fn can_annotation_help(
    env: &mut Env,
    pol: CanPolarity,
    annotation: &roc_parse::ast::TypeAnnotation,
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut VecMap<Symbol, Alias>,
    references: &mut References,
) -> Type {
    use roc_parse::ast::TypeAnnotation::*;

    match annotation {
        Function(argument_types, arrow, return_type) => {
            let mut args = Vec::new();

            for arg in *argument_types {
                let arg_ann = can_annotation_help(
                    env,
                    pol.set_neg(),
                    &arg.value,
                    arg.region,
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
                pol.set_pos(),
                &return_type.value,
                return_type.region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );

            let lambda_set = var_store.fresh();
            introduced_variables.insert_lambda_set(lambda_set);
            let closure = Type::Variable(lambda_set);

            let fx_type = match arrow {
                FunctionArrow::Pure => Type::Pure,
                FunctionArrow::Effectful => Type::Effectful,
            };

            Type::Function(args, Box::new(closure), Box::new(ret), Box::new(fx_type))
        }
        Apply(module_name, ident, type_arguments) => {
            let symbol = match make_apply_symbol(env, region, scope, module_name, ident, references)
            {
                Err(problem) => return problem,
                Ok(symbol) => symbol,
            };

            let mut args = Vec::new();

            if scope.abilities_store.is_ability(symbol) {
                let fresh_ty_var = find_fresh_var_name(introduced_variables);

                env.problem(roc_problem::can::Problem::AbilityUsedAsType(
                    fresh_ty_var.clone(),
                    symbol,
                    region,
                ));

                // Generate an variable bound to the ability so we can keep compiling.
                let var = var_store.fresh();
                introduced_variables.insert_able(
                    fresh_ty_var,
                    Loc::at(region, var),
                    AbilitySet::singleton(symbol),
                );
                return Type::Variable(var);
            }

            for arg in *type_arguments {
                let arg_ann = can_annotation_help(
                    env,
                    pol,
                    &arg.value,
                    arg.region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                args.push(Loc::at(arg.region, arg_ann));
            }

            match scope.lookup_alias(symbol) {
                Some(alias) => {
                    // use a known alias

                    if alias.type_variables.len() != args.len() {
                        env.problem(roc_problem::can::Problem::BadTypeArguments {
                            symbol,
                            region,
                            alias_needs: alias.type_variables.len() as u8,
                            type_got: args.len() as u8,
                            alias_kind: alias.kind,
                        });
                        return Type::Error;
                    }

                    let mut type_var_to_arg = Vec::new();

                    for (alias_arg, arg_ann) in alias.type_variables.iter().zip(args) {
                        type_var_to_arg.push(Loc::at(
                            arg_ann.region,
                            OptAbleType {
                                typ: arg_ann.value,
                                opt_abilities: alias_arg.value.opt_bound_abilities.clone(),
                            },
                        ));
                    }

                    let mut lambda_set_variables =
                        Vec::with_capacity(alias.lambda_set_variables.len());

                    for _ in 0..alias.lambda_set_variables.len() {
                        let lvar = var_store.fresh();

                        introduced_variables.insert_lambda_set(lvar);

                        lambda_set_variables.push(LambdaSet(Type::Variable(lvar)));
                    }

                    let mut infer_ext_in_output_types =
                        Vec::with_capacity(alias.infer_ext_in_output_variables.len());
                    for _ in 0..alias.infer_ext_in_output_variables.len() {
                        // Unfortunately the polarity might still be undetermined at this point,
                        // since this might be a delayed alias inside an alias. In these cases
                        // generate fresh variables to hold the extension-variables-to-be-inferred,
                        // which will be instantiated when the alias is used at a concrete site.
                        // Otherwise, instantiate the variables with how they should behave based
                        // on the polarity
                        let typ = match pol {
                            CanPolarity::InAlias | CanPolarity::Pos => {
                                let var = var_store.fresh();
                                introduced_variables.insert_infer_ext_in_output(var);
                                Type::Variable(var)
                            }
                            // TODO: determine for opaques
                            CanPolarity::InOpaque => Type::EmptyTagUnion,
                            CanPolarity::Neg => Type::EmptyTagUnion,
                        };
                        infer_ext_in_output_types.push(typ);
                    }

                    Type::DelayedAlias(AliasCommon {
                        symbol,
                        type_arguments: type_var_to_arg,
                        lambda_set_variables,
                        infer_ext_in_output_types,
                    })
                }
                None => Type::Apply(symbol, args, region),
            }
        }
        BoundVariable(v) => {
            let name = Lowercase::from(*v);

            match introduced_variables.var_by_name(&name) {
                Some(var) => Type::Variable(var),
                None => {
                    let var = var_store.fresh();

                    introduced_variables.insert_named(name, Loc::at(region, var));

                    Type::Variable(var)
                }
            }
        }
        As(
            loc_inner,
            _spaces,
            alias_header @ TypeHeader {
                name,
                vars: loc_vars,
            },
        ) => {
            let symbol = match scope.introduce(name.value.into(), region) {
                Ok(symbol) => symbol,

                Err((shadowed_symbol, shadow, _new_symbol)) => {
                    env.problem(roc_problem::can::Problem::Shadowing {
                        original_region: shadowed_symbol.region,
                        shadow,
                        kind: ShadowKind::Variable,
                    });

                    return Type::Error;
                }
            };

            let inner_type = can_annotation_help(
                env,
                CanPolarity::InOpaque,
                &loc_inner.value,
                region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );
            let mut vars = Vec::with_capacity(loc_vars.len());
            let mut lowercase_vars: Vec<Loc<AliasVar>> = Vec::with_capacity(loc_vars.len());

            references.insert_type_lookup(symbol, QualifiedReference::Unqualified);

            for loc_var in *loc_vars {
                let var = match loc_var.value {
                    TypeVar::Identifier(name) if name.chars().next().unwrap().is_lowercase() => {
                        name
                    }
                    _ => unreachable!("I thought this was validated during parsing"),
                };
                let var_name = Lowercase::from(var);

                // TODO(abilities): check that there are no abilities bound here.
                if let Some(var) = introduced_variables.var_by_name(&var_name) {
                    vars.push(Type::Variable(var));
                    lowercase_vars.push(Loc::at(
                        loc_var.region,
                        AliasVar {
                            name: var_name,
                            var,
                            opt_bound_abilities: None,
                        },
                    ));
                } else {
                    let var = var_store.fresh();

                    introduced_variables
                        .insert_named(var_name.clone(), Loc::at(loc_var.region, var));
                    vars.push(Type::Variable(var));

                    lowercase_vars.push(Loc::at(
                        loc_var.region,
                        AliasVar {
                            name: var_name,
                            var,
                            opt_bound_abilities: None,
                        },
                    ));
                }
            }

            let alias_args = vars.clone();

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
                hidden_variables.remove(&loc_var.value.var);
            }

            // TODO: handle implicit ext variables in `as` aliases
            let infer_ext_in_output = vec![];

            {
                let roc_types::types::VariableDetail {
                    type_variables,
                    lambda_set_variables: _,
                    recursion_variables,
                } = alias_actual.variables_detail();

                let mut hidden = type_variables;

                for var in (lowercase_vars.iter().map(|lv| lv.value.var))
                    .chain(recursion_variables.iter().copied())
                    .chain(infer_ext_in_output.iter().copied())
                {
                    hidden.remove(&var);
                }

                if !hidden.is_empty() {
                    env.problem(roc_problem::can::Problem::UnboundTypeVarsInAs(region));
                    return Type::Error;
                }
            }

            scope.add_alias(
                symbol,
                region,
                lowercase_vars,
                infer_ext_in_output,
                alias_actual,
                AliasKind::Structural, // aliases in "as" are never opaque
            );

            let alias = scope.lookup_alias(symbol).unwrap();
            local_aliases.insert(symbol, alias.clone());

            Type::Alias {
                symbol,
                type_arguments: vars.into_iter().map(OptAbleType::unbound).collect(),
                lambda_set_variables: alias.lambda_set_variables.clone(),
                infer_ext_in_output_types: alias
                    .infer_ext_in_output_variables
                    .iter()
                    .map(|v| Type::Variable(*v))
                    .collect(),
                actual: Box::new(alias.typ.clone()),
                kind: alias.kind,
            }
        }

        Tuple { elems, ext } => {
            let (ext_type, is_implicit_openness) = can_extension_type(
                env,
                pol,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
                ext,
                roc_problem::can::ExtensionTypeKind::Record,
            );

            debug_assert!(
                matches!(is_implicit_openness, ExtImplicitOpenness::No),
                "tuples should never be implicitly inferred open"
            );

            if elems.is_empty() {
                env.problem(roc_problem::can::Problem::EmptyTupleType(region));
            }

            let elem_types = can_assigned_tuple_elems(
                env,
                pol,
                &elems.items,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );

            Type::Tuple(
                elem_types,
                TypeExtension::from_type(ext_type, is_implicit_openness),
            )
        }
        Record { fields, ext } => {
            let (ext_type, is_implicit_openness) = can_extension_type(
                env,
                pol,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
                ext,
                roc_problem::can::ExtensionTypeKind::Record,
            );

            debug_assert!(
                matches!(is_implicit_openness, ExtImplicitOpenness::No),
                "records should never be implicitly inferred open"
            );

            if fields.is_empty() {
                match ext {
                    Some(_) => {
                        // just `a` does not mean the same as `{}a`, so even
                        // if there are no fields, still make this a `Record`,
                        // not an EmptyRec
                        Type::Record(
                            Default::default(),
                            TypeExtension::from_type(ext_type, is_implicit_openness),
                        )
                    }

                    None => Type::EmptyRec,
                }
            } else {
                let field_types = can_assigned_fields(
                    env,
                    pol,
                    &fields.items,
                    region,
                    scope,
                    var_store,
                    introduced_variables,
                    local_aliases,
                    references,
                );

                Type::Record(
                    field_types,
                    TypeExtension::from_type(ext_type, is_implicit_openness),
                )
            }
        }
        TagUnion { tags, ext, .. } => {
            let (ext_type, is_implicit_openness) = can_extension_type(
                env,
                pol,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
                ext,
                roc_problem::can::ExtensionTypeKind::TagUnion,
            );

            if tags.is_empty() {
                Type::TagUnion(
                    Default::default(),
                    TypeExtension::from_type(ext_type, is_implicit_openness),
                )
            } else {
                let mut tag_types = can_tags(
                    env,
                    pol,
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

                Type::TagUnion(
                    tag_types,
                    TypeExtension::from_type(ext_type, is_implicit_openness),
                )
            }
        }
        SpaceBefore(nested, _) | SpaceAfter(nested, _) => can_annotation_help(
            env,
            pol,
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

            introduced_variables.insert_wildcard(Loc::at(region, var));

            Type::Variable(var)
        }
        Inferred => {
            // Inference variables aren't bound to a rigid or a wildcard, so all we have to do is
            // make a fresh unconstrained variable, and let the type solver fill it in for us 🤠
            let var = var_store.fresh();

            introduced_variables.insert_inferred(Loc::at(region, var));

            Type::Variable(var)
        }
        Where(_annotation, clauses) => {
            debug_assert!(!clauses.is_empty());

            // Implements clauses are allowed only on the top level of a signature, which we handle elsewhere.
            env.problem(roc_problem::can::Problem::IllegalImplementsClause {
                region: Region::across_all(clauses.iter().map(|clause| &clause.region)),
            });

            Type::Error
        }
        Malformed(string) => {
            malformed(env, region, string);

            let var = var_store.fresh();

            introduced_variables.insert_wildcard(Loc::at(region, var));

            Type::Variable(var)
        }
    }
}

fn canonicalize_has_clause(
    env: &mut Env,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    clause: &Loc<roc_parse::ast::ImplementsClause<'_>>,
    pending_abilities_in_scope: &PendingAbilitiesInScope,
    references: &mut References,
) -> Result<(), Type> {
    let Loc {
        region,
        value: roc_parse::ast::ImplementsClause { var, abilities },
    } = clause;
    let region = *region;

    let var_name = var.extract_spaces().item;
    debug_assert!(
        var_name.starts_with(char::is_lowercase),
        "Vars should have been parsed as lowercase"
    );
    let var_name = Lowercase::from(var_name);

    let mut can_abilities = AbilitySet::with_capacity(abilities.len());
    for &Loc {
        region,
        value: ability,
    } in *abilities
    {
        let ability = match ability {
            TypeAnnotation::Apply(module_name, ident, _type_arguments) => {
                let symbol = make_apply_symbol(env, region, scope, module_name, ident, references)?;

                // Ability defined locally, whose members we are constructing right now...
                if !pending_abilities_in_scope.contains_key(&symbol)
                // or an ability that was imported from elsewhere
                && !scope.abilities_store.is_ability(symbol)
                {
                    env.problem(roc_problem::can::Problem::ImplementsClauseIsNotAbility { region });
                    return Err(Type::Error);
                }
                symbol
            }
            _ => {
                env.problem(roc_problem::can::Problem::ImplementsClauseIsNotAbility { region });
                return Err(Type::Error);
            }
        };

        let already_seen = can_abilities.insert(ability);

        if already_seen {
            env.problem(roc_problem::can::Problem::DuplicateImplementsAbility { ability, region });
        }
    }

    if let Some(shadowing) = introduced_variables.named_var_by_name(&var_name) {
        let var_name_ident = var_name.to_string().into();
        let shadow = Loc::at(region, var_name_ident);
        env.problem(roc_problem::can::Problem::Shadowing {
            original_region: shadowing.first_seen(),
            shadow,
            kind: ShadowKind::Variable,
        });
        return Err(Type::Error);
    }

    let var = var_store.fresh();

    introduced_variables.insert_able(var_name, Loc::at(region, var), can_abilities);

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn can_extension_type(
    env: &mut Env,
    pol: CanPolarity,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut VecMap<Symbol, Alias>,
    references: &mut References,
    opt_ext: &Option<&Loc<TypeAnnotation>>,
    ext_problem_kind: roc_problem::can::ExtensionTypeKind,
) -> (Type, ExtImplicitOpenness) {
    fn valid_record_ext_type(typ: &Type) -> bool {
        // Include erroneous types so that we don't overreport errors.
        matches!(
            typ,
            Type::EmptyRec | Type::Record(..) | Type::Variable(..) | Type::Error
        )
    }
    fn valid_tag_ext_type(typ: &Type) -> bool {
        matches!(
            typ,
            Type::EmptyTagUnion | Type::TagUnion(..) | Type::Variable(..) | Type::Error
        )
    }

    use roc_problem::can::ExtensionTypeKind;

    let valid_extension_type: fn(&Type) -> bool = match ext_problem_kind {
        ExtensionTypeKind::Record => valid_record_ext_type,
        ExtensionTypeKind::TagUnion => valid_tag_ext_type,
    };

    match opt_ext {
        Some(loc_ann) => {
            let ext_type = can_annotation_help(
                env,
                pol,
                &loc_ann.value,
                loc_ann.region,
                scope,
                var_store,
                introduced_variables,
                local_aliases,
                references,
            );

            if valid_extension_type(shallow_dealias_with_scope(scope, &ext_type)) {
                if matches!(loc_ann.extract_spaces().item, TypeAnnotation::Wildcard)
                    && matches!(ext_problem_kind, ExtensionTypeKind::TagUnion)
                    && pol == CanPolarity::Pos
                {
                    // Wildcards are redundant in positive positions, since they will always be
                    // inferred as necessary there!
                    env.problem(roc_problem::can::Problem::UnnecessaryOutputWildcard {
                        region: loc_ann.region,
                    })
                }

                (ext_type, ExtImplicitOpenness::No)
            } else {
                // Report an error but mark the extension variable to be inferred
                // so that we're as permissive as possible.
                //
                // THEORY: invalid extension types can appear in this position. Otherwise
                // they would be caught as errors during unification.
                env.problem(roc_problem::can::Problem::InvalidExtensionType {
                    region: loc_ann.region,
                    kind: ext_problem_kind,
                });

                let var = var_store.fresh();

                introduced_variables.insert_inferred(Loc::at_zero(var));

                (
                    Type::Variable(var),
                    // Since this is an error anyway, just be permissive
                    ExtImplicitOpenness::No,
                )
            }
        }
        None => match ext_problem_kind {
            ExtensionTypeKind::Record => (Type::EmptyRec, ExtImplicitOpenness::No),
            ExtensionTypeKind::TagUnion => {
                // In negative positions a missing extension variable forces a closed tag union;
                // otherwise, open-in-output-position means we give the tag an inference variable.
                match pol {
                    CanPolarity::Neg | CanPolarity::InOpaque => {
                        (Type::EmptyTagUnion, ExtImplicitOpenness::No)
                    }
                    CanPolarity::Pos | CanPolarity::InAlias => {
                        let var = var_store.fresh();
                        introduced_variables.insert_infer_ext_in_output(var);

                        (Type::Variable(var), ExtImplicitOpenness::Yes)
                    }
                }
            }
        },
    }
}

/// a shallow dealias, continue until the first constructor is not an alias.
fn shallow_dealias_with_scope<'a>(scope: &'a mut Scope, typ: &'a Type) -> &'a Type {
    let mut result = typ;
    loop {
        match result {
            Type::Alias { actual, .. } => {
                // another loop
                result = actual;
            }
            Type::DelayedAlias(AliasCommon { symbol, .. }) => match scope.lookup_alias(*symbol) {
                None => unreachable!(),
                Some(alias) => {
                    result = &alias.typ;
                }
            },

            _ => break,
        }
    }

    result
}

pub fn freshen_opaque_def(
    var_store: &mut VarStore,
    opaque: &Alias,
) -> (Vec<OptAbleVar>, Vec<LambdaSet>, Type) {
    debug_assert!(opaque.kind == AliasKind::Opaque);

    let fresh_variables: Vec<OptAbleVar> = opaque
        .type_variables
        .iter()
        .map(|alias_var| OptAbleVar {
            var: var_store.fresh(),
            opt_abilities: alias_var.value.opt_bound_abilities.clone(),
        })
        .collect();

    // NB: We don't introduce the fresh variables here, we introduce them during constraint gen.
    // NB: If there are bugs, check whether this is a problem!
    let mut introduced_variables = IntroducedVariables::default();

    let mut substitutions = ImMap::default();

    // Freshen all type variables used in the opaque.
    for (loc_var, fresh_var) in opaque.type_variables.iter().zip(fresh_variables.iter()) {
        let old_var = loc_var.value.var;
        substitutions.insert(old_var, Type::Variable(fresh_var.var));
        // NB: fresh var not introduced in this pass; see above.
    }

    // Freshen all nested recursion variables.
    for &rec_var in opaque.recursion_variables.iter() {
        let new = var_store.fresh();
        substitutions.insert(rec_var, Type::Variable(new));
    }

    // Freshen all nested lambda sets.
    let mut new_lambda_set_variables = Vec::with_capacity(opaque.lambda_set_variables.len());
    for typ in opaque.lambda_set_variables.iter() {
        if let Type::Variable(var) = typ.0 {
            let fresh = var_store.fresh();
            substitutions.insert(var, Type::Variable(fresh));
            introduced_variables.insert_lambda_set(fresh);
            new_lambda_set_variables.push(LambdaSet(Type::Variable(fresh)));
        } else {
            unreachable!("at this point there should be only vars in there");
        }
    }

    // Fresh the real type with our new variables.
    let actual_type = {
        let mut typ = opaque.typ.clone();
        typ.substitute(&substitutions);
        typ
    };

    (fresh_variables, new_lambda_set_variables, actual_type)
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
    pol: CanPolarity,
    fields: &&[Loc<AssignedField<'a, TypeAnnotation<'a>>>],
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut VecMap<Symbol, Alias>,
    references: &mut References,
) -> SendMap<Lowercase, RecordField<Type>> {
    use roc_parse::ast::AssignedField::*;
    use roc_types::types::RecordField::*;

    // SendMap doesn't have a `with_capacity`
    let mut field_types = SendMap::default();

    // field names we've seen so far in this record
    let mut seen = std::collections::HashMap::with_capacity(fields.len());

    for loc_field in fields.iter() {
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
                        pol,
                        &annotation.value,
                        annotation.region,
                        scope,
                        var_store,
                        introduced_variables,
                        local_aliases,
                        references,
                    );

                    let label = Lowercase::from(field_name.value);
                    check_record_field_suffix(env, label.suffix(), &field_type, &loc_field.region);

                    field_types.insert(label.clone(), RigidRequired(field_type));

                    break 'inner label;
                }
                OptionalValue(field_name, _, annotation) => {
                    let field_type = can_annotation_help(
                        env,
                        pol,
                        &annotation.value,
                        annotation.region,
                        scope,
                        var_store,
                        introduced_variables,
                        local_aliases,
                        references,
                    );

                    let label = Lowercase::from(field_name.value);
                    check_record_field_suffix(env, label.suffix(), &field_type, &loc_field.region);

                    field_types.insert(label.clone(), RigidOptional(field_type));

                    break 'inner label;
                }
                IgnoredValue(_, _, _) => unreachable!(),
                LabelOnly(loc_field_name) => {
                    // Interpret { a, b } as { a : a, b : b }
                    let field_name = Lowercase::from(loc_field_name.value);
                    let field_type = {
                        if let Some(var) = introduced_variables.var_by_name(&field_name) {
                            Type::Variable(var)
                        } else {
                            let field_var = var_store.fresh();
                            introduced_variables.insert_named(
                                field_name.clone(),
                                Loc::at(loc_field_name.region, field_var),
                            );
                            Type::Variable(field_var)
                        }
                    };

                    field_types.insert(field_name.clone(), RigidRequired(field_type));

                    break 'inner field_name;
                }
                SpaceBefore(nested, _) | SpaceAfter(nested, _) => {
                    // check the nested field instead
                    field = nested;
                    continue 'inner;
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

fn check_record_field_suffix(
    env: &mut Env,
    suffix: IdentSuffix,
    field_type: &Type,
    region: &Region,
) {
    match (suffix, field_type) {
        (IdentSuffix::None, Type::Function(_, _, _, fx)) if **fx == Type::Effectful => env
            .problems
            .push(Problem::UnsuffixedEffectfulRecordField(*region)),
        (IdentSuffix::Bang, Type::Function(_, _, _, fx)) if **fx == Type::Pure => {
            env.problems.push(Problem::SuffixedPureRecordField(*region))
        }
        _ => {}
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_assigned_tuple_elems(
    env: &mut Env,
    pol: CanPolarity,
    elems: &&[Loc<TypeAnnotation>],
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut VecMap<Symbol, Alias>,
    references: &mut References,
) -> VecMap<usize, Type> {
    let mut elem_types = VecMap::with_capacity(elems.len());

    for (index, loc_elem) in elems.iter().enumerate() {
        let elem_type = can_annotation_help(
            env,
            pol,
            &loc_elem.value,
            loc_elem.region,
            scope,
            var_store,
            introduced_variables,
            local_aliases,
            references,
        );

        elem_types.insert(index, elem_type);
    }

    elem_types
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
fn can_tags<'a>(
    env: &mut Env,
    pol: CanPolarity,
    tags: &'a [Loc<Tag<'a>>],
    region: Region,
    scope: &mut Scope,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
    local_aliases: &mut VecMap<Symbol, Alias>,
    references: &mut References,
) -> Vec<(TagName, Vec<Type>)> {
    let mut tag_types = Vec::with_capacity(tags.len());

    // tag names we've seen so far in this tag union
    let mut seen = std::collections::HashMap::with_capacity(tags.len());

    for loc_tag in tags.iter() {
        let mut tag = &loc_tag.value;

        // use this inner loop to unwrap the SpaceAfter/SpaceBefore
        // when we find the name of this tag, break out of the loop
        // with that value, so we can check whether the tag name is
        // a duplicate
        let new_name = 'inner: loop {
            match tag {
                Tag::Apply { name, args } => {
                    let name = name.value.into();
                    let mut arg_types = Vec::with_capacity(args.len());

                    for arg in args.iter() {
                        let ann = can_annotation_help(
                            env,
                            pol,
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

                    let tag_name = TagName(name);
                    tag_types.push((tag_name.clone(), arg_types));

                    break 'inner tag_name;
                }
                Tag::SpaceBefore(nested, _) | Tag::SpaceAfter(nested, _) => {
                    // check the nested tag instead
                    tag = nested;
                    continue 'inner;
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

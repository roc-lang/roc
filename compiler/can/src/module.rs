use crate::abilities::AbilitiesStore;
use crate::annotation::canonicalize_annotation;
use crate::def::{canonicalize_defs, sort_can_defs, Declaration, Def};
use crate::effect_module::HostedGeneratedFunctions;
use crate::env::Env;
use crate::expr::{ClosureData, Expr, Output, PendingDerives};
use crate::operator::desugar_def;
use crate::pattern::Pattern;
use crate::scope::Scope;
use bumpalo::Bump;
use roc_collections::{MutMap, SendMap, VecSet};
use roc_module::ident::Ident;
use roc_module::ident::Lowercase;
use roc_module::symbol::{IdentIds, IdentIdsByModule, ModuleId, ModuleIds, Symbol};
use roc_parse::ast::{self, TypeAnnotation};
use roc_parse::header::HeaderFor;
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, AliasKind, AliasVar, Type};

#[derive(Debug)]
pub struct Module {
    pub module_id: ModuleId,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub exposed_symbols: VecSet<Symbol>,
    pub referenced_values: VecSet<Symbol>,
    pub referenced_types: VecSet<Symbol>,
    /// all aliases. `bool` indicates whether it is exposed
    pub aliases: MutMap<Symbol, (bool, Alias)>,
    pub rigid_variables: RigidVariables,
    pub abilities_store: AbilitiesStore,
}

#[derive(Debug, Default)]
pub struct RigidVariables {
    pub named: MutMap<Variable, Lowercase>,
    pub able: MutMap<Variable, (Lowercase, Symbol)>,
    pub wildcards: VecSet<Variable>,
}

#[derive(Debug)]
pub struct ModuleOutput {
    pub aliases: MutMap<Symbol, Alias>,
    pub rigid_variables: RigidVariables,
    pub declarations: Vec<Declaration>,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub lookups: Vec<(Symbol, Variable, Region)>,
    pub problems: Vec<Problem>,
    pub referenced_values: VecSet<Symbol>,
    pub referenced_types: VecSet<Symbol>,
    pub symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    pub pending_derives: PendingDerives,
    pub scope: Scope,
}

fn validate_generate_with<'a>(
    generate_with: &'a [Loc<roc_parse::header::ExposedName<'a>>],
) -> (HostedGeneratedFunctions, Vec<Loc<Ident>>) {
    let mut functions = HostedGeneratedFunctions::default();
    let mut unknown = Vec::new();

    for generated in generate_with {
        match generated.value.as_str() {
            "after" => functions.after = true,
            "map" => functions.map = true,
            "always" => functions.always = true,
            "loop" => functions.loop_ = true,
            "forever" => functions.forever = true,
            other => {
                // we don't know how to generate this function
                let ident = Ident::from(other);
                unknown.push(Loc::at(generated.region, ident));
            }
        }
    }

    (functions, unknown)
}

#[derive(Debug)]
enum GeneratedInfo {
    Hosted {
        effect_symbol: Symbol,
        generated_functions: HostedGeneratedFunctions,
    },
    Builtin,
    NotSpecial,
}

impl GeneratedInfo {
    fn from_header_for<'a>(
        env: &mut Env,
        scope: &mut Scope,
        var_store: &mut VarStore,
        header_for: &HeaderFor<'a>,
    ) -> Self {
        match header_for {
            HeaderFor::Hosted {
                generates,
                generates_with,
            } => {
                let name: &str = generates.into();
                let (generated_functions, unknown_generated) =
                    validate_generate_with(generates_with);

                for unknown in unknown_generated {
                    env.problem(Problem::UnknownGeneratesWith(unknown));
                }

                let effect_symbol = scope.introduce(name.into(), Region::zero()).unwrap();

                {
                    let a_var = var_store.fresh();

                    let actual =
                        crate::effect_module::build_effect_actual(Type::Variable(a_var), var_store);

                    scope.add_alias(
                        effect_symbol,
                        Region::zero(),
                        vec![Loc::at_zero(AliasVar::unbound("a".into(), a_var))],
                        actual,
                        AliasKind::Opaque,
                    );
                }

                GeneratedInfo::Hosted {
                    effect_symbol,
                    generated_functions,
                }
            }
            HeaderFor::Builtin { generates_with } => {
                debug_assert!(generates_with.is_empty());
                GeneratedInfo::Builtin
            }
            _ => GeneratedInfo::NotSpecial,
        }
    }
}

fn has_no_implementation(expr: &Expr) -> bool {
    match expr {
        Expr::RuntimeError(RuntimeError::NoImplementationNamed { .. }) => true,
        Expr::Closure(closure_data)
            if matches!(
                closure_data.loc_body.value,
                Expr::RuntimeError(RuntimeError::NoImplementationNamed { .. })
            ) =>
        {
            true
        }

        _ => false,
    }
}

// TODO trim these down
#[allow(clippy::too_many_arguments)]
pub fn canonicalize_module_defs<'a>(
    arena: &'a Bump,
    loc_defs: &'a [Loc<ast::Def<'a>>],
    header_for: &roc_parse::header::HeaderFor,
    home: ModuleId,
    module_ids: &'a ModuleIds,
    exposed_ident_ids: IdentIds,
    dep_idents: &'a IdentIdsByModule,
    aliases: MutMap<Symbol, Alias>,
    imported_abilities_state: AbilitiesStore,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    exposed_symbols: &VecSet<Symbol>,
    symbols_from_requires: &[(Loc<Symbol>, Loc<TypeAnnotation<'a>>)],
    var_store: &mut VarStore,
) -> ModuleOutput {
    let mut can_exposed_imports = MutMap::default();
    let mut scope = Scope::new(home, exposed_ident_ids, imported_abilities_state);
    let mut env = Env::new(home, dep_idents, module_ids);
    let num_deps = dep_idents.len();

    for (name, alias) in aliases.into_iter() {
        scope.add_alias(
            name,
            alias.region,
            alias.type_variables,
            alias.typ,
            alias.kind,
        );
    }

    let generated_info =
        GeneratedInfo::from_header_for(&mut env, &mut scope, var_store, header_for);

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let mut desugared =
        bumpalo::collections::Vec::with_capacity_in(loc_defs.len() + num_deps, arena);

    for loc_def in loc_defs.iter() {
        desugared.push(&*arena.alloc(Loc {
            value: desugar_def(arena, &loc_def.value),
            region: loc_def.region,
        }));
    }

    let mut lookups = Vec::with_capacity(num_deps);
    let mut rigid_variables = RigidVariables::default();

    // Exposed values are treated like defs that appear before any others, e.g.
    //
    // imports [Foo.{ bar, baz }]
    //
    // ...is basically the same as if we'd added these extra defs at the start of the module:
    //
    // bar = Foo.bar
    // baz = Foo.baz
    //
    // Here we essentially add those "defs" to "the beginning of the module"
    // by canonicalizing them right before we canonicalize the actual ast::Def nodes.
    for (ident, (symbol, region)) in exposed_imports {
        let first_char = ident.as_inline_str().as_str().chars().next().unwrap();

        if first_char.is_lowercase() {
            // this is a value definition
            let expr_var = var_store.fresh();

            match scope.import(ident, symbol, region) {
                Ok(()) => {
                    // Add an entry to exposed_imports using the current module's name
                    // as the key; e.g. if this is the Foo module and we have
                    // exposes [Bar.{ baz }] then insert Foo.baz as the key, so when
                    // anything references `baz` in this Foo module, it will resolve to Bar.baz.
                    can_exposed_imports.insert(symbol, expr_var);

                    // This will be used during constraint generation,
                    // to add the usual Lookup constraint as if this were a normal def.
                    lookups.push((symbol, expr_var, region));
                }
                Err((_shadowed_symbol, _region)) => {
                    panic!("TODO gracefully handle shadowing in imports.")
                }
            }
        } else if [
            Symbol::LIST_LIST,
            Symbol::STR_STR,
            Symbol::DICT_DICT,
            Symbol::SET_SET,
            Symbol::BOX_BOX_TYPE,
        ]
        .contains(&symbol)
        {
            // These are not aliases but Apply's and we make sure they are always in scope
        } else {
            // This is a type alias or ability

            // the symbol should already be added to the scope when this module is canonicalized
            debug_assert!(
                scope.contains_alias(symbol) || scope.abilities_store.is_ability(symbol),
                "The {:?} is not a type alias or ability known in {:?}",
                symbol,
                home
            );

            // but now we know this symbol by a different identifier, so we still need to add it to
            // the scope
            match scope.import(ident, symbol, region) {
                Ok(()) => {
                    // here we do nothing special
                }
                Err((shadowed_symbol, _region)) => {
                    panic!(
                        "TODO gracefully handle shadowing in imports, {:?} is shadowed.",
                        shadowed_symbol
                    )
                }
            }
        }
    }

    let (defs, output, symbols_introduced) = canonicalize_defs(
        &mut env,
        Output::default(),
        var_store,
        &mut scope,
        &desugared,
        PatternType::TopLevelDef,
    );

    let pending_derives = output.pending_derives;

    // See if any of the new idents we defined went unused.
    // If any were unused and also not exposed, report it.
    for (symbol, region) in symbols_introduced {
        if !output.references.has_type_or_value_lookup(symbol)
            && !exposed_symbols.contains(&symbol)
            && !scope.abilities_store.is_specialization_name(symbol)
        {
            env.problem(Problem::UnusedDef(symbol, region));
        }
    }

    for named in output.introduced_variables.named {
        rigid_variables.named.insert(named.variable, named.name);
    }

    for able in output.introduced_variables.able {
        rigid_variables
            .able
            .insert(able.variable, (able.name, able.ability));
    }

    for var in output.introduced_variables.wildcards {
        rigid_variables.wildcards.insert(var.value);
    }

    let mut referenced_values = VecSet::default();
    let mut referenced_types = VecSet::default();

    // Gather up all the symbols that were referenced across all the defs' lookups.
    referenced_values.extend(output.references.value_lookups().copied());
    referenced_types.extend(output.references.type_lookups().copied());

    // Gather up all the symbols that were referenced across all the defs' calls.
    referenced_values.extend(output.references.calls().copied());

    // Gather up all the symbols that were referenced from other modules.
    referenced_values.extend(env.qualified_value_lookups.iter().copied());
    referenced_types.extend(env.qualified_type_lookups.iter().copied());

    // add any builtins used by other builtins
    let transitive_builtins: Vec<Symbol> = referenced_values
        .iter()
        .filter(|s| s.is_builtin())
        .flat_map(|s| crate::builtins::builtin_dependencies(*s))
        .copied()
        .collect();

    referenced_values.extend(transitive_builtins);

    // NOTE previously we inserted builtin defs into the list of defs here
    // this is now done later, in file.rs.

    // assume all exposed symbols are not actually defined in the module
    // then as we walk the module and encounter the definitions, remove
    // symbols from this set
    let mut exposed_but_not_defined = exposed_symbols.clone();

    let new_output = Output {
        aliases: output.aliases,
        ..Default::default()
    };

    let (mut declarations, mut output) = sort_can_defs(&mut env, var_store, defs, new_output);

    debug_assert!(
        output.pending_derives.is_empty(),
        "I thought pending derives are only found during def introduction"
    );

    let symbols_from_requires = symbols_from_requires
        .iter()
        .map(|(symbol, loc_ann)| {
            // We've already canonicalized the module, so there are no pending abilities.
            let pending_abilities_in_scope = &[];

            let ann = canonicalize_annotation(
                &mut env,
                &mut scope,
                &loc_ann.value,
                loc_ann.region,
                var_store,
                pending_abilities_in_scope,
            );

            ann.add_to(
                &mut output.aliases,
                &mut output.references,
                &mut output.introduced_variables,
            );

            (
                *symbol,
                Loc {
                    value: ann.typ,
                    region: loc_ann.region,
                },
            )
        })
        .collect();

    if let GeneratedInfo::Hosted {
        effect_symbol,
        generated_functions,
    } = generated_info
    {
        let mut exposed_symbols = VecSet::default();

        // NOTE this currently builds all functions, not just the ones that the user requested
        crate::effect_module::build_effect_builtins(
            &mut scope,
            effect_symbol,
            var_store,
            &mut exposed_symbols,
            &mut declarations,
            generated_functions,
        );
    }

    use crate::def::Declaration::*;
    for decl in declarations.iter_mut() {
        match decl {
            Declare(def) => {
                for (symbol, _) in def.pattern_vars.iter() {
                    if exposed_but_not_defined.contains(symbol) {
                        // Remove this from exposed_symbols,
                        // so that at the end of the process,
                        // we can see if there were any
                        // exposed symbols which did not have
                        // corresponding defs.
                        exposed_but_not_defined.remove(symbol);
                    }
                }

                // Temporary hack: we don't know exactly what symbols are hosted symbols,
                // and which are meant to be normal definitions without a body. So for now
                // we just assume they are hosted functions (meant to be provided by the platform)
                if has_no_implementation(&def.loc_expr.value) {
                    match generated_info {
                        GeneratedInfo::Builtin => {
                            let symbol = def.pattern_vars.iter().next().unwrap().0;
                            match crate::builtins::builtin_defs_map(*symbol, var_store) {
                                None => {
                                    panic!("A builtin module contains a signature without implementation for {:?}", symbol)
                                }
                                Some(mut replacement_def) => {
                                    replacement_def.annotation = def.annotation.take();
                                    *def = replacement_def;
                                }
                            }
                        }
                        GeneratedInfo::Hosted { effect_symbol, .. } => {
                            let symbol = def.pattern_vars.iter().next().unwrap().0;
                            let ident_id = symbol.ident_id();
                            let ident = scope
                                .locals
                                .ident_ids
                                .get_name(ident_id)
                                .unwrap()
                                .to_string();
                            let def_annotation = def.annotation.clone().unwrap();
                            let annotation = crate::annotation::Annotation {
                                typ: def_annotation.signature,
                                introduced_variables: def_annotation.introduced_variables,
                                references: Default::default(),
                                aliases: Default::default(),
                            };

                            let hosted_def = crate::effect_module::build_host_exposed_def(
                                &mut scope,
                                *symbol,
                                &ident,
                                effect_symbol,
                                var_store,
                                annotation,
                            );

                            *def = hosted_def;
                        }
                        _ => (),
                    }
                }
            }
            DeclareRec(defs, _) => {
                for def in defs {
                    for (symbol, _) in def.pattern_vars.iter() {
                        if exposed_but_not_defined.contains(symbol) {
                            // Remove this from exposed_symbols,
                            // so that at the end of the process,
                            // we can see if there were any
                            // exposed symbols which did not have
                            // corresponding defs.
                            exposed_but_not_defined.remove(symbol);
                        }
                    }
                }
            }

            InvalidCycle(entries) => {
                env.problems.push(Problem::BadRecursion(entries.to_vec()));
            }
            Builtin(def) => {
                // Builtins cannot be exposed in module declarations.
                // This should never happen!
                debug_assert!(def
                    .pattern_vars
                    .iter()
                    .all(|(symbol, _)| !exposed_but_not_defined.contains(symbol)));
            }
        }
    }

    let mut aliases = MutMap::default();

    if let GeneratedInfo::Hosted { effect_symbol, .. } = generated_info {
        // Remove this from exposed_symbols,
        // so that at the end of the process,
        // we can see if there were any
        // exposed symbols which did not have
        // corresponding defs.
        exposed_but_not_defined.remove(&effect_symbol);

        let hosted_alias = scope.lookup_alias(effect_symbol).unwrap().clone();
        aliases.insert(effect_symbol, hosted_alias);
    }

    for (symbol, alias) in output.aliases {
        // Remove this from exposed_symbols,
        // so that at the end of the process,
        // we can see if there were any
        // exposed symbols which did not have
        // corresponding defs.
        exposed_but_not_defined.remove(&symbol);

        aliases.insert(symbol, alias);
    }

    for (ability, members) in scope
        .abilities_store
        .iter_abilities()
        .filter(|(ab, _)| ab.module_id() == home)
    {
        exposed_but_not_defined.remove(&ability);
        members.iter().for_each(|member| {
            debug_assert!(member.module_id() == home);
            exposed_but_not_defined.remove(member);
        });
    }

    // By this point, all exposed symbols should have been removed from
    // exposed_symbols and added to exposed_vars_by_symbol. If any were
    // not, that means they were declared as exposed but there was
    // no actual declaration with that name!
    for symbol in exposed_but_not_defined {
        env.problem(Problem::ExposedButNotDefined(symbol));

        // In case this exposed value is referenced by other modules,
        // create a decl for it whose implementation is a runtime error.
        let mut pattern_vars = SendMap::default();
        pattern_vars.insert(symbol, var_store.fresh());

        let runtime_error = RuntimeError::ExposedButNotDefined(symbol);
        let def = Def {
            loc_pattern: Loc::at(Region::zero(), Pattern::Identifier(symbol)),
            loc_expr: Loc::at(Region::zero(), Expr::RuntimeError(runtime_error)),
            expr_var: var_store.fresh(),
            pattern_vars,
            annotation: None,
        };

        declarations.push(Declaration::Declare(def));
    }

    // Incorporate any remaining output.lookups entries into references.
    referenced_values.extend(output.references.value_lookups().copied());
    referenced_types.extend(output.references.type_lookups().copied());

    // Incorporate any remaining output.calls entries into references.
    referenced_values.extend(output.references.calls().copied());

    // Gather up all the symbols that were referenced from other modules.
    referenced_values.extend(env.qualified_value_lookups.iter().copied());
    referenced_types.extend(env.qualified_type_lookups.iter().copied());

    for declaration in declarations.iter_mut() {
        match declaration {
            Declare(def) => fix_values_captured_in_closure_def(def, &mut VecSet::default()),
            DeclareRec(defs, _) => {
                fix_values_captured_in_closure_defs(defs, &mut VecSet::default())
            }
            InvalidCycle(_) | Builtin(_) => {}
        }
    }

    ModuleOutput {
        scope,
        aliases,
        rigid_variables,
        declarations,
        referenced_values,
        referenced_types,
        exposed_imports: can_exposed_imports,
        problems: env.problems,
        symbols_from_requires,
        pending_derives,
        lookups,
    }
}

fn fix_values_captured_in_closure_def(
    def: &mut crate::def::Def,
    no_capture_symbols: &mut VecSet<Symbol>,
) {
    // patterns can contain default expressions, so much go over them too!
    fix_values_captured_in_closure_pattern(&mut def.loc_pattern.value, no_capture_symbols);

    fix_values_captured_in_closure_expr(&mut def.loc_expr.value, no_capture_symbols);
}

fn fix_values_captured_in_closure_defs(
    defs: &mut [crate::def::Def],
    no_capture_symbols: &mut VecSet<Symbol>,
) {
    // recursive defs cannot capture each other
    for def in defs.iter() {
        no_capture_symbols.extend(
            crate::traverse::symbols_introduced_from_pattern(&def.loc_pattern).map(|ls| ls.value),
        );
    }

    // TODO mutually recursive functions should both capture the union of both their capture sets

    for def in defs.iter_mut() {
        fix_values_captured_in_closure_def(def, no_capture_symbols);
    }
}

fn fix_values_captured_in_closure_pattern(
    pattern: &mut crate::pattern::Pattern,
    no_capture_symbols: &mut VecSet<Symbol>,
) {
    use crate::pattern::Pattern::*;

    match pattern {
        AppliedTag {
            arguments: loc_args,
            ..
        } => {
            for (_, loc_arg) in loc_args.iter_mut() {
                fix_values_captured_in_closure_pattern(&mut loc_arg.value, no_capture_symbols);
            }
        }
        UnwrappedOpaque { argument, .. } => {
            let (_, loc_arg) = &mut **argument;
            fix_values_captured_in_closure_pattern(&mut loc_arg.value, no_capture_symbols);
        }
        RecordDestructure { destructs, .. } => {
            for loc_destruct in destructs.iter_mut() {
                use crate::pattern::DestructType::*;
                match &mut loc_destruct.value.typ {
                    Required => {}
                    Optional(_, loc_expr) => {
                        fix_values_captured_in_closure_expr(&mut loc_expr.value, no_capture_symbols)
                    }
                    Guard(_, loc_pattern) => fix_values_captured_in_closure_pattern(
                        &mut loc_pattern.value,
                        no_capture_symbols,
                    ),
                }
            }
        }
        Identifier(_)
        | NumLiteral(..)
        | IntLiteral(..)
        | FloatLiteral(..)
        | StrLiteral(_)
        | SingleQuote(_)
        | Underscore
        | Shadowed(..)
        | MalformedPattern(_, _)
        | UnsupportedPattern(_)
        | OpaqueNotInScope(..)
        | AbilityMemberSpecialization { .. } => (),
    }
}

fn fix_values_captured_in_closure_expr(
    expr: &mut crate::expr::Expr,
    no_capture_symbols: &mut VecSet<Symbol>,
) {
    use crate::expr::Expr::*;

    match expr {
        LetNonRec(def, loc_expr) => {
            // LetNonRec(Box<Def>, Box<Located<Expr>>, Variable, Aliases),
            fix_values_captured_in_closure_def(def, no_capture_symbols);
            fix_values_captured_in_closure_expr(&mut loc_expr.value, no_capture_symbols);
        }
        LetRec(defs, loc_expr, _) => {
            // LetRec(Vec<Def>, Box<Located<Expr>>, Variable, Aliases),
            fix_values_captured_in_closure_defs(defs, no_capture_symbols);
            fix_values_captured_in_closure_expr(&mut loc_expr.value, no_capture_symbols);
        }

        Expect(condition, loc_expr) => {
            fix_values_captured_in_closure_expr(&mut condition.value, no_capture_symbols);
            fix_values_captured_in_closure_expr(&mut loc_expr.value, no_capture_symbols);
        }

        Closure(ClosureData {
            captured_symbols,
            name,
            arguments,
            loc_body,
            ..
        }) => {
            captured_symbols.retain(|(s, _)| !no_capture_symbols.contains(s));
            captured_symbols.retain(|(s, _)| s != name);

            if captured_symbols.is_empty() {
                no_capture_symbols.insert(*name);
            }

            // patterns can contain default expressions, so much go over them too!
            for (_, _, loc_pat) in arguments.iter_mut() {
                fix_values_captured_in_closure_pattern(&mut loc_pat.value, no_capture_symbols);
            }

            fix_values_captured_in_closure_expr(&mut loc_body.value, no_capture_symbols);
        }

        Num(..)
        | Int(..)
        | Float(..)
        | Str(_)
        | SingleQuote(_)
        | Var(_)
        | AbilityMember(..)
        | EmptyRecord
        | TypedHole { .. }
        | RuntimeError(_)
        | ZeroArgumentTag { .. }
        | Accessor { .. } => {}

        List { loc_elems, .. } => {
            for elem in loc_elems.iter_mut() {
                fix_values_captured_in_closure_expr(&mut elem.value, no_capture_symbols);
            }
        }

        When {
            loc_cond, branches, ..
        } => {
            fix_values_captured_in_closure_expr(&mut loc_cond.value, no_capture_symbols);

            for branch in branches.iter_mut() {
                fix_values_captured_in_closure_expr(&mut branch.value.value, no_capture_symbols);

                // patterns can contain default expressions, so much go over them too!
                for loc_pat in branch.patterns.iter_mut() {
                    fix_values_captured_in_closure_pattern(&mut loc_pat.value, no_capture_symbols);
                }

                if let Some(guard) = &mut branch.guard {
                    fix_values_captured_in_closure_expr(&mut guard.value, no_capture_symbols);
                }
            }
        }

        If {
            branches,
            final_else,
            ..
        } => {
            for (loc_cond, loc_then) in branches.iter_mut() {
                fix_values_captured_in_closure_expr(&mut loc_cond.value, no_capture_symbols);
                fix_values_captured_in_closure_expr(&mut loc_then.value, no_capture_symbols);
            }

            fix_values_captured_in_closure_expr(&mut final_else.value, no_capture_symbols);
        }

        Call(function, arguments, _) => {
            fix_values_captured_in_closure_expr(&mut function.1.value, no_capture_symbols);

            for (_, loc_arg) in arguments.iter_mut() {
                fix_values_captured_in_closure_expr(&mut loc_arg.value, no_capture_symbols);
            }
        }
        RunLowLevel { args, .. } | ForeignCall { args, .. } => {
            for (_, arg) in args.iter_mut() {
                fix_values_captured_in_closure_expr(arg, no_capture_symbols);
            }
        }

        Record { fields, .. }
        | Update {
            updates: fields, ..
        } => {
            for (_, field) in fields.iter_mut() {
                fix_values_captured_in_closure_expr(&mut field.loc_expr.value, no_capture_symbols);
            }
        }

        Access { loc_expr, .. } => {
            fix_values_captured_in_closure_expr(&mut loc_expr.value, no_capture_symbols);
        }

        Tag { arguments, .. } => {
            for (_, loc_arg) in arguments.iter_mut() {
                fix_values_captured_in_closure_expr(&mut loc_arg.value, no_capture_symbols);
            }
        }
        OpaqueRef { argument, .. } => {
            let (_, loc_arg) = &mut **argument;
            fix_values_captured_in_closure_expr(&mut loc_arg.value, no_capture_symbols);
        }
    }
}

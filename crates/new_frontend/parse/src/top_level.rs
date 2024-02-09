use arena::{Arena, Vec16, vec_struct};
use intern::InternKey;

soa!(u16 Ordered<'a> {
    ident: Vec<'a, Option<InternKey>>,
    ident_region: Vec<'a, Region>,
    node: Vec<'a, NodeId>,
    node_region: Vec<'a, Region>,
});

/// 'c - lives for canonicalization only, and is no longer needed afterwards
/// 'f - lives for frontend, will be used after canonicalization but not by the backend
/// 'b - lives for backend, will still need to be used after type checking has finished

/// SoA style, can combine these vecs later
///
/// We store these separately from everything else because canonicalization will use
/// this as the starting point for its scope. We also make sure to desugar all top-level
/// destrutures right away, so we can store everything simply and consistently like this.
pub struct TopLevel<'a> {
    // These are top-level nodes where declaration order matters, and so they are already ordered.
    // We will not sort them. This includes import, dbg, expect, crash, annotations, and non-function values.
    // ordered_ident: Vec16<'a, Option<InternKey>>,
    // ordered_ident_region: Vec16<'a, InternKey>,
    // ordered_node: Vec16<'a, NodeId>,
    // ordered_node_region: Vec16<'a, Region>,
    ordered: Ordered<'a>,

    /// These are top-level nodes where declaration order doesn't matter. We will order them later by
    /// topologically sorting them. This includes functions, type aliases, opaques, and ability defs.
    unordered_ident: Vec16<'a, Option<InternKey>>,
    unordered_ident_region: Vec16<'a, InternKey>,
    unordered_node: Vec16<'a, NodeId>,
    unordered_node_region: Vec16<'a, Region>,
    /// We need to know which unordered things reference each other, so we can topologically sort them.
    unordered_references: Vec16<'a, Vec16<'a, UnorderedTopLevelIndex>>
}

type UnorderedTopLevelIndex = u16;

/// TODO: use multiple arenas and figure out granular lifetimes for which ones can be reset at which times.
pub struct State<'a> {
    arena: Arena<'a>,

    // We intern lowercase and uppercase strings separately in order to make lookups faster.
    // There's an argument for splitting them up even further, e.g.
    // - Split uppercase into type variables, expression names, and record field names
    // - Split lowercase into type variables, expression names, and record field names
    // However, type variables are often either 1 letter (and therefore stored inline) or else
    // they are the same name as variable name (e.g. `elem`). Record field names commonly
    // overlap with variable names, especially with punning and destructuring (e.g. `{ foo }`).
    // Uppercase names tend to be less numerous,
    lowercase: Interns<'a>,
    uppercase: Interns<'a>,

    /// Top-level declarations and statements. These are stored separately from everything else because
    top_level: TopLevel<'a>,

    nodes: Nodes<'a>,

    /// We will attach expects and dbgs to this.
    last_non_fn_const_index: Option<u16>,
    // /////////////////////////// OLD ////////////////////////////////////////////////////////////////////

    // // ReversedVec because the most recently added things are the most likely to be successful lookup matches,
    // // but at the same time CPUs are faster at traversing forward though memory than backwards. So we want to
    // // be scanning forwards on these searches, yet also having the things most recently added to scope be the
    // // first results. That means we push new elems backwards from the end of the vec's capacity.
    // scope_names: ReversedVec16<'a, InternKey>,
    // scope_ids: ReversedVec16<'a, ConstId>,

    // next_const_id: ConstId,
    // /// Arena used for temporary allocations. This will be reset once we've moved to the next phase.
    // tmp: &'t mut Arena<'t>,

    // /// Arena used for recording things that last all the way until the reporting step of compilation
    // /// (e.g. interns). In batch builds, these will never be reset. In watch builds, they will only be
    // /// reset in between watch builds.
    // forever: &'r mut Arena<'r>,

    // /// Arena used for other durable allocations. Anything allocated in here will live through multiple
    // /// compiler phases.
    // ///
    // /// We can see how much of a difference this makes in the future by using the same arena for
    // /// both the temporary and durable allocators, and then removing the resets. If compile times
    // /// don't get noticeably worse, then it didn't help. But it should save page faults, which
    // /// should be relevant.
    // dur: &'d mut Arena<'d>,

    // /// Each of these that's a Pattern has a bit indicating whether the pattern continues.
    // /// So we can look at each Pattern entry and tell whether the pattern is done.
    // /// (When we encounter a token that tells us the pattern has ended, we can go back
    // /// and mutate the previous node to set the bit indicating it's the end of the Pattern.)
    // /// Similarly, each of these that's an Annotation has such a bit as well.
    // /// Using these combined, plus looking at line numbers, we can tell which annotations
    // /// were in fact immediately preceding patterns. Also I think there's a reasonable argument
    // /// for a design change here, where we just say "as long as the annotation is followed by a
    // /// matching pattern, it goes with it" and then the formatter collapses them. Yeah that
    // /// seems like a better design.
    // nodes: Vec32<'t, Node<'t>>,

    // /// As we encounter semantic strings (variables, field names, tag names, type/ability/etc names)
    // /// we intern them in here. String literals just get saved as a byte range into the source file.
    // /// These will be preserved all the way to the end of compilation, because they get used in reporting.
    // interns: Scope<'r>,

    // /// The initial scope we're going to prepare for canonicalization. Basically the goal is to populate
    // /// with top-level defs once we've
    // scope: Scope<'d>,

    // /// Which top-level defs reference each other. We need this info in order to reorder them.
    // /// Note that this has to be a growable vec; we may need to resize it as we encounter more
    // /// top-level defs. And since it's n^2 bits, we have to be careful not to preallocate too
    // /// much capacity.
    // top_level_references: Vec32<'t, bool>,

    // /// The index in nodes where each top-level def begins. In conjunction
    // /// with top_level_references, we can change the order of this vec and then canonicalization
    // /// can traverse this vec in order to canonicalize the top-level defs in the correct order.
    // top_level_def_offsets: Vec16<'d, u32>,
}

struct Region {
    line: u16,
    col: u16,
}

impl<'a> TopLevel<'a> {
    pub fn add_fn_pattern(&mut self, opt_ann: Option<Ann>) {
        //
    }

    pub fn add_non_fn_pattern(&mut self, opt_ann: Option<Ann>) {
        //
    }

    pub fn add_expect(&mut self) {}
}

// TODO trim these down
#[allow(clippy::too_many_arguments)]
pub fn canonicalize_module_defs<'a>(
    arena: &'a Bump,
    loc_defs: &'a mut Defs<'a>,
    header_type: &roc_parse::header::HeaderType,
    home: ModuleId,
    module_path: &str,
    src: &'a str,
    module_ids: &'a ModuleIds,
    exposed_ident_ids: IdentIds,
    dep_idents: &'a IdentIdsByModule,
    aliases: MutMap<Symbol, Alias>,
    imported_abilities_state: PendingAbilitiesStore,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    exposed_symbols: VecSet<Symbol>,
    symbols_from_requires: &[(Loc<Symbol>, Loc<TypeAnnotation<'a>>)],
    var_store: &mut VarStore,
) -> ModuleOutput {
    let mut can_exposed_imports = MutMap::default();
    let mut scope = Scope::new(home, exposed_ident_ids, imported_abilities_state);
    let mut env = Env::new(arena, home, dep_idents, module_ids);

    let mut scope = Scope::new(top_level);

    // TODO: do we actually want to iterate through all the unordered defs to filter out just the aliases?
    // Or do we really want to just have all the unordered defs become in-scope?
    for (name, alias) in top_level.iter_aliases() {
        scope.add_alias(
            name,
            alias.region,
            alias.type_variables,
            alias.infer_ext_in_output_variables,
            alias.typ,
            alias.kind,
        );
    }

    // TODO handle generated info (or the equivalent in effect interpreters world)
    // let generated_info =
    //     GeneratedInfo::from_header_type(&mut env, &mut scope, var_store, header_type);

    let mut rigid_variables = RigidVariables::default();


    let (defs, output, symbols_introduced) = canonicalize_defs(
        &mut env,
        Output::default(),
        var_store,
        &mut scope,
        loc_defs,
        PatternType::TopLevelDef,
    );

    let pending_derives = output.pending_derives;

    // See if any of the new idents we defined went unused.
    // If any were unused and also not exposed, report it.
    //
    // We'll catch symbols that are only referenced due to (mutual) recursion later,
    // when sorting the defs.
    for (symbol, region) in symbols_introduced {
        if !output.references.has_type_or_value_lookup(symbol)
            && !exposed_symbols.contains(&symbol)
            && !scope.abilities_store.is_specialization_name(symbol)
            && !symbol.is_exposed_for_builtin_derivers()
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
            .insert(able.variable, (able.name, able.abilities));
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

    let (mut declarations, mut output) = crate::def::sort_can_defs_new(
        &mut env,
        &mut scope,
        var_store,
        defs,
        new_output,
        &exposed_symbols,
    );

    debug_assert!(
        output.pending_derives.is_empty(),
        "I thought pending derives are only found during def introduction"
    );

    let symbols_from_requires = symbols_from_requires
        .iter()
        .map(|(symbol, loc_ann)| {
            // We've already canonicalized the module, so there are no pending abilities.
            let pending_abilities_in_scope = &Default::default();

            let ann = canonicalize_annotation(
                &mut env,
                &mut scope,
                &loc_ann.value,
                loc_ann.region,
                var_store,
                pending_abilities_in_scope,
                AnnotationFor::Value,
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

    for index in 0..declarations.len() {
        use crate::expr::DeclarationTag::*;

        let tag = declarations.declarations[index];

        match tag {
            Value => {
                let symbol = &declarations.symbols[index].value;

                // Remove this from exposed_symbols,
                // so that at the end of the process,
                // we can see if there were any
                // exposed symbols which did not have
                // corresponding defs.
                exposed_but_not_defined.remove(symbol);

                // Temporary hack: we don't know exactly what symbols are hosted symbols,
                // and which are meant to be normal definitions without a body. So for now
                // we just assume they are hosted functions (meant to be provided by the platform)
                if has_no_implementation(&declarations.expressions[index].value) {
                    match generated_info {
                        GeneratedInfo::Builtin => {
                            match crate::builtins::builtin_defs_map(*symbol, var_store) {
                                None => {
                                    internal_error!("A builtin module contains a signature without implementation for {:?}", symbol)
                                }
                                Some(replacement_def) => {
                                    declarations.update_builtin_def(index, replacement_def);
                                }
                            }
                        }
                        GeneratedInfo::Hosted { effect_symbol, .. } => {
                            let ident_id = symbol.ident_id();
                            let ident = scope
                                .locals
                                .ident_ids
                                .get_name(ident_id)
                                .unwrap()
                                .to_string();

                            let def_annotation = declarations.annotations[index].clone().unwrap();

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

                            declarations.update_builtin_def(index, hosted_def);
                        }
                        _ => (),
                    }
                }
            }
            Function(_) | Recursive(_) | TailRecursive(_) => {
                let symbol = &declarations.symbols[index].value;

                // Remove this from exposed_symbols,
                // so that at the end of the process,
                // we can see if there were any
                // exposed symbols which did not have
                // corresponding defs.
                exposed_but_not_defined.remove(symbol);

                // Temporary hack: we don't know exactly what symbols are hosted symbols,
                // and which are meant to be normal definitions without a body. So for now
                // we just assume they are hosted functions (meant to be provided by the platform)
                if has_no_implementation(&declarations.expressions[index].value) {
                    match generated_info {
                        GeneratedInfo::Builtin => {
                            match crate::builtins::builtin_defs_map(*symbol, var_store) {
                                None => {
                                    internal_error!("A builtin module contains a signature without implementation for {:?}", symbol)
                                }
                                Some(replacement_def) => {
                                    declarations.update_builtin_def(index, replacement_def);
                                }
                            }
                        }
                        GeneratedInfo::Hosted { effect_symbol, .. } => {
                            let ident_id = symbol.ident_id();
                            let ident = scope
                                .locals
                                .ident_ids
                                .get_name(ident_id)
                                .unwrap()
                                .to_string();

                            let def_annotation = declarations.annotations[index].clone().unwrap();

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

                            declarations.update_builtin_def(index, hosted_def);
                        }
                        _ => (),
                    }
                }
            }
            Destructure(d_index) => {
                let destruct_def = &declarations.destructs[d_index.index()];

                for (symbol, _) in BindingsFromPattern::new(&destruct_def.loc_pattern) {
                    exposed_but_not_defined.remove(&symbol);
                }
            }
            MutualRecursion { .. } => {
                // the declarations of this group will be treaded individually by later iterations
            }
            Expectation => { /* ignore */ }
            ExpectationFx => { /* ignore */ }
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

        declarations.push_def(def);
    }

    // Incorporate any remaining output.lookups entries into references.
    referenced_values.extend(output.references.value_lookups().copied());
    referenced_types.extend(output.references.type_lookups().copied());

    // Incorporate any remaining output.calls entries into references.
    referenced_values.extend(output.references.calls().copied());

    // Gather up all the symbols that were referenced from other modules.
    referenced_values.extend(env.qualified_value_lookups.iter().copied());
    referenced_types.extend(env.qualified_type_lookups.iter().copied());

    let mut fix_closures_no_capture_symbols = VecSet::default();
    let mut fix_closures_closure_captures = VecMap::default();
    for index in 0..declarations.len() {
        use crate::expr::DeclarationTag::*;

        // For each declaration, we need to fixup the closures inside its def.
        // Reuse the fixup buffer allocations from the previous iteration.
        fix_closures_no_capture_symbols.clear();
        fix_closures_closure_captures.clear();

        match declarations.declarations[index] {
            Value => {
                // def pattern has no default expressions, so skip
                let loc_expr = &mut declarations.expressions[index];

                fix_values_captured_in_closure_expr(
                    &mut loc_expr.value,
                    &mut fix_closures_no_capture_symbols,
                    &mut fix_closures_closure_captures,
                );
            }
            Function(f_index) | Recursive(f_index) | TailRecursive(f_index) => {
                let name = declarations.symbols[index].value;
                let function_def = &mut declarations.function_bodies[f_index.index()].value;
                let loc_expr = &mut declarations.expressions[index];

                function_def.captured_symbols.retain(|(s, _)| *s != name);

                let mut no_capture_symbols = VecSet::default();
                if function_def.captured_symbols.is_empty() {
                    no_capture_symbols.insert(name);
                }

                // patterns can contain default expressions, so must go over them too!
                for (_, _, loc_pat) in function_def.arguments.iter_mut() {
                    fix_values_captured_in_closure_pattern(
                        &mut loc_pat.value,
                        &mut fix_closures_no_capture_symbols,
                        &mut fix_closures_closure_captures,
                    );
                }

                fix_values_captured_in_closure_expr(
                    &mut loc_expr.value,
                    &mut fix_closures_no_capture_symbols,
                    &mut fix_closures_closure_captures,
                );
            }
            Destructure(d_index) => {
                let destruct_def = &mut declarations.destructs[d_index.index()];
                let loc_pat = &mut destruct_def.loc_pattern;
                let loc_expr = &mut declarations.expressions[index];

                fix_values_captured_in_closure_pattern(
                    &mut loc_pat.value,
                    &mut fix_closures_no_capture_symbols,
                    &mut fix_closures_closure_captures,
                );
                fix_values_captured_in_closure_expr(
                    &mut loc_expr.value,
                    &mut fix_closures_no_capture_symbols,
                    &mut fix_closures_closure_captures,
                );
            }
            MutualRecursion { .. } => {
                // the declarations of this group will be treaded individually by later iterations
            }
            Expectation => {
                let loc_expr = &mut declarations.expressions[index];
                fix_values_captured_in_closure_expr(
                    &mut loc_expr.value,
                    &mut fix_closures_no_capture_symbols,
                    &mut fix_closures_closure_captures,
                );
            }
            ExpectationFx => {
                let loc_expr = &mut declarations.expressions[index];
                fix_values_captured_in_closure_expr(
                    &mut loc_expr.value,
                    &mut fix_closures_no_capture_symbols,
                    &mut fix_closures_closure_captures,
                );
            }
        }
    }

    let collected = declarations.expects();

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
        loc_expects: collected.expects,
        loc_dbgs: collected.dbgs,
        exposed_symbols,
    }
}

fn fix_values_captured_in_closure_def(
    def: &mut crate::def::Def,
    no_capture_symbols: &mut VecSet<Symbol>,
    closure_captures: &mut VecMap<Symbol, Vec<(Symbol, Variable)>>,
) {
    // patterns can contain default expressions, so much go over them too!
    fix_values_captured_in_closure_pattern(
        &mut def.loc_pattern.value,
        no_capture_symbols,
        closure_captures,
    );

    fix_values_captured_in_closure_expr(
        &mut def.loc_expr.value,
        no_capture_symbols,
        closure_captures,
    );
}

fn fix_values_captured_in_closure_defs(
    defs: &mut [crate::def::Def],
    no_capture_symbols: &mut VecSet<Symbol>,
    closure_captures: &mut VecMap<Symbol, Vec<(Symbol, Variable)>>,
) {
    // Mutually recursive functions should both capture the union of all their capture sets
    //
    // Really unfortunate we make a lot of clones here, can this be done more efficiently?
    let mut total_capture_set = VecMap::default();
    for def in defs.iter_mut() {
        if let Expr::Closure(ClosureData {
            captured_symbols, ..
        }) = &def.loc_expr.value
        {
            total_capture_set.extend(captured_symbols.iter().copied());
        }
    }
    for def in defs.iter() {
        for symbol in
            crate::traverse::symbols_introduced_from_pattern(&def.loc_pattern).map(|ls| ls.value)
        {
            total_capture_set.remove(&symbol);
        }
    }

    let mut total_capture_set: Vec<_> = total_capture_set.into_iter().collect();
    total_capture_set.sort_by_key(|(sym, _)| *sym);
    for def in defs.iter_mut() {
        if let Expr::Closure(ClosureData {
            captured_symbols, ..
        }) = &mut def.loc_expr.value
        {
            *captured_symbols = total_capture_set.clone();
        }
    }

    for def in defs.iter_mut() {
        fix_values_captured_in_closure_def(def, no_capture_symbols, closure_captures);
    }
}

fn fix_values_captured_in_closure_pattern(
    pattern: &mut crate::pattern::Pattern,
    no_capture_symbols: &mut VecSet<Symbol>,
    closure_captures: &mut VecMap<Symbol, Vec<(Symbol, Variable)>>,
) {
    use crate::pattern::Pattern::*;

    match pattern {
        AppliedTag {
            arguments: loc_args,
            ..
        } => {
            for (_, loc_arg) in loc_args.iter_mut() {
                fix_values_captured_in_closure_pattern(
                    &mut loc_arg.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }
        UnwrappedOpaque { argument, .. } => {
            let (_, loc_arg) = &mut **argument;
            fix_values_captured_in_closure_pattern(
                &mut loc_arg.value,
                no_capture_symbols,
                closure_captures,
            );
        }
        RecordDestructure { destructs, .. } => {
            for loc_destruct in destructs.iter_mut() {
                use crate::pattern::DestructType::*;
                match &mut loc_destruct.value.typ {
                    Required => {}
                    Optional(_, loc_expr) => fix_values_captured_in_closure_expr(
                        &mut loc_expr.value,
                        no_capture_symbols,
                        closure_captures,
                    ),
                    Guard(_, loc_pattern) => fix_values_captured_in_closure_pattern(
                        &mut loc_pattern.value,
                        no_capture_symbols,
                        closure_captures,
                    ),
                }
            }
        }
        TupleDestructure { destructs, .. } => {
            for loc_destruct in destructs.iter_mut() {
                fix_values_captured_in_closure_pattern(
                    &mut loc_destruct.value.typ.1.value,
                    no_capture_symbols,
                    closure_captures,
                )
            }
        }
        List { patterns, .. } => {
            for loc_pat in patterns.patterns.iter_mut() {
                fix_values_captured_in_closure_pattern(
                    &mut loc_pat.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }
        As(subpattern, _) => {
            fix_values_captured_in_closure_pattern(
                &mut subpattern.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Identifier(_)
        | NumLiteral(..)
        | IntLiteral(..)
        | FloatLiteral(..)
        | StrLiteral(_)
        | SingleQuote(..)
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
    closure_captures: &mut VecMap<Symbol, Vec<(Symbol, Variable)>>,
) {
    use crate::expr::Expr::*;

    match expr {
        LetNonRec(def, loc_expr) => {
            // LetNonRec(Box<Def>, Box<Located<Expr>>, Variable, Aliases),
            fix_values_captured_in_closure_def(def, no_capture_symbols, closure_captures);
            fix_values_captured_in_closure_expr(
                &mut loc_expr.value,
                no_capture_symbols,
                closure_captures,
            );
        }
        LetRec(defs, loc_expr, _) => {
            // LetRec(Vec<Def>, Box<Located<Expr>>, Variable, Aliases),
            fix_values_captured_in_closure_defs(defs, no_capture_symbols, closure_captures);
            fix_values_captured_in_closure_expr(
                &mut loc_expr.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Expect {
            loc_condition,
            loc_continuation,
            ..
        }
        | ExpectFx {
            loc_condition,
            loc_continuation,
            ..
        }
        | Dbg {
            loc_message: loc_condition,
            loc_continuation,
            ..
        } => {
            fix_values_captured_in_closure_expr(
                &mut loc_condition.value,
                no_capture_symbols,
                closure_captures,
            );
            fix_values_captured_in_closure_expr(
                &mut loc_continuation.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Crash { msg, ret_var: _ } => {
            fix_values_captured_in_closure_expr(
                &mut msg.value,
                no_capture_symbols,
                closure_captures,
            );
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

            let original_captures_len = captured_symbols.len();
            let mut i = 0;
            let mut added_captures = false;
            while i < original_captures_len {
                // If we've captured a capturing closure, replace the captured closure symbol with
                // the symbols of its captures. That way, we can construct the closure with the
                // captures it needs inside our body.
                //
                // E.g.
                //   x = ""
                //   inner = \{} -> x
                //   outer = \{} -> inner {}
                //
                // initially `outer` captures [inner], but this is then replaced with just [x].
                let (captured_symbol, _) = captured_symbols[i];
                if let Some(captures) = closure_captures.get(&captured_symbol) {
                    debug_assert!(!captures.is_empty());
                    captured_symbols.extend(captures);
                    captured_symbols.swap_remove(i);
                    // Jump two, because the next element is now one of the newly-added captures,
                    // which we don't need to check.
                    i += 2;

                    added_captures = true;
                } else {
                    i += 1;
                }
            }
            if added_captures {
                // Re-sort, since we've added new captures.
                captured_symbols.sort_by_key(|(sym, _)| *sym);
                captured_symbols.dedup_by_key(|(sym, _)| *sym);
            }

            if captured_symbols.is_empty() {
                no_capture_symbols.insert(*name);
            } else {
                closure_captures.insert(*name, captured_symbols.to_vec());
            }

            // patterns can contain default expressions, so much go over them too!
            for (_, _, loc_pat) in arguments.iter_mut() {
                fix_values_captured_in_closure_pattern(
                    &mut loc_pat.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }

            fix_values_captured_in_closure_expr(
                &mut loc_body.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Num(..)
        | Int(..)
        | Float(..)
        | Str(_)
        | SingleQuote(..)
        | IngestedFile(..)
        | Var(..)
        | AbilityMember(..)
        | EmptyRecord
        | TypedHole { .. }
        | RuntimeError(_)
        | ZeroArgumentTag { .. }
        | RecordAccessor { .. } => {}

        List { loc_elems, .. } => {
            for elem in loc_elems.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut elem.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }

        When {
            loc_cond, branches, ..
        } => {
            fix_values_captured_in_closure_expr(
                &mut loc_cond.value,
                no_capture_symbols,
                closure_captures,
            );

            for branch in branches.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut branch.value.value,
                    no_capture_symbols,
                    closure_captures,
                );

                // patterns can contain default expressions, so much go over them too!
                for loc_pat in branch.patterns.iter_mut() {
                    fix_values_captured_in_closure_pattern(
                        &mut loc_pat.pattern.value,
                        no_capture_symbols,
                        closure_captures,
                    );
                }

                if let Some(guard) = &mut branch.guard {
                    fix_values_captured_in_closure_expr(
                        &mut guard.value,
                        no_capture_symbols,
                        closure_captures,
                    );
                }
            }
        }

        If {
            branches,
            final_else,
            ..
        } => {
            for (loc_cond, loc_then) in branches.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut loc_cond.value,
                    no_capture_symbols,
                    closure_captures,
                );
                fix_values_captured_in_closure_expr(
                    &mut loc_then.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }

            fix_values_captured_in_closure_expr(
                &mut final_else.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Call(function, arguments, _) => {
            fix_values_captured_in_closure_expr(
                &mut function.1.value,
                no_capture_symbols,
                closure_captures,
            );

            for (_, loc_arg) in arguments.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut loc_arg.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }
        RunLowLevel { args, .. } | ForeignCall { args, .. } => {
            for (_, arg) in args.iter_mut() {
                fix_values_captured_in_closure_expr(arg, no_capture_symbols, closure_captures);
            }
        }

        Record { fields, .. }
        | RecordUpdate {
            updates: fields, ..
        } => {
            for (_, field) in fields.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut field.loc_expr.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }

        Tuple { elems, .. } => {
            for (_var, expr) in elems.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut expr.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }

        RecordAccess { loc_expr, .. } | TupleAccess { loc_expr, .. } => {
            fix_values_captured_in_closure_expr(
                &mut loc_expr.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Tag { arguments, .. } => {
            for (_, loc_arg) in arguments.iter_mut() {
                fix_values_captured_in_closure_expr(
                    &mut loc_arg.value,
                    no_capture_symbols,
                    closure_captures,
                );
            }
        }
        OpaqueRef { argument, .. } => {
            let (_, loc_arg) = &mut **argument;
            fix_values_captured_in_closure_expr(
                &mut loc_arg.value,
                no_capture_symbols,
                closure_captures,
            );
        }
        OpaqueWrapFunction(_) => {}
    }
}

/// Type state for a single module.
#[derive(Debug)]
pub struct TypeState {
    pub subs: Subs,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub abilities: AbilitiesStore,
    pub solved_implementations: ResolvedImplementations,
}

impl TypeState {
    pub fn serialize(&self, writer: &mut impl std::io::Write) -> std::io::Result<usize> {
        let Self {
            subs,
            exposed_vars_by_symbol,
            abilities,
            solved_implementations,
        } = self;

        let written_subs = subs.serialize(exposed_vars_by_symbol, writer)?;
        let written_ab = abilities.serialize(writer)?;
        let written_solved_impls =
            crate::abilities::serialize_solved_implementations(solved_implementations, writer)?;

        Ok(written_subs + written_ab + written_solved_impls)
    }

    pub fn deserialize(bytes: &[u8]) -> (Self, usize) {
        let ((subs, exposed_vars_by_symbol), len_subs) = Subs::deserialize(bytes);
        let bytes = &bytes[len_subs..];

        let (abilities, len_abilities) = AbilitiesStore::deserialize(bytes);
        let bytes = &bytes[len_abilities..];

        let (solved_implementations, len_solved_impls) =
            crate::abilities::deserialize_solved_implementations(bytes);

        let total_offset = len_subs + len_abilities + len_solved_impls;

        (
            Self {
                subs,
                exposed_vars_by_symbol: exposed_vars_by_symbol.to_vec(),
                abilities,
                solved_implementations,
            },
            total_offset,
        )
    }
}

/// The types of all exposed values/functions of a collection of modules
#[derive(Clone, Debug, Default)]
pub struct ExposedByModule {
    exposed: MutMap<ModuleId, ExposedModuleTypes>,
}

impl ExposedByModule {
    pub fn insert(&mut self, module_id: ModuleId, exposed: ExposedModuleTypes) {
        self.exposed.insert(module_id, exposed);
    }

    pub fn get(&self, module_id: &ModuleId) -> Option<&ExposedModuleTypes> {
        self.exposed.get(module_id)
    }

    /// Convenient when you need mutable access to the StorageSubs in the ExposedModuleTypes
    pub fn get_mut(&mut self, module_id: &ModuleId) -> Option<&mut ExposedModuleTypes> {
        self.exposed.get_mut(module_id)
    }

    /// Create a clone of `self` that has just a subset of the modules
    ///
    /// Useful when we know what modules a particular module imports, and want just
    /// the exposed types for those exposed modules.
    pub fn retain_modules<'a>(&self, it: impl Iterator<Item = &'a ModuleId>) -> Self {
        let mut output = Self::default();

        for module_id in it {
            match self.exposed.get(module_id) {
                None => {
                    internal_error!("Module {:?} did not register its exposed values", module_id)
                }
                Some(exposed_types) => {
                    output.exposed.insert(*module_id, exposed_types.clone());
                }
            }
        }

        output
    }

    pub fn iter_all(&self) -> impl Iterator<Item = (&ModuleId, &ExposedModuleTypes)> {
        self.exposed.iter()
    }

    /// # Safety
    ///
    /// May only be called when the exposed types of a modules are no longer needed, or may be
    /// transitioned into another context.
    pub unsafe fn remove(&mut self, module_id: &ModuleId) -> Option<ExposedModuleTypes> {
        self.exposed.remove(module_id)
    }
}

#[derive(Clone, Debug, Default)]
pub struct ExposedForModule {
    pub exposed_by_module: ExposedByModule,
    pub imported_values: Vec<Symbol>,
}

impl ExposedForModule {
    pub fn new<'a>(
        it: impl Iterator<Item = &'a Symbol>,
        exposed_by_module: ExposedByModule,
    ) -> Self {
        let mut imported_values = Vec::new();

        for symbol in it {
            let module = exposed_by_module.exposed.get(&symbol.module_id());
            if let Some(ExposedModuleTypes { .. }) = module {
                imported_values.push(*symbol);
            } else {
                continue;
            }
        }

        Self {
            imported_values,
            exposed_by_module,
        }
    }
}

/// During type solving and monomorphization, a module must know how its imported ability
/// implementations are resolved - are they derived, or have a concrete implementation?
///
/// Unfortunately we cannot keep this information opaque, as it's important for properly
/// restoring specialization lambda sets. As such, we need to export implementation information,
/// which is the job of this structure.
pub type ResolvedImplementations = VecMap<ImplKey, ResolvedImpl>;

/// The types of all exposed values/functions of a module. This includes ability member
/// specializations.
#[derive(Clone, Debug)]
pub struct ExposedModuleTypes {
    pub exposed_types_storage_subs: ExposedTypesStorageSubs,
    pub resolved_implementations: ResolvedImplementations,
}

#[derive(Debug)]
pub struct Module {
    pub module_id: ModuleId,
    pub exposed_imports: MutMap<Symbol, Region>,
    pub exposed_symbols: VecSet<Symbol>,
    pub referenced_values: VecSet<Symbol>,
    pub referenced_types: VecSet<Symbol>,
    /// all aliases. `bool` indicates whether it is exposed
    pub aliases: MutMap<Symbol, (bool, Alias)>,
    pub rigid_variables: RigidVariables,
    pub abilities_store: PendingAbilitiesStore,
    pub loc_expects: VecMap<Region, Vec<ExpectLookup>>,
    pub loc_dbgs: VecMap<Symbol, DbgLookup>,
}

#[derive(Debug, Default)]
pub struct RigidVariables {
    pub named: MutMap<Variable, Lowercase>,
    pub able: MutMap<Variable, (Lowercase, AbilitySet)>,
    pub wildcards: VecSet<Variable>,
}

#[derive(Debug)]
pub struct ModuleOutput {
    pub aliases: MutMap<Symbol, Alias>,
    pub rigid_variables: RigidVariables,
    pub declarations: Declarations,
    pub exposed_imports: MutMap<Symbol, Region>,
    pub exposed_symbols: VecSet<Symbol>,
    pub problems: Vec<Problem>,
    pub referenced_values: VecSet<Symbol>,
    pub referenced_types: VecSet<Symbol>,
    pub symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    pub pending_derives: PendingDerives,
    pub scope: Scope,
    pub loc_expects: VecMap<Region, Vec<ExpectLookup>>,
    pub loc_dbgs: VecMap<Symbol, DbgLookup>,
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
    fn from_header_type(
        env: &mut Env,
        scope: &mut Scope,
        var_store: &mut VarStore,
        header_type: &HeaderType,
    ) -> Self {
        match header_type {
            HeaderType::Hosted {
                generates,
                generates_with,
                name: _,
                exposes: _,
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
                        vec![],
                        actual,
                        AliasKind::Opaque,
                    );
                }

                GeneratedInfo::Hosted {
                    effect_symbol,
                    generated_functions,
                }
            }
            HeaderType::Builtin {
                generates_with,
                name: _,
                exposes: _,
            } => {
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

    // In each of these, the index into the array will become its ConstId in canonicalization.
    // Each top-level def includes a name, whether it's a function, and its region.
    //
    // Note that when we encounter a destructure (e.g., at the top level, `{ foo, bar } = ...`),
    // we immediately desugar it all the way into named patterns (and wrapping the given expression
    // in .foo and .bar etc). We might need to register a fake name ahead of time (e.g. an InternKey::NONE or something)
    // to store the intermediate value, e.g. `foo = ... ` and then `bar = foo.bar`, `baz = foo.baz`
    // after desugaring. (We record those nodes as desugaring for purposes of error messages later.)
    //
    // This in turn means that although top-level desugaring needs to happen immediately, we don't actually
    // need special tracking in the state for that scenario. Just names, regions, ids, and function bit is sufficient.
    //
    // Also note that once these get handed off to the actual canonicalization process, these will just be the first
    // entries into the vecs.
    //
    ///
    /// These are either a type annotation, a body, or an `import` statement. Order matters for all of these!
    /// No matter what, these are only identifier patterns, where the ident is lowercase.
    ///
    /// `import` statements for values (as opposed to modules and types, which are handled elsewhere)
    /// get parsed into these. Their `pattern` is the identifier of the value being imported, and
    /// their `node` refers to the `import` keyword (as does its region). The node is used during
    /// canonicalization, so it can know this is an import. The node_region is not used for imports.
    ///
    /// The reason pattern_name has an Option<InternKey> is the strategy we use for top-level
    /// `expect` (and maybe in the future `dbg`). When we encounter these, we wrap them around
    /// the most recent def with an actual name (e.g. `foo =`) that isn't a function. We attach
    /// them to non-functions because functions can be reordered, but these shouldn't be. Since
    /// `expect`s and such can appear in the source code before the first named def, the first
    /// of these entries might end up with a pattern_name of None, because it's just a stack of
    /// `expect`s and such.

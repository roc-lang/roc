/// Canonicalize the top level of a module.
fn canonicalize_tl<'a, LcStrId, UcStrId, ModuleStrId, Region>(
    arena: &'a Bump,
    // This should have been initialized during parsing and populated with top-level identifiers, type alias defs, etc.
    tl_scope: TopLevelScope<'a, LcStrId, UcStrId, ModuleStrId, Region>,
    header_type: &roc_parse::header::HeaderType,
) {
    use roc_parse::header::HeaderType;

    let mut env = Env::new(arena, tl_scope);

    // TODO desugaring should have happened before we call canonicalize!
    // crate::desugar::desugar_defs_node_values(arena, loc_defs, src, &mut None, module_path, true);

    let generated_info = env.canonicalize_module_header(header_type);

    // TODO canonicalize all the top-level defs here.

    // TODO use the iterator returned by canonicalize_module_header to verify whether all the
    // exposed idents were actually defined in the tl. (Treat them as lookups, basically.)

    problems.extend(env.never_referenced());
}

struct Env<'a, LcStrId, UcStrId, ShorthandStrId, ModuleStrId, Region> {
    arena: &'a Bump,
    tl_scope: TopLevelScope<'a>,
    problems: Vec<'a, Problem>,
    references: (),
}

impl<'a> Env<'a> {
    fn new(arena: &'a Bump, tl_scope: TopLevelScope<'a>) -> Self {
        Env {
            arena,
            tl_scope,
            problems: Vec::new_in(arena),
            references: (),
        }
    }

    /// End the module. This does a few things:
    /// * Report any unused defs
    /// * Report any qualified lookups of bindings that weren't exposed, or imports of modules that weren't exposed
    /// * Reset tl scope to just the builtins, so this can be recycled for another module without copying those in again.
    pub fn end_module<
        LcIsExposed: Fn(LowercaseId) -> bool,
        UcIsExposed: Fn(UppercaseId) -> bool,
        ModuleIsExposed: Fn(ModuleId) -> bool,
    >() {
        // TODO: figure out how to report unused record fields and tag union variants
        // TODO: Report any unused bindings or imports
        // TODO: If anything is exposed from the module but not bound in the tl, report that.
        // TODO: Report any qualified lookups of bindings that weren't exposed, or imports of modules that weren't exposed
        // TODO: Reset tl scope to just the builtins, so this can be recycled for another module without copying those in again.

        /*
        // TODO figure out where all this goes; it was previously in can::module.rs
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

        // Gather up all the symbols that were referenced across all the defs' lookups.
        referenced_values.extend(output.references.value_lookups().copied());

        // Gather up all the symbols that were referenced across all the defs' calls.
        referenced_values.extend(output.references.calls().copied());

        // Gather up all the symbols that were referenced from other modules.
        referenced_values.extend(env.qualified_value_lookups.iter().copied());

        // NOTE previously we inserted builtin defs into the list of defs here
        // this is now done later, in file.rs.

        // In module.rs, this happened after we canonicalized all the defs.
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

        // In module.rs, this was the very last thing that happened.
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

        // These were used to return:
        // loc_expects: collected.expects,
        // loc_dbgs: collected.dbgs,
        let collected = declarations.expects();
        */
    }

    fn canonicalize_module_header(
        &self,
        header_type: &roc_parse::header::HeaderType,
    ) -> GeneratedInfo {
        // TODO return all the generated info for platform Tasks in `hosted`.

        // TODO need to return an iterator over exposed idents, so that after we finish
        // all the top-level defs, we can iterate through it and verify whether the idents
        // we're exposing in the header are actually defined in the top level!
        match header_type {
            HeaderType::App { provides, .. } => {
                // TODO add lookups for `provides`

                GeneratedInfo::NotSpecial
            }
            HeaderType::Hosted {
                exposes,
                generates,
                generates_with,
                ..
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

                // TODO Add uc and lc lookups for `exposes`

                GeneratedInfo::Hosted {
                    effect_symbol,
                    generated_functions,
                }
            }
            HeaderType::Package { exposes, .. } => {
                // TODO Add module lookups for `exposes`

                GeneratedInfo::NotSpecial
            }
            HeaderType::Platform {
                requires, exposes, ..
            } => {
                // TODO canonicalize `requires` by binding it into scope and then resolving its annotation.
                // TODO Add module lookups for `exposes`

                GeneratedInfo::NotSpecial
            }
            HeaderType::Builtin { exposes, .. } => {
                // TODO Add uc and lc lookups for `exposes`

                GeneratedInfo::Builtin
            }
            HeaderType::Module { exposes, .. } => {
                // TODO Add uc and lc lookups for `exposes`

                GeneratedInfo::NotSpecial
            }
        }
    }

    // This reports things that are unused because they are never referenced.
    // This is not *quite* the same as unused, because it misses the case where
    // things only refer to each other due to mutual recursion. We handle those elsewhere,
    // when sorting defs.
    pub fn never_referenced(&self) -> impl Iterator<Item = Problem> {
        let mut problems = Vec::new_in(self.arena);

        for (str_id, region) in self.tl_scope.unused_imports() {
            // TODO push a problem
        }

        for (str_id, region) in self.tl_scope.unused_lc() {
            // TODO push a problem
        }

        for (str_id, region) in self.tl_scope.unused_uc() {
            // TODO push a problem
        }
    }
}

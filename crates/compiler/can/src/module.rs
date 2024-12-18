use std::path::Path;

use crate::abilities::{AbilitiesStore, ImplKey, PendingAbilitiesStore, ResolvedImpl};
use crate::annotation::{canonicalize_annotation, AnnotationFor};
use crate::def::{canonicalize_defs, report_unused_imports, Def, DefKind};
use crate::desugar::desugar_record_destructures;
use crate::env::{Env, FxMode};
use crate::expr::{ClosureData, Declarations, ExpectLookup, Expr, Output, PendingDerives};
use crate::pattern::{
    canonicalize_record_destructs, BindingsFromPattern, Pattern, PermitShadows, RecordDestruct,
};
use crate::procedure::References;
use crate::scope::Scope;
use bumpalo::Bump;
use roc_collections::{MutMap, SendMap, VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::ident::Ident;
use roc_module::ident::Lowercase;
use roc_module::symbol::{IdentId, IdentIds, IdentIdsByModule, ModuleId, PackageModuleIds, Symbol};
use roc_parse::ast::{Defs, TypeAnnotation};
use roc_parse::header::HeaderType;
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::{ExposedTypesStorageSubs, Subs, VarStore, Variable};
use roc_types::types::{AbilitySet, Alias, Type};

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
    /// all aliases. `bool` indicates whether it is exposed
    pub aliases: MutMap<Symbol, (bool, Alias)>,
    pub rigid_variables: RigidVariables,
    pub abilities_store: PendingAbilitiesStore,
    pub loc_expects: VecMap<Region, Vec<ExpectLookup>>,
    pub has_dbgs: bool,
    pub module_params: Option<ModuleParams>,
}

#[derive(Debug, Clone)]
pub struct ModuleParams {
    pub region: Region,
    pub whole_symbol: Symbol,
    pub whole_var: Variable,
    pub record_var: Variable,
    pub record_ext_var: Variable,
    pub destructs: Vec<Loc<RecordDestruct>>,
    // used while lowering passed functions
    pub arity_by_name: VecMap<IdentId, usize>,
}

impl ModuleParams {
    pub fn pattern(&self) -> Loc<Pattern> {
        let record_pattern = Pattern::RecordDestructure {
            whole_var: self.record_var,
            ext_var: self.record_ext_var,
            destructs: self.destructs.clone(),
        };
        let loc_record_pattern = Loc::at(self.region, record_pattern);

        let as_pattern = Pattern::As(Box::new(loc_record_pattern), self.whole_symbol);
        Loc::at(self.region, as_pattern)
    }
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
    pub module_params: Option<ModuleParams>,
    pub declarations: Declarations,
    pub exposed_imports: MutMap<Symbol, Region>,
    pub exposed_symbols: VecSet<Symbol>,
    pub problems: Vec<Problem>,
    pub referenced_values: VecSet<Symbol>,
    pub symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    pub pending_derives: PendingDerives,
    pub scope: Scope,
    pub loc_expects: VecMap<Region, Vec<ExpectLookup>>,
    pub has_dbgs: bool,
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
    loc_defs: &'a mut Defs<'a>,
    header_type: &'a roc_parse::header::HeaderType,
    home: ModuleId,
    module_path: &'a str,
    src: &'a str,
    qualified_module_ids: &'a PackageModuleIds<'a>,
    exposed_ident_ids: IdentIds,
    dep_idents: &'a IdentIdsByModule,
    aliases: MutMap<Symbol, Alias>,
    imported_abilities_state: PendingAbilitiesStore,
    initial_scope: MutMap<Ident, (Symbol, Region)>,
    exposed_symbols: VecSet<Symbol>,
    symbols_from_requires: &[(Loc<Symbol>, Loc<TypeAnnotation<'a>>)],
    var_store: &mut VarStore,
    opt_shorthand: Option<&'a str>,
    fx_mode: FxMode,
) -> ModuleOutput {
    let mut can_exposed_imports = MutMap::default();

    let mut scope = Scope::new(
        home,
        qualified_module_ids
            .get_name(home)
            .expect("home module not found")
            .as_inner()
            .to_owned(),
        exposed_ident_ids,
        imported_abilities_state,
    );
    let mut env = Env::new(
        arena,
        src,
        home,
        arena.alloc(Path::new(module_path)),
        dep_idents,
        qualified_module_ids,
        opt_shorthand,
        fx_mode,
    );

    for (name, alias) in aliases.into_iter() {
        scope.add_alias(
            name,
            alias.region,
            alias.type_variables,
            alias.infer_ext_in_output_variables,
            alias.typ,
            alias.kind,
        );
    }

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.

    crate::desugar::desugar_defs_node_values(&mut env, &mut scope, loc_defs, true);

    let mut rigid_variables = RigidVariables::default();

    // Initial scope values are treated like defs that appear before any others.
    // They include builtin types that are automatically imported, and for a platform
    // package, the required values from the app.
    //
    // Here we essentially add those "defs" to "the beginning of the module"
    // by canonicalizing them right before we canonicalize the actual ast::Def nodes.
    for (ident, (symbol, region)) in initial_scope {
        let first_char = ident.as_inline_str().as_str().chars().next().unwrap();

        if first_char.is_lowercase() {
            match scope.import_symbol(ident, symbol, region) {
                Ok(()) => {
                    // Add an entry to exposed_imports using the current module's name
                    // as the key; e.g. if this is the Foo module and we have
                    // Bar exposes [baz] then insert Foo.baz as the key, so when
                    // anything references `baz` in this Foo module, it will resolve to Bar.baz.
                    can_exposed_imports.insert(symbol, region);
                }
                Err((_shadowed_symbol, _region)) => {
                    internal_error!("TODO gracefully handle shadowing in imports.")
                }
            }
        } else {
            // This is a type alias or ability

            // the symbol should already be added to the scope when this module is canonicalized
            debug_assert!(
                scope.contains_alias(symbol) || scope.abilities_store.is_ability(symbol),
                "The {symbol:?} is not a type alias or ability known in {home:?}"
            );

            // but now we know this symbol by a different identifier, so we still need to add it to
            // the scope
            match scope.import_symbol(ident, symbol, region) {
                Ok(()) => {
                    // here we do nothing special
                }
                Err((shadowed_symbol, _region)) => {
                    internal_error!(
                        "TODO gracefully handle shadowing in imports, {:?} is shadowed.",
                        shadowed_symbol
                    )
                }
            }
        }
    }

    let mut output = Output::default();

    let module_params = header_type.get_params().as_ref().map(
        |roc_parse::header::ModuleParams {
             pattern,
             before_arrow: _,
             after_arrow: _,
         }| {
            let desugared_patterns =
                desugar_record_destructures(&mut env, &mut scope, pattern.value);

            let (destructs, _) = canonicalize_record_destructs(
                &mut env,
                var_store,
                &mut scope,
                &mut output,
                PatternType::ModuleParams,
                &desugared_patterns,
                pattern.region,
                PermitShadows(false),
            );

            let whole_symbol = scope.gen_unique_symbol();
            env.top_level_symbols.insert(whole_symbol);

            let whole_var = var_store.fresh();

            env.home_params_record = Some((whole_symbol, whole_var));

            ModuleParams {
                region: pattern.region,
                whole_var,
                whole_symbol,
                record_var: var_store.fresh(),
                record_ext_var: var_store.fresh(),
                destructs,
                arity_by_name: Default::default(),
            }
        },
    );

    let (defs, output, symbols_introduced, imports_introduced) = canonicalize_defs(
        &mut env,
        output,
        var_store,
        &mut scope,
        loc_defs,
        PatternType::TopLevelDef,
    );

    for (_early_return_var, early_return_region, early_return_kind) in &scope.early_returns {
        env.problem(Problem::ReturnOutsideOfFunction {
            region: *early_return_region,
            return_kind: *early_return_kind,
        });
    }

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

    // Gather up all the symbols that were referenced across all the defs' lookups.
    referenced_values.extend(output.references.value_lookups().copied());

    // Gather up all the symbols that were referenced across all the defs' calls.
    referenced_values.extend(output.references.calls().copied());

    // Gather up all the symbols that were referenced from other modules.
    referenced_values.extend(env.qualified_value_lookups.iter().copied());

    // NOTE previously we inserted builtin defs into the list of defs here
    // this is now done later, in file.rs.

    // assume all exposed symbols are not actually defined in the module
    // then as we walk the module and encounter the definitions, remove
    // symbols from this set
    let mut exposed_but_not_defined = exposed_symbols.clone();

    let new_output = Output {
        aliases: output.aliases,
        references: output.references,
        ..Default::default()
    };

    let (mut declarations, mut output) = crate::def::sort_top_level_can_defs(
        &mut env,
        &mut scope,
        var_store,
        defs,
        new_output,
        &exposed_symbols,
    );

    let module_params = module_params.map(|params| ModuleParams {
        arity_by_name: declarations.take_arity_by_name(),
        ..params
    });

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

    report_unused_imports(imports_introduced, &output.references, &mut env, &mut scope);

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
                    match header_type {
                        HeaderType::Builtin { .. } => {
                            match crate::builtins::builtin_defs_map(*symbol, var_store) {
                                None => {
                                    internal_error!("A builtin module contains a signature without implementation for {:?}", symbol)
                                }
                                Some(replacement_def) => {
                                    declarations.update_builtin_def(index, replacement_def);
                                }
                            }
                        }
                        HeaderType::Hosted { .. } => {
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
                                references: References::new(),
                                aliases: Default::default(),
                            };

                            let hosted_def = crate::effect_module::build_host_exposed_def(
                                &mut scope, *symbol, &ident, var_store, annotation,
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
                    match header_type {
                        HeaderType::Builtin { .. } => {
                            match crate::builtins::builtin_defs_map(*symbol, var_store) {
                                None => {
                                    internal_error!("A builtin module contains a signature without implementation for {:?}", symbol)
                                }
                                Some(replacement_def) => {
                                    declarations.update_builtin_def(index, replacement_def);
                                }
                            }
                        }
                        HeaderType::Hosted { .. } => {
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
                                references: References::new(),
                                aliases: Default::default(),
                            };

                            let hosted_def = crate::effect_module::build_host_exposed_def(
                                &mut scope, *symbol, &ident, var_store, annotation,
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
        }
    }

    let mut aliases = MutMap::default();

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
            kind: DefKind::Let,
        };

        declarations.push_def(def);
    }

    // Incorporate any remaining output.lookups entries into references.
    referenced_values.extend(output.references.value_lookups().copied());

    // Incorporate any remaining output.calls entries into references.
    referenced_values.extend(output.references.calls().copied());

    // Gather up all the symbols that were referenced from other modules.
    referenced_values.extend(env.qualified_value_lookups.iter().copied());

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
        }
    }

    let collected = declarations.expects();

    ModuleOutput {
        scope,
        aliases,
        rigid_variables,
        module_params,
        declarations,
        referenced_values,
        exposed_imports: can_exposed_imports,
        problems: env.problems,
        symbols_from_requires,
        pending_derives,
        loc_expects: collected.expects,
        has_dbgs: collected.has_dbgs,
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

        Try { result_expr, .. } => {
            fix_values_captured_in_closure_expr(
                &mut result_expr.value,
                no_capture_symbols,
                closure_captures,
            );
        }

        Return { return_value, .. } => {
            fix_values_captured_in_closure_expr(
                &mut return_value.value,
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

                    added_captures = true;
                }

                // Always jump one, because the current element either does not have captures or
                // is now one of the newly-added captures, which we don't need to check.
                i += 1;
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
        | ParamsVar { .. }
        | AbilityMember(..)
        | EmptyRecord
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

        ImportParams(_, _, Some((_, expr))) => {
            fix_values_captured_in_closure_expr(expr, no_capture_symbols, closure_captures);
        }

        ImportParams(_, _, None) => {}

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

use crate::annotation::canonicalize_annotation;
use crate::annotation::IntroducedVariables;
use crate::env::Env;
use crate::expr::ClosureData;
use crate::expr::Expr::{self, *};
use crate::expr::{
    canonicalize_expr, local_successors, references_from_call, references_from_local, Output,
    Recursive,
};
use crate::pattern::{bindings_from_patterns, canonicalize_pattern, Pattern};
use crate::procedure::References;
use crate::scope::create_alias;
use crate::scope::Scope;
use roc_collections::all::{default_hasher, ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_parse::ast;
use roc_parse::ast::AliasHeader;
use roc_parse::pattern::PatternType;
use roc_problem::can::{CycleEntry, Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, Type};
use std::collections::HashMap;
use std::fmt::Debug;
use ven_graph::{strongly_connected_components, topological_sort};

#[derive(Clone, Debug, PartialEq)]
pub struct Def {
    pub loc_pattern: Loc<Pattern>,
    pub loc_expr: Loc<Expr>,
    pub expr_var: Variable,
    pub pattern_vars: SendMap<Symbol, Variable>,
    pub annotation: Option<Annotation>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    pub signature: Type,
    pub introduced_variables: IntroducedVariables,
    pub aliases: SendMap<Symbol, Alias>,
    pub region: Region,
}

#[derive(Debug)]
pub struct CanDefs {
    pub refs_by_symbol: MutMap<Symbol, (Region, References)>,
    pub can_defs_by_symbol: MutMap<Symbol, Def>,
    pub aliases: SendMap<Symbol, Alias>,
}
/// A Def that has had patterns and type annnotations canonicalized,
/// but no Expr canonicalization has happened yet. Also, it has had spaces
/// and nesting resolved, and knows whether annotations are standalone or not.
#[derive(Debug, Clone, PartialEq)]
enum PendingDef<'a> {
    /// A standalone annotation with no body
    AnnotationOnly(
        &'a Loc<ast::Pattern<'a>>,
        Loc<Pattern>,
        &'a Loc<ast::TypeAnnotation<'a>>,
    ),
    /// A body with no type annotation
    Body(
        &'a Loc<ast::Pattern<'a>>,
        Loc<Pattern>,
        &'a Loc<ast::Expr<'a>>,
    ),
    /// A body with a type annotation
    TypedBody(
        &'a Loc<ast::Pattern<'a>>,
        Loc<Pattern>,
        &'a Loc<ast::TypeAnnotation<'a>>,
        &'a Loc<ast::Expr<'a>>,
    ),

    /// A type alias, e.g. `Ints : List Int`
    Alias {
        name: Loc<Symbol>,
        vars: Vec<Loc<Lowercase>>,
        ann: &'a Loc<ast::TypeAnnotation<'a>>,
    },

    /// An invalid alias, that is ignored in the rest of the pipeline
    /// e.g. a shadowed alias, or a definition like `MyAlias 1 : Int`
    /// with an incorrect pattern
    InvalidAlias,
}

// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Declaration {
    Declare(Def),
    DeclareRec(Vec<Def>),
    Builtin(Def),
    InvalidCycle(Vec<CycleEntry>),
}

impl Declaration {
    pub fn def_count(&self) -> usize {
        use Declaration::*;
        match self {
            Declare(_) => 1,
            DeclareRec(defs) => defs.len(),
            InvalidCycle { .. } => 0,
            Builtin(_) => 0,
        }
    }
}

/// Returns a topologically sorted sequence of alias names
fn sort_aliases_before_introduction(mut alias_symbols: MutMap<Symbol, Vec<Symbol>>) -> Vec<Symbol> {
    let defined_symbols: Vec<Symbol> = alias_symbols.keys().copied().collect();

    // find the strongly connected components and their relations
    let sccs = {
        // only retain symbols from the current alias_defs
        for v in alias_symbols.iter_mut() {
            v.1.retain(|x| defined_symbols.iter().any(|s| s == x));
        }

        let all_successors_with_self = |symbol: &Symbol| alias_symbols[symbol].iter().copied();

        strongly_connected_components(&defined_symbols, all_successors_with_self)
    };

    // then sort the strongly connected components
    let groups: Vec<_> = (0..sccs.len()).collect();
    let mut group_symbols: Vec<Vec<Symbol>> = vec![Vec::new(); groups.len()];

    let mut symbol_to_group_index = MutMap::default();
    let mut group_to_groups = vec![Vec::new(); groups.len()];

    for (index, group) in sccs.iter().enumerate() {
        for s in group {
            symbol_to_group_index.insert(*s, index);
        }
    }

    for (index, group) in sccs.iter().enumerate() {
        for s in group {
            let reachable = &alias_symbols[s];
            for r in reachable {
                let new_index = symbol_to_group_index[r];

                if new_index != index {
                    group_to_groups[index].push(new_index);
                }
            }
        }
    }

    for v in group_symbols.iter_mut() {
        v.sort();
        v.dedup();
    }

    let all_successors_with_self = |group: &usize| group_to_groups[*group].iter().copied();

    // split into self-recursive and mutually recursive
    match topological_sort(&groups, all_successors_with_self) {
        Ok(result) => result
            .iter()
            .rev()
            .map(|group_index| sccs[*group_index].iter())
            .flatten()
            .copied()
            .collect(),

        Err(_loop_detected) => unreachable!("the groups cannot recurse"),
    }
}

#[inline(always)]
pub fn canonicalize_defs<'a>(
    env: &mut Env<'a>,
    mut output: Output,
    var_store: &mut VarStore,
    original_scope: &Scope,
    loc_defs: &'a [&'a Loc<ast::Def<'a>>],
    pattern_type: PatternType,
) -> (CanDefs, Scope, Output, MutMap<Symbol, Region>) {
    // Canonicalizing defs while detecting shadowing involves a multi-step process:
    //
    // 1. Go through each of the patterns.
    // 2. For each identifier pattern, get the scope.symbol() for the ident. (That symbol will use the home module for its module.)
    // 3. If that symbol is already in scope, then we're about to shadow it. Error!
    // 4. Otherwise, add it to the scope immediately, so we can detect shadowing within the same
    //    pattern (e.g. (Foo a a) = ...)
    // 5. Add this canonicalized pattern and its corresponding ast::Expr to pending_exprs.
    // 5. Once every pattern has been processed and added to scope, go back and canonicalize the exprs from
    //    pending_exprs, this time building up a canonical def for each one.
    //
    // This way, whenever any expr is doing lookups, it knows everything that's in scope -
    // even defs that appear after it in the source.
    //
    // This naturally handles recursion too, because a given expr which refers
    // to itself won't be processed until after its def has been added to scope.

    // Record both the original and final idents from the scope,
    // so we can diff them while detecting unused defs.
    let mut scope = original_scope.clone();
    let num_defs = loc_defs.len();
    let mut refs_by_symbol = MutMap::default();
    let mut can_defs_by_symbol = HashMap::with_capacity_and_hasher(num_defs, default_hasher());
    let mut pending = Vec::with_capacity(num_defs); // TODO bump allocate this!

    // Canonicalize all the patterns, record shadowing problems, and store
    // the ast::Expr values in pending_exprs for further canonicalization
    // once we've finished assembling the entire scope.
    for loc_def in loc_defs {
        match to_pending_def(env, var_store, &loc_def.value, &mut scope, pattern_type) {
            None => (),
            Some((new_output, pending_def)) => {
                // store the top-level defs, used to ensure that closures won't capture them
                if let PatternType::TopLevelDef = pattern_type {
                    match &pending_def {
                        PendingDef::AnnotationOnly(_, loc_can_pattern, _)
                        | PendingDef::Body(_, loc_can_pattern, _)
                        | PendingDef::TypedBody(_, loc_can_pattern, _, _) => {
                            env.top_level_symbols.extend(
                                bindings_from_patterns(std::iter::once(loc_can_pattern))
                                    .iter()
                                    .map(|t| t.0),
                            )
                        }
                        PendingDef::Alias { .. } | PendingDef::InvalidAlias => {}
                    }
                }
                // Record the ast::Expr for later. We'll do another pass through these
                // once we have the entire scope assembled. If we were to canonicalize
                // the exprs right now, they wouldn't have symbols in scope from defs
                // that get would have gotten added later in the defs list!
                pending.push(pending_def);
                output.union(new_output);
            }
        }
    }

    if cfg!(debug_assertions) {
        env.home.register_debug_idents(&env.ident_ids);
    }

    let mut aliases = SendMap::default();
    let mut value_defs = Vec::new();

    let mut alias_defs = MutMap::default();
    let mut alias_symbols = MutMap::default();

    for pending_def in pending.into_iter() {
        match pending_def {
            PendingDef::Alias { name, vars, ann } => {
                let symbols =
                    crate::annotation::find_alias_symbols(env.home, &mut env.ident_ids, &ann.value);

                alias_symbols.insert(name.value, symbols);
                alias_defs.insert(name.value, (name, vars, ann));
            }
            other => value_defs.push(other),
        }
    }

    let sorted = sort_aliases_before_introduction(alias_symbols);

    for alias_name in sorted {
        let (name, vars, ann) = alias_defs.remove(&alias_name).unwrap();

        let symbol = name.value;
        let can_ann = canonicalize_annotation(env, &mut scope, &ann.value, ann.region, var_store);

        // Record all the annotation's references in output.references.lookups
        for symbol in can_ann.references {
            output.references.lookups.insert(symbol);
            output.references.referenced_aliases.insert(symbol);
        }

        let mut can_vars: Vec<Loc<(Lowercase, Variable)>> = Vec::with_capacity(vars.len());
        let mut is_phantom = false;

        for loc_lowercase in vars.iter() {
            if let Some(var) = can_ann
                .introduced_variables
                .var_by_name(&loc_lowercase.value)
            {
                // This is a valid lowercase rigid var for the alias.
                can_vars.push(Loc {
                    value: (loc_lowercase.value.clone(), *var),
                    region: loc_lowercase.region,
                });
            } else {
                is_phantom = true;

                env.problems.push(Problem::PhantomTypeArgument {
                    alias: symbol,
                    variable_region: loc_lowercase.region,
                    variable_name: loc_lowercase.value.clone(),
                });
            }
        }

        if is_phantom {
            // Bail out
            continue;
        }

        let alias = create_alias(symbol, name.region, can_vars.clone(), can_ann.typ.clone());
        aliases.insert(symbol, alias.clone());
    }

    // Now that we know the alias dependency graph, we can try to insert recursion variables
    // where aliases are recursive tag unions, or detect illegal recursions.
    let mut aliases = correct_mutual_recursive_type_alias(env, &aliases, var_store);
    for (symbol, alias) in aliases.iter() {
        scope.add_alias(
            *symbol,
            alias.region,
            alias.type_variables.clone(),
            alias.typ.clone(),
        );
    }

    // Now that we have the scope completely assembled, and shadowing resolved,
    // we're ready to canonicalize any body exprs.
    for pending_def in value_defs.into_iter() {
        output = canonicalize_pending_def(
            env,
            pending_def,
            output,
            &mut scope,
            &mut can_defs_by_symbol,
            var_store,
            &mut refs_by_symbol,
            &mut aliases,
        );

        // TODO we should do something with these references; they include
        // things like type annotations.
    }

    // Determine which idents we introduced in the course of this process.
    let mut symbols_introduced = MutMap::default();

    for (symbol, region) in scope.symbols() {
        if !original_scope.contains_symbol(*symbol) {
            symbols_introduced.insert(*symbol, *region);
        }
    }

    // This returns both the defs info as well as the new scope.
    //
    // We have to return the new scope because we added defs to it
    // (and those lookups shouldn't fail later, e.g. when canonicalizing
    // the return expr), but we didn't want to mutate the original scope
    // directly because we wanted to keep a clone of it around to diff
    // when looking for unused idents.
    //
    // We have to return the scope separately from the defs, because the
    // defs need to get moved later.
    (
        CanDefs {
            refs_by_symbol,
            can_defs_by_symbol,
            aliases,
        },
        scope,
        output,
        symbols_introduced,
    )
}

#[inline(always)]
pub fn sort_can_defs(
    env: &mut Env<'_>,
    defs: CanDefs,
    mut output: Output,
) -> (Result<Vec<Declaration>, RuntimeError>, Output) {
    let CanDefs {
        refs_by_symbol,
        can_defs_by_symbol,
        aliases,
    } = defs;

    for (symbol, alias) in aliases.into_iter() {
        output.aliases.insert(symbol, alias);
    }

    // Determine the full set of references by traversing the graph.
    let mut visited_symbols = MutSet::default();
    let returned_lookups = ImSet::clone(&output.references.lookups);

    // Start with the return expression's referenced locals. They're the only ones that count!
    //
    // If I have two defs which reference each other, but neither of them is referenced
    // in the return expression, I don't want either of them (or their references) to end up
    // in the final output.references. They were unused, and so were their references!
    //
    // The reason we need a graph here is so we don't overlook transitive dependencies.
    // For example, if I have `a = b + 1` and the def returns `a + 1`, then the
    // def as a whole references both `a` *and* `b`, even though it doesn't
    // directly mention `b` - because `a` depends on `b`. If we didn't traverse a graph here,
    // we'd erroneously give a warning that `b` was unused since it wasn't directly referenced.
    for symbol in returned_lookups.into_iter() {
        // We only care about local symbols in this analysis.
        if symbol.module_id() == env.home {
            // Traverse the graph and look up *all* the references for this local symbol.
            let refs =
                references_from_local(symbol, &mut visited_symbols, &refs_by_symbol, &env.closures);

            output.references = output.references.union(refs);
        }
    }

    for symbol in ImSet::clone(&output.references.calls).into_iter() {
        // Traverse the graph and look up *all* the references for this call.
        // Reuse the same visited_symbols as before; if we already visited it,
        // we won't learn anything new from visiting it again!
        let refs =
            references_from_call(symbol, &mut visited_symbols, &refs_by_symbol, &env.closures);

        output.references = output.references.union(refs);
    }

    let mut defined_symbols: Vec<Symbol> = Vec::new();
    let mut defined_symbols_set: ImSet<Symbol> = ImSet::default();

    for symbol in can_defs_by_symbol.keys().into_iter() {
        defined_symbols.push(*symbol);
        defined_symbols_set.insert(*symbol);
    }

    // Use topological sort to reorder the defs based on their dependencies to one another.
    // This way, during code gen, no def will refer to a value that hasn't been initialized yet.
    // As a bonus, the topological sort also reveals any cycles between the defs, allowing
    // us to give a CircularAssignment error for invalid (mutual) recursion, and a `DeclareRec` for mutually
    // recursive definitions.

    // All successors that occur in the body of a symbol.
    let all_successors_without_self = |symbol: &Symbol| -> ImSet<Symbol> {
        // This may not be in refs_by_symbol. For example, the `f` in `f x` here:
        //
        // f = \z -> z
        //
        // (\x ->
        //     a = f x
        //     x
        // )
        //
        // It's not part of the current defs (the one with `a = f x`); rather,
        // it's in the enclosing scope. It's still referenced though, so successors
        // will receive it as an argument!
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                // We can only sort the symbols at the current level. That is safe because
                // symbols defined at higher levels cannot refer to symbols at lower levels.
                // Therefore they can never form a cycle!
                //
                // In the above example, `f` cannot reference `a`, and in the closure
                // a call to `f` cannot cycle back to `a`.
                let mut loc_succ = local_successors(references, &env.closures);

                // if the current symbol is a closure, peek into its body
                if let Some(References { lookups, .. }) = env.closures.get(symbol) {
                    let home = env.home;

                    for lookup in lookups {
                        if lookup != symbol && lookup.module_id() == home {
                            // DO NOT register a self-call behind a lambda!
                            //
                            // We allow `boom = \_ -> boom {}`, but not `x = x`
                            loc_succ.insert(*lookup);
                        }
                    }
                }

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols_set.contains(key));

                loc_succ
            }
            None => ImSet::default(),
        }
    };

    // All successors that occur in the body of a symbol, including the symbol itself
    // This is required to determine whether a symbol is recursive. Recursive symbols
    // (that are not faulty) always need a DeclareRec, even if there is just one symbol in the
    // group
    let mut all_successors_with_self = |symbol: &Symbol| -> ImSet<Symbol> {
        // This may not be in refs_by_symbol. For example, the `f` in `f x` here:
        //
        // f = \z -> z
        //
        // (\x ->
        //     a = f x
        //     x
        // )
        //
        // It's not part of the current defs (the one with `a = f x`); rather,
        // it's in the enclosing scope. It's still referenced though, so successors
        // will receive it as an argument!
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                // We can only sort the symbols at the current level. That is safe because
                // symbols defined at higher levels cannot refer to symbols at lower levels.
                // Therefore they can never form a cycle!
                //
                // In the above example, `f` cannot reference `a`, and in the closure
                // a call to `f` cannot cycle back to `a`.
                let mut loc_succ = local_successors(references, &env.closures);

                // if the current symbol is a closure, peek into its body
                if let Some(References { lookups, .. }) = env.closures.get(symbol) {
                    for lookup in lookups {
                        loc_succ.insert(*lookup);
                    }
                }

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols_set.contains(key));

                loc_succ
            }
            None => ImSet::default(),
        }
    };

    // If a symbol is a direct successor of itself, there is an invalid cycle.
    // The difference with the function above is that this one does not look behind lambdas,
    // but does consider direct self-recursion.
    let direct_successors = |symbol: &Symbol| -> ImSet<Symbol> {
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                let mut loc_succ = local_successors(references, &env.closures);

                // NOTE: if the symbol is a closure we DONT look into its body

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols_set.contains(key));

                // NOTE: direct recursion does matter here: `x = x` is invalid recursion!

                loc_succ
            }
            None => ImSet::default(),
        }
    };

    // TODO also do the same `addDirects` check elm/compiler does, so we can
    // report an error if a recursive definition can't possibly terminate!
    match ven_graph::topological_sort_into_groups(
        defined_symbols.as_slice(),
        all_successors_without_self,
    ) {
        Ok(groups) => {
            let mut declarations = Vec::new();

            // groups are in reversed order
            for group in groups.into_iter().rev() {
                group_to_declaration(
                    &group,
                    &env.closures,
                    &mut all_successors_with_self,
                    &can_defs_by_symbol,
                    &mut declarations,
                );
            }

            (Ok(declarations), output)
        }
        Err((mut groups, nodes_in_cycle)) => {
            let mut declarations = Vec::new();
            let mut problems = Vec::new();

            // nodes_in_cycle are symbols that form a syntactic cycle. That isn't always a problem,
            // and in general it's impossible to decide whether it is. So we use a crude heuristic:
            //
            // Definitions where the cycle occurs behind a lambda are OK
            //
            // boom = \_ -> boom {}
            //
            // But otherwise we report an error, e.g.
            //
            // foo = if b then foo else bar

            for cycle in strongly_connected_components(&nodes_in_cycle, all_successors_without_self)
            {
                // check whether the cycle is faulty, which is when it has
                // a direct successor in the current cycle. This catches things like:
                //
                // x = x
                //
                // or
                //
                // p = q
                // q = p
                let is_invalid_cycle = match cycle.get(0) {
                    Some(symbol) => {
                        let mut succs = direct_successors(symbol);

                        succs.retain(|key| cycle.contains(key));

                        !succs.is_empty()
                    }
                    None => false,
                };

                if is_invalid_cycle {
                    // We want to show the entire cycle in the error message, so expand it out.
                    let mut entries = Vec::new();

                    for symbol in &cycle {
                        match refs_by_symbol.get(symbol) {
                            None => unreachable!(
                                r#"Symbol `{:?}` not found in refs_by_symbol! refs_by_symbol was: {:?}"#,
                                symbol, refs_by_symbol
                            ),
                            Some((region, _)) => {
                                let expr_region =
                                    can_defs_by_symbol.get(symbol).unwrap().loc_expr.region;

                                let entry = CycleEntry {
                                    symbol: *symbol,
                                    symbol_region: *region,
                                    expr_region,
                                };

                                entries.push(entry);
                            }
                        }
                    }

                    // Sort them by line number to make the report more helpful.
                    entries.sort_by_key(|entry| entry.symbol_region);

                    problems.push(Problem::RuntimeError(RuntimeError::CircularDef(
                        entries.clone(),
                    )));

                    declarations.push(Declaration::InvalidCycle(entries));
                }

                // if it's an invalid cycle, other groups may depend on the
                // symbols defined here, so also push this cycle onto the groups
                //
                // if it's not an invalid cycle, this is slightly inefficient,
                // because we know this becomes exactly one DeclareRec already
                groups.push(cycle);
            }

            // now we have a collection of groups whose dependencies are not cyclic.
            // They are however not yet topologically sorted. Here we have to get a bit
            // creative to get all the definitions in the correct sorted order.

            let mut group_ids = Vec::with_capacity(groups.len());
            let mut symbol_to_group_index = MutMap::default();
            for (i, group) in groups.iter().enumerate() {
                for symbol in group {
                    symbol_to_group_index.insert(*symbol, i);
                }

                group_ids.push(i);
            }

            let successors_of_group = |group_id: &usize| {
                let mut result = MutSet::default();

                // for each symbol in this group
                for symbol in &groups[*group_id] {
                    // find its successors
                    for succ in all_successors_without_self(symbol) {
                        // and add its group to the result
                        match symbol_to_group_index.get(&succ) {
                            Some(index) => {
                                result.insert(*index);
                            }
                            None => unreachable!("no index for symbol {:?}", succ),
                        }
                    }
                }

                // don't introduce any cycles to self
                result.remove(group_id);

                result
            };

            match ven_graph::topological_sort_into_groups(&group_ids, successors_of_group) {
                Ok(sorted_group_ids) => {
                    for sorted_group in sorted_group_ids.iter().rev() {
                        for group_id in sorted_group.iter().rev() {
                            let group = &groups[*group_id];

                            group_to_declaration(
                                group,
                                &env.closures,
                                &mut all_successors_with_self,
                                &can_defs_by_symbol,
                                &mut declarations,
                            );
                        }
                    }
                }
                Err(_) => unreachable!("there should be no cycles now!"),
            }

            for problem in problems {
                env.problem(problem);
            }

            (Ok(declarations), output)
        }
    }
}

fn group_to_declaration(
    group: &[Symbol],
    closures: &MutMap<Symbol, References>,
    successors: &mut dyn FnMut(&Symbol) -> ImSet<Symbol>,
    can_defs_by_symbol: &MutMap<Symbol, Def>,
    declarations: &mut Vec<Declaration>,
) {
    use Declaration::*;

    // We want only successors in the current group, otherwise definitions get duplicated
    let filtered_successors = |symbol: &Symbol| -> ImSet<Symbol> {
        let mut result = successors(symbol);

        result.retain(|key| group.contains(key));
        result
    };

    // Patterns like
    //
    // { x, y } = someDef
    //
    // Can bind multiple symbols. When not incorrectly recursive (which is guaranteed in this function),
    // normally `someDef` would be inserted twice. We use the region of the pattern as a unique key
    // for a definition, so every definition is only inserted (thus typechecked and emitted) once
    let mut seen_pattern_regions: ImSet<Region> = ImSet::default();

    for cycle in strongly_connected_components(group, filtered_successors) {
        if cycle.len() == 1 {
            let symbol = &cycle[0];

            if let Some(can_def) = can_defs_by_symbol.get(symbol) {
                let mut new_def = can_def.clone();

                // Determine recursivity of closures that are not tail-recursive
                if let Closure(ClosureData {
                    recursive: recursive @ Recursive::NotRecursive,
                    ..
                }) = &mut new_def.loc_expr.value
                {
                    *recursive = closure_recursivity(*symbol, closures);
                }

                let is_recursive = successors(symbol).contains(symbol);

                if !seen_pattern_regions.contains(&new_def.loc_pattern.region) {
                    if is_recursive {
                        declarations.push(DeclareRec(vec![new_def.clone()]));
                    } else {
                        declarations.push(Declare(new_def.clone()));
                    }
                    seen_pattern_regions.insert(new_def.loc_pattern.region);
                }
            }
        } else {
            let mut can_defs = Vec::new();

            // Topological sort gives us the reverse of the sorting we want!
            for symbol in cycle.into_iter().rev() {
                if let Some(can_def) = can_defs_by_symbol.get(&symbol) {
                    let mut new_def = can_def.clone();

                    // Determine recursivity of closures that are not tail-recursive
                    if let Closure(ClosureData {
                        recursive: recursive @ Recursive::NotRecursive,
                        ..
                    }) = &mut new_def.loc_expr.value
                    {
                        *recursive = closure_recursivity(symbol, closures);
                    }

                    if !seen_pattern_regions.contains(&new_def.loc_pattern.region) {
                        can_defs.push(new_def.clone());
                    }

                    seen_pattern_regions.insert(new_def.loc_pattern.region);
                }
            }

            declarations.push(DeclareRec(can_defs));
        }
    }
}

fn pattern_to_vars_by_symbol(
    vars_by_symbol: &mut SendMap<Symbol, Variable>,
    pattern: &Pattern,
    expr_var: Variable,
) {
    use Pattern::*;
    match pattern {
        Identifier(symbol) | Shadowed(_, _, symbol) => {
            vars_by_symbol.insert(*symbol, expr_var);
        }

        AppliedTag { arguments, .. } => {
            for (var, nested) in arguments {
                pattern_to_vars_by_symbol(vars_by_symbol, &nested.value, *var);
            }
        }

        RecordDestructure { destructs, .. } => {
            for destruct in destructs {
                vars_by_symbol.insert(destruct.value.symbol, destruct.value.var);
            }
        }

        NumLiteral(..)
        | IntLiteral(..)
        | FloatLiteral(..)
        | StrLiteral(_)
        | Underscore
        | MalformedPattern(_, _)
        | UnsupportedPattern(_) => {}
    }
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
#[allow(clippy::cognitive_complexity)]
fn canonicalize_pending_def<'a>(
    env: &mut Env<'a>,
    pending_def: PendingDef<'a>,
    mut output: Output,
    scope: &mut Scope,
    can_defs_by_symbol: &mut MutMap<Symbol, Def>,
    var_store: &mut VarStore,
    refs_by_symbol: &mut MutMap<Symbol, (Region, References)>,
    aliases: &mut SendMap<Symbol, Alias>,
) -> Output {
    use PendingDef::*;

    // Make types for the body expr, even if we won't end up having a body.
    let expr_var = var_store.fresh();
    let mut vars_by_symbol = SendMap::default();

    match pending_def {
        AnnotationOnly(_, loc_can_pattern, loc_ann) => {
            // annotation sans body cannot introduce new rigids that are visible in other annotations
            // but the rigids can show up in type error messages, so still register them
            let ann =
                canonicalize_annotation(env, scope, &loc_ann.value, loc_ann.region, var_store);

            // Record all the annotation's references in output.references.lookups

            for symbol in ann.references {
                output.references.lookups.insert(symbol);
                output.references.referenced_aliases.insert(symbol);
            }

            aliases.extend(ann.aliases.clone());

            output.introduced_variables.union(&ann.introduced_variables);

            pattern_to_vars_by_symbol(&mut vars_by_symbol, &loc_can_pattern.value, expr_var);

            let typ = ann.typ;

            let arity = typ.arity();

            let problem = match &loc_can_pattern.value {
                Pattern::Identifier(symbol) => RuntimeError::NoImplementationNamed {
                    def_symbol: *symbol,
                },
                Pattern::Shadowed(region, loc_ident, _new_symbol) => RuntimeError::Shadowing {
                    original_region: *region,
                    shadow: loc_ident.clone(),
                },
                _ => RuntimeError::NoImplementation,
            };

            // Fabricate a body for this annotation, that will error at runtime
            let value = Expr::RuntimeError(problem);
            let is_closure = arity > 0;
            let loc_can_expr = if !is_closure {
                Loc {
                    value,
                    region: loc_ann.region,
                }
            } else {
                let symbol = env.gen_unique_symbol();

                // generate a fake pattern for each argument. this makes signatures
                // that are functions only crash when they are applied.
                let mut underscores = Vec::with_capacity(arity);

                for _ in 0..arity {
                    let underscore: Loc<Pattern> = Loc {
                        value: Pattern::Underscore,
                        region: Region::zero(),
                    };

                    underscores.push((var_store.fresh(), underscore));
                }

                let body_expr = Loc {
                    value,
                    region: loc_ann.region,
                };

                Loc {
                    value: Closure(ClosureData {
                        function_type: var_store.fresh(),
                        closure_type: var_store.fresh(),
                        closure_ext_var: var_store.fresh(),
                        return_type: var_store.fresh(),
                        name: symbol,
                        captured_symbols: Vec::new(),
                        recursive: Recursive::NotRecursive,
                        arguments: underscores,
                        loc_body: Box::new(body_expr),
                    }),
                    region: loc_ann.region,
                }
            };

            for (_, (symbol, _)) in scope.idents() {
                if !vars_by_symbol.contains_key(symbol) {
                    continue;
                }

                // We could potentially avoid some clones here by using Rc strategically,
                // but the total amount of cloning going on here should typically be minimal.
                can_defs_by_symbol.insert(
                    *symbol,
                    Def {
                        expr_var,
                        // TODO try to remove this .clone()!
                        loc_pattern: loc_can_pattern.clone(),
                        loc_expr: Loc {
                            region: loc_can_expr.region,
                            // TODO try to remove this .clone()!
                            value: loc_can_expr.value.clone(),
                        },
                        pattern_vars: vars_by_symbol.clone(),
                        annotation: Some(Annotation {
                            signature: typ.clone(),
                            introduced_variables: output.introduced_variables.clone(),
                            aliases: ann.aliases.clone(),
                            region: loc_ann.region,
                        }),
                    },
                );
            }
        }

        Alias { .. } => unreachable!("Aliases are handled in a separate pass"),
        InvalidAlias => {
            // invalid aliases (shadowed, incorrect patterns) get ignored
        }
        TypedBody(loc_pattern, loc_can_pattern, loc_ann, loc_expr) => {
            let ann =
                canonicalize_annotation(env, scope, &loc_ann.value, loc_ann.region, var_store);

            // Record all the annotation's references in output.references.lookups
            for symbol in ann.references {
                output.references.lookups.insert(symbol);
                output.references.referenced_aliases.insert(symbol);
            }

            let typ = ann.typ;

            for (symbol, alias) in ann.aliases.clone() {
                aliases.insert(symbol, alias);
            }

            output.introduced_variables.union(&ann.introduced_variables);

            // bookkeeping for tail-call detection. If we're assigning to an
            // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
            let outer_identifier = env.tailcallable_symbol;

            if let Pattern::Identifier(ref defined_symbol) = &loc_can_pattern.value {
                env.tailcallable_symbol = Some(*defined_symbol);
            };

            // register the name of this closure, to make sure the closure won't capture it's own name
            if let (Pattern::Identifier(ref defined_symbol), &ast::Expr::Closure(_, _)) =
                (&loc_can_pattern.value, &loc_expr.value)
            {
                env.closure_name_symbol = Some(*defined_symbol);
            };

            pattern_to_vars_by_symbol(&mut vars_by_symbol, &loc_can_pattern.value, expr_var);

            let (mut loc_can_expr, can_output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            output.references = output.references.union(can_output.references.clone());

            // reset the tailcallable_symbol
            env.tailcallable_symbol = outer_identifier;

            // see below: a closure needs a fresh References!
            let mut is_closure = false;

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            if let (
                &ast::Pattern::Identifier(_name),
                &Pattern::Identifier(ref defined_symbol),
                &Closure(ClosureData {
                    function_type,
                    closure_type,
                    closure_ext_var,
                    return_type,
                    name: ref symbol,
                    ref arguments,
                    loc_body: ref body,
                    ref captured_symbols,
                    ..
                }),
            ) = (
                &loc_pattern.value,
                &loc_can_pattern.value,
                &loc_can_expr.value,
            ) {
                is_closure = true;

                // Since everywhere in the code it'll be referred to by its defined name,
                // remove its generated name from the closure map. (We'll re-insert it later.)
                let references = env.closures.remove(symbol).unwrap_or_else(|| {
                    panic!(
                        "Tried to remove symbol {:?} from procedures, but it was not found: {:?}",
                        symbol, env.closures
                    )
                });

                // Re-insert the closure into the map, under its defined name.
                // closures don't have a name, and therefore pick a fresh symbol. But in this
                // case, the closure has a proper name (e.g. `foo` in `foo = \x y -> ...`
                // and we want to reference it by that name.
                env.closures.insert(*defined_symbol, references);

                // The closure is self tail recursive iff it tail calls itself (by defined name).
                let is_recursive = match can_output.tail_call {
                    Some(ref symbol) if symbol == defined_symbol => Recursive::TailRecursive,
                    _ => Recursive::NotRecursive,
                };

                // Recursion doesn't count as referencing. (If it did, all recursive functions
                // would result in circular def errors!)
                refs_by_symbol
                    .entry(*defined_symbol)
                    .and_modify(|(_, refs)| {
                        refs.lookups = refs.lookups.without(defined_symbol);
                    });

                // renamed_closure_def = Some(&defined_symbol);
                loc_can_expr.value = Closure(ClosureData {
                    function_type,
                    closure_type,
                    closure_ext_var,
                    return_type,
                    name: *defined_symbol,
                    captured_symbols: captured_symbols.clone(),
                    recursive: is_recursive,
                    arguments: arguments.clone(),
                    loc_body: body.clone(),
                });
            }

            // Store the referenced locals in the refs_by_symbol map, so we can later figure out
            // which defined names reference each other.
            for (_, (symbol, region)) in scope.idents() {
                if !vars_by_symbol.contains_key(symbol) {
                    continue;
                }

                let refs =
                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    if is_closure {
                        References::new()
                    } else {
                        can_output.references.clone()
                    };

                refs_by_symbol.insert(*symbol, (*region, refs));

                can_defs_by_symbol.insert(
                    *symbol,
                    Def {
                        expr_var,
                        // TODO try to remove this .clone()!
                        loc_pattern: loc_can_pattern.clone(),
                        loc_expr: Loc {
                            region: loc_can_expr.region,
                            // TODO try to remove this .clone()!
                            value: loc_can_expr.value.clone(),
                        },
                        pattern_vars: vars_by_symbol.clone(),
                        annotation: Some(Annotation {
                            signature: typ.clone(),
                            introduced_variables: output.introduced_variables.clone(),
                            aliases: ann.aliases.clone(),
                            region: loc_ann.region,
                        }),
                    },
                );
            }
        }
        // If we have a pattern, then the def has a body (that is, it's not a
        // standalone annotation), so we need to canonicalize the pattern and expr.
        Body(loc_pattern, loc_can_pattern, loc_expr) => {
            // bookkeeping for tail-call detection. If we're assigning to an
            // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
            let outer_identifier = env.tailcallable_symbol;

            if let (&ast::Pattern::Identifier(_name), &Pattern::Identifier(ref defined_symbol)) =
                (&loc_pattern.value, &loc_can_pattern.value)
            {
                env.tailcallable_symbol = Some(*defined_symbol);

                // TODO isn't types_by_symbol enough? Do we need vars_by_symbol too?
                vars_by_symbol.insert(*defined_symbol, expr_var);
            };

            // register the name of this closure, to make sure the closure won't capture it's own name
            if let (Pattern::Identifier(ref defined_symbol), &ast::Expr::Closure(_, _)) =
                (&loc_can_pattern.value, &loc_expr.value)
            {
                env.closure_name_symbol = Some(*defined_symbol);
            };

            let (mut loc_can_expr, can_output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            // reset the tailcallable_symbol
            env.tailcallable_symbol = outer_identifier;

            // see below: a closure needs a fresh References!
            let mut is_closure = false;

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            if let (
                &ast::Pattern::Identifier(_name),
                &Pattern::Identifier(ref defined_symbol),
                &Closure(ClosureData {
                    function_type,
                    closure_type,
                    closure_ext_var,
                    return_type,
                    name: ref symbol,
                    ref arguments,
                    loc_body: ref body,
                    ref captured_symbols,
                    ..
                }),
            ) = (
                &loc_pattern.value,
                &loc_can_pattern.value,
                &loc_can_expr.value,
            ) {
                is_closure = true;

                // Since everywhere in the code it'll be referred to by its defined name,
                // remove its generated name from the closure map. (We'll re-insert it later.)
                let references = env.closures.remove(symbol).unwrap_or_else(|| {
                    panic!(
                        "Tried to remove symbol {:?} from procedures, but it was not found: {:?}",
                        symbol, env.closures
                    )
                });

                // Re-insert the closure into the map, under its defined name.
                // closures don't have a name, and therefore pick a fresh symbol. But in this
                // case, the closure has a proper name (e.g. `foo` in `foo = \x y -> ...`
                // and we want to reference it by that name.
                env.closures.insert(*defined_symbol, references);

                // The closure is self tail recursive iff it tail calls itself (by defined name).
                let is_recursive = match can_output.tail_call {
                    Some(ref symbol) if symbol == defined_symbol => Recursive::TailRecursive,
                    _ => Recursive::NotRecursive,
                };

                // Recursion doesn't count as referencing. (If it did, all recursive functions
                // would result in circular def errors!)
                refs_by_symbol
                    .entry(*defined_symbol)
                    .and_modify(|(_, refs)| {
                        refs.lookups = refs.lookups.without(defined_symbol);
                    });

                loc_can_expr.value = Closure(ClosureData {
                    function_type,
                    closure_type,
                    closure_ext_var,
                    return_type,
                    name: *defined_symbol,
                    captured_symbols: captured_symbols.clone(),
                    recursive: is_recursive,
                    arguments: arguments.clone(),
                    loc_body: body.clone(),
                });
            }

            // Store the referenced locals in the refs_by_symbol map, so we can later figure out
            // which defined names reference each other.
            for (symbol, region) in bindings_from_patterns(std::iter::once(&loc_can_pattern)) {
                let refs =
                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    if is_closure {
                        References::new()
                    } else {
                        can_output.references.clone()
                    };

                refs_by_symbol.insert(symbol, (region, refs));

                can_defs_by_symbol.insert(
                    symbol,
                    Def {
                        expr_var,
                        // TODO try to remove this .clone()!
                        loc_pattern: loc_can_pattern.clone(),
                        loc_expr: Loc {
                            // TODO try to remove this .clone()!
                            region: loc_can_expr.region,
                            value: loc_can_expr.value.clone(),
                        },
                        pattern_vars: vars_by_symbol.clone(),
                        annotation: None,
                    },
                );
            }

            output.union(can_output);
        }
    };

    output
}

#[inline(always)]
pub fn can_defs_with_return<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: Scope,
    loc_defs: &'a [&'a Loc<ast::Def<'a>>],
    loc_ret: &'a Loc<ast::Expr<'a>>,
) -> (Expr, Output) {
    let (unsorted, mut scope, defs_output, symbols_introduced) = canonicalize_defs(
        env,
        Output::default(),
        var_store,
        &scope,
        loc_defs,
        PatternType::DefExpr,
    );

    // The def as a whole is a tail call iff its return expression is a tail call.
    // Use its output as a starting point because its tail_call already has the right answer!
    let (ret_expr, mut output) =
        canonicalize_expr(env, var_store, &mut scope, loc_ret.region, &loc_ret.value);

    output
        .introduced_variables
        .union(&defs_output.introduced_variables);
    output.references = output.references.union(defs_output.references);

    // Now that we've collected all the references, check to see if any of the new idents
    // we defined went unused by the return expression. If any were unused, report it.
    for (symbol, region) in symbols_introduced {
        if !output.references.has_lookup(symbol) {
            env.problem(Problem::UnusedDef(symbol, region));
        }
    }

    let (can_defs, output) = sort_can_defs(env, unsorted, output);

    match can_defs {
        Ok(decls) => {
            let mut loc_expr: Loc<Expr> = ret_expr;

            for declaration in decls.into_iter().rev() {
                loc_expr = Loc {
                    region: Region::zero(),
                    value: decl_to_let(var_store, declaration, loc_expr),
                };
            }

            (loc_expr.value, output)
        }
        Err(err) => (RuntimeError(err), output),
    }
}

fn decl_to_let(var_store: &mut VarStore, decl: Declaration, loc_ret: Loc<Expr>) -> Expr {
    match decl {
        Declaration::Declare(def) => {
            Expr::LetNonRec(Box::new(def), Box::new(loc_ret), var_store.fresh())
        }
        Declaration::DeclareRec(defs) => Expr::LetRec(defs, Box::new(loc_ret), var_store.fresh()),
        Declaration::InvalidCycle(entries) => {
            Expr::RuntimeError(RuntimeError::CircularDef(entries))
        }
        Declaration::Builtin(_) => {
            // Builtins should only be added to top-level decls, not to let-exprs!
            unreachable!()
        }
    }
}

fn closure_recursivity(symbol: Symbol, closures: &MutMap<Symbol, References>) -> Recursive {
    let mut visited = MutSet::default();

    let mut stack = Vec::new();

    if let Some(references) = closures.get(&symbol) {
        for v in &references.calls {
            stack.push(*v);
        }

        // while there are symbols left to visit
        while let Some(nested_symbol) = stack.pop() {
            if nested_symbol == symbol {
                return Recursive::Recursive;
            }

            // if the called symbol not yet in the graph
            if !visited.contains(&nested_symbol) {
                // add it to the visited set
                // if it calls any functions
                if let Some(nested_references) = closures.get(&nested_symbol) {
                    // add its called to the stack
                    for v in &nested_references.calls {
                        stack.push(*v);
                    }
                }
                visited.insert(nested_symbol);
            }
        }
    }

    Recursive::NotRecursive
}

fn to_pending_def<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    def: &'a ast::Def<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> Option<(Output, PendingDef<'a>)> {
    use roc_parse::ast::Def::*;

    match def {
        Annotation(loc_pattern, loc_ann) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = canonicalize_pattern(
                env,
                var_store,
                scope,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            Some((
                output,
                PendingDef::AnnotationOnly(loc_pattern, loc_can_pattern, loc_ann),
            ))
        }
        Body(loc_pattern, loc_expr) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = canonicalize_pattern(
                env,
                var_store,
                scope,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            Some((
                output,
                PendingDef::Body(loc_pattern, loc_can_pattern, loc_expr),
            ))
        }

        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: _,
            body_pattern,
            body_expr,
        } => {
            if ann_pattern.value.equivalent(&body_pattern.value) {
                // NOTE: Pick the body pattern, picking the annotation one is
                // incorrect in the presence of optional record fields!
                //
                // { x, y } : { x : Int, y ? Bool }*
                // { x, y ? False } = rec
                Some(pending_typed_body(
                    env,
                    body_pattern,
                    ann_type,
                    body_expr,
                    var_store,
                    scope,
                    pattern_type,
                ))
            } else {
                // the pattern of the annotation does not match the pattern of the body direc
                env.problems.push(Problem::SignatureDefMismatch {
                    annotation_pattern: ann_pattern.region,
                    def_pattern: body_pattern.region,
                });

                // TODO: Should we instead build some PendingDef::InvalidAnnotatedBody ? This would
                // remove the `Option` on this function (and be probably more reliable for further
                // problem/error reporting)
                None
            }
        }

        Alias {
            header: AliasHeader { name, vars },
            ann,
            ..
        } => {
            let region = Region::span_across(&name.region, &ann.region);

            match scope.introduce(
                name.value.into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                region,
            ) {
                Ok(symbol) => {
                    let mut can_rigids: Vec<Loc<Lowercase>> = Vec::with_capacity(vars.len());

                    for loc_var in vars.iter() {
                        match loc_var.value {
                            ast::Pattern::Identifier(name)
                                if name.chars().next().unwrap().is_lowercase() =>
                            {
                                let lowercase = Lowercase::from(name);
                                can_rigids.push(Loc {
                                    value: lowercase,
                                    region: loc_var.region,
                                });
                            }
                            _ => {
                                // any other pattern in this position is a syntax error.
                                env.problems.push(Problem::InvalidAliasRigid {
                                    alias_name: symbol,
                                    region: loc_var.region,
                                });

                                return Some((Output::default(), PendingDef::InvalidAlias));
                            }
                        }
                    }

                    Some((
                        Output::default(),
                        PendingDef::Alias {
                            name: Loc {
                                region: name.region,
                                value: symbol,
                            },
                            vars: can_rigids,
                            ann,
                        },
                    ))
                }

                Err((original_region, loc_shadowed_symbol, _new_symbol)) => {
                    env.problem(Problem::ShadowingInAnnotation {
                        original_region,
                        shadow: loc_shadowed_symbol,
                    });

                    Some((Output::default(), PendingDef::InvalidAlias))
                }
            }
        }

        Expect(_condition) => todo!(),

        SpaceBefore(sub_def, _) | SpaceAfter(sub_def, _) => {
            to_pending_def(env, var_store, sub_def, scope, pattern_type)
        }

        NotYetImplemented(s) => todo!("{}", s),
    }
}

fn pending_typed_body<'a>(
    env: &mut Env<'a>,
    loc_pattern: &'a Loc<ast::Pattern<'a>>,
    loc_ann: &'a Loc<ast::TypeAnnotation<'a>>,
    loc_expr: &'a Loc<ast::Expr<'a>>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> (Output, PendingDef<'a>) {
    // This takes care of checking for shadowing and adding idents to scope.
    let (output, loc_can_pattern) = canonicalize_pattern(
        env,
        var_store,
        scope,
        pattern_type,
        &loc_pattern.value,
        loc_pattern.region,
    );

    (
        output,
        PendingDef::TypedBody(loc_pattern, loc_can_pattern, loc_ann, loc_expr),
    )
}

/// Make aliases recursive
fn correct_mutual_recursive_type_alias<'a>(
    env: &mut Env<'a>,
    original_aliases: &SendMap<Symbol, Alias>,
    var_store: &mut VarStore,
) -> SendMap<Symbol, Alias> {
    let mut symbols_introduced = ImSet::default();

    for (key, _) in original_aliases.iter() {
        symbols_introduced.insert(*key);
    }

    let all_successors_with_self = |symbol: &Symbol| -> ImSet<Symbol> {
        match original_aliases.get(symbol) {
            Some(alias) => {
                let mut loc_succ = alias.typ.symbols();
                // remove anything that is not defined in the current block
                loc_succ.retain(|key| symbols_introduced.contains(key));

                loc_succ
            }
            None => ImSet::default(),
        }
    };

    // TODO investigate should this be in a loop?
    let defined_symbols: Vec<Symbol> = original_aliases.keys().copied().collect();

    let cycles = strongly_connected_components(&defined_symbols, all_successors_with_self);
    let mut solved_aliases = SendMap::default();

    for cycle in cycles {
        debug_assert!(!cycle.is_empty());

        let mut pending_aliases: SendMap<_, _> = cycle
            .iter()
            .map(|&sym| (sym, original_aliases.get(&sym).unwrap().clone()))
            .collect();

        // Make sure we report only one error for the cycle, not an error for every
        // alias in the cycle.
        let mut can_still_report_error = true;

        for &rec in cycle.iter() {
            // First, we need to instantiate the alias with any symbols in the currrent module it
            // depends on.
            // We only need to worry about symbols in this SCC or any prior one, since the SCCs
            // were sorted topologically, and we've already instantiated aliases coming from other
            // modules.
            let mut to_instantiate: ImMap<_, _> = solved_aliases.clone().into_iter().collect();
            let mut others_in_scc = Vec::with_capacity(cycle.len() - 1);
            for &other in cycle.iter() {
                if rec != other {
                    others_in_scc.push(other);
                    if let Some(alias) = original_aliases.get(&other) {
                        to_instantiate.insert(other, alias.clone());
                    }
                }
            }

            let alias = pending_aliases.get_mut(&rec).unwrap();
            alias.typ.instantiate_aliases(
                alias.region,
                &to_instantiate,
                var_store,
                &mut ImSet::default(),
            );

            // Now mark the alias recursive, if it needs to be.
            let is_self_recursive = alias.typ.contains_symbol(rec);
            let is_mutually_recursive = cycle.len() > 1;

            if is_self_recursive || is_mutually_recursive {
                let _made_recursive = make_tag_union_of_alias_recursive(
                    env,
                    rec,
                    alias,
                    vec![],
                    var_store,
                    &mut can_still_report_error,
                );
            }
        }

        // The cycle we just instantiated and marked recursive may still be an illegal cycle, if
        // all the types in the cycle are narrow newtypes. We can't figure this out until now,
        // because we need all the types to be deeply instantiated.
        let all_are_narrow = cycle.iter().all(|sym| {
            let typ = &pending_aliases.get(sym).unwrap().typ;
            matches!(typ, Type::RecursiveTagUnion(..)) && typ.is_narrow()
        });

        if all_are_narrow {
            // This cycle is illegal!
            let mut rest = cycle;
            let alias_name = rest.pop().unwrap();

            let alias = pending_aliases.get_mut(&alias_name).unwrap();

            mark_cyclic_alias(
                env,
                &mut alias.typ,
                alias_name,
                alias.region,
                rest,
                can_still_report_error,
            )
        }

        // Now, promote all resolved aliases in this cycle as solved.
        solved_aliases.extend(pending_aliases);
    }

    solved_aliases
}

fn make_tag_union_of_alias_recursive<'a>(
    env: &mut Env<'a>,
    alias_name: Symbol,
    alias: &mut Alias,
    others: Vec<Symbol>,
    var_store: &mut VarStore,
    can_report_error: &mut bool,
) -> Result<(), ()> {
    let alias_args = alias
        .type_variables
        .iter()
        .map(|l| (l.value.0.clone(), Type::Variable(l.value.1)))
        .collect::<Vec<_>>();

    make_tag_union_recursive_help(
        env,
        Loc::at(alias.header_region(), (alias_name, &alias_args)),
        alias.region,
        others,
        &mut alias.typ,
        var_store,
        can_report_error,
    )
}

/// Attempt to make a tag union recursive at the position of `recursive_alias`; for example,
///
/// ```roc
/// [ Cons a (ConsList a), Nil ] as ConsList a
/// ```
///
/// can be made recursive at the position "ConsList a" with a fresh recursive variable, say r1:
///
/// ```roc
/// [ Cons a r1, Nil ] as r1
/// ```
///
/// Returns `Err` if the tag union is recursive, but there is no structure-preserving recursion
/// variable for it. This can happen when the type is a nested datatype, for example in either of
///
/// ```roc
/// Nested a : [ Chain a (Nested (List a)), Term ]
/// DuoList a b : [ Cons a (DuoList b a), Nil ]
/// ```
///
/// When `Err` is returned, a problem will be added to `env`.
fn make_tag_union_recursive_help<'a>(
    env: &mut Env<'a>,
    recursive_alias: Loc<(Symbol, &[(Lowercase, Type)])>,
    region: Region,
    others: Vec<Symbol>,
    typ: &mut Type,
    var_store: &mut VarStore,
    can_report_error: &mut bool,
) -> Result<(), ()> {
    let Loc {
        value: (symbol, args),
        region: alias_region,
    } = recursive_alias;
    let vars = args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
    match typ {
        Type::TagUnion(tags, ext) => {
            let rec_var = var_store.fresh();
            let mut pending_typ = Type::RecursiveTagUnion(rec_var, tags.to_vec(), ext.clone());
            let substitution_result =
                pending_typ.substitute_alias(symbol, &vars, &Type::Variable(rec_var));
            match substitution_result {
                Ok(()) => {
                    // We can substitute the alias presence for the variable exactly.
                    *typ = pending_typ;
                    Ok(())
                }
                Err(differing_recursion_region) => {
                    env.problems.push(Problem::NestedDatatype {
                        alias: symbol,
                        def_region: alias_region,
                        differing_recursion_region,
                    });
                    Err(())
                }
            }
        }
        Type::RecursiveTagUnion(_, _, _) => Ok(()),
        Type::Alias {
            actual,
            type_arguments,
            ..
        } => make_tag_union_recursive_help(
            env,
            Loc::at_zero((symbol, type_arguments)),
            region,
            others,
            actual,
            var_store,
            can_report_error,
        ),
        _ => {
            mark_cyclic_alias(env, typ, symbol, region, others, *can_report_error);
            *can_report_error = false;

            Ok(())
        }
    }
}

fn mark_cyclic_alias<'a>(
    env: &mut Env<'a>,
    typ: &mut Type,
    symbol: Symbol,
    region: Region,
    others: Vec<Symbol>,
    report: bool,
) {
    let problem = roc_types::types::Problem::CyclicAlias(symbol, region, others.clone());
    *typ = Type::Erroneous(problem);

    if report {
        let problem = Problem::CyclicAlias(symbol, region, others);
        env.problems.push(problem);
    }
}

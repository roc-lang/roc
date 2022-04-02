use crate::annotation::canonicalize_annotation;
use crate::annotation::IntroducedVariables;
use crate::env::Env;
use crate::expr::ClosureData;
use crate::expr::Expr::{self, *};
use crate::expr::{canonicalize_expr, local_successors_with_duplicates, Output, Recursive};
use crate::pattern::{bindings_from_patterns, canonicalize_pattern, Pattern};
use crate::procedure::References;
use crate::scope::create_alias;
use crate::scope::Scope;
use roc_collections::all::ImSet;
use roc_collections::all::{default_hasher, ImEntry, ImMap, MutMap, MutSet, SendMap};
use roc_error_macros::todo_abilities;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_parse::ast;
use roc_parse::ast::TypeHeader;
use roc_parse::pattern::PatternType;
use roc_problem::can::{CycleEntry, Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::AliasKind;
use roc_types::types::LambdaSet;
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

    /// A structural or opaque type alias, e.g. `Ints : List Int` or `Age := U32` respectively.
    Alias {
        name: Loc<Symbol>,
        vars: Vec<Loc<Lowercase>>,
        ann: &'a Loc<ast::TypeAnnotation<'a>>,
        kind: AliasKind,
    },

    /// An invalid alias, that is ignored in the rest of the pipeline
    /// e.g. a shadowed alias, or a definition like `MyAlias 1 : Int`
    /// with an incorrect pattern
    InvalidAlias { kind: AliasKind },
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

/// Returns a topologically sorted sequence of alias/opaque names
fn sort_type_defs_before_introduction(
    mut referenced_symbols: MutMap<Symbol, Vec<Symbol>>,
) -> Vec<Symbol> {
    let defined_symbols: Vec<Symbol> = referenced_symbols.keys().copied().collect();

    // find the strongly connected components and their relations
    let sccs = {
        // only retain symbols from the current set of defined symbols; the rest come from other modules
        for v in referenced_symbols.iter_mut() {
            v.1.retain(|x| defined_symbols.iter().any(|s| s == x));
        }

        let all_successors_with_self = |symbol: &Symbol| referenced_symbols[symbol].iter().copied();

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
            let reachable = &referenced_symbols[s];
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
            .flat_map(|group_index| sccs[*group_index].iter())
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

                        // Type definitions aren't value definitions, so we don't need to do
                        // anything for them here.
                        PendingDef::Alias { .. } | PendingDef::InvalidAlias { .. } => {}
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
    let mut referenced_type_symbols = MutMap::default();

    for pending_def in pending.into_iter() {
        match pending_def {
            PendingDef::Alias {
                name,
                vars,
                ann,
                kind,
            } => {
                let referenced_symbols = crate::annotation::find_type_def_symbols(
                    env.home,
                    &mut env.ident_ids,
                    &ann.value,
                );

                referenced_type_symbols.insert(name.value, referenced_symbols);

                alias_defs.insert(name.value, (name, vars, ann, kind));
            }
            other => value_defs.push(other),
        }
    }

    let sorted = sort_type_defs_before_introduction(referenced_type_symbols);

    for type_name in sorted {
        let (name, vars, ann, kind) = alias_defs.remove(&type_name).unwrap();

        let symbol = name.value;
        let can_ann = canonicalize_annotation(env, &mut scope, &ann.value, ann.region, var_store);

        // Record all the annotation's references in output.references.lookups
        for symbol in can_ann.references {
            output.references.type_lookups.insert(symbol);
            output.references.referenced_type_defs.insert(symbol);
        }

        let mut can_vars: Vec<Loc<(Lowercase, Variable)>> = Vec::with_capacity(vars.len());
        let mut is_phantom = false;

        let mut named = can_ann.introduced_variables.named;
        for loc_lowercase in vars.iter() {
            match named.iter().position(|nv| nv.name == loc_lowercase.value) {
                Some(index) => {
                    // This is a valid lowercase rigid var for the type def.
                    let named_variable = named.swap_remove(index);

                    can_vars.push(Loc {
                        value: (named_variable.name, named_variable.variable),
                        region: loc_lowercase.region,
                    });
                }
                None => {
                    is_phantom = true;

                    env.problems.push(Problem::PhantomTypeArgument {
                        typ: symbol,
                        variable_region: loc_lowercase.region,
                        variable_name: loc_lowercase.value.clone(),
                    });
                }
            }
        }

        if is_phantom {
            // Bail out
            continue;
        }

        let IntroducedVariables {
            wildcards,
            inferred,
            ..
        } = can_ann.introduced_variables;
        let num_unbound = named.len() + wildcards.len() + inferred.len();
        if num_unbound > 0 {
            let one_occurrence = named
                .iter()
                .map(|nv| Loc::at(nv.first_seen, nv.variable))
                .chain(wildcards)
                .chain(inferred)
                .next()
                .unwrap()
                .region;

            env.problems.push(Problem::UnboundTypeVariable {
                typ: symbol,
                num_unbound,
                one_occurrence,
                kind,
            });

            // Bail out
            continue;
        }

        let alias = create_alias(
            symbol,
            name.region,
            can_vars.clone(),
            can_ann.typ.clone(),
            kind,
        );
        aliases.insert(symbol, alias.clone());
    }

    // Now that we know the alias dependency graph, we can try to insert recursion variables
    // where aliases are recursive tag unions, or detect illegal recursions.
    let mut aliases = correct_mutual_recursive_type_alias(env, aliases, var_store);
    for (symbol, alias) in aliases.iter() {
        scope.add_alias(
            *symbol,
            alias.region,
            alias.type_variables.clone(),
            alias.typ.clone(),
            alias.kind,
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
            // The result needs a thread-safe `SendMap`
            aliases: aliases.into_iter().collect(),
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
        mut can_defs_by_symbol,
        aliases,
    } = defs;

    for (symbol, alias) in aliases.into_iter() {
        output.aliases.insert(symbol, alias);
    }

    let mut defined_symbols: Vec<Symbol> = Vec::new();

    for symbol in can_defs_by_symbol.keys() {
        defined_symbols.push(*symbol);
    }

    // Use topological sort to reorder the defs based on their dependencies to one another.
    // This way, during code gen, no def will refer to a value that hasn't been initialized yet.
    // As a bonus, the topological sort also reveals any cycles between the defs, allowing
    // us to give a CircularAssignment error for invalid (mutual) recursion, and a `DeclareRec` for mutually
    // recursive definitions.

    // All successors that occur in the body of a symbol.
    let all_successors_without_self = |symbol: &Symbol| -> Vec<Symbol> {
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
                let mut loc_succ = local_successors_with_duplicates(references, &env.closures);

                // if the current symbol is a closure, peek into its body
                if let Some(References { value_lookups, .. }) = env.closures.get(symbol) {
                    let home = env.home;

                    for lookup in value_lookups {
                        if lookup != symbol && lookup.module_id() == home {
                            // DO NOT register a self-call behind a lambda!
                            //
                            // We allow `boom = \_ -> boom {}`, but not `x = x`
                            loc_succ.push(*lookup);
                        }
                    }
                }

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols.contains(key));

                loc_succ.sort();
                loc_succ.dedup();

                loc_succ
            }
            None => vec![],
        }
    };

    // All successors that occur in the body of a symbol, including the symbol itself
    // This is required to determine whether a symbol is recursive. Recursive symbols
    // (that are not faulty) always need a DeclareRec, even if there is just one symbol in the
    // group
    let mut all_successors_with_self = |symbol: &Symbol| -> Vec<Symbol> {
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
                let mut loc_succ = local_successors_with_duplicates(references, &env.closures);

                // if the current symbol is a closure, peek into its body
                if let Some(References { value_lookups, .. }) = env.closures.get(symbol) {
                    for lookup in value_lookups {
                        loc_succ.push(*lookup);
                    }
                }

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols.contains(key));

                loc_succ.sort();
                loc_succ.dedup();

                loc_succ
            }
            None => vec![],
        }
    };

    // If a symbol is a direct successor of itself, there is an invalid cycle.
    // The difference with the function above is that this one does not look behind lambdas,
    // but does consider direct self-recursion.
    let direct_successors = |symbol: &Symbol| -> Vec<Symbol> {
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                let mut loc_succ = local_successors_with_duplicates(references, &env.closures);

                // NOTE: if the symbol is a closure we DONT look into its body

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols.contains(key));

                // NOTE: direct recursion does matter here: `x = x` is invalid recursion!

                loc_succ.sort();
                loc_succ.dedup();

                loc_succ
            }
            None => vec![],
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
                    &mut can_defs_by_symbol,
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
                                &mut can_defs_by_symbol,
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
    successors: &mut dyn FnMut(&Symbol) -> Vec<Symbol>,
    can_defs_by_symbol: &mut MutMap<Symbol, Def>,
    declarations: &mut Vec<Declaration>,
) {
    use Declaration::*;

    // We want only successors in the current group, otherwise definitions get duplicated
    let filtered_successors = |symbol: &Symbol| -> Vec<Symbol> {
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
    let mut seen_pattern_regions: Vec<Region> = Vec::with_capacity(2);

    for cycle in strongly_connected_components(group, filtered_successors) {
        if cycle.len() == 1 {
            let symbol = &cycle[0];

            match can_defs_by_symbol.remove(symbol) {
                Some(mut new_def) => {
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
                        seen_pattern_regions.push(new_def.loc_pattern.region);

                        if is_recursive {
                            declarations.push(DeclareRec(vec![new_def]));
                        } else {
                            declarations.push(Declare(new_def));
                        }
                    }
                }
                None => roc_error_macros::internal_error!("def not available {:?}", symbol),
            }
        } else {
            let mut can_defs = Vec::new();

            // Topological sort gives us the reverse of the sorting we want!
            for symbol in cycle.into_iter().rev() {
                match can_defs_by_symbol.remove(&symbol) {
                    Some(mut new_def) => {
                        // Determine recursivity of closures that are not tail-recursive
                        if let Closure(ClosureData {
                            recursive: recursive @ Recursive::NotRecursive,
                            ..
                        }) = &mut new_def.loc_expr.value
                        {
                            *recursive = closure_recursivity(symbol, closures);
                        }

                        if !seen_pattern_regions.contains(&new_def.loc_pattern.region) {
                            seen_pattern_regions.push(new_def.loc_pattern.region);

                            can_defs.push(new_def);
                        }
                    }
                    None => roc_error_macros::internal_error!("def not available {:?}", symbol),
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

        UnwrappedOpaque {
            argument, opaque, ..
        } => {
            let (var, nested) = &**argument;
            pattern_to_vars_by_symbol(vars_by_symbol, &nested.value, *var);
            vars_by_symbol.insert(*opaque, expr_var);
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
        | SingleQuote(_)
        | Underscore
        | MalformedPattern(_, _)
        | UnsupportedPattern(_)
        | OpaqueNotInScope(..) => {}
    }
}

fn single_can_def(
    loc_can_pattern: Loc<Pattern>,
    loc_can_expr: Loc<Expr>,
    expr_var: Variable,
    opt_loc_annotation: Option<Loc<crate::annotation::Annotation>>,
    pattern_vars: SendMap<Symbol, Variable>,
) -> Def {
    let def_annotation = opt_loc_annotation.map(|loc_annotation| Annotation {
        signature: loc_annotation.value.typ,
        introduced_variables: loc_annotation.value.introduced_variables,
        aliases: loc_annotation.value.aliases,
        region: loc_annotation.region,
    });

    Def {
        expr_var,
        loc_pattern: loc_can_pattern,
        loc_expr: Loc {
            region: loc_can_expr.region,
            value: loc_can_expr.value,
        },
        pattern_vars,
        annotation: def_annotation,
    }
}

fn add_annotation_aliases(
    type_annotation: &crate::annotation::Annotation,
    aliases: &mut ImMap<Symbol, Alias>,
) {
    for (name, alias) in type_annotation.aliases.iter() {
        match aliases.entry(*name) {
            ImEntry::Occupied(_) => {
                // do nothing
            }
            ImEntry::Vacant(vacant) => {
                vacant.insert(alias.clone());
            }
        }
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
    aliases: &mut ImMap<Symbol, Alias>,
) -> Output {
    use PendingDef::*;

    // Make types for the body expr, even if we won't end up having a body.
    let expr_var = var_store.fresh();
    let mut vars_by_symbol = SendMap::default();

    match pending_def {
        AnnotationOnly(_, loc_can_pattern, loc_ann) => {
            // annotation sans body cannot introduce new rigids that are visible in other annotations
            // but the rigids can show up in type error messages, so still register them
            let type_annotation =
                canonicalize_annotation(env, scope, &loc_ann.value, loc_ann.region, var_store);

            // Record all the annotation's references in output.references.lookups

            for symbol in type_annotation.references.iter() {
                output.references.type_lookups.insert(*symbol);
                output.references.referenced_type_defs.insert(*symbol);
            }

            add_annotation_aliases(&type_annotation, aliases);

            output
                .introduced_variables
                .union(&type_annotation.introduced_variables);

            pattern_to_vars_by_symbol(&mut vars_by_symbol, &loc_can_pattern.value, expr_var);

            let arity = type_annotation.typ.arity();

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

            if let Pattern::Identifier(symbol) = loc_can_pattern.value {
                let def = single_can_def(
                    loc_can_pattern,
                    loc_can_expr,
                    expr_var,
                    Some(Loc::at(loc_ann.region, type_annotation)),
                    vars_by_symbol.clone(),
                );
                can_defs_by_symbol.insert(symbol, def);
            } else {
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
                                signature: type_annotation.typ.clone(),
                                introduced_variables: output.introduced_variables.clone(),
                                aliases: type_annotation.aliases.clone(),
                                region: loc_ann.region,
                            }),
                        },
                    );
                }
            }
        }

        Alias { .. } => unreachable!("Aliases are handled in a separate pass"),

        InvalidAlias { .. } => {
            // invalid aliases and opaques (shadowed, incorrect patterns) get ignored
        }
        TypedBody(_loc_pattern, loc_can_pattern, loc_ann, loc_expr) => {
            let type_annotation =
                canonicalize_annotation(env, scope, &loc_ann.value, loc_ann.region, var_store);

            // Record all the annotation's references in output.references.lookups
            for symbol in type_annotation.references.iter() {
                output.references.type_lookups.insert(*symbol);
                output.references.referenced_type_defs.insert(*symbol);
            }

            add_annotation_aliases(&type_annotation, aliases);

            output
                .introduced_variables
                .union(&type_annotation.introduced_variables);

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

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            if let Pattern::Identifier(symbol) = loc_can_pattern.value {
                if let Closure(ClosureData {
                    function_type,
                    closure_type,
                    closure_ext_var,
                    return_type,
                    name: ref closure_name,
                    ref arguments,
                    loc_body: ref body,
                    ref captured_symbols,
                    ..
                }) = loc_can_expr.value
                {
                    // Since everywhere in the code it'll be referred to by its defined name,
                    // remove its generated name from the closure map. (We'll re-insert it later.)
                    let references = env.closures.remove(closure_name).unwrap_or_else(|| {
                        panic!(
                            "Tried to remove symbol {:?} from procedures, but it was not found: {:?}",
                            closure_name, env.closures
                        )
                    });

                    // Re-insert the closure into the map, under its defined name.
                    // closures don't have a name, and therefore pick a fresh symbol. But in this
                    // case, the closure has a proper name (e.g. `foo` in `foo = \x y -> ...`
                    // and we want to reference it by that name.
                    env.closures.insert(symbol, references);

                    // The closure is self tail recursive iff it tail calls itself (by defined name).
                    let is_recursive = match can_output.tail_call {
                        Some(tail_symbol) if tail_symbol == symbol => Recursive::TailRecursive,
                        _ => Recursive::NotRecursive,
                    };

                    // Recursion doesn't count as referencing. (If it did, all recursive functions
                    // would result in circular def errors!)
                    refs_by_symbol.entry(symbol).and_modify(|(_, refs)| {
                        refs.value_lookups = refs.value_lookups.without(&symbol);
                    });

                    // renamed_closure_def = Some(&symbol);
                    loc_can_expr.value = Closure(ClosureData {
                        function_type,
                        closure_type,
                        closure_ext_var,
                        return_type,
                        name: symbol,
                        captured_symbols: captured_symbols.clone(),
                        recursive: is_recursive,
                        arguments: arguments.clone(),
                        loc_body: body.clone(),
                    });

                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    let refs = References::new();

                    refs_by_symbol.insert(symbol, (loc_can_pattern.region, refs));
                } else {
                    let refs = can_output.references;
                    refs_by_symbol.insert(symbol, (loc_ann.region, refs));
                }

                let def = single_can_def(
                    loc_can_pattern,
                    loc_can_expr,
                    expr_var,
                    Some(Loc::at(loc_ann.region, type_annotation)),
                    vars_by_symbol.clone(),
                );
                can_defs_by_symbol.insert(symbol, def);
            } else {
                for (_, (symbol, region)) in scope.idents() {
                    if !vars_by_symbol.contains_key(symbol) {
                        continue;
                    }

                    let refs = can_output.references.clone();

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
                                signature: type_annotation.typ.clone(),
                                introduced_variables: type_annotation.introduced_variables.clone(),
                                aliases: type_annotation.aliases.clone(),
                                region: loc_ann.region,
                            }),
                        },
                    );
                }
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

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            if let Pattern::Identifier(symbol) = loc_can_pattern.value {
                if let Closure(ClosureData {
                    function_type,
                    closure_type,
                    closure_ext_var,
                    return_type,
                    name: ref closure_name,
                    ref arguments,
                    loc_body: ref body,
                    ref captured_symbols,
                    ..
                }) = loc_can_expr.value
                {
                    // Since everywhere in the code it'll be referred to by its defined name,
                    // remove its generated name from the closure map. (We'll re-insert it later.)
                    let references = env.closures.remove(closure_name).unwrap_or_else(|| {
                        panic!(
                            "Tried to remove symbol {:?} from procedures, but it was not found: {:?}",
                            closure_name, env.closures
                        )
                    });

                    // Re-insert the closure into the map, under its defined name.
                    // closures don't have a name, and therefore pick a fresh symbol. But in this
                    // case, the closure has a proper name (e.g. `foo` in `foo = \x y -> ...`
                    // and we want to reference it by that name.
                    env.closures.insert(symbol, references);

                    // The closure is self tail recursive iff it tail calls itself (by defined name).
                    let is_recursive = match can_output.tail_call {
                        Some(tail_symbol) if tail_symbol == symbol => Recursive::TailRecursive,
                        _ => Recursive::NotRecursive,
                    };

                    // Recursion doesn't count as referencing. (If it did, all recursive functions
                    // would result in circular def errors!)
                    refs_by_symbol.entry(symbol).and_modify(|(_, refs)| {
                        refs.value_lookups = refs.value_lookups.without(&symbol);
                    });

                    loc_can_expr.value = Closure(ClosureData {
                        function_type,
                        closure_type,
                        closure_ext_var,
                        return_type,
                        name: symbol,
                        captured_symbols: captured_symbols.clone(),
                        recursive: is_recursive,
                        arguments: arguments.clone(),
                        loc_body: body.clone(),
                    });

                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    let refs = References::new();
                    refs_by_symbol.insert(symbol, (loc_pattern.region, refs));
                } else {
                    let refs = can_output.references.clone();
                    refs_by_symbol.insert(symbol, (loc_pattern.region, refs));
                }

                let def = single_can_def(
                    loc_can_pattern,
                    loc_can_expr,
                    expr_var,
                    None,
                    vars_by_symbol.clone(),
                );
                can_defs_by_symbol.insert(symbol, def);
            } else {
                // Store the referenced locals in the refs_by_symbol map, so we can later figure out
                // which defined names reference each other.
                for (symbol, region) in bindings_from_patterns(std::iter::once(&loc_can_pattern)) {
                    let refs = can_output.references.clone();
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
        if !output.references.has_value_lookup(symbol) && !output.references.has_type_lookup(symbol)
        {
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
            header: TypeHeader { name, vars },
            ann,
        }
        | Opaque {
            header: TypeHeader { name, vars },
            typ: ann,
        } => {
            let kind = if matches!(def, Alias { .. }) {
                AliasKind::Structural
            } else {
                AliasKind::Opaque
            };

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
                                let problem = Problem::InvalidAliasRigid {
                                    alias_name: symbol,
                                    region: loc_var.region,
                                };
                                env.problems.push(problem);

                                return Some((
                                    Output::default(),
                                    PendingDef::InvalidAlias { kind },
                                ));
                            }
                        }
                    }

                    let name = Loc {
                        region: name.region,
                        value: symbol,
                    };

                    let pending_def = PendingDef::Alias {
                        name,
                        vars: can_rigids,
                        ann,
                        kind,
                    };

                    Some((Output::default(), pending_def))
                }

                Err((original_region, loc_shadowed_symbol, _new_symbol)) => {
                    env.problem(Problem::ShadowingInAnnotation {
                        original_region,
                        shadow: loc_shadowed_symbol,
                    });

                    Some((Output::default(), PendingDef::InvalidAlias { kind }))
                }
            }
        }

        Ability { .. } => todo_abilities!(),

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
    mut original_aliases: SendMap<Symbol, Alias>,
    var_store: &mut VarStore,
) -> ImMap<Symbol, Alias> {
    let symbols_introduced: Vec<Symbol> = original_aliases.keys().copied().collect();

    let all_successors_with_self = |symbol: &Symbol| -> Vec<Symbol> {
        match original_aliases.get(symbol) {
            Some(alias) => {
                let mut loc_succ = alias.typ.symbols();
                // remove anything that is not defined in the current block
                loc_succ.retain(|key| symbols_introduced.contains(key));

                loc_succ
            }
            None => vec![],
        }
    };

    // TODO investigate should this be in a loop?
    let defined_symbols: Vec<Symbol> = original_aliases.keys().copied().collect();

    let cycles = strongly_connected_components(&defined_symbols, all_successors_with_self);
    let mut solved_aliases = ImMap::default();

    for cycle in cycles {
        debug_assert!(!cycle.is_empty());

        let mut pending_aliases: ImMap<_, _> = cycle
            .iter()
            .map(|&sym| (sym, original_aliases.remove(&sym).unwrap()))
            .collect();

        // Make sure we report only one error for the cycle, not an error for every
        // alias in the cycle.
        let mut can_still_report_error = true;

        // We need to instantiate the alias with any symbols in the currrent module it
        // depends on.
        // We only need to worry about symbols in this SCC or any prior one, since the SCCs
        // were sorted topologically, and we've already instantiated aliases coming from other
        // modules.
        // NB: ImMap::clone is O(1): https://docs.rs/im/latest/src/im/hash/map.rs.html#1527-1544
        let mut to_instantiate = solved_aliases.clone().union(pending_aliases.clone());

        for &rec in cycle.iter() {
            let alias = pending_aliases.get_mut(&rec).unwrap();
            // Don't try to instantiate the alias itself in its definition.
            let original_alias_def = to_instantiate.remove(&rec).unwrap();

            let mut new_lambda_sets = ImSet::default();
            alias.typ.instantiate_aliases(
                alias.region,
                &to_instantiate,
                var_store,
                &mut new_lambda_sets,
            );

            for lambda_set_var in new_lambda_sets {
                alias
                    .lambda_set_variables
                    .push(LambdaSet(Type::Variable(lambda_set_var)));
            }

            to_instantiate.insert(rec, original_alias_def);

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

    let made_recursive = make_tag_union_recursive_help(
        env,
        Loc::at(alias.header_region(), (alias_name, &alias_args)),
        alias.region,
        others,
        &mut alias.typ,
        var_store,
        can_report_error,
    );

    match made_recursive {
        MakeTagUnionRecursive::Cyclic => Ok(()),
        MakeTagUnionRecursive::MadeRecursive { recursion_variable } => {
            alias.recursion_variables.clear();
            alias.recursion_variables.insert(recursion_variable);

            Ok(())
        }
        MakeTagUnionRecursive::InvalidRecursion => Err(()),
    }
}

enum MakeTagUnionRecursive {
    Cyclic,
    MadeRecursive { recursion_variable: Variable },
    InvalidRecursion,
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
) -> MakeTagUnionRecursive {
    use MakeTagUnionRecursive::*;

    let Loc {
        value: (symbol, args),
        region: alias_region,
    } = recursive_alias;
    let vars = args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
    match typ {
        Type::TagUnion(tags, ext) => {
            let recursion_variable = var_store.fresh();
            let mut pending_typ =
                Type::RecursiveTagUnion(recursion_variable, tags.to_vec(), ext.clone());
            let substitution_result =
                pending_typ.substitute_alias(symbol, &vars, &Type::Variable(recursion_variable));
            match substitution_result {
                Ok(()) => {
                    // We can substitute the alias presence for the variable exactly.
                    *typ = pending_typ;

                    MadeRecursive { recursion_variable }
                }
                Err(differing_recursion_region) => {
                    env.problems.push(Problem::NestedDatatype {
                        alias: symbol,
                        def_region: alias_region,
                        differing_recursion_region,
                    });

                    InvalidRecursion
                }
            }
        }
        Type::RecursiveTagUnion(recursion_variable, _, _) => MadeRecursive {
            recursion_variable: *recursion_variable,
        },
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

            Cyclic
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

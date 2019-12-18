use crate::can::annotation::canonicalize_annotation;
use crate::can::env::Env;
use crate::can::expr::Expr::{self, *};
use crate::can::expr::{
    canonicalize_expr, local_successors, references_from_call, references_from_local, union_pairs,
    Output, Recursive, Rigids,
};
use crate::can::pattern::PatternType::*;
use crate::can::pattern::{canonicalize_pattern, idents_from_patterns, Pattern};
use crate::can::pattern::{remove_idents, PatternState};
use crate::can::problem::Problem;
use crate::can::problem::RuntimeError;
use crate::can::problem::RuntimeError::*;
use crate::can::procedure::References;
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, ImSet, MutMap, MutSet, SendMap};
use crate::graph::{strongly_connected_component, topological_sort};
use crate::ident::Ident;
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::Type;
use crate::types::{LetConstraint, PExpected};
use im_rc::Vector;
use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq)]
pub struct Def {
    pub pattern: Located<Pattern>,
    pub expr: Located<Expr>,
    pub variables_by_symbol: SendMap<Symbol, Variable>,
}

#[derive(Debug)]
pub struct CanDefs {
    pub refs_by_symbol: MutMap<Symbol, (Located<Ident>, References)>,
    pub can_defs_by_symbol: MutMap<Symbol, Def>,
    pub defined_idents: Vector<(Ident, (Symbol, Region))>,
}

#[derive(Default)]
pub struct Info {
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub def_types: SendMap<Symbol, Located<Type>>,
}

impl Info {
    pub fn with_capacity(capacity: usize) -> Self {
        Info {
            vars: Vec::with_capacity(capacity),
            constraints: Vec::with_capacity(capacity),
            def_types: SendMap::default(),
        }
    }
}

#[inline(always)]
pub fn canonicalize_defs<'a>(
    rigids: &Rigids,
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    loc_defs: &'a bumpalo::collections::Vec<'a, &'a Located<ast::Def<'a>>>,
    flex_info: &mut Info,
) -> CanDefs {
    use crate::parse::ast::Def::*;

    let mut refs_by_symbol = MutMap::default();
    let mut can_defs_by_symbol = MutMap::default();

    // Add the defined identifiers to scope. If there's a collision, it means there
    // was shadowing, which will be handled later.
    let defined_idents: Vector<(Ident, (Symbol, Region))> = idents_from_patterns(
        loc_defs
            .iter()
            .flat_map(|loc_def| pattern_from_def(&loc_def.value)),
        &scope,
    );

    scope.idents = union_pairs(scope.idents.clone(), defined_idents.iter());

    let mut it = loc_defs.iter().peekable();

    while let Some(loc_def) = it.next() {
        match &loc_def.value {
            Annotation(pattern, annotation) => match it.peek().map(|v| v.value.clone()) {
                Some(Body(body_pattern, body_expr)) if pattern == body_pattern => {
                    it.next();

                    let typed = TypedDef(body_pattern, annotation.clone(), body_expr);
                    canonicalize_def(
                        rigids,
                        env,
                        Located {
                            region: loc_def.region,
                            value: &typed,
                        },
                        scope,
                        &mut can_defs_by_symbol,
                        flex_info,
                        var_store,
                        &mut refs_by_symbol,
                    );
                }
                _ => {
                    canonicalize_def(
                        rigids,
                        env,
                        Located {
                            region: loc_def.region,
                            value: &loc_def.value,
                        },
                        scope,
                        &mut can_defs_by_symbol,
                        flex_info,
                        var_store,
                        &mut refs_by_symbol,
                    );
                }
            },

            Nested(Annotation(pattern, annotation)) => match it.peek().map(|v| v.value.clone()) {
                Some(Body(body_pattern, body_expr)) if pattern.value == body_pattern.value => {
                    it.next();

                    let typed = TypedDef(body_pattern, annotation.clone(), body_expr);
                    canonicalize_def(
                        rigids,
                        env,
                        Located {
                            region: loc_def.region,
                            value: &typed,
                        },
                        scope,
                        &mut can_defs_by_symbol,
                        flex_info,
                        var_store,
                        &mut refs_by_symbol,
                    );
                }
                _ => {
                    canonicalize_def(
                        rigids,
                        env,
                        Located {
                            region: loc_def.region,
                            value: &loc_def.value,
                        },
                        scope,
                        &mut can_defs_by_symbol,
                        flex_info,
                        var_store,
                        &mut refs_by_symbol,
                    );
                }
            },

            _ => {
                canonicalize_def(
                    rigids,
                    env,
                    Located {
                        region: loc_def.region,
                        value: &loc_def.value,
                    },
                    scope,
                    &mut can_defs_by_symbol,
                    flex_info,
                    var_store,
                    &mut refs_by_symbol,
                );
            }
        }
    }

    CanDefs {
        defined_idents,
        refs_by_symbol,
        can_defs_by_symbol,
    }
}

#[inline(always)]
pub fn sort_can_defs(
    env: &mut Env,
    defs: CanDefs,
    mut output: Output,
) -> (Result<Vec<Def>, RuntimeError>, Output) {
    let CanDefs {
        defined_idents,
        refs_by_symbol,
        can_defs_by_symbol,
    } = defs;

    // Determine the full set of references by traversing the graph.
    let mut visited_symbols = MutSet::default();

    let returned_locals = ImSet::clone(&output.references.locals);
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
    for symbol in returned_locals.into_iter() {
        // Traverse the graph and look up *all* the references for this local symbol.
        let refs =
            references_from_local(symbol, &mut visited_symbols, &refs_by_symbol, &env.closures);

        output.references = output.references.union(refs);
    }

    for symbol in ImSet::clone(&output.references.calls).into_iter() {
        // Traverse the graph and look up *all* the references for this call.
        // Reuse the same visited_symbols as before; if we already visited it, we
        // won't learn anything new from visiting it again!
        let refs =
            references_from_call(symbol, &mut visited_symbols, &refs_by_symbol, &env.closures);

        output.references = output.references.union(refs);
    }

    // Now that we've collected all the references, check to see if any of the new idents
    // we defined went unused by the return expression. If any were unused, report it.
    for (ident, (symbol, region)) in Vector::clone(&defined_idents) {
        if !output.references.has_local(&symbol) {
            let loc_ident = Located {
                region,
                value: ident.clone(),
            };

            env.problem(Problem::UnusedAssignment(loc_ident));
        }
    }

    // Use topological sort to reorder the defs based on their dependencies to one another.
    // This way, during code gen, no def will refer to a value that hasn't been initialized yet.
    // As a bonus, the topological sort also reveals any cycles between the defs, allowing
    // us to give a CircularAssignment error.
    let successors = |symbol: &Symbol| -> ImSet<Symbol> {
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
            Some((_, references)) => local_successors(&references, &env.closures),
            None => ImSet::default(),
        }
    };

    let mut defined_symbols: Vec<Symbol> = Vec::new();

    for symbol in can_defs_by_symbol.keys().into_iter() {
        defined_symbols.push(symbol.clone())
    }

    // TODO also do the same `addDirects` check elm/compiler does, so we can
    // report an error if a recursive definition can't possibly terminate!
    match topological_sort(defined_symbols.as_slice(), successors) {
        Ok(sorted_symbols) => {
            let mut can_defs = Vec::new();

            for symbol in sorted_symbols
                .into_iter()
                // Topological sort gives us the reverse of the sorting we want!
                .rev()
            {
                if let Some(can_def) = can_defs_by_symbol.get(&symbol) {
                    let mut new_def = can_def.clone();

                    // Determine recursivity of closures that are not tail-recursive
                    if let Closure(name, Recursive::NotRecursive, args, body) = new_def.expr.value {
                        let recursion = closure_recursivity(symbol.clone(), &env.closures);

                        new_def.expr.value = Closure(name, recursion, args, body);
                    }

                    can_defs.push(new_def);
                }
            }

            (Ok(can_defs), output)
        }
        Err(node_in_cycle) => {
            // We have one node we know is in the cycle.
            // We want to show the entire cycle in the error message, so expand it out.
            let mut loc_idents_in_cycle: Vec<Located<Ident>> = Vec::new();

            for symbol in strongly_connected_component(&node_in_cycle, successors)
                .into_iter()
                // Strongly connected component gives us the reverse of the sorting we want!
                .rev()
            {
                let refs = refs_by_symbol.get(&symbol).unwrap_or_else(|| {
                    panic!(
                        "Symbol not found in refs_by_symbol: {:?} - refs_by_symbol was: {:?}",
                        symbol, refs_by_symbol
                    )
                });

                loc_idents_in_cycle.push(refs.0.clone());
            }

            // Sort them to make the report more helpful.
            loc_idents_in_cycle = sort_cyclic_idents(
                loc_idents_in_cycle,
                &mut defined_idents.iter().map(|(ident, _)| ident),
            );

            env.problem(Problem::CircularAssignment(loc_idents_in_cycle.clone()));

            let mut regions = Vec::with_capacity(can_defs_by_symbol.len());

            for def in can_defs_by_symbol.values() {
                regions.push((def.pattern.region, def.expr.region));
            }

            (
                Err(RuntimeError::CircularDef(loc_idents_in_cycle, regions)),
                output,
            )
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn canonicalize_def<'a>(
    rigids: &Rigids,
    env: &mut Env,
    loc_def: Located<&'a ast::Def<'a>>,
    scope: &mut Scope,
    can_defs_by_symbol: &mut MutMap<Symbol, Def>,
    flex_info: &mut Info,
    var_store: &VarStore,
    refs_by_symbol: &mut MutMap<Symbol, (Located<Ident>, References)>,
) {
    use crate::parse::ast::Def::*;
    use crate::types::AnnotationSource;

    // Make types for the body expr, even if we won't end up having a body.
    let expr_var = var_store.fresh();
    let expr_type = Type::Variable(expr_var);
    let mut variables_by_symbol = SendMap::default();

    // Each def gets to have all the idents in scope that are defined in this
    // block. Order of defs doesn't matter, thanks to referential transparency!
    let (_opt_loc_pattern, (_loc_can_expr, _can_output)) = match loc_def.value {
        Annotation(_loc_pattern, loc_annotation) => {
            // TODO implement this:
            //
            // Is this a standalone annotation, or is it annotating the
            // next def? This is annotating the next def iff:
            //
            // 1. There is a next def.
            // 2. It is a Def::Body.
            // 3. Its Pattern contains at least one SpaceBefore.
            // 4. The count of all Newlines across all of its SpaceBefores is exactly 1.
            //
            // This tells us we're an annotation in the following scenario:
            //
            // foo : String
            // foo = "blah"
            //
            // Knowing that, we then need to incorporate the annotation's type constraints
            // into the next def's. To do this, we extract the next def from the iterator
            // immediately, then canonicalize it to get its Variable, then use that
            // Variable to generate the extra constraints.

            let value = Expr::RuntimeError(NoImplementation);
            let loc_expr = Located {
                value,
                region: loc_annotation.region,
            };

            (None, (loc_expr, Output::default()))
        }

        TypedDef(loc_pattern, loc_annotation, loc_expr) => {
            // Exclude the current ident from shadowable_idents; you can't shadow yourself!
            // (However, still include it in scope, because you *can* recursively refer to yourself.)
            let mut shadowable_idents = scope.idents.clone();
            remove_idents(&loc_pattern.value, &mut shadowable_idents);

            let pattern_var = var_store.fresh();
            let pattern_type = Type::Variable(pattern_var);
            let pattern_expected = PExpected::NoExpectation(pattern_type);

            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(1),
                constraints: Vec::with_capacity(1),
            };

            let loc_can_pattern = canonicalize_pattern(
                env,
                &mut state,
                var_store,
                scope,
                Assignment,
                &loc_pattern.value,
                loc_pattern.region,
                &mut shadowable_idents,
                pattern_expected,
            );

            flex_info.vars.push(pattern_var);

            // Any time there's a lookup on this symbol in the outer Let,
            // it should result in this expression's type. After all, this
            // is the type to which this symbol is defined!
            add_pattern_to_lookup_types(
                &scope,
                &loc_pattern,
                &mut flex_info.def_types,
                expr_type.clone(),
            );

            // bookkeeping for tail-call detection. If we're assigning to an
            // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
            let outer_identifier = env.tailcallable_symbol.clone();

            // TODO ensure TypedDef has a pattern identifier?
            // e.g. in elm, you can't type
            //
            // (foo, bar) : (Int, Bool)
            // implicitly, that is the case here too
            let mut fname = "invalid name".to_string();

            if let (
                &ast::Pattern::Identifier(ref name),
                &Pattern::Identifier(_, ref defined_symbol),
            ) = (&loc_pattern.value, &loc_can_pattern.value)
            {
                fname = name.to_string();
                env.tailcallable_symbol = Some(defined_symbol.clone());
                variables_by_symbol.insert(defined_symbol.clone(), expr_var);
            };

            let (ftv_sendmap, can_annotation) =
                canonicalize_annotation(&loc_annotation.value, var_store);

            let mut ftv = ImMap::default();

            for (k, v) in ftv_sendmap {
                ftv.insert(k.into(), Type::Variable(v));
            }

            // remove the known type variables (TODO can clone be prevented?)
            let new_rigids = ftv.difference(rigids.clone());

            let new_rtv = rigids.clone().union(new_rigids);

            let arity = if let crate::types::Type::Function(args, _) = &can_annotation {
                args.len()
            } else {
                0
            };

            let annotation_expected =
                FromAnnotation(fname, arity, AnnotationSource::TypedBody, can_annotation);

            let (mut loc_can_expr, can_output, ret_constraint) = canonicalize_expr(
                // rigids,
                &new_rtv,
                env,
                var_store,
                scope,
                loc_expr.region,
                &loc_expr.value,
                annotation_expected.clone(),
            );

            // ensure expected type unifies with annotated type
            state
                .constraints
                .push(Eq(expr_type.clone(), annotation_expected, loc_def.region));

            // reset the tailcallable_symbol
            env.tailcallable_symbol = outer_identifier;

            flex_info.constraints.push(Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: state.vars,
                def_types: state.headers,
                defs_constraint: And(state.constraints),
                ret_constraint,
            })));

            // see below: a closure needs a fresh References!
            let mut is_closure = false;

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            if let (
                &ast::Pattern::Identifier(ref _name),
                &Pattern::Identifier(_, ref defined_symbol),
                &Closure(ref symbol, _, ref arguments, ref body),
            ) = (
                &loc_pattern.value,
                &loc_can_pattern.value,
                &loc_can_expr.value.clone(),
            ) {
                is_closure = true;

                // Since everywhere in the code it'll be referred to by its defined name,
                // remove its generated name from the closure map. (We'll re-insert it later.)
                let references = env.closures.remove(&symbol).unwrap_or_else(|| {
                    panic!(
                        "Tried to remove symbol {:?} from procedures, but it was not found: {:?}",
                        symbol, env.closures
                    )
                });

                // Re-insert the closure into the map, under its defined name.
                // closures don't have a name, and therefore pick a fresh symbol. But in this
                // case, the closure has a proper name (e.g. `foo` in `foo = \x y -> ...`
                // and we want to reference it by that name.
                env.closures.insert(defined_symbol.clone(), references);

                // The closure is self tail recursive iff it tail calls itself (by defined name).
                let is_recursive = match can_output.tail_call {
                    Some(ref symbol) if symbol == defined_symbol => Recursive::TailRecursive,
                    _ => Recursive::NotRecursive,
                };

                // Recursion doesn't count as referencing. (If it did, all recursive functions
                // would result in circular def errors!)
                refs_by_symbol
                    .entry(defined_symbol.clone())
                    .and_modify(|(_, refs)| {
                        refs.locals = refs.locals.without(defined_symbol);
                    });

                // renamed_closure_def = Some(&defined_symbol);
                loc_can_expr.value = Closure(
                    symbol.clone(),
                    is_recursive,
                    arguments.clone(),
                    body.clone(),
                );
            }

            let mut defined_symbols = Vec::new();

            // Store the referenced locals in the refs_by_symbol map, so we can later figure out
            // which defined names reference each other.
            for (ident, (symbol, region)) in
                idents_from_patterns(std::iter::once(*loc_pattern), &scope)
            {
                let refs =
                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    if is_closure {
                        References::new()
                    } else {
                        can_output.references.clone()
                    };

                refs_by_symbol.insert(
                    symbol.clone(),
                    (
                        Located {
                            value: ident,
                            region,
                        },
                        refs,
                    ),
                );

                defined_symbols.push(symbol.clone());
            }

            for symbol in defined_symbols {
                can_defs_by_symbol.insert(
                    symbol,
                    Def {
                        // TODO try to remove this .clone()!
                        pattern: loc_can_pattern.clone(),
                        expr: Located {
                            region: loc_can_expr.region,
                            // TODO try to remove this .clone()!
                            value: loc_can_expr.value.clone(),
                        },
                        variables_by_symbol: im::HashMap::clone(&variables_by_symbol),
                    },
                );
            }

            (Some(loc_pattern), (loc_can_expr, can_output))
        }
        // If we have a pattern, then the def has a body (that is, it's not a
        // standalone annotation), so we need to canonicalize the pattern and expr.
        Body(loc_pattern, loc_expr) => {
            // Exclude the current ident from shadowable_idents; you can't shadow yourself!
            // (However, still include it in scope, because you *can* recursively refer to yourself.)
            let mut shadowable_idents = scope.idents.clone();
            remove_idents(&loc_pattern.value, &mut shadowable_idents);

            let pattern_var = var_store.fresh();
            let pattern_type = Type::Variable(pattern_var);
            let pattern_expected = PExpected::NoExpectation(pattern_type);

            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(1),
                constraints: Vec::with_capacity(1),
            };

            let loc_can_pattern = canonicalize_pattern(
                env,
                &mut state,
                var_store,
                scope,
                Assignment,
                &loc_pattern.value,
                loc_pattern.region,
                &mut shadowable_idents,
                pattern_expected,
            );

            flex_info.vars.push(pattern_var);

            // Any time there's a lookup on this symbol in the outer Let,
            // it should result in this expression's type. After all, this
            // is the type to which this symbol is defined!
            add_pattern_to_lookup_types(
                &scope,
                &loc_pattern,
                &mut flex_info.def_types,
                expr_type.clone(),
            );

            // bookkeeping for tail-call detection. If we're assigning to an
            // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
            let outer_identifier = env.tailcallable_symbol.clone();

            if let (
                &ast::Pattern::Identifier(ref _name),
                &Pattern::Identifier(_, ref defined_symbol),
            ) = (&loc_pattern.value, &loc_can_pattern.value)
            {
                env.tailcallable_symbol = Some(defined_symbol.clone());
                variables_by_symbol.insert(defined_symbol.clone(), expr_var);
            };

            let (mut loc_can_expr, can_output, ret_constraint) = canonicalize_expr(
                rigids,
                env,
                var_store,
                scope,
                loc_expr.region,
                &loc_expr.value,
                NoExpectation(expr_type.clone()),
            );

            // reset the tailcallable_symbol
            env.tailcallable_symbol = outer_identifier;

            flex_info.constraints.push(Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: state.vars,
                def_types: state.headers,
                defs_constraint: And(state.constraints),
                ret_constraint,
            })));

            // see below: a closure needs a fresh References!
            let mut is_closure = false;

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            if let (
                &ast::Pattern::Identifier(ref _name),
                &Pattern::Identifier(_, ref defined_symbol),
                &Closure(ref symbol, _, ref arguments, ref body),
            ) = (
                &loc_pattern.value,
                &loc_can_pattern.value,
                &loc_can_expr.value.clone(),
            ) {
                is_closure = true;

                // Since everywhere in the code it'll be referred to by its defined name,
                // remove its generated name from the closure map. (We'll re-insert it later.)
                let references = env.closures.remove(&symbol).unwrap_or_else(|| {
                    panic!(
                        "Tried to remove symbol {:?} from procedures, but it was not found: {:?}",
                        symbol, env.closures
                    )
                });

                // Re-insert the closure into the map, under its defined name.
                // closures don't have a name, and therefore pick a fresh symbol. But in this
                // case, the closure has a proper name (e.g. `foo` in `foo = \x y -> ...`
                // and we want to reference it by that name.
                env.closures.insert(defined_symbol.clone(), references);

                // The closure is self tail recursive iff it tail calls itself (by defined name).
                let is_recursive = match can_output.tail_call {
                    Some(ref symbol) if symbol == defined_symbol => Recursive::TailRecursive,
                    _ => Recursive::NotRecursive,
                };

                // Recursion doesn't count as referencing. (If it did, all recursive functions
                // would result in circular def errors!)
                refs_by_symbol
                    .entry(defined_symbol.clone())
                    .and_modify(|(_, refs)| {
                        refs.locals = refs.locals.without(defined_symbol);
                    });

                // renamed_closure_def = Some(&defined_symbol);
                loc_can_expr.value = Closure(
                    symbol.clone(),
                    is_recursive,
                    arguments.clone(),
                    body.clone(),
                );
            }

            let mut defined_symbols = Vec::new();

            // Store the referenced locals in the refs_by_symbol map, so we can later figure out
            // which defined names reference each other.
            for (ident, (symbol, region)) in
                idents_from_patterns(std::iter::once(*loc_pattern), &scope)
            {
                let refs =
                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    if is_closure {
                        References::new()
                    } else {
                        can_output.references.clone()
                    };

                refs_by_symbol.insert(
                    symbol.clone(),
                    (
                        Located {
                            value: ident,
                            region,
                        },
                        refs,
                    ),
                );

                defined_symbols.push(symbol.clone());
            }

            for symbol in defined_symbols {
                can_defs_by_symbol.insert(
                    symbol,
                    Def {
                        // TODO try to remove this .clone()!
                        pattern: loc_can_pattern.clone(),
                        expr: Located {
                            region: loc_can_expr.region,
                            // TODO try to remove this .clone()!
                            value: loc_can_expr.value.clone(),
                        },
                        variables_by_symbol: im::HashMap::clone(&variables_by_symbol),
                    },
                );
            }

            (Some(loc_pattern), (loc_can_expr, can_output))
        }
        Nested(value) => {
            return canonicalize_def(
                rigids,
                env,
                Located {
                    value,
                    region: loc_def.region,
                },
                scope,
                can_defs_by_symbol,
                flex_info,
                var_store,
                refs_by_symbol,
            );
        }
        SpaceBefore(_, _) | SpaceAfter(_, _) => {
            panic!("Somehow a space in a Def was not removed before canonicalization!")
        }
    };
}

/// When we get a list of cyclic idents, the first node listed is a matter of chance.
/// This reorders the list such that the first node listed is always alphabetically the lowest,
/// while preserving the overall order of the cycle.
///
/// Example: the cycle  (c ---> a ---> b)  becomes  (a ---> b ---> c)
pub fn sort_cyclic_idents<'a, I>(
    loc_idents: Vec<Located<Ident>>,
    ordered_idents: &mut I,
) -> Vec<Located<Ident>>
where
    I: Iterator<Item = &'a Ident>,
{
    // Find the first ident in ordered_idents that also appears in loc_idents.
    let first_ident = ordered_idents
        .find(|ident| {
            loc_idents
                .iter()
                .any(|loc_ident| &&loc_ident.value == ident)
        })
        .unwrap_or_else(|| {
            panic!(
                "Could not find any idents that appear in both loc_idents {:?} and ordered_idents",
                loc_idents
            )
        });

    let mut answer = Vec::with_capacity(loc_idents.len());
    let mut end = Vec::with_capacity(loc_idents.len());
    let mut encountered_first_ident = false;

    for loc_ident in loc_idents {
        if encountered_first_ident {
            answer.push(loc_ident);
        } else if &loc_ident.value == first_ident {
            encountered_first_ident = true;

            answer.push(loc_ident);
        } else {
            end.push(loc_ident);
        }
    }

    // Add the contents of `end` to the end of the answer.
    answer.extend_from_slice(end.as_slice());

    answer
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
pub fn can_defs_with_return<'a>(
    rigids: &Rigids,
    env: &mut Env,
    var_store: &VarStore,
    mut scope: Scope,
    loc_defs: &'a bumpalo::collections::Vec<'a, &'a Located<ast::Def<'a>>>,
    expected: Expected<Type>,
    mut flex_info: Info,
    rigid_info: Info,
    loc_ret: &'a Located<ast::Expr<'a>>,
) -> (Expr, Output, Constraint) {
    let unsorted = canonicalize_defs(rigids, env, var_store, &mut scope, loc_defs, &mut flex_info);

    // The def as a whole is a tail call iff its return expression is a tail call.
    // Use its output as a starting point because its tail_call already has the right answer!
    let (ret_expr, output, ret_con) = canonicalize_expr(
        rigids,
        env,
        var_store,
        &mut scope,
        loc_ret.region,
        &loc_ret.value,
        expected,
    );

    let (can_defs, output) = sort_can_defs(env, unsorted, output);

    // Rigid constraint for the def expr as a whole
    let constraint = Let(Box::new(LetConstraint {
        rigid_vars: rigid_info.vars,
        flex_vars: Vec::new(),
        def_types: rigid_info.def_types,
        defs_constraint:
            // Flex constraint
            Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: flex_info.vars,
                def_types: flex_info.def_types.clone(),
                defs_constraint:
                    // Final flex constraints
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: Vec::new(),
                        def_types: flex_info.def_types,
                        defs_constraint: True,
                        ret_constraint: And(flex_info.constraints)
                    })),
                ret_constraint: And(vec![And(rigid_info.constraints), ret_con])
            })),
        ret_constraint: True,
    }));

    match can_defs {
        Ok(defs) => (
            Defs(var_store.fresh(), defs, Box::new(ret_expr)),
            output,
            constraint,
        ),
        Err(err) => (RuntimeError(err), output, constraint),
    }
}

/// This lets us share bound type variables between nested annotations, e.g.
///
/// blah : Map k v -> Int
/// blah mapping =
///     nested : Map k v # <-- the same k and v from the top-level annotation
///     nested = mapping
///     42
///
/// In elm/compiler this is called RTV - the "Rigid Type Variables" dictionary.
/// type BoundTypeVars = ImMap<Box<str>, Type>;
fn add_pattern_to_lookup_types<'a>(
    scope: &Scope,
    loc_pattern: &'a Located<ast::Pattern<'a>>,
    lookup_types: &mut SendMap<Symbol, Located<Type>>,
    expr_type: Type,
) {
    let region = loc_pattern.region;

    match loc_pattern.value {
        ast::Pattern::Identifier(name) => {
            let symbol = scope.symbol(&name);
            let loc_type = Located {
                region,
                value: expr_type,
            };

            lookup_types.insert(symbol, loc_type);
        }
        _ => panic!("TODO constrain patterns other than Identifier"),
    }
}

fn pattern_from_def<'a>(def: &'a ast::Def<'a>) -> Option<&'a Located<ast::Pattern<'a>>> {
    use crate::parse::ast::Def::*;

    match def {
        Annotation(_, _) => None,
        Body(ref loc_pattern, _) => Some(loc_pattern),
        TypedDef(ref loc_pattern, _, _) => Some(loc_pattern),
        SpaceBefore(def, _) | SpaceAfter(def, _) | Nested(def) => pattern_from_def(def),
    }
}

fn closure_recursivity(symbol: Symbol, closures: &MutMap<Symbol, References>) -> Recursive {
    let mut visited = MutSet::default();

    let mut stack = Vec::new();

    if let Some(references) = closures.get(&symbol) {
        for v in &references.calls {
            stack.push(v.clone());
        }

        // while there are symbols left to visit
        while let Some(nested_symbol) = stack.pop() {
            if nested_symbol.clone() == symbol {
                return Recursive::Recursive;
            }

            // if the called symbol not yet in the graph
            if !visited.contains(&nested_symbol) {
                // add it to the visited set
                // if it calls any functions
                if let Some(nested_references) = closures.get(&nested_symbol) {
                    // add its called to the stack
                    for v in &nested_references.calls {
                        stack.push(v.clone());
                    }
                }
                visited.insert(nested_symbol);
            }
        }
    }

    Recursive::NotRecursive
}

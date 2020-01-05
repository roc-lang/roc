use crate::can::def::{can_defs_with_return, Def};
use crate::can::env::Env;
use crate::can::ident::Lowercase;
use crate::can::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_int, float_expr_from_result,
    int_expr_from_result,
};
use crate::can::pattern::idents_from_patterns;
use crate::can::pattern::PatternType::*;
use crate::can::pattern::{canonicalize_pattern, remove_idents, Pattern};
use crate::can::problem::Problem;
use crate::can::problem::RuntimeError;
use crate::can::problem::RuntimeError::*;
use crate::can::procedure::References;
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, ImSet, MutMap, MutSet, SendMap};
use crate::ident::Ident;
use crate::operator::CalledVia;
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use im_rc::Vector;
use std::fmt::Debug;
use std::i64;
use std::ops::Neg;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub rigids: SendMap<Variable, Lowercase>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(Variable, i64),
    Float(Variable, f64),
    Str(Box<str>),
    BlockStr(Box<str>),
    List(Variable, Vec<(Variable, Located<Expr>)>),

    // Lookups
    Var {
        symbol_for_lookup: Symbol,
        resolved_symbol: Symbol,
    },
    // Pattern Matching
    /// When is guaranteed to be exhaustive at this point. (If it wasn't, then
    /// a _ branch was added at the end that will throw a runtime error.)
    /// Also, `If` is desugared into `When` matching on `False` and `_` at this point.
    When {
        cond_var: Variable,
        expr_var: Variable,
        loc_cond: Box<Located<Expr>>,
        branches: Vec<(Located<Pattern>, Located<Expr>)>,
    },
    LetRec(Vec<Def>, Box<Located<Expr>>, Variable),
    LetNonRec(Box<Def>, Box<Located<Expr>>, Variable),

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call(
        Box<(Variable, Located<Expr>, Variable)>,
        Vec<(Variable, Located<Expr>)>,
        CalledVia,
    ),

    Closure(
        Variable,
        Symbol,
        Recursive,
        Vec<(Variable, Located<Pattern>)>,
        Box<(Located<Expr>, Variable)>,
    ),

    // Product Types
    Record(Variable, SendMap<Lowercase, Field>),

    /// Empty record constant
    EmptyRecord,

    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        ext_var: Variable,
        field_var: Variable,
        loc_expr: Box<Located<Expr>>,
        field: Lowercase,
    },
    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        ext_var: Variable,
        field_var: Variable,
        field: Lowercase,
    },

    Update {
        record_var: Variable,
        ext_var: Variable,
        symbol: Symbol,
        ident: Ident,
        updates: SendMap<Lowercase, Field>,
    },

    // Sum Types
    Tag {
        variant_var: Variable,
        ext_var: Variable,
        name: Symbol,
        arguments: Vec<(Variable, Located<Expr>)>,
    },

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub var: Variable,
    // The region of the full `foo: f bar`, rather than just `f bar`
    pub region: Region,
    pub loc_expr: Box<Located<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Recursive {
    Recursive,
    TailRecursive,
    NotRecursive,
}

pub fn canonicalize_expr(
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    region: Region,
    expr: &ast::Expr,
) -> (Located<Expr>, Output) {
    use self::Expr::*;

    let (expr, output) = match expr {
        ast::Expr::Int(string) => {
            let answer = int_expr_from_result(var_store, finish_parsing_int(string), env);

            (answer, Output::default())
        }
        ast::Expr::Float(string) => {
            let answer = float_expr_from_result(var_store, finish_parsing_float(string), env);

            (answer, Output::default())
        }
        ast::Expr::Record {
            fields,
            update: Some(loc_update),
        } => {
            let ident = if let ast::Expr::Var(module_parts, name) = &loc_update.value {
                Ident::new(module_parts, name)
            } else {
                panic!(
                    "TODO canonicalize invalid record update (non-Var in update position)\n{:?}",
                    &loc_update.value
                );
            };

            let (can_update, update_out) =
                canonicalize_expr(env, var_store, scope, loc_update.region, &loc_update.value);
            if let Var {
                resolved_symbol, ..
            } = can_update.value
            {
                let (can_fields, mut output) = canonicalize_fields(env, var_store, scope, fields);

                output.references = output.references.union(update_out.references);

                let answer = Update {
                    record_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    symbol: resolved_symbol,
                    ident,
                    updates: can_fields,
                };

                (answer, output)
            } else {
                panic!(
                    "TODO canonicalize invalid record update (non-Var in update position)\n{:?}",
                    can_update.value
                );
            }
        }
        ast::Expr::Record {
            fields,
            update: None,
        } => {
            if fields.is_empty() {
                (EmptyRecord, Output::default())
            } else {
                let (can_fields, output) = canonicalize_fields(env, var_store, scope, fields);

                (Record(var_store.fresh(), can_fields), output)
            }
        }
        ast::Expr::Str(string) => (Str((*string).into()), Output::default()),
        ast::Expr::BlockStr(lines) => {
            let joined = lines.iter().copied().collect::<Vec<&str>>().join("\n");

            (BlockStr(joined.into()), Output::default())
        }
        ast::Expr::List(loc_elems) => {
            if loc_elems.is_empty() {
                (List(var_store.fresh(), Vec::new()), Output::default())
            } else {
                let mut can_elems = Vec::with_capacity(loc_elems.len());
                let mut references = References::new();

                for loc_elem in loc_elems.iter() {
                    let (can_expr, elem_out) =
                        canonicalize_expr(env, var_store, scope, loc_elem.region, &loc_elem.value);

                    references = references.union(elem_out.references);

                    can_elems.push((var_store.fresh(), can_expr));
                }

                let mut output = Output::default();

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                (List(var_store.fresh(), can_elems), output)
            }
        }
        ast::Expr::Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_region = loc_fn.region;

            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) =
                canonicalize_expr(env, var_store, scope, fn_region, &loc_fn.value);

            // The function's return type
            let mut args = Vec::new();
            let mut outputs = Vec::new();

            for loc_arg in loc_args {
                let (arg_expr, arg_out) =
                    canonicalize_expr(env, var_store, scope, loc_arg.region, &loc_arg.value);

                args.push((var_store.fresh(), arg_expr));
                outputs.push(arg_out);
            }

            // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            let expr = match fn_expr.value {
                Var {
                    ref resolved_symbol,
                    ..
                } => {
                    output.references.calls.insert(resolved_symbol.clone());

                    // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                    output.tail_call = match &env.tailcallable_symbol {
                        Some(tc_sym) if tc_sym == resolved_symbol => Some(resolved_symbol.clone()),
                        Some(_) | None => None,
                    };

                    Call(
                        Box::new((var_store.fresh(), fn_expr, var_store.fresh())),
                        args,
                        *application_style,
                    )
                }
                RuntimeError(_) => {
                    // We can't call a runtime error; bail out by propagating it!
                    return (fn_expr, output);
                }
                Tag {
                    variant_var,
                    ext_var,
                    name,
                    ..
                } => Tag {
                    variant_var,
                    ext_var,
                    name,
                    arguments: args,
                },
                _ => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    Call(
                        Box::new((var_store.fresh(), fn_expr, var_store.fresh())),
                        args,
                        *application_style,
                    )
                }
            };

            for arg_out in outputs {
                output.references = output.references.union(arg_out.references);
            }

            (expr, output)
        }
        ast::Expr::Var(module_parts, name) => {
            let symbol = if module_parts.is_empty() {
                scope.symbol(name)
            } else {
                Symbol::from_parts(module_parts, name)
            };

            let ident = Ident::new(module_parts, name);

            canonicalize_lookup(env, scope, ident, symbol, region)
        } //ast::Expr::InterpolatedStr(pairs, suffix) => {
        //    let mut output = Output::new();
        //    let can_pairs: Vec<(String, Located<Expr>)> = pairs
        //        .into_iter()
        //        .map(|(string, loc_ident)| {
        //            // From a language design perspective, we only permit idents in interpolation.
        //            // However, in a canonical Expr we store it as a full Expr, not a Symbol.
        //            // This is so that we can resolve it to either Var or Unrecognized; if we
        //            // stored it as a Symbol, we couldn't record runtime errors here.
        //            let can_expr = match resolve_ident(
        //                &env,
        //                &scope,
        //                loc_ident.value,
        //                &mut output.references,
        //            ) {
        //                Ok(symbol) => Var(symbol),
        //                Err(ident) => {
        //                    let loc_ident = Located {
        //                        region: loc_ident.region,
        //                        value: ident,
        //                    };

        //                    env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

        //                    RuntimeError(UnrecognizedConstant(loc_ident))
        //                }
        //            };

        //            (
        //                string,
        //                Located {
        //                    region: loc_ident.region,
        //                    value: can_expr,
        //                },
        //            )
        //        })
        //        .collect();

        //    (InterpolatedStr(can_pairs, suffix), output)
        //}
        ast::Expr::Defs(loc_defs, loc_ret) => {
            can_defs_with_return(
                env,
                var_store,
                // The body expression gets a new scope for canonicalization,
                // so clone it.
                scope.clone(),
                loc_defs,
                loc_ret,
            )
        }
        ast::Expr::Closure(loc_arg_patterns, loc_body_expr) => {
            // The globally unique symbol that will refer to this closure once it gets converted
            // into a top-level procedure for code gen.
            //
            // The symbol includes the module name, the top-level declaration name, and the
            // index (0-based) of the closure within that declaration.
            //
            // Example: "MyModule$main$3" if this is the 4th closure in MyModule.main.
            //
            // In the case of `foo = \x y -> ...`, the symbol is later changed to `foo`.
            let symbol = scope.gen_unique_symbol();

            // The body expression gets a new scope for canonicalization.
            // Shadow `scope` to make sure we don't accidentally use the original one for the
            // rest of this block.
            let mut scope = scope.clone();

            let arg_idents: Vector<(Ident, (Symbol, Region))> =
                idents_from_patterns(loc_arg_patterns.iter(), &scope);

            // Add the arguments' idents to scope.idents. If there's a collision,
            // it means there was shadowing, which will be handled later.
            scope.idents = union_pairs(scope.idents, arg_idents.iter());

            let mut can_args = Vec::with_capacity(loc_arg_patterns.len());

            for loc_pattern in loc_arg_patterns.into_iter() {
                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(&loc_pattern.value, &mut shadowable_idents);

                let can_arg = canonicalize_pattern(
                    env,
                    var_store,
                    &mut scope,
                    FunctionArg,
                    &loc_pattern.value,
                    loc_pattern.region,
                    &mut shadowable_idents,
                );

                can_args.push((var_store.fresh(), can_arg));
            }

            let (loc_body_expr, mut output) = canonicalize_expr(
                env,
                var_store,
                &mut scope,
                loc_body_expr.region,
                &loc_body_expr.value,
            );

            // Now that we've collected all the references, check to see if any of the args we defined
            // went unreferenced. If any did, report them as unused arguments.
            for (ident, (arg_symbol, region)) in arg_idents {
                if !output.references.has_local(&arg_symbol) {
                    // The body never referenced this argument we declared. It's an unused argument!
                    env.problem(Problem::UnusedArgument(Located {
                        region,
                        value: ident,
                    }));
                }

                // We shouldn't ultimately count arguments as referenced locals. Otherwise,
                // we end up with weird conclusions like the expression (\x -> x + 1)
                // references the (nonexistant) local variable x!
                output.references.locals.remove(&arg_symbol);
            }

            env.register_closure(symbol.clone(), output.references.clone());

            (
                Closure(
                    var_store.fresh(),
                    symbol,
                    Recursive::NotRecursive,
                    can_args,
                    Box::new((loc_body_expr, var_store.fresh())),
                ),
                output,
            )
        }
        ast::Expr::When(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = var_store.fresh();
            let (can_cond, mut output) =
                canonicalize_expr(env, var_store, scope, region, &loc_cond.value);

            // the condition can never be a tail-call
            output.tail_call = None;

            let mut can_branches = Vec::with_capacity(branches.len());

            for (loc_pattern, loc_expr) in branches {
                let mut shadowable_idents = scope.idents.clone();

                remove_idents(&loc_pattern.value, &mut shadowable_idents);

                let (can_pattern, loc_can_expr, branch_references) = canonicalize_when_branch(
                    env,
                    var_store,
                    scope,
                    region,
                    loc_pattern,
                    loc_expr,
                    &mut output,
                );

                output.references = output.references.union(branch_references);

                can_branches.push((can_pattern, loc_can_expr));
            }

            // A "when" with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happend to be one. (The condition gave us our initial output value.)
            if branches.is_empty() {
                output.tail_call = None;
            }

            // Incorporate all three expressions into a combined Output value.
            let expr = When {
                expr_var: var_store.fresh(),
                cond_var,
                loc_cond: Box::new(can_cond),
                branches: can_branches,
            };

            (expr, output)
        }
        ast::Expr::Access(record_expr, field) => {
            let (loc_expr, output) = canonicalize_expr(env, var_store, scope, region, record_expr);

            (
                Access {
                    field_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    loc_expr: Box::new(loc_expr),
                    field: Lowercase::from(*field),
                },
                output,
            )
        }
        ast::Expr::AccessorFunction(field) => {
            let ext_var = var_store.fresh();
            let field_var = var_store.fresh();
            let field_name: Lowercase = (*field).into();

            (
                Accessor {
                    field: field_name,
                    ext_var,
                    field_var,
                },
                Output::default(),
            )
        }
        ast::Expr::GlobalTag(tag) => {
            let variant_var = var_store.fresh();
            let ext_var = var_store.fresh();

            (
                Tag {
                    name: Symbol::from_global_tag(tag),
                    arguments: vec![],
                    variant_var,
                    ext_var,
                },
                Output::default(),
            )
        }
        ast::Expr::PrivateTag(tag) => {
            let variant_var = var_store.fresh();
            let ext_var = var_store.fresh();

            (
                Tag {
                    name: Symbol::from_private_tag(&env.home, tag),
                    arguments: vec![],
                    variant_var,
                    ext_var,
                },
                Output::default(),
            )
        }
        ast::Expr::If(_)
        | ast::Expr::MalformedIdent(_)
        | ast::Expr::MalformedClosure
        | ast::Expr::PrecedenceConflict(_, _, _) => {
            panic!(
                "TODO restore the rest of canonicalize()'s branches {:?} {:?}",
                &expr,
                local_successors(&References::new(), &env.closures)
            );
        }
        ast::Expr::Nested(sub_expr) => {
            let (answer, output) = canonicalize_expr(env, var_store, scope, region, sub_expr);

            (answer.value, output)
        }
        ast::Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            let mut result = finish_parsing_base(string, *base);

            if *is_negative {
                result = result.map(i64::neg);
            }

            let answer = int_expr_from_result(var_store, result, env);

            (answer, Output::default())
        }
        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
        ast::Expr::ParensAround(sub_expr) => {
            panic!(
                "A ParensAround did not get removed during operator desugaring somehow: {:?}",
                sub_expr
            );
        }
        ast::Expr::SpaceBefore(sub_expr, _spaces) => {
            panic!(
                "A SpaceBefore did not get removed during operator desugaring somehow: {:?}",
                sub_expr
            );
        }
        ast::Expr::SpaceAfter(sub_expr, _spaces) => {
            panic!(
                "A SpaceAfter did not get removed during operator desugaring somehow: {:?}",
                sub_expr
            );
        }
        ast::Expr::BinOp((_, loc_op, _)) => {
            panic!(
                "A binary operator did not get desugared somehow: {:?}",
                loc_op
            );
        }
        ast::Expr::UnaryOp(_, loc_op) => {
            panic!(
                "A unary operator did not get desugared somehow: {:?}",
                loc_op
            );
        }
    };

    // At the end, diff used_idents and defined_idents to see which were unused.
    // Add warnings for those!

    // In a later phase, unused top level declarations won't get monomorphized or code-genned.
    // We aren't going to bother with DCE at the level of local defs. It's going to be
    // a rounding error anyway (especially given that they'll be surfaced as warnings), LLVM will
    // DCE them in optimized builds, and it's not worth the bookkeeping for dev builds.
    (
        Located {
            region,
            value: expr,
        },
        output,
    )
}

#[inline(always)]
fn canonicalize_lookup(
    env: &mut Env,
    scope: &Scope,
    ident: Ident,
    symbol_for_lookup: Symbol,
    region: Region,
) -> (Expr, Output) {
    use self::Expr::*;

    let mut output = Output::default();
    let can_expr = match resolve_ident(&env, &scope, ident, &mut output.references) {
        Ok(resolved_symbol) => Var {
            symbol_for_lookup,
            resolved_symbol,
        },
        Err(ident) => {
            let loc_ident = Located {
                region,
                value: ident,
            };

            env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

            RuntimeError(UnrecognizedConstant(loc_ident))
        }
    };

    (can_expr, output)
}

#[inline(always)]
fn canonicalize_when_branch<'a>(
    env: &mut Env,
    var_store: &VarStore,
    scope: &Scope,
    region: Region,
    loc_pattern: &Located<ast::Pattern<'a>>,
    loc_expr: &Located<ast::Expr<'a>>,
    output: &mut Output,
) -> (Located<Pattern>, Located<Expr>, References) {
    // Each case branch gets a new scope for canonicalization.
    // Shadow `scope` to make sure we don't accidentally use the original one for the
    // rest of this block.
    let mut scope = scope.clone();

    // Exclude the current ident from shadowable_idents; you can't shadow yourself!
    // (However, still include it in scope, because you *can* recursively refer to yourself.)
    let mut shadowable_idents = scope.idents.clone();
    remove_idents(&loc_pattern.value, &mut shadowable_idents);

    // Patterns introduce new idents to the scope!
    // Add the defined identifiers to scope. If there's a collision, it means there
    // was shadowing, which will be handled later.
    let defined_idents: Vector<(Ident, (Symbol, Region))> =
        idents_from_patterns(std::iter::once(loc_pattern), &scope);

    scope.idents = union_pairs(scope.idents, defined_idents.iter());

    let (can_expr, branch_output) =
        canonicalize_expr(env, var_store, &mut scope, region, &loc_expr.value);

    // If we already recorded a tail call then keep it, else use this branch's tail call
    match output.tail_call {
        Some(_) => {}
        None => output.tail_call = branch_output.tail_call,
    };

    // Now that we've collected all the references for this branch, check to see if
    // any of the new idents it defined were unused. If any were, report it.
    for (ident, (symbol, region)) in defined_idents {
        if !output.references.has_local(&symbol) {
            let loc_ident = Located {
                region,
                value: ident.clone(),
            };

            env.problem(Problem::UnusedAssignment(loc_ident));
        }
    }

    let loc_can_pattern = canonicalize_pattern(
        env,
        var_store,
        &mut scope,
        WhenBranch,
        &loc_pattern.value,
        loc_pattern.region,
        &mut shadowable_idents,
    );

    (loc_can_pattern, can_expr, branch_output.references)
}

pub fn union_pairs<'a, K, V, I>(mut map: ImMap<K, V>, pairs: I) -> ImMap<K, V>
where
    I: Iterator<Item = &'a (K, V)>,
    K: std::hash::Hash + std::cmp::Eq + Clone,
    K: 'a,
    V: Clone,
    V: 'a,
{
    for (ref k, ref v) in pairs {
        map.insert(k.clone(), v.clone());
    }

    map
}

pub fn local_successors<'a>(
    references: &'a References,
    closures: &'a MutMap<Symbol, References>,
) -> ImSet<Symbol> {
    let mut answer = references.locals.clone();

    for call_symbol in references.calls.iter() {
        answer = answer.union(call_successors(call_symbol, closures));
    }

    answer
}

fn call_successors<'a>(
    call_symbol: &'a Symbol,
    closures: &'a MutMap<Symbol, References>,
) -> ImSet<Symbol> {
    // TODO (this comment should be moved to a GH issue) this may cause an infinite loop if 2 definitions reference each other; may need to track visited definitions!
    match closures.get(call_symbol) {
        Some(references) => {
            let mut answer = local_successors(&references, closures);

            answer.insert(call_symbol.clone());

            answer
        }
        None => ImSet::default(),
    }
}

pub fn references_from_local<'a, T>(
    defined_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_def: &'a MutMap<Symbol, (T, References)>,
    closures: &'a MutMap<Symbol, References>,
) -> References
where
    T: Debug,
{
    let mut answer: References = References::new();

    match refs_by_def.get(&defined_symbol) {
        Some((_, refs)) => {
            visited.insert(defined_symbol);

            for local in refs.locals.iter() {
                if !visited.contains(&local) {
                    let other_refs: References =
                        references_from_local(local.clone(), visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(local.clone());
            }

            for call in refs.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs =
                        references_from_call(call.clone(), visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(call.clone());
            }

            answer
        }
        None => answer,
    }
}

pub fn references_from_call<'a, T>(
    call_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_def: &'a MutMap<Symbol, (T, References)>,
    closures: &'a MutMap<Symbol, References>,
) -> References
where
    T: Debug,
{
    match closures.get(&call_symbol) {
        Some(references) => {
            let mut answer = references.clone();

            visited.insert(call_symbol);

            for closed_over_local in references.locals.iter() {
                if !visited.contains(&closed_over_local) {
                    let other_refs = references_from_local(
                        closed_over_local.clone(),
                        visited,
                        refs_by_def,
                        closures,
                    );

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(closed_over_local.clone());
            }

            for call in references.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs =
                        references_from_call(call.clone(), visited, refs_by_def, closures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(call.clone());
            }

            answer
        }
        None => {
            // If the call symbol was not in the closure map, that means we're calling a non-function and
            // will get a type mismatch later. For now, assume no references as a result of the "call."
            References::new()
        }
    }
}

/// If it could not be found, return it unchanged as an Err.
#[inline(always)] // This is shared code between Var and InterpolatedStr; it was inlined when handwritten
fn resolve_ident<'a>(
    env: &'a Env,
    scope: &Scope,
    ident: Ident,
    references: &mut References,
) -> Result<Symbol, Ident> {
    if scope.idents.contains_key(&ident) {
        let recognized = match ident {
            Ident::Unqualified(name) => {
                let symbol = scope.symbol(&name);

                references.locals.insert(symbol.clone());

                symbol
            }
            Ident::Qualified(path, name) => {
                let symbol = Symbol::new(&path, &name);

                references.globals.insert(symbol.clone());

                symbol
            }
        };

        Ok(recognized)
    } else {
        match ident {
            Ident::Unqualified(name) => {
                // Try again, this time using the current module as the path.
                let qualified = Ident::Qualified(env.home.clone(), name.clone());

                if scope.idents.contains_key(&qualified) {
                    let symbol = Symbol::new(&env.home, &name);

                    references.globals.insert(symbol.clone());

                    Ok(symbol)
                } else {
                    // We couldn't find the unqualified ident in scope. NAMING PROBLEM!
                    Err(Ident::Unqualified(name))
                }
            }
            Ident::Qualified(module_name, name) => {
                let symbol = Symbol::from_qualified_ident(module_name, name);

                references.globals.insert(symbol.clone());

                Ok(symbol)
            }
        }
    }
}

fn canonicalize_fields<'a>(
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    fields: &'a [Located<ast::AssignedField<'a, ast::Expr<'a>>>],
) -> (SendMap<Lowercase, Field>, Output) {
    let mut can_fields = SendMap::default();
    let mut output = Output::default();

    for loc_field in fields.iter() {
        let (label, field_expr, field_out, field_var) =
            canonicalize_field(env, var_store, scope, &loc_field.value, loc_field.region);

        let field = Field {
            var: field_var,
            region: loc_field.region,
            loc_expr: Box::new(field_expr),
        };

        can_fields.insert(label, field);

        output.references = output.references.union(field_out.references);
    }

    (can_fields, output)
}

fn canonicalize_field<'a>(
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    field: &'a ast::AssignedField<'a, ast::Expr<'a>>,
    region: Region,
) -> (Lowercase, Located<Expr>, Output, Variable) {
    use crate::parse::ast::AssignedField::*;

    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        LabeledValue(label, _, loc_expr) => {
            let field_var = var_store.fresh();
            let (loc_can_expr, output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            (
                Lowercase::from(label.value),
                loc_can_expr,
                output,
                field_var,
            )
        }

        OptionalField(_, _, _) => panic!("invalid in expressions"),

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(_) => {
            panic!("Somehow a LabelOnly record field was not desugared!");
        }

        SpaceBefore(sub_field, _) | SpaceAfter(sub_field, _) => {
            canonicalize_field(env, var_store, scope, sub_field, region)
        }

        Malformed(_string) => {
            panic!("TODO canonicalize malformed record field");
        }
    }
}

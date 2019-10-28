use self::env::Env;
use self::expr::Expr::{self, *};
use self::pattern::PatternType::*;
use self::pattern::{canonicalize_pattern, Pattern};
use self::problem::Problem;
use self::problem::RuntimeError::*;
use self::procedure::{Procedure, References};
use self::scope::Scope;
use self::symbol::Symbol;
use bumpalo::Bump;
use collections::{ImMap, ImSet, MutMap, MutSet};
use constrain;
use graph::{strongly_connected_component, topological_sort};
use ident::Ident;
use parse::ast::{self, Def};
use region::{Located, Region};
use std::i64;
use subs::{Subs, Variable};
use types::Constraint::{self, *};
use types::Expected::{self, *};
use types::LetConstraint;
use types::Reason;
use types::Type::{self, *};

pub mod env;
pub mod expr;
pub mod operator;
pub mod pattern;
pub mod problem;
pub mod procedure;
pub mod scope;
pub mod string;
pub mod symbol;

pub fn canonicalize_declaration<'a>(
    arena: &Bump,
    subs: &mut Subs,
    home: Box<str>,
    name: Box<str>,
    region: Region,
    loc_expr: Located<ast::Expr<'a>>,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
    declared_variants: &ImMap<Symbol, Located<Box<str>>>,
    expected: Expected<Type>,
) -> (
    Located<Expr>,
    Output,
    Vec<Problem>,
    MutMap<Symbol, Procedure>,
) {
    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing any other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited an Operator node we'd recursively try to apply this to each of its nested
    // operators, and thena again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let loc_expr = operator::desugar(arena, &loc_expr);

    // If we're canonicalizing the declaration `foo = ...` inside the `Main` module,
    // scope_prefix will be "Main.foo$" and its first closure will be named "Main.foo$0"
    let scope_prefix = format!("{}.{}$", home, name).into();
    let mut scope = Scope::new(scope_prefix, declared_idents.clone());
    let mut env = Env::new(home, declared_variants.clone());
    let (loc_expr, output) = canonicalize_expr(
        &mut env,
        subs,
        &mut scope,
        region,
        &loc_expr.value,
        expected,
    );

    (loc_expr, output, env.problems, env.procedures)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub constraint: Constraint,
}

impl Output {
    pub fn new(constraint: Constraint) -> Output {
        Output {
            references: References::new(),
            tail_call: None,
            constraint,
        }
    }
}

fn canonicalize_expr(
    env: &mut Env,
    subs: &mut Subs,
    scope: &mut Scope,
    region: Region,
    expr: &ast::Expr,
    expected: Expected<Type>,
) -> (Located<Expr>, Output) {
    use self::Expr::*;

    let (expr, output) = match expr {
        ast::Expr::Int(string) => {
            let (constraint, answer) = int_from_parsed(subs, string, env, expected, region);

            (answer, Output::new(constraint))
        }
        ast::Expr::Float(string) => {
            let (constraint, answer) = float_from_parsed(subs, string, env, expected, region);

            (answer, Output::new(constraint))
        }
        ast::Expr::Record(fields) => {
            if fields.is_empty() {
                let constraint = Eq(EmptyRec, expected, region);

                (EmptyRecord, Output::new(constraint))
            } else {
                panic!("TODO canonicalize nonempty record");
            }
        }
        ast::Expr::Str(string) => {
            let constraint = Eq(constrain::str_type(), expected, region);

            (Str((*string).into()), Output::new(constraint))
        }
        ast::Expr::List(loc_elems) => {
            if loc_elems.is_empty() {
                let list_var = subs.mk_flex_var();
                let constraint = Eq(constrain::empty_list_type(list_var), expected, region);

                (List(list_var, Vec::new()), Output::new(constraint))
            } else {
                let mut can_elems = Vec::with_capacity(loc_elems.len());
                let list_var = subs.mk_flex_var(); // `v` in the type (List v)
                let list_type = Type::Variable(list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));
                let mut references = References::new();

                for loc_elem in loc_elems.iter() {
                    let elem_var = subs.mk_flex_var();
                    let elem_type = Variable(elem_var);
                    let elem_expected = NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_type.clone(),
                        ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let (can_expr, elem_out) = canonicalize_expr(
                        env,
                        subs,
                        scope,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(list_elem_constraint);
                    constraints.push(elem_out.constraint);

                    references = references.union(elem_out.references);

                    can_elems.push(can_expr);
                }

                constraints.push(Eq(constrain::list_type(list_type), expected, region));

                let mut output = Output::new(And(constraints));

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                (List(list_var, can_elems), output)
            }
        }

        //ast::Expr::If(loc_cond, loc_true, loc_false) => {
        //    // Canonicalize the nested expressions
        //    let (cond_expr, cond_out) = canonicalize(env, scope, *loc_cond);
        //    let (true_expr, true_out) = canonicalize(env, scope, *loc_true);
        //    let (false_expr, false_out) = canonicalize(env, scope, *loc_false);

        //    // Incorporate all three expressions into a combined Output value.
        //    let expr = If(
        //        Box::new(cond_expr),
        //        Box::new(true_expr),
        //        Box::new(false_expr),
        //    );
        //    let mut output = cond_out;

        //    // If both branches are tail calling the same symbol, then so is the conditional as a whole.
        //    // Also, if both branches are not tail calls (tail_call == None), then so is the conditional.
        //    // If the branches are different, we leave the default of None as-is.
        //    if true_out.tail_call == false_out.tail_call {
        //        output.tail_call = true_out.tail_call;
        //    }

        //    // To evaluate the whole if-expression, we depend on all the values that both branches depend on.
        //    output.references = output.references.union(true_out.references);
        //    output.references = output.references.union(false_out.references);

        //    (expr, output)
        //}
        ast::Expr::Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_var = subs.mk_flex_var();
            let fn_type = Variable(fn_var);
            let fn_region = loc_fn.region;
            let fn_expected = NoExpectation(fn_type.clone());
            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall(loc_args.len() as u8);

            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) =
                canonicalize_expr(env, subs, scope, loc_fn.region, &loc_fn.value, fn_expected);

            // The function's return type
            let ret_var = subs.mk_flex_var();
            let ret_type = Variable(ret_var);

            // This will be used in the occurs check
            let mut vars = Vec::with_capacity(2 + loc_args.len());

            vars.push(fn_var);
            vars.push(ret_var);

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            let mut args = Vec::new();
            let mut outputs = Vec::new();

            for (index, loc_arg) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_var = subs.mk_flex_var();
                let arg_type = Variable(arg_var);
                // TODO look up the name and use NamedFnArg if possible.
                let reason = Reason::AnonymousFnArg(index as u8);
                let expected_arg = ForReason(reason, arg_type.clone(), region);
                let (arg_expr, arg_out) = canonicalize_expr(
                    env,
                    subs,
                    scope,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

                let arg_con = arg_out.constraint.clone();

                vars.push(arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);

                args.push(arg_expr);
                outputs.push(arg_out);
            }

            // We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            let expr = match &fn_expr.value {
                &Var(_, ref sym) | &FunctionPointer(_, ref sym) => {
                    // In the FunctionPointer case, we're calling an inline closure;
                    // something like ((\a b -> a + b) 1 2)
                    output.references.calls.insert(sym.clone());

                    CallByName(sym.clone(), args, *application_style)
                }
                &RuntimeError(_) => {
                    // We can't call a runtime error; bail out by propagating it!
                    return (fn_expr, output);
                }
                not_var => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    // Use CallPointer here.
                    panic!("TODO support function calls that aren't by name, via CallPointer, in this case: {:?}", not_var);
                }
            };

            for arg_out in outputs {
                output.references = output.references.union(arg_out.references);
            }

            let fn_con = output.constraint;

            // TODO occurs check!
            // return $ exists vars $ CAnd ...

            let expected_fn_type = ForReason(
                fn_reason,
                Function(arg_types, Box::new(ret_type.clone())),
                region,
            );

            output.constraint = And(vec![
                fn_con,
                Eq(fn_type, expected_fn_type, fn_region),
                And(arg_cons),
                Eq(ret_type, expected, region),
            ]);

            (expr, output)
        }
        ast::Expr::Var(module_parts, name) => {
            let symbol = if module_parts.is_empty() {
                scope.symbol(name)
            } else {
                Symbol::from_parts(module_parts, name)
            };

            let mut output = Output::new(Lookup(symbol, expected, region));
            let ident = Ident::new(module_parts, name);
            let can_expr = match resolve_ident(&env, &scope, ident, &mut output.references) {
                Ok(symbol) => Var(subs.mk_flex_var(), symbol),
                Err(ident) => {
                    let loc_ident = Located {
                        region: region,
                        value: ident,
                    };

                    env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

                    RuntimeError(UnrecognizedConstant(loc_ident))
                }
            };

            (can_expr, output)
        }

        //ast::Expr::InterpolatedStr(pairs, suffix) => {
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

        //ast::Expr::ApplyVariant(variant_name, opt_args) => {
        //    // Canonicalize the arguments and union their references into our output.
        //    // We'll do this even if the variant name isn't recognized, since we still
        //    // want to report canonicalization problems with the variant's arguments,
        //    // and their references still matter for purposes of detecting unused things.
        //    let mut output = Output::new();

        //    let opt_can_args = match opt_args {
        //        Some(args) => {
        //            let mut can_args = Vec::with_capacity(args.len());

        //            for arg in args {
        //                let (loc_expr, arg_output) = canonicalize(env, scope, arg);

        //                output.references = output.references.union(arg_output.references);

        //                can_args.push(loc_expr);
        //            }

        //            Some(can_args)
        //        }
        //        None => None,
        //    };

        //    let can_expr = match resolve_variant_name(&env, variant_name, &mut output.references) {
        //        Ok(symbol) => ApplyVariant(symbol, opt_can_args),
        //        Err(variant_name) => {
        //            let loc_variant = Located {
        //                region: loc_expr.region,
        //                value: variant_name,
        //            };

        //            env.problem(Problem::UnrecognizedVariant(loc_variant.clone()));

        //            RuntimeError(UnrecognizedVariant(loc_variant))
        //        }
        //    };

        //    (can_expr, output)
        //}
        ast::Expr::Defs(defs, loc_ret) => {
            // The body expression gets a new scope for canonicalization,
            // so clone it.
            can_defs(env, subs, scope.clone(), defs, expected, loc_ret)
        }
        ast::Expr::Closure(loc_arg_patterns, loc_body_expr) => {
            // The globally unique symbol that will refer to this closure once it gets converted
            // into a top-level procedure for code gen.
            //
            // The symbol includes the module name, the top-level declaration name, and the
            // index (0-based) of the closure within that declaration.
            //
            // Example: "MyModule$main$3" if this is the 4th closure in MyModule.main.
            let symbol = scope.gen_unique_symbol();

            // The body expression gets a new scope for canonicalization.
            // Shadow `scope` to make sure we don't accidentally use the original one for the
            // rest of this block.
            let mut scope = scope.clone();

            let arg_idents: Vec<(Ident, (Symbol, Region))> =
                idents_from_patterns(loc_arg_patterns.iter(), &scope);

            // Add the arguments' idents to scope.idents. If there's a collision,
            // it means there was shadowing, which will be handled later.
            scope.idents = union_pairs(scope.idents, arg_idents.iter());

            let mut state = PatternState {
                assignment_types: ImMap::default(),
                vars: Vec::with_capacity(loc_arg_patterns.len()),
                reversed_constraints: Vec::with_capacity(1),
            };
            let args = constrain_args(loc_arg_patterns.iter(), &scope, subs, &mut state);
            let mut can_args: Vec<Located<Pattern>> = Vec::with_capacity(loc_arg_patterns.len());

            for loc_pattern in loc_arg_patterns.into_iter() {
                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(&loc_pattern.value, &mut shadowable_idents);

                can_args.push(canonicalize_pattern(
                    env,
                    subs,
                    &mut scope,
                    &FunctionArg,
                    &loc_pattern,
                    &mut shadowable_idents,
                ))
            }

            let body_type = NoExpectation(args.ret_type);
            let (loc_body_expr, mut output) = canonicalize_expr(
                env,
                subs,
                &mut scope,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

            state.reversed_constraints.reverse();

            let assignments_constraint = And(state.reversed_constraints);
            let ret_constraint = output.constraint;

            // panic!("TODO occurs check");

            // We need a var to record in the procedure for later.
            // We'll set it equal to the overall `expected` we've been passed.
            let var = subs.mk_flex_var();
            let typ = Variable(var);

            output.constraint = And(vec![
                Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: state.vars,
                    assignment_types: state.assignment_types,
                    assignments_constraint,
                    ret_constraint,
                })),
                // "the closure's type is equal to the var we've stored for later use in the proc"
                Eq(args.typ, NoExpectation(typ.clone()), region),
                // "the var we've stored for later is equal to the overall expected type"
                Eq(typ, expected, region),
            ]);

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

            // We've finished analyzing the closure. Its references.locals are now the values it closes over,
            // since we removed the only locals it shouldn't close over (its arguments).
            // Register it as a top-level procedure in the Env!
            env.register_closure(
                symbol.clone(),
                can_args,
                loc_body_expr,
                region,
                output.references.clone(),
                var,
                args.ret_var,
            );

            // Always return a function pointer, in case that's how the closure is being used (e.g. with Apply).
            (FunctionPointer(var, symbol), output)
        }

        // ast::Expr::Case(loc_cond, branches) => {
        //     // Canonicalize the conditional
        //     let (can_cond, mut output) = canonicalize(env, scope, *loc_cond);
        //     let mut can_branches = Vec::with_capacity(branches.len());
        //     let mut recorded_tail_call = false;

        //     for (loc_pattern, loc_expr) in branches {
        //         // Each case branch gets a new scope for canonicalization.
        //         // Shadow `scope` to make sure we don't accidentally use the original one for the
        //         // rest of this block.
        //         let mut scope = scope.clone();

        //         // Exclude the current ident from shadowable_idents; you can't shadow yourself!
        //         // (However, still include it in scope, because you *can* recursively refer to yourself.)
        //         let mut shadowable_idents = scope.idents.clone();
        //         remove_idents(loc_pattern.value.clone(), &mut shadowable_idents);

        //         let loc_can_pattern = canonicalize_pattern(
        //             env,
        //             &mut scope,
        //             &CaseBranch,
        //             &loc_pattern,
        //             &mut shadowable_idents,
        //         );

        //         // Patterns introduce new idents to the scope!
        //         // Add the assigned identifiers to scope. If there's a collision, it means there
        //         // was shadowing, which will be handled later.
        //         let assigned_idents: Vec<(Ident, (Symbol, Region))> =
        //             idents_from_patterns(std::iter::once(&loc_pattern), &scope);

        //         scope.idents = union_pairs(scope.idents, assigned_idents.iter());

        //         let (can_expr, branch_output) = canonicalize(env, &mut scope, loc_expr);

        //         output.references = output.references.union(branch_output.references);

        //         // If all branches are tail calling the same symbol, then so is the conditional as a whole.
        //         if !recorded_tail_call {
        //             // If we haven't recorded output.tail_call yet, record it.
        //             output.tail_call = branch_output.tail_call;
        //             recorded_tail_call = true;
        //         } else if branch_output.tail_call != output.tail_call {
        //             // If we recorded output.tail_call, but what we recorded differs from what we just saw,
        //             // then game over. This can't possibly be a self tail call!
        //             output.tail_call = None;
        //         }

        //         // Now that we've collected all the references for this branch, check to see if
        //         // any of the new idents it defined were unused. If any were, report it.
        //         for (ident, (symbol, region)) in assigned_idents {
        //             if !output.references.has_local(&symbol) {
        //                 let loc_ident = Located {
        //                     region: region,
        //                     value: ident.clone(),
        //                 };

        //                 env.problem(Problem::UnusedAssignment(loc_ident));
        //             }
        //         }

        //         can_branches.push((loc_can_pattern, can_expr));
        //     }

        //     // One of the branches should have flipped this, so this should only happen
        //     // in the situation where the case had no branches. That can come up, though!
        //     // A case with no branches is a runtime error, but it will mess things up
        //     // if code gen mistakenly thinks this is a tail call just because its condition
        //     // happend to be one. (The condition gave us our initial output value.)
        //     if !recorded_tail_call {
        //         output.tail_call = None;
        //     }

        //     // Incorporate all three expressions into a combined Output value.
        //     let expr = Case(Box::new(can_cond), can_branches);

        //     (expr, output)
        // }
        ast::Expr::BlockStr(_)
        | ast::Expr::Field(_, _)
        | ast::Expr::QualifiedField(_, _)
        | ast::Expr::AccessorFunction(_)
        | ast::Expr::If(_)
        | ast::Expr::Case(_, _)
        | ast::Expr::Variant(_, _)
        | ast::Expr::MalformedIdent(_)
        | ast::Expr::MalformedClosure
        | ast::Expr::PrecedenceConflict(_, _, _) => {
            panic!(
                "TODO restore the rest of canonicalize()'s branches {:?}",
                local_successors(&References::new(), &MutMap::default())
            );
        }
        ast::Expr::BinaryInt(string) => {
            let (constraint, answer) = bin_from_parsed(subs, string, env, expected, region);

            (answer, Output::new(constraint))
        }
        ast::Expr::HexInt(string) => {
            let (constraint, answer) = hex_from_parsed(subs, string, env, expected, region);

            (answer, Output::new(constraint))
        }
        ast::Expr::OctalInt(string) => {
            let (constraint, answer) = oct_from_parsed(subs, string, env, expected, region);

            (answer, Output::new(constraint))
        }
        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
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
        ast::Expr::Operator((_, loc_op, _)) => {
            panic!("An operator did not get desugared somehow: {:?}", loc_op);
        }
    };

    // At the end, diff used_idents and assigned_idents to see which were unused.
    // Add warnings for those!

    // In a later phase, unused top level declarations won't get monomorphized or code-genned.
    // We aren't going to bother with DCE at the level of local assignments. It's going to be
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

fn union_pairs<'a, K, V, I>(mut map: ImMap<K, V>, pairs: I) -> ImMap<K, V>
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

fn local_successors<'a>(
    references: &'a References,
    procedures: &'a MutMap<Symbol, Procedure>,
) -> ImSet<Symbol> {
    let mut answer = references.locals.clone();

    for call_symbol in references.calls.iter() {
        answer = answer.union(call_successors(call_symbol, procedures));
    }

    answer
}

fn call_successors<'a>(
    call_symbol: &'a Symbol,
    procedures: &'a MutMap<Symbol, Procedure>,
) -> ImSet<Symbol> {
    // TODO (this comment should be moved to a GH issue) this may cause an infinite loop if 2 procedures reference each other; may need to track visited procedures!
    match procedures.get(call_symbol) {
        Some(procedure) => {
            let mut answer = local_successors(&procedure.references, procedures);

            answer.insert(call_symbol.clone());

            answer
        }
        None => ImSet::default(),
    }
}

fn references_from_local<'a, T>(
    assigned_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_assignment: &'a MutMap<Symbol, (T, References)>,
    procedures: &'a MutMap<Symbol, Procedure>,
) -> References {
    match refs_by_assignment.get(&assigned_symbol) {
        Some((_, refs)) => {
            let mut answer: References = References::new();

            visited.insert(assigned_symbol);

            for local in refs.locals.iter() {
                if !visited.contains(&local) {
                    let other_refs: References = references_from_local(
                        local.clone(),
                        visited,
                        refs_by_assignment,
                        procedures,
                    );

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(local.clone());
            }

            for call in refs.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs =
                        references_from_call(call.clone(), visited, refs_by_assignment, procedures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(call.clone());
            }

            answer
        }
        None => {
            // This should never happen! If the local was not recognized, it should not have been
            // added to the local references.
            unreachable!();
        }
    }
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
        .unwrap();

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

fn references_from_call<'a, T>(
    call_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_assignment: &'a MutMap<Symbol, (T, References)>,
    procedures: &'a MutMap<Symbol, Procedure>,
) -> References {
    match procedures.get(&call_symbol) {
        Some(procedure) => {
            let mut answer = procedure.references.clone();

            visited.insert(call_symbol);

            for closed_over_local in procedure.references.locals.iter() {
                if !visited.contains(&closed_over_local) {
                    let other_refs = references_from_local(
                        closed_over_local.clone(),
                        visited,
                        refs_by_assignment,
                        procedures,
                    );

                    answer = answer.union(other_refs);
                }

                answer.locals.insert(closed_over_local.clone());
            }

            for call in procedure.references.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs =
                        references_from_call(call.clone(), visited, refs_by_assignment, procedures);

                    answer = answer.union(other_refs);
                }

                answer.calls.insert(call.clone());
            }

            answer
        }
        None => {
            // If the call symbol was not in the procedures map, that means we're calling a non-function and
            // will get a type mismatch later. For now, assume no references as a result of the "call."
            References::new()
        }
    }
}

fn idents_from_patterns<'a, I>(loc_patterns: I, scope: &Scope) -> Vec<(Ident, (Symbol, Region))>
where
    I: Iterator<Item = &'a Located<ast::Pattern<'a>>>,
{
    let mut answer = Vec::new();

    for loc_pattern in loc_patterns {
        add_idents_from_pattern(&loc_pattern.region, &loc_pattern.value, scope, &mut answer);
    }

    answer
}

/// helper function for idents_from_patterns
fn add_idents_from_pattern<'a>(
    region: &'a Region,
    pattern: &'a ast::Pattern<'a>,
    scope: &'a Scope,
    answer: &'a mut Vec<(Ident, (Symbol, Region))>,
) {
    use parse::ast::Pattern::*;

    match &pattern {
        &Identifier(name) => {
            let symbol = scope.symbol(&name);

            answer.push((Ident::Unqualified(name.to_string()), (symbol, *region)));
        }
        &QualifiedIdentifier(_name) => {
            panic!("TODO implement QualifiedIdentifier pattern.");
        }
        &Apply(_, _) => {
            panic!("TODO implement Apply pattern.");
            // &AppliedVariant(_, ref opt_loc_args) => match opt_loc_args {
            // &None => (),
            // &Some(ref loc_args) => {
            //     for loc_arg in loc_args.iter() {
            //         add_idents_from_pattern(loc_arg, scope, answer);
            //     }
            // }
            // },
        }

        &RecordDestructure(_) => {
            panic!("TODO implement RecordDestructure pattern in add_idents_from_pattern.");
        }
        &SpaceBefore(pattern, _) | &SpaceAfter(pattern, _) => {
            // Ignore the newline/comment info; it doesn't matter in canonicalization.
            add_idents_from_pattern(region, pattern, scope, answer)
        }
        &Variant(_, _)
        | &IntLiteral(_)
        | &HexIntLiteral(_)
        | &OctalIntLiteral(_)
        | &BinaryIntLiteral(_)
        | &FloatLiteral(_)
        | &StrLiteral(_)
        | &EmptyRecordLiteral
        | &Malformed(_)
        | &Underscore => (),
    }
}

fn remove_idents(pattern: &ast::Pattern, idents: &mut ImMap<Ident, (Symbol, Region)>) {
    use parse::ast::Pattern::*;

    match &pattern {
        Identifier(name) => {
            idents.remove(&(Ident::Unqualified(name.to_string())));
        }
        QualifiedIdentifier(_name) => {
            panic!("TODO implement QualifiedIdentifier pattern in remove_idents.");
        }
        Apply(_, _) => {
            panic!("TODO implement Apply pattern in remove_idents.");
            // AppliedVariant(_, Some(loc_args)) => {
            //     for loc_arg in loc_args {
            //         remove_idents(loc_arg.value, idents);
            //     }
            // }
        }
        RecordDestructure(_) => {
            panic!("TODO implement RecordDestructure pattern in remove_idents.");
        }
        SpaceBefore(pattern, _) | SpaceAfter(pattern, _) => {
            // Ignore the newline/comment info; it doesn't matter in canonicalization.
            remove_idents(pattern, idents)
        }
        Variant(_, _)
        | IntLiteral(_)
        | HexIntLiteral(_)
        | BinaryIntLiteral(_)
        | OctalIntLiteral(_)
        | FloatLiteral(_)
        | StrLiteral(_)
        | EmptyRecordLiteral
        | Malformed(_)
        | Underscore => {}
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
                let qualified = Ident::Qualified(env.home.clone().to_string(), name.clone());

                if scope.idents.contains_key(&qualified) {
                    let symbol = Symbol::new(&env.home, &name);

                    references.globals.insert(symbol.clone());

                    Ok(symbol)
                } else {
                    // We couldn't find the unqualified ident in scope. NAMING PROBLEM!
                    Err(Ident::Unqualified(name))
                }
            }
            qualified @ Ident::Qualified(_, _) => {
                // We couldn't find the qualified ident in scope. NAMING PROBLEM!
                Err(qualified)
            }
        }
    }
}

///// Translate a VariantName into a resolved symbol if it's found in env.declared_variants.
///// If it could not be found, return it unchanged as an Err.
//#[inline(always)]
//fn resolve_variant_name(
//    env: &Env,
//    variant_name: VariantName,
//    references: &mut References,
//) -> Result<Symbol, VariantName> {
//    let symbol = Symbol::from_variant(&variant_name, &env.home);

//    if env.variants.contains_key(&symbol) {
//        references.variants.insert(symbol.clone());

//        Ok(symbol)
//    } else {
//        // We couldn't find the qualified variant name in scope. NAMING PROBLEM!
//        Err(variant_name)
//    }
//}

#[inline(always)]
fn float_from_parsed(
    subs: &mut Subs,
    raw: &str,
    env: &mut Env,
    expected: Expected<Type>,
    region: Region,
) -> (Constraint, Expr) {
    // Ignore underscores.
    match raw.replace("_", "").parse::<f64>() {
        Ok(float) if float.is_finite() => (
            constrain::float_literal(subs, expected, region),
            Expr::Float(float),
        ),
        _ => {
            let runtime_error = FloatOutsideRange(raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            (True, Expr::RuntimeError(runtime_error))
        }
    }
}

#[inline(always)]
fn int_from_parsed(
    subs: &mut Subs,
    raw: &str,
    env: &mut Env,
    expected: Expected<Type>,
    region: Region,
) -> (Constraint, Expr) {
    // Ignore underscores.
    match raw.replace("_", "").parse::<i64>() {
        Ok(int) => (
            constrain::int_literal(subs, expected, region),
            Expr::Int(int),
        ),
        Err(_) => {
            let runtime_error = IntOutsideRange(raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            (True, Expr::RuntimeError(runtime_error))
        }
    }
}

#[inline(always)]
fn hex_from_parsed<'a>(
    subs: &mut Subs,
    raw: &'a str,
    env: &'a mut Env,
    expected: Expected<Type>,
    region: Region,
) -> (Constraint, Expr) {
    // Ignore underscores.
    match i64::from_str_radix(raw.replace("_", "").as_str(), 16) {
        Ok(int) => (
            constrain::int_literal(subs, expected, region),
            Expr::Int(int),
        ),
        Err(parse_err) => {
            let runtime_error = InvalidHex(parse_err, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            (True, Expr::RuntimeError(runtime_error))
        }
    }
}

#[inline(always)]
fn oct_from_parsed<'a>(
    subs: &mut Subs,
    raw: &'a str,
    env: &'a mut Env,
    expected: Expected<Type>,
    region: Region,
) -> (Constraint, Expr) {
    // Ignore underscores.
    match i64::from_str_radix(raw.replace("_", "").as_str(), 8) {
        Ok(int) => (
            constrain::int_literal(subs, expected, region),
            Expr::Int(int),
        ),
        Err(parse_err) => {
            let runtime_error = InvalidOctal(parse_err, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            (True, Expr::RuntimeError(runtime_error))
        }
    }
}

#[inline(always)]
fn bin_from_parsed<'a>(
    subs: &mut Subs,
    raw: &'a str,
    env: &'a mut Env,
    expected: Expected<Type>,
    region: Region,
) -> (Constraint, Expr) {
    // Ignore underscores.
    match i64::from_str_radix(raw.replace("_", "").as_str(), 2) {
        Ok(int) => (
            constrain::int_literal(subs, expected, region),
            Expr::Int(int),
        ),
        Err(parse_err) => {
            let runtime_error = InvalidBinary(parse_err, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            (True, Expr::RuntimeError(runtime_error))
        }
    }
}

struct Info {
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub assignment_types: ImMap<Symbol, Located<Type>>,
}

impl Info {
    pub fn with_capacity(capacity: usize) -> Self {
        Info {
            vars: Vec::with_capacity(capacity),
            constraints: Vec::with_capacity(capacity),
            assignment_types: ImMap::default(),
        }
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
// type BoundTypeVars = ImMap<Box<str>, Type>;

struct PatternState {
    assignment_types: ImMap<Symbol, Located<Type>>,
    vars: Vec<Variable>,
    reversed_constraints: Vec<Constraint>,
}

impl PatternState {
    pub fn add_pattern(
        &mut self,
        scope: &Scope,
        loc_pattern: Located<ast::Pattern>,
        expected: Expected<Type>,
    ) {
        let region = loc_pattern.region;

        match loc_pattern.value {
            ast::Pattern::Identifier(name) => {
                let symbol = scope.symbol(&name);

                self.add_to_assignment_types(region, symbol, expected)
            }
            ast::Pattern::Underscore => (),
            _ => panic!("TODO other patterns"),
        }
    }

    fn add_to_assignment_types(
        &mut self,
        region: Region,
        symbol: Symbol,
        expected: Expected<Type>,
    ) {
        self.assignment_types.insert(
            symbol,
            Located {
                region,
                value: expected.get_type(),
            },
        );
    }
}

fn add_pattern_to_lookup_types<'a>(
    scope: &Scope,
    loc_pattern: Located<ast::Pattern<'a>>,
    lookup_types: &mut ImMap<Symbol, Located<Type>>,
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

#[inline(always)]
fn can_defs<'a>(
    env: &mut Env,
    subs: &mut Subs,
    scope: Scope,
    defs: &'a bumpalo::collections::Vec<'a, (&'a [ast::CommentOrNewline<'a>], &'a Def<'a>)>,
    expected: Expected<Type>,
    loc_ret: &'a Located<ast::Expr<'a>>,
) -> (Expr, Output) {
    let mut scope = scope;

    // Add the assigned identifiers to scope. If there's a collision, it means there
    // was shadowing, which will be handled later.
    let assigned_idents: Vec<(Ident, (Symbol, Region))> = idents_from_patterns(
        defs.clone().iter().flat_map(|(_, def)| match def {
            Def::Annotation(_, _) => None,
            Def::Body(loc_pattern, _) => Some(loc_pattern),
            Def::TypeAlias(_, _) => None,
            Def::CustomType(_, _) => None,
        }),
        &scope,
    );

    scope.idents = union_pairs(scope.idents, assigned_idents.iter());

    // Used in canonicalization
    let mut refs_by_assignment: MutMap<Symbol, (Located<Ident>, References)> = MutMap::default();
    let mut can_assignments_by_symbol: MutMap<Symbol, (Located<Pattern>, Located<Expr>)> =
        MutMap::default();

    // Used in constraint generation
    let rigid_info = Info::with_capacity(defs.len());
    let mut flex_info = Info::with_capacity(defs.len());
    let mut iter = defs.iter().peekable();

    while let Some((_, def)) = iter.next() {
        // Each assignment gets to have all the idents in scope that are assigned in this
        // block. Order of assignments doesn't matter, thanks to referential transparency!
        let (opt_loc_pattern, (loc_can_expr, can_output)) = match def {
            Def::Annotation(_loc_pattern, loc_annotation) => {
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

                (None, (loc_expr, Output::new(True)))
            }
            Def::Body(loc_pattern, loc_expr) => {
                // Make types for the pattern and the body expr.
                let expr_var = subs.mk_flex_var();
                let expr_type = Type::Variable(expr_var);
                let pattern_var = subs.mk_flex_var();
                let pattern_type = Type::Variable(pattern_var);

                flex_info.vars.push(pattern_var);

                let mut state = PatternState {
                    assignment_types: ImMap::default(),
                    vars: Vec::with_capacity(1),
                    reversed_constraints: Vec::with_capacity(1),
                };

                state.add_pattern(
                    &scope,
                    loc_pattern.clone(),
                    NoExpectation(pattern_type.clone()),
                );
                state.reversed_constraints.reverse();
                let def_constraint = And(state.reversed_constraints);

                // Any time there's a lookup on this symbol in the outer Let,
                // it should result in this expression's type. After all, this
                // is the type to which this symbol is assigned!
                add_pattern_to_lookup_types(
                    &scope,
                    loc_pattern.clone(),
                    &mut flex_info.assignment_types,
                    expr_type.clone(),
                );

                let (loc_can_expr, output) = canonicalize_expr(
                    env,
                    subs,
                    &mut scope,
                    loc_expr.region,
                    &loc_expr.value,
                    NoExpectation(expr_type),
                );

                flex_info.constraints.push(Let(Box::new(LetConstraint {
                    rigid_vars: Vec::new(),
                    flex_vars: state.vars,
                    assignment_types: state.assignment_types,
                    assignments_constraint: def_constraint,
                    ret_constraint: output.constraint.clone(),
                })));

                (Some(loc_pattern), (loc_can_expr, output))
            }
            Def::CustomType(_, _) => {
                panic!("TODO error - custom types can only be defined at the toplevel")
            }
            Def::TypeAlias(_, _) => {
                panic!("TODO error - type aliases can only be defined at the toplevel")
            }
        };

        // If we have a pattern, then the def has a body (that is, it's not a
        // standalone annotation), so we need to canonicalize the pattern and expr.
        if let Some(loc_pattern) = opt_loc_pattern {
            // Exclude the current ident from shadowable_idents; you can't shadow yourself!
            // (However, still include it in scope, because you *can* recursively refer to yourself.)
            let mut shadowable_idents = scope.idents.clone();
            remove_idents(&loc_pattern.value, &mut shadowable_idents);

            let loc_can_pattern = canonicalize_pattern(
                env,
                subs,
                &mut scope,
                &Assignment,
                &loc_pattern,
                &mut shadowable_idents,
            );
            let mut renamed_closure_assignment: Option<&Symbol> = None;

            // Give closures names (and tail-recursive status) where appropriate.
            let can_expr = match (
                &loc_pattern.value,
                &loc_can_pattern.value,
                &loc_can_expr.value,
            ) {
                // First, make sure we are actually assigning an identifier instead of (for example) a variant.
                //
                // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
                // which also implies it's not a self tail call!
                //
                // Only assignments of the form (foo = ...) can be closure declarations or self tail calls.
                (
                    &ast::Pattern::Identifier(ref name),
                    &Pattern::Identifier(_, ref assigned_symbol),
                    &FunctionPointer(_, ref symbol),
                ) => {
                    // Since everywhere in the code it'll be referred to by its assigned name,
                    // remove its generated name from the procedure map. (We'll re-insert it later.)
                    let mut procedure = env.procedures.remove(&symbol).unwrap();
                    let proc_var = procedure.var;

                    // The original ident name will be used for debugging and stack traces.
                    procedure.name = Some((*name).into());

                    // The closure is self tail recursive iff it tail calls itself (by assigned name).
                    procedure.is_self_tail_recursive = match &can_output.tail_call {
                        &None => false,
                        &Some(ref symbol) => symbol == assigned_symbol,
                    };

                    // Re-insert the procedure into the map, under its assigned name. This way,
                    // when code elsewhere calls it by assigned name, it'll resolve properly.
                    env.procedures.insert(assigned_symbol.clone(), procedure);

                    // Recursion doesn't count as referencing. (If it did, all recursive functions
                    // would result in circular assignment errors!)
                    refs_by_assignment
                        .entry(assigned_symbol.clone())
                        .and_modify(|(_, refs)| {
                            refs.locals = refs.locals.without(assigned_symbol);
                        });

                    renamed_closure_assignment = Some(&assigned_symbol);

                    // Return a reference to the assigned symbol, since the auto-generated one no
                    // longer references any entry in the procedure map!
                    Var(proc_var, assigned_symbol.clone())
                }
                _ => loc_can_expr.value,
            };

            let mut assigned_symbols = Vec::new();

            // Store the referenced locals in the refs_by_assignment map, so we can later figure out
            // which assigned names reference each other.
            for (ident, (symbol, region)) in
                idents_from_patterns(std::iter::once(loc_pattern), &scope)
            {
                let refs =
                            // Functions' references don't count in assignments.
                            // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                            // parent commit for the bug this fixed!
                            if renamed_closure_assignment == Some(&symbol) {
                                References::new()
                            } else {
                                can_output.references.clone()
                            };

                refs_by_assignment.insert(
                    symbol.clone(),
                    (
                        Located {
                            value: ident,
                            region,
                        },
                        refs,
                    ),
                );

                assigned_symbols.push(symbol.clone());
            }

            for symbol in assigned_symbols {
                can_assignments_by_symbol.insert(
                    symbol,
                    (
                        loc_can_pattern.clone(),
                        Located {
                            region: loc_can_expr.region,
                            value: can_expr.clone(),
                        },
                    ),
                );
            }
        }
    }

    // The assignment as a whole is a tail call iff its return expression is a tail call.
    // Use its output as a starting point because its tail_call already has the right answer!
    let (ret_expr, mut output) = canonicalize_expr(
        env,
        subs,
        &mut scope,
        loc_ret.region,
        &loc_ret.value,
        expected,
    );

    let ret_con = output.constraint;

    // Rigid constraint for the def expr as a whole
    output.constraint = Let(Box::new(LetConstraint {
        rigid_vars: rigid_info.vars,
        flex_vars: Vec::new(),
        assignment_types: rigid_info.assignment_types,
        assignments_constraint:
            // Flex constraint
            Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: flex_info.vars,
                assignment_types: flex_info.assignment_types.clone(),
                assignments_constraint:
                    // Final flex constraints
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: Vec::new(),
                        assignment_types: flex_info.assignment_types,
                        assignments_constraint: True,
                        ret_constraint: And(flex_info.constraints)
                    })),
                ret_constraint: And(vec![And(rigid_info.constraints), ret_con])
            })),
        ret_constraint: True,
    }));

    // Determine the full set of references by traversing the graph.
    let mut visited_symbols = MutSet::default();

    // Start with the return expression's referenced locals. They are the only ones that count!
    //
    // If I have two assignments which reference each other, but neither of them
    // is referenced in the return expression, I don't want either of them (or their references)
    // to end up in the final output.references. They were unused, and so were their references!
    //
    // The reason we need a graph here is so we don't overlook transitive dependencies.
    // For example, if I have `a = b + 1` and the assignment returns `a + 1`, then the
    // assignment as a whole references both `a` *and* `b`, even though it doesn't
    // directly mention `b` - because `a` depends on `b`. If we didn't traverse a graph here,
    // we'd erroneously give a warning that `b` was unused since it wasn't directly referenced.
    for symbol in output.references.locals.clone().into_iter() {
        // Traverse the graph and look up *all* the references for this local symbol.
        let refs = references_from_local(
            symbol,
            &mut visited_symbols,
            &refs_by_assignment,
            &env.procedures,
        );

        output.references = output.references.union(refs);
    }

    for symbol in output.references.calls.clone().into_iter() {
        // Traverse the graph and look up *all* the references for this call.
        // Reuse the same visited_symbols as before; if we already visited it, we
        // won't learn anything new from visiting it again!
        let refs = references_from_call(
            symbol,
            &mut visited_symbols,
            &refs_by_assignment,
            &env.procedures,
        );

        output.references = output.references.union(refs);
    }

    // Now that we've collected all the references, check to see if any of the new idents
    // we defined went unused by the return expression. If any were unused, report it.
    for (ident, (symbol, region)) in assigned_idents.clone() {
        if !output.references.has_local(&symbol) {
            let loc_ident = Located {
                region: region,
                value: ident.clone(),
            };

            env.problem(Problem::UnusedAssignment(loc_ident));
        }
    }

    // Use topological sort to reorder the assignments based on their dependencies to one another.
    // This way, during code gen, no assignment will refer to a value that hasn't been initialized yet.
    // As a bonus, the topological sort also reveals any cycles between the assignments, allowing
    // us to give a CircularAssignment error.
    let successors = |symbol: &Symbol| -> ImSet<Symbol> {
        let (_, references) = refs_by_assignment.get(symbol).unwrap();

        local_successors(&references, &env.procedures)
    };

    let mut assigned_symbols: Vec<Symbol> = Vec::new();

    for symbol in can_assignments_by_symbol.keys().into_iter() {
        assigned_symbols.push(symbol.clone())
    }

    match topological_sort(assigned_symbols.as_slice(), successors) {
        Ok(sorted_symbols) => {
            let mut can_assignments = Vec::new();

            for symbol in sorted_symbols
                .into_iter()
                // Topological sort gives us the reverse of the sorting we want!
                .rev()
            {
                can_assignments.push(can_assignments_by_symbol.get(&symbol).unwrap().clone());
            }

            (
                Defs(subs.mk_flex_var(), can_assignments, Box::new(ret_expr)),
                output,
            )
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
                loc_idents_in_cycle.push(refs_by_assignment.get(&symbol).unwrap().0.clone());
            }

            // Sort them to make the report more helpful.
            loc_idents_in_cycle = sort_cyclic_idents(
                loc_idents_in_cycle,
                &mut assigned_idents.iter().map(|(ident, _)| ident),
            );

            env.problem(Problem::CircularAssignment(loc_idents_in_cycle.clone()));

            let mut regions = Vec::with_capacity(can_assignments_by_symbol.len());

            for (loc_pattern, loc_expr) in can_assignments_by_symbol.values() {
                regions.push((loc_pattern.region, loc_expr.region));
            }

            (
                RuntimeError(CircularAssignment(
                    loc_idents_in_cycle,
                    regions,
                    ret_expr.region,
                )),
                output,
            )
        }
    }
}

struct Args {
    pub vars: Vec<Variable>,
    pub typ: Type,
    pub ret_type: Type,
    pub ret_var: Variable,
}

fn constrain_args<'a, I>(args: I, scope: &Scope, subs: &mut Subs, state: &mut PatternState) -> Args
where
    I: Iterator<Item = &'a Located<ast::Pattern<'a>>>,
{
    let (mut vars, arg_types) = patterns_to_variables(args.into_iter(), scope, subs, state);

    let ret_var = subs.mk_flex_var();
    let ret_type = Type::Variable(ret_var);

    vars.push(ret_var);

    let typ = Type::Function(arg_types, Box::new(ret_type.clone()));

    Args {
        vars,
        typ,
        ret_type,
        ret_var,
    }
}

fn patterns_to_variables<'a, I>(
    patterns: I,
    scope: &Scope,
    subs: &mut Subs,
    state: &mut PatternState,
) -> (Vec<Variable>, Vec<Type>)
where
    I: Iterator<Item = &'a Located<ast::Pattern<'a>>>,
{
    let mut vars = Vec::with_capacity(state.vars.capacity());
    let mut pattern_types = Vec::with_capacity(state.vars.capacity());

    for loc_pattern in patterns {
        let pattern_var = subs.mk_flex_var();
        let pattern_type = Type::Variable(pattern_var);

        state.add_pattern(
            scope,
            loc_pattern.clone(),
            NoExpectation(pattern_type.clone()),
        );

        vars.push(pattern_var);
        pattern_types.push(pattern_type);
    }

    (vars, pattern_types)
}

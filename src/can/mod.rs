use self::env::Env;
use self::expr::Expr::{self, *};
use self::expr::Recursive;
use self::num::{
    finish_parsing_bin, finish_parsing_float, finish_parsing_hex, finish_parsing_int,
    finish_parsing_oct, float_expr_from_result, int_expr_from_result,
};
use self::pattern::PatternState;
use self::pattern::PatternType::*;
use self::pattern::{canonicalize_pattern, Pattern};
use self::problem::Problem;
use self::problem::RuntimeError::*;
use self::procedure::References;
use self::scope::Scope;
use self::symbol::Symbol;
use crate::can::pattern::PatternType;
use crate::collections::{ImMap, ImSet, MutMap, MutSet};
use crate::constrain::{self, exists};
use crate::graph::{strongly_connected_component, topological_sort};
use crate::ident::Ident;
use crate::parse::ast::{self, Def};
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::AnnotationSource::*;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::Type::{self, *};
use crate::types::{LetConstraint, PExpected, PReason, Reason};
use bumpalo::Bump;
use im::Vector;
use std::fmt::Debug;

pub mod env;
pub mod expr;
pub mod module;
pub mod num;
pub mod operator;
pub mod pattern;
pub mod problem;
pub mod procedure;
pub mod scope;
pub mod string;
pub mod symbol;

/// Whenever we encounter a user-defined type variable (a "rigid" var for short),
/// for example `a` in the annotation `identity : a -> a`, we add it to this
/// map so that expressions within that annotation can share these vars.
type Rigids = ImMap<Box<str>, Type>;

pub fn canonicalize_module_defs<'a>(
    arena: &Bump,
    loc_defs: bumpalo::collections::Vec<'a, Located<Def<'a>>>,
    home: Box<str>,
    scope: &mut ImMap<Box<str>, (Symbol, Region)>,
    var_store: &VarStore,
) -> Vector<(Located<Pattern>, Located<Expr>)> {
    let mut buf = Vector::new();

    for loc_def in loc_defs {
        buf.push_back(canonicalize_def(
            arena,
            &loc_def.value,
            loc_def.region,
            home.clone(),
            scope,
            var_store,
        ));
    }

    buf
}

fn canonicalize_def<'a>(
    arena: &Bump,
    def: &'a Def<'a>,
    region: Region,
    home: Box<str>,
    scope: &mut ImMap<Box<str>, (Symbol, Region)>,
    var_store: &VarStore,
) -> (Located<Pattern>, Located<Expr>) {
    match def {
        Def::Annotation(_loc_pattern, _loc_ann) => {
            panic!("TODO canonicalize top-level annotations");
        }
        Def::Body(loc_pattern, loc_expr) => {
            let variable = var_store.fresh();
            let expected = Expected::NoExpectation(Type::Variable(variable));
            let declared_idents = ImMap::default(); // TODO FIXME infer this from scope arg
            let name: Box<str> = "TODOfixme".into();

            // Desugar operators (convert them to Apply calls, taking into account
            // operator precedence and associativity rules), before doing other canonicalization.
            //
            // If we did this *during* canonicalization, then each time we
            // visited a BinOp node we'd recursively try to apply this to each of its nested
            // operators, and then again on *their* nested operators, ultimately applying the
            // rules multiple times unnecessarily.
            let loc_expr = operator::desugar(arena, &loc_expr);

            // If we're canonicalizing the declaration `foo = ...` inside the `Main` module,
            // scope_prefix will be "Main.foo$" and its first closure will be named "Main.foo$0"
            let scope_prefix = format!("{}.{}$", home, name).into();
            let mut scope = Scope::new(scope_prefix, declared_idents.clone());
            let mut env = Env::new(home);
            let (loc_expr, _) = canonicalize_expr(
                &ImMap::default(),
                &mut env,
                var_store,
                &mut scope,
                region,
                &loc_expr.value,
                expected,
            );

            // Exclude the current ident from shadowable_idents; you can't shadow yourself!
            // (However, still include it in scope, because you *can* recursively refer to yourself.)
            let mut shadowable_idents = scope.idents.clone();
            remove_idents(&loc_pattern.value, &mut shadowable_idents);

            let pattern_var = var_store.fresh();
            let pattern_type = Type::Variable(pattern_var);
            let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

            let mut pattern_state = PatternState {
                headers: ImMap::default(),
                vars: Vec::with_capacity(1),
                constraints: Vec::with_capacity(1),
            };
            let loc_pattern = canonicalize_pattern(
                &mut env,
                &mut pattern_state,
                var_store,
                &mut scope,
                PatternType::TopLevelDef,
                &loc_pattern.value,
                loc_pattern.region,
                &mut shadowable_idents,
                pattern_expected,
            );

            (loc_pattern, loc_expr)
        }
        Def::CustomType(_, _) => {
            panic!("TODO remove CustomType syntax");
        }
        Def::TypeAlias(_, _) => {
            panic!("TODO remove TypeAlias syntax");
        }

        // Ignore spaces
        Def::SpaceBefore(def, _) | Def::SpaceAfter(def, _) => {
            canonicalize_def(arena, def, region, home, scope, var_store)
        }
    }
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
pub fn canonicalize_declaration<'a>(
    arena: &Bump,
    var_store: &VarStore,
    home: Box<str>,
    name: Box<str>,
    region: Region,
    loc_expr: Located<ast::Expr<'a>>,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
    expected: Expected<Type>,
) -> (Located<Expr>, Output, Vec<Problem>) {
    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let loc_expr = operator::desugar(arena, &loc_expr);

    // If we're canonicalizing the declaration `foo = ...` inside the `Main` module,
    // scope_prefix will be "Main.foo$" and its first closure will be named "Main.foo$0"
    let scope_prefix = format!("{}.{}$", home, name).into();
    let mut scope = Scope::new(scope_prefix, declared_idents.clone());
    let mut env = Env::new(home);
    let (loc_expr, output) = canonicalize_expr(
        &ImMap::default(),
        &mut env,
        var_store,
        &mut scope,
        region,
        &loc_expr.value,
        expected,
    );

    (loc_expr, output, env.problems)
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
    rigids: &Rigids,
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    region: Region,
    expr: &ast::Expr,
    expected: Expected<Type>,
) -> (Located<Expr>, Output) {
    use self::Expr::*;

    let (expr, output) = match expr {
        ast::Expr::Int(string) => {
            let (constraint, answer) =
                int_expr_from_result(var_store, finish_parsing_int(string), env, expected, region);

            (answer, Output::new(constraint))
        }
        ast::Expr::Float(string) => {
            let (constraint, answer) = float_expr_from_result(
                var_store,
                finish_parsing_float(string),
                env,
                expected,
                region,
            );

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
        ast::Expr::BlockStr(lines) => {
            let constraint = Eq(constrain::str_type(), expected, region);
            let joined = lines.iter().copied().collect::<Vec<&str>>().join("\n");

            (BlockStr(joined.into()), Output::new(constraint))
        }
        ast::Expr::List(loc_elems) => {
            if loc_elems.is_empty() {
                let list_var = var_store.fresh();
                let constraint = Eq(constrain::empty_list_type(list_var), expected, region);

                (List(list_var, Vec::new()), Output::new(constraint))
            } else {
                let mut can_elems = Vec::with_capacity(loc_elems.len());
                let list_var = var_store.fresh(); // `v` in the type (List v)
                let list_type = Type::Variable(list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));
                let mut references = References::new();

                for loc_elem in loc_elems.iter() {
                    let elem_var = var_store.fresh();
                    let elem_type = Variable(elem_var);
                    let elem_expected = NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_type.clone(),
                        ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let (can_expr, elem_out) = canonicalize_expr(
                        rigids,
                        env,
                        var_store,
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
        //    panic!("TODO Emit a case-expression with False first for the else branch, then _");
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
            let fn_var = var_store.fresh();
            let fn_type = Variable(fn_var);
            let fn_region = loc_fn.region;
            let fn_expected = NoExpectation(fn_type.clone());
            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall {
                arity: loc_args.len() as u8,
            };

            // Canonicalize the function expression and its arguments
            let (fn_expr, mut output) = canonicalize_expr(
                rigids,
                env,
                var_store,
                scope,
                loc_fn.region,
                &loc_fn.value,
                fn_expected,
            );

            // The function's return type
            let ret_var = var_store.fresh();
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
                let arg_var = var_store.fresh();
                let arg_type = Variable(arg_var);
                // TODO look up the name and use NamedFnArg if possible.
                let reason = Reason::AnonymousFnArg {
                    arg_index: index as u8,
                };
                let expected_arg = ForReason(reason, arg_type.clone(), region);
                let (arg_expr, arg_out) = canonicalize_expr(
                    rigids,
                    env,
                    var_store,
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

            // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
            output.tail_call = None;

            let expr = match fn_expr.value {
                Var(_, ref sym) | FunctionPointer(_, ref sym) => {
                    // In the FunctionPointer case, we're calling an inline closure;
                    // something like ((\a b -> a + b) 1 2).
                    output.references.calls.insert(sym.clone());

                    // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                    output.tail_call = match &env.tailcallable_symbol {
                        Some(tc_sym) if tc_sym == sym => Some(sym.clone()),
                        Some(_) | None => None,
                    };

                    Call(Box::new(fn_expr.value), args, *application_style)
                }
                RuntimeError(_) => {
                    // We can't call a runtime error; bail out by propagating it!
                    return (fn_expr, output);
                }
                not_var => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    Call(Box::new(not_var), args, *application_style)
                }
            };

            for arg_out in outputs {
                output.references = output.references.union(arg_out.references);
            }

            let fn_con = output.constraint;
            let expected_fn_type = ForReason(
                fn_reason,
                Function(arg_types, Box::new(ret_type.clone())),
                region,
            );

            output.constraint = exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(fn_type, expected_fn_type, fn_region),
                    And(arg_cons),
                    Eq(ret_type, expected, region),
                ]),
            );

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
                Ok(symbol) => Var(var_store.fresh(), symbol),
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
        ast::Expr::Defs(defs, loc_ret) => {
            // The body expression gets a new scope for canonicalization,
            // so clone it.
            can_defs(
                rigids,
                env,
                var_store,
                scope.clone(),
                defs,
                expected,
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

            let arg_idents: Vec<(Ident, (Symbol, Region))> =
                idents_from_patterns(loc_arg_patterns.iter(), &scope);

            // Add the arguments' idents to scope.idents. If there's a collision,
            // it means there was shadowing, which will be handled later.
            scope.idents = union_pairs(scope.idents, arg_idents.iter());

            let mut state = PatternState {
                headers: ImMap::default(),
                vars: Vec::with_capacity(loc_arg_patterns.len()),
                constraints: Vec::with_capacity(1),
            };
            let mut can_args: Vec<Located<Pattern>> = Vec::with_capacity(loc_arg_patterns.len());
            let mut vars = Vec::with_capacity(state.vars.capacity());
            let mut pattern_types = Vec::with_capacity(state.vars.capacity());

            for loc_pattern in loc_arg_patterns.into_iter() {
                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(&loc_pattern.value, &mut shadowable_idents);

                let pattern_var = var_store.fresh();
                let pattern_type = Type::Variable(pattern_var);
                let pattern_expected = PExpected::NoExpectation(pattern_type.clone());

                pattern_types.push(pattern_type);

                let can_arg = canonicalize_pattern(
                    env,
                    &mut state,
                    var_store,
                    &mut scope,
                    FunctionArg,
                    &loc_pattern.value,
                    loc_pattern.region,
                    &mut shadowable_idents,
                    pattern_expected,
                );

                vars.push(pattern_var);

                can_args.push(can_arg);
            }

            let ret_var = var_store.fresh();
            let ret_type = Type::Variable(ret_var);

            state.vars.push(ret_var);

            let fn_typ = Type::Function(pattern_types, Box::new(ret_type.clone()));

            let body_type = NoExpectation(ret_type);
            let (loc_body_expr, mut output) = canonicalize_expr(
                rigids,
                env,
                var_store,
                &mut scope,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

            let defs_constraint = And(state.constraints);
            let ret_constraint = output.constraint;

            // panic!("TODO occurs check");

            output.constraint = exists(
                state.vars.clone(),
                And(vec![
                    Let(Box::new(LetConstraint {
                        rigid_vars: Vec::new(),
                        flex_vars: state.vars,
                        def_types: state.headers,
                        defs_constraint: defs_constraint,
                        ret_constraint,
                    })),
                    // "the closure's type is equal to expected  type"
                    Eq(fn_typ, expected, region),
                ]),
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
                    symbol,
                    Recursive::NotRecursive,
                    can_args,
                    Box::new(loc_body_expr),
                ),
                output,
            )
        }

        ast::Expr::Case(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = var_store.fresh();
            let cond_type = Variable(cond_var);
            let (can_cond, mut output) = canonicalize_expr(
                rigids,
                env,
                var_store,
                scope,
                region,
                &loc_cond.value,
                NoExpectation(cond_type.clone()),
            );

            // the condition can never be a tail-call
            output.tail_call = None;

            let mut can_branches = Vec::with_capacity(branches.len());
            let mut constraints = Vec::with_capacity(branches.len() + 1);
            let expr_con = output.constraint.clone();

            match expected {
                FromAnnotation(name, arity, _, typ) => {
                    for (index, (loc_pattern, loc_expr)) in branches.into_iter().enumerate() {
                        let mut shadowable_idents = scope.idents.clone();
                        remove_idents(&loc_pattern.value, &mut shadowable_idents);
                        let (can_pattern, loc_can_expr, branch_con, branch_references) =
                            canonicalize_case_branch(
                                env,
                                var_store,
                                rigids,
                                scope,
                                region,
                                loc_pattern,
                                loc_expr,
                                PExpected::ForReason(
                                    PReason::CaseMatch { index },
                                    cond_type.clone(),
                                    region,
                                ),
                                FromAnnotation(
                                    name.clone(),
                                    arity,
                                    TypedCaseBranch(index),
                                    typ.clone(),
                                ),
                                &mut output,
                            );

                        output.references = output.references.union(branch_references);

                        can_branches.push((can_pattern, loc_can_expr));

                        constraints.push(exists(
                            vec![cond_var],
                            // Each branch's pattern must have the same type
                            // as the condition expression did.
                            And(vec![expr_con.clone(), branch_con]),
                        ));
                    }
                }

                _ => {
                    let branch_var = var_store.fresh();
                    let branch_type = Variable(branch_var);
                    let mut branch_cons = Vec::with_capacity(branches.len());

                    for (index, (loc_pattern, loc_expr)) in branches.into_iter().enumerate() {
                        let mut shadowable_idents = scope.idents.clone();

                        remove_idents(&loc_pattern.value, &mut shadowable_idents);

                        let (can_pattern, loc_can_expr, branch_con, branch_references) =
                            canonicalize_case_branch(
                                env,
                                var_store,
                                rigids,
                                scope,
                                region,
                                loc_pattern,
                                loc_expr,
                                PExpected::ForReason(
                                    PReason::CaseMatch { index },
                                    cond_type.clone(),
                                    region,
                                ),
                                ForReason(
                                    Reason::CaseBranch { index },
                                    branch_type.clone(),
                                    region,
                                ),
                                &mut output,
                            );

                        output.references = output.references.union(branch_references);

                        can_branches.push((can_pattern, loc_can_expr));

                        branch_cons.push(branch_con);
                    }

                    constraints.push(exists(
                        vec![cond_var],
                        And(vec![
                            // Record the original conditional expression's constraint.
                            expr_con.clone(),
                            // Each branch's pattern must have the same type
                            // as the condition expression did.
                            And(branch_cons),
                            // The return type of each branch must equal
                            // the return type of the entire case-expression.
                            Eq(branch_type, expected, region),
                        ]),
                    ));
                }
            }

            // A case with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happend to be one. (The condition gave us our initial output value.)
            if branches.is_empty() {
                output.tail_call = None;
            }

            output.constraint = And(constraints);

            // Incorporate all three expressions into a combined Output value.
            let expr = Case(cond_var, Box::new(can_cond), can_branches);

            // TODO check for exhaustiveness. If this `case` is non-exaustive, then:
            //
            // 1. Record a Problem.
            // 2. Add an extra _ branch at the end which throws a runtime error.

            (expr, output)
        }
        ast::Expr::Access(_, _)
        | ast::Expr::AccessorFunction(_)
        | ast::Expr::If(_)
        | ast::Expr::GlobalTag(_)
        | ast::Expr::PrivateTag(_)
        | ast::Expr::MalformedIdent(_)
        | ast::Expr::MalformedClosure
        | ast::Expr::PrecedenceConflict(_, _, _) => {
            panic!(
                "TODO restore the rest of canonicalize()'s branches {:?}",
                local_successors(&References::new(), &env.closures)
            );
        }
        ast::Expr::Nested(sub_expr) => {
            let (answer, output) =
                canonicalize_expr(rigids, env, var_store, scope, region, sub_expr, expected);

            (answer.value, output)
        }
        ast::Expr::BinaryInt(string) => {
            let (constraint, answer) =
                int_expr_from_result(var_store, finish_parsing_bin(string), env, expected, region);
            (answer, Output::new(constraint))
        }
        ast::Expr::HexInt(string) => {
            let (constraint, answer) =
                int_expr_from_result(var_store, finish_parsing_hex(string), env, expected, region);
            (answer, Output::new(constraint))
        }
        ast::Expr::OctalInt(string) => {
            let (constraint, answer) =
                int_expr_from_result(var_store, finish_parsing_oct(string), env, expected, region);
            (answer, Output::new(constraint))
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

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn canonicalize_case_branch<'a>(
    env: &mut Env,
    var_store: &VarStore,
    rigids: &Rigids,
    scope: &Scope,
    region: Region,
    loc_pattern: &Located<ast::Pattern<'a>>,
    loc_expr: &Located<ast::Expr<'a>>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
    output: &mut Output,
) -> (Located<Pattern>, Located<Expr>, Constraint, References) {
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
    let defined_idents: Vec<(Ident, (Symbol, Region))> =
        idents_from_patterns(std::iter::once(loc_pattern), &scope);

    scope.idents = union_pairs(scope.idents, defined_idents.iter());

    let (can_expr, branch_output) = canonicalize_expr(
        rigids,
        env,
        var_store,
        &mut scope,
        region,
        &loc_expr.value,
        expr_expected,
    );

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

    let mut state = PatternState {
        headers: ImMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    let loc_can_pattern = canonicalize_pattern(
        env,
        &mut state,
        var_store,
        &mut scope,
        CaseBranch,
        &loc_pattern.value,
        loc_pattern.region,
        &mut shadowable_idents,
        pattern_expected,
    );

    let constraint = Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: state.vars,
        def_types: state.headers,
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint: branch_output.constraint,
    }));

    (
        loc_can_pattern,
        can_expr,
        constraint,
        branch_output.references,
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

fn references_from_local<'a, T>(
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
    I: Debug,
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
                "Could not find any idents that appear in both loc_idents {:?} and ordered_idents {:?}",
                loc_idents, ordered_idents
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

fn references_from_call<'a, T>(
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
    use crate::parse::ast::Pattern::*;

    match pattern {
        Identifier(name) => {
            let symbol = scope.symbol(&name);

            answer.push((Ident::Unqualified(name.to_string()), (symbol, *region)));
        }
        QualifiedIdentifier(_name) => {
            panic!("TODO implement QualifiedIdentifier pattern.");
        }
        Apply(_, _) => {
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

        RecordDestructure(_) => {
            panic!("TODO implement RecordDestructure pattern in add_idents_from_pattern.");
        }
        RecordField(_, _) => {
            panic!("TODO implement RecordField pattern in add_idents_from_pattern.");
        }
        SpaceBefore(pattern, _) | SpaceAfter(pattern, _) | Nested(pattern) => {
            // Ignore the newline/comment info; it doesn't matter in canonicalization.
            add_idents_from_pattern(region, pattern, scope, answer)
        }
        GlobalTag(_) | PrivateTag(_) | IntLiteral(_) | HexIntLiteral(_) | OctalIntLiteral(_)
        | BinaryIntLiteral(_) | FloatLiteral(_) | StrLiteral(_) | BlockStrLiteral(_)
        | EmptyRecordLiteral | Malformed(_) | Underscore => (),
    }
}

fn remove_idents(pattern: &ast::Pattern, idents: &mut ImMap<Ident, (Symbol, Region)>) {
    use crate::parse::ast::Pattern::*;

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
        RecordField(_, _) => {
            panic!("TODO implement RecordField pattern in remove_idents.");
        }
        SpaceBefore(pattern, _) | SpaceAfter(pattern, _) | Nested(pattern) => {
            // Ignore the newline/comment info; it doesn't matter in canonicalization.
            remove_idents(pattern, idents)
        }
        GlobalTag(_) | PrivateTag(_) | IntLiteral(_) | HexIntLiteral(_) | BinaryIntLiteral(_)
        | OctalIntLiteral(_) | FloatLiteral(_) | StrLiteral(_) | BlockStrLiteral(_)
        | EmptyRecordLiteral | Malformed(_) | Underscore => {}
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

struct Info {
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub def_types: ImMap<Symbol, Located<Type>>,
}

impl Info {
    pub fn with_capacity(capacity: usize) -> Self {
        Info {
            vars: Vec::with_capacity(capacity),
            constraints: Vec::with_capacity(capacity),
            def_types: ImMap::default(),
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

fn pattern_from_def<'a>(def: &'a Def<'a>) -> Option<&'a Located<ast::Pattern<'a>>> {
    match def {
        Def::Annotation(_, _) => None,
        Def::Body(ref loc_pattern, _) => Some(loc_pattern),
        Def::TypeAlias(_, _) => None,
        Def::CustomType(_, _) => None,
        Def::SpaceBefore(ref other_def, _) => pattern_from_def(other_def),
        Def::SpaceAfter(ref other_def, _) => pattern_from_def(other_def),
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

#[inline(always)]
fn can_defs<'a>(
    rigids: &Rigids,
    env: &mut Env,
    var_store: &VarStore,
    scope: Scope,
    defs: &'a bumpalo::collections::Vec<'a, &'a Located<Def<'a>>>,
    expected: Expected<Type>,
    loc_ret: &'a Located<ast::Expr<'a>>,
) -> (Expr, Output) {
    let mut scope = scope;

    // Add the defined identifiers to scope. If there's a collision, it means there
    // was shadowing, which will be handled later.
    let defined_idents: Vec<(Ident, (Symbol, Region))> = idents_from_patterns(
        // TODO can we get rid of this clone? It's recursively cloning expressions...
        defs.iter()
            .flat_map(|loc_def| pattern_from_def(&loc_def.value)),
        &scope,
    );

    scope.idents = union_pairs(scope.idents, defined_idents.iter());

    // Used in canonicalization
    let mut refs_by_def: MutMap<Symbol, (Located<Ident>, References)> = MutMap::default();
    let mut can_defs_by_symbol: MutMap<Symbol, (Located<Pattern>, Located<Expr>)> =
        MutMap::default();

    // Used in constraint generation
    let rigid_info = Info::with_capacity(defs.len());
    let mut flex_info = Info::with_capacity(defs.len());
    let iter = defs.iter();

    for loc_def in iter {
        // Make types for the body expr, even if we won't end up having a body.
        let expr_var = var_store.fresh();
        let expr_type = Type::Variable(expr_var);

        // Each def gets to have all the idents in scope that are defined in this
        // block. Order of defs doesn't matter, thanks to referential transparency!
        let (_opt_loc_pattern, (_loc_can_expr, _can_output)) = match loc_def.value {
            Def::Annotation(ref _loc_pattern, ref loc_annotation) => {
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
            // If we have a pattern, then the def has a body (that is, it's not a
            // standalone annotation), so we need to canonicalize the pattern and expr.
            Def::Body(ref loc_pattern, loc_expr) => {
                // Exclude the current ident from shadowable_idents; you can't shadow yourself!
                // (However, still include it in scope, because you *can* recursively refer to yourself.)
                let mut shadowable_idents = scope.idents.clone();
                remove_idents(&loc_pattern.value, &mut shadowable_idents);

                let pattern_var = var_store.fresh();
                let pattern_type = Type::Variable(pattern_var);
                let pattern_expected = PExpected::NoExpectation(pattern_type);

                let mut state = PatternState {
                    headers: ImMap::default(),
                    vars: Vec::with_capacity(1),
                    constraints: Vec::with_capacity(1),
                };

                let loc_can_pattern = canonicalize_pattern(
                    env,
                    &mut state,
                    var_store,
                    &mut scope,
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
                    // TODO can we we avoid this clone?
                    loc_pattern.clone(),
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
                };

                let (mut loc_can_expr, can_output) = canonicalize_expr(
                    rigids,
                    env,
                    var_store,
                    &mut scope,
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
                    ret_constraint: can_output.constraint.clone(),
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
                    refs_by_def
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

                // Store the referenced locals in the refs_by_def map, so we can later figure out
                // which defined names reference each other.
                for (ident, (symbol, region)) in
                    idents_from_patterns(std::iter::once(loc_pattern), &scope)
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

                    refs_by_def.insert(
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
                        (
                            loc_can_pattern.clone(),
                            Located {
                                region: loc_can_expr.region,
                                value: loc_can_expr.value.clone(),
                            },
                        ),
                    );
                }

                (Some(loc_pattern), (loc_can_expr, can_output))
            }
            Def::CustomType(_, _) => {
                panic!("TODO error - custom types can only be defined at the toplevel")
            }
            Def::TypeAlias(_, _) => {
                panic!("TODO error - type aliases can only be defined at the toplevel")
            }
            Def::SpaceBefore(_, _) => panic!("TODO support SpaceBefore in canonical defs"),
            Def::SpaceAfter(_, _) => panic!("TODO support SpaceAfter in canonical defs"),
        };
    }

    // The def as a whole is a tail call iff its return expression is a tail call.
    // Use its output as a starting point because its tail_call already has the right answer!
    let (ret_expr, mut output) = canonicalize_expr(
        rigids,
        env,
        var_store,
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

    // Determine the full set of references by traversing the graph.
    let mut visited_symbols = MutSet::default();

    let returned_locals = output.references.locals.clone();
    // Start with the return expression's referenced locals. They are the only ones that count!
    //
    // If I have two defs which reference each other, but neither of them
    // is referenced in the return expression, I don't want either of them (or their references)
    // to end up in the final output.references. They were unused, and so were their references!
    //
    // The reason we need a graph here is so we don't overlook transitive dependencies.
    // For example, if I have `a = b + 1` and the def returns `a + 1`, then the
    // def as a whole references both `a` *and* `b`, even though it doesn't
    // directly mention `b` - because `a` depends on `b`. If we didn't traverse a graph here,
    // we'd erroneously give a warning that `b` was unused since it wasn't directly referenced.
    for symbol in returned_locals.into_iter() {
        // Traverse the graph and look up *all* the references for this local symbol.
        let refs = references_from_local(symbol, &mut visited_symbols, &refs_by_def, &env.closures);

        output.references = output.references.union(refs);
    }

    for symbol in output.references.calls.clone().into_iter() {
        // Traverse the graph and look up *all* the references for this call.
        // Reuse the same visited_symbols as before; if we already visited it, we
        // won't learn anything new from visiting it again!
        let refs = references_from_call(symbol, &mut visited_symbols, &refs_by_def, &env.closures);

        output.references = output.references.union(refs);
    }

    // Now that we've collected all the references, check to see if any of the new idents
    // we defined went unused by the return expression. If any were unused, report it.
    for (ident, (symbol, region)) in defined_idents.clone() {
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
        // This may not be in refs_by_def. For example, the `f` in `f x` here:
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
        match refs_by_def.get(symbol) {
            Some((_, references)) => local_successors(&references, &env.closures),
            None => ImSet::default(),
        }
    };

    let mut defined_symbols: Vec<Symbol> = Vec::new();

    for symbol in can_defs_by_symbol.keys().into_iter() {
        defined_symbols.push(symbol.clone())
    }

    match topological_sort(defined_symbols.as_slice(), successors) {
        Ok(sorted_symbols) => {
            let mut can_defs = Vec::new();

            for symbol in sorted_symbols
                .into_iter()
                // Topological sort gives us the reverse of the sorting we want!
                .rev()
            {
                if let Some(can_def) = can_defs_by_symbol.get(&symbol) {
                    // Determine recursivity of closures that are not tail-recursive
                    if let Closure(name, Recursive::NotRecursive, args, body) =
                        can_def.1.value.clone()
                    {
                        let recursion = closure_recursivity(symbol.clone(), &env.closures);

                        let mut new_def = can_def.clone();
                        new_def.1.value = Closure(name, recursion, args, body);
                        can_defs.push(new_def);
                        continue;
                    }
                    can_defs.push(can_def.clone());
                }
            }

            (
                Defs(var_store.fresh(), can_defs, Box::new(ret_expr)),
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
                let refs = refs_by_def.get(&symbol).unwrap_or_else(|| {
                    panic!(
                        "Symbol not found in refs_by_def: {:?} - refs_by_def was: {:?}",
                        symbol, refs_by_def
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

            for (loc_pattern, loc_expr) in can_defs_by_symbol.values() {
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

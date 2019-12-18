use crate::can::def::{can_defs_with_return, Def, Info};
use crate::can::env::Env;
use crate::can::ident::Lowercase;
use crate::can::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_int, float_expr_from_result,
    int_expr_from_result,
};
use crate::can::pattern::PatternType::*;
use crate::can::pattern::{canonicalize_pattern, remove_idents, Pattern};
use crate::can::pattern::{idents_from_patterns, PatternState};
use crate::can::problem::Problem;
use crate::can::problem::RuntimeError;
use crate::can::problem::RuntimeError::*;
use crate::can::procedure::References;
use crate::can::scope::Scope;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, ImSet, MutMap, MutSet, SendMap};
use crate::constrain::{self, exists};
use crate::ident::Ident;
use crate::operator::CalledVia;
use crate::parse::ast;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::AnnotationSource::*;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::Type::{self, *};
use crate::types::{LetConstraint, PExpected, PReason, Reason};
use im_rc::Vector;
use std::fmt::Debug;
use std::i64;
use std::ops::Neg;

/// Whenever we encounter a user-defined type variable (a "rigid" var for short),
/// for example `a` in the annotation `identity : a -> a`, we add it to this
/// map so that expressions within that annotation can share these vars.
pub type Rigids = ImMap<Box<str>, Type>;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub rigids: SendMap<Variable, Lowercase>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    Str(Box<str>),
    BlockStr(Box<str>),
    List(Variable, Vec<Located<Expr>>),

    // Lookups
    Var(Variable, Symbol),
    /// Works the same as Var, but has an important marking purpose.
    /// See 13623e3f5f65ea2d703cf155f16650c1e8246502 for the bug this fixed.
    FunctionPointer(Variable, Symbol),

    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access(Box<Located<Expr>>, Box<str>),

    Tag(Box<str>, Vec<Expr>),

    // Pattern Matching
    /// Case is guaranteed to be exhaustive at this point. (If it wasn't, then
    /// a _ branch was added at the end that will throw a runtime error.)
    /// Also, `If` is desugared into `Case` matching on `False` and `_` at this point.
    Case(
        Variable,
        Box<Located<Expr>>,
        Vec<(Located<Pattern>, Located<Expr>)>,
    ),
    Defs(Variable, Vec<Def>, Box<Located<Expr>>),

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call(Box<Expr>, Vec<Located<Expr>>, CalledVia),

    Closure(Symbol, Recursive, Vec<Located<Pattern>>, Box<Located<Expr>>),

    // Product Types
    Record(Variable, SendMap<Lowercase, Located<Expr>>),
    EmptyRecord,

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Recursive {
    Recursive,
    TailRecursive,
    NotRecursive,
}

pub fn canonicalize_expr(
    rigids: &Rigids,
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    region: Region,
    expr: &ast::Expr,
    expected: Expected<Type>,
) -> (Located<Expr>, Output, Constraint) {
    use self::Expr::*;

    let (expr, output, constraint) = match expr {
        ast::Expr::Int(string) => {
            let (constraint, answer) =
                int_expr_from_result(var_store, finish_parsing_int(string), env, expected, region);

            (answer, Output::default(), constraint)
        }
        ast::Expr::Float(string) => {
            let (constraint, answer) = float_expr_from_result(
                var_store,
                finish_parsing_float(string),
                env,
                expected,
                region,
            );

            (answer, Output::default(), constraint)
        }
        ast::Expr::Record(fields) => {
            if fields.is_empty() {
                let constraint = Eq(EmptyRec, expected, region);

                (EmptyRecord, Output::default(), constraint)
            } else {
                let mut field_exprs = SendMap::default();
                let mut field_types = SendMap::default();
                let mut field_vars = Vec::with_capacity(fields.len());

                // Constraints need capacity for each field + 1 for the record itself.
                let mut constraints = Vec::with_capacity(1 + fields.len());
                let mut output = Output::default();

                for loc_field in fields.iter() {
                    let (label, field_expr, field_out, field_var, field_type, field_con) =
                        canonicalize_field(
                            rigids,
                            env,
                            var_store,
                            scope,
                            &loc_field.value,
                            loc_field.region,
                        );

                    field_vars.push(field_var);
                    field_exprs.insert(label.clone(), field_expr);
                    field_types.insert(label, field_type);

                    constraints.push(field_con);
                    output.references = output.references.union(field_out.references);
                }

                let record_var = var_store.fresh();
                let record_type = Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(Type::EmptyRec),
                );
                let record_con = Eq(record_type, expected, region);

                constraints.push(record_con);

                let constraint = exists(field_vars, And(constraints));

                (Record(record_var, field_exprs), output, constraint)
            }
        }
        ast::Expr::Str(string) => {
            let constraint = Eq(constrain::str_type(), expected, region);

            (Str((*string).into()), Output::default(), constraint)
        }
        ast::Expr::BlockStr(lines) => {
            let constraint = Eq(constrain::str_type(), expected, region);
            let joined = lines.iter().copied().collect::<Vec<&str>>().join("\n");

            (BlockStr(joined.into()), Output::default(), constraint)
        }
        ast::Expr::List(loc_elems) => {
            if loc_elems.is_empty() {
                let list_var = var_store.fresh();
                let constraint = Eq(constrain::empty_list_type(list_var), expected, region);

                (List(list_var, Vec::new()), Output::default(), constraint)
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
                    let (can_expr, elem_out, constraint) = canonicalize_expr(
                        rigids,
                        env,
                        var_store,
                        scope,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(list_elem_constraint);
                    constraints.push(constraint);

                    references = references.union(elem_out.references);

                    can_elems.push(can_expr);
                }

                constraints.push(Eq(constrain::list_type(list_type), expected, region));

                let mut output = Output::default();

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                (List(list_var, can_elems), output, And(constraints))
            }
        }
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
            let (fn_expr, mut output, fn_con) = canonicalize_expr(
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
                let (arg_expr, arg_out, arg_con) = canonicalize_expr(
                    rigids,
                    env,
                    var_store,
                    scope,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

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
                    return (fn_expr, output, True);
                }
                not_var => {
                    // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                    Call(Box::new(not_var), args, *application_style)
                }
            };

            for arg_out in outputs {
                output.references = output.references.union(arg_out.references);
            }

            let expected_fn_type = ForReason(
                fn_reason,
                Function(arg_types, Box::new(ret_type.clone())),
                region,
            );

            let constraint = exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(fn_type, expected_fn_type, fn_region),
                    And(arg_cons),
                    Eq(ret_type, expected, region),
                ]),
            );

            (expr, output, constraint)
        }
        ast::Expr::Var(module_parts, name) => {
            let symbol = if module_parts.is_empty() {
                scope.symbol(name)
            } else {
                Symbol::from_parts(module_parts, name)
            };

            let ident = Ident::new(module_parts, name);

            canonicalize_lookup(env, scope, ident, symbol, region, expected, var_store)
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
        ast::Expr::Defs(loc_defs, loc_ret) => {
            can_defs_with_return(
                rigids,
                env,
                var_store,
                // The body expression gets a new scope for canonicalization,
                // so clone it.
                scope.clone(),
                loc_defs,
                expected,
                Info::with_capacity(loc_defs.len()),
                Info::with_capacity(loc_defs.len()),
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

            let mut state = PatternState {
                headers: SendMap::default(),
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
            let (loc_body_expr, mut output, ret_constraint) = canonicalize_expr(
                rigids,
                env,
                var_store,
                &mut scope,
                loc_body_expr.region,
                &loc_body_expr.value,
                body_type,
            );

            let defs_constraint = And(state.constraints);

            let constraint = exists(
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
                constraint,
            )
        }

        ast::Expr::Case(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = var_store.fresh();
            let cond_type = Variable(cond_var);
            let (can_cond, mut output, expr_con) = canonicalize_expr(
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

            // Incorporate all three expressions into a combined Output value.
            let expr = Case(cond_var, Box::new(can_cond), can_branches);

            // TODO check for exhaustiveness. If this `case` is non-exaustive, then:
            //
            // 1. Record a Problem.
            // 2. Add an extra _ branch at the end which throws a runtime error.

            (expr, output, And(constraints))
        }
        ast::Expr::Access(record_expr, field) => {
            let ext_var = var_store.fresh();
            let field_var = var_store.fresh();
            let ext_type = Type::Variable(ext_var);
            let field_type = Type::Variable(field_var);

            let mut rec_field_types = SendMap::default();

            rec_field_types.insert(Lowercase::from(*field), field_type.clone());

            let record_type = Type::Record(rec_field_types, Box::new(ext_type));
            let record_expected = Expected::NoExpectation(record_type);

            let (loc_expr, output, mut constraint) = canonicalize_expr(
                &ImMap::default(),
                env,
                var_store,
                scope,
                region,
                record_expr,
                record_expected,
            );

            constraint = exists(
                vec![field_var, ext_var],
                And(vec![constraint, Eq(field_type, expected, region)]),
            );

            (loc_expr.value, output, constraint)
        }
        ast::Expr::AccessorFunction(_)
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
            let (answer, output, constraint) =
                canonicalize_expr(rigids, env, var_store, scope, region, sub_expr, expected);

            (answer.value, output, constraint)
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

            let (constraint, answer) =
                int_expr_from_result(var_store, result, env, expected, region);

            (answer, Output::default(), constraint)
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
        constraint,
    )
}

#[inline(always)]
fn canonicalize_lookup(
    env: &mut Env,
    scope: &Scope,
    ident: Ident,
    symbol: Symbol,
    region: Region,
    expected: Expected<Type>,
    var_store: &VarStore,
) -> (Expr, Output, Constraint) {
    use self::Expr::*;

    let mut output = Output::default();
    let (constraint, can_expr) = match resolve_ident(&env, &scope, ident, &mut output.references) {
        Ok(sub_symbol) => (
            Lookup(symbol, expected, region),
            Var(var_store.fresh(), sub_symbol),
        ),
        Err(ident) => {
            let loc_ident = Located {
                region,
                value: ident,
            };

            env.problem(Problem::UnrecognizedConstant(loc_ident.clone()));

            (True, RuntimeError(UnrecognizedConstant(loc_ident)))
        }
    };

    (can_expr, output, constraint)
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
    let defined_idents: Vector<(Ident, (Symbol, Region))> =
        idents_from_patterns(std::iter::once(loc_pattern), &scope);

    scope.idents = union_pairs(scope.idents, defined_idents.iter());

    let (can_expr, branch_output, ret_constraint) = canonicalize_expr(
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
        headers: SendMap::default(),
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
        ret_constraint,
    }));

    (
        loc_can_pattern,
        can_expr,
        constraint,
        branch_output.references,
    )
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

fn canonicalize_field<'a>(
    rigids: &Rigids,
    env: &mut Env,
    var_store: &VarStore,
    scope: &mut Scope,
    field: &'a ast::AssignedField<'a, ast::Expr<'a>>,
    region: Region,
) -> (Lowercase, Located<Expr>, Output, Variable, Type, Constraint) {
    use crate::parse::ast::AssignedField::*;

    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        LabeledValue(label, _, loc_expr) => {
            let field_var = var_store.fresh();
            let field_type = Variable(field_var);
            let field_expected = NoExpectation(field_type.clone());
            let (loc_can_expr, output, constraint) = canonicalize_expr(
                rigids,
                env,
                var_store,
                scope,
                loc_expr.region,
                &loc_expr.value,
                field_expected,
            );

            (
                Lowercase::from(label.value),
                loc_can_expr,
                output,
                field_var,
                field_type,
                constraint,
            )
        }

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(_) => {
            panic!("Somehow a LabelOnly record field was not desugared!");
        }

        SpaceBefore(sub_field, _) | SpaceAfter(sub_field, _) => {
            canonicalize_field(rigids, env, var_store, scope, sub_field, region)
        }

        Malformed(_string) => {
            panic!("TODO canonicalize malformed record field");
        }
    }
}

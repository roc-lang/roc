use crate::can::expr::Expr::{self};
use crate::can::pattern::Pattern;
use crate::can::procedure::{Procedure, References};
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
// use crate::constrain::{self, exists};
use crate::ident::Ident;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::AnnotationSource::TypedCaseBranch;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self};
use crate::types::LetConstraint;
use crate::types::PExpected::{self};
use crate::types::PReason::{self};
use crate::types::Reason;
use crate::types::Type::{self, *};
use crate::uniqueness::constrain::exists;
use std::fmt::Debug;

pub use crate::can::expr::Expr::*;

mod constrain;

pub struct Env {
    pub bound_names: ImMap<Symbol, Variable>,
    pub procedures: ImMap<Symbol, Procedure>,
}

#[allow(clippy::too_many_arguments)]
pub fn canonicalize_declaration(
    var_store: &VarStore,
    region: Region,
    loc_expr: Located<Expr>,
    _declared_idents: &ImMap<Ident, (Symbol, Region)>,
    expected: Expected<Type>,
) -> Output {
    let rigids = ImMap::default();

    canonicalize_expr(&rigids, var_store, region, &loc_expr.value, expected)
}

pub struct PatternState {
    pub headers: ImMap<Symbol, Located<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
}

fn canonicalize_pattern(
    state: &mut PatternState,
    pattern: &Located<Pattern>,
    expected: PExpected<Type>,
) {
    use crate::can::pattern::Pattern::*;
    use crate::types::PatternCategory;

    match &pattern.value {
        Identifier(_, symbol) => {
            state.headers.insert(
                symbol.clone(),
                Located {
                    region: pattern.region,
                    value: expected.clone().get_type(),
                },
            );
        }

        IntLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Int,
                Type::int(),
                expected,
            ));
        }
        FloatLiteral(_) => {
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Float,
                Type::float(),
                expected,
            ));
        }

        ExactString(_) => {
            state.constraints.push(Constraint::Pattern(
                pattern.region,
                PatternCategory::Str,
                Type::string(),
                expected,
            ));
        }

        Tag(_, _) | AppliedTag(_, _, _) | EmptyRecordLiteral(_) => {
            panic!("TODO add_constraints for {:?}", pattern);
        }

        Underscore(_) | Shadowed(_) | UnsupportedPattern(_) => {
            // no constraints
        }
    }
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

/// Whenever we encounter a user-defined type variable (a "rigid" var for short),
/// for example `a` in the annotation `identity : a -> a`, we add it to this
/// map so that expressions within that annotation can share these vars.
type Rigids = ImMap<Box<str>, Type>;

pub fn canonicalize_expr(
    rigids: &Rigids,
    var_store: &VarStore,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> Output {
    pub use crate::can::expr::Expr::*;

    match expr {
        Int(_) => {
            let constraint = constrain::int_literal(var_store, expected, region);
            (Output::new(constraint))
        }
        Float(_) => {
            let constraint = constrain::float_literal(var_store, expected, region);
            (Output::new(constraint))
        }
        BlockStr(_) | Str(_) => {
            let inferred = constrain::lift(var_store, constrain::str_type());
            let constraint = Eq(inferred, expected, region);
            (Output::new(constraint))
        }
        EmptyRecord => {
            let inferred = constrain::lift(var_store, EmptyRec);
            let constraint = Eq(inferred, expected, region);
            (Output::new(constraint))
        }
        Record(_, _) => panic!("TODO implement records"),
        List(_variable, loc_elems) => {
            if loc_elems.is_empty() {
                let list_var = var_store.fresh();
                let inferred = constrain::lift(var_store, constrain::empty_list_type(list_var));
                let constraint = Eq(inferred, expected, region);
                (Output::new(constraint))
            } else {
                // constrain `expected ~ List a` and that all elements `~ a`.
                let list_var = var_store.fresh(); // `v` in the type (List v)
                let list_type = Type::Variable(list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));
                let mut references = References::new();

                for loc_elem in loc_elems.iter() {
                    let elem_var = var_store.fresh();
                    let elem_type = Variable(elem_var);
                    let elem_expected = Expected::NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_type.clone(),
                        Expected::ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let elem_out = canonicalize_expr(
                        rigids,
                        var_store,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(list_elem_constraint);
                    constraints.push(elem_out.constraint);

                    references = references.union(elem_out.references);
                }
                let inferred = constrain::lift(var_store, constrain::list_type(list_type));
                constraints.push(Eq(inferred, expected, region));

                let mut output = Output::new(And(constraints));

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                output
            }
        }
        Var(_variable, symbol) => {
            // assume the variable is Shared
            let val_var = var_store.fresh();
            let uniq_var = var_store.fresh();

            let val_type = Variable(val_var);
            let uniq_type = Variable(uniq_var);
            let attr_type = constrain::attr_type(uniq_type.clone(), val_type);
            // constraint expected ~ the type of this symbol in the environment
            // Output::new(Lookup(symbol.clone(), expected, region))
            Output::new(And(vec![
                Lookup(symbol.clone(), expected.clone(), region),
                Eq(attr_type, expected, region),
                Eq(
                    uniq_type,
                    Expected::NoExpectation(constrain::shared_type()),
                    region,
                ),
            ]))
        }
        /*
        FunctionPointer(_variable, symbol) => match env.bound_names.get(symbol) {
            // constraint expected ~ the type of this symbol in the environment
            None => panic!("FunctionPointer: no variable for {:?}", symbol),
            Some(var) => Output::new(Eq(Variable(*var), expected, Region::zero())),
        },
        */
        Closure(_symbol, _recursion, args, body) => {
            // first, generate constraints for the arguments
            let mut arg_types = Vec::new();
            let mut arg_vars = Vec::new();

            let mut state = PatternState {
                headers: ImMap::default(),
                vars: Vec::with_capacity(1),
                constraints: Vec::with_capacity(1),
            };

            for pattern in args {
                let arg_var = var_store.fresh();
                let arg_typ = Variable(arg_var);
                canonicalize_pattern(
                    &mut state,
                    &pattern,
                    PExpected::NoExpectation(arg_typ.clone()),
                );
                arg_types.push(arg_typ);
                arg_vars.push(arg_var);
            }

            let ret_var = var_store.fresh();
            let ret_type = Variable(ret_var);

            state.vars.push(ret_var);

            let fn_typ = constrain::lift(
                var_store,
                Type::Function(arg_types, Box::new(ret_type.clone())),
            );

            let mut output = canonicalize_expr(
                rigids,
                var_store,
                region,
                &body.value,
                Expected::NoExpectation(ret_type.clone()),
            );

            let defs_constraint = And(state.constraints);
            let ret_constraint = output.constraint;

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

            output
        }

        Call(fn_expr, loc_args, _) => {
            let fn_var = var_store.fresh();
            let fn_type = Variable(fn_var);
            let ret_var = var_store.fresh();
            let ret_type = Variable(ret_var);
            let fn_expected = Expected::NoExpectation(fn_type.clone());
            let fn_region = Region::zero();

            let mut vars = Vec::with_capacity(2 + loc_args.len());

            // Canonicalize the function expression and its arguments
            let fn_con =
                canonicalize_expr(rigids, var_store, fn_region, &fn_expr, fn_expected).constraint;

            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall {
                arity: loc_args.len() as u8,
            };

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, loc_arg) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_var = var_store.fresh();
                let arg_type = Variable(arg_var);

                let reason = Reason::AnonymousFnArg {
                    arg_index: index as u8,
                };

                let expected_arg = Expected::ForReason(reason, arg_type.clone(), region);
                let arg_con = canonicalize_expr(
                    rigids,
                    var_store,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                )
                .constraint;

                vars.push(arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = Expected::ForReason(
                fn_reason,
                constrain::lift(var_store, Function(arg_types, Box::new(ret_type.clone()))),
                region,
            );

            Output::new(exists(
                vars,
                And(vec![
                    fn_con,
                    Eq(fn_type, expected_fn_type, fn_region),
                    And(arg_cons),
                    Eq(ret_type, expected, region),
                ]),
            ))
        }

        Defs(_, defs, loc_ret) => {
            // The body expression gets a new scope for canonicalization,
            // so clone it.
            Output::new(can_defs(rigids, var_store, defs, expected, loc_ret))
        }
        // Case( Variable, Box<Located<Expr>>, Vec<(Located<Pattern>, Located<Expr>)>,
        Case(_variable, loc_cond, branches) => {
            let cond_var = var_store.fresh();
            let cond_type = Variable(cond_var);
            let mut output = canonicalize_expr(
                rigids,
                var_store,
                region,
                &loc_cond.value,
                Expected::NoExpectation(cond_type.clone()),
            );

            let mut constraints = Vec::with_capacity(branches.len() + 1);
            let expr_con = output.constraint.clone();

            match expected {
                Expected::FromAnnotation(name, arity, _, typ) => {
                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = canonicalize_case_branch(
                            var_store,
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::CaseMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::FromAnnotation(
                                name.clone(),
                                arity,
                                TypedCaseBranch(index),
                                typ.clone(),
                            ),
                            &mut output,
                        );

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

                    for (index, (loc_pattern, loc_expr)) in branches.iter().enumerate() {
                        let branch_con = canonicalize_case_branch(
                            var_store,
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::CaseMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::ForReason(
                                Reason::CaseBranch { index },
                                branch_type.clone(),
                                region,
                            ),
                            &mut output,
                        );

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

            output.constraint = And(constraints);

            output
        }
        _ => panic!("{:?}", expr),
    }
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn canonicalize_case_branch(
    var_store: &VarStore,
    rigids: &Rigids,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
    _output: &mut Output,
) -> (Constraint) {
    let branch_output =
        canonicalize_expr(rigids, var_store, region, &loc_expr.value, expr_expected);

    let mut state = PatternState {
        headers: ImMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    // mutates the state, so return value is not used
    canonicalize_pattern(&mut state, &loc_pattern, pattern_expected);

    Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: state.vars,
        def_types: state.headers,
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint: branch_output.constraint,
    }))
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

fn add_pattern_to_lookup_types(
    loc_pattern: Located<Pattern>,
    lookup_types: &mut ImMap<Symbol, Located<Type>>,
    expr_type: Type,
) {
    let region = loc_pattern.region;

    match loc_pattern.value {
        Pattern::Identifier(_, symbol) => {
            let loc_type = Located {
                region,
                value: expr_type,
            };

            lookup_types.insert(symbol, loc_type);
        }
        _ => panic!("TODO constrain patterns other than Identifier"),
    }
}

fn can_defs(
    rigids: &Rigids,

    var_store: &VarStore,

    defs: &[(Located<Pattern>, Located<Expr>)],
    expected: Expected<Type>,
    body: &Located<Expr>,
) -> Constraint {
    let rigid_info = Info::with_capacity(defs.len());
    let mut flex_info = Info::with_capacity(defs.len());

    for (pattern, expr) in defs {
        let pattern_var = var_store.fresh();
        let pattern_type = Type::Variable(pattern_var);
        let pattern_expected = PExpected::NoExpectation(pattern_type);

        let mut state = PatternState {
            headers: ImMap::default(),
            vars: Vec::with_capacity(1),
            constraints: Vec::with_capacity(1),
        };

        canonicalize_pattern(&mut state, pattern, pattern_expected);

        flex_info.vars.push(pattern_var);

        let expr_var = var_store.fresh();
        let expr_type = Type::Variable(expr_var);
        let expr_constraint = canonicalize_expr(
            rigids,
            var_store,
            expr.region,
            &expr.value,
            Expected::NoExpectation(expr_type.clone()),
        )
        .constraint;

        add_pattern_to_lookup_types(
            // TODO can we we avoid this clone?
            pattern.clone(),
            &mut flex_info.def_types,
            expr_type.clone(),
        );

        flex_info.constraints.push(Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            def_types: state.headers,
            defs_constraint: And(state.constraints),
            ret_constraint: expr_constraint.clone(),
        })));
    }

    // The def as a whole is a tail call iff its return expression is a tail call.
    // Use its output as a starting point because its tail_call already has the right answer!
    let ret_con =
        canonicalize_expr(rigids, var_store, body.region, &body.value, expected).constraint;

    Let(Box::new(LetConstraint {
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
    }))
}

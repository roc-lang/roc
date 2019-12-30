use crate::can::def::Def;
use crate::can::expr::Expr;
use crate::can::expr::Output;
use crate::can::pattern::Pattern;
use crate::can::procedure::{Procedure, References};
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
// use crate::constrain::{self, exists};
use crate::can::ident::Lowercase;
use crate::can::pattern;
use crate::ident::Ident;
use crate::region::{Located, Region};
use crate::subs::{VarStore, Variable};
use crate::types::AnnotationSource::TypedWhenBranch;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self};
use crate::types::LetConstraint;
use crate::types::PExpected::{self};
use crate::types::PReason::{self};
use crate::types::Reason;
use crate::types::Type::{self, *};
use crate::uniqueness::constrain::exists;
use crate::uniqueness::sharing::VarUsage;

pub use crate::can::expr::Expr::*;

pub mod boolean_algebra;
mod constrain;
pub mod sharing;

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
) -> (Output, Constraint) {
    let rigids = ImMap::default();
    let mut var_usage = VarUsage::default();

    canonicalize_expr(
        &rigids,
        var_store,
        &mut var_usage,
        region,
        &loc_expr.value,
        expected,
    )
}

pub struct PatternState {
    pub headers: SendMap<Symbol, Located<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
}

fn canonicalize_pattern(
    var_store: &VarStore,
    state: &mut PatternState,
    pattern: &Located<Pattern>,
    expected: PExpected<Type>,
) {
    use crate::can::pattern::Pattern::*;
    use crate::types::PatternCategory;

    match &pattern.value {
        Identifier(symbol) => {
            state.headers.insert(
                symbol.clone(),
                Located {
                    region: pattern.region,
                    value: expected.get_type(),
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

        RecordDestructure(patterns) => {
            let ext_var = var_store.fresh();
            let ext_type = Type::Variable(ext_var);

            let mut field_types: SendMap<Lowercase, Type> = SendMap::default();
            for (pattern, maybe_guard) in patterns {
                let pat_var = var_store.fresh();
                let pat_type = Type::Variable(pat_var);
                let pattern_expected = PExpected::NoExpectation(pat_type.clone());

                if let Some(loc_guard) = maybe_guard {
                    canonicalize_pattern(var_store, state, pattern, pattern_expected.clone());
                    canonicalize_pattern(var_store, state, loc_guard, pattern_expected);
                } else {
                    canonicalize_pattern(var_store, state, pattern, pattern_expected);
                }

                let name = if let Identifier(n) = &pattern.value {
                    let a: Box<str> = n.clone().into();
                    let b: Lowercase = a.into();
                    b
                } else {
                    unreachable!("the lhs must be an identifier at this point");
                };

                state.vars.push(pat_var);
                field_types.insert(name, pat_type);
            }

            let record_type =
                constrain::lift(var_store, Type::Record(field_types, Box::new(ext_type)));
            let record_con = Constraint::Pattern(
                pattern.region,
                PatternCategory::Record,
                record_type,
                expected,
            );

            state.constraints.push(record_con);
        }

        Tag(_) | AppliedTag(_, _) => {
            panic!("TODO add_constraints for {:?}", pattern);
        }

        Underscore | Shadowed(_) | UnsupportedPattern(_) => {
            // no constraints
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
    var_usage: &mut VarUsage,
    region: Region,
    expr: &Expr,
    expected: Expected<Type>,
) -> (Output, Constraint) {
    pub use crate::can::expr::Expr::*;

    match expr {
        Int(_, _) => {
            let constraint = constrain::int_literal(var_store, expected, region);
            (Output::default(), constraint)
        }
        Float(_, _) => {
            let constraint = constrain::float_literal(var_store, expected, region);
            (Output::default(), constraint)
        }
        BlockStr(_) | Str(_) => {
            let inferred = constrain::lift(var_store, constrain::str_type());
            let constraint = Eq(inferred, expected, region);
            (Output::default(), constraint)
        }
        EmptyRecord => {
            let constraint = Eq(constrain::lift(var_store, EmptyRec), expected, region);

            (Output::default(), constraint)
        }
        Record(variable, fields) => {
            // NOTE: canonicalization guarantees at least one field
            // zero fields generates an EmptyRecord
            let mut field_types = SendMap::default();
            let mut field_vars = Vec::with_capacity(fields.len());

            // Constraints need capacity for each field + 1 for the record itself.
            let mut constraints = Vec::with_capacity(1 + fields.len());
            let mut output = Output::default();

            for (label, (_, loc_expr)) in fields.iter() {
                let field_var = var_store.fresh();
                let field_type = Variable(field_var);
                let field_expected = Expected::NoExpectation(field_type.clone());
                let (field_out, field_con) = canonicalize_expr(
                    rigids,
                    var_store,
                    var_usage,
                    loc_expr.region,
                    &loc_expr.value,
                    field_expected,
                );

                field_vars.push(field_var);
                field_types.insert(label.clone(), field_type);

                constraints.push(field_con);
                output.references = output.references.union(field_out.references);
            }

            let record_type = constrain::lift(
                var_store,
                Type::Record(
                    field_types,
                    // TODO can we avoid doing Box::new on every single one of these?
                    // For example, could we have a single lazy_static global Box they
                    // could all share?
                    Box::new(Type::EmptyRec),
                ),
            );
            let record_con = Eq(record_type, expected.clone(), region);
            let ext_con = Eq(Type::Variable(*variable), expected, region);

            constraints.push(record_con);
            constraints.push(ext_con);

            let constraint = exists(field_vars, And(constraints));

            (output, constraint)
        }
        Tag(name, arguments) => {
            panic!("TODO implement tag {:?} {:?}", name, arguments);
        }
        List(variable, loc_elems) => {
            if loc_elems.is_empty() {
                let list_var = *variable;
                let inferred = constrain::lift(var_store, constrain::empty_list_type(list_var));
                let constraint = Eq(inferred, expected, region);
                (Output::default(), constraint)
            } else {
                // constrain `expected ~ List a` and that all elements `~ a`.
                let list_var = *variable; // `v` in the type (List v)
                let list_type = Type::Variable(list_var);
                let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));
                let mut references = References::new();

                for (elem_var, loc_elem) in loc_elems.iter() {
                    let elem_type = Variable(*elem_var);
                    let elem_expected = Expected::NoExpectation(elem_type.clone());
                    let list_elem_constraint = Eq(
                        list_type.clone(),
                        Expected::ForReason(Reason::ElemInList, elem_type, region),
                        region,
                    );
                    let (elem_out, constraint) = canonicalize_expr(
                        rigids,
                        var_store,
                        var_usage,
                        loc_elem.region,
                        &loc_elem.value,
                        elem_expected,
                    );

                    constraints.push(list_elem_constraint);
                    constraints.push(constraint);

                    references = references.union(elem_out.references);
                }
                let inferred = constrain::lift(var_store, constrain::list_type(list_type));
                constraints.push(Eq(inferred, expected, region));

                let mut output = Output::default();

                output.references = references;

                // A list literal is never a tail call!
                output.tail_call = None;

                (output, And(constraints))
            }
        }
        Var(variable, symbol) => {
            var_usage.register(symbol);
            match var_usage.get_usage(symbol) {
                Some(sharing::ReferenceCount::Shared) => {
                    // the variable is used/consumed more than once, so it must be Shared
                    let val_var = *variable;
                    let uniq_var = var_store.fresh();

                    let val_type = Variable(val_var);
                    let uniq_type = Variable(uniq_var);
                    let attr_type = constrain::attr_type(uniq_type.clone(), val_type);

                    (
                        Output::default(),
                        And(vec![
                            Lookup(symbol.clone(), expected.clone(), region),
                            Eq(attr_type, expected, region),
                            Eq(
                                uniq_type,
                                Expected::NoExpectation(constrain::shared_type()),
                                region,
                            ),
                        ]),
                    )
                }
                Some(sharing::ReferenceCount::Unique) => {
                    // no additional constraints, keep uniqueness unbound
                    (
                        Output::default(),
                        Lookup(symbol.clone(), expected.clone(), region),
                    )
                }
                None => panic!("symbol not analyzed"),
            }
        }
        /*
        FunctionPointer(_variable, symbol) => match env.bound_names.get(symbol) {
            // constraint expected ~ the type of this symbol in the environment
            None => panic!("FunctionPointer: no variable for {:?}", symbol),
            Some(var) => Output::new(Eq(Variable(*var), expected, Region::zero())),
        },
        */
        FunctionPointer(_, _) => {
            panic!("TODO implement function pointer?");
        }
        Closure(_symbol, _recursion, args, boxed_body) => {
            let (ret_var, body) = &**boxed_body;

            // first, generate constraints for the arguments
            let mut arg_types = Vec::new();
            let mut arg_vars = Vec::new();

            let mut state = PatternState {
                headers: SendMap::default(),
                vars: Vec::with_capacity(1),
                constraints: Vec::with_capacity(1),
            };

            let mut vars = Vec::with_capacity(state.vars.capacity() + 1);
            let ret_type = Variable(*ret_var);

            vars.push(*ret_var);

            for (arg_var, pattern) in args {
                let arg_typ = Variable(*arg_var);
                canonicalize_pattern(
                    var_store,
                    &mut state,
                    &pattern,
                    PExpected::NoExpectation(arg_typ.clone()),
                );
                arg_types.push(arg_typ);
                arg_vars.push(arg_var);

                vars.push(*arg_var);
            }

            let fn_typ = constrain::lift(
                var_store,
                Type::Function(arg_types, Box::new(ret_type.clone())),
            );

            let (output, ret_constraint) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                region,
                &body.value,
                Expected::NoExpectation(ret_type),
            );

            // remove identifiers bound in the arguments from VarUsage
            for (_, pattern) in args {
                for identifier in pattern::symbols_from_pattern(&pattern.value) {
                    var_usage.unregister(&identifier);
                }
            }

            let defs_constraint = And(state.constraints);
            let constraint = exists(
                vars,
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

            (output, constraint)
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
            let (_, fn_con) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                fn_region,
                &fn_expr,
                fn_expected,
            );

            // TODO look up the name and use NamedFnArg if possible.
            let fn_reason = Reason::AnonymousFnCall {
                arity: loc_args.len() as u8,
            };

            let mut arg_types = Vec::with_capacity(loc_args.len());
            let mut arg_cons = Vec::with_capacity(loc_args.len());

            for (index, (arg_var, loc_arg)) in loc_args.iter().enumerate() {
                let region = loc_arg.region;
                let arg_type = Variable(*arg_var);

                let reason = Reason::AnonymousFnArg {
                    arg_index: index as u8,
                };

                let expected_arg = Expected::ForReason(reason, arg_type.clone(), region);
                let (_, arg_con) = canonicalize_expr(
                    rigids,
                    var_store,
                    var_usage,
                    loc_arg.region,
                    &loc_arg.value,
                    expected_arg,
                );

                vars.push(*arg_var);
                arg_types.push(arg_type);
                arg_cons.push(arg_con);
            }

            let expected_fn_type = Expected::ForReason(
                fn_reason,
                constrain::lift(var_store, Function(arg_types, Box::new(ret_type.clone()))),
                region,
            );

            (
                Output::default(),
                exists(
                    vars,
                    And(vec![
                        fn_con,
                        Eq(fn_type, expected_fn_type, fn_region),
                        And(arg_cons),
                        Eq(ret_type, expected, region),
                    ]),
                ),
            )
        }

        Defs(defs, loc_ret) => (
            Output::default(),
            can_defs(rigids, var_store, var_usage, defs, expected, loc_ret),
        ),
        When(variable, loc_cond, branches) => {
            let cond_var = *variable;
            let cond_type = Variable(cond_var);
            let (mut output, expr_con) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                region,
                &loc_cond.value,
                Expected::NoExpectation(cond_type.clone()),
            );

            let mut constraints = Vec::with_capacity(branches.len() + 1);

            let old_var_usage = var_usage.clone();

            match expected {
                Expected::FromAnnotation(name, arity, _, typ) => {
                    for (index, ((_patter_var, loc_pattern), (_expr_var, loc_expr))) in
                        branches.iter().enumerate()
                    {
                        let mut branch_var_usage = old_var_usage.clone();
                        let branch_con = canonicalize_when_branch(
                            var_store,
                            &mut branch_var_usage,
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::FromAnnotation(
                                name.clone(),
                                arity,
                                TypedWhenBranch(index),
                                typ.clone(),
                            ),
                            &mut output,
                        );

                        // required for a case like
                        //
                        // when b is
                        //      Foo x -> x + x
                        //      Bar x -> x
                        //
                        // In this case the `x` in the second branch is used uniquely
                        for symbol in pattern::symbols_from_pattern(&loc_pattern.value) {
                            branch_var_usage.unregister(&symbol);
                        }

                        var_usage.or(&branch_var_usage);

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

                    for (index, ((_pattern_var, loc_pattern), (_expr_var, loc_expr))) in
                        branches.iter().enumerate()
                    {
                        let mut branch_var_usage = old_var_usage.clone();
                        let branch_con = canonicalize_when_branch(
                            var_store,
                            &mut branch_var_usage,
                            rigids,
                            region,
                            loc_pattern,
                            loc_expr,
                            PExpected::ForReason(
                                PReason::WhenMatch { index },
                                cond_type.clone(),
                                region,
                            ),
                            Expected::ForReason(
                                Reason::WhenBranch { index },
                                branch_type.clone(),
                                region,
                            ),
                            &mut output,
                        );

                        // required for a case like
                        //
                        // case b when
                        //      Foo x -> x + x
                        //      Bar x -> x
                        //
                        // In this case the `x` in the second branch is used uniquely
                        for symbol in pattern::symbols_from_pattern(&loc_pattern.value) {
                            branch_var_usage.unregister(&symbol);
                        }

                        var_usage.or(&branch_var_usage);

                        branch_cons.push(branch_con);
                    }

                    constraints.push(exists(
                        vec![cond_var],
                        And(vec![
                            // Record the original conditional expression's constraint.
                            expr_con,
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

            (output, And(constraints))
        }

        Access {
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            let ext_type = Type::Variable(*ext_var);
            let field_type = Type::Variable(*field_var);

            let mut rec_field_types = SendMap::default();

            rec_field_types.insert(Lowercase::from(field.clone()), field_type.clone());

            let record_type =
                constrain::lift(var_store, Type::Record(rec_field_types, Box::new(ext_type)));
            let record_expected = Expected::NoExpectation(record_type);

            let (output, mut constraint) = canonicalize_expr(
                rigids,
                var_store,
                var_usage,
                loc_expr.region,
                &loc_expr.value,
                record_expected,
            );

            constraint = exists(
                vec![*field_var, *ext_var],
                And(vec![constraint, Eq(field_type, expected, region)]),
            );

            (output, constraint)
        }

        Accessor {
            field,
            field_var,
            ext_var,
        } => {
            let ext_type = Variable(*ext_var);
            let field_type = Variable(*field_var);

            let mut field_types = SendMap::default();
            let field_name = field.clone();
            field_types.insert(field_name, field_type.clone());
            let record_type =
                constrain::lift(var_store, Type::Record(field_types, Box::new(ext_type)));

            (
                Output::default(),
                exists(
                    vec![*field_var, *ext_var],
                    Eq(
                        Type::Function(vec![record_type], Box::new(field_type)),
                        expected,
                        region,
                    ),
                ),
            )
        }
        RuntimeError(_) => (Output::default(), True),
        // _ => panic!("{:?}", expr),
    }
}

// TODO trim down these arguments
#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn canonicalize_when_branch(
    var_store: &VarStore,
    var_usage: &mut VarUsage,
    rigids: &Rigids,
    region: Region,
    loc_pattern: &Located<Pattern>,
    loc_expr: &Located<Expr>,
    pattern_expected: PExpected<Type>,
    expr_expected: Expected<Type>,
    _output: &mut Output,
) -> Constraint {
    let (_, ret_constraint) = canonicalize_expr(
        rigids,
        var_store,
        var_usage,
        region,
        &loc_expr.value,
        expr_expected,
    );

    let mut state = PatternState {
        headers: SendMap::default(),
        vars: Vec::with_capacity(1),
        constraints: Vec::with_capacity(1),
    };

    // mutates the state, so return value is not used
    canonicalize_pattern(var_store, &mut state, &loc_pattern, pattern_expected);

    Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: state.vars,
        def_types: state.headers,
        defs_constraint: Constraint::And(state.constraints),
        ret_constraint,
    }))
}

struct Info {
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

fn add_pattern_to_lookup_types(
    loc_pattern: Located<Pattern>,
    lookup_types: &mut SendMap<Symbol, Located<Type>>,
    expr_type: Type,
) {
    let region = loc_pattern.region;

    match loc_pattern.value {
        Pattern::Identifier(symbol) => {
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
    var_usage: &mut VarUsage,
    defs: &[Def],
    expected: Expected<Type>,
    body: &Located<Expr>,
) -> Constraint {
    let rigid_info = Info::with_capacity(defs.len());
    let mut flex_info = Info::with_capacity(defs.len());
    let mut bound_symbols = Vec::with_capacity(defs.len());

    for def in defs {
        let pattern_var = var_store.fresh();
        let pattern_type = Type::Variable(pattern_var);
        let pattern_expected = PExpected::NoExpectation(pattern_type);

        let mut state = PatternState {
            headers: SendMap::default(),
            vars: Vec::with_capacity(1),
            constraints: Vec::with_capacity(1),
        };

        canonicalize_pattern(var_store, &mut state, &def.pattern, pattern_expected);

        flex_info.vars.push(pattern_var);

        let expr_var = var_store.fresh();
        let expr_type = Type::Variable(expr_var);
        let (_, expr_constraint) = canonicalize_expr(
            rigids,
            var_store,
            var_usage,
            def.expr.region,
            &def.expr.value,
            Expected::NoExpectation(expr_type.clone()),
        );

        add_pattern_to_lookup_types(
            // TODO can we we avoid this clone?
            def.pattern.clone(),
            &mut flex_info.def_types,
            expr_type.clone(),
        );

        bound_symbols.extend(pattern::symbols_from_pattern(&def.pattern.value));

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
    let (_, ret_con) = canonicalize_expr(
        rigids,
        var_store,
        var_usage,
        body.region,
        &body.value,
        expected,
    );

    // remove symbols bound in the let from var_usage
    for symbol in &bound_symbols {
        var_usage.unregister(symbol);
    }

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

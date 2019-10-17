use can::expr::Expr;
// use can::pattern::Pattern;
use can::procedure::Procedure;
// use can::symbol::Symbol;
use collections::ImMap;
// use operator::{ArgSide, Operator};
use region::{Located, Region};
use subs::{Subs, Variable};
use types::Constraint::{self, *};
use types::Expected::{self, *};
use types::Reason;
use types::Type::{self, *};

/// This lets us share bound type variables between nested annotations, e.g.
///
/// blah : Map k v -> Int
/// blah mapping =
///     nested : Map k v # <-- the same k and v from the top-level annotation
///     nested = mapping
///     42
///
/// In elm/compiler this is called RTV - the "Rigid Type Variables" dictionary.
type BoundTypeVars<'a> = ImMap<&'a str, Type>;

#[derive(Debug)]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Constraints(Vec::new())
    }

    pub fn add(&mut self, constraint: Constraint) {
        self.0.push(constraint)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Constraint> {
        self.0.iter()
    }

    pub fn into_iter(self) -> std::vec::IntoIter<Constraint> {
        self.0.into_iter()
    }
}

pub fn int_literal(
    subs: &mut Subs,
    constraints: &mut Constraints,
    expected: Expected<Type>,
    region: Region,
) {
    let typ = number_literal_type("Int", "Integer");
    let reason = Reason::IntLiteral;

    num_literal(subs, constraints, typ, reason, expected, region)
}

#[inline(always)]
pub fn float_literal(
    subs: &mut Subs,
    constraints: &mut Constraints,
    expected: Expected<Type>,
    region: Region,
) {
    let typ = number_literal_type("Float", "FloatingPoint");
    let reason = Reason::FloatLiteral;

    num_literal(subs, constraints, typ, reason, expected, region)
}

#[inline(always)]
fn num_literal(
    subs: &mut Subs,
    constraints: &mut Constraints,
    literal_type: Type,
    reason: Reason,
    expected: Expected<Type>,
    region: Region,
) {
    let num_var = subs.mk_flex_var();
    let num_type = Variable(num_var);
    let expected_literal = ForReason(reason, literal_type, region.clone());

    constraints.add(Eq(num_type.clone(), expected_literal, region.clone()));
    constraints.add(Eq(num_type, expected, region.clone()));
}

#[inline(always)]
fn number_literal_type(module_name: &str, type_name: &str) -> Type {
    builtin_type(
        "Num",
        "Num",
        vec![builtin_type(module_name, type_name, Vec::new())],
    )
}

#[inline(always)]
fn builtin_type(module_name: &str, type_name: &str, args: Vec<Type>) -> Type {
    Type::Apply {
        module_name: module_name.into(),
        name: type_name.into(),
        args,
    }
}

pub fn constrain<'a>(
    bound_vars: &'a BoundTypeVars<'a>,
    subs: &'a mut Subs,
    loc_expr: Located<Expr>,
    expected: Expected<Type>,
) -> Constraint {
    panic!("TODO inline constrain");
    // let region = loc_expr.region;

    // match loc_expr.value {
    //     List(elems) => list(elems, bound_vars, subs, expected, region),
    //     Var(sym) | FunctionPointer(sym) => Lookup(sym, expected, region),
    //     Assign(assignments, ret_expr) => {
    //         let ret_con = constrain(bound_vars, subs, *ret_expr, expected);

    //         if assignments.len() == 1 {
    //             // Don't bother allocating a Vec of them if there's only one!
    //             let (loc_pattern, loc_expr) = assignments.into_iter().next().unwrap();

    //             constrain_def(loc_pattern, loc_expr, bound_vars, subs, ret_con)
    //         } else {
    //             constrain_defs(assignments, bound_vars, subs, ret_con)
    //         }
    //     }
    //     Call(box_loc_fn_expr, args) => {
    //         constrain_call(bound_vars, subs, *box_loc_fn_expr, args, expected, region)
    //     }
    //     Expr::Operator(l_box_loc_expr, loc_op, r_box_loc_expr) => constrain_op(
    //         bound_vars,
    //         subs,
    //         *l_box_loc_expr,
    //         loc_op,
    //         *r_box_loc_expr,
    //         expected,
    //         region,
    //     ),
    //     _ => panic!("TODO constraints for {:?}", loc_expr.value),
    // }
}

// fn constrain_op(
// bound_vars: &BoundTypeVars,
// subs: &mut Subs,
// l_loc_expr: Located<Expr>,
// loc_op: Located<Operator>,
// r_loc_expr: Located<Expr>,
// expected: Expected<Type>,
// region: Region,
// ) -> Constraint {
// let op = loc_op.value;
// let op_types = Type::for_operator(op);
// // TODO use fn_var
// let _fn_var = subs.mk_flex_var();
// let ret_var = subs.mk_flex_var();
// let ret_type = Variable(ret_var);
// let ret_reason = Reason::OperatorRet(op);
// let expected_ret_type = ForReason(ret_reason, op_types.ret, region.clone());

// let (_l_var, l_con) = constrain_op_arg(
//     ArgSide::Left,
//     bound_vars,
//     subs,
//     op,
//     op_types.left,
//     l_loc_expr,
// );
// let (_r_var, r_con) = constrain_op_arg(
//     ArgSide::Right,
//     bound_vars,
//     subs,
//     op,
//     op_types.right,
//     r_loc_expr,
// );

// // TODO occurs check!
// // let vars = vec![fn_var, ret_var, l_var, r_var];
// // return $ exists (funcVar:resultVar:argVars) $ CAnd ...

// And(vec![
//     // the constraint from constrain on l_expr, expecting its hardcoded type
//     l_con,
//     // the constraint from constrain on r_expr, expecting its hardcoded type
//     r_con,
//     // The operator's args and return type should be its hardcoded types
//     Eq(ret_type.clone(), expected_ret_type, region.clone()),
//     // Finally, link the operator's return type to the given expected type
//     Eq(ret_type, expected, region),
// ])
// }

// #[inline(always)]
// fn constrain_op_arg(
// arg_side: ArgSide,
// bound_vars: &BoundTypeVars,
// subs: &mut Subs,
// op: Operator,
// typ: Type,
// loc_arg: Located<Expr>,
// ) -> (Variable, Constraint) {
// let region = loc_arg.region.clone();
// let arg_var = subs.mk_flex_var();
// let arg_type = Variable(arg_var);
// let reason = Reason::OperatorArg(op, arg_side);
// let expected_arg = ForReason(reason, typ, region.clone());
// let arg_con = And(vec![
//     // Recursively constrain the variable
//     constrain(bound_vars, subs, loc_arg, NoExpectation(arg_type.clone())),
//     // The variable should ultimately equal the hardcoded expected type
//     Eq(arg_type, expected_arg, region),
// ]);

// (arg_var, arg_con)
// }

// fn constrain_call(
// bound_vars: &BoundTypeVars,
// subs: &mut Subs,
// loc_expr: Located<Expr>,
// args: Vec<Located<Expr>>,
// expected: Expected<Type>,
// region: Region,
// ) -> Constraint {
// // The expression that evaluates to the function being called, e.g. `foo` in
// // (foo) bar baz
// let fn_var = subs.mk_flex_var();
// let fn_type = Variable(fn_var);
// let fn_region = loc_expr.region.clone();
// let fn_expected = NoExpectation(fn_type.clone());
// let fn_con = constrain(bound_vars, subs, loc_expr, fn_expected);
// let fn_reason =
//     // TODO look up the name and use NamedFnArg if possible.
//     Reason::AnonymousFnCall(args.len() as u8);

// // The function's return type
// let ret_var = subs.mk_flex_var();
// let ret_type = Variable(ret_var);

// // This will be used in the occurs check
// let mut vars = Vec::with_capacity(2 + args.len());

// vars.push(fn_var);
// vars.push(ret_var);

// let mut arg_types = Vec::with_capacity(args.len());
// let mut arg_cons = Vec::with_capacity(args.len());

// for (index, loc_arg) in args.into_iter().enumerate() {
//     let region = loc_arg.region.clone();
//     let arg_var = subs.mk_flex_var();
//     let arg_type = Variable(arg_var);
//     let reason =
//         // TODO look up the name and use NamedFnArg if possible.
//         Reason::AnonymousFnArg(index as u8);
//     let expected_arg = ForReason(reason, arg_type.clone(), region.clone());
//     let arg_con = constrain(bound_vars, subs, loc_arg, expected_arg);

//     vars.push(arg_var);
//     arg_types.push(arg_type);
//     arg_cons.push(arg_con);
// }

// // TODO occurs check!
// // return $ exists vars $ CAnd ...

// let expected_fn_type = ForReason(
//     fn_reason,
//     Function(arg_types, Box::new(ret_type.clone())),
//     region.clone(),
// );

// And(vec![
//     fn_con,
//     Eq(fn_type, expected_fn_type, fn_region),
//     And(arg_cons),
//     Eq(ret_type, expected, region),
// ])
// }

// pub fn constrain_defs(
// assignments: Vec<(Located<Pattern>, Located<Expr>)>,
// bound_vars: &BoundTypeVars,
// subs: &mut Subs,
// ret_con: Constraint,
// ) -> Constraint {
// let rigid_info = Info::with_capacity(assignments.len());
// let mut flex_info = Info::with_capacity(assignments.len());

// for (loc_pattern, loc_expr) in assignments {
//     let mut state = PatternState {
//         assignment_types: ImMap::default(),
//         vars: Vec::with_capacity(1),
//         reversed_constraints: Vec::with_capacity(1),
//     };
//     let pattern_var = subs.mk_flex_var();
//     let pattern_type = Type::Variable(pattern_var);

//     flex_info.vars.push(pattern_var);

//     state.add_pattern(loc_pattern.clone(), NoExpectation(pattern_type.clone()));
//     state.reversed_constraints.reverse();

//     let assignments_constraint = And(state.reversed_constraints);

//     // Set up types for the expr we're assigned to.
//     let expr_var = subs.mk_flex_var();
//     let expr_type = Type::Variable(expr_var);

//     // Any time there's a lookup on this symbol in the outer Let,
//     // it should result in this expression's type. After all, this
//     // is the type to which this symbol is assigned!
//     add_pattern_to_lookup_types(
//         loc_pattern,
//         &mut flex_info.assignment_types,
//         expr_type.clone(),
//     );

//     let expr_con = constrain(bound_vars, subs, loc_expr, NoExpectation(expr_type));
//     let def_con = Let(Box::new(LetConstraint {
//         rigid_vars: Vec::new(),
//         flex_vars: state.vars,
//         assignment_types: state.assignment_types,
//         assignments_constraint,
//         ret_constraint: expr_con,
//     }));

//     flex_info.constraints.push(def_con);
// }

// // Rigid constraint
// Let(Box::new(LetConstraint {
//     rigid_vars: rigid_info.vars,
//     flex_vars: Vec::new(),
//     assignment_types: rigid_info.assignment_types,
//     assignments_constraint:
//         // Flex constraint
//         Let(Box::new(LetConstraint {
//             rigid_vars: Vec::new(),
//             flex_vars: flex_info.vars,
//             assignment_types: flex_info.assignment_types.clone(),
//             assignments_constraint:
//                 // Final flex constraints
//                 Let(Box::new(LetConstraint {
//                     rigid_vars: Vec::new(),
//                     flex_vars: Vec::new(),
//                     assignment_types: flex_info.assignment_types,
//                     assignments_constraint: True,
//                     ret_constraint: And(flex_info.constraints)
//                 })),
//             ret_constraint: And(vec![And(rigid_info.constraints), ret_con])
//         })),
//     ret_constraint: True,
// }))
// }

// struct Info {
// pub vars: Vec<Variable>,
// pub constraints: Vec<Constraint>,
// pub assignment_types: ImMap<Symbol, Located<Type>>,
// }

// impl Info {
// pub fn with_capacity(capacity: usize) -> Self {
//     Info {
//         vars: Vec::with_capacity(capacity),
//         constraints: Vec::with_capacity(capacity),
//         assignment_types: ImMap::default(),
//     }
// }
// }

pub fn empty_list_type(var: Variable) -> Type {
    list_type(Type::Variable(var))
}

pub fn list_type(typ: Type) -> Type {
    builtin_type("List", "List", vec![typ])
}

pub fn str_type() -> Type {
    builtin_type("Str", "Str", Vec::new())
}

// pub fn constrain_def(
// loc_pattern: Located<Pattern>,
// loc_expr: Located<Expr>,
// bound_vars: &BoundTypeVars,
// subs: &mut Subs,
// ret_constraint: Constraint,
// ) -> Constraint {
// let mut state = PatternState {
//     assignment_types: ImMap::default(),
//     vars: Vec::with_capacity(1),
//     reversed_constraints: Vec::with_capacity(1),
// };
// let mut vars = Vec::with_capacity(state.vars.capacity());
// let pattern_var = subs.mk_flex_var();
// let pattern_type = Type::Variable(pattern_var);

// state.add_pattern(loc_pattern.clone(), NoExpectation(pattern_type.clone()));

// vars.push(pattern_var);

// // Set up types for the expr we're assigned to.
// let expr_var = subs.mk_flex_var();
// let expr_type = Type::Variable(expr_var);

// // These types are *only* for the current pattern. In contrast, the ones
// // in PatternState represent
// let mut lookup_types: ImMap<Symbol, Located<Type>> = ImMap::default();

// // Any time there's a lookup on this symbol in the outer Let,
// // it should result in this expression's type. After all, this
// // is the type to which this symbol is assigned!
// add_pattern_to_lookup_types(loc_pattern, &mut lookup_types, expr_type.clone());

// state.reversed_constraints.reverse();

// Let(Box::new(LetConstraint {
//     rigid_vars: Vec::new(),
//     flex_vars: vars,
//     assignments_constraint:
//         // This nested constraint represents the actually constrained expr.
//         Let(Box::new(LetConstraint {
//             rigid_vars: Vec::new(),
//             flex_vars: state.vars,
//             assignment_types: state.assignment_types,
//             assignments_constraint: And(state.reversed_constraints),
//             ret_constraint: constrain(bound_vars, subs, loc_expr, NoExpectation(expr_type))
//         })),
//     assignment_types: lookup_types,
//     ret_constraint,
// }))
// }

// fn add_pattern_to_lookup_types(
//     loc_pattern: Located<Pattern>,
//     lookup_types: &mut ImMap<Symbol, Located<Type>>,
//     expr_type: Type,
// ) {
//     let region = loc_pattern.region;

//     match loc_pattern.value {
//         Pattern::Identifier(symbol) => {
//             let loc_type = Located {
//                 region,
//                 value: expr_type,
//             };

//             lookup_types.insert(symbol, loc_type);
//         }
//         _ => panic!("TODO constrain patterns other than Identifier"),
//     }
// }

pub fn constrain_procedure<'a>(
    bound_vars: &'a BoundTypeVars<'a>,
    constraints: &'a mut Constraints,
    subs: &'a mut Subs,
    proc: Procedure,
    expected: Expected<Type>,
) -> Constraint {
    panic!("TODO inline constrain_procedure");
    // let mut state = PatternState {
    //     assignment_types: ImMap::default(),
    //     vars: Vec::with_capacity(proc.args.len()),
    //     reversed_constraints: Vec::with_capacity(1),
    // };

    // let args = constrain_args(proc.args.into_iter(), subs, &mut state);
    // let body_type = NoExpectation(args.ret_type);
    // let ret_constraint = constrain(bound_vars, subs, proc.body, body_type);

    // state.reversed_constraints.reverse();

    // let assignments_constraint = And(state.reversed_constraints);

    // // panic!("TODO occurs check");

    // And(vec![
    //     Let(Box::new(LetConstraint {
    //         rigid_vars: Vec::new(),
    //         flex_vars: state.vars,
    //         assignment_types: state.assignment_types,
    //         assignments_constraint,
    //         ret_constraint,
    //     })),
    //     Eq(args.typ, expected, proc.definition),
    // ])
}

// struct Args {
//     pub vars: Vec<Variable>,
//     pub typ: Type,
//     pub ret_type: Type,
// }

// fn constrain_args<I>(args: I, subs: &mut Subs, state: &mut PatternState) -> Args
// where
//     I: Iterator<Item = Located<Pattern>>,
// {
//     let (mut vars, arg_types) = patterns_to_variables(args.into_iter(), subs, state);

//     let ret_var = subs.mk_flex_var();
//     let ret_type = Type::Variable(ret_var);

//     vars.push(ret_var);

//     let typ = Type::Function(arg_types, Box::new(ret_type.clone()));

//     Args {
//         vars,
//         typ,
//         ret_type,
//     }
// }

// fn patterns_to_variables<I>(
//     patterns: I,
//     subs: &mut Subs,
//     state: &mut PatternState,
// ) -> (Vec<Variable>, Vec<Type>)
// where
//     I: Iterator<Item = Located<Pattern>>,
// {
//     let mut vars = Vec::with_capacity(state.vars.capacity());
//     let mut pattern_types = Vec::with_capacity(state.vars.capacity());

//     for loc_pattern in patterns {
//         let pattern_var = subs.mk_flex_var();
//         let pattern_type = Type::Variable(pattern_var);

//         state.add_pattern(loc_pattern, NoExpectation(pattern_type.clone()));

//         vars.push(pattern_var);
//         pattern_types.push(pattern_type);
//     }

//     (vars, pattern_types)
// }

// struct PatternState {
//     assignment_types: ImMap<Symbol, Located<Type>>,
//     vars: Vec<Variable>,
//     reversed_constraints: Vec<Constraint>,
// }

// impl PatternState {
//     pub fn add_pattern(&mut self, loc_pattern: Located<Pattern>, expected: Expected<Type>) {
//         use can::pattern::Pattern::*;

//         let region = loc_pattern.region;

//         match loc_pattern.value {
//             Identifier(symbol) => self.add_to_assignment_types(region, symbol, expected),
//             Underscore => (),
//             _ => panic!("TODO other patterns"),
//         }
//     }

//     fn add_to_assignment_types(
//         &mut self,
//         region: Region,
//         symbol: Symbol,
//         expected: Expected<Type>,
//     ) {
//         self.assignment_types.insert(
//             symbol,
//             Located {
//                 region,
//                 value: expected.get_type(),
//             },
//         );
//     }
// }

use canonicalize::{Pattern, Procedure, Symbol};
use canonicalize::Expr::{self, *};
use collections::ImMap;
use region::{Located, Region};
use subs::{Variable, Subs};
use types::{Expected, Expected::*, LetConstraint, Reason};
use types::Type::{self, *};
use types::Constraint::{self, *};

/// This lets us share bound type variables between nested annotations, e.g.
///
/// blah : Map k v -> Int
/// blah mapping =
///     nested : Map k v # <-- the same k and v from the top-level annotation
///     nested = mapping
///     42
///     
/// In elm/compiler this is called RTV - the "Rigid Type Variables" dictionary.
type BoundTypeVars = ImMap<String, Type>;

pub fn constrain(
    bound_vars: &BoundTypeVars,
    subs: &mut Subs,
    loc_expr: Located<Expr>,
    expected: Expected<Type>,
) -> Constraint {
    let region = loc_expr.region;

    match loc_expr.value {
        Int(_) => { Eq(num(subs.mk_flex_var()), expected, region) },
        Frac(_, _) => { fractional(subs, expected, region) },
        Approx(_) => { fractional(subs, expected, region) },
        Str(_) => { Eq(string(), expected, region) },
        EmptyStr => { Eq(string(), expected, region) },
        InterpolatedStr(_, _) => { Eq(string(), expected, region) },
        EmptyRecord => { Eq(EmptyRec, expected, region) },
        EmptyList => { Eq(empty_list(subs.mk_flex_var()), expected, region) },
        List(elems) => { list(elems, bound_vars, subs, expected, region) },
        Var(symbol) => Lookup(symbol, expected, region),
        Assign(assignments, ret_expr) => {
            if assignments.len() > 1 {
                panic!("TODO can't handle multiple assignments yet");
            } else {
                let ret_con = constrain(bound_vars, subs, *ret_expr, expected);
                let (loc_pattern, loc_expr) = assignments.first().unwrap_or_else(|| {
                    panic!("assignments.first() failed")
                });

                constrain_def(loc_pattern.clone(), loc_expr.clone(), bound_vars, subs, ret_con)
            }
        }
        _ => { panic!("TODO constraints for {:?}", loc_expr.value) }
    }
}

fn empty_list(var: Variable) -> Type {
    builtin_type("List", "List", vec![Type::Variable(var)])
}

fn string() -> Type {
    builtin_type("String", "String", Vec::new())
}

fn num(var: Variable) -> Type {
    builtin_type("Num", "Num", vec![Type::Variable(var)])
}

fn list(loc_elems: Vec<Located<Expr>>, bound_vars: &BoundTypeVars, subs: &mut Subs, expected: Expected<Type>, region: Region) -> Constraint {
    let list_var = subs.mk_flex_var(); // `v` in the type (List v)
    let list_type = Type::Variable(list_var);
    let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));

    for loc_elem in loc_elems {
        let elem_var = subs.mk_flex_var(); 
        let elem_type = Variable(elem_var);
        let elem_expected = NoExpectation(elem_type.clone());
        let elem_constraint = constrain(bound_vars, subs, loc_elem, elem_expected);
        let list_elem_constraint = 
            Eq(
                list_type.clone(), 
                ForReason(Reason::ElemInList, elem_type, region.clone()),
                region.clone()
            );

        constraints.push(elem_constraint);
        constraints.push(list_elem_constraint);
    }

    constraints.push(
        Eq(builtin_type("List", "List", vec![list_type]), expected, region)
    );

    And(constraints)
}

fn fractional(subs: &mut Subs, expected: Expected<Type>, region: Region) -> Constraint {
    // We'll make a Num var1 and a Fractional var2, 
    // and then add a constraint that var1 needs to equal Fractional var2
    let num_var = subs.mk_flex_var(); // Num var1
    let fractional_var = subs.mk_flex_var(); // Fractional var2
    let fractional_type = 
        Type::Apply(
            "Num".to_string(), 
            "Fractional".to_string(), 
            vec![Type::Variable(fractional_var)]
        );
    let num_var_type = Type::Variable(num_var);
    let num_type = 
        Type::Apply(
            "Num".to_string(), 
            "Num".to_string(), 
            vec![num_var_type.clone()]
        );
    let expected_fractional = 
        ForReason(Reason::FractionalLiteral, fractional_type, region.clone());

    And(vec![
        Eq(num_type, expected, region.clone()),
        Eq(num_var_type, expected_fractional, region),
    ])
}

fn builtin_type(module_name: &str, type_name: &str, args: Vec<Type>) -> Type {
    Type::Apply(module_name.to_string(), type_name.to_string(), args)
}

pub fn constrain_def(
    loc_pattern: Located<Pattern>,
    loc_expr: Located<Expr>,
    bound_vars: &BoundTypeVars,
    subs: &mut Subs,
    ret_constraint: Constraint
) -> Constraint {
    let mut state = PatternState {
        assignment_types: ImMap::default(),
        vars: Vec::with_capacity(1),
        reversed_constraints: Vec::with_capacity(1)
    };
    let args = constrain_args(std::iter::once(loc_pattern.clone()), subs, &mut state);
    let mut assignment_types: ImMap<Symbol, Located<Type>> = ImMap::default();

    match loc_pattern.value {
        Pattern::Identifier(symbol) => {
            let loc_type = Located {region: loc_pattern.region, value: args.typ};

            assignment_types.insert(symbol, loc_type);
        },
        _ => panic!("TODO constrain patterns other than Identifier")
    }

    state.reversed_constraints.reverse();

    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: args.vars,
        assignments_constraint:
            Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: state.vars,
                assignment_types: state.assignment_types,
                assignments_constraint: And(state.reversed_constraints),
                ret_constraint: constrain(bound_vars, subs, loc_expr, NoExpectation(args.ret_type))
            })),
        assignment_types,
        ret_constraint,
    }))
}

pub fn constrain_procedure(
    bound_vars: &BoundTypeVars,
    subs: &mut Subs,
    proc: Procedure,
    expected: Expected<Type>
) -> Constraint {
    let mut state = PatternState {
        assignment_types: ImMap::default(),
        vars: Vec::with_capacity(proc.args.len()),
        reversed_constraints: Vec::with_capacity(1)
    };

    let args = constrain_args(proc.args.into_iter(), subs, &mut state);
    let body_type = NoExpectation(args.ret_type);
    let ret_constraint = constrain(bound_vars, subs, proc.body, body_type);

    state.reversed_constraints.reverse();

    let assignments_constraint = And(state.reversed_constraints);

    // panic!("TODO occurs check");

    And(vec![
        Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            assignment_types: state.assignment_types,
            assignments_constraint,
            ret_constraint
        })),
        Eq(args.typ, expected, proc.definition)
    ])
}

struct Args {
    vars: Vec<Variable>, 
    typ: Type,
    ret_type: Type,
}

fn constrain_args<I>(
    args: I,
    subs: &mut Subs,
    state: &mut PatternState
) -> Args 
where I: Iterator<Item = Located<Pattern>>
{
    let (mut vars, arg_types) = 
        patterns_to_variables(args.into_iter(), subs, state);

    let ret_var = subs.mk_flex_var();
    let ret_type = Type::Variable(ret_var);

    vars.push(ret_var);

    let typ = Type::Function(arg_types, Box::new(ret_type.clone()));

    Args {vars, typ, ret_type}
}

fn patterns_to_variables<I>(
    patterns: I,
    subs: &mut Subs,
    state: &mut PatternState
) -> (Vec<Variable>, Vec<Type>) 
where I: Iterator<Item = Located<Pattern>>
{
    let mut vars = Vec::with_capacity(state.vars.capacity());
    let mut pattern_types = Vec::with_capacity(state.vars.capacity());

    for loc_pattern in patterns {
        let pattern_var = subs.mk_flex_var();
        let pattern_type = Type::Variable(pattern_var);

        state.add_pattern(loc_pattern, NoExpectation(pattern_type.clone()));

        vars.push(pattern_var);
        pattern_types.push(pattern_type);
    }

    (vars, pattern_types)
}


struct PatternState { 
    assignment_types: ImMap<Symbol, Located<Type>>,
    vars: Vec<Variable>,
    reversed_constraints: Vec<Constraint>
}

impl PatternState {
    pub fn add_pattern(&mut self, loc_pattern: Located<Pattern>, expected: Expected<Type>) {
        use canonicalize::Pattern::*;

        let region = loc_pattern.region;

        match loc_pattern.value {
            Identifier(symbol) => self.add_to_assignment_types(region, symbol, expected),
            Underscore => (),
            _ => panic!("TODO other patterns"),
        }
    }

    fn add_to_assignment_types(&mut self, region: Region, symbol: Symbol, expected: Expected<Type>) {
        self.assignment_types.insert(symbol, Located {region, value: expected.get_type()});
    }
}

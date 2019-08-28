use canonicalize::{Pattern, Procedure, Symbol};
use canonicalize::Expr::{self, *};
use collections::{ImMap, MutMap};
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
    bound_vars: BoundTypeVars,
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
        List(elems) => { list(elems, bound_vars.clone(), subs, expected, region) },
        _ => { panic!("TODO constraints") }
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

fn list(loc_elems: Vec<Located<Expr>>, bound_vars: BoundTypeVars, subs: &mut Subs, expected: Expected<Type>, region: Region) -> Constraint {
    let list_var = subs.mk_flex_var(); // `v` in the type (List v)
    let list_type = Type::Variable(list_var);
    let mut constraints = Vec::with_capacity(1 + (loc_elems.len() * 2));

    for loc_elem in loc_elems {
        let elem_var = subs.mk_flex_var(); 
        let elem_type = Variable(elem_var);
        let elem_expected = NoExpectation(elem_type.clone());
        let elem_constraint = constrain(bound_vars.clone(), subs, loc_elem, elem_expected);
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
    bound_vars: BoundTypeVars,
    subs: &mut Subs,
    body_constraint: Constraint
) -> Constraint {
    let mut state = PatternState {
        headers: MutMap::default(),
        vars: Vec::with_capacity(1),
        reversed_constraints: Vec::with_capacity(1)
    };
    let args = constrain_args(std::iter::once(loc_pattern), subs, &mut state);

    state.reversed_constraints.reverse();

    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: args.vars,
        header_constraint:
            Let(Box::new(LetConstraint {
                rigid_vars: Vec::new(),
                flex_vars: state.vars,
                header: state.headers,
                header_constraint: And(state.reversed_constraints),
                body_constraint: constrain(bound_vars, subs, loc_expr, NoExpectation(args.ret_type))
            })),
        body_constraint,
        header: panic!("TODO Map.singleton name (A.At region tipe)"),
    }))
}

pub fn constrain_procedure(
    bound_vars: BoundTypeVars,
    subs: &mut Subs,
    region: Region,
    proc: Procedure,
    expected: Expected<Type>
) -> Constraint {
    let mut state = PatternState {
        headers: MutMap::default(),
        vars: Vec::with_capacity(proc.args.len()),
        reversed_constraints: Vec::with_capacity(1)
    };
    let args = constrain_args(proc.args.into_iter(), subs, &mut state);
    let body_type = NoExpectation(args.ret_type);
    let body_constraint = constrain(bound_vars, subs, proc.body, body_type);

    state.reversed_constraints.reverse();

    let header_constraint = And(state.reversed_constraints);

    // panic!("TODO occurs check");

    And(vec![
        Let(Box::new(LetConstraint {
            rigid_vars: Vec::new(),
            flex_vars: state.vars,
            header: state.headers,
            header_constraint,
            body_constraint
        })),
        Eq(args.typ, expected, region)
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
    let mut vars = Vec::with_capacity(state.vars.capacity());
    let mut arg_types = Vec::with_capacity(state.vars.capacity());

    for loc_pattern in args {
        let arg_var = subs.mk_flex_var();
        let arg_type = Type::Variable(arg_var);

        add_pattern(loc_pattern, NoExpectation(arg_type.clone()), state);

        vars.push(arg_var);
        arg_types.push(arg_type);
    }

    let ret_var = subs.mk_flex_var();
    let ret_type = Type::Variable(ret_var);

    vars.push(ret_var);

    let typ = Type::Function(arg_types, Box::new(ret_type.clone()));

    Args {vars, typ, ret_type}
}

struct PatternState { 
    headers: MutMap<Symbol, Located<Type>>,
    vars: Vec<Variable>,
    reversed_constraints: Vec<Constraint>
}

fn add_to_headers(region: Region, symbol: Symbol, expected: Expected<Type>, state: &mut PatternState) {
    state.headers.insert(symbol, Located {region, value: expected.unwrap()});
}

fn add_pattern(loc_pattern: Located<Pattern>, expected: Expected<Type>, state: &mut PatternState) {
    use canonicalize::Pattern::*;

    let region = loc_pattern.region;

    match loc_pattern.value {
        Identifier(symbol) => add_to_headers(region, symbol, expected, state),
        Underscore => (),
        _ => panic!("TODO other patterns"),
    }
}


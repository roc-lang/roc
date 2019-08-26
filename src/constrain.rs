use canonicalize::{Pattern, Procedure, Symbol};
use canonicalize::Expr::{self, *};
use collections::{ImMap, MutMap};
use region::{Located, Region};
use subs::{Variable, Subs};
use types::{Expected, Expected::*, Builtin, LetConstraint};
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
    let builtin = |b| -> Constraint { Eq(region, Builtin(b), expected) };

    match loc_expr.value {
        Int(_) => { builtin(Builtin::Int) },
        Frac(_, _) => { builtin(Builtin::Frac) },
        Approx(_) => { builtin(Builtin::Approx) },
        Str(_) => { builtin(Builtin::Str) },
        EmptyStr => { builtin(Builtin::Str) },
        EmptyRecord => { builtin(Builtin::EmptyRecord) },
        InterpolatedStr(_, _) => { builtin(Builtin::Str) },
        _ => { panic!("TODO constraints") }
    }
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
        Eq(region, args.typ, expected)
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


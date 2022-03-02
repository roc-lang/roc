use crate::solve;
use roc_can::constraint::Constraint;
use roc_can::constraint_soa::{Constraint as ConstraintSoa, Constraints};
use roc_collections::all::MutMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_types::solved_types::{Solved, SolvedType};
use roc_types::subs::{Subs, VarStore, Variable};
use roc_types::types::Alias;

#[derive(Debug)]
pub struct SolvedModule {
    pub solved_types: MutMap<Symbol, SolvedType>,
    pub aliases: MutMap<Symbol, Alias>,
    pub exposed_symbols: Vec<Symbol>,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub problems: Vec<solve::TypeError>,
}

pub fn run_solve(
    rigid_variables: MutMap<Variable, Lowercase>,
    constraint: Constraint,
    var_store: VarStore,
) -> (Solved<Subs>, solve::Env, Vec<solve::TypeError>) {
    let env = solve::Env::default();

    let mut subs = Subs::new_from_varstore(var_store);

    for (var, name) in rigid_variables {
        subs.rigid_var(var, name);
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let (solved_subs, solved_env) = solve::run(&env, &mut problems, subs, &constraint);

    (solved_subs, solved_env, problems)
}

pub fn run_solve_soa(
    constraints: &Constraints,
    constraint: ConstraintSoa,
    rigid_variables: MutMap<Variable, Lowercase>,
    var_store: VarStore,
) -> (Solved<Subs>, solve::Env, Vec<solve::TypeError>) {
    let env = solve::Env::default();

    let mut subs = Subs::new_from_varstore(var_store);

    for (var, name) in rigid_variables {
        subs.rigid_var(var, name);
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let (solved_subs, solved_env) =
        solve::run_soa(constraints, &env, &mut problems, subs, &constraint);

    (solved_subs, solved_env, problems)
}

pub fn make_solved_types(
    solved_subs: &Solved<Subs>,
    exposed_vars_by_symbol: &[(Symbol, Variable)],
) -> MutMap<Symbol, SolvedType> {
    let mut solved_types = MutMap::default();

    // exposed_vars_by_symbol contains the Variables for all the Symbols
    // this module exposes. We want to convert those into flat SolvedType
    // annotations which are decoupled from our Subs, because that's how
    // other modules will generate constraints for imported values
    // within the context of their own Subs.
    for (symbol, var) in exposed_vars_by_symbol.iter() {
        let solved_type = SolvedType::new(solved_subs, *var);

        solved_types.insert(*symbol, solved_type);
    }

    solved_types
}

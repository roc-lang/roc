use crate::solve;
use roc_can::constraint::{Constraint as ConstraintSoa, Constraints};
use roc_collections::all::MutMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_types::solved_types::{Solved, SolvedType};
use roc_types::subs::{StorageSubs, Subs, Variable};
use roc_types::types::Alias;

#[derive(Debug)]
pub struct SolvedModule {
    pub aliases: MutMap<Symbol, Alias>,
    pub exposed_symbols: Vec<Symbol>,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub problems: Vec<solve::TypeError>,
    pub stored_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub storage_subs: StorageSubs,
}

pub fn run_solve(
    constraints: &Constraints,
    constraint: ConstraintSoa,
    rigid_variables: MutMap<Variable, Lowercase>,
    mut subs: Subs,
) -> (Solved<Subs>, solve::Env, Vec<solve::TypeError>) {
    let env = solve::Env::default();

    for (var, name) in rigid_variables {
        subs.rigid_var(var, name);
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let (solved_subs, solved_env) = solve::run(constraints, &env, &mut problems, subs, &constraint);

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

pub fn exposed_types_storage_subs(
    solved_subs: &mut Solved<Subs>,
    exposed_vars_by_symbol: &[(Symbol, Variable)],
) -> (StorageSubs, Vec<(Symbol, Variable)>) {
    let subs = solved_subs.inner_mut();
    let mut storage_subs = StorageSubs::new(Subs::new());
    let mut stored_vars_by_symbol = Vec::with_capacity(exposed_vars_by_symbol.len());

    for (symbol, var) in exposed_vars_by_symbol.iter() {
        let new_var = storage_subs.import_variable_from(subs, *var).variable;
        stored_vars_by_symbol.push((*symbol, new_var));
    }

    (storage_subs, stored_vars_by_symbol)
}

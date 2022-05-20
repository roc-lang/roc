use crate::solve::{self, Aliases};
use roc_can::abilities::{AbilitiesStore, SolvedSpecializations};
use roc_can::constraint::{Constraint as ConstraintSoa, Constraints};
use roc_can::module::RigidVariables;
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_types::solved_types::Solved;
use roc_types::subs::{StorageSubs, Subs, Variable};
use roc_types::types::Alias;

#[derive(Debug)]
pub struct SolvedModule {
    pub problems: Vec<solve::TypeError>,

    /// all aliases and their definitions. this has to include non-exposed aliases
    /// because exposed aliases can depend on non-exposed ones)
    pub aliases: MutMap<Symbol, (bool, Alias)>,

    /// Used when the goal phase is TypeChecking, and
    /// to create the types for HostExposed. This
    /// has some overlap with the StorageSubs fields,
    /// so maybe we can get rid of this at some point
    ///
    /// Contains both variables of symbols that are explicitly exposed by the header,
    /// and the variables of any solved ability specializations we have.
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,

    /// Used when importing this module into another module
    pub stored_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub storage_subs: StorageSubs,
    pub solved_specializations: SolvedSpecializations,
}

pub fn run_solve(
    constraints: &Constraints,
    constraint: ConstraintSoa,
    rigid_variables: RigidVariables,
    mut subs: Subs,
    mut aliases: Aliases,
    mut abilities_store: AbilitiesStore,
) -> (
    Solved<Subs>,
    solve::Env,
    Vec<solve::TypeError>,
    AbilitiesStore,
) {
    for (var, name) in rigid_variables.named {
        subs.rigid_var(var, name);
    }

    for (var, (name, ability)) in rigid_variables.able {
        subs.rigid_able_var(var, name, ability);
    }

    for var in rigid_variables.wildcards {
        subs.rigid_var(var, "*".into());
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let (solved_subs, solved_env) = solve::run(
        constraints,
        &mut problems,
        subs,
        &mut aliases,
        &constraint,
        &mut abilities_store,
    );

    (solved_subs, solved_env, problems, abilities_store)
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

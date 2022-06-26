use crate::solve::{self, Aliases};
use roc_can::abilities::{AbilitiesStore, ResolvedSpecializations};
use roc_can::constraint::{Constraint as ConstraintSoa, Constraints};
use roc_can::expr::PendingDerives;
use roc_can::module::RigidVariables;
use roc_collections::all::MutMap;
use roc_collections::VecMap;
use roc_derive_key::GlobalDerivedSymbols;
use roc_module::symbol::Symbol;
use roc_types::solved_types::Solved;
use roc_types::subs::{ExposedTypesStorageSubs, StorageSubs, Subs, Variable};
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
    pub solved_specializations: ResolvedSpecializations,
    pub exposed_types: ExposedTypesStorageSubs,
}

#[allow(clippy::too_many_arguments)] // TODO: put params in a context/env var
pub fn run_solve(
    constraints: &Constraints,
    constraint: ConstraintSoa,
    rigid_variables: RigidVariables,
    mut subs: Subs,
    mut aliases: Aliases,
    mut abilities_store: AbilitiesStore,
    pending_derives: PendingDerives,
    derived_symbols: GlobalDerivedSymbols,
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
        pending_derives,
        &mut abilities_store,
        derived_symbols,
    );

    (solved_subs, solved_env, problems, abilities_store)
}

/// Copies exposed types and all ability specializations, which may be implicitly exposed.
pub fn exposed_types_storage_subs(
    solved_subs: &mut Solved<Subs>,
    exposed_vars_by_symbol: &[(Symbol, Variable)],
    solved_specializations: &ResolvedSpecializations,
) -> ExposedTypesStorageSubs {
    let subs = solved_subs.inner_mut();
    let mut storage_subs = StorageSubs::new(Subs::new());
    let mut stored_vars_by_symbol = VecMap::with_capacity(exposed_vars_by_symbol.len());
    let mut stored_specialization_lambda_set_vars =
        VecMap::with_capacity(solved_specializations.len());

    for (symbol, var) in exposed_vars_by_symbol.iter() {
        let new_var = storage_subs.import_variable_from(subs, *var).variable;
        stored_vars_by_symbol.insert(*symbol, new_var);
    }

    for (_, member_specialization) in solved_specializations.iter() {
        for (_, &specialization_lset_var) in member_specialization.specialization_lambda_sets.iter()
        {
            let new_var = storage_subs
                .import_variable_from(subs, specialization_lset_var)
                .variable;
            stored_specialization_lambda_set_vars.insert(specialization_lset_var, new_var);
        }
    }

    ExposedTypesStorageSubs {
        storage_subs,
        stored_vars_by_symbol,
        stored_specialization_lambda_set_vars,
    }
}

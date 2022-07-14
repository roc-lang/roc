use crate::solve::{self, Aliases};
use roc_can::abilities::{AbilitiesStore, ResolvedSpecializations};
use roc_can::constraint::{Constraint as ConstraintSoa, Constraints};
use roc_can::expr::PendingDerives;
use roc_can::module::{ExposedByModule, RigidVariables};
use roc_collections::all::MutMap;
use roc_collections::VecMap;
use roc_derive::SharedDerivedModule;
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
use roc_types::subs::{Content, ExposedTypesStorageSubs, FlatType, StorageSubs, Subs, Variable};
use roc_types::types::Alias;

/// A marker that a given Subs has been solved.
/// The only way to obtain a Solved<Subs> is by running the solver on it.
#[derive(Clone, Debug)]
pub struct Solved<T>(pub T);

impl<T> Solved<T> {
    pub fn inner(&self) -> &'_ T {
        &self.0
    }

    pub fn inner_mut(&mut self) -> &'_ mut T {
        &mut self.0
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

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
    home: ModuleId,
    constraints: &Constraints,
    constraint: ConstraintSoa,
    rigid_variables: RigidVariables,
    mut subs: Subs,
    mut aliases: Aliases,
    mut abilities_store: AbilitiesStore,
    pending_derives: PendingDerives,
    exposed_by_module: &ExposedByModule,
    derived_module: SharedDerivedModule,
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
        home,
        constraints,
        &mut problems,
        subs,
        &mut aliases,
        &constraint,
        pending_derives,
        &mut abilities_store,
        exposed_by_module,
        derived_module,
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
            let specialization_lset_ambient_function_var = subs
                .get_lambda_set(specialization_lset_var)
                .ambient_function;

            // Import the ambient function of this specialization lambda set; that will import the
            // lambda set as well. The ambient function is needed for the lambda set compaction
            // algorithm.
            let imported_lset_ambient_function_var = storage_subs
                .import_variable_from(subs, specialization_lset_ambient_function_var)
                .variable;

            let imported_lset_var = match storage_subs
                .as_inner()
                .get_content_without_compacting(imported_lset_ambient_function_var)
            {
                Content::Structure(FlatType::Func(_, lambda_set_var, _)) => *lambda_set_var,
                content => internal_error!(
                    "ambient lambda set function import is not a function, found: {:?}",
                    roc_types::subs::SubsFmtContent(content, storage_subs.as_inner())
                ),
            };
            stored_specialization_lambda_set_vars
                .insert(specialization_lset_var, imported_lset_var);
        }
    }

    ExposedTypesStorageSubs {
        storage_subs,
        stored_vars_by_symbol,
        stored_specialization_lambda_set_vars,
    }
}

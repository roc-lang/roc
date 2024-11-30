use crate::solve::RunSolveOutput;
use crate::FunctionKind;
use crate::{aliases::Aliases, solve};
use roc_can::abilities::{AbilitiesStore, ResolvedImpl};
use roc_can::constraint::{Constraint, Constraints};
use roc_can::expr::PendingDerives;
use roc_can::module::{ExposedByModule, ModuleParams, ResolvedImplementations, RigidVariables};
use roc_collections::all::MutMap;
use roc_collections::{VecMap, VecSet};
use roc_derive::SharedDerivedModule;
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
use roc_solve_problem::TypeError;
use roc_types::subs::{Content, ExposedTypesStorageSubs, FlatType, StorageSubs, Subs, Variable};
use roc_types::types::{Alias, MemberImpl, Types};

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
    pub problems: Vec<TypeError>,

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
    pub solved_implementations: ResolvedImplementations,
    pub exposed_types: ExposedTypesStorageSubs,
}

pub struct SolveConfig<'a> {
    /// The module we are solving.
    pub home: ModuleId,
    pub constraints: &'a Constraints,
    pub root_constraint: Constraint,
    /// All types introduced in the module. Canonicalized, but not necessarily yet associated with
    /// a variable substitution.
    pub types: Types,
    /// How functions should be kinded.
    pub function_kind: FunctionKind,
    /// Table of types introduced in this module that claim to derive an ability implementation.
    /// Due for checking and instantiation after the solver runs over the module.
    pub pending_derives: PendingDerives,
    /// Types exposed by other modules.
    /// Available for late instantiation of imports, lambda sets, or ability types.
    pub exposed_by_module: &'a ExposedByModule,
    /// The unique `#Derived` module, used to generate and retrieve derived ability
    /// implementations.
    /// Needed during solving to resolve lambda sets from derived implementations that escape into
    /// the user module.
    pub derived_module: SharedDerivedModule,
    /// Symbols that are exposed to the host which might need special treatment.
    pub host_exposed_symbols: Option<&'a VecSet<Symbol>>,

    #[cfg(debug_assertions)]
    /// The checkmate collector for this module.
    pub checkmate: Option<roc_checkmate::Collector>,

    /// Module params
    pub module_params: Option<ModuleParams>,
    pub module_params_vars: VecMap<ModuleId, Variable>,
}

pub struct SolveOutput {
    pub subs: Solved<Subs>,
    pub scope: solve::Scope,
    pub errors: Vec<TypeError>,
    pub resolved_abilities_store: AbilitiesStore,

    #[cfg(debug_assertions)]
    pub checkmate: Option<roc_checkmate::Collector>,
}

pub fn run_solve(
    config: SolveConfig<'_>,
    rigid_variables: RigidVariables,
    mut subs: Subs,
    mut aliases: Aliases,
    mut abilities_store: AbilitiesStore,
) -> SolveOutput {
    for (var, name) in rigid_variables.named {
        subs.rigid_var(var, name);
    }

    for (var, (name, abilities)) in rigid_variables.able {
        subs.rigid_able_var(var, name, abilities);
    }

    for var in rigid_variables.wildcards {
        subs.rigid_var(var, "*".into());
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let RunSolveOutput {
        solved,
        scope,
        #[cfg(debug_assertions)]
        checkmate,
    } = solve::run(
        config,
        &mut problems,
        subs,
        &mut aliases,
        &mut abilities_store,
    );

    SolveOutput {
        subs: solved,
        scope,
        errors: problems,
        resolved_abilities_store: abilities_store,
        #[cfg(debug_assertions)]
        checkmate,
    }
}

/// Copies exposed types and all ability specializations, which may be implicitly exposed.
pub fn exposed_types_storage_subs(
    home: ModuleId,
    solved_subs: &mut Solved<Subs>,
    exposed_vars_by_symbol: &[(Symbol, Variable)],
    params_var: Option<Variable>,
    solved_implementations: &ResolvedImplementations,
    abilities_store: &AbilitiesStore,
) -> ExposedTypesStorageSubs {
    let subs = solved_subs.inner_mut();
    let mut storage_subs = StorageSubs::new(Subs::new());
    let mut stored_vars_by_symbol = VecMap::with_capacity(exposed_vars_by_symbol.len());

    for (symbol, var) in exposed_vars_by_symbol.iter() {
        let new_var = storage_subs.import_variable_from(subs, *var).variable;
        stored_vars_by_symbol.insert(*symbol, new_var);
    }

    let stored_params_var =
        params_var.map(|params_var| storage_subs.import_variable_from(subs, params_var).variable);

    let mut stored_specialization_lambda_set_vars =
        VecMap::with_capacity(solved_implementations.len());

    for (_, member_impl) in solved_implementations.iter() {
        match member_impl {
            ResolvedImpl::Impl(member_specialization) => {
                // Export all the lambda sets and their ambient functions.
                for (_, &lset_var) in member_specialization.specialization_lambda_sets.iter() {
                    let specialization_lset_ambient_function_var =
                        subs.get_lambda_set(lset_var).ambient_function;

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
                        Content::Structure(FlatType::Func(_, lambda_set_var, _, _)) => {
                            *lambda_set_var
                        }
                        content => internal_error!(
                            "ambient lambda set function import is not a function, found: {:?}",
                            roc_types::subs::SubsFmtContent(content, storage_subs.as_inner())
                        ),
                    };
                    stored_specialization_lambda_set_vars.insert(lset_var, imported_lset_var);
                }
            }
            ResolvedImpl::Error => {
                // nothing to do
            }
        }
    }

    // Store the regioned lambda sets of the ability members defined in this module.
    let stored_ability_member_vars = abilities_store
        .root_ability_members()
        .iter()
        .filter_map(|(member, data)| {
            if member.module_id() == home {
                let var = data.signature_var();
                let imported_var = storage_subs.import_variable_from(subs, var).variable;
                Some((var, imported_var))
            } else {
                None
            }
        })
        .collect();

    ExposedTypesStorageSubs {
        storage_subs,
        stored_vars_by_symbol,
        stored_specialization_lambda_set_vars,
        stored_ability_member_vars,
        stored_params_var,
    }
}

/// Extracts the ability member implementations owned by a solved module.
pub fn extract_module_owned_implementations(
    module_id: ModuleId,
    abilities_store: &AbilitiesStore,
) -> ResolvedImplementations {
    abilities_store
        .iter_declared_implementations()
        .filter_map(|(impl_key, member_impl)| {
            // This module solved this specialization if either the member or the type comes from the
            // module.
            if impl_key.ability_member.module_id() != module_id
                && impl_key.opaque.module_id() != module_id
            {
                return None;
            }

            let resolved_impl = match member_impl {
                MemberImpl::Impl(impl_symbol) => {
                    let specialization = abilities_store.specialization_info(*impl_symbol).expect(
                        "declared implementations should be resolved conclusively after solving",
                    );
                    ResolvedImpl::Impl(specialization.clone())
                }
                MemberImpl::Error => ResolvedImpl::Error,
            };

            Some((impl_key, resolved_impl))
        })
        .collect()
}

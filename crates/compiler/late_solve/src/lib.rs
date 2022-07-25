//! Crate roc_late_solve exposes type unification and solving primitives from the perspective of
//! the compiler backend.

use std::sync::{Arc, RwLock};

use bumpalo::Bump;
use roc_can::abilities::AbilitiesStore;
use roc_can::module::ExposedByModule;
use roc_collections::MutMap;
use roc_derive::SharedDerivedModule;
use roc_error_macros::internal_error;
use roc_module::symbol::ModuleId;
use roc_solve::solve::{compact_lambda_sets_of_vars, Phase, Pools};
use roc_types::subs::{get_member_lambda_sets_at_region, Content, FlatType, LambdaSet};
use roc_types::subs::{ExposedTypesStorageSubs, Subs, Variable};
use roc_unify::unify::{unify as unify_unify, Env, Mode, Unified};

pub use roc_solve::ability::resolve_ability_specialization;
pub use roc_solve::ability::Resolved;
pub use roc_types::subs::instantiate_rigids;

#[derive(Debug)]
pub struct UnificationFailed;

/// A global view of all abilities across all modules in a program under compilation.
/// [WorldAbilities::insert] adds a solved abilities store for a module to the global map.
/// Use [WorldAbilities::clone_ref] to get a thread-safe, reference-counted copy of the global map.
/// Note that this is indeed a map shared amongst everything during a compilation.
#[derive(Default, Debug)]
pub struct WorldAbilities {
    world: Arc<RwLock<MutMap<ModuleId, (AbilitiesStore, ExposedTypesStorageSubs)>>>,
}

impl WorldAbilities {
    pub fn insert(
        &mut self,
        module: ModuleId,
        store: AbilitiesStore,
        exposed_types: ExposedTypesStorageSubs,
    ) {
        let old_store = self
            .world
            .write()
            .unwrap()
            .insert(module, (store, exposed_types));

        debug_assert!(old_store.is_none(), "{:?} abilities not new", module);
    }

    #[inline(always)]
    pub fn with_module_exposed_type<T>(
        &mut self,
        module: ModuleId,
        mut f: impl FnMut(&mut ExposedTypesStorageSubs) -> T,
    ) -> T {
        let mut world = self.world.write().unwrap();
        let (_, exposed_types) = world.get_mut(&module).expect("module not in the world");

        f(exposed_types)
    }

    #[inline(always)]
    pub fn with_module_abilities_store<T, F>(&self, module: ModuleId, mut f: F) -> T
    where
        F: FnMut(&AbilitiesStore) -> T,
    {
        let world = self.world.read().unwrap();
        let (module_store, _module_types) = world
            .get(&module)
            .unwrap_or_else(|| internal_error!("Module {:?} not found in world abilities", module));
        f(module_store)
    }

    pub fn clone_ref(&self) -> Self {
        Self {
            world: Arc::clone(&self.world),
        }
    }
}

pub enum AbilitiesView<'a> {
    World(&'a WorldAbilities),
    Module(&'a AbilitiesStore),
}

impl AbilitiesView<'_> {
    #[inline(always)]
    pub fn with_module_abilities_store<T, F>(&self, module: ModuleId, mut f: F) -> T
    where
        F: FnMut(&AbilitiesStore) -> T,
    {
        match self {
            AbilitiesView::World(wa) => wa.with_module_abilities_store(module, f),
            AbilitiesView::Module(store) => f(store),
        }
    }
}

pub struct LatePhase<'a> {
    home: ModuleId,
    abilities: &'a AbilitiesView<'a>,
}

impl Phase for LatePhase<'_> {
    const IS_LATE: bool = true;

    #[inline(always)]
    fn with_module_abilities_store<T, F>(&self, module: ModuleId, f: F) -> T
    where
        F: FnMut(&AbilitiesStore) -> T,
    {
        self.abilities.with_module_abilities_store(module, f)
    }

    #[inline(always)]
    fn copy_lambda_set_ambient_function_to_home_subs(
        &self,
        external_lambda_set_var: Variable,
        external_module_id: ModuleId,
        target_subs: &mut Subs,
    ) -> Variable {
        match (external_module_id == self.home, self.abilities) {
            (true, _) | (false, AbilitiesView::Module(_)) => {
                // The lambda set (and hence its ambient function) should be available in the
                // current subs.
                let LambdaSet {
                    ambient_function, ..
                } = target_subs.get_lambda_set(external_lambda_set_var);
                ambient_function
            }
            (false, AbilitiesView::World(wa)) => {
                let mut world = wa.world.write().unwrap();
                let (_module_store, module_types) = world.get_mut(&external_module_id).unwrap();

                let storage_lambda_set_var = *module_types
                    .stored_specialization_lambda_set_vars
                    .get(&external_lambda_set_var)
                    .unwrap();
                let LambdaSet {
                    ambient_function, ..
                } = module_types
                    .storage_subs
                    .as_inner()
                    .get_lambda_set(storage_lambda_set_var);

                let copied = module_types
                    .storage_subs
                    // TODO: I think this is always okay, but revisit later when we're in a more
                    // stable position to see if we can get rid of the bookkeeping done as a result
                    // of this.
                    .export_variable_to_directly_to_use_site(target_subs, ambient_function);
                let our_ambient_function_var = copied.variable;

                debug_assert!(matches!(
                    target_subs.get_content_without_compacting(our_ambient_function_var),
                    Content::Structure(FlatType::Func(..))
                ));

                our_ambient_function_var
            }
        }
    }

    #[inline(always)]
    fn get_and_copy_ability_member_ambient_function(
        &self,
        ability_member: roc_module::symbol::Symbol,
        region: u8,
        target_subs: &mut Subs,
    ) -> Variable {
        match self.abilities {
            AbilitiesView::Module(abilities_store) => {
                // No option other than that the var must be in our module store.
                // Even if the specialization lambda set comes from another module,
                // we should have taken care to import it before starting solving in this module.
                let member_def = abilities_store
                    .member_def(ability_member)
                    .unwrap_or_else(|| {
                        internal_error!(
                            "{:?} is not resolved, or not an ability member!",
                            ability_member
                        )
                    });
                let member_var = member_def.signature_var();

                let region_lset = get_member_lambda_sets_at_region(target_subs, member_var, region);

                let LambdaSet {
                    ambient_function, ..
                } = target_subs.get_lambda_set(region_lset);

                ambient_function
            }
            AbilitiesView::World(wa) => {
                let member_home = ability_member.module_id();
                let mut world = wa.world.write().unwrap();
                let (module_store, module_types) = world.get_mut(&member_home).unwrap();

                let member_def = module_store.member_def(ability_member).unwrap_or_else(|| {
                    internal_error!(
                        "{:?} is not resolved, or not an ability member!",
                        ability_member
                    )
                });
                let member_var = member_def.signature_var();
                let storage_member_var = module_types
                    .stored_ability_member_vars
                    .get(&member_var)
                    .unwrap();

                let storage_lambda_set_var = get_member_lambda_sets_at_region(
                    module_types.storage_subs.as_inner(),
                    *storage_member_var,
                    region,
                );

                let LambdaSet {
                    ambient_function, ..
                } = module_types
                    .storage_subs
                    .as_inner()
                    .get_lambda_set(storage_lambda_set_var);

                let copied = module_types
                    .storage_subs
                    // TODO: I think this is always okay, but revisit later when we're in a more
                    // stable position to see if we can get rid of the bookkeeping done as a result
                    // of this.
                    .export_variable_to_directly_to_use_site(target_subs, ambient_function);

                let our_ambient_function_var = copied.variable;
                instantiate_rigids(target_subs, our_ambient_function_var);

                debug_assert!(matches!(
                    target_subs.get_content_without_compacting(our_ambient_function_var),
                    Content::Structure(FlatType::Func(..))
                ));

                our_ambient_function_var
            }
        }
    }
}

/// Unifies two variables and performs lambda set compaction.
/// Ranks and other ability demands are disregarded.
#[allow(clippy::too_many_arguments)]
pub fn unify(
    home: ModuleId,
    arena: &Bump,
    subs: &mut Subs,
    abilities: &AbilitiesView,
    derived_module: &SharedDerivedModule,
    exposed_by_module: &ExposedByModule,
    left: Variable,
    right: Variable,
) -> Result<(), UnificationFailed> {
    debug_assert_ne!(
        home,
        ModuleId::DERIVED_SYNTH,
        "derived module can only unify its subs in its own context!"
    );
    let unified = unify_unify(&mut Env::new(subs), left, right, Mode::EQ);

    match unified {
        Unified::Success {
            vars: _,
            must_implement_ability: _,
            lambda_sets_to_specialize,
            extra_metadata: _,
        } => {
            let mut pools = Pools::default();

            let late_phase = LatePhase { home, abilities };

            let must_implement_constraints = compact_lambda_sets_of_vars(
                subs,
                derived_module,
                arena,
                &mut pools,
                lambda_sets_to_specialize,
                &late_phase,
                exposed_by_module,
            );
            // At this point we can't do anything with must-implement constraints, since we're no
            // longer solving. We must assume that they were totally caught during solving.
            // After we land https://github.com/rtfeldman/roc/issues/3207 this concern should totally
            // go away.
            let _ = must_implement_constraints;
            // Pools are only used to keep track of variable ranks for generalization purposes.
            // Since we break generalization during monomorphization, `pools` is irrelevant
            // here. We only need it for `compact_lambda_sets_of_vars`, which is also used in a
            // solving context where pools are relevant.

            Ok(())
        }
        Unified::Failure(..) | Unified::BadType(..) => Err(UnificationFailed),
    }
}

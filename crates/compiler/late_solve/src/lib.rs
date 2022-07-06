//! Crate roc_late_solve exposes type unification and solving primitives from the perspective of
//! the compiler backend.

use std::sync::{Arc, RwLock};

use bumpalo::Bump;
use roc_can::abilities::AbilitiesStore;
use roc_collections::MutMap;
use roc_derive_key::GlobalDerivedSymbols;
use roc_module::symbol::ModuleId;
use roc_solve::solve::{compact_lambda_sets_of_vars, Phase, Pools};
use roc_types::subs::{Content, FlatType, LambdaSet};
use roc_types::subs::{ExposedTypesStorageSubs, Subs, Variable};
use roc_unify::unify::{unify as unify_unify, Mode, Unified};

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

    pub fn clone_ref(&self) -> Self {
        Self {
            world: Arc::clone(&self.world),
        }
    }
}

pub enum AbilitiesView<'a> {
    World(WorldAbilities),
    Module(&'a AbilitiesStore),
}

impl AbilitiesView<'_> {
    #[inline(always)]
    pub fn with_module_abilities_store<T, F>(&self, module: ModuleId, mut f: F) -> T
    where
        F: FnMut(&AbilitiesStore) -> T,
    {
        match self {
            AbilitiesView::World(wa) => {
                let world = wa.world.read().unwrap();
                let (module_store, _module_types) = world.get(&module).unwrap();
                f(module_store)
            }
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
}

/// Unifies two variables and performs lambda set compaction.
/// Ranks and other ability demands are disregarded.
pub fn unify(
    home: ModuleId,
    arena: &Bump,
    subs: &mut Subs,
    abilities: &AbilitiesView,
    derived_symbols: &GlobalDerivedSymbols,
    left: Variable,
    right: Variable,
) -> Result<(), UnificationFailed> {
    let unified = unify_unify(subs, left, right, Mode::EQ);
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
                arena,
                &mut pools,
                lambda_sets_to_specialize,
                &late_phase,
                derived_symbols,
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

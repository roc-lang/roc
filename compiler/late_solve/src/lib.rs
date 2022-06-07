//! Crate roc_late_solve exposes type unification and solving primitives from the perspective of
//! the compiler backend.

use bumpalo::Bump;
use roc_can::abilities::AbilitiesStore;
use roc_solve::solve::{compact_lambda_sets_of_vars, Pools};
use roc_types::subs::{Subs, Variable};
use roc_unify::unify::{unify as unify_unify, Mode, Unified};

/// Unifies two variables and performs lambda set compaction.
/// Ranks and other ability demands are disregarded.
pub fn unify(
    arena: &Bump,
    subs: &mut Subs,
    abilities_store: &AbilitiesStore,
    left: Variable,
    right: Variable,
) -> Result<(), ()> {
    let unified = unify_unify(subs, left, right, Mode::EQ);
    match unified {
        Unified::Success {
            vars: _,
            must_implement_ability: _,
            lambda_sets_to_specialize,
        } => {
            let mut pools = Pools::default();
            compact_lambda_sets_of_vars(
                subs,
                arena,
                &mut pools,
                abilities_store,
                lambda_sets_to_specialize,
            );
            // Pools are only used to keep track of variable ranks for generalization purposes.
            // Since we break generalization during monomorphization, `pools` is irrelevant
            // here. We only need it for `compact_lambda_sets_of_vars`, which is also used in a
            // solving context where pools are relevant.

            Ok(())
        }
        Unified::Failure(..) | Unified::BadType(..) => Err(()),
    }
}

pub use roc_solve::solve::instantiate_rigids;

pub use roc_solve::ability::resolve_ability_specialization;
pub use roc_solve::ability::Resolved;

use roc_can::abilities::AbilitiesStore;
use roc_region::all::{Loc, Region};
use roc_types::subs::Subs;
use roc_types::subs::Variable;
use roc_types::types::{Category, PatternCategory};
use roc_unify::unify::MustImplementAbility;

use crate::solve::{IncompleteAbilityImplementation, TypeError};

#[derive(Debug, Clone)]
pub enum AbilityImplError {
    /// Promote this to an error that the type does not fully implement an ability
    IncompleteAbility,
    /// Promote this error to a `TypeError::BadExpr` from elsewhere
    BadExpr(Region, Category, Variable),
    /// Promote this error to a `TypeError::BadPattern` from elsewhere
    BadPattern(Region, PatternCategory, Variable),
}

#[derive(Default)]
pub struct DeferredMustImplementAbility(Vec<(Vec<MustImplementAbility>, AbilityImplError)>);

impl DeferredMustImplementAbility {
    pub fn add(&mut self, must_implement: Vec<MustImplementAbility>, on_error: AbilityImplError) {
        self.0.push((must_implement, on_error));
    }

    pub fn check(self, subs: &mut Subs, abilities_store: &AbilitiesStore) -> Vec<TypeError> {
        // Two passes here. First up let's build up records of what types fully implement
        // abilities, and what specializations are available/missing for the ones that don't.
        // Use a vec since these lists should usually be pretty small.
        let mut good = vec![];
        let mut bad = vec![];

        macro_rules! is_good {
            ($e:expr) => {
                good.contains($e)
            };
        }
        macro_rules! get_bad {
            ($e:expr) => {
                bad.iter()
                    .find(|(cand, _)| $e == *cand)
                    .map(|(_, incomplete)| incomplete)
            };
        }

        for (mias, _) in self.0.iter() {
            for &mia @ MustImplementAbility { typ, ability } in mias {
                if is_good!(&mia) || get_bad!(mia).is_some() {
                    continue;
                }

                let members_of_ability = abilities_store.members_of_ability(ability).unwrap();
                let mut specialized_members = Vec::with_capacity(members_of_ability.len());
                let mut missing_members = Vec::with_capacity(members_of_ability.len());
                for &member in members_of_ability {
                    match abilities_store.get_specialization(member, typ) {
                        None => {
                            let root_data = abilities_store.member_def(member).unwrap();
                            missing_members.push(Loc::at(root_data.region, member));
                        }
                        Some(specialization) => {
                            specialized_members.push(Loc::at(specialization.region, member));
                        }
                    }
                }

                if missing_members.is_empty() {
                    good.push(mia);
                } else {
                    bad.push((
                        mia,
                        IncompleteAbilityImplementation {
                            typ,
                            ability,
                            specialized_members,
                            missing_members,
                        },
                    ));
                }
            }
        }

        // Now figure out what errors we need to report.
        let mut problems = vec![];

        // Keep track of which types that have an incomplete ability were reported as part of
        // another type error (from an expression or pattern). If we reported an error for a type
        // that doesn't implement an ability in that context, we don't want to repeat the error
        // message.
        let mut reported_in_context = vec![];
        let mut incomplete_not_in_context = vec![];

        for (must_implement, on_error) in self.0.into_iter() {
            use AbilityImplError::*;
            match on_error {
                IncompleteAbility => {
                    incomplete_not_in_context.extend(must_implement);
                }
                BadExpr(region, category, var) => {
                    let incomplete_types = must_implement
                        .iter()
                        .filter_map(|e| get_bad!(*e))
                        .cloned()
                        .collect::<Vec<_>>();
                    if !incomplete_types.is_empty() {
                        // Demote the bad variable that exposed this problem to an error, both so
                        // that we have an ErrorType to report and so that codegen knows to deal
                        // with the error later.
                        let (error_type, _moar_ghosts_n_stuff) = subs.var_to_error_type(var);
                        problems.push(TypeError::BadExprMissingAbility(
                            region,
                            category,
                            error_type,
                            incomplete_types,
                        ));
                        reported_in_context.extend(must_implement);
                    }
                }
                BadPattern(region, category, var) => {
                    let incomplete_types = must_implement
                        .iter()
                        .filter_map(|e| get_bad!(*e))
                        .cloned()
                        .collect::<Vec<_>>();
                    if !incomplete_types.is_empty() {
                        // Demote the bad variable that exposed this problem to an error, both so
                        // that we have an ErrorType to report and so that codegen knows to deal
                        // with the error later.
                        let (error_type, _moar_ghosts_n_stuff) = subs.var_to_error_type(var);
                        problems.push(TypeError::BadPatternMissingAbility(
                            region,
                            category,
                            error_type,
                            incomplete_types,
                        ));
                        reported_in_context.extend(must_implement);
                    }
                }
            };
        }

        for mia in incomplete_not_in_context.into_iter() {
            if let Some(must_implement) = get_bad!(mia) {
                if !reported_in_context.contains(&mia) {
                    problems.push(TypeError::IncompleteAbilityImplementation(
                        must_implement.clone(),
                    ));
                }
            }
        }

        problems
    }
}

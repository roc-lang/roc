use roc_can::abilities::AbilitiesStore;
use roc_collections::{VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Subs, Variable};
use roc_types::types::{AliasKind, Category, ErrorType, PatternCategory};
use roc_unify::unify::MustImplementConstraints;
use roc_unify::unify::{MustImplementAbility, Obligated};

use crate::solve::instantiate_rigids;
use crate::solve::TypeError;

#[derive(Debug, Clone)]
pub enum AbilityImplError {
    /// Promote this to an error that the type does not fully implement an ability
    IncompleteAbility,
    /// Promote this error to a `TypeError::BadExpr` from elsewhere
    BadExpr(Region, Category, Variable),
    /// Promote this error to a `TypeError::BadPattern` from elsewhere
    BadPattern(Region, PatternCategory, Variable),
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnderivableReason {
    NotABuiltin,
    /// The surface type is not derivable
    SurfaceNotDerivable,
    /// A nested type is not derivable
    NestedNotDerivable(ErrorType),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Unfulfilled {
    /// Incomplete custom implementation for an ability by an opaque type.
    Incomplete {
        typ: Symbol,
        ability: Symbol,
        specialized_members: Vec<Loc<Symbol>>,
        missing_members: Vec<Loc<Symbol>>,
    },
    /// Cannot derive implementation of an ability for a type.
    Underivable {
        typ: ErrorType,
        ability: Symbol,
        reason: UnderivableReason,
    },
}

#[derive(Default, Debug)]
pub struct DeferredMustImplementAbility {
    obligations: Vec<(MustImplementConstraints, AbilityImplError)>,
}

impl DeferredMustImplementAbility {
    pub fn add(&mut self, must_implement: MustImplementConstraints, on_error: AbilityImplError) {
        self.obligations.push((must_implement, on_error));
    }

    pub fn check_all(self, subs: &mut Subs, abilities_store: &AbilitiesStore) -> Vec<TypeError> {
        let mut problems = vec![];

        let mut obligation_cache = ObligationCache {
            abilities_store,
            unfulfilled: Default::default(),
            checked: VecSet::with_capacity(self.obligations.len()),
        };

        // Keep track of which types that have an incomplete ability were reported as part of
        // another type error (from an expression or pattern). If we reported an error for a type
        // that doesn't implement an ability in that context, we don't want to repeat the error
        // message.
        let mut reported_in_context = vec![];
        let mut incomplete_not_in_context = vec![];

        for (constraints, on_error) in self.obligations.into_iter() {
            let must_implement = constraints.get_unique();

            // First off, make sure we populate information about which of the "must implement"
            // constraints are met, and which aren't.
            for mia in must_implement.iter() {
                obligation_cache.check_one(subs, *mia);
            }

            // Now, figure out what errors we need to report.
            use AbilityImplError::*;
            match on_error {
                IncompleteAbility => {
                    // These aren't attached to another type error, so if these must_implement
                    // constraints aren't met, we'll emit a generic "this type doesn't implement an
                    // ability" error message at the end. We only want to do this if it turns out
                    // the "must implement" constraint indeed wasn't part of a more specific type
                    // error.
                    incomplete_not_in_context.extend(must_implement);
                }
                BadExpr(region, category, var) => {
                    let unfulfilled = must_implement
                        .iter()
                        .filter_map(|mia| obligation_cache.unfulfilled.get(mia))
                        .cloned()
                        .collect::<Vec<_>>();

                    if !unfulfilled.is_empty() {
                        // Demote the bad variable that exposed this problem to an error, both so
                        // that we have an ErrorType to report and so that codegen knows to deal
                        // with the error later.
                        let (error_type, _moar_ghosts_n_stuff) = subs.var_to_error_type(var);
                        problems.push(TypeError::BadExprMissingAbility(
                            region,
                            category,
                            error_type,
                            unfulfilled,
                        ));
                        reported_in_context.extend(must_implement);
                    }
                }
                BadPattern(region, category, var) => {
                    let unfulfilled = must_implement
                        .iter()
                        .filter_map(|mia| obligation_cache.unfulfilled.get(mia))
                        .cloned()
                        .collect::<Vec<_>>();

                    if !unfulfilled.is_empty() {
                        // Demote the bad variable that exposed this problem to an error, both so
                        // that we have an ErrorType to report and so that codegen knows to deal
                        // with the error later.
                        let (error_type, _moar_ghosts_n_stuff) = subs.var_to_error_type(var);
                        problems.push(TypeError::BadPatternMissingAbility(
                            region,
                            category,
                            error_type,
                            unfulfilled,
                        ));
                        reported_in_context.extend(must_implement);
                    }
                }
            }
        }

        // Go through and attach generic "type does not implement ability" errors, if they were not
        // part of a larger context.
        for mia in incomplete_not_in_context.into_iter() {
            if let Some(unfulfilled) = obligation_cache.unfulfilled.get(&mia) {
                if !reported_in_context.contains(&mia) {
                    problems.push(TypeError::UnfulfilledAbility(unfulfilled.clone()));
                }
            }
        }

        problems
    }
}

struct ObligationCache<'a> {
    abilities_store: &'a AbilitiesStore,

    checked: VecSet<MustImplementAbility>,
    unfulfilled: VecMap<MustImplementAbility, Unfulfilled>,
}

impl ObligationCache<'_> {
    fn check_one(&mut self, subs: &mut Subs, mia: MustImplementAbility) {
        if self.checked.contains(&mia) {
            return;
        }

        let MustImplementAbility { typ, ability } = mia;

        match typ {
            Obligated::Opaque(typ) => {
                let members_of_ability = self.abilities_store.members_of_ability(ability).unwrap();
                let mut specialized_members = Vec::with_capacity(members_of_ability.len());
                let mut missing_members = Vec::with_capacity(members_of_ability.len());
                for &member in members_of_ability {
                    match self.abilities_store.get_specialization(member, typ) {
                        None => {
                            let root_data = self.abilities_store.member_def(member).unwrap();
                            missing_members.push(Loc::at(root_data.region, member));
                        }
                        Some(specialization) => {
                            specialized_members.push(Loc::at(specialization.region, member));
                        }
                    }
                }

                if !missing_members.is_empty() {
                    self.unfulfilled.insert(
                        mia,
                        Unfulfilled::Incomplete {
                            typ,
                            ability,
                            specialized_members,
                            missing_members,
                        },
                    );
                }
            }

            Obligated::Adhoc(var) => {
                let opt_can_derive_builtin = match ability {
                    Symbol::ENCODE_ENCODING => Some(self.can_derive_encoding(subs, var)),
                    _ => None,
                };

                let opt_underivable = match opt_can_derive_builtin {
                    Some(Ok(())) => {
                        // can derive!
                        None
                    }
                    Some(Err(failure_var)) => Some(if failure_var == var {
                        UnderivableReason::SurfaceNotDerivable
                    } else {
                        let (error_type, _skeletons) = subs.var_to_error_type(failure_var);
                        UnderivableReason::NestedNotDerivable(error_type)
                    }),
                    None => Some(UnderivableReason::NotABuiltin),
                };

                if let Some(underivable_reason) = opt_underivable {
                    let (error_type, _skeletons) = subs.var_to_error_type(var);
                    self.unfulfilled.insert(
                        mia,
                        Unfulfilled::Underivable {
                            typ: error_type,
                            ability,
                            reason: underivable_reason,
                        },
                    );
                }
            }
        }

        self.checked.insert(mia);
    }

    // If we have a lot of these, consider using a visitor.
    // It will be very similar for most types (can't derive functions, can't derive unbound type
    // variables, can only derive opaques if they have an impl, etc).
    fn can_derive_encoding(&mut self, subs: &mut Subs, var: Variable) -> Result<(), Variable> {
        let mut stack = vec![var];
        let mut seen_recursion_vars = vec![];

        macro_rules! push_var_slice {
            ($slice:expr) => {
                stack.extend(subs.get_subs_slice($slice))
            };
        }

        while let Some(var) = stack.pop() {
            if seen_recursion_vars.contains(&var) {
                continue;
            }

            let content = subs.get_content_without_compacting(var);

            use Content::*;
            use FlatType::*;
            match content {
                FlexVar(_) | RigidVar(_) => return Err(var),
                FlexAbleVar(_, ability) | RigidAbleVar(_, ability) => {
                    if *ability != Symbol::ENCODE_ENCODING {
                        return Err(var);
                    }
                    // Any concrete type this variables is instantiated with will also gain a "does
                    // implement" check so this is okay.
                }
                RecursionVar {
                    structure,
                    opt_name: _,
                } => {
                    seen_recursion_vars.push(var);
                    stack.push(*structure);
                }
                Structure(flat_type) => match flat_type {
                    Apply(
                        Symbol::LIST_LIST | Symbol::SET_SET | Symbol::DICT_DICT | Symbol::STR_STR,
                        vars,
                    ) => push_var_slice!(*vars),
                    Apply(_, ..) => return Err(var),
                    Func(..) => {
                        return Err(var);
                    }
                    Record(fields, var) => {
                        push_var_slice!(fields.variables());
                        stack.push(*var);
                    }
                    TagUnion(tags, ext_var) => {
                        for i in tags.variables() {
                            push_var_slice!(subs[i]);
                        }
                        stack.push(*ext_var);
                    }
                    FunctionOrTagUnion(_, _, var) => stack.push(*var),
                    RecursiveTagUnion(rec_var, tags, ext_var) => {
                        seen_recursion_vars.push(*rec_var);
                        for i in tags.variables() {
                            push_var_slice!(subs[i]);
                        }
                        stack.push(*ext_var);
                    }
                    EmptyRecord | EmptyTagUnion => {
                        // yes
                    }
                    Erroneous(_) => return Err(var),
                },
                Alias(name, _, _, AliasKind::Opaque) => {
                    let mia = MustImplementAbility {
                        typ: Obligated::Opaque(*name),
                        ability: Symbol::ENCODE_ENCODING,
                    };

                    self.check_one(subs, mia);

                    if self.unfulfilled.contains_key(&mia) {
                        return Err(var);
                    }
                }
                Alias(_, arguments, real_type_var, _) => {
                    push_var_slice!(arguments.all_variables());
                    stack.push(*real_type_var);
                }
                RangedNumber(..) => {
                    // yes, all numbers can
                }
                Error => {
                    return Err(var);
                }
            }
        }

        Ok(())
    }
}

/// Determines what type implements an ability member of a specialized signature, given the
/// [MustImplementAbility] constraints of the signature.
pub fn type_implementing_specialization(
    specialization_must_implement_constraints: &MustImplementConstraints,
    ability: Symbol,
) -> Option<Obligated> {
    debug_assert!({
            specialization_must_implement_constraints
                .clone()
                .get_unique()
                .into_iter()
                .filter(|mia| mia.ability == ability)
                .count()
        } < 2,
        "Multiple variables bound to an ability - this is ambiguous and should have been caught in canonicalization: {:?}",
        specialization_must_implement_constraints
    );

    specialization_must_implement_constraints
        .iter_for_ability(ability)
        .next()
        .map(|mia| mia.typ)
}

/// Result of trying to resolve an ability specialization.
#[derive(Clone, Copy)]
pub enum Resolved {
    /// A user-defined specialization should be used.
    Specialization(Symbol),
    /// A specialization must be generated.
    NeedsGenerated,
}

pub fn resolve_ability_specialization(
    subs: &mut Subs,
    abilities_store: &AbilitiesStore,
    ability_member: Symbol,
    specialization_var: Variable,
) -> Option<Resolved> {
    use roc_unify::unify::{unify, Mode};

    let member_def = abilities_store
        .member_def(ability_member)
        .expect("Not an ability member symbol");

    let snapshot = subs.snapshot();

    let signature_var = member_def
        .signature_var()
        .unwrap_or_else(|| internal_error!("Signature var not resolved for {:?}", ability_member));

    instantiate_rigids(subs, signature_var);
    let (_, must_implement_ability) = unify(subs, specialization_var, signature_var, Mode::EQ)
        .expect_success(
            "If resolving a specialization, the specialization must be known to typecheck.",
        );

    subs.rollback_to(snapshot);

    let obligated =
        type_implementing_specialization(&must_implement_ability, member_def.parent_ability)?;

    let resolved = match obligated {
        Obligated::Opaque(symbol) => {
            let specialization = abilities_store.get_specialization(ability_member, symbol)?;

            Resolved::Specialization(specialization.symbol)
        }
        Obligated::Adhoc(_) => {
            // TODO: more rules need to be validated here, like is this a builtin ability?
            Resolved::NeedsGenerated
        }
    };

    Some(resolved)
}

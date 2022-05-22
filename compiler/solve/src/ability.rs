use roc_can::abilities::AbilitiesStore;
use roc_can::expr::PendingDerives;
use roc_collections::VecMap;
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{Content, FlatType, GetSubsSlice, Rank, Subs, Variable};
use roc_types::types::{AliasKind, Category, ErrorType, PatternCategory};
use roc_unify::unify::MustImplementConstraints;
use roc_unify::unify::{MustImplementAbility, Obligated};

use crate::solve::{instantiate_rigids, type_to_var};
use crate::solve::{Aliases, Pools, TypeError};

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
        missing_members: Vec<Loc<Symbol>>,
    },
    /// Cannot derive implementation of an ability for a structural type.
    AdhocUnderivable {
        typ: ErrorType,
        ability: Symbol,
        reason: UnderivableReason,
    },
    /// Cannot derive implementation of an ability for an opaque type.
    OpaqueUnderivable {
        typ: ErrorType,
        ability: Symbol,
        opaque: Symbol,
        derive_region: Region,
        reason: UnderivableReason,
    },
}

/// Indexes a deriving of an ability for an opaque type.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct DeriveKey {
    pub opaque: Symbol,
    pub ability: Symbol,
}

/// Indexes a custom implementation of an ability for an opaque type.
#[derive(Debug, PartialEq, Clone, Copy)]
struct ImplKey {
    opaque: Symbol,
    ability: Symbol,
}

#[derive(Debug)]
pub struct PendingDerivesTable(
    /// derive key -> (opaque type var to use for checking, derive region)
    VecMap<DeriveKey, (Variable, Region)>,
);

impl PendingDerivesTable {
    pub fn new(subs: &mut Subs, aliases: &mut Aliases, pending_derives: PendingDerives) -> Self {
        let mut table = VecMap::with_capacity(pending_derives.len());

        for (opaque, (typ, derives)) in pending_derives.into_iter() {
            for Loc {
                value: ability,
                region,
            } in derives
            {
                debug_assert!(
                    ability.is_builtin_ability(),
                    "Not a builtin - should have been caught during can"
                );
                let derive_key = DeriveKey { opaque, ability };

                // Neither rank nor pools should matter here.
                let opaque_var =
                    type_to_var(subs, Rank::toplevel(), &mut Pools::default(), aliases, &typ);
                let real_var = match subs.get_content_without_compacting(opaque_var) {
                    Content::Alias(_, _, real_var, AliasKind::Opaque) => real_var,
                    _ => internal_error!("Non-opaque in derives table"),
                };

                let old = table.insert(derive_key, (*real_var, region));
                debug_assert!(old.is_none());
            }
        }

        Self(table)
    }
}

#[derive(Debug)]
pub struct DeferredObligations {
    /// Obligations, to be filled in during solving of a module.
    obligations: Vec<(MustImplementConstraints, AbilityImplError)>,
    /// Derives that module-defined opaques claim to have.
    pending_derives: PendingDerivesTable,
    /// Derives that are claimed, but have also been determined to have
    /// specializations. Maps to the first member specialization of the same
    /// ability.
    dominated_derives: VecMap<DeriveKey, Region>,
}

impl DeferredObligations {
    pub fn new(pending_derives: PendingDerivesTable) -> Self {
        Self {
            obligations: Default::default(),
            pending_derives,
            dominated_derives: Default::default(),
        }
    }

    pub fn add(&mut self, must_implement: MustImplementConstraints, on_error: AbilityImplError) {
        self.obligations.push((must_implement, on_error));
    }

    pub fn dominate(&mut self, key: DeriveKey, impl_region: Region) {
        // Only builtin abilities can be derived, and hence dominated.
        if self.pending_derives.0.contains_key(&key) && !self.dominated_derives.contains_key(&key) {
            self.dominated_derives.insert(key, impl_region);
        }
    }

    // Rules for checking ability implementations:
    //  - Ad-hoc derives for structural types are checked on-the-fly
    //  - Opaque derives are registered as "pending" when we check a module
    //  - Opaque derives are always checked and registered at the end to make sure opaque
    //    specializations are found first
    //  - If an opaque O both derives and specializes an ability A
    //    - The specialization is recorded in the abilities store (this is done in solve/solve)
    //    - The derive is checked, but will not be recorded in the abilities store (this is done here)
    //    - Obligations for O to implement A will defer to whether the specialization is complete
    pub fn check_all(
        self,
        subs: &mut Subs,
        abilities_store: &AbilitiesStore,
    ) -> (Vec<TypeError>, Vec<DeriveKey>) {
        let mut problems = vec![];

        let Self {
            obligations,
            pending_derives,
            dominated_derives,
        } = self;

        let mut obligation_cache = ObligationCache {
            abilities_store,
            pending_derives: &pending_derives,
            dominated_derives: &dominated_derives,

            impl_cache: VecMap::with_capacity(obligations.len()),
            derive_cache: VecMap::with_capacity(pending_derives.0.len()),
        };

        let mut legal_derives = Vec::with_capacity(pending_derives.0.len());

        // First, check all derives.
        for (&derive_key, &(opaque_real_var, derive_region)) in pending_derives.0.iter() {
            obligation_cache.check_derive(subs, derive_key, opaque_real_var, derive_region);
            let result = obligation_cache.derive_cache.get(&derive_key).unwrap();
            match result {
                Ok(()) => legal_derives.push(derive_key),
                Err(problem) => problems.push(TypeError::UnfulfilledAbility(problem.clone())),
            }
        }

        for (derive_key, impl_region) in dominated_derives.iter() {
            let derive_region = pending_derives.0.get(derive_key).unwrap().1;

            problems.push(TypeError::DominatedDerive {
                opaque: derive_key.opaque,
                ability: derive_key.ability,
                derive_region,
                impl_region: *impl_region,
            });
        }

        // Keep track of which types that have an incomplete ability were reported as part of
        // another type error (from an expression or pattern). If we reported an error for a type
        // that doesn't implement an ability in that context, we don't want to repeat the error
        // message.
        let mut reported_in_context = vec![];
        let mut incomplete_not_in_context = vec![];

        for (constraints, on_error) in obligations.into_iter() {
            let must_implement = constraints.get_unique();

            let mut get_unfulfilled = |must_implement: &[MustImplementAbility]| {
                must_implement
                    .iter()
                    .filter_map(|mia| {
                        obligation_cache
                            .check_one(subs, *mia)
                            .as_ref()
                            .err()
                            .cloned()
                    })
                    .collect::<Vec<_>>()
            };

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
                    let unfulfilled = get_unfulfilled(&must_implement);

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
                    let unfulfilled = get_unfulfilled(&must_implement);

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
            if let Err(unfulfilled) = obligation_cache.check_one(subs, mia) {
                if !reported_in_context.contains(&mia) {
                    problems.push(TypeError::UnfulfilledAbility(unfulfilled.clone()));
                }
            }
        }

        (problems, legal_derives)
    }
}

type ObligationResult = Result<(), Unfulfilled>;

struct ObligationCache<'a> {
    abilities_store: &'a AbilitiesStore,
    dominated_derives: &'a VecMap<DeriveKey, Region>,
    pending_derives: &'a PendingDerivesTable,

    impl_cache: VecMap<ImplKey, ObligationResult>,
    derive_cache: VecMap<DeriveKey, ObligationResult>,
}

enum ReadCache {
    Impl,
    Derive,
}

impl ObligationCache<'_> {
    fn check_one(&mut self, subs: &mut Subs, mia: MustImplementAbility) -> ObligationResult {
        let MustImplementAbility { typ, ability } = mia;

        match typ {
            Obligated::Adhoc(var) => self.check_adhoc(subs, var, ability),
            Obligated::Opaque(opaque) => self.check_opaque_and_read(subs, opaque, ability).clone(),
        }
    }

    fn check_adhoc(&mut self, subs: &mut Subs, var: Variable, ability: Symbol) -> ObligationResult {
        // Not worth caching ad-hoc checks because variables are unlikely to be the same between
        // independent queries.

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

            Err(Unfulfilled::AdhocUnderivable {
                typ: error_type,
                ability,
                reason: underivable_reason,
            })
        } else {
            Ok(())
        }
    }

    fn check_opaque(&mut self, subs: &mut Subs, opaque: Symbol, ability: Symbol) -> ReadCache {
        let impl_key = ImplKey { opaque, ability };
        let derive_key = DeriveKey { opaque, ability };

        match self.pending_derives.0.get(&derive_key) {
            Some(&(opaque_real_var, derive_region)) => {
                if self.dominated_derives.contains_key(&derive_key) {
                    // We have a derive, but also a custom implementation. The custom
                    // implementation takes priority because we'll use that for codegen.
                    // We'll report an error for the conflict, and whether the derive is
                    // legal will be checked out-of-band.
                    self.check_impl(impl_key);
                    ReadCache::Impl
                } else {
                    // Only a derive
                    self.check_derive(subs, derive_key, opaque_real_var, derive_region);
                    ReadCache::Derive
                }
            }
            // Only an impl
            None => {
                self.check_impl(impl_key);
                ReadCache::Impl
            }
        }
    }

    fn check_opaque_and_read(
        &mut self,
        subs: &mut Subs,
        opaque: Symbol,
        ability: Symbol,
    ) -> &ObligationResult {
        match self.check_opaque(subs, opaque, ability) {
            ReadCache::Impl => self.impl_cache.get(&ImplKey { opaque, ability }).unwrap(),
            ReadCache::Derive => self
                .derive_cache
                .get(&DeriveKey { opaque, ability })
                .unwrap(),
        }
    }

    fn check_impl(&mut self, impl_key: ImplKey) {
        if self.impl_cache.get(&impl_key).is_some() {
            return;
        }

        let ImplKey { opaque, ability } = impl_key;

        let members_of_ability = self.abilities_store.members_of_ability(ability).unwrap();
        let mut missing_members = Vec::new();
        for &member in members_of_ability {
            if self
                .abilities_store
                .get_specialization(member, opaque)
                .is_none()
            {
                let root_data = self.abilities_store.member_def(member).unwrap();
                missing_members.push(Loc::at(root_data.region, member));
            }
        }

        let obligation_result = if !missing_members.is_empty() {
            Err(Unfulfilled::Incomplete {
                typ: opaque,
                ability,
                missing_members,
            })
        } else {
            Ok(())
        };

        self.impl_cache.insert(impl_key, obligation_result);
    }

    fn check_derive(
        &mut self,
        subs: &mut Subs,
        derive_key: DeriveKey,
        opaque_real_var: Variable,
        derive_region: Region,
    ) {
        if self.derive_cache.get(&derive_key).is_some() {
            return;
        }

        // The opaque may be recursive, so make sure we stop if we see it again.
        // We need to enforce that on both the impl and derive cache.
        let fake_fulfilled = Ok(());
        // If this opaque both derives and specializes, we may already know whether the
        // specialization fulfills or not. Since specializations take priority over derives, we
        // want to keep that result around.
        let impl_key = ImplKey {
            opaque: derive_key.opaque,
            ability: derive_key.ability,
        };
        let opt_specialization_result = self.impl_cache.insert(impl_key, fake_fulfilled.clone());
        let is_dominated = self.dominated_derives.contains_key(&derive_key);
        debug_assert!(
            opt_specialization_result.is_none() || is_dominated,
            "This derive also has a specialization but it's not marked as dominated!"
        );

        let old_deriving = self.derive_cache.insert(derive_key, fake_fulfilled.clone());
        debug_assert!(
            old_deriving.is_none(),
            "Already knew deriving result, but here anyway"
        );

        // Now we check whether the structural type behind the opaque is derivable, since that's
        // what we'll need to generate an implementation for during codegen.
        let real_var_result = self.check_adhoc(subs, opaque_real_var, derive_key.ability);

        let root_result = real_var_result.map_err(|err| match err {
            // Promote the failure, which should be related to a structural type not being
            // derivable for the ability, to a failure regarding the opaque in particular.
            Unfulfilled::AdhocUnderivable {
                typ,
                ability,
                reason,
            } => Unfulfilled::OpaqueUnderivable {
                typ,
                ability,
                reason,
                opaque: derive_key.opaque,
                derive_region,
            },
            _ => internal_error!("unexpected underivable result"),
        });

        // Remove the derive result because the specialization check should take priority.
        let check_has_fake = self.impl_cache.remove(&impl_key);
        debug_assert_eq!(check_has_fake.map(|(_, b)| b), Some(fake_fulfilled.clone()));

        if let Some(specialization_result) = opt_specialization_result {
            self.impl_cache.insert(impl_key, specialization_result);
        }

        // Make sure we fix-up with the correct result of the check.
        let check_has_fake = self.derive_cache.insert(derive_key, root_result);
        debug_assert_eq!(check_has_fake, Some(fake_fulfilled));
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
                    Apply(..) => return Err(var),
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
                    let opaque = *name;
                    if self
                        .check_opaque_and_read(subs, opaque, Symbol::ENCODE_ENCODING)
                        .is_err()
                    {
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

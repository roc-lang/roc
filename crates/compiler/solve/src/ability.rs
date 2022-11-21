use roc_can::abilities::AbilitiesStore;
use roc_can::expr::PendingDerives;
use roc_collections::{VecMap, VecSet};
#[cfg(debug_assertions)]
use roc_debug_flags::{dbg_do, ROC_PRINT_UNDERIVABLE};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_solve_problem::{
    NotDerivableContext, NotDerivableDecode, NotDerivableEq, TypeError, UnderivableReason,
    Unfulfilled,
};
use roc_types::num::NumericRange;
use roc_types::subs::{
    instantiate_rigids, Content, FlatType, GetSubsSlice, Rank, RecordFields, Subs, SubsSlice,
    Variable,
};
use roc_types::types::{AliasKind, Category, MemberImpl, PatternCategory, Polarity, Types};
use roc_unify::unify::{Env, MustImplementConstraints};
use roc_unify::unify::{MustImplementAbility, Obligated};

use crate::solve::type_to_var;
use crate::solve::{Aliases, Pools};

#[derive(Debug, Clone)]
pub enum AbilityImplError {
    /// Promote this to a generic error that a type doesn't implement an ability
    DoesNotImplement,
    /// Promote this error to a `TypeError::BadExpr` from elsewhere
    BadExpr(Region, Category, Variable),
    /// Promote this error to a `TypeError::BadPattern` from elsewhere
    BadPattern(Region, PatternCategory, Variable),
}

/// Indexes a requested deriving of an ability for an opaque type.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct RequestedDeriveKey {
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
    VecMap<RequestedDeriveKey, (Variable, Region)>,
);

impl PendingDerivesTable {
    pub fn new(
        subs: &mut Subs,
        types: &mut Types,
        aliases: &mut Aliases,
        pending_derives: PendingDerives,
        problems: &mut Vec<TypeError>,
        abilities_store: &mut AbilitiesStore,
        obligation_cache: &mut ObligationCache,
    ) -> Self {
        let mut table = VecMap::with_capacity(pending_derives.len());

        for (opaque, (typ, derives)) in pending_derives.into_iter() {
            for Loc {
                value: ability,
                region,
            } in derives
            {
                debug_assert!(
                    ability.is_derivable_ability(),
                    "Not a builtin - should have been caught during can"
                );
                let derive_key = RequestedDeriveKey { opaque, ability };

                // Neither rank nor pools should matter here.
                let typ = types.from_old_type(&typ);
                let opaque_var = type_to_var(
                    subs,
                    Rank::toplevel(),
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut Pools::default(),
                    types,
                    aliases,
                    typ,
                );
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

type ObligationResult = Result<(), Unfulfilled>;

#[derive(Default)]
pub struct ObligationCache {
    impl_cache: VecMap<ImplKey, ObligationResult>,
    derive_cache: VecMap<RequestedDeriveKey, ObligationResult>,
}

enum ReadCache {
    Impl,
}

pub struct CheckedDerives {
    pub legal_derives: Vec<RequestedDeriveKey>,
    pub problems: Vec<TypeError>,
}

impl ObligationCache {
    #[must_use]
    pub fn check_derives(
        &mut self,
        subs: &mut Subs,
        abilities_store: &AbilitiesStore,
        pending_derives: PendingDerivesTable,
    ) -> CheckedDerives {
        let mut legal_derives = Vec::with_capacity(pending_derives.0.len());
        let mut problems = vec![];

        // First, check all derives.
        for (&derive_key, &(opaque_real_var, derive_region)) in pending_derives.0.iter() {
            self.check_derive(
                subs,
                abilities_store,
                derive_key,
                opaque_real_var,
                derive_region,
            );
            let result = self.derive_cache.get(&derive_key).unwrap();
            match result {
                Ok(()) => legal_derives.push(derive_key),
                Err(problem) => problems.push(TypeError::UnfulfilledAbility(problem.clone())),
            }
        }

        CheckedDerives {
            legal_derives,
            problems,
        }
    }

    #[must_use]
    pub fn check_obligations(
        &mut self,
        subs: &mut Subs,
        abilities_store: &AbilitiesStore,
        must_implement: MustImplementConstraints,
        on_error: AbilityImplError,
    ) -> Vec<TypeError> {
        let must_implement = must_implement.get_unique();

        let mut get_unfulfilled = |must_implement: &[MustImplementAbility]| {
            must_implement
                .iter()
                .filter_map(|mia| {
                    self.check_one(subs, abilities_store, *mia)
                        .as_ref()
                        .err()
                        .cloned()
                })
                .collect::<Vec<_>>()
        };

        let mut reported_in_context = VecSet::default();
        let mut incomplete_not_in_context = VecSet::default();
        let mut problems = vec![];

        use AbilityImplError::*;
        match on_error {
            DoesNotImplement => {
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
                    let error_type = subs.var_to_error_type(var, Polarity::OF_VALUE);
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
                    let error_type = subs.var_to_error_type(var, Polarity::OF_PATTERN);
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

        // Go through and attach generic "type does not implement ability" errors, if they were not
        // part of a larger context.
        for mia in incomplete_not_in_context.into_iter() {
            // If the obligation is already cached, we must have already reported it in another
            // context.
            if !self.has_cached(mia) && !reported_in_context.contains(&mia) {
                if let Err(unfulfilled) = self.check_one(subs, abilities_store, mia) {
                    problems.push(TypeError::UnfulfilledAbility(unfulfilled.clone()));
                }
            }
        }

        problems
    }

    fn check_one(
        &mut self,
        subs: &mut Subs,
        abilities_store: &AbilitiesStore,
        mia: MustImplementAbility,
    ) -> ObligationResult {
        let MustImplementAbility { typ, ability } = mia;

        match typ {
            Obligated::Adhoc(var) => self.check_adhoc(subs, abilities_store, var, ability),
            Obligated::Opaque(opaque) => self
                .check_opaque_and_read(abilities_store, opaque, ability)
                .clone(),
        }
    }

    fn has_cached(&self, mia: MustImplementAbility) -> bool {
        match mia.typ {
            Obligated::Opaque(opaque) => self.impl_cache.contains_key(&ImplKey {
                opaque,
                ability: mia.ability,
            }),
            Obligated::Adhoc(_) => {
                // ad-hoc obligations are never cached
                false
            }
        }
    }

    fn check_adhoc(
        &mut self,
        subs: &mut Subs,
        abilities_store: &AbilitiesStore,
        var: Variable,
        ability: Symbol,
    ) -> ObligationResult {
        // Not worth caching ad-hoc checks because variables are unlikely to be the same between
        // independent queries.

        let opt_can_derive_builtin = match ability {
            Symbol::ENCODE_ENCODING => Some(DeriveEncoding::is_derivable(
                self,
                abilities_store,
                subs,
                var,
            )),

            Symbol::DECODE_DECODING => Some(DeriveDecoding::is_derivable(
                self,
                abilities_store,
                subs,
                var,
            )),

            Symbol::HASH_HASH_ABILITY => {
                Some(DeriveHash::is_derivable(self, abilities_store, subs, var))
            }

            Symbol::BOOL_EQ => Some(DeriveEq::is_derivable(self, abilities_store, subs, var)),

            _ => None,
        };

        let opt_underivable = match opt_can_derive_builtin {
            Some(Ok(())) => {
                // can derive!
                None
            }
            Some(Err(NotDerivable {
                var: failure_var,
                context,
            })) => {
                dbg_do!(ROC_PRINT_UNDERIVABLE, {
                    eprintln!("❌ derived {:?} of {:?}", ability, subs.dbg(failure_var));
                });

                Some(if failure_var == var {
                    UnderivableReason::SurfaceNotDerivable(context)
                } else {
                    let error_type = subs.var_to_error_type(failure_var, Polarity::OF_VALUE);
                    UnderivableReason::NestedNotDerivable(error_type, context)
                })
            }
            None => Some(UnderivableReason::NotABuiltin),
        };

        if let Some(underivable_reason) = opt_underivable {
            let error_type = subs.var_to_error_type(var, Polarity::OF_VALUE);

            Err(Unfulfilled::AdhocUnderivable {
                typ: error_type,
                ability,
                reason: underivable_reason,
            })
        } else {
            Ok(())
        }
    }

    fn check_opaque(
        &mut self,
        abilities_store: &AbilitiesStore,
        opaque: Symbol,
        ability: Symbol,
    ) -> ReadCache {
        let impl_key = ImplKey { opaque, ability };

        self.check_impl(abilities_store, impl_key);
        ReadCache::Impl
    }

    fn check_opaque_and_read(
        &mut self,
        abilities_store: &AbilitiesStore,
        opaque: Symbol,
        ability: Symbol,
    ) -> &ObligationResult {
        match self.check_opaque(abilities_store, opaque, ability) {
            ReadCache::Impl => self.impl_cache.get(&ImplKey { opaque, ability }).unwrap(),
        }
    }

    fn check_impl(&mut self, abilities_store: &AbilitiesStore, impl_key: ImplKey) {
        if self.impl_cache.get(&impl_key).is_some() {
            return;
        }

        let ImplKey { opaque, ability } = impl_key;
        let has_declared_impl = abilities_store.has_declared_implementation(opaque, ability);

        let obligation_result = if !has_declared_impl {
            Err(Unfulfilled::OpaqueDoesNotImplement {
                typ: opaque,
                ability,
            })
        } else {
            Ok(())
        };

        self.impl_cache.insert(impl_key, obligation_result);
    }

    fn check_derive(
        &mut self,
        subs: &mut Subs,
        abilities_store: &AbilitiesStore,
        derive_key: RequestedDeriveKey,
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

        let old_deriving = self.derive_cache.insert(derive_key, fake_fulfilled.clone());
        debug_assert!(
            old_deriving.is_none(),
            "Already knew deriving result, but here anyway"
        );

        // Now we check whether the structural type behind the opaque is derivable, since that's
        // what we'll need to generate an implementation for during codegen.
        let real_var_result =
            self.check_adhoc(subs, abilities_store, opaque_real_var, derive_key.ability);

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
}

#[inline(always)]
#[rustfmt::skip]
fn is_builtin_int_alias(symbol: Symbol) -> bool {
    matches!(symbol,
          Symbol::NUM_U8   | Symbol::NUM_UNSIGNED8
        | Symbol::NUM_U16  | Symbol::NUM_UNSIGNED16
        | Symbol::NUM_U32  | Symbol::NUM_UNSIGNED32
        | Symbol::NUM_U64  | Symbol::NUM_UNSIGNED64
        | Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128
        | Symbol::NUM_I8   | Symbol::NUM_SIGNED8
        | Symbol::NUM_I16  | Symbol::NUM_SIGNED16
        | Symbol::NUM_I32  | Symbol::NUM_SIGNED32
        | Symbol::NUM_I64  | Symbol::NUM_SIGNED64
        | Symbol::NUM_I128 | Symbol::NUM_SIGNED128
        | Symbol::NUM_NAT  | Symbol::NUM_NATURAL
    )
}

#[inline(always)]
#[rustfmt::skip]
fn is_builtin_float_alias(symbol: Symbol) -> bool {
    matches!(symbol,
        | Symbol::NUM_F32  | Symbol::NUM_BINARY32
        | Symbol::NUM_F64  | Symbol::NUM_BINARY64
    )
}

#[inline(always)]
#[rustfmt::skip]
fn is_builtin_dec_alias(symbol: Symbol) -> bool {
    matches!(symbol,
        | Symbol::NUM_DEC  | Symbol::NUM_DECIMAL,
    )
}

#[inline(always)]
#[rustfmt::skip]
fn is_builtin_number_alias(symbol: Symbol) -> bool {
    is_builtin_int_alias(symbol) || is_builtin_float_alias(symbol) || is_builtin_dec_alias(symbol)
}

struct NotDerivable {
    var: Variable,
    context: NotDerivableContext,
}

struct Descend(bool);

trait DerivableVisitor {
    const ABILITY: Symbol;
    const ABILITY_SLICE: SubsSlice<Symbol>;

    #[inline(always)]
    fn is_derivable_builtin_opaque(_symbol: Symbol) -> bool {
        false
    }

    #[inline(always)]
    fn visit_recursion(var: Variable) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_apply(var: Variable, _symbol: Symbol) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_func(var: Variable) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::Function,
        })
    }

    #[inline(always)]
    fn visit_record(
        _subs: &Subs,
        var: Variable,
        _fields: RecordFields,
    ) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_tag_union(var: Variable) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_recursive_tag_union(var: Variable) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_function_or_tag_union(var: Variable) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_empty_record(var: Variable) -> Result<(), NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_empty_tag_union(var: Variable) -> Result<(), NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_alias(var: Variable, _symbol: Symbol) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_floating_point_content(
        var: Variable,
        _subs: &mut Subs,
        _content_var: Variable,
    ) -> Result<Descend, NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn visit_ranged_number(var: Variable, _range: NumericRange) -> Result<(), NotDerivable> {
        Err(NotDerivable {
            var,
            context: NotDerivableContext::NoContext,
        })
    }

    #[inline(always)]
    fn is_derivable(
        obligation_cache: &mut ObligationCache,
        abilities_store: &AbilitiesStore,
        subs: &mut Subs,
        var: Variable,
    ) -> Result<(), NotDerivable> {
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
            match *content {
                FlexVar(opt_name) => {
                    // Promote the flex var to be bound to the ability.
                    subs.set_content(var, Content::FlexAbleVar(opt_name, Self::ABILITY_SLICE));
                }
                RigidVar(_) => {
                    return Err(NotDerivable {
                        var,
                        context: NotDerivableContext::UnboundVar,
                    })
                }
                FlexAbleVar(opt_name, abilities) => {
                    // This flex var inherits the ability.
                    let merged_abilites = roc_unify::unify::merged_ability_slices(
                        subs,
                        abilities,
                        Self::ABILITY_SLICE,
                    );
                    subs.set_content(var, Content::FlexAbleVar(opt_name, merged_abilites));
                }
                RigidAbleVar(_, abilities) => {
                    if !subs.get_subs_slice(abilities).contains(&Self::ABILITY) {
                        return Err(NotDerivable {
                            var,
                            context: NotDerivableContext::NoContext,
                        });
                    }
                }
                RecursionVar {
                    structure,
                    opt_name: _,
                } => {
                    let descend = Self::visit_recursion(var)?;
                    if descend.0 {
                        seen_recursion_vars.push(var);
                        stack.push(structure);
                    }
                }
                Structure(flat_type) => match flat_type {
                    Apply(symbol, vars) => {
                        let descend = Self::visit_apply(var, symbol)?;
                        if descend.0 {
                            push_var_slice!(vars)
                        }
                    }
                    Func(args, _clos, ret) => {
                        let descend = Self::visit_func(var)?;
                        if descend.0 {
                            push_var_slice!(args);
                            stack.push(ret);
                        }
                    }
                    Record(fields, ext) => {
                        let descend = Self::visit_record(subs, var, fields)?;
                        if descend.0 {
                            push_var_slice!(fields.variables());
                            if !matches!(
                                subs.get_content_without_compacting(ext),
                                Content::FlexVar(_) | Content::RigidVar(_)
                            ) {
                                // TODO: currently, just we suppose the presence of a flex var may
                                // include more or less things which we can derive. But, we should
                                // instead recurse here, and add a `t ~ u | u has Decode` constraint as needed.
                                stack.push(ext);
                            }
                        }
                    }
                    TagUnion(tags, ext) => {
                        let descend = Self::visit_tag_union(var)?;
                        if descend.0 {
                            for i in tags.variables() {
                                push_var_slice!(subs[i]);
                            }
                            stack.push(ext);
                        }
                    }
                    FunctionOrTagUnion(_tag_name, _fn_name, ext) => {
                        let descend = Self::visit_function_or_tag_union(var)?;
                        if descend.0 {
                            stack.push(ext);
                        }
                    }
                    RecursiveTagUnion(rec, tags, ext) => {
                        let descend = Self::visit_recursive_tag_union(var)?;
                        if descend.0 {
                            seen_recursion_vars.push(rec);
                            for i in tags.variables() {
                                push_var_slice!(subs[i]);
                            }
                            stack.push(ext);
                        }
                    }
                    EmptyRecord => Self::visit_empty_record(var)?,
                    EmptyTagUnion => Self::visit_empty_tag_union(var)?,
                },
                Alias(
                    Symbol::NUM_NUM | Symbol::NUM_INTEGER,
                    _alias_variables,
                    real_var,
                    AliasKind::Opaque,
                ) => {
                    // Unbound numbers and integers: always decay until a ground is hit,
                    // since all of our builtin abilities currently support integers.
                    stack.push(real_var);
                }
                Alias(Symbol::NUM_FLOATINGPOINT, _alias_variables, real_var, AliasKind::Opaque) => {
                    let descend = Self::visit_floating_point_content(var, subs, real_var)?;
                    if descend.0 {
                        // Decay to a ground
                        stack.push(real_var)
                    }
                }
                Alias(opaque, _alias_variables, _real_var, AliasKind::Opaque) => {
                    if obligation_cache
                        .check_opaque_and_read(abilities_store, opaque, Self::ABILITY)
                        .is_err()
                        && !Self::is_derivable_builtin_opaque(opaque)
                    {
                        return Err(NotDerivable {
                            var,
                            context: NotDerivableContext::Opaque(opaque),
                        });
                    }
                }
                Alias(symbol, _alias_variables, real_var, AliasKind::Structural) => {
                    let descend = Self::visit_alias(var, symbol)?;
                    if descend.0 {
                        stack.push(real_var);
                    }
                }
                RangedNumber(range) => Self::visit_ranged_number(var, range)?,

                LambdaSet(..) => {
                    return Err(NotDerivable {
                        var,
                        context: NotDerivableContext::NoContext,
                    })
                }
                Error => {
                    return Err(NotDerivable {
                        var,
                        context: NotDerivableContext::NoContext,
                    });
                }
            }
        }

        Ok(())
    }
}

struct DeriveEncoding;
impl DerivableVisitor for DeriveEncoding {
    const ABILITY: Symbol = Symbol::ENCODE_ENCODING;
    const ABILITY_SLICE: SubsSlice<Symbol> = Subs::AB_ENCODING;

    #[inline(always)]
    fn is_derivable_builtin_opaque(symbol: Symbol) -> bool {
        is_builtin_number_alias(symbol)
    }

    #[inline(always)]
    fn visit_recursion(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_apply(var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if matches!(
            symbol,
            Symbol::LIST_LIST | Symbol::SET_SET | Symbol::DICT_DICT | Symbol::STR_STR,
        ) {
            Ok(Descend(true))
        } else {
            Err(NotDerivable {
                var,
                context: NotDerivableContext::NoContext,
            })
        }
    }

    #[inline(always)]
    fn visit_record(
        _subs: &Subs,
        _var: Variable,
        _fields: RecordFields,
    ) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_recursive_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_function_or_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_empty_record(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_empty_tag_union(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_alias(_var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if is_builtin_number_alias(symbol) {
            Ok(Descend(false))
        } else {
            Ok(Descend(true))
        }
    }

    #[inline(always)]
    fn visit_ranged_number(_var: Variable, _range: NumericRange) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_floating_point_content(
        _var: Variable,
        _subs: &mut Subs,
        _content_var: Variable,
    ) -> Result<Descend, NotDerivable> {
        Ok(Descend(false))
    }
}

struct DeriveDecoding;
impl DerivableVisitor for DeriveDecoding {
    const ABILITY: Symbol = Symbol::DECODE_DECODING;
    const ABILITY_SLICE: SubsSlice<Symbol> = Subs::AB_DECODING;

    #[inline(always)]
    fn is_derivable_builtin_opaque(symbol: Symbol) -> bool {
        is_builtin_number_alias(symbol)
    }

    #[inline(always)]
    fn visit_recursion(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_apply(var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if matches!(
            symbol,
            Symbol::LIST_LIST | Symbol::SET_SET | Symbol::DICT_DICT | Symbol::STR_STR,
        ) {
            Ok(Descend(true))
        } else {
            Err(NotDerivable {
                var,
                context: NotDerivableContext::NoContext,
            })
        }
    }

    #[inline(always)]
    fn visit_record(
        subs: &Subs,
        var: Variable,
        fields: RecordFields,
    ) -> Result<Descend, NotDerivable> {
        for (field_name, _, field) in fields.iter_all() {
            if subs[field].is_optional() {
                return Err(NotDerivable {
                    var,
                    context: NotDerivableContext::Decode(NotDerivableDecode::OptionalRecordField(
                        subs[field_name].clone(),
                    )),
                });
            }
        }

        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_recursive_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_function_or_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_empty_record(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_empty_tag_union(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_alias(_var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if is_builtin_number_alias(symbol) {
            Ok(Descend(false))
        } else {
            Ok(Descend(true))
        }
    }

    #[inline(always)]
    fn visit_ranged_number(_var: Variable, _range: NumericRange) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_floating_point_content(
        _var: Variable,
        _subs: &mut Subs,
        _content_var: Variable,
    ) -> Result<Descend, NotDerivable> {
        Ok(Descend(false))
    }
}

struct DeriveHash;
impl DerivableVisitor for DeriveHash {
    const ABILITY: Symbol = Symbol::HASH_HASH_ABILITY;
    const ABILITY_SLICE: SubsSlice<Symbol> = Subs::AB_HASH;

    #[inline(always)]
    fn is_derivable_builtin_opaque(symbol: Symbol) -> bool {
        is_builtin_number_alias(symbol)
    }

    #[inline(always)]
    fn visit_recursion(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_apply(var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if matches!(
            symbol,
            Symbol::LIST_LIST | Symbol::SET_SET | Symbol::DICT_DICT | Symbol::STR_STR,
        ) {
            Ok(Descend(true))
        } else {
            Err(NotDerivable {
                var,
                context: NotDerivableContext::NoContext,
            })
        }
    }

    #[inline(always)]
    fn visit_record(
        subs: &Subs,
        var: Variable,
        fields: RecordFields,
    ) -> Result<Descend, NotDerivable> {
        for (field_name, _, field) in fields.iter_all() {
            if subs[field].is_optional() {
                return Err(NotDerivable {
                    var,
                    context: NotDerivableContext::Decode(NotDerivableDecode::OptionalRecordField(
                        subs[field_name].clone(),
                    )),
                });
            }
        }

        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_recursive_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_function_or_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_empty_record(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_empty_tag_union(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_alias(_var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if is_builtin_number_alias(symbol) {
            Ok(Descend(false))
        } else {
            Ok(Descend(true))
        }
    }

    #[inline(always)]
    fn visit_ranged_number(_var: Variable, _range: NumericRange) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_floating_point_content(
        _var: Variable,
        _subs: &mut Subs,
        _content_var: Variable,
    ) -> Result<Descend, NotDerivable> {
        Ok(Descend(false))
    }
}

struct DeriveEq;
impl DerivableVisitor for DeriveEq {
    const ABILITY: Symbol = Symbol::BOOL_EQ;
    const ABILITY_SLICE: SubsSlice<Symbol> = Subs::AB_EQ;

    #[inline(always)]
    fn is_derivable_builtin_opaque(symbol: Symbol) -> bool {
        is_builtin_int_alias(symbol) || is_builtin_dec_alias(symbol)
    }

    #[inline(always)]
    fn visit_recursion(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_apply(var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if matches!(
            symbol,
            Symbol::LIST_LIST
                | Symbol::SET_SET
                | Symbol::DICT_DICT
                | Symbol::STR_STR
                | Symbol::BOX_BOX_TYPE,
        ) {
            Ok(Descend(true))
        } else {
            Err(NotDerivable {
                var,
                context: NotDerivableContext::NoContext,
            })
        }
    }

    #[inline(always)]
    fn visit_record(
        subs: &Subs,
        var: Variable,
        fields: RecordFields,
    ) -> Result<Descend, NotDerivable> {
        for (field_name, _, field) in fields.iter_all() {
            if subs[field].is_optional() {
                return Err(NotDerivable {
                    var,
                    context: NotDerivableContext::Decode(NotDerivableDecode::OptionalRecordField(
                        subs[field_name].clone(),
                    )),
                });
            }
        }

        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_recursive_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_function_or_tag_union(_var: Variable) -> Result<Descend, NotDerivable> {
        Ok(Descend(true))
    }

    #[inline(always)]
    fn visit_empty_record(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_empty_tag_union(_var: Variable) -> Result<(), NotDerivable> {
        Ok(())
    }

    #[inline(always)]
    fn visit_alias(var: Variable, symbol: Symbol) -> Result<Descend, NotDerivable> {
        if is_builtin_float_alias(symbol) {
            Err(NotDerivable {
                var,
                context: NotDerivableContext::Eq(NotDerivableEq::FloatingPoint),
            })
        } else if is_builtin_number_alias(symbol) {
            Ok(Descend(false))
        } else {
            Ok(Descend(true))
        }
    }

    fn visit_floating_point_content(
        var: Variable,
        subs: &mut Subs,
        content_var: Variable,
    ) -> Result<Descend, NotDerivable> {
        use roc_unify::unify::{unify, Mode};

        // Of the floating-point types,
        // only Dec implements Eq.
        let mut env = Env::new(subs);
        let unified = unify(
            &mut env,
            content_var,
            Variable::DECIMAL,
            Mode::EQ,
            Polarity::Pos,
        );
        match unified {
            roc_unify::unify::Unified::Success { .. } => Ok(Descend(false)),
            roc_unify::unify::Unified::Failure(..) => Err(NotDerivable {
                var,
                context: NotDerivableContext::Eq(NotDerivableEq::FloatingPoint),
            }),
        }
    }

    #[inline(always)]
    fn visit_ranged_number(_var: Variable, _range: NumericRange) -> Result<(), NotDerivable> {
        // Ranged numbers are allowed, because they are always possibly ints - floats can not have
        // `isEq` derived, but if something were to be a float, we'd see it exactly as a float.
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
#[derive(Clone, Copy, Debug)]
pub enum Resolved {
    /// A user-defined specialization should be used.
    Specialization(Symbol),
    /// A specialization must be generated for the given type variable.
    NeedsGenerated(Variable),
}

/// An [`AbilityResolver`] is a shell of an abilities store that answers questions needed for
/// [resolving ability specializations][`resolve_ability_specialization`].
///
/// The trait is provided so you can implement your own resolver at other points in the compilation
/// process, for example during monomorphization we have module-re-entrant ability stores that are
/// not available during solving.
pub trait AbilityResolver {
    /// Gets the parent ability and type of an ability member.
    ///
    /// If needed, the type of the ability member will be imported into a local `subs` buffer; as
    /// such, subs must be provided.
    fn member_parent_and_signature_var(
        &self,
        ability_member: Symbol,
        home_subs: &mut Subs,
    ) -> Option<(Symbol, Variable)>;

    /// Finds the declared implementation of an [`ImplKey`][roc_can::abilities::ImplKey].
    fn get_implementation(&self, impl_key: roc_can::abilities::ImplKey) -> Option<MemberImpl>;
}

/// Trivial implementation of a resolver for a module-local abilities store, that defers all
/// queries to the module store.
impl AbilityResolver for AbilitiesStore {
    #[inline(always)]
    fn member_parent_and_signature_var(
        &self,
        ability_member: Symbol,
        _home_subs: &mut Subs, // only have access to one abilities store, do nothing with subs
    ) -> Option<(Symbol, Variable)> {
        self.member_def(ability_member)
            .map(|def| (def.parent_ability, def.signature_var()))
    }

    #[inline(always)]
    fn get_implementation(&self, impl_key: roc_can::abilities::ImplKey) -> Option<MemberImpl> {
        self.get_implementation(impl_key).copied()
    }
}

pub fn resolve_ability_specialization<R: AbilityResolver>(
    subs: &mut Subs,
    resolver: &R,
    ability_member: Symbol,
    specialization_var: Variable,
) -> Option<Resolved> {
    use roc_unify::unify::{unify, Mode};

    let (parent_ability, signature_var) = resolver
        .member_parent_and_signature_var(ability_member, subs)
        .expect("Not an ability member symbol");

    // Figure out the ability we're resolving in a temporary subs snapshot.
    let snapshot = subs.snapshot();

    instantiate_rigids(subs, signature_var);
    let (_vars, must_implement_ability, _lambda_sets_to_specialize, _meta) = unify(
        &mut Env::new(subs),
        specialization_var,
        signature_var,
        Mode::EQ,
        Polarity::Pos,
    )
    .expect_success(
        "If resolving a specialization, the specialization must be known to typecheck.",
    );

    subs.rollback_to(snapshot);

    let obligated = type_implementing_specialization(&must_implement_ability, parent_ability)?;

    let resolved = match obligated {
        Obligated::Opaque(symbol) => {
            let impl_key = roc_can::abilities::ImplKey {
                opaque: symbol,
                ability_member,
            };

            match resolver.get_implementation(impl_key)? {
                roc_types::types::MemberImpl::Impl(spec_symbol) => {
                    Resolved::Specialization(spec_symbol)
                }
                // TODO this is not correct. We can replace `Resolved` with `MemberImpl` entirely,
                // which will make this simpler.
                roc_types::types::MemberImpl::Error => Resolved::Specialization(Symbol::UNDERSCORE),
            }
        }
        Obligated::Adhoc(variable) => {
            // TODO: more rules need to be validated here, like is this a builtin ability?
            Resolved::NeedsGenerated(variable)
        }
    };

    Some(resolved)
}

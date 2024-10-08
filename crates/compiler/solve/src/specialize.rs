//! Module [specialize] is resolves specialization lambda sets.

use std::collections::VecDeque;

use roc_can::abilities::{AbilitiesStore, ImplKey};
use roc_collections::{soa::slice_extend_new, VecMap, VecSet};
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::ROC_TRACE_COMPACTION;
use roc_derive_key::{DeriveError, DeriveKey};
use roc_error_macros::{internal_error, todo_abilities};
use roc_module::symbol::{ModuleId, Symbol};
use roc_solve_schema::UnificationMode;
use roc_types::{
    subs::{
        get_member_lambda_sets_at_region, Content, Descriptor, GetSubsSlice, LambdaSet, Mark,
        OptVariable, Rank, Subs, UlsOfVar, Variable,
    },
    types::{AliasKind, MemberImpl, Polarity, Uls},
};
use roc_unify::unify::{unify, MustImplementConstraints};

use crate::{
    ability::builtin_module_with_unlisted_ability_impl,
    deep_copy::deep_copy_var_in,
    env::{DerivedEnv, SolveEnv},
};

/// What phase in the compiler is reaching out to specialize lambda sets?
/// This is important to distinguish subtle differences in the behavior of the solving algorithm.
//
// TODO the APIs of this trait suck, this needs a nice cleanup.
pub trait Phase {
    /// The regular type-solving phase, or during some later phase of compilation.
    /// During the solving phase we must anticipate that some information is still unknown and react to
    /// that; during late phases, we expect that all information is resolved.
    const IS_LATE: bool;

    fn with_module_abilities_store<T, F>(&self, module: ModuleId, f: F) -> T
    where
        F: FnMut(&AbilitiesStore) -> T;

    /// Given a known lambda set's ambient function in an external module, copy that ambient
    /// function into the given subs.
    fn copy_lambda_set_ambient_function_to_home_subs(
        &self,
        external_lambda_set_var: Variable,
        external_module_id: ModuleId,
        home_subs: &mut Subs,
    ) -> Variable;

    /// Find the ambient function var at a given region for an ability member definition (not a
    /// specialization!), and copy that into the given subs.
    fn get_and_copy_ability_member_ambient_function(
        &self,
        ability_member: Symbol,
        region: u8,
        home_subs: &mut Subs,
    ) -> Variable;
}

pub(crate) struct SolvePhase<'a> {
    pub abilities_store: &'a AbilitiesStore,
}
impl Phase for SolvePhase<'_> {
    const IS_LATE: bool = false;

    fn with_module_abilities_store<T, F>(&self, _module: ModuleId, mut f: F) -> T
    where
        F: FnMut(&AbilitiesStore) -> T,
    {
        // During solving we're only aware of our module's abilities store.
        f(self.abilities_store)
    }

    fn copy_lambda_set_ambient_function_to_home_subs(
        &self,
        external_lambda_set_var: Variable,
        _external_module_id: ModuleId,
        home_subs: &mut Subs,
    ) -> Variable {
        // During solving we're only aware of our module's abilities store, the var must
        // be in our module store. Even if the specialization lambda set comes from another
        // module, we should have taken care to import it before starting solving in this module.
        let LambdaSet {
            ambient_function, ..
        } = home_subs.get_lambda_set(external_lambda_set_var);
        ambient_function
    }

    fn get_and_copy_ability_member_ambient_function(
        &self,
        ability_member: Symbol,
        region: u8,
        home_subs: &mut Subs,
    ) -> Variable {
        // During solving we're only aware of our module's abilities store, the var must
        // be in our module store. Even if the specialization lambda set comes from another
        // module, we should have taken care to import it before starting solving in this module.
        let member_def = self
            .abilities_store
            .member_def(ability_member)
            .unwrap_or_else(|| {
                internal_error!(
                    "{:?} is not resolved, or not an ability member!",
                    ability_member
                )
            });
        let member_var = member_def.signature_var();

        let region_lset = get_member_lambda_sets_at_region(home_subs, member_var, region);

        let LambdaSet {
            ambient_function, ..
        } = home_subs.get_lambda_set(region_lset);

        ambient_function
    }
}

#[derive(Default)]
pub struct AwaitingSpecializations {
    // What variables' specialized lambda sets in `uls_of_var` will be unlocked for specialization
    // when an implementation key's specialization is resolved?
    waiting: VecMap<ImplKey, VecSet<Variable>>,
    uls_of_var: UlsOfVar,
}

impl AwaitingSpecializations {
    pub fn remove_for_specialized(&mut self, subs: &Subs, impl_key: ImplKey) -> UlsOfVar {
        let spec_variables = self
            .waiting
            .remove(&impl_key)
            .map(|(_, set)| set)
            .unwrap_or_default();

        let mut result = UlsOfVar::default();
        for var in spec_variables {
            let target_lambda_sets = self
                .uls_of_var
                .remove_dependent_unspecialized_lambda_sets(subs, var);

            result.extend(var, target_lambda_sets);
        }
        result
    }

    pub fn add(
        &mut self,
        impl_key: ImplKey,
        var: Variable,
        lambda_sets: impl IntoIterator<Item = Variable>,
    ) {
        self.uls_of_var.extend(var, lambda_sets);
        let waiting = self.waiting.get_or_insert(impl_key, Default::default);
        waiting.insert(var);
    }

    pub fn union(&mut self, other: Self) {
        for (impl_key, waiting_vars) in other.waiting {
            let waiting = self.waiting.get_or_insert(impl_key, Default::default);
            waiting.extend(waiting_vars);
        }
        self.uls_of_var.union(other.uls_of_var);
    }

    pub fn waiting_for(&self, impl_key: ImplKey) -> bool {
        self.waiting.contains_key(&impl_key)
    }
}

pub struct CompactionResult {
    pub obligations: MustImplementConstraints,
    pub awaiting_specialization: AwaitingSpecializations,
}

#[cfg(debug_assertions)]
fn trace_compaction_step_1(subs: &Subs, c_a: Variable, uls_a: &[Variable]) {
    let c_a = roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(c_a), subs);
    let uls_a = uls_a
        .iter()
        .map(|v| {
            format!(
                "{:?}",
                roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(*v), subs)
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    eprintln!("===lambda set compaction===");
    eprintln!("  concrete type: {c_a:?}");
    eprintln!("  step 1:");
    eprintln!("    uls_a = {{ {uls_a} }}");
}

#[cfg(debug_assertions)]
fn trace_compaction_step_2(subs: &Subs, uls_a: &[Variable]) {
    let uls_a = uls_a
        .iter()
        .map(|v| {
            format!(
                "{:?}",
                roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(*v), subs)
            )
        })
        .collect::<Vec<_>>()
        .join(",");
    eprintln!("  step 2:");
    eprintln!("    uls_a' = {{ {uls_a} }}");
}

#[cfg(debug_assertions)]
fn trace_compaction_step_3start() {
    eprintln!("  step 3:");
}

#[cfg(debug_assertions)]
fn trace_compaction_step_3iter_start(
    subs: &Subs,
    iteration_lambda_set: Variable,
    t_f1: Variable,
    t_f2: Variable,
) {
    let iteration_lambda_set = roc_types::subs::SubsFmtContent(
        subs.get_content_without_compacting(iteration_lambda_set),
        subs,
    );
    let t_f1 = roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(t_f1), subs);
    let t_f2 = roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(t_f2), subs);
    eprintln!("    - iteration: {iteration_lambda_set:?}");
    eprintln!("         {t_f1:?}");
    eprintln!("      ~  {t_f2:?}");
}

#[cfg(debug_assertions)]
#[rustfmt::skip]
fn trace_compaction_step_3iter_end(subs: &Subs, t_f_result: Variable, skipped: bool) {
    let t_f_result =
        roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(t_f_result), subs);
    if skipped {
    eprintln!("      SKIP");
    }
    eprintln!("      =  {t_f_result:?}\n");
}

macro_rules! trace_compact {
    (1. $subs:expr, $c_a:expr, $uls_a:expr) => {{
        dbg_do!(ROC_TRACE_COMPACTION, {
            trace_compaction_step_1($subs, $c_a, $uls_a)
        })
    }};
    (2. $subs:expr, $uls_a:expr) => {{
        dbg_do!(ROC_TRACE_COMPACTION, {
            trace_compaction_step_2($subs, $uls_a)
        })
    }};
    (3start.) => {{
        dbg_do!(ROC_TRACE_COMPACTION, { trace_compaction_step_3start() })
    }};
    (3iter_start. $subs:expr, $iteration_lset:expr, $t_f1:expr, $t_f2:expr) => {{
        dbg_do!(ROC_TRACE_COMPACTION, {
            trace_compaction_step_3iter_start($subs, $iteration_lset, $t_f1, $t_f2)
        })
    }};
    (3iter_end. $subs:expr, $t_f_result:expr) => {{
        dbg_do!(ROC_TRACE_COMPACTION, {
            trace_compaction_step_3iter_end($subs, $t_f_result, false)
        })
    }};
    (3iter_end_skipped. $subs:expr, $t_f_result:expr) => {{
        dbg_do!(ROC_TRACE_COMPACTION, {
            trace_compaction_step_3iter_end($subs, $t_f_result, true)
        })
    }};
}

#[inline(always)]
fn iter_concrete_of_unspecialized<'a>(
    subs: &'a Subs,
    c_a: Variable,
    uls: &'a [Uls],
) -> impl Iterator<Item = &'a Uls> {
    uls.iter()
        .filter(move |Uls(var, _, _)| subs.equivalent_without_compacting(*var, c_a))
}

/// Gets the unique unspecialized lambda resolving to concrete type `c_a` in a list of
/// unspecialized lambda sets.
#[inline(always)]
fn unique_unspecialized_lambda(subs: &Subs, c_a: Variable, uls: &[Uls]) -> Option<Uls> {
    let mut iter_concrete = iter_concrete_of_unspecialized(subs, c_a, uls);
    let uls = iter_concrete.next()?;
    debug_assert!(iter_concrete.next().is_none(), "multiple concrete");
    Some(*uls)
}

#[must_use]
pub fn compact_lambda_sets_of_vars<P: Phase>(
    env: &mut SolveEnv,
    uls_of_var: UlsOfVar,
    phase: &P,
) -> CompactionResult {
    let mut must_implement = MustImplementConstraints::default();
    let mut awaiting_specialization = AwaitingSpecializations::default();

    let mut uls_of_var_queue = VecDeque::with_capacity(uls_of_var.len());
    uls_of_var_queue.extend(uls_of_var.drain());

    // Suppose a type variable `a` with `uls_of_var` mapping `uls_a = {l1, ... ln}` has been instantiated to a concrete type `C_a`.
    while let Some((c_a, uls_a)) = uls_of_var_queue.pop_front() {
        let c_a = env.subs.get_root_key_without_compacting(c_a);
        // 1. Let each `l` in `uls_a` be of form `[solved_lambdas + ... + C:f:r + ...]`.
        //    NB: There may be multiple unspecialized lambdas of form `C:f:r, C:f1:r1, ..., C:fn:rn` in `l`.
        //    In this case, let `t1, ... tm` be the other unspecialized lambdas not of form `C:_:_`,
        //    that is, none of which are now specialized to the type `C`. Then, deconstruct
        //    `l` such that `l' = [solved_lambdas + t1 + ... + tm + C:f:r]` and `l1 = [[] + C:f1:r1], ..., ln = [[] + C:fn:rn]`.
        //    Replace `l` with `l', l1, ..., ln` in `uls_a`, flattened.
        //    TODO: the flattening step described above
        let uls_a = {
            let mut uls = uls_a.into_vec();

            // De-duplicate lambdas by root key.
            uls.iter_mut().for_each(|v| *v = env.subs.get_root_key(*v));
            uls.sort();
            uls.dedup();
            uls
        };

        trace_compact!(1. env.subs, c_a, &uls_a);

        // The flattening step - remove lambda sets that don't reference the concrete var, and for
        // flatten lambda sets that reference it more than once.
        let mut uls_a: Vec<_> = uls_a
            .into_iter()
            .flat_map(|lambda_set| {
                let LambdaSet {
                    solved,
                    recursion_var,
                    unspecialized,
                    ambient_function,
                } = env.subs.get_lambda_set(lambda_set);
                let lambda_set_rank = env.subs.get_rank(lambda_set);
                let unspecialized = env.subs.get_subs_slice(unspecialized);
                // TODO: is it faster to traverse once, see if we only have one concrete lambda, and
                // bail in that happy-path, rather than always splitting?
                let (concrete, mut not_concrete): (Vec<_>, Vec<_>) = unspecialized
                    .iter()
                    .copied()
                    .partition(|Uls(var, _, _)| env.subs.equivalent_without_compacting(*var, c_a));
                if concrete.len() == 1 {
                    // No flattening needs to be done, just return the lambda set as-is
                    return vec![lambda_set];
                }
                // Must flatten
                concrete
                    .into_iter()
                    .enumerate()
                    .map(|(i, concrete_lambda)| {
                        let (var, unspecialized) = if i == 0 {
                            // The first lambda set contains one concrete lambda, plus all solved
                            // lambdas, plus all other unspecialized lambdas.
                            // l' = [solved_lambdas + t1 + ... + tm + C:f:r]
                            let unspecialized = slice_extend_new(
                                &mut env.subs.unspecialized_lambda_sets,
                                not_concrete
                                    .drain(..)
                                    .chain(std::iter::once(concrete_lambda)),
                            );
                            (lambda_set, unspecialized)
                        } else {
                            // All the other lambda sets consists only of their respective concrete
                            // lambdas.
                            // ln = [[] + C:fn:rn]
                            let unspecialized = slice_extend_new(
                                &mut env.subs.unspecialized_lambda_sets,
                                [concrete_lambda],
                            );
                            let var = env.subs.fresh(Descriptor {
                                content: Content::Error,
                                rank: lambda_set_rank,
                                mark: Mark::NONE,
                                copy: OptVariable::NONE,
                            });
                            (var, unspecialized)
                        };

                        env.subs.set_content(
                            var,
                            Content::LambdaSet(LambdaSet {
                                solved,
                                recursion_var,
                                unspecialized,
                                ambient_function,
                            }),
                        );
                        var
                    })
                    .collect()
            })
            .collect();

        // 2. Now, each `l` in `uls_a` has a unique unspecialized lambda of form `C:f:r`.
        //    Sort `uls_a` primarily by `f` (arbitrary order), and secondarily by `r` in descending order.
        uls_a.sort_by(|v1, v2| {
            let unspec_1 = env
                .subs
                .get_subs_slice(env.subs.get_lambda_set(*v1).unspecialized);
            let unspec_2 = env
                .subs
                .get_subs_slice(env.subs.get_lambda_set(*v2).unspecialized);

            let Uls(_, f1, r1) = unique_unspecialized_lambda(env.subs, c_a, unspec_1).unwrap();
            let Uls(_, f2, r2) = unique_unspecialized_lambda(env.subs, c_a, unspec_2).unwrap();

            match f1.cmp(&f2) {
                std::cmp::Ordering::Equal => {
                    // Order by descending order of region.
                    r2.cmp(&r1)
                }
                ord => ord,
            }
        });

        trace_compact!(2. env.subs, &uls_a);

        // 3. For each `l` in `uls_a` with unique unspecialized lambda `C:f:r`:
        //    1. Let `t_f1` be the directly ambient function of the lambda set containing `C:f:r`. Remove `C:f:r` from `t_f1`'s lambda set.
        //       - For example, `(b' -[[] + Fo:f:2]-> {})` if `C:f:r=Fo:f:2`. Removing `Fo:f:2`, we get `(b' -[[]]-> {})`.
        //    2. Let `t_f2` be the directly ambient function of the specialization lambda set resolved by `C:f:r`.
        //       - For example, `(b -[[] + b:g:1]-> {})` if `C:f:r=Fo:f:2`, running on example from above.
        //    3. Unify `t_f1 ~ t_f2`.
        trace_compact!(3start.);
        for l in uls_a {
            let compaction_result = compact_lambda_set(env, c_a, l, phase);

            match compaction_result {
                OneCompactionResult::Compacted {
                    new_obligations,
                    new_lambda_sets_to_specialize,
                } => {
                    must_implement.extend(new_obligations);
                    uls_of_var_queue.extend(new_lambda_sets_to_specialize.drain());
                }
                OneCompactionResult::MustWaitForSpecialization(impl_key) => {
                    awaiting_specialization.add(impl_key, c_a, [l])
                }
            }
        }
    }

    CompactionResult {
        obligations: must_implement,
        awaiting_specialization,
    }
}

enum OneCompactionResult {
    Compacted {
        new_obligations: MustImplementConstraints,
        new_lambda_sets_to_specialize: UlsOfVar,
    },
    MustWaitForSpecialization(ImplKey),
}

#[must_use]
#[allow(clippy::too_many_arguments)]
fn compact_lambda_set<P: Phase>(
    env: &mut SolveEnv,
    resolved_concrete: Variable,
    this_lambda_set: Variable,
    phase: &P,
) -> OneCompactionResult {
    // 3. For each `l` in `uls_a` with unique unspecialized lambda `C:f:r`:
    //    1. Let `t_f1` be the directly ambient function of the lambda set containing `C:f:r`. Remove `C:f:r` from `t_f1`'s lambda set.
    //       - For example, `(b' -[[] + Fo:f:2]-> {})` if `C:f:r=Fo:f:2`. Removing `Fo:f:2`, we get `(b' -[[]]-> {})`.
    //    2. Let `t_f2` be the directly ambient function of the specialization lambda set resolved by `C:f:r`.
    //       - For example, `(b -[[] + b:g:1]-> {})` if `C:f:r=Fo:f:2`, from the algorithm's running example.
    //    3. Unify `t_f1 ~ t_f2`.
    let LambdaSet {
        solved,
        recursion_var,
        unspecialized,
        ambient_function: t_f1,
    } = env.subs.get_lambda_set(this_lambda_set);
    let target_rank = env.subs.get_rank(this_lambda_set);

    debug_assert!(!unspecialized.is_empty());

    let unspecialized = env.subs.get_subs_slice(unspecialized);

    // 1. Let `t_f1` be the directly ambient function of the lambda set containing `C:f:r`.
    let Uls(c, f, r) =
        unique_unspecialized_lambda(env.subs, resolved_concrete, unspecialized).unwrap();

    debug_assert!(env.subs.equivalent_without_compacting(c, resolved_concrete));

    // Now decide: do we
    // - proceed with specialization
    // - simply drop the specialization lambda set (due to an error)
    // - or do we need to wait, because we don't know enough information for the specialization yet?
    let specialization_decision = make_specialization_decision(env.subs, phase, c, f);
    let specialization_key_or_drop = match specialization_decision {
        SpecializeDecision::Specialize(key) => Ok(key),
        SpecializeDecision::Drop => Err(()),
        SpecializeDecision::PendingSpecialization(impl_key) => {
            // Bail, we need to wait for the specialization to be known.
            return OneCompactionResult::MustWaitForSpecialization(impl_key);
        }
    };

    // 1b. Remove `C:f:r` from `t_f1`'s lambda set.
    let new_unspecialized: Vec<_> = unspecialized
        .iter()
        .filter(|Uls(v, _, _)| {
            !env.subs
                .equivalent_without_compacting(*v, resolved_concrete)
        })
        .copied()
        .collect();
    debug_assert_eq!(new_unspecialized.len(), unspecialized.len() - 1);
    let t_f1_lambda_set_without_concrete = LambdaSet {
        solved,
        recursion_var,
        unspecialized: slice_extend_new(&mut env.subs.unspecialized_lambda_sets, new_unspecialized),
        ambient_function: t_f1,
    };
    env.subs.set_content(
        this_lambda_set,
        Content::LambdaSet(t_f1_lambda_set_without_concrete),
    );

    let specialization_key = match specialization_key_or_drop {
        Ok(specialization_key) => specialization_key,
        Err(()) => {
            // Do nothing other than to remove the concrete lambda to drop from the lambda set,
            // which we already did in 1b above.
            trace_compact!(3iter_end_skipped.env.subs, t_f1);
            return OneCompactionResult::Compacted {
                new_obligations: Default::default(),
                new_lambda_sets_to_specialize: Default::default(),
            };
        }
    };

    let specialization_ambient_function_var = get_specialization_lambda_set_ambient_function(
        env.subs,
        env.derived_env,
        phase,
        f,
        r,
        specialization_key,
        target_rank,
    );

    let t_f2 = match specialization_ambient_function_var {
        Ok(lset) => lset,
        Err(()) => {
            // Do nothing other than to remove the concrete lambda to drop from the lambda set,
            // which we already did in 1b above.
            trace_compact!(3iter_end_skipped.env.subs, t_f1);
            return OneCompactionResult::Compacted {
                new_obligations: Default::default(),
                new_lambda_sets_to_specialize: Default::default(),
            };
        }
    };

    // Ensure the specialized ambient function we'll unify with is not a generalized one, but one
    // at the rank of the lambda set being compacted.
    let t_f2 = deep_copy_var_in(env, target_rank, t_f2, env.arena);

    // 3. Unify `t_f1 ~ t_f2`.
    trace_compact!(3iter_start.env.subs, this_lambda_set, t_f1, t_f2);
    let (vars, new_obligations, new_lambda_sets_to_specialize, _meta) = unify(
        &mut env.uenv(),
        t_f1,
        t_f2,
        UnificationMode::LAMBDA_SET_SPECIALIZATION,
        Polarity::Pos,
    )
    .expect_success("ambient functions don't unify");
    trace_compact!(3iter_end.env.subs, t_f1);

    env.introduce(target_rank, &vars);

    OneCompactionResult::Compacted {
        new_obligations,
        new_lambda_sets_to_specialize,
    }
}

#[derive(Debug)]
enum SpecializationTypeKey {
    Opaque(Symbol),
    Derived(DeriveKey),
    Immediate(Symbol),
    SingleLambdaSetImmediate(Symbol),
}

#[derive(Debug)]
enum SpecializeDecision {
    Specialize(SpecializationTypeKey),
    Drop,

    /// Only relevant during module solving of recursive defs - we don't yet know the
    /// specialization type for a declared ability implementation, so we must hold off on
    /// specialization.
    PendingSpecialization(ImplKey),
}

fn make_specialization_decision<P: Phase>(
    subs: &Subs,
    phase: &P,
    var: Variable,
    ability_member: Symbol,
) -> SpecializeDecision {
    use Content::*;
    use SpecializationTypeKey::*;
    match subs.get_content_without_compacting(var) {
        Alias(opaque, _, _, AliasKind::Opaque)
            if !builtin_module_with_unlisted_ability_impl(opaque.module_id()) =>
        {
            if P::IS_LATE {
                SpecializeDecision::Specialize(Opaque(*opaque))
            } else {
                // Solving within a module.
                phase.with_module_abilities_store(opaque.module_id(), |abilities_store| {
                    make_ability_specialization_decision(*opaque, ability_member, abilities_store)
                })
            }
        }
        Structure(_) | Alias(_, _, _, _) | RecursionVar { .. } => {
            let builtin = match ability_member.try_into() {
                Ok(builtin) => builtin,
                Err(_) => return SpecializeDecision::Drop,
            };

            // This is a structural type, find the derived ability function it should use.
            match roc_derive_key::Derived::builtin(builtin, subs, var) {
                Ok(derived) => match derived {
                    roc_derive_key::Derived::Immediate(imm) => {
                        SpecializeDecision::Specialize(Immediate(imm))
                    }
                    roc_derive_key::Derived::SingleLambdaSetImmediate(imm) => {
                        SpecializeDecision::Specialize(SingleLambdaSetImmediate(imm))
                    }
                    roc_derive_key::Derived::Key(derive_key) => {
                        SpecializeDecision::Specialize(Derived(derive_key))
                    }
                },
                Err(DeriveError::UnboundVar) => {
                    // not specialized yet, but that also means that it can't possibly be derivable
                    // at this point?
                    // TODO: is this right? Revisit if it causes us problems in the future.
                    SpecializeDecision::Drop
                }
                Err(DeriveError::Underivable) => {
                    // we should have reported an error for this; drop the lambda set.
                    SpecializeDecision::Drop
                }
            }
        }
        Error => SpecializeDecision::Drop,
        FlexAbleVar(_, _)
        | RigidAbleVar(..)
        | FlexVar(..)
        | RigidVar(..)
        | LambdaSet(..)
        | ErasedLambda
        | Pure
        | Effectful
        | RangedNumber(..) => {
            internal_error!("unexpected")
        }
    }
}

fn make_ability_specialization_decision(
    opaque: Symbol,
    ability_member: Symbol,
    abilities_store: &AbilitiesStore,
) -> SpecializeDecision {
    use SpecializationTypeKey::*;
    let impl_key = ImplKey {
        opaque,
        ability_member,
    };
    match abilities_store.get_implementation(impl_key) {
        None => {
            match ability_member {
                // Inspect is special - if there is no implementation for the
                // opaque type, we always emit a default implementation.
                Symbol::INSPECT_TO_INSPECTOR => {
                    SpecializeDecision::Specialize(Immediate(Symbol::INSPECT_OPAQUE))
                }
                _ => {
                    // Doesn't specialize; an error will already be reported for this.
                    SpecializeDecision::Drop
                }
            }
        }
        Some(MemberImpl::Error) => {
            // TODO: probably not right, we may want to choose a derive decision!
            SpecializeDecision::Specialize(Opaque(opaque))
        }
        Some(MemberImpl::Impl(specialization_symbol)) => {
            match abilities_store.specialization_info(*specialization_symbol) {
                Some(_) => SpecializeDecision::Specialize(Opaque(opaque)),

                // If we expect a specialization impl but don't yet know it, we must hold off
                // compacting the lambda set until the specialization is well-known.
                None => SpecializeDecision::PendingSpecialization(impl_key),
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn get_specialization_lambda_set_ambient_function<P: Phase>(
    subs: &mut Subs,
    derived_env: &DerivedEnv,
    phase: &P,
    ability_member: Symbol,
    lset_region: u8,
    mut specialization_key: SpecializationTypeKey,
    target_rank: Rank,
) -> Result<Variable, ()> {
    loop {
        match specialization_key {
            SpecializationTypeKey::Opaque(opaque) => {
                let opaque_home = opaque.module_id();
                let found = phase.with_module_abilities_store(opaque_home, |abilities_store| {
                    find_opaque_specialization_ambient_function(
                        abilities_store,
                        opaque,
                        ability_member,
                        lset_region,
                    )
                });

                let external_specialized_lset = match found {
                    FoundOpaqueSpecialization::UpdatedSpecializationKey(key) => {
                        specialization_key = key;
                        continue;
                    }
                    FoundOpaqueSpecialization::AmbientFunction(lset) => lset,
                    FoundOpaqueSpecialization::NotFound => {
                        if P::IS_LATE {
                            internal_error!(
                                "expected to know a specialization for {:?}#{:?}, but it wasn't found",
                                opaque,
                                ability_member
                            );
                        } else {
                            // We'll have reported an error for this.
                            return Err(());
                        }
                    }
                };

                let specialized_ambient = phase.copy_lambda_set_ambient_function_to_home_subs(
                    external_specialized_lset,
                    opaque_home,
                    subs,
                );

                return Ok(specialized_ambient);
            }

            SpecializationTypeKey::Derived(derive_key) => {
                let mut derived_module = derived_env.derived_module.lock().unwrap();

                let (_, _, specialization_lambda_sets) =
                    derived_module.get_or_insert(derived_env.exposed_types, derive_key);

                let specialized_lambda_set = *specialization_lambda_sets
                    .get(&lset_region)
                    .expect("lambda set region not resolved");

                let specialized_ambient = derived_module.copy_lambda_set_ambient_function_to_subs(
                    specialized_lambda_set,
                    subs,
                    target_rank,
                );

                return Ok(specialized_ambient);
            }

            SpecializationTypeKey::Immediate(imm) => {
                // Immediates are like opaques in that we can simply look up their type definition in
                // the ability store, there is nothing new to synthesize.
                //
                // THEORY: if something can become an immediate, it will always be available in the
                // local ability store, because the transformation is local (?)
                //
                // TODO: I actually think we can get what we need here by examining `derived_env.exposed_types`,
                // since immediates can only refer to builtins - and in userspace, all builtin types
                // are available in `exposed_types`.
                let immediate_lambda_set_at_region =
                    phase.get_and_copy_ability_member_ambient_function(imm, lset_region, subs);

                return Ok(immediate_lambda_set_at_region);
            }

            SpecializationTypeKey::SingleLambdaSetImmediate(imm) => {
                let module_id = imm.module_id();
                debug_assert!(module_id.is_builtin());

                let module_types = &derived_env
                    .exposed_types
                    .get(&module_id)
                    .unwrap()
                    .exposed_types_storage_subs;

                // Since this immediate has only one lambda set, the region must be pointing to 1, and
                // moreover the imported function type is the ambient function of the single lset.
                debug_assert_eq!(lset_region, 1);
                let storage_var = module_types.stored_vars_by_symbol.get(&imm).unwrap();
                let imported = module_types
                    .storage_subs
                    .export_variable_to(subs, *storage_var);

                roc_types::subs::instantiate_rigids(subs, imported.variable);

                return Ok(imported.variable);
            }
        }
    }
}

enum FoundOpaqueSpecialization {
    UpdatedSpecializationKey(SpecializationTypeKey),
    AmbientFunction(Variable),
    NotFound,
}

fn find_opaque_specialization_ambient_function(
    abilities_store: &AbilitiesStore,
    opaque: Symbol,
    ability_member: Symbol,
    lset_region: u8,
) -> FoundOpaqueSpecialization {
    let impl_key = roc_can::abilities::ImplKey {
        opaque,
        ability_member,
    };

    let opt_specialization = abilities_store.get_implementation(impl_key);
    match opt_specialization {
        None => match ability_member {
            Symbol::INSPECT_TO_INSPECTOR => FoundOpaqueSpecialization::UpdatedSpecializationKey(
                SpecializationTypeKey::Immediate(Symbol::INSPECT_OPAQUE),
            ),
            _ => FoundOpaqueSpecialization::NotFound,
        },
        Some(member_impl) => match member_impl {
            MemberImpl::Impl(spec_symbol) => {
                let specialization =
                            abilities_store.specialization_info(*spec_symbol).expect("expected custom implementations to always have complete specialization info by this point");

                let specialized_lambda_set = *specialization
                    .specialization_lambda_sets
                    .get(&lset_region)
                    .unwrap_or_else(|| {
                        panic!(
                            "lambda set region not resolved: {:?}",
                            (spec_symbol, specialization)
                        )
                    });

                FoundOpaqueSpecialization::AmbientFunction(specialized_lambda_set)
            }
            MemberImpl::Error => todo_abilities!(),
        },
    }
}

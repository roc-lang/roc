use roc_collections::soa::slice_extend_new;
use roc_collections::VecMap;
use roc_debug_flags::{dbg_do, dbg_set};
#[cfg(debug_assertions)]
use roc_debug_flags::{
    ROC_PRINT_MISMATCHES, ROC_PRINT_UNIFICATIONS, ROC_VERIFY_OCCURS_ONE_RECURSION,
};
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_solve_schema::UnificationMode;
use roc_types::num::{FloatWidth, IntLitWidth, NumericRange};
use roc_types::subs::Content::{self, *};
use roc_types::subs::{
    AliasVariables, Descriptor, ErrorTypeContext, FlatType, GetSubsSlice, LambdaSet, Mark,
    OptVariable, RecordFields, Subs, SubsIndex, SubsSlice, TagExt, TupleElems, UlsOfVar,
    UnionLabels, UnionLambdas, UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::{
    AliasKind, DoesNotImplementAbility, ErrorType, Mismatch, Polarity, RecordField, Uls,
};

use crate::env::Env;

macro_rules! mismatch {
    () => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch],
            ..Outcome::default()
        }
    }};
    ($msg:expr) => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
            eprintln!($msg);
            eprintln!("");
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch],
            ..Outcome::default()
        }
    }};
    ($msg:expr,) => {{
        mismatch!($msg)
    }};
    ($msg:expr, $($arg:tt)*) => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
            eprintln!($msg, $($arg)*);
            eprintln!("");
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch],
            ..Outcome::default()
        }
    }};
    (%not_able, $var:expr, $abilities:expr, $msg:expr, $($arg:tt)*) => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
            eprintln!($msg, $($arg)*);
            eprintln!("");
        });

        let mut mismatches = Vec::with_capacity(1 + $abilities.len());
        mismatches.push(Mismatch::TypeMismatch);
        for ability in $abilities {
            mismatches.push(Mismatch::DoesNotImplementAbiity($var, *ability));
        }

        Outcome {
            mismatches,
            ..Outcome::default()
        }
    }}
}

type Pool = Vec<Variable>;

#[derive(Debug)]
pub struct Context {
    first: Variable,
    first_desc: Descriptor,
    second: Variable,
    second_desc: Descriptor,
    mode: UnificationMode,
}

pub trait MetaCollector: Default + std::fmt::Debug {
    /// Whether we are performing `member ~ specialization` where `member` is an ability member
    /// signature and `specialization` is an ability specialization for a given type. When this is
    /// the case, given a lambda set unification like
    /// `[[] + a:member:1] ~ [specialization-lambda-set]`, only the specialization lambda set will
    /// be kept around, and the record `(member, 1) => specialization-lambda-set` will be
    /// associated via [`Self::record_specialization_lambda_set`].
    const UNIFYING_SPECIALIZATION: bool;
    const IS_LATE: bool;

    fn record_specialization_lambda_set(&mut self, member: Symbol, region: u8, var: Variable);

    fn record_changed_variable(&mut self, subs: &Subs, var: Variable);

    fn union(&mut self, other: Self);
}

#[derive(Default, Debug)]
pub struct NoCollector;
impl MetaCollector for NoCollector {
    const UNIFYING_SPECIALIZATION: bool = false;
    const IS_LATE: bool = false;

    #[inline(always)]
    fn record_specialization_lambda_set(&mut self, _member: Symbol, _region: u8, _var: Variable) {}

    #[inline(always)]
    fn record_changed_variable(&mut self, _subs: &Subs, _var: Variable) {}

    #[inline(always)]
    fn union(&mut self, _other: Self) {}
}

#[derive(Default, Debug)]
pub struct SpecializationLsetCollector(pub VecMap<(Symbol, u8), Variable>);

impl MetaCollector for SpecializationLsetCollector {
    const UNIFYING_SPECIALIZATION: bool = true;
    const IS_LATE: bool = false;

    #[inline(always)]
    fn record_specialization_lambda_set(&mut self, member: Symbol, region: u8, var: Variable) {
        self.0.insert((member, region), var);
    }

    #[inline(always)]
    fn record_changed_variable(&mut self, _subs: &Subs, _var: Variable) {}

    #[inline(always)]
    fn union(&mut self, other: Self) {
        for (k, v) in other.0.into_iter() {
            let _old = self.0.insert(k, v);
            debug_assert!(_old.is_none(), "overwriting known lambda set");
        }
    }
}

#[derive(Debug)]
pub enum Unified<M: MetaCollector = NoCollector> {
    Success {
        vars: Pool,
        must_implement_ability: MustImplementConstraints,
        lambda_sets_to_specialize: UlsOfVar,

        /// The vast majority of the time the extra metadata is empty, so we make unification
        /// polymorphic over metadata collection to avoid unnecessary memory usage.
        extra_metadata: M,
    },
    Failure(Pool, ErrorType, ErrorType, DoesNotImplementAbility),
}

impl<M: MetaCollector> Unified<M> {
    pub fn expect_success(
        self,
        err_msg: &'static str,
    ) -> (Pool, MustImplementConstraints, UlsOfVar, M) {
        match self {
            Unified::Success {
                vars,
                must_implement_ability,
                lambda_sets_to_specialize,
                extra_metadata,
            } => (
                vars,
                must_implement_ability,
                lambda_sets_to_specialize,
                extra_metadata,
            ),
            _ => internal_error!("{}", err_msg),
        }
    }
}

/// Type obligated to implement an ability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Obligated {
    /// Opaque types can either define custom implementations for an ability, or ask the compiler
    /// to generate an implementation of a builtin ability for them. In any case they have unique
    /// obligation rules for abilities.
    Opaque(Symbol),
    /// A structural type for which the compiler can at most generate an adhoc implementation of
    /// a builtin ability.
    Adhoc(Variable),
}

/// Specifies that `type` must implement the ability `ability`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MustImplementAbility {
    pub typ: Obligated,
    pub ability: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MustImplementConstraints(Vec<MustImplementAbility>);

impl MustImplementConstraints {
    pub fn push(&mut self, must_implement: MustImplementAbility) {
        self.0.push(must_implement)
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get_unique(mut self) -> Vec<MustImplementAbility> {
        self.0.sort();
        self.0.dedup();
        self.0
    }

    pub fn iter_for_ability(&self, ability: Symbol) -> impl Iterator<Item = &MustImplementAbility> {
        self.0.iter().filter(move |mia| mia.ability == ability)
    }
}

#[derive(Debug, Default)]
pub struct Outcome<M: MetaCollector> {
    mismatches: Vec<Mismatch>,
    /// We defer these checks until the end of a solving phase.
    /// NOTE: this vector is almost always empty!
    must_implement_ability: MustImplementConstraints,
    /// We defer resolution of these lambda sets to the caller of [unify].
    /// See also [merge_flex_able_with_concrete].
    lambda_sets_to_specialize: UlsOfVar,
    /// Whether any variable in the path has changed (been merged with new content).
    has_changed: bool,
    extra_metadata: M,
}

impl<M: MetaCollector> Outcome<M> {
    fn union(&mut self, other: Self) {
        let Self {
            mismatches,
            must_implement_ability,
            lambda_sets_to_specialize,
            has_changed,
            extra_metadata,
        } = other;

        self.mismatches.extend(mismatches);
        self.must_implement_ability.extend(must_implement_ability);
        self.lambda_sets_to_specialize
            .union(lambda_sets_to_specialize);
        self.has_changed = self.has_changed || has_changed;
        self.extra_metadata.union(extra_metadata);
    }
}

/// Unifies two types.
/// The [mode][UnificationMode] enables or disables certain extensional features of unification.
///
/// `observed_pol` describes the [polarity][Polarity] of the type observed to be under unification.
/// This is only relevant for producing error types, and is not material to the unification
/// algorithm.
#[inline(always)]
pub fn unify(
    env: &mut Env,
    var1: Variable,
    var2: Variable,
    mode: UnificationMode,
    observed_pol: Polarity,
) -> Unified {
    unify_help(env, var1, var2, mode, observed_pol)
}

#[inline(always)]
#[must_use]
pub fn unify_introduced_ability_specialization(
    env: &mut Env,
    ability_member_signature: Variable,
    specialization_var: Variable,
    mode: UnificationMode,
) -> Unified<SpecializationLsetCollector> {
    unify_help(
        env,
        ability_member_signature,
        specialization_var,
        mode,
        Polarity::OF_VALUE,
    )
}

#[inline(always)]
#[must_use]
pub fn unify_with_collector<M: MetaCollector>(
    env: &mut Env,
    var1: Variable,
    var2: Variable,
    mode: UnificationMode,
    observed_pol: Polarity,
) -> Unified<M> {
    unify_help(env, var1, var2, mode, observed_pol)
}

#[inline(always)]
#[must_use]
fn unify_help<M: MetaCollector>(
    env: &mut Env,
    var1: Variable,
    var2: Variable,
    mode: UnificationMode,
    observed_pol: Polarity,
) -> Unified<M> {
    let mut vars = Vec::new();
    let Outcome {
        mismatches,
        must_implement_ability,
        lambda_sets_to_specialize,
        extra_metadata,
        has_changed: _,
    } = unify_pool(env, &mut vars, var1, var2, mode);

    if mismatches.is_empty() {
        Unified::Success {
            vars,
            must_implement_ability,
            lambda_sets_to_specialize,
            extra_metadata,
        }
    } else {
        let error_context = if mismatches.contains(&Mismatch::TypeNotInRange) {
            ErrorTypeContext::EXPAND_RANGES
        } else {
            ErrorTypeContext::empty()
        };

        let type1 = env.var_to_error_type_contextual(var1, error_context, observed_pol);
        let type2 = env.var_to_error_type_contextual(var2, error_context, observed_pol);

        env.union(var1, var2, Content::Error.into());

        let do_not_implement_ability = mismatches
            .into_iter()
            .filter_map(|mismatch| match mismatch {
                Mismatch::DoesNotImplementAbiity(var, ab) => {
                    let err_type =
                        env.var_to_error_type_contextual(var, error_context, observed_pol);
                    Some((err_type, ab))
                }
                _ => None,
            })
            .collect();

        Unified::Failure(vars, type1, type2, do_not_implement_ability)
    }
}

#[inline(always)]
#[must_use]
pub fn unify_pool<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    var1: Variable,
    var2: Variable,
    mode: UnificationMode,
) -> Outcome<M> {
    if env.equivalent(var1, var2) {
        Outcome::default()
    } else {
        let ctx = Context {
            first: var1,
            first_desc: env.get(var1),
            second: var2,
            second_desc: env.get(var2),
            mode,
        };

        unify_context(env, pool, ctx)
    }
}

/// Set `ROC_PRINT_UNIFICATIONS` in debug runs to print unifications as they start and complete as
/// a tree to stderr.
/// NOTE: Only run this on individual tests! Run on multiple threads, this would clobber each others' output.
#[cfg(debug_assertions)]
fn debug_print_unified_types<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    opt_outcome: Option<&Outcome<M>>,
) {
    use roc_types::subs::SubsFmtContent;

    static mut UNIFICATION_DEPTH: usize = 0;

    match opt_outcome {
        None => env.debug_start_unification(ctx.first, ctx.second, ctx.mode),
        Some(outcome) => {
            let success = outcome.mismatches.is_empty();
            env.debug_end_unification(ctx.first, ctx.second, success);
        }
    }

    dbg_do!(ROC_PRINT_UNIFICATIONS, {
        let prefix = match opt_outcome {
            None => "❔",
            Some(outcome) if outcome.mismatches.is_empty() => "✅",
            Some(_) => "❌",
        };

        let depth = unsafe { UNIFICATION_DEPTH };
        let indent = 2;
        let (use_depth, new_depth) = if opt_outcome.is_none() {
            (depth, depth + indent)
        } else {
            (depth - indent, depth - indent)
        };

        // NOTE: names are generated here (when creating an error type) and that modifies names
        // generated by pretty_print.rs. So many test will fail with changes in variable names when
        // this block runs.
        //        let (type1, _problems1) = subs.var_to_error_type(ctx.first);
        //        let (type2, _problems2) = subs.var_to_error_type(ctx.second);
        //        println!("\n --------------- \n");
        //        eprintln!("{:?}, {:?}", ctx.first, type1);
        //        println!("\n --- \n");
        //        eprintln!({:?}, {:?}", ctx.second, type2);
        //        println!("\n --------------- \n");
        let content_1 = env.get(ctx.first).content;
        let content_2 = env.get(ctx.second).content;
        let mode = ctx.mode.pretty_print();
        eprintln!(
            "{}{}({:?}-{:?}): {:?} {:?} {} {:?} {:?}",
            " ".repeat(use_depth),
            prefix,
            env.get_root_key_without_compacting(ctx.first),
            env.get_root_key_without_compacting(ctx.second),
            ctx.first,
            SubsFmtContent(&content_1, env),
            mode,
            ctx.second,
            SubsFmtContent(&content_2, env),
        );

        unsafe { UNIFICATION_DEPTH = new_depth };
    })
}

#[must_use]
fn unify_context<M: MetaCollector>(env: &mut Env, pool: &mut Pool, ctx: Context) -> Outcome<M> {
    #[cfg(debug_assertions)]
    debug_print_unified_types::<M>(env, &ctx, None);

    // This #[allow] is needed in release builds, where `result` is no longer used.
    #[allow(clippy::let_and_return)]
    let mut result: Outcome<M> = match &ctx.first_desc.content {
        FlexVar(opt_name) => unify_flex(env, &ctx, opt_name, &ctx.second_desc.content),
        FlexAbleVar(opt_name, abilities) => {
            unify_flex_able(env, &ctx, opt_name, *abilities, &ctx.second_desc.content)
        }
        RecursionVar {
            opt_name,
            structure,
        } => unify_recursion(
            env,
            pool,
            &ctx,
            opt_name,
            *structure,
            &ctx.second_desc.content,
        ),
        RigidVar(name) => unify_rigid(env, &ctx, name, &ctx.second_desc.content),
        RigidAbleVar(name, abilities) => {
            unify_rigid_able(env, &ctx, name, *abilities, &ctx.second_desc.content)
        }
        Structure(flat_type) => {
            unify_structure(env, pool, &ctx, flat_type, &ctx.second_desc.content)
        }
        Alias(symbol, args, real_var, AliasKind::Structural) => {
            unify_alias(env, pool, &ctx, *symbol, *args, *real_var)
        }
        Alias(symbol, args, real_var, AliasKind::Opaque) => {
            unify_opaque(env, pool, &ctx, *symbol, *args, *real_var)
        }
        LambdaSet(lset) => unify_lambda_set(env, pool, &ctx, *lset, &ctx.second_desc.content),
        ErasedLambda => unify_erased_lambda(env, pool, &ctx, &ctx.second_desc.content),
        Pure => unify_pure(env, &ctx, &ctx.second_desc.content),
        Effectful => unify_effectful(env, &ctx, &ctx.second_desc.content),
        &RangedNumber(range_vars) => unify_ranged_number(env, pool, &ctx, range_vars),
        Error => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            merge(env, &ctx, Error)
        }
    };

    if result.has_changed {
        result
            .extra_metadata
            .record_changed_variable(env, ctx.first);
        result
            .extra_metadata
            .record_changed_variable(env, ctx.second);
    }

    #[cfg(debug_assertions)]
    debug_print_unified_types(env, &ctx, Some(&result));

    result
}

fn not_in_range_mismatch<M: MetaCollector>() -> Outcome<M> {
    Outcome {
        mismatches: vec![Mismatch::TypeNotInRange],
        must_implement_ability: Default::default(),
        lambda_sets_to_specialize: Default::default(),
        has_changed: false,
        extra_metadata: Default::default(),
    }
}

#[inline(always)]
#[must_use]
fn unify_ranged_number<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    range_vars: NumericRange,
) -> Outcome<M> {
    let other_content = &ctx.second_desc.content;

    match other_content {
        FlexVar(_) => {
            // Ranged number wins
            merge(env, ctx, RangedNumber(range_vars))
        }
        RigidVar(name) => {
            // Int a vs Int <range>, the rigid wins
            merge(env, ctx, RigidVar(*name))
        }
        FlexAbleVar(_, abilities) => {
            // Range wins, modulo obligation checking.
            merge_flex_able_with_concrete(
                env,
                ctx,
                ctx.second,
                *abilities,
                RangedNumber(range_vars),
                Obligated::Adhoc(ctx.first),
            )
        }
        RecursionVar { .. } | Alias(..) | Structure(..) | RigidAbleVar(..) => {
            check_and_merge_valid_range(env, pool, ctx, ctx.first, range_vars, ctx.second)
        }
        &RangedNumber(other_range_vars) => match range_vars.intersection(&other_range_vars) {
            Some(range) => merge(env, ctx, RangedNumber(range)),
            None => not_in_range_mismatch(),
        },
        LambdaSet(..) | ErasedLambda => mismatch!(),
        Pure | Effectful => mismatch!("Cannot unify RangedNumber with fx var"),
        Error => merge(env, ctx, Error),
    }
}

fn check_and_merge_valid_range<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    range_var: Variable,
    range: NumericRange,
    var: Variable,
) -> Outcome<M> {
    use Content::*;
    let content = *env.get_content_without_compacting(var);

    macro_rules! merge_if {
        ($cond:expr) => {
            if $cond {
                merge(env, ctx, content)
            } else {
                not_in_range_mismatch()
            }
        };
    }

    match content {
        RangedNumber(other_range) => match range.intersection(&other_range) {
            Some(r) => {
                if r == range {
                    merge(env, ctx, RangedNumber(range))
                } else {
                    merge(env, ctx, RangedNumber(other_range))
                }
            }
            None => not_in_range_mismatch(),
        },
        Alias(symbol, args, _real_var, kind) => match symbol {
            Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => {
                merge_if!(range.contains_int_width(IntLitWidth::I8))
            }
            Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => {
                merge_if!(range.contains_int_width(IntLitWidth::U8))
            }
            Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => {
                merge_if!(range.contains_int_width(IntLitWidth::I16))
            }
            Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => {
                merge_if!(range.contains_int_width(IntLitWidth::U16))
            }
            Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => {
                merge_if!(range.contains_int_width(IntLitWidth::I32))
            }
            Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => {
                merge_if!(range.contains_int_width(IntLitWidth::U32))
            }
            Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => {
                merge_if!(range.contains_int_width(IntLitWidth::I64))
            }
            Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => {
                merge_if!(range.contains_int_width(IntLitWidth::U64))
            }
            Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => {
                merge_if!(range.contains_int_width(IntLitWidth::I128))
            }
            Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => {
                merge_if!(range.contains_int_width(IntLitWidth::U128))
            }

            Symbol::NUM_DEC | Symbol::NUM_DECIMAL => {
                merge_if!(range.contains_float_width(FloatWidth::Dec))
            }
            Symbol::NUM_F32 | Symbol::NUM_BINARY32 => {
                merge_if!(range.contains_float_width(FloatWidth::F32))
            }
            Symbol::NUM_F64 | Symbol::NUM_BINARY64 => {
                merge_if!(range.contains_float_width(FloatWidth::F64))
            }
            Symbol::NUM_FRAC | Symbol::NUM_FLOATINGPOINT => match range {
                NumericRange::IntAtLeastSigned(_) | NumericRange::IntAtLeastEitherSign(_) => {
                    mismatch!()
                }
                NumericRange::NumAtLeastSigned(_) | NumericRange::NumAtLeastEitherSign(_) => {
                    debug_assert_eq!(args.len(), 1);
                    let arg = env.get_subs_slice(args.all_variables())[0];
                    let new_range_var = wrap_range_var(env, symbol, range_var, kind);
                    unify_pool(env, pool, new_range_var, arg, ctx.mode)
                }
            },
            Symbol::NUM_NUM => {
                debug_assert_eq!(args.len(), 1);
                let arg = env.get_subs_slice(args.all_variables())[0];
                let new_range_var = wrap_range_var(env, symbol, range_var, kind);
                unify_pool(env, pool, new_range_var, arg, ctx.mode)
            }
            Symbol::NUM_INT | Symbol::NUM_INTEGER => {
                debug_assert_eq!(args.len(), 1);
                let arg = env.get_subs_slice(args.all_variables())[0];
                let new_range_var = wrap_range_var(env, symbol, range_var, kind);
                unify_pool(env, pool, new_range_var, arg, ctx.mode)
            }

            _ => mismatch!(),
        },

        _ => mismatch!(),
    }
}

/// Push a number range var down into a number type, so as to preserve type hierarchy structure.
/// For example when we have Num (Int a) ~ Num (NumericRange <U128>), we want to produce
///   Num (Int (NumericRange <U128>))
/// on the right (which this function does) and then unify
///   Num (Int a) ~ Num (Int (NumericRange <U128>))
fn wrap_range_var(
    env: &mut Env,
    symbol: Symbol,
    range_var: Variable,
    alias_kind: AliasKind,
) -> Variable {
    let range_desc = env.get(range_var);
    let new_range_var = env.fresh(range_desc);
    let var_slice = AliasVariables::insert_into_subs(env, [new_range_var], [], []);
    env.set_content(
        range_var,
        Alias(symbol, var_slice, new_range_var, alias_kind),
    );
    new_range_var
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
#[must_use]
fn unify_two_aliases<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    kind: AliasKind,
    symbol: Symbol,
    args: AliasVariables,
    real_var: Variable,
    other_args: AliasVariables,
    other_real_var: Variable,
) -> Outcome<M> {
    if args.len() == other_args.len() {
        let mut outcome = Outcome::default();

        let args_it = args
            .type_variables()
            .into_iter()
            .zip(other_args.type_variables());

        let lambda_set_it = args
            .lambda_set_variables()
            .into_iter()
            .zip(other_args.lambda_set_variables());

        let infer_ext_in_output_vars_it = (args.infer_ext_in_output_variables().into_iter())
            .zip(other_args.infer_ext_in_output_variables());

        let mut merged_args = Vec::with_capacity(args.type_variables().len());
        let mut merged_lambda_set_args = Vec::with_capacity(args.lambda_set_variables().len());
        let mut merged_infer_ext_in_output_vars =
            Vec::with_capacity(args.infer_ext_in_output_variables().len());
        debug_assert_eq!(
            merged_args.capacity()
                + merged_lambda_set_args.capacity()
                + merged_infer_ext_in_output_vars.capacity(),
            args.all_variables_len as usize,
        );

        for (l, r) in args_it {
            let l_var = env[l];
            let r_var = env[r];
            outcome.union(unify_pool(env, pool, l_var, r_var, ctx.mode));

            let merged_var = choose_merged_var(env, l_var, r_var);
            merged_args.push(merged_var);
        }

        if !outcome.mismatches.is_empty() {
            return outcome;
        }

        // Even if there are no changes to alias arguments, and no new variables were
        // introduced, we may still need to unify the "actual types" of the alias or opaque!
        //
        // The unification is not necessary from a types perspective (and in fact, we may want
        // to disable it for `roc check` later on), but it is necessary for the monomorphizer,
        // which expects identical types to be reflected in the same variable.
        //
        // As a concrete example, consider the unification of two opaques
        //
        //   P := [Zero, Succ P]
        //
        //   (@P (Succ n)) ~ (@P (Succ o))
        //
        // `P` has no arguments, and unification of the surface of `P` introduces nothing new.
        // But if we do not unify the types of `n` and `o`, which are recursion variables, they
        // will remain disjoint! Currently, the implication of this is that they will be seen
        // to have separate recursive memory layouts in the monomorphizer - which is no good
        // for our compilation model.
        //
        // As such, always unify the real vars.

        // Don't report real_var mismatches, because they must always be surfaced higher, from
        // the argument types.
        let mut real_var_outcome = unify_pool::<M>(env, pool, real_var, other_real_var, ctx.mode);
        let _ = real_var_outcome.mismatches.drain(..);
        outcome.union(real_var_outcome);

        let merged_real_var = choose_merged_var(env, real_var, other_real_var);

        // Now, we need to unify the lambda set and IOIOP variables of both aliases.
        //
        // We wait to do this until after we have unified the real vars, because the real vars
        // should contain the lambda set and IOIOP variables of the aliases, so most of these
        // should be short-circuits.
        {
            for (l, r) in lambda_set_it {
                let l_var = env[l];
                let r_var = env[r];
                outcome.union(unify_pool(env, pool, l_var, r_var, ctx.mode));

                let merged_var = choose_merged_var(env, l_var, r_var);
                merged_lambda_set_args.push(merged_var);
            }

            for (l, r) in infer_ext_in_output_vars_it {
                let l_var = env[l];
                let r_var = env[r];
                outcome.union(unify_pool(env, pool, l_var, r_var, ctx.mode));

                let merged_var = choose_merged_var(env, l_var, r_var);
                merged_infer_ext_in_output_vars.push(merged_var);
            }
        }

        // POSSIBLE OPT: choose_merged_var chooses the left when the choice is arbitrary. If
        // the merged vars are all left, avoid re-insertion. Is checking for argument slice
        // equality faster than re-inserting?
        let merged_variables = AliasVariables::insert_into_subs(
            env,
            merged_args,
            merged_lambda_set_args,
            merged_infer_ext_in_output_vars,
        );

        let merged_content = Content::Alias(symbol, merged_variables, merged_real_var, kind);

        outcome.union(merge(env, ctx, merged_content));

        outcome
    } else {
        mismatch!("{:?}", symbol)
    }
}

fn fix_fixpoint<M: MetaCollector>(env: &mut Env, ctx: &Context) -> Outcome<M> {
    let fixed_variables = crate::fix::fix_fixpoint(env, ctx.first, ctx.second);
    env.extend_fixed_variables(fixed_variables);
    Default::default()
}

// Unifies a structural alias
#[inline(always)]
#[must_use]
fn unify_alias<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    symbol: Symbol,
    args: AliasVariables,
    real_var: Variable,
) -> Outcome<M> {
    let other_content = &ctx.second_desc.content;

    let kind = AliasKind::Structural;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(env, ctx, Alias(symbol, args, real_var, kind))
        }
        RecursionVar { structure, .. } => {
            if env.seen_recursion_pair(ctx.first, ctx.second) {
                return fix_fixpoint(env, ctx);
            }

            env.add_recursion_pair(ctx.first, ctx.second);
            let outcome = unify_pool(env, pool, real_var, *structure, ctx.mode);
            env.remove_recursion_pair(ctx.first, ctx.second);

            outcome
        }
        RigidVar(_) | RigidAbleVar(..) | FlexAbleVar(..) => {
            unify_pool(env, pool, real_var, ctx.second, ctx.mode)
        }
        Alias(_, _, _, AliasKind::Opaque) => unify_pool(env, pool, real_var, ctx.second, ctx.mode),
        Alias(other_symbol, other_args, other_real_var, AliasKind::Structural) => {
            if symbol == *other_symbol {
                unify_two_aliases(
                    env,
                    pool,
                    ctx,
                    AliasKind::Structural,
                    symbol,
                    args,
                    real_var,
                    *other_args,
                    *other_real_var,
                )
            } else {
                unify_pool(env, pool, real_var, *other_real_var, ctx.mode)
            }
        }
        Structure(_) => unify_pool(env, pool, real_var, ctx.second, ctx.mode),
        RangedNumber(other_range_vars) => {
            check_and_merge_valid_range(env, pool, ctx, ctx.second, *other_range_vars, ctx.first)
        }
        LambdaSet(..) => mismatch!("cannot unify alias {:?} with lambda set {:?}: lambda sets should never be directly behind an alias!", ctx.first, other_content),
        ErasedLambda => mismatch!("cannot unify alias {:?} with an erased lambda!", ctx.first),
        Pure|Effectful => mismatch!("cannot unify alias {:?} with an fx var!", ctx.first),
        Error => merge(env, ctx, Error),
    }
}

#[inline(always)]
fn opaque_obligation(opaque: Symbol, opaque_var: Variable) -> Obligated {
    match opaque.module_id() {
        // Numbers should be treated as ad-hoc obligations for ability checking.
        ModuleId::NUM => Obligated::Adhoc(opaque_var),
        _ => Obligated::Opaque(opaque),
    }
}

#[inline(always)]
#[must_use]
fn unify_opaque<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    symbol: Symbol,
    args: AliasVariables,
    real_var: Variable,
) -> Outcome<M> {
    let other_content = &ctx.second_desc.content;

    let kind = AliasKind::Opaque;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(env, ctx, Alias(symbol, args, real_var, kind))
        }
        FlexAbleVar(_, abilities) => {
            // Opaque type wins
            merge_flex_able_with_concrete(
                env,
                ctx,
                ctx.second,
                *abilities,
                Alias(symbol, args, real_var, kind),
                opaque_obligation(symbol, ctx.first),
            )
        }
        Alias(_, _, other_real_var, AliasKind::Structural) => {
            unify_pool(env, pool, ctx.first, *other_real_var, ctx.mode)
        }
        RecursionVar { structure, .. } => {
            if env.seen_recursion_pair(ctx.first, ctx.second) {
                return fix_fixpoint(env, ctx);
            }

            env.add_recursion_pair(ctx.first, ctx.second);
            let outcome = unify_pool(env, pool, real_var, *structure, ctx.mode);
            env.remove_recursion_pair(ctx.first, ctx.second);

            outcome
        }
        Alias(other_symbol, other_args, other_real_var, AliasKind::Opaque) => {
            // Opaques types are only equal if the opaque symbols are equal!
            if symbol == *other_symbol {
                unify_two_aliases(
                    env,
                    pool,
                    ctx,
                    AliasKind::Opaque,
                    symbol,
                    args,
                    real_var,
                    *other_args,
                    *other_real_var,
                )
            } else {
                mismatch!("{:?}", symbol)
            }
        }
        RangedNumber(other_range_vars) => {
            // This opaque might be a number, check if it unifies with the target ranged number var.
            check_and_merge_valid_range(env, pool, ctx, ctx.second, *other_range_vars, ctx.first)
        }
        Error => merge(env, ctx, Error),
        // _other has an underscore because it's unused in --release builds
        _other => {
            // The type on the left is an opaque, but the one on the right is not!
            mismatch!("Cannot unify opaque {:?} with {:?}", symbol, _other)
        }
    }
}

#[inline(always)]
#[must_use]
fn unify_structure<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    flat_type: &FlatType,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            merge(env, ctx, Structure(*flat_type))
        }
        FlexAbleVar(_, abilities) => {
            // Structure wins
            merge_flex_able_with_concrete(
                env,
                ctx,
                ctx.second,
                *abilities,
                Structure(*flat_type),
                Obligated::Adhoc(ctx.first),
            )
        }
        // _name has an underscore because it's unused in --release builds
        RigidVar(_name) => {
            // Type mismatch! Rigid can only unify with flex.
            mismatch!(
                "trying to unify {:?} with rigid var {:?}",
                &flat_type,
                _name
            )
        }
        RigidAbleVar(_, _abilities) => {
            mismatch!(
                %not_able, ctx.first, env.get_subs_slice(*_abilities),
                "trying to unify {:?} with RigidAble {:?}",
                &flat_type,
                &other
            )
        }
        RecursionVar { structure, .. } => {
            if env.seen_recursion_pair(ctx.first, ctx.second) {
                return fix_fixpoint(env, ctx);
            }

            env.add_recursion_pair(ctx.first, ctx.second);

            let outcome = match flat_type {
                FlatType::TagUnion(_, _) => {
                    // unify the structure with this unrecursive tag union
                    unify_pool(env, pool, ctx.first, *structure, ctx.mode)
                }
                FlatType::RecursiveTagUnion(rec, _, _) => {
                    debug_assert!(
                        is_recursion_var(env, *rec),
                        "{:?}",
                        roc_types::subs::SubsFmtContent(
                            env.get_content_without_compacting(*rec),
                            env
                        )
                    );
                    // unify the structure with this recursive tag union
                    unify_pool(env, pool, ctx.first, *structure, ctx.mode)
                }
                FlatType::FunctionOrTagUnion(_, _, _) => {
                    // unify the structure with this unrecursive tag union
                    unify_pool(env, pool, ctx.first, *structure, ctx.mode)
                }
                // Only tag unions can be recursive; everything else is an error.
                _ => mismatch!(
                    "trying to unify {:?} with recursive type var {:?}",
                    &flat_type,
                    structure
                ),
            };

            env.remove_recursion_pair(ctx.first, ctx.second);
            outcome
        }

        Structure(ref other_flat_type) => {
            // Unify the two flat types
            unify_flat_type(env, pool, ctx, flat_type, other_flat_type)
        }
        // _sym has an underscore because it's unused in --release builds
        Alias(_sym, _, real_var, kind) => match kind {
            AliasKind::Structural => {
                // NB: not treating this as a presence constraint seems pivotal! I
                // can't quite figure out why, but it doesn't seem to impact other types.
                unify_pool(env, pool, ctx.first, *real_var, ctx.mode.as_eq())
            }
            AliasKind::Opaque => {
                mismatch!(
                    "Cannot unify structure {:?} with opaque {:?}",
                    &flat_type,
                    _sym
                )
            }
        },
        LambdaSet(..) => {
            mismatch!(
                "Cannot unify structure \n{:?} \nwith lambda set\n {:?}",
                roc_types::subs::SubsFmtContent(&Content::Structure(*flat_type), env),
                roc_types::subs::SubsFmtContent(other, env),
                // &flat_type,
                // other
            )
        }
        ErasedLambda => mismatch!(),
        Pure | Effectful => mismatch!("Cannot unify structure {:?} with fx vars", &flat_type),
        RangedNumber(other_range_vars) => {
            check_and_merge_valid_range(env, pool, ctx, ctx.second, *other_range_vars, ctx.first)
        }
        Error => merge(env, ctx, Error),
    }
}

#[inline(always)]
#[must_use]
fn unify_erased_lambda<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            if M::UNIFYING_SPECIALIZATION {
                todo_lambda_erasure!()
            }
            merge(env, ctx, Content::ErasedLambda)
        }
        Content::LambdaSet(..) => {
            if M::UNIFYING_SPECIALIZATION {
                todo_lambda_erasure!()
            }
            merge(env, ctx, Content::ErasedLambda)
        }
        ErasedLambda => merge(env, ctx, Content::ErasedLambda),
        RecursionVar { structure, .. } => unify_pool(env, pool, ctx.first, *structure, ctx.mode),
        RigidVar(..) | RigidAbleVar(..) => mismatch!("Lambda sets never unify with rigid"),
        FlexAbleVar(..) => mismatch!("Lambda sets should never have abilities attached to them"),
        Structure(..) => mismatch!("Lambda set cannot unify with non-lambda set structure"),
        RangedNumber(..) => mismatch!("Lambda sets are never numbers"),
        Alias(..) => mismatch!("Lambda set can never be directly under an alias!"),
        Pure | Effectful => mismatch!("Lambda set cannot unify with fx vars"),
        Error => merge(env, ctx, Error),
    }
}

#[inline(always)]
#[must_use]
fn unify_pure<M: MetaCollector>(env: &mut Env, ctx: &Context, other: &Content) -> Outcome<M> {
    match other {
        Pure | FlexVar(_) => merge(env, ctx, Pure),
        Effectful => merge(env, ctx, Effectful),
        RigidVar(_)
        | FlexAbleVar(_, _)
        | RigidAbleVar(_, _)
        | RecursionVar { .. }
        | Content::LambdaSet(_)
        | ErasedLambda
        | Structure(_)
        | Alias(_, _, _, _)
        | RangedNumber(_) => {
            mismatch!("Cannot unify pure with {:?}", other)
        }
        Error => merge(env, ctx, Error),
    }
}

#[inline(always)]
#[must_use]
fn unify_effectful<M: MetaCollector>(env: &mut Env, ctx: &Context, other: &Content) -> Outcome<M> {
    match other {
        Effectful | FlexVar(_) => merge(env, ctx, Effectful),
        Pure => mismatch!("Cannot unify effectful with pure"),
        RigidVar(_)
        | FlexAbleVar(_, _)
        | RigidAbleVar(_, _)
        | RecursionVar { .. }
        | Content::LambdaSet(_)
        | ErasedLambda
        | Structure(_)
        | Alias(_, _, _, _)
        | RangedNumber(_) => {
            mismatch!("Cannot unify effectful with {:?}", other)
        }
        Error => merge(env, ctx, Error),
    }
}

#[inline(always)]
#[must_use]
fn unify_lambda_set<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    lambda_set: LambdaSet,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            if M::UNIFYING_SPECIALIZATION {
                // TODO: It appears that this can happen in well-typed, reasonable programs, but it's
                // open question as to why! See also https://github.com/roc-lang/roc/issues/3163.
                let zero_lambda_set = LambdaSet {
                    solved: UnionLabels::default(),
                    recursion_var: OptVariable::NONE,
                    unspecialized: SubsSlice::default(),
                    ambient_function: env.fresh_unnamed_flex_var(),
                };

                extract_specialization_lambda_set(env, ctx, lambda_set, zero_lambda_set)
            } else {
                merge(env, ctx, Content::LambdaSet(lambda_set))
            }
        }
        Content::LambdaSet(other_lambda_set) => {
            if M::UNIFYING_SPECIALIZATION {
                extract_specialization_lambda_set(env, ctx, lambda_set, *other_lambda_set)
            } else {
                unify_lambda_set_help(env, pool, ctx, lambda_set, *other_lambda_set)
            }
        }
        RecursionVar { structure, .. } => {
            if env.seen_recursion_pair(ctx.first, ctx.second) {
                return fix_fixpoint(env, ctx);
            }
            env.add_recursion_pair(ctx.first, ctx.second);

            // suppose that the recursion var is a lambda set
            let outcome = unify_pool(env, pool, ctx.first, *structure, ctx.mode);

            env.remove_recursion_pair(ctx.first, ctx.second);
            outcome
        }
        ErasedLambda => merge(env, ctx, ErasedLambda),
        RigidVar(..) | RigidAbleVar(..) => mismatch!("Lambda sets never unify with rigid"),
        FlexAbleVar(..) => mismatch!("Lambda sets should never have abilities attached to them"),
        Structure(..) => mismatch!("Lambda set cannot unify with non-lambda set structure"),
        RangedNumber(..) => mismatch!("Lambda sets are never numbers"),
        Alias(..) => mismatch!("Lambda set can never be directly under an alias!"),
        Pure | Effectful => mismatch!("Lambda sets never unify with fx vars"),
        Error => merge(env, ctx, Error),
    }
}

fn extract_specialization_lambda_set<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    ability_member_proto_lset: LambdaSet,
    specialization_lset: LambdaSet,
) -> Outcome<M> {
    // We should have the unspecialized ability member lambda set on the left and the
    // specialization lambda set on the right. E.g.
    //
    //   [[] + a:toEncoder:1] ~ [[myTypeLset]]
    //
    // Taking that example, we keep around [[myTypeLset]] in the unification and associate
    // (toEncoder, 1) => [[myTypeLset]] in the metadata collector.

    let LambdaSet {
        solved: member_solved,
        recursion_var: member_rec_var,
        unspecialized: member_uls_slice,
        ambient_function: _,
    } = ability_member_proto_lset;

    debug_assert!(
        member_solved.is_empty(),
        "member signature should not have solved lambda sets"
    );
    debug_assert!(member_rec_var.is_none());

    let member_uls = env.get_subs_slice(member_uls_slice);
    debug_assert!(
        member_uls.len() <= 1,
        "member signature lambda sets should contain at most one unspecialized lambda set"
    );

    if member_uls.is_empty() {
        // This can happen if the specialized type has a lambda set that is determined to be
        // immaterial in the implementation of the specialization, because the specialization
        // lambda set does not line up with one required by the ability member prototype.
        // As an example, consider
        //
        //   Q := [ F (Str -> Str) ] implements [Eq {is_eq}]
        //
        //   is_eq = \@Q _, @Q _ -> Bool.false
        //
        // here the lambda set of `F`'s payload is part of the specialization signature, but it is
        // irrelevant to the specialization. As such, I believe it is safe to drop the
        // empty specialization lambda set.
        roc_tracing::info!(ambient_function=?env.get_root_key_without_compacting(specialization_lset.ambient_function), "ambient function in a specialization has a zero-lambda set");

        return merge(env, ctx, Content::LambdaSet(specialization_lset));
    }

    let Uls(_, member, region) = member_uls[0];

    let mut outcome: Outcome<M> = merge(env, ctx, Content::LambdaSet(specialization_lset));

    outcome
        .extra_metadata
        .record_specialization_lambda_set(member, region, ctx.second);

    outcome
}

#[derive(Debug)]
struct Sides {
    left: Vec<(Symbol, VariableSubsSlice)>,
    right: Vec<(Symbol, VariableSubsSlice)>,
}

impl Default for Sides {
    fn default() -> Self {
        Self {
            left: Vec::with_capacity(1),
            right: Vec::with_capacity(1),
        }
    }
}

struct SeparatedUnionLambdas {
    only_in_left: Vec<(Symbol, VariableSubsSlice)>,
    only_in_right: Vec<(Symbol, VariableSubsSlice)>,
    joined: Vec<(Symbol, VariableSubsSlice)>,
}

fn separate_union_lambdas<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    mode: UnificationMode,
    fields1: UnionLambdas,
    fields2: UnionLambdas,
) -> Result<(Outcome<M>, SeparatedUnionLambdas), Outcome<M>> {
    debug_assert!(
        fields1.is_sorted_allow_duplicates(env),
        "not sorted: {:?}",
        fields1.iter_from_subs(env).collect::<Vec<_>>()
    );
    debug_assert!(
        fields2.is_sorted_allow_duplicates(env),
        "not sorted: {:?}",
        fields2.iter_from_subs(env).collect::<Vec<_>>()
    );

    // lambda names -> (the captures for that lambda on the left side, the captures for that lambda on the right side)
    // e.g. [[F1 U8], [F1 U64], [F2 a]] ~ [[F1 Str], [F2 Str]] becomes
    //   F1 -> { left: [ [U8], [U64] ], right: [ [Str] ] }
    //   F2 -> { left: [ [a] ],         right: [ [Str] ] }
    let mut buckets: VecMap<Symbol, Sides> = VecMap::with_capacity(fields1.len() + fields2.len());

    let (mut fields_left, mut fields_right) =
        (fields1.iter_all().peekable(), fields2.iter_all().peekable());

    loop {
        use std::cmp::Ordering;

        let ord = match (fields_left.peek(), fields_right.peek()) {
            (Some((l, _)), Some((r, _))) => Some((env[*l]).cmp(&env[*r])),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match ord {
            Some(Ordering::Less) => {
                let (sym, vars) = fields_left.next().unwrap();
                let bucket = buckets.get_or_insert(env[sym], Sides::default);
                bucket.left.push((env[sym], env[vars]));
            }
            Some(Ordering::Greater) => {
                let (sym, vars) = fields_right.next().unwrap();
                let bucket = buckets.get_or_insert(env[sym], Sides::default);
                bucket.right.push((env[sym], env[vars]));
            }
            Some(Ordering::Equal) => {
                let (sym, left_vars) = fields_left.next().unwrap();
                let (_sym, right_vars) = fields_right.next().unwrap();
                debug_assert_eq!(env[sym], env[_sym]);

                let bucket = buckets.get_or_insert(env[sym], Sides::default);
                bucket.left.push((env[sym], env[left_vars]));
                bucket.right.push((env[sym], env[right_vars]));
            }
            None => break,
        }
    }

    let mut whole_outcome = Outcome::default();
    let mut only_in_left = Vec::with_capacity(fields1.len());
    let mut only_in_right = Vec::with_capacity(fields2.len());
    let mut joined = Vec::with_capacity(fields1.len() + fields2.len());
    for (lambda_name, Sides { left, mut right }) in buckets {
        match (left.as_slice(), right.as_slice()) {
            (&[], &[]) => internal_error!("somehow both are empty but there's an entry?"),
            (&[], _) => only_in_right.extend(right),
            (_, &[]) => only_in_left.extend(left),
            (_, _) => {
                'next_left: for (_, left_slice) in left {
                    // Does the current slice on the left unify with a slice on the right?
                    //
                    // If yes, we unify then and the unified result to `joined`.
                    //
                    // Otherwise if no such slice on the right is found, then the slice on the `left` has no slice,
                    // either on the left or right, it unifies with (since the left was constructed
                    // inductively via the same procedure).
                    //
                    // At the end each slice in the left and right has been explored, so
                    // - `joined` contains all the slices that can unify
                    // - left contains unique captures slices that will unify with no other slice
                    // - right contains unique captures slices that will unify with no other slice
                    //
                    // Note also if a slice l on the left and a slice r on the right unify, there
                    // is no other r' != r on the right such that l ~ r', and respectively there is
                    // no other l' != l on the left such that l' ~ r. Otherwise, it must be that l ~ l'
                    // (resp. r ~ r'), but then l = l' (resp. r = r'), and they would have become the same
                    // slice in a previous call to `separate_union_lambdas`.
                    'try_next_right: for (right_index, (_, right_slice)) in right.iter().enumerate()
                    {
                        if left_slice.len() != right_slice.len() {
                            continue 'try_next_right;
                        }

                        for (var1, var2) in (left_slice.into_iter()).zip(right_slice.into_iter()) {
                            let (var1, var2) = (env[var1], env[var2]);

                            if M::IS_LATE {
                                // Lambda sets are effectively tags under another name, and their usage can also result
                                // in the arguments of a lambda name being recursive. It very well may happen that
                                // during unification, a lambda set previously marked as not recursive becomes
                                // recursive. See the docs of [LambdaSet] for one example, or https://github.com/roc-lang/roc/pull/2307.
                                //
                                // Like with tag unions, if it is, we'll always pass through this branch. So, take
                                // this opportunity to promote the lambda set to recursive if need be.
                                //
                                // THEORY: observe that this check only happens in late unification
                                // (i.e. code generation, typically). We believe this to be correct
                                // because while recursive lambda sets must be collapsed to the
                                // code generator, we have not yet observed a case where they must
                                // collapsed to the type checker of the surface syntax.
                                // It is possible this assumption will be invalidated!
                                maybe_mark_union_recursive(env, pool, var1);
                                maybe_mark_union_recursive(env, pool, var2);
                            }

                            // Check whether the two type variables in the closure set are
                            // unifiable. If they are, we can unify them and continue on assuming
                            // that these lambdas are in fact the same.
                            //
                            // If they are not unifiable, that means the two lambdas must be
                            // different (since they have different capture sets), and so we don't
                            // want to merge the variables. Instead, we'll treat the lambda sets
                            // are disjoint, and keep them as independent lambda in the resulting
                            // set.
                            //
                            // # Nested lambda sets
                            //
                            // XREF https://github.com/roc-lang/roc/issues/4712
                            //
                            // We must be careful to ensure that if unifying nested lambda sets
                            // results in disjoint lambdas, that the parent lambda sets are
                            // ultimately treated disjointly as well.
                            // Consider
                            //
                            //   v1: {} -[ foo ({} -[ bar Str ]-> {}) ]-> {}
                            // ~ v2: {} -[ foo ({} -[ bar U64 ]-> {}) ]-> {}
                            //
                            // When considering unification of the nested sets
                            //
                            //   [ bar Str ]
                            // ~ [ bar U64 ]
                            //
                            // we should not unify these sets, even disjointly, because that would
                            // ultimately lead us to unifying
                            //
                            // v1 ~ v2
                            // => {} -[ foo ({} -[ bar Str, bar U64 ]-> {}) ] -> {}
                            //
                            // which is quite wrong - we do not have a lambda `foo` that captures
                            // either `bar captures: Str` or `bar captures: U64`, we have two
                            // different lambdas `foo` that capture different `bars`. The target
                            // unification is
                            //
                            // v1 ~ v2
                            // => {} -[ foo ({} -[ bar Str ]-> {}),
                            //          foo ({} -[ bar U64 ]-> {}) ] -> {}
                            let subs_snapshot = env.snapshot();
                            let pool_snapshot = pool.len();
                            let outcome: Outcome<M> = unify_pool(env, pool, var1, var2, mode);

                            if !outcome.mismatches.is_empty() {
                                // Rolling back will also pull apart any nested lambdas that
                                // were joined into the same set.
                                env.rollback_to(subs_snapshot);
                                pool.truncate(pool_snapshot);
                                continue 'try_next_right;
                            } else {
                                let outcome = unify_pool(env, pool, var1, var2, mode);
                                whole_outcome.union(outcome);
                            }
                        }

                        // All the variables unified, so we can join the left + right.
                        // The variables are unified in left and right slice, so just reuse the left slice.
                        joined.push((lambda_name, left_slice));
                        // Remove the right slice, it unifies with the left so this is its unique
                        // unification.
                        // Remove in-place so that the order is preserved.
                        right.remove(right_index);
                        continue 'next_left;
                    }

                    // No slice on the right unified with the left, so the slice on the left is on
                    // its own.
                    only_in_left.push((lambda_name, left_slice));
                }

                // Possible that there are items left over in the right, they are on their own.
                only_in_right.extend(right);
            }
        }
    }

    Ok((
        whole_outcome,
        SeparatedUnionLambdas {
            only_in_left,
            only_in_right,
            joined,
        },
    ))
}

/// ULS-SORT-ORDER:
///   - Arrange into partitions of (_, member, region), in ascending order of (member, region).
///   - Within each partition, place flex-able vars at the end of the partition.
///   - Amongst all flex-able vars, sort by their root key, so that identical vars are next to each other.
#[inline(always)]
fn unspecialized_lambda_set_sorter(subs: &Subs, uls1: Uls, uls2: Uls) -> std::cmp::Ordering {
    let Uls(var1, sym1, region1) = uls1;
    let Uls(var2, sym2, region2) = uls2;

    use std::cmp::Ordering::*;
    use Content::*;
    match (sym1, region1).cmp(&(sym2, region2)) {
        Equal => {
            match (
                subs.get_content_without_compacting(var1),
                subs.get_content_without_compacting(var2),
            ) {
                (FlexAbleVar(..) | RigidAbleVar(..), FlexAbleVar(..) | RigidAbleVar(..)) => subs
                    .get_root_key_without_compacting(var1)
                    .cmp(&subs.get_root_key_without_compacting(var2)),
                (FlexVar(..) | RigidVar(..), _) | (_, FlexVar(..) | RigidVar(..)) => {
                    internal_error!("unexpected variable type in unspecialized lambda set!")
                }
                (FlexAbleVar(..), _) => Greater,
                (_, FlexAbleVar(..)) => Less,
                // For everything else, sort by the root key
                (_, _) => subs
                    .get_root_key_without_compacting(var1)
                    .cmp(&subs.get_root_key_without_compacting(var2)),
            }
        }
        ord => ord,
    }
}

#[inline(always)]
fn sort_unspecialized_lambda_sets(subs: &Subs, mut uls: Vec<Uls>) -> Vec<Uls> {
    uls.sort_by(|&uls1, &uls2| unspecialized_lambda_set_sorter(subs, uls1, uls2));
    uls
}

#[inline(always)]
fn is_sorted_unspecialized_lamba_set_list(subs: &Subs, uls: &[Uls]) -> bool {
    uls == sort_unspecialized_lambda_sets(subs, uls.to_vec())
}

#[must_use = "must use outcomes!"]
fn unify_unspecialized_lambdas<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    mode: UnificationMode,
    uls_left: SubsSlice<Uls>,
    uls_right: SubsSlice<Uls>,
) -> Result<(SubsSlice<Uls>, Outcome<M>), Outcome<M>> {
    // Note that we don't need to update the bookkeeping of variable -> lambda set to be resolved,
    // because if we had v1 -> lset1, and now lset1 ~ lset2, then afterward either lset1 still
    // resolves to itself or re-points to lset2.
    // In either case the merged unspecialized lambda sets will be there.
    let (uls_left, uls_right) = match (uls_left.is_empty(), uls_right.is_empty()) {
        (true, true) => return Ok((SubsSlice::default(), Default::default())),
        (false, true) => return Ok((uls_left, Default::default())),
        (true, false) => return Ok((uls_right, Default::default())),
        (false, false) => (
            env.get_subs_slice(uls_left).to_vec(),
            env.get_subs_slice(uls_right).to_vec(),
        ),
    };

    // Unfortunately, it is not an invariant that `uls_left` and `uls_right` obey ULS-SORT-ORDER before
    // merging.
    //
    // That's because flex-able variables in unspecialized lambda sets may be unified at any time,
    // and unification of flex-able variables may change their root keys, which ULS-SORT-ORDER
    // considers.
    //
    // As such, we must sort beforehand. In practice these sets are very, very small (<5 elements).
    let uls_left = sort_unspecialized_lambda_sets(env, uls_left);
    let uls_right = sort_unspecialized_lambda_sets(env, uls_right);

    let (mut uls_left, mut uls_right) = (uls_left.iter().peekable(), uls_right.iter().peekable());
    let mut merged_uls = Vec::with_capacity(uls_left.len() + uls_right.len());
    let mut whole_outcome = Outcome::default();

    loop {
        let (uls_l, uls_r) = match (uls_left.peek(), uls_right.peek()) {
            (Some(uls_l), Some(uls_r)) => (**uls_l, **uls_r),
            (Some(_), None) => {
                merged_uls.push(*uls_left.next().unwrap());
                continue;
            }
            (None, Some(_)) => {
                merged_uls.push(*uls_right.next().unwrap());
                continue;
            }
            (None, None) => break,
        };

        let Uls(var_l, sym_l, region_l) = uls_l;
        let Uls(var_r, sym_r, region_r) = uls_r;

        use std::cmp::Ordering::*;
        match (sym_l, region_l).cmp(&(sym_r, region_r)) {
            Less => {
                // Left needs to catch up to right, add it to the merged lambdas.
                merged_uls.push(*uls_left.next().unwrap());
            }
            Greater => {
                // Right needs to catch up to left, add it to the merged lambdas.
                merged_uls.push(*uls_right.next().unwrap());
            }
            Equal => {
                // The interesting case - both point to the same specialization.
                use Content::*;
                match (
                    env.get_content_without_compacting(var_l),
                    env.get_content_without_compacting(var_r),
                ) {
                    (FlexAbleVar(..) | RigidAbleVar(..), FlexAbleVar(..) | RigidAbleVar(..)) => {
                        // If the types are root-equivalent, de-duplicate them.
                        //
                        // Otherwise, the type variables are disjoint, and we want to keep both
                        // of them, for purposes of disjoint variable lambda specialization.
                        //
                        // For more information, see "A Property that’s lost, and how we can hold on to it"
                        // in solve/docs/ambient_lambda_set_specialization.md.

                        if env.equivalent_without_compacting(var_l, var_r) {
                            //     ... a1    ...
                            //     ... b1=a1 ...
                            // =>  ... a1    ...
                            //
                            // Keep the one on the left, drop the one on the right.
                            //
                            // Then progress both, because the invariant tells us they must be
                            // disjoint, and if there were any concrete variables, they would have
                            // appeared earlier.
                            let _dropped = uls_right.next().unwrap();
                            let kept = uls_left.next().unwrap();
                            merged_uls.push(*kept);
                        } else if mode.is_lambda_set_specialization() {
                            //     ... a1    ...
                            //     ... b1    ...
                            // =>  ... a1=b1 ...
                            //
                            // If we're in the process of running the ambient lambda set
                            // specialization procedure, disjoint type variables being merged from
                            // the left and right lists are treated specially!
                            //
                            // In particular, we are unifying a local list of lambda sets, for
                            // which the specialization is for (on the left), with specialization
                            // lambda sets, which have just been freshened (on the right).
                            //
                            //     [ ..  a:lam:1 ] (local, undergoing specialization)
                            //     [ .. a':lam:1 ] (specialization lambda sets, just freshened)
                            //
                            // Because the specialization lambdas are freshened, they certainly are
                            // disjoint from the local lambdas - but they may be equivalent in
                            // principle, from the perspective of a human looking at the
                            // unification!
                            //
                            // Running with the example above, the specialization lambda set has an
                            // unspecialized lambda `a':lam:1`. Now, this is disjoint from
                            // `a:lam:1` in the local lambda set, from the purely technical
                            // perspective that `a' != a`.
                            //
                            // But, in expected function, they **should not** be treated as disjoint!
                            // In this case, the specialization lambda is not introducing any new
                            // information, and is targeting exactly the local lambda `a:lam:1`.
                            //
                            // So, to avoid introducing superfluous variables, we unify these disjoint
                            // variables once, and then progress on both sides. We progress on both
                            // sides to avoid unifying more than what we should in our principle.
                            //
                            // It doesn't matter which side we choose to progress on, since after
                            // unification of flex vars roots are equivalent. So, choose the left
                            // side.
                            //
                            // See the ambient lambda set specialization document for more details.
                            let outcome = unify_pool(env, pool, var_l, var_r, mode);
                            if !outcome.mismatches.is_empty() {
                                return Err(outcome);
                            }
                            whole_outcome.union(outcome);

                            debug_assert!(env.equivalent_without_compacting(var_l, var_r));

                            let _dropped = uls_right.next().unwrap();
                            let kept = uls_left.next().unwrap();
                            merged_uls.push(*kept);
                        } else {
                            // CASE: disjoint_flex_specializations
                            //
                            //     ... a1     ...
                            //     ... b1     ...
                            // =>  ... a1, b1 ...
                            //
                            // Keep both. But, we have to be careful about how we do this -
                            // immediately add the one with the lower root, and advance that side;
                            // keep the other as-is, because the next variable on the advanced side
                            // might be lower than the current non-advanced variable. For example:
                            //
                            //     ... 640 645 ...
                            //     ... 670 ...
                            //
                            // we want to add `640` to the merged list and advance to
                            //
                            //     ... 645 ...
                            //     ... 670 ...
                            //
                            // rather than adding both `640` and `670`, and skipping the comparison
                            // of `645` with `670`.
                            //
                            // An important thing to notice is that we *don't* want to advance
                            // both sides, because if these two variables are disjoint, then
                            // advancing one side *might* make the next comparison be between
                            // equivalent variables, for example in a case like
                            //
                            //     ... 640 670 ...
                            //     ... 670 ...
                            //
                            // In the above case, we certainly only want to advance the left side!
                            if env.get_root_key(var_l) < env.get_root_key(var_r) {
                                let kept = uls_left.next().unwrap();
                                merged_uls.push(*kept);
                            } else {
                                let kept = uls_right.next().unwrap();
                                merged_uls.push(*kept);
                            }
                        }
                    }
                    (FlexAbleVar(..) | RigidAbleVar(..), _) => {
                        //     ... a1       ...
                        //     ... {foo: _} ...
                        // =>  ... {foo: _} ...
                        //
                        // Unify them, then advance the merged flex var.

                        let outcome = unify_pool(env, pool, var_l, var_r, mode);
                        if !outcome.mismatches.is_empty() {
                            return Err(outcome);
                        }
                        whole_outcome.union(outcome);

                        let _dropped = uls_right.next().unwrap();
                    }
                    (_, FlexAbleVar(..) | RigidAbleVar(..)) => {
                        //     ... {foo: _} ...
                        //     ... a1       ...
                        // =>  ... {foo: _} ...
                        //
                        // Unify them, then advance the merged flex var.

                        let outcome = unify_pool(env, pool, var_l, var_r, mode);
                        if !outcome.mismatches.is_empty() {
                            return Err(outcome);
                        }
                        whole_outcome.union(outcome);

                        let _dropped = uls_left.next().unwrap();
                    }
                    (_, _) => {
                        if env.equivalent_without_compacting(var_l, var_r) {
                            //     ... a1    ...
                            //     ... b1=a1 ...
                            // =>  ... a1    ...
                            //
                            // Keep the one on the left, drop the one on the right. Then progress
                            // both, because the next variable on the left must be disjoint from
                            // the current on the right (resp. next variable on the right vs.
                            // current left) - if they aren't, then the invariant was broken.
                            //
                            // Then progress both, because the invariant tells us they must be
                            // disjoint, and if there were any concrete variables, they would have
                            // appeared earlier.
                            let _dropped = uls_right.next().unwrap();
                            let kept = uls_left.next().unwrap();
                            merged_uls.push(*kept);
                        } else {
                            // Even if these two variables unify, since they are not equivalent,
                            // they correspond to different specializations! As such we must not
                            // merge them.
                            //
                            // Instead, keep both, but do so by adding and advancing the side with
                            // the lower root. See CASE disjoint_flex_specializations for
                            // reasoning.
                            if env.get_root_key(var_l) < env.get_root_key(var_r) {
                                let kept = uls_left.next().unwrap();
                                merged_uls.push(*kept);
                            } else {
                                let kept = uls_right.next().unwrap();
                                merged_uls.push(*kept);
                            }
                        }
                    }
                }
            }
        }
    }

    debug_assert!(
        is_sorted_unspecialized_lamba_set_list(env, &merged_uls),
        "merging of unspecialized lambda sets does not preserve sort! {merged_uls:?}"
    );

    Ok((
        slice_extend_new(&mut env.unspecialized_lambda_sets, merged_uls),
        whole_outcome,
    ))
}

#[must_use]
fn unify_lambda_set_help<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    lset1: self::LambdaSet,
    lset2: self::LambdaSet,
) -> Outcome<M> {
    // LambdaSets unify like TagUnions, but can grow unbounded regardless of the extension
    // variable.

    let LambdaSet {
        solved: solved1,
        recursion_var: rec1,
        unspecialized: uls1,
        ambient_function: ambient_function_var1,
    } = lset1;
    let LambdaSet {
        solved: solved2,
        recursion_var: rec2,
        unspecialized: uls2,
        ambient_function: ambient_function_var2,
    } = lset2;

    // Assumed precondition: the ambient functions have already been unified, or are in the process
    // of being unified - otherwise, how could we have reached unification of lambda sets?
    let _ = ambient_function_var2;
    let ambient_function_var = ambient_function_var1;

    debug_assert!(
        (rec1.into_variable().into_iter())
            .chain(rec2.into_variable().into_iter())
            .all(|v| is_recursion_var(env, v)),
        "Recursion var is present, but it doesn't have a recursive content!"
    );

    let (
        mut whole_outcome,
        SeparatedUnionLambdas {
            only_in_left,
            only_in_right,
            joined,
        },
    ) = match separate_union_lambdas(env, pool, ctx.mode, solved1, solved2) {
        Ok((outcome, separated)) => (outcome, separated),
        Err(err_outcome) => return err_outcome,
    };

    let all_lambdas = joined
        .into_iter()
        .map(|(name, slice)| (name, env.get_subs_slice(slice).to_vec()));

    let all_lambdas = merge_sorted_preserving_duplicates(
        all_lambdas,
        only_in_left.into_iter().map(|(name, subs_slice)| {
            let vec = env.get_subs_slice(subs_slice).to_vec();
            (name, vec)
        }),
    );
    let all_lambdas = merge_sorted_preserving_duplicates(
        all_lambdas,
        only_in_right.into_iter().map(|(name, subs_slice)| {
            let vec = env.get_subs_slice(subs_slice).to_vec();
            (name, vec)
        }),
    );

    let recursion_var = match (rec1.into_variable(), rec2.into_variable()) {
        // Prefer left when it's available.
        (Some(rec), _) | (_, Some(rec)) => OptVariable::from(rec),
        (None, None) => OptVariable::NONE,
    };

    let merged_unspecialized = match unify_unspecialized_lambdas(env, pool, ctx.mode, uls1, uls2) {
        Ok((merged, outcome)) => {
            whole_outcome.union(outcome);
            merged
        }
        Err(outcome) => {
            debug_assert!(!outcome.mismatches.is_empty());
            return outcome;
        }
    };

    let new_solved = UnionLabels::insert_into_subs(env, all_lambdas);
    let new_lambda_set = Content::LambdaSet(LambdaSet {
        solved: new_solved,
        recursion_var,
        unspecialized: merged_unspecialized,
        ambient_function: ambient_function_var,
    });

    let merge_outcome = merge(env, ctx, new_lambda_set);
    whole_outcome.union(merge_outcome);
    whole_outcome
}

#[must_use]
fn unify_record<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    fields1: RecordFields,
    ext1: Variable,
    fields2: RecordFields,
    ext2: Variable,
) -> Outcome<M> {
    let (separate, ext1, ext2) = separate_record_fields(env, fields1, ext1, fields2, ext2);

    let shared_fields = separate.in_both;

    if separate.only_in_1.is_empty() {
        if separate.only_in_2.is_empty() {
            // these variable will be the empty record, but we must still unify them
            let ext_outcome = unify_pool(env, pool, ext1, ext2, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut field_outcome =
                unify_shared_fields(env, pool, ctx, shared_fields, OtherFields::None, ext1);

            field_outcome.union(ext_outcome);

            field_outcome
        } else {
            let only_in_2 = RecordFields::insert_into_subs(env, separate.only_in_2);
            let flat_type = FlatType::Record(only_in_2, ext2);
            let sub_record = fresh(env, pool, ctx, Structure(flat_type));
            let ext_outcome = unify_pool(env, pool, ext1, sub_record, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut field_outcome =
                unify_shared_fields(env, pool, ctx, shared_fields, OtherFields::None, sub_record);

            field_outcome.union(ext_outcome);

            field_outcome
        }
    } else if separate.only_in_2.is_empty() {
        let only_in_1 = RecordFields::insert_into_subs(env, separate.only_in_1);
        let flat_type = FlatType::Record(only_in_1, ext1);
        let sub_record = fresh(env, pool, ctx, Structure(flat_type));
        let ext_outcome = unify_pool(env, pool, sub_record, ext2, ctx.mode);

        if !ext_outcome.mismatches.is_empty() {
            return ext_outcome;
        }

        let mut field_outcome =
            unify_shared_fields(env, pool, ctx, shared_fields, OtherFields::None, sub_record);

        field_outcome.union(ext_outcome);

        field_outcome
    } else {
        let only_in_1 = RecordFields::insert_into_subs(env, separate.only_in_1);
        let only_in_2 = RecordFields::insert_into_subs(env, separate.only_in_2);

        let other_fields = OtherFields::Other(only_in_1, only_in_2);

        let ext = fresh(env, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::Record(only_in_1, ext);
        let flat_type2 = FlatType::Record(only_in_2, ext);

        let sub1 = fresh(env, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(env, pool, ctx, Structure(flat_type2));

        let rec1_outcome = unify_pool(env, pool, ext1, sub2, ctx.mode);
        if !rec1_outcome.mismatches.is_empty() {
            return rec1_outcome;
        }

        let rec2_outcome = unify_pool(env, pool, sub1, ext2, ctx.mode);
        if !rec2_outcome.mismatches.is_empty() {
            return rec2_outcome;
        }

        let mut field_outcome =
            unify_shared_fields(env, pool, ctx, shared_fields, other_fields, ext);

        field_outcome
            .mismatches
            .reserve(rec1_outcome.mismatches.len() + rec2_outcome.mismatches.len());
        field_outcome.union(rec1_outcome);
        field_outcome.union(rec2_outcome);

        field_outcome
    }
}

#[must_use]
fn unify_tuple<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    elems1: TupleElems,
    ext1: Variable,
    elems2: TupleElems,
    ext2: Variable,
) -> Outcome<M> {
    let (separate, ext1, ext2) = separate_tuple_elems(env, elems1, ext1, elems2, ext2);

    let shared_elems = separate.in_both;

    if separate.only_in_1.is_empty() {
        if separate.only_in_2.is_empty() {
            // these variable will be the empty tuple, but we must still unify them
            let ext_outcome = unify_pool(env, pool, ext1, ext2, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut field_outcome =
                unify_shared_tuple_elems(env, pool, ctx, shared_elems, OtherTupleElems::None, ext1);

            field_outcome.union(ext_outcome);

            field_outcome
        } else {
            let only_in_2 = TupleElems::insert_into_subs(env, separate.only_in_2);
            let flat_type = FlatType::Tuple(only_in_2, ext2);
            let sub_record = fresh(env, pool, ctx, Structure(flat_type));
            let ext_outcome = unify_pool(env, pool, ext1, sub_record, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut field_outcome = unify_shared_tuple_elems(
                env,
                pool,
                ctx,
                shared_elems,
                OtherTupleElems::None,
                sub_record,
            );

            field_outcome.union(ext_outcome);

            field_outcome
        }
    } else if separate.only_in_2.is_empty() {
        let only_in_1 = TupleElems::insert_into_subs(env, separate.only_in_1);
        let flat_type = FlatType::Tuple(only_in_1, ext1);
        let sub_record = fresh(env, pool, ctx, Structure(flat_type));
        let ext_outcome = unify_pool(env, pool, sub_record, ext2, ctx.mode);

        if !ext_outcome.mismatches.is_empty() {
            return ext_outcome;
        }

        let mut field_outcome = unify_shared_tuple_elems(
            env,
            pool,
            ctx,
            shared_elems,
            OtherTupleElems::None,
            sub_record,
        );

        field_outcome.union(ext_outcome);

        field_outcome
    } else {
        let only_in_1 = TupleElems::insert_into_subs(env, separate.only_in_1);
        let only_in_2 = TupleElems::insert_into_subs(env, separate.only_in_2);

        let other_fields = OtherTupleElems::Other(only_in_1, only_in_2);

        let ext = fresh(env, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::Tuple(only_in_1, ext);
        let flat_type2 = FlatType::Tuple(only_in_2, ext);

        let sub1 = fresh(env, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(env, pool, ctx, Structure(flat_type2));

        let rec1_outcome = unify_pool(env, pool, ext1, sub2, ctx.mode);
        if !rec1_outcome.mismatches.is_empty() {
            return rec1_outcome;
        }

        let rec2_outcome = unify_pool(env, pool, sub1, ext2, ctx.mode);
        if !rec2_outcome.mismatches.is_empty() {
            return rec2_outcome;
        }

        let mut field_outcome =
            unify_shared_tuple_elems(env, pool, ctx, shared_elems, other_fields, ext);

        field_outcome
            .mismatches
            .reserve(rec1_outcome.mismatches.len() + rec2_outcome.mismatches.len());
        field_outcome.union(rec1_outcome);
        field_outcome.union(rec2_outcome);

        field_outcome
    }
}

enum OtherFields {
    None,
    Other(RecordFields, RecordFields),
}

type SharedFields = Vec<(Lowercase, (RecordField<Variable>, RecordField<Variable>))>;

#[must_use]
fn unify_shared_fields<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    shared_fields: SharedFields,
    other_fields: OtherFields,
    ext: Variable,
) -> Outcome<M> {
    let mut matching_fields = Vec::with_capacity(shared_fields.len());
    let num_shared_fields = shared_fields.len();

    let mut whole_outcome = Outcome::default();

    for (name, (actual, expected)) in shared_fields {
        let local_outcome = unify_pool(
            env,
            pool,
            actual.into_inner(),
            expected.into_inner(),
            ctx.mode,
        );

        if local_outcome.mismatches.is_empty() {
            use RecordField::*;

            // Unification of optional fields
            //
            // Demanded does not unify with Optional
            // RigidOptional does not unify with Required or Demanded
            // RigidRequired does not unify with Optional
            // Unifying Required with Demanded => Demanded
            // Unifying Optional with Required => Required
            // Unifying Optional with RigidOptional => RigidOptional
            // Unifying X with X => X
            let actual = match (actual, expected) {
                (Demanded(_), Optional(_)) | (Optional(_), Demanded(_)) => {
                    // this is an error, but we continue to give better error messages
                    continue;
                }

                (Demanded(a), Required(b))
                | (Required(a), Demanded(b))
                | (Demanded(a), Demanded(b)) => Demanded(choose_merged_var(env, a, b)),
                (Required(a), Required(b))
                | (Required(a), Optional(b))
                | (Optional(a), Required(b)) => Required(choose_merged_var(env, a, b)),

                (Optional(a), Optional(b)) => Optional(choose_merged_var(env, a, b)),

                // rigid optional
                (RigidOptional(a), Optional(b)) | (Optional(b), RigidOptional(a)) => {
                    RigidOptional(choose_merged_var(env, a, b))
                }
                (RigidOptional(a), RigidOptional(b)) => RigidOptional(choose_merged_var(env, a, b)),
                (RigidOptional(_), Demanded(_) | Required(_) | RigidRequired(_))
                | (Demanded(_) | Required(_) | RigidRequired(_), RigidOptional(_)) => {
                    // this is an error, but we continue to give better error messages
                    continue;
                }

                // rigid required
                (RigidRequired(_), Optional(_)) | (Optional(_), RigidRequired(_)) => {
                    // this is an error, but we continue to give better error messages
                    continue;
                }
                (RigidRequired(a), Demanded(b) | Required(b))
                | (Demanded(b) | Required(b), RigidRequired(a)) => {
                    RigidRequired(choose_merged_var(env, a, b))
                }
                (RigidRequired(a), RigidRequired(b)) => RigidRequired(choose_merged_var(env, a, b)),
            };

            matching_fields.push((name, actual));
            whole_outcome.union(local_outcome);
        }
    }

    if num_shared_fields == matching_fields.len() {
        // pull fields in from the ext_var

        let (ext_fields, new_ext_var) = RecordFields::empty().sorted_iterator_and_ext(env, ext);
        let ext_fields: Vec<_> = ext_fields.into_iter().collect();

        let fields: RecordFields = match other_fields {
            OtherFields::None => {
                if ext_fields.is_empty() {
                    RecordFields::insert_into_subs(env, matching_fields)
                } else {
                    let all_fields = merge_sorted(matching_fields, ext_fields);
                    RecordFields::insert_into_subs(env, all_fields)
                }
            }
            OtherFields::Other(other1, other2) => {
                let mut all_fields = merge_sorted(matching_fields, ext_fields);
                all_fields = merge_sorted(
                    all_fields,
                    other1.iter_all().map(|(i1, i2, i3)| {
                        let field_name: Lowercase = env[i1].clone();
                        let variable = env[i2];
                        let record_field: RecordField<Variable> = env[i3].map(|_| variable);

                        (field_name, record_field)
                    }),
                );

                all_fields = merge_sorted(
                    all_fields,
                    other2.iter_all().map(|(i1, i2, i3)| {
                        let field_name: Lowercase = env[i1].clone();
                        let variable = env[i2];
                        let record_field: RecordField<Variable> = env[i3].map(|_| variable);

                        (field_name, record_field)
                    }),
                );

                RecordFields::insert_into_subs(env, all_fields)
            }
        };

        let flat_type = FlatType::Record(fields, new_ext_var);

        let merge_outcome = merge(env, ctx, Structure(flat_type));
        whole_outcome.union(merge_outcome);
        whole_outcome
    } else {
        mismatch!("in unify_shared_fields")
    }
}

enum OtherTupleElems {
    None,
    Other(TupleElems, TupleElems),
}

type SharedTupleElems = Vec<(usize, (Variable, Variable))>;

#[must_use]
fn unify_shared_tuple_elems<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    shared_elems: SharedTupleElems,
    other_elems: OtherTupleElems,
    ext: Variable,
) -> Outcome<M> {
    let mut matching_elems = Vec::with_capacity(shared_elems.len());
    let num_shared_elems = shared_elems.len();

    let mut whole_outcome = Outcome::default();

    for (name, (actual, expected)) in shared_elems {
        let local_outcome = unify_pool(env, pool, actual, expected, ctx.mode);

        if local_outcome.mismatches.is_empty() {
            let actual = choose_merged_var(env, actual, expected);

            matching_elems.push((name, actual));
            whole_outcome.union(local_outcome);
        }
    }

    if num_shared_elems == matching_elems.len() {
        // pull elems in from the ext_var

        let (ext_elems, new_ext_var) = TupleElems::empty().sorted_iterator_and_ext(env, ext);
        let ext_elems: Vec<_> = ext_elems.into_iter().collect();

        let elems: TupleElems = match other_elems {
            OtherTupleElems::None => {
                if ext_elems.is_empty() {
                    TupleElems::insert_into_subs(env, matching_elems)
                } else {
                    let all_elems = merge_sorted(matching_elems, ext_elems);
                    TupleElems::insert_into_subs(env, all_elems)
                }
            }
            OtherTupleElems::Other(other1, other2) => {
                let mut all_elems = merge_sorted(matching_elems, ext_elems);
                all_elems = merge_sorted(
                    all_elems,
                    other1.iter_all().map(|(i1, i2)| {
                        let elem_index: usize = env[i1];
                        let variable = env[i2];

                        (elem_index, variable)
                    }),
                );

                all_elems = merge_sorted(
                    all_elems,
                    other2.iter_all().map(|(i1, i2)| {
                        let elem_index: usize = env[i1];
                        let variable = env[i2];

                        (elem_index, variable)
                    }),
                );

                TupleElems::insert_into_subs(env, all_elems)
            }
        };

        let flat_type = FlatType::Tuple(elems, new_ext_var);

        let merge_outcome = merge(env, ctx, Structure(flat_type));
        whole_outcome.union(merge_outcome);
        whole_outcome
    } else {
        mismatch!("in unify_shared_tuple_elems")
    }
}

fn separate_record_fields(
    subs: &Subs,
    fields1: RecordFields,
    ext1: Variable,
    fields2: RecordFields,
    ext2: Variable,
) -> (
    Separate<Lowercase, RecordField<Variable>>,
    Variable,
    Variable,
) {
    let (it1, new_ext1) = fields1.sorted_iterator_and_ext(subs, ext1);
    let (it2, new_ext2) = fields2.sorted_iterator_and_ext(subs, ext2);

    let it1 = it1.collect::<Vec<_>>();
    let it2 = it2.collect::<Vec<_>>();

    (separate(it1, it2), new_ext1, new_ext2)
}

fn separate_tuple_elems(
    subs: &Subs,
    elems1: TupleElems,
    ext1: Variable,
    elems2: TupleElems,
    ext2: Variable,
) -> (Separate<usize, Variable>, Variable, Variable) {
    let (it1, new_ext1) = elems1.sorted_iterator_and_ext(subs, ext1);
    let (it2, new_ext2) = elems2.sorted_iterator_and_ext(subs, ext2);

    let it1 = it1.collect::<Vec<_>>();
    let it2 = it2.collect::<Vec<_>>();

    (separate(it1, it2), new_ext1, new_ext2)
}

// TODO: consider combining with `merge_sorted_help` with a `by_key` predicate.
// But that might not get inlined!
fn merge_sorted_keys<K, I1, I2>(input1: I1, input2: I2) -> Vec<K>
where
    K: Ord,
    I1: ExactSizeIterator<Item = K>,
    I2: ExactSizeIterator<Item = K>,
{
    use std::cmp::Ordering::{Equal, Greater, Less};

    let mut merged = Vec::with_capacity(input1.len() + input2.len());

    let mut input1 = input1.peekable();
    let mut input2 = input2.peekable();

    loop {
        let choice = match (input1.peek(), input2.peek()) {
            (Some(l), Some(r)) => l.cmp(r),
            (Some(_), None) => Less,
            (None, Some(_)) => Greater,
            (None, None) => break,
        };

        match choice {
            Less => {
                merged.push(input1.next().unwrap());
            }
            Greater => {
                merged.push(input2.next().unwrap());
            }
            Equal => {
                let k = input1.next().unwrap();
                let _ = input2.next().unwrap();
                merged.push(k)
            }
        }
    }

    merged
}

#[derive(Debug)]
struct Separate<K, V> {
    only_in_1: Vec<(K, V)>,
    only_in_2: Vec<(K, V)>,
    in_both: Vec<(K, (V, V))>,
}

fn merge_sorted_help<K, V, I1, I2>(input1: I1, input2: I2, preserve_duplicates: bool) -> Vec<(K, V)>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    use std::cmp::Ordering;

    let mut it1 = input1.into_iter().peekable();
    let mut it2 = input2.into_iter().peekable();

    let input1_len = it1.size_hint().0;
    let input2_len = it2.size_hint().0;

    let mut result = Vec::with_capacity(input1_len + input2_len);

    loop {
        let which = match (it1.peek(), it2.peek()) {
            (Some((l, _)), Some((r, _))) => Some(l.cmp(r)),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match which {
            Some(Ordering::Less) => {
                result.push(it1.next().unwrap());
            }
            Some(Ordering::Equal) => {
                let (k, v) = it1.next().unwrap();
                let (k2, v2) = it2.next().unwrap();
                result.push((k, v));
                if preserve_duplicates {
                    result.push((k2, v2));
                }
            }
            Some(Ordering::Greater) => {
                result.push(it2.next().unwrap());
            }
            None => break,
        }
    }

    result
}

fn merge_sorted<K, V, I1, I2>(input1: I1, input2: I2) -> Vec<(K, V)>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    merge_sorted_help(input1, input2, false)
}

fn merge_sorted_preserving_duplicates<K, V, I1, I2>(input1: I1, input2: I2) -> Vec<(K, V)>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    merge_sorted_help(input1, input2, true)
}

fn separate<K, V, I1, I2>(input1: I1, input2: I2) -> Separate<K, V>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    use std::cmp::Ordering;

    let mut it1 = input1.into_iter().peekable();
    let mut it2 = input2.into_iter().peekable();

    let input1_len = it1.size_hint().0;
    let input2_len = it2.size_hint().0;

    let max_common = std::cmp::min(input1_len, input2_len);

    let mut result = Separate {
        only_in_1: Vec::with_capacity(input1_len),
        only_in_2: Vec::with_capacity(input2_len),
        in_both: Vec::with_capacity(max_common),
    };

    loop {
        let which = match (it1.peek(), it2.peek()) {
            (Some((l, _)), Some((r, _))) => Some(l.cmp(r)),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match which {
            Some(Ordering::Less) => {
                result.only_in_1.push(it1.next().unwrap());
            }
            Some(Ordering::Equal) => {
                let (k, v1) = it1.next().unwrap();
                let (_, v2) = it2.next().unwrap();
                result.in_both.push((k, (v1, v2)));
            }
            Some(Ordering::Greater) => {
                result.only_in_2.push(it2.next().unwrap());
            }
            None => break,
        }
    }

    result
}

fn separate_union_tags(
    subs: &Subs,
    fields1: UnionTags,
    ext1: TagExt,
    fields2: UnionTags,
    ext2: TagExt,
) -> (Separate<TagName, VariableSubsSlice>, TagExt, TagExt) {
    let (it1, new_ext1) = fields1.sorted_slices_iterator_and_ext(subs, ext1);
    let (it2, new_ext2) = fields2.sorted_slices_iterator_and_ext(subs, ext2);

    (separate(it1, it2), new_ext1, new_ext2)
}

#[derive(Debug, Copy, Clone)]
enum Rec {
    None,
    Left(Variable),
    Right(Variable),
    #[allow(dead_code)]
    // dead_code because of https://github.com/roc-lang/roc/pull/6819/files#r1655317562
    Both(Variable, Variable),
}

/// Checks if an extension type `ext1` should be permitted to grow by the `candidate_type`, if it
/// uninhabited.
///
/// This is relevant for the unification strategy of branch and condition
/// types, where we would like
///
/// x : Result Str []
/// when x is
///     Ok x -> x
///
/// to typecheck, because [Ok Str][] = [Ok Str, Result []][] (up to isomorphism).
///
/// Traditionally, such types do not unify because the right-hand side is seen to have a
/// `Result` constructor, but for branches, that constructor is not material.
///
/// Note that such a unification is sound, precisely because `Result` is uninhabited!
///
/// NB: the tag union extension to grow should be `ext1`, and the candidate
/// NB: uninhabited type should be `candidate_type`.
/// of the variables under unification
fn should_extend_ext_with_uninhabited_type(
    subs: &Subs,
    ext1: TagExt,
    candidate_type: Variable,
) -> bool {
    matches!(
        subs.get_content_without_compacting(ext1.var()),
        Content::Structure(FlatType::EmptyTagUnion)
    ) && !subs.is_inhabited(candidate_type)
}

/// After extending an empty tag union extension type [with uninhabited
/// variants][should_extend_ext_with_uninhabited_type], the extension type must be closed again.
fn close_uninhabited_extended_union(subs: &mut Subs, mut var: Variable) {
    loop {
        match subs.get_content_without_compacting(var) {
            Structure(FlatType::EmptyTagUnion) => {
                return;
            }
            FlexVar(..) | FlexAbleVar(..) => {
                subs.set_content_unchecked(var, Structure(FlatType::EmptyTagUnion));
                return;
            }
            Structure(FlatType::TagUnion(_, ext))
            | Structure(FlatType::RecursiveTagUnion(_, _, ext)) => {
                var = ext.var();
            }
            _ => internal_error!("not a tag union"),
        }
    }
}

enum UnifySides<T, U> {
    Left(T, U),
    Right(U, T),
}

#[must_use]
fn unify_tag_ext<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    vars: UnifySides<TagExt, Variable>,
    mode: UnificationMode,
) -> Outcome<M> {
    let (ext, var, flip_for_unify) = match vars {
        UnifySides::Left(ext, var) => (ext, var, false),
        UnifySides::Right(var, ext) => (ext, var, true),
    };
    let legal_unification = match ext {
        TagExt::Openness(_) => {
            // Openness extensions can only unify with flex/rigids.
            // Anything else is a change in the monomorphic size of the tag and not
            // a reflection of the polymorphism of the tag, an error.
            matches!(
                env.get_content_without_compacting(var),
                FlexVar(..)
                    | RigidVar(..)
                    | FlexAbleVar(..)
                    | RigidAbleVar(..)
                    // errors propagate
                    | Error,
            )
        }
        TagExt::Any(_) => true,
    }
    // Tag unions are always extendable during specialization
    || M::IS_LATE;
    if legal_unification {
        if flip_for_unify {
            unify_pool(env, pool, var, ext.var(), mode)
        } else {
            unify_pool(env, pool, ext.var(), var, mode)
        }
    } else {
        mismatch!()
    }
}

#[must_use]
fn merge_tag_exts(ext1: TagExt, ext2: TagExt) -> TagExt {
    match (ext1, ext2) {
        (_, TagExt::Openness(v)) | (TagExt::Openness(v), _) => TagExt::Openness(v),
        (TagExt::Any(v), TagExt::Any(_)) => TagExt::Any(v),
    }
}

#[allow(clippy::too_many_arguments)]
#[must_use]
fn unify_tag_unions<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    tags1: UnionTags,
    initial_ext1: TagExt,
    tags2: UnionTags,
    initial_ext2: TagExt,
) -> Outcome<M> {
    let (separate, mut ext1, mut ext2) =
        separate_union_tags(env, tags1, initial_ext1, tags2, initial_ext2);

    let shared_tags = separate.in_both;

    if let (true, Content::Structure(FlatType::EmptyTagUnion)) =
        (ctx.mode.is_present(), env.get(ext1.var()).content)
    {
        if !separate.only_in_2.is_empty() {
            // Create a new extension variable that we'll fill in with the
            // contents of the tag union from our presence contraint.
            //
            // If there's no new (toplevel) tags we need to register for
            // presence, for example in the cases
            //    [A]      += [A]
            //    [A, B]   += [A]
            //    [A M, B] += [A N]
            // then we don't need to create a fresh ext variable, since the
            // tag union is definitely not growing on the top level.
            // Notice that in the last case
            //    [A M, B] += [A N]
            // the nested tag `A` **will** grow, but we don't need to modify
            // the top level extension variable for that!
            debug_assert!(ext1.is_any());
            let new_ext = TagExt::Any(fresh(env, pool, ctx, Content::FlexVar(None)));
            let new_union = Structure(FlatType::TagUnion(tags1, new_ext));
            let mut new_desc = ctx.first_desc;
            new_desc.content = new_union;
            env.set(ctx.first, new_desc);

            ext1 = new_ext;
        }
    }

    if separate.only_in_1.is_empty() {
        if separate.only_in_2.is_empty() {
            let ext_outcome = if ctx.mode.is_eq() {
                // Neither extension can grow in monomorphic tag sizes,
                // so the extension must either be [] or a polymorphic extension variable like `a`.
                // As such we can always unify directly even if the extensions are measuring
                // openness.
                unify_pool(env, pool, ext1.var(), ext2.var(), ctx.mode)
            } else {
                // In a presence context, we don't care about ext2 being equal to ext1
                Outcome::default()
            };

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut shared_tags_outcome = unify_shared_tags(
                env,
                pool,
                ctx,
                shared_tags,
                OtherTags2::Empty,
                merge_tag_exts(ext1, ext2),
            );

            shared_tags_outcome.union(ext_outcome);

            shared_tags_outcome
        } else {
            let extra_tags_in_2 = {
                let unique_tags2 = UnionTags::insert_slices_into_subs(env, separate.only_in_2);
                let flat_type = FlatType::TagUnion(unique_tags2, ext2);
                fresh(env, pool, ctx, Structure(flat_type))
            };

            // SPECIAL-CASE: if we can grow empty extensions with uninhabited types,
            // patch `ext1` to grow accordingly.
            let extend_ext_with_uninhabited =
                should_extend_ext_with_uninhabited_type(env, ext1, extra_tags_in_2);
            if extend_ext_with_uninhabited {
                debug_assert!(ext1.is_any());
                let new_ext = TagExt::Any(fresh(env, pool, ctx, Content::FlexVar(None)));
                let new_union = Structure(FlatType::TagUnion(tags1, new_ext));
                let mut new_desc = ctx.first_desc;
                new_desc.content = new_union;
                env.set(ctx.first, new_desc);

                ext1 = new_ext;
            }

            let ext_outcome =
                unify_tag_ext(env, pool, UnifySides::Left(ext1, extra_tags_in_2), ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let combined_ext = ext1.map(|_| extra_tags_in_2);

            let mut shared_tags_outcome =
                unify_shared_tags(env, pool, ctx, shared_tags, OtherTags2::Empty, combined_ext);

            shared_tags_outcome.union(ext_outcome);

            if extend_ext_with_uninhabited {
                close_uninhabited_extended_union(env, ctx.first);
            }

            shared_tags_outcome
        }
    } else if separate.only_in_2.is_empty() {
        let extra_tags_in_1 = {
            let unique_tags1 = UnionTags::insert_slices_into_subs(env, separate.only_in_1);
            let flat_type = FlatType::TagUnion(unique_tags1, ext1);
            fresh(env, pool, ctx, Structure(flat_type))
        };

        let mut total_outcome = Outcome::default();

        // In a presence context, we don't care about ext2 being equal to tags1
        let extend_ext_with_uninhabited = if ctx.mode.is_eq() {
            // SPECIAL-CASE: if we can grow empty extensions with uninhabited types,
            // patch `ext2` to grow accordingly.
            let extend_ext_with_uninhabited =
                should_extend_ext_with_uninhabited_type(env, ext2, extra_tags_in_1);
            if extend_ext_with_uninhabited {
                debug_assert!(ext2.is_any());
                let new_ext = TagExt::Any(fresh(env, pool, ctx, Content::FlexVar(None)));
                let new_union = Structure(FlatType::TagUnion(tags2, new_ext));
                let mut new_desc = ctx.second_desc;
                new_desc.content = new_union;
                env.set(ctx.second, new_desc);

                ext2 = new_ext;
            }

            let ext_outcome = unify_tag_ext(
                env,
                pool,
                UnifySides::Right(extra_tags_in_1, ext2),
                ctx.mode,
            );

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }
            total_outcome.union(ext_outcome);

            extend_ext_with_uninhabited
        } else {
            false
        };

        let combined_ext = ext2.map(|_| extra_tags_in_1);

        let shared_tags_outcome =
            unify_shared_tags(env, pool, ctx, shared_tags, OtherTags2::Empty, combined_ext);
        total_outcome.union(shared_tags_outcome);

        if extend_ext_with_uninhabited {
            close_uninhabited_extended_union(env, ctx.first);
        }

        total_outcome
    } else {
        let other_tags = OtherTags2::Union(separate.only_in_1.clone(), separate.only_in_2.clone());

        let unique_tags1 = UnionTags::insert_slices_into_subs(env, separate.only_in_1);
        let unique_tags2 = UnionTags::insert_slices_into_subs(env, separate.only_in_2);

        let ext_content = if ctx.mode.is_present() {
            Content::Structure(FlatType::EmptyTagUnion)
        } else {
            Content::FlexVar(None)
        };
        // If ext1 or ext2 are `Openness`, we will necessarily get an error below when unifying
        // the separate tags since `Openness` cannot grow in terms of width of tags. As such, the
        // unified type, when it exists, must always be include an `Any` extension.
        let ext = TagExt::Any(fresh(env, pool, ctx, ext_content));

        let flat_type1 = FlatType::TagUnion(unique_tags1, ext);
        let flat_type2 = FlatType::TagUnion(unique_tags2, ext);

        let sub1 = fresh(env, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(env, pool, ctx, Structure(flat_type2));

        // NOTE: for clearer error messages, we rollback unification of the ext vars when either fails
        //
        // This is inspired by
        //
        //
        //      f : [Red, Green] -> Bool
        //      f = \_ -> True
        //
        //      f Blue
        //
        //  In this case, we want the mismatch to be between `[Blue]a` and `[Red, Green]`, but
        //  without rolling back, the mismatch is between `[Blue, Red, Green]a` and `[Red, Green]`.
        //  TODO is this also required for the other cases?

        let mut total_outcome = Outcome::default();
        let snapshot = env.snapshot();

        let ext1_outcome = unify_tag_ext(env, pool, UnifySides::Left(ext1, sub2), ctx.mode);
        if !ext1_outcome.mismatches.is_empty() {
            env.rollback_to(snapshot);
            return ext1_outcome;
        }
        total_outcome.union(ext1_outcome);

        if ctx.mode.is_eq() {
            let ext2_outcome = unify_tag_ext(env, pool, UnifySides::Right(sub1, ext2), ctx.mode);
            if !ext2_outcome.mismatches.is_empty() {
                env.rollback_to(snapshot);
                return ext2_outcome;
            }
            total_outcome.union(ext2_outcome);
        }

        env.commit_snapshot(snapshot);

        let shared_tags_outcome = unify_shared_tags(env, pool, ctx, shared_tags, other_tags, ext);
        total_outcome.union(shared_tags_outcome);
        total_outcome
    }
}

#[derive(Debug)]
enum OtherTags2 {
    Empty,
    Union(
        Vec<(TagName, VariableSubsSlice)>,
        Vec<(TagName, VariableSubsSlice)>,
    ),
}

/// Promotes a non-recursive tag union or lambda set to its recursive variant, if it is found to be
/// recursive.
fn maybe_mark_union_recursive(env: &mut Env, pool: &mut Pool, union_var: Variable) {
    'outer: while let Err((_, chain)) = env.occurs(union_var) {
        // walk the chain till we find a tag union or lambda set, starting from the variable that
        // occurred recursively, which is always at the end of the chain.
        for &v in chain.iter().rev() {
            let description = env.get(v);
            match description.content {
                Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                    let rec_var = env.mark_tag_union_recursive(v, tags, ext_var);
                    pool.push(rec_var);

                    continue 'outer;
                }
                LambdaSet(self::LambdaSet {
                    solved,
                    recursion_var: OptVariable::NONE,
                    unspecialized,
                    ambient_function: ambient_function_var,
                }) => {
                    let rec_var = env.mark_lambda_set_recursive(
                        v,
                        solved,
                        unspecialized,
                        ambient_function_var,
                    );
                    pool.push(rec_var);

                    continue 'outer;
                }
                _ => { /* fall through */ }
            }
        }

        // Might not be any tag union if we only pass through `Apply`s. Otherwise, we have a bug!
        if chain.iter().all(|&v| {
            matches!(
                env.get_content_without_compacting(v),
                Content::Structure(FlatType::Apply(..))
            )
        }) {
            return;
        } else {
            // We may seen an occurs check that passes through another recursion var if the occurs
            // check is passing through another recursive type.
            // But, if ROC_VERIFY_OCCURS_ONE_RECURSION is set, we check that we only found a new
            // recursion.
            if dbg_set!(ROC_VERIFY_OCCURS_ONE_RECURSION)
                && !chain.iter().any(|&var| {
                    matches!(
                        env.get_content_without_compacting(var),
                        Content::Structure(FlatType::RecursiveTagUnion(..))
                    )
                })
            {
                internal_error!("recursive loop does not contain a tag union")
            }

            return;
        }
    }
}

fn choose_merged_var(subs: &Subs, var1: Variable, var2: Variable) -> Variable {
    // If one of the variables is a recursion var, keep that one, so that we avoid inlining
    // a recursive tag union type content where we should have a recursion var instead.
    //
    // When might this happen? For example, in the code
    //
    //   Indirect : [Indirect ConsList]
    //
    //   ConsList : [Nil, Cons Indirect]
    //
    //   l : ConsList
    //   l = Cons (Indirect (Cons (Indirect Nil)))
    //   #   ^^^^^^^^^^^^^^^~~~~~~~~~~~~~~~~~~~~~^ region-a
    //   #                  ~~~~~~~~~~~~~~~~~~~~~  region-b
    //   l
    //
    // Suppose `ConsList` has the expanded type `[Nil, Cons [Indirect <rec>]] as <rec>`.
    // After unifying the tag application annotated "region-b" with the recursion variable `<rec>`,
    // we might have that e.g. `actual` is `<rec>` and `expected` is `[Cons (Indirect ...)]`.
    //
    // Now, we need to be careful to set the type we choose to represent the merged type
    // here to be `<rec>`, not the tag union content of `expected`! Otherwise, we will
    // have lost a recursion variable in the recursive tag union.
    //
    // This would not be incorrect from a type perspective, but causes problems later on for e.g.
    // layout generation, which expects recursion variables to be placed correctly. Attempting to detect
    // this during layout generation does not work so well because it may be that there *are* recursive
    // tag unions that should be inlined, and not pass through recursion variables. So instead, resolve
    // these cases here.
    //
    // See tests labeled "issue_2810" for more examples.
    match (
        (var1, subs.get_content_unchecked(var1)),
        (var2, subs.get_content_unchecked(var2)),
    ) {
        ((var, Content::RecursionVar { .. }), _) | (_, (var, Content::RecursionVar { .. })) => var,
        _ => var1,
    }
}

#[inline]
fn find_union_rec(subs: &Subs, ctx: &Context) -> Rec {
    match (
        subs.get_content_without_compacting(ctx.first),
        subs.get_content_without_compacting(ctx.second),
    ) {
        (Structure(s1), Structure(s2)) => match (s1, s2) {
            (FlatType::RecursiveTagUnion(l, _, _), FlatType::RecursiveTagUnion(r, _, _)) => {
                Rec::Both(*l, *r)
            }
            (FlatType::RecursiveTagUnion(l, _, _), _) => Rec::Left(*l),
            (_, FlatType::RecursiveTagUnion(r, _, _)) => Rec::Right(*r),
            _ => Rec::None,
        },
        _ => Rec::None,
    }
}

#[must_use]
fn unify_shared_tags<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    shared_tags: Vec<(TagName, (VariableSubsSlice, VariableSubsSlice))>,
    other_tags: OtherTags2,
    ext: TagExt,
) -> Outcome<M> {
    let mut matching_tags = Vec::default();
    let num_shared_tags = shared_tags.len();

    let mut total_outcome = Outcome::default();

    for (name, (actual_vars, expected_vars)) in shared_tags {
        let mut matching_vars = Vec::with_capacity(actual_vars.len());

        let actual_len = actual_vars.len();
        let expected_len = expected_vars.len();

        for (actual_index, expected_index) in actual_vars.into_iter().zip(expected_vars.into_iter())
        {
            let actual = env[actual_index];
            let expected = env[expected_index];
            // NOTE the arguments of a tag can be recursive. For instance in the expression
            //
            //  ConsList a : [Nil, Cons a (ConsList a)]
            //
            //  Cons 1 (Cons "foo" Nil)
            //
            // We need to not just check the outer layer (inferring ConsList Int)
            // but also the inner layer (finding a type error, as desired)
            //
            // This correction introduces the same issue as in https://github.com/elm/compiler/issues/1964
            // Polymorphic recursion is now a type error.
            //
            // The strategy is to expand the recursive tag union as deeply as the non-recursive one
            // is.
            //
            // > RecursiveTagUnion(rvar, [Cons a rvar, Nil], ext)
            //
            // Conceptually becomes
            //
            // > RecursiveTagUnion(rvar, [Cons a [Cons a rvar, Nil], Nil], ext)
            //
            // and so on until the whole non-recursive tag union can be unified with it.
            //
            // One thing we have to watch out for is that a tag union we're hoping to
            // match a recursive tag union with didn't itself become recursive. If it has,
            // since we're expanding tag unions to equal depths as described above,
            // we'll always pass through this branch. So, we promote tag unions to recursive
            // ones here if it turns out they are that.
            maybe_mark_union_recursive(env, pool, actual);
            maybe_mark_union_recursive(env, pool, expected);

            let mut outcome = Outcome::<M>::default();

            outcome.union(unify_pool(env, pool, actual, expected, ctx.mode));

            if outcome.mismatches.is_empty() {
                let merged_var = choose_merged_var(env, actual, expected);

                matching_vars.push(merged_var);
            }

            total_outcome.union(outcome);
        }

        // only do this check after unification so the error message has more info
        if actual_len == expected_len && actual_len == matching_vars.len() {
            matching_tags.push((name, matching_vars));
        }
    }

    if num_shared_tags == matching_tags.len() {
        // pull fields in from the ext_var

        let (ext_fields, new_ext_var) = UnionTags::default().sorted_iterator_and_ext(env, ext);
        let ext_fields: Vec<_> = ext_fields
            .into_iter()
            .map(|(label, variables)| (label, variables.to_vec()))
            .collect();

        let new_tags: UnionTags = match other_tags {
            OtherTags2::Empty => {
                if ext_fields.is_empty() {
                    UnionTags::insert_into_subs(env, matching_tags)
                } else {
                    let all_fields = merge_sorted(matching_tags, ext_fields);
                    UnionTags::insert_into_subs(env, all_fields)
                }
            }
            OtherTags2::Union(other1, other2) => {
                let mut all_fields = merge_sorted(matching_tags, ext_fields);
                all_fields = merge_sorted(
                    all_fields,
                    other1.into_iter().map(|(field_name, subs_slice)| {
                        let vec = env.get_subs_slice(subs_slice).to_vec();

                        (field_name, vec)
                    }),
                );

                all_fields = merge_sorted(
                    all_fields,
                    other2.into_iter().map(|(field_name, subs_slice)| {
                        let vec = env.get_subs_slice(subs_slice).to_vec();

                        (field_name, vec)
                    }),
                );

                UnionTags::insert_into_subs(env, all_fields)
            }
        };

        // Look up if either unions are recursive, and if so, what the recursive variable is.
        //
        // We wait until we're about to merge the unions to do this, since above, while unifying
        // payloads, we may have promoted a non-recursive union involved in this unification to
        // a recursive one.
        let recursion_var = find_union_rec(env, ctx);

        let merge_outcome = unify_shared_tags_merge(env, ctx, new_tags, new_ext_var, recursion_var);

        total_outcome.union(merge_outcome);
        total_outcome
    } else {
        mismatch!(
            "Problem with Tag Union\nThere should be {:?} matching tags, but I only got \n{:?}",
            num_shared_tags,
            &matching_tags
        )
    }
}

#[must_use]
fn unify_shared_tags_merge<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    new_tags: UnionTags,
    new_ext: TagExt,
    recursion_var: Rec,
) -> Outcome<M> {
    if env.was_fixed(ctx.first) && env.was_fixed(ctx.second) {
        // Both of the tags we're looking at were just involved in fixpoint-fixing, so their types
        // should be aligned. As such, do not attempt to unify them and update the recursion
        // pointer again.
        debug_assert!(env.equivalent_without_compacting(ctx.first, ctx.second));
        return Default::default();
    }

    let flat_type = match recursion_var {
        Rec::None => FlatType::TagUnion(new_tags, new_ext),
        Rec::Left(rec) | Rec::Right(rec) | Rec::Both(rec, _) => {
            debug_assert!(is_recursion_var(env, rec), "{:?}", env.dbg(rec));
            FlatType::RecursiveTagUnion(rec, new_tags, new_ext)
        }
    };

    merge(env, ctx, Structure(flat_type))
}

#[inline(always)]
#[must_use]
fn unify_flat_type<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    left: &FlatType,
    right: &FlatType,
) -> Outcome<M> {
    use roc_types::subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => merge(env, ctx, Structure(*left)),

        (Record(fields, ext), EmptyRecord) if fields.has_only_optional_fields(env) => {
            unify_pool(env, pool, *ext, ctx.second, ctx.mode)
        }

        (EmptyRecord, Record(fields, ext)) if fields.has_only_optional_fields(env) => {
            unify_pool(env, pool, ctx.first, *ext, ctx.mode)
        }

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            unify_record(env, pool, ctx, *fields1, *ext1, *fields2, *ext2)
        }

        (Tuple(elems1, ext1), Tuple(elems2, ext2)) => {
            unify_tuple(env, pool, ctx, *elems1, *ext1, *elems2, *ext2)
        }

        (EmptyTagUnion, EmptyTagUnion) => merge(env, ctx, Structure(*left)),

        (TagUnion(tags, ext), EmptyTagUnion) if tags.is_empty() => {
            unify_pool(env, pool, ext.var(), ctx.second, ctx.mode)
        }

        (EmptyTagUnion, TagUnion(tags, ext)) if tags.is_empty() => {
            unify_pool(env, pool, ctx.first, ext.var(), ctx.mode)
        }

        (TagUnion(tags1, ext1), TagUnion(tags2, ext2)) => {
            unify_tag_unions(env, pool, ctx, *tags1, *ext1, *tags2, *ext2)
        }

        (RecursiveTagUnion(recursion_var, tags1, ext1), TagUnion(tags2, ext2)) => {
            debug_assert!(is_recursion_var(env, *recursion_var));
            // this never happens in type-correct programs, but may happen if there is a type error

            unify_tag_unions(env, pool, ctx, *tags1, *ext1, *tags2, *ext2)
        }

        (TagUnion(tags1, ext1), RecursiveTagUnion(recursion_var, tags2, ext2)) => {
            debug_assert!(is_recursion_var(env, *recursion_var));

            unify_tag_unions(env, pool, ctx, *tags1, *ext1, *tags2, *ext2)
        }

        (RecursiveTagUnion(rec1, tags1, ext1), RecursiveTagUnion(rec2, tags2, ext2)) => {
            debug_assert!(is_recursion_var(env, *rec1), "{:?}", env.dbg(*rec1));
            debug_assert!(is_recursion_var(env, *rec2), "{:?}", env.dbg(*rec2));

            let mut outcome = unify_tag_unions(env, pool, ctx, *tags1, *ext1, *tags2, *ext2);
            outcome.union(unify_pool(env, pool, *rec1, *rec2, ctx.mode));

            outcome
        }

        (Apply(l_symbol, l_args), Apply(r_symbol, r_args)) if l_symbol == r_symbol => {
            let mut outcome = unify_zip_slices(env, pool, *l_args, *r_args, ctx.mode);

            if outcome.mismatches.is_empty() {
                let chosen_args = env.reserve_into_vars(l_args.len());
                for ((store, var1), var2) in chosen_args
                    .into_iter()
                    .zip(l_args.into_iter())
                    .zip(r_args.into_iter())
                {
                    let var1 = env[var1];
                    let var2 = env[var2];
                    env[store] = choose_merged_var(env, var1, var2);
                }

                outcome.union(merge(env, ctx, Structure(Apply(*r_symbol, chosen_args))));
            }

            outcome
        }
        (Func(l_args, l_closure, l_ret, l_fx), Func(r_args, r_closure, r_ret, r_fx))
            if l_args.len() == r_args.len() =>
        {
            let arg_outcome = unify_zip_slices(env, pool, *l_args, *r_args, ctx.mode);
            let ret_outcome = unify_pool(env, pool, *l_ret, *r_ret, ctx.mode);
            let closure_outcome = unify_pool(env, pool, *l_closure, *r_closure, ctx.mode);
            let fx_outcome = unify_pool(env, pool, *l_fx, *r_fx, ctx.mode);

            let mut outcome = ret_outcome;

            outcome.union(closure_outcome);
            outcome.union(arg_outcome);
            outcome.union(fx_outcome);

            if outcome.mismatches.is_empty() {
                let merged_closure_var = choose_merged_var(env, *l_closure, *r_closure);

                outcome.union(merge(
                    env,
                    ctx,
                    Structure(Func(*r_args, merged_closure_var, *r_ret, *r_fx)),
                ));
            }

            outcome
        }
        (EffectfulFunc, Func(args, closure, ret, fx)) => {
            let mut outcome = unify_pool(env, pool, Variable::EFFECTFUL, *fx, ctx.mode);

            outcome.union(merge(
                env,
                ctx,
                Structure(Func(*args, *closure, *ret, Variable::EFFECTFUL)),
            ));

            outcome
        }
        (Func(args, closure, ret, fx), EffectfulFunc) => {
            let mut outcome = unify_pool(env, pool, *fx, Variable::EFFECTFUL, ctx.mode);

            outcome.union(merge(
                env,
                ctx,
                Structure(Func(*args, *closure, *ret, Variable::EFFECTFUL)),
            ));

            outcome
        }
        (FunctionOrTagUnion(tag_names, tag_symbols, ext), Func(args, closure, ret, fx)) => {
            unify_function_or_tag_union_and_func(
                env,
                pool,
                ctx,
                *tag_names,
                *tag_symbols,
                *ext,
                *args,
                *ret,
                *closure,
                *fx,
                true,
            )
        }
        (Func(args, closure, ret, fx), FunctionOrTagUnion(tag_names, tag_symbols, ext)) => {
            unify_function_or_tag_union_and_func(
                env,
                pool,
                ctx,
                *tag_names,
                *tag_symbols,
                *ext,
                *args,
                *ret,
                *closure,
                *fx,
                false,
            )
        }
        (
            FunctionOrTagUnion(tag_names_1, tag_symbols_1, ext1),
            FunctionOrTagUnion(tag_names_2, tag_symbols_2, ext2),
        ) => unify_two_function_or_tag_unions(
            env,
            pool,
            ctx,
            *tag_names_1,
            *tag_symbols_1,
            *ext1,
            *tag_names_2,
            *tag_symbols_2,
            *ext2,
        ),
        (TagUnion(tags1, ext1), FunctionOrTagUnion(tag_names, _, ext2)) => {
            let empty_tag_var_slices = slice_extend_new(
                &mut env.variable_slices,
                std::iter::repeat(Default::default()).take(tag_names.len()),
            );
            let tags2 = UnionTags::from_slices(*tag_names, empty_tag_var_slices);

            unify_tag_unions(env, pool, ctx, *tags1, *ext1, tags2, *ext2)
        }
        (FunctionOrTagUnion(tag_names, _, ext1), TagUnion(tags2, ext2)) => {
            let empty_tag_var_slices = slice_extend_new(
                &mut env.variable_slices,
                std::iter::repeat(Default::default()).take(tag_names.len()),
            );
            let tags1 = UnionTags::from_slices(*tag_names, empty_tag_var_slices);

            unify_tag_unions(env, pool, ctx, tags1, *ext1, *tags2, *ext2)
        }

        (RecursiveTagUnion(recursion_var, tags1, ext1), FunctionOrTagUnion(tag_names, _, ext2)) => {
            // this never happens in type-correct programs, but may happen if there is a type error
            debug_assert!(is_recursion_var(env, *recursion_var));

            let empty_tag_var_slices = slice_extend_new(
                &mut env.variable_slices,
                std::iter::repeat(Default::default()).take(tag_names.len()),
            );
            let tags2 = UnionTags::from_slices(*tag_names, empty_tag_var_slices);

            unify_tag_unions(env, pool, ctx, *tags1, *ext1, tags2, *ext2)
        }

        (FunctionOrTagUnion(tag_names, _, ext1), RecursiveTagUnion(recursion_var, tags2, ext2)) => {
            debug_assert!(is_recursion_var(env, *recursion_var));

            let empty_tag_var_slices = slice_extend_new(
                &mut env.variable_slices,
                std::iter::repeat(Default::default()).take(tag_names.len()),
            );
            let tags1 = UnionTags::from_slices(*tag_names, empty_tag_var_slices);

            unify_tag_unions(env, pool, ctx, tags1, *ext1, *tags2, *ext2)
        }

        // these have underscores because they're unused in --release builds
        (_other1, _other2) => {
            // any other combination is a mismatch
            mismatch!(
                "Trying to unify two flat types that are incompatible: {:?} ~ {:?}",
                roc_types::subs::SubsFmtFlatType(_other1, env),
                roc_types::subs::SubsFmtFlatType(_other2, env)
            )
        }
    }
}

#[must_use]
fn unify_zip_slices<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    left: SubsSlice<Variable>,
    right: SubsSlice<Variable>,
    mode: UnificationMode,
) -> Outcome<M> {
    let mut outcome = Outcome::default();

    let it = left.into_iter().zip(right);

    for (l_index, r_index) in it {
        let l_var = env[l_index];
        let r_var = env[r_index];

        outcome.union(unify_pool(env, pool, l_var, r_var, mode));
    }

    outcome
}

#[inline(always)]
#[must_use]
fn unify_rigid<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    name: &SubsIndex<Lowercase>,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(env, ctx, RigidVar(*name))
        }
        FlexAbleVar(_, other_ability) => {
            // Mismatch - Rigid can unify with FlexAble only when the Rigid has an ability
            // bound as well, otherwise the user failed to correctly annotate the bound.
            mismatch!(
                %not_able, ctx.first, env.get_subs_slice(*other_ability),
                "Rigid {:?} with FlexAble {:?}", ctx.first, other
            )
        }
        RangedNumber(..) => {
            // Int a vs Int <range>, the rigid wins
            merge(env, ctx, RigidVar(*name))
        }

        RigidVar(_)
        | RigidAbleVar(..)
        | RecursionVar { .. }
        | Structure(_)
        | Alias(..)
        | LambdaSet(..)
        | ErasedLambda
        | Pure
        | Effectful => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            mismatch!("Rigid {:?} with {:?}", ctx.first, &other)
        }

        Error => {
            // Error propagates.
            merge(env, ctx, Error)
        }
    }
}

#[inline(always)]
fn abilities_are_superset(superset: &[Symbol], subset: &[Symbol]) -> bool {
    subset.iter().all(|ability| superset.contains(ability))
}

#[inline(always)]
#[must_use]
fn unify_rigid_able<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    name: &SubsIndex<Lowercase>,
    abilities_slice: SubsSlice<Symbol>,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(env, ctx, RigidAbleVar(*name, abilities_slice))
        }
        FlexAbleVar(_, other_abilities_slice) => {
            let (abilities, other_abilities) = (
                env.get_subs_slice(abilities_slice),
                env.get_subs_slice(*other_abilities_slice),
            );

            if abilities_are_superset(abilities, other_abilities) {
                // The rigid has all the ability bounds of the flex, so rigid wins!
                merge(env, ctx, RigidAbleVar(*name, abilities_slice))
            } else {
                // Mismatch for now.
                // TODO check ability hierarchies.
                mismatch!(
                    %not_able, ctx.second, abilities,
                    "RigidAble {:?} with abilities {:?} not compatible with abilities {:?}",
                    ctx.first,
                    abilities,
                    other_abilities,
                )
            }
        }

        RigidVar(_)
        | RigidAbleVar(..)
        | RecursionVar { .. }
        | Structure(_)
        | Alias(..)
        | RangedNumber(..)
        | LambdaSet(..)
        | ErasedLambda
        | Pure
        | Effectful => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            mismatch!("Rigid {:?} with {:?}", ctx.first, &other)
        }

        Error => {
            // Error propagates.
            merge(env, ctx, Error)
        }
    }
}

#[inline(always)]
#[must_use]
fn unify_flex<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    opt_name: &Option<SubsIndex<Lowercase>>,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(other_opt_name) => {
            // Prefer using right's name.
            let opt_name = opt_name.or(*other_opt_name);
            merge(env, ctx, FlexVar(opt_name))
        }

        FlexAbleVar(opt_other_name, ability) => {
            // Prefer using right's name.
            let opt_name = (opt_other_name).or(*opt_name);
            merge(env, ctx, FlexAbleVar(opt_name, *ability))
        }

        RigidVar(_)
        | RigidAbleVar(_, _)
        | RecursionVar { .. }
        | Structure(_)
        | Alias(_, _, _, _)
        | RangedNumber(..)
        | LambdaSet(..)
        | ErasedLambda
        | Pure
        | Effectful => {
            // TODO special-case boolean here
            // In all other cases, if left is flex, defer to right.
            merge(env, ctx, *other)
        }

        Error => merge(env, ctx, Error),
    }
}

// TODO remove once https://github.com/rust-lang/rust/issues/53485 is stabilized
#[cfg(debug_assertions)]
fn is_sorted_dedup<T: Ord>(l: &[T]) -> bool {
    let mut iter = l.iter().peekable();
    while let Some(before) = iter.next() {
        if let Some(after) = iter.peek() {
            if before >= after {
                return false;
            }
        }
    }
    true
}

#[inline(always)]
pub fn merged_ability_slices(
    subs: &mut Subs,
    left_slice: SubsSlice<Symbol>,
    right_slice: SubsSlice<Symbol>,
) -> SubsSlice<Symbol> {
    // INVARIANT: abilities slices are inserted sorted into subs
    let left = subs.get_subs_slice(left_slice);
    let right = subs.get_subs_slice(right_slice);

    #[cfg(debug_assertions)]
    {
        debug_assert!(is_sorted_dedup(left));
        debug_assert!(is_sorted_dedup(right));
    }

    // In practice, ability lists should be very short, so check prefix runs foremost.
    if left.starts_with(right) {
        return left_slice;
    }
    if right.starts_with(left) {
        return right_slice;
    }

    let merged = merge_sorted_keys(left.iter().copied(), right.iter().copied());

    // TODO: check if there's an existing run in subs rather than re-inserting
    slice_extend_new(&mut subs.symbol_names, merged)
}

#[inline(always)]
#[must_use]
fn unify_flex_able<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    opt_name: &Option<SubsIndex<Lowercase>>,
    abilities_slice: SubsSlice<Symbol>,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(opt_other_name) => {
            // Prefer using right's name.
            let opt_name = (opt_other_name).or(*opt_name);
            merge(env, ctx, FlexAbleVar(opt_name, abilities_slice))
        }

        FlexAbleVar(opt_other_name, other_abilities_slice) => {
            // Prefer the right's name when possible.
            let opt_name = (opt_other_name).or(*opt_name);

            let merged_abilities =
                merged_ability_slices(env, abilities_slice, *other_abilities_slice);

            merge(env, ctx, FlexAbleVar(opt_name, merged_abilities))
        }

        RigidAbleVar(_, other_abilities_slice) => {
            let (abilities, other_abilities) = (
                env.get_subs_slice(abilities_slice),
                env.get_subs_slice(*other_abilities_slice),
            );

            if abilities_are_superset(other_abilities, abilities) {
                // Rigid has all the ability bounds of the flex, so rigid wins!
                merge(env, ctx, *other)
            } else {
                mismatch!(%not_able, ctx.second, abilities, "RigidAble {:?} vs {:?}", abilities, other_abilities)
            }
        }

        RigidVar(_) => mismatch!("FlexAble can never unify with non-able Rigid"),
        LambdaSet(..) | ErasedLambda => mismatch!("FlexAble with LambdaSet"),
        Pure | Effectful => mismatch!("FlexAble with fx var"),

        Alias(name, _args, _real_var, AliasKind::Opaque) => {
            // Opaque type wins
            merge_flex_able_with_concrete(
                env,
                ctx,
                ctx.first,
                abilities_slice,
                *other,
                opaque_obligation(*name, ctx.second),
            )
        }

        RecursionVar { .. }
        | Structure(_)
        | Alias(_, _, _, AliasKind::Structural)
        | RangedNumber(..) => {
            // Structural type wins.
            merge_flex_able_with_concrete(
                env,
                ctx,
                ctx.first,
                abilities_slice,
                *other,
                Obligated::Adhoc(ctx.second),
            )
        }

        Error => merge(env, ctx, Error),
    }
}

#[must_use]
fn merge_flex_able_with_concrete<M: MetaCollector>(
    env: &mut Env,
    ctx: &Context,
    flex_able_var: Variable,
    abilities: SubsSlice<Symbol>,
    concrete_content: Content,
    concrete_obligation: Obligated,
) -> Outcome<M> {
    let mut outcome = merge(env, ctx, concrete_content);

    for &ability in env.get_subs_slice(abilities) {
        let must_implement_ability = MustImplementAbility {
            typ: concrete_obligation,
            ability,
        };
        outcome.must_implement_ability.push(must_implement_ability);
    }

    // Figure which, if any, lambda sets should be specialized thanks to the flex able var
    // being instantiated. Now as much as I would love to do that here, we don't, because we might
    // be in the middle of solving a module and not resolved all available ability implementations
    // yet! Instead we chuck it up in the [Outcome] and let our caller do the resolution.
    //
    // If we ever organize ability implementations so that they are well-known before any other
    // unification is done, they can be solved in-band here!
    let uls_of_concrete = env.remove_dependent_unspecialized_lambda_sets(flex_able_var);
    outcome
        .lambda_sets_to_specialize
        .extend(flex_able_var, uls_of_concrete);

    outcome
}

#[inline(always)]
#[must_use]
fn unify_recursion<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    opt_name: &Option<SubsIndex<Lowercase>>,
    structure: Variable,
    other: &Content,
) -> Outcome<M> {
    if env.seen_recursion_pair(ctx.first, ctx.second) {
        return Default::default();
    }

    env.add_recursion_pair(ctx.first, ctx.second);

    let outcome = match other {
        RecursionVar {
            opt_name: other_opt_name,
            structure: other_structure,
        } => {
            // We haven't seen these two recursion vars yet, so go and unify their structures.
            // We need to do this before we merge the two recursion vars, since the unification of
            // the structures may be material.

            let mut outcome = unify_pool(env, pool, structure, *other_structure, ctx.mode);
            if !outcome.mismatches.is_empty() {
                return outcome;
            }

            let name = (*opt_name).or(*other_opt_name);
            let merge_outcome = merge(
                env,
                ctx,
                RecursionVar {
                    opt_name: name,
                    structure,
                },
            );

            outcome.union(merge_outcome);
            outcome
        }

        Structure(_) => {
            // unify the structure variable with this Structure
            unify_pool(env, pool, structure, ctx.second, ctx.mode)
        }
        RigidVar(_) => {
            mismatch!("RecursionVar {:?} with rigid {:?}", ctx.first, &other)
        }

        RigidAbleVar(..) => {
            mismatch!("RecursionVar {:?} with able var {:?}", ctx.first, &other)
        }

        FlexAbleVar(_, ability) => merge_flex_able_with_concrete(
            env,
            ctx,
            ctx.second,
            *ability,
            RecursionVar {
                structure,
                opt_name: *opt_name,
            },
            Obligated::Adhoc(ctx.first),
        ),

        FlexVar(_) => merge(
            env,
            ctx,
            RecursionVar {
                structure,
                opt_name: *opt_name,
            },
        ),

        Alias(_, _, actual, AliasKind::Structural) => {
            // look at the type the alias stands for
            unify_pool(env, pool, ctx.first, *actual, ctx.mode)
        }

        Alias(_, _, _, AliasKind::Opaque) => {
            // look at the type the recursion var stands for
            unify_pool(env, pool, structure, ctx.second, ctx.mode)
        }

        RangedNumber(..) => mismatch!(
            "RecursionVar {:?} with ranged number {:?}",
            ctx.first,
            &other
        ),

        LambdaSet(..) => {
            debug_assert!(!M::UNIFYING_SPECIALIZATION);

            // suppose that the recursion var is a lambda set
            unify_pool(env, pool, structure, ctx.second, ctx.mode)
        }

        ErasedLambda => mismatch!(),

        Pure | Effectful => mismatch!("RecursionVar with fx var"),

        Error => merge(env, ctx, Error),
    };

    env.remove_recursion_pair(ctx.first, ctx.second);

    outcome
}

#[must_use]
pub fn merge<M: MetaCollector>(env: &mut Env, ctx: &Context, content: Content) -> Outcome<M> {
    let mut outcome: Outcome<M> = Outcome::default();

    let rank = ctx.first_desc.rank.min(ctx.second_desc.rank);
    let desc = Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    env.union(ctx.first, ctx.second, desc);

    outcome.has_changed = true;
    outcome
}

fn register(env: &mut Env, desc: Descriptor, pool: &mut Pool) -> Variable {
    let var = env.fresh(desc);

    pool.push(var);

    var
}

fn fresh(env: &mut Env, pool: &mut Pool, ctx: &Context, content: Content) -> Variable {
    register(
        env,
        Descriptor {
            content,
            rank: ctx.first_desc.rank.min(ctx.second_desc.rank),
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        },
        pool,
    )
}

fn is_recursion_var(subs: &Subs, var: Variable) -> bool {
    matches!(
        subs.get_content_without_compacting(var),
        Content::RecursionVar { .. }
    ) ||
        // Error-like vars should always unify, so pretend they are recursion vars too.
        subs.is_error_var(var)
}

#[allow(clippy::too_many_arguments)]
#[must_use]
fn unify_function_or_tag_union_and_func<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    tag_names_slice: SubsSlice<TagName>,
    tag_fn_lambdas: SubsSlice<Symbol>,
    tag_ext: TagExt,
    function_arguments: VariableSubsSlice,
    function_return: Variable,
    function_lambda_set: Variable,
    function_fx: Variable,
    left: bool,
) -> Outcome<M> {
    let tag_names = env.get_subs_slice(tag_names_slice).to_vec();

    let union_tags = UnionTags::insert_slices_into_subs(
        env,
        tag_names.into_iter().map(|tag| (tag, function_arguments)),
    );
    let content = Content::Structure(FlatType::TagUnion(union_tags, tag_ext));

    let new_tag_union_var = fresh(env, pool, ctx, content);

    let mut outcome = if left {
        unify_pool(env, pool, new_tag_union_var, function_return, ctx.mode)
    } else {
        unify_pool(env, pool, function_return, new_tag_union_var, ctx.mode)
    };

    outcome.union(if left {
        unify_pool(env, pool, Variable::PURE, function_fx, ctx.mode)
    } else {
        unify_pool(env, pool, function_fx, Variable::PURE, ctx.mode)
    });

    {
        let lambda_names = env.get_subs_slice(tag_fn_lambdas).to_vec();
        let new_lambda_names = slice_extend_new(&mut env.symbol_names, lambda_names);
        let empty_captures_slices = slice_extend_new(
            &mut env.variable_slices,
            std::iter::repeat(Default::default()).take(new_lambda_names.len()),
        );
        let union_tags = UnionLambdas::from_slices(new_lambda_names, empty_captures_slices);

        let ambient_function_var = if left { ctx.first } else { ctx.second };
        let lambda_set_content = LambdaSet(self::LambdaSet {
            solved: union_tags,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: ambient_function_var,
        });

        let tag_lambda_set = register(
            env,
            Descriptor {
                content: lambda_set_content,
                rank: ctx.first_desc.rank.min(ctx.second_desc.rank),
                mark: Mark::NONE,
                copy: OptVariable::NONE,
            },
            pool,
        );

        let closure_outcome = if left {
            unify_pool(env, pool, tag_lambda_set, function_lambda_set, ctx.mode)
        } else {
            unify_pool(env, pool, function_lambda_set, tag_lambda_set, ctx.mode)
        };

        outcome.union(closure_outcome);
    }

    if outcome.mismatches.is_empty() {
        let desc = if left {
            env.get(ctx.second)
        } else {
            env.get(ctx.first)
        };

        outcome.union(merge(env, ctx, desc.content));
    }

    outcome
}

#[allow(clippy::too_many_arguments)]
fn unify_two_function_or_tag_unions<M: MetaCollector>(
    env: &mut Env,
    pool: &mut Pool,
    ctx: &Context,
    tag_names_1: SubsSlice<TagName>,
    tag_symbols_1: SubsSlice<Symbol>,
    ext1: TagExt,
    tag_names_2: SubsSlice<TagName>,
    tag_symbols_2: SubsSlice<Symbol>,
    ext2: TagExt,
) -> Outcome<M> {
    let merged_tags = {
        let mut all_tags: Vec<_> = (env.get_subs_slice(tag_names_1).iter())
            .chain(env.get_subs_slice(tag_names_2))
            .cloned()
            .collect();
        all_tags.sort();
        all_tags.dedup();
        slice_extend_new(&mut env.tag_names, all_tags)
    };
    let merged_lambdas = {
        let mut all_lambdas: Vec<_> = (env.get_subs_slice(tag_symbols_1).iter())
            .chain(env.get_subs_slice(tag_symbols_2))
            .cloned()
            .collect();
        all_lambdas.sort();
        all_lambdas.dedup();
        slice_extend_new(&mut env.symbol_names, all_lambdas)
    };

    let mut outcome = unify_pool(env, pool, ext1.var(), ext2.var(), ctx.mode);
    if !outcome.mismatches.is_empty() {
        return outcome;
    }

    let merge_outcome = merge(
        env,
        ctx,
        Content::Structure(FlatType::FunctionOrTagUnion(
            merged_tags,
            merged_lambdas,
            ext1,
        )),
    );

    outcome.union(merge_outcome);
    outcome
}

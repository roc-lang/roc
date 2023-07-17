use bumpalo::Bump;
use roc_can::{constraint::Constraints, module::ExposedByModule};
use roc_checkmate::with_checkmate;
use roc_derive::SharedDerivedModule;
use roc_types::subs::{Content, Descriptor, Mark, OptVariable, Rank, Subs, Variable};
use roc_unify::Env as UEnv;

use crate::{FunctionKind, Pools};

pub struct DerivedEnv<'a> {
    pub derived_module: &'a SharedDerivedModule,
    /// Exposed types needed by the derived module.
    pub exposed_types: &'a ExposedByModule,
}

/// Environment necessary for solving and specialization.
pub struct SolveEnv<'a> {
    pub arena: &'a Bump,
    pub derived_env: &'a DerivedEnv<'a>,
    pub subs: &'a mut Subs,
    pub pools: &'a mut Pools,
    #[cfg(debug_assertions)]
    pub checkmate: &'a mut Option<roc_checkmate::Collector>,
}

/// Environment necessary for inference.
pub struct InferenceEnv<'a> {
    pub constraints: &'a Constraints,
    pub function_kind: FunctionKind,
    pub arena: &'a Bump,
    pub derived_env: &'a DerivedEnv<'a>,
    pub subs: &'a mut Subs,
    pub pools: &'a mut Pools,
    #[cfg(debug_assertions)]
    pub checkmate: Option<roc_checkmate::Collector>,
}

impl<'a> SolveEnv<'a> {
    /// Introduce some variables to Pools at the given rank.
    /// Also, set each of their ranks in Subs to be the given rank.
    pub fn introduce(&mut self, rank: Rank, vars: &[Variable]) {
        introduce(self.subs, self.pools, rank, vars);
    }

    /// Retrieves an environment for unification.
    pub fn uenv(&mut self) -> UEnv {
        with_checkmate!({
            on => UEnv::new(self.subs, self.checkmate.as_mut()),
            off => UEnv::new(self.subs),
        })
    }
}

impl<'a> InferenceEnv<'a> {
    #[inline(always)]
    pub fn register(&mut self, rank: Rank, content: Content) -> Variable {
        let descriptor = Descriptor {
            content,
            rank,
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        };

        let var = self.subs.fresh(descriptor);

        self.pools.get_mut(rank).push(var);

        var
    }

    /// Introduce some variables to Pools at the given rank.
    /// Also, set each of their ranks in Subs to be the given rank.
    pub fn introduce(&mut self, rank: Rank, vars: &[Variable]) {
        introduce(self.subs, self.pools, rank, vars);
    }

    #[inline(always)]
    pub fn register_existing_var(&mut self, var: Variable) {
        self.pools.get_mut(self.subs.get_rank(var)).push(var);
    }

    pub fn register_with_known_var(
        &mut self,
        var: Variable,
        rank: Rank,
        content: Content,
    ) -> Variable {
        let descriptor = Descriptor {
            content,
            rank,
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        };

        self.subs.set(var, descriptor);

        self.pools.get_mut(rank).push(var);

        var
    }

    /// Retrieves an environment for unification.
    pub fn uenv(&mut self) -> UEnv {
        with_checkmate!({
            on => UEnv::new(self.subs, self.checkmate.as_mut()),
            off => UEnv::new(self.subs),
        })
    }

    pub fn as_solve_env(&mut self) -> SolveEnv {
        SolveEnv {
            arena: self.arena,
            derived_env: self.derived_env,
            subs: self.subs,
            pools: self.pools,
            #[cfg(debug_assertions)]
            checkmate: &mut self.checkmate,
        }
    }
}

/// Introduce some variables to Pools at the given rank.
/// Also, set each of their ranks in Subs to be the given rank.
fn introduce(subs: &mut Subs, pools: &mut Pools, rank: Rank, vars: &[Variable]) {
    let pool: &mut Vec<Variable> = pools.get_mut(rank);

    for &var in vars.iter() {
        subs.set_rank(var, rank);
    }

    pool.extend(vars);
}

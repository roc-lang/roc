use bumpalo::Bump;
use roc_can::{constraint::Constraints, module::ExposedByModule};
use roc_derive::SharedDerivedModule;
use roc_types::subs::{Content, Descriptor, Mark, OptVariable, Rank, Subs, Variable};

use crate::Pools;

pub struct DerivedEnv<'a> {
    pub derived_module: &'a SharedDerivedModule,
    /// Exposed types needed by the derived module.
    pub exposed_types: &'a ExposedByModule,
}

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub constraints: &'a Constraints,
    pub derived_env: &'a DerivedEnv<'a>,
    pub subs: &'a mut Subs,
    pub pools: &'a mut Pools,
}

impl<'a> Env<'a> {
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
        let pool: &mut Vec<Variable> = self.pools.get_mut(rank);

        for &var in vars.iter() {
            self.subs.set_rank(var, rank);
        }

        pool.extend(vars);
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
}

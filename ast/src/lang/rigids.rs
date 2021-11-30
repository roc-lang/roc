use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasherDefault,
};

use crate::mem_pool::{
    pool::Pool, pool_str::PoolStr, pool_vec::PoolVec, shallow_clone::ShallowClone,
};
use roc_collections::all::WyHash;
use roc_module::ident::Lowercase;
use roc_types::subs::Variable;

#[derive(Debug)]
pub struct Rigids {
    pub names: PoolVec<(Option<PoolStr>, Variable)>, // 8B
    padding: [u8; 1],
}

#[allow(clippy::needless_collect)]
impl Rigids {
    pub fn new(
        named: HashMap<Lowercase, Variable, BuildHasherDefault<WyHash>>,
        unnamed: HashSet<Variable, BuildHasherDefault<WyHash>>,
        pool: &mut Pool,
    ) -> Self {
        let names = PoolVec::with_capacity((named.len() + unnamed.len()) as u32, pool);

        let mut temp_names = Vec::new();

        temp_names.extend(named.iter().map(|(name, var)| (Some(name.as_str()), *var)));

        temp_names.extend(unnamed.iter().map(|var| (None, *var)));

        for (node_id, (opt_name, variable)) in names.iter_node_ids().zip(temp_names) {
            let poolstr = opt_name.map(|name| PoolStr::new(name, pool));

            pool[node_id] = (poolstr, variable);
        }

        Self {
            names,
            padding: Default::default(),
        }
    }

    pub fn named(&self, pool: &mut Pool) -> PoolVec<(PoolStr, Variable)> {
        let named = self
            .names
            .iter(pool)
            .filter_map(|(opt_pool_str, var)| {
                opt_pool_str.as_ref().map(|pool_str| (*pool_str, *var))
            })
            .collect::<Vec<(PoolStr, Variable)>>();

        PoolVec::new(named.into_iter(), pool)
    }

    pub fn unnamed(&self, pool: &mut Pool) -> PoolVec<Variable> {
        let unnamed = self
            .names
            .iter(pool)
            .filter_map(|(opt_pool_str, var)| {
                if opt_pool_str.is_none() {
                    Some(*var)
                } else {
                    None
                }
            })
            .collect::<Vec<Variable>>();

        PoolVec::new(unnamed.into_iter(), pool)
    }
}

impl ShallowClone for Rigids {
    fn shallow_clone(&self) -> Self {
        Self {
            names: self.names.shallow_clone(),
            padding: self.padding,
        }
    }
}

use crate::{
    lang::core::{def::def::References, types::Alias},
    mem_pool::pool::NodeId,
};
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;

use super::introduced_vars::IntroducedVariables;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub introduced_variables: IntroducedVariables,
    pub aliases: MutMap<Symbol, NodeId<Alias>>,
    pub non_closures: MutSet<Symbol>,
}

impl Output {
    pub fn union(&mut self, other: Self) {
        self.references.union_mut(other.references);

        if let (None, Some(later)) = (self.tail_call, other.tail_call) {
            self.tail_call = Some(later);
        }

        self.aliases.extend(other.aliases);
        self.non_closures.extend(other.non_closures);
    }
}

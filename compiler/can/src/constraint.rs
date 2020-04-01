use crate::expected::{Expected, PExpected};
use roc_collections::all::{ImMap, ImSet, SendMap};
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, Category, PatternCategory, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Type, Expected<Type>, Category, Region),
    Lookup(Symbol, Expected<Type>, Region),
    Pattern(Region, PatternCategory, Type, PExpected<Type>),
    True, // Used for things that always unify, e.g. blanks and runtime errors
    SaveTheEnvironment,
    Let(Box<LetConstraint>),
    And(Vec<Constraint>),
}

impl Constraint {
    pub fn instantiate_aliases(&mut self, var_store: &VarStore) {
        Self::instantiate_aliases_help(self, &ImMap::default(), var_store, &mut ImSet::default())
    }

    fn instantiate_aliases_help(
        &mut self,
        aliases: &ImMap<Symbol, Alias>,
        var_store: &VarStore,
        introduced: &mut ImSet<Variable>,
    ) {
        use Constraint::*;

        match self {
            True | SaveTheEnvironment => {}

            Eq(typ, expected, _, _) => {
                expected
                    .get_type_mut_ref()
                    .instantiate_aliases(aliases, var_store, introduced);
                typ.instantiate_aliases(aliases, var_store, introduced);
            }

            Lookup(_, expected, _) => {
                expected
                    .get_type_mut_ref()
                    .instantiate_aliases(aliases, var_store, introduced);
            }

            Pattern(_, _, typ, pexpected) => {
                pexpected
                    .get_type_mut_ref()
                    .instantiate_aliases(aliases, var_store, introduced);
                typ.instantiate_aliases(aliases, var_store, introduced);
            }

            And(nested) => {
                for c in nested.iter_mut() {
                    c.instantiate_aliases_help(aliases, var_store, introduced);
                }
            }

            Let(letcon) => {
                let mut new_aliases = aliases.clone();
                for (k, v) in letcon.def_aliases.iter() {
                    new_aliases.insert(*k, v.clone());
                }

                let mut introduced = ImSet::default();
                for Located { value: typ, .. } in letcon.def_types.iter_mut() {
                    typ.instantiate_aliases(&new_aliases, var_store, &mut introduced);
                }

                letcon.defs_constraint.instantiate_aliases_help(
                    &new_aliases,
                    var_store,
                    &mut introduced,
                );
                letcon.ret_constraint.instantiate_aliases_help(
                    &new_aliases,
                    var_store,
                    &mut introduced,
                );

                letcon.flex_vars.extend(introduced);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetConstraint {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
    pub def_types: SendMap<Symbol, Located<Type>>,
    pub def_aliases: SendMap<Symbol, Alias>,
    pub defs_constraint: Constraint,
    pub ret_constraint: Constraint,
}

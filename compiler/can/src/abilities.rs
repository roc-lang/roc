use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;
use roc_types::types::Type;

use crate::annotation::HasClause;

/// Stores information about an ability member definition, including the parent ability, the
/// defining type, and what type variables need to be instantiated with instances of the ability.
#[derive(Debug)]
struct AbilityMemberData {
    #[allow(unused)]
    parent_ability: Symbol,
    #[allow(unused)]
    signature: Type,
    #[allow(unused)]
    bound_has_clauses: Vec<HasClause>,
}

/// Stores information about what abilities exist in a scope, what it means to implement an
/// ability, and what types implement them.
// TODO(abilities): this should probably go on the Scope, I don't put it there for now because we
// are only dealing with inter-module abilities for now.
#[derive(Default, Debug)]
pub struct AbilitiesStore {
    /// Maps an ability to the members defining it.
    #[allow(unused)]
    members_of_ability: MutMap<Symbol, Vec<Symbol>>,

    /// Information about all members composing abilities.
    #[allow(unused)]
    ability_members: MutMap<Symbol, AbilityMemberData>,

    /// Tuples of (type, member) specifying that `type` declares an implementation of an ability
    /// member `member`.
    #[allow(unused)]
    declared_implementations: MutSet<(Symbol, Symbol)>,

    /// Cache of all ability member names in scope.
    ability_member_symbols: MutSet<Symbol>,
}

impl AbilitiesStore {
    pub fn register_ability(
        &mut self,
        ability: Symbol,
        members: Vec<(Symbol, Type, Vec<HasClause>)>,
    ) {
        let mut members_vec = Vec::with_capacity(members.len());
        for (member, signature, bound_has_clauses) in members.into_iter() {
            members_vec.push(member);
            let old_member = self.ability_members.insert(
                member,
                AbilityMemberData {
                    parent_ability: ability,
                    signature,
                    bound_has_clauses,
                },
            );
            debug_assert!(old_member.is_none(), "Replacing existing member definition");

            let old_member = self.ability_member_symbols.insert(member);
            debug_assert!(!old_member, "Replacing existing member entry");
        }
        let old_ability = self.members_of_ability.insert(ability, members_vec);
        debug_assert!(
            old_ability.is_none(),
            "Replacing existing ability definition"
        );
    }

    pub fn register_implementation(&mut self, implementing_type: Symbol, ability_member: Symbol) {
        let old_impl = self
            .declared_implementations
            .insert((implementing_type, ability_member));
        debug_assert!(!old_impl, "Replacing existing implementation");
    }

    pub fn is_ability_member_name(&self, name: Symbol) -> bool {
        self.ability_member_symbols.contains(&name)
    }
}

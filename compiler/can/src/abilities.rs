use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::{subs::Variable, types::Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberVariables {
    pub able_vars: Vec<Variable>,
    /// This includes - named rigid vars, lambda sets, wildcards. See
    /// [`crate::annotation::IntroducedVariables::collect_rigid`].
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
}

/// Stores information about an ability member definition, including the parent ability, the
/// defining type, and what type variables need to be instantiated with instances of the ability.
// TODO: SoA and put me in an arena
#[derive(Debug, Clone)]
pub struct AbilityMemberData {
    pub parent_ability: Symbol,
    pub signature_var: Variable,
    pub signature: Type,
    pub variables: MemberVariables,
    pub region: Region,
}

/// A particular specialization of an ability member.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemberSpecialization {
    pub symbol: Symbol,
    pub region: Region,
}

/// Stores information about what abilities exist in a scope, what it means to implement an
/// ability, and what types implement them.
// TODO(abilities): this should probably go on the Scope, I don't put it there for now because we
// are only dealing with inter-module abilities for now.
#[derive(Default, Debug, Clone)]
pub struct AbilitiesStore {
    /// Maps an ability to the members defining it.
    members_of_ability: MutMap<Symbol, Vec<Symbol>>,

    /// Information about all members composing abilities.
    ability_members: MutMap<Symbol, AbilityMemberData>,

    /// Map of symbols that specialize an ability member to the root ability symbol name.
    /// For example, for the program
    ///   Hash has hash : a -> U64 | a has Hash
    ///            ^^^^ gets the symbol "#hash"
    ///   hash = \@Id n -> n
    ///   ^^^^ gets the symbol "#hash1"
    ///
    /// We keep the mapping #hash1->#hash
    specialization_to_root: MutMap<Symbol, Symbol>,

    /// Maps a tuple (member, type) specifying that `type` declares an implementation of an ability
    /// member `member`, to the exact symbol that implements the ability.
    declared_specializations: MutMap<(Symbol, Symbol), MemberSpecialization>,
}

impl AbilitiesStore {
    /// Records the definition of an ability, including its members.
    pub fn register_ability(
        &mut self,
        ability: Symbol,
        members: Vec<(Symbol, Region, Variable, Type, MemberVariables)>,
    ) {
        let mut members_vec = Vec::with_capacity(members.len());
        for (member, region, signature_var, signature, variables) in members.into_iter() {
            members_vec.push(member);
            let old_member = self.ability_members.insert(
                member,
                AbilityMemberData {
                    parent_ability: ability,
                    signature_var,
                    signature,
                    region,
                    variables,
                },
            );
            debug_assert!(old_member.is_none(), "Replacing existing member definition");
        }
        let old_ability = self.members_of_ability.insert(ability, members_vec);
        debug_assert!(
            old_ability.is_none(),
            "Replacing existing ability definition"
        );
    }

    pub fn is_ability(&self, ability: Symbol) -> bool {
        self.members_of_ability.contains_key(&ability)
    }

    /// Records a specialization of `ability_member` with specialized type `implementing_type`.
    /// Entries via this function are considered a source of truth. It must be ensured that a
    /// specialization is validated before being registered here.
    pub fn register_specialization_for_type(
        &mut self,
        ability_member: Symbol,
        implementing_type: Symbol,
        specialization: MemberSpecialization,
    ) {
        let old_spec = self
            .declared_specializations
            .insert((ability_member, implementing_type), specialization);
        debug_assert!(old_spec.is_none(), "Replacing existing specialization");
    }

    /// Checks if `name` is a root ability member symbol name.
    /// Note that this will return `false` for specializations of an ability member, which have
    /// different symbols from the root.
    pub fn is_ability_member_name(&self, name: Symbol) -> bool {
        self.ability_members.contains_key(&name)
    }

    /// Returns information about all known ability members and their root symbols.
    pub fn root_ability_members(&self) -> &MutMap<Symbol, AbilityMemberData> {
        &self.ability_members
    }

    /// Records that the symbol `specializing_symbol` claims to specialize `ability_member`; for
    /// example the symbol of `hash : Id -> U64` specializing `hash : a -> U64 | a has Hash`.
    pub fn register_specializing_symbol(
        &mut self,
        specializing_symbol: Symbol,
        ability_member: Symbol,
    ) {
        self.specialization_to_root
            .insert(specializing_symbol, ability_member);
    }

    /// Returns whether a symbol is declared to specialize an ability member.
    pub fn is_specialization_name(&self, symbol: Symbol) -> bool {
        self.specialization_to_root.contains_key(&symbol)
    }

    /// Finds the symbol name and ability member definition for a symbol specializing the ability
    /// member, if it specializes any.
    /// For example, suppose `hash : Id -> U64` has symbol #hash1 and specializes
    /// `hash : a -> U64 | a has Hash` with symbol #hash. Calling this with #hash1 would retrieve
    /// the ability member data for #hash.
    pub fn root_name_and_def(
        &self,
        specializing_symbol: Symbol,
    ) -> Option<(Symbol, &AbilityMemberData)> {
        let root_symbol = self.specialization_to_root.get(&specializing_symbol)?;
        debug_assert!(self.ability_members.contains_key(root_symbol));
        let root_data = self.ability_members.get(root_symbol).unwrap();
        Some((*root_symbol, root_data))
    }

    /// Finds the ability member definition for a member name.
    pub fn member_def(&self, member: Symbol) -> Option<&AbilityMemberData> {
        self.ability_members.get(&member)
    }

    /// Returns an iterator over pairs (ability member, type) specifying that
    /// "ability member" has a specialization with type "type".
    pub fn get_known_specializations(&self) -> impl Iterator<Item = (Symbol, Symbol)> + '_ {
        self.declared_specializations.keys().copied()
    }

    /// Retrieves the specialization of `member` for `typ`, if it exists.
    pub fn get_specialization(&self, member: Symbol, typ: Symbol) -> Option<MemberSpecialization> {
        self.declared_specializations.get(&(member, typ)).copied()
    }

    /// Returns pairs of (type, ability member) specifying that "ability member" has a
    /// specialization with type "type".
    pub fn members_of_ability(&self, ability: Symbol) -> Option<&[Symbol]> {
        self.members_of_ability.get(&ability).map(|v| v.as_ref())
    }
}

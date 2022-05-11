use roc_collections::{all::MutMap, VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::{subs::Variable, types::Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberVariables {
    pub able_vars: Vec<Variable>,
    /// This includes - named rigid vars, lambda sets, wildcards. See
    /// [`IntroducedVariables::collect_rigid`](crate::annotation::IntroducedVariables::collect_rigid).
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub enum MemberTypeInfo {
    /// The member and its signature is defined locally, in the module the store is created for.
    /// We need to instantiate and introduce this during solving.
    Local {
        signature_var: Variable,
        signature: Type,
        variables: MemberVariables,
    },
    /// The member was defined in another module, so we'll import its variable when it's time to
    /// solve. At that point we'll resolve `var` here.
    Imported { signature_var: Option<Variable> },
}

/// Stores information about an ability member definition, including the parent ability, the
/// defining type, and what type variables need to be instantiated with instances of the ability.
// TODO: SoA and put me in an arena
#[derive(Debug, Clone)]
pub struct AbilityMemberData {
    pub parent_ability: Symbol,
    pub region: Region,
    pub typ: MemberTypeInfo,
}

impl AbilityMemberData {
    pub fn signature_var(&self) -> Option<Variable> {
        match self.typ {
            MemberTypeInfo::Local { signature_var, .. } => Some(signature_var),
            MemberTypeInfo::Imported { signature_var } => signature_var,
        }
    }
}

pub type SolvedSpecializations = VecMap<(Symbol, Symbol), MemberSpecialization>;

/// A particular specialization of an ability member.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemberSpecialization {
    pub symbol: Symbol,
    pub region: Region,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpecializationId(u32);

#[allow(clippy::derivable_impls)] // let's be explicit about this
impl Default for SpecializationId {
    fn default() -> Self {
        Self(0)
    }
}

/// Stores information about what abilities exist in a scope, what it means to implement an
/// ability, and what types implement them.
// TODO(abilities): this should probably go on the Scope, I don't put it there for now because we
// are only dealing with intra-module abilities for now.
// TODO(abilities): many of these should be `VecMap`s. Do some benchmarking.
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
    declared_specializations: SolvedSpecializations,

    next_specialization_id: u32,

    /// Resolved specializations for a symbol. These might be ephemeral (known due to type solving),
    /// or resolved on-the-fly during mono.
    resolved_specializations: MutMap<SpecializationId, Symbol>,
}

impl AbilitiesStore {
    /// Records the definition of an ability, including its members.
    pub fn register_ability<I>(&mut self, ability: Symbol, members: I)
    where
        I: IntoIterator<Item = (Symbol, AbilityMemberData)>,
        I::IntoIter: ExactSizeIterator,
    {
        let members = members.into_iter();
        let mut members_vec = Vec::with_capacity(members.len());
        for (member, member_data) in members {
            members_vec.push(member);
            let old_member = self.ability_members.insert(member, member_data);
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

    /// Returns an iterator over pairs ((ability member, type), specialization) specifying that
    /// "ability member" has a "specialization" for type "type".
    pub fn iter_specializations(
        &self,
    ) -> impl Iterator<Item = ((Symbol, Symbol), MemberSpecialization)> + '_ {
        self.declared_specializations.iter().map(|(k, v)| (*k, *v))
    }

    /// Retrieves the specialization of `member` for `typ`, if it exists.
    pub fn get_specialization(&self, member: Symbol, typ: Symbol) -> Option<MemberSpecialization> {
        self.declared_specializations.get(&(member, typ)).copied()
    }

    pub fn members_of_ability(&self, ability: Symbol) -> Option<&[Symbol]> {
        self.members_of_ability.get(&ability).map(|v| v.as_ref())
    }

    pub fn fresh_specialization_id(&mut self) -> SpecializationId {
        debug_assert!(self.next_specialization_id != std::u32::MAX);

        let id = SpecializationId(self.next_specialization_id);
        self.next_specialization_id += 1;
        id
    }

    pub fn insert_resolved(&mut self, id: SpecializationId, specialization: Symbol) {
        debug_assert!(self.is_specialization_name(specialization));

        let old_specialization = self.resolved_specializations.insert(id, specialization);

        debug_assert!(
            old_specialization.is_none(),
            "Existing resolution: {:?}",
            old_specialization
        );
    }

    pub fn remove_resolved(&mut self, id: SpecializationId) {
        let old_specialization = self.resolved_specializations.remove(&id);

        debug_assert!(
            old_specialization.is_some(),
            "Trying to remove a resolved specialization that was never there!",
        );
    }

    pub fn get_resolved(&self, id: SpecializationId) -> Option<Symbol> {
        self.resolved_specializations.get(&id).copied()
    }

    /// Creates a store from [`self`] that closes over the abilities/members given by the
    /// imported `symbols`, and their specializations (if any).
    pub fn closure_from_imported(&self, symbols: &VecSet<Symbol>) -> Self {
        let Self {
            members_of_ability,
            ability_members,
            declared_specializations,

            // Covered by `declared_specializations`
            specialization_to_root: _,

            // Taking closure for a new module, so specialization IDs can be fresh
            next_specialization_id: _,
            resolved_specializations: _,
        } = self;

        let mut new = Self::default();

        // 1. Figure out the abilities we need to introduce.
        let mut abilities_to_introduce = VecSet::with_capacity(2);
        symbols.iter().for_each(|symbol| {
            if let Some(member_data) = ability_members.get(symbol) {
                // If the symbol is member of an ability, we need to capture the entire ability.
                abilities_to_introduce.insert(member_data.parent_ability);
            }
            if members_of_ability.contains_key(symbol) {
                abilities_to_introduce.insert(*symbol);
            }
        });

        // 2. Add each ability, and any specializations of its members we know about.
        for ability in abilities_to_introduce.into_iter() {
            let members = members_of_ability.get(&ability).unwrap();
            let mut imported_member_data = Vec::with_capacity(members.len());
            for member in members {
                let mut member_data = ability_members.get(member).unwrap().clone();
                // All external members need to be marked as imported. We'll figure out their real
                // type variables when it comes time to solve the module we're currently importing
                // into.
                member_data.typ = MemberTypeInfo::Imported {
                    signature_var: None,
                };

                imported_member_data.push((*member, member_data));
            }

            new.register_ability(ability, imported_member_data);

            // Add any specializations of the ability's members we know about.
            declared_specializations
                .iter()
                .filter(|((member, _), _)| members.contains(member))
                .for_each(|(&(member, typ), &specialization)| {
                    new.register_specializing_symbol(specialization.symbol, member);
                    new.register_specialization_for_type(member, typ, specialization);
                });
        }

        new
    }

    pub fn union(&mut self, other: Self) {
        let Self {
            members_of_ability: other_members_of_ability,
            ability_members: mut other_ability_members,
            specialization_to_root,
            declared_specializations,
            next_specialization_id,
            resolved_specializations,
        } = other;

        for (ability, members) in other_members_of_ability.into_iter() {
            if let Some(my_members) = self.members_of_ability(ability) {
                debug_assert!(
                    my_members == members,
                    "Two abilities have different definitions, definitely a bug"
                );
            }
            let member_data = members
                .into_iter()
                .map(|member| (member, other_ability_members.remove(&member).unwrap()));
            self.register_ability(ability, member_data);
        }

        for (specialization, member) in specialization_to_root.into_iter() {
            let old_root = self.specialization_to_root.insert(specialization, member);
            debug_assert!(old_root.is_none() || old_root.unwrap() == member);
        }

        for ((member, typ), specialization) in declared_specializations.into_iter() {
            let old_specialization = self
                .declared_specializations
                .insert((member, typ), specialization);
            debug_assert!(
                old_specialization.is_none() || old_specialization.unwrap() == specialization
            );
        }

        debug_assert!(next_specialization_id == 0);
        debug_assert!(self.next_specialization_id == 0);
        debug_assert!(resolved_specializations.is_empty());
        debug_assert!(self.resolved_specializations.is_empty());
    }

    pub fn resolved_imported_member_var(&mut self, member: Symbol, var: Variable) {
        let member_data = self.ability_members.get_mut(&member).unwrap();
        match &mut member_data.typ {
            MemberTypeInfo::Imported { signature_var } => {
                let old = signature_var.replace(var);
                debug_assert!(old.is_none(), "Replacing existing variable!");
            }
            _ => internal_error!("{:?} is not imported!", member),
        }
    }
}

use std::num::NonZeroU32;

use roc_collections::{all::MutMap, VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
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

/// The member and its signature is defined locally, in the module the store is created for.
/// We need to instantiate and introduce this during solving.
#[derive(Debug, Clone)]
pub struct ResolvedMemberType(Variable);

/// Member type information that needs to be resolved from imports.
#[derive(Debug, Clone)]
pub enum PendingMemberType {
    /// The member and its signature is defined locally, in the module the store is created for.
    /// We need to instantiate and introduce this during solving.
    Local {
        signature_var: Variable,
        signature: Type,
        variables: MemberVariables,
    },
    /// The member was defined in another module, so we'll import its variable when it's time to
    /// solve. At that point we'll resolve `var` here.
    Imported,
}

pub trait ResolvePhase: std::fmt::Debug + Clone + Copy {
    type MemberType: std::fmt::Debug + Clone;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Pending;
impl ResolvePhase for Pending {
    type MemberType = PendingMemberType;
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Resolved;
impl ResolvePhase for Resolved {
    type MemberType = ResolvedMemberType;
}

/// Stores information about an ability member definition, including the parent ability, the
/// defining type, and what type variables need to be instantiated with instances of the ability.
// TODO: SoA and put me in an arena
#[derive(Debug, Clone)]
pub struct AbilityMemberData<Phase: ResolvePhase> {
    pub parent_ability: Symbol,
    pub region: Region,
    pub typ: Phase::MemberType,
}

impl AbilityMemberData<Resolved> {
    pub fn signature_var(&self) -> Variable {
        self.typ.0
    }
}

/// (member, specialization type) -> specialization
pub type SpecializationsMap<Phase> = VecMap<(Symbol, Symbol), MemberSpecialization<Phase>>;

pub type PendingSpecializations = SpecializationsMap<Pending>;
pub type ResolvedSpecializations = SpecializationsMap<Resolved>;

/// Solved lambda sets for an ability member specialization. For example, if we have
///
///   Default has default : {} -[[] + a:default:1]-> a | a has Default
///
///   A := {}
///   default = \{} -[[closA]]-> @A {}
///
/// and this [MemberSpecialization] is for `A`, then there is a mapping of
/// `1` to the variable representing `[[closA]]`.
pub type SpecializationLambdaSets = VecMap<u8, Variable>;

/// A particular specialization of an ability member.
#[derive(Debug, Clone)]
pub struct MemberSpecialization<Phase: ResolvePhase> {
    _phase: std::marker::PhantomData<Phase>,

    pub symbol: Symbol,

    pub specialization_lambda_sets: SpecializationLambdaSets,
}

impl MemberSpecialization<Resolved> {
    pub fn new(symbol: Symbol, specialization_lambda_sets: SpecializationLambdaSets) -> Self {
        Self {
            _phase: Default::default(),
            symbol,
            specialization_lambda_sets,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpecializationId(NonZeroU32);

static_assertions::assert_eq_size!(SpecializationId, Option<SpecializationId>);

pub enum SpecializationLambdaSetError {}

/// Stores information about what abilities exist in a scope, what it means to implement an
/// ability, and what types implement them.
// TODO(abilities): this should probably go on the Scope, I don't put it there for now because we
// are only dealing with intra-module abilities for now.
// TODO(abilities): many of these should be `VecMap`s. Do some benchmarking.
#[derive(Debug, Clone)]
pub struct IAbilitiesStore<Phase: ResolvePhase> {
    /// Maps an ability to the members defining it.
    members_of_ability: MutMap<Symbol, Vec<Symbol>>,

    /// Information about all members composing abilities.
    ability_members: MutMap<Symbol, AbilityMemberData<Phase>>,

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
    declared_specializations: SpecializationsMap<Phase>,

    next_specialization_id: NonZeroU32,

    /// Resolved specializations for a symbol. These might be ephemeral (known due to type solving),
    /// or resolved on-the-fly during mono.
    resolved_specializations: MutMap<SpecializationId, Symbol>,
}

impl<Phase: ResolvePhase> Default for IAbilitiesStore<Phase> {
    fn default() -> Self {
        Self {
             members_of_ability: Default::default(),
             ability_members: Default::default(),
             specialization_to_root: Default::default(),
             declared_specializations: Default::default(),
             next_specialization_id:
                 // Safety: 1 != 0
                 unsafe { NonZeroU32::new_unchecked(1) },
             resolved_specializations: Default::default(),
         }
    }
}

pub type AbilitiesStore = IAbilitiesStore<Resolved>;
pub type PendingAbilitiesStore = IAbilitiesStore<Pending>;

impl<Phase: ResolvePhase> IAbilitiesStore<Phase> {
    /// Records the definition of an ability, including its members.
    pub fn register_ability<I>(&mut self, ability: Symbol, members: I)
    where
        I: IntoIterator<Item = (Symbol, AbilityMemberData<Phase>)>,
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

    /// Checks if `name` is a root ability member symbol name.
    /// Note that this will return `false` for specializations of an ability member, which have
    /// different symbols from the root.
    pub fn is_ability_member_name(&self, name: Symbol) -> bool {
        self.ability_members.contains_key(&name)
    }

    pub fn is_ability(&self, ability: Symbol) -> bool {
        self.members_of_ability.contains_key(&ability)
    }

    /// Iterator over all abilities and their members that this store knows about.
    pub fn iter_abilities(&self) -> impl Iterator<Item = (Symbol, &[Symbol])> {
        self.members_of_ability
            .iter()
            .map(|(k, v)| (*k, v.as_slice()))
    }

    /// Returns information about all known ability members and their root symbols.
    pub fn root_ability_members(&self) -> &MutMap<Symbol, AbilityMemberData<Phase>> {
        &self.ability_members
    }

    /// Returns whether a symbol is declared to specialize an ability member.
    pub fn is_specialization_name(&self, symbol: Symbol) -> bool {
        self.specialization_to_root.contains_key(&symbol)
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

    pub fn members_of_ability(&self, ability: Symbol) -> Option<&[Symbol]> {
        self.members_of_ability.get(&ability).map(|v| v.as_ref())
    }

    pub fn fresh_specialization_id(&mut self) -> SpecializationId {
        debug_assert!(self.next_specialization_id.get() != std::u32::MAX);

        let id = SpecializationId(self.next_specialization_id);
        // Safety: we already checked this won't overflow, and we started > 0.
        self.next_specialization_id =
            unsafe { NonZeroU32::new_unchecked(self.next_specialization_id.get() + 1) };
        id
    }

    /// Creates a store from [`self`] that closes over the abilities/members given by the
    /// imported `symbols`, and their specializations (if any).
    pub fn closure_from_imported(&self, symbols: &VecSet<Symbol>) -> PendingAbilitiesStore {
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

        let mut new = PendingAbilitiesStore::default();

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
                let AbilityMemberData {
                    parent_ability,
                    region,
                    typ: _,
                } = ability_members.get(member).unwrap().clone();
                // All external members need to be marked as imported. We'll figure out their real
                // type variables when it comes time to solve the module we're currently importing
                // into.
                let imported_data = AbilityMemberData {
                    parent_ability,
                    region,
                    typ: PendingMemberType::Imported,
                };

                imported_member_data.push((*member, imported_data));
            }

            new.register_ability(ability, imported_member_data);

            // Add any specializations of the ability's members we know about.
            declared_specializations
                .iter()
                .filter(|((member, _), _)| members.contains(member))
                .for_each(|(&(member, typ), specialization)| {
                    new.register_specializing_symbol(specialization.symbol, member);
                    new.import_specialization(member, typ, specialization);
                });
        }

        new
    }
}

impl IAbilitiesStore<Resolved> {
    /// Finds the symbol name and ability member definition for a symbol specializing the ability
    /// member, if it specializes any.
    /// For example, suppose `hash : Id -> U64` has symbol #hash1 and specializes
    /// `hash : a -> U64 | a has Hash` with symbol #hash. Calling this with #hash1 would retrieve
    /// the ability member data for #hash.
    pub fn root_name_and_def(
        &self,
        specializing_symbol: Symbol,
    ) -> Option<(Symbol, &AbilityMemberData<Resolved>)> {
        let root_symbol = self.specialization_to_root.get(&specializing_symbol)?;
        debug_assert!(self.ability_members.contains_key(root_symbol));
        let root_data = self.ability_members.get(root_symbol).unwrap();
        Some((*root_symbol, root_data))
    }

    /// Finds the ability member definition for a member name.
    pub fn member_def(&self, member: Symbol) -> Option<&AbilityMemberData<Resolved>> {
        self.ability_members.get(&member)
    }

    /// Returns an iterator over pairs ((ability member, type), specialization) specifying that
    /// "ability member" has a "specialization" for type "type".
    pub fn iter_specializations(
        &self,
    ) -> impl Iterator<Item = ((Symbol, Symbol), &MemberSpecialization<Resolved>)> + '_ {
        self.declared_specializations.iter().map(|(k, v)| (*k, v))
    }

    /// Retrieves the specialization of `member` for `typ`, if it exists.
    pub fn get_specialization(
        &self,
        member: Symbol,
        typ: Symbol,
    ) -> Option<&MemberSpecialization<Resolved>> {
        self.declared_specializations.get(&(member, typ))
    }

    /// Records a specialization of `ability_member` with specialized type `implementing_type`.
    /// Entries via this function are considered a source of truth. It must be ensured that a
    /// specialization is validated before being registered here.
    pub fn register_specialization_for_type(
        &mut self,
        ability_member: Symbol,
        implementing_type: Symbol,
        specialization: MemberSpecialization<Resolved>,
    ) {
        let old_spec = self
            .declared_specializations
            .insert((ability_member, implementing_type), specialization);
        debug_assert!(old_spec.is_none(), "Replacing existing specialization");
    }

    pub fn insert_resolved(&mut self, id: SpecializationId, specialization: Symbol) {
        // May not be a thing in mono
        // debug_assert!(self.is_specialization_name(specialization));

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
}

impl IAbilitiesStore<Pending> {
    pub fn import_specialization(
        &mut self,
        ability_member: Symbol,
        implementing_type: Symbol,
        specialization: &MemberSpecialization<impl ResolvePhase>,
    ) {
        let MemberSpecialization {
            _phase,
            symbol,
            specialization_lambda_sets,
        } = specialization;

        let old_spec = self.declared_specializations.insert(
            (ability_member, implementing_type),
            MemberSpecialization {
                _phase: Default::default(),
                symbol: *symbol,
                specialization_lambda_sets: specialization_lambda_sets.clone(),
            },
        );
        debug_assert!(old_spec.is_none(), "Replacing existing specialization");
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
                .insert((member, typ), specialization.clone());
            debug_assert!(
                old_specialization.is_none()
                    || old_specialization.unwrap().symbol == specialization.symbol
            );
        }

        debug_assert_eq!(next_specialization_id.get(), 1);
        debug_assert_eq!(self.next_specialization_id.get(), 1);
        debug_assert!(resolved_specializations.is_empty());
        debug_assert!(self.resolved_specializations.is_empty());
    }

    pub fn resolve_for_module<Ctx, VarOfSymbol, ImportVar>(
        self,
        my_module: ModuleId,
        my_module_ctx: &mut Ctx,
        mut variable_of_symbol: VarOfSymbol,
        mut import_lambda_set_var_from_module: ImportVar,
    ) -> AbilitiesStore
    where
        VarOfSymbol: FnMut(&mut Ctx, Symbol) -> Variable,
        ImportVar: FnMut(&mut Ctx, ModuleId, Variable) -> Variable,
    {
        let Self {
            members_of_ability,
            ability_members,
            specialization_to_root,
            declared_specializations,
            next_specialization_id,
            resolved_specializations,
        } = self;

        let ability_members = ability_members
            .into_iter()
            .map(|(member_symbol, member_data)| {
                let AbilityMemberData {
                    parent_ability,
                    region,
                    typ,
                } = member_data;

                let typ = match typ {
                    PendingMemberType::Local {
                        signature_var,
                        signature: _,
                        variables: _,
                    } => ResolvedMemberType(signature_var),
                    PendingMemberType::Imported => {
                        ResolvedMemberType(variable_of_symbol(my_module_ctx, member_symbol))
                    }
                };

                let member_data = AbilityMemberData {
                    parent_ability,
                    region,
                    typ,
                };

                (member_symbol, member_data)
            })
            .collect();

        let declared_specializations = declared_specializations
            .into_iter()
            .map(
                |(
                    key,
                    MemberSpecialization {
                        _phase,
                        symbol,
                        specialization_lambda_sets,
                    },
                )| {
                    let symbol_module = symbol.module_id();
                    // NOTE: this totally assumes we're dealing with subs that belong to an
                    // individual module, things would be badly broken otherwise
                    let member_specialization = if symbol_module == my_module {
                        internal_error!("Ability store may only be pending before module solving, \
                            so there shouldn't be any known module specializations at this point, but we found one for {:?}", symbol);
                        // MemberSpecialization::new(symbol, specialization_lambda_sets)
                    } else {
                        let specialization_lambda_sets = specialization_lambda_sets
                            .into_iter()
                            .map(|(region, variable)| {
                                (
                                    region,
                                    import_lambda_set_var_from_module(
                                        my_module_ctx,
                                        symbol_module,
                                        variable,
                                    ),
                                )
                            })
                            .collect();
                        MemberSpecialization::new(symbol, specialization_lambda_sets)
                    };
                    (key, member_specialization)
                },
            )
            .collect();

        AbilitiesStore {
            members_of_ability,
            ability_members,
            specialization_to_root,
            declared_specializations,
            next_specialization_id,
            resolved_specializations,
        }
    }
}

use std::num::NonZeroU32;

use roc_collections::{all::MutMap, VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::Region;
use roc_types::{
    subs::Variable,
    types::{MemberImpl, Type},
};

/// During type solving and monomorphization, a module must know how its imported ability
/// implementations are resolved - are they derived, or have a concrete implementation?
///
/// Unfortunately we cannot keep this information opaque, as it's important for properly
/// restoring specialization lambda sets. As such, we need to export implementation information,
/// which is the job of this structure.
pub type ResolvedImplementations = VecMap<ImplKey, ResolvedImpl>;

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
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Resolved;
impl ResolvePhase for Resolved {
    type MemberType = ResolvedMemberType;
}

/// Stores information about an ability member definition, including the parent ability, the
/// defining type, and what type variables need to be instantiated with instances of the ability.
// TODO: SoA and put me in an arena
#[derive(Debug, Clone, PartialEq)]
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

/// Solved lambda sets for an ability member specialization. For example, if we have
///
///   Default implements default : {} -[[] + a:default:1]-> a where a implements Default
///
///   A := {}
///   default = \{} -[[closA]]-> @A {}
///
/// and this [MemberSpecialization] is for `A`, then there is a mapping of
/// `1` to the variable representing `[[closA]]`.
pub type SpecializationLambdaSets = VecMap<u8, Variable>;

/// A particular specialization of an ability member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberSpecializationInfo<Phase: ResolvePhase> {
    _phase: std::marker::PhantomData<Phase>,
    pub symbol: Symbol,
    pub specialization_lambda_sets: SpecializationLambdaSets,
}

impl MemberSpecializationInfo<Resolved> {
    pub fn new(symbol: Symbol, specialization_lambda_sets: SpecializationLambdaSets) -> Self {
        Self {
            _phase: Default::default(),
            symbol,
            specialization_lambda_sets,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SpecializationId(NonZeroU32);

static_assertions::assert_eq_size!(SpecializationId, Option<SpecializationId>);

/// A key into a particular implementation of an ability member for an opaque type.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ImplKey {
    pub opaque: Symbol,
    pub ability_member: Symbol,
}

/// Fully-resolved implementation of an ability member for an opaque type.
/// This is only fully known after type solving of the owning module.
#[derive(Clone, Debug)]
pub enum ResolvedImpl {
    Impl(MemberSpecializationInfo<Resolved>),
    Error,
}

/// Stores information about what abilities exist in a scope, what it means to implement an
/// ability, and what types implement them.
// TODO(abilities): this should probably go on the Scope, I don't put it there for now because we
// are only dealing with intra-module abilities for now.
// TODO(abilities): many of these should be `VecMap`s. Do some benchmarking.
#[derive(Debug, Clone)]
pub struct IAbilitiesStore<Phase: ResolvePhase> {
    /// Maps an ability to the members defining it.
    members_of_ability: MutMap<Symbol, Vec<Symbol>>,
    /// Map of symbols that specialize an ability member to the root ability symbol name,
    /// and the type the specialization claims to implement the ability for.
    ///
    /// For example, in the program
    ///
    ///   Hash implements hash : a -> U64 where a implements Hash
    ///
    ///   Id := {} implements [Hash {hash: myHash}]
    ///   myHash = \@Id n -> n
    ///
    /// We keep the mapping myHash->(hash, Id)
    specialization_to_root: MutMap<Symbol, ImplKey>,

    /// Information about all members composing abilities.
    ability_members: MutMap<Symbol, AbilityMemberData<Phase>>,

    /// Maps a tuple (member, type) specifying that `type` implements an ability
    /// member `member`, to how that implementation is defined.
    declared_implementations: MutMap<ImplKey, MemberImpl>,

    /// Information about specialized ability member implementations for a type.
    specializations: MutMap<Symbol, MemberSpecializationInfo<Phase>>,

    next_specialization_id: NonZeroU32,

    /// Resolved specializations for a symbol. These might be ephemeral (known due to type solving),
    /// or resolved on-the-fly during mono.
    resolved_specializations: MutMap<SpecializationId, Symbol>,
}

impl<Phase: ResolvePhase> Default for IAbilitiesStore<Phase> {
    fn default() -> Self {
        Self {
            members_of_ability: Default::default(),
            specialization_to_root: Default::default(),
            ability_members: Default::default(),
            declared_implementations: Default::default(),
            specializations: Default::default(),
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

    #[inline(always)]
    fn register_one_declared_impl(&mut self, impl_key: ImplKey, member_impl: MemberImpl) {
        if let MemberImpl::Impl(specialization_symbol) = member_impl {
            self.specialization_to_root
                .insert(specialization_symbol, impl_key);
        }
        self.declared_implementations.insert(impl_key, member_impl);
    }

    /// Records the implementations of an ability an opaque type declares to have.
    ///
    /// Calling this function does not validate that the implementations are correctly specializing
    /// in their definition, nor does it store type information about the implementations.
    ///
    /// It is expected that during type solving, the owner of the abilities store marks the claimed
    /// implementation as either a proper or erroring implementation using
    /// [`Self::mark_implementation`].
    pub fn register_declared_implementations(
        &mut self,
        implementing_type: Symbol,
        // (ability member, implementation)
        implementations: impl IntoIterator<Item = (Symbol, MemberImpl)>,
    ) {
        for (member, member_impl) in implementations.into_iter() {
            let impl_key = ImplKey {
                opaque: implementing_type,
                ability_member: member,
            };
            self.register_one_declared_impl(impl_key, member_impl);
        }
    }

    /// Returns whether a symbol is declared to specialize an ability member.
    pub fn is_specialization_name(&self, symbol: Symbol) -> bool {
        self.specialization_to_root.contains_key(&symbol)
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

    /// Finds the implementation key for a symbol specializing the ability member, if it specializes any.
    /// For example, suppose `hashId : Id -> U64` specializes `hash : a -> U64 where a implements Hash`.
    /// Calling this with `hashId` would retrieve (hash, hashId).
    pub fn impl_key(&self, specializing_symbol: Symbol) -> Option<&ImplKey> {
        self.specialization_to_root.get(&specializing_symbol)
    }

    /// Answers the question, "does an opaque type claim to implement a particular ability?"
    ///
    /// Whether the given opaque typ faithfully implements or derives all members of the given ability
    /// without errors is not validated.
    ///
    /// When the given ability is not known to the current store, this call will return `false`.
    pub fn has_declared_implementation(&self, opaque: Symbol, ability: Symbol) -> bool {
        // Idea: choose an ability member and check whether there is a declared implementation for it.
        // During canonicalization, we would have added either all members as declared
        // implementations, or none if the opaque doesn't implement the ability.
        match self.members_of_ability(ability) {
            Some(members) => self.declared_implementations.contains_key(&ImplKey {
                opaque,
                ability_member: members[0],
            }),
            None => false,
        }
    }

    /// Creates a store from [`self`] that closes over the abilities/members given by the
    /// imported `symbols`, and their specializations (if any).
    pub fn closure_from_imported(&self, symbols: &VecSet<Symbol>) -> PendingAbilitiesStore {
        let Self {
            members_of_ability,
            ability_members,
            declared_implementations,
            specializations,

            // Covered by `declared_implementations`
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
            declared_implementations
                .iter()
                .filter(|(impl_key, _)| members.contains(&impl_key.ability_member))
                .for_each(|(&impl_key, member_impl)| {
                    new.register_one_declared_impl(impl_key, *member_impl);

                    if let MemberImpl::Impl(spec_symbol) = member_impl {
                        if let Some(specialization_info) = specializations.get(spec_symbol) {
                            new.import_specialization(specialization_info);
                        }
                    }
                });
        }

        new
    }
}

#[derive(Debug)]
pub enum MarkError {
    NoDeclaredImpl,
    ImplIsNotCustom,
}

impl IAbilitiesStore<Resolved> {
    /// Finds the symbol name and ability member definition for a symbol specializing the ability
    /// member, if it specializes any.
    /// For example, suppose `hashId : Id -> U64` specializes `hash : a -> U64 where a implements Hash`.
    /// Calling this with `hashId` would retrieve the ability member data for `hash`, and what type
    /// `hashId` is specializing for.
    pub fn impl_key_and_def(
        &self,
        specializing_symbol: Symbol,
    ) -> Option<(ImplKey, &AbilityMemberData<Resolved>)> {
        let impl_key = self.impl_key(specializing_symbol)?;
        debug_assert!(self.ability_members.contains_key(&impl_key.ability_member));
        let root_data = self
            .ability_members
            .get(&impl_key.ability_member)
            .expect("impl keys can only exist for known ability members");
        Some((*impl_key, root_data))
    }

    /// Finds the ability member definition for a member name.
    pub fn member_def(&self, member: Symbol) -> Option<&AbilityMemberData<Resolved>> {
        self.ability_members.get(&member)
    }

    /// Returns an iterator over pairs ((ability member, type), implementation) specifying that
    /// the given type implements an ability member.
    pub fn iter_declared_implementations(
        &self,
    ) -> impl Iterator<Item = (ImplKey, &MemberImpl)> + '_ {
        self.declared_implementations.iter().map(|(k, v)| (*k, v))
    }

    /// Retrieves the declared implementation of `member` for `typ`, if it exists.
    pub fn get_implementation(&self, impl_key: ImplKey) -> Option<&MemberImpl> {
        self.declared_implementations.get(&impl_key)
    }

    /// Marks a declared implementation as either properly specializing, or as erroring.
    pub fn mark_implementation(
        &mut self,
        impl_key: ImplKey,
        mark: Result<MemberSpecializationInfo<Resolved>, ()>,
    ) -> Result<(), MarkError> {
        match self.declared_implementations.get_mut(&impl_key) {
            Some(member_impl) => match *member_impl {
                MemberImpl::Impl(specialization_symbol) => {
                    debug_assert!(!self.specializations.contains_key(&specialization_symbol));

                    match mark {
                        Ok(specialization_info) => {
                            self.specializations
                                .insert(specialization_symbol, specialization_info);
                        }
                        Err(()) => {
                            // Mark the member implementation as erroring, so we know to generate a
                            // runtime error function as appropriate.
                            *member_impl = MemberImpl::Error;
                        }
                    }

                    Ok(())
                }
                MemberImpl::Error => Err(MarkError::ImplIsNotCustom),
            },
            None => Err(MarkError::NoDeclaredImpl),
        }
    }

    pub fn specialization_info(
        &self,
        specialization_symbol: Symbol,
    ) -> Option<&MemberSpecializationInfo<Resolved>> {
        self.specializations.get(&specialization_symbol)
    }

    pub fn insert_resolved(&mut self, id: SpecializationId, specialization: Symbol) {
        let old_specialization = self.resolved_specializations.insert(id, specialization);

        debug_assert!(
            old_specialization.is_none(),
            "Existing resolution: {old_specialization:?}"
        );
    }

    pub fn get_resolved(&self, id: SpecializationId) -> Option<Symbol> {
        self.resolved_specializations.get(&id).copied()
    }

    pub fn serialize(&self, writer: &mut impl std::io::Write) -> std::io::Result<usize> {
        serialize::serialize(self, writer)
    }

    pub fn deserialize(bytes: &[u8]) -> (Self, usize) {
        serialize::deserialize(bytes)
    }
}

pub use serialize::deserialize_solved_implementations;
pub use serialize::serialize_solved_implementations;

impl IAbilitiesStore<Pending> {
    pub fn import_implementation(&mut self, impl_key: ImplKey, resolved_impl: &ResolvedImpl) {
        let member_impl = match resolved_impl {
            ResolvedImpl::Impl(specialization) => {
                self.import_specialization(specialization);
                MemberImpl::Impl(specialization.symbol)
            }
            ResolvedImpl::Error => MemberImpl::Error,
        };

        let old_declared_impl = self.declared_implementations.insert(impl_key, member_impl);
        debug_assert!(
            old_declared_impl.is_none() ||
                // Can happen between we import declared implementations during canonicalization, but
                // implementation information only after solving
                old_declared_impl.unwrap() == member_impl,
            "Replacing existing declared impl: {:?}",
            (impl_key, old_declared_impl)
        );
    }

    fn import_specialization(
        &mut self,
        specialization: &MemberSpecializationInfo<impl ResolvePhase>,
    ) {
        let MemberSpecializationInfo {
            _phase,
            symbol,
            specialization_lambda_sets,
        } = specialization;

        let old_spec = self.specializations.insert(
            *symbol,
            MemberSpecializationInfo {
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
            declared_implementations,
            next_specialization_id,
            resolved_specializations,
            specializations,
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

        for (impl_key, impl_) in declared_implementations.into_iter() {
            let old_impl = self.declared_implementations.insert(impl_key, impl_);
            debug_assert!(old_impl.is_none() || old_impl.unwrap() == impl_);
        }

        for (symbol, specialization_info) in specializations.into_iter() {
            let old_specialization = self
                .specializations
                .insert(symbol, specialization_info.clone());
            debug_assert!(
                old_specialization.is_none()
                    || old_specialization.unwrap().symbol == specialization_info.symbol
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
            declared_implementations,
            next_specialization_id,
            resolved_specializations,
            specializations,
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

        let specializations = specializations
            .into_iter()
            .map(
                |(symbol, specialization)| {
                    let MemberSpecializationInfo {
                        _phase,
                        symbol: _,
                        specialization_lambda_sets,
                    } = specialization;
                    let symbol_module = symbol.module_id();

                    // NOTE: this totally assumes we're dealing with subs that belong to an
                    // individual module, things would be badly broken otherwise
                    let member_specialization = if symbol_module == my_module {
                        internal_error!("Ability store may only be pending before module solving, \
                            so there shouldn't be any known module specializations at this point, but we found one for {:?}", symbol);
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

                        MemberSpecializationInfo::new(symbol, specialization_lambda_sets)
                    };
                    (symbol, member_specialization)
                }
            )
            .collect();

        AbilitiesStore {
            members_of_ability,
            ability_members,
            specialization_to_root,
            declared_implementations,
            next_specialization_id,
            resolved_specializations,
            specializations,
        }
    }
}

mod serialize {
    use roc_collections::{soa::slice_extend_new, MutMap, VecMap};
    use roc_module::symbol::Symbol;
    use roc_region::all::Region;
    use roc_serialize::bytes;
    use roc_types::{
        subs::{SubsSlice, Variable},
        types::MemberImpl,
    };

    use super::{
        AbilitiesStore, AbilityMemberData, ImplKey, MemberSpecializationInfo, Resolved,
        ResolvedImpl, ResolvedImplementations, ResolvedMemberType, SpecializationId,
    };

    use std::io::{self, Write};

    #[repr(C)]
    #[derive(Clone, Copy, Debug)]
    struct Header {
        members_of_ability: u64,
        specialization_to_root: u64,
        ability_members: u64,
        declared_implementations: u64,
        specializations: u64,
        next_specialization_id: u64,
        resolved_specializations: u64,
    }

    impl Header {
        fn from_store(store: &AbilitiesStore) -> Self {
            let AbilitiesStore {
                members_of_ability,
                specialization_to_root,
                ability_members,
                declared_implementations,
                specializations,
                next_specialization_id,
                resolved_specializations,
            } = store;

            Self {
                members_of_ability: members_of_ability.len() as _,
                specialization_to_root: specialization_to_root.len() as _,
                ability_members: ability_members.len() as _,
                declared_implementations: declared_implementations.len() as _,
                specializations: specializations.len() as _,
                next_specialization_id: next_specialization_id.get() as _,
                resolved_specializations: resolved_specializations.len() as _,
            }
        }

        fn to_array(self) -> [u8; std::mem::size_of::<Self>()] {
            // Safety: With repr(c) all fields are in order and properly aligned without padding.
            unsafe { std::mem::transmute(self) }
        }

        fn from_array(array: [u8; std::mem::size_of::<Self>()]) -> Self {
            // Safety: With repr(c) all fields are in order and properly aligned without padding.
            unsafe { std::mem::transmute(array) }
        }
    }

    pub(super) fn serialize(store: &AbilitiesStore, writer: &mut impl Write) -> io::Result<usize> {
        let header = Header::from_store(store).to_array();
        let written = header.len();
        writer.write_all(&header)?;

        let AbilitiesStore {
            members_of_ability,
            specialization_to_root,
            ability_members,
            declared_implementations,
            specializations,
            next_specialization_id: _, // written in the header
            resolved_specializations,
        } = store;

        let written = serialize_members_of_ability(members_of_ability, writer, written)?;
        let written = serialize_specializations_to_root(specialization_to_root, writer, written)?;
        let written = serialize_ability_members(ability_members, writer, written)?;
        let written =
            serialize_declared_implementations(declared_implementations, writer, written)?;
        let written = serialize_specializations(specializations, writer, written)?;
        let written =
            serialize_resolved_specializations(resolved_specializations, writer, written)?;

        Ok(written)
    }

    pub(super) fn deserialize(bytes: &[u8]) -> (AbilitiesStore, usize) {
        let mut offset = 0;

        let header_slice = &bytes[..std::mem::size_of::<Header>()];
        offset += header_slice.len();
        let header = Header::from_array(header_slice.try_into().unwrap());

        let (members_of_ability, offset) =
            deserialize_members_of_ability(bytes, header.members_of_ability as _, offset);
        let (specialization_to_root, offset) =
            deserialize_specialization_to_root(bytes, header.specialization_to_root as _, offset);
        let (ability_members, offset) =
            deserialize_ability_members(bytes, header.ability_members as _, offset);
        let (declared_implementations, offset) = deserialize_declared_implementations(
            bytes,
            header.declared_implementations as _,
            offset,
        );
        let (specializations, offset) =
            deserialize_specializations(bytes, header.specializations as _, offset);
        let (resolved_specializations, offset) = deserialize_resolved_specializations(
            bytes,
            header.resolved_specializations as _,
            offset,
        );

        (
            AbilitiesStore {
                members_of_ability,
                specialization_to_root,
                ability_members,
                declared_implementations,
                specializations,
                next_specialization_id: (header.next_specialization_id as u32).try_into().unwrap(),
                resolved_specializations,
            },
            offset,
        )
    }

    fn serialize_members_of_ability(
        members_of_ability: &MutMap<Symbol, Vec<Symbol>>,
        writer: &mut impl Write,
        written: usize,
    ) -> io::Result<usize> {
        bytes::serialize_map(
            members_of_ability,
            bytes::serialize_slice,
            bytes::serialize_slice_of_slices,
            writer,
            written,
        )
    }

    fn deserialize_members_of_ability(
        bytes: &[u8],
        length: usize,
        offset: usize,
    ) -> (MutMap<Symbol, Vec<Symbol>>, usize) {
        bytes::deserialize_map(
            bytes,
            bytes::deserialize_vec,
            bytes::deserialize_slice_of_slices,
            length,
            offset,
        )
    }

    #[derive(Clone, Copy)]
    #[repr(C)]
    struct SerImplKey(Symbol, Symbol);
    impl From<&ImplKey> for SerImplKey {
        fn from(k: &ImplKey) -> Self {
            Self(k.opaque, k.ability_member)
        }
    }
    impl From<&SerImplKey> for ImplKey {
        fn from(k: &SerImplKey) -> Self {
            Self {
                opaque: k.0,
                ability_member: k.1,
            }
        }
    }

    fn serialize_specializations_to_root(
        specialization_to_root: &MutMap<Symbol, ImplKey>,
        writer: &mut impl Write,
        written: usize,
    ) -> io::Result<usize> {
        bytes::serialize_map(
            specialization_to_root,
            bytes::serialize_slice,
            |keys, writer, written| {
                bytes::serialize_slice(
                    &keys.iter().map(SerImplKey::from).collect::<Vec<_>>(),
                    writer,
                    written,
                )
            },
            writer,
            written,
        )
    }

    fn deserialize_specialization_to_root(
        bytes: &[u8],
        length: usize,
        offset: usize,
    ) -> (MutMap<Symbol, ImplKey>, usize) {
        bytes::deserialize_map(
            bytes,
            bytes::deserialize_vec,
            |bytes, length, offset| {
                let (slice, offset) = bytes::deserialize_slice::<SerImplKey>(bytes, length, offset);
                (slice.iter().map(ImplKey::from).collect(), offset)
            },
            length,
            offset,
        )
    }

    #[derive(Clone, Copy)]
    #[repr(C)]
    struct SerMemberData(Symbol, Region, Variable);
    impl From<&AbilityMemberData<Resolved>> for SerMemberData {
        fn from(k: &AbilityMemberData<Resolved>) -> Self {
            Self(k.parent_ability, k.region, k.typ.0)
        }
    }
    impl From<&SerMemberData> for AbilityMemberData<Resolved> {
        fn from(k: &SerMemberData) -> Self {
            Self {
                parent_ability: k.0,
                region: k.1,
                typ: ResolvedMemberType(k.2),
            }
        }
    }

    fn serialize_ability_members(
        ability_members: &MutMap<Symbol, AbilityMemberData<Resolved>>,
        writer: &mut impl Write,
        written: usize,
    ) -> io::Result<usize> {
        bytes::serialize_map(
            ability_members,
            bytes::serialize_slice,
            |keys, writer, written| {
                bytes::serialize_slice(
                    &keys.iter().map(SerMemberData::from).collect::<Vec<_>>(),
                    writer,
                    written,
                )
            },
            writer,
            written,
        )
    }

    fn deserialize_ability_members(
        bytes: &[u8],
        length: usize,
        offset: usize,
    ) -> (MutMap<Symbol, AbilityMemberData<Resolved>>, usize) {
        bytes::deserialize_map(
            bytes,
            bytes::deserialize_vec,
            |bytes, length, offset| {
                let (slice, offset) =
                    bytes::deserialize_slice::<SerMemberData>(bytes, length, offset);
                (slice.iter().map(AbilityMemberData::from).collect(), offset)
            },
            length,
            offset,
        )
    }

    #[derive(Clone, Copy)]
    #[repr(C)]
    enum SerMemberImpl {
        Impl(Symbol),
        Error,
    }
    impl From<&MemberImpl> for SerMemberImpl {
        fn from(k: &MemberImpl) -> Self {
            match k {
                MemberImpl::Impl(s) => Self::Impl(*s),
                MemberImpl::Error => Self::Error,
            }
        }
    }
    impl From<&SerMemberImpl> for MemberImpl {
        fn from(k: &SerMemberImpl) -> Self {
            match k {
                SerMemberImpl::Impl(s) => Self::Impl(*s),
                SerMemberImpl::Error => Self::Error,
            }
        }
    }

    fn serialize_declared_implementations(
        declared_implementations: &MutMap<ImplKey, MemberImpl>,
        writer: &mut impl Write,
        written: usize,
    ) -> io::Result<usize> {
        bytes::serialize_map(
            declared_implementations,
            bytes::serialize_slice,
            |keys, writer, written| {
                bytes::serialize_slice(
                    &keys.iter().map(SerMemberImpl::from).collect::<Vec<_>>(),
                    writer,
                    written,
                )
            },
            writer,
            written,
        )
    }

    fn deserialize_declared_implementations(
        bytes: &[u8],
        length: usize,
        offset: usize,
    ) -> (MutMap<ImplKey, MemberImpl>, usize) {
        bytes::deserialize_map(
            bytes,
            bytes::deserialize_vec,
            |bytes, length, offset| {
                let (slice, offset) =
                    bytes::deserialize_slice::<SerMemberImpl>(bytes, length, offset);
                (slice.iter().map(MemberImpl::from).collect(), offset)
            },
            length,
            offset,
        )
    }

    #[derive(Clone, Copy)]
    #[repr(C)]
    struct SerMemberSpecInfo(Symbol, SubsSlice<u8>, SubsSlice<Variable>);

    fn serialize_specializations(
        specializations: &MutMap<Symbol, MemberSpecializationInfo<Resolved>>,
        writer: &mut impl Write,
        written: usize,
    ) -> io::Result<usize> {
        bytes::serialize_map(
            specializations,
            bytes::serialize_slice,
            |spec_info, writer, written| {
                let mut spec_lambda_sets_regions: Vec<u8> = Vec::new();
                let mut spec_lambda_sets_vars: Vec<Variable> = Vec::new();
                let mut ser_member_spec_infos: Vec<SerMemberSpecInfo> = Vec::new();

                for MemberSpecializationInfo {
                    _phase: _,
                    symbol,
                    specialization_lambda_sets,
                } in spec_info
                {
                    let regions = slice_extend_new(
                        &mut spec_lambda_sets_regions,
                        specialization_lambda_sets.keys().copied(),
                    );
                    let vars = slice_extend_new(
                        &mut spec_lambda_sets_vars,
                        specialization_lambda_sets.values().copied(),
                    );
                    ser_member_spec_infos.push(SerMemberSpecInfo(*symbol, regions, vars));
                }

                let written = bytes::serialize_slice(&ser_member_spec_infos, writer, written)?;
                let written = bytes::serialize_slice(&spec_lambda_sets_regions, writer, written)?;
                let written = bytes::serialize_slice(&spec_lambda_sets_vars, writer, written)?;

                Ok(written)
            },
            writer,
            written,
        )
    }

    fn deserialize_specializations(
        bytes: &[u8],
        length: usize,
        offset: usize,
    ) -> (MutMap<Symbol, MemberSpecializationInfo<Resolved>>, usize) {
        bytes::deserialize_map(
            bytes,
            bytes::deserialize_vec,
            |bytes, length, offset| {
                let (serialized_slices, offset) =
                    bytes::deserialize_slice::<SerMemberSpecInfo>(bytes, length, offset);

                let (regions_slice, offset) = {
                    let total_items = serialized_slices.iter().map(|s| s.1.len()).sum();
                    bytes::deserialize_slice::<u8>(bytes, total_items, offset)
                };

                let (vars_slice, offset) = {
                    let total_items = serialized_slices.iter().map(|s| s.2.len()).sum();
                    bytes::deserialize_slice::<Variable>(bytes, total_items, offset)
                };

                let mut spec_infos: Vec<MemberSpecializationInfo<Resolved>> =
                    Vec::with_capacity(length);
                for SerMemberSpecInfo(symbol, regions, vars) in serialized_slices {
                    let regions = regions_slice[regions.indices()].to_vec();
                    let lset_vars = vars_slice[vars.indices()].to_vec();
                    let spec_info = MemberSpecializationInfo::new(*symbol, unsafe {
                        VecMap::zip(regions, lset_vars)
                    });
                    spec_infos.push(spec_info)
                }

                (spec_infos, offset)
            },
            length,
            offset,
        )
    }

    fn serialize_resolved_specializations(
        resolved_specializations: &MutMap<SpecializationId, Symbol>,
        writer: &mut impl Write,
        written: usize,
    ) -> io::Result<usize> {
        bytes::serialize_map(
            resolved_specializations,
            bytes::serialize_slice,
            bytes::serialize_slice,
            writer,
            written,
        )
    }

    fn deserialize_resolved_specializations(
        bytes: &[u8],
        length: usize,
        offset: usize,
    ) -> (MutMap<SpecializationId, Symbol>, usize) {
        bytes::deserialize_map(
            bytes,
            bytes::deserialize_vec,
            bytes::deserialize_vec,
            length,
            offset,
        )
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    enum SerResolvedImpl {
        Impl(SerMemberSpecInfo),
        Error,
    }
    impl SerResolvedImpl {
        fn num_regions(&self) -> usize {
            match self {
                SerResolvedImpl::Impl(spec) => spec.1.len(),
                SerResolvedImpl::Error => 0,
            }
        }
    }

    pub fn serialize_solved_implementations(
        solved_impls: &ResolvedImplementations,
        writer: &mut impl std::io::Write,
    ) -> std::io::Result<usize> {
        let len = solved_impls.len() as u64;

        let written = bytes::serialize_slice(&[len], writer, 0)?;

        bytes::serialize_vec_map(
            solved_impls,
            |keys, writer, written| {
                bytes::serialize_slice(
                    &keys.iter().map(SerImplKey::from).collect::<Vec<_>>(),
                    writer,
                    written,
                )
            },
            |resolved_impls, writer, written| {
                let mut spec_lambda_sets_regions: Vec<u8> = Vec::new();
                let mut spec_lambda_sets_vars: Vec<Variable> = Vec::new();
                let mut ser_resolved_impls: Vec<SerResolvedImpl> = Vec::new();

                for resolved_impl in resolved_impls {
                    let ser_resolved_impl = match resolved_impl {
                        ResolvedImpl::Impl(MemberSpecializationInfo {
                            _phase: _,
                            symbol,
                            specialization_lambda_sets,
                        }) => {
                            let regions = slice_extend_new(
                                &mut spec_lambda_sets_regions,
                                specialization_lambda_sets.keys().copied(),
                            );
                            let vars = slice_extend_new(
                                &mut spec_lambda_sets_vars,
                                specialization_lambda_sets.values().copied(),
                            );
                            SerResolvedImpl::Impl(SerMemberSpecInfo(*symbol, regions, vars))
                        }
                        ResolvedImpl::Error => SerResolvedImpl::Error,
                    };

                    ser_resolved_impls.push(ser_resolved_impl);
                }

                let written = bytes::serialize_slice(&ser_resolved_impls, writer, written)?;
                let written = bytes::serialize_slice(&spec_lambda_sets_regions, writer, written)?;
                let written = bytes::serialize_slice(&spec_lambda_sets_vars, writer, written)?;

                Ok(written)
            },
            writer,
            written,
        )
    }

    pub fn deserialize_solved_implementations(bytes: &[u8]) -> (ResolvedImplementations, usize) {
        let (len_slice, offset) = bytes::deserialize_slice::<u64>(bytes, 1, 0);
        let length = len_slice[0];

        bytes::deserialize_vec_map(
            bytes,
            |bytes, length, offset| {
                let (slice, offset) = bytes::deserialize_slice::<SerImplKey>(bytes, length, offset);
                (slice.iter().map(ImplKey::from).collect(), offset)
            },
            |bytes, length, offset| {
                let (serialized_slices, offset) =
                    bytes::deserialize_slice::<SerResolvedImpl>(bytes, length, offset);

                let total_num_regions = serialized_slices.iter().map(|s| s.num_regions()).sum();

                let (regions_slice, offset) =
                    bytes::deserialize_slice::<u8>(bytes, total_num_regions, offset);

                let (vars_slice, offset) =
                    { bytes::deserialize_slice::<Variable>(bytes, total_num_regions, offset) };

                let mut all_resolved: Vec<ResolvedImpl> = Vec::with_capacity(length);
                for ser_resolved in serialized_slices {
                    let resolved = match ser_resolved {
                        SerResolvedImpl::Impl(SerMemberSpecInfo(symbol, regions, vars)) => {
                            let regions = regions_slice[regions.indices()].to_vec();
                            let lset_vars = vars_slice[vars.indices()].to_vec();
                            let spec_info = MemberSpecializationInfo::new(*symbol, unsafe {
                                VecMap::zip(regions, lset_vars)
                            });
                            ResolvedImpl::Impl(spec_info)
                        }
                        SerResolvedImpl::Error => ResolvedImpl::Error,
                    };

                    all_resolved.push(resolved);
                }

                (all_resolved, offset)
            },
            length as _,
            offset,
        )
    }
}

#[cfg(test)]
mod test {
    use roc_collections::VecMap;
    use roc_module::symbol::Symbol;
    use roc_region::all::Region;
    use roc_types::{subs::Variable, types::MemberImpl};

    use super::{
        AbilitiesStore, AbilityMemberData, ImplKey, MemberSpecializationInfo, ResolvedMemberType,
    };

    #[test]
    fn serde_abilities_store() {
        let store = {
            let mut store = AbilitiesStore::default();
            store.register_ability(
                Symbol::ARG_1,
                [
                    (
                        Symbol::ARG_2,
                        AbilityMemberData {
                            parent_ability: Symbol::ARG_1,
                            region: Region::zero(),
                            typ: ResolvedMemberType(Variable::BOOL),
                        },
                    ),
                    (
                        Symbol::ARG_3,
                        AbilityMemberData {
                            parent_ability: Symbol::ARG_1,
                            region: Region::zero(),
                            typ: ResolvedMemberType(Variable::BOOL),
                        },
                    ),
                ],
            );
            store.register_ability(
                Symbol::ARG_4,
                [(
                    Symbol::ARG_5,
                    AbilityMemberData {
                        parent_ability: Symbol::ARG_4,
                        region: Region::zero(),
                        typ: ResolvedMemberType(Variable::BOOL),
                    },
                )],
            );

            store.register_declared_implementations(
                Symbol::ATTR_ATTR,
                [
                    (Symbol::ARG_2, MemberImpl::Impl(Symbol::ATTR_INVALID)),
                    (Symbol::ARG_3, MemberImpl::Impl(Symbol::ARG_CLOSURE)),
                ],
            );

            store.register_declared_implementations(
                Symbol::ATTR_ATTR,
                [(Symbol::ARG_5, MemberImpl::Error)],
            );

            store
                .mark_implementation(
                    ImplKey {
                        opaque: Symbol::ATTR_ATTR,
                        ability_member: Symbol::ARG_2,
                    },
                    Ok(MemberSpecializationInfo::new(Symbol::UNDERSCORE, {
                        let mut map = VecMap::default();
                        map.insert(1, Variable::BOOL);
                        map.insert(2, Variable::U8);
                        map
                    })),
                )
                .unwrap();

            store
                .mark_implementation(
                    ImplKey {
                        opaque: Symbol::ATTR_ATTR,
                        ability_member: Symbol::ARG_3,
                    },
                    Ok(MemberSpecializationInfo::new(Symbol::UNDERSCORE, {
                        let mut map = VecMap::default();
                        map.insert(1, Variable::BOOL);
                        map.insert(2, Variable::U8);
                        map.insert(3, Variable::U32);
                        map.insert(4, Variable::U64);
                        map
                    })),
                )
                .unwrap();

            let spec_id1 = store.fresh_specialization_id();
            let spec_id2 = store.fresh_specialization_id();

            store.insert_resolved(spec_id1, Symbol::ARG_2);
            store.insert_resolved(spec_id2, Symbol::ARG_3);

            store
        };

        let mut bytes = Vec::new();
        let written = store.serialize(&mut bytes).unwrap();
        assert_eq!(bytes.len(), written);

        let AbilitiesStore {
            members_of_ability,
            specialization_to_root,
            ability_members,
            declared_implementations,
            specializations,
            next_specialization_id,
            resolved_specializations,
        } = store;

        let (de_store, offset) = AbilitiesStore::deserialize(&bytes);

        assert_eq!(bytes.len(), offset);

        assert_eq!(members_of_ability, de_store.members_of_ability);
        assert_eq!(specialization_to_root, de_store.specialization_to_root);
        assert_eq!(ability_members, de_store.ability_members);
        assert_eq!(declared_implementations, de_store.declared_implementations);
        assert_eq!(specializations, de_store.specializations);
        assert_eq!(next_specialization_id, de_store.next_specialization_id);
        assert_eq!(resolved_specializations, de_store.resolved_specializations);
    }
}

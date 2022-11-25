use crate::num::NumericRange;
use crate::pretty_print::Parens;
use crate::subs::{
    GetSubsSlice, RecordFields, Subs, UnionTags, VarStore, Variable, VariableSubsSlice,
};
use roc_collections::all::{HumanIndex, ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_collections::soa::{Index, Slice};
use roc_collections::VecMap;
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::{ForeignSymbol, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_region::all::{Loc, Region};
use std::fmt;
use std::fmt::Write;

pub const TYPE_NUM: &str = "Num";
pub const TYPE_INTEGER: &str = "Integer";
pub const TYPE_FLOATINGPOINT: &str = "FloatingPoint";

const GREEK_LETTERS: &[char] = &[
    'α', 'ν', 'β', 'ξ', 'γ', 'ο', 'δ', 'π', 'ε', 'ρ', 'ζ', 'σ', 'η', 'τ', 'θ', 'υ', 'ι', 'φ', 'κ',
    'χ', 'λ', 'ψ', 'μ', 'ω', 'ς',
];

///
/// Intuitively
///
/// - Demanded: only introduced by pattern matches, e.g. { x } ->
///     Cannot unify with an Optional field, but can unify with a Required field
/// - Required: introduced by record literals
///     Can unify with Optional and Demanded
/// - Optional: introduced by pattern matches, e.g. { x ? "" } ->
///     Can unify with Required, but not with Demanded
/// - RigidRequired: introduced by annotations, e.g. { x : Str}
///     Can only unify with Required and Demanded, to prevent an optional field being typed as Required
/// - RigidOptional: introduced by annotations, e.g. { x ? Str}
///     Can only unify with Optional, to prevent a required field being typed as Optional
#[derive(PartialEq, Eq, Clone, Hash)]
pub enum RecordField<T> {
    Demanded(T),
    Required(T),
    Optional(T),
    RigidRequired(T),
    RigidOptional(T),
}

impl<T: Copy> Copy for RecordField<T> {}

impl<T: fmt::Debug> fmt::Debug for RecordField<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RecordField::*;

        match self {
            Optional(typ) => write!(f, "Optional({:?})", typ),
            Required(typ) => write!(f, "Required({:?})", typ),
            Demanded(typ) => write!(f, "Demanded({:?})", typ),
            RigidRequired(typ) => write!(f, "RigidRequired({:?})", typ),
            RigidOptional(typ) => write!(f, "RigidOptional({:?})", typ),
        }
    }
}

impl<T> RecordField<T> {
    pub fn into_inner(self) -> T {
        use RecordField::*;

        match self {
            Optional(t) => t,
            Required(t) => t,
            Demanded(t) => t,
            RigidRequired(t) => t,
            RigidOptional(t) => t,
        }
    }

    pub fn as_inner(&self) -> &T {
        use RecordField::*;

        match self {
            Optional(t) => t,
            Required(t) => t,
            Demanded(t) => t,
            RigidRequired(t) => t,
            RigidOptional(t) => t,
        }
    }

    pub fn as_inner_mut(&mut self) -> &mut T {
        use RecordField::*;

        match self {
            Optional(t) => t,
            Required(t) => t,
            Demanded(t) => t,
            RigidRequired(t) => t,
            RigidOptional(t) => t,
        }
    }

    pub fn map<F, U>(&self, f: F) -> RecordField<U>
    where
        F: FnOnce(&T) -> U,
    {
        self.replace(f(self.as_inner()))
    }

    pub fn map_owned<F, U>(self, f: F) -> RecordField<U>
    where
        F: FnOnce(T) -> U,
    {
        use RecordField::*;
        match self {
            Optional(t) => Optional(f(t)),
            Required(t) => Required(f(t)),
            Demanded(t) => Demanded(f(t)),
            RigidRequired(t) => RigidRequired(f(t)),
            RigidOptional(t) => RigidOptional(f(t)),
        }
    }

    pub fn replace<U>(&self, u: U) -> RecordField<U> {
        use RecordField::*;
        match self {
            Optional(_) => Optional(u),
            Required(_) => Required(u),
            Demanded(_) => Demanded(u),
            RigidRequired(_) => RigidRequired(u),
            RigidOptional(_) => RigidOptional(u),
        }
    }

    pub fn is_optional(&self) -> bool {
        matches!(
            self,
            RecordField::Optional(..) | RecordField::RigidOptional(..)
        )
    }
}

impl RecordField<Type> {
    pub fn substitute(&mut self, substitutions: &ImMap<Variable, Type>) {
        use RecordField::*;

        match self {
            Optional(typ) => typ.substitute(substitutions),
            Required(typ) => typ.substitute(substitutions),
            Demanded(typ) => typ.substitute(substitutions),
            RigidRequired(typ) => typ.substitute(substitutions),
            RigidOptional(typ) => typ.substitute(substitutions),
        }
    }

    pub fn substitute_alias(
        &mut self,
        rep_symbol: Symbol,
        rep_args: &[Type],
        actual: &Type,
    ) -> Result<(), Region> {
        use RecordField::*;

        match self {
            Optional(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
            Required(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
            Demanded(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
            RigidRequired(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
            RigidOptional(typ) => typ.substitute_alias(rep_symbol, rep_args, actual),
        }
    }

    pub fn instantiate_aliases<'a, F>(
        &mut self,
        region: Region,
        aliases: &'a F,
        var_store: &mut VarStore,
        new_lambda_sets: &mut ImSet<Variable>,
        new_infer_ext_vars: &mut ImSet<Variable>,
    ) where
        F: Fn(Symbol) -> Option<&'a Alias>,
    {
        self.as_inner_mut().instantiate_aliases(
            region,
            aliases,
            var_store,
            new_lambda_sets,
            new_infer_ext_vars,
        )
    }

    pub fn contains_symbol(&self, rep_symbol: Symbol) -> bool {
        use RecordField::*;

        match self {
            Optional(typ) => typ.contains_symbol(rep_symbol),
            Required(typ) => typ.contains_symbol(rep_symbol),
            Demanded(typ) => typ.contains_symbol(rep_symbol),
            RigidRequired(typ) => typ.contains_symbol(rep_symbol),
            RigidOptional(typ) => typ.contains_symbol(rep_symbol),
        }
    }
    pub fn contains_variable(&self, rep_variable: Variable) -> bool {
        use RecordField::*;

        match self {
            Optional(typ) => typ.contains_variable(rep_variable),
            Required(typ) => typ.contains_variable(rep_variable),
            Demanded(typ) => typ.contains_variable(rep_variable),
            RigidRequired(typ) => typ.contains_variable(rep_variable),
            RigidOptional(typ) => typ.contains_variable(rep_variable),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LambdaSet(pub Type);

impl LambdaSet {
    pub fn as_inner(&self) -> &Type {
        &self.0
    }

    fn as_inner_mut(&mut self) -> &mut Type {
        &mut self.0
    }

    fn instantiate_aliases<'a, F>(
        &mut self,
        region: Region,
        aliases: &'a F,
        var_store: &mut VarStore,
        new_lambda_sets: &mut ImSet<Variable>,
        new_infer_ext_vars: &mut ImSet<Variable>,
    ) where
        F: Fn(Symbol) -> Option<&'a Alias>,
    {
        self.0.instantiate_aliases(
            region,
            aliases,
            var_store,
            new_lambda_sets,
            new_infer_ext_vars,
        )
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct AliasCommon {
    pub symbol: Symbol,
    pub type_arguments: Vec<Loc<OptAbleType>>,
    pub lambda_set_variables: Vec<LambdaSet>,
    pub infer_ext_in_output_types: Vec<Type>,
}

/// Represents a collection of abilities bound to a type variable.
///
/// Enforces the invariants
///   - There are no duplicate abilities (like a [VecSet][roc_collections::VecSet])
///   - Inserted abilities are in sorted order; they can be extracted with
///     [AbilitySet::into_sorted_iter]
///
/// This is useful for inserting into [Subs][crate::subs::Subs], so that the set need not be
/// re-sorted.
///
/// In the future we might want to do some small-vec optimizations, though that may be trivialized
/// away with a SoA representation of canonicalized types.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct AbilitySet(Vec<Symbol>);

impl AbilitySet {
    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    pub fn singleton(ability: Symbol) -> Self {
        Self(vec![ability])
    }

    pub fn insert(&mut self, ability: Symbol) -> bool {
        match self.0.binary_search(&ability) {
            Ok(_) => true,
            Err(insert_index) => {
                self.0.insert(insert_index, ability);
                false
            }
        }
    }

    pub fn contains(&self, ability: &Symbol) -> bool {
        self.0.contains(ability)
    }

    pub fn sorted_iter(&self) -> impl ExactSizeIterator<Item = &Symbol> {
        self.0.iter()
    }

    pub fn into_sorted_iter(self) -> impl ExactSizeIterator<Item = Symbol> {
        self.0.into_iter()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl FromIterator<Symbol> for AbilitySet {
    fn from_iter<T: IntoIterator<Item = Symbol>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let (lo, hi) = iter.size_hint();
        let mut this = Self::with_capacity(hi.unwrap_or(lo));
        for item in iter {
            this.insert(item);
        }
        this
    }
}

#[derive(Clone, Debug)]
pub struct OptAbleVar {
    pub var: Variable,
    pub opt_abilities: Option<AbilitySet>,
}

impl OptAbleVar {
    pub fn unbound(var: Variable) -> Self {
        Self {
            var,
            opt_abilities: None,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct OptAbleType {
    pub typ: Type,
    pub opt_abilities: Option<AbilitySet>,
}

impl OptAbleType {
    pub fn unbound(typ: Type) -> Self {
        Self {
            typ,
            opt_abilities: None,
        }
    }
}

/// Polarity of a type, or roughly, what side of an arrow it appears on.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Polarity {
    /// A type that appears in negative/input position
    Neg,
    /// A type that appears in positive/output position
    Pos,
}

impl std::ops::Neg for Polarity {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Polarity::Neg => todo!(),
            Polarity::Pos => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct AliasShared {
    pub symbol: Symbol,
    pub type_argument_abilities: Slice<AbilitySet>,
    pub type_argument_regions: Slice<Region>,
    pub lambda_set_variables: Slice<TypeTag>,
    pub infer_ext_in_output_variables: Slice<TypeTag>,
}

/// The tag (head constructor) of a canonical type stored in [Types].
#[derive(Debug, Clone, Copy)]
pub enum TypeTag {
    EmptyRecord,
    EmptyTagUnion,
    /// The arguments are implicit
    Function(
        /// lambda set
        Index<TypeTag>,
        /// return type
        Index<TypeTag>,
    ),
    /// Closure arguments are implicit
    ClosureTag {
        name: Symbol,
        ambient_function: Variable,
    },
    // type extension is implicit
    // tag name is in the `single_tag_union_tag_names` map
    FunctionOrTagUnion(Symbol),
    UnspecializedLambdaSet {
        unspecialized: Uls,
    },
    DelayedAlias {
        shared: Index<AliasShared>,
    },
    StructuralAlias {
        shared: Index<AliasShared>,
        actual: Index<TypeTag>,
    },
    OpaqueAlias {
        shared: Index<AliasShared>,
        actual: Index<TypeTag>,
    },
    HostExposedAlias {
        shared: Index<AliasShared>,
        actual_type: Index<TypeTag>,
        actual_variable: Variable,
    },

    Apply {
        symbol: Symbol,
        // type_argument_types: Slice<TypeTag>, implicit
        type_argument_regions: Slice<Region>,
        region: Region, // IDEA: make implicit, final element of `type_argument_regions`
    },
    Variable(Variable),
    RangedNumber(NumericRange),
    /// A type error, which will code gen to a runtime error
    /// The problem is at the index of the type tag
    Error,

    // TypeExtension is implicit in the type slice
    // it is length zero for closed, length 1 for open
    TagUnion(UnionTags),
    RecursiveTagUnion(Variable, UnionTags),
    Record(RecordFields),
}

/// Look-aside slice of types used in [Types], when the slice does not correspond to the direct
/// type arguments of a [TypeTag].
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct AsideTypeSlice(Slice<TypeTag>);

impl AsideTypeSlice {
    pub fn into_iter(&self) -> impl Iterator<Item = Index<TypeTag>> {
        self.0.into_iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Memory-dense storage of canonicalized types, sitting between the user-facing type syntax and
/// the [type solving representation][crate::subs::Content] of types.
///
/// See [TypeTag].
#[derive(Debug)]
pub struct Types {
    // main storage. Each type is represented by a tag, which is identified by its index.
    // `tags_slices` is a parallel array (so these two vectors always have the same size), that
    // allows storing a slice of types. This is used for storing the function argument types, or
    // the extension parameter of tag unions/records.
    tags: Vec<TypeTag>,
    tags_slices: Vec<Slice<TypeTag>>,

    // used to store other slices of types that are not the "main" arguments of a type stored in
    // `tags_slices`.
    aside_types_slices: Vec<Slice<TypeTag>>,

    // region info where appropriate (retained for generating error messages)
    regions: Vec<Region>,

    // tag unions
    tag_names: Vec<TagName>,

    // records
    field_types: Vec<RecordField<()>>,
    field_names: Vec<Lowercase>,

    // aliases
    type_arg_abilities: Vec<AbilitySet>, // TODO: structural sharing for `AbilitySet`s themselves
    aliases: Vec<AliasShared>,

    // these tag types are relatively rare, and so we store them in a way that reduces space, at
    // the cost of slightly higher lookup time
    single_tag_union_tag_names: VecMap<Index<TypeTag>, TagName>,
}

impl Default for Types {
    fn default() -> Self {
        Self::new()
    }
}

impl Types {
    pub const EMPTY_RECORD: Index<TypeTag> = Index::new(0);
    const EMPTY_RECORD_TAG: TypeTag = TypeTag::Variable(Variable::EMPTY_RECORD);
    const EMPTY_RECORD_ARGS: Slice<TypeTag> = Slice::empty();

    pub const EMPTY_TAG_UNION: Index<TypeTag> = Index::new(1);
    const EMPTY_TAG_UNION_TAG: TypeTag = TypeTag::Variable(Variable::EMPTY_TAG_UNION);
    const EMPTY_TAG_UNION_ARGS: Slice<TypeTag> = Slice::empty();

    pub const STR: Index<TypeTag> = Index::new(2);
    const STR_TAG: TypeTag = TypeTag::Variable(Variable::STR);
    const STR_ARGS: Slice<TypeTag> = Slice::empty();

    pub fn new() -> Self {
        Self {
            // tags.len() == tags_slices.len()
            tags: vec![
                Self::EMPTY_RECORD_TAG,
                Self::EMPTY_TAG_UNION_TAG,
                Self::STR_TAG,
            ],
            tags_slices: vec![
                Self::EMPTY_RECORD_ARGS,
                Self::EMPTY_TAG_UNION_ARGS,
                Self::STR_ARGS,
            ],

            aside_types_slices: Default::default(),

            regions: Default::default(),
            tag_names: Default::default(),
            field_types: Default::default(),
            field_names: Default::default(),
            type_arg_abilities: Default::default(),
            aliases: Default::default(),
            single_tag_union_tag_names: Default::default(),
        }
    }

    #[cfg(debug_assertions)]
    pub fn dbg(&self, tag: Index<TypeTag>) -> impl std::fmt::Debug + '_ {
        debug_types::DebugTag(self, tag)
    }

    pub fn get_type_arguments(&self, tag: Index<TypeTag>) -> Slice<TypeTag> {
        self.tags_slices[tag.index()]
    }

    #[track_caller]
    pub fn get_tag_name(&self, typ: &Index<TypeTag>) -> &TagName {
        self.single_tag_union_tag_names
            .get(typ)
            .expect("typ is not a single tag union")
    }

    pub fn record_fields_slices(
        &self,
        fields: RecordFields,
    ) -> (Slice<Lowercase>, Slice<RecordField<()>>, Slice<TypeTag>) {
        let RecordFields {
            length,
            field_names_start,
            variables_start,
            field_types_start,
        } = fields;

        let names = Slice::new(field_names_start, length);
        let fields = Slice::new(field_types_start, length);
        let tys = Slice::new(variables_start, length);

        (names, fields, tys)
    }

    pub fn union_tag_slices(&self, union: UnionTags) -> (Slice<TagName>, Slice<AsideTypeSlice>) {
        let UnionTags {
            length,
            labels_start,
            values_start,
            _marker,
        } = union;

        let tags = Slice::new(labels_start, length);
        let payload_slices = Slice::new(values_start, length);

        (tags, payload_slices)
    }

    /// # Safety
    ///
    /// May only be called if `var` is known to represent the type at `index`.
    #[must_use]
    pub unsafe fn emplace_variable(&mut self, index: Index<TypeTag>, var: Variable) -> TypeTag {
        std::mem::replace(&mut self.tags[index.index()], TypeTag::Variable(var))
    }

    fn reserve_type_tags(&mut self, length: usize) -> Slice<TypeTag> {
        use std::iter::repeat;

        debug_assert_eq!(self.tags.len(), self.tags_slices.len());

        self.tags_slices
            .extend(repeat(Slice::default()).take(length));

        Slice::extend_new(&mut self.tags, repeat(TypeTag::EmptyRecord).take(length))
    }

    fn reserve_type_tag(&mut self) -> Index<TypeTag> {
        debug_assert_eq!(self.tags.len(), self.tags_slices.len());

        self.tags_slices.push(Slice::default());

        Index::push_new(&mut self.tags, TypeTag::EmptyRecord)
    }

    fn set_type_tag(&mut self, index: Index<TypeTag>, tag: TypeTag, type_slice: Slice<TypeTag>) {
        debug_assert_eq!(self.tags.len(), self.tags_slices.len());

        self.tags[index.index()] = tag;
        self.tags_slices[index.index()] = type_slice;
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn from_old_type_slice<B>(
        &mut self,
        // evil, but allows us to emulate reference-polymorphism
        old: impl ExactSizeIterator<Item = B>,
    ) -> Slice<TypeTag>
    where
        B: std::borrow::Borrow<Type>,
    {
        let slice = self.reserve_type_tags(old.len());

        for (index, argument) in slice.into_iter().zip(old) {
            self.from_old_type_at(index, argument.borrow());
        }

        slice
    }

    fn tag_union_help(
        &mut self,
        tags: &[(TagName, Vec<Type>)],
        extension: &TypeExtension,
    ) -> (UnionTags, Slice<TypeTag>) {
        let tag_names_slice =
            Slice::extend_new(&mut self.tag_names, tags.iter().map(|(n, _)| n.clone()));

        // Store the payload slices in the aside buffer
        let type_slices = Slice::extend_new(
            &mut self.aside_types_slices,
            std::iter::repeat(Slice::default()).take(tags.len()),
        );

        for (slice_index, (_, types)) in type_slices.indices().zip(tags) {
            self.aside_types_slices[slice_index] = self.from_old_type_slice(types.iter());
        }

        let union_tags = UnionTags {
            length: tags.len() as u16,
            labels_start: tag_names_slice.start() as u32,
            values_start: type_slices.start() as u32,
            _marker: std::marker::PhantomData,
        };

        let type_slice = match extension {
            TypeExtension::Open(ext) => self.from_old_type(ext).as_slice(),
            TypeExtension::Closed => Slice::default(),
        };

        (union_tags, type_slice)
    }

    fn alias_shared_help(
        &mut self,
        symbol: Symbol,
        type_arguments: &[OptAbleType],
        lambda_set_variables: &[LambdaSet],
        infer_ext_in_output_types: &[Type],
    ) -> AliasShared {
        let lambda_set_slice = {
            let slice = self.reserve_type_tags(lambda_set_variables.len());

            for (index, argument) in slice.into_iter().zip(lambda_set_variables) {
                self.from_old_type_at(index, &argument.0);
            }

            Slice::new(slice.start() as _, slice.len() as _)
        };

        let infer_ext_in_output_slice = {
            let slice = self.reserve_type_tags(infer_ext_in_output_types.len());

            for (index, ty) in slice.into_iter().zip(infer_ext_in_output_types) {
                self.from_old_type_at(index, ty);
            }

            Slice::new(slice.start() as _, slice.len() as _)
        };

        let type_argument_abilities = Slice::extend_new(
            &mut self.type_arg_abilities,
            type_arguments
                .iter()
                .map(|a| a.opt_abilities.as_ref().cloned().unwrap_or_default()),
        );

        // TODO: populate correctly
        let type_argument_regions = Slice::extend_new(
            &mut self.regions,
            std::iter::repeat(Region::zero()).take(type_arguments.len()),
        );

        AliasShared {
            symbol,
            type_argument_abilities,
            type_argument_regions,
            lambda_set_variables: lambda_set_slice,
            infer_ext_in_output_variables: infer_ext_in_output_slice,
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn from_old_type(&mut self, old: &Type) -> Index<TypeTag> {
        let index = self.reserve_type_tag();
        self.from_old_type_at(index, old);
        index
    }

    pub fn function(
        &mut self,
        arguments: Slice<TypeTag>,
        lambda_set: Index<TypeTag>,
        ret: Index<TypeTag>,
    ) -> Index<TypeTag> {
        let index = self.reserve_type_tag();

        let tag = TypeTag::Function(lambda_set, ret);
        self.set_type_tag(index, tag, arguments);
        index
    }

    #[allow(clippy::wrong_self_convention)]
    fn from_old_type_at(&mut self, index: Index<TypeTag>, old: &Type) {
        match old {
            Type::EmptyRec => self.set_type_tag(index, TypeTag::EmptyRecord, Slice::default()),
            Type::EmptyTagUnion => {
                self.set_type_tag(index, TypeTag::EmptyTagUnion, Slice::default())
            }
            Type::Function(arguments, lambda_set, return_type) => {
                let argument_slice = self.from_old_type_slice(arguments.iter());

                let tag = TypeTag::Function(
                    self.from_old_type(lambda_set),
                    self.from_old_type(return_type),
                );

                self.set_type_tag(index, tag, argument_slice)
            }
            Type::Apply(symbol, arguments, region) => {
                let type_argument_regions =
                    Slice::extend_new(&mut self.regions, arguments.iter().map(|t| t.region));

                let type_slice = {
                    let slice = self.reserve_type_tags(arguments.len());

                    for (index, argument) in slice.into_iter().zip(arguments) {
                        self.from_old_type_at(index, &argument.value);
                    }

                    slice
                };

                self.set_type_tag(
                    index,
                    TypeTag::Apply {
                        symbol: *symbol,
                        type_argument_regions,
                        region: *region,
                    },
                    type_slice,
                )
            }
            Type::TagUnion(tags, extension) => {
                let (union_tags, type_slice) = self.tag_union_help(tags, extension);

                self.set_type_tag(index, TypeTag::TagUnion(union_tags), type_slice)
            }
            Type::RecursiveTagUnion(rec_var, tags, extension) => {
                let (union_tags, type_slice) = self.tag_union_help(tags, extension);
                let tag = TypeTag::RecursiveTagUnion(*rec_var, union_tags);

                self.set_type_tag(index, tag, type_slice)
            }
            Type::FunctionOrTagUnion(tag_name, symbol, extension) => {
                let type_slice = match extension {
                    TypeExtension::Open(ext) => self.from_old_type(ext).as_slice(),
                    TypeExtension::Closed => Slice::default(),
                };

                self.single_tag_union_tag_names
                    .insert(index, tag_name.clone());

                let tag = TypeTag::FunctionOrTagUnion(*symbol);
                self.set_type_tag(index, tag, type_slice)
            }
            Type::UnspecializedLambdaSet { unspecialized } => {
                let tag = TypeTag::UnspecializedLambdaSet {
                    unspecialized: *unspecialized,
                };
                self.set_type_tag(index, tag, Slice::default())
            }
            Type::Record(fields, extension) => {
                let type_slice = match extension {
                    TypeExtension::Open(ext) => self.from_old_type(ext).as_slice(),
                    TypeExtension::Closed => Slice::default(),
                };

                // should we sort at this point?
                let field_type_slice = {
                    let slice = self.reserve_type_tags(fields.len());

                    for (index, argument) in slice.into_iter().zip(fields.values()) {
                        self.from_old_type_at(index, argument.as_inner());
                    }

                    slice
                };

                let field_types = Slice::extend_new(
                    &mut self.field_types,
                    fields.values().map(|f| f.map(|_| ())),
                );

                let field_names = Slice::extend_new(&mut self.field_names, fields.keys().cloned());

                let record_fields = RecordFields {
                    length: fields.len() as u16,
                    field_names_start: field_names.start() as u32,
                    variables_start: field_type_slice.start() as u32,
                    field_types_start: field_types.start() as u32,
                };

                let tag = TypeTag::Record(record_fields);
                self.set_type_tag(index, tag, type_slice)
            }
            Type::ClosureTag {
                name,
                captures,
                ambient_function,
            } => {
                let type_slice = self.from_old_type_slice(captures.iter());

                let tag = TypeTag::ClosureTag {
                    name: *name,
                    ambient_function: *ambient_function,
                };
                self.set_type_tag(index, tag, type_slice)
            }

            Type::DelayedAlias(AliasCommon {
                symbol,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types,
            }) => {
                let type_argument_regions =
                    Slice::extend_new(&mut self.regions, type_arguments.iter().map(|t| t.region));

                let type_arguments_slice = {
                    let slice = self.reserve_type_tags(type_arguments.len());

                    for (index, argument) in slice.into_iter().zip(type_arguments) {
                        self.from_old_type_at(index, &argument.value.typ);
                    }

                    slice
                };

                let lambda_set_slice = {
                    let slice = self.reserve_type_tags(lambda_set_variables.len());

                    let it = slice.into_iter().zip(lambda_set_variables);
                    for (index, argument) in it {
                        self.from_old_type_at(index, &argument.0);
                    }

                    Slice::new(slice.start() as _, slice.len() as _)
                };

                let infer_ext_in_output_slice = {
                    let slice = self.reserve_type_tags(infer_ext_in_output_types.len());

                    let it = slice.into_iter().zip(infer_ext_in_output_types);
                    for (index, argument) in it {
                        self.from_old_type_at(index, argument);
                    }

                    Slice::new(slice.start() as _, slice.len() as _)
                };

                let type_argument_abilities = Slice::extend_new(
                    &mut self.type_arg_abilities,
                    type_arguments
                        .iter()
                        .map(|a| a.value.opt_abilities.as_ref().cloned().unwrap_or_default()),
                );

                let alias_shared = AliasShared {
                    symbol: *symbol,
                    type_argument_abilities,
                    type_argument_regions,
                    lambda_set_variables: lambda_set_slice,
                    infer_ext_in_output_variables: infer_ext_in_output_slice,
                };

                let shared = Index::push_new(&mut self.aliases, alias_shared);

                let tag = TypeTag::DelayedAlias { shared };

                self.set_type_tag(index, tag, type_arguments_slice)
            }
            Type::Alias {
                symbol,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types,
                actual,
                kind,
            } => {
                let type_arguments_slice = {
                    let slice = self.reserve_type_tags(type_arguments.len());

                    for (index, argument) in slice.into_iter().zip(type_arguments) {
                        self.from_old_type_at(index, &argument.typ);
                    }

                    slice
                };

                let alias_shared = self.alias_shared_help(
                    *symbol,
                    type_arguments,
                    lambda_set_variables,
                    infer_ext_in_output_types,
                );

                let shared = Index::push_new(&mut self.aliases, alias_shared);
                let actual = self.from_old_type(actual);

                let tag = match kind {
                    AliasKind::Structural => TypeTag::StructuralAlias { shared, actual },
                    AliasKind::Opaque => TypeTag::OpaqueAlias { shared, actual },
                };

                self.set_type_tag(index, tag, type_arguments_slice)
            }

            Type::HostExposedAlias {
                name,
                type_arguments,
                lambda_set_variables,
                actual_var,
                actual,
            } => {
                let type_arguments_slice = self.from_old_type_slice(type_arguments.iter());

                let lambda_set_slice = {
                    let slice = self.reserve_type_tags(lambda_set_variables.len());

                    for (index, argument) in slice.into_iter().zip(lambda_set_variables) {
                        self.from_old_type_at(index, &argument.0);
                    }

                    Slice::new(slice.start() as _, slice.len() as _)
                };

                let alias_shared = AliasShared {
                    symbol: *name,
                    type_argument_abilities: Slice::default(),
                    type_argument_regions: Slice::default(),
                    lambda_set_variables: lambda_set_slice,
                    infer_ext_in_output_variables: Slice::default(),
                };

                let tag = TypeTag::HostExposedAlias {
                    shared: Index::push_new(&mut self.aliases, alias_shared),
                    actual_type: self.from_old_type(actual),
                    actual_variable: *actual_var,
                };

                self.set_type_tag(index, tag, type_arguments_slice)
            }
            Type::Variable(var) => {
                self.set_type_tag(index, TypeTag::Variable(*var), Slice::default())
            }
            Type::RangedNumber(range) => {
                self.set_type_tag(index, TypeTag::RangedNumber(*range), Slice::default())
            }
            Type::Error => self.set_type_tag(index, TypeTag::Error, Slice::default()),
        }
    }

    /// Creates a deep clone of a type with substituted variables.
    pub fn clone_with_variable_substitutions(
        &mut self,
        typ: Index<TypeTag>,
        subs: &MutMap<Variable, Variable>,
    ) -> Index<TypeTag> {
        let cloned = self.reserve_type_tag();

        let mut stack = vec![(cloned, typ)];

        macro_rules! defer {
            ($type_index:expr) => {{
                let cloned_index = self.reserve_type_tag();
                stack.push((cloned_index, $type_index));
                cloned_index
            }};
        }

        macro_rules! defer_slice {
            ($type_slice:expr) => {{
                let cloned_indices = self.reserve_type_tags($type_slice.len());
                debug_assert_eq!(cloned_indices.len(), $type_slice.len());
                stack.extend(cloned_indices.into_iter().zip($type_slice.into_iter()));
                cloned_indices
            }};
        }

        macro_rules! subst {
            ($var:expr) => {{
                subs.get(&$var).copied().unwrap_or($var)
            }};
        }

        macro_rules! do_shared {
            ($shared:expr) => {{
                let AliasShared {
                    symbol,
                    type_argument_abilities,
                    type_argument_regions,
                    lambda_set_variables,
                    infer_ext_in_output_variables,
                } = self[$shared];

                let new_lambda_set_variables = defer_slice!(lambda_set_variables);
                let new_infer_ext_in_output_variables = defer_slice!(infer_ext_in_output_variables);

                let new_shared = AliasShared {
                    symbol,
                    type_argument_abilities,
                    type_argument_regions,
                    lambda_set_variables: new_lambda_set_variables,
                    infer_ext_in_output_variables: new_infer_ext_in_output_variables,
                };
                Index::push_new(&mut self.aliases, new_shared)
            }};
        }

        macro_rules! do_union_tags {
            ($union_tags:expr) => {{
                let (tags, payload_slices) = self.union_tag_slices($union_tags);

                let new_payload_slices = Slice::extend_new(
                    &mut self.aside_types_slices,
                    std::iter::repeat(Slice::default()).take(payload_slices.len()),
                );
                for (new_payload_slice_index, payload_slice_index) in
                    (new_payload_slices.indices()).zip(payload_slices.into_iter())
                {
                    let payload_slice = self[payload_slice_index];
                    let new_payload_slice = defer_slice!(payload_slice);
                    self.aside_types_slices[new_payload_slice_index] = new_payload_slice;
                }

                UnionTags {
                    length: tags.len() as _,
                    labels_start: tags.start() as _,
                    values_start: new_payload_slices.start() as _,
                    _marker: Default::default(),
                }
            }};
        }

        while let Some((dest_index, typ)) = stack.pop() {
            use TypeTag::*;

            let (tag, args) = match self[typ] {
                Variable(v) => (Variable(subst!(v)), Default::default()),
                EmptyRecord => (EmptyRecord, Default::default()),
                EmptyTagUnion => (EmptyTagUnion, Default::default()),
                Function(clos, ret) => {
                    let args = self.get_type_arguments(typ);

                    let new_args = defer_slice!(args);
                    let new_clos = defer!(clos);
                    let new_ret = defer!(ret);

                    (Function(new_clos, new_ret), new_args)
                }
                ClosureTag {
                    name,
                    ambient_function,
                } => {
                    let captures = self.get_type_arguments(typ);

                    let new_captures = defer_slice!(captures);
                    let new_ambient_function = subst!(ambient_function);

                    (
                        ClosureTag {
                            name,
                            ambient_function: new_ambient_function,
                        },
                        new_captures,
                    )
                }
                FunctionOrTagUnion(symbol) => {
                    let ext = self.get_type_arguments(typ);

                    let new_ext = defer_slice!(ext);
                    self.single_tag_union_tag_names
                        .insert(dest_index, self.get_tag_name(&typ).clone());

                    (FunctionOrTagUnion(symbol), new_ext)
                }
                UnspecializedLambdaSet {
                    unspecialized: Uls(var, sym, region),
                } => {
                    let new_var = subst!(var);

                    (
                        UnspecializedLambdaSet {
                            unspecialized: Uls(new_var, sym, region),
                        },
                        Default::default(),
                    )
                }
                DelayedAlias { shared } => {
                    let type_arguments = self.get_type_arguments(typ);

                    let new_type_arguments = defer_slice!(type_arguments);
                    let new_shared = do_shared!(shared);

                    (DelayedAlias { shared: new_shared }, new_type_arguments)
                }
                StructuralAlias { shared, actual } => {
                    let type_arguments = self.get_type_arguments(typ);

                    let new_type_arguments = defer_slice!(type_arguments);
                    let new_shared = do_shared!(shared);
                    let new_actual = defer!(actual);

                    (
                        StructuralAlias {
                            shared: new_shared,
                            actual: new_actual,
                        },
                        new_type_arguments,
                    )
                }
                OpaqueAlias { shared, actual } => {
                    let type_arguments = self.get_type_arguments(typ);

                    let new_type_arguments = defer_slice!(type_arguments);
                    let new_shared = do_shared!(shared);
                    let new_actual = defer!(actual);

                    (
                        OpaqueAlias {
                            shared: new_shared,
                            actual: new_actual,
                        },
                        new_type_arguments,
                    )
                }
                HostExposedAlias {
                    shared,
                    actual_type,
                    actual_variable,
                } => {
                    let type_arguments = self.get_type_arguments(typ);

                    let new_type_arguments = defer_slice!(type_arguments);
                    let new_shared = do_shared!(shared);
                    let new_actual_type = defer!(actual_type);
                    let new_actual_variable = subst!(actual_variable);

                    (
                        HostExposedAlias {
                            shared: new_shared,
                            actual_type: new_actual_type,
                            actual_variable: new_actual_variable,
                        },
                        new_type_arguments,
                    )
                }
                Apply {
                    symbol,
                    type_argument_regions,
                    region,
                } => {
                    let type_arguments = self.get_type_arguments(typ);

                    let new_type_arguments = defer_slice!(type_arguments);

                    (
                        Apply {
                            symbol,
                            type_argument_regions,
                            region,
                        },
                        new_type_arguments,
                    )
                }
                TagUnion(union_tags) => {
                    let ext_slice = self.get_type_arguments(typ);

                    let new_ext_slice = defer_slice!(ext_slice);
                    let new_union_tags = do_union_tags!(union_tags);

                    (TagUnion(new_union_tags), new_ext_slice)
                }
                RecursiveTagUnion(rec_var, union_tags) => {
                    let ext_slice = self.get_type_arguments(typ);

                    let new_rec_var = subst!(rec_var);
                    let new_ext_slice = defer_slice!(ext_slice);
                    let new_union_tags = do_union_tags!(union_tags);

                    (
                        RecursiveTagUnion(new_rec_var, new_union_tags),
                        new_ext_slice,
                    )
                }
                Record(fields) => {
                    let ext_slice = self.get_type_arguments(typ);
                    let (names, fields, tys) = self.record_fields_slices(fields);

                    debug_assert_eq!(names.len(), fields.len());
                    debug_assert_eq!(names.len(), tys.len());

                    let new_tys = defer_slice!(tys);
                    let new_ext_slice = defer_slice!(ext_slice);

                    let new_record_fields = RecordFields {
                        length: names.len() as _,
                        field_names_start: names.start() as _,
                        variables_start: new_tys.start() as _,
                        field_types_start: fields.start() as _,
                    };

                    (Record(new_record_fields), new_ext_slice)
                }
                RangedNumber(range) => (RangedNumber(range), Default::default()),
                Error => (Error, Default::default()),
            };

            self.set_type_tag(dest_index, tag, args);
        }

        cloned
    }
}

#[cfg(debug_assertions)]
mod debug_types {
    use std::fmt::Display;

    use crate::{
        subs::UnionLabels,
        types::{AliasShared, RecordField, Uls},
    };

    use super::{TypeTag, Types};
    use roc_collections::soa::{Index, Slice};
    use roc_module::ident::TagName;
    use ven_pretty::{Arena, DocAllocator, DocBuilder};

    pub struct DebugTag<'a>(pub &'a Types, pub Index<TypeTag>);

    impl<'a> std::fmt::Debug for DebugTag<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let DebugTag(types, idx) = self;
            let fmt = Arena::new();
            typ(types, &fmt, TPrec::Free, *idx).1.pretty(80).fmt(f)
        }
    }

    #[derive(PartialEq, PartialOrd)]
    enum TPrec {
        Free,
        Arg,
    }

    macro_rules! maybe_paren {
        ($paren_if_above:expr, $my_prec:expr, $doc:expr) => {
            maybe_paren!($paren_if_above, $my_prec, || true, $doc)
        };
        ($paren_if_above:expr, $my_prec:expr, $extra_cond:expr, $doc:expr) => {
            if $my_prec > $paren_if_above && $extra_cond() {
                $doc.parens().group()
            } else {
                $doc
            }
        };
    }

    fn typ<'a>(
        types: &'a Types,
        f: &'a Arena<'a>,
        p: TPrec,
        tag: Index<TypeTag>,
    ) -> DocBuilder<'a, Arena<'a>> {
        use TPrec::*;
        let group = match types[tag] {
            TypeTag::EmptyRecord => f.text("{}"),
            TypeTag::EmptyTagUnion => f.text("[]"),
            TypeTag::Function(clos, ret) => {
                let args = types.get_type_arguments(tag);
                maybe_paren!(
                    Free,
                    p,
                    f.intersperse(
                        args.into_iter().map(|a| typ(types, f, Arg, a)),
                        f.text(", "),
                    )
                    .append(f.text(" -"))
                    .append(typ(types, f, Free, clos))
                    .append(f.text("->"))
                    .append(f.line())
                    .append(typ(types, f, Arg, ret))
                    .nest(2)
                )
            }
            TypeTag::ClosureTag {
                name,
                ambient_function,
            } => {
                let captures = types.get_type_arguments(tag);
                f.text("[")
                    .append(
                        f.intersperse(
                            Some(f.text(format!("{name:?}")))
                                .into_iter()
                                .chain(captures.into_iter().map(|c| typ(types, f, Free, c))),
                            f.text(" "),
                        ),
                    )
                    .append(f.text(format!(", ^{ambient_function:?}")))
                    .append(f.text("]"))
            }
            TypeTag::FunctionOrTagUnion(_) => {
                let tag_name = types.get_tag_name(&tag);
                f.text(tag_name.0.as_str())
            }
            TypeTag::UnspecializedLambdaSet {
                unspecialized: Uls(var, sym, region),
            } => f
                .text("[")
                .append(f.text(format!("{var:?}:{sym:?}:{region}")))
                .append(f.text("]")),
            TypeTag::DelayedAlias { shared } => {
                maybe_paren!(Free, p, alias(types, f, tag, shared))
            }
            TypeTag::StructuralAlias { shared, actual }
            | TypeTag::OpaqueAlias { shared, actual }
            | TypeTag::HostExposedAlias {
                shared,
                actual_type: actual,
                actual_variable: _,
            } => maybe_paren!(
                Free,
                p,
                alias(types, f, tag, shared)
                    .append(f.line())
                    .append(f.text("==> "))
                    .append(typ(types, f, Free, actual).align())
                    .nest(2)
            ),
            TypeTag::Apply {
                symbol,
                type_argument_regions: _,
                region: _,
            } => {
                let args = types.get_type_arguments(tag);
                let fmt_args = args.into_iter().map(|arg| typ(types, f, Arg, arg));
                maybe_paren!(
                    Free,
                    p,
                    f.intersperse(
                        Some(f.text(format!("{symbol:?}")))
                            .into_iter()
                            .chain(fmt_args),
                        f.text(" "),
                    )
                )
            }
            TypeTag::Variable(var) => f.text(format!("{var:?}")),
            TypeTag::RangedNumber(range) => ranged(f, range),
            TypeTag::Error => f.text("ERROR"),
            TypeTag::TagUnion(tags) => {
                tag_union(types, f, f.nil(), tags, types.get_type_arguments(tag))
            }
            TypeTag::RecursiveTagUnion(rec, tags) => tag_union(
                types,
                f,
                f.text(format!("<rec {rec:?}>")),
                tags,
                types.get_type_arguments(tag),
            ),
            TypeTag::Record(fields) => {
                let (names, kind, tys) = types.record_fields_slices(fields);
                let fmt_fields = names
                    .into_iter()
                    .zip(kind.into_iter())
                    .zip(tys.into_iter())
                    .map(|((name, kind), ty)| {
                        let (name, kind) = (&types[name], types[kind]);
                        let fmt_kind = f.text(match kind {
                            RecordField::Demanded(_) | RecordField::Required(_) => ":",
                            RecordField::Optional(_) => "?",
                            RecordField::RigidRequired(_) => "!:",
                            RecordField::RigidOptional(_) => "!?",
                        });
                        f.text(name.as_str().to_owned())
                            .append(fmt_kind)
                            .append(f.text(" "))
                            .append(typ(types, f, Free, ty))
                    });
                f.text("{").append(
                    f.intersperse(fmt_fields, f.reflow(", "))
                        .append(
                            f.text("}")
                                .append(ext(types, f, types.get_type_arguments(tag))),
                        )
                        .group()
                        .align(),
                )
            }
        };
        group.group()
    }

    fn ext<'a>(
        types: &'a Types,
        f: &'a Arena<'a>,
        ext_slice: Slice<TypeTag>,
    ) -> DocBuilder<'a, Arena<'a>> {
        f.intersperse(
            ext_slice.into_iter().map(|e| typ(types, f, TPrec::Free, e)),
            f.nil(),
        )
        .group()
    }

    fn tag_union<'a>(
        types: &'a Types,
        f: &'a Arena<'a>,
        prefix: DocBuilder<'a, Arena<'a>>,
        tags: UnionLabels<TagName>,
        ext_slice: Slice<TypeTag>,
    ) -> DocBuilder<'a, Arena<'a>> {
        let (tags, payload_slices) = types.union_tag_slices(tags);
        let fmt_tags =
            tags.into_iter()
                .zip(payload_slices.into_iter())
                .map(|(tag, payload_slice_index)| {
                    let payload_slice = types[payload_slice_index];
                    let fmt_payloads = payload_slice
                        .into_iter()
                        .map(|p| typ(types, f, TPrec::Arg, p));
                    let iter = Some(f.text(types[tag].0.to_string()))
                        .into_iter()
                        .chain(fmt_payloads);
                    f.intersperse(iter, f.text(" "))
                });

        prefix.append(f.text("[")).append(
            f.intersperse(fmt_tags, f.reflow(", "))
                .append(f.text("]"))
                .append(ext(types, f, ext_slice))
                .group()
                .align(),
        )
    }

    fn alias<'a>(
        types: &'a Types,
        f: &'a Arena<'a>,
        tag: Index<TypeTag>,
        shared: Index<AliasShared>,
    ) -> DocBuilder<'a, Arena<'a>> {
        use TPrec::*;

        let AliasShared {
            symbol,
            type_argument_abilities,
            type_argument_regions: _,
            lambda_set_variables: _,
            infer_ext_in_output_variables: _,
        } = types[shared];
        let args = types.get_type_arguments(tag);
        let fmt_args = args
            .into_iter()
            .zip(type_argument_abilities.into_iter())
            .map(|(arg, abilities)| {
                let abilities = &types[abilities];
                let arg = typ(types, f, Arg, arg);
                if abilities.is_empty() {
                    return arg;
                }
                arg.append(f.text(" (+ "))
                    .append(f.intersperse(
                        abilities.sorted_iter().map(|ab| f.text(format!("{ab:?}"))),
                        f.text(", "),
                    ))
                    .append(f.text(")"))
            });
        f.intersperse(
            Some(f.text(format!("{symbol:?}")))
                .into_iter()
                .chain(fmt_args),
            f.text(" "),
        )
    }

    fn ranged<'a>(f: &'a Arena<'a>, range: crate::num::NumericRange) -> DocBuilder<'a, Arena<'a>> {
        use crate::num::IntLitWidth::*;
        use crate::num::NumericRange::*;

        let fmt_width = f.text(match range.width() {
            U8 | I8 => "8",
            U16 | I16 => "16",
            U32 | I32 => "32",
            U64 | I64 => "64",
            U128 | I128 => "128",
            Nat => "Nat",
            F32 => "F32",
            F64 => "F64",
            Dec => "Dec",
        });

        let pre = match range {
            IntAtLeastSigned(_) => "Int(- >=",
            IntAtLeastEitherSign(_) => "Int(+/- >=",
            NumAtLeastSigned(_) => "Num(- >=",
            NumAtLeastEitherSign(_) => "Num(+/- >=",
        };

        f.text(pre).append(fmt_width).append(f.text(")"))
    }
}

impl Polarity {
    pub const OF_VALUE: Polarity = Polarity::Pos;

    pub const OF_PATTERN: Polarity = Polarity::Neg;

    pub fn is_neg(&self) -> bool {
        matches!(self, Self::Neg)
    }

    pub fn is_pos(&self) -> bool {
        matches!(self, Self::Pos)
    }
}

macro_rules! impl_types_index {
    ($($field:ident, $ty:ty)*) => {$(
        impl std::ops::Index<Index<$ty>> for Types {
            type Output = $ty;

            fn index(&self, index: Index<$ty>) -> &Self::Output {
                // Validate that the types line up, so you can't accidentally
                // index into the wrong array.
                let _: &Vec<$ty> = &self.$field;

                &self.$field[index.index()]
            }
        }
    )*}
}

macro_rules! impl_types_index_slice {
    ($($field:ident, $ty:ty)*) => {$(
        impl std::ops::Index<Slice<$ty>> for Types {
            type Output = [$ty];

            fn index(&self, slice: Slice<$ty>) -> &Self::Output {
                // Validate that the types line up, so you can't accidentally
                // index into the wrong array.
                let _: &Vec<$ty> = &self.$field;

                &self.$field[slice.indices()]
            }
        }
    )*}
}

impl_types_index! {
    tags, TypeTag
    aliases, AliasShared
    type_arg_abilities, AbilitySet
    regions, Region
    tag_names, TagName
    field_types, RecordField<()>
    field_names, Lowercase
}

impl_types_index_slice! {
    tag_names, TagName
}

impl std::ops::Index<Index<AsideTypeSlice>> for Types {
    type Output = Slice<TypeTag>;

    fn index(&self, slice: Index<AsideTypeSlice>) -> &Self::Output {
        &self.aside_types_slices[slice.index()]
    }
}

impl std::ops::Index<Slice<AsideTypeSlice>> for Types {
    type Output = [Slice<TypeTag>];

    fn index(&self, slice: Slice<AsideTypeSlice>) -> &Self::Output {
        &self.aside_types_slices[slice.indices()]
    }
}

#[derive(PartialEq, Eq)]
pub enum Type {
    EmptyRec,
    EmptyTagUnion,
    /// A function. The types of its arguments, size of its closure, then the type of its return value.
    Function(Vec<Type>, Box<Type>, Box<Type>),
    Record(SendMap<Lowercase, RecordField<Type>>, TypeExtension),
    TagUnion(Vec<(TagName, Vec<Type>)>, TypeExtension),
    FunctionOrTagUnion(TagName, Symbol, TypeExtension),
    /// A function name that is used in our defunctionalization algorithm. For example in
    ///   g = \a ->
    ///     f = \{} -> a
    ///     f
    /// the closure under "f" has name "f" and captures "a".
    ClosureTag {
        name: Symbol,
        captures: Vec<Type>,
        ambient_function: Variable,
    },
    UnspecializedLambdaSet {
        unspecialized: Uls,
    },
    DelayedAlias(AliasCommon),
    Alias {
        symbol: Symbol,
        type_arguments: Vec<OptAbleType>,
        lambda_set_variables: Vec<LambdaSet>,
        infer_ext_in_output_types: Vec<Type>,
        actual: Box<Type>,
        kind: AliasKind,
    },
    HostExposedAlias {
        name: Symbol,
        type_arguments: Vec<Type>,
        lambda_set_variables: Vec<LambdaSet>,
        actual_var: Variable,
        actual: Box<Type>,
    },
    RecursiveTagUnion(Variable, Vec<(TagName, Vec<Type>)>, TypeExtension),
    /// Applying a type to some arguments (e.g. Dict.Dict String Int)
    Apply(Symbol, Vec<Loc<Type>>, Region),
    Variable(Variable),
    RangedNumber(NumericRange),
    /// A type error, which will code gen to a runtime error
    Error,
}

/// A lambda set under an arrow in a ability member signature. For example, in
///   Default has default : {} -> a | a has Default
/// the unspecialized lambda set for the arrow "{} -> a" would be `a:default:1`.
///
/// Lambda sets in member signatures are never known until those members are specialized at a
/// usage site. Unspecialized lambda sets aid us in recovering those lambda sets; when we
/// instantiate `a` with a proper type `T`, we'll know to resolve the lambda set by extracting
/// it at region "1" from the specialization of "default" for `T`.
#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Uls(pub Variable, pub Symbol, pub u8);

impl std::fmt::Debug for Uls {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Uls({:?}:{:?}:{:?})", self.0, self.1, self.2)
    }
}

static mut TYPE_CLONE_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

pub fn get_type_clone_count() -> usize {
    if cfg!(debug_assertions) {
        unsafe { TYPE_CLONE_COUNT.load(std::sync::atomic::Ordering::SeqCst) }
    } else {
        0
    }
}

impl Clone for Type {
    fn clone(&self) -> Self {
        #[cfg(debug_assertions)]
        unsafe {
            TYPE_CLONE_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
        };

        match self {
            Self::EmptyRec => Self::EmptyRec,
            Self::EmptyTagUnion => Self::EmptyTagUnion,
            Self::Function(arg0, arg1, arg2) => {
                Self::Function(arg0.clone(), arg1.clone(), arg2.clone())
            }
            Self::Record(arg0, arg1) => Self::Record(arg0.clone(), arg1.clone()),
            Self::TagUnion(arg0, arg1) => Self::TagUnion(arg0.clone(), arg1.clone()),
            Self::FunctionOrTagUnion(arg0, arg1, arg2) => {
                Self::FunctionOrTagUnion(arg0.clone(), *arg1, arg2.clone())
            }
            Self::ClosureTag {
                name,
                captures,
                ambient_function,
            } => Self::ClosureTag {
                name: *name,
                captures: captures.clone(),
                ambient_function: *ambient_function,
            },
            Self::UnspecializedLambdaSet { unspecialized } => Self::UnspecializedLambdaSet {
                unspecialized: *unspecialized,
            },
            Self::DelayedAlias(arg0) => Self::DelayedAlias(arg0.clone()),
            Self::Alias {
                symbol,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types: infer_ext_in_output_variables,
                actual,
                kind,
            } => Self::Alias {
                symbol: *symbol,
                type_arguments: type_arguments.clone(),
                lambda_set_variables: lambda_set_variables.clone(),
                infer_ext_in_output_types: infer_ext_in_output_variables.clone(),
                actual: actual.clone(),
                kind: *kind,
            },
            Self::HostExposedAlias {
                name,
                type_arguments,
                lambda_set_variables,
                actual_var,
                actual,
            } => Self::HostExposedAlias {
                name: *name,
                type_arguments: type_arguments.clone(),
                lambda_set_variables: lambda_set_variables.clone(),
                actual_var: *actual_var,
                actual: actual.clone(),
            },
            Self::RecursiveTagUnion(arg0, arg1, arg2) => {
                Self::RecursiveTagUnion(*arg0, arg1.clone(), arg2.clone())
            }
            Self::Apply(arg0, arg1, arg2) => Self::Apply(*arg0, arg1.clone(), *arg2),
            Self::Variable(arg0) => Self::Variable(*arg0),
            Self::RangedNumber(arg1) => Self::RangedNumber(*arg1),
            Self::Error => Self::Error,
        }
    }
}

impl Clone for OptAbleType {
    fn clone(&self) -> Self {
        // This passes through `Type`, so defer to that to bump the clone counter.
        Self {
            typ: self.typ.clone(),
            opt_abilities: self.opt_abilities.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum TypeExtension {
    Open(Box<Type>),
    Closed,
}

impl TypeExtension {
    #[inline(always)]
    pub fn from_type(typ: Type) -> Self {
        match typ {
            Type::EmptyTagUnion | Type::EmptyRec => Self::Closed,
            _ => Self::Open(Box::new(typ)),
        }
    }

    #[inline(always)]
    pub fn is_closed(&self) -> bool {
        match self {
            TypeExtension::Open(_) => false,
            TypeExtension::Closed => true,
        }
    }

    #[inline(always)]
    fn iter_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        match self {
            TypeExtension::Open(ext) => Some(ext.as_mut()).into_iter(),
            TypeExtension::Closed => None.into_iter(),
        }
    }
}

impl<'a> IntoIterator for &'a TypeExtension {
    type Item = &'a Type;

    type IntoIter = std::option::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            TypeExtension::Open(ext) => Some(ext.as_ref()).into_iter(),
            TypeExtension::Closed => None.into_iter(),
        }
    }
}

fn write_tags<'a>(
    f: &mut fmt::Formatter,
    tags: impl ExactSizeIterator<Item = &'a (TagName, Vec<Type>)>,
) -> fmt::Result {
    write!(f, "[")?;

    let mut it = tags.peekable();
    while let Some((label, arguments)) = it.next() {
        write!(f, "{:?}", label)?;

        for argument in arguments {
            write!(f, " {:?}", argument)?;
        }

        if it.peek().is_some() {
            write!(f, ", ")?;
        }
    }

    write!(f, "]")
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::EmptyRec => write!(f, "{{}}"),
            Type::EmptyTagUnion => write!(f, "[]"),
            Type::Function(args, closure, ret) => {
                write!(f, "Fn(")?;

                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{:?}", arg)?;
                }

                write!(f, " |{:?}|", closure)?;
                write!(f, " -> ")?;

                ret.fmt(f)?;

                write!(f, ")")
            }
            Type::Variable(var) => write!(f, "<{:?}>", var),

            Type::Apply(symbol, args, _) => {
                write!(f, "({:?}", symbol)?;

                for arg in args {
                    write!(f, " {:?}", arg)?;
                }

                write!(f, ")")
            }
            Type::Error => write!(f, "Erroneous"),
            Type::DelayedAlias(AliasCommon {
                symbol,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types,
            }) => {
                write!(f, "(DelayedAlias {:?}", symbol)?;

                for arg in type_arguments {
                    write!(f, " {:?}", arg)?;
                }

                for (lambda_set, greek_letter) in
                    lambda_set_variables.iter().zip(GREEK_LETTERS.iter())
                {
                    write!(f, " {}@{:?}", greek_letter, lambda_set.0)?;
                }

                for (i, infer_ext) in infer_ext_in_output_types.iter().enumerate() {
                    write!(f, " `{}@{:?}", i, infer_ext)?;
                }

                write!(f, ")")?;

                Ok(())
            }

            Type::Alias {
                symbol,
                type_arguments,
                lambda_set_variables,
                actual: _actual,
                ..
            } => {
                write!(f, "(Alias {:?}", symbol)?;

                for arg in type_arguments {
                    write!(f, " {:?}", &arg.typ)?;
                    if let Some(abs) = &arg.opt_abilities {
                        write!(f, ":{:?}", abs)?;
                    }
                }

                for (lambda_set, greek_letter) in
                    lambda_set_variables.iter().zip(GREEK_LETTERS.iter())
                {
                    write!(f, " {}@{:?}", greek_letter, lambda_set.0)?;
                }

                // Sometimes it's useful to see the expansion of the alias
                write!(f, "[ but actually {:?} ]", _actual)?;

                write!(f, ")")?;

                Ok(())
            }
            Type::HostExposedAlias {
                name,
                type_arguments: arguments,
                ..
            } => {
                write!(f, "HostExposedAlias {:?}", name)?;

                for arg in arguments {
                    write!(f, " {:?}", arg)?;
                }

                // Sometimes it's useful to see the expansion of the alias
                // write!(f, "[ but actually {:?} ]", _actual)?;

                Ok(())
            }
            Type::Record(fields, ext) => {
                write!(f, "{{")?;

                if !fields.is_empty() {
                    write!(f, " ")?;
                }

                let mut any_written_yet = false;

                for (label, field_type) in fields {
                    match field_type {
                        RecordField::Optional(_) | RecordField::RigidOptional(_) => {
                            write!(f, "{:?} ? {:?}", label, field_type)?
                        }
                        RecordField::Required(_)
                        | RecordField::Demanded(_)
                        | RecordField::RigidRequired(_) => {
                            write!(f, "{:?} : {:?}", label, field_type)?
                        }
                    }

                    if any_written_yet {
                        write!(f, ", ")?;
                    } else {
                        any_written_yet = true;
                    }
                }

                if !fields.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "}}")?;

                match ext {
                    TypeExtension::Closed => {
                        // This is a closed record. We're done!
                        Ok(())
                    }
                    TypeExtension::Open(other) => {
                        // This is an open record, so print the variable
                        // right after the '}'
                        //
                        // e.g. the "*" at the end of `{ x: Int }*`
                        // or the "r" at the end of `{ x: Int }r`
                        other.fmt(f)
                    }
                }
            }
            Type::TagUnion(tags, ext) => {
                write_tags(f, tags.iter())?;

                match ext {
                    TypeExtension::Closed => {
                        // This is a closed variant. We're done!
                        Ok(())
                    }
                    TypeExtension::Open(other) => {
                        // This is an open tag union, so print the variable
                        // right after the ']'
                        //
                        // e.g. the "*" at the end of `[Foo]*`
                        // or the "r" at the end of `[DivByZero]r`
                        other.fmt(f)
                    }
                }
            }
            Type::FunctionOrTagUnion(tag_name, _, ext) => {
                write!(f, "[")?;
                write!(f, "{:?}", tag_name)?;
                write!(f, "]")?;

                match ext {
                    TypeExtension::Closed => {
                        // This is a closed variant. We're done!
                        Ok(())
                    }
                    TypeExtension::Open(other) => {
                        // This is an open tag union, so print the variable
                        // right after the ']'
                        //
                        // e.g. the "*" at the end of `[Foo]*`
                        // or the "r" at the end of `[DivByZero]r`
                        other.fmt(f)
                    }
                }
            }
            Type::ClosureTag {
                name,
                captures,
                ambient_function: _,
            } => {
                write!(f, "ClosureTag(")?;

                write!(f, "{:?}, ", name)?;
                for capture in captures {
                    write!(f, "{:?}, ", capture)?;
                }

                write!(f, ")")
            }
            Type::RecursiveTagUnion(rec, tags, ext) => {
                write_tags(f, tags.iter())?;

                match ext {
                    TypeExtension::Closed => {
                        // This is a closed variant. We're done!
                        Ok(())
                    }
                    TypeExtension::Open(other) => {
                        // This is an open tag union, so print the variable
                        // right after the ']'
                        //
                        // e.g. the "*" at the end of `[Foo]*`
                        // or the "r" at the end of `[DivByZero]r`
                        other.fmt(f)
                    }
                }?;

                write!(f, " as <{:?}>", rec)
            }
            Type::RangedNumber(range_vars) => {
                write!(f, "Ranged({:?})", range_vars)
            }
            Type::UnspecializedLambdaSet { unspecialized } => {
                write!(f, "{:?}", unspecialized)
            }
        }
    }
}

impl Type {
    pub fn arity(&self) -> usize {
        if let Type::Function(args, _, _) = self {
            args.len()
        } else {
            0
        }
    }
    pub fn is_recursive(&self) -> bool {
        matches!(self, Type::RecursiveTagUnion(_, _, _))
    }

    pub fn is_empty_tag_union(&self) -> bool {
        matches!(self, Type::EmptyTagUnion)
    }

    pub fn is_empty_record(&self) -> bool {
        matches!(self, Type::EmptyRec)
    }

    pub fn variables(&self) -> ImSet<Variable> {
        let mut result = ImSet::default();
        variables_help(self, &mut result);

        result
    }

    pub fn variables_detail(&self) -> VariableDetail {
        let mut result = Default::default();
        variables_help_detailed(self, &mut result);

        result
    }

    pub fn substitute(&mut self, substitutions: &ImMap<Variable, Type>) {
        use Type::*;

        let mut stack = vec![self];

        while let Some(typ) = stack.pop() {
            match typ {
                Variable(v) => {
                    if let Some(replacement) = substitutions.get(v) {
                        *typ = replacement.clone();
                    }
                }
                Function(args, closure, ret) => {
                    stack.extend(args);
                    stack.push(closure);
                    stack.push(ret);
                }
                ClosureTag {
                    name: _,
                    captures,
                    ambient_function: _,
                } => stack.extend(captures),
                TagUnion(tags, ext) => {
                    for (_, args) in tags {
                        stack.extend(args.iter_mut());
                    }

                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                FunctionOrTagUnion(_, _, ext) => {
                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                RecursiveTagUnion(_, tags, ext) => {
                    for (_, args) in tags {
                        stack.extend(args.iter_mut());
                    }

                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                Record(fields, ext) => {
                    for (_, x) in fields.iter_mut() {
                        stack.push(x.as_inner_mut());
                    }

                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                Type::DelayedAlias(AliasCommon {
                    type_arguments,
                    lambda_set_variables,
                    infer_ext_in_output_types,
                    ..
                }) => {
                    for value in type_arguments.iter_mut() {
                        stack.push(&mut value.value.typ);
                    }

                    for lambda_set in lambda_set_variables.iter_mut() {
                        stack.push(lambda_set.as_inner_mut());
                    }

                    for infer_ext in infer_ext_in_output_types.iter_mut() {
                        stack.push(infer_ext);
                    }
                }
                Alias {
                    type_arguments,
                    lambda_set_variables,
                    infer_ext_in_output_types: infer_ext_in_output_variables,
                    actual,
                    ..
                } => {
                    for value in type_arguments.iter_mut() {
                        stack.push(&mut value.typ);
                    }

                    for lambda_set in lambda_set_variables.iter_mut() {
                        stack.push(lambda_set.as_inner_mut());
                    }

                    for infer_ext in infer_ext_in_output_variables.iter_mut() {
                        stack.push(infer_ext);
                    }

                    stack.push(actual);
                }
                HostExposedAlias {
                    type_arguments,
                    lambda_set_variables,
                    actual: actual_type,
                    ..
                } => {
                    for value in type_arguments.iter_mut() {
                        stack.push(value);
                    }

                    for lambda_set in lambda_set_variables.iter_mut() {
                        stack.push(lambda_set.as_inner_mut());
                    }

                    stack.push(actual_type);
                }
                Apply(_, args, _) => {
                    stack.extend(args.iter_mut().map(|t| &mut t.value));
                }
                RangedNumber(_) => {}
                UnspecializedLambdaSet {
                    unspecialized: Uls(v, _, _),
                } => {
                    debug_assert!(
                        substitutions.get(v).is_none(),
                        "unspecialized lambda sets should never be substituted before solving"
                    );
                }

                EmptyRec | EmptyTagUnion | Error => {}
            }
        }
    }

    pub fn substitute_variables(&mut self, substitutions: &MutMap<Variable, Variable>) {
        use Type::*;

        let mut stack = vec![self];

        while let Some(typ) = stack.pop() {
            match typ {
                Variable(v) => {
                    if let Some(replacement) = substitutions.get(v) {
                        *v = *replacement;
                    }
                }
                Function(args, closure, ret) => {
                    stack.extend(args);
                    stack.push(closure);
                    stack.push(ret);
                }
                ClosureTag {
                    name: _,
                    captures,
                    ambient_function: _,
                } => {
                    stack.extend(captures);
                }
                TagUnion(tags, ext) => {
                    for (_, args) in tags {
                        stack.extend(args.iter_mut());
                    }

                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                FunctionOrTagUnion(_, _, ext) => {
                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                RecursiveTagUnion(rec_var, tags, ext) => {
                    if let Some(replacement) = substitutions.get(rec_var) {
                        *rec_var = *replacement;
                    }

                    for (_, args) in tags {
                        stack.extend(args.iter_mut());
                    }

                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                Record(fields, ext) => {
                    for (_, x) in fields.iter_mut() {
                        stack.push(x.as_inner_mut());
                    }
                    if let TypeExtension::Open(ext) = ext {
                        stack.push(ext);
                    }
                }
                Type::DelayedAlias(AliasCommon {
                    type_arguments,
                    lambda_set_variables,
                    infer_ext_in_output_types,
                    ..
                }) => {
                    for value in type_arguments.iter_mut() {
                        stack.push(&mut value.value.typ);
                    }

                    for lambda_set in lambda_set_variables.iter_mut() {
                        stack.push(lambda_set.as_inner_mut());
                    }

                    for typ in infer_ext_in_output_types.iter_mut() {
                        stack.push(typ);
                    }
                }
                Alias {
                    type_arguments,
                    lambda_set_variables,
                    infer_ext_in_output_types,
                    actual,
                    ..
                } => {
                    for value in type_arguments.iter_mut() {
                        stack.push(&mut value.typ);
                    }
                    for lambda_set in lambda_set_variables.iter_mut() {
                        stack.push(lambda_set.as_inner_mut());
                    }
                    for typ in infer_ext_in_output_types.iter_mut() {
                        stack.push(typ);
                    }

                    stack.push(actual);
                }
                HostExposedAlias {
                    type_arguments,
                    lambda_set_variables,
                    actual: actual_type,
                    ..
                } => {
                    for value in type_arguments.iter_mut() {
                        stack.push(value);
                    }

                    for lambda_set in lambda_set_variables.iter_mut() {
                        stack.push(lambda_set.as_inner_mut());
                    }

                    stack.push(actual_type);
                }
                Apply(_, args, _) => {
                    stack.extend(args.iter_mut().map(|t| &mut t.value));
                }
                RangedNumber(_) => {}
                UnspecializedLambdaSet {
                    unspecialized: Uls(v, _, _),
                } => {
                    debug_assert!(
                        substitutions.get(v).is_none(),
                        "unspecialized lambda sets should never be substituted before solving"
                    );
                }

                EmptyRec | EmptyTagUnion | Error => {}
            }
        }
    }

    /// Swap Apply(rep_symbol, rep_args) with `actual`. Returns `Err` if there is an
    /// `Apply(rep_symbol, _)`, but the args don't match.
    pub fn substitute_alias(
        &mut self,
        rep_symbol: Symbol,
        rep_args: &[Type],
        actual: &Type,
    ) -> Result<(), Region> {
        use Type::*;

        match self {
            Function(args, closure, ret) => {
                for arg in args {
                    arg.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                closure.substitute_alias(rep_symbol, rep_args, actual)?;
                ret.substitute_alias(rep_symbol, rep_args, actual)
            }
            FunctionOrTagUnion(_, _, ext) => match ext {
                TypeExtension::Open(ext) => ext.substitute_alias(rep_symbol, rep_args, actual),
                TypeExtension::Closed => Ok(()),
            },
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.substitute_alias(rep_symbol, rep_args, actual)?;
                    }
                }

                match ext {
                    TypeExtension::Open(ext) => ext.substitute_alias(rep_symbol, rep_args, actual),
                    TypeExtension::Closed => Ok(()),
                }
            }
            Record(fields, ext) => {
                for (_, x) in fields.iter_mut() {
                    x.substitute_alias(rep_symbol, rep_args, actual)?;
                }

                match ext {
                    TypeExtension::Open(ext) => ext.substitute_alias(rep_symbol, rep_args, actual),
                    TypeExtension::Closed => Ok(()),
                }
            }
            DelayedAlias(AliasCommon {
                type_arguments,
                lambda_set_variables: _no_aliases_in_lambda_sets,
                infer_ext_in_output_types: _no_aliases_in_infer_ext_types,
                ..
            }) => {
                for ta in type_arguments {
                    ta.value
                        .typ
                        .substitute_alias(rep_symbol, rep_args, actual)?;
                }

                Ok(())
            }
            Alias {
                type_arguments,
                actual: alias_actual,
                ..
            } => {
                for ta in type_arguments {
                    ta.typ.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                alias_actual.substitute_alias(rep_symbol, rep_args, actual)
            }
            HostExposedAlias {
                actual: actual_type,
                ..
            } => actual_type.substitute_alias(rep_symbol, rep_args, actual),
            Apply(symbol, args, region) if *symbol == rep_symbol => {
                if args.len() == rep_args.len()
                    && args
                        .iter()
                        .zip(rep_args.iter())
                        .all(|(t1, t2)| &t1.value == t2)
                {
                    *self = actual.clone();

                    if let Apply(_, args, _) = self {
                        for arg in args {
                            arg.value.substitute_alias(rep_symbol, rep_args, actual)?;
                        }
                    }
                    return Ok(());
                }
                Err(*region)
            }
            Apply(_, args, _) => {
                for arg in args {
                    arg.value.substitute_alias(rep_symbol, rep_args, actual)?;
                }
                Ok(())
            }
            RangedNumber(_) => Ok(()),
            UnspecializedLambdaSet { .. } => Ok(()),
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Error | Variable(_) => Ok(()),
        }
    }

    fn contains_symbol_ext(ext: &TypeExtension, rep_symbol: Symbol) -> bool {
        match ext {
            TypeExtension::Open(ext) => ext.contains_symbol(rep_symbol),
            TypeExtension::Closed => false,
        }
    }

    pub fn contains_symbol(&self, rep_symbol: Symbol) -> bool {
        use Type::*;

        match self {
            Function(args, closure, ret) => {
                ret.contains_symbol(rep_symbol)
                    || closure.contains_symbol(rep_symbol)
                    || args.iter().any(|arg| arg.contains_symbol(rep_symbol))
            }
            FunctionOrTagUnion(_, _, ext) => Self::contains_symbol_ext(ext, rep_symbol),
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                Self::contains_symbol_ext(ext, rep_symbol)
                    || tags
                        .iter()
                        .flat_map(|v| v.1.iter())
                        .any(|arg| arg.contains_symbol(rep_symbol))
            }

            Record(fields, ext) => {
                Self::contains_symbol_ext(ext, rep_symbol)
                    || fields.values().any(|arg| arg.contains_symbol(rep_symbol))
            }
            DelayedAlias(AliasCommon {
                symbol,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types: _,
                ..
            }) => {
                symbol == &rep_symbol
                    || type_arguments
                        .iter()
                        .any(|v| v.value.typ.contains_symbol(rep_symbol))
                    || lambda_set_variables
                        .iter()
                        .any(|v| v.0.contains_symbol(rep_symbol))
            }
            Alias {
                symbol: alias_symbol,
                actual: actual_type,
                ..
            } => alias_symbol == &rep_symbol || actual_type.contains_symbol(rep_symbol),
            HostExposedAlias { name, actual, .. } => {
                name == &rep_symbol || actual.contains_symbol(rep_symbol)
            }
            Apply(symbol, _, _) if *symbol == rep_symbol => true,
            Apply(_, args, _) => args.iter().any(|arg| arg.value.contains_symbol(rep_symbol)),
            RangedNumber(_) => false,
            UnspecializedLambdaSet {
                unspecialized: Uls(_, sym, _),
            } => *sym == rep_symbol,
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Error | Variable(_) => false,
        }
    }

    fn contains_variable_ext(ext: &TypeExtension, rep_variable: Variable) -> bool {
        match ext {
            TypeExtension::Open(ext) => ext.contains_variable(rep_variable),
            TypeExtension::Closed => false,
        }
    }

    pub fn contains_variable(&self, rep_variable: Variable) -> bool {
        use Type::*;

        match self {
            Variable(v) => *v == rep_variable,
            Function(args, closure, ret) => {
                ret.contains_variable(rep_variable)
                    || closure.contains_variable(rep_variable)
                    || args.iter().any(|arg| arg.contains_variable(rep_variable))
            }
            FunctionOrTagUnion(_, _, ext) => Self::contains_variable_ext(ext, rep_variable),
            ClosureTag {
                name: _,
                captures,
                ambient_function: _,
            } => captures.iter().any(|t| t.contains_variable(rep_variable)),
            UnspecializedLambdaSet {
                unspecialized: Uls(v, _, _),
            } => *v == rep_variable,
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                Self::contains_variable_ext(ext, rep_variable)
                    || tags
                        .iter()
                        .flat_map(|v| v.1.iter())
                        .any(|arg| arg.contains_variable(rep_variable))
            }

            Record(fields, ext) => {
                Self::contains_variable_ext(ext, rep_variable)
                    || fields
                        .values()
                        .any(|arg| arg.contains_variable(rep_variable))
            }
            DelayedAlias(AliasCommon { .. }) => {
                todo!()
            }
            Alias {
                actual: actual_type,
                ..
            } => actual_type.contains_variable(rep_variable),
            HostExposedAlias { actual, .. } => actual.contains_variable(rep_variable),
            Apply(_, args, _) => args
                .iter()
                .any(|arg| arg.value.contains_variable(rep_variable)),
            RangedNumber(_) => false,
            EmptyRec | EmptyTagUnion | Error => false,
        }
    }

    pub fn symbols(&self) -> Vec<Symbol> {
        symbols_help(self)
    }

    /// a shallow dealias, continue until the first constructor is not an alias.
    pub fn shallow_dealias(&self) -> &Self {
        let mut result = self;
        while let Type::Alias { actual, .. } = result {
            result = actual;
        }
        result
    }

    pub fn shallow_structural_dealias(&self) -> &Self {
        let mut result = self;
        while let Type::Alias {
            actual,
            kind: AliasKind::Structural,
            ..
        } = result
        {
            result = actual;
        }
        result
    }

    pub fn instantiate_aliases<'a, F>(
        &mut self,
        region: Region,
        aliases: &'a F,
        var_store: &mut VarStore,
        new_lambda_set_variables: &mut ImSet<Variable>,
        new_infer_ext_vars: &mut ImSet<Variable>,
    ) where
        F: Fn(Symbol) -> Option<&'a Alias>,
    {
        use Type::*;

        match self {
            Function(args, closure, ret) => {
                for arg in args {
                    arg.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }
                closure.instantiate_aliases(
                    region,
                    aliases,
                    var_store,
                    new_lambda_set_variables,
                    new_infer_ext_vars,
                );
                ret.instantiate_aliases(
                    region,
                    aliases,
                    var_store,
                    new_lambda_set_variables,
                    new_infer_ext_vars,
                );
            }
            FunctionOrTagUnion(_, _, ext) => {
                if let TypeExtension::Open(ext) = ext {
                    ext.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }
            }
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                for (_, args) in tags {
                    for x in args {
                        x.instantiate_aliases(
                            region,
                            aliases,
                            var_store,
                            new_lambda_set_variables,
                            new_infer_ext_vars,
                        );
                    }
                }

                if let TypeExtension::Open(ext) = ext {
                    ext.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }
            }
            Record(fields, ext) => {
                for (_, x) in fields.iter_mut() {
                    x.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }

                if let TypeExtension::Open(ext) = ext {
                    ext.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }
            }
            DelayedAlias(AliasCommon {
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types,
                symbol: _,
            }) => {
                debug_assert!(lambda_set_variables
                    .iter()
                    .all(|lambda_set| matches!(lambda_set.0, Type::Variable(..))));
                debug_assert!(infer_ext_in_output_types
                    .iter()
                    .all(|t| matches!(t, Type::Variable(..) | Type::EmptyTagUnion)));
                type_arguments.iter_mut().for_each(|t| {
                    t.value.typ.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    )
                });
            }
            HostExposedAlias {
                type_arguments: type_args,
                lambda_set_variables,
                actual: actual_type,
                ..
            } => {
                for arg in type_args {
                    arg.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }

                for arg in lambda_set_variables {
                    arg.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }

                actual_type.instantiate_aliases(
                    region,
                    aliases,
                    var_store,
                    new_lambda_set_variables,
                    new_infer_ext_vars,
                );
            }
            Alias {
                type_arguments: type_args,
                lambda_set_variables,
                actual: actual_type,
                ..
            } => {
                for arg in type_args {
                    arg.typ.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }

                for arg in lambda_set_variables {
                    arg.instantiate_aliases(
                        region,
                        aliases,
                        var_store,
                        new_lambda_set_variables,
                        new_infer_ext_vars,
                    );
                }

                actual_type.instantiate_aliases(
                    region,
                    aliases,
                    var_store,
                    new_lambda_set_variables,
                    new_infer_ext_vars,
                );
            }
            Apply(symbol, args, _) => {
                if let Some(alias) = aliases(*symbol) {
                    // TODO switch to this, but we still need to check for recursion with the
                    // `else` branch.
                    // We would also need to determine polarity correct.
                    if false {
                        let mut type_var_to_arg = Vec::new();

                        for (alias_var, arg_ann) in alias.type_variables.iter().zip(args) {
                            type_var_to_arg.push(Loc::at(
                                arg_ann.region,
                                OptAbleType {
                                    typ: arg_ann.value.clone(),
                                    opt_abilities: alias_var.value.opt_bound_abilities.clone(),
                                },
                            ));
                        }

                        let mut lambda_set_variables =
                            Vec::with_capacity(alias.lambda_set_variables.len());

                        for _ in 0..alias.lambda_set_variables.len() {
                            let lvar = var_store.fresh();

                            new_lambda_set_variables.insert(lvar);

                            lambda_set_variables.push(LambdaSet(Type::Variable(lvar)));
                        }

                        let mut infer_ext_in_output_types =
                            Vec::with_capacity(alias.infer_ext_in_output_variables.len());

                        for _ in 0..alias.infer_ext_in_output_variables.len() {
                            let var = var_store.fresh();
                            new_infer_ext_vars.insert(var);
                            infer_ext_in_output_types.push(Type::Variable(var));
                        }

                        let alias = Type::DelayedAlias(AliasCommon {
                            symbol: *symbol,
                            type_arguments: type_var_to_arg,
                            lambda_set_variables,
                            infer_ext_in_output_types,
                        });

                        *self = alias;
                    } else {
                        if args.len() != alias.type_variables.len() {
                            // We will have already reported an error during canonicalization.
                            *self = Type::Error;
                            return;
                        }

                        let mut actual = alias.typ.clone();

                        let mut named_args = Vec::with_capacity(args.len());
                        let mut substitution = ImMap::default();

                        // TODO substitute further in args
                        for (
                            Loc {
                                value:
                                    AliasVar {
                                        var: placeholder,
                                        opt_bound_abilities,
                                        ..
                                    },
                                ..
                            },
                            filler,
                        ) in alias.type_variables.iter().zip(args.iter())
                        {
                            let mut filler = filler.clone();
                            filler.value.instantiate_aliases(
                                region,
                                aliases,
                                var_store,
                                new_lambda_set_variables,
                                new_infer_ext_vars,
                            );
                            named_args.push(OptAbleType {
                                typ: filler.value.clone(),
                                opt_abilities: opt_bound_abilities.clone(),
                            });
                            substitution.insert(*placeholder, filler.value);
                        }

                        // make sure hidden variables are freshly instantiated
                        let mut lambda_set_variables =
                            Vec::with_capacity(alias.lambda_set_variables.len());
                        for typ in alias.lambda_set_variables.iter() {
                            if let Type::Variable(var) = typ.0 {
                                let fresh = var_store.fresh();
                                new_lambda_set_variables.insert(fresh);
                                substitution.insert(var, Type::Variable(fresh));
                                lambda_set_variables.push(LambdaSet(Type::Variable(fresh)));
                            } else {
                                unreachable!("at this point there should be only vars in there");
                            }
                        }
                        let mut infer_ext_in_output_types =
                            Vec::with_capacity(alias.infer_ext_in_output_variables.len());
                        for var in alias.infer_ext_in_output_variables.iter() {
                            let fresh = var_store.fresh();
                            new_infer_ext_vars.insert(fresh);
                            substitution.insert(*var, Type::Variable(fresh));
                            infer_ext_in_output_types.push(Type::Variable(fresh));
                        }

                        actual.instantiate_aliases(
                            region,
                            aliases,
                            var_store,
                            new_lambda_set_variables,
                            new_infer_ext_vars,
                        );

                        actual.substitute(&substitution);

                        // instantiate recursion variable!
                        if let Type::RecursiveTagUnion(rec_var, mut tags, mut ext) = actual {
                            let new_rec_var = var_store.fresh();
                            substitution.clear();
                            substitution.insert(rec_var, Type::Variable(new_rec_var));

                            for typ in tags.iter_mut().flat_map(|v| v.1.iter_mut()) {
                                typ.substitute(&substitution);
                            }

                            if let TypeExtension::Open(ext) = &mut ext {
                                ext.substitute(&substitution);
                            }

                            actual = Type::RecursiveTagUnion(new_rec_var, tags, ext);
                        }
                        let alias = Type::Alias {
                            symbol: *symbol,
                            type_arguments: named_args,
                            lambda_set_variables,
                            infer_ext_in_output_types,
                            actual: Box::new(actual),
                            kind: alias.kind,
                        };

                        *self = alias;
                    }
                } else {
                    // one of the special-cased Apply types.
                    for x in args {
                        x.value.instantiate_aliases(
                            region,
                            aliases,
                            var_store,
                            new_lambda_set_variables,
                            new_infer_ext_vars,
                        );
                    }
                }
            }
            RangedNumber(_) => {}
            UnspecializedLambdaSet { .. } => {}
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Error | Variable(_) => {}
        }
    }

    pub fn instantiate_lambda_sets_as_unspecialized(
        &mut self,
        able_var: Variable,
        ability_member: Symbol,
    ) {
        instantiate_lambda_sets_as_unspecialized(self, able_var, ability_member)
    }

    pub fn is_tag_union_like(&self) -> bool {
        matches!(
            self,
            Type::TagUnion(..)
                | Type::RecursiveTagUnion(..)
                | Type::FunctionOrTagUnion(..)
                | Type::EmptyTagUnion
        )
    }

    /// We say a type is "narrow" if no type composing it is a proper sum; that is, no type
    /// composing it is a tag union with more than one variant.
    ///
    /// The types checked here must have all of their non-builtin `Apply`s instantiated, as a
    /// non-instantiated `Apply` would be ambiguous.
    ///
    /// The following are narrow:
    ///
    /// ```roc
    /// U8
    /// [A I8]
    /// [A [B [C U8]]]
    /// [A (R a)] as R a
    /// ```
    ///
    /// The following are not:
    ///
    /// ```roc
    /// [A I8, B U8 ]
    /// [A [B [Result U8 {}]]]         (Result U8 {} is actually [Ok U8, Err {}])
    /// [A { lst: List (R a) }] as R a     (List a is morally [Cons (List a), Nil] as List a)
    /// ```
    pub fn is_narrow(&self) -> bool {
        match self.shallow_dealias() {
            Type::TagUnion(tags, _ext) | Type::RecursiveTagUnion(_, tags, _ext) => {
                tags.len() == 1 && tags[0].1.len() == 1 && tags[0].1[0].is_narrow()
            }
            Type::Record(fields, ext) => match ext {
                TypeExtension::Open(ext) => {
                    fields.values().all(|field| field.as_inner().is_narrow()) && ext.is_narrow()
                }
                TypeExtension::Closed => fields.values().all(|field| field.as_inner().is_narrow()),
            },
            Type::Function(args, clos, ret) => {
                args.iter().all(|a| a.is_narrow()) && clos.is_narrow() && ret.is_narrow()
            }
            // Lists and sets are morally two-tagged unions, as they can be empty
            Type::Apply(Symbol::LIST_LIST | Symbol::SET_SET, _, _) => false,
            Type::Apply(..) => internal_error!("cannot chase an Apply!"),
            Type::Alias { .. } => internal_error!("should be dealiased"),
            // Must be conservative here because we don't know what the alias expands to yet
            Type::DelayedAlias(..) => false,
            // Non-composite types are trivially narrow
            _ => true,
        }
    }

    pub fn expect_variable(&self, reason: &'static str) -> Variable {
        match self {
            Type::Variable(v) => *v,
            _ => internal_error!("{}", reason),
        }
    }
}

fn symbols_help(initial: &Type) -> Vec<Symbol> {
    use Type::*;

    let mut output = vec![];
    let mut stack = vec![initial];

    while let Some(tipe) = stack.pop() {
        match tipe {
            Function(args, closure, ret) => {
                stack.push(ret);
                stack.push(closure);
                stack.extend(args);
            }
            FunctionOrTagUnion(_, _, ext) => {
                stack.extend(ext);
            }
            RecursiveTagUnion(_, tags, ext) | TagUnion(tags, ext) => {
                stack.extend(ext);
                stack.extend(tags.iter().flat_map(|v| v.1.iter()));
            }

            Record(fields, ext) => {
                stack.extend(ext);
                stack.extend(fields.values().map(|field| field.as_inner()));
            }
            DelayedAlias(AliasCommon {
                symbol,
                type_arguments,
                ..
            }) => {
                output.push(*symbol);
                stack.extend(type_arguments.iter().map(|ta| &ta.value.typ));
            }
            Alias {
                symbol: alias_symbol,
                actual: actual_type,
                ..
            } => {
                // because the type parameters are inlined in the actual type, we don't need to look
                // at the type parameters here
                output.push(*alias_symbol);
                stack.push(actual_type);
            }
            HostExposedAlias { name, actual, .. } => {
                // because the type parameters are inlined in the actual type, we don't need to look
                // at the type parameters here
                output.push(*name);
                stack.push(actual);
            }
            Apply(symbol, args, _) => {
                output.push(*symbol);
                stack.extend(args.iter().map(|t| &t.value));
            }
            RangedNumber(_) => {}
            UnspecializedLambdaSet {
                unspecialized: Uls(_, _sym, _),
            } => {
                // ignore the member symbol because unspecialized lambda sets are internal-only
            }
            EmptyRec | EmptyTagUnion | ClosureTag { .. } | Error | Variable(_) => {}
        }
    }

    output.sort();
    output.dedup();

    output
}

fn variables_help(tipe: &Type, accum: &mut ImSet<Variable>) {
    use Type::*;

    match tipe {
        EmptyRec | EmptyTagUnion | Error => (),

        Variable(v) => {
            accum.insert(*v);
        }

        Function(args, closure, ret) => {
            for arg in args {
                variables_help(arg, accum);
            }
            variables_help(closure, accum);
            variables_help(ret, accum);
        }
        Record(fields, ext) => {
            for (_, field) in fields {
                variables_help(field.as_inner(), accum);
            }

            if let TypeExtension::Open(ext) = ext {
                variables_help(ext, accum);
            }
        }
        ClosureTag {
            name: _,
            captures,
            ambient_function: _,
        } => {
            for t in captures {
                variables_help(t, accum);
            }
        }
        UnspecializedLambdaSet {
            unspecialized: Uls(v, _, _),
        } => {
            accum.insert(*v);
        }
        TagUnion(tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help(x, accum);
                }
            }

            if let TypeExtension::Open(ext) = ext {
                variables_help(ext, accum);
            }
        }
        FunctionOrTagUnion(_, _, ext) => {
            if let TypeExtension::Open(ext) = ext {
                variables_help(ext, accum);
            }
        }
        RecursiveTagUnion(rec, tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help(x, accum);
                }
            }

            if let TypeExtension::Open(ext) = ext {
                variables_help(ext, accum);
            }

            // just check that this is actually a recursive type
            debug_assert!(accum.contains(rec));

            // this rec var doesn't need to be in flex_vars or rigid_vars
            accum.remove(rec);
        }
        DelayedAlias(AliasCommon {
            type_arguments,
            lambda_set_variables,
            ..
        }) => {
            for arg in type_arguments {
                variables_help(&arg.value.typ, accum);
            }

            for lambda_set in lambda_set_variables {
                variables_help(&lambda_set.0, accum);
            }
        }
        Alias {
            type_arguments,
            actual,
            ..
        } => {
            for arg in type_arguments {
                variables_help(&arg.typ, accum);
            }
            variables_help(actual, accum);
        }
        HostExposedAlias {
            type_arguments: arguments,
            actual,
            ..
        } => {
            for arg in arguments {
                variables_help(arg, accum);
            }
            variables_help(actual, accum);
        }
        RangedNumber(_) => {}
        Apply(_, args, _) => {
            for x in args {
                variables_help(&x.value, accum);
            }
        }
    }
}

#[derive(Default)]
pub struct VariableDetail {
    pub type_variables: MutSet<Variable>,
    pub lambda_set_variables: Vec<Variable>,
    pub recursion_variables: MutSet<Variable>,
}

impl VariableDetail {
    pub fn is_empty(&self) -> bool {
        self.type_variables.is_empty()
            && self.lambda_set_variables.is_empty()
            && self.recursion_variables.is_empty()
    }
}

fn variables_help_detailed(tipe: &Type, accum: &mut VariableDetail) {
    use Type::*;

    match tipe {
        EmptyRec | EmptyTagUnion | Error => (),

        Variable(v) => {
            accum.type_variables.insert(*v);
        }

        Function(args, closure, ret) => {
            for arg in args {
                variables_help_detailed(arg, accum);
            }
            if let Type::Variable(v) = **closure {
                accum.lambda_set_variables.push(v);
            } else {
                variables_help_detailed(closure, accum);
            }

            variables_help_detailed(ret, accum);
        }
        Record(fields, ext) => {
            for (_, field) in fields {
                variables_help_detailed(field.as_inner(), accum);
            }

            if let TypeExtension::Open(ext) = ext {
                variables_help_detailed(ext, accum);
            }
        }
        ClosureTag {
            name: _,
            captures,
            ambient_function: _,
        } => {
            for t in captures {
                variables_help_detailed(t, accum);
            }
        }
        TagUnion(tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help_detailed(x, accum);
                }
            }

            if let TypeExtension::Open(ext) = ext {
                variables_help_detailed(ext, accum);
            }
        }
        FunctionOrTagUnion(_, _, ext) => {
            if let TypeExtension::Open(ext) = ext {
                variables_help_detailed(ext, accum);
            }
        }
        UnspecializedLambdaSet {
            unspecialized: Uls(var, _, _),
        } => {
            accum.type_variables.insert(*var);
        }
        RecursiveTagUnion(rec, tags, ext) => {
            for (_, args) in tags {
                for x in args {
                    variables_help_detailed(x, accum);
                }
            }

            if let TypeExtension::Open(ext) = ext {
                variables_help_detailed(ext, accum);
            }

            // just check that this is actually a recursive type
            // debug_assert!(accum.type_variables.contains(rec));

            // this rec var doesn't need to be in flex_vars or rigid_vars
            accum.type_variables.remove(rec);

            accum.recursion_variables.insert(*rec);
        }
        DelayedAlias(AliasCommon {
            type_arguments,
            lambda_set_variables,
            infer_ext_in_output_types: _,
            ..
        }) => {
            for arg in type_arguments {
                variables_help_detailed(&arg.value.typ, accum);
            }

            for lambda_set in lambda_set_variables {
                if let Type::Variable(v) = lambda_set.0 {
                    accum.lambda_set_variables.push(v);
                } else {
                    variables_help_detailed(&lambda_set.0, accum);
                }
            }
        }
        Alias {
            type_arguments,
            actual,
            infer_ext_in_output_types: _,
            ..
        } => {
            for arg in type_arguments {
                variables_help_detailed(&arg.typ, accum);
            }
            variables_help_detailed(actual, accum);
        }
        HostExposedAlias {
            type_arguments: arguments,
            actual,
            ..
        } => {
            for arg in arguments {
                variables_help_detailed(arg, accum);
            }
            variables_help_detailed(actual, accum);
        }
        RangedNumber(_) => {}
        Apply(_, args, _) => {
            for x in args {
                variables_help_detailed(&x.value, accum);
            }
        }
    }
}

#[derive(Debug)]
pub struct RecordStructure {
    /// Invariant: these should be sorted!
    pub fields: Vec<(Lowercase, RecordField<Variable>)>,
    pub ext: Variable,
}

#[derive(Debug)]
pub struct TagUnionStructure<'a> {
    /// Invariant: these should be sorted!
    pub fields: Vec<(TagName, &'a [Variable])>,
    pub ext: Variable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PReason {
    TypedArg {
        opt_name: Option<Symbol>,
        index: HumanIndex,
    },
    WhenMatch {
        index: HumanIndex,
        sub_pattern: HumanIndex,
    },
    TagArg {
        tag_name: TagName,
        index: HumanIndex,
    },
    ListElem,
    PatternGuard,
    OptionalField,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotationSource {
    TypedIfBranch {
        index: HumanIndex,
        num_branches: usize,
        region: Region,
    },
    TypedWhenBranch {
        index: HumanIndex,
        region: Region,
    },
    TypedBody {
        region: Region,
    },
    RequiredSymbol {
        region: Region,
    },
}

impl AnnotationSource {
    pub fn region(&self) -> Region {
        match self {
            &Self::TypedIfBranch { region, .. }
            | &Self::TypedWhenBranch { region, .. }
            | &Self::TypedBody { region, .. } => region,
            &Self::RequiredSymbol { region, .. } => region,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    FnArg {
        name: Option<Symbol>,
        arg_index: HumanIndex,
    },
    TypedArg {
        name: Option<Symbol>,
        arg_index: HumanIndex,
    },
    FnCall {
        name: Option<Symbol>,
        arity: u8,
    },
    LowLevelOpArg {
        op: LowLevel,
        arg_index: HumanIndex,
    },
    ForeignCallArg {
        foreign_symbol: ForeignSymbol,
        arg_index: HumanIndex,
    },
    FloatLiteral,
    IntLiteral,
    NumLiteral,
    StrInterpolation,
    WhenBranches,
    WhenBranch {
        index: HumanIndex,
    },
    WhenGuard,
    ExpectCondition,
    IfCondition,
    IfBranch {
        index: HumanIndex,
        total_branches: usize,
    },
    ElemInList {
        index: HumanIndex,
    },
    RecordUpdateValue(Lowercase),
    RecordUpdateKeys(Symbol, SendMap<Lowercase, Region>),
    RecordDefaultField(Lowercase),
    NumericLiteralSuffix,
    InvalidAbilityMemberSpecialization {
        member_name: Symbol,
        def_region: Region,
        unimplemented_abilities: DoesNotImplementAbility,
    },
    GeneralizedAbilityMemberSpecialization {
        member_name: Symbol,
        def_region: Region,
    },
    CrashArg,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Category {
    Lookup(Symbol),
    CallResult(Option<Symbol>, CalledVia),
    LowLevelOpResult(LowLevel),
    ForeignCall,
    TagApply {
        tag_name: TagName,
        args_count: usize,
    },
    OpaqueWrap(Symbol),
    OpaqueArg,
    Lambda,
    Uniqueness,
    ClosureSize,
    StrInterpolation,

    // storing variables in the ast
    Storage(&'static str, u32),

    // control flow
    If,
    When,

    // types
    Frac,
    Int,
    Num,
    List,
    Str,
    Character,

    // records
    Record,
    Accessor(Lowercase),
    Access(Lowercase),
    DefaultValue(Lowercase), // for setting optional fields

    AbilityMemberSpecialization(Symbol),

    Crash,

    Expect,
    Dbg,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternCategory {
    Record,
    List,
    EmptyRecord,
    PatternGuard,
    PatternDefault,
    Set,
    Map,
    Ctor(TagName),
    Opaque(Symbol),
    Str,
    Num,
    Int,
    Float,
    Character,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum AliasKind {
    /// A structural alias is something like
    ///   List a : [Nil, Cons a (List a)]
    /// It is typed structurally, so that a `List U8` is always equal to a `[Nil]_`, for example.
    Structural,
    /// An opaque alias corresponds to an opaque type from the language syntax, like
    ///   Age := U32
    /// It is type nominally, so that `Age` is never equal to `U8` - the only way to unwrap the
    /// structural type inside `Age` is to unwrap the opaque, so `Age` = `@Age U8`.
    Opaque,
}

impl AliasKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            AliasKind::Structural => "alias",
            AliasKind::Opaque => "opaque",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AliasVar {
    pub name: Lowercase,
    pub var: Variable,
    /// `Some` if this variable is bound to abilities; `None` otherwise.
    pub opt_bound_abilities: Option<AbilitySet>,
}

impl AliasVar {
    pub fn unbound(name: Lowercase, var: Variable) -> AliasVar {
        Self {
            name,
            var,
            opt_bound_abilities: None,
        }
    }
}

impl From<&AliasVar> for OptAbleVar {
    fn from(av: &AliasVar) -> OptAbleVar {
        OptAbleVar {
            var: av.var,
            opt_abilities: av.opt_bound_abilities.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemberImpl {
    /// The implementation is claimed to be at the given symbol.
    /// During solving we validate that the impl is really there.
    Impl(Symbol),
    /// The implementation is not present or does not match the expected member type.
    Error,
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub region: Region,
    pub type_variables: Vec<Loc<AliasVar>>,

    /// lambda set variables, e.g. the one annotating the arrow in
    /// a |c|-> b
    pub lambda_set_variables: Vec<LambdaSet>,

    /// Extension variables that should be inferred in output positions, and closed in input
    /// positions.
    pub infer_ext_in_output_variables: Vec<Variable>,

    pub recursion_variables: MutSet<Variable>,

    pub typ: Type,

    pub kind: AliasKind,
}

impl Alias {
    pub fn header_region(&self) -> Region {
        Region::across_all(
            [self.region]
                .iter()
                .chain(self.type_variables.iter().map(|tv| &tv.region)),
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Mismatch {
    TypeMismatch,
    IfConditionNotBool,
    InconsistentIfElse,
    InconsistentWhenBranches,
    CanonicalizationProblem,
    TypeNotInRange,
    DoesNotImplementAbiity(Variable, Symbol),
}

pub type DoesNotImplementAbility = Vec<(ErrorType, Symbol)>;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum ErrorType {
    Infinite,
    Type(Symbol, Vec<ErrorType>),
    /// If the name was auto-generated, it will start with a `#`.
    FlexVar(Lowercase),
    RigidVar(Lowercase),
    /// If the name was auto-generated, it will start with a `#`.
    FlexAbleVar(Lowercase, AbilitySet),
    RigidAbleVar(Lowercase, AbilitySet),
    Record(SendMap<Lowercase, RecordField<ErrorType>>, TypeExt),
    TagUnion(SendMap<TagName, Vec<ErrorType>>, TypeExt, Polarity),
    RecursiveTagUnion(
        Box<ErrorType>,
        SendMap<TagName, Vec<ErrorType>>,
        TypeExt,
        Polarity,
    ),
    Function(Vec<ErrorType>, Box<ErrorType>, Box<ErrorType>),
    Alias(Symbol, Vec<ErrorType>, Box<ErrorType>, AliasKind),
    Range(Vec<ErrorType>),
    Error,
}

impl std::fmt::Debug for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO remove clone
        write!(f, "{:?}", write_debug_error_type(self.clone()))
    }
}

impl ErrorType {
    pub fn unwrap_structural_alias(self) -> ErrorType {
        match self {
            ErrorType::Alias(_, _, real, AliasKind::Structural) => real.unwrap_structural_alias(),
            real => real,
        }
    }

    /// Adds all named type variables used in the type to a set.
    pub fn add_names(&self, taken: &mut MutSet<Lowercase>) {
        use ErrorType::*;
        match self {
            Infinite => {}
            Type(_, ts) => ts.iter().for_each(|t| t.add_names(taken)),
            FlexVar(v) | RigidVar(v) | FlexAbleVar(v, _) | RigidAbleVar(v, _) => {
                taken.insert(v.clone());
            }
            Record(fields, ext) => {
                fields
                    .iter()
                    .for_each(|(_, t)| t.as_inner().add_names(taken));
                ext.add_names(taken);
            }
            TagUnion(tags, ext, _) => {
                tags.iter()
                    .for_each(|(_, ts)| ts.iter().for_each(|t| t.add_names(taken)));
                ext.add_names(taken);
            }
            RecursiveTagUnion(t, tags, ext, _) => {
                t.add_names(taken);
                tags.iter()
                    .for_each(|(_, ts)| ts.iter().for_each(|t| t.add_names(taken)));
                ext.add_names(taken);
            }
            Function(args, capt, ret) => {
                args.iter().for_each(|t| t.add_names(taken));
                capt.add_names(taken);
                ret.add_names(taken);
            }
            Alias(_, ts, t, _) => {
                ts.iter().for_each(|t| {
                    t.add_names(taken);
                });
                t.add_names(taken);
            }
            Range(ts) => {
                ts.iter().for_each(|t| {
                    t.add_names(taken);
                });
            }
            Error => {}
        }
    }
}

pub fn write_error_type(interns: &Interns, error_type: ErrorType) -> String {
    let mut buf = String::new();
    write_error_type_help(interns, error_type, &mut buf, Parens::Unnecessary);

    buf
}

fn write_error_type_help(
    interns: &Interns,
    error_type: ErrorType,
    buf: &mut String,
    parens: Parens,
) {
    use ErrorType::*;

    match error_type {
        Infinite => buf.push('∞'),
        Error => buf.push('?'),
        FlexVar(name) => buf.push_str(name.as_str()),
        RigidVar(name) => buf.push_str(name.as_str()),
        Type(symbol, arguments) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            buf.push_str(symbol.as_str(interns));

            for arg in arguments {
                buf.push(' ');

                write_error_type_help(interns, arg, buf, Parens::InTypeParam);
            }

            if write_parens {
                buf.push(')');
            }
        }
        Alias(Symbol::NUM_NUM, mut arguments, _actual, _) => {
            debug_assert!(arguments.len() == 1);

            let argument = arguments.remove(0);

            match argument {
                Type(Symbol::NUM_INTEGER, _) => {
                    buf.push_str("Int");
                }
                Type(Symbol::NUM_FLOATINGPOINT, _) => {
                    buf.push_str("F64");
                }
                other => {
                    let write_parens = parens == Parens::InTypeParam;

                    if write_parens {
                        buf.push('(');
                    }
                    buf.push_str("Num ");
                    write_error_type_help(interns, other, buf, Parens::InTypeParam);

                    if write_parens {
                        buf.push(')');
                    }
                }
            }
        }
        Function(arguments, _closure, result) => {
            let write_parens = parens != Parens::Unnecessary;

            if write_parens {
                buf.push(')');
            }

            let mut it = arguments.into_iter().peekable();

            while let Some(arg) = it.next() {
                write_error_type_help(interns, arg, buf, Parens::InFn);
                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push_str(" -> ");

            write_error_type_help(interns, *result, buf, Parens::InFn);

            if write_parens {
                buf.push(')');
            }
        }
        Record(fields, ext) => {
            buf.push('{');

            for (label, field) in fields {
                use RecordField::*;

                buf.push_str(label.as_str());

                let content = match field {
                    Optional(content) | RigidOptional(content) => {
                        buf.push_str(" ? ");
                        content
                    }
                    Required(content) | Demanded(content) | RigidRequired(content) => {
                        buf.push_str(" : ");
                        content
                    }
                };

                write_error_type_help(interns, content, buf, Parens::Unnecessary);
            }

            buf.push('}');
            write_type_ext(ext, buf);
        }

        other => todo!("cannot format {:?} yet", other),
    }
}

pub fn write_debug_error_type(error_type: ErrorType) -> String {
    let mut buf = String::new();
    write_debug_error_type_help(error_type, &mut buf, Parens::Unnecessary);

    buf
}

fn write_debug_error_type_help(error_type: ErrorType, buf: &mut String, parens: Parens) {
    use ErrorType::*;

    match error_type {
        Infinite => buf.push('∞'),
        Error => buf.push('?'),
        FlexVar(name) | RigidVar(name) => buf.push_str(name.as_str()),
        FlexAbleVar(name, symbol) | RigidAbleVar(name, symbol) => {
            let write_parens = parens == Parens::InTypeParam;
            if write_parens {
                buf.push('(');
            }
            buf.push_str(name.as_str());
            write!(buf, "has {:?}", symbol).unwrap();
            if write_parens {
                buf.push(')');
            }
        }
        Type(symbol, arguments) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            write!(buf, "{:?}", symbol).unwrap();

            for arg in arguments {
                buf.push(' ');

                write_debug_error_type_help(arg, buf, Parens::InTypeParam);
            }

            if write_parens {
                buf.push(')');
            }
        }
        Alias(Symbol::NUM_NUM, mut arguments, _actual, _) => {
            debug_assert!(arguments.len() == 1);

            let argument = arguments.remove(0);

            match argument {
                Type(Symbol::NUM_INTEGER, _) => {
                    buf.push_str("Int");
                }
                Type(Symbol::NUM_FLOATINGPOINT, _) => {
                    buf.push_str("F64");
                }
                other => {
                    let write_parens = parens == Parens::InTypeParam;

                    if write_parens {
                        buf.push('(');
                    }
                    buf.push_str("Num ");
                    write_debug_error_type_help(other, buf, Parens::InTypeParam);

                    if write_parens {
                        buf.push(')');
                    }
                }
            }
        }
        Alias(symbol, arguments, _actual, _) => {
            let write_parens = parens == Parens::InTypeParam && !arguments.is_empty();

            if write_parens {
                buf.push('(');
            }
            write!(buf, "{:?}", symbol).unwrap();

            for arg in arguments {
                buf.push(' ');

                write_debug_error_type_help(arg, buf, Parens::InTypeParam);
            }

            // useful for debugging
            let write_out_alias = true;
            if write_out_alias {
                buf.push_str("[[ but really ");
                write_debug_error_type_help(*_actual, buf, Parens::Unnecessary);
                buf.push_str("]]");
            }

            if write_parens {
                buf.push(')');
            }
        }
        Function(arguments, _closure, result) => {
            let write_parens = parens != Parens::Unnecessary;

            if write_parens {
                buf.push('(');
            }

            let mut it = arguments.into_iter().peekable();

            while let Some(arg) = it.next() {
                write_debug_error_type_help(arg, buf, Parens::InFn);
                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push_str(" -> ");

            write_debug_error_type_help(*result, buf, Parens::InFn);

            if write_parens {
                buf.push(')');
            }
        }
        Record(fields, ext) => {
            buf.push('{');

            for (label, field) in fields {
                use RecordField::*;

                buf.push_str(label.as_str());

                let content = match field {
                    Optional(content) | RigidOptional(content) => {
                        buf.push_str(" ? ");
                        content
                    }
                    Required(content) | Demanded(content) | RigidRequired(content) => {
                        buf.push_str(" : ");
                        content
                    }
                };

                write_debug_error_type_help(content, buf, Parens::Unnecessary);
            }

            buf.push('}');
            write_type_ext(ext, buf);
        }
        TagUnion(tags, ext, _pol) => {
            buf.push('[');

            let mut it = tags.into_iter().peekable();

            while let Some((tag, args)) = it.next() {
                write!(buf, "{:?}", tag).unwrap();
                for arg in args {
                    buf.push(' ');
                    write_debug_error_type_help(arg, buf, Parens::InTypeParam);
                }

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push(']');
            write_type_ext(ext, buf);
        }
        RecursiveTagUnion(rec, tags, ext, _pol) => {
            buf.push('[');

            let mut it = tags.into_iter().peekable();
            while let Some((tag, args)) = it.next() {
                write!(buf, "{:?}", tag).unwrap();
                for arg in args {
                    buf.push(' ');
                    write_debug_error_type_help(arg, buf, Parens::Unnecessary);
                }

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push(']');
            write_type_ext(ext, buf);

            buf.push_str(" as ");

            write_debug_error_type_help(*rec, buf, Parens::Unnecessary);
        }
        Range(types) => {
            buf.push('<');

            let mut it = types.into_iter().peekable();
            while let Some(typ) = it.next() {
                write_debug_error_type_help(typ, buf, Parens::Unnecessary);

                if it.peek().is_some() {
                    buf.push_str(", ");
                }
            }

            buf.push('>');
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum TypeExt {
    Closed,
    FlexOpen(Lowercase),
    RigidOpen(Lowercase),
}

impl TypeExt {
    pub fn add_names(&self, taken: &mut MutSet<Lowercase>) {
        use TypeExt::*;
        match self {
            Closed => {}
            FlexOpen(n) | RigidOpen(n) => {
                taken.insert(n.clone());
            }
        }
    }
}

fn write_type_ext(ext: TypeExt, buf: &mut String) {
    use TypeExt::*;
    match ext {
        Closed => {}
        FlexOpen(lowercase) | RigidOpen(lowercase) => {
            buf.push_str(lowercase.as_str());
        }
    }
}

static THE_LETTER_A: u32 = 'a' as u32;

/// Generates a fresh type variable name, composed of lowercase alphabetic characters in sequence.
pub fn name_type_var<I, F: FnMut(&I, &str) -> bool>(
    letters_used: u32,
    taken: &mut impl Iterator<Item = I>,
    mut predicate: F,
) -> (Lowercase, u32) {
    // TODO we should arena-allocate this String,
    // so all the strings in the entire pass only require ~1 allocation.
    let mut buf = String::with_capacity((letters_used as usize) / 26 + 1);

    let is_taken = {
        let mut remaining = letters_used as i32;

        while remaining >= 0 {
            buf.push(std::char::from_u32(THE_LETTER_A + ((remaining as u32) % 26)).unwrap());
            remaining -= 26;
        }

        let generated_name: &str = buf.as_str();

        taken.any(|item| predicate(&item, generated_name))
    };

    if is_taken {
        // If the generated name is already taken, try again.
        name_type_var(letters_used + 1, taken, predicate)
    } else {
        (buf.into(), letters_used + 1)
    }
}

/// Generates a fresh type variable name given a hint, composed of the hint as a prefix and a
/// number as a suffix. For example, given hint `a` we'll name the variable `a`, `a1`, or `a27`.
pub fn name_type_var_with_hint<I, F: FnMut(&I, &str) -> bool>(
    hint: &str,
    taken: &mut impl Iterator<Item = I>,
    mut predicate: F,
) -> Lowercase {
    if !taken.any(|item| predicate(&item, hint)) {
        return hint.into();
    }

    let mut i = 0;
    loop {
        i += 1;
        let cand = format!("{}{}", hint, i);

        if !taken.any(|item| predicate(&item, &cand)) {
            return cand.into();
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RecordFieldsError;

pub fn gather_fields_unsorted_iter(
    subs: &Subs,
    other_fields: RecordFields,
    mut var: Variable,
) -> Result<
    (
        impl Iterator<Item = (&Lowercase, RecordField<Variable>)> + '_,
        Variable,
    ),
    RecordFieldsError,
> {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    let mut stack = vec![other_fields];

    loop {
        match subs.get_content_without_compacting(var) {
            Structure(Record(sub_fields, sub_ext)) => {
                stack.push(*sub_fields);

                if var == Variable::EMPTY_RECORD {
                    break;
                } else {
                    var = *sub_ext;
                }
            }

            Alias(_, _, actual_var, _) => {
                // TODO according to elm/compiler: "TODO may be dropping useful alias info here"
                var = *actual_var;
            }

            Structure(EmptyRecord) => break,
            FlexVar(_) | FlexAbleVar(..) => break,

            // TODO investigate apparently this one pops up in the reporting tests!
            RigidVar(_) | RigidAbleVar(..) => break,

            // Stop on errors in the record
            Error => break,

            _ => return Err(RecordFieldsError),
        }
    }

    let it = stack
        .into_iter()
        .flat_map(|fields| fields.iter_all())
        .map(move |(i1, i2, i3)| {
            let field_name: &Lowercase = &subs[i1];
            let variable = subs[i2];
            let record_field: RecordField<Variable> = subs[i3].map(|_| variable);

            (field_name, record_field)
        });

    Ok((it, var))
}

pub fn gather_fields(
    subs: &Subs,
    other_fields: RecordFields,
    var: Variable,
) -> Result<RecordStructure, RecordFieldsError> {
    let (it, ext) = gather_fields_unsorted_iter(subs, other_fields, var)?;

    let mut result: Vec<_> = it
        .map(|(ref_label, field)| (ref_label.clone(), field))
        .collect();

    result.sort_by(|(a, _), (b, _)| a.cmp(b));

    Ok(RecordStructure {
        fields: result,
        ext,
    })
}

#[derive(Debug)]
pub enum GatherTagsError {
    NotATagUnion(Variable),
}

/// Gathers tag payloads of a type, assuming it is a tag.
///
/// If the given type is unbound or an error, no payloads are returned.
///
/// If the given type cannot be seen as a tag, unbound type, or error, this
/// function returns an error.
pub fn gather_tags_unsorted_iter(
    subs: &Subs,
    other_fields: UnionTags,
    mut var: Variable,
) -> Result<
    (
        impl Iterator<Item = (&TagName, VariableSubsSlice)> + '_,
        Variable,
    ),
    GatherTagsError,
> {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    let mut stack = vec![other_fields];

    loop {
        match subs.get_content_without_compacting(var) {
            Structure(TagUnion(sub_fields, sub_ext)) => {
                stack.push(*sub_fields);

                var = *sub_ext;
            }

            Structure(FunctionOrTagUnion(_tag_name_index, _, _sub_ext)) => {
                todo!("this variant does not use SOA yet, and therefore this case is unreachable right now")
                // let sub_fields: UnionTags = (*tag_name_index).into();
                // stack.push(sub_fields);
                //
                // var = *sub_ext;
            }

            Structure(RecursiveTagUnion(_, _sub_fields, _sub_ext)) => {
                todo!("this variant does not use SOA yet, and therefore this case is unreachable right now")
                // stack.push(*sub_fields);
                //
                // var = *sub_ext;
            }

            Alias(_, _, actual_var, _) => {
                var = *actual_var;
            }

            Structure(EmptyTagUnion) => break,
            FlexVar(_) | FlexAbleVar(_, _) => break,

            // TODO investigate, this likely can happen when there is a type error
            RigidVar(_) | RigidAbleVar(_, _) => break,

            Error => break,

            _ => return Err(GatherTagsError::NotATagUnion(var)),
        }
    }

    let it = stack
        .into_iter()
        .flat_map(|union_tags| union_tags.iter_all())
        .map(move |(i1, i2)| {
            let tag_name: &TagName = &subs[i1];
            let subs_slice = subs[i2];

            (tag_name, subs_slice)
        });

    Ok((it, var))
}

pub fn gather_tags_slices(
    subs: &Subs,
    other_fields: UnionTags,
    var: Variable,
) -> Result<(Vec<(TagName, VariableSubsSlice)>, Variable), GatherTagsError> {
    let (it, ext) = gather_tags_unsorted_iter(subs, other_fields, var)?;

    let mut result: Vec<_> = it
        .map(|(ref_label, field): (_, VariableSubsSlice)| (ref_label.clone(), field))
        .collect();

    result.sort_by(|(a, _), (b, _)| a.cmp(b));

    Ok((result, ext))
}

pub fn gather_tags(
    subs: &Subs,
    other_fields: UnionTags,
    var: Variable,
) -> Result<TagUnionStructure, GatherTagsError> {
    let (it, ext) = gather_tags_unsorted_iter(subs, other_fields, var)?;

    let mut result: Vec<_> = it
        .map(|(ref_label, field): (_, VariableSubsSlice)| {
            (ref_label.clone(), subs.get_subs_slice(field))
        })
        .collect();

    result.sort_by(|(a, _), (b, _)| a.cmp(b));

    Ok(TagUnionStructure {
        fields: result,
        ext,
    })
}

fn instantiate_lambda_sets_as_unspecialized(
    typ: &mut Type,
    able_var: Variable,
    ability_member: Symbol,
) {
    // REGION-ORDERING: done in pre-order via the following pseudo code:
    //
    // Type_function = \region ->
    // 	let left_type, new_region = Type (region + 1)
    //   let right_type, new_region = Type (new_region)
    //   let func_type = left_type -[Lambda region]-> right_type
    //   (func_type, new_region)
    //
    // Since we want to pop types in pre-order, they should be pushed onto the
    // stack in post-order
    let mut stack = vec![typ];
    let mut region = 0;

    let mut new_uls = || {
        region += 1;
        Type::UnspecializedLambdaSet {
            unspecialized: Uls(able_var, ability_member, region),
        }
    };

    while let Some(typ) = stack.pop() {
        match typ {
            Type::EmptyRec => {}
            Type::EmptyTagUnion => {}
            Type::Function(args, lambda_set, ret) => {
                debug_assert!(
                    matches!(**lambda_set, Type::Variable(..)),
                    "lambda set already bound"
                );

                **lambda_set = new_uls();
                stack.push(ret);
                stack.extend(args.iter_mut().rev());
            }
            Type::Record(fields, ext) => {
                stack.extend(ext.iter_mut());
                for (_, x) in fields.iter_mut() {
                    stack.push(x.as_inner_mut());
                }
            }
            Type::TagUnion(tags, ext) | Type::RecursiveTagUnion(_, tags, ext) => {
                stack.extend(ext.iter_mut());
                for (_, ts) in tags {
                    for t in ts.iter_mut().rev() {
                        stack.push(t);
                    }
                }
            }
            Type::FunctionOrTagUnion(_, _, ext) => {
                stack.extend(ext.iter_mut());
            }
            Type::ClosureTag {
                name: _,
                captures,
                ambient_function: _,
            } => {
                stack.extend(captures.iter_mut().rev());
            }
            Type::UnspecializedLambdaSet { .. } => {
                internal_error!("attempting to re-instantiate ULS")
            }
            Type::DelayedAlias(AliasCommon {
                symbol: _,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types: _, // these are irrelevant for ULS instantiation, since they're inferred or closed
            }) => {
                for lambda_set in lambda_set_variables.iter_mut() {
                    debug_assert!(matches!(lambda_set.0, Type::Variable(_)));
                    lambda_set.0 = new_uls();
                }
                stack.extend(type_arguments.iter_mut().rev().map(|ta| &mut ta.value.typ));
            }
            Type::Alias {
                symbol: _,
                type_arguments,
                lambda_set_variables,
                infer_ext_in_output_types: _, // these are irrelevant for ULS instantiation, since they're inferred
                actual,
                kind: _,
            } => {
                for lambda_set in lambda_set_variables.iter_mut() {
                    debug_assert!(matches!(lambda_set.0, Type::Variable(_)));
                    lambda_set.0 = new_uls();
                }
                stack.push(actual);
                stack.extend(type_arguments.iter_mut().rev().map(|t| &mut t.typ));
            }
            Type::HostExposedAlias {
                name: _,
                type_arguments,
                lambda_set_variables,
                actual_var: _,
                actual,
            } => {
                for lambda_set in lambda_set_variables.iter_mut() {
                    debug_assert!(matches!(lambda_set.0, Type::Variable(_)));
                    lambda_set.0 = new_uls();
                }
                stack.push(actual);
                stack.extend(type_arguments.iter_mut().rev());
            }
            Type::Apply(_sym, args, _region) => {
                stack.extend(args.iter_mut().rev().map(|t| &mut t.value));
            }
            Type::Variable(_) => {}
            Type::RangedNumber(_) => {}
            Type::Error => {}
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn instantiate_lambda_sets_as_unspecialized() {
        let mut var_store = VarStore::default();
        let l1 = Box::new(Type::Variable(var_store.fresh()));
        let l2 = Box::new(Type::Variable(var_store.fresh()));
        let l3 = Box::new(Type::Variable(var_store.fresh()));
        let mut typ = Type::Function(
            vec![Type::Function(vec![], l2, Box::new(Type::EmptyRec))],
            l1,
            Box::new(Type::TagUnion(
                vec![(
                    TagName("A".into()),
                    vec![Type::Function(vec![], l3, Box::new(Type::EmptyRec))],
                )],
                TypeExtension::Closed,
            )),
        );

        let able_var = var_store.fresh();
        let member = Symbol::UNDERSCORE;
        typ.instantiate_lambda_sets_as_unspecialized(able_var, member);

        macro_rules! check_uls {
            ($typ:expr, $region:literal) => {{
                match $typ {
                    Type::UnspecializedLambdaSet {
                        unspecialized: Uls(var1, member1, $region),
                    } => {
                        assert!(var1 == able_var && member1 == member)
                    }
                    _ => panic!(),
                }
            }};
        }

        match typ {
            Type::Function(args, l1, ret) => {
                check_uls!(*l1, 1);

                match args.as_slice() {
                    [Type::Function(args, l2, ret)] => {
                        check_uls!(**l2, 2);
                        assert!(args.is_empty());
                        assert!(matches!(**ret, Type::EmptyRec));
                    }
                    _ => panic!(),
                }

                match *ret {
                    Type::TagUnion(tags, TypeExtension::Closed) => match tags.as_slice() {
                        [(name, args)] => {
                            assert_eq!(name.0.as_str(), "A");
                            match args.as_slice() {
                                [Type::Function(args, l3, ret)] => {
                                    check_uls!(**l3, 3);
                                    assert!(args.is_empty());
                                    assert!(matches!(**ret, Type::EmptyRec));
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
}

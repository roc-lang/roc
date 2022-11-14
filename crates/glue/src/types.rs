use crate::enums::Enums;
use crate::structs::Structs;
use bumpalo::Bump;
use fnv::FnvHashMap;
use roc_builtins::bitcode::{
    FloatWidth::*,
    IntWidth::{self, *},
};
use roc_collections::VecMap;
use roc_module::{
    ident::TagName,
    symbol::{Interns, Symbol},
};
use roc_mono::layout::{
    cmp_fields, ext_var_is_empty_tag_union, round_up_to_alignment, Builtin, Discriminant, Layout,
    LayoutCache, LayoutInterner, UnionLayout,
};
use roc_target::TargetInfo;
use roc_types::{
    subs::{Content, FlatType, GetSubsSlice, Subs, UnionLabels, UnionTags, Variable},
    types::{AliasKind, RecordField},
};
use std::fmt::Display;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(usize);

impl TypeId {
    /// Used when making recursive pointers, which need to temporarily
    /// have *some* TypeId value until we later in the process determine
    /// their real TypeId and can go back and fix them up.
    pub(crate) const PENDING: Self = Self(usize::MAX);

    /// When adding, we check for overflow based on whether we've exceeded this.
    const MAX: Self = Self(Self::PENDING.0 - 1);
}

#[derive(Debug, Clone)]
pub struct Types {
    // These are all indexed by TypeId
    types: Vec<RocType>,
    sizes: Vec<u32>,
    aligns: Vec<u32>,

    // Needed to check for duplicates
    types_by_name: FnvHashMap<String, TypeId>,

    /// Dependencies - that is, which type depends on which other type.
    /// This is important for declaration order in C; we need to output a
    /// type declaration earlier in the file than where it gets referenced by another type.
    deps: VecMap<TypeId, Vec<TypeId>>,
    target: TargetInfo,
}

impl Types {
    pub fn with_capacity(cap: usize, target_info: TargetInfo) -> Self {
        Self {
            target: target_info,
            types: Vec::with_capacity(cap),
            types_by_name: FnvHashMap::with_capacity_and_hasher(10, Default::default()),
            sizes: Vec::new(),
            aligns: Vec::new(),
            deps: VecMap::with_capacity(cap),
        }
    }

    pub fn is_equivalent(&self, a: &RocType, b: &RocType) -> bool {
        self.is_equivalent_help(RocTypeOrPending::Type(a), RocTypeOrPending::Type(b))
    }

    fn is_equivalent_help(&self, a: RocTypeOrPending, b: RocTypeOrPending) -> bool {
        use RocType::*;

        let (a, b) = match (a, b) {
            (RocTypeOrPending::Type(a), RocTypeOrPending::Type(b)) => (a, b),
            (RocTypeOrPending::Pending, RocTypeOrPending::Pending) => return true,
            _ => return false,
        };

        match (a, b) {
            (RocStr, RocStr) | (Bool, Bool) | (EmptyTagUnion, EmptyTagUnion) | (Unit, Unit) => true,
            (RocResult(ok_a, err_a), RocResult(ok_b, err_b)) => {
                self.is_equivalent_help(
                    self.get_type_or_pending(*ok_a),
                    self.get_type_or_pending(*ok_b),
                ) && self.is_equivalent_help(
                    self.get_type_or_pending(*err_a),
                    self.get_type_or_pending(*err_b),
                )
            }
            (Num(num_a), Num(num_b)) => num_a == num_b,
            (RocList(elem_a), RocList(elem_b))
            | (RocSet(elem_a), RocSet(elem_b))
            | (RocBox(elem_a), RocBox(elem_b))
            | (RecursivePointer(elem_a), RecursivePointer(elem_b)) => self.is_equivalent_help(
                self.get_type_or_pending(*elem_a),
                self.get_type_or_pending(*elem_b),
            ),
            (RocDict(key_a, val_a), RocDict(key_b, val_b)) => {
                self.is_equivalent_help(
                    self.get_type_or_pending(*key_a),
                    self.get_type_or_pending(*key_b),
                ) && self.is_equivalent_help(
                    self.get_type_or_pending(*val_a),
                    self.get_type_or_pending(*val_b),
                )
            }
            (TagUnion(union_a), TagUnion(union_b)) => {
                use RocTagUnion::*;

                match (union_a, union_b) {
                    (
                        SingleTagStruct {
                            name: _,
                            tag_name: tag_name_a,
                            payload_fields: payload_fields_a,
                        },
                        SingleTagStruct {
                            name: _,
                            tag_name: tag_name_b,
                            payload_fields: payload_fields_b,
                        },
                    ) => tag_name_a == tag_name_b && payload_fields_a == payload_fields_b,
                    (
                        NonNullableUnwrapped {
                            name: _,
                            tag_name: tag_name_a,
                            payload: payload_a,
                        },
                        NonNullableUnwrapped {
                            name: _,
                            tag_name: tag_name_b,
                            payload: payload_b,
                        },
                    ) => {
                        tag_name_a == tag_name_b
                            && self.is_equivalent_help(
                                self.get_type_or_pending(*payload_a),
                                self.get_type_or_pending(*payload_b),
                            )
                    }
                    (Enumeration { tags: tags_a, .. }, Enumeration { tags: tags_b, .. }) => {
                        tags_a == tags_b
                    }
                    (
                        NonRecursive {
                            tags: tags_a,
                            discriminant_size: disc_w_a,
                            discriminant_offset: disc_o_a,
                            ..
                        },
                        NonRecursive {
                            tags: tags_b,
                            discriminant_size: disc_w_b,
                            discriminant_offset: disc_o_b,
                            ..
                        },
                    )
                    | (
                        Recursive {
                            tags: tags_a,
                            discriminant_size: disc_w_a,
                            discriminant_offset: disc_o_a,
                            ..
                        },
                        Recursive {
                            tags: tags_b,
                            discriminant_size: disc_w_b,
                            discriminant_offset: disc_o_b,
                            ..
                        },
                    ) => {
                        if disc_w_a != disc_w_b
                            || disc_o_a != disc_o_b
                            || tags_a.len() != tags_b.len()
                        {
                            false
                        } else {
                            tags_a.iter().zip(tags_b.iter()).all(
                                |((name_a, opt_id_a), (name_b, opt_id_b))| {
                                    name_a == name_b
                                        && match (opt_id_a, opt_id_b) {
                                            (Some(id_a), Some(id_b)) => self.is_equivalent_help(
                                                self.get_type_or_pending(*id_a),
                                                self.get_type_or_pending(*id_b),
                                            ),
                                            (None, None) => true,
                                            (None, Some(_)) | (Some(_), None) => false,
                                        }
                                },
                            )
                        }
                    }
                    (
                        NullableWrapped { tags: tags_a, .. },
                        NullableWrapped { tags: tags_b, .. },
                    ) => {
                        if tags_a.len() != tags_b.len() {
                            false
                        } else {
                            tags_a.iter().zip(tags_b.iter()).all(
                                |((name_a, opt_id_a), (name_b, opt_id_b))| {
                                    name_a == name_b
                                        && match (opt_id_a, opt_id_b) {
                                            (Some(id_a), Some(id_b)) => self.is_equivalent_help(
                                                self.get_type_or_pending(*id_a),
                                                self.get_type_or_pending(*id_b),
                                            ),
                                            (None, None) => true,
                                            (None, Some(_)) | (Some(_), None) => false,
                                        }
                                },
                            )
                        }
                    }
                    (
                        NullableUnwrapped {
                            null_tag: null_tag_a,
                            non_null_tag: non_null_tag_a,
                            non_null_payload: non_null_payload_a,
                            null_represents_first_tag: null_represents_first_tag_a,
                            ..
                        },
                        NullableUnwrapped {
                            null_tag: null_tag_b,
                            non_null_tag: non_null_tag_b,
                            non_null_payload: non_null_payload_b,
                            null_represents_first_tag: null_represents_first_tag_b,
                            ..
                        },
                    ) => {
                        null_tag_a == null_tag_b
                            && non_null_tag_a == non_null_tag_b
                            && non_null_payload_a == non_null_payload_b
                            && null_represents_first_tag_a == null_represents_first_tag_b
                    }
                    // These are all listed explicitly so that if we ever add a new variant,
                    // we'll get an exhaustiveness error here.
                    (SingleTagStruct { .. }, _)
                    | (_, SingleTagStruct { .. })
                    | (NonNullableUnwrapped { .. }, _)
                    | (_, NonNullableUnwrapped { .. })
                    | (Enumeration { .. }, _)
                    | (_, Enumeration { .. })
                    | (NonRecursive { .. }, _)
                    | (_, NonRecursive { .. })
                    | (Recursive { .. }, _)
                    | (_, Recursive { .. })
                    | (NullableUnwrapped { .. }, _)
                    | (_, NullableUnwrapped { .. }) => false,
                }
            }
            (
                Struct {
                    fields: fields_a, ..
                },
                Struct {
                    fields: fields_b, ..
                },
            ) => {
                if fields_a.len() == fields_b.len() {
                    fields_a
                        .iter()
                        .zip(fields_b.iter())
                        .all(|((name_a, id_a), (name_b, id_b))| {
                            name_a == name_b
                                && self.is_equivalent_help(
                                    self.get_type_or_pending(*id_a),
                                    self.get_type_or_pending(*id_b),
                                )
                        })
                } else {
                    false
                }
            }
            (
                TagUnionPayload {
                    fields: fields_a, ..
                },
                TagUnionPayload {
                    fields: fields_b, ..
                },
            ) => {
                if fields_a.len() == fields_b.len() {
                    fields_a
                        .iter()
                        .zip(fields_b.iter())
                        .all(|((name_a, id_a), (name_b, id_b))| {
                            name_a == name_b
                                && self.is_equivalent_help(
                                    self.get_type_or_pending(*id_a),
                                    self.get_type_or_pending(*id_b),
                                )
                        })
                } else {
                    false
                }
            }
            (
                Function {
                    name: name_a,
                    args: args_a,
                    ret: ret_a,
                },
                Function {
                    name: name_b,
                    args: args_b,
                    ret: ret_b,
                },
            ) => {
                // for functions, the name is actually important because two functions
                // with the same type could have completely different implementations!
                if name_a == name_b
                    && args_a.len() == args_b.len()
                    && self.is_equivalent_help(
                        self.get_type_or_pending(*ret_a),
                        self.get_type_or_pending(*ret_b),
                    )
                {
                    args_a.iter().zip(args_b.iter()).all(|(id_a, id_b)| {
                        self.is_equivalent_help(
                            self.get_type_or_pending(*id_a),
                            self.get_type_or_pending(*id_b),
                        )
                    })
                } else {
                    false
                }
            }
            // These are all listed explicitly so that if we ever add a new variant,
            // we'll get an exhaustiveness error here.
            (RocStr, _)
            | (_, RocStr)
            | (Bool, _)
            | (_, Bool)
            | (RocResult(_, _), _)
            | (_, RocResult(_, _))
            | (Num(_), _)
            | (_, Num(_))
            | (RocList(_), _)
            | (_, RocList(_))
            | (RocDict(_, _), _)
            | (_, RocDict(_, _))
            | (RocSet(_), _)
            | (_, RocSet(_))
            | (RocBox(_), _)
            | (_, RocBox(_))
            | (TagUnion(_), _)
            | (_, TagUnion(_))
            | (EmptyTagUnion, _)
            | (_, EmptyTagUnion)
            | (Struct { .. }, _)
            | (_, Struct { .. })
            | (TagUnionPayload { .. }, _)
            | (_, TagUnionPayload { .. })
            | (RecursivePointer(_), _)
            | (_, RecursivePointer(_))
            | (Function { .. }, _)
            | (_, Function { .. }) => false,
        }
    }

    pub fn add_named<'a>(
        &mut self,
        interner: &LayoutInterner<'a>,
        name: String,
        typ: RocType,
        layout: Layout<'a>,
    ) -> TypeId {
        if let Some(existing_type_id) = self.types_by_name.get(&name) {
            let existing_type = self.get_type(*existing_type_id);

            if self.is_equivalent(existing_type, &typ) {
                *existing_type_id
            } else {
                // TODO report this gracefully!
                panic!(
                    "Duplicate name detected - {:?} could refer to either {:?} or {:?}",
                    name, existing_type, typ
                );
            }
        } else {
            let id = self.add_anonymous(interner, typ, layout);

            self.types_by_name.insert(name, id);

            id
        }
    }

    pub fn add_anonymous<'a>(
        &mut self,
        interner: &LayoutInterner<'a>,
        typ: RocType,
        layout: Layout<'a>,
    ) -> TypeId {
        for (id, existing_type) in self.types.iter().enumerate() {
            if self.is_equivalent(&typ, existing_type) {
                return TypeId(id);
            }
        }

        let id = TypeId(self.types.len());

        assert!(id.0 <= TypeId::MAX.0);

        self.types.push(typ);
        self.sizes
            .push(layout.stack_size_without_alignment(interner, self.target));
        self.aligns
            .push(layout.alignment_bytes(interner, self.target));

        id
    }

    pub fn depends(&mut self, id: TypeId, depends_on: TypeId) {
        self.deps.get_or_insert(id, Vec::new).push(depends_on);
    }

    pub fn get_type(&self, id: TypeId) -> &RocType {
        match self.types.get(id.0) {
            Some(typ) => typ,
            None => unreachable!("{:?}", id),
        }
    }

    fn get_type_or_pending(&self, id: TypeId) -> RocTypeOrPending {
        match self.types.get(id.0) {
            Some(typ) => RocTypeOrPending::Type(typ),
            None if id == TypeId::PENDING => RocTypeOrPending::Pending,
            None => unreachable!("{:?}", id),
        }
    }

    /// Contrast this with the size_ignoring_alignment method
    pub fn size_rounded_to_alignment(&self, id: TypeId) -> u32 {
        let size_ignoring_alignment = self.size_ignoring_alignment(id);
        let alignment = self.align(id);

        round_up_to_alignment(size_ignoring_alignment, alignment)
    }

    /// Contrast this with the size_rounded_to_alignment method
    pub fn size_ignoring_alignment(&self, id: TypeId) -> u32 {
        match self.sizes.get(id.0) {
            Some(size) => *size,
            None => unreachable!(),
        }
    }

    pub fn align(&self, id: TypeId) -> u32 {
        match self.aligns.get(id.0) {
            Some(align) => *align,
            None => unreachable!(),
        }
    }

    pub fn replace(&mut self, id: TypeId, typ: RocType) {
        debug_assert!(self.types.get(id.0).is_some());

        self.types[id.0] = typ;
    }

    pub fn ids(&self) -> impl ExactSizeIterator<Item = TypeId> {
        (0..self.types.len()).map(TypeId)
    }

    pub fn sorted_ids(&self) -> Vec<TypeId> {
        use roc_collections::{ReferenceMatrix, TopologicalSort};

        let mut matrix = ReferenceMatrix::new(self.types.len());

        for type_id in self.ids() {
            for dep in self.deps.get(&type_id).iter().flat_map(|x| x.iter()) {
                matrix.set_row_col(type_id.0, dep.0, true);
            }
        }

        match matrix.topological_sort_into_groups() {
            TopologicalSort::Groups { groups } => groups
                .into_iter()
                .flatten()
                .rev()
                .map(|n| TypeId(n as usize))
                .collect(),
            TopologicalSort::HasCycles {
                groups: _,
                nodes_in_cycle,
            } => unreachable!("Cyclic type definitions: {:?}", nodes_in_cycle),
        }
    }
}

enum RocTypeOrPending<'a> {
    Type(&'a RocType),
    /// A pending recursive pointer
    Pending,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RocType {
    RocStr,
    Bool,
    RocResult(TypeId, TypeId),
    Num(RocNum),
    RocList(TypeId),
    RocDict(TypeId, TypeId),
    RocSet(TypeId),
    RocBox(TypeId),
    TagUnion(RocTagUnion),
    EmptyTagUnion,
    Struct {
        name: String,
        fields: Vec<(String, TypeId)>,
    },
    TagUnionPayload {
        name: String,
        fields: Vec<(usize, TypeId)>,
    },
    /// A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    /// this would be the field of Cons containing the (recursive) StrConsList type,
    /// and the TypeId is the TypeId of StrConsList itself.
    RecursivePointer(TypeId),
    Function {
        name: String,
        args: Vec<TypeId>,
        ret: TypeId,
    },
    /// A zero-sized type, such as an empty record or a single-tag union with no payload
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum RocNum {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F32,
    F64,
    F128,
    Dec,
}

impl RocNum {
    /// These sizes don't vary by target.
    pub fn size(&self) -> u32 {
        use core::mem::size_of;

        let answer = match self {
            RocNum::I8 => size_of::<i8>(),
            RocNum::U8 => size_of::<u8>(),
            RocNum::I16 => size_of::<i16>(),
            RocNum::U16 => size_of::<u16>(),
            RocNum::I32 => size_of::<i32>(),
            RocNum::U32 => size_of::<u32>(),
            RocNum::I64 => size_of::<i64>(),
            RocNum::U64 => size_of::<u64>(),
            RocNum::I128 => size_of::<roc_std::I128>(),
            RocNum::U128 => size_of::<roc_std::U128>(),
            RocNum::F32 => size_of::<f32>(),
            RocNum::F64 => size_of::<f64>(),
            RocNum::F128 => todo!(),
            RocNum::Dec => size_of::<roc_std::RocDec>(),
        };

        answer as u32
    }
}

impl From<IntWidth> for RocNum {
    fn from(width: IntWidth) -> Self {
        match width {
            IntWidth::U8 => RocNum::U8,
            IntWidth::U16 => RocNum::U16,
            IntWidth::U32 => RocNum::U32,
            IntWidth::U64 => RocNum::U64,
            IntWidth::U128 => RocNum::U128,
            IntWidth::I8 => RocNum::I8,
            IntWidth::I16 => RocNum::I16,
            IntWidth::I32 => RocNum::I32,
            IntWidth::I64 => RocNum::I64,
            IntWidth::I128 => RocNum::I128,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RocTagUnion {
    Enumeration {
        name: String,
        tags: Vec<String>,
        size: u32,
    },
    /// A non-recursive tag union
    /// e.g. `Result a e : [Ok a, Err e]`
    NonRecursive {
        name: String,
        tags: Vec<(String, Option<TypeId>)>,
        discriminant_size: u32,
        discriminant_offset: u32,
    },
    /// A recursive tag union (general case)
    /// e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive {
        name: String,
        tags: Vec<(String, Option<TypeId>)>,
        discriminant_size: u32,
        discriminant_offset: u32,
    },
    /// Optimization: No need to store a tag ID (the payload is "unwrapped")
    /// e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
    NonNullableUnwrapped {
        name: String,
        tag_name: String,
        payload: TypeId, // These always have a payload.
    },
    /// Optimization: No need to store a tag ID (the payload is "unwrapped")
    /// e.g. `[Foo Str Bool]`
    SingleTagStruct {
        name: String,
        tag_name: String,
        payload_fields: Vec<TypeId>,
    },
    /// A recursive tag union that has an empty variant
    /// Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    /// It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    /// e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
    /// see also: https://youtu.be/ip92VMpf_-A?t=164
    NullableWrapped {
        name: String,
        index_of_null_tag: u16,
        tags: Vec<(String, Option<TypeId>)>,
        discriminant_size: u32,
        discriminant_offset: u32,
    },

    /// A recursive tag union with only two variants, where one is empty.
    /// Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
    /// e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
    NullableUnwrapped {
        name: String,
        /// e.g. Nil in `StrConsList : [Nil, Cons Str (ConsList Str)]`
        null_tag: String,
        /// e.g. Cons in `StrConsList : [Nil, Cons Str (ConsList Str)]`
        non_null_tag: String,
        /// There must be a payload associated with the non-null tag.
        /// Otherwise, this would have been an Enumeration!
        non_null_payload: TypeId,
        /// True iff the first tag (alphabetically) is represented by null.
        /// If this is false, it means the second tag is represented by null instead.
        null_represents_first_tag: bool,
    },
}

pub struct Env<'a> {
    arena: &'a Bump,
    subs: &'a Subs,
    layout_cache: LayoutCache<'a>,
    interns: &'a Interns,
    struct_names: Structs,
    enum_names: Enums,
    pending_recursive_types: VecMap<TypeId, Variable>,
    known_recursive_types: VecMap<Variable, TypeId>,
    target: TargetInfo,
}

impl<'a> Env<'a> {
    pub fn new(
        arena: &'a Bump,
        subs: &'a Subs,
        interns: &'a Interns,
        layout_interner: LayoutInterner<'a>,
        target: TargetInfo,
    ) -> Self {
        Env {
            arena,
            subs,
            interns,
            struct_names: Default::default(),
            enum_names: Default::default(),
            pending_recursive_types: Default::default(),
            known_recursive_types: Default::default(),
            layout_cache: LayoutCache::new(layout_interner, target),
            target,
        }
    }

    pub fn vars_to_types<I>(&mut self, variables: I) -> Types
    where
        I: Iterator<Item = Variable>,
    {
        let mut types = Types::with_capacity(variables.size_hint().0, self.target);

        for var in variables {
            self.add_type(var, &mut types);
        }

        self.resolve_pending_recursive_types(&mut types);

        types
    }

    fn add_type(&mut self, var: Variable, types: &mut Types) -> TypeId {
        roc_tracing::debug!(content=?roc_types::subs::SubsFmtContent(self.subs.get_content_without_compacting(var), self.subs), "adding type");

        let layout = self
            .layout_cache
            .from_var(self.arena, var, self.subs)
            .expect("Something weird ended up in the content");

        add_type_help(self, layout, var, None, types)
    }

    fn resolve_pending_recursive_types(&mut self, types: &mut Types) {
        // TODO if VecMap gets a drain() method, use that instead of doing take() and into_iter
        let pending = core::mem::take(&mut self.pending_recursive_types);

        for (type_id, root_var) in pending.into_iter() {
            let actual_type_id = *self
                .known_recursive_types
                .get(&root_var)
                .unwrap_or_else(|| {
                    unreachable!(
                        "There was no known recursive TypeId for the pending recursive type {:?}",
                        root_var
                    );
                });

            debug_assert!(
                matches!(types.get_type(type_id), RocType::RecursivePointer(TypeId::PENDING)),
                "The TypeId {:?} was registered as a pending recursive pointer, but was not stored in Types as one.",
                type_id
            );

            // size and alignment shouldn't change; this is still
            // a RecursivePointer, it's just pointing to something else.
            types.replace(type_id, RocType::RecursivePointer(actual_type_id));
        }
    }
}

fn add_type_help<'a>(
    env: &mut Env<'a>,
    layout: Layout<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;

    match subs.get_content_without_compacting(var) {
        Content::FlexVar(_)
        | Content::RigidVar(_)
        | Content::FlexAbleVar(_, _)
        | Content::RigidAbleVar(_, _) => {
            todo!("TODO give a nice error message for a non-concrete type being passed to the host")
        }
        Content::Structure(FlatType::Record(fields, ext)) => {
            let it = fields
                .unsorted_iterator(subs, *ext)
                .expect("something weird in content")
                .flat_map(|(label, field)| {
                    match field {
                        RecordField::Required(field_var)
                        | RecordField::Demanded(field_var)
                        | RecordField::RigidRequired(field_var) => {
                            Some((label.to_string(), field_var))
                        }
                        RecordField::Optional(_) | RecordField::RigidOptional(_) => {
                            // drop optional fields
                            None
                        }
                    }
                });

            let name = match opt_name {
                Some(sym) => sym.as_str(env.interns).to_string(),
                None => env.struct_names.get_name(var),
            };

            add_struct(env, name, it, types, layout, |name, fields| {
                RocType::Struct { name, fields }
            })
        }
        Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, *ext_var));

            add_tag_union(env, opt_name, tags, var, types, layout, None)
        }
        Content::Structure(FlatType::RecursiveTagUnion(rec_var, tags, ext_var)) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, *ext_var));

            let rec_root = subs.get_root_key_without_compacting(*rec_var);

            add_tag_union(env, opt_name, tags, var, types, layout, Some(rec_root))
        }
        Content::Structure(FlatType::Apply(symbol, _)) => match layout {
            Layout::Builtin(builtin) => {
                add_builtin_type(env, builtin, var, opt_name, types, layout)
            }
            _ => {
                if symbol.is_builtin() {
                    todo!(
                        "Handle Apply for builtin symbol {:?} and layout {:?}",
                        symbol,
                        layout
                    )
                } else {
                    todo!(
                        "Handle non-builtin Apply for symbol {:?} and layout {:?}",
                        symbol,
                        layout
                    )
                }
            }
        },
        Content::Structure(FlatType::Func(args, closure_var, ret_var)) => {
            let args = env.subs.get_subs_slice(*args);
            let mut arg_type_ids = Vec::with_capacity(args.len());

            for arg_var in args {
                let arg_layout = env
                    .layout_cache
                    .from_var(env.arena, *arg_var, env.subs)
                    .expect("Something weird ended up in the content");

                arg_type_ids.push(add_type_help(env, arg_layout, *arg_var, None, types));
            }

            let ret_type_id = {
                let ret_layout = env
                    .layout_cache
                    .from_var(env.arena, *ret_var, env.subs)
                    .expect("Something weird ended up in the content");

                add_type_help(env, ret_layout, *ret_var, None, types)
            };

            let name = format!("TODO_roc_function_{:?}", closure_var);
            let fn_type_id = types.add_named(
                &env.layout_cache.interner,
                name.clone(),
                RocType::Function {
                    name,
                    args: arg_type_ids.clone(),
                    ret: ret_type_id,
                },
                layout,
            );

            types.depends(fn_type_id, ret_type_id);

            for arg_type_id in arg_type_ids {
                types.depends(fn_type_id, arg_type_id);
            }

            fn_type_id
        }
        Content::Structure(FlatType::FunctionOrTagUnion(_, _, _)) => {
            todo!()
        }
        Content::Structure(FlatType::EmptyRecord) => {
            types.add_anonymous(&env.layout_cache.interner, RocType::Unit, layout)
        }
        Content::Structure(FlatType::EmptyTagUnion) => {
            types.add_anonymous(&env.layout_cache.interner, RocType::EmptyTagUnion, layout)
        }
        Content::Alias(name, alias_vars, real_var, _) => {
            if name.is_builtin() {
                match layout {
                    Layout::Builtin(builtin) => {
                        add_builtin_type(env, builtin, var, opt_name, types, layout)
                    }
                    Layout::Union(union_layout) if *name == Symbol::BOOL_BOOL => {
                        if cfg!(debug_assertions) {
                            match union_layout {
                                UnionLayout::NonRecursive(tag_layouts) => {
                                    // Bool should always have exactly two tags: True and False
                                    debug_assert_eq!(tag_layouts.len(), 2);

                                    // Both tags should have no payload
                                    debug_assert_eq!(tag_layouts[0].len(), 0);
                                    debug_assert_eq!(tag_layouts[1].len(), 0);
                                }
                                _ => debug_assert!(false),
                            }
                        }

                        types.add_anonymous(&env.layout_cache.interner, RocType::Bool, layout)
                    }
                    Layout::Union(union_layout) if *name == Symbol::RESULT_RESULT => {
                        match union_layout {
                            UnionLayout::NonRecursive(tags) => {
                                // Result should always have exactly two tags: Ok and Err
                                debug_assert_eq!(tags.len(), 2);

                                let type_vars =
                                    env.subs.get_subs_slice(alias_vars.type_variables());

                                let ok_var = type_vars[0];
                                let ok_layout =
                                    env.layout_cache.from_var(env.arena, ok_var, subs).unwrap();
                                let ok_id = add_type_help(env, ok_layout, ok_var, None, types);

                                let err_var = type_vars[1];
                                let err_layout =
                                    env.layout_cache.from_var(env.arena, err_var, subs).unwrap();
                                let err_id = add_type_help(env, err_layout, err_var, None, types);

                                let type_id = types.add_anonymous(
                                    &env.layout_cache.interner,
                                    RocType::RocResult(ok_id, err_id),
                                    layout,
                                );

                                types.depends(type_id, ok_id);
                                types.depends(type_id, err_id);

                                type_id
                            }
                            UnionLayout::Recursive(_)
                            | UnionLayout::NonNullableUnwrapped(_)
                            | UnionLayout::NullableWrapped { .. }
                            | UnionLayout::NullableUnwrapped { .. } => {
                                unreachable!();
                            }
                        }
                    }
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                // If this was a non-builtin type alias, we can use that alias name
                // in the generated bindings.
                add_type_help(env, layout, *real_var, Some(*name), types)
            }
        }
        Content::RangedNumber(_) => todo!(),
        Content::Error => todo!(),
        Content::RecursionVar { structure, .. } => {
            let type_id = types.add_anonymous(
                &env.layout_cache.interner,
                RocType::RecursivePointer(TypeId::PENDING),
                layout,
            );

            // These should be different Variables, but the same layout!
            debug_assert_eq!(
                layout,
                env.layout_cache
                    .from_var(env.arena, *structure, subs)
                    .unwrap()
            );

            let root_var = subs.get_root_key_without_compacting(var);

            env.pending_recursive_types.insert(type_id, root_var);

            type_id
        }
        Content::LambdaSet(_) => todo!(),
    }
}

fn add_builtin_type<'a>(
    env: &mut Env<'a>,
    builtin: Builtin<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
    layout: Layout<'a>,
) -> TypeId {
    use Content::*;
    use FlatType::*;

    let builtin_type = env.subs.get_content_without_compacting(var);

    match (builtin, builtin_type) {
        (Builtin::Int(width), _) => match width {
            U8 => types.add_anonymous(&env.layout_cache.interner, RocType::Num(RocNum::U8), layout),
            U16 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::U16),
                layout,
            ),
            U32 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::U32),
                layout,
            ),
            U64 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::U64),
                layout,
            ),
            U128 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::U128),
                layout,
            ),
            I8 => types.add_anonymous(&env.layout_cache.interner, RocType::Num(RocNum::I8), layout),
            I16 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::I16),
                layout,
            ),
            I32 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::I32),
                layout,
            ),
            I64 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::I64),
                layout,
            ),
            I128 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::I128),
                layout,
            ),
        },
        (Builtin::Float(width), _) => match width {
            F32 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::F32),
                layout,
            ),
            F64 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::F64),
                layout,
            ),
            F128 => types.add_anonymous(
                &env.layout_cache.interner,
                RocType::Num(RocNum::F128),
                layout,
            ),
        },
        (Builtin::Decimal, _) => types.add_anonymous(
            &env.layout_cache.interner,
            RocType::Num(RocNum::Dec),
            layout,
        ),
        (Builtin::Bool, _) => {
            types.add_anonymous(&env.layout_cache.interner, RocType::Bool, layout)
        }
        (Builtin::Str, _) => {
            types.add_anonymous(&env.layout_cache.interner, RocType::RocStr, layout)
        }
        (Builtin::List(elem_layout), Structure(Apply(Symbol::LIST_LIST, args))) => {
            let args = env.subs.get_subs_slice(*args);
            debug_assert_eq!(args.len(), 1);

            let elem_id = add_type_help(env, *elem_layout, args[0], opt_name, types);
            let list_id = types.add_anonymous(
                &env.layout_cache.interner,
                RocType::RocList(elem_id),
                layout,
            );

            types.depends(list_id, elem_id);

            list_id
        }
        (
            Builtin::List(elem_layout),
            Alias(Symbol::DICT_DICT, _alias_variables, alias_var, AliasKind::Opaque),
        ) => {
            match (
                elem_layout,
                env.subs.get_content_without_compacting(*alias_var),
            ) {
                (
                    Layout::Struct { field_layouts, .. },
                    Content::Structure(FlatType::Apply(Symbol::LIST_LIST, args_subs_slice)),
                ) => {
                    let (key_var, val_var) = {
                        let args_tuple = env.subs.get_subs_slice(*args_subs_slice);

                        debug_assert_eq!(args_tuple.len(), 1);

                        match env.subs.get_content_without_compacting(args_tuple[0]) {
                            Content::Structure(FlatType::TagUnion(union_tags, ext_var)) => {
                                let (mut iter, _) = union_tags.sorted_iterator_and_ext(env.subs, *ext_var);
                                let payloads = iter.next().unwrap().1;

                                debug_assert_eq!(iter.next(), None);

                                (payloads[0], payloads[1])
                            }
                            _ => {
                                unreachable!()
                            }
                        }
                    };

                    debug_assert_eq!(field_layouts.len(), 2);

                    let key_id = add_type_help(env, field_layouts[0], key_var, opt_name, types);
                    let val_id = add_type_help(env, field_layouts[1], val_var, opt_name, types);
                    let dict_id = types.add_anonymous(&env.layout_cache.interner,RocType::RocDict(key_id, val_id), layout);

                    types.depends(dict_id, key_id);
                    types.depends(dict_id, val_id);

                    dict_id
                }
                (elem_layout, alias_content) => unreachable!(
                    "Unrecognized List element for Dict. Layout was: {:?} and alias_content was: {:?}",
                    elem_layout,
                    alias_content
                ),
            }
        }
        (
            Builtin::List(elem_layout),
            Alias(Symbol::SET_SET, _alias_vars, alias_var, AliasKind::Opaque),
        ) => {
            match (
                elem_layout,
                env.subs.get_content_without_compacting(*alias_var),
            ) {
                (
                    Layout::Struct { field_layouts, .. },
                    Alias(Symbol::DICT_DICT, alias_args, _alias_var, AliasKind::Opaque),
                ) => {
                    let dict_type_vars = env.subs.get_subs_slice(alias_args.type_variables());

                    debug_assert_eq!(dict_type_vars.len(), 2);

                    // Sets only use the key of the Dict they wrap, not the value
                    let elem_var = dict_type_vars[0];

                    debug_assert_eq!(field_layouts.len(), 2);

                    let elem_id = add_type_help(env, field_layouts[0], elem_var, opt_name, types);
                    let set_id = types.add_anonymous(&env.layout_cache.interner,RocType::RocSet(elem_id), layout);

                    types.depends(set_id, elem_id);

                    set_id
                }
                (elem_layout, alias_content) => unreachable!(
                    "Unrecognized List element for Set. Layout was: {:?} and alias_content was: {:?}",
                    elem_layout,
                    alias_content
                ),
            }
        }
        (Builtin::List(elem_layout), alias) => {
            unreachable!(
                "The type alias {:?} was not an Apply(Symbol::LIST_LIST) as expected, given that its builtin was Builtin::List({:?})",
                alias, elem_layout
            );
        }
    }
}

fn add_struct<'a, I, L, F>(
    env: &mut Env<'a>,
    name: String,
    fields: I,
    types: &mut Types,
    layout: Layout<'a>,
    to_type: F,
) -> TypeId
where
    I: IntoIterator<Item = (L, Variable)>,
    L: Display + Ord,
    F: FnOnce(String, Vec<(L, TypeId)>) -> RocType,
{
    let subs = env.subs;
    let fields_iter = &mut fields.into_iter();
    let mut sortables =
        bumpalo::collections::Vec::with_capacity_in(fields_iter.size_hint().0, env.arena);

    for (label, field_var) in fields_iter {
        sortables.push((
            label,
            field_var,
            env.layout_cache
                .from_var(env.arena, field_var, subs)
                .unwrap(),
        ));
    }

    sortables.sort_by(|(label1, _, layout1), (label2, _, layout2)| {
        cmp_fields(
            &env.layout_cache.interner,
            label1,
            layout1,
            label2,
            layout2,
            env.layout_cache.target_info,
        )
    });

    let fields = sortables
        .into_iter()
        .map(|(label, field_var, field_layout)| {
            let type_id = add_type_help(env, field_layout, field_var, None, types);

            (label, type_id)
        })
        .collect::<Vec<(L, TypeId)>>();

    types.add_named(
        &env.layout_cache.interner,
        name.clone(),
        to_type(name, fields),
        layout,
    )
}

fn add_tag_union<'a>(
    env: &mut Env<'a>,
    opt_name: Option<Symbol>,
    union_tags: &UnionTags,
    var: Variable,
    types: &mut Types,
    layout: Layout<'a>,
    rec_root: Option<Variable>,
) -> TypeId {
    let subs = env.subs;
    let name = match opt_name {
        Some(sym) => sym.as_str(env.interns).to_string(),
        None => env.enum_names.get_name(var),
    };

    let tag_union_type = match layout {
        _ if union_tags.is_newtype_wrapper(subs)
            && matches!(
                subs.get_content_without_compacting(var),
                // Make sure this is a tag union, *not* a recursive tag union!
                // Otherwise, we could end up with a recursive tag union
                // getting unwrapped incorrectly.
                Content::Structure(FlatType::TagUnion(_, _))
            ) =>
        {
            let (tag_name, payload_vars) = single_tag_payload(union_tags, subs);

            // A newtype wrapper should always have exactly one payload.
            debug_assert_eq!(payload_vars.len(), 1);

            // A newtype wrapper should always have the same layout as its payload.
            let payload_layout = layout;
            let payload_id = add_type_help(env, payload_layout, payload_vars[0], None, types);

            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name: tag_name.to_string(),
                payload_fields: vec![payload_id],
            }
        }
        Layout::Union(union_layout) => {
            use UnionLayout::*;

            match union_layout {
                // A non-recursive tag union
                // e.g. `Result ok err : [Ok ok, Err err]`
                NonRecursive(_) => {
                    let tags =
                        union_tags_to_types(&name, union_tags, subs, env, types, layout, false);
                    // TODO deal with empty tag union
                    let discriminant_size = Discriminant::from_number_of_tags(tags.len())
                        .stack_size()
                        .max(1);
                    let discriminant_offset = union_layout
                        .tag_id_offset(&env.layout_cache.interner, env.target)
                        .unwrap();

                    RocTagUnion::NonRecursive {
                        name: name.clone(),
                        tags,
                        discriminant_size,
                        discriminant_offset,
                    }
                }
                // A recursive tag union (general case)
                // e.g. `Expr : [Sym Str, Add Expr Expr]`
                Recursive(_) => {
                    let tags =
                        union_tags_to_types(&name, union_tags, subs, env, types, layout, true);
                    let discriminant_size =
                        Discriminant::from_number_of_tags(tags.len()).stack_size();
                    let discriminant_offset = union_layout
                        .tag_id_offset(&env.layout_cache.interner, env.target)
                        .unwrap();

                    RocTagUnion::Recursive {
                        name: name.clone(),
                        tags,
                        discriminant_size,
                        discriminant_offset,
                    }
                }
                NonNullableUnwrapped(_) => {
                    let (tag_name, payload_vars) = single_tag_payload(union_tags, subs);
                    let (tag_name, opt_payload) =
                        tag_to_type(&name, env, tag_name, payload_vars, types, layout, true);

                    // A recursive tag union with just one constructor
                    // Optimization: No need to store a tag ID (the payload is "unwrapped")
                    // e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
                    RocTagUnion::NonNullableUnwrapped {
                        name: name.clone(),
                        tag_name: tag_name.to_string(),
                        payload: opt_payload.unwrap(),
                    }
                }
                // A recursive tag union that has an empty variant
                // Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
                // It has more than one other variant, so they need tag IDs (payloads are "wrapped")
                // e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
                // see also: https://youtu.be/ip92VMpf_-A?t=164
                NullableWrapped {
                    nullable_id,
                    other_tags,
                } => {
                    let tags =
                        union_tags_to_types(&name, union_tags, subs, env, types, layout, true);
                    let discriminant_size =
                        Discriminant::from_number_of_tags(other_tags.len()).stack_size();
                    let discriminant_offset = union_layout
                        .tag_id_offset(&env.layout_cache.interner, env.target)
                        .unwrap();

                    // nullable_id refers to the index of the tag that is represented at runtime as NULL.
                    // For example, in `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`,
                    // the ids would be Empty = 0, More = 1, Single = 2, because that's how those tags are
                    // ordered alphabetically. Since the Empty tag will be represented at runtime as NULL,
                    // and since Empty's tag id is 0, here nullable_id would be 0.
                    RocTagUnion::NullableWrapped {
                        name: name.clone(),
                        index_of_null_tag: nullable_id,
                        tags,
                        discriminant_size,
                        discriminant_offset,
                    }
                }
                // A recursive tag union with only two variants, where one is empty.
                // Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
                // e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
                NullableUnwrapped {
                    nullable_id: null_represents_first_tag,
                    other_fields: _, // TODO use this!
                } => {
                    let mut tags =
                        union_tags_to_types(&name, union_tags, subs, env, types, layout, true);
                    // NullableUnwrapped tag unions should always have exactly 2 tags.
                    debug_assert_eq!(tags.len(), 2);

                    let null_tag;
                    let non_null;

                    if null_represents_first_tag {
                        // If nullable_id is true, then the null tag is second, which means
                        // pop() will return it because it's at the end of the vec.
                        null_tag = tags.pop().unwrap().0;
                        non_null = tags.pop().unwrap();
                    } else {
                        // The null tag is first, which means the tag with the payload is second.
                        non_null = tags.pop().unwrap();
                        null_tag = tags.pop().unwrap().0;
                    }

                    let (non_null_tag, non_null_payload) = non_null;

                    RocTagUnion::NullableUnwrapped {
                        name: name.clone(),
                        null_tag,
                        non_null_tag,
                        non_null_payload: non_null_payload.unwrap(),
                        null_represents_first_tag,
                    }
                }
            }
        }
        Layout::Builtin(Builtin::Int(int_width)) => {
            add_int_enumeration(union_tags, subs, &name, int_width)
        }
        Layout::Struct { field_layouts, .. } => {
            let (tag_name, payload_fields) =
                single_tag_payload_fields(union_tags, subs, field_layouts, env, types);

            // A recursive tag union with just one constructor
            // Optimization: No need to store a tag ID (the payload is "unwrapped")
            // e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name: tag_name.to_string(),
                payload_fields,
            }
        }
        Layout::Builtin(Builtin::Bool) => {
            // This isn't actually a Bool, but rather a 2-tag union with no payloads
            // (so it has the same layout as a Bool, but actually isn't one; if it were
            // a real Bool, it would have been handled elsewhere already!)
            add_int_enumeration(union_tags, subs, &name, IntWidth::U8)
        }
        Layout::Builtin(builtin) => {
            let type_id = add_builtin_type(env, builtin, var, opt_name, types, layout);
            let (tag_name, _) = single_tag_payload(union_tags, subs);

            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name: tag_name.to_string(),
                payload_fields: vec![type_id],
            }
        }
        Layout::Boxed(elem_layout) => {
            let (tag_name, payload_fields) =
                single_tag_payload_fields(union_tags, subs, &[*elem_layout], env, types);

            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name: tag_name.to_string(),
                payload_fields,
            }
        }
        Layout::LambdaSet(_) => {
            todo!();
        }
        Layout::RecursivePointer => {
            // A single-tag union which only wraps itself is erroneous and should have
            // been turned into an error earlier in the process.
            unreachable!();
        }
    };

    let typ = RocType::TagUnion(tag_union_type);
    let type_id = types.add_named(&env.layout_cache.interner, name, typ, layout);

    if let Some(rec_var) = rec_root {
        env.known_recursive_types.insert(rec_var, type_id);
    }

    type_id
}

fn add_int_enumeration(
    union_tags: &UnionLabels<TagName>,
    subs: &Subs,
    name: &str,
    int_width: IntWidth,
) -> RocTagUnion {
    let tags: Vec<String> = union_tags
        .iter_from_subs(subs)
        .map(|(tag_name, _)| tag_name.0.as_str().to_string())
        .collect();
    RocTagUnion::Enumeration {
        name: name.to_string(),
        tags,
        size: int_width.stack_size(),
    }
}

fn union_tags_to_types<'a>(
    name: &str,
    union_tags: &UnionLabels<TagName>,
    subs: &Subs,
    env: &mut Env<'a>,
    types: &mut Types,
    layout: Layout<'a>,
    is_recursive: bool,
) -> Vec<(String, Option<TypeId>)> {
    let mut tags: Vec<(String, Vec<Variable>)> = union_tags
        .iter_from_subs(subs)
        .map(|(tag_name, payload_vars)| {
            let name_str = tag_name.0.as_str().to_string();

            (name_str, payload_vars.to_vec())
        })
        .collect();

    // Sort tags alphabetically by tag name
    tags.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

    tags.into_iter()
        .map(|(tag_name, payload_vars)| {
            tag_to_type(
                name,
                env,
                tag_name,
                &payload_vars,
                types,
                layout,
                is_recursive,
            )
        })
        .collect()
}

fn single_tag_payload<'a>(
    union_tags: &'a UnionLabels<TagName>,
    subs: &'a Subs,
) -> (&'a str, &'a [Variable]) {
    let mut iter = union_tags.iter_from_subs(subs);
    let (tag_name, payload_vars) = iter.next().unwrap();
    // This should be a single-tag union.
    debug_assert_eq!(iter.next(), None);

    (tag_name.0.as_str(), payload_vars)
}

fn single_tag_payload_fields<'a, 'b>(
    union_tags: &'b UnionLabels<TagName>,
    subs: &'b Subs,
    field_layouts: &[Layout<'a>],
    env: &mut Env<'a>,
    types: &mut Types,
) -> (&'b str, Vec<TypeId>) {
    let (tag_name, payload_vars) = single_tag_payload(union_tags, subs);

    let payload_fields: Vec<TypeId> = payload_vars
        .iter()
        .zip(field_layouts.iter())
        .map(|(field_var, field_layout)| add_type_help(env, *field_layout, *field_var, None, types))
        .collect();

    (tag_name, payload_fields)
}

fn tag_to_type<'a, D: Display>(
    name: &str,
    env: &mut Env<'a>,
    tag_name: D,
    payload_vars: &[Variable],
    types: &mut Types,
    layout: Layout<'a>,
    is_recursive: bool,
) -> (D, Option<TypeId>) {
    match struct_fields_needed(env, payload_vars.iter().copied()) {
        0 => {
            // no payload
            (tag_name, None)
        }
        1 if !is_recursive => {
            // this isn't recursive and there's 1 payload item, so it doesn't
            // need its own struct - e.g. for `[Foo Str, Bar Str]` both of them
            // can have payloads of plain old Str, no struct wrapper needed.
            let payload_var = payload_vars.get(0).unwrap();
            let payload_layout = env
                .layout_cache
                .from_var(env.arena, *payload_var, env.subs)
                .expect("Something weird ended up in the content");
            let payload_id = add_type_help(env, payload_layout, *payload_var, None, types);

            (tag_name, Some(payload_id))
        }
        _ => {
            // create a RocType for the payload and save it
            let struct_name = format!("{}_{}", &name, tag_name); // e.g. "MyUnion_MyVariant"
            let fields = payload_vars.iter().copied().enumerate();
            let struct_id = add_struct(env, struct_name, fields, types, layout, |name, fields| {
                RocType::TagUnionPayload { name, fields }
            });

            (tag_name, Some(struct_id))
        }
    }
}

fn struct_fields_needed<I: IntoIterator<Item = Variable>>(env: &mut Env<'_>, vars: I) -> usize {
    let subs = env.subs;
    let arena = env.arena;

    vars.into_iter().fold(0, |count, var| {
        let layout = env.layout_cache.from_var(arena, var, subs).unwrap();

        if layout.is_dropped_because_empty() {
            count
        } else {
            count + 1
        }
    })
}

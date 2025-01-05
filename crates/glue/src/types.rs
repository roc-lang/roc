use crate::enums::Enums;
use crate::roc_type;
use crate::structs::Structs;
use bumpalo::Bump;
use fnv::FnvHashMap;
use roc_builtins::bitcode::{
    FloatWidth::*,
    IntWidth::{self, *},
};
use roc_collections::{MutMap, VecMap};
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::{
    ident::TagName,
    symbol::{Interns, Symbol},
};
use roc_mono::{
    ir::LambdaSetId,
    layout::{
        cmp_fields, ext_var_is_empty_tag_union, round_up_to_alignment, Builtin, Discriminant,
        InLayout, Layout, LayoutCache, LayoutInterner, LayoutRepr, TLLayoutInterner, UnionLayout,
    },
};
use roc_target::{Architecture, OperatingSystem, Target};
use roc_types::{
    subs::{Content, FlatType, GetSubsSlice, Label, Subs, SubsSlice, UnionLabels, Variable},
    types::{AliasKind, RecordField},
};
use std::convert::From;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub struct File {
    pub name: String,
    pub content: String,
}

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

// TODO: remove this and instead generate directly into roc_type::Types
// Probably want to fix roc_std::RocDict and update roc_type::Types to use it first.
#[derive(Debug, Clone)]
pub struct Types {
    // These are all indexed by TypeId
    types: Vec<RocType>,
    sizes: Vec<u32>,
    aligns: Vec<u32>,

    entry_points: Vec<(String, TypeId)>,

    // Needed to check for duplicates
    types_by_name: FnvHashMap<String, TypeId>,

    /// Dependencies - that is, which type depends on which other type.
    /// This is important for declaration order in C; we need to output a
    /// type declaration earlier in the file than where it gets referenced by another type.
    deps: VecMap<TypeId, Vec<TypeId>>,
    target: Target,
}

impl Types {
    const UNIT: TypeId = TypeId(0);

    pub fn with_capacity(cap: usize, target: Target) -> Self {
        let mut types = Vec::with_capacity(cap);
        let mut sizes = Vec::with_capacity(cap);
        let mut aligns = Vec::with_capacity(cap);

        types.push(RocType::Unit);
        sizes.push(1);
        aligns.push(1);

        Self {
            target,
            types,
            sizes,
            aligns,
            types_by_name: FnvHashMap::with_capacity_and_hasher(10, Default::default()),
            entry_points: Vec::new(),
            deps: VecMap::with_capacity(cap),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new_with_entry_points<'a>(
        arena: &'a Bump,
        subs: &'a Subs,
        interns: &'a Interns,
        glue_procs_by_layout: MutMap<Layout<'a>, &'a [String]>,
        layout_cache: LayoutCache<'a>,
        target: Target,
        mut entry_points: MutMap<Symbol, Variable>,
    ) -> Self {
        let mut types = Self::with_capacity(entry_points.len(), target);
        let mut env = Env::new(
            arena,
            subs,
            interns,
            layout_cache.interner,
            glue_procs_by_layout,
            target,
        );

        for (_symbol, var) in entry_points.clone() {
            env.lambda_set_ids = env.find_lambda_sets(var);
            let id = env.add_toplevel_type(var, &mut types);

            let key = entry_points
                .iter()
                .find_map(|(k, v)| (*v == var).then_some((*k, id)));

            if let Some((k, id)) = key {
                let name = k.as_str(env.interns).to_string();
                types.entry_points.push((name, id));
                entry_points.remove(&k);
            }
        }

        debug_assert!(entry_points.is_empty());

        env.resolve_pending_recursive_types(&mut types);

        types
    }

    pub fn entry_points(&self) -> &[(String, TypeId)] {
        self.entry_points.as_slice()
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
            (Unsized, Unsized) => true,
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
                            payload: payload_a,
                        },
                        SingleTagStruct {
                            name: _,
                            tag_name: tag_name_b,
                            payload: payload_b,
                        },
                    ) => tag_name_a == tag_name_b && payload_a == payload_b,
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
                            discriminant_size: disc_size_a,
                            ..
                        },
                        NonRecursive {
                            tags: tags_b,
                            discriminant_size: disc_size_b,
                            ..
                        },
                    )
                    | (
                        Recursive {
                            tags: tags_a,
                            discriminant_size: disc_size_a,
                            ..
                        },
                        Recursive {
                            tags: tags_b,
                            discriminant_size: disc_size_b,
                            ..
                        },
                    ) => {
                        if disc_size_a == disc_size_b && tags_a.len() == tags_b.len() {
                            // discriminant offset doesn't matter for equality,
                            // since it's determined 100% by other fields
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
                        } else {
                            false
                        }
                    }
                    (
                        NullableWrapped { tags: tags_a, .. },
                        NullableWrapped { tags: tags_b, .. },
                    ) => {
                        if tags_a.len() != tags_b.len() {
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
                        } else {
                            false
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
                    (NonNullableUnwrapped { .. }, _)
                    | (_, NonNullableUnwrapped { .. })
                    | (Enumeration { .. }, _)
                    | (_, Enumeration { .. })
                    | (NonRecursive { .. }, _)
                    | (_, NonRecursive { .. })
                    | (Recursive { .. }, _)
                    | (_, Recursive { .. })
                    | (SingleTagStruct { .. }, NullableWrapped { .. })
                    | (NullableWrapped { .. }, SingleTagStruct { .. })
                    | (NullableUnwrapped { .. }, _)
                    | (_, NullableUnwrapped { .. }) => false,
                }
            }
            (
                TagUnionPayload {
                    fields: RocStructFields::HasClosure { fields: fields_a },
                    name: _,
                },
                TagUnionPayload {
                    fields: RocStructFields::HasClosure { fields: fields_b },
                    name: _,
                },
            )
            | (
                Struct {
                    fields: RocStructFields::HasClosure { fields: fields_a },
                    name: _,
                },
                Struct {
                    fields: RocStructFields::HasClosure { fields: fields_b },
                    name: _,
                },
            ) => {
                if fields_a.len() == fields_b.len() {
                    fields_a.iter().zip(fields_b.iter()).all(
                        |((name_a, id_a, _), (name_b, id_b, _))| {
                            name_a == name_b
                                && self.is_equivalent_help(
                                    self.get_type_or_pending(*id_a),
                                    self.get_type_or_pending(*id_b),
                                )
                        },
                    )
                } else {
                    false
                }
            }
            (
                TagUnionPayload {
                    fields: RocStructFields::HasNoClosure { fields: fields_a },
                    name: _,
                },
                TagUnionPayload {
                    fields: RocStructFields::HasNoClosure { fields: fields_b },
                    name: _,
                },
            )
            | (
                Struct {
                    fields: RocStructFields::HasNoClosure { fields: fields_a },
                    name: _,
                },
                Struct {
                    fields: RocStructFields::HasNoClosure { fields: fields_b },
                    name: _,
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
                    fields: RocStructFields::HasClosure { .. },
                    name: _,
                },
                TagUnionPayload {
                    fields: RocStructFields::HasNoClosure { .. },
                    name: _,
                },
            )
            | (
                TagUnionPayload {
                    fields: RocStructFields::HasNoClosure { .. },
                    name: _,
                },
                TagUnionPayload {
                    fields: RocStructFields::HasClosure { .. },
                    name: _,
                },
            )
            | (
                Struct {
                    fields: RocStructFields::HasNoClosure { .. },
                    name: _,
                },
                Struct {
                    fields: RocStructFields::HasClosure { .. },
                    name: _,
                },
            )
            | (
                Struct {
                    fields: RocStructFields::HasClosure { .. },
                    name: _,
                },
                Struct {
                    fields: RocStructFields::HasNoClosure { .. },
                    name: _,
                },
            ) => false,
            (
                Function(RocFn {
                    function_name: name_a,
                    extern_name: extern_a,
                    args: args_a,
                    lambda_set: lambda_a,
                    ret: ret_a,
                    is_toplevel: is_toplevel_a,
                }),
                Function(RocFn {
                    function_name: name_b,
                    extern_name: extern_b,
                    args: args_b,
                    lambda_set: lambda_b,
                    ret: ret_b,
                    is_toplevel: is_toplevel_b,
                }),
            ) => {
                // for functions, the name is actually important because two functions
                // with the same type could have completely different implementations!
                if name_a == name_b
                    && extern_a == extern_b
                    && is_toplevel_a == is_toplevel_b
                    && args_a.len() == args_b.len()
                    && self.is_equivalent_help(
                        self.get_type_or_pending(*lambda_a),
                        self.get_type_or_pending(*lambda_b),
                    )
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
            | (_, Function { .. })
            | (Unsized, _)
            | (_, Unsized) => false,
        }
    }

    pub fn add_named<'a>(
        &mut self,
        interner: &TLLayoutInterner<'a>,
        name: String,
        typ: RocType,
        layout: InLayout<'a>,
    ) -> TypeId {
        if let Some(existing_type_id) = self.types_by_name.get(&name) {
            let existing_type = self.get_type(*existing_type_id);

            if self.is_equivalent(existing_type, &typ) {
                *existing_type_id
            } else {
                // TODO report this gracefully!
                panic!(
                    "Duplicate name detected - {name:?} could refer to either {existing_type:?} or {typ:?}"
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
        interner: &TLLayoutInterner<'a>,
        typ: RocType,
        layout: InLayout<'a>,
    ) -> TypeId {
        for (id, existing_type) in self.types.iter().enumerate() {
            if self.is_equivalent(&typ, existing_type) {
                return TypeId(id);
            }
        }

        debug_assert_eq!(self.types.len(), self.sizes.len());
        debug_assert_eq!(self.types.len(), self.aligns.len());

        let id = TypeId(self.types.len());

        assert!(id.0 <= TypeId::MAX.0);

        let size = interner.stack_size(layout);
        let align = interner.alignment_bytes(layout);

        self.types.push(typ);
        self.sizes.push(size);
        self.aligns.push(align);

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

    pub fn target(&self) -> Target {
        self.target
    }
}

impl From<&Types> for roc_type::Types {
    fn from(types: &Types) -> Self {
        let deps = types
            .deps
            .iter()
            .map(|(k, v)| roc_type::Tuple2::T(k.0 as _, v.iter().map(|x| x.0 as _).collect()))
            .collect();
        let types_by_name = types
            .types_by_name
            .iter()
            .map(|(k, v)| roc_type::Tuple1::T(k.as_str().into(), v.0 as _))
            .collect();

        let entrypoints = types
            .entry_points()
            .iter()
            .map(|(k, v)| roc_type::Tuple1::T(k.as_str().into(), v.0 as _))
            .collect();

        roc_type::Types {
            aligns: types.aligns.as_slice().into(),
            deps,
            entrypoints,
            sizes: types.sizes.as_slice().into(),
            types: types.types.iter().map(roc_type::RocType::from).collect(),
            types_by_name,
            target: types.target.into(),
        }
    }
}

impl From<&RocType> for roc_type::RocType {
    fn from(rc: &RocType) -> Self {
        match rc {
            RocType::RocStr => roc_type::RocType::RocStr,
            RocType::Bool => roc_type::RocType::Bool,
            RocType::RocResult(ok, err) => roc_type::RocType::RocResult(ok.0 as _, err.0 as _),
            RocType::Num(num_type) => roc_type::RocType::Num(num_type.into()),
            RocType::RocList(elem) => roc_type::RocType::RocList(elem.0 as _),
            RocType::RocDict(k, v) => roc_type::RocType::RocDict(k.0 as _, v.0 as _),
            RocType::RocSet(elem) => roc_type::RocType::RocSet(elem.0 as _),
            RocType::RocBox(elem) => roc_type::RocType::RocBox(elem.0 as _),
            RocType::TagUnion(union) => roc_type::RocType::TagUnion(union.into()),
            RocType::EmptyTagUnion => roc_type::RocType::EmptyTagUnion,
            RocType::Struct { name, fields } => roc_type::RocType::Struct(roc_type::R1 {
                fields: fields.into(),
                name: name.as_str().into(),
            }),
            RocType::TagUnionPayload { name, fields } => {
                roc_type::RocType::TagUnionPayload(roc_type::R1 {
                    fields: fields.into(),
                    name: name.as_str().into(),
                })
            }
            RocType::RecursivePointer(elem) => roc_type::RocType::RecursivePointer(elem.0 as _),
            RocType::Function(RocFn {
                function_name,
                extern_name,
                args,
                lambda_set,
                ret,
                is_toplevel,
            }) => roc_type::RocType::Function(roc_type::RocFn {
                args: args.iter().map(|arg| arg.0 as _).collect(),
                function_name: function_name.as_str().into(),
                extern_name: extern_name.as_str().into(),
                ret: ret.0 as _,
                lambda_set: lambda_set.0 as _,
                is_toplevel: *is_toplevel,
            }),
            RocType::Unit => roc_type::RocType::Unit,
            RocType::Unsized => roc_type::RocType::Unsized,
        }
    }
}

impl From<&RocNum> for roc_type::RocNum {
    fn from(rn: &RocNum) -> Self {
        match rn {
            RocNum::I8 => roc_type::RocNum::I8,
            RocNum::U8 => roc_type::RocNum::U8,
            RocNum::I16 => roc_type::RocNum::I16,
            RocNum::U16 => roc_type::RocNum::U16,
            RocNum::I32 => roc_type::RocNum::I32,
            RocNum::U32 => roc_type::RocNum::U32,
            RocNum::I64 => roc_type::RocNum::I64,
            RocNum::U64 => roc_type::RocNum::U64,
            RocNum::I128 => roc_type::RocNum::I128,
            RocNum::U128 => roc_type::RocNum::U128,
            RocNum::F32 => roc_type::RocNum::F32,
            RocNum::F64 => roc_type::RocNum::F64,
            RocNum::Dec => roc_type::RocNum::Dec,
        }
    }
}

impl From<&RocTagUnion> for roc_type::RocTagUnion {
    fn from(rtu: &RocTagUnion) -> Self {
        match rtu {
            RocTagUnion::Enumeration { name, tags, size } => {
                roc_type::RocTagUnion::Enumeration(roc_type::R5 {
                    name: name.as_str().into(),
                    tags: tags.iter().map(|name| name.as_str().into()).collect(),
                    size: *size,
                })
            }
            RocTagUnion::NonRecursive {
                name,
                tags,
                discriminant_size,
                discriminant_offset,
            } => roc_type::RocTagUnion::NonRecursive(roc_type::R7 {
                name: name.as_str().into(),
                tags: tags
                    .iter()
                    .map(|(name, payload)| roc_type::R8 {
                        name: name.as_str().into(),
                        payload: payload.into(),
                    })
                    .collect(),
                discriminant_size: *discriminant_size,
                discriminant_offset: *discriminant_offset,
            }),
            RocTagUnion::Recursive {
                name,
                tags,
                discriminant_size,
                discriminant_offset,
            } => roc_type::RocTagUnion::Recursive(roc_type::R7 {
                name: name.as_str().into(),
                tags: tags
                    .iter()
                    .map(|(name, payload)| roc_type::R8 {
                        name: name.as_str().into(),
                        payload: payload.into(),
                    })
                    .collect(),
                discriminant_size: *discriminant_size,
                discriminant_offset: *discriminant_offset,
            }),
            RocTagUnion::NonNullableUnwrapped {
                name,
                tag_name,
                payload,
            } => roc_type::RocTagUnion::NonNullableUnwrapped(roc_type::R6 {
                name: name.as_str().into(),
                tag_name: tag_name.as_str().into(),
                payload: payload.0 as _,
            }),
            RocTagUnion::SingleTagStruct {
                name,
                tag_name,
                payload,
            } => roc_type::RocTagUnion::SingleTagStruct(roc_type::R14 {
                name: name.as_str().into(),
                tag_name: tag_name.as_str().into(),
                payload: payload.into(),
            }),
            RocTagUnion::NullableWrapped {
                name,
                index_of_null_tag,
                tags,
                discriminant_size,
                discriminant_offset,
            } => roc_type::RocTagUnion::NullableWrapped(roc_type::R10 {
                name: name.as_str().into(),
                index_of_null_tag: *index_of_null_tag,
                tags: tags
                    .iter()
                    .map(|(name, payload)| roc_type::R8 {
                        name: name.as_str().into(),
                        payload: payload.into(),
                    })
                    .collect(),
                discriminant_size: *discriminant_size,
                discriminant_offset: *discriminant_offset,
            }),
            RocTagUnion::NullableUnwrapped {
                name,
                null_tag,
                non_null_tag,
                non_null_payload,
                null_represents_first_tag,
            } => roc_type::RocTagUnion::NullableUnwrapped(roc_type::R9 {
                name: name.as_str().into(),
                non_null_payload: non_null_payload.0 as _,
                non_null_tag: non_null_tag.as_str().into(),
                null_tag: null_tag.as_str().into(),
                which_tag_is_null: if *null_represents_first_tag {
                    roc_type::U2::FirstTagIsNull
                } else {
                    roc_type::U2::SecondTagIsNull
                },
            }),
        }
    }
}

impl From<&RocStructFields> for roc_type::RocStructFields {
    fn from(struct_fields: &RocStructFields) -> Self {
        match struct_fields {
            RocStructFields::HasNoClosure { fields } => roc_type::RocStructFields::HasNoClosure(
                fields
                    .iter()
                    .map(|(name, id)| roc_type::R4 {
                        name: name.as_str().into(),
                        id: id.0 as _,
                    })
                    .collect(),
            ),
            RocStructFields::HasClosure { fields } => roc_type::RocStructFields::HasClosure(
                fields
                    .iter()
                    .map(|(name, id, accessors)| roc_type::R2 {
                        name: name.as_str().into(),
                        id: id.0 as _,
                        accessors: roc_type::R3 {
                            getter: accessors.getter.as_str().into(),
                        },
                    })
                    .collect(),
            ),
        }
    }
}

impl From<&RocSingleTagPayload> for roc_type::RocSingleTagPayload {
    fn from(struct_fields: &RocSingleTagPayload) -> Self {
        match struct_fields {
            RocSingleTagPayload::HasNoClosure { payload_fields } => {
                roc_type::RocSingleTagPayload::HasNoClosure(
                    payload_fields
                        .iter()
                        .map(|id| roc_type::R16 { id: id.0 as _ })
                        .collect(),
                )
            }
            RocSingleTagPayload::HasClosure { payload_getters } => {
                roc_type::RocSingleTagPayload::HasClosure(
                    payload_getters
                        .iter()
                        .map(|(id, name)| roc_type::R4 {
                            id: id.0 as _,
                            name: name.as_str().into(),
                        })
                        .collect(),
                )
            }
        }
    }
}

impl From<&Option<TypeId>> for roc_type::U1 {
    fn from(opt: &Option<TypeId>) -> Self {
        match opt {
            Some(x) => roc_type::U1::Some(x.0 as _),
            None => roc_type::U1::None,
        }
    }
}

impl From<Target> for roc_type::Target {
    fn from(target: Target) -> Self {
        roc_type::Target {
            architecture: target.architecture().into(),
            operating_system: target.operating_system().into(),
        }
    }
}

impl From<Architecture> for roc_type::Architecture {
    fn from(arch: Architecture) -> Self {
        match arch {
            Architecture::Aarch32 => roc_type::Architecture::Aarch32,
            Architecture::Aarch64 => roc_type::Architecture::Aarch64,
            Architecture::Wasm32 => roc_type::Architecture::Wasm32,
            Architecture::X86_32 => roc_type::Architecture::X86x32,
            Architecture::X86_64 => roc_type::Architecture::X86x64,
        }
    }
}

impl From<OperatingSystem> for roc_type::OperatingSystem {
    fn from(os: OperatingSystem) -> Self {
        // TODO: Update Glue to new OS Tags.
        match os {
            OperatingSystem::Windows => roc_type::OperatingSystem::Windows,
            OperatingSystem::Linux => roc_type::OperatingSystem::Linux,
            OperatingSystem::Mac => roc_type::OperatingSystem::Mac,
            OperatingSystem::Freestanding => roc_type::OperatingSystem::Freestanding,
        }
    }
}

enum RocTypeOrPending<'a> {
    Type(&'a RocType),
    /// A pending recursive pointer
    Pending,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Accessors {
    // The name of the extern
    pub getter: String,
    // TODO setter
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RocStructFields {
    HasNoClosure {
        fields: Vec<(String, TypeId)>,
    },
    HasClosure {
        fields: Vec<(String, TypeId, Accessors)>,
        // no struct_size because it's not knowable if there's a closure; must call a size getter!
    },
}

impl RocStructFields {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            RocStructFields::HasNoClosure { fields } => fields.len(),
            RocStructFields::HasClosure { fields } => fields.len(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RocFn {
    pub function_name: String,
    pub extern_name: String,
    pub is_toplevel: bool,
    pub args: Vec<TypeId>,
    pub lambda_set: TypeId,
    pub ret: TypeId,
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
        fields: RocStructFields,
    },
    TagUnionPayload {
        name: String,
        fields: RocStructFields,
    },
    /// A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    /// this would be the field of Cons containing the (recursive) StrConsList type,
    /// and the TypeId is the TypeId of StrConsList itself.
    RecursivePointer(TypeId),
    Function(RocFn),
    /// A zero-sized type, such as an empty record or a single-tag union with no payload
    Unit,
    /// A type that has a size that is not statically known
    Unsized,
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
pub enum RocSingleTagPayload {
    /// If at least one payload field contains a closure, we have to provide
    /// field getters and setters because the size and order of those fields can vary based on the
    /// application's implementation, so those sizes and order are not knowable at host build time.
    HasClosure {
        payload_getters: Vec<(TypeId, String)>,
    },
    HasNoClosure {
        payload_fields: Vec<TypeId>,
    },
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
        discriminant_offset: u32,
        discriminant_size: u32,
    },
    /// A recursive tag union (general case)
    /// e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive {
        name: String,
        tags: Vec<(String, Option<TypeId>)>,
        discriminant_offset: u32,
        discriminant_size: u32,
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
    /// Just like a normal struct, if one of the payload fields is a closure, we have to provide
    /// field getters and setters because the size and order of those fields can vary based on the
    /// application's implementation, so those sizes and order are not knowable at host build time.
    SingleTagStruct {
        name: String,
        tag_name: String,
        payload: RocSingleTagPayload,
    },
    /// A recursive tag union that has an empty variant
    /// Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    /// It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    /// e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
    /// see also: https://youtu.be/ip92VMpf_-A?t=164
    NullableWrapped {
        name: String,
        /// Which of the tags in .tags is the null pointer.
        /// Note that this index is *not necessarily* the same as the offset of that tag
        /// at runtime, which can move around if any of the payloads contain closures!
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

struct Env<'a> {
    arena: &'a Bump,
    subs: &'a Subs,
    layout_cache: LayoutCache<'a>,
    glue_procs_by_layout: MutMap<Layout<'a>, &'a [String]>,
    lambda_set_ids: MutMap<Variable, LambdaSetId>,
    interns: &'a Interns,
    struct_names: Structs,
    enum_names: Enums,
    pending_recursive_types: VecMap<TypeId, Variable>,
    known_recursive_types: VecMap<Variable, TypeId>,
}

impl<'a> Env<'a> {
    fn new(
        arena: &'a Bump,
        subs: &'a Subs,
        interns: &'a Interns,
        layout_interner: TLLayoutInterner<'a>,
        glue_procs_by_layout: MutMap<Layout<'a>, &'a [String]>,
        target: Target,
    ) -> Self {
        Env {
            arena,
            subs,
            interns,
            struct_names: Default::default(),
            enum_names: Default::default(),
            pending_recursive_types: Default::default(),
            known_recursive_types: Default::default(),
            glue_procs_by_layout,
            lambda_set_ids: Default::default(),
            layout_cache: LayoutCache::new(layout_interner, target),
        }
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
                "The TypeId {type_id:?} was registered as a pending recursive pointer, but was not stored in Types as one."
            );

            // size and alignment shouldn't change; this is still
            // a RecursivePointer, it's just pointing to something else.
            types.replace(type_id, RocType::RecursivePointer(actual_type_id));
        }
    }

    fn find_lambda_sets(&self, root: Variable) -> MutMap<Variable, LambdaSetId> {
        roc_mono::ir::find_lambda_sets(self.arena, self.subs, root)
    }

    fn add_toplevel_type(&mut self, var: Variable, types: &mut Types) -> TypeId {
        roc_tracing::debug!(content=?roc_types::subs::SubsFmtContent(self.subs.get_content_without_compacting(var), self.subs), "adding toplevel type");

        let layout = self
            .layout_cache
            .from_var(self.arena, var, self.subs)
            .expect("Something weird ended up in the content");

        match self.subs.get_content_without_compacting(var) {
            Content::Structure(FlatType::Func(args, closure_var, ret_var, _fx_var)) => {
                // this is a toplevel type, so the closure must be empty
                let is_toplevel = true;
                add_function_type(
                    self,
                    layout,
                    types,
                    args,
                    *closure_var,
                    *ret_var,
                    is_toplevel,
                )
            }
            _ => add_type_help(self, layout, var, None, types),
        }
    }
}

fn add_function_type<'a>(
    env: &mut Env<'a>,
    layout: InLayout<'a>,
    types: &mut Types,
    args: &SubsSlice<Variable>,
    closure_var: Variable,
    ret_var: Variable,
    is_toplevel: bool,
) -> TypeId {
    let args = env.subs.get_subs_slice(*args);
    let mut arg_type_ids = Vec::with_capacity(args.len());

    let name = format!("RocFunction_{closure_var:?}");

    let extern_name = match env.lambda_set_ids.get(&closure_var) {
        Some(id) => format!("roc__main_for_host_{}_caller", id.0),
        None => {
            debug_assert!(is_toplevel);
            String::from("this_extern_should_not_be_used_this_is_a_bug")
        }
    };

    for arg_var in args {
        let arg_layout = env
            .layout_cache
            .from_var(env.arena, *arg_var, env.subs)
            .expect("Something weird ended up in the content");

        arg_type_ids.push(add_type_help(env, arg_layout, *arg_var, None, types));
    }

    let lambda_set_type_id = if is_toplevel {
        Types::UNIT
    } else {
        let lambda_set_layout = env
            .layout_cache
            .from_var(env.arena, closure_var, env.subs)
            .expect("Something weird ended up in the content");

        // TODO this treats any lambda set as unsized. We should be able to figure out whether a
        // lambda set is unsized in practice, and use the runtime representation otherwise.
        add_type_help(env, lambda_set_layout, closure_var, None, types)
    };

    let ret_type_id = {
        let ret_layout = env
            .layout_cache
            .from_var(env.arena, ret_var, env.subs)
            .expect("Something weird ended up in the content");

        add_type_help(env, ret_layout, ret_var, None, types)
    };

    let fn_type_id = add_function(env, name, types, layout, |name| {
        RocType::Function(RocFn {
            function_name: name,
            extern_name,
            args: arg_type_ids.clone(),
            lambda_set: lambda_set_type_id,
            ret: ret_type_id,
            is_toplevel,
        })
    });

    types.depends(fn_type_id, ret_type_id);

    for arg_type_id in arg_type_ids {
        types.depends(fn_type_id, arg_type_id);
    }

    fn_type_id
}

fn add_type_help<'a>(
    env: &mut Env<'a>,
    layout: InLayout<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
) -> TypeId {
    let subs = env.subs;

    match subs.get_content_without_compacting(var) {
        Content::FlexVar(_)
        | Content::RigidVar(_)
        | Content::FlexAbleVar(_, _)
        | Content::RigidAbleVar(_, _)
        | Content::Structure(FlatType::EffectfulFunc) => {
            todo!("TODO give a nice error message for a non-concrete type being passed to the host")
        }
        Content::Structure(FlatType::Tuple(..)) => {
            todo!();
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
        Content::Structure(FlatType::Apply(symbol, _)) => match env.layout_cache.get_repr(layout) {
            LayoutRepr::Builtin(builtin) => {
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
        Content::Structure(FlatType::Func(args, closure_var, ret_var, _fx_var)) => {
            let is_toplevel = false; // or in any case, we cannot assume that we are

            add_function_type(
                env,
                layout,
                types,
                args,
                *closure_var,
                *ret_var,
                is_toplevel,
            )
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
                match env.layout_cache.get_repr(layout) {
                    LayoutRepr::Builtin(builtin) => {
                        add_builtin_type(env, builtin, var, opt_name, types, layout)
                    }
                    LayoutRepr::Union(union_layout) if *name == Symbol::BOOL_BOOL => {
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
                    LayoutRepr::Union(union_layout) if *name == Symbol::RESULT_RESULT => {
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
                    LayoutRepr::Struct { .. } if *name == Symbol::RESULT_RESULT => {
                        // can happen if one or both of a and b in `Result.Result a b` are the
                        // empty tag union `[]`
                        add_type_help(env, layout, *real_var, opt_name, types)
                    }
                    LayoutRepr::Struct { .. } if *name == Symbol::DICT_DICT => {
                        let type_vars = env.subs.get_subs_slice(alias_vars.type_variables());

                        let key_var = type_vars[0];
                        let key_layout =
                            env.layout_cache.from_var(env.arena, key_var, subs).unwrap();
                        let key_id = add_type_help(env, key_layout, key_var, None, types);

                        let value_var = type_vars[1];
                        let value_layout = env
                            .layout_cache
                            .from_var(env.arena, value_var, subs)
                            .unwrap();
                        let value_id = add_type_help(env, value_layout, value_var, None, types);

                        let type_id = types.add_anonymous(
                            &env.layout_cache.interner,
                            RocType::RocDict(key_id, value_id),
                            layout,
                        );

                        types.depends(type_id, key_id);
                        types.depends(type_id, value_id);

                        type_id
                    }
                    LayoutRepr::Struct { .. } if *name == Symbol::SET_SET => {
                        let type_vars = env.subs.get_subs_slice(alias_vars.type_variables());

                        let key_var = type_vars[0];
                        let key_layout =
                            env.layout_cache.from_var(env.arena, key_var, subs).unwrap();
                        let key_id = add_type_help(env, key_layout, key_var, None, types);

                        let type_id = types.add_anonymous(
                            &env.layout_cache.interner,
                            RocType::RocSet(key_id),
                            layout,
                        );

                        types.depends(type_id, key_id);

                        type_id
                    }
                    LayoutRepr::LambdaSet(_lambda_set) if *name == Symbol::TASK_TASK => {
                        let type_vars = env.subs.get_subs_slice(alias_vars.type_variables());
                        debug_assert_eq!(type_vars.len(), 2);

                        let ok_var = type_vars[0];
                        let ok_layout = env.layout_cache.from_var(env.arena, ok_var, subs).unwrap();
                        let ok_id = add_type_help(env, ok_layout, ok_var, None, types);

                        let err_var = type_vars[1];
                        let err_layout =
                            env.layout_cache.from_var(env.arena, err_var, subs).unwrap();
                        let err_id = add_type_help(env, err_layout, err_var, None, types);

                        let type_id = types.add_anonymous(
                            &env.layout_cache.interner,
                            RocType::Unsized,
                            layout,
                        );

                        types.depends(type_id, ok_id);
                        types.depends(type_id, err_id);

                        type_id
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
        Content::ErasedLambda => todo_lambda_erasure!(),
        Content::Pure | Content::Effectful => internal_error!("fx vars should not appear here"),
        Content::LambdaSet(lambda_set) => {
            let tags = lambda_set.solved;

            if tags.is_empty() {
                // this function does not capture anything. Represent that at runtime as a unit value
                types.add_anonymous(&env.layout_cache.interner, RocType::Unsized, layout)
            } else {
                add_tag_union(env, opt_name, &tags, var, types, layout, None)
            }
        }
    }
}

fn add_builtin_type<'a>(
    env: &mut Env<'a>,
    builtin: Builtin<'a>,
    var: Variable,
    opt_name: Option<Symbol>,
    types: &mut Types,
    layout: InLayout<'a>,
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

            let elem_id = add_type_help(env, elem_layout, args[0], opt_name, types);
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
                env.layout_cache.get_repr(elem_layout),
                env.subs.get_content_without_compacting(*alias_var),
            ) {
                (
                    LayoutRepr::Struct(field_layouts),
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
                env.layout_cache.get_repr(elem_layout),
                env.subs.get_content_without_compacting(*alias_var),
            ) {
                (
                    LayoutRepr::Struct(field_layouts),
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

fn add_function<'a, F>(
    env: &mut Env<'a>,
    name: String,
    types: &mut Types,
    layout: InLayout<'a>,
    to_type: F,
) -> TypeId
where
    F: FnOnce(String) -> RocType,
{
    // let subs = env.subs;
    // let arena = env.arena;

    types.add_named(
        &env.layout_cache.interner,
        name.clone(),
        to_type(name),
        layout,
    )
}

fn add_struct<'a, I, L, F>(
    env: &mut Env<'a>,
    name: String,
    fields: I,
    types: &mut Types,
    in_layout: InLayout<'a>,
    to_type: F,
) -> TypeId
where
    I: IntoIterator<Item = (L, Variable)>,
    L: Display + Ord,
    F: FnOnce(String, RocStructFields) -> RocType,
{
    let subs = env.subs;
    let arena = env.arena;
    let fields_iter = &mut fields.into_iter();
    let mut sortables =
        bumpalo::collections::Vec::with_capacity_in(fields_iter.size_hint().0, arena);

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
            *layout1,
            label2,
            *layout2,
        )
    });

    // This layout should have an entry in glue_procs_by_layout iff it
    // contains closures, but we'll double-check that with a debug_assert.
    let layout = env.layout_cache.interner.get(in_layout);
    let struct_fields = match env.glue_procs_by_layout.get(&layout) {
        Some(&glue_procs) => {
            debug_assert!(env
                .layout_cache
                .interner
                .has_varying_stack_size(in_layout, arena));

            let fields: Vec<(String, TypeId, Accessors)> = sortables
                .into_iter()
                .zip(glue_procs.iter())
                .map(|((label, field_var, field_layout), getter)| {
                    let type_id = add_type_help(env, field_layout, field_var, None, types);
                    let accessors = Accessors {
                        getter: getter.clone(),
                    };

                    (format!("{label}"), type_id, accessors)
                })
                .collect();

            RocStructFields::HasClosure { fields }
        }
        None => {
            // debug_assert!(!layout.has_varying_stack_size(&env.layout_cache.interner, arena));

            let fields: Vec<(String, TypeId)> = sortables
                .into_iter()
                .map(|(label, field_var, field_layout)| {
                    let type_id = add_type_help(env, field_layout, field_var, None, types);

                    (format!("{label}"), type_id)
                })
                .collect();

            RocStructFields::HasNoClosure { fields }
        }
    };

    types.add_named(
        &env.layout_cache.interner,
        name.clone(),
        to_type(name, struct_fields),
        in_layout,
    )
}

trait UnionTag: Label + std::fmt::Debug {
    fn union_tag_name(&self) -> String;
}

impl UnionTag for TagName {
    fn union_tag_name(&self) -> String {
        self.0.as_str().to_string()
    }
}

impl UnionTag for Symbol {
    fn union_tag_name(&self) -> String {
        format!("C{:?}_{}", self.module_id(), self.ident_id().index())
    }
}

fn tag_union_type_from_layout<'a>(
    env: &mut Env<'a>,
    opt_name: Option<Symbol>,
    name: String,
    union_tags: &UnionLabels<impl UnionTag>,
    var: Variable,
    types: &mut Types,
    layout: InLayout<'a>,
) -> RocTagUnion {
    let subs = env.subs;

    match env.layout_cache.get_repr(layout) {
        _ if union_tags.is_newtype_wrapper(subs)
            && matches!(
                subs.get_content_without_compacting(var),
                // Make sure this is a tag union, *not* a recursive tag union!
                // Otherwise, we could end up with a recursive tag union
                // getting unwrapped incorrectly.
                Content::Structure(FlatType::TagUnion(_, _))
            ) =>
        {
            // A newtype wrapper should always have the same layout as its payload.
            let payload_layout = layout;
            let (tag_name, payload) =
                single_tag_payload_fields(env, union_tags, subs, layout, &[payload_layout], types);

            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name,
                payload,
            }
        }
        LayoutRepr::Union(union_layout) => {
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
                        .tag_id_offset(&env.layout_cache.interner)
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
                        .tag_id_offset(&env.layout_cache.interner)
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
                        tag_name,
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
                        .tag_id_offset(&env.layout_cache.interner)
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
        LayoutRepr::Builtin(Builtin::Int(int_width)) => {
            add_int_enumeration(union_tags, subs, &name, int_width)
        }
        LayoutRepr::Struct(field_layouts) => {
            let (tag_name, payload) =
                single_tag_payload_fields(env, union_tags, subs, layout, field_layouts, types);

            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name,
                payload,
            }
        }
        LayoutRepr::Builtin(Builtin::Bool) => {
            // This isn't actually a Bool, but rather a 2-tag union with no payloads
            // (so it has the same layout as a Bool, but actually isn't one; if it were
            // a real Bool, it would have been handled elsewhere already!)
            add_int_enumeration(union_tags, subs, &name, IntWidth::U8)
        }
        LayoutRepr::Builtin(builtin) => {
            let type_id = add_builtin_type(env, builtin, var, opt_name, types, layout);
            let (tag_name, _) = single_tag_payload(union_tags, subs);

            RocTagUnion::SingleTagStruct {
                name: name.clone(),
                tag_name,
                payload: RocSingleTagPayload::HasNoClosure {
                    // Builtins have no closures
                    payload_fields: vec![type_id],
                },
            }
        }
        LayoutRepr::Ptr(_) => unreachable!("Ptr values are never publicly exposed"),
        LayoutRepr::LambdaSet(lambda_set) => tag_union_type_from_layout(
            env,
            opt_name,
            name,
            union_tags,
            var,
            types,
            lambda_set.runtime_representation(),
        ),
        LayoutRepr::RecursivePointer(_) => {
            // A single-tag union which only wraps itself is erroneous and should have
            // been turned into an error earlier in the process.
            unreachable!();
        }
        LayoutRepr::FunctionPointer(_) => todo_lambda_erasure!(),
        LayoutRepr::Erased(_) => todo_lambda_erasure!(),
    }
}

fn add_tag_union<'a>(
    env: &mut Env<'a>,
    opt_name: Option<Symbol>,
    union_tags: &UnionLabels<impl UnionTag>,
    var: Variable,
    types: &mut Types,
    layout: InLayout<'a>,
    rec_root: Option<Variable>,
) -> TypeId {
    let name = match opt_name {
        Some(sym) => sym.as_str(env.interns).to_string(),
        None => env.enum_names.get_name(var),
    };

    let tag_union_type = tag_union_type_from_layout(
        env,
        opt_name,
        name.to_string(),
        union_tags,
        var,
        types,
        layout,
    );

    let typ = RocType::TagUnion(tag_union_type);
    let type_id = types.add_named(&env.layout_cache.interner, name, typ, layout);

    if let Some(rec_var) = rec_root {
        env.known_recursive_types.insert(rec_var, type_id);
    }

    type_id
}

fn add_int_enumeration(
    union_tags: &UnionLabels<impl UnionTag>,
    subs: &Subs,
    name: &str,
    int_width: IntWidth,
) -> RocTagUnion {
    let tags: Vec<String> = union_tags
        .iter_from_subs(subs)
        .map(|(tag_name, _)| tag_name.union_tag_name())
        .collect();
    RocTagUnion::Enumeration {
        name: name.to_string(),
        tags,
        size: int_width.stack_size(),
    }
}

fn union_tags_to_types<'a>(
    name: &str,
    union_tags: &UnionLabels<impl UnionTag>,
    subs: &Subs,
    env: &mut Env<'a>,
    types: &mut Types,
    layout: InLayout<'a>,
    is_recursive: bool,
) -> Vec<(String, Option<TypeId>)> {
    let mut tags: Vec<(String, Vec<Variable>)> = union_tags
        .iter_from_subs(subs)
        .map(|(tag_name, payload_vars)| {
            let name_str = tag_name.union_tag_name();

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
    union_tags: &'a UnionLabels<impl UnionTag>,
    subs: &'a Subs,
) -> (String, &'a [Variable]) {
    let mut iter = union_tags.iter_from_subs(subs);
    let (tag_name, payload_vars) = iter.next().unwrap();

    // This should be a single-tag union, but it could be the remnant of a `Result.Result a []`,
    // where the `Err` branch is inconsequential, but still part of the type

    (tag_name.union_tag_name(), payload_vars)
}

fn single_tag_payload_fields<'a, 'b>(
    env: &mut Env<'a>,
    union_tags: &'b UnionLabels<impl UnionTag>,
    subs: &'b Subs,
    in_layout: InLayout<'a>,
    field_layouts: &[InLayout<'a>],
    types: &mut Types,
) -> (String, RocSingleTagPayload) {
    let layout = env.layout_cache.interner.get(in_layout);

    if std::env::var("SINGLE_TAG_GLUE_CHECK_OFF").as_deref() != Ok("1") {
        // There should be a glue_procs_by_layout entry iff this layout has a closure in it,
        // so we shouldn't need to separately check that. Howeevr, we still do a debug_assert
        // anyway just so we have some warning in case that relationship somehow didn't hold!
        debug_assert_eq!(
            env.glue_procs_by_layout.get(&layout).is_some(),
            env.layout_cache
                .interner
                .has_varying_stack_size(in_layout, env.arena),
            "glue_procs_by_layout for {:?} was {:?}, but the layout cache said its has_varying_stack_size was {}",
                &layout,
                env.glue_procs_by_layout.get(&layout),
                env.layout_cache
                    .interner
                    .has_varying_stack_size(in_layout, env.arena)
        );
    }

    let (tag_name, payload_vars) = single_tag_payload(union_tags, subs);

    let payload = match env.glue_procs_by_layout.get(&layout) {
        Some(glue_procs) => {
            let payload_getters = payload_vars
                .iter()
                .zip(field_layouts.iter())
                .zip(glue_procs.iter())
                .map(|((field_var, field_layout), getter_name)| {
                    let type_id = add_type_help(env, *field_layout, *field_var, None, types);

                    (type_id, getter_name.to_string())
                })
                .collect();

            RocSingleTagPayload::HasClosure { payload_getters }
        }
        None => RocSingleTagPayload::HasNoClosure {
            payload_fields: payload_vars
                .iter()
                .zip(field_layouts.iter())
                .map(|(field_var, field_layout)| {
                    add_type_help(env, *field_layout, *field_var, None, types)
                })
                .collect(),
        },
    };

    (tag_name, payload)
}

fn tag_to_type<'a, D: Display>(
    name: &str,
    env: &mut Env<'a>,
    tag_name: D,
    payload_vars: &[Variable],
    types: &mut Types,
    layout: InLayout<'a>,
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
            let payload_var = payload_vars.first().unwrap();
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

        if env.layout_cache.get_repr(layout).is_dropped_because_empty() {
            count
        } else {
            count + 1
        }
    })
}

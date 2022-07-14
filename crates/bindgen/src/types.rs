use crate::enums::Enums;
use crate::structs::Structs;
use bumpalo::Bump;
use roc_builtins::bitcode::{
    FloatWidth::*,
    IntWidth::{self, *},
};
use roc_collections::VecMap;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::{
    cmp_fields, ext_var_is_empty_tag_union, round_up_to_alignment, Builtin, Layout, LayoutCache,
    UnionLayout,
};
use roc_target::TargetInfo;
use roc_types::{
    subs::{Content, FlatType, GetSubsSlice, Subs, UnionTags, Variable},
    types::RecordField,
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
            sizes: Vec::new(),
            aligns: Vec::new(),
            deps: VecMap::with_capacity(cap),
        }
    }

    pub fn add(&mut self, typ: RocType, layout: Layout<'_>) -> TypeId {
        let id = TypeId(self.types.len());

        assert!(id.0 <= TypeId::MAX.0);

        self.types.push(typ);
        self.sizes
            .push(layout.stack_size_without_alignment(self.target));
        self.aligns.push(layout.alignment_bytes(self.target));

        id
    }

    pub fn depends(&mut self, id: TypeId, depends_on: TypeId) {
        self.deps.get_or_insert(id, Vec::new).push(depends_on);
    }

    pub fn get_type(&self, id: TypeId) -> &RocType {
        match self.types.get(id.0) {
            Some(typ) => typ,
            None => unreachable!(),
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
    Function(Vec<TypeId>, TypeId),
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
    },
    /// A non-recursive tag union
    /// e.g. `Result a e : [Ok a, Err e]`
    NonRecursive {
        name: String,
        tags: Vec<(String, Option<TypeId>)>,
        discriminant_type: RocNum,
    },
    /// A recursive tag union (general case)
    /// e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive {
        name: String,
        tags: Vec<(String, Option<TypeId>)>,
        discriminant_type: RocNum,
    },
    /// A recursive tag union with just one constructor
    /// Optimization: No need to store a tag ID (the payload is "unwrapped")
    /// e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
    NonNullableUnwrapped {
        name: String,
        content: TypeId,
    },

    /// A recursive tag union that has an empty variant
    /// Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    /// It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    /// e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
    /// see also: https://youtu.be/ip92VMpf_-A?t=164
    NullableWrapped {
        name: String,
        null_tag: String,
        non_null_tags: Vec<(u16, String, Option<TypeId>)>,
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
    pending_recursive_types: VecMap<TypeId, Layout<'a>>,
    known_recursive_types: VecMap<Layout<'a>, TypeId>,
    target: TargetInfo,
}

impl<'a> Env<'a> {
    pub fn new(
        arena: &'a Bump,
        subs: &'a Subs,
        interns: &'a mut Interns,
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
            layout_cache: LayoutCache::new(target),
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
        let layout = self
            .layout_cache
            .from_var(self.arena, var, self.subs)
            .expect("Something weird ended up in the content");

        add_type_help(self, layout, var, None, types)
    }

    fn resolve_pending_recursive_types(&mut self, types: &mut Types) {
        // TODO if VecMap gets a drain() method, use that instead of doing take() and into_iter
        let pending = core::mem::take(&mut self.pending_recursive_types);

        for (type_id, layout) in pending.into_iter() {
            let actual_type_id = self.known_recursive_types.get(&layout).unwrap_or_else(|| {
                unreachable!(
                    "There was no known recursive TypeId for the pending recursive type {:?}",
                    layout
                );
            });

            debug_assert!(
                matches!(types.get_type(type_id), RocType::RecursivePointer(TypeId::PENDING)),
                "The TypeId {:?} was registered as a pending recursive pointer, but was not stored in Types as one.",
                type_id
            );

            // size and alignment shouldn't change; this is still
            // a RecursivePointer, it's just pointing to something else.
            types.replace(type_id, RocType::RecursivePointer(*actual_type_id));
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
                        RecordField::Required(field_var) | RecordField::Demanded(field_var) => {
                            Some((label.to_string(), field_var))
                        }
                        RecordField::Optional(_) => {
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

            add_tag_union(env, opt_name, tags, var, types, layout)
        }
        Content::Structure(FlatType::RecursiveTagUnion(_rec_var, tag_vars, ext_var)) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, *ext_var));

            add_tag_union(env, opt_name, tag_vars, var, types, layout)
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
        Content::Structure(FlatType::Func(args, _closure_var, ret_var)) => {
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

            let fn_type_id =
                types.add(RocType::Function(arg_type_ids.clone(), ret_type_id), layout);

            types.depends(fn_type_id, ret_type_id);

            for arg_type_id in arg_type_ids {
                types.depends(fn_type_id, arg_type_id);
            }

            fn_type_id
        }
        Content::Structure(FlatType::FunctionOrTagUnion(_, _, _)) => {
            todo!()
        }
        Content::Structure(FlatType::Erroneous(_)) => todo!(),
        Content::Structure(FlatType::EmptyRecord) => types.add(RocType::Unit, layout),
        Content::Structure(FlatType::EmptyTagUnion) => types.add(RocType::EmptyTagUnion, layout),
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

                        types.add(RocType::Bool, layout)
                    }
                    Layout::Union(union_layout) if *name == Symbol::RESULT_RESULT => {
                        match union_layout {
                            UnionLayout::NonRecursive(tag_layouts) => {
                                // Result should always have exactly two tags: Ok and Err
                                debug_assert_eq!(tag_layouts.len(), 2);

                                // Both tags should have exactly 1 payload
                                debug_assert_eq!(tag_layouts[0].len(), 1);
                                debug_assert_eq!(tag_layouts[1].len(), 1);

                                let type_vars =
                                    env.subs.get_subs_slice(alias_vars.type_variables());

                                let ok_layout = tag_layouts[0][0];
                                let ok_var = type_vars[0];
                                let ok_id = add_type_help(env, ok_layout, ok_var, None, types);

                                let err_layout = tag_layouts[1][0];
                                let err_var = type_vars[1];
                                let err_id = add_type_help(env, err_layout, err_var, None, types);

                                let type_id = types.add(RocType::RocResult(ok_id, err_id), layout);

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
            let type_id = types.add(RocType::RecursivePointer(TypeId::PENDING), layout);
            let structure_layout = env
                .layout_cache
                .from_var(env.arena, *structure, subs)
                .unwrap();

            env.pending_recursive_types
                .insert(type_id, structure_layout);

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
    layout: Layout<'_>,
) -> TypeId {
    use Content::*;
    use FlatType::*;

    let builtin_type = env.subs.get_content_without_compacting(var);

    match (builtin, builtin_type) {
        (Builtin::Int(width), _) => match width {
            U8 => types.add(RocType::Num(RocNum::U8), layout),
            U16 => types.add(RocType::Num(RocNum::U16), layout),
            U32 => types.add(RocType::Num(RocNum::U32), layout),
            U64 => types.add(RocType::Num(RocNum::U64), layout),
            U128 => types.add(RocType::Num(RocNum::U128), layout),
            I8 => types.add(RocType::Num(RocNum::I8), layout),
            I16 => types.add(RocType::Num(RocNum::I16), layout),
            I32 => types.add(RocType::Num(RocNum::I32), layout),
            I64 => types.add(RocType::Num(RocNum::I64), layout),
            I128 => types.add(RocType::Num(RocNum::I128), layout),
        },
        (Builtin::Float(width), _) => match width {
            F32 => types.add(RocType::Num(RocNum::F32), layout),
            F64 => types.add(RocType::Num(RocNum::F64), layout),
            F128 => types.add(RocType::Num(RocNum::F128), layout),
        },
        (Builtin::Decimal, _) => types.add(RocType::Num(RocNum::Dec), layout),
        (Builtin::Bool, _) => types.add(RocType::Bool, layout),
        (Builtin::Str, _) => types.add(RocType::RocStr, layout),
        (Builtin::Dict(key_layout, val_layout), Structure(Apply(Symbol::DICT_DICT, args))) => {
            let args = env.subs.get_subs_slice(*args);
            debug_assert_eq!(args.len(), 2);

            let key_id = add_type_help(env, *key_layout, args[0], opt_name, types);
            let val_id = add_type_help(env, *val_layout, args[1], opt_name, types);
            let dict_id = types.add(RocType::RocDict(key_id, val_id), layout);

            types.depends(dict_id, key_id);
            types.depends(dict_id, val_id);

            dict_id
        }
        (Builtin::Set(elem_layout), Structure(Apply(Symbol::SET_SET, args))) => {
            let args = env.subs.get_subs_slice(*args);
            debug_assert_eq!(args.len(), 1);

            let elem_id = add_type_help(env, *elem_layout, args[0], opt_name, types);
            let set_id = types.add(RocType::RocSet(elem_id), layout);

            types.depends(set_id, elem_id);

            set_id
        }
        (Builtin::List(elem_layout), Structure(Apply(Symbol::LIST_LIST, args))) => {
            let args = env.subs.get_subs_slice(*args);
            debug_assert_eq!(args.len(), 1);

            let elem_id = add_type_help(env, *elem_layout, args[0], opt_name, types);
            let list_id = types.add(RocType::RocList(elem_id), layout);

            types.depends(list_id, elem_id);

            list_id
        }
        (layout, typ) => todo!("Handle builtin layout {:?} and type {:?}", layout, typ),
    }
}

fn add_struct<I, L, F>(
    env: &mut Env<'_>,
    name: String,
    fields: I,
    types: &mut Types,
    layout: Layout<'_>,
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

    types.add(to_type(name, fields), layout)
}

fn add_tag_union<'a>(
    env: &mut Env<'a>,
    opt_name: Option<Symbol>,
    union_tags: &UnionTags,
    var: Variable,
    types: &mut Types,
    layout: Layout<'a>,
) -> TypeId {
    let subs = env.subs;
    let mut tags: Vec<(String, Vec<Variable>)> = union_tags
        .iter_from_subs(subs)
        .map(|(tag_name, payload_vars)| {
            let name_str = tag_name.0.as_str().to_string();

            (name_str, payload_vars.to_vec())
        })
        .collect();

    let name = match opt_name {
        Some(sym) => sym.as_str(env.interns).to_string(),
        None => env.enum_names.get_name(var),
    };

    // Sort tags alphabetically by tag name
    tags.sort_by(|(name1, _), (name2, _)| name1.cmp(name2));

    let is_recursive = is_recursive_tag_union(layout);

    let mut tags: Vec<_> = tags
        .into_iter()
        .map(|(tag_name, payload_vars)| {
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
                    let struct_name = format!("{}_{}", name, tag_name); // e.g. "MyUnion_MyVariant"
                    let fields = payload_vars.iter().copied().enumerate();
                    let struct_id =
                        add_struct(env, struct_name, fields, types, layout, |name, fields| {
                            RocType::TagUnionPayload { name, fields }
                        });

                    (tag_name, Some(struct_id))
                }
            }
        })
        .collect();

    let typ = match layout {
        Layout::Union(union_layout) => {
            use UnionLayout::*;

            match union_layout {
                // A non-recursive tag union
                // e.g. `Result ok err : [Ok ok, Err err]`
                NonRecursive(_) => {
                    let discriminant_type = UnionLayout::discriminant_size(tags.len()).into();

                    RocType::TagUnion(RocTagUnion::NonRecursive {
                        name,
                        tags,
                        discriminant_type,
                    })
                }
                // A recursive tag union (general case)
                // e.g. `Expr : [Sym Str, Add Expr Expr]`
                Recursive(_) => {
                    let discriminant_type = UnionLayout::discriminant_size(tags.len()).into();

                    RocType::TagUnion(RocTagUnion::Recursive {
                        name,
                        tags,
                        discriminant_type,
                    })
                }
                // A recursive tag union with just one constructor
                // Optimization: No need to store a tag ID (the payload is "unwrapped")
                // e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
                NonNullableUnwrapped(_) => {
                    todo!()
                }
                // A recursive tag union that has an empty variant
                // Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
                // It has more than one other variant, so they need tag IDs (payloads are "wrapped")
                // e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
                // see also: https://youtu.be/ip92VMpf_-A?t=164
                NullableWrapped { .. } => {
                    todo!()
                }
                // A recursive tag union with only two variants, where one is empty.
                // Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
                // e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
                NullableUnwrapped {
                    nullable_id: null_represents_first_tag,
                    other_fields: _, // TODO use this!
                } => {
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

                    RocType::TagUnion(RocTagUnion::NullableUnwrapped {
                        name,
                        null_tag,
                        non_null_tag,
                        non_null_payload: non_null_payload.unwrap(),
                        null_represents_first_tag,
                    })
                }
            }
        }
        Layout::Builtin(Builtin::Int(_)) => RocType::TagUnion(RocTagUnion::Enumeration {
            name,
            tags: tags.into_iter().map(|(tag_name, _)| tag_name).collect(),
        }),
        Layout::Builtin(_)
        | Layout::Struct { .. }
        | Layout::Boxed(_)
        | Layout::LambdaSet(_)
        | Layout::RecursivePointer => {
            // These must be single-tag unions. Bindgen ordinary nonrecursive
            // tag unions for them, and let Rust do the unwrapping.
            //
            // This should be a very rare use case, and it's not worth overcomplicating
            // the rest of bindgen to make it do something different.
            RocType::TagUnion(RocTagUnion::NonRecursive {
                name,
                tags,
                discriminant_type: RocNum::U8,
            })
        }
    };

    let type_id = types.add(typ, layout);

    if is_recursive {
        env.known_recursive_types.insert(layout, type_id);
    }

    type_id
}

fn is_recursive_tag_union(layout: Layout) -> bool {
    use roc_mono::layout::UnionLayout::*;

    match layout {
        Layout::Union(tag_union) => match tag_union {
            NonRecursive(_) => false,
            Recursive(_)
            | NonNullableUnwrapped(_)
            | NullableWrapped { .. }
            | NullableUnwrapped { .. } => true,
        },
        _ => false,
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

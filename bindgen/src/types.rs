use core::mem::align_of;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::VecMap;
use roc_mono::layout::UnionLayout;
use roc_std::RocDec;
use roc_target::TargetInfo;
use std::convert::TryInto;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(usize);

impl TypeId {
    /// Used when making recursive pointers, which need to temporarily
    /// have *some* TypeId value until we later in the process determine
    /// their real TypeId and can go back and fix them up.
    pub(crate) const PENDING: Self = Self(usize::MAX);
}

#[derive(Default, Debug, Clone)]
pub struct Types {
    by_id: Vec<RocType>,

    /// Dependencies - that is, which type depends on which other type.
    /// This is important for declaration order in C; we need to output a
    /// type declaration earlier in the file than where it gets referenced by another type.
    deps: VecMap<TypeId, Vec<TypeId>>,
}

impl Types {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            by_id: Vec::with_capacity(cap),
            deps: VecMap::with_capacity(cap),
        }
    }

    pub fn add(&mut self, typ: RocType) -> TypeId {
        let id = TypeId(self.by_id.len());

        self.by_id.push(typ);

        id
    }

    pub fn depends(&mut self, id: TypeId, depends_on: TypeId) {
        self.deps.get_or_insert(id, Vec::new).push(depends_on);
    }

    pub fn get(&self, id: TypeId) -> &RocType {
        match self.by_id.get(id.0) {
            Some(typ) => typ,
            None => unreachable!(),
        }
    }

    pub fn replace(&mut self, id: TypeId, typ: RocType) {
        debug_assert!(self.by_id.get(id.0).is_some());

        self.by_id[id.0] = typ;
    }

    pub fn ids(&self) -> impl ExactSizeIterator<Item = TypeId> {
        (0..self.by_id.len()).map(TypeId)
    }

    pub fn sorted_ids(&self) -> Vec<TypeId> {
        use roc_collections::{ReferenceMatrix, TopologicalSort};

        let mut matrix = ReferenceMatrix::new(self.by_id.len());

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

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &RocType> {
        TypesIter {
            types: self.by_id.as_slice(),
            len: self.by_id.len(),
        }
    }
}

struct TypesIter<'a> {
    types: &'a [RocType],
    len: usize,
}

impl<'a> ExactSizeIterator for TypesIter<'a> {
    fn len(&self) -> usize {
        self.len
    }
}

impl<'a> Iterator for TypesIter<'a> {
    type Item = &'a RocType;

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.len;
        let answer = self.types.get(self.types.len() - len);

        self.len = len.saturating_sub(1);

        answer
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RocType {
    RocStr,
    Bool,
    Num(RocNum),
    RocList(TypeId),
    RocDict(TypeId, TypeId),
    RocSet(TypeId),
    RocBox(TypeId),
    TagUnion(RocTagUnion),
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    fn size(&self) -> usize {
        use core::mem::size_of;
        use RocNum::*;

        match self {
            I8 => size_of::<i8>(),
            U8 => size_of::<u8>(),
            I16 => size_of::<i16>(),
            U16 => size_of::<u16>(),
            I32 => size_of::<i32>(),
            U32 => size_of::<u32>(),
            I64 => size_of::<i64>(),
            U64 => size_of::<u64>(),
            I128 => size_of::<roc_std::I128>(),
            U128 => size_of::<roc_std::U128>(),
            F32 => size_of::<f32>(),
            F64 => size_of::<f64>(),
            F128 => todo!(),
            Dec => size_of::<roc_std::RocDec>(),
        }
    }

    fn alignment(&self, target_info: TargetInfo) -> usize {
        use RocNum::*;

        match self {
            I8 => IntWidth::I8.alignment_bytes(target_info) as usize,
            U8 => IntWidth::U8.alignment_bytes(target_info) as usize,
            I16 => IntWidth::I16.alignment_bytes(target_info) as usize,
            U16 => IntWidth::U16.alignment_bytes(target_info) as usize,
            I32 => IntWidth::I32.alignment_bytes(target_info) as usize,
            U32 => IntWidth::U32.alignment_bytes(target_info) as usize,
            I64 => IntWidth::I64.alignment_bytes(target_info) as usize,
            U64 => IntWidth::U64.alignment_bytes(target_info) as usize,
            I128 => IntWidth::I128.alignment_bytes(target_info) as usize,
            U128 => IntWidth::U128.alignment_bytes(target_info) as usize,
            F32 => FloatWidth::F32.alignment_bytes(target_info) as usize,
            F64 => FloatWidth::F64.alignment_bytes(target_info) as usize,
            F128 => FloatWidth::F128.alignment_bytes(target_info) as usize,
            Dec => align_of::<RocDec>(),
        }
    }
}

impl RocType {
    /// Useful when determining whether to derive Copy in a Rust type.
    pub fn has_pointer(&self, types: &Types) -> bool {
        match self {
            RocType::Bool
            | RocType::Num(_)
            | RocType::TagUnion(RocTagUnion::Enumeration { .. }) => false,
            RocType::RocStr
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_)
            | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { .. })
            | RocType::TagUnion(RocTagUnion::NullableUnwrapped { .. })
            | RocType::TagUnion(RocTagUnion::NullableWrapped { .. })
            | RocType::TagUnion(RocTagUnion::Recursive { .. })
            | RocType::RecursivePointer { .. } => true,
            RocType::TagUnion(RocTagUnion::NonRecursive { tags, .. }) => tags
                .iter()
                .any(|(_, payloads)| payloads.iter().any(|id| types.get(*id).has_pointer(types))),
            RocType::Struct { fields, .. } => fields
                .iter()
                .any(|(_, type_id)| types.get(*type_id).has_pointer(types)),
            RocType::TagUnionPayload { fields, .. } => fields
                .iter()
                .any(|(_, type_id)| types.get(*type_id).has_pointer(types)),
        }
    }

    /// Useful when determining whether to derive Eq, Ord, and Hash in a Rust type.
    pub fn has_float(&self, types: &Types) -> bool {
        self.has_float_help(types, &[])
    }

    fn has_float_help(&self, types: &Types, do_not_recurse: &[TypeId]) -> bool {
        match self {
            RocType::Num(num) => {
                use RocNum::*;

                match num {
                    F32 | F64 | F128 => true,
                    I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | I128 | U128 | Dec => false,
                }
            }
            RocType::RocStr
            | RocType::Bool
            | RocType::TagUnion(RocTagUnion::Enumeration { .. }) => false,
            RocType::RocList(id) | RocType::RocSet(id) | RocType::RocBox(id) => {
                types.get(*id).has_float_help(types, do_not_recurse)
            }
            RocType::RocDict(key_id, val_id) => {
                types.get(*key_id).has_float_help(types, do_not_recurse)
                    || types.get(*val_id).has_float_help(types, do_not_recurse)
            }
            RocType::Struct { fields, .. } => fields
                .iter()
                .any(|(_, type_id)| types.get(*type_id).has_float_help(types, do_not_recurse)),
            RocType::TagUnionPayload { fields, .. } => fields
                .iter()
                .any(|(_, type_id)| types.get(*type_id).has_float_help(types, do_not_recurse)),
            RocType::TagUnion(RocTagUnion::Recursive { tags, .. })
            | RocType::TagUnion(RocTagUnion::NonRecursive { tags, .. }) => {
                tags.iter().any(|(_, payloads)| {
                    payloads
                        .iter()
                        .any(|id| types.get(*id).has_float_help(types, do_not_recurse))
                })
            }
            RocType::TagUnion(RocTagUnion::NullableWrapped { non_null_tags, .. }) => {
                non_null_tags.iter().any(|(_, _, payloads)| {
                    payloads
                        .iter()
                        .any(|id| types.get(*id).has_float_help(types, do_not_recurse))
                })
            }
            RocType::TagUnion(RocTagUnion::NullableUnwrapped {
                non_null_payload: content,
                ..
            })
            | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { content, .. })
            | RocType::RecursivePointer(content) => {
                if do_not_recurse.contains(content) {
                    false
                } else {
                    let mut do_not_recurse: Vec<TypeId> = do_not_recurse.into();

                    do_not_recurse.push(*content);

                    types.get(*content).has_float_help(types, &do_not_recurse)
                }
            }
        }
    }

    /// Useful when determining whether to derive Default in a Rust type.
    pub fn has_enumeration(&self, types: &Types) -> bool {
        match self {
            RocType::TagUnion { .. } | RocType::RecursivePointer { .. } => true,
            RocType::RocStr | RocType::Bool | RocType::Num(_) => false,
            RocType::RocList(id) | RocType::RocSet(id) | RocType::RocBox(id) => {
                types.get(*id).has_enumeration(types)
            }
            RocType::RocDict(key_id, val_id) => {
                types.get(*key_id).has_enumeration(types)
                    || types.get(*val_id).has_enumeration(types)
            }
            RocType::Struct { fields, .. } => fields
                .iter()
                .any(|(_, type_id)| types.get(*type_id).has_enumeration(types)),
            RocType::TagUnionPayload { fields, .. } => fields
                .iter()
                .any(|(_, type_id)| types.get(*type_id).has_enumeration(types)),
        }
    }

    pub fn size(&self, types: &Types, target_info: TargetInfo) -> usize {
        use std::mem::size_of;

        match self {
            RocType::Bool => size_of::<bool>(),
            RocType::Num(num) => num.size(),
            RocType::RocStr | RocType::RocList(_) | RocType::RocDict(_, _) | RocType::RocSet(_) => {
                3 * target_info.ptr_size()
            }
            RocType::RocBox(_) => target_info.ptr_size(),
            RocType::TagUnion(tag_union) => match tag_union {
                RocTagUnion::Enumeration { tags, .. } => size_for_tag_count(tags.len()),
                RocTagUnion::NonRecursive { tags, .. } | RocTagUnion::Recursive { tags, .. } => {
                    // The "unpadded" size (without taking alignment into account)
                    // is the sum of all the sizes of the fields.
                    let size_unpadded = tags.iter().fold(0, |total, (_, opt_payload_id)| {
                        if let Some(payload_id) = opt_payload_id {
                            let payload = types.get(*payload_id);

                            total + payload.size(types, target_info)
                        } else {
                            total
                        }
                    });

                    // Round up to the next multiple of alignment, to incorporate
                    // any necessary alignment padding.
                    //
                    // e.g. if we have a record with a Str and a U8, that would be a
                    // size_unpadded of 25, because Str is three 8-byte pointers and U8 is 1 byte,
                    // but the 8-byte alignment of the pointers means we'll round 25 up to 32.
                    let discriminant_align = align_for_tag_count(tags.len(), target_info);
                    let align = self.alignment(types, target_info).max(discriminant_align);
                    let size_padded = (size_unpadded / align) * align;

                    if size_unpadded == size_padded {
                        // We don't have any alignment padding, which means we can't
                        // put the discriminant in the padding and the compiler will
                        // add extra space for it.
                        let discriminant_size = size_for_tag_count(tags.len());

                        size_padded + discriminant_size.max(align)
                    } else {
                        size_padded
                    }
                }
                RocTagUnion::NonNullableUnwrapped { .. } => todo!(),
                RocTagUnion::NullableWrapped { .. } => todo!(),
                RocTagUnion::NullableUnwrapped { .. } => todo!(),
            },
            RocType::Struct { fields, .. } => struct_size(
                fields.iter().map(|(_, type_id)| *type_id),
                types,
                target_info,
                self.alignment(types, target_info),
            ),
            RocType::TagUnionPayload { fields, .. } => struct_size(
                fields.iter().map(|(_, type_id)| *type_id),
                types,
                target_info,
                self.alignment(types, target_info),
            ),
            RocType::RecursivePointer { .. } => target_info.ptr_size(),
        }
    }

    pub fn alignment(&self, types: &Types, target_info: TargetInfo) -> usize {
        match self {
            RocType::RocStr
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_) => target_info.ptr_alignment_bytes(),
            RocType::Num(num) => num.alignment(target_info),
            RocType::Bool => align_of::<bool>(),
            RocType::TagUnion(RocTagUnion::NonRecursive { tags, .. }) => {
                // The smallest alignment this could possibly have is based on the number of tags - e.g.
                // 0 tags is an empty union (so, alignment 0), 1-255 tags has a u8 tag (so, alignment 1), etc.
                let mut align = align_for_tag_count(tags.len(), target_info);

                for (_, payloads) in tags {
                    for id in payloads {
                        align = align.max(types.get(*id).alignment(types, target_info));
                    }
                }

                align
            }
            RocType::TagUnion(RocTagUnion::Recursive { tags, .. }) => {
                // The smallest alignment this could possibly have is based on the number of tags - e.g.
                // 0 tags is an empty union (so, alignment 0), 1-255 tags has a u8 tag (so, alignment 1), etc.
                //
                // Unlike a regular tag union, a recursive one also includes a pointer.
                let ptr_align = target_info.ptr_alignment_bytes();
                let mut align = ptr_align.max(align_for_tag_count(tags.len(), target_info));

                for (_, payloads) in tags {
                    for id in payloads {
                        align = align.max(types.get(*id).alignment(types, target_info));
                    }
                }

                align
            }
            RocType::TagUnion(RocTagUnion::NullableWrapped { non_null_tags, .. }) => {
                // The smallest alignment this could possibly have is based on the number of tags - e.g.
                // 0 tags is an empty union (so, alignment 0), 1-255 tags has a u8 tag (so, alignment 1), etc.
                //
                // Unlike a regular tag union, a recursive one also includes a pointer.
                let ptr_align = target_info.ptr_alignment_bytes();
                let mut align =
                    ptr_align.max(align_for_tag_count(non_null_tags.len(), target_info));

                for (_, _, payloads) in non_null_tags {
                    for id in payloads {
                        align = align.max(types.get(*id).alignment(types, target_info));
                    }
                }

                align
            }
            RocType::Struct { fields, .. } => fields.iter().fold(0, |align, (_, field_id)| {
                align.max(types.get(*field_id).alignment(types, target_info))
            }),
            RocType::TagUnionPayload { fields, .. } => {
                fields.iter().fold(0, |align, (_, field_id)| {
                    align.max(types.get(*field_id).alignment(types, target_info))
                })
            }
            RocType::TagUnion(RocTagUnion::NullableUnwrapped {
                non_null_payload: content,
                ..
            })
            | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { content, .. }) => {
                types.get(*content).alignment(types, target_info)
            }
            RocType::TagUnion(RocTagUnion::Enumeration { tags, .. }) => {
                UnionLayout::discriminant_size(tags.len())
                    .stack_size()
                    .try_into()
                    .unwrap()
            }
            RocType::RecursivePointer { .. } => target_info.ptr_alignment_bytes(),
        }
    }
}

fn struct_size(
    fields: impl Iterator<Item = TypeId>,
    types: &Types,
    target_info: TargetInfo,
    align: usize,
) -> usize {
    // The "unpadded" size (without taking alignment into account)
    // is the sum of all the sizes of the fields.
    let size_unpadded = fields.fold(0, |total, field_id| {
        total + types.get(field_id).size(types, target_info)
    });

    // Round up to the next multiple of alignment, to incorporate
    // any necessary alignment padding.
    //
    // e.g. if we have a record with a Str and a U8, that would be a
    // size_unpadded of 25, because Str is three 8-byte pointers and U8 is 1 byte,
    // but the 8-byte alignment of the pointers means we'll round 25 up to 32.
    (size_unpadded / align) * align
}

fn size_for_tag_count(num_tags: usize) -> usize {
    if num_tags == 0 {
        // empty tag union
        0
    } else if num_tags < u8::MAX as usize {
        IntWidth::U8.stack_size() as usize
    } else if num_tags < u16::MAX as usize {
        IntWidth::U16.stack_size() as usize
    } else if num_tags < u32::MAX as usize {
        IntWidth::U32.stack_size() as usize
    } else if num_tags < u64::MAX as usize {
        IntWidth::U64.stack_size() as usize
    } else {
        panic!(
            "Too many tags. You can't have more than {} tags in a tag union!",
            u64::MAX
        );
    }
}

/// Returns the alignment of the discriminant based on the target
/// (e.g. on wasm, these are always 4)
fn align_for_tag_count(num_tags: usize, target_info: TargetInfo) -> usize {
    if num_tags == 0 {
        // empty tag union
        0
    } else if num_tags < u8::MAX as usize {
        IntWidth::U8.alignment_bytes(target_info) as usize
    } else if num_tags < u16::MAX as usize {
        IntWidth::U16.alignment_bytes(target_info) as usize
    } else if num_tags < u32::MAX as usize {
        IntWidth::U32.alignment_bytes(target_info) as usize
    } else if num_tags < u64::MAX as usize {
        IntWidth::U64.alignment_bytes(target_info) as usize
    } else {
        panic!(
            "Too many tags. You can't have more than {} tags in a tag union!",
            u64::MAX
        );
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
    },
    /// A recursive tag union (general case)
    /// e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive {
        name: String,
        tags: Vec<(String, Option<TypeId>)>,
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

impl RocTagUnion {
    /// The byte offset where the discriminant is located within the tag union's
    /// in-memory representation. So if you take a pointer to the tag union itself,
    /// and add discriminant_offset to it, you'll have a pointer to the discriminant.
    ///
    /// This is only useful when given tags from RocTagUnion::Recursive or
    /// RocTagUnion::NonRecursive - other tag types do not store their discriminants
    /// as plain numbers at a fixed offset!
    pub fn discriminant_offset(
        tags: &[(String, Option<TypeId>)],
        types: &Types,
        target_info: TargetInfo,
    ) -> usize {
        tags.iter()
            .fold(0, |max_size, (_, opt_tag_id)| match opt_tag_id {
                Some(tag_id) => {
                    let size_unpadded = match types.get(*tag_id) {
                        // For structs (that is, payloads), we actually want
                        // to get the size *before* alignment padding is taken
                        // into account, since the discriminant is
                        // stored after those bytes.
                        RocType::Struct { fields, .. } => {
                            fields.iter().fold(0, |total, (_, field_id)| {
                                total + types.get(*field_id).size(types, target_info)
                            })
                        }
                        typ => max_size.max(typ.size(types, target_info)),
                    };

                    max_size.max(size_unpadded)
                }

                None => max_size,
            })
    }
}

#[test]
fn sizes_agree_with_roc_std() {
    use std::mem::size_of;

    let target_info = TargetInfo::from(&target_lexicon::Triple::host());
    let mut types = Types::default();

    assert_eq!(
        RocType::RocStr.size(&types, target_info),
        size_of::<roc_std::RocStr>(),
    );

    assert_eq!(
        RocType::RocList(types.add(RocType::RocStr)).size(&types, target_info),
        size_of::<roc_std::RocList<()>>(),
    );

    // TODO enable this once we have RocDict in roc_std
    // assert_eq!(
    //     RocType::RocDict.size(&types, target_info),
    //     size_of::<roc_std::RocDict>(),
    // );
}

use core::mem::align_of;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::VecMap;
use roc_mono::layout::UnionLayout;
use roc_std::RocDec;
use roc_target::TargetInfo;
use std::convert::TryInto;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(usize);

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

        self.by_id.insert(id.0, typ);
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
    RocDec,
    RocList(TypeId),
    RocDict(TypeId, TypeId),
    RocSet(TypeId),
    RocBox(TypeId),
    TagUnion(RocTagUnion),
    Struct {
        name: String,
        fields: Vec<Field>,
    },
    /// Either a single-tag union or a single-field record
    TransparentWrapper {
        name: String,
        content: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Field {
    NonRecursive(String, TypeId),
    /// A recursive field, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    /// this would be the field of Cons containing the (recursive) StrConsList type,
    /// and the TypeId is the TypeId of StrConsList itself.
    Recursive(String, TypeId),
}

impl Field {
    pub fn type_id(&self) -> TypeId {
        match self {
            Field::NonRecursive(_, type_id) => *type_id,
            Field::Recursive(_, type_id) => *type_id,
        }
    }

    pub fn label(&self) -> &str {
        match self {
            Field::NonRecursive(label, _) => label,
            Field::Recursive(label, _) => label,
        }
    }
}

impl RocType {
    /// Useful when determining whether to derive Copy in a Rust type.
    pub fn has_pointer(&self, types: &Types) -> bool {
        match self {
            RocType::Bool
            | RocType::I8
            | RocType::U8
            | RocType::I16
            | RocType::U16
            | RocType::I32
            | RocType::U32
            | RocType::I64
            | RocType::U64
            | RocType::I128
            | RocType::U128
            | RocType::F32
            | RocType::F64
            | RocType::F128
            | RocType::TagUnion(RocTagUnion::Enumeration { .. })
            | RocType::RocDec => false,
            RocType::RocStr
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_)
            | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { .. })
            | RocType::TagUnion(RocTagUnion::NullableUnwrapped { .. })
            | RocType::TagUnion(RocTagUnion::NullableWrapped { .. })
            | RocType::TagUnion(RocTagUnion::Recursive { .. }) => true,
            RocType::TagUnion(RocTagUnion::NonRecursive { tags, .. }) => tags
                .iter()
                .any(|(_, payloads)| payloads.iter().any(|id| types.get(*id).has_pointer(types))),
            RocType::Struct { fields, .. } => fields.iter().any(|field| match field {
                Field::NonRecursive(_, type_id) => types.get(*type_id).has_pointer(types),
                Field::Recursive(_, _) => true,
            }),
            RocType::TransparentWrapper { content, .. } => types.get(*content).has_pointer(types),
        }
    }

    /// Useful when determining whether to derive Eq, Ord, and Hash in a Rust type.
    pub fn has_float(&self, types: &Types) -> bool {
        match self {
            RocType::F32 | RocType::F64 | RocType::F128 => true,
            RocType::RocStr
            | RocType::Bool
            | RocType::I8
            | RocType::U8
            | RocType::I16
            | RocType::U16
            | RocType::I32
            | RocType::U32
            | RocType::I64
            | RocType::U64
            | RocType::I128
            | RocType::U128
            | RocType::RocDec
            | RocType::TagUnion(RocTagUnion::Enumeration { .. }) => false,
            RocType::RocList(id) | RocType::RocSet(id) | RocType::RocBox(id) => {
                types.get(*id).has_float(types)
            }
            RocType::RocDict(key_id, val_id) => {
                types.get(*key_id).has_float(types) || types.get(*val_id).has_float(types)
            }
            RocType::Struct { fields, .. } => fields.iter().any(|field| match field {
                Field::NonRecursive(_, type_id) => types.get(*type_id).has_float(types),
                // This has a float iff there's a float somewhere else.
                // We don't want to recurse here, because that would recurse forever!
                Field::Recursive(_, _) => false,
            }),
            RocType::TagUnion(RocTagUnion::Recursive { tags, .. })
            | RocType::TagUnion(RocTagUnion::NonRecursive { tags, .. }) => tags
                .iter()
                .any(|(_, payloads)| payloads.iter().any(|id| types.get(*id).has_float(types))),
            RocType::TagUnion(RocTagUnion::NullableWrapped { non_null_tags, .. }) => non_null_tags
                .iter()
                .any(|(_, _, payloads)| payloads.iter().any(|id| types.get(*id).has_float(types))),
            RocType::TagUnion(RocTagUnion::NullableUnwrapped {
                non_null_payload: content,
                ..
            })
            | RocType::TagUnion(RocTagUnion::NonNullableUnwrapped { content, .. })
            | RocType::TransparentWrapper { content, .. } => types.get(*content).has_float(types),
        }
    }

    /// Useful when determining whether to derive Default in a Rust type.
    pub fn has_enumeration(&self, types: &Types) -> bool {
        match self {
            RocType::TagUnion { .. } => true,
            RocType::RocStr
            | RocType::Bool
            | RocType::I8
            | RocType::U8
            | RocType::I16
            | RocType::U16
            | RocType::I32
            | RocType::U32
            | RocType::I64
            | RocType::U64
            | RocType::I128
            | RocType::U128
            | RocType::F32
            | RocType::F64
            | RocType::F128
            | RocType::RocDec => false,
            RocType::RocList(id) | RocType::RocSet(id) | RocType::RocBox(id) => {
                types.get(*id).has_enumeration(types)
            }
            RocType::RocDict(key_id, val_id) => {
                types.get(*key_id).has_enumeration(types)
                    || types.get(*val_id).has_enumeration(types)
            }
            RocType::Struct { fields, .. } => fields.iter().any(|field| match field {
                Field::NonRecursive(_, type_id) => types.get(*type_id).has_enumeration(types),
                // This has an enumeration iff there's an enumeration somewhere else.
                // We don't want to recurse here, because that would recurse forever!
                Field::Recursive(_, _) => false,
            }),
            RocType::TransparentWrapper { content, .. } => {
                types.get(*content).has_enumeration(types)
            }
        }
    }

    pub fn size(&self, types: &Types, target_info: TargetInfo) -> usize {
        use std::mem::size_of;

        match self {
            RocType::Bool => size_of::<bool>(),
            RocType::I8 => size_of::<i8>(),
            RocType::U8 => size_of::<u8>(),
            RocType::I16 => size_of::<i16>(),
            RocType::U16 => size_of::<u16>(),
            RocType::I32 => size_of::<i32>(),
            RocType::U32 => size_of::<u32>(),
            RocType::I64 => size_of::<i64>(),
            RocType::U64 => size_of::<u64>(),
            RocType::I128 => size_of::<roc_std::I128>(),
            RocType::U128 => size_of::<roc_std::U128>(),
            RocType::F32 => size_of::<f32>(),
            RocType::F64 => size_of::<f64>(),
            RocType::F128 => todo!(),
            RocType::RocDec => size_of::<roc_std::RocDec>(),
            RocType::RocStr | RocType::RocList(_) | RocType::RocDict(_, _) | RocType::RocSet(_) => {
                3 * target_info.ptr_size()
            }
            RocType::RocBox(_) => target_info.ptr_size(),
            RocType::TagUnion(tag_union) => match tag_union {
                RocTagUnion::Enumeration { tags, .. } => size_for_tag_count(tags.len()),
                RocTagUnion::NonRecursive { tags, .. } => {
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
                RocTagUnion::Recursive { .. } => todo!(),
                RocTagUnion::NonNullableUnwrapped { .. } => todo!(),
                RocTagUnion::NullableWrapped { .. } => todo!(),
                RocTagUnion::NullableUnwrapped { .. } => todo!(),
            },
            RocType::Struct { fields, .. } => {
                // The "unpadded" size (without taking alignment into account)
                // is the sum of all the sizes of the fields.
                let size_unpadded = fields.iter().fold(0, |total, field| match field {
                    Field::NonRecursive(_, field_id) => {
                        total + types.get(*field_id).size(types, target_info)
                    }
                    Field::Recursive(_, _) => {
                        // The recursion var is a pointer.
                        total + target_info.ptr_size()
                    }
                });

                // Round up to the next multiple of alignment, to incorporate
                // any necessary alignment padding.
                //
                // e.g. if we have a record with a Str and a U8, that would be a
                // size_unpadded of 25, because Str is three 8-byte pointers and U8 is 1 byte,
                // but the 8-byte alignment of the pointers means we'll round 25 up to 32.
                let align = self.alignment(types, target_info);

                (size_unpadded / align) * align
            }
            RocType::TransparentWrapper { content, .. } => {
                types.get(*content).size(types, target_info)
            }
        }
    }

    pub fn alignment(&self, types: &Types, target_info: TargetInfo) -> usize {
        match self {
            RocType::RocStr
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_) => target_info.ptr_alignment_bytes(),
            RocType::RocDec => align_of::<RocDec>(),
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
            RocType::Struct { fields, .. } => fields.iter().fold(0, |align, field| match field {
                Field::NonRecursive(_, field_id) => {
                    align.max(types.get(*field_id).alignment(types, target_info))
                }
                Field::Recursive(_, _) => {
                    // The recursion var is a pointer.
                    align.max(target_info.ptr_alignment_bytes())
                }
            }),
            RocType::I8 => IntWidth::I8.alignment_bytes(target_info) as usize,
            RocType::U8 => IntWidth::U8.alignment_bytes(target_info) as usize,
            RocType::I16 => IntWidth::I16.alignment_bytes(target_info) as usize,
            RocType::U16 => IntWidth::U16.alignment_bytes(target_info) as usize,
            RocType::I32 => IntWidth::I32.alignment_bytes(target_info) as usize,
            RocType::U32 => IntWidth::U32.alignment_bytes(target_info) as usize,
            RocType::I64 => IntWidth::I64.alignment_bytes(target_info) as usize,
            RocType::U64 => IntWidth::U64.alignment_bytes(target_info) as usize,
            RocType::I128 => IntWidth::I128.alignment_bytes(target_info) as usize,
            RocType::U128 => IntWidth::U128.alignment_bytes(target_info) as usize,
            RocType::F32 => FloatWidth::F32.alignment_bytes(target_info) as usize,
            RocType::F64 => FloatWidth::F64.alignment_bytes(target_info) as usize,
            RocType::F128 => FloatWidth::F128.alignment_bytes(target_info) as usize,
            RocType::TransparentWrapper { content, .. }
            | RocType::TagUnion(RocTagUnion::NullableUnwrapped {
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
        }
    }
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
                            fields.iter().fold(0, |total, field| match field {
                                Field::NonRecursive(_, field_id) => {
                                    total + types.get(*field_id).size(types, target_info)
                                }
                                Field::Recursive(_, _) => {
                                    // The recursion var is a pointer.
                                    total + target_info.ptr_size()
                                }
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

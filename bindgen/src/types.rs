use core::mem::align_of;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::VecMap;
use roc_std::RocDec;
use roc_target::TargetInfo;
use ven_graph::topological_sort;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(usize);

#[derive(Default, Debug)]
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

    pub fn ids(&self) -> impl ExactSizeIterator<Item = TypeId> {
        (0..self.by_id.len()).map(TypeId)
    }

    pub fn sorted_ids(&self) -> Vec<TypeId> {
        // TODO: instead use the bitvec matrix type we use in the Roc compiler -
        // it's more efficient and also would bring us one step closer to dropping
        // the dependency on this topological_sort implementation!
        topological_sort(self.ids(), |id| match self.deps.get(id) {
            Some(dep_ids) => dep_ids.to_vec(),
            None => Vec::new(),
        })
        .unwrap_or_else(|err| {
            unreachable!("Cyclic type definitions: {:?}", err);
        })
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
    RecursiveTagUnion {
        name: String,
        tags: Vec<(String, Vec<TypeId>)>,
    },
    TagUnion {
        tag_bytes: u8,
        name: String,
        tags: Vec<(String, Vec<TypeId>)>,
    },
    Struct {
        name: String,
        fields: Vec<(String, TypeId)>,
    },
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
            | RocType::RocDec => false,
            RocType::RocStr
            | RocType::RocList(_)
            | RocType::RocDict(_, _)
            | RocType::RocSet(_)
            | RocType::RocBox(_)
            | RocType::RecursiveTagUnion { .. } => true,
            RocType::TagUnion { tags, .. } => tags
                .iter()
                .any(|(_, payloads)| payloads.iter().any(|id| types.get(*id).has_pointer(types))),
            RocType::Struct { fields, .. } => fields
                .iter()
                .any(|(_, id)| types.get(*id).has_pointer(types)),
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
            | RocType::RocDec => false,
            RocType::RocList(id) | RocType::RocSet(id) | RocType::RocBox(id) => {
                types.get(*id).has_float(types)
            }
            RocType::RocDict(key_id, val_id) => {
                types.get(*key_id).has_float(types) || types.get(*val_id).has_float(types)
            }
            RocType::RecursiveTagUnion { tags, .. } | RocType::TagUnion { tags, .. } => tags
                .iter()
                .any(|(_, payloads)| payloads.iter().any(|id| types.get(*id).has_float(types))),
            RocType::Struct { fields, .. } => {
                fields.iter().any(|(_, id)| types.get(*id).has_float(types))
            }
        }
    }

    /// Useful when determining whether to derive Default in a Rust type.
    pub fn has_tag_union(&self, types: &Types) -> bool {
        match self {
            RocType::RecursiveTagUnion { .. } | RocType::TagUnion { .. } => true,
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
                types.get(*id).has_tag_union(types)
            }
            RocType::RocDict(key_id, val_id) => {
                types.get(*key_id).has_tag_union(types) || types.get(*val_id).has_tag_union(types)
            }
            RocType::Struct { fields, .. } => fields
                .iter()
                .any(|(_, id)| types.get(*id).has_tag_union(types)),
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
            RocType::TagUnion { tags, .. } => {
                // The smallest alignment this could possibly have is based on the number of tags - e.g.
                // 0 tags is an empty union (so, alignment 0), 1-255 tags has a u8 tag (so, alignment 1), etc.
                let mut align = align_for_tag_count(tags.len());

                for (_, payloads) in tags {
                    for id in payloads {
                        align = align.max(types.get(*id).alignment(types, target_info));
                    }
                }

                align
            }
            RocType::RecursiveTagUnion { tags, .. } => {
                // The smallest alignment this could possibly have is based on the number of tags - e.g.
                // 0 tags is an empty union (so, alignment 0), 1-255 tags has a u8 tag (so, alignment 1), etc.
                //
                // Unlike a regular tag union, a recursive one also includes a pointer.
                let ptr_align = target_info.ptr_alignment_bytes();
                let mut align = ptr_align.max(align_for_tag_count(tags.len()));

                for (_, payloads) in tags {
                    for id in payloads {
                        align = align.max(types.get(*id).alignment(types, target_info));
                    }
                }

                align
            }
            RocType::Struct { fields, .. } => fields.iter().fold(0, |align, (_, id)| {
                align.max(types.get(*id).alignment(types, target_info))
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
        }
    }
}

fn align_for_tag_count(num_tags: usize) -> usize {
    if num_tags == 0 {
        // empty tag union
        0
    } else if num_tags < u8::MAX as usize {
        align_of::<u8>()
    } else if num_tags < u16::MAX as usize {
        align_of::<u16>()
    } else if num_tags < u32::MAX as usize {
        align_of::<u32>()
    } else if num_tags < u64::MAX as usize {
        align_of::<u64>()
    } else {
        panic!(
            "Too many tags. You can't have more than {} tags in a tag union!",
            u64::MAX
        );
    }
}

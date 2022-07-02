use smallvec::SmallVec;

use crate::name_cache::NamedTypeId;
use crate::util::id_bi_map::IdBiMap;

id_type! {
    pub TypeId(u32);
}

// TODO: Add slot information

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeData {
    Named { named: NamedTypeId },
    Tuple { fields: SmallVec<[TypeId; 10]> },
    Union { variants: SmallVec<[TypeId; 10]> },
    HeapCell,
    Bag { item: TypeId },
}

#[derive(Clone, Debug, Default)]
pub struct TypeCache {
    pub types: IdBiMap<TypeId, TypeData>,
}

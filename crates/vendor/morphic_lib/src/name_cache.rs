use crate::api;
use crate::util::id_bi_map::IdBiMap;

id_type! {
    pub NamedTypeId(u32);
}

id_type! {
    pub FuncId(u32);
}

id_type! {
    pub ConstId(u32);
}

id_type! {
    pub EntryPointId(u32);
}

#[derive(Clone, Debug, Default)]
pub(crate) struct NameCache {
    pub(crate) named_types: IdBiMap<NamedTypeId, (api::ModNameBuf, api::TypeNameBuf)>,
    pub(crate) funcs: IdBiMap<FuncId, (api::ModNameBuf, api::FuncNameBuf)>,
    pub(crate) consts: IdBiMap<ConstId, (api::ModNameBuf, api::ConstNameBuf)>,
    pub(crate) entry_points: IdBiMap<EntryPointId, api::EntryPointNameBuf>,
}

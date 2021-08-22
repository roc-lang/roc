use roc_collections::all::MutMap;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct SurgeryEntry {
    pub file_offset: u64,
    pub virtual_offset: u64,
    pub size: u8,
}

#[derive(Default, Serialize, Deserialize, PartialEq, Debug)]
pub struct Metadata {
    pub app_functions: Vec<String>,
    // offset followed by address.
    pub plt_addresses: MutMap<String, (u64, u64)>,
    pub surgeries: MutMap<String, Vec<SurgeryEntry>>,
    pub dynamic_symbol_indices: MutMap<String, u64>,
    pub dynamic_section_offset: Option<u64>,
    pub dynamic_lib_count: Option<u64>,
    pub shared_lib_index: Option<u64>,
    pub symbol_table_section_offset: Option<u64>,
    pub symbol_table_size: Option<u64>,
    pub dynamic_symbol_table_section_offset: Option<u64>,
}

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
    pub dynamic_section_offset: u64,
    pub dynamic_lib_count: u64,
    pub shared_lib_index: u64,
    pub symbol_table_section_offset: u64,
    pub symbol_table_size: u64,
    pub dynamic_symbol_table_section_offset: u64,
    pub load_align_constraint: u64,
    pub shift_start: u64,
    pub shift_end: u64,
    pub added_data: u64,
    pub first_load_aligned_size: u64,
    pub exec_len: u64,
}

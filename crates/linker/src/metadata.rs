use roc_collections::all::MutMap;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum VirtualOffset {
    Absolute,
    Relative(u64),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct SurgeryEntry {
    pub file_offset: u64,
    pub virtual_offset: VirtualOffset,
    pub size: u8,
}

// TODO: Reanalyze each piece of data in this struct.
// I think a number of them can be combined to reduce string duplication.
// Also I think a few of them aren't need.
// For example, I think preprocessing can deal with all shifting and remove the need for added_byte_count.
#[derive(Default, Serialize, Deserialize, PartialEq, Debug)]
pub struct Metadata {
    pub app_functions: Vec<String>,
    // offset followed by address.
    pub plt_addresses: MutMap<String, (u64, u64)>,
    pub surgeries: MutMap<String, Vec<SurgeryEntry>>,
    pub dynamic_symbol_indices: MutMap<String, u64>,
    pub roc_symbol_vaddresses: MutMap<String, u64>,
    pub exec_len: u64,
    pub load_align_constraint: u64,
    pub added_byte_count: u64,
    pub last_vaddr: u64,
    pub dynamic_section_offset: u64,
    pub dynamic_symbol_table_section_offset: u64,
    pub symbol_table_section_offset: u64,
    pub symbol_table_size: u64,
    pub macho_cmd_loc: u64,
}

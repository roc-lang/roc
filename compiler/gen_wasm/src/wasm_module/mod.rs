pub mod code_builder;
pub mod linking;
pub mod opcodes;
pub mod sections;
pub mod serialize;

pub use code_builder::{Align, CodeBuilder, LocalId, ValueType, VmSymbolState};
pub use linking::SymInfo;
pub use sections::{ConstExpr, Export, ExportType, Global, GlobalType, Signature, WasmModule};

//! Provides AST library for Roc
//!  
//! AST is used by roc_editor and (soon) roc_docs. In contrast to the compiler, 
//! these types do not keep track of a location in a file.
pub mod ast_error;
mod builtin_aliases;
mod canonicalization;
pub mod constrain;
pub mod lang;
pub mod mem_pool;
pub mod module;
pub mod parse;
pub mod solve_type;

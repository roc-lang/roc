//! Library for the Roc AST
//!  
//! Code to represent the [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
//! as used by the editor. In contrast to the compiler, the types in this AST do
//!  not keep track of the location of the matching code in the source file.
pub mod ast_error;
mod builtin_aliases;
mod canonicalization;
pub mod constrain;
pub mod lang;
pub mod mem_pool;
pub mod module;
pub mod parse;
pub mod solve_type;

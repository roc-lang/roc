use crate::symbol::Symbol;

/// Low-level operations that get translated directly into e.g. LLVM instructions.
/// These are always wrapped when exposed to end users, and can only make it
/// into an Expr when added directly by can::builtins
#[derive(Clone, Debug, PartialEq)]
pub enum LowLevel {
    ListLen { arg_from_scope: Symbol },
}

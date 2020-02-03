use crate::module::symbol::Symbol;
use crate::types::Type;
use crate::uniqueness::boolean_algebra::Bool;

pub fn attr_type(uniq: Bool, typ: Type) -> Type {
    crate::constrain::builtins::builtin_type(Symbol::ATTR_ATTR, vec![Type::Boolean(uniq), typ])
}

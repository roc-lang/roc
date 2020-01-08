use crate::subs::VarStore;
use crate::types::Type;
use crate::uniqueness::boolean_algebra::Bool;

pub fn lift(var_store: &VarStore, typ: Type) -> Type {
    let uniq_var = var_store.fresh();
    let uniq_type = Bool::Variable(uniq_var);

    attr_type(uniq_type, typ)
}

type Uniqueness = Bool;

pub fn attr_type(uniq: Uniqueness, typ: Type) -> Type {
    crate::constrain::builtins::builtin_type("Attr", "Attr", vec![Type::Boolean(uniq), typ])
}

pub fn shared_type() -> Uniqueness {
    Bool::Zero
}

/// We usually just leave a type parameter unbound (written `*`) when it's unique
#[allow(dead_code)]
pub fn unique_type() -> Uniqueness {
    Bool::One
}

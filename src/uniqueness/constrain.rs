use crate::collections::ImMap;
use crate::region::Region;
use crate::subs::{VarStore, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::Type::{self, *};
use crate::types::{self, LetConstraint, Reason};

pub fn exists(flex_vars: Vec<Variable>, constraint: Constraint) -> Constraint {
    Constraint::Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars,
        def_types: ImMap::default(),
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

pub fn lift(var_store: &VarStore, typ: Type) -> Type {
    let uniq_var = var_store.fresh();
    let uniq_type = Variable(uniq_var);

    attr_type(uniq_type, typ)
}

pub fn int_literal(var_store: &VarStore, expected: Expected<Type>, region: Region) -> Constraint {
    let typ = lift(var_store, number_literal_type("Int", "Integer"));
    let reason = Reason::IntLiteral;

    num_literal(var_store, typ, reason, expected, region)
}

#[inline(always)]
pub fn float_literal(var_store: &VarStore, expected: Expected<Type>, region: Region) -> Constraint {
    let typ = lift(var_store, number_literal_type("Float", "FloatingPoint"));
    let reason = Reason::FloatLiteral;

    num_literal(var_store, typ, reason, expected, region)
}

#[inline(always)]
fn num_literal(
    var_store: &VarStore,
    literal_type: Type,
    reason: Reason,
    expected: Expected<Type>,
    region: Region,
) -> Constraint {
    let num_var = var_store.fresh();
    let num_type = Variable(num_var);
    let expected_literal = ForReason(reason, literal_type, region);

    And(vec![
        Eq(num_type.clone(), expected_literal, region),
        Eq(num_type, expected, region),
    ])
}

#[inline(always)]
fn number_literal_type(module_name: &str, type_name: &str) -> Type {
    builtin_type(
        types::MOD_NUM,
        types::TYPE_NUM,
        vec![builtin_type(module_name, type_name, Vec::new())],
    )
}

#[inline(always)]
fn builtin_type(module_name: &str, type_name: &str, args: Vec<Type>) -> Type {
    Type::Apply {
        module_name: module_name.into(),
        name: type_name.into(),
        args,
    }
}

pub fn empty_list_type(var: Variable) -> Type {
    list_type(Type::Variable(var))
}

pub fn list_type(typ: Type) -> Type {
    builtin_type("List", "List", vec![typ])
}

pub fn str_type() -> Type {
    builtin_type("Str", "Str", Vec::new())
}

type Uniqueness = Type;

pub fn attr_type(uniq: Uniqueness, typ: Type) -> Type {
    builtin_type("Attr", "Attr", vec![uniq, typ])
}

pub fn shared_type() -> Uniqueness {
    builtin_type("Attr", "Shared", Vec::new())
}

/// We usually just leave a type parameter unbound (written `*`) when it's unique
#[allow(dead_code)]
pub fn unique_type() -> Uniqueness {
    builtin_type("Attr", "Unique", Vec::new())
}

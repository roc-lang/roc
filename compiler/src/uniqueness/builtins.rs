use crate::subs::Variable;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::Type::{self, *};
use crate::types::{LetConstraint, Reason};
use crate::uniqueness::boolean_algebra::Bool;
use roc_collections::all::SendMap;
use roc_module::symbol::Symbol;
use roc_region::all::Region;

#[inline(always)]
pub fn int_literal(num_var: Variable, expected: Expected<Type>, region: Region) -> Constraint {
    let num_type = Variable(num_var);
    let reason = Reason::IntLiteral;
    let int_type = builtin_type(Symbol::INT_INT, vec![]);
    let expected_literal = ForReason(reason, int_type, region);

    exists(
        vec![num_var],
        And(vec![
            Eq(num_type.clone(), expected_literal, region),
            Eq(num_type, expected, region),
        ]),
    )
}

#[inline(always)]
pub fn float_literal(num_var: Variable, expected: Expected<Type>, region: Region) -> Constraint {
    let num_type = Variable(num_var);
    let reason = Reason::FloatLiteral;
    let float_type = builtin_type(Symbol::FLOAT_FLOAT, vec![]);
    let expected_literal = ForReason(reason, float_type, region);

    exists(
        vec![num_var],
        And(vec![
            Eq(num_type.clone(), expected_literal, region),
            Eq(num_type, expected, region),
        ]),
    )
}

#[inline(always)]
pub fn exists(flex_vars: Vec<Variable>, constraint: Constraint) -> Constraint {
    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars,
        def_types: SendMap::default(),
        def_aliases: SendMap::default(),
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

#[inline(always)]
pub fn attr_type(uniqueness: Bool, value: Type) -> Type {
    Type::Apply(Symbol::ATTR_ATTR, vec![Type::Boolean(uniqueness), value])
}

#[inline(always)]
pub fn builtin_type(symbol: Symbol, args: Vec<Type>) -> Type {
    Type::Apply(Symbol::ATTR_ATTR, vec![Type::Apply(symbol, args)])
}

#[inline(always)]
pub fn empty_list_type(uniqueness: Bool, var: Variable) -> Type {
    list_type(uniqueness, Type::Variable(var))
}

#[inline(always)]
pub fn list_type(uniqueness: Bool, typ: Type) -> Type {
    attr_type(uniqueness, Type::Apply(Symbol::LIST_LIST, vec![typ]))
}

#[inline(always)]
pub fn str_type(uniqueness: Bool) -> Type {
    attr_type(uniqueness, Type::Apply(Symbol::STR_STR, Vec::new()))
}

use crate::collections::SendMap;
use crate::region::Region;
use crate::subs::Variable;
use crate::types::Constraint::{self, *};
use crate::types::Expected::{self, *};
use crate::types::Type::{self, *};
use crate::types::{self, LetConstraint, Reason};

#[inline(always)]
pub fn int_literal(var: Variable, expected: Expected<Type>, region: Region) -> Constraint {
    let typ = number_literal_type("Int", "Integer");
    let reason = Reason::IntLiteral;

    num_literal(var, typ, reason, expected, region)
}

#[inline(always)]
pub fn float_literal(var: Variable, expected: Expected<Type>, region: Region) -> Constraint {
    let typ = number_literal_type("Float", "FloatingPoint");
    let reason = Reason::FloatLiteral;

    num_literal(var, typ, reason, expected, region)
}

#[inline(always)]
pub fn exists(flex_vars: Vec<Variable>, constraint: Constraint) -> Constraint {
    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars,
        def_types: SendMap::default(),
        defs_constraint: constraint,
        ret_constraint: Constraint::True,
    }))
}

#[inline(always)]
fn num_literal(
    num_var: Variable,
    literal_type: Type,
    reason: Reason,
    expected: Expected<Type>,
    region: Region,
) -> Constraint {
    let num_type = Variable(num_var);
    let expected_literal = ForReason(reason, literal_type, region);

    exists(
        vec![num_var],
        And(vec![
            Eq(num_type.clone(), expected_literal, region),
            Eq(num_type, expected, region),
        ]),
    )
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
pub fn builtin_type(module_name: &str, type_name: &str, args: Vec<Type>) -> Type {
    Type::Apply {
        module_name: module_name.into(),
        name: type_name.into(),
        args,
    }
}

#[inline(always)]
pub fn empty_list_type(var: Variable) -> Type {
    list_type(Type::Variable(var))
}

#[inline(always)]
pub fn list_type(typ: Type) -> Type {
    builtin_type("List", "List", vec![typ])
}

#[inline(always)]
pub fn str_type() -> Type {
    builtin_type("Str", "Str", Vec::new())
}

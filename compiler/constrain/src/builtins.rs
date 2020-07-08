use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::LetConstraint;
use roc_can::expected::Expected::{self, *};
use roc_collections::all::SendMap;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::subs::Variable;
use roc_types::types::Category;
use roc_types::types::Reason;
use roc_types::types::Type::{self, *};

#[inline(always)]
pub fn int_literal(num_var: Variable, expected: Expected<Type>, region: Region) -> Constraint {
    let num_type = Variable(num_var);
    let reason = Reason::IntLiteral;
    let expected_literal = ForReason(reason, Type::Apply(Symbol::NUM_INT, vec![]), region);

    exists(
        vec![num_var],
        And(vec![
            Eq(num_type.clone(), expected_literal, Category::Int, region),
            Eq(num_type, expected, Category::Int, region),
        ]),
    )
}

#[inline(always)]
pub fn float_literal(num_var: Variable, expected: Expected<Type>, region: Region) -> Constraint {
    let num_type = Variable(num_var);
    let reason = Reason::FloatLiteral;
    let expected_literal = ForReason(reason, Type::Apply(Symbol::NUM_FLOAT, vec![]), region);

    exists(
        vec![num_var],
        And(vec![
            Eq(num_type.clone(), expected_literal, Category::Float, region),
            Eq(num_type, expected, Category::Float, region),
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
pub fn builtin_type(symbol: Symbol, args: Vec<Type>) -> Type {
    Type::Apply(symbol, args)
}

#[inline(always)]
pub fn empty_list_type(var: Variable) -> Type {
    list_type(Type::Variable(var))
}

#[inline(always)]
pub fn list_type(typ: Type) -> Type {
    builtin_type(Symbol::LIST_LIST, vec![typ])
}

#[inline(always)]
pub fn str_type() -> Type {
    builtin_type(Symbol::STR_STR, Vec::new())
}

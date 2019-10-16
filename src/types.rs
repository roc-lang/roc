use bumpalo::{self, Bump};
use can::symbol::Symbol;
use collections::ImMap;
use operator::{ArgSide, Operator};
use region::Located;
use region::Region;
use subs::Variable;

type ModuleName = String;

// The standard modules
pub const MOD_FLOAT: &'static str = "Float";
pub const MOD_INT: &'static str = "Int";
pub const MOD_STR: &'static str = "Str";
pub const MOD_LIST: &'static str = "List";
pub const MOD_MAP: &'static str = "Map";
pub const MOD_SET: &'static str = "Set";
pub const MOD_NUM: &'static str = "Num";
pub const MOD_DEFAULT: &'static str = "Default";

pub const TYPE_NUM: &'static str = "Num";

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type<'a> {
    EmptyRec,
    /// A function. The types of its arguments, then the type of its return value.
    Function(&'a [Type<'a>], &'a Type<'a>),
    Operator(&'a OperatorType<'a>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply {
        module_name: &'a str,
        name: &'a str,
        args: &'a [Type<'a>],
    },
    Variable(Variable),
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

impl<'a> Type<'a> {
    pub fn for_operator(arena: &'a Bump, op: Operator) -> OperatorType<'a> {
        use self::Operator::*;

        match op {
            Slash => op_type(Type::float(arena), Type::float(arena), Type::float(arena)),
            DoubleSlash => op_type(Type::int(arena), Type::int(arena), Type::int(arena)),
            // TODO actually, don't put these in types.rs - instead, replace them
            // with an equivalence to their corresponding stdlib functions - e.g.
            // Slash generates a new variable and an Eq constraint with Float.div.
            _ => panic!("TODO types for operator {:?}", op),
        }
    }

    pub fn num(args: &'a [Type<'a>]) -> Self {
        Type::Apply {
            module_name: MOD_NUM,
            name: TYPE_NUM,
            args,
        }
    }

    pub fn float(arena: &'a Bump) -> Self {
        let floating_point = Type::Apply {
            module_name: MOD_FLOAT,
            name: "FloatingPoint",
            args: &[],
        };

        Type::num(bumpalo::vec![in &arena; floating_point].into_bump_slice())
    }

    pub fn int(arena: &'a Bump) -> Self {
        let integer = Type::Apply {
            module_name: MOD_INT,
            name: "Integer",
            args: &[],
        };

        Type::num(bumpalo::vec![in &arena; integer].into_bump_slice())
    }

    pub fn string() -> Self {
        Type::Apply {
            module_name: MOD_STR,
            name: "Str",
            args: &[],
        }
    }

    /// This is needed to constrain `if` conditionals
    pub fn bool() -> Self {
        Type::Apply {
            module_name: MOD_DEFAULT,
            name: "Bool",
            args: &[],
        }
    }
}

fn op_type<'a>(left: Type<'a>, right: Type<'a>, ret: Type<'a>) -> OperatorType<'a> {
    OperatorType { left, right, ret }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct OperatorType<'a> {
    pub left: Type<'a>,
    pub right: Type<'a>,
    pub ret: Type<'a>,
}

#[derive(Debug, Clone)]
pub enum Expected<T> {
    NoExpectation(T),
    ForReason(Reason, T, Region),
}

impl<T> Expected<T> {
    pub fn get_type(self) -> T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Reason {
    AnonymousFnArg(u8 /* arg index */),
    NamedFnArg(String /* function name */, u8 /* arg index */),
    AnonymousFnCall(u8 /* arity */),
    NamedFnCall(String /* function name */, u8 /* arity */),
    OperatorArg(Operator, ArgSide),
    OperatorRet(Operator),
    FloatLiteral,
    IntLiteral,
    InterpolatedStringVar,
    ElemInList,
}

#[derive(Debug, Clone)]
pub enum Constraint<'a> {
    Eq(Type<'a>, Expected<Type<'a>>, Region),
    Lookup(Symbol<'a>, Expected<Type<'a>>, Region),
    True, // Used for things that always unify, e.g. blanks and runtime errors
    Let(&'a LetConstraint<'a>),
    And(&'a [Constraint<'a>]),
}

#[derive(Debug, Clone)]
pub struct LetConstraint<'a> {
    pub rigid_vars: &'a [Variable],
    pub flex_vars: &'a [Variable],
    pub assignment_types: ImMap<Symbol<'a>, Located<Type<'a>>>,
    pub assignments_constraint: Constraint<'a>,
    pub ret_constraint: Constraint<'a>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Problem {
    GenericMismatch,
    ExtraArguments,
    MissingArguments,
    IfConditionNotBool,
    InconsistentIfElse,
    InconsistentCaseBranches,
    CircularType,
    CanonicalizationProblem,
}

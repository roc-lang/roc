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
pub enum Type {
    EmptyRec,
    /// A function. The types of its arguments, then the type of its return value.
    Function(Vec<Type>, Box<Type>),
    Operator(Box<OperatorType>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply {
        module_name: Box<str>,
        name: Box<str>,
        args: Vec<Type>,
    },
    Variable(Variable),
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

impl Type {
    pub fn for_operator(op: Operator) -> OperatorType {
        use self::Operator::*;

        match op {
            Slash => op_type(Type::float(), Type::float(), Type::float()),
            DoubleSlash => op_type(Type::int(), Type::int(), Type::int()),
            // TODO actually, don't put these in types.rs - instead, replace them
            // with an equivalence to their corresponding stdlib functions - e.g.
            // Slash generates a new variable and an Eq constraint with Float.div.
            _ => panic!("TODO types for operator {:?}", op),
        }
    }

    pub fn num(args: Vec<Type>) -> Self {
        Type::Apply {
            module_name: MOD_NUM.into(),
            name: TYPE_NUM.into(),
            args,
        }
    }

    pub fn float() -> Self {
        let floating_point = Type::Apply {
            module_name: MOD_FLOAT.into(),
            name: "FloatingPoint".into(),
            args: Vec::new(),
        };

        Type::num(vec![floating_point])
    }

    pub fn int() -> Self {
        let integer = Type::Apply {
            module_name: MOD_INT.into(),
            name: "Integer".into(),
            args: Vec::new(),
        };

        Type::num(vec![integer])
    }

    pub fn string() -> Self {
        Type::Apply {
            module_name: MOD_STR.into(),
            name: "Str".into(),
            args: Vec::new(),
        }
    }

    /// This is needed to constrain `if` conditionals
    pub fn bool() -> Self {
        Type::Apply {
            module_name: MOD_DEFAULT.into(),
            name: "Bool".into(),
            args: Vec::new(),
        }
    }
}

fn op_type(left: Type, right: Type, ret: Type) -> OperatorType {
    OperatorType { left, right, ret }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct OperatorType {
    pub left: Type,
    pub right: Type,
    pub ret: Type,
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
pub enum Constraint {
    Eq(Type, Expected<Type>, Region),
    Lookup(Symbol, Expected<Type>, Region),
    True, // Used for things that always unify, e.g. blanks and runtime errors
    Let(Box<LetConstraint>),
    And(Vec<Constraint>),
}

#[derive(Debug, Clone)]
pub struct LetConstraint {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
    pub assignment_types: ImMap<Symbol, Located<Type>>,
    pub assignments_constraint: Constraint,
    pub ret_constraint: Constraint,
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

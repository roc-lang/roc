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
    Apply(ModuleName, String, Vec<Type>),
    Variable(Variable),
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

impl Type {
    pub fn for_operator(op: Operator) -> OperatorType {
        use self::Operator::*;

        match op {
            Slash => op_type(Type::float(), Type::float(), Type::float()),
            // TODO actually, drop this in favor of having the parser use Apply in the AST.
            _ => panic!("TODO types for operator {:?}", op),
        }
    }

    pub fn num(args: Vec<Type>) -> Self {
        Type::Apply(MOD_NUM.to_string(), TYPE_NUM.to_string(), args)
    }

    pub fn float() -> Self {
        let floating_point = Type::Apply(
            MOD_FLOAT.to_string(),
            "FloatingPoint".to_string(),
            Vec::new(),
        );

        Type::num(vec![floating_point])
    }

    pub fn int() -> Self {
        let integer = Type::Apply(MOD_INT.to_string(), "Integer".to_string(), Vec::new());

        Type::num(vec![integer])
    }

    pub fn string() -> Self {
        Type::Apply(MOD_STR.to_string(), "Str".to_string(), Vec::new())
    }

    /// This is needed to constrain `if` conditionals
    pub fn bool() -> Self {
        Type::Apply(MOD_DEFAULT.to_string(), "Bool".to_string(), Vec::new())
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

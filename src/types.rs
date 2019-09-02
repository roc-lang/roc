use canonicalize::Symbol;
use collections::ImMap;
use operator::{ArgSide, Operator};
use region::Located;
use region::Region;
use subs::Variable;

type ModuleName = String;

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
            _ => panic!("TODO types for operator {:?}", op),
        }
    }

    pub fn num(args: Vec<Type>) -> Self {
        Type::Apply("Num".to_string(), "Num".to_string(), args)
    }

    pub fn float() -> Self {
        let floating_point =
            Type::Apply("Float".to_string(), "FloatingPoint".to_string(), Vec::new());

        Type::num(vec![floating_point])
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

use subs::Variable;
use region::Region;
use operator::Operator;
use region::Located;
use canonicalize::Symbol;
use collections::MutMap;
use self::Type::*;
use self::Problem::*;
use std::cmp::min;
use std::usize;

type ModuleName = String;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    /// One of the concrete built-in types (e.g. Int)
    Builtin(Builtin),
    /// A function. The types of its arguments, then the type of its return value.
    Function(Vec<Type>, Box<Type>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(ModuleName, Box<Type>, Vec<Type>),
    Variable(Variable),
    /// A Blank in the editor (a "typed hole" as they're also known)
    Blank,
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

pub enum Expected<T> {
    NoExpectation(T),
    ForReason(Region, Reason, T),
}

impl<T> Expected<T> {
    pub fn unwrap(self) -> T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, _, val) => val
        }
    }
}

pub enum Reason {
    OperatorLeftArg(Operator),
    OperatorRightArg(Operator),
}

pub enum Constraint {
    Eq(Region, Type, Expected<Type>),
    True, // Used for things that always unify, e.g. blanks and runtime errors 
    Let(Box<LetConstraint>),
    And(Vec<Constraint>)
}

pub struct LetConstraint {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
    pub header: MutMap<Symbol, Located<Type>>,
    pub header_constraint: Constraint,
    pub body_constraint: Constraint,
}
      

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Problem {
    GenericMismatch(Box<Type>, Box<Type>),
    ExtraArguments,
    MissingArguments,
    IfConditionNotBool,
    InconsistentIfElse,
    InconsistentCaseBranches,
    CircularType,
    CanonicalizationProblem
}


/// Built-in types
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Builtin {
    Str, Int, Frac, Approx, 
    EmptyRecord,
}


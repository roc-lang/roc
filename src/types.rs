use subs::Variable;
use region::Region;
use operator::Operator;
use region::Located;
use canonicalize::Symbol;
use collections::ImMap;

type ModuleName = String;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    EmptyRec,
    /// A function. The types of its arguments, then the type of its return value.
    Function(Vec<Type>, Box<Type>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(ModuleName, String, Vec<Type>),
    Variable(Variable),
    /// A Blank in the editor (a "typed hole" as they're also known)
    Blank,
    /// A type error, which will code gen to a runtime error
    Erroneous(Problem),
}

#[derive(Debug)]
pub enum Expected<T> {
    NoExpectation(T),
    ForReason(Reason, T, Region),
}

impl<T> Expected<T> {
    pub fn get_type(self) -> T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val
        }
    }
}

#[derive(Debug)]
pub enum Reason {
    OperatorLeftArg(Operator),
    OperatorRightArg(Operator),
    FractionalLiteral,
    ElemInList,
}

#[derive(Debug)]
pub enum Constraint {
    Eq(Type, Expected<Type>, Region),
    Lookup(Symbol, Expected<Type>, Region),
    True, // Used for things that always unify, e.g. blanks and runtime errors 
    Let(Box<LetConstraint>),
    And(Vec<Constraint>)
}

#[derive(Debug)]
pub struct LetConstraint {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
    pub assignment_types: ImMap<Symbol, Located<Type>>,
    pub assignments_constraint: Constraint,
    pub ret_constraint: Constraint,
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

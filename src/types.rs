use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::can::symbol::Symbol;
use crate::collections::SendMap;
use crate::operator::{ArgSide, BinOp};
use crate::region::Located;
use crate::region::Region;
use crate::subs::Variable;
use std::fmt;

// The standard modules
pub const MOD_FLOAT: &str = "Float";
pub const MOD_BOOL: &str = "Bool";
pub const MOD_INT: &str = "Int";
pub const MOD_STR: &str = "Str";
pub const MOD_LIST: &str = "List";
pub const MOD_MAP: &str = "Map";
pub const MOD_SET: &str = "Set";
pub const MOD_NUM: &str = "Num";
pub const MOD_DEFAULT: &str = "Default";

pub const TYPE_NUM: &str = "Num";
pub const TYPE_INTEGER: &str = "Integer";
pub const TYPE_FLOATINGPOINT: &str = "FloatingPoint";

#[derive(PartialEq, Eq, Clone)]
pub enum Type {
    EmptyRec,
    /// A function. The types of its arguments, then the type of its return value.
    Function(Vec<Type>, Box<Type>),
    Record(SendMap<Lowercase, Type>, Box<Type>),
    Alias(ModuleName, Uppercase, Vec<(Lowercase, Type)>, Box<Type>),
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

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::EmptyRec => write!(f, "{{}}"),
            Type::Function(args, ret) => {
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        ", ".fmt(f)?;
                    }

                    arg.fmt(f)?;
                }

                write!(f, " -> ")?;

                ret.fmt(f)
            }
            Type::Variable(var) => write!(f, "<{:?}>", var),

            Type::Apply {
                module_name,
                name,
                args,
            } => {
                write!(f, "(")?;

                if !module_name.is_empty() {
                    write!(f, "{}.", module_name)?;
                }

                write!(f, "{}", name)?;

                for arg in args {
                    write!(f, " {:?}", arg)?;
                }

                write!(f, ")")
            }
            Type::Erroneous(problem) => {
                write!(f, "Erroneous(")?;

                problem.fmt(f)?;

                write!(f, ")")
            }
            Type::Alias(_, _, _, _) => {
                panic!("TODO fmt type aliases");
            }
            Type::Record(_, _) => {
                panic!("TODO fmt record types");
            }
        }
    }
}

impl Type {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expected<T> {
    NoExpectation(T),
    FromAnnotation(String, usize, AnnotationSource, T),
    ForReason(Reason, T, Region),
}

/// Like Expected, but for Patterns.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PExpected<T> {
    NoExpectation(T),
    ForReason(PReason, T, Region),
}

impl<T> PExpected<T> {
    pub fn get_type(self) -> T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PReason {
    TypedArg { name: Box<str>, index: usize },
    CaseMatch { index: usize },
    CtorArg { name: Box<str>, index: usize },
    ListEntry { index: usize },
    Tail,
}

impl<T> Expected<T> {
    pub fn get_type(self) -> T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationSource {
    TypedIfBranch(usize /* index */),
    TypedCaseBranch(usize /* index */),
    TypedBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    AnonymousFnArg { arg_index: u8 },
    NamedFnArg(String /* function name */, u8 /* arg index */),
    AnonymousFnCall { arity: u8 },
    NamedFnCall(String /* function name */, u8 /* arity */),
    BinOpArg(BinOp, ArgSide),
    BinOpRet(BinOp),
    FloatLiteral,
    IntLiteral,
    InterpolatedStringVar,
    CaseBranch { index: usize },
    ElemInList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Constraint {
    Eq(Type, Expected<Type>, Region),
    Lookup(Symbol, Expected<Type>, Region),
    Pattern(Region, PatternCategory, Type, PExpected<Type>),
    True, // Used for things that always unify, e.g. blanks and runtime errors
    Let(Box<LetConstraint>),
    And(Vec<Constraint>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternCategory {
    Record,
    EmptyRecord,
    List,
    Set,
    Map,
    Ctor(Box<str>),
    Int,
    Str,
    Float,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetConstraint {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
    pub def_types: SendMap<Symbol, Located<Type>>,
    pub defs_constraint: Constraint,
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

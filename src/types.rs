use crate::can::ident::{Lowercase, ModuleName, Uppercase};
use crate::can::symbol::Symbol;
use crate::collections::{MutSet, SendMap};
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
        module_name: ModuleName,
        name: Uppercase,
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
                let module_name = module_name.as_str();

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
            Type::Record(fields, ext) => {
                write!(f, "{{")?;

                if !fields.is_empty() {
                    write!(f, " ")?;
                }

                let mut any_written_yet = false;

                for (label, field_type) in fields {
                    write!(f, "{} : {:?}", label, field_type)?;

                    if any_written_yet {
                        write!(f, ", ")?;
                    } else {
                        any_written_yet = true;
                    }
                }

                if !fields.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "}}")?;

                match *ext.clone() {
                    Type::EmptyRec => {
                        // This is a closed record. We're done!
                        Ok(())
                    }
                    other => {
                        // This is an open record, so print the variable
                        // right after the '}'
                        //
                        // e.g. the "*" at the end of `{ x: Int }*`
                        // or the "r" at the end of `{ x: Int }r`
                        other.fmt(f)
                    }
                }
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

    pub fn arity(&self) -> usize {
        if let Type::Function(args, _) = self {
            args.len()
        } else {
            0
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
    CircularType(Symbol, ErrorType, Region),
    CanonicalizationProblem,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ErrorType {
    Infinite,
    Type(ModuleName, Uppercase, Vec<ErrorType>),
    FlexVar(Lowercase),
    RigidVar(Lowercase),
    Record(SendMap<Lowercase, ErrorType>, RecordExt),
    Function(Vec<ErrorType>, Box<ErrorType>),
    Alias(
        ModuleName,
        Uppercase,
        Vec<(Lowercase, ErrorType)>,
        Box<ErrorType>,
    ),
    Error,
}

impl ErrorType {
    pub fn unwrap_alias(self) -> ErrorType {
        match self {
            ErrorType::Alias(_, _, _, real) => real.unwrap_alias(),
            real => real,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum RecordExt {
    Closed,
    FlexOpen(Lowercase),
    RigidOpen(Lowercase),
}

static THE_LETTER_A: u32 = 'a' as u32;

pub fn name_type_var(letters_used: u32, taken: &mut MutSet<Lowercase>) -> (Lowercase, u32) {
    // TODO we should arena-allocate this String,
    // so all the strings in the entire pass only require ~1 allocation.
    let generated_name = if letters_used < 26 {
        // This should generate "a", then "b", etc.
        std::char::from_u32(THE_LETTER_A + letters_used)
            .unwrap_or_else(|| panic!("Tried to convert {} to a char", THE_LETTER_A + letters_used))
            .to_string()
            .into()
    } else {
        panic!("TODO generate aa, ab, ac, ...");
    };

    if taken.contains(&generated_name) {
        // If the generated name is already taken, try again.
        name_type_var(letters_used + 1, taken)
    } else {
        taken.insert(generated_name.clone());

        (generated_name, letters_used + 1)
    }
}

use crate::subs::{VarId, Variable};
use crate::types::{AliasKind, Problem, RecordField};
use roc_collections::all::ImMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};

#[derive(Debug, Clone)]
pub struct SolvedLambdaSet(pub SolvedType);

/// This is a fully solved type, with no Variables remaining in it.
#[derive(Debug, Clone)]
pub enum SolvedType {
    /// A function. The types of its arguments, then the type of its return value.
    Func(Vec<SolvedType>, Box<SolvedType>, Box<SolvedType>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(Symbol, Vec<SolvedType>),
    /// A bound type variable, e.g. `a` in `(a -> a)`
    Rigid(Lowercase),
    Flex(VarId),
    Wildcard,
    /// Inline type alias, e.g. `as List a` in `[Cons a (List a), Nil] as List a`
    Record {
        fields: Vec<(Lowercase, RecordField<SolvedType>)>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Box<SolvedType>,
    },
    EmptyRecord,
    TagUnion(Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    LambdaTag(Symbol, Vec<SolvedType>),
    FunctionOrTagUnion(TagName, Symbol, Box<SolvedType>),
    RecursiveTagUnion(VarId, Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    EmptyTagUnion,
    /// A type from an Invalid module
    Erroneous(Problem),

    Alias(
        Symbol,
        Vec<SolvedType>,
        Vec<SolvedLambdaSet>,
        Box<SolvedType>,
        AliasKind,
    ),

    HostExposedAlias {
        name: Symbol,
        arguments: Vec<SolvedType>,
        lambda_set_variables: Vec<SolvedLambdaSet>,
        actual_var: VarId,
        actual: Box<SolvedType>,
    },

    /// A type error
    Error,
}

#[derive(Clone, Debug)]
pub struct BuiltinAlias {
    pub region: Region,
    pub vars: Vec<Loc<Lowercase>>,
    pub typ: SolvedType,
    pub kind: AliasKind,
}

#[derive(Debug, Clone, Default)]
pub struct FreeVars {
    pub named_vars: ImMap<Lowercase, Variable>,
    pub unnamed_vars: ImMap<VarId, Variable>,
    pub wildcards: Vec<Variable>,
}

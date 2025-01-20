use crate::abilities::SpecializationId;
use crate::annotation::{freshen_opaque_def, IntroducedVariables};
use crate::def::{can_defs_with_return, Annotation, Def};
use crate::env::Env;
use crate::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_num, float_expr_from_result,
    int_expr_from_result, num_expr_from_result, FloatBound, IntBound, NumBound,
};
use crate::params_in_abilities_unimplemented;
use crate::pattern::{canonicalize_pattern, BindingsFromPattern, Pattern, PermitShadows};
use crate::procedure::{QualifiedReference, References};
use crate::scope::{Scope, SymbolLookup};
use crate::traverse::{walk_expr, Visitor};
use bumpalo::collections::Vec as BumpVec;
use roc_collections::soa::index_push_new;
use roc_collections::{SendMap, VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::{ForeignSymbol, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentId, ModuleId, Symbol};
use roc_parse::ast::{self, Defs, DesugarProblem, ResultTryKind, StrLiteral};
use roc_parse::expr::RecordValuePrefix;
use roc_parse::ident::Accessor;
use roc_parse::pattern::PatternType::*;
use roc_problem::can::{PrecedenceProblem, Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::num::SingleQuoteBound;
use roc_types::subs::{ExhaustiveMark, IllegalCycleMark, RedundantMark, VarStore, Variable};
use roc_types::types::{
    Alias, Category, EarlyReturnKind, IndexOrField, LambdaSet, OptAbleVar, Type,
};
use soa::Index;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::sync::Arc;
use std::{char, u32};

/// Derives that an opaque type has claimed, to checked and recorded after solving.
pub type PendingDerives = VecMap<Symbol, (Type, Vec<Loc<Symbol>>)>;

#[derive(Clone, Default, Debug)]
pub struct Output {
    pub references: References,
    pub tail_calls: Vec<Symbol>,
    pub introduced_variables: IntroducedVariables,
    pub aliases: VecMap<Symbol, Alias>,
    pub non_closures: VecSet<Symbol>,
    pub pending_derives: PendingDerives,
}

impl Output {
    pub fn union(&mut self, other: Self) {
        self.references.union_mut(&other.references);

        if self.tail_calls.is_empty() && !other.tail_calls.is_empty() {
            self.tail_calls = other.tail_calls;
        }

        self.introduced_variables
            .union_owned(other.introduced_variables);
        self.aliases.extend(other.aliases);
        self.non_closures.extend(other.non_closures);

        {
            let expected_derives_size = self.pending_derives.len() + other.pending_derives.len();
            self.pending_derives.extend(other.pending_derives);
            debug_assert!(
                expected_derives_size == self.pending_derives.len(),
                "Derives overwritten from nested scope - something is very wrong"
            );
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum IntValue {
    I128([u8; 16]),
    U128([u8; 16]),
}

impl Display for IntValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntValue::I128(n) => Display::fmt(&i128::from_ne_bytes(*n), f),
            IntValue::U128(n) => Display::fmt(&u128::from_ne_bytes(*n), f),
        }
    }
}

impl IntValue {
    pub fn as_u8(self) -> u8 {
        self.as_u128() as u8
    }

    pub fn as_i8(self) -> i8 {
        self.as_i128() as i8
    }

    pub fn as_u16(self) -> u16 {
        self.as_u128() as u16
    }

    pub fn as_i16(self) -> i16 {
        self.as_i128() as i16
    }

    pub fn as_u32(self) -> u32 {
        self.as_u128() as u32
    }

    pub fn as_i32(self) -> i32 {
        self.as_i128() as i32
    }

    pub fn as_u64(self) -> u64 {
        self.as_u128() as u64
    }

    pub fn as_i64(self) -> i64 {
        self.as_i128() as i64
    }

    pub fn as_u128(self) -> u128 {
        match self {
            IntValue::I128(i128) => i128::from_ne_bytes(i128) as u128,
            IntValue::U128(u128) => u128::from_ne_bytes(u128),
        }
    }

    pub fn as_i128(self) -> i128 {
        match self {
            IntValue::I128(i128) => i128::from_ne_bytes(i128),
            IntValue::U128(u128) => u128::from_ne_bytes(u128) as i128,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    Num(Variable, Box<str>, IntValue, NumBound),

    // Int and Float store a variable to generate better error messages
    Int(Variable, Variable, Box<str>, IntValue, IntBound),
    Float(Variable, Variable, Box<str>, f64, FloatBound),
    Str(Box<str>),
    // Number variable, precision variable, value, bound
    SingleQuote(Variable, Variable, char, SingleQuoteBound),
    List {
        elem_var: Variable,
        loc_elems: Vec<Loc<Expr>>,
    },

    // An ingested files, its bytes, and the type variable.
    IngestedFile(Box<PathBuf>, Arc<Vec<u8>>, Variable),

    // Lookups
    Var(Symbol, Variable),
    /// Like Var, but from a module with params
    ParamsVar {
        symbol: Symbol,
        var: Variable,
        params_symbol: Symbol,
        params_var: Variable,
    },
    AbilityMember(
        /// Actual member name
        Symbol,
        /// Specialization to use, and its variable.
        /// The specialization id may be [`None`] if construction of an ability member usage can
        /// prove the usage is polymorphic.
        Option<SpecializationId>,
        Variable,
    ),

    // Branching
    When {
        /// The actual condition of the when expression.
        loc_cond: Box<Loc<Expr>>,
        cond_var: Variable,
        /// Type of each branch (and therefore the type of the entire `when` expression)
        expr_var: Variable,
        region: Region,
        /// The branches of the when, and the type of the condition that they expect to be matched
        /// against.
        branches: Vec<WhenBranch>,
        branches_cond_var: Variable,
        /// Whether the branches are exhaustive.
        exhaustive: ExhaustiveMark,
    },
    If {
        cond_var: Variable,
        branch_var: Variable,
        branches: Vec<(Loc<Expr>, Loc<Expr>)>,
        final_else: Box<Loc<Expr>>,
    },

    // Let
    LetRec(Vec<Def>, Box<Loc<Expr>>, IllegalCycleMark),
    LetNonRec(Box<Def>, Box<Loc<Expr>>),

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call(
        Box<(Variable, Loc<Expr>, Variable, Variable, Variable)>,
        Vec<(Variable, Loc<Expr>)>,
        CalledVia,
    ),
    RunLowLevel {
        op: LowLevel,
        args: Vec<(Variable, Expr)>,
        ret_var: Variable,
    },
    ForeignCall {
        foreign_symbol: ForeignSymbol,
        args: Vec<(Variable, Expr)>,
        ret_var: Variable,
    },

    Closure(ClosureData),

    // Product Types
    Record {
        record_var: Variable,
        fields: SendMap<Lowercase, Field>,
    },

    /// Empty record constant
    EmptyRecord,

    Tuple {
        tuple_var: Variable,
        elems: Vec<(Variable, Box<Loc<Expr>>)>,
    },

    /// Module params expression in import
    ImportParams(ModuleId, Region, Option<(Variable, Box<Expr>)>),

    /// The "crash" keyword
    Crash {
        msg: Box<Loc<Expr>>,
        ret_var: Variable,
    },

    /// Look up exactly one field on a record, e.g. (expr).foo.
    RecordAccess {
        record_var: Variable,
        ext_var: Variable,
        field_var: Variable,
        loc_expr: Box<Loc<Expr>>,
        field: Lowercase,
    },

    /// tuple or field accessor as a function, e.g. (.foo) expr or (.1) expr
    RecordAccessor(StructAccessorData),

    TupleAccess {
        tuple_var: Variable,
        ext_var: Variable,
        elem_var: Variable,
        loc_expr: Box<Loc<Expr>>,
        index: usize,
    },

    RecordUpdate {
        record_var: Variable,
        ext_var: Variable,
        symbol: Symbol,
        updates: SendMap<Lowercase, Field>,
    },

    // Sum Types
    Tag {
        tag_union_var: Variable,
        ext_var: Variable,
        name: TagName,
        arguments: Vec<(Variable, Loc<Expr>)>,
    },

    ZeroArgumentTag {
        closure_name: Symbol,
        variant_var: Variable,
        ext_var: Variable,
        name: TagName,
    },

    /// A wrapping of an opaque type, like `@Age 21`
    OpaqueRef {
        opaque_var: Variable,
        name: Symbol,
        argument: Box<(Variable, Loc<Expr>)>,

        // The following help us link this opaque reference to the type specified by its
        // definition, which we then use during constraint generation. For example
        // suppose we have
        //
        //   Id n := [Id U64 n]
        //   @Id "sasha"
        //
        // Then `opaque` is "Id", `argument` is "sasha", but this is not enough for us to
        // infer the type of the expression as "Id Str" - we need to link the specialized type of
        // the variable "n".
        // That's what `specialized_def_type` and `type_arguments` are for; they are specialized
        // for the expression from the opaque definition. `type_arguments` is something like
        // [(n, fresh1)], and `specialized_def_type` becomes "[Id U64 fresh1]".
        specialized_def_type: Box<Type>,
        type_arguments: Vec<OptAbleVar>,
        lambda_set_variables: Vec<LambdaSet>,
    },

    // Opaque as a function, e.g. @Id as a shorthand for \x -> @Id x
    OpaqueWrapFunction(OpaqueWrapFunctionData),

    // Test
    Expect {
        loc_condition: Box<Loc<Expr>>,
        loc_continuation: Box<Loc<Expr>>,
        lookups_in_cond: Vec<ExpectLookup>,
    },

    Dbg {
        source_location: Box<str>,
        source: Box<str>,
        loc_message: Box<Loc<Expr>>,
        loc_continuation: Box<Loc<Expr>>,
        variable: Variable,
        symbol: Symbol,
    },

    Try {
        result_expr: Box<Loc<Expr>>,
        result_var: Variable,
        return_var: Variable,
        ok_payload_var: Variable,
        err_payload_var: Variable,
        err_ext_var: Variable,
        kind: TryKind,
    },

    Return {
        return_value: Box<Loc<Expr>>,
        return_var: Variable,
    },

    /// Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TryKind {
    KeywordPrefix,
    OperatorSuffix,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ExpectLookup {
    pub symbol: Symbol,
    pub var: Variable,
    pub ability_info: Option<SpecializationId>,
}

impl Expr {
    pub fn category(&self) -> Category {
        match self {
            Self::Num(..) => Category::Num,
            Self::Int(..) => Category::Int,
            Self::Float(..) => Category::Frac,
            Self::Str(..) => Category::Str,
            Self::IngestedFile(file_path, _, _) => Category::IngestedFile(file_path.clone()),
            Self::SingleQuote(..) => Category::Character,
            Self::List { .. } => Category::List,
            &Self::Var(sym, _) => Category::Lookup(sym),
            &Self::ParamsVar {
                symbol,
                var: _,
                params_symbol: _,
                params_var: _,
            } => Category::Lookup(symbol),
            &Self::AbilityMember(sym, _, _) => Category::Lookup(sym),
            Self::When { .. } => Category::When,
            Self::If { .. } => Category::If,
            Self::LetRec(_, expr, _) => expr.value.category(),
            Self::LetNonRec(_, expr) => expr.value.category(),
            &Self::Call(_, _, called_via) => Category::CallResult(None, called_via),
            &Self::RunLowLevel { op, .. } => Category::LowLevelOpResult(op),
            Self::ForeignCall { .. } => Category::ForeignCall,
            Self::Closure(..) => Category::Lambda,
            Self::Tuple { .. } => Category::Tuple,
            Self::Record { .. } => Category::Record,
            Self::EmptyRecord => Category::Record,
            Self::RecordAccess { field, .. } => Category::RecordAccess(field.clone()),
            Self::RecordAccessor(data) => Category::Accessor(data.field.clone()),
            Self::TupleAccess { index, .. } => Category::TupleAccess(*index),
            Self::RecordUpdate { .. } => Category::Record,
            Self::ImportParams(_, _, Some((_, expr))) => expr.category(),
            Self::ImportParams(_, _, None) => Category::Unknown,
            Self::Tag {
                name, arguments, ..
            } => Category::TagApply {
                tag_name: name.clone(),
                args_count: arguments.len(),
            },
            Self::ZeroArgumentTag { name, .. } => Category::TagApply {
                tag_name: name.clone(),
                args_count: 0,
            },
            &Self::OpaqueRef { name, .. } => Category::OpaqueWrap(name),
            &Self::OpaqueWrapFunction(OpaqueWrapFunctionData { opaque_name, .. }) => {
                Category::OpaqueWrap(opaque_name)
            }
            Self::Expect { .. } => Category::Expect,
            Self::Crash { .. } => Category::Crash,
            Self::Return { .. } => Category::Return(EarlyReturnKind::Return),

            Self::Dbg { .. } => Category::Expect,
            Self::Try { .. } => Category::TrySuccess,

            // these nodes place no constraints on the expression's type
            Self::RuntimeError(..) => Category::Unknown,
        }
    }

    pub fn contains_any_early_returns(&self) -> bool {
        match self {
            Self::Num { .. }
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::Str { .. }
            | Self::IngestedFile { .. }
            | Self::SingleQuote { .. }
            | Self::Var { .. }
            | Self::AbilityMember { .. }
            | Self::ParamsVar { .. }
            | Self::Closure(..)
            | Self::EmptyRecord
            | Self::RecordAccessor(_)
            | Self::ZeroArgumentTag { .. }
            | Self::OpaqueWrapFunction(_)
            | Self::RuntimeError(..) => false,
            Self::Return { .. } | Self::Try { .. } => true,
            Self::List { loc_elems, .. } => loc_elems
                .iter()
                .any(|elem| elem.value.contains_any_early_returns()),
            Self::When {
                loc_cond, branches, ..
            } => {
                loc_cond.value.contains_any_early_returns()
                    || branches.iter().any(|branch| {
                        branch
                            .guard
                            .as_ref()
                            .is_some_and(|guard| guard.value.contains_any_early_returns())
                            || branch.value.value.contains_any_early_returns()
                    })
            }
            Self::If {
                branches,
                final_else,
                ..
            } => {
                final_else.value.contains_any_early_returns()
                    || branches.iter().any(|(cond, then)| {
                        cond.value.contains_any_early_returns()
                            || then.value.contains_any_early_returns()
                    })
            }
            Self::LetRec(defs, expr, _cycle_mark) => {
                expr.value.contains_any_early_returns()
                    || defs
                        .iter()
                        .any(|def| def.loc_expr.value.contains_any_early_returns())
            }
            Self::LetNonRec(def, expr) => {
                def.loc_expr.value.contains_any_early_returns()
                    || expr.value.contains_any_early_returns()
            }
            Self::Call(_func, args, _called_via) => args
                .iter()
                .any(|(_var, arg_expr)| arg_expr.value.contains_any_early_returns()),
            Self::RunLowLevel { args, .. } | Self::ForeignCall { args, .. } => args
                .iter()
                .any(|(_var, arg_expr)| arg_expr.contains_any_early_returns()),
            Self::Tuple { elems, .. } => elems
                .iter()
                .any(|(_var, loc_elem)| loc_elem.value.contains_any_early_returns()),
            Self::Record { fields, .. } => fields
                .iter()
                .any(|(_field_name, field)| field.loc_expr.value.contains_any_early_returns()),
            Self::RecordAccess { loc_expr, .. } => loc_expr.value.contains_any_early_returns(),
            Self::TupleAccess { loc_expr, .. } => loc_expr.value.contains_any_early_returns(),
            Self::RecordUpdate { updates, .. } => {
                updates.iter().any(|(_field_name, field_update)| {
                    field_update.loc_expr.value.contains_any_early_returns()
                })
            }
            Self::ImportParams(_module_id, _region, params) => params
                .as_ref()
                .is_some_and(|(_var, p)| p.contains_any_early_returns()),
            Self::Tag { arguments, .. } => arguments
                .iter()
                .any(|(_var, arg)| arg.value.contains_any_early_returns()),
            Self::OpaqueRef { argument, .. } => argument.1.value.contains_any_early_returns(),
            Self::Crash { msg, .. } => msg.value.contains_any_early_returns(),
            Self::Dbg {
                loc_message,
                loc_continuation,
                ..
            } => {
                loc_message.value.contains_any_early_returns()
                    || loc_continuation.value.contains_any_early_returns()
            }
            Self::Expect {
                loc_condition,
                loc_continuation,
                ..
            } => {
                loc_condition.value.contains_any_early_returns()
                    || loc_continuation.value.contains_any_early_returns()
            }
        }
    }
}

/// Stores exhaustiveness-checking metadata for a closure argument that may
/// have an annotated type.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AnnotatedMark {
    pub annotation_var: Variable,
    pub exhaustive: ExhaustiveMark,
}

impl AnnotatedMark {
    pub fn new(var_store: &mut VarStore) -> Self {
        Self {
            annotation_var: var_store.fresh(),
            exhaustive: ExhaustiveMark::new(var_store),
        }
    }

    // NOTE: only ever use this if you *know* a pattern match is surely exhaustive!
    // Otherwise you will get unpleasant unification errors.
    pub fn known_exhaustive() -> Self {
        Self {
            annotation_var: Variable::EMPTY_TAG_UNION,
            exhaustive: ExhaustiveMark::known_exhaustive(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClosureData {
    pub function_type: Variable,
    pub closure_type: Variable,
    pub return_type: Variable,
    pub fx_type: Variable,
    pub early_returns: Vec<(Variable, Region, EarlyReturnKind)>,
    pub name: Symbol,
    pub captured_symbols: Vec<(Symbol, Variable)>,
    pub recursive: Recursive,
    pub arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)>,
    pub loc_body: Box<Loc<Expr>>,
}

/// A record or tuple accessor like `.foo` or `.0`, which is equivalent to `\r -> r.foo`
/// Struct accessors are desugared to closures; they need to have a name
/// so the closure can have a correct lambda set.
///
/// We distinguish them from closures so we can have better error messages
/// during constraint generation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructAccessorData {
    pub name: Symbol,
    pub function_var: Variable,
    pub record_var: Variable,
    pub closure_var: Variable,
    pub ext_var: Variable,
    pub field_var: Variable,

    /// Note that the `field` field is an `IndexOrField` in order to represent both
    /// record and tuple accessors. This is different from `TupleAccess` and
    /// `RecordAccess` (and RecordFields/TupleElems), which share less of their implementation.
    pub field: IndexOrField,
}

impl StructAccessorData {
    pub fn to_closure_data(self, record_symbol: Symbol) -> ClosureData {
        let StructAccessorData {
            name,
            function_var,
            record_var,
            closure_var,
            ext_var,
            field_var,
            field,
        } = self;

        // IDEA: convert accessor from
        //
        // .foo
        //
        // into
        //
        // (\r -> r.foo)
        let body = match field {
            IndexOrField::Index(index) => Expr::TupleAccess {
                tuple_var: record_var,
                ext_var,
                elem_var: field_var,
                loc_expr: Box::new(Loc::at_zero(Expr::Var(record_symbol, record_var))),
                index,
            },
            IndexOrField::Field(field) => Expr::RecordAccess {
                record_var,
                ext_var,
                field_var,
                loc_expr: Box::new(Loc::at_zero(Expr::Var(record_symbol, record_var))),
                field,
            },
        };

        let loc_body = Loc::at_zero(body);

        let arguments = vec![(
            record_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(record_symbol)),
        )];

        ClosureData {
            function_type: function_var,
            closure_type: closure_var,
            return_type: field_var,
            fx_type: Variable::PURE,
            early_returns: vec![],
            name,
            captured_symbols: vec![],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(loc_body),
        }
    }
}

/// An opaque wrapper like `@Foo`, which is equivalent to `\p -> @Foo p`
/// These are desugared to closures, but we distinguish them so we can have
/// better error messages during constraint generation.
#[derive(Clone, Debug, PartialEq)]
pub struct OpaqueWrapFunctionData {
    pub opaque_name: Symbol,
    pub opaque_var: Variable,
    // The following fields help link the concrete opaque type; see
    // `Expr::OpaqueRef` for more info on how they're used.
    pub specialized_def_type: Type,
    pub type_arguments: Vec<OptAbleVar>,
    pub lambda_set_variables: Vec<LambdaSet>,

    pub function_name: Symbol,
    pub function_var: Variable,
    pub argument_var: Variable,
    pub closure_var: Variable,
}

impl OpaqueWrapFunctionData {
    pub fn to_closure_data(self, argument_symbol: Symbol) -> ClosureData {
        let OpaqueWrapFunctionData {
            opaque_name,
            opaque_var,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
            function_name,
            function_var,
            argument_var,
            closure_var,
        } = self;

        // IDEA: convert
        //
        // @Foo
        //
        // into
        //
        // (\p -> @Foo p)
        let body = Expr::OpaqueRef {
            opaque_var,
            name: opaque_name,
            argument: Box::new((
                argument_var,
                Loc::at_zero(Expr::Var(argument_symbol, argument_var)),
            )),
            specialized_def_type: Box::new(specialized_def_type),
            type_arguments,
            lambda_set_variables,
        };

        let loc_body = Loc::at_zero(body);

        let arguments = vec![(
            argument_var,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(argument_symbol)),
        )];

        ClosureData {
            function_type: function_var,
            closure_type: closure_var,
            return_type: opaque_var,
            fx_type: Variable::PURE,
            early_returns: vec![],
            name: function_name,
            captured_symbols: vec![],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(loc_body),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub var: Variable,
    // The region of the full `foo: f bar`, rather than just `f bar`
    pub region: Region,
    pub loc_expr: Box<Loc<Expr>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Recursive {
    NotRecursive = 0,
    Recursive = 1,
    TailRecursive = 2,
}

impl Recursive {
    pub fn is_recursive(&self) -> bool {
        match self {
            Recursive::NotRecursive => false,
            Recursive::Recursive | Recursive::TailRecursive => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranchPattern {
    pub pattern: Loc<Pattern>,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    pub degenerate: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranch {
    pub patterns: Vec<WhenBranchPattern>,
    pub value: Loc<Expr>,
    pub guard: Option<Loc<Expr>>,
    /// Whether this branch is redundant in the `when` it appears in
    pub redundant: RedundantMark,
}

#[allow(clippy::too_many_arguments)]
fn canonicalize_expr_apply<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    loc_fn: &&'a Loc<ast::Expr>,
    args: BumpVec<'a, (Variable, Loc<Expr>)>,
    output: &mut Output,
    region: Region,
    application_style: CalledVia,
) -> Expr {
    use Expr::*;

    let fn_region = loc_fn.region;
    if let ast::Expr::OpaqueRef(name) = loc_fn.value {
        // We treat opaques specially, since an opaque can wrap exactly one argument.

        if args.is_empty() {
            let loc_name = Loc::at(region, (*name).into());
            let problem = roc_problem::can::RuntimeError::OpaqueNotApplied(loc_name);
            env.problem(Problem::RuntimeError(problem.clone()));
            RuntimeError(problem)
        } else if args.len() > 1 {
            let problem = roc_problem::can::RuntimeError::OpaqueAppliedToMultipleArgs(region);
            env.problem(Problem::RuntimeError(problem.clone()));
            RuntimeError(problem)
        } else {
            match scope.lookup_opaque_ref(name, loc_fn.region) {
                Err(runtime_error) => {
                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    RuntimeError(runtime_error)
                }
                Ok((name, opaque_def)) => {
                    let argument = Box::new(args.first().unwrap().clone());
                    output
                        .references
                        .insert_type_lookup(name, QualifiedReference::Unqualified);

                    let (type_arguments, lambda_set_variables, specialized_def_type) =
                        freshen_opaque_def(var_store, opaque_def);

                    OpaqueRef {
                        opaque_var: var_store.fresh(),
                        name,
                        argument,
                        specialized_def_type: Box::new(specialized_def_type),
                        type_arguments,
                        lambda_set_variables,
                    }
                }
            }
        }
    } else if let ast::Expr::Crash = loc_fn.value {
        // We treat crash specially, since crashing must be applied with one argument.

        debug_assert!(!args.is_empty());

        let crash = if args.len() > 1 {
            let args_region = Region::span_across(
                &args.first().unwrap().1.region,
                &args.last().unwrap().1.region,
            );
            env.problem(Problem::OverAppliedCrash {
                region: args_region,
            });
            // Still crash, just with our own message, and drop the references.
            Crash {
                msg: Box::new(Loc::at(
                    region,
                    Expr::Str(String::from("hit a crash!").into_boxed_str()),
                )),
                ret_var: var_store.fresh(),
            }
        } else {
            let msg = args.first().unwrap();
            Crash {
                msg: Box::new(msg.1.clone()),
                ret_var: var_store.fresh(),
            }
        };

        crash
    } else {
        // Canonicalize the function expression and its arguments
        let (fn_expr, fn_expr_output) =
            canonicalize_expr(env, var_store, scope, fn_region, &loc_fn.value);

        output.union(fn_expr_output);

        // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
        output.tail_calls = vec![];

        match fn_expr.value {
            Var(symbol, _) => {
                output.references.insert_call(symbol);

                // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                if env
                    .tailcallable_symbol
                    .is_some_and(|tc_sym| tc_sym == symbol)
                {
                    output.tail_calls.push(symbol);
                }

                Call(
                    Box::new((
                        var_store.fresh(),
                        fn_expr,
                        var_store.fresh(),
                        var_store.fresh(),
                        var_store.fresh(),
                    )),
                    args.to_vec(),
                    application_style,
                )
            }
            RuntimeError(_) => {
                // We can't call a runtime error; bail out by propagating it!
                return fn_expr.value;
            }
            Tag {
                tag_union_var: variant_var,
                ext_var,
                name,
                ..
            } => Tag {
                tag_union_var: variant_var,
                ext_var,
                name,
                arguments: args.to_vec(),
            },
            ZeroArgumentTag {
                variant_var,
                ext_var,
                name,
                ..
            } => Tag {
                tag_union_var: variant_var,
                ext_var,
                name,
                arguments: args.to_vec(),
            },
            _ => {
                // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                Call(
                    Box::new((
                        var_store.fresh(),
                        fn_expr,
                        var_store.fresh(),
                        var_store.fresh(),
                        var_store.fresh(),
                    )),
                    args.to_vec(),
                    application_style,
                )
            }
        }
    }
}

pub fn canonicalize_expr<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    region: Region,
    expr: &'a ast::Expr<'a>,
) -> (Loc<Expr>, Output) {
    use Expr::*;

    let (expr, output) = match expr {
        &ast::Expr::Num(str) => {
            let answer = num_expr_from_result(var_store, finish_parsing_num(str), region, env);

            (answer, Output::default())
        }
        &ast::Expr::Float(str) => {
            let answer = float_expr_from_result(var_store, finish_parsing_float(str), region, env);

            (answer, Output::default())
        }
        ast::Expr::Record(fields) => canonicalize_record(env, var_store, scope, region, *fields),
        ast::Expr::RecordUpdate {
            fields,
            update: loc_update,
        } => {
            let (can_update, update_out) =
                canonicalize_expr(env, var_store, scope, loc_update.region, &loc_update.value);
            if let Var(symbol, _) = &can_update.value {
                match canonicalize_fields(env, scope, var_store, region, fields.items) {
                    Ok((can_fields, mut output)) => {
                        output.references.union_mut(&update_out.references);

                        let answer = RecordUpdate {
                            record_var: var_store.fresh(),
                            ext_var: var_store.fresh(),
                            symbol: *symbol,
                            updates: can_fields,
                        };

                        (answer, output)
                    }
                    Err(runtime_error) => (Expr::RuntimeError(runtime_error), Output::default()),
                }
            } else {
                // only (optionally qualified) variables can be updated, not arbitrary expressions

                let error = roc_problem::can::RuntimeError::InvalidRecordUpdate {
                    region: can_update.region,
                };

                let answer = Expr::RuntimeError(error.clone());

                env.problems.push(Problem::RuntimeError(error));

                (answer, Output::default())
            }
        }

        ast::Expr::Tuple(fields) => {
            let mut can_elems = Vec::with_capacity(fields.len());
            let mut references = References::new();

            for loc_elem in fields.iter() {
                let (can_expr, elem_out) =
                    canonicalize_expr(env, var_store, scope, loc_elem.region, &loc_elem.value);

                references.union_mut(&elem_out.references);

                can_elems.push((var_store.fresh(), Box::new(can_expr)));
            }

            let output = Output {
                references,
                ..Default::default()
            };

            (
                Tuple {
                    tuple_var: var_store.fresh(),
                    elems: can_elems,
                },
                output,
            )
        }

        ast::Expr::Str(literal) => flatten_str_literal(env, var_store, scope, literal),

        ast::Expr::SingleQuote(string) => {
            let mut it = string.chars().peekable();
            if let Some(char) = it.next() {
                if it.peek().is_none() {
                    (
                        Expr::SingleQuote(
                            var_store.fresh(),
                            var_store.fresh(),
                            char,
                            SingleQuoteBound::from_char(char),
                        ),
                        Output::default(),
                    )
                } else {
                    // multiple chars is found
                    let error = roc_problem::can::RuntimeError::MultipleCharsInSingleQuote(region);
                    let answer = Expr::RuntimeError(error);

                    (answer, Output::default())
                }
            } else {
                // no characters found
                let error = roc_problem::can::RuntimeError::EmptySingleQuote(region);
                let answer = Expr::RuntimeError(error);

                (answer, Output::default())
            }
        }

        ast::Expr::List(loc_elems) => {
            if loc_elems.is_empty() {
                (
                    List {
                        elem_var: var_store.fresh(),
                        loc_elems: Vec::new(),
                    },
                    Output::default(),
                )
            } else {
                let mut can_elems = Vec::with_capacity(loc_elems.len());
                let mut references = References::new();

                for loc_elem in loc_elems.iter() {
                    let (can_expr, elem_out) =
                        canonicalize_expr(env, var_store, scope, loc_elem.region, &loc_elem.value);

                    references.union_mut(&elem_out.references);

                    can_elems.push(can_expr);
                }

                let output = Output {
                    references,
                    ..Default::default()
                };

                (
                    List {
                        elem_var: var_store.fresh(),
                        loc_elems: can_elems,
                    },
                    output,
                )
            }
        }
        ast::Expr::PncApply(loc_fn, loc_args) => {
            // The function's return type
            let mut args = BumpVec::with_capacity_in(loc_args.items.len(), env.arena);
            let mut output = Output::default();

            for loc_arg in loc_args.items.iter() {
                let (arg_expr, arg_out) =
                    canonicalize_expr(env, var_store, scope, loc_arg.region, &loc_arg.value);

                args.push((var_store.fresh(), arg_expr));
                output.references.union_mut(&arg_out.references);
            }
            let value = canonicalize_expr_apply(
                env,
                var_store,
                scope,
                loc_fn,
                args,
                &mut output,
                region,
                CalledVia::Space,
            );
            (value, output)
        }
        ast::Expr::Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz

            // The function's return type
            let mut args = BumpVec::with_capacity_in(loc_args.len(), env.arena);
            let mut output = Output::default();

            for loc_arg in loc_args.iter() {
                let (arg_expr, arg_out) =
                    canonicalize_expr(env, var_store, scope, loc_arg.region, &loc_arg.value);

                args.push((var_store.fresh(), arg_expr));
                output.references.union_mut(&arg_out.references);
            }
            let value = canonicalize_expr_apply(
                env,
                var_store,
                scope,
                loc_fn,
                args,
                &mut output,
                region,
                *application_style,
            );
            (value, output)
        }
        ast::Expr::Var { module_name, ident } => {
            canonicalize_var_lookup(env, var_store, scope, module_name, ident, region)
        }
        ast::Expr::Underscore(name) => {
            // we parse underscores, but they are not valid expression syntax

            let problem = roc_problem::can::RuntimeError::MalformedIdentifier(
                (*name).into(),
                if name.is_empty() {
                    roc_parse::ident::BadIdent::UnderscoreAlone(region.start())
                } else {
                    roc_parse::ident::BadIdent::UnderscoreAtStart {
                        position: region.start(),
                        // Check if there's an ignored identifier with this name in scope (for better error messages)
                        declaration_region: scope.lookup_ignored_local(name),
                    }
                },
                region,
            );

            env.problem(Problem::RuntimeError(problem.clone()));

            (RuntimeError(problem), Output::default())
        }
        ast::Expr::Crash => {
            // Naked crashes aren't allowed; we'll admit this with our own message, but yield an
            // error.
            env.problem(Problem::UnappliedCrash { region });

            (
                Crash {
                    msg: Box::new(Loc::at(
                        region,
                        Expr::Str(String::from("hit a crash!").into_boxed_str()),
                    )),
                    ret_var: var_store.fresh(),
                },
                Output::default(),
            )
        }
        ast::Expr::Defs(loc_defs, loc_ret) => {
            // The body expression gets a new scope for canonicalization,
            scope.inner_def_scope(|inner_scope| {
                let defs: Defs = (*loc_defs).clone();
                can_defs_with_return(env, var_store, inner_scope, env.arena.alloc(defs), loc_ret)
            })
        }
        ast::Expr::RecordBuilder { .. } => {
            internal_error!("Record builder should have been desugared by now")
        }
        ast::Expr::RecordUpdater(_) => {
            internal_error!("Record updater should have been desugared by now")
        }
        ast::Expr::Closure(loc_arg_patterns, loc_body_expr) => {
            let (closure_data, output) =
                canonicalize_closure(env, var_store, scope, loc_arg_patterns, loc_body_expr, None);

            (Closure(closure_data), output)
        }
        ast::Expr::When(loc_cond, branches) => {
            // Infer the condition expression's type.
            let cond_var = var_store.fresh();
            let (can_cond, mut output) =
                canonicalize_expr(env, var_store, scope, loc_cond.region, &loc_cond.value);

            // the condition can never be a tail-call
            output.tail_calls = vec![];

            let mut can_branches = Vec::with_capacity(branches.len());

            for branch in branches.iter() {
                let (can_when_branch, branch_references) = scope.inner_def_scope(|inner_scope| {
                    canonicalize_when_branch(
                        env,
                        var_store,
                        inner_scope,
                        region,
                        branch,
                        &mut output,
                    )
                });

                output.references.union_mut(&branch_references);

                can_branches.push(can_when_branch);
            }

            // A "when" with no branches is a runtime error, but it will mess things up
            // if code gen mistakenly thinks this is a tail call just because its condition
            // happened to be one. (The condition gave us our initial output value.)
            if branches.is_empty() {
                output.tail_calls = vec![];
            }

            // Incorporate all three expressions into a combined Output value.
            let expr = When {
                expr_var: var_store.fresh(),
                cond_var,
                region,
                loc_cond: Box::new(can_cond),
                branches: can_branches,
                branches_cond_var: var_store.fresh(),
                exhaustive: ExhaustiveMark::new(var_store),
            };

            (expr, output)
        }
        ast::Expr::RecordAccess(record_expr, field) => {
            let (loc_expr, output) = canonicalize_expr(env, var_store, scope, region, record_expr);

            (
                RecordAccess {
                    record_var: var_store.fresh(),
                    field_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    loc_expr: Box::new(loc_expr),
                    field: Lowercase::from(*field),
                },
                output,
            )
        }
        ast::Expr::AccessorFunction(field) => {
            canonicalize_accessor_function(env, var_store, scope, field, region)
        }
        ast::Expr::TupleAccess(tuple_expr, field) => {
            let (loc_expr, output) = canonicalize_expr(env, var_store, scope, region, tuple_expr);

            let res = if let Ok(index) = field.parse() {
                TupleAccess {
                    tuple_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    elem_var: var_store.fresh(),
                    loc_expr: Box::new(loc_expr),
                    index,
                }
            } else {
                let error = roc_problem::can::RuntimeError::InvalidTupleIndex(region);
                env.problem(Problem::RuntimeError(error.clone()));
                Expr::RuntimeError(error)
            };
            (res, output)
        }
        ast::Expr::TrySuffix { .. } => internal_error!(
            "a Expr::TrySuffix expression was not completely removed in desugar_value_def_suffixed"
        ),
        ast::Expr::Try => {
            // Treat remaining `try` keywords as normal variables so that we can continue to support `Result.try`
            canonicalize_var_lookup(env, var_store, scope, "", "try", region)
        }

        ast::Expr::Tag(tag) => {
            let variant_var = var_store.fresh();
            let ext_var = var_store.fresh();

            let symbol = scope.gen_unique_symbol();

            (
                ZeroArgumentTag {
                    name: TagName((*tag).into()),
                    variant_var,
                    closure_name: symbol,
                    ext_var,
                },
                Output::default(),
            )
        }
        ast::Expr::OpaqueRef(name) => {
            // If we're here, the opaque reference is definitely not wrapping an argument - wrapped
            // arguments are handled in the Apply branch.
            // Treat this as a function \payload -> @Opaque payload
            match scope.lookup_opaque_ref(name, region) {
                Err(runtime_error) => {
                    env.problem(Problem::RuntimeError(runtime_error.clone()));
                    (RuntimeError(runtime_error), Output::default())
                }
                Ok((name, opaque_def)) => {
                    let mut output = Output::default();
                    output
                        .references
                        .insert_type_lookup(name, QualifiedReference::Unqualified);

                    let (type_arguments, lambda_set_variables, specialized_def_type) =
                        freshen_opaque_def(var_store, opaque_def);

                    let fn_symbol = scope.gen_unique_symbol();

                    (
                        OpaqueWrapFunction(OpaqueWrapFunctionData {
                            opaque_name: name,
                            opaque_var: var_store.fresh(),
                            specialized_def_type,
                            type_arguments,
                            lambda_set_variables,

                            function_name: fn_symbol,
                            function_var: var_store.fresh(),
                            argument_var: var_store.fresh(),
                            closure_var: var_store.fresh(),
                        }),
                        output,
                    )
                }
            }
        }
        ast::Expr::Dbg => {
            // Dbg was not desugared as either part of an `Apply` or a `Pizza` binop, so it's
            // invalid.
            env.problem(Problem::UnappliedDbg { region });

            let invalid_dbg_expr = crate::desugar::desugar_invalid_dbg_expr(env, scope, region);

            let (loc_expr, output) =
                canonicalize_expr(env, var_store, scope, region, invalid_dbg_expr);

            (loc_expr.value, output)
        }
        ast::Expr::DbgStmt { .. } => {
            internal_error!("DbgStmt should have been desugared by now")
        }
        ast::Expr::LowLevelDbg((source_location, source), message, continuation) => {
            let mut output = Output::default();

            let (loc_message, output1) =
                canonicalize_expr(env, var_store, scope, message.region, &message.value);

            let (loc_continuation, output2) = canonicalize_expr(
                env,
                var_store,
                scope,
                continuation.region,
                &continuation.value,
            );

            output.union(output1);
            output.union(output2);

            // the symbol is used to bind the message `x = message`, and identify this `dbg`.
            // That would cause issues if we dbg a variable, like `dbg y`, because in the IR we
            // cannot alias variables. Hence, we make the dbg use that same variable `y`
            let symbol = match &loc_message.value {
                Expr::Var(symbol, _) => *symbol,
                _ => scope.gen_unique_symbol(),
            };

            (
                Dbg {
                    source_location: (*source_location).into(),
                    source: (*source).into(),
                    loc_message: Box::new(loc_message),
                    loc_continuation: Box::new(loc_continuation),
                    variable: var_store.fresh(),
                    symbol,
                },
                output,
            )
        }
        ast::Expr::LowLevelTry(loc_expr, kind) => {
            let (loc_result_expr, output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            let return_var = var_store.fresh();

            scope
                .early_returns
                .push((return_var, loc_expr.region, EarlyReturnKind::Try));

            (
                Try {
                    result_expr: Box::new(loc_result_expr),
                    result_var: var_store.fresh(),
                    return_var,
                    ok_payload_var: var_store.fresh(),
                    err_payload_var: var_store.fresh(),
                    err_ext_var: var_store.fresh(),
                    kind: match kind {
                        ResultTryKind::KeywordPrefix => TryKind::KeywordPrefix,
                        ResultTryKind::OperatorSuffix => TryKind::OperatorSuffix,
                    },
                },
                output,
            )
        }
        ast::Expr::Return(return_expr, after_return) => {
            let mut output = Output::default();

            if let Some(after_return) = after_return {
                let region_with_return =
                    Region::span_across(&return_expr.region, &after_return.region);

                env.problem(Problem::StatementsAfterReturn {
                    region: region_with_return,
                });
            }

            let (loc_return_expr, output1) = canonicalize_expr(
                env,
                var_store,
                scope,
                return_expr.region,
                &return_expr.value,
            );

            output.union(output1);

            let return_var = var_store.fresh();

            scope
                .early_returns
                .push((return_var, return_expr.region, EarlyReturnKind::Return));

            (
                Return {
                    return_value: Box::new(loc_return_expr),
                    return_var,
                },
                output,
            )
        }
        ast::Expr::If {
            if_thens,
            final_else: final_else_branch,
            ..
        } => {
            let mut branches = Vec::with_capacity(if_thens.len());
            let mut output = Output::default();

            for (condition, then_branch) in if_thens.iter() {
                let (loc_cond, cond_output) =
                    canonicalize_expr(env, var_store, scope, condition.region, &condition.value);

                let (loc_then, then_output) = canonicalize_expr(
                    env,
                    var_store,
                    scope,
                    then_branch.region,
                    &then_branch.value,
                );

                branches.push((loc_cond, loc_then));

                output.references.union_mut(&cond_output.references);
                output.references.union_mut(&then_output.references);
            }

            let (loc_else, else_output) = canonicalize_expr(
                env,
                var_store,
                scope,
                final_else_branch.region,
                &final_else_branch.value,
            );

            output.references.union_mut(&else_output.references);

            (
                If {
                    cond_var: var_store.fresh(),
                    branch_var: var_store.fresh(),
                    branches,
                    final_else: Box::new(loc_else),
                },
                output,
            )
        }

        ast::Expr::MalformedIdent(name, bad_ident) => {
            use roc_problem::can::RuntimeError::*;

            let problem = MalformedIdentifier((*name).into(), *bad_ident, region);
            env.problem(Problem::RuntimeError(problem.clone()));

            (RuntimeError(problem), Output::default())
        }
        ast::Expr::DesugarProblem(desugar_problem) => {
            use roc_problem::can::RuntimeError::*;

            let runtime_error = match desugar_problem {
                DesugarProblem::EmptyRecordBuilder(sub_expr) => EmptyRecordBuilder(sub_expr.region),
                DesugarProblem::SingleFieldRecordBuilder(sub_expr) => {
                    SingleFieldRecordBuilder(sub_expr.region)
                }
                DesugarProblem::OptionalFieldInRecordBuilder(loc_name, loc_value) => {
                    let sub_region = Region::span_across(&loc_name.region, &loc_value.region);
                    OptionalFieldInRecordBuilder {
                        record_region: region,
                        field_region: sub_region,
                    }
                }
                DesugarProblem::SpreadInRecordBuilder {
                    record_region,
                    spread_region,
                    opt_spread_expr: _,
                } => SpreadInRecordBuilder {
                    record_region: *record_region,
                    spread_region: *spread_region,
                },
                DesugarProblem::PrecedenceConflict(ast::PrecedenceConflict {
                    whole_region,
                    binop1_position,
                    binop2_position,
                    binop1,
                    binop2,
                    expr: _,
                }) => {
                    let region1 = Region::new(
                        *binop1_position,
                        binop1_position.bump_column(binop1.width() as u32),
                    );
                    let loc_binop1 = Loc::at(region1, *binop1);

                    let region2 = Region::new(
                        *binop2_position,
                        binop2_position.bump_column(binop2.width() as u32),
                    );
                    let loc_binop2 = Loc::at(region2, *binop2);

                    let problem = PrecedenceProblem::BothNonAssociative(
                        *whole_region,
                        loc_binop1,
                        loc_binop2,
                    );

                    InvalidPrecedence(problem, region)
                }
            };

            env.problems
                .push(Problem::RuntimeError(runtime_error.clone()));

            (RuntimeError(runtime_error), Output::default())
        }
        &ast::Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => {
            // the minus sign is added before parsing, to get correct overflow/underflow behavior
            let answer = match finish_parsing_base(string, base, is_negative) {
                Ok((int, bound)) => {
                    // Done in this kinda round about way with intermediate variables
                    // to keep borrowed values around and make this compile
                    let int_string = int.to_string();
                    let int_str = int_string.as_str();
                    int_expr_from_result(var_store, Ok((int_str, int, bound)), region, base, env)
                }
                Err(e) => int_expr_from_result(var_store, Err(e), region, base, env),
            };

            (answer, Output::default())
        }
        &ast::Expr::ParensAround(sub_expr) => {
            let (loc_expr, output) = canonicalize_expr(env, var_store, scope, region, sub_expr);

            (loc_expr.value, output)
        }
        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
        bad_expr @ ast::Expr::SpaceBefore(_, _) => {
            internal_error!(
                "A SpaceBefore did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::SpaceAfter(_, _) => {
            internal_error!(
                "A SpaceAfter did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::BinOps { .. } => {
            internal_error!(
                "A binary operator chain did not get desugared somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::UnaryOp(_, _) => {
            internal_error!(
                "A unary operator did not get desugared somehow: {:#?}",
                bad_expr
            );
        }
    };

    // At the end, diff used_idents and defined_idents to see which were unused.
    // Add warnings for those!

    // In a later phase, unused top level declarations won't get monomorphized or code-genned.
    // We aren't going to bother with DCE at the level of local defs. It's going to be
    // a rounding error anyway (especially given that they'll be surfaced as warnings), LLVM will
    // DCE them in optimized builds, and it's not worth the bookkeeping for dev builds.
    (
        Loc {
            region,
            value: expr,
        },
        output,
    )
}

fn canonicalize_accessor_function(
    env: &mut Env<'_>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    field: &Accessor<'_>,
    region: Region,
) -> (Expr, Output) {
    (
        Expr::RecordAccessor(StructAccessorData {
            name: scope.gen_unique_symbol(),
            function_var: var_store.fresh(),
            record_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            closure_var: var_store.fresh(),
            field_var: var_store.fresh(),
            field: match field {
                Accessor::RecordField(field) => IndexOrField::Field((*field).into()),
                Accessor::TupleIndex(index) => match index.parse() {
                    Ok(index) => IndexOrField::Index(index),
                    Err(_) => {
                        let error = roc_problem::can::RuntimeError::InvalidTupleIndex(region);
                        env.problem(Problem::RuntimeError(error.clone()));
                        return (Expr::RuntimeError(error), Output::default());
                    }
                },
            },
        }),
        Output::default(),
    )
}

pub fn canonicalize_record<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    region: Region,
    fields: ast::Collection<'a, Loc<ast::AssignedField<'a, ast::Expr<'a>>>>,
) -> (Expr, Output) {
    if fields.is_empty() {
        (Expr::EmptyRecord, Output::default())
    } else {
        match canonicalize_fields(env, scope, var_store, region, fields.items) {
            Ok((can_fields, output)) => (
                Expr::Record {
                    record_var: var_store.fresh(),
                    fields: can_fields,
                },
                output,
            ),
            Err(runtime_error) => (Expr::RuntimeError(runtime_error), Output::default()),
        }
    }
}

pub fn canonicalize_closure<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    loc_arg_patterns: &'a [Loc<ast::Pattern<'a>>],
    loc_body_expr: &'a Loc<ast::Expr<'a>>,
    opt_def_name: Option<Symbol>,
) -> (ClosureData, Output) {
    scope.inner_function_scope(|inner_scope| {
        canonicalize_closure_body(
            env,
            var_store,
            inner_scope,
            loc_arg_patterns,
            loc_body_expr,
            opt_def_name,
        )
    })
}

fn canonicalize_closure_body<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    loc_arg_patterns: &'a [Loc<ast::Pattern<'a>>],
    loc_body_expr: &'a Loc<ast::Expr<'a>>,
    opt_def_name: Option<Symbol>,
) -> (ClosureData, Output) {
    // The globally unique symbol that will refer to this closure once it gets converted
    // into a top-level procedure for code gen.
    let (symbol, is_anonymous) = match opt_def_name {
        Some(name) => (name, false),
        None => (scope.gen_unique_symbol(), true),
    };

    let mut can_args = Vec::with_capacity(loc_arg_patterns.len());
    let mut output = Output::default();

    for loc_pattern in loc_arg_patterns.iter() {
        let can_argument_pattern = canonicalize_pattern(
            env,
            var_store,
            scope,
            &mut output,
            FunctionArg,
            &loc_pattern.value,
            loc_pattern.region,
            PermitShadows(false),
        );

        can_args.push((
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            can_argument_pattern,
        ));
    }

    let bound_by_argument_patterns: Vec<_> =
        BindingsFromPattern::new_many(can_args.iter().map(|x| &x.2)).collect();

    let (loc_body_expr, new_output) = canonicalize_expr(
        env,
        var_store,
        scope,
        loc_body_expr.region,
        &loc_body_expr.value,
    );

    let mut references_top_level = false;

    let mut captured_symbols: Vec<_> = new_output
        .references
        .value_lookups()
        .copied()
        // filter out the closure's name itself
        .filter(|s| *s != symbol)
        // symbols bound either in this pattern or deeper down are not captured!
        .filter(|s| !new_output.references.bound_symbols().any(|x| x == s))
        .filter(|s| bound_by_argument_patterns.iter().all(|(k, _)| s != k))
        // filter out top-level symbols those will be globally available, and don't need to be captured
        .filter(|s| {
            let is_top_level = env.top_level_symbols.contains(s);
            references_top_level = references_top_level || is_top_level;
            !is_top_level
        })
        // filter out imported symbols those will be globally available, and don't need to be captured
        .filter(|s| s.module_id() == env.home)
        // filter out functions that don't close over anything
        .filter(|s| !new_output.non_closures.contains(s))
        .filter(|s| !output.non_closures.contains(s))
        .map(|s| (s, var_store.fresh()))
        .collect();

    if references_top_level {
        if let Some(params_record) = env.home_params_record {
            // If this module has params and the closure references top-level symbols,
            // we need to capture the whole record so we can pass it.
            // The lower_params pass will take care of removing the captures for top-level fns.
            captured_symbols.push(params_record);
        }
    }

    output.union(new_output);

    // Now that we've collected all the references, check to see if any of the args we defined
    // went unreferenced. If any did, report them as unused arguments.
    for (sub_symbol, region) in bound_by_argument_patterns {
        if !output.references.has_value_lookup(sub_symbol) {
            // The body never referenced this argument we declared. It's an unused argument!
            env.problem(Problem::UnusedArgument(
                symbol,
                is_anonymous,
                sub_symbol,
                region,
            ));
        } else {
            // We shouldn't ultimately count arguments as referenced locals. Otherwise,
            // we end up with weird conclusions like the expression (\x -> x + 1)
            // references the (nonexistent) local variable x!
            output.references.remove_value_lookup(&sub_symbol);
        }
    }

    let mut final_expr = &loc_body_expr;
    while let Expr::LetRec(_, inner, _) | Expr::LetNonRec(_, inner) = &final_expr.value {
        final_expr = inner;
    }

    if let Expr::Return { return_value, .. } = &final_expr.value {
        env.problem(Problem::ReturnAtEndOfFunction {
            region: return_value.region,
        });
    }

    // store the references of this function in the Env. This information is used
    // when we canonicalize a surrounding def (if it exists)
    env.closures.insert(symbol, output.references.clone());

    // sort symbols, so we know the order in which they're stored in the closure record
    captured_symbols.sort();

    // store that this function doesn't capture anything. It will be promoted to a
    // top-level function, and does not need to be captured by other surrounding functions.
    if captured_symbols.is_empty() {
        output.non_closures.insert(symbol);
    }

    let return_type_var = var_store.fresh();

    let closure_data = ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        return_type: return_type_var,
        fx_type: var_store.fresh(),
        early_returns: scope.early_returns.clone(),
        name: symbol,
        captured_symbols,
        recursive: Recursive::NotRecursive,
        arguments: can_args,
        loc_body: Box::new(loc_body_expr),
    };

    (closure_data, output)
}

enum MultiPatternVariables {
    OnePattern,
    MultiPattern {
        num_patterns: usize,
        bound_occurrences: VecMap<Symbol, (Region, usize)>,
    },
}

impl MultiPatternVariables {
    #[inline(always)]
    fn new(num_patterns: usize) -> Self {
        if num_patterns > 1 {
            Self::MultiPattern {
                num_patterns,
                bound_occurrences: VecMap::with_capacity(num_patterns),
            }
        } else {
            Self::OnePattern
        }
    }

    #[inline(always)]
    fn add_pattern(&mut self, pattern: &Loc<Pattern>) {
        match self {
            MultiPatternVariables::OnePattern => {}
            MultiPatternVariables::MultiPattern {
                bound_occurrences, ..
            } => {
                for (sym, region) in BindingsFromPattern::new(pattern) {
                    if !bound_occurrences.contains_key(&sym) {
                        bound_occurrences.insert(sym, (region, 0));
                    }
                    bound_occurrences.get_mut(&sym).unwrap().1 += 1;
                }
            }
        }
    }

    #[inline(always)]
    fn get_unbound(self) -> impl Iterator<Item = (Symbol, Region)> {
        let (bound_occurrences, num_patterns) = match self {
            MultiPatternVariables::OnePattern => (Default::default(), 1),
            MultiPatternVariables::MultiPattern {
                bound_occurrences,
                num_patterns,
            } => (bound_occurrences, num_patterns),
        };

        bound_occurrences
            .into_iter()
            .filter_map(move |(sym, (region, occurs))| {
                if occurs != num_patterns {
                    Some((sym, region))
                } else {
                    None
                }
            })
    }
}

#[inline(always)]
fn canonicalize_when_branch<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    _region: Region,
    branch: &'a ast::WhenBranch<'a>,
    output: &mut Output,
) -> (WhenBranch, References) {
    let mut patterns = Vec::with_capacity(branch.patterns.len());
    let mut multi_pattern_variables = MultiPatternVariables::new(branch.patterns.len());

    for (i, loc_pattern) in branch.patterns.iter().enumerate() {
        let permit_shadows = PermitShadows(i > 0); // patterns can shadow symbols defined in the first pattern.

        let can_pattern = canonicalize_pattern(
            env,
            var_store,
            scope,
            output,
            WhenBranch,
            &loc_pattern.value,
            loc_pattern.region,
            permit_shadows,
        );

        multi_pattern_variables.add_pattern(&can_pattern);
        patterns.push(WhenBranchPattern {
            pattern: can_pattern,
            degenerate: false,
        });
    }

    let mut some_symbols_not_bound_in_all_patterns = false;
    for (unbound_symbol, region) in multi_pattern_variables.get_unbound() {
        env.problem(Problem::NotBoundInAllPatterns {
            unbound_symbol,
            region,
        });

        some_symbols_not_bound_in_all_patterns = true;
    }

    let (value, mut branch_output) = canonicalize_expr(
        env,
        var_store,
        scope,
        branch.value.region,
        &branch.value.value,
    );

    let guard = match &branch.guard {
        None => None,
        Some(loc_expr) => {
            let (can_guard, guard_branch_output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            branch_output.union(guard_branch_output);
            Some(can_guard)
        }
    };

    let references = branch_output.references.clone();
    output.union(branch_output);

    // Now that we've collected all the references for this branch, check to see if
    // any of the new idents it defined were unused. If any were, report it.
    let mut pattern_bound_symbols_body_needs = VecSet::default();
    for (symbol, region) in BindingsFromPattern::new_many(patterns.iter().map(|pat| &pat.pattern)) {
        if output.references.has_value_lookup(symbol) {
            pattern_bound_symbols_body_needs.insert(symbol);
        } else {
            env.problem(Problem::UnusedBranchDef(symbol, region));
        }
    }

    if some_symbols_not_bound_in_all_patterns && !pattern_bound_symbols_body_needs.is_empty() {
        // There might be branches that don't bind all the symbols needed by the body; mark those
        // branches degenerate.
        for pattern in patterns.iter_mut() {
            let bound_by_pattern: VecSet<_> = BindingsFromPattern::new(&pattern.pattern)
                .map(|(sym, _)| sym)
                .collect();

            let binds_all_needed = pattern_bound_symbols_body_needs
                .iter()
                .all(|sym| bound_by_pattern.contains(sym));

            if !binds_all_needed {
                pattern.degenerate = true;
            }
        }
    }

    (
        WhenBranch {
            patterns,
            value,
            guard,
            redundant: RedundantMark::new(var_store),
        },
        references,
    )
}

fn canonicalize_fields<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    var_store: &mut VarStore,
    region: Region,
    fields: &'a [Loc<ast::AssignedField<'a, ast::Expr<'a>>>],
) -> Result<(SendMap<Lowercase, Field>, Output), RuntimeError> {
    let mut can_fields = SendMap::default();
    let mut output = Output::default();

    for loc_field in fields.iter() {
        match canonicalize_field(env, scope, var_store, &loc_field.value, loc_field.region) {
            Ok(CanonicalizedField {
                label,
                loc_expr,
                output: field_out,
                field_var,
            }) => {
                let field = Field {
                    var: field_var,
                    region: loc_field.region,
                    loc_expr: Box::new(loc_expr),
                };

                let replaced = can_fields.insert(label.clone(), field);

                if let Some(old) = replaced {
                    env.problems.push(Problem::DuplicateRecordFieldValue {
                        field_name: label,
                        field_region: loc_field.region,
                        record_region: region,
                        replaced_region: old.region,
                    });
                }

                output.references.union_mut(&field_out.references);
            }
            Err(CanonicalizeFieldProblem::InvalidOptionalValue {
                field_name,
                field_region,
            }) => {
                let runtime_error = RuntimeError::InvalidOptionalValue {
                    field_name: field_name.clone(),
                    field_region,
                    record_region: region,
                };
                env.problems
                    .push(Problem::RuntimeError(runtime_error.clone()));

                return Err(runtime_error);
            }
            Err(CanonicalizeFieldProblem::InvalidIgnoredValue {
                field_name,
                field_region,
            }) => {
                let runtime_error = RuntimeError::InvalidIgnoredValue {
                    field_name: field_name.clone(),
                    field_region,
                    record_region: region,
                };
                env.problems
                    .push(Problem::RuntimeError(runtime_error.clone()));

                return Err(runtime_error);
            }
            Err(CanonicalizeFieldProblem::SpreadNotImplementedYet(spread_region)) => {
                let runtime_error = RuntimeError::SpreadNotImplementedYet(spread_region);
                env.problems
                    .push(Problem::RuntimeError(runtime_error.clone()));

                return Err(runtime_error);
            }
        }
    }

    Ok((can_fields, output))
}

struct CanonicalizedField {
    label: Lowercase,
    loc_expr: Loc<Expr>,
    output: Output,
    field_var: Variable,
}

enum CanonicalizeFieldProblem {
    InvalidOptionalValue {
        field_name: Lowercase,
        field_region: Region,
    },
    InvalidIgnoredValue {
        field_name: Lowercase,
        field_region: Region,
    },
    SpreadNotImplementedYet(Region),
}

fn canonicalize_field<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    var_store: &mut VarStore,
    field: &'a ast::AssignedField<'a, ast::Expr<'a>>,
    field_region: Region,
) -> Result<CanonicalizedField, CanonicalizeFieldProblem> {
    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        ast::AssignedField::WithValue {
            ignored,
            loc_label,
            before_prefix: _,
            prefix,
            loc_val,
        } => {
            if *ignored {
                // An ignored value, e.g. `{ _name: 123 }`
                Err(CanonicalizeFieldProblem::InvalidIgnoredValue {
                    field_name: Lowercase::from(loc_label.value),
                    field_region: Region::span_across(&loc_label.region, &loc_val.region),
                })
            } else {
                match prefix {
                    RecordValuePrefix::Colon => {
                        let field_var = var_store.fresh();
                        let (loc_can_expr, output) = canonicalize_expr(
                            env,
                            var_store,
                            scope,
                            loc_val.region,
                            &loc_val.value,
                        );

                        Ok(CanonicalizedField {
                            label: Lowercase::from(loc_label.value),
                            loc_expr: loc_can_expr,
                            output,
                            field_var,
                        })
                    }
                    RecordValuePrefix::DoubleQuestion => {
                        Err(CanonicalizeFieldProblem::InvalidOptionalValue {
                            field_name: Lowercase::from(loc_label.value),
                            field_region: Region::span_across(&loc_label.region, &loc_val.region),
                        })
                    }
                }
            }
        }

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        ast::AssignedField::WithoutValue { ignored, loc_label } => {
            if *ignored {
                Err(CanonicalizeFieldProblem::InvalidIgnoredValue {
                    field_name: Lowercase::from(loc_label.value),
                    field_region,
                })
            } else {
                let field_var = var_store.fresh();
                let (field_expr, output) = canonicalize_var_lookup(
                    env,
                    var_store,
                    scope,
                    "",
                    loc_label.value,
                    field_region,
                );

                Ok(CanonicalizedField {
                    label: Lowercase::from(loc_label.value),
                    loc_expr: Loc::at(field_region, field_expr),
                    output,
                    field_var,
                })
            }
        }

        ast::AssignedField::SpreadValue(_opt_spread) => Err(
            CanonicalizeFieldProblem::SpreadNotImplementedYet(field_region),
        ),

        ast::AssignedField::SpaceBefore(sub_field, _)
        | ast::AssignedField::SpaceAfter(sub_field, _) => {
            canonicalize_field(env, scope, var_store, sub_field, field_region)
        }
    }
}

fn canonicalize_var_lookup(
    env: &mut Env<'_>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    module_name: &str,
    ident: &str,
    region: Region,
) -> (Expr, Output) {
    use Expr::*;

    let mut output = Output::default();
    let can_expr = if module_name.is_empty() {
        // Since module_name was empty, this is an unqualified var.
        // Look it up in scope!
        match scope.lookup_str(ident, region) {
            Ok(lookup) => {
                output
                    .references
                    .insert_value_lookup(lookup, QualifiedReference::Unqualified);

                if scope.abilities_store.is_ability_member_name(lookup.symbol) {
                    AbilityMember(
                        params_in_abilities_unimplemented!(lookup),
                        Some(scope.abilities_store.fresh_specialization_id()),
                        var_store.fresh(),
                    )
                } else {
                    lookup_to_expr(var_store, lookup)
                }
            }
            Err(problem) => {
                env.problem(Problem::RuntimeError(problem.clone()));

                RuntimeError(problem)
            }
        }
    } else {
        // Since module_name was nonempty, this is a qualified var.
        // Look it up in the env!
        match env.qualified_lookup(scope, module_name, ident, region) {
            Ok(lookup) => {
                output
                    .references
                    .insert_value_lookup(lookup, QualifiedReference::Qualified);

                if scope.abilities_store.is_ability_member_name(lookup.symbol) {
                    AbilityMember(
                        params_in_abilities_unimplemented!(lookup),
                        Some(scope.abilities_store.fresh_specialization_id()),
                        var_store.fresh(),
                    )
                } else {
                    lookup_to_expr(var_store, lookup)
                }
            }
            Err(problem) => {
                // Either the module wasn't imported, or
                // it was imported but it doesn't expose this ident.
                env.problem(Problem::RuntimeError(problem.clone()));

                RuntimeError(problem)
            }
        }
    };

    // If it's valid, this ident should be in scope already.

    (can_expr, output)
}

fn lookup_to_expr(
    var_store: &mut VarStore,
    SymbolLookup {
        symbol,
        module_params,
    }: SymbolLookup,
) -> Expr {
    if let Some((params_var, params_symbol)) = module_params {
        Expr::ParamsVar {
            symbol,
            var: var_store.fresh(),
            params_symbol,
            params_var,
        }
    } else {
        Expr::Var(symbol, var_store.fresh())
    }
}

fn flatten_str_literal<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    literal: &StrLiteral<'a>,
) -> (Expr, Output) {
    use ast::StrLiteral::*;

    match literal {
        PlainLine(str_slice) => (Expr::Str((*str_slice).into()), Output::default()),
        Line(segments) => flatten_str_lines(env, var_store, scope, &[segments]),
        Block(lines) => flatten_str_lines(env, var_store, scope, lines),
    }
}

/// Comments, newlines, and nested interpolation are disallowed inside interpolation
pub fn is_valid_interpolation(expr: &ast::Expr<'_>) -> bool {
    match expr {
        // These definitely contain neither comments nor newlines, so they are valid
        ast::Expr::Var { .. }
        | ast::Expr::SingleQuote(_)
        | ast::Expr::Str(StrLiteral::PlainLine(_))
        | ast::Expr::Float(_)
        | ast::Expr::Num(_)
        | ast::Expr::NonBase10Int { .. }
        | ast::Expr::AccessorFunction(_)
        | ast::Expr::RecordUpdater(_)
        | ast::Expr::Crash
        | ast::Expr::Dbg
        | ast::Expr::Try
        | ast::Expr::Underscore(_)
        | ast::Expr::MalformedIdent(_, _)
        | ast::Expr::Tag(_)
        | ast::Expr::OpaqueRef(_) => true,
        ast::Expr::LowLevelTry(loc_expr, _) => is_valid_interpolation(&loc_expr.value),
        // Newlines are disallowed inside interpolation, and these all require newlines
        ast::Expr::DbgStmt { .. }
        | ast::Expr::LowLevelDbg(_, _, _)
        | ast::Expr::Return(_, _)
        | ast::Expr::When(_, _)
        | ast::Expr::SpaceBefore(_, _)
        | ast::Expr::Str(StrLiteral::Block(_))
        | ast::Expr::SpaceAfter(_, _) => false,
        // Desugared dbg expression
        ast::Expr::Defs(_, loc_ret) => match loc_ret.value {
            ast::Expr::LowLevelDbg(_, _, continuation) => {
                is_valid_interpolation(&continuation.value)
            }
            _ => false,
        },
        // These can contain subexpressions, so we need to recursively check those
        ast::Expr::Str(StrLiteral::Line(segments)) => {
            segments.iter().all(|segment| match segment {
                ast::StrSegment::EscapedChar(_)
                | ast::StrSegment::Unicode(_)
                | ast::StrSegment::Plaintext(_) => true,
                // Disallow nested interpolation. Alternatively, we could allow it but require
                // a comment above it apologizing to the next person who has to read the code.
                ast::StrSegment::Interpolated(_) => false,
            })
        }
        ast::Expr::Record(fields) => fields.iter().all(|loc_field| match loc_field.value {
            ast::AssignedField::WithValue {
                before_prefix,
                loc_val,
                ..
            } => before_prefix.is_empty() && is_valid_interpolation(&loc_val.value),
            ast::AssignedField::WithoutValue { .. } => true,
            ast::AssignedField::SpreadValue(opt_spread) => {
                opt_spread.is_some_and(|spread| is_valid_interpolation(&spread.value))
            }
            ast::AssignedField::SpaceBefore(_, _) | ast::AssignedField::SpaceAfter(_, _) => false,
        }),
        ast::Expr::Tuple(fields) => fields
            .iter()
            .all(|loc_field| is_valid_interpolation(&loc_field.value)),
        ast::Expr::UnaryOp(loc_expr, _) | ast::Expr::Closure(_, loc_expr) => {
            is_valid_interpolation(&loc_expr.value)
        }
        ast::Expr::DesugarProblem(problem) => problem
            .loc_expr()
            .is_some_and(|loc_expr| is_valid_interpolation(&loc_expr.value)),
        ast::Expr::TupleAccess(sub_expr, _)
        | ast::Expr::ParensAround(sub_expr)
        | ast::Expr::RecordAccess(sub_expr, _)
        | ast::Expr::TrySuffix(sub_expr) => is_valid_interpolation(sub_expr),
        ast::Expr::Apply(loc_expr, args, _called_via) => {
            is_valid_interpolation(&loc_expr.value)
                && args
                    .iter()
                    .all(|loc_arg| is_valid_interpolation(&loc_arg.value))
        }
        ast::Expr::PncApply(loc_expr, args) => {
            is_valid_interpolation(&loc_expr.value)
                && args
                    .iter()
                    .all(|loc_arg| is_valid_interpolation(&loc_arg.value))
        }
        ast::Expr::BinOps(loc_exprs, loc_expr) => {
            is_valid_interpolation(&loc_expr.value)
                && loc_exprs
                    .iter()
                    .all(|(loc_expr, _binop)| is_valid_interpolation(&loc_expr.value))
        }
        ast::Expr::If {
            if_thens: branches,
            final_else: final_branch,
            ..
        } => {
            is_valid_interpolation(&final_branch.value)
                && branches.iter().all(|(loc_before, loc_after)| {
                    is_valid_interpolation(&loc_before.value)
                        && is_valid_interpolation(&loc_after.value)
                })
        }
        ast::Expr::List(elems) => elems
            .iter()
            .all(|loc_expr| is_valid_interpolation(&loc_expr.value)),
        ast::Expr::RecordUpdate { update, fields } => {
            is_valid_interpolation(&update.value)
                && fields
                    .iter()
                    .all(|loc_field| assigned_field_is_valid_interpolation(&loc_field.value))
        }
        ast::Expr::RecordBuilder { mapper, fields } => {
            is_valid_interpolation(&mapper.value)
                && fields
                    .iter()
                    .all(|loc_field| assigned_field_is_valid_interpolation(&loc_field.value))
        }
    }
}

fn assigned_field_is_valid_interpolation<'a>(
    assigned_field: &'a ast::AssignedField<ast::Expr<'a>>,
) -> bool {
    match assigned_field {
        ast::AssignedField::WithValue {
            before_prefix,
            loc_val,
            ..
        } => before_prefix.is_empty() && is_valid_interpolation(&loc_val.value),
        ast::AssignedField::WithoutValue { .. } => true,
        ast::AssignedField::SpreadValue(opt_spread) => {
            opt_spread.is_some_and(|spread| is_valid_interpolation(&spread.value))
        }
        ast::AssignedField::SpaceBefore(inner, spaces)
        | ast::AssignedField::SpaceAfter(inner, spaces) => {
            spaces.is_empty() && assigned_field_is_valid_interpolation(&inner)
        }
    }
}

enum StrSegment {
    Interpolation(Loc<Expr>),
    Plaintext(Box<str>),
}

fn flatten_str_lines<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    lines: &[&[ast::StrSegment<'a>]],
) -> (Expr, Output) {
    use ast::StrSegment::*;

    let mut buf = String::new();
    let mut segments = Vec::new();
    let mut output = Output::default();

    for line in lines {
        for segment in line.iter() {
            match segment {
                Plaintext(string) => {
                    buf.push_str(string);
                }
                Unicode(loc_hex_digits) => match u32::from_str_radix(loc_hex_digits.value, 16) {
                    Ok(code_pt) => match char::from_u32(code_pt) {
                        Some(ch) => {
                            buf.push(ch);
                        }
                        None => {
                            env.problem(Problem::InvalidUnicodeCodePt(loc_hex_digits.region));

                            return (
                                Expr::RuntimeError(RuntimeError::InvalidUnicodeCodePt(
                                    loc_hex_digits.region,
                                )),
                                output,
                            );
                        }
                    },
                    Err(_) => {
                        env.problem(Problem::InvalidHexadecimal(loc_hex_digits.region));

                        return (
                            Expr::RuntimeError(RuntimeError::InvalidHexadecimal(
                                loc_hex_digits.region,
                            )),
                            output,
                        );
                    }
                },
                Interpolated(loc_expr) => {
                    if is_valid_interpolation(loc_expr.value) {
                        // Interpolations desugar to Str.concat calls
                        output.references.insert_call(Symbol::STR_CONCAT);

                        if !buf.is_empty() {
                            segments.push(StrSegment::Plaintext(buf.into()));

                            buf = String::new();
                        }

                        let (loc_expr, new_output) = canonicalize_expr(
                            env,
                            var_store,
                            scope,
                            loc_expr.region,
                            loc_expr.value,
                        );

                        output.union(new_output);

                        segments.push(StrSegment::Interpolation(loc_expr));
                    } else {
                        env.problem(Problem::InvalidInterpolation(loc_expr.region));

                        return (
                            Expr::RuntimeError(RuntimeError::InvalidInterpolation(loc_expr.region)),
                            output,
                        );
                    }
                }
                EscapedChar(escaped) => buf.push(escaped.unescape()),
            }
        }
    }

    if !buf.is_empty() {
        segments.push(StrSegment::Plaintext(buf.into()));
    }

    (desugar_str_segments(var_store, segments), output)
}

/// Resolve string interpolations by desugaring a sequence of StrSegments
/// into nested calls to Str.concat
fn desugar_str_segments(var_store: &mut VarStore, segments: Vec<StrSegment>) -> Expr {
    use StrSegment::*;

    let n = segments.len();
    let mut iter = segments.into_iter().rev();
    let mut loc_expr = match iter.next() {
        Some(Plaintext(string)) => Loc::at(Region::zero(), Expr::Str(string)),
        Some(Interpolation(loc_expr)) => {
            if n == 1 {
                // We concat with the empty string to ensure a type error when loc_expr is not a string
                let empty_string = Loc::at(Region::zero(), Expr::Str("".into()));

                let fn_expr = Loc::at(
                    Region::zero(),
                    Expr::Var(Symbol::STR_CONCAT, var_store.fresh()),
                );
                let expr = Expr::Call(
                    Box::new((
                        var_store.fresh(),
                        fn_expr,
                        var_store.fresh(),
                        var_store.fresh(),
                        var_store.fresh(),
                    )),
                    vec![
                        (var_store.fresh(), empty_string),
                        (var_store.fresh(), loc_expr),
                    ],
                    CalledVia::StringInterpolation,
                );

                Loc::at(Region::zero(), expr)
            } else {
                loc_expr
            }
        }
        None => {
            // No segments? Empty string!

            Loc::at(Region::zero(), Expr::Str("".into()))
        }
    };

    for seg in iter {
        let loc_new_expr = match seg {
            Plaintext(string) => Loc::at(Region::zero(), Expr::Str(string)),
            Interpolation(loc_interpolated_expr) => loc_interpolated_expr,
        };

        let fn_expr = Loc::at(
            Region::zero(),
            Expr::Var(Symbol::STR_CONCAT, var_store.fresh()),
        );
        let expr = Expr::Call(
            Box::new((
                var_store.fresh(),
                fn_expr,
                var_store.fresh(),
                var_store.fresh(),
                var_store.fresh(),
            )),
            vec![
                (var_store.fresh(), loc_new_expr),
                (var_store.fresh(), loc_expr),
            ],
            CalledVia::StringInterpolation,
        );

        loc_expr = Loc::at(Region::zero(), expr);
    }

    loc_expr.value
}

#[derive(Clone, Debug)]
pub struct Declarations {
    pub declarations: Vec<DeclarationTag>,

    /// same lengths as declarations; has a dummy value if not applicable
    pub variables: Vec<Variable>,
    pub symbols: Vec<Loc<Symbol>>,
    pub annotations: Vec<Option<crate::def::Annotation>>,

    // used for ability member specializatons.
    pub specializes: VecMap<usize, Symbol>,

    // used while lowering params.
    arity_by_name: VecMap<IdentId, usize>,

    pub host_exposed_annotations: VecMap<usize, (Variable, crate::def::Annotation)>,

    pub function_bodies: Vec<Loc<FunctionDef>>,
    pub expressions: Vec<Loc<Expr>>,
    pub destructs: Vec<DestructureDef>,
}

impl Default for Declarations {
    fn default() -> Self {
        Self::new()
    }
}

impl Declarations {
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            declarations: Vec::with_capacity(capacity),
            variables: Vec::with_capacity(capacity),
            symbols: Vec::with_capacity(capacity),
            annotations: Vec::with_capacity(capacity),
            host_exposed_annotations: VecMap::new(),
            function_bodies: Vec::with_capacity(capacity),
            expressions: Vec::with_capacity(capacity),
            specializes: VecMap::default(), // number of specializations is probably low
            destructs: Vec::new(),          // number of destructs is probably low
            arity_by_name: VecMap::with_capacity(capacity),
        }
    }

    /// To store a recursive group in the vectors without nesting, we first push a "header"
    /// here, then push the definitions that are part of that recursive group
    pub fn push_recursive_group(&mut self, length: u16, cycle_mark: IllegalCycleMark) -> usize {
        let index = self.declarations.len();

        let tag = DeclarationTag::MutualRecursion { length, cycle_mark };
        self.declarations.push(tag);

        // dummy values
        self.variables.push(Variable::NULL);
        self.symbols.push(Loc::at_zero(Symbol::ATTR_ATTR));
        self.annotations.push(None);
        self.expressions.push(Loc::at_zero(Expr::EmptyRecord));

        index
    }

    pub fn push_recursive_def(
        &mut self,
        symbol: Loc<Symbol>,
        loc_closure_data: Loc<ClosureData>,
        expr_var: Variable,
        annotation: Option<Annotation>,
        host_annotation: Option<(Variable, Annotation)>,
        specializes: Option<Symbol>,
    ) -> usize {
        let index = self.declarations.len();

        let function_def = FunctionDef {
            closure_type: loc_closure_data.value.closure_type,
            return_type: loc_closure_data.value.return_type,
            fx_type: loc_closure_data.value.fx_type,
            early_returns: loc_closure_data.value.early_returns,
            captured_symbols: loc_closure_data.value.captured_symbols,
            arguments: loc_closure_data.value.arguments,
        };

        self.arity_by_name
            .insert(symbol.value.ident_id(), function_def.arguments.len());

        let loc_function_def = Loc::at(loc_closure_data.region, function_def);

        let function_def_index = index_push_new(&mut self.function_bodies, loc_function_def);

        let tag = match loc_closure_data.value.recursive {
            Recursive::NotRecursive | Recursive::Recursive => {
                DeclarationTag::Recursive(function_def_index)
            }
            Recursive::TailRecursive => DeclarationTag::TailRecursive(function_def_index),
        };

        if let Some(annotation) = host_annotation {
            self.host_exposed_annotations
                .insert(self.declarations.len(), annotation);
        }

        self.declarations.push(tag);
        self.variables.push(expr_var);
        self.symbols.push(symbol);
        self.annotations.push(annotation);

        self.expressions.push(*loc_closure_data.value.loc_body);

        if let Some(specializes) = specializes {
            self.specializes.insert(index, specializes);
        }

        index
    }

    pub fn push_function_def(
        &mut self,
        symbol: Loc<Symbol>,
        loc_closure_data: Loc<ClosureData>,
        expr_var: Variable,
        annotation: Option<Annotation>,
        host_annotation: Option<(Variable, Annotation)>,
        specializes: Option<Symbol>,
    ) -> usize {
        let index = self.declarations.len();

        let function_def = FunctionDef {
            closure_type: loc_closure_data.value.closure_type,
            return_type: loc_closure_data.value.return_type,
            fx_type: loc_closure_data.value.fx_type,
            early_returns: loc_closure_data.value.early_returns,
            captured_symbols: loc_closure_data.value.captured_symbols,
            arguments: loc_closure_data.value.arguments,
        };

        self.arity_by_name
            .insert(symbol.value.ident_id(), function_def.arguments.len());

        let loc_function_def = Loc::at(loc_closure_data.region, function_def);

        let function_def_index = index_push_new(&mut self.function_bodies, loc_function_def);

        if let Some(annotation) = host_annotation {
            self.host_exposed_annotations
                .insert(self.declarations.len(), annotation);
        }

        self.declarations
            .push(DeclarationTag::Function(function_def_index));
        self.variables.push(expr_var);
        self.symbols.push(symbol);
        self.annotations.push(annotation);

        self.expressions.push(*loc_closure_data.value.loc_body);

        if let Some(specializes) = specializes {
            self.specializes.insert(index, specializes);
        }

        index
    }

    pub fn push_expect(
        &mut self,
        preceding_comment: Region,
        name: Symbol,
        loc_expr: Loc<Expr>,
    ) -> usize {
        let index = self.declarations.len();

        self.declarations.push(DeclarationTag::Expectation);
        self.variables.push(Variable::BOOL);
        self.symbols.push(Loc::at(preceding_comment, name));
        self.annotations.push(None);

        self.expressions.push(loc_expr);

        index
    }

    pub fn push_value_def(
        &mut self,
        symbol: Loc<Symbol>,
        loc_expr: Loc<Expr>,
        expr_var: Variable,
        annotation: Option<Annotation>,
        host_annotation: Option<(Variable, Annotation)>,
        specializes: Option<Symbol>,
    ) -> usize {
        let index = self.declarations.len();

        if let Some(annotation) = host_annotation {
            self.host_exposed_annotations
                .insert(self.declarations.len(), annotation);
        }

        self.arity_by_name.insert(symbol.value.ident_id(), 0);

        self.declarations.push(DeclarationTag::Value);
        self.variables.push(expr_var);
        self.symbols.push(symbol);
        self.annotations.push(annotation);

        self.expressions.push(loc_expr);

        if let Some(specializes) = specializes {
            self.specializes.insert(index, specializes);
        }

        index
    }

    /// Any def with a weird pattern
    pub fn push_destructure_def(
        &mut self,
        loc_pattern: Loc<Pattern>,
        loc_expr: Loc<Expr>,
        expr_var: Variable,
        annotation: Option<Annotation>,
        pattern_vars: VecMap<Symbol, Variable>,
    ) -> usize {
        let index = self.declarations.len();

        let destruct_def = DestructureDef {
            loc_pattern,
            pattern_vars,
        };

        let destructure_def_index = index_push_new(&mut self.destructs, destruct_def);

        self.declarations
            .push(DeclarationTag::Destructure(destructure_def_index));
        self.variables.push(expr_var);
        self.symbols.push(Loc::at_zero(Symbol::ATTR_ATTR));
        self.annotations.push(annotation);

        self.expressions.push(loc_expr);

        index
    }

    pub fn push_def(&mut self, def: Def) {
        match def.loc_pattern.value {
            Pattern::Identifier(symbol) => match def.loc_expr.value {
                Expr::Closure(closure_data) => match closure_data.recursive {
                    Recursive::NotRecursive => {
                        self.push_function_def(
                            Loc::at(def.loc_pattern.region, symbol),
                            Loc::at(def.loc_expr.region, closure_data),
                            def.expr_var,
                            def.annotation,
                            None,
                            None,
                        );
                    }

                    Recursive::Recursive | Recursive::TailRecursive => {
                        self.push_recursive_def(
                            Loc::at(def.loc_pattern.region, symbol),
                            Loc::at(def.loc_expr.region, closure_data),
                            def.expr_var,
                            def.annotation,
                            None,
                            None,
                        );
                    }
                },
                _ => {
                    self.push_value_def(
                        Loc::at(def.loc_pattern.region, symbol),
                        def.loc_expr,
                        def.expr_var,
                        def.annotation,
                        None,
                        None,
                    );
                }
            },
            _ => todo!(),
        }
    }

    pub fn update_builtin_def(&mut self, index: usize, def: Def) {
        match def.loc_pattern.value {
            Pattern::Identifier(s) => assert_eq!(s, self.symbols[index].value),
            p => internal_error!("a builtin definition has a non-identifier pattern: {:?}", p),
        }

        match def.loc_expr.value {
            Expr::Closure(closure_data) => {
                let function_def = FunctionDef {
                    closure_type: closure_data.closure_type,
                    return_type: closure_data.return_type,
                    fx_type: closure_data.fx_type,
                    early_returns: closure_data.early_returns,
                    captured_symbols: closure_data.captured_symbols,
                    arguments: closure_data.arguments,
                };

                let loc_function_def = Loc::at(def.loc_expr.region, function_def);

                let function_def_index =
                    index_push_new(&mut self.function_bodies, loc_function_def);

                self.declarations[index] = DeclarationTag::Function(function_def_index);
                self.expressions[index] = *closure_data.loc_body;
                self.variables[index] = def.expr_var;
            }
            _ => {
                self.declarations[index] = DeclarationTag::Value;
                self.expressions[index] = def.loc_expr;
                self.variables[index] = def.expr_var;
            }
        }
    }

    /// Convert a value def to a function def with the given arguments
    /// Currently used in lower_params
    pub fn convert_value_to_function(
        &mut self,
        index: usize,
        new_arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)>,
        var_store: &mut VarStore,
    ) {
        match self.declarations[index] {
            DeclarationTag::Value => {
                let new_args_len = new_arguments.len();

                let loc_body = self.expressions[index].clone();
                let region = loc_body.region;

                let closure_data = ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    fx_type: var_store.fresh(),
                    early_returns: vec![],
                    name: self.symbols[index].value,
                    captured_symbols: vec![],
                    recursive: Recursive::NotRecursive,
                    arguments: new_arguments,
                    loc_body: Box::new(loc_body),
                };

                let loc_closure_data = Loc::at(region, closure_data);

                let function_def = FunctionDef {
                    closure_type: loc_closure_data.value.closure_type,
                    return_type: loc_closure_data.value.return_type,
                    fx_type: loc_closure_data.value.fx_type,
                    early_returns: loc_closure_data.value.early_returns,
                    captured_symbols: loc_closure_data.value.captured_symbols,
                    arguments: loc_closure_data.value.arguments,
                };

                let loc_function_def = Loc::at(region, function_def);

                let function_def_index =
                    index_push_new(&mut self.function_bodies, loc_function_def);

                if let Some(annotation) = &mut self.annotations[index] {
                    annotation.convert_to_fn(new_args_len, var_store);
                }

                if let Some((_var, annotation)) = self.host_exposed_annotations.get_mut(&index) {
                    annotation.convert_to_fn(new_args_len, var_store);
                }

                self.declarations[index] = DeclarationTag::Function(function_def_index);
            }
            _ => internal_error!("Expected value declaration"),
        };
    }

    pub fn len(&self) -> usize {
        self.declarations.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter_top_down(&self) -> impl Iterator<Item = (usize, DeclarationTag)> + '_ {
        self.declarations.iter().scan(0, |state, e| {
            let length_so_far = *state;

            *state += e.len();

            Some((length_so_far, *e))
        })
    }

    pub fn iter_bottom_up(&self) -> impl Iterator<Item = (usize, DeclarationTag)> + '_ {
        self.declarations
            .iter()
            .rev()
            .scan(self.declarations.len() - 1, |state, e| {
                let length_so_far = *state;

                *state = length_so_far.saturating_sub(e.len());

                Some((length_so_far, *e))
            })
    }

    pub fn expects(&self) -> ExpectCollector {
        let mut collector = ExpectCollector {
            expects: VecMap::default(),
            has_dbgs: false,
        };

        let var = Variable::EMPTY_RECORD;

        for index in 0..self.len() {
            use crate::expr::DeclarationTag::*;

            match self.declarations[index] {
                Value | Function(_) | Recursive(_) | TailRecursive(_) | Destructure(_) => {
                    // def pattern has no default expressions, so skip
                    let loc_expr = &self.expressions[index];

                    collector.visit_expr(&loc_expr.value, loc_expr.region, var);
                }
                MutualRecursion { .. } => {
                    // the self of this group will be treaded individually by later iterations
                }
                Expectation => {
                    let loc_expr =
                        toplevel_expect_to_inline_expect_pure(self.expressions[index].clone());

                    collector.visit_expr(&loc_expr.value, loc_expr.region, var);
                }
            }
        }

        collector
    }

    pub(crate) fn take_arity_by_name(&mut self) -> VecMap<IdentId, usize> {
        // `arity_by_name` is only needed for lowering module params
        std::mem::take(&mut self.arity_by_name)
    }
}

roc_error_macros::assert_sizeof_default!(DeclarationTag, 8);

#[derive(Clone, Copy, Debug)]
pub enum DeclarationTag {
    Value,
    Expectation,
    Function(Index<Loc<FunctionDef>>),
    Recursive(Index<Loc<FunctionDef>>),
    TailRecursive(Index<Loc<FunctionDef>>),
    Destructure(Index<DestructureDef>),
    MutualRecursion {
        length: u16,
        cycle_mark: IllegalCycleMark,
    },
}

impl DeclarationTag {
    fn len(self) -> usize {
        use DeclarationTag::*;

        match self {
            Function(_) | Recursive(_) | TailRecursive(_) => 1,
            Value => 1,
            Expectation => 1,
            Destructure(_) => 1,
            MutualRecursion { length, .. } => length as usize + 1,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub closure_type: Variable,
    pub return_type: Variable,
    pub fx_type: Variable,
    pub early_returns: Vec<(Variable, Region, EarlyReturnKind)>,
    pub captured_symbols: Vec<(Symbol, Variable)>,
    pub arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)>,
}

#[derive(Clone, Debug)]
pub struct DestructureDef {
    pub loc_pattern: Loc<Pattern>,
    pub pattern_vars: VecMap<Symbol, Variable>,
}

pub(crate) fn get_lookup_symbols(expr: &Expr) -> Vec<ExpectLookup> {
    let mut stack: Vec<&Expr> = vec![expr];
    let mut lookups: Vec<ExpectLookup> = Vec::new();

    while let Some(expr) = stack.pop() {
        match expr {
            Expr::Var(symbol, var)
            | Expr::ParamsVar {
                symbol,
                var,
                params_symbol: _,
                params_var: _,
            }
            | Expr::RecordUpdate {
                symbol,
                record_var: var,
                ..
            } => {
                // Don't introduce duplicates, or make unused variables
                if !lookups.iter().any(|l| l.symbol == *symbol) {
                    lookups.push(ExpectLookup {
                        symbol: *symbol,
                        var: *var,
                        ability_info: None,
                    });
                }
            }
            Expr::AbilityMember(symbol, spec_id, var) => {
                if !lookups.iter().any(|l| l.symbol == *symbol) {
                    lookups.push(ExpectLookup {
                        symbol: *symbol,
                        var: *var,
                        ability_info: *spec_id,
                    });
                }
            }
            Expr::List { loc_elems, .. } => {
                stack.extend(loc_elems.iter().map(|loc_elem| &loc_elem.value));
            }
            Expr::When {
                loc_cond, branches, ..
            } => {
                stack.push(&loc_cond.value);

                stack.reserve(branches.len());

                for branch in branches {
                    stack.push(&branch.value.value);

                    if let Some(guard) = &branch.guard {
                        stack.push(&guard.value);
                    }
                }
            }
            Expr::If {
                branches,
                final_else,
                ..
            } => {
                stack.reserve(1 + branches.len() * 2);

                for (loc_cond, loc_body) in branches {
                    stack.push(&loc_cond.value);
                    stack.push(&loc_body.value);
                }

                stack.push(&final_else.value);
            }
            Expr::LetRec(defs, expr, _illegal_cycle_mark) => {
                for def in defs {
                    stack.push(&def.loc_expr.value);
                }
                stack.push(&expr.value);
            }
            Expr::LetNonRec(def, expr) => {
                stack.push(&def.loc_expr.value);
                stack.push(&expr.value);
            }
            Expr::Call(boxed_expr, args, _called_via) => {
                stack.reserve(1 + args.len());

                match &boxed_expr.1.value {
                    Expr::Var(_, _) => {
                        // do nothing
                    }
                    function_expr => {
                        // add the expr being called
                        stack.push(function_expr);
                    }
                }

                for (_var, loc_arg) in args {
                    stack.push(&loc_arg.value);
                }
            }
            Expr::Tag { arguments, .. } => {
                stack.extend(arguments.iter().map(|(_var, loc_expr)| &loc_expr.value));
            }
            Expr::RunLowLevel { args, .. } | Expr::ForeignCall { args, .. } => {
                stack.extend(args.iter().map(|(_var, arg)| arg));
            }
            Expr::OpaqueRef { argument, .. } => {
                stack.push(&argument.1.value);
            }
            Expr::RecordAccess { loc_expr, .. }
            | Expr::TupleAccess { loc_expr, .. }
            | Expr::Closure(ClosureData {
                loc_body: loc_expr, ..
            }) => {
                stack.push(&loc_expr.value);
            }
            Expr::Record { fields, .. } => {
                stack.extend(fields.iter().map(|(_, field)| &field.loc_expr.value));
            }
            Expr::Tuple { elems, .. } => {
                stack.extend(elems.iter().map(|(_, elem)| &elem.value));
            }
            Expr::ImportParams(_, _, Some((_, expr))) => {
                stack.push(expr);
            }
            Expr::Expect {
                loc_continuation, ..
            }
            | Expr::Dbg {
                loc_continuation, ..
            } => {
                stack.push(&loc_continuation.value);

                // Intentionally ignore the lookups in the nested `expect` condition itself,
                // because they couldn't possibly influence the outcome of this `expect`!
            }
            Expr::Try { result_expr, .. } => {
                stack.push(&result_expr.value);
            }
            Expr::Return { return_value, .. } => {
                stack.push(&return_value.value);
            }
            Expr::Crash { msg, .. } => stack.push(&msg.value),
            Expr::Num(_, _, _, _)
            | Expr::Float(_, _, _, _, _)
            | Expr::Int(_, _, _, _, _)
            | Expr::Str(_)
            | Expr::IngestedFile(..)
            | Expr::ZeroArgumentTag { .. }
            | Expr::RecordAccessor(_)
            | Expr::SingleQuote(..)
            | Expr::EmptyRecord
            | Expr::RuntimeError(_)
            | Expr::ImportParams(_, _, None)
            | Expr::OpaqueWrapFunction(_) => {}
        }
    }

    lookups
}

/// Here we transform
///
/// ```ignore
/// expect
///     a = 1
///     b = 2
///
///     a == b
/// ```
///
/// into
///
/// ```ignore
/// a = 1
/// b = 2
///
/// expect a == b
///
/// {}
/// ```
///
/// This is supposed to happen just before monomorphization:
/// all type errors and such are generated from the user source,
/// but this transformation means that we don't need special codegen for toplevel expects
pub fn toplevel_expect_to_inline_expect_pure(mut loc_expr: Loc<Expr>) -> Loc<Expr> {
    enum StoredDef {
        NonRecursive(Region, Box<Def>),
        Recursive(Region, Vec<Def>, IllegalCycleMark),
    }

    let mut stack = vec![];
    let mut lookups_in_cond = vec![];

    loop {
        match loc_expr.value {
            Expr::LetNonRec(boxed_def, remainder) => {
                lookups_in_cond.extend(boxed_def.pattern_vars.iter().map(|(a, b)| ExpectLookup {
                    symbol: *a,
                    var: *b,
                    ability_info: None,
                }));

                stack.push(StoredDef::NonRecursive(loc_expr.region, boxed_def));
                loc_expr = *remainder;
            }
            Expr::LetRec(defs, remainder, mark) => {
                for def in &defs {
                    lookups_in_cond.extend(def.pattern_vars.iter().map(|(a, b)| ExpectLookup {
                        symbol: *a,
                        var: *b,
                        ability_info: None,
                    }));
                }

                stack.push(StoredDef::Recursive(loc_expr.region, defs, mark));
                loc_expr = *remainder;
            }
            _ => break,
        }
    }

    let expect_region = loc_expr.region;
    let expect = Expr::Expect {
        loc_condition: Box::new(loc_expr),
        loc_continuation: Box::new(Loc::at_zero(Expr::EmptyRecord)),
        lookups_in_cond,
    };

    let mut loc_expr = Loc::at(expect_region, expect);

    for stored in stack.into_iter().rev() {
        match stored {
            StoredDef::NonRecursive(region, boxed_def) => {
                loc_expr = Loc::at(region, Expr::LetNonRec(boxed_def, Box::new(loc_expr)));
            }
            StoredDef::Recursive(region, defs, illegal_cycle_mark) => {
                let let_rec = Expr::LetRec(defs, Box::new(loc_expr), illegal_cycle_mark);
                loc_expr = Loc::at(region, let_rec);
            }
        }
    }

    loc_expr
}

pub struct ExpectCollector {
    pub expects: VecMap<Region, Vec<ExpectLookup>>,
    pub has_dbgs: bool,
}

impl crate::traverse::Visitor for ExpectCollector {
    fn visit_expr(&mut self, expr: &Expr, _region: Region, var: Variable) {
        match expr {
            Expr::Expect {
                lookups_in_cond,
                loc_condition,
                ..
            } => {
                self.expects
                    .insert(loc_condition.region, lookups_in_cond.to_vec());
            }
            Expr::Dbg { .. } => {
                self.has_dbgs = true;
            }
            _ => (),
        }

        walk_expr(self, expr, var)
    }
}

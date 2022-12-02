use crate::abilities::SpecializationId;
use crate::annotation::{freshen_opaque_def, IntroducedVariables};
use crate::builtins::builtin_defs_map;
use crate::def::{can_defs_with_return, Annotation, Def};
use crate::env::Env;
use crate::num::{
    finish_parsing_base, finish_parsing_float, finish_parsing_num, float_expr_from_result,
    int_expr_from_result, num_expr_from_result, FloatBound, IntBound, NumBound,
};
use crate::pattern::{canonicalize_pattern, BindingsFromPattern, Pattern, PermitShadows};
use crate::procedure::References;
use crate::scope::Scope;
use crate::traverse::{walk_expr, Visitor};
use roc_collections::soa::Index;
use roc_collections::{SendMap, VecMap, VecSet};
use roc_error_macros::internal_error;
use roc_module::called_via::CalledVia;
use roc_module::ident::{ForeignSymbol, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_parse::ast::{self, Defs, EscapedChar, StrLiteral};
use roc_parse::pattern::PatternType::*;
use roc_problem::can::{PrecedenceProblem, Problem, RuntimeError};
use roc_region::all::{Loc, Region};
use roc_types::num::SingleQuoteBound;
use roc_types::subs::{ExhaustiveMark, IllegalCycleMark, RedundantMark, VarStore, Variable};
use roc_types::types::{Alias, Category, LambdaSet, OptAbleVar, Type};
use std::fmt::{Debug, Display};
use std::{char, u32};

/// Derives that an opaque type has claimed, to checked and recorded after solving.
pub type PendingDerives = VecMap<Symbol, (Type, Vec<Loc<Symbol>>)>;

#[derive(Clone, Default, Debug)]
pub struct Output {
    pub references: References,
    pub tail_call: Option<Symbol>,
    pub introduced_variables: IntroducedVariables,
    pub aliases: VecMap<Symbol, Alias>,
    pub non_closures: VecSet<Symbol>,
    pub pending_derives: PendingDerives,
}

impl Output {
    pub fn union(&mut self, other: Self) {
        self.references.union_mut(&other.references);

        if let (None, Some(later)) = (self.tail_call, other.tail_call) {
            self.tail_call = Some(later);
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

#[derive(Clone, Debug)]
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

    // Lookups
    Var(Symbol, Variable),
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
        /// Result type produced by the branches.
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
        Box<(Variable, Loc<Expr>, Variable, Variable)>,
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

    /// The "crash" keyword
    Crash {
        msg: Box<Loc<Expr>>,
        ret_var: Variable,
    },

    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        record_var: Variable,
        ext_var: Variable,
        field_var: Variable,
        loc_expr: Box<Loc<Expr>>,
        field: Lowercase,
    },
    /// field accessor as a function, e.g. (.foo) expr
    Accessor(AccessorData),

    Update {
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

    // not parsed, but is generated when lowering toplevel effectful expects
    ExpectFx {
        loc_condition: Box<Loc<Expr>>,
        loc_continuation: Box<Loc<Expr>>,
        lookups_in_cond: Vec<ExpectLookup>,
    },

    Dbg {
        loc_condition: Box<Loc<Expr>>,
        loc_continuation: Box<Loc<Expr>>,
        variable: Variable,
        symbol: Symbol,
    },

    /// Rendered as empty box in editor
    TypedHole(Variable),

    /// Compiles, but will crash if reached
    RuntimeError(RuntimeError),
}

#[derive(Clone, Copy, Debug)]
pub struct ExpectLookup {
    pub symbol: Symbol,
    pub var: Variable,
    pub ability_info: Option<SpecializationId>,
}

#[derive(Clone, Copy, Debug)]
pub struct DbgLookup {
    pub symbol: Symbol,
    pub var: Variable,
    pub region: Region,
    pub ability_info: Option<SpecializationId>,
}

impl Expr {
    pub fn category(&self) -> Category {
        match self {
            Self::Num(..) => Category::Num,
            Self::Int(..) => Category::Int,
            Self::Float(..) => Category::Frac,
            Self::Str(..) => Category::Str,
            Self::SingleQuote(..) => Category::Character,
            Self::List { .. } => Category::List,
            &Self::Var(sym, _) => Category::Lookup(sym),
            &Self::AbilityMember(sym, _, _) => Category::Lookup(sym),
            Self::When { .. } => Category::When,
            Self::If { .. } => Category::If,
            Self::LetRec(_, expr, _) => expr.value.category(),
            Self::LetNonRec(_, expr) => expr.value.category(),
            &Self::Call(_, _, called_via) => Category::CallResult(None, called_via),
            &Self::RunLowLevel { op, .. } => Category::LowLevelOpResult(op),
            Self::ForeignCall { .. } => Category::ForeignCall,
            Self::Closure(..) => Category::Lambda,
            Self::Record { .. } => Category::Record,
            Self::EmptyRecord => Category::Record,
            Self::Access { field, .. } => Category::Access(field.clone()),
            Self::Accessor(data) => Category::Accessor(data.field.clone()),
            Self::Update { .. } => Category::Record,
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
            Self::ExpectFx { .. } => Category::Expect,
            Self::Crash { .. } => Category::Crash,

            Self::Dbg { .. } => Category::Expect,

            // these nodes place no constraints on the expression's type
            Self::TypedHole(_) | Self::RuntimeError(..) => Category::Unknown,
        }
    }
}

/// Stores exhaustiveness-checking metadata for a closure argument that may
/// have an annotated type.
#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Debug)]
pub struct ClosureData {
    pub function_type: Variable,
    pub closure_type: Variable,
    pub return_type: Variable,
    pub name: Symbol,
    pub captured_symbols: Vec<(Symbol, Variable)>,
    pub recursive: Recursive,
    pub arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)>,
    pub loc_body: Box<Loc<Expr>>,
}

/// A record accessor like `.foo`, which is equivalent to `\r -> r.foo`
/// Accessors are desugared to closures; they need to have a name
/// so the closure can have a correct lambda set.
///
/// We distinguish them from closures so we can have better error messages
/// during constraint generation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AccessorData {
    pub name: Symbol,
    pub function_var: Variable,
    pub record_var: Variable,
    pub closure_var: Variable,
    pub ext_var: Variable,
    pub field_var: Variable,
    pub field: Lowercase,
}

impl AccessorData {
    pub fn to_closure_data(self, record_symbol: Symbol) -> ClosureData {
        let AccessorData {
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
        let body = Expr::Access {
            record_var,
            ext_var,
            field_var,
            loc_expr: Box::new(Loc::at_zero(Expr::Var(record_symbol, record_var))),
            field,
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
#[derive(Clone, Debug)]
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
            name: function_name,
            captured_symbols: vec![],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(loc_body),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct WhenBranchPattern {
    pub pattern: Loc<Pattern>,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    pub degenerate: bool,
}

#[derive(Clone, Debug)]
pub struct WhenBranch {
    pub patterns: Vec<WhenBranchPattern>,
    pub value: Loc<Expr>,
    pub guard: Option<Loc<Expr>>,
    /// Whether this branch is redundant in the `when` it appears in
    pub redundant: RedundantMark,
}

impl WhenBranch {
    pub fn pattern_region(&self) -> Region {
        Region::span_across(
            &self
                .patterns
                .first()
                .expect("when branch has no pattern?")
                .pattern
                .region,
            &self
                .patterns
                .last()
                .expect("when branch has no pattern?")
                .pattern
                .region,
        )
    }
}

impl WhenBranch {
    pub fn region(&self) -> Region {
        Region::across_all(
            self.patterns
                .iter()
                .map(|p| &p.pattern.region)
                .chain([self.value.region].iter()),
        )
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
        ast::Expr::Record(fields) => {
            if fields.is_empty() {
                (EmptyRecord, Output::default())
            } else {
                match canonicalize_fields(env, var_store, scope, region, fields.items) {
                    Ok((can_fields, output)) => (
                        Record {
                            record_var: var_store.fresh(),
                            fields: can_fields,
                        },
                        output,
                    ),
                    Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                        field_name,
                        field_region,
                        record_region,
                    }) => (
                        Expr::RuntimeError(roc_problem::can::RuntimeError::InvalidOptionalValue {
                            field_name,
                            field_region,
                            record_region,
                        }),
                        Output::default(),
                    ),
                }
            }
        }
        ast::Expr::Tuple(_fields) => {
            todo!("canonicalize tuple");
        }
        ast::Expr::RecordUpdate {
            fields,
            update: loc_update,
        } => {
            let (can_update, update_out) =
                canonicalize_expr(env, var_store, scope, loc_update.region, &loc_update.value);
            if let Var(symbol, _) = &can_update.value {
                match canonicalize_fields(env, var_store, scope, region, fields.items) {
                    Ok((can_fields, mut output)) => {
                        output.references.union_mut(&update_out.references);

                        let answer = Update {
                            record_var: var_store.fresh(),
                            ext_var: var_store.fresh(),
                            symbol: *symbol,
                            updates: can_fields,
                        };

                        (answer, output)
                    }
                    Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                        field_name,
                        field_region,
                        record_region,
                    }) => (
                        Expr::RuntimeError(roc_problem::can::RuntimeError::InvalidOptionalValue {
                            field_name,
                            field_region,
                            record_region,
                        }),
                        Output::default(),
                    ),
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
                    tail_call: None,
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
        ast::Expr::Apply(loc_fn, loc_args, application_style) => {
            // The expression that evaluates to the function being called, e.g. `foo` in
            // (foo) bar baz
            let fn_region = loc_fn.region;

            // The function's return type
            let mut args = Vec::new();
            let mut output = Output::default();

            for loc_arg in loc_args.iter() {
                let (arg_expr, arg_out) =
                    canonicalize_expr(env, var_store, scope, loc_arg.region, &loc_arg.value);

                args.push((var_store.fresh(), arg_expr));
                output.references.union_mut(&arg_out.references);
            }

            if let ast::Expr::OpaqueRef(name) = loc_fn.value {
                // We treat opaques specially, since an opaque can wrap exactly one argument.

                debug_assert!(!args.is_empty());

                if args.len() > 1 {
                    let problem =
                        roc_problem::can::RuntimeError::OpaqueAppliedToMultipleArgs(region);
                    env.problem(Problem::RuntimeError(problem.clone()));
                    (RuntimeError(problem), output)
                } else {
                    match scope.lookup_opaque_ref(name, loc_fn.region) {
                        Err(runtime_error) => {
                            env.problem(Problem::RuntimeError(runtime_error.clone()));
                            (RuntimeError(runtime_error), output)
                        }
                        Ok((name, opaque_def)) => {
                            let argument = Box::new(args.pop().unwrap());
                            output.references.insert_type_lookup(name);

                            let (type_arguments, lambda_set_variables, specialized_def_type) =
                                freshen_opaque_def(var_store, opaque_def);

                            let opaque_ref = OpaqueRef {
                                opaque_var: var_store.fresh(),
                                name,
                                argument,
                                specialized_def_type: Box::new(specialized_def_type),
                                type_arguments,
                                lambda_set_variables,
                            };

                            (opaque_ref, output)
                        }
                    }
                }
            } else if let ast::Expr::Crash = loc_fn.value {
                // We treat crash specially, since crashing must be applied with one argument.

                debug_assert!(!args.is_empty());

                let mut args = Vec::new();
                let mut output = Output::default();

                for loc_arg in loc_args.iter() {
                    let (arg_expr, arg_out) =
                        canonicalize_expr(env, var_store, scope, loc_arg.region, &loc_arg.value);

                    args.push(arg_expr);
                    output.references.union_mut(&arg_out.references);
                }

                let crash = if args.len() > 1 {
                    let args_region = Region::span_across(
                        &loc_args.first().unwrap().region,
                        &loc_args.last().unwrap().region,
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
                    let msg = args.pop().unwrap();
                    Crash {
                        msg: Box::new(msg),
                        ret_var: var_store.fresh(),
                    }
                };

                (crash, output)
            } else {
                // Canonicalize the function expression and its arguments
                let (fn_expr, fn_expr_output) =
                    canonicalize_expr(env, var_store, scope, fn_region, &loc_fn.value);

                output.union(fn_expr_output);

                // Default: We're not tail-calling a symbol (by name), we're tail-calling a function value.
                output.tail_call = None;

                let expr = match fn_expr.value {
                    Var(symbol, _) => {
                        output.references.insert_call(symbol);

                        // we're tail-calling a symbol by name, check if it's the tail-callable symbol
                        output.tail_call = match &env.tailcallable_symbol {
                            Some(tc_sym) if *tc_sym == symbol => Some(symbol),
                            Some(_) | None => None,
                        };

                        Call(
                            Box::new((
                                var_store.fresh(),
                                fn_expr,
                                var_store.fresh(),
                                var_store.fresh(),
                            )),
                            args,
                            *application_style,
                        )
                    }
                    RuntimeError(_) => {
                        // We can't call a runtime error; bail out by propagating it!
                        return (fn_expr, output);
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
                        arguments: args,
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
                        arguments: args,
                    },
                    _ => {
                        // This could be something like ((if True then fn1 else fn2) arg1 arg2).
                        Call(
                            Box::new((
                                var_store.fresh(),
                                fn_expr,
                                var_store.fresh(),
                                var_store.fresh(),
                            )),
                            args,
                            *application_style,
                        )
                    }
                };

                (expr, output)
            }
        }
        ast::Expr::Var { module_name, ident } => {
            canonicalize_var_lookup(env, var_store, scope, module_name, ident, region)
        }
        ast::Expr::Underscore(name) => {
            // we parse underscores, but they are not valid expression syntax
            let problem = roc_problem::can::RuntimeError::MalformedIdentifier(
                (*name).into(),
                roc_parse::ident::BadIdent::Underscore(region.start()),
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
            scope.inner_scope(|inner_scope| {
                let defs: Defs = (*loc_defs).clone();
                can_defs_with_return(env, var_store, inner_scope, env.arena.alloc(defs), loc_ret)
            })
        }
        ast::Expr::Backpassing(_, _, _) => {
            unreachable!("Backpassing should have been desugared by now")
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
            output.tail_call = None;

            let mut can_branches = Vec::with_capacity(branches.len());

            for branch in branches.iter() {
                let (can_when_branch, branch_references) = scope.inner_scope(|inner_scope| {
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
                output.tail_call = None;
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
                Access {
                    record_var: var_store.fresh(),
                    field_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    loc_expr: Box::new(loc_expr),
                    field: Lowercase::from(*field),
                },
                output,
            )
        }
        ast::Expr::RecordAccessorFunction(field) => (
            Accessor(AccessorData {
                name: scope.gen_unique_symbol(),
                function_var: var_store.fresh(),
                record_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                closure_var: var_store.fresh(),
                field_var: var_store.fresh(),
                field: (*field).into(),
            }),
            Output::default(),
        ),
        ast::Expr::TupleAccess(_record_expr, _field) => todo!("handle TupleAccess"),
        ast::Expr::TupleAccessorFunction(_) => todo!("handle TupleAccessorFunction"),
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
                    output.references.insert_type_lookup(name);

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
        ast::Expr::Expect(condition, continuation) => {
            let mut output = Output::default();

            let (loc_condition, output1) =
                canonicalize_expr(env, var_store, scope, condition.region, &condition.value);

            // Get all the lookups that were referenced in the condition,
            // so we can print their values later.
            let lookups_in_cond = get_lookup_symbols(&loc_condition.value);

            let (loc_continuation, output2) = canonicalize_expr(
                env,
                var_store,
                scope,
                continuation.region,
                &continuation.value,
            );

            output.union(output1);
            output.union(output2);

            (
                Expect {
                    loc_condition: Box::new(loc_condition),
                    loc_continuation: Box::new(loc_continuation),
                    lookups_in_cond,
                },
                output,
            )
        }
        ast::Expr::Dbg(condition, continuation) => {
            let mut output = Output::default();

            let (loc_condition, output1) =
                canonicalize_expr(env, var_store, scope, condition.region, &condition.value);

            let (loc_continuation, output2) = canonicalize_expr(
                env,
                var_store,
                scope,
                continuation.region,
                &continuation.value,
            );

            output.union(output1);
            output.union(output2);

            // the symbol is used to bind the condition `x = condition`, and identify this `dbg`.
            // That would cause issues if we dbg a variable, like `dbg y`, because in the IR we
            // cannot alias variables. Hence, we make the dbg use that same variable `y`
            let symbol = match &loc_condition.value {
                Expr::Var(symbol, _) => *symbol,
                _ => scope.gen_unique_symbol(),
            };

            (
                Dbg {
                    loc_condition: Box::new(loc_condition),
                    loc_continuation: Box::new(loc_continuation),
                    variable: var_store.fresh(),
                    symbol,
                },
                output,
            )
        }
        ast::Expr::If(if_thens, final_else_branch) => {
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

        ast::Expr::PrecedenceConflict(ast::PrecedenceConflict {
            whole_region,
            binop1_position,
            binop2_position,
            binop1,
            binop2,
            expr: _,
        }) => {
            use roc_problem::can::RuntimeError::*;

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

            let problem =
                PrecedenceProblem::BothNonAssociative(*whole_region, loc_binop1, loc_binop2);

            env.problem(Problem::PrecedenceProblem(problem.clone()));

            (
                RuntimeError(InvalidPrecedence(problem, region)),
                Output::default(),
            )
        }
        ast::Expr::MalformedClosure => {
            use roc_problem::can::RuntimeError::*;
            (RuntimeError(MalformedClosure(region)), Output::default())
        }
        ast::Expr::MalformedIdent(name, bad_ident) => {
            use roc_problem::can::RuntimeError::*;

            let problem = MalformedIdentifier((*name).into(), *bad_ident, region);
            env.problem(Problem::RuntimeError(problem.clone()));

            (RuntimeError(problem), Output::default())
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
        // Below this point, we shouln't see any of these nodes anymore because
        // operator desugaring should have removed them!
        bad_expr @ ast::Expr::ParensAround(_) => {
            panic!(
                "A ParensAround did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::SpaceBefore(_, _) => {
            panic!(
                "A SpaceBefore did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::SpaceAfter(_, _) => {
            panic!(
                "A SpaceAfter did not get removed during operator desugaring somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::BinOps { .. } => {
            panic!(
                "A binary operator chain did not get desugared somehow: {:#?}",
                bad_expr
            );
        }
        bad_expr @ ast::Expr::UnaryOp(_, _) => {
            panic!(
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

pub fn canonicalize_closure<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    loc_arg_patterns: &'a [Loc<ast::Pattern<'a>>],
    loc_body_expr: &'a Loc<ast::Expr<'a>>,
    opt_def_name: Option<Symbol>,
) -> (ClosureData, Output) {
    scope.inner_scope(|inner_scope| {
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
        .filter(|s| !env.top_level_symbols.contains(s))
        // filter out imported symbols those will be globally available, and don't need to be captured
        .filter(|s| s.module_id() == env.home)
        // filter out functions that don't close over anything
        .filter(|s| !new_output.non_closures.contains(s))
        .filter(|s| !output.non_closures.contains(s))
        .map(|s| (s, var_store.fresh()))
        .collect();

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

    let closure_data = ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        return_type: var_store.fresh(),
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
        bound_occurrences: VecMap<Symbol, (Region, u8)>,
    },
}

impl MultiPatternVariables {
    #[inline(always)]
    fn new(num_patterns: usize) -> Self {
        if num_patterns > 1 {
            Self::MultiPattern {
                bound_occurrences: VecMap::with_capacity(2),
            }
        } else {
            Self::OnePattern
        }
    }

    #[inline(always)]
    fn add_pattern(&mut self, pattern: &Loc<Pattern>) {
        match self {
            MultiPatternVariables::OnePattern => {}
            MultiPatternVariables::MultiPattern { bound_occurrences } => {
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
        let bound_occurrences = match self {
            MultiPatternVariables::OnePattern => Default::default(),
            MultiPatternVariables::MultiPattern { bound_occurrences } => bound_occurrences,
        };

        bound_occurrences
            .into_iter()
            .filter_map(|(sym, (region, occurs))| {
                if occurs == 1 {
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

enum CanonicalizeRecordProblem {
    InvalidOptionalValue {
        field_name: Lowercase,
        field_region: Region,
        record_region: Region,
    },
}
fn canonicalize_fields<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    region: Region,
    fields: &'a [Loc<ast::AssignedField<'a, ast::Expr<'a>>>],
) -> Result<(SendMap<Lowercase, Field>, Output), CanonicalizeRecordProblem> {
    let mut can_fields = SendMap::default();
    let mut output = Output::default();

    for loc_field in fields.iter() {
        match canonicalize_field(env, var_store, scope, &loc_field.value) {
            Ok((label, field_expr, field_out, field_var)) => {
                let field = Field {
                    var: field_var,
                    region: loc_field.region,
                    loc_expr: Box::new(field_expr),
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
                env.problems.push(Problem::InvalidOptionalValue {
                    field_name: field_name.clone(),
                    field_region,
                    record_region: region,
                });
                return Err(CanonicalizeRecordProblem::InvalidOptionalValue {
                    field_name,
                    field_region,
                    record_region: region,
                });
            }
        }
    }

    Ok((can_fields, output))
}

enum CanonicalizeFieldProblem {
    InvalidOptionalValue {
        field_name: Lowercase,
        field_region: Region,
    },
}
fn canonicalize_field<'a>(
    env: &mut Env<'a>,
    var_store: &mut VarStore,
    scope: &mut Scope,
    field: &'a ast::AssignedField<'a, ast::Expr<'a>>,
) -> Result<(Lowercase, Loc<Expr>, Output, Variable), CanonicalizeFieldProblem> {
    use roc_parse::ast::AssignedField::*;

    match field {
        // Both a label and a value, e.g. `{ name: "blah" }`
        RequiredValue(label, _, loc_expr) => {
            let field_var = var_store.fresh();
            let (loc_can_expr, output) =
                canonicalize_expr(env, var_store, scope, loc_expr.region, &loc_expr.value);

            Ok((
                Lowercase::from(label.value),
                loc_can_expr,
                output,
                field_var,
            ))
        }

        OptionalValue(label, _, loc_expr) => Err(CanonicalizeFieldProblem::InvalidOptionalValue {
            field_name: Lowercase::from(label.value),
            field_region: Region::span_across(&label.region, &loc_expr.region),
        }),

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(_) => {
            panic!("Somehow a LabelOnly record field was not desugared!");
        }

        SpaceBefore(sub_field, _) | SpaceAfter(sub_field, _) => {
            canonicalize_field(env, var_store, scope, sub_field)
        }

        Malformed(_string) => {
            panic!("TODO canonicalize malformed record field");
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
            Ok(symbol) => {
                output.references.insert_value_lookup(symbol);

                if scope.abilities_store.is_ability_member_name(symbol) {
                    AbilityMember(
                        symbol,
                        Some(scope.abilities_store.fresh_specialization_id()),
                        var_store.fresh(),
                    )
                } else {
                    Var(symbol, var_store.fresh())
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
            Ok(symbol) => {
                output.references.insert_value_lookup(symbol);

                if scope.abilities_store.is_ability_member_name(symbol) {
                    AbilityMember(
                        symbol,
                        Some(scope.abilities_store.fresh_specialization_id()),
                        var_store.fresh(),
                    )
                } else {
                    Var(symbol, var_store.fresh())
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

/// Currently uses the heuristic of "only inline if it's a builtin"
pub fn inline_calls(var_store: &mut VarStore, expr: Expr) -> Expr {
    use Expr::*;

    match expr {
        // Num stores the `a` variable in `Num a`. Not the same as the variable
        // stored in Int and Float below, which is strictly for better error messages
        other @ Num(..)
        | other @ Int(..)
        | other @ Float(..)
        | other @ Str { .. }
        | other @ SingleQuote(..)
        | other @ RuntimeError(_)
        | other @ EmptyRecord
        | other @ Accessor { .. }
        | other @ Update { .. }
        | other @ Var(..)
        | other @ AbilityMember(..)
        | other @ RunLowLevel { .. }
        | other @ TypedHole { .. }
        | other @ ForeignCall { .. }
        | other @ OpaqueWrapFunction(_)
        | other @ Crash { .. } => other,

        List {
            elem_var,
            loc_elems,
        } => {
            let mut new_elems = Vec::with_capacity(loc_elems.len());

            for loc_elem in loc_elems {
                let value = inline_calls(var_store, loc_elem.value);

                new_elems.push(Loc {
                    value,
                    region: loc_elem.region,
                });
            }

            List {
                elem_var,
                loc_elems: new_elems,
            }
        }
        // Branching
        When {
            cond_var,
            expr_var,
            region,
            loc_cond,
            branches,
            branches_cond_var,
            exhaustive,
        } => {
            let loc_cond = Box::new(Loc {
                region: loc_cond.region,
                value: inline_calls(var_store, loc_cond.value),
            });

            let mut new_branches = Vec::with_capacity(branches.len());

            for branch in branches {
                let value = Loc {
                    value: inline_calls(var_store, branch.value.value),
                    region: branch.value.region,
                };
                let guard = match branch.guard {
                    Some(loc_expr) => Some(Loc {
                        region: loc_expr.region,
                        value: inline_calls(var_store, loc_expr.value),
                    }),
                    None => None,
                };
                let new_branch = WhenBranch {
                    patterns: branch.patterns,
                    value,
                    guard,
                    redundant: RedundantMark::new(var_store),
                };

                new_branches.push(new_branch);
            }

            When {
                cond_var,
                expr_var,
                region,
                loc_cond,
                branches: new_branches,
                branches_cond_var,
                exhaustive,
            }
        }
        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let mut new_branches = Vec::with_capacity(branches.len());

            for (loc_cond, loc_expr) in branches {
                let loc_cond = Loc {
                    value: inline_calls(var_store, loc_cond.value),
                    region: loc_cond.region,
                };

                let loc_expr = Loc {
                    value: inline_calls(var_store, loc_expr.value),
                    region: loc_expr.region,
                };

                new_branches.push((loc_cond, loc_expr));
            }

            let final_else = Box::new(Loc {
                region: final_else.region,
                value: inline_calls(var_store, final_else.value),
            });

            If {
                cond_var,
                branch_var,
                branches: new_branches,
                final_else,
            }
        }

        Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => {
            let loc_condition = Loc {
                region: loc_condition.region,
                value: inline_calls(var_store, loc_condition.value),
            };

            let loc_continuation = Loc {
                region: loc_continuation.region,
                value: inline_calls(var_store, loc_continuation.value),
            };

            Expect {
                loc_condition: Box::new(loc_condition),
                loc_continuation: Box::new(loc_continuation),
                lookups_in_cond,
            }
        }

        ExpectFx {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => {
            let loc_condition = Loc {
                region: loc_condition.region,
                value: inline_calls(var_store, loc_condition.value),
            };

            let loc_continuation = Loc {
                region: loc_continuation.region,
                value: inline_calls(var_store, loc_continuation.value),
            };

            ExpectFx {
                loc_condition: Box::new(loc_condition),
                loc_continuation: Box::new(loc_continuation),
                lookups_in_cond,
            }
        }

        Dbg {
            loc_condition,
            loc_continuation,
            variable,
            symbol,
        } => {
            let loc_condition = Loc {
                region: loc_condition.region,
                value: inline_calls(var_store, loc_condition.value),
            };

            let loc_continuation = Loc {
                region: loc_continuation.region,
                value: inline_calls(var_store, loc_continuation.value),
            };

            Dbg {
                loc_condition: Box::new(loc_condition),
                loc_continuation: Box::new(loc_continuation),
                variable,
                symbol,
            }
        }

        LetRec(defs, loc_expr, mark) => {
            let mut new_defs = Vec::with_capacity(defs.len());

            for def in defs {
                new_defs.push(Def {
                    loc_pattern: def.loc_pattern,
                    loc_expr: Loc {
                        region: def.loc_expr.region,
                        value: inline_calls(var_store, def.loc_expr.value),
                    },
                    expr_var: def.expr_var,
                    pattern_vars: def.pattern_vars,
                    annotation: def.annotation,
                });
            }

            let loc_expr = Loc {
                region: loc_expr.region,
                value: inline_calls(var_store, loc_expr.value),
            };

            LetRec(new_defs, Box::new(loc_expr), mark)
        }

        LetNonRec(def, loc_expr) => {
            let def = Def {
                loc_pattern: def.loc_pattern,
                loc_expr: Loc {
                    region: def.loc_expr.region,
                    value: inline_calls(var_store, def.loc_expr.value),
                },
                expr_var: def.expr_var,
                pattern_vars: def.pattern_vars,
                annotation: def.annotation,
            };

            let loc_expr = Loc {
                region: loc_expr.region,
                value: inline_calls(var_store, loc_expr.value),
            };

            LetNonRec(Box::new(def), Box::new(loc_expr))
        }

        Closure(ClosureData {
            function_type,
            closure_type,
            return_type,
            recursive,
            name,
            captured_symbols,
            arguments,
            loc_body,
        }) => {
            let loc_expr = *loc_body;
            let loc_expr = Loc {
                value: inline_calls(var_store, loc_expr.value),
                region: loc_expr.region,
            };

            Closure(ClosureData {
                function_type,
                closure_type,
                return_type,
                recursive,
                name,
                captured_symbols,
                arguments,
                loc_body: Box::new(loc_expr),
            })
        }

        Record { record_var, fields } => {
            todo!(
                "Inlining for Record with record_var {:?} and fields {:?}",
                record_var,
                fields
            );
        }

        Access {
            record_var,
            ext_var,
            field_var,
            loc_expr,
            field,
        } => {
            todo!("Inlining for Access with record_var {:?}, ext_var {:?}, field_var {:?}, loc_expr {:?}, field {:?}", record_var, ext_var, field_var, loc_expr, field);
        }

        Tag {
            tag_union_var: variant_var,
            ext_var,
            name,
            arguments,
        } => {
            todo!(
                "Inlining for Tag with variant_var {:?}, ext_var {:?}, name {:?}, arguments {:?}",
                variant_var,
                ext_var,
                name,
                arguments
            );
        }

        OpaqueRef {
            opaque_var,
            name,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => {
            let (var, loc_expr) = *argument;
            let argument = Box::new((
                var,
                loc_expr.map_owned(|expr| inline_calls(var_store, expr)),
            ));

            OpaqueRef {
                opaque_var,
                name,
                argument,
                specialized_def_type,
                type_arguments,
                lambda_set_variables,
            }
        }

        ZeroArgumentTag {
            closure_name,
            variant_var,
            ext_var,
            name,
        } => {
            todo!(
                "Inlining for ZeroArgumentTag with closure_name {:?}, variant_var {:?}, ext_var {:?}, name {:?}",
                closure_name,
                variant_var,
                ext_var,
                name,
            );
        }

        Call(boxed_tuple, args, called_via) => {
            let (fn_var, loc_expr, closure_var, expr_var) = *boxed_tuple;

            match loc_expr.value {
                Var(symbol, _) if symbol.is_builtin() => {
                    match builtin_defs_map(symbol, var_store) {
                        Some(Def {
                            loc_expr:
                                Loc {
                                    value:
                                        Closure(ClosureData {
                                            recursive,
                                            arguments: params,
                                            loc_body: boxed_body,
                                            ..
                                        }),
                                    ..
                                },
                            ..
                        }) => {
                            debug_assert_eq!(recursive, Recursive::NotRecursive);

                            // Since this is a canonicalized Expr, we should have
                            // already detected any arity mismatches and replaced this
                            // with a RuntimeError if there was a mismatch.
                            debug_assert_eq!(params.len(), args.len());

                            // Start with the function's body as the answer.
                            let mut loc_answer = *boxed_body;

                            // Wrap the body in one LetNonRec for each argument,
                            // such that at the end we have all the arguments in
                            // scope with the values the caller provided.
                            for (
                                (_param_var, _exhaustive_mark, loc_pattern),
                                (expr_var, loc_expr),
                            ) in params.iter().cloned().zip(args.into_iter()).rev()
                            {
                                // TODO get the correct vars into here.
                                // Not sure if param_var should be involved.
                                let pattern_vars = SendMap::default();

                                let def = Def {
                                    loc_pattern,
                                    loc_expr,
                                    expr_var,
                                    pattern_vars,
                                    annotation: None,
                                };

                                loc_answer = Loc {
                                    region: Region::zero(),
                                    value: LetNonRec(Box::new(def), Box::new(loc_answer)),
                                };
                            }

                            loc_answer.value
                        }
                        Some(_) => {
                            unreachable!("Tried to inline a non-function");
                        }
                        None => {
                            unreachable!(
                                "Tried to inline a builtin that wasn't registered: {:?}",
                                symbol
                            );
                        }
                    }
                }
                _ => {
                    // For now, we only inline calls to builtins. Leave this alone!
                    Call(
                        Box::new((fn_var, loc_expr, closure_var, expr_var)),
                        args,
                        called_via,
                    )
                }
            }
        }
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

pub fn is_valid_interpolation(expr: &ast::Expr<'_>) -> bool {
    match expr {
        ast::Expr::Var { .. } => true,
        ast::Expr::RecordAccess(sub_expr, _) => is_valid_interpolation(sub_expr),
        _ => false,
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
                EscapedChar(escaped) => buf.push(unescape_char(escaped)),
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

    let mut iter = segments.into_iter().rev();
    let mut loc_expr = match iter.next() {
        Some(Plaintext(string)) => Loc::at(Region::zero(), Expr::Str(string)),
        Some(Interpolation(loc_expr)) => loc_expr,
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

/// Returns the char that would have been originally parsed to
pub fn unescape_char(escaped: &EscapedChar) -> char {
    use EscapedChar::*;

    match escaped {
        Backslash => '\\',
        Quote => '"',
        CarriageReturn => '\r',
        Tab => '\t',
        Newline => '\n',
    }
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
            function_bodies: Vec::with_capacity(capacity),
            expressions: Vec::with_capacity(capacity),
            specializes: VecMap::default(), // number of specializations is probably low
            destructs: Vec::new(),          // number of destructs is probably low
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
        specializes: Option<Symbol>,
    ) -> usize {
        let index = self.declarations.len();

        let function_def = FunctionDef {
            closure_type: loc_closure_data.value.closure_type,
            return_type: loc_closure_data.value.return_type,
            captured_symbols: loc_closure_data.value.captured_symbols,
            arguments: loc_closure_data.value.arguments,
        };

        let loc_function_def = Loc::at(loc_closure_data.region, function_def);

        let function_def_index = Index::push_new(&mut self.function_bodies, loc_function_def);

        let tag = match loc_closure_data.value.recursive {
            Recursive::NotRecursive | Recursive::Recursive => {
                DeclarationTag::Recursive(function_def_index)
            }
            Recursive::TailRecursive => DeclarationTag::TailRecursive(function_def_index),
        };

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
        specializes: Option<Symbol>,
    ) -> usize {
        let index = self.declarations.len();

        let function_def = FunctionDef {
            closure_type: loc_closure_data.value.closure_type,
            return_type: loc_closure_data.value.return_type,
            captured_symbols: loc_closure_data.value.captured_symbols,
            arguments: loc_closure_data.value.arguments,
        };

        let loc_function_def = Loc::at(loc_closure_data.region, function_def);

        let function_def_index = Index::push_new(&mut self.function_bodies, loc_function_def);

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

    pub fn push_expect_fx(
        &mut self,
        preceding_comment: Region,
        name: Symbol,
        loc_expr: Loc<Expr>,
    ) -> usize {
        let index = self.declarations.len();

        self.declarations.push(DeclarationTag::ExpectationFx);
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
        specializes: Option<Symbol>,
    ) -> usize {
        let index = self.declarations.len();

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

        let destructure_def_index = Index::push_new(&mut self.destructs, destruct_def);

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
                        );
                    }

                    Recursive::Recursive | Recursive::TailRecursive => {
                        self.push_recursive_def(
                            Loc::at(def.loc_pattern.region, symbol),
                            Loc::at(def.loc_expr.region, closure_data),
                            def.expr_var,
                            def.annotation,
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
                    captured_symbols: closure_data.captured_symbols,
                    arguments: closure_data.arguments,
                };

                let loc_function_def = Loc::at(def.loc_expr.region, function_def);

                let function_def_index =
                    Index::push_new(&mut self.function_bodies, loc_function_def);

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
            dbgs: VecMap::default(),
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
                ExpectationFx => {
                    let loc_expr =
                        toplevel_expect_to_inline_expect_fx(self.expressions[index].clone());

                    collector.visit_expr(&loc_expr.value, loc_expr.region, var);
                }
            }
        }

        collector
    }
}

roc_error_macros::assert_sizeof_default!(DeclarationTag, 8);

#[derive(Clone, Copy, Debug)]
pub enum DeclarationTag {
    Value,
    Expectation,
    ExpectationFx,
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
            Expectation | ExpectationFx => 1,
            Destructure(_) => 1,
            MutualRecursion { length, .. } => length as usize + 1,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub closure_type: Variable,
    pub return_type: Variable,
    pub captured_symbols: Vec<(Symbol, Variable)>,
    pub arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)>,
}

#[derive(Clone, Debug)]
pub struct DestructureDef {
    pub loc_pattern: Loc<Pattern>,
    pub pattern_vars: VecMap<Symbol, Variable>,
}

fn get_lookup_symbols(expr: &Expr) -> Vec<ExpectLookup> {
    let mut stack: Vec<&Expr> = vec![expr];
    let mut lookups: Vec<ExpectLookup> = Vec::new();

    while let Some(expr) = stack.pop() {
        match expr {
            Expr::Var(symbol, var)
            | Expr::Update {
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
            Expr::LetRec(_, _, _) => todo!(),
            Expr::LetNonRec { .. } => todo!(),
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
            Expr::Access { loc_expr, .. }
            | Expr::Closure(ClosureData {
                loc_body: loc_expr, ..
            }) => {
                stack.push(&loc_expr.value);
            }
            Expr::Record { fields, .. } => {
                stack.extend(fields.iter().map(|(_, field)| &field.loc_expr.value));
            }
            Expr::Expect {
                loc_continuation, ..
            }
            | Expr::ExpectFx {
                loc_continuation, ..
            }
            | Expr::Dbg {
                loc_continuation, ..
            } => {
                stack.push(&loc_continuation.value);

                // Intentionally ignore the lookups in the nested `expect` condition itself,
                // because they couldn't possibly influence the outcome of this `expect`!
            }
            Expr::Crash { msg, .. } => stack.push(&msg.value),
            Expr::Num(_, _, _, _)
            | Expr::Float(_, _, _, _, _)
            | Expr::Int(_, _, _, _, _)
            | Expr::Str(_)
            | Expr::ZeroArgumentTag { .. }
            | Expr::Accessor(_)
            | Expr::SingleQuote(..)
            | Expr::EmptyRecord
            | Expr::TypedHole(_)
            | Expr::RuntimeError(_)
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
pub fn toplevel_expect_to_inline_expect_pure(loc_expr: Loc<Expr>) -> Loc<Expr> {
    toplevel_expect_to_inline_expect_help(loc_expr, false)
}

pub fn toplevel_expect_to_inline_expect_fx(loc_expr: Loc<Expr>) -> Loc<Expr> {
    toplevel_expect_to_inline_expect_help(loc_expr, true)
}

fn toplevel_expect_to_inline_expect_help(mut loc_expr: Loc<Expr>, has_effects: bool) -> Loc<Expr> {
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
    let expect = if has_effects {
        Expr::ExpectFx {
            loc_condition: Box::new(loc_expr),
            loc_continuation: Box::new(Loc::at_zero(Expr::EmptyRecord)),
            lookups_in_cond,
        }
    } else {
        Expr::Expect {
            loc_condition: Box::new(loc_expr),
            loc_continuation: Box::new(Loc::at_zero(Expr::EmptyRecord)),
            lookups_in_cond,
        }
    };

    let mut loc_expr = Loc::at(expect_region, expect);

    for stored in stack {
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
    pub dbgs: VecMap<Symbol, DbgLookup>,
}

impl crate::traverse::Visitor for ExpectCollector {
    fn visit_expr(&mut self, expr: &Expr, _region: Region, var: Variable) {
        match expr {
            Expr::Expect {
                lookups_in_cond,
                loc_condition,
                ..
            }
            | Expr::ExpectFx {
                lookups_in_cond,
                loc_condition,
                ..
            } => {
                self.expects
                    .insert(loc_condition.region, lookups_in_cond.to_vec());
            }
            Expr::Dbg {
                loc_condition,
                variable,
                symbol,
                ..
            } => {
                let lookup = DbgLookup {
                    symbol: *symbol,
                    var: *variable,
                    region: loc_condition.region,
                    ability_info: None,
                };

                self.dbgs.insert(*symbol, lookup);
            }
            _ => (),
        }

        walk_expr(self, expr, var)
    }
}

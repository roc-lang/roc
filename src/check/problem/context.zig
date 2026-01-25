//! Context and Category types for precise type error reporting.
//!
//! Inspired by Elm's Type/Error.hs, these types track:
//! - Context: WHERE a type error occurred (syntactic context)
//! - Category: WHAT kind of value/type is involved
//!
//! This enables error messages like "in the 2nd argument of `map`" instead of
//! generic "type mismatch" errors.

const base = @import("base");
const types_mod = @import("types");
const can = @import("can");

const Ident = base.Ident;
const Var = types_mod.Var;
const CIR = can.CIR;

/// Where a type error occurred syntactically.
///
/// This tracks the syntactic context so error messages can say things like
/// "in the 2nd argument of `map`" or "in the condition of this if".
pub const Context = union(enum) {
    /// Top-level or unknown context
    none,

    // Function-related contexts
    /// Function call
    fn_call_arity: FnCallArityContext,
    /// Argument to a function call
    fn_call_arg: FnCallArgContext,
    /// Return position of a function
    fn_return,
    /// Argument in function definition (by index)
    fn_def_arg: u32,

    // Control flow contexts
    /// Condition of an if expression
    if_condition,
    /// Branch of an if expression
    if_branch: IfBranchContext,
    /// Pattern in a match branch
    match_pattern: MatchPatternContext,
    /// Body of a match branch
    match_branch: MatchBranchContext,

    // Data structure contexts
    /// Element in a list (0-indexed)
    list_entry: ListEntryContext,
    /// Field of a record
    record_field: Ident.Idx,
    /// Element of a tuple (0-indexed)
    tuple_entry: u32,
    /// Argument of a tag
    tag_arg: TagArgContext,

    // Type annotation contexts
    /// From a type annotation
    type_annotation,

    // Operator contexts
    /// Left side of binary operator
    binop_lhs: BinopContext,
    /// Right side of binary operator
    binop_rhs: BinopContext,

    // Special contexts
    /// The ? (try) operator (simple version for early return case)
    try_operator,
    /// The ? (try) operator with expression data (for invalid try operator errors)
    try_operator_expr: TryOperatorContext,
    /// An early return statement
    early_return,
    /// Statement expression (should be {})
    statement_value,
    /// Dot-syntax method call
    method_call: MethodCallContext,
    /// Nominal type constructor (tag, record, tuple, or value)
    nominal_constructor: NominalConstructorContext,
    /// Function arguments bound by same type variable have incompatible types
    fn_args_bound_var: FnArgsBoundVarContext,
    /// Platform requirement mismatch
    platform_requirement: PlatformRequirementContext,
    /// Method type mismatch (where clause)
    method_type: MethodTypeContext,

    /// Context for function call
    pub const FnCallArityContext = struct {
        /// Name of the function being called, if known
        fn_name: ?Ident.Idx,
        /// The number of args the fn expected
        expected_args: u32,
        /// The number of args the fn given
        actual_args: u32,
    };

    /// Context for function call arguments
    pub const FnCallArgContext = struct {
        /// Name of the function being called, if known
        fn_name: ?Ident.Idx,
        /// The whole call expr
        call_expr: CIR.Expr.Idx,
        /// 0-based index of the argument
        arg_index: u32,
        /// Total number of arguments
        num_args: u32,
        /// The type variable of the argument (for type display)
        arg_var: Var,
    };

    /// Context for if branch type errors
    pub const IfBranchContext = struct {
        /// 0-based index of the branch
        branch_index: u32,
        /// Total number of branches
        num_branches: u32,
        /// Whether this is the else branch
        is_else: bool,
        /// The parent if expression (for source highlighting)
        parent_if_expr: CIR.Expr.Idx,
        /// The last if branch before the problem (for comparison display)
        last_if_branch: CIR.Expr.IfBranch.Idx,
    };

    /// Context for match pattern type errors
    pub const MatchPatternContext = struct {
        /// 0-based index of the branch
        branch_index: u32,
        /// 0-based index of the pattern within the branch
        pattern_index: u32,
        /// Total number of branches
        num_branches: u32,
        /// Total number of patterns in the branch
        num_patterns: u32,
        /// The match expression (for source highlighting)
        match_expr: CIR.Expr.Idx,
    };

    /// Context for match branch body type errors
    pub const MatchBranchContext = struct {
        /// 0-based index of the branch
        branch_index: u32,
        /// Total number of branches
        num_branches: u32,
        /// The match expression (for source highlighting)
        match_expr: CIR.Expr.Idx,
    };

    /// Context for tag argument type errors
    pub const TagArgContext = struct {
        /// Name of the tag
        tag_name: Ident.Idx,
        /// 0-based index of the argument
        arg_index: u32,
    };

    /// Context for binary operator type errors
    pub const BinopContext = struct {
        operator: Binop,
        /// The binary operator expression (for source highlighting)
        binop_expr: CIR.Expr.Idx,

        pub const Binop = enum {
            @"and",
            @"or",
            plus,
            minus,
            times,
            div,
            eq,
            neq,
            lt,
            lte,
            gt,
            gte,
        };
    };

    /// Context for method call type errors
    pub const MethodCallContext = struct {
        /// Name of the method being called
        method_name: Ident.Idx,
        /// The type variable of the dispatcher (receiver)
        dispatcher_var: Var,
    };

    /// Context for list element type errors
    pub const ListEntryContext = struct {
        /// 0-based index of the element
        elem_index: u32,
        /// Total number of elements in the list
        list_length: u32,
        /// The last element node (for highlighting)
        last_elem_idx: CIR.Node.Idx,
    };

    /// Context for try operator type errors
    pub const TryOperatorContext = struct {
        /// The try expression (for source highlighting)
        expr: CIR.Expr.Idx,
    };

    /// Context for nominal type constructor errors
    pub const NominalConstructorContext = struct {
        backing_type: BackingType,

        pub const BackingType = enum { tag, record, tuple, value };
    };

    /// Context for function arguments bound by same type variable
    pub const FnArgsBoundVarContext = struct {
        /// Name of the function, if known
        fn_name: ?Ident.Idx,
        /// Type variable of the first argument
        first_arg_var: Var,
        /// Type variable of the second argument
        second_arg_var: Var,
        /// 0-based index of the first argument
        first_arg_index: u32,
        /// 0-based index of the second argument
        second_arg_index: u32,
        /// Total number of arguments
        num_args: u32,
    };

    /// Context for platform requirement mismatch
    pub const PlatformRequirementContext = struct {
        /// The identifier that the platform requires
        required_ident: Ident.Idx,
    };

    /// Context for method type mismatch (where clause)
    pub const MethodTypeContext = struct {
        /// The dispatcher type variable
        constraint_var: Var,
        /// The name of the type being dispatched
        dispatcher_name: Ident.Idx,
        /// The method name
        method_name: Ident.Idx,
    };
};

/// What kind of value/type is involved in an error.
///
/// This helps generate more specific error messages by knowing what kind of
/// thing the user is working with.
pub const Category = union(enum) {
    // Literal values
    /// A number literal
    number,
    /// A string literal
    string,
    /// A list literal or List type
    list,
    /// A record literal or record type
    record,
    /// A tuple literal or tuple type
    tuple,
    /// A tag value (with tag name)
    tag: Ident.Idx,

    // Functions
    /// A lambda/closure
    lambda,
    /// A function call result
    fn_call,

    // Types
    /// A nominal type (with type name)
    nominal: Ident.Idx,
    /// A type alias (with alias name)
    alias: Ident.Idx,

    // Type variables
    /// An unbound/flex type variable
    flex_var,
    /// A bound/rigid type variable (with name)
    rigid_var: Ident.Idx,

    // Special
    /// Type was inferred, no specific category
    inferred,
};

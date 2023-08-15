use roc_types::subs::Variable;

trait AstInfo {
    type Num;
    type Frac;
    type Int;
    type Str;
    type SingleQuote;
    type Record;
    type Tuple;
    type RecordAccess;
    type RecordAccessorFn;
    type RecordUpdate;
    type TupleAccess;
    type TupleAccessorFn;
    type When;
    type If;
    type RunLowLevel;
}

/// TODO: could maybe be useful to have different buckets of these?
struct AstVec<T> {
    heap_index: u32,
    len: u32,
}

/// TODO: use small string optimization on these
struct AstStr {
    heap_index: u32,
    len: u32,
}

/// Like AstStr, but no small string optimization, and length
/// is stored on heap right before bytes. This sacrifices the
/// small string optimization, but lets us avoid having a variant get bigger.
struct HeapAstStr {
    heap_index: u32,
}

/// Stores a can::expr::IntValue on the heap followed immediately by an AstStr.
/// Note that these can't be small string optimized.
struct AstInt {
    heap_index: u32,
    str_len: u32,
}

/// Stores a FracValue on the heap followed immediately by an AstStr.
/// Note that these can't be small string optimized.
struct AstFrac {
    heap_index: u32,
    str_len: u32,
}

/// When we parse this, we examine the number of digits before and after the decimal place.
/// We store it in the smallest size it'll fit into. If it won't fit in Dec, give an error.
/// If it later turns out after type checking that we stored something too small, that's okay.
/// We'll up-convert it during code gen. Those should be very quick arithmetic operations.
enum FracValue {
    F32(f32),
    F64(f64),
    Dec(RocDec),
}

pub struct Can();

pub struct Parsed();

/// You can only import 65K modules.
pub struct ModuleIndex(u16);

// 18B - note: if we can eliminate the below redundant Variables (noted in comments),
// then this can become 14B instead and we can potentially fit each variant in 16B (w/ discriminant).
impl AstInfo for Can {
    type Num = (
        Variable, // 4B
        AstInt,   // 8B
        NumBound, // 2B
    ); // 14B
    type Int = (
        // Variable, // 4B - redundant
        Variable, // 4B
        AstInt,   // 8B
        IntBound, // 2B
    ); // 14B
    type Float = (
        // Variable, // 4B - redundant
        Variable,   // 4B
        AstFrac,    // 8B
        FloatBound, // 2B
    ); // 14B
    type Str = AstStr; // 8B
    type SingleQuote = (
        // Variable, // 4B - redundant
        Variable,         // 4B
        char,             // 4B
        SingleQuoteBound, // 1B
    ); // 13B

    type Record = (
        Variable,                        // 4B
        AstVecMap<(AstStr, Loc<Field>)>, // 8B
    ); // 12B

    type Tuple = (
        Variable,                   // 4B
        AstVec<(Variable, ExprId)>, // 8B
    ); // 12B

    /// Look up exactly one field on a record, e.g. (expr).foo.
    type RecordAccess = (
        // Variable, // 4B ext_var - redundant
        // Variable, // 4B field_var - redundant
        Variable,   // 4B record_var
        ExprId,     // 4B loc_expr
        HeapAstStr, // 4B field_name
    ); // 12B

    /// e.g. `.foo`
    type RecordAccessorFn = (
        // Variable, // 4B fn_var - redundant
        // Variable, // 4B record_var - redundant
        // Variable, // 4B closure_var - redundant
        // Variable, // 4B ext_var - redundant
        Variable,    // 4B field_var
        IdentId,     // 4B name
        ModuleIndex, // 2B module
    ); // 10B

    type RecordUpdate = (
        // Variable, // 4B record_var - redundant
        Variable,                 // 4B ext_var
        IdentId,                  // 4B name
        ModuleIndex,              // 2B module
        AstVecMap<AstStr, Field>, // 8B updates
    ); // 18B - PROBLEM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    /// Look up exactly one field on a tuple, e.g. (expr).0.
    type TupleAccess = (
        // Variable, // 4B ext_var - redundant
        // Variable, // 4B elem_var - redundant
        Variable, // 4B tuple_var
        ExprId, // 4B loc_expr (no Region needed, assuming we have Region for each ExprId in a side table)
        index,  // 4B index
    ); // 12B

    /// e.g. `.0`
    type TupleAccessorFn = (
        // Variable,    // 4B fn_var - redundant
        // Variable,    // 4B tuple_var - redundant
        // Variable,    // 4B closure_var - redundant
        // Variable,    // 4B ext_var - redundant
        // Variable,    // 4B field_var - redundant with IdentId
        IdentId,     // 4B name
        ModuleIndex, // 2B module
    ); // 6B

    // Branching
    type When = (
        // ExprId, // 4B loc_cond - redundant, can always be the very next Expr after `when` token
        // Variable, // 4B expr_var - redundant
        // Variable, // 4B branches_cond_var - redundant
        // Variable, // 4B exhaustive_mark - redundant
        Variable,           // 4B cond_var
        AstVec<WhenBranch>, // 8B branches
    ); // 12B

    type If = (
        // ExprId, // first_cond 4B - redundant, must always be the very next Expr after `if` token
        // Variable, // 4B cond_var - redundant
        Variable, // 4B branch_var
        AstVec<(ExprId, ExprId)>, // 8B branches; each alternates between body and cond. Last cond
                  // is final `else` instead of an actual cond
    ); // 12B

    type RunLowLevel = (
        AstVec<(Variable, ExprId)>, // 8B args
        Variable,                   // 4B ret
        LowLevel,                   // 1B op
    ); // 13B

    type Function = (
        // ExprId, // 4B function_capture - redundant, will always be next ExprId
        // Variable, // 4B function_type - redundant
        // Variable, // 4B closure_type - redundant
        // Variable, // 4B return_type - redundant with IdentId
        IdentId,                                         // 4B name
        ModuleIndex,                                     // 2B module
        AstVec<(Variable, AnnotatedMark, Loc<Pattern>)>, // 8B args
        Recursive,                                       // 1B recursive (3 variants; could inline)
    ); // 15B

    type FunctionCapture = (
        // ExprId,   // 4B body - redundant, can always be next ExprId
        AstVec<(IdentId, ModuleIndex, Variable)>, // 8B captured_symbols
    ); // 8B
}

// 12B
impl AstInfo for Parsed {
    // Literals
    type Num = AstStr; // 8B
    type Frac = AstStr; // 8B
    type Int = (
        AstStr, // 8B
        Base,   // 1B
        Sign,   // 1B
    ); // 10B
    type Str = StrLiteral; // 13B
    type SingleQuote = AstStr; // 8B
    type Record = AstVec<Loc<AssignedField<ExprId>>>; // 8B
    type Tuple = AstVec<Loc<ExprId>>; // 8B

    // Record operations
    /// Look up exactly one field on a record, e.g. `x.foo`.
    type RecordAccess = (
        ExprId, // 4B
        AstStr, // 8B
    ); // 12B

    /// e.g. `.foo`
    type RecordAccessorFn = AstStr; // 8B

    // Collection Literals
    type RecordUpdate = (
        ExprId, // 4B loc_update (assuming Region can be obtained from ExprId)
        AstVecMap<Loc<AssignedField<ExprId>>>, // 8B fields
    ); // 12B

    /// Look up exactly one field on a tuple, e.g. (expr).0.
    type TupleAccess = (
        ExprId, // 4B
        AstStr, // 8B
    ); // 12B

    /// e.g. `.0`
    type TupleAccessorFn = AstStr; // 8B
                                   //
                                   // Branching
    type When = (
        // ExprId, // 4B loc_cond - redundant, can always be the very next Expr after `when` token
        AstVec<WhenBranch>,
    ); // 12B

    type If = (
        // ExprId, // first_cond 4B - redundant, must always be the very next Expr after `if` token
        AstVec<(ExprId, ExprId)>, // 8B branches; each alternates between body and cond. Last cond
                                  // is final `else` instead of an actual cond
    ); // 8B

    type IfMissingFinalElse = (
        // ExprId, // loc_cond 4B - redundant, must always be the very next Expr after `if` token
        ExprId, // body 4B
    ); // 0B

    type RunLowLevel = Infallible; // 0B - this is a Can thing only
}

#[derive(Clone, Debug)]
pub struct Field {
    pub var: Variable,    // 4B
    pub loc_expr: ExprId, // 4B; not Loc<ExprId> bc we're assuming we can get Region given any ExprId
}

pub enum AssignedField<Val> {
    // A required field with a label, e.g. `{ name: "blah" }` or `{ name : Str }`
    RequiredValue(Loc<AstStr>, Loc<Val>),

    // An optional field with a label, e.g. `{ name ? "blah" }`
    //
    // NOTE: This only comes up in type annotations (e.g. `name ? Str`)
    // and in destructuring patterns (e.g. `{ name ? "blah" }`)
    OptionalValue(Loc<AstStr>, Loc<Val>),

    // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
    LabelOnly(AstStr),

    /// A malformed assigned field, which will code gen to a runtime error
    Malformed(AstStr),
}

// 9B
pub enum StrLiteral {
    /// The most common case: a plain string with no escapes or interpolations
    PlainLine(AstStr), // 8B
    Line(AstVec<StrSegment>),          // 8B
    Block(AstVec<AstVec<StrSegment>>), // 8B
}

// 13B
pub enum StrSegment {
    Plaintext(AstStr),            //  8B e.g. "foo"
    EscapedChar(EscapedChar),     //  1B e.g. '\n' in "Hello!\n"
    Interpolated(ExprId, Region), // 12B e.g. (name) in "Hi, \(name)!"
    Unicode(AstStr), // 16B e.g. "00A0" in "\u(00A0)" - NOTE: this used to store Region; instead,
                     // infer that from preceding StrSegment lengths when printing error messages.
                     // Storing the Region explicitly here makes this go from 13B to 17B.
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EscapedChar {
    Newline,        // \n
    Tab,            // \t
    DoubleQuote,    // \"
    SingleQuote,    // \'
    Backslash,      // \\
    CarriageReturn, // \r
}

pub enum Base {
    Octal,
    Binary,
    Hex,
    Decimal,
}

pub enum Sign {
    Positive,
    Negative,
}

pub enum Expr<Info: AstInfo> {
    // Number Literals
    Frac(Info::Frac),
    Int(Info::Int),
    Num(Info::Num),

    // String Literals
    Str(Info::Str), // string without escapes in it
    /// eg 'b'
    SingleQuote(Info::SingleQuote),

    /// Look up exactly one field on a record, e.g. `x.foo`.
    RecordAccess(&'a Expr<'a>, &'a str),

    /// e.g. `.foo` or `.0`
    AccessorFunction(Accessor<'a>),

    /// Look up exactly one field on a tuple, e.g. `(x, y).1`.
    TupleAccess(&'a Expr<'a>, &'a str),

    // Collection Literals
    List(Collection<'a, &'a Loc<Expr<'a>>>),

    RecordUpdate {
        update: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    Record(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    Tuple(Collection<'a, &'a Loc<Expr<'a>>>),

    // Record Builders
    RecordBuilder(Collection<'a, Loc<RecordBuilderField<'a>>>),

    // The name of a file to be ingested directly into a variable.
    IngestedFile(&'a Path, &'a Loc<TypeAnnotation<'a>>),

    // Lookups
    Var {
        module_name: &'a str, // module_name will only be filled if the original Roc code stated something like `5 + SomeModule.myVar`, module_name will be blank if it was `5 + myVar`
        ident: &'a str,
    },

    Underscore(&'a str),

    // The "crash" keyword
    Crash,

    // Tags
    Tag(&'a str),

    // Reference to an opaque type, e.g. @Opaq
    OpaqueRef(&'a str),

    // Pattern Matching
    Closure(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(&'a Defs<'a>, &'a Loc<Expr<'a>>),
    Backpassing(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),
    Expect(&'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),
    Dbg(&'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a tag by name, do Apply(Tag(...), ...)
    Apply(&'a Loc<Expr<'a>>, &'a [&'a Loc<Expr<'a>>], CalledVia),
    BinOps(&'a [(Loc<Expr<'a>>, Loc<BinOp>)], &'a Loc<Expr<'a>>),
    UnaryOp(&'a Loc<Expr<'a>>, Loc<UnaryOp>),

    // Conditionals
    If(&'a [(Loc<Expr<'a>>, Loc<Expr<'a>>)], &'a Loc<Expr<'a>>),
    When(
        /// The condition
        &'a Loc<Expr<'a>>,
        /// A | B if bool -> expression
        /// <Pattern 1> | <Pattern 2> if <Guard> -> <Expr>
        /// Vec, because there may be many patterns, and the guard
        /// is Option<Expr> because each branch may be preceded by
        /// a guard (".. if ..").
        &'a [&'a WhenBranch<'a>],
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    ParensAround(&'a Expr<'a>),

    // Problems
    MalformedIdent(&'a str, crate::ident::BadIdent),
    MalformedClosure,
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(&'a PrecedenceConflict<'a>),
    MultipleRecordBuilders(&'a Loc<Expr<'a>>),
    UnappliedRecordBuilder(&'a Loc<Expr<'a>>),
}

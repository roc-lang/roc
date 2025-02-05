const base = @import("../base.zig");

pub const IR = struct {
    header: SpacesBefore(Header),
    defs: Defs,
};

pub fn Spaces(comptime T: type) type {
    return struct {
        before: []CommentOrNewline,
        item: T,
        after: []CommentOrNewline,

        pub fn no_spaces(item: T) Spaces(T) {
            return Spaces(T){ .before = .{}, .item = item, .after = .{} };
        }
    };
}

pub fn SpacesBefore(comptime T: type) type {
    return struct {
        before: []CommentOrNewline,
        item: T,
    };
}

pub fn SpacesAfter(comptime T: type) type {
    return struct {
        after: []CommentOrNewline,
        item: T,
    };
}

// // TODO: convert to fn Spaced(comptime T: type) type that returns a union using T
// pub enum Spaced<'a, T> {
//     Item(T),

//     // Spaces
//     SpaceBefore(&'a Spaced<'a, T>, &'a [CommentOrNewline<'a>]),
//     SpaceAfter(&'a Spaced<'a, T>, &'a [CommentOrNewline<'a>]),
// }

pub fn SpacedUnion(comptime T: type) type {
    return enum(union) {
        
    };
}

pub const Header = union(enum) {
    Module: ModuleHeader,
    App: AppHeader,
    Package: PackageHeader,
    Platform: PlatformHeader,
    Hosted: HostedHeader,
};

pub const WhenBranch = struct {
    patterns: [].{ Pattern, Region },
    value: .{ Expr, Region },
    guard: ?.{ Expr, Region },
};

pub const WhenPattern = struct {
    pattern: .{ Pattern, Region },
    guard: ?.{ Expr, Region },
};

pub enum StrSegment<'a> {
    Plaintext(&'a str),              // e.g. "foo"
    Unicode(Loc<&'a str>),           // e.g. "00A0" in "\u(00A0)"
    EscapedChar(EscapedChar),        // e.g. '\n' in "Hello!\n"
    Interpolated(Loc<&'a Expr<'a>>), // e.g. "$(expr)"
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SingleQuoteSegment<'a> {
    Plaintext(&'a str),    // e.g. 'f'
    Unicode(Loc<&'a str>), // e.g. '00A0' in '\u(00A0)'
    EscapedChar(EscapedChar), // e.g. '\n'
                           // No interpolated expressions in single-quoted strings
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EscapedChar {
    Newline,        // \n
    Tab,            // \t
    DoubleQuote,    // \"
    SingleQuote,    // \'
    Backslash,      // \\
    CarriageReturn, // \r
    Dollar,         // \$
}

pub enum SingleQuoteLiteral<'a> {
    /// The most common case: a plain character with no escapes
    PlainLine(&'a str),
    Line(&'a [SingleQuoteSegment<'a>]),
}

pub enum StrLiteral<'a> {
    /// The most common case: a plain string with no escapes or interpolations
    PlainLine(&'a str),
    Line(&'a [StrSegment<'a>]),
    Block(&'a [&'a [StrSegment<'a>]]),
}

pub const ResultTryKind = enum {
    KeywordPrefix,
    OperatorSuffix,
};

/// A parsed expression. This uses lifetimes extensively for two reasons:
///
/// 1. It uses Bump::alloc for all allocations, which returns a reference.
/// 2. It often stores references into the input string instead of allocating.
///
/// This dramatically reduces allocations during parsing. Once parsing is done,
/// we move on to canonicalization, which often needs to allocate more because
/// it's doing things like turning local variables into fully qualified symbols.
/// Once canonicalization is done, the arena and the input string get dropped.
pub enum Expr<'a> {
    // Number Literals
    Float(&'a str),
    Num(&'a str),
    NonBase10Int {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },

    /// String Literals
    Str(StrLiteral<'a>), // string without escapes in it
    /// eg 'b'
    SingleQuote(&'a str),

    /// Look up exactly one field on a record, e.g. `x.foo`.
    RecordAccess(&'a Expr<'a>, &'a str),

    /// e.g. `.foo` or `.0`
    AccessorFunction(Accessor<'a>),

    /// Update the value of a field in a record, e.g. `&foo`
    RecordUpdater(&'a str),

    /// Look up exactly one field on a tuple, e.g. `(x, y).1`.
    TupleAccess(&'a Expr<'a>, &'a str),

    /// Early return on failures - e.g. the ? in `File.read_utf8(path)?`
    TrySuffix(&'a Expr<'a>),

    // Collection Literals
    List(Collection<'a, &'a Loc<Expr<'a>>>),

    RecordUpdate {
        update: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    Record(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    Tuple(Collection<'a, &'a Loc<Expr<'a>>>),

    /// Mapper-based record builders, e.g.
    /// { Result.parallel <-
    ///     foo: Http.get_data(Foo),
    ///     bar: Http.get_data(Bar),
    /// }
    RecordBuilder {
        mapper: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    // Lookups
    Var {
        module_name: &'a str, // module_name will only be filled if the original Roc code stated something like `5 + SomeModule.my_var`, module_name will be blank if it was `5 + my_var`
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

    Dbg,
    DbgStmt {
        first: &'a Loc<Expr<'a>>,
        extra_args: &'a [&'a Loc<Expr<'a>>],
        continuation: &'a Loc<Expr<'a>>,
        pnc_style: bool,
    },

    /// The `try` keyword that performs early return on errors
    Try,
    // This form of try is a desugared Result unwrapper
    LowLevelTry(&'a Loc<Expr<'a>>, ResultTryKind),

    // This form of debug is a desugared call to roc_dbg
    LowLevelDbg(&'a (&'a str, &'a str), &'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a tag by name, do Apply(Tag(...), ...)
    Apply(&'a Loc<Expr<'a>>, &'a [&'a Loc<Expr<'a>>], CalledVia),
    PncApply(&'a Loc<Expr<'a>>, Collection<'a, &'a Loc<Expr<'a>>>),
    BinOps(&'a [(Loc<Expr<'a>>, Loc<BinOp>)], &'a Loc<Expr<'a>>),
    UnaryOp(&'a Loc<Expr<'a>>, Loc<UnaryOp>),

    // Conditionals
    If {
        if_thens: &'a [(Loc<Expr<'a>>, Loc<Expr<'a>>)],
        final_else: &'a Loc<Expr<'a>>,
        indented_else: bool,
    },
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

    Return(
        /// The return value
        &'a Loc<Expr<'a>>,
        /// The unused code after the return statement
        Option<&'a Loc<Expr<'a>>>,
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    ParensAround(&'a Expr<'a>),

    // Problems
    MalformedIdent(&'a str, crate::ident::BadIdent),
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(&'a PrecedenceConflict<'a>),
    EmptyRecordBuilder(&'a Loc<Expr<'a>>),
    SingleFieldRecordBuilder(&'a Loc<Expr<'a>>),
    OptionalFieldInRecordBuilder(&'a Loc<&'a str>, &'a Loc<Expr<'a>>),
}

pub fn split_around<T>(items: &[T], target: usize) -> (&[T], &[T]) {
    let (before, rest) = items.split_at(target);
    let after = &rest[1..];

    (before, after)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PrecedenceConflict<'a> {
    pub whole_region: Region,
    pub binop1_position: Position,
    pub binop2_position: Position,
    pub binop1: BinOp,
    pub binop2: BinOp,
    pub expr: &'a Loc<Expr<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeHeader<'a> {
    pub name: Loc<&'a str>,
    pub vars: &'a [Loc<TypeVar<'a>>],
}

impl<'a> TypeHeader<'a> {
    pub fn region(&self) -> Region {
        Region::across_all(
            [self.name.region]
                .iter()
                .chain(self.vars.iter().map(|v| &v.region)),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeVar<'a> {
    Identifier(&'a str),
    SpaceBefore(&'a TypeVar<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeVar<'a>, &'a [CommentOrNewline<'a>]),

    // These are syntactically parsed as exprs first, so if there's anything else here,
    // we consider it malformed but preserve it for error reporting and more resilient parsing.
    Malformed(&'a Expr<'a>),
}

/// The `implements` keyword associated with ability definitions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Implements<'a> {
    Implements,
    SpaceBefore(&'a Implements<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Implements<'a>, &'a [CommentOrNewline<'a>]),
}

/// An ability demand is a value defining the ability; for example `hash : a -> U64 where a implements Hash`
/// for a `Hash` ability.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbilityMember<'a> {
    pub name: Loc<Spaced<'a, &'a str>>,
    pub typ: Loc<TypeAnnotation<'a>>,
}

impl AbilityMember<'_> {
    pub fn region(&self) -> Region {
        Region::across_all([self.name.region, self.typ.region].iter())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeDef<'a> {
    /// A type alias. This is like a standalone annotation, except the pattern
    /// must be a capitalized Identifier, e.g.
    ///
    /// Foo : Bar Baz
    Alias {
        header: TypeHeader<'a>,
        ann: Loc<TypeAnnotation<'a>>,
    },

    /// An opaque type, wrapping its inner type. E.g. Age := U64.
    Opaque {
        header: TypeHeader<'a>,
        typ: Loc<TypeAnnotation<'a>>,
        derived: Option<&'a ImplementsAbilities<'a>>,
    },

    /// An ability definition. E.g.
    ///   Hash implements
    ///     hash : a -> U64 where a implements Hash
    Ability {
        header: TypeHeader<'a>,
        loc_implements: Loc<Implements<'a>>,
        members: &'a [AbilityMember<'a>],
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueDef<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),

    // TODO in canonicalization, check to see if there are any newlines after the
    // annotation; if not, and if it's followed by a Body, then the annotation
    // applies to that expr! (TODO: verify that the pattern for both annotation and body match.)
    // No need to track that relationship in any data structure.
    Body(&'a Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),

    AnnotatedBody {
        ann_pattern: &'a Loc<Pattern<'a>>,
        ann_type: &'a Loc<TypeAnnotation<'a>>,
        lines_between: &'a [CommentOrNewline<'a>],
        body_pattern: &'a Loc<Pattern<'a>>,
        body_expr: &'a Loc<Expr<'a>>,
    },

    Dbg {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },

    Expect {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },

    /// e.g. `import InternalHttp as Http exposing [Req]`.
    ModuleImport(ModuleImport<'a>),

    /// e.g. `import "path/to/my/file.txt" as myFile : Str`
    IngestedFileImport(IngestedFileImport<'a>),

    Stmt(&'a Loc<Expr<'a>>),

    StmtAfterExpr,
}

impl<'a> ValueDef<'a> {
    pub fn replace_expr(&mut self, new_expr: &'a Loc<Expr<'a>>) {
        match self {
            ValueDef::Body(_, expr) => *expr = new_expr,
            ValueDef::AnnotatedBody { body_expr, .. } => *body_expr = new_expr,
            _ => internal_error!("replacing expr in unsupported ValueDef"),
        }
    }
}

pub struct RecursiveValueDefIter<'a, 'b> {
    current: &'b Defs<'a>,
    index: usize,
    pending: std::vec::Vec<&'b Defs<'a>>,
}

pub struct ModuleImport<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ImportedModuleName<'a>>,
    pub params: Option<ModuleImportParams<'a>>,
    pub alias: Option<header::KeywordItem<'a, ImportAsKeyword, Loc<ImportAlias<'a>>>>,
    pub exposed: Option<
        header::KeywordItem<
            'a,
            ImportExposingKeyword,
            Collection<'a, Loc<Spaced<'a, header::ExposedName<'a>>>>,
        >,
    >,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ModuleImportParams<'a> {
    pub before: &'a [CommentOrNewline<'a>],
    pub params: Loc<Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IngestedFileImport<'a> {
    pub before_path: &'a [CommentOrNewline<'a>],
    pub path: Loc<StrLiteral<'a>>,
    pub name: header::KeywordItem<'a, ImportAsKeyword, Loc<&'a str>>,
    pub annotation: Option<IngestedFileAnnotation<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IngestedFileAnnotation<'a> {
    pub before_colon: &'a [CommentOrNewline<'a>],
    pub annotation: Loc<TypeAnnotation<'a>>,
}

impl<'a> Malformed for IngestedFileAnnotation<'a> {
    fn is_malformed(&self) -> bool {
        self.annotation.value.is_malformed()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ImportAsKeyword;

impl header::Keyword for ImportAsKeyword {
    const KEYWORD: &'static str = "as";
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ImportExposingKeyword;

impl header::Keyword for ImportExposingKeyword {
    const KEYWORD: &'static str = "exposing";
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImportedModuleName<'a> {
    pub package: Option<&'a str>,
    pub name: ModuleName<'a>,
}

impl<'a> From<ImportedModuleName<'a>> for QualifiedModuleName<'a> {
    fn from(imported: ImportedModuleName<'a>) -> Self {
        Self {
            opt_package: imported.package,
            module: imported.name.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImportAlias<'a>(&'a str);

impl<'a> ImportAlias<'a> {
    pub const fn new(name: &'a str) -> Self {
        ImportAlias(name)
    }

    pub const fn as_str(&'a self) -> &'a str {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Defs<'a> {
    pub tags: std::vec::Vec<EitherIndex<TypeDef<'a>, ValueDef<'a>>>,
    pub regions: std::vec::Vec<Region>,
    pub space_before: std::vec::Vec<Slice<CommentOrNewline<'a>>>,
    pub space_after: std::vec::Vec<Slice<CommentOrNewline<'a>>>,
    pub spaces: std::vec::Vec<CommentOrNewline<'a>>,
    pub type_defs: std::vec::Vec<TypeDef<'a>>,
    pub value_defs: std::vec::Vec<ValueDef<'a>>,
}

pub struct SplitDefsAround<'a> {
    pub before: Defs<'a>,
    pub after: Defs<'a>,
}

/// Should always be a zero-argument `Apply`; we'll check this in canonicalization
pub type AbilityName<'a> = Loc<TypeAnnotation<'a>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ImplementsClause<'a> {
    pub var: Loc<Spaced<'a, &'a str>>,
    pub abilities: &'a [AbilityName<'a>],
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AbilityImpls<'a> {
    // `{ eq: myEq }`
    AbilityImpls(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a AbilityImpls<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a AbilityImpls<'a>, &'a [CommentOrNewline<'a>]),
}

/// `Eq` or `Eq { eq: myEq }`
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ImplementsAbility<'a> {
    ImplementsAbility {
        /// Should be a zero-argument `Apply` or an error; we'll check this in canonicalization
        ability: Loc<TypeAnnotation<'a>>,
        impls: Option<Loc<AbilityImpls<'a>>>,
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a ImplementsAbility<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a ImplementsAbility<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ImplementsAbilities<'a> {
    pub before_implements_kw: &'a [CommentOrNewline<'a>],
    pub implements: Region,
    pub after_implements_kw: &'a [CommentOrNewline<'a>],
    pub item: Loc<Collection<'a, Loc<ImplementsAbility<'a>>>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionArrow {
    /// ->
    Pure,
    /// =>
    Effectful,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeAnnotation<'a> {
    /// A function. The types of its arguments, the type of arrow used, then the type of its return value.
    Function(
        &'a [Loc<TypeAnnotation<'a>>],
        FunctionArrow,
        &'a Loc<TypeAnnotation<'a>>,
    ),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(&'a str, &'a str, &'a [Loc<TypeAnnotation<'a>>]),

    /// A bound type variable, e.g. `a` in `(a -> a)`
    BoundVariable(&'a str),

    /// Inline type alias, e.g. `as List a` in `[Cons a (List a), Nil] as List a`
    As(
        &'a Loc<TypeAnnotation<'a>>,
        &'a [CommentOrNewline<'a>],
        TypeHeader<'a>,
    ),

    Record {
        fields: Collection<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    Tuple {
        elems: Collection<'a, Loc<TypeAnnotation<'a>>>,
        /// The row type variable in an open tuple, e.g. the `r` in `( Str, Str )r`.
        /// This is None if it's a closed tuple annotation like `( Str, Str )`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    /// A tag union, e.g. `[
    TagUnion {
        /// The row type variable in an open tag union, e.g. the `a` in `[Foo, Bar]a`.
        /// This is None if it's a closed tag union like `[Foo, Bar]`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
        tags: Collection<'a, Loc<Tag<'a>>>,
    },

    /// '_', indicating the compiler should infer the type
    Inferred,

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    /// A "where" clause demanding abilities designated by a `where`, e.g. `a -> U64 where a implements Hash`
    Where(&'a Loc<TypeAnnotation<'a>>, &'a [Loc<ImplementsClause<'a>>]),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tag<'a> {
    Apply {
        name: Loc<&'a str>,
        args: &'a [Loc<TypeAnnotation<'a>>],
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
}

pub fn AssignedField(comptime T: type) type {
    return union(enum) {
        // A required field with a label, e.g. `{ name: "blah" }` or `{ name : Str }`
        RequiredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

        // An ignored field, e.g. `{ _name: "blah" }` or `{ _ : Str }`
        IgnoredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

        // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
        LabelOnly(Loc<&'a str>),

        // We preserve this for the formatter; canonicalization ignores it.
        SpaceBefore(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),
        SpaceAfter(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),
    };
}

pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
    DocComment(&'a str),
}

pub const PatternAs = struct {
    spaces_before: &'a [CommentOrNewline<'a>],
    identifier: Loc<&'a str>,
    ident_region: base.Region,
}

pub enum Pattern<'a> {
    // Identifier
    Identifier {
        ident: &'a str,
    },
    QualifiedIdentifier {
        module_name: &'a str,
        ident: &'a str,
    },

    Tag(&'a str),

    OpaqueRef(&'a str),

    Apply(&'a Loc<Pattern<'a>>, &'a [Loc<Pattern<'a>>]),

    PncApply(&'a Loc<Pattern<'a>>, Collection<'a, Loc<Pattern<'a>>>),

    /// This is Located<Pattern> rather than Located<str> so we can record comments
    /// around the destructured names, e.g. { x ### x does stuff ###, y }
    /// In practice, these patterns will always be Identifier
    RecordDestructure(Collection<'a, Loc<Pattern<'a>>>),

    /// A required field pattern, e.g. { x: Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    RequiredField(&'a str, &'a Loc<Pattern<'a>>),

    /// An optional field pattern, e.g. { x ? Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    OptionalField(&'a str, &'a Loc<Expr<'a>>),

    // Literal
    NumLiteral(&'a str),
    NonBase10Literal {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },
    FloatLiteral(&'a str),
    StrLiteral(StrLiteral<'a>),

    /// Underscore pattern
    /// Contains the name of underscore pattern (e.g. "a" is for "_a" in code)
    /// Empty string is unnamed pattern ("" is for "_" in code)
    Underscore(&'a str),
    SingleQuote(&'a str),

    /// A tuple pattern, e.g. (Just x, 1)
    Tuple(Collection<'a, Loc<Pattern<'a>>>),

    /// A list pattern like [_, x, ..]
    List(Collection<'a, Loc<Pattern<'a>>>),

    /// A list-rest pattern ".."
    /// Can only occur inside of a [Pattern::List]
    ListRest(Option<(&'a [CommentOrNewline<'a>], PatternAs<'a>)>),

    As(&'a Loc<Pattern<'a>>, PatternAs<'a>),

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    MalformedIdent(&'a str, crate::ident::BadIdent),
    MalformedExpr(&'a Expr<'a>),
}

pub const Base = enum {
    Octal,
    Binary,
    Hex,
    Decimal,
};

pub fn Collection(comptime T: type) type {
    return struct {
        items: []T,
        // Use a pointer to a slice (rather than just a slice), in order to avoid bloating
        // Ast variants. The final_comments field is rarely accessed in the hot path, so
        // this shouldn't matter much for perf.
        // Use an Option, so it's possible to initialize without allocating.
        final_comments: ?[]CommentOrNewline,
    };
}

